{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The store, as the wallet asks it: a record of questions that hw fills
-- with the web client of a haskoin-store ('webStore') and that the tests
-- fill with scripted answers, so that the sync and the commands can be
-- played out without a network. Sending a transaction through it and
-- waiting for the store to see it is here too.
module Haskoin.Wallet.Store where

import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData, force)
import Control.Exception (ErrorCall (..), Exception, Handler (..), catches, evaluate, throwIO, try)
import Control.Lens ((%~), (&), (.~), (?~), (^.))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Get (runGetL)
import Data.Bytes.Put (runPutL)
import Data.Bytes.Serial (Serial, deserialize, serialize)
import Data.Default (def)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..), appEndo)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Store.WebCommon (ApiResource (..), Param (..), ParamBox (..), PostBox (..), asProxy, paramLabel, queryPath)
import Haskoin.Wallet.Config
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Request)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (StdMethod (..))
import Network.HTTP.Types.Status (statusCode, statusIsSuccessful)
import qualified Network.Wreq as Wreq
import Numeric.Natural (Natural)
import UnliftIO.Async (pooledMapConcurrentlyN)

{- The questions -}

-- | The questions the sync asks the store. Every answer is complete: a
-- function that lists things lists all of them, and the sync checks the
-- counts the store gives against the lists it returns, so that a truncated
-- answer is an error rather than lost coins or transactions.
data Store = Store
  { -- | Fails when the store is not in a state to be trusted
    storeHealth :: ExceptT String IO (),
    -- | The best block of the chain
    storeBestBlock :: ExceptT String IO Store.BlockData,
    -- | The blocks the store knows at a height, in the main chain or not
    storeBlocksAtHeight :: BlockHeight -> ExceptT String IO [Store.BlockData],
    -- | The latest blocks of the main chain, newest first: a hundred at
    -- most, without their lists of transactions
    storeLatestBlocks :: ExceptT String IO [Store.BlockData],
    -- | The transactions of a block, in their order in it
    storeBlockTransactions :: BlockHash -> ExceptT String IO [Store.Transaction],
    -- | The balance of each of the addresses, including its transaction
    -- and coin counts
    storeBalances :: [Address] -> ExceptT String IO [Store.Balance],
    -- | The newest n transactions of an address, newest first
    storeAddressTxs :: Address -> Natural -> ExceptT String IO [Store.TxRef],
    -- | The transactions with the given ids, deleted ones included; a
    -- transaction the store does not know is left out
    storeTransactions :: [TxHash] -> ExceptT String IO [Store.Transaction],
    -- | Every unspent output of the addresses
    storeUnspents :: [Address] -> ExceptT String IO [Store.Unspent],
    -- | The raw transactions with the given ids; a transaction the store
    -- does not know is left out
    storeRawTransactions :: [TxHash] -> ExceptT String IO [Tx],
    -- | Hand a signed transaction to the network. The store passes it to
    -- its peers without checking it and says nothing of their answer: only
    -- the store listing it afterwards tells that the network took it.
    storePublish :: Tx -> ExceptT String IO ()
  }

{- The web client -}

-- | A call of the web client to the store at the host, every failure shown
-- as a message: an error the store answered, a store hw could not reach,
-- or an answer it could not read. The web client decodes an answer only
-- once it is used, so the answer is read in full here, where its failure
-- is caught.
webCall :: (NFData a) => Text -> ExceptT Store.Except IO a -> ExceptT String IO a
webCall host action =
  ExceptT $ (answered =<< runExceptT action) `catches` [Handler unreachable, Handler unreadable, Handler tooLong]
  where
    answered (Left err) = return $ Left (storeError host err)
    answered (Right answer) = Right <$> evaluate (force answer)
    unreachable e = return $ Left (unreachableStore host e)
    unreadable (ErrorCall reason) =
      return $ Left ("The store at " <> cs host <> " gave an answer hw could not read: " <> reason)
    tooLong AnswerTooLong =
      return $ Left ("The store at " <> cs host <> " sent an answer of more than " <> show (answerLimit `div` (1024 * 1024)) <> " MiB, which hw does not read")

{- Calls -}

-- | The most bytes hw reads of one answer of the store: 64 MiB, sixteen
-- times a full block. The answers about a wallet's addresses and
-- transactions, a hundred at a time ('requestBatch'), are far smaller. A
-- store that sends more is refused before the answer is decoded: the web
-- client of haskoin-store reads a body in full, and a body without end
-- filled 1.28 GB in half a second.
answerLimit :: Int
answerLimit = 64 * 1024 * 1024

-- | The failure of an answer over 'answerLimit'
data AnswerTooLong = AnswerTooLong
  deriving (Show)

instance Exception AnswerTooLong

-- | A call to the store, as the web client of haskoin-store makes it
-- ('apiCall'), with the body of the answer read no further than
-- 'answerLimit' bytes. The request is the client's: the path and the
-- query of the resource, binary answers, and an error the store answers
-- read from its body.
storeCall :: (ApiResource a b) => Ctx -> ApiConfig -> a -> ExceptT Store.Except IO b
storeCall ctx (ApiConfig net apiHost) res = do
  query <- liftEither $ queryOptions net ctx res
  let url = apiHost <> net.name <> cs (queryPath net ctx res)
      options = appEndo (query <> binary) (Wreq.defaults & Wreq.manager %~ either (Left . boundedAnswers) Right)
  case resourceMethod (asProxy res) of
    GET -> decoded =<< liftIO (try (Wreq.getWith options url))
    POST -> case resourceBody res of
      Just (PostBox body) -> decoded =<< liftIO (try (Wreq.postWith options url (runPutL (serialize body))))
      Nothing -> throwError $ Store.StringError "Could not post resource"
    _ -> throwError $ Store.StringError "Unsupported HTTP method"
  where
    decoded :: (Serial b) => Either Store.Except (Wreq.Response BL.ByteString) -> ExceptT Store.Except IO b
    decoded = either throwError (return . runGetL deserialize . (^. Wreq.responseBody))
    binary = Endo $ (Wreq.header "Accept" .~ ["application/octet-stream"]) . (Wreq.checkResponse ?~ checkStatus)

-- | The query of a resource, as the web client of haskoin-store writes it
queryOptions :: (ApiResource a b) => Network -> Ctx -> a -> Either Store.Except (Endo Wreq.Options)
queryOptions net ctx res = mconcat <$> mapM option (snd (queryParams res))
  where
    option (ParamBox p) = do
      values <- maybe (Left (Store.UserError "Invalid Param")) Right (encodeParam net ctx p)
      return $ Endo $ Wreq.param (paramLabel p) .~ [Text.intercalate "," values]

-- | Refuse an answer of the store that is not a success, with the error
-- the store gave in its body, as the web client of haskoin-store does. A
-- health check answers 503 with its report when the store is not well.
checkStatus :: Request -> HTTP.Response HTTP.BodyReader -> IO ()
checkStatus req res
  | statusIsSuccessful status = return ()
  | "/health" `Text.isInfixOf` cs (HTTP.path req) && statusCode status == 503 = return ()
  | otherwise = do
      except <- Json.decodeStrict <$> HTTP.responseBody res
      throwIO $ fromMaybe (Store.StringError "could not decode error") except
  where
    status = HTTP.responseStatus res

-- | The settings of the connections, their answers read no further than
-- 'answerLimit' bytes: past it, 'AnswerTooLong'
boundedAnswers :: HTTP.ManagerSettings -> HTTP.ManagerSettings
boundedAnswers settings = settings {HTTP.managerModifyResponse = bounded <=< HTTP.managerModifyResponse settings}
  where
    bounded res = do
      count <- newIORef (0 :: Int)
      let body = do
            chunk <- HTTP.responseBody res
            total <- atomicModifyIORef' count (\n -> let m = n + BS.length chunk in (m, m))
            when (total > answerLimit) $ throwIO AnswerTooLong
            return chunk
      return res {HTTP.responseBody = body}

-- | The message of an error the store answered. The web client reads the
-- error from the body of the answer. A body it cannot read comes as the
-- error "could not decode error". The error page of a proxy in front of a
-- store that is down is such a body.
storeError :: Text -> Store.Except -> String
storeError host err = "The store at " <> cs host <> " answered with an error" <> what
  where
    what = case err of
      Store.StringError "could not decode error" -> " hw could not read"
      _ -> ": " <> show err

-- | The message of a store hw could not reach, and why
unreachableStore :: Text -> HttpException -> String
unreachableStore host e = "The store at " <> cs host <> " could not be reached: " <> reason
  where
    reason = case e of
      HttpExceptionRequest _ (ConnectionFailure _) -> "the connection failed"
      HttpExceptionRequest _ ConnectionTimeout -> "the connection timed out"
      HttpExceptionRequest _ ResponseTimeout -> "it did not answer in time"
      HttpExceptionRequest _ content -> show content
      InvalidUrlException url why -> "the address " <> url <> " is not valid: " <> why

-- | How many addresses or transactions one request to the store asks
-- about. A question about more of them goes in several requests. The store
-- puts no limit on a request; a hundred keeps each one short.
requestBatch :: Natural
requestBatch = 100

-- | The store of hw: the web API of the haskoin-store the configuration
-- names
webStore :: Ctx -> Config -> Network -> Store
webStore ctx cfg net =
  Store
    { storeHealth = do
        health <- call GetHealth
        unless (Store.isOK health) $
          throwError "The indexer health check has failed",
      storeBestBlock = call (GetBlockBest def),
      storeBlocksAtHeight = \height ->
        serial <$> call (GetBlockHeight (HeightParam (fromIntegral height)) def),
      storeLatestBlocks = serial <$> call (GetBlockLatest (NoTx True)),
      storeBlockTransactions = \hash -> serial <$> call (GetTxsBlock hash),
      storeBalances =
        batched $ \addrs -> serial <$> call (GetAddrsBalance addrs),
      storeAddressTxs = \addr count ->
        serial <$> call (GetAddrTxs addr def {limit = Just (fromIntegral count)}),
      storeTransactions =
        batched $ \tids -> serial <$> call (GetTxs tids),
      storeUnspents =
        batched $ \addrs -> serial <$> call (GetAddrsUnspent addrs everything),
      storeRawTransactions =
        batched $ \tids -> raw <$> call (GetTxsRaw tids),
      storePublish = \tx -> do
        Store.TxId published <- call (PostTx tx)
        unless (published == txHash tx) $
          throwError $ "The store returned the wrong transaction id: " <> cs (txHashToHex published)
    }
  where
    host = configHost cfg
    api = ApiConfig net (cs host)
    call req = webCall host $ storeCall ctx api req
    -- The items asked about, a hundred at a time, each request its own
    -- answer, the answers in the order asked
    batched ask items = concat <$> mapM ask (chunksOf (fromIntegral requestBatch) items)
    serial (Store.SerialList xs) = xs
    raw (Store.RawResultList xs) = xs
    -- A limit of zero asks the store for everything. Without a limit, the
    -- store answers with its default page of a hundred items and says
    -- nothing about the rest.
    everything = def {limit = Just 0}

{- Asking -}

-- | Ask the store from inside another monad, a database action usually
fromStore :: (MonadIO m) => ExceptT String IO a -> ExceptT String m a
fromStore action = ExceptT $ liftIO $ runExceptT action

-- | Why the store fails its health check, or Nothing when it passes. A
-- command over several networks asks this of each store first. It then
-- leaves out the networks whose store fails, rather than failing whole,
-- so that one store out of service does not hold back the others.
healthFailure :: Store -> IO (Maybe String)
healthFailure store = either Just (const Nothing) <$> runExceptT (storeHealth store)

-- | Ask the store several questions, at most the given number at a time,
-- and answer them in the order asked. One failed question fails them all.
inParallel :: Natural -> [ExceptT String IO a] -> ExceptT String IO [a]
inParallel atATime questions = do
  let threads = max 1 (fromIntegral atATime)
  answers <- liftIO $ pooledMapConcurrentlyN threads runExceptT questions
  liftEither $ sequence answers

-- | The raw transactions with the given ids, which the store must all have
rawTransactions :: Store -> [TxHash] -> ExceptT String IO [Tx]
rawTransactions _ [] = return []
rawTransactions store tids = do
  txs <- storeRawTransactions store tids
  when (length txs /= length tids) $
    throwError $
      "The store was asked for " <> show (length tids) <> " raw transactions but returned " <> show (length txs)
  return txs

{- Broadcasting -}

-- | How long 'broadcast' waits for the store to see a transaction, in
-- seconds
acceptanceWait :: Int
acceptanceWait = 10

-- | Hand a signed transaction to the network and wait for the store to see
-- it in its mempool, the only sign that the network accepted it: True once
-- seen, False after the wait. The transaction may then have been refused,
-- for a fee below what the nodes relay for instance, or may just be slow
-- to come back.
broadcast :: Store -> Tx -> ExceptT String IO Bool
broadcast = broadcastWith 1000000 acceptanceWait

-- | 'broadcast' asking the store the given number of times, waiting the
-- given microseconds before each
broadcastWith :: Int -> Int -> Store -> Tx -> ExceptT String IO Bool
broadcastWith pause asks store tx = do
  storePublish store tx
  seen asks
  where
    seen :: Int -> ExceptT String IO Bool
    seen 0 = return False
    seen left = do
      liftIO $ threadDelay pause
      known <- storeTransactions store [txHash tx]
      if any (\t -> t.txid == txHash tx && not t.deleted) known
        then return True
        else seen (left - 1)

-- | What broadcast answers when the store has not seen the transaction
notAccepted :: String
notAccepted =
  "The transaction was handed to the network but the store has not seen it after "
    <> show acceptanceWait
    <> " seconds. It stays signed and not online: run broadcast again later"
