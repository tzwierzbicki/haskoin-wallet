{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The store, as the wallet asks it: a record of questions that hw fills
-- with the web client of a haskoin-store ('webStore') and that the tests
-- fill with scripted answers, so that the sync and the commands can be
-- played out without a network. Sending a transaction through it and
-- waiting for the store to see it is here too.
module Haskoin.Wallet.Store where

import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData, force)
import Control.Exception (ErrorCall (..), Handler (..), catches, evaluate)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..))
import Data.Default (def)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Config
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
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
  ExceptT $ (answered =<< runExceptT action) `catches` [Handler unreachable, Handler unreadable]
  where
    answered (Left err) = return $ Left (show (err :: Store.Except))
    answered (Right answer) = Right <$> evaluate (force answer)
    unreachable e = return $ Left (unreachableStore host e)
    unreadable (ErrorCall reason) =
      return $ Left ("The store at " <> cs host <> " gave an answer hw could not read: " <> reason)

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

-- | The store of hw: the web API of a haskoin-store, with the batch sizes
-- of the configuration
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
      storeBalances = \addrs ->
        serial <$> batch (configAddrBatch cfg) (GetAddrsBalance addrs),
      storeAddressTxs = \addr count ->
        serial <$> call (GetAddrTxs addr def {limit = Just (fromIntegral count)}),
      storeTransactions = \tids ->
        serial <$> batch (configTxFullBatch cfg) (GetTxs tids),
      storeUnspents = \addrs ->
        serial <$> batch (configCoinBatch cfg) (GetAddrsUnspent addrs everything),
      storeRawTransactions = \tids ->
        raw <$> batch (configTxFullBatch cfg) (GetTxsRaw tids),
      storePublish = \tx -> do
        Store.TxId published <- call (PostTx tx)
        unless (published == txHash tx) $
          throwError $ "The store returned the wrong transaction id: " <> cs (txHashToHex published)
    }
  where
    host = configHost cfg
    api = ApiConfig net (cs host)
    call req = webCall host $ apiCall ctx api req
    batch size req = webCall host $ apiBatch ctx size api req
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

-- | What sendtx answers when the store has not seen the transaction
notAccepted :: String
notAccepted =
  "The transaction was handed to the network but the store has not seen it after "
    <> show acceptanceWait
    <> " seconds. It stays signed and not online: run sendtx again later"
