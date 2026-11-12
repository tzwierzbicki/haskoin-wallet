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
import Control.Exception (ErrorCall (..), Exception, Handler (..), catches, evaluate, fromException, throwIO, try)
import Control.Lens ((%~), (&), (.~), (?~), (^.))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Get (getWord64be, isEmpty, runGetL, runGetS)
import Data.Bytes.Put (runPutL)
import Data.Bytes.Serial (deserialize, serialize)
import Data.Default (def)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate, isSuffixOf)
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
import Network.TLS (TLSException (HandshakeFailed))
import Network.HTTP.Types (StdMethod (..), hContentType)
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
  { -- | Fails when the store is not in a state to be trusted, or when the
    -- host has no store of the network at all ('HealthFailure')
    storeHealth :: ExceptT HealthFailure IO (),
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
    storePublish :: Tx -> ExceptT String IO (),
    -- | Told of each failure of the store: a question it did not answer,
    -- and an answer hw refused ('refusedAnswer'). It does nothing, unless
    -- 'notingFailures' makes it note them, for a command that must tell a
    -- failure of the store from one of its own.
    storeFailed :: String -> IO ()
  }

{- The health of a store -}

-- | Why a store cannot be asked about its network. The cases are told
-- apart because sync and recover go over every network, and a host
-- seldom serves them all: the public server has no test network of
-- Bitcoin version 3, and nobody serves a regtest chain to the public. A
-- network the host does not serve is no failure of the store, and is
-- reported as such. A store that is down is one: its network was not
-- looked at, and a payment there may have been missed. A host that hw
-- could not reach at all is told apart too, because it fails the same
-- way for every network ('servedNetworks').
data HealthFailure
  = -- | The host has no store of the network: it answered the health
    -- check with 404, Not Found. The message names the host and the
    -- network.
    NotServed !String
  | -- | The host was reached, and the store of the network answers with an
    -- error, does not answer in time, sends an answer hw cannot read, or
    -- reports that it is not well. The message says which.
    Unhealthy !String
  | -- | hw could not reach the host: the connection failed or timed out,
    -- or its TLS handshake failed ('beforeRequest'). The message says
    -- which. No part of the request was sent, so the store of every other
    -- network at the host would fail the same way.
    Unreachable !String
  deriving (Eq, Show)

-- | The message of a health failure, for a command that needs the store
healthMessage :: HealthFailure -> String
healthMessage failure = case failure of
  NotServed message -> message
  Unhealthy message -> message
  Unreachable message -> message

-- | Check the health of a store, any failure as its message: for a
-- command on one network, which stops there
checkHealth :: Store -> ExceptT String IO ()
checkHealth = withExceptT healthMessage . storeHealth

-- | The answer of a host that has no store of the network: 404 to the
-- health check
data NetworkNotServed = NetworkNotServed
  deriving (Show)

instance Exception NetworkNotServed

{- The web client -}

-- | A call of the web client to the store at the host, every failure shown
-- as a message: an error the store answered, a store hw could not reach,
-- or an answer it could not read ('webCallApart')
webCall :: (NFData a) => Text -> ExceptT Store.Except IO a -> ExceptT String IO a
webCall host = withExceptT callFailureMessage . webCallApart host

-- | Why a call of the web client failed, told apart by whether it reached
-- the host. Only the health check needs to know. A host that could not be
-- reached would fail the same way for every network, so the checks of the
-- other networks are not made ('servedNetworks').
data CallFailure
  = -- | hw could not reach the host, and sent no part of the request
    -- ('beforeRequest')
    HostUnreached !String
  | -- | The host was reached, and the call failed after that: an error the
    -- store answered, no answer in time, or an answer hw could not read
    CallFailed !String
  deriving (Eq, Show)

-- | The message of a failed call, whichever way it failed
callFailureMessage :: CallFailure -> String
callFailureMessage failure = case failure of
  HostUnreached message -> message
  CallFailed message -> message

-- | 'webCall', with its failures told apart by whether the host was
-- reached. The web client decodes an answer only once it is used, so the
-- answer is read in full here, where its failure is caught.
webCallApart :: (NFData a) => Text -> ExceptT Store.Except IO a -> ExceptT CallFailure IO a
webCallApart host action =
  ExceptT $ (answered =<< runExceptT action) `catches` [Handler unreachable, Handler unreadable, Handler tooLong]
  where
    answered (Left err) = return $ Left (CallFailed (storeError host err))
    answered (Right answer) = Right <$> evaluate (force answer)
    unreachable e
      | beforeRequest e = return $ Left (HostUnreached (unreachableStore host e))
      | otherwise = return $ Left (CallFailed (unreachableStore host e))
    unreadable (ErrorCall reason) = return $ Left (CallFailed (unreadableAnswer host reason))
    tooLong AnswerTooLong =
      return $ Left (CallFailed ("The store at " <> cs host <> " sent an answer of more than " <> show (answerLimit `div` (1024 * 1024)) <> " MiB, which hw does not read"))

-- | Whether the web client failed before it sent any part of the request.
-- That is when the connection to the host failed or timed out, or when
-- its TLS handshake failed. Up to there, only the scheme, the name and the
-- port of the host count. The path, which names the network, is not sent
-- yet. So the call for every other network at the host would fail the
-- same way.
--
-- Every other failure comes after the request was sent, and may be of one
-- network alone. A proxy in front of several stores sends the path of
-- each network to a store of its own, and one of them can be slow or
-- down. A store that does not answer in time is such a failure, and so is
-- a TLS failure after the handshake.
beforeRequest :: HttpException -> Bool
beforeRequest e = case e of
  HttpExceptionRequest _ (ConnectionFailure _) -> True
  HttpExceptionRequest _ ConnectionTimeout -> True
  -- http-client passes the failures of the TLS library on inside its
  -- catch-all constructor, which holds other failures too
  HttpExceptionRequest _ (InternalException inner) -> handshakeFailed inner
  _ -> False
  where
    handshakeFailed inner = case fromException inner of
      Just (HandshakeFailed _) -> True
      _ -> False

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
-- ('apiCall'), its answer decoded as the client decodes it
-- ('storeAnswer')
storeCall :: (ApiResource a b) => Ctx -> ApiConfig -> a -> ExceptT Store.Except IO b
storeCall ctx api res = runGetL deserialize <$> storeAnswer ctx api res

-- | The body of the answer to a call to the store, not decoded yet, and
-- read no further than 'answerLimit' bytes. The request is the one of the
-- web client of haskoin-store: the path and the query of the resource,
-- binary answers, and an error the store answers read from its body. Most
-- answers are decoded as the client decodes them ('storeCall'). The raw
-- transactions are decoded the way of hw, which reads the formats of
-- every version of the store ('rawTxsAnswer').
storeAnswer :: (ApiResource a b) => Ctx -> ApiConfig -> a -> ExceptT Store.Except IO BL.ByteString
storeAnswer ctx (ApiConfig net apiHost) res = do
  query <- liftEither $ queryOptions net ctx res
  let url = resourceUrl apiHost net (cs (queryPath net ctx res))
      options = appEndo (query <> binary) (Wreq.defaults & Wreq.manager %~ either (Left . boundedAnswers) Right)
  case resourceMethod (asProxy res) of
    GET -> answerBody =<< liftIO (try (Wreq.getWith options url))
    POST -> case resourceBody res of
      Just (PostBox body) -> answerBody =<< liftIO (try (Wreq.postWith options url (runPutL (serialize body))))
      Nothing -> throwError $ Store.StringError "Could not post resource"
    _ -> throwError $ Store.StringError "Unsupported HTTP method"
  where
    answerBody :: Either Store.Except (Wreq.Response BL.ByteString) -> ExceptT Store.Except IO BL.ByteString
    answerBody = either throwError (return . (^. Wreq.responseBody))
    binary = Endo $ (Wreq.header "Accept" .~ ["application/octet-stream"]) . (Wreq.checkResponse ?~ checkStatus)

-- | The web address of a resource of the store of a network. It is the
-- host, then the name of the network, then the path of the resource, such
-- as https://api.haskoin.com/btc/block/best.
--
-- Users type the host with a slash at the end or without one. The web
-- client of haskoin-store puts the name of the network right after the
-- host, so it needs that slash. Without it, http://localhost:3000 gave
-- http://localhost:3000btc/block/best, which names no host at all. So hw
-- adds the slash when the host has none, and keeps the host as it is
-- otherwise. A host with a path keeps its path in front of the network.
-- That is the case of a proxy at http://proxy.example/stores, for
-- instance.
--
-- The slash is added here, where the address is made, and not when the
-- configuration is read. So every store hw makes gets it, and the
-- messages about the store name the host as the user typed it.
resourceUrl :: String -> Network -> String -> String
resourceUrl host net path = withSlash <> net.name <> path
  where
    withSlash
      | "/" `isSuffixOf` host = host
      | otherwise = host <> "/"

-- | The raw transactions in the answer of the store to 'GetTxsRaw', in
-- either of the formats of haskoin-store, or why they could not be read.
-- From version 1.9.0, the store writes the number of transactions first,
-- in eight bytes, as it writes every other list. Before, it wrote the
-- transactions alone, one after the other. The public server can run both
-- versions at once, one on each network, so hw reads both. The web client
-- of haskoin-store reads only one of them, the one of its own version.
--
-- The count is tried first, and must match the transactions that follow
-- it to the end of the answer. An answer of an older store cannot pass
-- for one with a count. Its first eight bytes would count billions of
-- transactions, unless its first transaction has version 0, which nodes
-- do not relay. Even then, the bytes after the first eight would have to
-- make up that many whole transactions. An answer with a count is never
-- read the other way, since it always reads with its count.
rawTxsAnswer :: BL.ByteString -> Either String [Tx]
rawTxsAnswer body = case runGetS counted bytes of
  Right txs -> Right txs
  Left _ -> either (const (Left inNeitherFormat)) Right (runGetS toTheEnd bytes)
  where
    bytes = BL.toStrict body
    counted = do
      count <- getWord64be
      txs <- toTheEnd
      unless (fromIntegral (length txs) == count) $ fail "the count does not match the transactions"
      return txs
    -- Transactions one after the other, up to the end of the answer
    toTheEnd = do
      done <- isEmpty
      if done then return [] else (:) <$> deserialize <*> toTheEnd
    inNeitherFormat = "the list of raw transactions is in neither format of haskoin-store, with its count first or without"

-- | The query of a resource, as the web client of haskoin-store writes it
queryOptions :: (ApiResource a b) => Network -> Ctx -> a -> Either Store.Except (Endo Wreq.Options)
queryOptions net ctx res = mconcat <$> mapM option (snd (queryParams res))
  where
    option (ParamBox p) = do
      values <- maybe (Left (Store.UserError "Invalid Param")) Right (encodeParam net ctx p)
      return $ Endo $ Wreq.param (paramLabel p) .~ [Text.intercalate "," values]

-- | Refuse an answer of the store that is not a success, with the error
-- the store gave in its body, as the web client of haskoin-store does
-- ('answeredError'). A health check answers 503 with its report when the
-- store is not well. A health check answered with 404 comes from a host
-- that has no store of the network: 'NetworkNotServed'.
checkStatus :: Request -> HTTP.Response HTTP.BodyReader -> IO ()
checkStatus req res
  | statusIsSuccessful status = return ()
  | isHealthCheck && statusCode status == 503 = return ()
  | isHealthCheck && statusCode status == 404 = throwIO NetworkNotServed
  | otherwise = do
      body <- HTTP.responseBody res
      throwIO $ fromMaybe (Store.StringError "could not decode error") (answeredError contentType body)
  where
    contentType = lookup hContentType (HTTP.responseHeaders res)
    status = HTTP.responseStatus res
    isHealthCheck = "/health" `Text.isSuffixOf` cs (HTTP.path req)

-- | The error in the body of an answer of the store, read as the
-- Content-Type of the answer says. haskoin-store sends its errors in
-- binary from version 1.9.0, to a client that asks for binary answers as
-- hw does. Older versions sent every error in JSON. The public server can
-- run both versions at once, one on each network. Nothing when the body is
-- no error of haskoin-store, as the error page of a proxy in front of a
-- store that is down.
answeredError :: Maybe BS.ByteString -> BS.ByteString -> Maybe Store.Except
answeredError contentType body
  | binary = either (const Nothing) Just (runGetS deserialize body)
  | otherwise = Json.decodeStrict body
  where
    binary = maybe False ("application/octet-stream" `BS.isPrefixOf`) contentType

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

-- | The message of an answer of the store that hw could not read, and
-- why
unreadableAnswer :: Text -> String -> String
unreadableAnswer host reason = "The store at " <> cs host <> " gave an answer hw could not read: " <> reason

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
        -- A host without a store of the network answers 404, which is
        -- told apart from every other failure. So is a host hw could not
        -- reach at all.
        answer <- liftIO $ try (runExceptT (webCallApart host (storeCall ctx api GetHealth)))
        case answer of
          Left NetworkNotServed -> throwError (NotServed ("The store at " <> cs host <> " does not serve " <> net.name))
          Right (Left (HostUnreached failure)) -> throwError (Unreachable failure)
          Right (Left (CallFailed failure)) -> throwError (Unhealthy failure)
          Right (Right health) ->
            unless (Store.isOK health) $
              throwError (Unhealthy "The indexer health check has failed"),
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
        batched $ \tids -> do
          body <- webCall host (storeAnswer ctx api (GetTxsRaw tids))
          either (throwError . unreadableAnswer host) return (rawTxsAnswer body),
      storePublish = \tx -> do
        Store.TxId published <- call (PostTx tx)
        unless (published == txHash tx) $
          throwError $ "The store returned the wrong transaction id: " <> cs (txHashToHex published),
      storeFailed = const (return ())
    }
  where
    host = configHost cfg
    api = ApiConfig net (cs host)
    call req = webCall host $ storeCall ctx api req
    -- The items asked about, a hundred at a time, each request its own
    -- answer, the answers in the order asked
    batched ask items = concat <$> mapM ask (chunksOf (fromIntegral requestBatch) items)
    serial (Store.SerialList xs) = xs
    -- A limit of zero asks the store for everything. Without a limit, the
    -- store answers with its default page of a hundred items and says
    -- nothing about the rest.
    everything = def {limit = Just 0}

{- Failures of the store -}

-- | The store, noting the message of each of its failures in the list, in
-- the order they happen: each question it did not answer, its health
-- check included, and each answer hw refused ('refusedAnswer'). A command
-- over several networks tells this way a failure of a store, after which
-- it goes on with the other networks, from a failure of its own, after
-- which it stops ('storeFailureApart').
--
-- A failure of the store is a fact of the network. Its message alone
-- would not tell it: the question a scan asks at its limit says "The
-- store reports ..." too, and the answer of the user to it is no failure
-- of the store.
notingFailures :: IORef [String] -> Store -> Store
notingFailures noted store =
  store
    { storeHealth = storeHealth store `catchError` \failure -> liftIO (note (healthMessage failure)) >> throwError failure,
      storeBestBlock = noted' (storeBestBlock store),
      storeBlocksAtHeight = noted' . storeBlocksAtHeight store,
      storeLatestBlocks = noted' (storeLatestBlocks store),
      storeBlockTransactions = noted' . storeBlockTransactions store,
      storeBalances = noted' . storeBalances store,
      storeAddressTxs = \addr count -> noted' (storeAddressTxs store addr count),
      storeTransactions = noted' . storeTransactions store,
      storeUnspents = noted' . storeUnspents store,
      storeRawTransactions = noted' . storeRawTransactions store,
      storePublish = noted' . storePublish store,
      storeFailed = \failure -> storeFailed store failure >> note failure
    }
  where
    -- The questions may be asked in parallel ('inParallel')
    note failure = atomicModifyIORef' noted (\failures -> (failures <> [failure], ()))
    noted' :: ExceptT String IO a -> ExceptT String IO a
    noted' question = question `catchError` \failure -> liftIO (note failure) >> throwError failure

-- | Run something the store answers, the checks of its answer included:
-- any failure of it is a failure of the store, which it is told of
-- ('storeFailed'), as if it had not answered. An answer whose counts do
-- not match, or that is about other things than those asked, is wrong on
-- the side of the store: a transaction that came in between two
-- questions, or a store that lies. The action must ask the store and
-- check its answer, and do nothing else: a failure of the database in it
-- would pass for one of the store.
answeredBy :: (MonadIO m) => Store -> ExceptT String m a -> ExceptT String m a
answeredBy store action = action `catchError` \failure -> liftIO (storeFailed store failure) >> throwError failure

-- | Refuse an answer of the store, for the reason given as Left: a
-- failure of the store ('answeredBy')
refusedAnswer :: (MonadIO m) => Store -> Either String a -> ExceptT String m a
refusedAnswer store = answeredBy store . liftEither

-- | Run an action on the store, and tell a failure of the store from any
-- other one. A failure of the store, a question it did not answer or an
-- answer hw refused, is Left with its message: the action ended there. A
-- failure of anything else fails the action: the user who answers no to
-- a question, or a row of the database that does not read. The action is
-- given the store that notes its failures ('notingFailures').
--
-- sync and recover go over every network this way. When the store of a
-- network fails, they report that network and go on with the others,
-- since the other stores have nothing to do with it. A failure of their
-- own would fail on every network alike, and stops them.
storeFailureApart :: (MonadIO m) => Store -> (Store -> ExceptT String m a) -> ExceptT String m (Either String a)
storeFailureApart store action = do
  noted <- liftIO $ newIORef []
  result <- lift $ runExceptT (action (notingFailures noted store))
  case result of
    Right answer -> return (Right answer)
    Left failure -> do
      byStore <- liftIO $ readIORef noted
      if failure `elem` byStore then return (Left failure) else throwError failure

{- Asking -}

-- | Ask the store from inside another monad, a database action usually
fromStore :: (MonadIO m) => ExceptT String IO a -> ExceptT String m a
fromStore action = ExceptT $ liftIO $ runExceptT action

-- | Why the store fails its health check, or Nothing when it passes. A
-- command over several networks asks this of each store first. It then
-- leaves out the networks whose store fails, rather than failing whole,
-- so that one store out of service does not hold back the others.
healthFailure :: Store -> IO (Maybe HealthFailure)
healthFailure store = either Just (const Nothing) <$> runExceptT (storeHealth store)

-- | What the health checks of the stores of several networks say, each
-- network in one of three groups, in the order given: those to work on,
-- those the host does not serve, and those whose store fails, with the
-- failure. The networks after one whose host hw could not reach are among
-- those that fail, with the same failure ('servedNetworks').
data Served = Served
  { servedUp :: ![Network],
    servedNot :: ![Network],
    servedDown :: ![(Network, String)]
  }
  deriving (Eq, Show)

-- | Ask the store of each network whether it is up, in the order given.
--
-- Once hw could not reach the host ('Unreachable'), it asks no further.
-- The stores of the networks after are at the same host, and would fail
-- the same way, so they are down with the same failure. The web client
-- waits 30 seconds for a connection. Asked again for each of the eight
-- networks of a portfolio, a host that does not answer would take four
-- minutes.
--
-- A store that answers that it is down tells nothing of the others, and
-- the checks go on. A proxy can send each network to a store of its own.
servedNetworks :: (Network -> Store) -> [Network] -> IO Served
servedNetworks storeOn nets = do
  checked <- checkUntilUnreachable nets
  return
    Served
      { servedUp = [net | (net, Nothing) <- checked],
        servedNot = [net | (net, Just (NotServed _)) <- checked],
        servedDown = [(net, healthMessage failure) | (net, Just failure) <- checked, isDown failure]
      }
  where
    checkUntilUnreachable [] = return []
    checkUntilUnreachable (net : rest) = do
      failure <- healthFailure (storeOn net)
      case failure of
        Just (Unreachable _) -> return ((net, failure) : [(other, failure) | other <- rest])
        _ -> ((net, failure) :) <$> checkUntilUnreachable rest
    isDown (NotServed _) = False
    isDown _ = True

-- | Refuse to go on when no network can be worked on: every store that
-- is there fails, with the failure of the first, or the host serves none
-- of the networks asked for, which the refusal names. A host that serves
-- none of the networks of hw is what a wrong host looks like.
requireSomeNetwork :: Text -> Served -> Either String ()
requireSomeNetwork host served
  | not (null (servedUp served)) = Right ()
  | (_, failure) : _ <- servedDown served = Left failure
  | otherwise =
      Left $
        "The store at "
          <> cs host
          <> " does not serve "
          <> intercalate ", " ((.name) <$> servedNot served)
          <> ": check the host in the configuration file"

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
