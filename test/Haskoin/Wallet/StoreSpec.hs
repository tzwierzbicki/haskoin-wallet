{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The web client of hw, as it tells why it could not ask the store: an
-- error the store answered, a store out of reach, or an answer it could
-- not read, and whether it reached the host at all. Then the answers of
-- the store, which are JSON: the raw transactions, the health check, and
-- the error in an answer. Then the health checks of the stores of several
-- networks, which tell a network the host does not serve from a store
-- that is down, and stop at a host hw could not reach.
-- The questions themselves are played out against scripted stores in the
-- specs of the sync and of the pending transactions.
module Haskoin.Wallet.StoreSpec (spec) where

import Control.Exception (ErrorCall (..), throwIO, toException)
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.String.Conversions (cs)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import qualified Haskoin.Store.Data as Store
import Haskoin (Network, bch, bchRegTest, btc, btcRegTest, btcTest, prepareContext, txHash, txHashToHex)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.HwTestUtils (withServerAnswering, withServerCountingConnections)
import Haskoin.Wallet.Store
import Haskoin.Wallet.TestUtils (emptyStore, fakeTxHash, notServed, otherAddr, replaced, spendingTx, storeTx, toStore, typedTxid, unhealthy, unreachableHost)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), defaultRequest)
import Network.TLS (AlertDescription (..), TLSError (..), TLSException (..))
import System.Timeout (timeout)
import Test.Hspec
import Text.RawString.QQ (r)

-- | A call of the web client to the store at http://store.test/
called :: ExceptT Store.Except IO [Word64] -> IO (Either String [Word64])
called = runExceptT . webCall "http://store.test/"

-- | A call that fails as the web client fails on a request it could not
-- complete
failingWith :: HttpExceptionContent -> ExceptT Store.Except IO [Word64]
failingWith content = liftIO $ throwIO (HttpExceptionRequest defaultRequest content)

spec :: Spec
spec = do
  callSpec
  urlSpec
  answeredErrorSpec
  reachedSpec
  answerSpec
  connectionsSpec
  healthSpec
  failureSpec
  broadcastSpec

callSpec :: Spec
callSpec =
  describe "A call of the web client" $ do
    it "gives the answer of the store" $
      called (return [5, 7]) `shouldReturn` Right [5, 7]

    it "fails with the error the store answered, naming the store" $
      called (throwError Store.ServerError)
        `shouldReturn` Left "The store at http://store.test/ answered with an error: ServerError"

    -- The web client fails with this error on an error page that is not
    -- the JSON of an error of haskoin-store: hw showed it as it is before
    it "fails with an error it could not read, as the error page of a proxy in front of a store that is down" $
      called (throwError (Store.StringError "could not decode error"))
        `shouldReturn` Left "The store at http://store.test/ answered with an error hw could not read"

    it "fails when the connection to the store fails" $
      called (failingWith (ConnectionFailure (toException (userError "Connection refused"))))
        `shouldReturn` Left "The store at http://store.test/ could not be reached: the connection failed"

    it "fails when the connection to the store times out" $
      called (failingWith ConnectionTimeout)
        `shouldReturn` Left "The store at http://store.test/ could not be reached: the connection timed out"

    it "fails when the store does not answer in time" $
      called (failingWith ResponseTimeout)
        `shouldReturn` Left "The store at http://store.test/ could not be reached: it did not answer in time"

    it "fails when the address of the store is not valid" $
      called (liftIO (throwIO (InvalidUrlException "store.test/btc/health" "Invalid URL")))
        `shouldReturn` Left "The store at http://store.test/ could not be reached: the address store.test/btc/health is not valid: Invalid URL"

    -- A reader of haskoin-store-data can fail with an error call, and only
    -- once the value it read is used, as this answer does in its second
    -- number
    it "fails with an answer it could not read, wherever in the answer" $
      called (return [5, error "Prelude.read: no parse"])
        `shouldReturn` Left "The store at http://store.test/ gave an answer hw could not read: Prelude.read: no parse"

-- | The error in an answer of the store, which is JSON
answeredErrorSpec :: Spec
answeredErrorSpec =
  describe "The error in an answer of the store" $ do
    it "is read from the JSON of its body" $
      answeredError [r|{"error":"not-found-or-invalid-arg","message":"Item not found"}|]
        `shouldBe` Just Store.ThingNotFound

    it "keeps the reason the store gives" $
      answeredError [r|{"error":"user-error","message":"Failed to parse request body"}|]
        `shouldBe` Just (Store.UserError "Failed to parse request body")

    it "is none for the error page of a proxy in front of a store that is down" $
      answeredError "<html><body><h1>502 Bad Gateway</h1></body></html>"
        `shouldBe` Nothing

    -- What the public server answered for a network whose store was down
    it "is none for JSON that names no error of haskoin-store" $
      answeredError [r|{"error":"Bad Gateway"}|] `shouldBe` Nothing

-- | The failures of a call, told apart by whether the host was reached.
-- The health check needs to know: a host that could not be reached would
-- fail the same way for every network.
reachedSpec :: Spec
reachedSpec =
  describe "A call of the web client, for a health check" $ do
    it "tells that the host could not be reached when the connection fails" $
      runExceptT (webCallApart "http://store.test/" (failingWith (ConnectionFailure (toException (userError "Connection refused")))))
        `shouldReturn` Left (HostUnreached "The store at http://store.test/ could not be reached: the connection failed")

    it "tells that the host could not be reached when the connection times out" $
      runExceptT (webCallApart "http://store.test/" (failingWith ConnectionTimeout))
        `shouldReturn` Left (HostUnreached "The store at http://store.test/ could not be reached: the connection timed out")

    -- What hw gets from a store that speaks plain HTTP, at a host given
    -- as https://
    it "tells that the host could not be reached when the TLS handshake fails" $
      runExceptT (webCallApart "http://store.test/" (failingWith (InternalException (toException (HandshakeFailed (Error_Protocol "record exceeding maximum size" RecordOverflow))))))
        `shouldReturn` Left (HostUnreached "The store at http://store.test/ could not be reached: InternalException (HandshakeFailed (Error_Protocol \"record exceeding maximum size\" RecordOverflow))")

    -- The request was sent, and a proxy can send the path of each network
    -- to a store of its own
    it "tells that the host was reached when the store does not answer in time" $
      runExceptT (webCallApart "http://store.test/" (failingWith ResponseTimeout))
        `shouldReturn` Left (CallFailed "The store at http://store.test/ could not be reached: it did not answer in time")

    -- http-client wraps more than the failures of TLS this way
    it "does not take any other failure inside the web client for a host it could not reach" $
      runExceptT (webCallApart "http://store.test/" (failingWith (InternalException (toException (ErrorCall "boom")))))
        `shouldReturn` Left (CallFailed "The store at http://store.test/ could not be reached: InternalException boom")

-- | The answers of the store, which hw reads from JSON. haskoin-store
-- answers in JSON alone from version 1.10, and every older version answers
-- in JSON when asked.
answerSpec :: Spec
answerSpec =
  prepareContext $ \ctx ->
    describe "An answer of the web store" $ do
      -- The store lists each raw transaction as a string of hex. This one
      -- is the first transaction of Bitcoin, mined on 3 January 2009.
      it "gives the raw transactions of a list of transactions in hex" $
        withServerAnswering
          [r|["01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000"]|]
          $ \_ url -> do
            storeOn <- webStores ctx (def :: Config) {configHost = cs url}
            runExceptT (map (txHashToHex . txHash) <$> storeRawTransactions (storeOn btc) [typedTxid "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"])
              `shouldReturn` Right ["4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"]

      -- The best block is an object, and this server answers a list
      it "fails as an answer hw could not read when it is not the JSON of what was asked" $
        withServerAnswering "[]" $ \_ url -> do
          storeOn <- webStores ctx (def :: Config) {configHost = cs url}
          failed <- runExceptT (storeBestBlock (storeOn btc))
          failed `shouldSatisfy` either (isPrefixOf ("The store at " <> url <> " gave an answer hw could not read: ")) (const False)

-- | The connections of a run of hw to the store: a sync asks a hundred
-- questions, and a connection opened for each would read the
-- certificates of the system and shake hands with the server each time
connectionsSpec :: Spec
connectionsSpec =
  prepareContext $ \ctx ->
    describe "The connections of a run of hw to the web store" $
      -- The server answers that it knows none of the transactions asked.
      -- Two questions go to the store of Bitcoin, and one to that of
      -- Bitcoin Cash, at the same host.
      it "carry every request of the run over the one connection the first request opened" $
        withServerCountingConnections "[]" $ \url opened -> do
          storeOn <- webStores ctx (def :: Config) {configHost = cs url}
          _ <- runExceptT (storeRawTransactions (storeOn btc) [fakeTxHash 1])
          _ <- runExceptT (storeRawTransactions (storeOn btc) [fakeTxHash 2])
          _ <- runExceptT (storeRawTransactions (storeOn bch) [fakeTxHash 3])
          opened `shouldReturn` 1

-- | The web address of the store of a network, made from the host of the
-- configuration. Users type the host with a slash at the end or without.
urlSpec :: Spec
urlSpec =
  describe "The web address of the store" $ do
    it "puts a slash between a host without one at the end and the network" $
      resourceUrl "http://localhost:3000" btc "/block/best"
        `shouldBe` "http://localhost:3000/btc/block/best"

    it "adds no second slash after a host that ends with one" $
      resourceUrl "https://api.haskoin.com/" btc "/block/best"
        `shouldBe` "https://api.haskoin.com/btc/block/best"

    -- A proxy that serves its stores under a path of its own
    it "keeps the path of the host in front of the network" $
      resourceUrl "http://proxy.test/stores" btc "/block/best"
        `shouldBe` "http://proxy.test/stores/btc/block/best"

    -- The server answers that it knows none of the transactions asked. The
    -- host is its address with the slash at the end taken off.
    prepareContext $ \ctx ->
      it "is reached with a host typed without a slash at the end" $
        withServerAnswering "[]" $ \_ url -> do
          let hostWithoutSlash = cs (init url)
          storeOn <- webStores ctx (def :: Config) {configHost = hostWithoutSlash}
          runExceptT (storeRawTransactions (storeOn btc) [fakeTxHash 1])
            `shouldReturn` Right []

-- | The health checks of the stores of several networks. sync and recover
-- go over every network, and no host serves them all.
healthSpec :: Spec
healthSpec = do
  describe "The health check of a store" $ do
    it "passes for a store that is up" $
      healthFailure up `shouldReturn` Nothing

    it "tells a store that is down, with its failure" $
      healthFailure (unhealthy up) `shouldReturn` Just (Unhealthy "The store is down")

    it "tells a network the host does not serve apart from a store that is down" $
      healthFailure (notServed up) `shouldReturn` Just (NotServed "The host does not serve the network")

    -- Nothing listens at port 1 of this computer, so the connection is
    -- refused
    prepareContext $ \ctx ->
      it "of the web store tells a host hw could not reach apart from a store that is down" $ do
        storeOn <- webStores ctx (def :: Config) {configHost = "http://127.0.0.1:1/"}
        runExceptT (storeHealth (storeOn btc))
          `shouldReturn` Left (Unreachable "The store at http://127.0.0.1:1/ could not be reached: the connection failed")

    -- What a haskoin-store of version 1.10.1 answered on the fourth test
    -- network of Bitcoin
    prepareContext $ \ctx ->
      it "of the web store passes for the JSON of a store that is up" $
        withServerAnswering
          [r|{"blocks":{"blocks":160880,"diff":0,"headers":160880,"max":3,"ok":true},"last-block":{"age":335,"max":14400,"ok":true},"last-tx":{"age":1,"max":1800,"ok":true},"net":"btctest4","ok":true,"peers":{"count":5,"min":1,"ok":true},"pending-txs":{"count":0,"max":10000,"ok":true},"time":"2026-11-12 16:54:00 UTC","version":"1.10.1"}|]
          $ \_ url -> do
            storeOn <- webStores ctx (def :: Config) {configHost = cs url}
            runExceptT (storeHealth (storeOn btc)) `shouldReturn` Right ()

    -- The same store without a peer, where it needs one
    prepareContext $ \ctx ->
      it "of the web store tells a store whose JSON reports that it is not well" $
        withServerAnswering
          [r|{"blocks":{"blocks":160880,"diff":0,"headers":160880,"max":3,"ok":true},"last-block":{"age":335,"max":14400,"ok":true},"last-tx":{"age":1,"max":1800,"ok":true},"net":"btctest4","ok":false,"peers":{"count":0,"min":1,"ok":false},"pending-txs":{"count":0,"max":10000,"ok":true},"time":"2026-11-12 16:54:00 UTC","version":"1.10.1"}|]
          $ \_ url -> do
            storeOn <- webStores ctx (def :: Config) {configHost = cs url}
            runExceptT (storeHealth (storeOn btc)) `shouldReturn` Left (Unhealthy "The indexer health check has failed")

    -- The same store, with a time that is none. haskoin-store-data reads
    -- the time only once it is used, and fails there with an error call.
    -- hw reads the answer in full when it comes, so the failure is told
    -- there, and crashes no command later.
    prepareContext $ \ctx ->
      it "of the web store tells a store whose health check has a time it cannot read as down, without a crash" $
        withServerAnswering
          [r|{"blocks":{"blocks":160880,"diff":0,"headers":160880,"max":3,"ok":true},"last-block":{"age":335,"max":14400,"ok":true},"last-tx":{"age":1,"max":1800,"ok":true},"net":"btctest4","ok":true,"peers":{"count":5,"min":1,"ok":true},"pending-txs":{"count":0,"max":10000,"ok":true},"time":"soon","version":"1.10.1"}|]
          $ \_ url -> do
            storeOn <- webStores ctx (def :: Config) {configHost = cs url}
            failed <- runExceptT (storeHealth (storeOn btc))
            failed `shouldSatisfy` \answer -> case answer of
              Left (Unhealthy message) -> ("The store at " <> url <> " gave an answer hw could not read: ") `isPrefixOf` message
              _ -> False

    -- A command on one network stops there, whichever way its store fails
    it "fails a command on one network with the message of either failure" $ do
      runExceptT (checkHealth up) `shouldReturn` Right ()
      runExceptT (checkHealth (unhealthy up)) `shouldReturn` Left "The store is down"
      runExceptT (checkHealth (notServed up)) `shouldReturn` Left "The host does not serve the network"

  describe "The health checks of the stores of several networks" $ do
    -- btc is up, the host has no btctest, and the store of bch is down
    it "sort each network: those to work on, those the host does not serve, and those whose store is down with its failure" $
      servedNetworks mixed [btc, btcTest, bch]
        `shouldReturn` Served {servedUp = [btc], servedNot = [btcTest], servedDown = [(bch, "The store is down")]}

    -- The store of btctest would have answered that it is up
    it "stop at a host hw could not reach, and name its network and every network after it as down with its failure" $
      servedNetworks hostGoneAtBch [btc, bch, btcTest]
        `shouldReturn` Served
          { servedUp = [btc],
            servedNot = [],
            servedDown =
              [ (bch, "The store at https://api.haskoin.com/ could not be reached: the connection timed out"),
                (btcTest, "The store at https://api.haskoin.com/ could not be reached: the connection timed out")
              ]
          }

    it "ask no store of the networks after a host hw could not reach" $ do
      asked <- newIORef []
      let noting net = (hostGoneAtBch net) {storeHealth = liftIO (modifyIORef asked (<> [net])) >> storeHealth (hostGoneAtBch net)}
      _ <- servedNetworks noting [btc, bch, btcTest, btcRegTest]
      readIORef asked `shouldReturn` [btc, bch]

    -- The host was reached, and a proxy can send each network to a store
    -- of its own
    it "go on after a store that answers that it is down" $
      servedNetworks mixed [bch, btc]
        `shouldReturn` Served {servedUp = [btc], servedNot = [], servedDown = [(bch, "The store is down")]}

    it "keep the networks of each group in the order asked" $
      servedNetworks (const up) [bch, btcTest, btc]
        `shouldReturn` Served {servedUp = [bch, btcTest, btc], servedNot = [], servedDown = []}

    it "let a command go on when one network at least can be worked on" $ do
      served <- servedNetworks mixed [btc, btcTest, bch]
      requireSomeNetwork "http://store.test/" served `shouldBe` Right ()

    -- Nothing to work on, and a store that is there fails: its failure
    -- is what the user must see
    it "stop a command with the failure of the first store that is down, when no network can be worked on" $ do
      served <- servedNetworks mixed [btcTest, bch]
      requireSomeNetwork "http://store.test/" served `shouldBe` Left "The store is down"

    -- What a wrong host looks like
    it "stop a command when the host serves none of the networks asked for, naming them and the host" $ do
      served <- servedNetworks (const (notServed up)) [btcRegTest, bchRegTest]
      requireSomeNetwork "http://store.test/" served
        `shouldBe` Left "The store at http://store.test/ does not serve btcreg, bchreg: check the host in the configuration file"
  where
    up = toStore emptyStore
    mixed :: Network -> Store
    mixed net
      | net == btc = up
      | net == bch = unhealthy up
      | otherwise = notServed up
    -- Every store is up, but hw cannot reach the host when it asks about bch
    hostGoneAtBch :: Network -> Store
    hostGoneAtBch net
      | net == bch = unreachableHost up
      | otherwise = up

-- | A command over several networks goes on with the others when the
-- store of one fails, and stops at any other failure: it tells the two
-- apart
failureSpec :: Spec
failureSpec =
  describe "A failure during the work on a store" $ do
    let down = (toStore emptyStore) {storeBestBlock = throwError "The store at http://store.test/ could not be reached: the connection failed"}
        bestBlock store = fromStore (storeBestBlock store)

    it "is given apart when it is a question the store did not answer" $
      runExceptT (storeFailureApart down (fmap (const ()) . bestBlock))
        `shouldReturn` Right (Left "The store at http://store.test/ could not be reached: the connection failed")

    it "is given apart when it is an answer hw refused" $
      runExceptT (storeFailureApart (toStore emptyStore) (\store -> refusedAnswer store (Left "The store counts 2 coins for the addresses to update but listed 1" :: Either String ())))
        `shouldReturn` Right (Left "The store counts 2 coins for the addresses to update but listed 1")

    -- The user answered no to the question of a scan
    it "fails the work when it is not the store's" $
      runExceptT (storeFailureApart (toStore emptyStore) (\_ -> throwError "The scan stopped there." :: ExceptT String IO ()))
        `shouldReturn` Left "The scan stopped there."

    -- The store failed first, and the work went on to fail otherwise
    it "fails the work when it ends on a failure that is not the store's, though the store failed before" $
      runExceptT (storeFailureApart down (\store -> (fmap (const ()) (bestBlock store) `catchError` \_ -> return ()) >> throwError "The scan stopped there." :: ExceptT String IO ()))
        `shouldReturn` Left "The scan stopped there."

    it "gives the answer of the work when nothing failed" $
      runExceptT (storeFailureApart (toStore emptyStore) (\_ -> return (2 :: Int)))
        `shouldReturn` (Right (Right 2) :: Either String (Either String Int))

-- | Sending a transaction: the store says nothing of what its peers made
-- of it, only listing it afterwards tells that the network took it
broadcastSpec :: Spec
broadcastSpec =
  prepareContext $ \ctx ->
    describe "A transaction handed to the network" $ do
      let tx = spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 20000)]
          -- A store that lists the transactions given, whatever it is asked
          listing listed = (toStore emptyStore) {storeTransactions = \_ -> return listed}
      it "is seen once the store lists it" $
        runExceptT (broadcastWith 0 1 (listing [storeTx (txHash tx) (Store.MemRef 0) [] [] 0]) tx) `shouldReturn` Right True
      -- A transaction replaced or dropped stays in the store, marked deleted
      it "is not seen while the store lists it as deleted" $
        runExceptT (broadcastWith 0 1 (listing [replaced (storeTx (txHash tx) (Store.MemRef 0) [] [] 0)]) tx) `shouldReturn` Right False
      -- The pause is an hour, and the test allows a second: a pause
      -- before the first ask would outlast it
      it "is seen at once when the store lists it at once, before any pause" $
        timeout 1000000 (runExceptT (broadcastWith 3600000000 3 (listing [storeTx (txHash tx) (Store.MemRef 0) [] [] 0]) tx))
          `shouldReturn` Just (Right True)
      -- The store never lists it. Three asks have two pauses between
      -- them: 2 x 20 ms = 40 ms.
      it "is asked for again only after a pause: three asks 20 ms apart take 40 ms at least" $ do
        started <- getCurrentTime
        runExceptT (broadcastWith 20000 3 (listing []) tx) `shouldReturn` Right False
        ended <- getCurrentTime
        (diffUTCTime ended started >= 0.04) `shouldBe` True
