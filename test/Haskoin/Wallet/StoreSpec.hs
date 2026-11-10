{-# LANGUAGE OverloadedStrings #-}

-- | The web client of hw, as it tells why it could not ask the store: an
-- error the store answered, a store out of reach, or an answer it could
-- not read. Then the health checks of the stores of several networks,
-- which tell a network the host does not serve from a store that is down.
-- The questions themselves are played out against scripted stores in the
-- specs of the sync and of the pending transactions.
module Haskoin.Wallet.StoreSpec (spec) where

import Control.Exception (throwIO, toException)
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word64)
import qualified Haskoin.Store.Data as Store
import Haskoin (Network, bch, bchRegTest, btc, btcRegTest, btcTest, prepareContext, txHash)
import Haskoin.Wallet.Store
import Haskoin.Wallet.TestUtils (emptyStore, fakeTxHash, notServed, otherAddr, replaced, spendingTx, storeTx, toStore, unhealthy)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), defaultRequest)
import Test.Hspec

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

    -- The web client decodes an answer only once it is used, and its
    -- decoder fails with an error call, as this answer does in its second
    -- number
    it "fails with an answer it could not read, wherever in the answer" $
      called (return [5, error "not enough bytes"])
        `shouldReturn` Left "The store at http://store.test/ gave an answer hw could not read: not enough bytes"

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
