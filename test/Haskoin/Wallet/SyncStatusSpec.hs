{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The line that tells what a sync is doing: its text, the stores that
-- tell the steps, and what reaches the terminal. The steps of a whole sync
-- are in the spec of the sync, and the line in a real terminal in the
-- output workflow.
module Haskoin.Wallet.SyncStatusSpec (spec) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.IORef (newIORef, readIORef)
import Data.Text (Text)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Store (Store (..))
import Haskoin.Wallet.SyncStatus
import Haskoin.Wallet.TestUtils
import System.IO (hClose)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempFile)

-- | What reaches the file of the handle, given what is done on the line
written :: (StatusLine -> IO ()) -> IO String
written use =
  withSystemTempFile "status-line" $ \path h -> do
    use (statusLineOn h)
    hClose h
    readFile path

-- | A list of 250 transaction ids
manyIds :: [TxHash]
manyIds = fakeTxHash <$> [1 .. 250]

spec :: Spec
spec = prepareContext $ \ctx -> do
  describe "The text of the line" $ do
    it "names the account, then what the sync is doing" $
      statusText "main" CheckingStore `shouldBe` "main: checking the store"

    it "tells how many used addresses a scan found" $
      statusText "main" (LookingForAddresses 340) `shouldBe` "main: looking for used addresses, 340 found"

    -- 20 × 412 / 1180 = 6.98, rounded down to 6
    it "shows a bar and a count of addresses for the transactions listed address by address" $
      statusText "main" (Counted ListingTransactions 412 1180)
        `shouldBe` "main: listing transactions  [######--------------]  412/1180 addresses"

    -- 20 × 300 / 2100 = 2.85, rounded down to 2
    it "shows a bar and a plain count for the transactions fetched" $
      statusText "main" (Counted FetchingTransactions 300 2100)
        `shouldBe` "main: fetching transactions  [##------------------]  300/2100"

    -- 20 × 100 / 250 = 8
    it "shows the coins read by address" $
      statusText "main" (Counted ReadingCoins 100 250)
        `shouldBe` "main: reading coins  [########------------]  100/250 addresses"

    it "starts with an empty bar" $
      statusText "main" (Counted ReadingBalances 0 40)
        `shouldBe` "main: reading balances  [--------------------]  0/40 addresses"

    -- 20 × 2099 / 2100 = 19.99, rounded down to 19
    it "keeps the bar short of full until all is done" $
      statusText "main" (Counted FetchingTransactions 2099 2100)
        `shouldBe` "main: fetching transactions  [###################-]  2099/2100"

    it "fills the bar when all is done" $
      statusText "main" (Counted FetchingRawTransactions 300 300)
        `shouldBe` "main: fetching raw transactions  [####################]  300/300"

    it "tells where a search over the accounts of a seed is, and how many accounts it found paid" $
      statusText "recover" (LookingAtAccount (Deriv :| 84 :| 0 :| 3) 2)
        `shouldBe` "recover: looking at /84'/0'/3', 2 accounts paid so far"

    it "says account in the singular for one paid account" $
      statusText "recover" (LookingAtAccount (Deriv :| 44 :| 0 :| 1) 1)
        `shouldBe` "recover: looking at /44'/0'/1', 1 account paid so far"

    -- 20 characters for the name, 2 for the colon and its space, 20 for
    -- the label, 21 for the numbers and 6 around the bar leave a bar of
    -- 79 - 69 = 10. 10 × 12345 / 67890 = 1.81, rounded down to 1.
    it "cuts a long name and narrows the bar, so that the line stays within 79 columns" $
      statusText "savings-of-the-whole-family" (Counted ListingTransactions 12345 67890)
        `shouldBe` "savings-of-the-wh...: listing transactions  [#---------]  12345/67890 addresses"

    it "leaves a name of 20 characters whole" $
      statusText "savings-of-the-whole" CheckingStore `shouldBe` "savings-of-the-whole: checking the store"

  describe "The store of a sync" $ do
    let telling steps = tellingStore (notingSteps steps "main")

    it "tells that it checks the store when asked whether it is up" $ do
      steps <- newIORef []
      runExceptT (storeHealth (telling steps (toStore emptyStore))) `shouldReturn` Right ()
      readIORef steps `shouldReturn` [("main", CheckingStore)]

    -- 250 ids make two batches of 100 and one of 50
    it "asks about a long list a hundred at a time" $ do
      steps <- newIORef []
      asked <- newIORef []
      _ <- runExceptT $ storeTransactions (telling steps (notingTxRequests asked (toStore emptyStore))) manyIds
      (map length <$> readIORef asked) `shouldReturn` [100, 100, 50]

    it "counts a long list as its batches are answered, from none to all" $ do
      steps <- newIORef []
      _ <- runExceptT $ storeTransactions (telling steps (toStore emptyStore)) manyIds
      readIORef steps
        `shouldReturn` [ ("main", Counted FetchingTransactions 0 250),
                         ("main", Counted FetchingTransactions 100 250),
                         ("main", Counted FetchingTransactions 200 250),
                         ("main", Counted FetchingTransactions 250 250)
                       ]

    it "gives the answers of all the batches, in the order asked" $ do
      steps <- newIORef []
      let paid tid = creditTx ctx tid (Store.BlockRef 800000 1) (externalAddr 0) 1000
          store = toStore emptyStore {fakeTransactions = paid <$> manyIds}
      answer <- runExceptT $ storeTransactions (telling steps store) manyIds
      (map (.txid) <$> answer) `shouldBe` Right manyIds

    it "asks nothing and tells nothing about a list of nothing" $ do
      steps <- newIORef []
      asked <- newIORef []
      _ <- runExceptT $ storeTransactions (telling steps (notingTxRequests asked (toStore emptyStore))) []
      readIORef asked `shouldReturn` []
      readIORef steps `shouldReturn` []

    it "fails with the store, and stops counting there" $ do
      steps <- newIORef []
      let failing = (toStore emptyStore) {storeTransactions = \_ -> throwError "The store is down"}
      runExceptT (storeTransactions (telling steps failing) manyIds) `shouldReturn` Left "The store is down"
      readIORef steps `shouldReturn` [("main", Counted FetchingTransactions 0 250)]

    it "names each list by what it asks: balances, coins and raw transactions" $ do
      steps <- newIORef []
      let store = telling steps (toStore emptyStore)
      _ <- runExceptT $ storeBalances store [externalAddr 0]
      _ <- runExceptT $ storeUnspents store [externalAddr 0]
      _ <- runExceptT $ storeRawTransactions store [fakeTxHash 1]
      readIORef steps
        `shouldReturn` [ ("main", Counted ReadingBalances 0 1),
                         ("main", Counted ReadingBalances 1 1),
                         ("main", Counted ReadingCoins 0 1),
                         ("main", Counted ReadingCoins 1 1),
                         ("main", Counted FetchingRawTransactions 0 1),
                         ("main", Counted FetchingRawTransactions 1 1)
                       ]

  describe "The store of a scan" $ do
    it "tells before each window how many used addresses the windows before had" $ do
      steps <- newIORef []
      store <- scanningStore (notingSteps steps "main") $ toStore emptyStore {fakeBalances = [paidOnce (externalAddr 0) 1000, paidOnce (externalAddr 1) 1000]}
      _ <- runExceptT $ storeBalances store [externalAddr 0, externalAddr 1]
      _ <- runExceptT $ storeBalances store [externalAddr 2, externalAddr 3]
      _ <- runExceptT $ storeBalances store [externalAddr 4, externalAddr 5]
      readIORef steps
        `shouldReturn` [ ("main", LookingForAddresses 0),
                         ("main", LookingForAddresses 2),
                         ("main", LookingForAddresses 2)
                       ]

    it "does not count an address the store lists without transactions" $ do
      steps <- newIORef []
      store <- scanningStore (notingSteps steps "main") $ toStore emptyStore {fakeBalances = [Store.Balance (externalAddr 0) 0 0 0 0 0]}
      _ <- runExceptT $ storeBalances store [externalAddr 0]
      _ <- runExceptT $ storeBalances store [externalAddr 1]
      readIORef steps `shouldReturn` [("main", LookingForAddresses 0), ("main", LookingForAddresses 0)]

    it "gives the balances as the store answered them" $ do
      store <- scanningStore (\_ -> return ()) $ toStore emptyStore {fakeBalances = [paidOnce (externalAddr 0) 1000]}
      runExceptT (storeBalances store [externalAddr 0]) `shouldReturn` Right [paidOnce (externalAddr 0) 1000]

  describe "Questions asked in parallel" $ do
    it "counts each answer once, never going back, even three at a time" $ do
      steps <- newIORef []
      let questions = return <$> [1 .. 5 :: Int]
      _ <- runExceptT $ countedInParallel (notingSteps steps "main") ListingTransactions 3 questions
      readIORef steps
        `shouldReturn` [ ("main", Counted ListingTransactions 0 5),
                         ("main", Counted ListingTransactions 1 5),
                         ("main", Counted ListingTransactions 2 5),
                         ("main", Counted ListingTransactions 3 5),
                         ("main", Counted ListingTransactions 4 5),
                         ("main", Counted ListingTransactions 5 5)
                       ]

    it "gives the answers in the order asked" $
      runExceptT (countedInParallel (\_ -> return ()) ListingTransactions 3 (return <$> [1, 2, 3 :: Int]))
        `shouldReturn` Right [1, 2, 3]

    it "tells nothing when there is nothing to ask" $ do
      steps <- newIORef ([] :: [(Text, Step)])
      _ <- runExceptT $ countedInParallel (notingSteps steps "main") ListingTransactions 3 ([] :: [ExceptT String IO Int])
      readIORef steps `shouldReturn` []

  describe "The line on a terminal" $ do
    it "goes back to the start of the line, writes the step, and clears what a longer step left" $
      written (\line -> showStep line "main" CheckingStore)
        `shouldReturn` "\rmain: checking the store\ESC[0K"

    it "is erased by clearing it from the start" $
      written eraseStatus `shouldReturn` "\r\ESC[0K"

    -- The whole sync is one transaction of the database, which a failure
    -- would undo
    it "lets the sync go on when it cannot be written" $
      withSystemTempFile "status-line" $ \_ h -> do
        hClose h
        showStep (statusLineOn h) "main" CheckingStore `shouldReturn` ()
