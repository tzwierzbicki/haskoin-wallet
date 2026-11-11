{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The line that tells what a sync or a recovery is doing: its text, the
-- stores that tell the steps, and what reaches the terminal. A sync is
-- named by its portfolio and its network, as in "portfolio0 btc". The
-- steps of a whole sync are in the spec of the sync, and the line in a
-- real terminal in the output workflow.
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
    it "names the portfolio and its network, then what the sync is doing" $
      statusText "portfolio0 btc" CheckingStore `shouldBe` "portfolio0 btc: checking the store"

    -- Before the mnemonic is typed, the store of every network is asked
    -- whether it is up
    it "tells which network recover checks the health of the store of" $
      statusText "recover" (CheckingHealth btcTest4) `shouldBe` "recover: checking the health of the store of btctest4"

    it "tells how many used addresses a scan found" $
      statusText "portfolio0 btc" (LookingForAddresses 340) `shouldBe` "portfolio0 btc: looking for used addresses, 340 found"

    -- The line without its bar has 60 columns: 16 for the name with its
    -- colon and space, 20 for the label, 6 around the bar and 18 for the
    -- numbers. That leaves a bar of 79 - 60 = 19.
    -- 19 × 412 / 1180 = 6.63, rounded down to 6
    it "shows a bar and a count of addresses for the transactions listed address by address" $
      statusText "portfolio0 btc" (Counted ListingTransactions 412 1180)
        `shouldBe` "portfolio0 btc: listing transactions  [######-------------]  412/1180 addresses"

    -- 20 × 300 / 2100 = 2.85, rounded down to 2
    it "shows a bar and a plain count for the transactions fetched" $
      statusText "portfolio0 btc" (Counted FetchingTransactions 300 2100)
        `shouldBe` "portfolio0 btc: fetching transactions  [##------------------]  300/2100"

    -- 20 × 100 / 250 = 8
    it "shows the coins read by address" $
      statusText "portfolio0 btc" (Counted ReadingCoins 100 250)
        `shouldBe` "portfolio0 btc: reading coins  [########------------]  100/250 addresses"

    it "starts with an empty bar" $
      statusText "portfolio0 btc" (Counted ReadingBalances 0 40)
        `shouldBe` "portfolio0 btc: reading balances  [--------------------]  0/40 addresses"

    -- 20 × 2099 / 2100 = 19.99, rounded down to 19
    it "keeps the bar short of full until all is done" $
      statusText "portfolio0 btc" (Counted FetchingTransactions 2099 2100)
        `shouldBe` "portfolio0 btc: fetching transactions  [###################-]  2099/2100"

    it "fills the bar when all is done" $
      statusText "portfolio0 btc" (Counted FetchingRawTransactions 300 300)
        `shouldBe` "portfolio0 btc: fetching raw transactions  [####################]  300/300"

    it "tells which portfolio of a seed recover is looking at, and how many portfolios it found paid" $
      statusText "recover" (LookingAtPortfolio 3 2)
        `shouldBe` "recover: looking at portfolio3, 2 portfolios paid so far"

    it "says portfolio in the singular for one paid portfolio" $
      statusText "recover" (LookingAtPortfolio 1 1)
        `shouldBe` "recover: looking at portfolio1, 1 portfolio paid so far"

    it "says portfolios in the plural when none was found paid yet" $
      statusText "recover" (LookingAtPortfolio 0 0)
        `shouldBe` "recover: looking at portfolio0, 0 portfolios paid so far"

    -- The accounts of the kinds hw does not hold, which recover looks for
    -- on each network
    it "tells where a search over the accounts of a seed is, and how many accounts it found paid" $
      statusText "recover on btc" (LookingAtAccount (Deriv :| 49 :| 0 :| 3) 2)
        `shouldBe` "recover on btc: looking at /49'/0'/3', 2 accounts paid so far"

    it "says account in the singular for one paid account" $
      statusText "recover on btc" (LookingAtAccount (Deriv :| 86 :| 0 :| 1) 1)
        `shouldBe` "recover on btc: looking at /86'/0'/1', 1 account paid so far"

    -- 20 characters for the name, 2 for the colon and its space, 20 for
    -- the label, 21 for the numbers and 6 around the bar leave a bar of
    -- 79 - 69 = 10. 10 × 12345 / 67890 = 1.81, rounded down to 1.
    it "cuts a long name and narrows the bar, so that the line stays within 79 columns" $
      statusText "portfolio123 bchtest4" (Counted ListingTransactions 12345 67890)
        `shouldBe` "portfolio123 bcht...: listing transactions  [#---------]  12345/67890 addresses"

    it "leaves a name of 20 characters whole" $
      statusText "portfolio10 bchtest4" CheckingStore `shouldBe` "portfolio10 bchtest4: checking the store"

  describe "The steps of a search" $ do
    -- It names no network: a portfolio is looked for on every network
    it "are told as those of recover, for the search over the portfolios of a seed" $ do
      steps <- newIORef []
      tellPortfolioSearch (notingSteps steps) 3 1
      readIORef steps `shouldReturn` [("recover", LookingAtPortfolio 3 1)]

    -- A path alone does not tell the networks apart: m/44'/0' is searched
    -- on Bitcoin Cash too
    it "are told as those of recover on a network, for the search over the accounts hw does not hold" $ do
      steps <- newIORef []
      tellSearch (notingSteps steps) bch (Deriv :| 44 :| 0 :| 2) 1
      readIORef steps `shouldReturn` [("recover on bch", LookingAtAccount (Deriv :| 44 :| 0 :| 2) 1)]

  describe "The store of a first health check" $ do
    it "tells that it checks the health of the store of its network when asked whether it is up" $ do
      steps <- newIORef []
      _ <- runExceptT (storeHealth (checkingHealth (notingSteps steps "recover") (const (toStore emptyStore)) bch))
      readIORef steps `shouldReturn` [("recover", CheckingHealth bch)]

  describe "The store of a sync" $ do
    let telling steps = tellingStore (notingSteps steps "portfolio0 btc")

    it "tells that it checks the store when asked whether it is up" $ do
      steps <- newIORef []
      runExceptT (storeHealth (telling steps (toStore emptyStore))) `shouldReturn` Right ()
      readIORef steps `shouldReturn` [("portfolio0 btc", CheckingStore)]

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
        `shouldReturn` [ ("portfolio0 btc", Counted FetchingTransactions 0 250),
                         ("portfolio0 btc", Counted FetchingTransactions 100 250),
                         ("portfolio0 btc", Counted FetchingTransactions 200 250),
                         ("portfolio0 btc", Counted FetchingTransactions 250 250)
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
      readIORef steps `shouldReturn` [("portfolio0 btc", Counted FetchingTransactions 0 250)]

    it "names each list by what it asks: balances, coins and raw transactions" $ do
      steps <- newIORef []
      let store = telling steps (toStore emptyStore)
      _ <- runExceptT $ storeBalances store [externalAddr 0]
      _ <- runExceptT $ storeUnspents store [externalAddr 0]
      _ <- runExceptT $ storeRawTransactions store [fakeTxHash 1]
      readIORef steps
        `shouldReturn` [ ("portfolio0 btc", Counted ReadingBalances 0 1),
                         ("portfolio0 btc", Counted ReadingBalances 1 1),
                         ("portfolio0 btc", Counted ReadingCoins 0 1),
                         ("portfolio0 btc", Counted ReadingCoins 1 1),
                         ("portfolio0 btc", Counted FetchingRawTransactions 0 1),
                         ("portfolio0 btc", Counted FetchingRawTransactions 1 1)
                       ]

  describe "The store of a scan" $ do
    it "tells before each window how many used addresses the windows before had" $ do
      steps <- newIORef []
      store <- scanningStore (notingSteps steps "portfolio0 btc") $ toStore emptyStore {fakeBalances = [paidOnce (externalAddr 0) 1000, paidOnce (externalAddr 1) 1000]}
      _ <- runExceptT $ storeBalances store [externalAddr 0, externalAddr 1]
      _ <- runExceptT $ storeBalances store [externalAddr 2, externalAddr 3]
      _ <- runExceptT $ storeBalances store [externalAddr 4, externalAddr 5]
      readIORef steps
        `shouldReturn` [ ("portfolio0 btc", LookingForAddresses 0),
                         ("portfolio0 btc", LookingForAddresses 2),
                         ("portfolio0 btc", LookingForAddresses 2)
                       ]

    it "does not count an address the store lists without transactions" $ do
      steps <- newIORef []
      store <- scanningStore (notingSteps steps "portfolio0 btc") $ toStore emptyStore {fakeBalances = [Store.Balance (externalAddr 0) 0 0 0 0 0]}
      _ <- runExceptT $ storeBalances store [externalAddr 0]
      _ <- runExceptT $ storeBalances store [externalAddr 1]
      readIORef steps `shouldReturn` [("portfolio0 btc", LookingForAddresses 0), ("portfolio0 btc", LookingForAddresses 0)]

    it "gives the balances as the store answered them" $ do
      store <- scanningStore (\_ -> return ()) $ toStore emptyStore {fakeBalances = [paidOnce (externalAddr 0) 1000]}
      runExceptT (storeBalances store [externalAddr 0]) `shouldReturn` Right [paidOnce (externalAddr 0) 1000]

  describe "Questions asked in parallel" $ do
    it "counts each answer once, never going back, even three at a time" $ do
      steps <- newIORef []
      let questions = return <$> [1 .. 5 :: Int]
      _ <- runExceptT $ countedInParallel (notingSteps steps "portfolio0 btc") ListingTransactions 3 questions
      readIORef steps
        `shouldReturn` [ ("portfolio0 btc", Counted ListingTransactions 0 5),
                         ("portfolio0 btc", Counted ListingTransactions 1 5),
                         ("portfolio0 btc", Counted ListingTransactions 2 5),
                         ("portfolio0 btc", Counted ListingTransactions 3 5),
                         ("portfolio0 btc", Counted ListingTransactions 4 5),
                         ("portfolio0 btc", Counted ListingTransactions 5 5)
                       ]

    it "gives the answers in the order asked" $
      runExceptT (countedInParallel (\_ -> return ()) ListingTransactions 3 (return <$> [1, 2, 3 :: Int]))
        `shouldReturn` Right [1, 2, 3]

    it "tells nothing when there is nothing to ask" $ do
      steps <- newIORef ([] :: [(Text, Step)])
      _ <- runExceptT $ countedInParallel (notingSteps steps "portfolio0 btc") ListingTransactions 3 ([] :: [ExceptT String IO Int])
      readIORef steps `shouldReturn` []

  describe "The line on a terminal" $ do
    it "goes back to the start of the line, writes the step, and clears what a longer step left" $
      written (\line -> showStep line "portfolio0 btc" CheckingStore)
        `shouldReturn` "\rportfolio0 btc: checking the store\ESC[0K"

    it "is erased by clearing it from the start" $
      written eraseStatus `shouldReturn` "\r\ESC[0K"

    -- The whole sync is one transaction of the database, which a failure
    -- would undo
    it "lets the sync go on when it cannot be written" $
      withSystemTempFile "status-line" $ \_ h -> do
        hClose h
        showStep (statusLineOn h) "portfolio0 btc" CheckingStore `shouldReturn` ()
