{-# LANGUAGE OverloadedStrings #-}

-- | The balance statement of report: what each address held at a date,
-- which transactions count for a day in the past, when a statement is
-- refused, what a statement of the addresses of some tags keeps, and
-- what the report says. The story of the transactions is
-- that of "Haskoin.Wallet.Report.TestUtils".
module Haskoin.Wallet.Report.BalanceStatementSpec (spec) where

import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone (..), UTCTime (..), ZonedTime (..), fromGregorian)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.AddressSelection (AddressSelection (..))
import Haskoin.Wallet.Database.Tag (TagFilter (..))
import Haskoin.Wallet.Report.BalanceStatement
import Haskoin.Wallet.Report.Pdf (Mark (..))
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), WalletNaming (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (legacyPath)
import Haskoin.Wallet.Report.Price (Price (..))
import Haskoin.Wallet.Report.Statement
import Haskoin.Wallet.Report.TestUtils
import Haskoin.Wallet.Tags (Tag)
-- The rows of a table are those of a report here, not those of a
-- table of the database
import Haskoin.Wallet.TestUtils hiding (tableRows)
import Test.Hspec

-- | The statement of the portfolios at a date on a clock of Central
-- Europe in winter, on btc alone, which was never synced
statementAt :: StatementDate -> [PortfolioHistory] -> Either String Statement
statementAt date = balanceStatement EveryWallet (const cet) date [(btc, Nothing)]

-- | The addresses with a balance of each portfolio of a statement, on
-- its first network
heldBy :: Either String Statement -> Either String [[AddressHolding]]
heldBy = fmap (map holdingsAddresses . concatMap assetPortfolios . take 1 . statementAssets)

-- | The first receiving address with the 50000 it was paid, and the
-- first change address with the 9000 of change
paidAddress, changeAddress :: AddressHolding
paidAddress = AddressHolding "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" (legacyPath (Deriv :/ 0 :/ 0)) "" 50000
changeAddress = AddressHolding "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn" (legacyPath (Deriv :/ 1 :/ 0)) "" 9000

-- | The statement for the end of 2023 of portfolio0 with the
-- transactions given, on btc and bch, both synced in January 2024
statementOfYear :: [TxInfo] -> Either String Statement
statementOfYear transactions =
  balanceStatement
    EveryWallet
    (const cet)
    (AtEndOf (fromGregorian 2023 12 31) cet)
    [(btc, Just syncedInJanuary), (bch, Just syncedInJanuary)]
    [PortfolioHistory mainPortfolioRef "" "Savings" [(btc, [ListedTx info Nothing | info <- transactions]), (bch, [])]]

-- | The report of that statement, made at 'preparedAt'
reportOfYear :: [TxInfo] -> Either String Report
reportOfYear transactions = statementReport preparedAt <$> statementOfYear transactions

-- | That statement of the payment received, valued in Swiss francs at
-- 87500 a bitcoin, a price that came from "the tax office"
valuedOfYear :: Either String Statement
valuedOfYear = valued (Valuation "CHF" [(btc, Price 87500 0)] "the tax office") =<< statementOfYear [received]

-- | The selection of the addresses tagged kyc, when the addresses given
-- of portfolio0 of wallet0 on btc have that tag, and no other address
-- of the wallets has a tag
taggedKyc :: [Text] -> AddressSelection
taggedKyc addresses = AddressSelection "tagged kyc" (TagFilter [kycTag] False) (kycAt addresses)

-- | The selection of the addresses without a tag, when the addresses
-- given have the tag kyc
withoutTag :: [Text] -> AddressSelection
withoutTag addresses = AddressSelection "without a tag" (TagFilter [] True) (kycAt addresses)

-- | The tag kyc on the addresses given of portfolio0 of wallet0 on btc
kycAt :: [Text] -> Map.Map (PortfolioRef, String, Text) [Tag]
kycAt addresses = Map.fromList [((mainPortfolioRef, "btc", address), [kycTag]) | address <- addresses]

spec :: Spec
spec = do
  describe "The balance of an address in a statement" $ do
    it "is what the address was paid" $
      heldBy (statementAt AtPreparation [savings [received]]) `shouldBe` Right [[paidAddress]]

    -- The 50000 of the first receiving address are spent, and the first
    -- change address holds the 9000 of change
    it "is less what the address spent, and an address that holds nothing is left out" $
      heldBy (statementAt AtPreparation [savings [spent, received]]) `shouldBe` Right [[changeAddress]]

    it "comes with the label of the address" $
      heldBy (statementAt AtPreparation [savings [txOnChain paymentToRent 1 (Store.BlockRef 800000 1) 3 False ""]])
        `shouldBe` Right [[paidAddress {holdingLabel = "Rent"}]]

    it "leaves out an unconfirmed transaction" $
      heldBy (statementAt AtPreparation [savings [spentUnconfirmed, received]]) `shouldBe` Right [[paidAddress]]

    it "leaves out a dead transaction, even one that was in a block" $
      heldBy (statementAt AtPreparation [savings [txOnChain spendSummary 2 (Store.BlockRef 800001 1) 0 True "", received]])
        `shouldBe` Right [[paidAddress]]

    -- One transaction pays four addresses. They come as addresses lists
    -- them: bip44 before bip84, receiving before change, then by index.
    it "is listed in the order of addresses: bip44 first, receiving before change, by index" $ do
      let paysFour =
            paymentSummary
              { summaryMyOutputs =
                  Map.fromList
                    [ (segwitExternalAddr 0, MyOutputs 4000 (segwitPath (Deriv :/ 0 :/ 0)) ""),
                      (internalAddr 0, MyOutputs 3000 (legacyPath (Deriv :/ 1 :/ 0)) ""),
                      (externalAddr 1, MyOutputs 2000 (legacyPath (Deriv :/ 0 :/ 1)) ""),
                      (externalAddr 0, MyOutputs 1000 (legacyPath (Deriv :/ 0 :/ 0)) "")
                    ]
              }
      fmap (map (map (\holding -> (holdingAddress holding, holdingBalance holding)))) (heldBy (statementAt AtPreparation [savings [txOnChain paysFour 1 (Store.BlockRef 800000 1) 3 False ""]]))
        `shouldBe` Right
          [ [ ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", 1000),
              ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 2000),
              ("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", 3000),
              ("bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", 4000)
            ]
          ]

    it "is refused below zero: a history that spends from an address more than it received is not sound" $
      heldBy (statementAt AtPreparation [savings [spent]])
        `shouldBe` Left "The transactions of portfolio0 of wallet0 on btc spend more from 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj than it received: hw sync --full reads them again"

  describe "A statement for a day in the past" $ do
    -- The block is dated 23:13 on the 14th in Central Europe
    it "counts a transaction whose block is dated on that day" $
      heldBy (statementAt (AtEndOf day14 cet) [savings [received]]) `shouldBe` Right [[paidAddress]]

    it "leaves out a transaction whose block is dated after that day" $
      heldBy (statementAt (AtEndOf day13 cet) [savings [received]]) `shouldBe` Right [[]]

    it "counts the transactions up to that day and leaves out the later ones" $ do
      -- The spend is in a block dated a week later, 21 Nov at 10:00 UTC
      let spentLater = datedAt (UTCTime (fromGregorian 2023 11 21) (10 * 3600)) spent
      heldBy (statementAt (AtEndOf day14 cet) [savings [spentLater, received]]) `shouldBe` Right [[paidAddress]]

    -- 22:13:20 UTC on the 14th is 00:13:20 on the 15th two hours ahead
    -- of UTC
    it "reads the day of a block on the clock of the computer" $ do
      let twoHoursAhead = TimeZone 120 False "EET"
      heldBy (balanceStatement EveryWallet (const twoHoursAhead) (AtEndOf day14 cet) [(btc, Nothing)] [savings [received]]) `shouldBe` Right [[]]

    -- Block 800000 is dated 00:05 on the 15th in Central Europe, 23:05
    -- UTC on the 14th. Block 800001, which spends its coin, is dated
    -- 23:55 on the 14th, 22:55 UTC. Counting the second block without
    -- the first would spend a coin that was never received.
    it "counts a block dated after the day when a later block is dated on the day" $ do
      let receivedLate = datedAt (UTCTime day14 (23 * 3600 + 5 * 60)) received
          spentEarly = datedAt (UTCTime day14 (22 * 3600 + 55 * 60)) spent
      heldBy (statementAt (AtEndOf day14 cet) [savings [spentEarly, receivedLate]]) `shouldBe` Right [[changeAddress]]

    it "names the last block it counts" $ do
      let spentLater = datedAt (UTCTime (fromGregorian 2023 11 21) (10 * 3600)) spent
      fmap (map assetLastBlock . statementAssets) (statementAt (AtEndOf day14 cet) [savings [spentLater, received]]) `shouldBe` Right [Just (BlockNamed 800000 Nothing (Just seenLocalInCet))]

    it "counts no block when every transaction came after the day" $
      fmap (map assetLastBlock . statementAssets) (statementAt (AtEndOf day13 cet) [savings [spent, received]]) `shouldBe` Right [Nothing]

    it "counts a payment between two portfolios for both, or for neither" $ do
      -- portfolio1 receives the 40000 that portfolio0 pays, in the same
      -- transaction of block 800001, dated a week after the 14th
      let aWeekLater = UTCTime (fromGregorian 2023 11 21) (10 * 3600)
          receivedBy1 = paymentSummary {summaryAmount = 40000, summaryMyOutputs = Map.fromList [(otherAddr 0, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 0)) "")]}
          holidays = PortfolioHistory (PortfolioRef 0 1) "" "Holidays" [(btc, [ListedTx (datedAt aWeekLater (txOnChain receivedBy1 2 (Store.BlockRef 800001 1) 2 False "")) Nothing])]
          both = [savings [datedAt aWeekLater spent, received], holidays]
          paidTo1 = AddressHolding "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" (legacyPath (Deriv :/ 0 :/ 0)) "" 40000
      (heldBy (statementAt (AtEndOf day14 cet) both), heldBy (statementAt (AtEndOf (fromGregorian 2023 11 21) cet) both))
        `shouldBe` (Right [[paidAddress], []], Right [[changeAddress], [paidTo1]])

  describe "A statement of now" $ do
    it "counts every confirmed transaction, whatever its date" $ do
      let inTheFuture = datedAt (UTCTime (fromGregorian 2099 1 1) 0) received
      heldBy (statementAt AtPreparation [savings [inTheFuture]]) `shouldBe` Right [[paidAddress]]

    -- 40000 paid + 1000 of fee = 41000 would leave the portfolio
    it "says how many unconfirmed transactions it leaves out, and what they add up to" $
      fmap (map assetUnconfirmed . statementAssets) (statementAt AtPreparation [savings [spentUnconfirmed, received]]) `shouldBe` Right [(1, -41000)]

    -- portfolio0 loses 41000 and portfolio1 gains 40000: the fee of
    -- 1000 is what the wallet loses
    it "counts an unconfirmed payment between two portfolios once, for its fee" $ do
      let receivedBy1 = paymentSummary {summaryAmount = 40000, summaryMyOutputs = Map.fromList [(otherAddr 0, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 0)) "")]}
          holidays = PortfolioHistory (PortfolioRef 0 1) "" "Holidays" [(btc, [ListedTx (txOnChain receivedBy1 2 (Store.MemRef 1700000000) 0 False "") Nothing])]
      fmap (map assetUnconfirmed . statementAssets) (statementAt AtPreparation [savings [spentUnconfirmed, received], holidays]) `shouldBe` Right [(1, -1000)]

  describe "A statement and the last sync" $ do
    -- The last sync reached a block dated 23:13 on the 14th: what
    -- happened later that day is unknown
    it "is refused for a day when the last sync reached a block dated on that day" $
      balanceStatement EveryWallet (const cet) (AtEndOf day14 cet) [(btc, Just (SyncedTo 800002 (fakeBlockHash 2) (Just seenTime)))] [savings [received]]
        `shouldBe` Left "The last sync of btc reached a block dated 14 November 2023, and the statement is for the end of 14 November 2023: it could miss transactions. Run hw sync first"

    it "is made for a day when the last sync reached a block dated after it" $
      heldBy (balanceStatement EveryWallet (const cet) (AtEndOf day13 cet) [(btc, Just (SyncedTo 800002 (fakeBlockHash 2) (Just seenTime)))] [savings [received]]) `shouldBe` Right [[]]

    it "is made for now however old the last sync is" $
      balanceStatement EveryWallet (const cet) AtPreparation [(btc, Just (SyncedTo 800002 (fakeBlockHash 2) (Just seenTime)))] [savings [received]] `shouldSatisfy` isRight

    it "tells the block the last sync reached, and its time on the clock of the computer" $
      fmap (map assetSynced . statementAssets) (balanceStatement EveryWallet (const cet) AtPreparation [(btc, Just (SyncedTo 800002 (fakeBlockHash 2) (Just seenTime)))] [savings [received]])
        `shouldBe` Right [Just (BlockNamed 800002 (Just (fakeBlockHash 2)) (Just seenLocalInCet))]

  describe "The portfolios of a statement" $ do
    it "are all there on every network, with or without a balance" $ do
      let empty = PortfolioHistory (PortfolioRef 0 1) "" "Holidays" [(btc, [])]
      fmap (map (map holdingsRef . assetPortfolios) . statementAssets) (statementAt AtPreparation [savings [received], empty])
        `shouldBe` Right [[PortfolioRef 0 0, PortfolioRef 0 1]]

    it "are named without their wallet when the statement covers one wallet" $
      fmap statementNaming (statementAt AtPreparation [savings [received]]) `shouldBe` Right WithoutWallets

    it "are named with their wallet when the statement covers several" $ do
      let ofWallet1 = PortfolioHistory (PortfolioRef 1 0) "Company" "" [(btc, [])]
      fmap (\statement -> (statementNaming statement, statementWallets statement)) (statementAt AtPreparation [savings [received], ofWallet1])
        `shouldBe` Right (WithWallets, [(0, "", 1), (1, "Company", 1)])

  describe "A statement of one wallet or of one portfolio" $ do
    -- wallet0 is described as "Personal" and wallet1 as "Business". Each
    -- has a portfolio0 that received the payment of 50000.
    let personal = PortfolioHistory mainPortfolioRef "Personal" "Savings" [(btc, [ListedTx received Nothing])]
        business = PortfolioHistory (PortfolioRef 1 0) "Business" "Till" [(btc, [ListedTx received Nothing])]
        -- portfolio1 of wallet0, which received the payment too
        holidays = PortfolioHistory (PortfolioRef 0 1) "Personal" "Holidays" [(btc, [ListedTx received Nothing])]
        ofScope scope = balanceStatement scope (const cet) AtPreparation [(btc, Nothing)]
        portfoliosOf = fmap (map holdingsRef . concatMap assetPortfolios . statementAssets)

    it "holds the portfolios of the one wallet it is of" $
      portfoliosOf (ofScope (OneWallet 1) [personal, holidays, business]) `shouldBe` Right [PortfolioRef 1 0]

    it "holds the one portfolio it is of" $
      portfoliosOf (ofScope (OnePortfolio (PortfolioRef 0 1)) [personal, holidays, business]) `shouldBe` Right [PortfolioRef 0 1]

    it "says in its facts which wallet it is of, and that it is of that one only" $
      fmap (factsNamed "Wallets" . statementReport preparedAt) (ofScope (OneWallet 1) [personal, business])
        `shouldBe` Right [["wallet1 only, 1 portfolio: Business"]]

    it "says in its facts which portfolio it is of, named with its wallet in a database of several" $
      fmap (factsNamed "Wallets" . statementReport preparedAt) (ofScope (OnePortfolio (PortfolioRef 0 1)) [personal, holidays, business])
        `shouldBe` Right [["portfolio1 of wallet0 only"]]

    it "names its one portfolio without its wallet in a database of one wallet" $
      fmap (factsNamed "Wallets" . statementReport preparedAt) (ofScope (OnePortfolio (PortfolioRef 0 1)) [personal, holidays])
        `shouldBe` Right [["portfolio1 only"]]

    it "says in a note what it covers, and that the holder has other wallets and portfolios" $
      fmap (take 1 . drop 2 . notesOf . statementReport preparedAt) (ofScope (OneWallet 1) [personal, business])
        `shouldBe` Right ["The statement covers only wallet1. The other wallets and portfolios of the holder are not part of it, and neither are assets the holder keeps elsewhere, such as at an exchange."]

    -- The payment of wallet0 is in block 800000, dated on the 20th,
    -- after the day. That of wallet1 is in block 800001, dated on the
    -- 14th, so every block up to 800001 counts, and wallet0 holds its
    -- 50000 on the 15th. A statement of wallet0 alone must say the same.
    it "gives a portfolio the balance it has in the statement of everything, whatever blocks the portfolios left out count" $ do
      let on20th = UTCTime (fromGregorian 2023 11 20) (12 * 3600)
          early = PortfolioHistory mainPortfolioRef "Personal" "Savings" [(btc, [ListedTx (datedAt on20th received) Nothing])]
          later = PortfolioHistory (PortfolioRef 1 0) "Business" "Till" [(btc, [ListedTx (txOnChain paymentSummary 3 (Store.BlockRef 800001 1) 2 False "") Nothing])]
      fmap (map (map holdingBalance . holdingsAddresses) . concatMap assetPortfolios . statementAssets) (balanceStatement (OneWallet 0) (const cet) (AtEndOf (fromGregorian 2023 11 15) cet) [(btc, Nothing)] [early, later])
        `shouldBe` Right [[50000]]

    it "leaves out of its note the unconfirmed transactions of the portfolios it does not cover" $ do
      let unconfirmedTill = PortfolioHistory (PortfolioRef 1 0) "Business" "Till" [(btc, [ListedTx spentUnconfirmed Nothing, ListedTx received Nothing])]
      fmap (map assetUnconfirmed . statementAssets) (ofScope (OneWallet 0) [personal, unconfirmedTill]) `shouldBe` Right [(0, 0)]

    it "is the statement of everything when its wallet is the only one, to the reference" $
      fmap (\statement -> (statementScope statement, factsNamed "Wallets" (statementReport preparedAt statement))) (ofScope (OneWallet 0) [personal, holidays])
        `shouldBe` Right (EveryWallet, [["1 wallet, 2 portfolios", "Personal"]])

    -- The lines are "end of 2023-12-31", "holder ", "scope wallet 1" and
    -- "btc 1 0 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj 50000", hashed with
    -- sha256sum
    it "has a reference that states its scope" $
      fmap (statementReference preparedAt) (balanceStatement (OneWallet 1) (const cet) (AtEndOf (fromGregorian 2023 12 31) cet) [(btc, Nothing)] [personal, business])
        `shouldBe` Right "8AA7-3FC3"

  -- portfolio0 holds the 50000 it received at
  -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj. In the tests with two
  -- portfolios it has spent them: it holds 9000 of change at
  -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, and portfolio1 holds the 40000
  -- paid to its address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU.
  describe "A balance statement of some addresses" $ do
    it "lists an address that has a tag asked for, with its whole balance" $
      heldBy (holdingsSelected (taggedKyc ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]) <$> statementAt AtPreparation [savings [received]])
        `shouldBe` Right [[paidAddress]]

    -- The tag is on the change address, which holds nothing yet
    it "leaves out an address that does not have the tag" $
      heldBy (holdingsSelected (taggedKyc ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"]) <$> statementAt AtPreparation [savings [received]])
        `shouldBe` Right [[]]

    it "lists an address without a tag when those are asked for" $
      heldBy (holdingsSelected (withoutTag []) <$> statementAt AtPreparation [savings [received]])
        `shouldBe` Right [[paidAddress]]

    it "leaves out an address that has a tag when those without one are asked for" $
      heldBy (holdingsSelected (withoutTag ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]) <$> statementAt AtPreparation [savings [received]])
        `shouldBe` Right [[]]

    -- The tag is on the change address of portfolio0, which holds 9000.
    -- The 40000 of portfolio1 are at an address without the tag.
    it "sums up the addresses it lists, and no other" $ do
      let report = statementReport preparedAt . holdingsSelected (taggedKyc ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"]) <$> statementAt AtPreparation [savings [spent, received], spending]
      fmap (take 1 . tablesOf) report `shouldBe` Right [[["Bitcoin", "BTC", "1", "0.00009000"]]]

    it "lists the portfolios that hold some at these addresses, under a total named Total alone" $ do
      let report = statementReport preparedAt . holdingsSelected (taggedKyc ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"]) <$> statementAt AtPreparation [savings [spent, received], spending]
      fmap (take 1 . drop 1 . tablesOf) report
        `shouldBe` Right [[["portfolio0", "Savings", "1", "0.00009000"], ["Total", "", "1", "0.00009000"]]]

    it "gives the balance of each address it lists, under a total named Total alone" $ do
      let report = statementReport preparedAt . holdingsSelected (taggedKyc ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"]) <$> statementAt AtPreparation [savings [spent, received], spending]
      fmap (drop 2 . tablesOf) report
        `shouldBe` Right [[["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", "bip44/1/0", "0.00009000"], ["Total", "", "0.00009000"]]]

    it "keeps the title of a balance statement, and says above it that it is selected by tag" $
      fmap ((\report -> (reportKind report, reportTitle report)) . statementReport preparedAt . holdingsSelected (taggedKyc [])) (statementOfYear [received])
        `shouldBe` Right ("Statement of cryptocurrency holdings, selected by tag", "Digital Asset Balance Statement")

    it "says in its facts which addresses it lists" $
      fmap (factsNamed "Addresses" . statementReport preparedAt . holdingsSelected (taggedKyc [])) (statementOfYear [received])
        `shouldBe` Right [["Only those tagged kyc"]]

    it "has no such fact when it lists every address" $
      fmap (factsNamed "Addresses") (reportOfYear [received]) `shouldBe` Right []

    it "says in a note that it lists some addresses only, by the tags they have when it is made" $
      fmap (any (Text.isInfixOf "by some of the addresses of the wallets only, on each blockchain: the addresses tagged kyc when the statement was prepared.") . notesOf . statementReport preparedAt . holdingsSelected (taggedKyc [])) (statementOfYear [received])
        `shouldBe` Right True

    -- The unconfirmed payment is of the whole portfolio: the statement
    -- cannot say what it would change at the addresses kept
    it "gives no count of the unconfirmed transactions left out" $ do
      let report = statementReport preparedAt . holdingsSelected (taggedKyc ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]) <$> statementAt AtPreparation [savings [spentUnconfirmed, received]]
      fmap (elem "Transactions that were not confirmed are not counted." . notesOf) report `shouldBe` Right True

    -- The portfolio holds 50000 on btc and 50000 on bch, and the tag is
    -- on its address of btc alone
    it "needs a price only for the assets the addresses kept hold" $ do
      let onBoth = PortfolioHistory mainPortfolioRef "" "Savings" [(btc, [ListedTx received Nothing]), (bch, [ListedTx received Nothing])]
          selected = holdingsSelected (taggedKyc ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]) <$> balanceStatement EveryWallet (const cet) AtPreparation [(btc, Nothing), (bch, Nothing)] [onBoth]
      fmap statementValuation (valued (Valuation "CHF" [(btc, Price 87500 0)] "") =<< selected)
        `shouldBe` Right (Just (Valuation "CHF" [(btc, Price 87500 0)] ""))

    -- The lines are "end of 2023-12-31", "holder ", "selection tagged
    -- kyc" and "btc 0 0 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj 50000",
    -- hashed with sha256sum
    it "has a reference that states its selection" $
      fmap (statementReference preparedAt . holdingsSelected (taggedKyc ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"])) (statementOfYear [received])
        `shouldBe` Right "35C1-7598"

  describe "The names of a statement" $ do
    it "call btc Bitcoin and bch Bitcoin Cash" $
      (assetName btc, assetName bch) `shouldBe` ("Bitcoin", "Bitcoin Cash")

    it "call a test network by the word hw has for it" $
      assetName btcTest4 `shouldBe` "Test network btctest4"

    it "give the unit of a network as its name in capitals" $
      (assetUnit btc, assetUnit bch, assetUnit btcTest4) `shouldBe` ("BTC", "BCH", "BTCTEST4")

    it "write a day with its month as a word, and no zero before the day" $
      (longDate (fromGregorian 2025 12 31), longDate (fromGregorian 2026 1 5)) `shouldBe` ("31 December 2025", "5 January 2026")

  describe "The report of a statement" $ do
    it "is titled Digital Asset Balance Statement, as of its day, and says above the title that it is about cryptocurrency" $
      fmap (\report -> (reportKind report, reportTitle report, reportSubtitle report, reportFooter report)) (reportOfYear [received])
        `shouldBe` Right ("Statement of cryptocurrency holdings", "Digital Asset Balance Statement", "As of 31 December 2023", "Digital Asset Balance Statement \183 As of 31 December 2023 \183 Ref. 5D69-3866")

    it "starts with the facts: its date, when and by whom it was made, its reference, the custody, the wallets, the assets and the blocks of the last sync" $
      fmap (take 1 . reportBlocks) (reportOfYear [received])
        `shouldBe` Right
          [ FactsBlock
              [ Fact "Statement date" ["31 December 2023, 24:00 (UTC+01:00)"],
                Fact "Prepared on" ["15 November 2026, 14:32 (UTC+01:00)"],
                Fact "Prepared by" ["The holder, from public blockchain data. Not audited."],
                Fact "Reference" ["5D69-3866"],
                Fact "Custody" ["Self-custody: the holder keeps the keys. No exchange or online wallet."],
                Fact "Wallets" ["1 wallet, 1 portfolio"],
                Fact "Assets" ["Bitcoin (BTC), Bitcoin Cash (BCH)"],
                Fact "Blockchain data" ["Bitcoin: synchronised to block 800002, dated 2 January 2024, 13:00", "Bitcoin Cash: synchronised to block 800002, dated 2 January 2024, 13:00"]
              ]
          ]

    -- The clock is two hours ahead of UTC at the midnight that ends the
    -- day, and one hour ahead when the statement is made
    it "states the end of its day with the zone the clock has at that midnight" $
      fmap (factsNamed "Statement date" . statementReport preparedAt) (balanceStatement EveryWallet (const cet) (AtEndOf (fromGregorian 2023 6 30) (TimeZone 120 True "CEST")) [(btc, Nothing)] [savings []])
        `shouldBe` Right [["30 June 2023, 24:00 (UTC+02:00)"]]

    it "names the holder first, when it has one" $
      fmap (take 1 . concat . (\report -> [[(factName fact, factLines fact) | fact <- facts] | FactsBlock facts <- reportBlocks report]) . statementReport preparedAt . (\statement -> statement {statementHolder = "Jane Doe"})) (statementOfYear [received])
        `shouldBe` Right [("Holder", ["Jane Doe"])]

    it "has its sections in order: the summary, the portfolios, the addresses, the blocks, the notes" $
      fmap (\report -> [title | SectionBlock title <- reportBlocks report]) (reportOfYear [received])
        `shouldBe` Right ["Summary", "Holdings by portfolio", "Holdings by address", "Blockchain data", "Notes"]

    -- 50000 satoshis are 0.00050000 coins
    it "sums up each asset in coins with eight decimals, an asset without a balance too" $
      fmap (take 1 . tablesOf) (reportOfYear [received])
        `shouldBe` Right [[["Bitcoin", "BTC", "1", "0.00050000"], ["Bitcoin Cash", "BCH", "0", "0.00000000"]]]

    it "gives the balance of each portfolio for an asset the wallets hold, and their total" $
      fmap (take 1 . drop 1 . tablesOf) (reportOfYear [received])
        `shouldBe` Right [[["portfolio0", "Savings", "1", "0.00050000"], ["Total Bitcoin", "", "1", "0.00050000"]]]

    it "gives the balance of each address of a portfolio, with its path, and their total" $
      fmap (drop 2 . tablesOf) (reportOfYear [received])
        `shouldBe` Right [[["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "bip44/0/0", "0.00050000"], ["Total portfolio0", "", "0.00050000"]]]

    it "heads the tables and the blocks of an asset with its name and its unit, and the table of a portfolio with its description" $
      fmap (\report -> [(title, remark) | GroupBlock title remark <- reportBlocks report]) (reportOfYear [received])
        `shouldBe` Right [("Bitcoin (BTC)", ""), ("portfolio0 \183 Bitcoin (BTC)", "Savings"), ("Bitcoin (BTC)", ""), ("Bitcoin Cash (BCH)", "")]

    it "writes the label of an address as the remark of its cell" $
      fmap (\report -> [cellRemark cell | TableBlock table <- reportBlocks report, row <- tableRows table, cell <- rowCells row, cellText cell == "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]) (reportOfYear [txOnChain paymentToRent 1 (Store.BlockRef 800000 1) 3 False ""])
        `shouldBe` Right ["Rent"]

    it "has no table of portfolios or of addresses when the wallets hold nothing" $
      fmap (\report -> [title | SectionBlock title <- reportBlocks report]) (reportOfYear []) `shouldBe` Right ["Summary", "Blockchain data", "Notes"]

    -- The sync reached the block 800002, whose hash the database has.
    -- The payment is in the block 800000, dated 23:13 on 14 Nov 2023 in
    -- Central Europe, whose hash nobody gave. Nothing counts on bch.
    it "names the block of the last sync and the last block counted on each network, with their dates and hashes" $
      fmap (drop 1 . (\report -> [[(factName fact, factLines fact) | fact <- facts] | FactsBlock facts <- reportBlocks report])) (reportOfYear [received])
        `shouldBe` Right
          [ [ ("Last block of the sync", ["Block 800002, dated 2 January 2024, 13:00", "Hash " <> hashOfBlock2]),
              ("Last block counted", ["Block 800000, dated 14 November 2023, 23:13", "Hash not available: the server did not give it"])
            ],
            [ ("Last block of the sync", ["Block 800002, dated 2 January 2024, 13:00", "Hash " <> hashOfBlock2]),
              ("Last block counted", ["None: the statement counts no transaction"])
            ]
          ]

    it "says first who prepared it, that the wallets are self-custody wallets, and what it covers" $
      fmap (take 3 . notesOf) (reportOfYear [received])
        `shouldBe` Right
          [ "This statement was prepared by the holder of the wallets, from the data of public blockchains. No third party has audited or confirmed it.",
            "The wallets are self-custody wallets: the holder keeps the keys that control the assets. They are not accounts at an exchange, at a bank or with an online wallet service, and no third party holds the assets.",
            "The statement covers only the wallets it was prepared from. Assets the holder keeps elsewhere, such as at an exchange or in another wallet, are not part of it."
          ]

    it "says that it gives no value in a currency when it has no price" $
      fmap (any (Text.isInfixOf "The statement gives no value in a currency.") . notesOf) (reportOfYear [received]) `shouldBe` Right True

    it "is dated at the time it is made when it is a statement of now" $ do
      let report = statementReport preparedAt <$> statementAt AtPreparation [savings [received]]
      fmap (\made -> (reportSubtitle made, [factLines fact | FactsBlock facts <- reportBlocks made, fact <- facts, factName fact == "Statement date"])) report
        `shouldBe` Right ("As of 15 November 2026", [["15 November 2026, 14:32 (UTC+01:00)"]])

    it "says of a network that was never synced that it is not synchronised" $ do
      let report = statementReport preparedAt <$> statementAt AtPreparation [savings [received]]
      fmap (\made -> [factLines fact | FactsBlock facts <- reportBlocks made, fact <- facts, factName fact == "Blockchain data"]) report
        `shouldBe` Right [["Bitcoin: not synchronised"]]

    -- 40000 paid + 1000 of fee = 41000 satoshis, 0.00041000 coins
    it "says in a note what the unconfirmed transactions left out would change" $ do
      let report = statementReport preparedAt <$> statementAt AtPreparation [savings [spentUnconfirmed, received]]
      fmap (any (Text.isInfixOf "On Bitcoin, 1 unconfirmed transaction was left out, which would change the balance by -0.00041000 BTC.") . notesOf) report `shouldBe` Right True

    it "names each wallet with its description when it covers several, and each portfolio with its wallet" $ do
      let ofWallet1 = PortfolioHistory (PortfolioRef 1 0) "Company" "" [(btc, [])]
          report = statementReport preparedAt <$> statementAt AtPreparation [savings [received], ofWallet1]
      fmap (\made -> ([factLines fact | FactsBlock facts <- reportBlocks made, fact <- facts, factName fact == "Wallets"], take 1 (drop 1 (tablesOf made)))) report
        `shouldBe` Right
          ( [["wallet0, 1 portfolio", "wallet1, 1 portfolio: Company"]],
            [[["portfolio0 of wallet0", "Savings", "1", "0.00050000"], ["portfolio0 of wallet1", "", "0", "0.00000000"], ["Total Bitcoin", "", "1", "0.00050000"]]]
          )

    -- The user asked for a report without any brand, and later for the
    -- logo of hw on it. The name in the logo is the one place a report
    -- names a program.
    it "names no program on any of its pages, but hw once, in its logo" $ do
      let texts = either (const []) (\report -> [Text.toLower text | page <- reportPages report, TextMark _ _ _ _ _ _ text <- page]) (reportOfYear [received])
      ([text | text <- texts, "hw" `elem` Text.words text], [text | text <- texts, "haskoin" `Text.isInfixOf` text]) `shouldBe` (["hw"], [])

  describe "A statement with prices" $ do
    -- 50000 satoshis are 0.0005 bitcoins: 0.0005 x 87500 = 43.75
    it "states the price and the value of each asset in the summary, and their total" $
      fmap (take 1 . tablesOf . statementReport preparedAt) valuedOfYear
        `shouldBe` Right
          [ [ ["Bitcoin", "BTC", "1", "0.00050000", "87,500.00", "43.75"],
              ["Bitcoin Cash", "BCH", "0", "0.00000000", "", ""],
              ["Total value", "", "", "", "", "43.75"]
            ]
          ]

    it "names the currency in the titles of the columns of prices and values" $
      fmap (\statement -> [columnTitle column | TableBlock table <- take 3 (reportBlocks (statementReport preparedAt statement)), column <- tableColumns table]) valuedOfYear
        `shouldBe` Right ["Asset", "Unit", "Funded addresses", "Balance", "Price (CHF)", "Value (CHF)"]

    it "states the value of each portfolio and of the total of its asset" $
      fmap (take 1 . drop 1 . tablesOf . statementReport preparedAt) valuedOfYear
        `shouldBe` Right [[["portfolio0", "Savings", "1", "0.00050000", "43.75"], ["Total Bitcoin", "", "1", "0.00050000", "43.75"]]]

    it "states no value for an address" $
      fmap (drop 2 . tablesOf . statementReport preparedAt) valuedOfYear
        `shouldBe` Right [[["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "bip44/0/0", "0.00050000"], ["Total portfolio0", "", "0.00050000"]]]

    -- 50000 satoshis of bitcoin at 87500 are 43.75, and 50000 satoshis
    -- of Bitcoin Cash at 400 are 0.20: 43.95 in all
    it "adds up the values of the assets" $ do
      let onBoth = [PortfolioHistory mainPortfolioRef "" "Savings" [(btc, [ListedTx received Nothing]), (bch, [ListedTx received Nothing])]]
          statement = valued (Valuation "CHF" [(btc, Price 87500 0), (bch, Price 400 0)] "") =<< balanceStatement EveryWallet (const cet) AtPreparation [(btc, Nothing), (bch, Nothing)] onBoth
      fmap (map last . take 1 . tablesOf . statementReport preparedAt) statement `shouldBe` Right [["Total value", "", "", "", "", "43.95"]]

    it "says in its facts and in a note that the holder gave the prices, and where they came from" $
      fmap (\statement -> let report = statementReport preparedAt statement in (factsNamed "Valuation" report, [note | note <- notesOf report, "Values are in CHF." `Text.isPrefixOf` note])) valuedOfYear
        `shouldBe` Right
          ( [["In CHF, at the prices the holder gave", "Source of the prices: the tax office"]],
            ["Values are in CHF. A value is a balance times the price of its asset, rounded to the nearest hundredth. The holder gave the prices. Their source: the tax office. The total of an asset is worked out from its total balance, so it may differ by a hundredth from the sum of the lines above it."]
          )

    it "is refused when an asset the wallets hold has no price: the total would leave it out" $
      (valued (Valuation "CHF" [(bch, Price 400 0)] "") =<< statementOfYear [received])
        `shouldBe` Left "The wallets hold coins on btc, which has no price: give one with --price btc=PRICE, or the total value would leave them out"

    it "needs no price for an asset the wallets hold none of" $
      fmap statementValuation valuedOfYear `shouldBe` Right (Just (Valuation "CHF" [(btc, Price 87500 0)] "the tax office"))

    it "is refused a price for a network it does not cover" $
      (valued (Valuation "CHF" [(btc, Price 87500 0), (bch, Price 400 0)] "") =<< statementAt AtPreparation [savings [received]])
        `shouldBe` Left "The statement does not cover bch, which --price names: it covers btc"

    it "is refused two prices for one network" $
      (valued (Valuation "CHF" [(btc, Price 87500 0), (btc, Price 90000 0)] "") =<< statementOfYear [received])
        `shouldBe` Left "--price names btc twice: give one price for each network"

  -- The references below are the first eight digits of the SHA-256 hash
  -- of the lines a statement states, worked out apart from hw with
  -- sha256sum. For the statement of the payment received, the lines are
  -- "end of 2023-12-31", "holder " and "btc 0 0
  -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj 50000".
  describe "The reference of a statement" $ do
    it "is eight digits of the hash of its date, its holder and its balances" $
      fmap (statementReference preparedAt) (statementOfYear [received]) `shouldBe` Right "5D69-3866"

    it "is the same whenever the statement for a day is made" $ do
      let aYearLater = ZonedTime (LocalTime (fromGregorian 2027 11 15) (TimeOfDay 9 0 0)) cet
      fmap (statementReference aYearLater) (statementOfYear [received]) `shouldBe` Right "5D69-3866"

    it "is the same whatever hash the last block counted is given" $
      fmap (statementReference preparedAt . namingLastBlocks [(btc, 800000, fakeBlockHash 9)]) (statementOfYear [received]) `shouldBe` Right "5D69-3866"

    -- The second line is then "holder Jane Doe"
    it "changes with the holder" $
      fmap (statementReference preparedAt . (\statement -> statement {statementHolder = "Jane Doe"})) (statementOfYear [received]) `shouldBe` Right "642B-DE54"

    -- Two lines follow the balances: "currency CHF" and "price btc
    -- 87,500.00"
    it "changes with the prices" $
      fmap (statementReference preparedAt) valuedOfYear `shouldBe` Right "2915-A236"

    -- The first line is then "at 2026-11-15 14:32"
    it "of a statement of now follows the minute it is made" $
      fmap (statementReference preparedAt) (statementAt AtPreparation [savings [received]]) `shouldBe` Right "EAC0-5731"

  describe "The hash of the last block a statement counts" $ do
    it "is to be asked when that block is not the one the last sync reached" $
      fmap lastBlocksUnnamed (statementOfYear [received]) `shouldBe` Right [(btc, 800000)]

    it "is known when that block is the one the last sync reached" $ do
      let syncedAtPayment = SyncedTo 800000 (fakeBlockHash 2) (Just (UTCTime (fromGregorian 2024 1 2) (12 * 3600)))
          statement = balanceStatement EveryWallet (const cet) AtPreparation [(btc, Just syncedAtPayment)] [savings [received]]
      fmap (\made -> (lastBlocksUnnamed made, map (fmap blockNamedHash . assetLastBlock) (statementAssets made))) statement
        `shouldBe` Right ([], [Just (Just (fakeBlockHash 2))])

    it "is the one given for its network and its height" $
      fmap (map (fmap blockNamedHash . assetLastBlock) . statementAssets . namingLastBlocks [(btc, 800000, fakeBlockHash 9)]) (statementOfYear [received])
        `shouldBe` Right [Just (Just (fakeBlockHash 9)), Nothing]

    it "is not one given for another height or another network" $
      fmap (lastBlocksUnnamed . namingLastBlocks [(btc, 800001, fakeBlockHash 9), (bch, 800000, fakeBlockHash 9)]) (statementOfYear [received])
        `shouldBe` Right [(btc, 800000)]
