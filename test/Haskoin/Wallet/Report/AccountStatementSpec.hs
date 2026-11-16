{-# LANGUAGE OverloadedStrings #-}

-- | The account statement of report: the opening balance of a period,
-- its lines with the balance after each, which transactions it lists,
-- when a statement is refused, and what the report says. The story of
-- the transactions is that of "Haskoin.Wallet.Report.TestUtils".
module Haskoin.Wallet.Report.AccountStatementSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime (..), TimeOfDay (..), TimeZone (..), UTCTime (..), ZonedTime (..), fromGregorian)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Report.AccountStatement
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.Pdf (Mark (..))
import Haskoin.Wallet.Report.Price (Price (..))
import Haskoin.Wallet.Report.Statement
import Haskoin.Wallet.Report.TestUtils
-- The rows of a table are those of a report here, not those of a
-- table of the database
import Haskoin.Wallet.TestUtils hiding (tableRows)
import Test.Hspec

-- The payment of 50000 is received on 14 Nov 2023, and 41000 of it are
-- spent the same day, fee included, unless a test dates them otherwise.
-- A history lists its transactions the newest first, so the payment
-- made comes before the payment received in the lists below.

-- | The first, the fifteenth and the last day of November 2023
november1, november15, november30 :: Day
november1 = fromGregorian 2023 11 1
november15 = fromGregorian 2023 11 15
november30 = fromGregorian 2023 11 30

-- | 20 Nov 2023 at 12:00 UTC, 13:00 in Central Europe
on20th :: UTCTime
on20th = UTCTime (fromGregorian 2023 11 20) (12 * 3600)

-- | The statement of portfolio0 with the transactions given, the newest
-- first, from the start of a day to the date given, on a clock of
-- Central Europe in winter, on btc alone, which was never synced
statementOver :: Day -> StatementDate -> [TxInfo] -> Either String AccountStatement
statementOver firstDay end transactions = accountStatement EveryWallet (const cet) (PeriodStart firstDay cet) end [(btc, Nothing)] [savings transactions]

-- | The accounts of a statement on its first network, one for each
-- portfolio
accountsOf :: Either String AccountStatement -> Either String [PortfolioActivity]
accountsOf = fmap (concatMap activityPortfolios . take 1 . accountsAssets)

-- | The lines of the first account of a statement, each as what the
-- function reads of it
linesOf :: (StatementLine -> a) -> Either String AccountStatement -> Either String [a]
linesOf reading = fmap (map reading . concatMap activityLines . take 1) . accountsOf

-- | The statement of November 2023 of portfolio0 with the transactions
-- given, the newest first, on btc and bch, both synced in January 2024
statementOfNovember :: [TxInfo] -> Either String AccountStatement
statementOfNovember transactions =
  accountStatement
    EveryWallet
    (const cet)
    (PeriodStart november1 cet)
    (AtEndOf november30 cet)
    [(btc, Just syncedInJanuary), (bch, Just syncedInJanuary)]
    [PortfolioHistory mainPortfolioRef "" "Savings" [(btc, [ListedTx info Nothing | info <- transactions]), (bch, [])]]

-- | The report of that statement, made at 'preparedAt'
reportOfNovember :: [TxInfo] -> Either String Report
reportOfNovember transactions = accountStatementReport preparedAt <$> statementOfNovember transactions

-- | That statement of the two payments, valued in Swiss francs at 87500
-- a bitcoin, a price that came from "the tax office"
valuedOfNovember :: Either String AccountStatement
valuedOfNovember = accountsValued (Valuation "CHF" [(btc, Price 87500 0)] "the tax office") =<< statementOfNovember [spent, received]

-- | The id of the transaction 1 of the tests as hw writes it: 62 zeros,
-- then 01. That of the transaction 2 ends in 02.
txid1, txid2 :: Text
txid1 = "0000000000000000000000000000000000000000000000000000000000000001"
txid2 = "0000000000000000000000000000000000000000000000000000000000000002"

spec :: Spec
spec = do
  describe "The opening balance of an account" $ do
    it "is zero when no transaction came before the period" $
      fmap (map activityOpening) (accountsOf (statementOver november1 (AtEndOf november30 cet) [spent, received])) `shouldBe` Right [0]

    -- The payment received on the 14th came before the period, and the
    -- payment made on the 20th is in it
    it "is what the transactions before the period add up to" $
      fmap (map activityOpening) (accountsOf (statementOver november15 (AtEndOf november30 cet) [datedAt on20th spent, received])) `shouldBe` Right [50000]

    -- 50000 received - 41000 spent = 9000, all before December
    it "counts every transaction before the period, a payment made too" $
      fmap (map activityOpening) (accountsOf (statementOver (fromGregorian 2023 12 1) (AtEndOf (fromGregorian 2023 12 31) cet) [spent, received])) `shouldBe` Right [9000]

    -- The first statement stops on the 14th, after the payment received.
    -- The second starts on the 15th.
    it "is the closing balance of the statement of the days before" $ do
      let history = [datedAt on20th spent, received]
          closing = fmap (map lineBalance . take 1 . reverse) . linesOf id
      (closing (statementOver november1 (AtEndOf day14 cet) history), fmap (map activityOpening) (accountsOf (statementOver november15 (AtEndOf november30 cet) history)))
        `shouldBe` (Right [50000], Right [50000])

    -- The block 800000 is dated on the 20th, in the period, and the block
    -- 800001 after it on the 14th, before the period: both count for
    -- the opening balance, 50000 - 41000 = 9000
    it "counts a block dated in the period when a later block is dated before it" $
      fmap (map (\account -> (activityOpening account, length (activityLines account)))) (accountsOf (statementOver november15 (AtEndOf november30 cet) [spent, datedAt on20th received]))
        `shouldBe` Right [(9000, 0)]

  describe "The lines of an account" $ do
    -- 50000 received, then 41000 spent: 50000 - 41000 = 9000
    it "are the transactions of the period, the oldest first, each with what the portfolio gained or lost and the balance after it" $
      linesOf (\line -> (lineAmount line, lineBalance line)) (statementOver november1 (AtEndOf november30 cet) [spent, received])
        `shouldBe` Right [(50000, 50000), (-41000, 9000)]

    it "name each transaction by its id and by the height of its block" $
      linesOf (\line -> (lineTxid line, lineBlock line)) (statementOver november1 (AtEndOf november30 cet) [spent, received])
        `shouldBe` Right [(fakeTxHash 1, 800000), (fakeTxHash 2, 800001)]

    it "are dated on the day of their block" $
      linesOf lineDay (statementOver november1 (AtEndOf november30 cet) [datedAt on20th spent, received])
        `shouldBe` Right [Just day14, Just (fromGregorian 2023 11 20)]

    -- 22:13:20 UTC on the 14th is 00:13:20 on the 15th on a clock two
    -- hours ahead of UTC
    it "read the day of a block on the clock of the computer" $ do
      let twoAhead = TimeZone 120 False "EET"
      linesOf lineDay (accountStatement EveryWallet (const twoAhead) (PeriodStart november1 twoAhead) (AtEndOf november30 twoAhead) [(btc, Nothing)] [savings [received]])
        `shouldBe` Right [Just november15]

    it "leave out a transaction that came before the period" $
      linesOf lineTxid (statementOver november15 (AtEndOf november30 cet) [datedAt on20th spent, received]) `shouldBe` Right [fakeTxHash 2]

    it "leave out a transaction that came after the period" $
      linesOf lineTxid (statementOver november1 (AtEndOf november15 cet) [datedAt on20th spent, received]) `shouldBe` Right [fakeTxHash 1]

    it "leave out an unconfirmed transaction" $
      linesOf lineTxid (statementOver november1 AtPreparation [spentUnconfirmed, received]) `shouldBe` Right [fakeTxHash 1]

    it "leave out a dead transaction, even one that was in a block" $ do
      let dead = txOnChain spendSummary 2 (Store.BlockRef 800001 1) 0 True ""
      linesOf lineTxid (statementOver november1 AtPreparation [dead, received]) `shouldBe` Right [fakeTxHash 1]

    -- The block 800000 is dated on the 20th, after the period, and the
    -- block 800001 after it on the 14th, in the period
    it "list a block dated after the period when a later block is dated in it" $
      linesOf lineTxid (statementOver november1 (AtEndOf november15 cet) [spent, datedAt on20th received])
        `shouldBe` Right [fakeTxHash 1, fakeTxHash 2]

    it "state the fee of a payment the portfolio made, and none for a payment it received" $
      linesOf lineFee (statementOver november1 (AtEndOf november30 cet) [spent, received]) `shouldBe` Right [Nothing, Just 1000]

    it "are refused when they take the balance below zero: such a history is not sound" $
      statementOver november1 (AtEndOf november30 cet) [spent]
        `shouldBe` Left "The transactions of portfolio0 of wallet0 on btc take its balance below zero: hw sync --full reads them again"

  describe "The description of a line" $ do
    it "is the label of its transaction" $
      linesOf lineName (statementOver november1 AtPreparation [txOnChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False "Invoice 42"]) `shouldBe` Right ["Invoice 42"]

    it "is the label of the address a payment was received at, when the transaction has none" $
      linesOf lineName (statementOver november1 AtPreparation [txOnChain paymentToRent 1 (Store.BlockRef 800000 1) 3 False ""]) `shouldBe` Right ["Rent"]

    it "says what the transaction did when nothing has a label" $
      linesOf lineName (statementOver november1 AtPreparation [spent, received]) `shouldBe` Right ["Payment received", "Payment sent"]

    it "says internal transfer for a payment between the addresses of the portfolio" $
      linesOf lineName (statementOver november1 AtPreparation [txOnChain internalSummary 2 (Store.BlockRef 800001 1) 2 False "", received])
        `shouldBe` Right ["Payment received", "Internal transfer"]

  describe "A payment from one portfolio to another" $ do
    -- portfolio0 pays 40000 to an address of portfolio1, with 9000 of
    -- change and 1000 of fee: it loses 41000, and portfolio1 gains 40000
    let transfer =
          accountStatement
            EveryWallet
            (const cet)
            (PeriodStart november1 cet)
            AtPreparation
            [(btc, Nothing)]
            [savings [spent, received], spending]

    it "is a line of both accounts: sent by the first and received by the second" $
      fmap (map (map lineAmount . activityLines)) (accountsOf transfer) `shouldBe` Right [[50000, -41000], [40000]]

    it "is described as a transfer between portfolios in both accounts" $
      fmap (map (map lineName . activityLines)) (accountsOf transfer)
        `shouldBe` Right [["Payment received", "Transfer between portfolios"], ["Transfer between portfolios"]]

  describe "An account statement of one wallet or of one portfolio" $ do
    -- portfolio0 pays 40000 to an address of portfolio1, as above.
    -- wallet1 is described as "Business", and its portfolio0 received a
    -- payment of 50000 of its own.
    let business = PortfolioHistory (PortfolioRef 1 0) "Business" "Till" [(btc, [ListedTx received Nothing])]
        ofScope scope = accountStatement scope (const cet) (PeriodStart november1 cet) (AtEndOf november30 cet) [(btc, Nothing)]

    it "holds the accounts of the one wallet it is of" $
      fmap (map activityRef) (accountsOf (ofScope (OneWallet 0) [savings [spent, received], spending, business])) `shouldBe` Right [mainPortfolioRef, PortfolioRef 0 1]

    it "holds the account of the one portfolio it is of" $
      fmap (map activityRef) (accountsOf (ofScope (OnePortfolio (PortfolioRef 0 1)) [savings [spent, received], spending, business])) `shouldBe` Right [PortfolioRef 0 1]

    it "still describes a payment to a portfolio it leaves out as a transfer between portfolios" $
      fmap (map (map lineName . activityLines)) (accountsOf (ofScope (OnePortfolio mainPortfolioRef) [savings [spent, received], spending]))
        `shouldBe` Right [["Payment received", "Transfer between portfolios"]]

    it "says in its facts and in a note which portfolio it is of, and that it is of that one only" $
      fmap (\statement -> let report = accountStatementReport preparedAt statement in (factsNamed "Wallets" report, take 1 (drop 2 (notesOf report)))) (ofScope (OnePortfolio mainPortfolioRef) [savings [spent, received], spending])
        `shouldBe` Right
          ( [["portfolio0 only"]],
            ["The statement covers only portfolio0. The other wallets and portfolios of the holder are not part of it, and neither are assets the holder keeps elsewhere, such as at an exchange."]
          )

    -- The payment of portfolio0 is in block 800000, dated on the 20th,
    -- in the period. That of wallet1 is in block 800001, dated on the
    -- 14th, before the period, so every block up to 800001 counts for
    -- the opening balance. A statement of portfolio0 alone must open
    -- with the same 50000.
    it "opens an account with the balance it has in the statement of everything, whatever blocks the portfolios left out count" $ do
      let later = PortfolioHistory (PortfolioRef 1 0) "Business" "Till" [(btc, [ListedTx (txOnChain paymentSummary 3 (Store.BlockRef 800001 1) 2 False "") Nothing])]
      fmap (map (\account -> (activityOpening account, length (activityLines account)))) (accountsOf (accountStatement (OnePortfolio mainPortfolioRef) (const cet) (PeriodStart november15 cet) (AtEndOf november30 cet) [(btc, Nothing)] [savings [datedAt on20th received], later]))
        `shouldBe` Right [(50000, 0)]

    it "is the statement of everything when its portfolio is the only one" $
      fmap accountsScope (ofScope (OnePortfolio mainPortfolioRef) [savings [spent, received]]) `shouldBe` Right EveryWallet

    -- The lines are "from 2023-11-01", "end of 2023-11-30", "holder ",
    -- "scope portfolio 0 0", then the opening balance and the two lines
    -- of the account, hashed with sha256sum
    it "has a reference that states its scope" $
      fmap (accountStatementReference preparedAt) (ofScope (OnePortfolio mainPortfolioRef) [savings [spent, received], spending]) `shouldBe` Right "4956-272E"

  describe "An account statement and the last sync" $ do
    -- The last sync reached a block dated at seenTime, 23:13:20 on 14
    -- Nov 2023 in Central Europe
    let syncedOn14th = [(btc, Just (SyncedTo 800001 (fakeBlockHash 2) (Just seenTime)))]
    it "is refused when the last sync reached a block dated on the last day of the period" $
      accountStatement EveryWallet (const cet) (PeriodStart november1 cet) (AtEndOf day14 cet) syncedOn14th [savings [received]]
        `shouldBe` Left "The last sync of btc reached a block dated 14 November 2023, and the statement is for the end of 14 November 2023: it could miss transactions. Run hw sync first"

    it "is made when the last sync reached a block dated after the period" $
      linesOf lineTxid (accountStatement EveryWallet (const cet) (PeriodStart november1 cet) (AtEndOf day13 cet) syncedOn14th [savings [received]]) `shouldBe` Right []

    it "is made up to now however old the last sync is" $
      linesOf lineTxid (accountStatement EveryWallet (const cet) (PeriodStart november1 cet) AtPreparation syncedOn14th [savings [received]]) `shouldBe` Right [fakeTxHash 1]

  describe "An account statement that stops now" $ do
    it "says how many unconfirmed transactions it leaves out, and what they add up to" $
      fmap (map activityUnconfirmed . accountsAssets) (statementOver november1 AtPreparation [spentUnconfirmed, received]) `shouldBe` Right [(1, -41000)]

    it "that stops at the end of a day counts none as left out" $
      fmap (map activityUnconfirmed . accountsAssets) (statementOver november1 (AtEndOf november30 cet) [spentUnconfirmed, received]) `shouldBe` Right [(0, 0)]

  describe "The report of an account statement" $ do
    it "is titled Digital Asset Account Statement, with its period, and says above the title that it is about cryptocurrency" $
      fmap (\report -> (reportKind report, reportTitle report, reportSubtitle report, reportFooter report)) (reportOfNovember [spent, received])
        `shouldBe` Right ("Statement of cryptocurrency transactions", "Digital Asset Account Statement", "1 November 2023 to 30 November 2023", "Digital Asset Account Statement \183 1 November 2023 to 30 November 2023 \183 Ref. 1610-2DDB")

    it "starts with the facts: its period, when and by whom it was made, its reference, the custody, the wallets, the assets and the blocks of the last sync" $
      fmap (take 1 . reportBlocks) (reportOfNovember [spent, received])
        `shouldBe` Right
          [ FactsBlock
              [ Fact "Statement period" ["From 1 November 2023, 00:00 (UTC+01:00)", "To 30 November 2023, 24:00 (UTC+01:00)"],
                Fact "Prepared on" ["15 November 2026, 14:32 (UTC+01:00)"],
                Fact "Prepared by" ["The holder, from public blockchain data. Not audited."],
                Fact "Reference" ["1610-2DDB"],
                Fact "Custody" ["Self-custody: the holder keeps the keys. No exchange or online wallet."],
                Fact "Wallets" ["1 wallet, 1 portfolio"],
                Fact "Assets" ["Bitcoin (BTC), Bitcoin Cash (BCH)"],
                Fact "Blockchain data" ["Bitcoin: synchronised to block 800002, dated 2 January 2024, 13:00", "Bitcoin Cash: synchronised to block 800002, dated 2 January 2024, 13:00"]
              ]
          ]

    -- The clock is two hours ahead of UTC at both ends of June, and one
    -- hour ahead when the statement is made
    it "states each end of its period with the zone the clock has at that midnight" $ do
      let summer = TimeZone 120 True "CEST"
      fmap (factsNamed "Statement period" . accountStatementReport preparedAt) (accountStatement EveryWallet (const cet) (PeriodStart (fromGregorian 2023 6 1) summer) (AtEndOf (fromGregorian 2023 6 30) summer) [(btc, Nothing)] [savings []])
        `shouldBe` Right [["From 1 June 2023, 00:00 (UTC+02:00)", "To 30 June 2023, 24:00 (UTC+02:00)"]]

    it "stops at the time it is made when its last day is not over, and is titled up to that day" $ do
      let report = accountStatementReport preparedAt <$> statementOver (fromGregorian 2026 11 1) AtPreparation [spent, received]
      fmap (\made -> (reportSubtitle made, factsNamed "Statement period" made)) report
        `shouldBe` Right ("1 November 2026 to 15 November 2026", [["From 1 November 2026, 00:00 (UTC+01:00)", "To 15 November 2026, 14:32 (UTC+01:00)"]])

    it "names the holder first, when it has one" $
      fmap (take 1 . concat . (\report -> [[(factName fact, factLines fact) | fact <- facts] | FactsBlock facts <- reportBlocks report]) . accountStatementReport preparedAt . (\statement -> statement {accountsHolder = "Jane Doe"})) (statementOfNovember [spent, received])
        `shouldBe` Right [("Holder", ["Jane Doe"])]

    it "has its sections in order: the summary, the portfolios, the transactions, the blocks, the notes" $
      fmap (\report -> [title | SectionBlock title <- reportBlocks report]) (reportOfNovember [spent, received])
        `shouldBe` Right ["Summary", "Activity by portfolio", "Transactions by portfolio", "Blockchain data", "Notes"]

    -- Nothing before November, 0.00050000 received, 0.00041000 sent:
    -- 0.00050000 - 0.00041000 = 0.00009000 at the end
    it "sums up each asset: its opening balance, what was received, what was sent and its closing balance, an asset without a transaction too" $
      fmap (take 1 . tablesOf) (reportOfNovember [spent, received])
        `shouldBe` Right
          [ [ ["Bitcoin (BTC)", "0.00000000", "0.00050000", "0.00041000", "0.00009000"],
              ["Bitcoin Cash (BCH)", "0.00000000", "0.00000000", "0.00000000", "0.00000000"]
            ]
          ]

    it "says under each asset of the summary how many transactions it has in the period" $
      fmap (\report -> [cellRemark cell | TableBlock table <- take 1 [block | block@(TableBlock _) <- reportBlocks report], row <- tableRows table, cell <- take 1 (rowCells row)]) (reportOfNovember [spent, received])
        `shouldBe` Right ["2 transactions", "0 transactions"]

    it "sums up each portfolio of an asset that has an account to list, and their total" $
      fmap (take 1 . drop 1 . tablesOf) (reportOfNovember [spent, received])
        `shouldBe` Right
          [ [ ["portfolio0", "0.00000000", "0.00050000", "0.00041000", "0.00009000"],
              ["Total Bitcoin", "0.00000000", "0.00050000", "0.00041000", "0.00009000"]
            ]
          ]

    it "lists the account of a portfolio: its opening balance, its lines with the balance after each, then its totals and its closing balance" $
      fmap (drop 2 . tablesOf) (reportOfNovember [spent, received])
        `shouldBe` Right
          [ [ ["1 Nov 2023", "Opening balance", "", "", "0.00000000"],
              ["14 Nov 2023", "Payment received", "0.00050000", "", "0.00050000"],
              ["14 Nov 2023", "Payment sent", "", "0.00041000", "0.00009000"],
              ["30 Nov 2023", "Closing balance", "0.00050000", "0.00041000", "0.00009000"]
            ]
          ]

    -- The fee of 1000 satoshis is 0.00001000 coins
    it "writes under each line the id of its transaction, its block, and the fee the portfolio paid" $
      fmap (\report -> [cellRemark cell | TableBlock table <- drop 2 [block | block@(TableBlock _) <- reportBlocks report], row <- tableRows table, cell <- rowCells row, not (Text.null (cellRemark cell))]) (reportOfNovember [spent, received])
        `shouldBe` Right [txid1 <> " \183 Block 800000", txid2 <> " \183 Block 800001 \183 Fee 0.00001000"]

    it "heads the tables and the blocks of an asset with its name and its unit, and the account of a portfolio with its description" $
      fmap (\report -> [(title, remark) | GroupBlock title remark <- reportBlocks report]) (reportOfNovember [spent, received])
        `shouldBe` Right [("Bitcoin (BTC)", ""), ("portfolio0 \183 Bitcoin (BTC)", "Savings"), ("Bitcoin (BTC)", ""), ("Bitcoin Cash (BCH)", "")]

    it "lists the account of a portfolio that held coins and had no transaction" $
      fmap (drop 2 . tablesOf . accountStatementReport preparedAt) (statementOver (fromGregorian 2023 12 1) (AtEndOf (fromGregorian 2023 12 31) cet) [spent, received])
        `shouldBe` Right
          [ [ ["1 Dec 2023", "Opening balance", "", "", "0.00009000"],
              ["31 Dec 2023", "Closing balance", "0.00000000", "0.00000000", "0.00009000"]
            ]
          ]

    it "has no table of portfolios or of transactions when nothing was held and nothing happened" $
      fmap (\report -> ([title | SectionBlock title <- reportBlocks report], length (tablesOf report))) (reportOfNovember [])
        `shouldBe` Right (["Summary", "Blockchain data", "Notes"], 1)

    -- No transaction came before November, so no block is before the
    -- period. The payment made is the last one counted, in block 800001.
    it "names on each network the block of the last sync, the last block before the period and the last block counted" $
      fmap (\report -> [(factName fact, factLines fact) | FactsBlock facts <- drop 1 (reportBlocks report), fact <- facts]) (reportOfNovember [spent, received])
        `shouldBe` Right
          [ ("Last block of the sync", ["Block 800002, dated 2 January 2024, 13:00", "Hash " <> hashOfBlock2]),
            ("Last block before the period", ["None: no transaction came before the period"]),
            ("Last block counted", ["Block 800001, dated 14 November 2023, 23:13", "Hash not available: the server did not give it"]),
            ("Last block of the sync", ["Block 800002, dated 2 January 2024, 13:00", "Hash " <> hashOfBlock2]),
            ("Last block before the period", ["None: no transaction came before the period"]),
            ("Last block counted", ["None: the statement counts no transaction"])
          ]

    it "says first who prepared it, that the wallets are self-custody wallets, and what it covers" $
      fmap (take 3 . notesOf) (reportOfNovember [spent, received])
        `shouldBe` Right
          [ "This statement was prepared by the holder of the wallets, from the data of public blockchains. No third party has audited or confirmed it.",
            "The wallets are self-custody wallets: the holder keeps the keys that control the assets. They are not accounts at an exchange, at a bank or with an online wallet service, and no third party holds the assets.",
            "The statement covers only the wallets it was prepared from. Assets the holder keeps elsewhere, such as at an exchange or in another wallet, are not part of it."
          ]

    it "says in a note that an amount sent includes the fee" $
      fmap (filter ("An amount received" `Text.isPrefixOf`) . notesOf) (reportOfNovember [spent, received])
        `shouldBe` Right ["An amount received is what a transaction added to the addresses of a portfolio, and an amount sent is what it took from them. An amount sent includes the transaction fee. A line states the fee when the portfolio paid all of it."]

    it "says that it gives no value in a currency when it has no price" $
      fmap (filter ("Amounts are in units" `Text.isPrefixOf`) . notesOf) (reportOfNovember [spent, received])
        `shouldBe` Right ["Amounts are in units of each asset, to eight decimal places. One unit is 100,000,000 satoshis. The statement gives no value in a currency."]

    -- The payment made is unconfirmed: it would take 41000 satoshis
    -- from the balance
    it "says in a note what the unconfirmed transactions left out would change" $
      fmap (filter ("Transactions that were not confirmed" `Text.isPrefixOf`) . notesOf . accountStatementReport preparedAt) (statementOver november1 AtPreparation [spentUnconfirmed, received])
        `shouldBe` Right ["Transactions that were not confirmed are not listed. On Bitcoin, 1 unconfirmed transaction was left out, which would change the balance by -0.00041000 BTC."]

    -- The name in the logo is the one place a report names a program
    it "names no program on any of its pages, but hw once, in its logo" $ do
      let texts = either (const []) (\report -> [Text.toLower text | page <- reportPages report, TextMark _ _ _ _ _ _ text <- page]) (reportOfNovember [spent, received])
      ([text | text <- texts, "hw" `elem` Text.words text], [text | text <- texts, "haskoin" `Text.isInfixOf` text]) `shouldBe` (["hw"], [])

  describe "An account statement with prices" $ do
    -- 9000 satoshis are 0.00009 bitcoins: 0.00009 x 87500 = 7.875,
    -- which rounds to 7.88
    it "states the value of each closing balance at the price of its asset, and their total" $
      fmap (take 1 . drop 1 . tablesOf . accountStatementReport preparedAt) valuedOfNovember
        `shouldBe` Right
          [ [ ["Bitcoin (BTC)", "0.00009000", "87,500.00", "7.88"],
              ["Bitcoin Cash (BCH)", "0.00000000", "", ""],
              ["Total value", "", "", "7.88"]
            ]
          ]

    it "names the currency in the titles of the columns of prices and values" $
      fmap (\statement -> [columnTitle column | TableBlock table <- take 1 (drop 1 [block | block@(TableBlock _) <- reportBlocks (accountStatementReport preparedAt statement)]), column <- tableColumns table]) valuedOfNovember
        `shouldBe` Right ["Asset", "Closing balance", "Price (CHF)", "Value (CHF)"]

    it "states no value for a transaction" $
      fmap (take 1 . drop 3 . tablesOf . accountStatementReport preparedAt) valuedOfNovember
        `shouldBe` Right
          [ [ ["1 Nov 2023", "Opening balance", "", "", "0.00000000"],
              ["14 Nov 2023", "Payment received", "0.00050000", "", "0.00050000"],
              ["14 Nov 2023", "Payment sent", "", "0.00041000", "0.00009000"],
              ["30 Nov 2023", "Closing balance", "0.00050000", "0.00041000", "0.00009000"]
            ]
          ]

    it "says in its facts and in a note that the holder gave the prices, where they came from, and what they value" $
      fmap (\statement -> let report = accountStatementReport preparedAt statement in (factsNamed "Valuation" report, [note | note <- notesOf report, "Values are in CHF." `Text.isPrefixOf` note])) valuedOfNovember
        `shouldBe` Right
          ( [["In CHF, at the prices the holder gave", "Source of the prices: the tax office"]],
            ["Values are in CHF. The value of a closing balance is the balance times the price of its asset, rounded to the nearest hundredth. The holder gave the prices. Their source: the tax office. The prices are those of the end of the period, so the transactions and the opening balances are not valued."]
          )

    it "is refused when an asset the wallets hold at the end of the period has no price: the total would leave it out" $
      (accountsValued (Valuation "CHF" [(bch, Price 400 0)] "") =<< statementOfNovember [spent, received])
        `shouldBe` Left "The wallets hold coins on btc, which has no price: give one with --price btc=PRICE, or the total value would leave them out"

    it "needs no price for an asset the wallets hold none of at the end of the period" $
      fmap accountsValuation valuedOfNovember `shouldBe` Right (Just (Valuation "CHF" [(btc, Price 87500 0)] "the tax office"))

    it "is refused a price for a network it does not cover" $
      (accountsValued (Valuation "CHF" [(btc, Price 87500 0), (bch, Price 400 0)] "") =<< statementOver november1 AtPreparation [spent, received])
        `shouldBe` Left "The statement does not cover bch, which --price names: it covers btc"

  -- The references below are the first eight digits of the SHA-256 hash
  -- of the lines a statement states, worked out apart from hw with
  -- sha256sum. For the statement of November of the two payments, the
  -- lines are "from 2023-11-01", "end of 2023-11-30", "holder ", "btc 0
  -- 0 opening 0", "btc 0 0 " then the id of the payment received then
  -- " 50000", "btc 0 0 " then the id of the payment made then
  -- " -41000", and "bch 0 0 opening 0".
  describe "The reference of an account statement" $ do
    it "is eight digits of the hash of its period, its holder, its opening balances and its lines" $
      fmap (accountStatementReference preparedAt) (statementOfNovember [spent, received]) `shouldBe` Right "1610-2DDB"

    it "is the same whenever the statement of a period is made" $ do
      let aYearLater = ZonedTime (LocalTime (fromGregorian 2027 11 15) (TimeOfDay 9 0 0)) cet
      fmap (accountStatementReference aYearLater) (statementOfNovember [spent, received]) `shouldBe` Right "1610-2DDB"

    it "is the same whatever hash the blocks it names are given" $
      fmap (accountStatementReference preparedAt . namingAccountBlocks [(btc, 800001, fakeBlockHash 9)]) (statementOfNovember [spent, received]) `shouldBe` Right "1610-2DDB"

    -- The third line is then "holder Jane Doe"
    it "changes with the holder" $
      fmap (accountStatementReference preparedAt . (\statement -> statement {accountsHolder = "Jane Doe"})) (statementOfNovember [spent, received]) `shouldBe` Right "B91B-8CFA"

    -- Two lines follow the accounts: "currency CHF" and "price btc
    -- 87,500.00"
    it "changes with the prices" $
      fmap (accountStatementReference preparedAt) valuedOfNovember `shouldBe` Right "B42F-F589"

  describe "The hashes of the blocks an account statement names" $ do
    -- The payment received on the 14th is the last block before the
    -- period, and the payment made on the 20th the last block counted
    let fromThe15th =
          accountStatement
            EveryWallet
            (const cet)
            (PeriodStart november15 cet)
            (AtEndOf november30 cet)
            [(btc, Just syncedInJanuary)]
            [savings [datedAt on20th spent, received]]

    it "are to be asked for the last block before the period and for the last block counted" $
      fmap accountBlocksUnnamed fromThe15th `shouldBe` Right [(btc, 800000), (btc, 800001)]

    it "are asked once when the two are one block, as in a period without a transaction" $
      fmap accountBlocksUnnamed (accountStatement EveryWallet (const cet) (PeriodStart november15 cet) (AtEndOf november30 cet) [(btc, Just syncedInJanuary)] [savings [received]])
        `shouldBe` Right [(btc, 800000)]

    it "are those given for their network and their height" $
      fmap (map (\asset -> (blockNamedHash <$> activityOpeningBlock asset, blockNamedHash <$> activityLastBlock asset)) . accountsAssets . namingAccountBlocks [(btc, 800000, fakeBlockHash 8), (btc, 800001, fakeBlockHash 9)]) fromThe15th
        `shouldBe` Right [(Just (Just (fakeBlockHash 8)), Just (Just (fakeBlockHash 9)))]

  describe "A day in the lines of a table" $
    it "is written short, with its month as a word and no zero before the day" $
      (shortDate (fromGregorian 2025 12 31), shortDate (fromGregorian 2026 1 5)) `shouldBe` ("31 Dec 2025", "5 Jan 2026")
