{-# LANGUAGE OverloadedStrings #-}

-- | The payment confirmation of report: which side of a payment the
-- wallets are on, what the confirmation lists, when it is refused, and
-- what the report says. The story of the transactions is that of
-- "Haskoin.Wallet.Report.TestUtils".
module Haskoin.Wallet.Report.PaymentConfirmationSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import Data.Word (Word8)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.PaymentConfirmation
import Haskoin.Wallet.Report.Price (Price (..))
import Haskoin.Wallet.Report.Statement
import Haskoin.Wallet.Report.TestUtils
-- The rows of a table are those of a report here, not those of a
-- table of the database
import Haskoin.Wallet.TestUtils hiding (tableRows)
import Test.Hspec

-- The transaction 1 pays 50000 to portfolio0, at
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, in block 800000. The transaction
-- 2 spends that coin in block 800001: 40000 to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, an address of others, 9000 of
-- change to 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, and 1000 of fee. Both
-- blocks are dated 14 Nov 2023 at 23:13:20 on a clock of Central Europe.

-- | The confirmation of a transaction, by the last byte of its txid,
-- from the side of a scope, listing the addresses given, on a clock of
-- Central Europe in winter. btc was synced in January 2024, up to block
-- 800002.
confirming :: StatementScope -> Word8 -> [Text] -> [PortfolioHistory] -> Either String PaymentConfirmation
confirming scope txid listed = paymentConfirmation scope (const cet) (fakeTxHash txid) listed [(btc, Just syncedInJanuary)]

-- | The confirmation of the payment made, the transaction 2, and that
-- of the payment received, the transaction 1, of portfolio0 alone
ofSpent, ofReceived :: Either String PaymentConfirmation
ofSpent = confirming EveryWallet 2 [] [savings [spent, received]]
ofReceived = confirming EveryWallet 1 [] [savings [spent, received]]

-- | The transaction 2 as a payment to two addresses of others: 60000 to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 30000 to
-- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ, with 9000 of change and 1000 of
-- fee
ofBatch :: [Text] -> Either String PaymentConfirmation
ofBatch listed = confirming EveryWallet 2 listed [savings [txOnChain batchSummary 2 (Store.BlockRef 800001 1) 2 False ""]]

-- | The report of a confirmation, made at 'preparedAt'
reportOf :: Either String PaymentConfirmation -> Either String Report
reportOf = fmap (paymentReport preparedAt)

-- | The id of the transaction 2 as hw writes it: 62 zeros, then 02
txid2 :: String
txid2 = "0000000000000000000000000000000000000000000000000000000000000002"

spec :: Spec
spec = do
  describe "The confirmation of a payment sent" $ do
    it "is of a payment sent when the portfolio made the transaction" $
      fmap paymentSide ofSpent `shouldBe` Right PaymentSent

    it "lists what the transaction pays to the addresses of others, and not its change" $
      fmap paymentPaid ofSpent `shouldBe` Right [PaidAddress "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" 40000]

    it "keeps apart what the transaction returns to the payer as change" $
      fmap paymentChange ofSpent `shouldBe` Right 9000

    it "states the fee of the transaction" $
      fmap paymentFee ofSpent `shouldBe` Right 1000

    it "lists each address of a payment to several, the one paid the most first" $
      fmap paymentPaid (ofBatch [])
        `shouldBe` Right [PaidAddress "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" 60000, PaidAddress "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" 30000]

    it "names the transaction and the height of its block" $
      fmap (\payment -> (paymentTxid payment, blockNamedHeight (paymentBlock payment))) ofSpent `shouldBe` Right (fakeTxHash 2, 800001)

    -- 22:13:20 UTC is 23:13:20 on a clock an hour ahead of UTC
    it "is dated as its block is, on the clock of the computer, with the zone of that clock" $
      fmap paymentTime ofSpent `shouldBe` Right (Just (LocalTime (fromGregorian 2023 11 14) (TimeOfDay 23 13 20), cet))

    it "states the confirmations the transaction has" $
      fmap paymentConfirmations ofSpent `shouldBe` Right 2

  describe "The confirmation of a payment received" $ do
    it "is of a payment received when the portfolio spends nothing in the transaction" $
      fmap paymentSide ofReceived `shouldBe` Right PaymentReceived

    it "lists what the transaction pays to the addresses of the portfolio" $
      fmap paymentPaid ofReceived `shouldBe` Right [PaidAddress "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 50000]

  describe "A confirmation of the addresses given" $ do
    it "lists the one address given, out of the two the transaction pays" $
      fmap paymentPaid (ofBatch ["1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"]) `shouldBe` Right [PaidAddress "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" 30000]

    -- The transaction pays 60000 to the address that is not listed
    it "says what the payments it leaves out add up to" $
      fmap paymentLeftOut (ofBatch ["1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"]) `shouldBe` Right 60000

    it "leaves nothing out when no address is given" $
      fmap paymentLeftOut (ofBatch []) `shouldBe` Right 0

    it "is refused an address the transaction pays as change, and names the addresses it could list" $
      ofBatch ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"]
        `shouldBe` Left "--address names 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, which is not among the addresses the confirmation lists: 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"

    it "is refused a word that is no address of the network of the payment" $
      ofBatch ["nonsense"] `shouldBe` Left "nonsense is not an address of btc"

  describe "A transaction that is no payment to confirm" $ do
    it "is refused when the wallet does not have it" $
      confirming EveryWallet 9 [] [savings [spent, received]]
        `shouldBe` Left "The wallet has no transaction 0000000000000000000000000000000000000000000000000000000000000009"

    it "is refused while it is not in a block: it could still be replaced" $
      confirming EveryWallet 2 [] [savings [spentUnconfirmed, received]]
        `shouldBe` Left ("The transaction " <> txid2 <> " is not confirmed yet, and could still be replaced: a payment is confirmed once its transaction is in a block. Run hw sync once it is")

    it "is refused when it was replaced or dropped: it paid nothing" $
      confirming EveryWallet 2 [] [savings [txOnChain spendSummary 2 (Store.BlockRef 800001 1) 0 True "", received]]
        `shouldBe` Left ("The transaction " <> txid2 <> " was replaced or dropped from the network, and paid nothing: hw show tells whether another transaction took its place")

    it "is refused when it only moves coins between the addresses of the wallet: it pays nobody" $
      confirming EveryWallet 2 [] [savings [txOnChain internalSummary 2 (Store.BlockRef 800001 1) 2 False "", received]]
        `shouldBe` Left ("The transaction " <> txid2 <> " pays no one outside the wallet: it moves coins between its own addresses.")

    -- Beside the coin of the portfolio, the transaction spends a coin
    -- of 20000 of another wallet
    it "is refused when it spends coins of the wallet and coins of others: it has no single payer" $
      confirming EveryWallet 2 [] [savings [txOnChain spendSummary {summaryOtherInputs = Map.fromList [(otherAddr 1, 20000)]} 2 (Store.BlockRef 800001 1) 2 False "", received]]
        `shouldBe` Left ("The transaction " <> txid2 <> " spends coins of the wallet and coins of others: a confirmation needs one payer, whose coins the transaction spends alone")

  describe "A payment from one portfolio to another" $ do
    -- portfolio0 pays 40000 to an address of portfolio1, with 9000 of
    -- change and 1000 of fee
    let both = [savings [spent, received], spending]
        till = PortfolioHistory (PortfolioRef 1 0) "Business" "Till" [(btc, [])]

    it "is no payment from the side of every wallet, and the refusal says how to confirm it" $
      confirming EveryWallet 2 [] both
        `shouldBe` Left ("The transaction " <> txid2 <> " pays no one outside the wallet: it moves coins between its own addresses. To confirm it as a payment between portfolio0 of wallet0 and portfolio1 of wallet0, name the one that paid, or the one that was paid, with --wallet and --portfolio")

    it "is a payment sent from the side of the portfolio that paid, to the address of the other" $
      fmap (\payment -> (paymentSide payment, paymentPaid payment)) (confirming (OnePortfolio mainPortfolioRef) 2 [] both)
        `shouldBe` Right (PaymentSent, [PaidAddress "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" 40000])

    it "is a payment received from the side of the portfolio that was paid, at its address" $
      fmap (\payment -> (paymentSide payment, paymentPaid payment)) (confirming (OnePortfolio (PortfolioRef 0 1)) 2 [] both)
        `shouldBe` Right (PaymentReceived, [PaidAddress "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" 40000])

    it "is refused from the side of a portfolio that has no part in it, and the refusal names those that have" $
      confirming (OnePortfolio (PortfolioRef 1 0)) 2 [] (both <> [till])
        `shouldBe` Left ("Portfolio0 of wallet1 has no part in the transaction " <> txid2 <> ": it is a transaction of portfolio0 of wallet0 and portfolio1 of wallet0")

  describe "The value of a payment" $ do
    it "is refused the price of another network than that of the payment" $
      (paymentValued (Valuation "CHF" [(bch, Price 400 0)] "") =<< ofSpent)
        `shouldBe` Left "The payment is on btc, and --price names bch: give the price of a coin of btc alone"

    it "is refused two prices of its network" $
      (paymentValued (Valuation "CHF" [(btc, Price 87500 0), (btc, Price 90000 0)] "") =<< ofSpent)
        `shouldBe` Left "--price names btc 2 times: give one price"

    -- 0.0004 bitcoins at 87500 are 35.00
    it "is beside each amount of the report, and beside their total" $
      fmap tablesOf (reportOf (paymentValued (Valuation "CHF" [(btc, Price 87500 0)] "") =<< ofSpent))
        `shouldBe` Right [[["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "0.00040000", "35.00"], ["Total paid", "0.00040000", "35.00"]]]

    it "is stated under the amount paid, with the price it rests on" $
      fmap (factsNamed "Amount paid") (reportOf (paymentValued (Valuation "CHF" [(btc, Price 87500 0)] "the tax office") =<< ofSpent))
        `shouldBe` Right [["0.00040000 BTC", "35.00 CHF, at 87,500.00 CHF per BTC"]]

    it "comes with a note that says how it is worked out, who gave the price and where the price came from" $
      fmap (take 1 . drop 4 . notesOf) (reportOf (paymentValued (Valuation "CHF" [(btc, Price 87500 0)] "the tax office") =<< ofSpent))
        `shouldBe` Right ["Values are in CHF: an amount times the price of the asset, rounded to the nearest hundredth. The payer gave the price. Its source: the tax office."]

  describe "The block of a payment" $ do
    -- The last sync reached block 800002, and the payment is in 800001
    it "has a hash to ask the server for when the last sync did not stop at it" $
      fmap paymentBlocksUnnamed ofSpent `shouldBe` Right [(btc, 800001)]

    it "has its hash already when the last sync stopped at it" $
      fmap paymentBlocksUnnamed (paymentConfirmation EveryWallet (const cet) (fakeTxHash 2) [] [(btc, Just (SyncedTo 800001 (fakeBlockHash 2) (Just seenTime)))] [savings [spent, received]])
        `shouldBe` Right []

    it "is named in the report by its height, its date and the hash the server gave" $
      fmap (factsNamed "Block") (reportOf (namingPaymentBlock [(btc, 800001, fakeBlockHash 2)] <$> ofSpent))
        `shouldBe` Right [["Block 800001, dated 14 November 2023, 23:13", "Hash " <> hashOfBlock2]]

    it "is named without a hash when the server gave none, and the report says so" $
      fmap (factsNamed "Block") (reportOf ofSpent)
        `shouldBe` Right [["Block 800001, dated 14 November 2023, 23:13", "Hash not available: the server did not give it"]]

  describe "The report of a payment sent" $ do
    let withNames = (\payment -> payment {paymentHolder = "Jane Doe", paymentCounterparty = "ACME Ltd", paymentPurpose = "Invoice 42"}) <$> ofSpent

    it "is titled Digital Asset Payment Confirmation, with the day of the payment, and says above the title that it is about cryptocurrency" $
      fmap (\report -> (reportKind report, reportTitle report, reportSubtitle report, reportFooter report)) (reportOf ofSpent)
        `shouldBe` Right ("Confirmation of a cryptocurrency payment", "Digital Asset Payment Confirmation", "Payment of 14 November 2023", "Digital Asset Payment Confirmation \183 Payment of 14 November 2023 \183 Ref. EFC7-A06D")

    it "starts with the facts: the payer, the recipient, the purpose, the date, the amount, the asset, when and by whom it was made, and its reference" $
      fmap (take 1 . reportBlocks) (reportOf withNames)
        `shouldBe` Right
          [ FactsBlock
              [ Fact "Payer" ["Jane Doe"],
                Fact "Recipient" ["ACME Ltd"],
                Fact "Purpose" ["Invoice 42"],
                Fact "Payment date" ["14 November 2023, 23:13 (UTC+01:00)"],
                Fact "Amount paid" ["0.00040000 BTC"],
                Fact "Asset" ["Bitcoin (BTC)"],
                Fact "Prepared on" ["15 November 2026, 14:32 (UTC+01:00)"],
                Fact "Prepared by" ["The payer, from public blockchain data. Not audited."],
                Fact "Reference" ["7CA3-2763"]
              ]
          ]

    it "names nobody and no purpose when none is given" $
      fmap (\report -> [factName fact | FactsBlock facts <- take 1 (reportBlocks report), fact <- facts]) (reportOf ofSpent)
        `shouldBe` Right ["Payment date", "Amount paid", "Asset", "Prepared on", "Prepared by", "Reference"]

    it "has its sections in order: the payment, the blockchain data, the notes" $
      fmap (\report -> [title | SectionBlock title <- reportBlocks report]) (reportOf ofSpent) `shouldBe` Right ["Payment", "Blockchain data", "Notes"]

    it "lists the address paid with its amount, then the total paid" $
      fmap tablesOf (reportOf ofSpent) `shouldBe` Right [[["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "0.00040000"], ["Total paid", "0.00040000"]]]

    -- 40000 paid + 1000 of fee = 41000 left the payer
    it "states the fee the payer paid on top, and what left the payer in all" $
      fmap (\report -> (factsNamed "Network fee" report, factsNamed "Total debited" report)) (reportOf ofSpent)
        `shouldBe` Right ([["0.00001000 BTC, paid by the payer on top of the amount"]], [["0.00041000 BTC"]])

    -- The transaction pays 60000 to the address left out
    it "of one address out of two states what the other payments add up to, and no total debited" $
      fmap (\report -> (factsNamed "Other payments" report, factsNamed "Total debited" report)) (reportOf (ofBatch ["1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"]))
        `shouldBe` Right ([["0.00060000 BTC, which this confirmation leaves out"]], [])

    it "names the transaction by its id, whole" $
      fmap (factsNamed "Transaction ID") (reportOf ofSpent) `shouldBe` Right [["0000000000000000000000000000000000000000000000000000000000000002"]]

    it "states the confirmations, and the block the last sync reached, up to which they are counted" $
      fmap (factsNamed "Confirmations") (reportOf ofSpent)
        `shouldBe` Right [["2 at the last synchronisation, which reached block 800002, dated 2 January 2024, 13:00"]]

    it "says first in its notes that the payer prepared it, that nobody audited it, that no bank carried the payment out, and what its reference tells" $
      fmap (take 1 . notesOf) (reportOf ofSpent)
        `shouldBe` Right ["This confirmation was prepared by the payer, from the data of a public blockchain. No third party has audited or confirmed it. The payment was made from a self-custody wallet, whose keys the payer keeps: no bank, exchange or payment service carried it out. Two confirmations with the same reference state the same transaction, names, purpose, amounts and price."]

    it "says in a note that the names are as the payer gave them, when it names somebody" $
      fmap (take 1 . drop 1 . notesOf) (reportOf withNames)
        `shouldBe` Right ["The payment is a transaction on the Bitcoin blockchain, which is public. Anyone can look the transaction up by its ID, and check the addresses it pays, the amounts and its block, which is named by its height and its hash. The blockchain does not say who holds an address. The names on this confirmation are as the payer gave them."]

    it "says in a note that the change of the payer is not listed" $
      fmap (take 1 . drop 2 . notesOf) (reportOf ofSpent)
        `shouldBe` Right ["The network fee went to the miners of the block. The payer paid it on top of the amount paid, so the addresses listed received that amount in full. What the transaction returns to the payer as change is no payment, and is not listed."]

  describe "The report of a payment received" $ do
    it "is titled Digital Asset Payment Receipt" $
      fmap (\report -> (reportKind report, reportTitle report)) (reportOf ofReceived)
        `shouldBe` Right ("Receipt of a cryptocurrency payment", "Digital Asset Payment Receipt")

    it "names the holder as the recipient, and the other side as the payer" $
      fmap (\report -> (factsNamed "Payer" report, factsNamed "Recipient" report)) (reportOf ((\payment -> payment {paymentHolder = "Jane Doe", paymentCounterparty = "ACME Ltd"}) <$> ofReceived))
        `shouldBe` Right ([["ACME Ltd"]], [["Jane Doe"]])

    it "lists the address the payment was received at with its amount, then the total received" $
      fmap tablesOf (reportOf ofReceived) `shouldBe` Right [[["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "0.00050000"], ["Total received", "0.00050000"]]]

    it "states no fee, which the payer paid" $
      fmap (factsNamed "Network fee") (reportOf ofReceived) `shouldBe` Right []

    it "says that the recipient prepared it" $
      fmap (factsNamed "Prepared by") (reportOf ofReceived) `shouldBe` Right [["The recipient, from public blockchain data. Not audited."]]

  describe "The reference of a payment confirmation" $ do
    -- The lines are "payment" and the txid, "network btc", "side
    -- confirmation", "holder ", "counterparty ", "purpose ", "paid
    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU 40000" and "fee 1000", hashed
    -- with sha256sum
    it "is worked out from the transaction, the side, the names, the purpose, the amounts and the fee" $
      fmap paymentReference ofSpent `shouldBe` Right "EFC7-A06D"

    it "stays the same as the transaction gains confirmations" $
      fmap paymentReference (confirming EveryWallet 2 [] [savings [txOnChain spendSummary 2 (Store.BlockRef 800001 1) 500 False "", received]]) `shouldBe` Right "EFC7-A06D"

    -- The same lines, with "holder Jane Doe", "counterparty ACME Ltd"
    -- and "purpose Invoice 42"
    it "changes with the names and the purpose" $
      fmap (\payment -> paymentReference payment {paymentHolder = "Jane Doe", paymentCounterparty = "ACME Ltd", paymentPurpose = "Invoice 42"}) ofSpent `shouldBe` Right "7CA3-2763"

    -- The lines of the first test, with "scope portfolio 0 0" after the
    -- purpose
    it "states the side it is on, when that is one portfolio of several" $
      fmap paymentReference (confirming (OnePortfolio mainPortfolioRef) 2 [] [savings [spent, received], spending]) `shouldBe` Right "2BC1-78E7"

    -- The lines are "payment" and the txid of the transaction 1,
    -- "network btc", "side receipt", the three empty names, and "paid
    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj 50000". A receipt states no
    -- fee.
    it "of a receipt states no fee" $
      fmap paymentReference ofReceived `shouldBe` Right "1AAD-8C57"
