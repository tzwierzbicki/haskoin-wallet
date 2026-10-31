{-# LANGUAGE OverloadedStrings #-}

-- | How hw shows a transaction: its first line, the compact view that txs
-- and pendingtxs list by default, and the verbose view of --verbose and of
-- the commands that answer with one transaction. The transactions are
-- those of the shared story of TestUtils, a payment of 50000 received at
-- /0/0 and then spent, shown at 'seenTimeInCet'.
module Haskoin.Wallet.PrettyPrinterSpec (spec) where

import qualified Data.Map.Strict as Map
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.PrettyPrinter (compactTxDoc, headlineDoc, txAnswerDoc, verboseTxDoc)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Test.Hspec

spec :: Spec
spec = do
  headlineSpec
  compactSpec
  verboseSpec
  answerSpec

headlineSpec :: Spec
headlineSpec =
  describe "The first line of a transaction" $ do
    -- 22:13 UTC is 23:13 CET. 50000 satoshis are 0.0005 bitcoins.
    it "shows a payment received as Incoming, with a plus sign, at the minute of the clock given" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "shows a payment sent as Outgoing, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations"]

    -- /0/0 pays 40000 to /0/1 and 9000 of change to /1/0: the account only
    -- loses the fee of 1000
    it "shows a transaction between addresses of the wallet as Internal, costing its fee" $ do
      let internal =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 40000 (Deriv :/ 0 :/ 1) ""), (internalAddr 0, MyOutputs 9000 (Deriv :/ 1 :/ 0) "")]
              }
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Internal     -0.00001000 btc  2 confirmations"]

    -- 1250000000 satoshis are 12.5 bitcoins
    it "lines up amounts of different widths" $ do
      let small = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False))
          large = small {txInfoSummary = paymentSummary {summaryAmount = 1250000000}}
      concatMap (renderedLines . headlineDoc UnitBitcoin (Just seenTimeInCet)) [small, large]
        `shouldBe` [ "14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations",
                     "14 Nov 2023 23:13  Incoming    +12.50000000 btc  3 confirmations"
                   ]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 1 False))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  1 confirmation"]

    it "gives the fee rate of a transaction not in a block yet" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  unconfirmed, 4 sat/byte"]

    it "says dead for a transaction replaced or dropped from the network, without its fee rate" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  dead"]

    it "says the time is unknown when hw does not know it, in a column of the same width" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) Nothing 3 False))
      renderedLines (headlineDoc UnitBitcoin Nothing info)
        `shouldBe` ["time unknown       Incoming     +0.00050000 btc  3 confirmations"]

    it "shows an unsigned transaction in preparation as not signed, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) []))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  not signed, 4 sat/byte"]

    it "shows a signed transaction not sent yet as signed and not sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False (Just seenTime) []))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  signed, not sent, 4 sat/byte"]

    it "shows a transaction sent that the sync has not seen yet as sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) True False (Just seenTime) []))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  sent, 4 sat/byte"]

    it "shows a transaction in conflict as conflicted, without its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) []))
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  conflicted"]

compactSpec :: Spec
compactSpec =
  describe "The compact view of a transaction" $ do
    it "gives the address a payment pays, without its change, then the txid" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
      renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    it "gives the address of the wallet a payment received is paid at, with its label" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False))
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj \"Rent\""

    -- otherAddr 1 is 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "gives the largest output of several, and counts the others" $ do
      let several = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 10000), (otherAddr 1, 29000), (otherAddr 2, 1000)]}
          info = TxInfo several (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ and 2 more"

    it "shows an unsigned transaction by its nosigHash" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) []))
      last (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info))
        `shouldBe` "  0000000000000000000000000000000000000000000000000000000000000007"

    it "shows a signed transaction by its txid" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False (Just seenTime) []))
      last (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info))
        `shouldBe` "  0000000000000000000000000000000000000000000000000000000000000002"

    it "names the transaction a replacement replaces, before its hash" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5]))
      drop 2 (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  replaces 0000000000000000000000000000000000000000000000000000000000000005",
                     "  0000000000000000000000000000000000000000000000000000000000000007"
                   ]

verboseSpec :: Spec
verboseSpec =
  describe "The verbose view of a transaction" $ do
    -- The input is the coin of 50000 at /0/0. 40000 go out and 9000 come
    -- back as change to /1/0.
    it "lists the address a payment spends from, the one it pays, and its change, in columns" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
      renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002",
                     "  Time     : 14 Nov 2023 23:13:20 CET",
                     "  Fee      : 0.00001000 btc, 4 sat/byte",
                     "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  /1/0"
                   ]

    it "shows the label of an address of the wallet, quoted, after its path" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
      (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0  \"Rent\""

    -- The payer spends 51000 from otherAddr 0, which is nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False))
      drop 4 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0"
                   ]

    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU sorts before
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ: the outputs come by address
    it "puts each address of a section on a line of its own, under the first" $ do
      let twoPaid = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 30000), (otherAddr 1, 10000)]}
          info = TxInfo twoPaid (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
      take 2 (drop 5 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00030000 btc",
                     "             1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  0.00010000 btc"
                   ]

    it "explains why a transaction is dead" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True))
      (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info) !! 3)
        `shouldBe` "  Status   : replaced, or dropped from the network"

    it "shows under the fee the change too small for an output that the fee took in" $ do
      let info = TxInfo spendSummary {summaryChangeInFee = Just 300} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
      take 2 (drop 3 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/byte",
                     "             includes 0.00000300 btc of change too small for an output"
                   ]

    it "shows an unsigned transaction by its nosigHash, with the time it was created" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) []))
      take 2 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  NoSigHash: 0000000000000000000000000000000000000000000000000000000000000007",
                     "  Created  : 14 Nov 2023 23:13:20 CET"
                   ]

    it "explains a conflict, and how to free the coins" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) []))
      (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info) !! 3)
        `shouldBe` "  Conflict : A coin it spends was spent by another transaction, or is gone. It can no longer be sent. Delete it with deletetx."

    it "names every transaction a replacement replaces, one under the other" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5, fakeTxHash 6]))
      take 2 (drop 2 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  Replaces : 0000000000000000000000000000000000000000000000000000000000000005",
                     "             0000000000000000000000000000000000000000000000000000000000000006"
                   ]

answerSpec :: Spec
answerSpec =
  describe "The transaction a command answers with" $
    -- A transaction read from a file has no time, so the clock of the
    -- computer plays no part
    it "names its network below its first line" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing []))
      shown <- txAnswerDoc btc UnitBitcoin info
      (renderedLines shown !! 1) `shouldBe` "  Network  : btc"
