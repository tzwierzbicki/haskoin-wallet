{-# LANGUAGE OverloadedStrings #-}

-- | How hw shows what it lists and answers with: a transaction, an
-- account, an address, a coin, a sync. Each has a compact view, the one
-- the listings show by default, and some a verbose view, that of --verbose
-- and of the commands that answer with one element. The transactions are
-- those of the shared story of TestUtils, a payment of 50000 received at
-- /0/0 and then spent, and the rows are those of the account main, shown
-- at 'seenTimeInCet'.
module Haskoin.Wallet.Cli.PrettyPrinterSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Address (CoinBalance (..))
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..))
import Haskoin.Wallet.Database (DBAccount (..))
import Haskoin.Wallet.Keys (MnemonicPass (..))
import Haskoin.Wallet.Cli.PrettyPrinter
import Haskoin.Wallet.Sync (SyncRes (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo
import Test.Hspec

spec :: Spec
spec = do
  headlineSpec
  compactSpec
  verboseSpec
  answerSpec
  accountSpec
  addressSpec
  balanceSpec
  coinSpec
  syncSpec
  mnemonicSpec

mnemonicSpec :: Spec
mnemonicSpec =
  describe "A new mnemonic as hw shows it" $ do
    it "says how many words it has and where its entropy came from, then numbers its words four on a line, in columns" $
      renderedLines (mnemonicDoc "/dev/random" (Text.words (mnemonicWords testMnemonic)) [])
        `shouldBe` [ "Mnemonic of 12 words, from /dev/random",
                     "   1. snow       2. senior     3. nerve      4. virus",
                     "   5. fabric     6. now        7. fringe     8. clip",
                     "   9. marble    10. interest  11. analyst   12. can"
                   ]

    it "shows its parts only, each numbered from 1, a blank line between them, and not the whole mnemonic" $
      renderedLines (mnemonicDoc "/dev/random" (Text.words (mnemonicWords testMnemonic)) (Text.words <$> testMnemonicParts))
        `shouldBe` [ "Mnemonic of 12 words in 2 parts, from /dev/random",
                     "",
                     "Part 1",
                     "   1. stamp      2. snap       3. purity     4. pepper",
                     "   5. arrange    6. notable    7. inform     8. grass",
                     "   9. crazy     10. salad     11. thought   12. slide",
                     "",
                     "Part 2",
                     "   1. boring     2. auction    3. demand     4. filter",
                     "   5. frog       6. accuse     7. company    8. exchange",
                     "   9. rely      10. slogan    11. trim      12. typical"
                   ]

accountSpec :: Spec
accountSpec =
  describe "An account as hw shows it" $ do
    -- 5000000 satoshis spendable, 100000 unconfirmed
    it "gives its name, network and path, then what it can spend, what is unconfirmed and its coins" $
      renderedLines (compactAccountDoc UnitBitcoin mainAccountRow 5000000)
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  spendable 0.05000000 btc, unconfirmed 0.00100000 btc, 3 coins"
                   ]

    it "leaves out the unconfirmed balance when there is none, and says an empty account has no coins" $
      renderedLines (compactAccountDoc UnitBitcoin mainAccountRow {dBAccountBalanceUnconfirmed = 0, dBAccountBalanceCoins = 0} 0)
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  spendable 0.00000000 btc, no coins"
                   ]

    it "gives every detail with --verbose, the time it was created on the clock given" $
      renderedLines (verboseAccountDoc UnitBitcoin seenTimeInCet mainAccountRow 5000000)
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  Fingerprint: bc560f95",
                     "  Addresses  : 12 receiving, 3 change",
                     "  Created    : 14 Nov 2023 23:13:20 CET",
                     "  Confirmed  : 0.05000000 btc",
                     "  Unconfirmed: 0.00100000 btc",
                     "  Spendable  : 0.05000000 btc",
                     "  Coins      : 3"
                   ]

    it "says the fingerprint is unknown when hw has not learnt it" $
      (renderedLines (verboseAccountDoc UnitBitcoin seenTimeInCet mainAccountRow {dBAccountFingerprint = Nothing} 5000000) !! 1)
        `shouldBe` "  Fingerprint: unknown"

addressSpec :: Spec
addressSpec =
  describe "An address as hw shows it" $ do
    it "gives its path, the address and its label, then what it can spend and what it received" $
      renderedLines (compactAddressDoc UnitBitcoin 4 paidAddressRow 50000)
        `shouldBe` [ "/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  spendable 0.00050000 btc, received 0.00050000 btc in 1 transaction"
                   ]

    it "says an address no transaction has paid is unused" $
      renderedLines (compactAddressDoc UnitBitcoin 4 unusedAddressRow 0)
        `shouldBe` [ "/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
                     "  unused"
                   ]

    -- A path of 5 characters, such as /0/10, is listed too
    it "pads its path to the width given, so that the addresses line up" $
      (renderedLines (compactAddressDoc UnitBitcoin 5 unusedAddressRow 0) !! 0)
        `shouldBe` "/0/1   1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"

    it "gives every detail with --verbose, the time it was created on the clock given" $
      renderedLines (verboseAddressDoc UnitBitcoin 4 seenTimeInCet paidAddressRow 50000)
        `shouldBe` [ "/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  Transactions: 1",
                     "  Received    : 0.00050000 btc",
                     "  Confirmed   : 0.00050000 btc",
                     "  Unconfirmed : 0.00000000 btc",
                     "  Spendable   : 0.00050000 btc",
                     "  Coins       : 1",
                     "  Created     : 14 Nov 2023 23:13:20 CET"
                   ]

balanceSpec :: Spec
balanceSpec =
  describe "An address as balances shows it" $
    -- 80000 in coins, of which 50000 can be spent now: 30000 cannot
    it "gives its path, the address and its label, then its total, spendable and unspendable balances" $
      renderedLines (coinBalanceDoc UnitBitcoin 4 paidAddressRow (CoinBalance 80000 50000))
        `shouldBe` [ "/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc"
                   ]

coinSpec :: Spec
coinSpec =
  describe "A coin as hw shows it" $ do
    it "gives its value, its state and its confirmations, then its address, then the coin as lockcoin takes it" $
      renderedLines (coinDoc btc UnitBitcoin spendableCoinRow)
        `shouldBe` [ "    0.00050000 btc  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
                     "  0000000000000000000000000000000000000000000000000000000000000001:0"
                   ]

    it "says a coin a pending transaction spends is locked by it" $
      (renderedLines (coinDoc btc UnitBitcoin spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "    0.00050000 btc  locked by a pending transaction, 3 confirmations"

    it "says a coin locked with lockcoin is locked by you" $
      (renderedLines (coinDoc btc UnitBitcoin spendableCoinRow {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "    0.00050000 btc  locked by you, 3 confirmations"

    it "says a free coin the wallet cannot spend yet is not spendable yet, and an unconfirmed one unconfirmed" $
      (renderedLines (coinDoc btc UnitBitcoin spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "    0.00050000 btc  not spendable yet, unconfirmed"

syncSpec :: Spec
syncSpec =
  describe "The sync of an account as hw shows it" $ do
    it "gives the account as accounts does, the block it is synced to, and each kind of change on a line" $
      renderedLines (syncResultDoc UnitBitcoin quietSync {syncResTxUpdates = 2, syncResCoinUpdates = 3, syncResRebroadcast = 1, syncResConflicted = 1, syncResNewAddrs = 4})
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  spendable 0.05000000 btc, unconfirmed 0.00100000 btc, 3 coins",
                     "  synced at block 800002",
                     "  2 transactions and 3 coins updated",
                     "  1 transaction sent again",
                     "  1 pending transaction conflicted",
                     "  4 new addresses found"
                   ]

    it "says nothing new when nothing changed" $
      renderedLines (syncResultDoc UnitBitcoin quietSync)
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  spendable 0.05000000 btc, unconfirmed 0.00100000 btc, 3 coins",
                     "  synced at block 800002, nothing new"
                   ]

    it "counts the transactions updated alone when no coin was" $
      drop 2 (renderedLines (syncResultDoc UnitBitcoin quietSync {syncResTxUpdates = 1}))
        `shouldBe` [ "  synced at block 800002",
                     "  1 transaction updated"
                   ]

    it "gives an account left out with why its store failed" $
      renderedLines (notSyncedDoc (mainAccountRow, "The store at http://127.0.0.1:1/ did not answer"))
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  not synced: The store at http://127.0.0.1:1/ did not answer"
                   ]

headlineSpec :: Spec
headlineSpec =
  describe "The first line of a transaction" $ do
    -- 22:13 UTC is 23:13 CET. 50000 satoshis are 0.0005 bitcoins.
    it "shows a payment received as Incoming, with a plus sign, at the minute of the clock given" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "shows a payment sent as Outgoing, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) ""
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
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Internal     -0.00001000 btc  2 confirmations"]

    -- 1250000000 satoshis are 12.5 bitcoins
    it "lines up amounts of different widths" $ do
      let small = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) ""
          large = small {txInfoSummary = paymentSummary {summaryAmount = 1250000000}}
      concatMap (renderedLines . headlineDoc UnitBitcoin (Just seenTimeInCet)) [small, large]
        `shouldBe` [ "14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations",
                     "14 Nov 2023 23:13  Incoming    +12.50000000 btc  3 confirmations"
                   ]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 1 False)) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  1 confirmation"]

    it "gives the fee rate of a transaction not in a block yet" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False)) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  unconfirmed, 4 sat/byte"]

    it "says dead for a transaction replaced or dropped from the network, without its fee rate" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True)) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  dead"]

    it "says the time is unknown when hw does not know it, in a column of the same width" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) Nothing 3 False)) ""
      renderedLines (headlineDoc UnitBitcoin Nothing info)
        `shouldBe` ["time unknown       Incoming     +0.00050000 btc  3 confirmations"]

    it "shows an unsigned transaction in preparation as not signed, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [])) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  not signed, 4 sat/byte"]

    it "shows a signed transaction not sent yet as signed and not sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False (Just seenTime) [])) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  signed, not sent, 4 sat/byte"]

    it "shows a transaction sent that the sync has not seen yet as sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) True False (Just seenTime) [])) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  sent, 4 sat/byte"]

    it "shows a transaction in conflict as conflicted, without its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) ""
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  conflicted"]

compactSpec :: Spec
compactSpec =
  describe "The compact view of a transaction" $ do
    it "gives the address a payment pays, without its change, then the txid" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) ""
      renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    it "shows the label of a transaction, quoted, on a line of its own below its first line" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "Rent of October"
      renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  \"Rent of October\"",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    it "gives the address of the wallet a payment received is paid at, with its label" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) ""
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj \"Rent\""

    -- otherAddr 1 is 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "gives the largest output of several, and counts the others" $ do
      let several = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 10000), (otherAddr 1, 29000), (otherAddr 2, 1000)]}
          info = TxInfo several (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) ""
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ and 2 more"

    it "shows an unsigned transaction by its nosigHash" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [])) ""
      last (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info))
        `shouldBe` "  0000000000000000000000000000000000000000000000000000000000000007"

    it "shows a signed transaction by its txid" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False (Just seenTime) [])) ""
      last (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just seenTimeInCet) info))
        `shouldBe` "  0000000000000000000000000000000000000000000000000000000000000002"

    it "names the transaction a replacement replaces, before its hash" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5])) ""
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
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) ""
      renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002",
                     "  Time     : 14 Nov 2023 23:13:20 CET",
                     "  Fee      : 0.00001000 btc, 4 sat/byte",
                     "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  /1/0"
                   ]

    it "shows the label of a transaction, quoted, as its first field" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "Rent of October"
      take 2 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  Label    : \"Rent of October\"",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    it "shows the label of an address of the wallet, quoted, after its path" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) ""
      (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0  \"Rent\""

    -- The payer spends 51000 from otherAddr 0, which is nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) ""
      drop 4 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0"
                   ]

    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU sorts before
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ: the outputs come by address
    it "puts each address of a section on a line of its own, under the first" $ do
      let twoPaid = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 30000), (otherAddr 1, 10000)]}
          info = TxInfo twoPaid (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) ""
      take 2 (drop 5 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00030000 btc",
                     "             1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  0.00010000 btc"
                   ]

    it "explains why a transaction is dead" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True)) ""
      (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info) !! 3)
        `shouldBe` "  Status   : replaced, or dropped from the network"

    it "shows under the fee the change too small for an output that the fee took in" $ do
      let info = TxInfo spendSummary {summaryChangeInFee = Just 300} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) ""
      take 2 (drop 3 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/byte",
                     "             includes 0.00000300 btc of change too small for an output"
                   ]

    it "shows an unsigned transaction by its nosigHash, with the time it was created" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [])) ""
      take 2 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  NoSigHash: 0000000000000000000000000000000000000000000000000000000000000007",
                     "  Created  : 14 Nov 2023 23:13:20 CET"
                   ]

    it "explains a conflict, and how to free the coins" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) ""
      (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info) !! 3)
        `shouldBe` "  Conflict : A coin it spends was spent by another transaction, or is gone. It can no longer be sent. Delete it with deletetx."

    it "names every transaction a replacement replaces, one under the other" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5, fakeTxHash 6])) ""
      take 2 (drop 2 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just seenTimeInCet) info)))
        `shouldBe` [ "  Replaces : 0000000000000000000000000000000000000000000000000000000000000005",
                     "             0000000000000000000000000000000000000000000000000000000000000006"
                   ]

answerSpec :: Spec
answerSpec =
  describe "The transaction a command answers with" $ do
    -- A transaction read from a file has no time, so the clock of the
    -- computer plays no part
    let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) ""

    it "names its network below its first line, outside the wallet" $ do
      shown <- txAnswerDoc btc Nothing UnitBitcoin info
      (renderedLines shown !! 1) `shouldBe` "  Network  : btc"

    it "names its account, then its network, below its first line" $ do
      shown <- txAnswerDoc btc (Just mainAccountRow) UnitBitcoin info
      take 2 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Account  : main",
                     "  Network  : btc"
                   ]

    it "gives its label after its account and its network" $ do
      shown <- txAnswerDoc btc (Just mainAccountRow) UnitBitcoin info {txInfoLabel = "Rent of October"}
      take 3 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Account  : main",
                     "  Network  : btc",
                     "  Label    : \"Rent of October\""
                   ]
