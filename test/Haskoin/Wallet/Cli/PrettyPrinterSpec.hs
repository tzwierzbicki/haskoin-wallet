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
import Haskoin.Wallet.Database (DBAccount (..), DBAddress (..))
import Haskoin.Wallet.Keys (ForeignKind (..), MnemonicPass (..))
import Haskoin.Wallet.Cli.PrettyPrinter
import Haskoin.Wallet.Cli.Style (renderPlain)
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncRes (..))
import Haskoin.Wallet.Labels (AccountImport (..))
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
  descriptorSpec
  importSpec
  addressSpec
  balanceSpec
  coinSpec
  syncSpec
  mnemonicSpec
  outsideTextSpec

-- What reaches a document without passing through a label, such as an
-- error quoting a file, is written out as the document is printed
outsideTextSpec :: Spec
outsideTextSpec =
  describe "A document as hw prints it" $
    it "has an escape in any of its texts written out, never as an order to the terminal" $
      renderPlain (errorDoc "Unknown network btc\ESC[2J")
        `shouldBe` "Error: Unknown network btc\\x1b[2J"

mnemonicSpec :: Spec
mnemonicSpec = do
  describe "A new mnemonic as hw shows it" $ do
    it "says how many words it has and where its entropy came from, then numbers its words four on a line, in columns" $
      renderedLines (mnemonicDoc "/dev/random" (Text.words (mnemonicWords testMnemonic)))
        `shouldBe` [ "Mnemonic of 12 words, from /dev/random",
                     "   1. snow       2. senior     3. nerve      4. virus",
                     "   5. fabric     6. now        7. fringe     8. clip",
                     "   9. marble    10. interest  11. analyst   12. can"
                   ]

  describe "The shares of a new backup as hw shows them" $ do
    it "say how many they are, of how many words, how many rebuild the seed and where the entropy came from, then each share under its number, a blank line between them" $
      renderedLines
        ( sharesDoc
            "/dev/random"
            2
            [ Text.words "acid fawn academic acid axis auction endless award answer random spew dryer again public civil unwrap scandal pleasure extra member",
              Text.words "acid fawn academic agency damage stilt math warmth satisfy step gravity prospect elder swing flexible explain subject sugar holy sled",
              Text.words "acid fawn academic always crisis research spirit eyebrow often hunting reaction diploma envelope have response cargo warn violence smirk symbolic"
            ]
        )
        `shouldBe` [ "Backup in 3 shares of 20 words, any 2 of them rebuild the seed, from /dev/random",
                     "",
                     "Share 1",
                     "   1. acid       2. fawn       3. academic   4. acid",
                     "   5. axis       6. auction    7. endless    8. award",
                     "   9. answer    10. random    11. spew      12. dryer",
                     "  13. again     14. public    15. civil     16. unwrap",
                     "  17. scandal   18. pleasure  19. extra     20. member",
                     "",
                     "Share 2",
                     "   1. acid       2. fawn       3. academic   4. agency",
                     "   5. damage     6. stilt      7. math       8. warmth",
                     "   9. satisfy   10. step      11. gravity   12. prospect",
                     "  13. elder     14. swing     15. flexible  16. explain",
                     "  17. subject   18. sugar     19. holy      20. sled",
                     "",
                     "Share 3",
                     "   1. acid       2. fawn       3. academic   4. always",
                     "   5. crisis     6. research   7. spirit     8. eyebrow",
                     "   9. often     10. hunting   11. reaction  12. diploma",
                     "  13. envelope  14. have      15. response  16. cargo",
                     "  17. warn      18. violence  19. smirk     20. symbolic"
                   ]

    it "say of one share that it is the whole backup" $
      renderedLines (sharesDoc "/dev/random" 1 [Text.words "analysis morning academic academic center intend saver petition romp expand dryer smirk award estate sunlight deny mortgage debut dictate rocky"])
        `shouldBe` [ "Backup in 1 share of 20 words, which is the whole backup, from /dev/random",
                     "",
                     "Share 1",
                     "   1. analysis   2. morning    3. academic   4. academic",
                     "   5. center     6. intend     7. saver      8. petition",
                     "   9. romp      10. expand    11. dryer     12. smirk",
                     "  13. award     14. estate    15. sunlight  16. deny",
                     "  17. mortgage  18. debut     19. dictate   20. rocky"
                   ]

    -- 33 words: eight lines of four, and one word on the ninth
    it "number the 33 words of a share of 32 bytes on nine lines" $ do
      let shown = renderedLines (sharesDoc "/dev/random" 2 (replicate 2 (Text.words "academic agency academic acid airport idle resident emphasis nail dwarf crunch cards adjust bundle impact election activity modify tension move fitness terminal emerald stick burning quantity payment body strike guitar emphasis enjoy flexible")))
      take 1 shown `shouldBe` ["Backup in 2 shares of 33 words, any 2 of them rebuild the seed, from /dev/random"]
      take 2 (drop 10 shown) `shouldBe` ["  29. strike    30. guitar    31. emphasis  32. enjoy", "  33. flexible"]

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

    -- The address /0/3 of main, which the wallet has not handed out, so
    -- it has no label
    it "gives the address checkaddr derives under its path, then its account and network" $
      renderedLines (checkAddrDoc mainAccountRow (Deriv :/ 0 :/ 3) "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1" "")
        `shouldBe` [ "/0/3  1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1",
                     "  Account  : main",
                     "  Network  : btc"
                   ]

    it "shows the label of the address checkaddr derives when the wallet has one" $
      (renderedLines (checkAddrDoc mainAccountRow (Deriv :/ 0 :/ 3) "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1" "Savings") !! 0)
        `shouldBe` "/0/3  1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1  \"Savings\""

    -- The rows of # stand for the rows of a code
    it "goes above its QR code, which starts on the next line" $
      renderedLines (withCodeDoc (verboseAddressDoc UnitBitcoin 4 seenTimeInCet paidAddressRow 50000) ["####", "####"])
        `shouldBe` [ "/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  Transactions: 1",
                     "  Received    : 0.00050000 btc",
                     "  Confirmed   : 0.00050000 btc",
                     "  Unconfirmed : 0.00000000 btc",
                     "  Spendable   : 0.00050000 btc",
                     "  Coins       : 1",
                     "  Created     : 14 Nov 2023 23:13:20 CET",
                     "####",
                     "####"
                   ]

descriptorSpec :: Spec
descriptorSpec =
  describe "The descriptor of an account as hw shows it" $ do
    it "gives the descriptor alone on its first line, then the account and its network" $
      renderedLines (descriptorDoc mainAccountRow ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j87") Nothing)
        `shouldBe` [ "pkh([bc560f95/44h/0h/0h]" <> Text.unpack testAccountXPub <> "/<0;1>/*)#vq597j87",
                     "  Account    : main",
                     "  Network    : btc"
                   ]

    -- The descriptor then names no origin
    it "says the fingerprint of the seed is unknown when it is" $
      renderedLines (descriptorDoc mainAccountRow {dBAccountFingerprint = Nothing} ("pkh(" <> testAccountXPub <> "/<0;1>/*)#mmejv52s") Nothing)
        `shouldBe` [ "pkh(" <> Text.unpack testAccountXPub <> "/<0;1>/*)#mmejv52s",
                     "  Account    : main",
                     "  Network    : btc",
                     "  Fingerprint: unknown"
                   ]

    it "adds the key with the zpub prefix of SLIP-132 on a row of its own for a bip84 account" $
      renderedLines (descriptorDoc segwitAccountRow ("wpkh([bc560f95/84h/0h/0h]" <> testSegwitAccountXPub <> "/<0;1>/*)#5nktextl") (Just testSegwitAccountZPub))
        `shouldBe` [ "wpkh([bc560f95/84h/0h/0h]" <> Text.unpack testSegwitAccountXPub <> "/<0;1>/*)#5nktextl",
                     "  Account    : spending",
                     "  Network    : btc",
                     "  SLIP-132   : " <> Text.unpack testSegwitAccountZPub
                   ]

    it "puts that row after the note on the fingerprint, which the descriptor lacks" $
      renderedLines (descriptorDoc segwitAccountRow {dBAccountFingerprint = Nothing} ("wpkh(" <> testSegwitAccountXPub <> "/<0;1>/*)#h3820wwk") (Just testSegwitAccountZPub))
        `shouldBe` [ "wpkh(" <> Text.unpack testSegwitAccountXPub <> "/<0;1>/*)#h3820wwk",
                     "  Account    : spending",
                     "  Network    : btc",
                     "  Fingerprint: unknown",
                     "  SLIP-132   : " <> Text.unpack testSegwitAccountZPub
                   ]

importSpec :: Spec
importSpec =
  describe "An account importacc created, as the answer shows it" $ do
    it "gives the account as accounts lists it, then that it was created" $
      renderedLines (accountImportDoc (AccountCreated mainAccountRow Nothing))
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  created"
                   ]

    -- The screen wraps the note at 80 columns; the test renders unbounded
    it "puts the note on a kind taken on trust under it, indented like the rest" $
      renderedLines (accountImportDoc (AccountCreated segwitAccountRow (Just "The key says no kind. The account is bip84 because --type says so.")))
        `shouldBe` [ "spending  btc  /84'/0'/0'",
                     "  created",
                     "  The key says no kind. The account is bip84 because --type says so."
                   ]

    -- The name of a refused account is the label of its record in the file
    it "writes out a line break in the name of a refused account, which stays on its line" $
      renderedLines (accountImportDoc (AccountRefused "main\ncreated" "The key does not read as a key of btc"))
        `shouldBe` [ "main\\ncreated",
                     "  not imported: The key does not read as a key of btc"
                   ]

balanceSpec :: Spec
balanceSpec =
  describe "An address as balances shows it" $ do
    -- 80000 in coins, of which 50000 can be spent now: 30000 cannot
    it "gives its path, the address and its label, then its total, spendable and unspendable balances" $
      renderedLines (coinBalanceDoc UnitBitcoin 4 paidAddressRow (CoinBalance 80000 50000))
        `shouldBe` [ "/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc"
                   ]

    -- balances lists both kinds of addresses, where a change address takes
    -- no label. addresses lists one kind at a time, and says nothing.
    it "says a change address is one, where its label would be, which addresses does not" $ do
      let changeRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = internalAddrsText !! 0, dBAddressLabel = ""}
      (renderedLines (coinBalanceDoc UnitBitcoin 4 changeRow (CoinBalance 80000 50000)) !! 0)
        `shouldBe` "/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  change address"
      (renderedLines (compactAddressDoc UnitBitcoin 4 changeRow 50000) !! 0)
        `shouldBe` "/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"

coinSpec :: Spec
coinSpec =
  describe "A coin as hw shows it" $ do
    it "gives its value, its state and its confirmations, then its address, then the coin as lockcoin takes it" $
      renderedLines (coinDoc btc UnitBitcoin spendableCoinRow)
        `shouldBe` [ "    0.00050000 btc  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
                     "  0000000000000000000000000000000000000000000000000000000000000001:0"
                   ]

    -- The label of the transaction below the first line, as a transaction
    -- shows its own, then the label of the address after it
    it "shows the label of its transaction on a line of its own, and the label of its address after the address" $
      renderedLines (coinDoc btc UnitBitcoin spendableCoinRow {jsonCoinTxLabel = "Salary", jsonCoinAddressLabel = "Wages"})
        `shouldBe` [ "    0.00050000 btc  spendable, 3 confirmations",
                     "  \"Salary\"",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj \"Wages\"",
                     "  0000000000000000000000000000000000000000000000000000000000000001:0"
                   ]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address, which
    -- takes no label
    it "says a coin at a change address is at one, where the label of the address would be" $
      (renderedLines (coinDoc btc UnitBitcoin spendableCoinRow {jsonCoinAddress = internalAddr 0, jsonCoinChange = True}) !! 1)
        `shouldBe` "  at 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn change address"

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

    it "lists the funds at paths hw cannot spend from: each account with its kind, its used addresses and what is left, then what to do" $
      renderedLines (foreignFundsDoc UnitBitcoin btc [ForeignFunds Bip49 0 3 70000, ForeignFunds Bip86 0 1 0])
        `shouldBe` [ "The seed was paid at paths hw cannot spend from:",
                     "  /49'/0'/0'  bip49, wrapped SegWit: 3 addresses used, 0.00070000 btc left",
                     "  /86'/0'/0'  bip86, Taproot: 1 address used, nothing left",
                     "  A wallet that holds such accounts can move the funds to an address of this wallet."
                   ]

    it "aligns the kinds when the paths differ in length" $
      take 2 (drop 1 (renderedLines (foreignFundsDoc UnitBitcoin btc [ForeignFunds Bip49 0 1 1000, ForeignFunds Bip49 10 1 1000])))
        `shouldBe` [ "  /49'/0'/0'   bip49, wrapped SegWit: 1 address used, 0.00001000 btc left",
                     "  /49'/0'/10'  bip49, wrapped SegWit: 1 address used, 0.00001000 btc left"
                   ]

    it "names the bip44 accounts of Bitcoin on Bitcoin Cash by the coin type of their path" $
      renderedLines (foreignFundsDoc UnitBitcoin bch [ForeignFunds Bip44OfBitcoin 0 2 50000]) !! 1
        `shouldBe` "  /44'/0'/0'  bip44 with the coin type of Bitcoin: 2 addresses used, 0.00050000 btc left"

headlineSpec :: Spec
headlineSpec =
  describe "The first line of a transaction" $ do
    -- 22:13 UTC is 23:13 CET. 50000 satoshis are 0.0005 bitcoins.
    it "shows a payment received as Incoming, with a plus sign, at the minute of the clock given" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "shows a payment sent as Outgoing, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
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
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Internal     -0.00001000 btc  2 confirmations"]

    -- 1250000000 satoshis are 12.5 bitcoins
    it "lines up amounts of different widths" $ do
      let small = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
          large = small {txInfoSummary = paymentSummary {summaryAmount = 1250000000}}
      concatMap (renderedLines . headlineDoc UnitBitcoin (Just seenTimeInCet)) [small, large]
        `shouldBe` [ "14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations",
                     "14 Nov 2023 23:13  Incoming    +12.50000000 btc  3 confirmations"
                   ]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 1 False)) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  1 confirmation"]

    it "gives the fee rate of a transaction not in a block yet" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False)) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  unconfirmed, 4 sat/vB"]

    it "says dead for a transaction replaced or dropped from the network, without its fee rate" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True)) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  dead"]

    it "says the time is unknown when hw does not know it, in a column of the same width" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) Nothing 3 False)) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin Nothing info)
        `shouldBe` ["time unknown       Incoming     +0.00050000 btc  3 confirmations"]

    it "shows an unsigned transaction in preparation as not signed, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [])) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  not signed, 4 sat/vB"]

    it "shows a signed transaction not sent yet as signed and not sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False (Just seenTime) [])) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  signed, not sent, 4 sat/vB"]

    it "shows a transaction sent that the sync has not seen yet as sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) True False (Just seenTime) [])) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  sent, 4 sat/vB"]

    it "shows a transaction in conflict as conflicted, without its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) "" Map.empty
      renderedLines (headlineDoc UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  conflicted"]

compactSpec :: Spec
compactSpec = do
  describe "The balance after a transaction" $ do
    let spent = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))

    it "takes the second line of the compact view, its number under the amount" $
      renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty))
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "                   Balance       0.01208000 btc",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    it "comes before the label in the compact view" $
      take 3 (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) (spent "Rent of October" Map.empty)))
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "                   Balance       0.01208000 btc",
                     "  \"Rent of October\""
                   ]

    it "shows an account left with nothing as zero" $
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just 0) (Just seenTimeInCet) (spent "" Map.empty)) !! 1)
        `shouldBe` "                   Balance       0.00000000 btc"

    it "keeps its number under the amount in satoshis, and nothing pads its unit" $
      take 2 (renderedLines (compactTxDoc btc UnitSatoshi Nothing (Just 1) (Just seenTimeInCet) (spent "" Map.empty)))
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing          -41000 sats  2 confirmations",
                     "                   Balance                1 sat"
                   ]

    it "follows the fee in the verbose view" $
      take 2 (drop 3 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty))))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  Balance  : 0.01208000 btc"
                   ]

  describe "The compact view of a transaction" $ do
    it "gives the address a payment pays, without its change, then the txid" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    it "shows the label of a transaction, quoted, on a line of its own below its first line" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "Rent of October" Map.empty
      renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  \"Rent of October\"",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    it "gives the address of the wallet a payment received is paid at, with its label" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj \"Rent\""

    -- otherAddr 1 is 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "gives the largest output of several, and counts the others" $ do
      let several = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 10000), (otherAddr 1, 29000), (otherAddr 2, 1000)]}
          info = TxInfo several (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ and 2 more"

    it "shows an unsigned transaction by its nosigHash" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [])) "" Map.empty
      last (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info))
        `shouldBe` "  0000000000000000000000000000000000000000000000000000000000000007"

    it "shows a signed transaction by its txid" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False (Just seenTime) [])) "" Map.empty
      last (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info))
        `shouldBe` "  0000000000000000000000000000000000000000000000000000000000000002"

    it "names the transaction a replacement replaces, before its hash" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5])) "" Map.empty
      drop 2 (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  replaces 0000000000000000000000000000000000000000000000000000000000000005",
                     "  0000000000000000000000000000000000000000000000000000000000000007"
                   ]

verboseSpec :: Spec
verboseSpec =
  describe "The verbose view of a transaction" $ do
    -- The input is the coin of 50000 at /0/0. 40000 go out and 9000 come
    -- back as change to /1/0.
    it "lists the address a payment spends from, the one it pays, and its change, in columns" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002",
                     "  Time     : 14 Nov 2023 23:13:20 CET",
                     "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  /1/0"
                   ]

    it "shows the label of a transaction, quoted, as its first field" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "Rent of October" Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Label    : \"Rent of October\"",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    it "shows the label of an address of the wallet, quoted, after its path" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0  \"Rent\""

    -- The coin spent is of transaction 1, the payment labelled Salary
    it "ends an address spent from with the labels of its coins, after the word from" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" (Map.fromList [(fakeTxHash 1, "Salary")])
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0  \"Rent\"  from \"Salary\""

    -- The spending from the first change address, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn
    it "marks a change address spent from as one, where its label would be" $ do
      let fromChange = spendSummary {summaryMyInputs = Map.fromList [(internalAddr 0, MyInputs 50000 (Deriv :/ 1 :/ 0) "" [OutPoint (fakeTxHash 1) 0])]}
          info = TxInfo fromChange (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  From     : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00050000 btc  /1/0  change address"

    -- A transfer of the account to its first change address, which the
    -- compact view names as the address paid
    it "marks a change address an internal transaction pays as one" $ do
      let toChange = spendSummary {summaryType = TxInternal, summaryOtherOutputs = Map.empty}
          info = TxInfo toChange (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn change address"

    it "ends an address spent from without a label of its own with the labels of its coins" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" (Map.fromList [(fakeTxHash 1, "Salary")])
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0  from \"Salary\""

    -- The payer spends 51000 from otherAddr 0, which is nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      drop 4 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0"
                   ]

    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU sorts before
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ: the outputs come by address
    it "puts each address of a section on a line of its own, under the first" $ do
      let twoPaid = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 30000), (otherAddr 1, 10000)]}
          info = TxInfo twoPaid (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00030000 btc",
                     "             1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  0.00010000 btc"
                   ]

    it "explains why a transaction is dead" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True)) "" Map.empty
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 3)
        `shouldBe` "  Status   : replaced, or dropped from the network"

    it "shows under the fee the change too small for an output that the fee took in" $ do
      let info = TxInfo spendSummary {summaryChangeInFee = Just 300} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 3 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "             includes 0.00000300 btc of change too small for an output"
                   ]

    it "shows the lock time under the fee, as the block the transaction waits for, and no field without one" $ do
      let locked = TxInfo spendSummary {summaryLockTime = 814037} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
          unlocked = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 3 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) locked)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  Lock time: block 814037"
                   ]
      length (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) unlocked)) `shouldBe` 7

    -- 500,000,000 and above, a lock time is a Unix time; hw writes none
    it "shows a lock time of 500,000,000 or more as a Unix time" $ do
      let locked = TxInfo spendSummary {summaryLockTime = 1700000000} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) locked) !! 4
        `shouldBe` "  Lock time: Unix time 1700000000"

    it "shows an unsigned transaction by its nosigHash, with the time it was created" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [])) "" Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  NoSigHash: 0000000000000000000000000000000000000000000000000000000000000007",
                     "  Created  : 14 Nov 2023 23:13:20 CET"
                   ]

    it "explains a conflict, and how to free the coins" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) "" Map.empty
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 3)
        `shouldBe` "  Conflict : A coin it spends was spent by another transaction, or is gone. It can no longer be sent. Delete it with deletetx."

    it "names every transaction a replacement replaces, one under the other" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5, fakeTxHash 6])) "" Map.empty
      take 2 (drop 2 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Replaces : 0000000000000000000000000000000000000000000000000000000000000005",
                     "             0000000000000000000000000000000000000000000000000000000000000006"
                   ]

answerSpec :: Spec
answerSpec =
  describe "The transaction a command answers with" $ do
    -- A transaction read from a file has no time, so the clock of the
    -- computer plays no part
    let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty

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

    -- A label from a file that tries to write a recipient line of its own
    it "writes out a line break in its label, which stays on the line of the label" $ do
      shown <- txAnswerDoc btc (Just mainAccountRow) UnitBitcoin info {txInfoLabel = "Rent\n  To       : 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"}
      take 2 (drop 3 (renderedLines shown))
        `shouldBe` [ "  Label    : \"Rent\\n  To       : 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ\"",
                     "  NoSigHash: 0000000000000000000000000000000000000000000000000000000000000007"
                   ]
