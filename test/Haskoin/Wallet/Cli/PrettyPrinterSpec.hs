{-# LANGUAGE OverloadedStrings #-}

-- | How hw shows what it lists and answers with: a transaction, an
-- account, an address, a coin, a sync. Each has a compact view, the one
-- the listings show by default, and some a verbose view, that of --verbose
-- and of the commands that answer with one element. Every listing also
-- shows in a table, that of --tabular. The transactions are
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
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.Cli.PrettyPrinter
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Cli.Style (renderPlain)
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncRes (..))
import Haskoin.Wallet.Bip329 (Record (..), RecordType (..), record)
import Haskoin.Wallet.Labels (AccountImport (..), FileShown (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo
import Test.Hspec

spec :: Spec
spec = do
  headlineSpec
  compactSpec
  tableSpec
  verboseSpec
  answerSpec
  accountSpec
  importSpec
  accountFileSpec
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

    -- The checksums of the descriptors here were computed apart from hw,
    -- and Bitcoin Core 32.1 gives the same (test/data/descriptors). A bip44
    -- account has no row of SLIP-132, since its xpub marks its kind.
    it "gives every detail with --verbose, the time it was created on the clock given, and ends with its descriptor" $
      renderedLines (verboseAccountDoc UnitBitcoin seenTimeInCet mainAccountRow 5000000)
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  Fingerprint: bc560f95",
                     "  Addresses  : 12 receiving, 3 change",
                     "  Created    : 14 Nov 2023 23:13:20 CET",
                     "  Confirmed  : 0.05000000 btc",
                     "  Unconfirmed: 0.00100000 btc",
                     "  Spendable  : 0.05000000 btc",
                     "  Coins      : 3",
                     "  Descriptor : pkh([bc560f95/44h/0h/0h]" <> Text.unpack testAccountXPub <> "/<0;1>/*)#vq597j87"
                   ]

    it "says the fingerprint is unknown when hw has not learnt it" $
      (renderedLines (verboseAccountDoc UnitBitcoin seenTimeInCet mainAccountRow {dBAccountFingerprint = Nothing} 5000000) !! 1)
        `shouldBe` "  Fingerprint: unknown"

    it "gives the descriptor without the origin of its key when the fingerprint is unknown" $
      last (renderedLines (verboseAccountDoc UnitBitcoin seenTimeInCet mainAccountRow {dBAccountFingerprint = Nothing} 5000000))
        `shouldBe` "  Descriptor : pkh(" <> Text.unpack testAccountXPub <> "/<0;1>/*)#mmejv52s"

    -- As the Coldcard file of test/data/descriptors writes the descriptor
    it "ends a bip84 account with its descriptor in wpkh, then its key as a zpub on a row of its own" $
      drop 8 (renderedLines (verboseAccountDoc UnitBitcoin seenTimeInCet segwitAccountRow 0))
        `shouldBe` [ "  Descriptor : wpkh([bc560f95/84h/0h/0h]" <> Text.unpack testSegwitAccountXPub <> "/<0;1>/*)#5nktextl",
                     "  SLIP-132   : " <> Text.unpack testSegwitAccountZPub
                   ]

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
    -- it has no label and no balance. The note is on one line, since the
    -- test lays it out with no width.
    it "gives the address show derives at a path the wallet does not hold cut under its path, then its account and network, then what it is for" $
      renderedLines (derivedAddrDoc mainAccountRow (Deriv :/ 0 :/ 3) "1yQZuJjA6w7h.........Cv22rKCas7F1")
        `shouldBe` [ "/0/3  1yQZuJjA6w7h.........Cv22rKCas7F1",
                     "  Account  : main",
                     "  Network  : btc",
                     "",
                     "Compare both ends with the address that receive gave out at /0/3 on the online computer. If they match, that address is of this account, and you can give it out. If they differ, the online computer may have been tampered with, and you should not give the address out. Nobody can make another address with the same ends. This screen is for checking only, so the middle of the address is hidden. Give out the address as receive showed it."
                   ]

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

-- | The id of a transaction, as a record of a file writes it
fileTxid :: Text.Text
fileTxid = "0000000000000000000000000000000000000000000000000000000000000001"

-- | A file of accounts as show shows it, of the one account given, with
-- what importacc would do with it and its records, and nothing else
fileOf :: AccountImport -> [Record] -> FileShown
fileOf outcome records = FileShown [(outcome, records)] [] 0 0 []

accountFileSpec :: Spec
accountFileSpec =
  describe "A file of accounts as show shows it" $ do
    it "gives an account as accounts lists it, what importacc would do with it, then its labels under a heading for each kind" $
      renderedLines
        ( accountFileDoc $
            fileOf
              (AccountKept mainAccountRow Nothing)
              [ (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Wages", recordKeypath = Just "/0/0"},
                (record TxRecord fileTxid) {recordLabel = Just "Salary"},
                (record OutputRecord (fileTxid <> ":0")) {recordSpendable = Just False}
              ]
        )
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  in the wallet",
                     "  Address labels",
                     "    /0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages\"",
                     "  Transaction labels",
                     "    0000000000000000000000000000000000000000000000000000000000000001  \"Salary\"",
                     "  Coins",
                     "    0000000000000000000000000000000000000000000000000000000000000001:0  locked"
                   ]

    it "says of an account the wallet does not hold that importacc adds it" $
      renderedLines (accountFileDoc (fileOf (AccountCreated mainAccountRow Nothing) []))
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  not in the wallet, and importacc adds it"
                   ]

    it "names an account the wallet holds under another name, which importacc renames" $
      renderedLines (accountFileDoc (fileOf (AccountRenamed "btc0" mainAccountRow) []))
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  in the wallet as btc0, and importacc renames it"
                   ]

    -- Sparrow names the key of an account after its signer
    it "gives why importacc would refuse an account, under the name the file gives it" $
      renderedLines (accountFileDoc (fileOf (AccountRefused "BIP39" "Give the --network, or a default-network in the configuration file") []))
        `shouldBe` [ "BIP39",
                     "  importacc refuses it: Give the --network, or a default-network in the configuration file"
                   ]

    it "says a coin the file unlocks is unlocked" $
      renderedLines (accountFileDoc (fileOf (AccountKept mainAccountRow Nothing) [(record OutputRecord (fileTxid <> ":1")) {recordSpendable = Just True}]))
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  in the wallet",
                     "  Coins",
                     "    0000000000000000000000000000000000000000000000000000000000000001:1  unlocked"
                   ]

    -- /0/10 is the longest path, five characters
    it "pads the key paths of the addresses to the longest, and gives an address without one after the padding" $
      renderedLines
        ( accountFileDoc $
            fileOf
              (AccountKept mainAccountRow Nothing)
              [ (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Wages", recordKeypath = Just "/0/0"},
                (record AddrRecord "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg") {recordLabel = Just "From Bob", recordKeypath = Just "/0/10"},
                (record AddrRecord "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1") {recordLabel = Just "Rent"}
              ]
        )
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  in the wallet",
                     "  Address labels",
                     "    /0/0   1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages\"",
                     "    /0/10  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  \"From Bob\"",
                     "           1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1  \"Rent\""
                   ]

    it "gives the labels of no account of the file last, then what hw keeps no label for and the lines that are not records" $
      renderedLines (accountFileDoc (FileShown [] [(record TxRecord fileTxid) {recordLabel = Just "Salary"}] 1 2 [4, 7]))
        `shouldBe` [ "Labels without an account in the file",
                     "  Transaction labels",
                     "    0000000000000000000000000000000000000000000000000000000000000001  \"Salary\"",
                     "",
                     "Left aside: 1 label of change addresses, 2 records hw keeps no label for",
                     "Lines that are not records: 4, 7"
                   ]

    -- What a file holds may be anything, an escape to clear the screen too
    it "writes out an escape in a reference of the file, never as an order to the terminal" $
      renderedLines (accountFileDoc (fileOf (AccountKept mainAccountRow Nothing) [(record AddrRecord "1KEn\ESC[2J") {recordLabel = Just "Wages"}]))
        `shouldBe` [ "main  btc  /44'/0'/0'",
                     "  in the wallet",
                     "  Address labels",
                     "    1KEn\\x1b[2J  \"Wages\""
                   ]

balanceSpec :: Spec
balanceSpec = do
  describe "An address as balances shows it" $ do
    -- 80000 in coins, of which 50000 can be spent now: 30000 cannot, which
    -- a pending transaction spends
    it "gives its path, the address and its label, then its total, spendable and unspendable balances" $
      renderedLines (coinBalanceDoc UnitBitcoin 4 paidAddressRow (CoinBalance 80000 50000 30000 0))
        `shouldBe` [ "/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc"
                   ]

    -- balances lists both kinds of addresses, where a change address takes
    -- no label. addresses lists one kind at a time, and says nothing.
    it "says a change address is one, where its label would be, which addresses does not" $ do
      let changeRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = internalAddrsText !! 0, dBAddressLabel = ""}
      (renderedLines (coinBalanceDoc UnitBitcoin 4 changeRow (CoinBalance 80000 50000 30000 0)) !! 0)
        `shouldBe` "/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  change address"
      (renderedLines (compactAddressDoc UnitBitcoin 4 changeRow 50000) !! 0)
        `shouldBe` "/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"

  -- Four coins hold 80000: 50000 can be spent, 20000 a pending
  -- transaction spends, 6000 the user locked, and the rest is unconfirmed:
  -- 80000 - 50000 - 20000 - 6000 = 4000
  describe "An address as balances --verbose shows it" $ do
    let fourCoins = paidAddressRow {dBAddressBalanceCoins = 4}

    it "gives its first line, then its total, its coins, its spendable amount, and its unspendable amount in three parts" $
      renderedLines (verboseBalanceDoc UnitBitcoin 4 fourCoins (CoinBalance 80000 50000 20000 6000))
        `shouldBe` [ "/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  Total            : 0.00080000 btc",
                     "  Coins            : 4",
                     "  Spendable        : 0.00050000 btc",
                     "  Locked by tx     : 0.00020000 btc",
                     "  Locked by you    : 0.00006000 btc",
                     "  Not spendable yet: 0.00004000 btc"
                   ]

    it "gives the parts of the unspendable amount even when they hold nothing" $
      drop 4 (renderedLines (verboseBalanceDoc UnitBitcoin 4 paidAddressRow (CoinBalance 50000 50000 0 0)))
        `shouldBe` [ "  Locked by tx     : 0.00000000 btc",
                     "  Locked by you    : 0.00000000 btc",
                     "  Not spendable yet: 0.00000000 btc"
                   ]

-- The coin of 'spendableCoinRow' at the first change address,
-- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, which takes no label
atChange :: JsonCoin
atChange = spendableCoinRow {jsonCoinAddress = internalAddr 0, jsonCoinChange = True, jsonCoinPath = Just (Deriv :/ 1 :/ 0)}

coinSpec :: Spec
coinSpec = do
  describe "A coin as hw shows it" $ do
    -- 22:13 UTC is 23:13 CET. spendableCoinRow is output 0 of transaction
    -- 1, whose txid starts with 00000000.
    it "gives the short id of the coin first, the time of its transaction, its value, its state and its confirmations, then its address in full with its path" $
      renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow)
        `shouldBe` [ "00000000:0  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  /0/0"
                   ]

    it "gives the short id of the coin as the short id of its txid, a colon and its index" $
      take 1 (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1}))
        `shouldBe` ["a1075db5:1  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations"]

    -- "a1075db5:12" takes 11 characters, one more than its column
    it "lets the short id of a coin past the tenth output of its transaction push the rest of its line" $
      take 1 (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 12}))
        `shouldBe` ["a1075db5:12  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations"]

    -- The one view that keeps the label of an address in full on its line
    it "gives the label of its address on the line of the address, after its path" $
      drop 1 (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinAddressLabel = "Wages"}))
        `shouldBe` ["  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  /0/0  \"Wages\""]

    it "gives a long label of its address whole, at the end of the line, without wrapping it" $
      drop 1 (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"}))
        `shouldBe` ["  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  /0/0  \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "leaves out the label of the transaction that paid it, which the verbose view gives" $
      renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinTxLabel = "Salary"})
        `shouldBe` [ "00000000:0  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  /0/0"
                   ]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address, which
    -- takes no label
    it "says a coin at a change address is at one, on the line of the address" $
      drop 1 (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) atChange))
        `shouldBe` ["  at 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  /1/0  change address"]

    it "says a coin a pending transaction spends is locked by tx, as the table does" $
      (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  locked by tx, 3 confirmations"

    it "says a coin locked with lockcoin is locked by you" $
      (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  locked by you, 3 confirmations"

    it "says 999+ confirmations for a coin of more than 999" $
      (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinConfirmations = 1000}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  spendable, 999+ confirmations"

    it "says a free coin the wallet cannot spend yet is not spendable yet, and an unconfirmed one unconfirmed" $
      (renderedLines (compactCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  not spendable yet, unconfirmed"

    it "says the time is unknown when hw does not know it, in a column of the same width" $
      (renderedLines (compactCoinDoc btc UnitBitcoin Nothing spendableCoinRow) !! 0)
        `shouldBe` "00000000:0  time unknown           0.00050000 btc  spendable, 3 confirmations"

  -- The fields are padded to "Short id", the longest of their names
  describe "A coin with every detail, as coins --verbose shows it" $ do
    -- spendableCoinRow is output 0 of transaction 1, in block 800000
    it "gives its first line, then the coin in full and by its short id, the block that holds it, the time of its transaction to the second and its address in full with its path as fields" $
      renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow)
        `shouldBe` [ "14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations",
                     "  Coin    : 0000000000000000000000000000000000000000000000000000000000000001:0",
                     "  Short id: 00000000:0",
                     "  Block   : 800000",
                     "  Time    : 14 Nov 2023 23:13:20 CET",
                     "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  /0/0"
                   ]

    it "gives the coin in full, then its short id, the short id of its txid and its index" $
      take 2 (drop 1 (renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1})))
        `shouldBe` [ "  Coin    : a1075db5c0ffee00000000000000000000000000000000000000000000000001:1",
                     "  Short id: a1075db5:1"
                   ]

    it "says in full words that a pending transaction locks the coin" $
      (renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "14 Nov 2023 23:13      0.00050000 btc  locked by a pending transaction, 3 confirmations"

    it "gives the full count of the confirmations, past 999 too" $
      (renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinConfirmations = 1234}) !! 0)
        `shouldBe` "14 Nov 2023 23:13      0.00050000 btc  spendable, 1234 confirmations"

    it "says 1 confirmation in the singular" $
      (renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinConfirmations = 1}) !! 0)
        `shouldBe` "14 Nov 2023 23:13      0.00050000 btc  spendable, 1 confirmation"

    it "gives the label of its transaction, quoted, as its first field" $
      (renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinTxLabel = "Salary"}) !! 1)
        `shouldBe` "  Label   : \"Salary\""

    -- "  Address : " takes 12 characters
    it "gives the label of its address on the line below, under the address" $
      drop 5 (renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinAddressLabel = "Wages"}))
        `shouldBe` [ "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  /0/0",
                     "            \"Wages\""
                   ]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address
    it "says a coin at a change address is at one, on the line below the address" $
      drop 5 (renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) atChange))
        `shouldBe` [ "  Address : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  /1/0",
                     "            change address"
                   ]

    it "gives no block for an unconfirmed coin, which is in none yet" $
      renderedLines (verboseCoinDoc btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False})
        `shouldBe` [ "14 Nov 2023 23:13      0.00050000 btc  not spendable yet, unconfirmed",
                     "  Coin    : 0000000000000000000000000000000000000000000000000000000000000001:0",
                     "  Short id: 00000000:0",
                     "  Time    : 14 Nov 2023 23:13:20 CET",
                     "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  /0/0"
                   ]

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

    it "gives a network recover left out with why its store failed" $
      renderedLines (notSearchedDoc [(bch, "The store at http://127.0.0.1:1/ did not answer")])
        `shouldBe` ["bch  not searched: The store at http://127.0.0.1:1/ did not answer"]

    it "lists the funds at paths hw cannot spend from: each account with its network, its path, its kind, its used addresses and what is left, then what to do" $
      renderedLines (foreignFundsDoc UnitBitcoin [(btc, ForeignFunds Bip49 0 3 70000), (btc, ForeignFunds Bip86 0 1 0)])
        `shouldBe` [ "The seed was paid at paths hw cannot spend from:",
                     "  btc  /49'/0'/0'  bip49, wrapped SegWit: 3 addresses used, 0.00070000 btc left",
                     "  btc  /86'/0'/0'  bip86, Taproot: 1 address used, nothing left",
                     "  A wallet that holds such accounts can move the funds to an address of this wallet."
                   ]

    it "aligns the kinds when the paths differ in length" $
      take 2 (drop 1 (renderedLines (foreignFundsDoc UnitBitcoin [(btc, ForeignFunds Bip49 0 1 1000), (btc, ForeignFunds Bip49 10 1 1000)])))
        `shouldBe` [ "  btc  /49'/0'/0'   bip49, wrapped SegWit: 1 address used, 0.00001000 btc left",
                     "  btc  /49'/0'/10'  bip49, wrapped SegWit: 1 address used, 0.00001000 btc left"
                   ]

    -- recover searches btc and bch, whose names are as long, but the
    -- column lines up names of any length
    it "aligns the paths when the names of the networks differ in length" $
      take 2 (drop 1 (renderedLines (foreignFundsDoc UnitBitcoin [(btcTest4, ForeignFunds Bip49 0 1 1000), (btc, ForeignFunds Bip49 0 1 1000)])))
        `shouldBe` [ "  btctest4  /49'/1'/0'  bip49, wrapped SegWit: 1 address used, 0.00001000 btc left",
                     "  btc       /49'/0'/0'  bip49, wrapped SegWit: 1 address used, 0.00001000 btc left"
                   ]

    it "names the bip44 accounts of Bitcoin on Bitcoin Cash by the coin type of their path" $
      renderedLines (foreignFundsDoc UnitBitcoin [(bch, ForeignFunds Bip44OfBitcoin 0 2 50000)]) !! 1
        `shouldBe` "  bch  /44'/0'/0'  bip44 with the coin type of Bitcoin: 2 addresses used, 0.00050000 btc left"

headlineSpec :: Spec
headlineSpec = do
  -- The first line of the verbose view starts with the time, and names
  -- the type of the transaction
  describe "The first line of the verbose view of a transaction" $ do
    -- 22:13 UTC is 23:13 CET. 50000 satoshis are 0.0005 bitcoins.
    it "shows a payment received as Incoming, with a plus sign, at the minute of the clock given" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      renderedLines (headlineDoc fullConfirmations UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "shows a payment sent as Outgoing, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (headlineDoc fullConfirmations UnitBitcoin (Just seenTimeInCet) info)
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
      renderedLines (headlineDoc fullConfirmations UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Internal     -0.00001000 btc  2 confirmations"]

  -- The first line of the compact view starts with the short id, and
  -- leaves the type to the sign and the colour of the amount. The txids
  -- of fakeTxHash all start with 00000000.
  describe "The first line of the compact view of a transaction" $ do
    it "starts with the short id of the txid, then gives the time, the amount with its sign, and the confirmations" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5  14 Nov 2023 23:13     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "gives a payment sent a minus sign, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations"]

    -- 1250000000 satoshis are 12.5 bitcoins
    it "lines up amounts of different widths" $ do
      let small = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
          large = small {txInfoSummary = paymentSummary {summaryAmount = 1250000000}}
      concatMap (renderedLines . compactHeadline UnitBitcoin (Just seenTimeInCet)) [small, large]
        `shouldBe` [ "00000000  14 Nov 2023 23:13     +0.00050000 btc  3 confirmations",
                     "00000000  14 Nov 2023 23:13    +12.50000000 btc  3 confirmations"
                   ]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 1 False)) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     +0.00050000 btc  1 confirmation"]

    it "counts the confirmations up to 999" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 999 False)) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  999 confirmations"]

    it "says 999+ confirmations for more than 999" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1000 False)) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  999+ confirmations"]

    it "gives the fee rate of a transaction not in a block yet" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False)) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  unconfirmed, 4 sat/vB"]

    it "says dead for a transaction replaced or dropped from the network, without its fee rate" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True)) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  dead"]

    it "says the time is unknown when hw does not know it, in a column of the same width" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) Nothing 3 False)) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin Nothing info)
        `shouldBe` ["00000000  time unknown          +0.00050000 btc  3 confirmations"]

    it "starts with the short id of the nosigHash of a transaction not signed, and says it is not signed, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5  14 Nov 2023 23:13     -0.00041000 btc  not signed, 4 sat/vB"]

    it "starts with the short id of the txid of a signed transaction, and says it is not sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["b2d4f6a8  14 Nov 2023 23:13     -0.00041000 btc  signed, not sent, 4 sat/vB"]

    it "shows a transaction sent that the sync has not seen yet as sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) True False (Just seenTime) [])) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  sent, 4 sat/vB"]

    it "shows a transaction in conflict as conflicted, without its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) "" Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  conflicted"]

compactSpec :: Spec
compactSpec = do
  describe "The balance after a transaction" $ do
    let spent = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))

    it "starts the second line of the compact view, before what the transaction is for" $
      renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty))
        `shouldBe` [ "00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  balance 0.01208000 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "comes before the label of the transaction" $
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) (spent "Rent of October" Map.empty)) !! 1)
        `shouldBe` "  balance 0.01208000 btc, \"Rent of October\""

    it "shows an account left with nothing as zero" $
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just 0) (Just seenTimeInCet) (spent "" Map.empty)) !! 1)
        `shouldBe` "  balance 0.00000000 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "is in satoshis with the amount, each with its unit" $
      renderedLines (compactTxDoc btc UnitSatoshi Nothing (Just 1) (Just seenTimeInCet) (spent "" Map.empty))
        `shouldBe` [ "00000000  14 Nov 2023 23:13          -41000 sats  2 confirmations",
                     "  balance 1 sat, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    -- txs gives no balance to a dead transaction, which does not count
    it "is left out when the listing gives none" $
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) (spent "" Map.empty)) !! 1)
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "follows the fee in the verbose view" $
      take 2 (drop 4 (renderedLines (verboseTxDoc btc UnitBitcoin [] (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty))))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  Balance  : 0.01208000 btc"
                   ]

  describe "The compact view of a transaction" $ do
    it "gives the address a payment pays, without its change, on the line below its first line" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "a1075db5  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "gives the label of a payment, quoted, in place of the address it pays" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "Rent of October" Map.empty
      renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  \"Rent of October\""
                   ]

    it "names the address of the wallet a payment received is paid at by its path, with its label" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at /0/0 \"Rent\""

    it "gives the label of a payment received after the path of its address, in place of the label of the address" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "Salary" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at /0/0 \"Salary\""

    -- otherAddr 1 is 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "gives the largest output of several, and counts the others" $ do
      let several = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 10000), (otherAddr 1, 29000), (otherAddr 2, 1000)]}
          info = TxInfo several (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ and 2 more"

    -- A Taproot address has 62 characters
    it "gives the address of another wallet whole, however long, at the end of its line" $ do
      let toTaproot = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(taprootExternalAddr 0, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) toTaproot) !! 1)
        `shouldBe` "  balance 0.01208000 btc, to bc1p8zl0j9e73592k9n6ftz4keyq24dq8djqd3rkcrepu2fw6zeryyjs8zz3ev"

    it "names the account first in a listing of every account" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin (Just mainAccountRow) Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  account main, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the transaction a replacement replaces by its short id, before what it is for" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [b2d4f6a8Txid])) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  replaces b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the two transactions a replacement replaces, the second after and" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [b2d4f6a8Txid, a1075db5Second])) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  replaces b2d4f6a8 and a1075db5, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

-- The columns have fixed widths: 8 for the short id of the txid, 17 for
-- the time, 8 for the type, 14 for each amount and 5 for the
-- confirmations, the width of "Confs". " │ " sets two cells apart. The
-- confirmations are right-aligned, as numbers. The last column says what
-- the transaction is for, whole.
tableSpec :: Spec
tableSpec = do
  describe "The table of txs --tabular" $ do
    let spent = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False))
        spentWith confirmations = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) confirmations False)) "" Map.empty

    -- Under a heading, the line has a dash for each character and the
    -- space on each side: 8 + 1 = 9 under Txid, 17 + 2 = 19 under Time,
    -- 8 + 2 = 10 under Type, 14 + 2 = 16 under each amount, 5 + 2 = 7
    -- under Confs, and 11 + 1 = 12 under Description
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (txTableHeader UnitBitcoin)
        `shouldBe` [ "Txid     │ Time              │ Type     │   Amount (btc) │  Balance (btc) │ Confs │ Description",
                     "─────────┼───────────────────┼──────────┼────────────────┼────────────────┼───────┼────────────"
                   ]

    it "names satoshis in the headings of the amounts when they are in satoshis" $
      take 1 (concatMap renderedLines (txTableHeader UnitSatoshi))
        `shouldBe` ["Txid     │ Time              │ Type     │  Amount (sats) │ Balance (sats) │ Confs │ Description"]

    -- 40000 paid + 1000 of fee = 41000
    -- The txids of fakeTxHash all start with 00000000
    it "gives a transaction one row: its time, type, amount, balance, confirmations and the short id of its txid, then the address paid" $
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the txid, its first 8 digits, in the Txid column" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "counts the confirmations up to 999" $
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spentWith 999))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │   999 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says 999+ for more than 999 confirmations" $
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spentWith 1000))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │  999+ │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives 0 confirmations for a transaction not in a block yet, without its fee rate" $ do
      let unconfirmed = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) unconfirmed)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     0 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "shows a payment received with a plus sign, and the path of the address of the wallet it was paid at" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at /0/0"]

    it "gives the label of a transaction, quoted, in place of the address paid" $
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spent "Rent of October" Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ \"Rent of October\""]

    it "gives a long label whole, in the last column" $
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spent "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month" Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    -- A Taproot address has 62 characters
    it "gives the address of another wallet whole, however long, never cut" $ do
      let toTaproot = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(taprootExternalAddr 0, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) toTaproot)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to bc1p8zl0j9e73592k9n6ftz4keyq24dq8djqd3rkcrepu2fw6zeryyjs8zz3ev"]

    it "gives a Bitcoin Cash address whole, with its prefix" $ do
      let toCash = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(bchExternalAddr, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (txRowDoc bch UnitBitcoin (Just 1208000) (Just seenTimeInCet) toCash)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"]

    it "gives the label of a payment received after the path of the address it was paid at" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "Salary" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at /0/0 \"Salary\""]

    it "gives the label of the address a payment was received at when the payment has none" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at /0/0 \"Rent\""]

    it "gives the label of a payment received rather than the label of its address" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "Salary" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at /0/0 \"Salary\""]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address
    it "says a payment received at a change address is at one" $ do
      let atChangeAddress = paymentSummary {summaryMyOutputs = Map.fromList [(internalAddr 0, MyOutputs 50000 (Deriv :/ 1 :/ 0) "")]}
          info = TxInfo atChangeAddress (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at /1/0 change address"]

    -- /0/0 pays 40000 to /0/1 and 9000 to /1/0: the account loses the fee
    -- of 1000
    it "gives the path of the address a transaction between addresses of the wallet pays the most, and counts the others" $ do
      let internal =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 40000 (Deriv :/ 0 :/ 1) ""), (internalAddr 0, MyOutputs 9000 (Deriv :/ 1 :/ 0) "")]
              }
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Internal │    -0.00001000 │     0.01208000 │     2 │ to /0/1 and 1 more"]

    it "gives a long label of an address whole, after its path" $ do
      let longLabel = paymentSummary {summaryMyOutputs = Map.fromList [(externalAddr 0, MyOutputs 50000 (Deriv :/ 0 :/ 0) "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month")]}
          info = TxInfo longLabel (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at /0/0 \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "leaves the balance of a dead transaction empty" $ do
      let dead = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin Nothing (Just seenTimeInCet) dead)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │                │  dead │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- A dead transaction has 0 confirmations, as an unconfirmed one does,
    -- but it will never confirm
    it "says dead in place of the confirmations of a dead transaction" $ do
      let dead = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin Nothing (Just seenTimeInCet) dead)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │                │  dead │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the amounts as numbers alone, since the header names their unit" $
      renderedLines (txRowDoc btc UnitSatoshi (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │         -41000 │        1208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- 1250000000000 satoshis are 12500 bitcoins: "+12500.00000000" takes
    -- 15 characters, one more than the column
    it "lets an amount wider than its column push the rest of its row" $ do
      let large = TxInfo paymentSummary {summaryAmount = 1250000000000} (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      renderedLines (txRowDoc btc UnitBitcoin (Just 50000) (Just seenTimeInCet) large)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │ +12500.00000000 │     0.00050000 │     3 │ at /0/0"]

  -- The screen reads a listing in chunks. A transaction without a time
  -- shows "time unknown", whatever the time zone of the computer.
  describe "A chunk of the table of txs" $ do
    let listed = ListedTx (TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) Nothing 2 False)) "" Map.empty) (Just 1208000)

    it "starts with the header when it is the first one" $ do
      doc <- chunkDoc UnitBitcoin Table True (ResponseTxs mainAccountRow [listed])
      renderedLines doc
        `shouldBe` [ "Txid     │ Time              │ Type     │   Amount (btc) │  Balance (btc) │ Confs │ Description",
                     "─────────┼───────────────────┼──────────┼────────────────┼────────────────┼───────┼────────────",
                     "00000000 │ time unknown      │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "holds its rows alone when it is a later one, with no header and no blank line" $ do
      doc <- chunkDoc UnitBitcoin Table False (ResponseTxs mainAccountRow [listed])
      renderedLines doc
        `shouldBe` ["00000000 │ time unknown      │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says there are no transactions, without a header, when there are none" $ do
      doc <- chunkDoc UnitBitcoin Table True (ResponseTxs mainAccountRow [])
      renderedLines doc `shouldBe` ["There are no transactions to display"]

  addressTableSpec
  accountTableSpec
  coinTableSpec
  balanceTableSpec
  pendingTableSpec

-- The columns of the receiving addresses of main, with paths of 4
-- characters: 4 for the path, 34 for a base58 address, 15 for
-- "Spendable (btc)", 14 for "Received (btc)", 5 for the transactions, then
-- the label
addressTableSpec :: Spec
addressTableSpec = do
  describe "The table of addresses --tabular" $ do
    let receiving = addressTableColumns UnitBitcoin mainAccountRow 4 False
        changeAddressRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", dBAddressLabel = ""}

    -- Under a heading, the line has a dash for each character and the
    -- space on each side: 4 + 1 = 5 under Path, 34 + 2 = 36 under
    -- Address, 15 + 2 = 17 under Spendable, 14 + 2 = 16 under Received,
    -- 5 + 2 = 7 under Txs, and 5 + 1 = 6 under Label
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader receiving)
        `shouldBe` [ "Path │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Label",
                     "─────┼────────────────────────────────────┼─────────────────┼────────────────┼───────┼──────"
                   ]

    it "gives an address one row: its path, the address, what it can spend and received, its transactions and its label" $
      renderedLines (addressRowDoc UnitBitcoin receiving paidAddressRow 50000)
        `shouldBe` ["/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │ \"Invoice 42\""]

    it "gives a long label whole, in the last column" $
      renderedLines (addressRowDoc UnitBitcoin receiving paidAddressRow {dBAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"} 50000)
        `shouldBe` ["/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "leaves the amounts of an address no transaction paid empty, and ends its row without a label" $
      renderedLines (addressRowDoc UnitBitcoin receiving unusedAddressRow 0)
        `shouldBe` ["/0/1 │ 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg │                 │                │     0 │"]

    -- 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1 has 33 characters
    it "pads a base58 address shorter than the longest, so that the columns after it line up" $ do
      let shorter = unusedAddressRow {dBAddressIndex = 3, dBAddressAddress = "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"}
      renderedLines (addressRowDoc UnitBitcoin receiving shorter 0)
        `shouldBe` ["/0/3 │ 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1  │                 │                │     0 │"]

    it "gives the amounts as numbers alone, in satoshis when they are, with wider headings" $ do
      let inSatoshis = addressTableColumns UnitSatoshi mainAccountRow 4 False
      concatMap renderedLines (take 1 (tableHeader inSatoshis) <> [addressRowDoc UnitSatoshi inSatoshis paidAddressRow 50000])
        `shouldBe` [ "Path │ Address                            │ Spendable (sats) │ Received (sats) │   Txs │ Label",
                     "/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │            50000 │           50000 │     1 │ \"Invoice 42\""
                   ]

    -- Under Txs, the last column, the line has 5 + 1 = 6 dashes
    it "has no column for a label in the table of change addresses, which never have one" $ do
      let change = addressTableColumns UnitBitcoin mainAccountRow 4 True
      concatMap renderedLines (tableHeader change <> [addressRowDoc UnitBitcoin change changeAddressRow 50000])
        `shouldBe` [ "Path │ Address                            │ Spendable (btc) │ Received (btc) │   Txs",
                     "─────┼────────────────────────────────────┼─────────────────┼────────────────┼──────",
                     "/1/0 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │      0.00050000 │     0.00050000 │     1"
                   ]

    it "makes the column of the addresses of a BIP44 account 34 characters wide, the longest base58 address" $
      addressWidth mainAccountRow `shouldBe` 34

    -- bc1q, then 38 characters for the key hash and the checksum
    it "makes the column of the addresses of a BIP84 account 42 characters wide, the length of every one" $
      addressWidth segwitAccountRow `shouldBe` 42

  -- The newest receiving address of main is /0/11, of 5 characters
  describe "A chunk of the table of addresses" $
    it "starts with the header when it is the first one, its paths as wide as the longest of the account" $ do
      doc <- chunkDoc UnitBitcoin Table True (ResponseAddresses mainAccountRow [(paidAddressRow, 50000)])
      renderedLines doc
        `shouldBe` [ "Path  │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Label",
                     "──────┼────────────────────────────────────┼─────────────────┼────────────────┼───────┼──────",
                     "/0/0  │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │ \"Invoice 42\""
                   ]

-- The columns of the accounts: the name, the network and the path as
-- wide as the widest of the accounts, or as their headings, then 15 for
-- "Spendable (btc)", 17 for "Unconfirmed (btc)", and 5 for the coins.
-- main can spend 5000000 of its 3 coins, and 100000 is unconfirmed.
accountTableSpec :: Spec
accountTableSpec =
  describe "The table of accounts --tabular" $ do
    let ofMain = accountTableColumns UnitBitcoin [mainAccountRow]

    -- "main" and "btc" are shorter than "Account" and "Network", of 7
    -- characters, and "/44'/0'/0'" takes 10: 7 + 1 = 8 under Account,
    -- 7 + 2 = 9 under Network, 10 + 2 = 12 under Path, 15 + 2 = 17 under
    -- Spendable, 17 + 2 = 19 under Unconfirmed, and 5 + 1 = 6 under Coins
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader ofMain)
        `shouldBe` [ "Account │ Network │ Path       │ Spendable (btc) │ Unconfirmed (btc) │ Coins",
                     "────────┼─────────┼────────────┼─────────────────┼───────────────────┼──────"
                   ]

    it "gives an account one row: its name, network and path, what it can spend, what is unconfirmed, and its coins" $
      renderedLines (accountRowDoc UnitBitcoin ofMain mainAccountRow 5000000)
        `shouldBe` ["main    │ btc     │ /44'/0'/0' │      0.05000000 │        0.00100000 │     3"]

    it "gives the path of a bip84 account, and zero for what an unpaid account holds" $
      renderedLines (accountRowDoc UnitBitcoin (accountTableColumns UnitBitcoin [segwitAccountRow]) segwitAccountRow 0)
        `shouldBe` ["spending │ btc     │ /84'/0'/0' │      0.00000000 │        0.00000000 │     0"]

    -- "family savings account" has 22 characters
    it "gives a long name whole, in a first column as wide as it" $ do
      let family = mainAccountRow {dBAccountName = "family savings account"}
      renderedLines (accountRowDoc UnitBitcoin (accountTableColumns UnitBitcoin [family]) family 5000000)
        `shouldBe` ["family savings account │ btc     │ /44'/0'/0' │      0.05000000 │        0.00100000 │     3"]

    -- The eleventh account of Bitcoin Cash, of coin type 145, has the path
    -- /44'/145'/10', of 13 characters
    it "makes the column of paths as wide as the longest path" $ do
      let eleventh = mainAccountRow {dBAccountNetwork = "bch", dBAccountIndex = 10}
      renderedLines (accountRowDoc UnitBitcoin (accountTableColumns UnitBitcoin [eleventh]) eleventh 5000000)
        `shouldBe` ["main    │ bch     │ /44'/145'/10' │      0.05000000 │        0.00100000 │     3"]

    -- Each of the five characters of 家族の貯金 takes two columns of the
    -- terminal: 5 × 2 = 10. "main" takes 4, and 6 spaces make 10.
    it "counts two columns for a wide character, so that the columns line up after it" $ do
      let family = mainAccountRow {dBAccountName = "家族の貯金"}
          both = accountTableColumns UnitBitcoin [family, mainAccountRow]
      concatMap renderedLines [accountRowDoc UnitBitcoin both family 5000000, accountRowDoc UnitBitcoin both mainAccountRow 5000000]
        `shouldBe` [ "家族の貯金 │ btc     │ /44'/0'/0' │      0.05000000 │        0.00100000 │     3",
                     "main       │ btc     │ /44'/0'/0' │      0.05000000 │        0.00100000 │     3"
                   ]

    -- "spending", of 8 characters, is the longest name of the two
    it "makes the first column of a chunk as wide as the longest name of its accounts" $ do
      doc <- chunkDoc UnitBitcoin Table True (ResponseAccounts [(mainAccountRow, 5000000), (segwitAccountRow, 0)])
      renderedLines doc
        `shouldBe` [ "Account  │ Network │ Path       │ Spendable (btc) │ Unconfirmed (btc) │ Coins",
                     "─────────┼─────────┼────────────┼─────────────────┼───────────────────┼──────",
                     "main     │ btc     │ /44'/0'/0' │      0.05000000 │        0.00100000 │     3",
                     "spending │ btc     │ /84'/0'/0' │      0.00000000 │        0.00000000 │     0"
                   ]

-- The columns of the coins of main, with paths of 4 characters: 17 for
-- the time, 14 for the value, 17 for the state, 5 for the confirmations,
-- 34 for a base58 address, 4 for the path, then the label. The coins are shown at
-- 'seenTimeInCet', 23:13 in Central Europe.
coinTableSpec :: Spec
coinTableSpec =
  describe "The table of coins --tabular" $ do
    let headed = coinTableColumns UnitBitcoin mainAccountRow 4
        row = coinRowDoc btc UnitBitcoin headed (Just seenTimeInCet)

    -- Under a heading, the line has a dash for each character and the
    -- space on each side: 12 + 1 = 13 under Coin, 17 + 2 = 19 under Time,
    -- 14 + 2 = 16 under Value, 17 + 2 = 19 under State, 5 + 2 = 7 under
    -- Confs, 34 + 2 = 36 under Address, 4 + 2 = 6 under Path, and 5 + 1 = 6
    -- under Label
    it "names its columns in a header, the unit in the heading of the value, with a line under it" $
      concatMap renderedLines (tableHeader headed)
        `shouldBe` [ "Coin         │ Time              │    Value (btc) │ State             │ Confs │ Address                            │ Path │ Label",
                     "─────────────┼───────────────────┼────────────────┼───────────────────┼───────┼────────────────────────────────────┼──────┼──────"
                   ]

    -- spendableCoinRow is output 0 of transaction 1, whose txid starts
    -- with 00000000
    it "gives a coin one row: the time of its transaction, its value, state, confirmations, its address in full with its path, and its short id, then nothing without a label" $
      renderedLines (row spendableCoinRow)
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │"]

    -- "a1075db5:1" takes 10 of the 12 characters of the column
    it "gives the short id of the coin, the short id of its txid and its index, in the Coin column" $
      renderedLines (row spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1})
        `shouldBe` ["a1075db5:1   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │"]

    it "gives the label of the address of the coin" $
      renderedLines (row spendableCoinRow {jsonCoinAddressLabel = "Wages"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │ \"Wages\""]

    it "leaves out the label of the transaction that paid the coin, which the verbose view gives" $
      renderedLines (row spendableCoinRow {jsonCoinTxLabel = "Salary"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │"]

    it "says a coin at a change address is at one, in place of a label" $
      renderedLines (row atChange)
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │ /1/0 │ change address"]

    it "says a coin a pending transaction spends is locked by tx" $
      renderedLines (row spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ locked by tx      │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │"]

    it "says a coin locked with lockcoin is locked by you" $
      renderedLines (row spendableCoinRow {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ locked by you     │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │"]

    it "says a free coin the wallet cannot spend yet is not spendable yet, and gives an unconfirmed one 0 confirmations" $
      renderedLines (row spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ not spendable yet │     0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │"]

    it "says 999+ for more than 999 confirmations" $
      renderedLines (row spendableCoinRow {jsonCoinConfirmations = 1000})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │  999+ │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │"]

    it "gives a long label whole, in the last column" $
      renderedLines (row spendableCoinRow {jsonCoinAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0 │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    -- main has 12 receiving addresses and 3 change addresses: its longest
    -- path is /0/11, of 5 characters. A coin without a time shows "time
    -- unknown", whatever the time zone of the computer.
    it "takes the width of the paths from the longest path of the account, in a chunk of coins" $ do
      doc <- chunkDoc UnitBitcoin Table True (ResponseCoins mainAccountRow [spendableCoinRow {jsonCoinTime = Nothing}])
      renderedLines doc
        `shouldBe` [ "Coin         │ Time              │    Value (btc) │ State             │ Confs │ Address                            │ Path  │ Label",
                     "─────────────┼───────────────────┼────────────────┼───────────────────┼───────┼────────────────────────────────────┼───────┼──────",
                     "00000000:0   │ time unknown      │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ /0/0  │"
                   ]

-- The columns of the balances of main, with paths of 4 characters: 4 for
-- the path, 34 for a base58 address, 14 for "Total (btc)", 15 for
-- "Spendable (btc)", 17 for "Unspendable (btc)", then the label
balanceTableSpec :: Spec
balanceTableSpec =
  describe "The table of balances --tabular" $ do
    let headed = balanceTableColumns UnitBitcoin mainAccountRow 4
        changeAddressRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", dBAddressLabel = ""}

    -- 4 + 1 = 5 under Path, 34 + 2 = 36 under Address, 14 + 2 = 16 under
    -- Total, 15 + 2 = 17 under Spendable, 17 + 2 = 19 under Unspendable,
    -- and 5 + 1 = 6 under Label
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader headed)
        `shouldBe` [ "Path │ Address                            │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Label",
                     "─────┼────────────────────────────────────┼────────────────┼─────────────────┼───────────────────┼──────"
                   ]

    -- 80000 in all, of which 50000 can be spent: 80000 - 50000 = 30000
    -- cannot, which a pending transaction spends
    it "gives an address one row: its path, the address, its total, spendable and unspendable amounts, then its label" $
      renderedLines (balanceRowDoc UnitBitcoin headed paidAddressRow (CoinBalance 80000 50000 30000 0))
        `shouldBe` ["/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │ \"Invoice 42\""]

    it "gives a long label whole, in the last column" $
      renderedLines (balanceRowDoc UnitBitcoin headed paidAddressRow {dBAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"} (CoinBalance 80000 50000 30000 0))
        `shouldBe` ["/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "says a change address is one, in place of a label" $
      renderedLines (balanceRowDoc UnitBitcoin headed changeAddressRow (CoinBalance 9000 9000 0 0))
        `shouldBe` ["/1/0 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │     0.00009000 │      0.00009000 │        0.00000000 │ change address"]

    -- main has 12 receiving addresses and 3 change addresses: its longest
    -- path is /0/11, of 5 characters
    it "takes the width of the paths from the longest path of the account, in a chunk of balances" $ do
      doc <- chunkDoc UnitBitcoin Table True (ResponseBalances mainAccountRow [(paidAddressRow, CoinBalance 80000 50000 30000 0)])
      renderedLines doc
        `shouldBe` [ "Path  │ Address                            │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Label",
                     "──────┼────────────────────────────────────┼────────────────┼─────────────────┼───────────────────┼──────",
                     "/0/0  │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │ \"Invoice 42\""
                   ]

-- The columns of the pending transactions: the account when every account
-- is listed, as wide as its longest name, 17 for the time, 8 for
-- the type, 14 for the amount, 10 for the status and 6 for the fee rate,
-- then what it is for
pendingTableSpec :: Spec
pendingTableSpec =
  describe "The table of pending transactions --tabular" $ do
    let ofOne = pendingTableColumns UnitBitcoin Nothing
        ofMain = pendingTableColumns UnitBitcoin (Just [mainAccountRow])
        preparedWith signedM online conflicted = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) signedM online conflicted (Just seenTime) [])) "" Map.empty
        unsigned = preparedWith Nothing False False
        timeless = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty

    -- 8 + 1 = 9 under Hash, 17 + 2 = 19 under Time, 8 + 2 = 10 under Type,
    -- 14 + 2 = 16 under Amount, 10 + 2 = 12 under Status, 6 + 2 = 8 under
    -- sat/vB, and 11 + 1 = 12 under Description
    it "names its columns in a header, without the account when it lists one account" $
      concatMap renderedLines (tableHeader ofOne)
        `shouldBe` [ "Hash     │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Description",
                     "─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼────────────"
                   ]

    -- "main" is shorter than "Account", of 7 characters: 7 + 2 = 9 under
    -- Account, after the 8 + 1 = 9 under Hash
    it "names the account after the short id when it lists every account" $
      concatMap renderedLines (tableHeader ofMain)
        `shouldBe` [ "Hash     │ Account │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Description",
                     "─────────┼─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼────────────"
                   ]

    -- 40000 paid + 1000 of fee = 41000, at 4 sat/vB. The hashes of
    -- fakeTxHash all start with 00000000.
    it "gives a transaction one row: its time, type, amount, status, fee rate and short id, then what it is for" $
      renderedLines (pendingRowDoc btc UnitBitcoin ofOne Nothing (Just seenTimeInCet) unsigned)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the nosigHash of a transaction not signed" $ do
      let notSigned = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty
      renderedLines (pendingRowDoc btc UnitBitcoin ofOne Nothing (Just seenTimeInCet) notSigned)
        `shouldBe` ["a1075db5 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the txid of a signed transaction, which the commands take" $ do
      let signed = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty
      renderedLines (pendingRowDoc btc UnitBitcoin ofOne Nothing (Just seenTimeInCet) signed)
        `shouldBe` ["b2d4f6a8 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ signed     │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says a signed transaction not sent yet is signed" $
      renderedLines (pendingRowDoc btc UnitBitcoin ofOne Nothing (Just seenTimeInCet) (preparedWith (Just (fakeTxHash 2)) False False))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ signed     │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says a transaction sent that the sync has not seen yet is sent" $
      renderedLines (pendingRowDoc btc UnitBitcoin ofOne Nothing (Just seenTimeInCet) (preparedWith (Just (fakeTxHash 2)) True False))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ sent       │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says a transaction in conflict is conflicted, and leaves its fee rate empty" $
      renderedLines (pendingRowDoc btc UnitBitcoin ofOne Nothing (Just seenTimeInCet) (preparedWith (Just (fakeTxHash 2)) False True))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ conflicted │        │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "names the account of the transaction after its short id when it lists every account" $
      renderedLines (pendingRowDoc btc UnitBitcoin ofMain (Just mainAccountRow) (Just seenTimeInCet) unsigned)
        `shouldBe` ["00000000 │ main    │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- "family savings" has 14 characters, more than "main" and "Account"
    it "makes the column of accounts as wide as the longest name it lists" $ do
      let family = mainAccountRow {dBAccountName = "family savings"}
          both = pendingTableColumns UnitBitcoin (Just [mainAccountRow, family])
      concatMap renderedLines [pendingRowDoc btc UnitBitcoin both (Just family) (Just seenTimeInCet) unsigned, pendingRowDoc btc UnitBitcoin both (Just mainAccountRow) (Just seenTimeInCet) unsigned]
        `shouldBe` [ "00000000 │ family savings │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "00000000 │ main           │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "gives a long name whole, in a first column as wide as it" $ do
      let family = mainAccountRow {dBAccountName = "family savings account for travel"}
      renderedLines (pendingRowDoc btc UnitBitcoin (pendingTableColumns UnitBitcoin (Just [family])) (Just family) (Just seenTimeInCet) unsigned)
        `shouldBe` ["00000000 │ family savings account for travel │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- The 13 characters of 家族の貯金と旅行の口座です take 26 columns: main
    -- takes 4, and 22 spaces pad it
    it "counts two columns for a wide character, so that the columns line up after it" $ do
      let family = mainAccountRow {dBAccountName = "家族の貯金と旅行の口座です"}
          both = pendingTableColumns UnitBitcoin (Just [mainAccountRow, family])
      concatMap renderedLines [pendingRowDoc btc UnitBitcoin both (Just family) (Just seenTimeInCet) unsigned, pendingRowDoc btc UnitBitcoin both (Just mainAccountRow) (Just seenTimeInCet) unsigned]
        `shouldBe` [ "00000000 │ 家族の貯金と旅行の口座です │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "00000000 │ main                       │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "gives the label of a payment in place of the address it pays, as the table of txs does" $ do
      let labelled = unsigned {txInfoLabel = "Rent of October"}
      renderedLines (pendingRowDoc btc UnitBitcoin ofOne Nothing (Just seenTimeInCet) labelled)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ \"Rent of October\""]

    -- A transaction without a time shows "time unknown", whatever the time
    -- zone of the computer
    it "names the accounts in a chunk of the pending transactions of every account" $ do
      doc <- chunkDoc UnitBitcoin Table True (ResponsePendingTxs Nothing [(mainAccountRow, timeless)])
      renderedLines doc
        `shouldBe` [ "Hash     │ Account │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Description",
                     "─────────┼─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼────────────",
                     "00000000 │ main    │ time unknown      │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "leaves the accounts out of a chunk of the pending transactions of one account" $ do
      doc <- chunkDoc UnitBitcoin Table False (ResponsePendingTxs (Just mainAccountRow) [(mainAccountRow, timeless)])
      renderedLines doc
        `shouldBe` ["00000000 │ time unknown      │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

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
                     "  Short id : 00000000",
                     "  Time     : 14 Nov 2023 23:13:20 CET",
                     "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  /1/0"
                   ]

    -- In satoshis, "50000 sats" takes 10 characters and "9000 sats" 9:
    -- the amounts are right-aligned, and the paths start in one column
    it "lines up the paths of the addresses of the wallet after amounts of different widths" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      drop 5 (renderedLines (verboseTxDoc btc UnitSatoshi [] Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  50000 sats  /0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  40000 sats",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn   9000 sats  /1/0"
                   ]

    it "gives the txid in full, then its short id, its first 8 digits" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Txid     : a1075db5c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : a1075db5"
                   ]

    it "gives the full count of the confirmations, past 999 too" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1234 False)) "" Map.empty
      take 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  1234 confirmations"]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1 False)) "" Map.empty
      take 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  1 confirmation"]

    it "shows the label of a transaction, quoted, as its first field" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "Rent of October" Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Label    : \"Rent of October\"",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    -- "  From     : " takes 13 characters
    it "shows the label of an address of the wallet, quoted, on the line below, under the address" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0",
                     "             \"Rent\""
                   ]

    -- The coin spent is of transaction 1, the payment labelled Salary
    it "follows the label of an address spent from with the labels of its coins, after the word from" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" (Map.fromList [(fakeTxHash 1, "Salary")])
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 6)
        `shouldBe` "             \"Rent\"  from \"Salary\""

    -- The spending from the first change address, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn
    it "marks a change address spent from as one, on the line below the address" $ do
      let fromChange = spendSummary {summaryMyInputs = Map.fromList [(internalAddr 0, MyInputs 50000 (Deriv :/ 1 :/ 0) "" [OutPoint (fakeTxHash 1) 0])]}
          info = TxInfo fromChange (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00050000 btc  /1/0",
                     "             change address"
                   ]

    -- A transfer of the account to its first change address, which the
    -- compact view names as the address paid
    it "marks a change address an internal transaction pays as one" $ do
      let toChange = spendSummary {summaryType = TxInternal, summaryOtherOutputs = Map.empty}
          info = TxInfo toChange (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      (renderedLines (compactTxDoc btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to /1/0 change address"

    it "gives the labels of the coins of an address spent from without a label of its own on the line below" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" (Map.fromList [(fakeTxHash 1, "Salary")])
      take 2 (drop 5 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0",
                     "             from \"Salary\""
                   ]

    -- The payer spends 51000 from otherAddr 0, which is nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      drop 5 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0"
                   ]

    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU sorts before
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ: the outputs come by address
    it "puts each address of a section on a line of its own, under the first" $ do
      let twoPaid = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 30000), (otherAddr 1, 10000)]}
          info = TxInfo twoPaid (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 6 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00030000 btc",
                     "             1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  0.00010000 btc"
                   ]

    it "explains why a transaction is dead" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True)) "" Map.empty
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  Status   : replaced, or dropped from the network"

    it "shows under the fee the change too small for an output that the fee took in" $ do
      let info = TxInfo spendSummary {summaryChangeInFee = Just 300} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 4 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "             includes 0.00000300 btc of change too small for an output"
                   ]

    it "shows the lock time under the fee, as the block the transaction waits for, and no field without one" $ do
      let locked = TxInfo spendSummary {summaryLockTime = 814037} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
          unlocked = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      take 2 (drop 4 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) locked)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  Lock time: block 814037"
                   ]
      length (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) unlocked)) `shouldBe` 8

    -- 500,000,000 and above, a lock time is a Unix time; hw writes none
    it "shows a lock time of 500,000,000 or more as a Unix time" $ do
      let locked = TxInfo spendSummary {summaryLockTime = 1700000000} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty
      renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) locked) !! 5
        `shouldBe` "  Lock time: Unix time 1700000000"

    it "shows an unsigned transaction by its nosigHash, in full then by its short id, with the time it was created" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty
      take 3 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  NoSigHash: a1075db5c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : a1075db5",
                     "  Created  : 14 Nov 2023 23:13:20 CET"
                   ]

    it "shows a signed transaction by its txid, in full then by its short id" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Txid     : b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : b2d4f6a8"
                   ]

    it "explains a conflict, and how to free the coins" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) "" Map.empty
      (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  Conflict : A coin it spends was spent by another transaction, or is gone. It can no longer be sent. Delete it with deletetx."

    it "names every transaction a replacement replaces, one under the other" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5, fakeTxHash 6])) "" Map.empty
      take 2 (drop 3 (renderedLines (verboseTxDoc btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
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
