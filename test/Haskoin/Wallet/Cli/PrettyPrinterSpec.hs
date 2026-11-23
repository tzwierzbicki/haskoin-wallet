{-# LANGUAGE OverloadedStrings #-}

-- | How hw shows what it lists and answers with: a transaction, a
-- portfolio, an address, a coin, a sync, an import. Each has a compact
-- view, the one the listings show by default, and some a verbose view,
-- that of --verbose and of the commands that answer with one element.
-- Every listing also shows in a table, that of --tabular. A listing over a
-- portfolio comes in sections, one for each network, or for each account
-- for the addresses. The transactions are those of the shared story of
-- TestUtils, a payment of 50000 received at /0/0 and then spent, and the
-- rows are those of the bip44 account of portfolio0 of wallet0 on btc,
-- shown at 'seenTimeInCet'.
module Haskoin.Wallet.Cli.PrettyPrinterSpec (spec) where

import Control.Exception (finally)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (addUTCTime)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Bip329 (Record (..), RecordType (..), record)
import Haskoin.Wallet.Cli.Listing (ChunkAt (..))
import Haskoin.Wallet.Cli.Parser (AddressCode (..), Command (..), NewPortfolio (..), Recover (..), TxShown (..), WordsLayout (..))
import Haskoin.Wallet.Cli.PrettyPrinter
import Haskoin.Wallet.Backup (BackupsShown (..), CopiesShown (..))
import Haskoin.Wallet.Cli.Response (AccountCounts (..), BackupFiles (..), ChangeFollowed (..), ChangeStop (..), Group (..), NewSecret (..), Response (..))
import Haskoin.Wallet.Cli.Style (errorDoc, renderPlain)
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Account (CoinTotals (..), NetworkView (..), PortfolioView (..))
import Haskoin.Wallet.Database.Address (AddressFunds (..), CoinBalance (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..), LeftCoin (..), LeftReason (..))
import Haskoin.Wallet.Database.PendingTx (Retag (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), ActiveAfter (..), Place (..), PortfolioRef (..), WalletNaming (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Database.Tag (TagDeleted (..), TagUse (..), TagWidths (..))
import Haskoin.Wallet.Tags (ShownTag (..), TagColor (..), plainTag)
import Haskoin.Wallet.Keys (AccountKind (..), ForeignKind (..), MnemonicPass (..), legacyPath)
import Haskoin.Wallet.DatabaseFile (DatabaseShown (..), PortfolioShown (..), WalletShown (..))
import Haskoin.Wallet.Labels (AccountImport (..), FileRecord (..), FileShown (..), FileTag (..), ImportReport (..), ImportScope (..), LabelImport (..), PortfolioImport (..), RecordFate (..), WalletImport (..), wholeFile)
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncAfter (..), SyncRes (..), nothingToSyncAfter)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Testnet3 (btcTestnet3)
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec

spec :: Spec
spec = do
  headlineSpec
  compactSpec
  tableSpec
  verboseSpec
  answerSpec
  otherPortfolioSpec
  placeSpec
  portfolioSpec
  newPortfolioSpec
  importSpec
  databaseFileSpec
  restoredSpec
  backupsSpec
  walletFileSpec
  addressSpec
  balanceSpec
  coinSpec
  syncSpec
  recoverSpec
  deletedSpec
  sectionSpec
  headerSpec
  nextStepsSpec
  mnemonicSpec
  outsideTextSpec
  hiddenKeySpec
  addressBookSpec

{- Fixtures of this spec -}

-- | Portfolio 1 of wallet 2, which every line that says where an answer
-- is names with its wallet
otherRef :: PortfolioRef
otherRef = PortfolioRef 2 1

-- | Portfolio 1 of wallet 0, the other portfolio of the test wallet
portfolio1Ref :: PortfolioRef
portfolio1Ref = PortfolioRef 0 1

-- | The first receiving address of the bip44 account of portfolio 1 on
-- btc, 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, as a transaction of another
-- portfolio names it once the database told where it is: bip44/0/0 of
-- portfolio 1, with the label given
inPortfolio1 :: Text -> Map.Map Address HeldElsewhere
inPortfolio1 label = Map.fromList [(account1Addr, HeldElsewhere portfolio1Ref (legacyPath (Deriv :/ 0 :/ 0)) label)]

-- | The spending of the shared story as a transfer to portfolio 1, as
-- portfolio 0 sees it: the 40000 go to the address of 'inPortfolio1',
-- which the summary of portfolio 0 counts among the addresses of others.
-- 9000 still come back as change, and 1000 are the fee.
transfer :: TxInfo
transfer =
  TxInfo
    spendSummary {summaryOtherOutputs = Map.fromList [(account1Addr, 40000)]}
    (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False []))
    ""
    Map.empty
    (inPortfolio1 "")
    noTxTags
    Map.empty

-- | What 'mainPortfolioView' holds on btc, without the keys of its
-- accounts
btcView :: NetworkView
btcView = NetworkView btc 5000000 100000 5100000 4900000 3 17 []

-- | A network of a portfolio where it holds nothing and has no address
emptyOn :: Network -> NetworkView
emptyOn net = NetworkView net 0 0 0 0 0 0 []

-- | A whole portfolio 0 of wallet 0, as newportfolio creates it: a bip44
-- and a bip84 account on btc and on each test network of Bitcoin, a bip44
-- one on the others, 2 + 1 + 2 + 2 + 2 + 1 + 1 + 1 = 12 accounts. The
-- keys are those of btc on every network.
wholePortfolioView :: PortfolioView
wholePortfolioView =
  mainPortfolioView
    { viewNetworks =
        [ (emptyOn net) {networkViewAccounts = keys}
          | (net, keys) <- [(btc, both), (bch, [legacyKeys]), (btcTestnet3, both), (btcTest4, both), (btcRegTest, both), (bchTest3, [legacyKeys]), (bchTest4, [legacyKeys]), (bchRegTest, [legacyKeys])]
        ]
    }
  where
    both = [legacyKeys, segwitKeys]

-- | The second receiving address of the bip84 account of portfolio 0 on
-- btc, which nothing has paid: bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2,
-- /0/1 of m/84'/0'/0'
segwitAddressRow :: DBAddress
segwitAddressRow = unusedAddressRow {dBAddressAddress = segwitExternalAddrsText !! 1}

-- | The lines hw prints for an answer, as it prints them on stdout when
-- stdout is a file: plain, without colours, 80 columns wide
printedLines :: Response -> IO [String]
printedLines = printedLinesIn NumberedColumns

-- | The lines hw prints for an answer, as 'printedLines' gives them, with
-- the words of a new secret in the layout given
printedLinesIn :: WordsLayout -> Response -> IO [String]
printedLinesIn layout answer =
  inScratchDirectory $ \dir -> do
    let file = dir </> "answer.txt"
    IO.hFlush IO.stdout
    saved <- hDuplicate IO.stdout
    ( do
        out <- IO.openFile file IO.WriteMode
        hDuplicateTo out IO.stdout
        IO.hClose out
        responsePrinter UnitBitcoin Compact WithoutWallets WithoutCode FullTx layout answer
        IO.hFlush IO.stdout
      )
      `finally` (hDuplicateTo saved IO.stdout >> IO.hClose saved)
    lines <$> IO.readFile' file

{- Outside text and mnemonics -}

-- What reaches a document without passing through a label, such as an
-- error quoting a file, is written out as the document is printed
outsideTextSpec :: Spec
outsideTextSpec =
  describe "A document as hw prints it" $
    it "has an escape in any of its texts written out, never as an order to the terminal" $
      renderPlain (errorDoc "Unknown network btc\ESC[2J")
        `shouldBe` "Error: Unknown network btc\\x1b[2J"

-- The key of the Bitcoin wiki's "Wallet import format" page
hiddenKeySpec :: Spec
hiddenKeySpec =
  describe "An error on the screen" $
    it "shows no private key that its text quotes" $
      renderPlain (errorDoc "The wallet has no address KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617")
        `shouldBe` "Error: The wallet has no address (a private key, not shown)"

mnemonicSpec :: Spec
mnemonicSpec = do
  describe "A new mnemonic as hw shows it" $ do
    it "says how many words it has and where its entropy came from, then numbers its words four on a line, in columns" $
      renderedLines (mnemonicDoc NumberedColumns "/dev/random" (Text.words (mnemonicWords testMnemonic)))
        `shouldBe` [ "Mnemonic of 12 words, from /dev/random",
                     "   1. snow       2. senior     3. nerve      4. virus",
                     "   5. fabric     6. now        7. fringe     8. clip",
                     "   9. marble    10. interest  11. analyst   12. can"
                   ]

  describe "The shares of a new backup as hw shows them" $ do
    it "say how many they are, of how many words, how many rebuild the seed and where the entropy came from, then each share under its number, a blank line between them" $
      renderedLines
        ( sharesDoc
            NumberedColumns
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
      renderedLines (sharesDoc NumberedColumns "/dev/random" 1 [Text.words "analysis morning academic academic center intend saver petition romp expand dryer smirk award estate sunlight deny mortgage debut dictate rocky"])
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
      let shown = renderedLines (sharesDoc NumberedColumns "/dev/random" 2 (replicate 2 (Text.words "academic agency academic acid airport idle resident emphasis nail dwarf crunch cards adjust bundle impact election activity modify tension move fitness terminal emerald stick burning quantity payment body strike guitar emphasis enjoy flexible")))
      take 1 shown `shouldBe` ["Backup in 2 shares of 33 words, any 2 of them rebuild the seed, from /dev/random"]
      take 2 (drop 10 shown) `shouldBe` ["  29. strike    30. guitar    31. emphasis  32. enjoy", "  33. flexible"]

  describe "The master private key of a new mnemonic, as newportfolio --show-master-key shows it" $ do
    let testWordsList = Text.words (mnemonicWords testMnemonic)
        created = ActiveAfter (PortfolioRef 0 0) True
    -- What hw prints, 80 columns wide. The key has 111 characters, and a
    -- break in it would make two lines to copy.
    it "is under the words, on a line of its own that holds the key whole and nothing else" $ do
      shown <- printedLines (ResponseNewPortfolio (Just (NewMnemonic "/dev/random" testWordsList)) (Just testMasterXPrv) wholePortfolioView created Nothing)
      take 9 shown
        `shouldBe` [ "Mnemonic of 12 words, from /dev/random",
                     "   1. snow       2. senior     3. nerve      4. virus",
                     "   5. fabric     6. now        7. fringe     8. clip",
                     "   9. marble    10. interest  11. analyst   12. can",
                     "",
                     "Master private key",
                     "xprv9s21ZrQH143K4PVwc7FKujfAkFhbjYRar1ht5iBN6kk8Ycr93yc7dgT8Q31MJge3xAsja5oVberk2gSvrWJ9Ft1pnUNoFuGNmQMuhxAL14R",
                     "",
                     "Created portfolio0 of wallet0"
                   ]

    it "is not shown when the option did not ask for it" $ do
      shown <- printedLines (ResponseNewPortfolio (Just (NewMnemonic "/dev/random" testWordsList)) Nothing wholePortfolioView created Nothing)
      take 6 shown
        `shouldBe` [ "Mnemonic of 12 words, from /dev/random",
                     "   1. snow       2. senior     3. nerve      4. virus",
                     "   5. fabric     6. now        7. fringe     8. clip",
                     "   9. marble    10. interest  11. analyst   12. can",
                     "",
                     "Created portfolio0 of wallet0"
                   ]

  describe "A new mnemonic on one line, as newportfolio --one-line shows it" $ do
    it "has its words on the line under the heading, a space between them, without numbers or indent" $
      renderedLines (mnemonicDoc OneLine "/dev/random" (Text.words (mnemonicWords testMnemonic)))
        `shouldBe` [ "Mnemonic of 12 words, from /dev/random",
                     "snow senior nerve virus fabric now fringe clip marble interest analyst can"
                   ]

    it "puts each share of a backup on a line of its own, under its number" $
      renderedLines
        ( sharesDoc
            OneLine
            "/dev/random"
            2
            [ Text.words "acid fawn academic acid axis auction endless award answer random spew dryer again public civil unwrap scandal pleasure extra member",
              Text.words "acid fawn academic agency damage stilt math warmth satisfy step gravity prospect elder swing flexible explain subject sugar holy sled"
            ]
        )
        `shouldBe` [ "Backup in 2 shares of 20 words, any 2 of them rebuild the seed, from /dev/random",
                     "",
                     "Share 1",
                     "acid fawn academic acid axis auction endless award answer random spew dryer again public civil unwrap scandal pleasure extra member",
                     "",
                     "Share 2",
                     "acid fawn academic agency damage stilt math warmth satisfy step gravity prospect elder swing flexible explain subject sugar holy sled"
                   ]

    -- What hw prints, 80 columns wide. The line of 33 words is far wider,
    -- and a break in it would make two lines to copy.
    it "keeps the 33 words of a share on one line, wider than the 80 columns hw lays out" $ do
      shown <-
        printedLinesIn
          OneLine
          ( ResponseNewPortfolio
              (Just (NewShares "/dev/random" 1 [Text.words "academic agency academic acid airport idle resident emphasis nail dwarf crunch cards adjust bundle impact election activity modify tension move fitness terminal emerald stick burning quantity payment body strike guitar emphasis enjoy flexible"]))
              Nothing
              wholePortfolioView
              (ActiveAfter (PortfolioRef 0 0) True)
              Nothing
          )
      take 5 shown
        `shouldBe` [ "Backup in 1 share of 33 words, which is the whole backup, from /dev/random",
                     "",
                     "Share 1",
                     "academic agency academic acid airport idle resident emphasis nail dwarf crunch cards adjust bundle impact election activity modify tension move fitness terminal emerald stick burning quantity payment body strike guitar emphasis enjoy flexible",
                     ""
                   ]

{- Where an answer is -}

placeSpec :: Spec
placeSpec = do
  describe "The line that says where an answer is" $ do
    it "names the portfolio, then the network, without the wallet in a database of one wallet" $
      renderedLines (placeLine WithoutWallets mainPlace) `shouldBe` ["portfolio0  btc"]

    -- The one wallet may be wallet2, once the others were deleted
    it "leaves the wallet out in a database of one wallet, even when it is not wallet0" $
      renderedLines (placeLine WithoutWallets (Place otherRef bch)) `shouldBe` ["portfolio1  bch"]

    it "names the wallet after the portfolio in a database of several wallets" $
      renderedLines (placeLine WithWallets (Place otherRef bch)) `shouldBe` ["portfolio1 of wallet2  bch"]

    -- portfolio0 alone could be that of any of the wallets
    it "names wallet0 too in a database of several wallets" $
      renderedLines (placeLine WithWallets mainPlace) `shouldBe` ["portfolio0 of wallet0  btc"]

    it "names an account by its portfolio, its network and its type" $
      renderedLines (accountRefLine WithoutWallets segwitAccountRef) `shouldBe` ["portfolio0  btc  bip84"]

    it "names the wallet of an account after its portfolio in a database of several wallets, wallet0 too" $
      renderedLines (accountRefLine WithWallets segwitAccountRef) `shouldBe` ["portfolio0 of wallet0  btc  bip84"]

  -- Each of the five characters of 家族の貯金 takes two columns of the
  -- terminal: 5 × 2 = 10, and 2 spaces make 12
  describe "A text padded to a width" $
    it "counts two columns for a wide character, so that what follows it lines up" $
      block 12 "家族の貯金" `shouldBe` "家族の貯金  "

{- Portfolios -}

portfolioSpec :: Spec
portfolioSpec = do
  describe "A portfolio as portfolios shows it" $ do
    -- The coins on btc are worth 5100000 satoshis, and 4900000 can be
    -- spent: 5100000 - 4900000 = 200000 cannot. The view is of the active
    -- portfolio.
    it "marks the active portfolio with a *, gives its name, its wallet and its description, then under them the day it was created and on a line what it holds on each network it has addresses or coins on" $
      renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView)
        `shouldBe` [ "* portfolio0  wallet0  \"savings\"",
                     "    created 14 Nov 2023",
                     "    btc  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins, 17 addresses"
                   ]

    -- The line once gave "spendable 0.04900000 btc, unconfirmed
    -- 0.00100000 btc": two amounts that overlap when the wallet can
    -- spend its own unconfirmed change, and no total
    it "starts the line of a network with the total, and gives no unconfirmed amount beside the amounts that add up to it" $ do
      let figures = Text.pack (renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView) !! 2)
      ("    btc  total 0.05100000 btc, " `Text.isPrefixOf` figures, "unconfirmed" `Text.isInfixOf` figures) `shouldBe` (True, False)

    -- Every coin can be spent: 5100000 of 5100000
    it "leaves out the unspendable amount when the portfolio can spend all it holds" $ do
      let allFree = mainPortfolioView {viewNetworks = [btcView {networkViewSpendable = 5100000}]}
      drop 2 (renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet allFree))
        `shouldBe` ["    btc  total 0.05100000 btc, spendable 0.05100000 btc, 3 coins, 17 addresses"]

    -- The mark takes a column of its own, so that the names line up
    it "starts the first line of a portfolio that is not the active one with a space where the mark goes" $
      take 1 (renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView {viewActive = False}))
        `shouldBe` ["  portfolio0  wallet0  \"savings\""]

    it "names a wallet other than wallet0 the same way, after the portfolio" $
      take 1 (renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView {viewRef = otherRef}))
        `shouldBe` ["* portfolio1  wallet2  \"savings\""]

    it "gives a portfolio without a description its name and its wallet alone" $
      take 1 (renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView {viewDescription = ""}))
        `shouldBe` ["* portfolio0  wallet0"]

    -- A whole portfolio has an account on eight networks: bch holds
    -- nothing here, and btctest4 has 2 addresses and no coin
    it "leaves out the networks where it has neither an address nor a coin, and lines up the figures of the others" $ do
      let three = mainPortfolioView {viewNetworks = [btcView, emptyOn bch, (emptyOn btcTest4) {networkViewAddresses = 2}]}
      drop 2 (renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet three))
        `shouldBe` [ "    btc       total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins, 17 addresses",
                     "    btctest4  total 0.00000000 btc, spendable 0.00000000 btc, no coins, 2 addresses"
                   ]

    it "says a portfolio with no address on any network has none yet" $
      renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView {viewNetworks = [emptyOn btc, emptyOn bch]})
        `shouldBe` [ "* portfolio0  wallet0  \"savings\"",
                     "    created 14 Nov 2023",
                     "    no addresses yet"
                   ]

    it "gives the description of its wallet on a line of its own, under the first" $
      take 2 (renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView {viewWalletDescription = "laptop"}))
        `shouldBe` ["* portfolio0  wallet0  \"savings\"", "    wallet0 is \"laptop\""]

  describe "A portfolio as portfolios --verbose shows it" $ do
    let withKeys = mainPortfolioView {viewNetworks = [btcView {networkViewAccounts = [legacyKeys, segwitKeys]}]}

    -- The total is split two ways. 4900000 spendable + 200000
    -- unspendable = 5100000, and 5000000 confirmed + 100000 unconfirmed
    -- = 5100000.
    it "gives its fingerprint and the time it was created on the clock given, then each network with its total, the two parts of it by what can be spent and the two by what is confirmed, and under it each account with its path, its descriptor and its SLIP-132 key" $
      renderedLines (verbosePortfolioDoc UnitBitcoin seenTimeInCet withKeys)
        `shouldBe` [ "* portfolio0  wallet0  \"savings\"",
                     "    Fingerprint: bc560f95",
                     "    Created    : 14 Nov 2023 23:13:20 CET",
                     "    btc",
                     "      Total      : 0.05100000 btc",
                     "      Spendable  : 0.04900000 btc",
                     "      Unspendable: 0.00200000 btc",
                     "      Confirmed  : 0.05000000 btc",
                     "      Unconfirmed: 0.00100000 btc",
                     "      Coins      : 3",
                     "      Addresses  : 17",
                     "      bip44  /44'/0'/0'",
                     "        Descriptor: pkh([bc560f95/44h/0h/0h]" <> Text.unpack testAccountXPub <> "/<0;1>/*)#vq597j87",
                     "        SLIP-132  : " <> Text.unpack testAccountXPub,
                     "      bip84  /84'/0'/0'",
                     "        Descriptor: wpkh([bc560f95/84h/0h/0h]" <> Text.unpack testSegwitAccountXPub <> "/<0;1>/*)#5nktextl",
                     "        SLIP-132  : " <> Text.unpack testSegwitAccountZPub
                   ]

    it "starts the first line of a portfolio that is not the active one with a space where the mark goes" $
      take 1 (renderedLines (verbosePortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView {viewActive = False}))
        `shouldBe` ["  portfolio0  wallet0  \"savings\""]

    it "gives every network the portfolio has an account on, those that hold nothing too" $
      filter (`elem` ["    btc", "    bch"]) (renderedLines (verbosePortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView {viewNetworks = [btcView, emptyOn bch]}))
        `shouldBe` ["    btc", "    bch"]

    it "gives the description of its wallet as a field" $
      (renderedLines (verbosePortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView {viewWalletDescription = "laptop"}) !! 3)
        `shouldBe` "    Wallet     : \"laptop\""

  -- The columns of the portfolios: the mark of the active portfolio, 1
  -- wide without a heading, then the wallet, the portfolio and the
  -- network as wide as their widest cells, or as their headings, then 14
  -- for the amounts under "Total (btc)", 15 for "Spendable (btc)", 17 for
  -- "Unspendable (btc)", 5 for the coins, 9 for the addresses, then the
  -- description
  describe "The table of portfolios --tabular" $ do
    let ofMain = portfolioTableColumns UnitBitcoin [mainPortfolioView]

    -- The mark takes 1: 1 + 1 = 2 under it. "wallet0" takes 7,
    -- "portfolio0" 10, and "Network" 7: 7 + 2 = 9 under Wallet, 10 + 2 = 12
    -- under Portfolio, 7 + 2 = 9 under Network, 14 + 2 = 16 under Total,
    -- 15 + 2 = 17 under Spendable, 17 + 2 = 19 under Unspendable, 5 + 2 =
    -- 7 under Coins, 9 + 2 = 11 under Addresses and 11 + 1 = 12 under
    -- Description
    it "names its columns in a header, the column of the mark without a heading and the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader ofMain)
        `shouldBe` [ "  │ Wallet  │ Portfolio  │ Network │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Coins │ Addresses │ Description",
                     "──┼─────────┼────────────┼─────────┼────────────────┼─────────────────┼───────────────────┼───────┼───────────┼────────────"
                   ]

    it "gives a portfolio a row on a network: the mark of the active portfolio, its wallet, its name, the network, the total it holds, what it can spend, what it cannot spend, its coins and addresses, and its description" $
      concatMap renderedLines (portfolioRowDocs UnitBitcoin ofMain mainPortfolioView)
        `shouldBe` ["* │ wallet0 │ portfolio0 │ btc     │     0.05100000 │      0.04900000 │        0.00200000 │     3 │        17 │ \"savings\""]

    it "leaves the column of the mark blank in the row of a portfolio that is not the active one" $
      concatMap renderedLines (portfolioRowDocs UnitBitcoin ofMain mainPortfolioView {viewActive = False})
        `shouldBe` ["  │ wallet0 │ portfolio0 │ btc     │     0.05100000 │      0.04900000 │        0.00200000 │     3 │        17 │ \"savings\""]

    -- "btctest4" takes 8, more than "Network"
    it "gives a portfolio a row for each network it has addresses or coins on, the column of the networks as wide as the widest" $ do
      let two = mainPortfolioView {viewNetworks = [btcView, emptyOn bch, (emptyOn btcTest4) {networkViewAddresses = 2}]}
          headed = portfolioTableColumns UnitBitcoin [two]
      concatMap renderedLines (portfolioRowDocs UnitBitcoin headed two)
        `shouldBe` [ "* │ wallet0 │ portfolio0 │ btc      │     0.05100000 │      0.04900000 │        0.00200000 │     3 │        17 │ \"savings\"",
                     "* │ wallet0 │ portfolio0 │ btctest4 │     0.00000000 │      0.00000000 │        0.00000000 │     0 │         2 │ \"savings\""
                   ]

    it "gives a portfolio with no address one row, with no network and no figures" $ do
      let unused = mainPortfolioView {viewNetworks = [emptyOn btc]}
      concatMap renderedLines (portfolioRowDocs UnitBitcoin (portfolioTableColumns UnitBitcoin [unused]) unused)
        `shouldBe` ["* │ wallet0 │ portfolio0 │         │                │                 │                   │       │           │ \"savings\""]

    -- "wallet12" takes 8 and "portfolio10" 11: the listing is read whole.
    -- 8 + 2 = 10 under Wallet, 11 + 2 = 13 under Portfolio.
    it "makes the columns of the wallets and of the portfolios of a chunk as wide as their widest names" $ do
      let later = mainPortfolioView {viewRef = PortfolioRef 12 10, viewActive = False, viewDescription = ""}
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponsePortfolios [mainPortfolioView, later])
      renderedLines doc
        `shouldBe` [ "  │ Wallet   │ Portfolio   │ Network │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Coins │ Addresses │ Description",
                     "──┼──────────┼─────────────┼─────────┼────────────────┼─────────────────┼───────────────────┼───────┼───────────┼────────────",
                     "* │ wallet0  │ portfolio0  │ btc     │     0.05100000 │      0.04900000 │        0.00200000 │     3 │        17 │ \"savings\"",
                     "  │ wallet12 │ portfolio10 │ btc     │     0.05100000 │      0.04900000 │        0.00200000 │     3 │        17 │"
                   ]

newPortfolioSpec :: Spec
newPortfolioSpec = do
  describe "The portfolio newportfolio created, as its answer shows it" $ do
    -- The first portfolio of the database, which became active
    it "says it was created, naming the portfolio and its wallet, then gives the fingerprint, counts the accounts and names each network with the types of its accounts, and says it is the active portfolio" $
      renderedLines (newPortfolioDoc wholePortfolioView (ActiveAfter (PortfolioRef 0 0) True))
        `shouldBe` [ "Created portfolio0 of wallet0",
                     "  Fingerprint: bc560f95",
                     "  Accounts   : 12, on btc (bip44 and bip84), bch (bip44), btctest (bip44 and bip84), btctest4 (bip44 and bip84), btcreg (bip44 and bip84), bchtest3 (bip44), bchtest4 (bip44) and bchreg (bip44)",
                     "portfolio0 of wallet0 is the active portfolio: the commands work in it."
                   ]

    -- The configuration shows btctest4 and bchtest4 alone. The portfolio
    -- has twelve accounts, and the answer counts those it names:
    -- 2 on btctest4 + 1 on bchtest4 = 3
    it "counts the accounts of the networks shown alone, which the types it names add up to" $
      take 1 (drop 2 (renderedLines (newPortfolioDoc mainPortfolioView {viewNetworks = [(emptyOn btcTest4) {networkViewAccounts = [legacyKeys, segwitKeys]}, (emptyOn bchTest4) {networkViewAccounts = [legacyKeys]}]} (ActiveAfter (PortfolioRef 0 0) True))))
        `shouldBe` ["  Accounts   : 3, on btctest4 (bip44 and bip84) and bchtest4 (bip44)"]

    it "names bip44 before bip84, in whatever order the accounts come" $
      take 1 (drop 2 (renderedLines (newPortfolioDoc mainPortfolioView {viewNetworks = [(emptyOn btc) {networkViewAccounts = [segwitKeys, legacyKeys]}]} (ActiveAfter (PortfolioRef 0 0) True))))
        `shouldBe` ["  Accounts   : 2, on btc (bip44 and bip84)"]

    -- What hw prints, 80 columns wide. The value starts at column 15, and
    -- each network with its types is kept whole, "and" with the last one.
    it "wraps the networks under the first line of the value, between two networks, when every network is shown" $ do
      shown <- printedLines (ResponseNewPortfolio Nothing Nothing wholePortfolioView (ActiveAfter (PortfolioRef 0 0) True) Nothing)
      take 4 (drop 2 shown)
        `shouldBe` [ "  Accounts   : 12, on btc (bip44 and bip84), bch (bip44),",
                     "               btctest (bip44 and bip84), btctest4 (bip44 and bip84),",
                     "               btcreg (bip44 and bip84), bchtest3 (bip44), bchtest4 (bip44)",
                     "               and bchreg (bip44)"
                   ]

    -- What hw prints, 80 columns wide: the six lines of the portfolio, the
    -- active one, then the warning
    it "is followed by the warning of a wallet created beside another, apart and wrapped" $ do
      shown <-
        printedLines
          ( ResponseNewPortfolio
              Nothing
              Nothing
              wholePortfolioView {viewRef = PortfolioRef 1 0, viewActive = False}
              (ActiveAfter (PortfolioRef 0 0) False)
              (Just "This mnemonic and passphrase are those of no wallet here: a new wallet1 was created. If you meant wallet0, the mnemonic or the passphrase was not the same: delete wallet1 with hw deletewallet 1")
          )
      drop 6 shown
        `shouldBe` [ "The active portfolio is still portfolio0 of wallet0.",
                     "",
                     "This mnemonic and passphrase are those of no wallet here: a new wallet1 was",
                     "created. If you meant wallet0, the mnemonic or the passphrase was not the same:",
                     "delete wallet1 with hw deletewallet 1"
                   ]

    it "names wallet0 in its sentence too, as every sentence does" $
      take 1 (renderedLines (newPortfolioDoc wholePortfolioView {viewRef = PortfolioRef 0 1, viewActive = False} (ActiveAfter (PortfolioRef 0 0) False)))
        `shouldBe` ["Created portfolio1 of wallet0"]

    -- portfolio1 is added to a database where portfolio0 is active
    it "says which portfolio is still active when the one it created is not" $
      drop 3 (renderedLines (newPortfolioDoc wholePortfolioView {viewRef = PortfolioRef 0 1, viewActive = False} (ActiveAfter (PortfolioRef 0 0) False)))
        `shouldBe` ["The active portfolio is still portfolio0 of wallet0."]

  describe "The portfolio switch made the active one, as its answer shows it" $
    it "says the portfolio is the active one now, then shows it as portfolios does, marked" $ do
      shown <- printedLines (ResponseSwitched mainPortfolioView {viewRef = PortfolioRef 0 1})
      take 2 shown `shouldBe` ["The active portfolio is now portfolio1 of wallet0.", "* portfolio1  wallet0  \"savings\""]

{- Imports and files of a wallet -}

-- | What importwallet did with a file of the wallets and accounts given,
-- with nothing else: no label set, nothing missing or left aside
importedWith :: [WalletImport] -> [AccountImport] -> ImportReport
importedWith wallets accounts =
  ImportReport
    { reportWallets = wallets,
      reportAccounts = accounts,
      reportWalletLabels = 0,
      reportPortfolioLabels = 0,
      reportAddressLabels = 0,
      reportTxLabels = 0,
      reportAddressTags = 0,
      reportTxTags = 0,
      reportContacts = 0,
      reportContactsOwn = 0,
      reportAddressLabelsReplaced = 0,
      reportTxLabelsReplaced = 0,
      reportContactLabelsReplaced = 0,
      reportCoinsLocked = 0,
      reportCoinsUnlocked = 0,
      reportAddressesMissing = 0,
      reportAddressesTooFar = 0,
      reportTxsMissing = 0,
      reportCoinsMissing = 0,
      reportOtherAccounts = 0,
      reportOtherPortfolios = 0,
      reportWalletLabelsKept = 0,
      reportPortfolioLabelsKept = 0,
      reportChangeAddresses = 0,
      reportNotKept = 0,
      reportUnreadable = []
    }

-- | What an import says when it created no portfolio, in a database whose
-- active portfolio is portfolio0 of wallet0
stillActive :: Maybe ActiveAfter
stillActive = Just (ActiveAfter (PortfolioRef 0 0) False)

importSpec :: Spec
importSpec =
  describe "What importwallet did, as its answer shows it" $ do
    -- The file brings the first portfolios of the database, and portfolio0
    -- became active
    it "names each wallet of the file with its fingerprint and that it was created, then each of its portfolios with what was added, then the active portfolio, then the labels set" $
      renderedLines (importDoc (importedWith [WalletImport 0 testFingerprint True Nothing [PortfolioImport 0 True Nothing, PortfolioImport 1 True Nothing]] []) (Just (ActiveAfter (PortfolioRef 0 0) True)))
        `shouldBe` [ "wallet0  bc560f95  created",
                     "  portfolio0  created, 12 accounts added",
                     "  portfolio1  created, 12 accounts added",
                     "",
                     "portfolio0 of wallet0 is the active portfolio: the commands work in it.",
                     "",
                     "Labels set: none"
                   ]

    -- The database had portfolio0 of wallet0, the active one, and the file
    -- brings wallet1
    it "says which portfolio is still active when the portfolios it created are not" $
      renderedLines (importDoc (importedWith [WalletImport 1 testFingerprint2 True Nothing [PortfolioImport 0 True Nothing]] []) (Just (ActiveAfter (PortfolioRef 0 0) False)))
        `shouldBe` [ "wallet1  4435aa1c  created",
                     "  portfolio0  created, 12 accounts added",
                     "",
                     "The active portfolio is still portfolio0 of wallet0.",
                     "",
                     "Labels set: none"
                   ]

    -- A portfolio the database has is whole already
    it "says a wallet the database had is already in it, and that its portfolio gets nothing" $
      take 2 (renderedLines (importDoc (importedWith [WalletImport 1 testFingerprint False Nothing [PortfolioImport 0 False Nothing]] []) stillActive))
        `shouldBe` ["wallet1  bc560f95  already in the database", "  portfolio0  nothing to add"]

    it "gives each key of the file left aside with why, apart from the wallets" $
      renderedLines (importDoc (importedWith [] [AccountLeftAside "xpub6CHK45s4QJWp" "The record names no network"]) stillActive)
        `shouldBe` ["xpub6CHK45s4QJWp", "  not imported: The record names no network", "", "Labels set: none"]

    it "counts the labels set, and the coins locked and unlocked" $
      renderedLines (importDoc ((importedWith [] []) {reportAddressLabels = 2, reportTxLabels = 1, reportCoinsLocked = 1, reportCoinsUnlocked = 3}) stillActive)
        `shouldBe` ["Labels set: 2 addresses, 1 transaction", "Coins locked: 1", "Coins unlocked: 3"]

    it "counts the labels set on wallets and portfolios before those of addresses and transactions" $
      renderedLines (importDoc ((importedWith [] []) {reportWalletLabels = 1, reportPortfolioLabels = 2, reportAddressLabels = 2, reportTxLabels = 1}) stillActive)
        `shouldBe` ["Labels set: 1 wallet, 2 portfolios, 2 addresses, 1 transaction"]

    it "counts the labels of wallets and portfolios that keep their own description among those left aside" $
      renderedLines (importDoc ((importedWith [] []) {reportWalletLabelsKept = 1, reportPortfolioLabelsKept = 2}) stillActive)
        `shouldBe` ["Labels set: none", "Left aside: 1 label of a wallet that keeps its own, 2 labels of portfolios that keep their own"]

    -- Of the 2 addresses labelled, 1 had another label before
    it "counts the labels of the database that a label of the file took the place of, under the labels set" $
      renderedLines (importDoc ((importedWith [] []) {reportAddressLabels = 2, reportAddressLabelsReplaced = 1, reportTxLabelsReplaced = 2, reportContactLabelsReplaced = 1}) stillActive)
        `shouldBe` ["Labels set: 2 addresses", "Labels replaced: 1 address, 2 transactions, 1 contact"]

    it "has no line for the labels replaced when the file replaced none" $
      renderedLines (importDoc ((importedWith [] []) {reportAddressLabels = 2}) stillActive)
        `shouldBe` ["Labels set: 2 addresses"]

    -- The labels of a wallet and of a portfolio are counted, not written
    it "does not write the descriptions the file gives a wallet and a portfolio, which it counts" $
      take 2 (renderedLines (importDoc (importedWith [WalletImport 0 testFingerprint False (Just (LabelImport "family" Nothing)) [PortfolioImport 0 False (Just (LabelImport "daily spending" (Just "offline copy")))]] []) stillActive))
        `shouldBe` [ "wallet0  bc560f95  already in the database",
                     "  portfolio0  nothing to add"
                   ]

    it "counts what the file labels that the wallet does not have yet" $
      renderedLines (importDoc ((importedWith [] []) {reportAddressesMissing = 1, reportTxsMissing = 2, reportCoinsMissing = 1}) stillActive)
        `shouldBe` ["Labels set: none", "Not in the wallet yet: 1 address, 2 transactions, 1 coin"]

    it "counts the records of the accounts the wallet does not have, and of the portfolios --wallet or --portfolio leaves out, among those left aside" $
      renderedLines (importDoc ((importedWith [] []) {reportOtherAccounts = 2, reportOtherPortfolios = 3}) stillActive)
        `shouldBe` ["Labels set: none", "Left aside: 2 records of accounts the wallet does not have, 3 records of the portfolios --wallet or --portfolio leaves out"]

    it "gives the lines of the file that are not records" $
      renderedLines (importDoc ((importedWith [] []) {reportUnreadable = [4, 7]}) stillActive)
        `shouldBe` ["Labels set: none", "Lines that are not records: 4, 7"]

-- | The id of a transaction, as a record of a file writes it
fileTxid :: Text
fileTxid = "0000000000000000000000000000000000000000000000000000000000000001"

-- | A file of a wallet as show shows it, of the wallets and the accounts
-- given, with what importwallet would do with them, and nothing else. No
-- record of an account replaces a label of the database, and the report
-- of the import counts nothing: the view ends with "Labels set: none".
fileOf :: [WalletImport] -> [(AccountImport, [Record])] -> FileShown
fileOf wallets accounts = FileShown (importedWith wallets (fst <$> accounts)) [(outcome, replacingNone <$> records) | (outcome, records) <- accounts] [] []

-- | A record of a file as show shows it, which importwallet would set,
-- and which replaces no label of the database
replacingNone :: Record -> FileRecord
replacingNone r = FileRecord r [] FateSet

-- | Wallet 0 with the fingerprint of the test wallet, which the database
-- has, and its portfolio 0, which lacks nothing the file holds
heldWallet :: WalletImport
heldWallet = WalletImport 0 testFingerprint False Nothing [PortfolioImport 0 False Nothing]

-- | A database file as show shows it, at format version 5, of the
-- wallets given
databaseOf :: [WalletShown] -> DatabaseShown
databaseOf = DatabaseShown 5

-- | Wallet 0 of a database file, with the fingerprint of the test wallet
-- and no description, and the portfolios given
shownWallet :: [PortfolioShown] -> WalletShown
shownWallet = WalletShown 0 testFingerprint ""

-- | Portfolio 0 of a wallet of a database file, the active one, without
-- a description, created at 'seenTime'
shownPortfolio :: PortfolioShown
shownPortfolio = PortfolioShown 0 True "" seenTime

databaseFileSpec :: Spec
databaseFileSpec =
  describe "A database file as show shows it" $ do
    -- A file of a wallet holds wallets and portfolios too, so the first
    -- words tell the two apart
    it "says first that it is an SQLite database, with its name, then its format version and what it holds" $
      take 2 (renderedLines (databaseFileDoc "copy.sqlite" (databaseOf [shownWallet [shownPortfolio]]) [[seenTimeInCet]]))
        `shouldBe` [ "SQLite database  copy.sqlite",
                     "  A database of hw at format version 5, with 1 wallet and 1 portfolio"
                   ]

    -- 2 wallets, of 2 portfolios and of 1: 3 portfolios
    it "counts the wallets, and the portfolios of all of them together" $
      let both = [shownWallet [shownPortfolio, shownPortfolio {portfolioShownIndex = 1}], (shownWallet [shownPortfolio]) {walletShownIndex = 1}]
       in (renderedLines (databaseFileDoc "copy.sqlite" (databaseOf both) [[seenTimeInCet, seenTimeInCet], [seenTimeInCet]]) !! 1)
            `shouldBe` "  A database of hw at format version 5, with 2 wallets and 3 portfolios"

    it "says of a database without a wallet that it has none, and shows nothing under its heading" $
      renderedLines (databaseFileDoc "copy.sqlite" (databaseOf []) [])
        `shouldBe` [ "SQLite database  copy.sqlite",
                     "  A database of hw at format version 5, with no wallet"
                   ]

    -- The clock given reads 14 Nov 2023 at 'seenTime'
    it "gives a wallet by its name and the fingerprint of its seed, then each of its portfolios under it, with the day it was created" $
      drop 3 (renderedLines (databaseFileDoc "copy.sqlite" (databaseOf [shownWallet [shownPortfolio]]) [[seenTimeInCet]]))
        `shouldBe` [ "wallet0  bc560f95",
                     "  * portfolio0  created 14 Nov 2023"
                   ]

    it "gives the description of a wallet after its fingerprint, and that of a portfolio after the day it was created" $
      let described = (shownWallet [shownPortfolio {portfolioShownDescription = "Savings"}]) {walletShownDescription = "Family"}
       in drop 3 (renderedLines (databaseFileDoc "copy.sqlite" (databaseOf [described]) [[seenTimeInCet]]))
            `shouldBe` [ "wallet0  bc560f95  \"Family\"",
                         "  * portfolio0  created 14 Nov 2023  \"Savings\""
                       ]

    -- The mark takes a column of its own, so that the names line up
    it "marks the active portfolio with a *, and starts the others with a space where the mark goes" $
      let two = shownWallet [shownPortfolio {portfolioShownActive = False}, shownPortfolio {portfolioShownIndex = 1}]
       in drop 4 (renderedLines (databaseFileDoc "copy.sqlite" (databaseOf [two]) [[seenTimeInCet, seenTimeInCet]]))
            `shouldBe` [ "    portfolio0  created 14 Nov 2023",
                         "  * portfolio1  created 14 Nov 2023"
                       ]

    -- portfolio9 has 10 characters and portfolio10 has 11
    it "pads the names of the portfolios of a wallet to the longest, so that the days line up" $
      let two = shownWallet [shownPortfolio {portfolioShownIndex = 9}, shownPortfolio {portfolioShownIndex = 10, portfolioShownActive = False}]
       in drop 4 (renderedLines (databaseFileDoc "copy.sqlite" (databaseOf [two]) [[seenTimeInCet, seenTimeInCet]]))
            `shouldBe` [ "  * portfolio9   created 14 Nov 2023",
                         "    portfolio10  created 14 Nov 2023"
                       ]

    it "puts a blank line between two wallets" $
      let both = [shownWallet [shownPortfolio], (shownWallet [shownPortfolio {portfolioShownActive = False}]) {walletShownIndex = 1, walletShownFingerprint = testFingerprint2}]
       in drop 3 (renderedLines (databaseFileDoc "copy.sqlite" (databaseOf both) [[seenTimeInCet], [seenTimeInCet]]))
            `shouldBe` [ "wallet0  bc560f95",
                         "  * portfolio0  created 14 Nov 2023",
                         "",
                         "wallet1  4435aa1c",
                         "    portfolio0  created 14 Nov 2023"
                       ]

    -- A description comes from the database of the file, which may be
    -- anyone's
    it "writes out an escape in a description, never as an order to the terminal" $
      let described = (shownWallet [shownPortfolio]) {walletShownDescription = "Fam\ESC[2Jily"}
       in (renderedLines (databaseFileDoc "copy.sqlite" (databaseOf [described]) [[seenTimeInCet]]) !! 3)
            `shouldBe` "wallet0  bc560f95  \"Fam\\x1b[2Jily\""

walletFileSpec :: Spec
walletFileSpec =
  describe "A file of a wallet as show shows it" $ do
    -- A database file holds wallets and portfolios too, so the first
    -- words tell the two apart
    it "says first that it is a BIP 329 file, with its name, in the answer of show" $
      renderedLines (walletFileAnswerDoc "wallet.jsonl" (fileOf [heldWallet] []))
        `shouldBe` [ "BIP 329 file  wallet.jsonl",
                     "  A file of a wallet, shown as importwallet would take it",
                     "",
                     "wallet0  bc560f95  in the database",
                     "  portfolio0  nothing to add",
                     "",
                     "Labels set: none"
                   ]

    it "gives each wallet of the file, with what importwallet would do with it and with each of its portfolios" $
      renderedLines (walletFileDoc (fileOf [WalletImport 0 testFingerprint True Nothing [PortfolioImport 0 True Nothing]] []))
        `shouldBe` ["wallet0  bc560f95  not in the database, and importwallet creates it", "  portfolio0  importwallet creates it, 12 accounts to add", "", "Labels set: none"]

    it "says of a wallet the database has that it is in it" $
      renderedLines (walletFileDoc (fileOf [heldWallet] []))
        `shouldBe` ["wallet0  bc560f95  in the database", "  portfolio0  nothing to add", "", "Labels set: none"]

    it "gives an account of the file by where it is, then its labels under a heading for each kind" $
      renderedLines
        ( walletFileDoc $
            fileOf
              [heldWallet]
              [ ( AccountHeld mainAccountRef,
                  [ (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Wages", recordKeypath = Just "/0/0"},
                    (record TxRecord fileTxid) {recordLabel = Just "Salary"},
                    (record OutputRecord (fileTxid <> ":0")) {recordSpendable = Just False}
                  ]
                )
              ]
        )
        `shouldBe` [ "wallet0  bc560f95  in the database",
                     "  portfolio0  nothing to add",
                     "",
                     "portfolio0  btc  bip44",
                     "  Address labels",
                     "    bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages\"",
                     "  Transaction labels",
                     "    0000000000000000000000000000000000000000000000000000000000000001  \"Salary\"",
                     "  Coins",
                     "    0000000000000000000000000000000000000000000000000000000000000001:0  locked",
                     "",
                     "Labels set: none"
                   ]

    -- A whole portfolio has twelve accounts, most without a label
    it "leaves out an account of the file without labels or locks" $
      renderedLines (walletFileDoc (fileOf [heldWallet] [(AccountAdded segwitAccountRef, [])]))
        `shouldBe` ["wallet0  bc560f95  in the database", "  portfolio0  nothing to add", "", "Labels set: none"]

    it "gives why importwallet would leave a key aside, under the start of the key" $
      renderedLines (walletFileDoc (fileOf [] [(AccountLeftAside "xpub6CHK45s4QJWp" "The record names no network", [])]))
        `shouldBe` ["xpub6CHK45s4QJWp", "  importwallet leaves it aside: The record names no network", "", "Labels set: none"]

    it "says a coin the file unlocks is unlocked" $
      renderedLines (walletFileDoc (fileOf [] [(AccountHeld mainAccountRef, [(record OutputRecord (fileTxid <> ":1")) {recordSpendable = Just True}])]))
        `shouldBe` [ "portfolio0  btc  bip44",
                     "  Coins",
                     "    0000000000000000000000000000000000000000000000000000000000000001:1  unlocked",
                     "",
                     "Labels set: none"
                   ]

    -- A file of wallet0 and wallet1, each with a coin to unlock in the
    -- bip44 account of its portfolio0 on btc. portfolio0 alone could be
    -- of either wallet.
    it "names the wallet of each account when the file has several wallets" $ do
      let unlocked = [(record OutputRecord (fileTxid <> ":1")) {recordSpendable = Just True}]
          newWallet = WalletImport 1 testFingerprint2 True Nothing [PortfolioImport 0 True Nothing]
          shown = walletFileDoc (fileOf [heldWallet, newWallet] [(AccountHeld mainAccountRef, unlocked), (AccountAdded (AccountRef (Place (PortfolioRef 1 0) btc) Bip44), unlocked)])
      filter (Text.isPrefixOf "portfolio" . Text.pack) (renderedLines shown)
        `shouldBe` ["portfolio0 of wallet0  btc  bip44", "portfolio0 of wallet1  btc  bip44"]

    -- The first line of the view names the one wallet of the file
    it "leaves the wallet out of the line of an account when the file has one wallet, even when it is not wallet0" $ do
      let unlocked = [(record OutputRecord (fileTxid <> ":1")) {recordSpendable = Just True}]
          onlyWallet = WalletImport 1 testFingerprint2 True Nothing [PortfolioImport 0 True Nothing]
          shown = walletFileDoc (fileOf [onlyWallet] [(AccountAdded (AccountRef (Place (PortfolioRef 1 0) btc) Bip44), unlocked)])
      filter (Text.isPrefixOf "portfolio" . Text.pack) (renderedLines shown)
        `shouldBe` ["portfolio0  btc  bip44"]

    -- bip44/0/10 is the longest path, ten characters with the type of
    -- the account
    it "pads the key paths of the addresses to the longest, and gives an address without one after the padding" $
      renderedLines
        ( walletFileDoc $
            fileOf
              []
              [ ( AccountHeld mainAccountRef,
                  [ (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Wages", recordKeypath = Just "/0/0"},
                    (record AddrRecord "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg") {recordLabel = Just "From Bob", recordKeypath = Just "/0/10"},
                    (record AddrRecord "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1") {recordLabel = Just "Rent"}
                  ]
                )
              ]
        )
        `shouldBe` [ "portfolio0  btc  bip44",
                     "  Address labels",
                     "    bip44/0/0   1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages\"",
                     "    bip44/0/10  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  \"From Bob\"",
                     "                1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1  \"Rent\"",
                     "",
                     "Labels set: none"
                   ]

    -- The record names no account of the file, so nothing says whether
    -- /0/0 is that of a bip44 or of a bip84 account
    it "gives the key path of a label of no account alone, without a type" $
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [replacingNone (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Wages", recordKeypath = Just "/0/0"}]})
        `shouldBe` [ "Labels without an account in the file",
                     "  Address labels",
                     "    /0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages\"",
                     "",
                     "Labels set: none"
                   ]

    -- importwallet would set the label of 1 address. The wallet does not
    -- have 1 other address, and the file has the label of 1 change
    -- address, 2 records hw keeps no label for, and two lines that are
    -- not records.
    it "ends with the counts importwallet would end its answer with, in the same words" $
      renderedLines (walletFileDoc (fileOf [] []) {shownReport = (importedWith [] []) {reportAddressLabels = 1, reportAddressesMissing = 1, reportChangeAddresses = 1, reportNotKept = 2, reportUnreadable = [4, 7]}})
        `shouldBe` [ "Labels set: 1 address",
                     "Not in the wallet yet: 1 address",
                     "Left aside: 1 label of change addresses, 2 records hw keeps no label for",
                     "Lines that are not records: 4, 7"
                   ]

    it "gives the labels of no account of the file last, before the counts" $
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [replacingNone (record TxRecord fileTxid) {recordLabel = Just "Salary"}]})
        `shouldBe` [ "Labels without an account in the file",
                     "  Transaction labels",
                     "    0000000000000000000000000000000000000000000000000000000000000001  \"Salary\"",
                     "",
                     "Labels set: none"
                   ]

    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is an address of another wallet
    it "says of a label of an address the wallet does not have that it is not in the wallet yet, at the end of its line" $
      take 3 (renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord (record AddrRecord "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") {recordLabel = Just "Landlord"} [] FateNotInWallet]}))
        `shouldBe` [ "Labels without an account in the file",
                     "  Address labels",
                     "    1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  \"Landlord\"  not in the wallet yet"
                   ]

    it "says of a lock of a coin the wallet does not have that it is not in the wallet yet, after the lock" $
      take 3 (renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord (record OutputRecord (fileTxid <> ":1")) {recordSpendable = Just False} [] FateNotInWallet]}))
        `shouldBe` [ "Labels without an account in the file",
                     "  Coins",
                     "    0000000000000000000000000000000000000000000000000000000000000001:1  locked  not in the wallet yet"
                   ]

    it "says of a record of an account of another wallet that it is left aside, since the wallet does not have its account" $
      take 3 (renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord (record TxRecord fileTxid) {recordLabel = Just "Salary"} [] FateOtherAccount]}))
        `shouldBe` [ "Labels without an account in the file",
                     "  Transaction labels",
                     "    0000000000000000000000000000000000000000000000000000000000000001  \"Salary\"  left aside: the wallet does not have its account"
                   ]

    it "ends the line of a wallet and of a portfolio with the description the file gives it" $
      renderedLines (walletFileDoc (fileOf [WalletImport 0 testFingerprint True (Just (LabelImport "family" Nothing)) [PortfolioImport 0 True (Just (LabelImport "daily spending" Nothing))]] []))
        `shouldBe` [ "wallet0  bc560f95  not in the database, and importwallet creates it  \"family\"",
                     "  portfolio0  importwallet creates it, 12 accounts to add  \"daily spending\"",
                     "",
                     "Labels set: none"
                   ]

    it "says of a description of the file that it is left aside when the wallet or the portfolio keeps its own, which it names" $
      renderedLines (walletFileDoc (fileOf [WalletImport 0 testFingerprint False (Just (LabelImport "family" (Just "ours"))) [PortfolioImport 0 False (Just (LabelImport "daily spending" (Just "offline copy")))]] []))
        `shouldBe` [ "wallet0  bc560f95  in the database  \"family\"  left aside: it keeps \"ours\"",
                     "  portfolio0  nothing to add  \"daily spending\"  left aside: it keeps \"offline copy\"",
                     "",
                     "Labels set: none"
                   ]

    it "names after a label of the file the label of the database it would replace" $
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Wages 2027", recordKeypath = Just "/0/0"} ["Wages, renamed offline"] FateSet]})
        `shouldBe` [ "Labels without an account in the file",
                     "  Address labels",
                     "    /0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages 2027\"  replaces \"Wages, renamed offline\"",
                     "",
                     "Labels set: none"
                   ]

    -- Two portfolios hold the transaction, each with a label of its own
    it "names each label a label of a transaction would replace, one for each portfolio that has another" $
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord (record TxRecord fileTxid) {recordLabel = Just "Salary"} ["Pay", "From work"] FateSet]})
        `shouldBe` [ "Labels without an account in the file",
                     "  Transaction labels",
                     "    0000000000000000000000000000000000000000000000000000000000000001  \"Salary\"  replaces \"Pay\" and \"From work\"",
                     "",
                     "Labels set: none"
                   ]

    -- The label comes before the tags, and what it replaces between them
    it "names the label a label would replace before the tags of the record" $
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Wages", recordTags = Just ["rent"]} ["Pay"] FateSet]})
        `shouldBe` [ "Labels without an account in the file",
                     "  Address labels and tags",
                     "    1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages\"  replaces \"Pay\"  [rent]",
                     "",
                     "Labels set: none"
                   ]

    -- A record with an empty label takes the label off its address
    it "says of a record with an empty label that it removes the label of the database" $
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just ""} ["Pay"] FateSet]})
        `shouldBe` [ "Labels without an account in the file",
                     "  Address labels",
                     "    1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  removes \"Pay\"",
                     "",
                     "Labels set: none"
                   ]

    it "counts the records of the portfolios --wallet or --portfolio leaves out, which importwallet would not read" $
      renderedLines (walletFileDoc (fileOf [] []) {shownReport = (importedWith [] []) {reportOtherPortfolios = 3}})
        `shouldBe` ["Labels set: none", "Left aside: 3 records of the portfolios --wallet or --portfolio leaves out"]

    -- What a file holds may be anything, an escape to clear the screen too
    it "writes out an escape in a reference of the file, never as an order to the terminal" $
      renderedLines (walletFileDoc (fileOf [] [(AccountHeld mainAccountRef, [(record AddrRecord "1KEn\ESC[2J") {recordLabel = Just "Wages"}])]))
        `shouldBe` [ "portfolio0  btc  bip44",
                     "  Address labels",
                     "    1KEn\\x1b[2J  \"Wages\"",
                     "",
                     "Labels set: none"
                   ]

{- Addresses -}

addressSpec :: Spec
addressSpec =
  describe "An address as hw shows it" $ do
    it "gives its path, the address and its label, then what its coins are worth and what it received" $
      renderedLines (compactAddressDoc btc UnitBitcoin 9 paidAddressRow (Holds (CoinTotals 50000 50000)) [])
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  total 0.00050000 btc, received 0.00050000 btc, 1 transaction"
                   ]

    -- The coin of 50000 is locked, as lockcoin locks it: the address
    -- still holds it, and can spend nothing
    it "says after the total what the address cannot spend, when its coin is locked or waits" $
      drop 1 (renderedLines (compactAddressDoc btc UnitBitcoin 9 paidAddressRow (Holds (CoinTotals 50000 0)) []))
        `shouldBe` ["  total 0.00050000 btc, unspendable 0.00050000 btc, received 0.00050000 btc, 1 transaction"]

    -- Two coins hold 80000, and one of 30000 is locked:
    -- 80000 - 50000 = 30000
    it "counts as unspendable the part of the total that the address cannot spend now" $
      drop 1 (renderedLines (compactAddressDoc btc UnitBitcoin 9 paidAddressRow {dBAddressBalanceConfirmed = 80000, dBAddressBalanceCoins = 2, dBAddressBalanceTxs = 2, dBAddressBalanceReceived = 80000} (Holds (CoinTotals 80000 50000)) []))
        `shouldBe` ["  total 0.00080000 btc, unspendable 0.00030000 btc, received 0.00080000 btc, 2 transactions"]

    -- The address was paid 50000 once, and a payment spent that coin: two
    -- transactions, of which one paid it
    it "gives an address that a payment emptied a total of zero, and counts its transactions apart from what it received, since some of them spent from it" $
      drop 1 (renderedLines (compactAddressDoc btc UnitBitcoin 9 paidAddressRow {dBAddressBalanceConfirmed = 0, dBAddressBalanceCoins = 0, dBAddressBalanceTxs = 2} (Holds (CoinTotals 0 0)) []))
        `shouldBe` ["  total 0.00000000 btc, received 0.00050000 btc, 2 transactions"]

    it "says an address no transaction has paid is unused" $
      renderedLines (compactAddressDoc btc UnitBitcoin 9 unusedAddressRow (Holds (CoinTotals 0 0)) [])
        `shouldBe` [ "bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
                     "  unused"
                   ]

    -- hw asked no server about the address: its row counts no
    -- transaction, and the chain may hold some
    it "says an address of a portfolio that never synced is not synced, and not that it is unused" $
      renderedLines (compactAddressDoc btc UnitBitcoin 9 unusedAddressRow NotSynced [])
        `shouldBe` [ "bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
                     "  not synced"
                   ]

    -- A path of 5 characters, such as /0/10, is listed too
    it "pads its path to the width given, so that the addresses line up" $
      (renderedLines (compactAddressDoc btc UnitBitcoin 10 unusedAddressRow (Holds (CoinTotals 0 0)) []) !! 0)
        `shouldBe` "bip44/0/1   1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"

    it "gives every detail with --verbose, the time it was created on the clock given" $
      renderedLines (verboseAddressDoc ownPathsAlone btc UnitBitcoin 9 seenTimeInCet paidAddressRow (Holds (CoinTotals 50000 50000)) [])
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  Transactions: 1",
                     "  Received    : 0.00050000 btc",
                     "  Total       : 0.00050000 btc",
                     "  Spendable   : 0.00050000 btc",
                     "  Unspendable : 0.00000000 btc",
                     "  Confirmed   : 0.00050000 btc",
                     "  Unconfirmed : 0.00000000 btc",
                     "  Coins       : 1",
                     "  Created     : 14 Nov 2023 23:13:20 CET"
                   ]

    it "gives with --verbose no amount and no count for an address of a portfolio that never synced, and says that it is not synced" $
      renderedLines (verboseAddressDoc ownPathsAlone btc UnitBitcoin 9 seenTimeInCet unusedAddressRow NotSynced [])
        `shouldBe` [ "bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
                     "  Balance     : not synced",
                     "  Created     : 14 Nov 2023 23:13:20 CET"
                   ]

    -- Two coins hold 80000, and one of 30000 is locked:
    -- 80000 - 50000 = 30000
    it "gives with --verbose what the coins of the address are worth, what it can spend of them and the rest" $
      take 3 (drop 3 (renderedLines (verboseAddressDoc ownPathsAlone btc UnitBitcoin 9 seenTimeInCet paidAddressRow (Holds (CoinTotals 80000 50000)) [])))
        `shouldBe` [ "  Total       : 0.00080000 btc",
                     "  Spendable   : 0.00050000 btc",
                     "  Unspendable : 0.00030000 btc"
                   ]

    -- receive answers with the address of an account of portfolio1 of
    -- wallet2, in a database of several wallets. The fields above the
    -- last are those of --verbose, whose time is on the clock of the
    -- computer.
    it "ends the address an answer gives with its account: its portfolio, its network and its type" $ do
      shown <- addressAnswerDoc WithWallets WithoutCode UnitBitcoin (Just otherRef) (AccountRef (Place otherRef btc) Bip84) segwitAddressRow (Holds (CoinTotals 0 0)) [] Nothing
      last (renderedLines shown) `shouldBe` "  Account     : portfolio1 of wallet2  btc  bip84"

    -- tag was given the address /0/0, whose coin a payment spent: 27740
    -- of change went to /1/0, which the tag followed. The lines come
    -- after the fields of the address, of which the account is the last.
    it "lists the change addresses that took the tags of tag with the address, after an empty line" $ do
      let changeRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = internalAddrsText !! 0, dBAddressLabel = "", dBAddressBalanceReceived = 27740}
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just mainPortfolioRef) mainAccountRef paidAddressRow (Holds (CoinTotals 0 0)) [plainTag kycTag] (Just (ChangeFollowed AddTags [(changeRow, Holds (CoinTotals 27740 27740), [plainTag kycTag])] []))
      dropWhile (/= "") (renderedLines shown)
        `shouldBe` [ "",
                     "Tagged with it, down the chain of its payments: 1 change address",
                     "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  [kyc]",
                     "  total 0.00027740 btc, received 0.00027740 btc, 1 transaction"
                   ]

    -- The change at /1/0 could not take the tag: the payment that pays
    -- it, of the short id 00000000, also spends a coin at /0/1, which has
    -- no tag kyc
    it "lists apart the change addresses where the tags stopped, each with the payment and the address that stopped them" $ do
      let changeRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = internalAddrsText !! 0, dBAddressLabel = "", dBAddressBalanceReceived = 27740}
          sourceRow = paidAddressRow {dBAddressIndex = 1, dBAddressAddress = "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", dBAddressLabel = ""}
          stop = ChangeStop (changeRow, Holds (CoinTotals 27740 27740), []) [plainTag kycTag] (fakeTxHash 7) (Just (sourceRow, Holds (CoinTotals 0 0), []))
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just mainPortfolioRef) mainAccountRef paidAddressRow (Holds (CoinTotals 0 0)) [plainTag kycTag] (Just (ChangeFollowed AddTags [] [stop]))
      dropWhile (/= "") (renderedLines shown)
        `shouldBe` [ "",
                     "No change address down the chain of its payments was tagged with it",
                     "",
                     "Not tagged, where the chain stops: 1 change address",
                     "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn",
                     "  total 0.00027740 btc, received 0.00027740 btc, 1 transaction",
                     "  The payment 00000000 also spends a coin at bip44/0/1, without the tag kyc"
                   ]

    -- The coin from outside the group is not of the portfolio: it has no
    -- address to tag
    it "says of a stop whose other coin is not of the portfolio that the coin has no tag" $ do
      let changeRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = internalAddrsText !! 0, dBAddressLabel = "", dBAddressBalanceReceived = 27740}
          stop = ChangeStop (changeRow, Holds (CoinTotals 27740 27740), []) [plainTag coldTag, plainTag kycTag] (fakeTxHash 7) Nothing
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just mainPortfolioRef) mainAccountRef paidAddressRow (Holds (CoinTotals 0 0)) [plainTag kycTag] (Just (ChangeFollowed AddTags [] [stop]))
      last (renderedLines shown) `shouldBe` "  The payment 00000000 also spends a coin that is not of the portfolio, and has no tag"

    -- The same address after untag: the change address has no tag left
    it "says the change addresses lost the tags of untag with the address" $ do
      let changeRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = internalAddrsText !! 0, dBAddressLabel = "", dBAddressBalanceReceived = 27740}
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just mainPortfolioRef) mainAccountRef paidAddressRow (Holds (CoinTotals 0 0)) [] (Just (ChangeFollowed RemoveTags [(changeRow, Holds (CoinTotals 27740 27740), [])] []))
      dropWhile (/= "") (renderedLines shown)
        `shouldBe` [ "",
                     "Untagged with it, down the chain of its payments: 1 change address",
                     "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn",
                     "  total 0.00027740 btc, received 0.00027740 btc, 1 transaction"
                   ]

    -- show finds an address of portfolio0 while portfolio1 is active
    it "says the account of the address is not of the active portfolio, and names the active one, when another one is active" $ do
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just (PortfolioRef 0 1)) segwitAccountRef segwitAddressRow (Holds (CoinTotals 0 0)) [] Nothing
      last (renderedLines shown) `shouldBe` "  Account     : portfolio0, not the active portfolio1  btc  bip84"

    -- The address /0/3 of the bip44 account, which the wallet has not
    -- handed out, so it has no label and no balance. The note is on one
    -- line, since the test lays it out with no width.
    it "gives the address show derives at a path the wallet does not hold cut under its path, then its account, then what it is for" $
      renderedLines (derivedAddrDoc WithoutWallets mainAccountRef (Deriv :/ 0 :/ 3) "1yQZuJjA6w7h.........Cv22rKCas7F1")
        `shouldBe` [ "bip44/0/3  1yQZuJjA6w7h.........Cv22rKCas7F1",
                     "  Account  : portfolio0  btc  bip44",
                     "",
                     "Compare both ends with the address that receive gave out at bip44/0/3 on the online computer. If they match, that address is of this account, and you can give it out. If they differ, the online computer may have been tampered with, and you should not give the address out. Nobody can make another address with the same ends. This screen is for checking only, so the middle of the address is hidden. Give out the address as receive showed it."
                   ]

    -- The rows of # stand for the rows of a code
    it "goes above its QR code, which starts on the next line" $
      renderedLines (withCodeDoc (verboseAddressDoc ownPathsAlone btc UnitBitcoin 9 seenTimeInCet paidAddressRow (Holds (CoinTotals 50000 50000)) []) ["####", "####"])
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  Transactions: 1",
                     "  Received    : 0.00050000 btc",
                     "  Total       : 0.00050000 btc",
                     "  Spendable   : 0.00050000 btc",
                     "  Unspendable : 0.00000000 btc",
                     "  Confirmed   : 0.00050000 btc",
                     "  Unconfirmed : 0.00000000 btc",
                     "  Coins       : 1",
                     "  Created     : 14 Nov 2023 23:13:20 CET",
                     "####",
                     "####"
                   ]

balanceSpec :: Spec
balanceSpec = do
  describe "An address as balances shows it" $ do
    -- 80000 in coins, of which 50000 can be spent now: 30000 cannot, which
    -- a pending transaction spends
    it "gives its path, the address and its label, then its total, spendable and unspendable balances" $
      renderedLines (coinBalanceDoc btc UnitBitcoin 9 paidAddressRow (CoinBalance 80000 50000 30000 0 0 0) [])
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc"
                   ]

    -- balances lists both kinds of addresses, where a change address takes
    -- no label. addresses lists one kind at a time, and says nothing.
    it "says a change address is one, where its label would be, which addresses does not" $ do
      let changeRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = internalAddrsText !! 0, dBAddressLabel = ""}
      (renderedLines (coinBalanceDoc btc UnitBitcoin 9 changeRow (CoinBalance 80000 50000 30000 0 0 0) []) !! 0)
        `shouldBe` "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  change address"
      (renderedLines (compactAddressDoc btc UnitBitcoin 9 changeRow (Holds (CoinTotals 50000 50000)) []) !! 0)
        `shouldBe` "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"

  -- Four coins hold 80000: 50000 can be spent, 20000 a pending
  -- transaction spends, 6000 the user locked, and the rest is unconfirmed:
  -- 80000 - 50000 - 20000 - 6000 = 4000
  describe "An address as balances --verbose shows it" $ do
    let fourCoins = paidAddressRow {dBAddressBalanceCoins = 4}

    it "gives its first line, then its total, its coins, its spendable amount, and its unspendable amount in four parts" $
      renderedLines (verboseBalanceDoc btc UnitBitcoin 9 fourCoins (CoinBalance 80000 50000 20000 6000 0 0) [])
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  Total            : 0.00080000 btc",
                     "  Coins            : 4",
                     "  Spendable        : 0.00050000 btc",
                     "  Locked by tx     : 0.00020000 btc",
                     "  Locked by you    : 0.00006000 btc",
                     "  Locked by tag    : 0.00000000 btc",
                     "  Not spendable yet: 0.00004000 btc"
                   ]

    it "gives the parts of the unspendable amount even when they hold nothing" $
      drop 4 (renderedLines (verboseBalanceDoc btc UnitBitcoin 9 paidAddressRow (CoinBalance 50000 50000 0 0 0 0) []))
        `shouldBe` [ "  Locked by tx     : 0.00000000 btc",
                     "  Locked by you    : 0.00000000 btc",
                     "  Locked by tag    : 0.00000000 btc",
                     "  Not spendable yet: 0.00000000 btc"
                   ]

    -- Of 80000, 50000 can be spent and 30000 hold tokens
    it "gives what is locked by tokens too, at an address with a coin that holds tokens" $
      drop 4 (renderedLines (verboseBalanceDoc btc UnitBitcoin 9 paidAddressRow (CoinBalance 80000 50000 0 0 0 30000) []))
        `shouldBe` [ "  Locked by tx     : 0.00000000 btc",
                     "  Locked by you    : 0.00000000 btc",
                     "  Locked by tag    : 0.00000000 btc",
                     "  Locked by tokens : 0.00030000 btc",
                     "  Not spendable yet: 0.00000000 btc"
                   ]

  -- The specification: balances gives a total for each network
  describe "The line above the addresses balances lists on a network" $
    it "says where they are, then the total, spendable and unspendable amounts of the network" $
      renderedLines (balanceTitle WithoutWallets UnitBitcoin mainPlace (CoinBalance 130000 100000 30000 0 0 0))
        `shouldBe` ["portfolio0  btc  total 0.00130000 btc, spendable 0.00100000 btc, unspendable 0.00030000 btc"]

{- Coins -}

-- The coin of 'spendableCoinRow' at the first change address,
-- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, which takes no label
atChange :: JsonCoin
atChange = spendableCoinRow {jsonCoinAddress = internalAddr 0, jsonCoinChange = True, jsonCoinPath = Just (legacyPath (Deriv :/ 1 :/ 0))}

coinSpec :: Spec
coinSpec = do
  describe "A coin as hw shows it" $ do
    -- 22:13 UTC is 23:13 CET. spendableCoinRow is output 0 of transaction
    -- 1, whose txid starts with 00000000.
    it "gives the short id of the coin first, the time of its transaction, its value, its state and its confirmations, then its address in full with its path" $
      renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow)
        `shouldBe` [ "00000000:0  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0"
                   ]

    it "gives the short id of the coin as the short id of its txid, a colon and its index" $
      take 1 (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1}))
        `shouldBe` ["a1075db5:1  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations"]

    -- "a1075db5:12" takes 11 characters, one more than its column
    it "lets the short id of a coin past the tenth output of its transaction push the rest of its line" $
      take 1 (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 12}))
        `shouldBe` ["a1075db5:12  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations"]

    -- The one view that keeps the label of an address in full on its line
    it "gives the label of its address on the line of the address, after its path" $
      drop 1 (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinAddressLabel = "Wages"}))
        `shouldBe` ["  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0  \"Wages\""]

    -- A coin has no tags of its own: it shows those of its address
    it "follows the label of its address with the tags of the address, each between brackets" $
      drop 1 (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinAddressLabel = "Wages", jsonCoinAddressTags = [plainTag coldTag, plainTag kycTag]}))
        `shouldBe` ["  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0  \"Wages\"  [cold] [kyc]"]

    it "gives a long label of its address whole, at the end of the line, without wrapping it" $
      drop 1 (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"}))
        `shouldBe` ["  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0  \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "leaves out the label of the transaction that paid it, which the verbose view gives" $
      renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinTxLabel = "Salary"})
        `shouldBe` [ "00000000:0  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0"
                   ]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address, which
    -- takes no label
    it "says a coin at a change address is at one, on the line of the address" $
      drop 1 (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) atChange))
        `shouldBe` ["  at 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  bip44/1/0  change address"]

    it "says a coin a pending transaction spends is locked by tx, as the table does" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  locked by tx, 3 confirmations"

    -- The replacement is a pending transaction too, and the line has
    -- little room: the verbose view says which of the two holds the coin
    it "says a coin of a payment that a pending transaction replaces is locked by tx too" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByReplacement, jsonCoinSpendable = False, jsonCoinConfirmations = 0}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  locked by tx, unconfirmed"

    it "says a coin of Bitcoin Cash that holds tokens is locked by tokens" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByTokens, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  locked by tokens, 3 confirmations"

    it "says a coin locked with lockcoin is locked by you" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  locked by you, 3 confirmations"

    it "says a coin that a tag of its address locks is locked by tag" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByTag, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  locked by tag, 3 confirmations"

    it "says 999+ confirmations for a coin of more than 999" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinConfirmations = 1000}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  spendable, 999+ confirmations"

    it "says a free coin the wallet cannot spend yet is not spendable yet, and an unconfirmed one unconfirmed" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  not spendable yet, unconfirmed"

    it "says the time is unknown when hw does not know it, in a column of the same width" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin Nothing spendableCoinRow) !! 0)
        `shouldBe` "00000000:0  time unknown           0.00050000 btc  spendable, 3 confirmations"

  -- The fields are padded to "Short id", the longest of their names
  describe "A coin with every detail, as coins --verbose shows it" $ do
    -- spendableCoinRow is output 0 of transaction 1, in block 800000
    it "gives its first line, then the coin in full and by its short id, the block that holds it, the time of its transaction to the second and its address in full with its path as fields" $
      renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow)
        `shouldBe` [ "14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations",
                     "  Coin    : 0000000000000000000000000000000000000000000000000000000000000001:0",
                     "  Short id: 00000000:0",
                     "  Block   : 800000",
                     "  Time    : 14 Nov 2023 23:13:20 CET",
                     "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0"
                   ]

    it "gives the coin in full, then its short id, the short id of its txid and its index" $
      take 2 (drop 1 (renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1})))
        `shouldBe` [ "  Coin    : a1075db5c0ffee00000000000000000000000000000000000000000000000001:1",
                     "  Short id: a1075db5:1"
                   ]

    it "says in full words that a pending transaction locks the coin" $
      (renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "14 Nov 2023 23:13      0.00050000 btc  locked by a pending transaction, 3 confirmations"

    it "says in full words that a pending replacement locks the coin of a payment it replaces" $
      (renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByReplacement, jsonCoinSpendable = False, jsonCoinConfirmations = 0}) !! 0)
        `shouldBe` "14 Nov 2023 23:13      0.00050000 btc  locked by a pending replacement, unconfirmed"

    it "gives the full count of the confirmations, past 999 too" $
      (renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinConfirmations = 1234}) !! 0)
        `shouldBe` "14 Nov 2023 23:13      0.00050000 btc  spendable, 1234 confirmations"

  -- The coin has no time, so the clock of the computer plays no part.
  -- spendableCoinRow is at the first receiving address of the bip44
  -- account of portfolio0 on btc.
  describe "A coin as lockcoin, unlockcoin and show answer with it" $ do
    let coin = spendableCoinRow {jsonCoinTime = Nothing}

    it "gives every detail, as coins --verbose does, then the account that holds it: its portfolio, its network and its type" $ do
      shown <- coinAnswerDoc WithoutWallets UnitBitcoin (Just mainPortfolioRef) mainAccountRef coin
      renderedLines shown
        `shouldBe` [ "time unknown           0.00050000 btc  spendable, 3 confirmations",
                     "  Coin    : 0000000000000000000000000000000000000000000000000000000000000001:0",
                     "  Short id: 00000000:0",
                     "  Block   : 800000",
                     "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0",
                     "  Account : portfolio0  btc  bip44"
                   ]

    -- lockcoin finds a coin of portfolio0 while portfolio1 is active
    it "says the account of the coin is not of the active portfolio, and names the active one, when another one is active" $ do
      shown <- coinAnswerDoc WithoutWallets UnitBitcoin (Just (PortfolioRef 0 1)) mainAccountRef coin
      last (renderedLines shown) `shouldBe` "  Account : portfolio0, not the active portfolio1  btc  bip44"

    it "says 1 confirmation in the singular" $
      (renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinConfirmations = 1}) !! 0)
        `shouldBe` "14 Nov 2023 23:13      0.00050000 btc  spendable, 1 confirmation"

    it "gives the label of its transaction, quoted, as its first field" $
      (renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinTxLabel = "Salary"}) !! 1)
        `shouldBe` "  Label   : \"Salary\""

    -- "  Address : " takes 12 characters
    it "gives the label of its address on the line below, under the address" $
      drop 5 (renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinAddressLabel = "Wages"}))
        `shouldBe` [ "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0",
                     "            \"Wages\""
                   ]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address
    it "says a coin at a change address is at one, on the line below the address" $
      drop 5 (renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) atChange))
        `shouldBe` [ "  Address : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  bip44/1/0",
                     "            change address"
                   ]

    it "gives no block for an unconfirmed coin, which is in none yet" $
      renderedLines (verboseCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False})
        `shouldBe` [ "14 Nov 2023 23:13      0.00050000 btc  not spendable yet, unconfirmed",
                     "  Coin    : 0000000000000000000000000000000000000000000000000000000000000001:0",
                     "  Short id: 00000000:0",
                     "  Time    : 14 Nov 2023 23:13:20 CET",
                     "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0"
                   ]

{- Syncs and recover -}

-- | A sync of portfolio 0 on a network where it holds nothing and nothing
-- changed
quietOn :: PortfolioRef -> Network -> SyncRes
quietOn ref net = quietSync {syncResPlace = Place ref net, syncResConfirmed = 0, syncResUnconfirmed = 0, syncResCoins = 0, syncResTotal = 0, syncResSpendable = 0}

-- | What a store at an address that refuses every connection says
downStore :: Text
downStore = "The store at http://127.0.0.1:1/ did not answer"

syncSpec :: Spec
syncSpec =
  describe "The sync of a portfolio as hw shows it" $ do
    -- quietSync: the 3 coins of portfolio0 on btc are worth 5100000, and
    -- it can spend 4900000: 5100000 - 4900000 = 200000 it cannot
    it "gives where it is, what the portfolio holds there, the block it is synced to, and each kind of change on a line" $
      renderedLines (syncResultDoc WithoutWallets UnitBitcoin quietSync {syncResTxUpdates = 2, syncResCoinUpdates = 3, syncResRebroadcast = 1, syncResConflicted = 1, syncResNewAddrs = 4})
        `shouldBe` [ "portfolio0  btc",
                     "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                     "  synced at block 800002",
                     "  2 transactions and 3 coins updated",
                     "  1 transaction sent again",
                     "  1 pending transaction conflicted",
                     "  4 new addresses found"
                   ]

    -- The command sent a transaction: that a sync was done, at which
    -- block, and that the transaction updated the wallet, is no news
    it "gives where it is and what the portfolio holds there, and neither the block nor the count of updates, after a transaction sent" $
      renderedLines (syncedAfterDoc WithoutWallets UnitBitcoin quietSync {syncResTxUpdates = 1, syncResCoinUpdates = 2})
        `shouldBe` [ "portfolio0  btc",
                     "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins"
                   ]

    -- The change of a payment of the wallet is unconfirmed and can be
    -- spent: the line once counted it under "spendable" and under
    -- "unconfirmed"
    it "leaves out the unspendable amount when the portfolio can spend all it holds, and gives no unconfirmed amount" $
      renderedLines (syncedAfterDoc WithoutWallets UnitBitcoin quietSync {syncResConfirmed = 0, syncResUnconfirmed = 27740, syncResCoins = 1, syncResTotal = 27740, syncResSpendable = 27740})
        `shouldBe` [ "portfolio0  btc",
                     "  total 0.00027740 btc, spendable 0.00027740 btc, 1 coin"
                   ]

    -- A sync tells each of these once
    it "still tells, after a transaction sent, of a lock that moved, of a transaction sent again and of a pending transaction in conflict" $
      renderedLines (syncedAfterDoc WithoutWallets UnitBitcoin quietSync {syncResTxUpdates = 1, syncResLocksMoved = [(OutPoint (fakeTxHash 1) 0, OutPoint (fakeTxHash 2) 1)], syncResRebroadcast = 1, syncResConflicted = 1})
        `shouldBe` [ "portfolio0  btc",
                     "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                     "  lock moved from 00000000:0 to 00000000:1, the coin that replaced it",
                     "  1 transaction sent again",
                     "  1 pending transaction conflicted"
                   ]

    -- The txid is fakeTxHash 1
    it "shows the portfolios synced below a transaction sent, each after a blank line" $
      printedLines (ResponseSentTx btc (fakeTxHash 1) (SyncAfter [quietSync, quietSync {syncResPlace = Place (PortfolioRef 0 1) btc}] Nothing))
        `shouldReturn` [ "Sent     : 0000000000000000000000000000000000000000000000000000000000000001",
                         "  Short id : 00000000",
                         "  Network  : btc",
                         "",
                         "portfolio0  btc",
                         "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                         "",
                         "portfolio1  btc",
                         "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins"
                       ]

    it "shows nothing below a transaction sent that touches no portfolio" $
      printedLines (ResponseSentTx btc (fakeTxHash 1) nothingToSyncAfter)
        `shouldReturn` [ "Sent     : 0000000000000000000000000000000000000000000000000000000000000001",
                         "  Short id : 00000000",
                         "  Network  : btc"
                       ]

    -- The sentence starts with what matters most below a payment: it is
    -- sent
    it "says below a transaction sent that it is sent and that the wallet could not sync after it, with the failure of the store" $
      printedLines (ResponseSentTx btc (fakeTxHash 1) (SyncAfter [] (Just downStore)))
        `shouldReturn` [ "Sent     : 0000000000000000000000000000000000000000000000000000000000000001",
                         "  Short id : 00000000",
                         "  Network  : btc",
                         "",
                         "The transaction is sent, and the wallet could not sync after it: The store at",
                         "http://127.0.0.1:1/ did not answer"
                       ]

    -- The portfolio holds 80000 on Bitcoin Cash in 2 coins. The sync
    -- stored both, and the one of 30000 holds tokens: 80000 - 30000 =
    -- 50000 can be spent.
    it "says how many of the new coins hold tokens, what they hold, that hw never spends them, and what moves them" $
      renderedLines (syncResultDoc WithoutWallets UnitBitcoin (quietOn mainPortfolioRef bch) {syncResConfirmed = 80000, syncResCoins = 2, syncResTotal = 80000, syncResSpendable = 50000, syncResCoinUpdates = 2, syncResTokenCoins = [30000]})
        `shouldBe` [ "portfolio0  bch",
                     "  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc, 2 coins",
                     "  synced at block 800002",
                     "  2 coins updated",
                     "  1 new coin holds tokens (CashTokens): 0.00030000 btc, which hw never spends",
                     "    a wallet that handles tokens moves it, with the same mnemonic"
                   ]

    -- 1000 + 2000 = 3000
    it "adds up what several new coins with tokens hold" $
      drop 4 (renderedLines (syncResultDoc WithoutWallets UnitBitcoin (quietOn mainPortfolioRef bch) {syncResCoins = 2, syncResCoinUpdates = 2, syncResTokenCoins = [1000, 2000]}))
        `shouldBe` [ "  2 new coins hold tokens (CashTokens): 0.00003000 btc, which hw never spends",
                     "    a wallet that handles tokens moves them, with the same mnemonic"
                   ]

    -- The transaction stays pending. Its short id names it for deletetx,
    -- and what the store said follows, whole.
    it "names a transaction the store did not publish when it was sent again, with what the store said" $
      renderedLines (syncResultDoc WithoutWallets UnitBitcoin quietSync {syncResNotPublished = [(fakeTxHash 9, "The store at https://api.haskoin.com/ refused the transaction: The fee is too low")]})
        `shouldBe` [ "portfolio0  btc",
                     "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                     "  synced at block 800002",
                     "  00000000 not sent again: The store at https://api.haskoin.com/ refused the transaction: The fee is too low"
                   ]

    -- The payment of a coin the user locked was replaced: its coin went
    -- and the coin of the replacement came, which makes two coins updated
    it "names a coin that lost its lock to the coin that replaced it, both by their short ids, below the coins updated" $
      renderedLines
        ( syncResultDoc WithoutWallets UnitBitcoin $
            quietSync
              { syncResCoinUpdates = 2,
                syncResLocksMoved =
                  [ ( OutPoint (typedTxid "655a1a87000000000000000000000000000000000000000000000000000000aa") 0,
                      OutPoint (typedTxid "64253442000000000000000000000000000000000000000000000000000000bb") 1
                    )
                  ]
              }
        )
        `shouldBe` [ "portfolio0  btc",
                     "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                     "  synced at block 800002",
                     "  2 coins updated",
                     "  lock moved from 655a1a87:0 to 64253442:1, the coin that replaced it"
                   ]

    it "says nothing new when nothing changed" $
      renderedLines (syncResultDoc WithoutWallets UnitBitcoin quietSync)
        `shouldBe` [ "portfolio0  btc",
                     "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                     "  synced at block 800002, nothing new"
                   ]

    it "counts the transactions updated alone when no coin was" $
      drop 2 (renderedLines (syncResultDoc WithoutWallets UnitBitcoin quietSync {syncResTxUpdates = 1}))
        `shouldBe` [ "  synced at block 800002",
                     "  1 transaction updated"
                   ]

    it "names the wallet of the portfolio in a database of several wallets" $
      take 1 (renderedLines (syncResultDoc WithWallets UnitBitcoin quietSync {syncResPlace = Place otherRef btc}))
        `shouldBe` ["portfolio1 of wallet2  btc"]

    it "names wallet0 too in a database of several wallets" $
      take 1 (renderedLines (syncResultDoc WithWallets UnitBitcoin quietSync))
        `shouldBe` ["portfolio0 of wallet0  btc"]

    -- A portfolio has accounts on eight networks, most of which hold
    -- nothing: those are named in one line for each portfolio
    -- The sync went over wallet0 and wallet2, so each line names its
    -- wallet
    it "names the networks where a portfolio holds nothing and nothing changed in one line for each portfolio, below the others" $
      renderedLines (syncResultsPrinter WithWallets UnitBitcoin [quietSync, quietOn mainPortfolioRef bch, quietOn otherRef btc, quietOn mainPortfolioRef btcTestnet3])
        `shouldBe` [ "portfolio0 of wallet0  btc",
                     "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                     "  synced at block 800002, nothing new",
                     "",
                     "portfolio0 of wallet0  nothing on bch and btctest",
                     "portfolio1 of wallet2  nothing on btc"
                   ]

    it "leaves the wallet out of the line of the networks where nothing changed in a database of one wallet" $
      renderedLines (syncResultsPrinter WithoutWallets UnitBitcoin [quietOn mainPortfolioRef bch])
        `shouldBe` ["portfolio0  nothing on bch"]

    it "gives a block of its own to a network where the sync found something, though the portfolio holds nothing there" $
      take 1 (renderedLines (syncResultsPrinter WithoutWallets UnitBitcoin [(quietOn mainPortfolioRef bch) {syncResNewAddrs = 1}]))
        `shouldBe` ["portfolio0  bch"]

    -- A host that answers 404 does not serve the network: no failure
    it "names the networks the host does not serve in one line" $
      renderedLines (notServedDoc [btcTestnet3, btcRegTest]) `shouldBe` ["Not served by the store: btctest, btcreg"]

    it "gives each network left out with why its store failed" $
      renderedLines (failedNetworksDoc "not synced" [(bch, downStore), (btcTest4, downStore)])
        `shouldBe` [ "bch  not synced: The store at http://127.0.0.1:1/ did not answer",
                     "btctest4  not synced: The store at http://127.0.0.1:1/ did not answer"
                   ]

    it "answers with the syncs, then the networks not served, then those down, a blank line between them" $
      printedLines (ResponseSync [quietSync, quietOn mainPortfolioRef bch] [btcTest4] [(bchTest4, downStore)] (Just mainPortfolioRef))
        `shouldReturn` [ "portfolio0  btc",
                         "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                         "  synced at block 800002, nothing new",
                         "",
                         "portfolio0  nothing on bch",
                         "",
                         "Not served by the store: btctest4",
                         "",
                         "bchtest4  not synced: The store at http://127.0.0.1:1/ did not answer"
                       ]

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

    it "aligns the paths when the names of the networks differ in length" $
      take 2 (drop 1 (renderedLines (foreignFundsDoc UnitBitcoin [(btcTest4, ForeignFunds Bip49 0 1 1000), (btc, ForeignFunds Bip49 0 1 1000)])))
        `shouldBe` [ "  btctest4  /49'/1'/0'  bip49, wrapped SegWit: 1 address used, 0.00001000 btc left",
                     "  btc       /49'/0'/0'  bip49, wrapped SegWit: 1 address used, 0.00001000 btc left"
                   ]

    it "names the bip44 accounts of Bitcoin on Bitcoin Cash by the coin type of their path" $
      renderedLines (foreignFundsDoc UnitBitcoin [(bch, ForeignFunds Bip44OfBitcoin 0 2 50000)]) !! 1
        `shouldBe` "  bch  /44'/0'/0'  bip44 with the coin type of Bitcoin: 2 addresses used, 0.00050000 btc left"

recoverSpec :: Spec
recoverSpec =
  describe "What recover found, as its answer shows it" $ do
    it "names the portfolios it recovered and their wallet" $
      renderedLines (recoveredDoc [btc, bch] 10 (Just 0) [0, 1])
        `shouldBe` ["Recovered portfolio0 and portfolio1 of wallet0"]

    it "says no portfolio was paid on the networks searched, and when the search ended, when it found none" $
      renderedLines (recoveredDoc [btc, bch, btcTest4] 10 Nothing [])
        `shouldBe` ["No portfolio of the mnemonic was paid on btc, bch or btctest4. The search ended after 10 portfolios in a row without a payment."]

    it "says no new portfolio was paid when the wallet of the mnemonic had every one it found" $
      renderedLines (recoveredDoc [btc] 10 (Just 1) [])
        `shouldBe` ["No new portfolio of the mnemonic was paid on btc. The search ended after 10 portfolios in a row without a payment."]

    -- The database had portfolio0 of wallet0, which stays active, and
    -- recover created wallet1
    it "says which portfolio is still active when the portfolios it recovered are not" $
      take 2 <$> printedLines (ResponseRecover [btc] 10 (Just 1) [0] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) False)) Nothing)
        `shouldReturn` ["Recovered portfolio0 of wallet1", "The active portfolio is still portfolio0 of wallet0."]

    it "says nothing of the active portfolio when it recovered none" $
      printedLines (ResponseRecover [btc] 10 (Just 0) [] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) False)) Nothing)
        `shouldReturn` ["No new portfolio of the mnemonic was paid on btc. The search ended after 10 portfolios in a row without a payment."]

    it "says what to check, on a line of its own, when it found nothing of a secret that takes a passphrase" $
      printedLines (ResponseRecover [btc] 10 Nothing [] [] [] [] [] Nothing (Just "If this mnemonic was paid, check the passphrase: another passphrase, or none, gives another wallet."))
        `shouldReturn` [ "No portfolio of the mnemonic was paid on btc. The search ended after 10 portfolios in a row without a payment.",
                         "If this mnemonic was paid, check the passphrase: another passphrase, or none, gives another wallet."
                       ]

    -- The network left out may have failed during the search or during
    -- the syncs after it. The database had no portfolio, and portfolio0
    -- became active.
    it "answers with what it found and the active portfolio, then the syncs, the networks not served, and those left out with why" $
      printedLines (ResponseRecover [btc] 10 (Just 0) [0] [quietSync] [] [btcTest4] [(bch, downStore)] (Just (ActiveAfter (PortfolioRef 0 0) True)) Nothing)
        `shouldReturn` [ "Recovered portfolio0 of wallet0",
                         "portfolio0 of wallet0 is the active portfolio: the commands work in it.",
                         "",
                         "portfolio0  btc",
                         "  total 0.05100000 btc, spendable 0.04900000 btc, unspendable 0.00200000 btc, 3 coins",
                         "  synced at block 800002, nothing new",
                         "",
                         "Not served by the store: btctest4",
                         "",
                         "bch  left out: The store at http://127.0.0.1:1/ did not answer"
                       ]

deletedSpec :: Spec
deletedSpec =
  describe "What deleteportfolio and deletewallet deleted, as their answers show it" $ do
    it "says what was deleted" $
      printedLines (ResponseDeleted "portfolio2 of wallet0 has been deleted" Nothing)
        `shouldReturn` ["portfolio2 of wallet0 has been deleted"]

    it "names under it the two files of the backup hw made first" $
      printedLines (ResponseDeleted "wallet1 has been deleted, with all of its portfolios" (Just backupBefore))
        `shouldReturn` [ "wallet1 has been deleted, with all of its portfolios",
                         "Before that, the database was copied to /home/someone/.hw/auto-backups/before.sqlite",
                         "Its labels are in /home/someone/.hw/auto-backups/before.jsonl"
                       ]

-- | The two files of a backup hw made before a command lost something
backupBefore :: BackupFiles
backupBefore = BackupFiles "/home/someone/.hw/auto-backups/before.sqlite" "/home/someone/.hw/auto-backups/before.jsonl"

{- Restoring the database -}

-- The clock given reads 14 Nov 2023 at 'seenTime'
restoredSpec :: Spec
restoredSpec =
  describe "What restoredatabase did, as its answer shows it" $ do
    it "says which copy the database was restored from, then lists the wallets it holds now, then names the backup of the database it replaced" $
      renderedLines (restoredDoc "copy.sqlite" (databaseOf [shownWallet [shownPortfolio]]) [[seenTimeInCet]] (Just backupBefore) Nothing)
        `shouldBe` [ "The database was restored from copy.sqlite",
                     "",
                     "wallet0  bc560f95",
                     "  * portfolio0  created 14 Nov 2023",
                     "",
                     "The database it replaced was copied to /home/someone/.hw/auto-backups/before.sqlite",
                     "Its labels are in /home/someone/.hw/auto-backups/before.jsonl"
                   ]

    it "ends with the wallets when hw made no backup" $
      renderedLines (restoredDoc "copy.sqlite" (databaseOf [shownWallet [shownPortfolio]]) [[seenTimeInCet]] Nothing Nothing)
        `shouldBe` [ "The database was restored from copy.sqlite",
                     "",
                     "wallet0  bc560f95",
                     "  * portfolio0  created 14 Nov 2023"
                   ]

    -- hw cannot copy a database it cannot read, so it moved the file
    it "ends with where the database it replaced was moved, when that one was damaged" $
      renderedLines (restoredDoc "copy.sqlite" (databaseOf [shownWallet [shownPortfolio]]) [[seenTimeInCet]] Nothing (Just "/home/someone/.hw/broken-databases/portfolios-2023-11-14T22-13-20.000Z.sqlite"))
        `shouldBe` [ "The database was restored from copy.sqlite",
                     "",
                     "wallet0  bc560f95",
                     "  * portfolio0  created 14 Nov 2023",
                     "",
                     "The damaged database it replaced was moved to /home/someone/.hw/broken-databases/portfolios-2023-11-14T22-13-20.000Z.sqlite"
                   ]

    it "says of a copy without a wallet that the database holds none" $
      renderedLines (restoredDoc "copy.sqlite" (databaseOf []) [] Nothing Nothing)
        `shouldBe` [ "The database was restored from copy.sqlite",
                     "",
                     "It holds no wallet"
                   ]

{- The backups -}

-- | The copies of a home as showbackups shows them, both kinds on: three
-- of the database, twelve of the labels, and one backup of the user. The
-- newest were made two days, five minutes and three weeks before
-- 'seenTime'.
backupsHeld :: BackupsShown
backupsHeld =
  BackupsShown
    { backupsAutoDirectory = "/home/someone/.hw/auto-backups",
      backupsDatabaseOn = True,
      -- 2 days are 2 × 86400 = 172800 seconds
      backupsDatabase = CopiesShown 3 (Just ("/home/someone/.hw/auto-backups/portfolios-version-5-2023-11-12T22-13-20.000Z-wallets-1-portfolios-3.sqlite", addUTCTime (-172800) seenTime)),
      backupsLabelsOn = True,
      -- 5 minutes are 300 seconds
      backupsLabels = CopiesShown 12 (Just ("/home/someone/.hw/auto-backups/portfolios-labels-2023-11-14T22-08-20.000Z-wallets-1-portfolios-3.jsonl", addUTCTime (-300) seenTime)),
      backupsUserDirectory = "/home/someone/.hw/user-backups",
      -- 21 days are 21 × 86400 = 1814400 seconds
      backupsUser = CopiesShown 1 (Just ("/home/someone/.hw/user-backups/portfolios-version-5-2023-10-24T22-13-20.000Z-wallets-1-portfolios-2.sqlite", addUTCTime (-1814400) seenTime))
    }

-- The time now is 'seenTime'
backupsSpec :: Spec
backupsSpec =
  describe "The backups as showbackups shows them" $ do
    it "gives each directory, then under it each kind of copy: how many, how long ago the newest was made, and its name" $
      renderedLines (backupsDoc seenTime backupsHeld)
        `shouldBe` [ "Automatic backups  /home/someone/.hw/auto-backups",
                     "  Database: 3 copies, the newest made 2 days ago",
                     "            portfolios-version-5-2023-11-12T22-13-20.000Z-wallets-1-portfolios-3.sqlite",
                     "  Labels  : 12 copies, the newest made 5 minutes ago",
                     "            portfolios-labels-2023-11-14T22-08-20.000Z-wallets-1-portfolios-3.jsonl",
                     "",
                     "User backups  /home/someone/.hw/user-backups",
                     "  Database: 1 copy, made 21 days ago",
                     "            portfolios-version-5-2023-10-24T22-13-20.000Z-wallets-1-portfolios-2.sqlite"
                   ]

    it "says of a kind without a copy that there is none yet" $
      drop 6 (renderedLines (backupsDoc seenTime backupsHeld {backupsUser = CopiesShown 0 Nothing}))
        `shouldBe` [ "User backups  /home/someone/.hw/user-backups",
                     "  Database: no copy yet"
                   ]

    it "says under the copies of a kind which key of the configuration turns it off" $
      take 4 (renderedLines (backupsDoc seenTime backupsHeld {backupsDatabaseOn = False}))
        `shouldBe` [ "Automatic backups  /home/someone/.hw/auto-backups",
                     "  Database: 3 copies, the newest made 2 days ago",
                     "            portfolios-version-5-2023-11-12T22-13-20.000Z-wallets-1-portfolios-3.sqlite",
                     "            Turned off: the key autobackup-database of the configuration is false"
                   ]

    it "says of a kind that is off and has no copy that it is off, and nothing else" $
      take 2 (drop 3 (renderedLines (backupsDoc seenTime backupsHeld {backupsLabelsOn = False, backupsLabels = CopiesShown 0 Nothing})))
        `shouldBe` [ "  Labels  : Turned off: the key autobackup-labels of the configuration is false",
                     ""
                   ]

{- Transactions -}

headlineSpec :: Spec
headlineSpec = do
  -- The first line of the verbose view starts with the time, and names
  -- the type of the transaction
  describe "The first line of the verbose view of a transaction" $ do
    -- 22:13 UTC is 23:13 CET. 50000 satoshis are 0.0005 bitcoins.
    it "shows a payment received as Incoming, with a plus sign, at the minute of the clock given" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (headlineDoc fullConfirmations UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "shows a payment sent as Outgoing, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (headlineDoc fullConfirmations UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations"]

    -- /0/0 pays 40000 to /0/1 and 9000 of change to /1/0: the portfolio
    -- only loses the fee of 1000
    it "shows a transaction between addresses of the portfolio as Internal, costing its fee" $ do
      let internal =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 1)) ""), (internalAddr 0, MyOutputs 9000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
              }
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (headlineDoc fullConfirmations UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Internal     -0.00001000 btc  2 confirmations"]

  -- The first line of the compact view starts with the short id, and
  -- leaves the type to the sign and the colour of the amount. The txids
  -- of fakeTxHash all start with 00000000.
  describe "The first line of the compact view of a transaction" $ do
    it "starts with the short id of the txid, then gives the time, the amount with its sign, and the confirmations" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5  14 Nov 2023 23:13     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "gives a payment sent a minus sign, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations"]

    -- 1250000000 satoshis are 12.5 bitcoins
    it "lines up amounts of different widths" $ do
      let small = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
          large = small {txInfoSummary = paymentSummary {summaryAmount = 1250000000}}
      concatMap (renderedLines . compactHeadline UnitBitcoin (Just seenTimeInCet)) [small, large]
        `shouldBe` [ "00000000  14 Nov 2023 23:13     +0.00050000 btc  3 confirmations",
                     "00000000  14 Nov 2023 23:13    +12.50000000 btc  3 confirmations"
                   ]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 1 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     +0.00050000 btc  1 confirmation"]

    it "counts the confirmations up to 999" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 999 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  999 confirmations"]

    it "says 999+ confirmations for more than 999" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1000 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  999+ confirmations"]

    it "gives the fee rate of a transaction not in a block yet" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  unconfirmed, 4.4 sat/vB"]

    it "says dead for a transaction replaced or dropped from the network, without its fee rate" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  dead"]

    it "says the time is unknown when hw does not know it, in a column of the same width" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) Nothing 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin Nothing info)
        `shouldBe` ["00000000  time unknown          +0.00050000 btc  3 confirmations"]

    it "starts with the short id of the nosigHash of a transaction not signed, and says it is not signed, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5  14 Nov 2023 23:13     -0.00041000 btc  not signed, 4.4 sat/vB"]

    it "starts with the short id of the txid of a signed transaction, and says it is not sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["b2d4f6a8  14 Nov 2023 23:13     -0.00041000 btc  signed, not sent, 4.4 sat/vB"]

    it "shows a transaction sent that the sync has not seen yet as sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) True False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  sent, 4.4 sat/vB"]

    it "shows a transaction in conflict as conflicted, without its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  conflicted"]

compactSpec :: Spec
compactSpec = do
  describe "The balance after a transaction" $ do
    let spent = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False []))

    it "starts the second line of the compact view, before what the transaction is for" $
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty Map.empty noTxTags Map.empty))
        `shouldBe` [ "00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  balance 0.01208000 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "comes before the label of the transaction" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) (spent "Rent of October" Map.empty Map.empty noTxTags Map.empty)) !! 1)
        `shouldBe` "  balance 0.01208000 btc, \"Rent of October\""

    it "shows a portfolio left with nothing on the network as zero" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just 0) (Just seenTimeInCet) (spent "" Map.empty Map.empty noTxTags Map.empty)) !! 1)
        `shouldBe` "  balance 0.00000000 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "is in satoshis with the amount, each with its unit" $
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitSatoshi Nothing (Just 1) (Just seenTimeInCet) (spent "" Map.empty Map.empty noTxTags Map.empty))
        `shouldBe` [ "00000000  14 Nov 2023 23:13          -41000 sats  2 confirmations",
                     "  balance 1 sat, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    -- txs gives no balance to a dead transaction, which does not count
    it "is left out when the listing gives none" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) (spent "" Map.empty Map.empty noTxTags Map.empty)) !! 1)
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "follows the fee in the verbose view" $
      take 2 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty Map.empty noTxTags Map.empty))))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4.4 sat/vB",
                     "  Balance  : 0.01208000 btc"
                   ]

  describe "The compact view of a transaction" $ do
    it "gives the address a payment pays, without its change, on the line below its first line" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "a1075db5  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "gives the label of a payment, quoted, in place of the address it pays" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "Rent of October" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  \"Rent of October\""
                   ]

    -- Each account of a portfolio has an address at /0/0: the type tells
    -- them apart
    it "names the address of the portfolio a payment received is paid at by the type of its account and its path, with its label" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at bip44/0/0 \"Rent\""

    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc is /0/0 of m/84'/0'/0'
    it "names an address of the bip84 account of the portfolio by bip84 and its path" $ do
      let toSegwit = paymentSummary {summaryMyOutputs = Map.fromList [(segwitExternalAddr 0, MyOutputs 50000 (segwitPath (Deriv :/ 0 :/ 0)) "")]}
          info = TxInfo toSegwit (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at bip84/0/0"

    it "gives the label of a payment received after the path of its address, in place of the label of the address" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "Salary" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at bip44/0/0 \"Salary\""

    -- otherAddr 1 is 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "gives the largest output of several, and counts the others" $ do
      let several = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 10000), (otherAddr 1, 29000), (otherAddr 2, 1000)]}
          info = TxInfo several (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ and 2 more"

    -- A Taproot address has 62 characters
    it "gives the address of another wallet whole, however long, at the end of its line" $ do
      let toTaproot = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(taprootExternalAddr 0, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) toTaproot) !! 1)
        `shouldBe` "  balance 0.01208000 btc, to bc1p8zl0j9e73592k9n6ftz4keyq24dq8djqd3rkcrepu2fw6zeryyjs8zz3ev"

    -- pendingtxs names where each transaction is
    it "names the portfolio and the network first when the listing names them" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin (Just mainPortfolioRef) Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  portfolio0 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the wallet of the portfolio there, wallet0 too, when the database has several wallets" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithWallets) btc UnitBitcoin (Just mainPortfolioRef) Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  portfolio0 of wallet0 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    -- The one wallet may be wallet2, once the others were deleted
    it "leaves the wallet out when the database has one wallet, even when it is not wallet0" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin (Just otherRef) Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  portfolio1 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the transaction a replacement replaces by its short id, before what it is for" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [b2d4f6a8Txid])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  replaces b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the two transactions a replacement replaces, the second after and" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [b2d4f6a8Txid, a1075db5Second])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  replaces b2d4f6a8 and a1075db5, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the transaction that replaced a dead one by its short id, before what it is for" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "a1075db5  14 Nov 2023 23:13     -0.00041000 btc  dead",
                     "  replaced by b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "names the two transactions that replaced a dead one, the second after and" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid, a1075db5Second])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  replaced by b2d4f6a8 and a1075db5, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    -- txs gives the balance first, and a dead transaction has none
    it "says nothing of a replacement for a dead transaction that names none" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

-- The columns have fixed widths: 8 for the short id of the txid, 17 for
-- the time, 8 for the type, 14 for each amount and 5 for the
-- confirmations, the width of "Confs". " │ " sets two cells apart. The
-- confirmations are right-aligned, as numbers. The last column says what
-- the transaction is for, whole.
tableSpec :: Spec
tableSpec = do
  describe "The table of txs --tabular" $ do
    let spent = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False []))
        spentWith confirmations = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) confirmations False [])) "" Map.empty Map.empty noTxTags Map.empty

    -- Under a heading, the line has a dash for each character and the
    -- space on each side: 8 + 1 = 9 under Txid, 17 + 2 = 19 under Time,
    -- 8 + 2 = 10 under Type, 14 + 2 = 16 under each amount, 5 + 2 = 7
    -- under Confs, 12 + 2 = 14 under Tags, and 11 + 1 = 12 under Description
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader (txColumns UnitBitcoin 12))
        `shouldBe` [ "Txid     │ Time              │ Type     │   Amount (btc) │  Balance (btc) │ Confs │ Tags         │ Description",
                     "─────────┼───────────────────┼──────────┼────────────────┼────────────────┼───────┼──────────────┼────────────"
                   ]

    it "names satoshis in the headings of the amounts when they are in satoshis" $
      take 1 (concatMap renderedLines (tableHeader (txColumns UnitSatoshi 12)))
        `shouldBe` ["Txid     │ Time              │ Type     │  Amount (sats) │ Balance (sats) │ Confs │ Tags         │ Description"]

    -- 40000 paid + 1000 of fee = 41000
    -- The txids of fakeTxHash all start with 00000000
    it "gives a transaction one row: its time, type, amount, balance, confirmations and the short id of its txid, then its tags and the address paid" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty Map.empty noTxTags Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the txid, its first 8 digits, in the Txid column" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "counts the confirmations up to 999" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) (spentWith 999))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │   999 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says 999+ for more than 999 confirmations" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) (spentWith 1000))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │  999+ │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives 0 confirmations for a transaction not in a block yet, without its fee rate" $ do
      let unconfirmed = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) unconfirmed)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     0 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "shows a payment received with a plus sign, and the type and the path of the address of the portfolio it was paid at" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │              │ at bip44/0/0"]

    it "gives the label of a transaction, quoted, in place of the address paid" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) (spent "Rent of October" Map.empty Map.empty noTxTags Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ \"Rent of October\""]

    it "gives a long label whole, in the last column" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) (spent "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month" Map.empty Map.empty noTxTags Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    -- A Taproot address has 62 characters
    it "gives the address of another wallet whole, however long, never cut" $ do
      let toTaproot = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(taprootExternalAddr 0, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) toTaproot)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to bc1p8zl0j9e73592k9n6ftz4keyq24dq8djqd3rkcrepu2fw6zeryyjs8zz3ev"]

    it "gives a Bitcoin Cash address whole, with its prefix" $ do
      let toCash = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(bchExternalAddr, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) bch UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) toCash)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"]

    it "gives the label of a payment received after the path of the address it was paid at" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "Salary" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │              │ at bip44/0/0 \"Salary\""]

    it "gives the label of the address a payment was received at when the payment has none" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │              │ at bip44/0/0 \"Rent\""]

    it "gives the label of a payment received rather than the label of its address" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "Salary" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │              │ at bip44/0/0 \"Salary\""]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address
    it "says a payment received at a change address is at one" $ do
      let atChangeAddress = paymentSummary {summaryMyOutputs = Map.fromList [(internalAddr 0, MyOutputs 50000 (legacyPath (Deriv :/ 1 :/ 0)) "")]}
          info = TxInfo atChangeAddress (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │              │ at bip44/1/0 change address"]

    -- /0/0 pays 40000 to /0/1 and 9000 to /1/0: the portfolio loses the fee
    -- of 1000
    it "gives the path of the address a transaction between addresses of the portfolio pays the most, and counts the others" $ do
      let internal =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 1)) ""), (internalAddr 0, MyOutputs 9000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
              }
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Internal │    -0.00001000 │     0.01208000 │     2 │              │ to bip44/0/1 and 1 more"]

    -- /0/0 pays 9000 to /0/1 and sends 40000 back to /1/0: the user named
    -- the receiving address, and the change is the larger of the two
    it "gives the path of the receiving address a transaction between addresses of the portfolio pays, though its change is larger" $ do
      let internal =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 9000 (legacyPath (Deriv :/ 0 :/ 1)) ""), (internalAddr 0, MyOutputs 40000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
              }
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Internal │    -0.00001000 │     0.01208000 │     2 │              │ to bip44/0/1 and 1 more"]

    it "gives a long label of an address whole, after its path" $ do
      let longLabel = paymentSummary {summaryMyOutputs = Map.fromList [(externalAddr 0, MyOutputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month")]}
          info = TxInfo longLabel (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │              │ at bip44/0/0 \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    -- A dead transaction has 0 confirmations, as an unconfirmed one does,
    -- but it will never confirm, and the balance does not count it
    it "leaves the balance of a dead transaction empty, and says dead in place of its confirmations" $ do
      let dead = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) Nothing (Just seenTimeInCet) dead)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │                │  dead │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "names the transaction that replaced a dead one first in its description, as the compact view does" $ do
      let dead = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) Nothing (Just seenTimeInCet) dead)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │                │  dead │              │ replaced by b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the amounts as numbers alone, since the header names their unit" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitSatoshi (txColumns UnitSatoshi 12) (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty Map.empty noTxTags Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │         -41000 │        1208000 │     2 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- 1250000000000 satoshis are 12500 bitcoins: "+12500.00000000" takes
    -- 15 characters, one more than the column
    it "lets an amount wider than its column push the rest of its row" $ do
      let large = TxInfo paymentSummary {summaryAmount = 1250000000000} (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 50000) (Just seenTimeInCet) large)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │ +12500.00000000 │     0.00050000 │     3 │              │ at bip44/0/0"]

  -- The screen reads a listing in chunks. A transaction without a time
  -- shows "time unknown", whatever the time zone of the computer.
  describe "A chunk of the table of txs" $ do
    let listed = ListedTx (TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) Nothing 2 False [])) "" Map.empty Map.empty noTxTags Map.empty) (Just 1208000)

    -- The widest tags of a transaction take 11 characters, and those of
    -- an address 15: the table of txs lists transactions
    it "gives its column of tags the width of the widest tags of a transaction of the database" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 15 11 0) (ChunkAt True True False) (ResponseTxs [mainGroup [listed]])
      take 1 (renderedLines doc)
        `shouldBe` ["Txid     │ Time              │ Type     │   Amount (btc) │  Balance (btc) │ Confs │ Tags        │ Description"]

    it "starts with the header when it is the first one" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseTxs [mainGroup [listed]])
      renderedLines doc
        `shouldBe` [ "Txid     │ Time              │ Type     │   Amount (btc) │  Balance (btc) │ Confs │ Tags         │ Description",
                     "─────────┼───────────────────┼──────────┼────────────────┼────────────────┼───────┼──────────────┼────────────",
                     "00000000 │ time unknown      │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "holds its rows alone when it is a later one, with no header and no blank line" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt False False False) (ResponseTxs [mainGroup [listed]])
      renderedLines doc
        `shouldBe` ["00000000 │ time unknown      │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says there are no transactions, without a header, when there are none" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseTxs [])
      renderedLines doc `shouldBe` ["There are no transactions to display"]

    it "puts the line of its place above the header when the listing may cover several places" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True True) (ResponseTxs [mainGroup [listed]])
      renderedLines doc
        `shouldBe` [ "portfolio0  btc",
                     "Txid     │ Time              │ Type     │   Amount (btc) │  Balance (btc) │ Confs │ Tags         │ Description",
                     "─────────┼───────────────────┼──────────┼────────────────┼────────────────┼───────┼──────────────┼────────────",
                     "00000000 │ time unknown      │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

  addressTableSpec
  coinTableSpec
  balanceTableSpec
  pendingTableSpec

-- The columns of the receiving addresses of the bip44 account, with paths
-- of 9 characters, as bip44/0/0: 9 for the path, 34 for a base58 address,
-- 14 for "Total (btc)", as wide as an amount, 15 for "Spendable (btc)",
-- 14 for "Received (btc)", 5 for the transactions, then the label
addressTableSpec :: Spec
addressTableSpec = do
  describe "The table of addresses --tabular" $ do
    let receiving = addressTableColumns UnitBitcoin btc [Bip44] 9 12 False
        changeAddressRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", dBAddressLabel = ""}

    -- Under a heading, the line has a dash for each character and the
    -- space on each side: 9 + 1 = 10 under Path, 34 + 2 = 36 under
    -- Address, 14 + 2 = 16 under Total, 15 + 2 = 17 under Spendable,
    -- 14 + 2 = 16 under Received,
    -- 5 + 2 = 7 under Txs, 12 + 2 = 14 under Tags, and 5 + 1 = 6 under Label
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader receiving)
        `shouldBe` [ "Path      │ Address                            │    Total (btc) │ Spendable (btc) │ Received (btc) │   Txs │ Tags         │ Label",
                     "──────────┼────────────────────────────────────┼────────────────┼─────────────────┼────────────────┼───────┼──────────────┼──────"
                   ]

    it "gives an address one row: its path, the address, what its coins are worth, what it can spend and received, its transactions, its tags and its label" $
      renderedLines (addressRowDoc UnitBitcoin btc receiving paidAddressRow (Holds (CoinTotals 50000 50000)) [])
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00050000 │      0.00050000 │     0.00050000 │     1 │              │ \"Invoice 42\""]

    -- The coin of 50000 is locked: the address holds it and can spend
    -- nothing, where an address that a payment emptied has a total of zero
    it "gives a locked coin in the total of its address and not in what it can spend" $
      renderedLines (addressRowDoc UnitBitcoin btc receiving paidAddressRow (Holds (CoinTotals 50000 0)) [])
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00050000 │      0.00000000 │     0.00050000 │     1 │              │ \"Invoice 42\""]

    it "gives a long label whole, in the last column" $
      renderedLines (addressRowDoc UnitBitcoin btc receiving paidAddressRow {dBAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"} (Holds (CoinTotals 50000 50000)) [])
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00050000 │      0.00050000 │     0.00050000 │     1 │              │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "leaves the amounts of an address no transaction paid empty, and ends its row without a label" $
      renderedLines (addressRowDoc UnitBitcoin btc receiving unusedAddressRow (Holds (CoinTotals 0 0)) [])
        `shouldBe` ["bip44/0/1 │ 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg │                │                 │                │     0 │              │"]

    -- The number of transactions is empty too: a zero would say that
    -- nobody paid the address
    it "leaves the amounts and the transactions of an address of a portfolio that never synced empty" $
      renderedLines (addressRowDoc UnitBitcoin btc receiving unusedAddressRow NotSynced [])
        `shouldBe` ["bip44/0/1 │ 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg │                │                 │                │       │              │"]

    -- 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1 has 33 characters
    it "pads a base58 address shorter than the longest, so that the columns after it line up" $ do
      let shorter = unusedAddressRow {dBAddressIndex = 3, dBAddressAddress = "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"}
      renderedLines (addressRowDoc UnitBitcoin btc receiving shorter (Holds (CoinTotals 0 0)) [])
        `shouldBe` ["bip44/0/3 │ 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1  │                │                 │                │     0 │              │"]

    it "gives the amounts as numbers alone, in satoshis when they are, with wider headings" $ do
      let inSatoshis = addressTableColumns UnitSatoshi btc [Bip44] 9 12 False
      concatMap renderedLines (take 1 (tableHeader inSatoshis) <> [addressRowDoc UnitSatoshi btc inSatoshis paidAddressRow (Holds (CoinTotals 50000 50000)) []])
        `shouldBe` [ "Path      │ Address                            │   Total (sats) │ Spendable (sats) │ Received (sats) │   Txs │ Tags         │ Label",
                     "bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │          50000 │            50000 │           50000 │     1 │              │ \"Invoice 42\""
                   ]

    -- Tags is the last column, 12 wide. The line has 5 + 2 = 7 dashes
    -- under Txs and 12 + 1 = 13 under Tags. A row without tags ends at
    -- the bar after Txs.
    it "has no column for a label in the table of change addresses, which never have one" $ do
      let change = addressTableColumns UnitBitcoin btc [Bip44] 9 12 True
      concatMap renderedLines (tableHeader change <> [addressRowDoc UnitBitcoin btc change changeAddressRow (Holds (CoinTotals 50000 50000)) []])
        `shouldBe` [ "Path      │ Address                            │    Total (btc) │ Spendable (btc) │ Received (btc) │   Txs │ Tags",
                     "──────────┼────────────────────────────────────┼────────────────┼─────────────────┼────────────────┼───────┼─────────────",
                     "bip44/1/0 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │     0.00050000 │      0.00050000 │     0.00050000 │     1 │"
                   ]

  describe "The width of the column of addresses" $ do
    it "is 34 characters for a bip44 account of Bitcoin, the longest base58 address" $
      addressWidth btc [Bip44] `shouldBe` 34

    -- bc1q, then 38 characters for the key hash and the checksum
    it "is 42 characters for a bip84 account, the length of every one" $
      addressWidth btc [Bip84] `shouldBe` 42

    it "is that of the wider type when the listing covers both" $
      addressWidth btc [Bip44, Bip84] `shouldBe` 42

    -- bitcoincash: takes 12 characters, and the address after it 42
    it "is 54 characters on Bitcoin Cash, the address with its prefix" $
      addressWidth bch [Bip44] `shouldBe` 54

  -- The newest receiving address of the account is bip44/0/11, of 10
  -- characters
  describe "A chunk of the table of addresses" $ do
    it "starts with the header when it is the first one, its paths as wide as the longest of the account" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseAddresses [mainGroup [(paidAddressRow, Holds (CoinTotals 50000 50000), [])]])
      renderedLines doc
        `shouldBe` [ "Path       │ Address                            │    Total (btc) │ Spendable (btc) │ Received (btc) │   Txs │ Tags         │ Label",
                     "───────────┼────────────────────────────────────┼────────────────┼─────────────────┼────────────────┼───────┼──────────────┼──────",
                     "bip44/0/0  │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00050000 │      0.00050000 │     0.00050000 │     1 │              │ \"Invoice 42\""
                   ]

    -- The widest set of tags that an address of the database has is
    -- "kyc rent salary", of 15 characters. This chunk has an address with
    -- one tag and an address with none: their labels start in one column.
    it "gives its column of tags the width of the widest tags of the database, so that the labels line up" $ do
      let tagged = (paidAddressRow, Holds (CoinTotals 50000 50000), [plainTag kycTag])
          bare = (paidAddressRow {dBAddressIndex = 1, dBAddressAddress = "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", dBAddressLabel = "Savings"}, Holds (CoinTotals 50000 50000), [])
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 15 0 0) (ChunkAt True True False) (ResponseAddresses [mainGroup [tagged, bare]])
      renderedLines doc
        `shouldBe` [ "Path       │ Address                            │    Total (btc) │ Spendable (btc) │ Received (btc) │   Txs │ Tags            │ Label",
                     "───────────┼────────────────────────────────────┼────────────────┼─────────────────┼────────────────┼───────┼─────────────────┼──────",
                     "bip44/0/0  │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00050000 │      0.00050000 │     0.00050000 │     1 │ kyc             │ \"Invoice 42\"",
                     "bip44/0/1  │ 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg │     0.00050000 │      0.00050000 │     0.00050000 │     1 │                 │ \"Savings\""
                   ]

    -- No address of the database has a tag
    it "gives its column of tags the width of its heading when no address has a tag" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 0 0 0) (ChunkAt True True False) (ResponseAddresses [mainGroup [(paidAddressRow, Holds (CoinTotals 50000 50000), [])]])
      renderedLines doc
        `shouldBe` [ "Path       │ Address                            │    Total (btc) │ Spendable (btc) │ Received (btc) │   Txs │ Tags │ Label",
                     "───────────┼────────────────────────────────────┼────────────────┼─────────────────┼────────────────┼───────┼──────┼──────",
                     "bip44/0/0  │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00050000 │      0.00050000 │     0.00050000 │     1 │      │ \"Invoice 42\""
                   ]

    it "puts the line of its account above the header when the listing may cover several accounts" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True True) (ResponseAddresses [mainGroup [(paidAddressRow, Holds (CoinTotals 50000 50000), [])]])
      take 2 (renderedLines doc)
        `shouldBe` ["portfolio0  btc  bip44", "Path       │ Address                            │    Total (btc) │ Spendable (btc) │ Received (btc) │   Txs │ Tags         │ Label"]

-- The columns of the coins of the bip44 account, with paths of 9
-- characters, as bip44/0/0: 17 for the time, 14 for the value, 17 for the
-- state, 5 for the confirmations, 34 for a base58 address, 9 for the
-- path, then the
-- label. The coins are shown at 'seenTimeInCet', 23:13 in Central Europe.
coinTableSpec :: Spec
coinTableSpec =
  describe "The table of coins --tabular" $ do
    let headed = coinTableColumns UnitBitcoin btc [Bip44] 9 12
        row = coinRowDoc btc UnitBitcoin headed (Just seenTimeInCet)

    -- Under a heading, the line has a dash for each character and the
    -- space on each side: 12 + 1 = 13 under Coin, 17 + 2 = 19 under Time,
    -- 14 + 2 = 16 under Value, 17 + 2 = 19 under State, 5 + 2 = 7 under
    -- Confs, 34 + 2 = 36 under Address, 9 + 2 = 11 under Path, 12 + 2 = 14
    -- under Tags, and 5 + 1 = 6 under Label
    it "names its columns in a header, the unit in the heading of the value, with a line under it" $
      concatMap renderedLines (tableHeader headed)
        `shouldBe` [ "Coin         │ Time              │    Value (btc) │ State             │ Confs │ Address                            │ Path      │ Tags         │ Label",
                     "─────────────┼───────────────────┼────────────────┼───────────────────┼───────┼────────────────────────────────────┼───────────┼──────────────┼──────"
                   ]

    -- spendableCoinRow is output 0 of transaction 1, whose txid starts
    -- with 00000000
    it "gives a coin one row: the time of its transaction, its value, state, confirmations, its address in full with its path, and its short id, then nothing without a label" $
      renderedLines (row spendableCoinRow)
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    -- "a1075db5:1" takes 10 of the 12 characters of the column
    it "gives the short id of the coin, the short id of its txid and its index, in the Coin column" $
      renderedLines (row spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1})
        `shouldBe` ["a1075db5:1   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    it "gives the label of the address of the coin" $
      renderedLines (row spendableCoinRow {jsonCoinAddressLabel = "Wages"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │ \"Wages\""]

    it "leaves out the label of the transaction that paid the coin, which the verbose view gives" $
      renderedLines (row spendableCoinRow {jsonCoinTxLabel = "Salary"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    it "says a coin at a change address is at one, in place of a label" $
      renderedLines (row atChange)
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │ bip44/1/0 │              │ change address"]

    it "says a coin a pending transaction spends is locked by tx" $
      renderedLines (row spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ locked by tx      │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    it "says a coin of a payment that a pending transaction replaces is locked by tx too" $
      renderedLines (row spendableCoinRow {jsonCoinLock = Just LockedByReplacement, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ locked by tx      │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    it "says a coin locked with lockcoin is locked by you" $
      renderedLines (row spendableCoinRow {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ locked by you     │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    it "says a coin that a tag of its address locks is locked by tag" $
      renderedLines (row spendableCoinRow {jsonCoinLock = Just LockedByTag, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ locked by tag     │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    it "says a free coin the wallet cannot spend yet is not spendable yet, and gives an unconfirmed one 0 confirmations" $
      renderedLines (row spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ not spendable yet │     0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    it "says 999+ for more than 999 confirmations" $
      renderedLines (row spendableCoinRow {jsonCoinConfirmations = 1000})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │  999+ │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │"]

    it "gives a long label whole, in the last column" $
      renderedLines (row spendableCoinRow {jsonCoinAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │              │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    -- The account has 12 receiving addresses and 3 change addresses: its
    -- longest path is bip44/0/11, of 10 characters. A coin without a time shows
    -- "time unknown", whatever the time zone of the computer.
    it "takes the width of the paths from the longest path of the account, in a chunk of coins" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseCoins [mainGroup [spendableCoinRow {jsonCoinTime = Nothing}]])
      renderedLines doc
        `shouldBe` [ "Coin         │ Time              │    Value (btc) │ State             │ Confs │ Address                            │ Path       │ Tags         │ Label",
                     "─────────────┼───────────────────┼────────────────┼───────────────────┼───────┼────────────────────────────────────┼────────────┼──────────────┼──────",
                     "00000000:0   │ time unknown      │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0  │              │"
                   ]

    -- The coins of both accounts of btc are listed together: the column of
    -- addresses takes the 42 characters of a bip84 address
    it "makes the column of addresses of a chunk of the coins of both types as wide as a bip84 address" $ do
      let both = Group mainPlace [AccountCounts Bip44 12 3, AccountCounts Bip84 2 0] [spendableCoinRow {jsonCoinTime = Nothing}]
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseCoins [both])
      take 1 (renderedLines doc)
        `shouldBe` ["Coin         │ Time              │    Value (btc) │ State             │ Confs │ Address                                    │ Path       │ Tags         │ Label"]

-- The columns of the balances of the bip44 account, with paths of 9
-- characters, as bip44/0/0: 9 for the path, 34 for a base58 address, 14 for "Total
-- (btc)", 15 for "Spendable (btc)", 17 for "Unspendable (btc)", then the
-- label
balanceTableSpec :: Spec
balanceTableSpec =
  describe "The table of balances --tabular" $ do
    let headed = balanceTableColumns UnitBitcoin btc [Bip44] 9 12
        changeAddressRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", dBAddressLabel = ""}

    -- 9 + 1 = 10 under Path, 34 + 2 = 36 under Address, 14 + 2 = 16 under
    -- Total, 15 + 2 = 17 under Spendable, 17 + 2 = 19 under Unspendable,
    -- 12 + 2 = 14 under Tags, and 5 + 1 = 6 under Label
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader headed)
        `shouldBe` [ "Path      │ Address                            │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Tags         │ Label",
                     "──────────┼────────────────────────────────────┼────────────────┼─────────────────┼───────────────────┼──────────────┼──────"
                   ]

    -- 80000 in all, of which 50000 can be spent: 80000 - 50000 = 30000
    -- cannot, which a pending transaction spends
    it "gives an address one row: its path, the address, its total, spendable and unspendable amounts, then its tags and its label" $
      renderedLines (balanceRowDoc UnitBitcoin btc headed paidAddressRow (CoinBalance 80000 50000 30000 0 0 0) [])
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │              │ \"Invoice 42\""]

    it "gives a long label whole, in the last column" $
      renderedLines (balanceRowDoc UnitBitcoin btc headed paidAddressRow {dBAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"} (CoinBalance 80000 50000 30000 0 0 0) [])
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │              │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "says a change address is one, in place of a label" $
      renderedLines (balanceRowDoc UnitBitcoin btc headed changeAddressRow (CoinBalance 9000 9000 0 0 0 0) [])
        `shouldBe` ["bip44/1/0 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │     0.00009000 │      0.00009000 │        0.00000000 │              │ change address"]

    -- The account has 12 receiving addresses and 3 change addresses: its
    -- longest path is bip44/0/11, of 10 characters. balances gives the total of
    -- the network above its addresses, even narrowed to one network.
    it "starts a chunk with the line of the network and its total, then the header, the paths as wide as the longest of the account" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseBalances [(mainGroup [(paidAddressRow, CoinBalance 80000 50000 30000 0 0 0, [])], CoinBalance 80000 50000 30000 0 0 0)])
      renderedLines doc
        `shouldBe` [ "portfolio0  btc  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc",
                     "Path       │ Address                            │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Tags         │ Label",
                     "───────────┼────────────────────────────────────┼────────────────┼─────────────────┼───────────────────┼──────────────┼──────",
                     "bip44/0/0  │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │              │ \"Invoice 42\""
                   ]

-- The columns of the pending transactions: the short id, the wallet when
-- the database has several, the portfolio and the network of each, as
-- wide as the widest the listing holds or as their headings, 17 for the
-- time, 8 for the type, 14 for the amount, 10 for the status and 6 for
-- the fee rate, then what it is for
pendingTableSpec :: Spec
pendingTableSpec =
  describe "The table of pending transactions --tabular" $ do
    let ofMain = pendingTableColumns UnitBitcoin WithoutWallets 12 [mainPlace]
        preparedWith signedM online conflicted = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) signedM online conflicted (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
        unsigned = preparedWith Nothing False False
        timeless = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty noTxTags Map.empty
        rowOf = pendingRowDoc UnitBitcoin WithoutWallets (activePaths WithoutWallets) ofMain mainPlace (Just seenTimeInCet)

    -- "portfolio0" takes 10, more than "Portfolio", and "btc" less than
    -- "Network", of 7: 8 + 1 = 9 under Hash, 10 + 2 = 12 under Portfolio,
    -- 7 + 2 = 9 under Network, 17 + 2 = 19 under Time, 8 + 2 = 10 under
    -- Type, 14 + 2 = 16 under Amount, 10 + 2 = 12 under Status, 6 + 2 = 8
    -- under sat/vB, 12 + 2 = 14 under Tags, and 11 + 1 = 12 under Description
    it "names its columns in a header: the short id, then the portfolio and the network, which each transaction names" $
      concatMap renderedLines (tableHeader ofMain)
        `shouldBe` [ "Hash     │ Portfolio  │ Network │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Tags         │ Description",
                     "─────────┼────────────┼─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼──────────────┼────────────"
                   ]

    -- 40000 paid + 1000 of fee = 41000, at 4.4 sat/vB (1000 / 226 = 4.42). The hashes of
    -- fakeTxHash all start with 00000000.
    it "gives a transaction one row: its short id, its portfolio and network, its time, type, amount, status and fee rate, then its tags and what it is for" $
      renderedLines (rowOf unsigned)
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │    4.4 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the nosigHash of a transaction not signed" $ do
      let notSigned = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (rowOf notSigned)
        `shouldBe` ["a1075db5 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │    4.4 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the txid of a signed transaction, which the commands take, and says it is signed" $ do
      let signed = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (rowOf signed)
        `shouldBe` ["b2d4f6a8 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ signed     │    4.4 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says a transaction sent that the sync has not seen yet is sent" $
      renderedLines (rowOf (preparedWith (Just (fakeTxHash 2)) True False))
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ sent       │    4.4 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says a transaction in conflict is conflicted, and leaves its fee rate empty" $
      renderedLines (rowOf (preparedWith (Just (fakeTxHash 2)) False True))
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ conflicted │        │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- "wallet0" takes 7, more than "Wallet": 7 + 2 = 9 under Wallet
    it "puts a column of the wallet before the portfolio when the database has several wallets" $
      concatMap renderedLines (tableHeader (pendingTableColumns UnitBitcoin WithWallets 12 [mainPlace]))
        `shouldBe` [ "Hash     │ Wallet  │ Portfolio  │ Network │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Tags         │ Description",
                     "─────────┼─────────┼────────────┼─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼──────────────┼────────────"
                   ]

    it "names the wallet of each transaction in that column, wallet0 too, and its portfolio alone in the next" $ do
      let elsewhere = Place otherRef btc
          both = pendingTableColumns UnitBitcoin WithWallets 12 [mainPlace, elsewhere]
      concatMap renderedLines [pendingRowDoc UnitBitcoin WithWallets (activePaths WithoutWallets) both elsewhere (Just seenTimeInCet) unsigned, pendingRowDoc UnitBitcoin WithWallets (activePaths WithoutWallets) both mainPlace (Just seenTimeInCet) unsigned]
        `shouldBe` [ "00000000 │ wallet2 │ portfolio1 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │    4.4 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "00000000 │ wallet0 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │    4.4 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    -- The one wallet may be wallet2, once the others were deleted
    it "leaves the wallet out when the database has one wallet, even when it is not wallet0" $ do
      let elsewhere = Place otherRef btc
          alone = pendingTableColumns UnitBitcoin WithoutWallets 12 [elsewhere]
      renderedLines (pendingRowDoc UnitBitcoin WithoutWallets (activePaths WithoutWallets) alone elsewhere (Just seenTimeInCet) unsigned)
        `shouldBe` ["00000000 │ portfolio1 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │    4.4 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "names the transaction a replacement replaces by its short id, before what it is for, as the compact view does" $ do
      let replacement = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [b2d4f6a8Txid])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (rowOf replacement)
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │    4.4 │              │ replaces b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the label of a payment in place of the address it pays, as the table of txs does" $
      renderedLines (rowOf unsigned {txInfoLabel = "Rent of October"})
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │    4.4 │              │ \"Rent of October\""]

    -- A transaction without a time shows "time unknown", whatever the time
    -- zone of the computer. The listing is one list, with no title.
    it "names where each transaction is in a chunk, under the header" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponsePendingTxs Nothing [(mainPlace, timeless)])
      renderedLines doc
        `shouldBe` [ "Hash     │ Portfolio  │ Network │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Tags         │ Description",
                     "─────────┼────────────┼─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼──────────────┼────────────",
                     "00000000 │ portfolio0 │ btc     │ time unknown      │ Outgoing │    -0.00041000 │ not signed │    4.4 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

verboseSpec :: Spec
verboseSpec =
  describe "The verbose view of a transaction" $ do
    -- pendingtxs --verbose names where each transaction is, first among
    -- the fields. A transaction without a time reads the same in every
    -- time zone.
    it "names the wallet of its portfolio in a listing, wallet0 too, when the database has several wallets" $ do
      let timeless = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty noTxTags Map.empty
      shown <- listedTxDoc (activePaths WithWallets) Verbose btc UnitBitcoin (Just mainPortfolioRef) Nothing timeless
      take 2 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Portfolio: portfolio0 of wallet0",
                     "  Network  : btc"
                   ]

    it "leaves the wallet out of a listing of one wallet, even when it is not wallet0" $ do
      let timeless = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty noTxTags Map.empty
      shown <- listedTxDoc (activePaths WithoutWallets) Verbose bch UnitBitcoin (Just otherRef) Nothing timeless
      take 2 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Portfolio: portfolio1",
                     "  Network  : bch"
                   ]

    -- The input is the coin of 50000 at /0/0. 40000 go out and 9000 come
    -- back as change to /1/0.
    it "lists the address a payment spends from, the one it pays, and its change, in columns" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002",
                     "  Short id : 00000000",
                     "  Time     : 14 Nov 2023 23:13:20 CET",
                     "  Fee      : 0.00001000 btc, 4.4 sat/vB",
                     "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0"
                   ]

    -- In satoshis, "50000 sats" takes 10 characters and "9000 sats" 9:
    -- the amounts are right-aligned, and the paths start in one column
    it "lines up the paths of the addresses of the portfolio after amounts of different widths" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitSatoshi [] Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  50000 sats  bip44/0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  40000 sats",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn   9000 sats  bip44/1/0"
                   ]

    it "gives the txid in full, then its short id, its first 8 digits" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Txid     : a1075db5c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : a1075db5"
                   ]

    it "gives the full count of the confirmations, past 999 too" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1234 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  1234 confirmations"]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  1 confirmation"]

    it "shows the label of a transaction, quoted, as its first field" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "Rent of October" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Label    : \"Rent of October\"",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    -- "  From     : " takes 13 characters
    it "shows the label of an address of the portfolio, quoted, on the line below, under the address" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "             \"Rent\""
                   ]

    -- The address spent from has the label Rent and the tags cold and
    -- kyc. The verbose view shows every tag of an address, below it,
    -- after its label.
    it "shows the tags of an address of the portfolio between brackets, after its label" $ do
      let tags = noTxTags {txAddressTags = Map.fromList [(externalAddr 0, [plainTag coldTag, ShownTag kycTag (Just TagRed)])]}
          info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty tags Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "             \"Rent\"  [cold] [kyc]"
                   ]

    -- The tags of the transaction have a field of their own, a space
    -- between them, without brackets: the field says they are tags
    it "gives the tags of the transaction a Tags field" $ do
      let tags = noTxTags {txOwnTags = [plainTag coldTag, plainTag kycTag]}
          info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty tags Map.empty
      filter (Text.isPrefixOf "  Tags" . Text.pack) (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` ["  Tags     : cold kyc"]

    -- The compact view shows the tags of the transaction alone: the tags
    -- of its addresses would fill the line
    it "leaves the tags of the addresses out of the compact view, and ends the line with those of the transaction" $ do
      let tags = TxTags [plainTag coldTag] (Map.fromList [(externalAddr 0, [plainTag kycTag])]) []
          info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty tags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at bip44/0/0, [cold]"

    -- The listing was asked to keep the transactions at addresses tagged
    -- kyc. The address has kyc and cold: only kyc is shown beside it.
    it "shows the tag a listing keeps beside the address that has it, in the compact view" $ do
      let tags = TxTags [] (Map.fromList [(externalAddr 0, [plainTag coldTag, plainTag kycTag])]) [kycTag]
          info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty tags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at bip44/0/0 [kyc]"

    -- The payment spends from bip44/0/0, which has the tag, and the
    -- second line names the address it pays: the address spent from
    -- follows on a line of its own, with its tag
    it "names an address that has the tag a listing keeps on a line of its own when the second line names another one" $ do
      let tags = TxTags [] (Map.fromList [(externalAddr 0, [plainTag kycTag])]) [kycTag]
          info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty tags Map.empty
      compactLinesBelow info
        `shouldBe` [ "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  bip44/0/0 [kyc]"
                   ]

    -- The batch payment spends from bip44/0/0, /0/1 and /0/2. The first
    -- and the last have the tag kyc, which the listing keeps. The middle
    -- one has the tag cold, which it does not keep.
    it "gives each such address a line of its own, in the order of their paths" $ do
      let tags = TxTags [] (Map.fromList [(externalAddr 2, [plainTag kycTag]), (externalAddr 1, [plainTag coldTag]), (externalAddr 0, [plainTag kycTag])]) [kycTag]
          info = (txOnChain batchSummary 2 (Store.BlockRef 800001 1) 2 False "") {txInfoTags = tags}
      compactLinesBelow info
        `shouldBe` [ "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 1 more",
                     "  bip44/0/0 [kyc]",
                     "  bip44/0/2 [kyc]"
                   ]

    -- The transaction has the tag rent, and the address it spends from
    -- has the tag kyc, which the listing keeps
    it "ends the second line with the tags of the transaction, above the line of such an address" $ do
      let tags = TxTags [plainTag rentTag] (Map.fromList [(externalAddr 0, [plainTag kycTag])]) [kycTag]
          info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty tags Map.empty
      compactLinesBelow info
        `shouldBe` [ "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, [rent]",
                     "  bip44/0/0 [kyc]"
                   ]

    -- A row of the table is one line. 1208000 is the balance of the
    -- portfolio after the payment.
    it "keeps such an address in the last cell of the table of txs, after a comma" $ do
      let tags = TxTags [] (Map.fromList [(externalAddr 0, [plainTag kycTag])]) [kycTag]
          info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty tags Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, bip44/0/0 [kyc]"]

    -- The coin spent is of transaction 1, the payment labelled Salary
    it "follows the label of an address spent from with the labels of its coins, after the word from" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" (Map.fromList [(fakeTxHash 1, "Salary")]) Map.empty noTxTags Map.empty
      (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 6)
        `shouldBe` "             \"Rent\"  from \"Salary\""

    -- The spending from the first change address, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn
    it "marks a change address spent from as one, on the line below the address" $ do
      let fromChange = spendSummary {summaryMyInputs = Map.fromList [(internalAddr 0, MyInputs 50000 (legacyPath (Deriv :/ 1 :/ 0)) "" [OutPoint (fakeTxHash 1) 0])]}
          info = TxInfo fromChange (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00050000 btc  bip44/1/0",
                     "             change address"
                   ]

    -- A transfer of the portfolio to its first change address, which the
    -- compact view names as the address paid
    it "marks a change address an internal transaction pays as one" $ do
      let toChange = spendSummary {summaryType = TxInternal, summaryOtherOutputs = Map.empty}
          info = TxInfo toChange (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to bip44/1/0 change address"

    it "gives the labels of the coins of an address spent from without a label of its own on the line below" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" (Map.fromList [(fakeTxHash 1, "Salary")]) Map.empty noTxTags Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "             from \"Salary\""
                   ]

    -- The payer spends 51000 from otherAddr 0, which is nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0"
                   ]

    -- The user may be the one who signs it, as in a sweep: 51000 are spent
    -- at 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    it "gives the amount an address of another wallet spends, while the transaction is not sent yet" $ do
      let shown = renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) (txHeldPending splitSummary))
      filter (isPrefixOf "  From") shown
        `shouldBe` ["  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00051000 btc"]

    -- 20000 to /0/0 of the portfolio, 30000 to another wallet
    it "lists every address a payment received pays while it is not sent yet, those of the portfolio first" $ do
      let shown = renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) (txHeldPending splitSummary))
      dropWhile (not . isPrefixOf "  To") shown
        `shouldBe` [ "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00020000 btc  bip44/0/0",
                     "             1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  0.00030000 btc"
                   ]

    -- The first line says +20000, what the portfolio gains: the 51000 the
    -- transaction spends are stated apart, before the fee
    it "says what a transaction not sent yet spends of coins that are not the portfolio's, before its fee" $ do
      let shown = renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) (txHeldPending splitSummary))
      take 3 (drop 3 shown)
        `shouldBe` [ "  Created  : 14 Nov 2023 23:13:20 CET",
                     "  Spends   : 0.00051000 btc of coins that are not this portfolio's",
                     "  Fee      : 0.00001000 btc, 4.4 sat/vB"
                   ]

    -- The same payment in a block: the 30000 to another wallet are the
    -- change of the payer, or what the payer paid to somebody else
    it "leaves out what a payment received pays to others, and what it spends, once it is on the chain" $ do
      let info = txOnChain splitSummary 1 (Store.BlockRef 800000 1) 3 False ""
      drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4.4 sat/vB",
                     "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00020000 btc  bip44/0/0"
                   ]

    -- 50000 paid + 1000 of fee = 51000 spent, which the portfolio neither
    -- gains nor loses: no sign
    it "heads a transaction between addresses of other wallets External, with the total it spends, fee included" $ do
      take 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) (txHeldPending externalSummary)))
        `shouldBe` ["14 Nov 2023 23:13  External      0.00051000 btc  not signed, 4.4 sat/vB"]

    -- The first line states the total already: no Spends line
    it "lists what an external transaction spends and pays, each address with its amount" $ do
      drop 3 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing Nothing (txOfFile externalSummary)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4.4 sat/vB",
                     "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00051000 btc",
                     "  To       : 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  0.00050000 btc"
                   ]

    -- The wallet does not hold it, so it has no time for it. "from a
    -- file" takes the 17 columns of a time.
    it "says from a file where the time goes, for a transaction in preparation read from a file" $ do
      take 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing Nothing (txOfFile spendSummary)))
        `shouldBe` ["from a file        Outgoing     -0.00041000 btc  not signed, 4.4 sat/vB"]

    -- The 30000 to another wallet are more than the 20000 to /0/0
    it "names the address of the portfolio a payment received pays in the compact view, before a larger output to another wallet" $ do
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) (txHeldPending splitSummary)) !! 1)
        `shouldBe` "  at bip44/0/0 and 1 more"

    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU sorts before
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ: the outputs come by address
    it "puts each address of a section on a line of its own, under the first" $ do
      let twoPaid = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 30000), (otherAddr 1, 10000)]}
          info = TxInfo twoPaid (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 6 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00030000 btc",
                     "             1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  0.00010000 btc"
                   ]

    it "names the transaction that replaced a dead one in full, under the words replaced by" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Status   : replaced by",
                     "             b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001"
                   ]

    it "names every transaction that replaced a dead one, one under the other" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid, a1075db5Second])) "" Map.empty Map.empty noTxTags Map.empty
      take 3 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Status   : replaced by",
                     "             b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001",
                     "             a1075db5c0ffee00000000000000000000000000000000000000000000000002"
                   ]

    -- The portfolio made the payment, so a replacement would spend one of
    -- its coins, and the portfolio would hold it
    it "says the network dropped a dead payment of the portfolio that nothing replaced" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  Status   : dropped from the network"

    -- Its payer can replace it with a transaction the portfolio never sees
    it "says a dead payment received was replaced or dropped, which the portfolio cannot tell apart" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  Status   : replaced, or dropped from the network"

    it "shows under the fee the change too small for an output that the fee took in" $ do
      let info = TxInfo spendSummary {summaryChangeInFee = Just 300} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4.4 sat/vB",
                     "             includes 0.00000300 btc of change too small for an output"
                   ]

    it "shows the lock time under the fee, as the block the transaction waits for, and no field without one" $ do
      let locked = TxInfo spendSummary {summaryLockTime = 814037} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
          unlocked = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) locked)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4.4 sat/vB",
                     "  Lock time: block 814037"
                   ]
      length (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) unlocked)) `shouldBe` 8

    -- 500,000,000 and above, a lock time is a Unix time; hw writes none
    it "shows a lock time of 500,000,000 or more as a Unix time" $ do
      let locked = TxInfo spendSummary {summaryLockTime = 1700000000} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) locked) !! 5
        `shouldBe` "  Lock time: Unix time 1700000000"

    it "shows an unsigned transaction by its nosigHash, in full then by its short id, with the time it was created" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      take 3 (drop 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  NoSigHash: a1075db5c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : a1075db5",
                     "  Created  : 14 Nov 2023 23:13:20 CET"
                   ]

    it "shows a signed transaction by its txid, in full then by its short id" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Txid     : b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : b2d4f6a8"
                   ]

    it "explains a conflict, and how to free the coins" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) "" Map.empty Map.empty noTxTags Map.empty
      (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  Conflict : A coin it spends was spent by another transaction, or is gone. It can no longer be sent. Delete it with deletetx."

    it "names every transaction a replacement replaces, one under the other" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5, fakeTxHash 6])) "" Map.empty Map.empty noTxTags Map.empty
      take 2 (drop 3 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Replaces : 0000000000000000000000000000000000000000000000000000000000000005",
                     "             0000000000000000000000000000000000000000000000000000000000000006"
                   ]

    -- The specification: the summary of a transaction gains its network
    -- and the types of the portfolio's addresses it involves
    it "gives in a listing the network of the transaction and the types of the addresses of the portfolio it involves, first" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      take 3 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin (whereFields btc Nothing info) Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  Network  : btc",
                     "  Types    : bip44"
                   ]

    -- The coin of 50000 at /0/0 of the bip44 account pays 40000 to /0/0
    -- of the bip84 account and 9000 of change to /1/0 of the bip44 one
    it "names both types for a transaction between the two accounts of the portfolio on btc" $ do
      let toSegwit =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(segwitExternalAddr 0, MyOutputs 40000 (segwitPath (Deriv :/ 0 :/ 0)) ""), (internalAddr 0, MyOutputs 9000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
              }
          info = TxInfo toSegwit (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty noTxTags Map.empty
      concatMap renderedLines (whereFields btc Nothing info) `shouldBe` ["Network  : btc", "Types    : bip44, bip84"]

    -- A sweep between outside addresses involves no address of a portfolio
    it "gives no types for a transaction that involves no address of the portfolio" $ do
      let outside = spendSummary {summaryMyInputs = Map.empty, summaryMyOutputs = Map.empty}
          info = TxInfo outside (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty noTxTags Map.empty
      concatMap renderedLines (whereFields btc Nothing info) `shouldBe` ["Network  : btc"]

answerSpec :: Spec
answerSpec =
  describe "The transaction a command answers with" $ do
    -- A transaction read from a file has no time, so the clock of the
    -- computer plays no part
    let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty noTxTags Map.empty

    it "names its network below its first line, outside the wallet" $ do
      shown <- txAnswerDoc WithoutWallets btc Nothing UnitBitcoin info
      (renderedLines shown !! 1) `shouldBe` "  Network  : btc"

    it "names its portfolio, then its network and the types of the portfolio's addresses it involves, below its first line" $ do
      shown <- txAnswerDoc WithoutWallets btc (Just (SeenFrom mainPortfolioRef (Just mainPortfolioRef))) UnitBitcoin info
      take 3 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Portfolio: portfolio0",
                     "  Network  : btc",
                     "  Types    : bip44"
                   ]

    it "names the wallet of its portfolio in a database of several wallets" $ do
      shown <- txAnswerDoc WithWallets bch (Just (SeenFrom otherRef (Just otherRef))) UnitBitcoin info
      take 2 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Portfolio: portfolio1 of wallet2",
                     "  Network  : bch"
                   ]

    it "names wallet0 too in a database of several wallets" $ do
      shown <- txAnswerDoc WithWallets btc (Just (SeenFrom mainPortfolioRef (Just mainPortfolioRef))) UnitBitcoin info
      (renderedLines shown !! 1) `shouldBe` "  Portfolio: portfolio0 of wallet0"

    -- A transfer from portfolio0 to portfolio1 is of portfolio0, which
    -- pays it, while portfolio1 is active
    it "says its portfolio is not the active one, and names the active one, when another portfolio is active" $ do
      shown <- txAnswerDoc WithoutWallets btc (Just (SeenFrom mainPortfolioRef (Just (PortfolioRef 0 1)))) UnitBitcoin info
      (renderedLines shown !! 1) `shouldBe` "  Portfolio: portfolio0, not the active portfolio1"

    -- The transaction is of portfolio0 of wallet0, while portfolio1 of
    -- wallet2 is active
    it "names the wallet of its portfolio and that of the active one in a database of several wallets" $ do
      shown <- txAnswerDoc WithWallets btc (Just (SeenFrom mainPortfolioRef (Just otherRef))) UnitBitcoin info
      (renderedLines shown !! 1) `shouldBe` "  Portfolio: portfolio0 of wallet0, not the active portfolio1 of wallet2"

    -- An answer read back from JSON, which leaves the active portfolio out
    it "says nothing of the active portfolio when the answer does not know it" $ do
      shown <- txAnswerDoc WithoutWallets btc (Just (SeenFrom mainPortfolioRef Nothing)) UnitBitcoin info
      (renderedLines shown !! 1) `shouldBe` "  Portfolio: portfolio0"

    -- A payment on the chain between portfolio0 and portfolio1 is shown
    -- once for each, while portfolio1 is active
    it "says in each view of a transaction of two portfolios whether that portfolio is not the active one" $ do
      shown <- heldTxsDoc WithoutWallets UnitBitcoin (Just (PortfolioRef 0 1)) [(mainPlace, info), (Place (PortfolioRef 0 1) btc, info)]
      filter (Text.isPrefixOf "  Portfolio" . Text.pack) (renderedLines shown)
        `shouldBe` [ "  Portfolio: portfolio0, not the active portfolio1",
                     "  Portfolio: portfolio1"
                   ]

    it "gives its label after where it is" $ do
      shown <- txAnswerDoc WithoutWallets btc (Just (SeenFrom mainPortfolioRef (Just mainPortfolioRef))) UnitBitcoin info {txInfoLabel = "Rent of October"}
      take 4 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Portfolio: portfolio0",
                     "  Network  : btc",
                     "  Types    : bip44",
                     "  Label    : \"Rent of October\""
                   ]

    -- A label from a file that tries to write a recipient line of its own
    it "writes out a line break in its label, which stays on the line of the label" $ do
      shown <- txAnswerDoc WithoutWallets btc (Just (SeenFrom mainPortfolioRef (Just mainPortfolioRef))) UnitBitcoin info {txInfoLabel = "Rent\n  To       : 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"}
      take 2 (drop 4 (renderedLines shown))
        `shouldBe` [ "  Label    : \"Rent\\n  To       : 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ\"",
                     "  NoSigHash: 0000000000000000000000000000000000000000000000000000000000000007"
                   ]

{- Listings in sections -}

sectionSpec :: Spec
sectionSpec =
  describe "A listing in sections, on the screen" $ do
    -- The coin has no time, so the clock of the computer plays no part
    let coin = spendableCoinRow {jsonCoinTime = Nothing}
        coinLines =
          [ "00000000:0  time unknown           0.00050000 btc  spendable, 3 confirmations",
            "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0"
          ]

    it "gives a section no line of its place when the options narrow the listing to one place" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseCoins [mainGroup [coin]])
      renderedLines doc `shouldBe` coinLines

    it "gives a section the line of its place, then a blank line, when the listing may cover several places" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt True True True) (ResponseCoins [mainGroup [coin]])
      renderedLines doc `shouldBe` ["portfolio0  btc", ""] <> coinLines

    -- The section is of portfolio1 of wallet2, in a database of several
    -- wallets, so its line names the wallet
    it "sets a section that starts below another apart from it by a blank line" $ do
      doc <- chunkDoc UnitBitcoin Compact WithWallets (TagWidths 12 12 12) (ChunkAt False True True) (ResponseCoins [Group (Place otherRef btc) [AccountCounts Bip44 1 0] [coin]])
      renderedLines doc `shouldBe` ["", "portfolio1 of wallet2  btc", ""] <> coinLines

    it "gives a chunk that goes on in its section its rows alone, after the blank line that sets them apart" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt False False True) (ResponseCoins [mainGroup [coin]])
      renderedLines doc `shouldBe` "" : coinLines

    it "gives each section of the addresses the line of its account" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt True True True) (ResponseAddresses [Group mainPlace [AccountCounts Bip84 2 0] [(segwitAddressRow, Holds (CoinTotals 0 0), [])]])
      renderedLines doc
        `shouldBe` ["portfolio0  btc  bip84", "", "bip84/0/1  bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2", "  unused"]

    -- The account has 12 receiving and 3 change addresses: the paths are
    -- padded to bip44/0/11, of 10 characters
    it "always gives the sections of balances the line of their place with the total of the network, though the listing is narrowed to it" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseBalances [(mainGroup [(paidAddressRow, CoinBalance 80000 50000 30000 0 0 0, [])], CoinBalance 130000 100000 30000 0 0 0)])
      renderedLines doc
        `shouldBe` [ "portfolio0  btc  total 0.00130000 btc, spendable 0.00100000 btc, unspendable 0.00030000 btc",
                     "",
                     "bip44/0/0   1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc"
                   ]

    -- balances lists the addresses of both accounts of btc together. The
    -- bip44 account has 12 receiving addresses, so the paths are padded to
    -- bip44/0/11, of 10 characters. 80000 + 20000 = 100000 in all, of
    -- which 50000 + 20000 = 70000 can be spent.
    it "gives each address of balances the type of its account before its path, as the section lists both types" $ do
      let both = Group mainPlace [AccountCounts Bip44 12 3, AccountCounts Bip84 2 0] [(paidAddressRow, CoinBalance 80000 50000 30000 0 0 0, []), (segwitAddressRow, CoinBalance 20000 20000 0 0 0 0, [])]
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponseBalances [(both, CoinBalance 100000 70000 30000 0 0 0)])
      renderedLines doc
        `shouldBe` [ "portfolio0  btc  total 0.00100000 btc, spendable 0.00070000 btc, unspendable 0.00030000 btc",
                     "",
                     "bip44/0/0   1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc",
                     "",
                     "bip84/0/1   bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2",
                     "  total 0.00020000 btc, spendable 0.00020000 btc, unspendable 0.00000000 btc"
                   ]

    it "says once that a listing has no rows, without the line of a place" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt True True True) (ResponseTxs [])
      renderedLines doc `shouldBe` ["There are no transactions to display"]

-- The pager keeps the first lines it is given on the screen, for the
-- header of a table taller than the screen
headerSpec :: Spec
headerSpec =
  describe "Whether a listing starts with the header of a table, which the pager keeps on the screen" $ do
    let txs = ResponseTxs [mainGroup []]

    it "is true of a table narrowed to one place" $
      startsWithHeader Table (ChunkAt True True False) txs `shouldBe` True

    it "is false of a table whose sections say where they are, since the line of a place comes first" $
      startsWithHeader Table (ChunkAt True True True) txs `shouldBe` False

    it "is false of a view that is no table" $
      startsWithHeader Compact (ChunkAt True True False) txs `shouldBe` False

    it "is false of the table of balances, whose sections always say where they are" $
      startsWithHeader Table (ChunkAt True True False) (ResponseBalances [(mainGroup [], CoinBalance 0 0 0 0 0 0)]) `shouldBe` False

    it "is true of the table of portfolios, which is one list" $
      startsWithHeader Table (ChunkAt True True False) (ResponsePortfolios [mainPortfolioView]) `shouldBe` True

{- Next steps -}

nextStepsSpec :: Spec
nextStepsSpec = do
  -- send, sendall, sweep and broadcast sync the wallet once the
  -- transaction is sent
  describe "The next steps after a transaction sent" $ do
    let sent synced = renderedLines <$> nextStepsPrinter (CommandBroadcast Nothing Nothing True Nothing) (ResponseSentTx btc (fakeTxHash 1) synced)

    it "are none when the wallet synced after it" $
      sent (SyncAfter [quietSync {syncResTxUpdates = 1}] Nothing) `shouldBe` Nothing

    it "are to sync the wallet when it could not sync after it" $
      sent (SyncAfter [] (Just downStore))
        `shouldBe` Just
          [ "Next, sync the wallet to follow the transaction:",
            "  hw sync"
          ]

    -- The sync that follows a sending looks at every pending transaction
    -- of the portfolio
    it "are those of sync when the sync found another pending transaction in conflict" $
      sent (SyncAfter [quietSync {syncResTxUpdates = 1, syncResConflicted = 1}] Nothing)
        `shouldBe` Just
          [ "Next, find the pending transaction in conflict, which can no longer be sent:",
            "  hw pendingtxs",
            "Then delete it, to free its coins:",
            "  hw deletetx HASH"
          ]

  describe "The next steps after a sync" $ do
    -- portfolio0 of wallet0 is the active portfolio
    let synced results = renderedLines <$> nextStepsPrinter (CommandSync Nothing False) (ResponseSync results [] [] (Just mainPortfolioRef))
        holdingIn ref = quietSync {syncResPlace = Place ref btc}
        conflictIn ref net = quietSync {syncResPlace = Place ref net, syncResConflicted = 1}
        updatedIn ref = quietSync {syncResPlace = Place ref btc, syncResTxUpdates = 2}

    -- txs would show what it showed before the sync
    it "are none when the sync found no new transaction" $
      synced [quietSync, holdingIn otherRef] `shouldBe` Nothing

    -- txs lists the active portfolio: no option names it
    it "name no portfolio when the sync found new transactions in the active portfolio" $
      synced [updatedIn mainPortfolioRef] `shouldBe` Just ["Next, see the transactions:", "  hw txs"]

    it "name no portfolio when the sync found new transactions in the active portfolio, and in another one" $
      synced [updatedIn mainPortfolioRef, updatedIn otherRef] `shouldBe` Just ["Next, see the transactions:", "  hw txs"]

    -- txs lists the active portfolio alone, which would show none of the
    -- new transactions
    it "start with the switch to the portfolio where the sync found new transactions, when it is not the active one" $
      synced [quietSync, updatedIn otherRef]
        `shouldBe` Just
          [ "Next, switch to portfolio1 of wallet2, where the sync found new transactions:",
            "  hw switch 1 --wallet 2",
            "Then see the transactions:",
            "  hw txs"
          ]

    it "switch in turn to each of the portfolios where the sync found new transactions, when none is the active one" $
      synced [updatedIn (PortfolioRef 0 1), updatedIn otherRef]
        `shouldBe` Just
          [ "Next, switch in turn to each portfolio where the sync found new transactions:",
            "  hw switch 1 --wallet 0",
            "  hw switch 1 --wallet 2",
            "In each, see the transactions:",
            "  hw txs"
          ]

    it "name no portfolio in the steps that find a pending transaction in conflict in the active portfolio" $
      synced [holdingIn (PortfolioRef 0 1), conflictIn mainPortfolioRef btc]
        `shouldBe` Just
          [ "Next, find the pending transaction in conflict, which can no longer be sent:",
            "  hw pendingtxs",
            "Then delete it, to free its coins:",
            "  hw deletetx HASH"
          ]

    -- pendingtxs lists every portfolio, and deletetx finds the portfolio
    -- of the transaction by itself
    it "name no portfolio either for a pending transaction in conflict in another portfolio than the active one" $
      synced [quietSync, conflictIn (PortfolioRef 0 1) btc]
        `shouldBe` Just
          [ "Next, find the pending transaction in conflict, which can no longer be sent:",
            "  hw pendingtxs",
            "Then delete it, to free its coins:",
            "  hw deletetx HASH"
          ]

    -- One in the active portfolio, one in a portfolio of another wallet
    it "speak of the pending transactions in conflict in the plural when the sync found several" $
      synced [conflictIn mainPortfolioRef btc, conflictIn otherRef bch]
        `shouldBe` Just
          [ "Next, find the pending transactions in conflict, which can no longer be sent:",
            "  hw pendingtxs",
            "Then delete them, to free their coins:",
            "  hw deletetx HASH"
          ]

    -- Every sync would send it again, and it keeps its coins locked
    it "are to delete the pending transaction the store did not publish, by its short id" $
      synced [quietSync {syncResNotPublished = [(fakeTxHash 9, "The fee is too low")]}]
        `shouldBe` Just
          [ "Next, delete the pending transaction that was not sent again, to free its coins:",
            "  hw deletetx 00000000"
          ]

    it "name each of several pending transactions the store did not publish" $
      synced [quietSync {syncResNotPublished = [(fakeTxHash 9, "The fee is too low"), (typedTxid "a1075db500000000000000000000000000000000000000000000000000000000", "The fee is too low")]}]
        `shouldBe` Just
          [ "Next, delete the pending transactions that were not sent again, to free their coins:",
            "  hw deletetx 00000000",
            "  hw deletetx a1075db5"
          ]

    it "come after those of a conflict, for a sync that found both" $
      synced [conflictIn mainPortfolioRef btc, quietSync {syncResNotPublished = [(fakeTxHash 9, "The fee is too low")]}]
        `shouldBe` Just
          [ "Next, find the pending transaction in conflict, which can no longer be sent:",
            "  hw pendingtxs",
            "Then delete it, to free its coins:",
            "  hw deletetx HASH",
            "Also delete the pending transaction that was not sent again, to free its coins:",
            "  hw deletetx 00000000"
          ]

    it "are none after a sync that synced nothing" $
      synced [] `shouldBe` Nothing

  -- The step names the wallet always: the active portfolio may be of
  -- another one
  describe "The next steps after a command that created a portfolio" $ do
    let existing = CommandNewPortfolio (NewPortfolio True False Nothing False Nothing Nothing False False)
        secondView = wholePortfolioView {viewRef = PortfolioRef 0 1, viewActive = False}
    it "are the switch to the portfolio newportfolio created, when another one stays active" $
      (renderedLines <$> nextStepsPrinter existing (ResponseNewPortfolio Nothing Nothing secondView (ActiveAfter (PortfolioRef 0 0) False) Nothing))
        `shouldBe` Just ["To work in portfolio1 of wallet0, make it the active one:", "  hw switch 1 --wallet 0"]

    it "are none when newportfolio made the portfolio it created the active one" $
      (renderedLines <$> nextStepsPrinter existing (ResponseNewPortfolio Nothing Nothing wholePortfolioView (ActiveAfter (PortfolioRef 0 0) True) Nothing))
        `shouldBe` Nothing

    -- recover created portfolios 0 and 2 of wallet1, and portfolio0 of
    -- wallet0 stays active
    it "are the switch to the first portfolio recover created, when another one stays active" $
      (renderedLines <$> nextStepsPrinter (CommandRecover (Recover 10)) (ResponseRecover [btc] 10 (Just 1) [0, 2] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) False)) Nothing))
        `shouldBe` Just ["To work in portfolio0 of wallet1, make it the active one:", "  hw switch 0 --wallet 1"]

    -- A new wallet always gets portfolio0, which becomes the active one
    -- in a database without a portfolio. recover found portfolio3 alone
    -- paid, and the user who lists the active portfolio would see nothing.
    it "are the switch to the portfolio recover found paid, when the empty portfolio0 of its new wallet became the active one" $
      (renderedLines <$> nextStepsPrinter (CommandRecover (Recover 10)) (ResponseRecover [btc] 10 (Just 0) [3] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) True)) Nothing))
        `shouldBe` Just ["To work in portfolio3 of wallet0, make it the active one:", "  hw switch 3 --wallet 0"]

    it "are none when recover made a portfolio it created the active one" $
      (renderedLines <$> nextStepsPrinter (CommandRecover (Recover 10)) (ResponseRecover [btc] 10 (Just 0) [0] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) True)) Nothing))
        `shouldBe` Nothing

    it "are the switch to the first portfolio importwallet created, when another one stays active" $
      (renderedLines <$> nextStepsPrinter (CommandImportWallet wholeFile "wallet.jsonl") (ResponseImport (importedWith [WalletImport 1 testFingerprint2 True Nothing [PortfolioImport 0 True Nothing]] []) (Just (ActiveAfter (PortfolioRef 0 0) False))))
        `shouldBe` Just ["To work in portfolio0 of wallet1, make it the active one:", "  hw switch 0 --wallet 1"]

    it "are none when importwallet made a portfolio it created the active one" $
      (renderedLines <$> nextStepsPrinter (CommandImportWallet wholeFile "wallet.jsonl") (ResponseImport (importedWith [WalletImport 0 testFingerprint True Nothing [PortfolioImport 0 True Nothing]] []) (Just (ActiveAfter (PortfolioRef 0 0) True))))
        `shouldBe` Nothing

    -- The file labels a coin the wallet does not have yet
    it "are a sync, then the same import again, for the labels importwallet could not place" $
      (renderedLines <$> nextStepsPrinter (CommandImportWallet wholeFile "wallet.jsonl") (ResponseImport ((importedWith [] []) {reportCoinsMissing = 1}) Nothing))
        `shouldBe` Just
          [ "Next, sync the wallet, for the addresses, transactions and coins the file labels:",
            "  hw sync",
            "Then import the file again, for the labels it could not place:",
            "  hw importwallet wallet.jsonl"
          ]

    -- The second import reads no more of the file than the first one
    it "name again the wallet and the portfolio of the file that importwallet was given, in the import to run again" $
      (renderedLines <$> nextStepsPrinter (CommandImportWallet (ImportScope (Just "bc560f95") (Just 1)) "wallet.jsonl") (ResponseImport ((importedWith [] []) {reportCoinsMissing = 1}) Nothing))
        `shouldBe` Just
          [ "Next, sync the wallet, for the addresses, transactions and coins the file labels:",
            "  hw sync",
            "Then import the file again, for the labels it could not place:",
            "  hw importwallet wallet.jsonl --wallet bc560f95 --portfolio 1"
          ]

{- Paths of another portfolio -}

-- A path alone is of the active portfolio, in a view of the active
-- portfolio. Any other path names its portfolio after it. portfolio0 is
-- the portfolio of the shared story, and portfolio1 the other one of the
-- wallet.
otherPortfolioSpec :: Spec
otherPortfolioSpec = do
  describe "A payment to another portfolio of the wallet, with every detail" $ do
    -- portfolio0 is active and pays: its own paths are alone
    it "names the portfolio of the address it pays after the path of that address" $
      drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) transfer))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0"
                   ]

    -- "  To       : " takes 13 characters
    it "shows the label the address has in its portfolio, quoted, on the line below, under the address" $
      take 2 (drop 6 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) transfer {txInfoElsewhere = inPortfolio1 "Savings"})))
        `shouldBe` [ "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1",
                     "             \"Savings\""
                   ]

    it "names the wallet of the other portfolio in a database of several wallets" $ do
      let inWallet2 = Map.fromList [(account1Addr, HeldElsewhere otherRef (legacyPath (Deriv :/ 0 :/ 0)) "")]
      (renderedLines (verboseTxDoc (activePaths WithWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) transfer {txInfoElsewhere = inWallet2}) !! 6)
        `shouldBe` "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1 of wallet2"

    -- The other portfolio is portfolio1 of wallet0. Its name alone would
    -- read as the portfolio1 of any wallet.
    it "names wallet0 after the other portfolio too in a database of several wallets" $
      (renderedLines (verboseTxDoc (activePaths WithWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) transfer) !! 6)
        `shouldBe` "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1 of wallet0"

    -- portfolio1 is active, and the transfer is of portfolio0, which pays
    -- it: the answer has three fields that say where it is above the
    -- five of 'verboseTxDoc'
    it "names the portfolio after every path when another portfolio is active, the active one too" $ do
      shown <- txAnswerDoc WithoutWallets btc (Just (SeenFrom mainPortfolioRef (Just portfolio1Ref))) UnitBitcoin transfer
      drop 8 (renderedLines shown)
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0",
                     "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0 of portfolio0"
                   ]

    -- An answer read back from JSON, which leaves the active portfolio out
    it "leaves the paths of its own portfolio alone when the answer does not know the active portfolio" $ do
      shown <- txAnswerDoc WithoutWallets btc (Just (SeenFrom mainPortfolioRef Nothing)) UnitBitcoin transfer
      drop 8 (renderedLines shown)
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0"
                   ]

  describe "A payment received from another portfolio of the wallet, with every detail" $
    -- The payment of the shared story, paid by portfolio1 from its coin of
    -- 51000 at bip44/0/0. An address of another wallet shows no amount
    -- there.
    it "gives what the address of the other portfolio spends, and names its portfolio after its path" $ do
      let received = TxInfo paymentSummary {summaryOtherInputs = Map.fromList [(account1Addr, 51000)]} (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty (inPortfolio1 "") noTxTags Map.empty
      drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) received))
        `shouldBe` [ "  From     : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00051000 btc  bip44/0/0 of portfolio1",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0"
                   ]

  -- portfolio0 of wallet0 pays portfolio1 of wallet2, which is active.
  -- show gives a view of the payment for each of the two portfolios. A
  -- portfolio named alone, as "portfolio0", would read as that of the
  -- wallet of the view, so every portfolio comes with its wallet.
  describe "A payment between portfolios of two wallets, with every detail" $ do
    let placing = filter (\shown -> any (`Text.isPrefixOf` Text.pack shown) ["  Portfolio", "  From", "  To", "  Change"]) . renderedLines

    -- The view of the portfolio that pays, which is not the active one:
    -- every path names its portfolio
    it "names the wallet after every portfolio in the view of the portfolio that pays" $ do
      let toWallet2 = Map.fromList [(account1Addr, HeldElsewhere otherRef (legacyPath (Deriv :/ 0 :/ 0)) "")]
      shown <- txAnswerDoc WithWallets btc (Just (SeenFrom mainPortfolioRef (Just otherRef))) UnitBitcoin transfer {txInfoElsewhere = toWallet2}
      placing shown
        `shouldBe` [ "  Portfolio: portfolio0 of wallet0, not the active portfolio1 of wallet2",
                     "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0 of wallet0",
                     "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1 of wallet2",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0 of portfolio0 of wallet0"
                   ]

    -- The view of the portfolio that is paid, the active one: its own
    -- path is alone, and the address that pays is of portfolio0 of
    -- wallet0. The payment is that of the shared story, paid from a coin
    -- of 51000.
    it "names wallet0 after the portfolio that pays, in the view of the active portfolio it pays" $ do
      let fromWallet0 = Map.fromList [(account1Addr, HeldElsewhere mainPortfolioRef (legacyPath (Deriv :/ 0 :/ 0)) "")]
          received = TxInfo paymentSummary {summaryOtherInputs = Map.fromList [(account1Addr, 51000)]} (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty fromWallet0 noTxTags Map.empty
      shown <- txAnswerDoc WithWallets btc (Just (SeenFrom otherRef (Just otherRef))) UnitBitcoin received
      placing shown
        `shouldBe` [ "  Portfolio: portfolio1 of wallet2",
                     "  From     : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00051000 btc  bip44/0/0 of portfolio0 of wallet0",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0"
                   ]

  describe "A payment to another portfolio of the wallet, in a listing" $ do
    it "names the address it pays by its path and its portfolio, in place of the address" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) transfer) !! 1)
        `shouldBe` "  to bip44/0/0 of portfolio1"

    it "keeps the path before the label of the transaction, which says what the payment is for" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) transfer {txInfoLabel = "Rent of October"}) !! 1)
        `shouldBe` "  to bip44/0/0 of portfolio1 \"Rent of October\""

    it "gives the label the address has in its portfolio when the transaction has none" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) transfer {txInfoElsewhere = inPortfolio1 "Savings"}) !! 1)
        `shouldBe` "  to bip44/0/0 of portfolio1 \"Savings\""

    -- 1208000 is the balance of portfolio0 after the transfer
    it "names the address the same way in the last column of the table of txs" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) transfer)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to bip44/0/0 of portfolio1"]

    it "names the wallet after that portfolio in a database of several wallets, wallet0 too" $
      (renderedLines (compactTxDoc (activePaths WithWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) transfer) !! 1)
        `shouldBe` "  to bip44/0/0 of portfolio1 of wallet0"

    it "names the wallet the same way in the last column of the table of txs" $
      renderedLines (txRowDoc (activePaths WithWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) transfer)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to bip44/0/0 of portfolio1 of wallet0"]

  -- pendingtxs lists every portfolio. The transaction moves the coin of
  -- 50000 of portfolio0 to its first change address, bip44/1/0, which
  -- gets 49000: 50000 - 1000 of fee. It has no time, so that it reads the
  -- same in every time zone.
  describe "A pending transaction of another portfolio than the active one, as pendingtxs lists it" $ do
    let movedSummary =
          spendSummary
            { summaryType = TxInternal,
              summaryAmount = -1000,
              summaryOtherOutputs = Map.empty,
              summaryMyOutputs = Map.fromList [(internalAddr 0, MyOutputs 49000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
            }
        moved = TxInfo movedSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty noTxTags Map.empty
        secondLine doc = renderedLines doc !! 1

    it "names its portfolio after the path of the address it pays" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponsePendingTxs (Just portfolio1Ref) [(mainPlace, moved)])
      secondLine doc `shouldBe` "  portfolio0 btc, to bip44/1/0 of portfolio0 change address"

    it "leaves the path alone when its portfolio is the active one" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponsePendingTxs (Just mainPortfolioRef) [(mainPlace, moved)])
      secondLine doc `shouldBe` "  portfolio0 btc, to bip44/1/0 change address"

    it "names the wallet after the portfolio, wallet0 too, when the database has several wallets" $ do
      doc <- chunkDoc UnitBitcoin Compact WithWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponsePendingTxs (Just portfolio1Ref) [(mainPlace, moved)])
      secondLine doc `shouldBe` "  portfolio0 of wallet0 btc, to bip44/1/0 of portfolio0 of wallet0 change address"

    -- The row comes under the two lines of the header. The transaction
    -- costs its fee of 1000, at 4.4 sat/vB.
    it "names its portfolio after the path in the last column of the table" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponsePendingTxs (Just portfolio1Ref) [(mainPlace, moved)])
      (renderedLines doc !! 2)
        `shouldBe` "00000000 │ portfolio0 │ btc     │ time unknown      │ Internal │    -0.00001000 │ not signed │    4.4 │              │ to bip44/1/0 of portfolio0 change address"

    it "names its portfolio after every path with --verbose" $ do
      doc <- chunkDoc UnitBitcoin Verbose WithoutWallets (TagWidths 12 12 12) (ChunkAt True True False) (ResponsePendingTxs (Just portfolio1Ref) [(mainPlace, moved)])
      filter (Text.isInfixOf "bip44" . Text.pack) (renderedLines doc)
        `shouldBe` [ "  Types    : bip44",
                     "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0",
                     "  To       : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00049000 btc  bip44/1/0 of portfolio0"
                   ]

  -- spendableCoinRow is a coin of portfolio0, and portfolio1 is active
  describe "A coin of another portfolio than the active one" $ do
    let coin = spendableCoinRow {jsonCoinTime = Nothing}
        fromPortfolio0 = pathsSeenFrom WithoutWallets (SeenFrom mainPortfolioRef (Just portfolio1Ref))

    it "names its portfolio after the path of its address, in the compact view of a report" $
      (renderedLines (compactCoinDoc fromPortfolio0 btc UnitBitcoin Nothing coin) !! 1)
        `shouldBe` "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0 of portfolio0"

    it "keeps the label of its address after the portfolio, on the line of the address" $
      (renderedLines (compactCoinDoc fromPortfolio0 btc UnitBitcoin Nothing coin {jsonCoinAddressLabel = "Wages"}) !! 1)
        `shouldBe` "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0 of portfolio0  \"Wages\""

    -- lockcoin finds a coin of portfolio0 while portfolio1 is active
    it "names its portfolio after the path of its address, in the answer of lockcoin, unlockcoin and show" $ do
      shown <- coinAnswerDoc WithoutWallets UnitBitcoin (Just portfolio1Ref) mainAccountRef coin
      (renderedLines shown !! 4) `shouldBe` "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0 of portfolio0"

    -- The coin is of portfolio0 of wallet0, while portfolio1 of wallet2
    -- is active
    it "names the wallet after each portfolio of that answer in a database of several wallets, wallet0 too" $ do
      shown <- coinAnswerDoc WithWallets UnitBitcoin (Just otherRef) mainAccountRef coin
      drop 4 (renderedLines shown)
        `shouldBe` [ "  Address : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0 of portfolio0 of wallet0",
                     "  Account : portfolio0 of wallet0, not the active portfolio1 of wallet2  btc  bip44"
                   ]

    -- bumpfee replaces a transaction of portfolio0 while portfolio1 is
    -- active, and the replacement leaves the coin, locked by the user. The
    -- coins left come after the 10 lines of the transaction and a blank
    -- line.
    it "names its portfolio after the path of its address, among the coins a transaction just prepared leaves" $ do
      let prepared = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty noTxTags Map.empty
          left = LeftCoin coin {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False} (Locked LockedByUser) []
      printed <- printedLines (ResponsePrepared mainPlace prepared [left] (Just portfolio1Ref))
      drop 11 printed
        `shouldBe` [ "1 coin stays in the portfolio:",
                     "",
                     "00000000:0  time unknown           0.00050000 btc  locked by you, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0 of portfolio0",
                     "  you locked it"
                   ]

  -- segwitAddressRow is bip84/0/1 of portfolio0
  describe "An address of another portfolio than the active one, as show and label answer with it" $ do
    it "names its portfolio after its path" $ do
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just portfolio1Ref) segwitAccountRef segwitAddressRow (Holds (CoinTotals 0 0)) [] Nothing
      take 1 (renderedLines shown) `shouldBe` ["bip84/0/1 of portfolio0  bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2"]

    it "gives its path alone when its portfolio is the active one" $ do
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just mainPortfolioRef) segwitAccountRef segwitAddressRow (Holds (CoinTotals 0 0)) [] Nothing
      take 1 (renderedLines shown) `shouldBe` ["bip84/0/1  bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2"]

    -- The address is of portfolio0 of wallet0, while portfolio1 of
    -- wallet2 is active
    it "names the wallet after the portfolio of its path in a database of several wallets, wallet0 too" $ do
      shown <- addressAnswerDoc WithWallets WithoutCode UnitBitcoin (Just otherRef) segwitAccountRef segwitAddressRow (Holds (CoinTotals 0 0)) [] Nothing
      take 1 (renderedLines shown) `shouldBe` ["bip84/0/1 of portfolio0 of wallet0  bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2"]

{- The address book -}

-- | The contact of the landlord: 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, the
-- address of another wallet that the spending of the shared story pays
-- 40000 to, and that the payment received was paid from. It was added at
-- 'seenTime'.
landlordContact :: Contact
landlordContact = Contact "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" "Landlord" [] seenTime

-- | A transaction whose addresses of others include the contact given,
-- at the address of the landlord
withContact :: Contact -> TxInfo -> TxInfo
withContact contact info = info {txInfoContacts = Map.fromList [(otherAddr 0, contact)]}

-- | The spending of the shared story, with the label given: 40000 to the
-- address of the landlord, 9000 of change, 1000 of fee
spendingLabelled :: Text -> TxInfo
spendingLabelled label = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) label Map.empty Map.empty noTxTags Map.empty

-- | The second line of the compact view of a transaction
compactSecondLine :: TxInfo -> String
compactSecondLine info = renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1

-- | The lines of the compact view of a transaction below its first one
compactLinesBelow :: TxInfo -> [String]
compactLinesBelow info = drop 1 (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info))

-- | The lines of the verbose view of a transaction, from the field given
verboseFrom :: String -> TxInfo -> [String]
verboseFrom fieldName info =
  dropWhile ((/= fieldName) . take (length fieldName)) (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))

addressBookSpec :: Spec
addressBookSpec = do
  describe "An address of the address book, in the compact view of a transaction" $ do
    it "is followed by the label of its contact" $
      compactSecondLine (withContact landlordContact (spendingLabelled ""))
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\""

    it "is followed by the words address book when its contact has no label" $
      compactSecondLine (withContact landlordContact {contactLabel = ""} (spendingLabelled ""))
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU address book"

    -- Without a contact, the label of a payment takes the place of the
    -- address it pays
    it "is named before the label of the payment, which takes the place of the label of the contact" $
      compactSecondLine (withContact landlordContact (spendingLabelled "Rent of October"))
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Rent of October\""

    it "leaves the line of a payment to a stranger as it is" $
      compactSecondLine (spendingLabelled "")
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    -- 1208000 is the balance of the portfolio after the payment
    it "is named the same way in the last column of the table of txs" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 1208000) (Just seenTimeInCet) (withContact landlordContact (spendingLabelled "")))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │              │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\""]

    -- The address is of portfolio 1, and the address book has it too
    it "is named as an address of the wallet when another portfolio holds it" $
      compactSecondLine transfer {txInfoContacts = Map.fromList [(account1Addr, landlordContact)]}
        `shouldBe` "  to bip44/0/0 of portfolio1"

  describe "A contact of the address book, in a listing that keeps the transactions at a tag (--address-tag)" $ do
    let keeping tags info = info {txInfoTags = (txInfoTags info) {txKeptAddressTags = tags}}
        tagged = landlordContact {contactTags = [plainTag kycTag, plainTag rentTag]}
        received = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty

    -- The contact has the tags kyc and rent, and the listing keeps rent
    it "shows the tag asked for after the label of the contact a payment pays, and no other tag of the contact" $
      compactSecondLine (keeping [rentTag] (withContact tagged (spendingLabelled "")))
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" [rent]"

    -- A payment received names the address it was paid at. The contact
    -- that paid it is what put the payment in the listing. Its address
    -- is long, so it follows on a line of its own.
    it "names the contact a payment received was paid from on a line of its own, with its label and the tag asked for" $
      compactLinesBelow (keeping [rentTag] (withContact tagged received))
        `shouldBe` [ "  at bip44/0/0",
                     "  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" [rent]"
                   ]

    it "does not name the contact a payment received was paid from when the listing keeps another tag" $
      compactLinesBelow (keeping [coldTag] (withContact tagged received))
        `shouldBe` ["  at bip44/0/0"]

    -- A row of the table is one line. 50000 is the balance of the
    -- portfolio after the payment.
    it "keeps the contact a payment received was paid from in the last cell of the table of txs" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (txColumns UnitBitcoin 12) (Just 50000) (Just seenTimeInCet) (keeping [rentTag] (withContact tagged received)))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │              │ at bip44/0/0, 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" [rent]"]

    it "shows no tag of a contact outside such a listing" $
      compactSecondLine (withContact tagged (spendingLabelled ""))
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\""

  describe "An address of the address book, in the verbose view of a transaction" $ do
    it "has the label of its contact on the line below it, under the address" $
      take 2 (verboseFrom "  To" (withContact landlordContact (spendingLabelled "")))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "             \"Landlord\""
                   ]

    it "has the tags of its contact after the label, each between brackets" $
      take 2 (verboseFrom "  To" (withContact landlordContact {contactTags = [plainTag kycTag, plainTag rentTag]} (spendingLabelled "")))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "             \"Landlord\"  [kyc] [rent]"
                   ]

    it "has the words address book below it when its contact has no label" $
      take 2 (verboseFrom "  To" (withContact landlordContact {contactLabel = ""} (spendingLabelled "")))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "             address book"
                   ]

    -- The payment received was paid from the address of the landlord
    it "is named among the addresses a payment received was paid from" $ do
      let received = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty noTxTags Map.empty
      verboseFrom "  From" (withContact landlordContact received)
        `shouldBe` [ "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "             \"Landlord\"",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0"
                   ]

    it "leaves the address of a stranger without a line below it" $
      take 2 (verboseFrom "  To" (spendingLabelled ""))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0"
                   ]

  describe "A contact of the address book" $ do
    let alice = Contact "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4" "Alice" [plainTag kycTag] seenTime
        onCash = Contact "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a" "" [] seenTime

    it "takes two lines in the compact view: its address with its label and its tags, then its address in fours" $
      renderedLines (compactContactDoc alice)
        `shouldBe` [ "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  \"Alice\"  [kyc]",
                     "  bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4"
                   ]

    it "shows its address alone on its first line when it has neither a label nor a tag" $
      renderedLines (compactContactDoc onCash)
        `shouldBe` [ "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a",
                     "  bitcoincash: qpm2 qszn hks2 3z76 29mm s6s4 cwef 74vc wvy2 2gdx 6a"
                   ]

    it "adds when it was added in the verbose view of the listing, which names no network" $
      renderedLines (verboseContactDoc Nothing seenTimeInCet alice)
        `shouldBe` [ "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  \"Alice\"  [kyc]",
                     "  In fours: bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4",
                     "  Added   : 14 Nov 2023 23:13:20 CET"
                   ]

    it "names its network in an answer about it alone" $
      renderedLines (verboseContactDoc (Just bch) seenTimeInCet onCash)
        `shouldBe` [ "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a",
                     "  In fours: bitcoincash: qpm2 qszn hks2 3z76 29mm s6s4 cwef 74vc wvy2 2gdx 6a",
                     "  Network : bch",
                     "  Added   : 14 Nov 2023 23:13:20 CET"
                   ]

    -- The address column is as wide as the longest address, 42
    -- characters, and the column in fours as its groups, 52
    it "takes one row of the table, under its address, its address in fours, its tags and its label" $ do
      let contacts = [alice, landlordContact]
          headed = contactTableColumns 12 contacts
      concatMap renderedLines (tableHeader headed <> (contactRowDoc headed <$> contacts))
        `shouldBe` [ "Address                                    │ In fours                                             │ Tags         │ Label",
                     "───────────────────────────────────────────┼──────────────────────────────────────────────────────┼──────────────┼──────",
                     "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4 │ bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4 │ kyc          │ \"Alice\"",
                     "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU         │ 1JCq 8Aa9 d9rg 4T4X V93R V3DM xd5u 7GkS SU           │              │ \"Landlord\""
                   ]

    it "is shown as it was once it is deleted" $
      renderedLines (contactDeletedDoc alice)
        `shouldBe` [ "Deleted from the address book",
                     "  bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  \"Alice\"  [kyc]"
                   ]

  describe "The listing of the address book" $ do
    it "says how to add a contact when it has none to show" $
      renderedLines noContacts
        `shouldBe` [ "There are no contacts to display",
                     "To add an address of somebody else to the address book, run: hw newcontact ADDRESS LABEL"
                   ]

  describe "A tag that locks its coins, as tags and renametag show it" $ do
    it "says so after what has it, on its line of tags" $
      renderedLines (tagUsesDoc [TagUse (plainTag kycTag) 2 0 0 False, TagUse (plainTag rentTag) 4 0 0 True])
        `shouldBe` [ "kyc   2 addresses",
                     "rent  4 addresses, locks its coins"
                   ]

    it "says so when nothing has it yet" $
      renderedLines (tagUsesDoc [TagUse (plainTag rentTag) 0 0 0 True])
        `shouldBe` ["rent  not used, locks its coins"]

    it "says so under a merge, which may have made the tag lock its coins" $
      renderedLines (tagMergedDoc kycTag (TagUse (plainTag rentTag) 3 0 0 True))
        `shouldBe` [ "Merged the tag kyc into rent",
                     "  It is on 3 addresses, and it locks its coins"
                   ]

  -- 80000 - 50000 spendable - 20000 locked by a tag = 10000 not
  -- spendable yet
  describe "An address of a tag that locks its coins, as balances --verbose shows it" $
    it "gives the value a tag locks on a line of its own, apart from what is not spendable yet" $
      drop 3 (renderedLines (verboseBalanceDoc btc UnitBitcoin 9 paidAddressRow (CoinBalance 80000 50000 0 0 20000 0) []))
        `shouldBe` [ "  Spendable        : 0.00050000 btc",
                     "  Locked by tx     : 0.00000000 btc",
                     "  Locked by you    : 0.00000000 btc",
                     "  Locked by tag    : 0.00020000 btc",
                     "  Not spendable yet: 0.00010000 btc"
                   ]

  describe "The tags of contacts, as tags and deletetag count them" $ do
    it "are counted after the addresses and the transactions of a tag" $
      renderedLines (tagUsesDoc [TagUse (plainTag kycTag) 2 1 3 False, TagUse (plainTag rentTag) 0 0 1 False])
        `shouldBe` [ "kyc   2 addresses, 1 transaction, 3 contacts",
                     "rent  1 contact"
                   ]

    it "are counted among what a renamed tag is on" $
      renderedLines (tagRenamedDoc kycTag (TagUse (plainTag rentTag) 2 1 3 False))
        `shouldBe` [ "Renamed the tag kyc to rent",
                     "  It is on 2 addresses, 1 transaction, 3 contacts"
                   ]

    it "are counted among what a tag is on after another was merged into it" $
      renderedLines (tagMergedDoc kycTag (TagUse (plainTag rentTag) 1 0 2 False))
        `shouldBe` [ "Merged the tag kyc into rent",
                     "  It is on 1 address, 2 contacts"
                   ]

    it "are not counted for a renamed tag that nothing has" $
      renderedLines (tagRenamedDoc kycTag (TagUse (plainTag rentTag) 0 0 0 False))
        `shouldBe` [ "Renamed the tag kyc to rent",
                     "  Nothing has it"
                   ]

    it "are counted among what a deleted tag was on" $
      renderedLines (tagDeletedDoc kycTag (TagDeleted 1 0 2))
        `shouldBe` [ "Deleted the tag kyc",
                     "  It was on 1 address, 2 contacts"
                   ]

  describe "The address book in the answer of importwallet" $ do
    it "is a line of its own, with how many contacts of the file are in it" $
      last (renderedLines (importDoc (importedWith [] []) {reportContacts = 2} Nothing))
        `shouldBe` "Address book: 2 contacts"

    it "has no line when the file holds no contact" $
      renderedLines (importDoc (importedWith [] []) Nothing) `shouldBe` ["Labels set: none"]

    it "leaves aside a contact that is an address of the wallet, and says so" $
      last (renderedLines (importDoc (importedWith [] []) {reportContactsOwn = 1} Nothing))
        `shouldBe` "Left aside: 1 contact that is an address of the wallet"

  describe "The tags that nothing has in a file of a wallet, as show shows it" $ do
    -- The names are padded to "cold" and "rent", four letters, and the
    -- colours to "red". A line ends at its last word.
    it "come under a heading of their own, each with its colour and with whether it locks its coins" $
      renderedLines (walletFileDoc (fileOf [] []) {shownTagsNotUsed = [FileTag (ShownTag coldTag Nothing) True, FileTag (ShownTag kycTag (Just TagRed)) False, FileTag (ShownTag rentTag Nothing) False]})
        `shouldBe` [ "Tags not used",
                     "  cold       locks its coins",
                     "  kyc   red",
                     "  rent",
                     "",
                     "Labels set: none"
                   ]

    it "come after the address book" $ do
      let contactRecord = (record AddrRecord "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") {recordLabel = Just "Landlord", recordNetwork = Just "btc", recordContact = Just True}
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [replacingNone contactRecord], shownTagsNotUsed = [FileTag (ShownTag rentTag Nothing) False]})
        `shouldBe` [ "Address book",
                     "  btc  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  \"Landlord\"",
                     "",
                     "Tags not used",
                     "  rent",
                     "",
                     "Labels set: none"
                   ]

  describe "The address book in a file of a wallet, as show shows it" $ do
    it "names after the label of a contact the label of the address book it would replace" $ do
      let contactRecord = (record AddrRecord "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") {recordLabel = Just "Landlord", recordNetwork = Just "btc", recordTags = Just ["rent"], recordContact = Just True}
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord contactRecord ["Mr Smith"] FateSet]})
        `shouldBe` [ "Address book",
                     "  btc  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  \"Landlord\"  replaces \"Mr Smith\"  [rent]",
                     "",
                     "Labels set: none"
                   ]

    it "comes under a heading of its own, each contact with its network, its address, its label and its tags" $ do
      let contactRecord = (record AddrRecord "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") {recordLabel = Just "Landlord", recordNetwork = Just "btc", recordTags = Just ["rent"], recordContact = Just True}
      renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [replacingNone contactRecord]})
        `shouldBe` [ "Address book",
                     "  btc  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  \"Landlord\"  [rent]",
                     "",
                     "Labels set: none"
                   ]

    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is an address of the wallet,
    -- which the address book does not take
    it "says of a contact that is an address of the wallet that it is left aside, at the end of its line" $ do
      let contactRecord = (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Me", recordNetwork = Just "btc", recordContact = Just True}
      take 2 (renderedLines (walletFileDoc (fileOf [] []) {shownOthers = [FileRecord contactRecord [] FateOwnAddress]}))
        `shouldBe` [ "Address book",
                     "  btc  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Me\"  left aside: an address of the wallet"
                   ]
