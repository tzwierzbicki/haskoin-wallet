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
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Bip329 (Record (..), RecordType (..), record)
import Haskoin.Wallet.Cli.Listing (ChunkAt (..))
import Haskoin.Wallet.Cli.Parser (AddressCode (..), Command (..), NewPortfolio (..), Recover (..), TxShown (..), WordsLayout (..))
import Haskoin.Wallet.Cli.PrettyPrinter
import Haskoin.Wallet.Cli.Response (AccountCounts (..), Group (..), NewSecret (..), Response (..))
import Haskoin.Wallet.Cli.Style (renderPlain)
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Account (NetworkView (..), PortfolioView (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..), LeftCoin (..), LeftReason (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), ActiveAfter (..), Place (..), PortfolioRef (..), WalletNaming (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (AccountKind (..), ForeignKind (..), MnemonicPass (..), legacyPath)
import Haskoin.Wallet.Labels (AccountImport (..), FileShown (..), ImportReport (..), ImportScope (..), PortfolioImport (..), WalletImport (..), wholeFile)
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncRes (..))
import Haskoin.Wallet.TestUtils
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

-- | What 'mainPortfolioView' holds on btc, without the keys of its
-- accounts
btcView :: NetworkView
btcView = NetworkView btc 5000000 100000 4900000 3 17 []

-- | A network of a portfolio where it holds nothing and has no address
emptyOn :: Network -> NetworkView
emptyOn net = NetworkView net 0 0 0 0 0 []

-- | A whole portfolio 0 of wallet 0, as newportfolio creates it: a bip44
-- and a bip84 account on btc and on each test network of Bitcoin, a bip44
-- one on the others, 2 + 1 + 2 + 2 + 2 + 1 + 1 + 1 = 12 accounts. The
-- keys are those of btc on every network.
wholePortfolioView :: PortfolioView
wholePortfolioView =
  mainPortfolioView
    { viewNetworks =
        [ (emptyOn net) {networkViewAccounts = keys}
          | (net, keys) <- [(btc, both), (bch, [legacyKeys]), (btcTest, both), (btcTest4, both), (btcRegTest, both), (bchTest, [legacyKeys]), (bchTest4, [legacyKeys]), (bchRegTest, [legacyKeys])]
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
              wholePortfolioView
              (ActiveAfter (PortfolioRef 0 0) True)
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
    -- 4900000 satoshis spendable on btc, 100000 unconfirmed. The view is
    -- of the active portfolio.
    it "marks the active portfolio with a *, gives its name, its wallet and its description, then under them the day it was created and on a line what it holds on each network it has addresses or coins on" $
      renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet mainPortfolioView)
        `shouldBe` [ "* portfolio0  wallet0  \"savings\"",
                     "    created 14 Nov 2023",
                     "    btc  spendable 0.04900000 btc, unconfirmed 0.00100000 btc, 3 coins, 17 addresses"
                   ]

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
    it "leaves out the networks where it has neither an address nor a coin, lines up the figures of the others, and gives no unconfirmed amount when there is none" $ do
      let three = mainPortfolioView {viewNetworks = [btcView, emptyOn bch, (emptyOn btcTest4) {networkViewAddresses = 2}]}
      drop 2 (renderedLines (compactPortfolioDoc UnitBitcoin seenTimeInCet three))
        `shouldBe` [ "    btc       spendable 0.04900000 btc, unconfirmed 0.00100000 btc, 3 coins, 17 addresses",
                     "    btctest4  spendable 0.00000000 btc, no coins, 2 addresses"
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

    it "gives its fingerprint and the time it was created on the clock given, then each network with its balances, and under it each account with its path, its descriptor and its SLIP-132 key" $
      renderedLines (verbosePortfolioDoc UnitBitcoin seenTimeInCet withKeys)
        `shouldBe` [ "* portfolio0  wallet0  \"savings\"",
                     "    Fingerprint: bc560f95",
                     "    Created    : 14 Nov 2023 23:13:20 CET",
                     "    btc",
                     "      Confirmed  : 0.05000000 btc",
                     "      Unconfirmed: 0.00100000 btc",
                     "      Spendable  : 0.04900000 btc",
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
  -- network as wide as their widest cells, or as their headings, then 15
  -- for "Spendable (btc)", 17 for "Unconfirmed (btc)", 5 for the coins, 9
  -- for the addresses, then the description
  describe "The table of portfolios --tabular" $ do
    let ofMain = portfolioTableColumns UnitBitcoin [mainPortfolioView]

    -- The mark takes 1: 1 + 1 = 2 under it. "wallet0" takes 7,
    -- "portfolio0" 10, and "Network" 7: 7 + 2 = 9 under Wallet, 10 + 2 = 12
    -- under Portfolio, 7 + 2 = 9 under Network, 15 + 2 = 17 under
    -- Spendable, 17 + 2 = 19 under Unconfirmed, 5 + 2 = 7 under Coins, 9 +
    -- 2 = 11 under Addresses and 11 + 1 = 12 under Description
    it "names its columns in a header, the column of the mark without a heading and the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader ofMain)
        `shouldBe` [ "  │ Wallet  │ Portfolio  │ Network │ Spendable (btc) │ Unconfirmed (btc) │ Coins │ Addresses │ Description",
                     "──┼─────────┼────────────┼─────────┼─────────────────┼───────────────────┼───────┼───────────┼────────────"
                   ]

    it "gives a portfolio a row on a network: the mark of the active portfolio, its wallet, its name, the network, what it can spend, what is unconfirmed, its coins and addresses, and its description" $
      concatMap renderedLines (portfolioRowDocs UnitBitcoin ofMain mainPortfolioView)
        `shouldBe` ["* │ wallet0 │ portfolio0 │ btc     │      0.04900000 │        0.00100000 │     3 │        17 │ \"savings\""]

    it "leaves the column of the mark blank in the row of a portfolio that is not the active one" $
      concatMap renderedLines (portfolioRowDocs UnitBitcoin ofMain mainPortfolioView {viewActive = False})
        `shouldBe` ["  │ wallet0 │ portfolio0 │ btc     │      0.04900000 │        0.00100000 │     3 │        17 │ \"savings\""]

    -- "btctest4" takes 8, more than "Network"
    it "gives a portfolio a row for each network it has addresses or coins on, the column of the networks as wide as the widest" $ do
      let two = mainPortfolioView {viewNetworks = [btcView, emptyOn bch, (emptyOn btcTest4) {networkViewAddresses = 2}]}
          headed = portfolioTableColumns UnitBitcoin [two]
      concatMap renderedLines (portfolioRowDocs UnitBitcoin headed two)
        `shouldBe` [ "* │ wallet0 │ portfolio0 │ btc      │      0.04900000 │        0.00100000 │     3 │        17 │ \"savings\"",
                     "* │ wallet0 │ portfolio0 │ btctest4 │      0.00000000 │        0.00000000 │     0 │         2 │ \"savings\""
                   ]

    it "gives a portfolio with no address one row, with no network and no figures" $ do
      let unused = mainPortfolioView {viewNetworks = [emptyOn btc]}
      concatMap renderedLines (portfolioRowDocs UnitBitcoin (portfolioTableColumns UnitBitcoin [unused]) unused)
        `shouldBe` ["* │ wallet0 │ portfolio0 │         │                 │                   │       │           │ \"savings\""]

    -- "wallet12" takes 8 and "portfolio10" 11: the listing is read whole.
    -- 8 + 2 = 10 under Wallet, 11 + 2 = 13 under Portfolio.
    it "makes the columns of the wallets and of the portfolios of a chunk as wide as their widest names" $ do
      let later = mainPortfolioView {viewRef = PortfolioRef 12 10, viewActive = False, viewDescription = ""}
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponsePortfolios [mainPortfolioView, later])
      renderedLines doc
        `shouldBe` [ "  │ Wallet   │ Portfolio   │ Network │ Spendable (btc) │ Unconfirmed (btc) │ Coins │ Addresses │ Description",
                     "──┼──────────┼─────────────┼─────────┼─────────────────┼───────────────────┼───────┼───────────┼────────────",
                     "* │ wallet0  │ portfolio0  │ btc     │      0.04900000 │        0.00100000 │     3 │        17 │ \"savings\"",
                     "  │ wallet12 │ portfolio10 │ btc     │      0.04900000 │        0.00100000 │     3 │        17 │"
                   ]

newPortfolioSpec :: Spec
newPortfolioSpec = do
  describe "The portfolio newportfolio created, as its answer shows it" $ do
    -- The first portfolio of the database, which became active
    it "says it was created, naming the portfolio and its wallet, then gives the fingerprint, counts the accounts and names each network with the types of its accounts, and says it is the active portfolio" $
      renderedLines (newPortfolioDoc wholePortfolioView (ActiveAfter (PortfolioRef 0 0) True))
        `shouldBe` [ "Created portfolio0 of wallet0",
                     "  Fingerprint: bc560f95",
                     "  Accounts   : 12, on btc (bip44 and bip84), bch (bip44), btctest (bip44 and bip84), btctest4 (bip44 and bip84), btcreg (bip44 and bip84), bchtest (bip44), bchtest4 (bip44) and bchreg (bip44)",
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
      shown <- printedLines (ResponseNewPortfolio Nothing wholePortfolioView (ActiveAfter (PortfolioRef 0 0) True))
      take 4 (drop 2 shown)
        `shouldBe` [ "  Accounts   : 12, on btc (bip44 and bip84), bch (bip44),",
                     "               btctest (bip44 and bip84), btctest4 (bip44 and bip84),",
                     "               btcreg (bip44 and bip84), bchtest (bip44), bchtest4 (bip44)",
                     "               and bchreg (bip44)"
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
      renderedLines (importDoc (importedWith [WalletImport 0 testFingerprint True [PortfolioImport 0 True, PortfolioImport 1 True]] []) (Just (ActiveAfter (PortfolioRef 0 0) True)))
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
      renderedLines (importDoc (importedWith [WalletImport 1 testFingerprint2 True [PortfolioImport 0 True]] []) (Just (ActiveAfter (PortfolioRef 0 0) False)))
        `shouldBe` [ "wallet1  4435aa1c  created",
                     "  portfolio0  created, 12 accounts added",
                     "",
                     "The active portfolio is still portfolio0 of wallet0.",
                     "",
                     "Labels set: none"
                   ]

    -- A portfolio the database has is whole already
    it "says a wallet the database had is already in it, and that its portfolio gets nothing" $
      take 2 (renderedLines (importDoc (importedWith [WalletImport 1 testFingerprint False [PortfolioImport 0 False]] []) stillActive))
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
-- given, with what importwallet would do with them, and nothing else
fileOf :: [WalletImport] -> [(AccountImport, [Record])] -> FileShown
fileOf wallets accounts = FileShown wallets accounts [] 0 0 0 []

-- | Wallet 0 with the fingerprint of the test wallet, which the database
-- has, and its portfolio 0, which lacks nothing the file holds
heldWallet :: WalletImport
heldWallet = WalletImport 0 testFingerprint False [PortfolioImport 0 False]

walletFileSpec :: Spec
walletFileSpec =
  describe "A file of a wallet as show shows it" $ do
    it "gives each wallet of the file, with what importwallet would do with it and with each of its portfolios" $
      renderedLines (walletFileDoc (fileOf [WalletImport 0 testFingerprint True [PortfolioImport 0 True]] []))
        `shouldBe` ["wallet0  bc560f95  not in the database, and importwallet creates it", "  portfolio0  importwallet creates it, 12 accounts to add"]

    it "says of a wallet the database has that it is in it" $
      renderedLines (walletFileDoc (fileOf [heldWallet] []))
        `shouldBe` ["wallet0  bc560f95  in the database", "  portfolio0  nothing to add"]

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
                     "    0000000000000000000000000000000000000000000000000000000000000001:0  locked"
                   ]

    -- A whole portfolio has twelve accounts, most without a label
    it "leaves out an account of the file without labels or locks" $
      renderedLines (walletFileDoc (fileOf [heldWallet] [(AccountAdded segwitAccountRef, [])]))
        `shouldBe` ["wallet0  bc560f95  in the database", "  portfolio0  nothing to add"]

    it "gives why importwallet would leave a key aside, under the start of the key" $
      renderedLines (walletFileDoc (fileOf [] [(AccountLeftAside "xpub6CHK45s4QJWp" "The record names no network", [])]))
        `shouldBe` ["xpub6CHK45s4QJWp", "  importwallet leaves it aside: The record names no network"]

    it "says a coin the file unlocks is unlocked" $
      renderedLines (walletFileDoc (fileOf [] [(AccountHeld mainAccountRef, [(record OutputRecord (fileTxid <> ":1")) {recordSpendable = Just True}])]))
        `shouldBe` [ "portfolio0  btc  bip44",
                     "  Coins",
                     "    0000000000000000000000000000000000000000000000000000000000000001:1  unlocked"
                   ]

    -- A file of wallet0 and wallet1, each with a coin to unlock in the
    -- bip44 account of its portfolio0 on btc. portfolio0 alone could be
    -- of either wallet.
    it "names the wallet of each account when the file has several wallets" $ do
      let unlocked = [(record OutputRecord (fileTxid <> ":1")) {recordSpendable = Just True}]
          newWallet = WalletImport 1 testFingerprint2 True [PortfolioImport 0 True]
          shown = walletFileDoc (fileOf [heldWallet, newWallet] [(AccountHeld mainAccountRef, unlocked), (AccountAdded (AccountRef (Place (PortfolioRef 1 0) btc) Bip44), unlocked)])
      filter (Text.isPrefixOf "portfolio" . Text.pack) (renderedLines shown)
        `shouldBe` ["portfolio0 of wallet0  btc  bip44", "portfolio0 of wallet1  btc  bip44"]

    -- The first line of the view names the one wallet of the file
    it "leaves the wallet out of the line of an account when the file has one wallet, even when it is not wallet0" $ do
      let unlocked = [(record OutputRecord (fileTxid <> ":1")) {recordSpendable = Just True}]
          onlyWallet = WalletImport 1 testFingerprint2 True [PortfolioImport 0 True]
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
                     "                1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1  \"Rent\""
                   ]

    -- The record names no account of the file, so nothing says whether
    -- /0/0 is that of a bip44 or of a bip84 account
    it "gives the key path of a label of no account alone, without a type" $
      renderedLines (walletFileDoc (FileShown [] [] [(record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordLabel = Just "Wages", recordKeypath = Just "/0/0"}] 0 0 0 []))
        `shouldBe` [ "Labels without an account in the file",
                     "  Address labels",
                     "    /0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages\""
                   ]

    it "gives the labels of no account of the file last, then what hw keeps no label for and the lines that are not records" $
      renderedLines (walletFileDoc (FileShown [] [] [(record TxRecord fileTxid) {recordLabel = Just "Salary"}] 0 1 2 [4, 7]))
        `shouldBe` [ "Labels without an account in the file",
                     "  Transaction labels",
                     "    0000000000000000000000000000000000000000000000000000000000000001  \"Salary\"",
                     "",
                     "Left aside: 1 label of change addresses, 2 records hw keeps no label for",
                     "Lines that are not records: 4, 7"
                   ]

    it "counts the records of the portfolios --wallet or --portfolio leaves out, which importwallet would not read" $
      renderedLines (walletFileDoc (FileShown [] [] [] 3 0 0 []))
        `shouldBe` ["Left aside: 3 records of the portfolios --wallet or --portfolio leaves out"]

    -- What a file holds may be anything, an escape to clear the screen too
    it "writes out an escape in a reference of the file, never as an order to the terminal" $
      renderedLines (walletFileDoc (fileOf [] [(AccountHeld mainAccountRef, [(record AddrRecord "1KEn\ESC[2J") {recordLabel = Just "Wages"}])]))
        `shouldBe` [ "portfolio0  btc  bip44",
                     "  Address labels",
                     "    1KEn\\x1b[2J  \"Wages\""
                   ]

{- Addresses -}

addressSpec :: Spec
addressSpec =
  describe "An address as hw shows it" $ do
    it "gives its path, the address and its label, then what it can spend and what it received" $
      renderedLines (compactAddressDoc btc UnitBitcoin 9 paidAddressRow 50000)
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  spendable 0.00050000 btc, received 0.00050000 btc in 1 transaction"
                   ]

    it "says an address no transaction has paid is unused" $
      renderedLines (compactAddressDoc btc UnitBitcoin 9 unusedAddressRow 0)
        `shouldBe` [ "bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
                     "  unused"
                   ]

    -- A path of 5 characters, such as /0/10, is listed too
    it "pads its path to the width given, so that the addresses line up" $
      (renderedLines (compactAddressDoc btc UnitBitcoin 10 unusedAddressRow 0) !! 0)
        `shouldBe` "bip44/0/1   1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"

    it "gives every detail with --verbose, the time it was created on the clock given" $
      renderedLines (verboseAddressDoc ownPathsAlone btc UnitBitcoin 9 seenTimeInCet paidAddressRow 50000)
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  Transactions: 1",
                     "  Received    : 0.00050000 btc",
                     "  Confirmed   : 0.00050000 btc",
                     "  Unconfirmed : 0.00000000 btc",
                     "  Spendable   : 0.00050000 btc",
                     "  Coins       : 1",
                     "  Created     : 14 Nov 2023 23:13:20 CET"
                   ]

    -- receive answers with the address of an account of portfolio1 of
    -- wallet2, in a database of several wallets. The fields above the
    -- last are those of --verbose, whose time is on the clock of the
    -- computer.
    it "ends the address an answer gives with its account: its portfolio, its network and its type" $ do
      shown <- addressAnswerDoc WithWallets WithoutCode UnitBitcoin (Just otherRef) (AccountRef (Place otherRef btc) Bip84) segwitAddressRow 0
      last (renderedLines shown) `shouldBe` "  Account     : portfolio1 of wallet2  btc  bip84"

    -- show finds an address of portfolio0 while portfolio1 is active
    it "says the account of the address is not of the active portfolio, and names the active one, when another one is active" $ do
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just (PortfolioRef 0 1)) segwitAccountRef segwitAddressRow 0
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
      renderedLines (withCodeDoc (verboseAddressDoc ownPathsAlone btc UnitBitcoin 9 seenTimeInCet paidAddressRow 50000) ["####", "####"])
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
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

balanceSpec :: Spec
balanceSpec = do
  describe "An address as balances shows it" $ do
    -- 80000 in coins, of which 50000 can be spent now: 30000 cannot, which
    -- a pending transaction spends
    it "gives its path, the address and its label, then its total, spendable and unspendable balances" $
      renderedLines (coinBalanceDoc btc UnitBitcoin 9 paidAddressRow (CoinBalance 80000 50000 30000 0))
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc"
                   ]

    -- balances lists both kinds of addresses, where a change address takes
    -- no label. addresses lists one kind at a time, and says nothing.
    it "says a change address is one, where its label would be, which addresses does not" $ do
      let changeRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = internalAddrsText !! 0, dBAddressLabel = ""}
      (renderedLines (coinBalanceDoc btc UnitBitcoin 9 changeRow (CoinBalance 80000 50000 30000 0)) !! 0)
        `shouldBe` "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  change address"
      (renderedLines (compactAddressDoc btc UnitBitcoin 9 changeRow 50000) !! 0)
        `shouldBe` "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"

  -- Four coins hold 80000: 50000 can be spent, 20000 a pending
  -- transaction spends, 6000 the user locked, and the rest is unconfirmed:
  -- 80000 - 50000 - 20000 - 6000 = 4000
  describe "An address as balances --verbose shows it" $ do
    let fourCoins = paidAddressRow {dBAddressBalanceCoins = 4}

    it "gives its first line, then its total, its coins, its spendable amount, and its unspendable amount in three parts" $
      renderedLines (verboseBalanceDoc btc UnitBitcoin 9 fourCoins (CoinBalance 80000 50000 20000 6000))
        `shouldBe` [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Invoice 42\"",
                     "  Total            : 0.00080000 btc",
                     "  Coins            : 4",
                     "  Spendable        : 0.00050000 btc",
                     "  Locked by tx     : 0.00020000 btc",
                     "  Locked by you    : 0.00006000 btc",
                     "  Not spendable yet: 0.00004000 btc"
                   ]

    it "gives the parts of the unspendable amount even when they hold nothing" $
      drop 4 (renderedLines (verboseBalanceDoc btc UnitBitcoin 9 paidAddressRow (CoinBalance 50000 50000 0 0)))
        `shouldBe` [ "  Locked by tx     : 0.00000000 btc",
                     "  Locked by you    : 0.00000000 btc",
                     "  Not spendable yet: 0.00000000 btc"
                   ]

  -- The specification: balances gives a total for each network
  describe "The line above the addresses balances lists on a network" $
    it "says where they are, then the total, spendable and unspendable amounts of the network" $
      renderedLines (balanceTitle WithoutWallets UnitBitcoin mainPlace (CoinBalance 130000 100000 30000 0))
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

    it "says a coin locked with lockcoin is locked by you" $
      (renderedLines (compactCoinDoc ownPathsAlone btc UnitBitcoin (Just seenTimeInCet) spendableCoinRow {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False}) !! 0)
        `shouldBe` "00000000:0  14 Nov 2023 23:13      0.00050000 btc  locked by you, 3 confirmations"

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
quietOn ref net = quietSync {syncResPlace = Place ref net, syncResConfirmed = 0, syncResUnconfirmed = 0, syncResCoins = 0, syncResSpendable = 0}

-- | What a store at an address that refuses every connection says
downStore :: Text
downStore = "The store at http://127.0.0.1:1/ did not answer"

syncSpec :: Spec
syncSpec =
  describe "The sync of a portfolio as hw shows it" $ do
    -- quietSync: portfolio0 on btc can spend 4900000 of its 3 coins, and
    -- 100000 is unconfirmed
    it "gives where it is, what the portfolio holds there, the block it is synced to, and each kind of change on a line" $
      renderedLines (syncResultDoc WithoutWallets UnitBitcoin quietSync {syncResTxUpdates = 2, syncResCoinUpdates = 3, syncResRebroadcast = 1, syncResConflicted = 1, syncResNewAddrs = 4})
        `shouldBe` [ "portfolio0  btc",
                     "  spendable 0.04900000 btc, unconfirmed 0.00100000 btc, 3 coins",
                     "  synced at block 800002",
                     "  2 transactions and 3 coins updated",
                     "  1 transaction sent again",
                     "  1 pending transaction conflicted",
                     "  4 new addresses found"
                   ]

    -- The transaction stays pending. Its short id names it for deletetx,
    -- and what the store said follows, whole.
    it "names a transaction the store did not publish when it was sent again, with what the store said" $
      renderedLines (syncResultDoc WithoutWallets UnitBitcoin quietSync {syncResNotPublished = [(fakeTxHash 9, "The store at https://api.haskoin.com/ refused the transaction: The fee is too low")]})
        `shouldBe` [ "portfolio0  btc",
                     "  spendable 0.04900000 btc, unconfirmed 0.00100000 btc, 3 coins",
                     "  synced at block 800002",
                     "  00000000 not sent again: The store at https://api.haskoin.com/ refused the transaction: The fee is too low"
                   ]

    it "says nothing new when nothing changed" $
      renderedLines (syncResultDoc WithoutWallets UnitBitcoin quietSync)
        `shouldBe` [ "portfolio0  btc",
                     "  spendable 0.04900000 btc, unconfirmed 0.00100000 btc, 3 coins",
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
      renderedLines (syncResultsPrinter WithWallets UnitBitcoin [quietSync, quietOn mainPortfolioRef bch, quietOn otherRef btc, quietOn mainPortfolioRef btcTest])
        `shouldBe` [ "portfolio0 of wallet0  btc",
                     "  spendable 0.04900000 btc, unconfirmed 0.00100000 btc, 3 coins",
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
      renderedLines (notServedDoc [btcTest, btcRegTest]) `shouldBe` ["Not served by the store: btctest, btcreg"]

    it "gives each network left out with why its store failed" $
      renderedLines (failedNetworksDoc "not synced" [(bch, downStore), (btcTest4, downStore)])
        `shouldBe` [ "bch  not synced: The store at http://127.0.0.1:1/ did not answer",
                     "btctest4  not synced: The store at http://127.0.0.1:1/ did not answer"
                   ]

    it "answers with the syncs, then the networks not served, then those down, a blank line between them" $
      printedLines (ResponseSync [quietSync, quietOn mainPortfolioRef bch] [btcTest4] [(bchTest4, downStore)] (Just mainPortfolioRef))
        `shouldReturn` [ "portfolio0  btc",
                         "  spendable 0.04900000 btc, unconfirmed 0.00100000 btc, 3 coins",
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
      take 2 <$> printedLines (ResponseRecover [btc] 10 (Just 1) [0] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) False)))
        `shouldReturn` ["Recovered portfolio0 of wallet1", "The active portfolio is still portfolio0 of wallet0."]

    it "says nothing of the active portfolio when it recovered none" $
      printedLines (ResponseRecover [btc] 10 (Just 0) [] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) False)))
        `shouldReturn` ["No new portfolio of the mnemonic was paid on btc. The search ended after 10 portfolios in a row without a payment."]

    -- The network left out may have failed during the search or during
    -- the syncs after it. The database had no portfolio, and portfolio0
    -- became active.
    it "answers with what it found and the active portfolio, then the syncs, the networks not served, and those left out with why" $
      printedLines (ResponseRecover [btc] 10 (Just 0) [0] [quietSync] [] [btcTest4] [(bch, downStore)] (Just (ActiveAfter (PortfolioRef 0 0) True)))
        `shouldReturn` [ "Recovered portfolio0 of wallet0",
                         "portfolio0 of wallet0 is the active portfolio: the commands work in it.",
                         "",
                         "portfolio0  btc",
                         "  spendable 0.04900000 btc, unconfirmed 0.00100000 btc, 3 coins",
                         "  synced at block 800002, nothing new",
                         "",
                         "Not served by the store: btctest4",
                         "",
                         "bch  left out: The store at http://127.0.0.1:1/ did not answer"
                       ]

deletedSpec :: Spec
deletedSpec =
  describe "What deleteportfolio and deletewallet deleted, as their answers show it" $
    it "says what was deleted" $
      printedLines (ResponseDeleted "portfolio2 of wallet0 has been deleted")
        `shouldReturn` ["portfolio2 of wallet0 has been deleted"]

{- Transactions -}

headlineSpec :: Spec
headlineSpec = do
  -- The first line of the verbose view starts with the time, and names
  -- the type of the transaction
  describe "The first line of the verbose view of a transaction" $ do
    -- 22:13 UTC is 23:13 CET. 50000 satoshis are 0.0005 bitcoins.
    it "shows a payment received as Incoming, with a plus sign, at the minute of the clock given" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      renderedLines (headlineDoc fullConfirmations UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Incoming     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "shows a payment sent as Outgoing, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
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
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (headlineDoc fullConfirmations UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["14 Nov 2023 23:13  Internal     -0.00001000 btc  2 confirmations"]

  -- The first line of the compact view starts with the short id, and
  -- leaves the type to the sign and the colour of the amount. The txids
  -- of fakeTxHash all start with 00000000.
  describe "The first line of the compact view of a transaction" $ do
    it "starts with the short id of the txid, then gives the time, the amount with its sign, and the confirmations" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5  14 Nov 2023 23:13     +0.00050000 btc  3 confirmations"]

    -- 40000 paid + 1000 of fee = 41000
    it "gives a payment sent a minus sign, its amount including the fee" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations"]

    -- 1250000000 satoshis are 12.5 bitcoins
    it "lines up amounts of different widths" $ do
      let small = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
          large = small {txInfoSummary = paymentSummary {summaryAmount = 1250000000}}
      concatMap (renderedLines . compactHeadline UnitBitcoin (Just seenTimeInCet)) [small, large]
        `shouldBe` [ "00000000  14 Nov 2023 23:13     +0.00050000 btc  3 confirmations",
                     "00000000  14 Nov 2023 23:13    +12.50000000 btc  3 confirmations"
                   ]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 1 False [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     +0.00050000 btc  1 confirmation"]

    it "counts the confirmations up to 999" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 999 False [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  999 confirmations"]

    it "says 999+ confirmations for more than 999" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1000 False [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  999+ confirmations"]

    it "gives the fee rate of a transaction not in a block yet" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  unconfirmed, 4 sat/vB"]

    it "says dead for a transaction replaced or dropped from the network, without its fee rate" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  dead"]

    it "says the time is unknown when hw does not know it, in a column of the same width" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) Nothing 3 False [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin Nothing info)
        `shouldBe` ["00000000  time unknown          +0.00050000 btc  3 confirmations"]

    it "starts with the short id of the nosigHash of a transaction not signed, and says it is not signed, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5  14 Nov 2023 23:13     -0.00041000 btc  not signed, 4 sat/vB"]

    it "starts with the short id of the txid of a signed transaction, and says it is not sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["b2d4f6a8  14 Nov 2023 23:13     -0.00041000 btc  signed, not sent, 4 sat/vB"]

    it "shows a transaction sent that the sync has not seen yet as sent, with its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) True False (Just seenTime) [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  sent, 4 sat/vB"]

    it "shows a transaction in conflict as conflicted, without its fee rate" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) "" Map.empty Map.empty
      renderedLines (compactHeadline UnitBitcoin (Just seenTimeInCet) info)
        `shouldBe` ["00000000  14 Nov 2023 23:13     -0.00041000 btc  conflicted"]

compactSpec :: Spec
compactSpec = do
  describe "The balance after a transaction" $ do
    let spent = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False []))

    it "starts the second line of the compact view, before what the transaction is for" $
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty Map.empty))
        `shouldBe` [ "00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  balance 0.01208000 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "comes before the label of the transaction" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) (spent "Rent of October" Map.empty Map.empty)) !! 1)
        `shouldBe` "  balance 0.01208000 btc, \"Rent of October\""

    it "shows a portfolio left with nothing on the network as zero" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just 0) (Just seenTimeInCet) (spent "" Map.empty Map.empty)) !! 1)
        `shouldBe` "  balance 0.00000000 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "is in satoshis with the amount, each with its unit" $
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitSatoshi Nothing (Just 1) (Just seenTimeInCet) (spent "" Map.empty Map.empty))
        `shouldBe` [ "00000000  14 Nov 2023 23:13          -41000 sats  2 confirmations",
                     "  balance 1 sat, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    -- txs gives no balance to a dead transaction, which does not count
    it "is left out when the listing gives none" $
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) (spent "" Map.empty Map.empty)) !! 1)
        `shouldBe` "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "follows the fee in the verbose view" $
      take 2 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty Map.empty))))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  Balance  : 0.01208000 btc"
                   ]

  describe "The compact view of a transaction" $ do
    it "gives the address a payment pays, without its change, on the line below its first line" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "a1075db5  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "gives the label of a payment, quoted, in place of the address it pays" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "Rent of October" Map.empty Map.empty
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "00000000  14 Nov 2023 23:13     -0.00041000 btc  2 confirmations",
                     "  \"Rent of October\""
                   ]

    -- Each account of a portfolio has an address at /0/0: the type tells
    -- them apart
    it "names the address of the portfolio a payment received is paid at by the type of its account and its path, with its label" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at bip44/0/0 \"Rent\""

    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc is /0/0 of m/84'/0'/0'
    it "names an address of the bip84 account of the portfolio by bip84 and its path" $ do
      let toSegwit = paymentSummary {summaryMyOutputs = Map.fromList [(segwitExternalAddr 0, MyOutputs 50000 (segwitPath (Deriv :/ 0 :/ 0)) "")]}
          info = TxInfo toSegwit (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at bip84/0/0"

    it "gives the label of a payment received after the path of its address, in place of the label of the address" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "Salary" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  at bip44/0/0 \"Salary\""

    -- otherAddr 1 is 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "gives the largest output of several, and counts the others" $ do
      let several = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 10000), (otherAddr 1, 29000), (otherAddr 2, 1000)]}
          info = TxInfo several (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ and 2 more"

    -- A Taproot address has 62 characters
    it "gives the address of another wallet whole, however long, at the end of its line" $ do
      let toTaproot = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(taprootExternalAddr 0, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just 1208000) (Just seenTimeInCet) toTaproot) !! 1)
        `shouldBe` "  balance 0.01208000 btc, to bc1p8zl0j9e73592k9n6ftz4keyq24dq8djqd3rkcrepu2fw6zeryyjs8zz3ev"

    -- pendingtxs names where each transaction is
    it "names the portfolio and the network first when the listing names them" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin (Just mainPortfolioRef) Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  portfolio0 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the wallet of the portfolio there, wallet0 too, when the database has several wallets" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithWallets) btc UnitBitcoin (Just mainPortfolioRef) Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  portfolio0 of wallet0 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    -- The one wallet may be wallet2, once the others were deleted
    it "leaves the wallet out when the database has one wallet, even when it is not wallet0" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin (Just otherRef) Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  portfolio1 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the transaction a replacement replaces by its short id, before what it is for" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [b2d4f6a8Txid])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  replaces b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the two transactions a replacement replaces, the second after and" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [b2d4f6a8Txid, a1075db5Second])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  replaces b2d4f6a8 and a1075db5, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    it "names the transaction that replaced a dead one by its short id, before what it is for" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid])) "" Map.empty Map.empty
      renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "a1075db5  14 Nov 2023 23:13     -0.00041000 btc  dead",
                     "  replaced by b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "names the two transactions that replaced a dead one, the second after and" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid, a1075db5Second])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  replaced by b2d4f6a8 and a1075db5, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    -- txs gives the balance first, and a dead transaction has none
    it "says nothing of a replacement for a dead transaction that names none" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty
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
        spentWith confirmations = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) confirmations False [])) "" Map.empty Map.empty

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
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the txid, its first 8 digits, in the Txid column" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["a1075db5 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "counts the confirmations up to 999" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spentWith 999))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │   999 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says 999+ for more than 999 confirmations" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spentWith 1000))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │  999+ │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives 0 confirmations for a transaction not in a block yet, without its fee rate" $ do
      let unconfirmed = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) unconfirmed)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     0 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "shows a payment received with a plus sign, and the type and the path of the address of the portfolio it was paid at" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at bip44/0/0"]

    it "gives the label of a transaction, quoted, in place of the address paid" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spent "Rent of October" Map.empty Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ \"Rent of October\""]

    it "gives a long label whole, in the last column" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) (spent "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month" Map.empty Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    -- A Taproot address has 62 characters
    it "gives the address of another wallet whole, however long, never cut" $ do
      let toTaproot = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(taprootExternalAddr 0, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) toTaproot)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to bc1p8zl0j9e73592k9n6ftz4keyq24dq8djqd3rkcrepu2fw6zeryyjs8zz3ev"]

    it "gives a Bitcoin Cash address whole, with its prefix" $ do
      let toCash = TxInfo spendSummary {summaryOtherOutputs = Map.fromList [(bchExternalAddr, 40000)]} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) bch UnitBitcoin (Just 1208000) (Just seenTimeInCet) toCash)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"]

    it "gives the label of a payment received after the path of the address it was paid at" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "Salary" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at bip44/0/0 \"Salary\""]

    it "gives the label of the address a payment was received at when the payment has none" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at bip44/0/0 \"Rent\""]

    it "gives the label of a payment received rather than the label of its address" $ do
      let info = TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "Salary" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at bip44/0/0 \"Salary\""]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address
    it "says a payment received at a change address is at one" $ do
      let atChangeAddress = paymentSummary {summaryMyOutputs = Map.fromList [(internalAddr 0, MyOutputs 50000 (legacyPath (Deriv :/ 1 :/ 0)) "")]}
          info = TxInfo atChangeAddress (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at bip44/1/0 change address"]

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
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Internal │    -0.00001000 │     0.01208000 │     2 │ to bip44/0/1 and 1 more"]

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
          info = TxInfo internal (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Internal │    -0.00001000 │     0.01208000 │     2 │ to bip44/0/1 and 1 more"]

    it "gives a long label of an address whole, after its path" $ do
      let longLabel = paymentSummary {summaryMyOutputs = Map.fromList [(externalAddr 0, MyOutputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month")]}
          info = TxInfo longLabel (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 50000) (Just seenTimeInCet) info)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │    +0.00050000 │     0.00050000 │     3 │ at bip44/0/0 \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    -- A dead transaction has 0 confirmations, as an unconfirmed one does,
    -- but it will never confirm, and the balance does not count it
    it "leaves the balance of a dead transaction empty, and says dead in place of its confirmations" $ do
      let dead = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just seenTimeInCet) dead)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │                │  dead │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "names the transaction that replaced a dead one first in its description, as the compact view does" $ do
      let dead = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing (Just seenTimeInCet) dead)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │                │  dead │ replaced by b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the amounts as numbers alone, since the header names their unit" $
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitSatoshi (Just 1208000) (Just seenTimeInCet) (spent "" Map.empty Map.empty))
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │         -41000 │        1208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- 1250000000000 satoshis are 12500 bitcoins: "+12500.00000000" takes
    -- 15 characters, one more than the column
    it "lets an amount wider than its column push the rest of its row" $ do
      let large = TxInfo paymentSummary {summaryAmount = 1250000000000} (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 50000) (Just seenTimeInCet) large)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Incoming │ +12500.00000000 │     0.00050000 │     3 │ at bip44/0/0"]

  -- The screen reads a listing in chunks. A transaction without a time
  -- shows "time unknown", whatever the time zone of the computer.
  describe "A chunk of the table of txs" $ do
    let listed = ListedTx (TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) Nothing 2 False [])) "" Map.empty Map.empty) (Just 1208000)

    it "starts with the header when it is the first one" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponseTxs [mainGroup [listed]])
      renderedLines doc
        `shouldBe` [ "Txid     │ Time              │ Type     │   Amount (btc) │  Balance (btc) │ Confs │ Description",
                     "─────────┼───────────────────┼──────────┼────────────────┼────────────────┼───────┼────────────",
                     "00000000 │ time unknown      │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    it "holds its rows alone when it is a later one, with no header and no blank line" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt False False False) (ResponseTxs [mainGroup [listed]])
      renderedLines doc
        `shouldBe` ["00000000 │ time unknown      │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says there are no transactions, without a header, when there are none" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponseTxs [])
      renderedLines doc `shouldBe` ["There are no transactions to display"]

    it "puts the line of its place above the header when the listing may cover several places" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True True) (ResponseTxs [mainGroup [listed]])
      renderedLines doc
        `shouldBe` [ "portfolio0  btc",
                     "Txid     │ Time              │ Type     │   Amount (btc) │  Balance (btc) │ Confs │ Description",
                     "─────────┼───────────────────┼──────────┼────────────────┼────────────────┼───────┼────────────",
                     "00000000 │ time unknown      │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

  addressTableSpec
  coinTableSpec
  balanceTableSpec
  pendingTableSpec

-- The columns of the receiving addresses of the bip44 account, with paths
-- of 9 characters, as bip44/0/0: 9 for the path, 34 for a base58 address,
-- 15 for "Spendable (btc)", 14 for "Received (btc)", 5 for the
-- transactions, then the label
addressTableSpec :: Spec
addressTableSpec = do
  describe "The table of addresses --tabular" $ do
    let receiving = addressTableColumns UnitBitcoin btc [Bip44] 9 False
        changeAddressRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", dBAddressLabel = ""}

    -- Under a heading, the line has a dash for each character and the
    -- space on each side: 9 + 1 = 10 under Path, 34 + 2 = 36 under
    -- Address, 15 + 2 = 17 under Spendable, 14 + 2 = 16 under Received,
    -- 5 + 2 = 7 under Txs, and 5 + 1 = 6 under Label
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader receiving)
        `shouldBe` [ "Path      │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Label",
                     "──────────┼────────────────────────────────────┼─────────────────┼────────────────┼───────┼──────"
                   ]

    it "gives an address one row: its path, the address, what it can spend and received, its transactions and its label" $
      renderedLines (addressRowDoc UnitBitcoin btc receiving paidAddressRow 50000)
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │ \"Invoice 42\""]

    it "gives a long label whole, in the last column" $
      renderedLines (addressRowDoc UnitBitcoin btc receiving paidAddressRow {dBAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"} 50000)
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "leaves the amounts of an address no transaction paid empty, and ends its row without a label" $
      renderedLines (addressRowDoc UnitBitcoin btc receiving unusedAddressRow 0)
        `shouldBe` ["bip44/0/1 │ 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg │                 │                │     0 │"]

    -- 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1 has 33 characters
    it "pads a base58 address shorter than the longest, so that the columns after it line up" $ do
      let shorter = unusedAddressRow {dBAddressIndex = 3, dBAddressAddress = "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"}
      renderedLines (addressRowDoc UnitBitcoin btc receiving shorter 0)
        `shouldBe` ["bip44/0/3 │ 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1  │                 │                │     0 │"]

    it "gives the amounts as numbers alone, in satoshis when they are, with wider headings" $ do
      let inSatoshis = addressTableColumns UnitSatoshi btc [Bip44] 9 False
      concatMap renderedLines (take 1 (tableHeader inSatoshis) <> [addressRowDoc UnitSatoshi btc inSatoshis paidAddressRow 50000])
        `shouldBe` [ "Path      │ Address                            │ Spendable (sats) │ Received (sats) │   Txs │ Label",
                     "bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │            50000 │           50000 │     1 │ \"Invoice 42\""
                   ]

    -- Under Txs, the last column, the line has 5 + 1 = 6 dashes
    it "has no column for a label in the table of change addresses, which never have one" $ do
      let change = addressTableColumns UnitBitcoin btc [Bip44] 9 True
      concatMap renderedLines (tableHeader change <> [addressRowDoc UnitBitcoin btc change changeAddressRow 50000])
        `shouldBe` [ "Path      │ Address                            │ Spendable (btc) │ Received (btc) │   Txs",
                     "──────────┼────────────────────────────────────┼─────────────────┼────────────────┼──────",
                     "bip44/1/0 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │      0.00050000 │     0.00050000 │     1"
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
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponseAddresses [mainGroup [(paidAddressRow, 50000)]])
      renderedLines doc
        `shouldBe` [ "Path       │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Label",
                     "───────────┼────────────────────────────────────┼─────────────────┼────────────────┼───────┼──────",
                     "bip44/0/0  │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │ \"Invoice 42\""
                   ]

    it "puts the line of its account above the header when the listing may cover several accounts" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True True) (ResponseAddresses [mainGroup [(paidAddressRow, 50000)]])
      take 2 (renderedLines doc)
        `shouldBe` ["portfolio0  btc  bip44", "Path       │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Label"]

-- The columns of the coins of the bip44 account, with paths of 9
-- characters, as bip44/0/0: 17 for the time, 14 for the value, 17 for the
-- state, 5 for the confirmations, 34 for a base58 address, 9 for the
-- path, then the
-- label. The coins are shown at 'seenTimeInCet', 23:13 in Central Europe.
coinTableSpec :: Spec
coinTableSpec =
  describe "The table of coins --tabular" $ do
    let headed = coinTableColumns UnitBitcoin btc [Bip44] 9
        row = coinRowDoc btc UnitBitcoin headed (Just seenTimeInCet)

    -- Under a heading, the line has a dash for each character and the
    -- space on each side: 12 + 1 = 13 under Coin, 17 + 2 = 19 under Time,
    -- 14 + 2 = 16 under Value, 17 + 2 = 19 under State, 5 + 2 = 7 under
    -- Confs, 34 + 2 = 36 under Address, 9 + 2 = 11 under Path, and 5 + 1 = 6
    -- under Label
    it "names its columns in a header, the unit in the heading of the value, with a line under it" $
      concatMap renderedLines (tableHeader headed)
        `shouldBe` [ "Coin         │ Time              │    Value (btc) │ State             │ Confs │ Address                            │ Path      │ Label",
                     "─────────────┼───────────────────┼────────────────┼───────────────────┼───────┼────────────────────────────────────┼───────────┼──────"
                   ]

    -- spendableCoinRow is output 0 of transaction 1, whose txid starts
    -- with 00000000
    it "gives a coin one row: the time of its transaction, its value, state, confirmations, its address in full with its path, and its short id, then nothing without a label" $
      renderedLines (row spendableCoinRow)
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │"]

    -- "a1075db5:1" takes 10 of the 12 characters of the column
    it "gives the short id of the coin, the short id of its txid and its index, in the Coin column" $
      renderedLines (row spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1})
        `shouldBe` ["a1075db5:1   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │"]

    it "gives the label of the address of the coin" $
      renderedLines (row spendableCoinRow {jsonCoinAddressLabel = "Wages"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │ \"Wages\""]

    it "leaves out the label of the transaction that paid the coin, which the verbose view gives" $
      renderedLines (row spendableCoinRow {jsonCoinTxLabel = "Salary"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │"]

    it "says a coin at a change address is at one, in place of a label" $
      renderedLines (row atChange)
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │ bip44/1/0 │ change address"]

    it "says a coin a pending transaction spends is locked by tx" $
      renderedLines (row spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ locked by tx      │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │"]

    it "says a coin locked with lockcoin is locked by you" $
      renderedLines (row spendableCoinRow {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ locked by you     │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │"]

    it "says a free coin the wallet cannot spend yet is not spendable yet, and gives an unconfirmed one 0 confirmations" $
      renderedLines (row spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ not spendable yet │     0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │"]

    it "says 999+ for more than 999 confirmations" $
      renderedLines (row spendableCoinRow {jsonCoinConfirmations = 1000})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │  999+ │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │"]

    it "gives a long label whole, in the last column" $
      renderedLines (row spendableCoinRow {jsonCoinAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"})
        `shouldBe` ["00000000:0   │ 14 Nov 2023 23:13 │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0 │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    -- The account has 12 receiving addresses and 3 change addresses: its
    -- longest path is bip44/0/11, of 10 characters. A coin without a time shows
    -- "time unknown", whatever the time zone of the computer.
    it "takes the width of the paths from the longest path of the account, in a chunk of coins" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponseCoins [mainGroup [spendableCoinRow {jsonCoinTime = Nothing}]])
      renderedLines doc
        `shouldBe` [ "Coin         │ Time              │    Value (btc) │ State             │ Confs │ Address                            │ Path       │ Label",
                     "─────────────┼───────────────────┼────────────────┼───────────────────┼───────┼────────────────────────────────────┼────────────┼──────",
                     "00000000:0   │ time unknown      │     0.00050000 │ spendable         │     3 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │ bip44/0/0  │"
                   ]

    -- The coins of both accounts of btc are listed together: the column of
    -- addresses takes the 42 characters of a bip84 address
    it "makes the column of addresses of a chunk of the coins of both types as wide as a bip84 address" $ do
      let both = Group mainPlace [AccountCounts Bip44 12 3, AccountCounts Bip84 2 0] [spendableCoinRow {jsonCoinTime = Nothing}]
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponseCoins [both])
      take 1 (renderedLines doc)
        `shouldBe` ["Coin         │ Time              │    Value (btc) │ State             │ Confs │ Address                                    │ Path       │ Label"]

-- The columns of the balances of the bip44 account, with paths of 9
-- characters, as bip44/0/0: 9 for the path, 34 for a base58 address, 14 for "Total
-- (btc)", 15 for "Spendable (btc)", 17 for "Unspendable (btc)", then the
-- label
balanceTableSpec :: Spec
balanceTableSpec =
  describe "The table of balances --tabular" $ do
    let headed = balanceTableColumns UnitBitcoin btc [Bip44] 9
        changeAddressRow = paidAddressRow {dBAddressInternal = True, dBAddressAddress = "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", dBAddressLabel = ""}

    -- 9 + 1 = 10 under Path, 34 + 2 = 36 under Address, 14 + 2 = 16 under
    -- Total, 15 + 2 = 17 under Spendable, 17 + 2 = 19 under Unspendable,
    -- and 5 + 1 = 6 under Label
    it "names its columns in a header, the unit in the headings of the amounts, with a line under it" $
      concatMap renderedLines (tableHeader headed)
        `shouldBe` [ "Path      │ Address                            │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Label",
                     "──────────┼────────────────────────────────────┼────────────────┼─────────────────┼───────────────────┼──────"
                   ]

    -- 80000 in all, of which 50000 can be spent: 80000 - 50000 = 30000
    -- cannot, which a pending transaction spends
    it "gives an address one row: its path, the address, its total, spendable and unspendable amounts, then its label" $
      renderedLines (balanceRowDoc UnitBitcoin btc headed paidAddressRow (CoinBalance 80000 50000 30000 0))
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │ \"Invoice 42\""]

    it "gives a long label whole, in the last column" $
      renderedLines (balanceRowDoc UnitBitcoin btc headed paidAddressRow {dBAddressLabel = "Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month"} (CoinBalance 80000 50000 30000 0))
        `shouldBe` ["bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │ \"Deposit for the flat in Lyon, paid by bank and card, the rest in cash next month\""]

    it "says a change address is one, in place of a label" $
      renderedLines (balanceRowDoc UnitBitcoin btc headed changeAddressRow (CoinBalance 9000 9000 0 0))
        `shouldBe` ["bip44/1/0 │ 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn │     0.00009000 │      0.00009000 │        0.00000000 │ change address"]

    -- The account has 12 receiving addresses and 3 change addresses: its
    -- longest path is bip44/0/11, of 10 characters. balances gives the total of
    -- the network above its addresses, even narrowed to one network.
    it "starts a chunk with the line of the network and its total, then the header, the paths as wide as the longest of the account" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponseBalances [(mainGroup [(paidAddressRow, CoinBalance 80000 50000 30000 0)], CoinBalance 80000 50000 30000 0)])
      renderedLines doc
        `shouldBe` [ "portfolio0  btc  total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc",
                     "Path       │ Address                            │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Label",
                     "───────────┼────────────────────────────────────┼────────────────┼─────────────────┼───────────────────┼──────",
                     "bip44/0/0  │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │     0.00080000 │      0.00050000 │        0.00030000 │ \"Invoice 42\""
                   ]

-- The columns of the pending transactions: the short id, the wallet when
-- the database has several, the portfolio and the network of each, as
-- wide as the widest the listing holds or as their headings, 17 for the
-- time, 8 for the type, 14 for the amount, 10 for the status and 6 for
-- the fee rate, then what it is for
pendingTableSpec :: Spec
pendingTableSpec =
  describe "The table of pending transactions --tabular" $ do
    let ofMain = pendingTableColumns UnitBitcoin WithoutWallets [mainPlace]
        preparedWith signedM online conflicted = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) signedM online conflicted (Just seenTime) [])) "" Map.empty Map.empty
        unsigned = preparedWith Nothing False False
        timeless = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty
        rowOf = pendingRowDoc UnitBitcoin WithoutWallets (activePaths WithoutWallets) ofMain mainPlace (Just seenTimeInCet)

    -- "portfolio0" takes 10, more than "Portfolio", and "btc" less than
    -- "Network", of 7: 8 + 1 = 9 under Hash, 10 + 2 = 12 under Portfolio,
    -- 7 + 2 = 9 under Network, 17 + 2 = 19 under Time, 8 + 2 = 10 under
    -- Type, 14 + 2 = 16 under Amount, 10 + 2 = 12 under Status, 6 + 2 = 8
    -- under sat/vB, and 11 + 1 = 12 under Description
    it "names its columns in a header: the short id, then the portfolio and the network, which each transaction names" $
      concatMap renderedLines (tableHeader ofMain)
        `shouldBe` [ "Hash     │ Portfolio  │ Network │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Description",
                     "─────────┼────────────┼─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼────────────"
                   ]

    -- 40000 paid + 1000 of fee = 41000, at 4 sat/vB. The hashes of
    -- fakeTxHash all start with 00000000.
    it "gives a transaction one row: its short id, its portfolio and network, its time, type, amount, status and fee rate, then what it is for" $
      renderedLines (rowOf unsigned)
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the nosigHash of a transaction not signed" $ do
      let notSigned = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty
      renderedLines (rowOf notSigned)
        `shouldBe` ["a1075db5 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the short id of the txid of a signed transaction, which the commands take, and says it is signed" $ do
      let signed = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty Map.empty
      renderedLines (rowOf signed)
        `shouldBe` ["b2d4f6a8 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ signed     │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says a transaction sent that the sync has not seen yet is sent" $
      renderedLines (rowOf (preparedWith (Just (fakeTxHash 2)) True False))
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ sent       │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "says a transaction in conflict is conflicted, and leaves its fee rate empty" $
      renderedLines (rowOf (preparedWith (Just (fakeTxHash 2)) False True))
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ conflicted │        │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- "wallet0" takes 7, more than "Wallet": 7 + 2 = 9 under Wallet
    it "puts a column of the wallet before the portfolio when the database has several wallets" $
      concatMap renderedLines (tableHeader (pendingTableColumns UnitBitcoin WithWallets [mainPlace]))
        `shouldBe` [ "Hash     │ Wallet  │ Portfolio  │ Network │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Description",
                     "─────────┼─────────┼────────────┼─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼────────────"
                   ]

    it "names the wallet of each transaction in that column, wallet0 too, and its portfolio alone in the next" $ do
      let elsewhere = Place otherRef btc
          both = pendingTableColumns UnitBitcoin WithWallets [mainPlace, elsewhere]
      concatMap renderedLines [pendingRowDoc UnitBitcoin WithWallets (activePaths WithoutWallets) both elsewhere (Just seenTimeInCet) unsigned, pendingRowDoc UnitBitcoin WithWallets (activePaths WithoutWallets) both mainPlace (Just seenTimeInCet) unsigned]
        `shouldBe` [ "00000000 │ wallet2 │ portfolio1 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "00000000 │ wallet0 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

    -- The one wallet may be wallet2, once the others were deleted
    it "leaves the wallet out when the database has one wallet, even when it is not wallet0" $ do
      let elsewhere = Place otherRef btc
          alone = pendingTableColumns UnitBitcoin WithoutWallets [elsewhere]
      renderedLines (pendingRowDoc UnitBitcoin WithoutWallets (activePaths WithoutWallets) alone elsewhere (Just seenTimeInCet) unsigned)
        `shouldBe` ["00000000 │ portfolio1 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "names the transaction a replacement replaces by its short id, before what it is for, as the compact view does" $ do
      let replacement = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [b2d4f6a8Txid])) "" Map.empty Map.empty
      renderedLines (rowOf replacement)
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ replaces b2d4f6a8, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    it "gives the label of a payment in place of the address it pays, as the table of txs does" $
      renderedLines (rowOf unsigned {txInfoLabel = "Rent of October"})
        `shouldBe` ["00000000 │ portfolio0 │ btc     │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │ not signed │      4 │ \"Rent of October\""]

    -- A transaction without a time shows "time unknown", whatever the time
    -- zone of the computer. The listing is one list, with no title.
    it "names where each transaction is in a chunk, under the header" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponsePendingTxs Nothing [(mainPlace, timeless)])
      renderedLines doc
        `shouldBe` [ "Hash     │ Portfolio  │ Network │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Description",
                     "─────────┼────────────┼─────────┼───────────────────┼──────────┼────────────────┼────────────┼────────┼────────────",
                     "00000000 │ portfolio0 │ btc     │ time unknown      │ Outgoing │    -0.00041000 │ not signed │      4 │ to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
                   ]

verboseSpec :: Spec
verboseSpec =
  describe "The verbose view of a transaction" $ do
    -- pendingtxs --verbose names where each transaction is, first among
    -- the fields. A transaction without a time reads the same in every
    -- time zone.
    it "names the wallet of its portfolio in a listing, wallet0 too, when the database has several wallets" $ do
      let timeless = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty
      shown <- listedTxDoc (activePaths WithWallets) Verbose btc UnitBitcoin (Just mainPortfolioRef) Nothing timeless
      take 2 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Portfolio: portfolio0 of wallet0",
                     "  Network  : btc"
                   ]

    it "leaves the wallet out of a listing of one wallet, even when it is not wallet0" $ do
      let timeless = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty
      shown <- listedTxDoc (activePaths WithoutWallets) Verbose bch UnitBitcoin (Just otherRef) Nothing timeless
      take 2 (drop 1 (renderedLines shown))
        `shouldBe` [ "  Portfolio: portfolio1",
                     "  Network  : bch"
                   ]

    -- The input is the coin of 50000 at /0/0. 40000 go out and 9000 come
    -- back as change to /1/0.
    it "lists the address a payment spends from, the one it pays, and its change, in columns" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)
        `shouldBe` [ "14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002",
                     "  Short id : 00000000",
                     "  Time     : 14 Nov 2023 23:13:20 CET",
                     "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0"
                   ]

    -- In satoshis, "50000 sats" takes 10 characters and "9000 sats" 9:
    -- the amounts are right-aligned, and the paths start in one column
    it "lines up the paths of the addresses of the portfolio after amounts of different widths" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitSatoshi [] Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  50000 sats  bip44/0/0",
                     "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  40000 sats",
                     "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn   9000 sats  bip44/1/0"
                   ]

    it "gives the txid in full, then its short id, its first 8 digits" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Txid     : a1075db5c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : a1075db5"
                   ]

    it "gives the full count of the confirmations, past 999 too" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1234 False [])) "" Map.empty Map.empty
      take 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  1234 confirmations"]

    it "says 1 confirmation in the singular" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 1 False [])) "" Map.empty Map.empty
      take 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` ["14 Nov 2023 23:13  Outgoing     -0.00041000 btc  1 confirmation"]

    it "shows the label of a transaction, quoted, as its first field" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "Rent of October" Map.empty Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Label    : \"Rent of October\"",
                     "  Txid     : 0000000000000000000000000000000000000000000000000000000000000002"
                   ]

    -- "  From     : " takes 13 characters
    it "shows the label of an address of the portfolio, quoted, on the line below, under the address" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "             \"Rent\""
                   ]

    -- The coin spent is of transaction 1, the payment labelled Salary
    it "follows the label of an address spent from with the labels of its coins, after the word from" $ do
      let info = TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" (Map.fromList [(fakeTxHash 1, "Salary")]) Map.empty
      (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 6)
        `shouldBe` "             \"Rent\"  from \"Salary\""

    -- The spending from the first change address, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn
    it "marks a change address spent from as one, on the line below the address" $ do
      let fromChange = spendSummary {summaryMyInputs = Map.fromList [(internalAddr 0, MyInputs 50000 (legacyPath (Deriv :/ 1 :/ 0)) "" [OutPoint (fakeTxHash 1) 0])]}
          info = TxInfo fromChange (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00050000 btc  bip44/1/0",
                     "             change address"
                   ]

    -- A transfer of the portfolio to its first change address, which the
    -- compact view names as the address paid
    it "marks a change address an internal transaction pays as one" $ do
      let toChange = spendSummary {summaryType = TxInternal, summaryOtherOutputs = Map.empty}
          info = TxInfo toChange (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      (renderedLines (compactTxDoc (activePaths WithoutWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) info) !! 1)
        `shouldBe` "  to bip44/1/0 change address"

    it "gives the labels of the coins of an address spent from without a label of its own on the line below" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" (Map.fromList [(fakeTxHash 1, "Salary")]) Map.empty
      take 2 (drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                     "             from \"Salary\""
                   ]

    -- The payer spends 51000 from otherAddr 0, which is nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty Map.empty
      drop 5 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info))
        `shouldBe` [ "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                     "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0"
                   ]

    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU sorts before
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ: the outputs come by address
    it "puts each address of a section on a line of its own, under the first" $ do
      let twoPaid = spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 0, 30000), (otherAddr 1, 10000)]}
          info = TxInfo twoPaid (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      take 2 (drop 6 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00030000 btc",
                     "             1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  0.00010000 btc"
                   ]

    it "names the transaction that replaced a dead one in full, under the words replaced by" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid])) "" Map.empty Map.empty
      take 2 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Status   : replaced by",
                     "             b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001"
                   ]

    it "names every transaction that replaced a dead one, one under the other" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid, a1075db5Second])) "" Map.empty Map.empty
      take 3 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Status   : replaced by",
                     "             b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001",
                     "             a1075db5c0ffee00000000000000000000000000000000000000000000000002"
                   ]

    -- The portfolio made the payment, so a replacement would spend one of
    -- its coins, and the portfolio would hold it
    it "says the network dropped a dead payment of the portfolio that nothing replaced" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty
      (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  Status   : dropped from the network"

    -- Its payer can replace it with a transaction the portfolio never sees
    it "says a dead payment received was replaced or dropped, which the portfolio cannot tell apart" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.MemRef 1700000000) (Just seenTime) 0 True [])) "" Map.empty Map.empty
      (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  Status   : replaced, or dropped from the network"

    it "shows under the fee the change too small for an output that the fee took in" $ do
      let info = TxInfo spendSummary {summaryChangeInFee = Just 300} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      take 2 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "             includes 0.00000300 btc of change too small for an output"
                   ]

    it "shows the lock time under the fee, as the block the transaction waits for, and no field without one" $ do
      let locked = TxInfo spendSummary {summaryLockTime = 814037} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
          unlocked = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      take 2 (drop 4 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) locked)))
        `shouldBe` [ "  Fee      : 0.00001000 btc, 4 sat/vB",
                     "  Lock time: block 814037"
                   ]
      length (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) unlocked)) `shouldBe` 8

    -- 500,000,000 and above, a lock time is a Unix time; hw writes none
    it "shows a lock time of 500,000,000 or more as a Unix time" $ do
      let locked = TxInfo spendSummary {summaryLockTime = 1700000000} (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) locked) !! 5
        `shouldBe` "  Lock time: Unix time 1700000000"

    it "shows an unsigned transaction by its nosigHash, in full then by its short id, with the time it was created" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty
      take 3 (drop 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  NoSigHash: a1075db5c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : a1075db5",
                     "  Created  : 14 Nov 2023 23:13:20 CET"
                   ]

    it "shows a signed transaction by its txid, in full then by its short id" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus a1075db5First (Just b2d4f6a8Txid) False False (Just seenTime) [])) "" Map.empty Map.empty
      take 2 (drop 1 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Txid     : b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001",
                     "  Short id : b2d4f6a8"
                   ]

    it "explains a conflict, and how to free the coins" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False True (Just seenTime) [])) "" Map.empty Map.empty
      (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info) !! 4)
        `shouldBe` "  Conflict : A coin it spends was spent by another transaction, or is gone. It can no longer be sent. Delete it with deletetx."

    it "names every transaction a replacement replaces, one under the other" $ do
      let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False (Just seenTime) [fakeTxHash 5, fakeTxHash 6])) "" Map.empty Map.empty
      take 2 (drop 3 (renderedLines (verboseTxDoc (activePaths WithoutWallets) btc UnitBitcoin [] Nothing (Just seenTimeInCet) info)))
        `shouldBe` [ "  Replaces : 0000000000000000000000000000000000000000000000000000000000000005",
                     "             0000000000000000000000000000000000000000000000000000000000000006"
                   ]

    -- The specification: the summary of a transaction gains its network
    -- and the types of the portfolio's addresses it involves
    it "gives in a listing the network of the transaction and the types of the addresses of the portfolio it involves, first" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
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
          info = TxInfo toSegwit (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty
      concatMap renderedLines (whereFields btc Nothing info) `shouldBe` ["Network  : btc", "Types    : bip44, bip84"]

    -- A sweep between outside addresses involves no address of a portfolio
    it "gives no types for a transaction that involves no address of the portfolio" $ do
      let outside = spendSummary {summaryMyInputs = Map.empty, summaryMyOutputs = Map.empty}
          info = TxInfo outside (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty
      concatMap renderedLines (whereFields btc Nothing info) `shouldBe` ["Network  : btc"]

answerSpec :: Spec
answerSpec =
  describe "The transaction a command answers with" $ do
    -- A transaction read from a file has no time, so the clock of the
    -- computer plays no part
    let info = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty

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
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt True True False) (ResponseCoins [mainGroup [coin]])
      renderedLines doc `shouldBe` coinLines

    it "gives a section the line of its place, then a blank line, when the listing may cover several places" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt True True True) (ResponseCoins [mainGroup [coin]])
      renderedLines doc `shouldBe` ["portfolio0  btc", ""] <> coinLines

    -- The section is of portfolio1 of wallet2, in a database of several
    -- wallets, so its line names the wallet
    it "sets a section that starts below another apart from it by a blank line" $ do
      doc <- chunkDoc UnitBitcoin Compact WithWallets (ChunkAt False True True) (ResponseCoins [Group (Place otherRef btc) [AccountCounts Bip44 1 0] [coin]])
      renderedLines doc `shouldBe` ["", "portfolio1 of wallet2  btc", ""] <> coinLines

    it "gives a chunk that goes on in its section its rows alone, after the blank line that sets them apart" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt False False True) (ResponseCoins [mainGroup [coin]])
      renderedLines doc `shouldBe` "" : coinLines

    it "gives each section of the addresses the line of its account" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt True True True) (ResponseAddresses [Group mainPlace [AccountCounts Bip84 2 0] [(segwitAddressRow, 0)]])
      renderedLines doc
        `shouldBe` ["portfolio0  btc  bip84", "", "bip84/0/1  bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2", "  unused"]

    -- The account has 12 receiving and 3 change addresses: the paths are
    -- padded to bip44/0/11, of 10 characters
    it "always gives the sections of balances the line of their place with the total of the network, though the listing is narrowed to it" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt True True False) (ResponseBalances [(mainGroup [(paidAddressRow, CoinBalance 80000 50000 30000 0)], CoinBalance 130000 100000 30000 0)])
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
      let both = Group mainPlace [AccountCounts Bip44 12 3, AccountCounts Bip84 2 0] [(paidAddressRow, CoinBalance 80000 50000 30000 0), (segwitAddressRow, CoinBalance 20000 20000 0 0)]
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt True True False) (ResponseBalances [(both, CoinBalance 100000 70000 30000 0)])
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
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt True True True) (ResponseTxs [])
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
      startsWithHeader Table (ChunkAt True True False) (ResponseBalances [(mainGroup [], CoinBalance 0 0 0 0)]) `shouldBe` False

    it "is true of the table of portfolios, which is one list" $
      startsWithHeader Table (ChunkAt True True False) (ResponsePortfolios [mainPortfolioView]) `shouldBe` True

{- Next steps -}

nextStepsSpec :: Spec
nextStepsSpec = do
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
    let existing = CommandNewPortfolio (NewPortfolio True Nothing False Nothing Nothing False)
        secondView = wholePortfolioView {viewRef = PortfolioRef 0 1, viewActive = False}
    it "are the switch to the portfolio newportfolio created, when another one stays active" $
      (renderedLines <$> nextStepsPrinter existing (ResponseNewPortfolio Nothing secondView (ActiveAfter (PortfolioRef 0 0) False)))
        `shouldBe` Just ["To work in portfolio1 of wallet0, make it the active one:", "  hw switch 1 --wallet 0"]

    it "are none when newportfolio made the portfolio it created the active one" $
      (renderedLines <$> nextStepsPrinter existing (ResponseNewPortfolio Nothing wholePortfolioView (ActiveAfter (PortfolioRef 0 0) True)))
        `shouldBe` Nothing

    -- recover created portfolios 0 and 2 of wallet1, and portfolio0 of
    -- wallet0 stays active
    it "are the switch to the first portfolio recover created, when another one stays active" $
      (renderedLines <$> nextStepsPrinter (CommandRecover (Recover 10)) (ResponseRecover [btc] 10 (Just 1) [0, 2] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) False))))
        `shouldBe` Just ["To work in portfolio0 of wallet1, make it the active one:", "  hw switch 0 --wallet 1"]

    -- A new wallet always gets portfolio0, which becomes the active one
    -- in a database without a portfolio. recover found portfolio3 alone
    -- paid, and the user who lists the active portfolio would see nothing.
    it "are the switch to the portfolio recover found paid, when the empty portfolio0 of its new wallet became the active one" $
      (renderedLines <$> nextStepsPrinter (CommandRecover (Recover 10)) (ResponseRecover [btc] 10 (Just 0) [3] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) True))))
        `shouldBe` Just ["To work in portfolio3 of wallet0, make it the active one:", "  hw switch 3 --wallet 0"]

    it "are none when recover made a portfolio it created the active one" $
      (renderedLines <$> nextStepsPrinter (CommandRecover (Recover 10)) (ResponseRecover [btc] 10 (Just 0) [0] [] [] [] [] (Just (ActiveAfter (PortfolioRef 0 0) True))))
        `shouldBe` Nothing

    it "are the switch to the first portfolio importwallet created, when another one stays active" $
      (renderedLines <$> nextStepsPrinter (CommandImportWallet wholeFile "wallet.jsonl") (ResponseImport (importedWith [WalletImport 1 testFingerprint2 True [PortfolioImport 0 True]] []) (Just (ActiveAfter (PortfolioRef 0 0) False))))
        `shouldBe` Just ["To work in portfolio0 of wallet1, make it the active one:", "  hw switch 0 --wallet 1"]

    it "are none when importwallet made a portfolio it created the active one" $
      (renderedLines <$> nextStepsPrinter (CommandImportWallet wholeFile "wallet.jsonl") (ResponseImport (importedWith [WalletImport 0 testFingerprint True [PortfolioImport 0 True]] []) (Just (ActiveAfter (PortfolioRef 0 0) True))))
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
      let received = TxInfo paymentSummary {summaryOtherInputs = Map.fromList [(account1Addr, 51000)]} (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty (inPortfolio1 "")
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
          received = TxInfo paymentSummary {summaryOtherInputs = Map.fromList [(account1Addr, 51000)]} (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False [])) "" Map.empty fromWallet0
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
      renderedLines (txRowDoc (activePaths WithoutWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) transfer)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to bip44/0/0 of portfolio1"]

    it "names the wallet after that portfolio in a database of several wallets, wallet0 too" $
      (renderedLines (compactTxDoc (activePaths WithWallets) btc UnitBitcoin Nothing Nothing (Just seenTimeInCet) transfer) !! 1)
        `shouldBe` "  to bip44/0/0 of portfolio1 of wallet0"

    it "names the wallet the same way in the last column of the table of txs" $
      renderedLines (txRowDoc (activePaths WithWallets) btc UnitBitcoin (Just 1208000) (Just seenTimeInCet) transfer)
        `shouldBe` ["00000000 │ 14 Nov 2023 23:13 │ Outgoing │    -0.00041000 │     0.01208000 │     2 │ to bip44/0/0 of portfolio1 of wallet0"]

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
        moved = TxInfo movedSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty
        secondLine doc = renderedLines doc !! 1

    it "names its portfolio after the path of the address it pays" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt True True False) (ResponsePendingTxs (Just portfolio1Ref) [(mainPlace, moved)])
      secondLine doc `shouldBe` "  portfolio0 btc, to bip44/1/0 of portfolio0 change address"

    it "leaves the path alone when its portfolio is the active one" $ do
      doc <- chunkDoc UnitBitcoin Compact WithoutWallets (ChunkAt True True False) (ResponsePendingTxs (Just mainPortfolioRef) [(mainPlace, moved)])
      secondLine doc `shouldBe` "  portfolio0 btc, to bip44/1/0 change address"

    it "names the wallet after the portfolio, wallet0 too, when the database has several wallets" $ do
      doc <- chunkDoc UnitBitcoin Compact WithWallets (ChunkAt True True False) (ResponsePendingTxs (Just portfolio1Ref) [(mainPlace, moved)])
      secondLine doc `shouldBe` "  portfolio0 of wallet0 btc, to bip44/1/0 of portfolio0 of wallet0 change address"

    -- The row comes under the two lines of the header. The transaction
    -- costs its fee of 1000, at 4 sat/vB.
    it "names its portfolio after the path in the last column of the table" $ do
      doc <- chunkDoc UnitBitcoin Table WithoutWallets (ChunkAt True True False) (ResponsePendingTxs (Just portfolio1Ref) [(mainPlace, moved)])
      (renderedLines doc !! 2)
        `shouldBe` "00000000 │ portfolio0 │ btc     │ time unknown      │ Internal │    -0.00001000 │ not signed │      4 │ to bip44/1/0 of portfolio0 change address"

    it "names its portfolio after every path with --verbose" $ do
      doc <- chunkDoc UnitBitcoin Verbose WithoutWallets (ChunkAt True True False) (ResponsePendingTxs (Just portfolio1Ref) [(mainPlace, moved)])
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
      let prepared = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty
          left = LeftCoin coin {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False} (Locked LockedByUser)
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
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just portfolio1Ref) segwitAccountRef segwitAddressRow 0
      take 1 (renderedLines shown) `shouldBe` ["bip84/0/1 of portfolio0  bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2"]

    it "gives its path alone when its portfolio is the active one" $ do
      shown <- addressAnswerDoc WithoutWallets WithoutCode UnitBitcoin (Just mainPortfolioRef) segwitAccountRef segwitAddressRow 0
      take 1 (renderedLines shown) `shouldBe` ["bip84/0/1  bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2"]

    -- The address is of portfolio0 of wallet0, while portfolio1 of
    -- wallet2 is active
    it "names the wallet after the portfolio of its path in a database of several wallets, wallet0 too" $ do
      shown <- addressAnswerDoc WithWallets WithoutCode UnitBitcoin (Just otherRef) segwitAccountRef segwitAddressRow 0
      take 1 (renderedLines shown) `shouldBe` ["bip84/0/1 of portfolio0 of wallet0  bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2"]
