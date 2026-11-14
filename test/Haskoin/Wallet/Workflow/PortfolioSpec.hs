{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wallets and portfolios as the user keeps them, from newportfolio to
-- deletewallet: a new mnemonic and a backup in shares, the portfolios of a
-- mnemonic the user has, the active portfolio, the first one created and
-- then the one switch makes active, the two options that say where a
-- command works in it and their defaults, what portfolios shows, the
-- descriptions and the defaults the user sets, the portfolios and wallets
-- deleted, the files of
-- exportwallet and importwallet between a signer and an online computer,
-- which create whole portfolios only, the files of several wallets, which
-- importwallet sorts by the fingerprints of their mnemonics, the databases
-- of other versions of
-- hw, which hw refuses and leaves as they are, the bip84 account of a
-- portfolio, its addresses and their labels, sync over every portfolio and
-- every network, recover, which finds whole portfolios, a passphrase as
-- typed, the messages the keys sign and check, and the backup of the
-- database.
--
-- A home holds the portfolios of the test mnemonic, made with
-- newportfolio --existing: portfolio0 of wallet0 first, then portfolio1.
-- The second test mnemonic makes wallet1. portfolio0 of wallet0, the first
-- portfolio created, is the active one until a switch. The fake store serves the same
-- chain on every network, so a payment to an address of portfolio0 on btc
-- is seen on btc alone: the other networks derive other keys.
module Haskoin.Wallet.Workflow.PortfolioSpec (spec) where

import Control.Monad (forM_, replicateM_)
import qualified Data.Aeson as Json
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef)
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Response (NewSecret (..), Response (..))
import Haskoin.Wallet.Database.Account (PortfolioView (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..))
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (AccountKind (..), MnemonicPass (..))
import Haskoin.Wallet.Migration.TestUtils (olderDatabases, version2Database, version6Database)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData (..))
import Haskoin.Wallet.Workflow.TestUtils
import Numeric.Natural (Natural)
import System.Directory (copyFile, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  newMnemonicSpec
  newSharesSpec
  existingSpec
  sharesSeedSpec ctx
  activeSpec ctx
  switchSpec
  optionsSpec
  portfoliosSpec ctx
  checkMnemonicSpec
  descriptionSpec
  setDefaultSpec ctx
  deletePortfolioSpec ctx
  deleteWalletSpec ctx
  walletFileSpec ctx
  severalWalletsSpec ctx
  otherVersionSpec
  segwitSpec ctx
  addressSpec ctx
  syncSpec ctx
  recoverSpec ctx
  passphraseSpec ctx
  messageSpec
  verifySpec
  backupSpec

{- Fixtures of this spec -}

-- | The twelve accounts of a whole portfolio, by network and type, in the
-- order of the networks of a portfolio, bip44 first: the table of the
-- specification, the test networks of Bitcoin with both types, those of
-- Bitcoin Cash with bip44 alone
twelveAccounts :: [(String, AccountKind)]
twelveAccounts =
  [ ("btc", Bip44),
    ("btc", Bip84),
    ("bch", Bip44),
    ("btctest", Bip44),
    ("btctest", Bip84),
    ("btctest4", Bip44),
    ("btctest4", Bip84),
    ("btcreg", Bip44),
    ("btcreg", Bip84),
    ("bchtest", Bip44),
    ("bchtest4", Bip44),
    ("bchreg", Bip44)
  ]

-- | The networks of a portfolio, in their order
networkNames :: [String]
networkNames = ["btc", "bch", "btctest", "btctest4", "btcreg", "bchtest", "bchtest4", "bchreg"]

-- | The descriptor of the bip44 account of portfolio0 on btc, as hw
-- writes it: checked with Bitcoin Core 32.1 (test/data/descriptors/README.md)
mainDescriptor :: Text
mainDescriptor = "pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j87"

-- | The descriptor of the bip84 account of portfolio0 on btc, as the
-- Coldcard file of test/data/descriptors writes it
segwitDescriptor :: Text
segwitDescriptor = "wpkh([bc560f95/84h/0h/0h]" <> testSegwitAccountXPub <> "/<0;1>/*)#5nktextl"

-- | The database file of a home
databaseOf :: FilePath -> FilePath
databaseOf home = home </> ".hw" </> "accounts.sqlite"

-- | A signer with portfolio0 and portfolio1 of the test wallet, which
-- exportwallet wrote to the file given, in the home of the signer
twoPortfoliosFile :: FilePath -> IO FilePath
twoPortfoliosFile signer = do
  newTestPortfolio signer
  newTestPortfolio signer
  _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
  return (signer </> "wallet.jsonl")

-- | A chain whose block 800000 pays the first receiving address of each
-- test wallet on btc: 50000 to that of the test mnemonic,
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, and 30000 to that of the second one,
-- 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2. recover finds portfolio0 of each.
bothWalletsPaid :: Ctx -> Chain
bothWalletsPaid ctx = confirmedChain (fundingTx ctx 1 [(externalAddr 0, 50000), (externalAddr2, 30000)])

-- | A home whose two wallets were lost and recovered in the other order,
-- with the file exportwallet wrote of them before the loss.
--
-- Before the loss, wallet0 is of the test mnemonic, bc560f95. It is
-- described "Family", and its first receiving address is labelled "Rent".
-- wallet1 is of the second test mnemonic, 4435aa1c. It is described
-- "Business", and its first receiving address is labelled "Invoices".
--
-- deletewallet stands for the loss of the computer. recover then takes
-- the second mnemonic first. So its wallet is wallet0 afterwards, and the
-- wallet of the test mnemonic is wallet1. portfolio0 of wallet0 is the
-- active portfolio. The store of the home must serve 'bothWalletsPaid',
-- for recover to find the wallets.
walletsRecoveredInReverse :: FilePath -> IO FilePath
walletsRecoveredInReverse home = do
  newTestPortfolio home
  newPortfolioOf home testMnemonic2
  _ <- answer home ["label", "Family", "--of-wallet"] []
  _ <- answer home ["receive", "--label", "Rent"] []
  switchTo home 1 0
  _ <- answer home ["label", "Business", "--of-wallet"] []
  _ <- answer home ["receive", "--label", "Invoices"] []
  _ <- answer home ["exportwallet", home </> "all.jsonl"] []
  -- The wallet of the active portfolio goes last
  _ <- answer home ["deletewallet", "bc560f95", "--wallet", "0"] []
  _ <- answer home ["deletewallet", "4435aa1c", "--wallet", "1"] []
  _ <- answer home ["recover"] (seedOnceOf testMnemonic2)
  _ <- answer home ["recover"] seedOnce
  return (home </> "all.jsonl")

-- | A signer with the three portfolios of 'threePortfolios', which
-- exportwallet wrote whole to the file given, in the home of the signer.
-- The file holds two wallets: that of the test mnemonic, bc560f95, with
-- portfolio0 and portfolio1, then that of the second test mnemonic,
-- 4435aa1c, with portfolio0.
threePortfoliosFile :: FilePath -> IO FilePath
threePortfoliosFile signer = do
  threePortfolios signer
  _ <- answer signer ["exportwallet", signer </> "all.jsonl"] []
  return (signer </> "all.jsonl")

-- | The file of portfolio0 of the test wallet, which exportwallet wrote
-- on a signer, with the text of one key put in place of another, in the
-- home of the signer: a file that names the wallet of the test mnemonic
-- and gives one of its accounts a key that mnemonic does not derive there
fileWithKeyReplaced :: FilePath -> Text -> Text -> IO FilePath
fileWithKeyReplaced signer key replacement = do
  newTestPortfolio signer
  _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
  content <- Text.pack <$> readFile (signer </> "wallet.jsonl")
  writeFile (signer </> "replaced.jsonl") (Text.unpack (Text.replace key replacement content))
  return (signer </> "replaced.jsonl")

-- | The file exportwallet wrote on a signer, at the path given, without
-- its records that hold the text given, in the home of the signer: a file
-- that gives some of the accounts of its portfolios
fileWithout :: FilePath -> FilePath -> Text -> IO FilePath
fileWithout signer file text = do
  records <- Text.lines . Text.pack <$> readFile file
  writeFile (signer </> "part.jsonl") (Text.unpack (Text.unlines (filter (not . Text.isInfixOf text) records)))
  return (signer </> "part.jsonl")

-- | The JSON of the configuration file of a home, as its keys are now
configFileOf :: FilePath -> IO (Maybe Json.Value)
configFileOf home = Json.decodeFileStrict (home </> ".hw" </> "config.json")

{- A new mnemonic -}

newMnemonicSpec :: Spec
newMnemonicSpec =
  describe "A new mnemonic made by newportfolio" $ do
    -- The passphrase is left empty, which is asked once
    it "has 12 words of the entropy of /dev/random by default, and 24 words for 32 bytes" $
      withHome $ \home -> do
        whole <- answer home ["newportfolio"] [""]
        whole `shouldSatisfy` \case ResponseNewPortfolio (Just (NewMnemonic "/dev/random" ws)) _ _ -> length ws == 12; _ -> False
        long <- answer home ["newportfolio", "--entropy", "32"] [""]
        long `shouldSatisfy` \case ResponseNewPortfolio (Just (NewMnemonic _ ws)) _ _ -> length ws == 24; _ -> False

    -- BIP39 adds a word of 11 bits for every 4 bytes of entropy
    it "has 15, 18 and 21 words for 20, 24 and 28 bytes of --entropy" $
      withHome $ \home ->
        forM_ [("20", 15), ("24", 18), ("28", 21)] $ \(bytes, count) -> do
          made <- answer home ["newportfolio", "--entropy", bytes] [""]
          made `shouldSatisfy` \case ResponseNewPortfolio (Just (NewMnemonic _ ws)) _ _ -> length ws == count; _ -> False

    it "is refused an --entropy other than 16, 20, 24, 28 or 32 bytes" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--entropy", "17"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --entropy: cannot parse value `17'"

    -- The rolls are asked once the entropy of the system is read. None are
    -- typed here, and no mnemonic is made without them.
    it "asks for dice rolls with --dice, and is not made without them" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--dice"] []
        runErr run `shouldSatisfy` Text.isInfixOf "Rolls: "
        run `shouldSatisfy` printed "Cancelled: the input ended before the answer"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    it "is a new wallet, wallet0, with its portfolio0, in a database without wallets" $
      withHome $ \home ->
        (portfolioRefsOf <$> answer home ["newportfolio"] [""]) `shouldReturn` Right [PortfolioRef 0 0]

    -- Each mnemonic is a wallet of its own
    it "is wallet1 beside a wallet0, with its own portfolio0" $
      withHome $ \home -> do
        newTestPortfolio home
        (portfolioRefsOf <$> answer home ["newportfolio"] [""]) `shouldReturn` Right [PortfolioRef 1 0]

    it "gets a whole portfolio0: an account of each type on every network, the test networks included" $
      withHome $ \home -> do
        made <- answer home ["newportfolio"] [""]
        (map (\(_, net, kind, _) -> (net, kind)) <$> accountXPubsOf made) `shouldBe` Right twelveAccounts

    -- The mnemonic printed, with the passphrase typed twice, gives the
    -- keys of the wallet
    it "is the mnemonic of the wallet, with the passphrase typed twice, as checkmnemonic checks" $
      withHome $ \home -> do
        made <- answer home ["newportfolio"] ["secret", "secret"]
        mnemonic <- case made of
          ResponseNewPortfolio (Just (NewMnemonic _ ws)) _ _ -> return (Text.unwords ws)
          other -> fail ("Not a new mnemonic: " <> show other)
        (checkedOf <$> hwJson home ["checkmnemonic"] [mnemonic, "secret"])
          `shouldReturn` Right (True, "The mnemonic and passphrase matched portfolio0 of wallet0")

    it "asks for the passphrase again when the second one does not match, and makes nothing on the third miss" $
      withHome $ \home -> do
        (errorOf <$> hwJson home ["newportfolio"] ["one", "two", "one", "two", "one", "two"])
          `shouldReturn` Right "The passphrases do not match"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    -- The words come first, then the portfolio, which hw keeps, and the
    -- active portfolio, which it is in a database that had none
    it "prints the mnemonic once, its words numbered, then the portfolio it created" $
      withHome $ \home -> do
        run <- hw home ["newportfolio"] [""]
        let shown = Text.lines (runOut run)
        take 1 shown `shouldBe` ["Mnemonic of 12 words, from /dev/random"]
        -- Three lines of four words, numbered
        map (Text.take 5) (take 3 (drop 1 shown)) `shouldBe` ["   1.", "   5.", "   9."]
        drop 4 shown `shouldSatisfy` \case
          [ "",
            "Created portfolio0 of wallet0",
            fingerprint,
            "  Accounts   : 12, on btc (bip44 and bip84), bch (bip44),",
            "               btctest (bip44 and bip84), btctest4 (bip44 and bip84),",
            "               btcreg (bip44 and bip84), bchtest (bip44), bchtest4 (bip44)",
            "               and bchreg (bip44)",
            "portfolio0 of wallet0 is the active portfolio: the commands work in it."
            ] -> "  Fingerprint: " `Text.isPrefixOf` fingerprint
          _ -> False

    -- 24 words are wider than 80 columns, as hw lays out its answers. The
    -- line printed is the mnemonic as the user copies it, and as
    -- checkmnemonic takes it back.
    it "prints its 24 words on one line with --one-line, a line that checkmnemonic takes as it is" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--one-line", "--entropy", "32"] ["secret", "secret"]
        copied <- case Text.lines (runOut run) of
          _heading : line : _ -> return line
          other -> fail ("Not a mnemonic on a line: " <> show other)
        (checkedOf <$> hwJson home ["checkmnemonic"] [copied, "secret"])
          `shouldReturn` Right (True, "The mnemonic and passphrase matched portfolio0 of wallet0")

    it "refuses --entropy, --dice, --shares, --threshold and --one-line with --existing, before the mnemonic is asked" $
      withHome $ \home ->
        forM_ [["--entropy", "16"], ["--dice"], ["--shares", "2", "--threshold", "2"], ["--threshold", "2"], ["--one-line"]] $ \options -> do
          run <- hw home (["newportfolio", "--existing"] <> options) seedTwice
          run `shouldSatisfy` printed "--entropy, --dice, --shares, --threshold and --one-line are for a new mnemonic: they do not go with --existing"
          run `shouldNotSatisfy` askedMnemonic

{- A new backup in shares -}

newSharesSpec :: Spec
newSharesSpec =
  describe "A new backup in shares made by newportfolio --shares" $ do
    it "comes in the shares of --shares, of 20 words each, with the threshold of --threshold" $
      withHome $ \home -> do
        made <- answer home ["newportfolio", "--shares", "3", "--threshold", "2"] [""]
        made `shouldSatisfy` \case ResponseNewPortfolio (Just (NewShares "/dev/random" 2 shares)) _ _ -> map length shares == [20, 20, 20]; _ -> False

    it "has shares of 33 words for 32 bytes of --entropy" $
      withHome $ \home -> do
        made <- answer home ["newportfolio", "--shares", "2", "--threshold", "2", "--entropy", "32"] [""]
        made `shouldSatisfy` \case ResponseNewPortfolio (Just (NewShares _ 2 shares)) _ _ -> map length shares == [33, 33]; _ -> False

    it "is one share, the whole backup, with --shares 1, with or without --threshold 1" $
      withHome $ \home ->
        forM_ [["newportfolio", "--shares", "1"], ["newportfolio", "--shares", "1", "--threshold", "1"]] $ \args -> do
          made <- answer home args [""]
          made `shouldSatisfy` \case ResponseNewPortfolio (Just (NewShares _ 1 [share])) _ _ -> length share == 20; _ -> False

    -- A heading, then each share under its number, five lines of four
    -- words: 1 + 2 x 7 lines, then a blank line and the portfolio
    it "prints a heading, then each share under its number, then the portfolio it created" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--shares", "2", "--threshold", "2"] [""]
        let shown = Text.lines (runOut run)
        take 3 shown `shouldBe` ["Backup in 2 shares of 20 words, any 2 of them rebuild the seed, from /dev/random", "", "Share 1"]
        take 2 (drop 15 shown) `shouldBe` ["", "Created portfolio0 of wallet0"]

    -- Without a passphrase, which is asked once. Shares are typed once.
    it "makes with newportfolio --existing the same portfolio from any threshold of its shares" $
      withHome $ \home -> withHome $ \firstHome -> withHome $ \lastHome -> do
        made <- answer home ["newportfolio", "--shares", "3", "--threshold", "2"] [""]
        shares <- case made of
          ResponseNewPortfolio (Just (NewShares _ _ shares@[_, _, _])) _ _ -> return (Text.unwords <$> shares)
          other -> fail $ "Not a backup in three shares: " <> show other
        fromFirst <- accountXPubsOf <$> answer firstHome ["newportfolio", "--existing"] (take 2 shares <> [""])
        fromLast <- accountXPubsOf <$> answer lastHome ["newportfolio", "--existing"] (reverse (drop 1 shares) <> [""])
        fromLast `shouldBe` fromFirst
        fromFirst `shouldSatisfy` either (const False) ((== 12) . length)

    it "needs --threshold for more than one share" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["newportfolio", "--shares", "3"] [""])
          `shouldReturn` Right "With more than one share, give --threshold: the number of shares that rebuild the seed"

    it "is refused a threshold of 1 for more than one share" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["newportfolio", "--shares", "3", "--threshold", "1"] [""])
          `shouldReturn` Right "With a threshold of 1 every share would be the whole backup: make one share, and copy it"

    it "is refused a threshold above the number of shares" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["newportfolio", "--shares", "3", "--threshold", "4"] [""])
          `shouldReturn` Right "The threshold cannot be greater than the number of shares, 3"

    it "is refused --threshold without --shares" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["newportfolio", "--threshold", "2"] [""])
          `shouldReturn` Right "--threshold goes with --shares: it is the number of shares that rebuild the seed"

    it "is refused 20, 24 or 28 bytes of --entropy, which a mnemonic alone has" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["newportfolio", "--shares", "1", "--entropy", "20"] [""])
          `shouldReturn` Right "A backup in shares has 16 or 32 bytes of entropy, not 20"

    it "comes in 1 to 16 shares" $
      withHome $ \home ->
        forM_ ["0", "17"] $ \shares -> do
          run <- hw home ["newportfolio", "--shares", shares, "--threshold", "2"] [""]
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf "The --shares value has to be between 1 and 16"

    -- 16 bytes for the seed and 12 for the random value that splits it:
    -- 224 bits, about 135 rolls of 5/3 bits
    it "asks with --dice for the rolls of the seed and of the random value that splits it" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--shares", "3", "--threshold", "2", "--dice"] []
        runErr run `shouldSatisfy` Text.isInfixOf "Roll a die about 135 times"
        run `shouldSatisfy` printed "Cancelled: the input ended before the answer"

{- A mnemonic the user has -}

-- | The keys of portfolio0 of the test wallet, account by account: the
-- five keys of the table of the specification, the test networks sharing
-- theirs
portfolio0Keys :: [(String, AccountKind, Text)]
portfolio0Keys =
  [ ("btc", Bip44, testAccountXPub),
    ("btc", Bip84, testSegwitAccountXPub),
    ("bch", Bip44, testAccountXPubBch),
    ("btctest", Bip44, testAccountXPubTest),
    ("btctest", Bip84, testSegwitAccountXPubTest),
    ("btctest4", Bip44, testAccountXPubTest),
    ("btctest4", Bip84, testSegwitAccountXPubTest),
    ("btcreg", Bip44, testAccountXPubTest),
    ("btcreg", Bip84, testSegwitAccountXPubTest),
    ("bchtest", Bip44, testAccountXPubTest),
    ("bchtest4", Bip44, testAccountXPubTest),
    ("bchreg", Bip44, testAccountXPubTest)
  ]

existingSpec :: Spec
existingSpec =
  describe "A portfolio of a mnemonic the user has, added by newportfolio --existing" $ do
    it "is portfolio0 of a new wallet, wallet0, when the database has no wallet of the mnemonic" $
      withHome $ \home ->
        (portfolioRefsOf <$> answer home ["newportfolio", "--existing"] seedTwice) `shouldReturn` Right [PortfolioRef 0 0]

    it "is in a wallet with the fingerprint of the seed of the mnemonic" $
      withHome $ \home ->
        (fingerprintsOf <$> answer home ["newportfolio", "--existing"] seedTwice) `shouldReturn` Right [(PortfolioRef 0 0, testFingerprintText)]

    -- m/44'/0'/0', m/84'/0'/0', m/44'/145'/0', m/44'/1'/0' and m/84'/1'/0'
    it "holds the twelve accounts of the five keys the seed derives at the paths of portfolio0" $
      withHome $ \home ->
        (fmap (map (\(_, net, kind, key) -> (net, kind, key))) . accountXPubsOf <$> answer home ["newportfolio", "--existing"] seedTwice)
          `shouldReturn` Right portfolio0Keys

    -- The first portfolio of the database becomes the active one
    it "prints the portfolio it created, with the fingerprint of its wallet and its twelve accounts" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--existing"] seedTwice
        runOut run
          `shouldBe` "Created portfolio0 of wallet0\n\
                     \  Fingerprint: bc560f95\n\
                     \  Accounts   : 12, on btc (bip44 and bip84), bch (bip44),\n\
                     \               btctest (bip44 and bip84), btctest4 (bip44 and bip84),\n\
                     \               btcreg (bip44 and bip84), bchtest (bip44), bchtest4 (bip44)\n\
                     \               and bchreg (bip44)\n\
                     \portfolio0 of wallet0 is the active portfolio: the commands work in it.\n"

    -- m/44'/0'/1' and m/84'/0'/1' of the test seed
    it "is portfolio1 when the wallet has portfolio0, with the keys at the account index 1" $
      withHome $ \home -> do
        newTestPortfolio home
        added <- answer home ["newportfolio", "--existing"] seedTwice
        portfolioRefsOf added `shouldBe` Right [PortfolioRef 0 1]
        (filter (\(_, net, _, _) -> net == "btc") <$> accountXPubsOf added)
          `shouldBe` Right [(PortfolioRef 0 1, "btc", Bip44, testAccountXPub1), (PortfolioRef 0 1, "btc", Bip84, testSegwitAccountXPub1)]

    -- The online computer has portfolio0, and gets portfolio2 alone from
    -- the file of a signer
    it "is portfolio1 when the wallet has portfolio0 and portfolio2: the lowest free index" $
      withHome $ \signer -> withHome $ \online -> do
        replicateM_ 3 (newTestPortfolio signer)
        _ <- answer signer ["exportwallet", "--portfolio", "2", signer </> "portfolio2.jsonl"] []
        newTestPortfolio online
        _ <- answer online ["importwallet", signer </> "portfolio2.jsonl"] []
        newTestPortfolio online
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 0 2]

    it "is portfolio0 of wallet1 for a second mnemonic, beside wallet0" $
      withHome $ \home -> do
        newTestPortfolio home
        (portfolioRefsOf <$> answer home ["newportfolio", "--existing"] (seedTwiceOf testMnemonic2)) `shouldReturn` Right [PortfolioRef 1 0]

    -- wallet0 is of the second test mnemonic, and holds the active portfolio.
    -- wallet1 is of the test mnemonic, with portfolio0.
    it "is the next portfolio of the wallet with the fingerprint of the seed, among the wallets of other seeds" $
      withHome $ \home -> do
        newPortfolioOf home testMnemonic2
        newTestPortfolio home
        (portfolioRefsOf <$> answer home ["newportfolio", "--existing"] seedTwice) `shouldReturn` Right [PortfolioRef 1 1]

    -- Under the umask 022, SQLite would make the database rw-r--r--
    it "is kept in a database its owner alone can read, whatever the umask lets" $
      withHome $ \home -> underUmask 0o022 $ do
        newTestPortfolio home
        -- The permission bits of rw-------
        (.&. 0o777) . fileMode <$> getFileStatus (databaseOf home) `shouldReturn` 0o600

    it "asks for the mnemonic again when it is typed differently the second time" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--existing"] ([mnemonicWords testMnemonic, mnemonicWords testMnemonic2] <> seedTwice)
        run `shouldSatisfy` printed "The mnemonics do not match"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]

    it "is cancelled when the input ends before the passphrase, and leaves nothing" $
      withHome $ \home -> do
        (errorOf <$> hwJson home ["newportfolio", "--existing"] (take 2 seedTwice)) `shouldReturn` Right "Cancelled: the input ended before the answer"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

{- A portfolio of a backup in shares -}

sharesSeedSpec :: Ctx -> Spec
sharesSeedSpec ctx =
  describe "A portfolio of a backup in shares" $ do
    it "is created from the shares typed once and the passphrase typed twice, with the key of the test networks the device tests derive" $
      withHome $ \home -> do
        made <- answer home ["newportfolio", "--existing"] sharesForNew
        (filter (\(_, net, kind, _) -> (net, kind) == ("btctest", Bip44)) <$> accountXPubsOf made)
          `shouldBe` Right [(PortfolioRef 0 0, "btctest", Bip44, testSharesTestnetXPub)]

    it "hands out on btctest the address the device tests of a hardware wallet give for the backup" $
      withHome $ \home -> do
        _ <- answer home ["newportfolio", "--existing"] sharesForNew
        receiveWith home ["--network", "btctest"] `shouldReturn` Text.unpack (testSharesTestnetAddrs !! 0)

    it "is the same account on Bitcoin as its seed gives apart from hw" $
      withHome $ \home -> do
        made <- answer home ["newportfolio", "--existing"] sharesForNew
        (filter (\(_, net, kind, _) -> (net, kind) == ("btc", Bip44)) <$> accountXPubsOf made)
          `shouldBe` Right [(PortfolioRef 0 0, "btc", Bip44, testSharesXPub)]
        receiveWith home [] `shouldReturn` Text.unpack testSharesAddr

    it "is in a wallet with the fingerprint of its seed" $
      withHome $ \home -> do
        _ <- answer home ["newportfolio", "--existing"] sharesForNew
        (fingerprintsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, testSharesFingerprintText)]

    -- The first share is a group of one, the three others are of a group
    -- that needs three: two groups, as the backup needs
    it "is created from the shares of a backup of several groups, and hands out the address the device tests give" $
      withHome $ \home -> do
        _ <- answer home ["newportfolio", "--existing"] (testGroupShares <> ["TREZOR", "TREZOR"])
        receiveWith home ["--network", "btctest"] `shouldReturn` Text.unpack testGroupSharesTestnetAddr

    -- Three attempts, each refused before the passphrase is typed again
    it "is not created with a passphrase that has an accent, which the format of the shares does not take" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["newportfolio", "--existing"] (testShares <> replicate 3 "caf\233"))
          `shouldReturn` Right "The passphrase of a backup in shares has printable ASCII characters only, as its format asks"

    it "is checked by checkmnemonic against its shares and its passphrase" $
      withHome $ \home -> do
        _ <- answer home ["newportfolio", "--existing"] sharesForNew
        (checkedOf <$> hwJson home ["checkmnemonic"] sharesOnce)
          `shouldReturn` Right (True, "The shares and passphrase matched portfolio0 of wallet0")
        (checkedOf <$> hwJson home ["checkmnemonic"] (testShares <> ["trezor"]))
          `shouldReturn` Right (False, "The shares and passphrase did not match portfolio0 of wallet0")

    it "is not matched by the mnemonic of another seed" $
      withHome $ \home -> do
        _ <- answer home ["newportfolio", "--existing"] sharesForNew
        (checkedOf <$> hwJson home ["checkmnemonic"] seedOnce)
          `shouldReturn` Right (False, "The mnemonic and passphrase did not match portfolio0 of wallet0")

    it "signs a message from its shares, which verifymsg checks against its address" $
      withHome $ \home -> do
        _ <- answer home ["newportfolio", "--existing"] sharesForNew
        address <- receiveWith home []
        signed <- hwJson home ["signmsg", address, "hello"] sharesOnce
        signature <- case signed of
          ResponseSignMsg "hello" signature _ -> return signature
          other -> fail $ "Not a signed message: " <> show other
        verified <- hwJson home ["verifymsg", address, Text.unpack signature, "hello"] []
        verified `shouldBe` ResponseVerifyMsg "hello" signature testSharesAddr True

    -- recover searches the test networks too
    it "is found by recover from its shares, when it was paid on btctest" $
      withChain ctx (chainPaying ctx sharesTestnetAddr) $ \home _ -> do
        found <- hwJson home ["recover"] sharesOnce
        recoveredOf found `shouldBe` Right (Just 0, [0])
        (lookup (Place (PortfolioRef 0 0) btcTest) <$> syncedOf found) `shouldBe` Right (Just 50000)

{- The active portfolio -}

-- | A home with portfolio0 and portfolio1 of wallet0, of the test
-- mnemonic, and portfolio0 of wallet1, of the second one. portfolio0 of
-- wallet0, the first created, is the active portfolio.
threePortfolios :: FilePath -> IO ()
threePortfolios home = do
  newTestPortfolio home
  newTestPortfolio home
  newPortfolioOf home testMnemonic2

-- The first receiving addresses: 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj of
-- the bip44 account of portfolio0 on btc, 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo
-- of portfolio1, and 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2 of portfolio0 of
-- wallet1
activeSpec :: Ctx -> Spec
activeSpec ctx =
  describe "The active portfolio, the one the commands work in" $ do
    it "is the portfolio newportfolio makes in a database without any" $
      withHome $ \home -> do
        _ <- answer home ["newportfolio"] [""]
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    it "is the portfolio newportfolio --existing makes in a database without any" $
      withHome $ \home -> do
        newTestPortfolio home
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    it "stays the first portfolio created when newportfolio --existing makes the next one of its wallet" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    -- A new mnemonic is a wallet of its own, wallet1
    it "stays the first portfolio created when newportfolio makes a new wallet" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["newportfolio"] [""]
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    it "is said by newportfolio to stay, with the switch to the portfolio it made" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["newportfolio", "--existing"] seedTwice
        runOut run
          `shouldBe` "Created portfolio1 of wallet0\n\
                     \  Fingerprint: bc560f95\n\
                     \  Accounts   : 12, on btc (bip44 and bip84), bch (bip44),\n\
                     \               btctest (bip44 and bip84), btctest4 (bip44 and bip84),\n\
                     \               btcreg (bip44 and bip84), bchtest (bip44), bchtest4 (bip44)\n\
                     \               and bchreg (bip44)\n\
                     \The active portfolio is still portfolio0 of wallet0.\n\
                     \\n\
                     \To work in portfolio1 of wallet0, make it the active one:\n\
                     \  hw switch 1 --wallet 0\n"

    -- A wallet is created with its portfolio0, the lowest index, though
    -- the chain pays the first receiving address of portfolio1 alone
    it "is portfolio0 of the wallet recover creates in a database without any, though portfolio1 alone was paid" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        _ <- answer home ["recover"] seedOnce
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    it "is said by recover to be the portfolio it recovered, in a database without any" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        take 2 (Text.lines (runOut run))
          `shouldBe` ["Recovered portfolio0 of wallet0", "portfolio0 of wallet0 is the active portfolio: the commands work in it."]

    -- The chain pays the first receiving address of the second test
    -- mnemonic, whose wallet recover creates as wallet1
    it "stays as it was when recover creates the portfolios of another wallet" $
      withChain ctx (chainPaying ctx externalAddr2) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["recover"] (seedOnceOf testMnemonic2)
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    it "is said by recover to stay, with the switch to the portfolio it recovered" $
      withChain ctx (chainPaying ctx externalAddr2) $ \home _ -> do
        newTestPortfolio home
        run <- hw home ["recover"] (seedOnceOf testMnemonic2)
        run `shouldSatisfy` printed "Recovered portfolio0 of wallet1\nThe active portfolio is still portfolio0 of wallet0.\n"
        run `shouldSatisfy` printed "\nTo work in portfolio0 of wallet1, make it the active one:\n  hw switch 0 --wallet 1\n"

    -- The file has portfolio0 and portfolio1: the first one created is
    -- portfolio0
    it "is portfolio0 of the wallet importwallet creates in a database without any" $
      withHome $ \signer -> withHome $ \online -> do
        file <- twoPortfoliosFile signer
        _ <- answer online ["importwallet", file] []
        activePortfolioIn online `shouldReturn` Right [PortfolioRef 0 0]

    it "stays as it was when importwallet creates a portfolio of its wallet" $
      withHome $ \signer -> withHome $ \online -> do
        file <- twoPortfoliosFile signer
        newTestPortfolio online
        _ <- answer online ["importwallet", file] []
        activePortfolioIn online `shouldReturn` Right [PortfolioRef 0 0]

    it "is said by importwallet to stay, with the switch to the first portfolio it created" $
      withHome $ \signer -> withHome $ \online -> do
        file <- twoPortfoliosFile signer
        newTestPortfolio online
        run <- hw online ["importwallet", file] []
        runOut run
          `shouldBe` Text.unlines
            [ "wallet0  bc560f95  already in the database",
              "  portfolio0  nothing to add",
              "  portfolio1  created, 12 accounts added",
              "",
              "The active portfolio is still portfolio0 of wallet0.",
              "",
              "Labels set: none",
              "",
              "To work in portfolio1 of wallet0, make it the active one:",
              "  hw switch 1 --wallet 0"
            ]

    it "is kept from one command to the next, until the next switch" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        _ <- answer home ["switch", "--portfolio", "1"] []
        receiveWith home [] `shouldReturn` "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo"
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [("17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "")]
        _ <- answer home ["switch", "--portfolio", "0"] []
        receiveWith home [] `shouldReturn` Text.unpack (externalAddrsText !! 0)

    -- A copy of the database is a database of its own
    it "is kept in the database, which a copy made by backup has too" $
      withHome $ \home -> withHome $ \restored -> do
        newTestPortfolio home
        newTestPortfolio home
        _ <- answer home ["switch", "--portfolio", "1"] []
        _ <- answer home ["backup", home </> "copy.sqlite"] []
        copyFile (home </> "copy.sqlite") (databaseOf restored)
        activePortfolioIn restored `shouldReturn` Right [PortfolioRef 0 1]

    it "is not written to the configuration file, which switch leaves as it was" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        earlier <- configFileOf home
        _ <- answer home ["switch", "--portfolio", "1"] []
        configFileOf home `shouldReturn` earlier

    it "is the first portfolio created after the last wallet is deleted" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["deletewallet", "bc560f95", "--wallet", "0"] []
        newPortfolioOf home testMnemonic2
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    -- A configuration file of an earlier build of hw 0.10 may still have
    -- them. wallet1 is of the second test mnemonic.
    it "is not the one the keys default-wallet and default-portfolio of an earlier build name" $
      withHome $ \home -> do
        threePortfolios home
        setConfigKey home "default-wallet" (Json.Number 1)
        setConfigKey home "default-portfolio" (Json.Number 0)
        receiveWith home [] `shouldReturn` Text.unpack (externalAddrsText !! 0)

    it "comes with a warning about the keys default-wallet and default-portfolio of an earlier build, which hw leaves aside" $
      withHome $ \home -> do
        newTestPortfolio home
        setConfigKey home "default-wallet" (Json.Number 0)
        setConfigKey home "default-portfolio" (Json.Number 0)
        run <- hw home ["receive"] []
        runExit run `shouldBe` ExitSuccess
        runErr run
          `shouldSatisfy` Text.isInfixOf ("The configuration file " <> Text.pack (home </> ".hw" </> "config.json") <> " has keys hw does not know, left aside: default-portfolio, default-wallet\n")

{- Switching -}

-- The store of the test home refuses every connection: switch needs none,
-- as on an offline computer
switchSpec :: Spec
switchSpec =
  describe "A portfolio made active by switch" $ do
    it "is the portfolio its argument names, in the wallet of the active portfolio without --wallet" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "1"] []
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 1]

    it "is the portfolio its argument names, in the wallet --wallet names" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "0", "--wallet", "1"] []
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 1 0]

    it "is the portfolio --portfolio names, in the wallet of the active portfolio without --wallet" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--portfolio", "1"] []
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 1]

    it "is the portfolio of the wallet --wallet names" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--wallet", "1", "--portfolio", "0"] []
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 1 0]

    -- wallet1 has portfolio0 and portfolio1 of the second test mnemonic
    it "is in the wallet of the portfolio active now without --wallet, after a switch to another wallet" $
      withHome $ \home -> do
        threePortfolios home
        newPortfolioOf home testMnemonic2
        _ <- answer home ["switch", "--wallet", "1", "--portfolio", "0"] []
        _ <- answer home ["switch", "--portfolio", "1"] []
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 1 1]

    it "takes the names the listings give, wallet1 and portfolio0, for --wallet and --portfolio" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--wallet", "wallet1", "--portfolio", "portfolio0"] []
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 1 0]

    it "is where the commands work next: receive gives out an address of the portfolio of the other wallet" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--wallet", "1", "--portfolio", "0"] []
        receiveWith home [] `shouldReturn` Text.unpack externalAddr2Text

    -- The portfolio as portfolios shows it, with the mark of the active one
    it "is shown in the answer, marked, after a line that says it is the active portfolio now" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        run <- hw home ["switch", "--portfolio", "1"] []
        Text.lines (runOut run) `shouldSatisfy` \case
          ["The active portfolio is now portfolio1 of wallet0.", "* portfolio1  wallet0", created, "    no addresses yet"] -> "    created " `Text.isPrefixOf` created
          _ -> False

    it "is answered in JSON with the portfolio, marked as the active one" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        (activePortfoliosOf <$> hwJson home ["switch", "--portfolio", "1"] []) `shouldReturn` Right [PortfolioRef 0 1]

    it "is refused a --wallet the database does not have, naming where the wallets are listed, and the active portfolio stays" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["switch", "--wallet", "3", "--portfolio", "0"] [])
          `shouldReturn` Right "There is no wallet3: portfolios lists the wallets"
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    it "is refused a --portfolio the wallet --wallet names does not have" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["switch", "--wallet", "1", "--portfolio", "1"] [])
          `shouldReturn` Right "wallet1 has no portfolio1: portfolios lists the portfolios"

    it "is refused a --portfolio the wallet of the active portfolio does not have, without --wallet" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["switch", "--portfolio", "7"] [])
          `shouldReturn` Right "wallet0 has no portfolio7: portfolios lists the portfolios"

    it "is refused in a database without a portfolio, which is told how to make one" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["switch", "--portfolio", "0"] [])
          `shouldReturn` Right "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"

    it "is refused without a portfolio, which switch never takes from the active portfolio" $
      withHome $ \home -> do
        threePortfolios home
        run <- hw home ["switch", "--wallet", "1"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Missing: (PORTFOLIO | (-p|--portfolio INT))"
        activePortfolioIn home `shouldReturn` Right [PortfolioRef 0 0]

    -- An account index of BIP32 is below 2^31 = 2147483648: an index of
    -- 2^64 would wrap around to portfolio0
    it "is refused a --portfolio or a --wallet of 2^31 or more" $
      withHome $ \home ->
        forM_ [("--portfolio", "portfolio", ["--wallet", "0"]), ("--wallet", "wallet", ["--portfolio", "0"])] $ \(option, what, other) ->
          forM_ ["2147483648", "18446744073709551616"] $ \index -> do
            run <- hw home (["switch", option, index] <> other) []
            runExit run `shouldBe` ExitFailure 1
            runErr run `shouldSatisfy` Text.isInfixOf ("option " <> Text.pack option <> ": The " <> Text.pack option <> " value " <> Text.pack index <> " is too large: the index of a " <> what <> " is below 2147483648, which is 2^31")

    it "takes the index 2^31 - 1 for --portfolio, the highest a portfolio can have" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["switch", "--portfolio", "2147483647"] [])
          `shouldReturn` Right "wallet0 has no portfolio2147483647: portfolios lists the portfolios"

{- Where a command works in the active portfolio -}

-- The first receiving addresses: 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj of
-- the bip44 account of portfolio0 on btc, the bip84 one
-- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc, the one on bch
-- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u. The
-- configuration file of the test home names btc.
optionsSpec :: Spec
optionsSpec =
  describe "Where a command works in the active portfolio, which two options and their defaults say" $ do
    it "is the bip44 account of portfolio0 of wallet0 on btc without an option or a key" $
      withHome $ \home -> do
        threePortfolios home
        receiveWith home [] `shouldReturn` Text.unpack (externalAddrsText !! 0)

    it "is on the network --network names" $
      withHome $ \home -> do
        threePortfolios home
        receiveWith home ["--network", "bch"] `shouldReturn` Text.unpack bchExternalAddrText

    it "is the account of the type --type names" $
      withHome $ \home -> do
        threePortfolios home
        receiveWith home ["--type", "bip84"] `shouldReturn` Text.unpack (segwitExternalAddrsText !! 0)

    it "is the account of the type --type names in the portfolio switch made active" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--portfolio", "1"] []
        (accountRefOf <$> answer home ["receive", "--type", "bip84"] []) `shouldReturn` Right (AccountRef portfolio1Place Bip84)

    it "is on the network the key default-network names" $
      withHome $ \home -> do
        threePortfolios home
        setConfigKey home "default-network" (Json.String "bch")
        receiveWith home [] `shouldReturn` Text.unpack bchExternalAddrText

    it "is the account of the type the key default-type names" $
      withHome $ \home -> do
        threePortfolios home
        setConfigKey home "default-type" (Json.String "bip84")
        receiveWith home [] `shouldReturn` Text.unpack (segwitExternalAddrsText !! 0)

    it "is on the --network given rather than the one of the key default-network" $
      withHome $ \home -> do
        threePortfolios home
        setConfigKey home "default-network" (Json.String "bch")
        receiveWith home ["--network", "btc"] `shouldReturn` Text.unpack (externalAddrsText !! 0)

    -- Bitcoin Cash has no SegWit
    it "is the bip44 account on bch though the key default-type says bip84" $
      withHome $ \home -> do
        threePortfolios home
        setConfigKey home "default-type" (Json.String "bip84")
        receiveWith home ["--network", "bch"] `shouldReturn` Text.unpack bchExternalAddrText

    it "is refused with --type bip84 on bch, which has no SegWit, and no address is given out" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["receive", "--network", "bch", "--type", "bip84"] [])
          `shouldReturn` Right "A bip84 account needs SegWit, which bch does not have"
        receiveWith home ["--network", "bch"] `shouldReturn` Text.unpack bchExternalAddrText

    it "is refused with --type bip84 on a test network of Bitcoin Cash too" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["receive", "--network", "bchtest", "--type", "bip84"] [])
          `shouldReturn` Right "A bip84 account needs SegWit, which bchtest does not have"

    it "is refused with --type bip84 when the key default-network names bch" $
      withHome $ \home -> do
        threePortfolios home
        setConfigKey home "default-network" (Json.String "bch")
        (errorOf <$> hwJson home ["receive", "--type", "bip84"] [])
          `shouldReturn` Right "A bip84 account needs SegWit, which bch does not have"

    -- bip49, wrapped SegWit, is no type of a portfolio
    it "is refused with a --type other than bip44 and bip84" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["receive", "--type", "bip49"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --type: The type of an account is bip44 or bip84"

{- The portfolios -}

-- | A chain whose block 800000 pays 50000 to the first receiving address
-- of the bip44 account of portfolio0 on btc, m/44'/0'/0'/0/0, and 20000
-- to the one on bch, m/44'/145'/0'/0/0
bothNetworksChain :: Ctx -> Chain
bothNetworksChain ctx = Chain 800002 [(fundingTx ctx 1 [(externalAddr 0, 50000), (bchExternalAddr, 20000)], Just 800000)]

portfoliosSpec :: Ctx -> Spec
portfoliosSpec ctx =
  describe "The portfolios, as portfolios shows them" $ do
    it "are listed with what each can spend on each network" $
      withHome $ \home -> do
        newTestPortfolio home
        (spendableOn btc <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, 0)]

    it "are listed portfolio by portfolio, the portfolios of wallet0 first" $
      withHome $ \home -> do
        threePortfolios home
        (portfolioRefsOf <$> hwJson home ["portfolios"] [])
          `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 1 0]

    it "are listed every one, whichever is active" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--wallet", "1", "--portfolio", "0"] []
        (portfolioRefsOf <$> hwJson home ["portfolios"] [])
          `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 1 0]

    -- As git branch marks the current branch: the lines under the first
    -- of each portfolio are indented by 4, past the mark
    it "mark the active portfolio with a * at the start of its first line, and the others with a space" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--portfolio", "1"] []
        run <- hw home ["portfolios"] []
        filter (Text.isInfixOf "  wallet") (Text.lines (runOut run))
          `shouldBe` ["  portfolio0  wallet0", "* portfolio1  wallet0", "  portfolio0  wallet1"]

    it "mark the active portfolio the same way with --verbose" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--wallet", "1", "--portfolio", "0"] []
        run <- hw home ["portfolios", "--verbose"] []
        filter (Text.isInfixOf "  wallet") (Text.lines (runOut run))
          `shouldBe` ["  portfolio0  wallet0", "  portfolio1  wallet0", "* portfolio0  wallet1"]

    it "say in JSON which portfolio is the active one" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--portfolio", "1"] []
        (activePortfoliosOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 1]

    -- A table of portfolios nothing paid: a row each, no network
    it "mark the active portfolio with --tabular in a first column, which has no heading" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--portfolio", "1"] []
        run <- hw home ["portfolios", "--tabular"] []
        map (Text.take 26) (Text.lines (runOut run))
          `shouldBe` [ "  │ Wallet  │ Portfolio  │",
                       "──┼─────────┼────────────┼",
                       "  │ wallet0 │ portfolio0 │",
                       "* │ wallet0 │ portfolio1 │",
                       "  │ wallet1 │ portfolio0 │"
                     ]

    it "say there is none to display in a database without wallets" $
      withHome $ \home -> do
        run <- hw home ["portfolios"] []
        runOut run `shouldBe` "There are no portfolios to display\n"

    -- 50000 at the bip44 /0/0 and 30000 at the bip84 /0/0: 50000 + 30000
    -- = 80000
    it "add up on btc what the bip44 and the bip84 account can spend" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        (spendableOn btc <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, 80000)]

    it "count on btc the coins and the addresses of both accounts together, on one line" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        run <- hw home ["portfolios"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n    btc  spendable 0.00080000 btc, 2 coins, 2 addresses\n"

    it "keep the figures of each network apart" $
      withChain ctx (bothNetworksChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        listed <- hwJson home ["portfolios"] []
        spendableOn btc listed `shouldBe` Right [(PortfolioRef 0 0, 50000)]
        spendableOn bch listed `shouldBe` Right [(PortfolioRef 0 0, 20000)]

    -- Nothing has paid the portfolio, and it has handed out no address:
    -- the networks it has accounts on are not worth a line each. It is the
    -- active portfolio, marked.
    it "say of a portfolio that gave out no address that it has none yet" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["portfolios"] []
        Text.lines (runOut run) `shouldSatisfy` \case
          ["* portfolio0  wallet0", created, "    no addresses yet"] -> "    created " `Text.isPrefixOf` created
          _ -> False

    it "are listed with --verbose with the fingerprint of the seed of their wallet" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["portfolios", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n    Fingerprint: " <> testFingerprintText <> "\n")

    -- The time is in the time zone of the computer, Central Europe here
    it "are listed with --verbose with the time they were created, to the second" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hwWithEnv [("TZ", centralEurope)] home ["portfolios", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n    Created    : "
        runOut run `shouldSatisfy` (\out -> " CET\n" `Text.isInfixOf` out || " CEST\n" `Text.isInfixOf` out)

    -- A wallet that takes only the SLIP-132 form for a native SegWit
    -- account makes a legacy wallet of an xpub
    it "are listed with --verbose with the descriptor of the bip84 account on btc, and its zpub on a row of its own" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["portfolios", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n      bip84  /84'/0'/0'\n        Descriptor: " <> segwitDescriptor <> "\n        SLIP-132  : " <> testSegwitAccountZPub <> "\n")

    -- SLIP-132 gives bip44 accounts the prefix of BIP32
    it "are listed with --verbose with the descriptor of the bip44 account on btc, and its xpub as its SLIP-132 key" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["portfolios", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n      bip44  /44'/0'/0'\n        Descriptor: " <> mainDescriptor <> "\n        SLIP-132  : " <> testAccountXPub <> "\n")

    it "are listed with --verbose with every network of the portfolio, whatever it holds there" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["portfolios", "--verbose"] []
        filter (`elem` map (("    " <>) . Text.pack) networkNames) (Text.lines (runOut run)) `shouldBe` map (("    " <>) . Text.pack) networkNames

    it "give in JSON the descriptor of each account" $
      withHome $ \home -> do
        newTestPortfolio home
        (fmap (filter (\(_, net, _, _) -> net == "btc")) . descriptorsOf <$> hwJson home ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 0, "btc", Bip44, mainDescriptor), (PortfolioRef 0 0, "btc", Bip84, segwitDescriptor)]

    -- The bip84 account of the test networks has a vpub, its tpub with
    -- the version bytes of SLIP-132, computed apart from hw
    it "give in JSON the SLIP-132 key of each account: a tpub for bip44 and a vpub for bip84 on a test network" $
      withHome $ \home -> do
        newTestPortfolio home
        (fmap (filter (\(_, net, _, _) -> net == "btctest")) . slip132KeysOf <$> hwJson home ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 0, "btctest", Bip44, Just testAccountXPubTest), (PortfolioRef 0 0, "btctest", Bip84, Just testSegwitAccountVPub)]

    -- portfolio1 is at the account index 1: m/44'/0'/1'
    it "give each portfolio the descriptors of its own index" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        (fmap (filter (\(ref, net, kind, _) -> (ref, net, kind) == (PortfolioRef 0 1, "btc", Bip44))) . descriptorsOf <$> hwJson home ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 1, "btc", Bip44, "pkh([bc560f95/44h/0h/1h]" <> testAccountXPub1 <> "/<0;1>/*)#nycrn993")]

    -- 80000 spendable on btc, in 2 coins at 2 addresses. The first column
    -- marks the active portfolio.
    it "are listed with --tabular one row for each network a portfolio holds something on, under a header" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        run <- hw home ["portfolios", "--tabular"] []
        Text.lines (runOut run)
          `shouldBe` [ "  │ Wallet  │ Portfolio  │ Network │ Spendable (btc) │ Unconfirmed (btc) │ Coins │ Addresses │ Description",
                       "──┼─────────┼────────────┼─────────┼─────────────────┼───────────────────┼───────┼───────────┼────────────",
                       "* │ wallet0 │ portfolio0 │ btc     │      0.00080000 │        0.00000000 │     2 │         2 │"
                     ]

{- A mnemonic checked -}

checkMnemonicSpec :: Spec
checkMnemonicSpec =
  describe "A mnemonic checked by checkmnemonic" $ do
    it "matches the portfolio of its seed, which checkmnemonic says" $
      withHome $ \home -> do
        newTestPortfolio home
        (checkedOf <$> hwJson home ["checkmnemonic"] seedOnce)
          `shouldReturn` Right (True, "The mnemonic and passphrase matched portfolio0 of wallet0")

    it "does not match the portfolio with another passphrase" $
      withHome $ \home -> do
        newTestPortfolio home
        (checkedOf <$> hwJson home ["checkmnemonic"] [mnemonicWords testMnemonic, "wrong passphrase"])
          `shouldReturn` Right (False, "The mnemonic and passphrase did not match portfolio0 of wallet0")

    -- A script tells from the exit code, as for a signature verifymsg
    -- finds not valid
    it "makes hw exit with an error when it does not match" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["checkmnemonic"] [mnemonicWords testMnemonic, "wrong passphrase"]
        runExit run `shouldBe` ExitFailure 1

    it "makes hw exit with an error in JSON too when it does not match" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["checkmnemonic", "--json"] [mnemonicWords testMnemonic, "wrong passphrase"]
        runExit run `shouldBe` ExitFailure 1

    it "lets hw exit with success when it matches" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["checkmnemonic"] seedOnce
        runExit run `shouldBe` ExitSuccess

    -- wallet1 is of the second test mnemonic
    it "is checked against the active portfolio, of a wallet switch made active, whose seed is the one to type" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--wallet", "1", "--portfolio", "0"] []
        (checkedOf <$> hwJson home ["checkmnemonic"] (seedOnceOf testMnemonic2))
          `shouldReturn` Right (True, "The mnemonic and passphrase matched portfolio0 of wallet1")
        (checkedOf <$> hwJson home ["checkmnemonic"] seedOnce)
          `shouldReturn` Right (False, "The mnemonic and passphrase did not match portfolio0 of wallet1")

    it "is checked against the portfolio switch made active" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--portfolio", "1"] []
        (checkedOf <$> hwJson home ["checkmnemonic"] seedOnce)
          `shouldReturn` Right (True, "The mnemonic and passphrase matched portfolio1 of wallet0")

{- Descriptions -}

descriptionSpec :: Spec
descriptionSpec =
  describe "The description label sets, given alone" $ do
    it "is that of the active portfolio, which portfolios shows after its name and its wallet" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["label", "Savings"] []
        run <- hw home ["portfolios"] []
        run `shouldSatisfy` printed "* portfolio0  wallet0  \"Savings\"\n"
        run `shouldSatisfy` printed "  portfolio1  wallet0\n"

    it "is that of the portfolio switch made active" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--portfolio", "1"] []
        _ <- answer home ["label", "Savings"] []
        run <- hw home ["portfolios"] []
        run `shouldSatisfy` printed "* portfolio1  wallet0  \"Savings\"\n"
        run `shouldSatisfy` printed "  portfolio0  wallet0\n"

    -- wallet0 has two portfolios, wallet1 one
    it "is that of the wallet with --of-wallet, which portfolios shows under each of its portfolios" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["label", "Family", "--of-wallet"] []
        run <- hw home ["portfolios"] []
        filter (Text.isInfixOf "is \"Family\"") (Text.lines (runOut run)) `shouldBe` ["    wallet0 is \"Family\"", "    wallet0 is \"Family\""]

    it "is that of the wallet of the active portfolio with --of-wallet, after a switch to another wallet" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["switch", "--wallet", "1", "--portfolio", "0"] []
        _ <- answer home ["label", "Family", "--of-wallet"] []
        run <- hw home ["portfolios"] []
        filter (Text.isInfixOf "is \"Family\"") (Text.lines (runOut run)) `shouldBe` ["    wallet1 is \"Family\""]

    it "goes with exportwallet and importwallet, which counts the description of the wallet and of each portfolio among the labels set" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        newTestPortfolio signer
        _ <- answer signer ["label", "Family", "--of-wallet"] []
        _ <- answer signer ["label", "Spending"] []
        _ <- answer signer ["switch", "1"] []
        _ <- answer signer ["label", "Savings"] []
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        run <- hw online ["importwallet", signer </> "wallet.jsonl"] []
        filter (Text.isPrefixOf "Labels set") (Text.lines (runOut run)) `shouldBe` ["Labels set: 1 wallet, 2 portfolios"]

    it "is kept by importwallet against that of the file, which it says it left aside" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["label", "Savings"] []
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        newTestPortfolio online
        _ <- answer online ["label", "Rent"] []
        run <- hw online ["importwallet", signer </> "wallet.jsonl"] []
        filter (\line -> any (`Text.isPrefixOf` line) ["Labels set", "Left aside"]) (Text.lines (runOut run))
          `shouldBe` ["Labels set: none", "Left aside: 1 label of a portfolio that keeps its own"]
        listed <- hw online ["portfolios"] []
        take 1 (Text.lines (runOut listed)) `shouldBe` ["* portfolio0  wallet0  \"Rent\""]

    it "is shown at once in the answer, with the portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["label", "Savings"] []
        take 1 (Text.lines (runOut run)) `shouldBe` ["* portfolio0  wallet0  \"Savings\""]

    it "is removed by an empty description" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["label", "Savings"] []
        _ <- answer home ["label", ""] []
        run <- hw home ["portfolios"] []
        take 1 (Text.lines (runOut run)) `shouldBe` ["* portfolio0  wallet0"]

    it "is refused with a line break, and the one before is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["label", "Savings"] []
        (errorOf <$> hwJson home ["label", "two\nlines"] [])
          `shouldReturn` Right "The description must not contain a line break or a control character"
        run <- hw home ["portfolios"] []
        take 1 (Text.lines (runOut run)) `shouldBe` ["* portfolio0  wallet0  \"Savings\""]

    it "is refused with a space at either end" $
      withHome $ \home -> do
        newTestPortfolio home
        forM_ [" Savings", "Savings "] $ \description ->
          (errorOf <$> hwJson home ["label", description] [])
            `shouldReturn` Right "The description must not start or end with a space"

    -- The user most likely left out the label of the address
    it "is refused for a word alone that reads as an address, and the one before is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["label", "Savings"] []
        (errorOf <$> hwJson home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"] [])
          `shouldReturn` Right "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj reads as an address, not as a description. To label it, give the label after it: hw label 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj LABEL"
        run <- hw home ["portfolios"] []
        take 1 (Text.lines (runOut run)) `shouldBe` ["* portfolio0  wallet0  \"Savings\""]

    it "is refused for a word alone that reads as a path" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["label", "bip84/0/3"] [])
          `shouldReturn` Right "bip84/0/3 reads as a path, not as a description. To label it, give the label after it: hw label bip84/0/3 LABEL"

    it "is refused with --network, since it is of the whole portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["label", "Savings", "--network", "btc"] [])
          `shouldReturn` Right "label takes no --network with the label alone: a description is of the whole portfolio, or of the whole wallet"

    it "is refused with --type, since it is of the whole portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["label", "Savings", "--type", "bip84"] [])
          `shouldReturn` Right "label takes no --type with the label alone: a description is of the whole portfolio, or of the whole wallet"

{- Defaults -}

-- The home of a test shows every network, and its file says so: the
-- networks setdefault shows and hides are in
-- "Haskoin.Wallet.Workflow.HiddenNetworksSpec".
setDefaultSpec :: Ctx -> Spec
setDefaultSpec ctx =
  describe "The defaults setdefault writes" $ do
    let everyNetworkShown = ["btc", "bch", "btctest", "btctest4", "btcreg", "bchtest", "bchtest4", "bchreg"] :: [Text]
    it "are the two keys of the configuration file, each with its value" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["setdefault", "--network", "bch", "--type", "bip84"] []
        config <- configFileOf home
        config
          `shouldBe` Just
            ( Json.object
                [ "default-network" Json..= ("bch" :: Text),
                  "default-type" Json..= ("bip84" :: Text),
                  "host" Json..= ("http://127.0.0.1:1/" :: Text),
                  "shown-networks" Json..= everyNetworkShown
                ]
            )

    it "leave the other keys of the file as they were" $
      withHome $ \home -> do
        newTestPortfolio home
        setUnit home "sat"
        _ <- answer home ["setdefault", "--type", "bip84"] []
        config <- configFileOf home
        config
          `shouldBe` Just
            ( Json.object
                [ "default-network" Json..= ("btc" :: Text),
                  "default-type" Json..= ("bip84" :: Text),
                  "host" Json..= ("http://127.0.0.1:1/" :: Text),
                  "shown-networks" Json..= everyNetworkShown,
                  "unit" Json..= ("sat" :: Text)
                ]
            )

    it "are said in the answer, key by key, with the file they were written to" $
      withHome $ \home -> do
        threePortfolios home
        run <- hw home ["setdefault", "--network", "bch", "--type", "bip84"] []
        runOut run `shouldBe` "default-network: bch\ndefault-type   : bip84\nWritten to " <> Text.pack (home </> ".hw" </> "config.json") <> "\n"

    it "make the commands work where they say: receive gives an address of the default type set" $
      withHome $ \home -> do
        threePortfolios home
        _ <- answer home ["setdefault", "--type", "bip84"] []
        receiveWith home [] `shouldReturn` Text.unpack (segwitExternalAddrsText !! 0)

    -- The file of a test home names the fake store, the network btc, and
    -- every network as shown
    it "include the unit of --unit, written to the key unit" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["setdefault", "--unit", "sat"] []
        config <- configFileOf home
        config
          `shouldBe` Just
            ( Json.object
                [ "default-network" Json..= ("btc" :: Text),
                  "host" Json..= ("http://127.0.0.1:1/" :: Text),
                  "shown-networks" Json..= everyNetworkShown,
                  "unit" Json..= ("sat" :: Text)
                ]
            )

    -- --unit goes before or after any command
    it "include the unit of --unit given before the command" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["--unit", "sat", "setdefault"] []
        config <- configFileOf home
        config
          `shouldBe` Just
            ( Json.object
                [ "default-network" Json..= ("btc" :: Text),
                  "host" Json..= ("http://127.0.0.1:1/" :: Text),
                  "shown-networks" Json..= everyNetworkShown,
                  "unit" Json..= ("sat" :: Text)
                ]
            )

    -- --unit takes bits for bit, as an amount does, and the help names
    -- the unit bit
    it "write a unit by the name the help gives it, whatever name was typed" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["setdefault", "--unit", "bits"] []
        config <- configFileOf home
        config
          `shouldBe` Just
            ( Json.object
                [ "default-network" Json..= ("btc" :: Text),
                  "host" Json..= ("http://127.0.0.1:1/" :: Text),
                  "shown-networks" Json..= everyNetworkShown,
                  "unit" Json..= ("bit" :: Text)
                ]
            )

    -- The portfolio has 50000 satoshis on btc
    it "make the amounts shown in the unit set" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["setdefault", "--unit", "sat"] []
        (printed "50000 sats" <$> hw home ["portfolios"] []) `shouldReturn` True

    it "include the view each of --compact, --verbose and --tabular names, written to the key listing-view" $
      withHome $ \home -> do
        newTestPortfolio home
        forM_ [("--compact", "compact"), ("--verbose", "verbose"), ("--tabular", "tabular" :: Text)] $ \(option, view) -> do
          _ <- answer home ["setdefault", option] []
          config <- configFileOf home
          config
            `shouldBe` Just
              ( Json.object
                  [ "default-network" Json..= ("btc" :: Text),
                    "host" Json..= ("http://127.0.0.1:1/" :: Text),
                    "listing-view" Json..= view,
                    "shown-networks" Json..= everyNetworkShown
                  ]
              )

    -- The table of portfolios starts with its headings, which no other
    -- view has
    it "make the listings show the view set" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["setdefault", "--tabular"] []
        listed <- Text.lines . runOut <$> hw home ["portfolios"] []
        take 1 listed `shouldBe` ["  │ Wallet  │ Portfolio  │ Network │ Spendable (btc) │ Unconfirmed (btc) │ Coins │ Addresses │ Description"]

    -- The keys are padded to the longest, listing-view, of 12 letters
    it "say the unit and the view in the answer, key by key" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["setdefault", "--unit", "sat", "--tabular"] []
        runOut run `shouldBe` "unit        : sat\nlisting-view: tabular\nWritten to " <> Text.pack (home </> ".hw" </> "config.json") <> "\n"

    it "are refused when no option names one" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["setdefault"] []) `shouldReturn` Right "Give the default to set: --network, --type, --unit, --compact, --verbose, --tabular, --show-network or --hide-network"

    -- The active portfolio is in the database, and switch changes it
    it "are refused --wallet and --portfolio, which switch replaces, and the file is left as it was" $
      withHome $ \home -> do
        threePortfolios home
        earlier <- configFileOf home
        forM_ ["--wallet", "--portfolio"] $ \option -> do
          run <- hw home ["setdefault", option, "1"] []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf ("Invalid option `" <> Text.pack option <> "'")
        configFileOf home `shouldReturn` earlier

{- Deleting a portfolio -}

deletePortfolioSpec :: Ctx -> Spec
deletePortfolioSpec ctx =
  describe "A portfolio deleted by deleteportfolio" $ do
    it "is the highest portfolio of its wallet, which portfolios then leaves out" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        (deletedOf <$> hwJson home ["deleteportfolio", "--portfolio", "1"] []) `shouldReturn` Right "portfolio1 of wallet0 has been deleted"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]

    it "is said to be deleted in one line" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        run <- hw home ["deleteportfolio", "--portfolio", "1"] []
        runOut run `shouldBe` "portfolio1 of wallet0 has been deleted\n"

    -- wallet1 has portfolio0 and portfolio1 of the second test mnemonic
    it "is one of the wallet of the active portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        newPortfolioOf home testMnemonic2
        _ <- answer home ["switch", "--wallet", "1", "--portfolio", "0"] []
        (deletedOf <$> hwJson home ["deleteportfolio", "--portfolio", "1"] []) `shouldReturn` Right "portfolio1 of wallet1 has been deleted"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 1 0]

    -- wallet0 has portfolio0 alone, and wallet1 has a portfolio1
    it "is looked for in the wallet of the active portfolio alone, and one of another wallet is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        newPortfolioOf home testMnemonic2
        (errorOf <$> hwJson home ["deleteportfolio", "--portfolio", "1"] [])
          `shouldReturn` Right "wallet0 has no portfolio1: portfolios lists the portfolios"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 1 0, PortfolioRef 1 1]

    -- The index is free again, and the keys at it are the same
    it "leaves its index to the next newportfolio --existing, which makes it again with the same keys" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        _ <- answer home ["deleteportfolio", "--portfolio", "1"] []
        again <- answer home ["newportfolio", "--existing"] seedTwice
        (filter (\(_, net, kind, _) -> (net, kind) == ("btc", Bip44)) <$> accountXPubsOf again)
          `shouldBe` Right [(PortfolioRef 0 1, "btc", Bip44, testAccountXPub1)]

    it "is never portfolio0, which a wallet always keeps" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["deleteportfolio", "--portfolio", "0"] [])
          `shouldReturn` Right "portfolio0 is never deleted: a wallet always keeps it, and a new one would be the same portfolio, with the same keys. deletewallet deletes a whole wallet"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]

    it "is never one below the highest index of its wallet, so that no gap is left behind" $
      withHome $ \home -> do
        replicateM_ 3 (newTestPortfolio home)
        (errorOf <$> hwJson home ["deleteportfolio", "--portfolio", "1"] [])
          `shouldReturn` Right "portfolio1 of wallet0 is not the last portfolio of its wallet: portfolio2 goes first"

    -- Which portfolio the user works in next is theirs to say
    it "is never the active portfolio, and the refusal says to switch to another one first" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        _ <- answer home ["switch", "--portfolio", "1"] []
        (errorOf <$> hwJson home ["deleteportfolio", "--portfolio", "1"] [])
          `shouldReturn` Right "portfolio1 of wallet0 is the active portfolio: switch to another portfolio first"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1]

    -- Nobody paid the address, which someone may pay later. receive gave
    -- it out in portfolio1, active then.
    it "is never one that gave out an address with receive" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        _ <- answer home ["receive"] []
        switchTo home 0 0
        (errorOf <$> hwJson home ["deleteportfolio", "--portfolio", "1"] [])
          `shouldReturn` Right "portfolio1 of wallet0 has had activity or gave out an address, so it is kept: a payment to it would go unseen, and recover would find it again. deletewallet deletes a whole wallet"

    -- The chain pays the first receiving address of portfolio1,
    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, which the sync finds
    it "is never one that was paid" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["deleteportfolio", "--portfolio", "1"] [])
          `shouldReturn` Right "portfolio1 of wallet0 has had activity or gave out an address, so it is kept: a payment to it would go unseen, and recover would find it again. deletewallet deletes a whole wallet"
        (spendableOn btc <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, 0), (PortfolioRef 0 1, 50000)]

    it "writes nothing to the configuration file" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        earlier <- configFileOf home
        _ <- answer home ["deleteportfolio", "--portfolio", "1"] []
        configFileOf home `shouldReturn` earlier

    -- --json is an option of every command, which is not the one missing
    it "must be named by --portfolio, which deleteportfolio never takes from the active portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        run <- hw home ["deleteportfolio", "--json"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Missing: (-p|--portfolio INT)"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1]

{- Deleting a wallet -}

-- | The answer of deletewallet when it deletes the last wallet of the
-- database
lastWalletDeleted :: Text
lastWalletDeleted = "wallet0 has been deleted, with all of its portfolios. The database has no wallet left: the first portfolio created next becomes the active one."

deleteWalletSpec :: Ctx -> Spec
deleteWalletSpec ctx =
  describe "A wallet deleted by deletewallet" $ do
    -- 4435aa1c is the fingerprint of the second test mnemonic
    it "is the one --wallet names, given its fingerprint, with all of its portfolios" $
      withHome $ \home -> do
        threePortfolios home
        newPortfolioOf home testMnemonic2
        (deletedOf <$> hwJson home ["deletewallet", "4435aa1c", "--wallet", "1"] [])
          `shouldReturn` Right "wallet1 has been deleted, with all of its portfolios"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1]

    it "is refused a fingerprint that is not that of the wallet, and nothing is deleted" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["deletewallet", "bc560f95", "--wallet", "1"] [])
          `shouldReturn` Right "The fingerprint given is not that of wallet1: portfolios --verbose shows it. Nothing was deleted"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 1 0]

    -- Every wallet has its fingerprint: no word stands for one hw does not
    -- know
    it "is refused the word unknown in place of its fingerprint, and nothing is deleted" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["deletewallet", "unknown", "--wallet", "0"] [])
          `shouldReturn` Right "The fingerprint given is not that of wallet0: portfolios --verbose shows it. Nothing was deleted"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]

    it "must be named by --wallet, which deletewallet never takes from the active portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["deletewallet", "bc560f95"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Missing: --wallet INT"

    -- The funds stay on the chain, and the mnemonic can still spend them.
    -- The wallet is the last one, that of the active portfolio.
    it "is deleted though it was paid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (deletedOf <$> hwJson home ["deletewallet", "bc560f95", "--wallet", "0"] []) `shouldReturn` Right lastWalletDeleted
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    -- Which portfolio the user works in next is theirs to say
    it "is refused the wallet of the active portfolio while another wallet exists, and says to switch to a portfolio of another wallet first" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["deletewallet", "bc560f95", "--wallet", "0"] [])
          `shouldReturn` Right "wallet0 holds the active portfolio, portfolio0 of wallet0: switch to a portfolio of another wallet first"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 1 0]

    it "deletes the wallet that held the active portfolio once a portfolio of another wallet is active" $
      withHome $ \home -> do
        threePortfolios home
        switchTo home 1 0
        (deletedOf <$> hwJson home ["deletewallet", "bc560f95", "--wallet", "0"] [])
          `shouldReturn` Right "wallet0 has been deleted, with all of its portfolios"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 1 0]

    it "deletes the last wallet, the one of the active portfolio, and says that the first portfolio created next becomes active" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        run <- hw home ["deletewallet", "bc560f95", "--wallet", "0"] []
        runOut run `shouldBe` lastWalletDeleted <> "\n"

    it "leaves no active portfolio once the last wallet is deleted, which switch says with how to make a wallet" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["deletewallet", "bc560f95", "--wallet", "0"] []
        (errorOf <$> hwJson home ["switch", "--portfolio", "0"] [])
          `shouldReturn` Right "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"

    it "leaves the names of the other wallets: wallet1 is still wallet1" $
      withHome $ \home -> do
        threePortfolios home
        switchTo home 1 0
        _ <- answer home ["deletewallet", "bc560f95", "--wallet", "0"] []
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 1 0]

    -- portfolio0 of wallet1 is active, so the new wallet0 is not
    it "gives the next new wallet the name wallet0 again, which does not become active" $
      withHome $ \home -> do
        threePortfolios home
        switchTo home 1 0
        _ <- answer home ["deletewallet", "bc560f95", "--wallet", "0"] []
        (portfolioRefsOf <$> answer home ["newportfolio", "--existing"] seedTwice) `shouldReturn` Right [PortfolioRef 0 0]
        receiveWith home [] `shouldReturn` Text.unpack externalAddr2Text

    it "writes nothing to the configuration file" $
      withHome $ \home -> do
        threePortfolios home
        switchTo home 1 0
        earlier <- configFileOf home
        _ <- answer home ["deletewallet", "bc560f95", "--wallet", "0"] []
        configFileOf home `shouldReturn` earlier

{- Wallet files -}

walletFileSpec :: Ctx -> Spec
walletFileSpec ctx =
  describe "A wallet copied with exportwallet and importwallet" $ do
    it "is written to the file exportwallet names, which exportwallet says" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["exportwallet", home </> "wallet.jsonl"] []
        runOut run `shouldBe` "Written to " <> Text.pack (home </> "wallet.jsonl") <> "\n"

    -- As an offline signer hands its portfolios to an online computer
    it "is imported on another computer by importwallet, with the fingerprint of its seed" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        _ <- answer online ["importwallet", signer </> "wallet.jsonl"] []
        (fingerprintsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, testFingerprintText)]

    it "is imported whole, the twelve accounts of each portfolio with their keys" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        (portfoliosImportedOf <$> hwJson online ["importwallet", signer </> "wallet.jsonl"] []) `shouldReturn` Right [(0, 0, True)]
        (fmap (map (\(_, net, kind, key) -> (net, kind, key))) . accountXPubsOf <$> hwJson online ["portfolios"] [])
          `shouldReturn` Right portfolio0Keys

    it "hands out on the online computer the addresses of the signer" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        _ <- answer online ["importwallet", signer </> "wallet.jsonl"] []
        receiveWith online [] `shouldReturn` Text.unpack (externalAddrsText !! 0)
        receiveWith online ["--type", "bip84"] `shouldReturn` Text.unpack (segwitExternalAddrsText !! 0)

    it "is written with every portfolio of the wallet without --portfolio" $
      withHome $ \signer -> withHome $ \online -> do
        file <- twoPortfoliosFile signer
        _ <- answer online ["importwallet", file] []
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1]

    -- The online computer has portfolio0 of the wallet already
    it "is written with the one portfolio exportwallet --portfolio names, and imported into the wallet the database has" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", "--portfolio", "1", signer </> "portfolio1.jsonl"] []
        newTestPortfolio online
        (portfoliosImportedOf <$> hwJson online ["importwallet", signer </> "portfolio1.jsonl"] [])
          `shouldReturn` Right [(0, 1, True)]

    -- The online computer has portfolio0 of the wallet already
    it "is read one portfolio at a time with importwallet --portfolio" $
      withHome $ \signer -> withHome $ \online -> do
        file <- twoPortfoliosFile signer
        newTestPortfolio online
        (portfoliosImportedOf <$> hwJson online ["importwallet", "--portfolio", "1", file] [])
          `shouldReturn` Right [(0, 1, True)]

    -- A wallet is created with its portfolio0
    it "is refused a file without the portfolio0 of a wallet the database does not have, and nothing is created" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", "--portfolio", "1", signer </> "portfolio1.jsonl"] []
        (errorOf <$> hwJson online ["importwallet", signer </> "portfolio1.jsonl"] [])
          `shouldReturn` Right "The file does not have portfolio0 of the wallet bc560f95, which the database does not have: a wallet is created with its portfolio0. Import a file of the whole wallet, as exportwallet writes it without --portfolio. Nothing was changed"
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right []

    it "is refused importwallet --portfolio 1 of a wallet the database does not have, and nothing is created" $
      withHome $ \signer -> withHome $ \online -> do
        file <- twoPortfoliosFile signer
        (errorOf <$> hwJson online ["importwallet", "--portfolio", "1", file] [])
          `shouldReturn` Right "The database has no wallet of the fingerprint bc560f95, and importwallet --portfolio 1 does not create one: a wallet is created with its portfolio0. Import the whole file, without --portfolio. Nothing was changed"
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right []

    -- The file gives the 8 accounts of portfolio1 that are not bip84: the 12
    -- but the 4 bip84 ones, of btc, btctest, btctest4 and btcreg
    it "is refused when it would create a portfolio without all twelve of its accounts, and nothing is created" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", "--portfolio", "1", signer </> "portfolio1.jsonl"] []
        part <- fileWithout signer (signer </> "portfolio1.jsonl") "\"purpose\":84"
        newTestPortfolio online
        (errorOf <$> hwJson online ["importwallet", part] [])
          `shouldReturn` Right "The file gives 8 of the 12 accounts of portfolio1 of wallet0, which the database does not have: importwallet creates a portfolio only whole, with every account. Write the file with exportwallet on the computer that has the whole portfolio. Nothing was changed"
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]

    -- The 8 accounts of portfolio0 that are not bip84, as above
    it "is refused the portfolio0 of a new wallet without all twelve of its accounts, and no wallet is created" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        part <- fileWithout signer (signer </> "wallet.jsonl") "\"purpose\":84"
        (errorOf <$> hwJson online ["importwallet", part] [])
          `shouldReturn` Right "The file gives 8 of the 12 accounts of portfolio0 of the wallet bc560f95, which the database does not have: importwallet creates a portfolio only whole, with every account. Write the file with exportwallet on the computer that has the whole portfolio. Nothing was changed"
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right []

    -- The database has portfolio0 whole: the file need not give every
    -- account of it
    it "takes a file that gives some of the accounts of a portfolio the database has, which gets nothing" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        part <- fileWithout signer (signer </> "wallet.jsonl") "\"purpose\":84"
        newTestPortfolio online
        (portfoliosImportedOf <$> hwJson online ["importwallet", part] []) `shouldReturn` Right [(0, 0, False)]

    it "is refused importwallet --portfolio of a portfolio the file has nothing of, naming the portfolios it has" $
      withHome $ \signer -> withHome $ \online -> do
        file <- twoPortfoliosFile signer
        (errorOf <$> hwJson online ["importwallet", "--portfolio", "5", file] [])
          `shouldReturn` Right "The file has no portfolio5: it has portfolio0 and portfolio1. Nothing was changed"
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right []

    it "never makes a second portfolio with an index the wallet has, when imported again" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        _ <- answer online ["importwallet", signer </> "wallet.jsonl"] []
        (portfoliosImportedOf <$> hwJson online ["importwallet", signer </> "wallet.jsonl"] []) `shouldReturn` Right [(0, 0, False)]
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]

    it "adds no account to a portfolio the database has, which keeps the twelve keys it had" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        newTestPortfolio online
        (addedAccountsOf <$> hwJson online ["importwallet", signer </> "wallet.jsonl"] []) `shouldReturn` Right []
        (fmap (map (\(_, net, kind, key) -> (net, kind, key))) . accountXPubsOf <$> hwJson online ["portfolios"] [])
          `shouldReturn` Right portfolio0Keys

    -- The key of the second test mnemonic at m/44'/0'/0', in place of the
    -- one of the test mnemonic
    it "is refused when an account of the file has another key than the same account of the database" $
      withHome $ \signer -> withHome $ \online -> do
        file <- fileWithKeyReplaced signer testAccountXPub testAccountXPub2
        newTestPortfolio online
        (errorOf <$> hwJson online ["importwallet", file] [])
          `shouldReturn` Right "The bip44 account of portfolio0 of wallet0 on btc has another key than the file gives it: the file and the database are not of one mnemonic. Nothing was changed"

    -- The file of portfolio1, whose bip44 account on btc has the key of
    -- portfolio0 there, written with the child number of the account index
    -- 1: the same key, since its public key and its chain code are the same
    it "is refused when it gives a key another portfolio holds, though under another child index" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", "--portfolio", "1", signer </> "portfolio1.jsonl"] []
        let key = testAccountPub ctx
            renumbered = xPubExport btc ctx (XPubKey key.depth key.parent 0x80000001 key.chain key.key)
        content <- Text.pack <$> readFile (signer </> "portfolio1.jsonl")
        writeFile (signer </> "renumbered.jsonl") (Text.unpack (Text.replace testAccountXPub1 renumbered content))
        newTestPortfolio online
        (errorOf <$> hwJson online ["importwallet", signer </> "renumbered.jsonl"] [])
          `shouldReturn` Right "portfolio0 of wallet0 already has this key on btc. Nothing was changed"
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]

    -- The tpub of the bip44 account 0 of the backup in shares on btctest4,
    -- where btctest has the one of the test wallet: the test networks
    -- share the path m/44'/1'/0', so its accounts hold one key
    it "is refused when it gives the accounts of one path on two test networks different keys, and nothing is created" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        records <- Text.lines . Text.pack <$> readFile (signer </> "wallet.jsonl")
        let twin line
              | "\"network\":\"btctest4\",\"purpose\":44" `Text.isInfixOf` line = Text.replace testAccountXPubTest testSharesTestnetXPub line
              | otherwise = line
        writeFile (signer </> "twins.jsonl") (Text.unpack (Text.unlines (twin <$> records)))
        (errorOf <$> hwJson online ["importwallet", signer </> "twins.jsonl"] [])
          `shouldReturn` Right "The bip44 account /44'/1'/0' of portfolio0 of wallet0 has one key on btctest and would get another on btctest4: the networks of one path share its key. Nothing was changed"
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right []

    -- A key alone is one account, of no portfolio
    it "is refused from a file that holds a key alone and no record" $
      withHome $ \home -> do
        writeFile (home </> "key.txt") (Text.unpack testAccountXPub <> "\n")
        (errorOf <$> hwJson home ["importwallet", home </> "key.txt"] [])
          `shouldReturn` Right ("The file " <> Text.pack (home </> "key.txt") <> " could not be read: It holds no record of a wallet. importwallet reads the file exportwallet writes. A key or a descriptor alone is one account, and does not say which portfolio it belongs to")

    it "is refused from a file that holds an output descriptor alone" $
      withHome $ \home -> do
        writeFile (home </> "descriptor.txt") (Text.unpack mainDescriptor <> "\n")
        (errorOf <$> hwJson home ["importwallet", home </> "descriptor.txt"] [])
          `shouldReturn` Right ("The file " <> Text.pack (home </> "descriptor.txt") <> " could not be read: It holds no record of a wallet. importwallet reads the file exportwallet writes. A key or a descriptor alone is one account, and does not say which portfolio it belongs to")

    it "is not written over a file that exists" $
      withHome $ \home -> do
        newTestPortfolio home
        writeFile (home </> "wallet.jsonl") "kept\n"
        (errorOf <$> hwJson home ["exportwallet", home </> "wallet.jsonl"] []) `shouldReturn` Right (alreadyExists (home </> "wallet.jsonl"))
        readFile (home </> "wallet.jsonl") `shouldReturn` "kept\n"

{- Files of several wallets -}

-- The signer has wallet0 of the test mnemonic, of the fingerprint
-- bc560f95, with portfolio0 and portfolio1. It has wallet1 of the second
-- test mnemonic, 4435aa1c, with portfolio0. The online computer creates
-- what a file brings, so its portfolios tell what the file holds.
severalWalletsSpec :: Ctx -> Spec
severalWalletsSpec ctx = do
  describe "The file exportwallet writes of a database of several wallets" $ do
    -- The active portfolio is of wallet1, and wallet0 is written too
    it "holds every portfolio of every wallet without options, whichever portfolio is active" $
      withHome $ \signer -> withHome $ \online -> do
        threePortfolios signer
        switchTo signer 1 0
        _ <- answer signer ["exportwallet", signer </> "all.jsonl"] []
        _ <- answer online ["importwallet", signer </> "all.jsonl"] []
        (fingerprintsOf <$> hwJson online ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 0, "bc560f95"), (PortfolioRef 0 1, "bc560f95"), (PortfolioRef 1 0, "4435aa1c")]

    -- The active portfolio is of wallet1, and --wallet names wallet0
    it "holds every portfolio of the one wallet --wallet names, and nothing of the others" $
      withHome $ \signer -> withHome $ \online -> do
        threePortfolios signer
        switchTo signer 1 0
        _ <- answer signer ["exportwallet", "--wallet", "0", signer </> "wallet0.jsonl"] []
        _ <- answer online ["importwallet", signer </> "wallet0.jsonl"] []
        (fingerprintsOf <$> hwJson online ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 0, "bc560f95"), (PortfolioRef 0 1, "bc560f95")]

    -- The online computer has portfolio0 of the test wallet already, and
    -- the file names portfolio1 of it alone
    it "holds the one portfolio --wallet and --portfolio name together" $
      withHome $ \signer -> withHome $ \online -> do
        threePortfolios signer
        switchTo signer 1 0
        _ <- answer signer ["exportwallet", "--wallet", "0", "--portfolio", "1", signer </> "portfolio1.jsonl"] []
        newTestPortfolio online
        (portfoliosImportedOf <$> hwJson online ["importwallet", signer </> "portfolio1.jsonl"] [])
          `shouldReturn` Right [(0, 1, True)]

    it "is refused for a --wallet the database does not have, naming where the wallets are listed" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["exportwallet", "--wallet", "3", home </> "wallet.jsonl"] [])
          `shouldReturn` Right "There is no wallet3: portfolios lists the wallets"

    it "is refused for a --portfolio the wallet --wallet names does not have" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["exportwallet", "--wallet", "1", "--portfolio", "1", home </> "wallet.jsonl"] [])
          `shouldReturn` Right "wallet1 has no portfolio1: portfolios lists the portfolios"

    it "is refused for a --portfolio the wallet of the active portfolio does not have, without --wallet" $
      withHome $ \home -> do
        threePortfolios home
        (errorOf <$> hwJson home ["exportwallet", "--portfolio", "7", home </> "wallet.jsonl"] [])
          `shouldReturn` Right "wallet0 has no portfolio7: portfolios lists the portfolios"

    -- The file would hold nothing
    it "is refused in a database without a wallet, which is told how to make one" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["exportwallet", home </> "wallet.jsonl"] [])
          `shouldReturn` Right "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"

  describe "A file of several wallets read by importwallet" $ do
    -- Both wallets of the file have a portfolio0
    it "is refused with --portfolio alone, which would name a portfolio of each wallet, and nothing is created" $
      withHome $ \signer -> withHome $ \online -> do
        file <- threePortfoliosFile signer
        (errorOf <$> hwJson online ["importwallet", file, "--portfolio", "0"] [])
          `shouldReturn` Right "The file holds the wallets bc560f95 and 4435aa1c, and --portfolio alone does not say which of them portfolio0 is of. Name the wallet by its fingerprint too, as in --wallet bc560f95 --portfolio 0. Nothing was changed"
        (portfolioRefsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right []

    -- wallet1 of the signer is the first wallet of the online computer
    it "gives the one wallet --wallet names by its fingerprint, and nothing of the others" $
      withHome $ \signer -> withHome $ \online -> do
        file <- threePortfoliosFile signer
        _ <- answer online ["importwallet", file, "--wallet", "4435aa1c"] []
        (fingerprintsOf <$> hwJson online ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, "4435aa1c")]

    it "gives every portfolio of the wallet --wallet names" $
      withHome $ \signer -> withHome $ \online -> do
        file <- threePortfoliosFile signer
        _ <- answer online ["importwallet", file, "--wallet", "bc560f95"] []
        (fingerprintsOf <$> hwJson online ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 0, "bc560f95"), (PortfolioRef 0 1, "bc560f95")]

    -- The online computer has portfolio0 of the test wallet already
    it "gives the one portfolio --wallet and --portfolio name together" $
      withHome $ \signer -> withHome $ \online -> do
        file <- threePortfoliosFile signer
        newTestPortfolio online
        (portfoliosImportedOf <$> hwJson online ["importwallet", file, "--wallet", "bc560f95", "--portfolio", "1"] [])
          `shouldReturn` Right [(0, 1, True)]

    it "is refused a --wallet it has nothing of, naming the wallets it has" $
      withHome $ \signer -> withHome $ \online -> do
        file <- threePortfoliosFile signer
        (errorOf <$> hwJson online ["importwallet", file, "--wallet", "12345678"] [])
          `shouldReturn` Right "The file has no wallet 12345678: it has the wallets bc560f95 and 4435aa1c. Nothing was changed"

    -- The other commands name a wallet by its index, which the file does
    -- not hold
    it "is refused a --wallet that is an index, since a file names a wallet by its fingerprint" $
      withHome $ \signer -> withHome $ \online -> do
        file <- threePortfoliosFile signer
        run <- hw online ["importwallet", file, "--wallet", "0", "--portfolio", "1"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printedSentence "The --wallet value 0 is not a fingerprint. A file names each wallet by its fingerprint, 8 hexadecimal digits such as bc560f95, and not by an index: show lists the wallets of a file"

    -- The wallet of bc560f95 was wallet0 where the file was written, and
    -- is wallet1 of the database now
    it "finds the wallet --wallet names by its fingerprint, whatever index the database gives it" $
      withChain ctx (bothWalletsPaid ctx) $ \home _ -> do
        file <- walletsRecoveredInReverse home
        (walletsImportedOf <$> hwJson home ["importwallet", file, "--wallet", "bc560f95"] []) `shouldReturn` Right [(1, False)]

    -- The file names the wallet of bc560f95 first, which is wallet1 of the
    -- database now. Then it names the wallet of 4435aa1c, which is wallet0.
    it "finds each wallet by the fingerprint of its mnemonic, whatever index the database gives it, and creates none" $
      withChain ctx (bothWalletsPaid ctx) $ \home _ -> do
        file <- walletsRecoveredInReverse home
        (walletsImportedOf <$> hwJson home ["importwallet", file] []) `shouldReturn` Right [(1, False), (0, False)]

    -- "Business" described wallet1, of 4435aa1c, which is wallet0 now
    it "gives each wallet its own description when the database numbers the wallets in another order" $
      withChain ctx (bothWalletsPaid ctx) $ \home _ -> do
        file <- walletsRecoveredInReverse home
        _ <- answer home ["importwallet", file] []
        (walletDescriptionsOf <$> hwJson home ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 0, "Business"), (PortfolioRef 1 0, "Family")]

    -- 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2 is of the second test mnemonic,
    -- wallet0 now, and 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj of the test
    -- mnemonic, wallet1 now
    it "labels each address in the wallet of its mnemonic when the database numbers the wallets in another order" $
      withChain ctx (bothWalletsPaid ctx) $ \home _ -> do
        file <- walletsRecoveredInReverse home
        _ <- answer home ["importwallet", file] []
        switchTo home 0 0
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [("1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2", "Invoices")]
        switchTo home 1 0
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Rent")]

  describe "A file of several wallets shown by show" $ do
    it "is refused with --portfolio alone, as importwallet refuses it" $
      withHome $ \signer -> withHome $ \online -> do
        file <- threePortfoliosFile signer
        (errorOf <$> hwJson online ["show", file, "--portfolio", "0"] [])
          `shouldReturn` Right "The file holds the wallets bc560f95 and 4435aa1c, and --portfolio alone does not say which of them portfolio0 is of. Name the wallet by its fingerprint too, as in --wallet bc560f95 --portfolio 0"

    -- The online computer has no wallet. The wallet of 4435aa1c would be
    -- its first one.
    it "is shown one wallet at a time with --wallet, as importwallet --wallet would leave it" $
      withHome $ \signer -> withHome $ \online -> do
        file <- threePortfoliosFile signer
        run <- hw online ["show", file, "--wallet", "4435aa1c"] []
        runOut run
          `shouldBe` "wallet0  4435aa1c  not in the database, and importwallet creates it\n\
                     \  portfolio0  importwallet creates it, 12 accounts to add\n"

{- Databases of other versions of hw -}

-- | A command of each kind, with what the user types for it: a listing,
-- one that writes to the database, one that asks for a mnemonic, and two
-- that ask the store
commandsOfEachKind :: [([String], [Text])]
commandsOfEachKind =
  [ (["portfolios"], []),
    (["receive"], []),
    (["newportfolio", "--existing"], seedTwice),
    (["recover"], seedOnce),
    (["sync"], [])
  ]

-- | The refusal of a database of hw 0.9, at format version 2, in a home,
-- as hw prints it: what to do, with commands that name the file where it
-- is
version2Refusal :: FilePath -> Text
version2Refusal home =
  "The database "
    <> file
    <> " was made by an earlier version of hw, at format version 2.\n\
       \hw 0.10.0 does not read it, and has left it as it is. To go on, move it aside\n\
       \and create the wallet again from its mnemonic:\n\
       \\n\
       \  mv "
    <> file
    <> " "
    <> Text.pack (home </> ".hw" </> "accounts-old.sqlite")
    <> "\n\
       \  hw recover\n\
       \\n\
       \recover finds the portfolios of the mnemonic that were paid. On an offline\n\
       \computer, run hw newportfolio --existing instead, once for each account index\n\
       \the old wallet used, from 0 up to the highest. Then carry the portfolios to the\n\
       \online computer with exportwallet and importwallet. The labels of the old\n\
       \database are not carried over.\n"
  where
    file = Text.pack (databaseOf home)

otherVersionSpec :: Spec
otherVersionSpec = do
  describe "A database of an earlier version of hw" $ do
    it "is refused by every command, with the instructions to create the wallet again, which name its file where it is" $
      withHome $ \home -> do
        inDatabaseFile (databaseOf home) version2Database
        forM_ commandsOfEachKind $ \(args, typed) ->
          (runErr <$> hw home args typed) `shouldReturn` version2Refusal home

    it "makes every command exit with 1, and print nothing on its output" $
      withHome $ \home -> do
        inDatabaseFile (databaseOf home) version2Database
        forM_ commandsOfEachKind $ \(args, typed) -> do
          run <- hw home args typed
          runExit run `shouldBe` ExitFailure 1
          runOut run `shouldBe` ""

    -- 1 is hw 0.8, which had no version table, 2 hw 0.9, 3 and 4 unreleased
    -- builds of hw 0.10
    it "is refused at each of the versions 1 to 4, which the refusal names" $
      forM_ olderDatabases $ \(version, _, database) -> withHome $ \home -> do
        inDatabaseFile (databaseOf home) database
        run <- hw home ["portfolios"] []
        take 1 (Text.lines (runErr run))
          `shouldBe` ["The database " <> Text.pack (databaseOf home) <> " was made by an earlier version of hw, at format version " <> Text.pack version <> "."]

    it "is left byte for byte as it was, with no copy of it and no other file beside it" $
      withHome $ \home -> do
        inDatabaseFile (databaseOf home) version2Database
        stored <- BS.readFile (databaseOf home)
        forM_ commandsOfEachKind $ \(args, typed) -> hw home args typed
        BS.readFile (databaseOf home) `shouldReturn` stored
        sort <$> listDirectory (home </> ".hw") `shouldReturn` ["accounts.sqlite", "config.json"]

    it "is refused before hw asks for a mnemonic" $
      withHome $ \home -> do
        inDatabaseFile (databaseOf home) version2Database
        forM_ [(["newportfolio", "--existing"], seedTwice), (["checkmnemonic"], seedOnce)] $ \(args, typed) -> do
          run <- hw home args typed
          run `shouldNotSatisfy` askedMnemonic

  describe "A database of a newer version of hw, at version 6" $ do
    it "is refused by every command, which says it is newer than this hw and exits with 1" $
      withHome $ \home -> do
        inDatabaseFile (databaseOf home) version6Database
        forM_ commandsOfEachKind $ \(args, typed) -> do
          run <- hw home args typed
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldBe` "The database is at version 6, newer than the version 5 of this hw\n"

    it "is left byte for byte as it was, with no copy of it and no other file beside it" $
      withHome $ \home -> do
        inDatabaseFile (databaseOf home) version6Database
        stored <- BS.readFile (databaseOf home)
        forM_ commandsOfEachKind $ \(args, typed) -> hw home args typed
        BS.readFile (databaseOf home) `shouldReturn` stored
        sort <$> listDirectory (home </> ".hw") `shouldReturn` ["accounts.sqlite", "config.json"]

{- The bip84 account of a portfolio -}

-- | The signature of the message "hello" by the address /0/0 of the bip84
-- account of the test wallet, as signmsg gives it, with the header byte of
-- BIP 137: checked apart from hw with the pure-Python verifier of the
-- message-signing vectors, which recovers the key of the address from it
segwitHelloSignature :: Text
segwitHelloSignature = "KPTsP4VeuwtqOgTJeMywhK80W9RJdrpsKebwu2/58WfqejQZQh54meETAcgDFYNrOSFmv3aAQ+V4BN0XOydeLEI="

segwitSpec :: Ctx -> Spec
segwitSpec ctx =
  describe "The bip84 account of a portfolio, of native SegWit" $ do
    it "hands out with receive --type bip84 addresses that start with bc1q, which show derives at their paths too" $
      withHome $ \home -> do
        newTestPortfolio home
        receiveWith home ["--type", "bip84"] `shouldReturn` Text.unpack (segwitExternalAddrsText !! 0)
        receiveWith home ["--type", "bip84"] `shouldReturn` Text.unpack (segwitExternalAddrsText !! 1)
        -- The ends of bc1qg83sxnghnw6du9rn2yx0a74q300y2l22zmsjm0 and
        -- bc1ql5j70a05srlvee3ugweyqukdfwwaph4pwaup74, cut since the wallet
        -- has not given them out
        (cutAddressOf <$> hwJson home ["show", "/0/5", "--type", "bip84"] []) `shouldReturn` Right "bc1qg83sxnghnw6du............00y2l22zmsjm0"
        (cutAddressOf <$> hwJson home ["show", "/1/0", "--type", "bip84"] []) `shouldReturn` Right "bc1ql5j70a05srlve............wwaph4pwaup74"

    it "counts its addresses apart from those of the bip44 account: each starts at /0/0" $
      withHome $ \home -> do
        newTestPortfolio home
        receiveWith home [] `shouldReturn` Text.unpack (externalAddrsText !! 0)
        receiveWith home ["--type", "bip84"] `shouldReturn` Text.unpack (segwitExternalAddrsText !! 0)
        receiveWith home [] `shouldReturn` Text.unpack (externalAddrsText !! 1)

    -- The payment: 10 bytes of frame, a P2WPKH input of 41 bytes, a P2PKH
    -- output of 34 and a P2WPKH change output of 31, then (2 + 108) / 4
    -- of witness data, rounded up to 28: 144 virtual bytes, a fee of 1440
    -- at 10 satoshis a virtual byte, where the same payment of a bip44
    -- coin is 226 bytes. Its coins are all bip84, so its change is too:
    -- 50000 - 20000 - 1440 = 28560. Signed, the input has an empty script
    -- and the signature in its witness, and the transaction is 116 bytes
    -- outside the witness and 109 or 110 with it: 144 virtual bytes as well.
    it "is paid, pays its fee on virtual bytes, is signed with the signature in the witness, and sent" $
      withChain ctx (chainPaying ctx (segwitExternalAddr 0)) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--type", "bip84"] []
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(PortfolioRef 0 0, 50000)]
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary, summarySize summary) `shouldBe` (1440, 10, 144)
        paidToWallet prepared `shouldBe` Map.fromList [(segwitInternalAddr 0, 28560)]
        nosig <- nosigOf prepared
        let file = home </> "signed.psbt"
        signed <- txOf =<< answer home ["signtx", nosig, "--output", file] seedOnce
        summarySize (txInfoSummary signed) `shouldBe` 144
        signedTx <- txSignDataTx . txFileData <$> readTxFile ctx file
        map (.script) signedTx.inputs `shouldBe` [BS.empty]
        map length signedTx.witness `shouldBe` [2]
        txid <- txidOf signed
        sent <- txOf =<< answer home ["broadcast", nosig, "--yes"] []
        txidOf sent `shouldReturn` txid
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(PortfolioRef 0 0, 28560)]

    -- The signature was checked apart from hw with the pure-Python verifier
    -- of the message-signing vectors: it recovers the key of the address,
    -- m/84'/0'/0'/0/0 of the test seed, under the header byte 39 of BIP 137
    it "signs a message from its address with the header byte of BIP 137, which verifymsg checks" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--type", "bip84"] []
        signed <- hwJson home ["signmsg", Text.unpack (segwitExternalAddrsText !! 0), "hello"] seedOnce
        signed `shouldBe` ResponseSignMsg "hello" segwitHelloSignature (segwitExternalAddrsText !! 0)
        hwJson home ["verifymsg", Text.unpack (segwitExternalAddrsText !! 0), Text.unpack segwitHelloSignature, "hello"] []
          `shouldReturn` ResponseVerifyMsg "hello" segwitHelloSignature (segwitExternalAddrsText !! 0) True

    -- The chain pays the first receiving address of the bip44 and of the
    -- bip84 account of portfolio0: 50000 + 30000 = 80000 in one pool
    it "is recovered by recover in the same portfolio as the bip44 account, both of which it syncs together" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        recoveredOf found `shouldBe` Right (Just 0, [0])
        (lookup mainPlace <$> syncedOf found) `shouldBe` Right (Just 80000)

{- Addresses -}

addressSpec :: Ctx -> Spec
addressSpec ctx =
  describe "The addresses of a portfolio" $ do
    it "are handed out by receive in the order of the receiving chain" $
      withHome $ \home -> do
        newTestPortfolio home
        forM_ (take 3 externalAddrsText) $ \expected ->
          (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(expected, "")]

    it "are counted in the figures portfolios shows of their network" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 2 $ answer home ["receive"] []
        run <- hw home ["portfolios"] []
        run `shouldSatisfy` printed "\n    btc  spendable 0.00000000 btc, no coins, 2 addresses\n"

    it "take the label given to receive" $
      withHome $ \home -> do
        newTestPortfolio home
        (addressesOf <$> hwJson home ["receive", "--label", "rent"] []) `shouldReturn` Right [(externalAddrsText !! 0, "rent")]

    it "are listed by hw addresses newest first, and after a given one when asked" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 3 $ answer home ["receive"] []
        (addressesOf <$> hwJson home ["addresses"] [])
          `shouldReturn` Right [(externalAddrsText !! 2, ""), (externalAddrsText !! 1, ""), (externalAddrsText !! 0, "")]
        (addressesOf <$> hwJson home ["addresses", "--max", "1"] []) `shouldReturn` Right [(externalAddrsText !! 2, "")]
        (addressesOf <$> hwJson home ["addresses", "--max", "1", "--after", Text.unpack (externalAddrsText !! 2)] [])
          `shouldReturn` Right [(externalAddrsText !! 1, "")]

    it "are shown by hw addresses under their derivation paths" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 2 $ answer home ["receive"] []
        run <- hw home ["addresses"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/1  " <> externalAddrsText !! 1)
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/0  " <> externalAddrsText !! 0)

    -- Narrowed to one account, the listing has no line naming where it is
    it "are listed by hw addresses of one account in two lines each, a blank line between them" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 2 $ answer home ["receive"] []
        run <- hw home ["addresses", "--network", "btc", "--type", "bip44"] []
        map Text.null (Text.lines (runOut run)) `shouldBe` [False, False, True, False, False]

    -- The path /0/9 is padded to the width of /0/10
    it "are aligned by hw addresses when their paths differ in length" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 11 $ answer home ["receive"] []
        run <- hw home ["addresses", "--max", "2"] []
        runOut run `shouldSatisfy` Text.isInfixOf "/0/10  "
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/9   " <> externalAddrsText !! 9)

    -- The payment of 20000 pays its change to the first change address
    it "are shown by hw addresses --change under their paths on the change chain" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["addresses", "--change"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/1/0  " <> internalAddrsText !! 0)

    it "are shown by receive under their derivation paths, with every detail" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["receive"] []
        runOut run `shouldSatisfy` Text.isPrefixOf ("bip44/0/0  " <> externalAddrsText !! 0 <> "\n  Transactions: 0\n")

    it "are named by label, given by their text" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 2 $ answer home ["receive"] []
        (addressesOf <$> hwJson home ["label", Text.unpack (externalAddrsText !! 1), "salary"] []) `shouldReturn` Right [(externalAddrsText !! 1, "salary")]
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 1, "salary"), (externalAddrsText !! 0, "")]

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is the first receiving address of
    -- portfolio1, which receive gave out while it was active. portfolio0
    -- is active when the address is labelled.
    it "are named by label in the portfolio they belong to, whichever is active" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        _ <- answer home ["receive"] []
        switchTo home 0 1
        _ <- answer home ["receive"] []
        switchTo home 0 0
        (addressesOf <$> hwJson home ["label", "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "rent"] []) `shouldReturn` Right [("17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "rent")]
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]
        switchTo home 0 1
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [("17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "rent")]

    -- The sixth receiving address of the bip44 account, which receive has
    -- not given out
    it "are not labelled when the wallet does not have them" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["label", Text.unpack (externalAddrsText !! 5), "salary"] [])
          `shouldReturn` Right ("The wallet has no address " <> externalAddrsText !! 5)

    -- The gap of BIP44: a wallet looking for payments stops after twenty
    -- unused addresses, so hw hands out no more until one is paid
    it "are refused past twenty that await a payment" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 20 $ answer home ["receive"] []
        (errorOf <$> hwJson home ["receive"] [])
          `shouldReturn` Right "The last 20 receiving addresses await a payment: reuse one of them, listed by hw addresses"

    -- Each account has a gap of its own
    it "are given out by the bip84 account when the bip44 one has twenty that await a payment" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 20 $ answer home ["receive"] []
        receiveWith home ["--type", "bip84"] `shouldReturn` Text.unpack (segwitExternalAddrsText !! 0)

    it "have no change address before the portfolio spends" $
      withHome $ \home -> do
        newTestPortfolio home
        (addressesOf <$> hwJson home ["addresses", "--change"] []) `shouldReturn` Right []

{- Sync -}

-- | Every pool of portfolio0 and portfolio1 of wallet0, in the order sync
-- goes over them: portfolio by portfolio, each network in the order of
-- the networks of a portfolio
twoPortfoliosPlaces :: [Place]
twoPortfoliosPlaces = [Place (PortfolioRef 0 index) net | index <- [0, 1], net <- [btc, bch, btcTest, btcTest4, btcRegTest, bchTest, bchTest4, bchRegTest]]

syncSpec :: Ctx -> Spec
syncSpec ctx =
  describe "A sync" $ do
    -- 50000 to portfolio0 and 30000 to portfolio1 on btc, found by the
    -- scan of the gap of each account: nothing else holds anything
    it "covers every portfolio of every wallet on every network, the test networks included" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        (syncedOf <$> hwJson home ["sync"] [])
          `shouldReturn` Right [(place, if place == mainPlace then 50000 else if place == portfolio1Place then 30000 else 0) | place <- twoPortfoliosPlaces]

    it "covers every portfolio, whichever is active" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        (fmap (map fst) . syncedOf <$> hwJson home ["sync"] []) `shouldReturn` Right twoPortfoliosPlaces

    it "covers the network --network names alone" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        (syncedOf <$> hwJson home ["sync", "--network", "btc"] []) `shouldReturn` Right [(mainPlace, 50000), (portfolio1Place, 30000)]

    -- A portfolio that holds nothing and changed nothing on a network gets
    -- no block of its own there
    it "names in one line the networks of a portfolio where nothing is held and nothing changed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["sync"] []
        run `shouldSatisfy` printed "\nportfolio0  nothing on bch, btctest, btctest4, btcreg, bchtest, bchtest4 and bchreg\n"

    -- The store answers every question about Bitcoin Cash with the error
    -- page of a proxy, as when the store behind the proxy is down
    it "leaves out a network whose store is down, with the failure of its store, and syncs the others" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        answered <- hwJson home ["sync"] []
        (lookup mainPlace <$> syncedOf answered) `shouldBe` Right (Just 50000)
        (map fst <$> notSyncedOf answered) `shouldBe` Right ["bch"]
        (map snd <$> notSyncedOf answered)
          `shouldSatisfy` either (const False) (all (\failure -> "The store at http://127.0.0.1:" `Text.isPrefixOf` failure && "/ answered with an error hw could not read: HTTP 502" `Text.isSuffixOf` failure))

    -- A script tells from the exit code that the sync is not whole
    it "exits with 1 when the store of a network is down, which it names last on a line not synced" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hw home ["sync"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "\n\nbch  not synced: The store at http://127.0.0.1:"

    -- The public server serves btc, bch, btctest4 and bchtest4 only
    it "names the networks the host does not serve, which is no failure" $
      withChainServing ctx [btc, bch, btcTest4, bchTest4] [] (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        answered <- hwJson home ["sync"] []
        notServedOf answered `shouldBe` Right ["btctest", "btcreg", "bchtest", "bchreg"]
        notSyncedOf answered `shouldBe` Right []
        (lookup mainPlace <$> syncedOf answered) `shouldBe` Right (Just 50000)

    it "exits with 0 when the host does not serve every network, which it names in one line" $
      withChainServing ctx [btc, bch, btcTest4, bchTest4] [] (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hw home ["sync"] []
        runExit run `shouldBe` ExitSuccess
        run `shouldSatisfy` printed "Not served by the store: btctest, btcreg, bchtest, bchreg\n"

    -- The store of bch answers its health check, then fails every question
    -- of the sync
    it "leaves out a network whose store fails after its health check, and syncs the others" $
      withChainFailingOn ctx [bch] (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        answered <- hwJson home ["sync"] []
        (lookup mainPlace <$> syncedOf answered) `shouldBe` Right (Just 50000)
        (map fst <$> syncedOf answered) `shouldSatisfy` either (const False) (notElem (Place mainPortfolioRef bch))
        (map fst <$> notSyncedOf answered) `shouldBe` Right ["bch"]

    it "exits with 1 when the store of a network fails after its health check, which it names on a line not synced" $
      withChainFailingOn ctx [bch] (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hw home ["sync"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "\n\nbch  not synced: The store at http://127.0.0.1:"

    -- The store of the test home refuses every connection: no network can
    -- be synced, which is what a wrong host looks like
    it "fails when no network can be synced, naming the store" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["sync"] []) `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed"

    -- The label and the lock are the user's: the store does not know them
    it "keeps with --full the labels and the locks the store cannot tell, while it writes the rest anew" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", txidArg (payment ctx), "salary"] []
        _ <- answer home ["lockcoin", coinArg (payment ctx) 0] []
        (spendableOn btc <$> hwJson home ["sync", "--full"] []) `shouldReturn` Right [(PortfolioRef 0 0, 0)]
        (coinLabelsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, "salary")]

    -- The payment is in the mempool, then leaves it: the store no longer
    -- lists it, and a sync of everything again finds it gone
    it "marks with --full as dead a transaction the store no longer lists" $
      withChain ctx (Chain 800002 [(payment ctx, Nothing)]) $ \home chain -> do
        synced home
        modifyIORef chain $ \c -> c {chainTxs = []}
        _ <- answer home ["sync", "--full"] []
        (deadTxsOf <$> hwJson home ["txs"] []) `shouldReturn` Right [txHash (payment ctx)]

{- Recovering a seed -}

-- | The paid chain, with a payment to the first receiving address of the
-- bip49 account 0 of the seed and of its bip86 account 0 in the same block
paidElsewhereChain :: Ctx -> Chain
paidElsewhereChain ctx = Chain 800002 [(payment ctx, Just 800000), (fundingTx ctx 3 [(wrappedExternalAddr 0, 30000), (taprootExternalAddr 0, 7000)], Just 800000)]

-- | A chain whose block 800000 pays 50000 to the first receiving address
-- of the bip44 account of each of the first portfolios of the test seed
-- on btc, as many as given
paidPortfoliosChain :: Ctx -> Natural -> IO Chain
paidPortfoliosChain ctx count = do
  firstAddrs <- firstAddrsOfAccounts ctx count
  return $ Chain 800002 [(fundingTx ctx 1 [(addr, 50000) | addr <- firstAddrs], Just 800000)]

-- | The first sentence of recover when no portfolio was paid: the eight
-- networks of a portfolio, and the gap of 10
noneFound :: Text
noneFound = "No portfolio of the mnemonic was paid on btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4 or bchreg. The search ended after 10 portfolios in a row without a payment."

recoverSpec :: Ctx -> Spec
recoverSpec ctx =
  describe "The portfolios of a seed found by recover" $ do
    it "are those paid, created in the wallet of the seed and synced" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        recoveredOf found `shouldBe` Right (Just 0, [0])
        (lookup mainPlace <$> syncedOf found) `shouldBe` Right (Just 50000)

    it "are created whole, with the twelve accounts of the five keys of their index" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- answer home ["recover"] seedOnce
        (fmap (map (\(_, net, kind, key) -> (net, kind, key))) . accountXPubsOf <$> hwJson home ["portfolios"] [])
          `shouldReturn` Right portfolio0Keys

    it "are said to be recovered, by their names and their wallet" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        take 1 (Text.lines (runOut run)) `shouldBe` ["Recovered portfolio0 of wallet0"]

    it "are found when paid on bch alone" $
      withChain ctx (chainPaying ctx bchExternalAddr) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        recoveredOf found `shouldBe` Right (Just 0, [0])
        (lookup (Place mainPortfolioRef bch) <$> syncedOf found) `shouldBe` Right (Just 50000)

    it "are found when paid at their bip84 account alone" $
      withChain ctx (chainPaying ctx (segwitExternalAddr 0)) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        recoveredOf found `shouldBe` Right (Just 0, [0])
        (lookup mainPlace <$> syncedOf found) `shouldBe` Right (Just 50000)

    -- The chain pays the first receiving address of portfolio1 alone
    it "are found past a portfolio never paid, and named alone as recovered" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        (recoveredOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right (Just 0, [1])

    -- A wallet always has portfolio0
    it "give the wallet found its portfolio0, though only portfolio1 was paid" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        _ <- answer home ["recover"] seedOnce
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 0 1]

    -- portfolio0 is one without a payment
    it "are looked for until --portfolio-gap portfolios in a row without a payment" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        (recoveredOf <$> hwJson home ["recover", "--portfolio-gap", "1"] seedOnce) `shouldReturn` Right (Nothing, [])
        (recoveredOf <$> hwJson home ["recover", "--portfolio-gap", "2"] seedOnce) `shouldReturn` Right (Just 0, [1])

    -- The nine portfolios 0 to 8 are not paid before portfolio9 is, and
    -- the ten portfolios 0 to 9 before portfolio10 is
    it "are looked for until 10 portfolios in a row without a payment, without --portfolio-gap" $ do
      addrs <- firstAddrsOfAccounts ctx 11
      withChain ctx (chainPaying ctx (addrs !! 9)) $ \home _ ->
        (recoveredOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right (Just 0, [9])
      withChain ctx (chainPaying ctx (addrs !! 10)) $ \home _ ->
        (recoveredOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right (Nothing, [])

    it "are looked for with a portfolio gap from 1 to 100" $
      withHome $ \home -> do
        forM_ ["0", "101"] $ \gap -> do
          run <- hw home ["recover", "--portfolio-gap", gap] seedOnce
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf "The portfolio gap is a number of portfolios, from 1 to 100"
        -- 100 is taken: recover goes on to the store, out of reach here
        run <- hw home ["recover", "--portfolio-gap", "100"] seedOnce
        run `shouldSatisfy` printed "The store at http://127.0.0.1:1/ could not be reached: the connection failed"

    it "leave out a portfolio the wallet has whole already, and say no new one was paid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        found <- hwJson home ["recover"] seedOnce
        recoveredOf found `shouldBe` Right (Just 0, [])
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]
        run <- hw home ["recover"] seedOnce
        take 1 (Text.lines (runOut run)) `shouldBe` ["No new portfolio of the mnemonic was paid on btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4 or bchreg. The search ended after 10 portfolios in a row without a payment."]

    -- wallet0 is of the second test mnemonic, and holds the active portfolio.
    -- wallet1 is of the test mnemonic, with portfolio0. The chain pays
    -- portfolio0 and portfolio1 of the test seed.
    it "are added to the wallet with the fingerprint of the seed, among the wallets of other seeds, those it lacks alone" $ do
      chain <- paidPortfoliosChain ctx 2
      withChain ctx chain $ \home _ -> do
        newPortfolioOf home testMnemonic2
        newTestPortfolio home
        (recoveredOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right (Just 1, [1])
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0, PortfolioRef 1 0, PortfolioRef 1 1]

    -- The server answers for every network but Bitcoin Cash
    it "are found on the networks whose store is up when the store of one is down" $
      withChainDownOn ctx [bch] (bothNetworksChain ctx) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        recoveredOf found `shouldBe` Right (Just 0, [0])
        (lookup mainPlace <$> syncedOf found) `shouldBe` Right (Just 50000)

    it "name last the network whose store is down, which was left out" $
      withChainDownOn ctx [bch] (bothNetworksChain ctx) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        runOut run `shouldSatisfy` Text.isInfixOf "\n\nbch  left out: The store at http://127.0.0.1:"
        runOut run `shouldSatisfy` Text.isSuffixOf "/ answered with an error hw could not read: HTTP 502\n"

    it "name in JSON the network whose store is down, with the failure of its store" $
      withChainDownOn ctx [bch] (bothNetworksChain ctx) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        (map fst <$> leftOutOf found) `shouldBe` Right ["bch"]
        (map snd <$> leftOutOf found)
          `shouldSatisfy` either (const False) (all (\failure -> "The store at http://127.0.0.1:" `Text.isPrefixOf` failure && "/ answered with an error hw could not read: HTTP 502" `Text.isSuffixOf` failure))

    -- A script tells from the exit code that the search is not whole, as it
    -- does after a sync that left a network out
    it "end with an error when a network was left out" $
      withChainDownOn ctx [bch] (bothNetworksChain ctx) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        runExit run `shouldBe` ExitFailure 1

    -- The public server serves btc, bch, btctest4 and bchtest4 only
    it "name the networks the host does not serve, which is no error" $
      withChainServing ctx [btc, bch, btcTest4, bchTest4] [] (paidChain ctx) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        notServedOf found `shouldBe` Right ["btctest", "btcreg", "bchtest", "bchreg"]
        leftOutOf found `shouldBe` Right []
        recoveredOf found `shouldBe` Right (Just 0, [0])

    it "end with success when the host does not serve every network" $
      withChainServing ctx [btc, bch, btcTest4, bchTest4] [] (paidChain ctx) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        runExit run `shouldBe` ExitSuccess
        run `shouldSatisfy` printed "Not served by the store: btctest, btcreg, bchtest, bchreg\n"

    -- The store of bch answers its health check, then fails every question
    it "name a network whose store fails after its health check on a line left out, and end with an error" $
      withChainFailingOn ctx [bch] (paidChain ctx) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "\nbch  left out: The store at http://127.0.0.1:"

    it "are found on the other networks when the store of one fails after its health check" $
      withChainFailingOn ctx [bch] (paidChain ctx) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        recoveredOf found `shouldBe` Right (Just 0, [0])
        (lookup mainPlace <$> syncedOf found) `shouldBe` Right (Just 50000)
        (map fst <$> leftOutOf found) `shouldBe` Right ["bch"]

    -- The chain pays the first receiving address of the bip44 account of
    -- portfolio0, and the first receiving address of the bip49 account 0
    -- and of the bip86 account 0 of the seed, which hw cannot spend from.
    -- The fake store has one chain for every network, so Bitcoin Cash
    -- reports the coin of m/44'/0'/0' too, as it would for a seed from
    -- before the fork.
    it "come with the funds at the paths of the seed hw cannot spend from, for which no account is added" $
      withChain ctx (paidElsewhereChain ctx) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        unspendableOf found
          `shouldBe` Right [("btc", "bip49", "/49'/0'/0'", 1, 30000), ("btc", "bip86", "/86'/0'/0'", 1, 7000), ("bch", "bip44", "/44'/0'/0'", 1, 50000)]
        (fmap (map (\(_, net, kind, _) -> (net, kind))) . accountXPubsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right twelveAccounts

    it "show those funds after the portfolios, each with its network, its path, its kind, its used addresses and what is left, then what to do" $
      withChain ctx (paidElsewhereChain ctx) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        run `shouldSatisfy` printed "Recovered portfolio0 of wallet0\n"
        run `shouldSatisfy` printed "\n\nThe seed was paid at paths hw cannot spend from:\n  btc  /49'/0'/0'  bip49, wrapped SegWit: 1 address used, 0.00030000 btc left\n  btc  /86'/0'/0'  bip86, Taproot: 1 address used, 0.00007000 btc left\n  bch  /44'/0'/0'  bip44 with the coin type of Bitcoin: 1 address used, 0.00050000 btc left\n  A wallet that holds such accounts can move the funds to an address of this wallet.\n"

    it "are said to be none when none was paid, before the funds at the paths hw cannot spend from" $
      withChain ctx (chainPaying ctx (wrappedExternalAddr 0)) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        runOut run `shouldBe` noneFound <> "\n\nThe seed was paid at paths hw cannot spend from:\n  btc  /49'/0'/0'  bip49, wrapped SegWit: 1 address used, 0.00050000 btc left\n  A wallet that holds such accounts can move the funds to an address of this wallet.\n"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    -- The chain pays 50000 to the first receiving address of each of the
    -- portfolios 0 to 20 of the seed: 21 portfolios, one more than recover
    -- finds before it asks. A store that lies reports them without end.
    -- The host serves btc alone: the search of Bitcoin Cash at m/44'/0'
    -- would ask about the same coins again.
    it "are asked about once the store reports 20 of them paid, and none is created when the answer is no" $ do
      chain <- paidPortfoliosChain ctx 21
      withChainServing ctx [btc] [] chain $ \home _ -> do
        run <- hw home ["recover"] (seedOnce <> ["n"])
        run `shouldSatisfy` printed "The store reports payments to 20 portfolios of the seed, as far as portfolio19. Keep looking? [y/N] "
        run `shouldSatisfy` printed "The search stopped there."
        runExit run `shouldBe` ExitFailure 1
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    it "are all created when the answer is yes, those found before the question and the one after" $ do
      chain <- paidPortfoliosChain ctx 21
      withChainServing ctx [btc] [] chain $ \home _ ->
        (recoveredOf <$> hwJson home ["recover"] (seedOnce <> ["y"])) `shouldReturn` Right (Just 0, [0 .. 20])

    -- The store of the test home refuses every connection, as on an offline
    -- computer, where the mnemonic would be typed for nothing
    it "are not looked for when the store is out of reach, which recover says before the mnemonic is asked" $
      withHome $ \home -> do
        run <- hw home ["recover"] seedOnce
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The store at http://127.0.0.1:1/ could not be reached: the connection failed"
        run `shouldNotSatisfy` askedMnemonic

    -- The chain pays the portfolio hw derived before it normalized the
    -- passphrase, where BIP39 gives another seed
    it "are looked for from the passphrase as typed when the normalized one has none, which recover says" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \home _ -> do
        run <- hw home ["recover"] cafeOnce
        runErr run `shouldSatisfy` Text.isInfixOf "The portfolios found come from the passphrase as typed"
        (fingerprintsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, cafeTypedFingerprint)]

    it "are those of the normalized passphrase, as BIP39 derives them, without a word about the other" $
      withChain ctx (chainPaying ctx cafeNormalizedAddr) $ \home _ -> do
        run <- hw home ["recover"] cafeOnce
        runErr run `shouldNotSatisfy` Text.isInfixOf "as typed"
        (fingerprintsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, cafeNormalizedFingerprint)]

{- The passphrase as typed -}

-- | The words of the test wallet with the passphrase "café", typed once, or
-- twice each as newportfolio --existing asks
cafeOnce, cafeTwice :: [Text]
cafeOnce = seedOnceOf (MnemonicPass (mnemonicWords testMnemonic) cafePassphrase)
cafeTwice = seedTwiceOf (MnemonicPass (mnemonicWords testMnemonic) cafePassphrase)

passphraseSpec :: Ctx -> Spec
passphraseSpec ctx =
  describe "A portfolio made from a passphrase with accents" $ do
    it "is of the seed BIP39 derives, when the database has no wallet of the passphrase as typed" $
      withHome $ \home -> do
        _ <- hw home ["newportfolio", "--existing"] cafeTwice
        (fingerprintsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, cafeNormalizedFingerprint)]

    -- hw 0.9 derived its keys from the passphrase as typed: a wallet it
    -- made from "café" has the keys of aa280627, and the new one those of
    -- a3a04db1, whose funds would seem lost
    it "warns that a wallet of an earlier hw has other keys, and points to recover, when the database has a wallet of neither seed" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--existing"] cafeTwice
        runErr run
          `shouldSatisfy` Text.isInfixOf
            "hw normalizes this passphrase as BIP39 asks, and the keys come from it so. \
            \hw 0.9 and earlier derived their keys from the passphrase as typed, so a wallet they made from it has other keys. \
            \To find such a wallet, run recover on an online computer: it tries the passphrase both ways."

    it "gives no such warning for a passphrase that normalizing leaves as it is" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--existing"] seedTwice
        runErr run `shouldNotSatisfy` Text.isInfixOf "normalizes this passphrase"

    -- recover found the wallet of the passphrase as typed
    it "is of the seed of the passphrase as typed when the database has a wallet of it and none of the other, which newportfolio says" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \home _ -> do
        _ <- answer home ["recover"] cafeOnce
        run <- hw home ["newportfolio", "--existing"] cafeTwice
        runErr run `shouldSatisfy` Text.isInfixOf "to keep them together"
        (fingerprintsOf <$> hwJson home ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 0, cafeTypedFingerprint), (PortfolioRef 0 1, cafeTypedFingerprint)]

    -- The online computer has the wallet of the passphrase as typed,
    -- which recover found, and the one BIP39 derives, from the file of a
    -- signer
    it "is of the seed BIP39 derives when the database has a wallet of each seed" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \online _ -> withHome $ \signer -> do
        _ <- answer signer ["newportfolio", "--existing"] cafeTwice
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        _ <- answer online ["recover"] cafeOnce
        _ <- answer online ["importwallet", signer </> "wallet.jsonl"] []
        run <- hw online ["newportfolio", "--existing"] cafeTwice
        runErr run `shouldNotSatisfy` Text.isInfixOf "as typed"
        (fingerprintsOf <$> hwJson online ["portfolios"] [])
          `shouldReturn` Right [(PortfolioRef 0 0, cafeTypedFingerprint), (PortfolioRef 1 0, cafeNormalizedFingerprint), (PortfolioRef 1 1, cafeNormalizedFingerprint)]

{- Signed messages -}

messageSpec :: Spec
messageSpec =
  describe "A message signed by signmsg" $ do
    it "is signed by the key of an address of the wallet, from its mnemonic" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        signed <- hwJson home ["signmsg", textOf (externalAddr 0), "hello"] seedOnce
        signed `shouldBe` ResponseSignMsg "hello" helloSignature (externalAddrsText !! 0)

    -- hw stores the address /0/0 of the bip44 account of portfolio0 on bch
    -- as bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u; it is
    -- typed here without its prefix, and in the legacy form. The signature
    -- is checked apart from hw as above: the key it recovers is the one of
    -- the address in both forms, m/44'/145'/0'/0/0 of the test seed.
    it "is signed by the key of a Bitcoin Cash address however it is written" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        forM_ ["qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ"] $ \written ->
          hwJson home ["signmsg", written, "hello"] seedOnce
            `shouldReturn` ResponseSignMsg
              "hello"
              "H98t/71A7/ED0lRtdsnAiVwcgUv+5sZr7WJwpHS3YFcCcTKtSG/rx2ooIXOTMbdhApTDjcfGjTu3Sjgh6Pl5qQc="
              (Text.pack written)

    it "is signed by the key of the address at a path of the active portfolio, which the answer names" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        signed <- hwJson home ["signmsg", "bip44/0/0", "hello"] seedOnce
        signed `shouldBe` ResponseSignMsg "hello" helloSignature "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"

    -- A new portfolio holds no address, as on the offline computer, where
    -- receive never runs
    it "is signed at a path on a computer that gave out no address, from the public key of the account" $
      withHome $ \home -> do
        newTestPortfolio home
        signed <- hwJson home ["signmsg", "bip44/0/0", "hello"] seedOnce
        signed `shouldBe` ResponseSignMsg "hello" helloSignature "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"

    -- The wallet gave out bip44/0/0 on bch alone, and the default network
    -- is btc
    it "is signed at a path on the default network without --network, whatever networks the wallet gave the address out on" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        signed <- hwJson home ["signmsg", "bip44/0/0", "hello"] seedOnce
        signed `shouldBe` ResponseSignMsg "hello" helloSignature "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"

    -- The signature is the one of that address above, written in any form
    it "is signed at a path on the --network named, which the answer names as the wallet stores it" $
      withHome $ \home -> do
        newTestPortfolio home
        hwJson home ["signmsg", "bip44/0/0", "hello", "--network", "bch"] seedOnce
          `shouldReturn` ResponseSignMsg
            "hello"
            "H98t/71A7/ED0lRtdsnAiVwcgUv+5sZr7WJwpHS3YFcCcTKtSG/rx2ooIXOTMbdhApTDjcfGjTu3Sjgh6Pl5qQc="
            "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address of the
    -- bip44 account of portfolio0 on btc. verifymsg checks the signature
    -- against it.
    it "is signed at a change path by the key of the change address there" $
      withHome $ \home -> do
        newTestPortfolio home
        signed <- hwJson home ["signmsg", "bip44/1/0", "hello"] seedOnce
        case signed of
          ResponseSignMsg _ signature signer -> do
            signer `shouldBe` "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"
            hwJson home ["verifymsg", "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", Text.unpack signature, "hello"] []
              `shouldReturn` ResponseVerifyMsg "hello" signature "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn" True
          other -> expectationFailure ("Not a signed message: " <> show other)

    it "is refused at a path of a type the network does not have, before the mnemonic is asked" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["signmsg", "bip84/0/0", "hello", "--network", "bch"] seedOnce
        run `shouldSatisfy` printed "A bip84 account needs SegWit, which bch does not have"
        run `shouldNotSatisfy` askedMnemonic

    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ is of another wallet. The wallet
    -- would hold an address of its own only once this computer gave it out.
    it "is refused for an address the wallet does not hold, with the path to give instead" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["signmsg", "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ", "hello"] seedOnce)
          `shouldReturn` Right "The wallet has no address 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ. A computer holds only the addresses it gave out. On another one, such as the offline computer, give the path of the address instead, as addresses shows it, such as bip84/0/3"

    it "is refused after three seeds that are not the wallet's" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["signmsg", textOf (externalAddr 0), "hello"] (concat (replicate 3 (seedOnceOf testMnemonic2))))
          `shouldReturn` Right "The mnemonic or the passphrase does not match the account"

    it "is refused for an address the wallet does not have, before the mnemonic is asked" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["signmsg", textOf (otherAddr 0), "hello"] seedOnce
        run `shouldSatisfy` printed ("The wallet has no address " <> Text.pack (textOf (otherAddr 0)))
        run `shouldNotSatisfy` askedMnemonic

    -- The block signmsg prints, line for line
    it "is written to the --output file as its armored block, and hw says where" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        let file = home </> "signed.txt"
        run <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--output", file] seedOnce
        runOut run `shouldBe` "Written to " <> Text.pack file <> "\n"
        (Text.pack <$> readFile file)
          `shouldReturn` Text.unlines
            [ "-----BEGIN BITCOIN SIGNED MESSAGE-----",
              "hello",
              "-----BEGIN BITCOIN SIGNATURE-----",
              externalAddrsText !! 0,
              helloSignature,
              "-----END BITCOIN SIGNATURE-----"
            ]

    it "is answered in --json by the --output file it was written to" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        let file = home </> "signed.txt"
        hwJson home ["signmsg", textOf (externalAddr 0), "hello", "--output", file] seedOnce
          `shouldReturn` ResponseFile file

    it "is refused for an --output file that exists, before the mnemonic is asked, and the file is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        let file = home </> "signed.txt"
        writeFile file "kept\n"
        run <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--output", file] seedOnce
        run `shouldSatisfy` printed (alreadyExists file)
        run `shouldNotSatisfy` askedMnemonic
        readFile file `shouldReturn` "kept\n"

{- Verified messages -}

verifySpec :: Spec
verifySpec =
  describe "A signature checked by verifymsg" $ do
    it "is valid for the message as signed, without the wallet, which hw says in one line" $
      withHome $ \home -> do
        hwJson home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello"] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True
        run <- hw home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello"] []
        runOut run `shouldBe` "The key of " <> externalAddrsText !! 0 <> " signed this message\n"

    it "is invalid for a changed message, and hw says so and exits with a failure" $
      withHome $ \home -> do
        run <- hw home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello!"] []
        runExit run `shouldBe` ExitFailure 1
        runOut run `shouldBe` "This is not a signature of this message by the key of " <> externalAddrsText !! 0 <> "\n"
        hwJson home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello!"] []
          `shouldReturn` ResponseVerifyMsg "hello!" helloSignature (externalAddrsText !! 0) False

    it "warns about the whitespace of a message found invalid" $
      withHome $ \home -> do
        run <- hw home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello "] []
        run `shouldSatisfy` printed "Warning: The message has leading or trailing whitespace"

    -- The default network of the home is btc, which does not read a
    -- bchtest address
    it "reads an address of another network than the default one, without --network" $
      withHome $ \home ->
        hwJson home ["verifymsg", Text.unpack bchTestAddrText, Text.unpack bchTestSignature, Text.unpack bchTestMessage] []
          `shouldReturn` ResponseVerifyMsg bchTestMessage bchTestSignature bchTestAddrText True

    -- The signature of Bitcoin Core's rpc_signmessagewithprivkey test, by a
    -- regtest key. Its P2PKH address reads the same on the six test
    -- networks, so any of them gives the answer.
    it "reads a P2PKH address that several networks share, without --network" $
      withHome $ \home ->
        hwJson home ["verifymsg", "mpLQjfK79b7CCV4VMJWEWAj5Mpx8Up5zxB", "INbVnW4e6PeRmsv2Qgu8NuopvrVjkcxob+sX8OcZG0SALhWybUjzMLPdAsXI46YZGb0KQTRii+wWIQzRpG/U+S0=", "This is just a test message"] []
          `shouldReturn` ResponseVerifyMsg "This is just a test message" "INbVnW4e6PeRmsv2Qgu8NuopvrVjkcxob+sX8OcZG0SALhWybUjzMLPdAsXI46YZGb0KQTRii+wWIQzRpG/U+S0=" "mpLQjfK79b7CCV4VMJWEWAj5Mpx8Up5zxB" True

    it "reads the address on the --network given" $
      withHome $ \home ->
        hwJson home ["verifymsg", Text.unpack bchTestAddrText, Text.unpack bchTestSignature, Text.unpack bchTestMessage, "--network", "bchtest4"] []
          `shouldReturn` ResponseVerifyMsg bchTestMessage bchTestSignature bchTestAddrText True

    it "refuses an address that the --network given does not read, even when another network reads it" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["verifymsg", Text.unpack bchTestAddrText, Text.unpack bchTestSignature, Text.unpack bchTestMessage, "--network", "btc"] [])
          `shouldReturn` Right "The address bchtest:qztv0mmm3yscxcvfr93hq3wn63xk50merq7wl5gp3m is not valid on btc"

    -- The bchtest address of the vector with its last character changed,
    -- from m to n: its checksum no longer matches
    it "refuses an address that no network reads" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["verifymsg", "bchtest:qztv0mmm3yscxcvfr93hq3wn63xk50merq7wl5gp3n", Text.unpack bchTestSignature, Text.unpack bchTestMessage] [])
          `shouldReturn` Right "The address bchtest:qztv0mmm3yscxcvfr93hq3wn63xk50merq7wl5gp3n is not valid on any network"

    it "reads the block signmsg printed, saved in a file" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        signed <- hw home ["signmsg", textOf (externalAddr 0), "hello"] seedOnce
        Text.lines (runOut signed)
          `shouldBe` [ "-----BEGIN BITCOIN SIGNED MESSAGE-----",
                       "hello",
                       "-----BEGIN BITCOIN SIGNATURE-----",
                       externalAddrsText !! 0,
                       helloSignature,
                       "-----END BITCOIN SIGNATURE-----"
                     ]
        let file = home </> "signed.txt"
        writeFile file (Text.unpack (runOut signed))
        hwJson home ["verifymsg", "--input", file] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "reads the file signmsg --output wrote" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        let file = home </> "signed.txt"
        _ <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--output", file] seedOnce
        hwJson home ["verifymsg", "--input", file] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "refuses a file that holds no block" $
      withHome $ \home -> do
        let file = home </> "plain.txt"
        writeFile file "hello\n"
        (errorOf <$> hwJson home ["verifymsg", "--input", file] [])
          `shouldReturn` Right (Text.pack $ "The file " <> file <> " could not be read: no \"-----BEGIN BITCOIN SIGNED MESSAGE-----\" line")

    it "reads a message of several lines, with its whitespace, from the block" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        let message = "two lines, \nand a trailing space "
        signed <- hw home ["signmsg", textOf (externalAddr 0), message] seedOnce
        let file = home </> "signed.txt"
        writeFile file (Text.unpack (runOut signed))
        checked <- hwJson home ["verifymsg", "--input", file] []
        case checked of
          ResponseVerifyMsg m _ a valid -> (m, a, valid) `shouldBe` (Text.pack message, externalAddrsText !! 0, True)
          other -> expectationFailure $ "Not a verifymsg answer: " <> show other

    it "reads the --json answer of signmsg, saved in a file" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        signed <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--json"] seedOnce
        let file = home </> "signed.psbt"
        writeFile file (Text.unpack (runOut signed))
        hwJson home ["verifymsg", "--input", file] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "refuses a file holding another answer of hw" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--json"] [""]
        let file = home </> "newportfolio.json"
        writeFile file (Text.unpack (runOut run))
        (errorOf <$> hwJson home ["verifymsg", "--input", file] [])
          `shouldReturn` Right (Text.pack $ "The file " <> file <> " holds an answer of hw, but not one of signmsg")

    -- The answer of a transaction whose place names a network hw does not
    -- know: hw stopped on such an answer with an uncaught exception before
    it "refuses a file holding another answer, whatever network it names" $
      withHome $ \home -> do
        let file = home </> "unknown.json"
        writeFile file "{\"type\":\"tx\",\"wallet\":0,\"portfolio\":0,\"network\":\"nope\",\"tx\":{}}"
        (errorOf <$> hwJson home ["verifymsg", "--input", file] [])
          `shouldReturn` Right (Text.pack $ "The file " <> file <> " holds an answer of hw, but not one of signmsg")

    -- The refusals of Bitcoin Core's message_verify test, with a Taproot
    -- address in place of its P2SH one, which hw takes for one of a key
    it "refuses an address that is not of one key" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["verifymsg", "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0", "signature should be irrelevant", "message too"] [])
          `shouldReturn` Right "The address is not a P2PKH or a SegWit address of one key: a message can only be signed from one"

    it "refuses a signature that is not in base64" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["verifymsg", "1KqbBpLy5FARmTPD4VZnDDpYjkUvkr82Pm", "invalid signature, not in base64 encoding", "message should be irrelevant"] [])
          `shouldReturn` Right "The signature is not in base64"

{- The database -}

backupSpec :: Spec
backupSpec = do
  describe "The database" $ do
    it "is copied by backup to a file, a database with the portfolios of the wallet" $
      withHome $ \home -> withHome $ \restored -> do
        newTestPortfolio home
        (shownFile <$> hwJson home ["backup", home </> "copy.sqlite"] []) `shouldReturn` Right (home </> "copy.sqlite")
        copyFile (home </> "copy.sqlite") (databaseOf restored)
        (portfolioRefsOf <$> hwJson restored ["portfolios"] []) `shouldReturn` Right [PortfolioRef 0 0]

    it "is not copied over a file that exists" $
      withHome $ \home -> do
        newTestPortfolio home
        writeFile (home </> "copy.sqlite") ""
        (errorOf <$> hwJson home ["backup", home </> "copy.sqlite"] [])
          `shouldReturn` Right (alreadyExists (home </> "copy.sqlite"))

    -- hw crashed with the exception of SQLite before
    it "is not copied into a directory that does not exist, and hw says so" $
      withHome $ \home -> do
        newTestPortfolio home
        let copy = home </> "missing" </> "copy.sqlite"
        (errorOf <$> hwJson home ["backup", copy] [])
          `shouldReturn` Right ("The database could not be copied to " <> Text.pack copy <> ": no such file or directory")

    it "is copied by backup into the backups of hw without a file" $
      withHome $ \home -> do
        newTestPortfolio home
        copy <- shownFile <$> hwJson home ["backup"] []
        copy `shouldSatisfy` either (const False) ((home </> ".hw" </> "backups") `isInfixOf`)

  describe "The labels" $ do
    -- The description is what the user typed, which the mnemonic does not
    -- bring back
    it "are copied after label into the backups of hw, in a file importwallet reads into another home" $
      withHome $ \home -> withHome $ \restored -> do
        newTestPortfolio home
        _ <- answer home ["label", "savings"] []
        let backups = home </> ".hw" </> "backups"
        copies <- sort <$> listDirectory backups
        newest <- case reverse copies of
          name : _ -> return (backups </> name)
          [] -> fail "No copy of the labels"
        _ <- answer restored ["importwallet", newest] []
        (descriptionsOf <$> hwJson restored ["portfolios"] []) `shouldReturn` Right [(PortfolioRef 0 0, "savings")]

    -- A file stands where the directory backups goes. The name of the copy
    -- carries the time it was made, so the warning is checked around it.
    it "are not copied when the copy cannot be written, which hw tells on stderr, and the command still succeeds" $
      withHome $ \home -> do
        newTestPortfolio home
        let backups = home </> ".hw" </> "backups"
        removeDirectoryRecursive backups
        writeFile backups ""
        run <- hw home ["label", "savings"] []
        let warned =
              Text.isPrefixOf ("The labels were not backed up. The file " <> Text.pack (backups </> "accounts-labels-")) (runErr run)
                && Text.isSuffixOf ".jsonl could not be written: not a directory\n" (runErr run)
        (runExit run, warned) `shouldBe` (ExitSuccess, True)
  where
    shownFile (ResponseFile file) = Right file
    shownFile other = Left (show other)
    descriptionsOf (ResponsePortfolios views) = Right [(viewRef view, viewDescription view) | view <- views]
    descriptionsOf other = Left (show other)
