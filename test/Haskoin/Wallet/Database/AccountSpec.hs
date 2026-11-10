{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | The accounts of the portfolios. A whole portfolio holds twelve
-- accounts, made together from its seed. A command reaches one of them
-- by the network and the type its options or the configuration give. A
-- partial portfolio, as the step from the database of hw 0.9 leaves it,
-- works with the accounts it has until its seed fills it in, and the
-- wallets of one seed are merged into one. A portfolio is deleted with
-- everything it holds, and shown with one set of figures for each
-- network.
module Haskoin.Wallet.Database.AccountSpec (spec) where

import Control.Monad (forM_, replicateM_, void)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson (eitherDecode, toJSON, (.=))
import Data.Default (def)
import Data.List (sortOn)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime (..), fromGregorian, getCurrentTime)
import Data.Word (Word64, Word8)
import qualified Data.Serialize as S
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Arbitrary (arbitraryAccountKeys, arbitraryNetworkView, arbitraryPortfolioView)
import Haskoin.Wallet.Cli.Listing (Paging (..))
import Haskoin.Wallet.Cli.Parser (Command (..), ListingOutput (..), Target (Target))
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Commands (commandResponse)
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address (genExtAddress, nextChangeAddress, updateAddressBalances)
import Haskoin.Wallet.Database.BestBlock (SyncedBlock (..), syncedBlock, updateSyncedBlock)
import Haskoin.Wallet.Database.Coin (refreshCoins, spendableBalance)
import Haskoin.Wallet.Database.PendingTx (pendingTxs)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo (markTxInfoDead, repsertSummary, txInfoKey, txsAfter)
import Haskoin.Wallet.Database.TxLabel (setTxLabel, txLabel)
import Haskoin.Wallet.Keys
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import System.FilePath ((</>))
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx -> do
  creatingSpec ctx
  nextIndexSpec ctx
  testNetworksSpec ctx
  choosingSpec ctx
  partialSpec ctx
  fillingSpec ctx
  adoptingSpec ctx
  listingSpec ctx
  deletingSpec ctx
  activitySpec ctx
  balanceSpec ctx
  viewSpec ctx
  jsonSpec
  -- A portfolio as portfolios shows it, and its parts, read back as they
  -- were written
  testIdentity
    def
      { jsonTests = [JsonBox (arbitraryPortfolioView ctx), JsonBox (arbitraryNetworkView ctx), JsonBox (arbitraryAccountKeys btc ctx)]
      }

{- Fixtures -}

-- | The key of an account of a seed, at an index that has one, as the
-- indexes of these tests all have
keyOf :: PublicSeed -> AccountKind -> Network -> Natural -> IO XPubKey
keyOf seed kind net index = maybe (fail "The child key at this index is invalid") return =<< seedAccountKey seed kind net index

-- | The key of an account written with another child number, as a file of
-- a wallet can write it: the same public key and chain code, which give
-- the same addresses
renumbered :: KeyIndex -> XPubKey -> XPubKey
renumbered child key = XPubKey key.depth key.parent child key.chain key.key

-- | The summaries of a pool, each by its transaction and whether it is
-- dead, in the order of the transaction ids
summariesHeld :: Pool -> ExceptT String (DB IO) [(TxHash, Bool)]
summariesHeld pool = do
  rows <- lift $ P.selectList [DBTxInfoPortfolio P.==. pool.poolPortfolio, DBTxInfoNetwork P.==. poolNetworkName pool] [P.Asc DBTxInfoTxid]
  liftEither $ mapM (\(P.Entity _ row) -> (,dBTxInfoDead row) <$> bytesToHash (dBTxInfoTxid row)) rows

-- | The creation date of a portfolio, as its row holds it now
createdOf :: DBPortfolioId -> ExceptT String (DB IO) UTCTime
createdOf key = dBPortfolioCreated . (.portfolioRow) <$> getPortfolio key

-- | Two days the rows of a test are dated by: 1 Jun 2023 and 1 Jan 2024
june2023, january2024 :: UTCTime
june2023 = UTCTime (fromGregorian 2023 6 1) 0
january2024 = UTCTime (fromGregorian 2024 1 1) 0

-- | The accounts of a portfolio as its rows hold them, each by the name of
-- its network and its path, in the order hw lists them
accountPaths :: Portfolio -> ExceptT String (DB IO) [(Text, String)]
accountPaths portfolio =
  map (\(_, acc) -> (dBAccountNetwork acc, pathToStr (accountPath portfolio acc))) <$> lift (portfolioAccountRows portfolio.portfolioId)

-- | The accounts of a portfolio, each by the name of its network and its
-- purpose, in the order hw lists them
accountsHeld :: DBPortfolioId -> ExceptT String (DB IO) [(Text, Int)]
accountsHeld key = map (\(_, acc) -> (dBAccountNetwork acc, dBAccountPurpose acc)) <$> lift (portfolioAccountRows key)

-- | The account a command works on, by the name of its network and its
-- purpose
chosenAccount :: Config -> Portfolio -> Maybe Network -> Maybe AccountKind -> ExceptT String (DB IO) (Text, Int)
chosenAccount cfg portfolio netM kindM = (\(_, acc) -> (dBAccountNetwork acc, dBAccountPurpose acc)) <$> chooseAccount cfg portfolio netM kindM

-- | The wallets of the database, each by its index and its fingerprint
walletsHeld :: ExceptT String (DB IO) [(Natural, Maybe Text)]
walletsHeld = map (\(_, wallet) -> (walletIndex wallet, dBWalletFingerprint wallet)) <$> lift getWallets

-- | The refusal of an account a partial portfolio lacks ends by saying how
-- to fill the portfolio in
fillInHint :: String
fillInHint = "It is a partial portfolio, which lacks accounts. Fill it in with fillwallet on the computer that has the mnemonic, or with importwallet and a file of the whole wallet that exportwallet wrote there."

-- | The twelve accounts of a whole portfolio, by network and type
wholePortfolio :: [(Text, Text)]
wholePortfolio =
  [ ("btc", "bip44"),
    ("btc", "bip84"),
    ("bch", "bip44"),
    ("btctest", "bip44"),
    ("btctest", "bip84"),
    ("btctest4", "bip44"),
    ("btctest4", "bip84"),
    ("btcreg", "bip44"),
    ("btcreg", "bip84"),
    ("bchtest", "bip44"),
    ("bchtest4", "bip44"),
    ("bchreg", "bip44")
  ]

-- | The accounts a portfolio lacks, by network and type
lacking :: DBPortfolioId -> ExceptT String (DB IO) [(Text, Text)]
lacking key = map (\(net, kind) -> (cs net.name, kindName kind)) <$> lift (missingAccounts key)

-- | Give what a portfolio holds on Bitcoin one row of every kind: a
-- receiving address of its bip44 account, a coin at the address given,
-- and a summary, a label, a pending transaction and a synced block around
-- the transaction of the number
filled :: Ctx -> Portfolio -> Address -> Word8 -> ExceptT String (DB IO) ()
filled ctx portfolio addr n = do
  let pool = portfolioPool portfolio btc
  (accId, _) <- accountOf portfolio btc Bip44
  _ <- genExtAddress ctx def accId ""
  _ <- refreshCoins btc ctx accId [addr] [storeCoin ctx (fakeTxHash n, 0) (Just 800000) addr 1000]
  _ <- lift $ repsertSummary pool (fakeTxHash n) (Store.BlockRef 800000 1) fixtureTime paymentSummary
  lift $ setTxLabel pool (fakeTxHash n) "rent"
  lift $ P.insert_ $ pendingRow pool (spendingTx ctx [(fakeTxHash n, 0)] [(otherAddr 0, 900)])
  lift $ updateSyncedBlock pool (fakeBlockHash n) 800000

-- | The keys of the accounts of a portfolio, read while it has them
accountIds :: Portfolio -> ExceptT String (DB IO) [DBAccountId]
accountIds portfolio = map fst <$> lift (portfolioAccountRows portfolio.portfolioId)

-- | How many rows a portfolio has, given the keys its accounts had:
-- accounts, addresses, coins, summaries, labels of transactions, pending
-- transactions and synced blocks
rowsOf :: Portfolio -> [DBAccountId] -> ExceptT String (DB IO) (Int, Int, Int, Int, Int, Int, Int)
rowsOf portfolio accounts =
  lift $
    (,,,,,,)
      <$> P.count [DBAccountPortfolio P.==. key]
      <*> P.count [DBAddressAccount P.<-. accounts]
      <*> P.count [DBCoinAccount P.<-. accounts]
      <*> P.count [DBTxInfoPortfolio P.==. key]
      <*> P.count [DBTxLabelPortfolio P.==. key]
      <*> P.count [DBPendingTxPortfolio P.==. key]
      <*> P.count [DBSyncedBlockPortfolio P.==. key]
  where
    key = portfolio.portfolioId

{- Creating -}

creatingSpec :: Ctx -> Spec
creatingSpec ctx =
  describe "Creating a whole portfolio from its seed" $ do
    it "gives portfolio 0 its twelve accounts: bip44 and bip84 on btc, bip44 on bch, and the test networks at the coin type 1" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        accountPaths portfolio
          `dbShouldBeE` [ ("btc", "/44'/0'/0'"),
                          ("btc", "/84'/0'/0'"),
                          ("bch", "/44'/145'/0'"),
                          ("btctest", "/44'/1'/0'"),
                          ("btctest", "/84'/1'/0'"),
                          ("btctest4", "/44'/1'/0'"),
                          ("btctest4", "/84'/1'/0'"),
                          ("btcreg", "/44'/1'/0'"),
                          ("btcreg", "/84'/1'/0'"),
                          ("bchtest", "/44'/1'/0'"),
                          ("bchtest4", "/44'/1'/0'"),
                          ("bchreg", "/44'/1'/0'")
                        ]

    it "gives portfolio 1 the same paths with 1' as the last step" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        portfolio <- nextPortfolioOf ctx testMnemonic
        accountPaths portfolio
          `dbShouldBeE` [ ("btc", "/44'/0'/1'"),
                          ("btc", "/84'/0'/1'"),
                          ("bch", "/44'/145'/1'"),
                          ("btctest", "/44'/1'/1'"),
                          ("btctest", "/84'/1'/1'"),
                          ("btctest4", "/44'/1'/1'"),
                          ("btctest4", "/84'/1'/1'"),
                          ("btcreg", "/44'/1'/1'"),
                          ("btcreg", "/84'/1'/1'"),
                          ("bchtest", "/44'/1'/1'"),
                          ("bchtest4", "/44'/1'/1'"),
                          ("bchreg", "/44'/1'/1'")
                        ]

    it "gives each account the key the seed derives at its path" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        let keyAt net kind = dBAccountXPubKey . snd <$> accountOf portfolio net kind
        keyAt btc Bip44 `dbShouldBeE` testAccountXPub
        keyAt btc Bip84 `dbShouldBeE` testSegwitAccountXPub
        keyAt bch Bip44 `dbShouldBeE` testAccountXPubBch
        keyAt btcTest Bip44 `dbShouldBeE` testAccountXPubTest
        keyAt btcTest Bip84 `dbShouldBeE` testSegwitAccountXPubTest

    it "gives the accounts of portfolio 1 the keys at the index 1" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        portfolio <- nextPortfolioOf ctx testMnemonic
        (dBAccountXPubKey . snd <$> accountOf portfolio btc Bip44) `dbShouldBeE` testAccountXPub1

    it "starts every account without an address and without a balance" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        accounts <- lift $ portfolioAccountRows portfolio.portfolioId
        liftIO $
          [(dBAccountExternal acc, dBAccountInternal acc, dBAccountBalanceConfirmed acc, dBAccountBalanceUnconfirmed acc, dBAccountBalanceCoins acc) | (_, acc) <- accounts]
            `shouldBe` replicate 12 (0, 0, 0, 0, 0)

    it "files the portfolio under the wallet of the seed, which keeps the fingerprint" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        liftIO $ do
          portfolioRef portfolio `shouldBe` PortfolioRef 0 0
          portfolioFingerprint portfolio `shouldBe` Just testFingerprint

    it "tells the kind and the pool of an account from its row" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (_, acc) <- accountOf portfolio btc Bip84
        liftIO $ do
          accountKind acc `shouldBe` Bip84
          dBAccountPurpose acc `shouldBe` 84
          accountPool acc `shouldBe` Pool portfolio.portfolioId btc
          accountRef portfolio acc `shouldBe` AccountRef (Place (PortfolioRef 0 0) btc) Bip84

    it "refuses a bip84 account on Bitcoin Cash, which has no SegWit" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" []
        key <- liftIO $ keyOf (testSeed ctx) Bip84 btc 0
        lift $
          dbShouldFailWith "A bip84 account needs SegWit, which bch does not have" $
            insertAccount portfolio.portfolioId Bip84 bch ctx key

    it "refuses a key an account of the network holds already, naming its portfolio" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        other <- migrated ctx (testSeed ctx) 1 "" []
        key <- liftIO $ keyOf (testSeed ctx) Bip44 btc 0
        lift $
          dbShouldFailWith "portfolio0 of wallet0 already has this key on btc" $
            insertAccount other.portfolioId Bip44 btc ctx key

    -- The key of account 0 written with the child number 5', which is
    -- 0x80000005: the addresses of portfolio 0 in portfolio 5
    it "refuses a key an account of the network holds, though written with another child number" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        other <- migrated ctx (testSeed ctx) 5 "" []
        key <- liftIO $ keyOf (testSeed ctx) Bip44 btc 0
        lift $
          dbShouldFailWith "portfolio0 of wallet0 already has this key on btc" $
            insertAccount other.portfolioId Bip44 btc ctx (renumbered 0x80000005 key)

{- The index of the next portfolio -}

nextIndexSpec :: Ctx -> Spec
nextIndexSpec ctx =
  describe "The index of the next portfolio of a wallet" $ do
    it "is 0 in a wallet without a portfolio" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        lift (nextPortfolioIndex (testSeed ctx) walletId) `dbShouldBeE` 0

    it "is 1 in a wallet with portfolio 0" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift (nextPortfolioIndex (testSeed ctx) portfolio.portfolioWalletId) `dbShouldBeE` 1

    it "is 1 in a wallet with portfolios 0 and 2: the lowest free index" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        _ <- createPortfolio ctx (testSeed ctx) (portfolio.portfolioWalletId, portfolio.portfolioWallet) 2
        lift (nextPortfolioIndex (testSeed ctx) portfolio.portfolioWalletId) `dbShouldBeE` 1

    it "counts the portfolios of the wallet alone, not those of another wallet" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        _ <- nextPortfolioOf ctx testMnemonic
        other <- portfolioOf ctx testMnemonic2
        lift (nextPortfolioIndex (testSeed2 ctx) other.portfolioWalletId) `dbShouldBeE` 1

    it "passes over an index whose child key is invalid, as BIP32 asks" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift (nextPortfolioIndex (seedWithoutIndex1 ctx) portfolio.portfolioWalletId) `dbShouldBeE` 2

    it "is the index of a deleted portfolio again, which comes back with the same keys" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        second <- nextPortfolioOf ctx testMnemonic
        lift (nextPortfolioIndex (testSeed ctx) first.portfolioWalletId) `dbShouldBeE` 2
        lift $ deletePortfolio second.portfolioId
        lift (nextPortfolioIndex (testSeed ctx) first.portfolioWalletId) `dbShouldBeE` 1
        again <- nextPortfolioOf ctx testMnemonic
        (dBAccountXPubKey . snd <$> accountOf again btc Bip44) `dbShouldBeE` testAccountXPub1

{- Test networks -}

-- Every test network has the coin type 1, so the accounts of one type of
-- a portfolio on two of them are at the same path
testNetworksSpec :: Ctx -> Spec
testNetworksSpec ctx =
  describe "The accounts of a portfolio on the test networks" $ do
    it "hold the same key on btctest and on btctest4, in a row for each network" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (firstId, first) <- accountOf portfolio btcTest Bip44
        (secondId, second) <- accountOf portfolio btcTest4 Bip44
        liftIO $ do
          dBAccountXPubKey first `shouldBe` testAccountXPubTest
          dBAccountXPubKey second `shouldBe` testAccountXPubTest
          firstId `shouldNotBe` secondId

    it "count their addresses apart: an address generated on btctest is not generated on btctest4" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (firstId, _) <- accountOf portfolio btcTest Bip44
        (secondId, _) <- accountOf portfolio btcTest4 Bip44
        _ <- genExtAddress ctx def firstId ""
        ((\acc -> (dBAccountExternal acc, dBAccountInternal acc)) <$> getAccountById firstId) `dbShouldBeE` (1, 0)
        ((\acc -> (dBAccountExternal acc, dBAccountInternal acc)) <$> getAccountById secondId) `dbShouldBeE` (0, 0)
        lift (P.count [DBAddressAccount P.==. firstId]) `dbShouldBeE` 1
        lift (P.count [DBAddressAccount P.==. secondId]) `dbShouldBeE` 0

    it "are found by their key on the network asked, not on another that shares it" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        onTestnet3 <- accountOf portfolio btcTest Bip44
        onTestnet4 <- accountOf portfolio btcTest4 Bip44
        let key net = maybe (fail "The key does not read") return (xPubImport net ctx testAccountXPubTest)
        testnet3Key <- key btcTest
        testnet4Key <- key btcTest4
        lift (accountWithKey btcTest ctx testnet3Key) `dbShouldBeE` Just onTestnet3
        lift (accountWithKey btcTest4 ctx testnet4Key) `dbShouldBeE` Just onTestnet4

    -- 0x80000005 is the child number 5'
    it "are found by their key written with another child number, which is the same key" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        held <- accountOf portfolio btcTest Bip44
        key <- liftIO $ keyOf (testSeed ctx) Bip44 btcTest 0
        lift (accountWithKey btcTest ctx (renumbered 0x80000005 key)) `dbShouldBeE` Just held

    it "take on btctest4 the key the portfolio holds on btctest for the same path" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btcTest, Bip44)]
        key <- liftIO $ keyOf (testSeed ctx) Bip44 btcTest4 0
        (dBAccountXPubKey . snd <$> insertAccount portfolio.portfolioId Bip44 btcTest4 ctx key) `dbShouldBeE` testAccountXPubTest

    -- The portfolio holds the key of the test mnemonic on btctest, and is
    -- given that of the second test mnemonic on btctest4
    it "refuse a bip44 key on btctest4 other than the one the portfolio holds on btctest, naming the path and the networks" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btcTest, Bip44)]
        key <- liftIO $ keyOf (testSeed2 ctx) Bip44 btcTest4 0
        lift $
          dbShouldFailWith "The bip44 account /44'/1'/0' of portfolio0 of wallet0 has one key on btctest and would get another on btctest4: the networks of one path share its key" $
            insertAccount portfolio.portfolioId Bip44 btcTest4 ctx key

    -- The test networks of Bitcoin Cash have the coin type 1 too
    it "refuse a bip44 key on bchtest other than the one the portfolio holds on btctest, since they share the path" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btcTest, Bip44)]
        key <- liftIO $ keyOf (testSeed2 ctx) Bip44 bchTest 0
        lift $
          dbShouldFailWith "The bip44 account /44'/1'/0' of portfolio0 of wallet0 has one key on btctest and would get another on bchtest: the networks of one path share its key" $
            insertAccount portfolio.portfolioId Bip44 bchTest ctx key

    it "refuse a bip84 key on btcreg other than the one the portfolio holds on btctest4, naming the path and the networks" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btcTest4, Bip84)]
        key <- liftIO $ keyOf (testSeed2 ctx) Bip84 btcRegTest 0
        lift $
          dbShouldFailWith "The bip84 account /84'/1'/0' of portfolio0 of wallet0 has one key on btctest4 and would get another on btcreg: the networks of one path share its key" $
            insertAccount portfolio.portfolioId Bip84 btcRegTest ctx key

{- The account and the pool a command works on -}

choosingSpec :: Ctx -> Spec
choosingSpec ctx = do
  describe "The account of a portfolio a command works on" $ do
    it "is the bip44 account on btc without an option" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        chosenAccount def portfolio Nothing Nothing `dbShouldBeE` ("btc", 44)

    it "is on the network --network names" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        chosenAccount def portfolio (Just bch) Nothing `dbShouldBeE` ("bch", 44)

    it "is of the type --type names" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        chosenAccount def portfolio Nothing (Just Bip84) `dbShouldBeE` ("btc", 84)

    it "is on the network the key default-network names" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        chosenAccount (configWith ["default-network" .= ("btctest4" :: Text)]) portfolio Nothing Nothing `dbShouldBeE` ("btctest4", 44)

    it "is of the type the key default-type names" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        chosenAccount (configWith ["default-type" .= ("bip84" :: Text)]) portfolio Nothing Nothing `dbShouldBeE` ("btc", 84)

    it "takes --network over the key default-network" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        chosenAccount (configWith ["default-network" .= ("btctest4" :: Text)]) portfolio (Just btc) Nothing `dbShouldBeE` ("btc", 44)

    it "takes --type over the key default-type" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        chosenAccount (configWith ["default-type" .= ("bip84" :: Text)]) portfolio Nothing (Just Bip44) `dbShouldBeE` ("btc", 44)

    it "is the bip44 account on a network of Bitcoin Cash though the default type is bip84" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        chosenAccount (configWith ["default-type" .= ("bip84" :: Text)]) portfolio (Just bch) Nothing `dbShouldBeE` ("bch", 44)

    it "refuses --type bip84 on a network of Bitcoin Cash" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift $
          dbShouldFailWith "A bip84 account needs SegWit, which bch does not have" $
            chosenAccount def portfolio (Just bch) (Just Bip84)

  describe "The type of account of a command on a network" $ do
    it "is bip44 unless the configuration or --type say otherwise" $
      chooseKind def btc Nothing `shouldBe` Right Bip44

    it "ignores a bip84 default on every network of Bitcoin Cash, and is bip44 there" $ do
      let segwitByDefault = configWith ["default-type" .= ("bip84" :: Text)]
      (defaultKindOn segwitByDefault <$> [bch, bchTest, bchTest4, bchRegTest]) `shouldBe` [Bip44, Bip44, Bip44, Bip44]

    it "follows a bip84 default on every network of Bitcoin" $ do
      let segwitByDefault = configWith ["default-type" .= ("bip84" :: Text)]
      (defaultKindOn segwitByDefault <$> [btc, btcTest, btcTest4, btcRegTest]) `shouldBe` [Bip84, Bip84, Bip84, Bip84]

    it "refuses an explicit bip84 on every network of Bitcoin Cash" $ do
      chooseKind def bch (Just Bip84) `shouldBe` Left "A bip84 account needs SegWit, which bch does not have"
      chooseKind def bchTest (Just Bip84) `shouldBe` Left "A bip84 account needs SegWit, which bchtest does not have"
      chooseKind def bchTest4 (Just Bip84) `shouldBe` Left "A bip84 account needs SegWit, which bchtest4 does not have"
      chooseKind def bchRegTest (Just Bip84) `shouldBe` Left "A bip84 account needs SegWit, which bchreg does not have"

  describe "The pool of a portfolio a command works on" $ do
    it "is what the portfolio holds on btc without an option" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        choosePool def portfolio Nothing `dbShouldBeE` Pool portfolio.portfolioId btc

    it "is what the portfolio holds on the network --network names" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        choosePool def portfolio (Just bch) `dbShouldBeE` Pool portfolio.portfolioId bch

    it "is what the portfolio holds on the network the key default-network names" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        choosePool (configWith ["default-network" .= ("bch" :: Text)]) portfolio Nothing `dbShouldBeE` Pool portfolio.portfolioId bch

    it "has the accounts of the portfolio on the network, bip44 first" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (map (dBAccountPurpose . snd) <$> lift (poolAccounts (portfolioPool portfolio btc))) `dbShouldBeE` [44, 84]
        (map (dBAccountPurpose . snd) <$> lift (poolAccounts (portfolioPool portfolio bch))) `dbShouldBeE` [44]

    it "gives its account of a type, if the network has it" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        segwit <- accountOf portfolio btc Bip84
        lift (poolAccount (portfolioPool portfolio btc) Bip84) `dbShouldBeE` Just segwit
        lift (poolAccount (portfolioPool portfolio bch) Bip84) `dbShouldBeE` Nothing

    it "refuses the account of a type it does not have, naming the type and the network" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift $
          dbShouldFailWith "The portfolio has no bip84 account on bch" $
            poolAccountOf (portfolioPool portfolio bch) Bip84

{- Partial portfolios -}

-- The portfolio is as the step from hw 0.9 leaves it, with the bip44
-- account on btc alone
partialSpec :: Ctx -> Spec
partialSpec ctx =
  describe "A partial portfolio" $ do
    it "works with the accounts it has" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        (dBAccountXPubKey . snd <$> chooseAccount def portfolio Nothing Nothing) `dbShouldBeE` testAccountXPub

    it "refuses an account of a type it lacks, and says how to fill the portfolio in" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        lift $
          dbShouldFailWith ("portfolio0 of wallet0 has no bip84 account on btc yet. " <> fillInHint) $
            chooseAccount def portfolio Nothing (Just Bip84)

    it "refuses an account on a network it lacks, and says how to fill the portfolio in" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        lift $
          dbShouldFailWith ("portfolio0 of wallet0 has no bip44 account on bch yet. " <> fillInHint) $
            chooseAccount def portfolio (Just bch) Nothing

    it "has a pool on the networks of its accounts" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        choosePool def portfolio Nothing `dbShouldBeE` Pool portfolio.portfolioId btc
        lift (portfolioPools portfolio.portfolioId) `dbShouldBeE` [Pool portfolio.portfolioId btc]

    it "refuses the pool of a network it has no account on, and says how to fill the portfolio in" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        lift $
          dbShouldFailWith ("portfolio0 of wallet0 has no account on bch yet. " <> fillInHint) $
            choosePool def portfolio (Just bch)

    it "names its wallet and its own index in the refusal" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        portfolio <- migrated ctx (testSeed2 ctx) 3 "" [(bch, Bip44)]
        liftIO $ missingAccount portfolio btc (Just Bip44) `shouldBe` "portfolio3 of wallet1 has no bip44 account on btc yet. " <> fillInHint

    -- Twelve accounts less the one it has
    it "lacks every account of a whole portfolio but those it has" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        lacking portfolio.portfolioId `dbShouldBeE` drop 1 wholePortfolio
        lift (isPartial portfolio.portfolioId) `dbShouldBeE` True

    it "is what a whole portfolio is not: a whole one lacks no account" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lacking portfolio.portfolioId `dbShouldBeE` []
        lift (isPartial portfolio.portfolioId) `dbShouldBeE` False

{- Filling in -}

fillingSpec :: Ctx -> Spec
fillingSpec ctx =
  describe "Filling in a partial portfolio from its seed" $ do
    -- Twelve accounts less the one it has
    it "adds the accounts the portfolio lacks, and counts them" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        fillPortfolio ctx (testSeed ctx) portfolio `dbShouldBeE` 11
        lacking portfolio.portfolioId `dbShouldBeE` []
        lift (P.count [DBAccountPortfolio P.==. portfolio.portfolioId]) `dbShouldBeE` 12

    it "adds the accounts at the index of the portfolio" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 1 "" [(bch, Bip44)]
        _ <- fillPortfolio ctx (testSeed ctx) portfolio
        (dBAccountXPubKey . snd <$> accountOf portfolio btc Bip44) `dbShouldBeE` testAccountXPub1

    it "leaves the accounts the portfolio has as they are, with their addresses" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        (accId, _) <- accountOf portfolio btc Bip44
        replicateM_ 3 $ genExtAddress ctx def accId ""
        asItWas <- accountOf portfolio btc Bip44
        _ <- fillPortfolio ctx (testSeed ctx) portfolio
        accountOf portfolio btc Bip44 `dbShouldBeE` asItWas
        lift (P.count [DBAddressAccount P.==. accId]) `dbShouldBeE` 3

    it "adds nothing to a whole portfolio" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        fillPortfolio ctx (testSeed ctx) portfolio `dbShouldBeE` 0
        lift (P.count [DBAccountPortfolio P.==. portfolio.portfolioId]) `dbShouldBeE` 12

    -- The portfolio has the account of the second test mnemonic on bch,
    -- and the seed is that of the first
    it "refuses a seed that gives another key for an account the portfolio has" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed2 ctx) 0 "" [(bch, Bip44)]
        lift $
          dbShouldFailWith "The bip44 account of portfolio0 on bch is not of this seed: it has another key" $
            fillPortfolio ctx (testSeed ctx) portfolio

    it "adds nothing when it refuses the seed" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed2 ctx) 0 "" [(bch, Bip44)]
        lift $ dbShouldFail $ fillPortfolio ctx (testSeed ctx) portfolio
        accountsHeld portfolio.portfolioId `dbShouldBeE` [("bch", 44)]

    -- Two wallets of the test mnemonic that were not merged: the portfolio
    -- to fill in lacks the account on bch, which the other wallet has. The
    -- accounts on btc come before it among those to add.
    it "adds nothing when a key the portfolio lacks is held by another portfolio, which it names" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        _ <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        lift $
          dbShouldFailWith "portfolio0 of wallet1 already has this key on bch" $
            fillPortfolio ctx (testSeed ctx) portfolio
        accountsHeld portfolio.portfolioId `dbShouldBeE` [("btc", 44)]

    -- The empty portfolio 0 of a wallet the step from hw 0.9 made bears
    -- the date of its wallet
    it "dates a portfolio that had no account by the moment it is filled in" $
      inDatabaseE $ do
        (walletId, wallet) <- lift $ insertWallet Nothing
        (key, row) <- insertPortfolio walletId 0 ""
        lift $ P.update key [DBPortfolioCreated P.=. june2023]
        fillingTime <- liftIO getCurrentTime
        _ <- fillPortfolio ctx (testSeed ctx) (Portfolio key row walletId wallet)
        created <- createdOf key
        liftIO $ created `shouldSatisfy` (>= fillingTime)

    it "keeps the date of a portfolio that had an account when it is filled in" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        lift $ P.update portfolio.portfolioId [DBPortfolioCreated P.=. june2023]
        _ <- fillPortfolio ctx (testSeed ctx) portfolio
        createdOf portfolio.portfolioId `dbShouldBeE` june2023

    -- The wallet has portfolio 2 alone, with one account: portfolio 0 is
    -- made with its 12 accounts, and portfolio 2 gains the 11 it lacks
    it "fills in every portfolio of a wallet, and makes its portfolio 0 when it has none" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 2 "" [(btc, Bip44)]
        -- 12 + 11 = 23
        fillWallet ctx (testSeed ctx) (portfolio.portfolioWalletId, portfolio.portfolioWallet) `dbShouldBeE` 23
        (map (portfolioIndex . snd) <$> lift (getPortfolios portfolio.portfolioWalletId)) `dbShouldBeE` [0, 2]
        lift (isPartial portfolio.portfolioId) `dbShouldBeE` False

{- The wallet of a seed -}

-- hw 0.9 could not tell that its accounts on btc and on bch were of one
-- mnemonic: the step from its database leaves them in two wallets, each
-- with its portfolio 0, and neither with a fingerprint
adoptingSpec :: Ctx -> Spec
adoptingSpec ctx =
  describe "Taking over the wallets of a seed" $ do
    it "merges the wallets whose keys the seed derives into the one with the lowest index" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        _ <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        adopted <- adoptSeed ctx (testSeed ctx)
        liftIO $ do
          (walletIndex . snd . adoptedWallet <$> adopted) `shouldBe` Just 0
          (adoptedMerged <$> adopted) `shouldBe` Just [1]
        (map fst <$> walletsHeld) `dbShouldBeE` [0]
        accountsHeld kept.portfolioId `dbShouldBeE` [("btc", 44), ("bch", 44)]

    it "records the fingerprint of the seed on the wallet that stays" $
      inDatabaseE $ do
        _ <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        _ <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        adopted <- adoptSeed ctx (testSeed ctx)
        liftIO $ (dBWalletFingerprint . snd . adoptedWallet <$> adopted) `shouldBe` Just (Just "bc560f95")
        walletsHeld `dbShouldBeE` [(0, Just "bc560f95")]

    it "keeps the accounts of the merged wallets as they are, with their keys" $
      inDatabaseE $ do
        _ <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        moved <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        asItWas <- accountOf moved bch Bip44
        _ <- adoptSeed ctx (testSeed ctx)
        kept <- choosePortfolio def defaultPortfolio
        (accId, acc) <- accountOf kept bch Bip44
        liftIO $ do
          accId `shouldBe` fst asItWas
          dBAccountXPubKey acc `shouldBe` testAccountXPubBch

    -- wallet1 is of the second test mnemonic
    it "leaves a wallet of another mnemonic alone" $
      inDatabaseE $ do
        _ <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        other <- migrated ctx (testSeed2 ctx) 0 "" [(btc, Bip44)]
        _ <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        adopted <- adoptSeed ctx (testSeed ctx)
        liftIO $ (adoptedMerged <$> adopted) `shouldBe` Just [2]
        walletsHeld `dbShouldBeE` [(0, Just "bc560f95"), (1, Nothing)]
        accountsHeld other.portfolioId `dbShouldBeE` [("btc", 44)]
        (dBAccountXPubKey . snd <$> accountOf other btc Bip44) `dbShouldBeE` testAccountXPub2

    it "finds no wallet of a seed the database has no key of, and changes nothing" $
      inDatabaseE $ do
        _ <- migrated ctx (testSeed2 ctx) 0 "" [(btc, Bip44)]
        adoptSeed ctx (testSeed ctx) `dbShouldBeE` Nothing
        walletsHeld `dbShouldBeE` [(0, Nothing)]

    -- The second wallet has portfolio 1, which the first one lacks
    it "moves a portfolio the wallet that stays does not have to it, at its index" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        _ <- migrated ctx (testSeed ctx) 1 "" [(bch, Bip44)]
        _ <- adoptSeed ctx (testSeed ctx)
        (map portfolioRef <$> lift getAllPortfolios) `dbShouldBeE` [PortfolioRef 0 0, PortfolioRef 0 1]
        (map (portfolioIndex . snd) <$> lift (getPortfolios kept.portfolioWalletId)) `dbShouldBeE` [0, 1]

    -- The names of the accounts of hw 0.9 are the descriptions of their
    -- portfolios
    it "keeps what each of two merged portfolios was called, in one description" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "main" [(btc, Bip44)]
        _ <- migrated ctx (testSeed ctx) 0 "cash" [(bch, Bip44)]
        _ <- adoptSeed ctx (testSeed ctx)
        (dBPortfolioDescription . (.portfolioRow) <$> getPortfolio kept.portfolioId) `dbShouldBeE` "main, cash"

    -- The merged portfolio had bch alone, which the other had nothing on
    it "carries over the transactions, labels, pending transactions and synced block of a merged portfolio" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        moved <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        let movedPool = portfolioPool moved bch
            keptPool = portfolioPool kept bch
        _ <- lift $ repsertSummary movedPool (fakeTxHash 1) (Store.BlockRef 800000 1) fixtureTime paymentSummary
        lift $ setTxLabel movedPool (fakeTxHash 1) "rent"
        lift $ P.insert_ $ pendingRow movedPool (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 900)])
        lift $ updateSyncedBlock movedPool (fakeBlockHash 1) 800000
        _ <- adoptSeed ctx (testSeed ctx)
        (length <$> txsAfter keptPool Nothing Nothing) `dbShouldBeE` 1
        lift (txLabel keptPool (fakeTxHash 1)) `dbShouldBeE` "rent"
        (length <$> pendingTxs keptPool) `dbShouldBeE` 1
        syncedBlock keptPool `dbShouldBeE` Just (SyncedBlock (fakeBlockHash 1) 800000)

    -- Both portfolios have an account on btc, of the two types: their
    -- summaries were each made for one account alone
    it "drops the live summaries and the synced block of a network both merged portfolios were on, for the next sync to make them again, and keeps the labels" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        moved <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip84)]
        forM_ [(kept, 1), (moved, 2)] $ \(portfolio, n) -> do
          let pool = portfolioPool portfolio btc
          _ <- lift $ repsertSummary pool (fakeTxHash n) (Store.BlockRef 800000 1) fixtureTime paymentSummary
          lift $ setTxLabel pool (fakeTxHash n) "rent"
          lift $ updateSyncedBlock pool (fakeBlockHash n) 800000
        _ <- adoptSeed ctx (testSeed ctx)
        let pool = portfolioPool kept btc
        accountsHeld kept.portfolioId `dbShouldBeE` [("btc", 44), ("btc", 84)]
        txsAfter pool Nothing Nothing `dbShouldBeE` []
        syncedBlock pool `dbShouldBeE` Nothing
        lift (txLabel pool (fakeTxHash 1)) `dbShouldBeE` "rent"
        lift (txLabel pool (fakeTxHash 2)) `dbShouldBeE` "rent"

    -- Both portfolios have an account on btc. The store no longer lists a
    -- dead transaction, so no sync would make its summary again. The live
    -- summary of transaction 3 is dropped, for the next sync.
    it "keeps the dead summaries of a network both merged portfolios were on, in the portfolio that stays" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        moved <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip84)]
        forM_ [(kept, 1), (moved, 2)] $ \(portfolio, n) -> do
          let pool = portfolioPool portfolio btc
          _ <- lift $ repsertSummary pool (fakeTxHash n) (Store.MemRef 0) fixtureTime paymentSummary
          lift $ markTxInfoDead pool (fakeTxHash n)
        _ <- lift $ repsertSummary (portfolioPool moved btc) (fakeTxHash 3) (Store.BlockRef 800000 1) fixtureTime paymentSummary
        _ <- adoptSeed ctx (testSeed ctx)
        summariesHeld (portfolioPool kept btc) `dbShouldBeE` [(fakeTxHash 1, True), (fakeTxHash 2, True)]

    -- The summary of the portfolio that stays is at block 800000, that of
    -- the other at block 800001
    it "keeps the dead summary of the portfolio that stays when both merged portfolios have the transaction dead" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        moved <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip84)]
        forM_ [(kept, 800000), (moved, 800001)] $ \(portfolio, height) -> do
          let pool = portfolioPool portfolio btc
          _ <- lift $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef height 1) fixtureTime paymentSummary
          lift $ markTxInfoDead pool (fakeTxHash 1)
        _ <- adoptSeed ctx (testSeed ctx)
        let pool = portfolioPool kept btc
        summariesHeld pool `dbShouldBeE` [(fakeTxHash 1, True)]
        (fmap (S.decode . dBTxInfoBlockRef) <$> lift (P.get (txInfoKey pool (fakeTxHash 1)))) `dbShouldBeE` Just (Right (Store.BlockRef 800000 1))

    -- The step from hw 0.9 gives the wallet of an account at index 1 an
    -- empty portfolio 0, dated by its wallet: 1 Jun 2023. It is wallet0,
    -- the older one. Portfolio 0 of the other wallet has an account, made
    -- on 1 Jan 2024.
    it "dates a merged portfolio by the one that has accounts, not by an empty one older than it" $
      inDatabaseE $ do
        cash <- migrated ctx (testSeed ctx) 1 "" [(bch, Bip44)]
        (emptyKey, _) <- insertPortfolio cash.portfolioWalletId 0 ""
        main <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        lift $ do
          P.update emptyKey [DBPortfolioCreated P.=. june2023]
          P.update cash.portfolioId [DBPortfolioCreated P.=. june2023]
          P.update main.portfolioId [DBPortfolioCreated P.=. january2024]
        _ <- adoptSeed ctx (testSeed ctx)
        accountsHeld emptyKey `dbShouldBeE` [("btc", 44)]
        createdOf emptyKey `dbShouldBeE` january2024

    -- The account on btc was made on 1 Jan 2024, the one on bch on 1 Jun
    -- 2023
    it "dates a merged portfolio by the older of the two when both have accounts" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        moved <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        lift $ do
          P.update kept.portfolioId [DBPortfolioCreated P.=. january2024]
          P.update moved.portfolioId [DBPortfolioCreated P.=. june2023]
        _ <- adoptSeed ctx (testSeed ctx)
        createdOf kept.portfolioId `dbShouldBeE` june2023

    it "keeps what each of two merged wallets was called, in one description" $
      inDatabaseE $ do
        kept <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        moved <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        setWalletDescription kept.portfolioWalletId "laptop"
        setWalletDescription moved.portfolioWalletId "phone"
        _ <- adoptSeed ctx (testSeed ctx)
        (fmap (dBWalletDescription . snd) <$> lift (walletAt 0)) `dbShouldBeE` Just "laptop, phone"

    -- The step from hw 0.9 leaves the accounts of one mnemonic on btc, on
    -- bch and on the test networks in three wallets
    it "keeps what each of three merged wallets was called, in one description, in the order of their indexes" $
      inDatabaseE $ do
        first <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        second <- migrated ctx (testSeed ctx) 0 "" [(bch, Bip44)]
        third <- migrated ctx (testSeed ctx) 0 "" [(btcTest, Bip44)]
        setWalletDescription first.portfolioWalletId "laptop"
        setWalletDescription second.portfolioWalletId "phone"
        setWalletDescription third.portfolioWalletId "tablet"
        _ <- adoptSeed ctx (testSeed ctx)
        (fmap (dBWalletDescription . snd) <$> lift (walletAt 0)) `dbShouldBeE` Just "laptop, phone, tablet"

    it "joins descriptions each once, leaving the empty ones out" $ do
      joinDescriptions ["main", "", "cash", "main"] `shouldBe` "main, cash"
      joinDescriptions ["", ""] `shouldBe` ""

    -- The keys decide, not the fingerprint, which is 32 bits of a hash:
    -- the wallet has the fingerprint of the test mnemonic and a key of
    -- the second one
    it "refuses a wallet that has the fingerprint of the seed and keys the seed does not derive" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        (portfolioKey, _) <- insertPortfolio walletId 0 ""
        foreignKey <- liftIO $ keyOf (testSeed2 ctx) Bip44 btc 0
        _ <- insertAccount portfolioKey Bip44 btc ctx foreignKey
        lift $
          dbShouldFailWith "wallet0 has the fingerprint of this seed and other keys: it is of another seed with the same fingerprint" $
            adoptSeed ctx (testSeed ctx)

    -- The wallet has the btc account of the test mnemonic and the bch
    -- account of the second one
    it "refuses a wallet that holds keys of the seed among keys of another seed" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "" [(btc, Bip44)]
        foreignKey <- liftIO $ keyOf (testSeed2 ctx) Bip44 bch 0
        _ <- insertAccount portfolio.portfolioId Bip44 bch ctx foreignKey
        lift $
          dbShouldFailWith "wallet0 holds keys of this seed among keys of another: its accounts are not of one seed" $
            adoptSeed ctx (testSeed ctx)

{- Listing -}

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "Listing the accounts" $ do
    -- wallet0 has portfolios 0 and 1, and wallet1 portfolio 0. The
    -- accounts on btc stand for the twelve of each portfolio.
    it "lists every account with its portfolio, in the order of the wallets, of their portfolios, then of the types" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        _ <- portfolioOf ctx testMnemonic2
        _ <- nextPortfolioOf ctx testMnemonic
        accounts <- lift getAccounts
        liftIO $ do
          length accounts `shouldBe` 36
          [(portfolioRef portfolio, dBAccountPurpose acc) | (portfolio, _, acc) <- accounts, dBAccountNetwork acc == "btc"]
            `shouldBe` [ (PortfolioRef 0 0, 44),
                         (PortfolioRef 0 0, 84),
                         (PortfolioRef 0 1, 44),
                         (PortfolioRef 0 1, 84),
                         (PortfolioRef 1 0, 44),
                         (PortfolioRef 1 0, 84)
                       ]

    it "gives a whole portfolio a pool on each of its eight networks, Bitcoin and Bitcoin Cash first" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (map poolNetworkName <$> lift (portfolioPools portfolio.portfolioId))
          `dbShouldBeE` ["btc", "bch", "btctest", "btctest4", "btcreg", "bchtest", "bchtest4", "bchreg"]

    it "finds every pool of the database on a network, each once though it has two accounts there" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        second <- portfolioOf ctx testMnemonic2
        (sortOn poolPortfolio <$> lift (poolsOnNetwork btc)) `dbShouldBeE` [Pool first.portfolioId btc, Pool second.portfolioId btc]

    -- The bip44 and the bip84 account of each of two portfolios
    it "finds every account of the database on a network" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        _ <- portfolioOf ctx testMnemonic2
        (length <$> lift (accountsOnNetwork btc)) `dbShouldBeE` 4
        (length <$> lift (accountsOnNetwork bch)) `dbShouldBeE` 2

    it "gives the accounts of the ids, in their order" $
      inDatabaseE $ do
        legacy <- testNewAcc ctx
        segwit <- testNewSegwitAcc ctx
        getAccountsById [fst segwit, fst legacy] `dbShouldBeE` [segwit, legacy]

    it "gives the portfolio of an account, and how the answers name the account" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        portfolio <- nextPortfolioOf ctx testMnemonic
        (_, acc) <- accountOf portfolio bch Bip44
        portfolioOfAccount acc `dbShouldBeE` portfolio
        accountRefOf acc `dbShouldBeE` AccountRef (Place (PortfolioRef 0 1) bch) Bip44

{- Deleting -}

-- Each portfolio holds, on Bitcoin, an address, a coin, a summary, a
-- label, a pending transaction and a synced block, beside its 12 accounts
deletingSpec :: Ctx -> Spec
deletingSpec ctx = do
  describe "Deleting a portfolio" $ do
    it "deletes it with its accounts, addresses, coins, summaries, labels of transactions, pending transactions and synced blocks" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        second <- nextPortfolioOf ctx testMnemonic
        filled ctx second account1Addr 2
        accounts <- accountIds second
        rowsOf second accounts `dbShouldBeE` (12, 1, 1, 1, 1, 1, 1)
        lift $ deletePortfolio second.portfolioId
        rowsOf second accounts `dbShouldBeE` (0, 0, 0, 0, 0, 0, 0)
        (map (portfolioIndex . snd) <$> lift (getPortfolios first.portfolioWalletId)) `dbShouldBeE` [0]

    it "deletes nothing of another portfolio" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        second <- nextPortfolioOf ctx testMnemonic
        filled ctx first (externalAddr 0) 1
        filled ctx second account1Addr 2
        accounts <- accountIds first
        lift $ deletePortfolio second.portfolioId
        rowsOf first accounts `dbShouldBeE` (12, 1, 1, 1, 1, 1, 1)

  describe "Deleting a wallet" $ do
    it "deletes it with all of its portfolios and what they hold" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        second <- nextPortfolioOf ctx testMnemonic
        filled ctx first (externalAddr 0) 1
        filled ctx second account1Addr 2
        firstAccounts <- accountIds first
        secondAccounts <- accountIds second
        lift $ deleteWallet first.portfolioWalletId
        rowsOf first firstAccounts `dbShouldBeE` (0, 0, 0, 0, 0, 0, 0)
        rowsOf second secondAccounts `dbShouldBeE` (0, 0, 0, 0, 0, 0, 0)
        lift getAllPortfolios `dbShouldBeE` []
        walletsHeld `dbShouldBeE` []

    it "deletes nothing of another wallet" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        other <- portfolioOf ctx testMnemonic2
        filled ctx first (externalAddr 0) 1
        filled ctx other externalAddr2 2
        accounts <- accountIds other
        lift $ deleteWallet first.portfolioWalletId
        rowsOf other accounts `dbShouldBeE` (12, 1, 1, 1, 1, 1, 1)
        (map portfolioRef <$> lift getAllPortfolios) `dbShouldBeE` [PortfolioRef 1 0]
        walletsHeld `dbShouldBeE` [(1, Just "4435aa1c")]

{- Activity -}

-- deleteportfolio deletes only a portfolio that never had any, and gave
-- out no address
activitySpec :: Ctx -> Spec
activitySpec ctx =
  describe "Whether a portfolio has had any activity" $ do
    it "is false for a new portfolio" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift (portfolioHasActivity portfolio.portfolioId) `dbShouldBeE` False

    -- Someone may pay the address later: deleting the portfolio would
    -- leave that payment unseen
    it "is true once receive gave out one of its addresses, though nobody paid it" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (accId, _) <- accountOf portfolio btc Bip44
        _ <- genExtAddress ctx def accId "Rent"
        lift (portfolioHasActivity portfolio.portfolioId) `dbShouldBeE` True

    -- A payment prepared here may be signed and sent elsewhere
    it "is true once a payment took one of its change addresses" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (accId, _) <- accountOf portfolio btc Bip84
        _ <- nextChangeAddress ctx def accId []
        lift (portfolioHasActivity portfolio.portfolioId) `dbShouldBeE` True

    it "is true once one of its addresses was paid, on whatever network" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (accId, _) <- accountOf portfolio bch Bip44
        _ <- genExtAddress ctx def accId ""
        updateAddressBalances bch accId [paidOnce bchExternalAddr 1000]
        lift (portfolioHasActivity portfolio.portfolioId) `dbShouldBeE` True

    it "is true once it has a coin" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (accId, _) <- accountOf portfolio btc Bip84
        _ <- refreshCoins btc ctx accId [segwitExternalAddr 0] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (segwitExternalAddr 0) 1000]
        lift (portfolioHasActivity portfolio.portfolioId) `dbShouldBeE` True

    it "is true once it has a transaction in its history" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        _ <- lift $ repsertSummary (portfolioPool portfolio btc) (fakeTxHash 1) (Store.BlockRef 800000 1) fixtureTime paymentSummary
        lift (portfolioHasActivity portfolio.portfolioId) `dbShouldBeE` True

    it "is true though the only transaction of its history is dead" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        _ <- lift $ repsertSummary (portfolioPool portfolio btc) (fakeTxHash 1) (Store.MemRef 0) fixtureTime paymentSummary
        lift $ markTxInfoDead (portfolioPool portfolio btc) (fakeTxHash 1)
        lift (portfolioHasActivity portfolio.portfolioId) `dbShouldBeE` True

    it "is true once it has a pending transaction" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift $ P.insert_ $ pendingRow (portfolioPool portfolio btc) (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 900)])
        lift (portfolioHasActivity portfolio.portfolioId) `dbShouldBeE` True

    it "is its own: the activity of another portfolio of the wallet does not count" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        second <- nextPortfolioOf ctx testMnemonic
        filled ctx first (externalAddr 0) 1
        lift (portfolioHasActivity second.portfolioId) `dbShouldBeE` False

{- Balances -}

balanceSpec :: Ctx -> Spec
balanceSpec ctx =
  describe "The balances of an account" $ do
    it "are zero without an address" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        acc <- lift $ updateAccountBalances accId
        liftIO $ balances acc `shouldBe` (0, 0, 0)

    it "are shown by portfolios with the balance the portfolio can spend now on the network" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInFile (dir </> "wallet.sqlite") $ do
          (accId, _) <- testNewAcc ctx
          _ <- genExtAddress ctx def accId ""
          refreshCoins btc ctx accId [externalAddr 0] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000]
        shownSpendable <$> commandResponse ctx wallet UnitBitcoin (CommandPortfolios defaultPortfolio (OnScreen Nothing))
          `shouldReturn` Right [("btc", 50000)]

    it "are shown by hw addresses with the balance each address can spend now" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInFile (dir </> "wallet.sqlite") $ do
          (accId, _) <- testNewAcc ctx
          _ <- genExtAddress ctx def accId ""
          _ <- genExtAddress ctx def accId ""
          refreshCoins btc ctx accId [externalAddr 0] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000]
        -- Newest first: the second address holds nothing
        addressSpendable <$> commandResponse ctx wallet UnitBitcoin (CommandAddresses (Target defaultPortfolio (Just btc) (Just Bip44)) False (OnScreen Nothing) (Paging Nothing Nothing))
          `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 0), ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", 50000)]

    it "add up those of its addresses" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        first <- genExtAddress ctx def accId ""
        second <- genExtAddress ctx def accId ""
        lift $ P.update (key first) [DBAddressBalanceConfirmed P.=. 5000, DBAddressBalanceUnconfirmed P.=. 300, DBAddressBalanceCoins P.=. 2]
        lift $ P.update (key second) [DBAddressBalanceConfirmed P.=. 2000, DBAddressBalanceCoins P.=. 1]
        acc <- lift $ updateAccountBalances accId
        liftIO $ balances acc `shouldBe` (7000, 300, 3)

    -- 5000000 confirmed and 100000 unconfirmed
    it "give what the account holds, confirmed or not" $
      accountTotal mainAccountRow `shouldBe` 5100000
  where
    balances acc = (dBAccountBalanceConfirmed acc, dBAccountBalanceUnconfirmed acc, dBAccountBalanceCoins acc)
    key addr = DBAddressKey (dBAddressAccount addr) (dBAddressInternal addr) (dBAddressIndex addr)

-- | What portfolios answered: the balance each portfolio can spend now on
-- each network where it can spend something, by the name of the network
shownSpendable :: Response -> Either String [(String, Word64)]
shownSpendable (ResponsePortfolios shown) =
  Right [((networkViewNetwork figures).name, networkViewSpendable figures) | view <- shown, figures <- viewNetworks view, networkViewSpendable figures > 0]
shownSpendable other = Left (show other)

-- | What `addresses` answered: each address by its text, with the balance it
-- can spend now
addressSpendable :: Response -> Either String [(Text, Word64)]
addressSpendable (ResponseAddresses groups) = Right [(dBAddressAddress addr, spendable) | group <- groups, (addr, spendable) <- groupRows group]
addressSpendable other = Left (show other)

{- A portfolio as portfolios shows it -}

-- | The figures of a view on a network
figuresOn :: Network -> PortfolioView -> [NetworkView]
figuresOn net view = [figures | figures <- viewNetworks view, networkViewNetwork figures == net]

-- | The view of portfolio 0 of the test wallet as the database has it
-- now, nothing spendable
viewNow :: Ctx -> ExceptT String (DB IO) PortfolioView
viewNow ctx = do
  portfolio <- testPortfolio ctx
  portfolioView portfolio (\_ -> return 0)

-- | Portfolio 0 of the test wallet with addresses and balances on
-- Bitcoin, as a sync leaves them. Its bip44 account has two receiving
-- addresses and a change address, and holds 5000 confirmed and 300
-- unconfirmed in 2 coins. Its bip84 account has one receiving address,
-- and holds 7000 confirmed in 1 coin.
paidOnBothTypes :: Ctx -> ExceptT String (DB IO) ()
paidOnBothTypes ctx = do
  (legacyId, _) <- testNewAcc ctx
  (segwitId, _) <- testNewSegwitAcc ctx
  replicateM_ 2 $ genExtAddress ctx def legacyId ""
  _ <- nextChangeAddress ctx def legacyId []
  _ <- genExtAddress ctx def segwitId ""
  updateAddressBalances btc legacyId [Store.Balance (externalAddr 0) 5000 300 2 3 9000]
  updateAddressBalances btc segwitId [Store.Balance (segwitExternalAddr 0) 7000 0 1 1 7000]
  void $ lift $ updateAccountBalances legacyId
  void $ lift $ updateAccountBalances segwitId

viewSpec :: Ctx -> Spec
viewSpec ctx =
  describe "A portfolio as portfolios shows it" $ do
    it "has one set of figures for each network of the portfolio, Bitcoin and Bitcoin Cash first" $
      inDatabaseE $ do
        view <- viewNow ctx
        liftIO $
          ((.name) . networkViewNetwork <$> viewNetworks view)
            `shouldBe` ["btc", "bch", "btctest", "btctest4", "btcreg", "bchtest", "bchtest4", "bchreg"]

    it "adds up the balances of the bip44 and of the bip84 account on btc" $
      inDatabaseE $ do
        paidOnBothTypes ctx
        view <- viewNow ctx
        -- 5000 + 7000 = 12000 confirmed, 300 + 0 unconfirmed
        liftIO $ [(networkViewConfirmed figures, networkViewUnconfirmed figures) | figures <- figuresOn btc view] `shouldBe` [(12000, 300)]

    it "counts the coins of the bip44 and of the bip84 account on btc together" $
      inDatabaseE $ do
        paidOnBothTypes ctx
        view <- viewNow ctx
        -- 2 + 1 = 3
        liftIO $ (networkViewCoins <$> figuresOn btc view) `shouldBe` [3]

    it "counts the addresses of the bip44 and of the bip84 account on btc together, receiving and change ones" $
      inDatabaseE $ do
        paidOnBothTypes ctx
        view <- viewNow ctx
        -- 2 receiving + 1 change of the bip44 account, 1 receiving of the bip84 one
        liftIO $ (networkViewAddresses <$> figuresOn btc view) `shouldBe` [4]

    it "keeps the figures of each network apart: what btc holds is not on bch" $
      inDatabaseE $ do
        paidOnBothTypes ctx
        view <- viewNow ctx
        liftIO $
          [(networkViewConfirmed figures, networkViewUnconfirmed figures, networkViewCoins figures, networkViewAddresses figures) | figures <- figuresOn bch view]
            `shouldBe` [(0, 0, 0, 0)]

    it "has both types on btc, and bip44 alone on bch" $
      inDatabaseE $ do
        view <- viewNow ctx
        liftIO $ do
          [keysKind <$> networkViewAccounts figures | figures <- figuresOn btc view] `shouldBe` [[Bip44, Bip84]]
          [keysKind <$> networkViewAccounts figures | figures <- figuresOn bch view] `shouldBe` [[Bip44]]

    -- A confirmed coin of 50000 at the first bip44 address and one of
    -- 30000 at the first bip84 address
    it "gives what the portfolio can spend now on a network, the coins of both types together" $
      inDatabaseE $ do
        (legacyId, _) <- testNewAcc ctx
        (segwitId, _) <- testNewSegwitAcc ctx
        _ <- refreshCoins btc ctx legacyId [externalAddr 0] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000]
        _ <- refreshCoins btc ctx segwitId [segwitExternalAddr 0] [storeCoin ctx (fakeTxHash 2, 0) (Just 800000) (segwitExternalAddr 0) 30000]
        portfolio <- testPortfolio ctx
        view <- portfolioView portfolio spendableBalance
        -- 50000 + 30000 = 80000
        liftIO $ do
          (networkViewSpendable <$> figuresOn btc view) `shouldBe` [80000]
          (networkViewSpendable <$> figuresOn bch view) `shouldBe` [0]

    -- The descriptors and the zpub are those of test/data/descriptors,
    -- their checksums computed apart from hw. SLIP-132 gives a bip44
    -- account the xpub prefix of BIP32.
    it "gives the keys of each account as another wallet takes them: path, key, descriptor, and the SLIP-132 key, the xpub of a bip44 account" $
      inDatabaseE $ do
        view <- viewNow ctx
        liftIO $
          concatMap networkViewAccounts (figuresOn btc view)
            `shouldBe` [ AccountKeys
                           Bip44
                           (Deriv :| 44 :| 0 :| 0)
                           testAccountXPub
                           "pkh([bc560f95/44h/0h/0h]xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah/<0;1>/*)#vq597j87"
                           (Just "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah"),
                         AccountKeys
                           Bip84
                           (Deriv :| 84 :| 0 :| 0)
                           testSegwitAccountXPub
                           ("wpkh([bc560f95/84h/0h/0h]" <> testSegwitAccountXPub <> "/<0;1>/*)#5nktextl")
                           (Just testSegwitAccountZPub)
                       ]

    -- The portfolio is given the creation date 'seenTime', 14 Nov 2023
    it "names the portfolio, and says what the user and the database say of it" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        setPortfolioDescription portfolio.portfolioId "savings"
        setWalletDescription portfolio.portfolioWalletId "steel backup"
        lift $ P.update portfolio.portfolioId [DBPortfolioCreated P.=. seenTime]
        view <- viewNow ctx
        liftIO $ do
          viewRef view `shouldBe` PortfolioRef 0 0
          viewDescription view `shouldBe` "savings"
          viewWalletDescription view `shouldBe` "steel backup"
          viewFingerprint view `shouldBe` Just testFingerprint
          viewCreated view `shouldBe` seenTime

    it "lists no missing account for a whole portfolio" $
      inDatabaseE $ do
        view <- viewNow ctx
        liftIO $ (viewMissing view, viewPartial view) `shouldBe` ([], False)

    -- As the step from hw 0.9 leaves a portfolio: one account, no fingerprint
    it "shows a partial portfolio with the networks it has, the accounts it lacks and no fingerprint" $
      inDatabaseE $ do
        portfolio <- migrated ctx (testSeed ctx) 0 "cash" [(bch, Bip44)]
        view <- portfolioView portfolio (\_ -> return 0)
        liftIO $ do
          ((.name) . networkViewNetwork <$> viewNetworks view) `shouldBe` ["bch"]
          viewPartial view `shouldBe` True
          -- Twelve accounts less the one it has
          [(cs net.name, kindName kind) | (net, kind) <- viewMissing view] `shouldBe` filter (/= ("bch", "bip44")) wholePortfolio
          viewFingerprint view `shouldBe` Nothing

    it "names networks as a sentence does" $ do
      networksText [btc] `shouldBe` "btc"
      networksText [btc, bch] `shouldBe` "btc and bch"
      networksText [btc, bch, btcTest] `shouldBe` "btc, bch and btctest"

{- JSON -}

-- | Portfolio 1 of wallet 0 as portfolios shows it: described, partial for
-- lack of its bch account, holding on btc 5000 confirmed and 300
-- unconfirmed in 2 coins, 4800 of it spendable, at 3 addresses, with the
-- keys of its two accounts there, which are those of account 0 of the test
-- wallet
fixtureView :: PortfolioView
fixtureView =
  PortfolioView
    { viewRef = PortfolioRef 0 1,
      viewDescription = "savings",
      viewWalletDescription = "steel backup",
      viewFingerprint = Just testFingerprint,
      viewCreated = fixtureTime,
      viewMissing = [(bch, Bip44)],
      viewNetworks =
        [ NetworkView
            { networkViewNetwork = btc,
              networkViewConfirmed = 5000,
              networkViewUnconfirmed = 300,
              networkViewSpendable = 4800,
              networkViewCoins = 2,
              networkViewAddresses = 3,
              networkViewAccounts =
                [ AccountKeys Bip44 (Deriv :| 44 :| 0 :| 0) testAccountXPub "pkh(first)" (Just testAccountXPub),
                  AccountKeys Bip84 (Deriv :| 84 :| 0 :| 0) testSegwitAccountXPub "wpkh(second)" (Just testSegwitAccountZPub)
                ]
            }
        ]
    }

viewJson :: String
viewJson =
  [r|{
  "wallet": 0,
  "portfolio": 1,
  "name": "portfolio1",
  "description": "savings",
  "walletdescription": "steel backup",
  "fingerprint": "bc560f95",
  "created": "2023-11-01T00:00:00Z",
  "partial": true,
  "missing": [{"network": "bch", "type": "bip44"}],
  "networks": [
    {
      "network": "btc",
      "balance": {"confirmed": 5000, "unconfirmed": 300, "spendable": 4800, "coins": 2},
      "addresses": 3,
      "accounts": [
        {
          "type": "bip44",
          "derivation": "/44'/0'/0'",
          "xPubKey": "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah",
          "descriptor": "pkh(first)",
          "slip132": "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah"
        },
        {
          "type": "bip84",
          "derivation": "/84'/0'/0'",
          "xPubKey": "xpub6CQF8An9YzRXpzoY1EtZNfzJyZHw4ZLJPymPJQ5N2Huj693bio75z3fhNUq6unam47GaZQanRezUbkhMZosNyZBrGT9NExM1fa8uwdaWnoV",
          "descriptor": "wpkh(second)",
          "slip132": "zpub6r4mjW7yrMWVXbBmfxTonrBKKVapwoKJECopsBs8nJfVCLg4E7SDEAyyQtkGubtbsPWC4MmuLyhaNKvV1ChQa2Z418YDQmyzD2GCiogfbAh"
        }
      ]
    }
  ]
}|]

jsonSpec :: Spec
jsonSpec =
  describe "A portfolio in JSON" $ do
    it "is written as portfolios prints it, its figures by network, and read back" $ do
      eitherDecode (cs viewJson) `shouldBe` Right (toJSON fixtureView)
      eitherDecode (cs viewJson) `shouldBe` Right fixtureView

    it "writes the fingerprint of a wallet that has none yet as null, and reads it back" $ do
      let unknown = fixtureView {viewFingerprint = Nothing}
          json = Text.replace "\"fingerprint\": \"bc560f95\"" "\"fingerprint\": null" (cs viewJson)
      eitherDecode (cs json) `shouldBe` Right (toJSON unknown)
      eitherDecode (cs json) `shouldBe` Right unknown

    -- A file given to hw may name any network
    it "refuses a portfolio on a network hw does not know, naming it" $
      (eitherDecode (cs (Text.replace "\"network\": \"btc\"" "\"network\": \"nope\"" (cs viewJson))) :: Either String PortfolioView)
        `shouldBe` Left "Error in $.networks[0]: Unknown network nope"
