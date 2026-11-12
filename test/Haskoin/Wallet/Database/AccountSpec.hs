{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | The accounts of the portfolios. A portfolio holds twelve accounts,
-- made together with it from its seed or from the keys of a file. A
-- command reaches one of them by the network and the type its options or
-- the configuration give. The wallet of a seed is the one with its
-- fingerprint, and its keys must be those of the seed. A portfolio is
-- deleted with everything it holds, and shown with one set of figures
-- for each network.
module Haskoin.Wallet.Database.AccountSpec (spec) where

import Control.Monad (replicateM_, void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson (eitherDecode, toJSON, (.=))
import Data.Default (def)
import Data.Either (isLeft)
import Data.List (sortOn)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Word (Word64, Word8)
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
import Haskoin.Wallet.Database.BestBlock (updateSyncedBlock)
import Haskoin.Wallet.Database.Coin (refreshCoins, spendableBalance)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo (markTxInfoDead, repsertSummary)
import Haskoin.Wallet.Database.TxLabel (setTxLabel)
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
  seedWalletSpec ctx
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

-- | The accounts of a portfolio as its rows hold them, each by the name of
-- its network and its path, in the order hw lists them
accountPaths :: Portfolio -> ExceptT String (DB IO) [(Text, String)]
accountPaths portfolio =
  map (\(_, acc) -> (dBAccountNetwork acc, pathToStr (accountPath portfolio acc))) <$> lift (portfolioAccountRows portfolio.portfolioId)

-- | The account a command works on, by the name of its network and its
-- purpose
chosenAccount :: Config -> Portfolio -> Maybe Network -> Maybe AccountKind -> ExceptT String (DB IO) (Text, Int)
chosenAccount cfg portfolio netM kindM = (\(_, acc) -> (dBAccountNetwork acc, dBAccountPurpose acc)) <$> chooseAccount cfg portfolio netM kindM

-- | The keys of the twelve accounts of a portfolio of a seed, at an index
-- that has them, as the indexes of these tests all have, with the key of
-- one account replaced when given: the input of a portfolio created from
-- the keys of a file
portfolioKeysOf :: PublicSeed -> Natural -> IO [(Network, AccountKind, XPubKey)]
portfolioKeysOf seed index = maybe (fail "The seed has no keys at this index") return =<< seedPortfolioKeys seed index

-- | The keys given, with the key of the account of the network and the
-- type replaced
withKeyOf :: Network -> AccountKind -> XPubKey -> [(Network, AccountKind, XPubKey)] -> [(Network, AccountKind, XPubKey)]
withKeyOf net kind key keys = [(n, k, if n == net && k == kind then key else old) | (n, k, old) <- keys]

-- | The indexes of the portfolios of the database, each with the index of
-- its wallet
portfoliosHeld :: ExceptT String (DB IO) [(Natural, Natural)]
portfoliosHeld = map (\p -> (walletIndex p.portfolioWallet, portfolioIndex p.portfolioRow)) <$> lift getAllPortfolios

-- | The wallets of the database, each by its index and its fingerprint
walletsHeld :: ExceptT String (DB IO) [(Natural, Text)]
walletsHeld = map (\(_, wallet) -> (walletIndex wallet, dBWalletFingerprint wallet)) <$> lift getWallets

-- | Give what a portfolio holds on Bitcoin one row of every kind: a
-- receiving address of its bip44 account, a coin at the address given,
-- and a summary, a label, a pending transaction and a synced block around
-- the transaction of the number
filled :: Ctx -> Portfolio -> Address -> Word8 -> ExceptT String (DB IO) ()
filled ctx portfolio addr n = do
  let pool = portfolioPool portfolio btc
  (accId, _) <- accountOf portfolio btc Bip44
  _ <- genExtAddress ctx def accId ""
  _ <- refreshCoins btc ctx accId (const seenTime) [addr] [storeCoin ctx (fakeTxHash n, 0) (Just 800000) addr 1000]
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
creatingSpec ctx = do
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

    -- newportfolio and recover create their portfolios this way
    it "makes the first portfolio created in the database the active one" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift activePortfolioId `dbShouldBeE` Just portfolio.portfolioId

    it "leaves the active portfolio as it is when it creates another one" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        _ <- nextPortfolioOf ctx testMnemonic
        lift activePortfolioId `dbShouldBeE` Just first.portfolioId

    it "files the portfolio under the wallet of the seed, which keeps the fingerprint" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        liftIO $ do
          portfolioRef portfolio `shouldBe` PortfolioRef 0 0
          portfolioFingerprint portfolio `shouldBe` testFingerprint

    it "tells the kind and the pool of an account from its row" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (_, acc) <- accountOf portfolio btc Bip84
        liftIO $ do
          accountKind acc `shouldBe` Bip84
          dBAccountPurpose acc `shouldBe` 84
          accountPool acc `shouldBe` Pool portfolio.portfolioId btc
          accountRef portfolio acc `shouldBe` AccountRef (Place (PortfolioRef 0 0) btc) Bip84

    -- Every portfolio is dated as it is created, with its accounts
    it "dates the portfolio at the moment it is created" $
      inDatabaseE $ do
        started <- liftIO getCurrentTime
        portfolio <- testPortfolio ctx
        ended <- liftIO getCurrentTime
        liftIO $ dBPortfolioCreated portfolio.portfolioRow `shouldSatisfy` (\created -> started <= created && created <= ended)

  describe "Creating a whole portfolio from the keys of a file" $ do
    it "gives portfolio 1 of the wallet the twelve accounts of the keys" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 1
        second <- createPortfolioWith ctx (first.portfolioWalletId, first.portfolioWallet) 1 "spending" keys
        accountPaths second `dbShouldBeE` [("btc", "/44'/0'/1'"), ("btc", "/84'/0'/1'"), ("bch", "/44'/145'/1'"), ("btctest", "/44'/1'/1'"), ("btctest", "/84'/1'/1'"), ("btctest4", "/44'/1'/1'"), ("btctest4", "/84'/1'/1'"), ("btcreg", "/44'/1'/1'"), ("btcreg", "/84'/1'/1'"), ("bchtest", "/44'/1'/1'"), ("bchtest4", "/44'/1'/1'"), ("bchreg", "/44'/1'/1'")]
        liftIO $ dBPortfolioDescription second.portfolioRow `shouldBe` "spending"

    it "refuses keys for eleven accounts of the twelve, and creates nothing" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 1
        lift $
          dbShouldFailWith "portfolio1 of wallet0 cannot be created without a key for each of its twelve accounts, one for every network and type" $
            createPortfolioWith ctx (first.portfolioWalletId, first.portfolioWallet) 1 "" (drop 1 keys)
        portfoliosHeld `dbShouldBeE` [(0, 0)]

    -- Bitcoin Cash has no SegWit, and the bip84 account on btc is missing
    it "refuses a bip84 key on Bitcoin Cash in place of the one on Bitcoin" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 1
        let onCash = [(if (net, kind) == (btc, Bip84) then bch else net, kind, key) | (net, kind, key) <- keys]
        lift $
          dbShouldFailWith "portfolio1 of wallet0 cannot be created without a key for each of its twelve accounts, one for every network and type" $
            createPortfolioWith ctx (first.portfolioWalletId, first.portfolioWallet) 1 "" onCash

    it "refuses a key an account of the network holds already, naming its portfolio" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 1
        key <- liftIO $ keyOf (testSeed ctx) Bip44 btc 0
        lift $
          dbShouldFailWith "portfolio0 of wallet0 already has this key on btc" $
            createPortfolioWith ctx (first.portfolioWalletId, first.portfolioWallet) 1 "" (withKeyOf btc Bip44 key keys)

    it "creates nothing when it refuses a key an account of the network holds" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 1
        key <- liftIO $ keyOf (testSeed ctx) Bip44 btc 0
        lift $ dbShouldFail $ createPortfolioWith ctx (first.portfolioWalletId, first.portfolioWallet) 1 "" (withKeyOf btc Bip44 key keys)
        portfoliosHeld `dbShouldBeE` [(0, 0)]

    -- The key of account 0 written with the child number 5', which is
    -- 0x80000005: the addresses of portfolio 0 in portfolio 5
    it "refuses a key an account of the network holds, though written with another child number" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 5
        key <- liftIO $ keyOf (testSeed ctx) Bip44 btc 0
        lift $
          dbShouldFailWith "portfolio0 of wallet0 already has this key on btc" $
            createPortfolioWith ctx (first.portfolioWalletId, first.portfolioWallet) 5 "" (withKeyOf btc Bip44 (renumbered 0x80000005 key) keys)

{- The index of the next portfolio -}

nextIndexSpec :: Ctx -> Spec
nextIndexSpec ctx =
  describe "The index of the next portfolio of a wallet" $ do
    it "is 0 in a wallet without a portfolio" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
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

    -- The keys of portfolio 0 of the test mnemonic, but on btctest4 that
    -- of the second test mnemonic, in a wallet of the fingerprint of the
    -- test mnemonic
    it "refuse a bip44 key on btctest4 other than the one of btctest, naming the path and the networks" $
      inDatabaseE $ do
        held <- lift $ insertWallet testFingerprint
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 0
        key <- liftIO $ keyOf (testSeed2 ctx) Bip44 btcTest4 0
        lift $
          dbShouldFailWith "The bip44 account /44'/1'/0' of portfolio0 of wallet0 has one key on btctest and would get another on btctest4: the networks of one path share its key" $
            createPortfolioWith ctx held 0 "" (withKeyOf btcTest4 Bip44 key keys)

    -- The test networks of Bitcoin Cash have the coin type 1 too
    it "refuse a bip44 key on bchtest other than the one of btctest, since they share the path" $
      inDatabaseE $ do
        held <- lift $ insertWallet testFingerprint
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 0
        key <- liftIO $ keyOf (testSeed2 ctx) Bip44 bchTest 0
        lift $
          dbShouldFailWith "The bip44 account /44'/1'/0' of portfolio0 of wallet0 has one key on btctest and would get another on bchtest: the networks of one path share its key" $
            createPortfolioWith ctx held 0 "" (withKeyOf bchTest Bip44 key keys)

    -- btctest comes first of the three test networks of Bitcoin
    it "refuse a bip84 key on btcreg other than the one of btctest, naming the path and the networks" $
      inDatabaseE $ do
        held <- lift $ insertWallet testFingerprint
        keys <- liftIO $ portfolioKeysOf (testSeed ctx) 0
        key <- liftIO $ keyOf (testSeed2 ctx) Bip84 btcRegTest 0
        lift $
          dbShouldFailWith "The bip84 account /84'/1'/0' of portfolio0 of wallet0 has one key on btctest and would get another on btcreg: the networks of one path share its key" $
            createPortfolioWith ctx held 0 "" (withKeyOf btcRegTest Bip84 key keys)

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

{- The wallet of a seed -}

seedWalletSpec :: Ctx -> Spec
seedWalletSpec ctx =
  describe "The wallet of a seed" $ do
    it "is the wallet with the fingerprint of the seed" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        (fmap (walletIndex . snd) <$> seedWallet ctx (testSeed ctx)) `dbShouldBeE` Just 0

    -- wallet0 is of the second test mnemonic, wallet1 of the test mnemonic
    it "is found by its fingerprint among the wallets of other seeds" $
      inDatabaseE $ do
        _ <- portfolioOf ctx testMnemonic2
        _ <- testPortfolio ctx
        (fmap (walletIndex . snd) <$> seedWallet ctx (testSeed ctx)) `dbShouldBeE` Just 1

    it "is none when the database has no wallet of the fingerprint of the seed" $
      inDatabaseE $ do
        _ <- portfolioOf ctx testMnemonic2
        (fmap (walletIndex . snd) <$> seedWallet ctx (testSeed ctx)) `dbShouldBeE` Nothing

    -- The keys decide, not the fingerprint, which is 32 bits of a hash:
    -- the wallet has the fingerprint of the test mnemonic, and portfolio
    -- 0 of the second one
    it "refuses a wallet that has the fingerprint of the seed and keys the seed does not derive" $
      inDatabaseE $ do
        held <- lift $ insertWallet testFingerprint
        _ <- createPortfolio ctx (testSeed2 ctx) held 0
        lift $
          dbShouldFailWith "wallet0 has the fingerprint of this seed and other keys: it is of another seed with the same fingerprint. Nothing was changed" $
            seedWallet ctx (testSeed ctx)

    -- Portfolio 0 is of the test mnemonic, and portfolio 1 of the second
    -- one, at the index 1
    it "refuses a wallet of the fingerprint of the seed with a portfolio of other keys, though another one is of the seed" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        _ <- createPortfolio ctx (testSeed2 ctx) (first.portfolioWalletId, first.portfolioWallet) 1
        lift $
          dbShouldFailWith "wallet0 has the fingerprint of this seed and other keys: it is of another seed with the same fingerprint. Nothing was changed" $
            seedWallet ctx (testSeed ctx)

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
        walletsHeld `dbShouldBeE` [(1, "4435aa1c")]

  -- wallet0 is of the test mnemonic, bc560f95, and wallet1 of the second
  -- one, 4435aa1c. The default wallet is wallet0, which stays.
  describe "The command deletewallet" $ do
    let twoWallets = do
          _ <- testPortfolio ctx
          void $ portfolioOf ctx testMnemonic2
    it "deletes the wallet --wallet names when it is given the fingerprint of that wallet" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInFile (dir </> "wallet.sqlite") twoWallets
        commandResponse ctx wallet UnitBitcoin (CommandDeleteWallet (Just 1) "4435aa1c")
          `shouldReturn` ResponseDeleted "wallet1 has been deleted, with all of its portfolios"

    -- Every wallet has its fingerprint: the word unknown is no fingerprint
    it "refuses the word unknown in place of the fingerprint of the wallet, and deletes nothing" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "wallet.sqlite"
        wallet <- walletInFile database twoWallets
        commandResponse ctx wallet UnitBitcoin (CommandDeleteWallet (Just 1) "unknown")
          `shouldReturn` ResponseError "The fingerprint given is not that of wallet1: portfolios --verbose shows it. Nothing was deleted"
        inDatabaseFile database (runExceptT walletsHeld) `shouldReturn` Right [(0, "bc560f95"), (1, "4435aa1c")]

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
        _ <- refreshCoins btc ctx accId (const seenTime) [segwitExternalAddr 0] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (segwitExternalAddr 0) 1000]
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
        lift $ markTxInfoDead (portfolioPool portfolio btc) 800000 (fakeTxHash 1)
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
          refreshCoins btc ctx accId (const seenTime) [externalAddr 0] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000]
        shownSpendable <$> commandResponse ctx wallet UnitBitcoin (CommandPortfolios (OnScreen Nothing))
          `shouldReturn` Right [("btc", 50000)]

    it "are shown by hw addresses with the balance each address can spend now" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInFile (dir </> "wallet.sqlite") $ do
          (accId, _) <- testNewAcc ctx
          _ <- genExtAddress ctx def accId ""
          _ <- genExtAddress ctx def accId ""
          refreshCoins btc ctx accId (const seenTime) [externalAddr 0] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000]
        -- Newest first: the second address holds nothing
        addressSpendable <$> commandResponse ctx wallet UnitBitcoin (CommandAddresses (Target (Just btc) (Just Bip44)) False (OnScreen Nothing) (Paging Nothing Nothing))
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
        _ <- refreshCoins btc ctx legacyId (const seenTime) [externalAddr 0] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000]
        _ <- refreshCoins btc ctx segwitId (const seenTime) [segwitExternalAddr 0] [storeCoin ctx (fakeTxHash 2, 0) (Just 800000) (segwitExternalAddr 0) 30000]
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
          viewFingerprint view `shouldBe` testFingerprint
          viewCreated view `shouldBe` seenTime

    it "says whether it is the active portfolio: the first one created is, the next one is not" $
      inDatabaseE $ do
        first <- testPortfolio ctx
        second <- nextPortfolioOf ctx testMnemonic
        firstView <- portfolioView first (\_ -> return 0)
        secondView <- portfolioView second (\_ -> return 0)
        liftIO $ (viewActive firstView, viewActive secondView) `shouldBe` (True, False)

    it "names networks as a sentence does" $ do
      networksText [btc] `shouldBe` "btc"
      networksText [btc, bch] `shouldBe` "btc and bch"
      networksText [btc, bch, btcTest] `shouldBe` "btc, bch and btctest"

{- JSON -}

-- | Portfolio 1 of wallet 0 as portfolios shows it: not the active one,
-- described, holding on
-- btc 5000 confirmed and 300 unconfirmed in 2 coins, 4800 of it
-- spendable, at 3 addresses, with the keys of its two accounts there,
-- which are those of account 0 of the test wallet. A portfolio has every
-- network: the view gives btc alone, as a test needs no more.
fixtureView :: PortfolioView
fixtureView =
  PortfolioView
    { viewRef = PortfolioRef 0 1,
      viewActive = False,
      viewDescription = "savings",
      viewWalletDescription = "steel backup",
      viewFingerprint = testFingerprint,
      viewCreated = fixtureTime,
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
  "active": false,
  "description": "savings",
  "walletdescription": "steel backup",
  "fingerprint": "bc560f95",
  "created": "2023-11-01T00:00:00Z",
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

    it "says that the portfolio is the active one with active true" $
      eitherDecode (cs (Text.replace "\"active\": false" "\"active\": true" (cs viewJson))) `shouldBe` Right (toJSON fixtureView {viewActive = True})

    -- Every wallet has its fingerprint
    it "refuses a portfolio without the fingerprint of its wallet" $
      (eitherDecode (cs (Text.replace "\"fingerprint\": \"bc560f95\"" "\"fingerprint\": null" (cs viewJson))) :: Either String PortfolioView)
        `shouldSatisfy` isLeft

    -- A file given to hw may name any network
    it "refuses a portfolio on a network hw does not know, naming it" $
      (eitherDecode (cs (Text.replace "\"network\": \"btc\"" "\"network\": \"nope\"" (cs viewJson))) :: Either String PortfolioView)
        `shouldBe` Left "Error in $.networks[0]: Unknown network nope"
