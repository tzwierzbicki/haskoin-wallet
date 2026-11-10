{-# LANGUAGE OverloadedStrings #-}

-- | The wallets and their portfolios: named by their indexes, which a new
-- one takes the lowest free of, described by the user, listed in the
-- order of their indexes, chosen by a command from its options and the
-- configuration, and named in the answers. The accounts of a portfolio
-- are in "Haskoin.Wallet.Database.AccountSpec": most tests here need the
-- rows of the wallets and of the portfolios alone.
module Haskoin.Wallet.Database.PortfolioSpec (spec) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson (eitherDecode, toJSON, (.=))
import qualified Data.Aeson as Json
import Data.Default (def)
import Data.Text (Text)
import Haskoin
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryAccountRef, arbitraryPlace, arbitraryPortfolioRef)
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account (deleteWallet)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = do
  namesSpec
  walletsSpec
  portfoliosSpec
  descriptionsSpec
  choosingSpec
  choosingSeveralSpec
  narrowingSpec
  placesSpec
  jsonSpec
  -- How the answers name a portfolio, a network of it and an account of
  -- it reads back as it was written
  testIdentity
    def
      { jsonTests = [JsonBox arbitraryPortfolioRef, JsonBox (arbitraryPlace btc), JsonBox (arbitraryAccountRef btc), JsonBox (arbitraryAccountRef bch)]
      }

{- Fixtures -}

-- | Two wallets as their rows alone, without any account: wallet0, of the
-- test mnemonic, with portfolios 0 and 1, and wallet1, of the second test
-- mnemonic, with portfolio 0
twoWallets :: ExceptT String (DB IO) ()
twoWallets = do
  (first, _) <- lift $ insertWallet (Just testFingerprint)
  (second, _) <- lift $ insertWallet (Just testFingerprint2)
  _ <- insertPortfolio first 0 ""
  _ <- insertPortfolio first 1 ""
  void $ insertPortfolio second 0 ""

-- | The names of the wallets of the database, in the order hw lists them
walletNames :: DB IO [Text]
walletNames = map (walletName . walletIndex . snd) <$> getWallets

-- | The portfolio a command works in, as the answers name it
chosen :: Config -> Maybe Natural -> Maybe Natural -> ExceptT String (DB IO) PortfolioRef
chosen cfg walletM portfolioM = portfolioRef <$> choosePortfolio cfg (PortfolioChoice walletM portfolioM)

-- | The portfolios a command that covers several works on, as the answers
-- name them
chosenSeveral :: Config -> Maybe Natural -> Maybe Natural -> ExceptT String (DB IO) [PortfolioRef]
chosenSeveral cfg walletM portfolioM = map portfolioRef <$> choosePortfolios cfg (PortfolioChoice walletM portfolioM)

-- | The portfolios the options of a command that finds its portfolio by
-- itself leave, as the answers name them, with how a refusal names them
narrowedTo :: Config -> Maybe Natural -> Maybe Natural -> ExceptT String (DB IO) (Maybe ([PortfolioRef], String))
narrowedTo cfg walletM portfolioM = fmap refsAndText <$> narrowedBy cfg (PortfolioChoice walletM portfolioM)
  where
    refsAndText narrowed = (portfolioRef <$> narrowedPortfolios narrowed, narrowedText narrowed)

{- Names -}

namesSpec :: Spec
namesSpec =
  describe "The names of wallets and portfolios" $ do
    it "name a wallet by its index: wallet0, wallet1 and so on" $
      (walletName <$> [0, 1, 12]) `shouldBe` ["wallet0", "wallet1", "wallet12"]

    it "name a portfolio by its index: portfolio0, portfolio1 and so on" $
      (portfolioName <$> [0, 1, 12]) `shouldBe` ["portfolio0", "portfolio1", "portfolio12"]

    it "give a new one the lowest index that is free, from 0" $ do
      lowestFree [] `shouldBe` 0
      lowestFree [0] `shouldBe` 1
      lowestFree [0, 1] `shouldBe` 2

    it "give a new one the index a deleted one left free, below the others" $ do
      lowestFree [0, 2] `shouldBe` 1
      lowestFree [1, 2] `shouldBe` 0

{- Wallets -}

walletsSpec :: Spec
walletsSpec =
  describe "The wallets of the database" $ do
    it "are numbered from 0 in the order they were made" $
      inDatabase $ do
        _ <- insertWallet (Just testFingerprint)
        _ <- insertWallet (Just testFingerprint2)
        walletNames `dbShouldBe` ["wallet0", "wallet1"]

    it "keep their names when another one is deleted: without wallet0, wallet1 is still wallet1" $
      inDatabase $ do
        (first, _) <- insertWallet (Just testFingerprint)
        _ <- insertWallet (Just testFingerprint2)
        deleteWallet first
        walletNames `dbShouldBe` ["wallet1"]

    it "give the next wallet the name a deleted one left: wallet0 again" $
      inDatabase $ do
        (first, _) <- insertWallet (Just testFingerprint)
        _ <- insertWallet (Just testFingerprint2)
        deleteWallet first
        (_, again) <- insertWallet Nothing
        liftIO $ walletName (walletIndex again) `shouldBe` "wallet0"
        walletNames `dbShouldBe` ["wallet0", "wallet1"]

    it "keep the fingerprint of their master key, and start without a description" $
      inDatabase $ do
        (_, wallet) <- insertWallet (Just testFingerprint)
        liftIO $ do
          dBWalletFingerprint wallet `shouldBe` Just "bc560f95"
          walletFingerprint wallet `shouldBe` Just testFingerprint
          dBWalletDescription wallet `shouldBe` ""

    -- As the step from the database of hw 0.9 makes them
    it "may have no fingerprint yet, several of them at once" $
      inDatabase $ do
        (_, first) <- insertWallet Nothing
        (_, second) <- insertWallet Nothing
        liftIO $ (walletFingerprint first, walletFingerprint second) `shouldBe` (Nothing, Nothing)
        walletNames `dbShouldBe` ["wallet0", "wallet1"]

    it "are found by their index" $
      inDatabase $ do
        _ <- insertWallet (Just testFingerprint)
        second <- insertWallet (Just testFingerprint2)
        walletAt 1 `dbShouldBe` Just second
        walletAt 2 `dbShouldBe` Nothing

    -- 2^64 = 18446744073709551616, which a number of 64 bits holds as 0
    it "have none at an index of 2^64, which would wrap around to wallet0" $
      inDatabase $ do
        _ <- insertWallet (Just testFingerprint)
        walletAt 18446744073709551616 `dbShouldBe` Nothing

    it "are found by the fingerprint of their master key" $
      inDatabase $ do
        _ <- insertWallet Nothing
        second <- insertWallet (Just testFingerprint2)
        walletWithFingerprint testFingerprint2 `dbShouldBe` Just second
        walletWithFingerprint testFingerprint `dbShouldBe` Nothing

{- Portfolios -}

portfoliosSpec :: Spec
portfoliosSpec =
  describe "The portfolios of a wallet" $ do
    it "are stored at their index, with the wallet they are of" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        (_, row) <- insertPortfolio walletId 3 ""
        liftIO $ do
          portfolioIndex row `shouldBe` 3
          portfolioName (portfolioIndex row) `shouldBe` "portfolio3"
          dBPortfolioWallet row `shouldBe` walletId
          dBPortfolioDescription row `shouldBe` ""

    it "are listed in the order of their indexes, whatever the order they were made in" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        _ <- insertPortfolio walletId 2 ""
        _ <- insertPortfolio walletId 0 ""
        _ <- insertPortfolio walletId 1 ""
        (map (portfolioIndex . snd) <$> lift (getPortfolios walletId)) `dbShouldBeE` [0, 1, 2]

    it "are found by their index in their wallet" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        first <- insertPortfolio walletId 0 ""
        lift (portfolioAt walletId 0) `dbShouldBeE` Just first
        lift (portfolioAt walletId 1) `dbShouldBeE` Nothing

    -- 2^64 + 1 = 18446744073709551617, which a number of 64 bits holds as 1
    it "have none at an index of 2^64 + 1, which would wrap around to portfolio1" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        _ <- insertPortfolio walletId 0 ""
        _ <- insertPortfolio walletId 1 ""
        lift (portfolioAt walletId 18446744073709551617) `dbShouldBeE` Nothing

    -- 2^31 = 2147483648, past the last hardened step of a path
    it "have none at the index 2^31, which no account can have" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        lift (portfolioAt walletId 2147483648) `dbShouldBeE` Nothing

    it "cannot be made at the index 2^31, which no account can have" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        lift $
          dbShouldFailWith "portfolio2147483648 cannot be: the index of a portfolio is below 2147483648, which is 2^31" $
            insertPortfolio walletId 2147483648 ""

    it "have one portfolio at an index: a second one is refused" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        _ <- insertPortfolio walletId 0 ""
        lift $ dbShouldFailWith "The wallet has portfolio0 already" $ insertPortfolio walletId 0 ""

    it "are each wallet's own: two wallets both have their portfolio0" $
      inDatabaseE $ do
        twoWallets
        (map portfolioRef <$> lift getAllPortfolios) `dbShouldBeE` [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 1 0]

    it "are read with their wallet, which gives the fingerprint of their seed" $
      inDatabaseE $ do
        (walletId, wallet) <- lift $ insertWallet (Just testFingerprint)
        (key, row) <- insertPortfolio walletId 1 ""
        portfolio <- getPortfolio key
        liftIO $ do
          portfolio `shouldBe` Portfolio key row walletId wallet
          portfolioRef portfolio `shouldBe` PortfolioRef 0 1
          portfolioFingerprint portfolio `shouldBe` Just testFingerprint

    it "know no fingerprint while their wallet has none" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet Nothing
        (key, _) <- insertPortfolio walletId 0 ""
        (portfolioFingerprint <$> getPortfolio key) `dbShouldBeE` Nothing

{- Descriptions -}

descriptionsSpec :: Spec
descriptionsSpec =
  describe "The description of a wallet or of a portfolio" $ do
    it "is one line of text, or none" $ do
      checkDescription "Savings of the family" `shouldBe` Right ()
      checkDescription "" `shouldBe` Right ()

    it "is refused with a line break or a control character" $ do
      checkDescription "savings\nspending" `shouldBe` Left "The description must not contain a line break or a control character"
      checkDescription "savings\tspending" `shouldBe` Left "The description must not contain a line break or a control character"

    it "is refused with a space at either end" $ do
      checkDescription " savings" `shouldBe` Left "The description must not start or end with a space"
      checkDescription "savings " `shouldBe` Left "The description must not start or end with a space"

    it "is set on a portfolio in place of the one it had" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        (key, _) <- insertPortfolio walletId 0 "savings"
        setPortfolioDescription key "spending"
        (fmap (dBPortfolioDescription . snd) <$> lift (portfolioAt walletId 0)) `dbShouldBeE` Just "spending"

    it "is removed from a portfolio by an empty one" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        (key, _) <- insertPortfolio walletId 0 "savings"
        setPortfolioDescription key ""
        (fmap (dBPortfolioDescription . snd) <$> lift (portfolioAt walletId 0)) `dbShouldBeE` Just ""

    it "is set on a wallet in place of the one it had, and leaves its portfolios theirs" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        _ <- insertPortfolio walletId 0 "savings"
        setWalletDescription walletId "paper backup in the safe"
        setWalletDescription walletId "steel backup in the safe"
        (fmap (dBWalletDescription . snd) <$> lift (walletAt 0)) `dbShouldBeE` Just "steel backup in the safe"
        (fmap (dBPortfolioDescription . snd) <$> lift (portfolioAt walletId 0)) `dbShouldBeE` Just "savings"

    it "is held to its rules when a portfolio is made with one, or given one" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet (Just testFingerprint)
        (key, _) <- insertPortfolio walletId 0 ""
        lift $ dbShouldFailWith "The description must not start or end with a space" $ insertPortfolio walletId 1 "savings "
        lift $ dbShouldFailWith "The description must not contain a line break or a control character" $ setPortfolioDescription key "a\nb"
        lift $ dbShouldFailWith "The description must not start or end with a space" $ setWalletDescription walletId " a"

{- The portfolio a command works in -}

-- The database has wallet0 with portfolios 0 and 1, and wallet1 with
-- portfolio 0
choosingSpec :: Spec
choosingSpec =
  describe "The portfolio a command works in" $ do
    it "is portfolio 0 of wallet 0 without an option" $
      inDatabaseE $ do
        twoWallets
        chosen def Nothing Nothing `dbShouldBeE` PortfolioRef 0 0

    it "is in the wallet --wallet names" $
      inDatabaseE $ do
        twoWallets
        chosen def (Just 1) Nothing `dbShouldBeE` PortfolioRef 1 0

    it "is the portfolio --portfolio names" $
      inDatabaseE $ do
        twoWallets
        chosen def Nothing (Just 1) `dbShouldBeE` PortfolioRef 0 1

    it "is in the wallet the key default-wallet names" $
      inDatabaseE $ do
        twoWallets
        chosen (configWith ["default-wallet" .= (1 :: Int)]) Nothing Nothing `dbShouldBeE` PortfolioRef 1 0

    it "is the portfolio the key default-portfolio names" $
      inDatabaseE $ do
        twoWallets
        chosen (configWith ["default-portfolio" .= (1 :: Int)]) Nothing Nothing `dbShouldBeE` PortfolioRef 0 1

    it "takes --wallet over the key default-wallet" $
      inDatabaseE $ do
        twoWallets
        chosen (configWith ["default-wallet" .= (1 :: Int)]) (Just 0) Nothing `dbShouldBeE` PortfolioRef 0 0

    it "takes --portfolio over the key default-portfolio" $
      inDatabaseE $ do
        twoWallets
        chosen (configWith ["default-portfolio" .= (1 :: Int)]) Nothing (Just 0) `dbShouldBeE` PortfolioRef 0 0

    it "refuses a default wallet that does not exist, naming the key default-wallet" $
      inDatabaseE $ do
        twoWallets
        lift $
          dbShouldFailWith "The key default-wallet of the configuration file names wallet3, which does not exist: give --wallet, or change the key with setdefault" $
            chosen (configWith ["default-wallet" .= (3 :: Int)]) Nothing Nothing

    it "refuses a default portfolio the wallet does not have, naming the key default-portfolio" $
      inDatabaseE $ do
        twoWallets
        lift $
          dbShouldFailWith "The key default-portfolio of the configuration file names portfolio4, which wallet0 does not have: give --portfolio, or change the key with setdefault" $
            chosen (configWith ["default-portfolio" .= (4 :: Int)]) Nothing Nothing

    -- portfolio 1 is the default, and wallet1 has portfolio 0 alone
    it "refuses a default portfolio the wallet --wallet names does not have, though the default wallet has it" $
      inDatabaseE $ do
        twoWallets
        lift $
          dbShouldFailWith "The key default-portfolio of the configuration file names portfolio1, which wallet1 does not have: give --portfolio, or change the key with setdefault" $
            chosen (configWith ["default-portfolio" .= (1 :: Int)]) (Just 1) Nothing

    it "refuses a --wallet that does not exist" $
      inDatabaseE $ do
        twoWallets
        lift $ dbShouldFailWith "There is no wallet2: portfolios lists the wallets" $ chosen def (Just 2) Nothing

    it "refuses a --portfolio the wallet does not have" $
      inDatabaseE $ do
        twoWallets
        lift $ dbShouldFailWith "wallet1 has no portfolio1: portfolios lists the portfolios" $ chosen def (Just 1) (Just 1)

    -- 2^64 = 18446744073709551616 would wrap around to portfolio0
    it "refuses a --portfolio of 2^64, though the wallet has portfolio0" $
      inDatabaseE $ do
        twoWallets
        lift $
          dbShouldFailWith "wallet0 has no portfolio18446744073709551616: portfolios lists the portfolios" $
            chosen def Nothing (Just 18446744073709551616)

    it "has none to work in while the database has no wallet, and says how to make one" $
      inDatabaseE $
        lift $
          dbShouldFailWith "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back" $
            chosen def Nothing Nothing

-- As sync, portfolios and exportsheet take them
choosingSeveralSpec :: Spec
choosingSeveralSpec =
  describe "The portfolios of a command that covers several" $ do
    it "are every portfolio of every wallet without an option, in the order of the wallets and of their portfolios" $
      inDatabaseE $ do
        twoWallets
        chosenSeveral def Nothing Nothing `dbShouldBeE` [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 1 0]

    it "are every portfolio of the wallet --wallet names" $
      inDatabaseE $ do
        twoWallets
        chosenSeveral def (Just 0) Nothing `dbShouldBeE` [PortfolioRef 0 0, PortfolioRef 0 1]

    it "are the one portfolio --portfolio names, in the default wallet" $
      inDatabaseE $ do
        twoWallets
        chosenSeveral def Nothing (Just 1) `dbShouldBeE` [PortfolioRef 0 1]

    it "are the one portfolio --wallet and --portfolio name together" $
      inDatabaseE $ do
        twoWallets
        chosenSeveral def (Just 1) (Just 0) `dbShouldBeE` [PortfolioRef 1 0]

    -- A command that covers every portfolio does not read the defaults
    it "are every portfolio of every wallet whatever the defaults of the configuration" $
      inDatabaseE $ do
        twoWallets
        chosenSeveral (configWith ["default-wallet" .= (1 :: Int)]) Nothing Nothing
          `dbShouldBeE` [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 1 0]

    it "are none in a database without a wallet" $
      inDatabaseE $ chosenSeveral def Nothing Nothing `dbShouldBeE` []

-- As show of a transaction, importtx, signtx, signsweep, bumpfee,
-- preparesweep and coin control take them: the options only narrow the
-- search, and never pick a default portfolio
narrowingSpec :: Spec
narrowingSpec =
  describe "The portfolios a command that finds its portfolio by itself looks in" $ do
    it "are every portfolio without an option" $
      inDatabaseE $ do
        twoWallets
        narrowedTo def Nothing Nothing `dbShouldBeE` Nothing

    it "are every portfolio of the wallet --wallet names alone, named by the wallet" $
      inDatabaseE $ do
        twoWallets
        narrowedTo def (Just 0) Nothing `dbShouldBeE` Just ([PortfolioRef 0 0, PortfolioRef 0 1], "wallet0")

    -- The default portfolio is portfolio 1: --wallet 0 alone still leaves
    -- portfolio 0 as well
    it "are every portfolio of the wallet --wallet names alone, whatever the key default-portfolio says" $
      inDatabaseE $ do
        twoWallets
        narrowedTo (configWith ["default-portfolio" .= (1 :: Int)]) (Just 0) Nothing
          `dbShouldBeE` Just ([PortfolioRef 0 0, PortfolioRef 0 1], "wallet0")

    it "are the one portfolio --portfolio names, of the default wallet, named in full" $
      inDatabaseE $ do
        twoWallets
        narrowedTo def Nothing (Just 1) `dbShouldBeE` Just ([PortfolioRef 0 1], "portfolio1 of wallet0")

    it "are the one portfolio --wallet and --portfolio name together" $
      inDatabaseE $ do
        twoWallets
        narrowedTo def (Just 1) (Just 0) `dbShouldBeE` Just ([PortfolioRef 1 0], "portfolio0 of wallet1")

    it "refuse a --wallet that does not exist" $
      inDatabaseE $ do
        twoWallets
        lift $ dbShouldFailWith "There is no wallet2: portfolios lists the wallets" $ narrowedTo def (Just 2) Nothing

{- How the answers name a place -}

placesSpec :: Spec
placesSpec =
  describe "How the answers name where coins and transactions are" $ do
    it "names what a portfolio holds on a network by the indexes of its wallet and its own, and the network" $
      inDatabaseE $ do
        twoWallets
        portfolio <- choosePortfolio def (PortfolioChoice (Just 0) (Just 1))
        liftIO $ portfolioPlace portfolio bch `shouldBe` Place (PortfolioRef 0 1) bch
        poolPlace (portfolioPool portfolio bch) `dbShouldBeE` Place (PortfolioRef 0 1) bch

    it "gives the pool of a portfolio on a network: the portfolio and the network" $
      inDatabaseE $ do
        twoWallets
        portfolio <- choosePortfolio def defaultPortfolio
        liftIO $ portfolioPool portfolio btcTest `shouldBe` Pool (portfolioId portfolio) btcTest

    -- m/purpose'/coin type'/index': 84 for bip84, 0 for btc, 145 for bch
    -- and 1 for a test network, then the index of the portfolio
    it "gives the path of an account from its type, its network and the index of its portfolio" $ do
      pathToStr (accountRefPath (AccountRef (Place (PortfolioRef 0 0) btc) Bip44)) `shouldBe` "/44'/0'/0'"
      pathToStr (accountRefPath (AccountRef (Place (PortfolioRef 0 2) btc) Bip84)) `shouldBe` "/84'/0'/2'"
      pathToStr (accountRefPath (AccountRef (Place (PortfolioRef 1 3) bch) Bip44)) `shouldBe` "/44'/145'/3'"
      pathToStr (accountRefPath (AccountRef (Place (PortfolioRef 0 1) btcTest4) Bip84)) `shouldBe` "/84'/1'/1'"

{- JSON -}

jsonSpec :: Spec
jsonSpec =
  describe "A place in JSON" $ do
    it "writes a portfolio as the indexes of its wallet and its own, and reads it back" $ do
      let json = "{\"wallet\": 1, \"portfolio\": 2}"
      eitherDecode json `shouldBe` Right (toJSON (PortfolioRef 1 2))
      eitherDecode json `shouldBe` Right (PortfolioRef 1 2)

    it "writes a portfolio on a network with the name of the network, and reads it back" $ do
      let json = "{\"wallet\": 1, \"portfolio\": 2, \"network\": \"bch\"}"
      eitherDecode json `shouldBe` Right (toJSON (Place (PortfolioRef 1 2) bch))
      eitherDecode json `shouldBe` Right (Place (PortfolioRef 1 2) bch)

    it "writes an account with its type and its path, and reads it back" $ do
      let json = "{\"wallet\": 0, \"portfolio\": 2, \"network\": \"btc\", \"type\": \"bip84\", \"derivation\": \"/84'/0'/2'\"}"
      eitherDecode json `shouldBe` Right (toJSON (AccountRef (Place (PortfolioRef 0 2) btc) Bip84))
      eitherDecode json `shouldBe` Right (AccountRef (Place (PortfolioRef 0 2) btc) Bip84)

    it "refuses a network hw does not know, naming it" $
      (eitherDecode "{\"wallet\": 1, \"portfolio\": 2, \"network\": \"nope\"}" :: Either String Place)
        `shouldBe` Left "Error in $: Unknown network nope"

    it "writes the pairs of a place flat, for the answers that place something" $
      Json.object (placePairs (Place (PortfolioRef 0 1) btc))
        `shouldBe` Json.object ["wallet" .= (0 :: Int), "portfolio" .= (1 :: Int), "network" .= ("btc" :: Text)]
