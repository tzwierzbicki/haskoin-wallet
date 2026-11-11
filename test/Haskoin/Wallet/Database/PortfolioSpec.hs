{-# LANGUAGE OverloadedStrings #-}

-- | The wallets and their portfolios: named by their indexes, which a new
-- one takes the lowest free of, described by the user, listed in the
-- order of their indexes, the active one, which switch changes, and how
-- the answers name them. The accounts of a portfolio
-- are in "Haskoin.Wallet.Database.AccountSpec": most tests here need the
-- rows of the wallets and of the portfolios alone.
module Haskoin.Wallet.Database.PortfolioSpec (spec) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson (eitherDecode, toJSON, (.=))
import qualified Data.Aeson as Json
import Data.Default (def)
import Data.Either (isLeft)
import Data.Text (Text)
import Haskoin
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryAccountRef, arbitraryActiveAfter, arbitraryPlace, arbitraryPortfolioRef)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account (deleteWallet)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Test.Hspec
import UnliftIO.Exception (SomeException, try)

spec :: Spec
spec = do
  namesSpec
  walletsSpec
  portfoliosSpec
  descriptionsSpec
  activeSpec
  switchSpec
  placesSpec
  jsonSpec
  -- How the answers name a portfolio, a network of it and an account of
  -- it, and the active portfolio after a command, read back as they were
  -- written
  testIdentity
    def
      { jsonTests = [JsonBox arbitraryPortfolioRef, JsonBox (arbitraryPlace btc), JsonBox (arbitraryAccountRef btc), JsonBox (arbitraryAccountRef bch), JsonBox arbitraryActiveAfter]
      }

{- Fixtures -}

-- | Two wallets as their rows alone, without any account: wallet0, of the
-- test mnemonic, with portfolios 0 and 1, and wallet1, of the second test
-- mnemonic, with portfolio 0
twoWallets :: ExceptT String (DB IO) ()
twoWallets = do
  (first, _) <- lift $ insertWallet testFingerprint
  (second, _) <- lift $ insertWallet testFingerprint2
  _ <- insertPortfolio first 0 ""
  _ <- insertPortfolio first 1 ""
  void $ insertPortfolio second 0 ""

-- | The names of the wallets of the database, in the order hw lists them
walletNames :: DB IO [Text]
walletNames = map (walletName . walletIndex . snd) <$> getWallets

-- | The portfolio switch makes the active one, as the answers name it
switched :: Maybe Natural -> Natural -> ExceptT String (DB IO) PortfolioRef
switched walletM index = portfolioRef <$> switchTo walletM index

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
        _ <- insertWallet testFingerprint
        _ <- insertWallet testFingerprint2
        walletNames `dbShouldBe` ["wallet0", "wallet1"]

    it "keep their names when another one is deleted: without wallet0, wallet1 is still wallet1" $
      inDatabase $ do
        (first, _) <- insertWallet testFingerprint
        _ <- insertWallet testFingerprint2
        deleteWallet first
        walletNames `dbShouldBe` ["wallet1"]

    it "give the next wallet the name a deleted one left: wallet0 again" $
      inDatabase $ do
        (first, _) <- insertWallet testFingerprint
        _ <- insertWallet testFingerprint2
        deleteWallet first
        (_, again) <- insertWallet testFingerprint
        liftIO $ walletName (walletIndex again) `shouldBe` "wallet0"
        walletNames `dbShouldBe` ["wallet0", "wallet1"]

    it "keep the fingerprint of their master key, and start without a description" $
      inDatabase $ do
        (_, wallet) <- insertWallet testFingerprint
        liftIO $ do
          dBWalletFingerprint wallet `shouldBe` "bc560f95"
          walletFingerprint wallet `shouldBe` testFingerprint
          dBWalletDescription wallet `shouldBe` ""

    it "have each a fingerprint of its own: the database refuses a second wallet with one it has" $
      inDatabase $ do
        _ <- insertWallet testFingerprint
        second <- try (insertWallet testFingerprint)
        liftIO $ (isLeft (second :: Either SomeException (DBWalletId, DBWallet))) `shouldBe` True

    it "are found by their index" $
      inDatabase $ do
        _ <- insertWallet testFingerprint
        second <- insertWallet testFingerprint2
        walletAt 1 `dbShouldBe` Just second
        walletAt 2 `dbShouldBe` Nothing

    -- 2^64 = 18446744073709551616, which a number of 64 bits holds as 0
    it "have none at an index of 2^64, which would wrap around to wallet0" $
      inDatabase $ do
        _ <- insertWallet testFingerprint
        walletAt 18446744073709551616 `dbShouldBe` Nothing

    -- 5596eb4d is the fingerprint of the seed of the shares of the tests
    it "are found by the fingerprint of their master key" $
      inDatabase $ do
        _ <- insertWallet testFingerprint
        second <- insertWallet testFingerprint2
        walletWithFingerprint testFingerprint2 `dbShouldBe` Just second
        walletWithFingerprint (fingerprintOf "5596eb4d") `dbShouldBe` Nothing

{- Portfolios -}

portfoliosSpec :: Spec
portfoliosSpec =
  describe "The portfolios of a wallet" $ do
    it "are stored at their index, with the wallet they are of" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        (_, row) <- insertPortfolio walletId 3 ""
        liftIO $ do
          portfolioIndex row `shouldBe` 3
          portfolioName (portfolioIndex row) `shouldBe` "portfolio3"
          dBPortfolioWallet row `shouldBe` walletId
          dBPortfolioDescription row `shouldBe` ""

    it "are listed in the order of their indexes, whatever the order they were made in" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        _ <- insertPortfolio walletId 2 ""
        _ <- insertPortfolio walletId 0 ""
        _ <- insertPortfolio walletId 1 ""
        (map (portfolioIndex . snd) <$> lift (getPortfolios walletId)) `dbShouldBeE` [0, 1, 2]

    it "are found by their index in their wallet" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        first <- insertPortfolio walletId 0 ""
        lift (portfolioAt walletId 0) `dbShouldBeE` Just first
        lift (portfolioAt walletId 1) `dbShouldBeE` Nothing

    -- 2^64 + 1 = 18446744073709551617, which a number of 64 bits holds as 1
    it "have none at an index of 2^64 + 1, which would wrap around to portfolio1" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        _ <- insertPortfolio walletId 0 ""
        _ <- insertPortfolio walletId 1 ""
        lift (portfolioAt walletId 18446744073709551617) `dbShouldBeE` Nothing

    -- 2^31 = 2147483648, past the last hardened step of a path
    it "have none at the index 2^31, which no account can have" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        lift (portfolioAt walletId 2147483648) `dbShouldBeE` Nothing

    it "cannot be made at the index 2^31, which no account can have" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        lift $
          dbShouldFailWith "portfolio2147483648 cannot be: the index of a portfolio is below 2147483648, which is 2^31" $
            insertPortfolio walletId 2147483648 ""

    it "have one portfolio at an index: a second one is refused" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        _ <- insertPortfolio walletId 0 ""
        lift $ dbShouldFailWith "The wallet has portfolio0 already" $ insertPortfolio walletId 0 ""

    it "are each wallet's own: two wallets both have their portfolio0" $
      inDatabaseE $ do
        twoWallets
        (map portfolioRef <$> lift getAllPortfolios) `dbShouldBeE` [PortfolioRef 0 0, PortfolioRef 0 1, PortfolioRef 1 0]

    it "are read with their wallet, which gives the fingerprint of their seed" $
      inDatabaseE $ do
        (walletId, wallet) <- lift $ insertWallet testFingerprint
        (key, row) <- insertPortfolio walletId 1 ""
        portfolio <- getPortfolio key
        liftIO $ do
          portfolio `shouldBe` Portfolio key row walletId wallet
          portfolioRef portfolio `shouldBe` PortfolioRef 0 1
          portfolioFingerprint portfolio `shouldBe` testFingerprint

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
        (walletId, _) <- lift $ insertWallet testFingerprint
        (key, _) <- insertPortfolio walletId 0 "savings"
        setPortfolioDescription key "spending"
        (fmap (dBPortfolioDescription . snd) <$> lift (portfolioAt walletId 0)) `dbShouldBeE` Just "spending"

    it "is removed from a portfolio by an empty one" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        (key, _) <- insertPortfolio walletId 0 "savings"
        setPortfolioDescription key ""
        (fmap (dBPortfolioDescription . snd) <$> lift (portfolioAt walletId 0)) `dbShouldBeE` Just ""

    it "is set on a wallet in place of the one it had, and leaves its portfolios theirs" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        _ <- insertPortfolio walletId 0 "savings"
        setWalletDescription walletId "paper backup in the safe"
        setWalletDescription walletId "steel backup in the safe"
        (fmap (dBWalletDescription . snd) <$> lift (walletAt 0)) `dbShouldBeE` Just "steel backup in the safe"
        (fmap (dBPortfolioDescription . snd) <$> lift (portfolioAt walletId 0)) `dbShouldBeE` Just "savings"

    it "is held to its rules when a portfolio is made with one, or given one" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        (key, _) <- insertPortfolio walletId 0 ""
        lift $ dbShouldFailWith "The description must not start or end with a space" $ insertPortfolio walletId 1 "savings "
        lift $ dbShouldFailWith "The description must not contain a line break or a control character" $ setPortfolioDescription key "a\nb"
        lift $ dbShouldFailWith "The description must not start or end with a space" $ setWalletDescription walletId " a"

{- The active portfolio -}

activeSpec :: Spec
activeSpec =
  describe "The active portfolio, where the commands work" $ do
    it "is the first portfolio created in the database" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        (key, _) <- insertPortfolio walletId 0 ""
        lift activePortfolioId `dbShouldBeE` Just key

    -- portfolio1 of wallet0 and portfolio0 of wallet1 come after
    -- portfolio0 of wallet0
    it "stays the first one when more portfolios are created" $
      inDatabaseE $ do
        twoWallets
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 0)

    it "is none in a database without a portfolio" $
      inDatabaseE $ activePortfolioRef `dbShouldBeE` Nothing

    it "is refused to a command in a database without a portfolio, which is told how to make one" $
      inDatabase $
        dbShouldFailWith "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back" requireActivePortfolio

    it "is none once the last wallet is deleted" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet testFingerprint
        _ <- insertPortfolio walletId 0 ""
        lift $ deleteWallet walletId
        activePortfolioRef `dbShouldBeE` Nothing

    -- The new wallet takes the index 0 again: its portfolio0 is another
    -- row than the one deleted
    it "is the first portfolio created after the last wallet was deleted" $
      inDatabaseE $ do
        (first, _) <- lift $ insertWallet testFingerprint
        _ <- insertPortfolio first 0 ""
        lift $ deleteWallet first
        (second, _) <- lift $ insertWallet testFingerprint2
        (key, _) <- insertPortfolio second 0 ""
        lift activePortfolioId `dbShouldBeE` Just key

{- switch -}

-- The database has wallet0 with portfolios 0 and 1, and wallet1 with
-- portfolio 0. portfolio0 of wallet0 is active, as the first one created.
switchSpec :: Spec
switchSpec =
  describe "A switch" $ do
    it "makes the portfolio --portfolio names the active one, in the wallet of the active portfolio without --wallet" $
      inDatabaseE $ do
        twoWallets
        switched Nothing 1 `dbShouldBeE` PortfolioRef 0 1
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 1)

    it "makes the portfolio of the wallet --wallet names the active one" $
      inDatabaseE $ do
        twoWallets
        switched (Just 1) 0 `dbShouldBeE` PortfolioRef 1 0
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 1 0)

    -- After a switch to wallet1, --portfolio 0 alone is portfolio0 of
    -- wallet1, not of wallet0
    it "takes the wallet of the portfolio active now without --wallet, after a switch to another wallet" $
      inDatabaseE $ do
        twoWallets
        _ <- switched (Just 1) 0
        switched Nothing 0 `dbShouldBeE` PortfolioRef 1 0

    it "refuses a --wallet the database does not have, and leaves the active portfolio as it was" $
      inDatabaseE $ do
        twoWallets
        lift $ dbShouldFailWith "There is no wallet2: portfolios lists the wallets" $ switched (Just 2) 0
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 0)

    it "refuses a --portfolio the wallet does not have" $
      inDatabaseE $ do
        twoWallets
        lift $ dbShouldFailWith "wallet1 has no portfolio1: portfolios lists the portfolios" $ switched (Just 1) 1

    -- wallet0 has no portfolio2, though wallet1 does not matter here
    it "refuses a --portfolio the wallet of the active portfolio does not have, without --wallet" $
      inDatabaseE $ do
        twoWallets
        lift $ dbShouldFailWith "wallet0 has no portfolio2: portfolios lists the portfolios" $ switched Nothing 2

    -- 2^64 = 18446744073709551616 would wrap around to portfolio0
    it "refuses a --portfolio of 2^64, though the wallet has portfolio0" $
      inDatabaseE $ do
        twoWallets
        lift $
          dbShouldFailWith "wallet0 has no portfolio18446744073709551616: portfolios lists the portfolios" $
            switched Nothing 18446744073709551616

    it "is refused in a database without a portfolio, which is told how to make one" $
      inDatabase $
        dbShouldFailWith "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back" $
          switched (Just 0) 0

    it "names itself in a refusal or a next step as the command to type" $
      switchCommandText (PortfolioRef 1 2) `shouldBe` "switch --wallet 1 --portfolio 2"

{- How the answers name a place -}

placesSpec :: Spec
placesSpec =
  describe "How the answers name where coins and transactions are" $ do
    it "names what a portfolio holds on a network by the indexes of its wallet and its own, and the network" $
      inDatabaseE $ do
        twoWallets
        wallet <- walletNamed 0
        portfolio <- portfolioNamed wallet 1
        liftIO $ portfolioPlace portfolio bch `shouldBe` Place (PortfolioRef 0 1) bch
        poolPlace (portfolioPool portfolio bch) `dbShouldBeE` Place (PortfolioRef 0 1) bch

    it "gives the pool of a portfolio on a network: the portfolio and the network" $
      inDatabaseE $ do
        twoWallets
        portfolio <- requireActivePortfolio
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

    it "writes the active portfolio after a command as its wallet and its index, with whether the command activated it, and reads it back" $ do
      let json = "{\"wallet\": 0, \"portfolio\": 1, \"activated\": false}"
      eitherDecode json `shouldBe` Right (toJSON (ActiveAfter (PortfolioRef 0 1) False))
      eitherDecode json `shouldBe` Right (ActiveAfter (PortfolioRef 0 1) False)

    it "writes the pairs of a place flat, for the answers that place something" $
      Json.object (placePairs (Place (PortfolioRef 0 1) btc))
        `shouldBe` Json.object ["wallet" .= (0 :: Int), "portfolio" .= (1 :: Int), "network" .= ("btc" :: Text)]
