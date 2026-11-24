{-# LANGUAGE OverloadedStrings #-}

-- | The answers of hw: in JSON, as --json writes them for scripts, and
-- whether hw exits with a failure for them, which a script tells from the
-- exit code
module Haskoin.Wallet.Cli.ResponseSpec (spec) where

import Data.Aeson (object, toJSON, (.=))
import Data.Default (def)
import Data.Text (Text)
import Haskoin (bch, btc, btcTest4)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryResponse)
import Haskoin.Wallet.Cli.Response (Response (..), responseFailed)
import Haskoin.Wallet.Database.Portfolio (ActiveAfter (..), PortfolioRef (..))
import Haskoin.Wallet.Sync (SyncAfter (..))
import Haskoin.Wallet.TestUtils (fakeTxHash, mainPortfolioRef, quietSync)
import Test.Hspec

spec :: Spec
spec = do
  -- An answer reads back as it was written
  testIdentity def {jsonTests = [JsonBox (arbitraryResponse btc)]}

  -- The key of the Bitcoin wiki's "Wallet import format" page
  describe "An error in JSON" $
    it "names no private key that its text quotes" $
      toJSON (ResponseError "The wallet has no address KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617")
        `shouldBe` object ["type" .= ("error" :: Text), "error" .= ("The wallet has no address (a private key, not shown)" :: Text)]

  -- The specification: sync and recover go on when the store of a network
  -- is down, and the exit status tells. A host that does not serve a
  -- network, as the public one serves no regtest, is no failure.
  describe "An answer hw exits with a failure for" $ do
    it "is a sync that left out a network whose store is down" $
      responseFailed (ResponseSync [quietSync] [] [(bch, downStore)] (Just mainPortfolioRef)) `shouldBe` True

    it "is not a sync that left out a network the host does not serve" $
      responseFailed (ResponseSync [quietSync] [btcTest4] [] (Just mainPortfolioRef)) `shouldBe` False

    it "is not a sync of every network" $
      responseFailed (ResponseSync [quietSync] [] [] (Just mainPortfolioRef)) `shouldBe` False

    -- The transaction is sent: a failure would tell a script that it is
    -- not
    it "is not a transaction sent that the wallet could not sync after" $
      responseFailed (ResponseSentTx btc (fakeTxHash 1) (SyncAfter [] (Just downStore))) `shouldBe` False

    it "is a recover that left out a network whose store is down" $
      responseFailed (ResponseRecover [btc] 10 (Just 0) [0] [quietSync] [] [] [(bch, downStore)] recoveredActive Nothing []) `shouldBe` True

    it "is not a recover that left out a network the host does not serve" $
      responseFailed (ResponseRecover [btc] 10 (Just 0) [0] [quietSync] [] [btcTest4] [] recoveredActive Nothing []) `shouldBe` False

    it "is an error" $
      responseFailed (ResponseError "There are no portfolios in the wallet") `shouldBe` True

    it "is a check of a mnemonic that does not match the portfolio" $
      responseFailed (ResponseChecked mainPortfolioRef False "The mnemonic and passphrase did not match portfolio0 of wallet0") `shouldBe` True

    it "is not a check of a mnemonic that matches the portfolio" $
      responseFailed (ResponseChecked mainPortfolioRef True "The mnemonic and passphrase matched portfolio0 of wallet0") `shouldBe` False
  where
    downStore :: Text
    downStore = "The store at http://127.0.0.1:1/ did not answer"
    -- A recovery into a database without a portfolio makes portfolio0 of
    -- the new wallet the active one
    recoveredActive = Just (ActiveAfter (PortfolioRef 0 0) True)
