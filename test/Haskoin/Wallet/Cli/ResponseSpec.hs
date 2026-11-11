{-# LANGUAGE OverloadedStrings #-}

-- | The answers of hw: in JSON, as --json writes them for scripts, and
-- whether hw exits with a failure for them, which a script tells from the
-- exit code
module Haskoin.Wallet.Cli.ResponseSpec (spec) where

import Data.Default (def)
import Data.Text (Text)
import Haskoin (bch, btc, btcTest4, prepareContext)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryResponse)
import Haskoin.Wallet.Cli.Response (Response (..), responseFailed)
import Haskoin.Wallet.Database.Portfolio (ActiveAfter (..), PortfolioRef (..))
import Haskoin.Wallet.TestUtils (mainPortfolioRef, quietSync)
import Test.Hspec

spec :: Spec
spec = do
  prepareContext $ \ctx ->
    -- An answer reads back as it was written
    testIdentity def {jsonTests = [JsonBox (arbitraryResponse btc ctx)]}

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

    it "is a recover that left out a network whose store is down" $
      responseFailed (ResponseRecover [btc] 10 (Just 0) [0] [quietSync] [] [] [(bch, downStore)] recoveredActive) `shouldBe` True

    it "is not a recover that left out a network the host does not serve" $
      responseFailed (ResponseRecover [btc] 10 (Just 0) [0] [quietSync] [] [btcTest4] [] recoveredActive) `shouldBe` False

    it "is an error" $
      responseFailed (ResponseError "There are no portfolios in the wallet") `shouldBe` True
  where
    downStore :: Text
    downStore = "The store at http://127.0.0.1:1/ did not answer"
    -- A recovery into a database without a portfolio makes portfolio0 of
    -- the new wallet the active one
    recoveredActive = Just (ActiveAfter (PortfolioRef 0 0) True)
