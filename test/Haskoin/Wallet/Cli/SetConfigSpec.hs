{-# LANGUAGE OverloadedStrings #-}

-- | The answers to the questions of setconfig without an option, once
-- they are given: the options of setconfig they stand for, one for each
-- setting the user changed. "Haskoin.Wallet.Workflow.SetConfigSpec" asks
-- the questions themselves, with lines typed and keys pressed.
module Haskoin.Wallet.Cli.SetConfigSpec (spec) where

import Data.Default (def)
import Haskoin (bch, btc, btcTest4)
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.Command (SetConfig (..), noSettings)
import Haskoin.Wallet.Cli.Listing (ListingOrder (..))
import Haskoin.Wallet.Cli.SetConfig
import Haskoin.Wallet.Config
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Keys (AccountKind (..))
import Test.Hspec

spec :: Spec
spec =
  -- The configuration hw has is the one of its defaults: btc and bch
  -- shown, btc, bip44, btc as the unit, the compact view, the usual
  -- order, no journal, both copies, the public server, 1 request at a
  -- time and a gap of 20
  describe "The options that the answers to the questions of setconfig stand for" $ do
    it "are none when every answer is the value hw has" $
      changedSettings def def `shouldBe` noSettings

    it "are the default network alone when it alone was changed" $
      changedSettings def def {configDefaultNetwork = bch} `shouldBe` noSettings {network = Just bch}

    it "are the default type alone when it alone was changed" $
      changedSettings def def {configDefaultType = Bip84} `shouldBe` noSettings {kind = Just Bip84}

    it "are the unit alone when it alone was changed" $
      changedSettings def def {configUnit = UnitSatoshi} `shouldBe` noSettings {unit = Just UnitSatoshi}

    it "are the view of the listings alone when it alone was changed" $
      changedSettings def def {configListingView = Table} `shouldBe` noSettings {view = Just Table}

    it "are the reverse order when the listings were reversed" $
      changedSettings def def {configListingReverse = True} `shouldBe` noSettings {listingOrder = Just ReverseOrder}

    it "are the usual order when reversed listings were put back" $
      changedSettings def {configListingReverse = True} def `shouldBe` noSettings {listingOrder = Just UsualOrder}

    it "are a journal to keep when the journal was turned on" $
      changedSettings def def {configJournal = True} `shouldBe` noSettings {journal = Just True}

    it "are no journal when the journal was turned off" $
      changedSettings def {configJournal = True} def `shouldBe` noSettings {journal = Just False}

    it "are no copy of the database when these copies were turned off" $
      changedSettings def def {configAutoBackupDatabase = False} `shouldBe` noSettings {autoBackupDatabase = Just False}

    it "are no copy of the labels when these copies were turned off" $
      changedSettings def def {configAutoBackupLabels = False} `shouldBe` noSettings {autoBackupLabels = Just False}

    it "are the address of the server alone when it alone was changed" $
      changedSettings def def {configHost = "http://localhost:3000/"} `shouldBe` noSettings {host = Just "http://localhost:3000/"}

    it "are the requests at once alone when they alone were changed" $
      changedSettings def def {configParallelRequests = 4} `shouldBe` noSettings {parallelRequests = Just 4}

    it "are the gap alone when it alone was changed" $
      changedSettings def def {configGap = 50} `shouldBe` noSettings {gap = Just 50}

    -- btc and bch were shown. bch is unchecked and btctest4 checked.
    it "show the networks that were checked, and hide those that were unchecked" $
      changedSettings def def {configShownNetworks = [btc, btcTest4]}
        `shouldBe` noSettings {showNetworks = [btcTest4], hideNetworks = [bch]}

    it "are an option for each of two settings that were changed" $
      changedSettings def def {configUnit = UnitBit, configGap = 30}
        `shouldBe` noSettings {unit = Just UnitBit, gap = Just 30}
