{-# LANGUAGE OverloadedStrings #-}

-- | The answers to the questions of setdefault without an option, once
-- they are given: the options of setdefault they stand for, one for each
-- setting the user changed. "Haskoin.Wallet.Workflow.SetDefaultSpec" asks
-- the questions themselves, with lines typed and keys pressed.
module Haskoin.Wallet.Cli.SetDefaultSpec (spec) where

import Data.Default (def)
import Haskoin (bch, btc, btcTest4)
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.Command (SetDefault (..), noDefaults)
import Haskoin.Wallet.Cli.Listing (ListingOrder (..))
import Haskoin.Wallet.Cli.SetDefault
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
  describe "The options that the answers to the questions of setdefault stand for" $ do
    it "are none when every answer is the value hw has" $
      changedDefaults def def `shouldBe` noDefaults

    it "are the default network alone when it alone was changed" $
      changedDefaults def def {configDefaultNetwork = bch} `shouldBe` noDefaults {network = Just bch}

    it "are the default type alone when it alone was changed" $
      changedDefaults def def {configDefaultType = Bip84} `shouldBe` noDefaults {kind = Just Bip84}

    it "are the unit alone when it alone was changed" $
      changedDefaults def def {configUnit = UnitSatoshi} `shouldBe` noDefaults {unit = Just UnitSatoshi}

    it "are the view of the listings alone when it alone was changed" $
      changedDefaults def def {configListingView = Table} `shouldBe` noDefaults {view = Just Table}

    it "are the reverse order when the listings were reversed" $
      changedDefaults def def {configListingReverse = True} `shouldBe` noDefaults {listingOrder = Just ReverseOrder}

    it "are the usual order when reversed listings were put back" $
      changedDefaults def {configListingReverse = True} def `shouldBe` noDefaults {listingOrder = Just UsualOrder}

    it "are a journal to keep when the journal was turned on" $
      changedDefaults def def {configJournal = True} `shouldBe` noDefaults {journal = Just True}

    it "are no journal when the journal was turned off" $
      changedDefaults def {configJournal = True} def `shouldBe` noDefaults {journal = Just False}

    it "are no copy of the database when these copies were turned off" $
      changedDefaults def def {configAutoBackupDatabase = False} `shouldBe` noDefaults {autoBackupDatabase = Just False}

    it "are no copy of the labels when these copies were turned off" $
      changedDefaults def def {configAutoBackupLabels = False} `shouldBe` noDefaults {autoBackupLabels = Just False}

    it "are the address of the server alone when it alone was changed" $
      changedDefaults def def {configHost = "http://localhost:3000/"} `shouldBe` noDefaults {host = Just "http://localhost:3000/"}

    it "are the requests at once alone when they alone were changed" $
      changedDefaults def def {configParallelRequests = 4} `shouldBe` noDefaults {parallelRequests = Just 4}

    it "are the gap alone when it alone was changed" $
      changedDefaults def def {configGap = 50} `shouldBe` noDefaults {gap = Just 50}

    -- btc and bch were shown. bch is unchecked and btctest4 checked.
    it "show the networks that were checked, and hide those that were unchecked" $
      changedDefaults def def {configShownNetworks = [btc, btcTest4]}
        `shouldBe` noDefaults {showNetworks = [btcTest4], hideNetworks = [bch]}

    it "are an option for each of two settings that were changed" $
      changedDefaults def def {configUnit = UnitBit, configGap = 30}
        `shouldBe` noDefaults {unit = Just UnitBit, gap = Just 30}
