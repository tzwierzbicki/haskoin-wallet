{-# LANGUAGE OverloadedStrings #-}

-- | The groups of coins a transaction joins, by the tags of the addresses
-- its coins are at, and when a replacement that adds coins joins one more.
--
-- The transaction of the tests spends up to four coins of the test
-- account: 50000 satoshis at /0/0, 30000 at /0/1, 20000 at /0/2 and
-- 10000 at /0/3, each the output 0 of a transaction of its own.
module Haskoin.Wallet.Tx.MixingSpec (spec) where

import qualified Data.Map.Strict as Map
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (legacyPath)
import Haskoin.Wallet.Tags (Tag, plainTag)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.Mixing
import Numeric.Natural (Natural)
import Test.Hspec

-- | The coin at the receiving address of the index: the output 0 of the
-- transaction whose id ends with the index plus one
coinAt :: Int -> OutPoint
coinAt index = OutPoint (fakeTxHash (fromIntegral index + 1)) 0

-- | A transaction that spends the coins at the receiving addresses of the
-- indexes given, each with its value and the tags of its address
spending :: [(Int, Natural, [Tag])] -> TxInfo
spending coins =
  (txOnChain summary 9 (Store.BlockRef 800001 1) 1 False "")
    { txInfoTags = noTxTags {txAddressTags = Map.fromList [(externalAddr index, plainTag <$> tags) | (index, _, tags) <- coins, not (null tags)]}
    }
  where
    summary =
      spendSummary
        { summaryMyInputs =
            Map.fromList
              [ (externalAddr index, MyInputs value (legacyPath (Deriv :/ 0 :/ fromIntegral index)) "" [coinAt index])
                | (index, value, _) <- coins
              ]
        }

-- | The group of the coins at the receiving addresses of the indexes
-- given, with its tags and what the coins are worth together
groupOf :: [Tag] -> [Int] -> Natural -> SpentGroup
groupOf tags indexes = SpentGroup (plainTag <$> tags) (coinAt <$> indexes)

spec :: Spec
spec = do
  describe "The groups of the coins a transaction spends" $ do
    -- 50000 + 30000 = 80000 for the two coins tagged kyc
    it "are the coins at addresses with the same tags, each group with what its coins are worth" $
      spentGroups (spending [(0, 50000, [kycTag]), (1, 30000, [kycTag]), (2, 20000, [coldTag])])
        `shouldBe` [groupOf [coldTag] [2] 20000, groupOf [kycTag] [0, 1] 80000]

    it "have the coins at addresses without a tag as a group of their own, after the others" $
      spentGroups (spending [(0, 50000, []), (1, 30000, [kycTag]), (2, 20000, [])])
        `shouldBe` [groupOf [kycTag] [1] 30000, groupOf [] [0, 2] 70000]

    it "tell a coin with two tags from a coin with one of them" $
      spentGroups (spending [(0, 50000, [kycTag, coldTag]), (1, 30000, [kycTag])])
        `shouldBe` [groupOf [coldTag, kycTag] [0] 50000, groupOf [kycTag] [1] 30000]

    it "take the same tags in another order for the same tags" $
      spentGroups (spending [(0, 50000, [kycTag, coldTag]), (1, 30000, [coldTag, kycTag])])
        `shouldBe` [groupOf [coldTag, kycTag] [0, 1] 80000]

  describe "A transaction that joins groups" $ do
    it "spends coins of two groups" $
      joinsGroups [groupOf [kycTag] [0] 50000, groupOf [coldTag] [1] 30000] `shouldBe` True

    it "is not one whose coins are all of one group" $
      joinsGroups [groupOf [kycTag] [0, 1] 80000] `shouldBe` False

    it "is not one whose coins have no tag at all" $
      joinsGroups [groupOf [] [0, 1] 80000] `shouldBe` False

  -- The transaction replaced spent the coin at /0/0, and those at /0/1
  -- and /0/2 when the test says so. The coin at /0/3 is added.
  describe "A coin added to a transaction" $ do
    it "joins a group to it when no coin it spent already has the tags of the coin added" $
      addsGroup [coinAt 3] [groupOf [kycTag] [0] 50000, groupOf [coldTag] [3] 10000] `shouldBe` True

    it "joins no group to it when the coin added has the tags of a coin it spent already" $
      addsGroup [coinAt 3] [groupOf [kycTag] [0, 3] 60000] `shouldBe` False

    it "joins no group to a transaction that joined two already, when it has the tags of one of them" $
      addsGroup [coinAt 3] [groupOf [kycTag] [0, 3] 60000, groupOf [coldTag] [1] 30000] `shouldBe` False

    it "joins a group to a transaction that joined two already, when it has the tags of neither" $
      addsGroup [coinAt 3] [groupOf [kycTag] [0] 50000, groupOf [coldTag] [1] 30000, groupOf [] [3] 10000] `shouldBe` True

    it "joins a group when it has no tag and the coins spent already have one" $
      addsGroup [coinAt 3] [groupOf [kycTag] [0] 50000, groupOf [] [3] 10000] `shouldBe` True
