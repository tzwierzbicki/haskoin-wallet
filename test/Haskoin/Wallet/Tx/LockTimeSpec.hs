{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The version, the lock time and the input sequences of the transactions
-- hw builds, which make them look like those of Bitcoin Core and Electrum,
-- and when a wallet counts as behind the chain.
module Haskoin.Wallet.Tx.LockTimeSpec (spec) where

import Data.List (unfoldr)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32)
import Haskoin
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.LockTime
import System.Random (StdGen, mkStdGen)
import Test.Hspec

spec :: Spec
spec = do
  headerSpec
  lockTimeSpec
  staleSpec

{- The header -}

headerSpec :: Spec
headerSpec =
  describe "The header of the transactions hw builds" $ do
    it "is version 2, the version of Bitcoin Core, Electrum and the Bitcoin Cash Node wallet" $
      txVersion `shouldBe` 2

    -- 0xfffffffd, the highest sequence BIP 125 reads as replaceable
    it "marks a transaction of Bitcoin, or of its test networks, replaceable" $
      inputSequence <$> [btc, btcTest, btcRegTest] `shouldBe` [0xfffffffd, 0xfffffffd, 0xfffffffd]

    -- 0xfffffffe, the highest sequence that lets a lock time apply, which
    -- Electron Cash and the Bitcoin Cash Node wallet write
    it "marks a transaction of Bitcoin Cash not replaceable, with a sequence the lock time applies under" $
      inputSequence <$> [bch, bchTest, bchRegTest] `shouldBe` [0xfffffffe, 0xfffffffe, 0xfffffffe]

    it "replaces the version, the lock time and the sequence of every input of a transaction, which keeps all the rest" $ do
      let built = Tx 1 [TxIn (OutPoint (fakeTxHash 1) 0) "\x51" maxBound, TxIn (OutPoint (fakeTxHash 2) 3) "" maxBound] [TxOut 1000 "\x6a"] [] 0
      withHeader btc 814037 built
        `shouldBe` Tx 2 [TxIn (OutPoint (fakeTxHash 1) 0) "\x51" 0xfffffffd, TxIn (OutPoint (fakeTxHash 2) 3) "" 0xfffffffd] [TxOut 1000 "\x6a"] [] 814037

{- The lock time -}

-- | The lock times drawn one after the other from the same generator
drawn :: Int -> Maybe BlockHeight -> StdGen -> [Word32]
drawn n height gen = take n $ unfoldr (Just . lockTimeFor height) gen

lockTimeSpec :: Spec
lockTimeSpec =
  describe "The lock time of a new transaction" $ do
    it "is 0 without a best block to draw it from" $
      drawn 10 Nothing (mkStdGen 1) `shouldBe` replicate 10 0

    it "is the height of the best block, or up to 99 blocks below it, never above" $ do
      let lockTimes = drawn 10000 (Just 814037) (mkStdGen 1)
      maximum lockTimes `shouldBe` 814037
      minimum lockTimes `shouldSatisfy` (>= 814037 - 99)

    -- One draw in ten goes back, by up to 99 blocks: of 10000 draws, about
    -- 1000, and about 10 of them by 0, which cannot be told apart from the
    -- draws that stay
    it "goes below the best block about one time in ten, as Electrum and Bitcoin Core draw it" $ do
      let lockTimes = drawn 10000 (Just 814037) (mkStdGen 1)
          below = length (filter (< 814037) lockTimes)
      below `shouldSatisfy` (\n -> n > 850 && n < 1150)

    -- About 1000 draws go back, by 0 to 99 blocks: each of the 99 heights
    -- below the best block, 813938 to 814036, is drawn, and none lower
    it "spreads the draws that go below over the 99 blocks" $ do
      let lockTimes = drawn 10000 (Just 814037) (mkStdGen 1)
      Set.fromList (filter (< 814037) lockTimes) `shouldBe` Set.fromList [813938 .. 814036]

    it "never goes below 0 from a best block near the start of the chain" $ do
      let lockTimes = drawn 10000 (Just 50) (mkStdGen 1)
      minimum lockTimes `shouldSatisfy` (>= 0)
      maximum lockTimes `shouldBe` 50
      minimum lockTimes `shouldBe` 0

{- Behind the chain -}

staleSpec :: Spec
staleSpec =
  describe "A wallet behind the chain" $ do
    -- 1800000000 is 2027-01-15 08:00:00 UTC; 8 hours are 28800 seconds
    it "has a best block mined more than eight hours before, by its header" $ do
      let now = posixSecondsToUTCTime 1800000000
      isStale now (Just (1800000000 - 28800)) `shouldBe` False
      isStale now (Just (1800000000 - 28801)) `shouldBe` True

    it "is one whose best block was mined at a time it does not know, as an older hw stored it" $
      isStale (posixSecondsToUTCTime 1800000000) Nothing `shouldBe` True

    it "is not one whose best block is dated after now, which a miner's clock allows" $
      isStale (posixSecondsToUTCTime 1800000000) (Just 1800003600) `shouldBe` False
