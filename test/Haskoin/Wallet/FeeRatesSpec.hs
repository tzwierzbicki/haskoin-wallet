{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.FeeRatesSpec where

import Control.Monad.Except (runExceptT, throwError)
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32, Word64, Word8)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.FeeRates
import Haskoin.Wallet.PrettyPrinter (agoText)
import Haskoin.Wallet.Store (Store (..))
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = do
  ratesSpec
  storeSpec
  choiceSpec
  agoSpec

-- | The rates of blocks and their transactions, computed
ratesSpec :: Spec
ratesSpec =
  describe "The fee rates recent blocks paid" $ do
    -- The bytes sorted by rate: 300 at 10, 100 at 20, 100 at 50. A quarter
    -- of the 500 bytes is 125 and half is 250, both within the first 300:
    -- the low and the median rate are 10. Nine tenths is 450, reached by
    -- the last 100: the high rate is 50.
    it "weigh each rate by the bytes it bought" $
      feeRatesOf [(blockAt 800000 1, [txPaying 1 3000 1200, txPaying 2 2000 400, txPaying 3 5000 400])]
        `shouldBe` Just (FeeRates 10 10 50 800000 800000 1700000000)

    -- 1000 satoshis for 85 bytes is 11.76 per byte
    it "round a rate up to a whole satoshi, so that paying it pays at least as much" $
      feeRatesOf [(blockAt 800000 1, [txPaying 1 1000 340])]
        `shouldBe` Just (FeeRates 12 12 12 800000 800000 1700000000)

    -- A weight of 6 is 2 virtual bytes, not 1: 3 satoshis for 2 bytes is
    -- 1.5 per byte
    it "count the weight in quarters, rounded up, as Bitcoin Core does" $
      feeRatesOf [(blockAt 800000 1, [txPaying 1 3 6])]
        `shouldBe` Just (FeeRates 2 2 2 800000 800000 1700000000)

    it "are at least 1 satoshi per byte" $
      feeRatesOf [(blockAt 800000 1, [txPaying 1 1 4000])]
        `shouldBe` Just (FeeRates 1 1 1 800000 800000 1700000000)

    it "leave the coinbase out, which pays no fee" $
      feeRatesOf [(blockAt 800000 1, [txPaying 1 0 1000, txPaying 2 500 400])]
        `shouldBe` Just (FeeRates 5 5 5 800000 800000 1700000000)

    it "are nothing for blocks without a transaction paying a fee" $ do
      feeRatesOf [(blockAt 800000 1, [txPaying 1 0 1000])] `shouldBe` Nothing
      feeRatesOf [] `shouldBe` Nothing

    it "name the blocks they come from, and the time of the latest" $
      feeRatesOf [(blockMined 800003 1700000300, [txPaying 1 400 400]), (blockMined 800001 1700000100, [txPaying 2 400 400])]
        `shouldBe` Just (FeeRates 4 4 4 800001 800003 1700000300)

-- | The rates read from the store
storeSpec :: Spec
storeSpec =
  describe "The fee rates read from the store" $ do
    -- The blocks 800009 to 800004 each hold one transaction of 100 bytes,
    -- at 9 to 4 satoshis per byte. The block above them is empty and the
    -- one below, at 3 per byte, is the seventh with fees: neither counts.
    -- Of the 600 bytes, a quarter is reached at 5, half at 6 and nine
    -- tenths at 9.
    it "come from the newest six blocks holding fees, however many empty blocks are among them" $ do
      let blocks = [withFees 800010 0] <> [withFees height (fromIntegral height - 800000) | height <- [800009, 800008 .. 800002]]
          txs = [(hashOf block, [txPaying (fromIntegral block.height) (100 * block.fee) 400]) | block <- blocks, block.fee > 0]
      rates <- runExceptT $ recentFeeRates 2 (toStore emptyStore {fakeLatestBlocks = blocks, fakeBlockTxs = txs})
      rates `shouldBe` Right (FeeRates 5 6 9 800004 800009 1700000000)

    it "fail when no recent block holds a fee" $ do
      rates <- runExceptT $ recentFeeRates 2 (toStore emptyStore {fakeLatestBlocks = [withFees 800010 0, withFees 800009 0]})
      rates `shouldBe` Left "No recent block of the store holds a transaction paying a fee"

    it "fail as the store does when it cannot be reached" $ do
      rates <- runExceptT $ recentFeeRates 2 (toStore emptyStore) {storeLatestBlocks = throwError "The store could not be reached"}
      rates `shouldBe` Left "The store could not be reached"
  where
    -- A block whose fees, as the store sums them, are the given satoshis
    withFees :: BlockHeight -> Word64 -> Store.BlockData
    withFees height fees = blockMinedAt height (fromIntegral height) 1700000000 fees
    hashOf :: Store.BlockData -> BlockHash
    hashOf block = headerHash block.header

-- | The --fee-rate option, read
choiceSpec :: Spec
choiceSpec =
  describe "A fee rate choice" $ do
    it "is a level by its name" $ do
      readFeeRateChoice "low" `shouldBe` Right (FeeLevelOf Low)
      readFeeRateChoice "median" `shouldBe` Right (FeeLevelOf Median)
      readFeeRateChoice "high" `shouldBe` Right (FeeLevelOf High)

    it "is a number of satoshis per byte" $
      readFeeRateChoice "7" `shouldBe` Right (FeeRateOf 7)

    it "is nothing else" $
      readFeeRateChoice "fast" `shouldBe` Left "Give a number of satoshis per byte, or low, median or high"

-- | How long ago a block was mined, in words
agoSpec :: Spec
agoSpec =
  describe "The age of a block, in words" $ do
    it "counts minutes up to two hours, hours up to two days, then days" $ do
      ago 30 `shouldBe` "less than a minute ago"
      ago 60 `shouldBe` "1 minute ago"
      ago 180 `shouldBe` "3 minutes ago"
      ago 7199 `shouldBe` "119 minutes ago"
      ago 7200 `shouldBe` "2 hours ago"
      ago 172799 `shouldBe` "47 hours ago"
      ago 172800 `shouldBe` "2 days ago"

    it "reads a time to come as just past" $
      ago (-60) `shouldBe` "less than a minute ago"
  where
    now = posixSecondsToUTCTime 1700000000
    ago seconds = agoText now (addUTCTime (negate seconds) now)

{- Fixtures -}

-- | A transaction of a block paying the fee, of the weight. The rest of it
-- does not matter to the rates.
txPaying :: Word8 -> Word64 -> Word32 -> Store.Transaction
txPaying n fee weight =
  Store.Transaction
    { Store.block = Store.BlockRef 800000 0,
      Store.version = 1,
      Store.locktime = 0,
      Store.inputs = [],
      Store.outputs = [],
      Store.deleted = False,
      Store.rbf = False,
      Store.timestamp = 0,
      Store.txid = fakeTxHash n,
      Store.size = weight `div` 4,
      Store.weight = weight,
      Store.fee = fee
    }

-- | A block of the main chain mined at the Unix time
blockMined :: BlockHeight -> Word32 -> Store.BlockData
blockMined height time = blockMinedAt height (fromIntegral height) time 0
