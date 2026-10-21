{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.CoinSelectionSpec where

import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.CoinSelection
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    coinSelectionSpec ctx
    changeAndFeeSpec ctx
    wasteSpec ctx
    txSizeSpec

-- Every coin pays the fee of its own input (148 bytes) and the target adds
-- the fee of the frame (10 bytes) and of the outputs (34 bytes each). With
-- one output at 10 satoshis per byte: a coin is worth its amount - 1480, and
-- the target to cover is the amount + 440. The long-term fee rate is 10, the
-- value Bitcoin Core uses.

-- | Coins of the test wallet with the given amounts
coinsWorth :: Ctx -> [Natural] -> [Store.Unspent]
coinsWorth ctx values = coinsOf ctx $ fundingTx ctx 1 $ zip (externalAddr <$> [0 ..]) values

-- | A selection as its amounts: the coins, the change and the fee
amounts :: Selection -> ([Word64], Word64, Word64)
amounts s = ((.value) <$> s.selected, s.changeAmount, s.feeAmount)

-- | Select for one output paid by the sender, with the given seed, fee rate
-- and dust
selectWithSeed :: Ctx -> Int -> CoinSelection -> Word64 -> Word64 -> Word64 -> [Natural] -> Either String ([Word64], Word64, Word64)
selectWithSeed ctx seed strategy feeRate dust amount coins =
  amounts <$> selectCoins strategy (mkStdGen seed) (Payment (FeeRates feeRate 10) SenderPays dust 1 amount) (coinsWorth ctx coins)

-- | Select with the seed 0; only the random draw depends on it
select :: Ctx -> CoinSelection -> Word64 -> Word64 -> Word64 -> [Natural] -> Either String ([Word64], Word64, Word64)
select ctx = selectWithSeed ctx 0

coinSelectionSpec :: Ctx -> Spec
coinSelectionSpec ctx =
  describe "Coin selection" $ do
    -- Four coins, 350000 to send. The largest coin covers it alone
    -- (400000 - 1480 >= 350440); the three smallest together too.
    let four = [100000, 200000, 300000, 400000]
    it "spends the largest coins first" $
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 2 = 452
      -- Change: 400000 - 350000 - 452 = 49548
      select ctx LargestFirst 2 546 350000 four `shouldBe` Right ([400000], 49548, 452)
    it "spends the smallest coins first" $
      -- Fee: 10 + 3 * 148 + 2 * 34 = 522 bytes * 50 = 26100
      -- Change: 100000 + 200000 + 300000 - 350000 - 26100 = 223900
      select ctx SmallestFirst 50 546 350000 four `shouldBe` Right ([100000, 200000, 300000], 223900, 26100)
    -- Three coins drawn at random: the seed decides their order and the
    -- draw stops as soon as the coins cover the target
    let three = [300000, 100000, 400000]
    it "draws coins at random, in the order of the seed" $
      -- Fee: 10 + 2 * 148 + 2 * 34 = 374 bytes * 10 = 3740
      -- Change: 100000 + 400000 - 350000 - 3740 = 146260
      selectWithSeed ctx 0 RandomDraw 10 546 350000 three `shouldBe` Right ([100000, 400000], 146260, 3740)
    it "draws other coins with another seed" $
      -- Fee: 10 + 2 * 148 + 2 * 34 = 374 bytes * 10 = 3740
      -- Change: 300000 + 400000 - 350000 - 3740 = 346260
      selectWithSeed ctx 1 RandomDraw 10 546 350000 three `shouldBe` Right ([300000, 400000], 346260, 3740)
    it "draws a single coin when the seed puts the largest one first" $
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 10 = 2260
      -- Change: 400000 - 350000 - 2260 = 47740
      selectWithSeed ctx 4 RandomDraw 10 546 350000 three `shouldBe` Right ([400000], 47740, 2260)
    it "wastes the least: few inputs when the fee rate is high" $
      -- Spending an input costs 148 * (50 - 10) = 5920 more than it will;
      -- one input beats three
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 50 = 11300
      -- Change: 400000 - 350000 - 11300 = 38700
      select ctx LeastWaste 50 546 350000 four `shouldBe` Right ([400000], 38700, 11300)
    it "wastes the least: consolidates when the fee rate is low" $
      -- Spending an input saves 148 * (10 - 2) = 1184 against the future;
      -- three inputs beat one
      -- Fee: 10 + 3 * 148 + 2 * 34 = 522 bytes * 2 = 1044
      -- Change: 100000 + 200000 + 300000 - 350000 - 1044 = 248956
      select ctx LeastWaste 2 546 350000 four `shouldBe` Right ([100000, 200000, 300000], 248956, 1044)
    it "wastes the least: prefers the fewest inputs when the fee rate is the long-term one" $
      -- An input costs 148 * (10 - 10) = 0 now or later: every proposal with
      -- a change output wastes 1820 and the largest-first one, tried first, wins
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 10 = 2260
      -- Change: 400000 - 350000 - 2260 = 47740
      select ctx LeastWaste 10 546 350000 four `shouldBe` Right ([400000], 47740, 2260)
    it "wastes the least: an exact match avoids the change output" $
      -- 300000 + 53400 - 2 * 1480 = 350440, the target exactly: no change output
      -- Fee: 10 + 2 * 148 + 34 = 340 bytes * 10 = 3400
      -- Change: 300000 + 53400 - 350000 - 3400 = 0
      select ctx LeastWaste 10 546 350000 [400000, 300000, 53400] `shouldBe` Right ([300000, 53400], 0, 3400)
    it "ignores a coin worth less than the fee of its input" $
      -- 1000 - 1480 < 0: the coin is left out although it comes first
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 10 = 2260
      -- Change: 400000 - 350000 - 2260 = 47740
      select ctx SmallestFirst 10 546 350000 [1000, 400000] `shouldBe` Right ([400000], 47740, 2260)
    it "fails when the coins do not cover the amount and the fee" $
      -- 100000 + 200000 + 300000 + 400000 - 4 * 1480 = 994080 < 1000000 + 440
      select ctx LeastWaste 10 546 1000000 four `shouldBe` Left "The coins do not cover the amount and the fee"
    it "fails without coins" $
      -- Nothing covers 1 + 440
      select ctx LeastWaste 10 546 1 [] `shouldBe` Left "The coins do not cover the amount and the fee"

changeAndFeeSpec :: Ctx -> Spec
changeAndFeeSpec ctx =
  describe "Change and fee" $ do
    -- 350000 to send at 1 satoshi per byte, with the dust limit at 546
    -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 1 = 226
    it "pays a change above the dust limit" $
      -- Change: 350773 - 350000 - 226 = 547
      select ctx LargestFirst 1 546 350000 [350773] `shouldBe` Right ([350773], 547, 226)
    it "pays a change exactly at the dust limit" $
      -- Change: 350772 - 350000 - 226 = 546
      select ctx LargestFirst 1 546 350000 [350772] `shouldBe` Right ([350772], 546, 226)
    it "gives a change below the dust limit to the fee" $
      -- Change: 350771 - 350000 - 226 = 545: no change output, and the fee
      -- is 350771 - 350000 = 771, the 226 of the transaction and the 545
      select ctx LargestFirst 1 546 350000 [350771] `shouldBe` Right ([350771], 0, 771)
    -- A dust limit of 0 pays any change, except a change of nothing
    it "pays a change of one satoshi with no dust limit" $
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 10 = 2260
      -- Change: 352261 - 350000 - 2260 = 1
      select ctx LargestFirst 10 0 350000 [352261] `shouldBe` Right ([352261], 1, 2260)
    it "never pays a change of zero, even with no dust limit" $
      -- Change: 352260 - 350000 - 2260 = 0: no change output, and the fee
      -- is 352260 - 350000 = 2260, the 1920 of the transaction and the 340
      select ctx LargestFirst 10 0 350000 [352260] `shouldBe` Right ([352260], 0, 2260)
    it "charges the fee of every output" $
      -- Fee: two outputs and the change, 10 + 148 + 3 * 34 = 260 bytes * 10 = 2600
      -- Change: 400000 - 350000 - 2600 = 47400
      amounts <$> selectCoins LargestFirst (mkStdGen 0) (Payment (FeeRates 10 10) SenderPays 546 2 350000) (coinsWorth ctx [400000])
        `shouldBe` Right ([400000], 47400, 2600)
    it "builds a free transaction at a fee rate of zero" $
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 0 = 0
      -- Change: 400000 - 350000 - 0 = 50000
      select ctx LargestFirst 0 546 350000 [400000] `shouldBe` Right ([400000], 50000, 0)

wasteSpec :: Ctx -> Spec
wasteSpec ctx =
  describe "Waste of a selection" $ do
    -- 350000 paid by the sender in one output, at the given fee rate
    let payment feeRate = Payment (FeeRates feeRate 10) SenderPays 546 1 350000
        selection coins change fee = Selection (coinsWorth ctx coins) 350000 change fee
    it "is the cost of the change output at the long-term fee rate" $
      -- Creating the output now (10 * 34) and spending it later (10 * 148)
      waste (payment 10) (selection [400000] 47740 2260) `shouldBe` 1820
    it "is the excess paid as fee without change" $
      -- 400000 - 350000 = 50000 of fee, 48080 beyond the 10 + 148 + 34 = 192
      -- bytes * 10 = 1920 of the transaction
      waste (payment 10) (selection [400000] 0 50000) `shouldBe` 48080
    it "is zero for an exact match" $
      -- 3400 of fee for 10 + 2 * 148 + 34 = 340 bytes * 10: nothing to spare
      waste (payment 10) (selection [300000, 53400] 0 3400) `shouldBe` 0
    it "is negative when consolidating is cheaper now than later" $
      -- Three inputs at 148 * (2 - 10) = -1184 each, plus a change output
      -- costing 2 * 34 + 10 * 148 = 1548
      waste (payment 2) (selection [100000, 200000, 300000] 248956 1044) `shouldBe` (-2004)
    it "is positive when inputs are expensive now" $
      -- One input at 148 * (50 - 10) = 5920, plus a change output costing
      -- 50 * 34 + 10 * 148 = 3180
      waste (payment 50) (selection [400000] 38700 11300) `shouldBe` 9100

txSizeSpec :: Spec
txSizeSpec =
  describe "Transaction size" $ do
    it "is 10 bytes of frame, 148 per input and 34 per output" $ do
      txSize 1 1 `shouldBe` 192
      txSize 1 2 `shouldBe` 226
      txSize 2 2 `shouldBe` 374
      txSize 3 2 `shouldBe` 522
      txSize 1 3 `shouldBe` 260
    it "is the frame alone without inputs and outputs" $
      txSize 0 0 `shouldBe` 10
    it "counts 3 bytes for a count of 253 or more" $ do
      -- 252 inputs: 10 + 252 * 148 + 34; 253 inputs: 12 + 253 * 148 + 34
      txSize 252 1 `shouldBe` 37340
      txSize 253 1 `shouldBe` 37490
      -- The same for the outputs: 10 + 148 + 252 * 34, then 12 + 148 + 253 * 34
      txSize 1 252 `shouldBe` 8726
      txSize 1 253 `shouldBe` 8762
    it "counts 5 bytes for a count above 65535" $ do
      -- 11 + 65535 * 148 + 34, then 13 + 65536 * 148 + 34
      txSize 65535 1 `shouldBe` 9699226
      txSize 65536 1 `shouldBe` 9699376
    it "counts 9 bytes for a count above 4294967295" $ do
      -- No such transaction exists, but the encoding rule holds:
      -- 8 + 5 + 1 + 4294967295 * 148, then 8 + 9 + 1 + 4294967296 * 148
      txSize 4294967295 0 `shouldBe` 635655159674
      txSize 4294967296 0 `shouldBe` 635655159826
