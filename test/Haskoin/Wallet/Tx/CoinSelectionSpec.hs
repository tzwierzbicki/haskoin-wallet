{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Tx.CoinSelectionSpec where

import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Tx.CoinSelection
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.Size (accountInput, accountOutputKind, outputDustLimit, txSize)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (choose, elements, forAll, label, listOf1)
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    coinSelectionSpec ctx
    coinKindSpec ctx
    bothTypesSpec ctx
    changeAndFeeSpec ctx
    wasteSpec ctx
    wholeTransactionSpec ctx
    selectionPropertySpec ctx

-- Every coin pays the fee of its own input and the target adds the fee of
-- the frame (10 bytes) and of the outputs (34 bytes each). The coins of
-- 'coinsWorth' are P2PKH coins, of a bip44 account, whose input is 148
-- bytes. With one output at 10 satoshis per virtual byte: such a coin is
-- worth its amount - 1480, and the target to cover is the amount + 440.
-- The coins of 'segwitCoinsWorth' are P2WPKH coins, of a bip84 account,
-- whose input is 68 virtual bytes: 41 bytes and a quarter of its 108-byte
-- witness. A transaction with such an input has 2 more bytes of witness
-- data, and 1 for each P2PKH input beside it, which count a quarter too.
-- The long-term fee rate is 10, the value Bitcoin Core uses.

-- | A selection as its amounts: the coins, the change and the fee
amounts :: Selection -> ([Word64], Word64, Word64)
amounts s = ((.value) <$> s.selected, s.changeAmount, s.feeAmount)

-- | Select for one output paid by the sender, with the given seed, fee rate
-- and dust
selectWithSeed :: Ctx -> Int -> CoinSelection -> Word64 -> Word64 -> Word64 -> [Natural] -> Either String ([Word64], Word64, Word64)
selectWithSeed ctx seed strategy feeRate dust amount coins =
  amounts <$> selectCoins strategy (mkStdGen seed) (Payment Bip44 (FeeRates feeRate 10) SenderPays dust [OutputPKHash] amount) (coinsWorth ctx coins)

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
    -- The input of a bip84 coin is 68 virtual bytes, a bip84 change output 31
    it "sizes a bip84 coin as a P2WPKH input, and change of the type bip84 as a P2WPKH output" $
      -- Fee: 10 + 41 + 34 + 31 = 116 bytes, then (2 + 108) / 4 rounded up to
      -- 28: 144 virtual bytes * 2 = 288. Change: 400000 - 350000 - 288 = 49712
      (amounts <$> selectCoins LargestFirst (mkStdGen 0) (Payment Bip84 (FeeRates 2 10) SenderPays 546 [OutputPKHash] 350000) (segwitCoinsWorth ctx four))
        `shouldBe` Right ([400000], 49712, 288)
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

{- The type of a coin -}

coinKindSpec :: Ctx -> Spec
coinKindSpec ctx =
  describe "The type of a coin" $ do
    it "is bip44 for a coin at a P2PKH address, whose input is P2PKH" $ do
      (coinKind <$> coinsWorth ctx [50000]) `shouldBe` [Bip44]
      (coinInput <$> coinsWorth ctx [50000]) `shouldBe` [InputPKHash]
    it "is bip84 for a coin at a P2WPKH address, whose input is P2WPKH" $ do
      (coinKind <$> segwitCoinsWorth ctx [50000]) `shouldBe` [Bip84]
      (coinInput <$> segwitCoinsWorth ctx [50000]) `shouldBe` [InputWitnessPKHash]
    -- The larger input, so that the fee of the coin is never too low
    it "is bip44 for a coin the store lists without an address" $
      (coinKind . withoutAddress <$> segwitCoinsWorth ctx [50000]) `shouldBe` [Bip44]
    it "is told for several coins, each type once, bip44 first" $ do
      coinKinds (segwitCoinsWorth ctx [30000, 20000] <> coinsWorth ctx [50000]) `shouldBe` [Bip44, Bip84]
      coinKinds (segwitCoinsWorth ctx [30000, 20000]) `shouldBe` [Bip84]
      coinKinds [] `shouldBe` []
  where
    withoutAddress :: Store.Unspent -> Store.Unspent
    withoutAddress coin =
      Store.Unspent
        { Store.block = coin.block,
          Store.outpoint = coin.outpoint,
          Store.value = coin.value,
          Store.script = coin.script,
          Store.address = Nothing
        }

{- Coins of both types -}

-- A payment of 250000 to one P2PKH output at 10 satoshis per virtual
-- byte, with change of the type bip44, from the coins of a portfolio: a
-- P2PKH coin of its bip44 account and P2WPKH coins of its bip84 account
bothTypesSpec :: Ctx -> Spec
bothTypesSpec ctx =
  describe "A selection among coins of both types" $ do
    let payment = Payment Bip44 (FeeRates 10 10) SenderPays 546 [OutputPKHash] 250000
    it "sizes each coin by its own type: 148 bytes for the P2PKH coin and 68 for the P2WPKH one" $
      -- 10 + 148 + 41 + 2 * 34 = 267 bytes, then (2 + 1 + 108) / 4 of
      -- witness data, rounded up to 28: 295 virtual bytes * 10 = 2950
      -- Change: 200000 + 100000 - 250000 - 2950 = 47050
      (amounts <$> selectCoins SmallestFirst (mkStdGen 0) payment (coinsWorth ctx [200000] <> segwitCoinsWorth ctx [100000]))
        `shouldBe` Right ([100000, 200000], 47050, 2950)
    it "spends the chosen coins of both types together, each sized by its own type" $
      (amounts <$> spendCoins payment (coinsWorth ctx [200000] <> segwitCoinsWorth ctx [100000]))
        `shouldBe` Right ([200000, 100000], 47050, 2950)
    -- At 10 satoshis per virtual byte the input of a P2WPKH coin costs 680
    -- and that of a P2PKH coin 1480: a coin of 1000 is worth 320 as the
    -- first and nothing as the second
    it "spends a bip84 coin of 1000, worth more than the 680 of its input" $
      -- 295 virtual bytes * 10 = 2950
      -- Change: 1000 + 400000 - 350000 - 2950 = 48050
      (amounts <$> selectCoins SmallestFirst (mkStdGen 0) payment {amount = 350000} (segwitCoinsWorth ctx [1000] <> coinsWorth ctx [400000]))
        `shouldBe` Right ([1000, 400000], 48050, 2950)
    it "leaves a bip44 coin of 1000, worth less than the 1480 of its input, beside a bip84 coin" $
      -- The bip84 coin alone: 10 + 41 + 2 * 34 = 119 bytes, then (2 + 108)
      -- / 4 rounded up to 28: 147 virtual bytes * 10 = 1470
      -- Change: 400000 - 350000 - 1470 = 48530
      (amounts <$> selectCoins SmallestFirst (mkStdGen 0) payment {amount = 350000} (coinsWorth ctx [1000] <> segwitCoinsWorth ctx [400000]))
        `shouldBe` Right ([400000], 48530, 1470)
    -- The same coins and the same payment, with change of the type bip84:
    -- a P2WPKH output of 31 bytes, 3 less than a P2PKH one
    it "sizes the change by its own type, whatever the types of the coins" $
      -- 292 virtual bytes * 10 = 2920
      -- Change: 200000 + 100000 - 250000 - 2920 = 47080
      (amounts <$> spendCoins payment {changeKind = Bip84, dust = 294} (coinsWorth ctx [200000] <> segwitCoinsWorth ctx [100000]))
        `shouldBe` Right ([200000, 100000], 47080, 2920)

changeAndFeeSpec :: Ctx -> Spec
changeAndFeeSpec ctx =
  describe "Change and fee" $ do
    -- 350000 to send at 1 satoshi per virtual byte, with the dust limit at 546
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
      amounts <$> selectCoins LargestFirst (mkStdGen 0) (Payment Bip44 (FeeRates 10 10) SenderPays 546 [OutputPKHash, OutputPKHash] 350000) (coinsWorth ctx [400000])
        `shouldBe` Right ([400000], 47400, 2600)
    it "builds a free transaction at a fee rate of zero" $
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 0 = 0
      -- Change: 400000 - 350000 - 0 = 50000
      select ctx LargestFirst 0 546 350000 [400000] `shouldBe` Right ([400000], 50000, 0)

wasteSpec :: Ctx -> Spec
wasteSpec ctx =
  describe "Waste of a selection" $ do
    -- 350000 paid by the sender in one output, at the given fee rate
    let payment feeRate = Payment Bip44 (FeeRates feeRate 10) SenderPays 546 [OutputPKHash] 350000
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
    it "counts a bip84 coin at the 68 virtual bytes of its input, and bip84 change at 31 to create and 68 to spend" $
      -- One input at 68 * (50 - 10) = 2720, plus a change output costing
      -- 50 * 31 + 10 * 68 = 2230
      waste (Payment Bip84 (FeeRates 50 10) SenderPays 294 [OutputPKHash] 350000) (Selection (segwitCoinsWorth ctx [400000]) 350000 42800 7200)
        `shouldBe` 4950
    it "counts each input of a selection of both types at its own size" $
      -- Inputs of 148 + 68 = 216 bytes at 50 - 10 = 40 each: 8640, plus a
      -- bip44 change output costing 50 * 34 + 10 * 148 = 3180
      waste (payment 50) (Selection (coinsWorth ctx [200000] <> segwitCoinsWorth ctx [200000]) 350000 35250 14750)
        `shouldBe` 11820

-- The fee must pay the rate on the whole transaction, which is a little
-- more than its parts. One bip84 input and one P2WPKH output: 10 + 68 + 31
-- = 109 virtual bytes counted part by part, and 110 for the whole, since
-- the 2 bytes of marker and flag of the witness are half a virtual byte,
-- rounded up: 110 * 10 = 1100. And 260 legacy inputs take 3 bytes to count
-- them, not 1: 12 + 260 * 148 + 34 = 38526 bytes, 385260 at 10.
wholeTransactionSpec :: Ctx -> Spec
wholeTransactionSpec ctx =
  describe "A payment the coins cover without change" $ do
    let segwitPayment = Payment Bip84 (FeeRates 10 10) SenderPays 294 [OutputWitnessPKHash] 20000
        legacyPayment amount = Payment Bip44 (FeeRates 10 10) SenderPays 546 [OutputPKHash] amount
        -- 260 coins of 2000, 520000 in all
        smallCoins = [storeCoin ctx (fakeTxHash 1, i) (Just 1) (externalAddr 0) 2000 | i <- [0 .. 259]]
    it "refuses a bip84 coin of 21090 that covers the fee of the parts, 1090, but not of the whole, 1100" $
      spendCoins segwitPayment (segwitCoinsWorth ctx [21090]) `shouldBe` Left "The chosen coins do not cover the amount and the fee"
    it "pays a fee of 1100 with a bip84 coin of 21100, the rate on the whole" $
      (amounts <$> spendCoins segwitPayment (segwitCoinsWorth ctx [21100])) `shouldBe` Right ([21100], 0, 1100)
    it "selects no bip84 coin that covers the parts but not the whole" $
      (amounts <$> selectCoins LargestFirst (mkStdGen 0) segwitPayment (segwitCoinsWorth ctx [21090]))
        `shouldBe` Left "The coins do not cover the amount and the fee"
    it "finds no exact match in a bip84 coin that covers the parts but not the whole" $
      (amounts <$> selectCoins LeastWaste (mkStdGen 0) segwitPayment (segwitCoinsWorth ctx [21090]))
        `shouldBe` Left "The coins do not cover the amount and the fee"
    -- 520000 - 134760 = 385240, 20 short of 385260
    it "refuses 260 legacy coins that cover the fee with 1 byte to count the inputs but not with the 3 it takes" $
      spendCoins (legacyPayment 134760) smallCoins `shouldBe` Left "The chosen coins do not cover the amount and the fee"
    -- 520000 - 134740 = 385260
    it "pays the rate on 260 legacy inputs when the coins cover the 3 bytes that count them" $
      (amounts <$> spendCoins (legacyPayment 134740) smallCoins) `shouldBe` Right (replicate 260 2000, 0, 385260)

-- Of coins of a portfolio, each of either type, a payment to one output
-- with change of either type, at a rate of 1 to 50: whatever the
-- selection, the fee pays the rate on the size of the whole transaction,
-- each input sized by the type its coin was drawn with, with its change
-- output when it has one, and the value of the coins goes to the
-- recipient, the change and the fee, every satoshi of it. The amount is
-- at most half the coins, so that most draws select. A fee short of the
-- rate shows in a few hundred draws, so the property takes 2000.
selectionPropertySpec :: Ctx -> Spec
selectionPropertySpec ctx =
  describe "Any selection" $
    modifyMaxSuccess (const 2000) $
      prop "pays at least the rate on the whole transaction, and splits the value of its coins exactly" $
      forAll draw $ \(changeType, rate, drawn, amount) ->
        let output = accountOutputKind changeType
            payment = Payment changeType (FeeRates rate 10) SenderPays (fromIntegral (outputDustLimit output)) [output] amount
            coins = [storeCoin ctx (fakeTxHash 1, i) (Just 1) (addressOf kind) (fromIntegral v) | (i, (kind, v)) <- zip [0 ..] drawn]
            -- The type each coin was drawn with, by its outpoint
            drawnKinds = Map.fromList [(OutPoint (fakeTxHash 1) i, kind) | (i, (kind, _)) <- zip [0 ..] drawn]
         in case selectCoins LeastWaste (mkStdGen 0) payment coins of
              Left _ -> label "no selection" True
              Right s ->
                let size = txSize [accountInput (drawnKinds Map.! coin.outpoint) | coin <- s.selected] (output : [output | s.changeAmount > 0])
                 in label "selected" $
                      sum ((.value) <$> s.selected) == s.paid + s.changeAmount + s.feeAmount
                        && s.feeAmount >= rate * size
  where
    -- An address of the test wallet of the type: P2PKH or P2WPKH
    addressOf Bip44 = externalAddr 0
    addressOf Bip84 = segwitExternalAddr 0
    draw = do
      changeType <- elements [Bip44, Bip84]
      rate <- choose (1, 50)
      drawn <- listOf1 ((,) <$> elements [Bip44, Bip84] <*> choose (1000, 1000000 :: Word64))
      let total = sum (snd <$> drawn)
      amount <- choose (1000, max 1000 (total `div` 2))
      return (changeType, rate, drawn, fromIntegral amount)
