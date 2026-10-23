{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The transaction builder: what it spends and pays, who pays the fee, coin
-- control, the strategies, the draws and shuffles decided by its seed, and
-- every error it reports. The coin selection itself is covered by
-- "Haskoin.Wallet.CoinSelectionSpec".
module Haskoin.Wallet.TxBuildingSpec where

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Default (def)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Address
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Coin
import Haskoin.Wallet.CoinSelection
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.RawTx
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxBuilding
import Haskoin.Wallet.TxInfo (TxInfo (..))
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (parseTxSignData, signWalletTx, verifySignedTx)
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    transactionSpec ctx
    recipientTypesSpec ctx
    dustSpec ctx
    feePayerSpec ctx
    coinControlSpec ctx
    strategySpec ctx
    shuffleSpec ctx
    errorSpec ctx
    databaseSpec ctx

-- Every test pays from coins of the test wallet, funded by one synthetic
-- transaction, at 10 satoshis per byte with a dust limit of 546. The sizes
-- are those of the coin selection: 10 bytes of frame, 148 per input and 34
-- per output. The usual transaction, one input and two outputs, is 226 bytes
-- and costs 2260; without a change output it is 192 bytes and costs 1920.

{- Helpers -}

-- | The outpoint of the i-th coin of 'fundingWorth'
coinAt :: Ctx -> [Natural] -> Word32 -> OutPoint
coinAt ctx values = OutPoint (txHash $ fundingWorth ctx values)

-- | The wallet as the builder sees it: the coins, the paths of the first ten
-- external addresses, the change address /1/0, and the recipients that
-- belong to the wallet with their paths
wallet :: [Store.Unspent] -> [(Address, SoftPath)] -> WalletCoins
wallet coins recipients =
  WalletCoins
    { coins = coins,
      coinPaths = Map.fromList [(externalAddr i, Deriv :/ 0 :/ fromIntegral i) | i <- [0 .. 9]],
      change = (internalAddr 0, Deriv :/ 1 :/ 0),
      recipientPaths = Map.fromList recipients
    }

-- | Build with the seed 0 and the given strategy, wallet, recipients and
-- payer, at 10 satoshis per byte with a dust limit of 546
build :: Ctx -> CoinSelection -> WalletCoins -> [(Address, Natural)] -> FeePayer -> Either String BuiltTx
build ctx strategy found recipients payer =
  buildWalletTx btc ctx (mkStdGen 0) strategy found recipients 10 546 payer []

-- | Pay recipients of other wallets from coins worth the given amounts, the
-- largest coins first, the sender paying the fee
pay :: Ctx -> [Natural] -> [(Address, Natural)] -> Either String BuiltTx
pay ctx values recipients =
  build ctx LargestFirst (wallet (coinsWorth ctx values) []) recipients SenderPays

-- | Like 'pay', the recipients paying the fee
recipientsPayFor :: Ctx -> [Natural] -> [(Address, Natural)] -> Either String BuiltTx
recipientsPayFor ctx values recipients =
  build ctx LargestFirst (wallet (coinsWorth ctx values) []) recipients RecipientsPay

-- | Like 'pay', spending exactly the coins at the given indices
payFrom :: Ctx -> [Natural] -> [Word32] -> [(Address, Natural)] -> Either String BuiltTx
payFrom ctx values indices recipients =
  buildWalletTx btc ctx (mkStdGen 0) LargestFirst (wallet (coinsWorth ctx values) []) recipients 10 546 SenderPays (coinAt ctx values <$> indices)

-- | Like 'pay', with the given seed and strategy
payWith :: Ctx -> Int -> CoinSelection -> [Natural] -> [(Address, Natural)] -> Either String BuiltTx
payWith ctx seed strategy values recipients =
  buildWalletTx btc ctx (mkStdGen seed) strategy (wallet (coinsWorth ctx values) []) recipients 10 546 SenderPays []

-- | What a built transaction spends and pays: the amounts of its inputs, in
-- order, and its outputs by amount, since the builder shuffles them
spends :: Ctx -> BuiltTx -> ([Word64], [(Address, Word64)])
spends ctx built = ((.value) <$> built.spentCoins, sortOn snd (outputsOf ctx built.tx))

-- | The outputs of a transaction as addresses and amounts, in order
outputsOf :: Ctx -> Tx -> [(Address, Word64)]
outputsOf ctx tx = [(forceRight (scriptToAddressBS ctx out.script), out.value) | out <- tx.outputs]

-- | The fee a built transaction pays: what its inputs hold beyond its outputs
feePaid :: BuiltTx -> Word64
feePaid built = sum ((.value) <$> built.spentCoins) - sum ((.value) <$> built.tx.outputs)

{- The transactions built -}

transactionSpec :: Ctx -> Spec
transactionSpec ctx =
  describe "Transactions built" $ do
    it "pays a recipient and the change" $ do
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 10 = 2260
      -- Change: 400000 - 350000 - 2260 = 47740
      built <- expectRight $ pay ctx [400000] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47740), (otherAddr 0, 350000)])
      feePaid built `shouldBe` 2260
      built.inputPaths `shouldBe` [Deriv :/ 0 :/ 0]
      built.outputPaths `shouldBe` [Deriv :/ 1 :/ 0]
    it "is a version 1 transaction with final sequences and no lock time" $ do
      built <- expectRight $ pay ctx [400000] [(otherAddr 0, 350000)]
      sortedOutputs built.tx
        `shouldBe` spendingTx ctx [(txHash (fundingWorth ctx [400000]), 0)] [(internalAddr 0, 47740), (otherAddr 0, 350000)]
    it "pays several recipients" $ do
      -- Fee: 10 + 148 + 3 * 34 = 260 bytes * 10 = 2600
      -- Change: 400000 - 150000 - 100000 - 2600 = 147400
      built <- expectRight $ pay ctx [400000] [(otherAddr 0, 150000), (otherAddr 1, 100000)]
      spends ctx built `shouldBe` ([400000], [(otherAddr 1, 100000), (internalAddr 0, 147400), (otherAddr 0, 150000)])
      feePaid built `shouldBe` 2600
    it "spends several coins, in the order of its inputs" $ do
      -- Fee: 10 + 2 * 148 + 2 * 34 = 374 bytes * 10 = 3740
      -- Change: 300000 + 200000 - 350000 - 3740 = 146260
      let three = [100000, 200000, 300000]
      built <- expectRight $ pay ctx three [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([300000, 200000], [(internalAddr 0, 146260), (otherAddr 0, 350000)])
      ((.outpoint) <$> built.tx.inputs) `shouldBe` [coinAt ctx three 2, coinAt ctx three 1]
      ((.outpoint) <$> built.spentCoins) `shouldBe` [coinAt ctx three 2, coinAt ctx three 1]
      built.inputPaths `shouldBe` [Deriv :/ 0 :/ 2, Deriv :/ 0 :/ 1]
    it "has no change output when the coins match the amount and the fee" $ do
      -- Fee: 10 + 148 + 34 = 192 bytes * 10 = 1920
      -- Change: 351920 - 350000 - 1920 = 0
      built <- expectRight $ pay ctx [351920] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([351920], [(otherAddr 0, 350000)])
      feePaid built `shouldBe` 1920
      built.outputPaths `shouldBe` []
    it "records the path of a recipient of the wallet once" $ do
      -- Two outputs to /0/3: 10 + 148 + 3 * 34 = 260 bytes * 10 = 2600
      -- Change: 400000 - 100000 - 50000 - 2600 = 247400
      let found = wallet (coinsWorth ctx [400000]) [(externalAddr 3, Deriv :/ 0 :/ 3)]
      built <- expectRight $ build ctx LargestFirst found [(externalAddr 3, 100000), (externalAddr 3, 50000)] SenderPays
      spends ctx built `shouldBe` ([400000], [(externalAddr 3, 50000), (externalAddr 3, 100000), (internalAddr 0, 247400)])
      built.outputPaths `shouldBe` [Deriv :/ 1 :/ 0, Deriv :/ 0 :/ 3]
    it "records the path of an address once for several of its coins" $ do
      -- Two coins on /0/0: 10 + 2 * 148 + 2 * 34 = 374 bytes * 10 = 3740
      -- Change: 200000 + 200000 - 350000 - 3740 = 46260
      let found = wallet (coinsOf ctx $ fundingTx ctx 1 [(externalAddr 0, 200000), (externalAddr 0, 200000)]) []
      built <- expectRight $ build ctx LargestFirst found [(otherAddr 0, 350000)] SenderPays
      spends ctx built `shouldBe` ([200000, 200000], [(internalAddr 0, 46260), (otherAddr 0, 350000)])
      built.inputPaths `shouldBe` [Deriv :/ 0 :/ 0]

{- The dust limit -}

-- An output is dust below the limit of 546, like in Bitcoin Core: an output
-- of exactly 546 is fine. A dust change goes to the fee; a dust recipient
-- output is an error.

{- Recipients of every standard address type -}

-- The wallet pays its own P2PKH change, but a recipient may be of any
-- standard kind, and each output is charged its own size: 34 bytes for
-- P2PKH, 32 for P2SH, 31 for P2WPKH, 43 for P2WSH and 43 for Taproot. The
-- addresses are the P2SH address of the swept key of
-- "Haskoin.Wallet.TxSweepSpec" and the examples of BIP173 and BIP350.
recipientTypesSpec :: Ctx -> Spec
recipientTypesSpec ctx =
  describe "Recipients of every standard address type" $ do
    it "pays a P2SH recipient: 10 + 148 + 32 + 34 = 224 bytes" $ do
      built <- expectRight $ pay ctx [400000] [(p2shAddr, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47760), (p2shAddr, 350000)])
      feePaid built `shouldBe` 2240
    it "pays a P2WPKH recipient: 10 + 148 + 31 + 34 = 223 bytes" $ do
      built <- expectRight $ pay ctx [400000] [(p2wpkhAddr, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47770), (p2wpkhAddr, 350000)])
      feePaid built `shouldBe` 2230
    it "pays a P2WSH recipient: 10 + 148 + 43 + 34 = 235 bytes" $ do
      built <- expectRight $ pay ctx [400000] [(p2wshAddr, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47650), (p2wshAddr, 350000)])
      feePaid built `shouldBe` 2350
    it "pays a Taproot recipient: 10 + 148 + 43 + 34 = 235 bytes" $ do
      built <- expectRight $ pay ctx [400000] [(taprootAddr, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47650), (taprootAddr, 350000)])
      feePaid built `shouldBe` 2350
    it "pays one recipient of each type in one transaction" $ do
      -- 10 + 148 + 34 + 32 + 31 + 43 + 43 + 34 = 375 bytes, a fee of 3750,
      -- and a change of 400000 - 350000 - 3750 = 46250
      built <- expectRight $ pay ctx [400000] everyType
      spends ctx built
        `shouldBe` ( [400000],
                     [ (internalAddr 0, 46250),
                       (otherAddr 0, 50000),
                       (p2shAddr, 60000),
                       (p2wpkhAddr, 70000),
                       (p2wshAddr, 80000),
                       (taprootAddr, 90000)
                     ]
                   )
      feePaid built `shouldBe` 3750
      built.outputPaths `shouldBe` [Deriv :/ 1 :/ 0]
    it "reports, signs and verifies the payment to every type" $ do
      built <- expectRight $ pay ctx [400000] everyType
      let tsd = TxSignData built.tx [fundingWorth ctx [400000]] built.inputPaths built.outputPaths (Just 10)
      unsignedInfo <- expectRight $ parseTxSignData btc ctx (testAccountPub ctx) tsd
      (unsignedInfo.txInfoSize, unsignedInfo.txInfoFeeByte, unsignedInfo.txInfoChangeInFee) `shouldBe` (375, 10, Just 0)
      (signed, info) <- expectRight $ signWalletTx btc ctx tsd testAccountKey
      verifySignedTx btc ctx signed `shouldBe` Right ()
      -- The signature of this transaction is 72 bytes, as estimated
      (info.txInfoFee, info.txInfoSize, info.txInfoFeeByte) `shouldBe` (3750, 375, 10)
      Map.toList info.txInfoOtherOutputs
        `shouldBe` [(otherAddr 0, 50000), (p2shAddr, 60000), (p2wpkhAddr, 70000), (p2wshAddr, 80000), (taprootAddr, 90000)]
  where
    everyType = [(otherAddr 0, 50000), (p2shAddr, 60000), (p2wpkhAddr, 70000), (p2wshAddr, 80000), (taprootAddr, 90000)]
    p2shAddr = fromJust $ textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p"
    p2wpkhAddr = fromJust $ textToAddr btc "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
    p2wshAddr = fromJust $ textToAddr btc "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
    taprootAddr = fromJust $ textToAddr btc "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"

dustSpec :: Ctx -> Spec
dustSpec ctx =
  describe "Dust limit" $ do
    it "pays a change above the dust limit" $ do
      -- Change: 352807 - 350000 - 2260 = 547
      built <- expectRight $ pay ctx [352807] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([352807], [(internalAddr 0, 547), (otherAddr 0, 350000)])
      feePaid built `shouldBe` 2260
    it "pays a change exactly at the dust limit" $ do
      -- Change: 352806 - 350000 - 2260 = 546
      built <- expectRight $ pay ctx [352806] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([352806], [(internalAddr 0, 546), (otherAddr 0, 350000)])
      feePaid built `shouldBe` 2260
    it "gives a change below the dust limit to the fee" $ do
      -- Change: 352805 - 350000 - 2260 = 545: no change output, and the fee
      -- is 352805 - 350000 = 2805, the 1920 of the transaction and the rest
      built <- expectRight $ pay ctx [352805] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([352805], [(otherAddr 0, 350000)])
      feePaid built `shouldBe` 2805
    it "pays a recipient above the dust limit" $ do
      -- Change: 400000 - 547 - 2260 = 397193
      built <- expectRight $ pay ctx [400000] [(otherAddr 0, 547)]
      spends ctx built `shouldBe` ([400000], [(otherAddr 0, 547), (internalAddr 0, 397193)])
    it "pays a recipient exactly the dust limit" $ do
      -- Change: 400000 - 546 - 2260 = 397194
      built <- expectRight $ pay ctx [400000] [(otherAddr 0, 546)]
      spends ctx built `shouldBe` ([400000], [(otherAddr 0, 546), (internalAddr 0, 397194)])
    it "fails when a recipient output is below the dust limit" $
      pay ctx [400000] [(otherAddr 0, 545)] `shouldBe` Left "Recipient output is smaller than the dust value"
    it "pays a recipient exactly the dust limit after its fee" $ do
      -- Fee: 226 bytes * 10 = 2260, paid by the recipient: 2806 - 2260 = 546
      -- Change: 400000 - 2806 = 397194
      built <- expectRight $ recipientsPayFor ctx [400000] [(otherAddr 0, 2806)]
      spends ctx built `shouldBe` ([400000], [(otherAddr 0, 546), (internalAddr 0, 397194)])
      feePaid built `shouldBe` 2260
    it "fails when a recipient is left just below the dust limit after its fee" $
      -- 2805 - 2260 = 545
      recipientsPayFor ctx [400000] [(otherAddr 0, 2805)] `shouldBe` Left "Recipient output is smaller than the dust value"

{- Who pays the fee -}

feePayerSpec :: Ctx -> Spec
feePayerSpec ctx =
  describe "Who pays the fee" $ do
    it "deducts the fee from the recipient when the recipient pays" $ do
      -- The whole coin is sent: 10 + 148 + 34 = 192 bytes * 10 = 1920 of fee
      -- Paid: 350000 - 1920 = 348080
      built <- expectRight $ recipientsPayFor ctx [350000] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([350000], [(otherAddr 0, 348080)])
      feePaid built `shouldBe` 1920
    it "leaves the change to the wallet when the recipient pays" $ do
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 10 = 2260, paid: 350000 - 2260 = 347740
      -- Change: 400000 - 350000 = 50000
      built <- expectRight $ recipientsPayFor ctx [400000] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 50000), (otherAddr 0, 347740)])
      feePaid built `shouldBe` 2260
    it "splits the fee between the recipients in equal parts" $ do
      -- Fee: 10 + 148 + 2 * 34 = 226 bytes * 10 = 2260, 1130 each
      -- Paid: 200000 - 1130 = 198870, 150000 - 1130 = 148870
      built <- expectRight $ recipientsPayFor ctx [350000] [(otherAddr 0, 200000), (otherAddr 1, 150000)]
      spends ctx built `shouldBe` ([350000], [(otherAddr 1, 148870), (otherAddr 0, 198870)])
      feePaid built `shouldBe` 2260
    it "makes the first recipient pay the remainder of the split" $ do
      -- Fee: 10 + 148 + 3 * 34 = 260 bytes * 10 = 2600 = 3 * 866 + 2
      -- Paid: 150000 - 866 - 2 = 149132, 120000 - 866 = 119134, 80000 - 866 = 79134
      built <- expectRight $ recipientsPayFor ctx [350000] [(otherAddr 0, 150000), (otherAddr 1, 120000), (otherAddr 2, 80000)]
      spends ctx built `shouldBe` ([350000], [(otherAddr 2, 79134), (otherAddr 1, 119134), (otherAddr 0, 149132)])
      feePaid built `shouldBe` 2600
    it "adds a dust change to the fee the recipient pays" $ do
      -- Change: 350300 - 350000 = 300, not above the dust of 546: no change output
      -- Fee: 1920 paid by the recipient (350000 - 1920 = 348080) and the 300 left over
      built <- expectRight $ recipientsPayFor ctx [350300] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([350300], [(otherAddr 0, 348080)])
      feePaid built `shouldBe` 2220

{- Coin control -}

coinControlSpec :: Ctx -> Spec
coinControlSpec ctx =
  describe "Coin control" $ do
    let four = [100000, 200000, 300000, 400000]
    it "spends exactly the coins listed" $ do
      -- The largest coin would cover 250000 alone; the two listed are spent
      -- Fee: 10 + 2 * 148 + 2 * 34 = 374 bytes * 10 = 3740
      -- Change: 100000 + 200000 - 250000 - 3740 = 46260
      built <- expectRight $ payFrom ctx four [0, 1] [(otherAddr 0, 250000)]
      spends ctx built `shouldBe` ([100000, 200000], [(internalAddr 0, 46260), (otherAddr 0, 250000)])
      feePaid built `shouldBe` 3740
    it "spends a listed coin worth less than the fee of its input" $ do
      -- 1000 - 1480 < 0, spent all the same
      -- Change: 1000 + 400000 - 350000 - 3740 = 47260
      built <- expectRight $ payFrom ctx [1000, 400000] [0, 1] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([1000, 400000], [(internalAddr 0, 47260), (otherAddr 0, 350000)])
    it "spends a coin listed twice once" $ do
      -- Fee: 226 bytes * 10 = 2260; change: 100000 - 50000 - 2260 = 47740
      built <- expectRight $ payFrom ctx [100000, 200000] [0, 0] [(otherAddr 0, 50000)]
      spends ctx built `shouldBe` ([100000], [(internalAddr 0, 47740), (otherAddr 0, 50000)])
    it "fails when a listed coin does not exist" $
      payFrom ctx four [5] [(otherAddr 0, 50000)] `shouldBe` Left "Selected coin does not exist or is not spendable"
    it "fails when the listed coins do not cover the amount and the fee" $
      -- 100000 - 1480 < 350000 + 440
      payFrom ctx four [0] [(otherAddr 0, 350000)] `shouldBe` Left "The selected coins do not cover the amount and the fee"

{- Strategies -}

strategySpec :: Ctx -> Spec
strategySpec ctx =
  describe "Strategies" $ do
    let four = [100000, 200000, 300000, 400000]
    it "spends the largest coins first" $ do
      -- Fee: 226 bytes * 10 = 2260; change: 400000 - 350000 - 2260 = 47740
      built <- expectRight $ payWith ctx 0 LargestFirst four [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47740), (otherAddr 0, 350000)])
    it "spends the smallest coins first" $ do
      -- Fee: 10 + 3 * 148 + 2 * 34 = 522 bytes * 10 = 5220
      -- Change: 100000 + 200000 + 300000 - 350000 - 5220 = 244780
      built <- expectRight $ payWith ctx 0 SmallestFirst four [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([100000, 200000, 300000], [(internalAddr 0, 244780), (otherAddr 0, 350000)])
    it "wastes the least: an exact match without change" $ do
      -- 300000 + 53400 - 2 * 1480 = 350440, the amount and the fee exactly
      -- Fee: 10 + 2 * 148 + 34 = 340 bytes * 10 = 3400
      built <- expectRight $ payWith ctx 0 LeastWaste [400000, 300000, 53400] [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([300000, 53400], [(otherAddr 0, 350000)])
      feePaid built `shouldBe` 3400
    -- Three coins drawn at random: the seed decides their order and the draw
    -- stops as soon as the coins cover the amount and the fee. The builder
    -- splits its generator, so the seeds draw otherwise than in the coin
    -- selection spec.
    let three = [300000, 100000, 400000]
    it "draws coins at random, in the order of the seed" $ do
      -- Change: 400000 - 350000 - 2260 = 47740
      built <- expectRight $ payWith ctx 0 RandomDraw three [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47740), (otherAddr 0, 350000)])
    it "draws other coins with another seed" $ do
      -- Fee: 374 bytes * 10 = 3740; change: 100000 + 300000 - 350000 - 3740 = 46260
      built <- expectRight $ payWith ctx 1 RandomDraw three [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([100000, 300000], [(internalAddr 0, 46260), (otherAddr 0, 350000)])
    it "draws other coins again with a third seed" $ do
      -- Change: 100000 + 400000 - 350000 - 3740 = 146260
      built <- expectRight $ payWith ctx 2 RandomDraw three [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([100000, 400000], [(internalAddr 0, 146260), (otherAddr 0, 350000)])

{- The shuffle of the outputs -}

shuffleSpec :: Ctx -> Spec
shuffleSpec ctx =
  describe "Shuffle of the outputs" $ do
    -- One coin pays two recipients and the change of 400000 - 250000 - 2600 =
    -- 147400: the seed decides the order of the three outputs
    let outputsWithSeed :: Int -> Either String [(Address, Word64)]
        outputsWithSeed seed =
          outputsOf ctx . (.tx) <$> payWith ctx seed LargestFirst [400000] [(otherAddr 0, 150000), (otherAddr 1, 100000)]
    it "orders the outputs by the seed" $
      outputsWithSeed 0 `shouldBe` Right [(internalAddr 0, 147400), (otherAddr 1, 100000), (otherAddr 0, 150000)]
    it "orders them otherwise with another seed" $
      outputsWithSeed 1 `shouldBe` Right [(otherAddr 0, 150000), (otherAddr 1, 100000), (internalAddr 0, 147400)]
    it "orders them otherwise again with a third seed" $
      outputsWithSeed 2 `shouldBe` Right [(internalAddr 0, 147400), (otherAddr 0, 150000), (otherAddr 1, 100000)]

{- Errors -}

errorSpec :: Ctx -> Spec
errorSpec ctx =
  describe "Errors" $ do
    it "fails without recipients" $
      pay ctx [400000] [] `shouldBe` Left "No recipients provided"
    it "fails at a fee rate of zero" $
      buildWalletTx btc ctx (mkStdGen 0) LargestFirst (wallet (coinsWorth ctx [400000]) []) [(otherAddr 0, 350000)] 0 546 SenderPays []
        `shouldBe` Left "The fee rate must be at least 1 satoshi per byte"
    it "fails when the coins do not cover the amount and the fee" $
      -- 100000 - 1480 < 350000 + 440
      pay ctx [100000] [(otherAddr 0, 350000)] `shouldBe` Left "The coins do not cover the amount and the fee"
    it "fails when a recipient cannot pay its share of the fee" $
      -- Fee: 226 bytes * 10 = 2260, 1130 each: more than the 1000 sent
      recipientsPayFor ctx [350000] [(otherAddr 0, 349000), (otherAddr 1, 1000)] `shouldBe` Left "Recipients can't pay for the fee"
    it "fails when the fee leaves a recipient with dust" $
      -- The whole coin is sent: 2000 - 1920 = 80 left to the recipient
      recipientsPayFor ctx [2000] [(otherAddr 0, 2000)] `shouldBe` Left "Recipient output is smaller than the dust value"
    it "fails when the fee exceeds what a recipient is sent" $
      -- Fee: 226 bytes * 10 = 2260, more than the 1000 sent: nothing is left
      recipientsPayFor ctx [400000] [(otherAddr 0, 1000)] `shouldBe` Left "Recipients can't pay for the fee"
    it "fails when a chosen coin has no derivation path" $ do
      -- A coin held by an address that is not in the wallet
      let found = wallet (coinsOf ctx $ fundingTx ctx 1 [(otherAddr 5, 400000)]) []
      build ctx LargestFirst found [(otherAddr 0, 350000)] SenderPays `shouldBe` Left "A chosen coin has no derivation path"
    it "fails when a chosen coin has no address" $ do
      let coin =
            Store.Unspent
              { Store.block = Store.MemRef 0,
                Store.outpoint = coinAt ctx [400000] 0,
                Store.value = 400000,
                Store.script = addressScript ctx (externalAddr 0),
                Store.address = Nothing
              }
      build ctx LargestFirst (wallet [coin] []) [(otherAddr 0, 350000)] SenderPays `shouldBe` Left "A chosen coin has no address"

{- With the database -}

-- | An account of the test wallet with its first external address, holding
-- a coin of 400000 confirmed at height 0, the best block
fundedAccount :: Ctx -> Config -> ExceptT String (DB IO) (DBAccountId, Tx)
fundedAccount ctx cfg = do
  (accId, _) <- testNewAcc ctx "test"
  lift $ updateBest btc (fakeBlockHash 0) 0
  _ <- genExtAddress ctx cfg accId ""
  let fund = fundingWorth ctx [400000]
  lift $ insertRawTx fund
  _ <- refreshCoins btc accId [externalAddr 0] [storeCoin ctx (txHash fund, 0) (Just 0) (externalAddr 0) 400000]
  return (accId, fund)

databaseSpec :: Ctx -> Spec
databaseSpec ctx =
  describe "With the database" $ do
    let cfg = def :: Config
    it "fetches the coins with their paths, the change address and the wallet recipients" $
      inDatabaseE $ do
        (accId, fund) <- fundedAccount ctx cfg
        found <- fetchWalletCoins btc ctx cfg accId [otherAddr 0, externalAddr 0] 1
        liftIO $ do
          found.coins `shouldBe` [storeCoin ctx (txHash fund, 0) (Just 0) (externalAddr 0) 400000]
          found.coinPaths `shouldBe` Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0)]
          found.change `shouldBe` (internalAddr 0, Deriv :/ 1 :/ 0)
          found.recipientPaths `shouldBe` Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0)]
    it "leaves out the coins without enough confirmations" $
      inDatabaseE $ do
        (accId, _) <- fundedAccount ctx cfg
        -- The coin at the best block has one confirmation
        found <- fetchWalletCoins btc ctx cfg accId [] 2
        liftIO $ found.coins `shouldBe` []
    it "builds the signing data with the transactions spent from" $
      inDatabaseE $ do
        (accId, fund) <- fundedAccount ctx cfg
        tsd <- buildTxSignData btc ctx cfg (mkStdGen 0) LargestFirst accId [(otherAddr 0, 350000)] 10 546 SenderPays 1 []
        liftIO $ do
          sortedOutputs (txSignDataTx tsd)
            `shouldBe` spendingTx ctx [(txHash fund, 0)] [(internalAddr 0, 47740), (otherAddr 0, 350000)]
          txSignDataInputs tsd `shouldBe` [fund]
          txSignDataInputPaths tsd `shouldBe` [Deriv :/ 0 :/ 0]
          txSignDataOutputPaths tsd `shouldBe` [Deriv :/ 1 :/ 0]
          txSignDataFeeRate tsd `shouldBe` Just 10
          isSigned (txSignDataTx tsd) `shouldBe` False
    it "fails without recipients" $
      inDatabaseE $ do
        (accId, _) <- fundedAccount ctx cfg
        result <- lift $ runExceptT $ buildTxSignData btc ctx cfg (mkStdGen 0) LargestFirst accId [] 10 546 SenderPays 1 []
        liftIO $ result `shouldBe` Left "No recipients provided"
