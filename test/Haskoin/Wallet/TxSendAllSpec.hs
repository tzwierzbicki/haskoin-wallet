{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The parts of preparesendall that read no database: the dust coins, the
-- amounts each recipient gets from the coins, the transaction and the report
-- on the dust coins. The recipients as typed, and how what remains is shared,
-- are in "Haskoin.Wallet.RecipientSpec", the coins left in the account in
-- "Haskoin.Wallet.CoinSpec", and the command as the user runs it in
-- "Haskoin.Wallet.WorkflowSpec".
module Haskoin.Wallet.TxSendAllSpec where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64, Word8)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Recipient (Share (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxBuilding (BuiltTx (..))
import Haskoin.Wallet.TxSendAll
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    dustCoinSpec ctx
    amountSpec ctx
    buildSpec ctx
    reportSpec ctx

-- Every test pays at 10 satoshis per byte with a dust limit of 546. The
-- sizes are those of the builder: 10 bytes of frame, 148 per input and 34
-- per output. The coins are confirmed, at the external addresses of the
-- test wallet.

{- Helpers -}

-- | A confirmed coin of the test wallet: the output of a fake transaction,
-- at an external address
coin :: Ctx -> Word8 -> Word32 -> Natural -> Store.Unspent
coin ctx w i = storeCoin ctx (fakeTxHash w, i) (Just 800000) (externalAddr (fromIntegral i))

-- | The coins of 50000 and 30000 satoshis
twoCoins :: Ctx -> [Store.Unspent]
twoCoins ctx = [coin ctx 1 0 50000, coin ctx 1 1 30000]

-- | The amounts of a send-all of the coins, at 10 satoshis per byte and a
-- dust limit of 546
amounts :: Ctx -> [Store.Unspent] -> [(Address, Share)] -> Either String [(Address, Natural)]
amounts ctx = sendAllAmounts ctx 10 546

-- | The wallet as the builder sees it: the coins, and the paths of the first
-- ten external addresses
found :: [Store.Unspent] -> [(Address, SoftPath)] -> SendAllCoins
found unspents recipients =
  SendAllCoins
    { coins = unspents,
      coinPaths = Map.fromList [(externalAddr i, Deriv :/ 0 :/ fromIntegral i) | i <- [0 .. 9]],
      recipientPaths = Map.fromList recipients
    }

-- | Build with the seed 0, at 10 satoshis per byte and a dust limit of 546
build :: Ctx -> SendAllCoins -> [(Address, Share)] -> Either String BuiltTx
build ctx wallet recipients = buildSendAll btc ctx (mkStdGen 0) wallet recipients 10 546

-- | The outputs of a transaction as addresses and amounts, by amount, since
-- the builder shuffles them
outputsOf :: Ctx -> Tx -> [(Address, Word64)]
outputsOf ctx transaction = sortOn snd [(forceRight (scriptToAddressBS ctx out.script), out.value) | out <- transaction.outputs]

-- | The report on the dust coins of 1200, 900 and 546 satoshis, beside the
-- coins of 50000 and 30000, all sent to one address, and whether other coins
-- stay in the account
threeDustReport :: Ctx -> Bool -> DustReport
threeDustReport ctx othersStaying =
  dustReport ctx 10 546 [(otherAddr 0, Remainder)] (twoCoins ctx) [coin ctx 2 1 900, coin ctx 2 0 1200, coin ctx 2 2 546] othersStaying

-- | The report on one dust coin beside the coins of 50000 and 30000
oneDustReport :: Ctx -> Natural -> DustReport
oneDustReport ctx value = dustReport ctx 10 546 [(otherAddr 0, Remainder)] (twoCoins ctx) [coin ctx 2 0 value] False

{- Dust coins -}

dustCoinSpec :: Ctx -> Spec
dustCoinSpec ctx =
  describe "A dust coin" $ do
    -- The input of 148 bytes costs 1480 at 10 satoshis a byte
    it "is worth no more than the fee of its input" $ do
      isDustCoin 10 (coin ctx 1 0 1480) `shouldBe` True
      isDustCoin 10 (coin ctx 1 0 1481) `shouldBe` False

    -- At 9 satoshis a byte, the input costs 1332
    it "is dust at one fee rate and not at a lower one" $
      isDustCoin 9 (coin ctx 1 0 1480) `shouldBe` False

{- Amounts -}

-- One input of 50000 and one of 30000, 80000 in all. To one address, the
-- transaction is 10 + 2 * 148 + 34 = 340 bytes, a fee of 3400; to two, it
-- is 374 bytes, a fee of 3740. How the rest is shared is in
-- "Haskoin.Wallet.RecipientSpec".
amountSpec :: Ctx -> Spec
amountSpec ctx =
  describe "The amounts of a send-all" $ do
    it "give an address without an amount everything but the fee of the coins" $
      amounts ctx (twoCoins ctx) [(otherAddr 0, Remainder)] `shouldBe` Right [(otherAddr 0, 76600)]

    -- 80000 - 30000 - 3740 = 46260
    it "give a fixed amount as typed, the fee of the coins paid by the address without one" $
      amounts ctx (twoCoins ctx) [(otherAddr 0, Fixed 30000), (otherAddr 1, Remainder)]
        `shouldBe` Right [(otherAddr 0, 30000), (otherAddr 1, 46260)]

{- The transaction -}

buildSpec :: Ctx -> Spec
buildSpec ctx =
  describe "The transaction of a send-all" $ do
    it "spends every coin in full, with no change" $ do
      built <- expectRight $ build ctx (found (twoCoins ctx) []) [(otherAddr 0, Remainder)]
      ((.outpoint) <$> built.tx.inputs) `shouldBe` [OutPoint (fakeTxHash 1) 0, OutPoint (fakeTxHash 1) 1]
      outputsOf ctx built.tx `shouldBe` [(otherAddr 0, 76600)]
      built.inputPaths `shouldBe` [Deriv :/ 0 :/ 0, Deriv :/ 0 :/ 1]
      built.outputPaths `shouldBe` []

    -- 80000 - 30000 - 3740 = 46260
    it "pays the fixed amounts and what remains" $ do
      built <- expectRight $ build ctx (found (twoCoins ctx) []) [(otherAddr 0, Fixed 30000), (otherAddr 1, Remainder)]
      outputsOf ctx built.tx `shouldBe` [(otherAddr 0, 30000), (otherAddr 1, 46260)]

    it "records the path of a recipient of the wallet" $ do
      built <- expectRight $ build ctx (found (twoCoins ctx) [(externalAddr 3, Deriv :/ 0 :/ 3)]) [(externalAddr 3, Remainder)]
      built.outputPaths `shouldBe` [Deriv :/ 0 :/ 3]

    it "is refused without coins to send" $
      build ctx (found [] []) [(otherAddr 0, Remainder)] `shouldBe` Left "There are no coins to send"

    it "is refused a fee rate of zero" $
      buildSendAll btc ctx (mkStdGen 0) (found (twoCoins ctx) []) [(otherAddr 0, Remainder)] 0 546
        `shouldBe` Left "The fee rate must be at least 1 satoshi per byte"

{- The report on dust coins -}

-- Beside the coins of 50000 and 30000, the dust coins of 1200, 900 and 546
-- add 3 inputs of 1480 each, 4440 of fee for 2646 of coins. To one address,
-- the 2 coins leave 80000 - 3400 = 76600; the 5 coins, 784 bytes, leave
-- 82646 - 7840 = 74806. Alone, the 3 dust coins to one address are 488
-- bytes: they leave at least 546 at up to (2646 - 546) / 488 = 4 satoshis a
-- byte. A coin is worth spending while its input of 148 bytes costs less
-- than it: the coin of 1200 up to 8 satoshis a byte, 900 up to 6 and 546 up
-- to 3.
reportSpec :: Ctx -> Spec
reportSpec ctx =
  describe "The report on the dust coins" $ do
    it "tells what spending the dust coins adds to the fee and takes from the recipients" $ do
      let report = threeDustReport ctx False
      (report.addedFee, report.withDust, report.withoutDust) `shouldBe` (4440, 74806, 76600)

    it "tells the highest fee rate at which the dust coins alone can be sent later" $
      (threeDustReport ctx False).laterRate `shouldBe` Just 4

    it "lists the dust coins, the largest first, each with the rates it is worth spending at" $
      dustReportLines (threeDustReport ctx False)
        `shouldBe` [ "3 coins cost more to spend than they are worth at 10 sat/byte, 1480 sat each:",
                     "  0000000000000000000000000000000000000000000000000000000000000002:0  1200 sat, worth spending at 8 sat/byte or less",
                     "  0000000000000000000000000000000000000000000000000000000000000002:1  900 sat, worth spending at 6 sat/byte or less",
                     "  0000000000000000000000000000000000000000000000000000000000000002:2  546 sat, worth spending at 3 sat/byte or less",
                     "Spending them adds 4440 sat of fee for 2646 sat of coins. What remains for the addresses without an amount is 74806 sat instead of 76600, 1794 less, and the account is empty.",
                     "Leaving them, the account keeps 2646 sat. Once the other coins are gone, they can only be spent together, at 4 sat/byte or less."
                   ]

    it "does not call the account empty when other coins stay in it" $
      (dustReportLines (threeDustReport ctx True) !! 4)
        `shouldBe` "Spending them adds 4440 sat of fee for 2646 sat of coins. What remains for the addresses without an amount is 74806 sat instead of 76600, 1794 less."

    it "asks whether to spend them too" $
      dustQuestion (threeDustReport ctx False) `shouldBe` "Spend them too?"

    -- Alone, the coin of 1200 to one address is 192 bytes: it leaves at
    -- least 546 at up to (1200 - 546) / 192 = 3 satoshis a byte
    it "tells of one dust coin in the singular" $ do
      let report = oneDustReport ctx 1200
      (dustReportLines report !! 0) `shouldBe` "1 coin costs more to spend than it is worth at 10 sat/byte, 1480 sat:"
      (dustReportLines report !! 3) `shouldBe` "Leaving it, the account keeps 1200 sat. Once the other coins are gone, it can only be spent at 3 sat/byte or less."
      dustQuestion report `shouldBe` "Spend it too?"

    -- At 1 satoshi a byte, the 192 bytes leave 600 - 192 = 408
    it "tells when the dust coins can never be spent alone" $
      (dustReportLines (oneDustReport ctx 600) !! 3)
        `shouldBe` "Leaving it, the account keeps 600 sat. Once the other coins are gone, it can never be spent: any fee would leave less than 546 sat."

    -- Its input costs 148 at 1 satoshi a byte
    it "tells of a coin worth less than its fee at any rate" $
      (dustReportLines (oneDustReport ctx 148) !! 1)
        `shouldBe` "  0000000000000000000000000000000000000000000000000000000000000002:0  148 sat, worth less than its fee at any rate"
