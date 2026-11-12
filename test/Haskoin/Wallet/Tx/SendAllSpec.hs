{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The parts of preparesendall that read no database: the dust coins, the
-- amounts each recipient gets from the coins, the transaction and the report
-- on the dust coins; then the signing data of a send-all of a portfolio in
-- the database. The recipients as typed, and how what remains is shared,
-- are in "Haskoin.Wallet.Tx.RecipientSpec", the coins left in the portfolio
-- in "Haskoin.Wallet.Database.CoinSpec", and the command as the user runs it
-- in "Haskoin.Wallet.Workflow.SendAllSpec".
module Haskoin.Wallet.Tx.SendAllSpec where

import Control.Monad.Trans (liftIO)
import Data.Default (def)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64, Word8)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Tx.Recipient (Share (..))
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.PrettyPrinter (dustReportDoc)
import Haskoin.Wallet.Database.Coin (JsonCoin, spendableCoins)
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath, legacyPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.Building (BuiltTx (..), WalletCoins (..), fetchWalletCoins)
import Haskoin.Wallet.Tx.BuildingSpec (fundedBoth)
import Haskoin.Wallet.Tx.SendAll
import Haskoin.Wallet.Tx.SignData
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
    signDataSpec ctx

-- Every test pays at 10 satoshis per virtual byte. The sizes are those of
-- the builder: 10 bytes of frame, 148 per P2PKH input and 34 per P2PKH
-- output. The coins are confirmed, at the external addresses of the test
-- wallet: those of its bip44 account, P2PKH coins, unless a test says
-- bip84. A P2WPKH coin, of the bip84 account, has an input of 41 bytes and
-- a witness of 108, which counts a quarter with the 2 bytes that mark a
-- transaction with witnesses and the 1 byte each P2PKH input has in their
-- place: 68 virtual bytes for the input on its own.

{- Helpers -}

-- | A confirmed coin of the test wallet: the output of a fake transaction,
-- at an external address
coin :: Ctx -> Word8 -> Word32 -> Natural -> Store.Unspent
coin ctx w i = storeCoin ctx (fakeTxHash w, i) (Just 800000) (externalAddr (fromIntegral i))

-- | The same at a receiving address of the bip84 account of the test
-- wallet: a P2WPKH coin
segwitCoin :: Ctx -> Word8 -> Word32 -> Natural -> Store.Unspent
segwitCoin ctx w i = storeCoin ctx (fakeTxHash w, i) (Just 800000) (segwitExternalAddr (fromIntegral i))

-- | The coins of 50000 and 30000 satoshis
twoCoins :: Ctx -> [Store.Unspent]
twoCoins ctx = [coin ctx 1 0 50000, coin ctx 1 1 30000]

-- | The coins of a portfolio that has both types: 50000 at the first
-- receiving address of its bip44 account, and 30000 at that of its bip84
-- account
bothTypes :: Ctx -> [Store.Unspent]
bothTypes ctx = [coin ctx 1 0 50000, segwitCoin ctx 3 0 30000]

-- | The amounts of a send-all of the coins, at 10 satoshis per virtual byte
amounts :: Ctx -> [Store.Unspent] -> [(Address, Share)] -> Either String [(Address, Natural)]
amounts ctx = sendAllAmounts ctx 10

-- | The portfolio as the builder sees it: the coins, and the paths of the
-- first ten receiving addresses of its bip44 and of its bip84 account
found :: [Store.Unspent] -> [(Address, PoolPath)] -> SendAllCoins
found unspents recipients =
  SendAllCoins
    { coins = unspents,
      coinPaths =
        Map.fromList $
          [(externalAddr i, legacyPath (Deriv :/ 0 :/ fromIntegral i)) | i <- [0 .. 9]]
            <> [(segwitExternalAddr i, segwitPath (Deriv :/ 0 :/ fromIntegral i)) | i <- [0 .. 9]],
      recipientPaths = Map.fromList recipients
    }

-- | Build with the seed 0, at 10 satoshis per virtual byte
build :: Ctx -> SendAllCoins -> [(Address, Share)] -> Either String BuiltTx
build ctx wallet recipients = buildSendAll btc ctx (mkStdGen 0) wallet recipients 10 800002

-- | The outputs of a transaction as addresses and amounts, by amount, since
-- the builder shuffles them
outputsOf :: Ctx -> Tx -> [(Address, Word64)]
outputsOf ctx transaction = sortOn snd [(forceRight (scriptToAddressBS btc ctx out.script), out.value) | out <- transaction.outputs]

-- | The report on the dust coins of 1200, 900 and 546 satoshis, beside the
-- coins of 50000 and 30000, all sent to one address, and whether other coins
-- stay in the portfolio
threeDustReport :: Ctx -> Bool -> DustReport
threeDustReport ctx othersStaying =
  dustReport ctx 10 [(otherAddr 0, Remainder)] (twoCoins ctx) [coin ctx 2 1 900, coin ctx 2 0 1200, coin ctx 2 2 546] othersStaying

-- | The report on one dust coin beside the coins of 50000 and 30000
oneDustReport :: Ctx -> Natural -> DustReport
oneDustReport ctx value = dustReport ctx 10 [(otherAddr 0, Remainder)] (twoCoins ctx) [coin ctx 2 0 value] False

-- | The three dust coins of 'threeDustReport' as coins shows them: each
-- at the receiving address of the bip44 account with its index
threeDustViews :: Ctx -> [JsonCoin]
threeDustViews ctx = [coinViewAt (legacyPath (Deriv :/ 0 :/ i)) (coin ctx 2 i v) | (i, v) <- [(0, 1200), (1, 900), (2, 546)]]

-- | The dust coin of 'oneDustReport' of the value given as coins shows it,
-- at the first receiving address of the bip44 account
oneDustView :: Ctx -> Natural -> [JsonCoin]
oneDustView ctx value = [coinViewAt (legacyPath (Deriv :/ 0 :/ 0)) (coin ctx 2 0 value)]

-- | The report on the dust coins given beside the coins of 50000 and
-- 30000, all sent to one address
dustReportOf :: Ctx -> [Store.Unspent] -> DustReport
dustReportOf ctx dust = dustReport ctx 10 [(otherAddr 0, Remainder)] (twoCoins ctx) dust False

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

    -- The P2WPKH input of a bip84 coin is 68 virtual bytes, 680 at 10
    -- satoshis a byte
    it "is a bip84 coin worth no more than the fee of its P2WPKH input" $ do
      isDustCoin 10 (segwitCoin ctx 1 0 680) `shouldBe` True
      isDustCoin 10 (segwitCoin ctx 1 0 681) `shouldBe` False
      worthSpendingUpTo (segwitCoin ctx 1 0 681) `shouldBe` 10

    -- At 10 satoshis a byte, the input of a bip44 coin costs 148 × 10 =
    -- 1480 and that of a bip84 coin 68 × 10 = 680. A coin of 1000 is
    -- between the two.
    it "is judged by the fee of its own input: 1000 satoshis are dust as a bip44 coin, and not as a bip84 coin" $ do
      isDustCoin 10 (coin ctx 1 0 1000) `shouldBe` True
      isDustCoin 10 (segwitCoin ctx 1 0 1000) `shouldBe` False

    it "costs the fee of its own input: 1480 for a bip44 coin and 680 for a bip84 coin at 10 satoshis a byte" $ do
      inputFee 10 (coin ctx 1 0 1000) `shouldBe` 1480
      inputFee 10 (segwitCoin ctx 1 0 1000) `shouldBe` 680

    -- (1000 - 1) / 148 = 6 and (1000 - 1) / 68 = 14, rounded down
    it "is worth spending up to a higher fee rate as a bip84 coin than as a bip44 coin of the same value" $ do
      worthSpendingUpTo (coin ctx 1 0 1000) `shouldBe` 6
      worthSpendingUpTo (segwitCoin ctx 1 0 1000) `shouldBe` 14

{- Amounts -}

-- One input of 50000 and one of 30000, 80000 in all. To one address, the
-- transaction is 10 + 2 * 148 + 34 = 340 bytes, a fee of 3400; to two, it
-- is 374 bytes, a fee of 3740. How the rest is shared is in
-- "Haskoin.Wallet.Tx.RecipientSpec".
amountSpec :: Ctx -> Spec
amountSpec ctx =
  describe "The amounts of a send-all" $ do
    -- Two coins of a bip84 account: 10 + 2 * 41 + 34 = 126 bytes, then
    -- (2 + 2 * 108) / 4 of witness data, rounded up to 55: 181 virtual
    -- bytes, a fee of 1810
    it "charge bip84 coins the virtual bytes of their P2WPKH inputs" $
      amounts ctx [segwitCoin ctx 1 0 50000, segwitCoin ctx 1 1 30000] [(otherAddr 0, Remainder)] `shouldBe` Right [(otherAddr 0, 78190)]

    -- One coin of each type: 10 + 148 + 41 + 34 = 233 bytes, then (2 + 1 +
    -- 108) / 4 of witness data, rounded up to 28: 261 virtual bytes, a fee
    -- of 2610. 50000 + 30000 - 2610 = 77390
    it "charge each coin of a portfolio the fee of its own input: 148 bytes for a bip44 coin, 68 for a bip84 coin" $ do
      sendAllFee ctx 10 (bothTypes ctx) [otherAddr 0] `shouldBe` 2610
      amounts ctx (bothTypes ctx) [(otherAddr 0, Remainder)] `shouldBe` Right [(otherAddr 0, 77390)]

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
      built.inputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 0), legacyPath (Deriv :/ 0 :/ 1)]
      built.outputPaths `shouldBe` []

    -- 50000 + 30000 less the fee of 2610 of the 261 virtual bytes
    it "spends the coins of both types of a portfolio together, in full, with no change" $ do
      built <- expectRight $ build ctx (found (bothTypes ctx) []) [(otherAddr 0, Remainder)]
      ((.outpoint) <$> built.tx.inputs) `shouldBe` [OutPoint (fakeTxHash 1) 0, OutPoint (fakeTxHash 3) 0]
      outputsOf ctx built.tx `shouldBe` [(otherAddr 0, 77390)]

    it "records the path of each coin with the type of its account" $ do
      built <- expectRight $ build ctx (found (bothTypes ctx) []) [(otherAddr 0, Remainder)]
      built.inputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 0), segwitPath (Deriv :/ 0 :/ 0)]

    -- 80000 - 30000 - 3740 = 46260
    it "pays the fixed amounts and what remains" $ do
      built <- expectRight $ build ctx (found (twoCoins ctx) []) [(otherAddr 0, Fixed 30000), (otherAddr 1, Remainder)]
      outputsOf ctx built.tx `shouldBe` [(otherAddr 0, 30000), (otherAddr 1, 46260)]

    it "records the path of a recipient of the wallet" $ do
      built <- expectRight $ build ctx (found (twoCoins ctx) [(externalAddr 3, legacyPath (Deriv :/ 0 :/ 3))]) [(externalAddr 3, Remainder)]
      built.outputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 3)]

    -- The coins of the bip44 account, moved to an address of the bip84
    -- account of the same portfolio
    it "records the path of a recipient of the other type of the portfolio, with its type" $ do
      built <- expectRight $ build ctx (found (twoCoins ctx) [(segwitExternalAddr 3, segwitPath (Deriv :/ 0 :/ 3))]) [(segwitExternalAddr 3, Remainder)]
      built.outputPaths `shouldBe` [segwitPath (Deriv :/ 0 :/ 3)]

    it "is a version 2 transaction with the lock time given, its inputs marked replaceable on Bitcoin" $ do
      built <- expectRight $ build ctx (found (twoCoins ctx) []) [(otherAddr 0, Remainder)]
      (built.tx.version, built.tx.locktime) `shouldBe` (2, 800002)
      ((.sequence) <$> built.tx.inputs) `shouldBe` [0xfffffffd, 0xfffffffd]

    -- 0xfffffffe: not final, so the lock time applies, and not replaceable
    it "gives every input of Bitcoin Cash, which replaces no transaction, the highest sequence under which the lock time applies" $ do
      built <- expectRight $ buildSendAll bch ctx (mkStdGen 0) (found (twoCoins ctx) []) [(otherAddr 0, Remainder)] 10 800002
      ((.sequence) <$> built.tx.inputs) `shouldBe` [0xfffffffe, 0xfffffffe]

    -- Nodes relay a transaction of at most 100000 virtual bytes. 676 P2PKH
    -- coins sent to one P2PKH output take 12 + 676 × 148 + 34 = 100094
    -- bytes: the coins are those of 676 outputs of one transaction, all at
    -- the first address of the wallet.
    it "is refused more coins than a transaction nodes relay can hold" $ do
      let manyCoins = [storeCoin ctx (fakeTxHash 3, i) (Just 800000) (externalAddr 0) 10000 | i <- [0 .. 675]]
      build ctx (found manyCoins []) [(otherAddr 0, Remainder)]
        `shouldBe` Left "The transaction would take 100094 virtual bytes, and nodes relay at most 100000: spend fewer coins or pay fewer recipients in one transaction"
    it "is refused without coins to send" $
      build ctx (found [] []) [(otherAddr 0, Remainder)] `shouldBe` Left "There are no coins to send"

    it "is refused a fee rate of zero" $
      buildSendAll btc ctx (mkStdGen 0) (found (twoCoins ctx) []) [(otherAddr 0, Remainder)] 0 800002
        `shouldBe` Left "The fee rate must be at least 1 satoshi per virtual byte"

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

    -- In bitcoins, the unit by default: 1480 satoshis are 0.00001480 btc
    -- The coins are outputs of transaction 2, whose txid starts with
    -- 00000000, at the first three receiving addresses of the bip44
    -- account. Each shows as coins shows it, by its short id first, as
    -- --coin takes it.
    it "lists the dust coins, the largest first, each as coins shows it with the rates it is worth spending at, then what spending and leaving them does" $ do
      shown <- renderedLines <$> dustReportDoc btc UnitBitcoin (threeDustViews ctx) (threeDustReport ctx False)
      shown
        `shouldBe` [ "3 coins cost more to spend than they are worth at 10 sat/vB, 0.00001480 btc each:",
                     "",
                     "00000000:0  time unknown           0.00001200 btc  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0",
                     "  worth spending at 8 sat/vB or less",
                     "",
                     "00000000:1  time unknown           0.00000900 btc  spendable, 3 confirmations",
                     "  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  bip44/0/1",
                     "  worth spending at 6 sat/vB or less",
                     "",
                     "00000000:2  time unknown           0.00000546 btc  spendable, 3 confirmations",
                     "  at 1Dg6Kg7kQuyiZz41HRWXKUWKRu6ZyEf1Nr  bip44/0/2",
                     "  worth spending at 3 sat/vB or less",
                     "",
                     "Spending them adds 0.00004440 btc of fee for 0.00002646 btc of coins. What remains for the addresses without an amount is 0.00074806 btc instead of 0.00076600 btc, 0.00001794 btc less, and the portfolio has no coin left on the network.",
                     "Leaving them, the portfolio keeps 0.00002646 btc. Once the other coins are gone, they can only be spent together, at 4 sat/vB or less."
                   ]

    -- In satoshis, as --unit sat asks
    it "does not say the portfolio has no coin left when other coins stay in it" $ do
      shown <- renderedLines <$> dustReportDoc btc UnitSatoshi (threeDustViews ctx) (threeDustReport ctx True)
      (shown !! 14)
        `shouldBe` "Spending them adds 4440 sats of fee for 2646 sats of coins. What remains for the addresses without an amount is 74806 sats instead of 76600 sats, 1794 sats less."

    it "asks whether to spend them too" $
      dustQuestion (threeDustReport ctx False) `shouldBe` "Spend them too?"

    -- Alone, the coin of 1200 to one address is 192 bytes: it leaves at
    -- least 546 at up to (1200 - 546) / 192 = 3 satoshis a byte
    it "tells of one dust coin in the singular" $ do
      let report = oneDustReport ctx 1200
      shown <- renderedLines <$> dustReportDoc btc UnitSatoshi (oneDustView ctx 1200) report
      (shown !! 0) `shouldBe` "1 coin costs more to spend than it is worth at 10 sat/vB, 1480 sats:"
      (shown !! 7) `shouldBe` "Leaving it, the portfolio keeps 1200 sats. Once the other coins are gone, it can only be spent at 3 sat/vB or less."
      dustQuestion report `shouldBe` "Spend it too?"

    -- At 1 satoshi a byte, the 192 bytes leave 600 - 192 = 408
    it "tells when the dust coins can never be spent alone" $ do
      shown <- renderedLines <$> dustReportDoc btc UnitSatoshi (oneDustView ctx 600) (oneDustReport ctx 600)
      (shown !! 7)
        `shouldBe` "Leaving it, the portfolio keeps 600 sats. Once the other coins are gone, it can never be spent: any fee would leave less than 546 sats."

    -- Its input costs 148 at 1 satoshi a byte
    it "tells of a coin worth less than its fee at any rate" $ do
      shown <- renderedLines <$> dustReportDoc btc UnitSatoshi (oneDustView ctx 148) (oneDustReport ctx 148)
      take 3 (drop 2 shown)
        `shouldBe` [ "00000000:0  time unknown                  148 sats  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0",
                     "  worth less than its fee at any rate"
                   ]

    -- A bip84 coin of 600 is dust at 10 satoshis a byte, where its input
    -- costs 680. Beside the two bip44 coins, it makes the transaction one
    -- with witnesses: 10 + 2 * 148 + 41 + 34 = 381 bytes, then (2 + 2 +
    -- 108) / 4 of witness data, 28, are 409 virtual bytes, a fee of 4090,
    -- 690 more than the 3400 of the two coins alone. 80600 - 4090 = 76510.
    it "counts what a bip84 dust coin adds to the fee by its P2WPKH input" $ do
      let report = dustReportOf ctx [segwitCoin ctx 2 0 600]
      (report.addedFee, report.withDust, report.withoutDust) `shouldBe` (690, 76510, 76600)

    -- Alone, the coin of 600 to one P2WPKH address is 10 + 41 + 31 = 82
    -- bytes and 28 of witness data, 110 virtual bytes. It must leave the
    -- 294 of a P2WPKH output: (600 - 294) / 110 = 2 satoshis a byte.
    it "tells when bip84 dust coins can be sent later, to an address of their own type" $ do
      let report = dustReportOf ctx [segwitCoin ctx 2 0 600]
      (report.reportKind, report.dustLimit, report.laterRate) `shouldBe` (Bip84, 294, Just 2)

    -- A bip44 coin of 1200 and a bip84 coin of 600, alone to one P2PKH
    -- address: 10 + 148 + 41 + 34 = 233 bytes and (2 + 1 + 108) / 4 of
    -- witness data, 28, are 261 virtual bytes. They must leave the 546 of
    -- a P2PKH output: (1800 - 546) / 261 = 4 satoshis a byte.
    it "tells when dust coins of both types can be sent later, to a bip44 address" $ do
      let report = dustReportOf ctx [coin ctx 2 0 1200, segwitCoin ctx 2 1 600]
      (report.reportKind, report.dustLimit, report.laterRate) `shouldBe` (Bip44, 546, Just 4)

    -- The input of the bip44 coin costs 1480 and that of the bip84 coin
    -- 680, so the first line gives no fee, and each coin gives its own.
    -- The coin of 1200 is worth spending up to (1200 - 1) / 148 = 8
    -- satoshis a byte, and the coin of 600 up to (600 - 1) / 68 = 8.
    it "gives each dust coin the fee of its own input when the dust coins are of both types" $ do
      let views = [coinViewAt (legacyPath (Deriv :/ 0 :/ 0)) (coin ctx 2 0 1200), coinViewAt (segwitPath (Deriv :/ 0 :/ 1)) (segwitCoin ctx 2 1 600)]
      shown <- renderedLines <$> dustReportDoc btc UnitSatoshi views (dustReportOf ctx [coin ctx 2 0 1200, segwitCoin ctx 2 1 600])
      take 9 shown
        `shouldBe` [ "2 coins cost more to spend than they are worth at 10 sat/vB:",
                     "",
                     "00000000:0  time unknown                 1200 sats  spendable, 3 confirmations",
                     "  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0",
                     "  costs 1480 sats, worth spending at 8 sat/vB or less",
                     "",
                     "00000000:1  time unknown                  600 sats  spendable, 3 confirmations",
                     "  at bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2  bip84/0/1",
                     "  costs 680 sats, worth spending at 8 sat/vB or less"
                   ]

{- The signing data, from a portfolio in the database -}

-- Portfolio 0 of the test wallet holds a coin of 50000 at the first
-- receiving address of its bip44 account and one of 30000 at that of its
-- bip84 account, both confirmed. Sent together to one P2PKH address, they
-- make the 261 virtual bytes of "The amounts of a send-all": a fee of
-- 2610, and 77390 for the recipient.
signDataSpec :: Ctx -> Spec
signDataSpec ctx =
  describe "The signing data of a send-all of a portfolio with coins of both types" $ do
    it "sends the coins of both accounts in one transaction with no change, each input paying the fee of its own size" $
      inDatabaseE $ do
        pool <- fundedBoth ctx def [50000] [30000]
        spendable <- spendableCoins ctx pool 1
        tsd <- buildSendAllSignData ctx (mkStdGen 0) pool spendable [(otherAddr 0, Remainder)] 10 800002
        liftIO $ do
          ((.outpoint) <$> tsd.txSignDataTx.inputs)
            `shouldMatchList` [OutPoint (txHash (fundingWorth ctx [50000])) 0, OutPoint (txHash (segwitFundingWorth ctx [30000])) 0]
          outputsOf ctx tsd.txSignDataTx `shouldBe` [(otherAddr 0, 77390)]

    it "carries the path of each coin with the type of its account, and the transactions both coins come from" $
      inDatabaseE $ do
        pool <- fundedBoth ctx def [50000] [30000]
        spendable <- spendableCoins ctx pool 1
        tsd <- buildSendAllSignData ctx (mkStdGen 0) pool spendable [(otherAddr 0, Remainder)] 10 800002
        liftIO $ do
          tsd.txSignDataInputPaths `shouldMatchList` [legacyPath (Deriv :/ 0 :/ 0), segwitPath (Deriv :/ 0 :/ 0)]
          tsd.txSignDataOutputPaths `shouldBe` []
          tsd.txSignDataInputs `shouldMatchList` [fundingWorth ctx [50000], segwitFundingWorth ctx [30000]]

    -- The bip44 coin alone, as --coin-type bip44 keeps it, to the first
    -- receiving address of the bip84 account: 10 + 148 + 31 = 189 bytes, a
    -- fee of 1890, and 50000 - 1890 = 48110. The coin of the bip84 account
    -- stays.
    it "moves the coins of one type to an address of the other type of the portfolio, whose path it records with its type" $
      inDatabaseE $ do
        pool <- fundedBoth ctx def [50000] [30000]
        ofType <- fetchWalletCoins ctx pool (Just Bip44) [] 1
        tsd <- buildSendAllSignData ctx (mkStdGen 0) pool ofType.coins [(segwitExternalAddr 0, Remainder)] 10 800002
        liftIO $ do
          ((.outpoint) <$> tsd.txSignDataTx.inputs) `shouldBe` [OutPoint (txHash (fundingWorth ctx [50000])) 0]
          outputsOf ctx tsd.txSignDataTx `shouldBe` [(segwitExternalAddr 0, 48110)]
          tsd.txSignDataOutputPaths `shouldBe` [segwitPath (Deriv :/ 0 :/ 0)]
