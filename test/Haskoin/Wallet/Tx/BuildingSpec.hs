{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The transaction builder: what it spends and pays, who pays the fee, coin
-- control, the strategies, the draws and shuffles decided by its seed, and
-- every error it reports. The coin selection itself is covered by
-- "Haskoin.Wallet.Tx.CoinSelectionSpec".
module Haskoin.Wallet.Tx.BuildingSpec where

import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Default (def)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account (getAccountById)
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Tx.CoinSelection
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.RawTx
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.Building
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..))
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (parseTxSignData, signWalletTx, verifySignedTx)
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
    changeBackSpec ctx

-- Every test pays from coins of the test wallet, funded by one synthetic
-- transaction, at 10 satoshis per virtual byte. The sizes are those of the
-- coin selection: 10 bytes of frame, 148 per input and 34 per output. The usual transaction, one input and two outputs, is 226 bytes
-- and costs 2260; without a change output it is 192 bytes and costs 1920.

{- Helpers -}

-- | The outpoint of the i-th coin of 'fundingWorth'
coinAt :: Ctx -> [Natural] -> Word32 -> OutPoint
coinAt ctx values = OutPoint (txHash $ fundingWorth ctx values)

-- | The wallet as the builder sees it: the coins, the paths of the first ten
-- external addresses, and the recipients that belong to the wallet with
-- their paths
wallet :: [Store.Unspent] -> [(Address, SoftPath)] -> WalletCoins
wallet unspents recipients =
  WalletCoins
    { ownKind = Bip44,
      coins = unspents,
      coinPaths = Map.fromList [(externalAddr i, Deriv :/ 0 :/ fromIntegral i) | i <- [0 .. 9]],
      recipientPaths = Map.fromList recipients
    }

-- | The change address of the builds: /1/0
firstChange :: (Address, SoftPath)
firstChange = (internalAddr 0, Deriv :/ 1 :/ 0)

-- | The bip84 account of the test wallet as the builder sees it: a coin of
-- 400000 at its first receiving address, /0/0
segwitWallet :: Ctx -> WalletCoins
segwitWallet ctx =
  WalletCoins
    { ownKind = Bip84,
      coins = coinsOf ctx (fundingTx ctx 1 [(segwitExternalAddr 0, 400000)]),
      coinPaths = Map.fromList [(segwitExternalAddr 0, Deriv :/ 0 :/ 0)],
      recipientPaths = Map.empty
    }

-- | Bitcoin Cash addresses of other kinds: the P2SH32 address of the
-- 32-byte hash 1863143c...903262, the token-aware address of the 20-byte
-- hash 76a04053...559873, and the plain P2PKH address of that hash. The
-- hashes are those of the examples of the CashAddr specification. The
-- addresses were encoded apart from haskoin-core, with an encoder written
-- from the specification.
script32Addr, tokenAddr, plainTokenAddr :: Address
script32Addr = fromJust $ textToAddr bch "bitcoincash:pvvxx9puznz3v6qyh5vjqv6kmgfkexzk0rx56faphrrr99syjqexyrusee847"
tokenAddr = fromJust $ textToAddr bch "bitcoincash:zpm2qsznhks23z7629mms6s4cwef74vcwvrqekrq9w"
plainTokenAddr = fromJust $ textToAddr bch "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"

-- | Build with the seed 0 and the given strategy, wallet, recipients and
-- payer, at 10 satoshis per virtual byte
build :: Ctx -> CoinSelection -> WalletCoins -> [(Address, Natural)] -> FeePayer -> Either String BuiltTx
build ctx strategy found recipients payer =
  buildWalletTx btc ctx (mkStdGen 0) strategy found firstChange recipients 10 800002 payer NoCoinControl

-- | Pay recipients from the wallet's coins, the largest first, the sender
-- paying the fee, at 10 satoshis per virtual byte, with the change address
-- given
payWithChange :: Ctx -> WalletCoins -> (Address, SoftPath) -> [(Address, Natural)] -> Either String BuiltTx
payWithChange ctx found change recipients =
  buildWalletTx btc ctx (mkStdGen 0) LargestFirst found change recipients 10 800002 SenderPays NoCoinControl

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
  buildWalletTx btc ctx (mkStdGen 0) LargestFirst (wallet (coinsWorth ctx values) []) firstChange recipients 10 800002 SenderPays (SpendChosen (coinAt ctx values <$> indices))

-- | Like 'payFrom', the strategy selecting among the coins at the given
-- indices instead of spending them all (--select)
selectAmong :: Ctx -> CoinSelection -> [Natural] -> [Word32] -> [(Address, Natural)] -> Either String BuiltTx
selectAmong ctx strategy values indices recipients =
  buildWalletTx btc ctx (mkStdGen 0) strategy (wallet (coinsWorth ctx values) []) firstChange recipients 10 800002 SenderPays (SelectAmongChosen (coinAt ctx values <$> indices))

-- | Like 'pay', with the given seed and strategy
payWith :: Ctx -> Int -> CoinSelection -> [Natural] -> [(Address, Natural)] -> Either String BuiltTx
payWith ctx seed strategy values recipients =
  buildWalletTx btc ctx (mkStdGen seed) strategy (wallet (coinsWorth ctx values) []) firstChange recipients 10 800002 SenderPays NoCoinControl

-- | What a built transaction spends and pays: the amounts of its inputs, in
-- order, and its outputs by amount, since the builder shuffles them
spends :: Ctx -> BuiltTx -> ([Word64], [(Address, Word64)])
spends = spendsOn btc

-- | 'spends' for a transaction of the network given. The network says
-- which address a script has: a P2SH32 output has one on Bitcoin Cash
-- alone.
spendsOn :: Network -> Ctx -> BuiltTx -> ([Word64], [(Address, Word64)])
spendsOn net ctx built = ((.value) <$> built.spentCoins, sortOn snd (outputsOn net ctx built.tx))

-- | The outputs of a transaction of Bitcoin as addresses and amounts, in
-- order
outputsOf :: Ctx -> Tx -> [(Address, Word64)]
outputsOf = outputsOn btc

-- | 'outputsOf' for a transaction of the network given
outputsOn :: Network -> Ctx -> Tx -> [(Address, Word64)]
outputsOn net ctx transaction = [(forceRight (scriptToAddressBS net ctx out.script), out.value) | out <- transaction.outputs]

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
    -- 10 + 41 + 34 + 31 = 116 bytes, then (2 + 108) / 4 of witness data,
    -- rounded up to 28: 144 virtual bytes, a fee of 1440 at 10, and 400000
    -- - 350000 - 1440 = 48560 of change, at the change address /1/0 of the
    -- account
    it "pays the fee of a bip84 account on its P2WPKH input and change, in virtual bytes" $ do
      built <- expectRight $ buildWalletTx btc ctx (mkStdGen 0) LargestFirst (segwitWallet ctx) (segwitInternalAddr 0, Deriv :/ 1 :/ 0) [(otherAddr 0, 350000)] 10 800002 SenderPays NoCoinControl
      spends ctx built `shouldBe` ([400000], [(segwitInternalAddr 0, 48560), (otherAddr 0, 350000)])
      feePaid built `shouldBe` 1440
      (built.inputPaths, built.outputPaths) `shouldBe` ([Deriv :/ 0 :/ 0], [Deriv :/ 1 :/ 0])
    it "is a version 2 transaction with the lock time given and its inputs marked replaceable" $ do
      built <- expectRight $ pay ctx [400000] [(otherAddr 0, 350000)]
      sortedOutputs built.tx
        `shouldBe` builtTx ctx 800002 [(txHash (fundingWorth ctx [400000]), 0)] [(internalAddr 0, 47740), (otherAddr 0, 350000)]
    -- 0xfffffffe: not final, so the lock time applies, and not replaceable
    it "gives every input of Bitcoin Cash, which replaces no transaction, the highest sequence under which the lock time applies" $ do
      let cashCoin = storeCoin ctx (fakeTxHash 8, 0) (Just 800000) bchExternalAddr 400000
          cashWallet = WalletCoins {ownKind = Bip44, coins = [cashCoin], coinPaths = Map.fromList [(bchExternalAddr, Deriv :/ 0 :/ 0)], recipientPaths = Map.empty}
      built <- expectRight $ buildWalletTx bch ctx (mkStdGen 0) LargestFirst cashWallet firstChange [(otherAddr 0, 350000)] 10 800002 SenderPays NoCoinControl
      ((.sequence) <$> built.tx.inputs) `shouldBe` [0xfffffffe]
      (built.tx.version, built.tx.locktime) `shouldBe` (2, 800002)
    -- Fee: 10 + 148 + 44 (a P2SH32 output: 8 + 1 + 35) + 34 + 34 = 270 bytes * 10 = 2700
    -- Change: 400000 - 100000 - 50000 - 2700 = 247300
    it "pays a P2SH32 address and a token-aware address on Bitcoin Cash, the latter with the plain P2PKH script" $ do
      let cashCoin = storeCoin ctx (fakeTxHash 8, 0) (Just 800000) bchExternalAddr 400000
          cashWallet = WalletCoins {ownKind = Bip44, coins = [cashCoin], coinPaths = Map.fromList [(bchExternalAddr, Deriv :/ 0 :/ 0)], recipientPaths = Map.empty}
      built <- expectRight $ buildWalletTx bch ctx (mkStdGen 0) LargestFirst cashWallet firstChange [(script32Addr, 100000), (tokenAddr, 50000)] 10 800002 SenderPays NoCoinControl
      -- The coin of the token-aware address shows at the plain address of the same hash
      spendsOn bch ctx built `shouldBe` ([400000], [(plainTokenAddr, 50000), (script32Addr, 100000), (internalAddr 0, 247300)])
      -- OP_DUP OP_HASH160 <20 bytes> OP_EQUALVERIFY OP_CHECKSIG, then OP_HASH256 <32 bytes> OP_EQUAL
      (encodeHex . (.script) <$> take 2 (sortOn (.value) built.tx.outputs))
        `shouldBe` [ "76a91476a04053bda0a88bda5177b86a15c3b29f55987388ac",
                     "aa201863143c14c5166804bd19203356da136c985678cd4d27a1b8c632960490326287"
                   ]
      feePaid built `shouldBe` 2700
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
-- "Haskoin.Wallet.Tx.SweepSpec" and the examples of BIP173 and BIP350.
recipientTypesSpec :: Ctx -> Spec
recipientTypesSpec ctx =
  describe "Recipients of every standard address type" $ do
    it "pays a P2SH recipient: 10 + 148 + 32 + 34 = 224 bytes" $ do
      built <- expectRight $ pay ctx [400000] [(scriptAddr, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47760), (scriptAddr, 350000)])
      feePaid built `shouldBe` 2240
    it "pays a P2WPKH recipient: 10 + 148 + 31 + 34 = 223 bytes" $ do
      built <- expectRight $ pay ctx [400000] [(witnessAddr, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47770), (witnessAddr, 350000)])
      feePaid built `shouldBe` 2230
    it "pays a P2WSH recipient: 10 + 148 + 43 + 34 = 235 bytes" $ do
      built <- expectRight $ pay ctx [400000] [(witnessScriptAddr, 350000)]
      spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47650), (witnessScriptAddr, 350000)])
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
                       (scriptAddr, 60000),
                       (witnessAddr, 70000),
                       (witnessScriptAddr, 80000),
                       (taprootAddr, 90000)
                     ]
                   )
      feePaid built `shouldBe` 3750
      built.outputPaths `shouldBe` [Deriv :/ 1 :/ 0]
    it "reports, signs and verifies the payment to every type" $ do
      built <- expectRight $ pay ctx [400000] everyType
      let tsd = TxSignData built.tx [fundingWorth ctx [400000]] built.inputPaths built.outputPaths (Just 10) Nothing [] noLabel
      unsignedInfo <- expectRight $ parseTxSignData btc ctx Bip44 (testAccountPub ctx) tsd
      (unsignedInfo.txInfoSummary.summarySize, unsignedInfo.txInfoSummary.summaryFeeByte, unsignedInfo.txInfoSummary.summaryChangeInFee) `shouldBe` (375, 10, Just 0)
      (signed, info) <- expectRight $ signWalletTx btc ctx Bip44 tsd testAccountKey
      verifySignedTx btc ctx signed `shouldBe` Right ()
      -- A signature is 72 bytes or 71, as its values fall, and the estimate
      -- counts 72. The signature of this transaction is 71 bytes: 374 bytes
      -- signed, one less than the 375 estimated
      (info.txInfoSummary.summaryFee, info.txInfoSummary.summarySize, info.txInfoSummary.summaryFeeByte) `shouldBe` (3750, 374, 10)
      Map.toList info.txInfoSummary.summaryOtherOutputs
        `shouldBe` [(otherAddr 0, 50000), (scriptAddr, 60000), (witnessAddr, 70000), (witnessScriptAddr, 80000), (taprootAddr, 90000)]
  where
    everyType = [(otherAddr 0, 50000), (scriptAddr, 60000), (witnessAddr, 70000), (witnessScriptAddr, 80000), (taprootAddr, 90000)]
    scriptAddr = fromJust $ textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p"
    witnessAddr = fromJust $ textToAddr btc "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
    witnessScriptAddr = fromJust $ textToAddr btc "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
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

    -- Each output has the limit nodes relay for its kind: 546 for a P2PKH
    -- output, 294 for a P2WPKH one. The payment
    -- of the bip84 account takes 144 virtual bytes with its change, a fee
    -- of 1440.
    it "keeps a P2WPKH change of 294 satoshis, the least nodes relay" $ do
      -- Change: 400000 - 398266 - 1440 = 294
      built <- expectRight $ payWithChange ctx (segwitWallet ctx) segwitChange [(otherAddr 0, 398266)]
      spends ctx built `shouldBe` ([400000], [(segwitInternalAddr 0, 294), (otherAddr 0, 398266)])
      feePaid built `shouldBe` 1440
    it "gives a P2WPKH change of 293 satoshis to the fee" $ do
      -- Change: 400000 - 398267 - 1440 = 293: no change output, and the
      -- fee is 400000 - 398267 = 1733
      built <- expectRight $ payWithChange ctx (segwitWallet ctx) segwitChange [(otherAddr 0, 398267)]
      spends ctx built `shouldBe` ([400000], [(otherAddr 0, 398267)])
      feePaid built `shouldBe` 1733
    it "gives a P2PKH change of 545 satoshis to the fee" $ do
      -- Change: 352805 - 350000 - 2260 = 545, below the 546 nodes relay
      built <- expectRight $ payWithChange ctx (wallet (coinsWorth ctx [352805]) []) firstChange [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([352805], [(otherAddr 0, 350000)])
    it "pays a P2WPKH recipient 294 satoshis, and refuses 293" $ do
      -- Fee: 10 + 148 + 31 + 34 = 223 bytes * 10 = 2230
      -- Change: 400000 - 294 - 2230 = 397476
      built <- expectRight $ payWithChange ctx (wallet (coinsWorth ctx [400000]) []) firstChange [(segwitExternalAddr 1, 294)]
      spends ctx built `shouldBe` ([400000], [(segwitExternalAddr 1, 294), (internalAddr 0, 397476)])
      payWithChange ctx (wallet (coinsWorth ctx [400000]) []) firstChange [(segwitExternalAddr 1, 293)]
        `shouldBe` Left "Recipient output is smaller than the dust value"
    it "refuses a P2PKH recipient paid 545 satoshis" $
      payWithChange ctx (wallet (coinsWorth ctx [400000]) []) firstChange [(otherAddr 0, 545)]
        `shouldBe` Left "Recipient output is smaller than the dust value"
  where
    segwitChange = (segwitInternalAddr 0, Deriv :/ 1 :/ 0)

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
      payFrom ctx four [5] [(otherAddr 0, 50000)] `shouldBe` Left "A chosen coin does not exist or is not spendable"
    it "fails when the listed coins do not cover the amount and the fee" $
      -- 100000 - 1480 < 350000 + 440
      payFrom ctx four [0] [(otherAddr 0, 350000)] `shouldBe` Left "The chosen coins do not cover the amount and the fee"

    -- With --select, the strategy selects among the chosen coins
    describe "with --select" $ do
      it "spends only what the payment needs among the chosen coins" $ do
        -- The largest chosen coin, 300000, pays 250000 alone
        -- Fee: 226 bytes * 10 = 2260; change: 300000 - 250000 - 2260 = 47740
        built <- expectRight $ selectAmong ctx LargestFirst four [0, 1, 2] [(otherAddr 0, 250000)]
        spends ctx built `shouldBe` ([300000], [(internalAddr 0, 47740), (otherAddr 0, 250000)])
      it "never spends a coin that is not chosen" $ do
        -- 400000 would pay alone, but is not chosen. 200000 - 1480 < 250000
        -- + 440, so both chosen coins are spent, the largest first.
        -- Fee: 374 bytes * 10 = 3740; change: 100000 + 200000 - 250000 - 3740 = 46260
        built <- expectRight $ selectAmong ctx LargestFirst four [0, 1] [(otherAddr 0, 250000)]
        spends ctx built `shouldBe` ([200000, 100000], [(internalAddr 0, 46260), (otherAddr 0, 250000)])
      it "follows the strategy among the chosen coins" $ do
        -- Smallest first: 100000 and 200000 cover 250000 + 440 once each
        -- pays its 1480. Change: 100000 + 200000 - 250000 - 3740 = 46260
        built <- expectRight $ selectAmong ctx SmallestFirst four [0, 1, 2] [(otherAddr 0, 250000)]
        spends ctx built `shouldBe` ([100000, 200000], [(internalAddr 0, 46260), (otherAddr 0, 250000)])
      it "leaves a chosen coin worth less than the fee of its input" $ do
        -- 1000 - 1480 < 0: spent without --select, left with it
        -- Change: 400000 - 350000 - 2260 = 47740
        built <- expectRight $ selectAmong ctx LargestFirst [1000, 400000] [0, 1] [(otherAddr 0, 350000)]
        spends ctx built `shouldBe` ([400000], [(internalAddr 0, 47740), (otherAddr 0, 350000)])
      it "fails when a chosen coin does not exist" $
        selectAmong ctx LargestFirst four [5] [(otherAddr 0, 50000)] `shouldBe` Left "A chosen coin does not exist or is not spendable"
      it "fails when the chosen coins do not cover the amount and the fee, though the account does" $
        -- 100000 - 1480 < 350000 + 440, and 400000 is not chosen
        selectAmong ctx LargestFirst four [0] [(otherAddr 0, 350000)] `shouldBe` Left "The coins do not cover the amount and the fee"

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
    -- 350000 paid from a coin of 400000, at a fee rate
    let payAt feeByte =
          buildWalletTx btc ctx (mkStdGen 0) LargestFirst (wallet (coinsWorth ctx [400000]) []) firstChange [(otherAddr 0, 350000)] feeByte 800002 SenderPays NoCoinControl
    it "fails without recipients" $
      pay ctx [400000] [] `shouldBe` Left "No recipients provided"
    it "fails at a fee rate of zero" $
      payAt 0 `shouldBe` Left "The fee rate must be at least 1 satoshi per virtual byte"
    -- A rate of 2^64 wrapped around to zero in the 64 bits of the builder
    it "fails at a fee rate above 2^32 - 1 satoshis per virtual byte" $
      payAt 4294967296 `shouldBe` Left "The fee rate must be at most 4294967295 satoshis per virtual byte"
    -- 226 bytes at 4294967295 satoshis each, far more than the coin, and
    -- far less than 2^64
    it "counts the fee at the highest rate without wrapping around" $
      payAt 4294967295 `shouldBe` Left "The coins do not cover the amount and the fee"
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
fundedAccount ctx cfg = fundedAt ctx cfg [400000]

-- | An account of the test wallet holding a coin of each value at its
-- external addresses in order, /0/0 first, confirmed at height 0, the best
-- block, and paid by 'fundingWorth'
fundedAt :: Ctx -> Config -> [Natural] -> ExceptT String (DB IO) (DBAccountId, Tx)
fundedAt ctx cfg values = do
  (accId, _) <- testNewAcc ctx "test"
  lift $ updateBest btc (fakeBlockHash 0) 0 1700000000
  mapM_ (const $ genExtAddress ctx cfg accId "") values
  let fund = fundingWorth ctx values
      addrs = externalAddr <$> [0 .. length values - 1]
  lift $ insertRawTx fund
  _ <- refreshCoins btc ctx accId addrs [storeCoin ctx (txHash fund, i) (Just 0) addr value | (i, addr, value) <- zip3 [0 ..] addrs values]
  return (accId, fund)

databaseSpec :: Ctx -> Spec
databaseSpec ctx =
  describe "With the database" $ do
    let cfg = def :: Config
    it "fetches the coins with their paths and the wallet recipients" $
      inDatabaseE $ do
        (accId, fund) <- fundedAccount ctx cfg
        found <- fetchWalletCoins btc ctx accId [otherAddr 0, externalAddr 0] 1
        liftIO $ do
          found.coins `shouldBe` [storeCoin ctx (txHash fund, 0) (Just 0) (externalAddr 0) 400000]
          found.coinPaths `shouldBe` Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0)]
          found.recipientPaths `shouldBe` Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0)]
    it "leaves out the coins without enough confirmations" $
      inDatabaseE $ do
        (accId, _) <- fundedAccount ctx cfg
        -- The coin at the best block has one confirmation
        found <- fetchWalletCoins btc ctx accId [] 2
        liftIO $ found.coins `shouldBe` []
    it "builds the signing data with the transactions spent from" $
      inDatabaseE $ do
        (accId, fund) <- fundedAccount ctx cfg
        tsd <- buildTxSignData btc ctx cfg (mkStdGen 0) LargestFirst accId [(otherAddr 0, 350000)] 10 800002 SenderPays 1 NoCoinControl NewChangeAddress
        liftIO $ do
          sortedOutputs (txSignDataTx tsd)
            `shouldBe` builtTx ctx 800002 [(txHash fund, 0)] [(internalAddr 0, 47740), (otherAddr 0, 350000)]
          txSignDataInputs tsd `shouldBe` [fund]
          txSignDataInputPaths tsd `shouldBe` [Deriv :/ 0 :/ 0]
          txSignDataOutputPaths tsd `shouldBe` [Deriv :/ 1 :/ 0]
          txSignDataFeeRate tsd `shouldBe` Just 10
          isSigned (txSignDataTx tsd) `shouldBe` False
    it "fails on a coin whose raw transaction is missing, naming it and the full sync that fetches it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        lift $ updateBest btc (fakeBlockHash 0) 0 1700000000
        _ <- genExtAddress ctx cfg accId ""
        _ <- refreshCoins btc ctx accId [externalAddr 0] [storeCoin ctx (fakeTxHash 9, 0) (Just 0) (externalAddr 0) 400000]
        result <- lift $ runExceptT $ buildTxSignData btc ctx cfg (mkStdGen 0) LargestFirst accId [(otherAddr 0, 350000)] 10 800002 SenderPays 1 NoCoinControl NewChangeAddress
        liftIO $
          result
            `shouldBe` Left "The raw transaction 0000000000000000000000000000000000000000000000000000000000000009 of a coin is missing: sync --full fetches it"
    it "fails without recipients" $
      inDatabaseE $ do
        (accId, _) <- fundedAccount ctx cfg
        result <- lift $ runExceptT $ buildTxSignData btc ctx cfg (mkStdGen 0) LargestFirst accId [] 10 800002 SenderPays 1 NoCoinControl NewChangeAddress
        liftIO $ result `shouldBe` Left "No recipients provided"

{- Sending the change back -}

-- | A payment of the amount to otherAddr 0 from the account, by the largest
-- coins first, at 10 satoshis per virtual byte
payBack :: Ctx -> Config -> DBAccountId -> Natural -> ChangeTo IO -> ExceptT String (DB IO) TxSignData
payBack ctx cfg accId amount =
  buildTxSignData btc ctx cfg (mkStdGen 0) LargestFirst accId [(otherAddr 0, amount)] 10 800002 SenderPays 1 NoCoinControl

-- | Send the change back to the given address, keeping the addresses it
-- was offered
pickingAndKeeping :: IORef [Address] -> Address -> ChangeTo IO
pickingAndKeeping offered picked = ChangeBack $ \addrs -> liftIO (writeIORef offered addrs) >> return picked

changeBackSpec :: Ctx -> Spec
changeBackSpec ctx = do
  describe "The change sent back to an address the payment spends from" $ do
    let cfg = def :: Config
    -- Fee: 10 + 2 * 148 + 2 * 34 = 374 bytes * 10 = 3740
    -- Change: 30000 + 20000 - 40000 - 3740 = 6260
    it "goes to the address picked, with its path" $
      inDatabaseE $ do
        (accId, _) <- fundedAt ctx cfg [30000, 20000]
        offered <- liftIO $ newIORef []
        tsd <- payBack ctx cfg accId 40000 (pickingAndKeeping offered (externalAddr 1))
        liftIO $ do
          sortOn snd (outputsOf ctx (txSignDataTx tsd)) `shouldBe` [(externalAddr 1, 6260), (otherAddr 0, 40000)]
          txSignDataOutputPaths tsd `shouldBe` [Deriv :/ 0 :/ 1]

    it "is picked among the addresses the coins of the payment come from" $
      inDatabaseE $ do
        (accId, _) <- fundedAt ctx cfg [30000, 20000]
        offered <- liftIO $ newIORef []
        _ <- payBack ctx cfg accId 40000 (pickingAndKeeping offered (externalAddr 1))
        liftIO $ readIORef offered `shouldReturn` [externalAddr 0, externalAddr 1]

    -- The coin of 30000 alone pays 20000: the coin at /0/1 is not spent
    it "is not picked at an address whose coins the payment does not spend" $
      inDatabaseE $ do
        (accId, _) <- fundedAt ctx cfg [30000, 20000]
        offered <- liftIO $ newIORef []
        _ <- payBack ctx cfg accId 20000 (pickingAndKeeping offered (externalAddr 0))
        liftIO $ readIORef offered `shouldReturn` [externalAddr 0]

    it "takes no new change address" $
      inDatabaseE $ do
        (accId, _) <- fundedAccount ctx cfg
        offered <- liftIO $ newIORef []
        _ <- payBack ctx cfg accId 350000 (pickingAndKeeping offered (externalAddr 0))
        (dBAccountInternal <$> getAccountById accId) `dbShouldBeE` 0

    it "is refused at an address the payment does not spend from" $
      inDatabaseE $ do
        (accId, _) <- fundedAccount ctx cfg
        offered <- liftIO $ newIORef []
        result <- lift $ runExceptT $ payBack ctx cfg accId 350000 (pickingAndKeeping offered (externalAddr 1))
        liftIO $ result `shouldBe` Left "The change can only go back to an address the payment spends from"

    -- Fee: 10 + 148 + 34 = 192 bytes * 10 = 1920, and 400000 - 1920 = 398080
    it "is not asked for by a payment without change" $
      inDatabaseE $ do
        (accId, _) <- fundedAccount ctx cfg
        tsd <- payBack ctx cfg accId 398080 (ChangeBack (const $ throwError "The change address was asked for"))
        liftIO $ outputsOf ctx (txSignDataTx tsd) `shouldBe` [(otherAddr 0, 398080)]

  describe "The change address of a payment" $ do
    let cfg = def :: Config
    it "is not taken by a payment without change" $
      inDatabaseE $ do
        (accId, _) <- fundedAccount ctx cfg
        _ <- payBack ctx cfg accId 398080 NewChangeAddress
        (dBAccountInternal <$> getAccountById accId) `dbShouldBeE` 0

    it "must be given to assemble a payment with change" $ do
      chosen <- expectRight $ choosePayment ctx (mkStdGen 0) LargestFirst (wallet (coinsWorth ctx [400000]) []) [(otherAddr 0, 350000)] 10 SenderPays NoCoinControl
      (.tx) <$> assemblePayment btc ctx (wallet (coinsWorth ctx [400000]) []) chosen Nothing 800002
        `shouldBe` Left "The payment has change, and no address for it"
