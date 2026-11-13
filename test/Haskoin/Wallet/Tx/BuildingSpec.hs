{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The transaction builder: what it spends and pays, who pays the fee, coin
-- control, the strategies, the draws and shuffles decided by its seed, and
-- every error it reports; then what it builds from a portfolio in the
-- database, whose coins may be of both types: the coins of one type alone,
-- the type of the change, and the change sent back to an address. The coin
-- selection itself is covered by "Haskoin.Wallet.Tx.CoinSelectionSpec".
module Haskoin.Wallet.Tx.BuildingSpec where

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson ((.=))
import Data.Default (def)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account (getAccountById)
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.Portfolio (portfolioPool)
import Haskoin.Wallet.Tx.CoinSelection
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.RawTx
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath, legacyPath)
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
    bothTypesSpec ctx
    databaseSpec ctx
    coinTypeSpec ctx
    changeTypeSpec ctx
    changeBackSpec ctx
    cashSpec ctx

-- Every test pays from coins of the test wallet, funded by one synthetic
-- transaction, at 10 satoshis per virtual byte. The sizes are those of the
-- coin selection: 10 bytes of frame, 148 per P2PKH input and 34 per P2PKH
-- output. The usual transaction, one input and two outputs, is 226 bytes
-- and costs 2260; without a change output it is 192 bytes and costs 1920.
-- A P2WPKH coin, of the bip84 account, has an input of 41 bytes and a
-- witness of 108, and a P2WPKH output is 31 bytes. The witnesses count a
-- quarter, rounded up, with the 2 bytes that mark a transaction that has
-- some and 1 byte for each P2PKH input beside them.

{- Helpers -}

-- | The outpoint of the i-th coin of 'fundingWorth'
coinAt :: Ctx -> [Natural] -> Word32 -> OutPoint
coinAt ctx values = OutPoint (txHash $ fundingWorth ctx values)

-- | The portfolio as the builder sees it: the coins, the paths of the first
-- ten receiving addresses of its bip44 account and of its bip84 account, and
-- the recipients that belong to the portfolio with their paths
wallet :: [Store.Unspent] -> [(Address, PoolPath)] -> WalletCoins
wallet unspents recipients =
  WalletCoins
    { coins = unspents,
      coinPaths =
        Map.fromList $
          [(externalAddr i, legacyPath (Deriv :/ 0 :/ fromIntegral i)) | i <- [0 .. 9]]
            <> [(segwitExternalAddr i, segwitPath (Deriv :/ 0 :/ fromIntegral i)) | i <- [0 .. 9]],
      recipientPaths = Map.fromList recipients
    }

-- | The change address of the builds: /1/0 of the bip44 account
firstChange :: (Address, PoolPath)
firstChange = (internalAddr 0, legacyPath (Deriv :/ 1 :/ 0))

-- | The first change address of the bip84 account, /1/0
segwitChange :: (Address, PoolPath)
segwitChange = (segwitInternalAddr 0, segwitPath (Deriv :/ 1 :/ 0))

-- | The portfolio with one coin, of its bip84 account: 400000 at the first
-- receiving address of that account, /0/0
segwitWallet :: Ctx -> WalletCoins
segwitWallet ctx = wallet (segwitCoinsWorth ctx [400000]) []

-- | The portfolio on Bitcoin Cash with one coin: 400000 at the first
-- receiving address of its account there
cashWallet :: Ctx -> WalletCoins
cashWallet ctx =
  WalletCoins
    { coins = [storeCoin ctx (fakeTxHash 8, 0) (Just 800000) bchExternalAddr 400000],
      coinPaths = Map.fromList [(bchExternalAddr, legacyPath (Deriv :/ 0 :/ 0))],
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
payWithChange :: Ctx -> WalletCoins -> (Address, PoolPath) -> [(Address, Natural)] -> Either String BuiltTx
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
      built.inputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 0)]
      built.outputPaths `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]
    -- 10 + 41 + 34 + 31 = 116 bytes, then (2 + 108) / 4 of witness data,
    -- rounded up to 28: 144 virtual bytes, a fee of 1440 at 10, and 400000
    -- - 350000 - 1440 = 48560 of change, at the change address /1/0 of the
    -- bip84 account
    it "pays the fee of a payment of a bip84 coin on its P2WPKH input and change, in virtual bytes" $ do
      built <- expectRight $ payWithChange ctx (segwitWallet ctx) segwitChange [(otherAddr 0, 350000)]
      spends ctx built `shouldBe` ([400000], [(segwitInternalAddr 0, 48560), (otherAddr 0, 350000)])
      feePaid built `shouldBe` 1440
      (built.inputPaths, built.outputPaths) `shouldBe` ([segwitPath (Deriv :/ 0 :/ 0)], [segwitPath (Deriv :/ 1 :/ 0)])
    it "is a version 2 transaction with the lock time given and its inputs marked replaceable" $ do
      built <- expectRight $ pay ctx [400000] [(otherAddr 0, 350000)]
      sortedOutputs built.tx
        `shouldBe` builtTx ctx 800002 [(txHash (fundingWorth ctx [400000]), 0)] [(internalAddr 0, 47740), (otherAddr 0, 350000)]
    -- 0xfffffffe: not final, so the lock time applies, and not replaceable
    it "gives every input of Bitcoin Cash, which replaces no transaction, the highest sequence under which the lock time applies" $ do
      built <- expectRight $ buildWalletTx bch ctx (mkStdGen 0) LargestFirst (cashWallet ctx) firstChange [(otherAddr 0, 350000)] 10 800002 SenderPays NoCoinControl
      ((.sequence) <$> built.tx.inputs) `shouldBe` [0xfffffffe]
      (built.tx.version, built.tx.locktime) `shouldBe` (2, 800002)
    -- Fee: 10 + 148 + 44 (a P2SH32 output: 8 + 1 + 35) + 34 + 34 = 270 bytes * 10 = 2700
    -- Change: 400000 - 100000 - 50000 - 2700 = 247300
    it "pays a P2SH32 address and a token-aware address on Bitcoin Cash, the latter with the plain P2PKH script" $ do
      built <- expectRight $ buildWalletTx bch ctx (mkStdGen 0) LargestFirst (cashWallet ctx) firstChange [(script32Addr, 100000), (tokenAddr, 50000)] 10 800002 SenderPays NoCoinControl
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
      built.inputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 2), legacyPath (Deriv :/ 0 :/ 1)]
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
      let found = wallet (coinsWorth ctx [400000]) [(externalAddr 3, legacyPath (Deriv :/ 0 :/ 3))]
      built <- expectRight $ build ctx LargestFirst found [(externalAddr 3, 100000), (externalAddr 3, 50000)] SenderPays
      spends ctx built `shouldBe` ([400000], [(externalAddr 3, 50000), (externalAddr 3, 100000), (internalAddr 0, 247400)])
      built.outputPaths `shouldBe` [legacyPath (Deriv :/ 1 :/ 0), legacyPath (Deriv :/ 0 :/ 3)]
    it "records the path of an address once for several of its coins" $ do
      -- Two coins on /0/0: 10 + 2 * 148 + 2 * 34 = 374 bytes * 10 = 3740
      -- Change: 200000 + 200000 - 350000 - 3740 = 46260
      let found = wallet (coinsOf ctx $ fundingTx ctx 1 [(externalAddr 0, 200000), (externalAddr 0, 200000)]) []
      built <- expectRight $ build ctx LargestFirst found [(otherAddr 0, 350000)] SenderPays
      spends ctx built `shouldBe` ([200000, 200000], [(internalAddr 0, 46260), (otherAddr 0, 350000)])
      built.inputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 0)]

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
      built.outputPaths `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]
    it "reports, signs and verifies the payment to every type" $ do
      built <- expectRight $ pay ctx [400000] everyType
      let tsd = TxSignData built.tx [fundingWorth ctx [400000]] built.inputPaths built.outputPaths (Just 10) Nothing [] noLabel
      unsignedInfo <- expectRight $ parseTxSignData btc ctx [(Bip44, testAccountPub ctx)] tsd
      (unsignedInfo.txInfoSummary.summarySize, unsignedInfo.txInfoSummary.summaryFeeByte, unsignedInfo.txInfoSummary.summaryChangeInFee) `shouldBe` (375, 10, Just 0)
      (signed, info) <- expectRight $ signWalletTx btc ctx tsd [(Bip44, testAccountKey)]
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
      pay ctx [400000] [(otherAddr 0, 545)]
        `shouldBe` Left "A recipient would get 545 satoshis, less than the dust limit of 546 satoshis for its kind of address. Nodes do not relay an output that small"
    it "pays a recipient exactly the dust limit after its fee" $ do
      -- Fee: 226 bytes * 10 = 2260, paid by the recipient: 2806 - 2260 = 546
      -- Change: 400000 - 2806 = 397194
      built <- expectRight $ recipientsPayFor ctx [400000] [(otherAddr 0, 2806)]
      spends ctx built `shouldBe` ([400000], [(otherAddr 0, 546), (internalAddr 0, 397194)])
      feePaid built `shouldBe` 2260
    it "fails when a recipient is left just below the dust limit after its fee" $
      -- 2805 - 2260 = 545
      recipientsPayFor ctx [400000] [(otherAddr 0, 2805)]
        `shouldBe` Left "A recipient would get 545 satoshis, less than the dust limit of 546 satoshis for its kind of address. Nodes do not relay an output that small"

    -- Each output has the limit nodes relay for its kind: 546 for a P2PKH
    -- output, 294 for a P2WPKH one. The payment of a bip84 coin with
    -- bip84 change takes 144 virtual bytes, a fee of 1440.
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
        `shouldBe` Left "A recipient would get 293 satoshis, less than the dust limit of 294 satoshis for its kind of address. Nodes do not relay an output that small"
    it "refuses a P2PKH recipient paid 545 satoshis" $
      payWithChange ctx (wallet (coinsWorth ctx [400000]) []) firstChange [(otherAddr 0, 545)]
        `shouldBe` Left "A recipient would get 545 satoshis, less than the dust limit of 546 satoshis for its kind of address. Nodes do not relay an output that small"

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
      it "fails when the chosen coins do not cover the amount and the fee, though the portfolio does" $
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
      recipientsPayFor ctx [2000] [(otherAddr 0, 2000)]
        `shouldBe` Left "A recipient would get 80 satoshis, less than the dust limit of 546 satoshis for its kind of address. Nodes do not relay an output that small"
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


{- Coins of both types -}

-- The portfolio has a coin of 30000 at the first receiving address of its
-- bip44 account and one of 20000 at that of its bip84 account. A payment
-- of 40000 needs both. Its sizes, as "Haskoin.Wallet.Tx.Size" gives them:
-- 10 bytes of frame, 148 for the P2PKH input, 41 for the P2WPKH input and
-- 34 for each of the two P2PKH outputs, the recipient's and the change,
-- which are 267 bytes; then the witness data, which counts a quarter: 2
-- bytes of marker and flag, 1 for the empty witness of the P2PKH input and
-- 108 for the witness of the P2WPKH input, 111 in all, 28 once rounded up.
-- The transaction is 295 virtual bytes: the 148 and the 68 of its two
-- inputs taken alone, the 78 of the rest, and 1 for the witness bytes the
-- 68 leave out. At 10 satoshis per virtual byte it costs 2950.
bothTypesSpec :: Ctx -> Spec
bothTypesSpec ctx =
  describe "A payment from coins of both types" $ do
    it "spends a bip44 and a bip84 coin of the portfolio in one transaction, each input sized by its own type" $ do
      -- Change: 30000 + 20000 - 40000 - 2950 = 7050
      built <- expectRight $ build ctx LargestFirst both [(otherAddr 0, 40000)] SenderPays
      spends ctx built `shouldBe` ([30000, 20000], [(internalAddr 0, 7050), (otherAddr 0, 40000)])
      feePaid built `shouldBe` 2950
    it "records the path of each coin with the type of its account" $ do
      built <- expectRight $ build ctx LargestFirst both [(otherAddr 0, 40000)] SenderPays
      built.inputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 0), segwitPath (Deriv :/ 0 :/ 0)]
    it "counts the fee for change of the type of the change address: 3 bytes less for a P2WPKH output" $ do
      -- 292 virtual bytes * 10 = 2920
      -- Change: 30000 + 20000 - 40000 - 2920 = 7080
      built <- expectRight $ payWithChange ctx both segwitChange [(otherAddr 0, 40000)]
      spends ctx built `shouldBe` ([30000, 20000], [(segwitInternalAddr 0, 7080), (otherAddr 0, 40000)])
      built.outputPaths `shouldBe` [segwitPath (Deriv :/ 1 :/ 0)]
    -- The coins are chosen for bip44 change, and the address given is a
    -- bip84 one: the fee would be 30 satoshis too high for it
    it "refuses to assemble a payment at a change address of another type than its fee was counted for" $ do
      chosen <- expectRight $ choosePayment ctx (mkStdGen 0) LargestFirst both [(otherAddr 0, 40000)] 10 SenderPays NoCoinControl Bip44
      (.tx) <$> assemblePayment btc ctx both chosen (Just segwitChange) 800002
        `shouldBe` Left "The change address is not a bip44 address, the type the fee was counted for"
  where
    both = wallet (coinsWorth ctx [30000] <> segwitCoinsWorth ctx [20000]) []

{- With the database -}

-- | Portfolio 0 of the test wallet on Bitcoin, holding a coin of 400000 at
-- the first receiving address of its bip44 account, confirmed at height 0,
-- the best block
fundedPool :: Ctx -> Config -> ExceptT String (DB IO) (Pool, Tx)
fundedPool ctx cfg = fundedAt ctx cfg [400000]

-- | Portfolio 0 of the test wallet on Bitcoin, holding a coin of each
-- value at the receiving addresses of its bip44 account in order, /0/0
-- first, confirmed at height 0, the best block, and paid by 'fundingWorth'
fundedAt :: Ctx -> Config -> [Natural] -> ExceptT String (DB IO) (Pool, Tx)
fundedAt ctx cfg values = do
  pool <- testPool ctx
  (accId, _) <- testNewAcc ctx
  lift $ updateBest btc (fakeBlockHash 0) 0 1700000000
  mapM_ (const $ genExtAddress ctx cfg accId "") values
  let fund = fundingWorth ctx values
      addrs = externalAddr <$> [0 .. length values - 1]
  lift $ insertRawTx fund
  _ <- refreshCoins btc ctx accId (const seenTime) addrs [storeCoin ctx (txHash fund, i) (Just 0) addr value | (i, addr, value) <- zip3 [0 ..] addrs values]
  return (pool, fund)

-- | The same portfolio holding coins of both types: those of 'fundedAt'
-- for the first values, and a coin of each of the second values at the
-- receiving addresses of its bip84 account in order, /0/0 first, confirmed
-- at the best block and paid by 'segwitFundingWorth'
fundedBoth :: Ctx -> Config -> [Natural] -> [Natural] -> ExceptT String (DB IO) Pool
fundedBoth ctx cfg legacyValues segwitValues = do
  (pool, _) <- fundedAt ctx cfg legacyValues
  (segwitId, _) <- testNewSegwitAcc ctx
  mapM_ (const $ genExtAddress ctx cfg segwitId "") segwitValues
  let fund = segwitFundingWorth ctx segwitValues
      addrs = segwitExternalAddr <$> [0 .. length segwitValues - 1]
  lift $ insertRawTx fund
  _ <- refreshCoins btc ctx segwitId (const seenTime) addrs [storeCoin ctx (txHash fund, i) (Just 0) addr value | (i, addr, value) <- zip3 [0 ..] addrs segwitValues]
  return pool

-- | The outpoint of the i-th coin of 'segwitFundingWorth'
segwitCoinAt :: Ctx -> [Natural] -> Word32 -> OutPoint
segwitCoinAt ctx values = OutPoint (txHash $ segwitFundingWorth ctx values)

-- | What the transaction of the signing data pays, by amount, since the
-- builder shuffles its outputs
paidBy :: Ctx -> TxSignData -> [(Address, Word64)]
paidBy ctx tsd = sortOn snd (outputsOf ctx (txSignDataTx tsd))

-- | What the transaction of the signing data spends, as outpoints, in the
-- order of its inputs
spentBy :: TxSignData -> [OutPoint]
spentBy tsd = (.outpoint) <$> (txSignDataTx tsd).inputs

-- | A payment of the amount to otherAddr 0 from the portfolio, by the
-- largest coins first, at 10 satoshis per virtual byte, the sender paying
-- the fee, from coins with one confirmation: with the configuration, the
-- one type of coins it may spend, coin control and where its change goes
paying :: Ctx -> Config -> Pool -> Maybe AccountKind -> Natural -> CoinControl -> ChangeTo -> ExceptT String (DB IO) TxSignData
paying ctx cfg pool coinType amount =
  buildTxSignData ctx cfg (mkStdGen 0) LargestFirst pool coinType [(otherAddr 0, amount)] 10 800002 SenderPays 1

databaseSpec :: Ctx -> Spec
databaseSpec ctx =
  describe "With the database" $ do
    let cfg = def :: Config
    it "fetches the coins with their paths and the wallet recipients" $
      inDatabaseE $ do
        (pool, fund) <- fundedPool ctx cfg
        found <- fetchWalletCoins ctx pool Nothing [otherAddr 0, externalAddr 0] 1
        liftIO $ do
          found.coins `shouldBe` [storeCoin ctx (txHash fund, 0) (Just 0) (externalAddr 0) 400000]
          found.coinPaths `shouldBe` Map.fromList [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0))]
          found.recipientPaths `shouldBe` Map.fromList [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0))]
    it "leaves out the coins without enough confirmations" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        -- The coin at the best block has one confirmation
        found <- fetchWalletCoins ctx pool Nothing [] 2
        liftIO $ found.coins `shouldBe` []
    it "builds the signing data with the transactions spent from" $
      inDatabaseE $ do
        (pool, fund) <- fundedPool ctx cfg
        tsd <- paying ctx cfg pool Nothing 350000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          sortedOutputs (txSignDataTx tsd)
            `shouldBe` builtTx ctx 800002 [(txHash fund, 0)] [(internalAddr 0, 47740), (otherAddr 0, 350000)]
          txSignDataInputs tsd `shouldBe` [fund]
          txSignDataInputPaths tsd `shouldBe` [legacyPath (Deriv :/ 0 :/ 0)]
          txSignDataOutputPaths tsd `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]
          txSignDataFeeRate tsd `shouldBe` Just 10
          isSigned (txSignDataTx tsd) `shouldBe` False
    it "fails on a coin whose raw transaction is missing, naming it and the full sync that fetches it" $
      inDatabaseE $ do
        pool <- testPool ctx
        (accId, _) <- testNewAcc ctx
        lift $ updateBest btc (fakeBlockHash 0) 0 1700000000
        _ <- genExtAddress ctx cfg accId ""
        _ <- refreshCoins btc ctx accId (const seenTime) [externalAddr 0] [storeCoin ctx (fakeTxHash 9, 0) (Just 0) (externalAddr 0) 400000]
        result <- lift $ runExceptT $ paying ctx cfg pool Nothing 350000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $
          result
            `shouldBe` Left "The raw transaction 0000000000000000000000000000000000000000000000000000000000000009 of a coin is missing: sync --full fetches it"
    -- A coin an earlier hw stored as a store reported it: 400000, where its
    -- transaction pays 1 btc. The payment would have left 99600000 of it
    -- to the fee. The txid, e555d280...43de, is worked out apart from hw.
    it "refuses a coin the wallet holds at another value than its transaction pays" $
      inDatabaseE $ do
        pool <- testPool ctx
        (accId, _) <- testNewAcc ctx
        lift $ updateBest btc (fakeBlockHash 0) 0 1700000000
        _ <- genExtAddress ctx cfg accId ""
        let fund = fundingWorth ctx [100000000]
        lift $ insertRawTx fund
        _ <- refreshCoins btc ctx accId (const seenTime) [externalAddr 0] [storeCoin ctx (txHash fund, 0) (Just 0) (externalAddr 0) 400000]
        result <- lift $ runExceptT $ paying ctx cfg pool Nothing 350000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $
          result
            `shouldBe` Left "The store reports the coin e555d2807e912fa1dd1d589e87653f8f55a52724e99e81b2a125cb93cb4743de:0 as worth 400000 satoshis, but its transaction pays 100000000"
    it "fails without recipients" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        result <- lift $ runExceptT $ buildTxSignData ctx cfg (mkStdGen 0) LargestFirst pool Nothing [] 10 800002 SenderPays 1 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ result `shouldBe` Left "No recipients provided"

    -- A coin of 30000 of the bip44 account and one of 20000 of the bip84
    -- account pay 40000 together: the 295 virtual bytes of
    -- "A payment from coins of both types" cost 2950, and 7050 are left
    it "builds a payment from the coins of both accounts of the portfolio, with the transactions of both" $
      inDatabaseE $ do
        pool <- fundedBoth ctx cfg [30000] [20000]
        tsd <- paying ctx cfg pool Nothing 40000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          spentBy tsd `shouldBe` [coinAt ctx [30000] 0, segwitCoinAt ctx [20000] 0]
          paidBy ctx tsd `shouldBe` [(internalAddr 0, 7050), (otherAddr 0, 40000)]
          txSignDataInputs tsd `shouldBe` [fundingWorth ctx [30000], segwitFundingWorth ctx [20000]]
          txSignDataInputPaths tsd `shouldBe` [legacyPath (Deriv :/ 0 :/ 0), segwitPath (Deriv :/ 0 :/ 0)]

{- The coins of one type -}

-- The portfolio holds coins of 300000 and 100000 in its bip44 account, at
-- /0/0 and /0/1, and coins of 200000 and 50000 in its bip84 account, at
-- /0/0 and /0/1. --coin-type keeps the coins of one type.
coinTypeSpec :: Ctx -> Spec
coinTypeSpec ctx =
  describe "The coins of one type (--coin-type)" $ do
    let cfg = def :: Config
        legacyValues = [300000, 100000]
        segwitValues = [200000, 50000]
        funded = fundedBoth ctx cfg legacyValues segwitValues
        valuesOf :: WalletCoins -> [Word64]
        valuesOf found = sort ((.value) <$> found.coins)
    it "are the bip44 coins alone with the type bip44, each with its path" $
      inDatabaseE $ do
        pool <- funded
        found <- fetchWalletCoins ctx pool (Just Bip44) [] 1
        liftIO $ do
          valuesOf found `shouldBe` [100000, 300000]
          found.coinPaths `shouldBe` Map.fromList [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0)), (externalAddr 1, legacyPath (Deriv :/ 0 :/ 1))]
    it "are the bip84 coins alone with the type bip84, each with its path" $
      inDatabaseE $ do
        pool <- funded
        found <- fetchWalletCoins ctx pool (Just Bip84) [] 1
        liftIO $ do
          valuesOf found `shouldBe` [50000, 200000]
          found.coinPaths `shouldBe` Map.fromList [(segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0)), (segwitExternalAddr 1, segwitPath (Deriv :/ 0 :/ 1))]
    it "are the coins of both types without a type" $
      inDatabaseE $ do
        pool <- funded
        found <- fetchWalletCoins ctx pool Nothing [] 1
        liftIO $ valuesOf found `shouldBe` [50000, 100000, 200000, 300000]

    -- The largest coin of the portfolio is the bip44 one of 300000. With
    -- the type bip84 the largest candidate is the coin of 200000, which
    -- pays 150000 alone: 144 virtual bytes with its bip84 change, 1440,
    -- and 200000 - 150000 - 1440 = 48560 of change.
    it "pay with the bip84 coins alone, though a bip44 coin is larger" $
      inDatabaseE $ do
        pool <- funded
        tsd <- paying ctx cfg pool (Just Bip84) 150000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          spentBy tsd `shouldBe` [segwitCoinAt ctx segwitValues 0]
          paidBy ctx tsd `shouldBe` [(segwitInternalAddr 0, 48560), (otherAddr 0, 150000)]

    -- Without a type, 350000 would take the coins of 300000 and 200000.
    -- With the type bip44 it takes the two bip44 coins: 374 bytes, 3740,
    -- and 300000 + 100000 - 350000 - 3740 = 46260 of change.
    it "pay with the bip44 coins alone, though a bip84 coin is larger than one of them" $
      inDatabaseE $ do
        pool <- funded
        tsd <- paying ctx cfg pool (Just Bip44) 350000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          spentBy tsd `shouldBe` [coinAt ctx legacyValues 0, coinAt ctx legacyValues 1]
          paidBy ctx tsd `shouldBe` [(internalAddr 0, 46260), (otherAddr 0, 350000)]

    -- The bip84 coins hold 250000, and the portfolio 650000
    it "do not cover a payment the coins of the other type would be needed for" $
      inDatabaseE $ do
        pool <- funded
        result <- lift $ runExceptT $ paying ctx cfg pool (Just Bip84) 300000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ result `shouldBe` Left "The coins do not cover the amount and the fee"

    -- Both bip84 coins are chosen, and both are spent, though the first
    -- pays alone: 10 + 2 * 41 + 34 + 31 = 157 bytes, then (2 + 2 * 108) /
    -- 4 of witness data, rounded up to 55, are 212 virtual bytes, 2120,
    -- and 200000 + 50000 - 150000 - 2120 = 97880 of change
    it "are all spent when they are the coins chosen (--coin), with the type of the chosen coins" $
      inDatabaseE $ do
        pool <- funded
        let chosen = SpendChosen [segwitCoinAt ctx segwitValues 0, segwitCoinAt ctx segwitValues 1]
        tsd <- paying ctx cfg pool (Just Bip84) 150000 chosen (NewChangeAddress Nothing)
        liftIO $ do
          sort (spentBy tsd) `shouldBe` [segwitCoinAt ctx segwitValues 0, segwitCoinAt ctx segwitValues 1]
          paidBy ctx tsd `shouldBe` [(segwitInternalAddr 0, 97880), (otherAddr 0, 150000)]

    it "are selected among when they are the coins chosen with --select" $
      inDatabaseE $ do
        pool <- funded
        let chosen = SelectAmongChosen [segwitCoinAt ctx segwitValues 0, segwitCoinAt ctx segwitValues 1]
        tsd <- paying ctx cfg pool (Just Bip84) 150000 chosen (NewChangeAddress Nothing)
        liftIO $ do
          spentBy tsd `shouldBe` [segwitCoinAt ctx segwitValues 0]
          paidBy ctx tsd `shouldBe` [(segwitInternalAddr 0, 48560), (otherAddr 0, 150000)]

    -- The command refuses such a coin first, naming it. The builder does
    -- not spend it either: it is not among the coins of the type.
    it "do not include a chosen coin of the other type, which is refused" $
      inDatabaseE $ do
        pool <- funded
        result <- lift $ runExceptT $ paying ctx cfg pool (Just Bip84) 150000 (SpendChosen [coinAt ctx legacyValues 0]) (NewChangeAddress Nothing)
        liftIO $ result `shouldBe` Left "A chosen coin does not exist or is not spendable"

{- The type of the change -}

-- The change of a payment goes to a new change address of the portfolio.
-- Its type is the first that applies: the type the command asks for; the
-- type of the coins the payment spends, when they are all of one type; the
-- default type of the configuration, when they are of both; bip44. A
-- P2PKH change output is 34 bytes and a P2WPKH one 31.
changeTypeSpec :: Ctx -> Spec
changeTypeSpec ctx =
  describe "The type of the change of a payment" $ do
    let cfg = def :: Config
        preferringSegwit = configWith ["default-type" .= ("bip84" :: Text)]
    -- A bip44 coin of 400000 pays 350000: 10 + 148 + 34 + 31 = 223 bytes,
    -- 2230, and 400000 - 350000 - 2230 = 47770 of change
    it "is the type the command asks for, whatever the type of the coins" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        tsd <- paying ctx cfg pool Nothing 350000 NoCoinControl (NewChangeAddress (Just Bip84))
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(segwitInternalAddr 0, 47770), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [segwitPath (Deriv :/ 1 :/ 0)]

    -- A bip84 coin of 400000 pays 350000 with bip84 change: 144 virtual
    -- bytes, 1440, and 48560 of change
    it "is bip84 when the coins of the portfolio are all bip84 coins, though the default type is bip44" $
      inDatabaseE $ do
        pool <- fundedBoth ctx cfg [] [400000]
        tsd <- paying ctx cfg pool Nothing 350000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(segwitInternalAddr 0, 48560), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [segwitPath (Deriv :/ 1 :/ 0)]

    -- A bip44 coin of 400000 pays 350000 with bip44 change: 226 bytes,
    -- 2260, and 47740 of change
    it "is bip44 when the coins of the portfolio are all bip44 coins, though the default type is bip84" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx preferringSegwit
        tsd <- paying ctx preferringSegwit pool Nothing 350000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(internalAddr 0, 47740), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]

    -- The portfolio has a bip44 coin of 20000 and a bip84 coin of 400000,
    -- which pays 350000 alone. The payment spends bip84 coins only, so its
    -- change is bip84, and its fee is that of a P2WPKH change output: 1440
    it "is the type of the coins the payment spends when they are all of one type, though the portfolio has coins of the other type" $
      inDatabaseE $ do
        pool <- fundedBoth ctx cfg [20000] [400000]
        tsd <- paying ctx cfg pool Nothing 350000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          spentBy tsd `shouldBe` [segwitCoinAt ctx [400000] 0]
          paidBy ctx tsd `shouldBe` [(segwitInternalAddr 0, 48560), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [segwitPath (Deriv :/ 1 :/ 0)]

    -- A bip44 coin of 30000 and a bip84 coin of 20000 pay 40000 together.
    -- With a P2WPKH change output: 292 virtual bytes, 2920, and 7080 of
    -- change
    it "is the default type of the configuration when the payment spends coins of both types" $
      inDatabaseE $ do
        pool <- fundedBoth ctx preferringSegwit [30000] [20000]
        tsd <- paying ctx preferringSegwit pool Nothing 40000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(segwitInternalAddr 0, 7080), (otherAddr 0, 40000)]
          txSignDataOutputPaths tsd `shouldBe` [segwitPath (Deriv :/ 1 :/ 0)]

    -- The same payment with a P2PKH change output: 295 virtual bytes,
    -- 2950, and 7050 of change
    it "is bip44 when the payment spends coins of both types and the configuration names no default type" $
      inDatabaseE $ do
        pool <- fundedBoth ctx cfg [30000] [20000]
        tsd <- paying ctx cfg pool Nothing 40000 NoCoinControl (NewChangeAddress Nothing)
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(internalAddr 0, 7050), (otherAddr 0, 40000)]
          txSignDataOutputPaths tsd `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]

    it "takes its new change address in the account of its type alone" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        _ <- paying ctx cfg pool Nothing 350000 NoCoinControl (NewChangeAddress (Just Bip84))
        (legacyId, _) <- testNewAcc ctx
        (segwitId, _) <- testNewSegwitAcc ctx
        (dBAccountInternal <$> getAccountById legacyId) `dbShouldBeE` 0
        (dBAccountInternal <$> getAccountById segwitId) `dbShouldBeE` 1

{- Sending the change back -}

-- | A payment of the amount to otherAddr 0 from the portfolio, by the
-- largest coins first, at 10 satoshis per virtual byte
payBack :: Ctx -> Config -> Pool -> Natural -> ChangeTo -> ExceptT String (DB IO) TxSignData
payBack ctx cfg pool amount = paying ctx cfg pool Nothing amount NoCoinControl

-- | The refusal of an address the change cannot go back to
notOfThePortfolio :: String
notOfThePortfolio = "The change can only go back to an address of the portfolio, on the network of the payment"

changeBackSpec :: Ctx -> Spec
changeBackSpec ctx = do
  describe "The change sent back to an address (--change-back)" $ do
    let cfg = def :: Config
    -- Fee: 10 + 2 * 148 + 2 * 34 = 374 bytes * 10 = 3740
    -- Change: 30000 + 20000 - 40000 - 3740 = 6260
    it "goes to a receiving address of the bip44 account of the portfolio, with its path" $
      inDatabaseE $ do
        (pool, _) <- fundedAt ctx cfg [30000, 20000]
        tsd <- payBack ctx cfg pool 40000 (ChangeBackTo (externalAddr 1))
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(externalAddr 1, 6260), (otherAddr 0, 40000)]
          txSignDataOutputPaths tsd `shouldBe` [legacyPath (Deriv :/ 0 :/ 1)]

    -- The coin of 400000 at /0/0 pays 350000, and the second receiving
    -- address holds no coin. Fee: 226 bytes * 10 = 2260; change: 47740
    it "goes to an address of the portfolio that the payment does not spend from" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        (accId, _) <- testNewAcc ctx
        _ <- genExtAddress ctx cfg accId ""
        tsd <- payBack ctx cfg pool 350000 (ChangeBackTo (externalAddr 1))
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(externalAddr 1, 47740), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [legacyPath (Deriv :/ 0 :/ 1)]

    it "goes to a change address of the bip44 account of the portfolio" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        (accId, _) <- testNewAcc ctx
        extendChainTo ctx accId AddrInternal 1
        tsd <- payBack ctx cfg pool 350000 (ChangeBackTo (internalAddr 0))
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(internalAddr 0, 47740), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]

    -- The change is a P2WPKH output, which the fee is counted for: 10 +
    -- 148 + 34 + 31 = 223 bytes, 2230, and 400000 - 350000 - 2230 = 47770
    it "goes to a receiving address of the bip84 account of the portfolio, the fee counted for its type" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        (segwitId, _) <- testNewSegwitAcc ctx
        _ <- genExtAddress ctx cfg segwitId ""
        tsd <- payBack ctx cfg pool 350000 (ChangeBackTo (segwitExternalAddr 0))
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(segwitExternalAddr 0, 47770), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [segwitPath (Deriv :/ 0 :/ 0)]

    it "goes to a change address of the bip84 account of the portfolio" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        (segwitId, _) <- testNewSegwitAcc ctx
        extendChainTo ctx segwitId AddrInternal 1
        tsd <- payBack ctx cfg pool 350000 (ChangeBackTo (segwitInternalAddr 0))
        liftIO $ do
          paidBy ctx tsd `shouldBe` [(segwitInternalAddr 0, 47770), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [segwitPath (Deriv :/ 1 :/ 0)]

    it "takes no new change address" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        _ <- payBack ctx cfg pool 350000 (ChangeBackTo (externalAddr 0))
        (legacyId, _) <- testNewAcc ctx
        (segwitId, _) <- testNewSegwitAcc ctx
        (dBAccountInternal <$> getAccountById legacyId) `dbShouldBeE` 0
        (dBAccountInternal <$> getAccountById segwitId) `dbShouldBeE` 0

    it "is refused at an address outside the wallet" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        result <- lift $ runExceptT $ payBack ctx cfg pool 350000 (ChangeBackTo (otherAddr 5))
        liftIO $ result `shouldBe` Left notOfThePortfolio

    -- The first receiving address of portfolio 1 of the same wallet
    it "is refused at an address of another portfolio of the wallet" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        other <- nextPortfolioOf ctx testMnemonic
        (otherId, _) <- accountOf other btc Bip44
        _ <- genExtAddress ctx cfg otherId ""
        result <- lift $ runExceptT $ payBack ctx cfg pool 350000 (ChangeBackTo account1Addr)
        liftIO $ result `shouldBe` Left notOfThePortfolio

    it "is refused at an address of a portfolio of another wallet" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        (otherId, _) <- testNewAcc2 ctx
        _ <- genExtAddress ctx cfg otherId ""
        result <- lift $ runExceptT $ payBack ctx cfg pool 350000 (ChangeBackTo externalAddr2)
        liftIO $ result `shouldBe` Left notOfThePortfolio

    -- The first receiving address of the portfolio on Bitcoin Cash, which
    -- its account there has, for a payment on Bitcoin
    it "is refused at an address the portfolio has on another network" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        (cashId, _) <- testNewAccBch ctx
        _ <- genExtAddress ctx cfg cashId ""
        result <- lift $ runExceptT $ payBack ctx cfg pool 350000 (ChangeBackTo bchExternalAddr)
        liftIO $ result `shouldBe` Left notOfThePortfolio

    -- /0/1 of the bip44 account, which the wallet has not handed out: the
    -- database knows the addresses it generated, and no other
    it "is refused at an address the wallet has not generated yet, which it does not know as the portfolio's" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        result <- lift $ runExceptT $ payBack ctx cfg pool 350000 (ChangeBackTo (externalAddr 1))
        liftIO $ result `shouldBe` Left notOfThePortfolio

    -- Fee: 10 + 148 + 34 = 192 bytes * 10 = 1920, and 400000 - 1920 = 398080
    it "pays nothing to the address when the payment has no change" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        tsd <- payBack ctx cfg pool 398080 (ChangeBackTo (externalAddr 0))
        liftIO $ do
          outputsOf ctx (txSignDataTx tsd) `shouldBe` [(otherAddr 0, 398080)]
          txSignDataOutputPaths tsd `shouldBe` []

  describe "The change address of a payment" $ do
    let cfg = def :: Config
    it "is not taken by a payment without change" $
      inDatabaseE $ do
        (pool, _) <- fundedPool ctx cfg
        _ <- payBack ctx cfg pool 398080 (NewChangeAddress Nothing)
        (accId, _) <- testNewAcc ctx
        (dBAccountInternal <$> getAccountById accId) `dbShouldBeE` 0

    it "must be given to assemble a payment with change" $ do
      chosen <- expectRight $ choosePayment ctx (mkStdGen 0) LargestFirst (wallet (coinsWorth ctx [400000]) []) [(otherAddr 0, 350000)] 10 SenderPays NoCoinControl Bip44
      (.tx) <$> assemblePayment btc ctx (wallet (coinsWorth ctx [400000]) []) chosen Nothing 800002
        `shouldBe` Left "The payment has change, and no address for it"

{- Bitcoin Cash -}

-- | Portfolio 0 of the test wallet on Bitcoin Cash, holding a coin of
-- 400000 at the first receiving address of its account there, confirmed
-- at the best block
fundedCash :: Ctx -> Config -> ExceptT String (DB IO) Pool
fundedCash ctx cfg = do
  portfolio <- testPortfolio ctx
  (accId, _) <- testNewAccBch ctx
  lift $ updateBest bch (fakeBlockHash 0) 0 1700000000
  _ <- genExtAddress ctx cfg accId ""
  let fund = fundingTx ctx 1 [(bchExternalAddr, 400000)]
  lift $ insertRawTx fund
  _ <- refreshCoins bch ctx accId (const seenTime) [bchExternalAddr] [storeCoin ctx (txHash fund, 0) (Just 0) bchExternalAddr 400000]
  return (portfolioPool portfolio bch)

-- | The first change address of the test wallet on Bitcoin Cash,
-- m/44'/145'/0'/1/0, written in the legacy form of its P2PKH address. It
-- was derived apart from hw with the Python tool of the message-signing
-- vectors.
cashChangeAddr :: Address
cashChangeAddr = testAddress "1Eb2U8LPm71fMhgL5EF2fg4UktNkuGY6ia"

-- A network of Bitcoin Cash has no SegWit: a portfolio has one account
-- there, a bip44 one, so every coin and the change are bip44. The coin of
-- 400000 pays 350000: 226 bytes, 2260, and 47740 of change.
cashSpec :: Ctx -> Spec
cashSpec ctx =
  describe "The change of a payment on Bitcoin Cash" $ do
    let cfg = def :: Config
        payingCash config pool = buildTxSignData ctx config (mkStdGen 0) LargestFirst pool Nothing [(otherAddr 0, 350000)] 10 800002 SenderPays 1 NoCoinControl
        paidOnCash tsd = sortOn snd (outputsOn bch ctx (txSignDataTx tsd))
    it "is bip44, at a change address of the bip44 account" $
      inDatabaseE $ do
        pool <- fundedCash ctx cfg
        tsd <- payingCash cfg pool (NewChangeAddress Nothing)
        liftIO $ do
          paidOnCash tsd `shouldBe` [(cashChangeAddr, 47740), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]

    it "is bip44 though the default type of the configuration is bip84" $
      inDatabaseE $ do
        let preferringSegwit = configWith ["default-type" .= ("bip84" :: Text)]
        pool <- fundedCash ctx preferringSegwit
        tsd <- payingCash preferringSegwit pool (NewChangeAddress Nothing)
        liftIO $ do
          paidOnCash tsd `shouldBe` [(cashChangeAddr, 47740), (otherAddr 0, 350000)]
          txSignDataOutputPaths tsd `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]

    it "is refused the type bip84, which the network does not have" $
      inDatabaseE $ do
        pool <- fundedCash ctx cfg
        result <- lift $ runExceptT $ payingCash cfg pool (NewChangeAddress (Just Bip84))
        liftIO $ result `shouldBe` Left "A bip84 account needs SegWit, which bch does not have"
