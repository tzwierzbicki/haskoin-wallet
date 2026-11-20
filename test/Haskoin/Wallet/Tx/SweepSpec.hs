{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Tx.SweepSpec where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef)
import Data.Default (def)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.PrettyPrinter (sweepDustReportDoc, unsweptDoc)
import Haskoin.Wallet.Database (DB, Pool)
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Testnet3 (btcTestnet3)
import Haskoin.Wallet.Tx.Dust (DustReport (..), dustQuestion, isDustCoin)
import Haskoin.Wallet.Tx.Recipient (Share (..))
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath, legacyPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo (MyOutputs (..), TxInfo (..), TxSummary (..), TxType (..))
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (noneHeld, parseTxSignData)
import Haskoin.Wallet.Tx.Sweep
import Haskoin.Wallet.TxVectors
import System.Random (mkStdGen)
import Test.Hspec

-- The signing vectors live in JSON files in test/data/tx-sweep (see the
-- README.md file there). Every .json file is loaded, so new vectors can be
-- added without changing this module.
spec :: Spec
spec = do
  -- Counted apart from hw: 20 vectors, as verify_vectors.py finds them
  txVectorSpec "test/data/tx-sweep" "Sweep signing test vectors" [("edge-cases.json", 8), ("external.json", 6), ("hw-regression.json", 4), ("trezor.json", 2)] sweepSigner
  keyAddressesSpec
  sweepSizeSpec
  sweepBuilderSpec
  sweepDustSpec
  givenCoinsSpec
  fetchSpec
  sweepIntoWalletSpec

-- | Sign with the sweep keys and the account public key, like signsweep does
sweepSigner :: VectorSigner
sweepSigner v = do
  specs <- maybe (Left "not a sweep vector: it has no sweep keys") Right v.vecSweepKeys
  accountKey <- prefixError "account" $ accountSigningKey v.vecNetwork v.vecAccount
  keys <- prefixError "sweep key" $ mapM (vectorKey v.vecNetwork) specs
  let accountPub = deriveXPubKey accountKey
  return $ signTxWithKeys v.vecNetwork v.vecSignData [(accountSpecKind v.vecAccount, accountPub)] noneHeld keys

{- Key addresses -}

-- | The swept key of the tests below (secret 0x11), and its addresses on
-- Bitcoin and on Bitcoin testnet, computed independently with the Python
-- primitives of test/data/message-signing/tools/btc.py
sweptKey :: SecKey
sweptKey = (.key) . fromJust $ fromWif btc "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU7Bqcefezr"

sweptKeyAddrsText, sweptKeyTestnetAddrsText :: [Text]
sweptKeyAddrsText =
  [ "18FtYoEJipP5bCJnjgWdeeEbxSvozSjEUA", -- P2PKH, compressed key
    "1LwjY8QPCW46jXn79eeN5SfZDsYNB9qDj1", -- P2PKH, uncompressed key
    "bc1qf7vmha6hqljyhs405efn0hkwj98gz74v86kel2", -- P2WPKH
    "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p" -- P2SH-P2WPKH
  ]
sweptKeyTestnetAddrsText =
  [ "mnmqqrKHXqpLNJnQTFV1UZSvpSXWsS5QQm",
    "n1TgqBVN1XVMWeFisDcjuMst5s95Bo2bMA",
    "tb1qf7vmha6hqljyhs405efn0hkwj98gz74vdud2ye",
    "2N9eX4fbRW1MPUiAang9UMrTDKuYpTN1p4G"
  ]

sweptKeyAddrs :: [Address]
sweptKeyAddrs = fromJust . textToAddr btc <$> sweptKeyAddrsText

-- | The compressed public key of the swept key
sweptPubKey :: PublicKey
sweptPubKey = derivePublicKey (wrapSecKey True sweptKey)

-- | The P2PKH addresses of the compressed and uncompressed encodings of
-- the swept key
sweptKeyCompressedAddr, sweptKeyUncompressedAddr :: Address
sweptKeyCompressedAddr = fromJust $ textToAddr btc "18FtYoEJipP5bCJnjgWdeeEbxSvozSjEUA"
sweptKeyUncompressedAddr = fromJust $ textToAddr btc "1LwjY8QPCW46jXn79eeN5SfZDsYNB9qDj1"

keyAddressesSpec :: Spec
keyAddressesSpec =
  describe "Addresses of a swept key" $ do
    it "are its P2PKH, P2WPKH and P2SH-P2WPKH addresses on Bitcoin" $
      addressesOn btc `shouldBe` sweptKeyAddrsText
    it "use the testnet encodings on Bitcoin testnet" $
      addressesOn btcTestnet3 `shouldBe` sweptKeyTestnetAddrsText
    it "are only the two P2PKH addresses on Bitcoin Cash" $
      keyAddresses bch sweptKey `shouldBe` take 2 sweptKeyAddrs
  where
    addressesOn :: Network -> [Text]
    addressesOn net = fromJust . addrToText net <$> keyAddresses net sweptKey

{- Sweep size -}

-- The coins: one at each of the four Bitcoin addresses of the swept key.
-- The key was given, so the sweep knows which of its two P2PKH addresses
-- is the one of its compressed public key. The sizes are written out
-- from the byte counts in the documentation of the estimator, in virtual
-- bytes: a transaction is 10 bytes of overhead around its inputs and
-- outputs, a P2PKH output is 34 bytes, and the witness data, with the 2
-- marker bytes, counts a quarter, rounded up.
sweepSizeSpec :: Spec
sweepSizeSpec =
  describe "Sweep size" $ do
    it "counts a P2PKH input with a compressed key as 148 bytes" $
      sizeOf [compressedCoin] [otherAddr 0] `shouldBe` Right 192
    it "counts a P2PKH input with an uncompressed key as 180 bytes" $
      sizeOf [uncompressedCoin] [otherAddr 0] `shouldBe` Right 224
    -- The address does not tell how its key is written. The larger size
    -- keeps the fee from coming out below the rate asked: 10 + 180 + 34
    it "counts a P2PKH input of a key that was not given as 180 bytes, the larger of the two it may take" $
      sizeWithoutKeys [compressedCoin] [otherAddr 0] `shouldBe` Right 224
    -- A SegWit key is always compressed: 10 + 41 + 34 = 85, then 28 of
    -- witness data
    it "counts a P2WPKH input of a key that was not given as it does with the key" $
      sizeWithoutKeys [witnessCoin] [otherAddr 0] `shouldBe` Right 113
    it "counts a P2WPKH input as 41 bytes, with a 108-byte witness after the 2 marker bytes, in quarters" $
      -- 10 + 41 + 34 = 85, then (2 + 108) / 4 = 27.5, rounded up to 28
      sizeOf [witnessCoin] [otherAddr 0] `shouldBe` Right 113
    it "counts a P2SH-P2WPKH input as 64 bytes, with the same witness" $
      -- 10 + 64 + 34 = 108, then 28 of witness data
      sizeOf [nestedCoin] [otherAddr 0] `shouldBe` Right 136
    it "counts an empty witness for a legacy input next to a SegWit one" $
      -- 10 + 148 + 41 + 34 = 233, then (2 + 1 + 108) / 4 = 27.75, rounded up to 28
      sizeOf [compressedCoin, witnessCoin] [otherAddr 0] `shouldBe` Right 261
    it "sizes the outputs by their scripts" $
      -- 10 + 148, then P2PKH 34, P2SH 32, P2WPKH 31, P2WSH 43 and P2TR 43
      sizeOf [compressedCoin] [otherAddr 0, scriptAddr, witnessAddr, witnessScriptAddr, taprootAddr] `shouldBe` Right 341
    it "refuses a coin the sweep cannot sign" $
      sizeOf [storeCoin (fakeTxHash 3, 0) Nothing witnessScriptAddr 100000] [otherAddr 0]
        `shouldBe` Left "Unsupported output script for the coin at bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
    -- hw names a coin by its address, and sweeps no coin without one
    it "refuses a coin paid to a public key itself (P2PK), which has no address" $
      sizeOf [p2pkCoin] [otherAddr 0] `shouldBe` Left "Unsupported output script for the coin at unknown address"
  where
    sizeOf swept to =
      sweepTxSize btc (SweepCoins swept [] Map.empty [sweptKeyCompressedAddr]) (fromJust . addressToOutput <$> to)
    -- A sweep prepared from addresses alone
    sizeWithoutKeys swept to =
      sweepTxSize btc (SweepCoins swept [] Map.empty []) (fromJust . addressToOutput <$> to)
    compressedCoin = storeCoin (fakeTxHash 1, 0) Nothing sweptKeyCompressedAddr 100000
    uncompressedCoin = storeCoin (fakeTxHash 1, 1) Nothing sweptKeyUncompressedAddr 200000
    witnessCoin = storeCoin (fakeTxHash 1, 2) Nothing (fromJust $ textToAddr btc "bc1qf7vmha6hqljyhs405efn0hkwj98gz74v86kel2") 300000
    nestedCoin = storeCoin (fakeTxHash 1, 3) Nothing (fromJust $ textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p") 400000
    -- A coin paid to the public key itself, which the store lists under
    -- no address
    p2pkCoin =
      Store.Unspent
        { Store.block = Store.MemRef 0,
          Store.outpoint = OutPoint (fakeTxHash 2) 0,
          Store.value = 100000,
          Store.script = encodeSerial (PayPK sweptPubKey),
          Store.address = Nothing
        }
    -- Destinations of every kind: the P2SH and P2WPKH addresses of the swept
    -- key, and the P2WSH and P2TR examples of BIP173 and BIP350
    scriptAddr = fromJust $ textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p"
    witnessAddr = fromJust $ textToAddr btc "bc1qf7vmha6hqljyhs405efn0hkwj98gz74v86kel2"
    witnessScriptAddr = fromJust $ textToAddr btc "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
    taprootAddr = fromJust $ textToAddr btc "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"

{- Sweep builder -}

-- The scenario: the swept key holds a coin at each of its four Bitcoin
-- addresses, and a later coin, of another transaction, at the address of
-- its compressed public key. The key is given. Everything is swept to an
-- address of another wallet and to the test wallet's second external
-- address. No coin is the wallet's: a sweep takes none.
sweepBuilderSpec :: Spec
sweepBuilderSpec =
  describe "Sweep builder" $ do
    it "spends every coin once and pays every destination" $ do
      tsd <- expectRight $ build destinations
      spentBy tsd `shouldMatchList` ((.outpoint) <$> allCoins)
      length tsd.txSignDataTx.outputs `shouldBe` length destinations
    it "splits what is left after the fee evenly" $ do
      tsd <- expectRight $ build destinations
      ((.value) <$> tsd.txSignDataTx.outputs) `shouldMatchList` [746425, 746425]
    it "records the transactions its coins come from, the path of the wallet's destination and the fee rate" $ do
      tsd <- expectRight $ build destinations
      tsd.txSignDataInputs `shouldBe` [keyFunding, laterFunding]
      tsd.txSignDataOutputPaths `shouldBe` [secondExternal]
      tsd.txSignDataFeeRate `shouldBe` Just 10
    -- The coins are of keys from outside the wallet
    it "records no path for a coin" $ do
      tsd <- expectRight $ build destinations
      tsd.txSignDataInputPaths `shouldBe` []
    it "builds a transaction the sweep signer accepts" $ do
      tsd <- expectRight $ build destinations
      (signed, info) <- expectRight $ signTxWithKeys btc tsd [(Bip44, accountPub)] noneHeld [sweptKey]
      isSigned signed.txSignDataTx `shouldBe` True
      -- The wallet receives one of the two outputs, and spends no coin
      info.txInfoSummary.summaryType `shouldBe` TxCredit
      info.txInfoSummary.summaryAmount `shouldBe` 746425
    it "estimates the size of the signed transaction part by part, every signature counted as 72 bytes" $ do
      tsd <- expectRight $ build destinations
      (signed, _) <- expectRight $ signTxWithKeys btc tsd [(Bip44, accountPub)] noneHeld [sweptKey]
      let tx = signed.txSignDataTx
          legacy = tx {witness = []}
      -- The estimate is 659 bytes outside the witnesses and 221 of
      -- witness data, every signature counted as 72 bytes, 715 virtual
      -- bytes in all. A signature is 72 bytes or 71, as its values fall,
      -- and three of these five are 71: signed, the transaction is 877
      -- bytes, 657 outside the witnesses and 220 of witness data. The
      -- inputs, in the order the builder shuffled them, are the P2WPKH
      -- coin (empty script, a witness of 71 + 33 + 2 with its two length
      -- bytes), the uncompressed P2PKH one (a script of 1 + 72 + 1 + 65),
      -- the P2SH-P2WPKH one (a script of 23, a witness of 72 + 33 + 2) and
      -- the two compressed P2PKH ones (1 + 71 + 1 + 33 each).
      ( BS.length (S.encode tx),
        BS.length (S.encode legacy),
        BS.length . (.script) <$> tx.inputs,
        (\stack -> length stack + sum (BS.length <$> stack)) <$> tx.witness
        )
        `shouldBe` (877, 657, [0, 139, 23, 106, 106], [106, 0, 107, 0, 0])
    -- Outside the wallet, the output that the wallet is paid is another
    -- wallet's too; the fee is the 7150 of the 715 virtual bytes
    it "is signed without an account when it is outside the wallet, every input and output another wallet's" $ do
      tsd <- expectRight $ build destinations
      (signed, info) <- expectRight $ signTxWithKeys btc tsd [] noneHeld [sweptKey]
      isSigned signed.txSignDataTx `shouldBe` True
      info.txInfoSummary.summaryFee `shouldBe` 7150
      (Map.null info.txInfoSummary.summaryMyInputs, Map.null info.txInfoSummary.summaryMyOutputs) `shouldBe` (True, True)
    -- The signer needs the transactions of the coins it spends, and no other
    it "leaves out the transaction of a coin it does not sweep" $ do
      tsd <- expectRight $ buildSweepSignData btc seed (sweepingOnly (coinsOf keyFunding) found) destinations 10 800002
      spentBy tsd `shouldMatchList` ((.outpoint) <$> coinsOf keyFunding)
      tsd.txSignDataInputs `shouldBe` [keyFunding]
    it "refuses to sweep without coins" $
      buildSweepSignData btc seed (SweepCoins [] [] Map.empty []) destinations 10 800002
        `shouldBe` Left "There are no coins to sweep in those addresses"
    it "refuses to sweep without destinations" $
      build [] `shouldBe` Left "No addresses to sweep to"
    it "is a version 2 transaction with the lock time given, its inputs marked replaceable on Bitcoin" $ do
      tsd <- expectRight $ build destinations
      (tsd.txSignDataTx.version, tsd.txSignDataTx.locktime) `shouldBe` (2, 800002)
      ((.sequence) <$> tsd.txSignDataTx.inputs) `shouldBe` replicate 5 0xfffffffd
    -- A coin at the first receiving address of cash, swept to another
    -- address of the wallet on Bitcoin Cash. 0xfffffffe: not final, so the
    -- lock time applies, and not replaceable.
    it "gives every input of Bitcoin Cash, which replaces no transaction, the highest sequence under which the lock time applies" $ do
      let cashCoin = storeCoin (fakeTxHash 8, 0) (Just 800000) bchExternalAddr 50000
      tsd <- expectRight $ buildSweepSignData bch seed (SweepCoins [cashCoin] [] Map.empty []) [(bchExternalAddr, Remainder)] 10 800002
      ((.sequence) <$> tsd.txSignDataTx.inputs) `shouldBe` [0xfffffffe]
    it "builds a sweep to addresses outside the wallet, P2PKH and Taproot, which the signer accepts" $ do
      -- 10 bytes of overhead, the inputs (581), a P2PKH output (34), a
      -- Taproot output (43) and the witness data (221) in quarters (56):
      -- 724 virtual bytes, a fee of 7240, and (1500000 - 7240) / 2 =
      -- 746380 for each destination
      tsd <- expectRight $ build [(otherAddr 0, Remainder), (taprootAddr, Remainder)]
      ((.value) <$> tsd.txSignDataTx.outputs) `shouldBe` [746380, 746380]
      tsd.txSignDataOutputPaths `shouldBe` []
      (_, info) <- expectRight $ signTxWithKeys btc tsd [(Bip44, accountPub)] noneHeld [sweptKey]
      info.txInfoSummary.summaryFee `shouldBe` 7240
      Map.toList info.txInfoSummary.summaryOtherOutputs `shouldBe` [(otherAddr 0, 746380), (taprootAddr, 746380)]
      -- A sweep has no change: nothing of the fee is change
      info.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
    -- Everything to the first receiving address of the bip84 account of
    -- the portfolio, a P2WPKH output of 31 bytes: 10 + 581 + 31 + 56 = 678
    -- virtual bytes, a fee of 6780, and 1500000 - 6780 = 1493220
    it "records the path of a destination of the bip84 account of the portfolio with its type, which the signer reads as the portfolio's" $ do
      let paying = found {destinationPaths = Map.fromList [(segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0))]}
      tsd <- expectRight $ buildSweepSignData btc seed paying [(segwitExternalAddr 0, Remainder)] 10 800002
      tsd.txSignDataOutputPaths `shouldBe` [segwitPath (Deriv :/ 0 :/ 0)]
      (_, info) <- expectRight $ signTxWithKeys btc tsd testPoolPubKeys noneHeld [sweptKey]
      info.txInfoSummary.summaryMyOutputs `shouldBe` Map.fromList [(segwitExternalAddr 0, MyOutputs 1493220 (segwitPath (Deriv :/ 0 :/ 0)) "")]
    -- The same 715 virtual bytes and fee of 7150: 1500000 - 500000 - 7150 = 992850
    it "pays a destination its fixed amount, the fee paid by the destination without one" $ do
      tsd <- expectRight $ build [(otherAddr 0, Fixed 500000), (externalAddr 1, Remainder)]
      paidBy tsd `shouldBe` [(otherAddr 0, 500000), (externalAddr 1, 992850)]
    it "refuses a sweep whose coins do not cover the fixed amounts and the fee, cutting none" $
      build [(otherAddr 0, Fixed 1500000), (externalAddr 1, Remainder)]
        `shouldBe` Left "The coins, 1500000 satoshis, do not cover the fixed amounts, 1500000, and the fee, 7150"
    it "refuses destinations that all have an amount, leaving no one what remains" $
      build [(otherAddr 0, Fixed 500000), (externalAddr 1, Fixed 500000)]
        `shouldBe` Left "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"
    it "is reported before signing with the size its fee was set on, at the rate asked" $ do
      -- One coin belongs to an uncompressed key, which the builder knew:
      -- 715 virtual bytes, and 7150 / 715 = 10
      tsd <- expectRight $ build destinations
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] tsd
      (info.txInfoSummary.summarySize, info.txInfoSummary.summaryFee, feeRateText info.txInfoSummary, info.txInfoSummary.summaryChangeInFee) `shouldBe` (715, 7150, "10", Nothing)
    -- Without the keys, the three P2PKH coins count 180 bytes each. Two of
    -- them counted 148 with the key: 2 × 32 = 64 bytes more than the 715,
    -- so 779 virtual bytes, a fee of 7790, and (1500000 - 7790) / 2 =
    -- 746105 for each destination
    it "sets the fee of a sweep prepared without the keys on the larger size of each P2PKH coin" $ do
      tsd <- expectRight $ buildWithoutKeys destinations
      ((.value) <$> tsd.txSignDataTx.outputs) `shouldMatchList` [746105, 746105]
    it "is reported before signing at the rate asked when it was prepared without the keys" $ do
      -- 7790 / 779 = 10
      tsd <- expectRight $ buildWithoutKeys destinations
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] tsd
      (info.txInfoSummary.summarySize, info.txInfoSummary.summaryFee, feeRateText info.txInfoSummary) `shouldBe` (779, 7790, "10")
    -- Two of the three coins counted 180 bytes have a compressed key:
    -- signed, the transaction is at most the 715 virtual bytes of the
    -- estimate with every key known, and 712 here, where some signatures
    -- are a byte shorter. So the fee of 7790 pays more than the 10 asked:
    -- 7790 / 712 = 10.94
    it "pays no less than the rate asked once signed, when it was prepared without the keys" $ do
      tsd <- expectRight $ buildWithoutKeys destinations
      (_, info) <- expectRight $ signTxWithKeys btc tsd [(Bip44, accountPub)] noneHeld [sweptKey]
      (info.txInfoSummary.summarySize, info.txInfoSummary.summaryFee, feeRateText info.txInfoSummary) `shouldBe` (712, 7790, "10.9")
    -- The coin of 100000 at the address of the compressed public key of
    -- the swept key, alone, to one P2PKH output: 10 + 148 + 34 = 192
    -- bytes with a compressed key, 224 with an uncompressed one
    it "is reported before signing with the larger size of a P2PKH coin when its fee is the rate on that size" $ do
      -- 100000 - 97760 = 2240, and 2240 / 10 = 224
      info <- expectRight $ parseTxSignData btc [] (oneCoinSweep 97760 (Just 10))
      (info.txInfoSummary.summarySize, info.txInfoSummary.summaryFee, feeRateText info.txInfoSummary) `shouldBe` (224, 2240, "10")
    it "is reported before signing with the smaller size of a P2PKH coin when its fee is the rate on that size" $ do
      -- 100000 - 98080 = 1920, and 1920 / 10 = 192
      info <- expectRight $ parseTxSignData btc [] (oneCoinSweep 98080 (Just 10))
      (info.txInfoSummary.summarySize, info.txInfoSummary.summaryFee, feeRateText info.txInfoSummary) `shouldBe` (192, 1920, "10")
    -- A file of another wallet gives no rate, and nothing tells how the
    -- key is written: most keys are compressed. 2240 / 192 = 11.66
    it "is reported before signing with the smaller size of a P2PKH coin when the file gives no fee rate" $ do
      info <- expectRight $ parseTxSignData btc [] (oneCoinSweep 97760 Nothing)
      (info.txInfoSummary.summarySize, info.txInfoSummary.summaryFee, feeRateText info.txInfoSummary) `shouldBe` (192, 2240, "11.6")
    it "refuses a fee larger than the coins" $
      buildWith 10000 destinations `shouldBe` Left "The coins, 1500000 satoshis, do not cover the fee, 7150000"
    -- The fee of 7150 leaves 1500000 - 7150 = 1492850 for the destinations,
    -- two P2PKH outputs whose limit is 546
    it "accepts a destination left exactly the dust limit" $ do
      -- 1492850 - 1492304 = 546
      tsd <- expectRight $ build [(otherAddr 0, Fixed 1492304), (externalAddr 1, Remainder)]
      paidBy tsd `shouldBe` [(externalAddr 1, 546), (otherAddr 0, 1492304)]
    it "refuses a destination left just below the dust limit" $
      -- 1492850 - 1492305 = 545
      build [(otherAddr 0, Fixed 1492305), (externalAddr 1, Remainder)]
        `shouldBe` Left "What remains for each address without an amount, 545 satoshis, is below the dust limit of 546"
    it "refuses a fee rate of zero" $
      buildWith 0 destinations `shouldBe` Left "The fee rate must be at least 1 satoshi per virtual byte"
    -- A rate of 2^64 wrapped around to zero, and others to any fee
    it "refuses a fee rate above 2^32 - 1 satoshis per virtual byte" $
      buildWith 4294967296 destinations `shouldBe` Left "The fee rate must be at most 4294967295 satoshis per virtual byte"
    -- 715 virtual bytes at 4294967295 satoshis each, far more than the
    -- coins, and far less than 2^64
    it "counts the fee at the highest rate without wrapping around" $
      buildWith 4294967295 destinations `shouldBe` Left "The coins, 1500000 satoshis, do not cover the fee, 3070901615925"
    it "refuses to sign a sweep without fee" $ do
      -- The later coin of 500000 is swept in full
      let tx = spendingTx [(txHash laterFunding, 0)] [(otherAddr 0, 500000)]
          unsigned = TxSignData tx [laterFunding] [] [] Nothing Nothing [] noLabel
      signTxWithKeys btc unsigned [] noneHeld [sweptKey] `shouldBe` Left "The transaction pays no fee"
    -- The coin is paid to the swept key's public key itself. haskoin-core
    -- gives such a coin no address, since the P2PKH address of the key
    -- would mislead, and hw names a coin by its address.
    it "refuses to sign a sweep of a coin paid to a public key itself (P2PK), which has no address" $ do
      let tx = spendingTx [(txHash p2pkFunding, 0)] [(otherAddr 0, 98420)]
          unsigned = TxSignData tx [p2pkFunding] [] [] (Just 10) Nothing [] noLabel
      signTxWithKeys btc unsigned [] noneHeld [sweptKey] `shouldBe` Left "There are non-standard inputs in the transaction"
    -- A SegWit coin is spent with a compressed public key: a block may
    -- hold a spend with the other encoding, but nodes do not relay one.
    -- The coin here pays the hash of the swept key's uncompressed public
    -- key, at the P2WPKH address computed apart from hw with the Python
    -- primitives of test/data/message-signing/tools/btc.py.
    it "refuses to sign a SegWit coin of the uncompressed public key of a key given, which nodes would not relay" $ do
      let tx = spendingTx [(txHash uncompressedWitnessFunding, 0)] [(otherAddr 0, 98000)]
          unsigned = TxSignData tx [uncompressedWitnessFunding] [] [] (Just 10) Nothing [] noLabel
      signTxWithKeys btc unsigned [] noneHeld [sweptKey]
        `shouldBe` Left "The coin of input 0 (bc1qmtz9cyhf5zqaehgsd3apj2p9692zunjr89xa83) is a SegWit coin of an uncompressed public key: nodes do not relay a spend of it"
  where
    seed = mkStdGen 0
    -- The wallet: the test wallet of TestUtils, paid at its second external address
    accountPub = testAccountPub
    secondExternal = legacyPath (Deriv :/ 0 :/ 1)
    -- The coins: 100000 to 400000 at the four addresses of the swept key,
    -- and a later one of 500000 at the address of its compressed public key
    keyFunding = fundingTx 1 (zip sweptKeyAddrs [100000, 200000, 300000, 400000])
    laterFunding = fundingTx 2 [(sweptKeyCompressedAddr, 500000)]
    -- A transaction paying 100000 to the swept key's public key itself
    -- (P2PK), as the coinbase transactions of the first years did
    p2pkFunding = Tx 2 [TxIn (OutPoint (fakeTxHash 3) 0) BS.empty 0xffffffff] [TxOut 100000 (encodeSerial (PayPK sweptPubKey))] [] 0
    -- A transaction paying 100000 to the P2WPKH address of the swept key's
    -- uncompressed public key
    uncompressedWitnessFunding = fundingTx 4 [(fromJust (textToAddr btc "bc1qmtz9cyhf5zqaehgsd3apj2p9692zunjr89xa83"), 100000)]
    allCoins :: [Store.Unspent]
    allCoins = coinsOf keyFunding <> coinsOf laterFunding
    found =
      SweepCoins
        { coins = allCoins,
          transactions = [keyFunding, laterFunding],
          destinationPaths = Map.fromList [(externalAddr 1, secondExternal)],
          compressedAddrs = [sweptKeyCompressedAddr]
        }
    -- The sweep: 10 satoshis per virtual byte. The estimated size is 10 bytes of overhead, the inputs (148 + 180 + 41 + 64 + 148 =
    -- 581), two P2PKH outputs (68) and the witness data (2 + 1 + 1 + 108 +
    -- 108 + 1 = 221) in quarters, rounded up (56): 715 virtual bytes, so the
    -- fee is 7150 and each destination gets (1500000 - 7150) / 2 = 746425.
    destinations = [(otherAddr 0, Remainder), (externalAddr 1, Remainder)]
    -- The Taproot example address of BIP350
    taprootAddr = fromJust $ textToAddr btc "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
    build = buildWith 10
    buildWith feeByte to = buildSweepSignData btc seed found to feeByte 800002
    -- A sweep of the first coin of the swept key alone, which pays the
    -- amount to another wallet, with the fee rate its file gives
    oneCoinSweep paid rate = TxSignData (spendingTx [(txHash keyFunding, 0)] [(otherAddr 0, paid)]) [keyFunding] [] [] rate Nothing [] noLabel
    -- The same sweep prepared from the addresses alone
    buildWithoutKeys to = buildSweepSignData btc seed found {compressedAddrs = []} to 10 800002
    spentBy :: TxSignData -> [OutPoint]
    spentBy tsd = (.outpoint) <$> tsd.txSignDataTx.inputs
    -- The outputs of the sweep by address, the smallest first, since the
    -- builder shuffles them
    paidBy :: TxSignData -> [(Address, Word64)]
    paidBy tsd = sortOn snd [(forceRight (scriptToAddressBS btc out.script), out.value) | out <- tsd.txSignDataTx.outputs]

{- Dust coins -}

-- An old wallet holds 30000 at one legacy address, and 900 and 600 at
-- another. The sweep pays one legacy address at 10 satoshis a byte. No key
-- was given unless a test says so, and a legacy coin then counts for the
-- 180 bytes of an uncompressed key: 1800 satoshis. With the two small
-- coins, the sweep is 10 + 3 × 180 + 34 = 584 bytes, a fee of 5840, and
-- 31500 - 5840 = 25660 for the destination. Without them, 10 + 180 + 34 =
-- 224 bytes, a fee of 2240, and 30000 - 2240 = 27760.
sweepDustSpec :: Spec
sweepDustSpec =
  describe "The dust coins of a sweep" $ do
    it "are the legacy coins worth no more than the fee of 180 bytes, when their key was not given" $
      (isDustCoin 10 <$>) <$> sizedSweepCoins btc (withoutKeys [legacyCoin 0 1800, legacyCoin 1 1801])
        `shouldBe` Right [True, False]

    -- The coins are at the address of the compressed key, which was given:
    -- their input takes 148 bytes
    it "are the legacy coins worth no more than the fee of 148 bytes, when their compressed key was given" $
      (isDustCoin 10 <$>) <$> sizedSweepCoins btc (SweepCoins [compressedCoin 0 1480, compressedCoin 1 1481] [] Map.empty [sweptKeyCompressedAddr])
        `shouldBe` Right [True, False]

    it "add their inputs to the fee, and lower what the destination gets" $ do
      report <- expectRight smallCoinsReport
      (report.addedFee, report.withDust, report.withoutDust) `shouldBe` (3600, 25660, 27760)

    -- Alone, the two small coins to one legacy address are 10 + 360 + 34 =
    -- 404 bytes. They must leave it 546: (1500 - 546) / 404 = 2 satoshis a
    -- byte.
    it "can be swept later, alone, up to the rate that leaves the dust limit to an address of the type of the destination" $ do
      report <- expectRight smallCoinsReport
      (report.dustLimit, report.laterRate) `shouldBe` (546, Just 2)

    -- To a native SegWit address, they are 10 + 360 + 31 = 401 bytes and
    -- must leave 294: (1500 - 294) / 401 = 3 satoshis a byte
    it "can be swept later at a higher rate when the destination is a native SegWit address" $ do
      report <- expectRight (reportTo (segwitExternalAddr 0))
      (report.dustLimit, report.laterRate) `shouldBe` (294, Just 3)

    -- The coins are outputs of transaction 2. A sweep names a coin by its
    -- txid in full, as --coin takes it: the wallet does not hold the coin.
    -- The coin of 900 is worth its 180 bytes up to 899 / 180 = 4 satoshis a
    -- byte, and the coin of 600 up to 599 / 180 = 3.
    it "are listed before the question, the largest first, each by its coin, its address and its amount, then what sweeping and leaving them does" $ do
      report <- expectRight smallCoinsReport
      renderedLines (sweepDustReportDoc btc UnitBitcoin report)
        `shouldBe` [ "2 coins cost more to spend than they are worth at 10 sat/vB, 0.00001800 btc each:",
                     "",
                     "0000000000000000000000000000000000000000000000000000000000000002:1",
                     "  at 1LwjY8QPCW46jXn79eeN5SfZDsYNB9qDj1  0.00000900 btc",
                     "  worth spending at 4 sat/vB or less",
                     "",
                     "0000000000000000000000000000000000000000000000000000000000000002:0",
                     "  at 1LwjY8QPCW46jXn79eeN5SfZDsYNB9qDj1  0.00000600 btc",
                     "  worth spending at 3 sat/vB or less",
                     "",
                     "Spending them adds 0.00003600 btc of fee for 0.00001500 btc of coins. What remains for the addresses without an amount is 0.00025660 btc instead of 0.00027760 btc, 0.00002100 btc less.",
                     "Leaving them keeps 0.00001500 btc at their addresses. Once the other coins are gone, they can only be spent together, at 2 sat/vB or less."
                   ]

    it "are asked about as those of a payment of everything" $ do
      report <- expectRight smallCoinsReport
      dustQuestion report `shouldBe` "Spend them too?"

    -- Alone, the coin of 600 to one legacy address is 224 bytes: at 1
    -- satoshi a byte it leaves 376, below the 546 an output must hold
    it "are told when they can never be swept alone" $ do
      sized <- expectRight $ sizedSweepCoins btc (withoutKeys [bigCoin, legacyCoin 0 600])
      let report = sweepDustReport 10 [(otherAddr 0, Remainder)] (take 1 sized) (drop 1 sized)
      last (renderedLines (sweepDustReportDoc btc UnitSatoshi report))
        `shouldBe` "Leaving it keeps 600 sats at its address. Once the other coins are gone, it can never be spent: any fee would leave less than 546 sats."

    it "are named below the sweep that leaves them, each by its coin, its address and its amount" $
      renderedLines (unsweptDoc btc UnitBitcoin [UnsweptCoin (OutPoint (fakeTxHash 2) 1) sweptKeyUncompressedAddr 900, UnsweptCoin (OutPoint (fakeTxHash 2) 0) sweptKeyUncompressedAddr 600])
        `shouldBe` [ "2 coins are worth less than their fee and are not in the sweep:",
                     "  0000000000000000000000000000000000000000000000000000000000000002:1",
                     "    at 1LwjY8QPCW46jXn79eeN5SfZDsYNB9qDj1  0.00000900 btc",
                     "  0000000000000000000000000000000000000000000000000000000000000002:0",
                     "    at 1LwjY8QPCW46jXn79eeN5SfZDsYNB9qDj1  0.00000600 btc"
                   ]

    it "are named in the singular when one coin is left" $
      take 1 (renderedLines (unsweptDoc btc UnitBitcoin [UnsweptCoin (OutPoint (fakeTxHash 2) 0) sweptKeyUncompressedAddr 600]))
        `shouldBe` ["1 coin is worth less than its fee and is not in the sweep:"]
  where
    -- The coin of 30000, at the legacy address of the compressed key
    bigCoin = storeCoin (fakeTxHash 1, 0) (Just 800000) sweptKeyCompressedAddr 30000
    -- A coin of transaction 3 at the legacy address of the compressed key
    compressedCoin i = storeCoin (fakeTxHash 3, i) (Just 800000) sweptKeyCompressedAddr
    -- A coin of transaction 2 at the legacy address of the uncompressed key
    legacyCoin i = storeCoin (fakeTxHash 2, i) (Just 800000) sweptKeyUncompressedAddr
    -- A sweep prepared from addresses alone
    withoutKeys swept = SweepCoins swept [] Map.empty []
    -- The report on the coins of 600 and 900 beside the coin of 30000, all
    -- swept to the address
    reportTo addr = do
      sized <- sizedSweepCoins btc (withoutKeys [bigCoin, legacyCoin 0 600, legacyCoin 1 900])
      return $ sweepDustReport 10 [(addr, Remainder)] (take 1 sized) (drop 1 sized)
    smallCoinsReport = reportTo (otherAddr 0)

{- Coins given by outpoint -}

-- The store's view of the transaction funding the swept key's four
-- addresses, first in block 800000, with its third output spent
givenCoinsSpec :: Spec
givenCoinsSpec =
  describe "The coins of a sweep given by outpoint" $ do
    it "reads them from their transactions, in the order given" $
      coinsOfTransactions [OutPoint (txHash keyFunding) 1, OutPoint (txHash keyFunding) 0] [storeFunding]
        `shouldBe` Right
          [ storeCoin (txHash keyFunding, 1) (Just 800000) (sweptKeyAddrs !! 1) 200000,
            storeCoin (txHash keyFunding, 0) (Just 800000) (sweptKeyAddrs !! 0) 100000
          ]
    it "refuses a coin of a transaction the store does not know" $
      coinsOfTransactions [OutPoint (fakeTxHash 7) 1] [storeFunding]
        `shouldBe` Left "Coin 0000000000000000000000000000000000000000000000000000000000000007:1 is unknown to the store"
    it "refuses a coin of a transaction the store reports as deleted" $
      coinsOfTransactions [OutPoint (txHash keyFunding) 0] [replaced storeFunding]
        `shouldBe` Left ("Coin " <> fundingHex <> ":0 is unknown to the store")
    it "refuses an output index past the last output" $
      coinsOfTransactions [OutPoint (txHash keyFunding) 4] [storeFunding]
        `shouldBe` Left ("Coin " <> fundingHex <> ":4 does not exist: the transaction has 4 outputs")
    it "refuses a spent output" $
      coinsOfTransactions [OutPoint (txHash keyFunding) 2] [storeFunding]
        `shouldBe` Left ("Coin " <> fundingHex <> ":2 is already spent")
  where
    keyFunding = fundingTx 1 (zip sweptKeyAddrs [100000, 200000, 300000, 400000])
    fundingHex = cs $ txHashToHex $ txHash keyFunding
    storeFunding =
      Store.Transaction
        { Store.block = Store.BlockRef 800000 0,
          Store.version = 1,
          Store.locktime = 0,
          Store.inputs = [],
          Store.outputs =
            [ Store.StoreOutput 100000 (addressScript (sweptKeyAddrs !! 0)) Nothing (Just (sweptKeyAddrs !! 0)),
              Store.StoreOutput 200000 (addressScript (sweptKeyAddrs !! 1)) Nothing (Just (sweptKeyAddrs !! 1)),
              Store.StoreOutput 300000 (addressScript (sweptKeyAddrs !! 2)) (Just (Store.Spender (fakeTxHash 9) 0)) (Just (sweptKeyAddrs !! 2)),
              Store.StoreOutput 400000 (addressScript (sweptKeyAddrs !! 3)) Nothing (Just (sweptKeyAddrs !! 3))
            ],
          Store.deleted = False,
          Store.rbf = False,
          Store.timestamp = 1700000000,
          Store.txid = txHash keyFunding,
          Store.size = 226,
          Store.weight = 904,
          Store.fee = 1000
        }

{- Fetching the coins -}

-- The store: the swept key holds a coin at each of its four Bitcoin
-- addresses, 100000 to 400000. The store knows their funding transaction,
-- raw and as it reports it.
fetchSpec :: Spec
fetchSpec =
  describe "Fetching the coins of a sweep" $ do
    it "fetches the coins at the given addresses and the transactions they come from" $
      inDatabaseE $ do
        pool <- walletPool
        found <- fetchSweepCoins btc (toStore store) (Just pool) [] (take 2 sweptKeyAddrs) [] [otherAddr 0]
        liftIO $ summary found `shouldBe` ([(0, 100000), (1, 200000)], [keyFunding], Map.empty, [])
    it "asks the store for every address of a given key and notes the address of its compressed public key" $
      inDatabaseE $ do
        pool <- walletPool
        asked <- liftIO $ newIORef []
        found <- fetchSweepCoins btc (notingUnspentRequests asked (toStore store)) (Just pool) [sweptKey] [] [] [otherAddr 0]
        liftIO $ do
          readIORef asked `shouldReturn` [sweptKeyAddrs]
          summary found `shouldBe` ([(0, 100000), (1, 200000), (2, 300000), (3, 400000)], [keyFunding], Map.empty, [sweptKeyCompressedAddr])
    it "takes the coins given by outpoint from their transactions, each coin once" $
      inDatabaseE $ do
        pool <- walletPool
        found <- fetchSweepCoins btc (toStore store) (Just pool) [] [sweptKeyAddrs !! 0] [OutPoint (txHash keyFunding) 0, OutPoint (txHash keyFunding) 3] [otherAddr 0]
        liftIO $ summary found `shouldBe` ([(0, 100000), (3, 400000)], [keyFunding], Map.empty, [])
    it "records the path of a destination of the portfolio, and none for a destination outside the wallet" $
      inDatabaseE $ do
        pool <- walletPool
        found <- fetchSweepCoins btc (toStore store) (Just pool) [] [sweptKeyAddrs !! 0] [] [externalAddr 1, otherAddr 0]
        liftIO $ found.destinationPaths `shouldBe` Map.fromList [(externalAddr 1, legacyPath (Deriv :/ 0 :/ 1))]
    -- The one coin found is at 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    it "names the addresses given that hold none of the coins found, each once, in the order given" $ do
      let found = SweepCoins [storeCoin (fakeTxHash 8, 0) (Just 800000) (otherAddr 0) 50000] [] Map.empty []
      addressesWithoutCoin [otherAddr 1, otherAddr 0, otherAddr 2, otherAddr 1] found `shouldBe` [otherAddr 1, otherAddr 2]

    -- The coins of the swept key, to the first receiving address of the
    -- bip84 account of the portfolio
    it "records the path of a destination of the bip84 account of the portfolio with its type" $
      inDatabaseE $ do
        pool <- walletPool
        found <- fetchSweepCoins btc (toStore store) (Just pool) [] [sweptKeyAddrs !! 0] [] [segwitExternalAddr 0]
        liftIO $ found.destinationPaths `shouldBe` Map.fromList [(segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0))]
    it "records the paths of destinations of both accounts of the portfolio, each with its type" $
      inDatabaseE $ do
        pool <- walletPool
        found <- fetchSweepCoins btc (toStore store) (Just pool) [] [sweptKeyAddrs !! 0] [] [externalAddr 1, segwitExternalAddr 0]
        liftIO $
          found.destinationPaths
            `shouldBe` Map.fromList [(externalAddr 1, legacyPath (Deriv :/ 0 :/ 1)), (segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0))]
    -- The second test wallet holds the first of the two destinations. The
    -- sweep pays portfolio 0 of the first wallet, so the path of the
    -- second destination alone is recorded.
    it "records no path of an address that another portfolio holds" $
      inDatabaseE $ do
        pool <- walletPool
        (otherId, _) <- testNewAcc2
        _ <- genExtAddress def otherId ""
        found <- fetchSweepCoins btc (toStore store) (Just pool) [] [sweptKeyAddrs !! 0] [] [externalAddr2, externalAddr 1]
        liftIO $ found.destinationPaths `shouldBe` Map.fromList [(externalAddr 1, legacyPath (Deriv :/ 0 :/ 1))]
    -- A sweep outside the wallet, which pays no portfolio
    it "records no paths without a portfolio to pay" $
      inDatabaseE $ do
        _ <- walletPool
        found <- fetchSweepCoins btc (toStore store) Nothing [] [sweptKeyAddrs !! 0] [] [externalAddr 1, otherAddr 0]
        liftIO $ found.destinationPaths `shouldBe` Map.empty
    it "finds nothing at an address without coins and asks for no raw transaction" $
      inDatabaseE $ do
        pool <- walletPool
        asked <- liftIO $ newIORef []
        found <- fetchSweepCoins btc (notingRawRequests asked (toStore store)) (Just pool) [] [otherAddr 5] [] [otherAddr 0]
        liftIO $ do
          summary found `shouldBe` ([], [], Map.empty, [])
          readIORef asked `shouldReturn` []
    it "refuses to fetch without keys, addresses or coins" $
      inDatabaseE $ do
        pool <- walletPool
        lift $
          dbShouldFailWith "No private keys, addresses or coins to sweep from" $
            fetchSweepCoins btc (toStore store) (Just pool) [] [] [] [otherAddr 0]
    it "refuses a store that does not return the transaction a coin comes from" $
      inDatabaseE $ do
        pool <- walletPool
        lift $
          dbShouldFailWith "The store was asked for 1 raw transactions but returned 0" $
            fetchSweepCoins btc (toStore store {fakeRawTransactions = []}) (Just pool) [] [sweptKeyAddrs !! 0] [] [otherAddr 0]
    -- The funding of the swept key pays 100000 to the first of its
    -- addresses. Its txid, b3aa78f0...fc22, is worked out apart from hw.
    it "refuses a coin the store reports worth less than its transaction pays, which would leave the rest to the fee" $
      inDatabaseE $ do
        pool <- walletPool
        let lying = store {fakeUnspents = [storeCoin (txHash keyFunding, 0) Nothing (sweptKeyAddrs !! 0) 5000]}
        lift $
          dbShouldFailWith "The store reports the coin b3aa78f0cd7d3fc81b9d7c0778c5c99339aeebf8487afc294e1ee5979f70fc22:0 as worth 5000 satoshis, but its transaction pays 100000" $
            fetchSweepCoins btc (toStore lying) (Just pool) [] [sweptKeyAddrs !! 0] [] [otherAddr 0]
    it "refuses a store that is not healthy" $
      inDatabaseE $ do
        pool <- walletPool
        lift $
          dbShouldFailWith "The store is down" $
            fetchSweepCoins btc (unhealthy (toStore store)) (Just pool) [] [sweptKeyAddrs !! 0] [] [otherAddr 0]
  where
    -- Portfolio 0 of the test wallet on Bitcoin, with the first two
    -- receiving addresses of its bip44 account and the first one of its
    -- bip84 account
    walletPool :: ExceptT String (DB IO) Pool
    walletPool = do
      (accId, _) <- testNewAcc
      _ <- genExtAddress def accId ""
      _ <- genExtAddress def accId ""
      (segwitId, _) <- testNewSegwitAcc
      _ <- genExtAddress def segwitId ""
      testPool
    keyFunding = fundingTx 1 (zip sweptKeyAddrs [100000, 200000, 300000, 400000])
    store =
      emptyStore
        { fakeUnspents = coinsOf keyFunding,
          fakeTransactions = [payoutTx (txHash keyFunding) (Store.BlockRef 800000 0) (zip sweptKeyAddrs [100000, 200000, 300000, 400000])],
          fakeRawTransactions = [(txHash keyFunding, keyFunding)]
        }
    -- What was fetched: the coins as (output index, value), the transactions,
    -- the paths of the wallet's destinations and the addresses known to be
    -- of compressed keys
    summary :: SweepCoins -> ([(Word32, Word64)], [Tx], Map.Map Address PoolPath, [Address])
    summary found = ([(c.outpoint.index, c.value) | c <- found.coins], found.transactions, found.destinationPaths, found.compressedAddrs)

{- Sweeping into the wallet -}

-- The original use of a sweep, as the commands run it: coins of an outside
-- key are swept to an address of the wallet, the transaction waits among
-- the pending transactions, is signed there by its nosigHash with the
-- outside key, and the signed one takes its place.
sweepIntoWalletSpec :: Spec
sweepIntoWalletSpec =
  describe "Sweeping into the wallet" $ do
    it "keeps the sweep pending, signs it in place and reports the credit" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc
        pool <- testPool
        _ <- genExtAddress def accId ""
        _ <- genExtAddress def accId ""
        -- preparesweep: the coins of the swept key, 100000 to 400000, to the
        -- wallet's second external address and to an outside address. The
        -- wallet holds no coin, so none is locked.
        found <- SweepCoins (coinsOf keyFunding) [keyFunding] <$> poolPaths pool [externalAddr 1, otherAddr 0] <*> pure [sweptKeyCompressedAddr]
        tsd <- liftEither $ buildSweepSignData btc (mkStdGen 0) found [(externalAddr 1, Remainder), (otherAddr 0, Remainder)] 10 800002
        let pendingHash = nosigHash tsd.txSignDataTx
        importPendingTx pool tsd `dbShouldBeE` pendingHash
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]
        coinsAfter pool [accId] AllCoins Nothing Nothing `dbShouldBeE` []
        -- signsweep by nosigHash, with the outside key only
        held <- requirePendingTx pendingHash
        liftIO $ held.pendingTxOnline `shouldBe` False
        (signed, info) <- liftEither $ signTxWithKeys btc held.pendingTxData [(Bip44, accountPub)] noneHeld [sweptKey]
        importPendingTx pool signed `dbShouldBeE` pendingHash
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
        -- The wallet receives one of the two outputs: 10 bytes of overhead,
        -- inputs of 148 + 180 + 41 + 64, two P2PKH outputs and 2 + 1 + 1 +
        -- 108 + 108 of witness data, 220 in quarters rounded up to 55, make
        -- 566 virtual bytes, a fee of 5660, so (1000000 - 5660) / 2 =
        -- 497170 each
        liftIO $ (info.txInfoSummary.summaryType, info.txInfoSummary.summaryAmount, info.txInfoSummary.summaryFee) `shouldBe` (TxCredit, 497170, 5660)
        liftIO $ Map.keys info.txInfoSummary.summaryMyOutputs `shouldBe` [externalAddr 1]
        -- Paid into the wallet or not, a sweep has no change
        liftIO $ info.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
        -- broadcast marks it online, and the sync retires it once the store
        -- lists it
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
    it "refuses to keep a transaction that does not involve the wallet" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc
        pool <- testPool
        _ <- genExtAddress def accId ""
        let tx = spendingTx [(txHash keyFunding, 0)] [(otherAddr 0, 90000)]
            outside = TxSignData tx [keyFunding] [] [] (Just 10) Nothing [] noLabel
        lift $
          dbShouldFailWith "The transaction does not involve this wallet" $
            importPendingTx pool outside
  where
    accountPub = testAccountPub
    keyFunding = fundingTx 1 (zip sweptKeyAddrs [100000, 200000, 300000, 400000])
