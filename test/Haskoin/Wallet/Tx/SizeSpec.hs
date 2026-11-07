{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The size of a transaction before it is signed, in virtual bytes, as
-- haskoin-core estimates it from the kinds of its inputs and outputs: the
-- kinds hw maps its accounts, coins and scripts onto, and the sizes hw
-- relies on; then the size of a signed transaction, and of one the store
-- reports. The numbers are written out from the byte counts in the
-- documentation of "Haskoin.Wallet.Tx.Size".
module Haskoin.Wallet.Tx.SizeSpec (spec) where

import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import Data.Word (Word32)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.Size
import Test.Hspec

spec :: Spec
spec = do
  describe "The size of a transaction of P2PKH inputs and outputs" $ do
    it "is 10 bytes of frame, 148 per input and 34 per output" $ do
      sizeOf 1 1 `shouldBe` 192
      sizeOf 1 2 `shouldBe` 226
      sizeOf 2 2 `shouldBe` 374
      sizeOf 3 2 `shouldBe` 522
      sizeOf 1 3 `shouldBe` 260
    it "is the frame alone without inputs and outputs" $
      sizeOf 0 0 `shouldBe` 10
    it "counts 3 bytes for a count of 253 or more" $ do
      -- 252 inputs: 10 + 252 * 148 + 34; 253 inputs: 12 + 253 * 148 + 34
      sizeOf 252 1 `shouldBe` 37340
      sizeOf 253 1 `shouldBe` 37490
      -- The same for the outputs: 10 + 148 + 252 * 34, then 12 + 148 + 253 * 34
      sizeOf 1 252 `shouldBe` 8726
      sizeOf 1 253 `shouldBe` 8762
    it "counts 5 bytes for a count above 65535" $ do
      -- 11 + 65535 * 148 + 34, then 13 + 65536 * 148 + 34
      sizeOf 65535 1 `shouldBe` 9699226
      sizeOf 65536 1 `shouldBe` 9699376

  -- What nodes relay, as Bitcoin Core computes it: three satoshis for each
  -- byte of the output and of the smallest input that spends it, which is
  -- 148 bytes without a witness and 67 with one
  describe "The dust limit of an output, by its kind" $ do
    it "is what nodes relay: 546 for P2PKH, 540 for P2SH, 294 for P2WPKH, 330 for P2WSH and Taproot" $ do
      outputDustLimit OutputPKHash `shouldBe` 546 -- 3 × (34 + 148)
      outputDustLimit OutputScriptHash `shouldBe` 540 -- 3 × (32 + 148)
      outputDustLimit OutputWitnessPKHash `shouldBe` 294 -- 3 × (31 + 67)
      outputDustLimit OutputWitnessScriptHash `shouldBe` 330 -- 3 × (43 + 67)
      outputDustLimit OutputTaproot `shouldBe` 330 -- 3 × (43 + 67)
    it "is 576 for the P2SH32 output of Bitcoin Cash" $
      outputDustLimit OutputScript32Hash `shouldBe` 576 -- 3 × (44 + 148)

  -- Nodes relay a transaction of at most 400000 weight units, which are
  -- 100000 virtual bytes. With 675 P2PKH coins and one P2PKH output a
  -- transaction takes 12 + 675 × 148 + 34 = 99946 bytes, and with 676 it
  -- takes 12 + 676 × 148 + 34 = 100094.
  describe "The largest transaction nodes relay" $ do
    it "holds 675 P2PKH coins paid to one P2PKH output" $
      checkStandardSize (replicate 675 InputPKHash) [OutputPKHash] `shouldBe` Right ()
    -- 12 + 675 × 148 + 34 + (8 + 1 + 45) = 100000 bytes, 400000 weight units
    -- exactly, which nodes relay; one byte more of script is too many
    it "holds a transaction of 400000 weight units exactly, and refuses one byte more" $ do
      checkStandardSize (replicate 675 InputPKHash) [OutputPKHash, OutputScript 45] `shouldBe` Right ()
      checkStandardSize (replicate 675 InputPKHash) [OutputPKHash, OutputScript 46]
        `shouldBe` Left "The transaction would take 100001 virtual bytes, and nodes relay at most 100000: spend fewer coins or pay fewer recipients in one transaction"
    it "does not hold 676 of them, which is refused with the size it would take" $
      checkStandardSize (replicate 676 InputPKHash) [OutputPKHash]
        `shouldBe` Left "The transaction would take 100094 virtual bytes, and nodes relay at most 100000: spend fewer coins or pay fewer recipients in one transaction"

  prepareContext $ \ctx -> do
    describe "The kind of an input, by the output script of its coin" $ do
      it "is P2PKH, P2WPKH, or P2WPKH nested in P2SH, the only script hash hw signs" $ do
        inputKind (scriptOf keyHashAddr) `shouldBe` Just InputPKHash
        inputKind (scriptOf witnessAddr) `shouldBe` Just InputWitnessPKHash
        inputKind (scriptOf scriptAddr) `shouldBe` Just InputNestedWitnessPKHash
      it "is unknown for a coin hw cannot sign" $ do
        inputKind (scriptOf witnessScriptAddr) `shouldBe` Nothing
        -- A coin paid to a public key itself (P2PK) has no address
        inputKind (PayPK (derivePublicKey ctx (wrapSecKey True sweptKey))) `shouldBe` Nothing
        inputKind (scriptOf taprootAddr) `shouldBe` Nothing

    describe "The virtual size of an input on its own, by its kind" $ do
      it "is 148 bytes for a P2PKH coin, 180 with an uncompressed key" $ do
        inputVirtualSize InputPKHash `shouldBe` 148
        inputVirtualSize InputUncompressedPKHash `shouldBe` 180
      it "is the bytes and a quarter of the 108-byte witness, rounded up: 41 + 27 for a P2WPKH coin, 64 + 27 nested in P2SH" $ do
        inputVirtualSize InputWitnessPKHash `shouldBe` 68
        inputVirtualSize InputNestedWitnessPKHash `shouldBe` 91

    describe "The size of an output, by its script" $
      it "is 34 bytes for P2PKH, 32 for P2SH, 31 for P2WPKH, 43 for P2WSH and Taproot" $ do
        outputSize (scriptOutputKind ctx (addressScript ctx keyHashAddr)) `shouldBe` 34
        outputSize (scriptOutputKind ctx (addressScript ctx scriptAddr)) `shouldBe` 32
        outputSize (scriptOutputKind ctx (addressScript ctx witnessAddr)) `shouldBe` 31
        outputSize (scriptOutputKind ctx (addressScript ctx witnessScriptAddr)) `shouldBe` 43
        outputSize (scriptOutputKind ctx (addressScript ctx taprootAddr)) `shouldBe` 43

    describe "The kind of an output, by its script" $
      it "is the kind haskoin-core names for the script, and that of its length for any other script" $ do
        scriptOutputKind ctx (addressScript ctx keyHashAddr) `shouldBe` OutputPKHash
        scriptOutputKind ctx (addressScript ctx scriptAddr) `shouldBe` OutputScriptHash
        scriptOutputKind ctx (addressScript ctx witnessAddr) `shouldBe` OutputWitnessPKHash
        scriptOutputKind ctx (addressScript ctx witnessScriptAddr) `shouldBe` OutputWitnessScriptHash
        scriptOutputKind ctx (addressScript ctx taprootAddr) `shouldBe` OutputTaproot
        -- OP_RETURN, then a push of the 5 bytes of "hello": 7 bytes
        scriptOutputKind ctx (hexBytes "6a0568656c6c6f") `shouldBe` OutputScript 7

    describe "The witness data of a transaction" $ do
      it "adds 2 marker bytes and the witness of a SegWit input, which count a quarter, rounded up" $
        -- 10 + 41 + 34 = 85, then (2 + 108) / 4 = 27.5, rounded up to 28
        txSize [InputWitnessPKHash] [OutputPKHash] `shouldBe` 113
      it "adds an empty 1-byte witness for a legacy input next to a SegWit one" $
        -- 10 + 148 + 41 + 34 = 233, then (2 + 1 + 108) / 4 = 27.75, rounded up to 28
        txSize [InputPKHash, InputWitnessPKHash] [OutputPKHash] `shouldBe` 261
      it "adds nothing to a transaction without SegWit inputs" $
        -- 10 + 148 + 180 + 34
        txSize [InputPKHash, InputUncompressedPKHash] [OutputPKHash] `shouldBe` 372

    describe "The size of a transaction of a BIP84 account, with P2WPKH inputs and outputs" $ do
      it "is 10 bytes of frame, 41 per input and 31 per output, then the witnesses in quarters" $ do
        -- 10 + 41 + 2 * 31 = 113, then (2 + 108) / 4 = 27.5, rounded up to 28:
        -- 141, where the same payment of a BIP44 account is 226
        segwitSizeOf 1 2 `shouldBe` 141
        -- 10 + 41 + 31 = 82, then 28
        segwitSizeOf 1 1 `shouldBe` 110
        -- 10 + 2 * 41 + 2 * 31 = 154, then (2 + 2 * 108) / 4 = 54.5, rounded up to 55
        segwitSizeOf 2 2 `shouldBe` 209
      it "spends P2WPKH coins, 68 virtual bytes each, where a BIP44 account spends P2PKH coins, 148 each" $ do
        accountInput Bip84 `shouldBe` InputWitnessPKHash
        accountInput Bip44 `shouldBe` InputPKHash
        accountInputSize Bip84 `shouldBe` 68
        accountInputSize Bip44 `shouldBe` 148
      it "pays its change to a 31-byte P2WPKH output, where a BIP44 account pays a 34-byte P2PKH one" $ do
        accountOutputKind Bip84 `shouldBe` OutputWitnessPKHash
        accountOutputKind Bip44 `shouldBe` OutputPKHash
        accountOutputSize Bip84 `shouldBe` 31
        accountOutputSize Bip44 `shouldBe` 34

    describe "The size of a signed transaction" $ do
      -- The P2WPKH payment of the Trezor signing vectors: 223 bytes, of
      -- which 114 are outside the witness, so 3 * 114 + 223 = 565 of
      -- weight, 141.25 virtual bytes, rounded up to 142
      it "is its weight in quarters, rounded up, for a SegWit transaction" $
        signedTxSize (fromJust (eitherToMaybe (S.decode (hexBytes trezorNativeTx)))) `shouldBe` 142
      -- The P2PKH payment of the Trezor signing vectors, trezor/testnet/
      -- one-two-fee: 226 bytes without a witness, so 226 virtual bytes
      it "is its size in bytes for a legacy transaction" $
        signedTxSize (fromJust (eitherToMaybe (S.decode (hexBytes trezorLegacyTx)))) `shouldBe` 226

    describe "The size of a transaction the store reports" $
      it "is its weight in quarters, rounded up" $ do
        -- The fixture: a weight of 904, 226 virtual bytes
        storeTxSize (storeTx (fakeTxHash 1) (Store.BlockRef 800000 0) [] [] 1000) `shouldBe` 226
        -- A weight of 565 is 141.25, rounded up to 142
        storeTxSize (withWeight 565 (storeTx (fakeTxHash 1) (Store.BlockRef 800000 0) [] [] 1000)) `shouldBe` 142
  where
    -- The same transaction of the store with the weight given
    withWeight :: Word32 -> Store.Transaction -> Store.Transaction
    withWeight w (Store.Transaction b v l i o d r t x s _ f) = Store.Transaction b v l i o d r t x s w f
    sizeOf inputs outputs = txSize (replicate inputs InputPKHash) (replicate outputs OutputPKHash)
    segwitSizeOf inputs outputs = txSize (replicate inputs InputWitnessPKHash) (replicate outputs OutputWitnessPKHash)
    scriptOf = fromJust . addressToOutput
    -- The signed transaction of the Trezor vector trezor/testnet/one-two-fee
    -- of test/data/tx-signing: one P2PKH input, two P2PKH outputs
    trezorLegacyTx = "0100000001cd3b93f5b24ae190ce5141235091cd93fbb2908e24e5b9ff6776aec11b0e04e5000000006b483045022100eba3bbcbb82ab1ebac88a394e8fb53b0263dadbb3e8072f0a21ee62818c911060220686a9b7f306d028b54a228b5c47cc6c27b1d01a3b0770440bcc64d55d8bace2c0121030e669acac1f280d1ddf441cd2ba5e97417bf2689e4bbec86df4f831bf9f7ffd0ffffffff021023cb01000000001976a91485eb47fe98f349065d6f044e27a4ac541af79ee288aca0bb0d00000000001976a9143d3cca567e00a04819742b21a696a67da796498b88ac00000000"
    -- The signed transaction of the Trezor vector trezor/testnet/native of
    -- test/data/tx-signing: one P2WPKH input, a P2SH and a P2WPKH output
    trezorNativeTx = "010000000001019e2123e595e20d6aa2c8d227b2b98c781b8b41fda635756eca0768b8ce8067b30000000000ffffffff02409c00000000000017a9147a55d61848e77ca266e79a39bfc85c580a6426c98750c3000000000000160014cd4ee15090b177f15ed0760d85956d93b5990d5f0247304402200c734ed16a9226162a29133c14fad3565332c60346050ceb9246e73a2fc8485002203463d40cf78eb5cc9718d6617d9f251b987e96cb58525795a507acb9b91696c7012103f60fc56bf7b5326537c7e86e0a63b6cd008eeb87d39af324cee5bcc3424bf4d000000000"
    -- The addresses of the swept key of "Haskoin.Wallet.Tx.SweepSpec", and
    -- the P2WSH and Taproot examples of BIP173 and BIP350
    sweptKey = (.key) . fromJust $ fromWif btc "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU7Bqcefezr"
    keyHashAddr = fromJust $ textToAddr btc "18FtYoEJipP5bCJnjgWdeeEbxSvozSjEUA"
    witnessAddr = fromJust $ textToAddr btc "bc1qf7vmha6hqljyhs405efn0hkwj98gz74v86kel2"
    scriptAddr = fromJust $ textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p"
    witnessScriptAddr = fromJust $ textToAddr btc "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
    taprootAddr = fromJust $ textToAddr btc "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
