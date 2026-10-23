{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The size of a transaction before it is signed: the frame, the inputs
-- by the kind of their coin, the outputs by their script, and the witness
-- data. The numbers are written out from the byte counts in the
-- documentation of "Haskoin.Wallet.TxSize".
module Haskoin.Wallet.TxSizeSpec (spec) where

import Data.Maybe (fromJust)
import Haskoin
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxSize
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

  describe "The size of a count" $
    it "is 1 byte up to 252, then 3, 5 and 9 bytes" $ do
      varIntSize 0 `shouldBe` 1
      varIntSize 252 `shouldBe` 1
      varIntSize 253 `shouldBe` 3
      varIntSize 65535 `shouldBe` 3
      varIntSize 65536 `shouldBe` 5
      varIntSize 4294967295 `shouldBe` 5
      varIntSize 4294967296 `shouldBe` 9

  prepareContext $ \ctx -> do
    describe "The size of an input, by the kind of its coin" $ do
      it "is 148 bytes for a P2PKH coin, 180 with an uncompressed key" $ do
        inputSize (scriptOf keyHashAddr) `shouldBe` Just (InputSize 148 0)
        uncompressedKeyInput `shouldBe` InputSize 180 0
      it "is 114 bytes for a P2PK coin" $
        inputSize (PayPK (derivePublicKey ctx (wrapSecKey True sweptKey))) `shouldBe` Just (InputSize 114 0)
      it "is 41 bytes and a 108-byte witness for a P2WPKH coin" $
        inputSize (scriptOf witnessAddr) `shouldBe` Just (InputSize 41 108)
      it "is 64 bytes and a 108-byte witness for a P2SH-P2WPKH coin" $
        inputSize (scriptOf scriptAddr) `shouldBe` Just (InputSize 64 108)
      it "is unknown for a coin hw cannot sign" $ do
        inputSize (scriptOf witnessScriptAddr) `shouldBe` Nothing
        inputSize (scriptOf taprootAddr) `shouldBe` Nothing

    describe "The size of an output, by its script" $
      it "is 34 bytes for P2PKH, 32 for P2SH, 31 for P2WPKH, 43 for P2WSH and Taproot" $ do
        outputSize (addressScript ctx keyHashAddr) `shouldBe` 34
        outputSize (addressScript ctx scriptAddr) `shouldBe` 32
        outputSize (addressScript ctx witnessAddr) `shouldBe` 31
        outputSize (addressScript ctx witnessScriptAddr) `shouldBe` 43
        outputSize (addressScript ctx taprootAddr) `shouldBe` 43
        p2pkhOutputSize `shouldBe` 34

    describe "The witness data of a transaction" $ do
      it "adds 2 marker bytes and the witness of a SegWit input" $
        -- 10 + 41 + 34, then 2 + 108
        txSize [InputSize 41 108] [34] `shouldBe` 195
      it "adds an empty 1-byte witness for a legacy input next to a SegWit one" $
        -- 10 + 148 + 41 + 34, then 2 + 1 + 108
        txSize [p2pkhInput, InputSize 41 108] [34] `shouldBe` 344
      it "adds nothing to a transaction without SegWit inputs" $
        txSize [p2pkhInput, uncompressedKeyInput, InputSize 114 0] [34] `shouldBe` 486
  where
    sizeOf inputs outputs = txSize (replicate inputs p2pkhInput) (replicate outputs p2pkhOutputSize)
    scriptOf = fromJust . addressToOutput
    -- The addresses of the swept key of "Haskoin.Wallet.TxSweepSpec", and
    -- the P2WSH and Taproot examples of BIP173 and BIP350
    sweptKey = (.key) . fromJust $ fromWif btc "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU7Bqcefezr"
    keyHashAddr = fromJust $ textToAddr btc "18FtYoEJipP5bCJnjgWdeeEbxSvozSjEUA"
    witnessAddr = fromJust $ textToAddr btc "bc1qf7vmha6hqljyhs405efn0hkwj98gz74v86kel2"
    scriptAddr = fromJust $ textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p"
    witnessScriptAddr = fromJust $ textToAddr btc "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
    taprootAddr = fromJust $ textToAddr btc "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
