{-# LANGUAGE OverloadedRecordDot #-}

-- | The size of a transaction before it is signed, which its fee is paid
-- on: counted from the kinds of its inputs and the scripts of its outputs,
-- in bytes of the serialized transaction, which is how hw measures sizes
-- and fee rates everywhere. Signatures are counted at their largest, so an
-- estimate is an upper bound, by a byte or two per input.
--
-- The network charges fees by weight, where the bytes of the witnesses
-- count a quarter: a transaction with SegWit inputs at a given rate per
-- byte pays more per virtual byte than that rate.
--
-- The fee rate and the dust limit a transaction is built with are checked
-- here too, for the builders of payments and of sweeps.
module Haskoin.Wallet.TxSize
  ( InputSize (..),
    inputSize,
    p2pkhInput,
    p2pkhInputSize,
    uncompressedKeyInput,
    outputSize,
    p2pkhOutputSize,
    txSize,
    varIntSize,
    maxFeeRate,
    checkFeeRateAndDust,
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import Haskoin (ScriptOutput (..))
import Haskoin.Wallet.Amounts (maxMoney)
import Numeric.Natural (Natural)

{- Inputs -}

-- | The size of a signed input: its bytes among the inputs, and the bytes
-- of its witness with their item count, 0 for an input without a witness
data InputSize = InputSize
  { inputBytes :: !Word64,
    witnessBytes :: !Word64
  }
  deriving (Eq, Show)

-- | The size of the signed input of a coin, by the kind of its output
-- script, for a compressed public key; Nothing for a kind hw cannot sign.
-- An input is 32 bytes of transaction hash, 4 of output index, 1 of script
-- length, the script and 4 of sequence; a signature is counted as 72 bytes
-- with its hash type, and the push of it as 1 more.
--
--   * P2PKH: the script pushes the signature and the 33-byte public key:
--     148 bytes ('p2pkhInput'; 180 for an uncompressed key,
--     'uncompressedKeyInput').
--   * P2PK: the script pushes the signature alone: 114 bytes.
--   * P2WPKH: an empty script, 41 bytes; the witness holds the signature and
--     the public key, each with its length, after their count: 108 bytes.
--   * P2SH-P2WPKH: the script pushes the 22-byte P2WPKH script, 64 bytes,
--     with the same witness. A P2SH coin is taken for one of these, the
--     only script hash hw signs.
inputSize :: ScriptOutput -> Maybe InputSize
inputSize script =
  case script of
    PayPKHash _ -> Just p2pkhInput
    PayPK _ -> Just (InputSize 114 0)
    PayWitnessPKHash _ -> Just (InputSize 41 108)
    PayScriptHash _ -> Just (InputSize 64 108)
    _ -> Nothing

-- | The input of a P2PKH coin of a compressed key, the kind the wallet
-- spends: a 107-byte script (1 + 72 for the signature with its hash type,
-- 1 + 33 for the public key) in a 148-byte input
p2pkhInput :: InputSize
p2pkhInput = InputSize p2pkhInputSize 0

p2pkhInputSize :: Word64
p2pkhInputSize = 148

-- | The input of a P2PKH coin of an uncompressed key: a 65-byte public key
-- instead of 33
uncompressedKeyInput :: InputSize
uncompressedKeyInput = InputSize 180 0

{- Outputs -}

-- | The size of an output paying a script: 8 bytes of amount, the length
-- of the script and the script
outputSize :: BS.ByteString -> Word64
outputSize script = 8 + varIntSize scriptLength + fromIntegral scriptLength
  where
    scriptLength = BS.length script

-- | A P2PKH output, the kind the wallet pays its change to: a 25-byte
-- script in a 34-byte output
p2pkhOutputSize :: Word64
p2pkhOutputSize = 34

{- Transactions -}

-- | The size of a transaction with the given inputs and outputs: 4 bytes
-- of version, the counts of inputs and outputs as variable-length
-- integers, the inputs, the outputs and 4 bytes of lock time. A
-- transaction with a witness also carries a marker byte and a flag byte,
-- then a witness for every input: an empty one, a single count byte, for
-- an input without.
txSize :: [InputSize] -> [Word64] -> Word64
txSize inputs outputs =
  overhead + sum ((.inputBytes) <$> inputs) + sum outputs + witnessData
  where
    overhead = 8 + varIntSize (length inputs) + varIntSize (length outputs)
    witnesses = (.witnessBytes) <$> inputs
    witnessData
      | all (== 0) witnesses = 0
      | otherwise = 2 + sum [max 1 w | w <- witnesses]

-- | The size in bytes of a count as Bitcoin encodes it: one byte up to 252,
-- then a marker byte and 2, 4 or 8 bytes
varIntSize :: Int -> Word64
varIntSize n
  | n <= 0xfc = 1
  | n <= 0xffff = 3
  | n <= 0xffffffff = 5
  | otherwise = 9

{- Rates -}

-- | The highest fee rate hw builds a transaction at, in satoshis per byte:
-- 32 bits, so that a fee, the rate times a size, stays within the 64 bits
-- of an amount
maxFeeRate :: Natural
maxFeeRate = 2 ^ (32 :: Int) - 1

-- | Refuse a fee rate or a dust limit that a transaction cannot be built
-- with: a rate of zero, which no node relays, a rate above 'maxFeeRate',
-- or a dust limit above the 21 million bitcoins there will ever be. The
-- command line takes any number, and the builders count in 64 bits: a
-- rate of 2^64 wrapped around to zero.
checkFeeRateAndDust :: Natural -> Natural -> Either String ()
checkFeeRateAndDust feeByte dust = do
  when (feeByte == 0) $ Left "The fee rate must be at least 1 satoshi per byte"
  when (feeByte > maxFeeRate) $ Left $ "The fee rate must be at most " <> show maxFeeRate <> " satoshis per byte"
  when (toInteger dust > maxMoney) $ Left "The dust limit must be at most the 21 million bitcoins there will ever be"
