{-# LANGUAGE OverloadedRecordDot #-}

-- | The sizes of transactions, in virtual bytes, which is how hw measures
-- sizes and fee rates everywhere: the size of a transaction before it is
-- signed, which its fee is paid on, counted from the kinds of its inputs
-- and the scripts of its outputs, and the size of a signed one, measured
-- from its bytes. Signatures are counted at their largest, so an estimate
-- is an upper bound, by a byte or two per input.
--
-- The network charges fees by weight: the bytes outside the witnesses
-- count four, and the bytes of the witnesses count one. The virtual size
-- is the weight in quarters, rounded up, as Bitcoin Core counts it, so the
-- virtual size of a transaction without a witness is its size in bytes.
-- The inputs of a BIP84 account spend P2WPKH coins, whose signatures are
-- in the witness: such an input is 68 virtual bytes, where a P2PKH one is
-- 148 ('accountInput').
--
-- The fee rate and the dust limit a transaction is built with are checked
-- here too, for the builders of payments and of sweeps, beside the default
-- fee rate and the rate above which hw asks the user first.
module Haskoin.Wallet.Tx.Size
  ( InputSize (..),
    inputSize,
    inputVirtualSize,
    p2pkhInput,
    p2pkhInputSize,
    p2wpkhInput,
    uncompressedKeyInput,
    accountInput,
    accountInputSize,
    outputSize,
    p2pkhOutputSize,
    p2wpkhOutputSize,
    accountOutputSize,
    txSize,
    signedTxSize,
    storeTxSize,
    varIntSize,
    highFeeRate,
    maxFeeRate,
    checkFeeRateAndDust,
    checkFeeRate,
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Data.Word (Word64)
import Haskoin (ScriptOutput (..), Tx (..))
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (maxMoney)
import Haskoin.Wallet.Keys (AccountKind (..))
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
--     the public key, each with its length, after their count: 108 bytes
--     ('p2wpkhInput').
--   * P2SH-P2WPKH: the script pushes the 22-byte P2WPKH script, 64 bytes,
--     with the same witness. A P2SH coin is taken for one of these, the
--     only script hash hw signs.
inputSize :: ScriptOutput -> Maybe InputSize
inputSize script =
  case script of
    PayPKHash _ -> Just p2pkhInput
    PayPK _ -> Just (InputSize 114 0)
    PayWitnessPKHash _ -> Just p2wpkhInput
    PayScriptHash _ -> Just (InputSize 64 108)
    _ -> Nothing

-- | The virtual size of an input on its own: its bytes, and a quarter of
-- those of its witness, rounded up. The marker and flag bytes of a
-- transaction with witnesses are counted once, in 'txSize'.
inputVirtualSize :: InputSize -> Word64
inputVirtualSize input = input.inputBytes + quarter input.witnessBytes

-- | The input of a P2PKH coin of a compressed key, the kind a BIP44
-- account spends: a 107-byte script (1 + 72 for the signature with its
-- hash type, 1 + 33 for the public key) in a 148-byte input
p2pkhInput :: InputSize
p2pkhInput = InputSize p2pkhInputSize 0

p2pkhInputSize :: Word64
p2pkhInputSize = 148

-- | The input of a P2WPKH coin, the kind a BIP84 account spends: 41 bytes
-- with an empty script, and the signature and the public key in its
-- witness, 108 bytes that count a quarter, 68 virtual bytes in all
p2wpkhInput :: InputSize
p2wpkhInput = InputSize 41 108

-- | The input of a P2PKH coin of an uncompressed key: a 65-byte public key
-- instead of 33
uncompressedKeyInput :: InputSize
uncompressedKeyInput = InputSize 180 0

-- | The input of a coin of an account of the kind: P2PKH for a BIP44
-- account, P2WPKH for a BIP84 one
accountInput :: AccountKind -> InputSize
accountInput Bip44 = p2pkhInput
accountInput Bip84 = p2wpkhInput

-- | The virtual size of the input of a coin of an account of the kind:
-- 148 for a BIP44 account, 68 for a BIP84 one
accountInputSize :: AccountKind -> Word64
accountInputSize = inputVirtualSize . accountInput

{- Outputs -}

-- | The size of an output paying a script: 8 bytes of amount, the length
-- of the script and the script
outputSize :: BS.ByteString -> Word64
outputSize script = 8 + varIntSize scriptLength + fromIntegral scriptLength
  where
    scriptLength = BS.length script

-- | A P2PKH output, the kind a BIP44 account pays its change to: a 25-byte
-- script in a 34-byte output
p2pkhOutputSize :: Word64
p2pkhOutputSize = 34

-- | A P2WPKH output, the kind a BIP84 account pays its change to: a
-- 22-byte script in a 31-byte output
p2wpkhOutputSize :: Word64
p2wpkhOutputSize = 31

-- | The size of the output an account of the kind pays its change to:
-- P2PKH for a BIP44 account, P2WPKH for a BIP84 one
accountOutputSize :: AccountKind -> Word64
accountOutputSize Bip44 = p2pkhOutputSize
accountOutputSize Bip84 = p2wpkhOutputSize

{- Transactions -}

-- | The virtual size of a transaction with the given inputs and outputs: 4
-- bytes of version, the counts of inputs and outputs as variable-length
-- integers, the inputs, the outputs and 4 bytes of lock time, all of which
-- count in full; then, for a transaction with a witness, a marker byte and
-- a flag byte, and a witness for every input, an empty one, a single count
-- byte, for an input without, which together count a quarter, rounded up.
txSize :: [InputSize] -> [Word64] -> Word64
txSize inputs outputs =
  overhead + sum ((.inputBytes) <$> inputs) + sum outputs + quarter witnessData
  where
    overhead = 8 + varIntSize (length inputs) + varIntSize (length outputs)
    witnesses = (.witnessBytes) <$> inputs
    witnessData
      | all (== 0) witnesses = 0
      | otherwise = 2 + sum [max 1 w | w <- witnesses]

-- | The virtual size of a signed transaction, from its bytes: those
-- outside the witnesses count four, those of the witnesses one, and the
-- weight is taken in quarters, rounded up. A transaction without a
-- witness is serialized without the marker and flag bytes, so its virtual
-- size is its size.
signedTxSize :: Tx -> Natural
signedTxSize tx = virtualSize (BS.length (S.encode tx {witness = []})) (BS.length (S.encode tx))

-- | The virtual size of a transaction as the store reports it, from the
-- weight the store gives it
storeTxSize :: Store.Transaction -> Natural
storeTxSize tx = fromIntegral ((tx.weight + 3) `div` 4)

-- | The virtual size from the bytes outside the witnesses and all the
-- bytes: the weight, three times the former plus the latter, in quarters
-- rounded up
virtualSize :: Int -> Int -> Natural
virtualSize base total = fromIntegral ((3 * base + total + 3) `div` 4)

-- | A number of witness bytes in virtual bytes: a quarter, rounded up
quarter :: Word64 -> Word64
quarter bytes = (bytes + 3) `div` 4

-- | The size in bytes of a count as Bitcoin encodes it: one byte up to 252,
-- then a marker byte and 2, 4 or 8 bytes
varIntSize :: Int -> Word64
varIntSize n
  | n <= 0xfc = 1
  | n <= 0xffff = 3
  | n <= 0xffffffff = 5
  | otherwise = 9

{- Rates -}

-- | The highest fee rate preparetx and preparesweep take without asking, in
-- satoshis per virtual byte. A rate above it is more often a typo than a
-- choice, such as 1000 for 100, so the user confirms it, or gives --yes.
-- Fees have gone higher at the busiest times of Bitcoin, so a higher rate
-- is not refused. The help of --fee-rate, --yes, preparetx and
-- preparesweep names this rate.
highFeeRate :: Natural
highFeeRate = 100

-- | The highest fee rate hw builds a transaction at, in satoshis per
-- virtual byte: 32 bits, so that a fee, the rate times a size, stays
-- within the 64 bits of an amount
maxFeeRate :: Natural
maxFeeRate = 2 ^ (32 :: Int) - 1

-- | Refuse a fee rate or a dust limit that a transaction cannot be built
-- with: a rate 'checkFeeRate' refuses, or a dust limit above the 21
-- million bitcoins there will ever be. The command line takes any number,
-- and the builders count in 64 bits: a rate of 2^64 wrapped around to
-- zero.
checkFeeRateAndDust :: Natural -> Natural -> Either String ()
checkFeeRateAndDust feeByte dust = do
  checkFeeRate feeByte
  when (toInteger dust > maxMoney) $ Left "The dust limit must be at most the 21 million bitcoins there will ever be"

-- | Refuse a fee rate that a transaction cannot be built with: zero,
-- which no node relays, or above 'maxFeeRate'
checkFeeRate :: Natural -> Either String ()
checkFeeRate feeByte = do
  when (feeByte == 0) $ Left "The fee rate must be at least 1 satoshi per virtual byte"
  when (feeByte > maxFeeRate) $ Left $ "The fee rate must be at most " <> show maxFeeRate <> " satoshis per virtual byte"
