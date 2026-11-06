{-# LANGUAGE OverloadedRecordDot #-}

-- | The sizes of transactions, in virtual bytes, which is how hw measures
-- sizes and fee rates everywhere: the size of a transaction before it is
-- signed, which its fee is paid on, estimated by haskoin-core from the
-- kinds of its inputs and outputs ('guessTxSize'), and the size of a
-- signed one, measured from its bytes. Signatures are counted at their
-- largest, so an estimate is an upper bound, by a byte or two per input.
--
-- The network charges fees by weight: the bytes outside the witnesses
-- count four, and the bytes of the witnesses count one. The virtual size
-- is the weight in quarters, rounded up, as Bitcoin Core counts it, so the
-- virtual size of a transaction without a witness is its size in bytes.
-- The inputs of a BIP84 account spend P2WPKH coins, whose signatures are
-- in the witness: such an input is 68 virtual bytes, where a P2PKH one is
-- 148 ('accountInput').
--
-- This module maps what hw knows, the kind of an account and the scripts
-- of its coins and outputs, onto the kinds of haskoin-core. The fee rate
-- and the dust limit a transaction is built with are checked here too,
-- for the builders of payments and of sweeps, beside the default fee rate
-- and the rate above which hw asks the user first.
--
-- Two limits of the relay policy of nodes are here as well, both taken
-- from haskoin-core: the smallest amount an output may hold, by its kind
-- ('outputDustLimit'), and the largest transaction ('checkStandardSize').
-- A transaction beyond either does not spread, so hw builds none.
module Haskoin.Wallet.Tx.Size
  ( inputKind,
    inputVirtualSize,
    accountInput,
    accountInputSize,
    scriptOutputKind,
    recipientOutputKind,
    accountOutputKind,
    accountOutputSize,
    outputDustLimit,
    txSize,
    checkStandardSize,
    signedTxSize,
    storeTxSize,
    highFeeRate,
    maxFeeRate,
    checkFeeRateAndDust,
    checkFeeRate,
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import Haskoin (Address, Ctx, InputKind (..), OutputKind (..), ScriptOutput (..), Tx, addressToOutput, dustThreshold, guessTxSize, guessTxWeight, inputWeight, marshal, maxStandardTxWeight, outputSize, txVirtualSize, unmarshal)
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (maxMoney)
import Haskoin.Wallet.Keys (AccountKind (..))
import Numeric.Natural (Natural)

{- Inputs -}

-- | The kind of the signed input of a coin, by its output script, for a
-- compressed public key; Nothing for a kind hw cannot sign. A P2SH coin
-- is taken for a P2SH-P2WPKH one, the only script hash hw signs. The
-- sweep tells a P2PKH coin of an uncompressed key apart itself, as
-- 'InputUncompressedPKHash'.
--
--   * P2PKH: the script pushes the signature and the 33-byte public key:
--     148 bytes (180 for an uncompressed key).
--   * P2WPKH: an empty script, 41 bytes; the witness holds the signature and
--     the public key, each with its length, after their count: 108 bytes.
--   * P2SH-P2WPKH: the script pushes the 22-byte P2WPKH script, 64 bytes,
--     with the same witness.
inputKind :: ScriptOutput -> Maybe InputKind
inputKind script =
  case script of
    PayPKHash _ -> Just InputPKHash
    PayWitnessPKHash _ -> Just InputWitnessPKHash
    PayScriptHash _ -> Just InputNestedWitnessPKHash
    _ -> Nothing

-- | The virtual size of an input of the kind on its own: its bytes, and a
-- quarter of those of its witness, rounded up. The marker and flag bytes
-- of a transaction with witnesses are counted once, in 'txSize'.
inputVirtualSize :: InputKind -> Word64
inputVirtualSize kind = fromIntegral bytes + quarter (fromIntegral witnessBytes)
  where
    (bytes, witnessBytes) = inputWeight kind

-- | The input of a coin of an account of the kind: P2PKH for a BIP44
-- account, 148 bytes; P2WPKH for a BIP84 one, 41 bytes with an empty
-- script, and the signature and the public key in its 108-byte witness
accountInput :: AccountKind -> InputKind
accountInput Bip44 = InputPKHash
accountInput Bip84 = InputWitnessPKHash

-- | The virtual size of the input of a coin of an account of the kind:
-- 148 for a BIP44 account, 68 for a BIP84 one
accountInputSize :: AccountKind -> Word64
accountInputSize = inputVirtualSize . accountInput

{- Outputs -}

-- | The kind of an output paying a script, as haskoin-core names the
-- kinds. The kind says the size of the output, 8 bytes of amount, the
-- length of the script and the script, and the amount below which nodes
-- do not relay it ('outputDustLimit'). A script of no named kind, such as
-- a bare multisig, a data carrier or a witness program of a later
-- version, is of the kind of its length.
scriptOutputKind :: Ctx -> BS.ByteString -> OutputKind
scriptOutputKind ctx script = case unmarshal ctx script :: Either String ScriptOutput of
  Right (PayPKHash _) -> OutputPKHash
  Right (PayScriptHash _) -> OutputScriptHash
  Right (PayScript32Hash _) -> OutputScript32Hash
  Right (PayWitnessPKHash _) -> OutputWitnessPKHash
  Right (PayWitnessScriptHash _) -> OutputWitnessScriptHash
  Right (PayWitness version program) | version == 1 && BS.length program == 32 -> OutputTaproot
  _ -> OutputScript (BS.length script)

-- | The kind of the output paying a recipient. An address that cannot be
-- paid is counted as P2PKH: the builder refuses it further down.
recipientOutputKind :: Ctx -> Address -> OutputKind
recipientOutputKind ctx addr =
  maybe OutputPKHash (scriptOutputKind ctx . marshal ctx) (addressToOutput addr)

-- | The output an account of the kind pays its change to: P2PKH for a
-- BIP44 account, a 25-byte script in a 34-byte output; P2WPKH for a BIP84
-- one, a 22-byte script in a 31-byte output
accountOutputKind :: AccountKind -> OutputKind
accountOutputKind Bip44 = OutputPKHash
accountOutputKind Bip84 = OutputWitnessPKHash

-- | The size of the output an account of the kind pays its change to: 34
-- for a BIP44 account, 31 for a BIP84 one
accountOutputSize :: AccountKind -> Word64
accountOutputSize = fromIntegral . outputSize . accountOutputKind

-- | The dust limit of an output of the kind: the smallest amount it may
-- hold, in satoshis. Nodes do not relay an output below the threshold of
-- its kind, which haskoin-core's dustThreshold gives as Bitcoin Core
-- computes it: 546 for P2PKH, 540 for P2SH, 294 for P2WPKH, and 330 for
-- P2WSH and Taproot. The limit asked with --dust-limit raises that of
-- every output to its amount, and lowers none: one output below what
-- nodes relay keeps the whole transaction from spreading. Without the
-- option the limit asked is zero.
outputDustLimit :: Natural -> OutputKind -> Natural
outputDustLimit asked kind = max asked (fromIntegral (dustThreshold kind))

{- Transactions -}

-- | The virtual size of a transaction with the given inputs and outputs,
-- as haskoin-core estimates it: 4 bytes of version, the counts of inputs
-- and outputs as variable-length integers, the inputs, the outputs and 4
-- bytes of lock time, all of which count in full; then, for a transaction
-- with a witness, a marker byte and a flag byte, and a witness for every
-- input, an empty one, a single count byte, for an input without, which
-- together count a quarter, rounded up.
txSize :: [InputKind] -> [OutputKind] -> Word64
txSize inputs outputs = fromIntegral (guessTxSize inputs outputs)

-- | Refuse a transaction too large for nodes to relay: one whose weight,
-- as haskoin-core estimates it, is above its maxStandardTxWeight, the
-- 400000 weight units of Bitcoin Core, which are 100000 virtual bytes.
-- Bitcoin Cash Node has the same limit, in bytes. The builders check the
-- transaction they are about to make: no fee makes a larger one spread.
checkStandardSize :: [InputKind] -> [OutputKind] -> Either String ()
checkStandardSize inputs outputs =
  when (guessTxWeight inputs outputs > maxStandardTxWeight) $
    Left $
      "The transaction would take "
        <> show (txSize inputs outputs)
        <> " virtual bytes, and nodes relay at most "
        <> show (maxStandardTxWeight `div` 4)
        <> ": spend fewer coins or pay fewer recipients in one transaction"

-- | The virtual size of a signed transaction, from its bytes, as
-- haskoin-core's txVirtualSize measures it: those outside the witnesses
-- count four, those of the witnesses one, and the weight is taken in
-- quarters, rounded up. A transaction without a witness is serialized
-- without the marker and flag bytes, so its virtual size is its size.
signedTxSize :: Tx -> Natural
signedTxSize = fromIntegral . txVirtualSize

-- | The virtual size of a transaction as the store reports it, from the
-- weight the store gives it
storeTxSize :: Store.Transaction -> Natural
storeTxSize tx = fromIntegral ((tx.weight + 3) `div` 4)

-- | A number of witness bytes in virtual bytes: a quarter, rounded up
quarter :: Word64 -> Word64
quarter bytes = (bytes + 3) `div` 4

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
