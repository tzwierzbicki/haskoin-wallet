{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The signing data of a transaction: the transaction, the transactions it
-- spends from, which say what each input is worth, the derivation paths of
-- the wallet's own inputs and outputs, and the fee rate it was built for.
-- The builders produce it, the signers sign it, and the commands write it
-- to files and keep it among the pending transactions of the database, as
-- JSON. Whether the transaction is signed is read from the transaction
-- itself. Before and after signing, it is identified by its 'nosigHash'.
module Haskoin.Wallet.TxSignData where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Haskoin
import Numeric.Natural (Natural)

data TxSignData = TxSignData
  { txSignDataTx :: !Tx,
    txSignDataInputs :: ![Tx],
    txSignDataInputPaths :: ![SoftPath],
    txSignDataOutputPaths :: ![SoftPath],
    -- | The fee rate the transaction was built for, in satoshis per byte,
    -- when known: what the fee pays beyond that rate is change that was not
    -- worth an output
    txSignDataFeeRate :: !(Maybe Natural)
  }
  deriving (Eq, Show)

{- JSON -}

-- The files and database rows of older versions of hw carry a "signed"
-- flag as well. It is ignored: the transaction says whether it is signed.

instance FromJSON TxSignData where
  parseJSON =
    withObject "txsigndata" $ \o -> do
      let f = eitherToMaybe . S.decode <=< decodeHex
      t <- maybe mzero return . f =<< o .: "tx"
      i <- maybe mzero return . mapM f =<< o .: "txinputs"
      TxSignData t i
        <$> o .: "inputpaths"
        <*> o .: "outputpaths"
        <*> o .:? "feerate"

instance ToJSON TxSignData where
  toJSON (TxSignData t i oi op r) =
    object
      [ "tx" .= encodeHex (S.encode t),
        "txinputs" .= (encodeHex . S.encode <$> i),
        "inputpaths" .= oi,
        "outputpaths" .= op,
        "feerate" .= r
      ]

instance MarshalJSON Ctx TxSignData where
  marshalValue _ = toJSON
  unmarshalValue _ = parseJSON

{- Signatures -}

-- | Whether every input of the transaction carries a signature: a script,
-- or witness data for a SegWit coin. Only their presence is checked here;
-- "Haskoin.Wallet.TxSigning" verifies the signatures themselves.
isSigned :: Tx -> Bool
isSigned tx = not (null tx.inputs) && and (zipWith signedInput tx.inputs witnesses)
  where
    -- One witness stack per input, or none at all in a legacy transaction
    witnesses = tx.witness <> repeat []
    signedInput :: TxIn -> WitnessStack -> Bool
    signedInput input stack = not (BS.null input.script) || not (null stack)

-- | The hash of a transaction without its signatures: the input scripts
-- and the witnesses cleared, so it is the same before and after signing.
-- It identifies a pending transaction. (haskoin-core's nosigTxHash keeps
-- the witnesses, which changes the hash of a SegWit transaction once signed.)
nosigHash :: Tx -> TxHash
nosigHash tx = nosigTxHash tx {witness = []}
