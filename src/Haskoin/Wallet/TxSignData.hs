{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The signing data of a transaction: the transaction, the transactions it
-- spends from, which say what each input is worth, the derivation paths of
-- the wallet's own inputs and outputs, the fee rate it was built for and
-- whether it is signed. The builders produce it, the signers sign it, and
-- the commands write it to files and keep it among the pending
-- transactions of the database, as JSON. Before and after signing, a
-- transaction is identified by its 'nosigHash'.
module Haskoin.Wallet.TxSignData where

import Control.Monad
import Data.Aeson
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
    txSignDataFeeRate :: !(Maybe Natural),
    txSignDataSigned :: !Bool
  }
  deriving (Eq, Show)

{- JSON -}

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
        <*> o .: "signed"

instance ToJSON TxSignData where
  toJSON (TxSignData t i oi op r s) =
    object
      [ "tx" .= encodeHex (S.encode t),
        "txinputs" .= (encodeHex . S.encode <$> i),
        "inputpaths" .= oi,
        "outputpaths" .= op,
        "feerate" .= r,
        "signed" .= s
      ]

instance MarshalJSON Ctx TxSignData where
  marshalValue _ = toJSON
  unmarshalValue _ = parseJSON

{- The hash without signatures -}

-- | The hash of a transaction without its signatures: the input scripts
-- and the witnesses cleared, so it is the same before and after signing.
-- It identifies a pending transaction. (haskoin-core's nosigTxHash keeps
-- the witnesses, which changes the hash of a SegWit transaction once signed.)
nosigHash :: Tx -> TxHash
nosigHash tx = nosigTxHash tx {witness = []}
