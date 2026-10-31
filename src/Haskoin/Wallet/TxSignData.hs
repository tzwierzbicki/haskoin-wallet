{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The signing data of a transaction: the transaction, the transactions it
-- spends from, which say what each input is worth, the derivation paths of
-- the wallet's own inputs and outputs, the fee rate it was built for, and
-- the fingerprint of the seed once it has been signed. The builders produce
-- it, the signers sign it, and the commands write it to files and keep it
-- among the pending transactions of the database, as JSON. Whether the
-- transaction is signed is read from the transaction itself. Before and
-- after signing, the database keys it by its 'nosigHash'. hw shows it by
-- its 'shownHash'. A file holds it with the network of the transaction, a
-- 'TxFile'.
module Haskoin.Wallet.TxSignData where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Pair)
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
    txSignDataFeeRate :: !(Maybe Natural),
    -- | The fingerprint of the seed that signed the transaction, which the
    -- signer writes so that an online wallet learns the seed of the account
    txSignDataFingerprint :: !(Maybe Fingerprint),
    -- | The ids of the transactions of the account it replaces, as bumpfee
    -- prepares it: a stuck transaction, and those that spend its change.
    -- None for any other transaction.
    txSignDataReplaces :: ![TxHash]
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
        <*> o .:? "fingerprint"
        <*> o .:? "replaces" .!= []

instance ToJSON TxSignData where
  toJSON = object . txSignDataPairs

-- | The keys of signing data in JSON. The transactions replaced are
-- written only for a replacement, so that the file of any other transaction
-- is as an earlier hw wrote it.
txSignDataPairs :: TxSignData -> [Pair]
txSignDataPairs (TxSignData t i oi op r f rs) =
  [ "tx" .= encodeHex (S.encode t),
    "txinputs" .= (encodeHex . S.encode <$> i),
    "inputpaths" .= oi,
    "outputpaths" .= op,
    "feerate" .= r,
    "fingerprint" .= f
  ]
    <> ["replaces" .= rs | not (null rs)]

instance MarshalJSON Ctx TxSignData where
  marshalValue _ = toJSON
  unmarshalValue _ = parseJSON

{- Files -}

-- | A transaction file, as preparetx, preparesweep, signtx, signsweep and
-- exporttx write it: the signing data, with the network of the transaction,
-- which the transaction itself does not tell. A file of an earlier hw
-- names no network.
data TxFile = TxFile
  { txFileNetwork :: !(Maybe Network),
    txFileData :: !TxSignData
  }
  deriving (Eq, Show)

-- The network is a key among those of the signing data, which an earlier
-- hw leaves aside
instance FromJSON TxFile where
  parseJSON value = TxFile <$> withObject "txfile" network value <*> parseJSON value
    where
      network o = do
        nameM <- o .:? "network"
        forM nameM $ \name -> maybe (fail ("Unknown network " <> name)) return (netByName name)

-- | The refusal of a file of an earlier hw, which names no network, where
-- nothing else tells the network of its transaction: such a file is cheap
-- to prepare again
noFileNetwork :: String
noFileNetwork = "The file names no network, as those of an earlier hw: prepare the transaction again"

instance ToJSON TxFile where
  toJSON (TxFile netM tsd) = object (maybe [] (\net -> ["network" .= net.name]) netM <> txSignDataPairs tsd)

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

-- | The hash hw shows for a transaction in preparation: its nosigHash until
-- it is signed, then its id, which the network and the explorers know. The
-- commands take either hash.
shownHash :: Tx -> TxHash
shownHash tx = if isSigned tx then txHash tx else nosigHash tx
