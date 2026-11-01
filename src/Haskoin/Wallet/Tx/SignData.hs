{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The signing data of a transaction: the transaction, the transactions it
-- spends from, which say what each input is worth, the derivation paths of
-- the wallet's own inputs and outputs, the fee rate it was built for, the
-- fingerprint of the seed once it has been signed, and the label the user
-- gave it. The builders produce it, the signers sign it, and the commands
-- write it to files and keep it
-- among the pending transactions of the database, as JSON. Whether the
-- transaction is signed is read from the transaction itself. Before and
-- after signing, the database keys it by its 'nosigHash'. hw shows it by
-- its 'shownHash'. A file holds it with the network of the transaction and
-- the labels of the account's addresses in it, by their paths, a 'TxFile'.
module Haskoin.Wallet.Tx.SignData where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Util (parsePathLabels, parseTxidLabels, pathLabelsJson, txidLabelsJson)
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
    txSignDataReplaces :: ![TxHash],
    -- | The label the user gave the transaction, or none. Nothing signs
    -- it: it is the user's note, which goes with the transaction to its
    -- file, and to the wallet's history once the network has it.
    txSignDataLabel :: !Text
  }
  deriving (Eq, Show)

-- | The label of a transaction the user has not labelled, as the builders
-- make it
noLabel :: Text
noLabel = Text.empty

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
        <*> o .:? "label" .!= ""

instance ToJSON TxSignData where
  toJSON = object . txSignDataPairs

-- | The keys of signing data in JSON. The transactions replaced are
-- written only for a replacement, and the label only when there is one, so
-- that the file of any other transaction is as an earlier hw wrote it.
txSignDataPairs :: TxSignData -> [Pair]
txSignDataPairs (TxSignData t i oi op r f rs l) =
  [ "tx" .= encodeHex (S.encode t),
    "txinputs" .= (encodeHex . S.encode <$> i),
    "inputpaths" .= oi,
    "outputpaths" .= op,
    "feerate" .= r,
    "fingerprint" .= f
  ]
    <> ["replaces" .= rs | not (null rs)]
    <> ["label" .= l | not (Text.null l)]

instance MarshalJSON Ctx TxSignData where
  marshalValue _ = toJSON
  unmarshalValue _ = parseJSON

{- Files -}

-- | The labels of the account's addresses, by their paths in the account,
-- such as /0/5: the paths the signing data gives for the account's inputs
-- and outputs. A label can only name a path, so it can only land on an
-- address the reader derives itself from the account's key, never on
-- another wallet's. Only the addresses that have a label are listed.
type PathLabels = Map SoftPath Text

-- | A transaction file, as preparetx, preparesweep, signtx, signsweep and
-- exporttx write it: the signing data, with the network of the transaction,
-- which the transaction itself does not tell, and the labels the wallet
-- that wrote the file had: those of the account's addresses the
-- transaction spends from or pays, by their paths, and those the coins it
-- spends show, by the transaction that paid each (see
-- 'Haskoin.Wallet.Database.TxInfo.coinLabel'). An offline computer has no
-- labels of its own, so it shows those of the file. A file of an earlier
-- hw names no network and carries no label.
data TxFile = TxFile
  { txFileNetwork :: !(Maybe Network),
    txFileData :: !TxSignData,
    txFilePathLabels :: !PathLabels,
    txFileInputLabels :: !(Map TxHash Text)
  }
  deriving (Eq, Show)

-- | A file that carries no label, as a pending transaction given by its
-- hash is read, and as an earlier hw wrote every file
plainTxFile :: Maybe Network -> TxSignData -> TxFile
plainTxFile netM tsd = TxFile netM tsd Map.empty Map.empty

-- The network and the labels are keys among those of the signing data,
-- which an earlier hw leaves aside
instance FromJSON TxFile where
  parseJSON value =
    TxFile
      <$> withObject "txfile" network value
      <*> parseJSON value
      <*> withObject "txfile" (\o -> maybe (return Map.empty) parsePathLabels =<< o .:? "pathlabels") value
      <*> withObject "txfile" (\o -> maybe (return Map.empty) parseTxidLabels =<< o .:? "inputlabels") value
    where
      network o = do
        nameM <- o .:? "network"
        forM nameM $ \name -> maybe (fail ("Unknown network " <> name)) return (netByName name)

-- | The refusal of a file of an earlier hw, which names no network, where
-- nothing else tells the network of its transaction: such a file is cheap
-- to prepare again
noFileNetwork :: String
noFileNetwork = "The file names no network, as those of an earlier hw: prepare the transaction again"

-- The labels are written only when there are some, so that the file of a
-- transaction without any is as an earlier hw wrote it
instance ToJSON TxFile where
  toJSON (TxFile netM tsd paths inputs) =
    object $
      maybe [] (\net -> ["network" .= net.name]) netM
        <> txSignDataPairs tsd
        <> ["pathlabels" .= pathLabelsJson paths | not (Map.null paths)]
        <> ["inputlabels" .= txidLabelsJson inputs | not (Map.null inputs)]

{- Signatures -}

-- | Whether every input of the transaction carries a signature: a script,
-- or witness data for a SegWit coin. Only their presence is checked here;
-- "Haskoin.Wallet.Tx.Signing" verifies the signatures themselves.
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
