{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.FileIO where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..))
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Numeric.Natural (Natural)
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (mapMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Util
import qualified System.Directory as D
import qualified System.IO as IO

data PubKeyDoc = PubKeyDoc
  { documentPubKey :: !XPubKey,
    documentNetwork :: !Network,
    documentName :: !Text,
    documentWallet :: !Fingerprint
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx PubKeyDoc where
  unmarshalValue ctx =
    withObject "pubkeydocument" $ \o -> do
      net <- maybe mzero return . netByName =<< o .: "network"
      PubKeyDoc
        <$> (unmarshalValue (net, ctx) =<< o .: "xpubkey")
        <*> return net
        <*> (o .: "name")
        <*> (o .: "wallet")

  marshalValue ctx (PubKeyDoc k net name wallet) =
    object
      [ "xpubkey" .= marshalValue (net, ctx) k,
        "network" .= net.name,
        "name" .= name,
        "wallet" .= wallet
      ]

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

checkPathFree :: (MonadIO m) => FilePath -> ExceptT String m ()
checkPathFree path = do
  exist <- liftIO $ D.doesPathExist path
  when exist $ throwError $ "Path '" <> path <> "' already exists"

-- JSON IO Helpers--

writeJsonFile :: FilePath -> Value -> IO ()
writeJsonFile filePath doc = C8.writeFile filePath $ encodeJsonPrettyLn doc

readJsonFile :: (FromJSON a) => FilePath -> IO (Either String a)
readJsonFile = eitherDecodeFileStrict'

writeMarshalFile :: (MarshalJSON s a) => s -> FilePath -> a -> IO ()
writeMarshalFile s filePath a = writeJsonFile filePath $ marshalValue s a

readMarshalFile :: (MarshalJSON s a) => s -> FilePath -> IO (Either String a)
readMarshalFile s filePath = do
  vE <- readJsonFile filePath
  return $ parseEither (unmarshalValue s) =<< vE

-- Parse wallet dump files for sweeping --

