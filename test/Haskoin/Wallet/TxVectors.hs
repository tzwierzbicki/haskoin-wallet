{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Transaction signing test vectors, shared by "Haskoin.Wallet.Tx.SigningSpec"
-- (wallet transactions, signed with the account key) and
-- "Haskoin.Wallet.Tx.SweepSpec" (sweeps, signed with external keys). The
-- vectors live in JSON files (see test/data/tx-signing/README.md for the
-- format); a spec gives the directory and the signer to use.
module Haskoin.Wallet.TxVectors where

import Control.Monad (forM_)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Keys (AccountKind)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.SignData
import Numeric.Natural (Natural)
import Test.Hspec

-- | How a spec signs a vector. The outer result is the setup: a key that
-- cannot be derived is a mistake in the vector, not a refusal of hw. The
-- inner result is what signing gave.
type VectorSigner =
  Vector -> Either String (Either String (TxSignData, TxInfo))

-- | One check per vector file of the directory, signing with the given
-- signer, and one that the files hold the vectors expected
txVectorSpec :: FilePath -> String -> [(FilePath, Int)] -> VectorSigner -> Spec
txVectorSpec dir title expected signer = do
  files <- runIO (loadVectorFiles dir :: IO [(FilePath, Either String VectorFile)])
  describe title $ do
    forM_ files $ vectorFileSpec vectorCount (fileProblems signer)
    vectorCountsSpec vectorCount expected files

vectorCount :: VectorFile -> Int
vectorCount = length . (.fileVectors)

fileProblems :: VectorSigner -> VectorFile -> Problems
fileProblems signer = concatMap (vectorProblems signer) . (.fileVectors)

{- Vector files -}

newtype VectorFile = VectorFile {fileVectors :: [Vector]}

instance FromJSON VectorFile where
  parseJSON = withObject "VectorFile" $ \o -> VectorFile <$> o .: "vectors"

data Vector = Vector
  { vecName :: !Text,
    vecNetwork :: !Network,
    -- | The wallet account: it signs the transaction, or only identifies the
    -- wallet's own inputs and outputs when sweep keys are given
    vecAccount :: !AccountSpec,
    -- | Private keys of a sweep transaction, signed like signsweep does
    vecSweepKeys :: !(Maybe [KeySpec]),
    vecSignData :: !TxSignData,
    vecExpect :: !Expect
  }

-- | What a vector expects from hw
data Expect
  = -- | hw must produce exactly this transaction
    ExpectMatch !Signed
  | -- | hw must refuse to sign, with this error
    ExpectError !String

data Signed = Signed
  { signedTx :: !Text,
    signedTxId :: !Text,
    -- | A summary of the TxInfo that hw reports with the signed transaction
    signedSummary :: !(Maybe Reported)
  }

data Reported = Reported
  { reportedType :: !TxType,
    reportedAmount :: !Integer,
    reportedFee :: !Natural,
    -- | The size of the signed transaction and the fee rate reported with it
    reportedSize :: !Natural,
    reportedFeeByte :: !Natural
  }

instance FromJSON Vector where
  parseJSON =
    withObject "Vector" $ \o -> do
      expect <-
        (o .:? "expect" .!= "match") >>= \case
          "match" -> ExpectMatch <$> o .: "signed"
          "error" -> ExpectError <$> o .: "error"
          e -> fail $ "Invalid expect value: " <> Text.unpack e
      account <- o .: "account"
      Vector
        <$> o .: "name"
        <*> (parseNetwork =<< o .: "network")
        <*> pure account
        <*> o .:? "sweepKeys"
        <*> (signDataOf (accountSpecKind account) =<< o .: "txSignData")
        <*> pure expect

-- | The signing data of a vector, with its paths in the account of the
-- vector. A vector has one account, and writes the paths of its own inputs
-- and outputs alone, relative to it, as "/0/3", which the tools of the
-- vectors read. hw writes the kind of the account with each path, since a
-- portfolio has two accounts, and reads no path without it. So each path
-- of a vector is given the kind of its account here, before hw reads the
-- signing data.
signDataOf :: AccountKind -> Json.Value -> Parser TxSignData
signDataOf kind = withObject "txSignData" $ \o -> parseJSON (Json.Object (KeyMap.mapWithKey withKind o))
  where
    withKind key value
      | key `elem` ["inputpaths", "outputpaths"], Json.Array paths <- value = Json.Array (ofAccount <$> paths)
      | otherwise = value
    ofAccount (Json.String path) = Json.object ["type" .= kind, "path" .= path]
    ofAccount other = other

instance FromJSON Signed where
  parseJSON =
    withObject "Signed" $ \o ->
      Signed
        <$> o .: "tx"
        <*> o .: "txid"
        <*> o .:? "txInfo"

instance FromJSON Reported where
  parseJSON =
    withObject "Reported" $ \o ->
      Reported
        <$> o .: "type"
        <*> o .: "amount"
        <*> o .: "fee"
        <*> o .: "size"
        <*> o .: "feeByte"

{- Checks -}

vectorProblems :: VectorSigner -> Vector -> Problems
vectorProblems signer v =
  named v.vecName $ reportSetupError $ do
    signed <- signer v
    return $
      case (v.vecExpect, signed) of
        (ExpectError err, result) ->
          check "result" (Left err) (txIdOf . fst <$> result)
        (ExpectMatch _, Left err) ->
          ["hw refused to sign: " <> err]
        (ExpectMatch expected, Right (signedData, info)) ->
          signedProblems expected signedData info
  where
    txIdOf = txHashToHex . txHash . (.txSignDataTx)

signedProblems :: Signed -> TxSignData -> TxInfo -> Problems
signedProblems expected signedData info =
  check "signed transaction" expected.signedTx (encodeHex $ S.encode tx)
    <> check "txid" expected.signedTxId (txHashToHex $ txHash tx)
    <> require "every input of the transaction is signed" (isSigned tx)
    <> maybe [] summaryProblems expected.signedSummary
  where
    tx = signedData.txSignDataTx
    summaryProblems :: Reported -> Problems
    summaryProblems s =
      check "transaction type" s.reportedType info.txInfoSummary.summaryType
        <> check "amount" s.reportedAmount info.txInfoSummary.summaryAmount
        <> check "fee" s.reportedFee info.txInfoSummary.summaryFee
        <> check "size" s.reportedSize info.txInfoSummary.summarySize
        <> check "fee rate" s.reportedFeeByte info.txInfoSummary.summaryFeeByte
