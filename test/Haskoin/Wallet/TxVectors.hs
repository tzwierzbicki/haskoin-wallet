{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Transaction signing test vectors, shared by "Haskoin.Wallet.TxSigningSpec"
-- (wallet transactions, signed with the account key) and
-- "Haskoin.Wallet.TxSweepSpec" (sweeps, signed with external keys). The
-- vectors live in JSON files (see test/data/tx-signing/README.md for the
-- format); a spec gives the directory and the signer to use.
module Haskoin.Wallet.TxVectors where

import Control.Monad (forM_)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Numeric.Natural (Natural)
import Test.Hspec

-- | How a spec signs a vector. The keys are derived once, with the first
-- context (a failure is a setup error); the returned function signs with any
-- context, so that a vector can be signed with two contexts.
type VectorSigner =
  Ctx -> Vector -> Either String (Ctx -> Either String (TxSignData, TxInfo))

-- | One check per vector file of the directory, signing with the given signer
txVectorSpec :: FilePath -> String -> VectorSigner -> Spec
txVectorSpec dir title signer =
  -- A second, separately randomized context: signing with both must give the
  -- same bytes (RFC6979 nonces do not depend on the context randomization).
  prepareContext $ \ctx ->
    prepareContext $ \ctx2 -> do
      files <- runIO (loadVectorFiles dir :: IO [(FilePath, Either String VectorFile)])
      describe title $
        forM_ files $ vectorFileSpec vectorCount (fileProblems signer ctx ctx2)

vectorCount :: VectorFile -> Int
vectorCount = length . (.fileVectors)

fileProblems :: VectorSigner -> Ctx -> Ctx -> VectorFile -> Problems
fileProblems signer ctx ctx2 = concatMap (vectorProblems signer ctx ctx2) . (.fileVectors)

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
    signedSummary :: !(Maybe Summary)
  }

data Summary = Summary
  { summaryType :: !TxType,
    summaryAmount :: !Integer,
    summaryFee :: !Natural,
    -- | The size of the signed transaction and the fee rate reported with it
    summarySize :: !Natural,
    summaryFeeByte :: !Natural
  }

instance FromJSON Vector where
  parseJSON =
    withObject "Vector" $ \o -> do
      expect <-
        (o .:? "expect" .!= "match") >>= \case
          "match" -> ExpectMatch <$> o .: "signed"
          "error" -> ExpectError <$> o .: "error"
          e -> fail $ "Invalid expect value: " <> Text.unpack e
      Vector
        <$> o .: "name"
        <*> (parseNetwork =<< o .: "network")
        <*> o .: "account"
        <*> o .:? "sweepKeys"
        <*> o .: "txSignData"
        <*> pure expect

instance FromJSON Signed where
  parseJSON =
    withObject "Signed" $ \o ->
      Signed
        <$> o .: "tx"
        <*> o .: "txid"
        <*> o .:? "txInfo"

instance FromJSON Summary where
  parseJSON =
    withObject "Summary" $ \o ->
      Summary
        <$> o .: "type"
        <*> o .: "amount"
        <*> o .: "fee"
        <*> o .: "size"
        <*> o .: "feeByte"

{- Checks -}

vectorProblems :: VectorSigner -> Ctx -> Ctx -> Vector -> Problems
vectorProblems signer ctx ctx2 v =
  named v.vecName $ reportSetupError $ do
    signWith <- signer ctx v
    return $
      case (v.vecExpect, signWith ctx) of
        (ExpectError err, result) ->
          check "result" (Left err) (txIdOf . fst <$> result)
        (ExpectMatch _, Left err) ->
          ["hw refused to sign: " <> err]
        (ExpectMatch expected, Right (signedData, info)) ->
          signedProblems expected signedData info
            <> require
              "the same transaction is produced with another context"
              (fmap fst (signWith ctx2) == Right signedData)
  where
    txIdOf = txHashToHex . txHash . (.txSignDataTx)

signedProblems :: Signed -> TxSignData -> TxInfo -> Problems
signedProblems expected signedData info =
  check "signed transaction" expected.signedTx (encodeHex $ S.encode tx)
    <> check "txid" expected.signedTxId (txHashToHex $ txHash tx)
    <> require "the transaction is marked as signed" signedData.txSignDataSigned
    <> maybe [] summaryProblems expected.signedSummary
  where
    tx = signedData.txSignDataTx
    summaryProblems :: Summary -> Problems
    summaryProblems s =
      check "transaction type" s.summaryType info.txInfoType
        <> check "amount" s.summaryAmount info.txInfoAmount
        <> check "fee" s.summaryFee info.txInfoFee
        <> check "size" s.summarySize info.txInfoSize
        <> check "fee rate" s.summaryFeeByte info.txInfoFeeByte
