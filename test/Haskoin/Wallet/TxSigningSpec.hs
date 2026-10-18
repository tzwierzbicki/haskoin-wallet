{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.TxSigningSpec where

import Control.Monad (forM_)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.FileIO (TxSignData (..))
import Haskoin.Wallet.Signing
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSigning (signWalletTx)
import Numeric.Natural (Natural)
import Test.Hspec

-- The test vectors live in JSON files in this directory (see the README.md
-- file there for the format and the origin of the vectors). Every .json file
-- is loaded, so new vectors can be added without changing this module.
vectorDir :: FilePath
vectorDir = "test/data/tx-signing"

spec :: Spec
spec =
  -- A second, separately randomized context: signing with both must give the
  -- same bytes (RFC6979 nonces do not depend on the context randomization).
  prepareContext $ \ctx ->
    prepareContext $ \ctx2 -> do
      files <- runIO $ loadVectorFiles vectorDir
      describe "Transaction signing test vectors" $
        forM_ files $ vectorFileSpec (length . (.fileVectors)) (fileProblems ctx ctx2)

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
    summaryFee :: !Natural
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

{- Tests -}

fileProblems :: Ctx -> Ctx -> VectorFile -> Problems
fileProblems ctx ctx2 = concatMap (vectorProblems ctx ctx2) . (.fileVectors)

vectorProblems :: Ctx -> Ctx -> Vector -> Problems
vectorProblems ctx ctx2 v =
  named v.vecName $ reportSetupError $ do
    signWith <- prefixError "key" $ vectorSigner ctx v
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

-- | How hw signs the vector: with the account key, like signtx does, or with
-- the sweep keys and the account public key, like signsweep does. The keys
-- are derived once; the returned function signs with any context.
vectorSigner ::
  Ctx -> Vector -> Either String (Ctx -> Either String (TxSignData, TxInfo))
vectorSigner ctx v = do
  accountKey <- accountSigningKey v.vecNetwork ctx v.vecAccount
  case v.vecSweepKeys of
    Nothing ->
      return $ \c -> signWalletTx v.vecNetwork c v.vecSignData accountKey
    Just specs -> do
      keys <- mapM (vectorKey ctx v.vecNetwork) specs
      let accountPub = deriveXPubKey ctx accountKey
      return $ \c -> signTxWithKeys v.vecNetwork c v.vecSignData accountPub keys

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
