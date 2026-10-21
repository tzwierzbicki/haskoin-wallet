{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The database format of version 2, written by hw 0.9.0 to 0.9.4, and the
-- step that brings a database of version 1 (hw 0.8) to it.
--
-- The step works on the tables of version 2 ("Haskoin.Wallet.Schema.V2")
-- and on the transaction summaries as hw 0.9.4 wrote them, copied word for
-- word from src/Haskoin/Wallet/TxInfo.hs at the tag v0.9.4 and kept here.
-- It reads the summaries of version 1, whose types are kept here too, and
-- writes exactly what hw 0.9 wrote. Nothing here follows the live code: the
-- step to the next version always starts from a database of hw 0.9,
-- whatever version hw is at.
module Haskoin.Wallet.Migration.V2 where

import Conduit (MonadUnliftIO)
import Control.Monad (forM_, void)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson (object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Schema.V2
import Haskoin.Wallet.TxInfo (mapAddrText, mapTextAddr, marshalMap, unmarshalMap)
import Numeric.Natural (Natural)

{- The transaction summaries of version 2 -}

data TxType = TxDebit | TxInternal | TxCredit
  deriving (Show, Eq)

instance Json.ToJSON TxType where
  toJSON =
    Json.String . \case
      TxDebit -> "debit"
      TxInternal -> "internal"
      TxCredit -> "credit"

instance Json.FromJSON TxType where
  parseJSON =
    Json.withText "txtype" $ \case
      "debit" -> return TxDebit
      "internal" -> return TxInternal
      "credit" -> return TxCredit
      _ -> fail "Invalid TxType"

data TxInfo = TxInfo
  { txInfoHash :: !(Maybe TxHash),
    txInfoType :: !TxType,
    txInfoAmount :: !Integer,
    txInfoMyOutputs :: !(Map Address MyOutputs),
    txInfoOtherOutputs :: !(Map Address Natural),
    txInfoNonStdOutputs :: ![Store.StoreOutput],
    txInfoMyInputs :: !(Map Address MyInputs),
    txInfoOtherInputs :: !(Map Address OtherInputs),
    txInfoNonStdInputs :: ![Store.StoreInput],
    txInfoSize :: !Natural,
    txInfoFee :: !Natural,
    txInfoFeeByte :: !Natural,
    txInfoBlockRef :: !Store.BlockRef,
    txInfoConfirmations :: !Natural,
    txInfoPending :: !(Maybe TxInfoPending)
  }
  deriving (Eq, Show)

data TxInfoPending = TxInfoPending
  { pendingNosigHash :: !TxHash,
    pendingSigned :: !Bool,
    pendingOnline :: !Bool
  }
  deriving (Eq, Show)

instance Json.ToJSON TxInfoPending where
  toJSON (TxInfoPending h s o) =
    object
      [ "nosighash" .= h,
        "signed" .= s,
        "online" .= o
      ]

instance Json.FromJSON TxInfoPending where
  parseJSON =
    withObject "TxInfoPending" $ \o -> do
      h <- o .: "nosighash"
      s <- o .: "signed"
      onl <- o .: "online"
      return $ TxInfoPending h s onl

data MyOutputs = MyOutputs
  { myOutputsValue :: !Natural,
    myOutputsPath :: !SoftPath,
    myOutputsLabel :: !Text
  }
  deriving (Eq, Show)

instance Json.ToJSON MyOutputs where
  toJSON (MyOutputs i p l) =
    object
      [ "value" .= i,
        "path" .= p,
        "label" .= l
      ]

instance Json.FromJSON MyOutputs where
  parseJSON =
    withObject "MyOutputs" $ \o -> do
      i <- o .: "value"
      p <- o .: "path"
      l <- o .: "label"
      return $ MyOutputs i p l

data MyInputs = MyInputs
  { myInputsValue :: !Natural,
    myInputsPath :: !SoftPath,
    myInputsLabel :: !Text,
    myInputsSigInput :: [SigInput]
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx MyInputs where
  marshalValue ctx (MyInputs i p l s) =
    object $
      [ "value" .= i,
        "path" .= p,
        "label" .= l
      ]
        ++ ["siginput" .= (marshalValue ctx <$> s) | not (null s)]

  unmarshalValue ctx =
    withObject "MyInputs" $ \o -> do
      i <- o .: "value"
      p <- o .: "path"
      l <- o .: "label"
      sM <- o .:? "siginput"
      s <- unmarshalValue ctx `mapM` fromMaybe [] sM
      return $ MyInputs i p l s

data OtherInputs = OtherInputs
  { otherInputsValue :: !Natural,
    otherInputsSigInput :: [SigInput]
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx OtherInputs where
  marshalValue ctx (OtherInputs i s) =
    object $
      ("value" .= i)
        : ["siginput" .= (marshalValue ctx <$> s) | not (null s)]

  unmarshalValue ctx =
    withObject "OtherInputs" $ \o -> do
      i <- o .: "value"
      sM <- o .:? "siginput"
      s <- unmarshalValue ctx `mapM` fromMaybe [] sM
      return $ OtherInputs i s

instance MarshalJSON (Network, Ctx) TxInfo where
  marshalValue (net, ctx) tx =
    object
      [ "txid" .= txInfoHash tx,
        "type" .= txInfoType tx,
        "amount" .= txInfoAmount tx,
        "myoutputs" .= mapAddrText net (txInfoMyOutputs tx),
        "otheroutputs" .= mapAddrText net (txInfoOtherOutputs tx),
        "nonstdoutputs" .= (marshalValue net <$> txInfoNonStdOutputs tx),
        "myinputs" .= marshalMap net ctx (txInfoMyInputs tx),
        "otherinputs" .= marshalMap net ctx (txInfoOtherInputs tx),
        "nonstdinputs" .= (marshalValue net <$> txInfoNonStdInputs tx),
        "size" .= txInfoSize tx,
        "fee" .= txInfoFee tx,
        "feebyte" .= txInfoFeeByte tx,
        "block" .= txInfoBlockRef tx,
        "confirmations" .= txInfoConfirmations tx,
        "pending" .= txInfoPending tx
      ]
  unmarshalValue (net, ctx) =
    Json.withObject "TxInfo" $ \o ->
      TxInfo
        <$> o .: "txid"
        <*> o .: "type"
        <*> o .: "amount"
        <*> (mapTextAddr net <$> o .: "myoutputs")
        <*> (mapTextAddr net <$> o .: "otheroutputs")
        <*> (mapM (unmarshalValue net) =<< o .: "nonstdoutputs")
        <*> (unmarshalMap net ctx =<< o .: "myinputs")
        <*> (unmarshalMap net ctx =<< o .: "otherinputs")
        <*> (mapM (unmarshalValue net) =<< o .: "nonstdinputs")
        <*> o .: "size"
        <*> o .: "fee"
        <*> o .: "feebyte"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "pending"

{- The transaction summaries of version 1 (hw 0.8) -}

data OldTxInfo = OldTxInfo
  { oldTxInfoHash :: !TxHash,
    oldTxInfoType :: !TxType,
    oldTxInfoAmount :: !Integer,
    oldTxInfoMyOutputs :: !(Map Address OldMyOutputs),
    oldTxInfoOtherOutputs :: !(Map Address Natural),
    oldTxInfoNonStdOutputs :: ![Store.StoreOutput],
    oldTxInfoMyInputs :: !(Map Address OldMyInputs),
    oldTxInfoOtherInputs :: !(Map Address OtherInputs),
    oldTxInfoNonStdInputs :: ![Store.StoreInput],
    oldTxInfoSize :: !Natural,
    oldTxInfoFee :: !Natural,
    oldTxInfoFeeByte :: !Natural,
    oldTxInfoBlockRef :: !Store.BlockRef,
    oldTxInfoConfirmations :: !Natural
  }
  deriving (Eq, Show)

data OldMyOutputs = OldMyOutputs
  { oldMyOutputsValue :: !Natural,
    oldMyOutputsPath :: !SoftPath
  }
  deriving (Eq, Show)

instance Json.ToJSON OldMyOutputs where
  toJSON (OldMyOutputs i p) =
    object
      [ "value" .= i,
        "path" .= p
      ]

instance Json.FromJSON OldMyOutputs where
  parseJSON =
    withObject "MyOutputs" $ \o -> do
      i <- o .: "value"
      p <- o .: "path"
      return $ OldMyOutputs i p

data OldMyInputs = OldMyInputs
  { oldMyInputsValue :: !Natural,
    oldMyInputsPath :: !SoftPath,
    oldMyInputsSigInput :: [SigInput]
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx OldMyInputs where
  marshalValue ctx (OldMyInputs i p s) =
    object $
      [ "value" .= i,
        "path" .= p
      ]
        ++ ["siginput" .= (marshalValue ctx <$> s) | not (null s)]

  unmarshalValue ctx =
    withObject "MyInputs" $ \o -> do
      i <- o .: "value"
      p <- o .: "path"
      sM <- o .:? "siginput"
      s <- unmarshalValue ctx `mapM` fromMaybe [] sM
      return $ OldMyInputs i p s

instance MarshalJSON (Network, Ctx) OldTxInfo where
  marshalValue (net, ctx) tx =
    object
      [ "txid" .= oldTxInfoHash tx,
        "type" .= oldTxInfoType tx,
        "amount" .= oldTxInfoAmount tx,
        "myoutputs" .= mapAddrText net (oldTxInfoMyOutputs tx),
        "otheroutputs" .= mapAddrText net (oldTxInfoOtherOutputs tx),
        "nonstdoutputs" .= (marshalValue net <$> oldTxInfoNonStdOutputs tx),
        "myinputs" .= marshalMap net ctx (oldTxInfoMyInputs tx),
        "otherinputs" .= marshalMap net ctx (oldTxInfoOtherInputs tx),
        "nonstdinputs" .= (marshalValue net <$> oldTxInfoNonStdInputs tx),
        "size" .= oldTxInfoSize tx,
        "fee" .= oldTxInfoFee tx,
        "feebyte" .= oldTxInfoFeeByte tx,
        "block" .= oldTxInfoBlockRef tx,
        "confirmations" .= oldTxInfoConfirmations tx
      ]
  unmarshalValue (net, ctx) =
    Json.withObject "TxInfo" $ \o ->
      OldTxInfo
        <$> o .: "txid"
        <*> o .: "type"
        <*> o .: "amount"
        <*> (mapTextAddr net <$> o .: "myoutputs")
        <*> (mapTextAddr net <$> o .: "otheroutputs")
        <*> (mapM (unmarshalValue net) =<< o .: "nonstdoutputs")
        <*> (unmarshalMap net ctx =<< o .: "myinputs")
        <*> (unmarshalMap net ctx =<< o .: "otherinputs")
        <*> (mapM (unmarshalValue net) =<< o .: "nonstdinputs")
        <*> o .: "size"
        <*> o .: "fee"
        <*> o .: "feebyte"
        <*> o .: "block"
        <*> o .: "confirmations"

migrateMyOutputs :: OldMyOutputs -> MyOutputs
migrateMyOutputs OldMyOutputs {..} =
  MyOutputs
    { myOutputsValue = oldMyOutputsValue,
      myOutputsPath = oldMyOutputsPath,
      myOutputsLabel = ""
    }

migrateMyInputs :: OldMyInputs -> MyInputs
migrateMyInputs OldMyInputs {..} =
  MyInputs
    { myInputsValue = oldMyInputsValue,
      myInputsPath = oldMyInputsPath,
      myInputsLabel = "",
      myInputsSigInput = oldMyInputsSigInput
    }

migrateTxInfo :: OldTxInfo -> TxInfo
migrateTxInfo OldTxInfo {..} =
  TxInfo
    { txInfoHash = Just oldTxInfoHash,
      txInfoType = oldTxInfoType,
      txInfoAmount = oldTxInfoAmount,
      txInfoMyOutputs = migrateMyOutputs <$> oldTxInfoMyOutputs,
      txInfoOtherOutputs = oldTxInfoOtherOutputs,
      txInfoNonStdOutputs = oldTxInfoNonStdOutputs,
      txInfoMyInputs = migrateMyInputs <$> oldTxInfoMyInputs,
      txInfoOtherInputs = oldTxInfoOtherInputs,
      txInfoNonStdInputs = oldTxInfoNonStdInputs,
      txInfoSize = oldTxInfoSize,
      txInfoFee = oldTxInfoFee,
      txInfoFeeByte = oldTxInfoFeeByte,
      txInfoBlockRef = oldTxInfoBlockRef,
      txInfoConfirmations = oldTxInfoConfirmations,
      txInfoPending = Nothing
    }

{- The step -}

-- | hw 0.9 identified the database format by its own version
versionLabel :: Text
versionLabel = "0.9"

-- | Bring a database of version 1 to version 2: rewrite every transaction
-- summary in the format of hw 0.9, create the tables hw 0.8 did not have,
-- the version table among them, and store the version as hw 0.9 did
migrateDB :: (MonadUnliftIO m) => Ctx -> ExceptT String (DB m) ()
migrateDB ctx = do
  accs <- lift . select $ from return
  forM_ accs $ \(Entity _ acc) -> do
    net <- accountNetwork acc
    res <- lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccountWallet ==. val (dBAccountWallet acc)
          &&. t ^. DBTxInfoAccountDerivation ==. val (dBAccountDerivation acc)
      return $ t ^. DBTxInfoId
    forM_ res $ \(Value tKey) -> do
      oldDBTx <- liftMaybe "OldTxInfo" =<< lift (P.get tKey)
      oldTxInfo <-
        liftMaybe "OldTxInfo" $
          unmarshalJSON (net, ctx) $
            BS.fromStrict $
              dBTxInfoBlob oldDBTx
      let newTxInfo = migrateTxInfo oldTxInfo
          newBlob = BS.toStrict $ marshalJSON (net, ctx) newTxInfo
      lift $ P.update tKey [DBTxInfoBlob P.=. newBlob]
  lift $ do
    void $ runMigrationQuiet migrateAll
    P.insert_ $ DBVersion versionLabel

accountNetwork :: (Monad m) => DBAccount -> ExceptT String m Network
accountNetwork acc =
  liftMaybe ("Unknown network " <> cs (dBAccountNetwork acc)) $
    netByName $
      cs $
        dBAccountNetwork acc
