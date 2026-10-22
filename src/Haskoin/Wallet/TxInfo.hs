{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.Wallet.TxInfo where

import Data.Aeson (object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Json
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

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
    -- | What the fee includes beyond the requested fee rate: change that was
    -- not worth an output. Nothing when the requested rate is unknown.
    txInfoChangeInFee :: !(Maybe Natural),
    txInfoBlockRef :: !Store.BlockRef,
    txInfoConfirmations :: !Natural,
    txInfoPending :: !(Maybe TxInfoPending),
    -- | The transaction is no longer valid: replaced, or dropped from the
    -- network. Its summary is kept, so that the history shows what happened.
    txInfoDead :: !Bool
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
        "changeinfee" .= txInfoChangeInFee tx,
        "block" .= txInfoBlockRef tx,
        "confirmations" .= txInfoConfirmations tx,
        "pending" .= txInfoPending tx,
        "dead" .= txInfoDead tx
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
        <*> o .:? "changeinfee"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "pending"
        <*> o .:? "dead" .!= False


storeToTxInfo ::
  Map Address SoftPath -> Natural -> Store.Transaction -> TxInfo
storeToTxInfo walletAddrs currHeight sTx =
  TxInfo
    { txInfoHash = Just sTx.txid,
      txInfoType = txType amount fee,
      txInfoAmount = amount,
      txInfoMyOutputs = Map.map (\(v, p) -> MyOutputs v p "") myOutputsMap,
      txInfoOtherOutputs = othOutputsMap,
      txInfoNonStdOutputs = nonStdOut,
      txInfoMyInputs = Map.map (\(v, p) -> MyInputs v p "" []) myInputsMap,
      txInfoOtherInputs = Map.map (`OtherInputs` []) othInputsMap,
      txInfoNonStdInputs = nonStdIn,
      txInfoSize = size,
      txInfoFee = fee,
      txInfoFeeByte = feeByte,
      txInfoChangeInFee = Nothing,
      txInfoBlockRef = sTx.block,
      txInfoConfirmations = fromMaybe 0 confM,
      txInfoPending = Nothing,
      txInfoDead = False
    }
  where
    size = fromIntegral sTx.size :: Natural
    fee = fromIntegral sTx.fee :: Natural
    feeByte = fee `div` size
    (outputMap, nonStdOut) = outputAddressMap sTx.outputs
    myOutputsMap = Map.intersectionWith (,) outputMap walletAddrs
    othOutputsMap = Map.difference outputMap walletAddrs
    (inputMap, nonStdIn) = inputAddressMap sTx.inputs
    myInputsMap = Map.intersectionWith (,) inputMap walletAddrs
    othInputsMap = Map.difference inputMap walletAddrs
    myOutputsSum =
      fromIntegral $ sum $ fst <$> Map.elems myOutputsMap :: Integer
    myInputsSum = fromIntegral $ sum $ fst <$> Map.elems myInputsMap :: Integer
    amount = myOutputsSum - myInputsSum
    confM =
      case sTx.block of
        Store.MemRef _ -> Nothing
        Store.BlockRef height _ ->
          (+ 1) <$> currHeight `safeSubtract` fromIntegral height

{- Helpers for building address maps -}

outputAddressMap ::
  [Store.StoreOutput] -> (Map Address Natural, [Store.StoreOutput])
outputAddressMap outs =
  (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> outs
    f (Store.StoreOutput v _ _ (Just a)) = Right (a, fromIntegral v)
    f s = Left s

inputAddressMap ::
  [Store.StoreInput] -> (Map Address Natural, [Store.StoreInput])
inputAddressMap ins =
  (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> ins
    f (Store.StoreInput _ _ _ _ v _ (Just a)) = Right (a, fromIntegral v)
    f s = Left s

txType :: Integer -> Natural -> TxType
txType amount fee
  | amount > 0 = TxCredit
  | abs amount == fromIntegral fee = TxInternal
  | otherwise = TxDebit

