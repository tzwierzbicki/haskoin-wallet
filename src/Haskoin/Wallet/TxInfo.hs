{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The transaction summaries of the wallet: what a transaction means to
-- an account, with its amounts and the inputs and outputs that are the
-- wallet's, their JSON form for the files and the output of hw, and their
-- rows in the database, which the sync keeps up to date.
module Haskoin.Wallet.TxInfo where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Serialize as S
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Database
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

-- | Whether the wallet spends or receives anything in the transaction
involvesWallet :: TxInfo -> Bool
involvesWallet txInfo = not (Map.null (txInfoMyInputs txInfo) && Map.null (txInfoMyOutputs txInfo))

data TxInfoPending = TxInfoPending
  { pendingNosigHash :: !TxHash,
    pendingSigned :: !Bool,
    -- | Sent to the network by the user
    pendingOnline :: !Bool,
    -- | A coin of the wallet it spends was spent by another transaction,
    -- so it can no longer be sent
    pendingConflicted :: !Bool
  }
  deriving (Eq, Show)

instance Json.ToJSON TxInfoPending where
  toJSON (TxInfoPending h s o c) =
    object
      [ "nosighash" .= h,
        "signed" .= s,
        "online" .= o,
        "conflicted" .= c
      ]

instance Json.FromJSON TxInfoPending where
  parseJSON =
    withObject "TxInfoPending" $ \o -> do
      h <- o .: "nosighash"
      s <- o .: "signed"
      onl <- o .: "online"
      c <- o .:? "conflicted" .!= False
      return $ TxInfoPending h s onl c

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

{- Summaries in the database -}

-- | The block of every live transaction summary of an account, by
-- transaction id. The dead ones are left out: the sync treats them as
-- unknown, so that a transaction the store lists again is fetched and comes
-- back to life.
txInfoBlockRefs ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) (Map TxHash Store.BlockRef)
txInfoBlockRefs (DBAccountKey wallet accDeriv) = do
  rows <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccountWallet ==. val wallet
          &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
          &&. t ^. DBTxInfoDead ==. val False
      return (t ^. DBTxInfoTxid, t ^. DBTxInfoBlockRef)
  entries <- forM rows $ \(Value tidT, Value refBytes) -> do
    tid <- liftEither $ maybeToEither "txInfoBlockRefs: invalid txid" $ hexToTxHash tidT
    ref <- liftEither $ S.decode refBytes
    return (tid, ref)
  return $ Map.fromList entries

-- | Mark the summary of a transaction dead: the store no longer has it, or
-- reports it replaced or dropped. The summary stays, for the history.
markTxInfoDead :: (MonadUnliftIO m) => DBAccountId -> TxHash -> DB m ()
markTxInfoDead (DBAccountKey wallet accDeriv) tid =
  P.update (DBTxInfoKey wallet accDeriv (txHashToHex tid)) [DBTxInfoDead P.=. True]

getConfirmedTxs ::
  (MonadUnliftIO m) => DBAccountId -> Bool -> ExceptT String (DB m) [TxHash]
getConfirmedTxs (DBAccountKey wallet accDeriv) confirm = do
  ts <-
    lift . select $
      from $ \t -> do
        where_ $
          t ^. DBTxInfoAccountWallet ==. val wallet
            &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
            &&. t ^. DBTxInfoConfirmed ==. val confirm
        orderBy [asc (t ^. DBTxInfoBlockRef)]
        return $ t ^. DBTxInfoTxid
  forM ts $ \(Value t) ->
    liftEither $ maybeToEither "getConfirmedTxs invalid TxHash" $ hexToTxHash t

-- Insert a new transaction or replace it, if it already exists
-- Returns True if there was a change or an insert
repsertTxInfo ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxInfo ->
  ExceptT String (DB m) (DBTxInfo, Bool)
repsertTxInfo net ctx accId txInfo = do
  time <- liftIO getCurrentTime
  tid <- liftEither $ maybeToEither "TxId" $ txInfoHash txInfo
  let confirmed' = Store.confirmed $ txInfoBlockRef txInfo
      tidT = txHashToHex tid
      bRef = S.encode $ txInfoBlockRef txInfo
      -- Confirmations will get updated when retrieving them
      blob =
        BS.toStrict $
          marshalJSON
            (net, ctx)
            txInfo
              { txInfoConfirmations = 0,
                txInfoPending = Nothing,
                txInfoDead = False
              }
      (DBAccountKey wallet accDeriv) = accId
      key = DBTxInfoKey wallet accDeriv tidT
      dbInfo =
        DBTxInfo
          { dBTxInfoAccountWallet = wallet,
            dBTxInfoAccountDerivation = accDeriv,
            dBTxInfoTxid = tidT,
            dBTxInfoBlockRef = bRef,
            dBTxInfoConfirmed = confirmed',
            dBTxInfoBlob = blob,
            dBTxInfoDead = False,
            dBTxInfoCreated = time
          }
  prevM <- lift $ P.get key
  case prevM of
    Just prev -> do
      -- A live transaction from the store: a summary marked dead comes back
      let newDBInfo =
            prev
              { dBTxInfoBlockRef = bRef,
                dBTxInfoConfirmed = confirmed',
                dBTxInfoBlob = blob,
                dBTxInfoDead = False
              }
      lift $ P.replace key newDBInfo
      return (newDBInfo, prev /= newDBInfo)
    Nothing -> do
      lift $ P.insert_ dbInfo
      return (dbInfo, True)

txsPage ::
  (MonadUnliftIO m) =>
  Ctx ->
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [TxInfo]
txsPage ctx accId@(DBAccountKey wallet accDeriv) (Page lim off) = do
  acc <- getAccountById accId
  let net = accountNetwork acc
  dbTxs <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccountWallet ==. val wallet
          &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
      orderBy [asc (t ^. DBTxInfoBlockRef)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return (t ^. DBTxInfoBlob, t ^. DBTxInfoDead)
  res <-
    forM dbTxs $ \(Value dbTx, Value dead) -> do
      txInfo <-
        liftMaybe "TxInfo unmarshalJSON Failed" $
          unmarshalJSON (net, ctx) $
            BS.fromStrict dbTx
      return txInfo {txInfoDead = dead}
  resLabels <- lift $ mapM (fillTxInfoLabels net) res
  bestM <- lift $ getBest net
  return $ updateConfirmations (snd <$> bestM) <$> resLabels
  where
    updateConfirmations bestM tif =
      tif {txInfoConfirmations = getConfirmations bestM (txInfoBlockRef tif)}

fillTxInfoLabels :: (MonadUnliftIO m) => Network -> TxInfo -> DB m TxInfo
fillTxInfoLabels net txInfo = do
  o <- mapM fillOutput $ Map.assocs $ txInfoMyOutputs txInfo
  i <- mapM fillInput $ Map.assocs $ txInfoMyInputs txInfo
  return $
    txInfo
      { txInfoMyOutputs = Map.fromList o,
        txInfoMyInputs = Map.fromList i
      }
  where
    fillOutput (a, o) = do
      resM <- runMaybeT $ do
        addrT <- hoistMaybe $ addrToText net a
        (Entity _ dbAddr) <- hoistMaybe =<< lift (P.getBy $ UniqueAddress addrT)
        return (a, o {myOutputsLabel = dBAddressLabel dbAddr})
      return $ fromMaybe (a, o) resM
    fillInput (a, i) = do
      resM <- runMaybeT $ do
        addrT <- hoistMaybe $ addrToText net a
        (Entity _ dbAddr) <- hoistMaybe =<< lift (P.getBy $ UniqueAddress addrT)
        return (a, i {myInputsLabel = dBAddressLabel dbAddr})
      return $ fromMaybe (a, i) resM
