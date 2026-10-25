{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The transaction summaries of the wallet. A 'TxSummary' says what a
-- transaction means to an account: its type and amount, its fee, and the
-- inputs and outputs that are the account's. A 'TxStatus' says where the
-- transaction stands: on the chain, as the store reports it, or in
-- preparation, held among the pending transactions. The two together, a
-- 'TxInfo', are what hw prints and writes as JSON. The summaries of the
-- transactions on the chain are stored in the database, which the sync
-- keeps up to date.
module Haskoin.Wallet.TxInfo where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Aeson as Json
import Data.Aeson.Types (Pair, Parser)
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
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

{- Summaries -}

-- | What a transaction does to the account: a credit brings money in, a
-- debit sends money out, an internal transaction moves money between the
-- account's own addresses and costs its fee
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

-- | What a transaction means to an account
data TxSummary = TxSummary
  { summaryType :: !TxType,
    -- | What the account gains (positive) or spends (negative), fee included
    summaryAmount :: !Integer,
    summaryFee :: !Natural,
    -- | The fee per byte, rounded down
    summaryFeeByte :: !Natural,
    -- | What the fee includes beyond the fee rate the transaction was built
    -- for: change that was not worth an output. Nothing when that rate is
    -- unknown, as it is for the transactions the store reports.
    summaryChangeInFee :: !(Maybe Natural),
    -- | The size in bytes: of the signed transaction, or estimated before
    -- it is signed
    summarySize :: !Natural,
    -- | The outputs paying the account, by address
    summaryMyOutputs :: !(Map Address MyOutputs),
    -- | The outputs paying others, by address
    summaryOtherOutputs :: !(Map Address Natural),
    -- | The outputs that pay no address
    summaryNonStdOutputs :: ![Store.StoreOutput],
    -- | The inputs spending coins of the account, by address
    summaryMyInputs :: !(Map Address MyInputs),
    -- | The inputs spending coins of others, by address
    summaryOtherInputs :: !(Map Address Natural),
    -- | The inputs that spend no address, the coinbase among them
    summaryNonStdInputs :: ![Store.StoreInput]
  }
  deriving (Eq, Show)

-- | What an address of the account receives in a transaction
data MyOutputs = MyOutputs
  { myOutputsValue :: !Natural,
    myOutputsPath :: !SoftPath,
    myOutputsLabel :: !Text
  }
  deriving (Eq, Show)

-- | What an address of the account spends in a transaction, and which coins
data MyInputs = MyInputs
  { myInputsValue :: !Natural,
    myInputsPath :: !SoftPath,
    myInputsLabel :: !Text,
    myInputsOutpoints :: ![OutPoint]
  }
  deriving (Eq, Show)

-- | Whether the account spends or receives anything in the transaction
involvesWallet :: TxSummary -> Bool
involvesWallet summary = not (Map.null (summaryMyInputs summary) && Map.null (summaryMyOutputs summary))

-- | Whether the account made the transaction: every input spends a coin of
-- the account
ownTransaction :: TxSummary -> Bool
ownTransaction summary =
  not (Map.null (summaryMyInputs summary))
    && Map.null (summaryOtherInputs summary)
    && null (summaryNonStdInputs summary)

-- | The type of a transaction, from what the account gains or loses and
-- the fee: a loss of exactly the fee moved money between its own addresses
txType :: Integer -> Natural -> TxType
txType amount fee
  | amount > 0 = TxCredit
  | abs amount == fromIntegral fee = TxInternal
  | otherwise = TxDebit

{- Status -}

-- | Where a transaction stands
data TxStatus
  = -- | On the chain, or in its mempool, as the store reports it
    OnChain !ChainStatus
  | -- | In preparation: built, signed or not, sent or not, and held among
    -- the pending transactions of the account
    Pending !PendingStatus
  deriving (Eq, Show)

data ChainStatus = ChainStatus
  { chainTxid :: !TxHash,
    chainBlock :: !Store.BlockRef,
    chainConfirmations :: !Natural,
    -- | The transaction is no longer valid: replaced, or dropped from the
    -- network. Its summary is kept, so that the history shows what happened.
    chainDead :: !Bool
  }
  deriving (Eq, Show)

data PendingStatus = PendingStatus
  { -- | The hash of the transaction without its signatures, which
    -- identifies it before and after signing
    pendingNosigHash :: !TxHash,
    -- | The id of the transaction once it is signed
    pendingTxid :: !(Maybe TxHash),
    -- | Sent to the network by the user
    pendingOnline :: !Bool,
    -- | A coin it spends was spent by another transaction, or is gone, as
    -- the store answered at the last sync: it can no longer be sent
    pendingConflicted :: !Bool
  }
  deriving (Eq, Show)

-- | Whether a transaction in preparation is signed: a signed transaction
-- has its final id
pendingSigned :: PendingStatus -> Bool
pendingSigned = isJust . pendingTxid

-- | A transaction as hw reports it: what it means to the account, and
-- where it stands
data TxInfo = TxInfo
  { txInfoSummary :: !TxSummary,
    txInfoStatus :: !TxStatus
  }
  deriving (Eq, Show)

-- | The id of the transaction, when it has one: a transaction in
-- preparation has none until it is signed
txInfoHash :: TxInfo -> Maybe TxHash
txInfoHash info =
  case txInfoStatus info of
    OnChain status -> Just (chainTxid status)
    Pending status -> pendingTxid status

{- JSON -}

-- The summary and the status are written as one flat object, the same for a
-- transaction on the chain and one in preparation, and addresses are
-- written as the text of the network.

instance Json.ToJSON MyOutputs where
  toJSON (MyOutputs v p l) =
    object
      [ "value" .= v,
        "path" .= p,
        "label" .= l
      ]

instance Json.FromJSON MyOutputs where
  parseJSON =
    withObject "MyOutputs" $ \o ->
      MyOutputs
        <$> o .: "value"
        <*> o .: "path"
        <*> o .: "label"

instance Json.ToJSON MyInputs where
  toJSON (MyInputs v p l outpoints) =
    object
      [ "value" .= v,
        "path" .= p,
        "label" .= l,
        "outpoints" .= outpoints
      ]

instance Json.FromJSON MyInputs where
  parseJSON =
    withObject "MyInputs" $ \o ->
      MyInputs
        <$> o .: "value"
        <*> o .: "path"
        <*> o .: "label"
        -- The summaries stored by earlier versions of hw have no outpoints
        <*> o .:? "outpoints" .!= []

instance MarshalJSON Network TxSummary where
  marshalValue net = object . summaryPairs net
  unmarshalValue net = withObject "TxSummary" (summaryParser net)

instance MarshalJSON Network TxInfo where
  marshalValue net (TxInfo summary status) =
    object $ summaryPairs net summary <> statusPairs status
  unmarshalValue net =
    withObject "TxInfo" $ \o -> TxInfo <$> summaryParser net o <*> statusParser o

summaryPairs :: Network -> TxSummary -> [Pair]
summaryPairs net s =
  [ "type" .= summaryType s,
    "amount" .= summaryAmount s,
    "myoutputs" .= mapAddrText net (summaryMyOutputs s),
    "otheroutputs" .= mapAddrText net (summaryOtherOutputs s),
    "nonstdoutputs" .= (marshalValue net <$> summaryNonStdOutputs s),
    "myinputs" .= mapAddrText net (summaryMyInputs s),
    "otherinputs" .= mapAddrText net (summaryOtherInputs s),
    "nonstdinputs" .= (marshalValue net <$> summaryNonStdInputs s),
    "size" .= summarySize s,
    "fee" .= summaryFee s,
    "feebyte" .= summaryFeeByte s,
    "changeinfee" .= summaryChangeInFee s
  ]

summaryParser :: Network -> Json.Object -> Parser TxSummary
summaryParser net o =
  TxSummary
    <$> o .: "type"
    <*> o .: "amount"
    <*> o .: "fee"
    <*> o .: "feebyte"
    <*> o .:? "changeinfee"
    <*> o .: "size"
    <*> (mapTextAddr net =<< o .: "myoutputs")
    <*> (mapTextAddr net =<< o .: "otheroutputs")
    <*> (mapM (unmarshalValue net) =<< o .: "nonstdoutputs")
    <*> (mapTextAddr net =<< o .: "myinputs")
    <*> (mapTextAddr net =<< mapM otherInputValue =<< o .: "otherinputs")
    <*> (mapM (unmarshalValue net) =<< o .: "nonstdinputs")
  where
    -- The summaries stored by earlier versions of hw wrote an object per
    -- input of others, with its value
    otherInputValue :: Json.Value -> Parser Natural
    otherInputValue (Json.Object input) = input .: "value"
    otherInputValue value = parseJSON value

-- | The keys of a status. A transaction in preparation is written with the
-- block, confirmations and dead flag of one that is nowhere yet, so that
-- every transaction has the same keys.
statusPairs :: TxStatus -> [Pair]
statusPairs (OnChain status) =
  [ "txid" .= chainTxid status,
    "block" .= chainBlock status,
    "confirmations" .= chainConfirmations status,
    "dead" .= chainDead status,
    "pending" .= Json.Null
  ]
statusPairs (Pending status) =
  [ "txid" .= pendingTxid status,
    "block" .= Store.MemRef 0,
    "confirmations" .= (0 :: Natural),
    "dead" .= False,
    "pending"
      .= object
        [ "nosighash" .= pendingNosigHash status,
          "signed" .= pendingSigned status,
          "online" .= pendingOnline status,
          "conflicted" .= pendingConflicted status
        ]
  ]

statusParser :: Json.Object -> Parser TxStatus
statusParser o = do
  pendingM <- o .:? "pending"
  case pendingM of
    Just p ->
      fmap Pending $
        PendingStatus
          <$> p .: "nosighash"
          <*> o .:? "txid"
          <*> p .: "online"
          <*> p .:? "conflicted" .!= False
    Nothing ->
      fmap OnChain $
        ChainStatus
          <$> o .: "txid"
          <*> o .: "block"
          <*> o .:? "confirmations" .!= 0
          <*> o .:? "dead" .!= False

{- From the store's transactions -}

-- | The summary of a transaction the store reports, for an account whose
-- addresses are given with their derivation paths. An output or input at an
-- address the network cannot write, a SegWit one on Bitcoin Cash for
-- instance, is kept apart like one that pays no address.
storeSummary :: Network -> Map Address SoftPath -> Store.Transaction -> TxSummary
storeSummary net walletAddrs sTx =
  TxSummary
    { summaryType = txType amount fee,
      summaryAmount = amount,
      summaryFee = fee,
      summaryFeeByte = fee `div` size,
      summaryChangeInFee = Nothing,
      summarySize = size,
      summaryMyOutputs = myOutputs,
      summaryOtherOutputs = outputs `Map.difference` walletAddrs,
      summaryNonStdOutputs = nonStdOut,
      summaryMyInputs = myInputs,
      summaryOtherInputs = Map.map fst (inputs `Map.difference` walletAddrs),
      summaryNonStdInputs = nonStdIn
    }
  where
    size = fromIntegral sTx.size :: Natural
    fee = fromIntegral sTx.fee :: Natural
    (outputs, nonStdOut) = outputAddressMap net sTx.outputs
    (inputs, nonStdIn) = inputAddressMap net sTx.inputs
    myOutputs = Map.intersectionWith (\v path -> MyOutputs v path "") outputs walletAddrs
    myInputs = Map.intersectionWith (\(v, spent) path -> MyInputs v path "" spent) inputs walletAddrs
    amount =
      toInteger (sum $ myOutputsValue <$> Map.elems myOutputs)
        - toInteger (sum $ myInputsValue <$> Map.elems myInputs)

-- | The value paid to each address of the network, and the outputs that
-- pay no such address
outputAddressMap :: Network -> [Store.StoreOutput] -> (Map Address Natural, [Store.StoreOutput])
outputAddressMap net outs =
  (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> outs
    f (Store.StoreOutput v _ _ (Just a)) | onNetwork net a = Right (a, fromIntegral v)
    f s = Left s

-- | The value spent by each address of the network with the coins it
-- spends, in the order of the inputs, and the inputs that spend no such
-- address
inputAddressMap :: Network -> [Store.StoreInput] -> (Map Address (Natural, [OutPoint]), [Store.StoreInput])
inputAddressMap net ins =
  (Map.fromListWith addLater rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> ins
    f (Store.StoreInput outpoint _ _ _ v _ (Just a)) | onNetwork net a = Right (a, (fromIntegral v, [outpoint]))
    f s = Left s
    addLater (v2, later) (v1, earlier) = (v1 + v2, earlier <> later)

{- Summaries in the database -}

-- | The block of every live transaction summary of an account, by
-- transaction id. The dead ones are left out: the sync treats them as
-- unknown, so that a transaction the store lists again is fetched and comes
-- back to life.
txInfoBlockRefs ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) (Map TxHash Store.BlockRef)
txInfoBlockRefs accId = do
  rows <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccount ==. val accId
          &&. t ^. DBTxInfoDead ==. val False
      return (t ^. DBTxInfoTxid, t ^. DBTxInfoBlockRef)
  entries <- forM rows $ \(Value tidBytes, Value refBytes) -> do
    tid <- liftEither $ bytesToHash tidBytes
    ref <- liftEither $ S.decode refBytes
    return (tid, ref)
  return $ Map.fromList entries

-- | The stored summary of a live transaction of the account, with its
-- block; nothing when the sync has not stored it or found it dead
liveSummary ::
  (MonadUnliftIO m) => Network -> DBAccountId -> TxHash -> ExceptT String (DB m) (Maybe (Store.BlockRef, TxSummary))
liveSummary net accId tid = do
  rowM <- lift $ P.get (DBTxInfoKey accId (hashBytes tid))
  forM (mfilter (not . dBTxInfoDead) rowM) $ \row -> do
    ref <- liftEither $ S.decode (dBTxInfoBlockRef row)
    summary <- liftEither $ decodeSummary net tid (dBTxInfoBlob row)
    return (ref, summary)

-- | A summary as the database stores it
decodeSummary :: Network -> TxHash -> BS.ByteString -> Either String TxSummary
decodeSummary net tid blob =
  maybeToEither ("The stored summary of transaction " <> cs (txHashToHex tid) <> " does not decode") $
    unmarshalJSON net (BS.fromStrict blob)

-- | Mark the summary of a transaction dead: the store no longer has it, or
-- reports it replaced or dropped. The summary stays, for the history.
markTxInfoDead :: (MonadUnliftIO m) => DBAccountId -> TxHash -> DB m ()
markTxInfoDead accId tid =
  P.update (DBTxInfoKey accId (hashBytes tid)) [DBTxInfoDead P.=. True]

-- | Store the summary of a live transaction of the account at its block,
-- or replace the one stored, which brings a summary marked dead back.
-- Returns whether the row changed.
repsertSummary ::
  (MonadUnliftIO m) => Network -> DBAccountId -> TxHash -> Store.BlockRef -> TxSummary -> DB m Bool
repsertSummary net accId tid ref summary = do
  time <- liftIO getCurrentTime
  let key = DBTxInfoKey accId (hashBytes tid)
      row =
        DBTxInfo
          { dBTxInfoAccount = accId,
            dBTxInfoTxid = hashBytes tid,
            dBTxInfoBlockRef = S.encode ref,
            dBTxInfoBlob = BS.toStrict $ marshalJSON net summary,
            dBTxInfoDead = False,
            dBTxInfoCreated = time
          }
  prevM <- P.get key
  case prevM of
    Just prev -> do
      let new = row {dBTxInfoCreated = dBTxInfoCreated prev}
      P.replace key new
      return (prev /= new)
    Nothing -> do
      P.insert_ row
      return True

-- | A page of the account's transactions on the chain, in the order of
-- their block references as the store encodes them: the mempool first,
-- then the newest blocks first. Their confirmations are counted from the
-- best block, none for a dead transaction, and the labels of their
-- addresses are those of today.
txsPage :: (MonadUnliftIO m) => DBAccountId -> Page -> ExceptT String (DB m) [TxInfo]
txsPage accId (Page lim off) = do
  acc <- getAccountById accId
  let net = accountNetwork acc
  rows <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccount ==. val accId
      orderBy [asc (t ^. DBTxInfoBlockRef)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return (t ^. DBTxInfoTxid, t ^. DBTxInfoBlockRef, t ^. DBTxInfoDead, t ^. DBTxInfoBlob)
  bestM <- lift $ getBest net
  forM rows $ \(Value tidBytes, Value refBytes, Value dead, Value blob) -> do
    tid <- liftEither $ bytesToHash tidBytes
    ref <- liftEither $ S.decode refBytes
    summary <- liftEither $ decodeSummary net tid blob
    let confirmations = if dead then 0 else getConfirmations (snd <$> bestM) ref
        status = ChainStatus tid ref confirmations dead
    lift $ fillTxInfoLabels net $ TxInfo summary (OnChain status)

-- | The transaction with the labels of the account's addresses filled in,
-- as the database has them now
fillTxInfoLabels :: (MonadUnliftIO m) => Network -> TxInfo -> DB m TxInfo
fillTxInfoLabels net info = do
  outputs <- Map.traverseWithKey (\addr o -> labelOutput o <$> addressLabel net addr) (summaryMyOutputs summary)
  inputs <- Map.traverseWithKey (\addr i -> labelInput i <$> addressLabel net addr) (summaryMyInputs summary)
  return info {txInfoSummary = summary {summaryMyOutputs = outputs, summaryMyInputs = inputs}}
  where
    summary = txInfoSummary info
    labelOutput o label = o {myOutputsLabel = label}
    labelInput i label = i {myInputsLabel = label}

-- | The label of an address of the wallet, or none
addressLabel :: (MonadUnliftIO m) => Network -> Address -> DB m Text
addressLabel net addr =
  fmap (fromMaybe "") . runMaybeT $ do
    addrT <- hoistMaybe $ addrToText net addr
    Entity _ row <- MaybeT $ P.getBy (UniqueAddress addrT)
    return $ dBAddressLabel row
