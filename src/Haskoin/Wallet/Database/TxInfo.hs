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
module Haskoin.Wallet.Database.TxInfo where

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
import Data.Char (toUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (isLeft, partitionEithers, rights)
import Data.Int (Int64)
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Serialize as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32, Word64)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.TxLabel (txLabel)
import Haskoin.Wallet.Tx.Size (storeTxSize)
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
    -- | The fee per virtual byte, rounded down
    summaryFeeByte :: !Natural,
    -- | What the fee includes beyond the fee rate the transaction was built
    -- for: change that was not worth an output. Nothing when that rate is
    -- unknown, as it is for the transactions the store reports.
    summaryChangeInFee :: !(Maybe Natural),
    -- | The size in virtual bytes ("Haskoin.Wallet.Tx.Size"): of the
    -- signed transaction, or estimated before it is signed
    summarySize :: !Natural,
    -- | The lock time: the block the transaction waits for, 0 for none. hw
    -- sets one near the best block (see "Haskoin.Wallet.Tx.LockTime").
    summaryLockTime :: !Word32,
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

-- | Whether the wallet made the transaction, as its summaries for each of
-- the accounts it involves tell: every input spends a coin of one of them.
-- An input one account sees as another's must be one another account of
-- the wallet spends, so the summary of every account that spends in the
-- transaction must be among them. With the summary of one account alone,
-- this is 'ownTransaction'.
walletTransaction :: [TxSummary] -> Bool
walletTransaction summaries = not (Set.null spentByWallet) && all spendsWalletCoinsOnly summaries
  where
    -- The addresses the accounts spend from, all together
    spentByWallet = Set.unions [Map.keysSet (summaryMyInputs summary) | summary <- summaries]
    spendsWalletCoinsOnly summary =
      Map.keysSet (summaryOtherInputs summary) `Set.isSubsetOf` spentByWallet
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
    -- | The time the store gives the transaction: when it first saw it in
    -- its mempool, or, for one it first saw in a block, the time of that
    -- block. Nothing for a summary stored without it, before hw kept it,
    -- until the next sync fetches the transaction again.
    chainTime :: !(Maybe UTCTime),
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
    pendingConflicted :: !Bool,
    -- | When the wallet started holding it, as it was prepared or imported:
    -- none for a transaction the wallet does not hold, read from a file
    pendingCreated :: !(Maybe UTCTime),
    -- | The ids of the transactions it replaces, for a replacement bumpfee
    -- prepared
    pendingReplaces :: ![TxHash]
  }
  deriving (Eq, Show)

-- | Whether a transaction in preparation is signed: a signed transaction
-- has its final id
pendingSigned :: PendingStatus -> Bool
pendingSigned = isJust . pendingTxid

-- | The hash hw shows for a transaction in preparation: its nosigHash
-- until it is signed, then its id. The commands take either hash.
pendingShownHash :: PendingStatus -> TxHash
pendingShownHash status = fromMaybe (pendingNosigHash status) (pendingTxid status)

-- | A transaction as hw reports it: what it means to the account, where it
-- stands, the label the user gave it, and the labels the coins it spends
-- show
data TxInfo = TxInfo
  { txInfoSummary :: !TxSummary,
    txInfoStatus :: !TxStatus,
    -- | The label of the transaction, or none: in its signing data while it
    -- is pending, then in the labels of the account
    txInfoLabel :: !Text,
    -- | The labels the coins of the account it spends show ('coinLabel'),
    -- by the transaction that paid each, those that have one. Filled in
    -- from the database, or from the file that carried the transaction.
    txInfoInputLabels :: !(Map TxHash Text)
  }
  deriving (Eq, Show)

-- | Whether a transaction was replaced or dropped from the network. Only
-- one on the chain can be: one in preparation is not out yet.
txInfoDead :: TxInfo -> Bool
txInfoDead info =
  case txInfoStatus info of
    OnChain status -> chainDead status
    Pending _ -> False

-- | A transaction as txs lists it, with the balance of the account after
-- it. A dead transaction has none: the balance of the account leaves it
-- out.
data ListedTx = ListedTx
  { listedTxInfo :: !TxInfo,
    listedTxBalance :: !(Maybe Integer)
  }
  deriving (Eq, Show)

-- | Where txs goes on: after a transaction, by its block and its id, and
-- with the balance before it, which is the balance after the next one
data TxsCursor = TxsCursor
  { txsCursorBlock :: !Store.BlockRef,
    txsCursorTxid :: !TxHash,
    txsCursorBalance :: !Integer
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

-- The summary, the status and the label are written as one flat object, the
-- same for a transaction on the chain and one in preparation, and addresses
-- are written as the text of the network.

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
  marshalValue net = object . txInfoPairs net
  unmarshalValue net =
    withObject "TxInfo" $ \o ->
      TxInfo
        <$> summaryParser net o
        <*> statusParser o
        <*> o .:? "label" .!= ""
        <*> (maybe (return Map.empty) parseTxidLabels =<< o .:? "inputlabels")

-- | The balance joins the keys of the transaction, in the same flat object
instance MarshalJSON Network ListedTx where
  marshalValue net (ListedTx info balance) =
    object $ txInfoPairs net info <> ["balance" .= balance]
  unmarshalValue net value =
    ListedTx
      <$> unmarshalValue net value
      <*> withObject "ListedTx" (.: "balance") value

txInfoPairs :: Network -> TxInfo -> [Pair]
txInfoPairs net (TxInfo summary status label inputs) =
  summaryPairs net summary <> statusPairs status <> ["label" .= label, "inputlabels" .= txidLabelsJson inputs]

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
    "locktime" .= summaryLockTime s,
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
    -- The summaries stored by earlier versions of hw have no lock time
    <*> o .:? "locktime" .!= 0
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
    "time" .= chainTime status,
    "confirmations" .= chainConfirmations status,
    "dead" .= chainDead status,
    "pending" .= Json.Null
  ]
statusPairs (Pending status) =
  [ "txid" .= pendingTxid status,
    "block" .= Store.MemRef 0,
    "time" .= Json.Null,
    "confirmations" .= (0 :: Natural),
    "dead" .= False,
    "pending"
      .= object
        [ "nosighash" .= pendingNosigHash status,
          "signed" .= pendingSigned status,
          "online" .= pendingOnline status,
          "conflicted" .= pendingConflicted status,
          "created" .= pendingCreated status,
          "replaces" .= pendingReplaces status
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
          <*> p .:? "created"
          <*> p .:? "replaces" .!= []
    Nothing ->
      fmap OnChain $
        ChainStatus
          <$> o .: "txid"
          <*> o .: "block"
          <*> o .:? "time"
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
      summaryLockTime = sTx.locktime,
      summaryMyOutputs = myOutputs,
      summaryOtherOutputs = outputs `Map.difference` walletAddrs,
      summaryNonStdOutputs = nonStdOut,
      summaryMyInputs = myInputs,
      summaryOtherInputs = Map.map fst (inputs `Map.difference` walletAddrs),
      summaryNonStdInputs = nonStdIn
    }
  where
    size = storeTxSize sTx
    fee = fromIntegral sTx.fee :: Natural
    (outputs, nonStdOut) = outputAddressMap net sTx.outputs
    (inputs, nonStdIn) = inputAddressMap net sTx.inputs
    myOutputs = Map.intersectionWith (\v path -> MyOutputs v path "") outputs walletAddrs
    myInputs = Map.intersectionWith (\(v, spent) path -> MyInputs v path "" spent) inputs walletAddrs
    amount =
      toInteger (sum $ myOutputsValue <$> Map.elems myOutputs)
        - toInteger (sum $ myInputsValue <$> Map.elems myInputs)

-- | The value paid to each address of the network, and the outputs that
-- pay no such address. An output at an address the network cannot write,
-- such as a SegWit one on Bitcoin Cash, is kept without its address, as
-- one that pays none: the summary is written on the network, which has no
-- text for it.
outputAddressMap :: Network -> [Store.StoreOutput] -> (Map Address Natural, [Store.StoreOutput])
outputAddressMap net outs =
  (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> outs
    f (Store.StoreOutput v _ _ (Just a)) | onNetwork net a = Right (a, fromIntegral v)
    f (Store.StoreOutput v script spender _) = Left (Store.StoreOutput v script spender Nothing)

-- | The value spent by each address of the network with the coins it
-- spends, in the order of the inputs, and the inputs that spend no such
-- address, kept without the address the network cannot write, as for the
-- outputs
inputAddressMap :: Network -> [Store.StoreInput] -> (Map Address (Natural, [OutPoint]), [Store.StoreInput])
inputAddressMap net ins =
  (Map.map inOrder (Map.fromListWith addEarlier rs), ls)
  where
    (ls, rs) = partitionEithers $ f <$> ins
    f (Store.StoreInput outpoint _ _ _ v _ (Just a)) | onNetwork net a = Right (a, (fromIntegral v, [outpoint]))
    f (Store.StoreInput outpoint sequenceNumber script pkscript v witness _) = Left (Store.StoreInput outpoint sequenceNumber script pkscript v witness Nothing)
    f coinbase = Left coinbase
    -- The coins of an address gather newest first, each in front, and
    -- are turned around once: appending each would take the square of
    -- their number
    addEarlier (v2, later) (v1, earlier) = (v1 + v2, later <> earlier)
    inOrder (v, outpoints) = (v, reverse outpoints)

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

-- | The ids of the live summaries of an account, whatever else their rows
-- hold. A full sync goes by these alone: it rewrites the rows, so one that
-- does not read must not stop it.
liveTxids :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) (Set TxHash)
liveTxids accId = do
  rows <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccount ==. val accId
          &&. t ^. DBTxInfoDead ==. val False
      return (t ^. DBTxInfoTxid)
  Set.fromList <$> liftEither (mapM (bytesToHash . unValue) rows)

-- | The live summaries of an account without their time, stored before hw
-- kept it, as the step to version 4 of the database copied them: the sync
-- fetches their transactions again to learn it. An index holds these rows
-- alone, so that asking costs nothing once every summary has its time.
untimedSummaries :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) [TxHash]
untimedSummaries accId = do
  rows <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccount ==. val accId
          &&. isNothing (t ^. DBTxInfoTime)
          &&. t ^. DBTxInfoDead ==. val False
      return (t ^. DBTxInfoTxid)
  liftEither $ mapM (bytesToHash . unValue) rows

-- | The live transactions of the account in the mempool, with their
-- summaries, by their ids, up to the count. The index on the block
-- references finds them: a mempool reference sorts before every block
-- reference, as the listings rely on.
mempoolSummaries :: (MonadUnliftIO m) => Network -> DBAccountId -> Natural -> ExceptT String (DB m) [(TxHash, TxSummary)]
mempoolSummaries net accId atMost = do
  rows <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccount ==. val accId
          &&. t ^. DBTxInfoBlockRef <. val firstBlock
          &&. t ^. DBTxInfoDead ==. val False
      orderBy [asc (t ^. DBTxInfoTxid)]
      limit (rowLimit atMost)
      return (t ^. DBTxInfoTxid, t ^. DBTxInfoBlob)
  forM rows $ \(Value tidBytes, Value blob) -> do
    tid <- liftEither $ bytesToHash tidBytes
    summary <- liftEither $ decodeSummary net tid blob
    return (tid, summary)
  where
    -- The first block reference in the order of the references: the
    -- highest block comes first
    firstBlock = S.encode (Store.BlockRef maxBound maxBound)

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

-- | The stored summaries of a live transaction, one for each account of
-- the network the sync has stored it for, each with its block: none when
-- the sync has stored it for no account, or found it dead for each. A
-- transaction from before the fork of Bitcoin Cash has the same id on both
-- chains, so the accounts of the other network are left out.
liveSummaries :: (MonadUnliftIO m) => Network -> TxHash -> ExceptT String (DB m) [(Store.BlockRef, TxSummary)]
liveSummaries net tid = do
  rows <-
    lift . select . from $ \(t `InnerJoin` a) -> do
      on $ t ^. DBTxInfoAccount ==. a ^. DBAccountId
      where_ $
        t ^. DBTxInfoTxid ==. val (hashBytes tid)
          &&. t ^. DBTxInfoDead ==. val False
          &&. a ^. DBAccountNetwork ==. val (cs net.name)
      return (t ^. DBTxInfoBlockRef, t ^. DBTxInfoBlob)
  forM rows $ \(Value refBytes, Value blob) -> do
    ref <- liftEither $ S.decode refBytes
    summary <- liftEither $ decodeSummary net tid blob
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

-- | Delete the dead summaries of the account that do not read. The store
-- no longer lists their transactions, so nothing can write them again, and
-- one of them would stop txs for good: it refuses a row that does not read.
-- A full sync does this last, when every live summary has just been
-- written.
dropUnreadableDead :: (MonadUnliftIO m) => Network -> DBAccountId -> DB m ()
dropUnreadableDead net accId = do
  rows <- P.selectList [DBTxInfoAccount P.==. accId, DBTxInfoDead P.==. True] []
  forM_ rows $ \(Entity key row) ->
    when (isLeft (readable row)) $ P.delete key
  where
    readable row = do
      tid <- bytesToHash (dBTxInfoTxid row)
      _ <- S.decode (dBTxInfoBlockRef row) :: Either String Store.BlockRef
      decodeSummary net tid (dBTxInfoBlob row)

-- | The time the store gives a transaction, in seconds since 1970
storeTime :: Word64 -> UTCTime
storeTime = posixSecondsToUTCTime . fromIntegral

-- | Store the summary of a live transaction of the account at its block,
-- with the time the store gives it, or replace the one stored, which
-- brings a summary marked dead back. Returns whether the row changed.
repsertSummary ::
  (MonadUnliftIO m) => Network -> DBAccountId -> TxHash -> Store.BlockRef -> UTCTime -> TxSummary -> DB m Bool
repsertSummary net accId tid ref time summary = do
  now <- liftIO getCurrentTime
  let key = DBTxInfoKey accId (hashBytes tid)
      row =
        DBTxInfo
          { dBTxInfoAccount = accId,
            dBTxInfoTxid = hashBytes tid,
            dBTxInfoBlockRef = S.encode ref,
            dBTxInfoTime = Just time,
            dBTxInfoAmount = fromInteger (summaryAmount summary),
            dBTxInfoBlob = BS.toStrict $ marshalJSON net summary,
            dBTxInfoDead = False,
            dBTxInfoCreated = now
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

-- | The account's transactions on the chain, in the order of their block
-- references as the store encodes them: the mempool first, then the newest
-- blocks first, and those with the same block reference by their ids, so
-- that the order is settled. Those after the transaction given, by its
-- block and its id, or the first ones, up to the count, or all of them.
-- Each is as 'chainTxInfo' reports it.
txsAfter :: (MonadUnliftIO m) => DBAccountId -> Maybe (Store.BlockRef, TxHash) -> Maybe Natural -> ExceptT String (DB m) [TxInfo]
txsAfter accId afterM countM = do
  acc <- getAccountById accId
  let net = accountNetwork acc
  rows <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccount ==. val accId
      -- From the block of the transaction given on, which the index on
      -- the block references finds, and past the transaction itself
      -- within that block
      forM_ afterM $ \(block, tid) -> do
        let blockBytes = val (S.encode block)
        where_ $
          t ^. DBTxInfoBlockRef >=. blockBytes
            &&. (t ^. DBTxInfoBlockRef >. blockBytes ||. t ^. DBTxInfoTxid >. val (hashBytes tid))
      orderBy [asc (t ^. DBTxInfoBlockRef), asc (t ^. DBTxInfoTxid)]
      forM_ countM $ limit . rowLimit
      return t
  heightM <- bestHeight net
  forM rows $ chainTxInfo net heightM . entityVal

-- | The transactions of txs after the cursor, or the first ones, up to the
-- count, or all of them. Each comes with the balance before it, which the
-- cursor after it carries. The first transaction of the listing starts
-- from what the account holds, and each one below from the balance before
-- the one above it.
listedTxsAfter :: (MonadUnliftIO m) => DBAccountId -> Maybe TxsCursor -> Maybe Natural -> ExceptT String (DB m) [(ListedTx, Integer)]
listedTxsAfter accId afterM countM = do
  acc <- getAccountById accId
  infos <- txsAfter accId (position <$> afterM) countM
  let start = maybe (accountTotal acc) txsCursorBalance afterM
  return $ snd (mapAccumL withBalance start infos)
  where
    position cursor = (txsCursorBlock cursor, txsCursorTxid cursor)

-- | A transaction with the balance after it, and that balance without
-- the transaction: the balance before it, and after the next one down. A
-- dead transaction takes nothing away, since the balance leaves it out.
withBalance :: Integer -> TxInfo -> (Integer, (ListedTx, Integer))
withBalance after info
  | txInfoDead info = (after, (ListedTx info Nothing, after))
  | otherwise = (before, (ListedTx info (Just after), before))
  where
    before = after - summaryAmount (txInfoSummary info)

-- | The cursor of the transaction --after names, whose balance takes away
-- every transaction down to that one
txsCursorAt :: (MonadUnliftIO m) => DBAccountId -> DBAccount -> TxHash -> ExceptT String (DB m) TxsCursor
txsCursorAt accId acc tid = do
  block <- txBlock accId tid
  above <- lift $ amountsThrough accId block tid
  return $ TxsCursor block tid (accountTotal acc - above)

-- | What the live transactions of the account add up to, from the top of
-- txs down to the transaction given, that one included. The index on the
-- block references holds the amounts, so the sum reads no row.
amountsThrough :: (MonadUnliftIO m) => DBAccountId -> Store.BlockRef -> TxHash -> DB m Integer
amountsThrough accId block tid = do
  total <- selectOne . from $ \t -> do
    let blockBytes = val (S.encode block)
    where_ $
      t ^. DBTxInfoAccount ==. val accId
        &&. t ^. DBTxInfoDead ==. val False
        &&. t ^. DBTxInfoBlockRef <=. blockBytes
        &&. (t ^. DBTxInfoBlockRef <. blockBytes ||. t ^. DBTxInfoTxid <=. val (hashBytes tid))
    return $ sum_ (t ^. DBTxInfoAmount)
  return $ toInteger (joinMaybe (0 :: Int64) id total)

-- | A transaction of the account on the chain, as txs shows it, if the
-- account has it, dead or alive
storedTxInfo :: (MonadUnliftIO m) => DBAccountId -> TxHash -> ExceptT String (DB m) (Maybe TxInfo)
storedTxInfo accId tid = do
  acc <- getAccountById accId
  let net = accountNetwork acc
  rowM <- lift $ P.get (DBTxInfoKey accId (hashBytes tid))
  heightM <- bestHeight net
  forM rowM $ chainTxInfo net heightM

-- | The stored summary of a transaction on the chain as hw reports it. Its
-- confirmations are counted from the best height given, none for a dead
-- transaction. Its label and the labels of its addresses are those of
-- today.
chainTxInfo :: (MonadUnliftIO m) => Network -> Maybe BlockHeight -> DBTxInfo -> ExceptT String (DB m) TxInfo
chainTxInfo net heightM row = do
  tid <- liftEither $ bytesToHash (dBTxInfoTxid row)
  ref <- liftEither $ S.decode (dBTxInfoBlockRef row)
  summary <- liftEither $ decodeSummary net tid (dBTxInfoBlob row)
  let dead = dBTxInfoDead row
      confirmations = if dead then 0 else getConfirmations heightM ref
      status = ChainStatus tid ref (dBTxInfoTime row) confirmations dead
  label <- lift $ txLabel (dBTxInfoAccount row) tid
  lift $ fillTxInfoLabels net (dBTxInfoAccount row) $ TxInfo summary (OnChain status) label Map.empty

-- | The block of a transaction of the account, the one --after names, or
-- why there is none
txBlock :: (MonadUnliftIO m) => DBAccountId -> TxHash -> ExceptT String (DB m) Store.BlockRef
txBlock accId tid = do
  rowM <- lift $ P.get (DBTxInfoKey accId (hashBytes tid))
  row <- liftMaybe ("The transaction " <> cs (txHashToHex tid) <> " is not in the account") rowM
  liftEither $ S.decode (dBTxInfoBlockRef row)

-- | At most the given number of ids of the account's transactions on the
-- chain that start with the hexadecimal digits, in the order of their ids:
-- for the shell. The key holds the bytes of an id in the order hw shows
-- them, so hex() writes its digits as they are typed, in capitals.
txidsStarting :: (MonadUnliftIO m) => DBAccountId -> String -> Natural -> DB m [TxHash]
txidsStarting accId digits atMost = do
  rows <-
    rawSql
      "select txid from d_b_tx_info where account = ? and substr(hex(txid), 1, ?) = ? order by txid limit ?"
      [P.toPersistValue accId, P.toPersistValue (length digits), P.toPersistValue (map toUpper digits), P.toPersistValue (rowLimit atMost :: Int)]
  return $ rights (bytesToHash . unSingle <$> rows)

-- | The transaction with its labels filled in as the database has them
-- now: those of the account's addresses, and those the coins it spends
-- show, by the transaction that paid each (see 'coinLabel')
fillTxInfoLabels :: (MonadUnliftIO m) => Network -> DBAccountId -> TxInfo -> DB m TxInfo
fillTxInfoLabels net accId info = do
  labelled <- relabelAddresses (addressLabel net) info
  inputLabels <- coinLabels net accId (spentTxids (txInfoSummary info))
  return labelled {txInfoInputLabels = inputLabels}

-- | The transactions the coins of the account a transaction spends come
-- from, each once
spentTxids :: TxSummary -> [TxHash]
spentTxids summary = nubOrd [outpoint.hash | input <- Map.elems (summaryMyInputs summary), outpoint <- myInputsOutpoints input]

-- | The label a coin of the transaction shows: the label of the
-- transaction, which paid the coin to the account or moved it between its
-- own addresses. The change of a payment the account made shows none: the
-- label says what the payment was for, not what its change is. None
-- either without a label, or when the account has no such transaction.
coinLabel :: (MonadUnliftIO m) => Network -> DBAccountId -> TxHash -> DB m Text
coinLabel net accId tid = do
  label <- txLabel accId tid
  if Text.null label
    then return ""
    else do
      rowM <- P.get (DBTxInfoKey accId (hashBytes tid))
      let summaryM = decodeSummary net tid . dBTxInfoBlob =<< maybe (Left "no such transaction") Right rowM
      return $ case summaryM of
        Right summary | summaryType summary /= TxDebit -> label
        _ -> ""

-- | The labels the coins of the transactions show ('coinLabel'), by
-- transaction, those that have one
coinLabels :: (MonadUnliftIO m) => Network -> DBAccountId -> [TxHash] -> DB m (Map TxHash Text)
coinLabels net accId tids = do
  labelled <- forM (nubOrd tids) $ \tid -> (,) tid <$> coinLabel net accId tid
  return $ Map.fromList [(tid, label) | (tid, label) <- labelled, not (Text.null label)]

-- | The transaction with the labels of the account's addresses, those it
-- spends from and those it pays, as the function gives them from the
-- database
relabelAddresses :: (Applicative f) => (Address -> f Text) -> TxInfo -> f TxInfo
relabelAddresses labelOf info =
  rebuild
    <$> Map.traverseWithKey (\addr o -> labelOutput o <$> labelOf addr) (summaryMyOutputs summary)
    <*> Map.traverseWithKey (\addr i -> labelInput i <$> labelOf addr) (summaryMyInputs summary)
  where
    summary = txInfoSummary info
    rebuild outputs inputs = info {txInfoSummary = summary {summaryMyOutputs = outputs, summaryMyInputs = inputs}}
    labelOutput o label = o {myOutputsLabel = label}
    labelInput i label = i {myInputsLabel = label}

-- | The transaction with the labels of the account's addresses, those it
-- spends from and those it pays, as the function gives them by the path
-- of each: from the file that carried the transaction
relabelPaths :: (SoftPath -> Text) -> TxInfo -> TxInfo
relabelPaths labelOf info =
  info {txInfoSummary = summary {summaryMyOutputs = outputs, summaryMyInputs = inputs}}
  where
    summary = txInfoSummary info
    outputs = Map.map (\o -> o {myOutputsLabel = labelOf (myOutputsPath o)}) (summaryMyOutputs summary)
    inputs = Map.map (\i -> i {myInputsLabel = labelOf (myInputsPath i)}) (summaryMyInputs summary)

-- | The label of an address of the wallet, or none
addressLabel :: (MonadUnliftIO m) => Network -> Address -> DB m Text
addressLabel net addr = maybe "" dBAddressLabel <$> walletAddressRow net addr

-- | The row of an address of the wallet, if the wallet has it
walletAddressRow :: (MonadUnliftIO m) => Network -> Address -> DB m (Maybe DBAddress)
walletAddressRow net addr =
  runMaybeT $ do
    addrT <- hoistMaybe $ addrToText net addr
    Entity _ row <- MaybeT $ P.getBy (UniqueAddress addrT)
    return row
