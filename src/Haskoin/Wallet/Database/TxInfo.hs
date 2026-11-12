{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The transaction summaries of the wallet. A 'TxSummary' says what a
-- transaction means to a portfolio on its network, a pool: its type and
-- amount, its fee, and the inputs and outputs that are the portfolio's,
-- whichever of its accounts on the network has them. A payment from a
-- BIP44 address of the portfolio to a BIP84 one moves nothing out of it,
-- and is one internal transaction, not a debit and a credit. A 'TxStatus' says where the
-- transaction stands: on the chain, as the store reports it, or in
-- preparation, held among the pending transactions. The two together, a
-- 'TxInfo', are what hw prints and writes as JSON. The summaries of the
-- transactions on the chain are stored in the database, which the sync
-- keeps up to date.
--
-- A summary is of one portfolio. Every address of another portfolio is
-- an address of others to it. A user often pays from one portfolio to
-- another, and wants to see that the payment went to an address of
-- theirs, not to a stranger. So a 'TxInfo' also names the addresses of
-- its transaction that another portfolio of the database holds
-- ('HeldElsewhere'). They are read from the database when the transaction
-- is shown, as the labels are. They are never stored. A stored summary
-- stays what its own portfolio alone can tell.
module Haskoin.Wallet.Database.TxInfo where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import qualified Data.Aeson as Json
import Data.Aeson.Types (Pair, Parser)
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (isLeft, partitionEithers, rights)
import Data.Int (Int64)
import Data.List (genericLength, genericTake, mapAccumL, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..))
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
import Haskoin.Wallet.Database.Address (addrPoolPath, poolAddressRow, storedAmong)
import Haskoin.Wallet.Database.Portfolio (Portfolio (..), PortfolioRef, getAllPortfolios, parsePortfolioRef, portfolioRef, portfolioRefPairs)
import Haskoin.Wallet.Database.TxLabel (txLabel)
import Haskoin.Wallet.Keys (AccountKind, PoolPath (..))
import Haskoin.Wallet.ShortId (ShortId, shortIdRange)
import Haskoin.Wallet.Tx.Size (storeTxSize)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

{- Summaries -}

-- | What a transaction does to the portfolio: a credit brings money in, a
-- debit sends money out, an internal transaction moves money between the
-- portfolio's own addresses on the network, of either kind, and costs its
-- fee. A payment to another portfolio of the same wallet is a debit here
-- and a credit there.
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

-- | What a transaction means to a portfolio on its network
data TxSummary = TxSummary
  { summaryType :: !TxType,
    -- | What the portfolio gains (positive) or spends (negative), fee included
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
    -- | The outputs paying the portfolio, by address
    summaryMyOutputs :: !(Map Address MyOutputs),
    -- | The outputs paying others, by address
    summaryOtherOutputs :: !(Map Address Natural),
    -- | The outputs that pay no address
    summaryNonStdOutputs :: ![Store.StoreOutput],
    -- | The inputs spending coins of the portfolio, by address
    summaryMyInputs :: !(Map Address MyInputs),
    -- | The inputs spending coins of others, by address
    summaryOtherInputs :: !(Map Address Natural),
    -- | The inputs that spend no address, the coinbase among them
    summaryNonStdInputs :: ![Store.StoreInput]
  }
  deriving (Eq, Show)

-- | What an address of the portfolio receives in a transaction
data MyOutputs = MyOutputs
  { myOutputsValue :: !Natural,
    -- | Where the address is in the portfolio: the kind of its account,
    -- and its path below the key of that account
    myOutputsPath :: !PoolPath,
    myOutputsLabel :: !Text
  }
  deriving (Eq, Show)

-- | What an address of the portfolio spends in a transaction, and which coins
data MyInputs = MyInputs
  { myInputsValue :: !Natural,
    myInputsPath :: !PoolPath,
    myInputsLabel :: !Text,
    myInputsOutpoints :: ![OutPoint]
  }
  deriving (Eq, Show)

-- | The types of the portfolio's addresses the transaction involves, each
-- once, BIP44 first: those it spends from and those it pays. A summary
-- shows them, since a portfolio holds coins of both types on Bitcoin.
summaryKinds :: TxSummary -> [AccountKind]
summaryKinds summary =
  Set.toAscList . Set.fromList $
    (pathKind . myInputsPath <$> Map.elems (summaryMyInputs summary))
      <> (pathKind . myOutputsPath <$> Map.elems (summaryMyOutputs summary))

-- | Whether the portfolio spends or receives anything in the transaction
involvesWallet :: TxSummary -> Bool
involvesWallet summary = not (Map.null (summaryMyInputs summary) && Map.null (summaryMyOutputs summary))

-- | Whether the portfolio made the transaction: every input spends a coin
-- of the portfolio
ownTransaction :: TxSummary -> Bool
ownTransaction summary =
  not (Map.null (summaryMyInputs summary))
    && Map.null (summaryOtherInputs summary)
    && null (summaryNonStdInputs summary)

-- | Whether the wallet made the transaction, as its summaries for each of
-- the portfolios it involves tell: every input spends a coin of one of
-- them. An input one portfolio sees as another's must be one another
-- portfolio of the database spends, so the summary of every portfolio
-- that spends in the transaction must be among them. With the summary of
-- one portfolio alone, this is 'ownTransaction'.
walletTransaction :: [TxSummary] -> Bool
walletTransaction summaries = not (Set.null spentByWallet) && all spendsWalletCoinsOnly summaries
  where
    -- The addresses the portfolios spend from, all together
    spentByWallet = Set.unions [Map.keysSet (summaryMyInputs summary) | summary <- summaries]
    spendsWalletCoinsOnly summary =
      Map.keysSet (summaryOtherInputs summary) `Set.isSubsetOf` spentByWallet
        && null (summaryNonStdInputs summary)

-- | The type of a transaction, from what the portfolio gains or loses and
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
    -- the pending transactions of the portfolio
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

-- | A transaction as hw reports it: what it means to the portfolio, where it
-- stands, the label the user gave it, and the labels the coins it spends
-- show
data TxInfo = TxInfo
  { txInfoSummary :: !TxSummary,
    txInfoStatus :: !TxStatus,
    -- | The label of the transaction, or none: in its signing data while it
    -- is pending, then in the labels of the portfolio
    txInfoLabel :: !Text,
    -- | The labels the coins of the portfolio it spends show ('coinLabel'),
    -- by the transaction that paid each, those that have one. Filled in
    -- from the database, or from the file that carried the transaction.
    txInfoInputLabels :: !(Map TxHash Text),
    -- | The addresses of others, in the summary, that another portfolio of
    -- the database holds on the network, each with where it is
    -- ('heldElsewhere'). Filled in from the database, and empty for a
    -- transaction of no portfolio.
    txInfoElsewhere :: !(Map Address HeldElsewhere)
  }
  deriving (Eq, Show)

-- | An address of another portfolio of the database, in a transaction as
-- a portfolio sees it. It says which portfolio holds the address, where
-- the address is there, and the label it has there. The summary of the
-- transaction counts it among the addresses of others, since its coins
-- are not of the portfolio of the summary. The views still name it as an
-- address of the wallet, with its portfolio, as in bip84\/0\/5 of
-- portfolio1.
data HeldElsewhere = HeldElsewhere
  { elsewherePortfolio :: !PortfolioRef,
    -- | The kind of its account there, and its path below the key of
    -- that account
    elsewherePath :: !PoolPath,
    elsewhereLabel :: !Text
  }
  deriving (Eq, Show)

-- | Whether a transaction was replaced or dropped from the network. Only
-- one on the chain can be: one in preparation is not out yet.
txInfoDead :: TxInfo -> Bool
txInfoDead info =
  case txInfoStatus info of
    OnChain status -> chainDead status
    Pending _ -> False

-- | A transaction as txs lists it, with the balance of the portfolio on
-- the network after it. A dead transaction has none: the balance leaves
-- it out.
data ListedTx = ListedTx
  { listedTxInfo :: !TxInfo,
    listedTxBalance :: !(Maybe Integer)
  }
  deriving (Eq, Show)

-- | Where txs goes on: after a transaction, by where it stands in the
-- order of txs and by its id, and with the balance before it, which is the
-- balance after the next one
data TxsCursor = TxsCursor
  { txsCursorKey :: !TimeKey,
    txsCursorTxid :: !TxHash,
    txsCursorBalance :: !Integer
  }
  deriving (Eq, Show)

{- The order of the listings that show a time -}

-- | Where a transaction or a coin stands in the listings that show a time,
-- txs and coins, and the exports of both. They list the unconfirmed ones
-- first, then the others. Each part lists the newest first, by the time
-- the views show: the time the store gives the transaction. Two rows of
-- the same time go by their block reference as the store encodes it, the
-- newest block and the last in a block first. Then they go by their key,
-- the txid or the outpoint, so that the order is settled. The transactions
-- of one block often share its time, so they keep the order of the block.
--
-- A transaction is unconfirmed when it is live and in the mempool. A dead
-- one goes among the others, at its time: it will never be confirmed.
-- Listed by block alone, a transaction that died in the mempool stayed
-- above every block for good. The block order also disagrees with the
-- times shown when a payment waits in the mempool while later ones
-- confirm. The time alone, without the two parts, would put a payment
-- whose block is dated ahead above the unconfirmed ones. Miners of test
-- networks often date blocks ahead, by up to two hours.
data TimeKey = TimeKey
  { -- | Whether it is in the first part, of the unconfirmed ones
    timeKeyUnconfirmed :: !Bool,
    timeKeyTime :: !UTCTime,
    timeKeyBlock :: !Store.BlockRef
  }
  deriving (Eq, Show)

-- | A key of the listings by time and the key of the row after it, as a
-- value that sorts in their order: the unconfirmed part first, the newest
-- first, then the block reference and the key as the database sorts them
timeOrder :: TimeKey -> KeyBytes -> (Bool, Down UTCTime, BS.ByteString, KeyBytes)
timeOrder key rowKey = (not key.timeKeyUnconfirmed, Down key.timeKeyTime, S.encode key.timeKeyBlock, rowKey)

-- | The reference where the blocks start, in the order the store encodes
-- references in: every reference of the mempool sorts before it, and
-- every reference of a block from it on
firstBlockRef :: BS.ByteString
firstBlockRef = S.encode (Store.BlockRef maxBound maxBound)

-- | The rows that come after a row in a part of the listings by time,
-- given the time, the block reference and the key of each row, and those
-- of the row: an older time, or the same time and a later block
-- reference, or the same block and a greater key. The time comes first,
-- as a range, for the index by time.
afterInTime :: (P.PersistField k) => SqlExpr (E.Value UTCTime) -> SqlExpr (E.Value BS.ByteString) -> SqlExpr (E.Value k) -> TimeKey -> k -> SqlExpr (E.Value Bool)
afterInTime time block key at rowKey =
  time <=. val at.timeKeyTime
    &&. ( time <. val at.timeKeyTime
            ||. (block >=. val blockBytes &&. (block >. val blockBytes ||. key >. val rowKey))
        )
  where
    blockBytes = S.encode at.timeKeyBlock

-- | The rows of a part of the listings by time from its top down to a
-- row, that row included: the reverse of 'afterInTime'
throughInTime :: (P.PersistField k) => SqlExpr (E.Value UTCTime) -> SqlExpr (E.Value BS.ByteString) -> SqlExpr (E.Value k) -> TimeKey -> k -> SqlExpr (E.Value Bool)
throughInTime time block key at rowKey =
  time >=. val at.timeKeyTime
    &&. ( time >. val at.timeKeyTime
            ||. (block <=. val blockBytes &&. (block <. val blockBytes ||. key <=. val rowKey))
        )
  where
    blockBytes = S.encode at.timeKeyBlock

-- | The order of a part of the listings by time, given the time, the
-- block reference and the key of each row
inTimeOrder :: (P.PersistField k) => SqlExpr (E.Value UTCTime) -> SqlExpr (E.Value BS.ByteString) -> SqlExpr (E.Value k) -> SqlQuery ()
inTimeOrder time block key = orderBy [desc time, asc block, asc key]

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
-- are written as the text of the network. An address of the portfolio is
-- written with its path below the key of its account and the type of that
-- account ('PoolPath').

instance Json.ToJSON MyOutputs where
  toJSON (MyOutputs v p l) =
    object
      [ "value" .= v,
        "path" .= pathInAccount p,
        "type" .= pathKind p,
        "label" .= l
      ]

instance Json.FromJSON MyOutputs where
  parseJSON =
    withObject "MyOutputs" $ \o ->
      MyOutputs
        <$> o .: "value"
        <*> parseJSON (Json.Object o)
        <*> o .: "label"

instance Json.ToJSON MyInputs where
  toJSON (MyInputs v p l outpoints) =
    object
      [ "value" .= v,
        "path" .= pathInAccount p,
        "type" .= pathKind p,
        "label" .= l,
        "outpoints" .= outpoints
      ]

instance Json.FromJSON MyInputs where
  parseJSON =
    withObject "MyInputs" $ \o ->
      MyInputs
        <$> o .: "value"
        <*> parseJSON (Json.Object o)
        <*> o .: "label"
        <*> o .: "outpoints"

-- | An address of another portfolio is written with the indexes of its
-- wallet and of its portfolio, then its path and its label as an address
-- of the portfolio is
instance Json.ToJSON HeldElsewhere where
  toJSON (HeldElsewhere ref p l) =
    object $
      portfolioRefPairs ref
        <> [ "path" .= pathInAccount p,
             "type" .= pathKind p,
             "label" .= l
           ]

instance Json.FromJSON HeldElsewhere where
  parseJSON =
    withObject "HeldElsewhere" $ \o ->
      HeldElsewhere
        <$> parsePortfolioRef o
        <*> parseJSON (Json.Object o)
        <*> o .: "label"

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
        <*> (maybe (return Map.empty) (mapTextAddr net) =<< o .:? "otherportfolios")

-- | The balance joins the keys of the transaction, in the same flat object
instance MarshalJSON Network ListedTx where
  marshalValue net (ListedTx info balance) =
    object $ txInfoPairs net info <> ["balance" .= balance]
  unmarshalValue net value =
    ListedTx
      <$> unmarshalValue net value
      <*> withObject "ListedTx" (.: "balance") value

-- | The keys of a transaction as hw reports it. The network and the types
-- of the portfolio's addresses it involves are written beside the
-- summary, which they follow from: the reader need not work them out, and
-- they are not read back. The addresses another portfolio of the database
-- holds are under "otherportfolios", by address. They stay among
-- "otheroutputs" and "otherinputs" too, which hold their values: a script
-- that reads those keys alone finds what it found before.
txInfoPairs :: Network -> TxInfo -> [Pair]
txInfoPairs net (TxInfo summary status label inputs elsewhere) =
  ["network" .= net.name, "types" .= summaryKinds summary]
    <> summaryPairs net summary
    <> statusPairs status
    <> [ "label" .= label,
         "inputlabels" .= txidLabelsJson inputs,
         "otherportfolios" .= mapAddrText net elsewhere
       ]

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
    <*> o .: "locktime"
    <*> (mapTextAddr net =<< o .: "myoutputs")
    <*> (mapTextAddr net =<< o .: "otheroutputs")
    <*> (mapM (unmarshalValue net) =<< o .: "nonstdoutputs")
    <*> (mapTextAddr net =<< o .: "myinputs")
    <*> (mapTextAddr net =<< o .: "otherinputs")
    <*> (mapM (unmarshalValue net) =<< o .: "nonstdinputs")

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

-- | The summary of a transaction the store reports, for a pool whose
-- addresses are given with their paths: those of every account of the
-- portfolio on the network. An output or input at an address the network
-- cannot write, a SegWit one on Bitcoin Cash for instance, is kept apart
-- like one that pays no address.
storeSummary :: Network -> Map Address PoolPath -> Store.Transaction -> TxSummary
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

-- | The block of every live transaction summary of a pool, by
-- transaction id. The dead ones are left out: the sync treats them as
-- unknown, so that a transaction the store lists again is fetched and comes
-- back to life.
txInfoBlockRefs ::
  (MonadUnliftIO m) => Pool -> ExceptT String (DB m) (Map TxHash Store.BlockRef)
txInfoBlockRefs pool = do
  rows <-
    lift . select . from $ \t -> do
      where_ $
        ofPool pool t
          &&. t ^. DBTxInfoDead ==. val False
      return (t ^. DBTxInfoTxid, t ^. DBTxInfoBlockRef)
  entries <- forM rows $ \(Value tidBytes, Value refBytes) -> do
    tid <- liftEither $ bytesToHash tidBytes
    ref <- liftEither $ S.decode refBytes
    return (tid, ref)
  return $ Map.fromList entries

-- | The summaries of a pool, in a query: those of its portfolio on its
-- network
ofPool :: Pool -> SqlExpr (Entity DBTxInfo) -> SqlExpr (E.Value Bool)
ofPool pool t =
  t ^. DBTxInfoPortfolio ==. val pool.poolPortfolio
    &&. t ^. DBTxInfoNetwork ==. val (poolNetworkName pool)

-- | The key of the summary of a transaction of a pool
txInfoKey :: Pool -> TxHash -> P.Key DBTxInfo
txInfoKey pool tid = DBTxInfoKey pool.poolPortfolio (poolNetworkName pool) (hashBytes tid)

-- | The pool of a summary row
txInfoPool :: DBTxInfo -> Either String Pool
txInfoPool row =
  maybeToEither "A stored summary names a network hw does not know" $
    Pool (dBTxInfoPortfolio row) <$> netByName (cs (dBTxInfoNetwork row))

-- | The ids of the live summaries of a pool, whatever else their rows
-- hold. A full sync goes by these alone: it rewrites the rows, so one that
-- does not read must not stop it.
liveTxids :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) (Set TxHash)
liveTxids pool = do
  rows <-
    lift . select . from $ \t -> do
      where_ $
        ofPool pool t
          &&. t ^. DBTxInfoDead ==. val False
      return (t ^. DBTxInfoTxid)
  Set.fromList <$> liftEither (mapM (bytesToHash . unValue) rows)

-- | Whether a summary is of an unconfirmed transaction, live and in the
-- mempool. The index on the block references finds them: a reference of
-- the mempool sorts before every reference of a block.
summaryUnconfirmed :: SqlExpr (Entity DBTxInfo) -> SqlExpr (E.Value Bool)
summaryUnconfirmed t =
  t ^. DBTxInfoBlockRef <. val firstBlockRef
    &&. t ^. DBTxInfoDead ==. val False

-- | The live transactions of the pool in the mempool, with their
-- summaries, by their ids, up to the count, or all of them
mempoolSummaries :: (MonadUnliftIO m) => Pool -> Maybe Natural -> ExceptT String (DB m) [(TxHash, TxSummary)]
mempoolSummaries pool countM = do
  rows <-
    lift . select . from $ \t -> do
      where_ $ ofPool pool t &&. summaryUnconfirmed t
      orderBy [asc (t ^. DBTxInfoTxid)]
      forM_ countM $ limit . rowLimit
      return (t ^. DBTxInfoTxid, t ^. DBTxInfoBlob)
  forM rows $ \(Value tidBytes, Value blob) -> do
    tid <- liftEither $ bytesToHash tidBytes
    summary <- liftEither $ decodeSummary pool.poolNetwork tid blob
    return (tid, summary)

-- | The addresses of the pool that its live transactions in the mempool
-- spend from or pay, as their summaries name them. An incremental sync
-- looks at these addresses whatever their balance
-- ('Haskoin.Wallet.Sync.balancesToLookAt'). A replacement can leave the
-- balance of an address exactly as it was. It does so when it pays the
-- address what the transaction it replaces paid, or spends the same coin
-- from it. The balance then cannot tell the sync that the transaction
-- changed.
mempoolAddresses :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) (Set Address)
mempoolAddresses pool = do
  inMempool <- mempoolSummaries pool Nothing
  return $ Set.fromList $ concatMap (summaryAddresses . snd) inMempool

-- | The addresses of the portfolio that a transaction spends from or pays
summaryAddresses :: TxSummary -> [Address]
summaryAddresses summary = Map.keys (summaryMyInputs summary) <> Map.keys (summaryMyOutputs summary)

-- | The stored summary of a live transaction of the pool, with its
-- block; nothing when the sync has not stored it or found it dead
liveSummary ::
  (MonadUnliftIO m) => Pool -> TxHash -> ExceptT String (DB m) (Maybe (Store.BlockRef, TxSummary))
liveSummary pool tid = do
  rowM <- lift $ P.get (txInfoKey pool tid)
  forM (mfilter (not . dBTxInfoDead) rowM) $ \row -> do
    ref <- liftEither $ S.decode (dBTxInfoBlockRef row)
    summary <- liftEither $ decodeSummary pool.poolNetwork tid (dBTxInfoBlob row)
    return (ref, summary)

-- | The stored summaries of a live transaction, one for each portfolio
-- the sync has stored it for on the network, each with its block: none
-- when the sync has stored it for no portfolio, or found it dead for
-- each. A transaction from before the fork of Bitcoin Cash has the same
-- id on both chains, so the summaries of the other network are left out.
--
-- The key of a summary starts with its portfolio and its network, and no
-- index starts with the txid: a query by the txid alone would read the
-- whole table, for every coin whose transaction 'trustedTx' looks at. So
-- the query goes over the portfolios, a few rows, and looks up the
-- summary of each by its key ('liveSummariesQuery').
liveSummaries :: (MonadUnliftIO m) => Network -> TxHash -> ExceptT String (DB m) [(Store.BlockRef, TxSummary)]
liveSummaries net tid = do
  rows <- lift . select $ liveSummariesQuery net tid
  forM rows $ \(Value refBytes, Value blob) -> do
    ref <- liftEither $ S.decode refBytes
    summary <- liftEither $ decodeSummary net tid blob
    return (ref, summary)

-- | The query of 'liveSummaries': the block and the blob of the live
-- summary of the transaction in each portfolio on the network. It goes
-- over the portfolios, and looks up the summary of each by its key. The
-- cross join tells SQLite to take the portfolios first: an inner join
-- leaves it the order, and with no statistics of the tables it reads
-- every summary instead, then their portfolios.
liveSummariesQuery :: Network -> TxHash -> SqlQuery (SqlExpr (E.Value BS.ByteString), SqlExpr (E.Value BS.ByteString))
liveSummariesQuery net tid =
  from $ \(p `CrossJoin` t) -> do
    where_ $
      t ^. DBTxInfoPortfolio ==. p ^. DBPortfolioId
        &&. t ^. DBTxInfoNetwork ==. val (cs net.name)
        &&. t ^. DBTxInfoTxid ==. val (hashBytes tid)
        &&. t ^. DBTxInfoDead ==. val False
    return (t ^. DBTxInfoBlockRef, t ^. DBTxInfoBlob)

-- | A summary as the database stores it
decodeSummary :: Network -> TxHash -> BS.ByteString -> Either String TxSummary
decodeSummary net tid blob =
  maybeToEither ("The stored summary of transaction " <> cs (txHashToHex tid) <> " does not decode") $
    unmarshalJSON net (BS.fromStrict blob)

-- | Mark the summary of a transaction dead: the store no longer has it, or
-- reports it replaced or dropped. The summary stays, for the history.
markTxInfoDead :: (MonadUnliftIO m) => Pool -> TxHash -> DB m ()
markTxInfoDead pool tid =
  P.update (txInfoKey pool tid) [DBTxInfoDead P.=. True]

-- | Delete the dead summaries of the pool that do not read. The store
-- no longer lists their transactions, so nothing can write them again, and
-- one of them would stop txs for good: it refuses a row that does not read.
-- A full sync does this last, when every live summary has just been
-- written.
dropUnreadableDead :: (MonadUnliftIO m) => Pool -> DB m ()
dropUnreadableDead pool = do
  rows <- P.selectList [DBTxInfoPortfolio P.==. pool.poolPortfolio, DBTxInfoNetwork P.==. poolNetworkName pool, DBTxInfoDead P.==. True] []
  forM_ rows $ \(Entity key row) ->
    when (isLeft (readable row)) $ P.delete key
  where
    readable row = do
      tid <- bytesToHash (dBTxInfoTxid row)
      _ <- S.decode (dBTxInfoBlockRef row) :: Either String Store.BlockRef
      decodeSummary pool.poolNetwork tid (dBTxInfoBlob row)

-- | The time the store gives a transaction, in seconds since 1970
storeTime :: Word64 -> UTCTime
storeTime = posixSecondsToUTCTime . fromIntegral

-- | Store the summary of a live transaction of the pool at its block,
-- with the time the store gives it, or replace the one stored, which
-- brings a summary marked dead back. Returns whether the row changed.
repsertSummary ::
  (MonadUnliftIO m) => Pool -> TxHash -> Store.BlockRef -> UTCTime -> TxSummary -> DB m Bool
repsertSummary pool tid ref time summary = do
  now <- liftIO getCurrentTime
  let key = txInfoKey pool tid
      net = pool.poolNetwork
      row =
        DBTxInfo
          { dBTxInfoPortfolio = pool.poolPortfolio,
            dBTxInfoNetwork = poolNetworkName pool,
            dBTxInfoTxid = hashBytes tid,
            dBTxInfoBlockRef = S.encode ref,
            dBTxInfoTime = time,
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

-- | Where the summary of a row stands in the order of txs ('TimeKey')
summaryTimeKey :: DBTxInfo -> Either String TimeKey
summaryTimeKey row = do
  ref <- S.decode (dBTxInfoBlockRef row)
  return
    TimeKey
      { timeKeyUnconfirmed = not (dBTxInfoDead row) && not (Store.confirmed ref),
        timeKeyTime = dBTxInfoTime row,
        timeKeyBlock = ref
      }

-- | A summary row as a value that sorts in the order of txs
summaryOrder :: DBTxInfo -> Either String (Bool, Down UTCTime, BS.ByteString, KeyBytes)
summaryOrder row = (`timeOrder` dBTxInfoTxid row) <$> summaryTimeKey row

-- | Each summary row with the value it sorts by in the order of txs
withSummaryOrder :: [DBTxInfo] -> Either String [((Bool, Down UTCTime, BS.ByteString, KeyBytes), DBTxInfo)]
withSummaryOrder = mapM (\row -> (\order -> (order, row)) <$> summaryOrder row)

-- | The summaries of the unconfirmed transactions of the pool, the first
-- part of txs, in their order. The index on the block references finds
-- them, and they are few, so they are sorted here.
unconfirmedSummaryRows :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) [DBTxInfo]
unconfirmedSummaryRows pool = do
  rows <- lift . select $ unconfirmedSummariesQuery pool
  keyed <- liftEither $ withSummaryOrder (entityVal <$> rows)
  return $ snd <$> sortOn fst keyed

-- | The query of 'unconfirmedSummaryRows', in no order
unconfirmedSummariesQuery :: Pool -> SqlQuery (SqlExpr (Entity DBTxInfo))
unconfirmedSummariesQuery pool =
  from $ \t -> do
    where_ $ ofPool pool t &&. summaryUnconfirmed t
    return t

-- | The query of the second part of txs: the summaries of the pool that
-- are not of an unconfirmed transaction, in their order, after the
-- transaction given in that part, up to the count. The index by time gives
-- them in that order, from the one given on.
otherSummariesQuery :: Pool -> Maybe (TimeKey, TxHash) -> Maybe Natural -> SqlQuery (SqlExpr (Entity DBTxInfo))
otherSummariesQuery pool afterM countM =
  from $ \t -> do
    where_ $ ofPool pool t &&. not_ (summaryUnconfirmed t)
    forM_ afterM $ \(key, tid) ->
      where_ $ afterInTime (t ^. DBTxInfoTime) (t ^. DBTxInfoBlockRef) (t ^. DBTxInfoTxid) key (hashBytes tid)
    inTimeOrder (t ^. DBTxInfoTime) (t ^. DBTxInfoBlockRef) (t ^. DBTxInfoTxid)
    forM_ countM $ limit . rowLimit
    return t

-- | The summary rows of txs after the transaction given, by where it
-- stands and its id, or the first ones, up to the count, or all of them.
-- The first part, of the unconfirmed transactions, comes whole from
-- 'unconfirmedSummaryRows'. The second part comes from the index by time.
summaryRowsAfter :: (MonadUnliftIO m) => Pool -> Maybe (TimeKey, TxHash) -> Maybe Natural -> ExceptT String (DB m) [DBTxInfo]
summaryRowsAfter pool afterM countM = do
  firstPart <- case afterM of
    -- A cursor in the second part is past the whole first part
    Just (key, _) | not key.timeKeyUnconfirmed -> return []
    _ -> do
      rows <- unconfirmedSummaryRows pool
      past <- case afterM of
        Nothing -> return rows
        Just (key, tid) -> do
          let cursor = timeOrder key (hashBytes tid)
          keyed <- liftEither $ withSummaryOrder rows
          return [row | (order, row) <- keyed, order > cursor]
      return $ maybe id genericTake countM past
  let left = (\n -> n - genericLength firstPart) <$> countM
      -- The cursor, when it is in the second part
      inSecond = mfilter (not . (.timeKeyUnconfirmed) . fst) afterM
  secondPart <-
    if left == Just 0
      then return []
      else map entityVal <$> lift (select (otherSummariesQuery pool inSecond left))
  return (firstPart <> secondPart)

-- | The pool's transactions on the chain in the order of txs ('TimeKey'):
-- those after the transaction given, by where it stands and its id, or
-- the first ones, up to the count, or all of them. Each is as
-- 'chainTxInfo' reports it.
txsAfter :: (MonadUnliftIO m) => Pool -> Maybe (TimeKey, TxHash) -> Maybe Natural -> ExceptT String (DB m) [TxInfo]
txsAfter pool afterM countM = do
  rows <- summaryRowsAfter pool afterM countM
  heightM <- bestHeight pool.poolNetwork
  forM rows $ chainTxInfo heightM

-- | What a pool holds, confirmed or not: the totals of its accounts added
-- up, the balance after its newest transaction
poolTotal :: (MonadUnliftIO m) => Pool -> DB m Integer
poolTotal pool = sum . map (accountTotal . snd) <$> poolAccounts pool

-- | The transactions of txs after the cursor, or the first ones, up to the
-- count, or all of them. Each comes with the cursor to go on after it,
-- which carries the balance before it. The first transaction of the
-- listing starts from what the pool holds, and each one below from the
-- balance before the one above it.
listedTxsAfter :: (MonadUnliftIO m) => Pool -> Maybe TxsCursor -> Maybe Natural -> ExceptT String (DB m) [(ListedTx, TxsCursor)]
listedTxsAfter pool afterM countM = do
  total <- lift $ poolTotal pool
  rows <- summaryRowsAfter pool (position <$> afterM) countM
  heightM <- bestHeight pool.poolNetwork
  listed <- forM rows $ \row -> do
    info <- chainTxInfo heightM row
    key <- liftEither $ summaryTimeKey row
    tid <- liftEither $ bytesToHash (dBTxInfoTxid row)
    return (info, key, tid)
  let start = maybe total txsCursorBalance afterM
  return $ snd (mapAccumL withBalance start listed)
  where
    position cursor = (txsCursorKey cursor, txsCursorTxid cursor)

-- | A transaction with the balance after it, and the cursor after it,
-- which carries that balance without the transaction: the balance before
-- it, and after the next one down. A dead transaction takes nothing away,
-- since the balance leaves it out.
withBalance :: Integer -> (TxInfo, TimeKey, TxHash) -> (Integer, (ListedTx, TxsCursor))
withBalance after (info, key, tid)
  | txInfoDead info = (after, (ListedTx info Nothing, TxsCursor key tid after))
  | otherwise = (before, (ListedTx info (Just after), TxsCursor key tid before))
  where
    before = after - summaryAmount (txInfoSummary info)

-- | The cursor of the transaction --after names, whose balance takes away
-- every transaction down to that one
txsCursorAt :: (MonadUnliftIO m) => Pool -> TxHash -> ExceptT String (DB m) TxsCursor
txsCursorAt pool tid = do
  key <- txTimeKey pool tid
  above <- amountsThrough pool key tid
  total <- lift $ poolTotal pool
  return $ TxsCursor key tid (total - above)

-- | What the live transactions of the pool add up to, from the top of
-- txs down to the transaction given, that one included. The unconfirmed
-- ones are few, and are added up here. The index by time holds the
-- amounts of the others, so their sum reads no row.
amountsThrough :: (MonadUnliftIO m) => Pool -> TimeKey -> TxHash -> ExceptT String (DB m) Integer
amountsThrough pool key tid = do
  unconfirmed <- unconfirmedSummaryRows pool
  keyed <- liftEither $ withSummaryOrder unconfirmed
  let cursor = timeOrder key (hashBytes tid)
      firstPart = sum [toInteger (dBTxInfoAmount row) | (order, row) <- keyed, order <= cursor]
  if key.timeKeyUnconfirmed
    then return firstPart
    else (firstPart +) <$> lift (confirmedAmountsThrough pool key tid)

-- | What the live transactions of the second part of txs add up to, from
-- its top down to the transaction given, that one included. A live
-- transaction there is a confirmed one: the dead ones add nothing.
confirmedAmountsThrough :: (MonadUnliftIO m) => Pool -> TimeKey -> TxHash -> DB m Integer
confirmedAmountsThrough pool key tid = do
  total <- selectOne $ confirmedAmountsQuery pool key tid
  return $ toInteger (joinMaybe (0 :: Int64) id total)

-- | The query of 'confirmedAmountsThrough'
confirmedAmountsQuery :: Pool -> TimeKey -> TxHash -> SqlQuery (SqlExpr (E.Value (Maybe Int64)))
confirmedAmountsQuery pool key tid =
  from $ \t -> do
    where_ $
      ofPool pool t
        &&. t ^. DBTxInfoDead ==. val False
        &&. t ^. DBTxInfoBlockRef >=. val firstBlockRef
        &&. throughInTime (t ^. DBTxInfoTime) (t ^. DBTxInfoBlockRef) (t ^. DBTxInfoTxid) key (hashBytes tid)
    return $ sum_ (t ^. DBTxInfoAmount)

-- | A transaction of the pool on the chain, as txs shows it, if the
-- pool has it, dead or alive
storedTxInfo :: (MonadUnliftIO m) => Pool -> TxHash -> ExceptT String (DB m) (Maybe TxInfo)
storedTxInfo pool tid = do
  rowM <- lift $ P.get (txInfoKey pool tid)
  heightM <- bestHeight pool.poolNetwork
  forM rowM $ chainTxInfo heightM

-- | The stored summary of a transaction on the chain as hw reports it. Its
-- confirmations are counted from the best height given, none for a dead
-- transaction. Its label and the labels of its addresses are those of
-- today.
chainTxInfo :: (MonadUnliftIO m) => Maybe BlockHeight -> DBTxInfo -> ExceptT String (DB m) TxInfo
chainTxInfo heightM row = do
  pool <- liftEither $ txInfoPool row
  let net = pool.poolNetwork
  tid <- liftEither $ bytesToHash (dBTxInfoTxid row)
  ref <- liftEither $ S.decode (dBTxInfoBlockRef row)
  summary <- liftEither $ decodeSummary net tid (dBTxInfoBlob row)
  let dead = dBTxInfoDead row
      confirmations = if dead then 0 else getConfirmations heightM ref
      status = ChainStatus tid ref (Just (dBTxInfoTime row)) confirmations dead
  label <- lift $ txLabel pool tid
  lift $ fillTxInfo pool $ TxInfo summary (OnChain status) label Map.empty Map.empty

-- | Where a transaction of the pool stands in the order of txs, for the
-- one --after names, or why the pool does not have it
txTimeKey :: (MonadUnliftIO m) => Pool -> TxHash -> ExceptT String (DB m) TimeKey
txTimeKey pool tid = do
  rowM <- lift $ P.get (txInfoKey pool tid)
  row <- liftMaybe ("The transaction " <> cs (txHashToHex tid) <> " is not in the portfolio on " <> pool.poolNetwork.name) rowM
  liftEither $ summaryTimeKey row

-- | At most the given number of ids of the pool's transactions on the
-- chain that start with the hexadecimal digits, in the order of their ids:
-- for the shell. The key holds the bytes of an id in the order hw shows
-- them, so hex() writes its digits as they are typed, in capitals.
txidsStarting :: (MonadUnliftIO m) => Pool -> String -> Natural -> DB m [TxHash]
txidsStarting pool digits atMost = do
  rows <-
    rawSql
      "select txid from d_b_tx_info where portfolio = ? and network = ? and substr(hex(txid), 1, ?) = ? order by txid limit ?"
      [P.toPersistValue pool.poolPortfolio, P.toPersistValue (poolNetworkName pool), P.toPersistValue (length digits), P.toPersistValue (map toUpper digits), P.toPersistValue (rowLimit atMost :: Int)]
  return $ rights (bytesToHash . unSingle <$> rows)

-- | The txids of the pool's transactions on the chain that have the
-- short id. The key holds the bytes of a txid in the order hw shows them,
-- so the txids that start with the short id are a range of the index of
-- the keys ('shortIdRange'), which the query reads alone.
txidsWith :: (MonadUnliftIO m) => Pool -> ShortId -> DB m [TxHash]
txidsWith pool short = do
  keys <-
    P.selectKeysList
      ( [DBTxInfoPortfolio P.==. pool.poolPortfolio, DBTxInfoNetwork P.==. poolNetworkName pool, DBTxInfoTxid P.>=. KeyBytes low]
          <> [DBTxInfoTxid P.<. KeyBytes high | Just high <- [highM]]
      )
      []
  return $ rights [bytesToHash txid | DBTxInfoKey _ _ txid <- keys]
  where
    (low, highM) = shortIdRange short

-- | Every pool of the database: each portfolio on each network it has an
-- account on, in the order of the wallets and of their portfolios
allPools :: (MonadUnliftIO m) => DB m [Pool]
allPools = do
  portfolios <- getAllPortfolios
  concat <$> forM portfolios (portfolioPools . (.portfolioId))

-- | The txids of the transactions on the chain that have the short id,
-- in every portfolio, each once: those a command can name by a txid,
-- which it then looks for in the portfolio it acts on
walletTxidsWith :: (MonadUnliftIO m) => ShortId -> DB m [TxHash]
walletTxidsWith short = do
  pools <- allPools
  nubOrd . concat <$> forM pools (`txidsWith` short)

-- | The transaction with what the database knows of it now: its labels
-- ('fillTxInfoLabels'), and the addresses of other portfolios it spends
-- from or pays ('fillElsewhere'). A transaction the wallet holds is shown
-- so.
fillTxInfo :: (MonadUnliftIO m) => Pool -> TxInfo -> DB m TxInfo
fillTxInfo pool info = fillElsewhere pool =<< fillTxInfoLabels pool info

-- | The transaction of the pool with the addresses that another portfolio
-- of the database holds, among those its summary counts as of others
-- ('heldElsewhere'). A transaction read from a file is filled in this way
-- too. Its labels stay as the file carries them. No file says which
-- portfolio holds an address it pays, so the database must.
fillElsewhere :: (MonadUnliftIO m) => Pool -> TxInfo -> DB m TxInfo
fillElsewhere pool info = do
  let summary = txInfoSummary info
  elsewhere <- heldElsewhere pool (Map.keys (summaryOtherOutputs summary) <> Map.keys (summaryOtherInputs summary))
  return info {txInfoElsewhere = elsewhere}

-- | The addresses among the given ones that another portfolio of the
-- database holds on the network of the pool. Each comes with its
-- portfolio, its path there and its label there. An address is of one
-- portfolio at most on a network, since a key is in the database once on
-- a network.
--
-- The database knows an address of a portfolio once the portfolio gave it
-- out or a sync found it. An address further down a chain has no row, so
-- it is not found. It shows as an address of others, as it did before. An
-- offline computer that gives out no address finds none.
--
-- A database of one portfolio has no other account to look in, and reads
-- no address here. Otherwise each account of the other portfolios on the
-- network is asked for the addresses, with one query for each account.
heldElsewhere :: (MonadUnliftIO m) => Pool -> [Address] -> DB m (Map Address HeldElsewhere)
heldElsewhere pool addrs
  | null addrs = return Map.empty
  | otherwise = do
      accounts <- getAccounts
      let net = pool.poolNetwork
          others =
            [ (portfolio, (accId, acc))
              | (portfolio, accId, acc) <- accounts,
                portfolio.portfolioId /= pool.poolPortfolio,
                accountNetwork acc == net
            ]
      found <- forM others $ \(portfolio, account) -> do
        rows <- storedAmong addrs account
        return
          [ (addr, HeldElsewhere (portfolioRef portfolio) path (dBAddressLabel row))
            | row <- rows,
              Just addr <- [textToAddr net (dBAddressAddress row)],
              Just path <- [addrPoolPath net row]
          ]
      return $ Map.fromList (concat found)

-- | The transaction with its labels filled in as the database has them
-- now: those of the portfolio's addresses, and those the coins it spends
-- show, by the transaction that paid each (see 'coinLabel')
fillTxInfoLabels :: (MonadUnliftIO m) => Pool -> TxInfo -> DB m TxInfo
fillTxInfoLabels pool info = do
  labelled <- relabelAddresses (addressLabel pool) info
  inputLabels <- coinLabels pool (spentTxids (txInfoSummary info))
  return labelled {txInfoInputLabels = inputLabels}

-- | The transactions the coins of the portfolio a transaction spends come
-- from, each once
spentTxids :: TxSummary -> [TxHash]
spentTxids summary = nubOrd [outpoint.hash | input <- Map.elems (summaryMyInputs summary), outpoint <- myInputsOutpoints input]

-- | The label a coin of the transaction shows: the label of the
-- transaction, which paid the coin to the portfolio or moved it between
-- its own addresses. The change of a payment the portfolio made shows
-- none: the label says what the payment was for, not what its change is.
-- None either without a label, or when the pool has no such transaction.
coinLabel :: (MonadUnliftIO m) => Pool -> TxHash -> DB m Text
coinLabel pool tid = do
  label <- txLabel pool tid
  if Text.null label
    then return ""
    else do
      rowM <- P.get (txInfoKey pool tid)
      let summaryM = decodeSummary pool.poolNetwork tid . dBTxInfoBlob =<< maybe (Left "no such transaction") Right rowM
      return $ case summaryM of
        Right summary | summaryType summary /= TxDebit -> label
        _ -> ""

-- | The labels the coins of the transactions show ('coinLabel'), by
-- transaction, those that have one
coinLabels :: (MonadUnliftIO m) => Pool -> [TxHash] -> DB m (Map TxHash Text)
coinLabels pool tids = do
  labelled <- forM (nubOrd tids) $ \tid -> (,) tid <$> coinLabel pool tid
  return $ Map.fromList [(tid, label) | (tid, label) <- labelled, not (Text.null label)]

-- | The times of transactions of the pool, by their ids: when the store
-- first saw each, as the history of the pool holds it. The sync gives a
-- coin the time of the transaction that paid it, which coins shows and
-- lists it by, as txs does a transaction. A transaction the pool has no
-- summary of is left out.
transactionTimes :: (MonadUnliftIO m) => Pool -> [TxHash] -> DB m (Map TxHash UTCTime)
transactionTimes pool tids = do
  found <- forM (nubOrd tids) $ \tid -> fmap (\row -> (tid, dBTxInfoTime row)) <$> P.get (txInfoKey pool tid)
  return $ Map.fromList [entry | Just entry <- found]

-- | The transaction with the labels of the portfolio's addresses, those it
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

-- | The transaction with the labels of the portfolio's addresses, those it
-- spends from and those it pays, as the function gives them by the path
-- of each: from the file that carried the transaction
relabelPaths :: (PoolPath -> Text) -> TxInfo -> TxInfo
relabelPaths labelOf info =
  info {txInfoSummary = summary {summaryMyOutputs = outputs, summaryMyInputs = inputs}}
  where
    summary = txInfoSummary info
    outputs = Map.map (\o -> o {myOutputsLabel = labelOf (myOutputsPath o)}) (summaryMyOutputs summary)
    inputs = Map.map (\i -> i {myInputsLabel = labelOf (myInputsPath i)}) (summaryMyInputs summary)

-- | The label of an address of the pool, or none
addressLabel :: (MonadUnliftIO m) => Pool -> Address -> DB m Text
addressLabel pool addr = maybe "" dBAddressLabel <$> poolAddressRow pool addr
