{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The coins of an account, the unspent outputs the store knows at its
-- addresses: kept up to date by the sync, listed for the user and offered
-- to the transaction builder when they can be spent.
module Haskoin.Wallet.Coin
  ( JsonCoin (..),
    coinOutpoint,
    toJsonCoin,
    groupCoins,
    coinPage,
    spendableCoins,
    Refreshed (..),
    refreshCoins,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Serialize as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.PendingTx (lockedOutpoints)
import Haskoin.Wallet.RawTx (lookupRawTx)
import Haskoin.Wallet.TxInfo (MyInputs (..), TxSummary (..), liveSummary, ownTransaction)
import Haskoin.Wallet.Util (Page (Page))
import Numeric.Natural (Natural)

{- Coins as the user sees them -}

data JsonCoin = JsonCoin
  { jsonCoinOutpoint :: !OutPoint,
    jsonCoinAddress :: !Address,
    jsonCoinValue :: !Word64,
    jsonCoinBlock :: !Store.BlockRef,
    jsonCoinConfirmations :: !Natural,
    jsonCoinLocked :: !Bool
  }
  deriving (Eq, Show)

instance MarshalJSON Network JsonCoin where
  marshalValue net c =
    object
      [ "outpoint" .= jsonCoinOutpoint c,
        "address" .= marshalValue net (jsonCoinAddress c),
        "value" .= jsonCoinValue c,
        "block" .= jsonCoinBlock c,
        "confirmations" .= jsonCoinConfirmations c,
        "locked" .= jsonCoinLocked c
      ]
  unmarshalValue net =
    withObject "JsonCoin" $ \o ->
      JsonCoin
        <$> o .: "outpoint"
        <*> (unmarshalValue net =<< o .: "address")
        <*> o .: "value"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "locked"

-- | The outpoint of a coin row
coinOutpoint :: DBCoin -> Either String OutPoint
coinOutpoint row =
  readStored "A stored coin has an outpoint that does not read" $
    bytesToOutpoint (dBCoinOutpoint row)

-- | A coin row as the user sees it: its confirmations counted from the best
-- block, and locked when a pending transaction spends it
toJsonCoin :: Network -> Maybe BlockHeight -> Set OutPoint -> DBCoin -> Either String JsonCoin
toJsonCoin net bestM locked row = do
  outpoint <- coinOutpoint row
  let addrText = dBCoinAddress row
      problem what = "The stored coin " <> coinText outpoint <> " has " <> what
  addr <-
    maybeToEither (problem $ "an address that does not read: " <> cs addrText) $
      textToAddr net addrText
  block <- readStored (problem "a block that does not read") $ S.decode (dBCoinBlockRef row)
  return
    JsonCoin
      { jsonCoinOutpoint = outpoint,
        jsonCoinAddress = addr,
        jsonCoinValue = dBCoinValue row,
        jsonCoinBlock = block,
        jsonCoinConfirmations = getConfirmations bestM block,
        jsonCoinLocked = outpoint `Set.member` locked
      }

-- | The coin of a row as the store last reported it
storedUnspent :: DBCoin -> Either String Store.Unspent
storedUnspent row = do
  outpoint <- coinOutpoint row
  readStored ("The stored coin " <> coinText outpoint <> " does not read") $ S.decode (dBCoinBlob row)

-- | A coin in a message: the id of its transaction and its output index
coinText :: OutPoint -> String
coinText outpoint = cs (txHashToHex outpoint.hash) <> " " <> show outpoint.index

-- | A value read back from a row, or the given message when it does not read
readStored :: String -> Either String a -> Either String a
readStored message = either (const $ Left message) Right

-- | The coins of the store by the text of their address, each coin once,
-- by its outpoint as the rows key it. A coin without an address, or at one
-- the network cannot write, is left out.
groupCoins :: Network -> [Store.Unspent] -> Map Text (Map KeyBytes Store.Unspent)
groupCoins net coins = Map.fromListWith Map.union (mapMaybe entry coins)
  where
    entry coin = do
      addr <- addrToText net =<< coin.address
      return (addr, Map.singleton (outpointBytes coin.outpoint) coin)

{- Listing -}

-- | A page of the coins of the account, spendable or not, in the order of
-- their block references as the store encodes them: the mempool first,
-- then the newest blocks first. Coins with the same block reference, the
-- outputs of one transaction for instance, follow the order of their
-- outpoints, so that no coin shows on two pages.
coinPage :: (MonadUnliftIO m) => Network -> DBAccountId -> Page -> ExceptT String (DB m) [JsonCoin]
coinPage net accId (Page lim off) = do
  locked <- lockedOutpoints
  bestM <- lift $ getBest net
  rows <-
    lift $
      P.selectList
        [DBCoinAccount P.==. accId]
        [P.Asc DBCoinBlockRef, P.Asc DBCoinOutpoint, P.LimitTo (fromIntegral lim), P.OffsetBy (fromIntegral off)]
  liftEither $ mapM (toJsonCoin net (snd <$> bestM) locked . P.entityVal) rows

{- Spending -}

-- | The coins the builder may spend: those with at least the confirmations
-- asked that no pending transaction holds. A coin from a coinbase also
-- waits until the coinbase matures, and an unconfirmed coin must come from
-- a transaction the account trusts ('trustedTx'): its own change.
spendableCoins :: (MonadUnliftIO m) => Network -> DBAccountId -> Natural -> ExceptT String (DB m) [Store.Unspent]
spendableCoins net accId minConf = do
  bestM <- lift $ getBest net
  locked <- lockedOutpoints
  rows <- lift $ P.selectList [DBCoinAccount P.==. accId] []
  coins <- liftEither $ mapM (storedUnspent . P.entityVal) rows
  let confirmations coin = getConfirmations (snd <$> bestM) coin.block
      spendable coin
        | confirmations coin == 0 = trustedTx net accId coin.outpoint.hash
        | confirmations coin < coinbaseMaturity = not <$> isCoinbaseTx coin.outpoint.hash
        | otherwise = return True
  filterM spendable [coin | coin <- coins, confirmations coin >= minConf, coin.outpoint `Set.notMember` locked]

-- | The confirmations a coinbase needs before it can be spent: consensus
-- lets a transaction spend it in the 100th block after its own, so from
-- the mempool once it has 100
coinbaseMaturity :: Natural
coinbaseMaturity = 100

-- | Whether the transaction is a coinbase, as the raw transaction stored for
-- its coins shows. One whose raw transaction is missing counts as an
-- ordinary transaction: the builder needs the raw transaction of every coin
-- it spends, and fails without it.
isCoinbaseTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) Bool
isCoinbaseTx tid = maybe False coinbase <$> lookupRawTx tid
  where
    -- A coinbase has a single input, which spends nothing
    coinbase tx = map (.outpoint) tx.inputs == [nullOutPoint]

-- | Whether the coins of a transaction of the account can be spent: it is
-- confirmed, or the account made it from coins that can be spent by this
-- same rule. Whoever pays the account can replace their transaction until
-- it confirms, which also undoes every transaction built on it. A
-- transaction the sync has not stored, or has found dead, is not trusted.
trustedTx :: (MonadUnliftIO m) => Network -> DBAccountId -> TxHash -> ExceptT String (DB m) Bool
trustedTx net accId tid = do
  found <- liveSummary net accId tid
  case found of
    Just (block, summary)
      | Store.confirmed block -> return True
      | ownTransaction summary -> and <$> mapM (trustedTx net accId) (spentTxs summary)
    _ -> return False
  where
    spentTxs summary =
      nubOrd [outpoint.hash | input <- Map.elems summary.summaryMyInputs, outpoint <- input.myInputsOutpoints]

{- Refreshing -}

-- | What a refresh changed: how many coins were inserted, moved to another
-- block or deleted, and the coins inserted and deleted
data Refreshed = Refreshed
  { refreshedCount :: !Int,
    insertedCoins :: ![DBCoin],
    deletedCoins :: ![DBCoin]
  }
  deriving (Eq, Show)

instance Semigroup Refreshed where
  a <> b =
    Refreshed
      { refreshedCount = a.refreshedCount + b.refreshedCount,
        insertedCoins = a.insertedCoins <> b.insertedCoins,
        deletedCoins = a.deletedCoins <> b.deletedCoins
      }

instance Monoid Refreshed where
  mempty = Refreshed 0 [] []

-- | Bring the coins of the account at the addresses in line with what the
-- store lists: insert the coins that are new, move those whose block
-- changed, delete those the store no longer lists. A coin the store lists
-- twice is stored once.
refreshCoins ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  [Address] ->
  [Store.Unspent] ->
  ExceptT String (DB m) Refreshed
refreshCoins net accId addrs unspents = mconcat <$> mapM refreshAddress addrs
  where
    listed = groupCoins net unspents
    refreshAddress addr = do
      addrText <-
        liftMaybe "The coins of an address of another network cannot be refreshed" $
          addrToText net addr
      rows <- lift $ P.selectList [DBCoinAccount P.==. accId, DBCoinAddress P.==. addrText] []
      let stored = Map.fromList [(dBCoinOutpoint row, row) | P.Entity _ row <- rows]
          current = Map.findWithDefault Map.empty addrText listed
          gone = Map.elems $ stored `Map.difference` current
          new = Map.elems $ current `Map.difference` stored
          moved =
            [ coin
              | (coin, row) <- Map.elems (Map.intersectionWith (,) current stored),
                S.encode coin.block /= dBCoinBlockRef row
            ]
      lift $ forM_ gone deleteCoin
      lift $ forM_ moved moveCoin
      inserted <- mapM (insertCoin net accId) new
      return
        Refreshed
          { refreshedCount = length gone + length moved + length new,
            insertedCoins = inserted,
            deletedCoins = gone
          }

-- | Store a coin of the account, at the address it pays
insertCoin :: (MonadUnliftIO m) => Network -> DBAccountId -> Store.Unspent -> ExceptT String (DB m) DBCoin
insertCoin net accId coin = do
  addr <-
    liftMaybe ("The coin " <> coinText coin.outpoint <> " pays no address of the network") $
      addrToText net =<< coin.address
  time <- liftIO getCurrentTime
  let row =
        DBCoin
          { dBCoinAccount = accId,
            dBCoinOutpoint = outpointBytes coin.outpoint,
            dBCoinAddress = addr,
            dBCoinValue = coin.value,
            dBCoinBlockRef = S.encode coin.block,
            dBCoinBlob = S.encode coin,
            dBCoinCreated = time
          }
  lift $ P.insert_ row
  return row

-- | Move a stored coin to the block the store now reports for it
moveCoin :: (MonadUnliftIO m) => Store.Unspent -> DB m ()
moveCoin coin =
  P.update
    (DBCoinKey $ outpointBytes coin.outpoint)
    [DBCoinBlockRef P.=. S.encode coin.block, DBCoinBlob P.=. S.encode coin]

-- | Delete a stored coin
deleteCoin :: (MonadUnliftIO m) => DBCoin -> DB m ()
deleteCoin row = P.delete (DBCoinKey $ dBCoinOutpoint row)
