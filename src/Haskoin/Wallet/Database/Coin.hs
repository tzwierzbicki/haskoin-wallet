{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The coins of the accounts, the unspent outputs the store knows at
-- their addresses: kept up to date by the sync, listed for the user and
-- offered to the transaction builder when they can be spent. Whether a
-- coin can be spent (@canSpend@) is computed when it is read, from the
-- coins, the pending transactions and the best block, and stored nowhere:
-- so is the spendable balance of a pool and of its addresses.
--
-- A coin is at an address, so its row names the account of that address.
-- A payment spends from a pool, the accounts of a portfolio on one
-- network together: the coins of a pool are those of its accounts, BIP44
-- and BIP84 ones mixed ('spendableCoins'). A coin is keyed by its account
-- and its outpoint. The test networks share their keys, and a store that
-- serves two of them the same chain reports the same coin on both.
--
-- A coin also keeps the time of the transaction that paid it, which the
-- sync writes from the summary of that transaction, and coins shows.
--
-- = Three locks, three keys
--
-- A lock keeps a coin out of the payments ('CoinLock'), and each kind is
-- taken off by whoever put it on. A pending transaction locks the coins
-- it spends, until it is sent or deleted. A replacement, which bumpfee
-- prepares, locks the coins of the transactions it replaces too, since
-- they go once it is sent
-- ("Haskoin.Wallet.Database.PendingTx".'replacedOutpoints'): that lock
-- has a name of its own, for the answers to say what holds the coin. The
-- user locks one coin with lockcoin, until unlockcoin. A tag that locks
-- its coins ('Haskoin.Wallet.Database.Tag.setTagLocksCoins') locks
-- every coin at the addresses that have it, those that arrive later too,
-- until the tag is told to unlock them.
--
-- The lock of a tag has one more key: a payment that names the tag with
-- --spend-from-tag spends its coins ('spendableCoinsOpening'). That is
-- what tells it from lockcoin. Savings are kept out of the payments of
-- every day, and are still spent in one command when the user means to,
-- without unlocking first and locking again afterwards, which is easy to
-- forget. Naming the coin or its address does not open the lock: the
-- same rule holds for every chooser but the one that says the tag.
--
-- A coin shows one lock, the first it has, in that order: a pending
-- transaction that spends it, one that replaces its transaction, the
-- user, a tag.
-- So a coin of a locking tag that the user also locked with lockcoin is
-- locked by the user, and stays so for a payment that names the tag.
--
-- The lock of the user is on one coin, and a payment that is replaced
-- comes back as another coin. The sync then moves the lock to the new
-- coin ('lockHeirs'), so that a replacement does not free what the user
-- set aside.
--
-- coins lists the coins in the order of the chain, as txs lists the
-- transactions (the type t'Haskoin.Wallet.Database.TxInfo.Standing'): the
-- unconfirmed ones first, then the others by their block and their
-- position in it, the newest first. This is the order of the block
-- references as the store encodes them, which an index of the coins
-- gives. The times shown follow it nearly: a block may be dated before
-- the block under it.
module Haskoin.Wallet.Database.Coin
  ( JsonCoin (..),
    CoinLock (..),
    coinOutpoint,
    coinText,
    readBlock,
    toJsonCoin,
    groupCoins,
    CoinFilter (..),
    coinsAfter,
    coinsStarting,
    coinsWith,
    coinAddressesStarting,
    coinBlock,
    listedCoinsAfter,
    accountCoinsQuery,
    Spending (..),
    spendingOf,
    spendingOpening,
    spendableCoins,
    spendableCoinsOpening,
    unspendableCoins,
    unspendableCoinsOpening,
    HeldBack (..),
    heldBackOf,
    spendableAtAddresses,
    balanceColumnsBy,
    spendableBalance,
    coinBalancesAfter,
    coinBalancesAt,
    lockCoin,
    unlockCoin,
    walletCoin,
    shownCoin,
    coinRows,
    setLockedByUser,
    userLockedCoins,
    coinHolders,
    checkCoinOutput,
    checkListedCoins,
    checkCoinsPaid,
    coinParents,
    coinsSpentBy,
    Refreshed (..),
    lockHeirs,
    refreshCoins,
    LeftReason (..),
    LeftCoin (..),
    leftOutpoints,
    leftCoinsOf,
    poolCoinViews,
    jsonCoinPairs,
    leftReasons,
    leftReasonText,
    leftCoinWhy,
    leftOfOtherType,
    chosenCoins,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Aeson ((.:?), (.!=), FromJSON (..), ToJSON (..), Value (String), object, withObject, withText, (.:), (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Either (rights)
import Data.List (genericTake, intercalate, sortOn)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (Single (..), rawSql)
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word32, Word64)
import Database.Esqueleto.Legacy (InnerJoin (..), SqlExpr, SqlQuery, Value (..), from, groupBy, having, in_, just, limit, on, orderBy, select, sum_, val, valList, where_, (&&.), (==.), (^.), (||.))
import qualified Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.CashTokens (holdsTokens, lockingScript)
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.Config (Config, configDefaultNetwork, networksText, onHiddenNetworks, onShownNetworks)
import Haskoin.Wallet.Database.Account (accountNetwork, accountPool, accountsOnNetwork, getAccountById, getAccountsById, poolAccounts)
import Haskoin.Wallet.Database.Address (CoinBalance (..), addrPath)
import Haskoin.Wallet.Database.Portfolio (Place, poolPlace)
import Haskoin.Wallet.Database.Tag (TagFilter, addressTagsAt, coinAtKept, isTagFiltered, noTagFilter)
import Haskoin.Wallet.Tags (ShownTag, Tag, tagText)
import Haskoin.Wallet.Keys (PoolPath (..), addressKind)
import Haskoin.Wallet.Database.PendingTx (PendingTx, intercalateAnd, lockedOutpoints, pendingReplacer, pendingShortId, pendingSpender, replacedOutpoints, tagLockedOutpoints, tagLocks, userLockedOutpoints)
import Haskoin.Wallet.Database.RawTx (getRawTx, lookupRawTx)
import Haskoin.Wallet.ShortId (ShortId, shortIdKey)
import Haskoin.Wallet.Database.TxInfo (coinLabel, liveSummaries, spentTxids, walletTransaction)
import Haskoin.Wallet.Util (rowLimit)
import Numeric.Natural (Natural)

{- Coins as the user sees them -}

-- | A coin as hw shows it and writes it in JSON: the coin, and what a view
-- shows beside it, which is its lock, its labels, and the tags and the path
-- of its address
data JsonCoin = JsonCoin
  { jsonCoinOutpoint :: !OutPoint,
    jsonCoinAddress :: !Address,
    jsonCoinValue :: !Word64,
    jsonCoinBlock :: !Store.BlockRef,
    jsonCoinConfirmations :: !Natural,
    -- | What holds it, if anything
    jsonCoinLock :: !(Maybe CoinLock),
    -- | The builder may spend it now (@canSpend@)
    jsonCoinSpendable :: !Bool,
    -- | The label of the transaction that paid it, when the coin shows it
    -- ('coinLabel'), or none
    jsonCoinTxLabel :: !Text,
    -- | The label of the address that holds it, or none
    jsonCoinAddressLabel :: !Text,
    -- | The tags of the address that holds it. A coin has no tags of its
    -- own: it belongs to the groups of its address, which is what coin
    -- control by tag spends from.
    jsonCoinAddressTags :: ![ShownTag],
    -- | Whether the address that holds it is a change address
    jsonCoinChange :: !Bool,
    -- | Where the address that holds it is in the portfolio: the kind of
    -- its account and its path below the key of that account, which the
    -- compact views show in place of the address
    jsonCoinPath :: !(Maybe PoolPath),
    -- | The time of the transaction that paid it, which the sync stores
    -- with the coin ('refreshCoins'). Its views start with it, as the
    -- views of a transaction do. Every coin of the database has one. A
    -- coin read from JSON may have none.
    jsonCoinTime :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

-- | What holds a coin back from spending: the tokens it holds, a pending
-- transaction that spends it, a pending transaction that replaces the
-- transaction it comes from and so removes it once sent, the user, with
-- lockcoin, or a tag of its address that locks its coins. A coin shows
-- one lock at most, the first of these that it has.
--
-- The lock of tokens is the one lock that nothing opens: hw does not
-- handle the tokens of Bitcoin Cash, and a payment of hw that spent the
-- coin could destroy them ("Haskoin.Wallet.CashTokens"). It comes first,
-- since the other locks come and go and this one stays.
data CoinLock = LockedByTokens | LockedByTransaction | LockedByReplacement | LockedByUser | LockedByTag
  deriving (Eq, Show)

instance ToJSON CoinLock where
  toJSON LockedByTokens = String "tokens"
  toJSON LockedByTransaction = String "transaction"
  toJSON LockedByReplacement = String "replacement"
  toJSON LockedByUser = String "user"
  toJSON LockedByTag = String "tag"

instance FromJSON CoinLock where
  parseJSON = withText "CoinLock" $ \case
    "tokens" -> return LockedByTokens
    "transaction" -> return LockedByTransaction
    "replacement" -> return LockedByReplacement
    "user" -> return LockedByUser
    "tag" -> return LockedByTag
    other -> fail $ "Unknown coin lock: " <> cs other

instance MarshalJSON Network JsonCoin where
  marshalValue net c = object (jsonCoinPairs net c)
  unmarshalValue net =
    withObject "JsonCoin" $ \o ->
      JsonCoin
        <$> o .: "outpoint"
        <*> (unmarshalValue net =<< o .: "address")
        <*> o .: "value"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "lock"
        <*> o .: "spendable"
        <*> o .:? "txlabel" .!= ""
        <*> o .:? "addresslabel" .!= ""
        <*> o .:? "addresstags" .!= []
        <*> o .:? "change" .!= False
        <*> (liftA2 PoolPath <$> o .:? "type" <*> o .:? "path")
        <*> o .:? "time"

-- | The keys of a coin in JSON, its address in the text of the network.
-- A coin a payment leaves has them too, beside why it stays ('leftCoinsOf').
jsonCoinPairs :: Network -> JsonCoin -> [Pair]
jsonCoinPairs net c =
  [ "outpoint" .= jsonCoinOutpoint c,
    "address" .= marshalValue net (jsonCoinAddress c),
    "value" .= jsonCoinValue c,
    "block" .= jsonCoinBlock c,
    "confirmations" .= jsonCoinConfirmations c,
    "lock" .= jsonCoinLock c,
    "spendable" .= jsonCoinSpendable c,
    "txlabel" .= jsonCoinTxLabel c,
    "addresslabel" .= jsonCoinAddressLabel c,
    "addresstags" .= jsonCoinAddressTags c,
    "change" .= jsonCoinChange c,
    "path" .= (pathInAccount <$> jsonCoinPath c),
    "type" .= (pathKind <$> jsonCoinPath c),
    "time" .= jsonCoinTime c
  ]

-- | A coin row as the user sees it: its confirmations counted from the best
-- block, its lock, spendable when the builder may spend it now, the label
-- it shows, the time of its transaction, and the label, the kind and the
-- path of its address
toJsonCoin :: (MonadUnliftIO m) => Spending -> Pool -> DBCoin -> ExceptT String (DB m) JsonCoin
toJsonCoin rules pool row = do
  outpoint <- liftEither $ coinOutpoint row
  addr <- liftEither $ coinAddress rules.spendingNetwork outpoint row
  block <- liftEither $ readBlock outpoint (dBCoinBlockRef row)
  spendable <- canSpend rules 0 outpoint block
  label <- lift $ coinLabel pool outpoint.hash
  addrRowM <- lift $ fmap P.entityVal <$> P.getBy (UniqueAddress (dBCoinAccount row) (dBCoinAddress row))
  tags <- lift $ addressTagsAt (dBCoinAccount row) (dBCoinAddress row)
  return
    JsonCoin
      { jsonCoinOutpoint = outpoint,
        jsonCoinAddress = addr,
        jsonCoinValue = dBCoinValue row,
        jsonCoinBlock = block,
        jsonCoinConfirmations = getConfirmations rules.spendingBest block,
        jsonCoinLock = Map.lookup outpoint rules.spendingLocks,
        jsonCoinSpendable = spendable,
        jsonCoinTxLabel = label,
        jsonCoinAddressLabel = maybe "" dBAddressLabel addrRowM,
        jsonCoinAddressTags = tags,
        jsonCoinChange = maybe False dBAddressInternal addrRowM,
        jsonCoinPath = PoolPath <$> addressKind addr <*> (addrPath <$> addrRowM),
        jsonCoinTime = Just (dBCoinTime row)
      }

{- Coin rows -}

-- | The outpoint of a coin row
coinOutpoint :: DBCoin -> Either String OutPoint
coinOutpoint = readOutpoint . dBCoinOutpoint

-- | An outpoint as the coin rows key it
readOutpoint :: KeyBytes -> Either String OutPoint
readOutpoint key = readStored "A stored coin has an outpoint that does not read" (bytesToOutpoint key)

-- | The address of a coin row, given its outpoint for the message
coinAddress :: Network -> OutPoint -> DBCoin -> Either String Address
coinAddress net outpoint row =
  maybeToEither (storedProblem outpoint $ "has an address that does not read: " <> cs (dBCoinAddress row)) $
    textToAddr net (dBCoinAddress row)

-- | The block of a coin as its row stores it, given its outpoint for the
-- message
readBlock :: OutPoint -> ByteString -> Either String Store.BlockRef
readBlock outpoint bytes =
  readStored (storedProblem outpoint "has a block that does not read") (S.decode bytes)

-- | The coin of a row as the store reported it, its script given by its
-- address: the refresh stores only coins whose locking script is the
-- standard script of their address ('groupCoins'). A coin that holds
-- tokens has that script here too, without its token prefix, which the
-- row does not keep: nothing spends such a coin ('LockedByTokens'), and
-- signing reads the script of a coin from its transaction, not from here.
storedUnspent :: Network -> DBCoin -> Either String Store.Unspent
storedUnspent net row = do
  outpoint <- coinOutpoint row
  addr <- coinAddress net outpoint row
  block <- readBlock outpoint (dBCoinBlockRef row)
  script <- maybeToEither (storedProblem outpoint "has an address without a standard script") $ standardScript addr
  return
    Store.Unspent
      { Store.block = block,
        Store.outpoint = outpoint,
        Store.value = dBCoinValue row,
        Store.script = script,
        Store.address = Just addr
      }

-- | The standard output script of an address: the one script a coin the
-- wallet can spend has at it
standardScript :: Address -> Maybe ByteString
standardScript addr = encodeSerial <$> addressToOutput addr

-- | What does not read in a stored coin, named by its outpoint
storedProblem :: OutPoint -> String -> String
storedProblem outpoint what = "The stored coin " <> coinText outpoint <> " " <> what

-- | A coin as hw writes it, and as --coin, lockcoin and unlockcoin take it:
-- the id of its transaction, a colon, and its index among the outputs
coinText :: OutPoint -> String
coinText outpoint = cs (txHashToHex outpoint.hash) <> ":" <> show outpoint.index

-- | A value read back from a row, or the given message when it does not read
readStored :: String -> Either String a -> Either String a
readStored message = either (const $ Left message) Right

-- | The coin rows of a pool: those of each of its accounts
poolCoins :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) [DBCoin]
poolCoins pool = lift $ do
  accounts <- map fst <$> poolAccounts pool
  map P.entityVal <$> P.selectList [DBCoinAccount P.<-. accounts] []

-- | The key of the row of a coin of an account
coinKey :: DBAccountId -> OutPoint -> P.Key DBCoin
coinKey accId outpoint = DBCoinKey accId (outpointBytes outpoint)

-- | Every row of a coin, by its outpoint: one for each account that holds
-- it. A coin is at one address, and an address in one account on a
-- network, so the rows are of accounts on different networks: the test
-- networks share their keys, and the same coin may be reported on two of
-- them.
coinRows :: (MonadUnliftIO m) => OutPoint -> DB m [DBCoin]
coinRows outpoint = map P.entityVal <$> P.selectList [DBCoinOutpoint P.==. outpointBytes outpoint] []

-- | The coins of the store by the text of their address, each coin once,
-- by its outpoint as the rows key it. A coin without an address, or at one
-- the network cannot write, is left out, and so is one whose script is not
-- the standard script of its address: the same address in other bytes,
-- which only a miner can put in a block, and which hw could not sign for.
-- The rows keep the address alone, which gives the script of every other.
--
-- The script compared is the one that locks the coin. On Bitcoin Cash a
-- coin that holds tokens has a token prefix in front of it
-- ('lockingScript'). Such a coin is kept, and stored as a coin that holds
-- tokens (@insertCoin@), which nothing spends.
groupCoins :: Network -> [Store.Unspent] -> Map Text (Map KeyBytes Store.Unspent)
groupCoins net coins = Map.fromListWith Map.union (mapMaybe entry coins)
  where
    entry coin = do
      addr <- coin.address
      locking <- lockingScript net coin.script
      guard (standardScript addr == Just locking)
      addrText <- addrToText net addr
      return (addrText, Map.singleton (outpointBytes coin.outpoint) coin)

{- Listing -}

-- | Which coins of a pool coins lists: all of them, or the locked ones
data CoinFilter = AllCoins | LockedCoins
  deriving (Eq, Show)

-- | The query of coins for an account: its coins that the filter keeps,
-- in the order of their block references as the store encodes them. The
-- unconfirmed ones come first, then the others by their block and their
-- position in it, the newest first in each. Coins with the same block
-- reference, the outputs of one transaction for instance, follow the
-- order of their outpoints, so that the order is settled. Those after the
-- coin given, by its block and its outpoint, up to the count. The index
-- on the block references gives them in that order, from the block of
-- the coin given on.
--
-- The reverse order is the mirror of this one: the oldest block first,
-- the unconfirmed coins last, and the coins of one block reference from
-- the last outpoint to the first. The same index gives it, read from its
-- other end.
accountCoinsQuery :: ListingOrder -> DBAccountId -> (SqlExpr (P.Entity DBCoin) -> SqlExpr (E.Value Bool)) -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> SqlQuery (SqlExpr (P.Entity DBCoin))
accountCoinsQuery order accId kept afterM countM =
  from $ \c -> do
    where_ $ c ^. DBCoinAccount ==. val accId &&. kept c
    forM_ afterM $ \(block, outpoint) -> do
      let blockBytes = val (S.encode block)
      where_ $
        notBeforeIn order (c ^. DBCoinBlockRef) blockBytes
          &&. (afterIn order (c ^. DBCoinBlockRef) blockBytes ||. afterIn order (c ^. DBCoinOutpoint) (val (outpointBytes outpoint)))
    orderBy [ascendingIn order (c ^. DBCoinBlockRef), ascendingIn order (c ^. DBCoinOutpoint)]
    forM_ countM $ limit . rowLimit
    return c

-- | The coins of the given accounts of a pool, spendable or not, in the
-- order of coins ('accountCoinsQuery'): the unconfirmed ones first, then
-- the others by their block, the newest first. Those after the coin
-- given, by its block and its outpoint, or the first ones, up to the
-- count, or all of them.
--
-- The accounts are those of the pool, or the one of a type alone. Each
-- account is read in that order from its own index, up to the count, and
-- the rows are merged: the first rows of the whole are among the first
-- rows of each. One query over two accounts would sort every coin of both
-- for each page.
coinsAfter :: (MonadUnliftIO m) => Pool -> [DBAccountId] -> CoinFilter -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> ExceptT String (DB m) [JsonCoin]
coinsAfter pool accIds shown afterM countM = map fst <$> listedCoinsAfter UsualOrder pool accIds shown noTagFilter afterM countM

-- | 'coinsAfter' in the order given, the usual one or its reverse, with
-- the oldest coins first and the unconfirmed ones last. Each coin comes
-- with its block and its outpoint: what the listing goes on after.
--
-- With a filter of tags, only the coins at an address the filter keeps
-- are given: one that has any of the tags, or one without a tag when
-- those are asked for ('coinAtKept'). The coins are still read in their
-- order, from the index, and each is asked about by the index of the
-- tags, so the query stops as soon as it has the count.
listedCoinsAfter :: (MonadUnliftIO m) => ListingOrder -> Pool -> [DBAccountId] -> CoinFilter -> TagFilter -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> ExceptT String (DB m) [(JsonCoin, (Store.BlockRef, OutPoint))]
listedCoinsAfter order pool accIds shown tagged afterM countM = do
  rules <- spendingOf pool.poolNetwork
  let shownCoins c = case shown of
        AllCoins -> val True
        LockedCoins -> c ^. DBCoinOutpoint `in_` valList (map outpointBytes (Map.keys rules.spendingLocks))
      kept c
        | isTagFiltered tagged = shownCoins c &&. coinAtKept tagged c
        | otherwise = shownCoins c
  perAccount <- forM accIds $ \accId ->
    lift $ map P.entityVal <$> select (accountCoinsQuery order accId kept afterM countM)
  let place row = (dBCoinBlockRef row, dBCoinOutpoint row)
      merged = case order of
        UsualOrder -> sortOn place (concat perAccount)
        ReverseOrder -> sortOn (Down . place) (concat perAccount)
  forM (maybe id genericTake countM merged) $ \row -> do
    coin <- toJsonCoin rules pool row
    return (coin, (jsonCoinBlock coin, jsonCoinOutpoint coin))

-- | At most the given number of coin rows of the account whose transaction
-- id, as hw shows it, starts with the hexadecimal digits, by their
-- outpoints: for the shell. The key holds the id in the reverse order of
-- its digits, so the whole bytes typed are matched on the tail of the id
-- in the key, and a last digit alone on the byte before.
coinsStarting :: (MonadUnliftIO m) => DBAccountId -> String -> Natural -> DB m [DBCoin]
coinsStarting accId digits atMost = do
  rows <-
    rawSql
      (cs ("select ?? from d_b_coin where account = ?" <> conditions <> " order by outpoint limit ?"))
      (P.toPersistValue accId : parameters <> [P.toPersistValue (rowLimit atMost :: Int)])
  return (P.entityVal <$> rows)
  where
    typed = map toUpper digits
    bytes = length typed `div` 2
    -- The digits of the whole bytes, those of each byte together, in the
    -- order of the key, as hex() writes them
    wholeBytes = concat (reverse (pairs (take (2 * bytes) typed)))
    lastDigit = drop (2 * bytes) typed
    conditions =
      concat [" and hex(substr(outpoint, ?, ?)) = ?" | bytes > 0]
        <> concat [" and substr(hex(substr(outpoint, ?, 1)), 1, 1) = ?" | not (null lastDigit)]
    parameters =
      concat [[P.toPersistValue (33 - bytes), P.toPersistValue bytes, P.toPersistValue wholeBytes] | bytes > 0]
        <> concat [[P.toPersistValue (32 - bytes), P.toPersistValue lastDigit] | not (null lastDigit)]
    pairs (a : b : more) = [a, b] : pairs more
    pairs _ = []

-- | The coins of the wallet, of every account, each once, whose
-- transaction has the short id and that are at the index among its
-- outputs. The key holds the
-- txid in the reverse order of the digits hw shows ('outpointBytes'), then
-- the index in 4 bytes, the lowest first. So the key ends with the short
-- id reversed, then the index. No index of the keys reads the end of a
-- key: every coin is read, as 'coinsStarting' reads them for the shell.
coinsWith :: (MonadUnliftIO m) => ShortId -> Word32 -> DB m [OutPoint]
coinsWith short index = do
  rows <-
    rawSql
      "select outpoint from d_b_coin where substr(outpoint, 29, 8) = ?"
      [P.toPersistValue keyEnd]
  return $ nubOrd (rights (bytesToOutpoint . unSingle <$> rows))
  where
    keyEnd = BS.reverse (shortIdKey short) <> S.runPut (S.putWord32le index)

-- | The text of at most the given number of addresses of the account that
-- hold a coin and start with the text, as
-- 'Haskoin.Wallet.Database.Address.addressesStarting' matches it: for the
-- shell
coinAddressesStarting :: (MonadUnliftIO m) => DBAccountId -> Text -> Natural -> DB m [Text]
coinAddressesStarting accId start atMost = do
  rows <-
    rawSql
      "select distinct address from d_b_coin where account = ? and substr(address, instr(address, ':') + 1, ?) = ? order by address limit ?"
      [P.toPersistValue accId, P.toPersistValue (Text.length start), P.toPersistValue start, P.toPersistValue (rowLimit atMost :: Int)]
  return (unSingle <$> rows)

-- | The block of a coin of the accounts, the one --after names, or why
-- there is none
coinBlock :: (MonadUnliftIO m) => [DBAccountId] -> OutPoint -> ExceptT String (DB m) Store.BlockRef
coinBlock accIds outpoint = do
  rows <- lift $ catMaybes <$> mapM (\accId -> P.get (coinKey accId outpoint)) accIds
  case rows of
    row : _ -> liftEither $ readBlock outpoint (dBCoinBlockRef row)
    [] -> throwError $ "The coin " <> coinText outpoint <> " is not among the coins listed"

{- Spending -}

-- | What deciding whether the coins of a network can be spent reads once:
-- the network, the height of its best block, and the locks of its coins.
-- The rules are the same for every portfolio on the network.
data Spending = Spending
  { spendingNetwork :: !Network,
    spendingBest :: !(Maybe BlockHeight),
    spendingLocks :: !(Map OutPoint CoinLock)
  }

-- | The spending rules of a network as the database has them now, for
-- the listings and for a payment that names no tag: every tag that locks
-- its coins locks them
spendingOf :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) Spending
spendingOf = spendingOpening []

-- | The spending rules of a network for a payment that names the given
-- tags with --spend-from-tag: the locks of those tags are open for it
-- ('tagLockedOutpoints')
spendingOpening :: (MonadUnliftIO m) => [Tag] -> Network -> ExceptT String (DB m) Spending
spendingOpening opened net = do
  heightM <- bestHeight net
  locks <- coinLocks opened net
  return Spending {spendingNetwork = net, spendingBest = heightM, spendingLocks = locks}

-- | The locks of the coins of a network: the coins that hold tokens,
-- those the pending transactions spend, those of the transactions they
-- replace, those the user locked, and those at the addresses of the tags
-- that lock their coins, but for the tags given. A coin with several
-- locks has the first of them in that order.
coinLocks :: (MonadUnliftIO m) => [Tag] -> Network -> ExceptT String (DB m) (Map OutPoint CoinLock)
coinLocks opened net = do
  byTokens <- tokenOutpoints net
  byTransactions <- lockedOutpoints net
  byReplacements <- replacedOutpoints net
  byUser <- userLockedOutpoints net
  byTags <- tagLockedOutpoints opened net
  return $
    Map.fromSet (const LockedByTokens) byTokens
      <> Map.fromSet (const LockedByTransaction) byTransactions
      <> Map.fromSet (const LockedByReplacement) byReplacements
      <> Map.fromSet (const LockedByUser) byUser
      <> Map.fromSet (const LockedByTag) byTags

-- | The coins of the wallet on a network that hold tokens, as the sync
-- stored them (@insertCoin@). The rows are read from the index of the
-- coins by account, and only on a network of Bitcoin Cash: Bitcoin has no
-- tokens, so nothing is read there.
tokenOutpoints :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Set OutPoint)
tokenOutpoints net
  | isNothing net.cashAddrPrefix = return Set.empty
  | otherwise = do
      accounts <- lift $ accountsOnNetwork net
      rows <- lift $ P.selectList [DBCoinAccount P.<-. accounts, DBCoinTokens P.==. True] []
      Set.fromList <$> mapM (liftEither . coinOutpoint . P.entityVal) rows

-- | Whether the builder may spend a coin with at least the given
-- confirmations: nothing locks it, neither the tokens it holds, nor a pending transaction, which
-- spends it or replaces its transaction, nor the user nor a tag, a coin from a coinbase has matured, and an unconfirmed coin comes
-- from a transaction the wallet trusts ('trustedTx'): one it made itself,
-- whether the coin is its change or what one of its portfolios paid another.
-- At zero confirmations, this says whether the coin can be spent now: the
-- spendable flag and balances.
canSpend :: (MonadUnliftIO m) => Spending -> Natural -> OutPoint -> Store.BlockRef -> ExceptT String (DB m) Bool
canSpend rules minConf outpoint block
  | confirmations < minConf || outpoint `Map.member` rules.spendingLocks = return False
  | confirmations == 0 = trustedTx rules.spendingNetwork outpoint.hash
  | confirmations < coinbaseMaturity = not <$> isCoinbaseTx outpoint.hash
  | otherwise = return True
  where
    confirmations = getConfirmations rules.spendingBest block

-- | The coins of a pool the builder may spend, with at least the
-- confirmations asked, as the store last reported them: those of every
-- account of the portfolio on the network, so that a payment can spend
-- BIP44 and BIP84 coins together
spendableCoins :: (MonadUnliftIO m) => Pool -> Natural -> ExceptT String (DB m) [Store.Unspent]
spendableCoins = spendableCoinsOpening []

-- | 'spendableCoins' for a payment that names the given tags with
-- --spend-from-tag: the coins those tags lock are among them
spendableCoinsOpening :: (MonadUnliftIO m) => [Tag] -> Pool -> Natural -> ExceptT String (DB m) [Store.Unspent]
spendableCoinsOpening opened pool minConf = do
  let net = pool.poolNetwork
  rules <- spendingOpening opened net
  coins <- liftEither . mapM (storedUnspent net) =<< poolCoins pool
  filterM (\coin -> canSpend rules minConf coin.outpoint coin.block) coins

-- | The coins the builder may not spend with at least the confirmations
-- asked, the others of 'spendableCoins', each with its lock. A coin without
-- a lock is not confirmed enough yet, or is a coinbase not yet mature.
unspendableCoins :: (MonadUnliftIO m) => Pool -> Natural -> ExceptT String (DB m) [(Store.Unspent, Maybe CoinLock)]
unspendableCoins = unspendableCoinsOpening []

-- | 'unspendableCoins' for a payment that names the given tags with
-- --spend-from-tag: the others of 'spendableCoinsOpening'
unspendableCoinsOpening :: (MonadUnliftIO m) => [Tag] -> Pool -> Natural -> ExceptT String (DB m) [(Store.Unspent, Maybe CoinLock)]
unspendableCoinsOpening opened pool minConf = do
  let net = pool.poolNetwork
  rules <- spendingOpening opened net
  coins <- liftEither . mapM (storedUnspent net) =<< poolCoins pool
  held <- filterM (\coin -> not <$> canSpend rules minConf coin.outpoint coin.block) coins
  return [(coin, Map.lookup coin.outpoint rules.spendingLocks) | coin <- held]

-- | What a portfolio holds on a network and cannot spend now: how many
-- coins, and what they are worth. A command that could not find the
-- coins it needed says it, and points to coins, which says why each coin
-- waits: the balance would otherwise say one thing and the command
-- another.
data HeldBack = HeldBack
  { heldBackCoins :: !Natural,
    heldBackValue :: !Word64
  }
  deriving (Eq, Show)

-- | The coins given, which the portfolio cannot spend now, counted and
-- added up: Nothing when there is none
heldBackOf :: [Store.Unspent] -> Maybe HeldBack
heldBackOf [] = Nothing
heldBackOf coins = Just (HeldBack (fromIntegral (length coins)) (sum ((.value) <$> coins)))

-- | The value of the coins of the account that can be spent now, by the
-- text of their address, for the given addresses only: the index of the
-- coins by account and address makes a page of addresses as quick to show
-- in a large account as in a small one. An address without such a coin is
-- left out.
spendableAtAddresses :: (MonadUnliftIO m) => Network -> DBAccountId -> [Text] -> ExceptT String (DB m) (Map Text Word64)
spendableAtAddresses net accId addrs = do
  rules <- spendingOf net
  coins <- balanceColumns [accId] (Just addrs)
  Map.fromListWith (+) <$> spendableValues rules coins

-- | The value of all the coins of a pool that can be spent now: those of
-- every account of the portfolio on the network
spendableBalance :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) Word64
spendableBalance pool = do
  rules <- spendingOf pool.poolNetwork
  accounts <- lift $ map fst <$> poolAccounts pool
  coins <- balanceColumns accounts Nothing
  sum . map snd <$> spendableValues rules coins

-- | The addresses of the given accounts of a pool that hold coins, each
-- with the balance of its coins, the largest total first. Addresses with
-- the same total follow the order of their accounts, then of their
-- derivation paths, so that the order is settled. In the reverse order,
-- its mirror, the smallest total comes first, and addresses with the same
-- total follow their accounts and their paths from the last to the
-- first. Those after the address given in the order, with its balance,
-- or the first ones, up to the count, or all of them. The order adds up
-- every coin of the accounts, in one query; the spendable values are read
-- for the addresses listed only.
--
-- With a filter of tags, only the addresses the filter keeps are given,
-- those that have any of the tags and those without a tag when asked
-- for: the coins at the other addresses are left out before they are
-- added up.
coinBalancesAfter :: (MonadUnliftIO m) => ListingOrder -> Network -> [DBAccountId] -> TagFilter -> Maybe (DBAddress, CoinBalance) -> Maybe Natural -> ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalancesAfter order net accIds tagged = coinBalances order net accIds tagged Nothing

-- | Those of the given addresses of the accounts that hold coins, each
-- with the balance of its coins, in the usual order of
-- 'coinBalancesAfter'
coinBalancesAt :: (MonadUnliftIO m) => Network -> [DBAccountId] -> [Text] -> ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalancesAt net accIds addrs = coinBalances UsualOrder net accIds noTagFilter (Just addrs) Nothing Nothing

-- | The addresses of the accounts that hold coins, each with the balance
-- of its coins, the largest total first, or in the reverse of that order:
-- at the given addresses or at all of them, after the given one or from
-- the first, up to the count or all of them. With a filter of tags, only
-- the addresses it keeps.
coinBalances ::
  (MonadUnliftIO m) =>
  ListingOrder ->
  Network ->
  [DBAccountId] ->
  TagFilter ->
  Maybe [Text] ->
  Maybe (DBAddress, CoinBalance) ->
  Maybe Natural ->
  ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalances order net accIds tagged addrsM afterM countM = do
  rows <- lift . select . from $ \(c `InnerJoin` a) -> do
    on $ c ^. DBCoinAccount ==. a ^. DBAddressAccount &&. c ^. DBCoinAddress ==. a ^. DBAddressAddress
    where_ $ c ^. DBCoinAccount `in_` valList accIds
    when (isTagFiltered tagged) $ where_ (coinAtKept tagged c)
    forM_ addrsM $ \addrs -> where_ $ c ^. DBCoinAddress `in_` valList addrs
    groupBy (a ^. DBAddressAccount, a ^. DBAddressInternal, a ^. DBAddressIndex)
    let total = sum_ (c ^. DBCoinValue)
        -- The total goes down in the usual order, and the account and
        -- the path go up
        largestLast = turned order
    -- After the address given, in the usual order: a smaller total, or
    -- the same total and an account, then a path, after its own
    forM_ afterM $ \(addr, balance) -> do
      let given = just (val (coinBalanceTotal balance))
          sameAccount = a ^. DBAddressAccount ==. val (dBAddressAccount addr)
          pathAfter =
            afterIn order (a ^. DBAddressInternal) (val (dBAddressInternal addr))
              ||. (a ^. DBAddressInternal ==. val (dBAddressInternal addr) &&. afterIn order (a ^. DBAddressIndex) (val (dBAddressIndex addr)))
          placeAfter = afterIn order (a ^. DBAddressAccount) (val (dBAddressAccount addr)) ||. (sameAccount &&. pathAfter)
      having $ afterIn largestLast total given ||. (total ==. given &&. placeAfter)
    orderBy [ascendingIn largestLast total, ascendingIn order (a ^. DBAddressAccount), ascendingIn order (a ^. DBAddressInternal), ascendingIn order (a ^. DBAddressIndex)]
    forM_ countM $ limit . rowLimit
    return (a, total)
  -- An address of the page holds a coin at least, so its total is never null
  let held = [(P.entityVal a, fromMaybe 0 total) | (a, Value total) <- rows]
  rules <- spendingOf net
  coins <- balanceColumns accIds (Just (dBAddressAddress . fst <$> held))
  byAddress <- Map.fromListWith (<>) <$> mapM (coinShare rules) coins
  return [(addr, Map.findWithDefault (CoinBalance total 0 0 0 0 0) (dBAddressAddress addr) byAddress) | (addr, total) <- held]

-- | What a coin adds to the balance of its address: its value to the
-- total, and to the part its state puts it in. A locked coin cannot be
-- spent (@canSpend@), so no coin counts in two parts. A pending
-- transaction holds a coin it spends and a coin of a transaction it
-- replaces alike: both count in the same part.
coinShare :: (MonadUnliftIO m) => Spending -> (OutPoint, Text, Word64, Store.BlockRef) -> ExceptT String (DB m) (Text, CoinBalance)
coinShare rules (outpoint, addr, value, block) = do
  spendable <- canSpend rules 0 outpoint block
  let lock = Map.lookup outpoint rules.spendingLocks
      partIf held = if held then value else 0
  return
    ( addr,
      CoinBalance
        value
        (partIf spendable)
        (partIf (lock `elem` [Just LockedByTransaction, Just LockedByReplacement]))
        (partIf (lock == Just LockedByUser))
        (partIf (lock == Just LockedByTag))
        (partIf (lock == Just LockedByTokens))
    )

-- | What the spendable balances read of the coins of accounts of one
-- network, at the given addresses or at all of them: the outpoint,
-- address, value and block of each.
--
-- The addresses are asked about 'inListBatch' at a time. A listing read
-- whole, as --json, --csv and exportsheet read it, asks about every
-- address of an account, and the total of balances about every address
-- that holds coins. SQLite takes no more than 32766 values in one query,
-- and refused the whole listing of a portfolio with more addresses.
balanceColumns :: (MonadUnliftIO m) => [DBAccountId] -> Maybe [Text] -> ExceptT String (DB m) [(OutPoint, Text, Word64, Store.BlockRef)]
balanceColumns = balanceColumnsBy inListBatch

-- | @balanceColumns@, asking about the given number of addresses in each
-- query
balanceColumnsBy :: (MonadUnliftIO m) => Natural -> [DBAccountId] -> Maybe [Text] -> ExceptT String (DB m) [(OutPoint, Text, Word64, Store.BlockRef)]
balanceColumnsBy batch accIds addrsM = do
  rows <- lift $ case addrsM of
    Nothing -> select (coinsAt Nothing)
    Just addrs -> concat <$> mapM (select . coinsAt . Just) (chunksOf (fromIntegral (max 1 batch)) addrs)
  liftEither $ forM rows $ \(Value key, Value addr, Value value, Value blockBytes) -> do
    outpoint <- readOutpoint key
    block <- readBlock outpoint blockBytes
    return (outpoint, addr, value, block)
  where
    -- The columns of the coins of the accounts, at some addresses or at
    -- every one
    coinsAt someM = from $ \c -> do
      where_ $ c ^. DBCoinAccount `in_` valList accIds
      forM_ someM $ \some -> where_ $ c ^. DBCoinAddress `in_` valList some
      return (c ^. DBCoinOutpoint, c ^. DBCoinAddress, c ^. DBCoinValue, c ^. DBCoinBlockRef)

-- | The address and value of those coins that can be spent now
spendableValues :: (MonadUnliftIO m) => Spending -> [(OutPoint, Text, Word64, Store.BlockRef)] -> ExceptT String (DB m) [(Text, Word64)]
spendableValues rules coins = do
  spendable <- filterM (\(outpoint, _, _, block) -> canSpend rules 0 outpoint block) coins
  return [(addr, value) | (_, addr, value, _) <- spendable]

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

-- | Whether the coins of a transaction can be spent before it confirms: it
-- is confirmed, or the wallet made it ('walletTransaction') from coins that
-- can be spent by this same rule. Whoever holds the keys of the inputs can
-- replace the transaction until it confirms, which also undoes every
-- transaction built on it. When the inputs are coins of the wallet's own
-- portfolios, that is the user: the change of a payment and what one
-- portfolio pays another are as safe as each other. A transaction the
-- sync has not stored for every portfolio that spends in it, or has found
-- dead, is not trusted.
--
-- The walk keeps what it found of each transaction, so that a parent two
-- transactions share is looked at once. A transaction met again while its
-- own parents are being looked at spends itself, through others or
-- directly: no chain holds such transactions, so a store that reports them
-- lies, and they are not trusted. The walk then ends, where it would
-- otherwise go round for ever.
trustedTx :: (MonadUnliftIO m) => Network -> TxHash -> ExceptT String (DB m) Bool
trustedTx net tid = evalStateT (trusted tid) Map.empty
  where
    -- What the walk found of each transaction: Nothing while its parents
    -- are being looked at, then whether it is trusted
    trusted :: (MonadUnliftIO m) => TxHash -> StateT (Map TxHash (Maybe Bool)) (ExceptT String (DB m)) Bool
    trusted t = do
      found <- gets (Map.lookup t)
      case found of
        Just (Just verdict) -> return verdict
        -- Met again on its own path: a cycle
        Just Nothing -> return False
        Nothing -> do
          modify (Map.insert t Nothing)
          verdict <- judge t
          modify (Map.insert t (Just verdict))
          return verdict
    judge t = do
      stored <- lift $ liveSummaries net t
      if any (Store.confirmed . fst) stored
        then return True
        else trustedUnconfirmed (snd <$> stored)
    -- The wallet made it, from transactions trusted by this same rule
    trustedUnconfirmed summaries
      | walletTransaction summaries = allM trusted (nubOrd (concatMap spentTxids summaries))
      | otherwise = return False
    -- Whether all are trusted, looking no further than the first that is not
    allM _ [] = return True
    allM f (x : xs) = do
      ok <- f x
      if ok then allM f xs else return False

{- Locks of the user -}

-- | Lock a coin of the wallet at the user's request: the builder no longer
-- offers it, and no pending transaction may spend it, until the user
-- unlocks it. Only a coin the user can lock is locked: one a pending
-- transaction holds is refused (@spentByPending@, @replacedByPending@),
-- and so is one the user locked already. A
-- coin a tag locks takes the lock of the user too, which holds it for a
-- payment that names the tag. The lock stays while the coin confirms,
-- moves to the coin that replaces it when its payment is replaced
-- ('lockHeirs'), and goes with the coin once it is spent.
-- Returns where the coin is and the coin as coins shows it.
lockCoin :: (MonadUnliftIO m) => (Network, DBCoin) -> ExceptT String (DB m) (Place, JsonCoin)
lockCoin (net, row) = do
  outpoint <- liftEither $ coinOutpoint row
  locks <- coinLocks [] net
  case Map.lookup outpoint locks of
    Just LockedByTransaction -> throwError . spentByPending =<< pendingSpender net outpoint
    Just LockedByReplacement -> throwError . replacedByPending =<< pendingReplacer net outpoint
    Just LockedByUser -> throwError "The coin is already locked"
    Just LockedByTokens -> throwError "The coin holds tokens (CashTokens), and hw never spends such a coin: it needs no lock"
    _ -> setLockedByUser (dBCoinAccount row) outpoint True
  shownCoin (net, row)

-- | Why lockcoin refuses a coin a pending transaction spends: the
-- transaction, by the short id pendingtxs shows, and the command that
-- frees the coin. The lock would say that nothing spends the coin while
-- a payment that does waits to be signed or sent, so the user settles
-- that payment first.
spentByPending :: Maybe PendingTx -> String
spentByPending = \case
  Just held ->
    let short = pendingShortId held
     in "The pending transaction " <> short <> " spends the coin already: deletetx " <> short <> " frees it"
  Nothing -> "A pending transaction spends the coin already: deletetx frees it"

-- | Why lockcoin refuses a coin of a transaction that a pending
-- transaction replaces: the replacement, by the short id pendingtxs
-- shows, and the command that frees the coin. The coin goes once the
-- replacement is sent, so the user settles the replacement first. A lock
-- put before the replacement was prepared stays, and moves to the change
-- of the replacement when that is at the same address ('lockHeirs').
replacedByPending :: Maybe PendingTx -> String
replacedByPending = \case
  Just held ->
    let short = pendingShortId held
     in "The pending transaction " <> short <> " replaces the payment the coin comes from: deletetx " <> short <> " frees it"
  Nothing -> "A pending transaction replaces the payment the coin comes from: deletetx frees it"

-- | Unlock a coin the user locked. A coin a pending transaction holds is
-- freed by deleting that transaction instead, and a coin a tag locks by
-- telling the tag to unlock its coins.
--
-- The lock of the user is read from the coin itself, not from the one
-- lock the coin shows: a replacement prepared after lockcoin holds the
-- coin too, and shows first. The lock of the user still comes off, and
-- the coin stays held by the replacement.
unlockCoin :: (MonadUnliftIO m) => (Network, DBCoin) -> ExceptT String (DB m) (Place, JsonCoin)
unlockCoin (net, row) = do
  outpoint <- liftEither $ coinOutpoint row
  locks <- coinLocks [] net
  if dBCoinLockedByUser row
    then setLockedByUser (dBCoinAccount row) outpoint False
    else throwError $ case Map.lookup outpoint locks of
      Just LockedByTransaction -> byPendingTransaction
      Just LockedByReplacement -> byPendingTransaction
      Just LockedByTag -> "The coin is locked by a tag of its address, not by lockcoin. hw tags shows the tags that lock their coins: hw tag TAG --unlock-coins unlocks them, and --spend-from-tag TAG spends them"
      Just LockedByTokens -> "The coin holds tokens (CashTokens), and nothing unlocks it: hw does not handle tokens and never spends such a coin. Move it with a wallet that handles the tokens of Bitcoin Cash"
      _ -> "The coin is not locked"
  shownCoin (net, row)
  where
    byPendingTransaction = "The coin is locked by a pending transaction, not by the user: deletetx frees it"

-- | The accounts that hold the coins, each once, on any network. A coin
-- the wallet does not hold has none.
coinHolders :: (MonadUnliftIO m) => [OutPoint] -> ExceptT String (DB m) [(DBAccountId, DBAccount)]
coinHolders outpoints = do
  rows <- lift $ concat <$> mapM coinRows outpoints
  getAccountsById $ nubOrd $ dBCoinAccount <$> rows

-- | The one row of a coin the user named, by its outpoint, with the
-- network of its account. The coin is looked for on the networks hw
-- shows. A coin with rows on several of them names the one on the
-- network given, the --network of the command, or else the one on the
-- default network of the configuration. When neither settles it, the
-- user is asked for --network, with the networks named. A coin the
-- wallet has on hidden networks alone is refused, with the way to show
-- its network ('onHiddenNetworks').
walletCoin :: (MonadUnliftIO m) => Config -> Maybe Network -> OutPoint -> ExceptT String (DB m) (Network, DBCoin)
walletCoin cfg netM outpoint = do
  rows <- lift $ coinRows outpoint
  everywhere <- forM rows $ \row -> (,row) . accountNetwork <$> getAccountById (dBCoinAccount row)
  let found = onShownNetworks cfg fst everywhere
      onNetwork net = [held | held@(rowNet, _) <- found, rowNet == net]
      networks = nubOrdOn (.name) (fst <$> found)
  case (netM, found) of
    (_, [])
      | null everywhere -> throwError ("The wallet has no coin " <> coinText outpoint)
      | otherwise -> throwError (onHiddenNetworks ("The coin " <> coinText outpoint) (fst <$> everywhere))
    (Just net, _) -> case onNetwork net of
      held : _ -> return held
      [] -> throwError $ "The coin " <> coinText outpoint <> " is on " <> networksText networks <> ", not on the --network " <> net.name
    (Nothing, [held]) -> return held
    (Nothing, _) -> case onNetwork (configDefaultNetwork cfg) of
      held : _ -> return held
      [] -> throwError $ "The coin " <> coinText outpoint <> " is on " <> networksText networks <> ": give the --network"

-- | Lock a coin of the account as lockcoin does, or take that lock off as
-- unlockcoin does
setLockedByUser :: (MonadUnliftIO m) => DBAccountId -> OutPoint -> Bool -> ExceptT String (DB m) ()
setLockedByUser accId outpoint locked =
  lift $ P.update (coinKey accId outpoint) [DBCoinLockedByUser P.=. locked]

-- | The coins of the account the user locked, in the order of their
-- outpoints: what exportwallet writes
userLockedCoins :: (MonadUnliftIO m) => DBAccountId -> DB m [DBCoin]
userLockedCoins accId =
  map P.entityVal <$> P.selectList [DBCoinAccount P.==. accId, DBCoinLockedByUser P.==. True] [P.Asc DBCoinOutpoint]

-- | A coin as coins shows it now, with where it is: what show answers. It
-- changes nothing.
shownCoin :: (MonadUnliftIO m) => (Network, DBCoin) -> ExceptT String (DB m) (Place, JsonCoin)
shownCoin (net, row) = do
  acc <- getAccountById (dBCoinAccount row)
  let pool = accountPool acc
  place <- poolPlace pool
  rules <- spendingOf net
  coin <- toJsonCoin rules pool row
  return (place, coin)

{- The coins against their transactions -}

-- | Refuse a coin the transaction it comes from does not pay as the store
-- reports it: the transaction is named by its hash, so its output is what
-- the coin holds, whatever the store says. A store that reported a coin
-- below its value would have hw build a payment whose fee takes the
-- difference: 1 btc reported as 0.0011 btc gave a fee of 0.9989 btc. The
-- transaction must be the coin's own, of the hash of its outpoint.
checkCoinOutput :: Tx -> Store.Unspent -> Either String ()
checkCoinOutput parent coin = do
  output <-
    maybe (Left (named <> ", but its transaction has " <> show (length parent.outputs) <> " outputs")) Right $
      listToMaybe (drop (fromIntegral coin.outpoint.index) parent.outputs)
  when (output.value /= coin.value) $
    Left (named <> " as worth " <> show coin.value <> " satoshis, but its transaction pays " <> show output.value)
  when (output.script /= coin.script) $
    Left (named <> " at another script than its transaction pays")
  where
    named = "The store reports the coin " <> coinText coin.outpoint

-- | Whether the raw transaction stored under the hash of its outpoint
-- pays each coin the store lists ('checkCoinOutput'): Left the reason to
-- refuse the answer of the store, for the first coin it does not pay. The
-- sync asks this once it has stored the raw transactions, before it
-- stores a coin. A coin whose raw transaction is not stored is not
-- checked: no builder spends it, since signing needs the raw transaction.
--
-- The refusal is given, not thrown: it is a failure of the store, which
-- the sync tells the store of ('Haskoin.Wallet.Store.refusedAnswer').
-- A failure of the action itself is one of the database: a stored raw
-- transaction that does not read.
checkListedCoins :: (MonadUnliftIO m) => [Store.Unspent] -> ExceptT String (DB m) (Either String ())
checkListedCoins coins = do
  checks <- forM coins $ \coin -> do
    parentM <- lookupRawTx coin.outpoint.hash
    return $ maybe (Right ()) (`checkCoinOutput` coin) parentM
  return (sequence_ checks)

-- | The transactions the coins come from, each once, in the order of the
-- coins, as a builder puts them in the signing data: read from the
-- database, and each coin checked against its output ('checkCoinOutput'),
-- so that no transaction is built on a value that a store got wrong.
coinParents :: (MonadUnliftIO m) => [Store.Unspent] -> ExceptT String (DB m) [Tx]
coinParents coins = do
  parents <- mapM getRawTx $ nubOrd $ (.outpoint.hash) <$> coins
  liftEither $ checkCoinsPaid parents coins
  return parents

-- | Refuse a coin whose transaction is not among the transactions given,
-- by its hash, or does not pay it ('checkCoinOutput'): for coins and
-- transactions that come from the store together, as those of a sweep
checkCoinsPaid :: [Tx] -> [Store.Unspent] -> Either String ()
checkCoinsPaid parents coins =
  forM_ coins $ \coin -> do
    parent <-
      maybe (Left ("The store gave no transaction of the coin " <> coinText coin.outpoint)) Right $
        Map.lookup coin.outpoint.hash byHash
    checkCoinOutput parent coin
  where
    byHash = Map.fromList [(txHash tx, tx) | tx <- parents]

-- | The coins among the given ones that the inputs of the transaction
-- spend, in the order of the inputs
coinsSpentBy :: Tx -> [Store.Unspent] -> [Store.Unspent]
coinsSpentBy tx coins = mapMaybe (\input -> Map.lookup input.outpoint byOutpoint) tx.inputs
  where
    byOutpoint = Map.fromList [(c.outpoint, c) | c <- coins]

{- Refreshing -}

-- | What a refresh changed: how many coins were inserted, rewritten or
-- deleted, the coins inserted and deleted, and the locks of the user that
-- moved from a coin deleted to a coin inserted ('lockHeirs'), each as the
-- coin that lost the lock, then the coin that took it
data Refreshed = Refreshed
  { refreshedCount :: !Int,
    insertedCoins :: ![DBCoin],
    deletedCoins :: ![DBCoin],
    movedLocks :: ![(OutPoint, OutPoint)]
  }
  deriving (Eq, Show)

instance Semigroup Refreshed where
  a <> b =
    Refreshed
      { refreshedCount = a.refreshedCount + b.refreshedCount,
        insertedCoins = a.insertedCoins <> b.insertedCoins,
        deletedCoins = a.deletedCoins <> b.deletedCoins,
        movedLocks = a.movedLocks <> b.movedLocks
      }

instance Monoid Refreshed where
  mempty = Refreshed 0 [] [] []

-- | Bring the coins of the account at the addresses in line with what the
-- store lists: insert the coins that are new, rewrite those whose block
-- or time changed, delete those the store no longer lists. A coin the
-- store lists twice is stored once. The value of a coin never changes,
-- and neither does whether it holds tokens, so one that differs is a
-- stored one gone wrong, which a full sync repairs here.
--
-- A coin the user locked may go while the coin of the transaction that
-- replaced its own comes at the same address. The new coin is then
-- stored locked ('lockHeirs').
--
-- Each coin takes the time of the transaction that paid it, which the
-- function given tells: the time of its summary, which the sync stores
-- before the coins. coins shows a coin with that time. The time of a
-- summary changes only when its transaction is fetched again, as when it
-- is confirmed, and the coins at its addresses are then refreshed too.
refreshCoins ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  (TxHash -> UTCTime) ->
  [Address] ->
  [Store.Unspent] ->
  ExceptT String (DB m) Refreshed
refreshCoins net accId timeOf addrs unspents = mconcat <$> mapM refreshAddress addrs
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
                  || coin.value /= dBCoinValue row
                  || timeOf coin.outpoint.hash /= dBCoinTime row
                  || holdsTokens net coin.script /= dBCoinTokens row
            ]
      heirs <- lockHeirs gone ((.outpoint) <$> new)
      lift $ forM_ gone deleteCoin
      lift $ forM_ moved $ \coin -> rewriteCoin net accId (timeOf coin.outpoint.hash) coin
      inserted <- forM new $ \coin ->
        insertCoin net accId (coin.outpoint `elem` (snd <$> heirs)) (timeOf coin.outpoint.hash) coin
      return
        Refreshed
          { refreshedCount = length gone + length moved + length new,
            insertedCoins = inserted,
            deletedCoins = gone,
            movedLocks = heirs
          }

-- | The locks of the user that move when coins of an address go and
-- others come: each coin that is gone and that the user had locked, with
-- the new coin that takes its lock.
--
-- Whoever made a payment that has no block yet can replace it, to raise
-- its fee. The same payment then arrives under another id, so its coin
-- is a new one, and the coin the user locked is gone. The user locked
-- the money and not an id: without the move, the next payment could
-- spend what was set aside, and nothing would say so.
--
-- A new coin takes the lock when its transaction replaced the
-- transaction of the locked coin, that is, when the two spend a coin in
-- common. The raw transactions tell, and both are stored here: the sync
-- stores those of the new coins before the coins, and drops those of the
-- coins that are gone after them. The summaries cannot tell, since they
-- keep only the coins of the wallet that a transaction spends. A raw
-- transaction that is missing or does not read tells nothing, and the
-- lock then goes with its coin.
--
-- Two cases are left out on purpose. A locked coin that is spent, or
-- whose payment was dropped, has no replacement, and its lock goes with
-- it. A replacement that pays another address of the wallet takes no
-- lock, since the refresh goes one address at a time.
lockHeirs :: (MonadUnliftIO m) => [DBCoin] -> [OutPoint] -> ExceptT String (DB m) [(OutPoint, OutPoint)]
lockHeirs gone new = do
  locked <- liftEither $ mapM coinOutpoint (filter dBCoinLockedByUser gone)
  if null locked || null new
    then return []
    else do
      txs <- Map.fromList . catMaybes <$> mapM rawOf (nubOrd ((.hash) <$> locked <> new))
      return (heirsOf txs locked new)
  where
    rawOf hash = either (const Nothing) (fmap (hash,)) <$> lift (runExceptT (lookupRawTx hash))

-- | Each new coin whose transaction replaced the transaction of a locked
-- coin, after that locked coin: the pairs of 'lockHeirs', from the raw
-- transactions known. A transaction that pays the address twice gives
-- two heirs to one locked coin, and both take the lock: too many locks
-- show in coins, and a lock too few does not.
heirsOf :: Map TxHash Tx -> [OutPoint] -> [OutPoint] -> [(OutPoint, OutPoint)]
heirsOf txs locked new =
  [(old, heir) | heir <- new, old <- take 1 (filter (`replacedBy` heir) locked)]
  where
    replacedBy old heir =
      old.hash /= heir.hash
        && fromMaybe False (spendSameCoin <$> Map.lookup old.hash txs <*> Map.lookup heir.hash txs)

-- | Whether two transactions spend a coin in common. A chain holds one of
-- them at most, so when the network has one, it took it in the place of
-- the other.
spendSameCoin :: Tx -> Tx -> Bool
spendSameCoin a b = any (`elem` spent b) (spent a)
  where
    spent tx = (.outpoint) <$> tx.inputs

-- | Store a coin of the account, at the address it pays, with the time
-- of the transaction that paid it, locked by the user or not. Whether the
-- coin holds tokens is stored with it ('holdsTokens'): the row does not
-- keep the script field that tells, and 'tokenOutpoints' reads it for
-- every listing and every payment.
insertCoin :: (MonadUnliftIO m) => Network -> DBAccountId -> Bool -> UTCTime -> Store.Unspent -> ExceptT String (DB m) DBCoin
insertCoin net accId locked paidAt coin = do
  addr <-
    liftMaybe ("The coin " <> coinText coin.outpoint <> " pays no address of the network") $
      addrToText net =<< coin.address
  time <- liftIO getCurrentTime
  -- A row with this key holds the coin under another address, which only
  -- a database gone wrong does. The coin takes the row over, so that a
  -- full sync gets past it, and what the user locked stays locked.
  let key = coinKey accId coin.outpoint
  previousM <- lift $ P.get key
  let row =
        DBCoin
          { dBCoinAccount = accId,
            dBCoinOutpoint = outpointBytes coin.outpoint,
            dBCoinAddress = addr,
            dBCoinValue = coin.value,
            dBCoinBlockRef = S.encode coin.block,
            dBCoinTime = paidAt,
            dBCoinLockedByUser = locked || maybe False dBCoinLockedByUser previousM,
            dBCoinTokens = holdsTokens net coin.script,
            dBCoinCreated = time
          }
  lift $ P.repsert key row
  return row

-- | Give a stored coin the block and the value the store now reports for
-- it, the time of the transaction that paid it, and whether it holds
-- tokens. Whether the user locked it stays.
rewriteCoin :: (MonadUnliftIO m) => Network -> DBAccountId -> UTCTime -> Store.Unspent -> DB m ()
rewriteCoin net accId paidAt coin =
  P.update
    (coinKey accId coin.outpoint)
    [DBCoinBlockRef P.=. S.encode coin.block, DBCoinValue P.=. coin.value, DBCoinTime P.=. paidAt, DBCoinTokens P.=. holdsTokens net coin.script]

-- | Delete a stored coin
deleteCoin :: (MonadUnliftIO m) => DBCoin -> DB m ()
deleteCoin row = P.delete (DBCoinKey (dBCoinAccount row) (dBCoinOutpoint row))

{- The coins a transaction leaves -}

-- | Why a transaction just prepared leaves a coin of the portfolio: a coin
-- that coin control chose, or, for a send-all, any coin of the portfolio
-- on the network
data LeftReason
  = -- | Worth less than the fee to spend it, and left as the user chose
    WorthLessThanFee
  | -- | Locked, by the tokens it holds, by the user, by a pending
    -- transaction, which spends it or replaces its transaction, or by a
    -- tag of its address that the payment did not name
    Locked !CoinLock
  | -- | Not confirmed enough yet, or a coinbase not yet mature
    NotSpendableYet
  | -- | Spendable, but not among the coins chosen with --coin,
    -- --spend-from or --spend-from-tag
    NotGiven
  | -- | Spendable, but of the type of address --coin-type leaves out
    OtherType
  deriving (Eq, Show)

instance ToJSON LeftReason where
  toJSON = String . cs . reasonName

instance FromJSON LeftReason where
  parseJSON = withText "LeftReason" $ \name ->
    case filter ((== cs name) . reasonName) leftReasons of
      [reason] -> return reason
      _ -> fail $ "Unknown reason a coin stays in the portfolio: " <> cs name

-- | Every reason, in the order the answer lists the coins in
leftReasons :: [LeftReason]
leftReasons = [WorthLessThanFee, Locked LockedByTokens, Locked LockedByUser, Locked LockedByTag, Locked LockedByTransaction, Locked LockedByReplacement, NotSpendableYet, NotGiven, OtherType]

-- | The name of a reason in JSON
reasonName :: LeftReason -> String
reasonName = \case
  WorthLessThanFee -> "dust"
  Locked LockedByTokens -> "lockedbytokens"
  Locked LockedByUser -> "lockedbyuser"
  Locked LockedByTag -> "lockedbytag"
  Locked LockedByTransaction -> "lockedbytransaction"
  Locked LockedByReplacement -> "lockedbyreplacement"
  NotSpendableYet -> "notspendableyet"
  NotGiven -> "notgiven"
  OtherType -> "othertype"

-- | A reason as the user reads it. The lock of a tag names the tags
-- given, those that hold the coin against the payment
-- ('Haskoin.Wallet.Database.PendingTx.tagLocks'): an address may have
-- several tags, and the payment may have named one of them, so "a tag"
-- alone would read as a refusal of the tag the user just gave. Without
-- them, as for a coin read back from JSON that names none, it says that
-- a tag does.
leftReasonText :: [Tag] -> LeftReason -> String
leftReasonText lockingTags = \case
  WorthLessThanFee -> "worth less than its fee"
  Locked LockedByTokens -> "it holds tokens, and hw never spends such a coin"
  Locked LockedByUser -> "you locked it"
  Locked LockedByTag -> case cs . tagText <$> lockingTags of
    [] -> "a tag of its address locks it"
    [tag] -> "the tag " <> tag <> " locks it"
    tags -> "the tags " <> intercalateAnd tags <> " lock it"
  Locked LockedByTransaction -> "a pending transaction spends it"
  Locked LockedByReplacement -> "a pending transaction replaces the payment it comes from"
  NotSpendableYet -> "not spendable yet"
  NotGiven -> "not chosen with --coin, --spend-from or --spend-from-tag"
  OtherType -> "not of the type --coin-type sends"

-- | Why a coin the builder may not spend stays, from its lock: without one,
-- it is not spendable yet
lockReason :: Maybe CoinLock -> LeftReason
lockReason = maybe NotSpendableYet Locked

-- | A coin of the portfolio a transaction leaves in it, and why. The
-- answer of preparetx and preparesendall lists them after the
-- transaction, each as coins shows it, with why it stays. Its JSON is
-- that of the coin, with the reason beside.
data LeftCoin = LeftCoin
  { -- | The coin as coins shows it now
    leftCoin :: !JsonCoin,
    leftCoinReason :: !LeftReason,
    -- | The tags that lock it against the payment, for a coin a tag
    -- locks: those of its address that lock their coins and that the
    -- payment did not name. In JSON, under "lockingtags".
    leftCoinLockingTags :: ![Tag]
  }
  deriving (Eq, Show)

instance MarshalJSON Network LeftCoin where
  marshalValue net left =
    object (jsonCoinPairs net left.leftCoin <> ["reason" .= left.leftCoinReason, "lockingtags" .= left.leftCoinLockingTags])
  unmarshalValue net value =
    LeftCoin
      <$> unmarshalValue net value
      <*> withObject "LeftCoin" (.: "reason") value
      <*> withObject "LeftCoin" (\o -> o .:? "lockingtags" .!= []) value

-- | Why a coin a transaction leaves stays, as the user reads it
leftCoinWhy :: LeftCoin -> String
leftCoinWhy left = leftReasonText left.leftCoinLockingTags left.leftCoinReason

-- | Why the coins of the portfolio a transaction leaves stay, each by its
-- outpoint, by reason in the order of 'leftReasons', the largest first:
-- the dust coins left, the coins the builder may not spend with their
-- locks, and the spendable coins not chosen with coin control.
-- 'leftCoinsOf' then finds them as coins shows them.
leftOutpoints :: [Store.Unspent] -> [(Store.Unspent, Maybe CoinLock)] -> [Store.Unspent] -> [(OutPoint, LeftReason)]
leftOutpoints dustLeft unspendable notGiven =
  [ (coin.outpoint, reason)
    | reason <- leftReasons,
      coin <- sortOn (Down . (.value)) [coin | (coin, why) <- everyLeft, why == reason]
  ]
  where
    everyLeft =
      [(coin, WorthLessThanFee) | coin <- dustLeft]
        <> [(coin, lockReason lock) | (coin, lock) <- unspendable]
        <> [(coin, NotGiven) | coin <- notGiven]

-- | The spendable coins a send-all leaves because --coin-type sends the
-- other type, each by its outpoint, the largest first. They come after
-- the coins 'leftOutpoints' gives.
leftOfOtherType :: [Store.Unspent] -> [(OutPoint, LeftReason)]
leftOfOtherType others = [(coin.outpoint, OtherType) | coin <- sortOn (Down . (.value)) others]

-- | The coins a transaction leaves in the pool, each as coins shows it
-- now, with why it stays, in the order given. A coin that a tag locks
-- comes with the tags that lock it against the payment, which named the
-- tags given with --spend-from-tag.
leftCoinsOf :: (MonadUnliftIO m) => Pool -> [Tag] -> [(OutPoint, LeftReason)] -> ExceptT String (DB m) [LeftCoin]
leftCoinsOf pool opened left = do
  views <- poolCoinViews pool (fst <$> left)
  closed <- tagLocks opened pool.poolNetwork
  let byOutpoint = Map.fromList [(jsonCoinOutpoint view, view) | view <- views]
      lockingTags outpoint reason = if reason == Locked LockedByTag then Map.findWithDefault [] outpoint closed else []
  return [LeftCoin view reason (lockingTags outpoint reason) | (outpoint, reason) <- left, Just view <- [Map.lookup outpoint byOutpoint]]

-- | The coins of the pool with these outpoints, as coins shows them now,
-- in the order given. The reports of preparesendall and bumpfee show
-- coins so before their questions, and the answers of preparetx and
-- preparesendall the coins a payment leaves. An outpoint the pool does
-- not hold is left out.
poolCoinViews :: (MonadUnliftIO m) => Pool -> [OutPoint] -> ExceptT String (DB m) [JsonCoin]
poolCoinViews pool outpoints = do
  rules <- spendingOf pool.poolNetwork
  accIds <- lift $ map fst <$> poolAccounts pool
  rows <- lift $ forM outpoints $ \outpoint ->
    listToMaybe . catMaybes <$> mapM (\accId -> P.get (coinKey accId outpoint)) accIds
  mapM (toJsonCoin rules pool) (catMaybes rows)

{- Coin control -}

-- | The coins that coin control chose among the coins of the pool, by
-- their outpoints and by the addresses that hold them, those named and
-- those of the tags named: those the builder
-- may spend now, and the others, each with its lock. The addresses count,
-- not their text. An outpoint the pool does not hold is refused, and so
-- is a choice none of whose coins can be spent, with why.
--
-- That refusal names the tags that lock a coin against the payment, and
-- the option that opens them. --coin and --spend-from do not open the
-- lock of a tag, and a user who gave one of two tags that lock an
-- address would read "a tag locks it" as a refusal of the tag given.
chosenCoins ::
  -- | The outpoints chosen
  [OutPoint] ->
  -- | The addresses chosen
  [Address] ->
  -- | The tags that lock each coin against the payment, from
  -- 'Haskoin.Wallet.Database.PendingTx.tagLocks'
  Map OutPoint [Tag] ->
  -- | The coins the builder may spend, from 'spendableCoins'
  [Store.Unspent] ->
  -- | The others, from 'unspendableCoins'
  [(Store.Unspent, Maybe CoinLock)] ->
  Either String ([Store.Unspent], [(Store.Unspent, Maybe CoinLock)])
chosenCoins outpoints addrs closed spendable unspendable = do
  forM_ outpoints $ \outpoint ->
    unless (outpoint `elem` held) $ Left $ "The portfolio has no coin " <> coinText outpoint <> " on the network of the payment"
  when (null chosenSpendable) $
    Left $ case chosenHeld of
      [] -> "The addresses chosen with --spend-from or --spend-from-tag hold no coins"
      _ -> "None of the coins chosen can be spent now: " <> intercalate "; " (heldText <$> chosenHeld) <> opening
  return (chosenSpendable, chosenHeld)
  where
    held = (.outpoint) <$> (spendable <> (fst <$> unspendable))
    chosen coin = coin.outpoint `elem` outpoints || maybe False (`elem` addrs) coin.address
    chosenSpendable = filter chosen spendable
    chosenHeld = filter (chosen . fst) unspendable
    lockingTags (coin, lock) = if lock == Just LockedByTag then Map.findWithDefault [] coin.outpoint closed else []
    heldText chosenCoin@(coin, lock) = coinText coin.outpoint <> ", " <> leftReasonText (lockingTags chosenCoin) (lockReason lock)
    -- What opens the locks of the tags named, after the coins
    opening = case cs . tagText <$> nubOrd (concatMap lockingTags chosenHeld) of
      [] -> ""
      [tag] -> ". Only a payment that names the tag spends its coins: --spend-from-tag " <> tag
      tags -> ". Only a payment that names these tags spends their coins: " <> unwords (("--spend-from-tag " <>) <$> tags)
