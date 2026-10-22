{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Synchronising an account with the store: address discovery, balances,
-- transaction summaries, coins and the raw transactions the coins come from.
--
-- The sync talks to the store through the 'Store' record. hw fills it with
-- the web client ('webStore'); the tests fill it with scripted answers, so
-- that reorganisations, replaced transactions and large wallets can be
-- played out without a network.
module Haskoin.Wallet.Sync where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Default (def)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

{- The store -}

-- | The questions the sync asks the store. Every answer is complete: a
-- function that lists things lists all of them, and the sync checks the
-- counts the store gives against the lists it returns, so that a truncated
-- answer is an error rather than lost coins or transactions.
data Store = Store
  { -- | Fails when the store is not in a state to be trusted
    storeHealth :: ExceptT String IO (),
    -- | The best block of the chain
    storeBestBlock :: ExceptT String IO Store.BlockData,
    -- | The blocks the store knows at a height, in the main chain or not
    storeBlocksAtHeight :: BlockHeight -> ExceptT String IO [Store.BlockData],
    -- | The balance of each of the addresses, including its transaction
    -- and coin counts
    storeBalances :: [Address] -> ExceptT String IO [Store.Balance],
    -- | The newest n transactions of an address, newest first
    storeAddressTxs :: Address -> Natural -> ExceptT String IO [Store.TxRef],
    -- | The transactions with the given ids, deleted ones included; a
    -- transaction the store does not know is left out
    storeTransactions :: [TxHash] -> ExceptT String IO [Store.Transaction],
    -- | Every unspent output of the addresses
    storeUnspents :: [Address] -> ExceptT String IO [Store.Unspent],
    -- | The raw transactions with the given ids
    storeRawTransactions :: [TxHash] -> ExceptT String IO [Tx]
  }

-- | The store of hw: the web API of a haskoin-store, with the batch sizes
-- of the configuration
webStore :: Ctx -> Config -> Network -> Store
webStore ctx cfg net =
  Store
    { storeHealth = do
        health <- call GetHealth
        unless (Store.isOK health) $
          throwError "The indexer health check has failed",
      storeBestBlock = call (GetBlockBest def),
      storeBlocksAtHeight = \height ->
        serial <$> call (GetBlockHeight (HeightParam (fromIntegral height)) def),
      storeBalances = \addrs ->
        serial <$> batch (configAddrBatch cfg) (GetAddrsBalance addrs),
      storeAddressTxs = \addr count ->
        serial <$> call (GetAddrTxs addr def {limit = Just (fromIntegral count)}),
      storeTransactions = \tids ->
        serial <$> batch (configTxFullBatch cfg) (GetTxs tids),
      storeUnspents = \addrs ->
        serial <$> batch (configCoinBatch cfg) (GetAddrsUnspent addrs everything),
      storeRawTransactions = \tids ->
        raw <$> batch (configTxFullBatch cfg) (GetTxsRaw tids)
    }
  where
    host = apiHost net cfg
    call req = liftExcept $ apiCall ctx host req
    batch size req = liftExcept $ apiBatch ctx size host req
    serial (Store.SerialList xs) = xs
    raw (Store.RawResultList xs) = xs
    -- A limit of zero asks the store for everything. Without a limit, the
    -- store answers with its default page of a hundred items and says
    -- nothing about the rest.
    everything = def {limit = Just 0}

-- | Ask the store from inside a database action
fromStore :: (MonadIO m) => ExceptT String IO a -> ExceptT String (DB m) a
fromStore action = ExceptT $ liftIO $ runExceptT action

{- Address discovery -}

-- | The number of external and internal addresses of an account that have
-- been used, found by scanning the chains of the public key: windows of
-- the recovery gap are checked for addresses with transactions until a whole
-- window has none. The scan therefore stops only after at least a recovery
-- gap of unused addresses, up to twice that many depending on where the
-- last used address falls in its window.
discoverAddrs ::
  (MonadIO m) =>
  Ctx ->
  Config ->
  Store ->
  XPubKey ->
  ExceptT String m (Natural, Natural)
discoverAddrs ctx cfg store pub = do
  let recoveryGap = configRecoveryGap cfg
  e <- go extDeriv 0 (Page recoveryGap 0)
  i <- go intDeriv 0 (Page recoveryGap 0)
  return (fromIntegral e, fromIntegral i)
  where
    go path d page@(Page lim off) = do
      let addrs = addrsDerivPage ctx path page pub
      bals <- ExceptT $ liftIO $ runExceptT $ storeBalances store (fst <$> addrs)
      let vBals = filter ((/= 0) . (.txs)) bals
      if null vBals
        then return d
        else do
          let dMax = findMax addrs $ (.address) <$> vBals
          go path (dMax + 1) (Page lim (off + lim))
    -- Find the largest ID amongst the addresses that have a positive balance
    findMax :: [(Address, SoftPath)] -> [Address] -> Int
    findMax addrs balAddrs =
      let fAddrs = filter ((`elem` balAddrs) . fst) addrs
       in fromIntegral $ maximum $ last . pathToList . snd <$> fAddrs

{- Synchronisation -}

data SyncRes = SyncRes
  { syncResAccount :: !DBAccount,
    syncResBlockHash :: !BlockHash,
    syncResBlockHeight :: !BlockHeight,
    syncResTxUpdates :: !Natural,
    syncResCoinUpdates :: !Natural
  }
  deriving (Eq, Show)

-- | Bring an account up to date with the store.
--
-- An incremental sync looks only at the addresses whose balance changed
-- since the last sync, which is every address a new, confirmed, replaced
-- or dropped transaction touches. A full sync, asked for or forced by a
-- reorganisation of the chain, looks at every address and also drops the
-- summaries of transactions the store no longer lists.
sync ::
  (MonadUnliftIO m) =>
  Ctx ->
  Store ->
  Network ->
  DBAccountId ->
  Bool ->
  ExceptT String (DB m) SyncRes
sync ctx store net accId full = do
  fromStore $ storeHealth store
  -- The best block is taken before anything else, so that a block found
  -- during the sync is caught up with on the next one rather than missed
  best <- fromStore $ storeBestBlock store
  reorganised <- chainReorganised store net
  let everything = full || reorganised
  -- The addresses of the account, with their stored balances
  (addrPathMap, addrBalMap) <- allAddressesMap net accId
  storeBals <- fromStore $ storeBalances store (Map.keys addrBalMap)
  changedBals <-
    if everything
      then return storeBals
      else liftEither $ filterAddresses storeBals addrBalMap
  let changedAddrs = (.address) <$> changedBals
  updateAddressBalances net changedBals
  newAcc <- lift $ updateAccountBalances accId
  -- Every transaction the store lists for the changed addresses. A
  -- transaction touching two of them is listed twice.
  listed <- concat <$> mapM (listTxs store net) changedBals
  -- The summaries already stored, with their blocks
  known <- txInfoBlockRefs accId
  -- What to fetch in full: the listed transactions that are new or whose
  -- block changed, and the unconfirmed summaries, whose transactions may
  -- have been confirmed, replaced or dropped since the last sync
  let changed = [r.txid | r <- listed, Map.lookup r.txid known /= Just r.block]
      unconfirmed = [tid | (tid, ref) <- Map.toList known, not (Store.confirmed ref)]
      toFetch = nub $ changed <> unconfirmed
  txs <- fromStore $ storeTransactions store toFetch
  -- A transaction the store reports as deleted, or does not return at all,
  -- is dead: replaced, dropped from the mempool, or left out by a
  -- reorganisation. Its summary goes.
  let live = filter (not . (.deleted)) txs
      alive = Set.fromList $ (.txid) <$> live
      dead = [tid | tid <- toFetch, tid `Set.notMember` alive, tid `Map.member` known]
      txInfos = storeToTxInfo addrPathMap (fromIntegral best.height) <$> live
  resTxInfo <- forM txInfos $ repsertTxInfo net ctx accId
  lift $ forM_ dead $ deleteTxInfo accId
  -- After a reorganisation a confirmed transaction may have vanished without
  -- a trace in the addresses it touched; a full sync lists every
  -- transaction of every address, so what is not listed is gone
  when everything $ do
    let stillListed = Set.fromList $ (.txid) <$> listed
        vanished = [tid | tid <- Map.keys known, tid `Set.notMember` stillListed]
    lift $ forM_ vanished $ deleteTxInfo accId
  -- The coins of the changed addresses, all of them
  unspents <- fromStore $ storeUnspents store changedAddrs
  let countedCoins = sum $ (.utxo) <$> changedBals
  when (fromIntegral (length unspents) /= countedCoins) $
    throwError $
      "The store counts "
        <> show countedCoins
        <> " coins for the addresses to update but listed "
        <> show (length unspents)
  (coinCount, newCoins) <- refreshCoins net accId changedAddrs unspents
  -- The raw transactions the coins come from, which signing needs
  depTxsHash <-
    if everything
      then return $ (.outpoint.hash) <$> unspents
      else mapM (liftEither . coinToTxHash) newCoins
  rawTxs <- fromStore $ storeRawTransactions store $ nub depTxsHash
  lift $ forM_ rawTxs insertRawTx
  -- A pending transaction the store lists has been sent to the network
  pendingTids <- pendingTxHashes accId
  let seen = Set.fromList $ (.txid) <$> listed
      sent = filter ((`Set.member` seen) . fst) pendingTids
  forM_ sent $ \(_, key) -> lift $ deletePendingTxOnline key
  lift $ updateBest net (headerHash best.header) best.height
  return $
    SyncRes
      newAcc
      (headerHash best.header)
      (fromIntegral best.height)
      (fromIntegral $ length $ filter id $ snd <$> resTxInfo)
      (fromIntegral coinCount)

-- | Whether the chain has moved away from the block of the last sync: that
-- block is no longer in the main chain at its height. Balances alone do not
-- show a reorganisation that leaves them unchanged while it moves
-- transactions to other blocks, so a full sync follows.
chainReorganised :: (MonadUnliftIO m) => Store -> Network -> ExceptT String (DB m) Bool
chainReorganised store net = do
  bestM <- lift $ getBest net
  case bestM of
    Nothing -> return False
    Just (hash, height) -> do
      blocks <- fromStore $ storeBlocksAtHeight store height
      let stillMain b = headerHash b.header == hash && b.main
      return $ not $ any stillMain blocks

-- | Every transaction the store lists for an address: as many as it counts
-- in the balance, which a listing cut short would not match
listTxs ::
  (MonadUnliftIO m) => Store -> Network -> Store.Balance -> ExceptT String (DB m) [Store.TxRef]
listTxs store net bal
  | bal.txs == 0 = return []
  | otherwise = do
      refs <- fromStore $ storeAddressTxs store bal.address (fromIntegral bal.txs)
      when (fromIntegral (length refs) /= bal.txs) $
        throwError $
          "The store counts "
            <> show bal.txs
            <> " transactions for "
            <> cs (fromJust $ addrToText net bal.address)
            <> " but listed "
            <> show (length refs)
      return refs

-- | The balances that differ from the stored ones: the addresses to update
filterAddresses ::
  [Store.Balance] ->
  Map Address AddressBalance ->
  Either String [Store.Balance]
filterAddresses sBals aMap
  | sort ((.address) <$> sBals) /= sort (Map.keys aMap) =
      Left "Sync: addresses do not match"
  | otherwise =
      Right $ filter f sBals
  where
    -- The address is in the map: the guard above checked the two sets
    f s =
      let b = fromJust $ s.address `Map.lookup` aMap
       in s.txs /= addrBalanceTxs b
            || s.confirmed /= addrBalanceConfirmed b
            || s.unconfirmed /= addrBalanceUnconfirmed b
            || s.utxo /= addrBalanceCoins b

coinToTxHash :: DBCoin -> Either String TxHash
coinToTxHash coin =
  maybeToEither "coinToTxHash: Invalid outpoint" $ do
    bs <- decodeHex $ dBCoinOutpoint coin
    op <- eitherToMaybe (S.decode bs) :: Maybe OutPoint
    return op.hash
