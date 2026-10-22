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
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Default (def)
import Data.List (nub, partition, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO (TxSignData (..))
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSigning (parseTxSignData)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import UnliftIO.Async (pooledMapConcurrentlyN)

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
    -- | The raw transactions with the given ids; a transaction the store
    -- does not know is left out
    storeRawTransactions :: [TxHash] -> ExceptT String IO [Tx],
    -- | Hand a signed transaction to the network. The store passes it to
    -- its peers without checking it and says nothing of their answer: only
    -- the store listing it afterwards tells that the network took it.
    storePublish :: Tx -> ExceptT String IO ()
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
        raw <$> batch (configTxFullBatch cfg) (GetTxsRaw tids),
      storePublish = \tx -> do
        Store.TxId published <- call (PostTx tx)
        unless (published == txHash tx) $
          throwError $ "The store returned the wrong transaction id: " <> cs (txHashToHex published)
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

-- | Ask the store several questions, at most the given number at a time,
-- and answer them in the order asked. One failed question fails them all.
inParallel :: Natural -> [ExceptT String IO a] -> ExceptT String IO [a]
inParallel atATime questions = do
  let threads = max 1 (fromIntegral atATime)
  answers <- liftIO $ pooledMapConcurrentlyN threads runExceptT questions
  liftEither $ sequence answers

{- Broadcasting -}

-- | How long 'broadcast' waits for the store to see a transaction, in
-- seconds
acceptanceWait :: Int
acceptanceWait = 10

-- | Hand a signed transaction to the network and wait for the store to see
-- it in its mempool, the only sign that the network accepted it: True once
-- seen, False after the wait. The transaction may then have been refused,
-- for a fee below what the nodes relay for instance, or may just be slow
-- to come back.
broadcast :: Store -> Tx -> ExceptT String IO Bool
broadcast = broadcastWith 1000000 acceptanceWait

-- | 'broadcast' asking the store the given number of times, waiting the
-- given microseconds before each
broadcastWith :: Int -> Int -> Store -> Tx -> ExceptT String IO Bool
broadcastWith pause asks store tx = do
  storePublish store tx
  seen asks
  where
    seen :: Int -> ExceptT String IO Bool
    seen 0 = return False
    seen left = do
      liftIO $ threadDelay pause
      known <- storeTransactions store [txHash tx]
      if any (\t -> t.txid == txHash tx && not t.deleted) known
        then return True
        else seen (left - 1)

-- | What sendtx answers when the store has not seen the transaction
notAccepted :: String
notAccepted =
  "The transaction was handed to the network but the store has not seen it after "
    <> show acceptanceWait
    <> " seconds. It stays signed and not online: run sendtx again later"

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
    syncResCoinUpdates :: !Natural,
    -- | Transactions the user sent that the store had lost, sent again
    syncResRebroadcast :: !Natural,
    -- | Pending transactions a coin of which was spent by another
    -- transaction
    syncResConflicted :: !Natural
  }
  deriving (Eq, Show)

-- | Bring an account up to date with the store.
--
-- An incremental sync looks only at the addresses whose balance changed
-- since the last sync, which is every address a new, confirmed, replaced
-- or dropped transaction touches. A full sync, asked for or forced by a
-- reorganisation of the chain, looks at every address and also marks dead
-- the summaries of transactions the store no longer lists.
sync ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  Store ->
  Network ->
  DBAccountId ->
  Bool ->
  ExceptT String (DB m) SyncRes
sync ctx cfg store net accId full = do
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
  -- Every transaction the store lists for each changed address. A
  -- transaction touching two of them is listed under both.
  listedByAddress <- listTxs cfg store net changedBals
  let listed = concatMap snd listedByAddress
  -- The summaries already stored, with their blocks
  known <- txInfoBlockRefs accId
  -- What to fetch in full: the listed transactions that are new or whose
  -- block changed; the unconfirmed summaries, whose transactions may have
  -- been confirmed, replaced or dropped since the last sync; and every
  -- transaction of an address that has transactions for the first time. A
  -- summary is built with the addresses the wallet knows at the time, so a
  -- transaction that also paid an address generated later, say a batch
  -- payout from someone deriving the public key, shows that output as
  -- somebody else's until it is rebuilt.
  let changed = [r.txid | r <- listed, Map.lookup r.txid known /= Just r.block]
      unconfirmed = [tid | (tid, ref) <- Map.toList known, not (Store.confirmed ref)]
      storedCount bal = maybe 0 addrBalanceTxs $ Map.lookup bal.address addrBalMap
      firstSeen = [r.txid | (bal, refs) <- listedByAddress, storedCount bal == 0, r <- refs]
      toFetch = nub $ changed <> unconfirmed <> firstSeen
  txs <- fromStore $ storeTransactions store toFetch
  -- A transaction the store reports as deleted, or does not return at all,
  -- is dead: replaced, dropped from the mempool, or left out by a
  -- reorganisation. Its summary is marked dead and kept, for the history.
  -- Dead summaries are not among the known ones, so a transaction the store
  -- lists again later is fetched and its summary comes back to life.
  let live = filter (not . (.deleted)) txs
      alive = Set.fromList $ (.txid) <$> live
      dead = [tid | tid <- toFetch, tid `Set.notMember` alive, tid `Map.member` known]
      txInfos = storeToTxInfo addrPathMap (fromIntegral best.height) <$> live
  resTxInfo <- forM txInfos $ repsertTxInfo net ctx accId
  lift $ forM_ dead $ markTxInfoDead accId
  -- After a reorganisation a confirmed transaction may have vanished without
  -- a trace in the addresses it touched; a full sync lists every
  -- transaction of every address, so what is not listed is dead
  when everything $ do
    let stillListed = Set.fromList $ (.txid) <$> listed
        vanished = [tid | tid <- Map.keys known, tid `Set.notMember` stillListed]
    lift $ forM_ vanished $ markTxInfoDead accId
  -- The coins of the changed addresses, all of them
  unspents <- fromStore $ storeUnspents store changedAddrs
  let countedCoins = sum $ (.utxo) <$> changedBals
  when (fromIntegral (length unspents) /= countedCoins) $
    throwError $
      "The store counts "
        <> show countedCoins
        <> " coins for the addresses to update but listed "
        <> show (length unspents)
  (coinCount, _) <- refreshCoins net accId changedAddrs unspents
  -- The raw transactions the coins come from, which signing needs. A raw
  -- transaction never changes, so the ones already stored are not fetched
  -- again. The store must have every one asked for: it lists the coins.
  let parents = nub $ (.outpoint.hash) <$> unspents
  missing <- lift $ missingRawTxs parents
  unless (null missing) $ do
    rawTxs <- fromStore $ storeRawTransactions store missing
    when (length rawTxs /= length missing) $
      throwError $
        "The store was asked for "
          <> show (length missing)
          <> " raw transactions but returned "
          <> show (length rawTxs)
    lift $ forM_ rawTxs insertRawTx
  -- The pending transactions. A signed one the store lists has reached the
  -- network and is done with. One the user sent that the store does not
  -- list is sent again: nodes forget unconfirmed transactions, and the
  -- store may never have accepted it; a signed one the user has not sent is
  -- left alone. One whose coins are gone conflicts with the transaction
  -- that spent them, and is counted for the user.
  pending <- pendingTxs accId
  let seen = Set.fromList $ (.txid) <$> listed
      reached p = p.pendingTxData.txSignDataSigned && txHash p.pendingTxData.txSignDataTx `Set.member` seen
      (done, waiting) = partition reached pending
  lift $ forM_ done $ deletePendingTxOnline . (.pendingTxKey)
  let resend = [p.pendingTxData.txSignDataTx | p <- waiting, p.pendingTxOnline]
  forM_ resend $ fromStore . storePublish store
  let pub = accountXPubKey ctx newAcc
  conflicted <- filterM (inConflict net pub) waiting
  lift $ updateBest net (headerHash best.header) best.height
  return $
    SyncRes
      newAcc
      (headerHash best.header)
      (fromIntegral best.height)
      (fromIntegral $ length $ filter id $ snd <$> resTxInfo)
      (fromIntegral coinCount)
      (fromIntegral $ length resend)
      (fromIntegral $ length conflicted)
  where
    inConflict net' pub p = do
      txInfo <- liftEither $ parseTxSignData net' ctx pub p.pendingTxData
      not . null <$> lift (conflictedInputs txInfo)

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

-- | Every transaction the store lists for each address that has any: as
-- many as it counts in the balance, which a listing cut short would not
-- match. The addresses are not listed together in one request, because the
-- store would merge a transaction listed under two of them into one and
-- the counts could no longer be checked; the listings are asked in
-- parallel instead, as many at a time as the configuration allows.
listTxs ::
  (MonadUnliftIO m) =>
  Config ->
  Store ->
  Network ->
  [Store.Balance] ->
  ExceptT String (DB m) [(Store.Balance, [Store.TxRef])]
listTxs cfg store net bals = do
  let used = filter ((/= 0) . (.txs)) bals
  listings <-
    fromStore $
      inParallel
        (configParallelRequests cfg)
        [storeAddressTxs store bal.address (fromIntegral bal.txs) | bal <- used]
  forM (zip used listings) $ \(bal, refs) -> do
    when (fromIntegral (length refs) /= bal.txs) $
      throwError $
        "The store counts "
          <> show bal.txs
          <> " transactions for "
          <> cs (fromJust $ addrToText net bal.address)
          <> " but listed "
          <> show (length refs)
    return (bal, refs)

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
