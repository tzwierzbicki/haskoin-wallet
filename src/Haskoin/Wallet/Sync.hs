{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Synchronising an account with the store: address discovery, balances,
-- transaction summaries, coins, the raw transactions the coins come from
-- and the pending transactions. The store is asked through the 'Store'
-- record of "Haskoin.Wallet.Store".
module Haskoin.Wallet.Sync where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Containers.ListUtils (nubOrd)
import Data.List (partition, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.PendingTx
import Haskoin.Wallet.RawTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (parseTxSignData)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

{- Address discovery -}

-- | The index after the last used address of a chain of the public key,
-- found by scanning windows of the given size from the given index: the
-- store is asked which addresses of a window have transactions, and a
-- window where none has ends the scan. The scan therefore stops only after
-- at least a window of unused addresses, up to twice that many depending
-- on where the last used address falls in its window.
scanChain :: (MonadIO m) => Ctx -> Store -> XPubKey -> SoftPath -> Natural -> Natural -> ExceptT String m Natural
scanChain ctx store pub chain window from = go from from
  where
    go next off = do
      let addrs = addrsDerivPage ctx chain (Page window off) pub
      bals <- fromStore $ storeBalances store (fst <$> addrs)
      let used = Set.fromList [bal.address | bal <- bals, bal.txs /= 0]
          usedIndices = [fromIntegral (pathIndex path) | (addr, path) <- addrs, addr `Set.member` used]
      if null usedIndices
        then return next
        else go (maximum usedIndices + 1) (off + window)

-- | The number of external and internal addresses of an account that have
-- been used, found by scanning both chains from their first address in
-- windows of the recovery gap
discoverAddrs :: (MonadIO m) => Ctx -> Config -> Store -> XPubKey -> ExceptT String m (Natural, Natural)
discoverAddrs ctx cfg store pub =
  (,) <$> scanChain ctx store pub extDeriv recoveryGap 0 <*> scanChain ctx store pub intDeriv recoveryGap 0
  where
    recoveryGap = configRecoveryGap cfg

-- | Generate the addresses of the account that were used without its
-- knowledge: the gap of addresses past the ones each chain has is asked
-- about, and a used address found there moves the chain up to and
-- including it. Someone paying the wallet from its public key, or another
-- wallet of the same seed, uses addresses this one has not generated yet.
-- Returns how many addresses were generated.
watchGap :: (MonadUnliftIO m) => Ctx -> Config -> Store -> DBAccountId -> ExceptT String (DB m) Natural
watchGap ctx cfg store accId = do
  acc <- getAccountById accId
  let pub = accountXPubKey ctx acc
      extend addrType = do
        let have = fromIntegral $ dBAccountCount addrType acc
        next <- scanChain ctx store pub (addrDeriv addrType) (configGap cfg) have
        discoverAccGenAddrs ctx cfg accId addrType (fromIntegral next)
        return (next - have)
  (+) <$> extend AddrExternal <*> extend AddrInternal

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
    syncResConflicted :: !Natural,
    -- | Addresses found used past the ones the account had, and generated
    syncResNewAddrs :: !Natural
  }
  deriving (Eq, Show)

-- | Bring an account up to date with the store.
--
-- An incremental sync looks only at the addresses whose balance changed
-- since the last sync, which is every address a new, confirmed, replaced
-- or dropped transaction touches. A full sync, asked for or forced by a
-- reorganisation of the chain, looks at every address and also marks dead
-- the summaries of transactions the store no longer lists. Every sync first
-- watches the gap of addresses past the ones the account has: a used
-- address found there is generated, with the ones before it, and synced
-- like the others.
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
  -- A full sync looks at everything anyway: only an incremental one asks
  -- whether the chain moved
  everything <- if full then return True else chainReorganised store net
  found <- watchGap ctx cfg store accId
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
      toFetch = nubOrd $ changed <> unconfirmed <> firstSeen
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
  refreshed <- refreshCoins net accId changedAddrs unspents
  -- The raw transactions the coins come from, which signing needs. A raw
  -- transaction never changes, so the ones already stored are not fetched
  -- again. The store must have every one asked for: it lists the coins.
  let parents = nubOrd $ (.outpoint.hash) <$> unspents
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
      reached p = isSigned p.pendingTxData.txSignDataTx && txHash p.pendingTxData.txSignDataTx `Set.member` seen
      (done, waiting) = partition reached pending
  lift $ forM_ done retirePendingTx
  let resend = [p.pendingTxData.txSignDataTx | p <- waiting, p.pendingTxOnline]
  forM_ resend $ fromStore . storePublish store
  let pub = accountXPubKey ctx newAcc
  conflicted <- filterM (inConflict net pub) waiting
  -- A raw transaction is kept while a coin comes from it. The parents of
  -- the coins that are gone and the transactions just retired are dropped,
  -- unless one of their outputs is still a coin of the wallet, change for
  -- instance. A full sync looks at every raw transaction instead, which
  -- also cleans up after the versions of hw that kept them all.
  goneParents <- liftEither $ mapM parentOf refreshed.deletedCoins
  let retired = [txHash p.pendingTxData.txSignDataTx | p <- done]
  _ <-
    if everything
      then dropOrphanRawTxs
      else lift $ dropRawTxsWithoutCoins (nubOrd $ goneParents <> retired)
  lift $ updateBest net (headerHash best.header) best.height
  return $
    SyncRes
      newAcc
      (headerHash best.header)
      (fromIntegral best.height)
      (fromIntegral $ length $ filter id $ snd <$> resTxInfo)
      (fromIntegral refreshed.refreshedCount)
      (fromIntegral $ length resend)
      (fromIntegral $ length conflicted)
      found
  where
    parentOf coin = (.hash) <$> coinOutpoint coin
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
          <> addrToString net bal.address
          <> " but listed "
          <> show (length refs)
    return (bal, refs)

{- The balances to update -}

-- | The balances that differ from the stored ones: the addresses to update
filterAddresses ::
  [Store.Balance] ->
  Map Address AddressBalance ->
  Either String [Store.Balance]
filterAddresses sBals aMap
  | sort ((.address) <$> sBals) /= sort (Map.keys aMap) =
      Left "Sync: addresses do not match"
  | otherwise = filterM changed sBals
  where
    -- Whether the store's balance differs from the stored one. The guard
    -- above makes every lookup succeed.
    changed s = do
      b <- maybeToEither "Sync: addresses do not match" $ Map.lookup s.address aMap
      return $
        s.txs /= addrBalanceTxs b
          || s.confirmed /= addrBalanceConfirmed b
          || s.unconfirmed /= addrBalanceUnconfirmed b
          || s.utxo /= addrBalanceCoins b
