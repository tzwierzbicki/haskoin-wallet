{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Synchronising an account with the store: the gap of addresses watched,
-- the balances, the transaction summaries, the coins and the raw
-- transactions they come from, and the pending transactions; and the
-- discovery of the used addresses of an account. The store is asked
-- through the 'Store' record of "Haskoin.Wallet.Store".
module Haskoin.Wallet.Sync where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Containers.ListUtils (nubOrd)
import Data.List (partition, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
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
      addrs <- liftIO $ chainAddresses ctx pub chain (Page window off)
      bals <- fromStore $ storeBalances store (fst <$> addrs)
      let used = Set.fromList [bal.address | bal <- bals, bal.txs /= 0]
          usedIndices = [fromIntegral i | (addr, i) <- addrs, addr `Set.member` used]
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
        extendChainTo ctx accId addrType next
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
-- Every sync first watches the gap of addresses past the ones the account
-- has: a used address found there is generated, with the ones before it,
-- and synced like the others. An incremental sync then looks only at the
-- addresses whose balance changed since the last sync, which is every
-- address a new, confirmed, replaced or dropped transaction touches. A
-- full sync, asked for or forced by a reorganisation of the chain, looks
-- at every address and also marks dead the summaries of transactions the
-- store no longer lists.
sync :: (MonadUnliftIO m) => Ctx -> Config -> Store -> Network -> DBAccountId -> Bool -> ExceptT String (DB m) SyncRes
sync ctx cfg store net accId full = do
  fromStore $ storeHealth store
  -- The best block is taken before anything else, so that a block found
  -- during the sync is caught up with on the next one rather than missed
  best <- fromStore $ storeBestBlock store
  -- A full sync looks at everything anyway: only an incremental one asks
  -- whether the chain moved
  everything <- if full then return True else chainReorganised store net
  found <- watchGap ctx cfg store accId
  -- The addresses of the account with their stored balances, and those the
  -- store reports changed
  (addrPaths, addrBals) <- allAddressesMap net accId
  storeBals <- fromStore $ storeBalances store (Map.keys addrBals)
  changedBals <- if everything then return storeBals else liftEither $ filterAddresses storeBals addrBals
  updateAddressBalances net changedBals
  acc <- lift $ updateAccountBalances accId
  -- Their transactions, and the summaries to store or to mark dead
  listings <- listTxs cfg store net changedBals
  known <- txInfoBlockRefs accId
  let toFetch = transactionsToFetch known addrBals listings
      listed = concatMap snd listings
  txs <- fromStore $ storeTransactions store toFetch
  updated <- lift $ forM (filter (not . (.deleted)) txs) $ \tx -> repsertSummary net accId tx.txid tx.block (storeSummary net addrPaths tx)
  lift $ forM_ (deadTransactions everything known toFetch txs listed) $ markTxInfoDead accId
  -- The coins, the pending transactions and the raw transactions
  refreshed <- syncCoins store net accId changedBals
  (resent, conflicted, retired) <- syncPending ctx store acc accId (Set.fromList $ (.txid) <$> listed)
  pruneRawTxs everything refreshed retired
  lift $ updateBest net (headerHash best.header) best.height
  return
    SyncRes
      { syncResAccount = acc,
        syncResBlockHash = headerHash best.header,
        syncResBlockHeight = fromIntegral best.height,
        syncResTxUpdates = fromIntegral $ length $ filter id updated,
        syncResCoinUpdates = fromIntegral refreshed.refreshedCount,
        syncResRebroadcast = resent,
        syncResConflicted = conflicted,
        syncResNewAddrs = found
      }

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

{- The balances -}

-- | The balances that differ from the stored ones: the addresses to update
filterAddresses :: [Store.Balance] -> Map Address AddressBalance -> Either String [Store.Balance]
filterAddresses sBals aMap
  | sort ((.address) <$> sBals) /= sort (Map.keys aMap) = Left "Sync: addresses do not match"
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

-- | Every transaction the store lists for each address that has any: as
-- many as it counts in the balance, which a listing cut short would not
-- match. The addresses are not listed together in one request, because the
-- store would merge a transaction listed under two of them into one and
-- the counts could no longer be checked; the listings are asked in
-- parallel instead, as many at a time as the configuration allows.
listTxs :: (MonadUnliftIO m) => Config -> Store -> Network -> [Store.Balance] -> ExceptT String (DB m) [(Store.Balance, [Store.TxRef])]
listTxs cfg store net bals = do
  let used = filter ((/= 0) . (.txs)) bals
  listings <- fromStore $ inParallel (configParallelRequests cfg) [storeAddressTxs store bal.address (fromIntegral bal.txs) | bal <- used]
  forM (zip used listings) $ \(bal, refs) -> do
    when (fromIntegral (length refs) /= bal.txs) $
      throwError $
        "The store counts " <> show bal.txs <> " transactions for " <> addrToString net bal.address <> " but listed " <> show (length refs)
    return (bal, refs)

{- The summaries -}

-- | What to fetch in full, from the summaries known with their blocks, the
-- stored balances and the listings of the changed addresses: the listed
-- transactions that are new or whose block changed; the unconfirmed
-- summaries, whose transactions may have been confirmed, replaced or
-- dropped since the last sync; and every transaction of an address that
-- has transactions for the first time. A summary is built with the
-- addresses the wallet knows at the time, so a transaction that also paid
-- an address generated later, say a batch payout from someone deriving the
-- public key, shows that output as somebody else's until it is rebuilt.
transactionsToFetch :: Map TxHash Store.BlockRef -> Map Address AddressBalance -> [(Store.Balance, [Store.TxRef])] -> [TxHash]
transactionsToFetch known addrBals listings = nubOrd $ changed <> unconfirmed <> firstSeen
  where
    changed = [r.txid | (_, refs) <- listings, r <- refs, Map.lookup r.txid known /= Just r.block]
    unconfirmed = [tid | (tid, ref) <- Map.toList known, not (Store.confirmed ref)]
    storedCount bal = maybe 0 addrBalanceTxs $ Map.lookup bal.address addrBals
    firstSeen = [r.txid | (bal, refs) <- listings, storedCount bal == 0, r <- refs]

-- | The known transactions that are dead: those fetched that the store
-- reports deleted or does not return, replaced, dropped from the mempool or
-- left out by a reorganisation; and, on a full sync, those no address lists
-- any more, since a reorganisation may remove a confirmed transaction
-- without a trace in the addresses it touched. Their summaries are marked
-- dead and kept, for the history, and are no longer known: a transaction
-- the store lists again later is fetched and comes back to life.
deadTransactions :: Bool -> Map TxHash Store.BlockRef -> [TxHash] -> [Store.Transaction] -> [Store.TxRef] -> [TxHash]
deadTransactions everything known fetched txs listed = nubOrd $ gone <> vanished
  where
    alive = Set.fromList [t.txid | t <- txs, not t.deleted]
    gone = [tid | tid <- fetched, tid `Set.notMember` alive, tid `Map.member` known]
    stillListed = Set.fromList $ (.txid) <$> listed
    vanished = [tid | everything, tid <- Map.keys known, tid `Set.notMember` stillListed]

{- The coins and their raw transactions -}

-- | The coins of the changed addresses, all of them, which the store must
-- list as many of as it counts, and the raw transactions they come from,
-- which signing needs. A raw transaction never changes, so the ones already
-- stored are not fetched again.
syncCoins :: (MonadUnliftIO m) => Store -> Network -> DBAccountId -> [Store.Balance] -> ExceptT String (DB m) Refreshed
syncCoins store net accId changedBals = do
  let changedAddrs = (.address) <$> changedBals
      counted = sum $ (.utxo) <$> changedBals
  unspents <- fromStore $ storeUnspents store changedAddrs
  when (fromIntegral (length unspents) /= counted) $
    throwError $
      "The store counts " <> show counted <> " coins for the addresses to update but listed " <> show (length unspents)
  refreshed <- refreshCoins net accId changedAddrs unspents
  missing <- lift $ missingRawTxs $ nubOrd $ (.outpoint.hash) <$> unspents
  rawTxs <- fromStore $ rawTransactions store missing
  lift $ forM_ rawTxs insertRawTx
  return refreshed

-- | Drop the raw transactions no coin needs any more. A raw transaction is
-- kept while a coin comes from it: the parents of the coins that are gone
-- and the transactions just retired are dropped, unless one of their
-- outputs is still a coin of the wallet, change for instance. A full sync
-- looks at every raw transaction instead, which also cleans up after the
-- versions of hw that kept them all.
pruneRawTxs :: (MonadUnliftIO m) => Bool -> Refreshed -> [TxHash] -> ExceptT String (DB m) ()
pruneRawTxs everything refreshed retired
  | everything = void dropOrphanRawTxs
  | otherwise = do
      goneParents <- liftEither $ mapM (fmap (.hash) . coinOutpoint) refreshed.deletedCoins
      void $ lift $ dropRawTxsWithoutCoins $ nubOrd $ goneParents <> retired

{- The pending transactions -}

-- | The pending transactions of the account after a sync, given the
-- transactions the store listed. A signed one the store lists has reached
-- the network and is retired. One the user sent that the store does not
-- list is sent again: nodes forget unconfirmed transactions, and the store
-- may never have accepted it; a signed one the user has not sent is left
-- alone. One whose coins are gone conflicts with the transaction that spent
-- them, and is counted for the user. Returns how many were sent again, how
-- many conflict, and the ids of those retired.
syncPending :: (MonadUnliftIO m) => Ctx -> Store -> DBAccount -> DBAccountId -> Set TxHash -> ExceptT String (DB m) (Natural, Natural, [TxHash])
syncPending ctx store acc accId seen = do
  held <- pendingTxs accId
  let (done, waiting) = partition reached held
  lift $ forM_ done retirePendingTx
  let resend = [txOf p | p <- waiting, p.pendingTxOnline]
  forM_ resend $ fromStore . storePublish store
  conflicted <- filterM (inConflict ctx acc) waiting
  return (fromIntegral (length resend), fromIntegral (length conflicted), txHash . txOf <$> done)
  where
    txOf p = p.pendingTxData.txSignDataTx
    reached p = isSigned (txOf p) && txHash (txOf p) `Set.member` seen
