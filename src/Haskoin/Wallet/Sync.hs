{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Synchronising an account with the store: the gap of addresses watched,
-- the balances, the transaction summaries, the coins and the raw
-- transactions they come from, and the pending transactions; and the
-- recovery of the accounts of a seed. The store is asked
-- through the 'Store' record of "Haskoin.Wallet.Store".
module Haskoin.Wallet.Sync where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Either (partitionEithers)
import Data.List (find, genericLength, partition, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import qualified Data.Set as Set
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Keys (AccountKind (..), ForeignKind, PublicSeed (..), accountDeriv, foreignAddress, foreignDeriv, keyAddress, networkForeignKinds, networkKinds)
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.RawTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

{- The used addresses past the ones the account has -}

-- | The question a scan asks when it reaches 'scanLimit', and a search
-- over the accounts of a seed when it reaches 'accountLimit': told what
-- the store reports, whether to look further. The command puts it to the
-- user on the terminal, and --yes does not answer it: a sync nobody
-- watches must not believe a store that lies. A no stops the scan or the
-- search, which has written nothing.
type ScanQuestion m = String -> ExceptT String m Bool

-- | The index after the last used address of a chain of the public key of
-- an account of the kind, found by scanning windows of the given size from
-- the given index: the store is asked which addresses of a window have
-- transactions, and a window where none has ends the scan. The scan
-- therefore stops only after at least a window of unused addresses, up to
-- twice that many depending on where the last used address falls in its
-- window. Where the chain is, for the question of 'scanLimit', is given
-- as the end of a sentence: "on the receiving chain of the account main".
scanChain :: (MonadIO m) => Ctx -> Store -> AccountKind -> XPubKey -> SoftPath -> Natural -> Natural -> String -> ScanQuestion m -> ExceptT String m Natural
scanChain ctx store kind = scanChainBy ctx store (Just . keyAddress kind ctx)

-- | 'scanChain', with the address of a key given by the function: for the
-- accounts hw does not hold
scanChainBy :: (MonadIO m) => Ctx -> Store -> (XPubKey -> Maybe Address) -> XPubKey -> SoftPath -> Natural -> Natural -> String -> ScanQuestion m -> ExceptT String m Natural
scanChainBy ctx store addressOf pub chain window from place ask = do
  used <- usedChainAddresses ctx store addressOf pub chain window from place ask
  return $ if null used then from else maximum (fst <$> used) + 1

-- | The used addresses of a chain of a public key from an index, each
-- with its index and its balance at the store, found by scanning windows
-- as 'scanChain' does: the scan behind it, which keeps what the store
-- said of the addresses. Every 'scanLimit' addresses the store reports
-- used in a row, the question is asked, and a no stops the scan.
usedChainAddresses :: (MonadIO m) => Ctx -> Store -> (XPubKey -> Maybe Address) -> XPubKey -> SoftPath -> Natural -> Natural -> String -> ScanQuestion m -> ExceptT String m [(Natural, Store.Balance)]
usedChainAddresses ctx store addressOf pub chain window from place ask = go from 0
  where
    -- The window at the offset, after the given number of addresses
    -- looked at since the scan began or last asked
    go off looked = do
      addrs <- liftIO $ chainAddressesBy addressOf ctx pub chain off window
      bals <- fromStore $ storeBalances store (fst <$> addrs)
      let balanceOf = Map.fromList [(bal.address, bal) | bal <- bals, bal.txs /= 0]
          used = [(fromIntegral i, bal) | (addr, i) <- addrs, Just bal <- [Map.lookup addr balanceOf]]
      if null used
        then return []
        else (used <>) <$> onward off (looked + window)
    -- The window after the one at the offset, once the scan has looked at
    -- the given number of addresses: at the limit, the question decides
    onward off looked
      | looked < scanLimit = go (off + window) looked
      | otherwise = do
          let found = reportedUsed (off + window - from) place
          goOn <- ask found
          unless goOn $ throwError (found <> " The scan stopped there.")
          go (off + window) 0

-- | What a scan reports when it reaches 'scanLimit': how far the store
-- reports used addresses, and where
reportedUsed :: Natural -> String -> String
reportedUsed far place = "The store reports used addresses as far as " <> show far <> " " <> place <> ", and more beyond."

-- | Generate the addresses of the account that were used without its
-- knowledge: the gap of addresses past the ones each chain has is asked
-- about, and a used address found there moves the chain up to and
-- including it. Someone paying the wallet from its public key, or another
-- wallet of the same seed, uses addresses this one has not generated yet.
-- Returns how many addresses were generated.
watchGap :: (MonadUnliftIO m) => Ctx -> Config -> Store -> DBAccountId -> ScanQuestion (DB m) -> ExceptT String (DB m) Natural
watchGap ctx cfg store accId ask = do
  acc <- getAccountById accId
  let pub = accountXPubKey ctx acc
      name = cs (dBAccountName acc)
      extend addrType = do
        let have = fromIntegral $ dBAccountCount addrType acc
            -- Where the scan is, for the question of the limit
            place
              | have == 0 = "on the " <> chainWord addrType <> " chain of the account " <> name
              | otherwise = "past the " <> show have <> " " <> chainWord addrType <> (if have == 1 then " address" else " addresses") <> " the account " <> name <> " has"
        next <- scanChain ctx store (accountKind acc) pub (addrDeriv addrType) (configGap cfg) have place ask
        extendChainTo ctx accId addrType next
        return (next - have)
  (+) <$> extend AddrExternal <*> extend AddrInternal

-- | The chain of an address type, as the questions name it
chainWord :: AddrType -> String
chainWord AddrExternal = "receiving"
chainWord AddrInternal = "change"

{- Recovering the accounts of a seed -}

-- | How many accounts a search finds paid before it asks whether to go
-- on: twenty. Few seeds have that many paid accounts of one kind, and
-- the owner of one that does answers yes. A store that lies reports an
-- account paid at every index, without end. With one used address in
-- each, no chain of theirs reaches 'scanLimit', so the search over the
-- accounts has a limit of its own.
accountLimit :: Natural
accountLimit = 20

-- | What a search finds at the index of an account
data Looked a
  = -- | Nothing to look at, which counts neither way: an index whose
    -- child key is invalid, or an account the wallet has already
    PassedOver
  | -- | An account that was never paid
    Unpaid
  | -- | An account that was paid, with what the search keeps of it
    Paid a

-- | The loop of a search over the accounts of a seed, as BIP44 describes
-- it: index after index from 0, until the given number of unpaid accounts
-- in a row. The last argument looks at the account of an index. The
-- result is what it kept of each paid account, in the order of the
-- indices.
--
-- After every 'accountLimit' accounts found paid, the question is asked.
-- It is told how many accounts the store reports, and the path of the
-- last one, which the second argument gives from its index. A no stops
-- the search with an error, so the command writes nothing.
searchAccounts :: (Monad m) => Natural -> (Natural -> HardPath) -> ScanQuestion m -> (Natural -> ExceptT String m (Looked a)) -> ExceptT String m [a]
searchAccounts accountGap pathOf ask look = go 0 0 0
  where
    -- The accounts from the index on, after the given number of unpaid
    -- accounts in a row, and of paid accounts since the search began
    go index unpaid found
      | unpaid >= accountGap = return []
      | otherwise = do
          looked <- look index
          case looked of
            PassedOver -> go (index + 1) unpaid found
            Unpaid -> go (index + 1) (unpaid + 1) found
            Paid account -> (account :) <$> onward index (found + 1)
    -- The accounts after the paid one at the index, which is the given
    -- number of them found: at every multiple of the limit, the question
    -- decides
    onward index found = do
      when (found `mod` accountLimit == 0) $ do
        let reported = reportedPaid found (pathOf index)
        goOn <- ask reported
        unless goOn $ throwError (reported <> " The search stopped there.")
      go (index + 1) 0 found

-- | What a search reports when it reaches 'accountLimit': how many
-- accounts the store reports paid, and the path of the last one
reportedPaid :: Natural -> HardPath -> String
reportedPaid found path =
  "The store reports payments to " <> show found <> " accounts of the seed, as far as the account " <> pathToStr path <> "."

-- | The accounts of a seed of a kind on a network that were paid, found as
-- BIP44 describes: account after account from index 0, the receiving
-- chain of each scanned for a used address within the gap. The search
-- ends after the given number of accounts in a row without one. The
-- standard stops at the first, but hw creates accounts on an offline
-- signer, where some never receive anything, so the gap of accounts is
-- the caller's. An account the wallet has already is passed over without
-- counting, and so is an index whose child key is invalid. After every
-- 'accountLimit' accounts found, the question is asked ('searchAccounts').
-- Returns the accounts found, with their keys.
recoverableAccounts :: (MonadUnliftIO m) => Ctx -> Config -> Store -> AccountKind -> Network -> PublicSeed -> Natural -> ScanQuestion (DB m) -> ExceptT String (DB m) [(Natural, XPubKey)]
recoverableAccounts ctx cfg store kind net seed accountGap ask =
  searchAccounts accountGap (accountDeriv kind net) ask $ \index -> do
    keyM <- liftIO $ seedAccountKey seed kind net index
    case keyM of
      Nothing -> return PassedOver
      Just key -> do
        held <- lift $ accountWithKey net ctx key
        case held of
          Just _ -> return PassedOver
          Nothing -> do
            paid <- (> 0) <$> scanChain ctx store kind key extDeriv (configGap cfg) 0 (seedAccountPlace extDeriv (accountDeriv kind net index)) ask
            return $ if paid then Paid (index, key) else Unpaid

-- | Recover the accounts of a seed on a network, of every kind the network
-- has, the BIP44 accounts first: those found paid are created, named after
-- the network, their index and their kind, and synced, and every account
-- of the seed records its fingerprint. The gap of accounts counts for each
-- kind apart. Returns the sync of each account created, in the order of
-- their kinds and indices.
recoverSeed :: (MonadUnliftIO m) => Ctx -> Config -> Store -> Network -> PublicSeed -> Natural -> ScanQuestion (DB m) -> ExceptT String (DB m) [SyncRes]
recoverSeed ctx cfg store net seed accountGap ask = do
  created <- fmap concat . forM (networkKinds net) $ \kind -> do
    found <- recoverableAccounts ctx cfg store kind net seed accountGap ask
    forM found $ \(index, key) -> insertAccount kind net ctx (recoveredName kind net index) key (Just (seedFingerprint seed))
  lift $ recordFingerprint ctx seed
  forM created $ \(accId, _) -> sync ctx cfg store net accId False ask

-- | Where a chain of an account of the seed is, for the question of
-- 'scanLimit': "on the receiving chain of the account /44'/0'/0' of the
-- seed"
seedAccountPlace :: SoftPath -> HardPath -> String
seedAccountPlace chain path =
  "on the " <> (if chain == intDeriv then "change" else "receiving") <> " chain of the account " <> pathToStr path <> " of the seed"

-- | The name of a recovered account: the network and the index, as in
-- btc0, for a BIP44 account, and with -segwit after them, as in
-- btc0-segwit, for a BIP84 one
recoveredName :: AccountKind -> Network -> Natural -> Text
recoveredName kind net index = cs net.name <> cs (show index) <> suffix
  where
    suffix :: Text
    suffix = case kind of
      Bip44 -> Text.empty
      Bip84 -> Text.pack "-segwit"

{- The funds of a seed at accounts hw does not hold -}

-- | What a seed holds at an account of a foreign kind: an account recover
-- found paid, which hw cannot spend from and does not create
data ForeignFunds = ForeignFunds
  { foreignKind :: !ForeignKind,
    foreignIndex :: !Natural,
    -- | The addresses of the account with transactions, on both chains
    foreignUsed :: !Natural,
    -- | What those addresses hold, confirmed or not, in satoshis
    foreignBalance :: !Word64
  }
  deriving (Eq, Show)

-- | The path of the account the funds are at, on the network
foreignPath :: Network -> ForeignFunds -> HardPath
foreignPath net funds = foreignDeriv (foreignKind funds) net (foreignIndex funds)

-- | The funds of a seed at the accounts of a foreign kind on a network,
-- found as 'recoverableAccounts' finds the accounts hw holds: account
-- after account from index 0, each paid when a receiving address within
-- the gap was used, the search over after the gap of accounts in a row
-- without one, and the question asked after every 'accountLimit'
-- accounts found. The change chain of an account found is scanned too,
-- so that the balance is the whole of it.
foreignFundsOfKind :: (MonadIO m) => Ctx -> Config -> Store -> ForeignKind -> Network -> PublicSeed -> Natural -> ScanQuestion m -> ExceptT String m [ForeignFunds]
foreignFundsOfKind ctx cfg store kind net seed accountGap ask =
  searchAccounts accountGap (foreignDeriv kind net) ask $ \index -> do
    keyM <- liftIO $ seedForeignKey seed kind net index
    case keyM of
      Nothing -> return PassedOver
      Just key -> do
        receiving <- usedOn index key extDeriv
        if null receiving
          then return Unpaid
          else do
            change <- usedOn index key intDeriv
            let bals = snd <$> receiving <> change
            return $ Paid (ForeignFunds kind index (genericLength bals) (sum [bal.confirmed + bal.unconfirmed | bal <- bals]))
  where
    usedOn index key chain =
      usedChainAddresses ctx store (foreignAddress kind net ctx) key chain (configGap cfg) 0 (seedAccountPlace chain (foreignDeriv kind net index)) ask

-- | The funds of a seed on a network at the accounts of every foreign kind
-- the network has, in the order of the kinds and of their indices
foreignFunds :: (MonadIO m) => Ctx -> Config -> Store -> Network -> PublicSeed -> Natural -> ScanQuestion m -> ExceptT String m [ForeignFunds]
foreignFunds ctx cfg store net seed accountGap ask =
  concat <$> forM (networkForeignKinds net) (\kind -> foreignFundsOfKind ctx cfg store kind net seed accountGap ask)

{- Synchronisation -}

data SyncRes = SyncRes
  { syncResAccount :: !DBAccount,
    -- | What the account can spend once synced ("Haskoin.Wallet.Database.Coin")
    syncResSpendable :: !Word64,
    syncResBlockHash :: !BlockHash,
    syncResBlockHeight :: !BlockHeight,
    syncResTxUpdates :: !Natural,
    syncResCoinUpdates :: !Natural,
    -- | Transactions the user sent that the store had lost, sent again
    syncResRebroadcast :: !Natural,
    -- | Pending transactions a coin of which was spent by another
    -- transaction, or is gone
    syncResConflicted :: !Natural,
    -- | Addresses found used past the ones the account had, and generated
    syncResNewAddrs :: !Natural
  }
  deriving (Eq, Show)

-- | Sync the accounts, each with the store of its network. The store of
-- each network is first asked whether it is up. The accounts of a network
-- whose store is down are not synced, and the others are. One store out of
-- service then does not hold back the accounts of the other networks.
-- Returns the syncs, and the accounts not synced with the failure of their
-- store, both in the order of the accounts.
syncAccounts :: (MonadUnliftIO m) => Ctx -> Config -> (Network -> Store) -> [(DBAccountId, DBAccount)] -> Bool -> ScanQuestion (DB m) -> ExceptT String (DB m) ([SyncRes], [(DBAccount, String)])
syncAccounts ctx cfg storeOn accs full ask = do
  failures <- liftIO $ forM networks $ \net -> (,) net.name <$> healthFailure (storeOn net)
  results <- forM accs $ \(accId, acc) -> do
    let net = accountNetwork acc
    case join (lookup net.name failures) of
      Just failure -> return $ Left (acc, failure)
      Nothing -> Right <$> sync ctx cfg (storeOn net) net accId full ask
  let (notSynced, synced) = partitionEithers results
  return (synced, notSynced)
  where
    -- Network has no Ord instance, so the networks are told apart by name
    networks = nubOrdOn (.name) (accountNetwork . snd <$> accs)
    healthFailure store = either Just (const Nothing) <$> runExceptT (storeHealth store)

-- | Bring an account up to date with the store.
--
-- Every sync first watches the gap of addresses past the ones the account
-- has: a used address found there is generated, with the ones before it,
-- and synced like the others. An incremental sync then looks only at the
-- addresses whose balance changed since the last sync, which is every
-- address a new, confirmed, replaced or dropped transaction touches. A
-- full sync, asked for or forced by a reorganisation of the chain, looks
-- at every address and also marks dead the summaries of transactions the
-- store no longer lists. The store must answer about the addresses and
-- the transactions it is asked about, and no others: a store that lies
-- gets no further than that, and 'scanLimit' bounds what it can say of
-- the addresses past the ones the account has.
sync :: (MonadUnliftIO m) => Ctx -> Config -> Store -> Network -> DBAccountId -> Bool -> ScanQuestion (DB m) -> ExceptT String (DB m) SyncRes
sync ctx cfg store net accId full ask = do
  fromStore $ storeHealth store
  -- The best block is taken before anything else, so that a block found
  -- during the sync is caught up with on the next one rather than missed
  best <- fromStore $ storeBestBlock store
  -- A full sync looks at everything anyway: only an incremental one asks
  -- whether the chain moved
  everything <- if full then return True else chainReorganised store net
  found <- watchGap ctx cfg store accId ask
  -- The addresses of the account with their stored balances, and those the
  -- store reports changed
  (addrPaths, addrBals) <- allAddressesMap net accId
  storeBals <- fromStore $ storeBalances store (Map.keys addrBals)
  liftEither $ answeredAbout (Map.keys addrBals) storeBals
  let changedBals = if everything then storeBals else changedBalances storeBals addrBals
  updateAddressBalances net accId changedBals
  acc <- lift $ updateAccountBalances accId
  -- Their transactions, and the summaries to store or to mark dead
  listings <- listTxs cfg store net changedBals
  known <- txInfoBlockRefs accId
  untimed <- untimedSummaries accId
  let toFetch = transactionsToFetch known untimed addrBals listings
      listed = concatMap snd listings
  txs <- fromStore $ storeTransactions store toFetch
  liftEither $ askedFor toFetch txs
  updated <- lift $ forM (filter (not . (.deleted)) txs) $ \tx ->
    repsertSummary net accId tx.txid tx.block (storeTime tx.timestamp) (storeSummary net addrPaths tx)
  lift $ forM_ (deadTransactions everything known toFetch txs listed) $ markTxInfoDead accId
  -- The coins, the pending transactions and the raw transactions
  refreshed <- syncCoins ctx store net accId changedBals
  (resent, conflicted, retired) <- syncPending store accId (Set.fromList $ (.txid) <$> listed)
  pruneRawTxs everything refreshed retired
  lift $ updateBest net (headerHash best.header) best.height best.header.timestamp
  -- After the best block, which matures the coins of a coinbase
  spendable <- spendableBalance net accId
  return
    SyncRes
      { syncResAccount = acc,
        syncResSpendable = spendable,
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
  bestM <- getBest net
  case bestM of
    Nothing -> return False
    Just best -> do
      blocks <- fromStore $ storeBlocksAtHeight store best.blockHeight
      let stillMain b = headerHash b.header == best.blockHash && b.main
      return $ not $ any stillMain blocks

{- The balances -}

-- | Refuse an answer about other addresses than the ones asked, or about
-- fewer. A store that lies could otherwise reach the rows of another
-- account, or plant coins at an address of nobody's; one that answers
-- about fewer has lost some.
answeredAbout :: [Address] -> [Store.Balance] -> Either String ()
answeredAbout asked bals
  | sort ((.address) <$> bals) /= sort asked = Left "The store answered about other addresses than the ones asked"
  | otherwise = Right ()

-- | The balances that differ from the stored ones: the addresses to
-- update. One of an address not stored counts as changed.
changedBalances :: [Store.Balance] -> Map Address AddressBalance -> [Store.Balance]
changedBalances sBals aMap = filter changed sBals
  where
    changed s = case Map.lookup s.address aMap of
      Nothing -> True
      Just b ->
        s.txs /= addrBalanceTxs b
          || s.confirmed /= addrBalanceConfirmed b
          || s.unconfirmed /= addrBalanceUnconfirmed b
          || s.utxo /= addrBalanceCoins b

-- | Refuse a transaction the store returned without being asked for it:
-- it would enter the history of the account
askedFor :: [TxHash] -> [Store.Transaction] -> Either String ()
askedFor asked txs =
  case [tx.txid | tx <- txs, tx.txid `Set.notMember` Set.fromList asked] of
    [] -> Right ()
    tid : _ -> Left ("The store returned a transaction it was not asked for: " <> cs (txHashToHex tid))

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
-- summaries without their time, the stored balances and the listings of
-- the changed addresses: the listed transactions that are new or whose
-- block changed; the unconfirmed summaries, whose transactions may have
-- been confirmed, replaced or dropped since the last sync; the summaries
-- without their time, stored before hw kept it, whatever their addresses;
-- and every transaction of an address that has transactions for the first
-- time. A summary is built with the addresses the wallet knows at the
-- time, so a transaction that also paid an address generated later, say a
-- batch payout from someone deriving the public key, shows that output as
-- somebody else's until it is rebuilt.
transactionsToFetch :: Map TxHash Store.BlockRef -> [TxHash] -> Map Address AddressBalance -> [(Store.Balance, [Store.TxRef])] -> [TxHash]
transactionsToFetch known untimed addrBals listings = nubOrd $ changed <> unconfirmed <> untimed <> firstSeen
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
syncCoins :: (MonadUnliftIO m) => Ctx -> Store -> Network -> DBAccountId -> [Store.Balance] -> ExceptT String (DB m) Refreshed
syncCoins ctx store net accId changedBals = do
  let changedAddrs = (.address) <$> changedBals
      counted = sum $ (.utxo) <$> changedBals
  unspents <- fromStore $ storeUnspents store changedAddrs
  when (fromIntegral (length unspents) /= counted) $
    throwError $
      "The store counts " <> show counted <> " coins for the addresses to update but listed " <> show (length unspents)
  refreshed <- refreshCoins net ctx accId changedAddrs unspents
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
-- the network and is retired, and so is one the network has under another
-- id ('recordStanding'). Each takes its label to the id the network has it
-- under. The store is asked whether the others can still spend their
-- coins, and those that cannot are counted for the user. One the user sent
-- that the store does not list is sent again, unless it is in conflict,
-- which the network would refuse: nodes forget unconfirmed transactions,
-- and the store may never have accepted it. A signed one the user has not
-- sent is left alone. Returns how many were sent again, how many conflict,
-- and the ids of those retired.
syncPending :: (MonadUnliftIO m) => Store -> DBAccountId -> Set TxHash -> ExceptT String (DB m) (Natural, Natural, [TxHash])
syncPending store accId seen = do
  held <- pendingTxs accId
  let (listed, waiting) = partition reached held
  (elsewhere, conflicted) <- recordStanding store waiting
  let done = [(p, txHash (txOf p)) | p <- listed] <> elsewhere
      settled = Set.fromList (pendingTxKey <$> (fst <$> elsewhere) <> conflicted)
      resend = [txOf p | p <- waiting, p.pendingTxOnline, pendingTxKey p `Set.notMember` settled]
  lift $ forM_ done $ uncurry retirePendingTx
  forM_ resend $ fromStore . storePublish store
  return (fromIntegral (length resend), fromIntegral (length conflicted), txHash . txOf . fst <$> done)
  where
    reached p = isSigned (txOf p) && txHash (txOf p) `Set.member` seen

-- | Ask the store where the pending transactions stand: about the
-- transactions their coins come from, and, when another transaction spends
-- one of those coins, about that transaction too. One whose coins a
-- transaction with its nosigHash spends is on the network under another
-- id, its signatures changed: re-encoded by a miner, or made by another
-- signer, and sent from elsewhere. The others get their conflict recorded
-- ('conflictedInputs'), true or false every time: a transaction whose
-- rival has left the network, dropped or undone by a reorganisation, can
-- be sent again. Without pending transactions the store is not asked, and
-- without a rival it is asked once. Returns those on the network under
-- another id, each with that id, and those in conflict.
recordStanding :: (MonadUnliftIO m) => Store -> [PendingTx] -> ExceptT String (DB m) ([(PendingTx, TxHash)], [PendingTx])
recordStanding _ [] = return ([], [])
recordStanding store held = do
  parents <- fromStore $ storeTransactions store $ nubOrd [input.outpoint.hash | p <- held, input <- (txOf p).inputs]
  let rivalsOf p = rivalSpenders parents (txOf p)
      rivals = nubOrd (concatMap rivalsOf held)
  rivalTxs <- if null rivals then return [] else fromStore (storeTransactions store rivals)
  let rivalNosigHashes = Map.fromList [(rival.txid, nosigHash (Store.transactionData rival)) | rival <- rivalTxs, not rival.deleted]
      -- The id of the rival that is this very transaction, signed otherwise
      anotherId p = find (\rival -> Map.lookup rival rivalNosigHashes == Just (nosigHash (txOf p))) (rivalsOf p)
      elsewhere = [(p, networkId) | p <- held, Just networkId <- [anotherId p]]
      others = [p | p <- held, isNothing (anotherId p)]
      -- A replacement spends the coins of the transactions it replaces, as
      -- long as those are not in a block
      unconfirmedRivals = Set.fromList [rival.txid | rival <- rivalTxs, not rival.deleted, not (Store.confirmed rival.block)]
      replacing p = Set.fromList p.pendingTxData.txSignDataReplaces `Set.intersection` unconfirmedRivals
  conflicted <- flip filterM others $ \p -> do
    let inConflict = not $ null $ conflictedInputs parents (replacing p) (txOf p)
    lift $ setPendingTxConflicted p inConflict
    return inConflict
  return (elsewhere, conflicted)

-- | The transaction of a pending transaction
txOf :: PendingTx -> Tx
txOf p = p.pendingTxData.txSignDataTx
