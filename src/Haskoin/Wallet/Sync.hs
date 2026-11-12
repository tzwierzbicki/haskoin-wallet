{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Synchronising the portfolios with the store, and recovering the
-- portfolios of a seed. The store is asked through the 'Store' record of
-- "Haskoin.Wallet.Store".
--
-- A sync works on one pool at a time: what a portfolio holds on one
-- network, the accounts of both kinds together ('sync'). It watches the
-- gap of addresses of each account, reads the balances, makes the
-- summaries of the transactions against the addresses of every account of
-- the pool, so that a summary is relative to the portfolio, and brings the
-- coins, the raw transactions they come from and the pending transactions
-- up to date.
--
-- sync and recover go over every network, the test networks included. A
-- network the host does not serve is left out and named, and a network
-- whose store is down is reported as a failure, while the others are
-- worked on ('syncPools', 'recoverSeed'). A store may also fail later, on
-- any question of a sync or a search. The work on its network ends there,
-- and what that work wrote is undone, while the other networks go on
-- ('storeFailureApart', 'syncEach'). A failure that is not the store's,
-- such as the user who answers no to a question, still stops the command.
module Haskoin.Wallet.Sync where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find, genericLength, nub, partition, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.RawTx
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (AccountKind (..), ForeignKind, PublicSeed (..), accountDeriv, foreignAddress, foreignDeriv, keyAddress, kindName, networkForeignKinds, seedPortfolioKeys)
import Haskoin.Wallet.Store
import Haskoin.Wallet.SyncStatus
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

-- | Who watches a sync or a search: the user on the terminal, or a test
data Watcher m = Watcher
  { -- | What decides at a limit
    question :: ScanQuestion m,
    -- | Where the steps go, for the line of "Haskoin.Wallet.SyncStatus"
    status :: Status
  }

-- | The index after the last used address of a chain of the public key of
-- an account of the kind, found by scanning windows of the given size from
-- the given index: the store is asked which addresses of a window have
-- transactions, and a window where none has ends the scan. The scan
-- therefore stops only after at least a window of unused addresses, up to
-- twice that many depending on where the last used address falls in its
-- window. Where the chain is, for the question of 'scanLimit', is given
-- as the end of a sentence: "on the receiving chain of the account main".
scanChain :: (MonadIO m) => Ctx -> Store -> AccountKind -> XPubKey -> SoftPath -> Natural -> Natural -> String -> Watcher m -> ExceptT String m Natural
scanChain ctx store kind = scanChainBy ctx store (Just . keyAddress kind ctx)

-- | 'scanChain', with the address of a key given by the function: for the
-- accounts hw does not hold
scanChainBy :: (MonadIO m) => Ctx -> Store -> (XPubKey -> Maybe Address) -> XPubKey -> SoftPath -> Natural -> Natural -> String -> Watcher m -> ExceptT String m Natural
scanChainBy ctx store addressOf pub chain window from place watch = do
  used <- usedChainAddresses ctx store addressOf pub chain window from place watch
  return $ if null used then from else maximum (fst <$> used) + 1

-- | The used addresses of a chain of a public key from an index, each
-- with its index and its balance at the store, found by scanning windows
-- as 'scanChain' does: the scan behind it, which keeps what the store
-- said of the addresses. Every 'scanLimit' addresses the store reports
-- used in a row, the question is asked, and a no stops the scan.
usedChainAddresses :: (MonadIO m) => Ctx -> Store -> (XPubKey -> Maybe Address) -> XPubKey -> SoftPath -> Natural -> Natural -> String -> Watcher m -> ExceptT String m [(Natural, Store.Balance)]
usedChainAddresses ctx store addressOf pub chain window from place watch = go from 0
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
          goOn <- watch.question found
          unless goOn $ throwError (found <> " The scan stopped there.")
          go (off + window) 0

-- | What a scan reports when it reaches 'scanLimit': how far the store
-- reports used addresses, and where
reportedUsed :: Natural -> String -> String
reportedUsed far place = "The store reports used addresses as far as " <> show far <> " " <> place <> ", and more beyond."

-- | Generate the addresses of an account that were used without its
-- knowledge: the gap of addresses past the ones each chain has is asked
-- about, and a used address found there moves the chain up to and
-- including it. Someone paying the wallet from its public key, or another
-- wallet of the same seed, uses addresses this one has not generated yet.
-- The account is named for the question of the limit, as in "the bip84
-- account of portfolio0 on btc". Returns how many addresses were
-- generated.
watchGap :: (MonadUnliftIO m) => Ctx -> Config -> Store -> String -> DBAccountId -> Watcher (DB m) -> ExceptT String (DB m) Natural
watchGap ctx cfg store name accId watch = do
  acc <- getAccountById accId
  let pub = accountXPubKey ctx acc
      extend addrType = do
        let have = fromIntegral $ dBAccountCount addrType acc
            -- Where the scan is, for the question of the limit
            place
              | have == 0 = "on the " <> chainWord addrType <> " chain of " <> name
              | otherwise = "past the " <> show have <> " " <> chainWord addrType <> (if have == 1 then " address " else " addresses ") <> name <> " has"
        next <- scanChain ctx store (accountKind acc) pub (addrDeriv addrType) (configGap cfg) have place watch
        extendChainTo ctx accId addrType next
        return (next - have)
  (+) <$> extend AddrExternal <*> extend AddrInternal

-- | The chain of an address type, as the questions name it
chainWord :: AddrType -> String
chainWord AddrExternal = "receiving"
chainWord AddrInternal = "change"

{- Recovering the portfolios of a seed -}

-- | How many portfolios, or accounts of a kind hw does not hold, a search
-- finds paid before it asks whether to go on: twenty. Few seeds have that
-- many, and the owner of one that does answers yes. A store that lies
-- reports a payment at every index, without end. With one used address
-- in each, no chain reaches 'scanLimit', so the search over the indexes
-- has a limit of its own.
accountLimit :: Natural
accountLimit = 20

-- | What a search finds at an index
data Looked a
  = -- | Nothing to look at, which counts neither way: an index whose
    -- child key is invalid, or a portfolio the wallet has whole already
    PassedOver
  | -- | An index that was never paid
    Unpaid
  | -- | An index that was paid, with what the search keeps of it
    Paid a

-- | The loop of a search over the indexes of a seed, as BIP44 describes
-- it: index after index from 0, until the given number of unpaid ones in
-- a row. The last argument looks at an index. The result is what it kept
-- of each paid index, in their order.
--
-- After every 'accountLimit' indexes found paid, the question is asked.
-- It is told what the store reports so far, which the third argument
-- words from how many were found and the index of the last one. A no
-- stops the search with an error, so the command writes nothing. The
-- second argument tells the status line each index the search looks at.
searchIndexes :: (MonadIO m) => Natural -> (Natural -> Natural -> IO ()) -> (Natural -> Natural -> String) -> Watcher m -> (Natural -> ExceptT String m (Looked a)) -> ExceptT String m [a]
searchIndexes gap tell reported watch look = go 0 0 0
  where
    -- The indexes from the given one on, after the given number of unpaid
    -- ones in a row, and of paid ones since the search began
    go index unpaid found
      | unpaid >= gap = return []
      | otherwise = do
          liftIO $ tell index found
          looked <- look index
          case looked of
            PassedOver -> go (index + 1) unpaid found
            Unpaid -> go (index + 1) (unpaid + 1) found
            Paid kept -> (kept :) <$> onward index (found + 1)
    -- The indexes after the paid one given, which is the given number of
    -- them found: at every multiple of the limit, the question decides
    onward index found = do
      when (found `mod` accountLimit == 0) $ do
        let said = reported found index
        goOn <- watch.question said
        unless goOn $ throwError (said <> " The search stopped there.")
      go (index + 1) 0 found

-- | The loop of a search over the accounts of a seed of one kind on a
-- network ('searchIndexes'), which the funds at the accounts hw does not
-- hold are found with. The status line tells each account the search
-- looks at, with its network: a path alone does not tell the networks
-- apart, since m\/44'\/0' is an account of Bitcoin on both.
searchAccounts :: (MonadIO m) => Natural -> Network -> (Natural -> HardPath) -> Watcher m -> (Natural -> ExceptT String m (Looked a)) -> ExceptT String m [a]
searchAccounts accountGap net pathOf watch =
  searchIndexes
    accountGap
    (\index found -> tellSearch watch.status net (pathOf index) found)
    (\found index -> reportedPaid found net (pathOf index))
    watch

-- | What a search reports when it reaches 'accountLimit': how many
-- accounts the store reports paid, on which network, and the path of the
-- last one
reportedPaid :: Natural -> Network -> HardPath -> String
reportedPaid found net path =
  "The store reports payments to " <> show found <> " accounts of the seed on " <> net.name <> ", as far as the account " <> pathToStr path <> "."

-- | What the search of recover reports when it reaches 'accountLimit':
-- how many portfolios the store reports paid, and the last one
reportedPortfolios :: Natural -> Natural -> String
reportedPortfolios found index =
  "The store reports payments to " <> show found <> " portfolios of the seed, as far as " <> cs (portfolioName index) <> "."

-- | Whether a portfolio of a seed was ever paid, on any of the networks
-- given, in any of its accounts there: the receiving chain of each is
-- scanned for a used address within the gap, and the first one found
-- settles it. The keys are those of the portfolio, by network and kind.
--
-- A network whose store fails during a scan is not scanned any further,
-- and the others are: it comes back with the failure of its store
-- ('storeFailureApart'). A portfolio paid there alone is then not found.
portfolioPaid :: (MonadIO m) => Ctx -> Config -> (Network -> Store) -> [Network] -> Natural -> [(Network, AccountKind, XPubKey)] -> Watcher m -> ExceptT String m (Bool, [(Network, String)])
portfolioPaid ctx cfg storeOn networks index keys watch = go [(net, kind, key) | (net, kind, key) <- keys, net `elem` networks] []
  where
    go [] failed = return (False, reverse failed)
    go ((net, kind, key) : others) failed = do
      let place = seedAccountPlace net extDeriv (accountDeriv kind net index)
      scanned <- storeFailureApart (storeOn net) $ \store -> scanChain ctx store kind key extDeriv (configGap cfg) 0 place watch
      case scanned of
        Left failure -> go [other | other@(otherNet, _, _) <- others, otherNet /= net] ((net, failure) : failed)
        Right used
          | used > 0 -> return (True, reverse failed)
          | otherwise -> go others failed

-- | What recover did with a seed
data Recovered = Recovered
  { -- | The wallet of the seed, when it has one now
    recoveredWallet :: !(Maybe Natural),
    -- | The indexes of the portfolios recovered: those found paid that
    -- the wallet did not have, which were created. The portfolio 0 a new
    -- wallet gets though it was never paid is not among them.
    recoveredPortfolios :: ![Natural],
    -- | The sync of each pool of the wallet on the networks searched
    recoveredSyncs :: ![SyncRes],
    -- | The networks whose store failed during the search or the syncs,
    -- each with the failure of its store: the search there ended with
    -- it, and the syncs there were undone. The networks given that are
    -- not among them were searched in full.
    recoveredDown :: ![(Network, String)]
  }
  deriving (Eq, Show)

-- | Recover the portfolios of a seed: those that were ever paid, on any of
-- the networks given, are created whole, and synced on those networks.
--
-- The search goes portfolio after portfolio from index 0, and ends after
-- the given number of portfolios in a row where nothing was found. A
-- portfolio is paid when any of its accounts was, of either kind, on any
-- network searched: it is then created with every account, on every
-- network, as newportfolio creates it. A portfolio the wallet has
-- already, whole as every portfolio is, is passed over without counting,
-- and so is an index whose child key is invalid. After every
-- 'accountLimit' portfolios found, the question is asked
-- ('searchIndexes').
--
-- The wallet of the seed is the one the database has with its
-- fingerprint ('seedWallet'), or a new one when a portfolio is found. A
-- new wallet gets its portfolio 0 whether that one was paid or not: a
-- wallet always has portfolio 0. A seed with no wallet and no paid
-- portfolio leaves the database as it was.
--
-- A network that is not searched, because its store is down or the host
-- does not serve it, is not among those given: a portfolio that was paid
-- on that network alone is then not found. A network whose store fails
-- during the search is searched no further, and the search goes on with
-- the others. It is not synced, and the result names it with the failure
-- of its store ('recoveredDown'). So is a network whose store fails
-- during the syncs, whose syncs are undone ('syncEach').
recoverSeed :: (MonadUnliftIO m) => Ctx -> Config -> (Network -> Store) -> [Network] -> PublicSeed -> Natural -> Watcher (DB m) -> ExceptT String (DB m) Recovered
recoverSeed ctx cfg storeOn networks seed portfolioGap watch = do
  -- The networks whose store failed so far in the search, with the failure
  failedRef <- liftIO $ newIORef []
  existing <- seedWallet ctx seed
  held <- case existing of
    Nothing -> return []
    Just wallet -> lift $ walletPortfolios wallet
  let heldIndexes = portfolioIndex . (.portfolioRow) <$> held
  found <- searchIndexes portfolioGap (tellPortfolioSearch watch.status) reportedPortfolios watch $ \index ->
    if index `elem` heldIndexes
      then return PassedOver
      else do
        keysM <- liftIO $ seedPortfolioKeys seed index
        case keysM of
          Nothing -> return PassedOver
          Just keys -> do
            failedSoFar <- liftIO $ readIORef failedRef
            let up = [net | net <- networks, net `notElem` (fst <$> failedSoFar)]
            (paid, failed) <- portfolioPaid ctx cfg storeOn up index keys watch
            liftIO $ modifyIORef' failedRef (<> failed)
            return $ if paid then Paid index else Unpaid
  failedInSearch <- liftIO $ readIORef failedRef
  let searchedUp = [net | net <- networks, net `notElem` (fst <$> failedInSearch)]
  if null found && isNothing existing
    then return (Recovered Nothing [] [] failedInSearch)
    else do
      wallet <- maybe (lift (insertWallet (seedFingerprint seed))) return existing
      -- The portfolios found that the wallet did not have. A seed whose
      -- portfolio 0 alone was paid has recovered that portfolio, though a
      -- new wallet would get it anyway.
      let created = [index | index <- found, index `notElem` heldIndexes]
          -- A new wallet gets its portfolio 0, paid or not
          creating = nub ([0 | isNothing existing] <> created)
      forM_ creating $ createPortfolio ctx seed wallet
      portfolios <- lift $ walletPortfolios wallet
      let pools = [portfolioPool portfolio net | portfolio <- portfolios, net <- searchedUp]
      (syncs, failedInSyncs) <- syncEach ctx cfg storeOn pools False watch
      return
        Recovered
          { recoveredWallet = Just (walletIndex (snd wallet)),
            recoveredPortfolios = created,
            recoveredSyncs = syncs,
            recoveredDown = failedInSearch <> failedInSyncs
          }

-- | Where a chain of an account of the seed is, for the question of
-- 'scanLimit': "on the receiving chain of the account /44'/0'/0' of the
-- seed on btc". The network is named because the path does not tell it:
-- m/44'/0' is searched on Bitcoin Cash too.
seedAccountPlace :: Network -> SoftPath -> HardPath -> String
seedAccountPlace net chain path =
  "on the " <> (if chain == intDeriv then "change" else "receiving") <> " chain of the account " <> pathToStr path <> " of the seed on " <> net.name

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
foreignFundsOfKind :: (MonadIO m) => Ctx -> Config -> Store -> ForeignKind -> Network -> PublicSeed -> Natural -> Watcher m -> ExceptT String m [ForeignFunds]
foreignFundsOfKind ctx cfg store kind net seed accountGap watch =
  searchAccounts accountGap net (foreignDeriv kind net) watch $ \index -> do
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
      usedChainAddresses ctx store (foreignAddress kind net ctx) key chain (configGap cfg) 0 (seedAccountPlace net chain (foreignDeriv kind net index)) watch

-- | The funds of a seed on a network at the accounts of every foreign kind
-- the network has, in the order of the kinds and of their indices
foreignFunds :: (MonadIO m) => Ctx -> Config -> Store -> Network -> PublicSeed -> Natural -> Watcher m -> ExceptT String m [ForeignFunds]
foreignFunds ctx cfg store net seed accountGap watch =
  concat <$> forM (networkForeignKinds net) (\kind -> foreignFundsOfKind ctx cfg store kind net seed accountGap watch)

{- Synchronisation -}

-- | What the sync of a pool did, and what the pool holds once synced
data SyncRes = SyncRes
  { -- | The portfolio and the network synced
    syncResPlace :: !Place,
    -- | What the pool holds, the balances of its accounts added up
    syncResConfirmed :: !Word64,
    syncResUnconfirmed :: !Word64,
    -- | How many coins it holds
    syncResCoins :: !Word64,
    -- | What it can spend once synced ("Haskoin.Wallet.Database.Coin")
    syncResSpendable :: !Word64,
    syncResBlockHash :: !BlockHash,
    syncResBlockHeight :: !BlockHeight,
    -- | How many summaries the sync stored, wrote again or marked dead. A
    -- transaction marked dead counts too, since txs then shows it dead.
    syncResTxUpdates :: !Natural,
    -- | How many coins it stored, wrote again or deleted
    syncResCoinUpdates :: !Natural,
    -- | Transactions the user sent that the store had lost, sent again
    syncResRebroadcast :: !Natural,
    -- | Pending transactions a coin of which was spent by another
    -- transaction, or is gone
    syncResConflicted :: !Natural,
    -- | Addresses found used past the ones the accounts had, and generated
    syncResNewAddrs :: !Natural
  }
  deriving (Eq, Show)

-- | What a sync over several networks did: the sync of each pool synced,
-- the networks the host does not serve, and the networks whose store is
-- down, each with the failure of its store
data Synced = Synced
  { syncedPools :: ![SyncRes],
    syncedNotServed :: ![Network],
    syncedDown :: ![(Network, String)]
  }
  deriving (Eq, Show)

-- | Sync the pools, each with the store of its network. The store of each
-- network is first asked whether it is up, which the line tells. Once the
-- host could not be reached, the stores of the networks after are not
-- asked, and are down ('servedNetworks'). The pools of a network the
-- host does not serve, or whose store is down, are not synced, and the
-- others are: one store out of service does not hold back the pools of
-- the other networks. A command that syncs every network cannot do
-- without this, since no host serves them all. A store that fails later,
-- during the sync of a pool, leaves its network out from there: the sync
-- of that pool is undone, the other pools of the network are not synced,
-- and the syncs of the other networks stay ('syncEach'). Returns the
-- syncs, in the order of the pools, and the networks left out, by why: a
-- network whose store failed is named with its failure, after those that
-- failed the first question. When no network can be synced at all, the
-- sync fails ('requireSomeNetwork').
syncPools :: (MonadUnliftIO m) => Ctx -> Config -> (Network -> Store) -> [Pool] -> Bool -> Watcher (DB m) -> ExceptT String (DB m) Synced
syncPools ctx cfg storeOn pools full watch = do
  served <- liftIO $ servedNetworks (checkingHealth (watch.status "sync") storeOn) networks
  unless (null pools) $ liftEither $ requireSomeNetwork (configHost cfg) served
  (results, failed) <- syncEach ctx cfg storeOn [pool | pool <- pools, pool.poolNetwork `elem` servedUp served] full watch
  return (Synced results (servedNot served) (servedDown served <> failed))
  where
    -- Network has no Ord instance, so the networks are told apart by name
    networks = nubOrdOn (.name) ((.poolNetwork) <$> pools)

-- | Sync the pools in their order, each with the store of its network, and
-- each in a savepoint of its own ('undoneIfRefused'). When the store of a
-- network fails during the sync of a pool ('storeFailureApart'), the
-- sync of that pool is undone alone, and the other pools of the network
-- are not synced: the store is down. The syncs of the pools before stay,
-- and the other networks go on. Any other failure fails the whole sync,
-- as the user who answers no to a question means it to. Returns the
-- syncs, and each network whose store failed with the failure, in the
-- order they failed.
syncEach :: (MonadUnliftIO m) => Ctx -> Config -> (Network -> Store) -> [Pool] -> Bool -> Watcher (DB m) -> ExceptT String (DB m) ([SyncRes], [(Network, String)])
syncEach ctx cfg storeOn pools full watch = go pools [] []
  where
    go [] done failed = return (reverse done, reverse failed)
    go (pool : rest) done failed
      | pool.poolNetwork `elem` (fst <$> failed) = go rest done failed
      | otherwise = do
          outcome <- storeFailureApart (storeOn pool.poolNetwork) $ \store ->
            undoneIfRefused (sync ctx cfg store pool full watch)
          case outcome of
            Right result -> go rest (result : done) failed
            Left failure -> go rest done ((pool.poolNetwork, failure) : failed)

-- | The label of a pool on the status line: its portfolio and its
-- network, as "portfolio0 btc". The wallet is named when it is not
-- wallet0, as "portfolio0 of wallet1 btc", as the lines of the answers
-- name it: a sync goes over the portfolios of every wallet.
poolLabel :: Place -> Text
poolLabel place = portfolioOfPlace place <> " " <> cs (placeNetwork place).name

-- | An account of a pool as the question of a scan names it: "the bip84
-- account of portfolio0 on btc", the wallet named as 'poolLabel' names it
accountPlaceName :: Place -> DBAccount -> String
accountPlaceName place acc =
  "the " <> cs (kindName (accountKind acc)) <> " account of " <> cs (portfolioOfPlace place) <> " on " <> (placeNetwork place).name

-- | The portfolio of a place as a label names it: "portfolio0", or
-- "portfolio0 of wallet1" for a wallet other than wallet0
portfolioOfPlace :: Place -> Text
portfolioOfPlace place =
  portfolioName (refPortfolio ref) <> (if refWallet ref == 0 then "" else " of " <> walletName (refWallet ref))
  where
    ref = placePortfolio place

-- | Bring a pool up to date with the store: the accounts of a portfolio
-- on one network, together.
--
-- Every sync first watches the gap of addresses past the ones each
-- account has: a used address found there is generated, with the ones
-- before it, and synced like the others. An incremental sync then looks
-- only at the addresses whose balance changed since the last sync, and at
-- those its transactions in the mempool touch ('balancesToLookAt'). A new,
-- confirmed or dropped transaction changes the balance of every address
-- it touches. A replacement may leave one as it was, so the sync looks at
-- the addresses of every transaction that may still be replaced.
--
-- The summary of a transaction is made against the addresses of every
-- account of the pool. A payment from a BIP44 address of the portfolio to
-- a BIP84 one is then one internal transaction, and a payment that spends
-- coins of both types is one debit.
--
-- When the chain may have moved under the pool ('chainMayHaveMoved'),
-- the sync looks at every address, and also marks dead the summaries of
-- transactions the store no longer lists. It still fetches only the
-- transactions that moved: a reorganisation cannot change what a
-- transaction holds, and a test network has them often.
--
-- A full sync, which the user asks for, is the way out of whatever went
-- wrong in the database. It goes by nothing the store said before: it
-- fetches every transaction listed and writes it again, it checks the raw
-- transactions the coins come from and replaces those that are damaged, and
-- a stored row that does not read cannot stop it. Only what the store
-- cannot tell stays: labels, the coins the
-- user locked, pending transactions, and the dead summaries kept for the
-- history, except those that do not read, which nothing can write again.
--
-- The store must answer about the addresses and the transactions it is
-- asked about, and no others: a store that lies gets no further than that,
-- and 'scanLimit' bounds what it can say of the addresses past the ones
-- the accounts have. Most steps reach the status line through the store
-- ("Haskoin.Wallet.SyncStatus").
sync :: (MonadUnliftIO m) => Ctx -> Config -> Store -> Pool -> Bool -> Watcher (DB m) -> ExceptT String (DB m) SyncRes
sync ctx cfg plainStore pool full watch = do
  let net = pool.poolNetwork
  place <- poolPlace pool
  accounts <- lift $ poolAccounts pool
  -- The questions to these stores tell the steps of the sync
  let tell = watch.status (poolLabel place)
      store = tellingStore tell plainStore
  scanStore <- liftIO $ scanningStore tell plainStore
  fromStore $ checkHealth store
  -- The best block is taken before anything else, so that a block found
  -- during the sync is caught up with on the next one rather than missed
  best <- fromStore $ storeBestBlock store
  -- A full sync looks at everything anyway: only an incremental one asks
  -- whether the chain moved
  everything <- if full then return True else chainMayHaveMoved store pool
  found <- sum <$> forM accounts (\(accId, acc) -> watchGap ctx cfg scanStore (accountPlaceName place acc) accId watch)
  -- The addresses of the pool with their stored balances, and those to
  -- look at: every one, or those the store reports changed and those a
  -- transaction in the mempool touches
  (addrPaths, addrBals) <- poolAddressesMap pool
  storeBals <- fromStore $ storeBalances store (Map.keys addrBals)
  refusedAnswer store $ answeredAbout (Map.keys addrBals) storeBals
  lookedAt <-
    if everything
      then return storeBals
      else (\inMempool -> balancesToLookAt inMempool storeBals addrBals) <$> mempoolAddresses pool
  -- Each account takes the balances of its own addresses
  synced <- forM accounts $ \(accId, _) -> do
    updateAddressBalances net accId lookedAt
    lift $ updateAccountBalances accId
  -- Their transactions, and the summaries to store or to mark dead
  listings <- listTxs cfg store net tell lookedAt
  let listed = concatMap snd listings
  (toFetch, live) <-
    if full
      then (,) (nubOrd ((.txid) <$> listed)) <$> liveTxids pool
      else do
        known <- txInfoBlockRefs pool
        return (transactionsToFetch known addrBals listings, Map.keysSet known)
  txs <- fromStore $ storeTransactions store toFetch
  refusedAnswer store $ askedFor toFetch txs
  refusedAnswer store $ mapM_ weighed txs
  updated <- lift $ forM (filter (not . (.deleted)) txs) $ \tx ->
    repsertSummary pool tx.txid tx.block (storeTime tx.timestamp) (storeSummary net addrPaths tx)
  let dead = deadTransactions everything live toFetch txs listed
  lift $ forM_ dead $ markTxInfoDead pool
  when full $ lift $ dropUnreadableDead pool
  -- The coins, the pending transactions and the raw transactions
  refreshed <- syncCoins ctx store pool full lookedAt
  (resent, conflicted, retired) <- syncPending store pool (Set.fromList $ (.txid) <$> listed)
  pruneRawTxs full refreshed retired
  lift $ do
    updateBest net (headerHash best.header) best.height best.header.timestamp
    updateSyncedBlock pool (headerHash best.header) best.height
  -- After the best block, which matures the coins of a coinbase
  spendable <- spendableBalance pool
  return
    SyncRes
      { syncResPlace = place,
        syncResConfirmed = sum (dBAccountBalanceConfirmed <$> synced),
        syncResUnconfirmed = sum (dBAccountBalanceUnconfirmed <$> synced),
        syncResCoins = sum (dBAccountBalanceCoins <$> synced),
        syncResSpendable = spendable,
        syncResBlockHash = headerHash best.header,
        syncResBlockHeight = fromIntegral best.height,
        syncResTxUpdates = fromIntegral $ length (filter id updated) + length dead,
        syncResCoinUpdates = fromIntegral refreshed.refreshedCount,
        syncResRebroadcast = resent,
        syncResConflicted = conflicted,
        syncResNewAddrs = found
      }

-- | Whether the chain may have moved under the pool since its last
-- sync: the block that sync reached is no longer in the main chain at its
-- height. Balances alone do not show a reorganisation that leaves them
-- unchanged while it moves transactions to other blocks, so the sync then
-- looks at every address. Each pool asks about its own block: the first
-- one to see a reorganisation moves the best block of the network past it.
-- A pool with no block may have moved too. It was never synced, which
-- makes every address new anyway, or it comes from a version of the
-- database that kept no block.
chainMayHaveMoved :: (MonadUnliftIO m) => Store -> Pool -> ExceptT String (DB m) Bool
chainMayHaveMoved store pool = do
  syncedM <- syncedBlock pool
  case syncedM of
    Nothing -> return True
    Just synced -> do
      blocks <- fromStore $ storeBlocksAtHeight store synced.syncedHeight
      let stillMain b = headerHash b.header == synced.syncedHash && b.main
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

-- | The balances of the addresses an incremental sync looks at. These are
-- the balances that differ from the stored ones. They are also the
-- balances of the addresses given, whatever those balances are. The
-- addresses given are those the transactions in the mempool touch
-- ('mempoolAddresses'). A balance of an address not stored counts as
-- changed.
--
-- The stored balance alone misses a replacement that leaves it as it was.
-- Such a replacement pays an address what the transaction it replaces
-- paid, or spends the same coin and nothing else from it. A fee bump of a
-- payment between two portfolios does this to the address paid. The sync
-- would mark the transaction replaced dead, and never see the
-- replacement. Looking at every address would catch it too, but would ask
-- the store about thousands of addresses on every sync. Only an
-- unconfirmed transaction can be replaced without a reorganisation, and a
-- reorganisation makes the sync look at every address anyway.
balancesToLookAt :: Set Address -> [Store.Balance] -> Map Address AddressBalance -> [Store.Balance]
balancesToLookAt inMempool sBals aMap = filter lookAt sBals
  where
    lookAt s = s.address `Set.member` inMempool || changed s
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

-- | Refuse a transaction the store reports of weight 0: no transaction
-- weighs nothing, and the fee rate of its summary would divide by it
weighed :: Store.Transaction -> Either String ()
weighed tx
  | tx.weight == 0 = Left ("The store reports a weight of 0 for the transaction " <> cs (txHashToHex tx.txid))
  | otherwise = Right ()

-- | Every transaction the store lists for each address that has any: as
-- many as it counts in the balance, which a listing cut short would not
-- match. The addresses are not listed together in one request, because the
-- store would merge a transaction listed under two of them into one and
-- the counts could no longer be checked; the listings are asked in
-- parallel instead, as many at a time as the configuration allows.
listTxs :: (MonadUnliftIO m) => Config -> Store -> Network -> Tell -> [Store.Balance] -> ExceptT String (DB m) [(Store.Balance, [Store.TxRef])]
listTxs cfg store net tell bals = do
  let used = filter ((/= 0) . (.txs)) bals
  listings <- fromStore $ countedInParallel tell ListingTransactions (configParallelRequests cfg) [storeAddressTxs store bal.address (fromIntegral bal.txs) | bal <- used]
  forM (zip used listings) $ \(bal, refs) -> do
    when (fromIntegral (length refs) /= bal.txs) $
      refusedAnswer store $
        Left ("The store counts " <> show bal.txs <> " transactions for " <> addrToString net bal.address <> " but listed " <> show (length refs))
    return (bal, refs)

{- The summaries -}

-- | What to fetch in full, from the summaries known with their blocks, the
-- stored balances and the listings of the addresses looked at: the listed
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

-- | The live transactions that are dead: those fetched that the store
-- reports deleted or does not return, replaced, dropped from the mempool or
-- left out by a reorganisation; and, when the sync looked at every address,
-- those no address lists any more, since a reorganisation may remove a
-- confirmed transaction without a trace in the addresses it touched. Their
-- summaries are marked dead and kept, for the history, and are no longer
-- live: a transaction the store lists again later is fetched and comes back
-- to life.
deadTransactions :: Bool -> Set TxHash -> [TxHash] -> [Store.Transaction] -> [Store.TxRef] -> [TxHash]
deadTransactions everything live fetched txs listed = nubOrd $ gone <> vanished
  where
    alive = Set.fromList [t.txid | t <- txs, not t.deleted]
    gone = [tid | tid <- fetched, tid `Set.notMember` alive, tid `Set.member` live]
    stillListed = Set.fromList $ (.txid) <$> listed
    vanished = [tid | everything, tid <- Set.toList live, tid `Set.notMember` stillListed]

{- The coins and their raw transactions -}

-- | The coins of the addresses of a pool the sync looks at, all of them,
-- which the store must list as many of as it counts, and the raw
-- transactions they come from, which signing needs. A raw transaction
-- never changes, so the ones already stored are not fetched again. A full sync checks them
-- instead, and fetches those that went wrong in the database. The raw
-- transactions are stored first, and a coin they do not pay as the store
-- reports it is refused before any coin is stored ('checkListedCoins'):
-- its value is what the builders count on. Each account of the pool then
-- takes the coins at its own addresses.
syncCoins :: (MonadUnliftIO m) => Ctx -> Store -> Pool -> Bool -> [Store.Balance] -> ExceptT String (DB m) Refreshed
syncCoins ctx store pool full lookedAt = do
  let net = pool.poolNetwork
      lookedAtAddrs = (.address) <$> lookedAt
      counted = sum $ (.utxo) <$> lookedAt
  unspents <- fromStore $ storeUnspents store lookedAtAddrs
  when (fromIntegral (length unspents) /= counted) $
    refusedAnswer store $
      Left ("The store counts " <> show counted <> " coins for the addresses to update but listed " <> show (length unspents))
  let parents = nubOrd $ (.outpoint.hash) <$> unspents
  wanted <- lift $ if full then unsoundRawTxs parents else missingRawTxs parents
  rawTxs <- answeredBy store $ fromStore $ rawTransactions store wanted
  lift $ forM_ rawTxs insertRawTx
  refusedAnswer store =<< checkListedCoins unspents
  accounts <- lift $ poolAccounts pool
  fmap mconcat . forM accounts $ \(accId, acc) -> do
    own <- lift $ storedAmong lookedAtAddrs (accId, acc)
    ownAddrs <- liftEither $ mapM (textToAddrE net . dBAddressAddress) own
    -- A set, as a first sync looks at thousands of addresses
    let ownSet = Set.fromList ownAddrs
    refreshCoins net ctx accId ownAddrs [coin | coin <- unspents, maybe False (`Set.member` ownSet) coin.address]

-- | Drop the raw transactions no coin needs any more. A raw transaction is
-- kept while a coin comes from it: the parents of the coins that are gone
-- and the transactions just retired are dropped, unless one of their
-- outputs is still a coin of the wallet, change for instance. A full sync
-- looks at every raw transaction instead, which also cleans up after the
-- versions of hw that kept them all.
pruneRawTxs :: (MonadUnliftIO m) => Bool -> Refreshed -> [TxHash] -> ExceptT String (DB m) ()
pruneRawTxs full refreshed retired
  | full = void dropOrphanRawTxs
  | otherwise = do
      goneParents <- liftEither $ mapM (fmap (.hash) . coinOutpoint) refreshed.deletedCoins
      void $ lift $ dropRawTxsWithoutCoins $ nubOrd $ goneParents <> retired

{- The pending transactions -}

-- | The pending transactions of the pool after a sync, given the
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
syncPending :: (MonadUnliftIO m) => Store -> Pool -> Set TxHash -> ExceptT String (DB m) (Natural, Natural, [TxHash])
syncPending store pool seen = do
  held <- pendingTxs pool
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
