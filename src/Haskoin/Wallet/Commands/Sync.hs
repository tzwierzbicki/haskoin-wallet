{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | sync, and the user who watches a sync or a recovery on the terminal:
-- the status line, and the question of a scan that finds too many used
-- addresses in a row ('terminalWatcher'). "Haskoin.Wallet.Sync" does the
-- work with the store. recover and the commands that send a transaction
-- sync with the same watcher.
module Haskoin.Wallet.Commands.Sync
  ( terminalWatcher,
    cmdSync,
    shownSyncs,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isNothing)
import Data.String.Conversions (cs)
import Haskoin
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Sync.Status (StatusLine (..), withStatusLine)

-- | The question of a scan that reaches
-- 'Haskoin.Wallet.Database.Address.scanLimit' used addresses in a row,
-- put to the user on the terminal. --yes does not answer it, so that
-- a sync nobody watches does not believe a store that lies: an input that
-- has ended cancels the command instead. The status line makes room for
-- it, and comes back with the next step.
--
-- "Keep looking?" alone reads as "more, or is this enough?", and a user
-- who answered no to keep what was found lost it. So the question says
-- first why it is asked, when a yes is right, and that a no keeps
-- nothing ('scanNotice'), a sentence a line, as the question of a wallet
-- behind the chain is laid out (@confirmBehind@).
scanQuestion :: StatusLine -> ScanQuestion (DB IO)
scanQuestion line found = do
  liftIO $ eraseStatus line
  liftIO $ mapM_ (tell terminal) (found : scanNotice)
  askYes terminal (NoScriptOption "to see what the store reports") "Keep looking?"

-- | What the user reads between the report of the store and the question
-- of a scan. With an honest store, a yes always ends: the scan stops at
-- the gap. So a no is for a report the user does not believe, and what
-- was found so far is that report: nothing of it is kept.
scanNotice :: [String]
scanNotice =
  [ "A store that lies can report this without end.",
    "If the report is right for this wallet, answer yes. A no stops here, and nothing of what was found is kept."
  ]

-- | The user on the terminal, watching a sync or a search on the line
terminalWatcher :: StatusLine -> Watcher (DB IO)
terminalWatcher line = Watcher {question = scanQuestion line, status = showStep line}

-- | Sync every portfolio, on every network it has an account on, or on the
-- --network alone. A sync covers every portfolio, the active one or not:
-- a user of several wallets wants them all synced, and syncing one too
-- many does no harm. It covers the networks the configuration hides too,
-- which the status line names as they are synced: a hidden network is up
-- to date the day the user shows it. The answer keeps to the networks hw
-- shows ('shownSyncs'). It gives no sync of a hidden network, and names
-- none as not served or not synced, so a store that fails on a hidden
-- network alone does not make the command exit with an error. A network
-- the host does not serve is named and left out. So is one whose store is
-- down, or fails during the sync of one of its pools, which also makes
-- the command exit with an error once the others are synced: the sync of
-- that pool is undone, and the syncs of the other networks are kept
-- ('syncPools'). So is one whose store is not on the chain of the last
-- sync, which only --new-chain follows
-- ('Haskoin.Wallet.Sync.chainSeen'). The answer names the active
-- portfolio too: the steps after a sync work in it, and a pending
-- transaction in conflict in another portfolio needs a switch first.
cmdSync :: Config -> Maybe Network -> SyncMode -> IO Response
cmdSync cfg netM mode =
  withStatusLine $ \line ->
    runDBResponse cfg $ do
      -- A new chain is taken on one network at a time, the one the user
      -- names: the stores of the other networks are refused as before
      when (mode == NewChain && isNothing netM) $
        throwError "--new-chain goes with --network: it follows the new chain of the store of one network"
      portfolios <- lift getAllPortfolios
      when (null portfolios) $ throwError noWallets
      pools <- concat <$> mapM (lift . portfolioPools . (.portfolioId)) portfolios
      let chosen = [pool | pool <- pools, maybe True (== pool.poolNetwork) netM]
      storeOn <- liftIO $ webStores cfg
      synced <- syncPools cfg storeOn chosen mode (terminalWatcher line)
      -- The steps after the sync work in the active portfolio
      activeM <- activePortfolioRef
      -- The answer keeps to the networks hw shows
      return $
        ResponseSync
          (shownSyncs cfg (syncedPools synced))
          (onShownNetworks cfg id (syncedNotServed synced))
          [(net, cs failure) | (net, failure) <- onShownNetworks cfg fst (syncedDown synced)]
          activeM

-- | The syncs an answer gives, among those sync or recover did: those of
-- the networks hw shows. The syncs of the hidden networks were done all
-- the same, and are kept in the database.
shownSyncs :: Config -> [SyncRes] -> [SyncRes]
shownSyncs cfg = onShownNetworks cfg (placeNetwork . syncResPlace)
