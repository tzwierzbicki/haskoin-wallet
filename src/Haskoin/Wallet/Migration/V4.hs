{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The database format of version 4, and the step that brings a database
-- of version 3 to it. A database of hw 0.9 reaches version 3 by the step
-- before this one, which only changes how the version is stored, so this
-- step is the one that turns the accounts of hw 0.9 into portfolios.
--
-- Version 4 files the accounts under portfolios, and the portfolios under
-- wallets: see "Haskoin.Wallet.Schema.V4". The step makes them from what
-- version 3 has:
--
-- * A wallet for each wallet of version 3, in the order their first
--   accounts were created, so that the oldest one is wallet0. A wallet of
--   version 3 was named by the fingerprint of the first account of its
--   seed on a coin type, m\/44'\/coin'\/0'. That tells which accounts
--   share a seed on one coin type, and nothing more: the accounts of one
--   seed on Bitcoin, on Bitcoin Cash and on the test networks, which share
--   the coin type 1, are in three wallets, and so they stay here. The fingerprint of the master key is not known, and the
--   new wallets have none. hw merges the wallets of one seed and records
--   the fingerprint once it is given the mnemonic, or a file of the wallet
--   ('Haskoin.Wallet.Database.Account.adoptSeed').
--
-- * A portfolio for each account index a wallet uses, and no other: a
--   wallet with accounts at the indexes 0 and 2 gets portfolio 0 and
--   portfolio 2, and index 1 stays free. A wallet always has portfolio 0,
--   so one is made, empty, for a wallet that has no account at index 0.
--   The names of the accounts of a portfolio become its description, in
--   the order the accounts were created. Its creation date is that of its
--   oldest account. The empty portfolio 0 bears the date of its wallet,
--   which counts for nothing: a merge takes the date of the portfolio that
--   has accounts, and its first account gives it a date of its own
--   ("Haskoin.Wallet.Database.Account".'insertAccount').
--
-- * The accounts as they are, each in the portfolio of its index, with the
--   purpose 44: every account of version 3 is a BIP44 one. The portfolios
--   are partial. The other accounts of a portfolio are behind hardened
--   steps of the path, which a public account key cannot derive, and the
--   step does not ask for the mnemonic. It ends with a notice that says
--   how to fill the portfolios in.
--
-- The transactions go from the account to its portfolio and network. An
-- account of version 3 is the only account of its portfolio on its
-- network, so its summaries, as they are, are those of the portfolio
-- there. Their blobs name the paths of the addresses without the type of
-- their account, which reads as BIP44, the type they are of.
--
-- The other columns change as the first form of version 4 had them. A
-- summary says whether its transaction is dead, where version 3 deleted
-- the summary: every summary it kept is alive. A summary keeps its amount
-- in a column, which the step reads from its blob: every blob has it,
-- since hw 0.9. The locked column of the coins and the free column of the
-- addresses go: the pending transactions tell which coins they lock and
-- which change addresses they hold. The confirmed columns of the
-- summaries and the coins, which nothing read, go as well, and so does
-- the copy of the whole coin that a coin row kept: its columns and its
-- address, which gives its script, are the coin. A pending transaction
-- keeps whether the last sync found it in conflict, out of conflict until
-- the next sync asks the store. The labels of the transactions, which
-- version 3 did not have, get a table of their own, empty. So does the
-- block the last sync of a portfolio reached on a network, which version
-- 3 did not keep: the first sync then looks at every address. The keys
-- are made for large wallets: an address is keyed by its chain and index
-- instead of the text of its path, and the transaction ids, outpoints,
-- nosigHashes and raw transaction hashes are the bytes of their
-- hexadecimal text; indexes persistent cannot declare complete the
-- tables.
--
-- The step reads the wallets and the accounts with the tables of
-- "Haskoin.Wallet.Schema.V3" and works on those of
-- "Haskoin.Wallet.Schema.V4": the old tables are set aside, the new ones
-- created, the wallets, portfolios and accounts stored, the other rows
-- copied under the new keys, the indexes created, and the old tables
-- dropped.
module Haskoin.Wallet.Migration.V4 where

import Conduit (MonadUnliftIO)
import Control.Monad (forM, forM_, void)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Persist as P
import Database.Persist.Sql (rawExecute, runMigrationQuiet)
import Haskoin.Wallet.Database (DB)
import qualified Haskoin.Wallet.Schema.V3 as V3
import Haskoin.Wallet.Schema.V4

-- | Bring a database of version 3 to version 4, and store the version 4.
-- Returns what to tell the user once it is done: how to fill in the
-- partial portfolios, when the database had accounts.
migrateDB :: (MonadUnliftIO m) => ExceptT String (DB m) [String]
migrateDB = lift $ do
  olds <- map P.entityVal <$> P.selectList [] [P.Asc V3.DBAccountCreated, P.Asc V3.DBAccountNetwork, P.Asc V3.DBAccountIndex]
  forM_ rebuiltTables $ \table ->
    rawExecute ("alter table " <> table <> " rename to old_" <> table) []
  void $ runMigrationQuiet migrateAll
  -- The wallets of version 3, the one with the oldest account first
  let oldWallets = nub (V3.dBAccountWallet <$> olds)
  forM_ (zip [0 ..] oldWallets) $ \(walletIndex, oldWallet) -> do
    let accounts = [old | old <- olds, V3.dBAccountWallet old == oldWallet]
        created = minimum (V3.dBAccountCreated <$> accounts)
    walletId <- P.insert (DBWallet walletIndex Nothing "" created)
    -- A portfolio for each index the wallet uses, and portfolio 0
    let indexes = nub (0 : sort (V3.dBAccountIndex <$> accounts))
    portfolios <- forM indexes $ \index -> do
      let members = [old | old <- accounts, V3.dBAccountIndex old == index]
          description = Text.intercalate ", " (nub (V3.dBAccountName <$> members))
          since = if null members then created else minimum (V3.dBAccountCreated <$> members)
      portfolioId <- P.insert (DBPortfolio walletId index description since)
      return (index, portfolioId)
    let portfolioOf = Map.fromList portfolios
    forM_ accounts $ \old ->
      forM_ (Map.lookup (V3.dBAccountIndex old) portfolioOf) $ \portfolioId ->
        P.insert_ (account portfolioId old)
  forM_ (copies <> tableIndexes) $ \statement -> rawExecute statement []
  -- The children first: a parent still referenced cannot be dropped
  forM_ (reverse rebuiltTables) $ \table -> rawExecute ("drop table old_" <> table) []
  P.deleteWhere ([] :: [P.Filter DBVersion])
  P.insert_ $ DBVersion 4
  return [fillInNotice | not (null olds)]

-- | What the user is told once a database with accounts is at version 4:
-- that the accounts are in partial portfolios, and how to fill them in
fillInNotice :: String
fillInNotice =
  "The accounts of this database are now in portfolios. A portfolio holds every account of a mnemonic \
  \at one account index, on every network. This database had only some of them, so its portfolios are \
  \partial, and the accounts of one mnemonic on Bitcoin, on Bitcoin Cash and on the test networks are in \
  \separate wallets. \
  \`hw portfolios` lists them. To fill them in and bring each mnemonic into one wallet, run `hw fillwallet` \
  \on the computer that has the mnemonic, once for each mnemonic. If that is another computer, run \
  \`hw exportwallet FILE` there afterwards, and `hw importwallet FILE` here."

-- | An account of version 3 as version 4 stores it, in its portfolio:
-- without its name, its index, its wallet and its derivation path, and
-- with the purpose 44, since every account of version 3 is a BIP44 one
account :: DBPortfolioId -> V3.DBAccount -> DBAccount
account portfolioId old =
  DBAccount
    { dBAccountPortfolio = portfolioId,
      dBAccountNetwork = V3.dBAccountNetwork old,
      dBAccountPurpose = 44,
      dBAccountExternal = V3.dBAccountExternal old,
      dBAccountInternal = V3.dBAccountInternal old,
      dBAccountXPubKey = V3.dBAccountXPubKey old,
      dBAccountBalanceConfirmed = V3.dBAccountBalanceConfirmed old,
      dBAccountBalanceUnconfirmed = V3.dBAccountBalanceUnconfirmed old,
      dBAccountBalanceCoins = V3.dBAccountBalanceCoins old,
      dBAccountCreated = V3.dBAccountCreated old
    }

-- | The tables whose keys change, the wallets and the accounts first
rebuiltTables :: [Text]
rebuiltTables = ["d_b_wallet", "d_b_account", "d_b_address", "d_b_tx_info", "d_b_coin", "d_b_pending_tx", "d_b_raw_tx"]

-- | The rows of the old tables under the new keys. The rows of an account
-- find the new account through its key and its network, unique in the old
-- table and in the new one. An address and a coin take the id of that
-- account, and a summary and a pending transaction its portfolio and its
-- network. The hashes and outpoints, stored as hexadecimal text, become
-- their bytes. The amount of a summary comes out of its blob. The columns
-- version 4 drops are left behind, and the other new ones take their
-- defaults: no summary is dead or has a time, which the next sync
-- fetches, no pending transaction is in conflict, and the user has locked
-- no coin. The locked column of version 3 said which coins the pending
-- transactions held, which version 4 reads from them.
copies :: [Text]
copies =
  [ "insert into d_b_address \
    \(account, internal, \"index\", address, label, balance_confirmed, balance_unconfirmed, \
    \balance_coins, balance_txs, balance_received, created) \
    \select n.id, o.internal, o.\"index\", o.address, o.label, o.balance_confirmed, o.balance_unconfirmed, \
    \o.balance_coins, o.balance_txs, o.balance_received, o.created \
    \from old_d_b_address o " <> throughAccount,
    "insert into d_b_tx_info (portfolio, network, txid, block_ref, amount, blob, created) \
    \select n.portfolio, n.network, unhex(o.txid), o.block_ref, json_extract(cast(o.blob as text), '$.amount'), o.blob, o.created \
    \from old_d_b_tx_info o " <> throughAccount,
    "insert into d_b_coin (account, outpoint, address, value, block_ref, created) \
    \select n.id, unhex(o.outpoint), o.address, o.value, o.block_ref, o.created \
    \from old_d_b_coin o " <> throughAccount,
    "insert into d_b_pending_tx (portfolio, network, nosig_hash, blob, online, created) \
    \select n.portfolio, n.network, unhex(o.nosig_hash), o.blob, o.online, o.created \
    \from old_d_b_pending_tx o " <> throughAccount,
    "insert into d_b_raw_tx (hash, blob) select unhex(hash), blob from old_d_b_raw_tx"
  ]
  where
    throughAccount =
      "join old_d_b_account a on a.wallet = o.account_wallet and a.derivation = o.account_derivation \
      \join d_b_account n on n.x_pub_key = a.x_pub_key and n.network = a.network"
