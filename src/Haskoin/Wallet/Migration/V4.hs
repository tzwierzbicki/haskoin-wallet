{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The database format of version 4, and the step that brings a database
-- of version 3 (hw 0.10) to it: the wallets are gone. An account was filed
-- under a wallet named by the fingerprint of the first account of its seed
-- on its network, which gave one seed a wallet per network. An account is
-- now identified by its key alone and gets a plain id, which its addresses,
-- summaries, coins and pending transactions point to. The fingerprint of
-- the master key of its seed starts unknown: hw learns it from the mnemonic
-- or from the offline wallet.
--
-- The other columns change with it. A summary says whether its transaction
-- is dead, where version 3 deleted the summary: every summary it kept is
-- alive. The locked column of the coins and the free column of the
-- addresses go: the pending transactions tell which coins they lock and
-- which change addresses they hold. The confirmed columns of the summaries
-- and the coins, which nothing read, go as well, and so does the copy of
-- the whole coin that a coin row kept: its columns and its address, which
-- gives its script, are the coin. A pending transaction keeps whether the
-- last sync found it in conflict, out of conflict until the next sync asks
-- the store. The keys are made for large wallets: an address is keyed by
-- its chain and index instead of the text of its path, and the transaction
-- ids, outpoints, nosigHashes and raw transaction hashes are the bytes of
-- their hexadecimal text; indexes persistent cannot declare complete the
-- tables.
--
-- The step reads the accounts with the tables of "Haskoin.Wallet.Schema.V3"
-- and works on those of "Haskoin.Wallet.Schema.V4": the old tables are set
-- aside, the new ones created, the accounts stored again in the order they
-- were created, the other rows copied under the new ids and keys, the
-- indexes created, and the old tables dropped.
module Haskoin.Wallet.Migration.V4 where

import Conduit (MonadUnliftIO)
import Control.Monad (forM_, void)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Persist.Sql (rawExecute, runMigrationQuiet)
import Haskoin.Wallet.Database (DB)
import qualified Haskoin.Wallet.Schema.V3 as V3
import Haskoin.Wallet.Schema.V4

-- | Bring a database of version 3 to version 4, and store the version 4
migrateDB :: (MonadUnliftIO m) => ExceptT String (DB m) ()
migrateDB = lift $ do
  olds <- P.selectList [] [P.Asc V3.DBAccountCreated, P.Asc V3.DBAccountNetwork, P.Asc V3.DBAccountIndex]
  forM_ rebuiltTables $ \table ->
    rawExecute ("alter table " <> table <> " rename to old_" <> table) []
  void $ runMigrationQuiet migrateAll
  forM_ olds $ P.insert_ . account . P.entityVal
  forM_ (copies <> tableIndexes) $ \statement -> rawExecute statement []
  -- The children first: a parent still referenced cannot be dropped
  forM_ (reverse rebuiltTables) $ \table -> rawExecute ("drop table old_" <> table) []
  rawExecute "drop table d_b_wallet" []
  P.deleteWhere ([] :: [P.Filter DBVersion])
  P.insert_ $ DBVersion 4

-- | An account of version 3 as version 4 stores it: without its wallet and
-- its derivation path, with the fingerprint of its seed still to be learnt
account :: V3.DBAccount -> DBAccount
account old =
  DBAccount
    { dBAccountName = V3.dBAccountName old,
      dBAccountIndex = V3.dBAccountIndex old,
      dBAccountNetwork = V3.dBAccountNetwork old,
      dBAccountExternal = V3.dBAccountExternal old,
      dBAccountInternal = V3.dBAccountInternal old,
      dBAccountXPubKey = V3.dBAccountXPubKey old,
      dBAccountFingerprint = Nothing,
      dBAccountBalanceConfirmed = V3.dBAccountBalanceConfirmed old,
      dBAccountBalanceUnconfirmed = V3.dBAccountBalanceUnconfirmed old,
      dBAccountBalanceCoins = V3.dBAccountBalanceCoins old,
      dBAccountCreated = V3.dBAccountCreated old
    }

-- | The tables whose keys change, the accounts first
rebuiltTables :: [Text]
rebuiltTables = ["d_b_account", "d_b_address", "d_b_tx_info", "d_b_coin", "d_b_pending_tx", "d_b_raw_tx"]

-- | The rows of the old tables under the new ids and keys. The rows of an
-- account find its id through its key, unique in the old table and in the
-- new one; the hashes and outpoints, stored as hexadecimal text, become
-- their bytes. The columns version 4 drops are left behind, and the new
-- ones take their defaults: no summary is dead and no pending transaction
-- is in conflict.
copies :: [Text]
copies =
  [ "insert into d_b_address \
    \(account, internal, \"index\", address, label, balance_confirmed, balance_unconfirmed, \
    \balance_coins, balance_txs, balance_received, created) \
    \select n.id, o.internal, o.\"index\", o.address, o.label, o.balance_confirmed, o.balance_unconfirmed, \
    \o.balance_coins, o.balance_txs, o.balance_received, o.created \
    \from old_d_b_address o " <> throughAccount,
    "insert into d_b_tx_info (account, txid, block_ref, blob, created) \
    \select n.id, unhex(o.txid), o.block_ref, o.blob, o.created \
    \from old_d_b_tx_info o " <> throughAccount,
    "insert into d_b_coin (account, outpoint, address, value, block_ref, created) \
    \select n.id, unhex(o.outpoint), o.address, o.value, o.block_ref, o.created \
    \from old_d_b_coin o " <> throughAccount,
    "insert into d_b_pending_tx (account, nosig_hash, blob, online, created) \
    \select n.id, unhex(o.nosig_hash), o.blob, o.online, o.created \
    \from old_d_b_pending_tx o " <> throughAccount,
    "insert into d_b_raw_tx (hash, blob) select unhex(hash), blob from old_d_b_raw_tx"
  ]
  where
    throughAccount =
      "join old_d_b_account a on a.wallet = o.account_wallet and a.derivation = o.account_derivation \
      \join d_b_account n on n.x_pub_key = a.x_pub_key"
