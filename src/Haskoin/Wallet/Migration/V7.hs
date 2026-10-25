{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The database format of version 7, and the step that brings a database
-- of version 6 to it: the wallets are gone. An account was filed under a
-- wallet named by the fingerprint of the first account of its seed on its
-- network, which gave one seed a wallet per network. An account is now
-- identified by its key alone and gets a plain id, which its addresses,
-- summaries, coins and pending transactions point to. The fingerprint of
-- the master key of its seed starts unknown: hw learns it from the mnemonic
-- or from the offline wallet. The confirmed columns of the summaries and
-- the coins, which nothing read, go as well. The keys are made for large
-- wallets: an address is keyed by its chain and index instead of the text
-- of its path, and the transaction ids, outpoints, nosigHashes and raw
-- transaction hashes are the bytes of their hexadecimal text; indexes
-- persistent cannot declare complete the tables.
--
-- The step reads the accounts with the tables of "Haskoin.Wallet.Schema.V6"
-- and works on those of "Haskoin.Wallet.Schema.V7": the old tables are set
-- aside, the new ones created, the accounts stored again in the order they
-- were created, the other rows copied under the new ids and keys, the
-- indexes created, and the old tables dropped.
module Haskoin.Wallet.Migration.V7 where

import Conduit (MonadUnliftIO)
import Control.Monad (forM_, void)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Persist.Sql (rawExecute, runMigrationQuiet)
import Haskoin.Wallet.Database (DB)
import qualified Haskoin.Wallet.Schema.V6 as V6
import Haskoin.Wallet.Schema.V7

-- | Bring a database of version 6 to version 7, and store the version 7
migrateDB :: (MonadUnliftIO m) => ExceptT String (DB m) ()
migrateDB = lift $ do
  olds <- P.selectList [] [P.Asc V6.DBAccountCreated, P.Asc V6.DBAccountNetwork, P.Asc V6.DBAccountIndex]
  forM_ rebuiltTables $ \table ->
    rawExecute ("alter table " <> table <> " rename to old_" <> table) []
  void $ runMigrationQuiet migrateAll
  forM_ olds $ P.insert_ . account . P.entityVal
  forM_ (copies <> tableIndexes) $ \statement -> rawExecute statement []
  -- The children first: a parent still referenced cannot be dropped
  forM_ (reverse rebuiltTables) $ \table -> rawExecute ("drop table old_" <> table) []
  rawExecute "drop table d_b_wallet" []
  P.deleteWhere ([] :: [P.Filter DBVersion])
  P.insert_ $ DBVersion 7

-- | An account of version 6 as version 7 stores it: without its wallet and
-- its derivation path, with the fingerprint of its seed still to be learnt
account :: V6.DBAccount -> DBAccount
account old =
  DBAccount
    { dBAccountName = V6.dBAccountName old,
      dBAccountIndex = V6.dBAccountIndex old,
      dBAccountNetwork = V6.dBAccountNetwork old,
      dBAccountExternal = V6.dBAccountExternal old,
      dBAccountInternal = V6.dBAccountInternal old,
      dBAccountXPubKey = V6.dBAccountXPubKey old,
      dBAccountFingerprint = Nothing,
      dBAccountBalanceConfirmed = V6.dBAccountBalanceConfirmed old,
      dBAccountBalanceUnconfirmed = V6.dBAccountBalanceUnconfirmed old,
      dBAccountBalanceCoins = V6.dBAccountBalanceCoins old,
      dBAccountCreated = V6.dBAccountCreated old
    }

-- | The tables whose keys change, the accounts first
rebuiltTables :: [Text]
rebuiltTables = ["d_b_account", "d_b_address", "d_b_tx_info", "d_b_coin", "d_b_pending_tx", "d_b_raw_tx"]

-- | The rows of the old tables under the new ids and keys. The rows of an
-- account find its id through its key, unique in the old table and in the
-- new one; the hashes and outpoints, stored as hexadecimal text, become
-- their bytes.
copies :: [Text]
copies =
  [ "insert into d_b_address \
    \(account, internal, \"index\", address, label, balance_confirmed, balance_unconfirmed, \
    \balance_coins, balance_txs, balance_received, created) \
    \select n.id, o.internal, o.\"index\", o.address, o.label, o.balance_confirmed, o.balance_unconfirmed, \
    \o.balance_coins, o.balance_txs, o.balance_received, o.created \
    \from old_d_b_address o " <> throughAccount,
    "insert into d_b_tx_info (account, txid, block_ref, blob, dead, created) \
    \select n.id, unhex(o.txid), o.block_ref, o.blob, o.dead, o.created \
    \from old_d_b_tx_info o " <> throughAccount,
    "insert into d_b_coin (account, outpoint, address, value, block_ref, blob, created) \
    \select n.id, unhex(o.outpoint), o.address, o.value, o.block_ref, o.blob, o.created \
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
