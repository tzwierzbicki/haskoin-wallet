{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The tables of the database format version 7, the latest one:
-- "Haskoin.Wallet.Database" exports them again as the tables of the wallet.
-- They stay here as they are once a later version changes them, so that the
-- step to that version still has them.
--
-- Version 7 dropped the wallets. An account used to be filed under a wallet
-- named by the fingerprint of the first account of its seed on its network,
-- which gave one seed a wallet per network. An account is now identified by
-- its key alone, and carries the fingerprint of the master key of its seed
-- once hw has learnt it: from the mnemonic, or from a document or a signed
-- transaction of the offline wallet. Every account has a plain id, which
-- its addresses, summaries, coins and pending transactions point to. The
-- confirmed columns of the summaries and the coins, which nothing read, are
-- gone as well.
--
-- The keys are made for large wallets. An address is keyed by its account,
-- its chain and its index, the order addrs lists it in, and its derivation
-- path is computed from the two. Transaction ids, outpoints and nosigHashes
-- are the bytes of the hash, not its hexadecimal text: half the size in
-- every key and index. The 'tableIndexes' below are the ones persistent cannot
-- declare: a covering index that serves the ordering of txs and the scan
-- of the sync without touching the rows, and the account of every coin and
-- pending transaction, which their keys do not give.
module Haskoin.Wallet.Schema.V7 where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import Database.Persist.TH
import Haskoin.Wallet.Schema.KeyBytes (KeyBytes)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

DBVersion
  version Int
  Primary version
  deriving Show

DBAccount
  name Text
  index Int
  network Text
  derivation Text
  external Int
  internal Int
  xPubKey Text
  fingerprint Text Maybe
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  created UTCTime default=CURRENT_TIMESTAMP
  UniqueName name
  UniqueXPubKey xPubKey
  deriving Show
  deriving Eq

DBAddress
  account DBAccountId
  internal Bool
  index Int
  address Text
  label Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  balanceTxs Word64
  balanceReceived Word64
  created UTCTime default=CURRENT_TIMESTAMP
  Primary account internal index
  UniqueAddress address
  deriving Show
  deriving Eq

DBTxInfo
  account DBAccountId
  txid KeyBytes
  blockRef ByteString
  blob ByteString
  dead Bool default=0
  created UTCTime default=CURRENT_TIMESTAMP
  Primary account txid
  deriving Show
  deriving Eq

DBCoin
  account DBAccountId
  outpoint KeyBytes
  address Text
  value Word64
  blockRef ByteString
  blob ByteString
  created UTCTime default=CURRENT_TIMESTAMP
  Primary outpoint
  deriving Show
  deriving Eq

DBRawTx
  hash KeyBytes
  blob ByteString
  Primary hash
  deriving Show
  deriving Eq

DBPendingTx
  account DBAccountId
  nosigHash KeyBytes
  blob ByteString
  online Bool
  created UTCTime default=CURRENT_TIMESTAMP
  Primary nosigHash
  deriving Show
  deriving Eq

DBBest
  network Text
  bestBlock Text
  bestHeight Int
  Primary network
  deriving Show
|]

-- | The indexes of the tables above that persistent cannot declare, created
-- right after them, for a new database and by the step to this version
-- alike. A later version that has persistent rebuild one of these tables
-- loses its indexes and must create them again.
tableIndexes :: [Text]
tableIndexes =
  [ "create index d_b_tx_info_account_block_ref on d_b_tx_info (account, block_ref, txid, dead)",
    "create index d_b_coin_account on d_b_coin (account)",
    "create index d_b_pending_tx_account on d_b_pending_tx (account)"
  ]
