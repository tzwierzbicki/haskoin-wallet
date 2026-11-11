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

-- | The tables of the database format version 5, the latest one:
-- "Haskoin.Wallet.Database" exports them again as the tables of hw. They
-- stay here as they are once a later version changes them, so that the
-- step to that version still has them ("Haskoin.Wallet.Migration").
--
-- Version 5 is the format of hw 0.10. It reads no database of an earlier
-- version: a user of hw 0.9 creates the wallet again from its mnemonic.
-- The earlier formats kept single accounts, and hw 0.9 kept no
-- fingerprint of the master key, which only the mnemonic gives.
--
-- The accounts are filed under portfolios, and the portfolios under
-- wallets. A wallet is the master key of one mnemonic. It has an index,
-- which names it, wallet0 and so on, a description, and the fingerprint
-- of its master key, which every wallet has, each its own. A portfolio
-- holds every account of its wallet that has the same account index, on
-- every network and of every kind: all twelve of them, from the moment it
-- exists. It has that index, which names it, portfolio0 and so on, a
-- description, and the date it was created. An account is one derivation
-- path on one network: it has its portfolio, its network and its purpose,
-- the first step of its path, 44 for a BIP44 account with P2PKH addresses
-- and 84 for a BIP84 account with P2WPKH addresses. Its index is that of
-- its portfolio, and the fingerprint of its seed that of its wallet.
--
-- One portfolio is the active one: the commands work in it, without an
-- option that names it, until the command switch makes another one
-- active. It is a fact the user decides, so it is kept, in a table of its
-- own that holds one row at most: the row at the slot 0, which names the
-- portfolio. The first portfolio created in a database becomes active. A
-- database without any portfolio has no row there. The configuration
-- file does not hold it: a key of the file could name a portfolio the
-- database does not have, as after the user points database-file at a
-- backup, and the two could disagree.
--
-- The test networks all have the coin type 1, so the accounts of a
-- portfolio on two of them have the same key and the same addresses.
-- Every unique key therefore names the network, or the account, which is
-- on one network: a key is unique on its network, an address in its
-- account, and a coin in its account.
--
-- The transactions belong to a portfolio on a network, not to an account:
-- a payment may spend the coins of the BIP44 and of the BIP84 account of a
-- portfolio together, and is shown once, as the portfolio sees it. So the
-- summaries, the labels of transactions, the pending transactions and the
-- block the last sync reached name a portfolio and a network. An address
-- and a coin name their account.
--
-- A summary says whether its transaction is dead, replaced or dropped
-- from the network, and keeps the time the store gives its transaction:
-- when the store first saw it, in its mempool or in a block. A summary
-- also keeps its amount in a column, beside the blob that holds it, so
-- that txs can add up the amounts without reading the blobs. A coin keeps
-- its address, which gives its script, and not the whole coin as the
-- store reported it. Whether a coin is locked and whether a change
-- address is free are read from the pending transactions, which a locked
-- column of the coins and a free column of the addresses could only
-- disagree with. A coin keeps instead whether the user locked it, which
-- only the user can tell. A pending transaction keeps whether the store,
-- at the last sync, found one of its coins spent by another transaction
-- or gone. The labels the user gives the transactions have a table of
-- their own, which the sync never touches: a summary it rewrites or marks
-- dead keeps its label. A pending transaction keeps its label in its
-- signing data until then. The block the last sync of a portfolio reached
-- on a network has a table of its own, with a row once it has synced.
-- The best block of the network cannot serve: each portfolio is synced
-- apart, and the first one to see a reorganisation of the chain moves
-- that block past it for the others.
--
-- The keys are made for large wallets. An address is keyed by its account,
-- its chain and its index, the order `addresses` lists it in, and its
-- derivation path is computed from the two, as the path of an account is
-- from its network, its purpose and the index of its portfolio.
-- Transaction ids, outpoints and nosigHashes are the bytes of the hash,
-- not its hexadecimal text: half the size in every key and index. The
-- 'tableIndexes' below are the ones persistent cannot declare: a covering
-- index that serves the ordering of txs, the sums of its balances and the
-- scan of the sync without touching the rows; the account of every coin;
-- the coins of an address of an account with their values, which the
-- sync refreshes and balances adds up without touching the rows; the
-- coins of an account in the order coins lists them; the coins by their
-- outpoint, for a command that names a coin without its account; and the
-- pending transactions of a portfolio on a network.
module Haskoin.Wallet.Schema.V5 where

import Data.ByteString (ByteString)
import Data.Int (Int64)
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

DBWallet
  index Int
  fingerprint Text
  description Text default=''
  created UTCTime default=CURRENT_TIMESTAMP
  UniqueWalletIndex index
  UniqueWalletFingerprint fingerprint
  deriving Show
  deriving Eq

DBPortfolio
  wallet DBWalletId
  index Int
  description Text default=''
  created UTCTime default=CURRENT_TIMESTAMP
  UniquePortfolio wallet index
  deriving Show
  deriving Eq

DBActivePortfolio
  slot Int
  portfolio DBPortfolioId
  Primary slot
  deriving Show
  deriving Eq

DBAccount
  portfolio DBPortfolioId
  network Text
  purpose Int default=44
  external Int
  internal Int
  xPubKey Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  created UTCTime default=CURRENT_TIMESTAMP
  UniqueAccount portfolio network purpose
  UniqueXPubKey network xPubKey
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
  UniqueAddress account address
  deriving Show
  deriving Eq

DBTxInfo
  portfolio DBPortfolioId
  network Text
  txid KeyBytes
  blockRef ByteString
  time UTCTime
  amount Int64
  blob ByteString
  dead Bool default=0
  created UTCTime default=CURRENT_TIMESTAMP
  Primary portfolio network txid
  deriving Show
  deriving Eq

DBTxLabel
  portfolio DBPortfolioId
  network Text
  txid KeyBytes
  label Text
  Primary portfolio network txid
  deriving Show
  deriving Eq

DBCoin
  account DBAccountId
  outpoint KeyBytes
  address Text
  value Word64
  blockRef ByteString
  lockedByUser Bool default=0
  created UTCTime default=CURRENT_TIMESTAMP
  Primary account outpoint
  deriving Show
  deriving Eq

DBRawTx
  hash KeyBytes
  blob ByteString
  Primary hash
  deriving Show
  deriving Eq

DBPendingTx
  portfolio DBPortfolioId
  network Text
  nosigHash KeyBytes
  blob ByteString
  online Bool
  conflicted Bool default=0
  created UTCTime default=CURRENT_TIMESTAMP
  Primary nosigHash
  deriving Show
  deriving Eq

DBBest
  network Text
  bestBlock Text
  bestHeight Int
  bestTime Int Maybe
  Primary network
  deriving Show

DBSyncedBlock
  portfolio DBPortfolioId
  network Text
  block Text
  height Int
  Primary portfolio network
  deriving Show
  deriving Eq
|]

-- | The indexes of the tables above that persistent cannot declare, created
-- right after them, in a new database. A later version that has
-- persistent rebuild one of these tables loses its indexes, and its step
-- must create them again.
tableIndexes :: [Text]
tableIndexes =
  [ "create index d_b_tx_info_pool_block_ref on d_b_tx_info (portfolio, network, block_ref, txid, dead, amount)",
    "create index d_b_coin_account on d_b_coin (account)",
    "create index d_b_coin_account_address on d_b_coin (account, address, value)",
    "create index d_b_coin_account_block_ref on d_b_coin (account, block_ref, outpoint)",
    "create index d_b_coin_outpoint on d_b_coin (outpoint)",
    "create index d_b_pending_tx_pool on d_b_pending_tx (portfolio, network)"
  ]
