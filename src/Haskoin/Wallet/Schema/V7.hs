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
module Haskoin.Wallet.Schema.V7 where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import Database.Persist.TH

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
  index Int
  derivation Text
  address Text
  label Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  balanceTxs Word64
  balanceReceived Word64
  internal Bool
  created UTCTime default=CURRENT_TIMESTAMP
  Primary account derivation
  UniqueAddress address
  deriving Show
  deriving Eq

DBTxInfo
  account DBAccountId
  txid Text
  blockRef ByteString
  blob ByteString
  dead Bool default=0
  created UTCTime default=CURRENT_TIMESTAMP
  Primary account txid
  deriving Show
  deriving Eq

DBCoin
  account DBAccountId
  outpoint Text
  address Text
  value Word64
  blockRef ByteString
  blob ByteString
  created UTCTime default=CURRENT_TIMESTAMP
  Primary outpoint
  deriving Show
  deriving Eq

DBRawTx
  hash Text
  blob ByteString
  Primary hash
  deriving Show
  deriving Eq

DBPendingTx
  account DBAccountId
  nosigHash Text
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
