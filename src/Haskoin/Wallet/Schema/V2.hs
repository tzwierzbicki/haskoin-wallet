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

-- | The tables of the database format version 2, written by hw 0.9.0 to
-- 0.9.4: the table definitions of src/Haskoin/Wallet/Database.hs at the tag
-- v0.9.4, copied word for word. The version table appeared with this
-- version, holding the text "0.9". The step to version 2 creates these
-- tables, and the step to version 3 starts from them.
--
-- The entities keep their names, which decide the table names, so this
-- module is imported qualified next to "Haskoin.Wallet.Database".
module Haskoin.Wallet.Schema.V2 where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

DBVersion
  version Text
  Primary version
  deriving Show

DBWallet
  fingerprint Text
  Primary fingerprint
  deriving Show

DBAccount
  name Text
  wallet DBWalletId
  index Int
  network Text
  derivation Text
  external Int
  internal Int
  xPubKey Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  created UTCTime default=CURRENT_TIME
  Primary wallet derivation
  UniqueName name
  UniqueXPubKey xPubKey
  UniqueNetworkId wallet network index
  deriving Show
  deriving Eq

DBAddress
  index Int
  accountWallet DBWalletId
  accountDerivation Text
  derivation Text
  address Text
  label Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  balanceTxs Word64
  balanceReceived Word64
  internal Bool
  free Bool
  created UTCTime default=CURRENT_TIME
  Primary accountWallet accountDerivation derivation
  UniqueAddress address
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBTxInfo
  accountWallet DBWalletId
  accountDerivation Text
  txid Text
  blockRef ByteString
  confirmed Bool
  blob ByteString
  created UTCTime default=CURRENT_TIME
  Primary accountWallet accountDerivation txid
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBCoin
  accountWallet DBWalletId
  accountDerivation Text
  outpoint Text
  address Text
  value Word64
  blockRef ByteString
  blob ByteString
  confirmed Bool
  locked Bool
  created UTCTime default=CURRENT_TIME
  Primary outpoint
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBRawTx
  hash Text
  blob ByteString
  Primary hash
  deriving Show
  deriving Eq

DBPendingTx
  accountWallet DBWalletId
  accountDerivation Text
  nosigHash Text
  blob ByteString
  online Bool
  created UTCTime default=CURRENT_TIME
  Primary nosigHash
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBBest
  network Text
  bestBlock Text
  bestHeight Int
  Primary network
  deriving Show
|]
