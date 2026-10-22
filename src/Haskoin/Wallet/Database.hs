{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The wallet database. Its tables are those of the latest format version,
-- defined in "Haskoin.Wallet.Schema.V5" and exported again from here: a
-- version that changes the tables gets a new schema module, and this module
-- moves to it.
module Haskoin.Wallet.Database
  ( module Haskoin.Wallet.Database,
    module Haskoin.Wallet.Schema.V5,
  )
where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Arrow (Arrow (second))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Lens ((&), (.~))
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.List (find, nub, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Database.Persist.SqlBackend.Internal (SqlBackend (..))
import Database.Persist.Sqlite (Statement (..), extraPragmas, mkSqliteConnectionInfo, runSqlConn, withSqliteConnInfo)
import Database.Sqlite (Error (..), SqliteException (..))
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Schema.V5
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSigning (parseTxSignData)
import UnliftIO.Exception (catch, throwIO)
import Haskoin.Wallet.Util (Page (Page), nosigHash, textToAddrE)
import Numeric.Natural (Natural)


type DB m = ReaderT SqlBackend (NoLoggingT (ResourceT m))

-- | Run a database action as one transaction that holds the database from
-- its first statement to its last. Every hw command, whether it reads or
-- writes, takes the write lock as it begins, so that commands run one at a
-- time: a sync cannot be undercut by a command slipping in while it waits
-- on the store, and what a command reads is what it writes against. A
-- command that finds the database held waits 'busyTimeout' for the other
-- one to finish, then fails with 'databaseInUse'.
runDB :: (MonadUnliftIO m) => Config -> DB m a -> m a
runDB cfg action = do
  dbFile <- liftIO $ databaseFile cfg
  runDBWith busyTimeout dbFile action

-- | How long a command waits for another one to release the database, in
-- milliseconds: enough for commands run back to back to follow each other,
-- too short to look stuck when a sync holds it
busyTimeout :: Int
busyTimeout = 3000

-- | 'runDB' on a database file, waiting the given milliseconds for the lock
runDBWith :: (MonadUnliftIO m) => Int -> FilePath -> DB m a -> m a
runDBWith timeout dbFile action =
  runResourceT . runNoLoggingT $
    withSqliteConnInfo connection $ \backend -> runSqlConn action (lockingAtOnce backend)
  where
    connection =
      mkSqliteConnectionInfo (cs dbFile)
        & extraPragmas .~ ["PRAGMA busy_timeout = " <> cs (show timeout) <> ";"]

-- | The connection with its transactions taking the write lock as they
-- begin rather than at their first write: SQLite's BEGIN IMMEDIATE
lockingAtOnce :: SqlBackend -> SqlBackend
lockingAtOnce backend =
  backend
    { connBegin = \prepare _ -> do
        statement <- prepare "BEGIN IMMEDIATE"
        _ <- stmtExecute statement []
        stmtReset statement
    }

-- | What a command answers when another hw command holds the database
databaseInUse :: String
databaseInUse = "The database is in use by another hw command; try again when it has finished"

-- | Run the action, answering 'databaseInUse' instead of failing when the
-- database is held by another command past the wait
catchDatabaseInUse :: (MonadUnliftIO m) => m a -> m (Either String a)
catchDatabaseInUse action = (Right <$> action) `catch` held
  where
    held failure = case failure of
      SqliteException ErrorBusy _ _ -> return $ Left databaseInUse
      SqliteException ErrorLocked _ _ -> return $ Left databaseInUse
      _ -> throwIO failure

globalMigration :: (MonadUnliftIO m) => DB m ()
globalMigration = void $ runMigrationQuiet migrateAll

{- Meta -}

updateBest ::
  (MonadUnliftIO m) => Network -> BlockHash -> BlockHeight -> DB m ()
updateBest net hash height = do
  let netT = cs net.name
      key = DBBestKey netT
  P.repsert key $ DBBest netT (blockHashToHex hash) (fromIntegral height)

getBest ::
  (MonadUnliftIO m) => Network -> DB m (Maybe (BlockHash, BlockHeight))
getBest net = do
  let netT = cs net.name
  resM <- P.get $ DBBestKey netT
  return $ do
    DBBest _ a b <- resM
    hash <- hexToBlockHash a
    return (hash, fromIntegral b)

{- Accounts -}

instance ToJSON DBAccount where
  toJSON acc =
    object
      [ "name" .= dBAccountName acc,
        "wallet" .= dBAccountWallet acc,
        "index" .= dBAccountIndex acc,
        "network" .= dBAccountNetwork acc,
        "derivation" .= dBAccountDerivation acc,
        "external" .= dBAccountExternal acc,
        "internal" .= dBAccountInternal acc,
        "xPubKey" .= dBAccountXPubKey acc,
        "balance" .= toJSON (AccountBalance confirm unconfirm coins),
        "created" .= dBAccountCreated acc
      ]
    where
      confirm = dBAccountBalanceConfirmed acc
      unconfirm = dBAccountBalanceUnconfirmed acc
      coins = dBAccountBalanceCoins acc

instance FromJSON DBAccount where
  parseJSON =
    withObject "DBAccount" $ \o -> do
      bal <- o .: "balance"
      DBAccount
        <$> o .: "name"
        <*> o .: "wallet"
        <*> o .: "index"
        <*> o .: "network"
        <*> o .: "derivation"
        <*> o .: "external"
        <*> o .: "internal"
        <*> o .: "xPubKey"
        <*> (bal .: "confirmed")
        <*> (bal .: "unconfirmed")
        <*> (bal .: "coins")
        <*> o .: "created"

data AccountBalance = AccountBalance
  { -- | confirmed balance
    accBalanceConfirmed :: !Word64,
    -- | unconfirmed balance
    accBalanceUnconfirmed :: !Word64,
    -- | number of unspent outputs
    accBalanceCoins :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AccountBalance where
  toJSON b =
    object
      [ "confirmed" .= accBalanceConfirmed b,
        "unconfirmed" .= accBalanceUnconfirmed b,
        "coins" .= accBalanceCoins b
      ]

instance FromJSON AccountBalance where
  parseJSON =
    withObject "accountbalance" $ \o ->
      AccountBalance
        <$> o .: "confirmed"
        <*> o .: "unconfirmed"
        <*> o .: "coins"

accountIndex :: DBAccount -> Natural
accountIndex = fromIntegral . dBAccountIndex

accountNetwork :: DBAccount -> Network
accountNetwork =
  fromMaybe (error "Invalid Network in database")
    . netByName
    . cs
    . dBAccountNetwork

accountWallet :: DBAccount -> Fingerprint
accountWallet acc =
  let (DBWalletKey walletFP) = dBAccountWallet acc
   in fromRight (error "Invalid WalletId in database") $
        textToFingerprint walletFP

accountXPubKey :: Ctx -> DBAccount -> XPubKey
accountXPubKey ctx acc =
  fromMaybe (error "Invalid XPubKey in database") $
    xPubImport (accountNetwork acc) ctx (dBAccountXPubKey acc)

nextAccountDeriv :: (MonadUnliftIO m) => Fingerprint -> Network -> DB m Natural
nextAccountDeriv walletFP net = do
  let walletId = DBWalletKey $ fingerprintToText walletFP
  idxs <-
    select . from $ \a -> do
      where_ $
        a ^. DBAccountNetwork ==. val (cs net.name)
          &&. a ^. DBAccountWallet ==. val walletId
      return $ a ^. DBAccountIndex
  return $ smallestUnused $ fromIntegral . unValue <$> idxs

smallestUnused :: [Natural] -> Natural
smallestUnused xs = fromJust $ find (not . (`elem` xs)) [0 ..]

existsAccount :: (MonadUnliftIO m) => Text -> DB m Bool
existsAccount name = P.existsBy $ UniqueName name

existsXPubKey :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> DB m Bool
existsXPubKey net ctx key = P.existsBy $ UniqueXPubKey $ xPubExport net ctx key

getWalletOrCreate :: (MonadUnliftIO m) => Fingerprint -> DB m DBWalletId
getWalletOrCreate fp = do
  let key = DBWalletKey $ fingerprintToText fp
  walletM <- P.get key
  case walletM of
    Just _ -> return key
    _ -> P.insert $ DBWallet $ fingerprintToText fp

insertAccount ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Fingerprint ->
  Text ->
  XPubKey ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
insertAccount net ctx walletFP name xpub = do
  existsName <- lift $ existsAccount name
  if existsName
    then throwError $ "Account " <> cs name <> " already exists"
    else do
      existsKey <- lift $ existsXPubKey net ctx xpub
      if existsKey
        then throwError "The XPubKey already exists"
        else do
          time <- liftIO getCurrentTime
          walletId <- lift $ getWalletOrCreate walletFP
          let path = bip44Deriv net $ fromIntegral $ xPubChild xpub
              idx = fromIntegral $ xPubIndex xpub
              account =
                DBAccount
                  { dBAccountName = name,
                    dBAccountWallet = walletId,
                    dBAccountIndex = idx,
                    dBAccountNetwork = cs net.name,
                    dBAccountDerivation = cs $ pathToStr path,
                    dBAccountExternal = 0,
                    dBAccountInternal = 0,
                    dBAccountXPubKey = xPubExport net ctx xpub,
                    dBAccountBalanceConfirmed = 0,
                    dBAccountBalanceUnconfirmed = 0,
                    dBAccountBalanceCoins = 0,
                    dBAccountCreated = time
                  }
          key <- lift $ P.insert account
          return (key, account)

deleteAccount :: (MonadUnliftIO m) => DBAccountId -> DB m ()
deleteAccount accId@(DBAccountKey accWallet accDeriv) = do
  delete . from $ \a ->
    where_ $
      a ^. DBAddressAccountWallet ==. val accWallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
  delete . from $ \t ->
    where_ $
      t ^. DBTxInfoAccountWallet ==. val accWallet
        &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
  delete . from $ \c ->
    where_ $
      c ^. DBCoinAccountWallet ==. val accWallet
        &&. c ^. DBCoinAccountDerivation ==. val accDeriv
  delete . from $ \p ->
    where_ $
      p ^. DBPendingTxAccountWallet ==. val accWallet
        &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
  P.delete accId

-- When a name is provided, get that account or throw an error if it doesn't
-- exist. When no name is provided, return the account only if there is one
-- account.
getAccountByName ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
getAccountByName (Just name) = do
  aM <- lift $ P.getBy $ UniqueName name
  case aM of
    Just a -> return (entityKey a, entityVal a)
    _ -> throwError $ "The account " <> cs name <> " does not exist"
getAccountByName Nothing = do
  as <- lift getAccounts
  case as of
    [a] -> return a
    [] -> throwError "There are no accounts in the wallet"
    _ -> throwError "Specify which account to use"

getAccountById ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) DBAccount
getAccountById accId = liftMaybe "Invalid account" =<< lift (P.get accId)

getAccounts :: (MonadUnliftIO m) => DB m [(DBAccountId, DBAccount)]
getAccounts =
  (go <$>)
    <$> P.selectList
      []
      [P.Asc DBAccountWallet, P.Asc DBAccountNetwork, P.Asc DBAccountIndex]
  where
    go a = (entityKey a, entityVal a)

getAccountNames :: (MonadUnliftIO m) => DB m [Text]
getAccountNames = do
  res <- select . from $ \a -> do
    orderBy [asc $ a ^. DBAccountCreated]
    return $ a ^. DBAccountName
  return $ unValue <$> res

renameAccount ::
  (MonadUnliftIO m) => Text -> Text -> ExceptT String (DB m) DBAccount
renameAccount oldName newName
  | oldName == newName = throwError "Old and new names are the same"
  | otherwise = do
      e <- lift $ existsAccount newName
      if e
        then throwError $ "The account " <> cs newName <> " already exists"
        else do
          c <-
            lift . updateCount $ \a -> do
              set a [DBAccountName =. val newName]
              where_ $ a ^. DBAccountName ==. val oldName
          if c == 0
            then throwError $ "The account " <> cs oldName <> " does not exist"
            else snd <$> getAccountByName (Just newName)

updateAccountBalances :: (MonadUnliftIO m) => DBAccountId -> DB m DBAccount
updateAccountBalances accId@(DBAccountKey wallet accDeriv) = do
  confirm <- selectSum DBAddressBalanceConfirmed
  unconfirm <- selectSum DBAddressBalanceUnconfirmed
  coins <- selectSum DBAddressBalanceCoins
  update $ \a -> do
    set
      a
      [ DBAccountBalanceConfirmed =. val (unpack confirm),
        DBAccountBalanceUnconfirmed =. val (unpack unconfirm),
        DBAccountBalanceCoins =. val (unpack coins)
      ]
    where_ $ a ^. DBAccountId ==. val accId
  fromJust <$> P.get accId
  where
    unpack m = fromMaybe 0 $ unValue $ fromMaybe (Value $ Just 0) m
    selectSum field =
      selectOne . from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return $ sum_ $ a ^. field

{- Addresses -}

data AddrType = AddrInternal | AddrExternal
  deriving (Eq, Show)

isInternal :: AddrType -> Bool
isInternal AddrInternal = True
isInternal AddrExternal = False

dBAccountCount :: AddrType -> DBAccount -> Int
dBAccountCount AddrInternal = dBAccountInternal
dBAccountCount AddrExternal = dBAccountExternal

dBAccountField :: AddrType -> EntityField DBAccount Int
dBAccountField AddrInternal = DBAccountInternal
dBAccountField AddrExternal = DBAccountExternal

addrDeriv :: AddrType -> SoftPath
addrDeriv AddrInternal = intDeriv
addrDeriv AddrExternal = extDeriv

data AddrFree = AddrFree | AddrBusy
  deriving (Eq, Show)

isAddrFree :: AddrFree -> Bool
isAddrFree AddrFree = True
isAddrFree AddrBusy = False

instance ToJSON DBAddress where
  toJSON addr =
    object
      [ "index" .= dBAddressIndex addr,
        "wallet" .= dBAddressAccountWallet addr,
        "account" .= dBAddressAccountDerivation addr,
        "derivation" .= dBAddressDerivation addr,
        "address" .= dBAddressAddress addr,
        "label" .= dBAddressLabel addr,
        "balance"
          .= AddressBalance
            (dBAddressBalanceConfirmed addr)
            (dBAddressBalanceUnconfirmed addr)
            (dBAddressBalanceCoins addr)
            (dBAddressBalanceTxs addr)
            (dBAddressBalanceReceived addr),
        "internal" .= dBAddressInternal addr,
        "free" .= dBAddressFree addr,
        "created" .= dBAddressCreated addr
      ]

instance FromJSON DBAddress where
  parseJSON =
    withObject "DBAddress" $ \o -> do
      bal <- o .: "balance"
      DBAddress
        <$> o .: "index"
        <*> o .: "wallet"
        <*> o .: "account"
        <*> o .: "derivation"
        <*> o .: "address"
        <*> o .: "label"
        <*> bal .: "confirmed"
        <*> bal .: "unconfirmed"
        <*> bal .: "coins"
        <*> bal .: "txs"
        <*> bal .: "received"
        <*> o .: "internal"
        <*> o .: "free"
        <*> o .: "created"

data AddressBalance = AddressBalance
  { -- | confirmed balance
    addrBalanceConfirmed :: !Word64,
    -- | unconfirmed balance
    addrBalanceUnconfirmed :: !Word64,
    -- | number of unspent outputs
    addrBalanceCoins :: !Word64,
    -- | number of transactions
    addrBalanceTxs :: !Word64,
    -- | total amount from all outputs in this address
    addrBalanceReceived :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AddressBalance where
  toJSON b =
    object
      [ "confirmed" .= addrBalanceConfirmed b,
        "unconfirmed" .= addrBalanceUnconfirmed b,
        "coins" .= addrBalanceCoins b,
        "txs" .= addrBalanceTxs b,
        "received" .= addrBalanceReceived b
      ]

instance FromJSON AddressBalance where
  parseJSON =
    withObject "accountbalance" $ \o ->
      AddressBalance
        <$> o .: "confirmed"
        <*> o .: "unconfirmed"
        <*> o .: "coins"
        <*> o .: "txs"
        <*> o .: "received"

updateAddressBalances ::
  (MonadUnliftIO m) =>
  Network ->
  [Store.Balance] ->
  ExceptT String (DB m) ()
updateAddressBalances net storeBals =
  forM_ storeBals $ \s -> do
    addrT <-
      liftEither $ maybeToEither "Invalid Address" (addrToText net s.address)
    lift . update $ \a -> do
      set
        a
        [ DBAddressBalanceConfirmed =. val s.confirmed,
          DBAddressBalanceUnconfirmed =. val s.unconfirmed,
          DBAddressBalanceCoins =. val s.utxo,
          DBAddressBalanceTxs =. val s.txs,
          DBAddressBalanceReceived =. val s.received
        ]
      where_ $ a ^. DBAddressAddress ==. val addrT

getAddressByText ::
  (MonadUnliftIO m) =>
  Text ->
  ExceptT String (DB m) (DBAccount, DBAddress)
getAddressByText addrTxt = do
  resM <- lift $ P.getBy $ UniqueAddress addrTxt
  case resM of
    Just (Entity _ addr) -> do
      let wallet = dBAddressAccountWallet addr
          deriv = dBAddressAccountDerivation addr
          accId = DBAccountKey wallet deriv
      accM <- lift $ P.get accId
      case accM of
        Just acc -> return (acc, addr)
        _ -> throwError "Account does not exist"
    _ -> throwError "Address does not exist"

insertAddress ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  SoftPath ->
  Address ->
  AddrFree ->
  ExceptT String (DB m) DBAddress
insertAddress net (DBAccountKey wallet accDeriv) deriv addr free = do
  time <- liftIO getCurrentTime
  addrT <- liftEither $ maybeToEither "Invalid Address" (addrToText net addr)
  let label = if isIntPath deriv then "Internal Address" else ""
      derivS = cs $ pathToStr deriv
      dbAddr =
        DBAddress
          (fromIntegral $ pathIndex deriv)
          wallet
          accDeriv
          derivS
          addrT
          label
          0
          0
          0
          0
          0
          (isIntPath deriv)
          (isAddrFree free)
          time
  lift $ P.insert_ dbAddr
  return dbAddr

-- This is an internal function
genNextAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  DBAccountId ->
  AddrType ->
  AddrFree ->
  Bool ->
  ExceptT String (DB m) DBAddress
genNextAddress ctx cfg accId addrType addrFree testGap = do
  acc <- getAccountById accId
  let net = accountNetwork acc
      pub = accountXPubKey ctx acc
      nextIdx = dBAccountCount addrType acc
      deriv = addrDeriv addrType
  when testGap $ checkGap cfg accId nextIdx addrType
  let (addr, _) = derivePathAddr ctx pub deriv (fromIntegral nextIdx)
  dbAddr <-
    insertAddress net accId (deriv :/ fromIntegral nextIdx) addr addrFree
  lift $ P.update accId [dBAccountField addrType P.=. nextIdx + 1]
  return dbAddr

checkGap ::
  (MonadUnliftIO m) =>
  Config ->
  DBAccountId ->
  Int ->
  AddrType ->
  ExceptT String (DB m) ()
checkGap cfg accId addrIdx addrType = do
  let gap = configGap cfg
  usedIdxM <- lift $ bestAddrWithFunds accId addrType
  let usedIdx = maybe 0 (+ 1) usedIdxM
  when (addrIdx >= usedIdx + fromIntegral gap) $
    throwError $
      "Can not generate addresses beyond the gap of " <> show gap

-- Highest address with a positive transaction count
bestAddrWithFunds ::
  (MonadUnliftIO m) => DBAccountId -> AddrType -> DB m (Maybe Int)
bestAddrWithFunds (DBAccountKey wallet accDeriv) addrType = do
  resM <- (flatMaybe <$>) . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val (isInternal addrType)
        &&. a ^. DBAddressBalanceTxs >. val 0
    return $ max_ $ a ^. DBAddressIndex
  return $ fromIntegral <$> resM

-- Generate the discovered external and internal addresses
discoverAccGenAddrs ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  DBAccountId ->
  AddrType ->
  Int ->
  ExceptT String (DB m) ()
discoverAccGenAddrs ctx cfg accId addrType newAddrCnt = do
  acc <- getAccountById accId
  let oldAddrCnt = dBAccountCount addrType acc
      cnt = max 0 $ newAddrCnt - oldAddrCnt
  -- False: Don't check the gap while discovering
  replicateM_ cnt $ genNextAddress ctx cfg accId addrType AddrBusy False

genExtAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  DBAccountId ->
  Text ->
  ExceptT String (DB m) DBAddress
genExtAddress ctx cfg accId label = do
  -- True: check the gap
  addr <- genNextAddress ctx cfg accId AddrExternal AddrBusy True
  setAddrLabel accId (dBAddressIndex addr) label

setAddrLabel ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Int ->
  Text ->
  ExceptT String (DB m) DBAddress
setAddrLabel accId@(DBAccountKey wallet accDeriv) idx label = do
  acc <- getAccountById accId
  let path = cs $ pathToStr $ extDeriv :/ fromIntegral idx
      aKey = DBAddressKey wallet accDeriv path
  unless (fromIntegral idx < dBAccountExternal acc) $
    throwError $
      "Address " <> show idx <> " does not exist"
  lift $ P.updateGet aKey [DBAddressLabel P.=. label]

nextFreeIntAddr ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  DBAccountId ->
  ExceptT String (DB m) DBAddress
nextFreeIntAddr ctx cfg accId@(DBAccountKey wallet accDeriv) = do
  resM <- lift . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val True
        &&. a ^. DBAddressFree ==. val True
    orderBy [asc $ a ^. DBAddressIndex]
    limit 1
    return a
  case resM of
    Just (Entity _ a) -> return a
    -- True: Check the gap
    Nothing -> genNextAddress ctx cfg accId AddrInternal AddrFree True

fromDBAddr :: Network -> DBAddress -> Either String (Address, SoftPath)
fromDBAddr net addrDB = do
  let addrT = dBAddressAddress addrDB
      derivT = dBAddressDerivation addrDB
  deriv <- maybeToEither "fromDBAddress deriv" $ parseSoft $ cs derivT
  addr <- maybeToEither "fromDBAddress addr" $ textToAddr net addrT
  return (addr, deriv)

setAddrsFree :: (MonadUnliftIO m) => AddrFree -> [Text] -> DB m Natural
setAddrsFree free addrs = do
  (fromIntegral <$>) . updateCount $ \a -> do
    set a [DBAddressFree =. val (isAddrFree free)]
    where_ $
      a ^. DBAddressAddress `in_` valList addrs
        &&. (a ^. DBAddressBalanceTxs ==. val 0)

addressPage :: (MonadUnliftIO m) => DBAccountId -> Page -> DB m [DBAddress]
addressPage (DBAccountKey wallet accDeriv) (Page lim off) = do
  as <-
    select $
      from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
            &&. a ^. DBAddressInternal ==. val False
        orderBy [desc (a ^. DBAddressIndex)]
        limit $ fromIntegral lim
        offset $ fromIntegral off
        return a
  return $ entityVal <$> as

allAddressesMap ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  ExceptT String (DB m) (Map Address SoftPath, Map Address AddressBalance)
allAddressesMap net (DBAccountKey wallet accDeriv) = do
  dbRes <-
    lift . select $
      from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return a
  res <-
    forM dbRes $ \(Entity _ dbAddr) -> do
      a <- liftEither $ textToAddrE net $ dBAddressAddress dbAddr
      d <-
        liftEither . maybeToEither "parsePath failed" $
          parseSoft . cs $
            dBAddressDerivation dbAddr
      let b =
            AddressBalance
              (dBAddressBalanceConfirmed dbAddr)
              (dBAddressBalanceUnconfirmed dbAddr)
              (dBAddressBalanceCoins dbAddr)
              (dBAddressBalanceTxs dbAddr)
              (dBAddressBalanceReceived dbAddr)
      return ((a, d), (a, b))
  return (Map.fromList $ fst <$> res, Map.fromList $ snd <$> res)

getCoinDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Store.Unspent ->
  DB m (Either String SoftPath)
getCoinDeriv net accId unspent =
  runExceptT $ do
    addr <-
      liftEither . maybeToEither "getCoinDeriv: no address" $ unspent.address
    liftEither <=< lift $ getAddrDeriv net accId addr

getAddrDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Address ->
  DB m (Either String SoftPath)
getAddrDeriv net (DBAccountKey wallet accDeriv) addr =
  runExceptT $ do
    addrT <-
      liftEither . maybeToEither "getAddrDeriv: no address" $ addrToText net addr
    derivM <-
      lift . selectOne . from $ \a -> do
        where_ $
          a ^. DBAddressAddress ==. val addrT
            &&. a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return $ a ^. DBAddressDerivation
    liftEither . maybeToEither "getAddrDeriv: no derivation" $
      parseSoft . cs . unValue =<< derivM

-- | The addresses that belong to the account, with their derivation paths
walletPaths ::
  (MonadUnliftIO m) =>
  Network -> DBAccountId -> [Address] -> ExceptT String (DB m) (Map Address SoftPath)
walletPaths net accId addrs =
  Map.fromList . catMaybes <$> lift (mapM pathOf (nub addrs))
  where
    pathOf addr = either (const Nothing) (\path -> Just (addr, path)) <$> getAddrDeriv net accId addr

{- Transactions -}

-- | The block of every live transaction summary of an account, by
-- transaction id. The dead ones are left out: the sync treats them as
-- unknown, so that a transaction the store lists again is fetched and comes
-- back to life.
txInfoBlockRefs ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) (Map TxHash Store.BlockRef)
txInfoBlockRefs (DBAccountKey wallet accDeriv) = do
  rows <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccountWallet ==. val wallet
          &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
          &&. t ^. DBTxInfoDead ==. val False
      return (t ^. DBTxInfoTxid, t ^. DBTxInfoBlockRef)
  entries <- forM rows $ \(Value tidT, Value refBytes) -> do
    tid <- liftEither $ maybeToEither "txInfoBlockRefs: invalid txid" $ hexToTxHash tidT
    ref <- liftEither $ S.decode refBytes
    return (tid, ref)
  return $ Map.fromList entries

-- | Mark the summary of a transaction dead: the store no longer has it, or
-- reports it replaced or dropped. The summary stays, for the history.
markTxInfoDead :: (MonadUnliftIO m) => DBAccountId -> TxHash -> DB m ()
markTxInfoDead (DBAccountKey wallet accDeriv) tid =
  P.update (DBTxInfoKey wallet accDeriv (txHashToHex tid)) [DBTxInfoDead P.=. True]

getConfirmedTxs ::
  (MonadUnliftIO m) => DBAccountId -> Bool -> ExceptT String (DB m) [TxHash]
getConfirmedTxs (DBAccountKey wallet accDeriv) confirm = do
  ts <-
    lift . select $
      from $ \t -> do
        where_ $
          t ^. DBTxInfoAccountWallet ==. val wallet
            &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
            &&. t ^. DBTxInfoConfirmed ==. val confirm
        orderBy [asc (t ^. DBTxInfoBlockRef)]
        return $ t ^. DBTxInfoTxid
  forM ts $ \(Value t) ->
    liftEither $ maybeToEither "getConfirmedTxs invalid TxHash" $ hexToTxHash t

-- Insert a new transaction or replace it, if it already exists
-- Returns True if there was a change or an insert
repsertTxInfo ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxInfo ->
  ExceptT String (DB m) (DBTxInfo, Bool)
repsertTxInfo net ctx accId txInfo = do
  time <- liftIO getCurrentTime
  tid <- liftEither $ maybeToEither "TxId" $ txInfoHash txInfo
  let confirmed' = Store.confirmed $ txInfoBlockRef txInfo
      tidT = txHashToHex tid
      bRef = S.encode $ txInfoBlockRef txInfo
      -- Confirmations will get updated when retrieving them
      blob =
        BS.toStrict $
          marshalJSON
            (net, ctx)
            txInfo
              { txInfoConfirmations = 0,
                txInfoPending = Nothing,
                txInfoDead = False
              }
      (DBAccountKey wallet accDeriv) = accId
      key = DBTxInfoKey wallet accDeriv tidT
      dbInfo =
        DBTxInfo
          { dBTxInfoAccountWallet = wallet,
            dBTxInfoAccountDerivation = accDeriv,
            dBTxInfoTxid = tidT,
            dBTxInfoBlockRef = bRef,
            dBTxInfoConfirmed = confirmed',
            dBTxInfoBlob = blob,
            dBTxInfoDead = False,
            dBTxInfoCreated = time
          }
  prevM <- lift $ P.get key
  case prevM of
    Just prev -> do
      -- A live transaction from the store: a summary marked dead comes back
      let newDBInfo =
            prev
              { dBTxInfoBlockRef = bRef,
                dBTxInfoConfirmed = confirmed',
                dBTxInfoBlob = blob,
                dBTxInfoDead = False
              }
      lift $ P.replace key newDBInfo
      return (newDBInfo, prev /= newDBInfo)
    Nothing -> do
      lift $ P.insert_ dbInfo
      return (dbInfo, True)

txsPage ::
  (MonadUnliftIO m) =>
  Ctx ->
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [TxInfo]
txsPage ctx accId@(DBAccountKey wallet accDeriv) (Page lim off) = do
  acc <- getAccountById accId
  let net = accountNetwork acc
  dbTxs <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccountWallet ==. val wallet
          &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
      orderBy [asc (t ^. DBTxInfoBlockRef)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return (t ^. DBTxInfoBlob, t ^. DBTxInfoDead)
  res <-
    forM dbTxs $ \(Value dbTx, Value dead) -> do
      txInfo <-
        liftMaybe "TxInfo unmarshalJSON Failed" $
          unmarshalJSON (net, ctx) $
            BS.fromStrict dbTx
      return txInfo {txInfoDead = dead}
  resLabels <- lift $ mapM (fillTxInfoLabels net) res
  bestM <- lift $ getBest net
  return $ updateConfirmations (snd <$> bestM) <$> resLabels
  where
    updateConfirmations bestM tif =
      tif {txInfoConfirmations = getConfirmations bestM (txInfoBlockRef tif)}

getConfirmations :: Maybe BlockHeight -> Store.BlockRef -> Natural
getConfirmations _ (Store.MemRef _) = 0
getConfirmations Nothing (Store.BlockRef _ _) = 1
getConfirmations (Just best) (Store.BlockRef height _)
  | best < height = 1
  | otherwise = fromIntegral $ best - height + 1

fillTxInfoLabels :: (MonadUnliftIO m) => Network -> TxInfo -> DB m TxInfo
fillTxInfoLabels net txInfo = do
  o <- mapM fillOutput $ Map.assocs $ txInfoMyOutputs txInfo
  i <- mapM fillInput $ Map.assocs $ txInfoMyInputs txInfo
  return $
    txInfo
      { txInfoMyOutputs = Map.fromList o,
        txInfoMyInputs = Map.fromList i
      }
  where
    fillOutput (a, o) = do
      resM <- runMaybeT $ do
        addrT <- hoistMaybe $ addrToText net a
        (Entity _ dbAddr) <- hoistMaybe =<< lift (P.getBy $ UniqueAddress addrT)
        return (a, o {myOutputsLabel = dBAddressLabel dbAddr})
      return $ fromMaybe (a, o) resM
    fillInput (a, i) = do
      resM <- runMaybeT $ do
        addrT <- hoistMaybe $ addrToText net a
        (Entity _ dbAddr) <- hoistMaybe =<< lift (P.getBy $ UniqueAddress addrT)
        return (a, i {myInputsLabel = dBAddressLabel dbAddr})
      return $ fromMaybe (a, i) resM

{- Coins -}

data JsonCoin = JsonCoin
  { jsonCoinOutpoint :: !OutPoint,
    jsonCoinAddress :: !Address,
    jsonCoinValue :: !Word64,
    jsonCoinBlock :: !Store.BlockRef,
    jsonCoinConfirmations :: !Natural,
    jsonCoinLocked :: !Bool
  }
  deriving (Eq, Show)

instance MarshalJSON Network JsonCoin where
  marshalValue net c =
    object
      [ "outpoint" .= jsonCoinOutpoint c,
        "address" .= marshalValue net (jsonCoinAddress c),
        "value" .= jsonCoinValue c,
        "block" .= jsonCoinBlock c,
        "confirmations" .= jsonCoinConfirmations c,
        "locked" .= jsonCoinLocked c
      ]
  unmarshalValue net =
    withObject "JsonCoin" $ \o ->
      JsonCoin
        <$> o .: "outpoint"
        <*> (unmarshalValue net =<< o .: "address")
        <*> o .: "value"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "locked"

-- | A coin as the user sees it, locked when one of the pending
-- transactions spends it
toJsonCoin :: Network -> Maybe BlockHeight -> Set OutPoint -> DBCoin -> Either String JsonCoin
toJsonCoin net bestM locked dbCoin = do
  op <- textToOutpoint $ dBCoinOutpoint dbCoin
  ad <-
    maybeToEither "toJsonCoin: Invalid address" $
      textToAddr net $
        dBCoinAddress dbCoin
  br <- S.decode $ dBCoinBlockRef dbCoin
  let confirmations = getConfirmations bestM br
  return $
    JsonCoin
      { jsonCoinOutpoint = op,
        jsonCoinAddress = ad,
        jsonCoinValue = dBCoinValue dbCoin,
        jsonCoinBlock = br,
        jsonCoinConfirmations = confirmations,
        jsonCoinLocked = op `Set.member` locked
      }

outpointText :: OutPoint -> Text
outpointText = encodeHex . S.encode

textToOutpoint :: Text -> Either String OutPoint
textToOutpoint t = do
  bs <- maybeToEither "textToOutpoint: invalid input" $ decodeHex t
  S.decode bs

-- Get all coins in an account, spendable or not
coinPage ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [JsonCoin]
coinPage net accId@(DBAccountKey wallet accDeriv) (Page lim off) = do
  locked <- lockedOutpoints accId
  coins <-
    lift . select . from $ \c -> do
      where_ $ do
        c ^. DBCoinAccountWallet ==. val wallet
          &&. c ^. DBCoinAccountDerivation ==. val accDeriv
      orderBy [asc (c ^. DBCoinBlockRef), desc (c ^. DBCoinCreated)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return c
  bestM <- lift $ getBest net
  mapM (liftEither . toJsonCoin net (snd <$> bestM) locked . entityVal) coins

-- Spendable coins must be confirmed and not locked
getSpendableCoins ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Natural ->
  ExceptT String (DB m) [Store.Unspent]
getSpendableCoins net accId@(DBAccountKey wallet accDeriv) minConf = do
  bestM <- lift $ getBest net
  locked <- lockedOutpoints accId
  coins <- lift . select . from $ \c -> do
    where_ $
      c ^. DBCoinAccountWallet ==. val wallet
        &&. c ^. DBCoinAccountDerivation ==. val accDeriv
    return c
  let f c = do
        ref <- liftEither $ S.decode $ dBCoinBlockRef c
        return $ getConfirmations (snd <$> bestM) ref >= minConf
  confirmedCoins <- filterM (f . entityVal) coins
  unspents <- mapM (liftEither . S.decode . dBCoinBlob . entityVal) confirmedCoins
  return $ filter (\unspent -> unspent.outpoint `Set.notMember` locked) unspents

insertCoin ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Text ->
  Store.Unspent ->
  DB m DBCoin
insertCoin (DBAccountKey wallet accDeriv) addr unspent = do
  time <- liftIO getCurrentTime
  let newCoin =
        DBCoin
          { dBCoinAccountWallet = wallet,
            dBCoinAccountDerivation = accDeriv,
            dBCoinOutpoint = outpointText unspent.outpoint,
            dBCoinAddress = addr,
            dBCoinValue = unspent.value,
            dBCoinBlockRef = S.encode unspent.block,
            dBCoinBlob = S.encode unspent,
            dBCoinConfirmed = Store.confirmed unspent.block,
            dBCoinCreated = time
          }
  P.insert_ newCoin
  return newCoin

updateCoin :: (MonadUnliftIO m) => Store.Unspent -> DB m ()
updateCoin unspent = do
  let key = DBCoinKey $ outpointText unspent.outpoint
  P.update
    key
    [ DBCoinBlob P.=. S.encode unspent,
      DBCoinConfirmed P.=. Store.confirmed unspent.block,
      DBCoinBlockRef P.=. S.encode unspent.block
    ]

deleteCoin :: (MonadUnliftIO m) => DBCoin -> DB m ()
deleteCoin coin =
  delete . from $ \c ->
    where_ $
      c ^. DBCoinOutpoint ==. val (dBCoinOutpoint coin)

getCoinsByAddr :: (MonadUnliftIO m) => Text -> DB m [DBCoin]
getCoinsByAddr addr = do
  coins <- select . from $ \c -> do
    where_ $ c ^. DBCoinAddress ==. val addr
    return c
  return $ entityVal <$> coins

-- This is the main coin function
-- Either insert, update or delete coins as required. Returns the number of
-- coins that have either been inserted, updated or deleted.
refreshCoins ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  [Address] ->
  [Store.Unspent] ->
  ExceptT String (DB m) (Int, [DBCoin])
refreshCoins net accId addrsToUpdate allUnspent = do
  let storeMap = groupCoins net allUnspent
  addrsToUpdateE <-
    mapM (liftEither . maybeToEither "Addr" . addrToText net) addrsToUpdate
  res <- forM addrsToUpdateE $ \addr -> do
    let storeCoins = fromMaybe [] $ Map.lookup addr storeMap
    localCoins <- lift $ getCoinsByAddr addr
    let storeOps = outpointText . (.outpoint) <$> storeCoins
        localOps = dBCoinOutpoint <$> localCoins
        toDelete = filter ((`notElem` storeOps) . dBCoinOutpoint) localCoins
        toInsert =
          filter ((`notElem` localOps) . outpointText . (.outpoint)) storeCoins
        toUpdate = filter (f localCoins) storeCoins
    lift $ forM_ toDelete deleteCoin
    lift $ forM_ toUpdate updateCoin
    newCoins <- lift $ forM toInsert $ insertCoin accId addr
    return (length toDelete + length toUpdate + length toInsert, newCoins)
  return (sum $ fst <$> res, concatMap snd res)
  where
    f localCoins s =
      let cM = find ((== outpointText s.outpoint) . dBCoinOutpoint) localCoins
       in case cM of
            Just c -> dBCoinBlockRef c /= S.encode s.block
            _ -> False

groupCoins :: Network -> [Store.Unspent] -> Map Text [Store.Unspent]
groupCoins net =
  Map.fromListWith (<>) . mapMaybe f
  where
    f x =
      case x.address of
        Just a -> (,[x]) <$> addrToText net a
        _ -> Nothing

{- Raw Transactions -}

insertRawTx :: (MonadUnliftIO m) => Tx -> DB m ()
insertRawTx tx = do
  let hash = txHashToHex $ txHash tx
      key = DBRawTxKey hash
  P.repsert key $ DBRawTx hash (S.encode tx)

getRawTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) Tx
getRawTx hash = do
  let key = DBRawTxKey $ txHashToHex hash
  txM <- lift $ P.get key
  case txM of
    Just tx -> liftEither . S.decode $ dBRawTxBlob tx
    Nothing -> throwError "getRawTx: missing transaction"

-- | Whether the raw transaction is stored
hasRawTx :: (MonadUnliftIO m) => TxHash -> DB m Bool
hasRawTx hash = isJust <$> P.get (DBRawTxKey $ txHashToHex hash)

-- | Those of the transactions whose raw transaction is not stored
missingRawTxs :: (MonadUnliftIO m) => [TxHash] -> DB m [TxHash]
missingRawTxs = filterM (fmap not . hasRawTx)

{- Pending Transactions -}

data TxOnline = TxOnline | TxOffline
  deriving (Eq, Show)

-- | A pending transaction by its nosigHash: its account, its signing data
-- and whether the user sent it to the network
getPendingTx ::
  (MonadUnliftIO m) =>
  TxHash ->
  DB m (Maybe (DBAccountId, TxSignData, Bool))
getPendingTx pendingHash = do
  let hashT = txHashToHex pendingHash
      key = DBPendingTxKey hashT
  resM <- P.get key
  case resM of
    Just (DBPendingTx wallet accDeriv _ blob online _) -> do
      let accId = DBAccountKey wallet accDeriv
          tsdM = Json.decode $ BS.fromStrict blob
      return $ do
        tsd <- tsdM
        return (accId, tsd, online)
    _ -> return Nothing

pendingTxPage ::
  (MonadUnliftIO m) =>
  Ctx ->
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [TxInfo]
pendingTxPage ctx accId@(DBAccountKey wallet accDeriv) (Page lim off) = do
  acc <- getAccountById accId
  let pub = accountXPubKey ctx acc
      net = accountNetwork acc
  tsds <-
    lift . select . from $ \p -> do
      where_ $ do
        p ^. DBPendingTxAccountWallet ==. val wallet
          &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
      orderBy [desc (p ^. DBPendingTxCreated)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return p
  forM tsds $ \(Entity _ res) -> do
    tsd <- liftEither . Json.eitherDecode . BS.fromStrict $ dBPendingTxBlob res
    txInfo <- liftEither $ parseTxSignData net ctx pub tsd
    conflicted <- lift $ conflictedInputs txInfo
    let pending =
          TxInfoPending (nosigHash $ txSignDataTx tsd) (txSignDataSigned tsd) (dBPendingTxOnline res) (not (null conflicted))
    let txInfoP = txInfo {txInfoPending = Just pending}
    lift $ fillTxInfoLabels net txInfoP

-- | A pending transaction as the sync and the coin locks see it
data PendingTx = PendingTx
  { pendingTxKey :: !DBPendingTxId,
    pendingTxData :: !TxSignData,
    -- | Sent to the network by the user
    pendingTxOnline :: !Bool
  }

-- | The pending transactions of an account
pendingTxs :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) [PendingTx]
pendingTxs (DBAccountKey wallet accDeriv) = do
  rows <-
    lift . select . from $ \p -> do
      where_ $ do
        p ^. DBPendingTxAccountWallet ==. val wallet
          &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
      return (p ^. DBPendingTxId, p ^. DBPendingTxBlob, p ^. DBPendingTxOnline)
  forM rows $ \(Value key, Value blob, Value online) -> do
    tsd <- liftEither . Json.eitherDecode $ BS.fromStrict blob
    return PendingTx {pendingTxKey = key, pendingTxData = tsd, pendingTxOnline = online}

-- | The coins locked by the pending transactions of an account: every
-- outpoint one of them spends. A coin is locked exactly as long as a
-- pending transaction spends it; nothing else records it.
lockedOutpoints :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) (Set OutPoint)
lockedOutpoints accId = do
  pending <- pendingTxs accId
  return $ Set.fromList [input.outpoint | p <- pending, input <- (txSignDataTx (pendingTxData p)).inputs]

-- | The coins of the wallet that a transaction spends and that are gone,
-- spent by another transaction: the transaction can no longer be sent
conflictedInputs :: (MonadUnliftIO m) => TxInfo -> DB m [OutPoint]
conflictedInputs txInfo = filterM missing outpoints
  where
    (outpoints, _, _) = parseTxInfoU txInfo
    missing outpoint = Maybe.isNothing <$> P.get (DBCoinKey $ outpointText outpoint)

-- Imports a pending transaction, locks coins and locks internal addresses
importPendingTx ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxSignData ->
  ExceptT String (DB m) TxHash
importPendingTx net ctx accId tsd@(TxSignData tx _ _ _ _ signed) = do
  acc <- getAccountById accId
  let pub = accountXPubKey ctx acc
      pendingHash = nosigHash $ txSignDataTx tsd
      bs = BS.toStrict $ Json.encode tsd
  prevM <- lift $ getPendingTx pendingHash
  case prevM of
    Just (_, TxSignData prevTx _ _ _ _ prevSigned, online) -> do
      when online $
        throwError "The transaction is already online"
      when (prevSigned && not signed) $
        throwError "Can not replace a signed transaction with an unsigned one"
      when (prevTx == tx) $
        throwError "The transaction already exists"
      when (not prevSigned && signed) $ do
        let key = DBPendingTxKey $ txHashToHex pendingHash
        lift $ P.update key [DBPendingTxBlob P.=. bs]
      return pendingHash
    Nothing -> do
      txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
      -- A pending transaction is retired when the sync sees it under one
      -- of the wallet's addresses; one touching none would wait forever
      unless (involvesWallet txInfoU) $
        throwError "The transaction does not involve this wallet"
      let (outpoints, outIntAddrs, restAddrs) = parseTxInfoU txInfoU
      -- The coins it spends must exist and be free of any other pending
      -- transaction; storing it is what locks them
      locked <- lockedOutpoints accId
      forM_ outpoints $ \outpoint -> do
        coinM <- lift $ P.get $ DBCoinKey $ outpointText outpoint
        when (Maybe.isNothing coinM) $
          throwError "A coin referenced by the transaction does not exist"
        when (outpoint `Set.member` locked) $
          throwError "A coin referenced by the transaction is locked"
      -- Verify addresses and set internal output addresses to busy
      outIntAddrsT <- mapM (liftMaybe "Addrs" . addrToText net . fst) outIntAddrs
      restAddrsT <- mapM (liftMaybe "Addrs" . addrToText net) restAddrs
      outIntAddrsE <- lift . select . from $ \a -> do
        where_ $ a ^. DBAddressAddress `in_` valList outIntAddrsT
        return a
      restAddrsE <- lift . select . from $ \a -> do
        where_ $ a ^. DBAddressAddress `in_` valList restAddrsT
        return a
      when
        ( length (outIntAddrsT <> restAddrsT)
            /= length (outIntAddrsE <> restAddrsE)
        )
        $ throwError "Some referenced addresses do not exist"
      unless (all (dBAddressFree . entityVal) outIntAddrsE) $
        throwError "Some of the internal output addresses are not free"
      -- Set the output internal addresses to not free
      _ <- lift $ setAddrsFree AddrBusy outIntAddrsT
      -- Insert the pending transaction
      time <- liftIO getCurrentTime
      let ptx =
            DBPendingTx
              (dBAccountWallet acc)
              (dBAccountDerivation acc)
              (txHashToHex pendingHash)
              bs
              False
              time
      lift $ P.insert_ ptx
      return pendingHash

-- (Outpoints, external addrs, internal addrs)
parseTxInfoU :: TxInfo -> ([OutPoint], [(Address, KeyIndex)], [Address])
parseTxInfoU TxInfo {..} =
  (outpoints, nub $ f <$> outIntAddrs, nub $ fst <$> restAddrs)
  where
    outpoints = (.outpoint) <$> concatMap myInputsSigInput (Map.elems txInfoMyInputs)
    outAddrs = second myOutputsPath <$> Map.assocs txInfoMyOutputs
    inAddrs = second myInputsPath <$> Map.assocs txInfoMyInputs
    (outIntAddrs, outExtAddrs) = partition (isIntPath . snd) outAddrs
    restAddrs = outExtAddrs <> inAddrs
    f (a, p) =
      case pathToList p of
        (_ : i : _) -> (a, i)
        _ -> error "parseTxInfoU"

-- Delete a pending transaction, unlocks coins and frees internal addresses
deletePendingTx ::
  (MonadUnliftIO m) =>
  Ctx ->
  TxHash ->
  ExceptT String (DB m) (Natural, Natural)
-- | Delete a pending transaction, whatever its state: the coins it spent
-- are free again by its absence, and its change address is freed. Returns
-- how many coins of the wallet it was holding and how many addresses were
-- freed. Whether a transaction the user sent may be deleted is for the
-- command to decide.
deletePendingTx ctx pendingHash = do
  let key = DBPendingTxKey $ txHashToHex pendingHash
  tsdM <- lift $ getPendingTx pendingHash
  case tsdM of
    Just (accId, tsd, _) -> do
      acc <- getAccountById accId
      let net = accountNetwork acc
          pub = accountXPubKey ctx acc
      txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
      let (outpoints, outIntAddrs, _) = parseTxInfoU txInfoU
      outIntAddrsT <-
        mapM (liftMaybe "Address" . addrToText net . fst) outIntAddrs
      held <- lift $ filterM (fmap isJust . P.get . DBCoinKey . outpointText) outpoints
      freedAddresses <- lift $ setAddrsFree AddrFree outIntAddrsT
      lift $ P.delete key
      return (fromIntegral (length held), fromIntegral freedAddresses)
    _ -> throwError "The pending transaction does not exist"

-- When the pending transaction is online, we just delete it
deletePendingTxOnline :: (MonadUnliftIO m) => DBPendingTxId -> DB m ()
deletePendingTxOnline = P.delete

setPendingTxOnline :: (MonadUnliftIO m) => TxHash -> DB m Natural
setPendingTxOnline nosigH = do
  let nosigHT = txHashToHex nosigH
  cnt <- updateCount $ \p -> do
    set p [DBPendingTxOnline =. val True]
    where_ $ p ^. DBPendingTxNosigHash ==. val nosigHT
  return $ fromIntegral cnt

{- Helpers -}

extDeriv :: SoftPath
extDeriv = Deriv :/ 0

intDeriv :: SoftPath
intDeriv = Deriv :/ 1

isExtPath :: SoftPath -> Bool
isExtPath p =
  case pathToList p of
    [0, _] -> True
    _ -> False

isIntPath :: SoftPath -> Bool
isIntPath p =
  case pathToList p of
    [1, _] -> True
    _ -> False

pathIndex :: SoftPath -> KeyIndex
pathIndex p =
  case pathToList p of
    [_, i] -> i
    _ -> error "Invalid pathIndex"

addrPath :: DBAddress -> SoftPath
addrPath addr = f :/ fromIntegral (dBAddressIndex addr)
  where
    f =
      if dBAddressInternal addr
        then intDeriv
        else extDeriv

bip44Deriv :: Network -> Natural -> HardPath
bip44Deriv net a = Deriv :| 44 :| net.bip44Coin :| fromIntegral a

xPubIndex :: XPubKey -> Natural
xPubIndex = fromIntegral . xPubChild

addrsDerivPage :: Ctx -> SoftPath -> Page -> XPubKey -> [(Address, SoftPath)]
addrsDerivPage ctx deriv (Page lim off) xpub =
  fmap (\(a, _, i) -> (a, deriv :/ i)) addrs
  where
    addrs =
      take (fromIntegral lim) $
        derivePathAddrs ctx xpub deriv (fromIntegral off)

-- like `maybe` but for a maybe/value sandwich
joinMaybe :: b -> (a -> b) -> Maybe (E.Value (Maybe a)) -> b
joinMaybe d f m =
  case m of
    Just (Value m') -> maybe d f m'
    Nothing -> d

flatMaybe :: Maybe (E.Value (Maybe a)) -> Maybe a
flatMaybe = joinMaybe Nothing Just
