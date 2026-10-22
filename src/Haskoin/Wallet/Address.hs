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

-- | The addresses of an account: external ones to receive with and
-- internal ones for change, generated along the derivation paths of the
-- account, with their labels, balances and whether an internal one is
-- free to be used.
module Haskoin.Wallet.Address where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Data.Aeson
import Data.List (find, nub, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Util (Page (Page), textToAddrE)
import Numeric.Natural (Natural)

{- Kinds of addresses -}

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

{- Addresses in JSON -}

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

{- Addresses in the database -}

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

{- Derivation paths -}

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

-- | The address of a row with its derivation path
fromDBAddr :: Network -> DBAddress -> Either String (Address, SoftPath)
fromDBAddr net addrDB = do
  let addrT = dBAddressAddress addrDB
      derivT = dBAddressDerivation addrDB
  deriv <- maybeToEither "fromDBAddress deriv" $ parseSoft $ cs derivT
  addr <- maybeToEither "fromDBAddress addr" $ textToAddr net addrT
  return (addr, deriv)

addrsDerivPage :: Ctx -> SoftPath -> Page -> XPubKey -> [(Address, SoftPath)]
addrsDerivPage ctx deriv (Page lim off) xpub =
  fmap (\(a, _, i) -> (a, deriv :/ i)) addrs
  where
    addrs =
      take (fromIntegral lim) $
        derivePathAddrs ctx xpub deriv (fromIntegral off)
