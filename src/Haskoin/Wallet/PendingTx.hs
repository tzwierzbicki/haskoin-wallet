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

-- | The pending transactions of an account, from preparation to the
-- network: stored with their signing data, holding the coins they spend,
-- signed in place, sent and watched until the sync retires them. The
-- commands of hw are thin layers over the functions here.
module Haskoin.Wallet.PendingTx where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Arrow (Arrow (second))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.List (find, nub, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.Database
import Haskoin.Wallet.RawTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (parseTxSignData, verifySignedTx)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

{- Summaries of pending transactions -}

-- | What a transaction of the wallet touches, from its summary: the coins
-- it spends, its internal addresses, which are change, with their
-- indices, and its other addresses of the wallet, paid or spent from
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

-- | The summary with its pending transaction marked online
setTxInfoOnline :: TxInfo -> TxInfo
setTxInfoOnline txInfo =
  case txInfoPending txInfo of
    Just p -> txInfo {txInfoPending = Just p {pendingOnline = True}}
    _ -> txInfo

{- Pending transactions in the database -}

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

-- | Delete a pending transaction, whatever its state: the coins it spent
-- are free again by its absence, and its change address is freed. Returns
-- how many coins of the wallet it was holding and how many addresses were
-- freed. Whether a transaction the user sent may be deleted is for the
-- command to decide.
deletePendingTx ::
  (MonadUnliftIO m) =>
  Ctx ->
  TxHash ->
  ExceptT String (DB m) (Natural, Natural)
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

{- Sending and abandoning -}

-- | Send a pending transaction of the wallet and wait for the store to see
-- it. Once it does, the transaction is marked online, for the sync to send
-- again should the store lose it, and its raw transaction is stored. If the
-- store does not see it, nothing is recorded and the answer says so.
sendPending ::
  (MonadUnliftIO m) => Ctx -> (Network -> Store) -> TxHash -> ExceptT String (DB m) (DBAccount, TxInfo)
sendPending ctx storeOn nosigH = do
  tsdM <- lift $ getPendingTx nosigH
  case tsdM of
    Just (accId, tsd@(TxSignData signedTx _ _ _ _ signed), _) -> do
      acc <- getAccountById accId
      let net = accountNetwork acc
          pub = accountXPubKey ctx acc
          store = storeOn net
      txInfo <- liftEither $ parseTxSignData net ctx pub tsd
      txInfoL <- lift $ fillTxInfoLabels net txInfo
      unless signed $ throwError "The transaction is not signed"
      liftEither $ verifySignedTx net ctx tsd
      fromStore $ storeHealth store
      accepted <- fromStore $ broadcast store signedTx
      unless accepted $ throwError notAccepted
      _ <- lift $ setPendingTxOnline nosigH
      lift $ insertRawTx signedTx
      return (acc, setTxInfoOnline txInfoL)
    _ -> throwError "The nosigHash does not exist in the wallet"

-- | Delete a pending transaction, freeing the coins it held. One the user
-- sent is abandoned only when the store does not know it: a transaction in
-- the network's mempool or in the chain may still confirm, and the sync
-- retires it once it is listed.
deleteTx ::
  (MonadUnliftIO m) => Ctx -> (Network -> Store) -> TxHash -> ExceptT String (DB m) (Natural, Natural)
deleteTx ctx storeOn nosigH = do
  pendingM <- lift $ getPendingTx nosigH
  case pendingM of
    Nothing -> throwError "The pending transaction does not exist"
    Just (accId, tsd, online) -> do
      when online $ do
        acc <- getAccountById accId
        let txid = txHash (txSignDataTx tsd)
        known <- fromStore $ storeTransactions (storeOn (accountNetwork acc)) [txid]
        when (any (\t -> t.txid == txid && not t.deleted) known) $
          throwError "The transaction is in the network's mempool or in the chain: syncacc retires it once it is listed"
      freed <- deletePendingTx ctx nosigH
      -- The raw transaction stored when it was sent goes with it, unless a
      -- coin of the wallet comes from it after all
      when online $ void $ lift $ dropRawTxsWithoutCoins [txHash (txSignDataTx tsd)]
      return freed
