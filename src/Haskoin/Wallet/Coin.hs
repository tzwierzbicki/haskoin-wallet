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

-- | The coins of an account, the unspent outputs the store knows at its
-- addresses: kept up to date by the sync, listed for the user and offered
-- to the transaction builder when they are confirmed enough and not held
-- by a pending transaction.
module Haskoin.Wallet.Coin where

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
import qualified Data.Serialize as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.PendingTx
import Haskoin.Wallet.Util (Page (Page), textToAddrE)
import Numeric.Natural (Natural)

{- Coins as the user sees them -}

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

-- | The outpoint of a coin row
coinOutpoint :: DBCoin -> Either String OutPoint
coinOutpoint = textToOutpoint . dBCoinOutpoint

-- | A coin as the user sees it, locked when one of the pending
-- transactions spends it
toJsonCoin :: Network -> Maybe BlockHeight -> Set OutPoint -> DBCoin -> Either String JsonCoin
toJsonCoin net bestM locked dbCoin = do
  op <- coinOutpoint dbCoin
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

-- | The coins of the store by the text of their address
groupCoins :: Network -> [Store.Unspent] -> Map Text [Store.Unspent]
groupCoins net =
  Map.fromListWith (<>) . mapMaybe f
  where
    f x =
      case x.address of
        Just a -> (,[x]) <$> addrToText net a
        _ -> Nothing

{- Coins in the database -}

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

-- | What a refresh changed: how many coins were inserted, updated or
-- deleted, and the coins inserted and deleted
data Refreshed = Refreshed
  { refreshedCount :: !Int,
    insertedCoins :: ![DBCoin],
    deletedCoins :: ![DBCoin]
  }
  deriving (Eq, Show)

-- | Bring the coins of the addresses in line with what the store lists:
-- insert the coins that are new, update those whose block changed, delete
-- those the store no longer lists
refreshCoins ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  [Address] ->
  [Store.Unspent] ->
  ExceptT String (DB m) Refreshed
refreshCoins net accId addrsToUpdate allUnspent = do
  let storeMap = groupCoins net allUnspent
  addrsToUpdateE <-
    mapM (liftEither . maybeToEither "Addr" . addrToText net) addrsToUpdate
  changes <- forM addrsToUpdateE $ \addr -> do
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
    return
      Refreshed
        { refreshedCount = length toDelete + length toUpdate + length toInsert,
          insertedCoins = newCoins,
          deletedCoins = toDelete
        }
  return
    Refreshed
      { refreshedCount = sum $ refreshedCount <$> changes,
        insertedCoins = concatMap insertedCoins changes,
        deletedCoins = concatMap deletedCoins changes
      }
  where
    f localCoins s =
      let cM = find ((== outpointText s.outpoint) . dBCoinOutpoint) localCoins
       in case cM of
            Just c -> dBCoinBlockRef c /= S.encode s.block
            _ -> False
