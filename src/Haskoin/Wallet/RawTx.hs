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

-- | The raw transactions the coins come from, which signing needs to know
-- what each input spends. A raw transaction is kept as long as a coin of
-- the wallet, of any account, comes from it. The sync drops the parents of
-- the coins that are gone and the transactions it retires, a full sync
-- drops every one without a coin, and the abandon of a sent transaction
-- drops it, once no coin comes from them.
module Haskoin.Wallet.RawTx where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Database

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

-- | Delete the raw transaction, if it is stored
deleteRawTx :: (MonadUnliftIO m) => TxHash -> DB m ()
deleteRawTx hash = P.delete (DBRawTxKey $ txHashToHex hash)

-- | Drop those of the given raw transactions that no coin comes from, and
-- say how many went
dropRawTxsWithoutCoins :: (MonadUnliftIO m) => [TxHash] -> DB m Int
dropRawTxsWithoutCoins hashes = do
  unused <- filterM (fmap not . coinComesFrom) hashes
  forM_ unused deleteRawTx
  return $ length unused

-- | Whether a coin of the wallet, of any account, is an output of the
-- transaction. A coin is stored under the hex of its outpoint, the bytes of
-- the hash first, so the coins of a transaction share a prefix.
coinComesFrom :: (MonadUnliftIO m) => TxHash -> DB m Bool
coinComesFrom hash = do
  let prefix = encodeHex (S.encode hash)
  found <- selectOne . from $ \c -> do
    where_ $ c ^. DBCoinOutpoint `like` (val prefix ++. (%))
    return $ c ^. DBCoinOutpoint
  return $ isJust found

-- | Drop every raw transaction that no coin comes from, and say how many
-- went: the sweep of a full sync, which also cleans up after the versions
-- of hw that kept every raw transaction
dropOrphanRawTxs :: (MonadUnliftIO m) => ExceptT String (DB m) Int
dropOrphanRawTxs = do
  outpoints <- lift . select . from $ \c -> return (c ^. DBCoinOutpoint)
  parents <- liftEither $ mapM (fmap (.hash) . textToOutpoint . unValue) outpoints
  stored <- lift . select . from $ \r -> return (r ^. DBRawTxHash)
  hashes <- liftEither $ mapM (maybeToEither "dropOrphanRawTxs: invalid hash" . hexToTxHash . unValue) stored
  let orphans = [hash | hash <- hashes, hash `Set.notMember` Set.fromList parents]
  lift $ forM_ orphans deleteRawTx
  return $ length orphans
