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
-- what each input spends.
module Haskoin.Wallet.RawTx where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Serialize as S
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
