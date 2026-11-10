{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The labels the user gives the transactions of a portfolio on a
-- network, by pool and txid. The sync never touches them, so a summary
-- it rewrites or marks dead keeps its label. A pending transaction keeps
-- its label in its signing data instead, which its file carries, and the
-- label moves here once the network has the transaction: see
-- 'Haskoin.Wallet.Database.PendingTx.retirePendingTx'.
module Haskoin.Wallet.Database.TxLabel where

import Conduit (MonadUnliftIO)
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Database

-- | The key of the label of a transaction of the pool
txLabelKey :: Pool -> TxHash -> P.Key DBTxLabel
txLabelKey pool tid = DBTxLabelKey pool.poolPortfolio (poolNetworkName pool) (hashBytes tid)

-- | The label of a transaction of the pool, or none
txLabel :: (MonadUnliftIO m) => Pool -> TxHash -> DB m Text
txLabel pool tid = maybe "" dBTxLabelLabel <$> P.get (txLabelKey pool tid)

-- | Every label of a transaction of the pool, by transaction, in the
-- order of the ids: what exportwallet writes
poolTxLabels :: (MonadUnliftIO m) => Pool -> DB m [(TxHash, Text)]
poolTxLabels pool = do
  rows <- P.selectList [DBTxLabelPortfolio P.==. pool.poolPortfolio, DBTxLabelNetwork P.==. poolNetworkName pool] [P.Asc DBTxLabelTxid]
  return $ rights [(\tid -> (tid, dBTxLabelLabel row)) <$> bytesToHash (dBTxLabelTxid row) | P.Entity _ row <- rows]

-- | Give a transaction of the pool a label, in place of the one it had.
-- An empty label removes it.
setTxLabel :: (MonadUnliftIO m) => Pool -> TxHash -> Text -> DB m ()
setTxLabel pool tid label
  | Text.null label = P.delete key
  | otherwise = P.repsert key (DBTxLabel pool.poolPortfolio (poolNetworkName pool) (hashBytes tid) label)
  where
    key = txLabelKey pool tid
