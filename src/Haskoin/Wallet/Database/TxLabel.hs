{-# LANGUAGE OverloadedStrings #-}

-- | The labels the user gives the transactions of an account on the
-- network, by account and txid. The sync never touches them, so a summary
-- it rewrites or marks dead keeps its label. A pending transaction keeps
-- its label in its signing data instead, which its file carries, and the
-- label moves here once the network has the transaction: see
-- 'Haskoin.Wallet.Database.PendingTx.retirePendingTx'.
module Haskoin.Wallet.Database.TxLabel where

import Conduit (MonadUnliftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Database

-- | The label of a transaction of the account, or none
txLabel :: (MonadUnliftIO m) => DBAccountId -> TxHash -> DB m Text
txLabel accId tid = maybe "" dBTxLabelLabel <$> P.get (DBTxLabelKey accId (hashBytes tid))

-- | Give a transaction of the account a label, in place of the one it had.
-- An empty label removes it.
setTxLabel :: (MonadUnliftIO m) => DBAccountId -> TxHash -> Text -> DB m ()
setTxLabel accId tid label
  | Text.null label = P.delete key
  | otherwise = P.repsert key (DBTxLabel accId (hashBytes tid) label)
  where
    key = DBTxLabelKey accId (hashBytes tid)
