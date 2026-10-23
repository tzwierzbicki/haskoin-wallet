{-# LANGUAGE OverloadedRecordDot #-}

-- | The best block of each network the wallet synced with: where the last
-- sync left off, and what the confirmations are counted from.
module Haskoin.Wallet.BestBlock where

import Conduit (MonadUnliftIO)
import Data.String.Conversions (cs)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database
import Numeric.Natural (Natural)

{- The best block in the database -}

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

{- Confirmations -}

-- | The confirmations of a transaction or a coin at a block, counted from
-- the best block: none in the mempool, one at the best block itself, and
-- one when the best block is unknown or behind
getConfirmations :: Maybe BlockHeight -> Store.BlockRef -> Natural
getConfirmations _ (Store.MemRef _) = 0
getConfirmations Nothing (Store.BlockRef _ _) = 1
getConfirmations (Just best) (Store.BlockRef height _)
  | best < height = 1
  | otherwise = fromIntegral $ best - height + 1
