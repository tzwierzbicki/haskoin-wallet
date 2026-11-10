{-# LANGUAGE OverloadedRecordDot #-}

-- | The best block of each network the wallet synced with: what the
-- confirmations are counted from, and what the lock time of a new
-- transaction is drawn from. Also the block the last sync of each pool
-- reached, which tells that pool whether the chain moved under it.
module Haskoin.Wallet.Database.BestBlock
  ( BestBlock (..),
    getBest,
    bestHeight,
    updateBest,
    SyncedBlock (..),
    syncedBlock,
    updateSyncedBlock,
    getConfirmations,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (forM)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Data.String.Conversions (cs)
import Data.Word (Word32)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database
import Numeric.Natural (Natural)

{- The best block in the database -}

-- | The best block of a network as the last sync left it
data BestBlock = BestBlock
  { blockHash :: !BlockHash,
    blockHeight :: !BlockHeight,
    -- | When it was mined, by its header, in seconds of Unix time. None for
    -- a block stored by an hw that kept no time, until the next sync.
    blockTime :: !(Maybe Word32)
  }
  deriving (Eq, Show)

-- | The best block of a network as the last sync left it, or nothing
-- before the first sync. A stored block that does not read is refused, as
-- every row that does not read. Every sync stores the block again.
getBest :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Maybe BestBlock)
getBest net = do
  rowM <- lift $ P.get (bestKey net)
  forM rowM $ \row ->
    liftMaybe ("The stored best block of " <> net.name <> " does not read: sync stores it again") $ do
      hash <- hexToBlockHash (dBBestBestBlock row)
      height <- word32 (dBBestBestHeight row)
      time <- traverse word32 (dBBestBestTime row)
      return $ BestBlock hash height time

-- | A height or a time, which a column holds as an integer
word32 :: Int -> Maybe Word32
word32 stored
  | stored >= 0 && stored <= fromIntegral (maxBound :: Word32) = Just (fromIntegral stored)
  | otherwise = Nothing

-- | The height of the best block of a network, which the confirmations are
-- counted from
bestHeight :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Maybe BlockHeight)
bestHeight net = fmap (.blockHeight) <$> getBest net

-- | Store the best block of a network, with the time its header gives, in
-- place of the one before: the sync does so once it is done
updateBest :: (MonadUnliftIO m) => Network -> BlockHash -> BlockHeight -> Word32 -> DB m ()
updateBest net hash height time =
  P.repsert (bestKey net) $
    DBBest
      { dBBestNetwork = cs net.name,
        dBBestBestBlock = blockHashToHex hash,
        dBBestBestHeight = fromIntegral height,
        dBBestBestTime = Just (fromIntegral time)
      }

-- | The row of the best block of a network, keyed by the network's name
bestKey :: Network -> P.Key DBBest
bestKey net = DBBestKey (cs net.name)

{- The block of a pool -}

-- | The block the last sync of a pool reached
data SyncedBlock = SyncedBlock
  { syncedHash :: !BlockHash,
    syncedHeight :: !BlockHeight
  }
  deriving (Eq, Show)

-- | The block the last sync of a pool reached, or nothing before its
-- first sync. A stored block that does not read is refused, as every row
-- that does not read: taking it for no block at all would hide why the
-- sync looks at everything. A full sync makes no such check and stores the
-- block again.
syncedBlock :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) (Maybe SyncedBlock)
syncedBlock pool = do
  rowM <- lift $ P.get (DBSyncedBlockKey pool.poolPortfolio (poolNetworkName pool))
  forM rowM $ \row ->
    liftMaybe "The block the last sync of the portfolio reached does not read: sync --full stores it again" $
      SyncedBlock <$> hexToBlockHash (dBSyncedBlockBlock row) <*> word32 (dBSyncedBlockHeight row)

-- | Store the block a sync of a pool reached, in place of the one
-- before: the sync does so once it is done
updateSyncedBlock :: (MonadUnliftIO m) => Pool -> BlockHash -> BlockHeight -> DB m ()
updateSyncedBlock pool hash height =
  P.repsert (DBSyncedBlockKey pool.poolPortfolio (poolNetworkName pool)) $
    DBSyncedBlock
      { dBSyncedBlockPortfolio = pool.poolPortfolio,
        dBSyncedBlockNetwork = poolNetworkName pool,
        dBSyncedBlockBlock = blockHashToHex hash,
        dBSyncedBlockHeight = fromIntegral height
      }

{- Confirmations -}

-- | The confirmations of a transaction or a coin at a block, counted from
-- the best block: none in the mempool, one at the best block itself, and
-- one when the best block is unknown or behind. They are counted as
-- natural numbers, which no height can overflow.
getConfirmations :: Maybe BlockHeight -> Store.BlockRef -> Natural
getConfirmations _ (Store.MemRef _) = 0
getConfirmations Nothing (Store.BlockRef _ _) = 1
getConfirmations (Just best) (Store.BlockRef height _)
  | best < height = 1
  | otherwise = fromIntegral best - fromIntegral height + 1
