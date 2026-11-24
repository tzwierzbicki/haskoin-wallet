{-# LANGUAGE OverloadedRecordDot #-}

-- | The best block of each network the wallet synced with: what the
-- confirmations are counted from, and what the lock time of a new
-- transaction is drawn from. Also the two blocks each pool keeps of its
-- last sync: the block the sync reached, which tells that pool whether the
-- chain moved under it, and a settled block below it, which tells whether
-- the store is still on the chain of that sync at all.
module Haskoin.Wallet.Database.BestBlock
  ( BestBlock (..),
    getBest,
    bestHeight,
    updateBest,
    SyncedBlock (..),
    syncedBlock,
    settledBlock,
    settledDepth,
    settledHeightBelow,
    poolEverSynced,
    updateSyncedBlock,
    getConfirmations,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (forM)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Data.Maybe (isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
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

{- The blocks of a pool -}

-- | A block of the chain a pool synced with, by its hash and its height
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
syncedBlock = storedBlock dBSyncedBlockBlock dBSyncedBlockHeight

-- | The settled block of a pool: the block of the main chain that stood
-- 'settledDepth' blocks below the one its last sync reached, or nothing
-- before its first sync. It is stored with the block the sync reached, and
-- refused like it when it does not read.
--
-- The block a sync reached is at the tip of the chain, where a
-- reorganisation replaces blocks every so often: a store that no longer
-- has it tells nothing more than that. No reorganisation reaches the
-- settled block. A store whose main chain does not hold it is behind the
-- wallet by more than a reorganisation, or on another chain, and the sync
-- refuses to follow it (@chainSeen@ of "Haskoin.Wallet.Sync").
--
-- hw pins no list of blocks in its code for this, as a node of Bitcoin
-- Cash does: such a list is old by the time it ships, and tells only that
-- a store is on another network. The block the wallet saw by itself is
-- always recent, on every network, and needs no upkeep.
settledBlock :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) (Maybe SyncedBlock)
settledBlock = storedBlock dBSyncedBlockSettledBlock dBSyncedBlockSettledHeight

-- | How far below the block a sync reached the settled block of a pool
-- stands: a hundred blocks, the depth at which the rules of the chain
-- themselves take a block for settled, since a coinbase can be spent from
-- there.
settledDepth :: BlockHeight
settledDepth = 100

-- | The height of the settled block below a block at the height given:
-- 'settledDepth' blocks below, or the first block of a chain that is
-- shorter
settledHeightBelow :: BlockHeight -> BlockHeight
settledHeightBelow height = height - min height settledDepth

-- | One of the two blocks the row of a pool holds, read by its columns
storedBlock :: (MonadUnliftIO m) => (DBSyncedBlock -> Text) -> (DBSyncedBlock -> Int) -> Pool -> ExceptT String (DB m) (Maybe SyncedBlock)
storedBlock hashOf heightOf pool = do
  rowM <- lift $ P.get (syncedBlockKey pool)
  forM rowM $ \row ->
    liftMaybe "The block the last sync of the portfolio reached does not read: sync --full stores it again" $
      SyncedBlock <$> hexToBlockHash (hashOf row) <*> word32 (heightOf row)

-- | Whether a sync of the pool ever finished. Before the first one, hw
-- has asked no server about the addresses of the pool, and what the
-- database says of them, no transaction and no coin, is not what the
-- chain says: the views of an address then say that it is not synced
-- (the type t'Haskoin.Wallet.Database.Address.AddressFunds'). An offline
-- computer stays there for good.
--
-- It asks whether the pool has a block, and does not read it: a view
-- has no use for the block, and must not fail on one that does not read
-- ('syncedBlock').
poolEverSynced :: (MonadUnliftIO m) => Pool -> DB m Bool
poolEverSynced pool = isJust <$> P.get (syncedBlockKey pool)

-- | Store the block a sync of a pool reached and its settled block, in
-- place of the ones before: the sync does so once it is done
updateSyncedBlock :: (MonadUnliftIO m) => Pool -> SyncedBlock -> SyncedBlock -> DB m ()
updateSyncedBlock pool reached settled =
  P.repsert (syncedBlockKey pool) $
    DBSyncedBlock
      { dBSyncedBlockPortfolio = pool.poolPortfolio,
        dBSyncedBlockNetwork = poolNetworkName pool,
        dBSyncedBlockBlock = blockHashToHex reached.syncedHash,
        dBSyncedBlockHeight = fromIntegral reached.syncedHeight,
        dBSyncedBlockSettledBlock = blockHashToHex settled.syncedHash,
        dBSyncedBlockSettledHeight = fromIntegral settled.syncedHeight
      }

-- | The row of the blocks of a pool
syncedBlockKey :: Pool -> P.Key DBSyncedBlock
syncedBlockKey pool = DBSyncedBlockKey pool.poolPortfolio (poolNetworkName pool)

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
