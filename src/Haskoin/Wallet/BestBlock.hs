{-# LANGUAGE OverloadedRecordDot #-}

-- | The best block of each network the wallet synced with: where the last
-- sync left off, and what the confirmations are counted from.
module Haskoin.Wallet.BestBlock
  ( getBest,
    bestHeight,
    updateBest,
    getConfirmations,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (forM)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Data.String.Conversions (cs)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database
import Numeric.Natural (Natural)

{- The best block in the database -}

-- | The best block of a network as the last sync left it, or nothing
-- before the first sync. A stored block that does not read is refused, as
-- every row that does not read: taking it for no block at all would skip
-- the check for a reorganisation of the chain that an incremental sync
-- makes. A full sync makes no such check and stores the block again.
getBest :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Maybe (BlockHash, BlockHeight))
getBest net = do
  rowM <- lift $ P.get (bestKey net)
  forM rowM $ \row ->
    liftMaybe ("The stored best block of " <> net.name <> " does not read: sync --full stores it again") $ do
      hash <- hexToBlockHash (dBBestBestBlock row)
      height <- blockHeight (dBBestBestHeight row)
      return (hash, height)
  where
    blockHeight :: Int -> Maybe BlockHeight
    blockHeight stored
      | stored >= 0 && stored <= fromIntegral (maxBound :: BlockHeight) = Just (fromIntegral stored)
      | otherwise = Nothing

-- | The height of the best block of a network, which the confirmations are
-- counted from
bestHeight :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Maybe BlockHeight)
bestHeight net = fmap snd <$> getBest net

-- | Store the best block of a network, in place of the one before: the sync
-- does so once it is done
updateBest :: (MonadUnliftIO m) => Network -> BlockHash -> BlockHeight -> DB m ()
updateBest net hash height =
  P.repsert (bestKey net) $
    DBBest
      { dBBestNetwork = cs net.name,
        dBBestBestBlock = blockHashToHex hash,
        dBBestBestHeight = fromIntegral height
      }

-- | The row of the best block of a network, keyed by the network's name
bestKey :: Network -> P.Key DBBest
bestKey net = DBBestKey (cs net.name)

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
