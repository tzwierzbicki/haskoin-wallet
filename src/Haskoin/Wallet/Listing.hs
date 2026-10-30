{-# LANGUAGE ExistentialQuantification #-}

-- | A listing of hw: accounts, addrs, balances, coins, txs and pendingtxs.
-- On the screen, its rows come from the database in chunks. Each chunk is
-- read in a transaction of its own, and holds the rows after the last row
-- of the chunk before. A long listing starts to show at once this way, and
-- the database is free while the user reads. A chunk starts after a
-- cursor: what orders the listing, taken from the row before it. A script
-- reads a listing the same way with --after and --max, which name the row
-- to start after and how many rows to read.
module Haskoin.Wallet.Listing
  ( Paging (..),
    Listing (..),
    chunkRows,
    readListing,
    streamListing,
  )
where

import Control.Monad (join, void, when)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.List.NonEmpty as NonEmpty
import Data.String.Conversions (cs)
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database (DB, catchDatabaseInUse, runDB)
import Haskoin.Wallet.Response (Response (..))
import Numeric.Natural (Natural)

-- | What the command line asks of a listing: the rows after the one it
-- names, or the first ones, and how many at most, or all of them. The row
-- is named as the listing shows it: by its address, its coin, its
-- transaction id or its nosigHash.
data Paging after = Paging
  { pagingAfter :: !(Maybe after),
    pagingMax :: !(Maybe Natural)
  }
  deriving (Eq, Show)

-- | A listing, with rows and a cursor of its own kind, which the code
-- that reads it does not see
data Listing = forall cursor row.
  Listing
  { -- | Where to start: after the row --after names, or at the first row
    listingStart :: Maybe cursor,
    -- | How many rows at most, or all of them
    listingMax :: Maybe Natural,
    -- | The rows after the cursor, or the first ones, up to the count, or
    -- all of them, in one query
    listingRows :: Maybe cursor -> Maybe Natural -> ExceptT String (DB IO) [row],
    -- | The cursor of a row, after which the next chunk starts, or why
    -- the row has none
    listingCursor :: row -> Either String cursor,
    -- | The rows as the answer of the command: every row for --json, a
    -- chunk of them for the screen
    listingAnswer :: [row] -> Response
  }

-- | How many rows a chunk of the screen holds: more than a screen shows,
-- few enough to read in an instant
chunkRows :: Natural
chunkRows = 100

-- | Read the listing whole, in one transaction, as the answer --json
-- writes: every row --max allows, after the row --after names
readListing :: Config -> ExceptT String (DB IO) Listing -> IO Response
readListing cfg build = do
  result <- readOnce cfg $ do
    Listing start most rows _ answer <- build
    answer <$> rows start most
  return $ either (ResponseError . cs) id result

-- | Read the listing chunk by chunk, each chunk in a transaction of its
-- own, and pass each one to the action as an answer, telling the first
-- chunk apart: an empty first chunk is an empty listing. The reading stops
-- when the rows run out, when --max rows were read, or when the action
-- answers False, as it does once the pager is closed. An empty chunk after
-- the first one is not passed on.
streamListing :: Config -> ExceptT String (DB IO) Listing -> (Bool -> Response -> IO Bool) -> IO (Either String ())
streamListing cfg build onChunk = do
  built <- readOnce cfg build
  case built of
    Left err -> return (Left err)
    Right (Listing start most rows cursorOf answer) -> do
      let chunk first cursor left = do
            let count = maybe chunkRows (min chunkRows) left
            got <- readOnce cfg (rows cursor (Just count))
            case got of
              Left err -> return (Left err)
              Right [] -> do
                when first $ void $ onChunk True (answer [])
                return (Right ())
              Right (found : others) -> do
                continue <- onChunk first (answer (found : others))
                let taken = fromIntegral (length (found : others))
                    left' = subtract taken <$> left
                if not continue || taken < count || left' == Just 0
                  then return (Right ())
                  else case cursorOf (NonEmpty.last (found NonEmpty.:| others)) of
                    Left err -> return (Left err)
                    Right next -> chunk False (Just next) left'
      chunk True start most

-- | Run a reading of the database in one transaction: its answer, its
-- failure, or that of a database held by another command
readOnce :: Config -> ExceptT String (DB IO) a -> IO (Either String a)
readOnce cfg action = join <$> catchDatabaseInUse (runDB cfg (runExceptT action))
