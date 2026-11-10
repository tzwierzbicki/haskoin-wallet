{-# LANGUAGE ExistentialQuantification #-}

-- | A listing of hw: portfolios, addresses, balances, coins, txs and
-- pendingtxs. On the screen, its rows come from the database in chunks.
-- Each chunk is read in a transaction of its own, and holds the rows after
-- the last row of the chunk before. A long listing starts to show at once
-- this way, and the database is free while the user reads. A chunk starts
-- after a cursor: what orders the listing, taken from the row before it.
-- A script reads a listing the same way with --after and --max, which
-- name the row to start after and how many rows to read.
--
-- A listing over a portfolio comes in sections ('Sections'): one for each
-- network, or for each account for the addresses. Each section is a
-- listing of its own ('Listing'), with its own rows and its own cursor,
-- and they are read one after the other. Merging the sections into one
-- order was rejected: the rows of two networks have nothing to be ordered
-- by together, and a query over several accounts could not use the index
-- that makes a chunk fast. A section without rows shows nothing. --max
-- counts the rows of all the sections together, and --after starts in the
-- section that holds the row it names, which the code that builds the
-- listing finds.
--
-- A listing of few rows, which reads all of them for any chunk anyway, is
-- read whole instead, in one chunk ('listingWhole'). Its table can then
-- size a column to its widest cell, which a listing in chunks cannot: a
-- later chunk would not line up with the first. portfolios and pendingtxs
-- are read so.
module Haskoin.Wallet.Cli.Listing
  ( Paging (..),
    Listing (..),
    Sections (..),
    ChunkAt (..),
    oneSection,
    chunkRows,
    readListing,
    streamListing,
    wholeListing,
  )
where

import Control.Monad (join)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.List.NonEmpty as NonEmpty
import Data.String.Conversions (cs)
import Haskoin.Wallet.Cli.Response (Response (..), joinListings)
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database (DB, catchDatabaseInUse, runDB)
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

-- | A section of a listing, with rows and a cursor of its own kind, which
-- the code that reads it does not see
data Listing = forall cursor row.
  Listing
  { -- | Where to start: after the row --after names, or at the first row
    listingStart :: Maybe cursor,
    -- | The rows after the cursor, or the first ones, up to the count, or
    -- all of them, in one query
    listingRows :: Maybe cursor -> Maybe Natural -> ExceptT String (DB IO) [row],
    -- | The cursor of a row, after which the next chunk starts, or why
    -- the row has none
    listingCursor :: row -> Either String cursor,
    -- | The rows as the answer of the command: every row for --json, a
    -- chunk of them for the screen. No row gives an answer that lists
    -- nothing, without a group for the section.
    listingAnswer :: [row] -> Response,
    -- | Whether the screen reads every row in one chunk, as the first one
    listingWhole :: Bool
  }

-- | A listing, as the sections it is read in, in their order
data Sections = Sections
  { -- | Whether each section says where it is on the screen: a listing
    -- that may cover several places. A listing the options narrow to one
    -- place does not repeat it.
    sectionsTitled :: Bool,
    -- | How many rows at most, over all the sections, or all of them
    sectionsMax :: Maybe Natural,
    -- | The sections, the first of which starts after the row --after
    -- names, if any. There is at least one.
    sectionsListings :: [Listing]
  }

-- | A listing of one section, which names no place: the rows at most,
-- and the section
oneSection :: Maybe Natural -> Listing -> Sections
oneSection most listing = Sections False most [listing]

-- | Where a chunk is in the listing the screen reads, which tells what
-- comes above its rows
data ChunkAt = ChunkAt
  { -- | Whether it is the first chunk the listing shows: nothing is above
    -- it. The chunk of a listing without rows is its first.
    chunkFirst :: !Bool,
    -- | Whether it is the first chunk of its section, which starts with
    -- the title and the header of the section
    chunkStartsSection :: !Bool,
    -- | Whether the sections of the listing say where they are
    chunkTitled :: !Bool
  }
  deriving (Eq, Show)

-- | How many rows a chunk of the screen holds: more than a screen shows,
-- few enough to read in an instant
chunkRows :: Natural
chunkRows = 100

-- | Read the listing whole, in one transaction, as the answer --json
-- writes: every row --max allows, after the row --after names
readListing :: Config -> ExceptT String (DB IO) Sections -> IO Response
readListing cfg build = either (ResponseError . cs) id <$> readOnce cfg (wholeListing build)

-- | Every row --max allows, after the row --after names, as the answer of
-- the command, read in the transaction the caller runs: exportsheet reads
-- its listings in one, so that they agree with each other. The answers of
-- the sections are joined into one, in their order.
wholeListing :: ExceptT String (DB IO) Sections -> ExceptT String (DB IO) Response
wholeListing build = do
  Sections _ most listings <- build
  answers <- sectionAnswers most listings
  case answers of
    [] -> throwError "The listing has no section"
    first : others -> return (foldl joinListings first others)
  where
    sectionAnswers _ [] = return []
    sectionAnswers left (Listing start rows _ answer _ : others)
      -- The first section answers even when --max is 0, so that the
      -- listing has an answer
      | left == Just 0 = (: []) . answer <$> rows start left
      | otherwise = do
          found <- rows start left
          let left' = subtract (fromIntegral (length found)) <$> left
          if left' == Just 0
            then return [answer found]
            else (answer found :) <$> sectionAnswers left' others

-- | What reading a section came to: the reader is gone or --max rows were
-- read, so the listing ends, or the section ran out of rows, and the next
-- one starts. The second says whether the listing has still shown nothing,
-- and how many rows --max leaves.
data SectionEnd = ListingEnds | SectionRanOut !Bool !(Maybe Natural)

-- | Read the listing section by section and chunk by chunk, each chunk in
-- a transaction of its own, and pass each one to the action as an answer,
-- with where it is in the listing. The reading stops when the rows of the
-- last section run out, when --max rows were read, or when the action
-- answers False, as it does once the pager is closed. A chunk without
-- rows is not passed on, so a section without rows shows nothing. When no
-- section has a row, the action is passed one answer without rows, as the
-- first chunk: an empty listing says so. A section read whole is passed
-- on as one chunk, however many rows it has.
streamListing :: Config -> ExceptT String (DB IO) Sections -> (ChunkAt -> Response -> IO Bool) -> IO (Either String ())
streamListing cfg build onChunk = do
  built <- readOnce cfg build
  case built of
    Left err -> return (Left err)
    Right (Sections titled most listings) -> do
      ended <- through titled True most listings
      case (ended, listings) of
        (Left err, _) -> return (Left err)
        -- No section had a row: the listing says so once
        (Right True, Listing _ _ _ answer _ : _) -> Right () <$ onChunk (ChunkAt True True titled) (answer [])
        _ -> return (Right ())
  where
    -- Read the sections left. Answers whether the listing has still
    -- shown nothing.
    through :: Bool -> Bool -> Maybe Natural -> [Listing] -> IO (Either String Bool)
    through _ nothingYet _ [] = return (Right nothingYet)
    through titled nothingYet left (listing : others) = do
      ended <- section titled nothingYet left listing
      case ended of
        Left err -> return (Left err)
        Right ListingEnds -> return (Right False)
        Right (SectionRanOut nothingYet' left') -> through titled nothingYet' left' others
    section :: Bool -> Bool -> Maybe Natural -> Listing -> IO (Either String SectionEnd)
    section titled nothingYet left (Listing start rows _ answer True) = do
      whole <- readOnce cfg (rows start left)
      case whole of
        Left err -> return (Left err)
        Right [] -> return (Right (SectionRanOut nothingYet left))
        Right found -> do
          continue <- onChunk (ChunkAt nothingYet True titled) (answer found)
          let left' = subtract (fromIntegral (length found)) <$> left
          return . Right $ if not continue || left' == Just 0 then ListingEnds else SectionRanOut False left'
    section titled nothingYet left (Listing start rows cursorOf answer False) =
      let chunk startsSection nothing cursor remaining = do
            let count = maybe chunkRows (min chunkRows) remaining
            got <- readOnce cfg (rows cursor (Just count))
            case got of
              Left err -> return (Left err)
              Right [] -> return (Right (SectionRanOut nothing remaining))
              Right (found : more) -> do
                continue <- onChunk (ChunkAt nothing startsSection titled) (answer (found : more))
                let taken = fromIntegral (length (found : more))
                    remaining' = subtract taken <$> remaining
                if not continue || remaining' == Just 0
                  then return (Right ListingEnds)
                  else
                    if taken < count
                      then return (Right (SectionRanOut False remaining'))
                      else case cursorOf (NonEmpty.last (found NonEmpty.:| more)) of
                        Left err -> return (Left err)
                        Right next -> chunk False False (Just next) remaining'
       in chunk True nothingYet start left

-- | Run a reading of the database in one transaction: its answer, its
-- failure, or that of a database held by another command
readOnce :: Config -> ExceptT String (DB IO) a -> IO (Either String a)
readOnce cfg action = join <$> catchDatabaseInUse (runDB cfg (runExceptT action))
