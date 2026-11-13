{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The line that tells what a sync or a recovery is doing while it waits
-- on the store. It goes to stderr, and only on a terminal, so the answers
-- and the --json output stay as they are. It is erased before a question,
-- and at the end, so nothing of it stays on the screen.
--
-- A sync learns its work as it goes: a scan ends when it finds a window of
-- unused addresses, the addresses to list are known after the balances, and
-- the transactions to fetch after the listings. A percentage of the whole
-- would jump backwards, so the line tells one step at a time, with a bar
-- only when the step knows its total.
--
-- The sync itself says little about all this. Most of the steps are told by
-- the store it asks: 'tellingStore' and 'scanningStore' wrap the store,
-- and each question tells the step it belongs to. Before any of it, sync
-- and recover ask the store of every network whether it is up, which
-- 'checkingHealth' tells: a host that does not answer can take a while.
module Haskoin.Wallet.SyncStatus
  ( -- * The steps
    Step (..),
    Task (..),
    statusText,
    statusTextWithin,

    -- * Telling the steps
    Tell,
    Status,
    noStatus,
    tellSearch,
    tellPortfolioSearch,

    -- * The store that tells
    checkingHealth,
    tellingStore,
    scanningStore,
    countedInParallel,

    -- * The line on the terminal
    StatusLine (..),
    withStatusLine,
    statusLineOn,
    statusLineWithin,
  )
where

import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Exception (IOException, catch, finally)
import Control.Monad (forM, when)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (genericLength)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (HardPath, Network (..), pathToStr)
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Store (Store (..), inParallel, requestBatch)
import Numeric.Natural (Natural)
import System.Console.ANSI (clearFromCursorToLineEndCode, hSupportsANSI)
import qualified System.Console.Terminal.Size as Terminal
import System.IO (Handle, hFlush, hPutStr, stderr)

{- The steps -}

-- | What a sync or a search is doing
data Step
  = -- | Asking the store whether it is up, and where the chain is
    CheckingStore
  | -- | Asking the store of the network whether it is up, before the work
    -- starts. sync and recover ask the store of every network first.
    CheckingHealth Network
  | -- | Scanning past the addresses the account has, with how many of them
    -- were found used so far
    LookingForAddresses Natural
  | -- | A task, with how much of it is done and its total
    Counted Task Natural Natural
  | -- | Looking at the account of a seed at the path, with how many
    -- accounts were found paid so far
    LookingAtAccount HardPath Natural
  | -- | Looking at the portfolio of a seed at the index, with how many
    -- portfolios were found paid so far
    LookingAtPortfolio Natural Natural
  | -- | Sending again a transaction the user sent that the store has
    -- lost. A store waits for a node to announce the transaction back,
    -- half a minute at most, so the line says what the wait is for.
    SendingAgain
  deriving (Eq, Show)

-- | What the store is asked about item by item, so that the total is known
data Task
  = ReadingBalances
  | ListingTransactions
  | FetchingTransactions
  | ReadingCoins
  | FetchingRawTransactions
  deriving (Eq, Show)

-- | How a task is named on the line
taskName :: Task -> String
taskName ReadingBalances = "reading balances"
taskName ListingTransactions = "listing transactions"
taskName FetchingTransactions = "fetching transactions"
taskName ReadingCoins = "reading coins"
taskName FetchingRawTransactions = "fetching raw transactions"

-- | What a task counts, after the numbers. The tasks that go by address
-- say so: "listing transactions" counts addresses, not transactions.
taskUnit :: Task -> String
taskUnit ReadingBalances = " addresses"
taskUnit ListingTransactions = " addresses"
taskUnit FetchingTransactions = ""
taskUnit ReadingCoins = " addresses"
taskUnit FetchingRawTransactions = ""

-- | The widest the line gets on a terminal of 80 columns or more. A line
-- that wraps cannot be redrawn in place. The last column is left free,
-- since some terminals wrap on reaching it. A narrower terminal gets a
-- narrower line ('statusLineWithin').
lineWidth :: Int
lineWidth = 79

-- | The widest the bar gets, between its brackets. A long line gets a
-- shorter bar.
barWidth :: Int
barWidth = 20

-- | The widest a name gets. A longer one is cut, so that the line fits.
nameWidth :: Int
nameWidth = 20

-- | The text of the line: what the step is of, and what it does
statusText :: Text -> Step -> String
statusText = statusTextWithin lineWidth

-- | The text of the line in the given number of columns. The bar of a
-- task takes what room the line has, down to none. What still does not
-- fit is cut at the end. The line wrapped on a terminal of less than 80
-- columns before, and each step then left a line behind it on the screen.
statusTextWithin :: Int -> Text -> Step -> String
statusTextWithin width name step = take width $ case step of
  CheckingStore -> subject <> "checking the store"
  CheckingHealth net -> subject <> "checking the health of the store of " <> net.name
  LookingForAddresses found -> subject <> "looking for used addresses, " <> show found <> " found"
  Counted task done total -> withBar width (subject <> taskName task) done total (taskUnit task)
  LookingAtAccount path paid -> subject <> "looking at " <> pathToStr path <> ", " <> paidSoFar paid
  LookingAtPortfolio index paid -> subject <> "looking at portfolio" <> show index <> ", " <> portfoliosPaidSoFar paid
  SendingAgain -> subject <> "sending a transaction again"
  where
    subject = cutName name <> ": "

-- | The name, with three dots in place of what does not fit
cutName :: Text -> String
cutName name
  | Text.length name <= nameWidth = Text.unpack name
  | otherwise = Text.unpack (Text.take (nameWidth - 3) name) <> "..."

-- | How many accounts were found paid so far
paidSoFar :: Natural -> String
paidSoFar 1 = "1 account paid so far"
paidSoFar paid = show paid <> " accounts paid so far"

-- | How many portfolios were found paid so far
portfoliosPaidSoFar :: Natural -> String
portfoliosPaidSoFar 1 = "1 portfolio paid so far"
portfoliosPaidSoFar paid = show paid <> " portfolios paid so far"

-- | The label, then the bar and the numbers, as wide as a line of the
-- given width allows
withBar :: Int -> String -> Natural -> Natural -> String -> String
withBar within label done total unit =
  label <> "  " <> bar width done total <> "  " <> numbers
  where
    numbers = show done <> "/" <> show total <> unit
    -- What is left once the label and the numbers are in, less the two
    -- spaces on each side of the bar and its two brackets
    room = within - length label - length numbers - 6
    width = max 0 (min barWidth room)

-- | A bar of the given width, rounded down, so that it is full only when
-- all is done
bar :: Int -> Natural -> Natural -> String
bar width done total = "[" <> replicate filled '#' <> replicate (width - filled) '-' <> "]"
  where
    filled = width * fromIntegral done `div` max 1 (fromIntegral total)

{- Telling the steps -}

-- | Where the steps of one sync or one search go
type Tell = Step -> IO ()

-- | Where the steps go, given what they are of: a portfolio and its
-- network, as in "portfolio0 btc", "recover" for the search over the
-- portfolios of a seed, or "recover on" a network for a search over the
-- accounts hw does not hold. The first health checks are of "sync" or
-- "recover", and their step names the network ('CheckingHealth').
type Status = Text -> Tell

-- | Nowhere: for the tests, and off a terminal
noStatus :: Status
noStatus _ _ = return ()

-- | Tell that a search is looking at the account at the path on the
-- network, with how many accounts it found paid so far. The line reads
-- "recover on btc: looking at /44'/0'/0', 0 accounts paid so far". It names
-- the network, since recover searches two in one run.
tellSearch :: Status -> Network -> HardPath -> Natural -> IO ()
tellSearch status net path paid = status ("recover on " <> Text.pack net.name) (LookingAtAccount path paid)

-- | Tell that the search of recover is looking at the portfolio at the
-- index, with how many portfolios it found paid so far. The line reads
-- "recover: looking at portfolio3, 1 portfolio paid so far". It names no
-- network: a portfolio is looked for on every network in turn.
tellPortfolioSearch :: Status -> Natural -> Natural -> IO ()
tellPortfolioSearch status index paid = status "recover" (LookingAtPortfolio index paid)

{- The store that tells -}

-- | The store of each network, telling that it checks the health of the
-- store when asked whether it is up. sync and recover ask the store of
-- every network first, before anything else is on the line. Without the
-- line, a host that does not answer would leave the screen empty for as
-- long as the web client waits.
checkingHealth :: Tell -> (Network -> Store) -> Network -> Store
checkingHealth tell storeOn net =
  store {storeHealth = liftIO (tell (CheckingHealth net)) >> storeHealth store}
  where
    store = storeOn net

-- | The store, telling the steps of a sync as it is asked. It asks about a
-- long list one batch at a time, so that the count moves as the answers
-- come, and a list of nothing asks nothing and tells nothing.
tellingStore :: Tell -> Store -> Store
tellingStore tell store =
  store
    { storeHealth = liftIO (tell CheckingStore) >> storeHealth store,
      storeBalances = inBatches tell ReadingBalances (storeBalances store),
      storeTransactions = inBatches tell FetchingTransactions (storeTransactions store),
      storeUnspents = inBatches tell ReadingCoins (storeUnspents store),
      storeRawTransactions = inBatches tell FetchingRawTransactions (storeRawTransactions store),
      storePublish = \tx -> liftIO (tell SendingAgain) >> storePublish store tx
    }

-- | The store of a scan, telling how many used addresses it reported so
-- far, on every chain the scan goes through
scanningStore :: Tell -> Store -> IO Store
scanningStore tell store = do
  found <- newIORef 0
  return
    store
      { storeBalances = \addrs -> do
          liftIO $ tell . LookingForAddresses =<< readIORef found
          bals <- storeBalances store addrs
          liftIO $ modifyIORef' found (+ usedAmong bals)
          return bals
      }

-- | How many of the balances are of used addresses: those with transactions
usedAmong :: [Store.Balance] -> Natural
usedAmong bals = genericLength [bal | bal <- bals, bal.txs /= 0]

-- | 'inParallel', counting the answers as a task on the line
countedInParallel :: Tell -> Task -> Natural -> [ExceptT String IO a] -> ExceptT String IO [a]
countedInParallel tell task atATime questions = do
  addDone <- liftIO $ counting tell task (genericLength questions)
  inParallel atATime [question <* liftIO (addDone 1) | question <- questions]

-- | Ask about the items a batch at a time, counting them as a task on the
-- line. A batch is one request of the web client.
inBatches :: Tell -> Task -> ([a] -> ExceptT String IO [b]) -> [a] -> ExceptT String IO [b]
inBatches tell task ask items = do
  addDone <- liftIO $ counting tell task (genericLength items)
  answers <- forM (chunksOf (fromIntegral requestBatch) items) $ \batch -> do
    answer <- ask batch
    liftIO $ addDone (genericLength batch)
    return answer
  return (concat answers)

-- | Start a task of the given total: it shows at zero, unless there is
-- nothing to do, and the function returned adds to what is done. Several
-- threads can call it: each addition is told before the next one starts,
-- so the count never goes back.
counting :: Tell -> Task -> Natural -> IO (Natural -> IO ())
counting tell task total = do
  doneSoFar <- newMVar 0
  when (total > 0) $ tell (Counted task 0 total)
  return $ \more -> modifyMVar_ doneSoFar $ \done -> do
    tell (Counted task (done + more) total)
    return (done + more)

{- The line on the terminal -}

-- | The line, for the command that runs a sync
data StatusLine = StatusLine
  { -- | Put a step on the line, in place of the one before
    showStep :: Status,
    -- | Take the line off the screen, before a question or a message. The
    -- next step brings it back.
    eraseStatus :: IO ()
  }

-- | Run the action with a line on stderr, if stderr is a terminal that
-- understands the code that clears a line, and nothing otherwise. The line
-- is gone when the action ends, however it ends, so the answer of the
-- command that follows starts on a clean line.
withStatusLine :: (StatusLine -> IO a) -> IO a
withStatusLine action = do
  onTerminal <- hSupportsANSI stderr
  width <- lineWidthOn stderr
  let line = if onTerminal then statusLineWithin width stderr else StatusLine noStatus (return ())
  action line `finally` eraseStatus line

-- | How wide the line may be on the terminal of the handle: one column
-- less than the terminal has, and 'lineWidth' at most. The width is read
-- once, when the command starts. A terminal that does not tell its width
-- gets 'lineWidth', and so does one that tells a width of 0, as a
-- terminal no window was ever set on does.
lineWidthOn :: Handle -> IO Int
lineWidthOn h = do
  window <- Terminal.hSize h `catch` unknown
  return $ case window of
    Just size | Terminal.width size > 1 -> min lineWidth (Terminal.width size - 1)
    _ -> lineWidth
  where
    unknown :: IOException -> IO (Maybe (Terminal.Window Int))
    unknown _ = return Nothing

-- | The line on the handle, as wide as a terminal of 80 columns or more
-- allows ('statusLineWithin')
statusLineOn :: Handle -> StatusLine
statusLineOn = statusLineWithin lineWidth

-- | The line on the handle, in the given number of columns. Each text goes
-- back to the start of the line, writes over what is there, and clears
-- what a longer text left after it.
statusLineWithin :: Int -> Handle -> StatusLine
statusLineWithin width h =
  StatusLine
    { showStep = \name step -> write (statusTextWithin width name step),
      eraseStatus = write ""
    }
  where
    write text = bestEffort $ do
      hPutStr h ("\r" <> text <> clearFromCursorToLineEndCode)
      hFlush h

-- | Go on when the line cannot be written, on a terminal that went away
-- for instance. A failure here would undo the whole sync, which is one
-- transaction of the database, for the sake of a line on the screen.
bestEffort :: IO () -> IO ()
bestEffort action = action `catch` ignored
  where
    ignored :: IOException -> IO ()
    ignored _ = return ()
