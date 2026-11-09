{-# LANGUAGE ScopedTypeVariables #-}

-- | The pager a listing goes through on a terminal, as git shows a log:
-- less, unless HW_PAGER, the key pager of config.json or PAGER names
-- another one. less quits by itself when the listing fits on the screen,
-- shows the colours, leaves the listing on the screen when it quits, and
-- keeps each line on one line of the screen (-F, -R, -X and -S), unless
-- LESS is set to something else. A table taller than the screen also
-- keeps its header on the screen, and the verbose view of accounts wraps
-- its lines instead, for its descriptors ('lessOptions'). A listing goes
-- out as it is when stdout is not a terminal, with --no-pager, and when
-- the pager named is empty or cat.
--
-- The pager starts with the first chunk of the listing, not before it: how
-- less shows a table depends on how tall the table is ('tableShown').
module Haskoin.Wallet.Cli.Pager
  ( Shown (..),
    lessOptions,
    pagerCommand,
    pagerNamed,
    readLessVersion,
    tableShown,
    withPager,
    writeDoc,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, catch, finally, throwIO, try)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.IO.Exception (IOErrorType (ResourceVanished))
import Haskoin.Wallet.Config (Config (configPager))
import Haskoin.Wallet.Cli.Style (Output, Style, outputOf, putDoc)
import Numeric.Natural (Natural)
import Prettyprinter (Doc)
import System.Directory (findExecutable)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath (takeFileName)
import System.IO (Handle, hClose, hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.IO.Error (ioeGetErrorType)
import System.Posix.Signals (Handler (Ignore), installHandler, sigINT)
import System.Process (CreateProcess (env, std_in), ProcessHandle, StdStream (CreatePipe), createProcess, readProcess, shell, waitForProcess)
import Text.Read (readMaybe)

-- | The pager to pipe a listing through, or none: with --no-pager, when
-- stdout is not a terminal, or when the pager named is none
pagerCommand :: Bool -> Config -> IO (Maybe String)
pagerCommand noPager cfg = do
  terminal <- hIsTerminalDevice stdout
  hwPager <- lookupEnv "HW_PAGER"
  pager <- lookupEnv "PAGER"
  return $
    if noPager || not terminal
      then Nothing
      else pagerNamed hwPager (configPager cfg) pager

-- | The pager named by HW_PAGER, the key pager of config.json or PAGER,
-- the first of them set, or less when none is. None when the name is
-- empty or cat.
pagerNamed :: Maybe String -> Maybe Text -> Maybe String -> Maybe String
pagerNamed hwPager fromConfig pager =
  case words named of
    [] -> Nothing
    ["cat"] -> Nothing
    _ -> Just named
  where
    named = fromMaybe "less" (hwPager <|> (cs <$> fromConfig) <|> pager)

-- | What a listing shows, which tells less how to show it: lines that
-- scroll together, those of a compact or a verbose view or of a table
-- that fits on the screen, the rows of a table taller than the screen,
-- which the user scrolls under its header, or lines that wrap at the edge
-- of the screen, those of a view that ends with a key to copy whole
-- ('Haskoin.Wallet.Cli.Parser.wrapsLines')
data Shown = Lines | TallTable | WrappedLines
  deriving (Eq, Show)

-- | How less shows a table of the lines given, on a screen of the rows
-- given if known. less keeps the last row of the screen for its prompt,
-- so a table of as many lines as the screen has rows is taller than the
-- screen. A screen of unknown height counts as tall enough: the header
-- then scrolls with the rows, but a short table still closes by itself.
tableShown :: Maybe Int -> Int -> Shown
tableShown screenRows tableLines
  | maybe False (tableLines >=) screenRows = TallTable
  | otherwise = Lines

-- | Run the action with a function that writes a document of the listing
-- where the terminal takes it, and tells whether the reader still reads
-- ('writeDoc'): on the input of the pager, or on stdout when there is
-- none. The pager starts at the first document, with the options for what
-- the function given says it shows, and is waited for after the action,
-- as long as the user reads. Ctrl-C then goes to the pager, which handles
-- it; hw ignores it meanwhile. A pager that is not installed is reported
-- on stderr, and the listing is printed without one.
withPager :: Maybe String -> (Doc Style -> Shown) -> ((Doc Style -> IO Bool) -> IO a) -> IO a
withPager pagerM shownOf action = do
  output <- outputOf stdout
  case pagerM of
    Nothing -> action (writeDoc stdout output)
    Just command -> do
      installed <- case words command of
        program : _ -> findExecutable program
        [] -> return Nothing
      case installed of
        Nothing -> do
          hPutStrLn stderr $ "The pager " <> command <> " was not found. The listing is printed without one."
          action (writeDoc stdout output)
        Just _ -> pipedThrough command shownOf output action

-- | A pager running: its input, its process, and what hw did on Ctrl-C
-- before it started
data Running = Running !Handle !ProcessHandle !Handler

-- | Run the action with a function that writes on the input of the pager
-- command, which starts at the first document written, then wait for the
-- pager
pipedThrough :: String -> (Doc Style -> Shown) -> Output -> ((Doc Style -> IO Bool) -> IO a) -> IO a
pipedThrough command shownOf output action = do
  started <- newIORef Nothing
  let write doc = do
        running <- readIORef started
        Running input _ _ <- case running of
          Just pager -> return pager
          Nothing -> do
            pager <- startPager command (shownOf doc)
            writeIORef started (Just pager)
            return pager
        writeDoc input output doc
  action write `finally` (mapM_ stopPager =<< readIORef started)

-- | Start the pager command, which the shell runs, with the options of
-- less for what the listing shows. LESS, when the user set it, stays as it
-- is, for a table too.
startPager :: String -> Shown -> IO Running
startPager command shown = do
  environment <- getEnvironment
  withLess <-
    if any ((== "LESS") . fst) environment
      then return environment
      else do
        -- Only a tall table asks, and only less: --header depends on its
        -- version
        version <- if shown == TallTable then lessVersion command else return Nothing
        return (("LESS", lessOptions shown version) : environment)
  (inputM, _, _, pager) <- createProcess (shell command) {std_in = CreatePipe, env = Just withLess}
  input <- maybe (ioError (userError "The pager has no input")) return inputM
  previous <- installHandler sigINT Ignore Nothing
  return (Running input pager previous)

-- | Close the input of the pager and wait for the user to quit it
stopPager :: Running -> IO ()
stopPager (Running input pager previous) = do
  -- The pager may be gone already, with its input closed
  hClose input `catch` \(_ :: IOException) -> return ()
  _ <- waitForProcess pager
  void (installHandler sigINT previous Nothing)

-- | The options hw gives less in LESS, given the version of less if
-- known. The first three are those git gives it: quit when the listing
-- fits on the screen (-F), show the colours (-R), and leave the listing
-- on the screen when less quits (-X).
--
-- Every listing but one also keeps each line on one line of the screen,
-- cut at its edge, the arrow keys scrolling it sideways (-S). The views
-- are laid out in columns: the rows of a table under its header, the
-- fields of a verbose view, a label under its address. A line that
-- wrapped would go on at the first column of the screen and break them.
-- The compact and the verbose views are laid out to fit in 80 columns, so
-- on most screens only a long label or name goes past the edge. A table
-- is often wider. less marks a line it cut with a > at the edge of the
-- screen, and then stays open, so that the user can scroll to the rest of
-- it. git once gave -S too, and dropped it as a matter of taste: a line it
-- cuts, such as a long line of a diff, hides text until the user scrolls.
-- Wrapping hides nothing, but here it would break what the columns show.
--
-- The verbose view of accounts is the one listing whose lines wrap (no
-- -S). Each account in it ends with its output descriptor, 148 columns or
-- more, which the user copies into another wallet. A descriptor cut at the
-- edge of the screen could not be copied, and less would stay open on
-- every such listing, however short. Without -S, less leaves the wrapping
-- to the terminal, which then copies the line whole, and quits by itself
-- when the listing fits. The wrapped rows come last in each account, so
-- that they break no column of the other fields.
--
-- From less 608 on, the header of a table taller than the screen, the line
-- of headings and the line under it, stays at the top of the screen as the
-- rows scroll (--header=2). A table that fits gets no --header: with it,
-- less never quits by itself, even with -F, and the user would have to
-- quit every short table. An older less would refuse --header with a
-- warning above the table, and so would one whose version hw cannot tell.
lessOptions :: Shown -> Maybe Natural -> String
lessOptions Lines _ = "FRXS"
lessOptions WrappedLines _ = "FRX"
lessOptions TallTable version
  | maybe False (>= 608) version = "FRXS --header=2"
  | otherwise = "FRXS"

-- | The version of less, as the first line of less --version gives it:
-- 668 for "less 668 (GNU regular expressions)". None for what another
-- program prints.
readLessVersion :: String -> Maybe Natural
readLessVersion printed = case words printed of
  "less" : number : _ -> readMaybe (takeWhile isDigit number)
  _ -> Nothing

-- | The version of the less a pager command runs, by asking it. None when
-- the command runs another program, or when less does not answer.
lessVersion :: String -> IO (Maybe Natural)
lessVersion command = case words command of
  program : _ | takeFileName program == "less" -> do
    printed <- try (readProcess program ["--version"] "") :: IO (Either IOException String)
    return (either (const Nothing) readLessVersion printed)
  _ -> return Nothing

-- | Write the document on the handle, styled or plain, and send it on to
-- the reader. False once the reader is gone: the pager when the user quits
-- it before the end of the listing, or the command the listing is piped
-- to when it stops reading. There is nothing left to write for.
writeDoc :: Handle -> Output -> Doc Style -> IO Bool
writeDoc h output doc = do
  written <- try (putDoc h output doc >> hFlush h) :: IO (Either IOException ())
  case written of
    Right () -> return True
    Left e
      | ioeGetErrorType e == ResourceVanished -> return False
      | otherwise -> throwIO e
