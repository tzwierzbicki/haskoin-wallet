module Haskoin.Wallet.Main where

import Control.Exception (IOException, try)
import Control.Monad (join, when)
import Control.Monad.Except (ExceptT)
import Data.String.Conversions (cs)
import Data.Foldable (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import qualified Data.ByteString.Char8 as C8
import GHC.IO.Encoding (setFileSystemEncoding, setLocaleEncoding, utf8)
import Haskoin.Wallet.Commands (answerWalletNaming, commandListing, commandResponse, journalIfMissing, keptTagsOfListing, listingWalletNaming, refusedNetwork)
import Haskoin.Wallet.Config (Config (..), commandUnit, databaseFile, initConfig)
import Haskoin.Wallet.Amounts (AmountUnit)
import Haskoin.Wallet.Database (DB, catchDatabaseInUse, runDB)
import Haskoin.Wallet.Database.Portfolio (WalletNaming)
import Haskoin.Wallet.Memory (keepMemoryPrivate)
import Haskoin.Wallet.Cli.Listing (ChunkAt (..), KeptTags, Sections, configListingOrder, streamListing)
import Haskoin.Wallet.Cli.Pager (Paged (..), Shown (..), pagerCommand, tableShown, withPager)
import Haskoin.Wallet.Migration (Hooks (..), checkDatabaseFile, migrateDB)
import Haskoin.Wallet.Backup (backupDatabase, migrationBackupDirectory)
import Haskoin.Wallet.Cli.Parser (GlobalOptions (..), addressCodeOf, csvFileOf, parserMain, txShownOf, viewOf, wordsLayoutOf, wrapsLines)
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.Cli.PrettyPrinter (chunkDoc, errorDoc, keptTagsDoc, prettyPrinter, printNextSteps, startsWithHeader)
import Haskoin.Wallet.Cli.Response (Response (..), responseFailed)
import Haskoin.Wallet.Cli.Setup (needsSetup, setupMain)
import Haskoin.Wallet.Cli.Style (renderPlain)
import Haskoin.Wallet.Util (encodeJsonPretty)
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (getArgs)
import System.Exit (die, exitFailure, exitSuccess)
import Prettyprinter (vcat)
import System.IO (hPutStrLn, stderr, stdout)

{- The program -}

clientMain :: IO ()
clientMain = do
  -- This comes first, before any secret is typed. See
  -- "Haskoin.Wallet.Memory".
  keepMemoryPrivate
  setUtf8Encoding
  argumentsProblem <- invalidArgumentsError
  forM_ argumentsProblem die
  -- Plain hw, where it was never set up, asks its user the settings to
  -- start with and writes the configuration file. With a file, plain hw
  -- shows its help, as the parser has it do. See
  -- "Haskoin.Wallet.Cli.Setup".
  firstRun <- needsSetup
  when firstRun $ setupMain >> exitSuccess
  (cmd, GlobalOptions unitGiven json noPager) <- parserMain
  -- Only a command reads the configuration and touches the database:
  -- help, --version and usage errors leave ~/.hw as it is, and a
  -- database newer than this hw still gets help. A command run before
  -- hw was set up is refused here, and told to run plain hw.
  (cfg, warnings) <- either die return =<< initConfig
  forM_ warnings $ hPutStrLn stderr
  let unit = commandUnit cfg unitGiven
      -- The view an option asks for, or that of the configuration
      view = viewOf (configListingView cfg) cmd
      -- An answer on the screen, or in JSON. One that says the command
      -- failed exits as a failure. The screen names the wallet of each
      -- portfolio when the database has several. JSON always gives the
      -- wallet.
      answer res
        | json = jsonPrinter res
        | otherwise = do
            naming <- answerWalletNaming cfg res
            prettyPrinter unit view naming (addressCodeOf cmd) (txShownOf cmd) (wordsLayoutOf cmd) res
            printNextSteps cmd res
  -- A --network the configuration hides is refused before the command
  -- runs and before the database is opened: nothing but the
  -- configuration says so. The refusal is an answer, in JSON too.
  forM_ (refusedNetwork cfg cmd) $ \refusal -> do
    answer (ResponseError (cs refusal))
    exitFailure
  -- A database of an earlier version is refused, untouched, and the
  -- refusal names its file. Its version is read without a connection
  -- that writes, which would change the journal mode of its file.
  let database = databaseFile cfg
  either die return . join =<< catchDatabaseInUse (checkDatabaseFile database)
  migrated <- catchDatabaseInUse $ runDB cfg (migrateDB (migrationHooks cfg) database)
  either die return (join migrated)
  -- The journal hw keeps for ledger is written now when the
  -- configuration asks for one and the file is not there: the key was
  -- set by hand or in the setup, or the file was deleted. Every command
  -- passes here, a listing too.
  journalIfMissing cfg
  -- A listing goes to the screen, unless --json or --csv asks for it
  -- whole: then it is an answer as any other. On the screen, a listing
  -- that asks for no order takes that of the configuration.
  case commandListing cfg (configListingOrder cfg) cmd of
    Just listing | not json, Nothing <- csvFileOf cmd -> do
      pager <- pagerCommand stdout noPager
      naming <- listingWalletNaming cfg
      -- The tags the listing keeps the rows of, which its first line
      -- names. A word that is no tag is refused, as any answer is.
      kept <- keptTagsOfListing cfg cmd
      case kept of
        Left refusal -> answer (ResponseError (cs refusal))
        Right keptM -> printListing cfg pager unit view naming (wrapsLines view cmd) keptM listing
    _ -> answer =<< commandResponse cfg unit cmd

-- | Print a listing on the screen, chunk by chunk, through the pager when
-- there is one, its elements in the view given. The pager keeps the rows
-- of a table whole, and the header of a table taller than the screen on
-- the screen: the first chunk tells how tall the table is, the height of
-- the terminal how tall the screen. A listing in sections that say where
-- they are starts with a title, not with the header of a table, so its
-- header scrolls with its rows ('startsWithHeader'). The pager wraps the
-- lines of the listing when told to, rather than cut them ('wrapsLines').
-- Every chunk names the wallet of a portfolio or not, as given. A listing
-- that fails exits as a failure, as an answer does.
--
-- A listing that keeps the rows of some tags starts with a line that
-- names them ('keptTagsDoc'), above its first chunk, so that the reader
-- knows rows are left out. That line is then the first one, not the
-- header of a table, so the pager keeps no header on the screen.
printListing :: Config -> Maybe String -> AmountUnit -> View -> WalletNaming -> Bool -> Maybe KeptTags -> ExceptT String (DB IO) Sections -> IO ()
printListing cfg pager unit view naming wraps keptM listing = do
  screenRows <- fmap Terminal.height <$> Terminal.size
  -- Whether the first chunk starts with the header of a table: the pager
  -- starts at the first chunk, and asks then
  headed <- newIORef False
  let shownOf doc
        | wraps = return WrappedLines
        | otherwise = do
            pinned <- readIORef headed
            return $ if pinned then tableShown screenRows (length (lines (renderPlain doc))) else Lines
  failed <- withPager (Paged stdout "listing") pager shownOf $ \write -> do
    streamed <- streamListing cfg listing $ \at answer -> do
      when (chunkFirst at) $ writeIORef headed (isNothing keptM && startsWithHeader view at answer)
      doc <- chunkDoc unit view naming at answer
      write $ case keptM of
        Just kept | chunkFirst at -> vcat [keptTagsDoc kept, doc]
        _ -> doc
    case streamed of
      Left err -> write (errorDoc (cs err)) >> return True
      Right () -> return False
  when failed exitFailure

-- | How the migration talks to the user, on stderr, and backs up the
-- database before it changes anything
migrationHooks :: Config -> Hooks
migrationHooks cfg =
  Hooks
    { announce = hPutStrLn stderr,
      backup = \version stamp -> do
        let database = databaseFile cfg
        -- Without its backup, the database is not migrated
        copy <- either die return =<< backupDatabase database (migrationBackupDirectory database) version stamp
        hPutStrLn stderr $ "The database at version " <> show version <> " was backed up to " <> copy
    }

{- The encoding of the arguments -}

-- | Use UTF-8 for the command line arguments, the standard streams and file
-- paths, whatever the locale of the process is.
--
-- GHC decodes the command line arguments with the file system encoding, which
-- follows the locale. In a process without a UTF-8 locale (cron jobs,
-- containers, some ssh sessions), every non-ASCII character of an argument is
-- replaced by U+FFFD. The message given to signmsg would then be signed
-- silently altered, and printing it back would fail. This must run before the
-- arguments are parsed.
--
-- With the strict UTF-8 encoding, an argument that is not valid UTF-8 is an
-- error instead of being altered.
setUtf8Encoding :: IO ()
setUtf8Encoding = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8

-- | The error to report when the command line arguments are not valid UTF-8,
-- or Nothing when they are. GHC decodes the arguments on every call of
-- getArgs, so the command line parser would otherwise fail with a raw
-- decoding exception.
invalidArgumentsError :: IO (Maybe String)
invalidArgumentsError = do
  decoded <- try getArgs :: IO (Either IOException [String])
  return $
    case decoded of
      Left _ ->
        Just
          "The command line arguments are not valid UTF-8.\
          \ hw reads and prints text, including signed messages, as UTF-8."
      Right _ -> Nothing

{- JSON output -}

-- | The answer in JSON on stdout. A failure exits as one, as the pretty
-- printer does, so that a script can tell it from the exit code.
jsonPrinter :: Response -> IO ()
jsonPrinter res = do
  C8.putStr (encodeJsonPretty res)
  when (responseFailed res) exitFailure
