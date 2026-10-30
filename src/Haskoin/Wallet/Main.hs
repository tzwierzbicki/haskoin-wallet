module Haskoin.Wallet.Main where

import Control.Exception (IOException, try)
import Control.Monad (join, when)
import Control.Monad.Except (ExceptT)
import Data.String.Conversions (cs)
import Data.Foldable (forM_)
import qualified Data.ByteString.Char8 as C8
import GHC.IO.Encoding (setFileSystemEncoding, setLocaleEncoding, utf8)
import Haskoin (withContext)
import Haskoin.Wallet.Commands (commandListing, commandResponse)
import Haskoin.Wallet.Config (Config, databaseFile, initConfig)
import Haskoin.Wallet.Amounts (AmountUnit)
import Haskoin.Wallet.Database (DB, catchDatabaseInUse, runDB)
import Haskoin.Wallet.Listing (Listing, streamListing)
import Haskoin.Wallet.Pager (pagerCommand, withPager, writeDoc)
import Haskoin.Wallet.Migration (Hooks (..), migrateDB)
import Haskoin.Wallet.Backup (backupDatabase, migrationBackupDirectory)
import Haskoin.Wallet.Parser (GlobalOptions (..), parserMain)
import Haskoin.Wallet.PrettyPrinter (chunkDoc, errorDoc, prettyPrinter, printNextSteps)
import Haskoin.Wallet.Response (Response (..), responseFailed)
import Haskoin.Wallet.Util (encodeJsonPretty)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)

{- The program -}

clientMain :: IO ()
clientMain = do
  setUtf8Encoding
  argumentsProblem <- invalidArgumentsError
  forM_ argumentsProblem die
  withContext $ \ctx -> do
    (cmd, GlobalOptions unit json noPager) <- parserMain
    -- Only a command reads the configuration and touches the database:
    -- help and usage errors leave ~/.hw as it is, and a database newer
    -- than this hw still gets help
    (cfg, warnings) <- either die return =<< initConfig
    forM_ warnings $ hPutStrLn stderr
    migrated <- catchDatabaseInUse $ runDB cfg (migrateDB (migrationHooks cfg) ctx)
    either die return (join migrated)
    case commandListing ctx cmd of
      Just listing | not json -> do
        pager <- pagerCommand noPager cfg
        printListing cfg pager unit listing
      _ -> do
        res <- commandResponse ctx cfg unit cmd
        if json
          then jsonPrinter res
          else do
            prettyPrinter unit res
            printNextSteps cmd res

-- | Print a listing on the screen, chunk by chunk, through the pager when
-- there is one. A listing that fails exits as a failure, as an answer does.
printListing :: Config -> Maybe String -> AmountUnit -> ExceptT String (DB IO) Listing -> IO ()
printListing cfg pager unit listing = do
  failed <- withPager pager $ \h output -> do
    let write = writeDoc h output
    streamed <- streamListing cfg listing $ \first answer -> write (chunkDoc unit first answer)
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
      backup = \version -> do
        database <- databaseFile cfg
        directory <- migrationBackupDirectory
        -- Without its backup, the database is not migrated
        copy <- either die return =<< backupDatabase database directory version
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
