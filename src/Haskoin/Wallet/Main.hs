-- | The program hw, from the command line to the answer. 'clientMain' is
-- all of it: it protects the memory of the process, runs the setup of a
-- first run, reads the command, the configuration and the database, which
-- it brings to the version of this hw, and runs the command. A listing
-- goes to the screen through the pager, chunk after chunk. Any other
-- answer is printed whole, on the screen or in JSON.
--
-- Help, --version and a usage error stop before the configuration is
-- read: they work without a wallet, and leave the data directory as it is.
module Haskoin.Wallet.Main where

import Control.Exception (IOException, finally, try)
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.String.Conversions (cs)
import Data.Foldable (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import qualified Data.ByteString.Char8 as C8
import GHC.IO.Encoding (setFileSystemEncoding, setLocaleEncoding, utf8)
import Haskoin.Wallet.Commands (answerWalletNaming, commandListing, commandResponse, journalBeforeCommand, keptTagsOfListing, listingTagWidths, listingWalletNaming, refusedNetwork, runsOnDamagedDatabase)
import Haskoin.Wallet.Config (Config (..), commandUnit, databaseFile, initConfig)
import Haskoin.Wallet.Amounts (UnitChoice)
import Haskoin.Wallet.Database (DB, DatabaseFailure (..), runDB, tryDatabase)
import Haskoin.Wallet.Database.Failure (databaseFailureExplained)
import Haskoin.Wallet.Database.Portfolio (WalletNaming)
import Haskoin.Wallet.Database.Tag (TagWidths)
import Haskoin.Wallet.Memory (keepMemoryPrivate)
import Haskoin.Wallet.Cli.Listing (ChunkAt (..), KeptTags, Sections, configListingOrder, streamListing)
import Haskoin.Wallet.Cli.Pager (Paged (..), Shown (..), pagerCommand, tableShown, withPager)
import Haskoin.Wallet.Database.Migration (Hooks (..), checkDatabaseFile, migrateDB)
import Haskoin.Wallet.Database.Backup (backupDatabase, migrationBackupDirectory)
import Haskoin.Wallet.Cli.Command (addressCodeOf, csvFileOf, txShownOf, viewOf, wordsLayoutOf, wrapsLines)
import Haskoin.Wallet.Cli.Parser (GlobalOptions (..), parserMain)
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Cli.Prompt (forgetTypedSecrets)
import Haskoin.Wallet.Cli.PrettyPrinter (prettyPrinter)
import Haskoin.Wallet.Cli.PrettyPrinter.Listing (chunkDoc, keptTagsDoc, startsWithHeader)
import Haskoin.Wallet.Cli.PrettyPrinter.NextSteps (printNextSteps)
import Haskoin.Wallet.Cli.Response (Response (..), responseFailed)
import Haskoin.Wallet.Cli.Setup (needsSetup, setupMain)
import Haskoin.Wallet.Cli.Style (renderPlain)
import Haskoin.Wallet.Cli.Wrap (dieWith, warn)
import Haskoin.Wallet.Util (encodeJsonPretty)
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Prettyprinter (vcat)
import System.IO (stdout)

{- The program -}

-- | Run hw on the arguments of the process: see the header of this module
clientMain :: IO ()
clientMain = runCommand `finally` forgetTypedSecrets

-- | What 'clientMain' runs before it forgets what was typed ahead of a
-- secret ("Haskoin.Wallet.Cli.Prompt"): hw ends through an exit from
-- several places here, and each must pass there.
runCommand :: IO ()
runCommand = do
  -- This comes first, before any secret is typed. See
  -- "Haskoin.Wallet.Memory".
  keepMemoryPrivate
  setUtf8Encoding
  argumentsProblem <- invalidArgumentsError
  forM_ argumentsProblem dieWith
  -- Plain hw, where it was never set up, asks its user the settings to
  -- start with and writes the configuration file. With a file, plain hw
  -- shows its help, as --help does: the parser has it do so. See
  -- "Haskoin.Wallet.Cli.Setup".
  firstRun <- needsSetup
  when firstRun $ setupMain >> exitSuccess
  (cmd, GlobalOptions unitGiven json noPager) <- parserMain
  -- Only a command reads the configuration and touches the database:
  -- help, --version and usage errors leave ~/.hw as it is, and a
  -- database newer than this hw still gets help. A command run before
  -- hw was set up is refused here, and told to run plain hw.
  (cfg, warnings) <- either dieWith return =<< initConfig
  forM_ warnings warn
  let choice = commandUnit cfg unitGiven
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
            prettyPrinter choice view naming (addressCodeOf cmd) (txShownOf cmd) (wordsLayoutOf cmd) res
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
  opened <- tryDatabase . runExceptT $ do
    ExceptT (checkDatabaseFile database)
    ExceptT (runDB cfg (migrateDB (migrationHooks cfg) database))
  case opened of
    -- The journal hw keeps for ledger is written now when the
    -- configuration asks for one and the file is not there: the key
    -- was set by hand or in the setup, or the file was deleted. It is
    -- also written now when the accounts of the tags changed in the
    -- configuration file since it was written. Every command passes
    -- here, a listing too.
    Right checked -> either dieWith return checked >> journalBeforeCommand cfg
    -- A database SQLite cannot read stops every command but the two
    -- that bring a backup back. They run without it.
    Left (DatabaseDamaged _) | runsOnDamagedDatabase cmd -> return ()
    -- Any other failure of SQLite stops the command, with a sentence
    -- that says what to do
    Left failure -> dieWith =<< databaseFailureExplained cfg failure
  -- A listing goes to the screen, unless --json or --csv asks for it
  -- whole: then it is an answer as any other. On the screen, a listing
  -- that asks for no order takes that of the configuration.
  case commandListing cfg (configListingOrder cfg) cmd of
    Just listing | not json, Nothing <- csvFileOf cmd -> do
      pager <- pagerCommand stdout noPager
      naming <- listingWalletNaming cfg
      tagsWide <- listingTagWidths cfg
      -- The tags the listing keeps the rows of, which its first line
      -- names. A word that is no tag is refused, as any answer is.
      kept <- keptTagsOfListing cfg cmd
      case kept of
        Left refusal -> answer (ResponseError (cs refusal))
        Right keptM -> do
          failure <- printListing cfg pager choice view naming tagsWide (wrapsLines view cmd) keptM listing
          forM_ failure $ \refusal -> answer (ResponseError (cs refusal))
    _ -> answer =<< commandResponse cfg choice cmd

-- | Print a listing on the screen, chunk by chunk, through the pager when
-- there is one, its elements in the view given. The pager keeps the rows
-- of a table whole, and the header of a table taller than the screen on
-- the screen: the first chunk tells how tall the table is, the height of
-- the terminal how tall the screen. A listing in sections that say where
-- they are starts with a title, not with the header of a table, so its
-- header scrolls with its rows ('startsWithHeader'). The pager wraps the
-- lines of the listing when told to, rather than cut them ('wrapsLines').
-- Every chunk names the wallet of a portfolio or not, as given, and
-- gives the columns of tags of a table the widths given.
--
-- A listing that fails answers with why, and writes nothing of it: the
-- failure is no part of the listing, so it does not go through the
-- pager, which cuts a long line at the edge of the screen. The caller
-- prints it as the failure of any command, once the pager has ended.
--
-- A listing that keeps the rows of some tags starts with a line that
-- names them ('keptTagsDoc'), above its first chunk, so that the reader
-- knows rows are left out. That line is then the first one, not the
-- header of a table, so the pager keeps no header on the screen.
printListing :: Config -> Maybe String -> UnitChoice -> View -> WalletNaming -> TagWidths -> Bool -> Maybe KeptTags -> ExceptT String (DB IO) Sections -> IO (Maybe String)
printListing cfg pager choice view naming tagsWide wraps keptM listing = do
  screenRows <- fmap Terminal.height <$> Terminal.size
  -- Whether the first chunk starts with the header of a table: the pager
  -- starts at the first chunk, and asks then
  headed <- newIORef False
  let shownOf doc
        | wraps = return WrappedLines
        | otherwise = do
            pinned <- readIORef headed
            return $ if pinned then tableShown screenRows (length (lines (renderPlain doc))) else Lines
  withPager (Paged stdout "listing") pager shownOf $ \write -> do
    streamed <- streamListing cfg listing $ \at answer -> do
      when (chunkFirst at) $ writeIORef headed (isNothing keptM && startsWithHeader view at answer)
      doc <- chunkDoc choice view naming tagsWide at answer
      write $ case keptM of
        Just kept | chunkFirst at -> vcat [keptTagsDoc kept, doc]
        _ -> doc
    return (either Just (const Nothing) streamed)

-- | How the migration talks to the user, on stderr, and backs up the
-- database before it changes anything
migrationHooks :: Config -> Hooks
migrationHooks cfg =
  Hooks
    { announce = warn,
      backup = \version stamp -> do
        let database = databaseFile cfg
        -- Without its backup, the database is not migrated
        copy <- either dieWith return =<< backupDatabase database (migrationBackupDirectory database) version stamp
        warn $ "The database at version " <> show version <> " was backed up to " <> copy
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
