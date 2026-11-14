{-# LANGUAGE OverloadedStrings #-}

-- | The copies hw makes of the database and of its labels. There are
-- three kinds, each in a directory of its own.
--
-- * The migration copies the database before it changes its format, in
--   case a step goes wrong ('migrationBackupDirectory').
--
-- * hw copies the database and its labels by itself into the backups
--   directory, after the commands that change them, and keeps the three
--   newest copies of each ('rollingBackupDirectory'). A copy of the
--   database follows each command that adds or deletes a wallet or a
--   portfolio ('rollDatabaseCopy'). A copy of the labels follows each
--   command that changes what exportwallet writes ('backupLabels').
--
-- * The backup command copies the database and its labels into the
--   user-backups directory, and hw never removes them
--   ('userBackupDirectory', 'userBackup').
--
-- A copy of the labels is the file exportwallet writes without options:
-- every portfolio of every wallet, with its labels, its descriptions and
-- the coins the user locked ("Haskoin.Wallet.Labels"). importwallet reads
-- it back into a new database. It holds what nothing else can rebuild:
-- what the user typed. The mnemonic rebuilds the keys, and a sync the
-- history. A copy of the database keeps everything, the pending
-- transactions too, but only another hw of the same format reads it.
--
-- Every copy is made after the change, so that the newest one holds the
-- database as it is now. When the user makes a third portfolio, the copy
-- holds three portfolios.
--
-- The backups and user-backups directories are next to the database:
-- ~/.hw/backups and ~/.hw/user-backups for the database of hw. A database
-- the configuration puts elsewhere, on an encrypted disk for instance,
-- keeps its copies next to it, since they show every address it holds.
-- The tests of hw keep their databases in scratch directories, so their
-- copies stay there too, away from the wallet of the user.
--
-- Each name carries the time of the copy, in UTC to the millisecond
-- ('stampText'). So names sort in the order the copies were made,
-- whatever the time zone, even in the hour the clock goes back in autumn,
-- and two commands in one second write two copies. Local time, as the
-- migration copies carry it, repeats an hour in autumn, and its names
-- would then sort out of order. The name of a copy of the database also
-- carries the format version and how many wallets and portfolios it holds,
-- so that the user can tell the copies apart without opening them
-- ('databaseCopyName').
--
-- hw keeps three copies of each kind in the backups directory
-- ('copiesKept'). It writes the new one first, then removes the oldest,
-- so that a failure never leaves fewer copies. It removes only the files
-- whose names it would write itself ('databaseCopyStamp',
-- 'labelCopies'). A copy the backup command of an earlier hw left in
-- the backups directory has a name without counts, so it stays.
module Haskoin.Wallet.Backup
  ( -- * The copies before a migration
    migrationBackupDirectory,
    backupDatabase,
    backupName,
    copyDatabase,

    -- * The names of the copies
    stampText,
    DatabaseStamp (..),
    databaseCopyName,
    databaseCopyStamp,

    -- * The copies hw makes by itself
    rollingBackupDirectory,
    copiesKept,
    rollDatabaseCopy,
    databaseCopies,
    labelBackupName,
    labelCopies,
    backupLabels,

    -- * The copies of the backup command
    userBackupDirectory,
    userBackup,
  )
where

import Control.Exception (bracket, try)
import Control.Monad (forM_, guard, void)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.List (sortOn, stripPrefix)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, formatTime, getZonedTime, parseTimeM)
import Database.Persist.Sql (PersistValue (PersistText))
import qualified Database.Sqlite as Sqlite
import Haskoin.Wallet.Config (hwDataDirectory)
import Haskoin.Wallet.FileIO (Placement (..), placeFile, privateDirectory, readFileBytes, systemReason, writeFileBytes)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile)
import System.FilePath (splitExtension, takeBaseName, takeDirectory, (</>))
import Text.Read (readMaybe)

{- The copies before a migration -}

-- | Where the migration puts its copies: the migration-backups directory of
-- hw
migrationBackupDirectory :: IO FilePath
migrationBackupDirectory = (</> "migration-backups") <$> hwDataDirectory

-- | Copy the database, at the given format version, into a directory,
-- created if missing, and return the copy, named after the database, the
-- version and the time, or say why it could not be made
backupDatabase :: FilePath -> FilePath -> Int -> IO (Either String FilePath)
backupDatabase database directory version = do
  time <- getZonedTime
  let copy = directory </> backupName database version time
  created <- systemReason (createDirectoryIfMissing True directory)
  case created of
    Left reason -> return $ Left ("The directory " <> directory <> " could not be created: " <> reason)
    Right () -> do
      copied <- copyDatabase database copy
      return (copy <$ copied)

-- | The name of a copy before a migration:
-- accounts-version-2-2026-10-21T14-05-33.sqlite for accounts.sqlite at
-- version 2, copied on 2026-10-21 at 14:05:33. The migration reads the
-- database of an older format, so it does not count its wallets.
backupName :: FilePath -> Int -> ZonedTime -> FilePath
backupName database version time =
  takeBaseName database
    <> "-version-"
    <> show version
    <> "-"
    <> formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" time
    <> ".sqlite"

-- | Copy the database to a file that does not exist yet. SQLite writes the
-- copy itself, so that it is complete and consistent whatever the state of
-- the file: hw keeps a write-ahead log, whose pages a plain copy of the file
-- would miss, and a connection of hw may be open while the copy is made.
-- SQLite writes it into a new file next to the copy, which takes the
-- copy's name once it is whole and on the disk ('placeFile'), since
-- SQLite does not put it there itself. A copy that fails is a message,
-- closes what it opened, and leaves no file behind.
copyDatabase :: FilePath -> FilePath -> IO (Either String ())
copyDatabase database copy = do
  placed <- systemReason (try (placeFile AtFreePath copy vacuumInto) :: IO (Either Sqlite.SqliteException ()))
  return $ first couldNotCopy (placed >>= first sqliteReason)
  where
    vacuumInto :: FilePath -> IO ()
    vacuumInto new =
      bracket (Sqlite.open (cs database)) Sqlite.close $ \connection ->
        bracket (Sqlite.prepare connection "vacuum into ?") Sqlite.finalize $ \statement -> do
          Sqlite.bind statement [PersistText (cs new)]
          void $ Sqlite.step statement
    couldNotCopy why = "The database could not be copied to " <> copy <> ": " <> why
    sqliteReason e = case Sqlite.seError e of
      Sqlite.ErrorFull -> "the disk is full"
      -- The name persistent-sqlite gives SQLite's "file is not a database"
      Sqlite.ErrorNotAConnection -> "it is not an SQLite database"
      other -> show other

{- The names of the copies -}

-- | The time of a copy as its name writes it: 2026-11-14T10-05-33.123Z
-- for 10:05:33.123 UTC on 2026-11-14. It has the same width at every
-- time, the milliseconds included when there are none, so that the names
-- sort in the order of their times.
stampText :: UTCTime -> String
stampText time =
  formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" time
    <> "."
    -- %q gives the picoseconds in twelve digits: the first three are the
    -- milliseconds
    <> take 3 (formatTime defaultTimeLocale "%q" time)
    <> "Z"

-- | The time a text writes as 'stampText' does, and nothing else: a time
-- written in another way is not that of a copy of hw
readStamp :: String -> Maybe UTCTime
readStamp text = do
  time <- parseTimeM False defaultTimeLocale "%Y-%m-%dT%H-%M-%S%QZ" text
  guard (stampText time == text)
  return time

-- | How many characters 'stampText' writes
stampWidth :: Int
stampWidth = length ("2026-11-14T10-05-33.123Z" :: String)

-- | What the name of a copy of the database says about it
data DatabaseStamp = DatabaseStamp
  { -- | The format version of the database ("Haskoin.Wallet.Migration")
    stampVersion :: !Int,
    -- | When the copy was made
    stampTime :: !UTCTime,
    -- | How many wallets the database holds
    stampWallets :: !Int,
    -- | How many portfolios it holds, those of every wallet together
    stampPortfolios :: !Int
  }
  deriving (Eq, Show)

-- | The name of a copy of the database, without its extension:
-- accounts-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3 for
-- accounts.sqlite at version 5, copied at 10:05:33.123 UTC on 2026-11-14,
-- with 2 wallets and 3 portfolios. The backup command gives this name to
-- its copy of the labels too, so that the two files of one backup go
-- together.
databaseStem :: FilePath -> DatabaseStamp -> FilePath
databaseStem database stamp =
  takeBaseName database
    <> "-version-"
    <> show (stampVersion stamp)
    <> "-"
    <> stampText (stampTime stamp)
    <> "-wallets-"
    <> show (stampWallets stamp)
    <> "-portfolios-"
    <> show (stampPortfolios stamp)

-- | The name of a copy of the database:
-- accounts-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite
databaseCopyName :: FilePath -> DatabaseStamp -> FilePath
databaseCopyName database stamp = databaseStem database stamp <> ".sqlite"

-- | What the name of a file says, when it is the name 'databaseCopyName'
-- gives a copy of the database. Any other name is not that of such a
-- copy: a copy the backup command of an earlier hw made, which has no
-- counts, the copies of another database, a copy of the labels, a file
-- of the user, or the new file a write fills before it takes its name.
databaseCopyStamp :: FilePath -> FilePath -> Maybe DatabaseStamp
databaseCopyStamp database name = do
  let (stem, extension) = splitExtension name
  guard (extension == ".sqlite")
  afterBase <- stripPrefix (takeBaseName database <> "-version-") stem
  (version, afterVersion) <- leadingNumber afterBase
  timeAndAfter <- stripPrefix "-" afterVersion
  let (timeText, afterTime) = splitAt stampWidth timeAndAfter
  time <- readStamp timeText
  walletsAndAfter <- stripPrefix "-wallets-" afterTime
  (wallets, afterWallets) <- leadingNumber walletsAndAfter
  portfoliosText <- stripPrefix "-portfolios-" afterWallets
  (portfolios, rest) <- leadingNumber portfoliosText
  guard (null rest)
  let stamp = DatabaseStamp version time wallets portfolios
  -- The name hw would write, and not "05" for 5, say
  guard (databaseCopyName database stamp == name)
  return stamp

-- | The number a text starts with, and the rest of the text
leadingNumber :: String -> Maybe (Int, String)
leadingNumber text = case span isDigit text of
  ([], _) -> Nothing
  (digits, rest) -> (\number -> (number, rest)) <$> readMaybe digits

{- The copies hw makes by itself -}

-- | Where hw puts the copies it makes by itself of a database and of its
-- labels: the backups directory next to the database. For the database
-- of hw, it is ~/.hw/backups.
rollingBackupDirectory :: FilePath -> FilePath
rollingBackupDirectory database = takeDirectory database </> "backups"

-- | How many copies hw keeps of each kind it makes by itself: the newest
-- three. The oldest goes when a fourth is written.
copiesKept :: Int
copiesKept = 3

-- | Copy the database, as the stamp describes it, into the backups
-- directory, and keep the three newest of its copies there. The caller
-- reads the counts of the stamp in the transaction it holds while the
-- copy is made, so that no other command changes them in between. The
-- answer is the copy, or why it could not be made, or why an old copy
-- could not be removed.
rollDatabaseCopy :: FilePath -> DatabaseStamp -> IO (Either String FilePath)
rollDatabaseCopy database stamp = runExceptT $ do
  let directory = rollingBackupDirectory database
      copy = directory </> databaseCopyName database stamp
  makePrivateDirectory directory
  before <- liftIO $ databaseCopies database
  liftEither =<< liftIO (copyDatabase database copy)
  removeOldest copy before
  return copy

-- | The copies hw made by itself of a database, the oldest first
databaseCopies :: FilePath -> IO [FilePath]
databaseCopies database =
  filesByTime (rollingBackupDirectory database) (fmap stampTime . databaseCopyStamp database)

-- | The name of a copy of the labels: accounts-labels-2026-11-14T10-05-33.123Z.jsonl
-- for accounts.sqlite, copied at 10:05:33.123 UTC on 2026-11-14
labelBackupName :: FilePath -> UTCTime -> FilePath
labelBackupName database time = takeBaseName database <> "-labels-" <> stampText time <> ".jsonl"

-- | The time of a copy of the labels of the database, when the file name
-- is the name 'labelBackupName' gives one. Any other name is not that of
-- such a copy.
labelCopyTime :: FilePath -> FilePath -> Maybe UTCTime
labelCopyTime database name = do
  let (stem, extension) = splitExtension name
  guard (extension == ".jsonl")
  readStamp =<< stripPrefix (takeBaseName database <> "-labels-") stem

-- | The copies of the labels of a database, the oldest first
labelCopies :: FilePath -> IO [FilePath]
labelCopies database = filesByTime (rollingBackupDirectory database) (labelCopyTime database)

-- | Write the labels of a database, as exportwallet writes them, to a new
-- copy made at the time given, and keep the newest three copies. The
-- answer is the copy written, or Nothing when the newest copy holds the
-- same labels already, which writes nothing: that copy would push out an
-- older one that differs, for nothing. A copy that cannot be made is a
-- message, and so is an old copy that cannot be removed.
backupLabels :: FilePath -> UTCTime -> ByteString -> IO (Either String (Maybe FilePath))
backupLabels database time content = runExceptT $ do
  let directory = rollingBackupDirectory database
      copy = directory </> labelBackupName database time
  makePrivateDirectory directory
  before <- liftIO $ labelCopies database
  newest <- liftIO $ mapM readFileBytes (listToMaybe (reverse before))
  if newest == Just (Right content)
    then return Nothing
    else do
      liftEither =<< liftIO (writeFileBytes copy content)
      removeOldest copy before
      return (Just copy)

-- | The files of a directory whose names give a time, the oldest first:
-- none when the directory is not there
filesByTime :: FilePath -> (FilePath -> Maybe UTCTime) -> IO [FilePath]
filesByTime directory timeOf = do
  exists <- doesDirectoryExist directory
  names <- if exists then listDirectory directory else return []
  return [directory </> name | (_, name) <- sortOn fst [(time, name) | name <- names, Just time <- [timeOf name]]]

-- | Remove the oldest of the copies, given the oldest first, so that the
-- copy just written and the newest of the others make three. The copy
-- just written is always kept, even when its time is older than the
-- others, as after the clock was set back: it holds the database as it
-- is now, and the others give way to it.
removeOldest :: FilePath -> [FilePath] -> ExceptT String IO ()
removeOldest written copies = do
  let others = filter (/= written) copies
      stale = take (length others - (copiesKept - 1)) others
  forM_ stale $ \old ->
    liftEither . first (couldNotRemove old) =<< liftIO (systemReason (removeFile old))
  where
    couldNotRemove old reason = "The old copy " <> old <> " could not be removed: " <> reason

{- The copies of the backup command -}

-- | Where the backup command puts its copies of a database and of its
-- labels: the user-backups directory next to the database. For the
-- database of hw, it is ~/.hw/user-backups. hw never removes a file
-- there, so it is apart from the backups directory, where hw keeps three
-- copies of each kind.
userBackupDirectory :: FilePath -> FilePath
userBackupDirectory database = takeDirectory database </> "user-backups"

-- | Copy the database, as the stamp describes it, into the user-backups
-- directory, and write its labels beside it, as exportwallet writes them.
-- Both files take the name 'databaseCopyName' gives the stamp, the copy
-- with .sqlite and the labels with .jsonl in its place. The answer is the two files, or why
-- one could not be written. The caller reads the stamp and the labels in
-- the transaction it holds while the copy is made, so that the three
-- agree.
userBackup :: FilePath -> DatabaseStamp -> ByteString -> IO (Either String (FilePath, FilePath))
userBackup database stamp labels = runExceptT $ do
  let directory = userBackupDirectory database
      stem = directory </> databaseStem database stamp
      copy = stem <> ".sqlite"
      labelsFile = stem <> ".jsonl"
  makePrivateDirectory directory
  liftEither =<< liftIO (copyDatabase database copy)
  liftEither =<< liftIO (writeFileBytes labelsFile labels)
  return (copy, labelsFile)

-- | Make a directory of copies, when it is new, so that its owner alone
-- can enter it, as ~/.hw ('privateDirectory'): a copy shows every address
-- of every wallet. Each copy is readable by its owner alone, as every
-- file hw writes. The directory goes next to the database, which is
-- there, so it is one level to make.
makePrivateDirectory :: FilePath -> ExceptT String IO ()
makePrivateDirectory directory =
  liftEither . first couldNotCreate =<< liftIO (systemReason (privateDirectory directory))
  where
    couldNotCreate reason = "The directory " <> directory <> " could not be created: " <> reason
