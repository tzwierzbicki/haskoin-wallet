{-# LANGUAGE OverloadedStrings #-}

-- | Copies of the database: the ones hw keeps before migrating it, in case a
-- step goes wrong, and the ones the backup command makes. Then the copies
-- of its labels, which hw writes after each command that changes them.
--
-- A copy of the labels is the file exportwallet writes without options:
-- every portfolio of every wallet, with its labels, its descriptions and
-- the coins the user locked ("Haskoin.Wallet.Labels"). It is written after
-- the change, so that the newest copy holds the wallet as it is now.
-- importwallet reads it back into a new database. A copy of the database
-- keeps more, but only when the user runs the backup command. A copy of
-- the labels is made without a command, and holds what nothing else can
-- rebuild: what the user typed. The mnemonic rebuilds the keys, and a
-- sync the history.
--
-- The copies go in the backups directory next to the database:
-- ~/.hw/backups for the database of hw ('labelBackupDirectory'). A
-- database the configuration puts elsewhere, on an encrypted disk for
-- instance, keeps its copies next to it, since they show every address it
-- holds. The tests of hw keep their databases in scratch directories, so
-- their copies stay there too, away from the wallet of the user.
--
-- hw keeps the three newest copies ('labelCopiesKept'). Each name carries
-- the time of the copy, in UTC to the millisecond ('labelBackupName'). So
-- names sort in the order the copies were made, whatever the time zone,
-- even in the hour the clock goes back in autumn, and two commands in one
-- second write two copies. Local time, as the copies of the database
-- carry it, repeats an hour in autumn, and its names would then sort out
-- of order. A copy equal to the newest one is not written: it would push
-- out an older copy that differs, for nothing ('backupLabels').
module Haskoin.Wallet.Backup
  ( -- * Copies of the database
    backupDirectory,
    migrationBackupDirectory,
    backupDatabase,
    backupName,
    copyDatabase,

    -- * Copies of the labels
    labelBackupDirectory,
    labelBackupName,
    labelCopiesKept,
    labelCopies,
    backupLabels,
  )
where

import Control.Exception (bracket, try)
import Control.Monad (forM_, void)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.List (sort, stripPrefix)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, formatTime, getZonedTime, parseTimeM)
import Database.Persist.Sql (PersistValue (PersistText))
import qualified Database.Sqlite as Sqlite
import Haskoin.Wallet.Config (hwDataDirectory)
import Haskoin.Wallet.FileIO (Placement (..), placeFile, privateDirectory, readFileBytes, systemReason, writeFileBytes)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile)
import System.FilePath (takeBaseName, takeDirectory, (</>))

{- The directories -}

-- | Where the backup command puts its copies: the backups directory of hw
backupDirectory :: IO FilePath
backupDirectory = (</> "backups") <$> hwDataDirectory

-- | Where the migration puts its copies: the migration-backups directory of
-- hw
migrationBackupDirectory :: IO FilePath
migrationBackupDirectory = (</> "migration-backups") <$> hwDataDirectory

{- Copying the database -}

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

-- | The name of a copy: accounts-version-2-2026-10-21T14-05-33.sqlite for
-- accounts.sqlite at version 2, copied on 2026-10-21 at 14:05:33
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

{- Copying the labels -}

-- | Where the copies of the labels of a database go: the backups
-- directory next to it. For the database of hw, it is ~/.hw/backups, where
-- the backup command puts its copies.
labelBackupDirectory :: FilePath -> FilePath
labelBackupDirectory database = takeDirectory database </> "backups"

-- | How many copies of the labels hw keeps: the newest three. The oldest
-- goes when a fourth is written.
labelCopiesKept :: Int
labelCopiesKept = 3

-- | The name of a copy of the labels: accounts-labels-2026-11-14T10-05-33.123Z.jsonl
-- for accounts.sqlite, copied on 2026-11-14 at 10:05:33.123 UTC. The
-- time has the same width in every name, so that the names sort in the
-- order of their times.
labelBackupName :: FilePath -> UTCTime -> FilePath
labelBackupName database time =
  labelsPrefix database
    <> formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" time
    <> "."
    -- %q gives the picoseconds in twelve digits: the first three are the
    -- milliseconds
    <> take 3 (formatTime defaultTimeLocale "%q" time)
    <> "Z.jsonl"

-- | The start of the name of every copy of the labels of a database
labelsPrefix :: FilePath -> FilePath
labelsPrefix database = takeBaseName database <> "-labels-"

-- | Whether a file name is that of a copy of the labels of the database:
-- the name 'labelBackupName' gives for some time. Every other file of the
-- directory is not a copy, and hw leaves it alone: the copies of the
-- database, the copies of the labels of another database, a file the
-- user put there, and the new file a write fills before it takes its
-- name.
isLabelCopyOf :: FilePath -> FilePath -> Bool
isLabelCopyOf database name =
  case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H-%M-%S%QZ.jsonl" =<< stripPrefix (labelsPrefix database) name of
    Just time -> labelBackupName database time == name
    Nothing -> False

-- | The copies of the labels of a database, the oldest first
labelCopies :: FilePath -> IO [FilePath]
labelCopies database = do
  let directory = labelBackupDirectory database
  exists <- doesDirectoryExist directory
  names <- if exists then listDirectory directory else return []
  return [directory </> name | name <- sort names, isLabelCopyOf database name]

-- | Write the labels of a database, as exportwallet writes them, to a new
-- copy made at the time given, and keep the newest three copies. The
-- answer is the copy written, or Nothing when the newest copy holds the
-- same labels already, which writes nothing. A copy that cannot be made
-- is a message, and so is an old copy that cannot be removed.
--
-- The new copy is written before the oldest goes, so that a failure never
-- leaves fewer copies. The new copy is always kept, even when its name
-- sorts before the others, as after the clock was set back. The others
-- give way to it.
--
-- The backups directory is made private when it is new, as ~/.hw is
-- ('privateDirectory'): a copy shows every address of every wallet. Each
-- copy is readable by its owner alone, as every file hw writes.
backupLabels :: FilePath -> UTCTime -> ByteString -> IO (Either String (Maybe FilePath))
backupLabels database time content = runExceptT $ do
  let directory = labelBackupDirectory database
      copy = directory </> labelBackupName database time
  liftEither . first (couldNotCreate directory) =<< liftIO (systemReason (privateDirectory directory))
  before <- liftIO $ labelCopies database
  newest <- liftIO $ mapM readFileBytes (listToMaybe (reverse before))
  if newest == Just (Right content)
    then return Nothing
    else do
      liftEither =<< liftIO (writeFileBytes copy content)
      let others = filter (/= copy) before
          stale = take (length others - (labelCopiesKept - 1)) others
      forM_ stale $ \old ->
        liftEither . first (couldNotRemove old) =<< liftIO (systemReason (removeFile old))
      return (Just copy)
  where
    couldNotCreate directory reason = "The directory " <> directory <> " could not be created: " <> reason
    couldNotRemove old reason = "The old copy " <> old <> " could not be removed: " <> reason
