{-# LANGUAGE OverloadedStrings #-}

-- | Copies of the database: the ones hw keeps before migrating it, in case a
-- step goes wrong, and the ones the backup command makes
module Haskoin.Wallet.Backup
  ( backupDirectory,
    migrationBackupDirectory,
    backupDatabase,
    backupName,
    copyDatabase,
  )
where

import Control.Exception (bracket, catch)
import Control.Monad (void)
import Data.String.Conversions (cs)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime, getZonedTime)
import Database.Persist.Sql (PersistValue (PersistText))
import qualified Database.Sqlite as Sqlite
import Haskoin.Wallet.Config (hwDataDirectory)
import Haskoin.Wallet.FileIO (systemReason)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, (</>))

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
-- A copy that fails is a message, and closes what it opened.
copyDatabase :: FilePath -> FilePath -> IO (Either String ())
copyDatabase database copy = (Right <$> vacuumInto) `catch` (return . Left . couldNotCopy)
  where
    vacuumInto =
      bracket (Sqlite.open (cs database)) Sqlite.close $ \connection ->
        bracket (Sqlite.prepare connection "vacuum into ?") Sqlite.finalize $ \statement -> do
          Sqlite.bind statement [PersistText (cs copy)]
          void $ Sqlite.step statement
    couldNotCopy e = "The database could not be copied to " <> copy <> ": " <> reason (Sqlite.seError e)
    reason Sqlite.ErrorCan'tOpen = "the file could not be created"
    reason Sqlite.ErrorFull = "the disk is full"
    reason other = show other
