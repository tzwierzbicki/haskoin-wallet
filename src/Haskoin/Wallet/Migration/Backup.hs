{-# LANGUAGE OverloadedStrings #-}

-- | The copy of the database that hw keeps before migrating it, in case a
-- step goes wrong
module Haskoin.Wallet.Migration.Backup
  ( backupDirectory,
    backupDatabase,
    backupName,
  )
where

import Data.String.Conversions (cs)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime, getZonedTime)
import Database.Persist.Sql (PersistValue (PersistText))
import qualified Database.Sqlite as Sqlite
import Haskoin.Wallet.Config (hwDataDirectory)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, (</>))

-- | Where the copies go: the migration-backups directory of hw
backupDirectory :: IO FilePath
backupDirectory = (</> "migration-backups") <$> hwDataDirectory

-- | Copy the database, at the given format version, into a directory,
-- created if missing, and return the copy, named after the database, the
-- version and the time
backupDatabase :: FilePath -> FilePath -> Int -> IO FilePath
backupDatabase database directory version = do
  time <- getZonedTime
  let copy = directory </> backupName database version time
  createDirectoryIfMissing True directory
  copyDatabase database copy
  return copy

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

-- | Let SQLite write the copy itself, so that it is complete and consistent
-- whatever the state of the file: hw keeps a write-ahead log, whose pages a
-- plain copy of the file would miss, and the connection about to migrate
-- the database is open while the copy is made
copyDatabase :: FilePath -> FilePath -> IO ()
copyDatabase database copy = do
  connection <- Sqlite.open (cs database)
  statement <- Sqlite.prepare connection "vacuum into ?"
  Sqlite.bind statement [PersistText (cs copy)]
  _ <- Sqlite.step statement
  Sqlite.finalize statement
  Sqlite.close connection
