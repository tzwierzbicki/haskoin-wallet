{-# LANGUAGE OverloadedStrings #-}

-- | Helpers shared by the migration tests: the migration of hw run with its
-- announcements and backups collected instead of done, and schema
-- comparisons. The database helpers themselves are in
-- "Haskoin.Wallet.TestUtils".
--
-- The tables of each format version live under "Haskoin.Wallet.Schema":
-- versions 2 and later with hw, version 1 with the tests, since nothing
-- migrates to it. The spec of a version builds a database from the tables of
-- the version before it, fills it with rows written as that hw wrote them,
-- migrates to its version only, and checks the result against the tables of
-- its version and the data its hw wrote.
--
-- A version spec sees its two versions and nothing else. Rows are read with
-- the entities of its schema modules, never those of "Haskoin.Wallet.Database",
-- and stored data is parsed and compared with the types frozen in the step
-- module that wrote it, never with "Haskoin.Wallet.TxInfo" or any other live
-- type. Only the orchestrator spec, "Haskoin.Wallet.MigrationSpec", may use
-- the live tables and 'latestDatabaseVersion', since the whole migration is
-- its scope. A spec that reaches the live code breaks the day a later
-- version changes it, which is exactly what these tests must survive.
module Haskoin.Wallet.Migration.TestUtils where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import Database.Persist.Sql (Migration, Single (..), rawSql, transactionSave)
import Haskoin (Ctx)
import Haskoin.Wallet.Database (DB, migrateAll)
import Haskoin.Wallet.Migration (Hooks (..), latestDatabaseVersion, migrateDBTo)
import Haskoin.Wallet.TestUtils (createTables, inMemory)

-- | Commit the fixture, as the hw that wrote the database did. A failed
-- migration rolls its transaction back, which must undo the migration only:
-- in an in-memory database, an uncommitted fixture would vanish with its
-- tables.
commitFixture :: DB IO ()
commitFixture = transactionSave

-- | The migration to a format version, with what it does outside the
-- database collected instead of done: its result, the steps it announces
-- in order, and the versions it backs up the database at, which is one
-- version before the first step or none. The spec of a version migrates from
-- the version before it to that version only; the versions after it have
-- their own specs.
migrateToAndCollect :: Int -> Ctx -> DB IO (Either String (), [String], [Int])
migrateToAndCollect target ctx = do
  announced <- liftIO $ newIORef []
  backedUp <- liftIO $ newIORef []
  let hooks =
        Hooks
          { announce = \line -> modifyIORef announced (<> [line]),
            backup = \version -> modifyIORef backedUp (<> [version])
          }
  result <- migrateDBTo target hooks ctx
  steps <- liftIO $ readIORef announced
  versions <- liftIO $ readIORef backedUp
  return (result, steps, versions)

-- | The migration of hw, to the latest version
migrateAndCollect :: Ctx -> DB IO (Either String (), [String], [Int])
migrateAndCollect = migrateToAndCollect latestDatabaseVersion

-- | The schema of the database: the SQL that created each table and index
databaseSchema :: DB IO [(Text, Text)]
databaseSchema = do
  rows <-
    rawSql
      "select name, coalesce(sql, '') from sqlite_master \
      \where type in ('table', 'index') order by name"
      []
  return [(name, sql) | (Single name, Single sql) <- rows]

-- | The schema of a database created from a schema definition
schemaCreatedBy :: Migration -> IO [(Text, Text)]
schemaCreatedBy definition = inMemory $ createTables definition >> databaseSchema

-- | The schema of a database created by this hw
currentSchema :: IO [(Text, Text)]
currentSchema = schemaCreatedBy migrateAll
