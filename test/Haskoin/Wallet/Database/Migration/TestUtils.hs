{-# LANGUAGE OverloadedStrings #-}

-- | Helpers shared by the migration tests: the frame of the migration run
-- with its announcements and backups collected instead of done, the
-- schema and rows of a database, to tell what a migration changed, and
-- the databases of the versions this hw refuses. The database helpers
-- themselves are in "Haskoin.Wallet.TestUtils".
--
-- The database of an earlier version, 4, is built with SQL, with a table
-- and the version table. hw reads nothing of it but its version. The one
-- of version 6, of a newer hw, has the tables of this one, and the one of
-- another program has a table and no version. "Haskoin.Wallet.Database.MigrationSpec"
-- refuses them in the database layer, and the workflow tests put them in
-- the home of the hw binary ("Haskoin.Wallet.Workflow.PortfolioSpec"), so
-- that both see the same databases.
--
-- hw 0.10 has no step yet ("Haskoin.Wallet.Database.Migration"), and
-- "Haskoin.Wallet.Database.MigrationSpec" tests the frame with a step of its own.
-- The first step of hw, to version 6, gets a spec of its own,
-- Migration/V6Spec.hs. It builds a database from the tables of version 5,
-- frozen in "Haskoin.Wallet.Database.Schema.V5", fills it with rows written as hw
-- 0.10 writes them, runs the step alone with 'migrateWithAndCollect', and
-- checks the result against the tables of version 6 and the data the step
-- writes.
--
-- A version spec sees its two versions and nothing else. Rows are read
-- with the entities of its schema modules, never those of
-- "Haskoin.Wallet.Database", and stored data is parsed and compared with
-- the types frozen in the step module that wrote it, never with
-- "Haskoin.Wallet.Database.TxInfo" or any other live type. Only
-- "Haskoin.Wallet.Database.MigrationSpec" may use the live tables, since what this
-- hw makes of a database is its scope. A spec that reaches the live code
-- breaks the day a later version changes it, which is exactly what these
-- tests must survive.
module Haskoin.Wallet.Database.Migration.TestUtils where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..), Single (..), rawExecute, rawSql, transactionSave)
import Haskoin.Wallet.Database.Backup (CopyStamp (..))
import Haskoin.Wallet.Database (DB, DBVersion (..), globalMigration, migrateAll)
import Haskoin.Wallet.Database.Migration (Hooks (..), Step, migrateDB, migrateWith)
import Haskoin.Wallet.TestUtils (createTables, inMemory, tableRows)

-- | Commit the fixture, as the hw that wrote the database did. A failed
-- migration rolls its transaction back, which must undo the migration only:
-- in an in-memory database, an uncommitted fixture would vanish with its
-- tables.
commitFixture :: DB IO ()
commitFixture = transactionSave

-- | Where the tests say the database file is. The refusal of an older
-- database names it, in the commands that move it aside.
databasePath :: FilePath
databasePath = "/home/someone/.hw/portfolios.sqlite"

-- | The hooks of a migration that write down what it does outside the
-- database, in the order it does it: the line "Backup at version N, with
-- W wallets and P portfolios" for a backup at version N of a database of W
-- wallets and P portfolios, and each announcement as it is. The time of
-- the backup changes from run to run, so it is left out.
loggingHooks :: IO (Hooks, IO [String])
loggingHooks = do
  happened <- newIORef []
  let note line = modifyIORef happened (<> [line])
      hooks =
        Hooks
          { announce = note,
            backup = \version stamp ->
              note
                ( "Backup at version "
                    <> show version
                    <> ", with "
                    <> show (stampWallets stamp)
                    <> " wallets and "
                    <> show (stampPortfolios stamp)
                    <> " portfolios"
                )
          }
  return (hooks, readIORef happened)

-- | The migration of hw, with its steps and to its latest version, of a
-- database said to be at the path given, with what it does outside the
-- database written down ('loggingHooks')
migrateAndLog :: FilePath -> DB IO (Either String (), [String])
migrateAndLog path = do
  (hooks, events) <- liftIO loggingHooks
  result <- migrateDB hooks path
  (,) result <$> liftIO events

-- | The frame of the migration with the steps given, to the version
-- given, as the spec of a step runs it, with what it does outside the
-- database written down ('loggingHooks')
migrateWithAndLog :: Int -> [Step IO] -> DB IO (Either String (), [String])
migrateWithAndLog target steps = do
  (hooks, events) <- liftIO loggingHooks
  result <- migrateWith target steps hooks databasePath
  (,) result <$> liftIO events

-- | The schema of the database: the SQL that created each table and index
databaseSchema :: DB IO [(Text, Text)]
databaseSchema = do
  rows <-
    rawSql
      "select name, coalesce(sql, '') from sqlite_master \
      \where type in ('table', 'index') order by name"
      []
  return [(name, sql) | (Single name, Single sql) <- rows]

-- | Every row of the database: each table by its name, with its rows
databaseRows :: DB IO [(Text, [[PersistValue]])]
databaseRows = do
  tables <- rawSql "select name from sqlite_master where type = 'table' order by name" []
  mapM (\(Single table) -> (,) table <$> tableRows table) tables

-- | All the database holds, its schema and its rows, to tell that a
-- migration left it as it was
databaseContent :: DB IO ([(Text, Text)], [(Text, [[PersistValue]])])
databaseContent = (,) <$> databaseSchema <*> databaseRows

-- | The schema of a database created by the given setup: the tables of a
-- version, with its indexes when it has some
schemaCreatedBy :: DB IO () -> IO [(Text, Text)]
schemaCreatedBy setup = inMemory $ setup >> databaseSchema

-- | The schema of a database created by this hw
currentSchema :: IO [(Text, Text)]
currentSchema = schemaCreatedBy globalMigration

{- The databases this hw refuses -}

-- | A database of an earlier version, 4, which no step migrates: a
-- wallet whose fingerprint is not known, as that unreleased form of hw
-- 0.10 allowed, and the version as a number
version4Database :: DB IO ()
version4Database = do
  rawExecute "create table d_b_wallet (id integer primary key, \"index\" integer not null, fingerprint varchar null)" []
  rawExecute "insert into d_b_wallet (\"index\", fingerprint) values (0, null)" []
  rawExecute "create table d_b_version (version integer not null, primary key (version))" []
  rawExecute "insert into d_b_version (version) values (4)" []
  commitFixture

-- | A database of another program: a table with a row, and no table of
-- the format version
otherProgramDatabase :: DB IO ()
otherProgramDatabase = do
  rawExecute "create table bookmarks (id integer primary key, url varchar not null)" []
  rawExecute "insert into bookmarks (url) values ('https://example.com/')" []
  commitFixture

-- | A database of a newer hw, at version 6, with the tables of this one
version6Database :: DB IO ()
version6Database = do
  createTables migrateAll
  P.insert_ $ DBVersion 6
  commitFixture
