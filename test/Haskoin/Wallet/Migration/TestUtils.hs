{-# LANGUAGE OverloadedStrings #-}

-- | Helpers shared by the migration tests: the frame of the migration run
-- with its announcements and backups collected instead of done, the
-- schema and rows of a database, to tell what a migration changed, and
-- the databases of the versions this hw refuses. The database helpers
-- themselves are in "Haskoin.Wallet.TestUtils".
--
-- The databases of the versions before 5 are built with SQL, with a table
-- or two of each and the version table as each wrote it: hw 0.8 had none,
-- hw 0.9 stored the text "0.9", and the unreleased versions 3 and 4 a
-- number. hw reads nothing of them but their version. The one of version
-- 6, of a newer hw, has the tables of this one. "Haskoin.Wallet.MigrationSpec"
-- refuses them in the database layer, and the workflow tests put them in
-- the home of the hw binary ("Haskoin.Wallet.Workflow.PortfolioSpec"), so
-- that both see the same databases.
--
-- hw 0.10 has no step yet ("Haskoin.Wallet.Migration"), and
-- "Haskoin.Wallet.MigrationSpec" tests the frame with a step of its own.
-- The first step of hw, to version 6, gets a spec of its own,
-- Migration/V6Spec.hs. It builds a database from the tables of version 5,
-- frozen in "Haskoin.Wallet.Schema.V5", fills it with rows written as hw
-- 0.10 writes them, runs the step alone with 'migrateWithAndCollect', and
-- checks the result against the tables of version 6 and the data the step
-- writes.
--
-- A version spec sees its two versions and nothing else. Rows are read
-- with the entities of its schema modules, never those of
-- "Haskoin.Wallet.Database", and stored data is parsed and compared with
-- the types frozen in the step module that wrote it, never with
-- "Haskoin.Wallet.Database.TxInfo" or any other live type. Only
-- "Haskoin.Wallet.MigrationSpec" may use the live tables, since what this
-- hw makes of a database is its scope. A spec that reaches the live code
-- breaks the day a later version changes it, which is exactly what these
-- tests must survive.
module Haskoin.Wallet.Migration.TestUtils where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..), Single (..), rawExecute, rawSql, transactionSave)
import Haskoin (Ctx)
import Haskoin.Wallet.Database (DB, DBVersion (..), globalMigration, migrateAll)
import Haskoin.Wallet.Migration (Hooks (..), Step, migrateDB, migrateWith)
import Haskoin.Wallet.TestUtils (createTables, inMemory, tableRows, testAccountXPub)

-- | Commit the fixture, as the hw that wrote the database did. A failed
-- migration rolls its transaction back, which must undo the migration only:
-- in an in-memory database, an uncommitted fixture would vanish with its
-- tables.
commitFixture :: DB IO ()
commitFixture = transactionSave

-- | Where the tests say the database file is. The refusal of an older
-- database names it, in the commands that move it aside.
databasePath :: FilePath
databasePath = "/home/someone/.hw/accounts.sqlite"

-- | The hooks of a migration that write down what it does outside the
-- database, in the order it does it: the line "Backup at version N" for a
-- backup at version N, and each announcement as it is
loggingHooks :: IO (Hooks, IO [String])
loggingHooks = do
  happened <- newIORef []
  let note line = modifyIORef happened (<> [line])
      hooks =
        Hooks
          { announce = note,
            backup = \version -> note ("Backup at version " <> show version)
          }
  return (hooks, readIORef happened)

-- | The migration of hw, with its steps and to its latest version, of a
-- database said to be at the path given, with what it does outside the
-- database written down ('loggingHooks')
migrateAndLog :: FilePath -> Ctx -> DB IO (Either String (), [String])
migrateAndLog path ctx = do
  (hooks, events) <- liftIO loggingHooks
  result <- migrateDB hooks path ctx
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

{- The databases of each version -}

-- | The databases of the versions before 5, each with the version as it
-- writes it and what made it
olderDatabases :: [(String, String, DB IO ())]
olderDatabases =
  [ ("1", "hw 0.8", version1Database),
    ("2", "hw 0.9", version2Database),
    ("3", "an unreleased hw 0.10", version3Database),
    ("4", "an unreleased hw 0.10", version4Database)
  ]

-- | A database of hw 0.8: an account, and no version table
version1Database :: DB IO ()
version1Database = do
  accountTable
  commitFixture

-- | A database of hw 0.9: an account, and the version table with the
-- text hw 0.9 wrote
version2Database :: DB IO ()
version2Database = do
  accountTable
  rawExecute "create table d_b_version (version varchar not null)" []
  rawExecute "insert into d_b_version (version) values ('0.9')" []
  commitFixture

-- | A database of the first unreleased form of hw 0.10: an account, and
-- the version as a number
version3Database :: DB IO ()
version3Database = do
  accountTable
  numberedVersion 3
  commitFixture

-- | A database of the second unreleased form of hw 0.10: a wallet whose
-- fingerprint is not known, as that form allowed, and the version 4
version4Database :: DB IO ()
version4Database = do
  rawExecute "create table d_b_wallet (id integer primary key, \"index\" integer not null, fingerprint varchar null)" []
  rawExecute "insert into d_b_wallet (\"index\", fingerprint) values (0, null)" []
  numberedVersion 4
  commitFixture

-- | A database of a newer hw, at version 6, with the tables of this one
version6Database :: DB IO ()
version6Database = do
  createTables migrateAll
  P.insert_ $ DBVersion 6
  commitFixture

-- | The table of the accounts of the earlier versions, with the BIP44
-- account 0 of the test mnemonic, named main
accountTable :: DB IO ()
accountTable = do
  rawExecute "create table d_b_account (id integer primary key, name varchar not null, x_pub_key varchar not null)" []
  rawExecute "insert into d_b_account (name, x_pub_key) values ('main', ?)" [PersistText testAccountXPub]

-- | The version table of the versions from 3 on, with the number
numberedVersion :: Int -> DB IO ()
numberedVersion version = do
  rawExecute "create table d_b_version (version integer not null, primary key (version))" []
  rawExecute "insert into d_b_version (version) values (?)" [PersistInt64 (fromIntegral version)]
