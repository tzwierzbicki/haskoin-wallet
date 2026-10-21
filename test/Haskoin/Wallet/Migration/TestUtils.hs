{-# LANGUAGE OverloadedStrings #-}

-- | Helpers shared by the migration tests: in-memory databases built from the
-- schema of an old hw, raw views of the tables, and the migration of hw run
-- with its announcements collected instead of printed.
--
-- The tables of each format version live under "Haskoin.Wallet.Schema":
-- versions 2 and 3 with hw, version 1 with the tests, since nothing migrates
-- to it. The spec of a version builds a database from the tables of the
-- version before it, fills it with rows written as that hw wrote them,
-- migrates to its version only, and checks the result against the tables of
-- its version and the data its hw wrote.
module Haskoin.Wallet.Migration.TestUtils where

import Conduit (runConduit, sinkList, (.|))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Database.Persist.Sql (Migration, PersistValue, Single (..), rawQuery, rawSql, runMigrationQuiet, transactionSave)
import Database.Persist.Sqlite (runSqlite)
import Haskoin (Ctx)
import Haskoin.Wallet.Database (DB, migrateAll)
import Haskoin.Wallet.Migration (Hooks (..), latestDatabaseVersion, migrateDBTo)
import Test.Hspec

-- | Run an action on a new in-memory database, without any table
inMemory :: DB IO a -> IO a
inMemory = runSqlite ":memory:"

-- | Create the tables of a schema: the current one ('migrateAll') or an old
-- one
createTables :: Migration -> DB IO ()
createTables = void . runMigrationQuiet

-- | Commit the fixture, as the hw that wrote the database did. A failed
-- migration rolls its transaction back, which must undo the migration only:
-- in an in-memory database, an uncommitted fixture would vanish with its
-- tables.
commitFixture :: DB IO ()
commitFixture = transactionSave

-- | The result of a database action must be the expected value
dbShouldBe :: (HasCallStack, Eq a, Show a) => DB IO a -> a -> DB IO ()
dbShouldBe action expected = do
  actual <- action
  liftIO $ actual `shouldBe` expected

-- | The creation time of the fixture rows
fixtureTime :: UTCTime
fixtureTime = UTCTime (fromGregorian 2023 11 1) 0

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

-- | The result of a migration alone
resultOf :: (Either String (), [String], [Int]) -> Either String ()
resultOf (result, _, _) = result

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

-- | The rows of a query, as raw values, in a fixed order
queryRows :: Text -> DB IO [[PersistValue]]
queryRows query = sort <$> runConduit (rawQuery query [] .| sinkList)

-- | All the rows of a table
tableRows :: Text -> DB IO [[PersistValue]]
tableRows table = queryRows $ "select * from " <> table

-- | All the rows of several tables
tablesRows :: [Text] -> DB IO [[[PersistValue]]]
tablesRows = mapM tableRows

-- | What the version table holds, each value with its SQLite storage class
storedVersions :: DB IO [(PersistValue, Text)]
storedVersions = do
  rows <- rawSql "select version, typeof(version) from d_b_version" []
  return [(value, storageClass) | (Single value, Single storageClass) <- rows]
