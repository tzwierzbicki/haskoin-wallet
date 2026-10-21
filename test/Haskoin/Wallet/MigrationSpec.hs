{-# LANGUAGE OverloadedStrings #-}

-- | The migration of hw around its steps: new databases, databases already at
-- the latest version, databases of a newer hw, and the steps run one after
-- the other, after one backup. Each step is tested on its own in
-- "Haskoin.Wallet.Migration.V2Spec" and "Haskoin.Wallet.Migration.V3Spec",
-- from the version before it to its version; the copy of the database is
-- tested in "Haskoin.Wallet.BackupSpec".
module Haskoin.Wallet.MigrationSpec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin (prepareContext)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Migration
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Schema.V1 as V1
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "The migration of hw" $ do
    it "sees a database without tables as having no version" $
      inMemory $
        runExceptT databaseVersion `dbShouldBe` Right Nothing

    it "creates the tables of a new database at the latest version, without a backup" $
      inMemory $ do
        migrateAndCollect ctx `dbShouldBe` (Right (), [], [])
        storedVersions `dbShouldBe` [(PersistInt64 (fromIntegral latestDatabaseVersion), "integer")]
        runExceptT databaseVersion `dbShouldBe` Right (Just latestDatabaseVersion)
        expected <- liftIO currentSchema
        databaseSchema `dbShouldBe` expected

    it "leaves a database at the latest version alone" $
      inMemory $ do
        createTables migrateAll
        P.insert_ $ DBVersion latestDatabaseVersion
        P.insert_ $ DBWallet "892eb8e4"
        commitFixture
        original <- (,) <$> databaseSchema <*> tablesRows ["d_b_version", "d_b_wallet"]
        migrateAndCollect ctx `dbShouldBe` (Right (), [], [])
        ((,) <$> databaseSchema <*> tablesRows ["d_b_version", "d_b_wallet"]) `dbShouldBe` original

    it "backs up the database once, then runs the steps one after the other" $
      inMemory $ do
        createTables V1.migrateAll
        commitFixture
        migrateToAndCollect 3 ctx
          `dbShouldBe` ( Right (),
                         [ "Migrating the database to version 2: the transaction summaries of hw 0.9",
                           "Migrating the database to version 3: the version stored as a number"
                         ],
                         [1]
                       )
        storedVersions `dbShouldBe` [(PersistInt64 3, "integer")]

    it "refuses a database of a newer hw" $
      inMemory $ do
        createTables migrateAll
        P.insert_ $ DBVersion newer
        commitFixture
        migrateAndCollect ctx
          `dbShouldBe` ( Left $
                           "The database is at version "
                             <> show newer
                             <> ", newer than the version "
                             <> show latestDatabaseVersion
                             <> " of this hw",
                         [],
                         []
                       )
        storedVersions `dbShouldBe` [(PersistInt64 (fromIntegral newer), "integer")]
  where
    newer = latestDatabaseVersion + 1
