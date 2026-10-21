{-# LANGUAGE OverloadedStrings #-}

-- | The migration of hw around its steps: new databases, databases already at
-- the latest version, databases of a newer hw, the steps run one after the
-- other, and the backup taken before them. Each step is tested on its own in
-- "Haskoin.Wallet.Migration.V2Spec" and "Haskoin.Wallet.Migration.V3Spec",
-- from the version before it to its version.
module Haskoin.Wallet.MigrationSpec (spec) where

import Control.Exception (finally)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, isSuffixOf)
import Data.String.Conversions (cs)
import Data.Time (LocalTime (..), TimeOfDay (..), ZonedTime (..), fromGregorian, utc)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..), transactionSave)
import Database.Persist.Sqlite (runSqlite)
import Haskoin (prepareContext)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Migration
import Haskoin.Wallet.Migration.Backup (backupDatabase, backupName)
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Schema.V1 as V1
import qualified Haskoin.Wallet.Schema.V2 as V2
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  describe "The backup before a migration" $ do
    it "names the copy after the database, the version and the time" $
      backupName "/home/someone/.hw/accounts.sqlite" 2 (ZonedTime (LocalTime (fromGregorian 2026 10 21) (TimeOfDay 14 5 33)) utc)
        `shouldBe` "accounts-version-2-2026-10-21T14-05-33.sqlite"

    it "copies the database into the backups directory" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            backups = dir </> "migration-backups"
        runSqlite (cs database) $ do
          createTables V2.migrateAll
          P.insert_ $ V2.DBVersion "0.9"
          P.insert_ $ V2.DBWallet "892eb8e4"
        copy <- backupDatabase database backups 2
        takeDirectory copy `shouldBe` backups
        takeFileName copy `shouldSatisfy` \name -> "accounts-version-2-" `isPrefixOf` name && ".sqlite" `isSuffixOf` name
        runSqlite (cs copy) storedVersions `shouldReturn` [(PersistText "0.9", "text")]
        runSqlite (cs copy) (tableRows "d_b_wallet") `shouldReturn` [[PersistText "892eb8e4"]]

    it "copies the rows still in the write-ahead log" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
        -- Committed while the connection is open: the rows are in the
        -- write-ahead log, not yet in the file
        copy <- runSqlite (cs database) $ do
          createTables V2.migrateAll
          P.insert_ $ V2.DBVersion "0.9"
          transactionSave
          liftIO $ backupDatabase database (dir </> "migration-backups") 2
        runSqlite (cs copy) storedVersions `shouldReturn` [(PersistText "0.9", "text")]

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

-- | Run a test in a fresh directory under the temporary directory, removed
-- afterwards
inScratchDirectory :: (FilePath -> IO a) -> IO a
inScratchDirectory test = do
  base <- getTemporaryDirectory
  let dir = base </> "hw-migration-backup-test"
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
  createDirectoryIfMissing True dir
  test dir `finally` removeDirectoryRecursive dir
