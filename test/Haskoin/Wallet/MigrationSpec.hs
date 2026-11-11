{-# LANGUAGE OverloadedStrings #-}

-- | What this hw makes of a database, by its format version: a new one
-- gets the tables of version 5, one of version 5 opens as it is, and any
-- other is refused and left as it was, with instructions for one of an
-- earlier hw. hw 0.10 migrates no earlier database, so the frame of the
-- migration is tested here with a step of its own, from version 5 to a
-- version 6 that exists only in these tests. The copy of the database is
-- tested in "Haskoin.Wallet.BackupSpec".
--
-- The databases of the earlier versions are built with SQL, with a table
-- or two of each and the version table as each wrote it, in
-- "Haskoin.Wallet.Migration.TestUtils", which the workflow tests share.
module Haskoin.Wallet.MigrationSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.List (isInfixOf, isPrefixOf)
import Data.String.Conversions (cs)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..), rawExecute)
import qualified Database.Sqlite as Sqlite
import Haskoin (prepareContext)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Migration
import Haskoin.Wallet.Migration.TestUtils
import Haskoin.Wallet.TestUtils
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  describe "The migration of hw" $ do
    it "sees a database without tables as having no version" $
      inMemory $
        runExceptT databaseVersion `dbShouldBe` Right Nothing

    it "creates a new database at version 5 with the tables of this hw, without a backup" $
      inMemory $ do
        migrateAndLog databasePath ctx `dbShouldBe` (Right (), [])
        storedVersions `dbShouldBe` [(PersistInt64 5, "integer")]
        expected <- liftIO currentSchema
        databaseSchema `dbShouldBe` expected

    it "leaves a database at version 5 as it is" $
      inMemory $ do
        version5Database
        original <- databaseContent
        migrateAndLog databasePath ctx `dbShouldBe` (Right (), [])
        databaseContent `dbShouldBe` original

    forM_ olderDatabases $ \(version, made, database) -> do
      it ("refuses a database at version " <> version <> ", of " <> made <> ", with the instructions to create the wallet again") $
        inMemory $ do
          database
          migrateAndLog databasePath ctx `dbShouldBe` (Left (olderRefusal version), [])

      it ("leaves a database at version " <> version <> " as it was when it refuses it") $
        inMemory $ do
          database
          original <- databaseContent
          _ <- migrateAndLog databasePath ctx
          databaseContent `dbShouldBe` original

    it "quotes a path with a space in the commands that move the database aside" $
      inMemory $ do
        version2Database
        (result, _) <- migrateAndLog "/home/some one/.hw/accounts.sqlite" ctx
        liftIO $
          either id (const "") result
            `shouldSatisfy` isInfixOf "\n  mv '/home/some one/.hw/accounts.sqlite' '/home/some one/.hw/accounts-old.sqlite'\n"

    it "refuses a database at version 6, of a newer hw" $
      inMemory $ do
        version6Database
        migrateAndLog databasePath ctx
          `dbShouldBe` (Left "The database is at version 6, newer than the version 5 of this hw", [])

    it "leaves a database at version 6 as it was when it refuses it" $
      inMemory $ do
        version6Database
        original <- databaseContent
        _ <- migrateAndLog databasePath ctx
        databaseContent `dbShouldBe` original

  describe "The check of a database file before hw opens it to write" $ do
    it "lets hw create a database where there is no file" $
      inScratchDirectory $ \dir ->
        checkDatabaseFile ctx (dir </> "accounts.sqlite") `shouldReturn` Right ()

    it "lets hw open a database at version 5" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "accounts.sqlite"
        inDatabaseFile file version5Database
        checkDatabaseFile ctx file `shouldReturn` Right ()

    it "refuses a database of hw 0.9 with the instructions, naming its file" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "accounts.sqlite"
        journalModeDatabase file
        result <- checkDatabaseFile ctx file
        either id (const "") result
          `shouldSatisfy` isPrefixOf ("The database " <> file <> " was made by an earlier version of hw, at format version 2.\n")

    -- A connection that writes would turn the file to WAL mode first
    it "leaves a refused database file byte for byte as it was, in its own journal mode" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "accounts.sqlite"
        journalModeDatabase file
        original <- BS.readFile file
        _ <- checkDatabaseFile ctx file
        BS.readFile file `shouldReturn` original
        listDirectory dir `shouldReturn` ["accounts.sqlite"]

  describe "The frame of the migration, with a step from version 5 to 6" $ do
    it "backs up the database once, at version 5, then runs the step and tells its notice" $
      inMemory $ do
        version5Database
        migrateWithAndLog 6 [testStep]
          `dbShouldBe` ( Right (),
                         [ "Backup at version 5",
                           "Migrating the database to version 6: a test step",
                           "The test step tells the user this"
                         ]
                       )

    it "leaves the database as the step makes it, at version 6" $
      inMemory $ do
        version5Database
        _ <- migrateWithAndLog 6 [testStep]
        storedVersions `dbShouldBe` [(PersistInt64 6, "integer")]
        tableRows "d_b_test" `dbShouldBe` [[PersistInt64 1]]

    it "undoes everything the step did when it fails" $
      inMemory $ do
        version5Database
        original <- databaseContent
        migrateWithAndLog 6 [failingStep]
          `dbShouldBe` ( Left "The test step failed",
                         [ "Backup at version 5",
                           "Migrating the database to version 6: a failing step"
                         ]
                       )
        databaseContent `dbShouldBe` original

    it "refuses a database older than the version the step starts from, with the instructions" $
      inMemory $ do
        version4Database
        migrateWithAndLog 6 [testStep] `dbShouldBe` (Left (olderRefusal "4"), [])

-- | A database file of hw 0.9, its version table holding the text "0.9",
-- in the journal mode SQLite takes by default, rollback with a journal
-- file: written through SQLite itself, outside any transaction, since a
-- connection of persistent would turn it to WAL mode
journalModeDatabase :: FilePath -> IO ()
journalModeDatabase file = do
  connection <- Sqlite.open (cs file)
  forM_
    [ "PRAGMA journal_mode = DELETE",
      "CREATE TABLE d_b_version (version VARCHAR NOT NULL)",
      "INSERT INTO d_b_version VALUES ('0.9')",
      "CREATE TABLE d_b_account (name VARCHAR NOT NULL)",
      "INSERT INTO d_b_account VALUES ('main')"
    ]
    $ \sql -> do
      statement <- Sqlite.prepare connection sql
      _ <- Sqlite.step statement
      Sqlite.finalize statement
  Sqlite.close connection

-- | The refusal of a database at an earlier version, written as its
-- number, with the commands for the file at 'databasePath'
olderRefusal :: String -> String
olderRefusal version =
  "The database /home/someone/.hw/accounts.sqlite was made by an earlier version of hw, at format version "
    <> version
    <> ".\n\
       \hw 0.10.0 does not read it, and has left it as it is. To go on, move it aside\n\
       \and create the wallet again from its mnemonic:\n\
       \\n\
       \  mv /home/someone/.hw/accounts.sqlite /home/someone/.hw/accounts-old.sqlite\n\
       \  hw recover\n\
       \\n\
       \recover finds the portfolios of the mnemonic that were paid. On an offline\n\
       \computer, run hw newportfolio --existing instead, once for each account index\n\
       \the old wallet used, from 0 up to the highest. Then carry the portfolios to the\n\
       \online computer with exportwallet and importwallet. The labels of the old\n\
       \database are not carried over."

{- A step of the tests -}

-- | A step to version 6 that adds a table with a row, stores the version
-- 6 itself, as a step does, and has a notice for the user
testStep :: Step IO
testStep =
  Step 6 "a test step" $ do
    lift $ do
      rawExecute "create table d_b_test (id integer primary key)" []
      rawExecute "insert into d_b_test (id) values (1)" []
      P.deleteWhere ([] :: [P.Filter DBVersion])
      P.insert_ (DBVersion 6)
    return ["The test step tells the user this"]

-- | A step to version 6 that adds a table and stores the version 6, then
-- fails
failingStep :: Step IO
failingStep =
  Step 6 "a failing step" $ do
    lift $ do
      rawExecute "create table d_b_test (id integer primary key)" []
      P.deleteWhere ([] :: [P.Filter DBVersion])
      P.insert_ (DBVersion 6)
    throwError "The test step failed"

{- The database of this hw -}

-- | A database of this hw, with a best block
version5Database :: DB IO ()
version5Database = do
  createTables migrateAll
  P.insert_ $ DBVersion 5
  P.insert_ $ DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105 Nothing
  commitFixture
