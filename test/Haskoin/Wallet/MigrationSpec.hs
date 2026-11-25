{-# LANGUAGE OverloadedStrings #-}

-- | What this hw makes of a database, by its format version: a new one
-- gets the tables of version 5, one of version 5 opens as it is, and any
-- other is refused and left as it was, with instructions for one of an
-- earlier hw. hw 0.10 migrates no earlier database, so the frame of the
-- migration is tested here with a step of its own, from version 5 to a
-- version 6 that exists only in these tests. The copy of the database is
-- tested in "Haskoin.Wallet.BackupSpec".
--
-- The databases hw refuses are built with SQL, in
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
import Haskoin.Wallet.Database
import Haskoin.Wallet.Migration
import Haskoin.Wallet.Migration.TestUtils
import Haskoin.Wallet.TestUtils
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "The migration of hw" $ do
    it "sees a database without tables as a new one, with no tables" $
      inMemory $
        storedVersion `dbShouldBe` NoTables

    it "creates a new database at version 5 with the tables of this hw, without a backup" $
      inMemory $ do
        migrateAndLog databasePath `dbShouldBe` (Right (), [])
        storedVersions `dbShouldBe` [(PersistInt64 5, "integer")]
        expected <- liftIO currentSchema
        databaseSchema `dbShouldBe` expected

    it "leaves a database at version 5 as it is" $
      inMemory $ do
        version5Database
        original <- databaseContent
        migrateAndLog databasePath `dbShouldBe` (Right (), [])
        databaseContent `dbShouldBe` original

    it "refuses a database at an earlier version, 4, with the instructions to create the wallet again" $
      inMemory $ do
        version4Database
        migrateAndLog databasePath `dbShouldBe` (Left (olderRefusal "4"), [])

    it "leaves a database at an earlier version as it was when it refuses it" $
      inMemory $ do
        version4Database
        original <- databaseContent
        _ <- migrateAndLog databasePath
        databaseContent `dbShouldBe` original

    -- Any SQLite file of another program: hw must not write its tables
    -- beside those of the file
    it "refuses a database that has tables and gives no format version, as not a database of hw, with the command that moves it aside" $
      inMemory $ do
        otherProgramDatabase
        migrateAndLog databasePath
          `dbShouldBe` ( Left
                           "The database /home/someone/.hw/portfolios.sqlite does not give a format version, so it is not a database of hw. hw has left it as it is. To go on, move it aside:\n\
                           \\n\
                           \  mv /home/someone/.hw/portfolios.sqlite /home/someone/.hw/portfolios-old.sqlite",
                         []
                       )

    it "leaves a database that gives no format version as it was when it refuses it" $
      inMemory $ do
        otherProgramDatabase
        original <- databaseContent
        _ <- migrateAndLog databasePath
        databaseContent `dbShouldBe` original

    it "quotes a path with a space in the commands that move the database aside" $
      inMemory $ do
        version4Database
        (result, _) <- migrateAndLog "/home/some one/.hw/portfolios.sqlite"
        liftIO $
          either id (const "") result
            `shouldSatisfy` isInfixOf "\n  mv '/home/some one/.hw/portfolios.sqlite' '/home/some one/.hw/portfolios-old.sqlite'\n"

    it "refuses a database at version 6, of a newer hw" $
      inMemory $ do
        version6Database
        migrateAndLog databasePath
          `dbShouldBe` (Left "The database is at version 6, newer than the version 5 of this hw", [])

    it "leaves a database at version 6 as it was when it refuses it" $
      inMemory $ do
        version6Database
        original <- databaseContent
        _ <- migrateAndLog databasePath
        databaseContent `dbShouldBe` original

  describe "The check of a database file before hw opens it to write" $ do
    it "lets hw create a database where there is no file" $
      inScratchDirectory $ \dir ->
        checkDatabaseFile (dir </> "portfolios.sqlite") `shouldReturn` Right ()

    it "lets hw open a database at version 5" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "portfolios.sqlite"
        inDatabaseFile file version5Database
        checkDatabaseFile file `shouldReturn` Right ()

    it "refuses a database of an earlier version with the instructions, naming its file" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "portfolios.sqlite"
        journalModeDatabase file
        result <- checkDatabaseFile file
        either id (const "") result
          `shouldSatisfy` isPrefixOf ("The database " <> file <> " was made by an earlier version of hw, at format version 4. ")

    -- A connection that writes would turn the file to WAL mode first
    it "leaves a refused database file byte for byte as it was, in its own journal mode" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "portfolios.sqlite"
        journalModeDatabase file
        original <- BS.readFile file
        _ <- checkDatabaseFile file
        BS.readFile file `shouldReturn` original
        listDirectory dir `shouldReturn` ["portfolios.sqlite"]

  describe "The frame of the migration, with a step from version 5 to 6" $ do
    it "backs up the database once, at version 5, then runs the step and tells its notice" $
      inMemory $ do
        version5Database
        migrateWithAndLog 6 [testStep]
          `dbShouldBe` ( Right (),
                         [ "Backup at version 5, with 0 wallets and 0 portfolios",
                           "Migrating the database to version 6: a test step",
                           "The test step tells the user this"
                         ]
                       )

    -- wallet0 holds portfolio0 and portfolio1 of the test mnemonic, and
    -- wallet1 portfolio0 of the second one: 2 wallets, 3 portfolios
    it "names the backup with the counts of wallets and portfolios of the database before the step" $
      inMemory $ do
        version5Database
        _ <- runExceptT $ portfolioOf testMnemonic >> nextPortfolioOf testMnemonic >> portfolioOf testMnemonic2
        commitFixture
        (take 1 . snd <$> migrateWithAndLog 6 [testStep])
          `dbShouldBe` ["Backup at version 5, with 2 wallets and 3 portfolios"]

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
                         [ "Backup at version 5, with 0 wallets and 0 portfolios",
                           "Migrating the database to version 6: a failing step"
                         ]
                       )
        databaseContent `dbShouldBe` original

    it "refuses a database older than the version the step starts from, with the instructions" $
      inMemory $ do
        version4Database
        migrateWithAndLog 6 [testStep] `dbShouldBe` (Left (olderRefusal "4"), [])

-- | A database file of an earlier version, 4, in the journal mode SQLite
-- takes by default, rollback with a journal
-- file: written through SQLite itself, outside any transaction, since a
-- connection of persistent would turn it to WAL mode
journalModeDatabase :: FilePath -> IO ()
journalModeDatabase file = do
  connection <- Sqlite.open (cs file)
  forM_
    [ "PRAGMA journal_mode = DELETE",
      "CREATE TABLE d_b_version (version INTEGER NOT NULL)",
      "INSERT INTO d_b_version VALUES (4)",
      "CREATE TABLE d_b_wallet (\"index\" INTEGER NOT NULL)",
      "INSERT INTO d_b_wallet VALUES (0)"
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
  "The database /home/someone/.hw/portfolios.sqlite was made by an earlier version of hw, at format version "
    <> version
    <> ". hw 0.10.0 does not read it, and has left it as it is. To go on, move it aside and create the wallet again from its mnemonic:\n\
       \\n\
       \  mv /home/someone/.hw/portfolios.sqlite /home/someone/.hw/portfolios-old.sqlite\n\
       \  hw recover\n\
       \\n\
       \recover finds the portfolios of the mnemonic that were paid. On an offline computer, run hw newportfolio --existing instead, once for each account index the old wallet used, from 0 up to the highest. Then carry the portfolios to the online computer with exportwallet and importwallet. The labels of the old database are not carried over."

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
