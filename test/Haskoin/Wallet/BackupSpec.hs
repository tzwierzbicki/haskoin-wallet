{-# LANGUAGE OverloadedStrings #-}

-- | The copies of the database: the copy itself, and the backup command.
-- Then the copies of the labels: their names, where they go, and the
-- three hw keeps.
module Haskoin.Wallet.BackupSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import qualified Data.ByteString.Char8 as C8
import Data.Default (def)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (LocalTime (..), TimeOfDay (..), UTCTime (..), ZonedTime (..), fromGregorian, utc)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..), transactionSave)
import Haskoin.Wallet.Backup (backupDatabase, backupLabels, backupName, labelBackupDirectory, labelBackupName)
import Haskoin.Wallet.Commands (cmdBackup)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.TestUtils
import System.Directory (createDirectory, listDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec = do
  describe "The copy of the database" $ do
    it "is named after the database, the version and the time" $
      backupName "/home/someone/.hw/accounts.sqlite" 2 (ZonedTime (LocalTime (fromGregorian 2026 10 21) (TimeOfDay 14 5 33)) utc)
        `shouldBe` "accounts-version-2-2026-10-21T14-05-33.sqlite"

    it "goes into the given directory, created if missing" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            backups = dir </> "backups"
        inDatabaseFile database $ do
          createTables migrateAll
          P.insert_ $ DBVersion 5
          P.insert_ $ DBBest "btc" bestBlockHash 800105 Nothing
        copy <- either fail return =<< backupDatabase database backups 5
        takeDirectory copy `shouldBe` backups
        takeFileName copy `shouldSatisfy` \name -> "accounts-version-5-" `isPrefixOf` name && ".sqlite" `isSuffixOf` name
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]
        inDatabaseFile copy (tableRows "d_b_best") `shouldReturn` [[PersistText "btc", PersistText bestBlockHash, PersistInt64 800105, PersistNull]]

    -- SQLite writes the copy next to its path, and it then takes the path
    it "leaves no other file next to it" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            backups = dir </> "backups"
        inDatabaseFile database $ createTables migrateAll
        copy <- either fail return =<< backupDatabase database backups 5
        listDirectory backups `shouldReturn` [takeFileName copy]

    it "includes the rows still in the write-ahead log" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
        -- Committed while the connection is open: the rows are in the
        -- write-ahead log, not yet in the file
        copy <- inDatabaseFile database $ do
          createTables migrateAll
          P.insert_ $ DBVersion 5
          transactionSave
          liftIO $ either fail return =<< backupDatabase database (dir </> "backups") 5
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]

    -- A file stands where the directory would go
    it "is not made when its directory cannot be created, and says why" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            backups = dir </> "taken" </> "backups"
        inDatabaseFile database $ createTables migrateAll
        writeFile (dir </> "taken") ""
        backupDatabase database backups 5
          `shouldReturn` Left ("The directory " <> backups <> " could not be created: not a directory")

  describe "The backup command" $ do
    it "copies the database to the given file" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            copy = dir </> "copy.sqlite"
        inDatabaseFile database $ do
          createTables migrateAll
          P.insert_ $ DBVersion 5
          P.insert_ $ DBBest "btc" bestBlockHash 800105 Nothing
        cmdBackup (configUsing database) (Just copy) `shouldReturn` ResponseFile copy
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]
        inDatabaseFile copy (tableRows "d_b_best")
          `shouldReturn` [[PersistText "btc", PersistText bestBlockHash, PersistInt64 800105, PersistNull]]

    it "refuses to overwrite a file" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            copy = dir </> "copy.sqlite"
        inDatabaseFile database $ createTables migrateAll
        _ <- cmdBackup (configUsing database) (Just copy)
        cmdBackup (configUsing database) (Just copy)
          `shouldReturn` ResponseError (cs $ "Path '" <> copy <> "' already exists")

    it "says why it could not copy the database into a directory that does not exist" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            copy = dir </> "missing" </> "copy.sqlite"
        inDatabaseFile database $ createTables migrateAll
        cmdBackup (configUsing database) (Just copy)
          `shouldReturn` ResponseError (cs $ "The database could not be copied to " <> copy <> ": no such file or directory")

    it "says why it could not copy a file that is not a database, and leaves no file behind" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            copy = dir </> "copy.sqlite"
        writeFile database "not a database\n"
        cmdBackup (configUsing database) (Just copy)
          `shouldReturn` ResponseError (cs $ "The database could not be copied to " <> copy <> ": it is not an SQLite database")
        listDirectory dir `shouldReturn` ["accounts.sqlite"]

  labelCopiesSpec

labelCopiesSpec :: Spec
labelCopiesSpec =
  describe "The copies of the labels" $ do
    it "are named after the database and the time in UTC, to the millisecond" $
      labelBackupName "/home/someone/.hw/accounts.sqlite" copyTime1
        `shouldBe` "accounts-labels-2026-11-14T10-05-33.123Z.jsonl"

    -- Every name has the same width, so that the names sort in the order
    -- of their times
    it "write the milliseconds when there are none" $
      labelBackupName "/home/someone/.hw/accounts.sqlite" copyTime3
        `shouldBe` "accounts-labels-2026-11-14T10-20-00.000Z.jsonl"

    it "go into the backups directory next to the database" $
      labelBackupDirectory "/home/someone/.hw/accounts.sqlite" `shouldBe` "/home/someone/.hw/backups"

    it "start with a first copy, which holds the labels given" $
      inScratchDirectory $ \dir -> do
        let copy = dir </> "backups" </> "accounts-labels-2026-11-14T10-05-33.123Z.jsonl"
        backupLabels (dir </> "accounts.sqlite") copyTime1 "first\n" `shouldReturn` Right (Just copy)
        readFile copy `shouldReturn` "first\n"

    it "are not written again when the newest copy holds the same labels" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
        _ <- backupLabels database copyTime1 "first\n"
        backupLabels database copyTime2 "first\n" `shouldReturn` Right Nothing
        listDirectory (dir </> "backups") `shouldReturn` ["accounts-labels-2026-11-14T10-05-33.123Z.jsonl"]

    -- The user labelled an address, then took the label back
    it "are written again when only an older copy holds the same labels" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
        _ <- backupLabels database copyTime1 "first\n"
        _ <- backupLabels database copyTime2 "second\n"
        backupLabels database copyTime3 "first\n"
          `shouldReturn` Right (Just (dir </> "backups" </> "accounts-labels-2026-11-14T10-20-00.000Z.jsonl"))

    it "are three at most: the fourth removes the oldest" $
      inScratchDirectory $ \dir -> do
        writeFourCopies (dir </> "accounts.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory (dir </> "backups")
          `shouldReturn` [ "accounts-labels-2026-11-14T10-05-33.456Z.jsonl",
                           "accounts-labels-2026-11-14T10-20-00.000Z.jsonl",
                           "accounts-labels-2026-11-15T08-00-00.000Z.jsonl"
                         ]

    -- The clock was set back before the fourth copy, whose name now
    -- sorts first. It holds the labels as they are now.
    it "keep the one just written, even when its time is the oldest, and remove the oldest of the others" $
      inScratchDirectory $ \dir -> do
        writeFourCopies (dir </> "accounts.sqlite") [copyTime2, copyTime3, copyTime4, copyTime1]
        sort <$> listDirectory (dir </> "backups")
          `shouldReturn` [ "accounts-labels-2026-11-14T10-05-33.123Z.jsonl",
                           "accounts-labels-2026-11-14T10-20-00.000Z.jsonl",
                           "accounts-labels-2026-11-15T08-00-00.000Z.jsonl"
                         ]

    -- A copy of the database, a copy of the labels of another database,
    -- one with a name of a time hw does not write, and a file of the user
    it "leave alone the files of the directory that are no copies of the labels of the database" $
      inScratchDirectory $ \dir -> do
        let backups = dir </> "backups"
            others =
              [ "accounts-labels-2026-11-14T10-05-33Z.jsonl",
                "accounts-version-5-2026-11-14T10-05-33.sqlite",
                "notes.txt",
                "savings-labels-2026-11-14T10-05-33.123Z.jsonl"
              ]
        createDirectory backups
        forM_ others $ \name -> writeFile (backups </> name) "kept\n"
        writeFourCopies (dir </> "accounts.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory backups
          `shouldReturn` [ "accounts-labels-2026-11-14T10-05-33.456Z.jsonl",
                           "accounts-labels-2026-11-14T10-05-33Z.jsonl",
                           "accounts-labels-2026-11-14T10-20-00.000Z.jsonl",
                           "accounts-labels-2026-11-15T08-00-00.000Z.jsonl",
                           "accounts-version-5-2026-11-14T10-05-33.sqlite",
                           "notes.txt",
                           "savings-labels-2026-11-14T10-05-33.123Z.jsonl"
                         ]

    -- A copy shows every address of every wallet
    it "go into a backups directory its owner alone can enter, whatever the umask lets" $
      inScratchDirectory $ \dir -> underUmask 0o022 $ do
        _ <- backupLabels (dir </> "accounts.sqlite") copyTime1 "first\n"
        -- The permission bits of rwx------
        (.&. 0o777) . fileMode <$> getFileStatus (dir </> "backups") `shouldReturn` 0o700

    -- A file stands where the directory would go
    it "say why a copy could not be written" $
      inScratchDirectory $ \dir -> do
        let copy = dir </> "backups" </> "accounts-labels-2026-11-14T10-05-33.123Z.jsonl"
        writeFile (dir </> "backups") ""
        backupLabels (dir </> "accounts.sqlite") copyTime1 "first\n"
          `shouldReturn` Left ("The file " <> copy <> " could not be written: not a directory")

-- | Write four copies of the labels of a database, at the times given in
-- their order, each with other labels
writeFourCopies :: FilePath -> [UTCTime] -> IO ()
writeFourCopies database times =
  forM_ (zip times ["first\n", "second\n", "third\n", "fourth\n"]) $ \(time, labels) ->
    either fail (const (return ())) =<< backupLabels database time (C8.pack labels)

-- | The times of the copies of the labels: two in the same second, one
-- later that day, and one the next day
copyTime1, copyTime2, copyTime3, copyTime4 :: UTCTime
-- 10:05:33.123 is 36000 + 300 + 33.123 seconds into the day
copyTime1 = UTCTime (fromGregorian 2026 11 14) 36333.123
copyTime2 = UTCTime (fromGregorian 2026 11 14) 36333.456
-- 10:20:00 is 36000 + 1200 seconds into the day
copyTime3 = UTCTime (fromGregorian 2026 11 14) 37200
-- 08:00:00 is 8 × 3600 = 28800 seconds into the day
copyTime4 = UTCTime (fromGregorian 2026 11 15) 28800

-- | The hash of the best block the databases of the tests hold
bestBlockHash :: Text
bestBlockHash = "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054"

-- | The configuration of an hw whose database is the given file
configUsing :: FilePath -> Config
configUsing database = def {configDatabaseFile = Just database}
