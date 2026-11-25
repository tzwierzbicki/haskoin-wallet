{-# LANGUAGE OverloadedStrings #-}

-- | The copies of the database: the copy itself, and the backup command,
-- which copies the database and its labels into user-backups and keeps
-- them all. Then the copies hw makes by itself into auto-backups, of the
-- database and of the labels: their names, where they go, how many of
-- each hw keeps, and which files of the directory the rotation takes for
-- copies. The backup hw makes before a command loses something is two of
-- those copies. hw keeps the copies by their age: the newest ones, and the
-- last copy of each of the latest hours, days, weeks and months. Most
-- tests of the files here keep the three newest alone, so that a fourth
-- copy shows the rotation.
module Haskoin.Wallet.Database.BackupSpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import qualified Data.ByteString.Char8 as C8
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (UTCTime (..), defaultTimeLocale, fromGregorian, parseTimeOrError)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..), transactionSave)
import Haskoin.Wallet.Database.Backup (BackupsShown (..), CopiesShown (..), CopyStamp (..), Retention (..), autoBackupDirectory, autoBackupRetention, backupDatabase, backupsShown, databaseCopyName, databaseCopyStamp, labelCopyName, labelCopyStamp, migrationBackupDirectory, retained, rollBackup, rollDatabaseCopy, rollLabelCopy, userBackup, userBackupDirectory)
import Haskoin.Wallet.Commands (cmdBackup, cmdExportWallet)
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO (newFileNextTo)
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.TestUtils
import System.Directory (createDirectory, createFileLink, doesDirectoryExist, getPermissions, listDirectory, setOwnerReadable, setPermissions)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec = do
  describe "The copy of the database" $ do
    -- The stamp is what the migration read of the database: 2 wallets
    -- and 3 portfolios at 10:05:33.123 UTC
    it "is named, before a migration, as the copies of the database in the other directories" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        backupDatabase database (dir </> "migration-backups") 5 (CopyStamp copyTime1 2 3)
          `shouldReturn` Right (dir </> "migration-backups" </> "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite")

    it "goes, before a migration, into the migration-backups directory next to the database" $
      migrationBackupDirectory "/home/someone/.hw/portfolios.sqlite" `shouldBe` "/home/someone/.hw/migration-backups"

    it "goes into the given directory, created if missing" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "backups"
        inDatabaseFile database $ do
          createTables migrateAll
          P.insert_ $ DBVersion 5
          P.insert_ $ DBBest "btc" bestBlockHash 800105 Nothing
        copy <- either fail return =<< backupDatabase database backups 5 (CopyStamp copyTime1 0 0)
        takeDirectory copy `shouldBe` backups
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]
        inDatabaseFile copy (tableRows "d_b_best") `shouldReturn` [[PersistText "btc", PersistText bestBlockHash, PersistInt64 800105, PersistNull]]

    -- SQLite writes the copy next to its path, and it then takes the path
    it "leaves no other file next to it" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "backups"
        inDatabaseFile database $ createTables migrateAll
        copy <- either fail return =<< backupDatabase database backups 5 (CopyStamp copyTime1 0 0)
        listDirectory backups `shouldReturn` [takeFileName copy]

    it "includes the rows still in the write-ahead log" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        -- Committed while the connection is open: the rows are in the
        -- write-ahead log, not yet in the file
        copy <- inDatabaseFile database $ do
          createTables migrateAll
          P.insert_ $ DBVersion 5
          transactionSave
          liftIO $ either fail return =<< backupDatabase database (dir </> "backups") 5 (CopyStamp copyTime1 0 0)
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]

    -- A file stands where the directory would go
    it "is not made when its directory cannot be created, and says why" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "taken" </> "backups"
        inDatabaseFile database $ createTables migrateAll
        writeFile (dir </> "taken") ""
        backupDatabase database backups 5 (CopyStamp copyTime1 0 0)
          `shouldReturn` Left ("The directory " <> backups <> " could not be created: not a directory")

  describe "The backup command" $ do
    it "copies the database to the given file" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "copy.sqlite"
        inDatabaseFile database $ do
          createTables migrateAll
          P.insert_ $ DBVersion 5
          P.insert_ $ DBBest "btc" bestBlockHash 800105 Nothing
        cmdBackup (configIn dir) (Just copy) `shouldReturn` ResponseFile copy
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]
        inDatabaseFile copy (tableRows "d_b_best")
          `shouldReturn` [[PersistText "btc", PersistText bestBlockHash, PersistInt64 800105, PersistNull]]

    it "refuses to overwrite a file" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "copy.sqlite"
        inDatabaseFile database $ createTables migrateAll
        _ <- cmdBackup (configIn dir) (Just copy)
        cmdBackup (configIn dir) (Just copy)
          `shouldReturn` ResponseError (cs $ "Path '" <> copy <> "' already exists")

    it "says why it could not copy the database into a directory that does not exist" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "missing" </> "copy.sqlite"
        inDatabaseFile database $ createTables migrateAll
        cmdBackup (configIn dir) (Just copy)
          `shouldReturn` ResponseError (cs $ "The database could not be copied to " <> copy <> ": no such file or directory")

    it "says why it could not copy a file that is not a database, and leaves no file behind" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "copy.sqlite"
        writeFile database "not a database\n"
        cmdBackup (configIn dir) (Just copy)
          `shouldReturn` ResponseError (cs $ "The database could not be copied to " <> copy <> ": it is not an SQLite database")
        listDirectory dir `shouldReturn` ["portfolios.sqlite"]

    it "copies the database to the given file alone, and writes nothing into user-backups" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        _ <- cmdBackup (configIn dir) (Just (dir </> "copy.sqlite"))
        doesDirectoryExist (dir </> "user-backups") `shouldReturn` False

    it "without a file, puts the copy of the database and its labels into user-backups next to the database" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInDirectory dir testPortfolio
        written <- filesOf <$> cmdBackup wallet Nothing
        map takeDirectory <$> written `shouldBe` Right [dir </> "user-backups", dir </> "user-backups"]

    -- The time in the middle of the name changes from run to run
    it "without a file, names the copy after the version and the counts of wallets and portfolios, and its labels as a copy of the labels" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInDirectory dir testPortfolio
        written <- filesOf <$> cmdBackup wallet Nothing
        let namedLike start end file = start `isPrefixOf` takeFileName file && end `isSuffixOf` takeFileName file
        case written of
          Right [copy, labels] -> do
            copy `shouldSatisfy` namedLike "portfolios-version-5-" "-wallets-1-portfolios-1.sqlite"
            labels `shouldSatisfy` namedLike "portfolios-labels-" "-wallets-1-portfolios-1.jsonl"
          other -> expectationFailure ("Not a backup: " <> show other)

    it "without a file, writes the labels exportwallet writes" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInDirectory dir testPortfolio
        _ <- cmdExportWallet wallet Nothing Nothing (dir </> "wallet.jsonl")
        exported <- readFile (dir </> "wallet.jsonl")
        written <- filesOf <$> cmdBackup wallet Nothing
        case written of
          Right [_, labels] -> readFile labels `shouldReturn` exported
          other -> expectationFailure ("Not a backup: " <> show other)

  describe "The backups of the user" $ do
    it "are named as the copies of the auto-backups directory: the database with its version, the labels as labels, both with the time in UTC to the millisecond and the counts of wallets and portfolios" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "user-backups"
        newDatabase database
        userBackup database 5 (CopyStamp copyTime1 2 3) "labels\n"
          `shouldReturn` Right
            ( backups </> "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite",
              backups </> "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl"
            )

    it "go into the user-backups directory next to the database" $
      userBackupDirectory "/home/someone/.hw/portfolios.sqlite" `shouldBe` "/home/someone/.hw/user-backups"

    it "are all kept: a fourth leaves the three before it" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        forM_ (zip [copyTime1, copyTime2, copyTime3, copyTime4] [1 ..]) $ \(time, portfolios) ->
          either fail (const (return ())) =<< userBackup database 5 (CopyStamp time 1 portfolios) "labels\n"
        sort <$> listDirectory (dir </> "user-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-2.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-3.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.jsonl",
                           "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.sqlite",
                           "portfolios-version-5-2026-11-14T10-05-33.456Z-wallets-1-portfolios-2.sqlite",
                           "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-3.sqlite",
                           "portfolios-version-5-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.sqlite"
                         ]

  copyNamesSpec
  databaseCopiesSpec
  labelCopiesSpec
  backupBeforeLossSpec
  rotationSpec
  retentionSpec
  backupsShownSpec

{- The names of the copies hw makes by itself -}

copyNamesSpec :: Spec
copyNamesSpec =
  describe "The name of a copy hw makes by itself" $ do
    it "is, for the database, its name, the version, the time in UTC to the millisecond, and the counts of wallets and portfolios" $
      databaseCopyName "/home/someone/.hw/portfolios.sqlite" 5 (CopyStamp copyTime1 2 3)
        `shouldBe` "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite"

    it "is, for the labels, the name of the database, the time in UTC to the millisecond, and the counts of wallets and portfolios" $
      labelCopyName "/home/someone/.hw/portfolios.sqlite" (CopyStamp copyTime1 2 3)
        `shouldBe` "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl"

    -- Every name has the same width up to the counts, so that the names
    -- sort in the order of their times
    it "writes the milliseconds when there are none" $
      labelCopyName "/home/someone/.hw/portfolios.sqlite" (CopyStamp copyTime3 2 3)
        `shouldBe` "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-2-portfolios-3.jsonl"

    it "reads back as the version, the time and the counts it gives, for a copy of the database" $
      databaseCopyStamp "/home/someone/.hw/portfolios.sqlite" "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite"
        `shouldBe` Just (5, CopyStamp copyTime1 2 3)

    it "reads back as the time and the counts it gives, for a copy of the labels" $
      labelCopyStamp "/home/someone/.hw/portfolios.sqlite" "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl"
        `shouldBe` Just (CopyStamp copyTime1 2 3)

    -- Each differs from the name hw writes in one place: the name of the
    -- database, the kind, a part left out, a time written otherwise, a
    -- number hw writes otherwise, something added, or the extension
    it "is not the name of a file that differs from it anywhere, for a copy of the database" $
      forM_
        [ "savings-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite",
          "my-portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite",
          "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite",
          "portfolios-version-5-2026-11-14T10-05-33.123Z.sqlite",
          "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2.sqlite",
          "portfolios-version-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite",
          "portfolios-version-5-2026-11-14T10-05-33Z-wallets-2-portfolios-3.sqlite",
          "portfolios-version-5-2026-11-14T10-05-33.123-wallets-2-portfolios-3.sqlite",
          "portfolios-version-5-2026-11-14T10:05:33.123Z-wallets-2-portfolios-3.sqlite",
          "portfolios-version-5-2026-13-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite",
          "portfolios-version-05-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite",
          "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-02-portfolios-3.sqlite",
          "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3-mine.sqlite",
          "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3 (copy).sqlite",
          "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite.bak",
          "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl",
          "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3"
        ]
        $ \name -> (name, databaseCopyStamp "/home/someone/.hw/portfolios.sqlite" name) `shouldBe` (name, Nothing)

    it "is not the name of a file that differs from it anywhere, for a copy of the labels" $
      forM_
        [ "savings-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl",
          "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl",
          "portfolios-labels-2026-11-14T10-05-33.123Z.jsonl",
          "portfolios-labels-2026-11-14T10-05-33Z-wallets-2-portfolios-3.jsonl",
          "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-03.jsonl",
          "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3-mine.jsonl",
          "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.json",
          "labels.jsonl"
        ]
        $ \name -> (name, labelCopyStamp "/home/someone/.hw/portfolios.sqlite" name) `shouldBe` (name, Nothing)

    -- hw fills a new file next to the copy before it gives it the name of
    -- the copy. A crash leaves that file behind, perhaps half written.
    it "is not the name of the new file a write fills before it takes that name" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        beingCopied <- newFileNextTo (dir </> "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite")
        beingWritten <- newFileNextTo (dir </> "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl")
        (databaseCopyStamp database (takeFileName beingCopied), labelCopyStamp database (takeFileName beingWritten))
          `shouldBe` (Nothing, Nothing)

{- The copies of the database hw makes by itself -}

databaseCopiesSpec :: Spec
databaseCopiesSpec =
  describe "The copies of the database hw makes by itself" $ do
    it "go into the auto-backups directory next to the database" $
      autoBackupDirectory "/home/someone/.hw/portfolios.sqlite" `shouldBe` "/home/someone/.hw/auto-backups"

    it "start with a first copy, a database at its version" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "auto-backups" </> "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.sqlite"
        newDatabase database
        rollDatabaseCopy (newestAlone 3) database 5 (CopyStamp copyTime1 1 1) `shouldReturn` Right copy
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]

    -- The portfolios a user makes one after the other
    it "are three at most when three are kept: the fourth removes the oldest" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        rollFourCopies (newestAlone 3) database
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-version-5-2026-11-14T10-05-33.456Z-wallets-1-portfolios-2.sqlite",
                           "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-3.sqlite",
                           "portfolios-version-5-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.sqlite"
                         ]

    -- Files the user keeps there: a copy named by hand, a copy of the
    -- database without counts, a copy with something added to its name, a
    -- copy of another database, and notes
    it "leave alone the files of the directory that are no copies of the database" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "auto-backups"
            others =
              [ "before-the-move.sqlite",
                "notes.txt",
                "portfolios-version-5-2026-10-21T14-05-33.sqlite",
                "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1-mine.sqlite",
                "savings-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.sqlite"
              ]
        newDatabase database
        createDirectory backups
        forM_ others $ \name -> writeFile (backups </> name) "kept\n"
        rollFourCopies (newestAlone 3) database
        sort <$> listDirectory backups
          `shouldReturn` [ "before-the-move.sqlite",
                           "notes.txt",
                           "portfolios-version-5-2026-10-21T14-05-33.sqlite",
                           "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1-mine.sqlite",
                           "portfolios-version-5-2026-11-14T10-05-33.456Z-wallets-1-portfolios-2.sqlite",
                           "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-3.sqlite",
                           "portfolios-version-5-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.sqlite",
                           "savings-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.sqlite"
                         ]

    -- Three of each are kept: 3 + 3 = 6
    it "are counted apart from the copies of the labels, which they do not remove" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        writeLabelCopies (newestAlone 3) database [copyTime1, copyTime2, copyTime3, copyTime4]
        rollFourCopies (newestAlone 3) database
        length <$> listDirectory (dir </> "auto-backups") `shouldReturn` 6

    -- A file stands where the directory would go
    it "say why a copy could not be made" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "auto-backups" </> "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.sqlite"
        newDatabase database
        writeFile (dir </> "auto-backups") ""
        rollDatabaseCopy (newestAlone 3) database 5 (CopyStamp copyTime1 1 1)
          `shouldReturn` Left ("The database could not be copied to " <> copy <> ": not a directory")

-- | Copy a database four times into its auto-backups directory, at the
-- four times of the copies, as a user who makes a portfolio each time:
-- with 1, 2, 3 and 4 portfolios. The retention given first says which
-- copies each rotation keeps.
rollFourCopies :: Retention -> FilePath -> IO ()
rollFourCopies kept database =
  forM_ (zip [copyTime1, copyTime2, copyTime3, copyTime4] [1 ..]) $ \(time, portfolios) ->
    either fail (const (return ())) =<< rollDatabaseCopy kept database 5 (CopyStamp time 1 portfolios)

-- | A database at version 5 without a wallet, in the file given
newDatabase :: FilePath -> IO ()
newDatabase database =
  inDatabaseFile database $ do
    createTables migrateAll
    P.insert_ $ DBVersion 5

-- | The two files of the answer of a backup to the user-backups
-- directory: the copy of the database, then its labels
filesOf :: Response -> Either String [FilePath]
filesOf (ResponseBackup database labels) = Right [database, labels]
filesOf other = Left (show other)

{- The copies of the labels hw makes by itself -}

-- Each copy is of a database of 1 wallet and 1 portfolio
labelCopiesSpec :: Spec
labelCopiesSpec =
  describe "The copies of the labels" $ do
    it "start with a first copy in the auto-backups directory, which holds the labels given" $
      inScratchDirectory $ \dir -> do
        let copy = dir </> "auto-backups" </> "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl"
        rollLabelCopy (newestAlone 3) (dir </> "portfolios.sqlite") (CopyStamp copyTime1 1 1) "first\n" `shouldReturn` Right (Just copy)
        readFile copy `shouldReturn` "first\n"

    it "are not written again when the newest copy holds the same labels" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        _ <- rollLabelCopy (newestAlone 3) database (CopyStamp copyTime1 1 1) "first\n"
        rollLabelCopy (newestAlone 3) database (CopyStamp copyTime2 1 1) "first\n" `shouldReturn` Right Nothing
        listDirectory (dir </> "auto-backups") `shouldReturn` ["portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl"]

    -- The user labelled an address, then took the label back
    it "are written again when only an older copy holds the same labels" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        _ <- rollLabelCopy (newestAlone 3) database (CopyStamp copyTime1 1 1) "first\n"
        _ <- rollLabelCopy (newestAlone 3) database (CopyStamp copyTime2 1 1) "second\n"
        rollLabelCopy (newestAlone 3) database (CopyStamp copyTime3 1 1) "first\n"
          `shouldReturn` Right (Just (dir </> "auto-backups" </> "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl"))

    it "are three at most when three are kept: the fourth removes the oldest" $
      inScratchDirectory $ \dir -> do
        writeLabelCopies (newestAlone 3) (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- Files the user keeps there: an export of their own, a copy of the
    -- labels without counts, a copy with something added to its name, a
    -- copy of the labels of another database, and notes
    it "leave alone the files of the directory that are no copies of the labels of the database" $
      inScratchDirectory $ \dir -> do
        let backups = dir </> "auto-backups"
            others =
              [ "my-wallet.jsonl",
                "notes.txt",
                "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1-mine.jsonl",
                "portfolios-labels-2026-11-14T10-05-33.123Z.jsonl",
                "savings-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl"
              ]
        createDirectory backups
        forM_ others $ \name -> writeFile (backups </> name) "kept\n"
        writeLabelCopies (newestAlone 3) (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory backups
          `shouldReturn` [ "my-wallet.jsonl",
                           "notes.txt",
                           "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1-mine.jsonl",
                           "portfolios-labels-2026-11-14T10-05-33.123Z.jsonl",
                           "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl",
                           "savings-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- A copy shows every address of every wallet
    it "go into an auto-backups directory its owner alone can enter, whatever the umask lets" $
      inScratchDirectory $ \dir -> underUmask 0o022 $ do
        _ <- rollLabelCopy (newestAlone 3) (dir </> "portfolios.sqlite") (CopyStamp copyTime1 1 1) "first\n"
        -- The permission bits of rwx------
        (.&. 0o777) . fileMode <$> getFileStatus (dir </> "auto-backups") `shouldReturn` 0o700

    -- A file stands where the directory would go
    it "say why a copy could not be written" $
      inScratchDirectory $ \dir -> do
        let copy = dir </> "auto-backups" </> "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl"
        writeFile (dir </> "auto-backups") ""
        rollLabelCopy (newestAlone 3) (dir </> "portfolios.sqlite") (CopyStamp copyTime1 1 1) "first\n"
          `shouldReturn` Left ("The file " <> copy <> " could not be written: not a directory")

{- The backup before a loss -}

-- deletewallet, deleteportfolio and restoredatabase make this backup
-- before they lose what the database holds
backupBeforeLossSpec :: Spec
backupBeforeLossSpec =
  describe "The backup hw makes before a command loses something" $ do
    it "is a copy of the database and a copy of its labels in the auto-backups directory, named as the copies hw makes after a command" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "auto-backups"
        newDatabase database
        rollBackup (newestAlone 3) database 5 (CopyStamp copyTime1 2 3) "labels\n"
          `shouldReturn` Right
            ( backups </> "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite",
              backups </> "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl"
            )

    it "holds the labels given" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        made <- rollBackup (newestAlone 3) database 5 (CopyStamp copyTime1 2 3) "labels\n"
        mapM (readFile . snd) made `shouldReturn` Right "labels\n"

    -- The copy of the labels of 10:05:33.123 holds the same labels
    it "names the newest copy of the labels when it holds the same labels, and writes no second one" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "auto-backups"
        newDatabase database
        _ <- rollLabelCopy (newestAlone 3) database (CopyStamp copyTime1 1 1) "first\n"
        rollBackup (newestAlone 3) database 5 (CopyStamp copyTime3 1 1) "first\n"
          `shouldReturn` Right
            ( backups </> "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.sqlite",
              backups </> "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl"
            )

    -- Three copies of the database are kept, and the backup is the fourth
    it "is kept by the rules of the other copies: it removes the oldest copy of the database" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        forM_ (zip [copyTime1, copyTime2, copyTime3] [1 ..]) $ \(time, portfolios) ->
          either fail (const (return ())) =<< rollDatabaseCopy (newestAlone 3) database 5 (CopyStamp time 1 portfolios)
        _ <- rollBackup (newestAlone 3) database 5 (CopyStamp copyTime4 1 4) "labels\n"
        sort . filter (".sqlite" `isSuffixOf`) <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-version-5-2026-11-14T10-05-33.456Z-wallets-1-portfolios-2.sqlite",
                           "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-3.sqlite",
                           "portfolios-version-5-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.sqlite"
                         ]

    -- Three copies are kept, and the copy after the command is the
    -- fourth since the backup
    it "is removed in its turn, once newer copies of the database take its place" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        _ <- rollBackup (newestAlone 3) database 5 (CopyStamp copyTime1 1 1) "labels\n"
        forM_ (zip [copyTime2, copyTime3, copyTime4] [2 ..]) $ \(time, portfolios) ->
          either fail (const (return ())) =<< rollDatabaseCopy (newestAlone 3) database 5 (CopyStamp time 1 portfolios)
        sort . filter (".sqlite" `isSuffixOf`) <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-version-5-2026-11-14T10-05-33.456Z-wallets-1-portfolios-2.sqlite",
                           "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-3.sqlite",
                           "portfolios-version-5-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.sqlite"
                         ]

    it "puts nothing into the user-backups directory" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        _ <- rollBackup (newestAlone 3) database 5 (CopyStamp copyTime1 2 3) "labels\n"
        doesDirectoryExist (dir </> "user-backups") `shouldReturn` False

-- | Write a copy of the labels of a database of 1 wallet and 1 portfolio
-- at each of the times given, in their order, each with other labels.
-- The retention given first says which copies each rotation keeps.
writeLabelCopies :: Retention -> FilePath -> [UTCTime] -> IO ()
writeLabelCopies kept database times =
  forM_ (zip times ["first\n", "second\n", "third\n", "fourth\n"]) $ \(time, labels) ->
    either fail (const (return ())) =<< rollLabelCopy kept database (CopyStamp time 1 1) (C8.pack labels)

{- The rotation -}

-- The rotation is the same for the copies of the database and for those
-- of the labels. These tests run it on the copies of the labels, which
-- need no database.
rotationSpec :: Spec
rotationSpec =
  describe "The rotation of the copies hw makes by itself" $ do
    -- Four copies are fewer than the ten newest kept
    it "removes no copy while every copy is one it keeps" $
      inScratchDirectory $ \dir -> do
        writeLabelCopies (newestAlone 10) (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    it "keeps the newest copy alone when it keeps one" $
      inScratchDirectory $ \dir -> do
        writeLabelCopies (newestAlone 1) (dir </> "portfolios.sqlite") [copyTime1, copyTime2]
        listDirectory (dir </> "auto-backups")
          `shouldReturn` ["portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl"]

    -- Three copies were made while the ten newest were kept. The fourth
    -- is made while two are: it leaves the two newest, itself and the
    -- third.
    it "removes every copy it does not keep, not the oldest alone" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        writeLabelCopies (newestAlone 10) database [copyTime1, copyTime2, copyTime3]
        _ <- rollLabelCopy (newestAlone 2) database (CopyStamp copyTime4 1 1) "fourth\n"
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- The clock was set back before the fourth copy, whose name now
    -- sorts first. It holds the labels as they are now, so it stays. The
    -- three newest by their times are the three others, which stay too.
    it "keeps the copy just written, even when its time is the oldest and no rule keeps it" $
      inScratchDirectory $ \dir -> do
        writeLabelCopies (newestAlone 3) (dir </> "portfolios.sqlite") [copyTime2, copyTime3, copyTime4, copyTime1]
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- The copy of the clock that was set back is no longer the one just
    -- written. A fifth copy, at the right time again, leaves the three
    -- newest by their times.
    it "removes a copy of an earlier time at the next copy, when no rule keeps it" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        writeLabelCopies (newestAlone 3) database [copyTime2, copyTime3, copyTime1]
        _ <- rollLabelCopy (newestAlone 3) database (CopyStamp copyTime4 1 1) "fourth\n"
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- A crash left an empty file under the name of a copy, with a time
    -- after the three copies made next. Counted as a copy, it would be
    -- the newest of the three kept, and push out the first one.
    it "does not count an empty file under the name of a copy, and removes it" $
      inScratchDirectory $ \dir -> do
        let backups = dir </> "auto-backups"
        createDirectory backups
        writeFile (backups </> "portfolios-labels-2026-11-16T00-00-00.000Z-wallets-1-portfolios-1.jsonl") ""
        writeLabelCopies (newestAlone 3) (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3]
        sort <$> listDirectory backups
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- A directory and a link, each under the name of a copy with a time
    -- after the four copies made next. Counted as copies, they would be
    -- the two newest, and leave room for one copy only.
    it "does not count a directory or a link under the name of a copy, and leaves them there" $
      inScratchDirectory $ \dir -> do
        let backups = dir </> "auto-backups"
        createDirectory backups
        createDirectory (backups </> "portfolios-labels-2026-11-16T00-00-00.000Z-wallets-1-portfolios-1.jsonl")
        writeFile (dir </> "mine.jsonl") "kept\n"
        createFileLink (dir </> "mine.jsonl") (backups </> "portfolios-labels-2026-11-17T00-00-00.000Z-wallets-1-portfolios-1.jsonl")
        writeLabelCopies (newestAlone 3) (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory backups
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-16T00-00-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-17T00-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- The fourth copy is of the next day. One copy is kept as the newest,
    -- and the last copy of each of two days: the third copy is the last
    -- one of the first day.
    it "keeps the last copy of an earlier day beside the newest" $
      inScratchDirectory $ \dir -> do
        let newestAndTwoDays = Retention {keptNewest = 1, keptHourly = 0, keptDaily = 2, keptWeekly = 0, keptMonthly = 0}
        writeLabelCopies newestAndTwoDays (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- The owner took away their right to list the directory
    it "says why it could not read the directory, and writes no copy then" $
      inScratchDirectory $ \dir -> do
        let backups = dir </> "auto-backups"
        createDirectory backups
        listable <- getPermissions backups
        setPermissions backups (setOwnerReadable False listable)
        written <- rollLabelCopy (newestAlone 3) (dir </> "portfolios.sqlite") (CopyStamp copyTime1 1 1) "first\n" `finally` setPermissions backups listable
        written `shouldBe` Left ("The directory " <> backups <> " could not be read: permission denied")
        listDirectory backups `shouldReturn` []

{- Which copies are kept -}

-- The copies are named by their times in UTC, as in "2026-11-14 10:05".
-- 14 November 2026 is a Saturday.
retentionSpec :: Spec
retentionSpec =
  describe "The copies hw keeps" $ do
    it "are the newest ones, as many as it keeps" $
      retained (newestAlone 2) clock ["2026-11-14 10:00", "2026-11-14 10:01", "2026-11-14 10:02"]
        `shouldBe` ["2026-11-14 10:01", "2026-11-14 10:02"]

    it "are the last copy of each hour" $
      retained (keeping {keptHourly = 2}) clock ["2026-11-14 09:10", "2026-11-14 09:50", "2026-11-14 10:05", "2026-11-14 10:40"]
        `shouldBe` ["2026-11-14 09:50", "2026-11-14 10:40"]

    it "are the last copy of each day" $
      retained (keeping {keptDaily = 2}) clock ["2026-11-12 09:00", "2026-11-13 08:00", "2026-11-13 23:00", "2026-11-14 07:00"]
        `shouldBe` ["2026-11-13 23:00", "2026-11-14 07:00"]

    -- Sunday 15 November ends a week, and Monday 16 starts the next
    it "are the last copy of each week, which starts on Monday" $
      retained (keeping {keptWeekly = 2}) clock ["2026-11-14 12:00", "2026-11-15 12:00", "2026-11-16 12:00", "2026-11-17 12:00"]
        `shouldBe` ["2026-11-15 12:00", "2026-11-17 12:00"]

    it "are the last copy of each month" $
      retained (keeping {keptMonthly = 2}) clock ["2026-09-30 12:00", "2026-10-01 12:00", "2026-10-31 12:00", "2026-11-02 12:00"]
        `shouldBe` ["2026-10-31 12:00", "2026-11-02 12:00"]

    -- The wallet was left alone from March to November
    it "count the days that have a copy, however far apart they are" $
      retained (keeping {keptDaily = 2}) clock ["2026-03-01 09:00", "2026-03-02 09:00", "2026-11-14 09:00"]
        `shouldBe` ["2026-03-02 09:00", "2026-11-14 09:00"]

    -- The newest copy, and the last copy of each of two days
    it "are those any of the rules keeps" $
      retained (keeping {keptNewest = 1, keptDaily = 2}) clock ["2026-11-13 10:00", "2026-11-13 11:00", "2026-11-14 09:00", "2026-11-14 09:30"]
        `shouldBe` ["2026-11-13 11:00", "2026-11-14 09:30"]

    it "are given in the order of their times, whatever the order of the copies" $
      retained (newestAlone 2) clock ["2026-11-14 10:02", "2026-11-14 10:00", "2026-11-14 10:01"]
        `shouldBe` ["2026-11-14 10:01", "2026-11-14 10:02"]

    it "are none of no copy" $
      retained autoBackupRetention clock [] `shouldBe` []

    -- A script set twelve labels from 10:00 to 10:11. The ten newest
    -- copies are those of 10:02 to 10:11. The copy of 08:30, made before
    -- the script ran, is the last one of its hour, so it stays.
    it "of hw still hold the copy made before twelve commands in a row" $
      retained
        autoBackupRetention
        clock
        [ "2026-11-14 08:30",
          "2026-11-14 10:00",
          "2026-11-14 10:01",
          "2026-11-14 10:02",
          "2026-11-14 10:03",
          "2026-11-14 10:04",
          "2026-11-14 10:05",
          "2026-11-14 10:06",
          "2026-11-14 10:07",
          "2026-11-14 10:08",
          "2026-11-14 10:09",
          "2026-11-14 10:10",
          "2026-11-14 10:11"
        ]
        `shouldBe` [ "2026-11-14 08:30",
                     "2026-11-14 10:02",
                     "2026-11-14 10:03",
                     "2026-11-14 10:04",
                     "2026-11-14 10:05",
                     "2026-11-14 10:06",
                     "2026-11-14 10:07",
                     "2026-11-14 10:08",
                     "2026-11-14 10:09",
                     "2026-11-14 10:10",
                     "2026-11-14 10:11"
                   ]

    -- One copy a day, each at noon, on the 14 days from 1 to 14 November.
    -- The ten newest are those of 5 to 14. Each copy is the last of its
    -- hour and of its day, and 14 hours are fewer than the 24 kept.
    it "of hw are all of fourteen copies made a day apart" $
      length (retained autoBackupRetention clock ["2026-11-" <> day <> " 12:00" | day <- ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14"]])
        `shouldBe` 14
  where
    -- A retention that keeps nothing, for a test to set the one rule it
    -- is about
    keeping = newestAlone 0

-- | The time a test writes as "2026-11-14 10:05", in UTC
clock :: String -> UTCTime
clock = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M"

-- | A retention that keeps the newest copies alone, as many as given. A
-- small count shows a copy go after a few copies.
newestAlone :: Int -> Retention
newestAlone count = Retention {keptNewest = count, keptHourly = 0, keptDaily = 0, keptWeekly = 0, keptMonthly = 0}

{- What the directories hold -}

backupsShownSpec :: Spec
backupsShownSpec =
  describe "What showbackups shows" $ do
    it "is no copy of any kind in a data directory without the two directories" $
      inScratchDirectory $ \dir ->
        backupsShown True True (dir </> "portfolios.sqlite")
          `shouldReturn` Right
            BackupsShown
              { backupsAutoDirectory = dir </> "auto-backups",
                backupsDatabaseOn = True,
                backupsDatabase = CopiesShown 0 Nothing,
                backupsLabelsOn = True,
                backupsLabels = CopiesShown 0 Nothing,
                backupsUserDirectory = dir </> "user-backups",
                backupsUser = CopiesShown 0 Nothing
              }

    -- Four copies of the database and three of the labels, the last ones
    -- at 08:00 on 15 November and at 10:20 on 14 November
    it "counts the copies of each kind hw made by itself, and names the newest of each with its time" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "auto-backups"
        newDatabase database
        rollFourCopies autoBackupRetention database
        writeLabelCopies autoBackupRetention database [copyTime1, copyTime2, copyTime3]
        shown <- backupsShown True True database
        (backupsDatabase <$> shown, backupsLabels <$> shown)
          `shouldBe` ( Right (CopiesShown 4 (Just (backups </> "portfolios-version-5-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.sqlite", copyTime4))),
                       Right (CopiesShown 3 (Just (backups </> "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl", copyTime3)))
                     )

    -- Each backup of the user is a copy of the database with its labels
    -- beside it. The two files count as one backup.
    it "counts the backups of the user by their copies of the database" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        _ <- userBackup database 5 (CopyStamp copyTime1 1 1) "labels\n"
        _ <- userBackup database 5 (CopyStamp copyTime3 1 2) "labels\n"
        (fmap backupsUser <$> backupsShown True True database)
          `shouldReturn` Right (CopiesShown 2 (Just (dir </> "user-backups" </> "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-2.sqlite", copyTime3)))

    it "leaves out a file of the user in the directory of the copies" $
      inScratchDirectory $ \dir -> do
        createDirectory (dir </> "auto-backups")
        writeFile (dir </> "auto-backups" </> "before-the-move.sqlite") "kept\n"
        (fmap backupsDatabase <$> backupsShown True True (dir </> "portfolios.sqlite")) `shouldReturn` Right (CopiesShown 0 Nothing)

    it "says which kinds of copy the configuration turns off" $
      inScratchDirectory $ \dir -> do
        shown <- backupsShown False True (dir </> "portfolios.sqlite")
        (backupsDatabaseOn <$> shown, backupsLabelsOn <$> shown) `shouldBe` (Right False, Right True)

-- | The times of the copies: two in the same second, one later that day,
-- and one the next day
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

