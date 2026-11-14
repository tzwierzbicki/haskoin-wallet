{-# LANGUAGE OverloadedStrings #-}

-- | The copies of the database: the copy itself, and the backup command,
-- which copies the database and its labels into user-backups and keeps
-- them all. Then the copies hw makes by itself into auto-backups, of the
-- database and of the labels: their names, where they go, how many of
-- each hw keeps, and which files of the directory the rotation takes for
-- copies. The count of copies to keep comes from the configuration. Most
-- tests here ask for three, so that a fourth copy shows the rotation.
module Haskoin.Wallet.BackupSpec (spec) where

import Control.Exception (finally)
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
import Haskoin (prepareContext)
import Haskoin.Wallet.Backup (CopyStamp (..), autoBackupDirectory, backupDatabase, backupName, databaseCopyName, databaseCopyStamp, labelCopyName, labelCopyStamp, rollDatabaseCopy, rollLabelCopy, userBackup, userBackupDirectory)
import Haskoin.Wallet.Commands (cmdBackup, cmdExportWallet)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO (newFileNextTo)
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import System.Directory (createDirectory, createFileLink, doesDirectoryExist, getPermissions, listDirectory, setOwnerReadable, setPermissions)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  describe "The copy of the database" $ do
    it "is named after the database, the version and the time" $
      backupName "/home/someone/.hw/portfolios.sqlite" 2 (ZonedTime (LocalTime (fromGregorian 2026 10 21) (TimeOfDay 14 5 33)) utc)
        `shouldBe` "portfolios-version-2-2026-10-21T14-05-33.sqlite"

    it "goes into the given directory, created if missing" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "backups"
        inDatabaseFile database $ do
          createTables migrateAll
          P.insert_ $ DBVersion 5
          P.insert_ $ DBBest "btc" bestBlockHash 800105 Nothing
        copy <- either fail return =<< backupDatabase database backups 5
        takeDirectory copy `shouldBe` backups
        takeFileName copy `shouldSatisfy` \name -> "portfolios-version-5-" `isPrefixOf` name && ".sqlite" `isSuffixOf` name
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]
        inDatabaseFile copy (tableRows "d_b_best") `shouldReturn` [[PersistText "btc", PersistText bestBlockHash, PersistInt64 800105, PersistNull]]

    -- SQLite writes the copy next to its path, and it then takes the path
    it "leaves no other file next to it" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "backups"
        inDatabaseFile database $ createTables migrateAll
        copy <- either fail return =<< backupDatabase database backups 5
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
          liftIO $ either fail return =<< backupDatabase database (dir </> "backups") 5
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]

    -- A file stands where the directory would go
    it "is not made when its directory cannot be created, and says why" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            backups = dir </> "taken" </> "backups"
        inDatabaseFile database $ createTables migrateAll
        writeFile (dir </> "taken") ""
        backupDatabase database backups 5
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
        cmdBackup ctx (configUsing database) (Just copy) `shouldReturn` ResponseFile copy
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]
        inDatabaseFile copy (tableRows "d_b_best")
          `shouldReturn` [[PersistText "btc", PersistText bestBlockHash, PersistInt64 800105, PersistNull]]

    it "refuses to overwrite a file" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "copy.sqlite"
        inDatabaseFile database $ createTables migrateAll
        _ <- cmdBackup ctx (configUsing database) (Just copy)
        cmdBackup ctx (configUsing database) (Just copy)
          `shouldReturn` ResponseError (cs $ "Path '" <> copy <> "' already exists")

    it "says why it could not copy the database into a directory that does not exist" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "missing" </> "copy.sqlite"
        inDatabaseFile database $ createTables migrateAll
        cmdBackup ctx (configUsing database) (Just copy)
          `shouldReturn` ResponseError (cs $ "The database could not be copied to " <> copy <> ": no such file or directory")

    it "says why it could not copy a file that is not a database, and leaves no file behind" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "copy.sqlite"
        writeFile database "not a database\n"
        cmdBackup ctx (configUsing database) (Just copy)
          `shouldReturn` ResponseError (cs $ "The database could not be copied to " <> copy <> ": it is not an SQLite database")
        listDirectory dir `shouldReturn` ["portfolios.sqlite"]

    it "copies the database to the given file alone, and writes nothing into user-backups" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        _ <- cmdBackup ctx (configUsing database) (Just (dir </> "copy.sqlite"))
        doesDirectoryExist (dir </> "user-backups") `shouldReturn` False

    it "without a file, puts the copy of the database and its labels into user-backups next to the database" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInFile (dir </> "portfolios.sqlite") (testPortfolio ctx)
        written <- filesOf <$> cmdBackup ctx wallet Nothing
        map takeDirectory <$> written `shouldBe` Right [dir </> "user-backups", dir </> "user-backups"]

    -- The time in the middle of the name changes from run to run
    it "without a file, names the copy after the version and the counts of wallets and portfolios, and its labels the same" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInFile (dir </> "portfolios.sqlite") (testPortfolio ctx)
        written <- filesOf <$> cmdBackup ctx wallet Nothing
        let nameParts name = ("portfolios-version-5-" `isPrefixOf` name, "-wallets-1-portfolios-1" `isSuffixOf` dropExtension name)
        map (nameParts . takeFileName) <$> written `shouldBe` Right [(True, True), (True, True)]

    it "without a file, writes the labels exportwallet writes" $
      inScratchDirectory $ \dir -> do
        wallet <- walletInFile (dir </> "portfolios.sqlite") (testPortfolio ctx)
        _ <- cmdExportWallet ctx wallet Nothing Nothing (dir </> "wallet.jsonl")
        exported <- readFile (dir </> "wallet.jsonl")
        written <- filesOf <$> cmdBackup ctx wallet Nothing
        case written of
          Right [_, labels] -> readFile labels `shouldReturn` exported
          other -> expectationFailure ("Not a backup: " <> show other)

  describe "The backups of the user" $ do
    it "are named after the database, the version, the time in UTC to the millisecond, and the counts of wallets and portfolios" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            stem = dir </> "user-backups" </> "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3"
        newDatabase database
        userBackup database 5 (CopyStamp copyTime1 2 3) "labels\n"
          `shouldReturn` Right (stem <> ".sqlite", stem <> ".jsonl")

    it "go into the user-backups directory next to the database" $
      userBackupDirectory "/home/someone/.hw/portfolios.sqlite" `shouldBe` "/home/someone/.hw/user-backups"

    it "are all kept: a fourth leaves the three before it" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        forM_ (zip [copyTime1, copyTime2, copyTime3, copyTime4] [1 ..]) $ \(time, portfolios) ->
          either fail (const (return ())) =<< userBackup database 5 (CopyStamp time 1 portfolios) "labels\n"
        sort <$> listDirectory (dir </> "user-backups")
          `shouldReturn` [ "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.sqlite",
                           "portfolios-version-5-2026-11-14T10-05-33.456Z-wallets-1-portfolios-2.jsonl",
                           "portfolios-version-5-2026-11-14T10-05-33.456Z-wallets-1-portfolios-2.sqlite",
                           "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-3.jsonl",
                           "portfolios-version-5-2026-11-14T10-20-00.000Z-wallets-1-portfolios-3.sqlite",
                           "portfolios-version-5-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.jsonl",
                           "portfolios-version-5-2026-11-15T08-00-00.000Z-wallets-1-portfolios-4.sqlite"
                         ]

  copyNamesSpec
  databaseCopiesSpec
  labelCopiesSpec
  rotationSpec

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
        rollDatabaseCopy 3 database 5 (CopyStamp copyTime1 1 1) `shouldReturn` Right copy
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 5, "integer")]

    -- The portfolios a user makes one after the other
    it "are three at most when three are kept: the fourth removes the oldest" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        newDatabase database
        rollFourCopies 3 database
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
        rollFourCopies 3 database
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
        writeLabelCopies 3 database [copyTime1, copyTime2, copyTime3, copyTime4]
        rollFourCopies 3 database
        length <$> listDirectory (dir </> "auto-backups") `shouldReturn` 6

    -- A file stands where the directory would go
    it "say why a copy could not be made" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
            copy = dir </> "auto-backups" </> "portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.sqlite"
        newDatabase database
        writeFile (dir </> "auto-backups") ""
        rollDatabaseCopy 3 database 5 (CopyStamp copyTime1 1 1)
          `shouldReturn` Left ("The database could not be copied to " <> copy <> ": not a directory")

-- | Copy a database four times into its auto-backups directory, at the
-- four times of the copies, as a user who makes a portfolio each time:
-- with 1, 2, 3 and 4 portfolios. The count given first says how many
-- copies each rotation keeps.
rollFourCopies :: Natural -> FilePath -> IO ()
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
        rollLabelCopy 3 (dir </> "portfolios.sqlite") (CopyStamp copyTime1 1 1) "first\n" `shouldReturn` Right (Just copy)
        readFile copy `shouldReturn` "first\n"

    it "are not written again when the newest copy holds the same labels" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        _ <- rollLabelCopy 3 database (CopyStamp copyTime1 1 1) "first\n"
        rollLabelCopy 3 database (CopyStamp copyTime2 1 1) "first\n" `shouldReturn` Right Nothing
        listDirectory (dir </> "auto-backups") `shouldReturn` ["portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl"]

    -- The user labelled an address, then took the label back
    it "are written again when only an older copy holds the same labels" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        _ <- rollLabelCopy 3 database (CopyStamp copyTime1 1 1) "first\n"
        _ <- rollLabelCopy 3 database (CopyStamp copyTime2 1 1) "second\n"
        rollLabelCopy 3 database (CopyStamp copyTime3 1 1) "first\n"
          `shouldReturn` Right (Just (dir </> "auto-backups" </> "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl"))

    it "are three at most when three are kept: the fourth removes the oldest" $
      inScratchDirectory $ \dir -> do
        writeLabelCopies 3 (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
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
        writeLabelCopies 3 (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
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
        _ <- rollLabelCopy 3 (dir </> "portfolios.sqlite") (CopyStamp copyTime1 1 1) "first\n"
        -- The permission bits of rwx------
        (.&. 0o777) . fileMode <$> getFileStatus (dir </> "auto-backups") `shouldReturn` 0o700

    -- A file stands where the directory would go
    it "say why a copy could not be written" $
      inScratchDirectory $ \dir -> do
        let copy = dir </> "auto-backups" </> "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl"
        writeFile (dir </> "auto-backups") ""
        rollLabelCopy 3 (dir </> "portfolios.sqlite") (CopyStamp copyTime1 1 1) "first\n"
          `shouldReturn` Left ("The file " <> copy <> " could not be written: not a directory")

-- | Write a copy of the labels of a database of 1 wallet and 1 portfolio
-- at each of the times given, in their order, each with other labels.
-- The count given first says how many copies each rotation keeps.
writeLabelCopies :: Natural -> FilePath -> [UTCTime] -> IO ()
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
    -- Four copies are fewer than the ten asked for
    it "removes no copy while there are fewer than the count asked for" $
      inScratchDirectory $ \dir -> do
        writeLabelCopies 10 (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    it "keeps the newest copy alone when asked for one" $
      inScratchDirectory $ \dir -> do
        writeLabelCopies 1 (dir </> "portfolios.sqlite") [copyTime1, copyTime2]
        listDirectory (dir </> "auto-backups")
          `shouldReturn` ["portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl"]

    -- The user lowered the count from ten to two after three copies. The
    -- fourth copy leaves the two newest: itself and the third.
    it "removes every copy past a count that was lowered, not the oldest alone" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "portfolios.sqlite"
        writeLabelCopies 10 database [copyTime1, copyTime2, copyTime3]
        _ <- rollLabelCopy 2 database (CopyStamp copyTime4 1 1) "fourth\n"
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- The clock was set back before the fourth copy, whose name now
    -- sorts first. It holds the labels as they are now.
    it "keeps the copy just written, even when its time is the oldest, and removes the oldest of the others" $
      inScratchDirectory $ \dir -> do
        writeLabelCopies 3 (dir </> "portfolios.sqlite") [copyTime2, copyTime3, copyTime4, copyTime1]
        sort <$> listDirectory (dir </> "auto-backups")
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-1.jsonl",
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
        writeLabelCopies 3 (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3]
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
        writeLabelCopies 3 (dir </> "portfolios.sqlite") [copyTime1, copyTime2, copyTime3, copyTime4]
        sort <$> listDirectory backups
          `shouldReturn` [ "portfolios-labels-2026-11-14T10-05-33.456Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-14T10-20-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-15T08-00-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-16T00-00-00.000Z-wallets-1-portfolios-1.jsonl",
                           "portfolios-labels-2026-11-17T00-00-00.000Z-wallets-1-portfolios-1.jsonl"
                         ]

    -- The owner took away their right to list the directory
    it "says why it could not read the directory, and writes no copy then" $
      inScratchDirectory $ \dir -> do
        let backups = dir </> "auto-backups"
        createDirectory backups
        listable <- getPermissions backups
        setPermissions backups (setOwnerReadable False listable)
        written <- rollLabelCopy 3 (dir </> "portfolios.sqlite") (CopyStamp copyTime1 1 1) "first\n" `finally` setPermissions backups listable
        written `shouldBe` Left ("The directory " <> backups <> " could not be read: permission denied")
        listDirectory backups `shouldReturn` []

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

-- | The configuration of an hw whose database is the given file
configUsing :: FilePath -> Config
configUsing database = def {configDatabaseFile = Just database}
