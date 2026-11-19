-- | What hw does when SQLite fails on its database: the sentence a
-- command answers with in place of an exception, and the damaged
-- database that restoredatabase moves aside.
--
-- A database is damaged when its file is no database, or is one cut
-- short: a copy that stopped halfway, a full disk, a file put there by
-- mistake. That is the moment the backups exist for, so the answer names
-- the newest backup of the database and the command that restores it.
-- hw tells the damage from the failure of SQLite alone, and checks no
-- row ("Haskoin.Wallet.Database".'DatabaseFailure').
--
-- A command that meets a damaged database changes nothing: it says so
-- again each time, until the user restores a backup. Moving the file
-- aside as soon as hw sees the damage was turned down. hw would then
-- start a new, empty database, and the next command would show a wallet
-- without a portfolio, with no word of why. A file that reads as damaged
-- for a moment, a copy still being written for instance, would be moved
-- too.
--
-- restoredatabase moves the damaged database itself
-- ('setDamagedAside'), into the directory broken-databases next to the
-- database, with the files SQLite keeps beside a database. A move by
-- hand would leave those behind, and SQLite would apply them to the new
-- database. Nothing is deleted: the user may still look into the file.
--
-- When no backup of the database helps, the wallet is created again in
-- a new database. The answer then gives two ways, with their commands:
-- importwallet with the newest copy of the labels, which needs no
-- mnemonic and brings the labels and the tags back, and recover, from
-- the mnemonic.
module Haskoin.Wallet.DatabaseFailure
  ( catchDatabaseFailure,
    databaseFailureExplained,
    databaseFailureText,
    newestBackup,
    newestLabels,
    databaseDamage,
    brokenDatabaseDirectory,
    brokenDatabaseName,
    setDamagedAside,
  )
where

import Control.Monad (forM, forM_, join, when)
import Data.Bifunctor (first)
import Data.List (intercalate, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Time (UTCTime, getCurrentTime)
import Haskoin.Wallet.Backup (autoBackupDirectory, databaseCopyTime, labelCopyTime, rollingFiles, stampText, userBackupDirectory)
import Haskoin.Wallet.Config (Config, databaseFile)
import Haskoin.Wallet.Database (DatabaseFailure (..), databaseInUse, tryDatabase)
import Haskoin.Wallet.FileIO (privateDirectory, systemReason)
import Haskoin.Wallet.Migration (checkDatabaseFile, moveAsideCommand, recreateSteps, shellWord)
import System.Directory (doesFileExist, doesPathExist, renameFile)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))

{- What a command answers -}

-- | Run work on the database of hw, answering with a sentence instead of
-- failing when SQLite fails on it ('databaseFailureExplained')
catchDatabaseFailure :: Config -> IO a -> IO (Either String a)
catchDatabaseFailure cfg action =
  either (fmap Left . databaseFailureExplained cfg) (return . Right) =<< tryDatabase action

-- | What hw says of a failure of SQLite on its database. The
-- directories of the backups are read when the answer names a copy: the
-- newest one of the database for a damaged database, and the newest one
-- of the labels where the wallet is created again.
databaseFailureExplained :: Config -> DatabaseFailure -> IO String
databaseFailureExplained cfg failure = do
  let database = databaseFile cfg
      named = case failure of
        DatabaseDamaged _ -> True
        DatabaseOfOtherBuild _ -> True
        _ -> False
  newest <- if named then newestBackup database else return Nothing
  labels <- if named then newestLabels database else return Nothing
  return (databaseFailureText database newest labels failure)

-- | What hw says of a failure of SQLite on the database file given,
-- whose newest backup and whose newest copy of the labels are the next
-- two arguments, if it has them. Each kind of failure gets its way out:
--
-- * A damaged database: the command that restores the newest backup,
--   written out, since the user may never have run it. Without a backup,
--   the ways to create the wallet again, from a copy of the labels or
--   from the mnemonic.
--
-- * A database of another build: no backup of the database helps, since
--   that build made the backups too. The wallet is created again.
--
-- * Any other failure: the words of SQLite, which name the cause, such
--   as "database or disk is full".
--
-- A paragraph is one line here, and is wrapped where it is printed
-- ("Haskoin.Wallet.Cli.Wrap"). A command to copy is a line of its own,
-- indented, which stays whole.
databaseFailureText :: FilePath -> Maybe FilePath -> Maybe FilePath -> DatabaseFailure -> String
databaseFailureText database newest labels failure = case failure of
  DatabaseInUse -> databaseInUse
  DatabaseDamaged said -> intercalate "\n" (damaged said)
  DatabaseOfOtherBuild said ->
    intercalate "\n" . recreating database labels $
      "The database " <> database <> " does not have the tables of this hw (" <> said <> "), so another build of hw made it. Use the hw that made it, or move the database aside and create the wallet again"
  DatabaseFailed said -> "SQLite could not use the database " <> database <> " (" <> said <> ")"
  where
    broken = brokenDatabaseDirectory database
    isDamaged said = "The database " <> database <> " is damaged: SQLite cannot read it (" <> said <> ")."
    damaged said = case newest of
      Just backup ->
        [ isDamaged said <> " To put the newest backup in its place, run:",
          "",
          "  hw restoredatabase " <> shellWord backup,
          "",
          "restoredatabase first moves the damaged database into the directory " <> broken <> ". hw showbackups shows the other backups."
        ]
      Nothing ->
        [ isDamaged said <> " hw has no backup of it in the directories " <> autoBackupDirectory database <> " and " <> userBackupDirectory database <> ".",
          "",
          "With a backup kept elsewhere, run hw restoredatabase FILE, which first moves the damaged database into the directory " <> broken <> ".",
          ""
        ]
          <> recreating database labels "Without one, move the damaged database aside and create the wallet again"

-- | The ways to create a wallet again when hw cannot read its database
-- and no backup of the database helps, after the start of the sentence
-- that says to move the database aside and to create the wallet again.
-- The lines end that sentence and give the commands.
--
-- With a copy of the labels, there are two ways. importwallet with that
-- copy brings back the portfolios, their labels and their tags, which
-- the mnemonic does not. It runs twice, around a sync: the labels of
-- what the wallet does not have yet wait for the sync to find it. The
-- other way is recover, from the mnemonic. Without a copy of the
-- labels, the mnemonic is the one way, with the steps of a database of
-- an earlier version ('recreateSteps').
recreating :: FilePath -> Maybe FilePath -> String -> [String]
recreating database labelsM start = case labelsM of
  Nothing -> (start <> " from its mnemonic:") : recreateSteps database
  Just labels ->
    [ start <> ":",
      "",
      moveAsideCommand database,
      "",
      "Then, from the newest copy of its labels, which brings back the portfolios, their labels and their tags:",
      "",
      "  hw importwallet " <> shellWord labels,
      "  hw sync",
      "  hw importwallet " <> shellWord labels,
      "",
      "The second importwallet sets the labels of what the sync found. Or, from the mnemonic, which brings back the portfolios that were paid, without their labels:",
      "",
      "  hw recover"
    ]

-- | The newest backup of a database file: the newest copy hw made by
-- itself or the newest one backup wrote, whichever is the later
newestBackup :: FilePath -> IO (Maybe FilePath)
newestBackup = newestCopy databaseCopyTime

-- | The newest copy of the labels of a database file, in the same two
-- directories
newestLabels :: FilePath -> IO (Maybe FilePath)
newestLabels = newestCopy labelCopyTime

-- | The newest copy of one kind of a database file, in the directory of
-- the copies hw makes by itself and in that of the backup command. The
-- function given reads the time of a copy of that kind from the name of
-- a file. A directory that cannot be read holds no copy here: the
-- failure to report is that of the database.
newestCopy :: (FilePath -> FilePath -> Maybe UTCTime) -> FilePath -> IO (Maybe FilePath)
newestCopy timeOf database = do
  found <- forM [autoBackupDirectory database, userBackupDirectory database] $ \directory ->
    rollingFiles directory (timeOf database)
  return . fmap snd . listToMaybe . sortOn (Down . fst) $ concat [copies | Right (copies, _) <- found]

{- The damaged database set aside -}

-- | The words of SQLite on the database file when it is damaged, and
-- nothing when SQLite reads it, or when it is not there. The file is
-- read as hw reads the format version of a database before it opens it,
-- and is not changed.
databaseDamage :: FilePath -> IO (Maybe String)
databaseDamage database = do
  probed <- tryDatabase (checkDatabaseFile database)
  return $ case probed of
    Left (DatabaseDamaged said) -> Just said
    _ -> Nothing

-- | Where restoredatabase moves a damaged database: the directory
-- broken-databases next to the database. For the database of hw, it is
-- ~\/.hw\/broken-databases. hw never removes a file there. It is apart
-- from the directories of the backups, whose files restoredatabase
-- reads.
brokenDatabaseDirectory :: FilePath -> FilePath
brokenDatabaseDirectory database = takeDirectory database </> "broken-databases"

-- | The name a damaged database takes in that directory, from the time
-- it is moved: portfolios-2026-11-14T10-05-33.123Z.sqlite. The time
-- keeps two damaged databases apart.
brokenDatabaseName :: FilePath -> UTCTime -> FilePath
brokenDatabaseName database time = takeBaseName database <> "-" <> stampText time <> takeExtension database

-- | Move the damaged database file given out of the way, into
-- 'brokenDatabaseDirectory', and answer with where it is now, or with
-- why it could not be moved.
--
-- The files SQLite keeps beside a database, its write-ahead log and the
-- index of that log, go with it under the same name, and go first. A
-- crash in between then leaves the damaged database in place without
-- them, and never a log without its database, which SQLite would apply
-- to the next database made there.
setDamagedAside :: FilePath -> IO (Either String FilePath)
setDamagedAside database = do
  time <- getCurrentTime
  let directory = brokenDatabaseDirectory database
      moved = directory </> brokenDatabaseName database time
  outcome <- systemReason $ do
    privateDirectory directory
    taken <- doesPathExist moved
    if taken
      then return (Left "a file has that name already")
      else do
        forM_ ["-wal", "-shm", ""] $ \suffix -> do
          exists <- doesFileExist (database <> suffix)
          when exists $ renameFile (database <> suffix) (moved <> suffix)
        return (Right moved)
  return $ first (couldNotMove moved) (join outcome)
  where
    couldNotMove moved reason = "The damaged database " <> database <> " could not be moved to " <> moved <> ": " <> reason
