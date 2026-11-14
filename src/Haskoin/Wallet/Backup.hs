{-# LANGUAGE OverloadedStrings #-}

-- | The copies hw makes of the database and of its labels. There are
-- three kinds, each in a directory of its own.
--
-- * The migration copies the database before it changes its format, in
--   case a step goes wrong ('migrationBackupDirectory').
--
-- * hw copies the database and its labels by itself into the auto-backups
--   directory, after the commands that change them, and keeps the newest
--   copies of each, ten unless the configuration says otherwise
--   ('autoBackupDirectory'). A copy of the database follows each command
--   that adds or deletes a wallet or a portfolio ('rollDatabaseCopy'). A
--   copy of the labels follows each command that changes what
--   exportwallet writes ('rollLabelCopy').
--
-- * The backup command copies the database and its labels into the
--   user-backups directory, and hw never removes them
--   ('userBackupDirectory', 'userBackup').
--
-- A copy of the labels is the file exportwallet writes without options:
-- every portfolio of every wallet, with its labels, its descriptions and
-- the coins the user locked ("Haskoin.Wallet.Labels"). importwallet reads
-- it back into a new database. It holds what nothing else can rebuild:
-- what the user typed. The mnemonic rebuilds the keys, and a sync the
-- history. A copy of the database keeps everything, the pending
-- transactions too, but only another hw of the same format reads it.
--
-- Every copy is made after the change, so that the newest one holds the
-- database as it is now. When the user makes a third portfolio, the copy
-- holds three portfolios.
--
-- The auto-backups and user-backups directories are next to the database:
-- ~/.hw/auto-backups and ~/.hw/user-backups for the database of hw. A
-- database the configuration puts elsewhere, on an encrypted disk for
-- instance, keeps its copies next to it, since they show every address it
-- holds. The tests of hw keep their databases in scratch directories, so
-- their copies stay there too, away from the wallet of the user.
--
-- = The names of the copies
--
-- A name starts with the name of the database, portfolios for
-- portfolios.sqlite, so that two databases of one directory keep their
-- copies apart. It then carries the time of the copy, in UTC to the
-- millisecond, and how many wallets and portfolios the database held
-- ('labelCopyName'). The name of a copy of the database also carries its
-- format version ('databaseCopyName'). So the user can tell the copies apart without opening
-- them. The name counts the wallets and the portfolios, and show lists
-- them, from the copy itself, for the user who is about to restore one
-- ("Haskoin.Wallet.DatabaseFile").
--
-- The time is in UTC so that the names sort in the order the copies were
-- made, whatever the time zone, even in the hour the clock goes back in
-- autumn. The milliseconds let two commands in one second write two
-- copies. Local time, as the migration copies carry it, repeats an hour
-- in autumn, and its names would then sort out of order.
--
-- = Which files the rotation touches
--
-- hw keeps the newest copies of each kind in the auto-backups directory,
-- as many as the configuration asks for, and removes the older ones
-- ('Haskoin.Wallet.Config.configAutoBackupCopies'). The user may keep
-- files of their own in that directory, so the rotation must never take
-- one of them for a copy. hw keeps no list of the copies it made: such a list
-- could be lost or go stale, and the directory itself tells. A file is a
-- copy only when all of this holds ('rollingFiles'):
--
-- * Its whole name is one hw writes for this database: the name of the
--   database, the kind of copy, a time written as hw writes it, the two
--   counts and the extension, with nothing before, between or after
--   ('databaseCopyStamp', 'labelCopyStamp'). The name hw would write for
--   what the name says must be the name itself, so "05" for 5 is not one.
--
-- * It is a regular file. A directory or a link under such a name is not
--   hw's doing.
--
-- * It holds something. An empty file under such a name is what a write
--   that was cut short left: hw takes the name with an empty file before
--   it fills the copy ("Haskoin.Wallet.FileIO"). It counts for nothing,
--   and the next rotation removes it.
--
-- Every other file is left alone: it is neither counted among the copies
-- kept nor removed. That covers a file of the user under any other name, a
-- copy of another database, a copy an earlier hw named otherwise, and
-- the new file a write fills before it takes its name.
--
-- The new copy is written first, and whole, before an older one goes
-- ('rotate'). So a failure never leaves fewer copies. The copy just
-- written is always kept, even when its time is the oldest, as after the
-- clock was set back.
--
-- The count is read at each copy, and no copy remembers the count it was
-- made under. So a count the user lowers removes every copy past it at
-- the next copy of that kind, and a count the user raises removes none
-- until that many copies are there.
module Haskoin.Wallet.Backup
  ( -- * The copies before a migration
    migrationBackupDirectory,
    backupDatabase,
    backupName,
    copyDatabase,

    -- * The names of the copies
    CopyStamp (..),
    stampText,
    databaseCopyName,
    databaseCopyStamp,
    labelCopyName,
    labelCopyStamp,

    -- * The copies hw makes by itself
    autoBackupDirectory,
    rollDatabaseCopy,
    rollLabelCopy,
    rollingFiles,
    rotate,

    -- * The copies of the backup command
    userBackupDirectory,
    userBackup,
  )
where

import Control.Exception (bracket, try)
import Control.Monad (forM, forM_, guard, void)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.List (genericLength, genericTake, sortOn, stripPrefix)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, formatTime, getZonedTime, parseTimeM)
import Database.Persist.Sql (PersistValue (PersistText))
import qualified Database.Sqlite as Sqlite
import Haskoin.Wallet.Config (hwDataDirectory)
import Haskoin.Wallet.FileIO (Placement (..), placeFile, privateDirectory, readFileBytes, systemReason, writeFileBytes)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile)
import System.FilePath (splitExtension, takeBaseName, takeDirectory, (</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import Numeric.Natural (Natural)
import System.Posix.Files (fileSize, getSymbolicLinkStatus, isRegularFile)
import Text.Read (readMaybe)

{- The copies before a migration -}

-- | Where the migration puts its copies: the migration-backups directory of
-- hw
migrationBackupDirectory :: IO FilePath
migrationBackupDirectory = (</> "migration-backups") <$> hwDataDirectory

-- | Copy the database, at the given format version, into a directory,
-- created if missing, and return the copy, named after the database, the
-- version and the time, or say why it could not be made
backupDatabase :: FilePath -> FilePath -> Int -> IO (Either String FilePath)
backupDatabase database directory version = do
  time <- getZonedTime
  let copy = directory </> backupName database version time
  created <- systemReason (createDirectoryIfMissing True directory)
  case created of
    Left reason -> return $ Left ("The directory " <> directory <> " could not be created: " <> reason)
    Right () -> do
      copied <- copyDatabase database copy
      return (copy <$ copied)

-- | The name of a copy before a migration:
-- portfolios-version-5-2026-10-21T14-05-33.sqlite for portfolios.sqlite at
-- version 5, copied on 2026-10-21 at 14:05:33. The migration reads the
-- database of an older format, so it does not count its wallets.
backupName :: FilePath -> Int -> ZonedTime -> FilePath
backupName database version time =
  takeBaseName database
    <> "-version-"
    <> show version
    <> "-"
    <> formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" time
    <> ".sqlite"

-- | Copy the database to a file that does not exist yet. SQLite writes the
-- copy itself, so that it is complete and consistent whatever the state of
-- the file: hw keeps a write-ahead log, whose pages a plain copy of the file
-- would miss, and a connection of hw may be open while the copy is made.
-- SQLite writes it into a new file next to the copy, which takes the
-- copy's name once it is whole and on the disk ('placeFile'), since
-- SQLite does not put it there itself. A copy that fails is a message,
-- closes what it opened, and leaves no file behind.
copyDatabase :: FilePath -> FilePath -> IO (Either String ())
copyDatabase database copy = do
  placed <- systemReason (try (placeFile AtFreePath copy vacuumInto) :: IO (Either Sqlite.SqliteException ()))
  return $ first couldNotCopy (placed >>= first sqliteReason)
  where
    vacuumInto :: FilePath -> IO ()
    vacuumInto new =
      bracket (Sqlite.open (cs database)) Sqlite.close $ \connection ->
        bracket (Sqlite.prepare connection "vacuum into ?") Sqlite.finalize $ \statement -> do
          Sqlite.bind statement [PersistText (cs new)]
          void $ Sqlite.step statement
    couldNotCopy why = "The database could not be copied to " <> copy <> ": " <> why
    sqliteReason e = case Sqlite.seError e of
      Sqlite.ErrorFull -> "the disk is full"
      -- The name persistent-sqlite gives SQLite's "file is not a database"
      Sqlite.ErrorNotAConnection -> "it is not an SQLite database"
      other -> show other

{- The names of the copies -}

-- | What the name of a copy says about the database it was made of
data CopyStamp = CopyStamp
  { -- | When the copy was made
    stampTime :: !UTCTime,
    -- | How many wallets the database held
    stampWallets :: !Int,
    -- | How many portfolios it held, those of every wallet together
    stampPortfolios :: !Int
  }
  deriving (Eq, Show)

-- | The time of a copy as its name writes it: 2026-11-14T10-05-33.123Z
-- for 10:05:33.123 UTC on 2026-11-14. It has the same width at every
-- time, the milliseconds included when there are none, so that the names
-- sort in the order of their times.
stampText :: UTCTime -> String
stampText time =
  formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" time
    <> "."
    -- %q gives the picoseconds in twelve digits: the first three are the
    -- milliseconds
    <> take 3 (formatTime defaultTimeLocale "%q" time)
    <> "Z"

-- | The time a text writes as 'stampText' does, and nothing else: a time
-- written in another way is not that of a copy of hw
readStampTime :: String -> Maybe UTCTime
readStampTime text = do
  time <- parseTimeM False defaultTimeLocale "%Y-%m-%dT%H-%M-%S%QZ" text
  guard (stampText time == text)
  return time

-- | How many characters 'stampText' writes
stampWidth :: Int
stampWidth = length ("2026-11-14T10-05-33.123Z" :: String)

-- | A stamp as the end of a name writes it, before the extension:
-- 2026-11-14T10-05-33.123Z-wallets-2-portfolios-3 for a copy made at
-- 10:05:33.123 UTC on 2026-11-14 of a database of 2 wallets and 3
-- portfolios
stampSuffix :: CopyStamp -> String
stampSuffix stamp =
  stampText (stampTime stamp)
    <> "-wallets-"
    <> show (stampWallets stamp)
    <> "-portfolios-"
    <> show (stampPortfolios stamp)

-- | The stamp a text writes as 'stampSuffix' does, with nothing after it
readStampSuffix :: String -> Maybe CopyStamp
readStampSuffix text = do
  let (timeText, afterTime) = splitAt stampWidth text
  time <- readStampTime timeText
  (wallets, afterWallets) <- leadingNumber =<< stripPrefix "-wallets-" afterTime
  (portfolios, rest) <- leadingNumber =<< stripPrefix "-portfolios-" afterWallets
  guard (null rest)
  return (CopyStamp time wallets portfolios)

-- | The number a text starts with, and the rest of the text
leadingNumber :: String -> Maybe (Int, String)
leadingNumber text = case span isDigit text of
  ([], _) -> Nothing
  (digits, rest) -> (\number -> (number, rest)) <$> readMaybe digits

-- | The name of a copy of the database, without its extension:
-- portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3
-- for portfolios.sqlite at version 5. The backup command gives this name
-- to its copy of the labels too, so that the two files of one backup go
-- together.
databaseCopyStem :: FilePath -> Int -> CopyStamp -> FilePath
databaseCopyStem database version stamp =
  takeBaseName database <> "-version-" <> show version <> "-" <> stampSuffix stamp

-- | The name of a copy of the database at the format version given:
-- portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite
-- for portfolios.sqlite at version 5, copied at 10:05:33.123 UTC on
-- 2026-11-14 with 2 wallets and 3 portfolios
databaseCopyName :: FilePath -> Int -> CopyStamp -> FilePath
databaseCopyName database version stamp = databaseCopyStem database version stamp <> ".sqlite"

-- | The format version and the stamp the name of a file gives, when it
-- is the name 'databaseCopyName' gives a copy of the database. Any other
-- name is not that of such a copy.
databaseCopyStamp :: FilePath -> FilePath -> Maybe (Int, CopyStamp)
databaseCopyStamp database name = do
  let (stem, extension) = splitExtension name
  guard (extension == ".sqlite")
  (version, afterVersion) <- leadingNumber =<< stripPrefix (takeBaseName database <> "-version-") stem
  stamp <- readStampSuffix =<< stripPrefix "-" afterVersion
  -- The name hw would write, and not "05" for 5, say
  guard (databaseCopyName database version stamp == name)
  return (version, stamp)

-- | The name of a copy of the labels:
-- portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl
-- for portfolios.sqlite, copied at 10:05:33.123 UTC on 2026-11-14 with 2
-- wallets and 3 portfolios
labelCopyName :: FilePath -> CopyStamp -> FilePath
labelCopyName database stamp = takeBaseName database <> "-labels-" <> stampSuffix stamp <> ".jsonl"

-- | The stamp the name of a file gives, when it is the name
-- 'labelCopyName' gives a copy of the labels of the database. Any other
-- name is not that of such a copy.
labelCopyStamp :: FilePath -> FilePath -> Maybe CopyStamp
labelCopyStamp database name = do
  let (stem, extension) = splitExtension name
  guard (extension == ".jsonl")
  stamp <- readStampSuffix =<< stripPrefix (takeBaseName database <> "-labels-") stem
  guard (labelCopyName database stamp == name)
  return stamp

{- The copies hw makes by itself -}

-- | Where hw puts the copies it makes by itself of a database and of its
-- labels: the auto-backups directory next to the database. For the
-- database of hw, it is ~/.hw/auto-backups. Its name says that hw looks
-- after it: hw removes the older copies there.
autoBackupDirectory :: FilePath -> FilePath
autoBackupDirectory database = takeDirectory database </> "auto-backups"

-- | Copy the database, at the format version given and as the stamp
-- describes it, into the auto-backups directory, and keep the newest of
-- its copies there, as many as the count given first. The caller reads
-- the counts of the stamp in the transaction it holds while the copy is
-- made, so that no other command changes them in between. The answer is
-- the copy, or why it could not be made, or why an older copy could not
-- be removed.
rollDatabaseCopy :: Natural -> FilePath -> Int -> CopyStamp -> IO (Either String FilePath)
rollDatabaseCopy kept database version stamp = runExceptT $ do
  let directory = autoBackupDirectory database
      copy = directory </> databaseCopyName database version stamp
  makePrivateDirectory directory
  liftEither =<< liftIO (copyDatabase database copy)
  rotate kept directory (fmap (stampTime . snd) . databaseCopyStamp database) copy
  return copy

-- | Write the labels of a database, as exportwallet writes them, to a new
-- copy the stamp describes, and keep the newest copies, as many as the
-- count given first. The answer is the copy written, or Nothing when the
-- newest copy holds the same labels already, which writes nothing: that
-- copy would push out an older one that differs, for nothing. A copy
-- that cannot be made is a message, and so is an older copy that cannot
-- be removed.
rollLabelCopy :: Natural -> FilePath -> CopyStamp -> ByteString -> IO (Either String (Maybe FilePath))
rollLabelCopy kept database stamp labels = runExceptT $ do
  let directory = autoBackupDirectory database
      copy = directory </> labelCopyName database stamp
      timeOf = fmap stampTime . labelCopyStamp database
  makePrivateDirectory directory
  (before, _) <- ExceptT (rollingFiles directory timeOf)
  newest <- liftIO $ mapM readFileBytes (listToMaybe (reverse before))
  if newest == Just (Right labels)
    then return Nothing
    else do
      liftEither =<< liftIO (writeFileBytes copy labels)
      rotate kept directory timeOf copy
      return (Just copy)

-- | What a directory holds of one kind of copy: the copies, the oldest
-- first, then the empty files under the name of a copy, which a write
-- that was cut short left. Or why the directory could not be read.
--
-- The function given reads the time of a copy from the name of a file,
-- and gives nothing for any other name. A file is a copy when its name
-- reads so, when it is a regular file, and when it holds something. The
-- header of this module says why each of the three is asked. A directory
-- that is not there holds nothing.
rollingFiles :: FilePath -> (FilePath -> Maybe UTCTime) -> IO (Either String ([FilePath], [FilePath]))
rollingFiles directory timeOf = fmap (first couldNotRead) . systemReason $ do
  exists <- doesDirectoryExist directory
  names <- if exists then listDirectory directory else return []
  -- Each regular file with the name of a copy, with its time and whether
  -- it is empty
  named <- forM [(time, name) | name <- names, Just time <- [timeOf name]] $ \(time, name) -> do
    let file = directory </> name
    -- The status of the file itself, not of what a link points at
    status <- getSymbolicLinkStatus file
    return [(time, file, fileSize status == 0) | isRegularFile status]
  let byTime = sortOn (\(time, file, _) -> (time, file)) (concat named)
      copies = [file | (_, file, empty) <- byTime, not empty]
      leftovers = [file | (_, file, empty) <- byTime, empty]
  return (copies, leftovers)
  where
    couldNotRead reason = "The directory " <> directory <> " could not be read: " <> reason

-- | Once a copy is written, remove the copies of its kind the rotation no
-- longer keeps. The count given first says how many copies stay: the
-- copy just written, and the newest of the others up to that count. With
-- a count of three, the copy written and the two newest others stay. The
-- directory is read again, now that the copy is there.
--
-- The copy just written is always kept, even when its time is older than
-- the others, as after the clock was set back: it holds the database as
-- it is now, and the others give way to it. So a count of zero keeps one
-- copy, as a count of one does. The configuration refuses zero.
--
-- Every copy past the count goes, not the oldest alone: the user may
-- have lowered the count since the last copy.
--
-- What a cut-short write left goes too. A file that is gone already is
-- no failure: the user may have removed it.
rotate :: Natural -> FilePath -> (FilePath -> Maybe UTCTime) -> FilePath -> ExceptT String IO ()
rotate kept directory timeOf written = do
  (copies, leftovers) <- ExceptT (rollingFiles directory timeOf)
  let others = filter (/= written) copies
      -- The copy just written takes one of the places kept. The count
      -- may be of any size, so the subtraction is on Integer, which
      -- cannot overflow. A result below zero removes nothing.
      othersKept = toInteger kept - 1
      staleCount = genericLength others - othersKept :: Integer
      stale = genericTake staleCount others
  forM_ (stale <> leftovers) $ \old ->
    liftEither . first (couldNotRemove old) =<< liftIO (systemReason (removeIfThere old))
  where
    couldNotRemove old reason = "The old copy " <> old <> " could not be removed: " <> reason
    removeIfThere old =
      removeFile old `catchIOError` \failure ->
        if isDoesNotExistError failure then return () else ioError failure

{- The copies of the backup command -}

-- | Where the backup command puts its copies of a database and of its
-- labels: the user-backups directory next to the database. For the
-- database of hw, it is ~/.hw/user-backups. hw never removes a file
-- there, so it is apart from the auto-backups directory, where hw keeps
-- the newest copies of each kind alone.
userBackupDirectory :: FilePath -> FilePath
userBackupDirectory database = takeDirectory database </> "user-backups"

-- | Copy the database, at the format version given and as the stamp
-- describes it, into the user-backups directory, and write its labels
-- beside it, as exportwallet writes them. Both files take the name
-- 'databaseCopyName' gives, the copy with .sqlite and the labels with
-- .jsonl in its place. The answer is the two files, or why one could not
-- be written. The caller reads the stamp and the labels in the
-- transaction it holds while the copy is made, so that the three agree.
userBackup :: FilePath -> Int -> CopyStamp -> ByteString -> IO (Either String (FilePath, FilePath))
userBackup database version stamp labels = runExceptT $ do
  let directory = userBackupDirectory database
      stem = directory </> databaseCopyStem database version stamp
      copy = stem <> ".sqlite"
      labelsFile = stem <> ".jsonl"
  makePrivateDirectory directory
  liftEither =<< liftIO (copyDatabase database copy)
  liftEither =<< liftIO (writeFileBytes labelsFile labels)
  return (copy, labelsFile)

-- | Make a directory of copies, when it is new, so that its owner alone
-- can enter it, as ~/.hw ('privateDirectory'): a copy shows every address
-- of every wallet. Each copy is readable by its owner alone, as every
-- file hw writes. The directory goes next to the database, which is
-- there, so it is one level to make.
makePrivateDirectory :: FilePath -> ExceptT String IO ()
makePrivateDirectory directory =
  liftEither . first couldNotCreate =<< liftIO (systemReason (privateDirectory directory))
  where
    couldNotCreate reason = "The directory " <> directory <> " could not be created: " <> reason
