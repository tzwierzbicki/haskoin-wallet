{-# LANGUAGE OverloadedStrings #-}

-- | The copies hw makes of the database and of its labels. There are
-- three kinds, each in a directory of its own.
--
-- * The migration copies the database into the migration-backups
--   directory before it changes its format, in case a step goes wrong
--   ('migrationBackupDirectory').
--
-- * hw copies the database and its labels by itself into the auto-backups
--   directory, after the commands that change them, and keeps the recent
--   copies of each and a thinning trail of older ones
--   ('autoBackupDirectory', 'autoBackupRetention'). A copy of the database
--   follows each command that adds a wallet or a portfolio
--   ('rollDatabaseCopy'). A copy of the labels follows each command that
--   changes what exportwallet writes ('rollLabelCopy'). hw copies both
--   there before it deletes a wallet or a portfolio, and before it
--   replaces the database with a copy ('rollBackup').
--
-- * The backup command copies the database and its labels into the
--   user-backups directory, and hw never removes them
--   ('userBackupDirectory', 'userBackup'). Only the user puts a file
--   there. Every copy hw makes by itself is in the auto-backups directory,
--   under one rule for what stays.
--
-- A copy of the labels is the file exportwallet writes without options:
-- every portfolio of every wallet, with its labels, its descriptions and
-- the coins the user locked ("Haskoin.Wallet.WalletFile"). importwallet reads
-- it back into a new database. It holds what nothing else can rebuild:
-- what the user typed. The mnemonic rebuilds the keys, and a sync the
-- history. A copy of the database keeps everything, the pending
-- transactions too, but only another hw of the same format reads it.
--
-- A copy is made after the change, so that the newest one holds the
-- database as it is now. When the user makes a third portfolio, the copy
-- holds three portfolios. A command that loses what the database holds
-- has a copy before it, the last one that holds what it lost
-- ('rollBackup'). Such a command has that copy alone: the deletion of a
-- wallet or a portfolio, and restoredatabase. What the database holds
-- after it is elsewhere already, in the copy made before a deletion, and
-- in the file a restore read. A copy after it would also take the place
-- of the copy made before as the last of its hour, which the rules below
-- keep.
--
-- The three directories are next to the database, in the data directory
-- of hw: ~\/.hw\/migration-backups, ~\/.hw\/auto-backups and
-- ~\/.hw\/user-backups. They show every address of the database, so they
-- stay with it. A user who makes ~\/.hw a link to an encrypted disk has
-- the copies there too ("Haskoin.Wallet.Config"). The tests of hw keep
-- their databases in scratch directories, so their copies stay there too,
-- away from the wallet of the user.
--
-- = The names of the copies
--
-- A name starts with the name of the database, portfolios for
-- portfolios.sqlite, the file a copy goes back in place of. It then
-- carries the time of the copy, in UTC to the millisecond, and how many
-- wallets and portfolios the database held ('labelCopyName'). The name of
-- a copy of the database also carries its format version
-- ('databaseCopyName'). So the user can tell the copies apart without
-- opening them. The name counts the wallets and the portfolios, and show
-- lists them, from the copy itself, for the user who is about to restore
-- one ("Haskoin.Wallet.Database.File").
--
-- The time is in UTC so that the names sort in the order the copies were
-- made, whatever the time zone, even in the hour the clock goes back in
-- autumn. Local time repeats an hour in autumn, and its names would then
-- sort out of order. The milliseconds let two commands in one second
-- write two copies.
--
-- The three directories name their files alike. A copy of the database
-- has the same name in migration-backups, auto-backups and user-backups,
-- and so does a copy of the labels. So the user who has learnt one name
-- reads them all.
--
-- = Which copies are kept
--
-- hw keeps copies by their age, not by their number alone
-- ('autoBackupRetention', 'retained'). Of each kind it keeps:
--
-- * The ten newest copies, whatever their times.
--
-- * The last copy of each hour, for the last 24 hours that have a copy.
--
-- * The last copy of each day, for the last 7 such days.
--
-- * The last copy of each week, for the last 4 such weeks.
--
-- * The last copy of each month, for the last 12 such months.
--
-- A copy that any of the five rules keeps stays. Every other copy is
-- removed. That is 57 copies of a kind at most, and far fewer as a rule,
-- since one copy often serves several rules.
--
-- A count alone would not do. Each command that sets a label writes a
-- copy of the labels, so a script that labels twenty addresses writes
-- twenty copies in a few seconds. With the newest ten alone, they would
-- push out every copy made before the script ran, the one the user wants
-- back when the script was wrong. The copies by hour, day, week and month
-- reach further back, and one copy stands for each period however many
-- were made in it.
--
-- This is the retention that backup programs call grandfather, father and
-- son. The ladder is the common one. Each step covers one unit of the
-- next, a day of hours, a week of days, a month of weeks and a year of
-- months. The numbers are fixed. A key of the configuration set one count
-- before, and one count cannot say five. The two keys that turn the
-- copies off are still there.
--
-- The rules count the periods that have a copy, not the periods of the
-- calendar back from today. So the rotation needs no clock. The names of
-- the files tell it all. A wallet left alone for two years loses no copy
-- to the passing of time. Only a newer copy pushes an older one out.
--
-- The periods are those of UTC, as the times of the names are. The
-- periods of the time zone of the computer would move the day a copy
-- falls in when the computer travels. A week starts on Monday.
--
-- = Which files the rotation touches
--
-- The user may keep
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
-- copy an earlier hw named otherwise, and the new file a write fills
-- before it takes its name. A copy of the database of another computer
-- is not covered. Every database of hw is named portfolios.sqlite, so
-- such a copy has a name hw writes, and counts as one of the copies.
--
-- The new copy is written first, and whole, before an older one goes
-- ('rotate'). So a failure never leaves fewer copies. The copy just
-- written is always kept, even when its time is the oldest, as after the
-- clock was set back.
--
-- Two hw at once may each remove copies. Neither removes a copy the other
-- would keep. A copy that the rules keep among all the files of the
-- directory is kept among any part of them too.
module Haskoin.Wallet.Database.Backup
  ( -- * The copies before a migration
    migrationBackupDirectory,
    backupDatabase,
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
    rollBackup,
    rollingFiles,
    databaseCopyTime,
    labelCopyTime,
    rotate,

    -- * Which copies are kept
    Retention (..),
    autoBackupRetention,
    retained,

    -- * The copies of the backup command
    userBackupDirectory,
    userBackup,

    -- * What the directories hold
    BackupsShown (..),
    CopiesShown (..),
    backupsShown,

    -- * In JSON
    backupsShownPairs,
    parseBackupsShown,
  )
where

import Control.Exception (bracket, try)
import Control.Monad (forM, forM_, guard, void)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, withObject, (.:), (.=))
import Data.Aeson.Types (Object, Pair, Parser)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.List (sortOn, stripPrefix)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Time (Day, TimeOfDay (..), UTCTime (..), defaultTimeLocale, formatTime, parseTimeM, timeToTimeOfDay, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Database.Persist.Sql (PersistValue (PersistText))
import qualified Database.Sqlite as Sqlite
import Haskoin.Wallet.FileIO (Placement (..), placeFile, privateDirectory, readFileBytes, systemReason, writeFileBytes)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile)
import System.FilePath (splitExtension, takeBaseName, takeDirectory, (</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.Files (fileSize, getSymbolicLinkStatus, isRegularFile)
import Text.Read (readMaybe)

{- The copies before a migration -}

-- | Where the migration puts its copies of a database: the
-- migration-backups directory next to it. For the database of hw, it is
-- ~\/.hw\/migration-backups.
migrationBackupDirectory :: FilePath -> FilePath
migrationBackupDirectory database = takeDirectory database </> "migration-backups"

-- | Copy the database, at the given format version and as the stamp
-- describes it, into a directory, created if missing, and return the
-- copy, or say why it could not be made. The copy takes the name of a
-- copy of the database in the other directories ('databaseCopyName').
-- The migration reads the stamp from the database as it is, before its
-- first step.
backupDatabase :: FilePath -> FilePath -> Int -> CopyStamp -> IO (Either String FilePath)
backupDatabase database directory version stamp = do
  let copy = directory </> databaseCopyName database version stamp
  created <- systemReason (createDirectoryIfMissing True directory)
  case created of
    Left reason -> return $ Left ("The directory " <> directory <> " could not be created: " <> reason)
    Right () -> do
      copied <- copyDatabase database copy
      return (copy <$ copied)

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

-- | The name of a copy of the database at the format version given:
-- portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite
-- for portfolios.sqlite at version 5, copied at 10:05:33.123 UTC on
-- 2026-11-14 with 2 wallets and 3 portfolios
databaseCopyName :: FilePath -> Int -> CopyStamp -> FilePath
databaseCopyName database version stamp =
  takeBaseName database <> "-version-" <> show version <> "-" <> stampSuffix stamp <> ".sqlite"

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
-- database of hw, it is ~\/.hw\/auto-backups. Its name says that hw looks
-- after it: hw removes the older copies there.
autoBackupDirectory :: FilePath -> FilePath
autoBackupDirectory database = takeDirectory database </> "auto-backups"

-- | Copy the database, at the format version given and as the stamp
-- describes it, into the auto-backups directory, and keep those of its
-- copies there that the retention given first keeps. The caller reads
-- the counts of the stamp in the transaction it holds while the copy is
-- made, so that no other command changes them in between. The answer is
-- the copy, or why it could not be made, or why an older copy could not
-- be removed.
rollDatabaseCopy :: Retention -> FilePath -> Int -> CopyStamp -> IO (Either String FilePath)
rollDatabaseCopy retention database version stamp = runExceptT $ do
  let directory = autoBackupDirectory database
      copy = directory </> databaseCopyName database version stamp
  makePrivateDirectory directory
  liftEither =<< liftIO (copyDatabase database copy)
  rotate retention directory (databaseCopyTime database) copy
  return copy

-- | The time the name of a file gives, when it is the name of a copy of
-- the database
databaseCopyTime :: FilePath -> FilePath -> Maybe UTCTime
databaseCopyTime database = fmap (stampTime . snd) . databaseCopyStamp database

-- | The time the name of a file gives, when it is the name of a copy of
-- the labels of the database
labelCopyTime :: FilePath -> FilePath -> Maybe UTCTime
labelCopyTime database = fmap stampTime . labelCopyStamp database

-- | Write the labels of a database, as exportwallet writes them, to a new
-- copy the stamp describes, and keep those of the copies that the
-- retention given first keeps. The answer is the copy written, or Nothing
-- when the newest copy holds the same labels already, which writes
-- nothing: that copy would push out an older one that differs, for
-- nothing. A copy that cannot be made is a message, and so is an older
-- copy that cannot be removed.
rollLabelCopy :: Retention -> FilePath -> CopyStamp -> ByteString -> IO (Either String (Maybe FilePath))
rollLabelCopy retention database stamp labels =
  runExceptT $ either (const Nothing) Just <$> placeLabelCopy retention database stamp labels

-- | The work of 'rollLabelCopy', which also names the copy that holds the
-- labels when it writes none: Left is the newest copy, which holds them
-- already, and Right the copy written
placeLabelCopy :: Retention -> FilePath -> CopyStamp -> ByteString -> ExceptT String IO (Either FilePath FilePath)
placeLabelCopy retention database stamp labels = do
  let directory = autoBackupDirectory database
      copy = directory </> labelCopyName database stamp
      timeOf = labelCopyTime database
  makePrivateDirectory directory
  (before, _) <- ExceptT (rollingFiles directory timeOf)
  let newest = snd <$> listToMaybe (reverse before)
  held <- liftIO $ mapM readFileBytes newest
  case newest of
    Just file | held == Just (Right labels) -> return (Left file)
    _ -> do
      liftEither =<< liftIO (writeFileBytes copy labels)
      rotate retention directory timeOf copy
      return (Right copy)

-- | Before a command loses what the database holds, copy the database
-- and its labels into the auto-backups directory, as the stamp describes
-- them, and answer with the file of each. The two are the copies hw
-- makes after a command, made once more before this one
-- ('rollDatabaseCopy', 'rollLabelCopy'). So one rule says what stays in
-- the directory, and the retention given first applies to them as to any
-- other copy.
--
-- The labels are not written again when the newest copy holds them
-- already, as after any command. The answer then names that copy, which
-- is where the labels are.
--
-- The backup command keeps its copies for good, in the user-backups
-- directory ('userBackup'). These copies are not there, since the user
-- did not ask for them: that directory holds what the user put in it,
-- and nothing else.
rollBackup :: Retention -> FilePath -> Int -> CopyStamp -> ByteString -> IO (Either String (FilePath, FilePath))
rollBackup retention database version stamp labels = runExceptT $ do
  copy <- ExceptT (rollDatabaseCopy retention database version stamp)
  labelsFile <- either id id <$> placeLabelCopy retention database stamp labels
  return (copy, labelsFile)

-- | What a directory holds of one kind of copy: the copies, each with
-- the time its name gives, the oldest first, then the empty files under
-- the name of a copy, which a write that was cut short left. Or why the
-- directory could not be read.
--
-- The function given reads the time of a copy from the name of a file,
-- and gives nothing for any other name. A file is a copy when its name
-- reads so, when it is a regular file, and when it holds something. The
-- header of this module says why each of the three is asked. A directory
-- that is not there holds nothing.
rollingFiles :: FilePath -> (FilePath -> Maybe UTCTime) -> IO (Either String ([(UTCTime, FilePath)], [FilePath]))
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
      copies = [(time, file) | (time, file, empty) <- byTime, not empty]
      leftovers = [file | (_, file, empty) <- byTime, empty]
  return (copies, leftovers)
  where
    couldNotRead reason = "The directory " <> directory <> " could not be read: " <> reason

-- | Once a copy is written, remove the copies of its kind that the
-- retention given first no longer keeps ('retained'). The directory is
-- read again, now that the copy is there.
--
-- The copy just written is always kept, even when no rule keeps it. That
-- happens after the clock was set back, when its time is older than
-- those of the others. It holds the database as it is now, so it stays.
--
-- What a cut-short write left goes too. A file that is gone already is
-- no failure. The user may have removed it, or another hw.
rotate :: Retention -> FilePath -> (FilePath -> Maybe UTCTime) -> FilePath -> ExceptT String IO ()
rotate retention directory timeOf written = do
  (copies, leftovers) <- ExceptT (rollingFiles directory timeOf)
  let kept = written : map snd (retained retention fst copies)
      stale = [file | (_, file) <- copies, file `notElem` kept]
  forM_ (stale <> leftovers) $ \old ->
    liftEither . first (couldNotRemove old) =<< liftIO (systemReason (removeIfThere old))
  where
    couldNotRemove old reason = "The old copy " <> old <> " could not be removed: " <> reason
    removeIfThere old =
      removeFile old `catchIOError` \failure ->
        if isDoesNotExistError failure then return () else ioError failure

{- Which copies are kept -}

-- | How many copies of one kind the rotation keeps: the newest ones, and
-- the last copy of each of the latest hours, days, weeks and months that
-- have a copy. A copy stays when any of the five keeps it. The header of
-- this module says why copies are kept by their age.
data Retention = Retention
  { -- | The newest copies, whatever their times: the last changes, each
    -- of which the user can go back to
    keptNewest :: !Int,
    -- | The hours whose last copy is kept
    keptHourly :: !Int,
    -- | The days whose last copy is kept
    keptDaily :: !Int,
    -- | The weeks whose last copy is kept
    keptWeekly :: !Int,
    -- | The months whose last copy is kept
    keptMonthly :: !Int
  }
  deriving (Eq, Show)

-- | The copies hw keeps of each kind in the auto-backups directory: the
-- ten newest, then the last copy of each hour for 24 hours, of each day
-- for 7 days, of each week for 4 weeks, and of each month for 12 months.
-- Each step covers one unit of the next: a day, a week, a month and a
-- year. Backup programs keep copies on the same ladder.
--
-- Ten newest copies let the user step back through the last ten changes
-- one by one. The copies are small. Those of the labels are a few
-- kilobytes each. Those of the database are larger, but one is made only
-- when a wallet or a portfolio is added or deleted.
autoBackupRetention :: Retention
autoBackupRetention =
  Retention
    { keptNewest = 10,
      keptHourly = 24,
      keptDaily = 7,
      keptWeekly = 4,
      keptMonthly = 12
    }

-- | Those of the copies that a retention keeps, the oldest first, given
-- the time of each copy. Nothing but the times tells. The rule needs no
-- clock, so it is the same today and in a year.
--
-- A period counts only when it has a copy. The last 7 days are the 7
-- latest days in which a copy was made, however far apart they are.
retained :: Retention -> (copy -> UTCTime) -> [copy] -> [copy]
retained retention timeOf copies =
  reverse [copy | (rank, copy) <- ranked, rank `Set.member` keptRanks]
  where
    -- The copies from the newest to the oldest, each with its rank
    ranked = zip [0 :: Int ..] (sortOn (Down . timeOf) copies)
    keptRanks =
      Set.fromList . concat $
        [ take (keptNewest retention) (map fst ranked),
          lastOfEach hourOf (keptHourly retention),
          lastOfEach dayOf (keptDaily retention),
          lastOfEach weekOf (keptWeekly retention),
          lastOfEach monthOf (keptMonthly retention)
        ]
    -- The rank of the last copy of each period that has one, for as many
    -- periods as the count, from the latest back. The copies of a period
    -- follow each other in the ranking, and the first of them is the
    -- last one made.
    lastOfEach :: (Eq period) => (UTCTime -> period) -> Int -> [Int]
    lastOfEach periodOf count =
      take count [fst (NonEmpty.head samePeriod) | samePeriod <- NonEmpty.groupBy inOnePeriod ranked]
      where
        inOnePeriod (_, one) (_, other) = periodOf (timeOf one) == periodOf (timeOf other)

-- | The hour of UTC a time is in: its day, and the hour of that day
hourOf :: UTCTime -> (Day, Int)
hourOf time = (utctDay time, todHour (timeToTimeOfDay (utctDayTime time)))

-- | The day of UTC a time is in
dayOf :: UTCTime -> Day
dayOf = utctDay

-- | The week of UTC a time is in, as ISO 8601 numbers the weeks: the year
-- of the week and its number. A week starts on Monday.
weekOf :: UTCTime -> (Integer, Int)
weekOf time = (year, week)
  where
    (year, week, _) = toWeekDate (utctDay time)

-- | The month of UTC a time is in: its year, and the month of that year
monthOf :: UTCTime -> (Integer, Int)
monthOf time = (year, month)
  where
    (year, month, _) = toGregorian (utctDay time)

{- The copies of the backup command -}

-- | Where the backup command puts its copies of a database and of its
-- labels: the user-backups directory next to the database. For the
-- database of hw, it is ~\/.hw\/user-backups. hw never removes a file
-- there, so it is apart from the auto-backups directory, where hw keeps
-- the newest copies of each kind alone.
userBackupDirectory :: FilePath -> FilePath
userBackupDirectory database = takeDirectory database </> "user-backups"

-- | Copy the database, at the format version given and as the stamp
-- describes it, into the user-backups directory, and write its labels
-- beside it, as exportwallet writes them. The two files take the names
-- of the copies in the auto-backups directory, 'databaseCopyName' and
-- 'labelCopyName', so that a name means the same in both directories.
-- They share the stamp, which pairs them. The answer is the two files,
-- or why one could not be written. The caller reads the stamp and the
-- labels in the transaction it holds while the copy is made, so that the
-- three agree.
userBackup :: FilePath -> Int -> CopyStamp -> ByteString -> IO (Either String (FilePath, FilePath))
userBackup database version stamp labels = runExceptT $ do
  let directory = userBackupDirectory database
      copy = directory </> databaseCopyName database version stamp
      labelsFile = directory </> labelCopyName database stamp
  makePrivateDirectory directory
  liftEither =<< liftIO (copyDatabase database copy)
  liftEither =<< liftIO (writeFileBytes labelsFile labels)
  return (copy, labelsFile)

{- What the directories hold -}

-- | The copies of one kind in a directory, as showbackups shows them:
-- how many there are, and the newest one with the time its name gives.
-- A directory without a copy has no newest one.
data CopiesShown = CopiesShown
  { copiesCount :: !Int,
    copiesNewest :: !(Maybe (FilePath, UTCTime))
  }
  deriving (Eq, Show)

-- | The copies hw keeps of a database, as showbackups shows them. The
-- user reads there that the copies are being made, and how old the
-- newest of each kind is. A copy that could not be made is a warning on
-- stderr at the time, which a script may not show. This answer is where
-- the user sees it later. The newest copy is then older than their last
-- change.
--
-- It also tells whether each kind of automatic copy is on, since a kind
-- that is off stops growing without a word.
data BackupsShown = BackupsShown
  { -- | The auto-backups directory
    backupsAutoDirectory :: !FilePath,
    -- | Whether hw copies the database by itself
    backupsDatabaseOn :: !Bool,
    -- | The copies of the database in the auto-backups directory
    backupsDatabase :: !CopiesShown,
    -- | Whether hw copies the labels by itself
    backupsLabelsOn :: !Bool,
    -- | The copies of the labels in the auto-backups directory
    backupsLabels :: !CopiesShown,
    -- | The user-backups directory
    backupsUserDirectory :: !FilePath,
    -- | The copies of the database in the user-backups directory. Each
    -- has its labels beside it, which are not counted apart.
    backupsUser :: !CopiesShown
  }
  deriving (Eq, Show)

-- | What the two directories of copies of a database hold now, given
-- whether the configuration has hw copy the database and the labels by
-- itself. Or why a directory could not be read. A directory that is not
-- there holds no copy.
--
-- A file counts as a copy when the rotation would take it for one
-- ('rollingFiles'). So the counts are those of the files hw looks after,
-- and a file of the user in a directory is left out.
backupsShown :: Bool -> Bool -> FilePath -> IO (Either String BackupsShown)
backupsShown databaseOn labelsOn database = runExceptT $ do
  let automatic = autoBackupDirectory database
      kept = userBackupDirectory database
  databaseCopies <- copiesIn automatic (databaseCopyTime database)
  labelCopies <- copiesIn automatic (labelCopyTime database)
  userCopies <- copiesIn kept (databaseCopyTime database)
  return
    BackupsShown
      { backupsAutoDirectory = automatic,
        backupsDatabaseOn = databaseOn,
        backupsDatabase = databaseCopies,
        backupsLabelsOn = labelsOn,
        backupsLabels = labelCopies,
        backupsUserDirectory = kept,
        backupsUser = userCopies
      }
  where
    copiesIn directory timeOf = do
      (copies, _) <- ExceptT (rollingFiles directory timeOf)
      return
        CopiesShown
          { copiesCount = length copies,
            copiesNewest = (\(time, file) -> (file, time)) <$> listToMaybe (reverse copies)
          }

-- | The keys of the copies of one kind as the --json answer of
-- showbackups writes them: how many there are, and the newest one with
-- its time, or null in a directory without a copy
copiesShownPairs :: CopiesShown -> [Pair]
copiesShownPairs copies =
  [ "copies" .= copiesCount copies,
    "newest" .= (newestJson <$> copiesNewest copies)
  ]
  where
    newestJson (file, time) = object ["file" .= file, "time" .= time]

-- | The copies of one kind back from the keys 'copiesShownPairs' writes
parseCopiesShown :: Object -> Parser CopiesShown
parseCopiesShown o = do
  newest <- o .: "newest"
  CopiesShown <$> o .: "copies" <*> mapM newestOf newest
  where
    newestOf = withObject "newest copy" $ \copy -> (,) <$> copy .: "file" <*> copy .: "time"

-- | The keys of the copies hw keeps as the --json answer of showbackups
-- writes them: the automatic backups, with their directory and each kind
-- of copy, which says whether it is on, and the backups of the user
backupsShownPairs :: BackupsShown -> [Pair]
backupsShownPairs shown =
  [ "automatic"
      .= object
        [ "directory" .= backupsAutoDirectory shown,
          "database" .= object (("enabled" .= backupsDatabaseOn shown) : copiesShownPairs (backupsDatabase shown)),
          "labels" .= object (("enabled" .= backupsLabelsOn shown) : copiesShownPairs (backupsLabels shown))
        ],
    "user" .= object (("directory" .= backupsUserDirectory shown) : copiesShownPairs (backupsUser shown))
  ]

-- | The copies hw keeps back from the keys 'backupsShownPairs' writes
parseBackupsShown :: Object -> Parser BackupsShown
parseBackupsShown o = do
  automatic <- o .: "automatic"
  database <- automatic .: "database"
  labels <- automatic .: "labels"
  user <- o .: "user"
  BackupsShown
    <$> automatic .: "directory"
    <*> database .: "enabled"
    <*> parseCopiesShown database
    <*> labels .: "enabled"
    <*> parseCopiesShown labels
    <*> user .: "directory"
    <*> parseCopiesShown user

-- | Make a directory of copies, when it is new, so that its owner alone
-- can enter it, as ~\/.hw ('privateDirectory'): a copy shows every address
-- of every wallet. Each copy is readable by its owner alone, as every
-- file hw writes. The directory goes next to the database, which is
-- there, so it is one level to make.
makePrivateDirectory :: FilePath -> ExceptT String IO ()
makePrivateDirectory directory =
  liftEither . first couldNotCreate =<< liftIO (systemReason (privateDirectory directory))
  where
    couldNotCreate reason = "The directory " <> directory <> " could not be created: " <> reason
