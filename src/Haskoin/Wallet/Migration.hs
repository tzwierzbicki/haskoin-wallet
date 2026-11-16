{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The version of the database format, and the frame of the migrations
-- between versions. The format version is a counter, independent of the
-- version of hw: it moves only when existing databases need a schema or
-- data migration. Version 5 is the format of hw 0.10.
--
-- hw 0.10 migrates no database of an earlier format, and 'steps' is
-- empty. The formats before it kept single accounts, where version 5
-- keeps whole portfolios of twelve accounts, and hw 0.9 kept no
-- fingerprint of the master key of a wallet. A public key of an account
-- derives none of the other accounts of its portfolio, which are behind
-- hardened steps of the path, and the fingerprint comes from the master
-- key: only the mnemonic gives either. A migration would have left
-- portfolios that lack accounts, and wallets without a fingerprint, for
-- the user to fill in from the mnemonic, on the offline computer when the
-- mnemonic is there. Creating the wallet again from the mnemonic is the
-- same work for the user, and leaves no half-made wallet behind. So a
-- database of an earlier version is refused, untouched, with what to do
-- instead ('migrateDB').
--
-- = What the frame does
--
-- The frame stays for the first migration, to version 6. Here is what it
-- does, with or without steps:
--
-- * An empty database gets the tables of this hw and the latest version.
--
-- * A database at the latest version is left as it is.
--
-- * A database of a later version is refused, untouched: a newer hw made
--   it, and this one would misread it.
--
-- * A database older than the version the first step starts from is
--   refused, untouched, with the instructions to create the wallet again
--   from its mnemonic. With no steps, that is every database older than
--   the latest version.
--
-- * Any other database is backed up once, as it is ('Hooks'), then each
--   step from its version on runs, in the order of the versions, in the
--   one transaction of the command. A step that fails undoes every step.
--   The copy is named as the other copies of the database are, with the
--   counts of its wallets and portfolios ('storedCopyStamp').
--
-- = Which schema to reference
--
-- Read this before writing any code that touches a table in or around a
-- migration. The mistake to avoid is to reference the wrong schema.
--
-- == The two schemas
--
-- * The live schema is the tables of the latest format version. Code
--   takes them from "Haskoin.Wallet.Database", which exports the latest
--   schema module again. Today the latest is version 5.
--
-- * A frozen schema is the module of a version that a later version
--   replaced. It never changes again. There is none yet.
--   "Haskoin.Wallet.Schema.V5" becomes one the day version 6 is added.
--
-- == The two kinds of code
--
-- * Live code is everything but the step modules. It is the commands, the
--   modules under "Haskoin.Wallet.Database", the backups and the frame in
--   this module, with their specs.
--
-- * A step module holds the step from version N-1 to version N. The first
--   one will be "Haskoin.Wallet.Migration.V6". Its spec counts with it.
--
-- == The rules
--
-- 1. Live code uses the live schema, and takes it from
--    "Haskoin.Wallet.Database". It never imports a schema module by its
--    version. Only "Haskoin.Wallet.Database" does, to export the latest
--    one.
--
-- 2. A step module uses the schema modules of its two versions, N-1 and
--    N, each imported by its version. It never uses live code. That means
--    no entity of "Haskoin.Wallet.Database", no type of
--    "Haskoin.Wallet.Database.TxInfo" and no 'latestDatabaseVersion'. A
--    type the step needs for stored data, the JSON of a blob for
--    instance, is copied into the step module.
--
-- 3. The frame in this module is live code, so rule 1 holds for it too.
--    It imports no schema module by its version, even though it runs on a
--    database that is still at an older version.
--
-- 4. No version before 5 has a schema module, a step or code that reads
--    its tables, and none is ever added. hw refuses such a database from
--    its version alone. The first step goes from version 5 to version 6.
--
-- 5. The tables of a released version never change in place. A change
--    goes into the schema module of a new version, with its step. Version
--    5 is released with hw 0.10.0. Until then its tables change in place,
--    and no version 6 is added.
--
-- == Why
--
-- Live code follows the latest version by itself. When a version changes
-- a table, the compiler points at every piece of live code that reads
-- it, and the work of that version fixes each one. Code pinned to an
-- older schema still compiles. So nobody looks at it, and it reads the
-- database wrongly.
--
-- A step is the opposite. It must keep working on a database of version
-- N-1 for as long as hw exists, long after the live code has moved on.
-- Anything a step takes from the live code breaks the day a later version
-- changes it.
--
-- == The frame before its steps
--
-- The frame runs before the steps, when the database is still behind the
-- live schema. A row read through a live entity could then fail on a
-- column that a step is about to add. So before the steps the frame only
-- reads the version, with plain SQL, and counts rows, which names a table
-- and none of its columns ('storedCopyStamp'). This is no reason to
-- import a frozen schema here. If a version ever renames a table the
-- frame counts, the work of that version changes how the frame counts
-- it.
--
-- == Mistakes to avoid
--
-- * Importing "Haskoin.Wallet.Schema.V5" in the frame, a command or a
--   backup, because the database may still be at version 5 there. Rule 1
--   says to use the live tables.
--
-- * Using an entity of "Haskoin.Wallet.Database" in a step module or its
--   spec, because it is the same type today. Rule 2 says to import the
--   schema module by its version. The two stop being the same type with
--   the next version.
--
-- * Leaving something out of a design because an older database could not
--   provide it. Rule 4 says that no older database is ever read.
--
-- * Adding a schema module to change a version that is not released yet,
--   or changing a released one in place. Rule 5 says which to do.
--
-- = Adding the format version 6, the first step
--
-- * Freeze the tables of version 5: "Haskoin.Wallet.Schema.V5" never
--   changes again. Copy its tables to "Haskoin.Wallet.Schema.V6", change
--   them there, and point "Haskoin.Wallet.Database" at the new module.
--
-- * Write the step in "Haskoin.Wallet.Migration.V6", against the tables of
--   version 6, and of version 5 where it reads them, and against the
--   stored data as hw 0.10 wrote it. Whatever types the step needs for
--   that data, the JSON of a blob for instance, copy them into the step
--   module as they are on the day of the copy. The step stores the
--   version 6 itself, and creates again the indexes of a table it has
--   persistent rebuild ("Haskoin.Wallet.Schema.V5".'tableIndexes').
--
-- * List the step in 'steps', as @Step 6 "what it changes" V6.migrateDB@,
--   and set 'latestDatabaseVersion' to 6. A database of version 5 then
--   migrates, and one of version 4 or below is still refused.
--
-- * Test the step in test/Haskoin/Wallet/Migration/V6Spec.hs, from the
--   tables of version 5 to version 6 only: build a database of version 5
--   with the entities of "Haskoin.Wallet.Schema.V5", fill it with rows as
--   hw 0.10 writes them, run 'migrateWith' with the step alone, and check
--   the result against the tables of version 6 and the data the step
--   should write.
--
-- * Keep the tests of the frame in test/Haskoin/Wallet/MigrationSpec.hs:
--   they run a step of their own, and do not change with the steps of hw.
--   Add there the whole way from version 5 to the latest one.
--
-- Every point of this list follows the rules above. The step and its
-- spec see versions 5 and 6 and nothing else, by rule 2.
module Haskoin.Wallet.Migration
  ( latestDatabaseVersion,
    databaseVersion,
    Step (..),
    Hooks (..),
    migrateDB,
    migrateWith,
    checkDatabaseFile,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (forM_, unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Either (isLeft)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import Database.Persist.Sql
  ( PersistValue (PersistInt64, PersistText),
    rawSql,
    transactionUndo,
    unSingle,
  )
import qualified Database.Persist as P
import Haskoin.Wallet.Backup (CopyStamp (..))
-- The live tables. This module never imports a schema module by its
-- version: see "Which schema to reference" in the header.
import Haskoin.Wallet.Database
import qualified Paths_haskoin_wallet as Paths
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))

{- The versions and their steps -}

-- | The version of the database format of this hw. Version 1 is the
-- format of hw 0.8, before the version table existed; version 2 is the
-- format of hw 0.9, whose version table holds the text "0.9"; from
-- version 3 the version table holds the number itself. Versions 3 and 4
-- were never released. Version 5 is the format of hw 0.10: wallets with
-- the fingerprint of their master key, their portfolios, and the twelve
-- accounts of each.
--
-- Whenever the tables or the stored data of a released version must
-- change, bump it, and add the tables and the step as the header of this
-- module says.
latestDatabaseVersion :: Int
latestDatabaseVersion = 5

-- | A step from the version before 'stepTo' to 'stepTo'. It leaves the
-- database as the hw of that version would have made it: its tables, its
-- data and its version stamp. It answers with what the user must be told
-- once it is done, if anything: what the step could not do by itself,
-- and how the user does it.
data Step m = Step
  { stepTo :: !Int,
    stepName :: !String,
    stepAction :: ExceptT String (DB m) [String]
  }

-- | The steps of this hw, in the order of their versions: none, since hw
-- 0.10 migrates no earlier database. The first one will be the step to
-- version 6.
steps :: (MonadUnliftIO m) => [Step m]
steps = []

-- | The oldest version a database can be at for the steps to bring it to
-- the version given: the one the first step starts from, or that version
-- itself when there is no step
oldestMigrated :: Int -> [Step m] -> Int
oldestMigrated target given = case given of
  first : _ -> stepTo first - 1
  [] -> target

{- The version of a database -}

getDatabaseTables :: (MonadUnliftIO m) => DB m [Text]
getDatabaseTables = do
  s <- rawSql "select name from sqlite_master where type='table'" []
  return $ unSingle <$> s

-- | The value stored in the version table, whatever its type: the entity
-- of "Haskoin.Wallet.Database" reads numbers only, but hw 0.9 stored a text
getStoredVersion :: (MonadUnliftIO m) => DB m (Maybe PersistValue)
getStoredVersion = do
  rows <- rawSql "select version from d_b_version" []
  return $ unSingle <$> listToMaybe rows

-- | Store the format version of a new database: the single row of the
-- version table. The steps store theirs themselves.
setVersion :: (MonadUnliftIO m) => Int -> DB m ()
setVersion version = do
  P.deleteWhere ([] :: [P.Filter DBVersion])
  P.insert_ $ DBVersion version

-- | The format version of the database, or Nothing for an empty database.
-- A database without the version table is at version 1: the table appeared
-- with version 2. This is all hw reads of an older database, to tell it
-- apart from a new one and from one of a newer hw.
databaseVersion :: (MonadUnliftIO m) => ExceptT String (DB m) (Maybe Int)
databaseVersion = do
  tables <- lift getDatabaseTables
  if null tables
    then return Nothing
    else
      if "d_b_version" `notElem` tables
        then return $ Just 1
        else do
          storedM <- lift getStoredVersion
          stored <- maybe (throwError "The database has no version") return storedM
          Just <$> versionOf stored

-- | The version a stored value stands for: the number itself, or the label
-- "0.9" that hw 0.9 wrote (version 2)
versionOf :: (Monad m) => PersistValue -> ExceptT String m Int
versionOf (PersistInt64 n) = return $ fromIntegral n
versionOf (PersistText label)
  | "0.9" `Text.isPrefixOf` label = return 2
  | otherwise = throwError $ "Unknown database version " <> cs label
versionOf value = throwError $ "Unknown database version " <> show value

{- Migrating -}

-- | What the migration does outside the database: announce each step before
-- it runs, and what a step has to tell the user once it is done, and back
-- up the database once before the first step, given the version it is at
-- and what the name of the copy says of it ('storedCopyStamp'). hw prints
-- the announcements to stderr and copies the database file; the tests
-- collect them instead.
data Hooks = Hooks
  { announce :: String -> IO (),
    backup :: Int -> CopyStamp -> IO ()
  }

-- | Bring the database file given to the latest format version with the
-- steps of this hw, or refuse it ('migrateWith'). The path of the file is
-- for the refusal of an older database, which says how to move it aside.
-- Every command of hw runs this first.
migrateDB :: (MonadUnliftIO m) => Hooks -> FilePath -> DB m (Either String ())
migrateDB hooks file = migrateWith latestDatabaseVersion steps hooks file

-- | Bring the database to a format version with the steps given, as the
-- header of this module says: hw gives the latest version and its steps
-- ('migrateDB'), and a test gives a version and a step of its own, to
-- test the frame or a step alone. An empty database gets the tables of
-- this hw at the latest version, whatever the version given. A database
-- of a later version than the one given is refused, and so is one older
-- than the first step starts from. Otherwise the steps above its version
-- run, up to the version given, after one backup of the database as it
-- is. A refusal, or a step that fails, rolls back everything.
migrateWith :: (MonadUnliftIO m) => Int -> [Step m] -> Hooks -> FilePath -> DB m (Either String ())
migrateWith target given hooks file = do
  result <- runExceptT $ migrate target given hooks file
  when (isLeft result) transactionUndo
  return result

migrate :: (MonadUnliftIO m) => Int -> [Step m] -> Hooks -> FilePath -> ExceptT String (DB m) ()
migrate target given hooks file = do
  versionM <- databaseVersion
  case versionM of
    Nothing -> lift $ do
      globalMigration
      setVersion latestDatabaseVersion
    Just stored -> do
      forM_ (versionRefusal target given file stored) throwError
      let pending = [s | s <- given, stepTo s > stored, stepTo s <= target]
      -- One backup of the database as it is, before anything changes
      unless (null pending) $ do
        stamp <- lift storedCopyStamp
        liftIO $ backup hooks stored stamp
      forM_ pending $ \s -> do
        liftIO $ announce hooks $ "Migrating the database to version " <> show (stepTo s) <> ": " <> stepName s
        notices <- stepAction s
        liftIO $ mapM_ (announce hooks) notices

-- | What the name of the copy before a migration says of the database:
-- the time, and how many wallets and portfolios it holds, as the other
-- copies of the database say it ("Haskoin.Wallet.Backup"). It is read in
-- the transaction of the migration, before the first step.
--
-- It counts the rows of the tables of this hw, as every other part of
-- the frame uses them. It does not read the rows, as the stamp of the
-- other copies does: the database is still one version behind the tables
-- of this hw, and a row read with a column the step is about to add
-- would fail. A count names the table alone.
storedCopyStamp :: (MonadIO m) => DB m CopyStamp
storedCopyStamp = do
  wallets <- P.count ([] :: [P.Filter DBWallet])
  portfolios <- P.count ([] :: [P.Filter DBPortfolio])
  now <- liftIO getCurrentTime
  return
    CopyStamp
      { stampTime = now,
        stampWallets = wallets,
        stampPortfolios = portfolios
      }

-- | Why this hw does not open a database at the version given, if it does
-- not: a version newer than the one the steps lead to, or one older than
-- the version the first step starts from. The runner refuses so, and so
-- does 'checkDatabaseFile', before hw opens the file to write.
versionRefusal :: Int -> [Step m] -> FilePath -> Int -> Maybe String
versionRefusal target given file stored
  | stored > target =
      Just $
        "The database is at version "
          <> show stored
          <> ", newer than the version "
          <> show target
          <> " of this hw"
  | stored < oldestMigrated target given = Just (olderDatabase file stored)
  | otherwise = Nothing

-- | Refuse a database file this hw does not open, before hw opens it to
-- write: one at a version no step migrates, or a newer one
-- ('versionRefusal'). The version is read through a connection that
-- writes nothing ('runDBReadOnly'). A connection that writes would set
-- the journal mode of a file that has another one, so a refused database
-- would not stay byte for byte as it was. A file that is not there, or a
-- database without tables, is no refusal: hw creates its tables.
checkDatabaseFile :: FilePath -> IO (Either String ())
checkDatabaseFile file = do
  exists <- doesFileExist file
  if not exists
    then return (Right ())
    else do
      versionM <- runDBReadOnly file (runExceptT databaseVersion)
      return $ do
        stored <- versionM
        forM_ stored $ \version ->
          forM_ (versionRefusal latestDatabaseVersion (steps :: [Step IO]) file version) Left

-- | The refusal of a database of an earlier version that no step
-- migrates, with the path of its file and its version: what to do
-- instead, which is to move it aside and create the wallet again from its
-- mnemonic. recover finds the portfolios that were paid. On an offline
-- computer, which cannot run recover, newportfolio --existing creates them
-- one by one, and exportwallet and importwallet carry them to the online
-- one. The commands name the file where it is, quoted for the shell when
-- its path needs it, so that the user can paste them.
olderDatabase :: FilePath -> Int -> String
olderDatabase file version =
  intercalate
    "\n"
    [ "The database " <> file <> " was made by an earlier version of hw, at format version " <> show version <> ".",
      "hw " <> showVersion Paths.version <> " does not read it, and has left it as it is. To go on, move it aside",
      "and create the wallet again from its mnemonic:",
      "",
      "  mv " <> shellWord file <> " " <> shellWord movedAside,
      "  hw recover",
      "",
      "recover finds the portfolios of the mnemonic that were paid. On an offline",
      "computer, run hw newportfolio --existing instead, once for each account index",
      "the old wallet used, from 0 up to the highest. Then carry the portfolios to the",
      "online computer with exportwallet and importwallet. The labels of the old",
      "database are not carried over."
    ]
  where
    -- portfolios.sqlite becomes portfolios-old.sqlite, in the same directory
    movedAside = takeDirectory file </> (takeBaseName file <> "-old" <> takeExtension file)

-- | A path as a word of the shell: as it is when every character of it
-- means itself there, in single quotes otherwise, with a quote inside it
-- written '\''
shellWord :: FilePath -> String
shellWord path
  | not (null path) && all plain path = path
  | otherwise = "'" <> concatMap quoted path <> "'"
  where
    plain c = c `elem` ("/._-+@%:,=" :: String) || c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z'] || c `elem` ['0' .. '9']
    quoted '\'' = "'\\''"
    quoted c = [c]
