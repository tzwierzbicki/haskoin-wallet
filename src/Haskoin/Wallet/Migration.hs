{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The version of the database format, and the migrations between
-- versions. The format version is a counter, independent of the version of
-- hw: it moves only when existing databases need a schema or data migration,
-- and every step is listed in 'steps'. Each version from 2 on has a module
-- under "Haskoin.Wallet.Migration" holding the step from the version before
-- it, written against the tables of its version under "Haskoin.Wallet.Schema"
-- and against the stored data as the hw of its version wrote it. A step knows
-- nothing of the versions after it, so a database goes from version to
-- version as the hw of each version left it.
module Haskoin.Wallet.Migration
  ( latestDatabaseVersion,
    databaseVersion,
    Hooks (..),
    migrateDB,
    migrateDBTo,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (forM_, unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Either (isLeft)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql
  ( PersistValue (PersistInt64, PersistText),
    rawSql,
    transactionUndo,
    unSingle,
  )
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Database
import qualified Haskoin.Wallet.Migration.V2 as V2
import qualified Haskoin.Wallet.Migration.V3 as V3

-- | The version of the database format of this hw. Version 1 is the format
-- of hw 0.8, before the version table existed; version 2 is the format of
-- hw 0.9, whose version table holds the text "0.9"; from version 3 the
-- version table holds the number itself. Whenever the tables or the stored
-- data must change, bump it, add the new tables as a module under
-- "Haskoin.Wallet.Schema" and point "Haskoin.Wallet.Database" at them, and
-- add the step as a module under "Haskoin.Wallet.Migration", listed in
-- 'steps'.
latestDatabaseVersion :: Int
latestDatabaseVersion = 3

-- | A step from the version before 'stepTo' to 'stepTo'. It leaves the
-- database as the hw of that version left it: its tables, its data and its
-- version stamp.
data Step m = Step
  { stepTo :: !Int,
    stepName :: !String,
    stepAction :: ExceptT String (DB m) ()
  }

-- | The steps, in the order of their versions
steps :: (MonadUnliftIO m) => Ctx -> [Step m]
steps ctx =
  [ Step 2 "the transaction summaries of hw 0.9" (V2.migrateDB ctx),
    Step 3 "the version stored as a number" V3.migrateDB
  ]

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

-- | What the migration does outside the database: announce each step before
-- it runs, and back up the database once before the first step, given the
-- version it is at. hw prints the announcements to stderr and copies the
-- database file; the tests collect them instead.
data Hooks = Hooks
  { announce :: String -> IO (),
    backup :: Int -> IO ()
  }

-- | Store the format version of a new database: the single row of the
-- version table. The steps store theirs themselves, as their hw did.
setVersion :: (MonadUnliftIO m) => Int -> DB m ()
setVersion version = do
  P.deleteWhere ([] :: [P.Filter DBVersion])
  P.insert_ $ DBVersion version

-- | The format version of the database, or Nothing for an empty database.
-- A database without the version table is at version 1: the table appeared
-- with version 2.
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

-- | Bring the database to the latest format version. See 'migrateDBTo'.
migrateDB :: (MonadUnliftIO m) => Hooks -> Ctx -> DB m (Either String ())
migrateDB = migrateDBTo latestDatabaseVersion

-- | Bring the database to a format version, the latest one for hw: run the
-- steps above its version up to that one, after one backup of the database
-- as it is. An empty database gets the tables of this hw at the latest
-- version, whatever the target. An error rolls everything back. The tests of
-- a step migrate to its version only.
migrateDBTo :: (MonadUnliftIO m) => Int -> Hooks -> Ctx -> DB m (Either String ())
migrateDBTo target hooks ctx = do
  result <- runExceptT $ migrate target hooks ctx
  when (isLeft result) transactionUndo
  return result

migrate :: (MonadUnliftIO m) => Int -> Hooks -> Ctx -> ExceptT String (DB m) ()
migrate target hooks ctx = do
  versionM <- databaseVersion
  case versionM of
    Nothing -> lift $ do
      globalMigration
      setVersion latestDatabaseVersion
    Just stored -> do
      when (stored > latestDatabaseVersion) $
        throwError $
          "The database is at version "
            <> show stored
            <> ", newer than the version "
            <> show latestDatabaseVersion
            <> " of this hw"
      let pending = [s | s <- steps ctx, stepTo s > stored, stepTo s <= target]
      -- One backup of the database as it is, before anything changes
      unless (null pending) $ liftIO $ backup hooks stored
      forM_ pending $ \s -> do
        liftIO $ announce hooks $ "Migrating the database to version " <> show (stepTo s) <> ": " <> stepName s
        stepAction s
