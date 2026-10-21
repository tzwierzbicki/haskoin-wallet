{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The version of the database format, and the migrations between
-- versions. The format version is a counter, independent of the version of
-- hw: it moves only when existing databases need a schema or data migration,
-- and every step is listed in 'migrations'.
module Haskoin.Wallet.Migration
  ( latestDatabaseVersion,
    databaseVersion,
    migrateDB,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (forM_, when)
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
import System.IO (hPutStrLn, stderr)

-- | The version of the database format of this hw. Version 1 is the format
-- of hw 0.8, before the version table existed; version 2 is the format of
-- hw 0.9, whose version table holds the text "0.9"; from version 3 the
-- version table holds the number itself. Bump it, and add a step to
-- 'migrations', whenever the schema or the stored data must change.
latestDatabaseVersion :: Int
latestDatabaseVersion = 3

-- | A step from the previous format version to 'migrationTo'. A step may
-- end with 'globalMigration' to create the tables and columns it adds; the
-- caller stamps the version.
data Migration m = Migration
  { migrationTo :: !Int,
    migrationName :: !String,
    migrationAction :: ExceptT String (DB m) ()
  }

-- | The migrations, in the order of their versions
migrations :: (MonadUnliftIO m) => Ctx -> [Migration m]
migrations ctx =
  [ Migration 2 "the transaction summaries of hw 0.9" (V2.migrateDB ctx),
    Migration 3 "the version stored as a number" V3.migrateDB
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

-- | Store the format version: the single row of the version table
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

-- | Bring the database to the latest format version: create the tables of
-- an empty database, or run the migrations above its version, each stamped
-- as it completes. An error rolls everything back.
migrateDB :: (MonadUnliftIO m) => Ctx -> DB m (Either String ())
migrateDB ctx = do
  result <- runExceptT $ migrate ctx
  when (isLeft result) transactionUndo
  return result

migrate :: (MonadUnliftIO m) => Ctx -> ExceptT String (DB m) ()
migrate ctx = do
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
      forM_ [m | m <- migrations ctx, migrationTo m > stored] $ \m -> do
        liftIO $ hPutStrLn stderr $ "Migrating the database to version " <> show (migrationTo m) <> ": " <> migrationName m
        migrationAction m
        lift $ setVersion (migrationTo m)
