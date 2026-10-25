{-# LANGUAGE OverloadedStrings #-}

-- | The copies of the database: the copy itself, and the backup command
module Haskoin.Wallet.BackupSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.List (isPrefixOf, isSuffixOf)
import Data.String.Conversions (cs)
import Data.Time (LocalTime (..), TimeOfDay (..), ZonedTime (..), fromGregorian, utc)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..), transactionSave)
import Haskoin.Wallet.Backup (backupDatabase, backupName)
import Haskoin.Wallet.Commands (Response (..), cmdBackup)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Migration (latestDatabaseVersion)
import Haskoin.Wallet.TestUtils
import qualified Haskoin.Wallet.Schema.V2 as V2
import System.FilePath (takeDirectory, takeFileName, (</>))
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
          createTables V2.migrateAll
          P.insert_ $ V2.DBVersion "0.9"
          P.insert_ $ V2.DBWallet "892eb8e4"
        copy <- backupDatabase database backups 2
        takeDirectory copy `shouldBe` backups
        takeFileName copy `shouldSatisfy` \name -> "accounts-version-2-" `isPrefixOf` name && ".sqlite" `isSuffixOf` name
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistText "0.9", "text")]
        inDatabaseFile copy (tableRows "d_b_wallet") `shouldReturn` [[PersistText "892eb8e4"]]

    it "includes the rows still in the write-ahead log" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
        -- Committed while the connection is open: the rows are in the
        -- write-ahead log, not yet in the file
        copy <- inDatabaseFile database $ do
          createTables V2.migrateAll
          P.insert_ $ V2.DBVersion "0.9"
          transactionSave
          liftIO $ backupDatabase database (dir </> "backups") 2
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistText "0.9", "text")]

  describe "The backup command" $ do
    it "copies the database to the given file" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            copy = dir </> "copy.sqlite"
        inDatabaseFile database $ do
          createTables migrateAll
          P.insert_ $ DBVersion latestDatabaseVersion
          P.insert_ $ DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105
        cmdBackup (configUsing database) (Just copy) `shouldReturn` ResponseFile copy
        inDatabaseFile copy storedVersions `shouldReturn` [(PersistInt64 (fromIntegral latestDatabaseVersion), "integer")]
        inDatabaseFile copy (tableRows "d_b_best")
          `shouldReturn` [[PersistText "btc", PersistText "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054", PersistInt64 800105]]

    it "refuses to overwrite a file" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
            copy = dir </> "copy.sqlite"
        inDatabaseFile database $ createTables migrateAll
        _ <- cmdBackup (configUsing database) (Just copy)
        cmdBackup (configUsing database) (Just copy)
          `shouldReturn` ResponseError (cs $ "Path '" <> copy <> "' already exists")

-- | The configuration of an hw whose database is the given file
configUsing :: FilePath -> Config
configUsing database = def {configDatabaseFile = Just database}
