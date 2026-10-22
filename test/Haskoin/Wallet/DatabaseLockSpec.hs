{-# LANGUAGE OverloadedStrings #-}

-- | The database lock: one hw command at a time. A command takes the lock
-- as it begins, whether it reads or writes, the next one waits for it a
-- while, and gives up with a message.
module Haskoin.Wallet.DatabaseLockSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Sql (Single (..), rawExecute, rawSql)
import Haskoin.Wallet.Commands (Response (..), cmdVersion)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils (inScratchDirectory)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Async (async, wait)

spec :: Spec
spec =
  describe "The database lock" $ do
    it "is taken as a command begins, even one that only reads" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
        held <- newEmptyMVar
        release <- newEmptyMVar
        reader <- async $ runDBWith 100 database $ do
          _ <- tables
          liftIO $ putMVar held ()
          liftIO $ takeMVar release
        takeMVar held
        catchDatabaseInUse (runDBWith 100 database tables) `shouldReturn` Left databaseInUse
        catchDatabaseInUse (runDBWith 100 database (rawExecute "create table t (x int)" []))
          `shouldReturn` Left databaseInUse
        putMVar release ()
        wait reader
        -- Released at the end of the command
        runDBWith 100 database tables `shouldReturn` []

    it "waits for a command that finishes in time" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
        held <- newEmptyMVar
        writer <- async $ runDBWith 100 database $ do
          rawExecute "create table first (x int)" []
          liftIO $ putMVar held ()
          -- Held for 0.3 seconds, well within the 2 seconds the other waits
          liftIO $ threadDelay 300000
        takeMVar held
        catchDatabaseInUse (runDBWith 2000 database (rawExecute "create table second (x int)" []))
          `shouldReturn` Right ()
        wait writer
        runDBWith 100 database tables `shouldReturn` ["first", "second"]

    it "answers a command with the message when the database stays held" $
      inScratchDirectory $ \dir -> do
        let database = dir </> "accounts.sqlite"
        held <- newEmptyMVar
        release <- newEmptyMVar
        holder <- async $ runDBWith 100 database $ do
          liftIO $ putMVar held ()
          liftIO $ takeMVar release
        takeMVar held
        -- The command waits its full 'busyTimeout' before answering
        cmdVersion (def :: Config) {configDatabaseFile = Just database}
          `shouldReturn` ResponseError (cs databaseInUse)
        putMVar release ()
        wait holder

-- | The tables of the database, by name
tables :: DB IO [Text]
tables = map unSingle <$> rawSql "select name from sqlite_master where type = 'table' order by name" []
