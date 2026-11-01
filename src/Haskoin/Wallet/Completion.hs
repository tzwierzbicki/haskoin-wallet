-- | What the shell completes from the wallet: the names of the accounts.
-- The shell runs hw at each Tab, so a completion reads the database and
-- changes nothing. It writes no configuration file and creates no
-- database. It migrates nothing, writes nothing, and does not wait for
-- another command. When it cannot read the database, the shell offers
-- nothing for the word.
module Haskoin.Wallet.Completion
  ( accountNames,
    readForCompletion,
  )
where

import Control.Monad.Except (runExceptT)
import Data.String.Conversions (cs)
import Haskoin.Wallet.Account (getAccountNames)
import Haskoin.Wallet.Config (configAsItIs, databaseFileIn)
import Haskoin.Wallet.Database (DB, runDBReadOnly)
import Haskoin.Wallet.Migration (databaseVersion, latestDatabaseVersion)
import qualified System.Directory as D
import UnliftIO.Exception (handleAny)

-- | The names of the accounts, for --account and the arguments that name
-- an account
accountNames :: IO [String]
accountNames = readForCompletion (map cs <$> getAccountNames)

-- | Run a read of the wallet database for the shell, or give nothing. The
-- database is the one hw works on: ~/.hw/accounts.sqlite, or the file
-- the configuration names. It gives nothing when there is no database,
-- when the database is of another format version than this hw's, and on
-- any failure.
readForCompletion :: DB IO [a] -> IO [a]
readForCompletion action = handleAny (const (return [])) $ do
  -- The directory of hw, without creating it
  dir <- D.getAppUserDataDirectory "hw"
  configM <- configAsItIs dir
  case configM of
    Nothing -> return []
    Just cfg -> do
      let file = databaseFileIn dir cfg
      exists <- D.doesFileExist file
      if exists then runDBReadOnly file readCurrent else return []
  where
    -- A database of an older version is migrated by the next command, and
    -- one of a newer version is refused. Their tables may differ.
    readCurrent = do
      version <- runExceptT databaseVersion
      if version == Right (Just latestDatabaseVersion) then action else return []
