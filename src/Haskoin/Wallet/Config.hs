{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The configuration of hw: the file config.json of the data directory,
-- written with the defaults when there is none, and the paths of that
-- directory.
module Haskoin.Wallet.Config where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import Data.Default
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin.Store.WebClient
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import qualified System.Directory as D

{- The data directory -}

-- | The directory of hw, ~/.hw, created when missing
hwDataDirectory :: IO FilePath
hwDataDirectory = do
  dir <- D.getAppUserDataDirectory "hw"
  D.createDirectoryIfMissing True dir
  return dir

-- | The configuration file of a data directory
configFile :: FilePath -> FilePath
configFile dir = dir </> "config.json"

-- | The database of a data directory, unless the configuration names
-- another file
databaseFileIn :: FilePath -> Config -> FilePath
databaseFileIn dir cfg = fromMaybe (dir </> "accounts.sqlite") (configDatabaseFile cfg)

-- | The database hw works on
databaseFile :: Config -> IO FilePath
databaseFile cfg = (`databaseFileIn` cfg) <$> hwDataDirectory

{- The configuration -}

-- | The settings of hw. Each one has a default, which a new configuration
-- file is written with and which a key missing from the file keeps.
data Config = Config
  { -- | The web address of the haskoin-store that hw asks about the chain
    configHost :: Text,
    -- | The gap of BIP44: how many unused addresses hw generates past the
    -- last used one of a chain, and watches for payments on a sync
    configGap :: Natural,
    -- | How many addresses discoveracc asks about at a time: the scan of a
    -- chain stops at the first batch of them without a used address
    configRecoveryGap :: Natural,
    -- | How many addresses one request for balances asks about
    configAddrBatch :: Natural,
    -- | How many addresses one request for coins asks about
    configCoinBatch :: Natural,
    -- | How many transactions one request fetches, in full or raw
    configTxFullBatch :: Natural,
    -- | How many requests the sync sends to the store at a time. The
    -- public store at api.haskoin.com refuses a second request while one
    -- is in flight (503 Service Temporarily Unavailable), so the default
    -- is one; a store of your own may allow more.
    configParallelRequests :: Natural,
    -- | The database to work on, instead of accounts.sqlite in ~/.hw
    configDatabaseFile :: Maybe FilePath
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON =
    withObject "config" $ \o -> do
      -- A key missing from the file, or set to null, takes its default: a
      -- file written by an earlier hw, or trimmed by hand, still reads.
      -- A key hw no longer has, like the tx-batch of hw 0.9, is ignored.
      let key name field = o .:? name .!= field (def :: Config)
          count name field = key name field >>= atLeastOne name
      Config
        <$> key "host" configHost
        <*> count "gap" configGap
        <*> count "recovery-gap" configRecoveryGap
        <*> count "addr-batch" configAddrBatch
        <*> count "coin-batch" configCoinBatch
        <*> count "tx-full-batch" configTxFullBatch
        <*> count "parallel-requests" configParallelRequests
        <*> o .:? "database-file"

-- | A count of the configuration cannot be zero: a gap of zero would refuse
-- every new address, and a batch of zero would ask the store forever
atLeastOne :: Key -> Natural -> Parser Natural
atLeastOne name 0 = fail $ "The key " <> show name <> " must be at least 1"
atLeastOne _ n = return n

instance ToJSON Config where
  toJSON cfg =
    object
      [ "host" .= configHost cfg,
        "gap" .= configGap cfg,
        "recovery-gap" .= configRecoveryGap cfg,
        "addr-batch" .= configAddrBatch cfg,
        "coin-batch" .= configCoinBatch cfg,
        "tx-full-batch" .= configTxFullBatch cfg,
        "parallel-requests" .= configParallelRequests cfg,
        "database-file" .= configDatabaseFile cfg
      ]

instance Default Config where
  def =
    Config
      { configHost = cs (def :: ApiConfig).host,
        configGap = 20,
        configRecoveryGap = 40,
        configAddrBatch = 100,
        configCoinBatch = 100,
        configTxFullBatch = 100,
        configParallelRequests = 1,
        configDatabaseFile = Nothing
      }

{- Reading the file -}

-- | The configuration of a data directory. A directory without a
-- configuration file gets one with the defaults first.
readConfig :: FilePath -> ExceptT String IO Config
readConfig dir = do
  let file = configFile dir
  exists <- liftIO $ D.doesFileExist file
  unless exists $ liftIO $ writeJsonFile file $ toJSON (def :: Config)
  cfgE <- liftIO $ readJsonFile file
  liftEither $ first (\reason -> "The configuration file " <> file <> " could not be read: " <> reason) cfgE

-- | The configuration of hw, from ~/.hw
initConfig :: IO (Either String Config)
initConfig = runExceptT . readConfig =<< hwDataDirectory
