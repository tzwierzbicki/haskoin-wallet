{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The configuration of hw: the file config.json of the data directory,
-- written with the defaults when there is none, and the paths of that
-- directory.
module Haskoin.Wallet.Config where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.Default
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin (Network (..), allNets, btc, netByName)
import Haskoin.Store.WebClient
import Haskoin.Wallet.FileIO
import Numeric.Natural (Natural)
import qualified System.Directory as D
import System.FilePath ((</>))

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
-- file is written with and which a key missing from the file keeps, but
-- the default network: a file without it has none.
data Config = Config
  { -- | The web address of the haskoin-store that hw asks about the chain
    configHost :: Text,
    -- | The gap of BIP44: how many unused addresses hw generates past the
    -- last used one of a chain, and looks at for payments on a sync
    configGap :: Natural,
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
    configDatabaseFile :: Maybe FilePath,
    -- | The network of a command that needs one when --network is not
    -- given, for a user of one network. Without it, --network must be.
    -- A sweep that pays no account of the wallet does not take it: it
    -- needs its --network.
    configDefaultNetwork :: Maybe Network
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON =
    withObject "config" $ \o -> do
      -- A key missing from the file, or set to null, takes its default: a
      -- file written by an earlier hw, or trimmed by hand, still reads.
      -- A key hw no longer has, like the tx-batch of hw 0.9 or the
      -- recovery-gap of hw 0.10, is ignored.
      let key name field = o .:? name .!= field (def :: Config)
          count name field = key name field >>= atLeastOne name
      Config
        <$> key "host" configHost
        <*> count "gap" configGap
        <*> count "addr-batch" configAddrBatch
        <*> count "coin-batch" configCoinBatch
        <*> count "tx-full-batch" configTxFullBatch
        <*> count "parallel-requests" configParallelRequests
        <*> o .:? "database-file"
        <*> (traverse networkNamed =<< o .:? "default-network")

-- | The network of a name in the configuration file
networkNamed :: String -> Parser Network
networkNamed text = maybe (fail refused) return (netByName text)
  where
    refused = "The key \"default-network\" must be a network: " <> intercalate ", " ((.name) <$> allNets)

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
        "addr-batch" .= configAddrBatch cfg,
        "coin-batch" .= configCoinBatch cfg,
        "tx-full-batch" .= configTxFullBatch cfg,
        "parallel-requests" .= configParallelRequests cfg,
        "database-file" .= configDatabaseFile cfg,
        "default-network" .= ((.name) <$> configDefaultNetwork cfg)
      ]

instance Default Config where
  def =
    Config
      { configHost = cs (def :: ApiConfig).host,
        configGap = 20,
        configAddrBatch = 100,
        configCoinBatch = 100,
        configTxFullBatch = 100,
        configParallelRequests = 1,
        configDatabaseFile = Nothing,
        configDefaultNetwork = Just btc
      }

-- | The network of a command that needs one: the one --network names, or
-- the default network of the configuration, when it has one
commandNetwork :: Config -> Maybe Network -> Maybe Network
commandNetwork cfg given = given <|> configDefaultNetwork cfg

-- | The refusal of a command that needs a network and has none
noNetwork :: String
noNetwork = "Give the --network, or a default-network in the configuration file"

{- Reading the file -}

-- | The configuration of a data directory, with the warnings to show the
-- user: the keys of the file hw does not know, which it leaves aside. A
-- directory without a configuration file gets one with the defaults first.
readConfig :: FilePath -> ExceptT String IO (Config, [String])
readConfig dir = do
  let file = configFile dir
      refused reason = "The configuration file " <> file <> " could not be read: " <> reason
  exists <- liftIO $ D.doesFileExist file
  unless exists $ liftEither =<< liftIO (writeJsonFile file $ toJSON (def :: Config))
  value <- liftEither =<< liftIO (readJsonFile file)
  cfg <- liftEither $ first refused $ parseEither parseJSON value
  return (cfg, configWarnings file value)

-- | A warning naming the keys of the file hw does not know: a key of an
-- earlier version, or a typo that would otherwise leave a setting at its
-- default without a word
configWarnings :: FilePath -> Value -> [String]
configWarnings file (Object o)
  | null unknown = []
  | otherwise = ["The configuration file " <> file <> " has keys hw does not know, left aside: " <> intercalate ", " unknown]
  where
    unknown = sort [Key.toString key | key <- KeyMap.keys o, key `notElem` knownKeys]
configWarnings _ _ = []

-- | The keys of the configuration file: those hw writes
knownKeys :: [Key]
knownKeys =
  case toJSON (def :: Config) of
    Object o -> KeyMap.keys o
    _ -> []

-- | The configuration of hw, from ~/.hw, with its warnings
initConfig :: IO (Either String (Config, [String]))
initConfig = runExceptT . readConfig =<< hwDataDirectory
