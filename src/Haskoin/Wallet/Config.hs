{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The configuration of hw: the file config.json of the data directory,
-- written with the defaults when there is none, and the paths of that
-- directory. Four of its keys say where a command works when its options
-- do not: the wallet, the portfolio, the network and the type of account
-- ('configDefaultWallet' and the three after it). setdefault and
-- deletewallet change them in the file ('setConfigKeys').
module Haskoin.Wallet.Config where

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
import qualified Data.Text as Text
import Haskoin (Network (..), allNets, btc, netByName)
import Haskoin.Store.WebClient
import Haskoin.Wallet.Amounts (AmountUnit (..), readUnit, unitName)
import Haskoin.Wallet.Cli.View (View (..), readView, viewName, viewNames)
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys (AccountKind (..), indexFits, kindName, kindNamed, kindNames)
import Numeric.Natural (Natural)
import qualified System.Directory as D
import System.FilePath ((</>))

{- The data directory -}

-- | The directory of hw, ~/.hw, created when missing so that its owner
-- alone can enter it ('privateDirectory'): the database it holds is not
-- for the other users of the computer
hwDataDirectory :: IO FilePath
hwDataDirectory = do
  dir <- D.getAppUserDataDirectory "hw"
  privateDirectory dir
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
    -- last used one of a chain, and looks at for payments on a sync
    configGap :: Natural,
    -- | How many requests the sync sends to the store at a time. The
    -- public store at api.haskoin.com refuses a second request while one
    -- is in flight (503 Service Temporarily Unavailable), so the default
    -- is one; a store of your own may allow more.
    configParallelRequests :: Natural,
    -- | The database to work on, instead of accounts.sqlite in ~/.hw
    configDatabaseFile :: Maybe FilePath,
    -- | The wallet a command works on when --wallet is not given, by its
    -- index: wallet0 unless the user chose another. A user with one
    -- mnemonic has one wallet and never sets it. deletewallet writes it
    -- when the default wallet is the one deleted, so that a wallet made
    -- later, which takes the lowest free index, does not become the
    -- default by itself.
    configDefaultWallet :: Natural,
    -- | The portfolio of that wallet a command works on when --portfolio
    -- is not given, by its index: portfolio0 unless the user chose
    -- another. Most users only ever need portfolio 0.
    configDefaultPortfolio :: Natural,
    -- | The network of a command that needs one when --network is not
    -- given: btc unless the user chose another. A sweep that pays no
    -- portfolio does not take it: it needs its --network. sync and
    -- recover do not take it either: they go over every network.
    configDefaultNetwork :: Network,
    -- | The type of account a command takes when --type is not given:
    -- bip44 unless the user chose another. receive gives out an address
    -- of that type, and a payment that spends coins of both types sends
    -- its change to an address of that type. The networks of Bitcoin Cash
    -- have BIP44 accounts alone, and take bip44 whatever this says
    -- ('Haskoin.Wallet.Database.Account.defaultKindOn').
    configDefaultType :: AccountKind,
    -- | The unit of the amounts when --unit is not given, for a user who
    -- counts in satoshis or in bits. It is the default of --unit, so it
    -- sets the unit of the amounts shown and of those typed without a
    -- unit. Splitting the two would make "1000" typed mean another amount
    -- than "1000" shown. JSON answers count satoshis whatever the unit, so
    -- a script that reads them is not affected.
    configUnit :: AmountUnit,
    -- | The view of a listing when no --compact, --verbose or --tabular
    -- is given, for a user who always wants the same one. The option
    -- overrides it for one listing, and --compact brings back the compact
    -- view. It sets the screen alone. --json and --csv answer the same
    -- whatever it says, so a script that reads them is not affected. The
    -- commands that answer with one element keep their own view.
    configListingView :: View,
    -- | The command a listing goes through on a terminal, instead of less
    -- or the PAGER of the environment. The empty string is no pager.
    configPager :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON =
    withObject "config" $ \o -> do
      -- A key missing from the file, or set to null, takes its default: a
      -- file written by an earlier hw, or trimmed by hand, still reads.
      -- A key hw no longer has, like the batch sizes of hw 0.9 (addr-batch,
      -- tx-batch, coin-batch, tx-full-batch) or its recovery-gap, is ignored.
      -- A key set to null in the file also takes its default.
      let key name field = o .:? name .!= field (def :: Config)
          count name field = key name field >>= atLeastOne name
      Config
        <$> key "host" configHost
        <*> count "gap" configGap
        <*> count "parallel-requests" configParallelRequests
        <*> o .:? "database-file"
        <*> (key "default-wallet" configDefaultWallet >>= anIndex "default-wallet")
        <*> (key "default-portfolio" configDefaultPortfolio >>= anIndex "default-portfolio")
        <*> (networkNamed =<< key "default-network" ((.name) . configDefaultNetwork))
        <*> (typeNamed =<< key "default-type" (kindName . configDefaultType))
        -- Missing or null, the key reads as the name of the default unit
        <*> (unitNamed =<< key "unit" (unitName . configUnit))
        <*> (viewNamed =<< key "listing-view" (viewName . configListingView))
        <*> o .:? "pager"

-- | The network of a name in the configuration file
networkNamed :: String -> Parser Network
networkNamed text = maybe (fail refused) return (netByName text)
  where
    refused = "The key \"default-network\" must be a network: " <> intercalate ", " ((.name) <$> allNets)

-- | The type of account of a name in the configuration file
typeNamed :: Text -> Parser AccountKind
typeNamed text = maybe (fail refused) return (kindNamed text)
  where
    refused = "The key \"default-type\" must be a type of account: " <> intercalate ", " (cs <$> kindNames)

-- | The unit of a name in the configuration file, any name --unit takes
unitNamed :: Text -> Parser AmountUnit
unitNamed text = maybe (fail refused) return (readUnit text)
  where
    refused = "The key \"unit\" must be a unit: btc, bit or sat"

-- | The view of a name in the configuration file, as the option that asks
-- for it writes it
viewNamed :: Text -> Parser View
viewNamed text = maybe (fail refused) return (readView text)
  where
    refused = "The key \"listing-view\" must be a view: " <> intercalate ", " (cs <$> viewNames)

-- | The index of a wallet or of a portfolio must be below 2^31
-- ('indexFits'). A larger one names no wallet and no portfolio: as a
-- number of the database, it would wrap around to the index of another.
anIndex :: Key -> Natural -> Parser Natural
anIndex name index
  | indexFits index = return index
  | otherwise = fail $ "The key " <> show name <> " must be an index below 2147483648, which is 2^31"

-- | A count of the configuration cannot be zero: a gap of zero would refuse
-- every new address, and zero requests at a time would send none
atLeastOne :: Key -> Natural -> Parser Natural
atLeastOne name 0 = fail $ "The key " <> show name <> " must be at least 1"
atLeastOne _ n = return n

instance ToJSON Config where
  toJSON cfg =
    object
      [ "host" .= configHost cfg,
        "gap" .= configGap cfg,
        "parallel-requests" .= configParallelRequests cfg,
        "database-file" .= configDatabaseFile cfg,
        "default-wallet" .= configDefaultWallet cfg,
        "default-portfolio" .= configDefaultPortfolio cfg,
        "default-network" .= (configDefaultNetwork cfg).name,
        "default-type" .= kindName (configDefaultType cfg),
        "unit" .= unitName (configUnit cfg),
        "listing-view" .= viewName (configListingView cfg),
        "pager" .= configPager cfg
      ]

instance Default Config where
  def =
    Config
      { configHost = cs (def :: ApiConfig).host,
        configGap = 20,
        configParallelRequests = 1,
        configDatabaseFile = Nothing,
        configDefaultWallet = 0,
        configDefaultPortfolio = 0,
        configDefaultNetwork = btc,
        configDefaultType = Bip44,
        configUnit = UnitBitcoin,
        configListingView = Compact,
        configPager = Nothing
      }

-- | The network of a command that needs one: the one --network names, or
-- the default network of the configuration
commandNetwork :: Config -> Maybe Network -> Network
commandNetwork cfg = fromMaybe (configDefaultNetwork cfg)

-- | The unit of the amounts of a command: the one --unit names, or the
-- unit of the configuration
commandUnit :: Config -> Maybe AmountUnit -> AmountUnit
commandUnit cfg given = fromMaybe (configUnit cfg) given

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

-- | The configuration of a data directory as it stands, for a reader that
-- must change nothing. It is the defaults when there is no file, and
-- Nothing when the file cannot be read. Unlike 'readConfig', it writes no
-- file and leaves out the warnings.
configAsItIs :: FilePath -> IO (Maybe Config)
configAsItIs dir = do
  let file = configFile dir
  exists <- D.doesFileExist file
  if exists
    then either (const Nothing) Just . (>>= parseEither parseJSON) <$> readJsonFile file
    else return (Just def)

{- Changing the file -}

-- | Set keys of the configuration file of a data directory, and leave
-- every other key of the file as it is, those hw does not know too. The
-- file is read as the JSON object it is, not as a 'Config', so that
-- nothing but the keys given changes. The values must be ones the keys
-- take: the caller writes what 'Config' reads back.
--
-- setdefault writes the defaults the user chooses this way, and
-- deletewallet the default wallet it picks. The user then never has to
-- edit the file by hand.
setConfigKeys :: FilePath -> [(Text, Value)] -> ExceptT String IO ()
setConfigKeys dir changes = do
  let file = configFile dir
  value <- liftEither =<< liftIO (readJsonFile file)
  case value of
    Object o -> do
      let changed = foldr (\(name, new) -> KeyMap.insert (Key.fromText name) new) o changes
      liftEither =<< liftIO (replaceJsonFile file (Object changed))
    _ -> liftEither (Left ("The configuration file " <> file <> " does not hold a JSON object"))

-- | The names of the four keys that say where a command works
defaultWalletKey, defaultPortfolioKey, defaultNetworkKey, defaultTypeKey :: Text
defaultWalletKey = Text.pack "default-wallet"
defaultPortfolioKey = Text.pack "default-portfolio"
defaultNetworkKey = Text.pack "default-network"
defaultTypeKey = Text.pack "default-type"
