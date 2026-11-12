{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The configuration of hw: the file config.json of the data directory,
-- written with the defaults when there is none, and the paths of that
-- directory. Two of its keys say where in a portfolio a command works when
-- its options do not: the network and the type of account
-- ('configDefaultNetwork' and 'configDefaultType'). Two more say how the
-- answers look: the unit of the amounts and the view of the listings
-- ('configUnit' and 'configListingView'). One says which networks hw
-- shows ('configShownNetworks'). setdefault changes these five in the
-- file ('setConfigKeys').
--
-- A portfolio has accounts on eight networks, and most users work on two
-- of them. So the file lists the networks hw shows, btc and bch in a new
-- file, and the others are hidden. A hidden network is not disabled. sync
-- and recover still work on it, so that showing it later needs no catching
-- up. It is left out of every answer, and a command cannot name it
-- ('isShown', 'hiddenNetworkRefusal'). The key lists the networks to
-- show, not those to hide. A network a later hw adds then stays hidden
-- until the user asks for it, and the file says what the user sees.
--
-- The portfolio a command works in is no key of the file: it is the
-- active portfolio, which the database keeps and switch changes
-- ("Haskoin.Wallet.Database.Portfolio"). The keys default-wallet and
-- default-portfolio of earlier builds of hw 0.10 are gone. A file that
-- still has them reads, and the warning of the keys hw does not know
-- names them ('configWarnings').
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
import Data.List (intercalate, nub, sort)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network (..), allNets, bch, btc, netByName)
import Haskoin.Store.WebClient
import Haskoin.Wallet.Amounts (AmountUnit (..), readUnit, unitName)
import Haskoin.Wallet.Cli.View (View (..), readView, viewName, viewNames)
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys (AccountKind (..), kindName, kindNamed, kindNames, portfolioNetworks)
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
  { -- | The web address of the haskoin-store that hw asks about the chain.
    -- It may end with a slash or not. The name of the network goes after
    -- it, with a slash between ('Haskoin.Wallet.Store.resourceUrl').
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
    -- | The network of a command that needs one when --network is not
    -- given: btc unless the user chose another. A sweep that pays no
    -- portfolio does not take it: it needs its --network. sync and
    -- recover do not take it either: they go over every network.
    configDefaultNetwork :: Network,
    -- | The networks hw shows: btc and bch unless the user chose others.
    -- The other networks of a portfolio are hidden. Every answer leaves
    -- them out, and a command that names one is refused, with the way to
    -- show it. sync and recover still work on them, so that the wallet is
    -- up to date on a network the day the user shows it. Only the status
    -- line of the two names a hidden network, since it says what the user
    -- is waiting for. The default network is always one of these: a file
    -- where it is not is refused ('checkDefaultShown'). They are kept in
    -- the order of the networks of a portfolio, each once
    -- ('inPortfolioOrder').
    configShownNetworks :: [Network],
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
      cfg <-
        Config
          <$> key "host" configHost
          <*> count "gap" configGap
          <*> count "parallel-requests" configParallelRequests
          <*> o .:? "database-file"
          <*> (networkNamed =<< key "default-network" ((.name) . configDefaultNetwork))
          <*> (shownNetworksNamed =<< key "shown-networks" (map (.name) . configShownNetworks))
          <*> (typeNamed =<< key "default-type" (kindName . configDefaultType))
          -- Missing or null, the key reads as the name of the default unit
          <*> (unitNamed =<< key "unit" (unitName . configUnit))
          <*> (viewNamed =<< key "listing-view" (viewName . configListingView))
          <*> o .:? "pager"
      either fail return (checkDefaultShown cfg)
      return cfg

-- | The network of a name in the configuration file
networkNamed :: String -> Parser Network
networkNamed text = maybe (fail refused) return (netByName text)
  where
    refused = "The key \"default-network\" must be a network: " <> intercalate ", " ((.name) <$> allNets)

-- | The networks the key shown-networks lists by their names, in the
-- order of the networks of a portfolio. A name hw does not know is
-- refused, rather than left aside: the user who mistyped a network would
-- wonder why it stays hidden. An empty list is refused too, since hw
-- would then show nothing.
shownNetworksNamed :: [String] -> Parser [Network]
shownNetworksNamed names = do
  networks <- mapM (\name -> maybe (fail (unknown name)) return (netByName name)) names
  if null networks then fail empty else return (inPortfolioOrder networks)
  where
    known = intercalate ", " ((.name) <$> portfolioNetworks)
    unknown name = "The key \"shown-networks\" lists " <> show name <> ", which is no network. The networks are: " <> known
    empty = "The key \"shown-networks\" must list at least one network: " <> known

-- | Refuse a configuration whose default network is hidden. A command
-- without --network works on the default network, so it would work on a
-- network hw shows nothing of. The refusal names both ways out.
checkDefaultShown :: Config -> Either String ()
checkDefaultShown cfg
  | isShown cfg (configDefaultNetwork cfg) = Right ()
  | otherwise =
      Left $
        "The key \"default-network\" names "
          <> name
          <> ", which the key \"shown-networks\" does not list. Add \""
          <> name
          <> "\" to \"shown-networks\", or set \"default-network\" to "
          <> intercalate " or " ((.name) <$> configShownNetworks cfg)
  where
    name = (configDefaultNetwork cfg).name

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
        "default-network" .= (configDefaultNetwork cfg).name,
        "shown-networks" .= ((.name) <$> configShownNetworks cfg),
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
        configDefaultNetwork = btc,
        configShownNetworks = [btc, bch],
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

{- The networks hw shows -}

-- | Whether hw shows the network: it is one of those the key
-- shown-networks lists
isShown :: Config -> Network -> Bool
isShown cfg net = net `elem` configShownNetworks cfg

-- | Those of the elements that are on a network hw shows, given the
-- network of each. Every answer goes through this, and so does every
-- search for what the user names: a hidden network then never comes up
-- as a candidate, and never has to be told apart with --network.
onShownNetworks :: Config -> (a -> Network) -> [a] -> [a]
onShownNetworks cfg networkOf = filter (isShown cfg . networkOf)

-- | The networks given, each once, in the order of the networks of a
-- portfolio: how the configuration keeps the networks it shows, and how
-- setdefault writes them, whatever the order they were typed in. A
-- network that is none of a portfolio's comes after them, rather than be
-- dropped without a word, should a name ever read as one.
inPortfolioOrder :: [Network] -> [Network]
inPortfolioOrder networks = ofPortfolio <> others
  where
    ofPortfolio = [net | net <- portfolioNetworks, net `elem` networks]
    others = nub [net | net <- networks, net `notElem` portfolioNetworks]

-- | The command that shows a hidden network, as a refusal gives it to the
-- user to run
showNetworkCommand :: Network -> String
showNetworkCommand net = "hw setdefault --show-network " <> net.name

-- | The refusal of a --network that names a hidden network. It says which
-- networks hw shows, and gives the command that shows this one too. hw
-- does not show the network for the one command instead. A user who hid
-- a network asked not to see it, and a typo such as btctest for btc would
-- otherwise work on a network the user does not watch.
hiddenNetworkRefusal :: Config -> Network -> String
hiddenNetworkRefusal cfg net =
  "The network "
    <> net.name
    <> " is hidden. hw shows "
    <> networksText (configShownNetworks cfg)
    <> ". To show "
    <> net.name
    <> " too, run: "
    <> showNetworkCommand net

-- | The refusal of what the user named, an address or a transaction for
-- instance, that the wallet has on hidden networks alone. Saying that the
-- wallet does not have it would be untrue, and showing it would show a
-- hidden network. So the refusal names the networks, and gives the command
-- that shows the first of them. The first text says what was named, as in
-- "The address tb1q...".
onHiddenNetworks :: String -> [Network] -> String
onHiddenNetworks what networks = case inPortfolioOrder networks of
  [] -> what <> " is on a hidden network"
  [only] -> what <> " is on " <> only.name <> ", which is hidden. To show " <> only.name <> ", run: " <> showNetworkCommand only
  several@(lowest : _) ->
    what
      <> " is on "
      <> networksText several
      <> ", which are hidden. To show one of them, run for instance: "
      <> showNetworkCommand lowest

-- | The networks of a text, as a sentence names them: "btc", "btc and
-- bch", "btc, bch and btctest"
networksText :: [Network] -> String
networksText networks = case reverse ((.name) <$> networks) of
  [] -> ""
  [only] -> only
  final : others -> intercalate ", " (reverse others) <> " and " <> final

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
-- earlier version, such as default-wallet and default-portfolio, which the
-- active portfolio of the database replaced, or a typo that would
-- otherwise leave a setting at its default without a word
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
-- setdefault writes the defaults the user chooses this way. The user then
-- never has to edit the file by hand.
setConfigKeys :: FilePath -> [(Text, Value)] -> ExceptT String IO ()
setConfigKeys dir changes = do
  let file = configFile dir
  value <- liftEither =<< liftIO (readJsonFile file)
  case value of
    Object o -> do
      let changed = foldr (\(name, new) -> KeyMap.insert (Key.fromText name) new) o changes
      liftEither =<< liftIO (replaceJsonFile file (Object changed))
    _ -> liftEither (Left ("The configuration file " <> file <> " does not hold a JSON object"))

-- | The names of the two keys that say where in a portfolio a command
-- works
defaultNetworkKey, defaultTypeKey :: Text
defaultNetworkKey = Text.pack "default-network"
defaultTypeKey = Text.pack "default-type"

-- | The names of the two keys that say how the answers look: the unit of
-- the amounts and the view of the listings
unitKey, listingViewKey :: Text
unitKey = Text.pack "unit"
listingViewKey = Text.pack "listing-view"

-- | The name of the key that lists the networks hw shows
shownNetworksKey :: Text
shownNetworksKey = Text.pack "shown-networks"
