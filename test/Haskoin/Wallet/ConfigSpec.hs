{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The configuration file of hw: what it holds, how a file written by an
-- older hw or trimmed by hand reads, the four keys that say where a
-- command works, the networks hw shows, the data directory around the
-- file, and the keys a command changes in it
module Haskoin.Wallet.ConfigSpec (spec) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (Value, eitherDecode, toJSON, (.=))
import Data.Default (def)
import Data.List (isPrefixOf)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin (bch, bchTest, btc, btcRegTest, btcTest, btcTest4)
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.Config
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils (configWith, inScratchDirectory)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryConfig)
import System.Directory (createDirectory)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  fileSpec
  keySpec
  defaultsSpec
  shownNetworksSpec
  directorySpec
  changeSpec
  -- A configuration reads back as it was written
  testIdentity def {jsonTests = [JsonBox arbitraryConfig]}

fileSpec :: Spec
fileSpec =
  describe "The configuration file" $ do
    it "holds the default configuration" $
      toJSON (def :: Config) `shouldBe` decodeValue defaultFile

    it "reads the default configuration back" $
      decodeConfig defaultFile `shouldBe` Right def

    it "reads a file of hw 0.9: its batch sizes and recovery-gap mean nothing now, the parallel requests take their default" $
      decodeConfig hw09File `shouldBe` Right def

    it "reads the number of parallel requests" $
      configParallelRequests <$> decodeConfig twoRequestsFile `shouldBe` Right 2

keySpec :: Spec
keySpec =
  describe "A key of the configuration file" $ do
    it "takes its default when missing, so an empty file is the default configuration" $
      decodeConfig "{}" `shouldBe` Right def

    it "sets its value alone, the others keeping their defaults" $
      decodeConfig [r|{"gap": 30}|] `shouldBe` Right def {configGap = 30}

    it "takes its default when null" $
      decodeConfig [r|{"gap": null, "database-file": null, "unit": null, "listing-view": null}|] `shouldBe` Right def

    -- For a user who counts in satoshis: the amounts are shown in them, and
    -- read in them when typed without a unit
    it "sets the unit of the amounts when --unit is not given" $
      configUnit <$> decodeConfig [r|{"unit": "sat"}|] `shouldBe` Right UnitSatoshi

    it "names a unit as --unit does, by its plural too, in any case" $
      configUnit <$> decodeConfig [r|{"unit": "Bits"}|] `shouldBe` Right UnitBit

    it "cannot be a unit hw does not know" $
      decodeConfig [r|{"unit": "mbtc"}|]
        `shouldBe` Left "Error in $: The key \"unit\" must be a unit: btc, bit or sat"

    -- For a user who always wants a table: the listings show one without
    -- --tabular
    it "sets the view of the listings when no option asks for one" $
      configListingView <$> decodeConfig [r|{"listing-view": "tabular"}|] `shouldBe` Right Table

    it "names each view as the option that asks for it does" $ do
      configListingView <$> decodeConfig [r|{"listing-view": "compact"}|] `shouldBe` Right Compact
      configListingView <$> decodeConfig [r|{"listing-view": "verbose"}|] `shouldBe` Right Verbose

    -- "table" is what the help calls the view, but not its name
    it "cannot be a view hw does not know, naming the views it knows" $
      decodeConfig [r|{"listing-view": "table"}|]
        `shouldBe` Left "Error in $: The key \"listing-view\" must be a view: compact, verbose, tabular"

    -- hw 0.10 before portfolios had this key. The change of a payment now
    -- goes where --change-back ADDRESS says, or to a new change address.
    it "means nothing any more when it is change-back, and the file still reads" $
      decodeConfig [r|{"gap": 30, "change-back": true}|] `shouldBe` Right def {configGap = 30}

    it "names the database to work on" $
      configDatabaseFile <$> decodeConfig [r|{"database-file": "/backups/portfolios.sqlite"}|]
        `shouldBe` Right (Just "/backups/portfolios.sqlite")

    it "cannot be a gap of zero, which would refuse every new address" $
      decodeConfig [r|{"gap": 0}|]
        `shouldBe` Left "Error in $: The key \"gap\" must be at least 1"

    it "cannot be zero requests at a time, which would send none" $
      decodeConfig [r|{"parallel-requests": 0}|]
        `shouldBe` Left "Error in $: The key \"parallel-requests\" must be at least 1"

    it "keeps ten copies of each kind when the file has no key autobackup-copies" $
      configAutoBackupCopies <$> decodeConfig "{}" `shouldBe` Right 10

    it "sets how many copies hw keeps of each kind" $
      configAutoBackupCopies <$> decodeConfig [r|{"autobackup-copies": 25}|] `shouldBe` Right 25

    -- The keys autobackup-database and autobackup-labels turn the copies off
    it "cannot be zero copies kept" $
      decodeConfig [r|{"autobackup-copies": 0}|]
        `shouldBe` Left "Error in $: The key \"autobackup-copies\" must be at least 1"

    it "must be a number when its default is one" $
      decodeConfig [r|{"gap": "twenty"}|]
        `shouldBe` Left "Error in $.gap: parsing Natural failed, expected Number, but encountered String"

-- | The two keys that say where in the active portfolio a command works
-- when its options do not: the network and the type of account
defaultsSpec :: Spec
defaultsSpec =
  describe "Where a command works without an option" $ do
    it "is on btc in a file without the key default-network" $
      configDefaultNetwork <$> decodeConfig "{}" `shouldBe` Right btc

    it "is in the bip44 account in a file without the key default-type" $
      configDefaultType <$> decodeConfig "{}" `shouldBe` Right Bip44

    it "is on the network the key default-network names" $
      configDefaultNetwork <$> decodeConfig [r|{"default-network": "bch"}|] `shouldBe` Right bch

    it "is in the account of the type the key default-type names" $
      configDefaultType <$> decodeConfig [r|{"default-type": "bip84"}|] `shouldBe` Right Bip84

    it "takes each key alone, the other keeping its default" $
      decodeConfig [r|{"default-type": "bip84"}|] `shouldBe` Right def {configDefaultType = Bip84}

    -- A file trimmed by hand, or written by a tool that writes null for
    -- what it does not set
    it "is btc and bip44 when the two keys are null" $
      decodeConfig [r|{"default-network": null, "default-type": null}|]
        `shouldBe` Right def

    it "cannot be on a network hw does not know" $
      decodeConfig [r|{"default-network": "doge"}|]
        `shouldSatisfy` either ("Error in $: The key \"default-network\" must be a network: " `isPrefixOf`) (const False)

    -- hw looks for the funds of a bip49 account when it recovers a seed,
    -- and holds no such account
    it "cannot be in an account of a type hw does not hold, naming the types it holds" $
      decodeConfig [r|{"default-type": "bip49"}|]
        `shouldBe` Left "Error in $: The key \"default-type\" must be a type of account: bip44, bip84"

    it "is on the network --network names, whatever the default network" $
      commandNetwork (configWith ["default-network" .= ("bch" :: Text)]) (Just btcTest) `shouldBe` btcTest

    it "is on the default network without --network" $
      commandNetwork (configWith ["default-network" .= ("bch" :: Text)]) Nothing `shouldBe` bch

-- | The key that lists the networks hw shows. The others are hidden: left
-- out of every answer, while sync and recover still work on them.
shownNetworksSpec :: Spec
shownNetworksSpec =
  describe "The networks hw shows" $ do
    it "are btc and bch in a file without the key shown-networks" $
      configShownNetworks <$> decodeConfig "{}" `shouldBe` Right [btc, bch]

    it "are btc and bch when the key is null" $
      configShownNetworks <$> decodeConfig [r|{"shown-networks": null}|] `shouldBe` Right [btc, bch]

    it "are those the key lists" $
      configShownNetworks <$> decodeConfig [r|{"shown-networks": ["btc", "btctest4"]}|] `shouldBe` Right [btc, btcTest4]

    -- btc comes before btctest4 in a portfolio, and is listed twice
    it "are kept in the order of the networks of a portfolio, each once, whatever the order of the file" $
      configShownNetworks <$> decodeConfig [r|{"shown-networks": ["btctest4", "btc", "btc"]}|] `shouldBe` Right [btc, btcTest4]

    it "cannot be none: hw would show nothing" $
      decodeConfig [r|{"shown-networks": []}|]
        `shouldBe` Left "Error in $: The key \"shown-networks\" must list at least one network: btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4, bchreg"

    -- A mistyped network would otherwise stay hidden without a word
    it "cannot include a network hw does not know, which is named with the networks it knows" $
      decodeConfig [r|{"shown-networks": ["btc", "doge"]}|]
        `shouldBe` Left "Error in $: The key \"shown-networks\" lists \"doge\", which is no network. The networks are: btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4, bchreg"

    -- A command without --network works on the default network
    it "must include the default network: a file that hides it is refused, with both ways out" $
      decodeConfig [r|{"default-network": "btctest4"}|]
        `shouldBe` Left "Error in $: The key \"default-network\" names btctest4, which the key \"shown-networks\" does not list. Add \"btctest4\" to \"shown-networks\", or set \"default-network\" to btc or bch"

    it "may leave out btc, when the default network is another one they include" $
      decodeConfig [r|{"default-network": "bch", "shown-networks": ["bch"]}|]
        `shouldBe` Right def {configDefaultNetwork = bch, configShownNetworks = [bch]}

    it "include a network the key lists, and no other" $ do
      let cfg = configWith ["shown-networks" .= (["btc", "btctest4"] :: [Text])]
      (isShown cfg <$> [btc, bch, btcTest, btcTest4]) `shouldBe` [True, False, False, True]

    -- Each element is given with its network: a number here
    it "keep what is on one of them, in its order, and leave out the rest" $ do
      let cfg = configWith ["shown-networks" .= (["btc", "btctest4"] :: [Text])]
      onShownNetworks cfg snd [(1 :: Int, btcTest4), (2, bch), (3, btc), (4, btcTest)] `shouldBe` [(1, btcTest4), (3, btc)]

    it "are written in the order of the networks of a portfolio, each once" $
      inPortfolioOrder [btcRegTest, bchTest, btc, btcTest4, btc] `shouldBe` [btc, btcTest4, btcRegTest, bchTest]

    it "do not include a hidden network named with --network, which is refused with the networks shown and the command that shows it" $
      hiddenNetworkRefusal def btcTest4
        `shouldBe` "The network btctest4 is hidden. hw shows btc and bch. To show btctest4 too, run: hw setdefault --show-network btctest4"

    it "do not include what the wallet has on one hidden network, which is refused with the command that shows the network" $
      onHiddenNetworks "The coin a1075db5:1" [btcTest4]
        `shouldBe` "The coin a1075db5:1 is on btctest4, which is hidden. To show btctest4, run: hw setdefault --show-network btctest4"

    -- The test networks share their keys: one address is on several. The
    -- first in the order of a portfolio is the one offered.
    it "do not include what the wallet has on several hidden networks, which are all named" $
      onHiddenNetworks "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih" [btcTest4, btcTest]
        `shouldBe` "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4, which are hidden. To show one of them, run for instance: hw setdefault --show-network btctest"

    it "are named as a sentence names them" $ do
      networksText [btc] `shouldBe` "btc"
      networksText [btc, bch] `shouldBe` "btc and bch"
      networksText [btc, bch, btcTest] `shouldBe` "btc, bch and btctest"

directorySpec :: Spec
directorySpec =
  describe "The data directory" $ do
    it "holds the database, unless the configuration names another file" $ do
      databaseFileIn "/home/someone/.hw" def `shouldBe` "/home/someone/.hw/portfolios.sqlite"
      databaseFileIn "/home/someone/.hw" def {configDatabaseFile = Just "/backups/portfolios.sqlite"}
        `shouldBe` "/backups/portfolios.sqlite"

    it "gets the default configuration file when it has none, without a warning" $
      inScratchDirectory $ \dir -> do
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (def, [])
        written <- readFile (configFile dir)
        decodeValue written `shouldBe` decodeValue defaultFile

    it "keeps the configuration file it has, and reads it" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) twoRequestsFile
        cfg <- runExceptT $ readConfig dir
        configParallelRequests . fst <$> cfg `shouldBe` Right 2
        readFile (configFile dir) `shouldReturn` twoRequestsFile

    it "refuses a configuration file that does not read, naming it and the reason" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) [r|{"gap": 0}|]
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Left ("The configuration file " <> configFile dir <> " could not be read: Error in $: The key \"gap\" must be at least 1")

    -- A directory stands where the file goes: hw crashed there before, with
    -- the exception of the system
    it "says why it could not write the configuration file it lacks" $
      inScratchDirectory $ \dir -> do
        createDirectory (configFile dir)
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Left ("The file " <> configFile dir <> " could not be written: is a directory")

    it "warns about the keys it does not know, naming the file: those of an earlier hw" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) hw09File
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (def, ["The configuration file " <> configFile dir <> " has keys hw does not know, left aside: addr-batch, coin-batch, recovery-gap, tx-batch, tx-full-batch"])

    -- The builds of hw 0.10 before the active portfolio wrote the two keys,
    -- with the defaults of the wallet and of the portfolio. The database
    -- keeps the active portfolio now, and the keys mean nothing.
    it "warns about the keys default-wallet and default-portfolio of an earlier build, and reads the file" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) [r|{"gap": 30, "default-wallet": 1, "default-portfolio": 2}|]
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (def {configGap = 30}, ["The configuration file " <> configFile dir <> " has keys hw does not know, left aside: default-portfolio, default-wallet"])

    -- The file hw 0.10 wrote before portfolios had the key change-back
    it "warns about the key change-back of an earlier hw, which it no longer knows, and reads the file" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) [r|{"gap": 30, "change-back": true}|]
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (def {configGap = 30}, ["The configuration file " <> configFile dir <> " has keys hw does not know, left aside: change-back"])

    it "warns about a mistyped key, whose setting would otherwise keep its default without a word" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) [r|{"gaps": 30}|]
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (def, ["The configuration file " <> configFile dir <> " has keys hw does not know, left aside: gaps"])

-- | The keys a command changes in the file: setdefault writes the defaults
-- the user chooses
changeSpec :: Spec
changeSpec =
  describe "Setting keys of the configuration file" $ do
    it "names the two keys that say where in a portfolio a command works as the file writes them" $
      [defaultNetworkKey, defaultTypeKey] `shouldBe` ["default-network", "default-type"]

    it "names the keys of the unit and of the view of the listings as the file writes them" $
      [unitKey, listingViewKey] `shouldBe` ["unit", "listing-view"]

    it "names the key of the networks hw shows as the file writes it" $
      shownNetworksKey `shouldBe` "shown-networks"

    it "changes the keys given, which the configuration then reads" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) defaultFile
        changed <- runExceptT $ setConfigKeys dir [("default-type", toJSON ("bip84" :: Text)), ("default-network", toJSON ("bch" :: Text))]
        changed `shouldBe` Right ()
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (def {configDefaultType = Bip84, configDefaultNetwork = bch}, [])

    -- The file sets a gap of 30, and has a key of an earlier hw and a
    -- mistyped one
    it "leaves every other key of the file as it is, those hw does not know too" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) [r|{"gap": 30, "default-type": "bip84", "change-back": true, "gaps": 5}|]
        changed <- runExceptT $ setConfigKeys dir [("default-network", toJSON ("bch" :: Text))]
        changed `shouldBe` Right ()
        written <- readFile (configFile dir)
        decodeValue written
          `shouldBe` decodeValue [r|{"gap": 30, "default-type": "bip84", "change-back": true, "gaps": 5, "default-network": "bch"}|]

    it "adds a key the file did not have, and no other" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) "{}"
        changed <- runExceptT $ setConfigKeys dir [("default-type", toJSON ("bip84" :: Text))]
        changed `shouldBe` Right ()
        written <- readFile (configFile dir)
        decodeValue written `shouldBe` decodeValue [r|{"default-type": "bip84"}|]

    it "changes nothing when no key is given" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) twoRequestsFile
        changed <- runExceptT $ setConfigKeys dir []
        changed `shouldBe` Right ()
        written <- readFile (configFile dir)
        decodeValue written `shouldBe` decodeValue twoRequestsFile

    it "refuses a file that does not hold a JSON object, naming it, and leaves it as it is" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) "[1, 2]"
        changed <- runExceptT $ setConfigKeys dir [("default-type", toJSON ("bip84" :: Text))]
        changed `shouldBe` Left ("The configuration file " <> configFile dir <> " does not hold a JSON object")
        readFile (configFile dir) `shouldReturn` "[1, 2]"

-- | What hw makes of the text of a configuration file
decodeConfig :: String -> Either String Config
decodeConfig = eitherDecode . cs

decodeValue :: String -> Value
decodeValue = either error id . eitherDecode . cs

-- | The file hw writes when there is none
defaultFile :: String
defaultFile =
  [r|{
  "host": "https://api.haskoin.com/",
  "gap": 20,
  "parallel-requests": 1,
  "database-file": null,
  "default-network": "btc",
  "shown-networks": ["btc", "bch"],
  "default-type": "bip44",
  "unit": "btc",
  "listing-view": "compact",
  "pager": null,
  "autobackup-database": true,
  "autobackup-labels": true,
  "autobackup-copies": 10
}|]

-- | The file hw 0.9 wrote when there was none: batch sizes and a
-- recovery-gap that no longer mean anything, and no parallel-requests key
hw09File :: String
hw09File =
  [r|{
  "host": "https://api.haskoin.com/",
  "gap": 20,
  "recovery-gap": 40,
  "addr-batch": 100,
  "tx-batch": 100,
  "coin-batch": 100,
  "tx-full-batch": 100,
  "database-file": null
}|]

-- | A file asking for two requests at a time
twoRequestsFile :: String
twoRequestsFile =
  [r|{
  "host": "https://api.haskoin.com/",
  "gap": 20,
  "parallel-requests": 2
}|]
