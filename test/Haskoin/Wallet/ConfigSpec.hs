{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The configuration file of hw: what it holds, how a file written by an
-- older hw or trimmed by hand reads, the four keys that say where a
-- command works, the data directory around the file, and the keys a
-- command changes in it
module Haskoin.Wallet.ConfigSpec (spec) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (Value, eitherDecode, toJSON, (.=))
import Data.Default (def)
import Data.List (isPrefixOf)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin (bch, btc, btcTest)
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
      configDatabaseFile <$> decodeConfig [r|{"database-file": "/backups/accounts.sqlite"}|]
        `shouldBe` Right (Just "/backups/accounts.sqlite")

    it "cannot be a gap of zero, which would refuse every new address" $
      decodeConfig [r|{"gap": 0}|]
        `shouldBe` Left "Error in $: The key \"gap\" must be at least 1"

    it "cannot be zero requests at a time, which would send none" $
      decodeConfig [r|{"parallel-requests": 0}|]
        `shouldBe` Left "Error in $: The key \"parallel-requests\" must be at least 1"

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

directorySpec :: Spec
directorySpec =
  describe "The data directory" $ do
    it "holds the database, unless the configuration names another file" $ do
      databaseFileIn "/home/someone/.hw" def `shouldBe` "/home/someone/.hw/accounts.sqlite"
      databaseFileIn "/home/someone/.hw" def {configDatabaseFile = Just "/backups/accounts.sqlite"}
        `shouldBe` "/backups/accounts.sqlite"

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
  "default-type": "bip44",
  "unit": "btc",
  "listing-view": "compact",
  "pager": null
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
