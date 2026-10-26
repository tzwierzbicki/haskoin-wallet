{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The configuration file of hw: what it holds, how a file written by an
-- older hw or trimmed by hand reads, and the data directory around it
module Haskoin.Wallet.ConfigSpec (spec) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (Value, eitherDecode, toJSON)
import Data.Default (def)
import Data.String.Conversions (cs)
import Haskoin.Wallet.Config
import Haskoin.Wallet.TestUtils (inScratchDirectory)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), MarshalJsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryConfig)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  fileSpec
  keySpec
  directorySpec
  -- A configuration reads back as it was written
  testIdentity def {jsonTests = [JsonBox arbitraryConfig]}

fileSpec :: Spec
fileSpec =
  describe "The configuration file" $ do
    it "holds the default configuration" $
      toJSON (def :: Config) `shouldBe` decodeValue defaultFile

    it "reads the default configuration back" $
      decodeConfig defaultFile `shouldBe` Right def

    it "reads a file of hw 0.9: the tx-batch and recovery-gap keys mean nothing now, the parallel requests take their default" $
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
      decodeConfig [r|{"gap": null, "database-file": null}|] `shouldBe` Right def

    it "names the database to work on" $
      configDatabaseFile <$> decodeConfig [r|{"database-file": "/backups/accounts.sqlite"}|]
        `shouldBe` Right (Just "/backups/accounts.sqlite")

    it "cannot be a gap of zero, which would refuse every new address" $
      decodeConfig [r|{"gap": 0}|]
        `shouldBe` Left "Error in $: The key \"gap\" must be at least 1"

    it "cannot be a batch of zero, which would ask the store forever" $
      decodeConfig [r|{"addr-batch": 0}|]
        `shouldBe` Left "Error in $: The key \"addr-batch\" must be at least 1"

    it "must be a number when its default is one" $
      decodeConfig [r|{"gap": "twenty"}|]
        `shouldBe` Left "Error in $.gap: parsing Natural failed, expected Number, but encountered String"

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

    it "warns about the keys it does not know, naming the file: those of an earlier hw" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) hw09File
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (def, ["The configuration file " <> configFile dir <> " has keys hw does not know, left aside: recovery-gap, tx-batch"])

    it "warns about a mistyped key, whose setting would otherwise keep its default without a word" $
      inScratchDirectory $ \dir -> do
        writeFile (configFile dir) [r|{"gaps": 30}|]
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (def, ["The configuration file " <> configFile dir <> " has keys hw does not know, left aside: gaps"])

-- | What hw makes of the text of a configuration file
decodeConfig :: String -> Either String Config
decodeConfig = eitherDecode . cs

decodeValue :: String -> Value
decodeValue = either error id . eitherDecode . cs

-- | The file hw 0.10 writes when there is none
defaultFile :: String
defaultFile =
  [r|{
  "host": "https://api.haskoin.com/",
  "gap": 20,
  "addr-batch": 100,
  "coin-batch": 100,
  "tx-full-batch": 100,
  "parallel-requests": 1,
  "database-file": null
}|]

-- | The file hw 0.9 wrote when there was none: tx-batch and recovery-gap
-- keys that no longer mean anything, and no parallel-requests key
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
  "addr-batch": 100,
  "coin-batch": 100,
  "tx-full-batch": 100,
  "parallel-requests": 2
}|]
