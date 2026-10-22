{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The configuration file of hw: what it holds, and reading one written
-- by an older hw
module Haskoin.Wallet.ConfigSpec (spec) where

import Data.Aeson (Value, eitherDecode, toJSON)
import Data.Default (def)
import Data.String.Conversions (cs)
import Haskoin.Wallet.Config
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec =
  describe "The configuration file" $ do
    it "holds the default configuration" $
      toJSON (def :: Config) `shouldBe` decodeValue defaultFile

    it "reads the default configuration back" $
      eitherDecode (cs defaultFile) `shouldBe` Right (def :: Config)

    it "reads a file written by hw 0.9, which has no parallel-requests entry" $
      eitherDecode (cs hw09File)
        `shouldBe` Right
          Config
            { configHost = "https://api.haskoin.com/",
              configGap = 20,
              configRecoveryGap = 40,
              configAddrBatch = 100,
              configTxBatch = 100,
              configCoinBatch = 100,
              configTxFullBatch = 100,
              configParallelRequests = 1,
              configDatabaseFile = Nothing
            }

    it "reads the number of parallel requests" $
      (configParallelRequests <$> eitherDecode (cs twoRequestsFile)) `shouldBe` Right 2

decodeValue :: String -> Value
decodeValue = either error id . eitherDecode . cs

-- | The file hw 0.10 writes when there is none
defaultFile :: String
defaultFile =
  [r|{
  "host": "https://api.haskoin.com/",
  "gap": 20,
  "recovery-gap": 40,
  "addr-batch": 100,
  "tx-batch": 100,
  "coin-batch": 100,
  "tx-full-batch": 100,
  "parallel-requests": 1,
  "database-file": null
}|]

-- | The file hw 0.9 wrote when there was none
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
  "recovery-gap": 40,
  "addr-batch": 100,
  "tx-batch": 100,
  "coin-batch": 100,
  "tx-full-batch": 100,
  "parallel-requests": 2
}|]
