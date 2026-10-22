{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The migration to the format version 4: a database of version 3, built
-- from its tables ("Haskoin.Wallet.Schema.V3") with the version stored as a
-- number, is migrated to version 4 only and ends with the dead column on
-- its transaction summaries.
module Haskoin.Wallet.Migration.V4Spec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Migration (databaseVersion)
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Schema.V3 as V3
import qualified Haskoin.Wallet.Schema.V4 as V4
import Haskoin.Wallet.TestUtils
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "From the database of version 3 to version 4" $ do
    it "sees a database of version 3 as version 3" $
      inMemory $ do
        version3Database
        runExceptT databaseVersion `dbShouldBe` Right (Just 3)

    it "backs up the database at version 3, announces the step and stores version 4" $
      inMemory $ do
        version3Database
        migrateToAndCollect 4 ctx `dbShouldBe` (Right (), [stepAnnounced], [3])
        storedVersions `dbShouldBe` [(PersistInt64 4, "integer")]
        runExceptT databaseVersion `dbShouldBe` Right (Just 4)

    it "adds the dead column to the summaries, false for the existing ones" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select txid, dead from d_b_tx_info" `dbShouldBe` [[PersistText creditTxid, PersistInt64 0]]

    it "keeps the summaries, labels included, and their other columns" $
      inMemory $ do
        version3Database
        original <- summaryColumns
        migrateTo4 ctx
        summaryColumns `dbShouldBe` original

    it "leaves every other table as it was" $
      inMemory $ do
        version3Database
        original <- tablesRows otherTables
        migrateTo4 ctx
        tablesRows otherTables `dbShouldBe` original

    it "ends with the tables of version 4" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        expected <- liftIO $ schemaCreatedBy V4.migrateAll
        databaseSchema `dbShouldBe` expected

{- Running the step -}

stepAnnounced :: String
stepAnnounced = "Migrating the database to version 4: the dead transactions"

-- | The migration to version 4, which must succeed
migrateTo4 :: Ctx -> DB IO ()
migrateTo4 ctx = migrateToAndCollect 4 ctx `dbShouldBe` (Right (), [stepAnnounced], [3])

-- | The columns of the summaries that version 3 had
summaryColumns :: DB IO [[PersistValue]]
summaryColumns =
  queryRows "select account_wallet, account_derivation, txid, block_ref, confirmed, blob, created from d_b_tx_info"

{- The database of version 3 -}

-- | A database of version 3 with the test wallet, its Bitcoin account, a
-- labelled address and the labelled summary of a credit
version3Database :: DB IO ()
version3Database = do
  createTables V3.migrateAll
  P.insert_ $ V3.DBVersion 3
  P.insert_ $ V3.DBWallet "892eb8e4"
  P.insert_ btcAccount
  P.insert_ fixtureAddress
  P.insert_ creditSummary
  P.insert_ fixtureBest
  commitFixture

otherTables :: [Text]
otherTables =
  [ "d_b_wallet",
    "d_b_account",
    "d_b_address",
    "d_b_coin",
    "d_b_raw_tx",
    "d_b_pending_tx",
    "d_b_best"
  ]

testWalletKey :: V3.DBWalletId
testWalletKey = V3.DBWalletKey "892eb8e4"

btcDerivation :: Text
btcDerivation = "/44'/0'/0'"

btcAccount :: V3.DBAccount
btcAccount =
  V3.DBAccount
    { V3.dBAccountName = "main",
      V3.dBAccountWallet = testWalletKey,
      V3.dBAccountIndex = 0,
      V3.dBAccountNetwork = "btc",
      V3.dBAccountDerivation = btcDerivation,
      V3.dBAccountExternal = 1,
      V3.dBAccountInternal = 0,
      V3.dBAccountXPubKey = testAccountXPub,
      V3.dBAccountBalanceConfirmed = 50000,
      V3.dBAccountBalanceUnconfirmed = 0,
      V3.dBAccountBalanceCoins = 1,
      V3.dBAccountCreated = fixtureTime
    }

fixtureAddress :: V3.DBAddress
fixtureAddress =
  V3.DBAddress
    { V3.dBAddressIndex = 0,
      V3.dBAddressAccountWallet = testWalletKey,
      V3.dBAddressAccountDerivation = btcDerivation,
      V3.dBAddressDerivation = "/0/0",
      V3.dBAddressAddress = "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
      V3.dBAddressLabel = "salary",
      V3.dBAddressBalanceConfirmed = 50000,
      V3.dBAddressBalanceUnconfirmed = 0,
      V3.dBAddressBalanceCoins = 1,
      V3.dBAddressBalanceTxs = 1,
      V3.dBAddressBalanceReceived = 50000,
      V3.dBAddressInternal = False,
      V3.dBAddressFree = False,
      V3.dBAddressCreated = fixtureTime
    }

fixtureBest :: V3.DBBest
fixtureBest =
  V3.DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105

creditTxid :: Text
creditTxid = "0000000000000000000000000000000000000000000000000000000000000011"

-- | 50000 satoshis received on /0/0, labelled "salary", as a version 3
-- database holds it: with the change absorbed into the fee, which hw 0.10
-- added as an optional field
creditSummary :: V3.DBTxInfo
creditSummary =
  V3.DBTxInfo
    { V3.dBTxInfoAccountWallet = testWalletKey,
      V3.dBTxInfoAccountDerivation = btcDerivation,
      V3.dBTxInfoTxid = creditTxid,
      V3.dBTxInfoBlockRef = S.encode (Store.BlockRef 800000 3),
      V3.dBTxInfoConfirmed = True,
      V3.dBTxInfoBlob = cs creditBlob,
      V3.dBTxInfoCreated = fixtureTime
    }

-- | The summary as a version 3 database holds it
creditBlob :: String
creditBlob =
  [r|{
    "amount": 50000,
    "block": {
        "height": 800000,
        "position": 3
    },
    "changeinfee": null,
    "confirmations": 12,
    "fee": 1000,
    "feebyte": 4,
    "myinputs": {},
    "myoutputs": {
        "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {
            "label": "salary",
            "path": "/0/0",
            "value": 50000
        }
    },
    "nonstdinputs": [],
    "nonstdoutputs": [],
    "otherinputs": {
        "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ": {
            "value": 100000
        }
    },
    "otheroutputs": {
        "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU": 49000
    },
    "pending": null,
    "size": 226,
    "txid": "0000000000000000000000000000000000000000000000000000000000000011",
    "type": "credit"
}|]
