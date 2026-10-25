{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The migration to the format version 5: a database of version 4, built
-- from its tables ("Haskoin.Wallet.Schema.V4"), is migrated to version 5
-- only and ends without the locked column of its coins.
module Haskoin.Wallet.Migration.V5Spec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Migration (databaseVersion)
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Schema.V4 as V4
import qualified Haskoin.Wallet.Schema.V5 as V5
import Haskoin.Wallet.TestUtils
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "From the database of version 4 to version 5" $ do
    it "sees a database of version 4 as version 4" $
      inMemory $ do
        version4Database
        runExceptT databaseVersion `dbShouldBe` Right (Just 4)

    it "backs up the database at version 4, announces the step and stores version 5" $
      inMemory $ do
        version4Database
        migrateToAndCollect 5 ctx `dbShouldBe` (Right (), [stepAnnounced], [4])
        storedVersions `dbShouldBe` [(PersistInt64 5, "integer")]
        runExceptT databaseVersion `dbShouldBe` Right (Just 5)

    it "drops the locked column of the coins" $
      inMemory $ do
        version4Database
        coinColumnNames `dbShouldBe` ["account_derivation", "account_wallet", "address", "blob", "block_ref", "confirmed", "created", "locked", "outpoint", "value"]
        migrateTo5 ctx
        coinColumnNames `dbShouldBe` ["account_derivation", "account_wallet", "address", "blob", "block_ref", "confirmed", "created", "outpoint", "value"]

    it "keeps the coins, the locked one included, with their other columns" $
      inMemory $ do
        version4Database
        original <- coinColumns
        migrateTo5 ctx
        coinColumns `dbShouldBe` original

    it "leaves every other table as it was, the pending transactions included" $
      inMemory $ do
        version4Database
        original <- tablesRows otherTables
        migrateTo5 ctx
        tablesRows otherTables `dbShouldBe` original

    it "ends with the tables of version 5" $
      inMemory $ do
        version4Database
        migrateTo5 ctx
        expected <- liftIO $ schemaCreatedBy (createTables V5.migrateAll)
        databaseSchema `dbShouldBe` expected

{- Running the step -}

stepAnnounced :: String
stepAnnounced = "Migrating the database to version 5: the coin locks read from the pending transactions"

-- | The migration to version 5, which must succeed
migrateTo5 :: Ctx -> DB IO ()
migrateTo5 ctx = migrateToAndCollect 5 ctx `dbShouldBe` (Right (), [stepAnnounced], [4])

-- | The columns of the coins, by name
coinColumnNames :: DB IO [Text]
coinColumnNames = do
  rows <- queryRows "select name from pragma_table_info('d_b_coin')"
  return [name | [PersistText name] <- rows]

-- | The columns of the coins that version 5 keeps
coinColumns :: DB IO [[PersistValue]]
coinColumns =
  queryRows "select account_wallet, account_derivation, outpoint, address, value, block_ref, blob, confirmed, created from d_b_coin order by outpoint"

{- The database of version 4 -}

-- | A database of version 4 with the test wallet, its Bitcoin account, an
-- address, two coins, one of them locked by the pending transaction that
-- spends it, and that pending transaction
version4Database :: DB IO ()
version4Database = do
  createTables V4.migrateAll
  P.insert_ $ V4.DBVersion 4
  P.insert_ $ V4.DBWallet "892eb8e4"
  P.insert_ btcAccount
  P.insert_ fixtureAddress
  P.insert_ $ fixtureCoin (OutPoint (fakeTxHash 1) 0) 50000 True
  P.insert_ $ fixtureCoin (OutPoint (fakeTxHash 2) 1) 20000 False
  P.insert_ fixturePending
  P.insert_ fixtureBest
  commitFixture

otherTables :: [Text]
otherTables =
  [ "d_b_wallet",
    "d_b_account",
    "d_b_address",
    "d_b_tx_info",
    "d_b_raw_tx",
    "d_b_pending_tx",
    "d_b_best"
  ]

testWalletKey :: V4.DBWalletId
testWalletKey = V4.DBWalletKey "892eb8e4"

btcDerivation :: Text
btcDerivation = "/44'/0'/0'"

btcAccount :: V4.DBAccount
btcAccount =
  V4.DBAccount
    { V4.dBAccountName = "main",
      V4.dBAccountWallet = testWalletKey,
      V4.dBAccountIndex = 0,
      V4.dBAccountNetwork = "btc",
      V4.dBAccountDerivation = btcDerivation,
      V4.dBAccountExternal = 1,
      V4.dBAccountInternal = 0,
      V4.dBAccountXPubKey = testAccountXPub,
      V4.dBAccountBalanceConfirmed = 70000,
      V4.dBAccountBalanceUnconfirmed = 0,
      V4.dBAccountBalanceCoins = 2,
      V4.dBAccountCreated = fixtureTime
    }

fixtureAddress :: V4.DBAddress
fixtureAddress =
  V4.DBAddress
    { V4.dBAddressIndex = 0,
      V4.dBAddressAccountWallet = testWalletKey,
      V4.dBAddressAccountDerivation = btcDerivation,
      V4.dBAddressDerivation = "/0/0",
      V4.dBAddressAddress = "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
      V4.dBAddressLabel = "",
      V4.dBAddressBalanceConfirmed = 70000,
      V4.dBAddressBalanceUnconfirmed = 0,
      V4.dBAddressBalanceCoins = 2,
      V4.dBAddressBalanceTxs = 2,
      V4.dBAddressBalanceReceived = 70000,
      V4.dBAddressInternal = False,
      V4.dBAddressFree = False,
      V4.dBAddressCreated = fixtureTime
    }

-- | A confirmed coin of the address at height 800000, as a version 4
-- database holds it, locked or not
fixtureCoin :: OutPoint -> Word64 -> Bool -> V4.DBCoin
fixtureCoin outpoint value locked =
  V4.DBCoin
    { V4.dBCoinAccountWallet = testWalletKey,
      V4.dBCoinAccountDerivation = btcDerivation,
      V4.dBCoinOutpoint = encodeHex (S.encode outpoint),
      V4.dBCoinAddress = "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
      V4.dBCoinValue = value,
      V4.dBCoinBlockRef = S.encode (Store.BlockRef 800000 3),
      V4.dBCoinBlob = S.encode unspent,
      V4.dBCoinConfirmed = True,
      V4.dBCoinLocked = locked,
      V4.dBCoinCreated = fixtureTime
    }
  where
    unspent =
      Store.Unspent
        { Store.block = Store.BlockRef 800000 3,
          Store.outpoint = outpoint,
          Store.value = value,
          Store.script = "",
          Store.address = Nothing
        }

-- | The pending transaction spending the locked coin, as hw 0.10 stores
-- its signing data; the migration does not read it
fixturePending :: V4.DBPendingTx
fixturePending =
  V4.DBPendingTx
    { V4.dBPendingTxAccountWallet = testWalletKey,
      V4.dBPendingTxAccountDerivation = btcDerivation,
      V4.dBPendingTxNosigHash = "7d1c5e9d2a6f4b3c8e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d",
      V4.dBPendingTxBlob = cs pendingBlob,
      V4.dBPendingTxOnline = False,
      V4.dBPendingTxCreated = fixtureTime
    }

pendingBlob :: String
pendingBlob =
  [r|{
    "feerate": 10,
    "inputpaths": [
        "/0/0"
    ],
    "outputpaths": [],
    "signed": false,
    "tx": "01000000010100000000000000000000000000000000000000000000000000000000000000000000000000ffffffff0150c30000000000001976a914111111111111111111111111111111111111111188ac00000000",
    "txinputs": []
}|]

fixtureBest :: V4.DBBest
fixtureBest =
  V4.DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105
