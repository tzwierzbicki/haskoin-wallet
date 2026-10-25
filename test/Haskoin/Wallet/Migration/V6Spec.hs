{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The migration to the format version 6: a database of version 5, built
-- from its tables ("Haskoin.Wallet.Schema.V5"), is migrated to version 6
-- only and ends without the free column of its addresses.
module Haskoin.Wallet.Migration.V6Spec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Serialize as S
import Data.Text (Text)
import Data.Word (Word64)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Migration (databaseVersion)
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Schema.V5 as V5
import qualified Haskoin.Wallet.Schema.V6 as V6
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "From the database of version 5 to version 6" $ do
    it "sees a database of version 5 as version 5" $
      inMemory $ do
        version5Database
        runExceptT databaseVersion `dbShouldBe` Right (Just 5)

    it "backs up the database at version 5, announces the step and stores version 6" $
      inMemory $ do
        version5Database
        migrateToAndCollect 6 ctx `dbShouldBe` (Right (), [stepAnnounced], [5])
        storedVersions `dbShouldBe` [(PersistInt64 6, "integer")]
        runExceptT databaseVersion `dbShouldBe` Right (Just 6)

    it "drops the free column of the addresses" $
      inMemory $ do
        version5Database
        addressColumnNames
          `dbShouldBe` [ "account_derivation", "account_wallet", "address", "balance_coins", "balance_confirmed",
                         "balance_received", "balance_txs", "balance_unconfirmed", "created", "derivation", "free",
                         "index", "internal", "label" ]
        migrateTo6 ctx
        addressColumnNames
          `dbShouldBe` [ "account_derivation", "account_wallet", "address", "balance_coins", "balance_confirmed",
                         "balance_received", "balance_txs", "balance_unconfirmed", "created", "derivation",
                         "index", "internal", "label" ]

    it "keeps the addresses, the free one included, with their other columns" $
      inMemory $ do
        version5Database
        original <- addressColumns
        migrateTo6 ctx
        addressColumns `dbShouldBe` original

    it "leaves every other table as it was" $
      inMemory $ do
        version5Database
        original <- tablesRows otherTables
        migrateTo6 ctx
        tablesRows otherTables `dbShouldBe` original

    it "ends with the tables of version 6" $
      inMemory $ do
        version5Database
        migrateTo6 ctx
        expected <- liftIO $ schemaCreatedBy (createTables V6.migrateAll)
        databaseSchema `dbShouldBe` expected

{- Running the step -}

stepAnnounced :: String
stepAnnounced = "Migrating the database to version 6: the change addresses read from the pending transactions"

-- | The migration to version 6, which must succeed
migrateTo6 :: Ctx -> DB IO ()
migrateTo6 ctx = migrateToAndCollect 6 ctx `dbShouldBe` (Right (), [stepAnnounced], [5])

-- | The columns of the addresses, by name
addressColumnNames :: DB IO [Text]
addressColumnNames = do
  rows <- queryRows "select name from pragma_table_info('d_b_address')"
  return [name | [PersistText name] <- rows]

-- | The columns of the addresses that version 6 keeps
addressColumns :: DB IO [[PersistValue]]
addressColumns =
  queryRows
    "select account_wallet, account_derivation, \"index\", derivation, address, label, balance_confirmed, \
    \balance_unconfirmed, balance_coins, balance_txs, balance_received, internal, created from d_b_address"

{- The database of version 5 -}

-- | A database of version 5 with the test wallet, its Bitcoin account, a
-- receiving address that was paid and a change address still free, a coin
-- and the best block
version5Database :: DB IO ()
version5Database = do
  createTables V5.migrateAll
  P.insert_ $ V5.DBVersion 5
  P.insert_ $ V5.DBWallet "892eb8e4"
  P.insert_ btcAccount
  P.insert_ $ fixtureAddress "/0/0" "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" "salary" 50000 False False
  P.insert_ $ fixtureAddress "/1/0" "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn" "Internal Address" 0 True True
  P.insert_ fixtureCoin
  P.insert_ fixtureBest
  commitFixture

otherTables :: [Text]
otherTables =
  [ "d_b_wallet",
    "d_b_account",
    "d_b_tx_info",
    "d_b_coin",
    "d_b_raw_tx",
    "d_b_pending_tx",
    "d_b_best"
  ]

testWalletKey :: V5.DBWalletId
testWalletKey = V5.DBWalletKey "892eb8e4"

btcDerivation :: Text
btcDerivation = "/44'/0'/0'"

btcAccount :: V5.DBAccount
btcAccount =
  V5.DBAccount
    { V5.dBAccountName = "main",
      V5.dBAccountWallet = testWalletKey,
      V5.dBAccountIndex = 0,
      V5.dBAccountNetwork = "btc",
      V5.dBAccountDerivation = btcDerivation,
      V5.dBAccountExternal = 1,
      V5.dBAccountInternal = 1,
      V5.dBAccountXPubKey = testAccountXPub,
      V5.dBAccountBalanceConfirmed = 50000,
      V5.dBAccountBalanceUnconfirmed = 0,
      V5.dBAccountBalanceCoins = 1,
      V5.dBAccountCreated = fixtureTime
    }

-- | An address of the account at the given path, paid the given amount in
-- one transaction, or nothing, as a version 5 database holds it with its
-- free flag
fixtureAddress :: Text -> Text -> Text -> Word64 -> Bool -> Bool -> V5.DBAddress
fixtureAddress path address label received internal free =
  V5.DBAddress
    { V5.dBAddressIndex = 0,
      V5.dBAddressAccountWallet = testWalletKey,
      V5.dBAddressAccountDerivation = btcDerivation,
      V5.dBAddressDerivation = path,
      V5.dBAddressAddress = address,
      V5.dBAddressLabel = label,
      V5.dBAddressBalanceConfirmed = received,
      V5.dBAddressBalanceUnconfirmed = 0,
      V5.dBAddressBalanceCoins = if received > 0 then 1 else 0,
      V5.dBAddressBalanceTxs = if received > 0 then 1 else 0,
      V5.dBAddressBalanceReceived = received,
      V5.dBAddressInternal = internal,
      V5.dBAddressFree = free,
      V5.dBAddressCreated = fixtureTime
    }

-- | The coin of the receiving address, confirmed at height 800000
fixtureCoin :: V5.DBCoin
fixtureCoin =
  V5.DBCoin
    { V5.dBCoinAccountWallet = testWalletKey,
      V5.dBCoinAccountDerivation = btcDerivation,
      V5.dBCoinOutpoint = encodeHex (S.encode outpoint),
      V5.dBCoinAddress = "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
      V5.dBCoinValue = 50000,
      V5.dBCoinBlockRef = S.encode (Store.BlockRef 800000 3),
      V5.dBCoinBlob = S.encode unspent,
      V5.dBCoinConfirmed = True,
      V5.dBCoinCreated = fixtureTime
    }
  where
    outpoint = OutPoint (fakeTxHash 1) 0
    unspent =
      Store.Unspent
        { Store.block = Store.BlockRef 800000 3,
          Store.outpoint = outpoint,
          Store.value = 50000,
          Store.script = "",
          Store.address = Nothing
        }

fixtureBest :: V5.DBBest
fixtureBest =
  V5.DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105
