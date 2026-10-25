{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The migration to the format version 7: a database of version 6, built
-- from its tables ("Haskoin.Wallet.Schema.V6"), is migrated to version 7
-- only and ends without wallets: its accounts have ids, which their rows
-- point to, the fingerprint of their seed still to be learnt, its hashes
-- and outpoints as bytes, and the indexes of the version.
module Haskoin.Wallet.Migration.V7Spec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.Serialize as S
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Migration (databaseVersion)
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Schema.V6 as V6
import qualified Haskoin.Wallet.Schema.V7 as V7
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "From the database of version 6 to version 7" $ do
    it "sees a database of version 6 as version 6" $
      inMemory $ do
        version6Database
        runExceptT databaseVersion `dbShouldBe` Right (Just 6)

    it "backs up the database at version 6, announces the step and stores version 7" $
      inMemory $ do
        version6Database
        migrateToAndCollect 7 ctx `dbShouldBe` (Right (), [stepAnnounced], [6])
        storedVersions `dbShouldBe` [(PersistInt64 7, "integer")]
        runExceptT databaseVersion `dbShouldBe` Right (Just 7)

    it "gives the accounts ids in the order they were created, whatever wallet they were under" $
      inMemory $ do
        version6Database
        migrateTo7 ctx
        queryRows "select id, name, network, x_pub_key from d_b_account order by id"
          `dbShouldBe` [ [PersistInt64 1, PersistText "main", PersistText "btc", PersistText testAccountXPub],
                         [PersistInt64 2, PersistText "cash", PersistText "bch", PersistText testAccountXPubBch],
                         [PersistInt64 3, PersistText "savings", PersistText "btc", PersistText savingsXPub]
                       ]

    it "leaves the fingerprint of every seed to be learnt" $
      inMemory $ do
        version6Database
        migrateTo7 ctx
        queryRows "select fingerprint from d_b_account" `dbShouldBe` [[PersistNull], [PersistNull], [PersistNull]]

    it "keeps the other columns of the accounts" $
      inMemory $ do
        version6Database
        migrateTo7 ctx
        queryRows "select id, \"index\", external, internal, balance_confirmed, balance_unconfirmed, balance_coins, created from d_b_account"
          `dbShouldBe` [ [PersistInt64 1, PersistInt64 0, PersistInt64 1, PersistInt64 1, PersistInt64 50000, PersistInt64 0, PersistInt64 1, PersistText "2023-11-01T00:00:00Z"],
                         [PersistInt64 2, PersistInt64 0, PersistInt64 1, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistText "2023-11-02T00:00:00Z"],
                         [PersistInt64 3, PersistInt64 1, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistText "2023-11-03T00:00:00Z"]
                       ]

    it "points the addresses, summaries, coins and pending transactions to their account's id" $
      inMemory $ do
        version6Database
        migrateTo7 ctx
        queryRows "select account, internal, \"index\", address from d_b_address"
          `dbShouldBe` [ [PersistInt64 1, PersistInt64 0, PersistInt64 0, PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
                         [PersistInt64 1, PersistInt64 1, PersistInt64 0, PersistText "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"],
                         [PersistInt64 2, PersistInt64 0, PersistInt64 0, PersistText bchExternalAddrText]
                       ]
        queryRows "select account, txid from d_b_tx_info" `dbShouldBe` [[PersistInt64 1, PersistByteString (hexBytes (txHashToHex (fakeTxHash 1)))]]
        queryRows "select account, outpoint, address from d_b_coin"
          `dbShouldBe` [[PersistInt64 1, PersistByteString (S.encode (OutPoint (fakeTxHash 1) 0)), PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]
        queryRows "select account, nosig_hash from d_b_pending_tx" `dbShouldBe` [[PersistInt64 3, PersistByteString (hexBytes (txHashToHex (fakeTxHash 5)))]]

    it "keys the raw transactions by the bytes of their hash, keeping them" $
      inMemory $ do
        version6Database
        migrateTo7 ctx
        queryRows "select hash, blob from d_b_raw_tx" `dbShouldBe` [[PersistByteString (hexBytes (txHashToHex (fakeTxHash 1))), PersistByteString "raw"]]

    it "keeps the other columns of the rows, except that nothing is confirmed any more and no path is written out" $
      inMemory $ do
        version6Database
        addressesBefore <- queryRows "select \"index\", address, label, balance_confirmed, balance_unconfirmed, balance_coins, balance_txs, balance_received, internal, created from d_b_address"
        summariesBefore <- queryRows "select block_ref, blob, dead, created from d_b_tx_info"
        coinsBefore <- queryRows "select address, value, block_ref, blob, created from d_b_coin"
        pendingBefore <- queryRows "select blob, online, created from d_b_pending_tx"
        migrateTo7 ctx
        queryRows "select \"index\", address, label, balance_confirmed, balance_unconfirmed, balance_coins, balance_txs, balance_received, internal, created from d_b_address"
          `dbShouldBe` addressesBefore
        queryRows "select block_ref, blob, dead, created from d_b_tx_info" `dbShouldBe` summariesBefore
        queryRows "select address, value, block_ref, blob, created from d_b_coin" `dbShouldBe` coinsBefore
        queryRows "select blob, online, created from d_b_pending_tx" `dbShouldBe` pendingBefore
        columnNames "d_b_address"
          `dbShouldBe` [ "account", "address", "balance_coins", "balance_confirmed", "balance_received", "balance_txs",
                         "balance_unconfirmed", "created", "index", "internal", "label" ]
        columnNames "d_b_tx_info" `dbShouldBe` ["account", "blob", "block_ref", "created", "dead", "txid"]
        columnNames "d_b_coin" `dbShouldBe` ["account", "address", "blob", "block_ref", "created", "outpoint", "value"]

    it "leaves the best blocks as they were" $
      inMemory $ do
        version6Database
        original <- tablesRows otherTables
        migrateTo7 ctx
        tablesRows otherTables `dbShouldBe` original

    it "ends with the tables and the indexes of version 7, the wallets gone" $
      inMemory $ do
        version6Database
        migrateTo7 ctx
        expected <- liftIO $ schemaCreatedBy (createTables V7.migrateAll >> createIndexesOf V7.tableIndexes)
        databaseSchema `dbShouldBe` expected

{- Running the step -}

stepAnnounced :: String
stepAnnounced = "Migrating the database to version 7: the accounts identified by their keys, without wallets"

-- | The migration to version 7, which must succeed
migrateTo7 :: Ctx -> DB IO ()
migrateTo7 ctx = migrateToAndCollect 7 ctx `dbShouldBe` (Right (), [stepAnnounced], [6])

-- | The columns of a table, by name
columnNames :: Text -> DB IO [Text]
columnNames table = do
  rows <- queryRows $ "select name from pragma_table_info('" <> table <> "') order by name"
  return [name | [PersistText name] <- rows]

otherTables :: [Text]
otherTables = ["d_b_best"]

-- | The bytes of a hexadecimal text, as the step turns the old keys into
hexBytes :: Text -> ByteString
hexBytes = fromJust . decodeHex

{- The database of version 6 -}

-- | A database of version 6 with two wallets of the one test seed, as hw
-- filed them: the Bitcoin wallet 892eb8e4 with the account main, created
-- first, and savings, created last, and the Bitcoin Cash wallet a1b2c3d4
-- with the account cash, created in between. main has a receiving address
-- that was paid, a change address, the summary and the coin of its payment;
-- cash has a receiving address; savings holds a pending transaction.
version6Database :: DB IO ()
version6Database = do
  createTables V6.migrateAll
  P.insert_ $ V6.DBVersion 6
  P.insert_ $ V6.DBWallet "892eb8e4"
  P.insert_ $ V6.DBWallet "a1b2c3d4"
  P.insert_ $ account btcWallet "main" 0 "btc" "/44'/0'/0'" 1 1 testAccountXPub 50000 1 fixtureTime
  P.insert_ $ account bchWallet "cash" 0 "bch" "/44'/145'/0'" 1 0 testAccountXPubBch 0 0 laterTime
  P.insert_ $ account btcWallet "savings" 1 "btc" "/44'/0'/1'" 0 0 savingsXPub 0 0 latestTime
  P.insert_ $ address btcWallet "/44'/0'/0'" 0 "/0/0" "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" "salary" 50000 False
  P.insert_ $ address btcWallet "/44'/0'/0'" 0 "/1/0" "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn" "" 0 True
  P.insert_ $ address bchWallet "/44'/145'/0'" 0 "/0/0" bchExternalAddrText "" 0 False
  P.insert_ $
    V6.DBTxInfo btcWallet "/44'/0'/0'" (txHashToHex (fakeTxHash 1)) (S.encode (Store.BlockRef 800000 3)) True "{\"type\": \"credit\"}" False fixtureTime
  P.insert_ $
    V6.DBCoin btcWallet "/44'/0'/0'" (encodeHex (S.encode (OutPoint (fakeTxHash 1) 0))) "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 50000 (S.encode (Store.BlockRef 800000 3)) "coin" True fixtureTime
  P.insert_ $ V6.DBRawTx (txHashToHex (fakeTxHash 1)) "raw"
  P.insert_ $ V6.DBPendingTx btcWallet "/44'/0'/1'" (txHashToHex (fakeTxHash 5)) "{\"tx\": \"\"}" False latestTime
  P.insert_ $ V6.DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105
  commitFixture
  where
    btcWallet = V6.DBWalletKey "892eb8e4"
    bchWallet = V6.DBWalletKey "a1b2c3d4"
    account wallet name index net deriv ext int xpub confirmed coins created =
      V6.DBAccount name wallet index net deriv ext int xpub confirmed 0 coins created
    address wallet accDeriv index path addr label received internal =
      V6.DBAddress index wallet accDeriv path addr label received 0 (if received > 0 then 1 else 0) (if received > 0 then 1 else 0) received internal fixtureTime

-- | The key of the account savings, m/44'/0'/1' of the test seed
savingsXPub :: Text
savingsXPub = "xpub6CHK45s4QJWpt9C5kpAnypsE5ratNMWRGE3pZNQgGnxHTvZ1y6E65mSKMmz7V5ZUnVq5JMgq1Qros98GhLMCK7eoDaNo8kjqNdJzD8otqLW"

laterTime, latestTime :: UTCTime
laterTime = UTCTime (fromGregorian 2023 11 2) 0
latestTime = UTCTime (fromGregorian 2023 11 3) 0
