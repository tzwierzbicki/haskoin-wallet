{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The migration to the format version 4: a database of version 3, built
-- from its tables ("Haskoin.Wallet.Schema.V3"), is migrated to version 4
-- and ends without wallets: its accounts have ids, which their rows point
-- to, the fingerprint of their seed still to be learnt, its hashes and
-- outpoints as bytes, the columns whose values are now read elsewhere
-- gone, and the indexes of the version.
module Haskoin.Wallet.Migration.V4Spec (spec) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian)
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
import System.FilePath ((</>))
import Test.Hspec

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

    it "migrates a database without any account, as a new wallet of version 3 is" $
      inMemory $ do
        createTables V3.migrateAll
        P.insert_ $ V3.DBVersion 3
        commitFixture
        migrateTo4 ctx
        tablesRows ["d_b_account", "d_b_address", "d_b_tx_info", "d_b_coin", "d_b_raw_tx", "d_b_pending_tx"] `dbShouldBe` replicate 6 []
        expected <- liftIO $ schemaCreatedBy (createTables V4.migrateAll >> createIndexesOf V4.tableIndexes)
        databaseSchema `dbShouldBe` expected

    it "gives the accounts ids in the order they were created, whatever wallet they were under" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select id, name, network, x_pub_key from d_b_account order by id"
          `dbShouldBe` [ [PersistInt64 1, PersistText "main", PersistText "btc", PersistText testAccountXPub],
                         [PersistInt64 2, PersistText "cash", PersistText "bch", PersistText testAccountXPubBch],
                         [PersistInt64 3, PersistText "savings", PersistText "btc", PersistText savingsXPub]
                       ]

    it "numbers the accounts created at the same moment by network, then by index" $
      inMemory $ do
        version3Database
        P.updateWhere [] [V3.DBAccountCreated P.=. fixtureTime]
        commitFixture
        migrateTo4 ctx
        queryRows "select id, name from d_b_account order by id"
          `dbShouldBe` [ [PersistInt64 1, PersistText "cash"],
                         [PersistInt64 2, PersistText "main"],
                         [PersistInt64 3, PersistText "savings"]
                       ]

    it "leaves the fingerprint of every seed to be learnt" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select fingerprint from d_b_account" `dbShouldBe` [[PersistNull], [PersistNull], [PersistNull]]

    it "keeps the other columns of the accounts" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select id, \"index\", external, internal, balance_confirmed, balance_unconfirmed, balance_coins, created from d_b_account"
          `dbShouldBe` [ [PersistInt64 1, PersistInt64 0, PersistInt64 1, PersistInt64 1, PersistInt64 50000, PersistInt64 0, PersistInt64 1, PersistText "2023-11-01T00:00:00Z"],
                         [PersistInt64 2, PersistInt64 0, PersistInt64 1, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistText "2023-11-02T00:00:00Z"],
                         [PersistInt64 3, PersistInt64 1, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistText "2023-11-03T00:00:00Z"]
                       ]

    it "points the addresses, summaries, coins and pending transactions to their account's id" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select account, internal, \"index\", address from d_b_address"
          `dbShouldBe` [ [PersistInt64 1, PersistInt64 0, PersistInt64 0, PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
                         [PersistInt64 1, PersistInt64 1, PersistInt64 0, PersistText "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"],
                         [PersistInt64 2, PersistInt64 0, PersistInt64 0, PersistText bchExternalAddrText]
                       ]
        queryRows "select account, txid from d_b_tx_info" `dbShouldBe` [[PersistInt64 1, PersistByteString (hexBytes (txHashToHex (fakeTxHash 1)))]]
        queryRows "select account, outpoint, address from d_b_coin"
          `dbShouldBe` [[PersistInt64 1, PersistByteString (S.encode (OutPoint (fakeTxHash 1) 0)), PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]
        queryRows "select account, nosig_hash from d_b_pending_tx" `dbShouldBe` [[PersistInt64 3, PersistByteString (hexBytes (txHashToHex (fakeTxHash 5)))]]

    it "keeps apart the rows of two seeds whose accounts have the same path" $
      inMemory $ do
        version3Database
        anotherSeed
        migrateTo4 ctx
        queryRows "select id, name, x_pub_key from d_b_account where id in (1, 4)"
          `dbShouldBe` [[PersistInt64 1, PersistText "main", PersistText testAccountXPub], [PersistInt64 4, PersistText "other", PersistText testAccountXPub2]]
        queryRows "select account, address from d_b_address where internal = 0 and account in (1, 4)"
          `dbShouldBe` [[PersistInt64 1, PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"], [PersistInt64 4, PersistText externalAddr2Text]]
        queryRows "select account, txid from d_b_tx_info"
          `dbShouldBe` [ [PersistInt64 1, PersistByteString (hexBytes (txHashToHex (fakeTxHash 1)))],
                         [PersistInt64 4, PersistByteString (hexBytes (txHashToHex (fakeTxHash 2)))]
                       ]
        queryRows "select account, address from d_b_coin"
          `dbShouldBe` [[PersistInt64 1, PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"], [PersistInt64 4, PersistText externalAddr2Text]]

    it "keys the raw transactions by the bytes of their hash, keeping them" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select hash, blob from d_b_raw_tx" `dbShouldBe` [[PersistByteString (hexBytes (txHashToHex (fakeTxHash 1))), PersistByteString "raw"]]

    it "keeps the other columns of every row, the free change address and the locked coin included" $
      inMemory $ do
        version3Database
        addressesBefore <- queryRows "select \"index\", address, label, balance_confirmed, balance_unconfirmed, balance_coins, balance_txs, balance_received, internal, created from d_b_address"
        summariesBefore <- queryRows "select block_ref, blob, created from d_b_tx_info"
        coinsBefore <- queryRows "select address, value, block_ref, created from d_b_coin"
        pendingBefore <- queryRows "select blob, online, created from d_b_pending_tx"
        migrateTo4 ctx
        queryRows "select \"index\", address, label, balance_confirmed, balance_unconfirmed, balance_coins, balance_txs, balance_received, internal, created from d_b_address"
          `dbShouldBe` addressesBefore
        queryRows "select block_ref, blob, created from d_b_tx_info" `dbShouldBe` summariesBefore
        queryRows "select address, value, block_ref, created from d_b_coin" `dbShouldBe` coinsBefore
        queryRows "select blob, online, created from d_b_pending_tx" `dbShouldBe` pendingBefore

    it "drops the wallets and the columns whose values are read elsewhere now: paths, confirmed and free flags, locks, and the copy of each coin" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        columnNames "d_b_account"
          `dbShouldBe` [ "balance_coins", "balance_confirmed", "balance_unconfirmed", "created", "external", "fingerprint",
                         "id", "index", "internal", "name", "network", "x_pub_key" ]
        columnNames "d_b_address"
          `dbShouldBe` [ "account", "address", "balance_coins", "balance_confirmed", "balance_received", "balance_txs",
                         "balance_unconfirmed", "created", "index", "internal", "label" ]
        columnNames "d_b_tx_info" `dbShouldBe` ["account", "blob", "block_ref", "created", "dead", "txid"]
        columnNames "d_b_coin" `dbShouldBe` ["account", "address", "block_ref", "created", "locked_by_user", "outpoint", "value"]
        columnNames "d_b_pending_tx" `dbShouldBe` ["account", "blob", "conflicted", "created", "nosig_hash", "online"]

    it "marks no summary dead: version 3 deleted the summaries of dead transactions" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select dead from d_b_tx_info" `dbShouldBe` [[PersistInt64 0]]

    it "takes no lock of version 3 for a lock of the user: those were the pending transactions' locks" $
      inMemory $ do
        version3Database
        queryRows "select locked from d_b_coin" `dbShouldBe` [[PersistInt64 1]]
        migrateTo4 ctx
        queryRows "select locked_by_user from d_b_coin" `dbShouldBe` [[PersistInt64 0]]

    it "starts every pending transaction out of conflict, for the next sync to ask the store" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select conflicted from d_b_pending_tx" `dbShouldBe` [[PersistInt64 0]]

    it "leaves the best blocks as they were" $
      inMemory $ do
        version3Database
        original <- tablesRows otherTables
        migrateTo4 ctx
        tablesRows otherTables `dbShouldBe` original

    it "changes nothing when it fails halfway: the database stays at version 3, as it was" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "accounts.sqlite"
        inDatabaseFile file $ do
          version3Database
          -- A raw transaction under a key that is not hexadecimal, which no
          -- hw wrote: the step fails on it, the last of its copies, after it
          -- has set the old tables aside and filled the new ones
          P.insert_ $ V3.DBRawTx "not hexadecimal" "raw"
          commitFixture
        original <- inDatabaseFile file $ (,) <$> databaseSchema <*> tablesRows version3Tables
        result <- try (inDatabaseFile file (migrateToAndCollect 4 ctx)) :: IO (Either SomeException (Either String (), [String], [Int]))
        either show (const "no failure") result `shouldSatisfy` ("NOT NULL constraint failed: d_b_raw_tx.hash" `isInfixOf`)
        inDatabaseFile file ((,) <$> databaseSchema <*> tablesRows version3Tables) `shouldReturn` original
        inDatabaseFile file (runExceptT databaseVersion) `shouldReturn` Right (Just 3)

    it "ends with the tables and the indexes of version 4, the wallets gone" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        expected <- liftIO $ schemaCreatedBy (createTables V4.migrateAll >> createIndexesOf V4.tableIndexes)
        databaseSchema `dbShouldBe` expected

{- Running the step -}

stepAnnounced :: String
stepAnnounced = "Migrating the database to version 4: the accounts identified by their keys, without wallets"

-- | The migration to version 4, which must succeed
migrateTo4 :: Ctx -> DB IO ()
migrateTo4 ctx = migrateToAndCollect 4 ctx `dbShouldBe` (Right (), [stepAnnounced], [3])

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

{- The database of version 3 -}

-- | A database of version 3 with two wallets of the one test seed, as hw
-- filed them: the Bitcoin wallet 892eb8e4 with the account main, created
-- first, and savings, created last, and the Bitcoin Cash wallet a1b2c3d4
-- with the account cash, created in between. main has a receiving address
-- that was paid and a change address still free, the summary of its
-- payment and its coin, locked by a pending transaction of the time; cash
-- has a receiving address; savings holds a pending transaction.
version3Database :: DB IO ()
version3Database = do
  createTables V3.migrateAll
  P.insert_ $ V3.DBVersion 3
  P.insert_ $ V3.DBWallet "892eb8e4"
  P.insert_ $ V3.DBWallet "a1b2c3d4"
  P.insert_ $ account btcWallet "main" 0 "btc" "/44'/0'/0'" 1 1 testAccountXPub 50000 1 fixtureTime
  P.insert_ $ account bchWallet "cash" 0 "bch" "/44'/145'/0'" 1 0 testAccountXPubBch 0 0 laterTime
  P.insert_ $ account btcWallet "savings" 1 "btc" "/44'/0'/1'" 0 0 savingsXPub 0 0 latestTime
  P.insert_ $ address btcWallet "/44'/0'/0'" 0 "/0/0" "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" "salary" 50000 False False
  P.insert_ $ address btcWallet "/44'/0'/0'" 0 "/1/0" "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn" "" 0 True True
  P.insert_ $ address bchWallet "/44'/145'/0'" 0 "/0/0" bchExternalAddrText "" 0 False False
  P.insert_ $
    V3.DBTxInfo btcWallet "/44'/0'/0'" (txHashToHex (fakeTxHash 1)) (S.encode (Store.BlockRef 800000 3)) True "{\"type\": \"credit\"}" fixtureTime
  P.insert_ $
    V3.DBCoin btcWallet "/44'/0'/0'" (encodeHex (S.encode (OutPoint (fakeTxHash 1) 0))) "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 50000 (S.encode (Store.BlockRef 800000 3)) "coin" True True fixtureTime
  P.insert_ $ V3.DBRawTx (txHashToHex (fakeTxHash 1)) "raw"
  P.insert_ $ V3.DBPendingTx btcWallet "/44'/0'/1'" (txHashToHex (fakeTxHash 5)) "{\"tx\": \"\"}" False latestTime
  P.insert_ $ V3.DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105
  commitFixture
  where
    btcWallet = V3.DBWalletKey "892eb8e4"
    bchWallet = V3.DBWalletKey "a1b2c3d4"
    account wallet name index net deriv ext int xpub confirmed coins created =
      V3.DBAccount name wallet index net deriv ext int xpub confirmed 0 coins created
    address wallet accDeriv index path addr label received internal free =
      V3.DBAddress index wallet accDeriv path addr label received 0 (if received > 0 then 1 else 0) (if received > 0 then 1 else 0) received internal free fixtureTime

-- | Every table of version 3
version3Tables :: [Text]
version3Tables = ["d_b_version", "d_b_wallet", "d_b_account", "d_b_address", "d_b_tx_info", "d_b_coin", "d_b_raw_tx", "d_b_pending_tx", "d_b_best"]

-- | The second test seed in its own wallet, 807a5cfb, with the account other
-- at m/44'/0'/0', the same path as main, created after the others: its
-- receiving address, paid 10000 by transaction 2, and that coin. Only the
-- wallet tells its rows from those of main.
anotherSeed :: DB IO ()
anotherSeed = do
  P.insert_ $ V3.DBWallet "807a5cfb"
  P.insert_ $ V3.DBAccount "other" otherWallet 0 "btc" "/44'/0'/0'" 1 0 testAccountXPub2 10000 0 1 otherTime
  P.insert_ $ V3.DBAddress 0 otherWallet "/44'/0'/0'" "/0/0" externalAddr2Text "" 10000 0 1 1 10000 False False otherTime
  P.insert_ $
    V3.DBTxInfo otherWallet "/44'/0'/0'" (txHashToHex (fakeTxHash 2)) (S.encode (Store.BlockRef 800001 1)) True "{\"type\": \"credit\"}" otherTime
  P.insert_ $
    V3.DBCoin otherWallet "/44'/0'/0'" (encodeHex (S.encode (OutPoint (fakeTxHash 2) 0))) externalAddr2Text 10000 (S.encode (Store.BlockRef 800001 1)) "coin" True False otherTime
  commitFixture
  where
    otherWallet = V3.DBWalletKey "807a5cfb"
    otherTime = UTCTime (fromGregorian 2023 11 4) 0

-- | The key of the account savings, m/44'/0'/1' of the test seed
savingsXPub :: Text
savingsXPub = testAccountXPub1

laterTime, latestTime :: UTCTime
laterTime = UTCTime (fromGregorian 2023 11 2) 0
latestTime = UTCTime (fromGregorian 2023 11 3) 0
