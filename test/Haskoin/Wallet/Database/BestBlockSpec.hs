{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The best block of each network, as the database keeps it from one sync
-- to the next, and the confirmations counted from it: those of the
-- transactions txs lists, and those of the coins coins lists and the
-- builder spends. Also the block the last sync of each pool reached, a
-- portfolio on a network, which the best block of the network cannot
-- stand for: each pool is synced apart. And the settled block stored with
-- it, a hundred blocks below.
module Haskoin.Wallet.Database.BestBlockSpec (spec) where

import Control.Monad.Trans (lift)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio (portfolioPool)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Testnet3 (btcTestnet3)
import Test.Hspec

spec :: Spec
spec = do
  storedSpec
  syncedSpec
  settledSpec
  confirmationsSpec

{- Fixtures -}

-- | The hash of the Bitcoin best block, stored at height 814037
bitcoinBlock :: BlockHash
bitcoinBlock = "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb"

-- | The next best block of Bitcoin, stored at height 814038: a hash made
-- up for the tests, which the wallet stores without checking it
nextBitcoinBlock :: BlockHash
nextBitcoinBlock = fakeBlockHash 2

-- | The hash of the Bitcoin testnet best block, stored at height 2535443
testnetBlock :: BlockHash
testnetBlock = "000000000000001400d91785c92efc15ccfc1949e8581f06db7914a707c3f81d"

-- | The Bitcoin block a sync reached
reachedBitcoin :: SyncedBlock
reachedBitcoin = SyncedBlock bitcoinBlock 814037

-- | The settled block stored with it, a hundred blocks below: its hash is
-- that of another block, which the wallet stores without checking it
settledBitcoin :: SyncedBlock
settledBitcoin = SyncedBlock "000000000003ba27aa200b1cecaad478d2b00432346c3f1f3986da1afd33e506" 813937

{- The best block in the database -}

storedSpec :: Spec
storedSpec =
  describe "The best block of a network" $ do
    it "is unknown before the first sync" $
      inDatabaseE $
        getBest btc `dbShouldBeE` Nothing

    -- Block 814037 was mined on 2023-10-28 at 19:47:43 UTC
    it "is read back as it was stored, with the time its header gives" $
      inDatabaseE $ do
        lift $ updateBest btc bitcoinBlock 814037 1698522463
        getBest btc `dbShouldBeE` Just (BestBlock bitcoinBlock 814037 (Just 1698522463))

    it "gives its height alone, which the confirmations are counted from" $
      inDatabaseE $ do
        bestHeight btc `dbShouldBeE` Nothing
        lift $ updateBest btc bitcoinBlock 814037 1698522463
        bestHeight btc `dbShouldBeE` Just 814037

    it "is replaced by the next one, a single row for the network" $
      inDatabaseE $ do
        lift $ updateBest btc bitcoinBlock 814037 1698522463
        lift $ updateBest btc nextBitcoinBlock 814038 1698523063
        getBest btc `dbShouldBeE` Just (BestBlock nextBitcoinBlock 814038 (Just 1698523063))
        lift (P.count ([] :: [P.Filter DBBest])) `dbShouldBeE` 1

    it "is kept for each network apart" $
      inDatabaseE $ do
        lift $ updateBest btc bitcoinBlock 814037 1698522463
        lift $ updateBest btcTestnet3 testnetBlock 2535443 1698522000
        getBest btc `dbShouldBeE` Just (BestBlock bitcoinBlock 814037 (Just 1698522463))
        getBest btcTestnet3 `dbShouldBeE` Just (BestBlock testnetBlock 2535443 (Just 1698522000))
        getBest bch `dbShouldBeE` Nothing

    it "is stored under the name of its network, with the hash as it is displayed and the time in seconds" $
      inDatabase $ do
        updateBest btc bitcoinBlock 814037 1698522463
        tableRows "d_b_best"
          `dbShouldBe` [[PersistText "btc", PersistText "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb", PersistInt64 814037, PersistInt64 1698522463]]

    -- The hw of before 2026-11 kept no time: the next sync stores one
    it "reads a block stored without its time, by an older hw, as a block whose time is unknown" $
      inDatabaseE $ do
        lift $ P.insert_ $ DBBest "btc" "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb" 814037 Nothing
        getBest btc `dbShouldBeE` Just (BestBlock bitcoinBlock 814037 Nothing)

    it "is refused when its stored hash does not read, rather than taken for no block" $
      inDatabase $ do
        P.insert_ $ DBBest "btc" "nonsense" 814037 Nothing
        dbShouldFailWith "The stored best block of btc does not read: sync stores it again" $ getBest btc

    it "is refused when its stored height is no block height" $
      inDatabase $ do
        P.insert_ $ DBBest "btc" "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb" (-1) Nothing
        dbShouldFailWith "The stored best block of btc does not read: sync stores it again" $ getBest btc

    it "is refused when its stored time is no Unix time" $
      inDatabase $ do
        P.insert_ $ DBBest "btc" "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb" 814037 (Just (-1))
        dbShouldFailWith "The stored best block of btc does not read: sync stores it again" $ getBest btc

{- The block of a pool -}

-- The pool is what portfolio 0 of the test wallet holds on Bitcoin
syncedSpec :: Spec
syncedSpec =
  describe "The block the last sync of a pool reached" $ do
    it "is unknown before the first sync of the pool" $
      inDatabaseE $ do
        pool <- testPool
        syncedBlock pool `dbShouldBeE` Nothing

    it "is read back as it was stored" $
      inDatabaseE $ do
        pool <- testPool
        lift $ updateSyncedBlock pool reachedBitcoin settledBitcoin
        syncedBlock pool `dbShouldBeE` Just (SyncedBlock bitcoinBlock 814037)

    it "is replaced by the next one, a single row for the pool" $
      inDatabaseE $ do
        pool <- testPool
        lift $ updateSyncedBlock pool reachedBitcoin settledBitcoin
        lift $ updateSyncedBlock pool (SyncedBlock nextBitcoinBlock 814038) (SyncedBlock (fakeBlockHash 3) 813938)
        syncedBlock pool `dbShouldBeE` Just (SyncedBlock nextBitcoinBlock 814038)
        lift (P.count ([] :: [P.Filter DBSyncedBlock])) `dbShouldBeE` 1

    it "is kept apart for each network of a portfolio" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        lift $ updateSyncedBlock (portfolioPool portfolio btc) reachedBitcoin settledBitcoin
        lift $ updateSyncedBlock (portfolioPool portfolio btcTestnet3) (SyncedBlock testnetBlock 2535443) (SyncedBlock (fakeBlockHash 4) 2535343)
        syncedBlock (portfolioPool portfolio btc) `dbShouldBeE` Just (SyncedBlock bitcoinBlock 814037)
        syncedBlock (portfolioPool portfolio btcTestnet3) `dbShouldBeE` Just (SyncedBlock testnetBlock 2535443)
        syncedBlock (portfolioPool portfolio bch) `dbShouldBeE` Nothing

    -- Portfolio 1 of the wallet has not synced yet: the block portfolio 0
    -- reached says nothing of it
    it "is kept apart for each portfolio on a network" $
      inDatabaseE $ do
        first <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        lift $ updateSyncedBlock (portfolioPool first btc) reachedBitcoin settledBitcoin
        syncedBlock (portfolioPool first btc) `dbShouldBeE` Just (SyncedBlock bitcoinBlock 814037)
        syncedBlock (portfolioPool second btc) `dbShouldBeE` Nothing

    it "tells that the pool was never synced until a sync stores it" $
      inDatabaseE $ do
        pool <- testPool
        lift (poolEverSynced pool) `dbShouldBeE` False
        lift $ updateSyncedBlock pool reachedBitcoin settledBitcoin
        lift (poolEverSynced pool) `dbShouldBeE` True

    -- Portfolio 1 of the wallet, and the same portfolio on Bitcoin Cash,
    -- have not synced
    it "tells so for its own pool only, not for another portfolio or another network" $
      inDatabaseE $ do
        first <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        lift $ updateSyncedBlock (portfolioPool first btc) reachedBitcoin settledBitcoin
        lift (poolEverSynced (portfolioPool second btc)) `dbShouldBeE` False
        lift (poolEverSynced (portfolioPool first bch)) `dbShouldBeE` False

    -- The best block of the network is one row for every portfolio
    it "does not move with the best block of its network" $
      inDatabaseE $ do
        pool <- testPool
        lift $ updateSyncedBlock pool reachedBitcoin settledBitcoin
        lift $ updateBest btc nextBitcoinBlock 814038 1698523063
        syncedBlock pool `dbShouldBeE` Just (SyncedBlock bitcoinBlock 814037)

    it "is stored under its portfolio and the name of its network, with the hash as it is displayed" $
      inDatabaseE $ do
        pool <- testPool
        lift $ updateSyncedBlock pool reachedBitcoin settledBitcoin
        lift (tableRows "d_b_synced_block")
          `dbShouldBeE` [ [ PersistInt64 1,
                            PersistText "btc",
                            PersistText "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb",
                            PersistInt64 814037,
                            PersistText "000000000003ba27aa200b1cecaad478d2b00432346c3f1f3986da1afd33e506",
                            PersistInt64 813937
                          ]
                        ]

{- The settled block of a pool -}

settledSpec :: Spec
settledSpec =
  describe "The settled block of a pool, a hundred blocks below the one its last sync reached" $ do
    it "is unknown before the first sync of the pool" $
      inDatabaseE $ do
        pool <- testPool
        settledBlock pool `dbShouldBeE` Nothing

    it "is read back as it was stored with the block the sync reached" $
      inDatabaseE $ do
        pool <- testPool
        lift $ updateSyncedBlock pool reachedBitcoin settledBitcoin
        settledBlock pool `dbShouldBeE` Just (SyncedBlock "000000000003ba27aa200b1cecaad478d2b00432346c3f1f3986da1afd33e506" 813937)

    it "is replaced by the next one with the block the next sync reached" $
      inDatabaseE $ do
        pool <- testPool
        lift $ updateSyncedBlock pool reachedBitcoin settledBitcoin
        lift $ updateSyncedBlock pool (SyncedBlock nextBitcoinBlock 814038) (SyncedBlock (fakeBlockHash 3) 813938)
        settledBlock pool `dbShouldBeE` Just (SyncedBlock (fakeBlockHash 3) 813938)

    it "is refused when its stored height is no height" $
      inDatabaseE $ do
        pool <- testPool
        lift $ updateSyncedBlock pool reachedBitcoin settledBitcoin
        lift $ P.update (DBSyncedBlockKey pool.poolPortfolio (poolNetworkName pool)) [DBSyncedBlockSettledHeight P.=. (-1)]
        lift $
          dbShouldFailWith "The block the last sync of the portfolio reached does not read: sync --full stores it again" $
            settledBlock pool

    -- 814037 - 100 = 813937
    it "stands a hundred blocks below the block a sync reaches" $
      settledHeightBelow 814037 `shouldBe` 813937

    -- A chain of 101 blocks has the heights 0 to 100
    it "is the first block of a chain that has a hundred blocks or fewer above it" $
      (settledHeightBelow <$> [0, 40, 100, 101]) `shouldBe` [0, 0, 0, 1]

{- Confirmations -}

confirmationsSpec :: Spec
confirmationsSpec =
  describe "The confirmations of a transaction or a coin" $ do
    it "are none in the mempool, whatever the best block" $ do
      getConfirmations (Just 800002) (Store.MemRef 1700000000) `shouldBe` 0
      getConfirmations Nothing (Store.MemRef 1700000000) `shouldBe` 0

    it "are one in the best block itself" $
      getConfirmations (Just 800000) (Store.BlockRef 800000 3) `shouldBe` 1

    it "count the blocks from the transaction's to the best one, both included" $ do
      getConfirmations (Just 800002) (Store.BlockRef 800000 3) `shouldBe` 3
      getConfirmations (Just 800099) (Store.BlockRef 800000 3) `shouldBe` 100

    it "count up to the highest heights without overflowing" $
      getConfirmations (Just 4294967295) (Store.BlockRef 0 0) `shouldBe` 4294967296

    it "are one while the best block is unknown, before the first sync" $
      getConfirmations Nothing (Store.BlockRef 800000 3) `shouldBe` 1

    it "are one while the best block is behind the transaction's, until the next sync" $
      getConfirmations (Just 799990) (Store.BlockRef 800000 3) `shouldBe` 1

    it "do not depend on the position of the transaction in its block" $
      getConfirmations (Just 800002) (Store.BlockRef 800000 0) `shouldBe` getConfirmations (Just 800002) (Store.BlockRef 800000 2500)
