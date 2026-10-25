{-# LANGUAGE OverloadedStrings #-}

-- | The best block of each network, as the database keeps it from one sync
-- to the next, and the confirmations counted from it: those of the
-- transactions txs lists, and those of the coins coins lists and the
-- builder spends.
module Haskoin.Wallet.BestBlockSpec (spec) where

import Control.Monad.Trans (lift)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = do
  storedSpec
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

{- The best block in the database -}

storedSpec :: Spec
storedSpec =
  describe "The best block of a network" $ do
    it "is unknown before the first sync" $
      inDatabaseE $
        getBest btc `dbShouldBeE` Nothing

    it "is read back as it was stored" $
      inDatabaseE $ do
        lift $ updateBest btc bitcoinBlock 814037
        getBest btc `dbShouldBeE` Just (bitcoinBlock, 814037)

    it "gives its height alone, which the confirmations are counted from" $
      inDatabaseE $ do
        bestHeight btc `dbShouldBeE` Nothing
        lift $ updateBest btc bitcoinBlock 814037
        bestHeight btc `dbShouldBeE` Just 814037

    it "is replaced by the next one, a single row for the network" $
      inDatabaseE $ do
        lift $ updateBest btc bitcoinBlock 814037
        lift $ updateBest btc nextBitcoinBlock 814038
        getBest btc `dbShouldBeE` Just (nextBitcoinBlock, 814038)
        lift (P.count ([] :: [P.Filter DBBest])) `dbShouldBeE` 1

    it "is kept for each network apart" $
      inDatabaseE $ do
        lift $ updateBest btc bitcoinBlock 814037
        lift $ updateBest btcTest testnetBlock 2535443
        getBest btc `dbShouldBeE` Just (bitcoinBlock, 814037)
        getBest btcTest `dbShouldBeE` Just (testnetBlock, 2535443)
        getBest bch `dbShouldBeE` Nothing

    it "is stored under the name of its network, with the hash as it is displayed" $
      inDatabase $ do
        updateBest btc bitcoinBlock 814037
        tableRows "d_b_best"
          `dbShouldBe` [[PersistText "btc", PersistText "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb", PersistInt64 814037]]

    it "is refused when its stored hash does not read, rather than taken for no block" $
      inDatabase $ do
        P.insert_ $ DBBest "btc" "nonsense" 814037
        dbShouldFailWith "The stored best block of btc does not read: sync --full stores it again" $ getBest btc

    it "is refused when its stored height is no block height" $
      inDatabase $ do
        P.insert_ $ DBBest "btc" "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb" (-1)
        dbShouldFailWith "The stored best block of btc does not read: sync --full stores it again" $ getBest btc

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
