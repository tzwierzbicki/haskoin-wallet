{-# LANGUAGE OverloadedStrings #-}

-- | The transaction summaries of an account in the database
module Haskoin.Wallet.TxInfoSpec (spec) where

import Conduit (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import qualified Data.Serialize as S
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> describe "Transaction summaries" $ txsSpec ctx

-- | A summary that says nothing: an internal transaction of nothing
emptySummary :: TxSummary
emptySummary = TxSummary TxInternal 0 0 0 Nothing 0 Map.empty Map.empty [] Map.empty Map.empty []

-- | The empty summary of transaction 0 as txs lists it, at a block with
-- the given confirmations
listedAt :: Store.BlockRef -> Natural -> TxInfo
listedAt ref confirmations = TxInfo emptySummary (OnChain (ChainStatus (fakeTxHash 0) ref confirmations False))

-- | The row of transaction 0 of the test account
storedRow :: DB IO (Maybe DBTxInfo)
storedRow = P.get $ DBTxInfoKey (DBWalletKey testFingerprintText) "/44'/0'/0'" (txHashToHex (fakeTxHash 0))

txsSpec :: Ctx -> Spec
txsSpec ctx =
  it "can manage transactions" $ do
    inDatabaseE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- Simple insert and retrieval
      lift (repsertSummary btc accId (fakeTxHash 0) (Store.MemRef 0) emptySummary) `dbShouldBeE` True
      dbInfo <- lift storedRow >>= liftMaybe "The summary is not stored"
      liftIO $ do
        dBTxInfoAccountWallet dbInfo `shouldBe` DBWalletKey testFingerprintText
        dBTxInfoAccountDerivation dbInfo `shouldBe` "/44'/0'/0'"
        dBTxInfoBlockRef dbInfo `shouldBe` S.encode (Store.MemRef 0)
        dBTxInfoConfirmed dbInfo `shouldBe` False
      -- Reinserting should produce no change
      lift (repsertSummary btc accId (fakeTxHash 0) (Store.MemRef 0) emptySummary) `dbShouldBeE` False
      lift storedRow `dbShouldBeE` Just dbInfo
      txsPage accId (Page 5 0) `dbShouldBeE` [listedAt (Store.MemRef 0) 0]
      -- Check that confirmations are updated correctly
      lift $ updateBest btc (fakeBlockHash 0) 0
      getConfirmedTxs accId True `dbShouldBeE` []
      getConfirmedTxs accId False `dbShouldBeE` [fakeTxHash 0]
      txsPage accId (Page 5 0) `dbShouldBeE` [listedAt (Store.MemRef 0) 0]
      lift (repsertSummary btc accId (fakeTxHash 0) (Store.BlockRef 0 0) emptySummary) `dbShouldBeE` True
      dbInfo' <- lift storedRow >>= liftMaybe "The summary is not stored"
      liftIO $ do
        dBTxInfoBlockRef dbInfo' `shouldBe` S.encode (Store.BlockRef 0 0)
        dBTxInfoConfirmed dbInfo' `shouldBe` True
        dBTxInfoCreated dbInfo' `shouldBe` dBTxInfoCreated dbInfo
      txsPage accId (Page 5 0) `dbShouldBeE` [listedAt (Store.BlockRef 0 0) 1]
      getConfirmedTxs accId True `dbShouldBeE` [fakeTxHash 0]
      getConfirmedTxs accId False `dbShouldBeE` []
      lift $ updateBest btcTest (fakeBlockHash 0) 10
      lift $ updateBest btc (fakeBlockHash 0) 20
      txsPage accId (Page 5 0) `dbShouldBeE` [listedAt (Store.BlockRef 0 0) 21]
      lift (repsertSummary btc accId (fakeTxHash 0) (Store.BlockRef 10 0) emptySummary) `dbShouldBeE` True
      dbInfo'' <- lift storedRow >>= liftMaybe "The summary is not stored"
      liftIO $ do
        dBTxInfoBlockRef dbInfo'' `shouldBe` S.encode (Store.BlockRef 10 0)
        dBTxInfoConfirmed dbInfo'' `shouldBe` True
      txsPage accId (Page 5 0) `dbShouldBeE` [listedAt (Store.BlockRef 10 0) 11]
