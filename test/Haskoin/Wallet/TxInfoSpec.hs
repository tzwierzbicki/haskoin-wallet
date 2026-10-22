{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

-- | The transaction summaries of an account in the database
module Haskoin.Wallet.TxInfoSpec (spec) where

import Conduit (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import qualified Data.Serialize as S
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> describe "Transaction summaries" $ txsSpec ctx

emptyTxInfo :: TxInfo
emptyTxInfo =
  TxInfo
    (Just $ fakeTxHash 0)
    TxInternal
    0
    Map.empty
    Map.empty
    []
    Map.empty
    Map.empty
    []
    0
    0
    0
    Nothing 
    (Store.MemRef 0)
    0
    Nothing
    False

txsSpec :: Ctx -> Spec
txsSpec ctx =
  it "can manage transactions" $ do
    inDatabaseE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- Simple insert and retrieval
      (dbInfo, change) <- repsertTxInfo btc ctx accId emptyTxInfo
      liftIO $ do
        change `shouldBe` True
        dBTxInfoAccountWallet dbInfo `shouldBe` DBWalletKey testFingerprintText
        dBTxInfoAccountDerivation dbInfo `shouldBe` "/44'/0'/0'"
        dBTxInfoBlockRef dbInfo `shouldBe` S.encode (Store.MemRef 0)
        dBTxInfoConfirmed dbInfo `shouldBe` False
      -- Reinserting should produce no change
      (dbInfo2, change2) <- repsertTxInfo btc ctx accId emptyTxInfo
      liftIO $ do
        change2 `shouldBe` False
        dbInfo2 `shouldBe` dbInfo
      txsPage ctx accId (Page 5 0) `dbShouldBeE` [emptyTxInfo]
      -- Check that confirmations are updated correctly
      lift $ updateBest btc (fakeBlockHash 0) 0
      getConfirmedTxs accId True `dbShouldBeE` []
      getConfirmedTxs accId False `dbShouldBeE` [fakeTxHash 0]
      txsPage ctx accId (Page 5 0) `dbShouldBeE` [emptyTxInfo]
      (dbInfo', change') <-
        repsertTxInfo
          btc
          ctx
          accId
          emptyTxInfo {txInfoBlockRef = Store.BlockRef 0 0}
      liftIO $ do
        change' `shouldBe` True
        dBTxInfoBlockRef dbInfo' `shouldBe` S.encode (Store.BlockRef 0 0)
        dBTxInfoConfirmed dbInfo' `shouldBe` True
        dBTxInfoCreated dbInfo' `shouldBe` dBTxInfoCreated dbInfo
      txsPage ctx accId (Page 5 0)
        `dbShouldBeE` [ emptyTxInfo
                          { txInfoBlockRef = Store.BlockRef 0 0,
                            txInfoConfirmations = 1
                          }
                      ]
      getConfirmedTxs accId True `dbShouldBeE` [fakeTxHash 0]
      getConfirmedTxs accId False `dbShouldBeE` []
      lift $ updateBest btcTest (fakeBlockHash 0) 10
      lift $ updateBest btc (fakeBlockHash 0) 20
      txsPage ctx accId (Page 5 0)
        `dbShouldBeE` [ emptyTxInfo
                          { txInfoBlockRef = Store.BlockRef 0 0,
                            txInfoConfirmations = 21
                          }
                      ]
      (dbInfo'', change'') <-
        repsertTxInfo
          btc
          ctx
          accId
          emptyTxInfo {txInfoBlockRef = Store.BlockRef 10 0}
      liftIO $ do
        change'' `shouldBe` True
        dBTxInfoBlockRef dbInfo'' `shouldBe` S.encode (Store.BlockRef 10 0)
        dBTxInfoConfirmed dbInfo'' `shouldBe` True
      txsPage ctx accId (Page 5 0)
        `dbShouldBeE` [ emptyTxInfo
                          { txInfoBlockRef = Store.BlockRef 10 0,
                            txInfoConfirmations = 11
                          }
                      ]
