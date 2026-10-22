{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

-- | The coins of an account in the database
module Haskoin.Wallet.CoinSpec (spec) where

import Conduit (liftIO)
import Control.Arrow (second)
import Control.Monad.Trans (lift)
import qualified Data.Set as Set
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Util
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> describe "Coins" $ coinSpec ctx

coinSpec :: Ctx -> Spec
coinSpec ctx =
  it "can manage coins" $ do
    inDatabaseE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- Insert a single coin
      let coin1 = storeCoin ctx (fakeTxHash 0, 0) Nothing (head externalAddrs) 10
      (c, dbCoin) <- second head <$> refreshCoins btc accId externalAddrs [coin1]
      liftIO $ do
        c `shouldBe` 1
        dBCoinAccountWallet dbCoin `shouldBe` DBWalletKey testFingerprintText
        dBCoinAccountDerivation dbCoin `shouldBe` "/44'/0'/0'"
        dBCoinAddress dbCoin `shouldBe` head externalAddrsText
        dBCoinConfirmed dbCoin `shouldBe` False
      -- Test JSON coins
      let jsonCoin1 = forceRight $ toJsonCoin btc Nothing Set.empty dbCoin
      liftIO $
        jsonCoin1
          `shouldBe` JsonCoin
            { jsonCoinOutpoint = OutPoint (fakeTxHash 0) 0,
              jsonCoinAddress = head externalAddrs,
              jsonCoinValue = 10,
              jsonCoinBlock = Store.MemRef 0,
              jsonCoinConfirmations = 0,
              jsonCoinLocked = False
            }
      coinPage btc accId (Page 5 0) `dbShouldBeE` [jsonCoin1]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1]
      getSpendableCoins btc accId 1 `dbShouldBeE` []
      -- Nothing should happen when refreshing the same data
      refreshCoins btc accId externalAddrs [coin1] `dbShouldBeE` (0, [])
      -- Confirm and update the coin
      let coin1' = coin1 {Store.block = Store.BlockRef 1 0} :: Store.Unspent
      refreshCoins btc accId externalAddrs [coin1'] `dbShouldBeE` (1, [])
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 1 0,
                            jsonCoinConfirmations = 1
                          }
                      ]
      -- Best = 0
      lift $ updateBest btc (fakeBlockHash 0) 0
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 1 0,
                            jsonCoinConfirmations = 1
                          }
                      ]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      -- Best = 1
      lift $ updateBest btc (fakeBlockHash 0) 1
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 1 0,
                            jsonCoinConfirmations = 1
                          }
                      ]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      -- Best = 10
      lift $ updateBest btc (fakeBlockHash 0) 10
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 1 0,
                            jsonCoinConfirmations = 10
                          }
                      ]
      getSpendableCoins btc accId 9 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 10 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 11 `dbShouldBeE` []
      let coin1'' = coin1 {Store.block = Store.BlockRef 10 0} :: Store.Unspent
      refreshCoins btc accId externalAddrs [coin1''] `dbShouldBeE` (1, [])
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 10 0,
                            jsonCoinConfirmations = 1
                          }
                      ]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1'']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1'']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      -- Delete the coin
      refreshCoins btc accId externalAddrs [] `dbShouldBeE` (1, [])
      getSpendableCoins btc accId 0 `dbShouldBeE` []
      coinPage btc accId (Page 5 0) `dbShouldBeE` []
