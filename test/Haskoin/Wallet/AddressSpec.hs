{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

-- | The external and internal addresses of an account in the database
module Haskoin.Wallet.AddressSpec (spec) where

import Conduit (liftIO)
import Control.Monad
import Control.Monad.Trans (lift)
import qualified Data.Map.Strict as Map
import qualified Database.Persist as P
import Data.Default (def)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Util
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx ->
    describe "Addresses" $ do
      let cfg = def :: Config
      extAddressSpec ctx cfg
      intAddressSpec ctx cfg
      holeSpec ctx cfg

extAddressSpec :: Ctx -> Config -> Spec
extAddressSpec ctx cfg =
  it "can generate external addresses" $ do
    inDatabaseE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- No addresses yet
      lift $ highestUsedIndex accId AddrInternal `dbShouldBe` Nothing
      lift $ highestUsedIndex accId AddrExternal `dbShouldBe` Nothing
      lift $ addressPage accId AddrExternal (Page 5 0) `dbShouldBe` []
      -- Generate external addresses
      ext1 <- genExtAddress ctx cfg accId "Address 1"
      liftIO $ do
        dBAddressIndex ext1 `shouldBe` 0
        dBAddressAccountWallet ext1 `shouldBe` DBWalletKey testFingerprintText
        dBAddressAccountDerivation ext1 `shouldBe` "/44'/0'/0'"
        dBAddressDerivation ext1 `shouldBe` "/0/0"
        dBAddressAddress ext1 `shouldBe` head externalAddrsText
        dBAddressLabel ext1 `shouldBe` "Address 1"
        dBAddressInternal ext1 `shouldBe` False
      lift $ addressPage accId AddrExternal (Page 5 0) `dbShouldBe` [ext1]
      ext2 <- genExtAddress ctx cfg accId "Address 2"
      liftIO $ do
        dBAddressIndex ext2 `shouldBe` 1
        dBAddressAccountWallet ext2 `shouldBe` DBWalletKey testFingerprintText
        dBAddressAccountDerivation ext2 `shouldBe` "/44'/0'/0'"
        dBAddressDerivation ext2 `shouldBe` "/0/1"
        dBAddressAddress ext2 `shouldBe` externalAddrsText !! 1
        dBAddressLabel ext2 `shouldBe` "Address 2"
        dBAddressInternal ext2 `shouldBe` False
      -- Test Paging
      lift $ addressPage accId AddrExternal (Page 5 0) `dbShouldBe` [ext2, ext1]
      lift $ addressPage accId AddrExternal (Page 1 0) `dbShouldBe` [ext2]
      lift $ addressPage accId AddrExternal (Page 1 1) `dbShouldBe` [ext1]
      lift $ addressPage accId AddrExternal (Page 1 2) `dbShouldBe` []
      -- Set address labels
      _ <- setAddrLabel accId 0 "test address"
      lift $
        addressPage accId AddrExternal (Page 5 0)
          `dbShouldBe` [ext2, ext1 {dBAddressLabel = "test address"}]
      -- Test the gap
      replicateM_ 18 $ genExtAddress ctx cfg accId "" -- We have 20 addresses
      lift $ dbShouldFail $ genExtAddress ctx cfg accId "" -- fail gap
      lift $ addressPage accId AddrExternal (Page 100 0) `dbShouldSatisfy` ((== 20) . length)
      updateAddressBalances btc [Store.Balance (externalAddrs !! 2) 0 0 0 1 1]
      updateAddressBalances btc [Store.Balance (externalAddrs !! 4) 0 0 0 1 1]
      lift $ highestUsedIndex accId AddrExternal `dbShouldBe` Just 4
      replicateM_ 5 $ genExtAddress ctx cfg accId "" -- We have 25 addresses
      lift $ dbShouldFail $ genExtAddress ctx cfg accId "" -- fail gap
      lift $ addressPage accId AddrExternal (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      -- Test discoverAccGenAddrs
      updateAddressBalances btc [Store.Balance (externalAddrs !! 9) 0 0 0 1 1]
      extendChainTo ctx accId AddrExternal 0
      lift $ addressPage accId AddrExternal (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      extendChainTo ctx accId AddrExternal 25
      lift $ addressPage accId AddrExternal (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      extendChainTo ctx accId AddrExternal 26
      lift $ addressPage accId AddrExternal (Page 100 0) `dbShouldSatisfy` ((== 26) . length)
      -- Test getAddrDeriv
      addressPath btc accId (externalAddr 0) `dbShouldBeE` Just (Deriv :/ 0 :/ 0)
      addressPath btc accId (externalAddr 7) `dbShouldBeE` Just (Deriv :/ 0 :/ 7)
      -- Test accound balances
      acc1 <- lift $ updateAccountBalances accId
      liftIO $ do
        dBAccountBalanceConfirmed acc1 `shouldBe` 0
        dBAccountBalanceUnconfirmed acc1 `shouldBe` 0
        dBAccountBalanceCoins acc1 `shouldBe` 0
      updateAddressBalances btc [Store.Balance (externalAddrs !! 1) 2 0 1 1 2]
      updateAddressBalances btc [Store.Balance (externalAddrs !! 4) 2 8 2 2 10]
      _ <- nextChangeAddress ctx cfg accId []
      updateAddressBalances btc [Store.Balance (head internalAddrs) 3 2 1 1 5]
      acc1' <- lift $ updateAccountBalances accId
      liftIO $ do
        dBAccountBalanceConfirmed acc1' `shouldBe` 7
        dBAccountBalanceUnconfirmed acc1' `shouldBe` 10
        dBAccountBalanceCoins acc1' `shouldBe` 4
      -- Test account indices
      liftIO $ do
        dBAccountInternal acc1' `shouldBe` 1
        dBAccountExternal acc1' `shouldBe` 26

intAddressSpec :: Ctx -> Config -> Spec
intAddressSpec ctx cfg =
  it "can generate internal addresses" $ do
    inDatabaseE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- The first change address, generated when there is none
      int1 <- nextChangeAddress ctx cfg accId []
      liftIO $ do
        dBAddressIndex int1 `shouldBe` 0
        dBAddressAccountWallet int1 `shouldBe` DBWalletKey testFingerprintText
        dBAddressAccountDerivation int1 `shouldBe` "/44'/0'/0'"
        dBAddressDerivation int1 `shouldBe` "/1/0"
        dBAddressAddress int1 `shouldBe` internalAddrsText !! 0
        dBAddressLabel int1 `shouldBe` ""
        dBAddressInternal int1 `shouldBe` True
      -- The same address while nothing uses it; the next one while a
      -- pending transaction pays it
      nextChangeAddress ctx cfg accId [] `dbShouldBeE` int1
      (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0]) `dbShouldBeE` 1
      -- An address paid on the chain is never used again
      updateAddressBalances btc [Store.Balance (internalAddr 0) 1000 0 1 1 1000]
      (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1
      (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 1]) `dbShouldBeE` 2
      -- The lowest available address comes first
      (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 2]) `dbShouldBeE` 1
      acc <- getAccountById accId
      liftIO $ do
        dBAccountInternal acc `shouldBe` 3
        dBAccountExternal acc `shouldBe` 0

-- A chain with a missing row, as BIP32 leaves one where a child key is
-- invalid: the count moved on, nothing else happened at that index
holeSpec :: Ctx -> Config -> Spec
holeSpec ctx cfg =
  it "lives with a hole in a chain, as an invalid child key leaves one" $
    inDatabaseE $ do
      (accId@(DBAccountKey wallet accDeriv), _) <- testNewAcc ctx "test"
      replicateM_ 3 $ genExtAddress ctx cfg accId ""
      _ <- nextChangeAddress ctx cfg accId []
      _ <- nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0]
      _ <- nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0, Deriv :/ 1 :/ 1]
      lift $ P.delete $ DBAddressKey wallet accDeriv "/0/1"
      lift $ P.delete $ DBAddressKey wallet accDeriv "/1/1"
      (map dBAddressIndex <$> lift (addressPage accId AddrExternal (Page 5 0))) `dbShouldBeE` [2, 0]
      lift $ dbShouldFailWith "Address 1 does not exist" $ setAddrLabel accId 1 "gone"
      (dBAddressIndex <$> genExtAddress ctx cfg accId "") `dbShouldBeE` 3
      (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0]) `dbShouldBeE` 2
      (Map.size . fst <$> allAddressesMap btc accId) `dbShouldBeE` 5
