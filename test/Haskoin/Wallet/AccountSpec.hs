{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

-- | The accounts of the wallet in the database
module Haskoin.Wallet.AccountSpec (spec) where

import Conduit (liftIO)
import Control.Monad.Trans (lift)
import Haskoin
import Haskoin.Wallet.Account
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> describe "Accounts" $ accountSpec ctx

accountSpec :: Ctx -> Spec
accountSpec ctx = do
  it "can find the correct account derivations with smallestUnused" $ do
    smallestUnused [] `shouldBe` 0
    smallestUnused [0] `shouldBe` 1
    smallestUnused [1] `shouldBe` 0
    smallestUnused [2] `shouldBe` 0
    smallestUnused [1,2,3] `shouldBe` 0
    smallestUnused [1,3,4] `shouldBe` 0
    smallestUnused [0,2] `shouldBe` 1
    smallestUnused [0,1,3,4] `shouldBe` 2
    smallestUnused [0,1,3,4,6,7] `shouldBe` 2
    smallestUnused [0,1,2,3,4,5] `shouldBe` 6
  it "can create and rename accounts" $ do
    inDatabaseE $ do
      -- No accounts
      lift $ dbShouldFail $ getAccountByName $ Just "acc1"
      lift $ dbShouldFail $ getAccountByName Nothing
      lift $ nextAccountDeriv testFingerprint btc `dbShouldBe` 0
      lift $ nextAccountDeriv testFingerprint btc `dbShouldBe` 0 -- Still 0
      lift $ getAccounts `dbShouldBe` []
      -- Check basic account properties
      (accId, acc) <- testNewAcc ctx "acc1"
      lift $ nextAccountDeriv testFingerprint btc `dbShouldBe` 1
      lift $ nextAccountDeriv testFingerprint btc `dbShouldBe` 1 -- Still 1
      liftIO $ do
        dBAccountName acc `shouldBe` "acc1"
        dBAccountWallet acc `shouldBe` DBWalletKey testFingerprintText
        dBAccountIndex acc `shouldBe` 0
        dBAccountNetwork acc `shouldBe` "btc"
        dBAccountDerivation acc `shouldBe` "/44'/0'/0'"
        dBAccountExternal acc `shouldBe` 0
        dBAccountInternal acc `shouldBe` 0
        dBAccountXPubKey acc `shouldBe` testAccountXPub
        dBAccountBalanceConfirmed acc `shouldBe` 0
        dBAccountBalanceUnconfirmed acc `shouldBe` 0
        dBAccountBalanceCoins acc `shouldBe` 0
      -- Check account retrieval
      lift $ getAccounts `dbShouldBe` [(accId, acc)]
      getAccountByName (Just "acc1") `dbShouldBeE` (accId, acc)
      getAccountByName Nothing `dbShouldBeE` (accId, acc) -- There is just 1 account
      getAccountById accId `dbShouldBeE` acc
      lift $ getAccounts `dbShouldBe` [(accId, acc)]
      -- Check the creation of a second account
      (accId2, acc2) <- testNewAcc ctx "acc2"
      lift $ nextAccountDeriv testFingerprint btc `dbShouldBe` 2
      getAccountByName (Just "acc2") `dbShouldBeE` (accId2, acc2)
      lift $ getAccounts `dbShouldBe` [(accId, acc), (accId2, acc2)]
      lift $ dbShouldFail $ getAccountByName Nothing -- There are > 1 accounts
      liftIO $ do
        dBAccountWallet acc2 `shouldBe` DBWalletKey testFingerprintText
        dBAccountDerivation acc2 `shouldBe` "/44'/0'/1'"
        dBAccountIndex acc2 `shouldBe` 1
      -- Create an account in a new wallet
      (accId3, acc3) <- testNewAcc2 ctx "acc3"
      liftIO $ do
        dBAccountName acc3 `shouldBe` "acc3"
        dBAccountWallet acc3 `shouldBe` DBWalletKey testFingerprintText2
        dBAccountIndex acc3 `shouldBe` 0
        dBAccountNetwork acc3 `shouldBe` "btc"
        dBAccountDerivation acc3 `shouldBe` "/44'/0'/0'"
        dBAccountXPubKey acc3 `shouldBe` testAccountXPub2
      lift $ nextAccountDeriv testFingerprint2 btc `dbShouldBe` 1
      lift $
        getAccounts `dbShouldBe` [(accId3, acc3), (accId, acc), (accId2, acc2)]
      getAccountByName (Just "acc3") `dbShouldBeE` (accId3, acc3)
      lift $ dbShouldFail $ getAccountByName Nothing -- There are > 1 accounts
      getAccountById accId3 `dbShouldBeE` acc3
      -- Rename an account
      lift $ dbShouldFail $ renameAccount "acc2" "acc2"
      lift $ dbShouldFail $ renameAccount "acc2" "acc3"
      lift $ dbShouldFail $ renameAccount "doesnotexist" "hello world"
      acc2' <- renameAccount "acc2" "hello world"
      liftIO $ acc2' `shouldBe` acc2 {dBAccountName = "hello world"}
      lift $ getAccountNames `dbShouldBe` ["acc1", "hello world", "acc3"]
      _ <- renameAccount "hello world" "acc2"
      lift $ getAccountNames `dbShouldBe` ["acc1", "acc2", "acc3"]
