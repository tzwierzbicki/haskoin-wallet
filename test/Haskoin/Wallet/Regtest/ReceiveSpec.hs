{-# LANGUAGE OverloadedStrings #-}

-- | Payments received on live regtest chains: the addresses hw gives out,
-- and what it shows of a payment before and after its block. See
-- "Haskoin.Wallet.RegtestUtils" for the chains.
module Haskoin.Wallet.Regtest.ReceiveSpec (spec) where

import Control.Monad (forM_)
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Haskoin.Wallet.Workflow.TestUtils (answer)
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $ do
  -- The addresses are those Bitcoin Core derives from the test mnemonic
  -- and its passphrase, with deriveaddresses, at m/44'/1'/0'/0/0 and
  -- m/84'/1'/0'/0/0. The address of bchreg is the first one, as Bitcoin
  -- Cash Node writes it.
  describe "The first address of a portfolio on a regtest network" $ do
    it "is the legacy address Bitcoin Core derives from the mnemonic, on btcreg" $
      onChains $ \chains ->
        withWallet chains testMnemonic $ \home ->
          receiveOn home Btc "bip44" `shouldReturn` "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"

    it "is the native SegWit address Bitcoin Core derives from the mnemonic, on btcreg" $
      onChains $ \chains ->
        withWallet chains testMnemonic $ \home ->
          receiveOn home Btc "bip84" `shouldReturn` "bcrt1qjngk945r5gv9pk0ur7uqz9c6ntngkmtv56cnc2"

    it "is the same key as on btcreg, written as Bitcoin Cash Node writes it, on bchreg" $
      onChains $ \chains ->
        withWallet chains testMnemonic $ \home ->
          receiveOn home Bch "bip44" `shouldReturn` "bchreg:qqfkav00kdf7d3njjawa4dn0z8kzlfqdfgcn86qasd"

  forM_ [Btc, Bch] $ \chain ->
    describe ("A payment of 100000 satoshis received on " <> networkOf chain) $ do
      -- The sender can still replace it
      it "is unconfirmed and cannot be spent before it has a block" $
        onChains $ \chains ->
          withWallet chains testMnemonic $ \home -> do
            mine chains chain 101
            address <- receiveOn home chain "bip44"
            _ <- payFromNode chains chain address 100000
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxCredit, 100000, Just 0)]
            coinsOn home chain `shouldReturn` [(100000, 0, False)]

      it "has one confirmation and can be spent once it has a block" $
        onChains $ \chains ->
          withWallet chains testMnemonic $ \home -> do
            mine chains chain 101
            address <- receiveOn home chain "bip44"
            _ <- payFromNode chains chain address 100000
            mine chains chain 1
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxCredit, 100000, Just 1)]
            coinsOn home chain `shouldReturn` [(100000, 1, True)]

  describe "Payments received on btcreg at both types of address" $ do
    it "are both coins of the portfolio, the native SegWit one and the legacy one" $
      onChains $ \chains ->
        withWallet chains testMnemonic $ \home -> do
          mine chains Btc 101
          legacy <- receiveOn home Btc "bip44"
          segwit <- receiveOn home Btc "bip84"
          _ <- payFromNode chains Btc legacy 100000
          mine chains Btc 1
          _ <- payFromNode chains Btc segwit 40000
          mine chains Btc 1
          _ <- syncNow home
          coinsOn home Btc `shouldReturn` [(40000, 1, True), (100000, 2, True)]

    it "add up in the history of the portfolio, the newest first" $
      onChains $ \chains ->
        withWallet chains testMnemonic $ \home -> do
          mine chains Btc 101
          legacy <- receiveOn home Btc "bip44"
          segwit <- receiveOn home Btc "bip84"
          _ <- payFromNode chains Btc legacy 100000
          mine chains Btc 1
          _ <- payFromNode chains Btc segwit 40000
          mine chains Btc 1
          _ <- syncNow home
          txsOn home Btc `shouldReturn` [(TxCredit, 40000, Just 1), (TxCredit, 100000, Just 2)]

  describe "The same key paid on both regtest networks" $ do
    -- The test networks share their keys: the first legacy address of
    -- btcreg and the first address of bchreg are one key
    it "holds a coin on each network, kept apart" $
      onChains $ \chains ->
        withWallet chains testMnemonic $ \home -> do
          mine chains Btc 101
          mine chains Bch 101
          core <- receiveOn home Btc "bip44"
          cash <- receiveOn home Bch "bip44"
          _ <- payFromNode chains Btc core 100000
          _ <- payFromNode chains Bch cash 30000
          mine chains Btc 1
          mine chains Bch 1
          _ <- syncNow home
          coinsOn home Btc `shouldReturn` [(100000, 1, True)]
          coinsOn home Bch `shouldReturn` [(30000, 1, True)]

    it "is synced on one network alone by sync --network, which leaves the other as it was" $
      onChains $ \chains ->
        withWallet chains testMnemonic $ \home -> do
          mine chains Btc 101
          mine chains Bch 101
          core <- receiveOn home Btc "bip44"
          cash <- receiveOn home Bch "bip44"
          _ <- payFromNode chains Btc core 100000
          _ <- payFromNode chains Bch cash 30000
          mine chains Btc 1
          mine chains Bch 1
          _ <- answer home ["sync", "--network", "bchreg"] []
          coinsOn home Btc `shouldReturn` []
          coinsOn home Bch `shouldReturn` [(30000, 1, True)]

  describe "A sync of a wallet nobody paid" $ do
    it "finds no transaction on a chain without a block" $
      onChains $ \chains ->
        withWallet chains testMnemonic $ \home -> do
          _ <- syncNow home
          txsOn home Btc `shouldReturn` []
          txsOn home Bch `shouldReturn` []
