{-# LANGUAGE OverloadedStrings #-}

-- | recover on live regtest chains, under different histories of a
-- mnemonic. See "Haskoin.Wallet.RegtestUtils" for the chains.
--
-- Every address here is one Bitcoin Core derives from the first test
-- mnemonic and its passphrase, with deriveaddresses, at the path the test
-- names. So recover is checked against keys that hw did not make.
module Haskoin.Wallet.Regtest.RecoverSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.HwTestUtils (Run (..), hw)
import Haskoin.Wallet.Keys (MnemonicPass (..))
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Haskoin.Wallet.Workflow.TestUtils (recoveredOf, seedOnce, switchTo, unspendableOf)
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $ do
  -- The wallet of the node pays 100000 satoshis to the address, in a
  -- block
  let paid chains chain address = do
        _ <- payFromNode chains chain address 100000
        mine chains chain 1

  describe "recover, for a mnemonic paid on btcreg" $ do
    -- m/84'/1'/0'/0/3
    it "finds portfolio0 from one payment at its fourth native SegWit address" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "bcrt1qxs878x34jym8rda234w4jupky2klrdt2nwqqtz"
        withHomeOn chains $ \home -> do
          (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Just 0, [0])
          spendableOf home Btc `shouldReturn` 100000

    -- m/44'/1'/0'/0/25: a wallet looks at 20 unused addresses in a row
    it "finds nothing from one payment at the 26th legacy address, past the gap of 20" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mqPhwa4vm1dFRLMUL1L4xENgJL7XAdpae2"
        withHomeOn chains $ \home ->
          (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Nothing, [])

    -- m/44'/1'/0'/0/19 and /0/39: the second is 20 past the first
    it "finds a payment 20 addresses past another one" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mqJ378oymkDmcsfYknHigJGBrJPxkSqLEM"
        paid chains Btc "mznDX2z3s6pzyTxQucAPiV9NKpushL3cbU"
        withHomeOn chains $ \home -> do
          _ <- recoverWith home testMnemonic []
          spendableOf home Btc `shouldReturn` 200000

    -- m/44'/1'/3'/0/0
    it "finds portfolio3 when it alone was paid" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mnFYaneAm8Jm68ca2FaZrvKCgCzKTt9LyN"
        withHomeOn chains $ \home ->
          (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Just 0, [3])

    -- portfolio0 of the new wallet is the active one, and it is empty
    it "gives the switch to portfolio3 when it alone was paid" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mnFYaneAm8Jm68ca2FaZrvKCgCzKTt9LyN"
        withHomeOn chains $ \home -> do
          run <- hw home ["recover"] seedOnce
          runOut run `shouldSatisfy` Text.isInfixOf "To work in portfolio3 of wallet0, make it the active one:\n  hw switch 3 --wallet 0\n"

    it "shows the payment of portfolio3 after that switch" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mnFYaneAm8Jm68ca2FaZrvKCgCzKTt9LyN"
        withHomeOn chains $ \home -> do
          _ <- recoverWith home testMnemonic []
          switchTo home 0 3
          txsOn home Btc `shouldReturn` [(TxCredit, 100000, Just 1)]

    -- m/44'/1'/12'/0/0: recover stops after 10 portfolios without a payment
    it "finds nothing when portfolio12 alone was paid" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mmU47rkrTB8WpenMyAE9hdoXE3rgExKSnJ"
        withHomeOn chains $ \home ->
          (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Nothing, [])

    it "finds portfolio12 with --portfolio-gap 13" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mmU47rkrTB8WpenMyAE9hdoXE3rgExKSnJ"
        withHomeOn chains $ \home ->
          (recoveredOf <$> recoverWith home testMnemonic ["--portfolio-gap", "13"]) `shouldReturn` Right (Just 0, [12])

    -- The first legacy address of the mnemonic with its passphrase
    it "finds nothing without the passphrase of the mnemonic" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"
        withHomeOn chains $ \home ->
          (recoveredOf <$> recoverWith home (MnemonicPass (mnemonicWords testMnemonic) "") []) `shouldReturn` Right (Nothing, [])

    it "finds nothing new the second time" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"
        withHomeOn chains $ \home -> do
          _ <- recoverWith home testMnemonic []
          (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Just 0, [])

    -- The payment received, and the payment of everything that followed:
    -- the fee is 192 bytes × 10 = 1920, inside the 100000
    it "finds a portfolio that holds nothing any more, by its history" $
      onChains $ \chains ->
        withPaidWallet chains Btc $ \spender -> do
          _ <- payAll chains Btc spender testMnemonic [outsideAddress, "--fee-rate", "10"]
          mine chains Btc 1
          withHomeOn chains $ \home -> do
            _ <- recoverWith home testMnemonic []
            txsOn home Btc `shouldReturn` [(TxDebit, -100000, Just 1), (TxCredit, 100000, Just 2)]

    it "finds nothing when the one payment of the mnemonic was replaced in a reorganisation" $
      onChains $ \chains ->
        withReceived chains Btc $ \received -> do
          _ <- replaceWithRival chains Btc (receivedRivalHome received) (receivedRival received)
          withHomeOn chains $ \home ->
            (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Nothing, [])

  describe "recover, for a mnemonic paid where other wallets keep their accounts" $ do
    -- m/49'/1'/0'/0/0, wrapped SegWit, and m/86'/1'/0'/0/0, Taproot
    it "tells of the funds at the paths of wrapped SegWit and of Taproot, which hw cannot spend" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "2NBDzHM4DcEBv9frkRKN2uXnamGN9u63k8Y"
        paid chains Btc "bcrt1ppalxw3tmgzu0nwa9cmcc8jdwq7au2zlutr3yukq008xqt8n7z8ysah7vgu"
        withHomeOn chains $ \home ->
          (unspendableOf <$> recoverWith home testMnemonic [])
            `shouldReturn` Right
              [ ("btcreg", "bip49" :: Text, "/49'/1'/0'", 1, 100000),
                ("btcreg", "bip86", "/86'/1'/0'", 1, 100000)
              ]

    it "creates no wallet for them" $
      onChains $ \chains -> do
        mine chains Btc 101
        paid chains Btc "2NBDzHM4DcEBv9frkRKN2uXnamGN9u63k8Y"
        withHomeOn chains $ \home ->
          (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Nothing, [])

  describe "recover, for a mnemonic paid on bchreg" $ do
    -- The first legacy address, which Bitcoin Cash Node takes in the form
    -- of Bitcoin too
    it "finds portfolio0 from one payment on bchreg alone" $
      onChains $ \chains -> do
        mine chains Bch 101
        paid chains Bch "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"
        withHomeOn chains $ \home -> do
          (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Just 0, [0])
          spendableOf home Bch `shouldReturn` 100000
          spendableOf home Btc `shouldReturn` 0
