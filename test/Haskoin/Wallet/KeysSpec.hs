{-# LANGUAGE OverloadedStrings #-}

-- | The keys of the test wallets, derived from their mnemonics. The shared
-- wallet of "Haskoin.Wallet.TestUtils" writes its keys and fingerprint out
-- in full; these tests tie those values to the derivation.
module Haskoin.Wallet.KeysSpec where

import Haskoin
import Haskoin.Wallet.Keys
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx ->
    describe "Keys of the test wallets" $ do
      it "derives the account key of the test wallet from its mnemonic" $
        xPrvExport btc <$> signingKey btc ctx testMnemonic 0 `shouldBe` Right testAccountXPrv
      it "derives the account public key of the test wallet" $
        xPubExport btc ctx . deriveXPubKey ctx <$> signingKey btc ctx testMnemonic 0 `shouldBe` Right testAccountXPub
      it "derives the fingerprint of the test wallet" $
        fingerprintToText <$> walletFingerprint btc ctx testMnemonic `shouldBe` Right testFingerprintText
      it "derives the keys and the fingerprint of the second wallet" $ do
        xPrvExport btc <$> signingKey btc ctx testMnemonic2 0 `shouldBe` Right testAccountXPrv2
        xPubExport btc ctx . deriveXPubKey ctx <$> signingKey btc ctx testMnemonic2 0 `shouldBe` Right testAccountXPub2
        fingerprintToText <$> walletFingerprint btc ctx testMnemonic2 `shouldBe` Right testFingerprintText2
