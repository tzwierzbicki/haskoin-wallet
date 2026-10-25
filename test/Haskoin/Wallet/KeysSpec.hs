{-# LANGUAGE OverloadedStrings #-}

-- | The keys of the test wallets, derived from their mnemonics. The shared
-- wallet of "Haskoin.Wallet.TestUtils" writes its keys and the fingerprint
-- of its seed out in full; these tests tie those values to the derivation. Then the
-- private keys of a sweep, in every format a user may write them.
module Haskoin.Wallet.KeysSpec where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Keys
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = do
  sweepKeysSpec
  prepareContext $ \ctx ->
    describe "Keys of the test wallets" $ do
      it "derives the account key of the test wallet from its mnemonic" $
        xPrvExport btc <$> signingKey btc ctx testMnemonic 0 `shouldBe` Right testAccountXPrv
      it "derives the account public key of the test wallet" $
        xPubExport btc ctx . deriveXPubKey ctx <$> signingKey btc ctx testMnemonic 0 `shouldBe` Right testAccountXPub
      it "derives the fingerprint of the seed from its master public key, the same on every network" $
        fingerprintToText . seedFingerprint <$> seedOf ctx testMnemonic `shouldBe` Right testFingerprintText
      it "derives the public key of an account from the public side of the seed" $ do
        seed <- expectRight $ seedOf ctx testMnemonic
        (fmap (xPubExport btc ctx) <$> seedAccountKey seed btc 0) `shouldReturn` Just testAccountXPub
      it "derives the keys and the fingerprint of the second wallet" $ do
        xPrvExport btc <$> signingKey btc ctx testMnemonic2 0 `shouldBe` Right testAccountXPrv2
        xPubExport btc ctx . deriveXPubKey ctx <$> signingKey btc ctx testMnemonic2 0 `shouldBe` Right testAccountXPub2
        fingerprintToText . seedFingerprint <$> seedOf ctx testMnemonic2 `shouldBe` Right testFingerprintText2
  passphraseSpec

{- The passphrase, as BIP39 hashes it -}

-- | The master keys of the words of the test wallet with the passphrase
-- "café", computed apart from hw with Python's hashlib, hmac and
-- unicodedata, by the PBKDF2 and HMAC of BIP39 and BIP32; the same script
-- gives the master key of the official BIP39 vector "abandon … about" with
-- "TREZOR". The first is from the passphrase in NFKD form, as BIP39 asks;
-- the second from its composed é as typed, as hw derived it before.
cafeMasterNormalized, cafeMasterTyped :: Text
cafeMasterNormalized = "xprv9s21ZrQH143K28mmRktNKJK6miBqMSwxLLpJEZaDyhTf18YbxXC6paThZQypFWYoD3GRxuRJBc9hfdBc3YvrhfVn3gNJMdRFfcvw2Vu91R9"
cafeMasterTyped = "xprv9s21ZrQH143K2eFJBCPUdcN7HDG1a5nexyyc4qmrmyK8sYEgopcTiFoYD9ADxSrWq8NFLGqCqrUxNrc2XwvFhWVL65iNLpmCSPANrSHPsqw"

-- | The words of the test wallet with a passphrase
withPassphrase :: Text -> MnemonicPass
withPassphrase = MnemonicPass (mnemonicWords testMnemonic)

passphraseSpec :: Spec
passphraseSpec =
  describe "The passphrase of a seed" $ do
    it "is normalized to NFKD, as BIP39 asks: the master key of other wallets" $
      xPrvExport btc <$> masterKey (withPassphrase "caf\233") `shouldBe` Right cafeMasterNormalized

    it "gives the same seed whatever form its accents were typed in" $
      xPrvExport btc <$> masterKey (withPassphrase "cafe\769") `shouldBe` Right cafeMasterNormalized

    it "splits an accented letter into its letter and its accent" $
      normalizePassphrase "caf\233" `shouldBe` "cafe\769"

    it "leaves a passphrase of plain ASCII as it is" $
      normalizePassphrase "correct horse battery staple" `shouldBe` "correct horse battery staple"

    it "can still give the master key hw derived before, from the passphrase as typed" $
      xPrvExport btc <$> typedMasterKey (withPassphrase "caf\233") `shouldBe` Right cafeMasterTyped


{- Private keys of a sweep -}

-- The key of the Bitcoin wiki's "Wallet import format" and "Private key"
-- pages, as 64 hexadecimal digits and in WIF for both encodings of its
-- public key; its testnet WIFs; and the minikey of the wiki's "Mini private
-- key format" page with the key it holds.
wikiKeyHex, wikiKeyWif, wikiKeyWifUncompressed, wikiKeyTestnetWif, wikiKeyTestnetWifUncompressed, miniKey, miniKeyHex :: Text
wikiKeyHex = "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
wikiKeyWif = "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617"
wikiKeyWifUncompressed = "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ"
wikiKeyTestnetWif = "cMzLdeGd5vEqxB8B6VFQoRopQ3sLAAvEzDAoQgvX54xwofSWj1fx"
wikiKeyTestnetWifUncompressed = "91gGn1HgSap6CbU12F6z3pJri26xzp7Ay1VW6NHCoEayNXwRpu2"
miniKey = "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy"
miniKeyHex = "4c7a9640c72dc2099f23715d0c8a0d8a35f8906e3cab61dd3f78b67bf887c9ab"

wikiKey, miniKeySecret :: SecKey
wikiKey = fromJust $ secKey =<< decodeHex wikiKeyHex
miniKeySecret = fromJust $ secKey =<< decodeHex miniKeyHex

sweepKeysSpec :: Spec
sweepKeysSpec = do
  describe "A private key of a sweep" $ do
    it "reads a WIF key for a compressed public key" $
      parseSecKey btc wikiKeyWif `shouldBe` Just wikiKey
    it "reads a WIF key for an uncompressed public key" $
      parseSecKey btc wikiKeyWifUncompressed `shouldBe` Just wikiKey
    it "reads the WIF keys of Bitcoin testnet on that network only" $ do
      parseSecKey btcTest wikiKeyTestnetWif `shouldBe` Just wikiKey
      parseSecKey btcTest wikiKeyTestnetWifUncompressed `shouldBe` Just wikiKey
      parseSecKey btc wikiKeyTestnetWif `shouldBe` Nothing
      parseSecKey btcTest wikiKeyWif `shouldBe` Nothing
    it "reads a minikey" $
      parseSecKey btc miniKey `shouldBe` Just miniKeySecret
    it "reads 64 hexadecimal digits" $
      parseSecKey btc wikiKeyHex `shouldBe` Just wikiKey
    it "refuses a WIF key with a wrong checksum" $
      parseSecKey btc "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98618" `shouldBe` Nothing
    it "refuses hexadecimal digits of the wrong length" $
      parseSecKey btc "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa" `shouldBe` Nothing
    it "refuses a word that is no key" $
      parseSecKey btc "my old key" `shouldBe` Nothing

  describe "The private keys of a file or a typed line" $ do
    it "takes every key of every line, with comments and labels around them" $
      parseSecKeys btc keyFile `shouldBe` Right [wikiKey, miniKeySecret, wikiKey, wikiKey]
    it "takes keys typed on one line" $
      parseSecKeys btc (wikiKeyWif <> " " <> miniKey) `shouldBe` Right [wikiKey, miniKeySecret]
    it "reads nothing from an empty text" $
      parseSecKeys btc "" `shouldBe` Right []
    it "refuses a line with words but no key" $
      parseSecKeys btc (wikiKeyWif <> "\n\nKwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98618 old wallet\n")
        `shouldBe` Left "No private key on line 3: KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98618 old wallet"
  where
    keyFile =
      "# The keys of the old wallet\n"
        <> wikiKeyWif <> " first key\n"
        <> "\n"
        <> "  # indented comment\n"
        <> miniKey <> "\n"
        <> wikiKeyWifUncompressed <> " " <> wikiKeyHex <> " two keys on a line\n"
