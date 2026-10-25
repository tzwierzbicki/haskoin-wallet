{-# LANGUAGE OverloadedStrings #-}

-- | The keys of the test wallets, derived from their mnemonics. The shared
-- wallet of "Haskoin.Wallet.TestUtils" writes its keys and the fingerprint
-- of its seed out in full; these tests tie those values to the derivation. Then the
-- private keys of a sweep, in every format a user may write them.
module Haskoin.Wallet.KeysSpec where

import Control.Monad (forM_)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
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
  otherImplementationsSpec

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


{- Against other implementations of BIP39 -}

-- | Three vectors of BIP39's Japanese test file, which BIP39 links to
-- (test_JP_BIP39.json of bip32JP.github.io): its vectors 1, 8 and 15, of
-- 12, 18 and 24 words. Their words are Japanese, which haskoin does not
-- read, so they check the normalization itself: the seed of the mnemonic
-- and the passphrase in NFKD form, and its master key. NFKD rewrites much
-- of them: a square of katakana and voiced kana in the passphrase, and the
-- ideographic spaces between the words.
data JapaneseVector = JapaneseVector
  { japaneseMnemonic :: Text,
    japaneseSeed :: Text,
    japaneseMaster :: Text
  }

japaneseVectors :: [(String, JapaneseVector)]
japaneseVectors =
  [ ( "1, of 12 words",
      JapaneseVector
        "あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あいこくしん\x3000あおぞら"
        "a262d6fb6122ecf45be09c50492b31f92e9beb7d9a845987a02cefda57a15f9c467a17872029a9e92299b5cbdf306e3a0ee620245cbd508959b6cb7ca637bd55"
        "xprv9s21ZrQH143K258jAiWPAM6JYT9hLA91MV3AZUKfxmLZJCjCHeSjBvMbDy8C1mJ2FL5ytExyS97FAe6pQ6SD5Jt9SwHaLorA8i5Eojokfo1"
    ),
    ( "8, of 18 words",
      JapaneseVector
        "われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000われる\x3000りんご"
        "f0f738128a65b8d1854d68de50ed97ac1831fc3a978c569e415bbcb431a6a671d4377e3b56abd518daa861676c4da75a19ccb41e00c37d086941e471a4374b95"
        "xprv9s21ZrQH143K2ThaKxBDxUByy4gNwULJyqKQzZXyF3aLyGdknnP18KvKVZwCvBJGXaAsKd7oh2ypLbjyDn4bDY1iiSPvNkKsVAGQGj7G3PZ"
    ),
    ( "15, of 24 words",
      JapaneseVector
        "くのう\x3000てぬぐい\x3000そんかい\x3000すろっと\x3000ちきゅう\x3000ほあん\x3000とさか\x3000はくしゅ\x3000ひびく\x3000みえる\x3000そざい\x3000てんすう\x3000たんぴん\x3000くしょう\x3000すいようび\x3000みけん\x3000きさらぎ\x3000げざん\x3000ふくざつ\x3000あつかう\x3000はやい\x3000くろう\x3000おやゆび\x3000こすう"
        "32e78dce2aff5db25aa7a4a32b493b5d10b4089923f3320c8b287a77e512455443298351beb3f7eb2390c4662a2e566eec5217e1a37467af43b46668d515e41b"
        "xprv9s21ZrQH143K2gbMb94GNwdogai6fA3vTrALH8eoNJKqPWn9KyeBMhUQLpsN5ePJkZdHsPmyDsECNLRaYiposqDDqsbk3ANk9hbsSgmVq7G"
    )
  ]

-- | The passphrase of every Japanese vector, as they write it
japanesePassphrase :: Text
japanesePassphrase = "㍍ガバヴァぱばぐゞちぢ十人十色"

-- | The seed of BIP39: PBKDF2 with HMAC-SHA512 over the mnemonic, salted
-- with "mnemonic" and the passphrase, both normalized, 2048 rounds, 64
-- bytes
bip39Seed :: Text -> Text -> ByteString
bip39Seed mnemonic passphrase =
  fastPBKDF2_SHA512
    (Parameters 2048 64)
    (encodeUtf8 (normalizePassphrase mnemonic))
    ("mnemonic" <> encodeUtf8 (normalizePassphrase passphrase))

-- | The master keys of the words of the test wallet with UTF-8
-- passphrases, computed on 2026-10-25 with Trezor's python-mnemonic, the
-- reference implementation of BIP39 (Mnemonic.to_seed, then
-- to_hd_master_key, from its master branch); the same run gave the
-- official vector of "abandon … about" with "TREZOR". The Czech passphrase
-- is the one of its own test of normalization, test_utf8_nfkd.
trezorVectors :: [(String, Text, Text)]
trezorVectors =
  [ ( "a Czech passphrase, its accents composed",
      "Neuvěřitelně bezpečné heslíčko",
      "xprv9s21ZrQH143K2QNKT314TMbCpkbTFvCXQZ1yeT6SW6meyqvYFJ6YD8CAyTbUueibmCWZHsBemsFhbMgaCcn4xCvuJ3PpsjMHQMNcW3AfFWB"
    ),
    ( "the same passphrase, its accents decomposed: the same key",
      "Neuve\x30cr\x30citelne\x30c bezpec\x30cne\x301 hesli\x301\&c\x30cko",
      "xprv9s21ZrQH143K2QNKT314TMbCpkbTFvCXQZ1yeT6SW6meyqvYFJ6YD8CAyTbUueibmCWZHsBemsFhbMgaCcn4xCvuJ3PpsjMHQMNcW3AfFWB"
    ),
    ( "the passphrase of the Japanese vectors",
      japanesePassphrase,
      "xprv9s21ZrQH143K2NfcXM1NuNqyRVaMyN1Jr1yKgNtCorTVqvB7p2mYZqSLFDQmP1LBvTUWATnokSFK4ibBfdVKcZqsiXBSAx2GZNKyvYwsm5r"
    ),
    ( "a German passphrase, with a sharp s and umlauts",
      "Straße über München",
      "xprv9s21ZrQH143K3XcLLMuRBABEiFwjWCYPJ9M6zMbt77oNA1UsduhYntoZ9Lw29YoPyUrgKRU6LE3VUGBvYFDz4DzuWAPPRgtwhmk4KqtE1dJ"
    ),
    ( "a Vietnamese passphrase, with stacked accents",
      "mật khẩu",
      "xprv9s21ZrQH143K2NCxSHfoghTQoztcvtRCuqXxeRHaLk8ntg4uQKGQLYX2H5UEhhuvR1LXFJ4XzRqJDVJkjFRGXV5EnXYu55ZxRDuzKoVLihe"
    ),
    ( "the ligature fi and a superscript two, which normalization spells out",
      "\xfb01nal\xb2",
      "xprv9s21ZrQH143K2TNemLwv528ZCW19r4AUdpbxWcyccxw4dUvqmYxtV4wSLbbNsxEnL8J4b8Y9r4EDZQMkx8EUiAZq8seyKtxo95Avrfgwv2D"
    ),
    ( "an emoji",
      "🔑 clé",
      "xprv9s21ZrQH143K47z8V3nNEmYakzH2iv1nbeojt3VbSbxF1c3QWEzfn1hHQEJXd8o8jza6go5udqDLGmT7P5giJ7AqvBy9Z96EjWrKB9awCs9"
    )
  ]

otherImplementationsSpec :: Spec
otherImplementationsSpec =
  describe "The seed, against other implementations of BIP39" $ do
    it "takes, in the Japanese vectors, a passphrase that normalization rewrites" $
      normalizePassphrase japanesePassphrase `shouldNotBe` japanesePassphrase

    forM_ japaneseVectors $ \(name, vector) ->
      it ("gives the seed and the master key of the Japanese vector " <> name) $ do
        let seed = bip39Seed (japaneseMnemonic vector) japanesePassphrase
        encodeHex seed `shouldBe` japaneseSeed vector
        xPrvExport btc (makeXPrvKey seed) `shouldBe` japaneseMaster vector

    forM_ trezorVectors $ \(name, passphrase, master) ->
      it ("gives the master key Trezor's python-mnemonic gives for " <> name) $
        xPrvExport btc <$> masterKey (withPassphrase passphrase) `shouldBe` Right master

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
