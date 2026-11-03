{-# LANGUAGE OverloadedStrings #-}

-- | Output descriptors as hw reads and writes them: the descriptor of an
-- account, the checksum of BIP 380, the key origins of BIP 329 records,
-- and the descriptors the files of other wallets carry
module Haskoin.Wallet.DescriptorSpec (spec) where

import qualified Data.Aeson as Json
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Descriptor
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils (testAccountXPub, testAccountXPubTest, testFingerprint, testSegwitAccountVPub, testSegwitAccountXPub, testSegwitAccountXPubTest, testSegwitAccountZPub)
import Test.Hspec

spec :: Spec
spec = do
  descriptorSpec
  checksumSpec
  keySpec
  originSpec
  fileSpec

{- Fixtures -}

-- | The path of account 0 of a seed on Bitcoin, m/44'/0'/0'
account0 :: HardPath
account0 = Deriv :| 44 :| 0 :| 0

-- | The descriptor of the account main of the test wallet, as hw writes
-- it. The checksum was computed apart from hw with the code of BIP 380,
-- and Bitcoin Core 32.1 gives the same (test/data/descriptors/README.md).
mainDescriptor :: Text
mainDescriptor = "pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j87"

-- | The same account read as a descriptor
mainParsed :: Descriptor
mainParsed = Descriptor ["pkh"] (Just (testFingerprint, toGeneric account0)) testAccountXPub BothChains

-- | The bip84 account 0 of the test wallet, and its descriptor
segwitAccount0 :: HardPath
segwitAccount0 = Deriv :| 84 :| 0 :| 0

segwitDescriptor :: Text
segwitDescriptor = "wpkh([bc560f95/84h/0h/0h]" <> testSegwitAccountXPub <> "/<0;1>/*)#5nktextl"

{- Descriptors -}

descriptorSpec :: Spec
descriptorSpec =
  describe "The descriptor of an account" $ do
    it "is its key in pkh, with the origin of the key, both chains and its checksum, hardened steps marked with an h" $
      descriptorText (accountDescriptor Bip44 (Just testFingerprint) account0 testAccountXPub) `shouldBe` mainDescriptor

    -- The descriptor of the bip84 section of the Coldcard file of
    -- test/data/descriptors, for the bip84 account of the test wallet
    it "is its key in wpkh for a bip84 account, as a Coldcard writes it" $ do
      descriptorText (accountDescriptor Bip84 (Just testFingerprint) segwitAccount0 testSegwitAccountXPub) `shouldBe` segwitDescriptor
      parseDescriptor segwitDescriptor `shouldBe` Right (Descriptor ["wpkh"] (Just (testFingerprint, toGeneric segwitAccount0)) testSegwitAccountXPub BothChains)

    it "names no origin when the fingerprint of the seed is unknown" $
      descriptorText (accountDescriptor Bip44 Nothing account0 testAccountXPub)
        `shouldBe` "pkh(" <> testAccountXPub <> "/<0;1>/*)#mmejv52s"

    it "reads back as written" $
      parseDescriptor mainDescriptor `shouldBe` Right mainParsed

    it "is read with hardened steps marked with an apostrophe or an H, and without a checksum" $ do
      parseDescriptor ("pkh([bc560f95/44'/0'/0']" <> testAccountXPub <> "/<0;1>/*)") `shouldBe` Right mainParsed
      parseDescriptor ("pkh([bc560f95/44H/0H/0H]" <> testAccountXPub <> "/<0;1>/*)") `shouldBe` Right mainParsed

    it "is read for one chain, as Bitcoin Core writes an account in two descriptors" $ do
      parseDescriptor ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/0/*)#77rvns4p")
        `shouldBe` Right mainParsed {descriptorChains = ReceivingChain}
      parseDescriptor ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/1/*)#02xdw99e")
        `shouldBe` Right mainParsed {descriptorChains = ChangeChain}

    it "is read from a key alone, with or without its origin, as the wallets that take a key give it" $ do
      parseDescriptor testAccountXPub `shouldBe` Right (Descriptor [] Nothing testAccountXPub BothChains)
      parseDescriptor ("[bc560f95/44h/0h/0h]" <> testAccountXPub)
        `shouldBe` Right (Descriptor [] (Just (testFingerprint, toGeneric account0)) testAccountXPub BothChains)

    it "is read with the spaces around it" $
      parseDescriptor ("  " <> mainDescriptor <> "\n") `shouldBe` Right mainParsed

    it "is refused with a wrong checksum, or one that is not eight characters" $ do
      parseDescriptor ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j88")
        `shouldBe` Left "The checksum of the descriptor is wrong"
      parseDescriptor ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j8")
        `shouldBe` Left "The checksum of the descriptor is wrong"

    it "is refused when the path after the key does not derive the chains of an account" $ do
      let chainsRefused = Left "The descriptor must end in /<0;1>/*, /0/* or /1/* after its key, to derive the addresses of an account"
      parseDescriptor ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/0/5)") `shouldBe` chainsRefused
      parseDescriptor ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> ")") `shouldBe` chainsRefused
      parseDescriptor ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*h)") `shouldBe` chainsRefused
      parseDescriptor ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<1;0>/*)") `shouldBe` chainsRefused

    it "is not read from a descriptor of several keys, nor from a text that is not one" $ do
      let notOne = Left "The text is not an output descriptor of one key"
      parseDescriptor ("wsh(sortedmulti(2,[a1b2c3d4/48h/0h/0h/2h]" <> testAccountXPub <> "/0/*,[d34db33f/48h/0h/0h/2h]" <> testAccountXPubTest <> "/0/*))") `shouldBe` notOne
      parseDescriptor "not a descriptor" `shouldBe` notOne
      parseDescriptor "pkh([bc560f9/44h/0h/0h]xpub6CHK/<0;1>/*)" `shouldBe` notOne
      parseDescriptor "pkh(" `shouldBe` notOne
      parseDescriptor "" `shouldBe` notOne

    it "gives the origin of its key, as a record names the account by it, when it has one" $ do
      descriptorOrigin mainParsed `shouldBe` Just (Origin ["pkh"] testFingerprint (toGeneric account0))
      descriptorOrigin mainParsed {descriptorKeyOrigin = Nothing} `shouldBe` Nothing

    it "is what a text with script functions or a key origin looks like, whatever follows" $ do
      looksLikeDescriptor "pkh(what)" `shouldBe` True
      looksLikeDescriptor "wsh(sortedmulti(2,a,b))" `shouldBe` True
      looksLikeDescriptor "[bc560f95/44h/0h/0h]" `shouldBe` True
      looksLikeDescriptor "not a record" `shouldBe` False
      looksLikeDescriptor "xpub6CHK" `shouldBe` False
      looksLikeDescriptor "" `shouldBe` False

{- Checksums -}

checksumSpec :: Spec
checksumSpec =
  describe "The checksum of a descriptor" $ do
    -- The test vector of BIP 380
    it "is the one BIP 380 gives raw(deadbeef)" $
      checksum "raw(deadbeef)" `shouldBe` Just "89f8spxm"

    it "is none for a text with a character descriptors do not hold" $
      checksum "raw(\220)" `shouldBe` Nothing

{- Extended keys -}

keySpec :: Spec
keySpec =
  describe "An extended key" $ do
    it "is private when it starts with xprv or tprv" $ do
      isPrivateKey "xprv9yHxeaLAZvxXb9VtJNesqk8avfN8misGAW9DUW9eacZJNqsfZxqKLmK5jfmvFideQqGesviJeagzSQYCuQySjgvt7TdfowKja5aJqbgyuNh" `shouldBe` True
      isPrivateKey "tprv8ZgxMBicQKsPdLA1DWtYbSdVVfjzd7dNqEvvBFL6V6YjwLkiQnzT6y9NgSHFG27K3sKpofaeeZjrSZkomqA7YRtncLyEEnuqhRWUeMMHq3T" `shouldBe` True
      isPrivateKey testAccountXPub `shouldBe` False

    -- The zpub and the vpub are the keys of the BIP84 account 0 of the test
    -- wallet on Bitcoin and on testnet as a Coldcard and Electrum write
    -- them, the xpub and the tpub the same keys as BIP32 writes them
    it "reads a zpub or a vpub of SLIP-132 as the xpub or tpub of a bip84 account" $ do
      readAccountKey testSegwitAccountZPub `shouldBe` Right (Just Bip84, testSegwitAccountXPub)
      readAccountKey testSegwitAccountVPub `shouldBe` Right (Just Bip84, testSegwitAccountXPubTest)

    it "writes the key of a bip84 account with the zpub prefix of SLIP-132, which Electrum takes" $
      slip132Key Bip84 testSegwitAccountXPub `shouldBe` Just testSegwitAccountZPub

    it "writes a vpub for a bip84 account of a test network" $
      slip132Key Bip84 testSegwitAccountXPubTest `shouldBe` Just testSegwitAccountVPub

    it "writes no SLIP-132 key for a bip44 account, whose xpub marks it" $ do
      slip132Key Bip44 testAccountXPub `shouldBe` Nothing
      slip132Key Bip44 testAccountXPubTest `shouldBe` Nothing

    it "writes none for text that is not an extended key" $
      slip132Key Bip84 "not a key" `shouldBe` Nothing

    it "reads an xpub or a tpub as it is, which marks no kind of account" $ do
      readAccountKey testAccountXPub `shouldBe` Right (Nothing, testAccountXPub)
      readAccountKey testAccountXPubTest `shouldBe` Right (Nothing, testAccountXPubTest)

    it "refuses the ypub and upub of a bip49 account, and the keys of multisig accounts" $ do
      readAccountKey "ypub6YGZhSg8Nso498vduxTVMtR3EtZUu8H3bKfUmJDp5x1QXjy9DdACFQ8VwcgtS6NSgzCMGs5mUSCP2iG3BtXCRBP8Zh7V74rwL9qRRbN1yGV"
        `shouldBe` Left "The key has the prefix ypub of SLIP-132, which marks a BIP49 account, with P2SH-P2WPKH addresses: hw holds bip44 and bip84 accounts"
      readAccountKey "upub5DmL6Fd1D7dcMfS5r7XxxGH5Lbnxvp1KGZC3CJQMGYE2S6h7GNA1ZGqoSrcGKGnEQdeEYj5BrvDuqGeT6YSjG9BpwZEpvbxdMKcmTdQ2NR9"
        `shouldBe` Left "The key has the prefix upub of SLIP-132, which marks a BIP49 account, with P2SH-P2WPKH addresses: hw holds bip44 and bip84 accounts"
      readAccountKey "Zpub6r4mjW7yrMWVXbBmfxTonrBKKVapwoKJECopsBs8nJfVCLg4E7SDEAyyQtkGubtbsPWC4MmuLyhaNKvV1ChQa2Z418YDQmyzD2GCiogfbAh"
        `shouldBe` Left "The key has the prefix Zpub of SLIP-132, which marks a multisig account: hw holds bip44 and bip84 accounts of one key"

    it "refuses a zpub that does not read as an extended key" $
      readAccountKey "zpub6r4mjW7yrMW" `shouldBe` Left "The key starts with zpub but does not read as an extended key"

    it "leaves any other text as it is" $
      readAccountKey "not a key" `shouldBe` Right (Nothing, "not a key")

{- Origins -}

originSpec :: Spec
originSpec =
  describe "The origin of a record" $ do
    it "is written for a BIP44 account as pkh([fingerprint/44'/coin'/index'])" $
      originText (accountOrigin Bip44 testFingerprint account0) `shouldBe` "pkh([bc560f95/44'/0'/0'])"

    it "is written for a BIP84 account as wpkh([fingerprint/84'/coin'/index'])" $
      originText (accountOrigin Bip84 testFingerprint segwitAccount0) `shouldBe` "wpkh([bc560f95/84'/0'/0'])"

    it "names the kind of account by its script functions: pkh for bip44, wpkh for bip84, and nothing for the others" $ do
      (kindScripts Bip44, kindScripts Bip84) `shouldBe` (["pkh"], ["wpkh"])
      kindOfScripts ["wpkh"] `shouldBe` Just Bip84
      kindOfScripts ["pkh"] `shouldBe` Just Bip44
      kindOfScripts ["sh", "wpkh"] `shouldBe` Nothing
      kindOfScripts [] `shouldBe` Nothing

    it "reads back as written" $
      parseOrigin "pkh([bc560f95/44'/0'/0'])" `shouldBe` Just (Origin ["pkh"] testFingerprint (toGeneric account0))

    -- BIP 380 lets a descriptor mark a hardened step with h or H as well
    it "reads hardened steps marked with h or H, as other wallets write them" $ do
      parseOrigin "pkh([73c5da0a/44h/0h/0h])" `shouldBe` Just (Origin ["pkh"] "73c5da0a" (toGeneric account0))
      parseOrigin "pkh([73c5da0a/44H/0H/0H])" `shouldBe` Just (Origin ["pkh"] "73c5da0a" (toGeneric account0))

    it "reads the script functions around the key from the outside in, and writes them back" $ do
      let nested = Origin ["sh", "wpkh"] "d34db33f" (toGeneric (Deriv :| 49 :| 0 :| 0 :: HardPath))
      parseOrigin "sh(wpkh([d34db33f/49'/0'/0']))" `shouldBe` Just nested
      originText nested `shouldBe` "sh(wpkh([d34db33f/49'/0'/0']))"

    it "reads a key origin without a script, and a fingerprint without a path" $ do
      parseOrigin "[d34db33f/84'/0'/0']" `shouldBe` Just (Origin [] "d34db33f" (toGeneric (Deriv :| 84 :| 0 :| 0 :: HardPath)))
      parseOrigin "pkh([d34db33f])" `shouldBe` Just (Origin ["pkh"] "d34db33f" Deriv)

    it "reads a path with soft steps" $
      parseOrigin "pkh([d34db33f/44'/0'/0'/1])" `shouldBe` Just (Origin ["pkh"] "d34db33f" (toGeneric account0 :/ 1))

    it "is none for a descriptor of several keys" $
      parseOrigin "wsh(sortedmulti(2,[a1b2c3d4/48'/0'/0'/2'],[d34db33f/48'/0'/0'/2']))" `shouldBe` Nothing

    it "is none for a whole descriptor, with a key after the origin" $
      parseOrigin ("pkh([bc560f95/44'/0'/0']" <> testAccountXPub <> "/<0;1>/*)") `shouldBe` Nothing

    it "is none for a fingerprint that is not eight hexadecimal digits, or a path that is not one" $ do
      parseOrigin "pkh([d34db33/44'/0'/0'])" `shouldBe` Nothing
      parseOrigin "pkh([d34db33fa/44'/0'/0'])" `shouldBe` Nothing
      parseOrigin "pkh([d34db33g/44'/0'/0'])" `shouldBe` Nothing
      parseOrigin "pkh([d34db33f/44'/x'])" `shouldBe` Nothing
      parseOrigin "pkh(d34db33f/44'/0'/0')" `shouldBe` Nothing
      parseOrigin "" `shouldBe` Nothing

{- The files of other wallets -}

-- | A JSON file of test/data/descriptors as one object
fileObject :: FilePath -> IO Json.Object
fileObject name = either fail return =<< Json.eitherDecodeFileStrict' ("test/data/descriptors/" <> name)

-- | The descriptor of the test account on the test networks, on one chain,
-- as Bitcoin Core writes it
testChain :: Text -> Text
testChain chain = "pkh([bc560f95/44h/1h/0h]" <> testAccountXPubTest <> "/" <> chain <> "/*)"

fileSpec :: Spec
fileSpec =
  describe "The descriptors of a file of another wallet" $ do
    it "are those of the file of Bitcoin Core's listdescriptors, as written" $ do
      watching <- fileObject "core-watch-only.json"
      fileDescriptors watching `shouldBe` Just [testChain "0" <> "#r9hl74rq", testChain "1" <> "#j3j7rqnc"]

    -- Core's default wallet: pkh, sh(wpkh), tr and wpkh, two chains each
    it "are the eight descriptors of a wallet Bitcoin Core created" $ do
      wallet <- fileObject "core-default-wallet.json"
      (length <$> fileDescriptors wallet) `shouldBe` Just 8
      (take 1 <$> fileDescriptors wallet)
        `shouldBe` Just ["pkh([b079db60/44h/1h/0h]tpubDDd7Rn8xiQMbbJCFmhfyXmKzcGhhgxvXAXzHnMdd1gyE8NDXu1YMJXuYihYddaJaJkuUsLHeUGpXECBSkg8Gh3MKUQHe1ygZviP8XL4AyFq/0/*)#6fj392r6"]

    it "are those of the sections of the generic JSON of a Coldcard, BIP44, BIP49 and BIP84, as written" $ do
      coldcard <- fileObject "coldcard-export.json"
      fileDescriptors coldcard
        `shouldBe` Just
          [ mainDescriptor,
            "sh(wpkh([bc560f95/49h/0h/0h]xpub6DSJPn1DECFaHqjX5bfs9oKY4vR2xWHYgD9FyuKvhwdXUe9uxxzddLUMvQjJSBiXHM5YXPVD1mqq9ReUUC7BcwhXhMR4XA3T4Rmn31uhQm3/<0;1>/*))#2cc7tef4",
            "wpkh([bc560f95/84h/0h/0h]xpub6CQF8An9YzRXpzoY1EtZNfzJyZHw4ZLJPymPJQ5N2Huj693bio75z3fhNUq6unam47GaZQanRezUbkhMZosNyZBrGT9NExM1fa8uwdaWnoV/<0;1>/*)#5nktextl"
          ]

    -- An older firmware writes no descriptor: the fingerprint of the file
    -- and the path and key of the section make one
    it "are made from the fingerprint, the path and the key of a Coldcard file that writes no descriptor" $ do
      let older = Json.decodeStrict' "{\"xfp\": \"BC560F95\", \"bip44\": {\"deriv\": \"m/44'/0'/0'\", \"xpub\": \"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah\"}}"
      (fileDescriptors =<< older) `shouldBe` Just ["pkh([bc560f95/44'/0'/0']" <> testAccountXPub <> "/<0;1>/*)"]

    it "are none for a record of BIP 329, or an object of neither shape" $ do
      (fileDescriptors =<< Json.decodeStrict' "{\"type\": \"xpub\", \"ref\": \"xpub6CHK\"}") `shouldBe` Nothing
      (fileDescriptors =<< Json.decodeStrict' "{}") `shouldBe` Nothing
      (fileDescriptors =<< Json.decodeStrict' "{\"descriptors\": \"none\"}") `shouldBe` Nothing
