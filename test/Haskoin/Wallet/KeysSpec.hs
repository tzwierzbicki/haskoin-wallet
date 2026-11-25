{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The keys of the test wallets, derived from their mnemonics. The shared
-- wallet of "Haskoin.Wallet.TestUtils" writes its keys and the fingerprint
-- of its seed out in full; these tests tie those values to the derivation.
-- Then the accounts a portfolio is made of, and the keys a seed gives
-- them. Then the keys and the addresses of other wallets, which hw must
-- derive as they do. Then the private keys of a sweep, in every format a
-- user may write them.
module Haskoin.Wallet.KeysSpec where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.List (isInfixOf, nub)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Normalize (NormalizationMode (NFKD), normalize)
import Haskoin
import Haskoin.Crypto.Keys.Slip39 (EncryptedMasterSecret (..))
import Haskoin.Wallet.Database.Address (genExtAddress, nextChangeAddress)
import Haskoin.Wallet.Database (DB, DBAddress (..))
import Haskoin.Wallet.Keys
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Testnet3 (btcTestnet3)
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = do
  sweepKeysSpec
  chainPathSpec
  shownSecretSpec
  knownWalletsSpec
  leadingZeroSpec
  sharesSeedSpec
  masterKeySpec
  describe "Keys of the test wallets" $ do
    it "derives the account key of the test wallet from its mnemonic" $
      xPrvExport btc <$> signingKey Bip44 btc testMnemonic 0 `shouldBe` Right testAccountXPrv
    it "derives the account public key of the test wallet" $
      xPubExport btc . deriveXPubKey <$> signingKey Bip44 btc testMnemonic 0 `shouldBe` Right testAccountXPub
    it "derives the fingerprint of the seed from its master public key, the same on every network" $
      fingerprintToText . seedFingerprint <$> seedOf testMnemonic `shouldBe` Right testFingerprintText
    it "derives the public key of an account from the public side of the seed" $ do
      seed <- expectRight $ seedOf testMnemonic
      (fmap (xPubExport btc) <$> seedAccountKey seed Bip44 btc 0) `shouldReturn` Just testAccountXPub
    it "derives the keys and the fingerprint of the second wallet" $ do
      xPrvExport btc <$> signingKey Bip44 btc testMnemonic2 0 `shouldBe` Right testAccountXPrv2
      xPubExport btc . deriveXPubKey <$> signingKey Bip44 btc testMnemonic2 0 `shouldBe` Right testAccountXPub2
      fingerprintToText . seedFingerprint <$> seedOf testMnemonic2 `shouldBe` Right testFingerprintText2
    it "derives the keys of the bip84 account of the test wallet along m/84'/0'/0'" $ do
      xPrvExport btc <$> signingKey Bip84 btc testMnemonic 0 `shouldBe` Right testSegwitAccountXPrv
      xPubExport btc . deriveXPubKey <$> signingKey Bip84 btc testMnemonic 0 `shouldBe` Right testSegwitAccountXPub
    it "derives the public key of the bip84 account from the public side of the seed" $ do
      seed <- expectRight $ seedOf testMnemonic
      (fmap (xPubExport btc) <$> seedAccountKey seed Bip84 btc 0) `shouldReturn` Just testSegwitAccountXPub
  kindSpec
  portfolioKeysSpec
  foreignKindSpec
  portfolioAccountsSpec
  poolPathSpec
  passphraseSpec
  otherImplementationsSpec

{- The kinds of accounts -}

-- | The paths below the key of an account that are of its addresses
chainPathSpec :: Spec
chainPathSpec =
  describe "A path below the key of an account" $ do
    it "is of an address of the account on its receiving chain, /0/n, or on its change chain, /1/n" $
      map isChainPath ["/0/0", "/1/0", "/0/2147483647", "/1/5"] `shouldBe` [True, True, True, True]
    it "is of no address of the account anywhere else: another chain, a chain alone, a step below an address, or the key itself" $
      map isChainPath ["/2/7", "/0", "/1", "/0/5/3", Deriv] `shouldBe` [False, False, False, False, False]

kindSpec :: Spec
kindSpec =
  describe "The kinds of accounts" $ do
    it "are named after their BIP, whose number is the purpose of their path" $ do
      (kindPurpose Bip44, kindPurpose Bip84) `shouldBe` (44, 84)
      (kindName Bip44, kindName Bip84) `shouldBe` ("bip44", "bip84")
      kindNames `shouldBe` ["bip44", "bip84"]
      kindOfPurpose 84 `shouldBe` Just Bip84
      kindOfPurpose 49 `shouldBe` Nothing
      kindNamed "BIP84" `shouldBe` Just Bip84
      kindNamed "bip49" `shouldBe` Nothing

    it "set the first step of the path of an account, before the coin type of the network and the index" $ do
      pathToStr (accountDeriv Bip44 btc 0) `shouldBe` "/44'/0'/0'"
      pathToStr (accountDeriv Bip84 btc 0) `shouldBe` "/84'/0'/0'"
      pathToStr (accountDeriv Bip84 btcTestnet3 3) `shouldBe` "/84'/1'/3'"

    -- The first receiving key of the bip44 and of the bip84 account of the
    -- test wallet, whose addresses are those of the test fixtures
    it "give a key its address: P2PKH for bip44, P2WPKH for bip84" $ do
      addrToText btc (keyAddress Bip44 (derivePubPath firstKey testAccountPub)) `shouldBe` Just (externalAddrsText !! 0)
      addrToText btc (keyAddress Bip84 (derivePubPath firstKey testSegwitAccountPub)) `shouldBe` Just (segwitExternalAddrsText !! 0)

    it "give a key the script of its address, which an account spends the coins of" $ do
      let public = wrapPubKey True (derivePubPath firstKey testSegwitAccountPub).key
      outputAddress btc (keyScript Bip84 public) `shouldBe` Just (segwitExternalAddr 0)
      spendsScript Bip84 (keyScript Bip84 public) `shouldBe` True
      spendsScript Bip44 (keyScript Bip84 public) `shouldBe` False
      spendsScript Bip44 (keyScript Bip44 public) `shouldBe` True
      spendsScript Bip84 (keyScript Bip44 public) `shouldBe` False

    it "are both on the networks of Bitcoin, and bip44 alone on those of Bitcoin Cash, which has no SegWit" $ do
      networkKinds btc `shouldBe` [Bip44, Bip84]
      networkKinds btcTestnet3 `shouldBe` [Bip44, Bip84]
      networkKinds bch `shouldBe` [Bip44]
      checkKindOnNetwork bch Bip84 `shouldBe` Left "A bip84 account needs SegWit, which bch does not have"
      checkKindOnNetwork bch Bip44 `shouldBe` Right ()
      checkKindOnNetwork btc Bip84 `shouldBe` Right ()
  where
    firstKey = Deriv :/ 0 :/ 0 :: SoftPath

{- The accounts of a portfolio -}

-- | The twelve accounts of a whole portfolio, by the name of their
-- network and of their type, in the order hw lists them: the table of the
-- specification, a row for each network of each of its five paths
twelveAccounts :: [(String, Text)]
twelveAccounts =
  [ ("btc", "bip44"),
    ("btc", "bip84"),
    ("bch", "bip44"),
    ("btctest", "bip44"),
    ("btctest", "bip84"),
    ("btctest4", "bip44"),
    ("btctest4", "bip84"),
    ("btcreg", "bip44"),
    ("btcreg", "bip84"),
    ("bchtest3", "bip44"),
    ("bchtest4", "bip44"),
    ("bchreg", "bip44")
  ]

portfolioAccountsSpec :: Spec
portfolioAccountsSpec =
  describe "The accounts of a portfolio" $ do
    it "are on eight networks: Bitcoin, Bitcoin Cash, then the test networks of each" $
      map (.name) portfolioNetworks `shouldBe` ["btc", "bch", "btctest", "btctest4", "btcreg", "bchtest3", "bchtest4", "bchreg"]

    it "are twelve: bip44 and bip84 on the networks of Bitcoin, bip44 alone on those of Bitcoin Cash" $
      [(net.name, kindName kind) | (net, kind) <- portfolioAccounts] `shouldBe` twelveAccounts

    -- The five paths of the table of the specification, for portfolio 0
    -- and for portfolio 1, whose last step is 1'
    it "are at five paths, the test networks sharing the coin type 1, with the index of the portfolio as the last step" $ do
      let pathsOf index = nub [pathToStr (accountDeriv kind net index) | (net, kind) <- portfolioAccounts]
      pathsOf 0 `shouldBe` ["/44'/0'/0'", "/84'/0'/0'", "/44'/145'/0'", "/44'/1'/0'", "/84'/1'/0'"]
      pathsOf 1 `shouldBe` ["/44'/0'/1'", "/84'/0'/1'", "/44'/145'/1'", "/44'/1'/1'", "/84'/1'/1'"]

    it "rank their networks in the order hw lists them, which orders whatever it groups by network" $
      map networkRank [btc, bch, btcTestnet3, btcTest4, btcRegTest, bchTest3, bchTest4, bchRegTest] `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7]

-- | The keys of portfolio 1 of the test wallet that the shared fixtures
-- do not have: m/44'/145'/1', m/44'/1'/1' and m/84'/1'/1'. They were
-- derived apart from hw, as 'testSegwitAccountXPub1' was.
testAccountXPubBch1, testAccountXPubTest1, testSegwitAccountXPubTest1 :: Text
testAccountXPubBch1 = "xpub6DXWb36CzdZvXczuQJkPWfPZTkZFQR6KtWaBdkSJVn1JnDxFhax8mHVEuJiVRKGnWFAsJE2z1xm8b537zZ5HCpeagMTgRtscewKyNg241hE"
testAccountXPubTest1 = "tpubDCEeDpHvZZ49UfWyCY8mu8dLCVf81KVQtKKETQrQZnpsWQVoHUCo6aZbpXrRaBaNLzVGB28pj6Bmq639gp2NAi5scUt6LbN6HuNW4d8fkhq"
testSegwitAccountXPubTest1 = "tpubDDRUQfT52zY86TAPcsutu2TitBXF4PTq1AqJBBwR9HRynX8pG6ZBk66o7ifWhKtxA5vdK9wv5N1U3mcX8X5vYGhszGZkw6xmbxC8n2Fv6vA"

portfolioKeysSpec :: Spec
portfolioKeysSpec =
  describe "The keys of a portfolio of a seed" $ do
    it "are twelve, one for each account of the portfolio, in their order" $ do
      keys <- portfolioKeys 0
      [(net.name, kindName kind) | (net, kind, _) <- keys] `shouldBe` twelveAccounts

    it "are, for portfolio 0 on btc, the keys at m/44'/0'/0' and m/84'/0'/0'" $ do
      keys <- portfolioKeys 0
      keyOf keys btc Bip44 `shouldBe` [testAccountXPub]
      keyOf keys btc Bip84 `shouldBe` [testSegwitAccountXPub]

    it "are, for portfolio 0 on bch, the key at m/44'/145'/0'" $ do
      keys <- portfolioKeys 0
      keyOf keys bch Bip44 `shouldBe` [testAccountXPubBch]

    -- The coin type of every test network is 1
    it "are one key for the bip44 accounts of the six test networks, the key at m/44'/1'/0'" $ do
      keys <- portfolioKeys 0
      concatMap (\net -> keyOf keys net Bip44) [btcTestnet3, btcTest4, btcRegTest, bchTest3, bchTest4, bchRegTest]
        `shouldBe` replicate 6 testAccountXPubTest

    it "are one key for the bip84 accounts of the three test networks of Bitcoin, the key at m/84'/1'/0'" $ do
      keys <- portfolioKeys 0
      concatMap (\net -> keyOf keys net Bip84) [btcTestnet3, btcTest4, btcRegTest]
        `shouldBe` replicate 3 testSegwitAccountXPubTest

    it "are no bip84 key on the networks of Bitcoin Cash, which have no SegWit" $ do
      keys <- portfolioKeys 0
      concatMap (\net -> keyOf keys net Bip84) [bch, bchTest3, bchTest4, bchRegTest] `shouldBe` []

    -- Twelve accounts from five keys
    it "are, for portfolio 1, the keys at the same paths with 1' as the last step" $ do
      keys <- portfolioKeys 1
      keyOf keys btc Bip44 `shouldBe` [testAccountXPub1]
      keyOf keys btc Bip84 `shouldBe` [testSegwitAccountXPub1]
      keyOf keys bch Bip44 `shouldBe` [testAccountXPubBch1]
      keyOf keys btcTest4 Bip44 `shouldBe` [testAccountXPubTest1]
      keyOf keys bchRegTest Bip44 `shouldBe` [testAccountXPubTest1]
      keyOf keys btcRegTest Bip84 `shouldBe` [testSegwitAccountXPubTest1]

    it "are those of another seed for another mnemonic" $ do
      second <- expectRight $ seedOf testMnemonic2
      keys <- maybe (fail "The seed has no portfolio 0") return =<< seedPortfolioKeys second 0
      keyOf keys btc Bip44 `shouldBe` [testAccountXPub2]

    -- BIP32 says to skip an index whose child key is invalid
    it "are none at an index where a child key is invalid, so the seed has no portfolio there" $
      (fmap length <$> seedPortfolioKeys seedWithoutIndex1 1) `shouldReturn` Nothing
  where
    portfolioKeys :: Natural -> IO [(Network, AccountKind, XPubKey)]
    portfolioKeys index = do
      seed <- expectRight $ seedOf testMnemonic
      maybe (fail "The seed has no portfolio at the index") return =<< seedPortfolioKeys seed index
    -- The keys of the accounts of a kind on a network, as their network
    -- writes them
    keyOf :: [(Network, AccountKind, XPubKey)] -> Network -> AccountKind -> [Text]
    keyOf keys net kind = [xPubExport net key | (keyNet, keyKind, key) <- keys, keyNet == net, keyKind == kind]

{- The path of an address in a portfolio -}

poolPathSpec :: Spec
poolPathSpec =
  describe "The path of an address of a portfolio on a network" $ do
    it "is named with the type of its account in front, since both accounts have an address at /0/3" $ do
      poolPathText (PoolPath Bip84 (Deriv :/ 0 :/ 3)) `shouldBe` "bip84/0/3"
      poolPathText (PoolPath Bip44 (Deriv :/ 0 :/ 3)) `shouldBe` "bip44/0/3"

    it "is a path of the bip44 account when nothing says its type, as every path was before portfolios" $
      legacyPath (Deriv :/ 1 :/ 0) `shouldBe` PoolPath Bip44 (Deriv :/ 1 :/ 0)

    -- hw writes the type of every path it stores
    it "is refused in JSON without the type of its account" $
      (Json.eitherDecode "{\"path\":\"/0/3\"}" :: Either String PoolPath)
        `shouldBe` Left "Error in $: key \"type\" not found"

    it "is refused in JSON with a type hw does not hold" $
      (Json.eitherDecode "{\"type\":\"bip49\",\"path\":\"/0/3\"}" :: Either String PoolPath)
        `shouldBe` Left "Error in $.type: Unknown account type bip49"

    it "tells the type of account of an address by its form: P2PKH for bip44, P2WPKH for bip84, none for another form" $ do
      addressKind (externalAddr 0) `shouldBe` Just Bip44
      addressKind (segwitExternalAddr 0) `shouldBe` Just Bip84
      addressKind (wrappedExternalAddr 0) `shouldBe` Nothing
      addressKind (taprootExternalAddr 0) `shouldBe` Nothing

{- The kinds of accounts hw does not hold -}

foreignKindSpec :: Spec
foreignKindSpec =
  describe "The kinds of accounts hw does not hold" $ do
    it "are named after their BIP, the bip44 accounts of Bitcoin by theirs, and described by what sets them apart" $ do
      map foreignKindName [minBound .. maxBound] `shouldBe` ["bip49", "bip86", "bip44"]
      foreignKindNamed "bip86" `shouldBe` Just Bip86
      foreignKindNamed "bip84" `shouldBe` Nothing
      map foreignKindDescription [minBound .. maxBound] `shouldBe` ["bip49, wrapped SegWit", "bip86, Taproot", "bip44 with the coin type of Bitcoin"]

    it "set the purpose of the path of an account, and the coin type of Bitcoin for its bip44 accounts" $ do
      pathToStr (foreignDeriv Bip49 btc 0) `shouldBe` "/49'/0'/0'"
      pathToStr (foreignDeriv Bip86 btcTestnet3 3) `shouldBe` "/86'/1'/3'"
      pathToStr (foreignDeriv Bip44OfBitcoin bch 2) `shouldBe` "/44'/0'/2'"

    it "are bip49 and bip86 on the networks of Bitcoin, and the bip44 accounts of Bitcoin on Bitcoin Cash alone, whose test networks have the coin type of Bitcoin's" $ do
      networkForeignKinds btc `shouldBe` [Bip49, Bip86]
      networkForeignKinds btcTestnet3 `shouldBe` [Bip49, Bip86]
      networkForeignKinds bch `shouldBe` [Bip44OfBitcoin]
      networkForeignKinds bchTest3 `shouldBe` []
      networkForeignKinds bchRegTest `shouldBe` []

    -- The keys of the bip49 and bip86 accounts 0 of the test wallet, derived
    -- apart from hw with the Python tool of the message-signing vectors
    it "have their keys derived from the public side of the seed, along their paths" $ do
      seed <- expectRight $ seedOf testMnemonic
      (fmap (xPubExport btc) <$> seedForeignKey seed Bip49 btc 0) `shouldReturn` Just testWrappedAccountXPub
      (fmap (xPubExport btc) <$> seedForeignKey seed Bip86 btc 0) `shouldReturn` Just testTaprootAccountXPub
      (fmap (xPubExport btc) <$> seedForeignKey seed Bip44OfBitcoin bch 0) `shouldReturn` Just testAccountXPub

    it "give a key its address: P2SH-P2WPKH for bip49, P2TR for bip86, P2PKH for the bip44 accounts of Bitcoin" $ do
      (addrToText btc =<< foreignAddress Bip49 btc (derivePubPath firstKey testWrappedAccountPub)) `shouldBe` Just (wrappedExternalAddrsText !! 0)
      (addrToText btc =<< foreignAddress Bip86 btc (derivePubPath firstKey testTaprootAccountPub)) `shouldBe` Just (taprootExternalAddrsText !! 0)
      foreignAddress Bip44OfBitcoin bch (derivePubPath firstKey testAccountPub) `shouldBe` Just (externalAddr 0)

    -- A Taproot output is a SegWit one, which Bitcoin Cash does not have
    it "give a key of bip86 no address on Bitcoin Cash, which has no SegWit" $
      foreignAddress Bip86 bch (derivePubPath firstKey testTaprootAccountPub) `shouldBe` Nothing

    -- The vectors of BIP 49 and BIP 86, for the mnemonic of eleven times
    -- "abandon" and "about", without a passphrase. BIP 86 tweaks the key
    -- with an even y coordinate, which these vectors catch.
    it "derive the addresses of the test vectors of BIP 49 and BIP 86" $ do
      seed <- expectRight $ seedOf abandonMnemonic
      wrapped <- maybe (fail "The key is invalid") return =<< seedForeignKey seed Bip49 btcTestnet3 0
      (addrToText btcTestnet3 =<< foreignAddress Bip49 btcTestnet3 (derivePubPath firstKey wrapped)) `shouldBe` Just "2Mww8dCYPUpKHofjgcXcBCEGmniw9CoaiD2"
      taproot <- maybe (fail "The key is invalid") return =<< seedForeignKey seed Bip86 btc 0
      xPubExport btc taproot `shouldBe` "xpub6BgBgsespWvERF3LHQu6CnqdvfEvtMcQjYrcRzx53QJjSxarj2afYWcLteoGVky7D3UKDP9QyrLprQ3VCECoY49yfdDEHGCtMMj92pReUsQ"
      let addressAt path = addrToText btc =<< foreignAddress Bip86 btc (derivePubPath path taproot)
      addressAt (Deriv :/ 0 :/ 0) `shouldBe` Just "bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr"
      addressAt (Deriv :/ 0 :/ 1) `shouldBe` Just "bc1p4qhjn9zdvkux4e44uhx8tc55attvtyu358kutcqkudyccelu0was9fqzwh"
      addressAt (Deriv :/ 1 :/ 0) `shouldBe` Just "bc1p3qkhfews2uk44qtvauqyr2ttdsw7svhkl9nkm9s9c3x4ax5h60wqwruhk7"
  where
    firstKey = Deriv :/ 0 :/ 0 :: SoftPath
    abandonMnemonic = MnemonicPass "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about" ""

{- The seed of a backup in shares -}

sharesSeedSpec :: Spec
sharesSeedSpec = do
  describe "The seed of a backup in shares" $ do
    it "has the master key the device tests of a hardware wallet give for its passphrase" $
      xPrvExport btc <$> secretMasterKey testSharesSecret `shouldBe` Right testSharesMasterKey

    -- Any passphrase decrypts the backup to a seed: the master key of the
    -- seed 4ba7d31410fa0996f02ffb7cc3b992a1, derived apart from hw
    it "has another master key without its passphrase, and no error" $
      xPrvExport btc <$> secretMasterKey (SharesSecret testSharesBackup "")
        `shouldBe` Right "xprv9s21ZrQH143K3reExTJbGTHPu6mGuUx6yN1H1KkqoiAcw6j1t6hBiwwnXYxNQXxU8T7pANSv1rJYQPXn1LMQk1gbWes5BjSz3rJ5ZfH1cc3"

    -- The format of the shares asks for a passphrase of printable ASCII:
    -- one with an accent has several encodings that look alike, each of
    -- which would give another seed
    it "has no master key for a passphrase with an accent, which the format of the shares does not take" $
      xPrvExport btc <$> secretMasterKey (SharesSecret testSharesBackup "caf\233")
        `shouldBe` Left "The passphrase must contain only printable ASCII characters, code points 32 to 126"

    -- BIP32 takes a seed of 16 to 64 bytes. The format of the shares lets
    -- a backup hold more: 66 bytes here.
    it "is refused when the backup holds more bytes than a seed has" $
      xPrvExport btc <$> secretMasterKey (SharesSecret (EncryptedMasterSecret 10282 False 0 (BS.replicate 66 0)) "")
        `shouldBe` Left "The backup holds a secret of 66 bytes, which is not a seed: a seed has 16 to 64 bytes"

    it "derives the keys of its bip44 account 0 on testnet, m/44'/1'/0'" $ do
      master <- expectRight $ secretMasterKey testSharesSecret
      xPrvExport btcTestnet3 (accountKey Bip44 btcTestnet3 master 0) `shouldBe` testSharesTestnetXPrv
      xPubExport btcTestnet3 (deriveXPubKey (accountKey Bip44 btcTestnet3 master 0)) `shouldBe` testSharesTestnetXPub

    -- The first one is the address the device tests give for the seed
    it "derives the first two addresses of that account" $ do
      let account = fromJust $ xPubImport btcTestnet3 testSharesTestnetXPub
          addressAt i = addrToText btcTestnet3 (keyAddress Bip44 (derivePubPath (Deriv :/ 0 :/ i) account))
      map addressAt [0, 1] `shouldBe` map Just testSharesTestnetAddrs

    it "derives the public key of its bip44 account 0 on Bitcoin from the public side of the seed, and its first address" $ do
      seed <- expectRight $ secretSeed testSharesSecret
      account <- seedAccountKey seed Bip44 btc 0
      fmap (xPubExport btc) account `shouldBe` Just testSharesXPub
      fmap (addrToText btc . keyAddress Bip44 . derivePubPath (Deriv :/ 0 :/ 0)) account `shouldBe` Just (Just testSharesAddr)

    it "has the fingerprint of its master public key" $
      fingerprintToText . seedFingerprint <$> secretSeed testSharesSecret `shouldBe` Right testSharesFingerprintText

  describe "The seed of a mnemonic, as the secret a command asks for" $ do
    it "is the seed BIP39 derives" $
      fingerprintToText . seedFingerprint <$> secretSeed (MnemonicSecret testMnemonic) `shouldBe` Right testFingerprintText

    it "gives the account key of the test wallet" $
      (\master -> xPrvExport btc (accountKey Bip44 btc master 0)) <$> secretMasterKey (MnemonicSecret testMnemonic) `shouldBe` Right testAccountXPrv

{- A master private key, given in place of the mnemonic -}

masterKeySpec :: Spec
masterKeySpec = do
  -- The account key m/44'/0'/0' of the test wallet tells which master key
  -- was read
  let account0 master = xPrvExport btc (accountKey Bip44 btc master 0)
  describe "A master private key as the user gives it" $ do
    it "is read from a text that starts with xprv" $
      account0 <$> readMasterKey testMasterXPrv `shouldBe` Right testAccountXPrv

    it "is the same key from the text a tool set to a test network writes, which starts with tprv" $
      account0 <$> readMasterKey testMasterTPrv `shouldBe` Right testAccountXPrv

    it "is read with spaces around it, as the line of a file may have" $
      account0 <$> readMasterKey ("  " <> testMasterXPrv <> " \r") `shouldBe` Right testAccountXPrv

    it "is refused when it is mistyped, without repeating it" $
      readMasterKey testMasterXPrvTypo
        `shouldBe` Left "The master private key does not read: it is mistyped, or it is not a key that starts with xprv or tprv"

    it "is refused when it is a public key" $
      readMasterKey testAccountXPub
        `shouldBe` Left "The master private key does not read: it is mistyped, or it is not a key that starts with xprv or tprv"

    -- The key of the account m/44'/0'/0' is three steps below its master key
    it "is refused when it is the key of an account, with its depth" $
      readMasterKey testAccountXPrv
        `shouldBe` Left "This is not a master private key: it is a key derived from one, at depth 3"

  describe "The master private key of a secret, as newportfolio --show-master-key prints it" $ do
    -- The key Python derives from the words and the passphrase
    it "is the master key BIP39 and BIP32 derive from a mnemonic and its passphrase, written with xprv" $
      masterKeyText (MnemonicSecret testMnemonic) `shouldBe` Right testMasterXPrv

    it "is written with xprv for a key that was given with tprv" $
      (masterKeyText . MasterKeySecret =<< readMasterKey testMasterTPrv) `shouldBe` Right testMasterXPrv

  describe "The seed of a master private key, as the secret a command asks for" $ do
    it "is the seed of the wallet of that key" $
      fingerprintToText . seedFingerprint <$> secretSeed (MasterKeySecret testMasterKey) `shouldBe` Right testFingerprintText

    it "gives the account key of the test wallet" $
      account0 <$> secretMasterKey (MasterKeySecret testMasterKey) `shouldBe` Right testAccountXPrv

{- A secret, shown -}

-- | The secret of a seed as a log, an error message or a failed test would
-- print it
shownSecretSpec :: Spec
shownSecretSpec =
  describe "The secret of a seed, shown" $ do
    it "hides the words and the passphrase of a mnemonic" $
      show testMnemonic `shouldBe` "MnemonicPass {mnemonicWords = <redacted>, mnemonicPass = <redacted>}"

    it "hides the mnemonic a command asked for" $
      show (MnemonicSecret testMnemonic)
        `shouldBe` "MnemonicSecret (MnemonicPass {mnemonicWords = <redacted>, mnemonicPass = <redacted>})"

    -- 10282 is the identifier every share of the backup carries. It is not
    -- secret.
    it "hides the bytes and the passphrase of a backup in shares, and keeps its identifier" $
      show testSharesSecret
        `shouldBe` "SharesSecret (EncryptedMasterSecret {identifier = 10282, extendable = False, iterationExponent = 0, ciphertext = <redacted>}) <redacted>"

    it "hides a master private key whole" $
      show (MasterKeySecret testMasterKey) `shouldBe` "MasterKeySecret <redacted>"

    it "goes in parentheses inside another value, as any value does" $
      show (Just (MnemonicSecret testMnemonic))
        `shouldBe` "Just (MnemonicSecret (MnemonicPass {mnemonicWords = <redacted>, mnemonicPass = <redacted>}))"

{- The passphrase, as BIP39 hashes it -}

-- | The master key of the words of the test wallet with the passphrase
-- "café" in NFKD form, as BIP39 asks, computed apart from hw with Python's
-- hashlib, hmac and unicodedata, by the PBKDF2 and HMAC of BIP39 and
-- BIP32; the same script gives the master key of the official BIP39
-- vector "abandon … about" with "TREZOR".
cafeMasterNormalized :: Text
cafeMasterNormalized = "xprv9s21ZrQH143K28mmRktNKJK6miBqMSwxLLpJEZaDyhTf18YbxXC6paThZQypFWYoD3GRxuRJBc9hfdBc3YvrhfVn3gNJMdRFfcvw2Vu91R9"

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
    (encodeUtf8 (normalize NFKD mnemonic))
    ("mnemonic" <> encodeUtf8 (normalize NFKD passphrase))

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
      normalize NFKD japanesePassphrase `shouldNotBe` japanesePassphrase

    forM_ japaneseVectors $ \(name, vector) ->
      it ("gives the seed and the master key of the Japanese vector " <> name) $ do
        let seed = bip39Seed (japaneseMnemonic vector) japanesePassphrase
        encodeHex seed `shouldBe` japaneseSeed vector
        xPrvExport btc (makeXPrvKey seed) `shouldBe` japaneseMaster vector

    forM_ trezorVectors $ \(name, passphrase, master) ->
      it ("gives the master key Trezor's python-mnemonic gives for " <> name) $
        xPrvExport btc <$> masterKey (withPassphrase passphrase) `shouldBe` Right master

{- The accounts of known wallets -}

-- | A wallet made elsewhere: its mnemonic and its passphrase, and the first
-- receiving and change addresses of its first Bitcoin account, at
-- m/44'/0'/0'/0/0 and m/44'/0'/0'/1/0
data KnownWallet = KnownWallet
  { knownMnemonic :: Text,
    knownPassphrase :: Text,
    knownReceiving :: Text,
    knownChange :: Text
  }

-- | Wallets of every mnemonic length, some with a passphrase. They were
-- added in 2020 without a source. On 2026-10-25 every address was checked
-- apart from hw, with the pure-Python BIP39 and BIP32 of
-- test/data/message-signing/tools/btc.py. The last wallet is the one of
-- iancoleman/bip39 issue 58: its seed is BIP32's test vector 3, whose
-- master private key starts with a zero byte.
knownWallets :: [(String, KnownWallet)]
knownWallets =
  [ ( "a wallet of 12 words without a passphrase",
      KnownWallet
        "modify truck lens identify brief coffee gather volcano fatal together muscle elephant"
        ""
        "1KiWbwzHhwH2KdLyreGuJq36SP2pPeGeim"
        "1BY9FShyC6rnEUdPAVQTX37TFZqEkR1enQ"
    ),
    ( "the same words with the passphrase \"password\"",
      KnownWallet
        "modify truck lens identify brief coffee gather volcano fatal together muscle elephant"
        "password"
        "1Ha18XQ74YfCnuUAjGTF9rnbS7pm1aCzW4"
        "17SounFZ11k8urp9Pee2FNYjzGU5qwLUfT"
    ),
    ( "the same words with the passphrase \"Hello world\", which has a space",
      KnownWallet
        "modify truck lens identify brief coffee gather volcano fatal together muscle elephant"
        "Hello world"
        "1KpHPSa9eMohQznkddzmHT9AeMAscYe29Y"
        "16WJrKzdik1HgnoiNiLUW2uGDqry2fSGVx"
    ),
    ( "a wallet of 15 words",
      KnownWallet
        "fiscal gadget drastic coconut awful crime during common salmon manage random cost evil owner city"
        ""
        "131dALC6WxwTZzTsHUsifAJ8bkQdDEeeqJ"
        "1JHCmBLzDARCdDgn2WwJQr88tGW8V3EyKj"
    ),
    ( "a wallet of 18 words",
      KnownWallet
        "host mind elephant tone sound apple service tomato subject attend motion stick fuel fan rail bamboo tree build"
        ""
        "15nbJ6oHvYiapKu1vBYmyQzVSTjiJNcfuV"
        "12rMvadbhRxFaUP4FoRUmABa1JEPLrrqHL"
    ),
    ( "a wallet of 21 words",
      KnownWallet
        "flight skill wisdom mixture patch dirt trouble behind chair glad detect swarm swap truly cruise medal walnut glide wrestle route defy"
        ""
        "12uDDJU7BJUxJomzbFt5BW3GnukGZz6Y5t"
        "1Gv62T4UkLxJ6dnsmNXENMgjBhLzEzJ1Sc"
    ),
    ( "a wallet of 24 words",
      KnownWallet
        "use turtle trap pause spin venue hazard hope slot april cattle fork stand finish arrest nasty acoustic clog entire course universe evil desert produce"
        ""
        "19fJdt5HjJUrmnvKM8fTS43o9aK3NmFZMT"
        "17Kmofo8651DdydBS7zVVy9JLkixkxj6SK"
    ),
    ( "the same words with a passphrase of 256 characters",
      KnownWallet
        "use turtle trap pause spin venue hazard hope slot april cattle fork stand finish arrest nasty acoustic clog entire course universe evil desert produce"
        "nMCm#Qe7u-*LG^99KS%hSNZJh6w&e&w4S&7HA-22^7vDE#F\
        \@=hNZZZ$M=ZSYG42J8P+U+3sf$xS6YpJf7DemaU#fLEVBW8\
        \!FmugPj=K=E*KDmUuQh_2%7K2W8@RJ4?Pua2HFCb8$@ZB^p\
        \s=T=PNFy5APBBdaz_eTq&r&v3hp6P-tTfvNP9J+xfQ-FKZS\
        \7_?g+P*9MTkpjy*K5=8y+y9db#Sv+b*5bfmV8?@YKrf#AEH\
        \7!ARqBXMhS7d%GYAxB%RG"
        "1L1oKPYJ9j3EFX5CcFXz1d3kkAW9Hoh5ic"
        "1J7jpmP7NVoryofiVSCKMMvMitkCP3U6Qj"
    ),
    -- https://github.com/iancoleman/bip39/issues/58
    ( "the wallet of iancoleman/bip39 issue 58, whose master private key starts with a zero byte",
      KnownWallet
        "fruit wave dwarf banana earth journey tattoo true farm silk olive fence"
        "banana"
        "17rxURoF96VhmkcEGCj5LNQkmN9HVhWb7F"
        "16wbbYdecq9QzXdxa58q2dYXJRc8sfkE4J"
    )
  ]

-- | The first receiving and change addresses of the first Bitcoin account
-- of a mnemonic and a passphrase, as hw gives them once portfolio 0 of the
-- wallet is in the database
firstAddresses :: Text -> Text -> ExceptT String (DB IO) (Text, Text)
firstAddresses mnemonic passphrase = do
  (accId, _) <- newAccountOf (MnemonicPass mnemonic passphrase) btc
  receiving <- genExtAddress def accId ""
  change <- nextChangeAddress def accId []
  return (dBAddressAddress receiving, dBAddressAddress change)

knownWalletsSpec :: Spec
knownWalletsSpec =
  describe "The account of a known wallet" $
    forM_ knownWallets $ \(name, wallet) ->
      it ("gives the first receiving and change addresses of " <> name) $
        inDatabaseE $
          firstAddresses (knownMnemonic wallet) (knownPassphrase wallet)
            `dbShouldBeE` (knownReceiving wallet, knownChange wallet)

{- Private keys that start with a zero byte -}

-- BIP32's test vectors 3 and 4 check that an implementation keeps the zero
-- bytes a private key starts with. hw derives every account along hardened
-- paths, where the child of a private key comes from its 32 bytes, the
-- leading zeros included.

-- | The seed of BIP32's test vector 3. Its master key starts with a zero
-- byte, and m/0' is its hardened child. It is the seed of the wallet of
-- iancoleman/bip39 issue 58 above, and haskoin-core tests it too.
--
-- bitpay/bitcore-lib#47 and iancoleman/bip39#58
-- https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#test-vector-3
vector3Seed :: ByteString
vector3Seed = fromJust $ decodeHex "4b381541583be4423346c643850da4b320e46a87ae3d2a4e6da11eba819cd4acba45d239319ac14f863b8d5ab5a0d0c64d2e8a1e7d1457df2e5a3c51c73235be"

-- | The seed of BIP32's test vector 4. Its key at m/0' starts with a zero
-- byte, and m/0'/1' is its hardened child. haskoin-core's own tests stop
-- at vector 3.
--
-- btcsuite/btcutil#172
-- https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#test-vector-4
vector4Seed :: ByteString
vector4Seed = fromJust $ decodeHex "3ddd5602285899a946114506157c7997e5444528f3003f6134712147db19b678"

-- | The private and public keys at a path of a seed
keysAt :: ByteString -> HardPath -> (Text, Text)
keysAt seed path = (xPrvExport btc key, xPubExport btc (deriveXPubKey key))
  where
    key = derivePath path (makeXPrvKey seed)

leadingZeroSpec :: Spec
leadingZeroSpec =
  describe "A private key that starts with a zero byte, in BIP32's test vectors" $ do
    it "is exported with its zero byte: the master key of vector 3" $
      keysAt vector3Seed Deriv
        `shouldBe` ( "xprv9s21ZrQH143K25QhxbucbDDuQ4naNntJRi4KUfWT7xo4EKsHt2QJDu7KXp1A3u7Bi1j8ph3EGsZ9Xvz9dGuVrtHHs7pXeTzjuxBrCmmhgC6",
                     "xpub661MyMwAqRbcEZVB4dScxMAdx6d4nFc9nvyvH3v4gJL378CSRZiYmhRoP7mBy6gSPSCYk6SzXPTf3ND1cZAceL7SfJ1Z3GC8vBgp2epUt13"
                   )

    it "gives the hardened child BIP32 gives: m/0' of vector 3" $
      keysAt vector3Seed (Deriv :| 0)
        `shouldBe` ( "xprv9uPDJpEQgRQfDcW7BkF7eTya6RPxXeJCqCJGHuCJ4GiRVLzkTXBAJMu2qaMWPrS7AANYqdq6vcBcBUdJCVVFceUvJFjaPdGZ2y9WACViL4L",
                     "xpub68NZiKmJWnxxS6aaHmn81bvJeTESw724CRDs6HbuccFQN9Ku14VQrADWgqbhhTHBaohPX4CjNLf9fq9MYo6oDaPPLPxSb7gwQN3ih19Zm4Y"
                   )

    it "has the parent BIP32 gives: the master key of vector 4" $
      keysAt vector4Seed Deriv
        `shouldBe` ( "xprv9s21ZrQH143K48vGoLGRPxgo2JNkJ3J3fqkirQC2zVdk5Dgd5w14S7fRDyHH4dWNHUgkvsvNDCkvAwcSHNAQwhwgNMgZhLtQC63zxwhQmRv",
                     "xpub661MyMwAqRbcGczjuMoRm6dXaLDEhW1u34gKenbeYqAix21mdUKJyuyu5F1rzYGVxyL6tmgBUAEPrEz92mBXjByMRiJdba9wpnN37RLLAXa"
                   )

    it "is exported with its zero byte: m/0' of vector 4" $
      keysAt vector4Seed (Deriv :| 0)
        `shouldBe` ( "xprv9vB7xEWwNp9kh1wQRfCCQMnZUEG21LpbR9NPCNN1dwhiZkjjeGRnaALmPXCX7SgjFTiCTT6bXes17boXtjq3xLpcDjzEuGLQBM5ohqkao9G",
                     "xpub69AUMk3qDBi3uW1sXgjCmVjJ2G6WQoYSnNHyzkmdCHEhSZ4tBok37xfFEqHd2AddP56Tqp4o56AePAgCjYdvpW2PU2jbUPFKsav5ut6Ch1m"
                   )

    it "gives the hardened child BIP32 gives: m/0'/1' of vector 4" $
      keysAt vector4Seed (Deriv :| 0 :| 1)
        `shouldBe` ( "xprv9xJocDuwtYCMNAo3Zw76WENQeAS6WGXQ55RCy7tDJ8oALr4FWkuVoHJeHVAcAqiZLE7Je3vZJHxspZdFHfnBEjHqU5hG1Jaj32dVoS6XLT1",
                     "xpub6BJA1jSqiukeaesWfxe6sNK9CCGaujFFSJLomWHprUL9DePQ4JDkM5d88n49sMGJxrhpjazuXYWdMf17C9T5XnxkopaeS7jGk1GyyVziaMt"
                   )

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
      parseSecKey btcTestnet3 wikiKeyTestnetWif `shouldBe` Just wikiKey
      parseSecKey btcTestnet3 wikiKeyTestnetWifUncompressed `shouldBe` Just wikiKey
      parseSecKey btc wikiKeyTestnetWif `shouldBe` Nothing
      parseSecKey btcTestnet3 wikiKeyWif `shouldBe` Nothing
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
    it "refuses a line with words but no key, naming the line by its number alone" $
      parseSecKeys btc (wikiKeyWif <> "\n\nmy old wallet\n")
        `shouldBe` Left "No private key on line 3"

  -- The keys are typed hidden, so the user cannot see a typo. A word meant
  -- as a key that does not read was taken for a label and dropped.
  describe "A word of a key file that was meant as a private key and is not one" $ do
    -- The key of the Bitcoin wiki's WIF page with its last character
    -- changed: one try per character of that place gives the key back
    it "is refused beside a good key, by its place and its line, with the reason: a wrong checksum" $
      parseSecKeys btc ("# The old wallet\n" <> wikiKeyWif <> " " <> mistypedWif <> "\n")
        `shouldBe` Left "Word 2 of line 2 has the form of a private key, but its checksum is wrong, as when a character is mistyped. No key was read"
    it "is not printed back in the refusal" $
      parseSecKeys btc mistypedWif `shouldSatisfy` either (not . isInfixOf (Text.unpack mistypedWif)) (const False)
    it "is told as a key of btc or bch when the keys are read for a test network" $
      parseSecKeys btcTestnet3 (wikiKeyWif <> "\n")
        `shouldBe` Left "Word 1 of line 1 is a private key of btc or bch, and the keys are read for btctest. No key was read"
    it "is told as a key of a test network when the keys are read for btc" $
      parseSecKeys btc (wikiKeyTestnetWif <> "\n")
        `shouldBe` Left "Word 1 of line 1 is a private key of a test network, and the keys are read for btc. No key was read"
    -- The WIF of the wiki without its last character: 51 characters
    it "is told its length when a character of a WIF was lost" $
      parseSecKeys btc "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP9861"
        `shouldBe` Left "Word 1 of line 1 has the letters of a private key, but not its length: it has 51 characters, and a key in WIF has 51 when it starts with 5 or 9, and 52 when it starts with K, L or c. No key was read"
    -- The hexadecimal key of the wiki without its last two digits
    it "is told its length when digits of a hexadecimal key were lost" $
      parseSecKeys btc "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa"
        `shouldBe` Left "Word 1 of line 1 has 62 hexadecimal digits, and a private key has 64. No key was read"
    it "is told to drop the 0x before the digits of a hexadecimal key" $
      parseSecKeys btc ("0x" <> wikiKeyHex)
        `shouldBe` Left "Word 1 of line 1 is written with 0x: write the 64 hexadecimal digits of a private key alone. No key was read"
    it "is passed over in a comment" $
      parseSecKeys btc ("# was " <> mistypedWif <> "\n" <> wikiKeyWif <> "\n") `shouldBe` Right [wikiKey]
  where
    mistypedWif = "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98618"
    keyFile =
      "# The keys of the old wallet\n"
        <> wikiKeyWif <> " first key\n"
        <> "\n"
        <> "  # indented comment\n"
        <> miniKey <> "\n"
        <> wikiKeyWifUncompressed <> " " <> wikiKeyHex <> " two keys on a line\n"
