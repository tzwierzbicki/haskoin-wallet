{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An account as the user keeps it, from createacc to deleteacc: its
-- descriptor, for other wallets and from them, its
-- addresses and their labels, an address checked on the offline computer,
-- its mnemonic typed in parts, the messages its keys sign and check, the
-- backup of its database, and its recovery from a seed, with a passphrase
-- as typed.
module Haskoin.Wallet.Workflow.AccountSpec (spec) where

import Control.Monad (forM_, replicateM_, void)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Control.Monad.Except (runExceptT)
import Data.List (isInfixOf, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..), runDBWith)
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (MnemonicPass (..))
import Haskoin.Wallet.Migration (databaseVersion)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..))
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData (..))
import Haskoin.Wallet.Workflow.TestUtils
import System.Directory (copyFile, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  accountSpec
  segwitSpec ctx
  descriptorSpec
  addressSpec ctx
  checkAddrSpec
  splitSpec
  sharesSpec ctx
  messageSpec
  verifySpec
  backupSpec
  recoverSpec ctx
  passphraseSpec ctx

{- An account -}

accountSpec :: Spec
accountSpec =
  describe "An account, from createacc to deleteacc" $ do
    it "is created from the mnemonic and the passphrase, each typed twice, at the first index of the seed" $
      withHome $ \home ->
        (keyOf <$> hwJson home ["createacc", "main"] seedTwice) `shouldReturn` Right ("main", 0, testAccountXPub)

    it "is listed by accounts, with what it can spend" $
      withHome $ \home -> do
        createMain home
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    it "is listed alone by accounts --account" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        (accountsOf <$> hwJson home ["accounts", "--account", "second"] []) `shouldReturn` Right [("second", 1, 0)]

    -- Nothing has paid the accounts: nothing to spend, and no coins
    it "is listed by accounts in two lines, a blank line between accounts" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        run <- hw home ["accounts"] []
        runOut run `shouldBe` "main  btc  /44'/0'/0'\n  spendable 0.00000000 btc, no coins\n\nsecond  btc  /44'/0'/1'\n  spendable 0.00000000 btc, no coins\n"

    it "is listed by accounts --verbose with the fingerprint of its seed" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["accounts", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n  Fingerprint: " <> testFingerprintText <> "\n")

    -- The time is in the time zone of the computer, Central Europe here
    it "is listed by accounts --verbose with the time it was created, to the second" $
      withHome $ \home -> do
        createMain home
        run <- hwWithEnv [("TZ", centralEurope)] home ["accounts", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Created    : "
        runOut run `shouldSatisfy` (\out -> " CET\n" `Text.isInfixOf` out || " CEST\n" `Text.isInfixOf` out)

    it "takes the next index of a seed that has an account already" $
      withHome $ \home -> do
        createMain home
        (keyOf <$> hwJson home ["createacc", "second"] seedTwice) `shouldReturn` Right ("second", 1, testAccountXPub1)

    it "is refused a name the wallet has, before the mnemonic is asked" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["createacc", "main"] seedTwice
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The account main already exists"
        run `shouldNotSatisfy` askedMnemonic

    -- The configuration file of the test home names btc
    it "is created on the default network of the configuration file, or on the --network given" $
      withHome $ \home -> do
        setDefaultNetwork home (Just "bch")
        (fmap dBAccountNetwork . accountOf <$> hwJson home ["createacc", "main"] seedTwice) `shouldReturn` Right "bch"
        (fmap dBAccountNetwork . accountOf <$> hwJson home ["createacc", "second", "--network", "btc"] seedTwice) `shouldReturn` Right "btc"

    it "needs the --network without a default network in the configuration file, before the mnemonic is asked" $
      withHome $ \home -> do
        setDefaultNetwork home Nothing
        run <- hw home ["createacc", "main"] seedTwice
        run `shouldSatisfy` printed "Give the --network, or a default-network in the configuration file"
        run `shouldNotSatisfy` askedMnemonic

    it "is checked by checkmnemonic against a mnemonic and a passphrase" $
      withHome $ \home -> do
        createMain home
        matched <- hwJson home ["checkmnemonic"] seedOnce
        matched `shouldSatisfy` \case ResponseAccResult _ True _ -> True; _ -> False
        missed <- hwJson home ["checkmnemonic"] [mnemonicWords testMnemonic, "wrong passphrase"]
        missed `shouldSatisfy` \case ResponseAccResult _ False "The mnemonic and passphrase did not match the account" -> True; _ -> False

    -- other is of the second seed, and main of the first
    it "is checked by checkmnemonic --account, whose seed is the one to type" $
      withHome $ \home -> do
        createMain home
        let second = [mnemonicWords testMnemonic2, mnemonicPass testMnemonic2]
        _ <- answer home ["createacc", "other"] (concatMap (replicate 2) second)
        matched <- hwJson home ["checkmnemonic", "--account", "other"] second
        matched `shouldSatisfy` \case ResponseAccResult acc True _ -> dBAccountName acc == "other"; _ -> False
        missed <- hwJson home ["checkmnemonic", "--account", "other"] seedOnce
        missed `shouldSatisfy` \case ResponseAccResult _ False _ -> True; _ -> False

    it "is checked by checkmnemonic --parts against the parts of its mnemonic" $
      withHome $ \home -> do
        createMain home
        matched <- hwJson home ["checkmnemonic", "--parts", "2"] partsOnce
        matched `shouldSatisfy` \case ResponseAccResult _ True _ -> True; _ -> False

    -- The key of account 0 on Bitcoin, imported on Bitcoin Cash, where an
    -- xpub reads the same
    it "is told by checkmnemonic to be of another network, when the mnemonic gives its key there" $
      withHome $ \home -> do
        writeFile (home </> "main.txt") (Text.unpack testAccountXPub <> "\n")
        _ <- answer home ["importacc", home </> "main.txt", "--network", "bch"] []
        (accResultOf <$> hwJson home ["checkmnemonic"] seedOnce)
          `shouldReturn` Right (False, "The mnemonic and passphrase did not match the account, but they give its key at /44'/0'/0' on btc: it is an account of btc, not of bch. Delete it with deleteacc and import it again with --network btc.")

    it "is renamed by renameacc" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["renameacc", "main", "savings"] []
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("savings", 0, 0)]

    it "is deleted by deleteacc, given its network and its path" $
      withHome $ \home -> do
        createMain home
        deleted <- hwJson home ["deleteacc", "main", "btc", "/44'/0'/0'"] []
        deleted `shouldSatisfy` \case ResponseAccResult _ True "The account main has been deleted" -> True; _ -> False
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

    it "is deleted by deleteacc, which says so in one line" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["deleteacc", "main", "btc", "/44'/0'/0'"] []
        runOut run `shouldBe` "The account main has been deleted\n"

    it "is not deleted for a network or a path that is not its own" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["deleteacc", "main", "bch", "/44'/0'/0'"] [])
          `shouldReturn` Right "The network of the account to delete did not match"
        (errorOf <$> hwJson home ["deleteacc", "main", "btc", "/44'/0'/1'"] [])
          `shouldReturn` Right "The full derivation path of the account to delete did not match"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    it "is written to the file exportacc names, which exportacc says" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["exportacc", home </> "main.json"] []
        runOut run `shouldBe` "Written to " <> Text.pack (home </> "main.json") <> "\n"

    -- As an offline signer hands its accounts to an online computer
    it "is exported to a file by exportacc, and imported from it in another wallet by importacc" $
      withHome $ \signer -> withHome $ \online -> do
        createMain signer
        _ <- answer signer ["exportacc", signer </> "main.json"] []
        (keyOf <$> hwJson online ["importacc", signer </> "main.json"] []) `shouldReturn` Right ("main", 0, testAccountXPub)

{- Descriptors -}

-- | The descriptor of main, as hw writes it: checked with Bitcoin Core
-- 32.1 (test/data/descriptors/README.md)
mainDescriptor :: Text
mainDescriptor = "pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j87"

{- A bip84 account -}

-- | A chain whose block 800000 pays 50000 to the first receiving address
-- of the bip84 account of the test wallet
segwitPaidChain :: Ctx -> Chain
segwitPaidChain ctx = Chain 800002 [(fundingTx ctx 3 [(segwitExternalAddr 0, 50000)], Just 800000)]

-- | The descriptor of the bip84 account, as the Coldcard file of
-- test/data/descriptors writes it
segwitDescriptor :: Text
segwitDescriptor = "wpkh([bc560f95/84h/0h/0h]" <> testSegwitAccountXPub <> "/<0;1>/*)#5nktextl"

-- | Create the bip84 account spending of the test wallet in a home
createSpending :: FilePath -> IO ()
createSpending home = void $ answer home ["createacc", "spending", "--type", "bip84"] seedTwice

segwitSpec :: Ctx -> Spec
segwitSpec ctx =
  describe "A native SegWit account, of the kind bip84" $ do
    it "is created by createacc --type bip84 at the first bip84 index of the seed, under the path m/84'/0'/0'" $
      withHome $ \home -> do
        createMain home
        (keyOf <$> hwJson home ["createacc", "spending", "--type", "bip84"] seedTwice) `shouldReturn` Right ("spending", 0, testSegwitAccountXPub)
        run <- hw home ["accounts"] []
        run `shouldSatisfy` printed "main  btc  /44'/0'/0'"
        run `shouldSatisfy` printed "spending  btc  /84'/0'/0'"

    it "is refused on Bitcoin Cash, which has no SegWit, before the mnemonic is asked" $
      withHome $ \home -> do
        run <- hw home ["createacc", "cash", "--network", "bch", "--type", "bip84"] seedTwice
        run `shouldSatisfy` printed "A bip84 account needs SegWit, which bch does not have"
        run `shouldNotSatisfy` askedMnemonic

    it "is refused a type other than bip44 and bip84" $
      withHome $ \home -> do
        run <- hw home ["createacc", "wrapped", "--type", "bip49"] seedTwice
        runExit run `shouldNotBe` ExitSuccess
        run `shouldSatisfy` printed "The type of an account is bip44 or bip84"

    it "hands out addresses that start with bc1q, which checkaddr derives too" $
      withHome $ \home -> do
        createSpending home
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(segwitExternalAddrsText !! 0, "")]
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(segwitExternalAddrsText !! 1, "")]
        (addressesOf <$> hwJson home ["checkaddr", "/0/5"] []) `shouldReturn` Right [(segwitExternalAddrsText !! 5, "")]
        (addressesOf <$> hwJson home ["checkaddr", "/1/0"] []) `shouldReturn` Right [(segwitInternalAddrsText !! 0, "")]

    it "is shown by descriptor as wpkh, as a Coldcard writes it, with its zpub for Electrum on a row of its own" $
      withHome $ \home -> do
        createSpending home
        (descriptorsOf <$> hwJson home ["descriptor"] []) `shouldReturn` Right [("spending", segwitDescriptor)]
        (slip132KeysOf <$> hwJson home ["descriptor"] []) `shouldReturn` Right [("spending", Just testSegwitAccountZPub)]
        run <- hw home ["descriptor"] []
        runOut run `shouldBe` segwitDescriptor <> "\n  Account    : spending\n  Network    : btc\n  SLIP-132   : " <> testSegwitAccountZPub <> "\n"

    -- As Electrum would be handed the account, and the key handed back to
    -- another hw: a key alone carries no fingerprint
    it "is imported from its zpub by another wallet as a bip84 account, named after its network, index and kind" $
      withHome $ \signer -> withHome $ \online -> do
        createSpending signer
        shown <- slip132KeysOf <$> answer signer ["descriptor"] []
        zpub <- case shown of
          Right [(_, Just key)] -> return key
          other -> fail ("Not one zpub: " <> show other)
        writeFile (online </> "spending.txt") (Text.unpack zpub <> "\n")
        (keyOf <$> hwJson online ["importacc", online </> "spending.txt"] []) `shouldReturn` Right ("btc0-segwit", 0, testSegwitAccountXPub)
        (fingerprintsOf <$> hwJson online ["accounts"] []) `shouldReturn` Right [("btc0-segwit", Nothing)]
        (addressesOf <$> hwJson online ["receive"] []) `shouldReturn` Right [(segwitExternalAddrsText !! 0, "")]

    it "is refused by importacc with --type bip44 for its zpub, which marks a bip84 account" $
      withHome $ \home -> do
        writeFile (home </> "spending.txt") (Text.unpack testSegwitAccountZPub <> "\n")
        run <- hw home ["importacc", home </> "spending.txt", "--type", "bip44"] []
        run `shouldSatisfy` printed "not imported: The key is a zpub, which marks a bip84 account in SLIP-132, and --type says bip44."
        (refusedOf <$> hwJson home ["importacc", home </> "spending.txt", "--type", "bip44"] [])
          `shouldReturn` Right [("zpub6r4mjW7y...", "The key is a zpub, which marks a bip84 account in SLIP-132, and --type says bip44. Leave --type out, or write the key as an xpub or a tpub if it is of a bip44 account after all.")]

    -- An xpub alone says nothing of its path. The note wraps at 80 columns,
    -- indented like the rest of the account.
    it "is noted by importacc as taken on trust from its xpub with --type bip84, with what tells if the type was wrong" $
      withHome $ \shown -> withHome $ \json -> do
        writeFile (shown </> "spending.txt") (Text.unpack testSegwitAccountXPub <> "\n")
        run <- hw shown ["importacc", shown </> "spending.txt", "--type", "bip84"] []
        runOut run
          `shouldBe` "btc0-segwit  btc  /84'/0'/0'\n  created\n  The key says no kind. The account is bip84 because --type says so. If the key\n  is of a bip44 account, a sync finds none of its transactions and signtx\n  refuses the mnemonic. Then deleteacc, and import the key again with --type\n  bip44.\n\nLabels set: none\n\nNext, name the account, which the file did not:\n  hw renameacc btc0-segwit NAME\n"
        writeFile (json </> "spending.txt") (Text.unpack testSegwitAccountXPub <> "\n")
        (createdNotesOf <$> hwJson json ["importacc", json </> "spending.txt", "--type", "bip84"] [])
          `shouldReturn` Right [("btc0-segwit", Just "The key says no kind. The account is bip84 because --type says so. If the key is of a bip44 account, a sync finds none of its transactions and signtx refuses the mnemonic. Then deleteacc, and import the key again with --type bip44.")]

    -- The bip84 key imported as a bip44 account, which the note above
    -- warns of
    it "is told apart by checkmnemonic when its xpub was imported as bip44: the mnemonic gives the key at the bip84 path" $
      withHome $ \home -> do
        writeFile (home </> "spending.txt") (Text.unpack testSegwitAccountXPub <> "\n")
        _ <- answer home ["importacc", home </> "spending.txt"] []
        (accResultOf <$> hwJson home ["checkmnemonic"] seedOnce)
          `shouldReturn` Right (False, "The mnemonic and passphrase did not match the account, but they give its key at /84'/0'/0' on btc: it is a bip84 account, not a bip44 one. Delete it with deleteacc and import it again with --type bip84.")

    -- The key of the bip49 account 0 of the seed, m/49'/0'/0', imported
    -- as a bip44 account
    it "is told by checkmnemonic to be of a kind hw cannot spend from, when the mnemonic gives its key at such a path" $
      withHome $ \home -> do
        writeFile (home </> "wrapped.txt") (Text.unpack testWrappedAccountXPub <> "\n")
        _ <- answer home ["importacc", home </> "wrapped.txt"] []
        (accResultOf <$> hwJson home ["checkmnemonic"] seedOnce)
          `shouldReturn` Right (False, "The mnemonic and passphrase did not match the account, but they give its key at /49'/0'/0' on btc: it is an account of the kind bip49, wrapped SegWit, which hw cannot spend from.")

    -- The bip84 account of the file of Bitcoin Core, of
    -- test/data/descriptors: the vpub of its tpub, computed apart from hw
    it "is shown with a vpub on a test network, where the keys are tpub" $
      withHome $ \home -> do
        file <- makeAbsolute "test/data/descriptors/core-default-wallet.json"
        _ <- answer home ["importacc", file, "--network", "btctest"] []
        (slip132KeysOf <$> hwJson home ["descriptor", "--account", "btctest0-segwit"] [])
          `shouldReturn` Right [("btctest0-segwit", Just "vpub5Z1pM9RQ283SnGWmTeYJB46HYNFZom6eT5JgfZ73Vg9gzYZogXGmjepwEePvVAJqdMadJ7uYRcuXVjdYVuRJtL14o4aam2NbDQ7QzzeUsc8")]

    it "is exported by exportacc and imported by importacc in another wallet, as a bip84 account with its fingerprint" $
      withHome $ \signer -> withHome $ \online -> do
        createSpending signer
        _ <- answer signer ["exportacc", signer </> "spending.jsonl"] []
        (keyOf <$> hwJson online ["importacc", signer </> "spending.jsonl"] []) `shouldReturn` Right ("spending", 0, testSegwitAccountXPub)
        (fingerprintsOf <$> hwJson online ["accounts"] []) `shouldReturn` Right [("spending", Just "bc560f95")]
        run <- hw online ["accounts"] []
        run `shouldSatisfy` printed "spending  btc  /84'/0'/0'"
        (addressesOf <$> hwJson online ["receive"] []) `shouldReturn` Right [(segwitExternalAddrsText !! 0, "")]

    -- The payment: 10 bytes of frame, a P2WPKH input of 41 bytes, a P2PKH
    -- output of 34 and a P2WPKH change output of 31, then (2 + 108) / 4
    -- of witness data, rounded up to 28: 144 virtual bytes, a fee of 1440
    -- at 10 satoshis a virtual byte, where the same payment of a bip44
    -- account is 226 bytes. Signed, the input has an empty script and the
    -- signature in its witness, and the transaction is 116 bytes outside
    -- the witness and 109 or 110 with it: 144 virtual bytes as well.
    it "is paid, pays its fee on virtual bytes, is signed with the signature in the witness, and sent" $
      withChain ctx (segwitPaidChain ctx) $ \home _ -> do
        createSpending home
        _ <- answer home ["receive"] []
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("spending", 0, 50000)]
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary, summarySize summary) `shouldBe` (1440, 10, 144)
        paidToWallet prepared `shouldBe` Map.fromList [(segwitInternalAddr 0, 28560)]
        nosig <- nosigOf prepared
        let file = home </> "signed.psbt"
        signed <- txOf =<< answer home ["signtx", nosig, "--output", file] seedOnce
        summarySize (txInfoSummary signed) `shouldBe` 144
        signedTx <- txSignDataTx . txFileData <$> readTxFile ctx file
        map (.script) signedTx.inputs `shouldBe` [BS.empty]
        map length signedTx.witness `shouldBe` [2]
        txid <- txidOf signed
        sent <- txOf =<< answer home ["sendtx", nosig, "--yes"] []
        txidOf sent `shouldReturn` txid
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("spending", 0, 28560)]

    -- The signature was checked apart from hw with the pure-Python verifier
    -- of the message-signing vectors: it recovers the key of the address,
    -- m/84'/0'/0'/0/0 of the test seed, under the header byte 39 of BIP 137
    it "signs a message from its address with the header byte of BIP 137, which verifymsg checks" $
      withHome $ \home -> do
        createSpending home
        _ <- answer home ["receive"] []
        signed <- hwJson home ["signmsg", Text.unpack (segwitExternalAddrsText !! 0), "hello"] seedOnce
        signed `shouldBe` ResponseSignMsg "hello" segwitHelloSignature (segwitExternalAddrsText !! 0)
        hwJson home ["verifymsg", Text.unpack (segwitExternalAddrsText !! 0), Text.unpack segwitHelloSignature, "hello"] []
          `shouldReturn` ResponseVerifyMsg "hello" segwitHelloSignature (segwitExternalAddrsText !! 0) True

    -- The chain pays the first receiving address of the bip44 account 0 and
    -- of the bip84 account 0 of the seed
    it "is recovered by recover beside the bip44 account of the seed, named btc0-segwit" $
      withChain ctx (Chain 800002 [(payment ctx, Just 800000), (fundingTx ctx 3 [(segwitExternalAddr 0, 30000)], Just 800000)]) $ \home _ -> do
        (accountsOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right [("btc0", 0, 50000), ("btc0-segwit", 0, 30000)]
        run <- hw home ["accounts"] []
        run `shouldSatisfy` printed "btc0  btc  /44'/0'/0'"
        run `shouldSatisfy` printed "btc0-segwit  btc  /84'/0'/0'"

-- | The signature of the message "hello" by the address /0/0 of the bip84
-- account of the test wallet, as signmsg gives it, with the header byte of
-- BIP 137: checked apart from hw with the pure-Python verifier of the
-- message-signing vectors, which recovers the key of the address from it
segwitHelloSignature :: Text
segwitHelloSignature = "KPTsP4VeuwtqOgTJeMywhK80W9RJdrpsKebwu2/58WfqejQZQh54meETAcgDFYNrOSFmv3aAQ+V4BN0XOydeLEI="

descriptorSpec :: Spec
descriptorSpec =
  describe "The descriptor of an account" $ do
    it "is shown by descriptor alone on a line, then the account and its network" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["descriptor"] []
        runOut run `shouldBe` mainDescriptor <> "\n  Account    : main\n  Network    : btc\n"

    it "has no row of SLIP-132 for a bip44 account, whose xpub Electrum takes as it is" $
      withHome $ \home -> do
        createMain home
        (slip132KeysOf <$> hwJson home ["descriptor"] []) `shouldReturn` Right [("main", Nothing)]

    -- second is account 1 of the seed, m/44'/0'/1'
    it "is shown for every account, or for the --account named" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        (descriptorsOf <$> hwJson home ["descriptor"] [])
          `shouldReturn` Right [("main", mainDescriptor), ("second", "pkh([bc560f95/44h/0h/1h]" <> testAccountXPub1 <> "/<0;1>/*)#nycrn993")]
        (descriptorsOf <$> hwJson home ["descriptor", "--account", "main"] []) `shouldReturn` Right [("main", mainDescriptor)]

    -- An account imported from its key alone has no fingerprint
    it "names no origin when the wallet does not know the fingerprint of the seed, and says so" $
      withHome $ \home -> do
        writeFile (home </> "key.txt") (Text.unpack testAccountXPub <> "\n")
        _ <- answer home ["importacc", home </> "key.txt"] []
        run <- hw home ["descriptor"] []
        runOut run `shouldBe` "pkh(" <> testAccountXPub <> "/<0;1>/*)#mmejv52s\n  Account    : btc0\n  Network    : btc\n  Fingerprint: unknown\n"

    it "is refused when the wallet has no account" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["descriptor"] []) `shouldReturn` Right "There are no accounts in the wallet"

    -- As a signer hands its account to another wallet, which hands it back
    it "is read by importacc in another wallet, which gets the key and the fingerprint, and names the account after its network and index" $
      withHome $ \signer -> withHome $ \online -> do
        createMain signer
        shown <- descriptorsOf <$> answer signer ["descriptor"] []
        descriptor <- case shown of
          Right [(_, descriptor)] -> return descriptor
          other -> fail ("Not one descriptor: " <> show other)
        writeFile (online </> "main.txt") (Text.unpack descriptor <> "\n")
        (keyOf <$> hwJson online ["importacc", online </> "main.txt"] []) `shouldReturn` Right ("btc0", 0, testAccountXPub)
        (fingerprintsOf <$> hwJson online ["accounts"] []) `shouldReturn` Right [("btc0", Just "bc560f95")]

    it "is imported with the next step of naming the account, which the file did not" $
      withHome $ \home -> do
        writeFile (home </> "main.txt") (Text.unpack mainDescriptor <> "\n")
        run <- hw home ["importacc", home </> "main.txt"] []
        run `shouldSatisfy` printed "Next, name the account, which the file did not:"
        run `shouldSatisfy` printed "hw renameacc btc0 NAME"

    it "needs the --network, as a descriptor names none" $
      withHome $ \home -> do
        setDefaultNetwork home Nothing
        writeFile (home </> "main.txt") (Text.unpack mainDescriptor <> "\n")
        run <- hw home ["importacc", home </> "main.txt"] []
        run `shouldSatisfy` printed "Give the --network, or a default-network in the configuration file"
        (keyOf <$> hwJson home ["importacc", home </> "main.txt", "--network", "btc"] []) `shouldReturn` Right ("btc0", 0, testAccountXPub)

    -- The file of Bitcoin Core's listdescriptors, of test/data/descriptors
    it "is read from the file of Bitcoin Core, whose BIP44 and BIP84 accounts are added" $
      withHome $ \home -> do
        file <- makeAbsolute "test/data/descriptors/core-default-wallet.json"
        _ <- answer home ["importacc", file, "--network", "btctest"] []
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btctest0", 0, 0), ("btctest0-segwit", 0, 0)]
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btctest0", Just "b079db60"), ("btctest0-segwit", Just "b079db60")]

{- Addresses -}

addressSpec :: Ctx -> Spec
addressSpec ctx =
  describe "The addresses of an account" $ do
    it "are handed out by receive in the order of the receiving chain" $
      withHome $ \home -> do
        createMain home
        forM_ (take 3 externalAddrsText) $ \expected ->
          (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(expected, "")]

    -- The answer read the account before the address was taken, and showed
    -- none counted
    it "are counted in the account receive shows" $
      withHome $ \home -> do
        createMain home
        (fmap dBAccountExternal . accountOf <$> hwJson home ["receive"] []) `shouldReturn` Right 1

    it "take the label given to receive" $
      withHome $ \home -> do
        createMain home
        (addressesOf <$> hwJson home ["receive", "--label", "rent"] []) `shouldReturn` Right [(externalAddrsText !! 0, "rent")]

    it "are listed by hw addresses newest first, and after a given one when asked" $
      withHome $ \home -> do
        createMain home
        replicateM_ 3 $ answer home ["receive"] []
        (addressesOf <$> hwJson home ["addresses"] [])
          `shouldReturn` Right [(externalAddrsText !! 2, ""), (externalAddrsText !! 1, ""), (externalAddrsText !! 0, "")]
        (addressesOf <$> hwJson home ["addresses", "--max", "1"] []) `shouldReturn` Right [(externalAddrsText !! 2, "")]
        (addressesOf <$> hwJson home ["addresses", "--max", "1", "--after", Text.unpack (externalAddrsText !! 2)] [])
          `shouldReturn` Right [(externalAddrsText !! 1, "")]

    it "are shown by hw addresses under their derivation paths" $
      withHome $ \home -> do
        createMain home
        replicateM_ 2 $ answer home ["receive"] []
        run <- hw home ["addresses"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/1  " <> externalAddrsText !! 1)
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/0  " <> externalAddrsText !! 0)

    it "are listed by hw addresses in two lines each, a blank line between them" $
      withHome $ \home -> do
        createMain home
        replicateM_ 2 $ answer home ["receive"] []
        run <- hw home ["addresses"] []
        map Text.null (Text.lines (runOut run)) `shouldBe` [False, False, True, False, False]

    -- The path /0/9 is padded to the width of /0/10
    it "are aligned by hw addresses when their paths differ in length" $
      withHome $ \home -> do
        createMain home
        replicateM_ 11 $ answer home ["receive"] []
        run <- hw home ["addresses", "--max", "2"] []
        runOut run `shouldSatisfy` Text.isInfixOf "/0/10  "
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/9   " <> externalAddrsText !! 9)

    -- The payment of 20000 pays its change to the first change address
    it "are shown by hw addresses --change under their paths on the change chain" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["addresses", "--change"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/1/0  " <> internalAddrsText !! 0)

    it "are shown by receive under their derivation paths, with every detail" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["receive"] []
        runOut run `shouldSatisfy` Text.isPrefixOf ("/0/0  " <> externalAddrsText !! 0 <> "\n  Transactions: 0\n")

    it "are named by label, given by their text" $
      withHome $ \home -> do
        createMain home
        replicateM_ 2 $ answer home ["receive"] []
        (addressesOf <$> hwJson home ["label", Text.unpack (externalAddrsText !! 1), "salary"] []) `shouldReturn` Right [(externalAddrsText !! 1, "salary")]
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 1, "salary"), (externalAddrsText !! 0, "")]

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is the first receiving address of
    -- second, account 1 of the seed
    it "are named by label in the account they belong to, with no --account" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "second"] []
        (addressesOf <$> hwJson home ["label", "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "rent"] []) `shouldReturn` Right [("17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "rent")]
        (addressesOf <$> hwJson home ["addresses", "--account", "main"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

    -- The sixth receiving address of main, which receive has not given out
    it "are not labelled when the wallet does not have them" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["label", Text.unpack (externalAddrsText !! 5), "salary"] []) `shouldReturn` Right "Address does not exist"

    -- The gap of BIP44: a wallet looking for payments stops after twenty
    -- unused addresses, so hw hands out no more until one is paid
    it "are refused past twenty that await a payment" $
      withHome $ \home -> do
        createMain home
        replicateM_ 20 $ answer home ["receive"] []
        (errorOf <$> hwJson home ["receive"] [])
          `shouldReturn` Right "The last 20 receiving addresses await a payment: reuse one of them, listed by hw addresses"

    it "have no change address before the account spends" $
      withHome $ \home -> do
        createMain home
        (addressesOf <$> hwJson home ["addresses", "--change"] []) `shouldReturn` Right []

{- Checking an address on the offline computer -}

checkAddrSpec :: Spec
checkAddrSpec =
  describe "An address checked with checkaddr" $ do
    -- The signer never ran receive: it derives the address from the key
    it "is the one receive gave out online, at the path receive showed, on the signer that never handed it out" $
      withHome $ \signer -> withHome $ \online -> do
        createMain signer
        _ <- answer signer ["exportacc", signer </> "main.json"] []
        _ <- answer online ["importacc", signer </> "main.json"] []
        replicateM_ 3 $ answer online ["receive"] []
        (addressesOf <$> hwJson online ["receive"] []) `shouldReturn` Right [(externalAddrsText !! 3, "")]
        (addressesOf <$> hwJson signer ["checkaddr", "/0/3"] []) `shouldReturn` Right [(externalAddrsText !! 3, "")]

    it "is a change address for a path on the change chain, before the account spends" $
      withHome $ \home -> do
        createMain home
        (addressesOf <$> hwJson home ["checkaddr", "/1/0"] []) `shouldReturn` Right [(internalAddrsText !! 0, "")]

    it "is shown under its path, then its account and network" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["checkaddr", "/0/3"] []
        runOut run `shouldBe` "/0/3  " <> externalAddrsText !! 3 <> "\n  Account  : main\n  Network  : btc\n"

    it "is shown with its label when the wallet has it" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive", "--label", "rent"] []
        (addressesOf <$> hwJson home ["checkaddr", "/0/0"] []) `shouldReturn` Right [(externalAddrsText !! 0, "rent")]

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is the first receiving address of
    -- second, account 1 of the seed
    it "is of the --account named, which a wallet of several accounts needs" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        (addressesOf <$> hwJson home ["checkaddr", "/0/0", "--account", "second"] []) `shouldReturn` Right [("17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "")]
        (errorOf <$> hwJson home ["checkaddr", "/0/0"] []) `shouldReturn` Right "Specify which account to use"

    -- hw stores the address /0/0 of cash, the Bitcoin Cash account of the
    -- test wallet, as bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u
    it "is written as the network of the account writes it" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        (addressesOf <$> hwJson home ["checkaddr", "/0/0"] []) `shouldReturn` Right [("bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", "")]

    it "hands out nothing: the wallet has no address after it, and receive starts at the first" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["checkaddr", "/0/5"] []
        _ <- answer home ["checkaddr", "/1/5"] []
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right []
        (addressesOf <$> hwJson home ["addresses", "--change"] []) `shouldReturn` Right []
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

    -- A third chain, a hardened step, one step, three steps, no leading
    -- slash, a master key, and the first hardened index
    it "is refused for a path that is not that of an address, with the two forms" $
      withHome $ \home -> do
        createMain home
        forM_ ["/2/0", "/0/3'", "/0", "/0/3/1", "0/3", "m/0/3", "/0/2147483648"] $ \path -> do
          run <- hw home ["checkaddr", path] []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf ("The path " <> Text.pack path <> " is not that of an address: /0/i is the receiving address i, and /1/i the change address i")

{- New mnemonics and backups in shares -}

splitSpec :: Spec
splitSpec = do
  describe "A new mnemonic" $ do
    it "has 12 words of the entropy of /dev/random by default, and 24 words for 32 bytes" $
      withHome $ \home -> do
        whole <- answer home ["mnemonic"] []
        whole `shouldSatisfy` \case ResponseMnemonic "/dev/random" ws -> length ws == 12; _ -> False
        long <- answer home ["mnemonic", "--entropy", "32"] []
        long `shouldSatisfy` \case ResponseMnemonic _ ws -> length ws == 24; _ -> False

    -- BIP39 adds a word of 11 bits for every 4 bytes of entropy
    it "has 15, 18 and 21 words for 20, 24 and 28 bytes of --entropy" $
      withHome $ \home ->
        forM_ [("20", 15), ("24", 18), ("28", 21)] $ \(bytes, count) -> do
          generated <- answer home ["mnemonic", "--entropy", bytes] []
          generated `shouldSatisfy` \case ResponseMnemonic _ ws -> length ws == count; _ -> False

    it "is refused an --entropy other than 16, 20, 24, 28 or 32 bytes" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--entropy", "17"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --entropy: cannot parse value `17'"

    -- Earlier versions split a mnemonic into parts that were all needed
    it "is split into parts no more: --parts is refused, and names --shares" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--parts", "2"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --parts: hw makes no mnemonic in parts any more: make a backup in shares with --shares and --threshold"

    -- The rolls are asked once the entropy of the system is read. None are
    -- typed here, and no mnemonic is made without them.
    it "asks for dice rolls with --dice, and is not made without them" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--dice"] []
        runErr run `shouldSatisfy` Text.isInfixOf "Rolls: "
        run `shouldSatisfy` printed "Cancelled: the input ended before the answer"

  describe "A mnemonic that an earlier version split into parts" $
    -- The two parts of the test mnemonic, typed twice, then its passphrase
    -- twice
    it "makes with createacc --parts the account of the mnemonic" $
      withHome $ \home ->
        (keyOf <$> hwJson home ["createacc", "main", "--parts", "2"] (testMnemonicParts <> testMnemonicParts <> replicate 2 (mnemonicPass testMnemonic)))
          `shouldReturn` Right ("main", 0, testAccountXPub)

  describe "A new backup in shares" $ do
    it "comes in the shares of --shares, of 20 words each, with the threshold of --threshold" $
      withHome $ \home -> do
        made <- answer home ["mnemonic", "--shares", "3", "--threshold", "2"] []
        made `shouldSatisfy` \case ResponseShares "/dev/random" 2 shares -> map length shares == [20, 20, 20]; _ -> False

    it "has shares of 33 words for 32 bytes of --entropy" $
      withHome $ \home -> do
        made <- answer home ["mnemonic", "--shares", "2", "--threshold", "2", "--entropy", "32"] []
        made `shouldSatisfy` \case ResponseShares _ 2 shares -> map length shares == [33, 33]; _ -> False

    it "is one share, the whole backup, with --shares 1, with or without --threshold 1" $
      withHome $ \home ->
        forM_ [["mnemonic", "--shares", "1"], ["mnemonic", "--shares", "1", "--threshold", "1"]] $ \args -> do
          made <- answer home args []
          made `shouldSatisfy` \case ResponseShares _ 1 [share] -> length share == 20; _ -> False

    -- A heading, then each share under its number, five lines of four
    -- words: 1 + 2 x 7 lines
    it "prints a heading, then each share under its number" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--shares", "2", "--threshold", "2"] []
        let shown = Text.lines (runOut run)
        take 3 shown `shouldBe` ["Backup in 2 shares of 20 words, any 2 of them rebuild the seed, from /dev/random", "", "Share 1"]
        length shown `shouldBe` 15

    -- Without a passphrase, which is asked once. Shares are typed once.
    it "makes with createacc the same account from any threshold of its shares" $
      withHome $ \home -> withHome $ \firstHome -> withHome $ \lastHome -> do
        made <- answer home ["mnemonic", "--shares", "3", "--threshold", "2"] []
        shares <- case made of
          ResponseShares _ _ shares@[_, _, _] -> return (Text.unwords <$> shares)
          other -> fail $ "Not a backup in three shares: " <> show other
        fromFirst <- keyOf <$> hwJson firstHome ["createacc", "main"] (take 2 shares <> [""])
        fromLast <- keyOf <$> hwJson lastHome ["createacc", "main"] (reverse (drop 1 shares) <> [""])
        fromLast `shouldBe` fromFirst
        (\(_, index, _) -> index) <$> fromFirst `shouldBe` Right 0

    it "needs --threshold for more than one share" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["mnemonic", "--shares", "3"] [])
          `shouldReturn` Right "With more than one share, give --threshold: the number of shares that rebuild the seed"

    it "is refused a threshold of 1 for more than one share" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["mnemonic", "--shares", "3", "--threshold", "1"] [])
          `shouldReturn` Right "With a threshold of 1 every share would be the whole backup: make one share, and copy it"

    it "is refused a threshold above the number of shares" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["mnemonic", "--shares", "3", "--threshold", "4"] [])
          `shouldReturn` Right "The threshold cannot be greater than the number of shares, 3"

    it "is refused --threshold without --shares" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["mnemonic", "--threshold", "2"] [])
          `shouldReturn` Right "--threshold goes with --shares: it is the number of shares that rebuild the seed"

    it "is refused 20, 24 or 28 bytes of --entropy, which a mnemonic alone has" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["mnemonic", "--shares", "1", "--entropy", "20"] [])
          `shouldReturn` Right "A backup in shares has 16 or 32 bytes of entropy, not 20"

    it "comes in 1 to 16 shares" $
      withHome $ \home ->
        forM_ ["0", "17"] $ \shares -> do
          run <- hw home ["mnemonic", "--shares", shares, "--threshold", "2"] []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf "The --shares value has to be between 1 and 16"

    -- 16 bytes for the seed and 12 for the random value that splits it:
    -- 224 bits, about 135 rolls of 5/3 bits
    it "asks with --dice for the rolls of the seed and of the random value that splits it" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--shares", "3", "--threshold", "2", "--dice"] []
        runErr run `shouldSatisfy` Text.isInfixOf "Roll a die about 135 times"
        run `shouldSatisfy` printed "Cancelled: the input ended before the answer"

{- Accounts of a backup in shares -}

sharesSpec :: Ctx -> Spec
sharesSpec ctx =
  describe "An account of a backup in shares" $ do
    it "is created from the shares typed once and the passphrase typed twice, with the key of the seed other wallets derive" $
      withHome $ \home ->
        (keyOf <$> hwJson home ["createacc", "vault", "--network", "btctest"] sharesForNew)
          `shouldReturn` Right ("vault", 0, testSharesTestnetXPub)

    it "hands out the address the device tests of a hardware wallet give for the backup" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "vault", "--network", "btctest"] sharesForNew
        receiveFor home "vault" `shouldReturn` Text.unpack (testSharesTestnetAddrs !! 0)

    it "is the same account on Bitcoin as its seed gives apart from hw" $
      withHome $ \home -> do
        (keyOf <$> hwJson home ["createacc", "vault"] sharesForNew) `shouldReturn` Right ("vault", 0, testSharesXPub)
        receiveFor home "vault" `shouldReturn` Text.unpack testSharesAddr

    it "records the fingerprint of its seed" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "vault"] sharesForNew
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("vault", Just testSharesFingerprintText)]

    -- The first share is a group of one, the three others are of a group
    -- that needs three: two groups, as the backup needs
    it "is created from the shares of a backup of several groups, and hands out the address the device tests give" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "vault", "--network", "btctest"] (testGroupShares <> ["TREZOR", "TREZOR"])
        receiveFor home "vault" `shouldReturn` Text.unpack testGroupSharesTestnetAddr

    -- Three attempts, each refused before the passphrase is typed again
    it "is not created with a passphrase that has an accent, which the format of the shares does not take" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["createacc", "vault"] (testShares <> replicate 3 "caf\233"))
          `shouldReturn` Right "The passphrase of a backup in shares has printable ASCII characters only: other wallets would not take this one"

    it "is checked by checkmnemonic against its shares and its passphrase" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "vault"] sharesForNew
        (accResultOf <$> hwJson home ["checkmnemonic"] sharesOnce)
          `shouldReturn` Right (True, "The shares and passphrase matched the account")
        (accResultOf <$> hwJson home ["checkmnemonic"] (testShares <> ["trezor"]))
          `shouldReturn` Right (False, "The shares and passphrase did not match the account")

    it "is not matched by the mnemonic of another seed" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "vault"] sharesForNew
        (accResultOf <$> hwJson home ["checkmnemonic"] seedOnce)
          `shouldReturn` Right (False, "The mnemonic and passphrase did not match the account")

    it "signs a message from its shares, which verifymsg checks against its address" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "vault"] sharesForNew
        address <- receiveFor home "vault"
        signed <- hwJson home ["signmsg", address, "hello"] sharesOnce
        signature <- case signed of
          ResponseSignMsg "hello" signature _ -> return signature
          other -> fail $ "Not a signed message: " <> show other
        verified <- hwJson home ["verifymsg", address, Text.unpack signature, "hello"] []
        verified `shouldBe` ResponseVerifyMsg "hello" signature testSharesAddr True

    it "is found by recover from its shares, when it was paid" $
      withChain ctx (chainPaying ctx sharesTestnetAddr) $ \home _ ->
        (accountsOf <$> hwJson home ["recover", "--network", "btctest"] sharesOnce)
          `shouldReturn` Right [("btctest0", 0, 50000)]

{- Signed messages -}

messageSpec :: Spec
messageSpec =
  describe "A message signed by signmsg" $ do
    it "is signed by the key of an address of the wallet, from its mnemonic" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hwJson home ["signmsg", textOf (externalAddr 0), "hello"] seedOnce
        signed `shouldBe` ResponseSignMsg "hello" helloSignature (externalAddrsText !! 0)

    -- A signature of the key is always the same
    it "is signed from the parts of the mnemonic with --parts" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hwJson home ["signmsg", textOf (externalAddr 0), "hello", "--parts", "2"] partsOnce
        signed `shouldBe` ResponseSignMsg "hello" helloSignature (externalAddrsText !! 0)

    -- cash is the Bitcoin Cash account of the test wallet. hw stores the
    -- address /0/0 of cash as
    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u; it is typed
    -- here without its prefix, and in the legacy form. The signature is
    -- checked apart from hw as above: the key it recovers is the one of the
    -- address in both forms, m/44'/145'/0'/0/0 of the test seed.
    it "is signed by the key of a Bitcoin Cash address however it is written" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        _ <- answer home ["receive"] []
        forM_ ["qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ"] $ \written ->
          hwJson home ["signmsg", written, "hello"] seedOnce
            `shouldReturn` ResponseSignMsg
              "hello"
              "H98t/71A7/ED0lRtdsnAiVwcgUv+5sZr7WJwpHS3YFcCcTKtSG/rx2ooIXOTMbdhApTDjcfGjTu3Sjgh6Pl5qQc="
              (Text.pack written)

    it "is refused after three seeds that are not the account's" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        let otherSeed = [mnemonicWords testMnemonic2, mnemonicPass testMnemonic2]
        (errorOf <$> hwJson home ["signmsg", textOf (externalAddr 0), "hello"] (concat (replicate 3 otherSeed)))
          `shouldReturn` Right "The mnemonic or the passphrase does not match the account"

    it "is refused for an address the wallet does not have, before the mnemonic is asked" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["signmsg", textOf (otherAddr 0), "hello"] seedOnce
        run `shouldSatisfy` printed "Address does not exist"
        run `shouldNotSatisfy` askedMnemonic

    -- The block signmsg prints, line for line
    it "is written to the --output file as its armored block, and hw says where" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        let file = home </> "signed.txt"
        run <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--output", file] seedOnce
        runOut run `shouldBe` "Written to " <> Text.pack file <> "\n"
        (Text.pack <$> readFile file)
          `shouldReturn` Text.unlines
            [ "-----BEGIN BITCOIN SIGNED MESSAGE-----",
              "hello",
              "-----BEGIN BITCOIN SIGNATURE-----",
              externalAddrsText !! 0,
              helloSignature,
              "-----END BITCOIN SIGNATURE-----"
            ]

    it "is answered in --json by the --output file it was written to" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        let file = home </> "signed.txt"
        hwJson home ["signmsg", textOf (externalAddr 0), "hello", "--output", file] seedOnce
          `shouldReturn` ResponseFile file

    it "is refused for an --output file that exists, before the mnemonic is asked, and the file is kept" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        let file = home </> "signed.txt"
        writeFile file "kept\n"
        run <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--output", file] seedOnce
        run `shouldSatisfy` printed (alreadyExists file)
        run `shouldNotSatisfy` askedMnemonic
        readFile file `shouldReturn` "kept\n"

{- Verified messages -}

verifySpec :: Spec
verifySpec =
  describe "A signature checked by verifymsg" $ do
    it "is valid for the message as signed, without the wallet, which hw says in one line" $
      withHome $ \home -> do
        hwJson home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello"] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True
        run <- hw home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello"] []
        runOut run `shouldBe` "The key of " <> externalAddrsText !! 0 <> " signed this message\n"

    it "is invalid for a changed message, and hw says so and exits with a failure" $
      withHome $ \home -> do
        run <- hw home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello!"] []
        runExit run `shouldBe` ExitFailure 1
        runOut run `shouldBe` "This is not a signature of this message by the key of " <> externalAddrsText !! 0 <> "\n"
        hwJson home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello!"] []
          `shouldReturn` ResponseVerifyMsg "hello!" helloSignature (externalAddrsText !! 0) False

    it "warns about the whitespace of a message found invalid" $
      withHome $ \home -> do
        run <- hw home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello "] []
        run `shouldSatisfy` printed "Warning: The message has leading or trailing whitespace"

    -- The signature of Bitcoin Core's rpc_signmessagewithprivkey test, by a
    -- regtest key: its P2PKH address reads on testnet
    it "reads the address on the network given, the default one otherwise" $
      withHome $ \home -> do
        let signature = "INbVnW4e6PeRmsv2Qgu8NuopvrVjkcxob+sX8OcZG0SALhWybUjzMLPdAsXI46YZGb0KQTRii+wWIQzRpG/U+S0="
            address = "mpLQjfK79b7CCV4VMJWEWAj5Mpx8Up5zxB"
            message = "This is just a test message"
        hwJson home ["verifymsg", address, signature, message, "--network", "btctest"] []
          `shouldReturn` ResponseVerifyMsg (Text.pack message) (Text.pack signature) (Text.pack address) True
        (errorOf <$> hwJson home ["verifymsg", address, signature, message] [])
          `shouldReturn` Right "The address mpLQjfK79b7CCV4VMJWEWAj5Mpx8Up5zxB is not valid on btc"

    it "reads the block signmsg printed, saved in a file" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hw home ["signmsg", textOf (externalAddr 0), "hello"] seedOnce
        Text.lines (runOut signed)
          `shouldBe` [ "-----BEGIN BITCOIN SIGNED MESSAGE-----",
                       "hello",
                       "-----BEGIN BITCOIN SIGNATURE-----",
                       externalAddrsText !! 0,
                       helloSignature,
                       "-----END BITCOIN SIGNATURE-----"
                     ]
        let file = home </> "signed.txt"
        writeFile file (Text.unpack (runOut signed))
        hwJson home ["verifymsg", "--input", file] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "reads the file signmsg --output wrote" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        let file = home </> "signed.txt"
        _ <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--output", file] seedOnce
        hwJson home ["verifymsg", "--input", file] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "refuses a file that holds no block" $
      withHome $ \home -> do
        let file = home </> "plain.txt"
        writeFile file "hello\n"
        (errorOf <$> hwJson home ["verifymsg", "--input", file] [])
          `shouldReturn` Right (Text.pack $ "The file " <> file <> " could not be read: no \"-----BEGIN BITCOIN SIGNED MESSAGE-----\" line")

    it "reads a message of several lines, with its whitespace, from the block" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        let message = "two lines, \nand a trailing space "
        signed <- hw home ["signmsg", textOf (externalAddr 0), message] seedOnce
        let file = home </> "signed.txt"
        writeFile file (Text.unpack (runOut signed))
        checked <- hwJson home ["verifymsg", "--input", file] []
        case checked of
          ResponseVerifyMsg m _ a valid -> (m, a, valid) `shouldBe` (Text.pack message, externalAddrsText !! 0, True)
          other -> expectationFailure $ "Not a verifymsg answer: " <> show other

    it "reads the --json answer of signmsg, saved in a file" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--json"] seedOnce
        let file = home </> "signed.psbt"
        writeFile file (Text.unpack (runOut signed))
        hwJson home ["verifymsg", "--input", file] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "refuses a file holding another answer of hw" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--json"] []
        let file = home </> "mnemonic.json"
        writeFile file (Text.unpack (runOut run))
        (errorOf <$> hwJson home ["verifymsg", "--input", file] [])
          `shouldReturn` Right (Text.pack $ "The file " <> file <> " holds an answer of hw, but not one of signmsg")

    -- The refusals of Bitcoin Core's message_verify test, with a Taproot
    -- address in place of its P2SH one, which hw takes for one of a key
    it "refuses an address that is not of one key" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["verifymsg", "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0", "signature should be irrelevant", "message too"] [])
          `shouldReturn` Right "The address is not a P2PKH or a SegWit address of one key: a message can only be signed from one"

    it "refuses a signature that is not in base64" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["verifymsg", "1KqbBpLy5FARmTPD4VZnDDpYjkUvkr82Pm", "invalid signature, not in base64 encoding", "message should be irrelevant"] [])
          `shouldReturn` Right "The signature is not in base64"

{- Backups and old databases -}

backupSpec :: Spec
backupSpec =
  describe "The database" $ do
    it "is copied by backup to a file, a database with the accounts of the wallet" $
      withHome $ \home -> withHome $ \restored -> do
        createMain home
        (shownFile <$> hwJson home ["backup", home </> "copy.sqlite"] []) `shouldReturn` Right (home </> "copy.sqlite")
        copyFile (home </> "copy.sqlite") (restored </> ".hw" </> "accounts.sqlite")
        (accountsOf <$> hwJson restored ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    it "is not copied over a file that exists" $
      withHome $ \home -> do
        createMain home
        writeFile (home </> "copy.sqlite") ""
        (errorOf <$> hwJson home ["backup", home </> "copy.sqlite"] [])
          `shouldReturn` Right (alreadyExists (home </> "copy.sqlite"))

    -- hw crashed with the exception of SQLite before
    it "is not copied into a directory that does not exist, and hw says so" $
      withHome $ \home -> do
        createMain home
        let copy = home </> "missing" </> "copy.sqlite"
        (errorOf <$> hwJson home ["backup", copy] [])
          `shouldReturn` Right ("The database could not be copied to " <> Text.pack copy <> ": no such file or directory")

    it "is copied by backup into the backups of hw without a file" $
      withHome $ \home -> do
        createMain home
        copy <- shownFile <$> hwJson home ["backup"] []
        copy `shouldSatisfy` either (const False) ((home </> ".hw" </> "backups") `isInfixOf`)

    it "is migrated from version 3 by the first command, backed up first" $
      withHome $ \home -> do
        version3Main (home </> ".hw" </> "accounts.sqlite")
        run <- hw home ["accounts"] []
        runExit run `shouldBe` ExitSuccess
        runErr run `shouldSatisfy` Text.isInfixOf "Migrating the database to version 4: the accounts identified by their keys, without wallets"
        runErr run `shouldSatisfy` Text.isInfixOf "The database at version 3 was backed up to"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]
        runDBWith 100 (home </> ".hw" </> "accounts.sqlite") (runExceptT databaseVersion) `shouldReturn` Right (Just 4)
  where
    shownFile (ResponseFile file) = Right file
    shownFile other = Left (show other)

{- Recovering a seed -}

recoverSpec :: Ctx -> Spec
recoverSpec ctx =
  describe "The accounts of a seed recovered by recover" $ do
    it "are those paid, named after their network and index, and synced" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (accountsOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right [("btc0", 0, 50000)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", 0, 50000)]

    it "are found from the parts of the mnemonic with --parts" $
      withChain ctx (paidChain ctx) $ \home _ ->
        (accountsOf <$> hwJson home ["recover", "--parts", "2"] partsOnce) `shouldReturn` Right [("btc0", 0, 50000)]

    -- The chain pays account 1 only. Account 0 is one without a payment.
    it "are looked for until --account-gap accounts in a row without a payment" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        (accountsOf <$> hwJson home ["recover", "--account-gap", "1"] seedOnce) `shouldReturn` Right []
        (accountsOf <$> hwJson home ["recover", "--account-gap", "2"] seedOnce) `shouldReturn` Right [("btc1", 1, 50000)]

    -- The configuration file of the test home names btc. The chain pays the
    -- first receiving address of account 0 of the seed on Bitcoin Cash.
    it "are looked for on the --network given" $
      withChain ctx (chainPaying ctx bchExternalAddr) $ \home _ ->
        (accountsOf <$> hwJson home ["recover", "--network", "bch"] seedOnce) `shouldReturn` Right [("bch0", 0, 50000)]

    -- The chain pays the first receiving address of account 0 of the seed,
    -- and the first receiving address of its bip49 account 0 and of its
    -- bip86 account 0, which hw cannot spend from
    it "come with the funds at the bip49 and bip86 paths of the seed, which hw cannot spend and adds no account for" $
      withChain ctx (paidElsewhereChain ctx) $ \home _ -> do
        found <- hwJson home ["recover"] seedOnce
        accountsOf found `shouldBe` Right [("btc0", 0, 50000)]
        unspendableOf found `shouldBe` Right [("bip49", "/49'/0'/0'", 1, 30000), ("bip86", "/86'/0'/0'", 1, 7000)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", 0, 50000)]

    it "are shown before those funds, each of which with its kind, its used addresses and what is left, then what to do" $
      withChain ctx (paidElsewhereChain ctx) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        run `shouldSatisfy` printed "btc0  btc  /44'/0'/0'\n"
        run `shouldSatisfy` printed "\n\nThe seed was paid at paths hw cannot spend from:\n  /49'/0'/0'  bip49, wrapped SegWit: 1 address used, 0.00030000 btc left\n  /86'/0'/0'  bip86, Taproot: 1 address used, 0.00007000 btc left\n  A wallet that holds such accounts can move the funds to an address of this wallet.\n"

    it "are said to be none when none was paid, before the funds at the paths hw cannot spend from" $
      withChain ctx (chainPaying ctx (wrappedExternalAddr 0)) $ \home _ -> do
        run <- hw home ["recover"] seedOnce
        runOut run `shouldBe` "No account of the seed was paid on btc: the search ended after 10 accounts in a row without a payment\n\nThe seed was paid at paths hw cannot spend from:\n  /49'/0'/0'  bip49, wrapped SegWit: 1 address used, 0.00050000 btc left\n  A wallet that holds such accounts can move the funds to an address of this wallet.\n"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

    -- The chain pays the first receiving address of account 0 of the seed
    -- on Bitcoin, m/44'/0'/0', whose keys hold Bitcoin Cash as well. hw
    -- holds the accounts of Bitcoin Cash at m/44'/145' only.
    it "come with the Bitcoin Cash at the bip44 accounts of Bitcoin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        found <- hwJson home ["recover", "--network", "bch"] seedOnce
        accountsOf found `shouldBe` Right []
        unspendableOf found `shouldBe` Right [("bip44", "/44'/0'/0'", 1, 50000)]

    it "leave out an account the wallet has" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        (accountsOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right []
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    it "are not looked for without a --network or a default network in the configuration file, before the mnemonic" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        setDefaultNetwork home Nothing
        run <- hw home ["recover"] seedOnce
        run `shouldSatisfy` printed "Give the --network, or a default-network in the configuration file"
        run `shouldNotSatisfy` askedMnemonic

    -- The chain pays the account hw derived before it normalized the
    -- passphrase, where BIP39 gives another seed
    it "are looked for from the passphrase as typed when the normalized one has none, which recover says" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \home _ -> do
        run <- hw home ["recover"] cafeOnce
        runErr run `shouldSatisfy` Text.isInfixOf "The accounts found come from the passphrase as typed"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", 0, 50000)]
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", Just cafeTypedFingerprint)]

    it "are those of the normalized passphrase, as BIP39 derives them, without a word about the other" $
      withChain ctx (chainPaying ctx cafeNormalizedAddr) $ \home _ -> do
        run <- hw home ["recover"] cafeOnce
        runErr run `shouldNotSatisfy` Text.isInfixOf "as typed"
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", Just cafeNormalizedFingerprint)]

    -- The chain pays 50000 to the first receiving address of each of the
    -- accounts 0 to 20 of the seed: 21 accounts, one more than recover
    -- finds before it asks. A store that lies reports them without end.
    it "are asked about once the store reports 20 of them paid, and none is added when the answer is no" $ do
      firstAddrs <- firstAddrsOfAccounts ctx 21
      withChain ctx (Chain 800002 [(fundingTx ctx 1 [(addr, 50000) | addr <- firstAddrs], Just 800000)]) $ \home _ -> do
        run <- hw home ["recover"] (seedOnce <> ["n"])
        run `shouldSatisfy` printed "The store reports payments to 20 accounts of the seed, as far as the account /44'/0'/19'. Keep looking? [y/N] "
        run `shouldSatisfy` printed "The search stopped there."
        runExit run `shouldBe` ExitFailure 1
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

    it "are all added when the answer is yes, those found before the question and the one after" $ do
      firstAddrs <- firstAddrsOfAccounts ctx 21
      withChain ctx (Chain 800002 [(fundingTx ctx 1 [(addr, 50000) | addr <- firstAddrs], Just 800000)]) $ \home _ -> do
        found <- hwJson home ["recover"] (seedOnce <> ["y"])
        (map (\(name, _, _) -> name) <$> accountsOf found)
          `shouldBe` Right
            [ "btc0", "btc1", "btc2", "btc3", "btc4", "btc5", "btc6",
              "btc7", "btc8", "btc9", "btc10", "btc11", "btc12", "btc13",
              "btc14", "btc15", "btc16", "btc17", "btc18", "btc19", "btc20"
            ]

    -- Every account searched is a scan at the store
    it "are looked for with an account gap from 1 to 100" $
      withHome $ \home -> do
        forM_ ["0", "101"] $ \gap -> do
          run <- hw home ["recover", "--account-gap", gap] seedOnce
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf "The account gap is a number of accounts, from 1 to 100"
        -- 100 is taken: recover goes on to the store, out of reach here
        run <- hw home ["recover", "--account-gap", "100"] seedOnce
        run `shouldSatisfy` printed "The store at http://127.0.0.1:1/ could not be reached: the connection failed"

    -- The store of the test home refuses every connection, as on an offline
    -- computer, where the mnemonic would be typed for nothing
    it "are not looked for when the store is out of reach, which recover asks before the mnemonic" $
      withHome $ \home -> do
        run <- hw home ["recover"] seedOnce
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The store at http://127.0.0.1:1/ could not be reached: the connection failed"
        run `shouldNotSatisfy` askedMnemonic

{- The passphrase as typed -}

-- | The words of the test wallet with the passphrase "café", typed once, or
-- twice each as createacc asks
cafeOnce, cafeTwice :: [Text]
cafeOnce = [mnemonicWords testMnemonic, cafePassphrase]
cafeTwice = [mnemonicWords testMnemonic, mnemonicWords testMnemonic, cafePassphrase, cafePassphrase]

-- | The paid chain, with a payment to the first receiving address of the
-- bip49 account 0 of the seed and of its bip86 account 0 in the same block
paidElsewhereChain :: Ctx -> Chain
paidElsewhereChain ctx = Chain 800002 [(payment ctx, Just 800000), (fundingTx ctx 3 [(wrappedExternalAddr 0, 30000), (taprootExternalAddr 0, 7000)], Just 800000)]

passphraseSpec :: Ctx -> Spec
passphraseSpec ctx =
  describe "An account created from a passphrase with accents" $ do
    it "is of the seed BIP39 derives, when the wallet has no account of the passphrase as typed" $
      withHome $ \home -> do
        run <- hw home ["createacc", "main"] cafeTwice
        runErr run `shouldNotSatisfy` Text.isInfixOf "as typed"
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", Just cafeNormalizedFingerprint)]

    -- btc0 was recovered from the passphrase as typed
    it "is of the seed of the passphrase as typed when the wallet has an account of it and none of the other, which createacc says" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \home _ -> do
        _ <- answer home ["recover"] cafeOnce
        run <- hw home ["createacc", "second"] cafeTwice
        runErr run `shouldSatisfy` Text.isInfixOf "to keep them together"
        (fmap sort . accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", 0, 50000), ("second", 1, 0)]
        (fmap sort . fingerprintsOf <$> hwJson home ["accounts"] [])
          `shouldReturn` Right [("btc0", Just cafeTypedFingerprint), ("second", Just cafeTypedFingerprint)]

    -- main is of the seed of BIP39, btc0 of the passphrase as typed
    it "is of the seed BIP39 derives when the wallet has accounts of both seeds" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \home _ -> do
        _ <- answer home ["createacc", "main"] cafeTwice
        _ <- answer home ["recover"] cafeOnce
        run <- hw home ["createacc", "third"] cafeTwice
        runErr run `shouldNotSatisfy` Text.isInfixOf "as typed"
        (fmap sort . fingerprintsOf <$> hwJson home ["accounts"] [])
          `shouldReturn` Right [("btc0", Just cafeTypedFingerprint), ("main", Just cafeNormalizedFingerprint), ("third", Just cafeNormalizedFingerprint)]
