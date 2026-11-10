{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The accounts and labels of the wallet as the records of a BIP 329 file:
-- what exportacc writes for an account, what importacc makes of each
-- kind of record, and of the descriptors and the files of other wallets,
-- in a database of its own
module Haskoin.Wallet.LabelsSpec (spec) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Default (def)
import qualified Data.Serialize as S
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Bip329
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address (genExtAddress, updateAddressBalances)
import Haskoin.Wallet.Database.BestBlock (updateBest)
import Haskoin.Wallet.Database.TxInfo (repsertSummary)
import Haskoin.Wallet.Database.TxLabel (setTxLabel, txLabel)
import Haskoin.Wallet.Labels
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  exportSpec ctx
  importKeySpec ctx
  importDescriptorSpec ctx
  importFileSpec ctx
  importAddressSpec ctx
  importTxSpec ctx
  importCoinSpec ctx
  importOtherSpec ctx
  wordLineSpec ctx
  showFileSpec ctx

{- Fixtures -}

-- | The id of the payment of the shared story, as the records write it:
-- fakeTxHash 1, whose bytes read backwards
paymentTxid :: Text
paymentTxid = "0000000000000000000000000000000000000000000000000000000000000001"

-- | The origin of the account main of the test seed
mainOrigin :: Text
mainOrigin = "pkh([bc560f95/44'/0'/0'])"

-- | The key of main as a record, with its name, origin and network
mainKeyLine :: Text
mainKeyLine = "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"" <> mainOrigin <> "\",\"network\":\"btc\"}\n"

-- | The key of account 1 of the test seed as a record, named second, with
-- its origin and network
secondKeyLine :: Text
secondKeyLine = "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub1 <> "\",\"label\":\"second\",\"origin\":\"pkh([bc560f95/44'/0'/1'])\",\"network\":\"btc\"}\n"

-- | The descriptor of main, as hw and a Coldcard write it
mainDescriptor :: Text
mainDescriptor = "pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j87"

-- | The key of main, as a refused line names it: its first twelve characters
mainKeyStart :: Text
mainKeyStart = "xpub6CHK45s4..."

-- | The payment of 50000 stored in the account, in block 800000, and the
-- best block at the height given
paymentStored :: DBAccountId -> BlockHeight -> DB IO ()
paymentStored accId best = do
  _ <- repsertSummary btc accId (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
  updateBest btc (fakeBlockHash 2) best 1700000000

-- | The coin of the payment stored in the account, at the first receiving
-- address, locked by the user or not
coinStored :: DBAccountId -> Bool -> DB IO ()
coinStored accId locked =
  P.insert_ $ DBCoin accId (outpointBytes (OutPoint (fakeTxHash 1) 0)) (externalAddrsText !! 0) 50000 (S.encode (Store.BlockRef 800000 1)) locked fixtureTime

-- | The account main of the test seed, with its first receiving address
mainWithAddress :: Ctx -> ExceptT String (DB IO) (DBAccountId, DBAccount)
mainWithAddress ctx = do
  held@(accId, _) <- testNewAcc ctx "main"
  _ <- genExtAddress ctx def accId ""
  return held

-- | The accounts of the wallet by name, network and fingerprint
accountsStored :: DB IO [(Text, Text, Maybe Text)]
accountsStored = map (\(_, a) -> (dBAccountName a, dBAccountNetwork a, dBAccountFingerprint a)) <$> getAccounts

-- | The kind and the path of an account the import created, if it did
createdKind :: AccountImport -> Maybe (AccountKind, String)
createdKind (AccountCreated acc _) = Just (accountKind acc, pathToStr (accountPath acc))
createdKind _ = Nothing

-- | The note of an account the import created, if it did: what it took on
-- trust, when it took anything
createdNote :: AccountImport -> Maybe (Maybe Text)
createdNote (AccountCreated _ note) = Just note
createdNote _ = Nothing

-- | The labels of the receiving addresses of an account, by index
addressLabels :: DBAccountId -> DB IO [(Int, Text)]
addressLabels accId = do
  rows <- P.selectList [DBAddressAccount P.==. accId, DBAddressInternal P.==. False] [P.Asc DBAddressIndex]
  return [(dBAddressIndex a, dBAddressLabel a) | P.Entity _ a <- rows]

-- | A report of an import that changed nothing
nothingDone :: ImportReport
nothingDone = ImportReport [] 0 0 0 0 0 0 0 0 0 0 0 []

{- Exporting -}

exportSpec :: Ctx -> Spec
exportSpec ctx =
  describe "The records of an account" $ do
    -- The payment has 6 confirmations at best block 800005
    it "are its key with its name, origin, network and purpose, then its labelled addresses, transactions and locked coins" $
      inDatabaseE $ do
        held@(accId, _) <- testNewAcc ctx "main"
        _ <- genExtAddress ctx def accId "Salary"
        lift $ setTxLabel accId (fakeTxHash 1) "First salary"
        lift $ paymentStored accId 800005
        lift $ coinStored accId True
        records <- accountRecords ctx held
        liftIO $
          map recordLine records
            `shouldBe` [ "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"purpose\":44}",
                         "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"keypath\":\"/0/0\"}",
                         "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"First salary\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"height\":800000,\"fee\":1000,\"value\":50000}",
                         "{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"spendable\":false,\"keypath\":\"/0/0\",\"height\":800000,\"value\":50000}"
                       ]

    -- 3 confirmations at best block 800002: the height may still change
    it "give a transaction and a coin no height before six blocks confirm them" $
      inDatabaseE $ do
        held@(accId, _) <- mainWithAddress ctx
        lift $ setTxLabel accId (fakeTxHash 1) "First salary"
        lift $ paymentStored accId 800002
        lift $ coinStored accId True
        records <- accountRecords ctx held
        liftIO $ map recordHeight (drop 1 records) `shouldBe` [Nothing, Nothing]

    it "give a labelled transaction the chain does not have yet neither height, fee nor value" $
      inDatabaseE $ do
        held@(accId, _) <- testNewAcc ctx "main"
        lift $ setTxLabel accId (fakeTxHash 1) "First salary"
        records <- accountRecords ctx held
        liftIO $
          drop 1 records
            `shouldBe` [(record TxRecord paymentTxid) {recordLabel = Just "First salary", recordOrigin = Just mainOrigin}]

    it "have no origin for an account whose seed hw does not know" $
      inDatabaseE $ do
        held@(accId, _) <- insertAccount Bip44 btc ctx "main" (testAccountPub ctx) Nothing
        _ <- genExtAddress ctx def accId "Salary"
        records <- accountRecords ctx held
        liftIO $ map recordOrigin records `shouldBe` [Nothing, Nothing]

    it "leave out an address without a label and a coin that is free" $
      inDatabaseE $ do
        held@(accId, _) <- mainWithAddress ctx
        lift $ coinStored accId False
        records <- accountRecords ctx held
        liftIO $ map recordType records `shouldBe` [XpubRecord]

{- Importing keys -}

importKeySpec :: Ctx -> Spec
importKeySpec ctx =
  describe "An xpub record" $ do
    it "creates the account, named after its label, on its network, with the fingerprint of its origin" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing mainKeyLine
        accounts <- lift accountsStored
        liftIO $ accounts `shouldBe` [("main", "btc", Just "bc560f95")]
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("main", testAccountXPub)]

    it "names an account without a label after its network and index, as recover does" $
      inDatabaseE $ do
        _ <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"network\":\"btc\"}\n")
        lift accountsStored `dbShouldBeE` [("btc0", "btc", Nothing)]

    -- A file of another wallet names no network
    it "takes the network of the command for a key that names none, and needs one" $ do
      inDatabaseE $ do
        _ <- importFile ctx (Just bch) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPubBch <> "\",\"label\":\"cash\"}\n")
        lift accountsStored `dbShouldBeE` [("cash", "bch", Nothing)]
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "Give the --network, or a default-network in the configuration file"]

    it "is refused for a key that does not read on the network" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPubTest <> "\",\"label\":\"test\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "test" "The key does not read as a key of btc"]

    it "is refused for an unknown network" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"network\":\"nowhere\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "Unknown network nowhere"]

    -- The master key of the vectors of the BIP, at depth 0
    it "is refused for a key that is not three hardened steps below the master key" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing "{\"type\":\"xpub\",\"ref\":\"xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8\",\"label\":\"Extended Public Key\"}\n"
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "Extended Public Key" "The key is not that of an account, three hardened steps below the master key"]

    -- The key of the test account at /0', four hardened steps deep
    it "is refused for a key a hardened step further than an account" $
      inDatabaseE $ do
        let deeper = xPubExport btc ctx (deriveXPubKey ctx (hardSubKey ctx testAccountKey 0))
        report <- importFile ctx (Just btc) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> deeper <> "\",\"label\":\"deeper\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "deeper" "The key is not that of an account, three hardened steps below the master key"]

    -- The key of the master key of the shares of the tests at
    -- m/44'/0'/0, three steps deep, the last one not hardened
    it "is refused for a key three steps deep whose last step is not hardened" $
      inDatabaseE $ do
        let master = fromMaybe (error "master") (xPrvImport btc testSharesMasterKey)
            soft = xPubExport btc ctx (deriveXPubKey ctx (derivePath ctx (Deriv :| 44 :| 0 :/ 0 :: DerivPath) master))
        report <- importFile ctx (Just btc) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> soft <> "\",\"label\":\"soft\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "soft" "The key is not that of an account, three hardened steps below the master key"]

    -- The path is that of the account; the script functions, those of a
    -- BIP49 wallet, are not
    it "is refused when the script functions of its origin are not those of the account, at its very path" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"sh(wpkh([bc560f95/44'/0'/0']))\",\"network\":\"btc\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The key is not that of the bip44 account /44'/0'/0': its origin is sh(wpkh([bc560f95/44'/0'/0']))"]

    it "is refused when its origin is not that of the account at its path, as the pkh origin of a key at a BIP84 path" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"pkh([bc560f95/84'/0'/0'])\",\"network\":\"btc\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The key is not that of the bip44 account /44'/0'/0': its origin is pkh([bc560f95/84'/0'/0'])"]

    it "is refused when its origin is of a kind of account hw does not hold, as sh(wpkh) of a BIP49 wallet" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"sh(wpkh([bc560f95/49'/0'/0']))\",\"network\":\"btc\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The key is not that of the bip44 account /44'/0'/0': its origin is sh(wpkh([bc560f95/49'/0'/0']))"]

    it "adds a bip84 account when its origin is wpkh, named with -segwit after its network and index" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testSegwitAccountXPub <> "\",\"origin\":\"wpkh([bc560f95/84'/0'/0'])\",\"network\":\"btc\"}\n")
        liftIO $ map createdKind (reportAccounts report) `shouldBe` [Just (Bip84, "/84'/0'/0'")]
        lift accountsStored `dbShouldBeE` [("btc0-segwit", "btc", Just "bc560f95")]

    it "adds a bip84 account when its purpose is 84, as hw writes it for such an account without a known seed" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testSegwitAccountXPub <> "\",\"label\":\"spending\",\"network\":\"btc\",\"purpose\":84}\n")
        liftIO $ map createdKind (reportAccounts report) `shouldBe` [Just (Bip84, "/84'/0'/0'")]
        lift accountsStored `dbShouldBeE` [("spending", "btc", Nothing)]

    it "is refused when its origin and its purpose name two kinds of account" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"purpose\":84}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The record says its key is of accounts of two kinds, bip44 and bip84: its origin, its purpose and the prefix of its key do not agree"]

    it "is refused when its purpose is of no kind hw holds" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"network\":\"btc\",\"purpose\":49}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The purpose 49 is of no kind of account hw holds: bip44 or bip84"]

    it "adds a bip44 account from a bare key, and a bip84 one with --type bip84" $
      inDatabaseE $ do
        legacy <- importFile ctx (Just btc) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\"}\n")
        liftIO $ map createdKind (reportAccounts legacy) `shouldBe` [Just (Bip44, "/44'/0'/0'")]
        segwit <- importFile ctx (Just btc) (Just Bip84) ("{\"type\":\"xpub\",\"ref\":\"" <> testSegwitAccountXPub <> "\"}\n")
        liftIO $ map createdKind (reportAccounts segwit) `shouldBe` [Just (Bip84, "/84'/0'/0'")]
        lift accountsStored `dbShouldBeE` [("btc0", "btc", Nothing), ("btc0-segwit", "btc", Nothing)]

    -- A bare key says nothing of its path, so its kind is the user's word
    -- or the default, and the answer says what tells when it was wrong
    it "notes that the kind of a bare key was taken by default" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\"}\n")
        liftIO $
          map createdNote (reportAccounts report)
            `shouldBe` [Just (Just "The key says no kind. The account is bip44 by default. If the key is of a bip84 account, a sync finds none of its transactions and signtx refuses the mnemonic. Then deleteacc, and import the key again with --type bip84.")]

    it "notes that the kind of a bare key was taken on --type" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) (Just Bip84) ("{\"type\":\"xpub\",\"ref\":\"" <> testSegwitAccountXPub <> "\"}\n")
        liftIO $
          map createdNote (reportAccounts report)
            `shouldBe` [Just (Just "The key says no kind. The account is bip84 because --type says so. If the key is of a bip44 account, a sync finds none of its transactions and signtx refuses the mnemonic. Then deleteacc, and import the key again with --type bip44.")]

    it "notes nothing for a key that says its kind, by its purpose or by its origin" $
      inDatabaseE $ do
        purpose <- importFile ctx (Just btc) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testSegwitAccountXPub <> "\",\"purpose\":84}\n")
        liftIO $ map createdNote (reportAccounts purpose) `shouldBe` [Just Nothing]
        origin <- importFile ctx (Just btc) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\"}\n")
        liftIO $ map createdNote (reportAccounts origin) `shouldBe` [Just Nothing]

    it "is refused as a bip84 account on Bitcoin Cash, which has no SegWit" $
      inDatabaseE $ do
        report <- importFile ctx (Just bch) Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPubBch <> "\",\"label\":\"cash\",\"purpose\":84}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "cash" "A bip84 account needs SegWit, which bch does not have"]

    it "is refused for an account the wallet holds as one kind when the record says another" $
      inDatabaseE $ do
        _ <- insertAccount Bip44 btc ctx "main" (testAccountPub ctx) Nothing
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"network\":\"btc\",\"purpose\":84}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The account main holds this key as a bip44 account, and the record says it is of a bip84 one"]

    -- The wallet has the account main of the second seed
    it "is refused a name another account has, and the import goes on" $
      inDatabaseE $ do
        _ <- testNewAcc2 ctx "main"
        report <- importFile ctx Nothing Nothing (mainKeyLine <> "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub1 <> "\",\"label\":\"second\",\"network\":\"btc\"}\n")
        liftIO $ map created (reportAccounts report) `shouldBe` [Nothing, Just ("second", testAccountXPub1)]
        liftIO $ take 1 (reportAccounts report) `shouldBe` [AccountRefused "main" "The account main already exists on btc"]

    -- The wallet has the account main on bch, the file one on btc
    it "takes a name an account of another network has" $
      inDatabaseE $ do
        _ <- testNewAccBch ctx "main"
        report <- importFile ctx Nothing Nothing mainKeyLine
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("main", testAccountXPub)]

    it "renames the account the wallet holds under the key to its label" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"vault\",\"network\":\"btc\"}\n")
        liftIO $ map renamed (reportAccounts report) `shouldBe` [Just ("main", "vault")]
        lift accountsStored `dbShouldBeE` [("vault", "btc", Just "bc560f95")]

    -- Sparrow labels a key with the name of its signer, BIP39
    it "keeps the name of that account against the label of a file of another wallet, which names no network" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"BIP39\"}\n")
        liftIO $ map kept (reportAccounts report) `shouldBe` [Just ("main", Nothing)]

    it "keeps the name of that account when the label is the name of another account, and says so" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        _ <- testNewAcc2 ctx "savings"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"savings\",\"network\":\"btc\"}\n")
        liftIO $ map kept (reportAccounts report) `shouldBe` [Just ("main", Just "The account savings already exists on btc")]

    it "gives an account stored without the fingerprint of its seed the one of the origin" $
      inDatabaseE $ do
        _ <- insertAccount Bip44 btc ctx "main" (testAccountPub ctx) Nothing
        report <- importFile ctx Nothing Nothing mainKeyLine
        liftIO $ map kept (reportAccounts report) `shouldBe` [Just ("main", Nothing)]
        lift accountsStored `dbShouldBeE` [("main", "btc", Just "bc560f95")]

    it "is refused when its origin names another seed than the account's" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"pkh([4435aa1c/44'/0'/0'])\",\"network\":\"btc\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The account main is of the seed bc560f95, not of 4435aa1c"]

    it "is refused when the wallet holds the key on another network" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"network\":\"bch\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The account main on btc holds this key, which is of no account on bch"]

    it "is read from the document of an earlier exportacc" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing "{\n  \"xpubkey\": \"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah\",\n  \"network\": \"btc\",\n  \"name\": \"main\",\n  \"fingerprint\": \"bc560f95\"\n}\n"
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("main", testAccountXPub)]
        lift accountsStored `dbShouldBeE` [("main", "btc", Just "bc560f95")]

    it "is not read from such a document on an unknown network" $
      inDatabase $
        dbShouldFail $ importFile ctx Nothing Nothing "{\"xpubkey\": \"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah\", \"network\": \"nowhere\", \"name\": \"main\"}"
  where
    created (AccountCreated acc _) = Just (dBAccountName acc, dBAccountXPubKey acc)
    created _ = Nothing
    renamed (AccountRenamed was acc) = Just (was, dBAccountName acc)
    renamed _ = Nothing
    kept (AccountKept acc note) = Just (dBAccountName acc, note)
    kept _ = Nothing

{- Importing descriptors -}

importDescriptorSpec :: Ctx -> Spec
importDescriptorSpec ctx =
  describe "A descriptor" $ do
    it "creates the account of its key, named after its network and index, with the fingerprint of its origin" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing (mainDescriptor <> "\n")
        lift accountsStored `dbShouldBeE` [("btc0", "btc", Just "bc560f95")]
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("btc0", testAccountXPub)]

    it "is read without a checksum, with hardened steps marked with an apostrophe, and for one chain" $
      inDatabaseE $ do
        _ <- importFile ctx (Just btc) Nothing ("pkh([bc560f95/44'/0'/0']" <> testAccountXPub <> "/0/*)\n")
        lift accountsStored `dbShouldBeE` [("btc0", "btc", Just "bc560f95")]

    it "is read from a key alone, which gives no fingerprint" $
      inDatabaseE $ do
        _ <- importFile ctx (Just btc) Nothing (testAccountXPub <> "\n")
        lift accountsStored `dbShouldBeE` [("btc0", "btc", Nothing)]

    it "needs the --network, as it names none" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing (mainDescriptor <> "\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused mainKeyStart "Give the --network, or a default-network in the configuration file"]

    -- The descriptor of the bip84 section of the Coldcard file of
    -- test/data/descriptors
    it "adds a bip84 account from a wpkh descriptor, named with -segwit after its network and index" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("wpkh([bc560f95/84h/0h/0h]" <> testSegwitAccountXPub <> "/<0;1>/*)#5nktextl\n")
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("btc0-segwit", testSegwitAccountXPub)]
        liftIO $ map createdKind (reportAccounts report) `shouldBe` [Just (Bip84, "/84'/0'/0'")]
        lift accountsStored `dbShouldBeE` [("btc0-segwit", "btc", Just "bc560f95")]

    -- BIP 380: the script function says the kind, with or without the
    -- origin of the key. hw writes such a descriptor for a bip84 account
    -- whose seed it does not know.
    it "adds a bip84 account from a wpkh descriptor without an origin, with no note, as the script function says its kind" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("wpkh(" <> testSegwitAccountXPub <> "/<0;1>/*)\n")
        liftIO $ map createdKind (reportAccounts report) `shouldBe` [Just (Bip84, "/84'/0'/0'")]
        liftIO $ map createdNote (reportAccounts report) `shouldBe` [Just Nothing]

    it "adds a bip44 account from a pkh descriptor without an origin, with no note" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("pkh(" <> testAccountXPub <> "/<0;1>/*)\n")
        liftIO $ map createdKind (reportAccounts report) `shouldBe` [Just (Bip44, "/44'/0'/0'")]
        liftIO $ map createdNote (reportAccounts report) `shouldBe` [Just Nothing]

    it "is refused without an origin when its script function is of no kind hw holds, as tr of a Taproot wallet" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("tr(" <> testSegwitAccountXPub <> "/<0;1>/*)\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "xpub6CQF8An9..." "The script tr() of the descriptor is of no kind of account hw holds: pkh() for bip44, wpkh() for bip84"]
        lift accountsStored `dbShouldBeE` []

    it "is refused without an origin when its script functions are those of a BIP49 wallet, sh(wpkh)" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("sh(wpkh(" <> testSegwitAccountXPub <> "/<0;1>/*))\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "xpub6CQF8An9..." "The script sh(wpkh()) of the descriptor is of no kind of account hw holds: pkh() for bip44, wpkh() for bip84"]

    it "is refused when it is of a kind of account hw does not hold, as sh(wpkh) of a BIP49 wallet" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("sh(wpkh([bc560f95/49h/0h/0h]" <> testAccountXPub <> "/<0;1>/*))\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused mainKeyStart "The key is not that of the bip44 account /44'/0'/0': its origin is sh(wpkh([bc560f95/49'/0'/0']))"]

    it "is refused with a private key" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPrv <> "/<0;1>/*)#w6xkgv40\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "xprv9yHxeaLA..." "The key is a private key. hw takes the public key of an account, and no file of accounts should hold a private one"]

    -- The zpub of the bip84 section of the Coldcard file of
    -- test/data/descriptors, which some wallets write for the key of a
    -- bip84 account
    it "adds a bip84 account from a zpub of SLIP-132, stored as its xpub, with no note, as the zpub says its kind" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing (testSegwitAccountZPub <> "\n")
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("btc0-segwit", testSegwitAccountXPub)]
        liftIO $ map createdKind (reportAccounts report) `shouldBe` [Just (Bip84, "/84'/0'/0'")]
        liftIO $ map createdNote (reportAccounts report) `shouldBe` [Just Nothing]

    it "adds it with --type bip84 as well, which agrees with the zpub" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) (Just Bip84) (testSegwitAccountZPub <> "\n")
        liftIO $ map createdKind (reportAccounts report) `shouldBe` [Just (Bip84, "/84'/0'/0'")]

    -- The prefix of the key and --type contradict: the import takes neither
    it "is refused with --type bip44 for a zpub, which marks a bip84 account" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) (Just Bip44) (testSegwitAccountZPub <> "\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "zpub6r4mjW7y..." "The key is a zpub, which marks a bip84 account in SLIP-132, and --type says bip44. Leave --type out, or write the key as an xpub or a tpub if it is of a bip44 account after all."]
        lift accountsStored `dbShouldBeE` []

    it "is refused with --type bip44 for a vpub likewise, on a test network" $
      inDatabaseE $ do
        report <- importFile ctx (Just btcTest) (Just Bip44) (testSegwitAccountVPub <> "\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "vpub5ZPKH6HZ..." "The key is a vpub, which marks a bip84 account in SLIP-132, and --type says bip44. Leave --type out, or write the key as an xpub or a tpub if it is of a bip44 account after all."]

    it "finds the account the wallet holds under its xpub from its zpub" $
      inDatabaseE $ do
        _ <- insertAccount Bip84 btc ctx "spending" (testSegwitAccountPub ctx) Nothing
        report <- importFile ctx (Just btc) Nothing (testSegwitAccountZPub <> "\n")
        liftIO $ map kept (reportAccounts report) `shouldBe` [Just ("spending", Nothing)]

    it "is refused with --type bip44 for the zpub of an account the wallet holds too" $
      inDatabaseE $ do
        _ <- insertAccount Bip84 btc ctx "spending" (testSegwitAccountPub ctx) Nothing
        report <- importFile ctx (Just btc) (Just Bip44) (testSegwitAccountZPub <> "\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "zpub6r4mjW7y..." "The key is a zpub, which marks a bip84 account in SLIP-132, and --type says bip44. Leave --type out, or write the key as an xpub or a tpub if it is of a bip44 account after all."]

    it "is refused with a ypub of SLIP-132, which marks a BIP49 account" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing "ypub6YGZhSg8Nso498vduxTVMtR3EtZUu8H3bKfUmJDp5x1QXjy9DdACFQ8VwcgtS6NSgzCMGs5mUSCP2iG3BtXCRBP8Zh7V74rwL9qRRbN1yGV\n"
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "ypub6YGZhSg8..." "The key has the prefix ypub of SLIP-132, which marks a BIP49 account, with P2SH-P2WPKH addresses: hw holds bip44 and bip84 accounts"]

    it "is refused with a wrong checksum, named by its start" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j88\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "pkh([bc560f9..." "The checksum of the descriptor is wrong"]

    it "is refused when its path does not derive the chains of an account" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/0/5)\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "pkh([bc560f9..." "The descriptor must end in /<0;1>/*, /0/* or /1/* after its key, to derive the addresses of an account"]

    -- Bitcoin Core writes an account as two descriptors, one per chain
    it "adds the account once from the two descriptors of its chains" $
      inDatabaseE $ do
        report <-
          importFile ctx (Just btc) Nothing $
            "pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/0/*)#77rvns4p\npkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/1/*)#02xdw99e\n"
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("btc0", testAccountXPub)]

    it "finds the account the wallet holds under its key, which learns the fingerprint of its origin" $
      inDatabaseE $ do
        _ <- insertAccount Bip44 btc ctx "main" (testAccountPub ctx) Nothing
        report <- importFile ctx (Just btc) Nothing (mainDescriptor <> "\n")
        liftIO $ map kept (reportAccounts report) `shouldBe` [Just ("main", Nothing)]
        lift accountsStored `dbShouldBeE` [("main", "btc", Just "bc560f95")]

    it "is read among the records of a BIP 329 file, before them" $
      inDatabaseE $ do
        report <-
          importFile ctx (Just btc) Nothing $
            "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n" <> mainDescriptor <> "\n"
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("btc0", testAccountXPub)]
        liftIO $ reportTxLabels report `shouldBe` 1
  where
    created (AccountCreated acc _) = Just (dBAccountName acc, dBAccountXPubKey acc)
    created _ = Nothing
    kept (AccountKept acc note) = Just (dBAccountName acc, note)
    kept _ = Nothing

{- Importing the files of other wallets -}

-- | A file of test/data/descriptors
descriptorFile :: FilePath -> IO Text
descriptorFile name = Text.readFile ("test/data/descriptors/" <> name)

importFileSpec :: Ctx -> Spec
importFileSpec ctx =
  describe "A file of descriptors of another wallet" $ do
    -- The default wallet of Bitcoin Core 32.1 on regtest, with pkh,
    -- sh(wpkh), tr and wpkh descriptors of its own seed, two chains each
    it "of Bitcoin Core's listdescriptors adds the BIP44 and the BIP84 accounts, and refuses the keys of the other kinds" $
      inDatabaseE $ do
        content <- liftIO $ descriptorFile "core-default-wallet.json"
        report <- importFile ctx (Just btcTest) Nothing content
        lift accountsStored `dbShouldBeE` [("btctest0", "btctest", Just "b079db60"), ("btctest0-segwit", "btctest", Just "b079db60")]
        liftIO $
          reportAccounts report
            `shouldBe` [ AccountCreated (accountNamed report "btctest0") Nothing,
                         AccountRefused "tpubDD6QPbsK..." "The key is not that of the bip44 account /44'/1'/0': its origin is sh(wpkh([b079db60/49'/1'/0']))",
                         AccountRefused "tpubDChx3pxw..." "The key is not that of the bip44 account /44'/1'/0': its origin is tr([b079db60/86'/1'/0'])",
                         AccountCreated (accountNamed report "btctest0-segwit") Nothing
                       ]
        liftIO $ map createdKind (reportAccounts report) `shouldBe` [Just (Bip44, "/44'/1'/0'"), Nothing, Nothing, Just (Bip84, "/84'/1'/0'")]

    -- A wallet of Core without private keys, watching the test account
    it "of a Bitcoin Core wallet that watches the test account adds it, with the fingerprint of its seed" $
      inDatabaseE $ do
        content <- liftIO $ descriptorFile "core-watch-only.json"
        report <- importFile ctx (Just btcTest) Nothing content
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("btctest0", testAccountXPubTest)]
        lift accountsStored `dbShouldBeE` [("btctest0", "btctest", Just "bc560f95")]

    it "of the generic JSON of a Coldcard adds the BIP44 and the BIP84 accounts, and refuses the BIP49 section" $
      inDatabaseE $ do
        content <- liftIO $ descriptorFile "coldcard-export.json"
        report <- importFile ctx (Just btc) Nothing content
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("btc0", testAccountXPub), Nothing, Just ("btc0-segwit", testSegwitAccountXPub)]
        liftIO $
          take 1 (drop 1 (reportAccounts report))
            `shouldBe` [AccountRefused "xpub6DSJPn1D..." "The key is not that of the bip44 account /44'/0'/0': its origin is sh(wpkh([bc560f95/49'/0'/0']))"]
        lift accountsStored `dbShouldBeE` [("btc0", "btc", Just "bc560f95"), ("btc0-segwit", "btc", Just "bc560f95")]

    it "of an older Coldcard, with the path and the key of each section but no descriptor, adds the account the same" $
      inDatabaseE $ do
        _ <- importFile ctx (Just btc) Nothing ("{\"xfp\": \"BC560F95\", \"bip44\": {\"deriv\": \"m/44'/0'/0'\", \"xpub\": \"" <> testAccountXPub <> "\"}}")
        lift accountsStored `dbShouldBeE` [("btc0", "btc", Just "bc560f95")]
  where
    created (AccountCreated acc _) = Just (dBAccountName acc, dBAccountXPubKey acc)
    created _ = Nothing
    -- The account the report created under the name
    accountNamed report name =
      case [acc | AccountCreated acc _ <- reportAccounts report, dBAccountName acc == name] of
        [acc] -> acc
        _ -> error ("No account " <> Text.unpack name <> " created")

{- Importing addresses -}

importAddressSpec :: Ctx -> Spec
importAddressSpec ctx = do
  describe "An addr record" $ do
    it "labels an address of the wallet, in whatever account has it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    -- The third receiving address, which the account has not generated
    it "derives an address the wallet does not have from its key path, generating the chain up to it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 2 <> "\",\"label\":\"From Bob\",\"origin\":\"" <> mainOrigin <> "\",\"keypath\":\"/0/2\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, ""), (1, ""), (2, "From Bob")]

    it "derives the address without an origin, in the account whose key gives it" $
      inDatabaseE $ do
        _ <- testNewAcc2 ctx "other"
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 1 <> "\",\"label\":\"From Bob\",\"keypath\":\"/0/1\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, ""), (1, "From Bob")]

    it "leaves the label of a change address aside, and generates none" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing ("{\"type\":\"addr\",\"ref\":\"" <> internalAddrsText !! 0 <> "\",\"label\":\"Change\",\"keypath\":\"/1/0\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportChangeAddresses = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountInternal acc `shouldBe` 0

    -- The address /0/1000, with /0/0 used: the thousandth past it
    it "derives an address up to a thousand past the last used one of its chain" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        updateAddressBalances btc accId [paidOnce (externalAddr 0) 1000]
        report <- importFile ctx Nothing Nothing "{\"type\":\"addr\",\"ref\":\"1KANnCm9fYm1z6A8o7hPMjsBCqgFWt9v8N\",\"label\":\"Far\",\"keypath\":\"/0/1000\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountExternal acc `shouldBe` 1001

    -- The address /0/1001, with /0/0 used: one past the thousand
    it "does not derive an address over a thousand past the last used one, and counts it among those not in the wallet" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        updateAddressBalances btc accId [paidOnce (externalAddr 0) 1000]
        report <- importFile ctx Nothing Nothing "{\"type\":\"addr\",\"ref\":\"1BUtUkmTvZHpz7wPeEJNTX4VLTgLNxKNt2\",\"label\":\"Far\",\"keypath\":\"/0/1001\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1, reportAddressesTooFar = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountExternal acc `shouldBe` 1

    -- The address /0/1000 of a chain with no used address: the thousand
    -- count from its start
    it "counts the thousand from the start of a chain with no used address" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing "{\"type\":\"addr\",\"ref\":\"1KANnCm9fYm1z6A8o7hPMjsBCqgFWt9v8N\",\"label\":\"Far\",\"keypath\":\"/0/1000\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1, reportAddressesTooFar = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountExternal acc `shouldBe` 1

    it "counts an address the wallet does not have and no key path places" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 5 <> "\",\"label\":\"Lost\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1}

    it "does nothing without a label" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\"}\n"
        liftIO $ report `shouldBe` nothingDone
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

    it "removes a label with an empty one" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        _ <- genExtAddress ctx def accId "Salary"
        _ <- importFile ctx Nothing Nothing "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"\"}\n"
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

  describe "A pubkey record" $ do
    it "labels the address of the key, when the wallet has it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        -- The key of the first receiving address, /0/0 of the account key
        let key = derivePubPath ctx (Deriv :/ 0 :/ 0) (testAccountPub ctx)
            hex = encodeHex (exportPubKey ctx True key.key)
        report <- importFile ctx Nothing Nothing ("{\"type\":\"pubkey\",\"ref\":\"" <> hex <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    -- An x-only key of 32 bytes, as Taproot writes them
    it "is left aside when hw cannot read the key" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing "{\"type\":\"pubkey\",\"ref\":\"83409659355b6d1cc3c32decd5d561abaac86c37a353b52895a5e6c196d6f448\",\"label\":\"Taproot\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportNotKept = 1}

{- Importing transactions -}

importTxSpec :: Ctx -> Spec
importTxSpec ctx =
  describe "A tx record" $ do
    it "labels the transaction in the account its origin names, before the sync has it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (txLabel accId (fakeTxHash 1)) `dbShouldBeE` "Salary"

    -- The accounts main and second of the seed both have the payment
    it "labels a transaction without origin in every account that has it" $
      inDatabaseE $ do
        (mainId, _) <- testNewAcc ctx "main"
        (secondId, _) <- testNewAcc ctx "second"
        (thirdId, _) <- testNewAcc ctx "third"
        lift $ paymentStored mainId 800002
        lift $ paymentStored secondId 800002
        report <- importFile ctx Nothing Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 2}
        lift (mapM (`txLabel` fakeTxHash 1) [mainId, secondId, thirdId]) `dbShouldBeE` ["Salary", "Salary", ""]

    it "labels a transaction without origin in the only account of the wallet, which need not have it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (txLabel accId (fakeTxHash 1)) `dbShouldBeE` "Salary"

    it "counts a transaction without origin that no account of several has" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        _ <- testNewAcc ctx "second"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxsMissing = 1}

    -- The origin of account 0 of the second seed
    it "counts a transaction whose origin names an account the wallet does not have" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"pkh([4435aa1c/44'/0'/0'])\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportOtherAccounts = 1}

    it "matches an origin to an account whose seed hw does not know yet" $
      inDatabaseE $ do
        (accId, _) <- insertAccount Bip44 btc ctx "main" (testAccountPub ctx) Nothing
        _ <- testNewAcc2 ctx "other"
        report <- importFile ctx Nothing Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (txLabel accId (fakeTxHash 1)) `dbShouldBeE` "Salary"

    it "is counted among the unreadable lines when its reference is not a transaction id" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing Nothing "{\"type\":\"tx\",\"ref\":\"not-a-txid\",\"label\":\"Salary\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportUnreadable = [1]}

{- Importing coins -}

importCoinSpec :: Ctx -> Spec
importCoinSpec ctx =
  describe "An output record" $ do
    it "locks the coin when it is not spendable, as lockcoin does" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        lift $ coinStored accId False
        report <- importFile ctx Nothing Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsLocked = 1}
        lift (fmap (fmap dBCoinLockedByUser) (P.get (DBCoinKey (outpointBytes (OutPoint (fakeTxHash 1) 0))))) `dbShouldBeE` Just True

    it "unlocks the coin when it is spendable" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        lift $ coinStored accId True
        report <- importFile ctx Nothing Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":true}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsUnlocked = 1}
        lift (fmap (fmap dBCoinLockedByUser) (P.get (DBCoinKey (outpointBytes (OutPoint (fakeTxHash 1) 0))))) `dbShouldBeE` Just False

    it "counts a coin the wallet does not have" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsMissing = 1}

    -- hw keeps no label on a coin: a coin shows the label of its transaction
    it "leaves the label of a coin aside" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        lift $ coinStored accId False
        report <- importFile ctx Nothing Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (received)\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportNotKept = 1}

{- The rest of a file -}

importOtherSpec :: Ctx -> Spec
importOtherSpec ctx =
  describe "The import of a file" $ do
    -- Line 3 is of a type the BIP does not define, line 4 is not JSON, and
    -- line 5 has no reference
    it "leaves aside inputs, silent payment keys and other types, and numbers the lines that are not records" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <-
          importFile ctx Nothing Nothing $
            Text.unlines
              [ "{\"type\":\"input\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (input)\"}",
                "{\"type\":\"spscan\",\"ref\":\"spscan1q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zsq9q6qjevn2kmdrnpuxt0v6h2kr2a2epkr0g6nk55ftf0xcxtddazgkrth3e\",\"label\":\"Scan\"}",
                "{\"type\":\"future\",\"ref\":{\"not\":\"validated\"},\"label\":\"Unknown\"}",
                "not a record",
                "{\"type\":\"addr\",\"label\":\"Salary\"}",
                "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}"
              ]
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1, reportNotKept = 3, reportUnreadable = [4, 5]}

    it "takes the keys first, wherever they are in the file, so that their records find their accounts" $
      inDatabaseE $ do
        report <-
          importFile ctx Nothing Nothing $
            "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n" <> mainKeyLine
        liftIO $ reportTxLabels report `shouldBe` 1

    it "does nothing with an empty file" $
      inDatabaseE $ do
        report <- importFile ctx Nothing Nothing ""
        liftIO $ report `shouldBe` nothingDone
        liftIO $ reportNeedsSync report `shouldBe` False

    it "needs a sync when it labels something the wallet does not have" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <- importFile ctx Nothing Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ reportNeedsSync report `shouldBe` True

{- A word alone on a line -}

wordLineSpec :: Ctx -> Spec
wordLineSpec ctx =
  describe "A word alone on a line of a file" $ do
    -- Two words of the test mnemonic, one a line, as a backup on paper is
    -- copied
    it "is a line that is not a record when it does not start as an extended key does" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing "snow\nsenior\n"
        liftIO $ report `shouldBe` nothingDone {reportUnreadable = [1, 2]}

    -- The key of main, with its last character changed from h to a
    it "is refused as a key, with the reason, when it starts as an extended key does" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) Nothing "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uaa\n"
        liftIO $ reportAccounts report `shouldBe` [AccountRefused mainKeyStart "The key does not read as a key of btc"]

{- Showing a file -}

-- | The file of accounts of the text, as show shows it
shownOf :: Ctx -> Text -> ExceptT String (DB IO) FileShown
shownOf ctx content = fileShown ctx Nothing =<< liftEither (readAccountFile ctx content)

-- | The records of each account of a file as show sorts them, by their
-- references, then the records of no account of the file
recordsSorted :: FileShown -> ([[Text]], [Text])
recordsSorted shown = ([recordRef <$> records | (_, records) <- shownAccounts shown], recordRef <$> shownOthers shown)

showFileSpec :: Ctx -> Spec
showFileSpec ctx = do
  describe "A file of accounts as show shows it" $ do
    it "gives an account the wallet does not hold as importacc would create it, and the wallet keeps none of it" $
      inDatabaseE $ do
        shown <- shownOf ctx mainKeyLine
        liftIO $ map (created . fst) (shownAccounts shown) `shouldBe` [Just ("main", testAccountXPub)]
        lift accountsStored `dbShouldBeE` []

    -- The file names main savings, and hw wrote it: importacc would rename
    -- main
    it "keeps what the wallet held before, and undoes only what it took in" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        shown <- shownOf ctx ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"savings\",\"origin\":\"" <> mainOrigin <> "\",\"network\":\"btc\"}\n")
        liftIO $ map (renamed . fst) (shownAccounts shown) `shouldBe` [Just ("main", "savings")]
        lift accountsStored `dbShouldBeE` [("main", "btc", Just "bc560f95")]

    it "puts each record under the account of the file whose origin it names" $
      inDatabaseE $ do
        shown <-
          shownOf ctx $
            mainKeyLine
              <> secondKeyLine
              <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Savings\",\"origin\":\"pkh([bc560f95/44'/0'/1'])\"}\n"
              <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\",\"origin\":\"" <> mainOrigin <> "\"}\n"
        liftIO $ recordsSorted shown `shouldBe` ([["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"], [paymentTxid]], [])

    it "puts a record without an origin under the account of a file of one account" $
      inDatabaseE $ do
        shown <- shownOf ctx (mainKeyLine <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\"}\n")
        liftIO $ recordsSorted shown `shouldBe` ([["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]], [])

    it "puts a record without an origin apart in a file of two accounts" $
      inDatabaseE $ do
        shown <- shownOf ctx (mainKeyLine <> secondKeyLine <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\"}\n")
        liftIO $ recordsSorted shown `shouldBe` ([[], []], ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"])

    -- The origin of the test vectors of BIP 329, of another seed
    it "puts apart a record whose origin names no account of the file" $
      inDatabaseE $ do
        shown <- shownOf ctx (mainKeyLine <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Transaction\",\"origin\":\"wpkh([d34db33f/84'/0'/0'])\"}\n")
        liftIO $ recordsSorted shown `shouldBe` ([[]], [paymentTxid])

    -- The key of main alone says nothing of its seed. The wallet holds main
    -- and knows its seed, bc560f95.
    it "finds the account of a key without an origin by the fingerprint of the seed the wallet knows" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        shown <- shownOf ctx (testAccountXPub <> "\n" <> secondKeyLine <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n")
        liftIO $ recordsSorted shown `shouldBe` ([[paymentTxid], []], [])

    it "shows a coin with the lock the file gives it" $
      inDatabaseE $ do
        shown <- shownOf ctx (mainKeyLine <> "{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ map snd (shownAccounts shown) `shouldBe` [[(record OutputRecord (paymentTxid <> ":0")) {recordSpendable = Just False}]]

    it "leaves aside a coin with a label alone, which hw does not keep" $
      inDatabaseE $ do
        shown <- shownOf ctx (mainKeyLine <> "{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (received)\"}\n")
        liftIO $ (recordsSorted shown, shownNotKept shown) `shouldBe` (([[]], []), 1)

    it "counts the label of a change address apart, by its key path, and does not show it" $
      inDatabaseE $ do
        shown <- shownOf ctx (mainKeyLine <> "{\"type\":\"addr\",\"ref\":\"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn\",\"label\":\"Change\",\"keypath\":\"/1/0\"}\n")
        liftIO $ (recordsSorted shown, shownChangeLabels shown) `shouldBe` (([[]], []), 1)

    it "passes over a transaction without a label, which sets nothing" $
      inDatabaseE $ do
        shown <- shownOf ctx (mainKeyLine <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\"}\n")
        liftIO $ (recordsSorted shown, shownNotKept shown, shownUnreadable shown) `shouldBe` (([[]], []), 0, [])

    -- Line 1 names no transaction, line 4 is of a type the BIP does not
    -- define, line 5 is not JSON, and line 6 has no reference
    it "leaves aside what hw keeps no label for, and numbers the lines that are not records in their order" $
      inDatabaseE $ do
        shown <-
          shownOf ctx $
            Text.unlines
              [ "{\"type\":\"tx\",\"ref\":\"f91d0a8a\",\"label\":\"Salary\"}",
                "{\"type\":\"input\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (input)\"}",
                "{\"type\":\"spscan\",\"ref\":\"spscan1q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zsq9q6qjevn2kmdrnpuxt0v6h2kr2a2epkr0g6nk55ftf0xcxtddazgkrth3e\",\"label\":\"Scan\"}",
                "{\"type\":\"future\",\"ref\":{\"not\":\"validated\"},\"label\":\"Unknown\"}",
                "not a record",
                "{\"type\":\"addr\",\"label\":\"Salary\"}"
              ]
        liftIO $ (shownNotKept shown, shownUnreadable shown) `shouldBe` (3, [1, 5, 6])

  describe "A file" $ do
    it "holds nothing of a file of accounts when no line is a key or a record" $
      holdsAnything <$> readAccountFile ctx "Buy milk\nsnow\n" `shouldBe` Right False

    it "holds something of a file of accounts with a record of a type the BIP does not define" $
      holdsAnything <$> readAccountFile ctx "{\"type\":\"future\",\"ref\":\"a\"}\n" `shouldBe` Right True
  where
    created (AccountCreated acc _) = Just (dBAccountName acc, dBAccountXPubKey acc)
    created _ = Nothing
    renamed (AccountRenamed was acc) = Just (was, dBAccountName acc)
    renamed _ = Nothing
