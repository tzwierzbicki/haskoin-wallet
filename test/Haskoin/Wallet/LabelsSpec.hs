{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The accounts and labels of the wallet as the records of a BIP 329 file:
-- what exportacc writes for an account, and what importacc makes of each
-- kind of record, in a database of its own
module Haskoin.Wallet.LabelsSpec (spec) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Default (def)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Bip329
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address (genExtAddress)
import Haskoin.Wallet.Database.BestBlock (updateBest)
import Haskoin.Wallet.Database.TxInfo (repsertSummary)
import Haskoin.Wallet.Database.TxLabel (setTxLabel, txLabel)
import Haskoin.Wallet.Labels
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  exportSpec ctx
  importKeySpec ctx
  importAddressSpec ctx
  importTxSpec ctx
  importCoinSpec ctx
  importOtherSpec ctx

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

-- | The labels of the receiving addresses of an account, by index
addressLabels :: DBAccountId -> DB IO [(Int, Text)]
addressLabels accId = do
  rows <- P.selectList [DBAddressAccount P.==. accId, DBAddressInternal P.==. False] [P.Asc DBAddressIndex]
  return [(dBAddressIndex a, dBAddressLabel a) | P.Entity _ a <- rows]

-- | A report of an import that changed nothing
nothingDone :: ImportReport
nothingDone = ImportReport [] 0 0 0 0 0 0 0 0 0 0 []

{- Exporting -}

exportSpec :: Ctx -> Spec
exportSpec ctx =
  describe "The records of an account" $ do
    -- The payment has 6 confirmations at best block 800005
    it "are its key with its name, origin and network, then its labelled addresses, transactions and locked coins" $
      inDatabaseE $ do
        held@(accId, _) <- testNewAcc ctx "main"
        _ <- genExtAddress ctx def accId "Salary"
        lift $ setTxLabel accId (fakeTxHash 1) "First salary"
        lift $ paymentStored accId 800005
        lift $ coinStored accId True
        records <- accountRecords ctx held
        liftIO $
          map recordLine records
            `shouldBe` [ "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\"}",
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
        held@(accId, _) <- insertAccount btc ctx "main" (testAccountPub ctx) Nothing
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
        report <- importFile ctx Nothing mainKeyLine
        accounts <- lift accountsStored
        liftIO $ accounts `shouldBe` [("main", "btc", Just "bc560f95")]
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("main", testAccountXPub)]

    it "names an account without a label after its network and index, as recover does" $
      inDatabaseE $ do
        _ <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"network\":\"btc\"}\n")
        lift accountsStored `dbShouldBeE` [("btc0", "btc", Nothing)]

    -- A file of another wallet names no network
    it "takes the network of the command for a key that names none, and needs one" $ do
      inDatabaseE $ do
        _ <- importFile ctx (Just bch) ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPubBch <> "\",\"label\":\"cash\"}\n")
        lift accountsStored `dbShouldBeE` [("cash", "bch", Nothing)]
      inDatabaseE $ do
        report <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "Give the --network, or a default-network in the configuration file"]

    it "is refused for a key that does not read on the network" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPubTest <> "\",\"label\":\"test\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "test" "The key does not read as a key of btc"]

    it "is refused for an unknown network" $
      inDatabaseE $ do
        report <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"network\":\"nowhere\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "Unknown network nowhere"]

    -- The master key of the vectors of the BIP, at depth 0
    it "is refused for a key that is not three hardened steps below the master key" $
      inDatabaseE $ do
        report <- importFile ctx (Just btc) "{\"type\":\"xpub\",\"ref\":\"xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8\",\"label\":\"Extended Public Key\"}\n"
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "Extended Public Key" "The key is not that of a BIP44 account, three hardened steps below the master key"]

    it "is refused when its origin is not that of the BIP44 account, as a key of a BIP84 wallet" $
      inDatabaseE $ do
        report <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"wpkh([bc560f95/84'/0'/0'])\",\"network\":\"btc\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The key is not that of the BIP44 account /44'/0'/0': its origin is wpkh([bc560f95/84'/0'/0'])"]

    -- The wallet has the account main of the second seed
    it "is refused a name another account has, and the import goes on" $
      inDatabaseE $ do
        _ <- testNewAcc2 ctx "main"
        report <- importFile ctx Nothing (mainKeyLine <> "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub1 <> "\",\"label\":\"second\",\"network\":\"btc\"}\n")
        liftIO $ map created (reportAccounts report) `shouldBe` [Nothing, Just ("second", testAccountXPub1)]
        liftIO $ take 1 (reportAccounts report) `shouldBe` [AccountRefused "main" "The account main already exists"]

    it "renames the account the wallet holds under the key to its label" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"vault\",\"network\":\"btc\"}\n")
        liftIO $ map renamed (reportAccounts report) `shouldBe` [Just ("main", "vault")]
        lift accountsStored `dbShouldBeE` [("vault", "btc", Just "bc560f95")]

    -- Sparrow labels a key with the name of its signer, BIP39
    it "keeps the name of that account against the label of a file of another wallet, which names no network" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"BIP39\"}\n")
        liftIO $ map kept (reportAccounts report) `shouldBe` [Just ("main", Nothing)]

    it "keeps the name of that account when the label is the name of another account, and says so" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        _ <- testNewAcc2 ctx "savings"
        report <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"savings\",\"network\":\"btc\"}\n")
        liftIO $ map kept (reportAccounts report) `shouldBe` [Just ("main", Just "The account savings already exists")]

    it "gives an account stored without the fingerprint of its seed the one of the origin" $
      inDatabaseE $ do
        _ <- insertAccount btc ctx "main" (testAccountPub ctx) Nothing
        report <- importFile ctx Nothing mainKeyLine
        liftIO $ map kept (reportAccounts report) `shouldBe` [Just ("main", Nothing)]
        lift accountsStored `dbShouldBeE` [("main", "btc", Just "bc560f95")]

    it "is refused when its origin names another seed than the account's" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"pkh([4435aa1c/44'/0'/0'])\",\"network\":\"btc\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The account main is of the seed bc560f95, not of 4435aa1c"]

    it "is refused when the wallet holds the key on another network" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"network\":\"bch\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountRefused "main" "The account main on btc holds this key, which is of no account on bch"]

    it "is read from the document of an earlier exportacc" $
      inDatabaseE $ do
        report <- importFile ctx Nothing "{\n  \"xpubkey\": \"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah\",\n  \"network\": \"btc\",\n  \"name\": \"main\",\n  \"fingerprint\": \"bc560f95\"\n}\n"
        liftIO $ map created (reportAccounts report) `shouldBe` [Just ("main", testAccountXPub)]
        lift accountsStored `dbShouldBeE` [("main", "btc", Just "bc560f95")]

    it "is not read from such a document on an unknown network" $
      inDatabase $
        dbShouldFail $ importFile ctx Nothing "{\"xpubkey\": \"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah\", \"network\": \"nowhere\", \"name\": \"main\"}"
  where
    created (AccountCreated acc) = Just (dBAccountName acc, dBAccountXPubKey acc)
    created _ = Nothing
    renamed (AccountRenamed was acc) = Just (was, dBAccountName acc)
    renamed _ = Nothing
    kept (AccountKept acc note) = Just (dBAccountName acc, note)
    kept _ = Nothing

{- Importing addresses -}

importAddressSpec :: Ctx -> Spec
importAddressSpec ctx = do
  describe "An addr record" $ do
    it "labels an address of the wallet, in whatever account has it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    -- The third receiving address, which the account has not generated
    it "derives an address the wallet does not have from its key path, generating the chain up to it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 2 <> "\",\"label\":\"From Bob\",\"origin\":\"" <> mainOrigin <> "\",\"keypath\":\"/0/2\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, ""), (1, ""), (2, "From Bob")]

    it "derives the address without an origin, in the account whose key gives it" $
      inDatabaseE $ do
        _ <- testNewAcc2 ctx "other"
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 1 <> "\",\"label\":\"From Bob\",\"keypath\":\"/0/1\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, ""), (1, "From Bob")]

    it "leaves the label of a change address aside, and generates none" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing ("{\"type\":\"addr\",\"ref\":\"" <> internalAddrsText !! 0 <> "\",\"label\":\"Change\",\"keypath\":\"/1/0\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportChangeAddresses = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountInternal acc `shouldBe` 0

    it "counts an address the wallet does not have and no key path places" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <- importFile ctx Nothing ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 5 <> "\",\"label\":\"Lost\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1}

    it "does nothing without a label" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        report <- importFile ctx Nothing "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\"}\n"
        liftIO $ report `shouldBe` nothingDone
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

    it "removes a label with an empty one" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        _ <- genExtAddress ctx def accId "Salary"
        _ <- importFile ctx Nothing "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"\"}\n"
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

  describe "A pubkey record" $ do
    it "labels the address of the key, when the wallet has it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        -- The key of the first receiving address, /0/0 of the account key
        let key = derivePubPath ctx (Deriv :/ 0 :/ 0) (testAccountPub ctx)
            hex = encodeHex (exportPubKey ctx True key.key)
        report <- importFile ctx Nothing ("{\"type\":\"pubkey\",\"ref\":\"" <> hex <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    -- An x-only key of 32 bytes, as Taproot writes them
    it "is left aside when hw cannot read the key" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <- importFile ctx Nothing "{\"type\":\"pubkey\",\"ref\":\"83409659355b6d1cc3c32decd5d561abaac86c37a353b52895a5e6c196d6f448\",\"label\":\"Taproot\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportNotKept = 1}

{- Importing transactions -}

importTxSpec :: Ctx -> Spec
importTxSpec ctx =
  describe "A tx record" $ do
    it "labels the transaction in the account its origin names, before the sync has it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        report <- importFile ctx Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n")
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
        report <- importFile ctx Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 2}
        lift (mapM (`txLabel` fakeTxHash 1) [mainId, secondId, thirdId]) `dbShouldBeE` ["Salary", "Salary", ""]

    it "labels a transaction without origin in the only account of the wallet, which need not have it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        report <- importFile ctx Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (txLabel accId (fakeTxHash 1)) `dbShouldBeE` "Salary"

    it "counts a transaction without origin that no account of several has" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        _ <- testNewAcc ctx "second"
        report <- importFile ctx Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxsMissing = 1}

    -- The origin of account 0 of the second seed
    it "counts a transaction whose origin names an account the wallet does not have" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"pkh([4435aa1c/44'/0'/0'])\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportOtherAccounts = 1}

    it "matches an origin to an account whose seed hw does not know yet" $
      inDatabaseE $ do
        (accId, _) <- insertAccount btc ctx "main" (testAccountPub ctx) Nothing
        _ <- testNewAcc2 ctx "other"
        report <- importFile ctx Nothing ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (txLabel accId (fakeTxHash 1)) `dbShouldBeE` "Salary"

    it "is counted among the unreadable lines when its reference is not a transaction id" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        report <- importFile ctx Nothing "{\"type\":\"tx\",\"ref\":\"not-a-txid\",\"label\":\"Salary\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportUnreadable = [1]}

{- Importing coins -}

importCoinSpec :: Ctx -> Spec
importCoinSpec ctx =
  describe "An output record" $ do
    it "locks the coin when it is not spendable, as lockcoin does" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        lift $ coinStored accId False
        report <- importFile ctx Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsLocked = 1}
        lift (fmap (fmap dBCoinLockedByUser) (P.get (DBCoinKey (outpointBytes (OutPoint (fakeTxHash 1) 0))))) `dbShouldBeE` Just True

    it "unlocks the coin when it is spendable" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        lift $ coinStored accId True
        report <- importFile ctx Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":true}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsUnlocked = 1}
        lift (fmap (fmap dBCoinLockedByUser) (P.get (DBCoinKey (outpointBytes (OutPoint (fakeTxHash 1) 0))))) `dbShouldBeE` Just False

    it "counts a coin the wallet does not have" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <- importFile ctx Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsMissing = 1}

    -- hw keeps no label on a coin: a coin shows the label of its transaction
    it "leaves the label of a coin aside" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress ctx
        lift $ coinStored accId False
        report <- importFile ctx Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (received)\"}\n")
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
          importFile ctx Nothing $
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
          importFile ctx Nothing $
            "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n" <> mainKeyLine
        liftIO $ reportTxLabels report `shouldBe` 1

    it "does nothing with an empty file" $
      inDatabaseE $ do
        report <- importFile ctx Nothing ""
        liftIO $ report `shouldBe` nothingDone
        liftIO $ reportNeedsSync report `shouldBe` False

    it "needs a sync when it labels something the wallet does not have" $
      inDatabaseE $ do
        _ <- mainWithAddress ctx
        report <- importFile ctx Nothing ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ reportNeedsSync report `shouldBe` True
