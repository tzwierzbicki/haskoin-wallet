{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The accounts of the wallet: created from a seed, named, told apart by
-- their keys and grouped by the fingerprint of their seed once hw has
-- learnt it, listed, deleted, with their balances; and the document that
-- carries one from an offline wallet to an online one
module Haskoin.Wallet.AccountSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson (Value, eitherDecode, toJSON)
import Data.Default (def)
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address (genExtAddress)
import Haskoin.Wallet.Coin (refreshCoins)
import Haskoin.Wallet.Commands (Response (..), cmdExportAcc, cmdImportAcc)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Keys
import Haskoin.Wallet.PendingTx (importPendingTx)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxSignData (TxSignData (..))
import Numeric.Natural (Natural)
import System.FilePath ((</>))
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx -> do
  creatingSpec ctx
  namingSpec ctx
  fingerprintSpec ctx
  listingSpec ctx
  deletingSpec ctx
  balanceSpec ctx
  documentSpec ctx
  jsonSpec

{- Fixtures -}

-- | The key of an account of the test seed on a network, by index
keyOf :: Ctx -> Network -> Natural -> XPubKey
keyOf ctx = seedAccountKey (testSeed ctx)

-- | The key of account 1 of the test seed on Bitcoin, m/44'/0'/1'
savingsXPub :: Text
savingsXPub = "xpub6CHK45s4QJWpt9C5kpAnypsE5ratNMWRGE3pZNQgGnxHTvZ1y6E65mSKMmz7V5ZUnVq5JMgq1Qros98GhLMCK7eoDaNo8kjqNdJzD8otqLW"

-- | An account of the test seed stored without the fingerprint of its
-- seed, as a document of an earlier hw imports it
watchOnly :: Ctx -> Network -> Natural -> Text -> ExceptT String (DB IO) (DBAccountId, DBAccount)
watchOnly ctx net index name = insertAccount net ctx name (keyOf ctx net index) Nothing

-- | The names of the accounts as hw lists them, with their fingerprints
listed :: DB IO [(Text, Maybe Text)]
listed = map (\(_, a) -> (dBAccountName a, dBAccountFingerprint a)) <$> getAccounts

-- | The account given its first receiving address and a coin of the
-- amount at it, paid by the transaction returned
fundedWith :: Ctx -> DBAccountId -> Natural -> ExceptT String (DB IO) Tx
fundedWith ctx accId amount = do
  _ <- genExtAddress ctx def accId ""
  let funding = fundingTx ctx 1 [(externalAddr 0, amount)]
  _ <- refreshCoins btc accId externalAddrs [storeCoin ctx (txHash funding, 0) (Just 800000) (externalAddr 0) amount]
  return funding

-- | The coin of the funding spent to another wallet, 40000 of it, with the
-- signing data naming a seed as a signer writes it
paymentOf :: Ctx -> Tx -> Fingerprint -> TxSignData
paymentOf ctx funding fingerprint =
  TxSignData (spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 40000)]) [funding] [Deriv :/ 0 :/ 0] [] Nothing (Just fingerprint)

{- Creating -}

creatingSpec :: Ctx -> Spec
creatingSpec ctx =
  describe "Creating an account" $ do
    it "stores the first account of a seed at index 0, under its BIP44 path, with the fingerprint of the seed" $
      inDatabaseE $ do
        (_, acc) <- testNewAcc ctx "main"
        liftIO $ do
          dBAccountName acc `shouldBe` "main"
          dBAccountIndex acc `shouldBe` 0
          dBAccountNetwork acc `shouldBe` "btc"
          dBAccountDerivation acc `shouldBe` "/44'/0'/0'"
          dBAccountXPubKey acc `shouldBe` testAccountXPub
          dBAccountFingerprint acc `shouldBe` Just "bc560f95"
          (dBAccountExternal acc, dBAccountInternal acc) `shouldBe` (0, 0)
          (dBAccountBalanceConfirmed acc, dBAccountBalanceUnconfirmed acc, dBAccountBalanceCoins acc) `shouldBe` (0, 0, 0)

    it "gives the next account of the seed the next index" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        (_, savings) <- testNewAcc ctx "savings"
        liftIO $ do
          dBAccountIndex savings `shouldBe` 1
          dBAccountDerivation savings `shouldBe` "/44'/0'/1'"
          dBAccountXPubKey savings `shouldBe` savingsXPub

    it "counts the accounts of a seed by their keys: another seed's, or another network's, do not count" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        _ <- testNewAcc2 ctx "other"
        lift $ nextAccountIndex btc ctx (testSeed ctx) `dbShouldBe` 1
        lift $ nextAccountIndex btc ctx (testSeed2 ctx) `dbShouldBe` 1
        lift $ nextAccountIndex bch ctx (testSeed ctx) `dbShouldBe` 0

    it "takes the smallest index without an account, so that a deleted account comes back as it was" $
      inDatabaseE $ do
        (mainId, _) <- testNewAcc ctx "main"
        _ <- testNewAcc ctx "savings"
        lift $ nextAccountIndex btc ctx (testSeed ctx) `dbShouldBe` 2
        lift $ deleteAccount mainId
        lift $ nextAccountIndex btc ctx (testSeed ctx) `dbShouldBe` 0
        (_, again) <- testNewAcc ctx "main again"
        liftIO $ dBAccountXPubKey again `shouldBe` testAccountXPub

    it "records the path of an imported key from its index, with no seed known" $
      inDatabaseE $ do
        (_, acc) <- watchOnly ctx btc 3 "third"
        liftIO $ (dBAccountIndex acc, dBAccountDerivation acc, dBAccountFingerprint acc) `shouldBe` (3, "/44'/0'/3'", Nothing)

    it "refuses a name already taken" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        lift $ dbShouldFailWith "Account main already exists" $ watchOnly ctx btc 1 "main"

    it "refuses a key already held, naming its account" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        lift $ dbShouldFailWith "Account main already has this key" $ watchOnly ctx btc 0 "again"

    it "holds one test network per account index, since they share their keys" $
      inDatabaseE $ do
        -- Bitcoin testnet, Bitcoin Cash testnet and the regtests all use coin
        -- type 1: account 0 has the same key on each of them
        let key net = fromJust $ xPubImport net ctx testAccountXPubTest
        _ <- insertAccount btcTest ctx "testnet" (key btcTest) Nothing
        lift $
          dbShouldFailWith "Account testnet on btctest already has this key: the test networks share their account keys, so a wallet holds one of them per account index" $
            insertAccount bchTest ctx "cash testnet" (key bchTest) Nothing

{- Naming -}

namingSpec :: Ctx -> Spec
namingSpec ctx = do
  describe "Finding an account by name" $ do
    it "finds the account of the name" $
      inDatabaseE $ do
        (accId, acc) <- testNewAcc ctx "main"
        _ <- testNewAcc ctx "savings"
        getAccountByName (Just "main") `dbShouldBeE` (accId, acc)

    it "finds the only account without a name" $
      inDatabaseE $ do
        (accId, acc) <- testNewAcc ctx "main"
        getAccountByName Nothing `dbShouldBeE` (accId, acc)

    it "needs a name when there are several accounts" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        _ <- testNewAcc ctx "savings"
        lift $ dbShouldFailWith "Specify which account to use" $ getAccountByName Nothing

    it "has none to find in an empty wallet" $
      inDatabaseE $ lift $ dbShouldFailWith "There are no accounts in the wallet" $ getAccountByName Nothing

    it "knows no account of a name it does not have" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        lift $ dbShouldFailWith "The account nobody does not exist" $ getAccountByName (Just "nobody")

  describe "Renaming an account" $ do
    it "changes its name and nothing else" $
      inDatabaseE $ do
        (accId, acc) <- testNewAcc ctx "main"
        renameAccount "main" "salary" `dbShouldBeE` acc {dBAccountName = "salary"}
        getAccountByName (Just "salary") `dbShouldBeE` (accId, acc {dBAccountName = "salary"})

    it "changes nothing when the name is the one it has" $
      inDatabaseE $ do
        (_, acc) <- testNewAcc ctx "main"
        renameAccount "main" "main" `dbShouldBeE` acc

    it "refuses a name already taken" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        _ <- testNewAcc ctx "savings"
        lift $ dbShouldFailWith "The account savings already exists" $ renameAccount "main" "savings"

    it "refuses to rename an account that does not exist" $
      inDatabaseE $ lift $ dbShouldFailWith "The account nobody does not exist" $ renameAccount "nobody" "somebody"

{- The fingerprint of the seed -}

fingerprintSpec :: Ctx -> Spec
fingerprintSpec ctx =
  describe "The fingerprint of the seed" $ do
    it "is recorded on every account of the seed, on every network, once the seed is proven" $
      inDatabaseE $ do
        _ <- watchOnly ctx btc 0 "main"
        _ <- watchOnly ctx btc 1 "savings"
        _ <- watchOnly ctx bch 0 "cash"
        _ <- insertAccount btc ctx "other" (seedAccountKey (testSeed2 ctx) btc 0) Nothing
        lift $ recordFingerprint ctx (testSeed ctx)
        lift $ listed `dbShouldBe` [("cash", Just "bc560f95"), ("main", Just "bc560f95"), ("savings", Just "bc560f95"), ("other", Nothing)]

    it "is learnt from the offline wallet for an account that has none" $
      inDatabaseE $ do
        (accId, _) <- watchOnly ctx btc 0 "main"
        learnFingerprint accId testFingerprint
        (dBAccountFingerprint <$> getAccountById accId) `dbShouldBeE` Just "bc560f95"

    it "is accepted again once recorded, and another one refused" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        learnFingerprint accId testFingerprint
        lift $ dbShouldFailWith "The account main is of the seed bc560f95, not of 4435aa1c" $ learnFingerprint accId testFingerprint2

    it "arrives with a signed transaction, which names the seed that signed it" $
      inDatabaseE $ do
        (accId, _) <- watchOnly ctx btc 0 "main"
        funding <- fundedWith ctx accId 50000
        _ <- importPendingTx btc ctx accId $ signedByWallet ctx $ paymentOf ctx funding testFingerprint
        (dBAccountFingerprint <$> getAccountById accId) `dbShouldBeE` Just "bc560f95"

    it "refuses a signed transaction naming another seed than the account's" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        funding <- fundedWith ctx accId 50000
        lift $
          dbShouldFailWith "The account main is of the seed bc560f95, not of 4435aa1c" $
            importPendingTx btc ctx accId $ signedByWallet ctx $ paymentOf ctx funding testFingerprint2

{- Listing -}

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "Listing the accounts" $
    it "puts the accounts of one seed together, by network and index, and those of an unknown seed last" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        _ <- watchOnly ctx btc 2 "watch"
        _ <- testNewAccBch ctx "cash"
        _ <- testNewAcc2 ctx "other"
        _ <- testNewAcc ctx "savings"
        (map fst <$> lift listed) `dbShouldBeE` ["other", "cash", "main", "savings", "watch"]

{- Deleting -}

deletingSpec :: Ctx -> Spec
deletingSpec ctx =
  describe "Deleting an account" $
    it "deletes it with its addresses, summaries, coins and pending transactions, and nothing of another account" $
      inDatabaseE $ do
        (mainId, _) <- testNewAcc ctx "main"
        (otherId, _) <- testNewAcc2 ctx "other"
        forM_ [(mainId, externalAddr 0, 1), (otherId, externalAddr2, 2)] $ \(accId, addr, n) -> do
          _ <- genExtAddress ctx def accId ""
          _ <- refreshCoins btc accId [addr] [storeCoin ctx (fakeTxHash n, 0) (Just 800000) addr 1000]
          lift $ P.insert_ $ DBTxInfo accId (hashBytes (fakeTxHash n)) (S.encode (Store.BlockRef 800000 1)) "{}" False fixtureTime
          lift $ P.insert_ $ DBPendingTx accId (hashBytes (fakeTxHash (n + 2))) "{}" False fixtureTime
        lift $ rowsOf mainId `dbShouldBe` (1, 1, 1, 1)
        lift $ deleteAccount mainId
        lift $ rowsOf mainId `dbShouldBe` (0, 0, 0, 0)
        lift $ rowsOf otherId `dbShouldBe` (1, 1, 1, 1)
        lift $ (map fst <$> listed) `dbShouldBe` ["other"]
  where
    rowsOf accId =
      (,,,)
        <$> P.count [DBAddressAccount P.==. accId]
        <*> P.count [DBTxInfoAccount P.==. accId]
        <*> P.count [DBCoinAccount P.==. accId]
        <*> P.count [DBPendingTxAccount P.==. accId]

{- Balances -}

balanceSpec :: Ctx -> Spec
balanceSpec ctx =
  describe "The balances of an account" $ do
    it "are zero without an address" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        acc <- lift $ updateAccountBalances accId
        liftIO $ balances acc `shouldBe` (0, 0, 0)

    it "add up those of its addresses" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        first <- genExtAddress ctx def accId ""
        second <- genExtAddress ctx def accId ""
        lift $ P.update (key first) [DBAddressBalanceConfirmed P.=. 5000, DBAddressBalanceUnconfirmed P.=. 300, DBAddressBalanceCoins P.=. 2]
        lift $ P.update (key second) [DBAddressBalanceConfirmed P.=. 2000, DBAddressBalanceCoins P.=. 1]
        acc <- lift $ updateAccountBalances accId
        liftIO $ balances acc `shouldBe` (7000, 300, 3)
  where
    balances acc = (dBAccountBalanceConfirmed acc, dBAccountBalanceUnconfirmed acc, dBAccountBalanceCoins acc)
    key addr = DBAddressKey (dBAddressAccount addr) (dBAddressInternal addr) (dBAddressIndex addr)

{- The document -}

documentSpec :: Ctx -> Spec
documentSpec ctx =
  describe "The account document, from an offline wallet to an online one" $ do
    it "is written with the key, the network, the name and the fingerprint of the seed" $
      inScratchDirectory $ \dir -> do
        offline <- walletIn (dir </> "offline.sqlite") (testNewAcc ctx "main")
        let file = dir </> "main.json"
        cmdExportAcc ctx offline (Just "main") file `shouldReturn` ResponseFile file
        written <- readFile file
        decodeValue written `shouldBe` decodeValue mainDocument

    it "creates the account in the online wallet, with the fingerprint" $
      inScratchDirectory $ \dir -> do
        online <- walletIn (dir </> "online.sqlite") (return ())
        writeFile (dir </> "main.json") mainDocument
        response <- cmdImportAcc ctx online (dir </> "main.json")
        imported response `shouldBe` Right ("main", "btc", testAccountXPub, Just "bc560f95")

    it "records the fingerprint of an account the online wallet has already, and the same document again is refused" $
      inScratchDirectory $ \dir -> do
        online <- walletIn (dir </> "online.sqlite") (watchOnly ctx btc 0 "main")
        writeFile (dir </> "main.json") mainDocument
        response <- cmdImportAcc ctx online (dir </> "main.json")
        imported response `shouldBe` Right ("main", "btc", testAccountXPub, Just "bc560f95")
        cmdImportAcc ctx online (dir </> "main.json") `shouldReturn` ResponseError "Account main already exists"

    it "reads a document of an earlier hw, which named a wallet instead of the seed" $
      inScratchDirectory $ \dir -> do
        online <- walletIn (dir </> "online.sqlite") (return ())
        writeFile (dir </> "old.json") oldDocument
        response <- cmdImportAcc ctx online (dir </> "old.json")
        imported response `shouldBe` Right ("main", "btc", testAccountXPub, Nothing)

-- | A wallet in a database file, prepared by the action, and the
-- configuration that points hw at it
walletIn :: FilePath -> ExceptT String (DB IO) a -> IO Config
walletIn database prepare = do
  inDatabaseFile database $ do
    createTables migrateAll
    _ <- liftIO . expectRight =<< runExceptT prepare
    return ()
  return def {configDatabaseFile = Just database}

-- | What importacc answered: the account by its name, network, key and
-- fingerprint, or its refusal
imported :: Response -> Either String (Text, Text, Text, Maybe Text)
imported (ResponseAccount acc) = Right (dBAccountName acc, dBAccountNetwork acc, dBAccountXPubKey acc, dBAccountFingerprint acc)
imported (ResponseError err) = Left (cs err)
imported other = Left (show other)

-- | The document of the account main of the test seed, as exportacc writes it
mainDocument :: String
mainDocument =
  [r|{
  "xpubkey": "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah",
  "network": "btc",
  "name": "main",
  "fingerprint": "bc560f95"
}|]

-- | The same account as hw 0.10 exported it, under its wallet id
oldDocument :: String
oldDocument =
  [r|{
  "xpubkey": "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah",
  "network": "btc",
  "name": "main",
  "wallet": "892eb8e4"
}|]

decodeValue :: String -> Value
decodeValue = either error id . eitherDecode . cs

{- JSON -}

jsonSpec :: Spec
jsonSpec =
  describe "Accounts in JSON" $ do
    it "prints an account as accounts does, and reads it back" $ do
      eitherDecode (cs accountJson) `shouldBe` Right (toJSON fixtureAccount)
      eitherDecode (cs accountJson) `shouldBe` Right fixtureAccount

    it "prints the fingerprint of a seed not known yet as null, and reads it back" $ do
      let unknown = fixtureAccount {dBAccountFingerprint = Nothing}
      eitherDecode (cs unknownSeedJson) `shouldBe` Right (toJSON unknown)
      eitherDecode (cs unknownSeedJson) `shouldBe` Right unknown

-- | The account main after two receiving and one change address, holding
-- 5000 confirmed and 300 unconfirmed in two coins
fixtureAccount :: DBAccount
fixtureAccount = DBAccount "main" 0 "btc" "/44'/0'/0'" 2 1 testAccountXPub (Just "bc560f95") 5000 300 2 fixtureTime

accountJson :: String
accountJson =
  [r|{
  "name": "main",
  "index": 0,
  "network": "btc",
  "derivation": "/44'/0'/0'",
  "external": 2,
  "internal": 1,
  "xPubKey": "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah",
  "fingerprint": "bc560f95",
  "balance": {"confirmed": 5000, "unconfirmed": 300, "coins": 2},
  "created": "2023-11-01T00:00:00Z"
}|]

-- | The same account before its seed was proven
unknownSeedJson :: String
unknownSeedJson =
  [r|{
  "name": "main",
  "index": 0,
  "network": "btc",
  "derivation": "/44'/0'/0'",
  "external": 2,
  "internal": 1,
  "xPubKey": "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah",
  "fingerprint": null,
  "balance": {"confirmed": 5000, "unconfirmed": 300, "coins": 2},
  "created": "2023-11-01T00:00:00Z"
}|]
