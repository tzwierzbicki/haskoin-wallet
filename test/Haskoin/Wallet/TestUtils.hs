{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Helpers shared by the test modules. The QuickCheck generators are in
-- "Haskoin.Wallet.Arbitrary".
module Haskoin.Wallet.TestUtils where

import Conduit (runConduit, sinkList, (.|))
import Control.Exception (finally)
import Control.Monad (forM, unless, void, when)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.!=), (.:), (.:?))
import qualified Data.ByteString as BS
import Data.Either (fromRight, isLeft)
import Data.IORef (IORef, modifyIORef)
import Data.List (find, isSuffixOf, sort, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.Word (Word32, Word64, Word8)
import Database.Persist.Sql (Migration, PersistValue, Single (..), rawQuery, rawSql, runMigrationQuiet)
import Database.Persist.Sqlite (runSqlite)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account (insertAccount, nextAccountDeriv)
import Haskoin.Wallet.Database (DB, DBAccount, DBAccountId, migrateAll)
import Haskoin.Wallet.Keys (MnemonicPass (..), signingKey, walletFingerprint)
import Haskoin.Wallet.Store
import Haskoin.Wallet.TxSignData (TxSignData (..))
import Haskoin.Wallet.TxSigning (signWalletTx)
import Numeric.Natural (Natural)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

{- Either -}

forceRight :: Either a b -> b
forceRight = fromRight (error "fromRight")

-- | The Right value, or a test failure showing the error
expectRight :: (HasCallStack) => Either String a -> IO a
expectRight = either (\err -> expectationFailure err >> fail err) return

{- Databases -}

-- A test runs its database actions on a new in-memory database, which is
-- gone when the test ends. The wallet's tables are created unless the test
-- creates its own, as the migration tests do from old schemas.

-- | Run an action on a new in-memory database, without any table
inMemory :: DB IO a -> IO a
inMemory = runSqlite ":memory:"

-- | Run an action on a new in-memory database with the wallet's tables
inDatabase :: DB IO a -> IO a
inDatabase action = inMemory $ createTables migrateAll >> action

-- | Like 'inDatabase', and the action must succeed: an error fails the test
-- with its message
inDatabaseE :: ExceptT String (DB IO) a -> IO ()
inDatabaseE action = inDatabase $ do
  result <- runExceptT action
  liftIO $ void $ expectRight result

-- | Run an action on a database file, created if missing
inDatabaseFile :: FilePath -> DB IO a -> IO a
inDatabaseFile path = runSqlite (cs path)

-- | Create the tables of a schema: the wallet's ('migrateAll') or an old one
createTables :: Migration -> DB IO ()
createTables = void . runMigrationQuiet

-- | The creation time of the rows the tests write
fixtureTime :: UTCTime
fixtureTime = UTCTime (fromGregorian 2023 11 1) 0

-- | Run a test in a fresh directory under the temporary directory, removed
-- afterwards, for the tests that need database files
inScratchDirectory :: (FilePath -> IO a) -> IO a
inScratchDirectory test = do
  base <- getTemporaryDirectory
  let dir = base </> "hw-database-test"
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir
  createDirectoryIfMissing True dir
  test dir `finally` removeDirectoryRecursive dir

{- Checks inside a database action -}

-- | The result of a database action must be the expected value
dbShouldBe :: (HasCallStack, Eq a, Show a) => DB IO a -> a -> DB IO ()
dbShouldBe action expected = do
  actual <- action
  liftIO $ actual `shouldBe` expected

-- | The result of a database action must satisfy the condition
dbShouldSatisfy :: (HasCallStack, Show a) => DB IO a -> (a -> Bool) -> DB IO ()
dbShouldSatisfy action condition = do
  actual <- action
  liftIO $ actual `shouldSatisfy` condition

-- | The result of a database action that can fail must be the expected value
dbShouldBeE :: (HasCallStack, Eq a, Show a) => ExceptT String (DB IO) a -> a -> ExceptT String (DB IO) ()
dbShouldBeE action expected = do
  actual <- action
  liftIO $ actual `shouldBe` expected

-- | The database action must fail
dbShouldFail :: (HasCallStack, Show a) => ExceptT String (DB IO) a -> DB IO ()
dbShouldFail action = do
  result <- runExceptT action
  liftIO $ result `shouldSatisfy` isLeft

-- | The database action must fail with the given message
dbShouldFailWith :: (HasCallStack, Eq a, Show a) => String -> ExceptT String (DB IO) a -> DB IO ()
dbShouldFailWith message action = do
  result <- runExceptT action
  liftIO $ result `shouldBe` Left message

{- Raw views of a database -}

-- | The rows of a query, as raw values, in a fixed order
queryRows :: Text -> DB IO [[PersistValue]]
queryRows query = sort <$> runConduit (rawQuery query [] .| sinkList)

-- | All the rows of a table
tableRows :: Text -> DB IO [[PersistValue]]
tableRows table = queryRows $ "select * from " <> table

-- | All the rows of several tables
tablesRows :: [Text] -> DB IO [[[PersistValue]]]
tablesRows = mapM tableRows

-- | What the version table holds, each value with its SQLite storage class
storedVersions :: DB IO [(PersistValue, Text)]
storedVersions = do
  rows <- rawSql "select version, typeof(version) from d_b_version" []
  return [(value, storageClass) | (Single value, Single storageClass) <- rows]

-- | A new account of the test wallet in the database, with the given name
testNewAcc :: Ctx -> Text -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewAcc ctx name = do
  let fp = forceRight $ walletFingerprint btc ctx testMnemonic
  d <- lift $ nextAccountDeriv fp btc
  let prv = forceRight $ signingKey btc ctx testMnemonic d
      pub = deriveXPubKey ctx prv
  insertAccount btc ctx fp name pub

-- | A new account of the second test wallet in the database
testNewAcc2 :: Ctx -> Text -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewAcc2 ctx name = do
  let fp = forceRight $ walletFingerprint btc ctx testMnemonic2
  d <- lift $ nextAccountDeriv fp btc
  let prv = forceRight $ signingKey btc ctx testMnemonic2 d
      pub = deriveXPubKey ctx prv
  insertAccount btc ctx fp name pub

-- | A new Bitcoin Cash account of the test wallet in the database. hw
-- names a wallet by the fingerprint of its account 0 on the network, so
-- this account is filed under another wallet than the Bitcoin accounts of
-- the same mnemonic.
testNewAccBch :: Ctx -> Text -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewAccBch ctx name = do
  let fp = forceRight $ walletFingerprint bch ctx testMnemonic
  d <- lift $ nextAccountDeriv fp bch
  let prv = forceRight $ signingKey bch ctx testMnemonic d
      pub = deriveXPubKey ctx prv
  insertAccount bch ctx fp name pub

{- Test vector files -}

-- Test vectors are stored as JSON files in a directory under test/data (see
-- test/data/message-signing for an example). Each test module defines the
-- type of its files, loads them with 'loadVectorFiles' and checks each file
-- with 'vectorFileSpec'.

-- | Load every .json file of a directory, in alphabetical order. A file that
-- can not be parsed is returned as Left, so that the test reports it.
loadVectorFiles :: (FromJSON a) => FilePath -> IO [(FilePath, Either String a)]
loadVectorFiles dir = do
  files <- sort . filter (".json" `isSuffixOf`) <$> listDirectory dir
  forM files $ \f -> (f,) <$> eitherDecodeFileStrict (dir <> "/" <> f)

-- | One check per vector file. It passes only if every vector of the file
-- passes. Otherwise it reports every problem, not only the first one. The
-- caller gives the number of vectors in a file and the problems found in it.
vectorFileSpec :: (a -> Int) -> (a -> Problems) -> (FilePath, Either String a) -> Spec
vectorFileSpec _ _ (file, Left err) =
  it file $ expectationFailure $ "Could not parse the file: " <> err
vectorFileSpec vectorCount fileProblems (file, Right doc) =
  it (file <> " (" <> show (vectorCount doc) <> " vectors)") $
    unless (null problems) $
      expectationFailure $
        show (length problems)
          <> (if length problems == 1 then " failure:\n" else " failures:\n")
          <> unlines problems
  where
    problems = fileProblems doc

{- Collecting problems -}

-- | The problems found in a test vector, as readable messages. An empty list
-- means that the vector passed. Assertions like 'shouldBe' stop at the first
-- failure; collecting problems instead lets a test report every failing
-- vector of a file at once.
type Problems = [String]

-- | A problem if the actual value differs from the expected one
check :: (Eq a, Show a) => String -> a -> a -> Problems
check label expected actual =
  [ label <> ": expected " <> show expected <> ", got " <> show actual
    | expected /= actual
  ]

-- | A problem if the condition does not hold
require :: String -> Bool -> Problems
require label ok = ["failed: " <> label | not ok]

-- | Prefix the problems of a vector with its name
named :: Text -> Problems -> Problems
named name = map (\p -> "  " <> Text.unpack name <> ": " <> p)

prefixError :: String -> Either String a -> Either String a
prefixError p = either (Left . ((p <> ": ") <>)) Right

-- | The checks of a vector first read its data (a key, a signature, an
-- address...), which can fail (Left). Such a setup error is reported as the
-- only problem of the vector. Otherwise (Right), the problems found by the
-- checks are returned.
reportSetupError :: Either String Problems -> Problems
reportSetupError (Left setupError) = [setupError]
reportSetupError (Right problems) = problems

{- Networks and keys in test vectors -}

-- | Read a network by its haskoin name (btc, btctest, bch, bchtest...)
parseNetwork :: (MonadFail m) => String -> m Network
parseNetwork name =
  maybe (fail $ "Unknown network: " <> name) return (netByName name)

-- | How a private key is specified in a test vector. In JSON, one of:
--
-- * {"wif": ...}: WIF private key, compressed or uncompressed
-- * {"secretHex": ...}: 32-byte secret in hex
-- * {"mnemonic", "passphrase", "path"}: BIP-39 mnemonic and any BIP-32 path
-- * {"mnemonic", "passphrase", "account", "chain", "index"}: derived like hw
--   does, with 'signingKey' for the account key m/44'/coin'/account'
data KeySpec
  = KeyWif !Text
  | KeySecret !Text
  | -- | Mnemonic, passphrase and any BIP-32 path
    KeyPath !Text !Text !Text
  | -- | Mnemonic, passphrase, account, chain and index, derived like hw does
    KeyAccount !Text !Text !Natural !Natural !Natural

instance FromJSON KeySpec where
  parseJSON =
    withObject "KeySpec" $ \o -> do
      wifM <- o .:? "wif"
      secretM <- o .:? "secretHex"
      mnemonicM <- o .:? "mnemonic"
      pathM <- o .:? "path"
      pass <- o .:? "passphrase" .!= ""
      case (wifM, secretM, mnemonicM, pathM) of
        (Just wif, _, _, _) -> return $ KeyWif wif
        (_, Just secret, _, _) -> return $ KeySecret secret
        (_, _, Just mnem, Just path) -> return $ KeyPath mnem pass path
        (_, _, Just mnem, Nothing) ->
          KeyAccount mnem pass
            <$> o .: "account"
            <*> o .: "chain"
            <*> o .: "index"
        _ -> fail "Invalid key specification"

-- | The private key of a key specification
vectorKey :: Ctx -> Network -> KeySpec -> Either String SecKey
vectorKey ctx net =
  \case
    KeyWif wif -> (.key) <$> maybeToEither "Invalid WIF" (fromWif net wif)
    KeySecret secret ->
      maybeToEither "Invalid secret" $ secKey =<< decodeHex secret
    KeyPath mnem pass path -> do
      seed <- mnemonicToSeed pass mnem
      parsed <- maybeToEither "Invalid path" $ parsePath (Text.unpack path)
      return (derivePath ctx parsed.get (makeXPrvKey seed)).key
    KeyAccount mnem pass acc chain idx -> do
      accKey <- accountSigningKey net ctx (AccountSpec mnem pass acc)
      let path = Deriv :/ fromIntegral chain :/ fromIntegral idx :: SoftPath
      return (derivePath ctx path accKey).key

-- | An account of a BIP-39 mnemonic, derived like hw does: the account key is
-- m/44'/coin'/account'. In JSON: {"mnemonic", "passphrase", "account"}, where
-- the passphrase is optional.
data AccountSpec = AccountSpec !Text !Text !Natural

instance FromJSON AccountSpec where
  parseJSON =
    withObject "AccountSpec" $ \o ->
      AccountSpec
        <$> o .: "mnemonic"
        <*> o .:? "passphrase" .!= ""
        <*> o .: "account"

-- | The account private key of an account specification
accountSigningKey :: Network -> Ctx -> AccountSpec -> Either String XPrvKey
accountSigningKey net ctx (AccountSpec mnem pass acc) =
  signingKey net ctx (MnemonicPass mnem pass) acc

{- Synthetic transactions and coins -}

-- The transactions of the tests exist on no blockchain: a funding transaction
-- spends a fake outpoint and pays the addresses a test needs, and the test
-- then spends the coins of that funding transaction.

-- | A transaction hash that exists nowhere: one byte, then zeros
fakeTxHash :: Word8 -> TxHash
fakeTxHash w =
  either (error "fakeTxHash") id $ S.decode $ w `BS.cons` BS.replicate 31 0x00

-- | A block hash that exists nowhere: one byte, then zeros
fakeBlockHash :: Word8 -> BlockHash
fakeBlockHash w =
  either (error "fakeBlockHash") id $ S.decode $ w `BS.cons` BS.replicate 31 0x00

-- | An unsigned transaction spending the given outputs and paying the given
-- amounts to the given addresses (version 1, final sequence, no lock time)
spendingTx :: Ctx -> [(TxHash, Word32)] -> [(Address, Natural)] -> Tx
spendingTx ctx spent paid =
  Tx 1 [TxIn (OutPoint h i) BS.empty maxBound | (h, i) <- spent] (payment <$> paid) [] 0
  where
    payment (addr, amount) = TxOut (fromIntegral amount) (addressScript ctx addr)

-- | A transaction funded by a fake outpoint, paying the given addresses
fundingTx :: Ctx -> Word8 -> [(Address, Natural)] -> Tx
fundingTx ctx w = spendingTx ctx [(fakeTxHash w, 0)]

-- | A coin as the store reports it: at the given block height, or unconfirmed
storeCoin :: Ctx -> (TxHash, Word32) -> Maybe Natural -> Address -> Natural -> Store.Unspent
storeCoin ctx (h, i) heightM addr amount =
  Store.Unspent
    { Store.block = maybe (Store.MemRef 0) (\height -> Store.BlockRef (fromIntegral height) 0) heightM,
      Store.outpoint = OutPoint h i,
      Store.value = fromIntegral amount,
      Store.script = addressScript ctx addr,
      Store.address = Just addr
    }

-- | All the coins of a transaction, unconfirmed, as the store reports them
coinsOf :: Ctx -> Tx -> [Store.Unspent]
coinsOf ctx tx =
  [ Store.Unspent
      { Store.block = Store.MemRef 0,
        Store.outpoint = OutPoint (txHash tx) i,
        Store.value = out.value,
        Store.script = out.script,
        Store.address = either (const Nothing) Just (scriptToAddressBS ctx out.script)
      }
    | (i, out) <- zip [0 ..] tx.outputs
  ]

-- | The transaction funding coins worth the given amounts, on the external
-- addresses /0/0, /0/1, ... of the test wallet
fundingWorth :: Ctx -> [Natural] -> Tx
fundingWorth ctx values = fundingTx ctx 1 $ zip (externalAddr <$> [0 ..]) values

-- | The coins of 'fundingWorth', as the store reports them
coinsWorth :: Ctx -> [Natural] -> [Store.Unspent]
coinsWorth ctx = coinsOf ctx . fundingWorth ctx

-- | The output script paying an address
addressScript :: Ctx -> Address -> BS.ByteString
addressScript ctx addr =
  maybe (error "addressScript: unsupported address") (marshal ctx) (addressToOutput addr)

-- | The transaction with its outputs sorted by amount: the builders shuffle
-- them and the tests do not pin that order
sortedOutputs :: Tx -> Tx
sortedOutputs (Tx version inputs outputs witness locktime) =
  Tx version inputs (sortOn (.value) outputs) witness locktime

{- The test wallet -}

-- The tests share one wallet, written out in full so that any deviation in a
-- derivation is caught: a mnemonic with a passphrase, the fingerprint of the
-- wallet, the keys of its first Bitcoin account (m/44'/0'/0') and the first
-- ten addresses of the account's external and internal chains. A second
-- wallet and ten addresses of other wallets complete the set.

testMnemonic :: MnemonicPass
testMnemonic =
  MnemonicPass
    "snow senior nerve virus fabric now fringe clip marble interest analyst can"
    "correct horse battery staple"

testFingerprintText :: Text
testFingerprintText = "892eb8e4"

testFingerprint :: Fingerprint
testFingerprint = forceRight $ textToFingerprint testFingerprintText

-- | The keys of account 0 of the test wallet, m/44'/0'/0'
testAccountXPrv, testAccountXPub :: Text
testAccountXPrv = "xprv9yHxeaLAZvxXb9VtJNesqk8avfN8misGAW9DUW9eacZJNqsfZxqKLmK5jfmvFideQqGesviJeagzSQYCuQySjgvt7TdfowKja5aJqbgyuNh"
testAccountXPub = "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah"

testAccountKey :: XPrvKey
testAccountKey = fromMaybe (error "testAccountKey") $ xPrvImport btc testAccountXPrv

-- | The signing data signed with the account key of the test wallet, for
-- the fixtures that need a transaction with real signatures
signedByWallet :: Ctx -> TxSignData -> TxSignData
signedByWallet ctx tsd = fst $ forceRight $ signWalletTx btc ctx tsd testAccountKey

testAccountPub :: Ctx -> XPubKey
testAccountPub ctx = fromMaybe (error "testAccountPub") $ xPubImport btc ctx testAccountXPub

-- | The external (receiving) addresses /0/0 to /0/9 of the test account
externalAddrsText :: [Text]
externalAddrsText =
  [ "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
    "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
    "1Dg6Kg7kQuyiZz41HRWXKUWKRu6ZyEf1Nr",
    "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1",
    "1cWcYiGK7NwjPBJuKRqZxV4aymUnPu1mx",
    "1MZuimSXigp8oqxkVUvZofqHNtVjdcdAqc",
    "1JReTkpFnsrMqhSEJwUNZXPAyeTo2HQfnE",
    "1Hx9xWAHhcjea5uJnyADktCfcLbuBnRnwA",
    "1HXJhfiD7JFCGMFZnhKRsZxoPF7xDTqWXP",
    "1MZpAt1FofY69B6fzooFxZqe6SdrVrC3Yw"
  ]

-- | The internal (change) addresses /1/0 to /1/9 of the test account
internalAddrsText :: [Text]
internalAddrsText =
  [ "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn",
    "1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6",
    "1MZNPWwFwy2CqVgWBq6unPWBWrZTQ7WTnr",
    "19XbPiR98wmoJQZ42K8pVMzdCwSXZBh7iz",
    "1Gkn7EsphiaYuv6XXvG4Kyg3LSfqFMeXHX",
    "14VkCGcLkNqUwRMVjpLEyodAhXvzUWLqPM",
    "1PkyVUxPMGTLzUWNFNraMagACA1x3eD4CF",
    "1M2mmDhWTjEuqPfUdaQH6XPsr5i29gx581",
    "184JdZjasQUmNo2AimkbKAW2sxXMF9BAvK",
    "13b1QVnWFRwCrjvhthj4JabpnJ4nyxbBqm"
  ]

externalAddrs, internalAddrs :: [Address]
externalAddrs = testAddress <$> externalAddrsText
internalAddrs = testAddress <$> internalAddrsText

-- | Addresses of other wallets
otherAddrs :: [Address]
otherAddrs =
  testAddress
    <$> [ "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
          "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ",
          "191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q",
          "1FVnYNLRdR5vQkynApupUez6ZfcDqsLHdj",
          "1PmNJHnbk7Kct5FMqbEVRxqqR2mXVQKK5P",
          "18CaQNcVwzUkE9KvwmMd6a5UWNgqJFEAh1",
          "1M2Cv69B7LRud8su2wdd7HV2i6MrXqzdKP",
          "19xYPmoJ2XV1vJnSkzsrXUJXCgKvPE3ri4",
          "1N2JAKWVFAoKFEUci3tY3kvrGFY6poRgvm",
          "15EANoYyJoo1J51ERdQzNwZCyhEtPfcP8g"
        ]

-- | The external address /0/i, the internal address /1/i, and the i-th
-- address of another wallet
externalAddr, internalAddr, otherAddr :: Int -> Address
externalAddr i = externalAddrs !! i
internalAddr i = internalAddrs !! i
otherAddr i = otherAddrs !! i

testAddress :: Text -> Address
testAddress = fromMaybe (error "testAddress") . textToAddr btc

-- | A second wallet, with its fingerprint and the keys of its account 0
testMnemonic2 :: MnemonicPass
testMnemonic2 =
  MnemonicPass
    "boring auction demand filter frog accuse company exchange rely slogan trim typical"
    "correct horse battery staple"

testFingerprintText2 :: Text
testFingerprintText2 = "807a5cfb"

testFingerprint2 :: Fingerprint
testFingerprint2 = forceRight $ textToFingerprint testFingerprintText2

testAccountXPrv2, testAccountXPub2 :: Text
testAccountXPrv2 = "xprv9yXnZpEVdonEtT3strknsAgso5qq1cwRooo6susmzVmB5E2vvNw1KKRBgvwvNLxXdBHnkEN5R5uXi2QDs3tpkoBbL61NE6bnSbcrvH6keGa"
testAccountXPub2 = "xpub6CX8yKmPUBLY6w8LztHoEJdcM7gKR5fHB2ihgJHPYqJ9x2N5TvFFs7jfYCX6So9oYyu6eLDTG5dbQWPncv1PYJtXLJ4cwymhoCpeTEmnZFZ"

-- | The first external address of account 0 of the second wallet, /0/0
externalAddr2Text :: Text
externalAddr2Text = "1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2"

externalAddr2 :: Address
externalAddr2 = testAddress externalAddr2Text

-- | The test wallet on Bitcoin Cash: the public key of its account 0,
-- m/44'/145'/0', and the first external address of that account
testAccountXPubBch :: Text
testAccountXPubBch = "xpub6DXWb36CzdZvWbtSrYQkbxtapXciSuuQYJhtg4ysKAcppi5bjxxHPBNgZRyabXmF1WLotb4wVoqzGB7beYL1SLoHTd4xPCQnFRkXXt2NXTB"

bchExternalAddrText :: Text
bchExternalAddrText = "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"

bchExternalAddr :: Address
bchExternalAddr = fromMaybe (error "bchExternalAddr") (textToAddr bch bchExternalAddrText)

{- The scripted store -}

-- A store the sync and the commands can be played against without a
-- network, filled with the answers it gives

-- | What the store answers: every list is complete, as the real store's
-- are when asked for everything
data FakeStore = FakeStore
  { fakeBest :: Store.BlockData,
    -- | The blocks at each height, for the reorganisation check
    fakeBlocks :: [Store.BlockData],
    -- | Addresses not listed have an empty balance
    fakeBalances :: [Store.Balance],
    -- | The transactions of each address, newest first
    fakeTxRefs :: [(Address, [Store.TxRef])],
    fakeTransactions :: [Store.Transaction],
    fakeUnspents :: [Store.Unspent],
    -- | Raw transactions by the id the store knows them under. Most stores
    -- here list their payments under made-up ids rather than the hashes
    -- of these raw transactions, which the sync only notices when it
    -- checks which raw transactions it has: 'realPaymentStore' is the one
    -- to use for that.
    fakeRawTransactions :: [(TxHash, Tx)]
  }

toStore :: FakeStore -> Store
toStore fake =
  Store
    { storeHealth = return (),
      storeBestBlock = return fake.fakeBest,
      storeBlocksAtHeight = \height ->
        return $ filter ((== height) . (.height)) fake.fakeBlocks,
      storeBalances = \addrs ->
        return [fromMaybe (Store.Balance a 0 0 0 0 0) (find ((== a) . (.address)) fake.fakeBalances) | a <- addrs],
      storeAddressTxs = \addr count ->
        return $ take (fromIntegral count) $ fromMaybe [] $ lookup addr fake.fakeTxRefs,
      storeTransactions = \tids ->
        return $ filter ((`elem` tids) . (.txid)) fake.fakeTransactions,
      storeUnspents = \addrs ->
        return $ filter ((`elem` (Just <$> addrs)) . (.address)) fake.fakeUnspents,
      storeRawTransactions = \tids ->
        return [tx | (tid, tx) <- fake.fakeRawTransactions, tid `elem` tids],
      storePublish = \_ -> return ()
    }

-- | A store with nothing in it: no transactions, no coins, no balances
emptyStore :: FakeStore
emptyStore =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeBalances = [],
      fakeTxRefs = [],
      fakeTransactions = [],
      fakeUnspents = [],
      fakeRawTransactions = []
    }

-- | The store, noting every transaction handed to it for the network
notingPublished :: IORef [Tx] -> Store -> Store
notingPublished published store =
  store
    { storePublish = \tx -> do
        liftIO $ modifyIORef published (<> [tx])
        storePublish store tx
    }

-- | The store, noting every list of raw transactions it is asked for
notingRawRequests :: IORef [[TxHash]] -> Store -> Store
notingRawRequests asked store =
  store
    { storeRawTransactions = \tids -> do
        liftIO $ modifyIORef asked (<> [tids])
        storeRawTransactions store tids
    }

-- | The store, noting every list of addresses whose coins it is asked for
notingUnspentRequests :: IORef [[Address]] -> Store -> Store
notingUnspentRequests asked store =
  store
    { storeUnspents = \addrs -> do
        liftIO $ modifyIORef asked (<> [addrs])
        storeUnspents store addrs
    }

-- | The store, noting every height whose blocks it is asked for
notingBlockRequests :: IORef [BlockHeight] -> Store -> Store
notingBlockRequests asked store =
  store
    { storeBlocksAtHeight = \height -> do
        liftIO $ modifyIORef asked (<> [height])
        storeBlocksAtHeight store height
    }

-- | The store, noting every list of addresses whose balances it is asked for
notingBalanceRequests :: IORef [[Address]] -> Store -> Store
notingBalanceRequests asked store =
  store
    { storeBalances = \addrs -> do
        liftIO $ modifyIORef asked (<> [addrs])
        storeBalances store addrs
    }

-- | A block of the main chain, whose hash depends on the nonce
blockAt :: BlockHeight -> Word32 -> Store.BlockData
blockAt height nonce =
  Store.BlockData
    height
    True
    0
    (BlockHeader 1 (fakeBlockHash 0) (fakeTxHash 0).get 1700000000 0x1d00ffff nonce)
    1000
    4000
    []
    0
    0
    0

-- | A transaction of the store paying an amount to one of our addresses,
-- funded by somebody else, with a fee of 1000
creditTx :: Ctx -> TxHash -> Store.BlockRef -> Address -> Word64 -> Store.Transaction
creditTx ctx tid block addr amount = payoutTx ctx tid block [(addr, amount)]

-- | A transaction of the store paying several addresses, funded by somebody
-- else, with a fee of 1000
payoutTx :: Ctx -> TxHash -> Store.BlockRef -> [(Address, Word64)] -> Store.Transaction
payoutTx ctx tid block paid =
  Store.Transaction
    { Store.block = block,
      Store.version = 1,
      Store.locktime = 0,
      Store.inputs =
        [ Store.StoreInput
            (OutPoint (fakeTxHash 0xee) 0)
            0xffffffff
            BS.empty
            (addressScript ctx (otherAddr 0))
            (sum (snd <$> paid) + 1000)
            []
            (Just (otherAddr 0))
        ],
      Store.outputs = [Store.StoreOutput amount (addressScript ctx addr) Nothing (Just addr) | (addr, amount) <- paid],
      Store.deleted = False,
      Store.rbf = False,
      Store.timestamp = 1700000000,
      Store.txid = tid,
      Store.size = 226,
      Store.weight = 904,
      Store.fee = 1000
    }

-- | The same transaction, replaced or dropped: the store keeps it, marked
-- as deleted
replaced :: Store.Transaction -> Store.Transaction
replaced (Store.Transaction b v l i o _ r t x s w f) =
  Store.Transaction b v l i o True r t x s w f
