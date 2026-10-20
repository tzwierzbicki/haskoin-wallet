{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Helpers shared by the test modules. The QuickCheck generators are in
-- "Haskoin.Wallet.Arbitrary".
module Haskoin.Wallet.TestUtils where

import Control.Monad (forM, unless, void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.!=), (.:), (.:?))
import qualified Data.ByteString as BS
import Data.Either (fromRight, isRight)
import Data.List (isSuffixOf, sort, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (runMigrationQuiet)
import Database.Persist.Sqlite (runSqlite)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database (DB, DBAccount, DBAccountId, insertAccount, migrateAll, nextAccountDeriv)
import Haskoin.Wallet.Keys (MnemonicPass (..), signingKey, walletFingerprint)
import Data.Word (Word32, Word8)
import Numeric.Natural (Natural)
import System.Directory (listDirectory)
import Test.HUnit (Assertion)
import Test.Hspec

{- Either -}

forceRight :: Either a b -> b
forceRight = fromRight (error "fromRight")

-- | The Right value, or a test failure showing the error
expectRight :: (HasCallStack) => Either String a -> IO a
expectRight = either (\err -> expectationFailure err >> fail err) return

{- Database -}

-- | Run an action on a fresh in-memory wallet database
runDBMemory :: DB IO a -> Assertion
runDBMemory action = do
  runSqlite ":memory:" $ do
    _ <- runMigrationQuiet migrateAll
    void action

-- | Like 'runDBMemory', and the action must succeed (Right)
runDBMemoryE :: (Show a) => ExceptT String (DB IO) a -> Assertion
runDBMemoryE action = do
  runSqlite ":memory:" $ do
    _ <- runMigrationQuiet migrateAll
    resE <- runExceptT action
    liftIO $ resE `shouldSatisfy` isRight

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
