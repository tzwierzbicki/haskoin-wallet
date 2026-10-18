{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Helpers shared by the test modules. The QuickCheck generators are in
-- "Haskoin.Wallet.Arbitrary".
module Haskoin.Wallet.TestUtils where

import Control.Monad (forM, unless, void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, withObject, (.!=), (.:), (.:?))
import Data.Either (fromRight, isRight)
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (runMigrationQuiet)
import Database.Persist.Sqlite (runSqlite)
import Haskoin
import Haskoin.Wallet.Database (DB, migrateAll)
import Haskoin.Wallet.Signing (MnemonicPass (..), signingKey)
import Numeric.Natural (Natural)
import System.Directory (listDirectory)
import Test.HUnit (Assertion)
import Test.Hspec

{- Either -}

forceRight :: Either a b -> b
forceRight = fromRight (error "fromRight")

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
