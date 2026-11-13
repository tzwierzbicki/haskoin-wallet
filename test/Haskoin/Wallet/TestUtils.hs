{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Helpers shared by the test modules. The QuickCheck generators are in
-- "Haskoin.Wallet.Arbitrary".
module Haskoin.Wallet.TestUtils where

import System.Posix.Files (setFileCreationMask)
import System.Posix.Types (FileMode)
import Conduit (runConduit, sinkList, (.|))
import Control.Exception (bracket)
import Control.Monad (forM, unless, void)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict, encode, object, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Pair, parseEither)
import Data.Default (def)
import qualified Data.ByteString as BS
import Data.Either (fromRight, isLeft)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (find, genericTake, isSuffixOf, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone (..), UTCTime (UTCTime), ZonedTime, fromGregorian, utcToZonedTime)
import Data.Word (Word32, Word64, Word8)
import Database.Persist.Sql (Migration, PersistValue, Single (..), rawQuery, rawSql, runMigrationQuiet, toSqlKey)
import Database.Persist.Sqlite (runSqlite)
import Haskoin
import Haskoin.Crypto.Keys.Slip39 (EncryptedMasterSecret (..))
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account (AccountKeys (..), NetworkView (..), PortfolioView (..), createPortfolio, nextPortfolioIndex, poolAccountOf, seedWallet)
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), Place (..), Portfolio (..), PortfolioRef (..), insertWallet, portfolioAt, portfolioPool)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database.Coin (JsonCoin (..))
import Haskoin.Wallet.Database (DB, DBAccount (..), DBAccountId, DBAddress (..), DBPendingTx (..), DBWallet, DBWalletId, Pool (..), hashBytes, migrateAll, poolNetworkName)
import Haskoin.Wallet.Entropy (Sources (..), systemEntropy)
import Haskoin.Wallet.Keys (AccountKind (..), ForeignKind (..), MnemonicPass (..), PoolPath (..), PublicSeed (..), SeedSecret (..), foreignAddress, keyAddress, kindNamed, legacyPath, portfolioNetworks, seedOf, signingKey)
import Haskoin.Wallet.ShortId (ShortId, readShortId)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync (ScanQuestion, SyncRes (..), Watcher (Watcher))
import Haskoin.Wallet.SyncStatus (Status, Step, noStatus)
import Haskoin.Wallet.Cli.Parser (Command, program)
import qualified Options.Applicative as O
import Haskoin.Wallet.Cli.Parser.Args (Args (..), CommandSpec (..))
import Haskoin.Wallet.Cli.Parser.Run (commandNamed)
import Haskoin.Wallet.Cli.Response (AccountCounts (..), Group (..))
import Haskoin.Wallet.Cli.Sheet (Cell (..), Sheet (..))
import Haskoin.Wallet.Cli.Style (Style, renderPlain)
import Haskoin.Wallet.Database.TxInfo (ChainStatus (..), MyInputs (..), MyOutputs (..), PendingStatus (..), TxInfo (..), TxStatus (..), TxSummary (..), TxType (..), pendingSigned)
import Haskoin.Wallet.Tx.SignData (TxSignData (..), noLabel, nosigHash)
import Haskoin.Wallet.Tx.Signing (signWalletTx)
import Numeric.Natural (Natural)
import Prettyprinter (Doc)
import System.Directory (listDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)
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

-- | A wallet in a database file, prepared by the action, and the
-- configuration that points hw at it, for the tests of the commands
walletInFile :: FilePath -> ExceptT String (DB IO) a -> IO Config
walletInFile database prepare = do
  inDatabaseFile database $ do
    createTables migrateAll
    _ <- liftIO . expectRight =<< runExceptT prepare
    return ()
  return def {configDatabaseFile = Just database}

-- | The configuration with every network of a portfolio shown, where a
-- new configuration shows btc and bch alone: for the tests that work on
-- a test network. A test of the hidden networks sets the networks it
-- shows itself.
showingEveryNetwork :: Config -> Config
showingEveryNetwork cfg = cfg {configShownNetworks = portfolioNetworks}

-- | Create the tables of a schema: the wallet's ('migrateAll') or an old one
createTables :: Migration -> DB IO ()
createTables = void . runMigrationQuiet

-- | The configuration a file with these keys alone gives, as hw reads
-- it: every other key keeps its default. The keys are written as the
-- file names them, so that a test of a default names its key.
configWith :: [Pair] -> Config
configWith keys = either (error . ("configWith: " <>)) id (parseEither parseJSON (object keys))

-- | The creation time of the rows the tests write
fixtureTime :: UTCTime
fixtureTime = UTCTime (fromGregorian 2023 11 1) 0

-- | The time the test stores give their transactions, 1700000000 seconds
-- after 1970: 14 Nov 2023 22:13:20 UTC
seenTime :: UTCTime
seenTime = UTCTime (fromGregorian 2023 11 14) (22 * 3600 + 13 * 60 + 20)

-- | The zone of a clock of Central Europe in winter, an hour ahead of UTC
cet :: TimeZone
cet = TimeZone 60 False "CET"

-- | 'seenTime' on a clock of Central Europe, an hour ahead of UTC in
-- November: 14 Nov 2023 23:13:20 CET
seenTimeInCet :: ZonedTime
seenTimeInCet = utcToZonedTime cet seenTime

-- | What a clock of Central Europe reads at 'seenTime': 22:13:20 UTC + 1
-- hour = 23:13:20
seenLocalInCet :: LocalTime
seenLocalInCet = LocalTime (fromGregorian 2023 11 14) (TimeOfDay 23 13 20)

-- | The three cells a sheet gives 'seenTime' on that clock: the time in
-- UTC, as the clock reads it, and how far the clock is from UTC
seenTimeCells :: [Cell]
seenTimeCells = [TimeCell seenTime, LocalTimeCell seenLocalInCet, TextCell "UTC+01:00"]

-- | The cells of each row of a sheet under the headings given, in the
-- order given: what a test of a wide sheet looks at. A heading the sheet
-- does not have fails the test.
cellsUnder :: [Text] -> Sheet -> [[Cell]]
cellsUnder headings sheet = [[row !! column heading | heading <- headings] | row <- sheetRows sheet]
  where
    column heading = case lookup heading (zip (sheetHeadings sheet) [0 ..]) of
      Just index -> index
      Nothing -> error ("The sheet has no column " <> show heading)

-- | The lines of a document of hw, without its colours
renderedLines :: Doc Style -> [String]
renderedLines = lines . renderPlain

-- | Run a test in a fresh directory under the temporary directory, removed
-- afterwards, for the tests that need database files. The directory has
-- a name of its own each time. With one fixed name, two runs of the tests
-- at once would empty the directory of each other, and scripts/dev runs
-- the tests as several processes.
inScratchDirectory :: (FilePath -> IO a) -> IO a
inScratchDirectory = withSystemTempDirectory "hw-database-test"

-- | Run the action with the umask given, then put the one before back. The
-- hw a workflow runs inherits it: what hw makes is shown private whatever
-- the umask of the computer running the tests.
underUmask :: FileMode -> IO a -> IO a
underUmask mask action = bracket (setFileCreationMask mask) setFileCreationMask (const action)

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

-- | What the command line reads from the words typed after hw: the command
-- the first word names, with its options, or the first line of why the
-- words are refused, as hw prints it after "Error: "
parsedLine :: [String] -> Either String Command
parsedLine [] = Left "No command"
parsedLine (name : rest) =
  case commandNamed program name of
    Nothing -> Left ("hw has no command " <> name)
    Just command ->
      case O.execParserPure O.defaultPrefs (O.info command.args.parser mempty) rest of
        O.Success parsed -> Right parsed
        O.Failure failure -> Left (takeWhile (/= '\n') (fst (O.renderFailure failure "hw")))
        O.CompletionInvoked _ -> Left "A request of the shell"

-- | Portfolio 0 of a seed, whole, as newportfolio creates it, with the
-- wallet of the seed: both are created when the database does not have
-- them, so a test asks for the portfolio wherever it needs it
portfolioOf :: Ctx -> MnemonicPass -> ExceptT String (DB IO) Portfolio
portfolioOf ctx mnem = do
  let seed = forceRight $ seedOf ctx mnem
  held <- walletOfSeed ctx seed
  found <- lift $ portfolioAt (fst held) 0
  case found of
    Just (key, row) -> return (Portfolio key row (fst held) (snd held))
    Nothing -> createPortfolio ctx seed held 0

-- | The next portfolio of a seed, whole, at the lowest free index of its
-- wallet, as newportfolio --existing creates it: portfolio 0 first, then
-- portfolio 1
nextPortfolioOf :: Ctx -> MnemonicPass -> ExceptT String (DB IO) Portfolio
nextPortfolioOf ctx mnem = do
  let seed = forceRight $ seedOf ctx mnem
  held <- walletOfSeed ctx seed
  index <- lift $ nextPortfolioIndex seed (fst held)
  createPortfolio ctx seed held index

-- | The wallet of a seed in the database, the one with its fingerprint,
-- created when the database has none
walletOfSeed :: Ctx -> PublicSeed -> ExceptT String (DB IO) (DBWalletId, DBWallet)
walletOfSeed ctx seed =
  maybe (lift (insertWallet (seedFingerprint seed))) return =<< seedWallet ctx seed

-- | The account of a kind of a portfolio on a network
accountOf :: Portfolio -> Network -> AccountKind -> ExceptT String (DB IO) (DBAccountId, DBAccount)
accountOf portfolio net = poolAccountOf (portfolioPool portfolio net)

-- | The BIP44 account of portfolio 0 of a seed on a network, the portfolio
-- created when missing
newAccountOf :: Ctx -> MnemonicPass -> Network -> ExceptT String (DB IO) (DBAccountId, DBAccount)
newAccountOf = newAccountOfKind Bip44

-- | The account of a kind of portfolio 0 of a seed on a network, the
-- portfolio created when missing
newAccountOfKind :: AccountKind -> Ctx -> MnemonicPass -> Network -> ExceptT String (DB IO) (DBAccountId, DBAccount)
newAccountOfKind kind ctx mnem net = do
  portfolio <- portfolioOf ctx mnem
  accountOf portfolio net kind

-- | The public side of the two test seeds
testSeed, testSeed2 :: Ctx -> PublicSeed
testSeed ctx = forceRight $ seedOf ctx testMnemonic
testSeed2 ctx = forceRight $ seedOf ctx testMnemonic2

-- | The test seed as if the child key of its account 1 were invalid. No
-- seed with such an index is known, the odds being one in 2^127.
seedWithoutIndex1 :: Ctx -> PublicSeed
seedWithoutIndex1 ctx =
  (testSeed ctx) {seedAccountKey = \kind net index -> if index == 1 then return Nothing else seedAccountKey (testSeed ctx) kind net index}

-- | The test wallet with the passphrase "café", its é composed as a keyboard
-- types it, which NFKD splits into e and an accent: the seed BIP39 derives,
-- and the one hw derived before it normalized the passphrase. For each, the
-- fingerprint of the seed and the first receiving address of its account 0
-- on Bitcoin, derived by haskoin-core alone and by the Python tool of the
-- message-signing vectors, which agree.
cafePassphrase :: Text
cafePassphrase = "caf\233"

cafeNormalizedFingerprint, cafeTypedFingerprint :: Text
cafeNormalizedFingerprint = "a3a04db1"
cafeTypedFingerprint = "aa280627"

cafeNormalizedAddr, cafeTypedAddr :: Address
cafeNormalizedAddr = testAddress "1MNgmi8rv8tie1QdPowAu1ywKiNAbL2RPj"
cafeTypedAddr = testAddress "1N24hDMGThGAPhKVrd7fNWdtE4uHsNf3UL"

-- | The first receiving address of accounts 1 and 2 of the test seed on
-- Bitcoin; that of account 0 is externalAddr 0
account1Addr, account2Addr :: Address
account1Addr = testAddress "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo"
account2Addr = testAddress "1CtzLtfkTFgv6tHN4zBHWHtrtCeBdD1KNt"

-- | The first receiving address, /0/0, of each of the first bip44 accounts
-- of the test seed on Bitcoin, as many as asked, from account 0 on. The
-- first three are externalAddr 0, account1Addr and account2Addr. They are
-- derived, since they are too many to write out: a store or a chain that
-- pays them has more paid accounts than a search finds before it asks
-- whether to go on.
firstAddrsOfAccounts :: Ctx -> Natural -> IO [Address]
firstAddrsOfAccounts ctx = firstAddrsBy ctx (seedAccountKey (testSeed ctx) Bip44 btc) (Just . keyAddress Bip44 ctx)

-- | The same for the bip49 accounts of the test seed, which hw does not
-- hold. The first two are wrappedExternalAddr 0 and wrappedAccount1Addr.
firstAddrsOfWrappedAccounts :: Ctx -> Natural -> IO [Address]
firstAddrsOfWrappedAccounts ctx = firstAddrsBy ctx (seedForeignKey (testSeed ctx) Bip49 btc) (foreignAddress Bip49 btc ctx)

-- | The address at /0/0 of each of the first accounts, as many as asked,
-- the key of an account and the address of a key given by the functions
firstAddrsBy :: Ctx -> (Natural -> IO (Maybe XPubKey)) -> (XPubKey -> Maybe Address) -> Natural -> IO [Address]
firstAddrsBy ctx keyOfAccount addressOf count =
  forM (genericTake count [0 ..]) $ \index -> do
    key <- fromMaybe (error "firstAddrsBy: no key at the index") <$> keyOfAccount index
    return $ fromMaybe (error "firstAddrsBy: no address for the key") (addressOf (derivePubPath ctx (Deriv :/ 0 :/ 0 :: SoftPath) key))

-- | Portfolio 0 of the test wallet in the database, whole, created when
-- missing
testPortfolio :: Ctx -> ExceptT String (DB IO) Portfolio
testPortfolio ctx = portfolioOf ctx testMnemonic

-- | What portfolio 0 of the test wallet holds on Bitcoin: its pool there
testPool :: Ctx -> ExceptT String (DB IO) Pool
testPool ctx = (`portfolioPool` btc) <$> testPortfolio ctx

-- | The BIP44 account of portfolio 0 of the test wallet on Bitcoin,
-- m/44'/0'/0', the portfolio created when missing
testNewAcc :: Ctx -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewAcc ctx = newAccountOf ctx testMnemonic btc

-- | The same account of the second test wallet
testNewAcc2 :: Ctx -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewAcc2 ctx = newAccountOf ctx testMnemonic2 btc

-- | The account of portfolio 0 of the test wallet on Bitcoin Cash,
-- m/44'/145'/0'
testNewAccBch :: Ctx -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewAccBch ctx = newAccountOf ctx testMnemonic bch

-- | The BIP84 account of portfolio 0 of the test wallet on Bitcoin,
-- m/84'/0'/0'
testNewSegwitAcc :: Ctx -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewSegwitAcc ctx = newAccountOfKind Bip84 ctx testMnemonic btc

{- Rows of the wallet -}

-- | The BIP44 account of portfolio 0 of the test wallet on Bitcoin as its
-- row holds it: m/44'/0'/0' of the test mnemonic, in the portfolio of key
-- 1, with 12 receiving and 3 change addresses, 0.05 btc confirmed and
-- 0.001 btc unconfirmed in 3 coins, created at 'seenTime'
mainAccountRow :: DBAccount
mainAccountRow = DBAccount (toSqlKey 1) "btc" 44 12 3 testAccountXPub 5000000 100000 3 seenTime

-- | The BIP84 account of the same portfolio on Bitcoin as its row holds
-- it: m/84'/0'/0' of the test mnemonic, with 2 receiving addresses handed
-- out, no change address and nothing received, created at 'seenTime'
segwitAccountRow :: DBAccount
segwitAccountRow = DBAccount (toSqlKey 1) "btc" 84 2 0 testSegwitAccountXPub 0 0 0 seenTime

-- | How the answers name portfolio 0 of wallet 0, and what it holds on
-- Bitcoin
mainPortfolioRef :: PortfolioRef
mainPortfolioRef = PortfolioRef 0 0

mainPlace :: Place
mainPlace = Place mainPortfolioRef btc

-- | How the answers name the two accounts of that place
mainAccountRef, segwitAccountRef :: AccountRef
mainAccountRef = AccountRef mainPlace Bip44
segwitAccountRef = AccountRef mainPlace Bip84

-- | Rows of the BIP44 account of that place, as a listing groups them: a
-- group of 'mainPlace' that covers the account of 'mainAccountRow', with
-- its 12 receiving and 3 change addresses
mainGroup :: [row] -> Group row
mainGroup = Group mainPlace [AccountCounts Bip44 12 3]

-- | Portfolio 0 of wallet 0 as portfolios shows it, with the two accounts
-- above on Bitcoin and no other: the active portfolio, described as
-- "savings", in a wallet
-- without a description whose fingerprint is that of the test mnemonic,
-- created at 'seenTime'. On Bitcoin it holds 0.05 btc confirmed and
-- 0.001 btc unconfirmed in 3 coins, can spend 0.049 btc now, and has 17
-- addresses, 12 + 3 of the BIP44 account and 2 of the BIP84 one. A
-- portfolio has every network: the view leaves the others out, as the
-- printers show only the networks a view gives them.
mainPortfolioView :: PortfolioView
mainPortfolioView =
  PortfolioView
    { viewRef = mainPortfolioRef,
      viewActive = True,
      viewDescription = "savings",
      viewWalletDescription = "",
      viewFingerprint = testFingerprint,
      viewCreated = seenTime,
      viewNetworks =
        [ NetworkView
            { networkViewNetwork = btc,
              networkViewConfirmed = 5000000,
              networkViewUnconfirmed = 100000,
              networkViewSpendable = 4900000,
              networkViewCoins = 3,
              networkViewAddresses = 17,
              networkViewAccounts = []
            }
        ]
    }

-- | The keys of the two accounts of portfolio 0 on btc, as portfolios
-- --verbose gives them. The checksums of the descriptors were computed
-- apart from hw, and Bitcoin Core 32.1 gives the same
-- (test/data/descriptors). The SLIP-132 key of a bip44 account is its
-- xpub, and that of a bip84 account its zpub.
legacyKeys, segwitKeys :: AccountKeys
legacyKeys = AccountKeys Bip44 (Deriv :| 44 :| 0 :| 0) testAccountXPub ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j87") (Just testAccountXPub)
segwitKeys = AccountKeys Bip84 (Deriv :| 84 :| 0 :| 0) testSegwitAccountXPub ("wpkh([bc560f95/84h/0h/0h]" <> testSegwitAccountXPub <> "/<0;1>/*)#5nktextl") (Just testSegwitAccountZPub)

-- | The first receiving address of that account as its row holds it,
-- labelled "Invoice 42": paid 50000 in one transaction, which it still
-- holds in one coin, created at 'seenTime'
paidAddressRow :: DBAddress
paidAddressRow = DBAddress (toSqlKey 1) False 0 (externalAddrsText !! 0) "Invoice 42" 50000 0 1 1 50000 seenTime

-- | Its second receiving address, which nothing has paid
unusedAddressRow :: DBAddress
unusedAddressRow = DBAddress (toSqlKey 1) False 1 (externalAddrsText !! 1) "" 0 0 0 0 0 seenTime

-- | The coin of 50000 at the first receiving address, /0/0, as coins
-- lists it: in block 800000, 3 confirmations deep, free and spendable,
-- paid by a transaction the store saw at 'seenTime'
spendableCoinRow :: JsonCoin
spendableCoinRow = JsonCoin (OutPoint (fakeTxHash 1) 0) (externalAddr 0) 50000 (Store.BlockRef 800000 1) 3 Nothing True "" "" False (Just (legacyPath (Deriv :/ 0 :/ 0))) (Just seenTime)

-- | A coin of the store as coins lists it, at the path given: 3
-- confirmations deep, free and spendable, without labels. Its time is
-- unknown, so that the clock of the computer plays no part in its views.
coinViewAt :: PoolPath -> Store.Unspent -> JsonCoin
coinViewAt path coin =
  JsonCoin coin.outpoint (fromMaybe (error "coinViewAt: a coin without an address") coin.address) coin.value coin.block 3 Nothing True "" "" False (Just path) Nothing

-- | A sync of portfolio 0 on Bitcoin that found nothing new, at block
-- 800002: it holds 0.05 btc confirmed and 0.001 btc unconfirmed in 3
-- coins, and can spend 0.049 btc now
quietSync :: SyncRes
quietSync = SyncRes mainPlace 5000000 100000 3 4900000 (fakeBlockHash 2) 800002 0 0 0 0 0

{- Transaction summaries -}

-- The story of these summaries is that of TxInfoSpec: a payment of 50000
-- received at the first receiving address of the test account, then spent:
-- 40000 to an outside address, 9000 of change to its first change
-- address, and 1000 of fee.

-- | What the payment means to the account: 226 bytes, so 4 satoshis per
-- byte of fee
paymentSummary :: TxSummary
paymentSummary =
  TxSummary
    { summaryType = TxCredit,
      summaryAmount = 50000,
      summaryFee = 1000,
      summaryFeeByte = 4,
      summaryChangeInFee = Nothing,
      summarySize = 226,
      summaryLockTime = 0,
      summaryMyOutputs = Map.fromList [(externalAddr 0, MyOutputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "")],
      summaryOtherOutputs = Map.empty,
      summaryNonStdOutputs = [],
      summaryMyInputs = Map.empty,
      summaryOtherInputs = Map.fromList [(otherAddr 0, 51000)],
      summaryNonStdInputs = []
    }

-- | What spending it means to the account: the 40000 and the fee leave
spendSummary :: TxSummary
spendSummary =
  TxSummary
    { summaryType = TxDebit,
      summaryAmount = -41000,
      summaryFee = 1000,
      summaryFeeByte = 4,
      summaryChangeInFee = Nothing,
      summarySize = 226,
      summaryLockTime = 0,
      summaryMyOutputs = Map.fromList [(internalAddr 0, MyOutputs 9000 (legacyPath (Deriv :/ 1 :/ 0)) "")],
      summaryOtherOutputs = Map.fromList [(otherAddr 0, 40000)],
      summaryNonStdOutputs = [],
      summaryMyInputs = Map.fromList [(externalAddr 0, MyInputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "" [OutPoint (fakeTxHash 1) 0])],
      summaryOtherInputs = Map.empty,
      summaryNonStdInputs = []
    }

-- | The summaries once the receiving address is labelled "Rent"
paymentToRent, spendFromRent :: TxSummary
paymentToRent = paymentSummary {summaryMyOutputs = Map.fromList [(externalAddr 0, MyOutputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "Rent")]}
spendFromRent = spendSummary {summaryMyInputs = Map.fromList [(externalAddr 0, MyInputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "Rent" [OutPoint (fakeTxHash 1) 0])]}

-- | The state of a transaction in preparation: its nosigHash, and whether
-- it is signed, sent and in conflict. Nothing for a transaction on the chain.
pendingFlags :: TxInfo -> Maybe (TxHash, Bool, Bool, Bool)
pendingFlags info =
  case txInfoStatus info of
    Pending p -> Just (pendingNosigHash p, pendingSigned p, pendingOnline p, pendingConflicted p)
    OnChain _ -> Nothing

-- | The chain status of a listed transaction, which is always on the chain
chainOf :: TxInfo -> ChainStatus
chainOf info =
  case txInfoStatus info of
    OnChain status -> status
    Pending _ -> error "A listed transaction is on the chain"

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

-- | The vector files of a directory hold the vectors expected, file by
-- file, as counted apart from hw. A file deleted, or a list of vectors
-- renamed, which the reader takes for an empty one, fails this test
-- rather than passing over its vectors.
vectorCountsSpec :: (a -> Int) -> [(FilePath, Int)] -> [(FilePath, Either String a)] -> Spec
vectorCountsSpec count expected files =
  it ("holds " <> show (sum (snd <$> expected)) <> " vectors in " <> show (length expected) <> " files") $
    [(file, either (const 0) count doc) | (file, doc) <- files] `shouldBe` expected

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
      accKey <- accountSigningKey net ctx (AccountSpec Bip44 mnem pass acc)
      let path = Deriv :/ fromIntegral chain :/ fromIntegral idx :: SoftPath
      return (derivePath ctx path accKey).key

-- | An account of a BIP-39 mnemonic, derived like hw does: the account key is
-- m/44'/coin'/account', or m/84'/coin'/account' for a bip84 account. In
-- JSON: {"mnemonic", "passphrase", "account", "type"}, where the passphrase
-- is optional and the type is bip44 unless given.
data AccountSpec = AccountSpec !AccountKind !Text !Text !Natural

instance FromJSON AccountSpec where
  parseJSON =
    withObject "AccountSpec" $ \o ->
      AccountSpec
        <$> (kindOf =<< o .:? "type" .!= "bip44")
        <*> o .: "mnemonic"
        <*> o .:? "passphrase" .!= ""
        <*> o .: "account"
    where
      kindOf name = maybe (fail ("Unknown account type " <> Text.unpack name)) return (kindNamed name)

-- | The kind of the account of a specification
accountSpecKind :: AccountSpec -> AccountKind
accountSpecKind (AccountSpec kind _ _ _) = kind

-- | The account private key of an account specification
accountSigningKey :: Network -> Ctx -> AccountSpec -> Either String XPrvKey
accountSigningKey net ctx (AccountSpec kind mnem pass acc) =
  signingKey kind net ctx (MnemonicPass mnem pass) acc

{- Synthetic transactions and coins -}

-- The transactions of the tests exist on no blockchain: a funding transaction
-- spends a fake outpoint and pays the addresses a test needs, and the test
-- then spends the coins of that funding transaction.

-- | A transaction id as the user types it, in hexadecimal
typedTxid :: Text -> TxHash
typedTxid = fromMaybe (error "typedTxid") . hexToTxHash

-- | A short id as the user types it: 8 hexadecimal digits
typedShortId :: Text -> ShortId
typedShortId = fromMaybe (error "typedShortId") . readShortId

-- | Two txids that share the short id a1075db5, their first 8 digits, and
-- one of the short id b2d4f6a8. The ids of 'fakeTxHash' all start with
-- 00000000, so tests that tell two short ids apart take these.
a1075db5First, a1075db5Second, b2d4f6a8Txid :: TxHash
a1075db5First = typedTxid "a1075db5c0ffee00000000000000000000000000000000000000000000000001"
a1075db5Second = typedTxid "a1075db5c0ffee00000000000000000000000000000000000000000000000002"
b2d4f6a8Txid = typedTxid "b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001"

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

-- | A transaction as hw builds it on Bitcoin, spending the outpoints and
-- paying the addresses: as 'spendingTx', in version 2, with the lock time
-- given and every input marked replaceable by the sequence 0xfffffffd of
-- BIP 125 (see "Haskoin.Wallet.Tx.LockTime")
builtTx :: Ctx -> Word32 -> [(TxHash, Word32)] -> [(Address, Natural)] -> Tx
builtTx ctx lockTime spent paid =
  Tx 2 [TxIn (OutPoint h i) BS.empty 0xfffffffd | (h, i) <- spent] (payment <$> paid) [] lockTime
  where
    payment (addr, amount) = TxOut (fromIntegral amount) (addressScript ctx addr)

-- | A transaction funded by a fake outpoint, paying the given addresses
fundingTx :: Ctx -> Word8 -> [(Address, Natural)] -> Tx
fundingTx ctx w = spendingTx ctx [(fakeTxHash w, 0)]

-- | A pending transaction of a pool as its row holds it: unsigned,
-- without derivation paths or label, holding the coins its inputs spend
pendingRow :: Pool -> Tx -> DBPendingTx
pendingRow pool tx =
  DBPendingTx
    { dBPendingTxPortfolio = pool.poolPortfolio,
      dBPendingTxNetwork = poolNetworkName pool,
      dBPendingTxNosigHash = hashBytes (nosigHash tx),
      dBPendingTxBlob = BS.toStrict $ encode $ TxSignData tx [] [] [] Nothing Nothing [] noLabel,
      dBPendingTxOnline = False,
      dBPendingTxConflicted = False,
      dBPendingTxCreated = fixtureTime
    }

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

-- | All the coins of a transaction, unconfirmed, as the store reports them.
-- The address of each is read as on Bitcoin, which has every kind of
-- address the tests pay.
coinsOf :: Ctx -> Tx -> [Store.Unspent]
coinsOf ctx tx =
  [ Store.Unspent
      { Store.block = Store.MemRef 0,
        Store.outpoint = OutPoint (txHash tx) i,
        Store.value = out.value,
        Store.script = out.script,
        Store.address = either (const Nothing) Just (scriptToAddressBS btc ctx out.script)
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

-- | The transaction funding coins worth the given amounts, on the receiving
-- addresses /0/0, /0/1, ... of the BIP84 account of the test wallet: what
-- 'fundingWorth' is for its BIP44 account
segwitFundingWorth :: Ctx -> [Natural] -> Tx
segwitFundingWorth ctx values = fundingTx ctx 2 $ zip (segwitExternalAddr <$> [0 ..]) values

-- | The coins of 'segwitFundingWorth', as the store reports them: P2WPKH
-- coins, where those of 'coinsWorth' are P2PKH
segwitCoinsWorth :: Ctx -> [Natural] -> [Store.Unspent]
segwitCoinsWorth ctx = coinsOf ctx . segwitFundingWorth ctx

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

-- | The fingerprint of the seed of the test wallet: that of its master key
testFingerprintText :: Text
testFingerprintText = "bc560f95"

testFingerprint :: Fingerprint
testFingerprint = forceRight $ textToFingerprint testFingerprintText

-- | The keys of account 0 of the test wallet, m/44'/0'/0'
testAccountXPrv, testAccountXPub :: Text
testAccountXPrv = "xprv9yHxeaLAZvxXb9VtJNesqk8avfN8misGAW9DUW9eacZJNqsfZxqKLmK5jfmvFideQqGesviJeagzSQYCuQySjgvt7TdfowKja5aJqbgyuNh"
testAccountXPub = "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah"

-- | The public key of account 1 of the test wallet, m/44'/0'/1': the
-- next account of the seed
testAccountXPub1 :: Text
testAccountXPub1 = "xpub6CHK45s4QJWpt9C5kpAnypsE5ratNMWRGE3pZNQgGnxHTvZ1y6E65mSKMmz7V5ZUnVq5JMgq1Qros98GhLMCK7eoDaNo8kjqNdJzD8otqLW"

-- | The public key of the BIP84 account 1 of the test wallet, m/84'/0'/1':
-- the bip84 account of its portfolio 1 on Bitcoin. It was derived apart
-- from hw, with the Python tool of the message-signing vectors and a
-- serialization of BIP32 written for it, which gave back the keys of
-- portfolio 0 and 'testAccountXPub1' first.
testSegwitAccountXPub1 :: Text
testSegwitAccountXPub1 = "xpub6CQF8An9YzRXqxfmHLF5ffeqd5vqfyHz9swNS5zjqMYLi3pwH43TAnCq8fERvv3qMUUsTwBEaT6jXudbsdaZZps2CGhucwM61PoTFpBvDX5"

-- | The keys of the BIP84 account 0 of the test wallet, m/84'/0'/0', and
-- the first ten addresses of its two chains. The public key and the first
-- address are those the Coldcard file of test/data/descriptors gives for
-- the seed; all of them were derived apart from hw with the Python tool
-- of the message-signing vectors.
testSegwitAccountXPrv, testSegwitAccountXPub :: Text
testSegwitAccountXPrv = "xprv9yQtifFFicsEcWj4uDMZ1Y3aRXTSf6cT2kqnW1fkTxNkDLiTBFnqSFMDXC3Jy1XFREhqmya5GnmuSMYVbmos2HDchLqJGZiTJtdKjPp6qDT"
testSegwitAccountXPub = "xpub6CQF8An9YzRXpzoY1EtZNfzJyZHw4ZLJPymPJQ5N2Huj693bio75z3fhNUq6unam47GaZQanRezUbkhMZosNyZBrGT9NExM1fa8uwdaWnoV"

-- | The public key of the BIP84 account with the zpub prefix of SLIP-132,
-- as Electrum and a Coldcard write it: the same key, its four version
-- bytes swapped, computed apart from hw
testSegwitAccountZPub :: Text
testSegwitAccountZPub = "zpub6r4mjW7yrMWVXbBmfxTonrBKKVapwoKJECopsBs8nJfVCLg4E7SDEAyyQtkGubtbsPWC4MmuLyhaNKvV1ChQa2Z418YDQmyzD2GCiogfbAh"

testSegwitAccountKey :: XPrvKey
testSegwitAccountKey = fromMaybe (error "testSegwitAccountKey") $ xPrvImport btc testSegwitAccountXPrv

testSegwitAccountPub :: Ctx -> XPubKey
testSegwitAccountPub ctx = fromMaybe (error "testSegwitAccountPub") $ xPubImport btc ctx testSegwitAccountXPub

-- | The receiving addresses /0/0 to /0/9 of the BIP84 account
segwitExternalAddrsText :: [Text]
segwitExternalAddrsText =
  [ "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc",
    "bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2",
    "bc1qrtmkfqwtavl80tm65mpwrrzt7pamm0syaageg2",
    "bc1q30arh9afqsymrypvya7lr68305lgv0cmfwr48w",
    "bc1q74rr85p6g48nxuzsh9cc6q0m4jqfuyg0wxjaar",
    "bc1qg83sxnghnw6du9rn2yx0a74q300y2l22zmsjm0",
    "bc1qvu4fzlulavum5ttxml7t2rdgnd6e4l9ls6ep9j",
    "bc1qj5e9ycrm73a38wen2t2a625amd4gek2f7v6xr6",
    "bc1q886qqrsms0ggsez6elckxfn4mpttyh3x9pgqqc",
    "bc1qhrdz0l0tzrcdeltuavsu8kn5tjj29m0m7wgdm4"
  ]

-- | The change addresses /1/0 to /1/9 of the BIP84 account
segwitInternalAddrsText :: [Text]
segwitInternalAddrsText =
  [ "bc1ql5j70a05srlvee3ugweyqukdfwwaph4pwaup74",
    "bc1q2uprg958790vg0w8k5zjkntjh46mf3yc7vc782",
    "bc1qnx0y3ap8f90px8ykvzt6vhzjpfcadnuvns3qpp",
    "bc1qm2l6zjex86aea8cepuhghwnk0sdly2lvzz23uf",
    "bc1quyj4p259snvcujp0a5ntzeyzzs4p6yzmae325h",
    "bc1qs8d4wdld3rvcaz4ynr263wzl40c6u0awla8x6f",
    "bc1q90u76d4knj3mgp3mc07lwryn0r9jaf73ytfzzp",
    "bc1qd205vvtzfkty6e6jsv0f3ul88mm6n7pm7kgjvc",
    "bc1qknv3fy496fhy5nrs95jch9d2rq5a5dwx8qmsf4",
    "bc1qetesxsjf99fje2q696gkl7xam5cv0lran49a8k"
  ]

-- | A path of the BIP84 account of a pool, as 'legacyPath' is one of its
-- BIP44 account
segwitPath :: SoftPath -> PoolPath
segwitPath = PoolPath Bip84

-- | The receiving address /0/i and the change address /1/i of the BIP84
-- account
segwitExternalAddr, segwitInternalAddr :: Int -> Address
segwitExternalAddr i = testAddress (segwitExternalAddrsText !! i)
segwitInternalAddr i = testAddress (segwitInternalAddrsText !! i)

-- | The key of the BIP84 account 0 of the test wallet on the test networks,
-- m/84'/1'/0', the same text on each of them
testSegwitAccountXPubTest :: Text
testSegwitAccountXPubTest = "tpubDDRUQfT52zY83AN2e4818hHNGVa4EgPmXHDUKFn6ba4fcBhjfq9dcSv5M44a2bGWsy6Y9JYPyBPPFf8qsFb2tdpL9LJNbk9epnUyF9Yobtg"

-- | The same key with the vpub prefix of SLIP-132, computed apart from hw
testSegwitAccountVPub :: Text
testSegwitAccountVPub = "vpub5ZPKH6HZ2MVi4mnwX9ZfWbkzbSBWN3ui9RajPuwBVeZ1khQa6HyRDsvy4c4cWH1SGnL22mqQWkrEnuR72zumQizqM4ADkPrShNmGi1jtzXq"

-- | The accounts of the test wallet of the kinds hw does not hold, which
-- recover looks for: the key of the BIP49 account 0, m/49'/0'/0', and
-- the first five addresses of its two chains, P2SH-P2WPKH; the first
-- receiving address of the BIP49 account 1; and the same of the BIP86
-- account 0, m/86'/0'/0', whose addresses are P2TR. All derived apart
-- from hw with the Python tool of the message-signing vectors, whose
-- derivation was checked against the vectors of BIP 49, BIP 84 and
-- BIP 86 first.
testWrappedAccountXPub :: Text
testWrappedAccountXPub = "xpub6DSJPn1DECFaHqjX5bfs9oKY4vR2xWHYgD9FyuKvhwdXUe9uxxzddLUMvQjJSBiXHM5YXPVD1mqq9ReUUC7BcwhXhMR4XA3T4Rmn31uhQm3"

testWrappedAccountPub :: Ctx -> XPubKey
testWrappedAccountPub ctx = fromMaybe (error "testWrappedAccountPub") $ xPubImport btc ctx testWrappedAccountXPub

wrappedExternalAddrsText :: [Text]
wrappedExternalAddrsText =
  [ "3F2meb7rc8H1iij4Z4h7ZW4skSxFvyX6Ks",
    "3Gj5qxN37hKXjuHjY6PtjvQXVkRj8Q2hQo",
    "3GCbzjo6c6BhwJpCg7mhjkVLoTscid2SRz",
    "3JYW348M8gnbw53gBvPjdsrMP95qi6ehfF",
    "3BpdjM8ZH31QTfy3eCFmyX8DRLRbJ2noHV"
  ]

wrappedInternalAddrsText :: [Text]
wrappedInternalAddrsText =
  [ "3PtUnPBhySzjMG996ZRgeHebs5PCd7qEHJ",
    "32Dv272CRJhd2aTL2RZbXnoaMMd7uiaGy4",
    "32Z1RJE2EezPo7ujVR2maYeM9tw4vJoZQT",
    "3G24PqZkVabv23Qhc3MYikT4d1h8cU1Wux",
    "34CLC4QRPL8fGb4whN3xn8Mh5f9qqkoXbp"
  ]

-- | The receiving address /0/i and the change address /1/i of the BIP49
-- account 0
wrappedExternalAddr, wrappedInternalAddr :: Int -> Address
wrappedExternalAddr i = testAddress (wrappedExternalAddrsText !! i)
wrappedInternalAddr i = testAddress (wrappedInternalAddrsText !! i)

-- | The first receiving address of the BIP49 account 1, m/49'/0'/1'/0/0
wrappedAccount1Addr :: Address
wrappedAccount1Addr = testAddress "3FQUvWTfeMUcqxfNTxGqL2aJsNBadyhd3Q"

testTaprootAccountXPub :: Text
testTaprootAccountXPub = "xpub6C2p6JewFDCzaAxWktknQQxjHuqWgsXkH1ikanD6jrjuUWKyjoifhNSMj1tYaMfsKW7YZzMu5Pzw2t9rYGHi5jf1DrSNwpu6YYvrj9ZmG1C"

testTaprootAccountPub :: Ctx -> XPubKey
testTaprootAccountPub ctx = fromMaybe (error "testTaprootAccountPub") $ xPubImport btc ctx testTaprootAccountXPub

taprootExternalAddrsText :: [Text]
taprootExternalAddrsText =
  [ "bc1p8zl0j9e73592k9n6ftz4keyq24dq8djqd3rkcrepu2fw6zeryyjs8zz3ev",
    "bc1p8xcqvkxy92yp73jdyx2np4gc9n5pua72mfy6tpj00q7vqwhca25qk0dy72",
    "bc1pykh5mycshgvcg3lq94yfjvgtaf0g3e76x4yqasukxkh6j9a3pcssycy0af",
    "bc1pfw0wclq6cj8plp0e0glkgdmdal27txhdc3lrsat5cfdyq6mj7unq33j4yh",
    "bc1ptu6lg68933utqcazhztenpsxnqf3d2pts70u9fvhqgt7c89avzlsjt3fwl"
  ]

taprootInternalAddrsText :: [Text]
taprootInternalAddrsText =
  [ "bc1pwgkx9jl5zns4twes524kls50zlxc45k4hcu9sxhga8g28sr9cchszuaxef",
    "bc1pasgt6r0jwcm3qltj9hnehpj3u32hhf2593xq7efm4eva6ycen7aspcpj6r",
    "bc1pak9cr67qx6549ftakl59jj8dcsx7gdeeqf6w4kgtp3jqs58e84lqytlvyh",
    "bc1pu0nxupen84t3aqwhfqxtj9yuta8x7r6ch7uvq06lzfe4vyxpzcaqktlza0",
    "bc1p9h27gzw2aptrh8fdvgrcj6rwt8jjvntw9xwcnj7rzc9pls9lfalslhutnf"
  ]

-- | The receiving address /0/i and the change address /1/i of the BIP86
-- account 0
taprootExternalAddr, taprootInternalAddr :: Int -> Address
taprootExternalAddr i = testAddress (taprootExternalAddrsText !! i)
taprootInternalAddr i = testAddress (taprootInternalAddrsText !! i)

testAccountKey :: XPrvKey
testAccountKey = fromMaybe (error "testAccountKey") $ xPrvImport btc testAccountXPrv

-- | The signing data signed with the account keys of the test wallet, for
-- the fixtures that need a transaction with real signatures
signedByWallet :: Ctx -> TxSignData -> TxSignData
signedByWallet ctx tsd = fst $ forceRight $ signWalletTx btc ctx tsd testPoolPrvKeys

testAccountPub :: Ctx -> XPubKey
testAccountPub ctx = fromMaybe (error "testAccountPub") $ xPubImport btc ctx testAccountXPub

-- | The private keys of the two accounts of portfolio 0 of the test wallet
-- on Bitcoin, each with its kind, as the signer takes them: m/44'/0'/0'
-- and m/84'/0'/0'
testPoolPrvKeys :: [(AccountKind, XPrvKey)]
testPoolPrvKeys = [(Bip44, testAccountKey), (Bip84, testSegwitAccountKey)]

-- | Their public keys, as a transaction is read against them
testPoolPubKeys :: Ctx -> [(AccountKind, XPubKey)]
testPoolPubKeys ctx = [(Bip44, testAccountPub ctx), (Bip84, testSegwitAccountPub ctx)]

-- | The QR code of the first receiving address of the test wallet, as a
-- terminal draws it. It holds bitcoin:1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj.
-- Two decoders read this drawing, zxing-cpp and OpenCV, and segno encodes
-- the same modules with the same mask.
p2pkhAddressCode :: [Text]
p2pkhAddressCode =
  [ "                                     ",
    "                                     ",
    "    █▀▀▀▀▀█  ▀▄ ▄ ▀█▀█▄ █ █▀▀▀▀▀█    ",
    "    █ ███ █ █▄▄▄█ ▄ ▀▄▀ ▀ █ ███ █    ",
    "    █ ▀▀▀ █ █▀ ▄ ▀▄▄▀▀▄▀▀ █ ▀▀▀ █    ",
    "    ▀▀▀▀▀▀▀ █▄▀ █ █▄█ ▀ █ ▀▀▀▀▀▀▀    ",
    "    ▀ ██▀█▀  ▄▄  █▀▄▄▀▀▀▄ █▀█▀▀▄▄    ",
    "     ▄▀   ▀▄▄▄▄   █▀   ▄▀ ▄▄ █▀▄     ",
    "    █ █▄▀▄▀ █ █ ▀▀██▄█▀ ▄▄▄▄▄▄█ ▄    ",
    "    ▀▀▀█▀▀▀▀▄█ ▄█▀ ▀  ▀▄▀█████▀      ",
    "    █ ▄▀ █▀ █▄▀▀ ▄▀▄▄▀▄▀▄▄ ▄█ ███    ",
    "    █ ██ ▀▀█ ▄█▄▄ ▀▀  ▄▄█  ▀ █ ▄▄    ",
    "    ▀ ▀▀▀ ▀▀█ ▀▄ █▀███▄▄█▀▀▀█▄██▄    ",
    "    █▀▀▀▀▀█ ▄▀▄▄▄▀ █▀▀▀▀█ ▀ █ ▀▄▄    ",
    "    █ ███ █ ██▄█▄  ▀█▀▄▄▀▀▀▀█▀███    ",
    "    █ ▀▀▀ █ ▀▄▄▀▄▄ ██ ▀ ▄▄▄██ ▀█     ",
    "    ▀▀▀▀▀▀▀ ▀     ▀▀▀▀▀▀ ▀  ▀▀▀      ",
    "                                     ",
    "                                     "
  ]

-- | The QR code of the first receiving address of the BIP84 test account.
-- It holds BITCOIN:BC1QYNNYWXCVS9DWUQK8H53XZN9KPEYPRGQEACXWTC, in the
-- alphanumeric mode, and was checked as 'p2pkhAddressCode' was.
p2wpkhAddressCode :: [Text]
p2wpkhAddressCode =
  [ "                                     ",
    "                                     ",
    "    █▀▀▀▀▀█ ▀▄█▄ ▀▀█ █▀   █▀▀▀▀▀█    ",
    "    █ ███ █ ▀▀▀▀█▀ ▄ ▄▄▀▄ █ ███ █    ",
    "    █ ▀▀▀ █ ▄█ █▄▄▄▀▀█▀▀  █ ▀▀▀ █    ",
    "    ▀▀▀▀▀▀▀ ▀ █▄▀▄▀ ▀ █ ▀ ▀▀▀▀▀▀▀    ",
    "    ▀▄█▄▄▄▀▀ █▀█▄▀ ▀▄▀▄ ▀▄ ▀  ▀▄▀    ",
    "    █▄▄  ▀▀  ▄███▀██▀ █▀█ ▀██▀▄██    ",
    "    ▀▄▀▀ ▄▀▀ ▀██▀██▀▄██ ▀▄█▄█▀ ▀     ",
    "     ▄█ ▀█▀ ▀▀ ▄ ▄ ▄██   ▄██  █ █    ",
    "    ▀█ ▄▀ ▀▀▀ ▀▀▀▀ █▄ ▄█▄██ ▀ ▄█     ",
    "    ▀▀█▄▄█▀▄█▀▄█▄▀█▄▀█▀▀▀▄ █ ▄█▀▀    ",
    "    ▀▀▀▀  ▀▀█ ▄▀▀██▄ ██▀█▀▀▀█ ▄▀█    ",
    "    █▀▀▀▀▀█ ▀█▄█ ▄ █▄█ ▄█ ▀ █  █▀    ",
    "    █ ███ █  ▄▀██▀ ▀▀█ █▀█▀▀▀▀██     ",
    "    █ ▀▀▀ █ ▀▀▀█▀  ▀█▄▄   ▀ ▀██▀█    ",
    "    ▀▀▀▀▀▀▀ ▀ ▀ ▀ ▀▀ ▀▀ ▀  ▀   ▀▀    ",
    "                                     ",
    "                                     "
  ]

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

-- | A SegWit address, which Bitcoin writes and Bitcoin Cash cannot: the
-- P2WPKH example of BIP 173
segwitAddr :: Address
segwitAddr = testAddress "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"

-- | The key of the first account of the test wallet at /2/7, and its
-- P2PKH address: a path below the account where it has no address, since
-- its addresses are at /0/n and /1/n. A crafted transaction file could
-- pay funds of the account there, where the wallet would never see them.
-- Both were derived with haskoin-core, apart from hw's code.
keyAt27 :: Ctx -> PublicKey
keyAt27 ctx = forceRight (unmarshal ctx (hexBytes "0355d0e6bcc6e5ebd257b97ea27942a1676b3ac61dedb4e2ce20adb8b84afd4160"))

addrAt27 :: Address
addrAt27 = testAddress "1K7GH8Bg5VrS8boE37csWFPKjbvG9GCiaK"

-- | A second wallet, with its fingerprint and the keys of its account 0
testMnemonic2 :: MnemonicPass
testMnemonic2 =
  MnemonicPass
    "boring auction demand filter frog accuse company exchange rely slogan trim typical"
    "correct horse battery staple"

testFingerprintText2 :: Text
testFingerprintText2 = "4435aa1c"

testFingerprint2 :: Fingerprint
testFingerprint2 = forceRight $ textToFingerprint testFingerprintText2

testAccountXPrv2, testAccountXPub2 :: Text
testAccountXPrv2 = "xprv9yXnZpEVdonEtT3strknsAgso5qq1cwRooo6susmzVmB5E2vvNw1KKRBgvwvNLxXdBHnkEN5R5uXi2QDs3tpkoBbL61NE6bnSbcrvH6keGa"
testAccountXPub2 = "xpub6CX8yKmPUBLY6w8LztHoEJdcM7gKR5fHB2ihgJHPYqJ9x2N5TvFFs7jfYCX6So9oYyu6eLDTG5dbQWPncv1PYJtXLJ4cwymhoCpeTEmnZFZ"

-- | The first external address of account 0 of the second wallet, /0/0
externalAddr2Text :: Text
externalAddr2Text = "1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2"

-- | The signature of the message "hello" by the address /0/0 of the test
-- wallet, as signmsg gives it, checked apart from hw with the pure-Python
-- verifier of the message-signing vectors
-- (test/data/message-signing/tools/btc.py): it recovers the key of the
-- address from it
helloSignature :: Text
helloSignature = "IJ6CSrFWDRjgP/CfS+0UTM6ziJ2pil12tGj0xLBzPTEGeN2FipE7Y/tWxGnLliGzRObBxkgxKXQEZEgCmoeWeMo="

externalAddr2 :: Address
externalAddr2 = testAddress externalAddr2Text

-- | The test wallet on Bitcoin Cash: the public key of its account 0,
-- m/44'/145'/0', and the first external address of that account
testAccountXPubBch :: Text
testAccountXPubBch = "xpub6DXWb36CzdZvWbtSrYQkbxtapXciSuuQYJhtg4ysKAcppi5bjxxHPBNgZRyabXmF1WLotb4wVoqzGB7beYL1SLoHTd4xPCQnFRkXXt2NXTB"

-- | The key of account 0 of the test wallet on the test networks: Bitcoin
-- testnet, Bitcoin Cash testnet and the regtests all use coin type 1 and
-- the tpub prefix, so the text is the same on each of them
testAccountXPubTest :: Text
testAccountXPubTest = "tpubDCEeDpHvZZ49SMZ4VkifyjwqyJjKVFkbJHzhxZJiQgcMa2vnQnMhyXcu1ZZcReuwrprnxVnJsJGro7xdrEABAtK4AGZGqv2r4H7WytWLbPm"

-- | The first receiving address of the bip44 account of portfolio 0 of the
-- test wallet on a test network of Bitcoin, m/44'/1'/0'/0/0. The text is
-- the same on btctest, btctest4 and btcreg: they share the coin type 1,
-- so the key, and the form of their P2PKH addresses. Derived apart from
-- hw with the Python tool of the message-signing vectors.
testnetExternalAddrText :: Text
testnetExternalAddrText = "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"

testnetExternalAddr :: Address
testnetExternalAddr = fromMaybe (error "testnetExternalAddr") (textToAddr btcTest testnetExternalAddrText)

bchExternalAddrText :: Text
bchExternalAddrText = "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"

bchExternalAddr :: Address
bchExternalAddr = fromMaybe (error "bchExternalAddr") (textToAddr bch bchExternalAddrText)

-- | A P2PKH address of Bitcoin Cash testnet, whose prefix bchtest is
-- shorter than bitcoincash: the receiving address /0/0 of m/44'/1'/0' of
-- the seed "abandon ... about" with the passphrase TREZOR, from the
-- vectors of test/data/message-signing
bchTestAddr :: Address
bchTestAddr = fromMaybe (error "bchTestAddr") (textToAddr bchTest "bchtest:qztv0mmm3yscxcvfr93hq3wn63xk50merq7wl5gp3m")

{- Who watches a sync -}

-- | A watcher with the answers given, and no status line
answeringWith :: ScanQuestion m -> Watcher m
answeringWith answer = Watcher answer noStatus

-- | The answers to the question of a scan that reaches its limit: none
-- expected, which fails the scan, or yes, or no
noQuestion, keepLooking, stopLooking :: (Monad m) => Watcher m
noQuestion = answeringWith $ \found -> throwError ("The scan asked a question: " <> found)
keepLooking = answeringWith $ \_ -> return True
stopLooking = answeringWith $ \_ -> return False

-- | A status that notes each step with what it is of, in the order told
notingSteps :: IORef [(Text, Step)] -> Status
notingSteps steps name step = modifyIORef steps (<> [(name, step)])

{- The scripted store -}

-- A store the sync and the commands can be played against without a
-- network, filled with the answers it gives

-- | What the store answers: every list is complete, as the real store's
-- are when asked for everything
data FakeStore = FakeStore
  { fakeBest :: Store.BlockData,
    -- | The blocks at each height, for the reorganisation check
    fakeBlocks :: [Store.BlockData],
    -- | The latest blocks, newest first, for the fee rates
    fakeLatestBlocks :: [Store.BlockData],
    -- | The transactions of the latest blocks, by block hash
    fakeBlockTxs :: [(BlockHash, [Store.Transaction])],
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
      storeLatestBlocks = return fake.fakeLatestBlocks,
      storeBlockTransactions = \hash -> return $ fromMaybe [] (lookup hash fake.fakeBlockTxs),
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
      storePublish = \_ -> return (),
      storeFailed = \_ -> return ()
    }

-- | A store with nothing in it: no transactions, no coins, no balances
emptyStore :: FakeStore
emptyStore =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
      fakeBalances = [],
      fakeTxRefs = [],
      fakeTransactions = [],
      fakeUnspents = [],
      fakeRawTransactions = []
    }

-- | The store, failing its health check: it is there, and down
unhealthy :: Store -> Store
unhealthy store = store {storeHealth = throwError (Unhealthy "The store is down")}

-- | The store of a network the host does not serve: its health check says
-- so, which is no failure
notServed :: Store -> Store
notServed store = store {storeHealth = throwError (NotServed "The host does not serve the network")}

-- | The store at a host hw could not reach: its health check says so, as
-- the check of every other network at the host would
unreachableHost :: Store -> Store
unreachableHost store = store {storeHealth = throwError (Unreachable "The store at https://api.haskoin.com/ could not be reached: the connection timed out")}

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

-- | The store, noting every list of transactions it is asked for
notingTxRequests :: IORef [[TxHash]] -> Store -> Store
notingTxRequests asked store =
  store
    { storeTransactions = \tids -> do
        liftIO $ modifyIORef asked (<> [tids])
        storeTransactions store tids
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
blockAt height nonce = blockMinedAt height nonce 1700000000 0

-- | A block of the main chain, whose hash depends on the nonce, mined at
-- the Unix time, holding transactions paying the fees in all
blockMinedAt :: BlockHeight -> Word32 -> Word32 -> Word64 -> Store.BlockData
blockMinedAt height nonce time fees =
  Store.BlockData
    height
    True
    0
    (BlockHeader 1 (fakeBlockHash 0) (fakeTxHash 0).get time 0x1d00ffff nonce)
    1000
    4000
    []
    0
    fees
    0

-- | The balance of an address paid once, holding the amount in one
-- confirmed coin
paidOnce :: Address -> Word64 -> Store.Balance
paidOnce addr amount = Store.Balance addr amount 0 1 1 amount

-- | The balance of an address paid once, by a transaction still in the
-- mempool
paidInMempool :: Address -> Word64 -> Store.Balance
paidInMempool addr amount = Store.Balance addr 0 amount 1 1 amount

-- | The balance of an address that received the amount once and spent it
-- all: two transactions, nothing left
emptied :: Address -> Word64 -> Store.Balance
emptied addr amount = Store.Balance addr 0 0 0 2 amount

-- | A transaction of the store paying an amount to one of our addresses,
-- funded by somebody else, with a fee of 1000
creditTx :: Ctx -> TxHash -> Store.BlockRef -> Address -> Word64 -> Store.Transaction
creditTx ctx tid block addr amount = payoutTx ctx tid block [(addr, amount)]

-- | A transaction of the store paying several addresses, funded by somebody
-- else, with a fee of 1000
payoutTx :: Ctx -> TxHash -> Store.BlockRef -> [(Address, Word64)] -> Store.Transaction
payoutTx ctx tid block paid =
  storeTx tid block [inputFrom ctx (fakeTxHash 0xee, 0) (otherAddr 0) (sum (snd <$> paid) + 1000)] (uncurry (outputTo ctx) <$> paid) 1000

-- | A transaction of the store spending one coin of ours, paying the given
-- addresses with a fee of 1000
spendingStoreTx :: Ctx -> TxHash -> Store.BlockRef -> (TxHash, Word32) -> Address -> [(Address, Word64)] -> Store.Transaction
spendingStoreTx ctx tid block spent from paid =
  storeTx tid block [inputFrom ctx spent from (sum (snd <$> paid) + 1000)] (uncurry (outputTo ctx) <$> paid) 1000

-- | A transaction of the store with the given inputs, outputs and fee,
-- 226 bytes long, at the given block, with no lock time
storeTx :: TxHash -> Store.BlockRef -> [Store.StoreInput] -> [Store.StoreOutput] -> Word64 -> Store.Transaction
storeTx = storeTxLockedAt 0

-- | 'storeTx', with the lock time given
storeTxLockedAt :: Word32 -> TxHash -> Store.BlockRef -> [Store.StoreInput] -> [Store.StoreOutput] -> Word64 -> Store.Transaction
storeTxLockedAt lockTime tid block inputs outputs fee =
  Store.Transaction
    { Store.block = block,
      Store.version = 1,
      Store.locktime = lockTime,
      Store.inputs = inputs,
      Store.outputs = outputs,
      Store.deleted = False,
      Store.rbf = False,
      Store.timestamp = 1700000000,
      Store.txid = tid,
      Store.size = 226,
      Store.weight = 904,
      Store.fee = fee
    }

-- | An input of the store spending the coin of an address, worth the amount
inputFrom :: Ctx -> (TxHash, Word32) -> Address -> Word64 -> Store.StoreInput
inputFrom ctx (spentHash, spentIndex) from amount =
  Store.StoreInput (OutPoint spentHash spentIndex) 0xffffffff BS.empty (addressScript ctx from) amount [] (Just from)

-- | An output of the store paying an address
outputTo :: Ctx -> Address -> Word64 -> Store.StoreOutput
outputTo ctx to amount = Store.StoreOutput amount (addressScript ctx to) Nothing (Just to)

-- | The same transaction with another time, in seconds since 1970: the
-- store first saw it then, or took it from a block of that time. The
-- transactions above all have the time 1700000000, 'seenTime'.
seenAt :: Word64 -> Store.Transaction -> Store.Transaction
seenAt time (Store.Transaction b v l i o d r _ x s w f) =
  Store.Transaction b v l i o d r time x s w f

-- | The same transaction, replaced or dropped: the store keeps it, marked
-- as deleted
replaced :: Store.Transaction -> Store.Transaction
replaced (Store.Transaction b v l i o _ r t x s w f) =
  Store.Transaction b v l i o True r t x s w f

-- | The same transaction with one of its outputs spent, as the store
-- reports it: by the given input of another transaction
withSpender :: Word32 -> (TxHash, Word32) -> Store.Transaction -> Store.Transaction
withSpender index (spender, input) (Store.Transaction b v l i outputs d r t x s w f) =
  Store.Transaction b v l i (zipWith mark [0 ..] outputs) d r t x s w f
  where
    mark n (Store.StoreOutput value script spent addr)
      | n == index = Store.StoreOutput value script (Just (Store.Spender spender input)) addr
      | otherwise = Store.StoreOutput value script spent addr

{- Bytes -}

-- | The bytes a hexadecimal literal writes
hexBytes :: Text -> BS.ByteString
hexBytes text = fromMaybe (error ("Not hex: " <> cs text)) (decodeHex text)

-- | The fingerprint a hexadecimal literal writes
fingerprintOf :: Text -> Fingerprint
fingerprintOf = forceRight . textToFingerprint

-- | The file Bitcoin Core prepared for the test account
-- (test/data/psbt/core/core-prepared.psbt), signed by the test wallet into
-- that file, in base64: the file Bitcoin Core's analyzepsbt finds
-- complete, and whose transaction its testmempoolaccept accepts on the
-- regtest chain the coin is on. Its signed input carries its final
-- script, no derivation record, as a finalizer leaves it, and the path
-- /0/0 of its address in the field of hw. That record was cleared and
-- that field added apart from hw, in the file checked on the chain, and
-- Bitcoin Core 32.1 extracts the same transaction from both. This holds
-- for the two files below as well. See test/data/psbt/README.md.
corePreparedSignedText :: BS.ByteString
corePreparedSignedText = "cHNidP8BAHcCAAAAAZzolVrP9CarlPjUSHujJgVmsHiFB+Mm+Bafv63Mct7+AAAAAAD9////AgDh9QUAAAAAGXapFKvIhl67bbYllWAxY5ddm6o2RjLKiKw2CBAkAQAAABl2qRRFWqUMJmMUF3HrrCCW43gzkRIn1oisAAAAAAX8Amh3AANidGMAAQCHAgAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP////8DAWYA/////wIA8gUqAQAAABl2qRTIDVUiPi8QYTY63Sp7A2PChb8pjYisAAAAAAAAAAAmaiSqIant4vYcP3HR3v0/qZnfo2lTdVxpBol5mWK0i+vYNpdOjPkAAAAAAQdqRzBEAiAt4w5OLHA+pvdD8Ech2/HZRoxlJ5EQEddAV3jzEPe6tAIgGhnWoPRPsXAT4lNaosniGeLccnAzo++kwH50AovU9p4BIQMvZDcCIlbIVOAJDn1si2XpZgbvf0BTmeDwGHyTCXrA+QX8Amh3AggAAAAAAAAAAAAAIgIDdvLnBkWN11Zzuhm479hC793ZvArkHjEznf/PkXtttusYvFYPlSwAAIAAAACAAAAAgAEAAAAAAAAAAA=="

-- | The file Bitcoin Core prepared for the bip84 account of the test
-- wallet (test/data/psbt/core/core-prepared-segwit.psbt), signed by the
-- test wallet into that file, in base64: the file Bitcoin Core 32.1's
-- analyzepsbt finds complete, and whose transaction its testmempoolaccept
-- accepts on the regtest chain the coin is on, 144 virtual bytes for a
-- fee of 1440. See test/data/psbt/README.md.
coreSegwitPreparedSignedText :: BS.ByteString
coreSegwitPreparedSignedText = "cHNidP8BAHQCAAAAAWc1xfZ0yPv0PUM5wdsglkTvSrW2RVXDC/aJc1pkR9gIAAAAAAD9////AgDh9QUAAAAAGXapFC0S+z6YrxZ3BWxVA/Hx0aMiQAbZiKxgCxAkAQAAABYAFP0l5/X0gP7M5jxDskByzUud0N6hAAAAAAX8Amh3AANidGMAAQCDAgAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP////8CUQD+////AgDyBSoBAAAAFgAUJOZHGwyBWu4Cx70iYUy2DkgRoBkAAAAAAAAAACZqJKohqe3i9hw/cdHe/T+pmd+jaVN1XGkGiXmZYrSL69g2l06M+QAAAAABAR8A8gUqAQAAABYAFCTmRxsMgVruAse9ImFMtg5IEaAZAQhsAkgwRQIhAOextS4gCOX8iHiGEXf4bEqQo2OlixpAsJf1zIGkwO7mAiA0MlumEVMEGQx8IT9NPcwcRR4Wk4SBnQPmTTWVcH8q4AEhAnZS3vvAmDIdpIXaYXXei4ePQz8NiTNDVSYlyc1Qkz9TBfwCaHcCCAAAAAAAAAAAAAAiAgMSvuVMTQzd0oxq2bXlbxXHLisLmALO0zcJZKa9BYoOChi8Vg+VVAAAgAAAAIAAAACAAQAAAAAAAAAA"

-- | The same, signed into the version 2 file Core writes by default
-- (test/data/psbt/core/core-prepared-v2.psbt): the file above with the
-- fields of version 2, converted apart from hw. Bitcoin Core 32.1's
-- analyzepsbt finds it complete, its finalizepsbt extracts the transaction
-- of the file above, and its testmempoolaccept accepts it.
corePreparedSignedV2Text :: BS.ByteString
corePreparedSignedV2Text = "cHNidP8BAgQCAAAAAQMEAAAAAAEEAQEBBQECAfsEAgAAAAX8Amh3AANidGMAAQCHAgAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP////8DAWYA/////wIA8gUqAQAAABl2qRTIDVUiPi8QYTY63Sp7A2PChb8pjYisAAAAAAAAAAAmaiSqIant4vYcP3HR3v0/qZnfo2lTdVxpBol5mWK0i+vYNpdOjPkAAAAAAQdqRzBEAiAt4w5OLHA+pvdD8Ech2/HZRoxlJ5EQEddAV3jzEPe6tAIgGhnWoPRPsXAT4lNaosniGeLccnAzo++kwH50AovU9p4BIQMvZDcCIlbIVOAJDn1si2XpZgbvf0BTmeDwGHyTCXrA+QEOIJzolVrP9CarlPjUSHujJgVmsHiFB+Mm+Bafv63Mct7+AQ8EAAAAAAEQBP3///8F/AJodwIIAAAAAAAAAAAAAQMIAOH1BQAAAAABBBl2qRSryIZeu222JZVgMWOXXZuqNkYyyoisACICA3by5wZFjddWc7oZuO/YQu/d2bwK5B4xM53/z5F7bbbrGLxWD5UsAACAAAAAgAAAAIABAAAAAAAAAAEDCDYIECQBAAAAAQQZdqkURVqlDCZjFBdx66wgluN4M5ESJ9aIrAA="

{- Signatures at the edge of the curve -}

-- | Order n of the secp256k1 curve, for the tests that build a signature
-- with r or s outside 1 to n−1, which recovers no key
secp256k1Order :: Integer
secp256k1Order = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

-- | A scalar as a signature carries it: 32 bytes, big-endian
toBytes32 :: Integer -> BS.ByteString
toBytes32 i = BS.replicate (32 - BS.length bs) 0 <> bs
  where
    bs = integerToBS i

{- A backup in shares -}

-- | Three shares of a backup of six, any three of which rebuild it, and its
-- passphrase: a set of the device tests of Trezor's firmware
-- (MNEMONIC_SLIP39_BASIC_20_3of6 in tests/common.py), whose
-- test_passphrase_slip39_basic.py gives the master key and the first
-- testnet address of the seed. The other keys and addresses below were
-- derived from that seed apart from hw, with the SLIP-39 verifier of
-- haskoin-core and the Python tools of test/data/message-signing/tools,
-- which gave back that master key and that address first.
testShares :: [Text]
testShares =
  [ "extra extend academic bishop cricket bundle tofu goat apart victim enlarge program behavior permit course armed jerky faint language modern",
    "extra extend academic acne away best indicate impact square oasis prospect painting voting guest either argue username racism enemy eclipse",
    "extra extend academic arcade born dive legal hush gross briefing talent drug much home firefly toxic analysis idea umbrella slice"
  ]

testSharesPassphrase :: Text
testSharesPassphrase = "TREZOR"

-- | The backup the shares rebuild, which the passphrase decrypts: its
-- bytes are MNEMONIC_SLIP39_BASIC_20_3of6_SECRET of those tests, and the
-- shares carry the identifier 10282, no extendable flag and the iteration
-- exponent 0
testSharesBackup :: EncryptedMasterSecret
testSharesBackup = EncryptedMasterSecret 10282 False 0 (fromMaybe (error "testSharesBackup") (decodeHex "491b795b80fc21ccdf466c0fbc98c8fc"))

-- | The secret of the seed of the shares with their passphrase
testSharesSecret :: SeedSecret
testSharesSecret = SharesSecret testSharesBackup testSharesPassphrase

-- | The master key of the seed of the shares with their passphrase, as the
-- device tests give it, and the fingerprint of the seed
testSharesMasterKey, testSharesFingerprintText :: Text
testSharesMasterKey = "xprv9s21ZrQH143K2pMWi8jrTawHaj16uKk4CSbvo4Zt61tcrmuUDMx2o1Byzcr3saXNGNvHP8zZgXVdJHsXVdzYFPavxvCyaGyGr1WkAYG83ce"
testSharesFingerprintText = "5596eb4d"

-- | The keys of the bip44 account 0 of that seed on Bitcoin's testnet,
-- m/44'/1'/0', and its first two receiving addresses: the first is the
-- address the device tests give
testSharesTestnetXPrv, testSharesTestnetXPub :: Text
testSharesTestnetXPrv = "tprv8fo2vdaeWiGHxEvp1STwjxywEooz5kMp2GJGANWqtqdUqHamGg2boUJktFZGxAbgkz4jEqsrPgHTYC4VwKwgGUrsRdAq8WZtCgjS4PEYnP2"
testSharesTestnetXPub = "tpubDCV553ctf5wxqhxbu68Y9Ne3oqKvF5YibZu3StZ9K7RsfmqXu4rByxvd4MoWSykTqqvgbUEZBHJw9iXfitPge7Mwxg873BUE3as7yF6iWSr"

testSharesTestnetAddrs :: [Text]
testSharesTestnetAddrs = ["mi4HXfRJAqCDyEdet5veunBvXLTKSxpuim", "mzDSttNhCtvkJExSAiz6xksdyEQUuxuDVc"]

-- | The public key of the bip44 account 0 of that seed on Bitcoin,
-- m/44'/0'/0', and its first receiving address
testSharesXPub, testSharesAddr :: Text
testSharesXPub = "xpub6CDgeTHt97zmW24XoYdjH9PVFMUj6qcYAe9SZXMKRKJbvTNeZhutNkQqajLyZrQ9DCqdnGenKhBD6UTrT1nHnoLCfFHkdeX8hDsZx1je6b2"
testSharesAddr = "1NomKAUNnbASwbPuGHmkSVmnrJS5tZeVce"

-- | Shares of a backup of several groups, of the same device tests
-- (MNEMONIC_SLIP39_ADVANCED_20): two groups of four rebuild it. The first
-- share is its group alone; the three others are three of the five shares
-- of another group, which needs three. With the passphrase "TREZOR" the
-- tests give the master key and the first testnet address below.
testGroupShares :: [Text]
testGroupShares =
  [ "eraser senior beard romp adorn nuclear spill corner cradle style ancient family general leader ambition exchange unusual garlic promise voice",
    "eraser senior ceramic snake clay various huge numb argue hesitate auction category timber browser greatest hanger petition script leaf pickup",
    "eraser senior ceramic shaft dynamic become junior wrist silver peasant force math alto coal amazing segment yelp velvet image paces",
    "eraser senior ceramic round column hawk trust auction smug shame alive greatest sheriff living perfect corner chest sled fumes adequate"
  ]

testGroupSharesMasterKey, testGroupSharesTestnetAddr :: Text
testGroupSharesMasterKey = "xprv9s21ZrQH143K3dzDLfeY3cMp23u5vDeFYftu5RPYZPucKc99mNEddU4w99GxdgUGcSfMpVDxhnR1XpJzZNXRN1m6xNgnzFS5MwMP6QyBRKV"
testGroupSharesTestnetAddr = "mkKDUMRR1CcK8eLAzCZAjKnNbCquPoWPxN"

{- Sources of entropy -}

-- | Random bytes, read from /dev/random once, on 2026-10-26: the 64 bytes
-- of a sample of a sound source
goodSample :: BS.ByteString
goodSample =
  BS.pack
    [ 0x5d, 0x51, 0x10, 0x86, 0xbe, 0x8b, 0xbf, 0x2f, 0xa3, 0x2c, 0xdf, 0x3d, 0x16, 0x77, 0x94, 0x68,
      0xa6, 0x1a, 0x1b, 0x48, 0x20, 0x21, 0x40, 0x18, 0xc8, 0x8f, 0x8e, 0xa3, 0x19, 0xd1, 0xe5, 0x5d,
      0xda, 0x22, 0x2e, 0xd6, 0x6a, 0xd1, 0xe3, 0x98, 0xe3, 0x41, 0xef, 0xd1, 0x9b, 0x01, 0x6a, 0xbe,
      0x6d, 0xc8, 0x9e, 0x49, 0x83, 0xf4, 0xe0, 0xec, 0xcc, 0xc9, 0x6a, 0x4d, 0x1c, 0xd6, 0x83, 0x4f
    ]

-- | What hw says before it reads the system
readingSystemMessage :: String
readingSystemMessage = "Reading the entropy of the system, which can take a moment while the system gathers it"

-- | The sources of hw: the entropy of the system, no dice, nothing said
systemSources :: Sources IO
systemSources = Sources {readSystem = systemEntropy, askDice = Nothing, say = \_ -> return ()}

-- | Sources that give out the given entropies in order, named "fixed", with
-- the given dice; what they say and do is noted in the log, a read as
-- "system" and the dice as "dice", each with the number of bytes asked
fixedSources :: IORef [String] -> [BS.ByteString] -> Maybe BS.ByteString -> IO (Sources IO)
fixedSources logged entropies diceM = do
  left <- newIORef entropies
  return
    Sources
      { readSystem = \bytes -> do
          liftIO $ modifyIORef logged (<> ["system " <> show bytes])
          rest <- liftIO $ readIORef left
          case rest of
            [] -> throwError "The fixed entropies ran out"
            entropy : more -> liftIO (writeIORef left more) >> return ("fixed", entropy),
        askDice = fmap (\dice bytes -> liftIO (modifyIORef logged (<> ["dice " <> show bytes])) >> return dice) diceM,
        say = \message -> modifyIORef logged (<> [message])
      }
