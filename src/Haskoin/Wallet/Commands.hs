{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The commands of hw. Each reads what it needs from the database, the
-- store and the user, and answers with a 'Response'. A command runs in one
-- transaction of the database: one that fails leaves the database as it
-- was, and answers with its error. An answer that shows an account reads it
-- once the command is done, so that it shows what the command changed.
module Haskoin.Wallet.Commands
  ( commandResponse,
    commandListing,

    -- * Listings
    cmdCsv,
    cmdExportSheet,
    accountsListing,
    addressesListing,
    balancesListing,
    coinsListing,
    txsListing,
    pendingTxsListing,

    -- * Mnemonics and accounts
    cmdMnemonic,
    cmdCreateAcc,
    cmdRecover,
    cmdCheckMnemonic,
    cmdImportAcc,
    cmdExportAcc,
    cmdRenameAcc,
    cmdDeleteAcc,
    cmdAccounts,

    -- * Addresses
    cmdReceive,
    cmdShow,
    cmdAddresses,
    cmdBalances,
    cmdLabel,

    -- * Coins
    cmdCoins,
    cmdLockCoin,
    cmdUnlockCoin,

    -- * Transactions
    cmdTxs,
    cmdPendingTxs,
    cmdImportTx,
    cmdExportTx,
    cmdDeleteTx,

    -- * Payments: preparing, signing and sending
    cmdPrepareTx,
    cmdPrepareSendAll,
    cmdSignTx,
    cmdBroadcast,
    cmdBumpFee,

    -- * Sweeps
    cmdPrepareSweep,
    cmdSignSweep,

    -- * Sync, messages and backups
    cmdSync,
    cmdFees,
    cmdSignMsg,
    cmdVerifyMsg,
    cmdBackup,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, join, unless, void, when, (<=<))
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char (isHexDigit)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List (find, genericTake, nub, partition)
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word64)
import Database.Persist.Sqlite (transactionUndo)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Amounts (AmountUnit)
import Haskoin.Wallet.Backup (backupDatabase, backupDirectory, copyDatabase)
import Haskoin.Wallet.Bip329 (recordsText)
import Haskoin.Wallet.CutAddress (cutAddress)
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.Tx.FeeRates
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Shares (genShares)
import Haskoin.Wallet.Labels (accountRecords, fileShown, holdsAnything, importFile, readAccountFile)
import Haskoin.Wallet.Cli.Csv (sheetCsv)
import Haskoin.Wallet.Cli.Ods (workbookOds)
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.MessageSigning
import Haskoin.Wallet.Migration (latestDatabaseVersion)
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Database.BestBlock (BestBlock (..), getBest)
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Cli.PrettyPrinter (addedCoinsDoc, agoText, childrenDoc, coinBalancesPrinter, dustReportDoc, feeRatesDoc, reducedDoc, renderOn, txAnswerDoc)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Sheet (AccountSheets (..), Sheet, listingSheet, stacked, walletSheets)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.SyncStatus (StatusLine (..), withStatusLine)
import Haskoin.Wallet.Tx.Building (ChangeTo (..), CoinControl (..), buildTxSignData, coinWithPath)
import Haskoin.Wallet.Tx.BumpFee
import Haskoin.Wallet.Tx.LockTime (isStale, lockTimeFor)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Tx.PsbtFile (FileAccount (..), psbtTxFile, signedPsbt, txFilePsbt)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (heldChainsOf, labelledTxFile, labelsFromFile, parseOutsideTxSignData, parseTxFile, parseTxSignData, signHeldWalletTx)
import Haskoin.Wallet.Tx.Recipient (Share, checkSharing, readPayment, readRecipient, recipientAddress)
import Haskoin.Wallet.Tx.SendAll
  ( DustReport,
    buildSendAllSignData,
    dustQuestion,
    dustReport,
    isDustCoin,
    sendAllAmounts,
  )
import Haskoin.Wallet.Tx.Size (checkFeeRate, highFeeRate)
import Haskoin.Wallet.Tx.Sweep (buildSweepSignData, fetchSweepCoins, signTxWithKeys)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Directory (doesPathExist)
import System.IO (hPutStrLn, stderr)
import System.Random (initStdGen)

{- Running a command -}

-- | The answer of hw to a command
commandResponse :: Ctx -> Config -> AmountUnit -> Command -> IO Response
commandResponse ctx cfg unit = \case
  -- A listing to write to a CSV file, whatever it lists
  cmd
    | Just file <- csvFileOf cmd,
      Just listing <- commandListing ctx cfg cmd ->
        cmdCsv cfg file listing
  -- Mnemonics and accounts
  CommandMnemonic opts -> cmdMnemonic opts
  CommandCreateAcc name net kind -> cmdCreateAcc ctx cfg name net kind
  CommandRecover opts -> cmdRecover ctx cfg opts
  CommandCheckMnemonic account -> cmdCheckMnemonic ctx cfg account
  CommandImportAcc netM kindM file -> cmdImportAcc ctx cfg netM kindM file
  CommandExportAcc account file -> cmdExportAcc ctx cfg account file
  CommandRenameAcc opts -> cmdRenameAcc cfg opts
  CommandDeleteAcc name net path -> cmdDeleteAcc cfg name net path
  CommandAccounts account _ -> cmdAccounts cfg account
  -- Addresses
  CommandReceive opts -> cmdReceive ctx cfg opts
  CommandShow account shown netM -> cmdShow ctx cfg account shown netM
  CommandAddresses account internal _ paging -> cmdAddresses cfg account internal paging
  CommandBalances account _ paging -> cmdBalances cfg account paging
  CommandLabel opts -> cmdLabel ctx cfg opts
  -- Coins
  CommandCoins account locked _ paging -> cmdCoins cfg account locked paging
  CommandLockCoin outpoint -> cmdLockCoin cfg outpoint
  CommandUnlockCoin outpoint -> cmdUnlockCoin cfg outpoint
  -- Transactions
  CommandTxs account _ paging -> cmdTxs cfg account paging
  CommandPendingTxs account _ paging -> cmdPendingTxs ctx cfg account paging
  CommandImportTx account file -> cmdImportTx ctx cfg account file
  CommandExportTx named file -> cmdExportTx ctx cfg named file
  CommandDeleteTx named -> cmdDeleteTx ctx cfg named
  CommandPrepareTx opts -> cmdPrepareTx ctx cfg unit opts
  CommandPrepareSendAll opts -> cmdPrepareSendAll ctx cfg unit opts
  CommandSignTx signing -> cmdSignTx ctx cfg unit signing
  CommandBroadcast namedM fileM yes netM -> cmdBroadcast ctx cfg unit namedM fileM yes netM
  CommandBumpFee opts -> cmdBumpFee ctx cfg unit opts
  -- Sweeps
  CommandPrepareSweep opts -> cmdPrepareSweep ctx cfg unit opts
  CommandSignSweep signing keysM netM -> cmdSignSweep ctx cfg unit signing keysM netM
  -- Sync, messages and backups
  CommandSync account full -> cmdSync ctx cfg account full
  CommandFees netM -> cmdFees ctx cfg netM
  CommandSignMsg opts -> cmdSignMsg ctx cfg opts
  CommandVerifyMsg opts -> cmdVerifyMsg ctx cfg opts
  CommandBackup fileM -> cmdBackup cfg fileM
  CommandExportSheet account file -> cmdExportSheet ctx cfg account file

-- | The listing of a command that lists, which the screen reads chunk by
-- chunk, or Nothing for a command that answers at once
commandListing :: Ctx -> Config -> Command -> Maybe (ExceptT String (DB IO) Listing)
commandListing ctx cfg = \case
  CommandAccounts account _ -> Just $ accountsListing account
  CommandAddresses account internal _ paging -> Just $ addressesListing cfg account internal paging
  CommandBalances account _ paging -> Just $ balancesListing cfg account paging
  CommandCoins account locked _ paging -> Just $ coinsListing cfg account locked paging
  CommandTxs account _ paging -> Just $ txsListing cfg account paging
  CommandPendingTxs account _ paging -> Just $ pendingTxsListing ctx account paging
  _ -> Nothing

-- | Run a command in one transaction of the database, which no other
-- command changes meanwhile. What a command wrote before it failed is
-- undone.
runDBResponse :: Config -> ExceptT String (DB IO) Response -> IO Response
runDBResponse cfg action = do
  result <- catchDatabaseInUse $ runDB cfg $ do
    answer <- runExceptT action
    when (isLeft answer) transactionUndo
    return answer
  return $ answered (join result)

-- | Run a command that needs no database
runResponse :: ExceptT String IO Response -> IO Response
runResponse action = answered <$> runExceptT action

-- | The answer of a command, or its error
answered :: Either String Response -> Response
answered = either (ResponseError . cs) id

-- | The account of a command that acts on one account, as the user chose
-- it, with the default account of the default network of the
-- configuration in a wallet of several accounts
-- ('chooseAccount')
chosenAccount :: Config -> AccountChoice -> ExceptT String (DB IO) (DBAccountId, DBAccount)
chosenAccount cfg = chooseAccount (defaultAccountNetwork cfg)

{- Answers -}

-- | An account, with the balance it can spend now
withSpendable :: (DBAccountId, DBAccount) -> ExceptT String (DB IO) (DBAccount, Word64)
withSpendable (accId, acc) = (,) acc <$> spendableBalance (accountNetwork acc) accId

-- | What an address can spend now, among the spendable balances read for
-- it: nothing when it holds no such coin
spendableAt :: Map Text Word64 -> DBAddress -> Word64
spendableAt balances addr = Map.findWithDefault 0 (dBAddressAddress addr) balances

-- | The answer showing an account, with the balance it can spend now
accountAnswer :: DBAccountId -> ExceptT String (DB IO) Response
accountAnswer accId = do
  acc <- getAccountById accId
  uncurry ResponseAccount <$> withSpendable (accId, acc)

-- | The answer showing an address of the account, with the balance it can
-- spend now
addressAnswer :: DBAccountId -> DBAddress -> ExceptT String (DB IO) Response
addressAnswer accId addr = do
  acc <- getAccountById accId
  balances <- spendableAtAddresses (accountNetwork acc) accId [dBAddressAddress addr]
  return $ ResponseAddress acc addr (spendableAt balances addr)

-- | The answer showing a transaction of the account
txAnswer :: DBAccountId -> TxInfo -> ExceptT String (DB IO) Response
txAnswer accId info = do
  acc <- getAccountById accId
  return $ ResponseTx acc info

{- Mnemonics and accounts -}

-- | A new mnemonic, or with --shares the shares of a new backup, from the
-- entropy of the system, mixed with dice rolls with --dice
cmdMnemonic :: NewMnemonic -> IO Response
cmdMnemonic opts =
  runResponse $ do
    let sources =
          Sources
            { readSystem = systemEntropy,
              askDice = if opts.dice then Just (askDiceEntropy terminal) else Nothing,
              say = tell terminal
            }
    case opts.shares of
      Nothing -> do
        when (isJust opts.threshold) $
          throwError "--threshold goes with --shares: it is the number of shares that rebuild the seed"
        (source, mnemonic) <- genMnemonic sources opts.entropy
        return $ ResponseMnemonic source (Text.words mnemonic)
      Just count -> do
        (source, threshold, shares) <- genShares sources opts.entropy count opts.threshold
        return $ ResponseShares source (fromIntegral threshold) (Text.words <$> shares)

-- | Create an account of a kind on a network from a mnemonic, or from the
-- shares of a backup: the next account of the seed of that kind on that
-- network
cmdCreateAcc :: Ctx -> Config -> Text -> Maybe Network -> AccountKind -> IO Response
cmdCreateAcc ctx cfg name netM kind =
  runDBResponse cfg $ do
    -- Before the user types the mnemonic, not after
    net <- liftMaybe noNetwork (commandNetwork cfg netM)
    checkNameFree net name
    liftEither $ checkKindOnNetwork net kind
    secret <- askNewSeedSecret terminal
    seed <- seedToCreateWith ctx secret
    (_, xpub) <- lift $ nextAccount kind net ctx seed
    (accId, _) <- insertAccount kind net ctx name xpub (Just (seedFingerprint seed))
    -- The accounts of the seed created before hw recorded fingerprints
    lift $ recordFingerprint ctx seed
    accountAnswer accId

-- | The seed createacc derives the account from: the one BIP39 derives
-- from the mnemonic and the passphrase, or the one SLIP-39 derives from a
-- backup in shares and its passphrase. For a mnemonic with a passphrase
-- that normalizing changes, hw derived another seed before, from the
-- passphrase as typed. When the wallet has an account of that seed and
-- none of the other, the new account is of it too, so that one seed keeps
-- one set of accounts, and the user is told.
seedToCreateWith :: Ctx -> SeedSecret -> ExceptT String (DB IO) PublicSeed
seedToCreateWith ctx secret = do
  seed <- liftEither $ secretSeed ctx secret
  typedM <- liftEither $ secretTypedSeed ctx secret
  case typedM of
    Nothing -> return seed
    Just typed -> do
      ofNormalized <- lift $ seedHasAccount ctx seed
      ofTyped <- lift $ seedHasAccount ctx typed
      if ofTyped && not ofNormalized
        then liftIO (tell terminal typedPassphraseCreated) >> return typed
        else return seed

-- | Recover the accounts of a seed that were paid, from its mnemonic or
-- the shares of its backup: see "Haskoin.Wallet.Sync". The search covers
-- the network --network names, or else Bitcoin and Bitcoin Cash
-- ('recoverNetworks'), one network after the other.
--
-- The store of each network is asked whether it is up before the mnemonic
-- is typed. A network whose store is down is left out, and the answer
-- names it, as sync does, so that one store out of service does not hold
-- back the other network. When no store is up, the command fails before
-- the mnemonic: the computer is offline, or the host is wrong.
--
-- The funds of the seed at the accounts hw does not hold are told, not
-- taken. When no account is found on any network and normalizing changes
-- the passphrase, the accounts hw derived before, from the passphrase as
-- typed, are looked for too, and the user is told of those. Only the seed
-- of BIP39 is looked at for the other kinds of accounts: other wallets
-- derive from no other.
cmdRecover :: Ctx -> Config -> Recover -> IO Response
cmdRecover ctx cfg opts =
  withStatusLine $ \line ->
    runDBResponse cfg $ do
      let storeOn = webStore ctx cfg
          watch = terminalWatcher line
      -- An offline computer hears it before the mnemonic is typed
      checked <- liftIO $ forM (recoverNetworks opts.network) $ \net -> (,) net <$> healthFailure (storeOn net)
      let searched = [net | (net, Nothing) <- checked]
          failures = [(net, failure) | (net, Just failure) <- checked]
      -- No store is up: the command fails with the first failure, as it
      -- does on one network
      case failures of
        (_, failure) : _ | null searched -> throwError failure
        _ -> return ()
      -- Typed once: a mistyped mnemonic finds no paid account
      secret <- askSeedSecret terminal
      seed <- liftEither $ secretSeed ctx secret
      let accountsOn fromSeed net = recoverSeed ctx cfg (storeOn net) net fromSeed opts.accountGap watch
          unspendableOn net = map ((,) net) <$> foreignFunds ctx cfg (storeOn net) net seed opts.accountGap watch
      onEach <- forM searched $ \net -> (,) <$> accountsOn seed net <*> unspendableOn net
      let found = concatMap fst onEach
          elsewhere = concatMap snd onEach
      typedM <- liftEither $ secretTypedSeed ctx secret
      fromTyped <- case typedM of
        Just typed | null found -> do
          results <- concat <$> forM searched (accountsOn typed)
          unless (null results) $ liftIO $ do
            eraseStatus line
            tell terminal typedPassphraseRecovered
          return results
        _ -> return []
      return $ ResponseRecover searched opts.accountGap (found <> fromTyped) elsewhere [(net, cs failure) | (net, failure) <- failures]

cmdCheckMnemonic :: Ctx -> Config -> AccountChoice -> IO Response
cmdCheckMnemonic ctx cfg account =
  runDBResponse cfg $ do
    (accId, acc) <- chosenAccount cfg account
    secret <- askSeedSecret terminal
    matched <- matchAccount terminal (accountKind acc) (accountNetwork acc) ctx (accountIndex acc) (accountXPubKey ctx acc) secret
    -- A seed proven gives its fingerprint to every account of it
    for_ matched $ \(seed, _) -> lift $ recordFingerprint ctx seed
    tested <- getAccountById accId
    let typed = case secret of
          MnemonicSecret _ -> "The mnemonic and passphrase"
          SharesSecret _ _ -> "The shares and passphrase"
    case matched of
      Just _ -> return $ ResponseAccResult tested True (typed <> " matched the account")
      Nothing -> do
        seed <- liftEither $ secretSeed ctx secret
        elsewhere <- liftIO $ keyElsewhere ctx acc seed
        return $ ResponseAccResult tested False (typed <> " did not match the account" <> elsewhere)

-- | What checkmnemonic adds when the seed has the key of the account at
-- another path than the account's. An account imported from its key
-- alone, which says nothing of its path, was given its kind and its
-- network by the user: this tells which it has, and how to import it
-- again, or that it is of a kind hw cannot spend from. The account's own
-- network comes first among the paths found, since the test networks
-- share a coin type. Empty when the seed has the key nowhere hw looks.
keyElsewhere :: Ctx -> DBAccount -> PublicSeed -> IO Text
keyElsewhere ctx acc seed = do
  held <- accountsWithKey seed key
  others <- foreignAccountsWithKey seed key
  return $ case (onOwnNetworkFirst held, onOwnNetworkFirst others) of
    (Just (kind, net), _) ->
      let (what, options) = heldAs kind net
       in ", but they give its key at " <> pathOn (accountDeriv kind net index) net <> ": it is " <> what <> ". Delete it with deleteacc and import it again with " <> options <> "."
    (Nothing, Just (kind, net)) ->
      ", but they give its key at " <> pathOn (foreignDeriv kind net index) net <> ": it is an account of the kind " <> cs (foreignKindDescription kind) <> ", which hw cannot spend from."
    _ -> ""
  where
    key = accountXPubKey ctx acc
    index = accountIndex acc
    ownNet = accountNetwork acc
    onOwnNetworkFirst :: [(kind, Network)] -> Maybe (kind, Network)
    onOwnNetworkFirst found = find ((== ownNet.name) . (.name) . snd) found <|> listToMaybe found
    pathOn :: HardPath -> Network -> Text
    pathOn path net = cs (pathToStr path) <> " on " <> cs net.name
    -- What the account is, and the options that import it so
    heldAs :: AccountKind -> Network -> (Text, Text)
    heldAs kind net
      | net.name == ownNet.name = ("a " <> kindName kind <> " account, not a " <> kindName (accountKind acc) <> " one", "--type " <> kindName kind)
      | kind == accountKind acc = ("an account of " <> cs net.name <> ", not of " <> cs ownNet.name, "--network " <> cs net.name)
      | otherwise = ("a " <> kindName kind <> " account of " <> cs net.name <> ", not a " <> kindName (accountKind acc) <> " one of " <> cs ownNet.name, "--type " <> kindName kind <> " --network " <> cs net.name)

-- | Add the accounts of a BIP 329 file and set the labels it carries, as
-- "Haskoin.Wallet.Labels" reads it, a key that says no kind of its account
-- taken for one of the kind of --type. The answer says what became of
-- each account, and what the file labels that the wallet does not have
-- yet.
cmdImportAcc :: Ctx -> Config -> Maybe Network -> Maybe AccountKind -> FilePath -> IO Response
cmdImportAcc ctx cfg netM kindM file =
  runDBResponse cfg $ do
    content <- liftEitherIO $ readTextFile file
    ResponseImport <$> importFile ctx (commandNetwork cfg netM) kindM content

-- | Write the account, or every account, to a BIP 329 file with its labels
-- and the coins the user locked
cmdExportAcc :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdExportAcc ctx cfg account file =
  runDBResponse cfg $ do
    accs <- getAccountsByName account
    when (null accs) $ throwError "There are no accounts in the wallet"
    checkPathFree file
    records <- concat <$> mapM (accountRecords ctx) accs
    liftEitherIO $ writeTextFile file (recordsText records)
    return $ ResponseFile file

cmdRenameAcc :: Config -> RenameAcc -> IO Response
cmdRenameAcc cfg opts =
  runDBResponse cfg $ do
    _ <- renameAccount opts.old opts.new
    accountAnswer . fst =<< getAccountByName opts.new

-- | Delete an account, given its network and its path too, which must be
-- those of the account
cmdDeleteAcc :: Config -> Text -> Network -> HardPath -> IO Response
cmdDeleteAcc cfg name net path =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName name
    unless (accountNetwork acc == net) $
      throwError "The network of the account to delete did not match"
    unless (path == accountPath acc) $
      throwError "The full derivation path of the account to delete did not match"
    lift $ deleteAccount accId
    return $ ResponseAccResult acc True ("The account " <> name <> " has been deleted")

-- | Write every row of a listing to a new CSV file, for a spreadsheet,
-- and answer with the file ("Haskoin.Wallet.Cli.Csv"). The listing is
-- read whole, in one transaction, as --json reads it, then written: the
-- file is either whole or not there. A path that is taken is refused
-- before the listing is read.
cmdCsv :: Config -> FilePath -> ExceptT String (DB IO) Listing -> IO Response
cmdCsv cfg file listing = do
  free <- runExceptT (checkPathFree file)
  case free of
    Left err -> return (ResponseError (cs err))
    Right () -> do
      answer <- readListing cfg listing
      case listingSheet answer of
        -- The listing failed, and its answer says why
        Nothing -> return answer
        Just sheet -> either (ResponseError . cs) (const (ResponseFile file)) <$> writeFileBytes file (sheetCsv sheet)

-- | Write every listing of the account, or of every account, to a new
-- OpenDocument spreadsheet, a tab each ("Haskoin.Wallet.Cli.Ods"), and
-- answer with the file. The tabs hold what the CSV files of the listings
-- hold ('walletSheets'). The six listings are read in one transaction, so
-- that the tabs agree with each other: the balances with the coins, and
-- the coins with the transactions.
cmdExportSheet :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdExportSheet ctx cfg account file =
  runDBResponse cfg $ do
    checkPathFree file
    accs <- getAccountsByName account
    when (null accs) $ throwError "There are no accounts in the wallet"
    accounts <- sheetOf (accountsListing account)
    held <- forM accs $ \(_, acc) -> (acc,) <$> accountSheets (AccountChoice (Just (dBAccountName acc)) Nothing)
    pending <- sheetOf (pendingTxsListing ctx account everyRow)
    now <- liftIO getCurrentTime
    liftEitherIO $ writeFileBytes file (workbookOds now (walletSheets accounts held pending))
    return $ ResponseFile file
  where
    everyRow :: Paging a
    everyRow = Paging Nothing Nothing
    -- The listings of one account: its receiving addresses then its change
    -- addresses, its balances, all its coins, and its transactions
    accountSheets named = do
      receiving <- sheetOf (addressesListing cfg named False everyRow)
      change <- sheetOf (addressesListing cfg named True everyRow)
      AccountSheets (stacked [receiving, change])
        <$> sheetOf (balancesListing cfg named everyRow)
        <*> sheetOf (coinsListing cfg named False everyRow)
        <*> sheetOf (txsListing cfg named everyRow)

-- | The sheet of a listing, read whole in the transaction the caller runs
sheetOf :: ExceptT String (DB IO) Listing -> ExceptT String (DB IO) Sheet
sheetOf listing = do
  answer <- wholeListing listing
  -- Every listing has a sheet: an answer without one would be a bug
  maybe (throwError "The listing has no sheet") return (listingSheet answer)

cmdAccounts :: Config -> Maybe Text -> IO Response
cmdAccounts cfg account = readListing cfg (accountsListing account)

-- | The accounts, each with the balance it can spend now, in the order
-- 'getAccounts' gives them: an account names the one to go on after
accountsListing :: Maybe Text -> ExceptT String (DB IO) Listing
accountsListing account =
  return
    Listing
      { listingStart = Nothing,
        listingMax = Nothing,
        listingRows = \afterM countM -> do
          accs <- getAccountsByName account
          let following = case afterM of
                Nothing -> accs
                Just name -> drop 1 $ dropWhile ((/= name) . dBAccountName . snd) accs
          mapM withSpendable $ maybe id genericTake countM following,
        listingCursor = Right . dBAccountName . fst,
        listingAnswer = ResponseAccounts,
        -- Every chunk reads every account anyway, and the table of
        -- accounts takes the width of its names from all of them
        listingWhole = True
      }

{- Short ids -}

-- A command takes a hash in full or by its short id, its first 8 digits
-- (see "Haskoin.Wallet.ShortId"). A short id is looked up among the
-- hashes the command can name, in every account, and the command then
-- goes on as with the full hash: it says what it says of a full hash the
-- account does not hold, for instance. A short id that matches none gets
-- the answer a full hash that matches nothing gets, with the short id in
-- it. One that matches two or more is refused, with each one named, for
-- the user to give the full hash.

-- | The pending transaction a command names, by its nosigHash or its txid
-- once signed, in full or by its short id
pendingHashOf :: HashRef -> ExceptT String (DB IO) TxHash
pendingHashOf = fullHashOf noPendingTx pendingNamed pendingHashesWith

-- | The transaction on the chain that txs --after names, by its txid in
-- full or by its short id, among those of every account. The listing
-- then refuses one of another account.
chainTxidOf :: HashRef -> ExceptT String (DB IO) TxHash
chainTxidOf named = fullHashOf none transactionsNamed (lift . walletTxidsWith) named
  where
    none = "The transaction " <> hashRefText named <> " is not in the account"

-- | The transaction a command names, pending or on the chain, by its
-- txid, or by its nosigHash while it is pending, in full or by its short
-- id: what label and bumpfee take
walletTxHashOf :: HashRef -> ExceptT String (DB IO) TxHash
walletTxHashOf named = fullHashOf (noTransaction named) walletTxsNamed withShortId named
  where
    withShortId short = (<>) <$> pendingHashesWith short <*> lift (walletTxidsWith short)

-- | The answer to a hash that names no transaction of the wallet
noTransaction :: HashRef -> String
noTransaction named = "The wallet has no transaction " <> hashRefText named

-- | The coin a command names, by its txid in full or by its short id, and
-- its index, among the coins of every account: what lockcoin, unlockcoin
-- and --coin take
walletCoinOf :: CoinRef -> ExceptT String (DB IO) OutPoint
walletCoinOf named = coinNamed ("The wallet has no coin " <> coinRefText named) named

-- | The coin a command names, as 'walletCoinOf' finds it, with the answer
-- to a short id that matches no coin
coinNamed :: String -> CoinRef -> ExceptT String (DB IO) OutPoint
coinNamed none = \case
  CoinRef (FullHash hash) index -> return (OutPoint hash index)
  CoinRef (ShortHash short) index -> liftEither . pickCoin none short index =<< lift (coinsWith short index)

{- Addresses -}

cmdReceive :: Ctx -> Config -> Receive -> IO Response
cmdReceive ctx cfg opts =
  runDBResponse cfg $ do
    (accId, _) <- chosenAccount cfg opts.account
    addr <- genExtAddress ctx cfg accId (fromMaybe "" opts.label)
    addressAnswer accId addr

-- | Show what the word names, as its shape tells ('Shown'), or without a
-- word the newest receiving address of the account. Nothing is generated
-- or stored. An address and a coin tell their account, so --account is
-- refused with them rather than ignored: the user who typed it may have
-- meant another account, or another thing to show. A file of accounts
-- tells its accounts too. A path and the newest address are of the
-- account the user chose, as for receive: --network names the network of
-- its default account. With a file, --network is the network of the
-- file. An address, a transaction and a coin are of an account, on the
-- network of the account, so --network is refused with them.
cmdShow :: Ctx -> Config -> Maybe Text -> Maybe Shown -> Maybe Network -> IO Response
cmdShow ctx cfg account shownM netM =
  runDBResponse cfg $ case shownM of
    Just (ShownFile file) -> shownFile ctx cfg account netM file
    Nothing -> newestReceiving cfg choice
    Just (ShownPath addrType index) -> addressAtPath ctx cfg choice addrType index
    Just (ShownAddress typed) -> do
      refuseAccount "An address"
      refuseNetwork "an address"
      shownAddress typed
    Just (ShownTx named) -> do
      refuseNetwork "a transaction"
      shownTxs ctx account named
    Just (ShownCoin named) -> do
      refuseAccount "A coin"
      refuseNetwork "a coin"
      (acc, coin) <- walletCoinShown =<< walletCoinOf named
      return $ ResponseCoins acc [coin]
  where
    choice = AccountChoice account netM
    refuseAccount what =
      when (isJust account) $
        throwError (what <> " tells its account, so show takes no --account with it")
    refuseNetwork what =
      when (isJust netM) $
        throwError ("show takes no --network with " <> what <> ", which is on the network of its account")

-- | The newest receiving address of the account, the first one addresses
-- lists, to give out again
newestReceiving :: Config -> AccountChoice -> ExceptT String (DB IO) Response
newestReceiving cfg account = do
  (accId, _) <- chosenAccount cfg account
  newest <- lift $ addressesAfter accId AddrExternal Nothing (Just 1)
  case newest of
    addr : _ -> addressAnswer accId addr
    [] -> throwError "The account has no receiving address yet: receive gives out the first one"

-- | The address of the account at an index of one of its chains, derived
-- from the public key of the account: what an offline computer shows to
-- check an address the online computer gave out. The address shown is
-- always the one the key derives. When the wallet holds it, it is shown
-- as receive shows one, with its label, its balance and its QR code. When
-- the wallet does not, it is shown cut, with its middle hidden and
-- without a code ("Haskoin.Wallet.CutAddress"). receive did not give it
-- out, and it may lie past the gap that a restored wallet looks at, so
-- nobody must be able to pay it. Its ends are enough to compare it with
-- the address the online computer shows.
addressAtPath :: Ctx -> Config -> AccountChoice -> AddrType -> KeyIndex -> ExceptT String (DB IO) Response
addressAtPath ctx cfg account addrType index = do
  (accId, acc) <- chosenAccount cfg account
  let path = addrDeriv addrType :/ index
  addrM <- liftIO $ deriveChainAddress ctx (accountKind acc) (accountXPubKey ctx acc) (addrDeriv addrType) index
  -- BIP32 says to skip an index whose child key is invalid, so the chain
  -- has no address there: see "Haskoin.Wallet.Database.Address"
  addr <- liftMaybe ("There is no address at " <> cs (pathText path) <> ": the key at that index is invalid, and the chain skips it") addrM
  -- The row is looked up by the address derived, not by its path, so that
  -- the answer shows what the key derives in either case
  held <- lift $ storedAmong [addr] (accId, acc)
  case held of
    row : _ -> addressAnswer accId row
    [] -> ResponseDerivedAddr acc path <$> liftMaybe "An account derives no address of this kind" (cutAddress (accountNetwork acc) addr)

-- | An address of the wallet, however it is written, to give out again.
-- The address tells its account. One the wallet does not hold is refused,
-- so that its QR code never pays anyone else.
shownAddress :: Text -> ExceptT String (DB IO) Response
shownAddress typed = do
  found <- lift $ findAddressByText typed
  case found of
    Just (_, addr) -> addressAnswer (dBAddressAccount addr) addr
    Nothing -> throwError ("The wallet has no address " <> cs typed)

-- | A file, a transaction file or a file of accounts, as its bytes tell
-- ('txFileForm'). The wallet keeps nothing of either.
--
-- show takes for a file any word of no other shape ('readShown'). A word
-- with no slash that names no file may have been meant as something else,
-- a short id with a digit missing for instance. The answer then names the
-- five kinds show takes. A word with a slash is a file path, and gets the
-- answer of any command that reads a missing file.
shownFile :: Ctx -> Config -> Maybe Text -> Maybe Network -> FilePath -> ExceptT String (DB IO) Response
shownFile ctx cfg account givenM file = do
  exists <- liftIO $ doesPathExist file
  unless (exists || '/' `elem` file) $
    throwError $
      file
        <> " is not a path, an address, a transaction or a coin, and no file has that name: show takes a path such as /0/3,"
        <> " an address of the wallet, a txid or a nosigHash, a coin TXID:INDEX, or a file of a transaction or of accounts,"
        <> " each hash in full or by its short id, its first 8 digits"
  bytes <- liftEitherIO $ readFileBytes file
  case txFileForm bytes of
    Just _ -> shownTxFile ctx cfg account givenM file bytes
    Nothing -> do
      when (isJust account) $
        throwError "A file of accounts tells its accounts, so show takes no --account with it"
      shownAccountFile ctx cfg givenM file bytes

-- | The transaction of a file, as the account it involves sees it. One that
-- involves no account is shown as outside the wallet, on the network of
-- --network or of the configuration when the file names none. The labels
-- shown are those the file carries, not those of this wallet, since the
-- file is what an offline computer has.
shownTxFile :: Ctx -> Config -> Maybe Text -> Maybe Network -> FilePath -> BS.ByteString -> ExceptT String (DB IO) Response
shownTxFile ctx cfg account givenM file bytes = do
  TxFileRead txFile@(TxFile netM tsd _ _) _ <- txFileOfBytes ctx (commandNetwork cfg givenM) file bytes
  named <- fileNetwork givenM netM
  (accountM, completed) <- transactionAccount ctx account named tsd
  case accountM of
    Just (_, acc) -> ResponseTx acc <$> liftEither (parseTxFile (accountNetwork acc) ctx (accountKind acc) (accountXPubKey ctx acc) txFile {txFileData = completed})
    Nothing -> do
      net <- liftMaybe noFileNetwork (commandNetwork cfg named)
      ResponseOutsideTx net <$> liftEither (parseOutsideTxSignData net ctx tsd)

-- | A file of accounts, as importacc reads it, which importacc would add
-- to the wallet: each account as importacc would take it, on the network
-- of --network or of the configuration when the file names none, with
-- its labels and locks ("Haskoin.Wallet.Labels"). The file is refused
-- when it holds nothing a file of accounts holds. Its bytes are in no form
-- of a transaction file either, so the refusal names both kinds.
shownAccountFile :: Ctx -> Config -> Maybe Network -> FilePath -> BS.ByteString -> ExceptT String (DB IO) Response
shownAccountFile ctx cfg givenM file bytes = do
  content <- either (const (throwError neither)) return (decodeUtf8' bytes)
  accountFile <- liftEither $ first (couldNotRead file) (readAccountFile ctx content)
  unless (holdsAnything accountFile) $ throwError neither
  ResponseAccountFile <$> fileShown ctx (commandNetwork cfg givenM) accountFile
  where
    neither = couldNotRead file "Neither a transaction file, a PSBT in bytes or in base64 text or the hexadecimal of a transaction, nor a file of accounts that importacc reads"

-- | A transaction of the wallet, pending or on the chain, as each account
-- that holds it sees it, or as the account --account names sees it. The
-- short id is matched among the transactions of every account, as label
-- matches it.
shownTxs :: Ctx -> Maybe Text -> HashRef -> ExceptT String (DB IO) Response
shownTxs ctx account named = do
  askedM <- forM account getAccountByName
  hash <- walletTxHashOf named
  views <- transactionViews ctx hash
  kept <- case askedM of
    Nothing -> return views
    Just (askedId, asked) ->
      case [view | view@(heldId, _, _) <- views, heldId == askedId] of
        [] -> throwError ("The transaction " <> hashRefText named <> " is not in the account " <> cs (dBAccountName asked))
        mine -> return mine
  return $ ResponseHeldTxs [(acc, info) | (_, acc, info) <- kept]

cmdAddresses :: Config -> AccountChoice -> Bool -> Paging Text -> IO Response
cmdAddresses cfg account internal paging = readListing cfg (addressesListing cfg account internal paging)

-- | The addresses of a chain of the account, newest first, each with the
-- balance it can spend now. An address names the one to go on after.
addressesListing :: Config -> AccountChoice -> Bool -> Paging Text -> ExceptT String (DB IO) Listing
addressesListing cfg account internal paging = do
  (accId, acc) <- chosenAccount cfg account
  let net = accountNetwork acc
      addrType = if internal then AddrInternal else AddrExternal
  start <- forM paging.pagingAfter $ fmap dBAddressIndex . listedAddress net accId addrType
  return
    Listing
      { listingStart = start,
        listingMax = paging.pagingMax,
        listingRows = \afterM countM -> do
          addrs <- lift $ addressesAfter accId addrType afterM countM
          balances <- spendableAtAddresses net accId (dBAddressAddress <$> addrs)
          return [(addr, spendableAt balances addr) | addr <- addrs],
        listingCursor = Right . dBAddressIndex . fst,
        listingAnswer = ResponseAddresses acc,
        listingWhole = False
      }

cmdBalances :: Config -> AccountChoice -> Paging Text -> IO Response
cmdBalances cfg account paging = readListing cfg (balancesListing cfg account paging)

-- | The addresses of the account that hold coins, receiving and change
-- addresses alike, each with the balance of its coins. An address names
-- the one to go on after, with the balance it holds now.
balancesListing :: Config -> AccountChoice -> Paging Text -> ExceptT String (DB IO) Listing
balancesListing cfg account paging = do
  (accId, acc) <- chosenAccount cfg account
  let net = accountNetwork acc
  start <- forM paging.pagingAfter $ \text -> do
    _ <- liftEither $ textToAddrE net text
    held <- coinBalancesAt net accId [text]
    case held of
      [row] -> return row
      _ -> throwError $ "The address " <> cs text <> " holds no coins in the account"
  return
    Listing
      { listingStart = start,
        listingMax = paging.pagingMax,
        listingRows = coinBalancesAfter net accId,
        listingCursor = Right,
        listingAnswer = ResponseBalances acc,
        listingWhole = False
      }

-- | Label a receiving address, or a transaction: see 'labelTransaction'
cmdLabel :: Ctx -> Config -> Label -> IO Response
cmdLabel ctx cfg opts =
  runDBResponse cfg $
    case opts.labelled of
      LabelledAddress typed -> do
        addr <- labelAddress typed opts.label
        addressAnswer (dBAddressAccount addr) addr
      LabelledTx named -> do
        hash <- walletTxHashOf named
        ResponseLabelledTxs <$> labelTransaction ctx hash opts.label

{- Coins -}

cmdCoins :: Config -> AccountChoice -> Bool -> Paging CoinRef -> IO Response
cmdCoins cfg account locked paging = readListing cfg (coinsListing cfg account locked paging)

-- | The coins of the account, or its locked ones, in the order coins
-- lists them. A coin names the one to go on after, with its block.
coinsListing :: Config -> AccountChoice -> Bool -> Paging CoinRef -> ExceptT String (DB IO) Listing
coinsListing cfg account locked paging = do
  (accId, acc) <- chosenAccount cfg account
  let shown = if locked then LockedCoins else AllCoins
  start <- forM paging.pagingAfter $ \named -> do
    outpoint <- coinNamed ("The coin " <> coinRefText named <> " is not in the account") named
    (,outpoint) <$> coinBlock accId outpoint
  return
    Listing
      { listingStart = start,
        listingMax = paging.pagingMax,
        listingRows = coinsAfter (accountNetwork acc) accId shown,
        listingCursor = \coin -> Right (jsonCoinBlock coin, jsonCoinOutpoint coin),
        listingAnswer = ResponseCoins acc,
        listingWhole = False
      }

-- | Lock a coin at the user's request, and show it
cmdLockCoin :: Config -> CoinRef -> IO Response
cmdLockCoin cfg named =
  runDBResponse cfg $ do
    (acc, coin) <- lockCoin =<< walletCoinOf named
    return $ ResponseCoins acc [coin]

-- | Unlock a coin the user locked, and show it
cmdUnlockCoin :: Config -> CoinRef -> IO Response
cmdUnlockCoin cfg named =
  runDBResponse cfg $ do
    (acc, coin) <- unlockCoin =<< walletCoinOf named
    return $ ResponseCoins acc [coin]

{- Transactions -}

cmdTxs :: Config -> AccountChoice -> Paging HashRef -> IO Response
cmdTxs cfg account paging = readListing cfg (txsListing cfg account paging)

-- | The transactions of the account on the chain, in the order txs lists
-- them, each with the balance after it. A transaction names the one to go
-- on after, with its block and the balance so far, so that a chunk adds up
-- nothing. Only --after adds up the transactions above it.
txsListing :: Config -> AccountChoice -> Paging HashRef -> ExceptT String (DB IO) Listing
txsListing cfg account paging = do
  (accId, acc) <- chosenAccount cfg account
  start <- forM paging.pagingAfter (txsCursorAt accId acc <=< chainTxidOf)
  return
    Listing
      { listingStart = start,
        listingMax = paging.pagingMax,
        listingRows = listedTxsAfter accId,
        listingCursor = \(listed, before) -> case txInfoStatus (listedTxInfo listed) of
          OnChain chain -> Right (TxsCursor (chainBlock chain) (chainTxid chain) before)
          Pending _ -> Left "txs lists transactions of the chain only",
        listingAnswer = ResponseTxs acc . map fst,
        listingWhole = False
      }

cmdPendingTxs :: Ctx -> Config -> Maybe Text -> Paging HashRef -> IO Response
cmdPendingTxs ctx cfg account paging = readListing cfg (pendingTxsListing ctx account paging)

-- | The pending transactions of the account, or of every account, newest
-- first. The one to go on after is named by either of its hashes. The
-- listing goes on after the time it was created and its nosigHash.
pendingTxsListing :: Ctx -> Maybe Text -> Paging HashRef -> ExceptT String (DB IO) Listing
pendingTxsListing ctx account paging = do
  asked <- forM account getAccountByName
  let accIdM = fst <$> asked
  start <- forM paging.pagingAfter (pendingCursor accIdM <=< pendingHashOf)
  return
    Listing
      { listingStart = start,
        listingMax = paging.pagingMax,
        listingRows = pendingAfter ctx accIdM,
        listingCursor = \(_, info) -> case txInfoStatus info of
          Pending pending | Just created <- pendingCreated pending -> Right (created, pendingNosigHash pending)
          _ -> Left "pendingtxs lists the transactions the wallet holds only",
        listingAnswer = ResponsePendingTxs (snd <$> asked),
        -- A few transactions, read whole: the table of them sizes its
        -- column of accounts to the longest name
        listingWhole = True
      }

-- | Add the transaction of a file to the pending transactions of its
-- account, and show it as the wallet holds it from now on: with the labels
-- of the wallet, not those of the file
cmdImportTx :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdImportTx ctx cfg account file =
  runDBResponse cfg $ do
    TxFileRead (TxFile netM tsd _ _) _ <- readTxFileIn ctx (commandNetwork cfg Nothing) file
    (accountM, completed) <- transactionAccount ctx account netM tsd
    (accId, acc) <- liftMaybe notInWallet accountM
    pendingHash <- importPendingTx (accountNetwork acc) ctx accId completed
    txAnswer accId =<< pendingSummary ctx acc =<< requirePendingTx pendingHash

-- | Write a pending transaction to a file, with the labels of its
-- addresses as the wallet has them now
cmdExportTx :: Ctx -> Config -> HashRef -> FilePath -> IO Response
cmdExportTx ctx cfg named file =
  runDBResponse cfg $ do
    held <- requirePendingTx =<< pendingHashOf named
    checkPathFree file
    acc <- getAccountById held.pendingTxAccount
    info <- txInfoOf ctx (held.pendingTxAccount, acc) held.pendingTxData
    writeTxFile ctx (Just acc) file (labelledTxFile (accountNetwork acc) info held.pendingTxData)
    return $ ResponseFile file

-- | Delete a pending transaction, named by either of its hashes. The
-- answer gives both, whichever the user typed.
cmdDeleteTx :: Ctx -> Config -> HashRef -> IO Response
cmdDeleteTx ctx cfg named =
  runDBResponse cfg $ do
    held <- requirePendingTx =<< pendingHashOf named
    let tx = txSignDataTx held.pendingTxData
        txidM = if isSigned tx then Just (txHash tx) else Nothing
    (coins, addrs) <- deleteTx ctx (webStore ctx cfg) (nosigHash tx)
    return $ ResponseDeleteTx (nosigHash tx) txidM coins addrs

{- Payments: preparing, signing and sending -}

-- Sweeps are prepared and signed like payments, with the helpers here.

-- | The refusal of a transaction that no account of the wallet can keep or
-- sign
notInWallet :: String
notInWallet = "The transaction does not involve this wallet"

-- | The keys of an account, as the derivation records of its files name
-- them
fileAccountOf :: Ctx -> DBAccount -> FileAccount
fileAccountOf ctx acc = FileAccount (accountXPubKey ctx acc) (accountKind acc) (accountPath acc) (accountFingerprint acc) (accountNetwork acc)

-- | A transaction file as read: the transaction file, and the PSBT it was
-- read from when it was one, for the signed transaction to be written
-- into it
data TxFileRead = TxFileRead !TxFile !(Maybe Psbt)

-- | The transaction file at the path. A PSBT, in bytes or in base64 text,
-- is read with its paths relative to the account of the wallet its
-- derivation records name (see "Haskoin.Wallet.Tx.PsbtFile"). A
-- transaction alone, in hexadecimal, as Bitcoin Core's finalizepsbt gives
-- it, is read with the signing data the wallet holds for it, which it
-- must. Anything else is refused.
--
-- The network given is the one of the command, from --network or from
-- the configuration, if any. It says how the signatures another signer
-- left in a PSBT are verified when neither the file nor an account of
-- the wallet names a network ('psbtTxFile').
readTxFileIn :: Ctx -> Maybe Network -> FilePath -> ExceptT String (DB IO) TxFileRead
readTxFileIn ctx fallbackM file = do
  bytes <- liftEitherIO $ readFileBytes file
  txFileOfBytes ctx fallbackM file bytes

-- | The forms a transaction file takes, as its first bytes tell them
data TxFileForm
  = -- | A PSBT in bytes, as BIP 174 writes it
    PsbtBytes
  | -- | A PSBT in base64 text
    PsbtText
  | -- | A transaction alone, in hexadecimal
    HexTransaction

-- | The form of a transaction file the bytes are in, if any. The spaces
-- and line breaks around a text do not count. A file of accounts is in
-- none of these forms: it is JSON, a descriptor or a key, none of which
-- is all hexadecimal digits.
txFileForm :: BS.ByteString -> Maybe TxFileForm
txFileForm bytes
  | isPsbt bytes = Just PsbtBytes
  | base64Magic `BS.isPrefixOf` text = Just PsbtText
  | isHexText = Just HexTransaction
  | otherwise = Nothing
  where
    text = C8.strip bytes
    -- "psbt" and 0xff in base64: what the text of any PSBT starts with
    base64Magic = "cHNidP"
    isHexText = not (BS.null text) && even (BS.length text) && C8.all isHexDigit text

-- | The transaction file of the bytes of a file, read as 'readTxFileIn'
-- reads it
txFileOfBytes :: Ctx -> Maybe Network -> FilePath -> BS.ByteString -> ExceptT String (DB IO) TxFileRead
txFileOfBytes ctx fallbackM file bytes =
  case txFileForm bytes of
    Just PsbtBytes -> fromPsbt =<< refused (decodePsbt bytes)
    Just PsbtText -> fromPsbt =<< refused (decodePsbtBase64 stripped)
    Just HexTransaction -> fromRawTx =<< refused (rawTransaction stripped)
    Nothing -> throwError (couldNotRead file "Not a PSBT, in bytes or in base64 text, nor the hexadecimal of a transaction")
  where
    -- The text without the spaces and line breaks around it
    stripped = C8.strip bytes
    refused :: Either String a -> ExceptT String (DB IO) a
    refused = liftEither . first (couldNotRead file)
    fromPsbt psbt = do
      accounts <- lift getAccounts
      txFile <- refused $ psbtTxFile ctx fallbackM (fileAccountOf ctx . snd <$> accounts) psbt
      return $ TxFileRead txFile (Just psbt)
    -- The transaction a hexadecimal text holds, which must be the whole
    -- text: haskoin-core's decodeTx refuses bytes after the transaction,
    -- as Bitcoin Core does. The words of the parser name its internals,
    -- so the refusal is hw's own.
    rawTransaction :: BS.ByteString -> Either String Tx
    rawTransaction text =
      maybe (Left notOneTransaction) (first (const notOneTransaction) . decodeTx) (decodeHex (cs text))
    notOneTransaction = "The hexadecimal text is not one whole transaction"
    fromRawTx tx = do
      heldM <- lookupPendingTx (nosigHash tx)
      held <- liftMaybe "The file holds a transaction alone, which the wallet does not hold as pending: hw needs the whole transaction each of its coins comes from, which a PSBT carries" heldM
      let tsd = held.pendingTxData
      return $ TxFileRead (plainTxFile Nothing tsd {txSignDataTx = tx}) Nothing

-- | The network a file is on, when something names it: the file itself,
-- or --network, which may only repeat what the file names
fileNetwork :: Maybe Network -> Maybe Network -> ExceptT String (DB IO) (Maybe Network)
fileNetwork givenM fileM = case (givenM, fileM) of
  (Just given, Just named)
    | given /= named -> throwError $ "The file is on " <> named.name <> ", not on the --network " <> given.name
  _ -> return (fileM <|> givenM)

-- | Write a transaction file as a PSBT, with the keys of its account when
-- it has one, at a path that must be free. The user is told, on stderr,
-- when the file names the keys without the fingerprint of the seed, which
-- a hardware wallet needs to find its keys: the wallet does not know it
-- yet.
writeTxFile :: Ctx -> Maybe DBAccount -> FilePath -> TxFile -> ExceptT String (DB IO) ()
writeTxFile ctx accM file txFile = do
  psbt <- liftEither $ txFilePsbt ctx (fileAccountOf ctx <$> accM) txFile
  liftEitherIO $ writeFileBytes file (encodePsbt psbt)
  for_ accM $ \acc ->
    when (isNothing (accountFingerprint acc) && isNothing (txSignDataFingerprint (txFileData txFile))) $
      liftIO $ hPutStrLn stderr noFingerprintWarning

-- | What the user is told when a file names the keys of the account by
-- the account's own key, as the wallet does not know the fingerprint of
-- the seed
noFingerprintWarning :: String
noFingerprintWarning = "The wallet does not know the fingerprint of the seed, so the file names the keys by the account's own key, which a hardware wallet does not sign. The wallet learns the fingerprint when signtx or signmsg signs with the seed, when importtx or broadcast takes a file signtx signed, or when importacc gives the origin of the account."

-- | Write a transaction of the network to the --output file, when there is
-- one, with the labels of its addresses as its summary has them, and the
-- keys of its account when it has one
writeOutput :: Ctx -> Maybe DBAccount -> Network -> Maybe FilePath -> TxInfo -> TxSignData -> ExceptT String (DB IO) ()
writeOutput ctx accM net output info tsd = for_ output $ \file -> writeTxFile ctx accM file (labelledTxFile net info tsd)

-- | The signing data of a transaction just prepared, with the label of
-- --label, if given
withLabel :: Maybe Text -> TxSignData -> TxSignData
withLabel labelM tsd = tsd {txSignDataLabel = fromMaybe noLabel labelM}

-- | Keep a transaction of the account just prepared: among its pending
-- transactions, and in the --output file when there is one. The answer
-- shows it.
keepPrepared :: Ctx -> DBAccountId -> DBAccount -> Maybe FilePath -> TxSignData -> ExceptT String (DB IO) Response
keepPrepared ctx accId acc output tsd = txAnswer accId =<< storePrepared ctx accId acc output tsd

-- | Keep a transaction of the account just prepared, as 'keepPrepared'
-- does, and give its summary
storePrepared :: Ctx -> DBAccountId -> DBAccount -> Maybe FilePath -> TxSignData -> ExceptT String (DB IO) TxInfo
storePrepared ctx accId acc output tsd = do
  info <- txInfoOf ctx (accId, acc) tsd
  _ <- importPendingTx (accountNetwork acc) ctx accId tsd
  writeOutput ctx (Just acc) (accountNetwork acc) output info tsd
  return info

-- | Ask before preparing a transaction at a fee rate above 'highFeeRate',
-- unless --yes: a rate that high is more often a typo than a choice. A fee
-- rate no transaction can be built with is refused first, without a
-- question.
confirmFeeRate :: Bool -> Natural -> ExceptT String (DB IO) ()
confirmFeeRate yes feeByte = do
  liftEither $ checkFeeRate feeByte
  when (feeByte > highFeeRate && not yes) $ do
    accepted <-
      askYes terminal $
        "The fee rate of " <> show feeByte <> " satoshis per virtual byte is above " <> show highFeeRate
          <> ", the most hw takes without asking. Prepare the transaction anyway?"
    unless accepted $ throwError "The transaction was not prepared: give a lower --fee-rate"

-- | Whether the wallet is behind the chain on the network: its best block,
-- as the last sync left it, was mined more than eight hours ago
-- ('isStale'), or at a time the wallet does not know. Left what to tell
-- the user then, since a sync may also find coins spent or received since.
-- Right the height of the best block otherwise, which the lock time of a
-- new transaction is drawn from ('lockTimeFor'), or none before the first
-- sync: a wallet that never synced has no coins to spend, and no height to
-- be behind.
behindTheChain :: Network -> ExceptT String (DB IO) (Either String (Maybe BlockHeight))
behindTheChain net = do
  bestM <- getBest net
  now <- liftIO getCurrentTime
  return $ case bestM of
    Nothing -> Right Nothing
    Just best
      | Just time <- best.blockTime,
        not (isStale now (Just time)) ->
          Right (Just best.blockHeight)
      | Just time <- best.blockTime ->
          Left $
            "The wallet is behind the chain: its best block, " <> show best.blockHeight <> ", was mined "
              <> agoText now (posixSecondsToUTCTime (fromIntegral time)) <> ", and a sync may find coins spent or received since."
      | otherwise ->
          Left $
            "The wallet may be behind the chain: it does not know when its best block, " <> show best.blockHeight
              <> ", was mined, and a sync may find coins spent or received since."

-- | Ask whether to prepare a transaction while the wallet is behind the
-- chain, told why, unless --yes. The transaction then gets a lock time of
-- 0, as Bitcoin Core and Electrum give one then. A no stops the command:
-- the alternative is to sync first.
confirmBehind :: Bool -> String -> ExceptT String (DB IO) ()
confirmBehind yes why =
  unless yes $ do
    agreed <- askYes terminal $ why <> " Prepare the transaction anyway, with a lock time of 0?"
    unless agreed $ throwError "The transaction was not prepared: run sync first"

-- | The height the lock time of a new transaction is drawn from, which
-- 'behindTheChain' gives: none when the wallet is behind the chain and the
-- user agrees to go on ('confirmBehind'), for a lock time of 0
lockTimeHeight :: Bool -> Network -> ExceptT String (DB IO) (Maybe BlockHeight)
lockTimeHeight yes net = do
  behind <- behindTheChain net
  case behind of
    Left why -> confirmBehind yes why >> return Nothing
    Right heightM -> return heightM

-- | The fee rate of a transaction, in satoshis per virtual byte: the
-- number given, or one of the rates recent blocks paid, read from the
-- store, at the level given. Without either, hw shows those rates and
-- asks, taking a level or a number as --fee-rate does, the median by
-- default. A rate below the lowest the transaction can pay, which a
-- replacement has, is not offered, and a number below it is asked again.
-- The rates and the question go to stderr, to keep the --json answer
-- clean. hw never guesses a rate: without a --fee-rate, a store that
-- cannot be reached stops the command.
chooseFeeRate :: Config -> Network -> Store -> Natural -> Maybe FeeRateChoice -> ExceptT String (DB IO) Natural
chooseFeeRate _ _ _ _ (Just (FeeRateOf rate)) = return rate
chooseFeeRate cfg net store lowest choice = do
  rates <- fromStore $ withExceptT (<> ". Give a --fee-rate to go on without the rates recent blocks paid") $ recentFeeRates (configParallelRequests cfg) store
  case choice of
    Just (FeeLevelOf level) -> do
      let rate = rateAt level rates
      when (rate < lowest) $ throwError (levelTooLow level rate)
      liftIO . tell terminal $ "The " <> cs (levelName level) <> " rate recent blocks paid: " <> show rate <> " satoshis per virtual byte"
      return rate
    _ -> do
      now <- liftIO getCurrentTime
      liftIO $ renderOn stderr (feeRatesDoc net now rates)
      askAnswer terminal (feeRateQuestion lowest rates) (feeRateAnswer lowest rates)
  where
    levelTooLow level rate =
      "The " <> cs (levelName level) <> " rate recent blocks paid, " <> show rate
        <> " satoshis per virtual byte, is below " <> show lowest <> ": give a --fee-rate of at least " <> show lowest

-- | Show the fee rates recent blocks paid on the network, read from the
-- store. No wallet is needed.
cmdFees :: Ctx -> Config -> Maybe Network -> IO Response
cmdFees ctx cfg netM =
  runResponse $ do
    net <- liftMaybe noNetwork (commandNetwork cfg netM)
    rates <- recentFeeRates (configParallelRequests cfg) (webStore ctx cfg net)
    return $ ResponseFees net rates

-- | Keep a transaction of the account just prepared, as 'keepPrepared'
-- does, and answer with the coins it leaves in the account, and why
preparedAnswer :: Ctx -> DBAccountId -> DBAccount -> Maybe FilePath -> TxSignData -> [LeftCoin] -> ExceptT String (DB IO) Response
preparedAnswer ctx accId acc output tsd left = do
  info <- storePrepared ctx accId acc output tsd
  -- The account as the transaction leaves it
  held <- getAccountById accId
  return $ ResponsePrepared held info left

-- | The account a transaction is prepared from, with the addresses that
-- coin control chose, read on its network. The coins of --coin and the
-- addresses of --spend-from name their account, which must be on the
-- --network, if given. Otherwise the account is chosen as for any command
-- that acts on one account: by --account, or as the only one, or as the
-- default account of the network ('chosenAccount'). Each --spend-from
-- address must be one of the account: the address counts, not its text, so
-- a Bitcoin Cash address is found without its prefix, or in its legacy
-- form.
controlledAccount :: Config -> AccountChoice -> [OutPoint] -> [Text] -> ExceptT String (DB IO) (DBAccountId, DBAccount, [Address])
controlledAccount cfg account outpoints spendFrom = do
  byCoin <- coinAccounts outpoints
  byAddress <- accountsHolding (\net -> mapMaybe (textToAddr net) spendFrom)
  (accId, acc) <- case nonEmpty (nub (byCoin <> byAddress)) of
    Nothing -> chosenAccount cfg account
    Just held -> do
      chosen <- accountAmong (\name -> "The coins or addresses given are not in the account " <> cs name) account.namedAccount held
      liftEither $ checkOnNetwork account.namedNetwork (snd chosen)
      return chosen
  let net = accountNetwork acc
  addrs <- forM spendFrom $ \typed -> do
    addr <- liftEither $ textToAddrE net typed
    path <- addressPath net accId addr
    when (isNothing path) $
      throwError $ "The address " <> cs typed <> " is not in the account " <> cs (dBAccountName acc)
    return addr
  return (accId, acc, addrs)

-- | Prepare a payment from the account, unsigned. Coin control spends the
-- coins of --coin and those at the addresses of --spend-from, all of those
-- that can be spent, and only them. With --select, the strategy selects
-- among them instead, as it does among every coin of the account without
-- coin control. The answer lists the coins chosen that cannot be spent, and
-- why. The chosen coins --select does not need are not listed, like the
-- coins a selection leaves without coin control.
cmdPrepareTx :: Ctx -> Config -> AmountUnit -> PrepareTx -> IO Response
cmdPrepareTx ctx cfg unit opts =
  runDBResponse cfg $ do
    coins <- mapM walletCoinOf opts.coins
    (accId, acc, spendFrom) <- controlledAccount cfg opts.account coins opts.spendFrom
    let net = accountNetwork acc
        controlled = not (null coins && null spendFrom)
    when (opts.select && not controlled) $
      throwError "--select needs coins to select among: give --coin or --spend-from"
    -- Before the transaction is built, not after
    for_ opts.output checkPathFree
    recipients <- liftEither $ mapM (readPayment net unit) opts.recipients
    heightM <- lockTimeHeight opts.yes net
    feeByte <- chooseFeeRate cfg net (webStore ctx cfg net) 1 opts.feeRate
    confirmFeeRate opts.yes feeByte
    (control, left) <-
      if not controlled
        then return (NoCoinControl, [])
        else do
          spendable <- spendableCoins net ctx accId opts.minConf
          unspendable <- unspendableCoins net ctx accId opts.minConf
          (chosen, held) <- liftEither $ chosenCoins coins spendFrom spendable unspendable
          let outpoints = (.outpoint) <$> chosen
          labels <- lift $ labelsOfCoins net accId (fst <$> held)
          return (if opts.select then SelectAmongChosen outpoints else SpendChosen outpoints, leftCoins labels [] held [])
    let changeTo
          | fromMaybe (configChangeBack cfg) opts.changeBack = ChangeBack (changeBackTo unit net accId)
          | otherwise = NewChangeAddress
    gen <- liftIO initStdGen
    let (lockTime, buildGen) = lockTimeFor heightM gen
    tsd <- buildTxSignData net ctx cfg buildGen opts.strategy accId recipients feeByte lockTime opts.feePayer opts.minConf control changeTo
    preparedAnswer ctx accId acc opts.output (withLabel opts.label tsd) left

-- | The address the change of a payment goes back to, among the addresses
-- its coins come from: the only one, or the one the user picks. They are
-- listed as balances lists them, the largest total first, and the first is
-- the default. The list and the question go to stderr, to keep the --json
-- answer clean.
changeBackTo :: AmountUnit -> Network -> DBAccountId -> [Address] -> ExceptT String (DB IO) Address
changeBackTo _ _ _ [addr] = return addr
changeBackTo unit net accId addrs = do
  held <- coinBalancesAt net accId =<< liftEither (mapM (addrToTextE net) addrs)
  liftIO $ do
    tell terminal $ "The payment spends coins at " <> show (length held) <> " addresses. Its change can go back to any of them:"
    renderOn stderr (coinBalancesPrinter unit held)
  chosen <- askChoice terminal "Which address gets the change? Type its path" [([pathText (addrPath a), dBAddressAddress a], a) | (a, _) <- held]
  liftEither $ textToAddrE net (dBAddressAddress chosen)

-- | Prepare a transaction sending the coins of the account to the
-- recipients, unsigned, with no change: all the coins it can spend now, or
-- those that coin control chose, as for preparetx. Without coin control,
-- the coins worth less than their fee are spent or left as the user
-- answers, or as --spend-dust or --leave-dust says. The answer lists every
-- coin left in the account, and why. See "Haskoin.Wallet.Tx.SendAll".
cmdPrepareSendAll :: Ctx -> Config -> AmountUnit -> PrepareSendAll -> IO Response
cmdPrepareSendAll ctx cfg unit opts =
  runDBResponse cfg $ do
    coins <- mapM walletCoinOf opts.coins
    (accId, acc, spendFrom) <- controlledAccount cfg opts.account coins opts.spendFrom
    let net = accountNetwork acc
        kind = accountKind acc
        controlled = not (null coins && null spendFrom)
    -- Before the transaction is built, not after
    for_ opts.output checkPathFree
    recipients <- liftEither $ mapM (readRecipient net unit) opts.recipients
    liftEither $ checkSharing recipients
    when (controlled && isJust opts.dustCoins) $
      throwError "The coins chosen with --coin or --spend-from are all sent: give --spend-dust or --leave-dust without them"
    heightM <- lockTimeHeight opts.yes net
    feeByte <- chooseFeeRate cfg net (webStore ctx cfg net) 1 opts.feeRate
    confirmFeeRate opts.yes feeByte
    spendable <- spendableCoins net ctx accId opts.minConf
    unspendable <- unspendableCoins net ctx accId opts.minConf
    labels <- lift $ labelsOfCoins net accId (spendable <> (fst <$> unspendable))
    (sent, left) <-
      if controlled
        then do
          (sent, _) <- liftEither $ chosenCoins coins spendFrom spendable unspendable
          return (sent, leftCoins labels [] unspendable (notSent sent spendable))
        else do
          sent <- coinsWithDust kind labels feeByte recipients (not (null unspendable)) spendable
          return (sent, leftCoins labels (notSent sent spendable) unspendable [])
    gen <- liftIO initStdGen
    let (lockTime, buildGen) = lockTimeFor heightM gen
    tsd <- buildSendAllSignData net ctx buildGen accId sent recipients feeByte lockTime
    preparedAnswer ctx accId acc opts.output (withLabel opts.label tsd) left
  where
    -- The spendable coins the transaction does not send
    notSent sent = filter (`notElem` sent)
    -- The spendable coins, with or without the dust coins. They are left
    -- without a question when spending them too would leave too little for
    -- the recipients, and when the other coins cannot pay the recipients
    -- either, which the builder then tells.
    coinsWithDust :: AccountKind -> Map TxHash Text -> Natural -> [(Address, Share)] -> Bool -> [Store.Unspent] -> ExceptT String (DB IO) [Store.Unspent]
    coinsWithDust kind labels feeByte recipients othersStay spendable
      | null dust || null worth = return spendable
      | otherwise =
          case opts.dustCoins of
            Just SpendDust -> return spendable
            Just LeaveDust -> return worth
            Nothing
              | isLeft (paying spendable) || isLeft (paying worth) -> return worth
              | otherwise -> askAboutDust (dustReport ctx kind feeByte recipients worth dust othersStay)
      where
        (dust, worth) = partition (isDustCoin kind feeByte) spendable
        paying coins = sendAllAmounts ctx kind feeByte coins recipients
        -- On stderr, with the question, to keep the --json answer clean
        askAboutDust :: DustReport -> ExceptT String (DB IO) [Store.Unspent]
        askAboutDust report = do
          liftIO $ renderOn stderr (dustReportDoc unit labels report)
          spending <- askYes terminal (dustQuestion report)
          return $ if spending then spendable else worth

-- | The file a signed transaction is written to, besides the wallet that
-- holds it
data SignedFile
  = -- | None: the pending transaction signed by its nosigHash is enough
    NoSignedFile
  | -- | The --output file, which must be free
    NewFile FilePath
  | -- | The --input file without --output, which the user agreed to
    -- replace: the unsigned transaction is of no use once signed
    ReplacedFile FilePath

-- | The transaction signtx and signsweep sign, as its file holds it, with
-- the network the file names and the labels it carries, the account of the
-- wallet it involves, if any (see 'transactionAccount'), and the file to
-- write it to once signed. A pending transaction given by its nosigHash
-- is as a file that names no network and labels no address. A
-- transaction signed already, or sent, is not signed again; the --output
-- file must be free, and the --input file is replaced only if the user
-- agrees, all before the user types a secret.
transactionToSign ::
  Ctx ->
  -- | The network of the command, for a file that names none ('readTxFileIn')
  Maybe Network ->
  Signing ->
  ExceptT String (DB IO) (Maybe (DBAccountId, DBAccount), TxFileRead, SignedFile)
transactionToSign ctx fallbackM signing = do
  TxFileRead txFile@(TxFile netM tsd _ _) sourceM <- case (signing.nosigHash, signing.input) of
    (Nothing, Nothing) ->
      throwError "Give the NOSIGHASH of a pending transaction, or an --input file"
    (Just _, Just _) ->
      throwError "Can not specify both a NOSIGHASH and an --input file"
    (Just named, _) -> do
      held <- requirePendingTx =<< pendingHashOf named
      when held.pendingTxOnline $
        throwError "The transaction is already online"
      return $ TxFileRead (plainTxFile Nothing held.pendingTxData) Nothing
    (_, Just file) -> readTxFileIn ctx fallbackM file
  when (isSigned (txSignDataTx tsd)) $
    throwError "The transaction is already signed"
  (accountM, completed) <- transactionAccount ctx signing.account netM tsd
  signedFile <- case (signing.input, signing.output) of
    (_, Just file) -> NewFile file <$ checkPathFree file
    (Just file, Nothing) -> do
      replacing <- askYes terminal ("Replace " <> file <> " with the signed transaction? Give an --output file to write it elsewhere.")
      unless replacing $ throwError "The transaction was not signed: give an --output file to write it to"
      return (ReplacedFile file)
    (Nothing, Nothing) -> return NoSignedFile
  return (accountM, TxFileRead txFile {txFileData = completed} sourceM, signedFile)

-- | Show the transaction about to be signed, before the secret is asked
-- for, as show shows it: with the wallet's labels for a pending
-- transaction, those of the file for a file. On stderr, as the questions,
-- to keep the --json answer clean.
showToSign :: Ctx -> AmountUnit -> Signing -> Network -> Maybe (DBAccountId, DBAccount) -> TxFile -> ExceptT String (DB IO) ()
showToSign ctx unit signing net accountM txFile = do
  let tsd = txFileData txFile
  info <- case accountM of
    Just (accId, acc) -> do
      parsed <- liftEither $ parseTxSignData net ctx (accountKind acc) (accountXPubKey ctx acc) tsd
      if isJust signing.nosigHash
        then lift (fillTxInfoLabels net accId parsed)
        else return (labelsFromFile txFile parsed)
    Nothing -> liftEither $ parseOutsideTxSignData net ctx tsd
  doc <- liftIO $ txAnswerDoc net (snd <$> accountM) unit info
  liftIO $ renderOn stderr doc

-- | Write a transaction just signed to its file, if any, with the keys of
-- its account when it has one: into the PSBT the --input file held, which
-- keeps every field of it, or as a new PSBT for a transaction that came
-- from the wallet
writeSigned :: Ctx -> Maybe DBAccount -> Maybe Psbt -> SignedFile -> TxFile -> ExceptT String (DB IO) ()
writeSigned ctx accM sourceM signedFile signed =
  case signedFile of
    NoSignedFile -> return ()
    NewFile file -> write (writeFileBytes file)
    ReplacedFile file -> write (replaceFileBytes file)
  where
    accountM = fileAccountOf ctx <$> accM
    write put = do
      psbt <- liftEither $ case sourceM of
        Just source -> signedPsbt ctx accountM source signed
        Nothing -> txFilePsbt ctx accountM signed
      liftEitherIO $ put (encodePsbt psbt)

-- | Keep a transaction just signed: in place of the pending one when it was
-- given by its nosigHash, and in its file when there is one. The answer
-- shows it with its labels: the wallet's for a pending transaction, and
-- those the --input file carries for a transaction of a file, which go on
-- to the signed file. The file is what an offline computer has.
keepSigned :: Ctx -> Network -> Signing -> SignedFile -> DBAccountId -> TxFileRead -> TxSignData -> TxInfo -> ExceptT String (DB IO) Response
keepSigned ctx net signing signedFile accId (TxFileRead file sourceM) signed info = do
  labelled <-
    if fromWallet
      then lift (fillTxInfoLabels net accId info)
      else return (labelsFromFile file info)
  when fromWallet $ void $ importPendingTx net ctx accId signed
  acc <- getAccountById accId
  writeSigned ctx (Just acc) sourceM signedFile (labelledTxFile net labelled signed)
  txAnswer accId labelled
  where
    fromWallet = isJust signing.nosigHash

-- | Sign a transaction of the account with the key of its mnemonic, or of
-- the shares of its backup. The transaction is shown first, as show
-- shows it, so that the user sees what the mnemonic is typed for: the
-- prompt is the confirmation, and an interrupt stops it.
cmdSignTx :: Ctx -> Config -> AmountUnit -> Signing -> IO Response
cmdSignTx ctx cfg unit signing =
  runDBResponse cfg $ do
    (accountM, file@(TxFileRead txFile _), signedFile) <- transactionToSign ctx (commandNetwork cfg Nothing) signing
    (accId, acc) <- liftMaybe notInWallet accountM
    let net = accountNetwork acc
    showToSign ctx unit signing net (Just (accId, acc)) txFile
    (seed, key) <- askSigningKey terminal (accountKind acc) net ctx (accountIndex acc) (accountXPubKey ctx acc)
    -- The signed data names the seed, for an online wallet to learn it
    let named = (txFileData txFile) {txSignDataFingerprint = Just (seedFingerprint seed)}
    (signed, info) <- liftEither $ signHeldWalletTx net ctx (accountKind acc) (heldChainsOf acc) named key
    lift $ recordFingerprint ctx seed
    keepSigned ctx net signing signedFile accId file signed info

-- | Send a signed transaction: a pending one of the wallet, by its id or
-- its nosigHash, or the one of a file. One the wallet holds goes on the
-- network of its account, which the file or --network may only name; one
-- it does not hold goes on the network its file names, or on that of
-- --network or of the configuration when the file names none. See
-- 'sendSigned'. The user sees the transaction and says whether to send
-- it, unless --yes.
cmdBroadcast :: Ctx -> Config -> AmountUnit -> Maybe HashRef -> Maybe FilePath -> Bool -> Maybe Network -> IO Response
cmdBroadcast ctx cfg unit namedM fileM yes givenM =
  case (namedM, fileM) of
    (Just named, Nothing) -> sending $ (`TxFileRead` Nothing) . plainTxFile Nothing . (.pendingTxData) <$> (requirePendingTx =<< pendingHashOf named)
    (Nothing, Just file) -> sending (readTxFileIn ctx (commandNetwork cfg givenM) file)
    (Nothing, Nothing) -> return $ ResponseError "Give the TXID of a pending transaction or an --input file"
    (Just _, Just _) -> return $ ResponseError "Can not specify both a TXID and an --input file"
  where
    -- The labels of a file are left aside: the transaction is shown as the
    -- wallet holds it
    sending readTx =
      runDBResponse cfg $ do
        TxFileRead (TxFile netM tsd _ _) _ <- readTx
        named <- fileNetwork givenM netM
        sent <- sendSigned ctx (webStore ctx cfg) beforeSending named (commandNetwork cfg named) tsd
        return $ case sent of
          SentPending acc info -> ResponseTx acc info
          SentOutside net txid -> ResponseSentTx net txid
    -- On stderr, with the question, to keep the --json answer clean
    beforeSending net accountM info =
      unless yes $ do
        liftIO $ renderOn stderr =<< txAnswerDoc net accountM unit info
        confirmed <- askYes terminal "Send this transaction to the network? This cannot be undone."
        unless confirmed $ throwError "The transaction was not sent"

-- | Prepare the replacement of a stuck transaction of the account at a
-- higher fee rate, as "Haskoin.Wallet.Tx.BumpFee" builds it. hw asks before
-- it replaces the transactions that spend the change of the stuck one, and
-- before it adds coins of the account, unless --yes. When no coin is
-- added, it asks before the recipients pay out of what they receive, and
-- which of several does, --yes or not. Every refusal comes before any
-- question: the replacement of every way to pay the fee is built first,
-- and asked about after. The answer is that of preparetx.
cmdBumpFee :: Ctx -> Config -> AmountUnit -> BumpFee -> IO Response
cmdBumpFee ctx cfg unit opts =
  runDBResponse cfg $ do
    -- Before anything is asked, not after
    for_ opts.output checkPathFree
    bumped <- walletTxHashOf opts.bumped
    holders <-
      maybe (throwError ("The wallet has no transaction " <> cs (txHashToHex bumped))) return . nonEmpty
        =<< transactionAccounts bumped
    (accId, acc) <- accountAmong (\name -> "The transaction is not in the account " <> cs name) opts.account holders
    let net = accountNetwork acc
        store = webStore ctx cfg net
    unless net.replaceByFee $
      throwError "Bitcoin Cash replaces no transaction: its nodes keep the first one they see"
    stuck <- stuckTransaction ctx store accId acc bumped
    liftEither $ checkReplaceable stuck
    case opts.feeRate of
      Just (FeeRateOf _) -> return ()
      _ ->
        liftIO . tell terminal $
          "The transaction pays " <> show stuck.sentSummary.summaryFeeByte
            <> " satoshis per virtual byte: a replacement must pay at least "
            <> show (minimumRate stuck)
    feeByte <- chooseFeeRate cfg net store (minimumRate stuck) opts.feeRate
    when (feeByte < minimumRate stuck) $
      throwError $
        "The transaction pays " <> show stuck.sentSummary.summaryFeeByte
          <> " sat/vB: give a --fee-rate of at least "
          <> show (minimumRate stuck)
    children <- spendingChildren ctx store accId acc [stuck]
    refuseSpentElsewhere net accId (stuck : children)
    plan <- bumpPlan net ctx accId (accountKind acc) (stuck : children)
    -- A wallet behind the chain is asked about with the other questions,
    -- once nothing can refuse the replacement: it is built for a lock time
    -- of 0 meanwhile
    behind <- behindTheChain net
    gen <- liftIO initStdGen
    let (lockTime, buildGen) = lockTimeFor (either (const Nothing) id behind) gen
    -- The network takes no replacement that adds an unconfirmed coin
    confirmed <- spendableCoins net ctx accId 1
    labels <- lift $ labelsOfCoins net accId confirmed
    candidates <- forM confirmed $ \coin -> (\(addr, path) -> (coin, addr, path)) <$> coinWithPath net accId coin
    offered <- liftEither $ fundings ctx buildGen plan candidates feeByte
    built <- forM offered $ \funding -> do
      tsd <- buildBump ctx cfg net accId buildGen plan funding feeByte lockTime
      info <- txInfoOf ctx (accId, acc) tsd
      liftEither $ checkBumpFee plan info.txInfoSummary
      return (funding, tsd)
    -- The questions, once nothing can refuse the replacement any more. A
    -- change address a replacement took is given back if the answer is no,
    -- with everything else the command did.
    confirmFeeRate opts.yes feeByte
    either (confirmBehind opts.yes) (const (return ())) behind
    unless (null children) $ do
      liftIO $ renderOn stderr (childrenDoc children)
      agreed <- if opts.yes then return True else askYes terminal (childrenQuestion children)
      unless agreed $ throwError "The transaction was not replaced"
    tsd <- agreedFunding net unit labels feeByte opts.yes built
    preparedAnswer ctx accId acc opts.output tsd []

-- | The replacement of the funding the user agrees to, among those built,
-- each told on stderr with what it is about, to keep the --json answer
-- clean. The change needs no agreement. The coins to add are asked about
-- first, with the labels they show, then the recipients: whether the lone
-- one pays, or which of several does, or all of them, a choice --yes does
-- not make. None agreed, the transaction stays as it is.
agreedFunding :: Network -> AmountUnit -> Map TxHash Text -> Natural -> Bool -> [(Funding, TxSignData)] -> ExceptT String (DB IO) TxSignData
agreedFunding net unit labels feeByte yes built =
  case built of
    (ChangePays, tsd) : _ -> return tsd
    _ -> do
      fromCoins <- offerCoins [(added, tsd) | (CoinsAdded added, tsd) <- built]
      fromRecipients <- maybe (offerReductions [(way, tsd) | (RecipientsReduced way, tsd) <- built]) (return . Just) fromCoins
      liftMaybe "The transaction was not replaced" fromRecipients
  where
    agreedTo asked = if yes then return True else askYes terminal asked
    offerCoins [] = return Nothing
    offerCoins ((added, tsd) : _) = do
      liftIO $ renderOn stderr (addedCoinsDoc net unit labels feeByte added)
      agreed <- agreedTo (addedQuestion added)
      return $ if agreed then Just tsd else Nothing
    offerReductions [] = return Nothing
    offerReductions ways@((way, tsd) : _) = do
      liftIO $ renderOn stderr (reducedDoc net unit feeByte (fst <$> ways))
      case way.reductionRecipients of
        [_] -> do
          agreed <- agreedTo (reducedQuestion (fst <$> ways))
          return $ if agreed then Just tsd else Nothing
        -- No is the default: an empty answer keeps the transaction
        _ -> askChoice terminal (reducedQuestion (fst <$> ways)) ((["no", "n"], Nothing) : [(reductionAnswers net w, Just t) | (w, t) <- ways])

{- Sweeps -}

-- | Sweep coins to the destinations: the coins at the given addresses,
-- typed or in a file, at the addresses of the given private keys, and the
-- coins given by outpoint. The keys are not needed here, only to sign: a
-- sweep can be prepared from addresses or coins alone. A sweep that pays an
-- address of the wallet is of the account of that address, on its network,
-- and waits among its pending transactions to be signed by its nosigHash.
-- One that pays no address of the wallet involves no account: hw is then a
-- utility, which writes the sweep to a file, on the --network given, to be
-- signed from there. That --network must be given: the default network of
-- the configuration does not apply. The keys, the coins and the legacy
-- addresses of Bitcoin Cash read the same on Bitcoin, so a wrong guess
-- would sweep the coins of the other chain. A destination is an address,
-- or an address and its amount, as for preparesendall.
cmdPrepareSweep :: Ctx -> Config -> AmountUnit -> PrepareSweep -> IO Response
cmdPrepareSweep ctx cfg unit opts =
  runDBResponse cfg $ do
    -- Before the keys are typed, not after
    for_ opts.output checkPathFree
    -- The account of a destination is found by its address, an amount after
    -- it or not, before the network to read the amounts on is known
    holders <- accountsHolding (\net -> mapMaybe (recipientAddress net) opts.sweepTo)
    case nonEmpty holders of
      Nothing -> do
        for_ opts.account $ \name -> getAccountByName name >> throwError (paysNo name)
        net <-
          liftMaybe
            "Give the --network of a sweep that pays no account of the wallet: the default-network of the configuration file does not apply to it"
            opts.network
        when (isNothing opts.output) $
          throwError "The sweep does not involve this wallet: give an --output file to write it to"
        tsd <- sweepOn net Nothing
        info <- liftEither $ parseOutsideTxSignData net ctx tsd
        writeOutput ctx Nothing net opts.output info tsd
        return $ ResponseOutsideTx net info
      Just paid -> do
        -- The addresses of the wallet it pays name their account
        unless (length (nub (accountNetwork . snd <$> holders)) == 1) $
          throwError "The sweep pays accounts of several networks"
        (accId, acc) <- accountAmong paysNo opts.account paid
        let net = accountNetwork acc
        for_ opts.network $ \given ->
          unless (given == net) $
            throwError $ "The sweep pays an account on " <> net.name <> ", not on the --network " <> given.name
        keepPrepared ctx accId acc opts.output =<< sweepOn net (Just accId)
  where
    paysNo :: Text -> String
    paysNo name = "The sweep pays no address of the account " <> cs name
    -- The sweep on the network, which the account it pays, if any, knows
    sweepOn net accIdM = do
      -- The addresses, the file and the destinations are checked before
      -- the keys are typed, not after
      typed <- liftEither $ mapM (textToAddrE net) opts.sweepFrom
      listed <- maybe (return []) (addressesOfFile net) opts.addressFile
      sweepTo <- liftEither $ mapM (readRecipient net unit) opts.sweepTo
      liftEither $ checkSharing sweepTo
      feeByte <- chooseFeeRate cfg net (webStore ctx cfg net) 1 opts.feeRate
      confirmFeeRate opts.yes feeByte
      keys <- maybe (return []) (readSecKeys terminal net) opts.keys
      found <- fetchSweepCoins net ctx (webStore ctx cfg net) accIdM keys (typed <> listed) opts.coins (fst <$> sweepTo)
      -- The lock time is drawn from the best block of the store, which the
      -- coins come from: a sweep is prepared online, whatever the wallet
      -- knows of the chain. A store behind the chain gives none.
      best <- fromStore $ storeBestBlock (webStore ctx cfg net)
      now <- liftIO getCurrentTime
      gen <- liftIO initStdGen
      let heightM = if isStale now (Just best.header.timestamp) then Nothing else Just best.height
          (lockTime, buildGen) = lockTimeFor heightM gen
      withLabel opts.label <$> liftEither (buildSweepSignData net ctx buildGen found sweepTo feeByte lockTime)
    -- The addresses of the --address-file, read on the network of the
    -- sweep
    addressesOfFile net file = liftEither . parseAddresses net =<< liftEitherIO (readTextFile file)

-- | Sign a sweep with private keys: a pending transaction of the wallet,
-- by its nosigHash, or one in a file. A sweep outside the wallet is signed
-- on the network its file names, or on that of --network or of the
-- configuration when the file names none, from its file to another, or in
-- its place.
cmdSignSweep :: Ctx -> Config -> AmountUnit -> Signing -> Maybe KeySource -> Maybe Network -> IO Response
cmdSignSweep ctx cfg unit signing keysM givenM =
  runDBResponse cfg $ do
    (accountM, file@(TxFileRead txFile@(TxFile netM tsd _ _) sourceM), signedFile) <- transactionToSign ctx (commandNetwork cfg givenM) signing
    case accountM of
      Just (accId, acc) -> do
        let net = accountNetwork acc
        -- Shown before the keys are asked for, as signtx shows it
        showToSign ctx unit signing net accountM txFile
        keys <- keysOn net
        (signed, info) <- liftEither $ signTxWithKeys net ctx tsd (Just (accountKind acc, accountXPubKey ctx acc, heldChainsOf acc)) keys
        keepSigned ctx net signing signedFile accId file signed info
      Nothing -> do
        named <- fileNetwork givenM netM
        net <- liftMaybe noFileNetwork (commandNetwork cfg named)
        showToSign ctx unit signing net Nothing txFile
        keys <- keysOn net
        (signed, info) <- liftEither $ signTxWithKeys net ctx tsd Nothing keys
        -- No address of the sweep is the wallet's: none has a label
        writeSigned ctx Nothing sourceM signedFile (labelledTxFile net info signed)
        return $ ResponseOutsideTx net info
  where
    -- Without a file, the keys are typed: signing cannot do without them
    keysOn net = readSecKeys terminal net (fromMaybe KeysOnTerminal keysM)

{- Sync, messages and backups -}

-- | The question of a scan that reaches 'scanLimit' used addresses in a
-- row, put to the user on the terminal. --yes does not answer it, so that
-- a sync nobody watches does not believe a store that lies: an input that
-- has ended cancels the command instead. The status line makes room for
-- it, and comes back with the next step.
scanQuestion :: StatusLine -> ScanQuestion (DB IO)
scanQuestion line found = do
  liftIO $ eraseStatus line
  askYes terminal (found <> " Keep looking?")

-- | The user on the terminal, watching a sync or a search on the line
terminalWatcher :: StatusLine -> Watcher (DB IO)
terminalWatcher line = Watcher {question = scanQuestion line, status = showStep line}

cmdSync :: Ctx -> Config -> Maybe Text -> Bool -> IO Response
cmdSync ctx cfg account full =
  withStatusLine $ \line ->
    runDBResponse cfg $ do
      accs <- getAccountsByName account
      when (null accs) $ throwError "There are no accounts in the wallet"
      (results, notSynced) <- syncAccounts ctx cfg (webStore ctx cfg) accs full (terminalWatcher line)
      return $ ResponseSync results [(acc, cs failure) | (acc, failure) <- notSynced]

-- | Sign a message with the key of an address of the wallet. The answer is
-- the signed message, or the --output file its armored block went to.
cmdSignMsg :: Ctx -> Config -> SignMsg -> IO Response
cmdSignMsg ctx cfg opts =
  runDBResponse cfg $ do
    -- Before the mnemonic is asked, not after
    for_ opts.output checkPathFree
    (acc, addrDB) <- getAddressByText opts.address
    let net = accountNetwork acc
    addr <- liftEither $ textToAddrE net opts.address
    -- Warned before the mnemonic is asked, so that the user can stop. On
    -- stderr, to keep the armored block and the --json output clean.
    liftIO $ mapM_ (hPutStrLn stderr) $ signingWarnings opts.message
    (seed, accountPrvKey) <- askSigningKey terminal (accountKind acc) net ctx (accountIndex acc) (accountXPubKey ctx acc)
    lift $ recordFingerprint ctx seed
    let addrKey = derivePath ctx (addrPath addrDB) accountPrvKey
    signature <- encodeSignature <$> liftEither (signMessage ctx addr addrKey.key opts.message)
    case opts.output of
      Nothing -> return $ ResponseSignMsg opts.message signature opts.address
      Just file -> do
        liftEitherIO $ writeTextFile file (signedMessageText opts.address signature opts.message)
        return $ ResponseFile file

-- | Check a signed message: whether the key of the address signed it. The
-- wallet is not needed: the address can be anyone's.
cmdVerifyMsg :: Ctx -> Config -> VerifyMsg -> IO Response
cmdVerifyMsg ctx cfg opts =
  runResponse $ do
    (address, signature, message) <- case opts.source of
      Given address signature message -> return (address, signature, message)
      FromFile file -> signedMessageFromFile file
    net <- liftMaybe noNetwork (commandNetwork cfg opts.network)
    addr <- liftEither $ textToAddrE net address
    valid <- liftEither $ verifyMessage ctx addr signature message
    -- A message that is hard to copy exactly may be why the signature does
    -- not check out. On stderr, to keep the --json answer clean.
    unless valid $ liftIO $ mapM_ (hPutStrLn stderr) $ messageWarnings message
    return $ ResponseVerifyMsg message signature address valid

-- | The address, the signature and the message of the output of signmsg
-- saved in a file: its --json answer when the file holds JSON, else its
-- armored block
signedMessageFromFile :: FilePath -> ExceptT String IO (Text, Text, Text)
signedMessageFromFile file = do
  content <- liftEitherIO $ readTextFile file
  if "{" `Text.isPrefixOf` Text.stripStart content
    then do
      answer <- liftEither $ first (couldNotRead file) $ parseEither signMsgAnswer =<< Json.eitherDecodeStrict' (encodeUtf8 content)
      case answer of
        Just signed -> return signed
        Nothing -> throwError $ "The file " <> file <> " holds an answer of hw, but not one of signmsg"
    else liftEither $ first (couldNotRead file) $ readSignedMessageBlock content
  where
    -- The fields of the answer of signmsg alone: the address, the
    -- signature and the message. Another answer is not read further, so
    -- that a file crafted as one has nothing more to be read for.
    signMsgAnswer :: Json.Value -> Parser (Maybe (Text, Text, Text))
    signMsgAnswer = Json.withObject "an answer of hw" $ \o -> do
      kind <- o Json..: "type"
      if kind == ("signmsg" :: Text)
        then (\message signature address -> Just (address, signature, message)) <$> o Json..: "message" <*> o Json..: "signature" <*> o Json..: "address"
        else return Nothing

-- | Copy the database to the given file, which must not exist, or into the
-- backups directory of hw under a name carrying the format version and the
-- time. The database is at the latest format version, since every command
-- migrates it first.
cmdBackup :: Config -> Maybe FilePath -> IO Response
cmdBackup cfg fileM =
  runResponse $ do
    database <- liftIO $ databaseFile cfg
    copy <- case fileM of
      Just file -> do
        checkPathFree file
        liftEitherIO $ copyDatabase database file
        return file
      Nothing -> liftEitherIO $ do
        directory <- backupDirectory
        backupDatabase database directory latestDatabaseVersion
    return $ ResponseFile copy
