{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The commands of hw. Each reads what it needs from the database, the
-- store and the user, and answers with a 'Response'. A command runs in one
-- transaction of the database: one that fails leaves the database as it
-- was, and answers with its error. An answer that shows an account reads it
-- once the command is done, so that it shows what the command changed.
module Haskoin.Wallet.Commands
  ( commandResponse,

    -- * Mnemonics and accounts
    cmdMnemonic,
    cmdCreateAcc,
    cmdRecover,
    cmdTestAcc,
    cmdImportAcc,
    cmdExportAcc,
    cmdRenameAcc,
    cmdDeleteAcc,
    cmdAccounts,

    -- * Addresses
    cmdReceive,
    cmdAddrs,
    cmdLabel,

    -- * Coins
    cmdCoins,
    cmdLockCoin,
    cmdUnlockCoin,

    -- * Transactions
    cmdTxs,
    cmdPendingTxs,
    cmdReviewTx,
    cmdImportTx,
    cmdExportTx,
    cmdDeleteTx,

    -- * Payments: preparing, signing and sending
    cmdPrepareTx,
    cmdSignTx,
    cmdSendTx,

    -- * Sweeps
    cmdPrepareSweep,
    cmdSignSweep,

    -- * Sync, messages, backups and version
    cmdSync,
    cmdSignMsg,
    cmdBackup,
    cmdVersion,
  )
where

import Control.Monad (forM, join, unless, void, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Base64 as B64
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Data.Word (Word64)
import Database.Persist.Sqlite (transactionUndo)
import Haskoin
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.Amounts (AmountUnit, readAmount)
import Haskoin.Wallet.Backup (backupDatabase, backupDirectory, copyDatabase)
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.MessageSigning
import Haskoin.Wallet.Migration (databaseVersion, latestDatabaseVersion)
import Haskoin.Wallet.Parser
import Haskoin.Wallet.PendingTx
import Haskoin.Wallet.Prompt
import Haskoin.Wallet.Response
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.TxBuilding (buildTxSignData)
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (parseOutsideTxSignData, signWalletTx)
import Haskoin.Wallet.TxSweep (buildSweepSignData, fetchSweepCoins, signTxWithKeys)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import qualified Paths_haskoin_wallet as Paths
import System.IO (hPutStrLn, stderr)
import System.Random (initStdGen)

{- Running a command -}

-- | The answer of hw to a command
commandResponse :: Ctx -> Config -> AmountUnit -> Command -> IO Response
commandResponse ctx cfg unit = \case
  -- Mnemonics and accounts
  CommandMnemonic opts -> cmdMnemonic opts
  CommandCreateAcc name net parts -> cmdCreateAcc ctx cfg name net parts
  CommandRecover opts -> cmdRecover ctx cfg opts
  CommandTestAcc account parts -> cmdTestAcc ctx cfg account parts
  CommandImportAcc file -> cmdImportAcc ctx cfg file
  CommandExportAcc account file -> cmdExportAcc ctx cfg account file
  CommandRenameAcc opts -> cmdRenameAcc cfg opts
  CommandDeleteAcc name net path -> cmdDeleteAcc cfg name net path
  CommandAccounts account -> cmdAccounts cfg account
  -- Addresses
  CommandReceive opts -> cmdReceive ctx cfg opts
  CommandAddrs account internal page -> cmdAddrs cfg account internal page
  CommandLabel account index label -> cmdLabel cfg account index label
  -- Coins
  CommandCoins account locked page -> cmdCoins cfg account locked page
  CommandLockCoin outpoint -> cmdLockCoin cfg outpoint
  CommandUnlockCoin outpoint -> cmdUnlockCoin cfg outpoint
  -- Transactions
  CommandTxs account page -> cmdTxs cfg account page
  CommandPendingTxs account page -> cmdPendingTxs ctx cfg account page
  CommandReviewTx account file -> cmdReviewTx ctx cfg account file
  CommandImportTx account file -> cmdImportTx ctx cfg account file
  CommandExportTx nosigH file -> cmdExportTx cfg nosigH file
  CommandDeleteTx nosigH -> cmdDeleteTx ctx cfg nosigH
  CommandPrepareTx opts -> cmdPrepareTx ctx cfg unit opts
  CommandSignTx signing parts -> cmdSignTx ctx cfg signing parts
  CommandSendTx nosigHM fileM -> cmdSendTx ctx cfg nosigHM fileM
  -- Sweeps
  CommandPrepareSweep opts -> cmdPrepareSweep ctx cfg opts
  CommandSignSweep signing keysM -> cmdSignSweep ctx cfg signing keysM
  -- Sync, messages, backups and version
  CommandSync account full -> cmdSync ctx cfg account full
  CommandSignMsg opts -> cmdSignMsg ctx cfg opts
  CommandBackup fileM -> cmdBackup cfg fileM
  CommandVersion -> cmdVersion cfg

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

-- | A new mnemonic, from the entropy of the system, mixed with dice rolls
-- with --dice
cmdMnemonic :: NewMnemonic -> IO Response
cmdMnemonic opts =
  runResponse $ do
    let sources =
          Sources
            { readSystem = systemEntropy,
              askDice = if opts.dice then Just (askDiceEntropy terminal) else Nothing,
              say = tell terminal
            }
    (source, mnemonic, parts) <- genMnemonic sources opts.entropy opts.parts
    return $ ResponseMnemonic source (Text.words mnemonic) (Text.words <$> parts)

cmdCreateAcc :: Ctx -> Config -> Text -> Maybe Network -> Natural -> IO Response
cmdCreateAcc ctx cfg name netM parts =
  runDBResponse cfg $ do
    -- Before the user types the mnemonic, not after
    checkNameFree name
    net <- liftMaybe noNetwork (commandNetwork cfg netM)
    mnem <- askNewMnemonicPass terminal parts
    seed <- liftEither $ seedOf ctx mnem
    (_, xpub) <- lift $ nextAccount net ctx seed
    (accId, _) <- insertAccount net ctx name xpub (Just (seedFingerprint seed))
    -- The accounts of the seed created before hw recorded fingerprints
    lift $ recordFingerprint ctx seed
    accountAnswer accId

-- | Recover the accounts of a seed that were paid on a network, from its
-- mnemonic: see "Haskoin.Wallet.Sync".
cmdRecover :: Ctx -> Config -> Recover -> IO Response
cmdRecover ctx cfg opts =
  runDBResponse cfg $ do
    net <- liftMaybe noNetwork (commandNetwork cfg opts.network)
    let store = webStore ctx cfg net
    -- An offline computer hears it before the mnemonic is typed
    fromStore $ storeHealth store
    -- Typed once: a mistyped mnemonic finds no paid account
    mnem <- askMnemonicPass terminal opts.parts
    seed <- liftEither $ seedOf ctx mnem
    ResponseRecover net opts.accountGap <$> recoverSeed ctx cfg store net seed opts.accountGap

cmdTestAcc :: Ctx -> Config -> Maybe Text -> Natural -> IO Response
cmdTestAcc ctx cfg account parts =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName account
    mnem <- askMnemonicPass terminal parts
    matched <- matchAccount terminal (accountNetwork acc) ctx (accountIndex acc) (accountXPubKey ctx acc) mnem
    -- A seed proven gives its fingerprint to every account of it
    for_ matched $ \(seed, _) -> lift $ recordFingerprint ctx seed
    tested <- getAccountById accId
    return $ case matched of
      Just _ -> ResponseAccResult tested True "The mnemonic and passphrase matched the account"
      Nothing -> ResponseAccResult tested False "The mnemonic and passphrase did not match the account"

cmdImportAcc :: Ctx -> Config -> FilePath -> IO Response
cmdImportAcc ctx cfg file =
  runDBResponse cfg $ do
    PubKeyDoc xpub net name fingerprintM <- liftEitherIO $ readMarshalFile ctx file
    heldM <- lift $ accountWithKey net ctx xpub
    accId <- case (heldM, fingerprintM) of
      -- An account the wallet has already, whose document now names its seed
      (Just (heldId, held), Just fingerprint) | isNothing (dBAccountFingerprint held) -> do
        learnFingerprint heldId fingerprint
        return heldId
      _ -> fst <$> insertAccount net ctx name xpub fingerprintM
    accountAnswer accId

cmdExportAcc :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdExportAcc ctx cfg account file =
  runDBResponse cfg $ do
    (_, acc) <- getAccountByName account
    checkPathFree file
    let doc = PubKeyDoc (accountXPubKey ctx acc) (accountNetwork acc) (dBAccountName acc) (accountFingerprint acc)
    liftEitherIO $ writeMarshalFile ctx file doc
    return $ ResponseFile file

cmdRenameAcc :: Config -> RenameAcc -> IO Response
cmdRenameAcc cfg opts =
  runDBResponse cfg $ do
    _ <- renameAccount opts.old opts.new
    accountAnswer . fst =<< getAccountByName (Just opts.new)

-- | Delete an account, given its network and its path too, which must be
-- those of the account
cmdDeleteAcc :: Config -> Text -> Network -> HardPath -> IO Response
cmdDeleteAcc cfg name net path =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName (Just name)
    unless (accountNetwork acc == net) $
      throwError "The network of the account to delete did not match"
    unless (path == accountPath acc) $
      throwError "The full derivation path of the account to delete did not match"
    lift $ deleteAccount accId
    return $ ResponseAccResult acc True ("The account " <> name <> " has been deleted")

cmdAccounts :: Config -> Maybe Text -> IO Response
cmdAccounts cfg account =
  runDBResponse cfg $ do
    accs <- getAccountsByName account
    ResponseAccounts <$> mapM withSpendable accs

{- Addresses -}

cmdReceive :: Ctx -> Config -> Receive -> IO Response
cmdReceive ctx cfg opts =
  runDBResponse cfg $ do
    (accId, _) <- getAccountByName opts.account
    addr <- genExtAddress ctx cfg accId (fromMaybe "" opts.label)
    addressAnswer accId addr

cmdAddrs :: Config -> Maybe Text -> Bool -> Page -> IO Response
cmdAddrs cfg account internal page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName account
    addrs <- lift $ addressPage accId (if internal then AddrInternal else AddrExternal) page
    balances <- spendableAtAddresses (accountNetwork acc) accId (dBAddressAddress <$> addrs)
    return $ ResponseAddresses acc [(addr, spendableAt balances addr) | addr <- addrs]

cmdLabel :: Config -> Maybe Text -> Int -> Text -> IO Response
cmdLabel cfg account index label =
  runDBResponse cfg $ do
    (accId, _) <- getAccountByName account
    addr <- setAddrLabel accId index label
    addressAnswer accId addr

{- Coins -}

cmdCoins :: Config -> Maybe Text -> Bool -> Page -> IO Response
cmdCoins cfg account locked page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName account
    coins <- coinPage (accountNetwork acc) accId (if locked then LockedCoins else AllCoins) page
    return $ ResponseCoins acc coins

-- | Lock a coin at the user's request, and show it
cmdLockCoin :: Config -> OutPoint -> IO Response
cmdLockCoin cfg outpoint =
  runDBResponse cfg $ do
    (acc, coin) <- lockCoin outpoint
    return $ ResponseCoins acc [coin]

-- | Unlock a coin the user locked, and show it
cmdUnlockCoin :: Config -> OutPoint -> IO Response
cmdUnlockCoin cfg outpoint =
  runDBResponse cfg $ do
    (acc, coin) <- unlockCoin outpoint
    return $ ResponseCoins acc [coin]

{- Transactions -}

cmdTxs :: Config -> Maybe Text -> Page -> IO Response
cmdTxs cfg account page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName account
    ResponseTxs acc <$> txsPage accId page

cmdPendingTxs :: Ctx -> Config -> Maybe Text -> Page -> IO Response
cmdPendingTxs ctx cfg account page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName account
    ResponseTxs acc <$> pendingTxPage ctx accId page

-- | Show the transaction of a file, as the account it involves sees it, or
-- as outside the wallet, without keeping it
cmdReviewTx :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdReviewTx ctx cfg account file =
  runDBResponse cfg $ do
    TxFile netM tsd <- liftEitherIO $ readJsonFile file
    accountM <- transactionAccount ctx account netM tsd
    case accountM of
      Just (_, acc) -> ResponseTx acc <$> txInfoOf ctx acc tsd
      Nothing -> do
        net <- liftMaybe noFileNetwork netM
        ResponseOutsideTx net <$> liftEither (parseOutsideTxSignData net ctx tsd)

cmdImportTx :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdImportTx ctx cfg account file =
  runDBResponse cfg $ do
    TxFile netM tsd <- liftEitherIO $ readJsonFile file
    (accId, acc) <- liftMaybe notInWallet =<< transactionAccount ctx account netM tsd
    info <- txInfoOf ctx acc tsd
    _ <- importPendingTx (accountNetwork acc) ctx accId tsd
    txAnswer accId info

cmdExportTx :: Config -> TxHash -> FilePath -> IO Response
cmdExportTx cfg nosigH file =
  runDBResponse cfg $ do
    held <- requirePendingTx nosigH
    checkPathFree file
    acc <- getAccountById held.pendingTxAccount
    liftEitherIO $ writeJsonFile file (TxFile (Just (accountNetwork acc)) held.pendingTxData)
    return $ ResponseFile file

cmdDeleteTx :: Ctx -> Config -> TxHash -> IO Response
cmdDeleteTx ctx cfg nosigH =
  runDBResponse cfg $ do
    (coins, addrs) <- deleteTx ctx (webStore ctx cfg) nosigH
    return $ ResponseDeleteTx nosigH coins addrs

{- Payments: preparing, signing and sending -}

-- Sweeps are prepared and signed like payments, with the helpers here.

-- | The refusal of a transaction that no account of the wallet can keep or
-- sign
notInWallet :: String
notInWallet = "The transaction does not involve this wallet"

-- | Write a transaction of the network to the --output file, when there is
-- one
writeOutput :: Network -> Maybe FilePath -> TxSignData -> ExceptT String (DB IO) ()
writeOutput net output tsd = for_ output $ \file -> liftEitherIO $ writeJsonFile file (TxFile (Just net) tsd)

-- | Keep a transaction of the account just prepared: among its pending
-- transactions, and in the --output file when there is one. The answer
-- shows it.
keepPrepared :: Ctx -> DBAccountId -> DBAccount -> Maybe FilePath -> TxSignData -> ExceptT String (DB IO) Response
keepPrepared ctx accId acc output tsd = do
  info <- txInfoOf ctx acc tsd
  _ <- importPendingTx (accountNetwork acc) ctx accId tsd
  writeOutput (accountNetwork acc) output tsd
  txAnswer accId info

-- | Prepare a payment from the account, unsigned
cmdPrepareTx :: Ctx -> Config -> AmountUnit -> PrepareTx -> IO Response
cmdPrepareTx ctx cfg unit opts =
  runDBResponse cfg $ do
    -- The coins given with --coin name their account
    holders <- coinAccounts opts.coins
    (accId, acc) <- accountAmong (\name -> "The coins given are not in the account " <> cs name) opts.account holders
    let net = accountNetwork acc
    -- Before the transaction is built, not after
    for_ opts.output checkPathFree
    recipients <- liftEither $ mapM (recipient net) opts.recipients
    gen <- liftIO initStdGen
    tsd <- buildTxSignData net ctx cfg gen opts.strategy accId recipients opts.feeByte opts.dust opts.feePayer opts.minConf opts.coins
    keepPrepared ctx accId acc opts.output tsd
  where
    recipient net (addr, amount) = do
      paid <- textToAddrE net addr
      satoshis <- readAmount unit amount
      return (paid, fromIntegral satoshis)

-- | The transaction signtx and signsweep sign, with the network its file
-- names and the account of the wallet it involves, if any: see
-- 'transactionAccount'. A transaction signed already, or sent, is not
-- signed again, and the --output file must be free, all before the user
-- types a secret.
transactionToSign ::
  Ctx ->
  Signing ->
  ExceptT String (DB IO) (Maybe (DBAccountId, DBAccount), Maybe Network, TxSignData)
transactionToSign ctx signing = do
  TxFile netM tsd <- case (signing.nosigHash, signing.input, signing.output) of
    (Nothing, Nothing, _) ->
      throwError "Give the NOSIGHASH of a pending transaction, or an --input file and an --output file"
    (Just _, Just _, _) ->
      throwError "Can not specify both a NOSIGHASH and an --input file"
    (_, Just _, Nothing) ->
      throwError "Give an --output file with the --input file"
    (Just nosigH, _, _) -> do
      held <- requirePendingTx nosigH
      when held.pendingTxOnline $
        throwError "The transaction is already online"
      return $ TxFile Nothing held.pendingTxData
    (_, Just file, _) -> liftEitherIO $ readJsonFile file
  when (isSigned (txSignDataTx tsd)) $
    throwError "The transaction is already signed"
  accountM <- transactionAccount ctx signing.account netM tsd
  for_ signing.output checkPathFree
  return (accountM, netM, tsd)

-- | Keep a transaction just signed: in place of the pending one when it was
-- given by its nosigHash, and in the --output file when there is one. The
-- answer shows it, with the labels of its addresses.
keepSigned :: Ctx -> Network -> Signing -> DBAccountId -> TxSignData -> TxInfo -> ExceptT String (DB IO) Response
keepSigned ctx net signing accId signed info = do
  when (isJust signing.nosigHash) $ void $ importPendingTx net ctx accId signed
  writeOutput net signing.output signed
  txAnswer accId =<< lift (fillTxInfoLabels net info)

-- | Sign a transaction of the account with the key of its mnemonic
cmdSignTx :: Ctx -> Config -> Signing -> Natural -> IO Response
cmdSignTx ctx cfg signing parts =
  runDBResponse cfg $ do
    (accountM, _, tsd) <- transactionToSign ctx signing
    (accId, acc) <- liftMaybe notInWallet accountM
    let net = accountNetwork acc
    (seed, key) <- askSigningKey terminal net ctx parts (accountIndex acc) (accountXPubKey ctx acc)
    -- The signed data names the seed, for an online wallet to learn it
    let named = tsd {txSignDataFingerprint = Just (seedFingerprint seed)}
    (signed, info) <- liftEither $ signWalletTx net ctx named key
    lift $ recordFingerprint ctx seed
    keepSigned ctx net signing accId signed info

-- | Send a signed transaction: a pending one of the wallet, by its
-- nosigHash, or the one of a file. One the wallet holds goes on the network
-- of its account, which the file may only name; one it does not hold goes
-- on the network its file names. See 'sendSigned'.
cmdSendTx :: Ctx -> Config -> Maybe TxHash -> Maybe FilePath -> IO Response
cmdSendTx ctx cfg nosigHM fileM =
  case (nosigHM, fileM) of
    (Just nosigH, Nothing) -> sending $ TxFile Nothing . (.pendingTxData) <$> requirePendingTx nosigH
    (Nothing, Just file) -> sending $ liftEitherIO (readJsonFile file)
    (Nothing, Nothing) -> return $ ResponseError "Give the NOSIGHASH of a pending transaction or an --input file"
    (Just _, Just _) -> return $ ResponseError "Can not specify both a NOSIGHASH and an --input file"
  where
    sending readTx =
      runDBResponse cfg $ do
        TxFile netM tsd <- readTx
        sent <- sendSigned ctx (webStore ctx cfg) netM tsd
        return $ case sent of
          SentPending acc info -> ResponseTx acc info
          SentOutside net txid -> ResponseSentTx net txid

{- Sweeps -}

-- | Sweep coins to the destinations: the coins at the given addresses and
-- at the addresses of the given private keys, and the coins given by
-- outpoint. The keys are not needed here, only to sign: a sweep can be
-- prepared from addresses or coins alone. A sweep that pays an address of
-- the wallet is of the account of that address, on its network, and waits
-- among its pending transactions to be signed by its nosigHash. One that
-- pays no address of the wallet involves no account: hw is then a utility,
-- which writes the sweep to a file, on the --network given, to be signed
-- from there.
cmdPrepareSweep :: Ctx -> Config -> PrepareSweep -> IO Response
cmdPrepareSweep ctx cfg opts =
  runDBResponse cfg $ do
    -- Before the keys are typed, not after
    for_ opts.output checkPathFree
    holders <- addressAccounts opts.sweepTo
    case holders of
      [] -> do
        for_ opts.account $ \name -> getAccountByName (Just name) >> throwError (paysNo name)
        net <-
          liftMaybe
            "Give the --network of a sweep that pays no account of the wallet, or a default-network in the configuration file"
            (commandNetwork cfg opts.network)
        when (isNothing opts.output) $
          throwError "The sweep does not involve this wallet: give an --output file to write it to"
        tsd <- sweepOn net Nothing
        info <- liftEither $ parseOutsideTxSignData net ctx tsd
        writeOutput net opts.output tsd
        return $ ResponseOutsideTx net info
      _ -> do
        -- The addresses of the wallet it pays name their account
        unless (length (nub (accountNetwork . snd <$> holders)) == 1) $
          throwError "The sweep pays accounts of several networks"
        (accId, acc) <- accountAmong paysNo opts.account holders
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
      sweepFrom <- liftEither $ mapM (textToAddrE net) opts.sweepFrom
      sweepTo <- liftEither $ mapM (textToAddrE net) opts.sweepTo
      keys <- maybe (return []) (readSecKeys terminal net) opts.keys
      found <- fetchSweepCoins net ctx (webStore ctx cfg net) accIdM keys sweepFrom opts.coins sweepTo
      gen <- liftIO initStdGen
      liftEither $ buildSweepSignData net ctx gen found sweepTo opts.feeByte opts.dust

-- | Sign a sweep with private keys: a pending transaction of the wallet,
-- by its nosigHash, or one in a file. A sweep outside the wallet is signed
-- on the network its file names, from its file to another.
cmdSignSweep :: Ctx -> Config -> Signing -> Maybe KeySource -> IO Response
cmdSignSweep ctx cfg signing keysM =
  runDBResponse cfg $ do
    (accountM, netM, tsd) <- transactionToSign ctx signing
    case accountM of
      Just (accId, acc) -> do
        let net = accountNetwork acc
        keys <- keysOn net
        (signed, info) <- liftEither $ signTxWithKeys net ctx tsd (Just (accountXPubKey ctx acc)) keys
        keepSigned ctx net signing accId signed info
      Nothing -> do
        net <- liftMaybe noFileNetwork netM
        keys <- keysOn net
        (signed, info) <- liftEither $ signTxWithKeys net ctx tsd Nothing keys
        writeOutput net signing.output signed
        return $ ResponseOutsideTx net info
  where
    -- Without a file, the keys are typed: signing cannot do without them
    keysOn net = readSecKeys terminal net (fromMaybe KeysOnTerminal keysM)

{- Sync, messages, backups and version -}

cmdSync :: Ctx -> Config -> Maybe Text -> Bool -> IO Response
cmdSync ctx cfg account full =
  runDBResponse cfg $ do
    accs <- getAccountsByName account
    when (null accs) $ throwError "There are no accounts in the wallet"
    results <- forM accs $ \(accId, acc) -> do
      let net = accountNetwork acc
      sync ctx cfg (webStore ctx cfg net) net accId full
    return $ ResponseSync results

-- | Sign a message with the key of an address of the wallet
cmdSignMsg :: Ctx -> Config -> SignMsg -> IO Response
cmdSignMsg ctx cfg opts =
  runDBResponse cfg $ do
    (acc, addrDB) <- getAddressByText opts.address
    let net = accountNetwork acc
    addr <- liftEither $ textToAddrE net opts.address
    -- Warned before the mnemonic is asked, so that the user can stop. On
    -- stderr, to keep the armored block and the --json output clean.
    liftIO $ mapM_ (hPutStrLn stderr) $ messageWarnings opts.message
    (seed, accountPrvKey) <- askSigningKey terminal net ctx opts.parts (accountIndex acc) (accountXPubKey ctx acc)
    lift $ recordFingerprint ctx seed
    let addrKey = derivePath ctx (addrPath addrDB) accountPrvKey
    signature <- liftEither $ signMessage ctx addr addrKey.key opts.message
    return $ ResponseSignMsg opts.message (cs (B64.encode signature)) opts.address

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

cmdVersion :: Config -> IO Response
cmdVersion cfg =
  runDBResponse cfg $ do
    -- Every command migrates the database first, which gives it a version
    dbVersion <- liftMaybe "The database has no format version" =<< databaseVersion
    return $ ResponseVersion (cs (showVersion Paths.version)) (cs (show dbVersion))
