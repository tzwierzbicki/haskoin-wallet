{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The commands of hw. Each reads what it needs from the database, the
-- store and the user, and answers with a 'Response'. A command runs in one
-- transaction of the database: one that fails leaves the database as it
-- was, and answers with its error. send is the one exception: it keeps the
-- transaction it signed before it sends it ('keepSoFar'). An answer that
-- shows a portfolio reads it once the command is done, so that it shows
-- what the command changed.
--
-- A command works in the active portfolio, which the database keeps and
-- switch changes ('requireActivePortfolio'). Inside it, --network names
-- what the portfolio holds on one network, its pool, and --type one of the
-- two accounts of the pool. A listing covers the whole portfolio unless
-- the options narrow it, in a section for each network, or for each
-- account for the addresses ("Haskoin.Wallet.Cli.Listing").
--
-- Some commands find their portfolio by themselves, from what they are
-- given: a transaction, a coin, an address, the destinations of a sweep.
-- When several portfolios are candidates, the active one is taken when it
-- is one of them; otherwise the refusal names them, for the user to
-- switch to the one to use. Their answers carry the active portfolio, so
-- that the screen says when the portfolio shown is another one, whose
-- paths and labels the user might take for those of the active one
-- ("Haskoin.Wallet.Cli.Response"). portfolios, pendingtxs, sync and
-- exportsheet cover every portfolio.
--
-- A command that changes a default writes it to the configuration file
-- inside its transaction, as its last step ('writeDefaults'): when the
-- file cannot be written, the database is left as it was, so that the two
-- never disagree. Only setdefault does. It writes the network, the type,
-- the unit, the view of the listings and the networks hw shows. The
-- active portfolio is no key of the file: it is in the database.
--
-- A portfolio has accounts on eight networks, and the configuration lists
-- those hw shows ("Haskoin.Wallet.Config"). The others are hidden, not
-- disabled. sync and recover work on every network, and their status line
-- names each one. Everything else keeps to the shown networks. A listing
-- has no section for a hidden network, and an answer no line. A command
-- that looks for what the user named, an address or a transaction, looks
-- on the shown networks, so that a hidden network is never one of
-- several to tell apart with --network. What the wallet has on hidden
-- networks alone is refused, with the command that shows its network
-- ('shownAmong'): answering that the wallet does not have it would be
-- untrue. A --network that names a hidden network is refused before the
-- command runs ('refusedNetwork').
module Haskoin.Wallet.Commands
  ( commandResponse,
    commandListing,
    refusedNetwork,
    answerWalletNaming,
    listingWalletNaming,

    -- * Listings
    cmdCsv,
    cmdExportSheet,
    portfoliosListing,
    addressesListing,
    balancesListing,
    coinsListing,
    txsListing,
    pendingTxsListing,

    -- * Wallets and portfolios
    cmdNewPortfolio,
    cmdRecover,
    cmdCheckMnemonic,
    cmdImportWallet,
    cmdExportWallet,
    cmdSwitch,
    cmdSetDefault,
    cmdDeletePortfolio,
    cmdDeleteWallet,

    -- * Addresses
    cmdReceive,
    cmdShow,
    cmdLabel,

    -- * Coins
    cmdLockCoin,
    cmdUnlockCoin,

    -- * Transactions
    cmdImportTx,
    cmdExportTx,
    cmdDeleteTx,

    -- * Payments: preparing, signing and sending
    cmdSend,
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
import Control.Monad (filterM, forM, join, unless, void, when, (<=<))
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError, withExceptT)
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
import Data.List (genericTake, intersect, nub, partition, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Database.Persist.Sqlite (transactionSave, transactionUndo)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit, showAmount, showUnit, unitName)
import Haskoin.Wallet.Backup (backupDatabase, backupDirectory, copyDatabase)
import Haskoin.Wallet.Bip329 (recordsText)
import Haskoin.Wallet.Cli.Csv (sheetCsv)
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Ods (workbookOds)
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.PrettyPrinter (SeenFrom (..), addedCoinsDoc, agoText, childrenDoc, dustReportDoc, feeRatesDoc, pathsSeenFrom, preparedDoc, reducedDoc, renderOn, sharedDoc, txAnswerDoc)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Sheet (PortfolioSheets (..), Sheet, TxSheets, localListingSheet, localTxSheets, stacked, walletSheets)
import Haskoin.Wallet.Cli.View (viewName)
import Haskoin.Wallet.Config
import Haskoin.Wallet.CutAddress (cutAddress)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock (BestBlock (..), getBest)
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.RawTx (dropOrphanRawTxs)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Labels (AccountImport (..), FileShown (..), ImportScope (..), fileShown, holdsAnything, importFile, readWalletFile, walletRecords, wholeFile)
import Haskoin.Wallet.MessageSigning
import Haskoin.Wallet.Migration (latestDatabaseVersion)
import Haskoin.Wallet.Shares (genShares)
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.SyncStatus (StatusLine (..), checkingHealth, withStatusLine)
import Haskoin.Wallet.Tx.Building (ChangeTo (..), CoinControl (..), PreparedPayment (..), buildTxSignData, coinWithPath)
import Haskoin.Wallet.Tx.BumpFee
import Haskoin.Wallet.Tx.CoinSelection (FeePayer (..), coinKind, coinsDoNotCover)
import Haskoin.Wallet.Tx.FeeRates
import Haskoin.Wallet.Tx.LockTime (isStale, lockTimeFor)
import Haskoin.Wallet.Tx.PsbtFile (FileAccount (..), psbtTxFile, signedPsbt, txFilePsbt)
import Haskoin.Wallet.Tx.Recipient (Share, checkSharing, readPayment, readRecipient, recipientAddress)
import Haskoin.Wallet.Tx.SendAll
  ( DustReport (dustCoins),
    buildSendAllSignData,
    noCoinsToSend,
    dustQuestion,
    dustReport,
    isDustCoin,
    sendAllAmounts,
  )
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (heldPoolOf, labelledTxFile, labelsFromFile, noneHeld, parseOutsideTxSignData, parseTxFile, parseTxSignData, signHeldWalletTx)
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
  -- A listing asked whole, as --json asks it
  cmd | Just listing <- commandListing ctx cfg cmd -> readListing cfg listing
  -- Wallets and portfolios
  CommandNewPortfolio opts -> cmdNewPortfolio ctx cfg opts
  CommandRecover opts -> cmdRecover ctx cfg opts
  CommandCheckMnemonic -> cmdCheckMnemonic ctx cfg
  CommandImportWallet scope file -> cmdImportWallet ctx cfg scope file
  CommandExportWallet walletM portfolioM file -> cmdExportWallet ctx cfg walletM portfolioM file
  CommandSwitch walletM index -> cmdSwitch cfg walletM index
  CommandSetDefault opts -> cmdSetDefault cfg opts
  CommandDeletePortfolio indexM -> cmdDeletePortfolio cfg indexM
  CommandDeleteWallet walletM fingerprint -> cmdDeleteWallet cfg walletM fingerprint
  -- Addresses
  CommandReceive opts -> cmdReceive ctx cfg opts
  CommandShow target fileScope shown -> cmdShow ctx cfg target fileScope shown
  CommandLabel opts -> cmdLabel ctx cfg opts
  -- Coins
  CommandLockCoin named netM -> cmdLockCoin cfg named netM
  CommandUnlockCoin named netM -> cmdUnlockCoin cfg named netM
  -- Transactions
  CommandImportTx file -> cmdImportTx ctx cfg file
  CommandExportTx named file -> cmdExportTx ctx cfg named file
  CommandDeleteTx named -> cmdDeleteTx ctx cfg named
  CommandPrepareTx opts -> cmdPrepareTx ctx cfg unit opts
  CommandSend opts -> cmdSend ctx cfg unit opts
  CommandPrepareSendAll opts -> cmdPrepareSendAll ctx cfg unit opts
  CommandSignTx signing -> cmdSignTx ctx cfg unit signing
  CommandBroadcast namedM fileM yes netM -> cmdBroadcast ctx cfg unit namedM fileM yes netM
  CommandBumpFee opts -> cmdBumpFee ctx cfg unit opts
  -- Sweeps
  CommandPrepareSweep opts -> cmdPrepareSweep ctx cfg unit opts
  CommandSignSweep signing keysM netM -> cmdSignSweep ctx cfg unit signing keysM netM
  -- Sync, messages and backups
  CommandSync netM full -> cmdSync ctx cfg netM full
  CommandFees netM -> cmdFees ctx cfg netM
  CommandSignMsg opts -> cmdSignMsg ctx cfg opts
  CommandVerifyMsg opts -> cmdVerifyMsg ctx cfg opts
  CommandBackup fileM -> cmdBackup cfg fileM
  CommandExportSheet file -> cmdExportSheet ctx cfg file
  -- The listings, which the cases above answer
  CommandPortfolios {} -> return notAListing
  CommandAddresses {} -> return notAListing
  CommandBalances {} -> return notAListing
  CommandCoins {} -> return notAListing
  CommandTxs {} -> return notAListing
  CommandPendingTxs {} -> return notAListing
  where
    notAListing = ResponseError "The command has no listing to answer with"

-- | The listing of a command that lists, which the screen reads chunk by
-- chunk, or Nothing for a command that answers at once. portfolios and
-- pendingtxs list every portfolio, and the others list the active one.
-- Each lists the networks the configuration shows.
commandListing :: Ctx -> Config -> Command -> Maybe (ExceptT String (DB IO) Sections)
commandListing ctx cfg = \case
  CommandPortfolios _ -> Just (portfoliosListing cfg)
  CommandAddresses target internal _ paging -> Just $ inActive $ \portfolio -> addressesListing cfg portfolio target internal paging
  CommandBalances target _ paging -> Just $ inActive $ \portfolio -> balancesListing cfg portfolio target paging
  CommandCoins target locked _ paging -> Just $ inActive $ \portfolio -> coinsListing cfg portfolio target locked paging
  CommandTxs netM _ paging -> Just $ inActive $ \portfolio -> txsListing cfg portfolio netM paging
  CommandPendingTxs netM _ paging -> Just $ pendingTxsListing ctx cfg netM paging
  _ -> Nothing
  where
    inActive listing = listing =<< requireActivePortfolio

-- | Why the command is refused before it runs, when its --network names a
-- network the configuration hides: what is hidden, the networks hw shows,
-- and the command that shows this one too. Nothing for a command that may
-- run. The check is made once, here, for every command that has the
-- option ('networkNamedBy'), so that none of them works on a hidden
-- network by an oversight.
refusedNetwork :: Config -> Command -> Maybe String
refusedNetwork cfg cmd = case networkNamedBy cmd of
  Just net | not (isShown cfg net) -> Just (hiddenNetworkRefusal cfg net)
  _ -> Nothing

-- | Run a command in one transaction of the database, which no other
-- command changes meanwhile. What a command wrote before it failed is
-- undone, back to where it last called 'keepSoFar', if it did.
runDBResponse :: Config -> ExceptT String (DB IO) Response -> IO Response
runDBResponse cfg action = do
  result <- catchDatabaseInUse $ runDB cfg $ do
    answer <- runExceptT action
    when (isLeft answer) transactionUndo
    return answer
  return $ answered (join result)

-- | Keep what the command wrote so far, whatever happens next: a failure
-- after this point undoes only what was written after it.
--
-- A command is one transaction of the database, so that a failure leaves
-- no trace. send needs one exception. It signs a transaction, then hands
-- it to the network, and the second step can fail after the transaction
-- left the computer, or be interrupted while the store waits for its
-- nodes. The signed transaction must be in the wallet by then, with its
-- coins locked, as it is between signtx and broadcast. So send ends its
-- transaction of the database here, and goes on in a new one, which takes
-- the lock of the database again at once.
keepSoFar :: ExceptT String (DB IO) ()
keepSoFar = lift transactionSave

-- | How the screen names the wallets in the answer of a command: with
-- the name of each wallet when the database has several
-- ('WalletNaming'). It is read once the command has run, since
-- newportfolio, recover and importwallet may add a wallet that their
-- answer names. A failure names no portfolio in a view, so nothing is
-- read for it: the database may not be there yet, or be held by another
-- command, which is what the failure says. When another command takes
-- the database between the command and this reading, every wallet is
-- named. A name too many is never read wrong.
answerWalletNaming :: Config -> Response -> IO WalletNaming
answerWalletNaming _ (ResponseError _) = return WithoutWallets
answerWalletNaming cfg _ = listingWalletNaming cfg

-- | How the screen names the wallets in a listing, read before its first
-- chunk: the same for every chunk, so that the lines of a listing agree.
-- When another command holds the database, every wallet is named, and
-- the listing then fails on its first chunk.
listingWalletNaming :: Config -> IO WalletNaming
listingWalletNaming cfg = either (const WithWallets) id <$> catchDatabaseInUse (runDB cfg walletNaming)

-- | Run a command that needs no database
runResponse :: ExceptT String IO Response -> IO Response
runResponse action = answered <$> runExceptT action

-- | The answer of a command, or its error
answered :: Either String Response -> Response
answered = either (ResponseError . cs) id

{- Where a command works -}

-- | The accounts of the portfolio a listing covers: every one on the
-- networks hw shows, or only those on --network, or of --type. A type the
-- network does not have is refused. Every other choice keeps an account:
-- a portfolio has every account of every network.
listedAccounts :: Config -> Portfolio -> Maybe Network -> Maybe AccountKind -> ExceptT String (DB IO) [(DBAccountId, DBAccount)]
listedAccounts cfg portfolio netM kindM = do
  for_ ((,) <$> netM <*> kindM) $ liftEither . uncurry checkKindOnNetwork
  accounts <- lift $ portfolioAccountRows portfolio.portfolioId
  return
    [ held
      | held@(_, acc) <- onShownNetworks cfg (accountNetwork . snd) accounts,
        maybe True (== accountNetwork acc) netM,
        maybe True (== accountKind acc) kindM
    ]

-- | What the user named, among where the wallet has it: the elements on
-- the networks hw shows, given the network of each. When the wallet has
-- it on hidden networks alone, the refusal names them, with the command
-- that shows one ('onHiddenNetworks'). Nothing found at all is left to
-- the caller, which says so in its own words. The text says what was
-- named, as in "The coin a1075db5:1".
shownAmong :: Config -> String -> (a -> Network) -> [a] -> ExceptT String (DB IO) [a]
shownAmong cfg what networkOf found =
  case (onShownNetworks cfg networkOf found, found) of
    ([], _ : _) -> throwError (onHiddenNetworks what (nub (networkOf <$> found)))
    (shown, _) -> return shown

-- | The accounts a listing covers, by network, in the order of the
-- networks of a portfolio: what the portfolio holds on each network is
-- listed together
byNetwork :: [(DBAccountId, DBAccount)] -> [(Network, [(DBAccountId, DBAccount)])]
byNetwork accounts =
  [ (net, [held | held@(_, acc) <- accounts, accountNetwork acc == net])
    | net <- nub (accountNetwork . snd <$> accounts)
  ]

-- | The one pool a command works on among those that hold what it names:
-- the destinations of a sweep. The pools are those on the networks hw
-- shows, which the caller narrowed them to ('shownAmong'). They are
-- narrowed to the --network, when given. When what is left is on several
-- networks, as the same key is on each test network, the one on the
-- default network is taken. When it
-- is in several portfolios, the pool of the active portfolio is taken, when
-- it is one of them: the user who switched to a portfolio means that one.
-- Otherwise the refusal names them, for the user to switch to the one to
-- use. The first text says what was named, as in "The destinations of the
-- sweep".
poolAmong :: Config -> Maybe Network -> String -> [Pool] -> ExceptT String (DB IO) Pool
poolAmong cfg netM what held = do
  activeM <- lift activePortfolioId
  let fitting = nub (filter (\pool -> maybe True (== pool.poolNetwork) netM) held)
      networks = nub ((.poolNetwork) <$> fitting)
      onDefault = filter ((== configDefaultNetwork cfg) . (.poolNetwork)) fitting
      -- Those of one network: of the default network among several
      onOneNetwork = if length networks > 1 then onDefault else fitting
  case (fitting, netM) of
    ([only], _) -> return only
    ([], Just net) -> throwError (what <> " are on " <> networksText (nub ((.poolNetwork) <$> held)) <> ", not on the --network " <> net.name)
    _
      | null onOneNetwork -> throwError (what <> " are on " <> networksText networks <> ": give the --network")
      | [only] <- onOneNetwork -> return only
      | [ofActive] <- filter ((== activeM) . Just . (.poolPortfolio)) onOneNetwork -> return ofActive
      | otherwise -> do
          places <- mapM poolPlace onOneNetwork
          throwError (what <> " are in several portfolios, " <> intercalateAnd (placeText <$> places) <> ": switch to the one to use first")

-- | Write keys of the configuration file of hw, as the last step of a
-- command: a file that cannot be written fails the command, which undoes
-- what it did in the database
writeDefaults :: [(Text, Json.Value)] -> ExceptT String (DB IO) ()
writeDefaults changes = do
  dir <- liftIO hwDataDirectory
  ExceptT . liftIO . runExceptT $ setConfigKeys dir changes

{- Answers -}

-- | A portfolio as the answers show it, with what it can spend now on
-- each network hw shows. Its hidden networks are left out of every view
-- of it, and of its JSON.
viewOfPortfolio :: Config -> Portfolio -> ExceptT String (DB IO) PortfolioView
viewOfPortfolio cfg portfolio = do
  view <- portfolioView portfolio spendableBalance
  return view {viewNetworks = onShownNetworks cfg networkViewNetwork (viewNetworks view)}

-- | What an address can spend now, among the spendable balances read for
-- it: nothing when it holds no such coin
spendableAt :: Map Text Word64 -> DBAddress -> Word64
spendableAt balances addr = Map.findWithDefault 0 (dBAddressAddress addr) balances

-- | The answer showing an address of the account, with the balance it can
-- spend now. The account may be of another portfolio than the active one:
-- show and label find the address in every portfolio.
addressAnswer :: DBAccountId -> DBAddress -> ExceptT String (DB IO) Response
addressAnswer accId addr = do
  acc <- getAccountById accId
  ref <- accountRefOf acc
  balances <- spendableAtAddresses (accountNetwork acc) accId [dBAddressAddress addr]
  ResponseAddress ref addr (spendableAt balances addr) <$> activePortfolioRef

-- | The answer showing a transaction of a portfolio on a network. The
-- portfolio may be another one than the active one, when the command
-- found it from the transaction.
txAnswer :: Pool -> TxInfo -> ExceptT String (DB IO) Response
txAnswer pool info = ResponseTx <$> poolPlace pool <*> pure info <*> activePortfolioRef

-- | The portfolio of a place, with the active portfolio, as the view of a
-- transaction names them: what signtx and broadcast show before they ask
seenFromPlace :: Place -> ExceptT String (DB IO) SeenFrom
seenFromPlace place = SeenFrom (placePortfolio place) <$> activePortfolioRef

-- | How a listing counts the addresses of an account, for the widths of
-- its paths
accountCounts :: DBAccount -> AccountCounts
accountCounts acc = AccountCounts (accountKind acc) (fromIntegral (dBAccountExternal acc)) (fromIntegral (dBAccountInternal acc))

-- | The group of a listing for the accounts of a portfolio on a network
groupOf :: Portfolio -> Network -> [(DBAccountId, DBAccount)] -> [row] -> Group row
groupOf portfolio net accounts = Group (portfolioPlace portfolio net) (accountCounts . snd <$> accounts)

-- | The answer of a section of a listing: its rows in their group, or an
-- answer without a group when it has no row, so that a section without
-- rows adds nothing to the whole listing
grouped :: ([group] -> Response) -> ([row] -> group) -> [row] -> Response
grouped answer _ [] = answer []
grouped answer group rows = answer [group rows]

-- | The answer showing a coin, with the account that holds it. The account
-- may be of another portfolio than the active one: show, lockcoin and
-- unlockcoin find the coin in every portfolio.
coinAnswer :: (Network, DBCoin) -> (Place, JsonCoin) -> ExceptT String (DB IO) Response
coinAnswer (_, row) (_, coin) = do
  ref <- accountRefOf =<< getAccountById (dBCoinAccount row)
  ResponseCoin ref coin <$> activePortfolioRef

{- Wallets and portfolios -}

-- | Make a new mnemonic, or the shares of a new backup, with its wallet
-- and the portfolio 0 of it. Or, with --existing, ask for a mnemonic the
-- user has and add a portfolio to its wallet, the one with its
-- fingerprint ('seedWallet'), at the lowest free index, or create the
-- wallet with its portfolio 0. A portfolio is created whole, with every
-- account ('Haskoin.Wallet.Keys.portfolioAccounts'). The first portfolio
-- of a database becomes the active one, and the answer says which
-- portfolio is active ('newPortfolioAnswer').
cmdNewPortfolio :: Ctx -> Config -> NewPortfolio -> IO Response
cmdNewPortfolio ctx cfg opts
  | opts.existing = runDBResponse cfg $ do
      -- Each of these would do nothing here. Refused, it does not let
      -- the user believe it did something.
      when (opts.dice || isJust opts.shares || isJust opts.threshold || isJust opts.entropy || opts.oneLine) $
        throwError "--entropy, --dice, --shares, --threshold and --one-line are for a new mnemonic: they do not go with --existing"
      secret <- askNewSeedSecret terminal
      seed <- seedToCreateWith ctx secret
      newPortfolioAnswer cfg Nothing $ do
        held <- maybe (lift (insertWallet (seedFingerprint seed))) return =<< seedWallet ctx seed
        index <- lift $ nextPortfolioIndex seed (fst held)
        createPortfolio ctx seed held index
  | otherwise = do
      -- The secret is made and its passphrase asked before the database
      -- is held: the user may take a while
      made <- runExceptT newSecret
      case made of
        Left err -> return (ResponseError (cs err))
        Right (shown, secret) -> runDBResponse cfg $ do
          seed <- liftEither $ secretSeed ctx secret
          -- A new mnemonic is a new wallet. The same fingerprint in the
          -- database would mean the same seed came out twice.
          clash <- lift $ walletWithFingerprint (seedFingerprint seed)
          for_ clash $ \(_, wallet) ->
            throwError $
              "The new mnemonic has the fingerprint of " <> cs (walletName (walletIndex wallet)) <> ": nothing was created. Run the command again for another mnemonic"
          newPortfolioAnswer cfg (Just shown) $ do
            held <- lift $ insertWallet (seedFingerprint seed)
            createPortfolio ctx seed held 0
  where
    sources =
      Sources
        { readSystem = systemEntropy,
          askDice = if opts.dice then Just (askDiceEntropy terminal) else Nothing,
          say = tell terminal
        }
    -- 16 bytes, a mnemonic of 12 words, unless --entropy says otherwise
    entropyBytes = fromMaybe 16 opts.entropy
    -- The new secret as the answer shows it, and with its passphrase as
    -- the wallet is derived from it
    newSecret :: ExceptT String IO (NewSecret, SeedSecret)
    newSecret = case opts.shares of
      Nothing -> do
        when (isJust opts.threshold) $
          throwError "--threshold goes with --shares: it is the number of shares that rebuild the seed"
        (source, mnemonic) <- genMnemonic sources entropyBytes
        secret <- askNewPassphraseOf terminal (MnemonicWords mnemonic)
        return (NewMnemonic source (Text.words mnemonic), secret)
      Just count -> do
        (source, threshold, shares) <- genShares sources entropyBytes count opts.threshold
        backup <- liftEither $ backupOfShares shares
        secret <- askNewPassphraseOf terminal (BackupShares backup)
        return (NewShares source (fromIntegral threshold) (Text.words <$> shares), secret)

-- | The answer of newportfolio: the secret it made, if any, the portfolio
-- the action creates, and the active portfolio after it, which is the new
-- one when the database had none
newPortfolioAnswer :: Config -> Maybe NewSecret -> ExceptT String (DB IO) Portfolio -> ExceptT String (DB IO) Response
newPortfolioAnswer cfg shownM create = do
  (portfolio, activeM) <- withActiveAfter create
  -- The database has a portfolio now, so it has an active one
  active <- liftMaybe noWallets activeM
  view <- viewOfPortfolio cfg portfolio
  return $ ResponseNewPortfolio shownM view active

-- | The seed newportfolio --existing adds a portfolio from: the one BIP39
-- derives from the mnemonic and the passphrase, or the one SLIP-39
-- derives from a backup in shares and its passphrase. For a mnemonic with
-- a passphrase that normalizing changes, hw 0.9 derived another seed,
-- from the passphrase as typed, and recover makes a wallet of that seed
-- when it finds its funds. When the database has a wallet of that seed
-- and none of the other, the new portfolio is of it too, so that one
-- mnemonic keeps one wallet, and the user is told.
seedToCreateWith :: Ctx -> SeedSecret -> ExceptT String (DB IO) PublicSeed
seedToCreateWith ctx secret = do
  seed <- liftEither $ secretSeed ctx secret
  typedM <- liftEither $ secretTypedSeed ctx secret
  case typedM of
    Nothing -> return seed
    Just typed -> do
      ofNormalized <- isJust <$> seedWallet ctx seed
      ofTyped <- isJust <$> seedWallet ctx typed
      if
        | ofTyped && not ofNormalized -> liftIO (tell terminal typedPassphraseCreated) >> return typed
        -- A wallet hw 0.9 made from this passphrase has the keys of the
        -- passphrase as typed, and this one will not
        | not ofNormalized -> liftIO (tell terminal typedPassphraseNew) >> return seed
        | otherwise -> return seed

-- | Recover the portfolios of a seed that were paid, from its mnemonic or
-- the shares of its backup: see "Haskoin.Wallet.Sync". The search covers
-- every network of a portfolio, the test networks included, and the
-- hidden ones: a portfolio paid on a hidden network alone is still found
-- and created, and its network is up to date the day the user shows it.
-- The status line names each network as it is searched. The answer keeps
-- to the networks hw shows ('onShownNetworks'): it names no hidden network
-- as searched or left out, and gives none of the syncs or of the funds
-- found there. A store that fails on a hidden network alone does not make
-- the command exit with an error. In a database without any portfolio,
-- the first one created becomes the active one, and the answer says which
-- portfolio is active.
--
-- The store of each network is asked whether it is up before the mnemonic
-- is typed, which the line tells. Once the host could not be reached, the
-- stores of the networks after are not asked, and are down
-- ('servedNetworks'). A network the host does not serve is named and left
-- out, and so is one whose store is down, which also makes the command
-- exit with an error once the others are searched: a portfolio that was
-- paid on that network alone is not found. When no network can be
-- searched, the command fails before the mnemonic: the computer is
-- offline, or the host is wrong. A store that fails later, during the
-- search, the syncs or the search of the funds at other paths, is named
-- the same way: its network is searched no further, and the others are,
-- and the wallet found is kept ('storeFailureApart'). The answer names as
-- searched the networks that were searched in full.
--
-- The funds of the seed at the accounts hw does not hold are told, not
-- taken. When the seed has no wallet and no paid portfolio, and
-- normalizing changes the passphrase, the portfolios hw derived before,
-- from the passphrase as typed, are looked for too, and the user is told
-- of those. Only the seed of BIP39 is looked at for the other kinds of
-- accounts: other wallets derive from no other.
cmdRecover :: Ctx -> Config -> Recover -> IO Response
cmdRecover ctx cfg opts =
  withStatusLine $ \line ->
    runDBResponse cfg $ do
      storeOn <- liftIO $ webStores ctx cfg
      let watch = terminalWatcher line
      -- An offline computer hears it before the mnemonic is typed
      served <- liftIO $ servedNetworks (checkingHealth (watch.status "recover") storeOn) portfolioNetworks
      -- With no network to search, the refusal names the shown ones
      liftEither $ requireSomeNetwork (configHost cfg) served {servedNot = onShownNetworks cfg id (servedNot served)}
      -- The question for the mnemonic starts on a clean line
      liftIO $ eraseStatus line
      let searched = servedUp served
      activeBefore <- lift activePortfolioId
      -- Typed once: a mistyped mnemonic finds no paid portfolio
      secret <- askSeedSecret terminal
      seed <- liftEither $ secretSeed ctx secret
      found <- recoverSeed ctx cfg storeOn searched seed opts.portfolioGap watch
      typedM <- liftEither $ secretTypedSeed ctx secret
      -- The networks whose store has not failed yet
      let upAfter failed = [net | net <- searched, net `notElem` (fst <$> failed)]
      (recovered, failedInRecovery) <- case typedM of
        Just typed | isNothing (recoveredWallet found) -> do
          fromTyped <- recoverSeed ctx cfg storeOn (upAfter (recoveredDown found)) typed opts.portfolioGap watch
          let failed = recoveredDown found <> recoveredDown fromTyped
          if isJust (recoveredWallet fromTyped)
            then do
              liftIO $ eraseStatus line >> tell terminal typedPassphraseRecovered
              return (fromTyped, failed)
            else return (found, failed)
        _ -> return (found, recoveredDown found)
      -- The funds at other paths, on each network whose store is still up
      fundsSearched <- forM (upAfter failedInRecovery) $ \net ->
        storeFailureApart (storeOn net) $ \store -> map (net,) <$> foreignFunds ctx cfg store net seed opts.portfolioGap watch
      let elsewhere = concat [funds | Right funds <- fundsSearched]
          failed = failedInRecovery <> [(net, failure) | (net, Left failure) <- zip (upAfter failedInRecovery) fundsSearched]
      active <- activeAfterFrom activeBefore
      -- The answer keeps to the networks hw shows
      return $
        ResponseRecover
          (onShownNetworks cfg id (upAfter failed))
          opts.portfolioGap
          (recoveredWallet recovered)
          (recoveredPortfolios recovered)
          (shownSyncs cfg (recoveredSyncs recovered))
          (onShownNetworks cfg fst elsewhere)
          (onShownNetworks cfg id (servedNot served))
          [(net, cs failure) | (net, failure) <- onShownNetworks cfg fst (servedDown served <> failed)]
          active

-- | Check a mnemonic and its passphrase, or the shares of a backup,
-- against the active portfolio: they match when they give the key of
-- every account the portfolio has. Nothing is written. An answer that
-- they do not match makes hw exit with an error ('responseFailed').
cmdCheckMnemonic :: Ctx -> Config -> IO Response
cmdCheckMnemonic ctx cfg =
  runDBResponse cfg $ do
    portfolio <- requireActivePortfolio
    accounts <- lift $ portfolioAccountRows portfolio.portfolioId
    secret <- askSeedSecret terminal
    seed <- liftEither $ secretSeed ctx secret
    typedM <- liftEither $ secretTypedSeed ctx secret
    let gives candidate = liftIO $ and <$> mapM (seedDerives ctx candidate portfolio . snd) accounts
    normalized <- gives seed
    asTyped <- if normalized then return False else maybe (return False) gives typedM
    -- Other wallets would not find a portfolio of the passphrase as typed
    when asTyped $ liftIO $ tell terminal typedPassphraseChecked
    let typed = case secret of
          MnemonicSecret _ -> "The mnemonic and passphrase"
          SharesSecret _ _ -> "The shares and passphrase"
        matched = normalized || asTyped
        said = typed <> (if matched then " matched " else " did not match ") <> cs (portfolioText portfolio)
    return $ ResponseChecked (portfolioRef portfolio) matched said

-- | What checkmnemonic tells of a portfolio that matches the passphrase as
-- typed, and not as BIP39 asks to normalize it
typedPassphraseChecked :: String
typedPassphraseChecked =
  "This portfolio comes from the passphrase as typed, which hw did not normalize as BIP39 asks: \
  \other wallets would not find it from this mnemonic and passphrase."

-- | Add the portfolios of a BIP 329 file to the database and set the
-- labels it carries, as "Haskoin.Wallet.Labels" reads it. The file names
-- each of its wallets by the fingerprint of its seed. --wallet reads one
-- wallet of the file, by that fingerprint, and --portfolio one portfolio
-- ('scopeWallet', 'scopePortfolio'). The answer says what became of each
-- wallet and each portfolio read, what the file labels that the wallet
-- does not have yet, and which portfolio is active: the first one
-- created, in a database that had none.
cmdImportWallet :: Ctx -> Config -> ImportScope -> FilePath -> IO Response
cmdImportWallet ctx cfg scope file =
  runDBResponse cfg $ do
    content <- liftEitherIO $ readTextFile file
    -- A key or a descriptor alone, or a file of another kind, holds no
    -- record: it is refused, not read as an empty file
    unless (holdsAnything (readWalletFile content)) $ throwError (couldNotRead file notAWalletFile)
    -- The import is undone whole when it is refused, which the refusal
    -- says. show shares its refusals, and changes nothing either way.
    (report, active) <- withActiveAfter $ withExceptT (<> ". Nothing was changed") (importFile ctx scope content)
    return (ResponseImport report active)

-- | Why a file without any record is refused by importwallet
notAWalletFile :: String
notAWalletFile =
  "It holds no record of a wallet. importwallet reads the file exportwallet writes. \
  \A key or a descriptor alone is one account, and does not say which portfolio it belongs to"

-- | Write portfolios to a BIP 329 file, with their labels and the coins
-- the user locked. Without options, the file holds every portfolio of
-- every wallet. --wallet and --portfolio write less ('portfoliosToExport').
cmdExportWallet :: Ctx -> Config -> Maybe Natural -> Maybe Natural -> FilePath -> IO Response
cmdExportWallet ctx cfg walletM portfolioM file =
  runDBResponse cfg $ do
    portfolios <- portfoliosToExport walletM portfolioM
    checkPathFree file
    records <- walletRecords ctx portfolios
    liftEitherIO $ writeTextFile file (recordsText records)
    return $ ResponseFile file

-- | The portfolios exportwallet writes, in the order of the wallets and
-- of the portfolios of each.
--
-- Without an option, they are every portfolio of every wallet. The file
-- is then the whole database. One file moves it to another computer, and
-- one file keeps every label before a wallet is deleted. The active
-- portfolio plays no part. So the same command writes the same file
-- whichever portfolio the user last switched to.
--
-- The wallet of the active portfolio was the default before. A user of
-- several mnemonics then had to switch and export once for each. A
-- wallet they forgot was left out without a word.
--
-- The options narrow the file down. --wallet writes every portfolio of
-- one wallet. It is for a file that must not show the other wallets,
-- since a file of several wallets tells its reader that one person holds
-- them all. --portfolio writes one portfolio. Its wallet is the one
-- --wallet names, or the wallet of the active portfolio without --wallet,
-- as for switch ('walletNamedOrActive').
--
-- A file of several wallets has no field that a file of one wallet
-- lacks. Each record names its wallet in its origin, by the fingerprint
-- of the mnemonic. BIP 329 gives the origin to tell apart the wallets of
-- one export, and importwallet finds each wallet by it
-- ("Haskoin.Wallet.Labels"). The index of a wallet is not in the file.
-- So wallet0 here may be wallet1 on the computer that reads the file.
--
-- A wallet or a portfolio the database does not have is refused. So is
-- the export of a database without a wallet ('noWallets'), since its
-- file would be empty.
portfoliosToExport :: Maybe Natural -> Maybe Natural -> ExceptT String (DB IO) [Portfolio]
portfoliosToExport walletM portfolioM = case (walletM, portfolioM) of
  (Nothing, Nothing) -> do
    portfolios <- lift getAllPortfolios
    when (null portfolios) $ throwError noWallets
    return portfolios
  (Just wallet, Nothing) -> lift . walletPortfolios =<< walletNamed wallet
  (_, Just index) -> do
    wallet <- walletNamedOrActive walletM
    pure <$> portfolioNamed wallet index

-- | Make the portfolio --wallet and --portfolio name the active one, the
-- one the commands work in from now on, and show it ('switchTo')
cmdSwitch :: Config -> Maybe Natural -> Natural -> IO Response
cmdSwitch cfg walletM index =
  runDBResponse cfg $ do
    portfolio <- switchTo walletM index
    ResponseSwitched <$> viewOfPortfolio cfg portfolio

-- | Write the defaults the options give to the configuration file: the
-- network, the type of address, the unit and the view of the listings,
-- and the networks hw shows. Each is written by the name the file reads
-- back, which is also the name the answer shows. The portfolio the
-- commands work in is no default of the file: switch makes another one
-- active.
--
-- --show-network and --hide-network change the networks hw shows
-- ('shownAfter'). The default network, the one --network names or the
-- one the file has, must be among them once the command is done
-- ('checkDefaultStaysShown'). So one command shows a network and makes it
-- the default, or hides the default network and names another. The
-- networks are written whole, in the order of the networks of a
-- portfolio, and the answer lists them.
cmdSetDefault :: Config -> SetDefault -> IO Response
cmdSetDefault cfg opts =
  runDBResponse cfg $ do
    shown <- liftEither $ shownAfter cfg opts.showNetworks opts.hideNetworks
    liftEither $ checkDefaultStaysShown cfg opts.network shown
    let named =
          [(defaultNetworkKey, cs net.name) | Just net <- [opts.network]]
            <> [(defaultTypeKey, kindName kind) | Just kind <- [opts.kind]]
            <> [(unitKey, unitName unit) | Just unit <- [opts.unit]]
            <> [(listingViewKey, viewName view) | Just view <- [opts.view]]
        changesNetworks = not (null opts.showNetworks && null opts.hideNetworks)
        names = [cs net.name | net <- shown] :: [Text]
    when (null named && not changesNetworks) $
      throwError "Give the default to set: --network, --type, --unit, --compact, --verbose, --tabular, --show-network or --hide-network"
    writeDefaults $
      [(key, Json.toJSON name) | (key, name) <- named]
        <> [(shownNetworksKey, Json.toJSON names) | changesNetworks]
    file <- configFile <$> liftIO hwDataDirectory
    return $ ResponseDefaults (named <> [(shownNetworksKey, Text.intercalate ", " names) | changesNetworks]) file

-- | The networks hw shows once setdefault has shown the first ones given
-- and hidden the second ones: those of the configuration, with the ones
-- to show added and the ones to hide taken out, in the order of the
-- networks of a portfolio. Showing a network hw shows already, or hiding
-- a hidden one, changes nothing and is no error, since the file ends up
-- as asked. A network given to both options is refused: the user cannot
-- mean both.
shownAfter :: Config -> [Network] -> [Network] -> Either String [Network]
shownAfter cfg toShow toHide =
  case [net | net <- toShow, net `elem` toHide] of
    net : _ -> Left ("The network " <> net.name <> " is given to both --show-network and --hide-network")
    [] -> Right (inPortfolioOrder [net | net <- configShownNetworks cfg <> toShow, net `notElem` toHide])

-- | Refuse to leave the default network hidden: the one setdefault is
-- given with --network, or else the one of the configuration, must be
-- among the networks hw shows once the command is done. A command without
-- --network works on the default network, and would otherwise work on a
-- network hw shows nothing of. Each refusal gives the command that does
-- what the user meant.
checkDefaultStaysShown :: Config -> Maybe Network -> [Network] -> Either String ()
checkDefaultStaysShown cfg givenM shown = case givenM of
  Just net
    | net `notElem` shown ->
        Left $
          "The network "
            <> net.name
            <> " is hidden, and the default network must be one hw shows. To show it and make it the default, run: "
            <> showNetworkCommand net
            <> " --network "
            <> net.name
  Nothing
    | current `notElem` shown ->
        Left $
          "The network "
            <> current.name
            <> " is the default network, which hw always shows. To hide it, name another default in the same command"
            <> maybe "" (\other -> ", as in: hw setdefault --hide-network " <> current.name <> " --network " <> other.name) (listToMaybe shown)
  _ -> Right ()
  where
    current = configDefaultNetwork cfg

-- | Delete a portfolio of the wallet of the active portfolio that was
-- never used: not portfolio 0, not the active one, the last of its
-- wallet, without any activity, and without any address given out
-- ('portfolioHasActivity'). The portfolio must be named. The active
-- portfolio is refused rather than replaced by another one: which one
-- the user works in next is theirs to say, with switch.
cmdDeletePortfolio :: Config -> Maybe Natural -> IO Response
cmdDeletePortfolio cfg indexM =
  runDBResponse cfg $ do
    index <- liftMaybe "Give the --portfolio to delete: deleteportfolio takes no default" indexM
    active <- requireActivePortfolio
    portfolio <- portfolioNamed (active.portfolioWalletId, active.portfolioWallet) index
    let name = portfolioText portfolio
    when (index == 0) $
      throwError "portfolio0 is never deleted: a wallet always keeps it, and a new one would be the same portfolio, with the same keys. deletewallet deletes a whole wallet"
    when (portfolio.portfolioId == active.portfolioId) $
      throwError (name <> " is the active portfolio: switch to another portfolio first")
    others <- lift $ getPortfolios portfolio.portfolioWalletId
    let highest = maximum (index : (portfolioIndex . snd <$> others))
    when (index /= highest) $
      throwError (name <> " is not the last portfolio of its wallet: " <> cs (portfolioName highest) <> " goes first")
    used <- lift $ portfolioHasActivity portfolio.portfolioId
    when used $
      throwError (name <> " has had activity or gave out an address, so it is kept: a payment to it would go unseen, and recover would find it again. deletewallet deletes a whole wallet")
    lift $ deletePortfolio portfolio.portfolioId
    return $ ResponseDeleted (cs name <> " has been deleted")

-- | Delete a wallet with all of its portfolios, whatever they hold. The
-- wallet must be named, and its fingerprint given, which confirms the
-- wallet. The wallet of the active portfolio is refused while another
-- wallet exists: which one the user works in next is theirs to say, with
-- switch. The last wallet goes with the active portfolio, and the next
-- portfolio created becomes active, as in a new database.
cmdDeleteWallet :: Config -> Maybe Natural -> Text -> IO Response
cmdDeleteWallet cfg walletM typed =
  runDBResponse cfg $ do
    index <- liftMaybe "Give the --wallet to delete: deletewallet takes no default" walletM
    (walletId, wallet) <- walletNamed index
    let name = cs (walletName index) :: String
    unless (Text.toLower (Text.strip typed) == fingerprintToText (walletFingerprint wallet)) $
      throwError ("The fingerprint given is not that of " <> name <> ": portfolios --verbose shows it. Nothing was deleted")
    others <- filter ((/= walletId) . fst) <$> lift getWallets
    activeM <- activePortfolio
    for_ activeM $ \active ->
      when (active.portfolioWalletId == walletId && not (null others)) $
        throwError (name <> " holds the active portfolio, " <> portfolioText active <> ": switch to a portfolio of another wallet first")
    lift $ deleteWallet walletId
    -- The raw transactions its coins came from are of no use any more
    _ <- dropOrphanRawTxs
    return . ResponseDeleted $
      cs name
        <> " has been deleted, with all of its portfolios"
        <> (if null others then ". The database has no wallet left: the first portfolio created next becomes the active one." else "")

{- Listings to files -}

-- | Write every row of a listing to a new CSV file, for a spreadsheet,
-- and answer with the file ("Haskoin.Wallet.Cli.Csv"). The listing is
-- read whole, in one transaction, as --json reads it, then written: the
-- file is either whole or not there. A path that is taken is refused
-- before the listing is read. The file holds the sheet of the listing
-- ('localListingSheet'), the one exportsheet writes to the tab of the
-- listing, so that the rows of one file fit the other.
cmdCsv :: Config -> FilePath -> ExceptT String (DB IO) Sections -> IO Response
cmdCsv cfg file listing = do
  free <- runExceptT (checkPathFree file)
  case free of
    Left err -> return (ResponseError (cs err))
    Right () -> do
      answer <- readListing cfg listing
      sheetM <- localListingSheet answer
      case sheetM of
        -- The listing failed, and its answer says why
        Nothing -> return answer
        Just sheet -> either (ResponseError . cs) (const (ResponseFile file)) <$> writeFileBytes file (sheetCsv sheet)

-- | Write every listing of every portfolio to a new OpenDocument
-- spreadsheet, a tab each ("Haskoin.Wallet.Cli.Ods"), and answer with the
-- file. The tab of a listing holds its sheet, the one --csv writes, for
-- every portfolio ('walletSheets'). So the rows of a CSV file fit under
-- the headings of its tab. Two more tabs hold the addresses of the
-- transactions and of the pending transactions, a row for each address.
-- The listings are read in one transaction, so that the tabs agree with
-- each other: the balances with the coins, the coins with the
-- transactions, and the addresses with their transactions. The
-- spreadsheet is the record of the whole wallet, so the active portfolio
-- does not narrow it. It holds what the listings show: the networks the
-- configuration hides are left out of it, as they are of each listing.
cmdExportSheet :: Ctx -> Config -> FilePath -> IO Response
cmdExportSheet ctx cfg file =
  runDBResponse cfg $ do
    checkPathFree file
    portfolios <- lift getAllPortfolios
    when (null portfolios) $ throwError noWallets
    summary <- sheetOf (portfoliosListing cfg)
    held <- mapM portfolioSheets portfolios
    -- The pending transactions of every portfolio in one tab, and their
    -- addresses in another
    pending <- txSheetsOf (pendingTxsListing ctx cfg Nothing everyRow)
    now <- liftIO getCurrentTime
    liftEitherIO $ writeFileBytes file (workbookOds now (walletSheets summary held pending))
    return $ ResponseFile file
  where
    everyRow :: Paging a
    everyRow = Paging Nothing Nothing
    -- The listings of one portfolio, over every network hw shows: its
    -- receiving addresses then its change addresses, its balances, all
    -- its coins, and its transactions with their addresses
    portfolioSheets portfolio = do
      let whole = Target Nothing Nothing
      receiving <- sheetOf (addressesListing cfg portfolio whole False everyRow)
      change <- sheetOf (addressesListing cfg portfolio whole True everyRow)
      PortfolioSheets (stacked [receiving, change])
        <$> sheetOf (balancesListing cfg portfolio whole everyRow)
        <*> sheetOf (coinsListing cfg portfolio whole False everyRow)
        <*> txSheetsOf (txsListing cfg portfolio Nothing everyRow)

-- | The sheet of a listing, read whole in the transaction the caller
-- runs, its local times on the clock of this computer
sheetOf :: ExceptT String (DB IO) Sections -> ExceptT String (DB IO) Sheet
sheetOf = sheetsOf localListingSheet

-- | The two sheets of a listing of transactions, read once: its
-- transactions and their addresses. Both come from one reading, so that
-- every address is of a transaction of the other sheet.
txSheetsOf :: ExceptT String (DB IO) Sections -> ExceptT String (DB IO) TxSheets
txSheetsOf = sheetsOf localTxSheets

-- | What the function given makes of the answer of a listing, read whole
-- in the transaction the caller runs: its sheet, or its sheets
sheetsOf :: (Response -> IO (Maybe sheets)) -> ExceptT String (DB IO) Sections -> ExceptT String (DB IO) sheets
sheetsOf sheetsFrom listing = do
  answer <- wholeListing listing
  made <- liftIO (sheetsFrom answer)
  -- Every listing has a sheet: an answer without one would be a bug
  maybe (throwError "The listing has no sheet") return made

-- | Every portfolio, each with what it can spend now on each network hw
-- shows and whether it is the active one, in the order of the wallets and
-- of the portfolios of each: a portfolio names the one to go on after
portfoliosListing :: Config -> ExceptT String (DB IO) Sections
portfoliosListing cfg =
  return . oneSection Nothing $
    Listing
      { listingStart = Nothing,
        listingRows = \afterM countM -> do
          portfolios <- lift getAllPortfolios
          let following = case afterM of
                Nothing -> portfolios
                Just cursor -> drop 1 $ dropWhile ((/= cursor) . portfolioRef) portfolios
          mapM (viewOfPortfolio cfg) $ maybe id genericTake countM following,
        listingCursor = Right . viewRef,
        listingAnswer = ResponsePortfolios,
        -- Every chunk reads every portfolio anyway, and the table of
        -- portfolios takes the width of its columns from all of them
        listingWhole = True
      }

{- Short ids -}

-- A command takes a hash in full or by its short id, its first 8 digits
-- (see "Haskoin.Wallet.ShortId"). A short id is looked up among the
-- hashes the command can name, in every portfolio, and the command then
-- goes on as with the full hash: it says what it says of a full hash the
-- portfolio does not hold, for instance. A short id that matches none
-- gets the answer a full hash that matches nothing gets, with the short
-- id in it. One that matches two or more is refused, with each one named,
-- for the user to give the full hash.

-- | The pending transaction a command names, by its nosigHash or its txid
-- once signed, in full or by its short id
pendingHashOf :: HashRef -> ExceptT String (DB IO) TxHash
pendingHashOf = fullHashOf noPendingTx pendingNamed pendingHashesWith

-- | The pending transaction a command names, as 'pendingHashOf' finds it,
-- which must exist, on a network hw shows. One on a hidden network is
-- refused, with the command that shows its network: exporttx, deletetx,
-- signtx, signsweep and broadcast all take a pending transaction by its
-- hash, and none of them works on a hidden network.
pendingTxNamed :: Config -> HashRef -> ExceptT String (DB IO) PendingTx
pendingTxNamed cfg named = do
  held <- requirePendingTx =<< pendingHashOf named
  _ <- shownAmong cfg ("The pending transaction " <> hashRefText named) (.poolNetwork) [held.pendingTxPool]
  return held

-- | The transaction on the chain that txs --after names, by its txid in
-- full or by its short id, among those of every portfolio. The listing
-- then refuses one of another portfolio.
chainTxidOf :: HashRef -> ExceptT String (DB IO) TxHash
chainTxidOf named = fullHashOf none transactionsNamed (lift . walletTxidsWith) named
  where
    none = "The transaction " <> hashRefText named <> " is not among those listed"

-- | The transaction a command names, pending or on the chain, by its
-- txid, or by its nosigHash while it is pending, in full or by its short
-- id: what label and bumpfee take
walletTxHashOf :: HashRef -> ExceptT String (DB IO) TxHash
walletTxHashOf named = fullHashOf (noTransaction named) walletTxsNamed walletHashesWith named

-- | The answer to a hash that names no transaction of the wallet
noTransaction :: HashRef -> String
noTransaction named = "The wallet has no transaction " <> hashRefText named

-- | The coin a command names, by its txid in full or by its short id, and
-- its index, among the coins of every portfolio: what lockcoin,
-- unlockcoin and --coin take
walletCoinOf :: CoinRef -> ExceptT String (DB IO) OutPoint
walletCoinOf named = coinNamed ("The wallet has no coin " <> coinRefText named) named

-- | The coin a command names, as 'walletCoinOf' finds it, with the answer
-- to a short id that matches no coin
coinNamed :: String -> CoinRef -> ExceptT String (DB IO) OutPoint
coinNamed none = \case
  CoinRef (FullHash hash) index -> return (OutPoint hash index)
  CoinRef (ShortHash short) index -> liftEither . pickCoin none short index =<< lift (coinsWith short index)

{- Addresses -}

-- | Give out the next receiving address of the account --network and
-- --type pick in the active portfolio ('chooseAccount')
cmdReceive :: Ctx -> Config -> Receive -> IO Response
cmdReceive ctx cfg opts =
  runDBResponse cfg $ do
    portfolio <- requireActivePortfolio
    (accId, _) <- chooseAccount cfg portfolio opts.target.network opts.target.kind
    addr <- genExtAddress ctx cfg accId (fromMaybe "" opts.label)
    addressAnswer accId addr

-- | Show what the word names, as its shape tells ('Shown'), or without a
-- word the newest receiving address of the account. Nothing is generated
-- or stored.
--
-- A path and the newest address are of the account the options pick in
-- the active portfolio, as for receive. A path that starts with its type,
-- as the views write it (bip84/0/3), names its account itself. An address
-- and a coin tell where they are too, so --type is refused with them, and
-- with such a path, rather than ignored: the user who typed it may have
-- meant another thing to show. --network then only tells apart an address
-- or a coin the wallet has on several networks, as the test networks share
-- their keys. A transaction is shown as each
-- portfolio that holds it sees it, whichever is active. A file of a
-- wallet is read as importwallet reads it, with --wallet and --portfolio,
-- which name one wallet and one portfolio of the file, and go with no
-- other word.
cmdShow :: Ctx -> Config -> Target -> ImportScope -> Maybe Shown -> IO Response
cmdShow ctx cfg target fileScope shownM =
  runDBResponse cfg $ do
    when (fileScope /= wholeFile && not (namesFile shownM)) $ throwError walletFileScopeOnly
    case shownM of
      Just (ShownFile file) -> shownFile ctx cfg target fileScope file
      Nothing -> newestReceiving cfg target
      Just (ShownPath Nothing addrType index) -> addressAtPath ctx cfg target addrType index
      Just (ShownPath (Just kind) addrType index) -> do
        refuseType "a path that starts with its type"
        addressAtPath ctx cfg (Target target.network (Just kind)) addrType index
      Just (ShownAddress typed) -> do
        refuseType "an address"
        (_, accId, addr) <- addressByText cfg target.network typed
        addressAnswer accId addr
      Just (ShownTx named) -> do
        refuseType "a transaction"
        shownTxs ctx cfg target named
      Just (ShownCoin named) -> do
        refuseType "a coin"
        held <- walletCoin cfg target.network =<< walletCoinOf named
        coinAnswer held =<< shownCoin held
  where
    namesFile = \case
      Just (ShownFile _) -> True
      _ -> False
    refuseType what =
      when (isJust target.kind) $
        throwError ("show takes no --type with " <> what)

-- | The refusal of show --wallet or --portfolio with anything but a file
-- of a wallet. The two options name a wallet and a portfolio of such a
-- file, not where show looks.
walletFileScopeOnly :: String
walletFileScopeOnly =
  "show takes --wallet and --portfolio only with a file of a wallet, where they name one wallet and one portfolio of the file. The rest is shown in the active portfolio, or where it is"

-- | The newest receiving address of the account, the first one addresses
-- lists, to give out again
newestReceiving :: Config -> Target -> ExceptT String (DB IO) Response
newestReceiving cfg target = do
  portfolio <- requireActivePortfolio
  (accId, _) <- chooseAccount cfg portfolio target.network target.kind
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
addressAtPath :: Ctx -> Config -> Target -> AddrType -> KeyIndex -> ExceptT String (DB IO) Response
addressAtPath ctx cfg target addrType index = do
  portfolio <- requireActivePortfolio
  (accId, acc) <- chooseAccount cfg portfolio target.network target.kind
  let path = addrDeriv addrType :/ index
  addr <- derivedAddressAt ctx acc addrType index
  -- The row is looked up by the address derived, not by its path, so that
  -- the answer shows what the key derives in either case
  held <- lift $ storedAmong [addr] (accId, acc)
  case held of
    row : _ -> addressAnswer accId row
    [] -> ResponseDerivedAddr (accountRef portfolio acc) path <$> liftMaybe "An account derives no address of this kind" (cutAddress (accountNetwork acc) addr)

-- | A file, a transaction file or a file of a wallet, as its bytes tell
-- ('txFileForm'). The wallet keeps nothing of either.
--
-- show takes for a file any word of no other shape ('readShown'). A word
-- with no slash that names no file may have been meant as something else,
-- a short id with a digit missing for instance. The answer then names the
-- five kinds show takes. A word with a slash is a file path, and gets the
-- answer of any command that reads a missing file.
shownFile :: Ctx -> Config -> Target -> ImportScope -> FilePath -> ExceptT String (DB IO) Response
shownFile ctx cfg target fileScope file = do
  exists <- liftIO $ doesPathExist file
  unless (exists || '/' `elem` file) $
    throwError $
      file
        <> " is not a path, an address, a transaction or a coin, and no file has that name: show takes a path such as /0/3,"
        <> " an address of the wallet, a txid or a nosigHash, a coin TXID:INDEX, or a file of a transaction or of a wallet,"
        <> " each hash in full or by its short id, its first 8 digits"
  when (isJust target.kind) $ throwError "show takes no --type with a file"
  bytes <- liftEitherIO $ readFileBytes file
  case txFileForm bytes of
    Just _ -> do
      when (fileScope /= wholeFile) $ throwError walletFileScopeOnly
      shownTxFile ctx cfg target file bytes
    Nothing -> shownWalletFile ctx cfg target fileScope file bytes

-- | The transaction of a file, as the portfolio it involves sees it. One
-- that involves no portfolio is shown as outside the wallet, on the
-- network of --network or of the configuration when the file names none.
-- The labels shown are those the file carries, not those of this wallet,
-- since the file is what an offline computer has. hw finds the portfolio
-- ('transactionPool'): a transfer between two portfolios is shown as the
-- one that pays sees it. An address it pays that another portfolio of the
-- database holds is named so ('fillElsewhere'): no file carries that.
shownTxFile :: Ctx -> Config -> Target -> FilePath -> BS.ByteString -> ExceptT String (DB IO) Response
shownTxFile ctx cfg target file bytes = do
  TxFileRead txFile@(TxFile netM tsd _ _) _ <- txFileOfBytes ctx (commandNetwork cfg target.network) file bytes
  named <- fileNetwork target.network netM
  activeM <- lift activePortfolioId
  (poolM, completed) <- transactionPool ctx cfg activeM named tsd
  case poolM of
    Just pool -> do
      keys <- lift $ poolKeys ctx pool
      info <- liftEither (parseTxFile pool.poolNetwork ctx keys txFile {txFileData = completed})
      txAnswer pool =<< lift (fillElsewhere pool info)
    Nothing -> do
      let net = commandNetwork cfg named
      ResponseOutsideTx net <$> liftEither (parseOutsideTxSignData net ctx tsd)

-- | A file of a wallet, as importwallet reads it: each wallet and each
-- portfolio as importwallet would leave it, with the labels and locks of
-- its accounts ("Haskoin.Wallet.Labels"). The file is refused when it
-- holds nothing a file of a wallet holds. Its bytes are in no form of a
-- transaction file either, so the refusal names both kinds.
--
-- The accounts of the file on the networks hw hides are left out, with
-- their labels and locks, as every answer leaves those networks out.
-- importwallet still imports them: the file is the whole wallet. An
-- account importwallet would leave aside is shown whatever its network,
-- since that is a problem of the file to tell.
shownWalletFile :: Ctx -> Config -> Target -> ImportScope -> FilePath -> BS.ByteString -> ExceptT String (DB IO) Response
shownWalletFile ctx cfg target fileScope file bytes = do
  when (isJust target.network) $
    throwError "show takes no --network with a file of a wallet: each of its records names its network"
  content <- either (const (throwError neither)) return (decodeUtf8' bytes)
  let walletFile = readWalletFile content
  unless (holdsAnything walletFile) $ throwError neither
  shown <- fileShown ctx fileScope walletFile
  return $ ResponseWalletFile shown {shownAccounts = filter (onShownNetwork . fst) (shownAccounts shown)}
  where
    onShownNetwork = \case
      AccountAdded ref -> isShown cfg (accountRefNetwork ref)
      AccountHeld ref -> isShown cfg (accountRefNetwork ref)
      AccountLeftAside _ _ -> True
    neither = couldNotRead file "Neither a transaction file, a PSBT in bytes or in base64 text or the hexadecimal of a transaction, nor a file of a wallet that importwallet reads"

-- | A transaction of the wallet, pending or on the chain, as each
-- portfolio that holds it sees it, on the networks hw shows, or on the
-- --network when given. The short id is matched among the transactions of
-- every portfolio, as label matches it.
shownTxs :: Ctx -> Config -> Target -> HashRef -> ExceptT String (DB IO) Response
shownTxs ctx cfg target named = do
  hash <- walletTxHashOf named
  views <- shownViews cfg named =<< transactionViews ctx hash
  let kept =
        [ (place, info)
          | (pool, place, info) <- views,
            maybe True (== pool.poolNetwork) target.network
        ]
  when (null kept) $
    throwError $
      "The transaction "
        <> hashRefText named
        <> " is not in the wallet"
        <> maybe "" ((" on " <>) . (.name)) target.network
  ResponseHeldTxs kept <$> activePortfolioRef

-- | The views of a transaction the user named, among those of every
-- portfolio that holds it: those on the networks hw shows ('shownAmong')
shownViews :: Config -> HashRef -> [(Pool, Place, TxInfo)] -> ExceptT String (DB IO) [(Pool, Place, TxInfo)]
shownViews cfg named = shownAmong cfg ("The transaction " <> hashRefText named) (\(pool, _, _) -> pool.poolNetwork)

-- | The addresses of a chain of a portfolio, a section for each of its
-- accounts on the networks hw shows, or for those --network and --type
-- keep, in the order of the networks and of the types. In each section
-- the newest come first, each with the balance it can spend now. An
-- address names the one to go on after: the listing starts in the section
-- of its account. An address the test networks share is in the section of
-- each, and is refused without --network ('sectionOfAddress').
addressesListing :: Config -> Portfolio -> Target -> Bool -> Paging Text -> ExceptT String (DB IO) Sections
addressesListing cfg portfolio target internal paging = do
  accounts <- listedAccounts cfg portfolio target.network target.kind
  let addrType = if internal then AddrInternal else AddrExternal
      section startM (accId, acc) =
        let net = accountNetwork acc
         in Listing
              { listingStart = startM,
                listingRows = \afterM countM -> do
                  addrs <- lift $ addressesAfter accId addrType afterM countM
                  balances <- spendableAtAddresses net accId (dBAddressAddress <$> addrs)
                  return [(addr, spendableAt balances addr) | addr <- addrs],
                listingCursor = Right . dBAddressIndex . fst,
                listingAnswer = grouped ResponseAddresses (groupOf portfolio net [(accId, acc)]),
                listingWhole = False
              }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (accounts, Nothing)
    Just text -> do
      tried <- forM accounts $ \(accId, acc) -> lift (runExceptT (listedAddress (accountNetwork acc) accId addrType text))
      found <- liftEither $ sectionOfAddress text (zip (accountNetwork . snd <$> accounts) (either (const Nothing) Just <$> tried))
      case (found, tried) of
        (Just (i, row), _) -> return (drop i accounts, Just (dBAddressIndex row))
        -- The one account of the listing says why the address is not its own
        (Nothing, [Left refusal]) -> throwError refusal
        (Nothing, _) -> throwError ("The address " <> cs text <> " is not among the addresses listed")
  return $ Sections (length accounts > 1) paging.pagingMax (zipWith section (startM : repeat Nothing) from)

-- | The addresses of a portfolio that hold coins, receiving and change
-- addresses alike, each with the balance of its coins, a section for each
-- network hw shows, or for the one --network keeps. --type keeps the
-- addresses of one type. Each section comes with the total of its
-- network. An address names the one to go on after, with the balance it
-- holds now: the listing starts in the section of its network. An address
-- the test networks share may hold coins on several, and is then refused
-- without --network ('sectionOfAddress').
balancesListing :: Config -> Portfolio -> Target -> Paging Text -> ExceptT String (DB IO) Sections
balancesListing cfg portfolio target paging = do
  accounts <- listedAccounts cfg portfolio target.network target.kind
  let networks = byNetwork accounts
  sections <- forM networks $ \(net, onNet) -> do
    let accIds = fst <$> onNet
    -- The total of the network, over every address the section covers
    total <- sumCoinBalances . map snd <$> coinBalancesAfter net accIds Nothing Nothing
    return $ \startM ->
      Listing
        { listingStart = startM,
          listingRows = coinBalancesAfter net accIds,
          listingCursor = Right,
          listingAnswer = grouped ResponseBalances (\rows -> (groupOf portfolio net onNet rows, total)),
          listingWhole = False
        }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (sections, Nothing)
    Just text -> do
      held <- forM networks $ \(net, onNet) ->
        if isJust (textToAddr net text) then (net,) . listToMaybe <$> coinBalancesAt net (fst <$> onNet) [text] else return (net, Nothing)
      found <- liftEither $ sectionOfAddress text held
      case found of
        Just (i, row) -> return (drop i sections, Just row)
        Nothing -> throwError $ "The address " <> cs text <> " holds no coins among those listed"
  return $ Sections (length networks > 1) paging.pagingMax (zipWith ($) from (startM : repeat Nothing))

-- | The balances of several addresses added up
sumCoinBalances :: [CoinBalance] -> CoinBalance
sumCoinBalances balances =
  CoinBalance
    { coinBalanceTotal = sum (coinBalanceTotal <$> balances),
      coinBalanceSpendable = sum (coinBalanceSpendable <$> balances),
      coinBalanceLockedByTx = sum (coinBalanceLockedByTx <$> balances),
      coinBalanceLockedByUser = sum (coinBalanceLockedByUser <$> balances)
    }

-- | Label what the word before the label names: a receiving address, the
-- receiving address at a path of the active portfolio, or a transaction
-- ('labelTransaction'). With the label alone, describe the active
-- portfolio, or its wallet with --of-wallet ('describeActive').
--
-- An address and a transaction tell where they are, so they are labelled
-- in their portfolio, whichever is active. A path means nothing without a
-- portfolio, so it is one of the active portfolio ('heldAddressAt'). An
-- address or a path the wallet has on several networks is the one on the
-- --network, the only one, or else the one on the default network
-- ('chooseOnNetwork'). --type goes only with a path without its type. An
-- address and a typed path say their type, and a transaction has none.
-- An option that would change nothing is refused rather than ignored, as
-- show refuses it.
cmdLabel :: Ctx -> Config -> Label -> IO Response
cmdLabel ctx cfg opts =
  runDBResponse cfg $
    case opts.labelled of
      Nothing -> describeActive cfg opts
      Just word -> do
        when opts.ofWallet $
          throwError "label takes --of-wallet only with the label alone, which is then the description of the wallet: hw label DESCRIPTION --of-wallet"
        labelled <- liftEither (readLabelled word)
        case labelled of
          LabelledAddress typed -> do
            refuseType "an address"
            (_, accId, row) <- addressByText cfg opts.target.network typed
            addressAnswer accId =<< labelAddress row opts.label
          LabelledPath kindM AddrExternal index -> do
            when (isJust kindM) $ refuseType "a path that starts with its type"
            portfolio <- requireActivePortfolio
            (accId, row) <- heldAddressAt cfg portfolio opts.target.network (kindM <|> opts.target.kind) AddrExternal index
            addressAnswer accId =<< labelAddress row opts.label
          LabelledPath _ AddrInternal _ ->
            throwError changeTakesNoLabel
          LabelledTx named -> do
            refuseType "a transaction"
            when (isJust opts.target.network) $
              throwError "label takes no --network with a transaction: its label is the same in every portfolio that holds it"
            hash <- walletTxHashOf named
            -- One on hidden networks alone is refused before it is
            -- labelled. Otherwise it is labelled wherever it is, and
            -- shown where hw shows it.
            _ <- shownViews cfg named =<< transactionViews ctx hash
            views <- labelTransaction ctx hash opts.label
            ResponseLabelledTxs (onShownNetworks cfg (placeNetwork . fst) views) <$> activePortfolioRef
  where
    refuseType what =
      when (isJust opts.target.kind) $
        throwError ("label takes no --type with " <> what)

-- | Set the description of the active portfolio, or of its wallet with
-- --of-wallet, to the label typed alone. An empty description removes it.
-- The answer shows the active portfolio, with the description of its
-- wallet too.
--
-- A word that has the shape of something label labels, such as an
-- address, is refused ('labelledShapeOf'). The user most likely left out
-- its label, and the portfolio would lose its description for the
-- address. A description is of the whole portfolio, or of the whole
-- wallet, so --network and --type are refused with it.
describeActive :: Config -> Label -> ExceptT String (DB IO) Response
describeActive cfg opts = do
  let typed = cs opts.label
  for_ (labelledShapeOf typed) $ \shape ->
    throwError (typed <> " reads as " <> shape <> ", not as a description. To label it, give the label after it: hw label " <> typed <> " LABEL")
  when (isJust opts.target.network) $
    throwError "label takes no --network with the label alone: a description is of the whole portfolio, or of the whole wallet"
  when (isJust opts.target.kind) $
    throwError "label takes no --type with the label alone: a description is of the whole portfolio, or of the whole wallet"
  liftEither $ checkDescription opts.label
  portfolio <- requireActivePortfolio
  if opts.ofWallet
    then setWalletDescription portfolio.portfolioWalletId opts.label
    else setPortfolioDescription portfolio.portfolioId opts.label
  -- Read again, with the new description
  ResponsePortfolio <$> (viewOfPortfolio cfg =<< getPortfolio portfolio.portfolioId)

{- Coins -}

-- | The coins of a portfolio, or its locked ones, a section for each
-- network hw shows, or for the one --network keeps, in the order coins
-- lists them. --type keeps the coins of one type. A coin names the one to
-- go on after, with its block: the listing starts in the section of its
-- network.
coinsListing :: Config -> Portfolio -> Target -> Bool -> Paging CoinRef -> ExceptT String (DB IO) Sections
coinsListing cfg portfolio target locked paging = do
  accounts <- listedAccounts cfg portfolio target.network target.kind
  let networks = byNetwork accounts
      shown = if locked then LockedCoins else AllCoins
      section startM (net, onNet) =
        Listing
          { listingStart = startM,
            listingRows = listedCoinsAfter (portfolioPool portfolio net) (fst <$> onNet) shown,
            listingCursor = Right . snd,
            listingAnswer = grouped ResponseCoins (groupOf portfolio net onNet) . map fst,
            listingWhole = False
          }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (networks, Nothing)
    Just named -> do
      outpoint <- coinNamed ("The coin " <> coinRefText named <> " is not among those listed") named
      blocks <- forM networks $ \(_, onNet) -> lift (runExceptT (coinBlock (fst <$> onNet) outpoint))
      case [(i, block) | (i, Right block) <- zip [0 :: Int ..] blocks] of
        (i, block) : _ -> return (drop i networks, Just (block, outpoint))
        [] -> throwError ("The coin " <> coinRefText named <> " is not among those listed")
  return $ Sections (length networks > 1) paging.pagingMax (zipWith section (startM : repeat Nothing) from)

-- | Lock a coin at the user's request, and show it. A coin the wallet has
-- on several networks is the one on the --network, or on the default
-- network ('walletCoin').
cmdLockCoin :: Config -> CoinRef -> Maybe Network -> IO Response
cmdLockCoin cfg named netM =
  runDBResponse cfg $ do
    held <- walletCoin cfg netM =<< walletCoinOf named
    coinAnswer held =<< lockCoin held

-- | Unlock a coin the user locked, and show it
cmdUnlockCoin :: Config -> CoinRef -> Maybe Network -> IO Response
cmdUnlockCoin cfg named netM =
  runDBResponse cfg $ do
    held <- walletCoin cfg netM =<< walletCoinOf named
    coinAnswer held =<< unlockCoin held

{- Transactions -}

-- | The pools of a portfolio a listing of transactions covers: every
-- network of the portfolio hw shows, or the --network alone
listedPools :: Config -> Portfolio -> Maybe Network -> ExceptT String (DB IO) [Pool]
listedPools cfg portfolio netM = do
  pools <- lift $ portfolioPools portfolio.portfolioId
  return [pool | pool <- onShownNetworks cfg (.poolNetwork) pools, maybe True (== pool.poolNetwork) netM]

-- | The transactions of a portfolio on the chain, a section for each
-- network hw shows, or for the one --network keeps, in the order txs
-- lists them, each with the balance after it on its network. A
-- transaction names the
-- one to go on after, with where it stands in txs ('Standing') and the
-- balance so far, so that a chunk adds up nothing. Only --after adds up the transactions above it.
-- The listing starts in the section of the first network that has the
-- transaction.
txsListing :: Config -> Portfolio -> Maybe Network -> Paging HashRef -> ExceptT String (DB IO) Sections
txsListing cfg portfolio netM paging = do
  pools <- listedPools cfg portfolio netM
  sections <- forM pools $ \pool -> do
    accounts <- lift $ poolAccounts pool
    return $ \startM ->
      Listing
        { listingStart = startM,
          listingRows = listedTxsAfter pool,
          listingCursor = Right . snd,
          listingAnswer = grouped ResponseTxs (groupOf portfolio pool.poolNetwork accounts) . map fst,
          listingWhole = False
        }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (sections, Nothing)
    Just named -> do
      txid <- chainTxidOf named
      cursors <- forM pools $ \pool -> lift (runExceptT (txsCursorAt pool txid))
      case [(i, cursor) | (i, Right cursor) <- zip [0 :: Int ..] cursors] of
        (i, cursor) : _ -> return (drop i sections, Just cursor)
        [] -> throwError ("The transaction " <> hashRefText named <> " is not among those listed")
  return $ Sections (length pools > 1) paging.pagingMax (zipWith ($) from (startM : repeat Nothing))

-- | The pending transactions of every portfolio of every wallet, on
-- every network hw shows, or on the --network alone ('pendingListing').
-- Unlike the other listings, pendingtxs does not keep to the active
-- portfolio. A pending transaction waits for the user: to be signed, sent
-- or deleted. One left in another portfolio would be forgotten, and the
-- coins it spends stay locked until then. There are few of them, so one
-- list of them all stays short. The commands that take one find its
-- portfolio by themselves, so the user acts on it without a switch.
--
-- pendingtxs keeps to the shown networks, as the other listings do. A
-- transaction pending on a hidden network locks coins of that network
-- alone, which no listing shows either.
pendingTxsListing :: Ctx -> Config -> Maybe Network -> Paging HashRef -> ExceptT String (DB IO) Sections
pendingTxsListing ctx cfg netM paging = do
  pools <- onShownNetworks cfg (.poolNetwork) <$> lift allPools
  pendingListing ctx (Just [pool | pool <- pools, maybe True (== pool.poolNetwork) netM]) paging

-- | The pending transactions of the pools, or of every pool, in one list
-- rather than in sections: by wallet, by portfolio and by network, in the
-- order of the networks of a portfolio, and the newest first on each.
-- Each says where it is. The answer carries the active portfolio, for the
-- screen to name the portfolio after the paths of a transaction of
-- another one. The one to go on after is named by either of its
-- hashes. There are few of them, so the listing is read whole, and its
-- table sizes its columns to all of them. pendingtxs and exportsheet list
-- the pools on the networks hw shows, and pendingtxs those of one network
-- with --network.
pendingListing :: Ctx -> Maybe [Pool] -> Paging HashRef -> ExceptT String (DB IO) Sections
pendingListing ctx poolsM paging = do
  start <- forM paging.pagingAfter (fmap snd . pendingCursor poolsM <=< pendingHashOf)
  activeM <- activePortfolioRef
  return . oneSection paging.pagingMax $
    Listing
      { listingStart = start,
        listingRows = \afterM countM -> do
          held <- sortOn placeOrder <$> pendingAfter ctx poolsM Nothing Nothing
          let following = case afterM of
                Nothing -> held
                Just cursor -> drop 1 $ dropWhile ((/= Just cursor) . pendingHashOfRow) held
          return $ maybe id genericTake countM following,
        listingCursor = maybe (Left "pendingtxs lists the transactions the wallet holds only") Right . pendingHashOfRow,
        listingAnswer = ResponsePendingTxs activeM,
        listingWhole = True
      }
  where
    -- By wallet, portfolio and network: the sort is stable, so it keeps
    -- the newest first in each
    placeOrder (place, _) = (placePortfolio place, networkRank (placeNetwork place))
    pendingHashOfRow (_, info) = case txInfoStatus info of
      Pending pending -> Just (pendingNosigHash pending)
      OnChain _ -> Nothing

-- | Add the transaction of a file to the pending transactions of its
-- portfolio, and show it as the wallet holds it from now on: with the
-- labels of the wallet, not those of the file. hw finds the portfolio by
-- itself: the one that pays, when the transaction moves funds between
-- two, or the active one among several it cannot tell apart
-- ('transactionPool').
cmdImportTx :: Ctx -> Config -> FilePath -> IO Response
cmdImportTx ctx cfg file =
  runDBResponse cfg $ do
    TxFileRead (TxFile netM tsd _ _) _ <- readTxFileIn ctx (configDefaultNetwork cfg) file
    activeM <- lift activePortfolioId
    (poolM, completed) <- transactionPool ctx cfg activeM netM tsd
    pool <- liftMaybe notInWallet poolM
    pendingHash <- importPendingTx ctx pool completed
    txAnswer pool =<< pendingSummary ctx =<< requirePendingTx pendingHash

-- | Write a pending transaction to a file, with the labels of its
-- addresses as the wallet has them now
cmdExportTx :: Ctx -> Config -> HashRef -> FilePath -> IO Response
cmdExportTx ctx cfg named file =
  runDBResponse cfg $ do
    held <- pendingTxNamed cfg named
    checkPathFree file
    let pool = held.pendingTxPool
    info <- txInfoOf ctx pool held.pendingTxData
    writeTxFile ctx (Just pool) file (labelledTxFile pool.poolNetwork info held.pendingTxData)
    return $ ResponseFile file

-- | Delete a pending transaction, named by either of its hashes. The
-- answer gives both, whichever the user typed.
cmdDeleteTx :: Ctx -> Config -> HashRef -> IO Response
cmdDeleteTx ctx cfg named =
  runDBResponse cfg $ do
    held <- pendingTxNamed cfg named
    let tx = txSignDataTx held.pendingTxData
        txidM = if isSigned tx then Just (txHash tx) else Nothing
    storeOn <- liftIO $ webStores ctx cfg
    (coins, addrs) <- deleteTx ctx storeOn (nosigHash tx)
    return $ ResponseDeleteTx (nosigHash tx) txidM coins addrs

{- Payments: preparing, signing and sending -}

-- Sweeps are prepared and signed like payments, with the helpers here.

-- | The refusal of a transaction that no portfolio of the wallet can keep
-- or sign. An offline computer that made one portfolio of a mnemonic gets
-- this for a file of another portfolio of it, so the refusal says how to
-- add the portfolio.
notInWallet :: String
notInWallet =
  "The transaction does not involve this wallet. It may be of a portfolio this database does not have yet: \
  \newportfolio --existing adds the next portfolio of a mnemonic, and importwallet the portfolios of a file"

-- | The keys of an account, as the derivation records of its files name
-- them: with its path in its portfolio, and the fingerprint of its
-- wallet
fileAccountOf :: Ctx -> Portfolio -> DBAccount -> FileAccount
fileAccountOf ctx portfolio acc =
  FileAccount (accountXPubKey ctx acc) (accountKind acc) (accountPath portfolio acc) (portfolioFingerprint portfolio) (accountNetwork acc)

-- | The keys of the accounts of a pool, as its files name them
poolFileAccounts :: Ctx -> Pool -> ExceptT String (DB IO) [FileAccount]
poolFileAccounts ctx pool = do
  portfolio <- getPortfolio pool.poolPortfolio
  map (fileAccountOf ctx portfolio . snd) <$> lift (poolAccounts pool)

-- | A transaction file as read: the transaction file, and the PSBT it was
-- read from when it was one, for the signed transaction to be written
-- into it
data TxFileRead = TxFileRead !TxFile !(Maybe Psbt)

-- | A pending transaction of the wallet, read as a file that names no
-- network and labels no address, and that comes from no PSBT: what a
-- command given a hash works on, in place of the file of a command given
-- --input
heldTxRead :: PendingTx -> TxFileRead
heldTxRead held = TxFileRead (plainTxFile Nothing held.pendingTxData) Nothing

-- | The transaction file at the path. A PSBT, in bytes or in base64 text,
-- is read with its paths relative to the portfolio of the wallet its
-- derivation records name (see "Haskoin.Wallet.Tx.PsbtFile"). A
-- transaction alone, in hexadecimal, as Bitcoin Core's finalizepsbt gives
-- it, is read with the signing data the wallet holds for it, which it
-- must. Anything else is refused.
--
-- The network given is the one of the command, from --network or from
-- the configuration. It says how the signatures another signer left in a
-- PSBT are verified when neither the file nor a portfolio of the wallet
-- names a network ('psbtTxFile').
readTxFileIn :: Ctx -> Network -> FilePath -> ExceptT String (DB IO) TxFileRead
readTxFileIn ctx fallback file = do
  bytes <- liftEitherIO $ readFileBytes file
  txFileOfBytes ctx fallback file bytes

-- | The forms a transaction file takes, as its first bytes tell them
data TxFileForm
  = -- | A PSBT in bytes, as BIP 174 writes it
    PsbtBytes
  | -- | A PSBT in base64 text
    PsbtText
  | -- | A transaction alone, in hexadecimal
    HexTransaction

-- | The form of a transaction file the bytes are in, if any. The spaces
-- and line breaks around a text do not count. A file of a wallet is in
-- none of these forms: it is JSON, which is not all hexadecimal digits.
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
txFileOfBytes :: Ctx -> Network -> FilePath -> BS.ByteString -> ExceptT String (DB IO) TxFileRead
txFileOfBytes ctx fallback file bytes =
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
      -- The accounts of each pool together: a payment may spend the
      -- coins of both
      pools <- lift allAccountPools
      txFile <- refused $ psbtTxFile ctx (Just fallback) pools psbt
      return $ TxFileRead txFile (Just psbt)
    allAccountPools = do
      accounts <- getAccounts
      let poolsHeld = nub [accountPool acc | (_, _, acc) <- accounts]
      return [[fileAccountOf ctx portfolio acc | (portfolio, _, acc) <- accounts, accountPool acc == pool] | pool <- poolsHeld]
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

-- | Write a transaction file as a PSBT, with the keys of the accounts of
-- its pool when it has one, named by the fingerprint of the mnemonic and
-- their paths, at a path that must be free
writeTxFile :: Ctx -> Maybe Pool -> FilePath -> TxFile -> ExceptT String (DB IO) ()
writeTxFile ctx poolM file txFile = do
  accounts <- maybe (return []) (poolFileAccounts ctx) poolM
  psbt <- liftEither $ txFilePsbt ctx accounts txFile
  liftEitherIO $ writeFileBytes file (encodePsbt psbt)

-- | Write a transaction of the network to the --output file, when there is
-- one, with the labels of its addresses as its summary has them, and the
-- keys of its pool when it has one
writeOutput :: Ctx -> Maybe Pool -> Network -> Maybe FilePath -> TxInfo -> TxSignData -> ExceptT String (DB IO) ()
writeOutput ctx poolM net output info tsd = for_ output $ \file -> writeTxFile ctx poolM file (labelledTxFile net info tsd)

-- | The signing data of a transaction just prepared, with the label of
-- --label, if given
withLabel :: Maybe Text -> TxSignData -> TxSignData
withLabel labelM tsd = tsd {txSignDataLabel = fromMaybe noLabel labelM}

-- | Keep a transaction of the pool just prepared: among its pending
-- transactions, and in the --output file when there is one. The answer
-- shows it.
keepPrepared :: Ctx -> Pool -> Maybe FilePath -> TxSignData -> ExceptT String (DB IO) Response
keepPrepared ctx pool output tsd = txAnswer pool =<< storePrepared ctx pool Nothing output tsd

-- | Keep a transaction of the pool just prepared, as 'keepPrepared' does,
-- and give its summary. The address --change-back named, if any, may be
-- used or held by another pending transaction ('importPreparedTx').
--
-- The summary is the one of the transaction as the wallet now holds it
-- ('pendingSummary'), so that the answer shows the time it was created,
-- as pendingtxs and show do from then on. The signing data alone has no
-- time, and the answer started with "time unknown" before.
storePrepared :: Ctx -> Pool -> Maybe Address -> Maybe FilePath -> TxSignData -> ExceptT String (DB IO) TxInfo
storePrepared ctx pool changeBackM output tsd = do
  pendingHash <- importPreparedTx ctx pool changeBackM tsd
  info <- pendingSummary ctx =<< requirePendingTx pendingHash
  writeOutput ctx (Just pool) pool.poolNetwork output info tsd
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
    let net = commandNetwork cfg netM
    storeOn <- liftIO $ webStores ctx cfg
    rates <- recentFeeRates (configParallelRequests cfg) (storeOn net)
    return $ ResponseFees net rates

-- | A transaction just prepared, as the wallet keeps it: what preparetx,
-- preparesendall and bumpfee answer with, and what send goes on from
data Prepared = Prepared
  { -- | The pool that pays it
    preparedPool :: !Pool,
    -- | Its nosigHash, which names it among the pending transactions
    -- until it is signed
    preparedHash :: !TxHash,
    -- | The transaction as the wallet holds it
    preparedInfo :: !TxInfo,
    -- | The coins it leaves in the portfolio, each as coins shows it, and
    -- why it stays
    preparedLeft :: ![LeftCoin]
  }

-- | Keep a transaction of the pool just prepared, as 'keepPrepared' does,
-- with the coins it leaves in the portfolio
keepPreparedLeaving :: Ctx -> Pool -> Maybe Address -> Maybe FilePath -> TxSignData -> [(OutPoint, LeftReason)] -> ExceptT String (DB IO) Prepared
keepPreparedLeaving ctx pool changeBackM output tsd leftOut = do
  left <- leftCoinsOf pool leftOut
  info <- storePrepared ctx pool changeBackM output tsd
  return (Prepared pool (nosigHash (txSignDataTx tsd)) info left)

-- | The answer that shows a transaction just prepared, with the coins it
-- leaves in the portfolio
preparedResponse :: Prepared -> ExceptT String (DB IO) Response
preparedResponse prepared = do
  place <- poolPlace prepared.preparedPool
  ResponsePrepared place prepared.preparedInfo prepared.preparedLeft <$> activePortfolioRef

-- | Keep a transaction of the pool just prepared, as 'keepPrepared' does,
-- and answer with the coins it leaves in the portfolio, each as coins
-- shows it, and why it stays
preparedAnswer :: Ctx -> Pool -> Maybe Address -> Maybe FilePath -> TxSignData -> [(OutPoint, LeftReason)] -> ExceptT String (DB IO) Response
preparedAnswer ctx pool changeBackM output tsd leftOut =
  preparedResponse =<< keepPreparedLeaving ctx pool changeBackM output tsd leftOut

-- | The pool a transaction is prepared from, with the addresses that coin
-- control chose, read on its network. The pool is that of the active
-- portfolio on a network: the --network, or else the one the coins of
-- --coin and the addresses of --spend-from are on, or else the default
-- network. Those coins and addresses must be in the active portfolio: one
-- in another portfolio is refused, with the portfolio that holds it and
-- the switch to it. A payment from another portfolio than the one the
-- user works in would surprise. The test networks share their keys, so
-- the coins and addresses may be on several networks: --network or the
-- default network then names one ('paymentNetwork'). Each --spend-from
-- address must be one of the pool: the address counts, not its text, so a
-- Bitcoin Cash address is found without its prefix, or in its legacy
-- form.
--
-- A --spend-from path, such as bip44/1/0, is one of the active portfolio.
-- It tells the networks where the portfolio has an address at that path,
-- as an address tells the networks that hold it ('rowsAtPath'). The
-- address spent from is then the one at the path on the network of the
-- payment.
--
-- Only the networks hw shows count. A coin or an address the wallet has
-- on hidden networks alone is refused, with the command that shows its
-- network ('shownAmong').
controlledPool :: Config -> Maybe Network -> [OutPoint] -> [AddressNamed] -> ExceptT String (DB IO) (Pool, [Address])
controlledPool cfg netM outpoints spendFrom = do
  portfolio <- requireActivePortfolio
  byCoin <- forM outpoints $ \outpoint -> do
    let what = "The coin " <> shortCoinText outpoint
    holders <- shownAmong cfg what (.poolNetwork) . map (accountPool . snd) =<< coinHolders [outpoint]
    networksInActive what portfolio holders
  byAddress <- forM spendFrom $ \case
    NamedByAddress typed -> do
      let what = "The address " <> cs typed
      holders <- shownAmong cfg what (.poolNetwork) . map (\(_, _, acc) -> accountPool acc) =<< accountsHolding (\net -> maybeToList (textToAddr net typed))
      networksInActive what portfolio holders
    NamedByPath kind addrType index -> onShownNetworks cfg id . map snd <$> lift (rowsAtPath cfg portfolio (Just kind) addrType index)
  net <- liftEither $ paymentNetwork cfg netM (nub (concat (byCoin <> byAddress)))
  let pool = portfolioPool portfolio net
      place = portfolioPlace portfolio net
  addrs <- forM spendFrom $ \named -> do
    addrM <- poolAddressNamed pool named
    case (addrM, named) of
      (Just addr, _) -> return addr
      (Nothing, NamedByAddress typed) -> throwError ("The address " <> cs typed <> " is not in " <> placeText place)
      (Nothing, NamedByPath {}) -> throwError (placeText place <> " has no address at " <> addressNamedText named)
  return (pool, addrs)

-- | The networks of the active portfolio that hold a coin or an address
-- the user gave, among the pools that hold it, or the refusal of one that
-- only another portfolio holds, with the switch to that portfolio. The
-- text names what was given, as in "The coin a1075db5:1". Nothing at all
-- holds a coin the wallet does not have, which coin control refuses
-- later, with the reason.
networksInActive :: String -> Portfolio -> [Pool] -> ExceptT String (DB IO) [Network]
networksInActive what active holders =
  case ([pool.poolNetwork | pool <- holders, pool.poolPortfolio == active.portfolioId], holders) of
    ([], holder : _) -> do
      other <- getPortfolio holder.poolPortfolio
      throwError (what <> " is in " <> portfolioText other <> ", not in the active portfolio: run " <> switchCommandText (portfolioRef other) <> " first")
    (networks, _) -> return networks

-- | The network of a payment: the --network, or else the one the coins
-- and addresses of coin control are on, given as the networks that hold
-- them, or else the default network. When they are on several, as the
-- test networks share their keys, the default network is taken if it is
-- one of them. A --network that holds none of them is refused: the coins
-- would not be found there.
paymentNetwork :: Config -> Maybe Network -> [Network] -> Either String Network
paymentNetwork cfg netM held = case (netM, held) of
  (Just net, _)
    | null held || net `elem` held -> Right net
    | otherwise -> Left ("The coins or addresses given are on " <> networksText held <> ", not on the --network " <> net.name)
  (Nothing, []) -> Right (configDefaultNetwork cfg)
  (Nothing, [only]) -> Right only
  (Nothing, several)
    | configDefaultNetwork cfg `elem` several -> Right (configDefaultNetwork cfg)
    | otherwise -> Left ("The coins or addresses given are on " <> networksText several <> ": give the --network")

-- | Refuse the coins chosen with --coin or --spend-from that --coin-type
-- leaves out. The filter never adds a coin to those the choosers chose.
-- It only narrows them, and a chosen coin it would drop means the two
-- options disagree. Dropping the coin without a word would hide that.
refuseOtherType :: Maybe AccountKind -> [Store.Unspent] -> ExceptT String (DB IO) ()
refuseOtherType kindM chosen =
  for_ kindM $ \kind ->
    for_ (filter ((/= kind) . coinKind) chosen) $ \coin ->
      throwError $
        "The coin " <> shortCoinText coin.outpoint <> " is a " <> cs (kindName (coinKind coin)) <> " coin, and --coin-type " <> cs (kindName kind) <> " spends " <> cs (kindName kind) <> " coins only"

-- | Prepare a payment from the active portfolio on a network, unsigned,
-- and answer with it: 'preparePayment', which send starts with too, says
-- how the options are read.
cmdPrepareTx :: Ctx -> Config -> AmountUnit -> PrepareTx -> IO Response
cmdPrepareTx ctx cfg unit opts =
  runDBResponse cfg $ do
    storeOn <- liftIO $ webStores ctx cfg
    preparedResponse =<< preparePayment ctx cfg storeOn unit opts

-- | Build the payment the options ask for, and keep it as a pending
-- transaction of the active portfolio, unsigned, which locks its coins.
-- It may spend the coins of both types of address together. This is all
-- that preparetx does but its answer, and what send starts with. The two
-- commands share it, so they make the same transaction of the same
-- options, with the same questions and the same refusals. The stores are
-- those of the command. The fee rates of recent blocks are read from one
-- when the options give no rate.
--
-- Coin control has choosers and filters. The choosers, --coin and
-- --spend-from, add up: the coins chosen are those of every --coin and
-- those at every --spend-from address. The payment spends all of those
-- that can be spent, and only them. With --select, the strategy selects
-- among them instead, as it does among every coin of the pool without a
-- chooser. The filters, --min-conf and --coin-type, narrow the coins down:
-- every coin of the pool, or the coins chosen. They never add a coin, so
-- --coin-type with a --spend-from of the same type spends the coins at
-- that address alone. The answer lists the coins chosen that cannot be
-- spent, and why. The chosen coins --select does not need are not listed,
-- like the coins a selection leaves without a chooser.
--
-- The change goes to a new change address of the portfolio, of the type
-- the builder works out ("Haskoin.Wallet.Tx.Building".'changeKindRule').
-- --change-back sends it back to an address of the portfolio on the
-- network of the payment instead: the one it names, by itself or by its
-- path, or with largest that of the largest coin the payment spends, of
-- the type of the change.
preparePayment :: Ctx -> Config -> (Network -> Store) -> AmountUnit -> PrepareTx -> ExceptT String (DB IO) Prepared
preparePayment ctx cfg storeOn unit opts = do
  coins <- mapM walletCoinOf opts.coins
  (pool, spendFrom) <- controlledPool cfg opts.network coins opts.spendFrom
  let net = pool.poolNetwork
      controlled = not (null coins && null spendFrom)
  when (opts.select && not controlled) $
    throwError "--select needs coins to select among: give --coin or --spend-from"
  -- An address named has a type of its own. largest picks a coin of the
  -- type of the change, which --change-type may set.
  case (opts.changeBack, opts.changeType) of
    (Just (BackToNamed _), Just _) ->
      throwError "--change-back ADDRESS names the address of the change, which has a type: it takes no --change-type. --change-back largest does."
    _ -> return ()
  -- A type the network does not have is refused before anything is asked
  for_ opts.coinType $ liftEither . checkKindOnNetwork net
  for_ opts.changeType $ \kind -> do
    liftEither $ checkKindOnNetwork net kind
    void $ poolAccountOf pool kind
  changeTo <- case opts.changeBack of
    Nothing -> return (NewChangeAddress opts.changeType)
    Just BackToLargestCoin -> return (ChangeToLargestCoin opts.changeType)
    Just (BackToNamed named) -> do
      addrM <- poolAddressNamed pool named
      place <- poolPlace pool
      let refused = "The change can only go back to an address of " <> placeText place <> ", and "
      case (addrM, named) of
        (Just addr, _) -> return (ChangeBackTo addr)
        (Nothing, NamedByAddress typed) -> throwError (refused <> cs typed <> " is not one")
        (Nothing, NamedByPath {}) -> throwError (refused <> "it has none at " <> addressNamedText named)
  -- Before the transaction is built, not after
  for_ opts.output checkPathFree
  recipients <- liftEither $ mapM (readPayment net unit) opts.recipients
  heightM <- lockTimeHeight opts.yes net
  feeByte <- chooseFeeRate cfg net (storeOn net) 1 opts.feeRate
  confirmFeeRate opts.yes feeByte
  (control, left) <-
    if not controlled
      then return (NoCoinControl, [])
      else do
        spendable <- spendableCoins ctx pool opts.minConf
        unspendable <- unspendableCoins ctx pool opts.minConf
        (chosen, held) <- liftEither $ chosenCoins coins spendFrom spendable unspendable
        refuseOtherType opts.coinType (chosen <> (fst <$> held))
        let outpoints = (.outpoint) <$> chosen
        return (if opts.select then SelectAmongChosen outpoints else SpendChosen outpoints, leftOutpoints [] held [])
  gen <- liftIO initStdGen
  let (lockTime, buildGen) = lockTimeFor heightM gen
  prepared <-
    buildTxSignData ctx cfg buildGen opts.strategy pool opts.coinType recipients feeByte lockTime opts.feePayer opts.minConf control changeTo
      `catchError` tellingHeldBack ctx unit pool opts.minConf coinsDoNotCover
  keepPreparedLeaving ctx pool prepared.changeChosen opts.output (withLabel opts.label prepared.signData) left

-- | Fail as a payment failed, and, when it failed for the reason given,
-- say what the portfolio holds on the network that it cannot spend now.
--
-- "The coins do not cover the amount and the fee" was all the user read
-- when the portfolio held enough: a payment of somebody else that waits
-- for its block, a coinbase under a hundred blocks, coins locked by a
-- pending transaction. The balance said one thing and the refusal
-- another. The sentence added gives the amount and points to coins, which
-- says why each coin waits.
tellingHeldBack :: Ctx -> AmountUnit -> Pool -> Natural -> String -> String -> ExceptT String (DB IO) a
tellingHeldBack ctx unit pool minConf reason failure
  | failure /= reason = throwError failure
  | otherwise = do
      held <- unspendableCoins ctx pool minConf
      let value = sum [toInteger coin.value | (coin, _) <- held]
          coins = if length held == 1 then "1 coin" else show (length held) <> " coins"
      throwError $
        if null held
          then failure
          else
            failure
              <> ". The portfolio holds "
              <> cs (showAmount unit value)
              <> " "
              <> cs (showUnit unit value)
              <> " more on "
              <> pool.poolNetwork.name
              <> ", in "
              <> coins
              <> " it cannot spend now: hw coins says why"

-- | Prepare a transaction sending the coins of the portfolio on a network
-- to the recipients, unsigned, with no change: all the coins it can spend
-- now, of both types, or those that the choosers of coin control chose,
-- as for preparetx. The filters narrow them down as for preparetx:
-- --coin-type sends the coins of one type. Without --coin and
-- --spend-from, the coins worth less than their fee are spent or left as
-- the user answers, or as --spend-dust or --leave-dust says. The answer
-- lists every coin left in the portfolio on the network, and why. See
-- "Haskoin.Wallet.Tx.SendAll".
cmdPrepareSendAll :: Ctx -> Config -> AmountUnit -> PrepareSendAll -> IO Response
cmdPrepareSendAll ctx cfg unit opts =
  runDBResponse cfg $ do
    coins <- mapM walletCoinOf opts.coins
    (pool, spendFrom) <- controlledPool cfg opts.network coins opts.spendFrom
    let net = pool.poolNetwork
        controlled = not (null coins && null spendFrom)
    for_ opts.coinType $ liftEither . checkKindOnNetwork net
    -- Before the transaction is built, not after
    for_ opts.output checkPathFree
    recipients <- liftEither $ mapM (readRecipient net unit) opts.recipients
    liftEither $ checkSharing recipients
    when (controlled && isJust opts.dustCoins) $
      throwError "The coins chosen with --coin or --spend-from are all sent: give --spend-dust or --leave-dust without them"
    heightM <- lockTimeHeight opts.yes net
    storeOn <- liftIO $ webStores ctx cfg
    feeByte <- chooseFeeRate cfg net (storeOn net) 1 opts.feeRate
    confirmFeeRate opts.yes feeByte
    spendable <- spendableCoins ctx pool opts.minConf
    unspendable <- unspendableCoins ctx pool opts.minConf
    (sent, left) <-
      if controlled
        then do
          (sent, held) <- liftEither $ chosenCoins coins spendFrom spendable unspendable
          refuseOtherType opts.coinType (sent <> (fst <$> held))
          return (sent, leftOutpoints [] unspendable (notSent sent spendable))
        else do
          -- The coins of the other type stay, whatever they are worth
          let (ofType, others) = partition (\coin -> maybe True (== coinKind coin) opts.coinType) spendable
          sent <- coinsWithDust pool feeByte recipients (not (null unspendable && null others)) ofType
          return (sent, leftOutpoints (notSent sent ofType) unspendable [] <> leftOfOtherType others)
    gen <- liftIO initStdGen
    let (lockTime, buildGen) = lockTimeFor heightM gen
    tsd <-
      buildSendAllSignData ctx buildGen pool sent recipients feeByte lockTime
        `catchError` tellingHeldBack ctx unit pool opts.minConf noCoinsToSend
    preparedAnswer ctx pool Nothing opts.output (withLabel opts.label tsd) left
  where
    -- The spendable coins the transaction does not send
    notSent sent = filter (`notElem` sent)
    -- The spendable coins, with or without the dust coins. They are left
    -- without a question when spending them too would leave too little for
    -- the recipients, and when the other coins cannot pay the recipients
    -- either, which the builder then tells.
    coinsWithDust :: Pool -> Natural -> [(Address, Share)] -> Bool -> [Store.Unspent] -> ExceptT String (DB IO) [Store.Unspent]
    coinsWithDust pool feeByte recipients othersStay spendable
      | null dust || null worth = return spendable
      | otherwise =
          case opts.dustCoins of
            Just SpendDust -> return spendable
            Just LeaveDust -> return worth
            Nothing
              | isLeft (paying spendable) || isLeft (paying worth) -> return worth
              | otherwise -> askAboutDust (dustReport ctx feeByte recipients worth dust othersStay)
      where
        (dust, worth) = partition (isDustCoin feeByte) spendable
        paying sending = sendAllAmounts ctx feeByte sending recipients
        -- On stderr, with the question, to keep the --json answer clean.
        -- Each coin is shown as coins shows it.
        askAboutDust :: DustReport -> ExceptT String (DB IO) [Store.Unspent]
        askAboutDust report = do
          views <- poolCoinViews pool ((.outpoint) <$> report.dustCoins)
          liftIO $ renderOn stderr =<< dustReportDoc pool.poolNetwork unit views report
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
-- the network the file names and the labels it carries, the pool of the
-- wallet it involves, if any, which hw finds by itself
-- ('transactionPool'), and the file to
-- write it to once signed. A pending transaction given by its nosigHash
-- is as a file that names no network and labels no address. A
-- transaction signed already, or sent, is not signed again; the --output
-- file must be free, and the --input file is replaced only if the user
-- agrees, all before the user types a secret.
transactionToSign ::
  Ctx ->
  Config ->
  -- | The --network of the command, for a file that names none
  Maybe Network ->
  Signing ->
  ExceptT String (DB IO) (Maybe Pool, TxFileRead, SignedFile)
transactionToSign ctx cfg givenM signing = do
  TxFileRead txFile@(TxFile netM tsd _ _) sourceM <- case (signing.nosigHash, signing.input) of
    (Nothing, Nothing) ->
      throwError "Give the NOSIGHASH of a pending transaction, or an --input file"
    (Just _, Just _) ->
      throwError "Can not specify both a NOSIGHASH and an --input file"
    (Just named, _) -> do
      held <- pendingTxNamed cfg named
      when held.pendingTxOnline $
        throwError "The transaction is already online"
      return (heldTxRead held)
    (_, Just file) -> readTxFileIn ctx (commandNetwork cfg givenM) file
  when (isSigned (txSignDataTx tsd)) $
    throwError "The transaction is already signed"
  named <- fileNetwork givenM netM
  -- The pool found is the one that pays, if any: the keys of a portfolio
  -- that does not pay the transaction sign none of its inputs
  activeM <- lift activePortfolioId
  (poolM, completed) <- transactionPool ctx cfg activeM named tsd
  signedFile <- case (signing.input, signing.output) of
    (_, Just file) -> NewFile file <$ checkPathFree file
    (Just file, Nothing) -> do
      replacing <- askYes terminal ("Replace " <> file <> " with the signed transaction? Give an --output file to write it elsewhere.")
      unless replacing $ throwError "The transaction was not signed: give an --output file to write it to"
      return (ReplacedFile file)
    (Nothing, Nothing) -> return NoSignedFile
  return (poolM, TxFileRead txFile {txFileData = completed} sourceM, signedFile)

-- | Show the transaction about to be signed, before the secret is asked
-- for, as show shows it: with the wallet's labels for a pending
-- transaction, those of the file for a file, and either way with the
-- addresses another portfolio of the database holds named as such, and
-- with the time the wallet created it when it holds it. On stderr, as the
-- questions, to keep the --json answer clean.
showToSign :: Ctx -> AmountUnit -> Signing -> Network -> Maybe Pool -> TxFile -> ExceptT String (DB IO) ()
showToSign ctx unit signing net poolM txFile = do
  let tsd = txFileData txFile
  (info, seenM) <- case poolM of
    Just pool -> do
      keys <- lift $ poolKeys ctx pool
      parsed <- liftEither $ parseTxSignData net ctx keys tsd
      filled <-
        lift $
          if isJust signing.nosigHash
            then fillTxInfo pool parsed
            else fillElsewhere pool (labelsFromFile txFile parsed)
      -- A transaction the pool holds is shown with what the database
      -- knows of it, the time it was created first, as pendingtxs shows it
      heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
      let info = case heldM of
            Just held | held.pendingTxPool == pool -> filled {txInfoStatus = heldStatus held filled.txInfoStatus}
            _ -> filled
      seen <- seenFromPlace =<< poolPlace pool
      return (info, Just seen)
    Nothing -> (,Nothing) <$> liftEither (parseOutsideTxSignData net ctx tsd)
  naming <- lift walletNaming
  doc <- liftIO $ txAnswerDoc naming net seenM unit info
  liftIO $ renderOn stderr doc

-- | Write a transaction just signed to its file, if any, with the keys of
-- its pool when it has one: into the PSBT the --input file held, which
-- keeps every field of it, or as a new PSBT for a transaction that came
-- from the wallet
writeSigned :: Ctx -> Maybe Pool -> Maybe Psbt -> SignedFile -> TxFile -> ExceptT String (DB IO) ()
writeSigned ctx poolM sourceM signedFile signed =
  case signedFile of
    NoSignedFile -> return ()
    NewFile file -> write (writeFileBytes file)
    ReplacedFile file -> write (replaceFileBytes file)
  where
    write put = do
      accounts <- maybe (return []) (poolFileAccounts ctx) poolM
      psbt <- liftEither $ case sourceM of
        Just source -> signedPsbt ctx accounts source signed
        Nothing -> txFilePsbt ctx accounts signed
      liftEitherIO $ put (encodePsbt psbt)

-- | Keep a transaction just signed: in place of the pending one when it was
-- given by its nosigHash, and in its file when there is one. It is given
-- back with its labels, as the answer shows it: the wallet's for a pending
-- transaction, and those the --input file carries for a transaction of a
-- file, which go on to the signed file. The file is what an offline
-- computer has.
keepSigned :: Ctx -> Signing -> SignedFile -> Pool -> TxFileRead -> TxSignData -> TxInfo -> ExceptT String (DB IO) TxInfo
keepSigned ctx signing signedFile pool (TxFileRead file sourceM) signed info = do
  labelled <-
    lift $
      if fromWallet
        then fillTxInfo pool info
        else fillElsewhere pool (labelsFromFile file info)
  when fromWallet $ void $ importPendingTx ctx pool signed
  writeSigned ctx (Just pool) sourceM signedFile (labelledTxFile pool.poolNetwork labelled signed)
  return labelled
  where
    fromWallet = isJust signing.nosigHash

-- | Sign a transaction of a portfolio with the keys of its mnemonic, or of
-- the shares of its backup, and answer with it: 'signWalletTx', which
-- send signs with too. The transaction is shown first, as show shows it.
cmdSignTx :: Ctx -> Config -> AmountUnit -> Signing -> IO Response
cmdSignTx ctx cfg unit signing =
  runDBResponse cfg $ do
    (pool, signed) <- signWalletTx ctx cfg signing $ \pool -> showToSign ctx unit signing pool.poolNetwork (Just pool)
    txAnswer pool signed

-- | Sign a transaction of a portfolio with the keys of its mnemonic, or of
-- the shares of its backup, and keep it signed ('keepSigned'): all that
-- signtx does but its answer, and the second step of send. The keys are
-- those of every account the portfolio has on the network of the
-- transaction, since a payment may spend the coins of both. The pool of
-- the transaction is given back with the transaction as signed.
--
-- The transaction is shown first, by the action given, so that the user
-- sees what the mnemonic is typed for: the prompt is the confirmation,
-- and an interrupt stops it. The command says how it shows it. Every
-- refusal comes before that, and so before the user types a secret
-- ('transactionToSign').
signWalletTx ::
  Ctx ->
  Config ->
  Signing ->
  -- | Show the transaction to sign, given its pool and as its file reads
  (Pool -> TxFile -> ExceptT String (DB IO) ()) ->
  ExceptT String (DB IO) (Pool, TxInfo)
signWalletTx ctx cfg signing showFirst = do
  (poolM, file@(TxFileRead txFile _), signedFile) <- transactionToSign ctx cfg Nothing signing
  pool <- liftMaybe notInWallet poolM
  let net = pool.poolNetwork
  portfolio <- getPortfolio pool.poolPortfolio
  accounts <- lift $ poolAccounts pool
  showFirst pool txFile
  keysOfPool <- lift $ poolKeys ctx pool
  (seed, keys) <- askSigningKeys terminal net ctx (portfolioIndex portfolio.portfolioRow) keysOfPool
  -- The signed data names the seed, for the derivation records of its
  -- file
  let named = (txFileData txFile) {txSignDataFingerprint = Just (seedFingerprint seed)}
  (signed, info) <- liftEither $ signHeldWalletTx net ctx (heldPoolOf (snd <$> accounts)) named keys
  (pool,) <$> keepSigned ctx signing signedFile pool file signed info

-- | Send a signed transaction: a pending one of the wallet, by its id or
-- its nosigHash, or the one of a file. One the wallet holds goes on the
-- network of its portfolio, which the file or --network may only name; one
-- it does not hold goes on the network its file names, or on that of
-- --network or of the configuration when the file names none. See
-- 'sendSigned'. The user sees the transaction and says whether to send
-- it, unless --yes.
cmdBroadcast :: Ctx -> Config -> AmountUnit -> Maybe HashRef -> Maybe FilePath -> Bool -> Maybe Network -> IO Response
cmdBroadcast ctx cfg unit namedM fileM yes givenM =
  case (namedM, fileM) of
    (Just named, Nothing) -> sending (heldTxRead <$> pendingToSend named)
    (Nothing, Just file) -> sending (readTxFileIn ctx (commandNetwork cfg givenM) file)
    (Nothing, Nothing) -> return $ ResponseError "Give the TXID of a pending transaction or an --input file"
    (Just _, Just _) -> return $ ResponseError "Can not specify both a TXID and an --input file"
  where
    -- The pending transaction named. A hash that names a transaction of
    -- the history gets an answer of its own. A sync has seen that
    -- transaction on the network and took it off the pending ones, so
    -- "The pending transaction does not exist" left the user of a payment
    -- that stays unconfirmed with nothing to do next.
    pendingToSend named =
      pendingTxNamed cfg named `catchError` \failure -> do
        inHistory <- lift $ runExceptT (chainTxidOf named)
        throwError $ case inHistory of
          Right _ | failure == noPendingTx -> sentAlready named
          _ -> failure
    sentAlready named =
      "The transaction "
        <> hashRefText named
        <> " is not pending: it was sent, and a sync has seen it on the network, so there is nothing to send."
        <> " txs shows where it stands. A payment of Bitcoin that stays unconfirmed is replaced with bumpfee"
    sending readTx =
      runDBResponse cfg $ do
        storeOn <- liftIO $ webStores ctx cfg
        sendTxRead ctx cfg storeOn unit yes givenM =<< readTx

-- | Send a signed transaction, read from the wallet or from a file, and
-- answer with it as sent: all that broadcast does once it has read its
-- transaction, and the last step of send. See 'sendSigned' for what is
-- sent where. The user sees the transaction and says whether to send it,
-- unless the command says not to ask. broadcast says so with --yes. send
-- always does: it showed the transaction before it asked for the
-- mnemonic, and typing the mnemonic was the answer.
--
-- The labels of a file are left aside: the transaction is shown as the
-- wallet holds it.
sendTxRead ::
  Ctx ->
  Config ->
  -- | The stores of the command
  (Network -> Store) ->
  AmountUnit ->
  -- | Whether to send without showing the transaction and asking
  Bool ->
  -- | The --network of the command, for a file that names none
  Maybe Network ->
  TxFileRead ->
  ExceptT String (DB IO) Response
sendTxRead ctx cfg storeOn unit yes givenM (TxFileRead (TxFile netM tsd _ _) _) = do
  named <- fileNetwork givenM netM
  -- A file of a hidden network, or of a transaction pending on one, is
  -- refused, as the pending transaction named by its hash is
  heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
  _ <- shownAmong cfg "The transaction" id (maybeToList named <> [held.pendingTxPool.poolNetwork | Just held <- [heldM]])
  sent <- sendSigned ctx storeOn beforeSending named (commandNetwork cfg named) tsd
  case sent of
    SentPending place info -> ResponseTx place info <$> activePortfolioRef
    SentOutside net txid -> return (ResponseSentTx net txid)
  where
    -- On stderr, with the question, to keep the --json answer clean
    beforeSending net placeM info =
      unless yes $ do
        seenM <- mapM seenFromPlace placeM
        naming <- lift walletNaming
        liftIO $ renderOn stderr =<< txAnswerDoc naming net seenM unit info
        confirmed <- askYes terminal "Send this transaction to the network? This cannot be undone."
        unless confirmed $ throwError "The transaction was not sent"

-- | What send tells the user after the transaction and before the
-- mnemonic. signtx signs and stops, and broadcast asks before it sends.
-- send asks nothing after the mnemonic, so the user must know by then that
-- typing it sends the payment.
sendNotice :: String
sendNotice = "Typing the mnemonic signs this transaction and sends it to the network. This cannot be undone."

-- | Make a payment in one command: prepare it as preparetx does, sign it
-- as signtx does, and send it as broadcast does. The three steps are
-- those of the three commands ('preparePayment', 'signWalletTx' and
-- 'sendTxRead'), run one after the other, so the payment and what the
-- wallet holds of it afterwards are the same either way. The command
-- takes the options of preparetx, without an --output file.
--
-- The command is for a wallet whose mnemonic is typed on the online
-- computer anyway. It asks for the mnemonic and needs the store, so it
-- cannot keep the mnemonic offline. The three commands stay for that.
--
-- The transaction is shown once, before the mnemonic is asked, as
-- preparetx answers with it: with the coins it leaves in the portfolio.
-- Nothing of it changes between then and the sending, so it is not shown
-- again, and the answer on the screen is its txid alone. Typing the
-- mnemonic is the confirmation. broadcast asks whether to send, and send
-- does not: the user saw the transaction, was told that the mnemonic
-- sends it ('sendNotice'), and typed it.
--
-- What a failure leaves depends on whether the transaction was signed:
--
-- * Before the signature, nothing is kept. A refusal, a no to a question
--   and a mnemonic not typed all undo the pending transaction. The user
--   did not confirm the payment, and an unsigned transaction left behind
--   would lock its coins until a deletetx.
--
-- * After the signature, the signed transaction is kept, as signtx leaves
--   it ('keepSoFar'). It is in the wallet before it is handed to the
--   network. A sending that fails, or that is interrupted while the store
--   waits for its nodes, may have reached the network all the same. The
--   coins must then stay locked, and broadcast must be able to send the
--   transaction again. The failure says so, and names the transaction.
--
-- The store is asked whether it is well before the mnemonic is asked, so
-- that a store that is down does not cost the user a mnemonic typed for
-- nothing.
cmdSend :: Ctx -> Config -> AmountUnit -> PrepareTx -> IO Response
cmdSend ctx cfg unit opts =
  runDBResponse cfg $ do
    storeOn <- liftIO $ webStores ctx cfg
    -- As preparetx: the transaction is pending, and its coins are locked
    prepared <- preparePayment ctx cfg storeOn unit opts
    -- A store that is down stops the command here, before the mnemonic
    fromStore $ checkHealth (storeOn prepared.preparedPool.poolNetwork)
    -- As signtx NOSIGHASH
    _ <- signWalletTx ctx cfg (signingPending prepared.preparedHash) $ \_ _ -> showBeforeMnemonic prepared
    -- The signed transaction is in the wallet from here on, whatever
    -- comes of the sending
    keepSoFar
    -- As broadcast TXID --yes
    signed <- requirePendingTx prepared.preparedHash
    sendTxRead ctx cfg storeOn unit True Nothing (heldTxRead signed) `catchError` (throwError . withWhatIsKept signed)
  where
    -- The transaction as preparetx answers with it, then, after a blank
    -- line, what typing the mnemonic does. On stderr, as the questions, to
    -- keep the --json answer clean.
    showBeforeMnemonic :: Prepared -> ExceptT String (DB IO) ()
    showBeforeMnemonic prepared = do
      place <- poolPlace prepared.preparedPool
      activeM <- activePortfolioRef
      naming <- lift walletNaming
      liftIO $ renderOn stderr =<< preparedDoc naming unit place prepared.preparedInfo prepared.preparedLeft activeM
      liftIO $ tell terminal ("\n" <> sendNotice)
    -- Why the sending failed, then what the wallet holds and what to do
    -- with it. The user typed the mnemonic and reads a failure, so the
    -- failure says that the signature is not lost. The transaction is
    -- named by the short id of its txid, as the next steps name one.
    withWhatIsKept :: PendingTx -> String -> String
    withWhatIsKept signed failure =
      let short = shortHashText (txHash (txSignDataTx signed.pendingTxData))
       in failure
            <> ". The transaction is signed and stays pending: broadcast "
            <> short
            <> " sends it again, and deletetx "
            <> short
            <> " deletes it and frees its coins"

-- | Prepare the replacement of a stuck transaction of a portfolio at a
-- higher fee rate, as "Haskoin.Wallet.Tx.BumpFee" builds it. The
-- transaction tells its portfolio: the one that pays it, when it is in
-- the history of two. hw asks before it replaces the transactions that
-- spend the change of the stuck one, and before it adds coins of the
-- portfolio, unless --yes. When no coin is added, it asks before the
-- recipients pay out of what they receive, and which of several does,
-- unless --yes, which has them all pay in equal shares when they can.
-- --recipients-pay has the recipients pay what the higher fee adds
-- instead, in equal shares: the change keeps its amount, and no coin is
-- added ('fundings'). Every refusal comes before any question: the
-- replacement of every way to pay the fee is built first, and asked about
-- after. The answer is that of preparetx.
cmdBumpFee :: Ctx -> Config -> AmountUnit -> BumpFee -> IO Response
cmdBumpFee ctx cfg unit opts =
  runDBResponse cfg $ do
    -- Before anything is asked, not after
    for_ opts.output checkPathFree
    bumped <- walletTxHashOf opts.bumped
    pool <- bumpedPool bumped
    storeOn <- liftIO $ webStores ctx cfg
    let net = pool.poolNetwork
        store = storeOn net
    unless net.replaceByFee $
      throwError "Bitcoin Cash replaces no transaction: its nodes keep the first one they see"
    stuck <- stuckTransaction ctx store pool bumped
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
    children <- spendingChildren ctx store pool [stuck]
    refuseSpentElsewhere pool (stuck : children)
    plan <- bumpPlan ctx cfg pool (stuck : children)
    -- A wallet behind the chain is asked about with the other questions,
    -- once nothing can refuse the replacement: it is built for a lock time
    -- of 0 meanwhile
    behind <- behindTheChain net
    gen <- liftIO initStdGen
    let (lockTime, buildGen) = lockTimeFor (either (const Nothing) id behind) gen
    -- The network takes no replacement that adds an unconfirmed coin
    confirmed <- spendableCoins ctx pool 1
    candidates <- forM confirmed $ \coin -> (\(addr, path) -> (coin, addr, path)) <$> coinWithPath pool coin
    offered <- liftEither $ fundings ctx buildGen plan candidates feeByte opts.recipientsPay
    built <- forM offered $ \funding -> do
      tsd <- buildBump ctx cfg pool buildGen plan funding feeByte lockTime
      info <- txInfoOf ctx pool tsd
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
    tsd <- agreedFunding pool unit feeByte opts.yes opts.recipientsPay built
    preparedAnswer ctx pool Nothing opts.output tsd []
  where
    -- The pool the transaction is replaced in: the one that holds it on a
    -- network hw shows, or among several the one that pays it. Among
    -- several that pay it, or several that hold it when none pays, the
    -- one of the active portfolio, when it is one of them.
    bumpedPool bumped = do
      activeM <- lift activePortfolioId
      holders <- shownAmong cfg ("The transaction " <> hashRefText opts.bumped) (.poolNetwork) =<< transactionPools bumped
      paying <- filterM (fmap (maybe False (not . Map.null . summaryMyInputs . txInfoSummary)) . (`storedTxInfo` bumped)) holders
      let candidates = if null paying then holders else paying
      case candidates of
        [] -> throwError ("The wallet has no transaction " <> cs (txHashToHex bumped))
        [only] -> return only
        _
          | [ofActive] <- filter ((== activeM) . Just . (.poolPortfolio)) candidates -> return ofActive
          | otherwise -> do
              places <- mapM poolPlace candidates
              throwError ("The transaction is in " <> intercalateAnd (placeText <$> places) <> ": switch to the one to use first")

-- | The replacement of the funding the user agrees to, among those built,
-- each told on stderr with what it is about, to keep the --json answer
-- clean. The change needs no agreement. The coins to add are asked about
-- first, each as coins shows it, then the recipients: whether the lone
-- one pays, or which of several does, or all of them. None agreed, the
-- transaction stays as it is.
--
-- --yes agrees to the first that pays, and among several recipients to
-- all of them paying in equal shares. When the shares would leave one
-- below the dust limit, it is still asked which one pays alone: --yes
-- picks no recipient.
--
-- With --recipients-pay, the one replacement built has all the recipients
-- pay what the higher fee adds, in equal shares ('fundings'). It is told
-- as the others are, and taken without a question.
agreedFunding :: Pool -> AmountUnit -> Natural -> Bool -> FeePayer -> [(Funding, TxSignData)] -> ExceptT String (DB IO) TxSignData
agreedFunding pool unit feeByte yes payer built =
  case built of
    (ChangePays, tsd) : _ -> return tsd
    _ -> do
      fromCoins <- offerCoins [(added, tsd) | (CoinsAdded added, tsd) <- built]
      fromRecipients <- maybe (offerReductions [(way, tsd) | (RecipientsReduced way, tsd) <- built]) (return . Just) fromCoins
      liftMaybe "The transaction was not replaced" fromRecipients
  where
    agreedTo asked = if yes then return True else askYes terminal asked
    net = pool.poolNetwork
    offerCoins [] = return Nothing
    offerCoins ((added, tsd) : _) = do
      views <- poolCoinViews pool [coin.outpoint | (coin, _, _) <- added]
      -- The transaction is replaced in its own portfolio, which may be
      -- another one than the active one
      seen <- seenFromPlace =<< poolPlace pool
      naming <- lift walletNaming
      liftIO $ renderOn stderr =<< addedCoinsDoc (pathsSeenFrom naming seen) net unit feeByte views
      agreed <- agreedTo (addedQuestion added)
      return $ if agreed then Just tsd else Nothing
    offerReductions [] = return Nothing
    -- The one way --recipients-pay leaves: nothing to choose
    offerReductions ((way, tsd) : _)
      | payer == RecipientsPay = takeShares way tsd
    offerReductions ways@((way, tsd) : _) = case way.reductionRecipients of
      [_] -> do
        liftIO $ renderOn stderr (reducedDoc net unit feeByte (fst <$> ways))
        agreed <- agreedTo (reducedQuestion (fst <$> ways))
        return $ if agreed then Just tsd else Nothing
      _
        | yes, (shared, sharedTsd) : _ <- [(w, t) | (w, t) <- ways, w.reductionPaidBy == AllRecipients] ->
            takeShares shared sharedTsd
        | otherwise -> do
            liftIO $ renderOn stderr (reducedDoc net unit feeByte (fst <$> ways))
            -- No is the default: an empty answer keeps the transaction
            askChoice terminal (reducedQuestion (fst <$> ways)) ((["no", "n"], Nothing) : [(reductionAnswers net w, Just t) | (w, t) <- ways])
    -- All the recipients paying in equal shares, told and taken without a
    -- question, as --yes or --recipients-pay says
    takeShares way tsd = do
      liftIO $ renderOn stderr (sharedDoc payer net unit feeByte way)
      return (Just tsd)

{- Sweeps -}

-- | Sweep coins to the destinations: the coins at the given addresses,
-- typed or in a file, at the addresses of the given private keys, and the
-- coins given by outpoint. The keys are not needed here, only to sign: a
-- sweep can be prepared from addresses or coins alone. A sweep that pays
-- an address of the wallet is of the portfolio of that address, on its
-- network, and waits among its pending transactions to be signed by its
-- nosigHash. hw finds the portfolio from the destinations, and takes the
-- active one when they are of several ('poolAmong'). Destinations of the
-- wallet on two networks are refused, since a sweep is on one network. One
-- that pays no address of the wallet involves no portfolio: hw is then a
-- utility, which writes the sweep to a file, on the --network given, to be
-- signed from there. That --network must be given: the default network of the
-- configuration does not apply. The keys, the coins and the legacy
-- addresses of Bitcoin Cash read the same on Bitcoin, so a wrong guess
-- would sweep the coins of the other chain. A destination is an address,
-- or an address and its amount, as for preparesendall.
cmdPrepareSweep :: Ctx -> Config -> AmountUnit -> PrepareSweep -> IO Response
cmdPrepareSweep ctx cfg unit opts =
  runDBResponse cfg $ do
    -- Before the keys are typed, not after
    for_ opts.output checkPathFree
    -- The portfolio of a destination is found by its address, an amount
    -- after it or not, before the network to read the amounts on is known
    everyHolder <- accountsHolding (\net -> mapMaybe (recipientAddress net) opts.sweepTo)
    -- The destinations the wallet has on hidden networks alone are
    -- refused: read as outside the wallet, the sweep would pay an account
    -- of it that no answer shows
    holders <- shownAmong cfg "The sweep pays an address of the wallet that" (\(_, _, acc) -> accountNetwork acc) everyHolder
    case nub [accountPool acc | (_, _, acc) <- holders] of
      [] -> do
        net <-
          liftMaybe
            "Give the --network of a sweep that pays no portfolio of the wallet: the default-network of the configuration file does not apply to it"
            opts.network
        when (isNothing opts.output) $
          throwError "The sweep does not involve this wallet: give an --output file to write it to"
        tsd <- sweepOn net Nothing
        info <- liftEither $ parseOutsideTxSignData net ctx tsd
        writeOutput ctx Nothing net opts.output info tsd
        return $ ResponseOutsideTx net info
      paid -> do
        -- The addresses of the wallet it pays tell its portfolio and its
        -- network
        networks <- sweepNetworks
        pool <- poolAmong cfg opts.network "The destinations of the sweep" [p | p <- paid, p.poolNetwork `elem` networks]
        keepPrepared ctx pool opts.output =<< sweepOn pool.poolNetwork (Just pool)
  where
    -- The networks a sweep that pays the wallet can be on: those that hold
    -- every destination the wallet holds. A sweep is on one network, and
    -- the destinations of the wallet on two networks are refused. The
    -- default network must not choose between them: a Bitcoin Cash
    -- address in its legacy form reads on Bitcoin too, and the sweep would
    -- pay it there, where no account of the wallet watches it. Test
    -- networks that share their keys hold the same addresses, and
    -- 'poolAmong' takes the one of --network or of the default network.
    -- Only the networks hw shows count.
    sweepNetworks = do
      heldOn <- forM opts.sweepTo $ \typed -> do
        holding <- accountsHolding (\net -> maybeToList (recipientAddress net typed))
        return $ onShownNetworks cfg id (nub [accountNetwork acc | (_, _, acc) <- holding])
      let held = filter (not . null) heldOn
          common = case held of
            [] -> []
            some : others -> foldr intersect some others
      when (null common) $
        throwError ("The sweep pays addresses of the wallet on " <> networksText (nub (concat held)) <> ": a sweep is on one network")
      return common
    -- The sweep on the network, which the pool it pays, if any, knows
    sweepOn net poolM = do
      -- The addresses, the file and the destinations are checked before
      -- the keys are typed, not after
      typed <- liftEither $ mapM (textToAddrE net) opts.sweepFrom
      listed <- maybe (return []) (addressesOfFile net) opts.addressFile
      sweepTo <- liftEither $ mapM (readRecipient net unit) opts.sweepTo
      liftEither $ checkSharing sweepTo
      storeOn <- liftIO $ webStores ctx cfg
      let store = storeOn net
      feeByte <- chooseFeeRate cfg net store 1 opts.feeRate
      confirmFeeRate opts.yes feeByte
      keys <- maybe (return []) (readSecKeys terminal net) opts.keys
      found <- fetchSweepCoins net ctx store poolM keys (typed <> listed) opts.coins (fst <$> sweepTo)
      -- The lock time is drawn from the best block of the store, which the
      -- coins come from: a sweep is prepared online, whatever the wallet
      -- knows of the chain. A store behind the chain gives none.
      best <- fromStore $ storeBestBlock store
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
    (poolM, file@(TxFileRead txFile@(TxFile netM tsd _ _) sourceM), signedFile) <- transactionToSign ctx cfg givenM signing
    case poolM of
      Just pool -> do
        let net = pool.poolNetwork
        -- Shown before the keys are asked for, as signtx shows it
        showToSign ctx unit signing net poolM txFile
        keys <- keysOn net
        accounts <- lift $ poolAccounts pool
        keysOfPool <- lift $ poolKeys ctx pool
        (signed, info) <- liftEither $ signTxWithKeys net ctx tsd keysOfPool (heldPoolOf (snd <$> accounts)) keys
        txAnswer pool =<< keepSigned ctx signing signedFile pool file signed info
      Nothing -> do
        named <- fileNetwork givenM netM
        let net = commandNetwork cfg named
        showToSign ctx unit signing net Nothing txFile
        keys <- keysOn net
        (signed, info) <- liftEither $ signTxWithKeys net ctx tsd [] noneHeld keys
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

-- | Sync every portfolio, on every network it has an account on, or on the
-- --network alone. A sync covers every portfolio, the active one or not:
-- a user of several wallets wants them all synced, and syncing one too
-- many does no harm. It covers the networks the configuration hides too,
-- which the status line names as they are synced: a hidden network is up
-- to date the day the user shows it. The answer keeps to the networks hw
-- shows ('shownSyncs'). It gives no sync of a hidden network, and names
-- none as not served or not synced, so a store that fails on a hidden
-- network alone does not make the command exit with an error. A network
-- the host does not serve is named and left out. So is one whose store is
-- down, or fails during the sync of one of its pools, which also makes
-- the command exit with an error once the others are synced: the sync of
-- that pool is undone, and the syncs of the other networks are kept
-- ('syncPools'). The answer names the active portfolio too: the steps
-- after a sync work in it, and a pending transaction in conflict in
-- another portfolio needs a switch first.
cmdSync :: Ctx -> Config -> Maybe Network -> Bool -> IO Response
cmdSync ctx cfg netM full =
  withStatusLine $ \line ->
    runDBResponse cfg $ do
      portfolios <- lift getAllPortfolios
      when (null portfolios) $ throwError noWallets
      pools <- concat <$> mapM (lift . portfolioPools . (.portfolioId)) portfolios
      let chosen = [pool | pool <- pools, maybe True (== pool.poolNetwork) netM]
      storeOn <- liftIO $ webStores ctx cfg
      synced <- syncPools ctx cfg storeOn chosen full (terminalWatcher line)
      -- The steps after the sync work in the active portfolio
      activeM <- activePortfolioRef
      -- The answer keeps to the networks hw shows
      return $
        ResponseSync
          (shownSyncs cfg (syncedPools synced))
          (onShownNetworks cfg id (syncedNotServed synced))
          [(net, cs failure) | (net, failure) <- onShownNetworks cfg fst (syncedDown synced)]
          activeM

-- | The syncs an answer gives, among those sync or recover did: those of
-- the networks hw shows. The syncs of the hidden networks were done all
-- the same, and are kept in the database.
shownSyncs :: Config -> [SyncRes] -> [SyncRes]
shownSyncs cfg = onShownNetworks cfg (placeNetwork . syncResPlace)

-- | Sign a message with the key of an address of the wallet. The answer is
-- the signed message, or the --output file its armored block went to.
--
-- An address tells its portfolio, whichever is active. The wallet must
-- hold it. One it has on several networks is the one on the --network,
-- or on the default network ('addressByText').
--
-- A path, such as bip84/0/3, names the address the public key of the
-- account derives there, in the active portfolio, on the --network or the
-- default network, as show derives it ('derivedAddressAt'). It needs no
-- row. The offline computer never gives out an address, since receive
-- runs online, and holds none: a path is how it signs. The signed message
-- names the address as typed, or, for a path, in the form the wallet
-- stores, so that the reader sees which address signed.
cmdSignMsg :: Ctx -> Config -> SignMsg -> IO Response
cmdSignMsg ctx cfg opts =
  runDBResponse cfg $ do
    -- Before the mnemonic is asked, not after
    for_ opts.output checkPathFree
    (portfolio, acc, path, signer) <- case opts.address of
      NamedByAddress typed -> do
        (portfolio, accId, row) <- addressByTextOr (notHeldToSign typed) cfg opts.network typed
        acc <- getAccountById accId
        return (portfolio, acc, addrPath row, typed)
      NamedByPath kind addrType index -> do
        portfolio <- requireActivePortfolio
        (_, acc) <- chooseAccount cfg portfolio opts.network (Just kind)
        derived <- derivedAddressAt ctx acc addrType index
        stored <- liftEither $ addrToTextE (accountNetwork acc) derived
        return (portfolio, acc, addrDeriv addrType :/ index, stored)
    let net = accountNetwork acc
    addr <- liftEither $ textToAddrE net signer
    -- Warned before the mnemonic is asked, so that the user can stop. On
    -- stderr, to keep the armored block and the --json output clean.
    liftIO $ mapM_ (hPutStrLn stderr) $ signingWarnings opts.message
    (_, accountPrvKey) <- askSigningKey terminal (accountKind acc) net ctx (portfolioIndex portfolio.portfolioRow) (accountXPubKey ctx acc)
    let addrKey = derivePath ctx path accountPrvKey
    signature <- encodeSignature <$> liftEither (signMessage ctx addr addrKey.key opts.message)
    case opts.output of
      Nothing -> return $ ResponseSignMsg opts.message signature signer
      Just file -> do
        liftEitherIO $ writeTextFile file (signedMessageText signer signature opts.message)
        return $ ResponseFile file

-- | The refusal of signmsg for an address the wallet does not hold. A
-- computer holds the addresses it gave out, and the offline one gives out
-- none, so the refusal points to the path, which needs no row.
notHeldToSign :: Text -> String
notHeldToSign typed =
  "The wallet has no address "
    <> cs typed
    <> ". A computer holds only the addresses it gave out. On another one, such as the offline computer, give the path of the address instead, as addresses shows it, such as bip84/0/3"

-- | Check a signed message: whether the key of the address signed it. The
-- wallet is not needed: the address can be anyone's.
cmdVerifyMsg :: Ctx -> Config -> VerifyMsg -> IO Response
cmdVerifyMsg ctx cfg opts =
  runResponse $ do
    (address, signature, message) <- case opts.source of
      Given address signature message -> return (address, signature, message)
      FromFile file -> signedMessageFromFile file
    let net = commandNetwork cfg opts.network
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
-- refuses one of another version first ("Haskoin.Wallet.Migration").
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
