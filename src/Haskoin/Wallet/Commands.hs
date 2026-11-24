{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The commands of hw. Each reads what it needs from the database, the
-- store and the user, and answers with a 'Response'. A command runs in one
-- transaction of the database: one that fails leaves the database as it
-- was, and answers with its error. send and sendall are the one exception:
-- each keeps the transaction it signed before it sends it (@keepSoFar@).
-- An answer that shows a portfolio reads it once the command is done, so
-- that it shows what the command changed.
--
-- A command works in the active portfolio, which the database keeps and
-- switch changes ('requireActivePortfolio'). Inside it, --network names
-- what the portfolio holds on one network, its pool, and --type one of the
-- two accounts of the pool. A listing covers the whole portfolio unless
-- the options narrow it, in a section for each network, or for each
-- account for the addresses ("Haskoin.Wallet.Cli.Listing"). --reverse
-- turns the rows of each section around, and nothing else changes the
-- order of a listing ('ListingOrder'). --tag keeps the rows that have a
-- tag, and leaves the others out ('keptTagsOfListing'). A filter never
-- changes what a row shows.
--
-- A tag is a word the user puts on addresses and transactions
-- ("Haskoin.Wallet.Tags"). tag, untag, tags, renametag and deletetag
-- manage them ('cmdTag'). They are of the whole database, so the five
-- commands work whichever portfolio is active. The listings keep the rows of a tag, a
-- payment spends the coins at the addresses of one (@controlledPool@),
-- and its new change address takes the tags its coins share when asked
-- ("Haskoin.Wallet.Database.PendingTx".'changeTakesTags'). tag and untag
-- go down the chain of change of an address when asked
-- ("Haskoin.Wallet.Database.ChangeChain"). A payment that spends coins
-- of different tags together asks first (@confirmMixing@).
--
-- The address book keeps the addresses of others, the contacts
-- ("Haskoin.Wallet.Contacts"). newcontact, contacts and deletecontact
-- manage it ('cmdNewContact'). It is of the whole database too. label,
-- tag, untag and show take a contact by its address, where they take an
-- address of the wallet (@namedAddress@): the user need not remember
-- which of the two an address is. A transaction shows its addresses that
-- are contacts as such, with their labels
-- ("Haskoin.Wallet.Database.TxInfo".'fillContacts').
--
-- Some commands find their portfolio by themselves, from what they are
-- given: a transaction, a coin, an address, the destinations of a sweep.
-- When several portfolios are candidates, the active one is taken when it
-- is one of them; otherwise the refusal names them, for the user to
-- switch to the one to use. Their answers carry the active portfolio, so
-- that the screen says when the portfolio shown is another one, whose
-- paths and labels the user might take for those of the active one
-- ("Haskoin.Wallet.Cli.Response"). portfolios, pendingtxs, sync,
-- exportsheet, exportjournal and report cover every portfolio.
--
-- A command that adds a wallet or a portfolio is followed by a copy of
-- the database (@changesPortfolios@, @backUpDatabase@). A command that
-- changes the wallets, their descriptions, the labels, the tags, the
-- address book or the locked coins is followed by a copy of the labels,
-- the file exportwallet writes (@changesLabels@, @backUpLabels@). Both
-- go into the auto-backups directory next to the database, which keeps
-- the recent copies of each and a thinning trail of older ones, unless
-- the configuration turns them off ("Haskoin.Wallet.Backup").
--
-- A command that loses what the database holds copies it first, with
-- its labels, into the auto-backups directory too. deletewallet and
-- deleteportfolio do so before they delete, and restoredatabase before
-- it replaces the database (@backUpBeforeLosing@). That copy holds the
-- database as it was. Such a command has no copy after it
-- (@backsUpBefore@): one command makes one backup, of what is worth the
-- most. Every copy hw makes by itself is in that one directory. The
-- user-backups directory holds the copies of the backup command alone.
--
-- A command that deletes from the database for good asks first, and says
-- what is about to go (@confirmDeletion@). The five commands whose name
-- starts with delete do: deleteportfolio, deletewallet, deletetx,
-- deletetag and deletecontact. --yes answers for a script. A command
-- that only takes something off, such as untag or unlockcoin, asks
-- nothing: the user puts it back with one command.
--
-- A command that changes a default writes it to the configuration file
-- inside its transaction, as its last step (@writeDefaults@): when the
-- file cannot be written, the database is left as it was, so that the two
-- never disagree. Only setdefault does. It writes the network, the type,
-- the unit, the view of the listings and the networks hw shows. The
-- active portfolio is no key of the file: it is in the database.
-- showdefault reads both back, and changes neither ('cmdShowDefault').
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
-- (@shownAmong@): answering that the wallet does not have it would be
-- untrue. A --network that names a hidden network is refused before the
-- command runs ('refusedNetwork').
module Haskoin.Wallet.Commands
  ( commandResponse,
    journalBeforeCommand,
    commandListing,
    refusedNetwork,
    runsOnDamagedDatabase,
    answerWalletNaming,
    listingWalletNaming,
    listingTagWidths,

    -- * Listings
    cmdCsv,
    cmdExportSheet,
    cmdExportJournal,
    cmdReport,
    portfoliosListing,
    addressesListing,
    balancesListing,
    coinsListing,
    txsListing,
    pendingTxsListing,
    keptTagsOfListing,

    -- * Wallets and portfolios
    cmdNewPortfolio,
    cmdRecover,
    cmdCheckMnemonic,
    cmdImportWallet,
    cmdExportWallet,
    cmdSwitch,
    cmdSetDefault,
    cmdShowDefault,
    cmdDeletePortfolio,
    cmdDeleteWallet,
    cmdShowBackups,
    cmdRestoreDatabase,

    -- * Addresses
    cmdReceive,
    cmdShow,
    cmdLabel,

    -- * Tags
    cmdTag,
    cmdUntag,
    cmdTags,
    cmdRenameTag,
    cmdDeleteTag,

    -- * The address book
    cmdNewContact,
    cmdDeleteContact,
    contactsListing,

    -- * Coins
    cmdLockCoin,
    cmdUnlockCoin,

    -- * Transactions
    cmdImportTx,
    cmdExportTx,
    cmdDeleteTx,

    -- * Payments: preparing, signing and sending
    cmdSend,
    cmdSendAll,
    cmdPrepareTx,
    cmdPrepareSendAll,
    cmdSignTx,
    cmdBroadcast,
    cmdBumpFee,

    -- * Sweeps
    cmdSweep,
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
import Control.Exception (displayException)
import Control.Monad (filterM, forM, forM_, guard, join, unless, void, when, (<=<))
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
import Data.Containers.ListUtils (nubOrd)
import Data.List (genericTake, intercalate, intersect, nub, partition, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (Day, LocalTime (..), TimeZone, UTCTime, ZonedTime (..), addDays, fromGregorian, getCurrentTime, getTimeZone, getZonedTime, localTimeToUTC, midnight, toGregorian, utc)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Database.Persist.Sqlite (transactionSave, transactionUndo)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.AddressSelection (AddressSelection (..))
import Haskoin.Wallet.Amounts (AmountUnit, showAmount, showUnit, unitName)
import Haskoin.Wallet.Backup (CopyStamp (..), autoBackupRetention, backupsShown, copyDatabase, rollBackup, rollDatabaseCopy, rollLabelCopy, userBackup)
import Haskoin.Wallet.Bip329 (recordsText)
import Haskoin.Wallet.Cli.Csv (sheetCsv)
import Haskoin.Wallet.Cli.Journal (journalHeader, localJournal)
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Ods (workbookOds)
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.PrettyPrinter (SeenFrom (..), addedCoinsDoc, agoText, childrenDoc, dustReportDoc, feeRatesDoc, mixingDoc, orList, pathsSeenFrom, preparedDoc, reducedDoc, renderOn, sharedDoc, sweepDoc, sweepDustReportDoc, txAnswerDoc)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Sheet (PortfolioSheets (..), Sheet, TxSheets, localListingSheet, localTxSheets, stacked, walletSheets)
import Haskoin.Wallet.Cli.View (viewName)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Contacts (Contact (..))
import qualified Toml
import Haskoin.Wallet.CutAddress (cutAddress)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock (BestBlock (..), getBest, poolEverSynced)
import Haskoin.Wallet.Database.ChangeChain (ChainRetagged (..), ChainStop (..), retagChangeChain)
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.Contact
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.RawTx (dropOrphanRawTxs)
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Cli.Wrap (warn)
import Haskoin.Wallet.DatabaseFailure (catchDatabaseFailure, databaseDamage, databaseFailureExplained, setDamagedAside)
import Haskoin.Wallet.DatabaseFile (DatabaseShown (..), WalletShown (..), databaseHeaderLength, databaseShown, databaseToRestore, heldNow, startsDatabase)
import Haskoin.Wallet.Labels (AccountImport (..), FileShown (..), ImportScope (..), databaseRecords, fileShown, holdsAnything, importCreated, importFile, readWalletFile, walletRecords, wholeFile)
import Haskoin.Wallet.MessageSigning
import Haskoin.Wallet.Migration (createDatabase, latestDatabaseVersion)
import Haskoin.Wallet.Report.AccountStatement (AccountStatement (..), PeriodStart (..), accountBlocksUnnamed, accountStatement, accountStatementReport, accountsSelected, accountsValued, namingAccountBlocks)
import Haskoin.Wallet.Report.BalanceStatement (Statement (..), balanceStatement, holdingsSelected, lastBlocksUnnamed, namingLastBlocks, statementReport, valued)
import Haskoin.Wallet.Report.Layout (reportPdf)
import Haskoin.Wallet.Report.PaymentConfirmation (PaymentConfirmation (paymentCounterparty, paymentHolder, paymentPurpose), namingPaymentBlock, paymentBlocksUnnamed, paymentConfirmation, paymentReport, paymentValued)
import Haskoin.Wallet.Report.Statement (PortfolioHistory (..), StatementDate (..), StatementScope (..), SyncedTo (..), Valuation (..), longDate, statementTimes)
import Haskoin.Wallet.Shares (genShares)
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.SyncStatus (StatusLine (..), checkingHealth, withStatusLine)
import Haskoin.Wallet.Tags (ShownTag (..), Tag, readTag, tagText)
import Haskoin.Wallet.TxSelection (TxSelection (..))
import Haskoin.Wallet.Tx.Building (ChangeTo (..), CoinControl (..), PreparedPayment (..), buildTxSignData, coinWithPath)
import Haskoin.Wallet.Tx.BumpFee
import Haskoin.Wallet.Tx.CoinSelection (FeePayer (..), coinKind, coinsDoNotCover)
import Haskoin.Wallet.Tx.FeeRates
import Haskoin.Wallet.Tx.LockTime (isStale, lockTimeFor)
import Haskoin.Wallet.Tx.Mixing (SpentGroup (..), addsGroup, joinsOutsideTags, spentGroups)
import Haskoin.Wallet.Tx.PsbtFile (FileAccount (..), psbtTxFile, signedPsbt, signerChecks, txFilePsbt)
import Haskoin.Wallet.Tx.Recipient (Share, checkSharing, readPayment, checkAddressReads, readRecipient, recipientAddress)
import Haskoin.Wallet.Tx.Dust (DustReport (dustCoins), SizedCoin (coin), dustQuestion, isDustCoin, spendingAmounts)
import Haskoin.Wallet.Tx.SendAll (buildSendAllSignData, noCoinsToSend, sendAllDustReport, walletSized)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (SpentCoin, outsideInputs, outsideUnsigned, heldPoolOf, labelledTxFile, labelsFromFile, noneHeld, parseOutsideTxSignData, parseTxFile, parseTxSignData, signHeldWalletTx)
import Haskoin.Wallet.Tx.Size (checkFeeRate, highFeeRate, paidRateText)
import Haskoin.Wallet.Tx.Sweep (SweepCoins (coins, withTokens), UnsweptCoin, UnsweptReason (..), addressesWithoutCoin, buildSweepSignData, fetchSweepCoins, signTxWithKeys, sizedSweepCoins, sweepDustReport, sweepableAddress, sweepingOnly, sweptAddresses, unsweptCoins)
-- The addresses of a private key, where message signing has those of a
-- public key under the same name
import qualified Haskoin.Wallet.Tx.Sweep as Sweep (keyAddresses)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Directory (canonicalizePath, doesPathExist, removeFile)
import System.IO (stderr)
import System.Random (initStdGen)
import UnliftIO (withRunInIO)
import UnliftIO.Exception (tryAny)

{- Running a command -}

-- | The answer of hw to a command. After a command that added a wallet
-- or a portfolio, hw keeps a copy of the database (@backUpDatabase@).
-- After a command that may change the labels, it keeps a copy of them
-- (@backUpLabels@). The configuration can turn either off. A command
-- that failed is not followed by a copy: it undid what it wrote, but for
-- the signed transaction of a send or a sendall. Nor is a command that
-- loses what the database holds, whose copy was made before it
-- (@backsUpBefore@).
--
-- After a command that may change the journal hw keeps for ledger, hw
-- writes the journal again, when the configuration asks for one
-- (@journalAfter@). A command that failed changed nothing the journal
-- holds.
commandResponse :: Config -> AmountUnit -> Command -> IO Response
commandResponse cfg unit cmd = do
  response <- commandAnswer cfg unit cmd
  unless (backsUpBefore response) $ do
    when (configAutoBackupDatabase cfg && changesPortfolios response) $
      backUpDatabase cfg
    when (configAutoBackupLabels cfg && changesLabels cmd && not (isError response)) $
      backUpLabels cfg
  unless (isError response) $
    journalAfter cfg cmd
  return response
  where
    isError = \case
      ResponseError _ -> True
      _ -> False

-- | The answer of hw to a command, without the copy of the labels after it
commandAnswer :: Config -> AmountUnit -> Command -> IO Response
commandAnswer cfg unit = \case
  -- A listing to write to a CSV file, whatever it lists
  cmd
    | Just file <- csvFileOf cmd,
      Just listing <- commandListing cfg UsualOrder cmd ->
        cmdCsv cfg file listing
  -- A listing asked whole, as --json asks it
  cmd | Just listing <- commandListing cfg UsualOrder cmd -> readListing cfg listing
  -- Wallets and portfolios
  CommandNewPortfolio opts -> cmdNewPortfolio cfg opts
  CommandRecover opts -> cmdRecover cfg opts
  CommandCheckMnemonic -> cmdCheckMnemonic cfg
  CommandImportWallet scope file -> cmdImportWallet cfg scope file
  CommandExportWallet walletM portfolioM file -> cmdExportWallet cfg walletM portfolioM file
  CommandSwitch walletM portfolioM -> cmdSwitch cfg walletM portfolioM
  CommandSetDefault opts -> cmdSetDefault cfg opts
  CommandShowDefault opts -> cmdShowDefault cfg opts
  CommandDeletePortfolio index yes -> cmdDeletePortfolio cfg index yes
  CommandDeleteWallet index yes -> cmdDeleteWallet cfg index yes
  -- Addresses
  CommandReceive opts -> cmdReceive cfg opts
  CommandShow target fileScope shown -> cmdShow cfg target fileScope shown
  CommandLabel opts -> cmdLabel cfg opts
  -- Tags
  CommandTag opts -> cmdTag cfg opts
  CommandUntag opts -> cmdUntag cfg opts
  CommandTags -> cmdTags cfg
  CommandRenameTag opts -> cmdRenameTag cfg opts
  CommandDeleteTag tag yes -> cmdDeleteTag cfg tag yes
  -- The address book
  CommandNewContact opts -> cmdNewContact cfg opts
  CommandDeleteContact address netM yes -> cmdDeleteContact cfg address netM yes
  -- Coins
  CommandLockCoin named netM -> cmdLockCoin cfg named netM
  CommandUnlockCoin named netM -> cmdUnlockCoin cfg named netM
  -- Transactions
  CommandImportTx file -> cmdImportTx cfg file
  CommandExportTx named file -> cmdExportTx cfg named file
  CommandDeleteTx named yes -> cmdDeleteTx cfg named yes
  CommandPrepareTx opts -> cmdPrepareTx cfg unit opts
  CommandSend opts -> cmdSend cfg unit opts
  CommandSendAll opts -> cmdSendAll cfg unit opts
  CommandPrepareSendAll opts -> cmdPrepareSendAll cfg unit opts
  CommandSignTx signing -> cmdSignTx cfg unit signing
  CommandBroadcast namedM fileM yes netM -> cmdBroadcast cfg unit namedM fileM yes netM
  CommandBumpFee opts -> cmdBumpFee cfg unit opts
  -- Sweeps
  CommandSweep opts -> cmdSweep cfg unit opts
  CommandPrepareSweep opts -> cmdPrepareSweep cfg unit opts
  CommandSignSweep signing keysM netM -> cmdSignSweep cfg unit signing keysM netM
  -- Sync, messages and backups
  CommandSync netM mode -> cmdSync cfg netM mode
  CommandFees netM -> cmdFees cfg netM
  CommandSignMsg opts -> cmdSignMsg cfg opts
  CommandVerifyMsg opts -> cmdVerifyMsg cfg opts
  CommandBackup fileM -> cmdBackup cfg fileM
  CommandShowBackups -> cmdShowBackups cfg
  CommandRestoreDatabase file yes -> cmdRestoreDatabase cfg file yes
  CommandExportSheet file kept -> cmdExportSheet cfg file kept
  CommandExportJournal file kept -> cmdExportJournal cfg file kept
  CommandReport opts -> cmdReport cfg opts
  -- The listings, which the cases above answer
  CommandPortfolios {} -> return notAListing
  CommandAddresses {} -> return notAListing
  CommandBalances {} -> return notAListing
  CommandCoins {} -> return notAListing
  CommandTxs {} -> return notAListing
  CommandPendingTxs {} -> return notAListing
  CommandContacts {} -> return notAListing
  where
    notAListing = ResponseError "The command has no listing to answer with"

-- | Whether a command runs when the database is damaged, a file SQLite
-- cannot read ("Haskoin.Wallet.DatabaseFailure"). restoredatabase puts a
-- backup in its place, and showbackups reads the directories of the
-- backups alone: the two the user needs to get the wallet back. Every
-- other command is refused, with the way to restore.
runsOnDamagedDatabase :: Command -> Bool
runsOnDamagedDatabase cmd = case cmd of
  CommandRestoreDatabase {} -> True
  CommandShowBackups -> True
  _ -> False

-- | The listing of a command that lists, which the screen reads chunk by
-- chunk, or Nothing for a command that answers at once. portfolios and
-- pendingtxs list every portfolio, contacts lists the address book, which
-- is of no portfolio, and the others list the active one.
-- Each lists the networks the configuration shows.
--
-- The order given is that of a listing whose options ask for none
-- ('orderUnasked'). The screen gives the order of the key
-- listing-reverse. --json and --csv give the usual order, so that the
-- key changes the screen alone (@commandAnswer@).
commandListing :: Config -> ListingOrder -> Command -> Maybe (ExceptT String (DB IO) Sections)
commandListing cfg unasked = \case
  CommandPortfolios _ -> Just (portfoliosListing cfg)
  CommandAddresses target internal tags _ paging -> Just $ inActive $ \portfolio -> addressesListing cfg portfolio target internal tags (ordered paging)
  CommandBalances target tags _ paging -> Just $ inActive $ \portfolio -> balancesListing cfg portfolio target tags (ordered paging)
  CommandCoins target locked tags _ paging -> Just $ inActive $ \portfolio -> coinsListing cfg portfolio target locked tags (ordered paging)
  CommandTxs netM kept _ paging -> Just $ inActive $ \portfolio -> txsListing cfg portfolio netM kept (ordered paging)
  CommandPendingTxs netM kept _ paging -> Just $ pendingTxsListing cfg netM kept (ordered paging)
  CommandContacts netM tags _ paging -> Just $ contactsListing cfg netM tags (ordered paging)
  _ -> Nothing
  where
    inActive listing = listing =<< requireActivePortfolio
    ordered :: Paging after -> Paging after
    ordered = orderUnasked unasked

-- | The tags a listing command keeps the rows of, as the first line of
-- the listing names them on the screen, each with its colour. Nothing
-- for a listing asked for no tag, and for a command that lists nothing.
-- A word that is no tag of the database is refused here, before the
-- listing shows a line: the user most likely mistyped it
-- ('requireTags').
--
-- addresses and balances keep the addresses that have a tag. coins keeps
-- the coins at such an address: a coin has no tag of its own. txs and
-- pendingtxs keep the transactions that have a tag, with --tag, and
-- those at an address or at a contact that has one, with --address-tag.
-- contacts keeps the contacts that have a tag.
--
-- With --untagged, each keeps what has no tag, and the line says so in
-- the words of its listing: an address or a contact has none, a coin is
-- at an address that has none, and a transaction has none and is at no
-- contact that has one (the type t'Haskoin.Wallet.Database.Tag.TxTagFilter').
keptTagsOfListing :: Config -> Command -> IO (Either String (Maybe KeptTags))
keptTagsOfListing cfg cmd =
  fmap join . catchDatabaseFailure cfg . runDB cfg . runExceptT $ case cmd of
    CommandAddresses _ _ kept _ _ -> keptRowTagsNamed "Addresses" kept
    CommandBalances _ kept _ _ -> keptRowTagsNamed "Addresses" kept
    CommandCoins _ _ kept _ _ -> keptTagsNamed "Coins" [] kept.keptTags ("at addresses without a tag" <$ guard kept.keptUntagged)
    CommandTxs _ kept _ _ -> keptTxTagsNamed "Transactions" kept
    CommandPendingTxs _ kept _ _ -> keptTxTagsNamed "Pending transactions" kept
    CommandContacts _ kept _ _ -> keptRowTagsNamed "Contacts" kept
    _ -> return Nothing

-- | The tags something keeps its rows by, each with its colour, given
-- what it lists, the tags of the rows, the tags of their addresses, and
-- the words for the rows without a tag when it keeps them. Nothing when
-- it keeps every row. A word that is no tag of the database is refused
-- ('requireTags').
keptTagsNamed :: String -> [Tag] -> [Tag] -> Maybe String -> ExceptT String (DB IO) (Maybe KeptTags)
keptTagsNamed noun rowTags atAddresses withoutTag
  | null rowTags && null atAddresses && isNothing withoutTag = return Nothing
  | otherwise = Just <$> (KeptTags noun <$> requireTags rowTags <*> requireTags atAddresses <*> pure withoutTag)

-- | The tags a filter of rows keeps them by, under the name given for
-- what is listed ('keptTagsNamed'): the tags of the rows themselves, and
-- the rows without a tag when it keeps them. addresses, balances,
-- contacts and the balance statement of some addresses all say it in
-- these words.
keptRowTagsNamed :: String -> TagFilter -> ExceptT String (DB IO) (Maybe KeptTags)
keptRowTagsNamed noun kept = keptTagsNamed noun kept.keptTags [] ("without a tag" <$ guard kept.keptUntagged)

-- | The tags a filter of transactions keeps them by, under the name
-- given for what is listed ('keptTagsNamed'). The transactions without
-- a tag it keeps are those left to sort, and are named as such
-- (the type t'Haskoin.Wallet.Database.Tag.TxTagFilter'). txs, pendingtxs, and
-- the statement and the journal of some transactions all say it in
-- these words.
keptTxTagsNamed :: String -> TxTagFilter -> ExceptT String (DB IO) (Maybe KeptTags)
keptTxTagsNamed noun kept =
  keptTagsNamed noun kept.keptTxTags kept.keptAddressTags ("without a tag or a tagged contact" <$ guard kept.keptUntaggedTxs)

-- | The transactions a filter of tags keeps in the portfolios given, on
-- the networks hw shows, for a statement or a journal of some
-- transactions ("Haskoin.Wallet.TxSelection"). Nothing for a filter
-- that keeps every transaction. A word that is no tag of the database
-- is refused, as a listing refuses it.
--
-- The database is asked about each portfolio on each network with the
-- condition of txs ('txidsKept'), in the transaction the caller runs.
-- So the selection is what txs lists with the same options in each
-- portfolio, and it agrees with the history the caller reads next.
txSelection :: Config -> [Portfolio] -> TxTagFilter -> ExceptT String (DB IO) (Maybe TxSelection)
txSelection cfg portfolios kept = do
  named <- keptTxTagsNamed "Transactions" kept
  forM named $ \tags -> do
    found <- forM portfolios $ \portfolio -> do
      pools <- listedPools cfg portfolio Nothing
      forM pools $ \pool -> do
        txids <- txidsKept pool kept
        return [(portfolioRef portfolio, pool.poolNetwork.name, txid) | txid <- txids]
    return (TxSelection (keptTagsText tags) (Set.fromList (concat (concat found))))

-- | The addresses a filter of tags keeps in the portfolios given, on the
-- networks hw shows, for a balance statement of some addresses
-- ("Haskoin.Wallet.AddressSelection"). Nothing for a filter that keeps
-- every address. A word that is no tag of the database is refused, as a
-- listing refuses it.
--
-- Only the addresses that have a tag are read, each with its tags
-- ('accountAddressTags'): the selection takes any other address for one
-- without a tag.
addressSelection :: Config -> [Portfolio] -> TagFilter -> ExceptT String (DB IO) (Maybe AddressSelection)
addressSelection cfg portfolios kept = do
  named <- keptRowTagsNamed "Addresses" kept
  forM named $ \tags -> do
    found <- forM portfolios $ \portfolio -> do
      pools <- listedPools cfg portfolio Nothing
      forM pools $ \pool -> do
        accounts <- lift (poolAccounts pool)
        tagged <- lift (concat <$> mapM (accountAddressTags . fst) accounts)
        return [((portfolioRef portfolio, pool.poolNetwork.name, address), itsTags) | ((_, _, address), itsTags) <- tagged]
    return (AddressSelection (keptTagsText tags) kept (Map.fromList (concat (concat found))))

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

{- The copies of the database and of the labels -}

-- | Whether the answer of a command says that it lost what the database
-- held: ResponseDeleted is the answer of deleteportfolio and deletewallet
-- alone, and ResponseRestored that of restoredatabase. hw makes its
-- backup before such a command (@backUpBeforeLosing@), and no copy after
-- it, of the database or of the labels. One command makes one backup, of
-- what is worth the most.
--
-- The copy made before is the one to keep. It is the last that holds
-- what the command lost, and nothing else does. What the database holds
-- after the command is elsewhere already. After a deletion, the copy
-- made before holds all of it. After a restore, the file the user
-- restored from holds all of it.
--
-- So a copy after the command would protect little, and it would do
-- harm. It would be the last copy of its hour, its day, its week and its
-- month, in place of the copy before it, and the rules that keep the
-- last copy of each period would keep the copy without what was lost
-- ("Haskoin.Wallet.Backup".'autoBackupRetention'). Without it, the copy
-- that holds what was lost is the newest one, until the next copy.
--
-- The answer tells, whether or not the copy before was made. With the
-- copies of the database turned off, no copy is made before. Nor is one
-- when restoredatabase replaces a database without a wallet, which has
-- nothing to lose. No copy follows then either: the rule stays one
-- sentence, and the next command that changes the wallets or the labels
-- makes a copy.
backsUpBefore :: Response -> Bool
backsUpBefore = \case
  ResponseDeleted {} -> True
  ResponseRestored {} -> True
  _ -> False

-- | Whether the answer of a command says that it added or deleted a
-- wallet or a portfolio. hw keeps a copy of the database after such a
-- command (@backUpDatabase@), unless it lost what the database held:
-- its copy is then made before it (@backsUpBefore@).
--
-- The answer is what tells, not the command alone. importwallet often
-- only sets labels, and recover may find no portfolio the wallet lacks:
-- the database then holds the same wallets and portfolios as before, and
-- a copy would push out an older one for nothing. Each answer says what
-- the command created. newportfolio always creates a portfolio, and
-- ResponseDeleted is the answer of deleteportfolio and deletewallet
-- alone. restoredatabase replaces every wallet and portfolio with those
-- of a copy. A new command that adds or deletes a portfolio needs its
-- answer here.
changesPortfolios :: Response -> Bool
changesPortfolios = \case
  ResponseNewPortfolio {} -> True
  -- A new wallet always gets a portfolio counted among those created
  ResponseRecover _ _ _ created _ _ _ _ _ _ _ -> not (null created)
  ResponseImport report _ -> not (null (importCreated report))
  ResponseDeleted {} -> True
  ResponseRestored {} -> True
  _ -> False

-- | Copy the database into the auto-backups directory next to it, and
-- keep the copies there that the rotation keeps
-- ('autoBackupRetention', 'rollDatabaseCopy'). It runs once
-- the command is done, in a transaction of its own, so that the copy
-- holds the database as the command left it, and its name the counts of
-- that database. The transaction holds the database while SQLite copies
-- it, so no other command changes it in between.
backUpDatabase :: Config -> IO ()
backUpDatabase cfg =
  automaticCopy "The database was not backed up." cfg $ do
    let database = databaseFile cfg
    stamp <- copyStampNow
    void . liftEitherIO $ rollDatabaseCopy autoBackupRetention database latestDatabaseVersion stamp

-- | Run a copy hw makes by itself after a command, in a transaction of
-- its own, which holds the database. Two hw at once then cannot write
-- and remove copies at the same time.
--
-- A copy that cannot be made is told on stderr, after the text given,
-- which says what was not backed up. The command is not undone, and its
-- answer stands: what the user asked for is done, and the next copy may
-- succeed. So nothing a copy does may stop hw. A failure hw did not
-- foresee is caught too, and told the same way ('tryAny'), rather than
-- end a command that succeeded with the error of its copy.
--
-- The warning ends with what to do. The next copy of that kind comes
-- only with the next command that changes it, which may be far off. So
-- the warning names backup, which makes a copy now. showbackups shows
-- later how old the newest copy is.
--
-- Another hw may take the database between the command and its copy,
-- and hold it longer than the copy waits. The copy is then missed. The
-- warning says so in its own words. The words of a command that found
-- the database held tell the user to try again, and here the command is
-- done.
automaticCopy :: String -> Config -> ExceptT String (DB IO) () -> IO ()
automaticCopy notBackedUp = afterCommand notBackedUp "Another hw command took the database before the copy was made" "To make a copy now, run: hw backup"

-- | Run what hw does by itself after a command, in a transaction of its
-- own, which holds the database: a copy ('automaticCopy'), or the journal
-- it keeps ('keepJournal'). A failure is told on stderr and stops
-- nothing, since the command is done. The warning starts with the first
-- text, which says what was not done. Then comes the reason: the second
-- text when another hw held the database for too long. It ends by saying
-- that the command is done, and with the third text, which says what to
-- run to do it now.
afterCommand :: String -> String -> String -> Config -> ExceptT String (DB IO) () -> IO ()
afterCommand notDone databaseTaken whatToRun =
  besideCommand notDone databaseTaken ("The command itself is done. " <> whatToRun)

-- | Run what hw does by itself beside a command, in a transaction of its
-- own, which holds the database. It is the work of 'afterCommand', and
-- of the journal hw writes before a command when the file is missing or
-- behind the configuration ('journalBeforeCommand').
--
-- A failure is told on stderr and stops nothing: the user asked for the
-- command, not for this. The warning starts with the first text, which
-- says what was not done. Then comes the reason: the second text when
-- another hw held the database for too long. It ends with the third
-- text, which says what this means for the command and what to run.
besideCommand :: String -> String -> String -> Config -> ExceptT String (DB IO) () -> IO ()
besideCommand notDone databaseTaken whatNow cfg action = do
  made <- tryAny . tryDatabase . runDB cfg $ runExceptT action
  case made of
    Left failure -> notDoneFor (displayException failure)
    Right (Left DatabaseInUse) -> notDoneFor databaseTaken
    Right (Left failure) -> notDoneFor =<< databaseFailureExplained cfg failure
    Right (Right outcome) -> either notDoneFor return outcome
  where
    notDoneFor reason = warn (notDone <> " " <> reason <> ". " <> whatNow)

-- | Copy the database and its labels in the transaction of the command,
-- and answer with the two files. The function given first writes them,
-- from the database, its format version, the stamp and the labels: into
-- the user-backups directory for backup ('userBackup'), or into the
-- auto-backups directory before a loss ('rollBackup'). The copy, its
-- name and its labels are read in that one transaction, which holds the
-- database, so they agree with each other.
--
-- SQLite copies the database through a connection of its own, which
-- reads what the database held before this transaction. So the copy
-- holds the database as it was when the command began, even when the
-- command has written since.
backupNow :: (FilePath -> Int -> CopyStamp -> BS.ByteString -> IO (Either String (FilePath, FilePath))) -> Config -> ExceptT String (DB IO) BackupFiles
backupNow write cfg = do
  stamp <- copyStampNow
  records <- databaseRecords =<< lift getAllPortfolios
  (copy, labels) <- liftEitherIO $ write (databaseFile cfg) latestDatabaseVersion stamp (encodeUtf8 (recordsText records))
  return BackupFiles {backupDatabase = copy, backupLabels = labels}

-- | Before a command loses what the database holds, copy the database
-- and its labels into the auto-backups directory ('rollBackup'). The
-- answer is the file of each, which the answer of the command names.
--
-- deletewallet and deleteportfolio call it right before they delete, and
-- restoredatabase before it replaces the database. The copies hw makes
-- after a command hold the database as the command left it, without what
-- it lost. This one is the last that holds it.
--
-- It is one of the copies hw makes by itself, so it is with them and
-- stays as long as the rules for them keep it
-- ('autoBackupRetention'). No copy follows the command
-- (@backsUpBefore@), so this one is the newest copy until the next
-- one, and the last of its hour unless that one comes within the hour.
-- A user who wants it for good runs backup before the command, whose
-- copies hw never removes. The user-backups directory is for those
-- alone: a file there is one the user asked for.
--
-- A copy that cannot be made stops the command, with the text given
-- first, which says that nothing was done. The command has lost nothing
-- yet, so stopping is the safe answer. A copy after a command cannot
-- stop it any more, which is why that one only warns ('automaticCopy').
--
-- No copy is made when the configuration turns the copies of the
-- database off. The user asked hw to write no copy by itself. The answer
-- is then Nothing, and the command goes on.
--
-- The files stay when the command fails after the copy. A backup too
-- many does no harm.
backUpBeforeLosing :: Config -> String -> ExceptT String (DB IO) (Maybe BackupFiles)
backUpBeforeLosing cfg nothingDone
  | configAutoBackupDatabase cfg = Just <$> withExceptT stopped (backupNow (rollBackup autoBackupRetention) cfg)
  | otherwise = return Nothing
  where
    stopped reason =
      nothingDone
        <> ", since the backup that goes first could not be made. "
        <> reason
        <> ". To go on without a backup, set autobackup-database to false in the configuration"

-- | What the name of a copy made now says of the database: the time, and
-- how many wallets and portfolios the database holds. It is read in the
-- transaction of the copy.
copyStampNow :: ExceptT String (DB IO) CopyStamp
copyStampNow = do
  wallets <- lift getWallets
  portfolios <- lift getAllPortfolios
  now <- liftIO getCurrentTime
  return
    CopyStamp
      { stampTime = now,
        stampWallets = length wallets,
        stampPortfolios = length portfolios
      }

-- | Whether a command may change what exportwallet writes: the wallets
-- and their portfolios, their descriptions, the labels and the tags of
-- addresses and of transactions, and the coins the user locked. hw keeps
-- a copy of the labels after such a command (@backUpLabels@), unless it
-- is deletewallet, deleteportfolio or restoredatabase, which have their
-- copy before them (@backsUpBefore@).
--
-- Every command is named, with no case for the rest, so that the
-- compiler asks about each new command whether it changes them.
--
-- Some commands write a label the file does not carry yet. preparetx,
-- preparesendall, preparesweep and bumpfee label a transaction that is
-- not signed, and preparetx and preparesendall tag one with --tag. The
-- file holds the transactions that are signed alone: signtx, signsweep,
-- send, sendall or sweep then changes the file. preparetx changes the file all
-- the same with --tag-change, by the tags of its new change address,
-- which takes those of the coins spent
-- ("Haskoin.Wallet.Database.PendingTx".'changeTakesTags'). The file holds
-- every tagged address. A tag --tag made, which nothing
-- else has, is not in the file either, as no tag without a use is. receive
-- without --label and without --tag gives out an address without a label
-- or a tag, and the file holds the labelled and the tagged addresses
-- alone. bumpfee gives its replacement the tags of the transaction it
-- replaces, which the file writes once the replacement is signed.
--
-- sync changes the file too, but only in what the chain says: the
-- height, the fee and the value of a transaction, a pending transaction
-- that reached the chain, and a locked coin that was spent. A sync after
-- importwallet rebuilds all of it from the chain, so the copies need none
-- of it. Each sync that changed one of them would push out an older copy
-- instead, one that may hold a label the user changed since.
changesLabels :: Command -> Bool
changesLabels = \case
  -- A new wallet or portfolio, a description, or a portfolio gone
  CommandNewPortfolio _ -> True
  CommandRecover _ -> True
  CommandImportWallet _ _ -> True
  CommandDeletePortfolio _ _ -> True
  CommandDeleteWallet _ _ -> True
  -- A label of an address or of a transaction, or a description
  CommandLabel _ -> True
  CommandReceive opts -> isJust opts.label || not (null opts.tags)
  -- A tag of an address or of a transaction, a colour, a new name, or a
  -- tag gone: the file exportwallet writes holds the tags
  CommandTag _ -> True
  CommandUntag _ -> True
  CommandRenameTag _ -> True
  CommandDeleteTag _ _ -> True
  CommandTags -> False
  -- A contact added to the address book, or gone: the file exportwallet
  -- writes holds the address book. label, tag and untag change a contact
  -- too, and are counted above.
  CommandNewContact _ -> True
  CommandDeleteContact _ _ _ -> True
  CommandContacts {} -> False
  -- A coin locked or unlocked
  CommandLockCoin _ _ -> True
  CommandUnlockCoin _ _ -> True
  -- A signed transaction with its label, kept in the wallet, or gone
  CommandSignTx _ -> True
  CommandSignSweep {} -> True
  CommandSend _ -> True
  CommandSendAll _ -> True
  CommandSweep _ -> True
  CommandBroadcast {} -> True
  CommandImportTx _ -> True
  CommandDeleteTx _ _ -> True
  -- The tags of a change address, which the coins spent give it
  CommandPrepareTx opts -> opts.tagChange
  -- A transaction that is not signed yet, whose label and tags are not
  -- in the file
  CommandPrepareSendAll _ -> False
  CommandPrepareSweep _ -> False
  CommandBumpFee _ -> False
  -- What the chain says alone
  CommandSync _ _ -> False
  -- Neither the wallets nor their labels
  CommandCheckMnemonic -> False
  CommandExportWallet {} -> False
  CommandPortfolios _ -> False
  CommandSwitch _ _ -> False
  CommandSetDefault _ -> False
  CommandShowDefault _ -> False
  CommandShow {} -> False
  CommandAddresses {} -> False
  CommandBalances {} -> False
  CommandTxs {} -> False
  CommandPendingTxs {} -> False
  CommandCoins {} -> False
  CommandExportTx _ _ -> False
  CommandFees _ -> False
  CommandSignMsg _ -> False
  CommandVerifyMsg _ -> False
  CommandBackup _ -> False
  CommandShowBackups -> False
  CommandExportSheet _ _ -> False
  CommandExportJournal _ _ -> False
  CommandReport _ -> False
  -- Every wallet and label of a copy, in place of those of the database
  CommandRestoreDatabase _ _ -> True

-- | Write the labels of every wallet to a new copy in the auto-backups
-- directory next to the database, as exportwallet writes them, and keep
-- the copies there that the rotation keeps
-- ('autoBackupRetention', 'rollLabelCopy'). It runs once the command is
-- done, in a transaction of its own, so that the copy holds the wallet
-- as the command left it, and its name the counts of that database.
--
-- A database without a wallet writes no copy. Its file would be empty,
-- as exportwallet refuses to write it, and it would push out a copy that
-- still holds a wallet.
--
-- A send or a sendall that signed its transaction and could not send it
-- answers with an error, so no copy follows. Its transaction is kept with its label
-- all the same, and goes in the copy of the broadcast or the deletetx
-- the answer tells the user to run.
backUpLabels :: Config -> IO ()
backUpLabels cfg =
  automaticCopy "The labels were not backed up." cfg $ do
    let database = databaseFile cfg
    portfolios <- lift getAllPortfolios
    unless (null portfolios) $ do
      records <- databaseRecords portfolios
      stamp <- copyStampNow
      void . liftEitherIO $ rollLabelCopy autoBackupRetention database stamp (encodeUtf8 (recordsText records))

-- | Run a command in one transaction of the database, which no other
-- command changes meanwhile. What a command wrote before it failed is
-- undone, back to where it last called @keepSoFar@, if it did.
runDBResponse :: Config -> ExceptT String (DB IO) Response -> IO Response
runDBResponse cfg = runDBResponseOn cfg (runDB cfg)

-- | Run a command in one transaction of the database, as 'runDBResponse'
-- does, on the connection that the function given opens. It is
-- there for restoredatabase, whose connection also reads a copy of the
-- database ('runDBWithAttached'). A failure of SQLite is an answer too,
-- which says what to do ('catchDatabaseFailure').
runDBResponseOn :: Config -> (DB IO (Either String Response) -> IO (Either String Response)) -> ExceptT String (DB IO) Response -> IO Response
runDBResponseOn cfg run action = do
  result <- catchDatabaseFailure cfg . run $ do
    answer <- runExceptT action
    when (isLeft answer) transactionUndo
    return answer
  return $ answered (join result)

-- | Keep what the command wrote so far, whatever happens next: a failure
-- after this point undoes only what was written after it.
--
-- A command is one transaction of the database, so that a failure leaves
-- no trace. send and sendall need one exception (@prepareSignSend@). Each
-- signs a transaction, then hands it to the network, and the second step
-- can fail after the transaction left the computer, or be interrupted
-- while the store waits for its nodes. The signed transaction must be in
-- the wallet by then, with its coins locked, as it is between signtx and
-- broadcast. So the command ends its transaction of the database here,
-- and goes on in a new one, which takes the lock of the database again at
-- once.
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
-- When another command holds the database, or when SQLite fails on it,
-- every wallet is named, and the listing then fails on its first chunk.
listingWalletNaming :: Config -> IO WalletNaming
listingWalletNaming cfg = either (const WithWallets) id <$> tryDatabase (runDB cfg walletNaming)

-- | How wide the columns of tags of a listing in a table are, read
-- before its first chunk as 'listingWalletNaming' is, and for the same
-- reason: every chunk must agree. When the database cannot be read, the
-- columns are as wide as their heading, and the listing then fails on
-- its first chunk.
listingTagWidths :: Config -> IO TagWidths
listingTagWidths cfg = either (const (TagWidths 0 0 0)) id <$> tryDatabase (runDB cfg tagWidths)

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
-- shows, which the caller narrowed them to (@shownAmong@). They are
-- narrowed to the --network, when given. When what is left is on several
-- networks, as the same key is on each test network, the --network is
-- asked for. The default network does not choose between them: a sweep
-- on the wrong one takes the coins of another chain, and the README
-- promises a refusal. When it
-- is in several portfolios, the pool of the active portfolio is taken, when
-- it is one of them: the user who switched to a portfolio means that one.
-- Otherwise the refusal names them, for the user to switch to the one to
-- use. The first text says what was named, as in "The destinations of the
-- sweep".
poolAmong :: Maybe Network -> String -> [Pool] -> ExceptT String (DB IO) Pool
poolAmong netM what held = do
  activeM <- lift activePortfolioId
  let fitting = nub (filter (\pool -> maybe True (== pool.poolNetwork) netM) held)
      networks = nub ((.poolNetwork) <$> fitting)
  case (fitting, netM) of
    ([only], _) -> return only
    ([], Just net) -> throwError (what <> " are on " <> networksText (nub ((.poolNetwork) <$> held)) <> ", not on the --network " <> net.name)
    _
      | length networks > 1 -> throwError (what <> " are on " <> networksText networks <> ": give the --network")
      | [ofActive] <- filter ((== activeM) . Just . (.poolPortfolio)) fitting -> return ofActive
      | otherwise -> do
          places <- mapM poolPlace fitting
          throwError (what <> " are in several portfolios, " <> intercalateAnd (placeText <$> places) <> ": switch to the one to use first")

-- | Write keys of the configuration file of the data directory, as the
-- last step of a command: a file that cannot be written fails the
-- command, which undoes what it did in the database
writeDefaults :: Config -> [(Text, Toml.Value)] -> ExceptT String (DB IO) ()
writeDefaults cfg changes =
  ExceptT . liftIO . runExceptT $ setConfigKeys (configDataDirectory cfg) changes

{- Answers -}

-- | A portfolio as the answers show it, with what its coins are worth
-- and what it can spend now on each network hw shows. Its hidden
-- networks are left out of every view of it, and of its JSON. The view
-- only says whether the portfolio is used on one of them
-- ('viewUsedOnHidden'), without a name.
viewOfPortfolio :: Config -> Portfolio -> ExceptT String (DB IO) PortfolioView
viewOfPortfolio cfg portfolio = do
  view <- portfolioView portfolio coinTotals
  return
    view
      { viewNetworks = onShownNetworks cfg networkViewNetwork (viewNetworks view),
        viewUsedOnHidden = not (null (hiddenUse cfg view))
      }

-- | The hidden networks a portfolio is used on, given its view on every
-- network: those where it has addresses or coins ('networkInUse')
hiddenUse :: Config -> PortfolioView -> [Network]
hiddenUse cfg view = [networkViewNetwork net | net <- viewNetworks view, not (isShown cfg (networkViewNetwork net)), networkInUse net]

-- | The portfolios of a wallet that are used on hidden networks, each
-- with those networks: what recover tells of them
walletHiddenUse :: Config -> Natural -> ExceptT String (DB IO) [(PortfolioRef, [Network])]
walletHiddenUse cfg wallet = do
  portfolios <- lift . walletPortfolios =<< walletNamed wallet
  used <- forM portfolios $ \portfolio -> (portfolioRef portfolio,) . hiddenUse cfg <$> portfolioView portfolio coinTotals
  return [use | use@(_, _ : _) <- used]

-- | What hw knows of the funds of an address of a pool, given whether
-- the pool was ever synced and the totals of the coins read for its
-- addresses. The totals are those of the address, nothing when it holds
-- no coin. Before the first sync, they are not known: hw never asked
-- (the type t'AddressFunds').
fundsAt :: Bool -> Map Text CoinTotals -> DBAddress -> AddressFunds
fundsAt everSynced totals addr
  | everSynced = Holds (Map.findWithDefault mempty (dBAddressAddress addr) totals)
  | otherwise = NotSynced

-- | The answer showing an address of the account, with what hw knows of
-- its funds (@addressShown@) and its tags. The account may be of another portfolio than the active one:
-- show and label find the address in every portfolio.
addressAnswer :: DBAccountId -> DBAddress -> ExceptT String (DB IO) Response
addressAnswer = retaggedAnswer AddTags Nothing

-- | The answer of tag and of untag about an address of the account: the
-- address, as 'addressAnswer' shows it, then, when the command went down
-- the chain of its payments (--with-change), the change addresses that
-- followed it, each read again as it is now, and where the tags stopped
-- (@stopShown@). There may be none, and the answer then says so: the user
-- asked for them.
retaggedAnswer :: Retag -> Maybe ChainRetagged -> DBAccountId -> DBAddress -> ExceptT String (DB IO) Response
retaggedAnswer retag followedM accId addr = do
  acc <- getAccountById accId
  ref <- accountRefOf acc
  (_, funds, tags) <- addressShown addr
  changeM <- forM followedM $ \followed ->
    ChangeFollowed retag <$> mapM addressShown followed.chainChanged <*> mapM stopShown followed.chainStops
  ResponseAddress ref addr funds tags changeM <$> activePortfolioRef
  where
    stopShown :: ChainStop -> ExceptT String (DB IO) ChangeStop
    stopShown stop =
      ChangeStop
        <$> addressShown stop.stopAt
        <*> lift (shownTags stop.stopTags)
        <*> pure stop.stopPayment
        <*> mapM addressShown stop.stopSource

-- | An address as an answer shows it: with what hw knows of its funds,
-- the totals of its coins, what they are worth and what it can spend
-- now, or that its pool was never synced (@fundsAt@), and its tags
addressShown :: DBAddress -> ExceptT String (DB IO) (DBAddress, AddressFunds, [ShownTag])
addressShown addr = do
  acc <- getAccountById (dBAddressAccount addr)
  everSynced <- lift $ poolEverSynced (accountPool acc)
  totals <- coinTotalsAtAddresses (accountNetwork acc) (dBAddressAccount addr) [dBAddressAddress addr]
  tags <- lift $ addressTags addr
  return (addr, fundsAt everSynced totals addr, tags)

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

-- | How a section of a listing stands with the sync of its network.
--
-- A network where the portfolio never synced has no coin and no
-- transaction in the database, so its section of balances, coins or txs
-- has no row, and a section without rows shows nothing. The listing
-- would then say that there is nothing to display, of addresses hw
-- asked no server about. So such a section shows without a row, and
-- says that the network is not synced, when the accounts it covers have
-- an address there. A network without an address stays out, as
-- portfolios leaves it out: most users never touch most networks.
data SectionSync = SectionSync
  { -- | Whether a sync of the portfolio on the network ever finished
    sectionEverSynced :: !Bool,
    -- | Whether the section shows without a row, to say that its network
    -- is not synced ('listingShownEmpty')
    sectionShownNotSynced :: !Bool
  }

-- | How the section of the accounts of a portfolio on a network stands
-- with the sync
sectionSync :: Portfolio -> Network -> [(DBAccountId, DBAccount)] -> ExceptT String (DB IO) SectionSync
sectionSync portfolio net accounts = do
  everSynced <- lift $ poolEverSynced (portfolioPool portfolio net)
  return (SectionSync everSynced (not everSynced && any (hasAddresses . snd) accounts))
  where
    hasAddresses acc = dBAccountExternal acc + dBAccountInternal acc > 0

-- | The group of a listing for the accounts of a portfolio on a network,
-- given how its section stands with the sync
groupOf :: Portfolio -> Network -> [(DBAccountId, DBAccount)] -> SectionSync -> [row] -> Group row
groupOf portfolio net accounts state = Group (portfolioPlace portfolio net) (accountCounts . snd <$> accounts) state.sectionEverSynced

-- | The answer of a section of a listing: its rows in their group, or an
-- answer without a group when it has no row, so that a section without
-- rows adds nothing to the whole listing. A section that shows as not
-- synced answers with its group, though it has no row.
grouped :: SectionSync -> ([group] -> Response) -> ([row] -> group) -> [row] -> Response
grouped state answer group rows
  | null rows && not state.sectionShownNotSynced = answer []
  | otherwise = answer [group rows]

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
-- portfolio is active (@newPortfolioAnswer@).
--
-- A mnemonic typed with another passphrase is another seed, which hw
-- cannot tell from a mnemonic the user brings to this database: both are
-- of no wallet here. So --existing creates the wallet either way, and
-- warns when the database had wallets, since the user may have meant one
-- of them (@newWalletWarning@). With --match the user says that the
-- wallet is here, and a seed of no wallet is refused (@unmatchedRefusal@).
cmdNewPortfolio :: Config -> NewPortfolio -> IO Response
cmdNewPortfolio cfg opts
  | opts.match && not opts.existing =
      return $ ResponseError "--match goes with --existing: it refuses a mnemonic that is of no wallet of the database"
  | opts.existing = runDBResponse cfg $ do
      -- Each of these would do nothing here. Refused, it does not let
      -- the user believe it did something.
      when (opts.dice || isJust opts.shares || isJust opts.threshold || isJust opts.entropy || opts.oneLine || opts.showMasterKey) $
        throwError "--entropy, --dice, --shares, --threshold, --one-line and --show-master-key are for a new mnemonic: they do not go with --existing"
      had <- lift $ map (walletIndex . snd) <$> getWallets
      -- No mnemonic can match, so none is asked for
      when (opts.match && null had) $
        throwError "There is no wallet yet for a mnemonic to match: nothing was created. Without --match, newportfolio --existing creates the wallet of the mnemonic"
      secret <- askNewSeedSecret terminal
      seed <- seedToCreateWith secret
      heldM <- seedWallet seed
      when (opts.match && isNothing heldM) $ throwError (unmatchedRefusal secret)
      -- A wallet created beside others may be a mistake
      let warning created = newWalletWarning secret had (refWallet created) <$ guard (isNothing heldM && not (null had))
      newPortfolioAnswer cfg Nothing Nothing warning $ do
        held <- maybe (lift (insertWallet (seedFingerprint seed))) return heldM
        index <- lift $ nextPortfolioIndex seed (fst held)
        createPortfolio seed held index
  | otherwise = do
      -- The secret is made and its passphrase asked before the database
      -- is held: the user may take a while
      made <- runExceptT newSecret
      case made of
        Left err -> return (ResponseError (cs err))
        Right (shown, secret) -> runDBResponse cfg $ do
          seed <- liftEither $ secretSeed secret
          -- Shown with the words, once, and only when asked for
          masterKeyM <- liftEither $ if opts.showMasterKey then Just <$> masterKeyText secret else Right Nothing
          -- A new mnemonic is a new wallet. The same fingerprint in the
          -- database would mean the same seed came out twice.
          clash <- lift $ walletWithFingerprint (seedFingerprint seed)
          for_ clash $ \(_, wallet) ->
            throwError $
              "The new mnemonic has the fingerprint of " <> cs (walletName (walletIndex wallet)) <> ": nothing was created. Run the command again for another mnemonic"
          newPortfolioAnswer cfg (Just shown) masterKeyM (const Nothing) $ do
            held <- lift $ insertWallet (seedFingerprint seed)
            createPortfolio seed held 0
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

-- | The answer of newportfolio: the secret it made, if any, its master
-- private key when --show-master-key asks for it, the portfolio the
-- action creates, the active portfolio after it, which is the new one
-- when the database had none, and the warning that goes with the
-- portfolio created, if any
newPortfolioAnswer :: Config -> Maybe NewSecret -> Maybe Text -> (PortfolioRef -> Maybe Text) -> ExceptT String (DB IO) Portfolio -> ExceptT String (DB IO) Response
newPortfolioAnswer cfg shownM masterKeyM warningOf create = do
  (portfolio, activeM) <- withActiveAfter create
  -- The database has a portfolio now, so it has an active one
  active <- liftMaybe noWallets activeM
  view <- viewOfPortfolio cfg portfolio
  return $ ResponseNewPortfolio shownM masterKeyM view active (warningOf (viewRef view))

-- | What newportfolio --existing says of a secret that gives the keys of
-- no wallet of the database
ofNoWallet :: SeedSecret -> String
ofNoWallet = \case
  MnemonicSecret _ -> "This mnemonic and passphrase are those of no wallet here"
  SharesSecret _ _ -> "These shares and passphrase are those of no wallet here"
  MasterKeySecret _ -> "This master private key is that of no wallet here"

-- | The warning of newportfolio --existing when it created the wallet of
-- the secret beside the wallets the database had, given their indexes and
-- the index of the new one. The same typo of a passphrase, typed twice,
-- gives a wallet of a secret the user does not know they typed, and funds
-- sent to it can be spent with that typo only. The answer would otherwise
-- read as the one of a portfolio added, and propose to switch to it. So
-- it names the wallets the user may have meant and the command that
-- deletes the new one, which holds nothing yet.
newWalletWarning :: SeedSecret -> [Natural] -> Natural -> Text
newWalletWarning secret had created =
  cs $
    ofNoWallet secret
      <> (": a new " <> name created <> " was created. If you meant ")
      <> (orList (name <$> had) <> ", " <> differed <> ": delete ")
      <> (name created <> " with hw deletewallet " <> show created)
  where
    name = cs . walletName
    differed = case secret of
      MnemonicSecret _ -> "the mnemonic or the passphrase was not the same"
      SharesSecret _ _ -> "the shares or the passphrase were not the same"
      MasterKeySecret _ -> "the key was not the same"

-- | Why newportfolio --existing --match creates nothing from a secret
-- that gives the keys of no wallet of the database
unmatchedRefusal :: SeedSecret -> String
unmatchedRefusal secret = ofNoWallet secret <> ": nothing was created. Without --match, newportfolio --existing creates a new wallet"

-- | The seed newportfolio --existing adds a portfolio from: the one BIP39
-- derives from the mnemonic and the passphrase, or the one SLIP-39
-- derives from a backup in shares and its passphrase. For a mnemonic with
-- a passphrase that normalizing changes, hw 0.9 derived another seed,
-- from the passphrase as typed, and recover makes a wallet of that seed
-- when it finds its funds. When the database has a wallet of that seed
-- and none of the other, the new portfolio is of it too, so that one
-- mnemonic keeps one wallet, and the user is told.
seedToCreateWith :: SeedSecret -> ExceptT String (DB IO) PublicSeed
seedToCreateWith secret = do
  seed <- liftEither $ secretSeed secret
  typedM <- liftEither $ secretTypedSeed secret
  case typedM of
    Nothing -> return seed
    Just typed -> do
      ofNormalized <- isJust <$> seedWallet seed
      ofTyped <- isJust <$> seedWallet typed
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
-- the command exit with an error.
--
-- One thing is told of the hidden networks: which of them each portfolio
-- of the wallet is used on (@walletHiddenUse@), with the command that
-- shows them. recover runs once, and its answer is where the user learns
-- what the mnemonic holds. Without it, a portfolio that was paid on a
-- hidden network alone is recovered, has nothing on any network of the
-- answer, and the funds stay unseen, on Bitcoin Cash for the user who
-- hid it. In a database without any portfolio,
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
-- taken. When nothing at all was found, the answer says to check the
-- passphrase (@passphraseHint@). When the seed has no wallet and no paid
-- portfolio, and normalizing changes the passphrase, the portfolios hw
-- derived before, from the passphrase as typed, are looked for too, and
-- the user is told of those. Only the seed of BIP39 is looked at for the
-- other kinds of accounts: other wallets derive from no other.
cmdRecover :: Config -> Recover -> IO Response
cmdRecover cfg opts =
  withStatusLine $ \line ->
    runDBResponse cfg $ do
      storeOn <- liftIO $ webStores cfg
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
      seed <- liftEither $ secretSeed secret
      found <- recoverSeed cfg storeOn searched seed opts.portfolioGap watch
      typedM <- liftEither $ secretTypedSeed secret
      -- The networks whose store has not failed yet
      let upAfter failed = [net | net <- searched, net `notElem` (fst <$> failed)]
      (recovered, failedInRecovery) <- case typedM of
        Just typed | isNothing (recoveredWallet found) -> do
          fromTyped <- recoverSeed cfg storeOn (upAfter (recoveredDown found)) typed opts.portfolioGap watch
          let failed = recoveredDown found <> recoveredDown fromTyped
          if isJust (recoveredWallet fromTyped)
            then do
              liftIO $ eraseStatus line >> tell terminal typedPassphraseRecovered
              return (fromTyped, failed)
            else return (found, failed)
        _ -> return (found, recoveredDown found)
      -- The funds at other paths, on each network whose store is still up
      fundsSearched <- forM (upAfter failedInRecovery) $ \net ->
        storeFailureApart (storeOn net) $ \store -> map (net,) <$> foreignFunds cfg store net seed opts.portfolioGap watch
      let elsewhere = concat [funds | Right funds <- fundsSearched]
          failed = failedInRecovery <> [(net, failure) | (net, Left failure) <- zip (upAfter failedInRecovery) fundsSearched]
      active <- activeAfterFrom activeBefore
      usedOnHidden <- maybe (return []) (walletHiddenUse cfg) (recoveredWallet recovered)
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
          (if isNothing (recoveredWallet recovered) && null elsewhere then passphraseHint secret else Nothing)
          usedOnHidden

-- | What recover adds when it found nothing of a secret that takes a
-- passphrase: no portfolio, and no funds at the paths hw does not hold.
-- A mistyped passphrase is the usual reason for an empty answer from a
-- mnemonic that was paid: it gives the seed of another wallet, which was
-- never paid. Without the hint the answer reads as "the funds are gone".
-- A master private key takes no passphrase, and gets none.
passphraseHint :: SeedSecret -> Maybe Text
passphraseHint = \case
  MnemonicSecret _ -> Just (hintFor "this mnemonic")
  SharesSecret _ _ -> Just (hintFor "this backup")
  MasterKeySecret _ -> Nothing
  where
    hintFor secret = "If " <> secret <> " was paid, check the passphrase: another passphrase, or none, gives another wallet."

-- | Check a mnemonic and its passphrase, the shares of a backup, or a
-- master private key, against the active portfolio: they match when they give the key of
-- every account the portfolio has. Nothing is written. An answer that
-- they do not match makes hw exit with an error ('responseFailed').
--
-- A secret that does not match may be that of another wallet of the
-- database, which the answer then names (@otherWalletOf@). A user with a
-- sheet of words and several wallets would otherwise switch to each
-- wallet in turn, and read the first answer as a bad backup.
cmdCheckMnemonic :: Config -> IO Response
cmdCheckMnemonic cfg =
  runDBResponse cfg $ do
    portfolio <- requireActivePortfolio
    accounts <- lift $ portfolioAccountRows portfolio.portfolioId
    secret <- askSeedSecret terminal
    seed <- liftEither $ secretSeed secret
    typedM <- liftEither $ secretTypedSeed secret
    let gives candidate = liftIO $ and <$> mapM (seedDerives candidate portfolio . snd) accounts
    normalized <- gives seed
    asTyped <- if normalized then return False else maybe (return False) gives typedM
    -- Other wallets would not find a portfolio of the passphrase as typed
    when asTyped $ liftIO $ tell terminal typedPassphraseChecked
    let matched = normalized || asTyped
    otherM <- if matched then return Nothing else otherWalletOf (seed : maybeToList typedM)
    let (typed, theyMatch) = case secret of
          MnemonicSecret _ -> ("The mnemonic and passphrase", "they match ")
          SharesSecret _ _ -> ("The shares and passphrase", "they match ")
          MasterKeySecret _ -> ("The master private key", "it matches ")
        said =
          typed
            <> (if matched then " matched " else " did not match ")
            <> cs (portfolioText portfolio)
            <> maybe "" (\other -> ": " <> theyMatch <> cs (walletName other)) otherM
    return $ ResponseChecked (portfolioRef portfolio) matched said

-- | The wallet of the database that the first of the seeds with one is
-- of, by its index: what checkmnemonic names when a secret does not
-- match the active portfolio. The seeds are those of one secret, the
-- passphrase normalized and as typed. A wallet that has the fingerprint
-- of a seed and other keys is not the wallet of that seed ('seedWallet').
otherWalletOf :: [PublicSeed] -> ExceptT String (DB IO) (Maybe Natural)
otherWalletOf seeds = listToMaybe . catMaybes <$> mapM walletOf seeds
  where
    walletOf seed = (fmap (walletIndex . snd) <$> seedWallet seed) `catchError` \_ -> return Nothing

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
cmdImportWallet :: Config -> ImportScope -> FilePath -> IO Response
cmdImportWallet cfg scope file =
  runDBResponse cfg . tellingStopped $ do
    content <- liftEitherIO $ readTextFile file
    -- A key or a descriptor alone, or a file of another kind, holds no
    -- record: it is refused, not read as an empty file
    unless (holdsAnything (readWalletFile content)) $ throwError (couldNotRead file notAWalletFile)
    -- The import is undone whole when it is refused, which the refusal
    -- says. show shares its refusals, and changes nothing either way.
    (report, active) <- withActiveAfter $ withExceptT (<> ". Nothing was changed") (importFile scope content)
    return (ResponseImport report active)

-- | Why a file without any record is refused by importwallet
notAWalletFile :: String
notAWalletFile =
  "It holds no record of a wallet. importwallet reads the file exportwallet writes. \
  \A key or a descriptor alone is one account, and does not say which portfolio it belongs to"

-- | Write portfolios to a BIP 329 file, with their labels and the coins
-- the user locked. Without options, the file holds every portfolio of
-- every wallet, then the address book ('databaseRecords'). --wallet and
-- --portfolio write less (@portfoliosToExport@), and no address book.
cmdExportWallet :: Config -> Maybe Natural -> Maybe Natural -> FilePath -> IO Response
cmdExportWallet cfg walletM portfolioM file =
  runDBResponse cfg $ do
    portfolios <- portfoliosToExport walletM portfolioM
    checkFileToWrite file
    -- The address book goes in the file of the whole database alone: a
    -- file narrowed to a wallet or a portfolio is made to show less
    records <- if isNothing walletM && isNothing portfolioM then databaseRecords portfolios else walletRecords portfolios
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
-- one the commands work in from now on, and show it ('switchTo').
-- --wallet alone names portfolio0 of that wallet.
cmdSwitch :: Config -> Maybe Natural -> Maybe Natural -> IO Response
cmdSwitch cfg walletM portfolioM =
  runDBResponse cfg $ do
    portfolio <- switchTo walletM portfolioM
    ResponseSwitched <$> viewOfPortfolio cfg portfolio

-- | Write the defaults the options give to the configuration file: the
-- network, the type of address, the unit, the view of the listings and
-- whether they list their rows in the reverse order, the networks hw
-- shows, and whether hw keeps a journal for ledger. Each is written by
-- the name the file reads back, which is also the name the answer shows.
--
-- --journal and --no-journal only write the key here. The journal itself
-- is written or removed once the command is done (@journalAfter@), as
-- after any command that changes it. The answer says where the journal
-- is, or that it goes, in a t'JournalSwitch'. The portfolio the commands
-- work in is no default of the file: switch makes another one active.
--
-- --show-network and --hide-network change the networks hw shows
-- (@shownAfter@). The default network, the one --network names or the
-- one the file has, must be among them once the command is done
-- (@checkDefaultStaysShown@). So one command shows a network and makes it
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
        -- The switches setdefault writes: true or false, not a name
        switched =
          [(listingReverseKey, order == ReverseOrder) | Just order <- [opts.listingOrder]]
            <> [(journalKey, kept) | Just kept <- [opts.journal]]
        changesNetworks = not (null opts.showNetworks && null opts.hideNetworks)
        names = [cs net.name | net <- shown] :: [Text]
    when (null named && null switched && not changesNetworks) $
      throwError "Give the default to set: --network, --type, --unit, --compact, --verbose, --tabular, --reverse, --no-reverse, --show-network, --hide-network, --journal or --no-journal"
    writeDefaults cfg $
      [(key, Toml.Text name) | (key, name) <- named]
        <> [(key, Toml.Bool switch) | (key, switch) <- switched]
        <> [(shownNetworksKey, Toml.List (Toml.Text <$> names)) | changesNetworks]
    let file = configFile (configDataDirectory cfg)
        written =
          named
            <> [(key, switchText switch) | (key, switch) <- switched]
            <> [(shownNetworksKey, shownNetworksText shown) | changesNetworks]
    return $ ResponseDefaults written file ((`JournalSwitch` journalFile cfg) <$> opts.journal)

-- | Show the defaults setdefault writes, as the configuration has them,
-- and the active portfolio, which switch changes. Each switch asks for
-- one of them, and no switch for all of them ('showsEverything'). A user
-- who wonders where a command works wants to see them together, and a
-- script asks for the one it reads.
--
-- A key the file does not have shows the value hw takes without it: the
-- value the commands work with, which is what the user wants to know.
-- The keys come in the order setdefault writes them, whatever the order
-- of the switches.
--
-- The active portfolio is no key of the file. It is read from the
-- database, and comes after the keys. --portfolio names it with its
-- wallet, since a portfolio is named by both. --wallet names the wallet
-- alone, for a user of several mnemonics who wants to know which one the
-- commands work with. A database without any portfolio has none, which
-- the answer says. It is no failure: the question has an answer.
cmdShowDefault :: Config -> ShowDefault -> IO Response
cmdShowDefault cfg opts =
  runDBResponse cfg $ do
    active <- activeShownNow
    let file = configFile (configDataDirectory cfg)
    return $ ResponseShownDefaults keys file active
  where
    included switchGiven = showsEverything opts || switchGiven
    keys =
      [(defaultNetworkKey, cs (configDefaultNetwork cfg).name) | included opts.askedNetwork]
        <> [(defaultTypeKey, kindName (configDefaultType cfg)) | included opts.askedType]
        <> [(unitKey, unitName (configUnit cfg)) | included opts.askedUnit]
        <> [(listingViewKey, viewName (configListingView cfg)) | included opts.askedView]
        <> [(listingReverseKey, switchText (configListingReverse cfg)) | included opts.askedReverse]
        <> [(shownNetworksKey, shownNetworksText (configShownNetworks cfg)) | included opts.askedShownNetworks]
        <> [(journalKey, switchText (configJournal cfg)) | included opts.askedJournal]
    -- --portfolio names the wallet too, so it wins over --wallet
    activeShownNow
      | included opts.askedPortfolio = ActivePortfolioShown <$> activePortfolioRef
      | opts.askedWallet = ActiveWalletShown . fmap refWallet <$> activePortfolioRef
      | otherwise = return ActiveNotAsked

-- | A switch of the configuration, as the file writes it and as the
-- answers of setdefault and showdefault give it: true or false
switchText :: Bool -> Text
switchText switch = if switch then "true" else "false"

-- | The networks hw shows, as the answers of setdefault and showdefault
-- give them: their names in a line, as in "btc, bch, btctest4"
shownNetworksText :: [Network] -> Text
shownNetworksText networks = Text.intercalate ", " [cs net.name | net <- networks]

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

{- The question before a deletion -}

-- | Ask before a command deletes something from the database for good,
-- unless --yes. The five commands that start with delete call it:
-- deleteportfolio, deletewallet, deletetx, deletetag and deletecontact.
--
-- The names of some of them are close to those of harmless commands, as
-- deletetag is to untag. So each one first says what is about to go, in
-- the words of the notice given, and deletes only after a y or a yes. A
-- no stops the command with an error, and the database is as it was.
--
-- Each sentence of the notice has a line of its own, and the question
-- comes on the last line, where the answer is typed. The question of a
-- wallet behind the chain is laid out the same way ('confirmBehind').
--
-- A command calls it after its refusals and before it writes anything.
-- So a deletion that cannot happen is refused without a question, and a
-- no leaves no backup behind.
confirmDeletion :: Bool -> [String] -> String -> ExceptT String (DB IO) ()
confirmDeletion yes notice asked =
  unless yes $ do
    liftIO $ mapM_ (tell terminal) notice
    agreed <- askYes terminal asked
    unless agreed $ throwError "Nothing was deleted"

-- | The sentence of a notice that says whether hw backs up the database
-- before the deletion (@backUpBeforeLosing@)
backupFirstNotice :: Config -> String
backupFirstNotice cfg
  | configAutoBackupDatabase cfg = "hw copies the database and its labels into auto-backups first."
  | otherwise = "The database is not copied first, since autobackup-database is false in the configuration."

-- | A text of the user between double quotes, as the listings show a
-- label or a description
quoted :: Text -> String
quoted said = "\"" <> cs said <> "\""

-- | What deleteportfolio says before its question: the portfolio that
-- goes, with its description when it has one, that nothing goes with it,
-- and whether the database is copied first
deletePortfolioNotice :: Config -> Portfolio -> [String]
deletePortfolioNotice cfg portfolio =
  [portfolioText portfolio <> " will be deleted from the database."]
    <> ["Its description is " <> quoted description <> "." | not (Text.null description)]
    <> [ "It was never used, so no address, coin or transaction goes with it.",
         backupFirstNotice cfg
       ]
  where
    description = dBPortfolioDescription portfolio.portfolioRow

-- | What deletewallet says before its question: the wallet that goes and
-- how many portfolios it has, what tells it from another wallet, what
-- goes with it, that the funds are not touched, and whether the database
-- is copied first.
--
-- The description and the fingerprint are there for the user to check
-- that the index names the wallet they mean. An index alone is easy to
-- mistype.
deleteWalletNotice :: Config -> DBWallet -> Int -> [String]
deleteWalletNotice cfg wallet portfolioCount =
  [ name <> " will be deleted from the database, with " <> portfolios <> ".",
    identity,
    "Every address, label, transaction, coin and pending transaction of the wallet goes with it.",
    "The funds stay on the chain, and the mnemonic can still spend them.",
    backupFirstNotice cfg
  ]
  where
    name = cs (walletName (walletIndex wallet)) :: String
    portfolios
      | portfolioCount == 1 = "its portfolio"
      | otherwise = "its " <> show portfolioCount <> " portfolios"
    fingerprint = cs (fingerprintToText (walletFingerprint wallet)) :: String
    description = dBWalletDescription wallet
    identity
      | Text.null description = "Its fingerprint is " <> fingerprint <> "."
      | otherwise = "Its description is " <> quoted description <> ", and its fingerprint is " <> fingerprint <> "."

-- | What deletetag says before its question: the tag that goes, how many
-- addresses, transactions and contacts lose it, and the command that
-- takes a tag off one of them alone. deletetag is easy to take for
-- untag, so the notice names untag while there is still time. The
-- sentences read the same for one holder and for several.
--
-- A tag that nothing has is said to be so, and untag is left out: there
-- is nothing to take it off.
deleteTagNotice :: TagUse -> [String]
deleteTagNotice use =
  ("The tag " <> name <> " will be deleted from the whole database.") : holders <> unlocked
  where
    -- The coins the tag kept out of the payments are free once it is gone
    unlocked =
      [ "It locks its coins. Once it is deleted, any payment may spend the coins at its addresses."
        | use.tagUseLocksCoins
      ]
    name = cs (tagText (shownTag use.tagUseTag)) :: String
    counts =
      [ count <> " " <> (if held == 1 then one else several)
        | (held, one, several) <-
            [ (use.tagUseAddresses, "address", "addresses"),
              (use.tagUseTxs, "transaction", "transactions"),
              (use.tagUseContacts, "contact", "contacts")
            ],
          held > 0,
          let count = show held
      ]
    holders = case counts of
      [] -> ["No address, no transaction and no contact has it."]
      _ ->
        [ "It comes off " <> listed counts <> ", and no label or other tag changes.",
          "To take a tag off one address, transaction or contact and keep the tag, use untag instead."
        ]
    -- "a", "a and b", or "a, b and c"
    listed [one] = one
    listed parts = intercalate ", " (init parts) <> " and " <> last parts

-- | What deletecontact says before its question: the contact that goes,
-- with its label when it has one, that its tags stay in the database,
-- and what becomes of the transactions that paid it
deleteContactNotice :: Contact -> [String]
deleteContactNotice contact =
  ["The contact " <> cs (contactAddress contact) <> label <> " will be deleted from the address book."]
    <> ["Its tags come off it, and the tags themselves stay." | not (null (contactTags contact))]
    <> ["The transactions that paid the address stay in the history, and show it as the address of a stranger."]
  where
    label
      | Text.null (contactLabel contact) = ""
      | otherwise = " " <> quoted (contactLabel contact)

-- | What deletetx says before its question: the pending transaction that
-- goes, by the short id pendingtxs shows, where it stands, and what
-- becomes of its coins.
--
-- Where it stands tells the user what the deletion costs. An unsigned
-- transaction is prepared again in a moment. A signed one took the
-- mnemonic. One in conflict can only be deleted.
deleteTxNotice :: PendingTx -> [String]
deleteTxNotice held =
  [ "The pending transaction " <> shortHashText (shownHash tx) <> " will be deleted from the wallet, with its label and its tags.",
    standing,
    "The coins it spends can be spent again, and its change address is free again."
  ]
  where
    tx = txSignDataTx held.pendingTxData
    standing
      | held.pendingTxConflicted = "It is in conflict with another transaction, and can no longer be sent."
      | held.pendingTxOnline = "It was sent, and the server no longer knows it."
      | isSigned tx = "It is signed, and was not sent."
      | otherwise = "It is not signed."

{- Deleting portfolios and wallets -}

-- | Delete a portfolio of the wallet of the active portfolio that was
-- never used: not portfolio 0, not the active one, the last of its
-- wallet, without any activity, and without any address given out
-- ('portfolioHasActivity'). The portfolio is named by its index. The
-- active portfolio is refused rather than replaced by another one: which
-- one the user works in next is theirs to say, with switch.
--
-- Once every rule is passed, the user is asked, unless --yes
-- (@confirmDeletion@). The database and its labels are then copied
-- (@backUpBeforeLosing@). A portfolio that is refused, or that the user
-- keeps, leaves no copy.
cmdDeletePortfolio :: Config -> Natural -> Bool -> IO Response
cmdDeletePortfolio cfg index yes =
  runDBResponse cfg $ do
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
    confirmDeletion yes (deletePortfolioNotice cfg portfolio) ("Delete " <> name <> "?")
    backup <- backUpBeforeLosing cfg "Nothing was deleted"
    lift $ deletePortfolio portfolio.portfolioId
    return $ ResponseDeleted (cs name <> " has been deleted") backup

-- | Delete a wallet with all of its portfolios, whatever they hold. The
-- wallet is named by its index. The wallet of the active portfolio is
-- refused while another wallet exists: which one the user works in next
-- is theirs to say, with switch. The last wallet goes with the active
-- portfolio, and the next portfolio created becomes active, as in a new
-- database.
--
-- The user is asked first, unless --yes (@confirmDeletion@). The
-- question shows the description and the fingerprint of the wallet, so
-- that a mistyped index is caught there. The command once took the
-- fingerprint as an argument for that. The user had to look it up in
-- portfolios --verbose, and the question now shows it instead.
--
-- The database and its labels are then copied (@backUpBeforeLosing@).
-- The copy keeps what nothing else brings back: the labels of the
-- wallet, and its pending transactions. A wallet that is refused, or
-- that the user keeps, leaves no copy.
cmdDeleteWallet :: Config -> Natural -> Bool -> IO Response
cmdDeleteWallet cfg index yes =
  runDBResponse cfg $ do
    (walletId, wallet) <- walletNamed index
    let name = cs (walletName index) :: String
    others <- filter ((/= walletId) . fst) <$> lift getWallets
    activeM <- activePortfolio
    for_ activeM $ \active ->
      when (active.portfolioWalletId == walletId && not (null others)) $
        throwError (name <> " holds the active portfolio, " <> portfolioText active <> ": switch to a portfolio of another wallet first")
    portfolios <- lift $ getPortfolios walletId
    confirmDeletion yes (deleteWalletNotice cfg wallet (length portfolios)) ("Delete " <> name <> "?")
    backup <- backUpBeforeLosing cfg "Nothing was deleted"
    lift $ deleteWallet walletId
    -- The raw transactions its coins came from are of no use any more
    _ <- dropOrphanRawTxs
    let said =
          cs name
            <> " has been deleted, with all of its portfolios"
            <> (if null others then ". The database has no wallet left: the first portfolio created next becomes the active one." else "")
    return $ ResponseDeleted said backup

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
  free <- runExceptT (checkFileToWrite file)
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
-- The last tab holds the address book, on the networks hw shows.
-- Each listing is read in its usual order ('everyRow'): a spreadsheet
-- sorts its rows as the reader likes.
--
-- The listings are read in one transaction, so that the tabs agree with
-- each other: the balances with the coins, the coins with the
-- transactions, and the addresses with their transactions. The
-- spreadsheet is the record of the whole wallet, so the active portfolio
-- does not narrow it. It holds what the listings show: the networks the
-- configuration hides are left out of it, as they are of each listing.
--
-- With --tag or --untagged, each tab holds what its listing gives with
-- the same options: the addresses of a tag with their balances and
-- their coins, and the transactions and the contacts of that tag
-- ('txFilterOfRows'). The tab of the portfolios has no tags and stays
-- whole. A word that is no tag of the database is refused before
-- anything is read.
cmdExportSheet :: Config -> FilePath -> TagFilter -> IO Response
cmdExportSheet cfg file kept =
  runDBResponse cfg $ do
    checkFileToWrite file
    _ <- requireTags kept.keptTags
    portfolios <- lift getAllPortfolios
    when (null portfolios) $ throwError noWallets
    summary <- sheetOf (portfoliosListing cfg)
    held <- mapM portfolioSheets portfolios
    -- The pending transactions of every portfolio in one tab, and their
    -- addresses in another
    pending <- txSheetsOf (pendingTxsListing cfg Nothing keptTxs everyRow)
    -- The address book, which is of no portfolio, in a tab of its own
    contacts <- sheetOf (contactsListing cfg Nothing kept everyRow)
    now <- liftIO getCurrentTime
    liftEitherIO $ writeFileBytes file (workbookOds now (walletSheets summary held pending contacts))
    return $ ResponseFile file
  where
    -- The transactions the tabs of transactions keep
    keptTxs = txFilterOfRows kept
    -- The listings of one portfolio, over every network hw shows: its
    -- receiving addresses then its change addresses, its balances, all
    -- its coins, and its transactions with their addresses
    portfolioSheets portfolio = do
      let whole = Target Nothing Nothing
      receiving <- sheetOf (addressesListing cfg portfolio whole False kept everyRow)
      change <- sheetOf (addressesListing cfg portfolio whole True kept everyRow)
      PortfolioSheets (stacked [receiving, change])
        <$> sheetOf (balancesListing cfg portfolio whole kept everyRow)
        <*> sheetOf (coinsListing cfg portfolio whole False kept everyRow)
        <*> txSheetsOf (txsListing cfg portfolio Nothing keptTxs everyRow)

-- | Write the transactions of every portfolio to a new journal of
-- double-entry accounting, in the format of the ledger command line tool
-- ("Haskoin.Wallet.Cli.Journal"), and answer with the file.
--
-- The journal is of the whole wallet, as the spreadsheet of exportsheet
-- is, and for a stronger reason. A payment from one portfolio to another
-- is one entry that moves the money between two accounts. The journal of
-- one portfolio alone would count that money as spent. So the active
-- portfolio does not narrow the journal, and the command takes no
-- portfolio.
--
-- The transactions are those txs lists, read for every portfolio in one
-- transaction of the database, so that both sides of a payment between
-- two portfolios agree (@journalNow@). The networks the configuration
-- hides are left out, as they are of txs. What a payment is posted to
-- follows the accounts the configuration gives to tags
-- ('configJournalAccounts').
--
-- With --tag, --address-tag or --untagged, the journal holds the
-- transactions txs lists with the same options, in any portfolio, and
-- states no balances at its end (@txSelection@).
--
-- The file is written whole, and a path that is taken is refused, as
-- for every file hw writes. The user deletes the old journal first.
cmdExportJournal :: Config -> FilePath -> TxTagFilter -> IO Response
cmdExportJournal cfg file kept =
  runDBResponse cfg $ do
    checkFileToWrite file
    portfolios <- lift getAllPortfolios
    when (null portfolios) $ throwError noWallets
    selected <- txSelection cfg portfolios kept
    journal <- journalNow cfg selected portfolios
    liftEitherIO $ writeFileBytes file journal
    return $ ResponseFile file

-- | The journal of the portfolios given as the bytes of its file, as the
-- database has them now, on the networks hw shows, its dates on the clock
-- of this computer, and its payments filed under the accounts the
-- configuration gives to tags ("Haskoin.Wallet.Cli.Journal"). With a
-- selection, it is the journal of the transactions the selection keeps.
-- Without one, exportjournal and the journal hw keeps write the same
-- bytes.
--
-- The transactions are those txs lists for each portfolio, on each
-- network hw shows. They are read whole, in a few queries for each
-- portfolio and network ('wholeHistory'), and not through the listing of
-- txs, which asks the database about each transaction in turn.
journalNow :: Config -> Maybe TxSelection -> [Portfolio] -> ExceptT String (DB IO) BS.ByteString
journalNow cfg selected portfolios = do
  histories <- forM portfolios $ \portfolio -> do
    pools <- listedPools cfg portfolio Nothing
    forM pools $ \pool -> do
      listed <- wholeHistory pool
      return [(portfolioPlace portfolio pool.poolNetwork, row) | row <- listed]
  encodeUtf8 <$> liftIO (localJournal (configJournalAccounts cfg) selected (concat (concat histories)))

{- Reports -}

-- | Write a report to a new PDF file, and answer with the file
-- ("Haskoin.Wallet.Report.Layout"). The option of the report says which
-- one.
--
-- A report is of the whole database, as the spreadsheet and the journal
-- are: it is what the owner declares, and the active portfolio is only
-- where the commands work. It holds the networks hw shows. --wallet and
-- --portfolio narrow it down (@reportScope@). The history of every
-- portfolio is read all the same, since a narrowed statement is worked
-- out from all of them ("Haskoin.Wallet.Report.Statement").
--
-- --tag, --address-tag and --untagged keep an account statement to the
-- transactions txs lists with the same options in each portfolio
-- (@txSelection@). The statement is worked out whole first, then loses
-- the other lines, and with them its balances
-- ("Haskoin.Wallet.Report.AccountStatement").
--
-- --tag and --untagged keep a balance statement to the addresses
-- balances lists with the same options in each portfolio
-- (@addressSelection@). It too is worked out whole first, then loses
-- the other addresses, and is valued after that: the prices it needs
-- are those of the assets these addresses hold
-- ("Haskoin.Wallet.Report.BalanceStatement").
--
-- A payment confirmation is of one transaction, which it finds in every
-- portfolio, as show does. --wallet and --portfolio then say whose side
-- of the payment it is on
-- ("Haskoin.Wallet.Report.PaymentConfirmation").
--
-- Everything that can refuse the report comes first: the options that do
-- not go together, the days, the last sync, a missing price. The server
-- is asked last, for the hashes of the blocks the report names, and a
-- server that does not answer refuses nothing (@blockHashesAt@).
--
-- The file is written whole, and a path that is taken is refused, as
-- for every file hw writes.
cmdReport :: Config -> ReportOptions -> IO Response
cmdReport cfg opts =
  runDBResponse cfg $ do
    checkFileToWrite opts.file
    liftEither $ checkReportOptions opts
    valuation <- liftEither $ valuationOf opts
    now <- liftIO getZonedTime
    portfolios <- lift getAllPortfolios
    when (null portfolios) $ throwError noWallets
    scope <- reportScope opts.wallet opts.portfolio
    report <- case opts.kind of
      BalanceReport -> do
        date <- statementDateAt "--date" "a statement of now" now opts.date
        selected <- addressSelection cfg portfolios (TagFilter opts.kept.keptTxTags opts.kept.keptUntaggedTxs)
        counted <- statementAt cfg portfolios (\zoneAt -> balanceStatement scope zoneAt date)
        priced <- liftEither $ maybe Right valued valuation (maybe id holdingsSelected selected counted)
        hashes <- blockHashesAt cfg (lastBlocksUnnamed priced)
        let statement = (namingLastBlocks hashes priced) {statementHolder = holderOf opts}
        return (statementReport now statement)
      AccountReport -> do
        (start, end) <- statementPeriodAt now opts.from opts.to
        selected <- txSelection cfg portfolios opts.kept
        counted <- statementAt cfg portfolios (\zoneAt -> accountStatement scope zoneAt start end)
        priced <- liftEither $ maybe Right accountsValued valuation (maybe id accountsSelected selected counted)
        hashes <- blockHashesAt cfg (accountBlocksUnnamed priced)
        let statement = (namingAccountBlocks hashes priced) {accountsHolder = holderOf opts}
        return (accountStatementReport now statement)
      PaymentReport named -> do
        txid <- walletTxHashOf named
        histories <- paymentHistories cfg portfolios named txid
        counted <- statementOf histories (\zoneAt -> paymentConfirmation scope zoneAt txid opts.addresses)
        priced <- liftEither $ maybe Right paymentValued valuation counted
        hashes <- blockHashesAt cfg (paymentBlocksUnnamed priced)
        let payment =
              (namingPaymentBlock hashes priced)
                { paymentHolder = holderOf opts,
                  paymentCounterparty = maybe "" Text.strip opts.counterparty,
                  paymentPurpose = maybe "" Text.strip opts.purpose
                }
        return (paymentReport now payment)
    liftEitherIO $ writeFileBytes opts.file (reportPdf now report)
    return $ ResponseFile opts.file

-- | What a report covers, as --wallet and --portfolio say it: every
-- wallet without them, the portfolios of one wallet with --wallet, and
-- one portfolio with --portfolio. The wallet of that portfolio is the
-- one --wallet names, or the wallet of the active portfolio without
-- --wallet. The options mean what they mean for exportwallet
-- (@portfoliosToExport@), so that a user learns them once.
--
-- A wallet or a portfolio the database does not have is refused.
reportScope :: Maybe Natural -> Maybe Natural -> ExceptT String (DB IO) StatementScope
reportScope walletM portfolioM = case (walletM, portfolioM) of
  (Nothing, Nothing) -> return EveryWallet
  (Just index, Nothing) -> OneWallet index <$ walletNamed index
  (_, Just index) -> do
    wallet <- walletNamedOrActive walletM
    OnePortfolio . portfolioRef <$> portfolioNamed wallet index

-- | Who holds the wallets, as --holder says it, without the spaces
-- around it, or nothing
holderOf :: ReportOptions -> Text
holderOf opts = maybe "" Text.strip opts.holder

-- | Refuse an option that does not go with the report. A balance
-- statement is for a day, an account statement for a period, and a
-- payment confirmation for a transaction, which has its own day. The
-- other side of a payment, its purpose and its addresses go with a
-- payment confirmation alone. The tags go with the two statements:
-- they keep an account statement to some transactions and a balance
-- statement to some addresses. --address-tag goes with an account
-- statement alone, since --tag is already about the tags of the
-- addresses in a balance statement, as it is in balances. An option of
-- another report would be left aside without a word, and the report
-- would not be the one the user asked for.
--
-- An account statement kept to some transactions takes no price. A
-- price values a closing balance, and such a statement states none
-- ("Haskoin.Wallet.Report.AccountStatement").
checkReportOptions :: ReportOptions -> Either String ()
checkReportOptions opts = case opts.kind of
  BalanceReport
    | isJust opts.from || isJust opts.to ->
        Left "--from and --to go with --transactions: give the day of a balance statement with --date"
    | ofPaymentOnly -> Left paymentOnly
    | not (null opts.kept.keptAddressTags) ->
        Left "--address-tag goes with --transactions: it keeps an account statement to the transactions at the addresses of a tag. --tag keeps a balance statement to the addresses that have a tag"
  AccountReport
    | isJust opts.date ->
        Left "--date goes with --balances: give the period of an account statement with --from and --to"
    | ofPaymentOnly -> Left paymentOnly
    | selects && priced ->
        Left "--price, --currency and --price-source go with a statement of every transaction: they value the closing balances, and a statement kept to some transactions with --tag, --address-tag or --untagged states none"
  PaymentReport _
    | isJust opts.date || isJust opts.from || isJust opts.to ->
        Left "--date, --from and --to go with --balances and --transactions: a payment confirmation is dated as its transaction is"
    | selects -> Left statementOnly
  _ -> Right ()
  where
    ofPaymentOnly = isJust opts.counterparty || isJust opts.purpose || not (null opts.addresses)
    paymentOnly = "--counterparty, --purpose and --address go with --payment: they say who a payment was with, what it was for and which address it paid"
    selects = isTxFiltered opts.kept
    statementOnly = "--tag, --address-tag and --untagged go with --balances and --transactions: they keep a statement to some of the addresses or to some of the transactions"
    priced = not (null opts.prices) || isJust opts.currency || isJust opts.priceSource

-- | The prices a report values the balances at, as its options give
-- them, or none. The prices and their currency go together: a price
-- without a currency says nothing, and neither does a currency without a
-- price. The source of the prices goes with them too.
valuationOf :: ReportOptions -> Either String (Maybe Valuation)
valuationOf opts = case (opts.prices, opts.currency, opts.priceSource) of
  ([], Nothing, Nothing) -> Right Nothing
  ([], Just _, _) -> Left "--currency goes with --price: give the price of a coin of each network, such as --price btc=87500"
  ([], Nothing, Just _) -> Left "--price-source goes with --price: give the price of a coin of each network, such as --price btc=87500"
  (_, Nothing, _) -> Left "Give the currency of the prices with --currency, such as --currency CHF"
  (prices, Just currency, source) -> Right (Just (Valuation currency prices (maybe "" Text.strip source)))

-- | The date a statement stops at: the end of the day an option gives,
-- or the moment the report is made. The option is named, and so is what
-- the report is without it, for the message. A day that is not over is
-- refused. Its end is still to come, and a statement for it would be
-- one of now under another name: without the option, the report says
-- the time it is of.
--
-- The end of a day comes with the zone the clock of this computer has
-- at that midnight, which the report states.
statementDateAt :: String -> String -> ZonedTime -> Maybe Day -> ExceptT String (DB IO) StatementDate
statementDateAt _ _ _ Nothing = return AtPreparation
statementDateAt option without now (Just day)
  | day < today = AtEndOf day <$> liftIO (zoneAtEndOf day)
  | otherwise =
      throwError $
        "The day of "
          <> option
          <> " must be over, and today is "
          <> cs (longDate today)
          <> ": give an earlier day, or leave "
          <> option
          <> " out for "
          <> without
  where
    today = localDay (zonedTimeToLocalTime now)

-- | The period of an account statement, as --from and --to give it:
-- where it starts and where it stops.
--
-- Without --to, the period runs up to the moment the report is made.
-- Without --from, it starts on the first day of the month of its last
-- day. So no option gives the month that is running, as a bank sends
-- it, and --to alone gives the month of that day.
--
-- A period that stops before it starts is refused, and so is one that
-- starts on a day still to come. The start comes with the zone the
-- clock of this computer has at the midnight that starts its first day,
-- which is the midnight that ends the day before.
statementPeriodAt :: ZonedTime -> Maybe Day -> Maybe Day -> ExceptT String (DB IO) (PeriodStart, StatementDate)
statementPeriodAt now fromM toM = do
  end <- statementDateAt "--to" "a statement up to now" now toM
  let lastDay = fromMaybe today toM
      firstDay = fromMaybe (firstOfMonth lastDay) fromM
  when (firstDay > lastDay) . throwError $ case toM of
    Just _ -> "The period starts on " <> cs (longDate firstDay) <> " and stops on " <> cs (longDate lastDay) <> ": give a day of --from that is not after the day of --to"
    Nothing -> "The day of --from is still to come, and today is " <> cs (longDate today) <> ": give today or an earlier day"
  startZone <- liftIO (zoneAtEndOf (pred firstDay))
  return (PeriodStart firstDay startZone, end)
  where
    today = localDay (zonedTimeToLocalTime now)
    firstOfMonth day = let (year, month, _) = toGregorian day in fromGregorian year month 1

-- | The zone the clock of this computer has at the midnight that ends a
-- day. The zone of a moment is asked of the computer in UTC, and the
-- midnight is known on the local clock. So the moment is first worked
-- out as if the clock were on UTC, which is off by the offset of the
-- zone at most, and then again with the zone found there. The two agree
-- unless the clock changes within those hours, and the second is then
-- the right one.
zoneAtEndOf :: Day -> IO TimeZone
zoneAtEndOf day = do
  nearby <- getTimeZone (localTimeToUTC utc midnightAfter)
  getTimeZone (localTimeToUTC nearby midnightAfter)
  where
    midnightAfter = LocalTime (addDays 1 day) midnight

-- | A statement of the portfolios, as the database has them now, on the
-- networks hw shows. The statement is worked out by the function given,
-- from the zone of the clock at each time, the block the last sync of
-- each network reached, and the history of each portfolio: that of the
-- balance statement or that of the account statement
-- ("Haskoin.Wallet.Report.Statement").
--
-- The transactions of every portfolio are read whole, in the
-- transaction the caller runs ('wholeHistory'), so that the two sides of
-- a payment between portfolios agree. The best block of each network is
-- the one its last sync left. The days are read on the clock of this
-- computer, each with the zone the computer had on that day.
statementAt :: Config -> [Portfolio] -> ((UTCTime -> TimeZone) -> [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> Either String statement) -> ExceptT String (DB IO) statement
statementAt cfg portfolios worked = do
  histories <- forM portfolios $ \portfolio -> do
    pools <- listedPools cfg portfolio Nothing
    historyOf portfolio <$> forM pools (\pool -> (pool.poolNetwork,) <$> wholeHistory pool)
  statementOf histories worked

-- | A portfolio as a report reads it, with the transactions given for
-- each network
historyOf :: Portfolio -> [(Network, [ListedTx])] -> PortfolioHistory
historyOf portfolio = PortfolioHistory (portfolioRef portfolio) (dBWalletDescription portfolio.portfolioWallet) (dBPortfolioDescription portfolio.portfolioRow)

-- | What a report is worked out from, given the histories of the
-- portfolios: the function given gets the zone of the clock at each
-- time, the block the last sync of each network of the histories
-- reached, and the histories.
statementOf :: [PortfolioHistory] -> ((UTCTime -> TimeZone) -> [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> Either String statement) -> ExceptT String (DB IO) statement
statementOf histories worked = do
  -- The networks of the histories, each once, in the order they come.
  -- In a statement every portfolio has the same ones.
  let shown = nub (concatMap (map fst . historyPools) histories)
  networks <- forM shown $ \net -> do
    best <- getBest net
    return (net, (\block -> SyncedTo block.blockHeight block.blockHash (posixSecondsToUTCTime . fromIntegral <$> block.blockTime)) <$> best)
  zones <- liftIO $ Map.fromList <$> mapM (\time -> (time,) <$> getTimeZone time) (statementTimes networks histories)
  liftEither $ worked (\time -> Map.findWithDefault utc time zones) networks histories

-- | The portfolios as a payment confirmation reads them: each with the
-- one transaction named, as the portfolio sees it, on the networks hw
-- shows. A portfolio that has no part in the transaction has no
-- transaction here. Every portfolio is still there, since the
-- confirmation tells from all of them whether its side is the whole
-- database ("Haskoin.Wallet.Report.PaymentConfirmation").
--
-- The transaction is read alone, as show reads it ('transactionViews'),
-- and not with the whole history, which a statement needs and a
-- confirmation does not. A transaction the wallet does not have is
-- refused, and so is one on hidden networks alone.
paymentHistories :: Config -> [Portfolio] -> HashRef -> TxHash -> ExceptT String (DB IO) [PortfolioHistory]
paymentHistories cfg portfolios named txid = do
  views <- shownViews cfg named =<< transactionViews txid
  return
    [ historyOf portfolio [(pool.poolNetwork, [ListedTx info Nothing]) | (pool, place, info) <- views, placePortfolio place == portfolioRef portfolio]
      | portfolio <- portfolios
    ]

-- | The hashes of the blocks given, each on its network at its height,
-- as the server has them: those a statement names and the database does
-- not have. The server lists the blocks it knows at a height, and the
-- one of its main chain is taken.
--
-- A server that does not answer, or that has no such block, gives no
-- hash for it, and the statement says so. A report is of the balances
-- and of the transactions, which need no server: a hash that cannot be
-- had today must not hold it back.
blockHashesAt :: Config -> [(Network, BlockHeight)] -> ExceptT String (DB IO) [(Network, BlockHeight, BlockHash)]
blockHashesAt cfg blocks = do
  storeOn <- liftIO $ webStores cfg
  hashes <- forM blocks $ \(net, height) -> do
    answer <- liftIO $ runExceptT (storeBlocksAtHeights (storeOn net) [height])
    return [(net, height, headerHash block.header) | Right blocks' <- [answer], block <- take 1 (filter (.main) blocks')]
  return (concat hashes)

{- The journal hw keeps -}

-- When the key journal of the configuration is on, hw keeps the journal
-- of exportjournal in a file next to the database ('journalFile'), and
-- writes it again after each command that changes it. The user includes
-- that file in a journal of their own, once, and the reports of ledger
-- are then up to date with no export to run. Run by hand, the export is
-- the step a user forgets, and a journal that is behind says nothing of
-- being behind.
--
-- The file is hw's own, as its automatic backups are. hw never writes
-- over a file the user names, and this one the user does not name: hw
-- replaces it without asking, and removes it when the key is turned off.
--
-- Two rules keep the file. After a command that may change the journal,
-- hw writes it again (@journalAfter@). Before any command, hw writes it
-- when it is not there, or when it was written for other accounts of
-- tags than the configuration gives now ('journalBeforeCommand').

-- | Whether a command may change the journal hw keeps: the live
-- transactions of the chain, their labels and their tags, the labels of
-- the addresses and of the contacts that name an entry, the tags of the
-- contacts, which may file a payment under an account, the wallets and
-- the portfolios, and the networks hw shows.
--
-- The accounts the configuration gives to tags change the journal too.
-- No command writes them: the user does, by hand, and the journal
-- follows before the next command ('journalBeforeCommand').
--
-- Every command is named, with no case for the rest, so that the
-- compiler asks about each new command whether it changes the journal.
-- A command wrongly left out would leave the journal behind until the
-- next sync. A command wrongly counted costs one reading of the
-- transactions, since a journal that did not change is not written
-- ('keepJournal'). So a command in doubt is counted.
--
-- The transactions in preparation are not in the journal. So the
-- commands that prepare, sign, send, import or delete one change nothing
-- of it: the transaction reaches the journal with the sync that finds it
-- on the chain, with its label and its tags.
changesJournal :: Command -> Bool
changesJournal = \case
  -- What the chain says: new transactions, confirmations, transactions
  -- replaced or dropped, and the blocks of a reorganisation. recover
  -- syncs the portfolios it finds.
  CommandSync _ _ -> True
  CommandRecover _ -> True
  -- The labels and the tags of transactions, the labels of addresses
  -- and the contacts, which name the entries. A label or a tag of
  -- something the journal does not show changes nothing of it, and the
  -- command cannot tell.
  CommandLabel _ -> True
  CommandTag _ -> True
  CommandUntag _ -> True
  CommandRenameTag _ -> True
  CommandDeleteTag _ _ -> True
  CommandNewContact _ -> True
  CommandDeleteContact _ _ _ -> True
  -- Labels, tags and contacts of a file, and portfolios
  CommandImportWallet _ _ -> True
  -- Wallets and portfolios gone, or all replaced by those of a copy
  CommandDeleteWallet _ _ -> True
  CommandDeletePortfolio _ _ -> True
  CommandRestoreDatabase _ _ -> True
  -- The networks hw shows, which are those of the journal, and the key
  -- that turns the journal on
  CommandSetDefault opts -> isJust opts.journal || not (null opts.showNetworks && null opts.hideNetworks)
  -- A new portfolio has no transaction, and a new address was never paid
  CommandNewPortfolio _ -> False
  CommandReceive _ -> False
  -- A transaction in preparation, which the journal leaves out
  CommandPrepareTx _ -> False
  CommandPrepareSendAll _ -> False
  CommandPrepareSweep _ -> False
  CommandBumpFee _ -> False
  CommandSignTx _ -> False
  CommandSignSweep {} -> False
  CommandSend _ -> False
  CommandSendAll _ -> False
  CommandSweep _ -> False
  CommandBroadcast {} -> False
  CommandImportTx _ -> False
  CommandDeleteTx _ _ -> False
  -- A coin locked or unlocked, which no entry shows
  CommandLockCoin _ _ -> False
  CommandUnlockCoin _ _ -> False
  -- The commands that change nothing
  CommandCheckMnemonic -> False
  CommandExportWallet {} -> False
  CommandPortfolios _ -> False
  CommandSwitch _ _ -> False
  CommandShowDefault _ -> False
  CommandShow {} -> False
  CommandAddresses {} -> False
  CommandBalances {} -> False
  CommandTxs {} -> False
  CommandPendingTxs {} -> False
  CommandCoins {} -> False
  CommandTags -> False
  CommandContacts {} -> False
  CommandExportTx _ _ -> False
  CommandFees _ -> False
  CommandSignMsg _ -> False
  CommandVerifyMsg _ -> False
  CommandBackup _ -> False
  CommandShowBackups -> False
  CommandExportSheet _ _ -> False
  CommandExportJournal _ _ -> False
  CommandReport _ -> False

-- | Bring the journal hw keeps up to date after a command that
-- succeeded, when the command may have changed it ('changesJournal') and
-- the configuration asks for a journal.
--
-- setdefault changes the configuration file the command was started
-- with, so the configuration is read again after it. It may have turned
-- the journal on, which is then written at once. It may have shown or
-- hidden a network, which the journal must follow. It may have turned
-- the journal off, and the file is then removed ('dropJournal').
journalAfter :: Config -> Command -> IO ()
journalAfter cfg cmd
  | not (changesJournal cmd) = return ()
  | otherwise = case cmd of
      CommandSetDefault opts
        | opts.journal == Just False -> dropJournal cfg
        | otherwise -> do
            reread <- configAsItIs (configDataDirectory cfg)
            forM_ reread $ \now -> when (configJournal now) (keepJournal now)
      _ -> when (configJournal cfg) (keepJournal cfg)

-- | Write the journal of every portfolio to the file hw keeps next to
-- the database ('journalFile'), in place of the one there. It runs once
-- the command is done, in a transaction of its own, so that the journal
-- holds the wallet as the command left it.
--
-- A journal that says what the file already says is not written. The
-- text of a journal depends on the history alone
-- ("Haskoin.Wallet.Cli.Journal"), so the file changes only when the
-- history does, and keeps the time of its last real change.
--
-- The new file takes the place of the old one in one step
-- ('replaceFileBytes'). ledger may read the journal while hw writes it,
-- and finds the old file or the new one, never a part of one.
--
-- A database without a wallet still has a journal, which holds its
-- first comment alone. The file is included in a journal of the user,
-- and ledger would refuse a file that is missing.
--
-- A journal that cannot be written is told on stderr, and stops nothing:
-- the command is done ('afterCommand'). The journal is then behind until
-- the next command that changes it, so the warning gives the command
-- that writes it now.
keepJournal :: Config -> IO ()
keepJournal cfg =
  afterCommand
    ("The journal " <> file <> " was not brought up to date.")
    "Another hw command took the database before the journal was written"
    "To write the journal now, run: hw setdefault --journal"
    cfg
    (writeJournal cfg)
  where
    file = journalFile cfg

-- | Write the journal of every portfolio to the file hw keeps, unless
-- the file says the same already, in the transaction the caller runs.
-- 'keepJournal' says why the file is compared first and how it is
-- replaced.
writeJournal :: Config -> ExceptT String (DB IO) ()
writeJournal cfg = do
  portfolios <- lift getAllPortfolios
  journal <- journalNow cfg Nothing portfolios
  current <- liftIO (readFileBytes file)
  unless (current == Right journal) $
    liftEitherIO (replaceFileBytes file journal)
  where
    file = journalFile cfg

-- | Write the journal hw keeps, before a command, when the
-- configuration asks for one and the file is missing, or was written for
-- other accounts of tags than the configuration gives now. hw runs it
-- before every command, once the database is ready.
--
-- The journal is otherwise written after the commands that change it
-- (@journalAfter@). That leaves two gaps, both made by a change no
-- command sees.
--
-- The first is a file that is not there. A user who sets the key journal
-- in the configuration file by hand, or who answers yes in the setup,
-- has no file until such a command runs. A user who deleted the file
-- has none either. Meanwhile the journal that includes the file does
-- not read: ledger refuses a file that is missing.
--
-- The second is the table journal-accounts, which the user writes by
-- hand. The journal would keep its payments in the old accounts until
-- the next sync, and the user who just wrote a line would see nothing
-- change in ledger. The journal says in its first comment which accounts
-- it was written for ('journalHeader'). hw reads that comment, and
-- writes the journal again when the configuration would write another
-- (@journalWrittenFor@).
--
-- So any command brings the file up to date in these two cases, a
-- listing too.
--
-- The check costs one look at the file system and the first bytes of
-- the file per command. A file with the right first comment is left to
-- @journalAfter@, which knows which commands change it, so a command
-- that changes nothing still reads no transaction. A file hw cannot read
-- is left to it as well: hw cannot tell what it holds, and @journalAfter@
-- says so when it fails to replace it.
--
-- It runs before the command and not after it. Every command passes
-- here once, while a listing on the screen and a command that fails
-- end in other places. A command that changes the journal then writes
-- it a second time, which costs one more reading of the transactions,
-- once.
--
-- A journal that cannot be written is told on stderr, and the command
-- still runs (@besideCommand@).
journalBeforeCommand :: Config -> IO ()
journalBeforeCommand cfg = when (configJournal cfg) $ do
  exists <- doesPathExist file
  upToDate <- if exists then journalWrittenFor cfg else return False
  unless upToDate $
    besideCommand
      ( if exists
          then "The journal " <> file <> " was written for other accounts than the configuration file gives to tags now, and was not written again."
          else "The journal " <> file <> " is missing, and was not written."
      )
      "Another hw command holds the database"
      "The command runs without it. To write the journal, run: hw setdefault --journal"
      cfg
      (writeJournal cfg)
  where
    file = journalFile cfg

-- | Whether the journal hw keeps was written for the accounts the
-- configuration gives to tags now: whether the file starts with the
-- first comment a journal of this configuration has ('journalHeader').
-- After that comment comes an empty line, or the end of the file in a
-- database without a wallet. A longer comment is that of another table,
-- one with more tags.
--
-- Only the start of the file is read, which a long history does not
-- make longer. A file that cannot be read counts as written for them:
-- 'journalBeforeCommand' says why.
journalWrittenFor :: Config -> IO Bool
journalWrittenFor cfg = do
  start <- readFileStart (BS.length header + 1) (journalFile cfg)
  return $ case start of
    Left _ -> True
    Right bytes -> bytes == header || bytes == header <> "\n"
  where
    header = encodeUtf8 (Text.unlines (journalHeader (configJournalAccounts cfg)))

-- | Remove the journal hw kept next to the database, once setdefault
-- --no-journal turned it off. A journal left there would still be read
-- by the journal that includes it, and would fall behind the wallet
-- without a word. With the file gone, ledger says that it is missing.
-- The file holds nothing the user wrote, and exportjournal writes it
-- again. A file that is not there is no failure. One that cannot be
-- removed is told on stderr.
dropJournal :: Config -> IO ()
dropJournal cfg = do
  removed <- tryAny $ do
    exists <- doesPathExist file
    when exists (removeFile file)
  case removed of
    Left failure -> warn ("The journal " <> file <> " could not be removed. " <> displayException failure <> ". Remove it by hand.")
    Right () -> return ()
  where
    file = journalFile cfg

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
        listingWhole = True,
        listingShownEmpty = False
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
-- --type pick in the active portfolio ('chooseAccount'), with the label
-- and the tags given for it
cmdReceive :: Config -> Receive -> IO Response
cmdReceive cfg opts =
  runDBResponse cfg $ do
    portfolio <- requireActivePortfolio
    (accId, _) <- chooseAccount cfg portfolio opts.target.network opts.target.kind
    addr <- genExtAddress cfg accId (fromMaybe "" opts.label)
    lift $ tagAddress addr opts.tags
    addressAnswer accId addr

-- | Show what the word names, as its shape tells ('Shown'), or without a
-- word the newest receiving address of the account. Nothing is generated
-- or stored.
--
-- A path and the newest address are of the account the options pick in
-- the active portfolio, as for receive. A path that starts with its type,
-- as the views write it (bip84\/0\/3), names its account itself. An address
-- and a coin tell where they are too, so --type is refused with them, and
-- with such a path, rather than ignored: the user who typed it may have
-- meant another thing to show. --network then only tells apart an address
-- or a coin the wallet has on several networks, as the test networks share
-- their keys. A transaction is shown as each
-- portfolio that holds it sees it, whichever is active. A file of a
-- wallet is read as importwallet reads it, with --wallet and --portfolio,
-- which name one wallet and one portfolio of the file, and go with no
-- other word. A database file takes no option: show lists every wallet
-- and every portfolio of it (@shownDatabaseFile@).
cmdShow :: Config -> Target -> ImportScope -> Maybe Shown -> IO Response
cmdShow cfg target fileScope shownM =
  runDBResponse cfg $ do
    when (fileScope /= wholeFile && not (namesFile shownM)) $ throwError walletFileScopeOnly
    case shownM of
      Just (ShownFile file) -> shownFile cfg target fileScope file
      Nothing -> newestReceiving cfg target
      Just (ShownPath Nothing addrType index) -> addressAtPath cfg target addrType index
      Just (ShownPath (Just kind) addrType index) -> do
        refuseType "a path that starts with its type"
        addressAtPath cfg (Target target.network (Just kind)) addrType index
      Just (ShownAddress typed) -> do
        refuseType "an address"
        named <- namedAddress (notHeldToShow cfg typed) cfg target.network typed
        case named of
          OwnAddress accId addr -> addressAnswer accId addr
          BookAddress net contact -> return (ResponseContact net contact)
      Just (ShownTx named) -> do
        refuseType "a transaction"
        shownTxs cfg target named
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
addressAtPath :: Config -> Target -> AddrType -> KeyIndex -> ExceptT String (DB IO) Response
addressAtPath cfg target addrType index = do
  portfolio <- requireActivePortfolio
  (accId, acc) <- chooseAccount cfg portfolio target.network target.kind
  let path = addrDeriv addrType :/ index
  addr <- derivedAddressAt acc addrType index
  -- The row is looked up by the address derived, not by its path, so that
  -- the answer shows what the key derives in either case
  held <- lift $ storedAmong [addr] (accId, acc)
  case held of
    row : _ -> addressAnswer accId row
    [] -> ResponseDerivedAddr (accountRef portfolio acc) path <$> liftMaybe "An account derives no address of this kind" (cutAddress (accountNetwork acc) addr)

-- | A file: a database file, a transaction file or a file of a wallet,
-- as its bytes tell. The wallet keeps nothing of any of them.
--
-- A database is told first, by the bytes every SQLite database starts
-- with ('startsDatabase'), read alone: a database may be large, and show
-- opens it rather than read it whole. Any other file is read whole, and
-- is a transaction file when its bytes are in one of its forms
-- ('txFileForm'), or else a file of a wallet. The three kinds cannot be
-- taken for each other: the name of a file tells nothing, so a copy of
-- the database and the file of its labels are told apart even when the
-- user swapped their names.
--
-- show takes for a file any word of no other shape ('readShown'). A word
-- with no slash that names no file may have been meant as something else,
-- a short id with a digit missing for instance. The answer then names the
-- five kinds show takes. A word with a slash is a file path, and gets the
-- answer of any command that reads a missing file.
shownFile :: Config -> Target -> ImportScope -> FilePath -> ExceptT String (DB IO) Response
shownFile cfg target fileScope file = do
  exists <- liftIO $ doesPathExist file
  unless (exists || '/' `elem` file) $
    throwError $
      file
        <> " is not a path, an address, a transaction or a coin, and no file has that name: show takes a path such as /0/3,"
        <> " an address of the wallet, a txid or a nosigHash, a coin TXID:INDEX, or a file of a transaction, of a wallet or of a database,"
        <> " each hash in full or by its short id, its first 8 digits"
  when (isJust target.kind) $ throwError "show takes no --type with a file"
  start <- liftEitherIO $ readFileStart databaseHeaderLength file
  if startsDatabase start
    then shownDatabaseFile target fileScope file
    else do
      bytes <- liftEitherIO $ readFileBytes file
      case txFileForm bytes of
        Just _ -> do
          when (fileScope /= wholeFile) $ throwError walletFileScopeOnly
          shownTxFile cfg target file bytes
        Nothing -> shownWalletFile cfg target fileScope file bytes

-- | A database file: its wallets and their portfolios, read from the file
-- without changing it ("Haskoin.Wallet.DatabaseFile"). It is what the
-- user looks at before putting a copy of the database in place of the
-- database. The file is read through a connection of its own, apart from
-- the one of the command, which stays on the database of hw.
--
-- No option narrows the view. A database holds few wallets and
-- portfolios, and the user wants to see all that comes back. So --network,
-- --wallet and --portfolio are refused rather than ignored, as show
-- refuses any option that does not go with its word. Each refusal says
-- that the file is an SQLite database: --wallet and --portfolio go with a
-- file of a wallet, and the user may have taken this file for one.
shownDatabaseFile :: Target -> ImportScope -> FilePath -> ExceptT String (DB IO) Response
shownDatabaseFile target fileScope file = do
  when (isJust target.network) $
    throwError (noOption "--network" <> ": its wallets and their portfolios are on every network")
  when (fileScope /= wholeFile) $
    throwError (noOption "--wallet and no --portfolio" <> ". They name one wallet and one portfolio of a BIP 329 file of a wallet. Every wallet and every portfolio of a database is shown")
  ResponseDatabaseFile file <$> liftEitherIO (databaseShown file)
  where
    noOption options = "show takes no " <> options <> " with " <> file <> ", which is an SQLite database"

-- | The transaction of a file, as the portfolio it involves sees it. One
-- that involves no portfolio is shown as outside the wallet, on the
-- network of --network or of the configuration when the file names none.
-- The labels shown are those the file carries, not those of this wallet,
-- since the file is what an offline computer has. hw finds the portfolio
-- ('transactionPool'): a transfer between two portfolios is shown as the
-- one that pays sees it. An address it pays that another portfolio of the
-- database holds is named so, and so is one of the address book
-- ('fillAddresses'): no file carries that.
shownTxFile :: Config -> Target -> FilePath -> BS.ByteString -> ExceptT String (DB IO) Response
shownTxFile cfg target file bytes = do
  TxFileRead txFile@(TxFile netM tsd _ _) _ <- txFileOfBytes (commandNetwork cfg target.network) file bytes
  named <- fileNetwork target.network netM
  activeM <- lift activePortfolioId
  (poolM, completed) <- transactionPool cfg activeM named tsd
  case poolM of
    Just pool -> do
      keys <- lift $ poolKeys pool
      info <- liftEither (parseTxFile pool.poolNetwork keys txFile {txFileData = completed})
      txAnswer pool =<< lift (fillAddresses pool info)
    Nothing -> do
      let net = commandNetwork cfg named
      ResponseOutsideTx net <$> (lift . fillContacts net =<< liftEither (parseOutsideTxSignData net tsd))

-- | A file of a wallet, as importwallet reads it: each wallet and each
-- portfolio as importwallet would leave it, with the labels and locks of
-- its accounts ("Haskoin.Wallet.Labels"). The file is refused when it
-- holds nothing a file of a wallet holds. It is no database and no
-- transaction file either, so the refusal names the three kinds.
--
-- The accounts of the file on the networks hw hides are left out, with
-- their labels and locks, as every answer leaves those networks out.
-- importwallet still imports them: the file is the whole wallet. An
-- account importwallet would leave aside is shown whatever its network,
-- since that is a problem of the file to tell.
shownWalletFile :: Config -> Target -> ImportScope -> FilePath -> BS.ByteString -> ExceptT String (DB IO) Response
shownWalletFile cfg target fileScope file bytes = do
  when (isJust target.network) $
    throwError "show takes no --network with a file of a wallet: each of its records names its network"
  content <- either (const (throwError neither)) return (decodeUtf8' bytes)
  let walletFile = readWalletFile content
  unless (holdsAnything walletFile) $ throwError neither
  shown <- fileShown fileScope walletFile
  return $ ResponseWalletFile file shown {shownAccounts = filter (onShownNetwork . fst) (shownAccounts shown)}
  where
    onShownNetwork = \case
      AccountAdded ref -> isShown cfg (accountRefNetwork ref)
      AccountHeld ref -> isShown cfg (accountRefNetwork ref)
      AccountLeftAside _ _ -> True
    neither = couldNotRead file "Neither a transaction file, a PSBT in bytes or in base64 text or the hexadecimal of a transaction, nor a file of a wallet that importwallet reads, nor an SQLite database"

-- | A transaction of the wallet, pending or on the chain, as each
-- portfolio that holds it sees it, on the networks hw shows, or on the
-- --network when given. The short id is matched among the transactions of
-- every portfolio, as label matches it.
shownTxs :: Config -> Target -> HashRef -> ExceptT String (DB IO) Response
shownTxs cfg target named = do
  hash <- walletTxHashOf named
  views <- shownViews cfg named =<< transactionViews hash
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
-- portfolio that holds it: those on the networks hw shows (@shownAmong@)
shownViews :: Config -> HashRef -> [(Pool, Place, TxInfo)] -> ExceptT String (DB IO) [(Pool, Place, TxInfo)]
shownViews cfg named = shownAmong cfg ("The transaction " <> hashRefText named) (\(pool, _, _) -> pool.poolNetwork)

-- | The addresses of a chain of a portfolio, a section for each of its
-- accounts on the networks hw shows, or for those --network and --type
-- keep, in the order of the networks and of the types. In each section
-- the newest come first, or the first address with --reverse, each with
-- what hw knows of its funds: the totals of its coins, what they are
-- worth and what it can spend of them now, or that the portfolio was
-- never synced on the network (@fundsAt@). An address names the one to go on after: the listing starts in the section
-- of its account. An address the test networks share is in the section of
-- each, and is refused without --network ('sectionOfAddress').
--
-- With a filter of tags, the listing keeps the addresses that have any
-- one of the tags, and those without a tag when asked for
-- ('keptAddressesAfter'). Each address comes with its tags, all of them,
-- kept or not.
addressesListing :: Config -> Portfolio -> Target -> Bool -> TagFilter -> Paging Text -> ExceptT String (DB IO) Sections
addressesListing cfg portfolio target internal kept paging = do
  _ <- requireTags kept.keptTags
  accounts <- listedAccounts cfg portfolio target.network target.kind
  -- An account without an address shows nothing, synced or not: its
  -- addresses say themselves that they are not synced
  synced <- forM accounts $ \(_, acc) -> (`SectionSync` False) <$> lift (poolEverSynced (accountPool acc))
  let addrType = if internal then AddrInternal else AddrExternal
      listed = zip accounts synced
      section startM ((accId, acc), state) =
        let net = accountNetwork acc
         in Listing
              { listingStart = startM,
                listingRows = \afterM countM -> do
                  addrs <- lift $ keptAddressesAfter (listedOrder paging) accId addrType kept afterM countM
                  totals <- coinTotalsAtAddresses net accId (dBAddressAddress <$> addrs)
                  tagsOf <- lift $ tagsOfAddressRows addrs
                  return [(addr, fundsAt state.sectionEverSynced totals addr, tagsOf addr) | addr <- addrs],
                listingCursor = \(addr, _, _) -> Right (dBAddressIndex addr),
                listingAnswer = grouped state ResponseAddresses (groupOf portfolio net [(accId, acc)] state),
                listingWhole = False,
                listingShownEmpty = False
              }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (listed, Nothing)
    Just text -> do
      tried <- forM accounts $ \(accId, acc) -> lift (runExceptT (listedAddress (accountNetwork acc) accId addrType text))
      found <- liftEither $ sectionOfAddress text (zip (accountNetwork . snd <$> accounts) (either (const Nothing) Just <$> tried))
      case (found, tried) of
        (Just (i, row), _) -> return (drop i listed, Just (dBAddressIndex row))
        -- The one account of the listing says why the address is not its own
        (Nothing, [Left refusal]) -> throwError refusal
        (Nothing, _) -> throwError ("The address " <> cs text <> " is not among the addresses listed")
  return $ Sections (length accounts > 1) paging.pagingMax (zipWith section (startM : repeat Nothing) from)

-- | The addresses of a portfolio that hold coins, receiving and change
-- addresses alike, each with the balance of its coins, a section for each
-- network hw shows, or for the one --network keeps. --type keeps the
-- addresses of one type. Each section comes with the total of its
-- network, and lists the largest balance first, or the smallest with
-- --reverse. An address names the one to go on after, with the balance it
-- holds now: the listing starts in the section of its network. An address
-- the test networks share may hold coins on several, and is then refused
-- without --network ('sectionOfAddress').
--
-- With a filter of tags, the listing keeps the addresses that have any
-- one of the tags, and those without a tag when asked for. The total of
-- each network adds up those addresses alone: what the portfolio holds
-- under the tags. So balances --tag savings answers how much the savings
-- hold, and balances --untagged how much is in no group.
--
-- The section of a network where the portfolio never synced says so
-- (the type @SectionSync@).
balancesListing :: Config -> Portfolio -> Target -> TagFilter -> Paging Text -> ExceptT String (DB IO) Sections
balancesListing cfg portfolio target kept paging = do
  _ <- requireTags kept.keptTags
  accounts <- listedAccounts cfg portfolio target.network target.kind
  let networks = byNetwork accounts
  sections <- forM networks $ \(net, onNet) -> do
    let accIds = fst <$> onNet
    -- The total of the network, over every address the section covers
    total <- foldMap snd <$> coinBalancesAfter UsualOrder net accIds kept Nothing Nothing
    state <- sectionSync portfolio net onNet
    return $ \startM ->
      Listing
        { listingStart = startM,
          listingRows = \afterM countM -> do
            rows <- coinBalancesAfter (listedOrder paging) net accIds kept afterM countM
            tagsOf <- lift $ tagsOfAddressRows (fst <$> rows)
            return [(addr, balance, tagsOf addr) | (addr, balance) <- rows],
          listingCursor = \(addr, balance, _) -> Right (addr, balance),
          listingAnswer = grouped state ResponseBalances (\rows -> (groupOf portfolio net onNet state rows, total)),
          listingWhole = False,
          listingShownEmpty = state.sectionShownNotSynced
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

-- | Label what the word before the label names: a receiving address, the
-- receiving address at a path of the active portfolio, or a transaction
-- ('labelTransaction'). With the label alone, describe the active
-- portfolio, or its wallet with --of-wallet (@describeActive@).
--
-- An address that is none of the wallet is looked for in the address
-- book, and its contact is labelled (@namedAddress@).
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
cmdLabel :: Config -> Label -> IO Response
cmdLabel cfg opts =
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
            named <- namedAddress (notHeldOrKept cfg typed) cfg opts.target.network typed
            case named of
              OwnAddress accId row -> addressAnswer accId =<< labelAddress row opts.label
              -- The label of a contact says whose address it is
              BookAddress net contact -> do
                lift $ setContactLabel net (contactAddress contact) opts.label
                contactAnswer net (contactAddress contact)
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
            _ <- shownViews cfg named =<< transactionViews hash
            views <- labelTransaction hash opts.label
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

{- Tags -}

-- | Put tags on what the first word names, besides the tags it has: an
-- address of the wallet, the address at a path of the active portfolio,
-- or a transaction, as label finds each ('readLabelled'). Then give each
-- tag the colour of --color, if any. With no tag after the first word,
-- that word is a tag, and the command only sets it up: its colour, and
-- whether it locks its coins (@setUpTag@). --lock-coins and
-- --unlock-coins go with a tag alone. Beside something to tag they
-- would read as locking the coins of that address, which they do not:
-- the tag locks the coins of every address that has it.
-- With --with-change, the change down the chain of the payments of an
-- address takes the tags too (@retagNamed@).
--
-- An address and a transaction tell where they are, so they are tagged in
-- their portfolio, whichever is active. A change address takes tags, at
-- its path or by itself, though it takes no label: a tag says which
-- group the coins of an address belong to, and the change of a payment
-- belongs to one as much as what was received
-- ("Haskoin.Wallet.Database.Tag"). A transaction is tagged in every
-- portfolio that holds it ('retagTransaction'). An option that would
-- change nothing is refused rather than ignored, as label refuses it.
--
-- A tag that locks its coins may come to lock a coin that a pending
-- transaction spends: the user is told (@tellingStopped@).
cmdTag :: Config -> TagOptions -> IO Response
cmdTag cfg opts =
  runDBResponse cfg . tellingStopped $
    case opts.tags of
      [] -> setUpTag opts
      tag : _ | opts.lockCoins || opts.unlockCoins -> do
        let (option, forOneCoin) =
              if opts.lockCoins
                then ("--lock-coins", "To lock one coin, use lockcoin.")
                else ("--unlock-coins", "To unlock one coin, use unlockcoin.")
            word = cs (tagText tag)
        throwError $
          option
            <> " goes with a tag alone, and is about the coins at every address that has the tag: hw tag "
            <> word
            <> " "
            <> option
            <> ". "
            <> forOneCoin
      tags -> do
        -- The colours first, so that the answer shows the tags in them.
        -- A refusal of what to tag undoes them with the rest.
        for_ opts.color $ \colorM -> lift (forM_ tags (`setTagColor` colorM))
        retagNamed cfg AddTags opts.first tags opts.withChange opts.target

-- | Set up the tag typed alone, and answer with the tag as tags lists
-- it: give it a colour or remove its colour (--color), make it lock its
-- coins (--lock-coins) or stop it (--unlock-coins). The tag is made if
-- the database had none of that name: a tag can be coloured before it
-- is used, and can lock its coins before it has any, so that the first
-- one it receives is locked. --unlock-coins makes no tag: there is
-- nothing to unlock, and the word was most likely mistyped.
--
-- A tag that locks its coins already is locked again without a word,
-- and so for one unlocked twice. The answer shows what the tag does
-- now, and a script can run the command without asking first.
--
-- Without an option there is nothing to do, and the user most likely left
-- out the tags of an address or of a transaction, so the refusal says how
-- to give them. A word that has the shape of something tag tags, such as
-- a short id, and that is no tag of the database yet, is refused too, for
-- the same reason: tag would otherwise make a tag out of the id of a
-- transaction. A tag of that shape that exists is coloured as any other.
-- --network and --type name nothing here, and are refused.
setUpTag :: TagOptions -> ExceptT String (DB IO) Response
setUpTag opts = do
  let typed = opts.first
      forgotten = "To tag it, give the tags after it: hw tag " <> typed <> " TAG"
  when (opts.lockCoins && opts.unlockCoins) $
    throwError "--lock-coins and --unlock-coins say opposite things: give one of them"
  when (isNothing opts.color && not (opts.lockCoins || opts.unlockCoins)) $
    throwError $ case labelledShapeOf typed of
      Just shape -> typed <> " reads as " <> shape <> ". " <> forgotten
      Nothing -> "tag needs what to tag, then the tags: hw tag ADDRESS|TXID|PATH TAG. To colour the tag " <> typed <> ", add --color COLOR"
  when (isJust opts.target.network) $
    throwError "tag takes no --network with a tag alone: a tag is the same in the whole database"
  when (isJust opts.target.kind) $
    throwError "tag takes no --type with a tag alone: a tag is the same in the whole database"
  when opts.withChange $
    throwError "tag takes no --with-change with a tag alone: nothing is tagged"
  tag <- case readTag (cs typed) of
    Right tag -> return tag
    Left refusal -> throwError $ case labelledShapeOf typed of
      Just shape -> typed <> " reads as " <> shape <> ", not as a tag. " <> forgotten
      Nothing -> refusal
  heldM <- lift $ heldTag tag
  for_ (labelledShapeOf typed) $ \shape ->
    when (isNothing heldM) $
      throwError (typed <> " reads as " <> shape <> ", and is no tag yet. " <> forgotten)
  when (opts.unlockCoins && isNothing heldM) $ throwError (noSuchTag tag)
  for_ opts.color $ lift . setTagColor tag
  when (opts.lockCoins || opts.unlockCoins) $ lift (setTagLocksCoins tag opts.lockCoins)
  ResponseTags . filter ((== tag) . shownTag . (.tagUseTag)) <$> lift tagUses

-- | Take tags off what the first word names, which is found as tag finds
-- it. A tag it does not have is refused, and nothing is changed: the
-- command fails, which undoes the tags it took off before. With
-- --with-change, a tag the address does not have still comes off the
-- change addresses down its chain (@retagNamed@).
cmdUntag :: Config -> Untag -> IO Response
cmdUntag cfg opts =
  runDBResponse cfg $ retagNamed cfg RemoveTags opts.tagged opts.tags opts.withChange opts.target

-- | Put tags on what a word names, or take them off, and answer with it
-- as it is now: the address, or the transaction as each portfolio that
-- holds it sees it. The word is read as label reads it ('readLabelled'):
-- a path, a hash, or else an address. --network tells apart an address or
-- a path the wallet has on several networks, and --type completes a path
-- typed without its type. An address that is none of the wallet is looked
-- for in the address book, and its contact takes the tags
-- (@namedAddress@).
--
-- With --with-change, the change down the chain of the payments of an
-- address of the wallet follows it
-- ("Haskoin.Wallet.Database.ChangeChain"), and the answer lists the
-- change addresses that did. Only such an address has a chain: the option
-- is refused with a transaction or a contact.
retagNamed :: Config -> Retag -> String -> [Tag] -> Bool -> Target -> ExceptT String (DB IO) Response
retagNamed cfg retag word typedTags withChange target = do
  named <- liftEither (readLabelled word)
  case named of
    LabelledAddress typed -> do
      refuseType "an address"
      found <- namedAddress (notTaggable typed) cfg target.network typed
      case found of
        OwnAddress accId row -> retagAddress accId row
        BookAddress net contact -> do
          refuseWithChange "a contact: its payments are not of the wallet, so hw knows no change of them"
          retagContact net (contactAddress contact)
    LabelledPath kindM addrType index -> do
      when (isJust kindM) $ refuseType "a path that starts with its type"
      portfolio <- requireActivePortfolio
      (accId, row) <- heldAddressAt cfg portfolio target.network (kindM <|> target.kind) addrType index
      retagAddress accId row
    LabelledTx hashNamed -> do
      refuseType "a transaction"
      refuseWithChange "a transaction: the tags that go down the chain of change are those of an address"
      when (isJust target.network) $
        throwError (command <> " takes no --network with a transaction: its tags are the same in every portfolio that holds it")
      hash <- walletTxHashOf hashNamed
      -- One on hidden networks alone is refused before it is tagged.
      -- Otherwise it is tagged wherever it is, and shown where hw shows
      -- it.
      _ <- shownViews cfg hashNamed =<< transactionViews hash
      views <- retagTransaction retag hash tags
      ResponseLabelledTxs (onShownNetworks cfg (placeNetwork . fst) views) <$> activePortfolioRef
  where
    -- A tag typed twice is typed once: untag would otherwise look for
    -- it again once it is off, and say that the address does not have it
    tags = nubOrd typedTags
    command = case retag of
      AddTags -> "tag"
      RemoveTags -> "untag"
    refuseType what =
      when (isJust target.kind) $
        throwError (command <> " takes no --type with " <> what)
    refuseWithChange what =
      when withChange $
        throwError (command <> " takes no --with-change with " <> what)
    -- untag is refused a tag the address does not have. With
    -- --with-change it still goes down the chain, and is refused only
    -- when no change address had the tags either: a user who forgot
    -- the option the first time runs the command again with it, and
    -- the address has lost the tag by then.
    retagAddress accId row = do
      missing <- case retag of
        AddTags -> [] <$ lift (tagAddress row tags)
        RemoveTags -> lift $ untagAddress row tags
      followedM <-
        if withChange
          then do
            pool <- accountPool <$> getAccountById accId
            Just <$> retagChangeChain retag pool row tags
          else return Nothing
      let lacked = "The address " <> cs (dBAddressAddress row) <> " does not have " <> tagsNamed missing
      case (missing, (.chainChanged) <$> followedM) of
        ([], _) -> return ()
        (_, Nothing) -> throwError lacked
        (_, Just []) -> throwError (lacked <> ", and no change address down the chain of its payments has " <> oneOrMore missing "it" "them")
        (_, Just _) -> liftIO $ tell terminal (lacked <> ": " <> oneOrMore missing "it" "they" <> " came off the change addresses alone")
      retaggedAnswer retag followedM accId row
    -- A word for the tags a sentence named before
    oneOrMore :: [Tag] -> String -> String -> String
    oneOrMore named one several = if length named == 1 then one else several
    -- A contact of the address book takes tags as an address of the
    -- wallet does
    retagContact net address = do
      case retag of
        AddTags -> lift $ tagContact net address tags
        RemoveTags -> do
          missing <- lift $ untagContact net address tags
          unless (null missing) $
            throwError ("The contact " <> cs address <> " does not have " <> tagsNamed missing)
      contactAnswer net address
    -- A first word that is no address of the wallet. When it reads as an
    -- address on no network, the user most likely put a tag first.
    notTaggable typed
      | any (\net -> isJust (textToAddr net typed)) allNets = notHeldOrKept cfg typed
      | otherwise =
          cs typed
            <> " is not an address, a path or the id of a transaction. "
            <> command
            <> " takes what to "
            <> command
            <> " first, then the tags: hw "
            <> command
            <> " ADDRESS|TXID|PATH TAG"

-- | List every tag of the database, with its colour and with how many
-- addresses and transactions have it, in every portfolio: a tag is of the
-- whole database
cmdTags :: Config -> IO Response
cmdTags cfg = runDBResponse cfg (ResponseTags <$> lift tagUses)

-- | Give a tag a new name in the whole database: on every address and
-- every transaction that has it, in every portfolio, and on every contact
-- of the address book. The tag keeps its colour. The answer shows the tag
-- as it is now, with what has it.
--
-- A new name that another tag has is refused, unless --merge asks for the
-- two to become one ('renameTag'). A merge cannot be taken apart, so it
-- never happens by a slip of the keyboard: the refusal names the option.
-- With --merge, the second word must be a tag. One that is none was most
-- likely mistyped, and a rename to the mistyped word would look done.
--
-- A word that is no tag has nothing to rename and is refused, as
-- deletetag refuses it. So is a new name equal to the old one, which
-- happens when only the capital letters differ: there is one tag for
-- both, and the user is told so rather than shown a rename that changed
-- nothing.
--
-- The command asks no question. Nothing is deleted without --merge, and
-- --merge is the word that agrees to a merge. The copy of the labels hw
-- keeps after each change holds the tags as they were before it
-- (@backUpLabels@). A merge into a tag that locks its coins may lock a
-- coin that a pending transaction spends: the user is told
-- (@tellingStopped@).
cmdRenameTag :: Config -> RenameTag -> IO Response
cmdRenameTag cfg opts =
  runDBResponse cfg . tellingStopped $ do
    _ <- requireTags [opts.oldTag]
    when (opts.oldTag == opts.newTag) $
      throwError ("The tag is named " <> old <> " already. Capital letters are taken as small ones.")
    takenM <- lift $ heldTag opts.newTag
    case (takenM, opts.merge) of
      (Just _, False) ->
        throwError ("There is a tag " <> new <> " already. To merge " <> old <> " into it, add --merge: hw renametag " <> old <> " " <> new <> " --merge")
      (Nothing, True) ->
        throwError ("There is no tag " <> new <> " to merge " <> old <> " into. hw tags lists the tags. To rename " <> old <> ", leave out --merge.")
      _ -> return ()
    lift $ renameTag opts.oldTag opts.newTag
    uses <- lift tagUses
    use <- case filter ((== opts.newTag) . shownTag . (.tagUseTag)) uses of
      [use] -> return use
      _ -> throwError (noSuchTag opts.newTag)
    return $ if opts.merge then ResponseTagMerged opts.oldTag use else ResponseTagRenamed opts.oldTag use
  where
    old = cs (tagText opts.oldTag)
    new = cs (tagText opts.newTag)

-- | Delete a tag from the whole database: from every address and every
-- transaction that has it, in every portfolio, and from every contact of
-- the address book, with its colour. The answer says how many had it. A word that is no tag is refused, since
-- there is nothing to delete, and the user most likely mistyped it.
--
-- The user is asked first, unless --yes, and told how many have the tag
-- (@confirmDeletion@, @deleteTagNotice@). The command deletes no funds,
-- no key and no label, but it is easy to take for untag, and it takes
-- the tag off everything at once. The copy of the labels hw keeps after
-- each change holds the tags as they were before it (@backUpLabels@).
cmdDeleteTag :: Config -> Tag -> Bool -> IO Response
cmdDeleteTag cfg tag yes =
  runDBResponse cfg $ do
    _ <- requireTags [tag]
    uses <- lift tagUses
    use <- case filter ((== tag) . shownTag . (.tagUseTag)) uses of
      [use] -> return use
      _ -> throwError (noSuchTag tag)
    confirmDeletion yes (deleteTagNotice use) ("Delete the tag " <> cs (tagText tag) <> "?")
    ResponseTagDeleted tag <$> lift (deleteTag tag)

{- Locks that come after a pending transaction -}

-- | Run a command that may lock coins, then tell the user of each
-- pending transaction that spends a coin the command locked: tag,
-- renametag --merge and importwallet. On stderr, as the questions, to
-- keep the --json answer clean.
--
-- A lock is what the user asked for last, so it wins over an older
-- payment: the command locks, and signtx and broadcast then refuse the
-- payment ("Haskoin.Wallet.Database.PendingTx".'refuseLockedCoins').
-- Refusing the lock instead was turned down. It would leave every other
-- coin of the tag unlocked until the user had settled the payment, and
-- it would make importwallet or renametag fail for a reason far from
-- what they do. lockcoin does refuse: it is about one coin, and a
-- refusal there leaves nothing else unlocked.
--
-- The user is told at once, since a pending payment is easy to forget,
-- and since this is the moment to delete it ('stoppedNotice'). The
-- transactions are those refused after the command and not before it,
-- so a command says nothing of a lock that was there already.
tellingStopped :: ExceptT String (DB IO) a -> ExceptT String (DB IO) a
tellingStopped command = do
  before <- stopped
  result <- command
  after <- stoppedPendingTxs
  liftIO $ mapM_ (tell terminal . uncurry stoppedNotice) [new | new@(held, _) <- after, held.pendingTxKey `notElem` before]
  return result
  where
    stopped = map ((.pendingTxKey) . fst) <$> stoppedPendingTxs

-- | What a command that locks a coin says of a pending transaction that
-- spends it: the transaction, by the short id pendingtxs shows, the
-- coin, as coins lists it, and what the lock does to the transaction. One
-- that waits to be signed or sent is stopped, and deletetx deletes it.
-- One that was sent is on its way, and no lock of the wallet stops the
-- network.
stoppedNotice :: PendingTx -> OutPoint -> String
stoppedNotice held outpoint =
  ("The pending transaction " <> short <> " spends the coin " <> shortCoinText outpoint <> ", which is locked now. ")
    <> if held.pendingTxOnline
      then "It was sent already, and the lock does not stop it"
      else "It can no longer be signed or sent: deletetx " <> short <> " deletes it"
  where
    short = pendingShortId held

{- The address book -}

-- | What an address the user typed names: an address of a wallet of the
-- database, with its account, or a contact of the address book, with its
-- network
data NamedAddress
  = OwnAddress !DBAccountId !DBAddress
  | BookAddress !Network !Contact

-- | What an address the user typed names, for label, tag, untag and
-- show. The wallet is looked in first, as before the address book
-- existed. An address that no wallet of the database has is looked for
-- in the address book. So one command works on both kinds of address,
-- and the user need not say which kind an address is.
--
-- An address the wallet has is never a contact: newcontact refuses it.
-- Should the wallet come to hold a contact later, as after importwallet
-- adds the wallet that owns it, the address is the wallet's here too.
--
-- A contact is picked among the networks as an address of the wallet is
-- ('chooseOnNetwork'): the one on --network, or the only one, or else
-- the one on the default network. The first argument is the refusal of
-- an address that is in neither.
namedAddress :: String -> Config -> Maybe Network -> Text -> ExceptT String (DB IO) NamedAddress
namedAddress notFound cfg netM typed = do
  own <- lift $ findAddressByText typed
  if not (null own)
    then (\(_, accId, row) -> OwnAddress accId row) <$> addressByTextOr notFound cfg netM typed
    else uncurry (flip BookAddress) <$> contactNamed notFound cfg netM typed

-- | The one contact an address the user typed names, with its network,
-- or the refusal given when the address book has none with it
contactNamed :: String -> Config -> Maybe Network -> Text -> ExceptT String (DB IO) (Contact, Network)
contactNamed notFound cfg netM typed = do
  found <- lift $ contactsNamed typed
  case chooseOnNetwork cfg netM [(row, snd row) | row <- found] of
    Right row -> return row
    Left FoundNowhere -> throwError notFound
    Left (OnHiddenNetworks networks) -> throwError (onHiddenNetworks ("The contact " <> cs typed) networks)
    Left (NotOnGivenNetwork net networks) ->
      throwError $ "The contact " <> cs typed <> " is on " <> networksText networks <> ", not on the --network " <> net.name
    Left (OnSeveralNetworks networks) ->
      throwError $ "The contact " <> cs typed <> " is on " <> networksText (nub networks) <> ": give the --network"

-- | The refusal of label, tag and untag for an address that the wallet
-- has not generated on this computer and that is not in the address
-- book (@notGeneratedOrKept@).
notHeldOrKept :: Config -> Text -> String
notHeldOrKept cfg = notGeneratedOrKept cfg []

-- | The refusal of show for such an address. It also says how to check
-- an address that another computer of the wallet gave out: by its path,
-- which show derives from the key of the account, whether this computer
-- generated the address or not. The offline computer generates no
-- address, and checking one there is what a user pastes it for.
notHeldToShow :: Config -> Text -> String
notHeldToShow cfg =
  notGeneratedOrKept
    cfg
    ["To check an address that another computer of the wallet gave out, give its path, as receive printed it: hw show PATH"]

-- | The refusal of an address that no wallet of the database has
-- generated and that is not in the address book, with the advice of the
-- command, a sentence each, before the way to keep an address of
-- somebody else.
--
-- It says what this computer knows, and not that the address is none of
-- the wallet: it may well be one. A computer holds the addresses it
-- generated, when receive gave them out or a sync found them paid. The
-- same wallet on another computer, such as the offline one, has not
-- generated the addresses the first one gave out. "The wallet has no
-- address" was false there: a careful user took it for a sign that the
-- online computer was tampered with, and a careless one added an address
-- of the wallet to the address book.
--
-- hw does not look for the address among those the accounts would
-- derive next. Such a search has an end, a thousand addresses for
-- instance, and a chain of a computer that never synced starts at zero:
-- the search would find the address in a small wallet, and deny it again
-- in a large one, after the user learned to trust the answer. A path
-- names an address at any size, so the refusal of show sends there.
--
-- An address that only hidden networks read is refused as anything else
-- on hidden networks alone is ('onHiddenNetworks'), with the command
-- that shows one of them. Every advice here ends in a refusal for it:
-- newcontact keeps no contact on a hidden network, and a path needs the
-- --network of the address. Showing the network is the first step.
--
-- A word that reads as an address on no network is of nobody, and the
-- refusal says no more.
notGeneratedOrKept :: Config -> [String] -> Text -> String
notGeneratedOrKept cfg advice typed
  | reading@(_ : _) <- fst <$> readingNetworks typed,
    null (onShownNetworks cfg id reading) =
      onHiddenNetworks ("The address " <> cs typed) reading
  | any (\net -> isJust (textToAddr net typed)) allNets =
      intercalate ". " $
        ["The wallet has not generated the address " <> cs typed <> " on this computer, and it is not in the address book"]
          <> advice
          <> ["To add an address of somebody else to the address book, run: hw newcontact " <> cs typed <> " LABEL"]
  | otherwise = "The wallet has no address " <> cs typed

-- | The answer showing a contact of a network as it is now, read again
-- by its address after a command that changed it
contactAnswer :: Network -> Text -> ExceptT String (DB IO) Response
contactAnswer net address = do
  contactM <- lift $ contactAt net address
  maybe (throwError ("The address book has no contact " <> cs address)) (return . ResponseContact net) contactM

-- | Add an address of somebody else to the address book, with its label,
-- if any, and its tags, and answer with the contact.
--
-- The network of the contact is the one --network names, where the
-- address must read. Without it, it is the only shown network the
-- address reads on, or else the default network, when the address reads
-- there. Most addresses read on one network. A legacy address reads on
-- Bitcoin and on Bitcoin Cash, and an address of a test network on the
-- others. hw does not guess past the default: the user is asked for
-- --network.
--
-- An address of a wallet of the database is refused, with where it is.
-- The address book is for the addresses of others. An address of the
-- wallet has a path, a label and tags of its own, and the views name it
-- by them. Two labels for one address would disagree sooner or later.
-- The check looks on the network of the contact alone: the same address
-- on another network is another chain, where the wallet may not hold it.
cmdNewContact :: Config -> NewContact -> IO Response
cmdNewContact cfg opts =
  runDBResponse cfg $ do
    (net, addr) <- liftEither $ contactNetwork cfg opts.network opts.address
    address <- liftEither $ contactText net addr
    own <- lift $ findAddressByText opts.address
    onNet <- filterM (\(_, accId, _) -> (== net) . accountNetwork <$> getAccountById accId) own
    forM_ (listToMaybe onNet) $ \(portfolio, _, row) ->
      throwError $
        cs address
          <> " is an address of the wallet: "
          <> cs (maybe (pathText (addrPath row)) poolPathText (addrPoolPath net row))
          <> " of "
          <> cs (portfolioText portfolio)
          <> ". The address book is for the addresses of others. To label it, run: hw label "
          <> cs address
          <> " LABEL"
    addContact net addr (fromMaybe "" opts.label)
    lift $ tagContact net address opts.tags
    contactAnswer net address

-- | The network a new contact is on, and its address read there: see
-- 'cmdNewContact'. A word that reads as an address on no network is
-- refused as no address. One that reads on hidden networks alone is
-- refused with the way to show one.
contactNetwork :: Config -> Maybe Network -> Text -> Either String (Network, Address)
contactNetwork cfg netM typed =
  case (netM, shown) of
    (Just net, _) -> maybe (Left (cs typed <> " is not an address of " <> net.name)) (Right . (net,)) (textToAddr net typed)
    (Nothing, [only]) -> Right only
    (Nothing, [])
      | null everywhere -> Left (cs typed <> " is not an address")
      | otherwise -> Left (onHiddenNetworks ("The address " <> cs typed) (fst <$> everywhere))
    (Nothing, several) ->
      maybe
        (Left ("The address " <> cs typed <> " reads on " <> networksText (fst <$> several) <> ": give the --network"))
        Right
        (listToMaybe [reading | reading <- several, fst reading == configDefaultNetwork cfg])
  where
    everywhere = readingNetworks typed
    shown = onShownNetworks cfg fst everywhere

-- | The networks of a portfolio that read a text as an address, each with
-- the address it reads. The text of an address does not always name one
-- network: one that starts with 1 reads on btc and on bch, and one that
-- starts with m on every test network.
readingNetworks :: Text -> [(Network, Address)]
readingNetworks typed = [(net, addr) | net <- portfolioNetworks, Just addr <- [textToAddr net typed]]

-- | Delete a contact from the address book, by its address, and answer
-- with the contact as it was. The contact is found as label finds it:
-- on --network, or the only one, or else on the default network.
--
-- The user is asked first, unless --yes, and shown the contact with its
-- label (@confirmDeletion@, @deleteContactNotice@). The command deletes
-- no funds and no key, and the copy of the labels hw keeps after each
-- change holds the address book as it was before (@backUpLabels@).
cmdDeleteContact :: Config -> Text -> Maybe Network -> Bool -> IO Response
cmdDeleteContact cfg typed netM yes =
  runDBResponse cfg $ do
    (contact, net) <- contactNamed (notInBook typed) cfg netM typed
    confirmDeletion yes (deleteContactNotice contact) "Delete the contact?"
    lift $ deleteContact net (contactAddress contact)
    return (ResponseContactDeleted net contact)

-- | The refusal of an address the address book does not have
notInBook :: Text -> String
notInBook typed = "The address book has no contact " <> cs typed <> ". hw contacts lists them."

-- | The address book, a section for each network hw shows that has
-- contacts, or for the one --network keeps, in the order of the labels
-- ('Haskoin.Wallet.Contacts.contactOrder'), or in its reverse with
-- --reverse. A contact names the one to go on after, by its address: the
-- listing starts in the section of its network.
--
-- With a filter of tags, the listing keeps the contacts that have any
-- one of the tags, and those without a tag when asked for. Each contact
-- comes with all of its tags, kept or not. A word that is no tag is
-- refused, as in the other listings.
--
-- An address book is typed by hand and holds few rows, so each section
-- is read whole, and its table sizes its columns to all of its contacts
-- ("Haskoin.Wallet.Database.Contact").
--
-- The listing is the same whichever portfolio is active, and works in a
-- database without any: the address book is of the whole database.
contactsListing :: Config -> Maybe Network -> TagFilter -> Paging Text -> ExceptT String (DB IO) Sections
contactsListing cfg netM kept paging = do
  _ <- requireTags kept.keptTags
  let listed = [net | net <- portfolioNetworks, isShown cfg net, maybe True (== net) netM]
      rowsOf net = do
        contacts <- lift $ networkContacts net kept
        return $ case listedOrder paging of
          UsualOrder -> contacts
          ReverseOrder -> reverse contacts
      section net startM =
        Listing
          { listingStart = startM,
            listingRows = \afterM countM -> do
              contacts <- rowsOf net
              let following = case afterM of
                    Nothing -> contacts
                    Just cursor -> drop 1 $ dropWhile ((/= cursor) . contactAddress) contacts
              return $ maybe id genericTake countM following,
            listingCursor = Right . contactAddress,
            listingAnswer = \contacts -> ResponseContacts [(net, contacts) | not (null contacts)],
            listingWhole = True,
            listingShownEmpty = False
          }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (listed, Nothing)
    Just typed -> do
      held <- forM listed $ \net -> do
        contacts <- rowsOf net
        let named = [address | Just addr <- [textToAddr net typed], Right address <- [contactText net addr]]
        return (net, listToMaybe [contactAddress contact | contact <- contacts, contactAddress contact `elem` named])
      found <- liftEither $ sectionOfAddress typed held
      case found of
        Just (i, address) -> return (drop i listed, Just address)
        Nothing -> throwError ("The contact " <> cs typed <> " is not among the contacts listed")
  return $ Sections (length listed > 1) paging.pagingMax (zipWith section from (startM : repeat Nothing))

{- Coins -}

-- | The coins of a portfolio, or its locked ones, a section for each
-- network hw shows, or for the one --network keeps, in the order coins
-- lists them, or in its reverse with --reverse, the oldest coin first.
-- --type keeps the coins of one type. A coin names the one to
-- go on after, with its block: the listing starts in the section of its
-- network.
--
-- With a filter of tags, the listing keeps the coins at an address that
-- has any one of the tags, and those at an address without a tag when
-- asked for. A coin has no tag of its own: it is in the groups of its
-- address, which is what --spend-from-tag spends from.
--
-- The section of a network where the portfolio never synced says so
-- (the type @SectionSync@).
coinsListing :: Config -> Portfolio -> Target -> Bool -> TagFilter -> Paging CoinRef -> ExceptT String (DB IO) Sections
coinsListing cfg portfolio target locked kept paging = do
  _ <- requireTags kept.keptTags
  accounts <- listedAccounts cfg portfolio target.network target.kind
  let networks = byNetwork accounts
      shown = if locked then LockedCoins else AllCoins
  sections <- forM networks $ \(net, onNet) -> do
    state <- sectionSync portfolio net onNet
    return $ \startM ->
      Listing
        { listingStart = startM,
          listingRows = listedCoinsAfter (listedOrder paging) (portfolioPool portfolio net) (fst <$> onNet) shown kept,
          listingCursor = Right . snd,
          listingAnswer = grouped state ResponseCoins (groupOf portfolio net onNet state) . map fst,
          listingWhole = False,
          listingShownEmpty = state.sectionShownNotSynced
        }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (sections, Nothing)
    Just named -> do
      outpoint <- coinNamed ("The coin " <> coinRefText named <> " is not among those listed") named
      blocks <- forM networks $ \(_, onNet) -> lift (runExceptT (coinBlock (fst <$> onNet) outpoint))
      case [(i, block) | (i, Right block) <- zip [0 :: Int ..] blocks] of
        (i, block) : _ -> return (drop i sections, Just (block, outpoint))
        [] -> throwError ("The coin " <> coinRefText named <> " is not among those listed")
  return $ Sections (length networks > 1) paging.pagingMax (zipWith ($) from (startM : repeat Nothing))

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
-- With --reverse each section starts with the oldest transaction, and
-- each transaction keeps the balance it has in the usual order.
-- The listing starts in the section of the first network that has the
-- transaction.
--
-- With a filter of tags, the listing keeps the transactions that have
-- one of the tags asked for, and those at an address that has one of the
-- tags of addresses asked for, an address of the portfolio or a contact
-- of the address book, and those left to sort when asked for
-- ('keptTxsAfter'). A filter only leaves transactions out: each one left
-- shows the balance it shows in the whole listing, that of the portfolio
-- after it.
--
-- The section of a network where the portfolio never synced says so
-- (the type @SectionSync@).
txsListing :: Config -> Portfolio -> Maybe Network -> TxTagFilter -> Paging HashRef -> ExceptT String (DB IO) Sections
txsListing cfg portfolio netM kept paging = do
  _ <- requireTags (kept.keptTxTags <> kept.keptAddressTags)
  pools <- listedPools cfg portfolio netM
  sections <- forM pools $ \pool -> do
    accounts <- lift $ poolAccounts pool
    state <- sectionSync portfolio pool.poolNetwork accounts
    return $ \startM ->
      Listing
        { listingStart = startM,
          listingRows = keptTxsAfter (listedOrder paging) pool kept,
          listingCursor = Right . snd,
          listingAnswer = grouped state ResponseTxs (groupOf portfolio pool.poolNetwork accounts state) . map fst,
          listingWhole = False,
          listingShownEmpty = state.sectionShownNotSynced
        }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (sections, Nothing)
    Just named -> do
      txid <- chainTxidOf named
      cursors <- forM pools $ \pool -> lift (runExceptT (txsCursorAt (listedOrder paging) pool txid))
      case [(i, cursor) | (i, Right cursor) <- zip [0 :: Int ..] cursors] of
        (i, cursor) : _ -> return (drop i sections, Just cursor)
        [] -> throwError ("The transaction " <> hashRefText named <> " is not among those listed")
  return $ Sections (length pools > 1) paging.pagingMax (zipWith ($) from (startM : repeat Nothing))

-- | The pending transactions of every portfolio of every wallet, on
-- every network hw shows, or on the --network alone (@pendingListing@).
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
--
-- With a filter of tags, the listing keeps the pending transactions the
-- filter keeps, as txs does (@pendingListing@).
pendingTxsListing :: Config -> Maybe Network -> TxTagFilter -> Paging HashRef -> ExceptT String (DB IO) Sections
pendingTxsListing cfg netM kept paging = do
  _ <- requireTags (kept.keptTxTags <> kept.keptAddressTags)
  pools <- onShownNetworks cfg (.poolNetwork) <$> lift allPools
  pendingListing (Just [pool | pool <- pools, maybe True (== pool.poolNetwork) netM]) kept paging

-- | The pending transactions of the pools, or of every pool, in one list
-- rather than in sections: by wallet, by portfolio and by network, in the
-- order of the networks of a portfolio, and the newest first on each, or
-- the oldest first with --reverse. The places keep their order, as the
-- sections of the other listings do. Each says where it is. The answer carries the active portfolio, for the
-- screen to name the portfolio after the paths of a transaction of
-- another one. The one to go on after is named by either of its
-- hashes. There are few of them, so the listing is read whole, and its
-- table sizes its columns to all of them. pendingtxs and exportsheet list
-- the pools on the networks hw shows, and pendingtxs those of one network
-- with --network.
--
-- A filter of tags keeps the transactions that have one of its tags, or
-- that spend from or pay an address of their portfolio that has one of its
-- tags of addresses, or a contact of the address book that has one, or that
-- are left to sort when those are asked for (the type
-- t'Haskoin.Wallet.Database.Tag.TxTagFilter'). The listing is read whole,
-- so the filter is applied to the transactions once read, each with its
-- tags. Each one kept is told the tags of addresses the listing keeps, for
-- its short views to show them (the type t'TxTags').
pendingListing :: Maybe [Pool] -> TxTagFilter -> Paging HashRef -> ExceptT String (DB IO) Sections
pendingListing poolsM kept paging = do
  start <- forM paging.pagingAfter (fmap snd . pendingCursor poolsM <=< pendingHashOf)
  activeM <- activePortfolioRef
  return . oneSection paging.pagingMax $
    Listing
      { listingStart = start,
        listingRows = \afterM countM -> do
          everyPending <- pendingAfter poolsM Nothing Nothing
          let newestFirst = [(place, toldKept info) | (place, info) <- everyPending, keeps info]
              held = sortOn placeOrder $ case listedOrder paging of
                UsualOrder -> newestFirst
                ReverseOrder -> reverse newestFirst
          let following = case afterM of
                Nothing -> held
                Just cursor -> drop 1 $ dropWhile ((/= Just cursor) . pendingHashOfRow) held
          return $ maybe id genericTake countM following,
        listingCursor = maybe (Left "pendingtxs lists the transactions the wallet holds only") Right . pendingHashOfRow,
        listingAnswer = ResponsePendingTxs activeM,
        listingWhole = True,
        listingShownEmpty = False
      }
  where
    -- By wallet, portfolio and network: the sort is stable, so it keeps
    -- the order of the transactions in each
    placeOrder (place, _) = (placePortfolio place, networkRank (placeNetwork place))
    pendingHashOfRow (_, info) = case txInfoStatus info of
      Pending pending -> Just (pendingNosigHash pending)
      OnChain _ -> Nothing
    -- Whether the filter keeps a transaction, from the tags it shows
    keeps info
      | not (isTxFiltered kept) = True
      | otherwise =
          let tags = txInfoTags info
              named shown = shownTag <$> shown
              tagsOfContacts = contactTags <$> Map.elems (txInfoContacts info)
           in any (`elem` kept.keptTxTags) (named (txOwnTags tags))
                || any (any (`elem` kept.keptAddressTags) . named) (Map.elems (txAddressTags tags))
                -- The contacts it pays or was paid from count as its
                -- addresses do
                || any (any (`elem` kept.keptAddressTags) . named) tagsOfContacts
                -- Left to sort: no tag of its own, and none of a contact
                || (kept.keptUntaggedTxs && null (txOwnTags tags) && all null tagsOfContacts)
    toldKept info = info {txInfoTags = (txInfoTags info) {txKeptAddressTags = kept.keptAddressTags}}

-- | Add the transaction of a file to the pending transactions of its
-- portfolio, and show it as the wallet holds it from now on: with the
-- labels of the wallet, not those of the file. hw finds the portfolio by
-- itself: the one that pays, when the transaction moves funds between
-- two, or the active one among several it cannot tell apart
-- ('transactionPool').
--
-- A file of a transaction the wallet holds already, in the same version
-- or signed where the file is not, changes nothing. The command says so
-- on stderr and answers with the transaction as the wallet holds it, as
-- any import does: importing a file twice is no mistake.
cmdImportTx :: Config -> FilePath -> IO Response
cmdImportTx cfg file =
  runDBResponse cfg $ do
    TxFileRead (TxFile netM tsd _ _) _ <- readTxFileIn (configDefaultNetwork cfg) file
    activeM <- lift activePortfolioId
    (poolM, completed) <- transactionPool cfg activeM netM tsd
    pool <- liftMaybe (notInWallet <> ". " <> sweepStaysInFile) poolM
    (pendingHash, did) <- importedTx pool completed
    held <- requirePendingTx pendingHash
    when (did == HeldAlready) $ liftIO $ tell terminal (heldAlreadyNotice held)
    txAnswer pool =<< pendingSummary held
  where
    sweepStaysInFile =
      "A sweep that pays no address of the wallet stays in its file: \
      \hw signsweep --input signs it there, and hw broadcast --input sends it"

-- | What importtx says of a file that brings nothing new: the wallet
-- holds the transaction, and how far along it is there
heldAlreadyNotice :: PendingTx -> String
heldAlreadyNotice held = "The wallet holds this transaction already, " <> state <> ": nothing was changed."
  where
    state
      | held.pendingTxOnline = "sent"
      | isSigned (txSignDataTx held.pendingTxData) = "signed and not sent"
      | otherwise = "not signed"

-- | Write a pending transaction to a file, with the labels of its
-- addresses as the wallet has them now
cmdExportTx :: Config -> HashRef -> FilePath -> IO Response
cmdExportTx cfg named file =
  runDBResponse cfg $ do
    held <- pendingTxNamed cfg named
    checkFileToWrite file
    let pool = held.pendingTxPool
    info <- txInfoOf pool held.pendingTxData
    writeTxFile (Just pool) file (labelledTxFile pool.poolNetwork info held.pendingTxData)
    return $ ResponseFile file

-- | Delete a pending transaction, named by either of its hashes. The
-- answer gives both, whichever the user typed.
--
-- The user is asked first, unless --yes, and told where the transaction
-- stands (@confirmDeletion@, @deleteTxNotice@). The question comes after
-- the store was asked: a transaction the server still knows is refused
-- without a question ('deleteTxAfter').
cmdDeleteTx :: Config -> HashRef -> Bool -> IO Response
cmdDeleteTx cfg named yes =
  runDBResponse cfg $ do
    held <- pendingTxNamed cfg named
    let tx = txSignDataTx held.pendingTxData
        txidM = if isSigned tx then Just (txHash tx) else Nothing
        asking found = confirmDeletion yes (deleteTxNotice found) "Delete the pending transaction?"
    storeOn <- liftIO $ webStores cfg
    (coins, addrs) <- deleteTxAfter storeOn asking (nosigHash tx)
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
fileAccountOf :: Portfolio -> DBAccount -> FileAccount
fileAccountOf portfolio acc =
  FileAccount (accountXPubKey acc) (accountKind acc) (accountPath portfolio acc) (portfolioFingerprint portfolio) (accountNetwork acc)

-- | The keys of the accounts of a pool, as its files name them
poolFileAccounts :: Pool -> ExceptT String (DB IO) [FileAccount]
poolFileAccounts pool = do
  portfolio <- getPortfolio pool.poolPortfolio
  map (fileAccountOf portfolio . snd) <$> lift (poolAccounts pool)

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
readTxFileIn :: Network -> FilePath -> ExceptT String (DB IO) TxFileRead
readTxFileIn fallback file = do
  bytes <- liftEitherIO $ readFileBytes file
  txFileOfBytes fallback file bytes

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
txFileOfBytes :: Network -> FilePath -> BS.ByteString -> ExceptT String (DB IO) TxFileRead
txFileOfBytes fallback file bytes =
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
      txFile <- refused $ psbtTxFile (Just fallback) pools psbt
      return $ TxFileRead txFile (Just psbt)
    allAccountPools = do
      accounts <- getAccounts
      let poolsHeld = nub [accountPool acc | (_, _, acc) <- accounts]
      return [[fileAccountOf portfolio acc | (portfolio, _, acc) <- accounts, accountPool acc == pool] | pool <- poolsHeld]
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
writeTxFile :: Maybe Pool -> FilePath -> TxFile -> ExceptT String (DB IO) ()
writeTxFile poolM file txFile = do
  accounts <- maybe (return []) poolFileAccounts poolM
  psbt <- liftEither $ txFilePsbt accounts txFile
  liftEitherIO $ writeFileBytes file (encodePsbt psbt)

-- | Write a transaction of the network to the --output file, when there is
-- one, with the labels of its addresses as its summary has them, and the
-- keys of its pool when it has one
writeOutput :: Maybe Pool -> Network -> Maybe FilePath -> TxInfo -> TxSignData -> ExceptT String (DB IO) ()
writeOutput poolM net output info tsd = for_ output $ \file -> writeTxFile poolM file (labelledTxFile net info tsd)

-- | The signing data of a transaction just prepared, with the label of
-- --label, if given
withLabel :: Maybe Text -> TxSignData -> TxSignData
withLabel labelM tsd = tsd {txSignDataLabel = fromMaybe noLabel labelM}

-- | Keep a transaction of the pool just prepared, among its pending
-- transactions and in the --output file when there is one, and give its
-- summary. The address --change-back named, if any, may be
-- used or held by another pending transaction ('importPreparedTx'). The
-- tags are those the payment named with --spend-from-tag, whose locks it
-- opens.
--
-- The summary is the one of the transaction as the wallet now holds it
-- ('pendingSummary'), so that the answer shows the time it was created,
-- as pendingtxs and show do from then on. The signing data alone has no
-- time, and the answer started with "time unknown" before.
storePrepared :: Pool -> Maybe Address -> [Tag] -> Maybe FilePath -> TxSignData -> ExceptT String (DB IO) TxInfo
storePrepared pool changeBackM opened output tsd = do
  pendingHash <- importPreparedTx pool changeBackM opened tsd
  info <- pendingSummary =<< requirePendingTx pendingHash
  writeOutput (Just pool) pool.poolNetwork output info tsd
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
-- It is a sentence for each line of the screen: the fact, the block that
-- shows it, and what a sync may change. On one line they ran past the
-- width of a terminal, with the question after them.
-- Right the height of the best block otherwise, which the lock time of a
-- new transaction is drawn from ('lockTimeFor'), or none before the first
-- sync: a wallet that never synced has no coins to spend, and no height to
-- be behind.
behindTheChain :: Network -> ExceptT String (DB IO) (Either [String] (Maybe BlockHeight))
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
          Left
            [ "The wallet is behind the chain.",
              "Its best block, " <> show best.blockHeight <> ", was mined " <> agoText now (posixSecondsToUTCTime (fromIntegral time)) <> ".",
              syncMayFind
            ]
      | otherwise ->
          Left
            [ "The wallet may be behind the chain.",
              "It does not know when its best block, " <> show best.blockHeight <> ", was mined.",
              syncMayFind
            ]
  where
    syncMayFind = "A sync may find coins spent or received since."

-- | Ask whether to prepare a transaction while the wallet is behind the
-- chain, told why, unless --yes. The transaction then gets a lock time of
-- 0, as Bitcoin Core and Electrum give one then. A no stops the command:
-- the alternative is to sync first.
--
-- Each sentence of the reason has a line of its own, and the question
-- comes on the last line, where the answer is typed. The dust coins of
-- sendall are told before their question in the same way.
confirmBehind :: Bool -> [String] -> ExceptT String (DB IO) ()
confirmBehind yes why =
  unless yes $ do
    liftIO $ mapM_ (tell terminal) why
    agreed <- askYes terminal "Prepare the transaction anyway, with a lock time of 0?"
    unless agreed $ throwError "The transaction was not prepared: run sync first"

-- | Ask before a transaction of the pool is kept that joins groups of
-- coins the user keeps apart with tags, unless --yes: the coins it spends
-- do not all have the same tags, as a kyc coin and a p2p coin
-- ("Haskoin.Wallet.Tx.Mixing"). The coin selection picks coins by their
-- value and knows nothing of tags, so the user learns it here, while the
-- coins can still be chosen otherwise.
--
-- The groups are told first, each on a line, then the question on the
-- last line, as given, with what the command fails with after a no. The
-- transaction is given as it was built, before anything is kept: a no
-- leaves no pending transaction, no change address taken and no --output
-- file. The command calls it after its other questions, since the coins
-- are only known once the transaction is built, which the answers to
-- those decide.
--
-- The rule given says whether the groups are asked about: 'joinsOutsideTags'
-- for a payment, and for a replacement whether the coins it adds bring a
-- group ('addsGroup').
--
-- --yes says nothing of the groups. The answer of the command shows the
-- tags under each address the transaction spends from, as it always does.
confirmMixing :: Bool -> AmountUnit -> Pool -> ([SpentGroup] -> Bool) -> String -> String -> TxSignData -> ExceptT String (DB IO) ()
confirmMixing yes unit pool asksAbout asked declined tsd =
  unless yes $ do
    groups <- spentGroups <$> txInfoOf pool tsd
    when (asksAbout groups) $ do
      liftIO $ renderOn stderr (mixingDoc unit groups)
      agreed <- askYes terminal asked
      unless agreed $ throwError declined

-- | @confirmMixing@ for a payment just built, which preparetx, send,
-- preparesendall and sendall ask the same way. The tags are those the
-- payment named with --spend-from-tag: coins that all have them are the
-- group the user asked for ('joinsOutsideTags'). A no says how to choose
-- the coins of one group.
confirmPaymentMixing :: Bool -> AmountUnit -> Pool -> [Tag] -> TxSignData -> ExceptT String (DB IO) ()
confirmPaymentMixing yes unit pool named =
  confirmMixing yes unit pool (joinsOutsideTags named) "Prepare the transaction anyway?" $
    "The transaction was not prepared: --spend-from-tag, --spend-from and --coin choose the coins it spends"

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
cmdFees :: Config -> Maybe Network -> IO Response
cmdFees cfg netM =
  runResponse $ do
    let net = commandNetwork cfg netM
    storeOn <- liftIO $ webStores cfg
    rates <- recentFeeRates (configParallelRequests cfg) (storeOn net)
    return $ ResponseFees net rates

-- | A transaction just prepared, as the wallet keeps it: what preparetx,
-- preparesendall and bumpfee answer with, and what send and sendall go on
-- from
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

-- | Keep a transaction of the pool just prepared, as 'storePrepared' does,
-- with the coins it leaves in the portfolio. The tags are those the
-- payment named with --spend-from-tag ('storePrepared').
keepPreparedLeaving :: Pool -> Maybe Address -> [Tag] -> Maybe FilePath -> TxSignData -> [(OutPoint, LeftReason)] -> ExceptT String (DB IO) Prepared
keepPreparedLeaving pool changeBackM opened output tsd leftOut = do
  left <- leftCoinsOf pool opened leftOut
  info <- storePrepared pool changeBackM opened output tsd
  return (Prepared pool (nosigHash (txSignDataTx tsd)) info left)

-- | A transaction just prepared, with the tags of --tag put on it. This is
-- what the command tag does with the hash of the transaction afterwards,
-- done at the moment the user knows what the payment is for. A tag put
-- later is the step that is skipped.
--
-- The tags are kept under the nosigHash, as those of any pending
-- transaction, and go to its txid when a sync finds it on the network
-- ('retirePendingTx'). A tag that is not in the database yet is made, as
-- the command tag makes one. The command is one transaction of the
-- database, so a payment that fails after this leaves neither the tags nor
-- a new tag behind.
--
-- The summary is read again with its tags, since the answer shows them,
-- and send and sendall show the transaction from it before the mnemonic.
-- The --output file is written before, and holds no tag: a file carries
-- the label of a transaction alone.
taggedPrepared :: [Tag] -> Prepared -> ExceptT String (DB IO) Prepared
taggedPrepared [] prepared = return prepared
taggedPrepared tags prepared = lift $ do
  tagTx prepared.preparedPool prepared.preparedHash tags
  info <- fillTxInfoTags prepared.preparedPool prepared.preparedInfo
  return prepared {preparedInfo = info}

-- | A transaction just prepared, with the tags of the group of its coins
-- on its new change address, when --tag-change asks for them
-- ("Haskoin.Wallet.Database.PendingTx".'changeTakesTags'). The summary is
-- read again with them, since the answer shows the tags of the change
-- under its address. A payment without change tags nothing. A change
-- that takes no tag, because the coins spent share none, is told on
-- stderr: the option was typed to keep the change in a group, and the
-- answer would otherwise look like that of any payment.
changeTaggedPrepared :: Bool -> Prepared -> ExceptT String (DB IO) Prepared
changeTaggedPrepared False prepared = return prepared
changeTaggedPrepared True prepared = do
  taken <- lift $ changeTakesTags prepared.preparedPool prepared.preparedInfo.txInfoSummary
  when (any (null . snd) taken) . liftIO $
    tell terminal "--tag-change put no tag on the change address: the coins the payment spends share no tag"
  info <- lift $ fillTxInfoTags prepared.preparedPool prepared.preparedInfo
  return prepared {preparedInfo = info}

-- | The answer that shows a transaction just prepared, with the coins it
-- leaves in the portfolio
preparedResponse :: Prepared -> ExceptT String (DB IO) Response
preparedResponse prepared = do
  place <- poolPlace prepared.preparedPool
  ResponsePrepared place prepared.preparedInfo prepared.preparedLeft <$> activePortfolioRef

-- | Keep a transaction of the pool just prepared, as 'storePrepared' does,
-- and answer with the coins it leaves in the portfolio, each as coins
-- shows it, and why it stays
preparedAnswer :: Pool -> Maybe Address -> Maybe FilePath -> TxSignData -> [(OutPoint, LeftReason)] -> ExceptT String (DB IO) Response
preparedAnswer pool changeBackM output tsd leftOut =
  preparedResponse =<< keepPreparedLeaving pool changeBackM [] output tsd leftOut

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
-- A --spend-from path, such as bip44\/1\/0, is one of the active portfolio.
-- It tells the networks where the portfolio has an address at that path,
-- as an address tells the networks that hold it ('rowsAtPath'). The
-- address spent from is then the one at the path on the network of the
-- payment.
--
-- A --spend-from-tag tag chooses every address of the pool that has it,
-- as if each had its own --spend-from. It tells the networks where the
-- active portfolio has an address with the tag. A word that is no tag of
-- the database is refused ('requireTags'), and so is a tag that no
-- address of the active portfolio has, on the network of the payment:
-- the user means to spend something, and a payment built from other coins
-- would not be the one asked for.
--
-- Only the networks hw shows count. A coin or an address the wallet has
-- on hidden networks alone is refused, with the command that shows its
-- network (@shownAmong@).
--
-- The addresses named and those of the tags come apart. --coin-type
-- refuses an address named that is of the other type, since the two
-- options disagree about it. A tag is a group, which may hold addresses
-- of both types, so --coin-type narrows it instead ('chosenAddresses').
controlledPool :: Config -> Maybe Network -> [OutPoint] -> [AddressNamed] -> [Tag] -> ExceptT String (DB IO) (Pool, [Address], [Address])
controlledPool cfg netM outpoints spendFrom spendFromTags = do
  portfolio <- requireActivePortfolio
  _ <- requireTags spendFromTags
  accounts <- onShownNetworks cfg (accountNetwork . snd) <$> lift (portfolioAccountRows portfolio.portfolioId)
  byTag <- forM spendFromTags $ \tag -> do
    tagged <- lift $ filterM (\(accId, _) -> not . null <$> taggedAddressRows accId [tag]) accounts
    when (null tagged) $
      throwError ("No address of " <> portfolioText portfolio <> " has the tag " <> cs (tagText tag) <> ": hw addresses --tag " <> cs (tagText tag) <> " lists those of the active portfolio")
    return (nub (accountNetwork . snd <$> tagged))
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
  net <- liftEither $ paymentNetwork cfg netM (nub (concat (byCoin <> byAddress <> byTag)))
  let pool = portfolioPool portfolio net
      place = portfolioPlace portfolio net
  addrs <- forM spendFrom $ \named -> do
    addrM <- poolAddressNamed pool named
    case (addrM, named) of
      (Just addr, _) -> return addr
      (Nothing, NamedByAddress typed) -> throwError ("The address " <> cs typed <> " is not in " <> placeText place)
      (Nothing, NamedByPath {}) -> throwError (placeText place <> " has no address at " <> addressNamedText named)
  tagged <- forM spendFromTags $ \tag -> do
    held <- map fst <$> taggedPoolAddresses pool tag
    when (null held) $
      throwError ("No address of " <> placeText place <> " has the tag " <> cs (tagText tag))
    return held
  return (pool, addrs, nub (concat tagged))

-- | The addresses coin control spends from: those named with
-- --spend-from, and those of the tags of --spend-from-tag that --coin-type
-- keeps. An address of a tag that is of the other type is left out
-- without a word, as a filter leaves out a coin of every coin of the
-- portfolio: --spend-from-tag kyc --coin-type bip44 spends the bip44
-- coins of the tag. An address named is kept whatever its type, and its
-- coins are refused later when the filter leaves them out
-- ('refuseOtherType').
chosenAddresses :: Maybe AccountKind -> [Address] -> [Address] -> [Address]
chosenAddresses coinTypeM named tagged =
  nub (named <> [addr | addr <- tagged, maybe True ((== addressKind addr) . Just) coinTypeM])

-- | The addresses of a pool that have a tag, each with its path in the
-- pool, in the order of the accounts and of their paths: what
-- --spend-from-tag spends from
taggedPoolAddresses :: Pool -> Tag -> ExceptT String (DB IO) [(Address, PoolPath)]
taggedPoolAddresses pool tag = do
  accounts <- lift $ poolAccounts pool
  fmap concat . forM accounts $ \(accId, acc) -> do
    rows <- lift $ taggedAddressRows accId [tag]
    forM rows $ \(internal, index, text) -> do
      addr <- liftEither $ textToAddrE pool.poolNetwork text
      return (addr, PoolPath (accountKind acc) (chainPath internal index))

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
    | otherwise -> Left ("The coins, addresses or tags given are on " <> networksText held <> ", not on the --network " <> net.name)
  (Nothing, []) -> Right (configDefaultNetwork cfg)
  (Nothing, [only]) -> Right only
  (Nothing, several)
    | configDefaultNetwork cfg `elem` several -> Right (configDefaultNetwork cfg)
    | otherwise -> Left ("The coins, addresses or tags given are on " <> networksText several <> ": give the --network")

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
-- and answer with it: @preparePayment@, which send starts with too, says
-- how the options are read.
cmdPrepareTx :: Config -> AmountUnit -> PrepareTx -> IO Response
cmdPrepareTx cfg unit opts =
  runDBResponse cfg $ do
    storeOn <- liftIO $ webStores cfg
    preparedResponse =<< preparePayment cfg storeOn unit opts

-- | Build the payment the options ask for, and keep it as a pending
-- transaction of the active portfolio, unsigned, which locks its coins.
-- It may spend the coins of both types of address together. This is all
-- that preparetx does but its answer, and what send starts with. The two
-- commands share it, so they make the same transaction of the same
-- options, with the same questions and the same refusals. The stores are
-- those of the command. The fee rates of recent blocks are read from one
-- when the options give no rate.
--
-- Coin control has choosers and filters. The choosers, --coin,
-- --spend-from and --spend-from-tag, add up: the coins chosen are those
-- of every --coin, those at every --spend-from address, and those at
-- every address that has a --spend-from-tag tag. The payment spends all of those
-- that can be spent, and only them. With --select, the strategy selects
-- among them instead, as it does among every coin of the pool without a
-- chooser. The filters, --min-conf and --coin-type, narrow the coins down:
-- every coin of the pool, or the coins chosen. They never add a coin, so
-- --coin-type with a --spend-from of the same type spends the coins at
-- that address alone. The answer lists the coins chosen that cannot be
-- spent, and why. The chosen coins --select does not need are not listed,
-- like the coins a selection leaves without a chooser.
--
-- The tags of --spend-from-tag are also the keys of the payment: they
-- open the lock of each of them that locks its coins
-- ("Haskoin.Wallet.Database.Coin".'spendableCoinsOpening'). The coins of
-- every other tag that locks stay unspendable, for the strategy and for
-- --coin and --spend-from alike, and the pending transaction is kept
-- with the same tags opened ('keepPreparedLeaving').
--
-- The change goes to a new change address of the portfolio, of the type
-- the builder works out ('Haskoin.Wallet.Tx.Building.changeKindRule').
-- --change-back sends it back to an address of the portfolio on the
-- network of the payment instead: the one it names, by itself or by its
-- path, or with largest that of the largest coin the payment spends, of
-- the type of the change, or with smallest that of the smallest one.
--
-- --tag-change gives the new change address the tags every coin the
-- payment spends has, once the transaction is kept ('changeTaggedPrepared').
-- So --spend-from-tag with it keeps the change in the group it spends
-- from. Without it the change address takes no tag. It is refused with
-- --change-back: the address the change goes back to keeps its own tags.
--
-- The coins of the payment are known once it is built. When they do not
-- all have the same tags, the user is asked before anything is kept
-- ('confirmPaymentMixing'): the selection knows nothing of tags, and one
-- transaction joins for good the groups the tags keep apart.
--
-- --label and --tag say what the payment is for. The label goes into the
-- signing data, and with it to the --output file. The tags are put on the
-- pending transaction ('taggedPrepared').
preparePayment :: Config -> (Network -> Store) -> AmountUnit -> PrepareTx -> ExceptT String (DB IO) Prepared
preparePayment cfg storeOn unit opts = do
  coins <- mapM walletCoinOf opts.coins
  (pool, namedAddrs, taggedAddrs) <- controlledPool cfg opts.network coins opts.spendFrom opts.spendFromTags
  let net = pool.poolNetwork
      spendFrom = chosenAddresses opts.coinType namedAddrs taggedAddrs
      controlled = not (null coins && null namedAddrs && null opts.spendFromTags)
  when (opts.select && not controlled) $
    throwError "--select needs coins to select among: give --coin, --spend-from or --spend-from-tag"
  when (opts.tagChange && isJust opts.changeBack) $
    throwError "--tag-change tags a new change address, and --change-back sends the change to an address that keeps its tags: give one of them"
  -- An address named has a type of its own. largest and smallest pick a
  -- coin of the type of the change, which --change-type may set.
  case (opts.changeBack, opts.changeType) of
    (Just (BackToNamed _), Just _) ->
      throwError "--change-back ADDRESS names the address of the change, which has a type: it takes no --change-type. --change-back largest and smallest do."
    _ -> return ()
  -- A type the network does not have is refused before anything is asked
  for_ opts.coinType $ liftEither . checkKindOnNetwork net
  for_ opts.changeType $ \kind -> do
    liftEither $ checkKindOnNetwork net kind
    void $ poolAccountOf pool kind
  changeTo <- case opts.changeBack of
    Nothing -> return (NewChangeAddress opts.changeType)
    Just BackToLargestCoin -> return (ChangeToLargestCoin opts.changeType)
    Just BackToSmallestCoin -> return (ChangeToSmallestCoin opts.changeType)
    Just (BackToNamed named) -> do
      addrM <- poolAddressNamed pool named
      place <- poolPlace pool
      let refused = "The change can only go back to an address of " <> placeText place <> ", and "
      case (addrM, named) of
        (Just addr, _) -> return (ChangeBackTo addr)
        (Nothing, NamedByAddress typed) -> throwError (refused <> cs typed <> " is not one")
        (Nothing, NamedByPath {}) -> throwError (refused <> "it has none at " <> addressNamedText named)
  -- Before the transaction is built, not after
  for_ opts.output checkFileToWrite
  recipients <- liftEither $ mapM (readPayment net unit) opts.recipients
  heightM <- lockTimeHeight opts.yes net
  feeByte <- chooseFeeRate cfg net (storeOn net) 1 opts.feeRate
  confirmFeeRate opts.yes feeByte
  (control, left) <-
    if not controlled
      then return (NoCoinControl, [])
      else do
        spendable <- spendableCoinsOpening opts.spendFromTags pool opts.minConf
        unspendable <- unspendableCoinsOpening opts.spendFromTags pool opts.minConf
        closed <- tagLocks opts.spendFromTags net
        (chosen, held) <- liftEither $ chosenCoins coins spendFrom closed spendable unspendable
        refuseOtherType opts.coinType (chosen <> (fst <$> held))
        let outpoints = (.outpoint) <$> chosen
        return (if opts.select then SelectAmongChosen outpoints else SpendChosen outpoints, leftOutpoints [] held [])
  gen <- liftIO initStdGen
  let (lockTime, buildGen) = lockTimeFor heightM gen
  prepared <-
    buildTxSignData cfg buildGen opts.strategy pool opts.coinType recipients feeByte lockTime opts.feePayer opts.minConf opts.spendFromTags control changeTo
      `catchError` tellingHeldBack unit pool opts.minConf opts.spendFromTags coinsDoNotCover
  confirmPaymentMixing opts.yes unit pool opts.spendFromTags prepared.signData
  taggedPrepared opts.tags
    =<< changeTaggedPrepared opts.tagChange
    =<< keepPreparedLeaving pool prepared.changeChosen opts.spendFromTags opts.output (withLabel opts.label prepared.signData) left

-- | Fail as a payment failed, and, when it failed for the reason given,
-- say what the portfolio holds on the network that it cannot spend now.
--
-- "The coins do not cover the amount and the fee" was all the user read
-- when the portfolio held enough: a payment of somebody else that waits
-- for its block, a coinbase under a hundred blocks, coins locked by a
-- pending transaction, by the user or by a tag the payment did not name
-- (the tags given are those it named). The balance said one thing and the refusal
-- another. The sentence added gives the amount and points to coins, which
-- says why each coin waits.
--
-- The coins that a transaction already sent took have a sentence of
-- their own, which points to sync ('takenSentence').
tellingHeldBack :: AmountUnit -> Pool -> Natural -> [Tag] -> String -> String -> ExceptT String (DB IO) a
tellingHeldBack unit pool minConf opened reason failure
  | failure /= reason = throwError failure
  | otherwise = do
      (waiting, taken) <- notSpendableNow opened pool minConf
      let net = pool.poolNetwork
      throwError . intercalate ". " $
        failure : maybeToList (heldBackSentence unit net <$> waiting) <> maybeToList (takenSentence unit net <$> taken)

-- | What a portfolio holds on a network and cannot spend now, with the
-- command that says why
heldBackSentence :: AmountUnit -> Network -> HeldBack -> String
heldBackSentence unit net back =
  "The portfolio holds " <> coinsWorth unit back.heldBackValue <> " more on " <> net.name <> ", in " <> coinCount back <> " it cannot spend now: hw coins says why"

-- | What the transactions already sent took of a portfolio on a network,
-- with the command that brings what they left.
--
-- hw learns the change of a payment from the next sync, so a second
-- payment right after the first found no coin, and read only that the
-- portfolio held a coin it could not spend. The coin was spent, and a
-- sync was all it took. The change is promised only when one of the
-- transactions pays the portfolio.
takenSentence :: AmountUnit -> Network -> TakenBySent -> String
takenSentence unit net taken =
  (if one then "A transaction" else show taken.takenBy <> " transactions")
    <> (" sent from this wallet took " <> coinsWorth unit taken.takenCoins.heldBackValue <> " of the portfolio on " <> net.name)
    <> (", in " <> coinCount taken.takenCoins <> ": run hw sync to " <> next)
  where
    one = taken.takenBy == 1
    next
      | not taken.takenPaysBack = "bring the coins up to date"
      | one = "get its change"
      | otherwise = "get their change"

-- | An amount of satoshis with its unit, as a refusal writes it
coinsWorth :: AmountUnit -> Word64 -> String
coinsWorth unit value = cs (showAmount unit (toInteger value)) <> " " <> cs (showUnit unit (toInteger value))

-- | How many coins were counted: \"1 coin\", \"3 coins\"
coinCount :: HeldBack -> String
coinCount back = if back.heldBackCoins == 1 then "1 coin" else show back.heldBackCoins <> " coins"

-- | Prepare a payment of all the coins of the active portfolio on a
-- network, unsigned, and answer with it: @prepareSendAll@, which sendall
-- starts with too, says which coins are sent and how the options are
-- read.
cmdPrepareSendAll :: Config -> AmountUnit -> PrepareSendAll -> IO Response
cmdPrepareSendAll cfg unit opts =
  runDBResponse cfg $ do
    storeOn <- liftIO $ webStores cfg
    preparedResponse =<< prepareSendAll cfg storeOn unit opts

-- | Build a transaction sending the coins of the portfolio on a network
-- to the recipients, with no change, and keep it as a pending transaction
-- of the active portfolio, unsigned, which locks its coins. This is all
-- that preparesendall does but its answer, and what sendall starts with.
-- The two commands share it, as preparetx and send share
-- @preparePayment@, so they make the same transaction of the same
-- options, with the same questions and the same refusals. The stores are
-- those of the command. The fee rates of recent blocks are read from one
-- when the options give no rate.
--
-- The coins sent are all those the portfolio can spend now, of both
-- types, or those that the choosers of coin control chose, as for
-- preparetx. The filters narrow them down as for preparetx: --coin-type
-- sends the coins of one type. Without a chooser, the coins
-- worth less than their fee are spent or left as the user answers, or as
-- --spend-dust or --leave-dust says. The transaction prepared lists every
-- coin left in the portfolio on the network, and why. See
-- "Haskoin.Wallet.Tx.SendAll".
--
-- The user is asked before coins that do not all have the same tags are
-- sent together, as for preparetx ('confirmPaymentMixing').
--
-- --label and --tag say what the transaction is for, as for preparetx
-- ('taggedPrepared').
prepareSendAll :: Config -> (Network -> Store) -> AmountUnit -> PrepareSendAll -> ExceptT String (DB IO) Prepared
prepareSendAll cfg storeOn unit opts = do
  coins <- mapM walletCoinOf opts.coins
  (pool, namedAddrs, taggedAddrs) <- controlledPool cfg opts.network coins opts.spendFrom opts.spendFromTags
  let net = pool.poolNetwork
      spendFrom = chosenAddresses opts.coinType namedAddrs taggedAddrs
      controlled = not (null coins && null namedAddrs && null opts.spendFromTags)
  for_ opts.coinType $ liftEither . checkKindOnNetwork net
  -- Before the transaction is built, not after
  for_ opts.output checkFileToWrite
  recipients <- liftEither $ mapM (readRecipient net unit) opts.recipients
  liftEither $ checkSharing recipients
  when (controlled && isJust opts.dustCoins) $
    throwError "The coins chosen with --coin, --spend-from or --spend-from-tag are all sent: give --spend-dust or --leave-dust without them"
  heightM <- lockTimeHeight opts.yes net
  feeByte <- chooseFeeRate cfg net (storeOn net) 1 opts.feeRate
  confirmFeeRate opts.yes feeByte
  spendable <- spendableCoinsOpening opts.spendFromTags pool opts.minConf
  unspendable <- unspendableCoinsOpening opts.spendFromTags pool opts.minConf
  (sent, left) <-
    if controlled
      then do
        closed <- tagLocks opts.spendFromTags net
        (sent, held) <- liftEither $ chosenCoins coins spendFrom closed spendable unspendable
        refuseOtherType opts.coinType (sent <> (fst <$> held))
        return (sent, leftOutpoints [] unspendable (notSent sent spendable))
      else do
        -- The coins of the other type stay, whatever they are worth
        let (ofType, others) = partition (\coin -> maybe True (== coinKind coin) opts.coinType) spendable
        sent <- coinsWithDust opts.dustCoins feeByte recipients walletSized (askAboutDust pool feeByte recipients (not (null unspendable && null others))) ofType
        return (sent, leftOutpoints (notSent sent ofType) unspendable [] <> leftOfOtherType others)
  gen <- liftIO initStdGen
  let (lockTime, buildGen) = lockTimeFor heightM gen
  tsd <-
    buildSendAllSignData buildGen pool sent recipients feeByte lockTime
      `catchError` tellingHeldBack unit pool opts.minConf opts.spendFromTags noCoinsToSend
  confirmPaymentMixing opts.yes unit pool opts.spendFromTags tsd
  taggedPrepared opts.tags =<< keepPreparedLeaving pool Nothing opts.spendFromTags opts.output (withLabel opts.label tsd) left
  where
    -- The spendable coins the transaction does not send
    notSent sent = filter (`notElem` sent)
    -- The report on the dust coins of the portfolio and the question, on
    -- stderr, to keep the --json answer clean. Each coin is shown as coins
    -- shows it.
    askAboutDust :: Pool -> Natural -> [(Address, Share)] -> Bool -> [Store.Unspent] -> [Store.Unspent] -> ExceptT String (DB IO) Bool
    askAboutDust pool feeByte recipients othersStay worth dust = do
      let report = sendAllDustReport feeByte recipients worth dust othersStay
      views <- poolCoinViews pool ((.coin.outpoint) <$> report.dustCoins)
      liftIO $ renderOn stderr =<< dustReportDoc pool.poolNetwork unit views report
      askYes terminal (dustQuestion report)

-- | The coins that a command spends among those it would spend in full,
-- preparesendall or preparesweep: all of them, or all but the dust coins
-- ("Haskoin.Wallet.Tx.Dust"). --spend-dust and --leave-dust say which, and
-- the user is asked otherwise, once and for every dust coin: the command
-- gives what shows the report and asks, from the coins worth spending and
-- the dust coins, and any answer but yes leaves them. Nothing is asked
-- when no coin is dust, and when every coin is: the builder then tells that
-- the coins do not pay their fee. The dust coins are left without a
-- question when spending them too would leave too little for the
-- recipients, and when the other coins cannot pay the recipients either,
-- which the builder then tells. Each command sizes its coins its own way.
coinsWithDust ::
  (Monad m) =>
  -- | The answer of --spend-dust or --leave-dust
  Maybe DustCoins ->
  -- | The fee rate
  Natural ->
  [(Address, Share)] ->
  -- | A coin with the input that spends it
  (coin -> SizedCoin) ->
  -- | Show the report on the coins worth spending and the dust coins, and
  -- ask whether to spend the dust coins too
  ([coin] -> [coin] -> m Bool) ->
  [coin] ->
  m [coin]
coinsWithDust choice feeByte recipients sized ask coins
  | null dust || null worth = return coins
  | otherwise =
      case choice of
        Just SpendDust -> return coins
        Just LeaveDust -> return worth
        Nothing
          | isLeft (paying coins) || isLeft (paying worth) -> return worth
          | otherwise -> do
              spending <- ask worth dust
              return $ if spending then coins else worth
  where
    (dust, worth) = partition (isDustCoin feeByte . sized) coins
    paying sending = spendingAmounts feeByte (sized <$> sending) recipients

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
-- the network the file names and the labels it carries, and the pool of
-- the wallet it involves, if any, which hw finds by itself
-- ('transactionPool'). A pending transaction given by its nosigHash is as
-- a file that names no network and labels no address. A transaction
-- signed already, or sent, is not signed again, nor is one that spends a
-- coin the user locked
-- ("Haskoin.Wallet.Database.PendingTx".'refuseLockedCoins'), nor a file
-- that the signer of the spec refuses as a whole ('signerChecks'). The
-- command then refuses what its secret does not sign (@refuseForMnemonic@,
-- @refuseForPrivateKeys@) and settles the file to write
-- (@signedFileOf@), all before the user types a secret.
transactionToSign ::
  Config ->
  -- | The --network of the command, for a file that names none
  Maybe Network ->
  Signing ->
  ExceptT String (DB IO) (Maybe Pool, TxFileRead)
transactionToSign cfg givenM signing = do
  TxFileRead txFile@(TxFile netM tsd _ _) sourceM <- case (signing.nosigHash, signing.input) of
    (Nothing, Nothing) ->
      throwError "Give the NOSIGHASH of a pending transaction, or an --input file"
    (Just _, Just _) ->
      throwError "Can not specify both a NOSIGHASH and an --input file"
    (Just named, _) -> do
      held <- pendingTxNamed cfg named
      when held.pendingTxOnline $
        throwError "The transaction was sent already: there is nothing left to sign"
      return (heldTxRead held)
    (_, Just file) -> readTxFileIn (commandNetwork cfg givenM) file
  when (isSigned (txSignDataTx tsd)) $
    throwError "The transaction is already signed"
  named <- fileNetwork givenM netM
  -- The pool found is the one that pays, if any: the keys of a portfolio
  -- that does not pay the transaction sign none of its inputs
  activeM <- lift activePortfolioId
  (poolM, completed) <- transactionPool cfg activeM named tsd
  -- A transaction of no pool spends no coin of the wallet
  forM_ poolM $ \pool -> refuseLockedCoins pool.poolNetwork (txSignDataTx tsd)
  -- A PSBT is checked on the network it is signed on: that of its pool,
  -- or else the one named, or the default
  forM_ sourceM $ liftEither . signerChecks (maybe (commandNetwork cfg named) (.poolNetwork) poolM)
  return (poolM, TxFileRead txFile {txFileData = completed} sourceM)

-- | The file a command writes its signed transaction to: the --output
-- file, which must be free, or the --input file, which is replaced only
-- if the user agrees. Asked once nothing else can refuse the
-- transaction, and before the user types a secret.
signedFileOf :: Signing -> ExceptT String (DB IO) SignedFile
signedFileOf signing = case (signing.input, signing.output) of
  (_, Just file) -> NewFile file <$ checkFileToWrite file
  (Just file, Nothing) -> do
    replacing <- askYes terminal ("Replace " <> file <> " with the signed transaction? Give an --output file to write it elsewhere.")
    unless replacing $ throwError "The transaction was not signed: give an --output file to write it to"
    return (ReplacedFile file)
  (Nothing, Nothing) -> return NoSignedFile

-- | The transaction to sign as the user named it, for a refusal to give
-- the command that signs it: its hash as typed, or its --input file
signingNamed :: Signing -> String
signingNamed signing = case signing.input of
  Just file -> "--input " <> file
  Nothing -> maybe "NOSIGHASH" hashRefText signing.nosigHash

-- | Refuse a transaction of a pool that the mnemonic does not sign,
-- before signtx asks for anything. The mnemonic signs the coins of the
-- wallet. It signs nothing of a transaction that spends none of them: a
-- sweep, which signsweep signs with the private keys of its coins. It
-- cannot finish one with an input from outside the wallet that nobody
-- signed ('outsideUnsigned').
--
-- pendingtxs lists payments and sweeps together, so the wrong command is
-- an easy mistake. Before, signtx asked for the mnemonic of a sweep and
-- then failed: a secret typed for nothing is a secret exposed for
-- nothing.
refuseForMnemonic :: Signing -> Pool -> TxSignData -> ExceptT String (DB IO) ()
refuseForMnemonic signing pool tsd = do
  (summary, outside) <- inputsOutside pool tsd
  unless (paysFrom summary) $
    throwError $
      "The transaction spends no coin of this wallet, so the mnemonic signs nothing in it. \
      \The private keys of its coins sign such a transaction, a sweep: hw signsweep "
        <> signingNamed signing
  forM_ (listToMaybe [(i, coin) | (i, coin, False) <- outside]) $
    throwError . uncurry (outsideUnsigned pool.poolNetwork)

-- | Refuse a transaction of a pool that private keys do not sign, before
-- signsweep asks for anything. The user holds no private key of a coin of
-- the wallet, so a transaction that spends coins of the wallet alone, a
-- payment, is left to signtx. Before, signsweep asked for the keys of a
-- payment and then named a missing key. A transaction that also spends
-- coins from outside the wallet is read on: an older hw prepared such
-- sweeps.
refuseForPrivateKeys :: Signing -> Pool -> TxSignData -> ExceptT String (DB IO) ()
refuseForPrivateKeys signing pool tsd = do
  (_, outside) <- inputsOutside pool tsd
  when (null outside) $
    throwError $
      "Every coin the transaction spends is of this wallet: the mnemonic signs them, not private keys. \
      \Sign the transaction with hw signtx "
        <> signingNamed signing

-- | The summary of a transaction as its pool sees it, and its inputs
-- that spend no coin of the pool ('outsideInputs')
inputsOutside :: Pool -> TxSignData -> ExceptT String (DB IO) (TxSummary, [(Int, SpentCoin, Bool)])
inputsOutside pool tsd = do
  keys <- lift $ poolKeys pool
  liftEither $ do
    summary <- txInfoSummary <$> parseTxSignData pool.poolNetwork keys tsd
    (summary,) <$> outsideInputs pool.poolNetwork summary tsd

-- | Show the transaction about to be signed, before the secret is asked
-- for, as show shows it: with the wallet's labels for a pending
-- transaction, those of the file for a file, and either way with the
-- addresses another portfolio of the database holds named as such, and
-- with the time the wallet created it when it holds it. On stderr, as the
-- questions, to keep the --json answer clean.
showToSign :: AmountUnit -> Signing -> Network -> Maybe Pool -> TxFile -> ExceptT String (DB IO) ()
showToSign unit signing net poolM txFile = do
  let tsd = txFileData txFile
  (info, seenM) <- case poolM of
    Just pool -> do
      keys <- lift $ poolKeys pool
      parsed <- liftEither $ parseTxSignData net keys tsd
      filled <-
        lift $
          if isJust signing.nosigHash
            then fillTxInfo pool parsed
            else fillAddresses pool (labelsFromFile txFile parsed)
      -- A transaction the pool holds is shown with what the database
      -- knows of it, the time it was created first, as pendingtxs shows it
      heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
      let info = case heldM of
            Just held | held.pendingTxPool == pool -> filled {txInfoStatus = heldStatus held filled.txInfoStatus}
            _ -> filled
      seen <- seenFromPlace =<< poolPlace pool
      return (info, Just seen)
    Nothing -> (,Nothing) <$> (lift . fillContacts net =<< liftEither (parseOutsideTxSignData net tsd))
  naming <- lift walletNaming
  doc <- liftIO $ txAnswerDoc naming net seenM unit info
  liftIO $ renderOn stderr doc

-- | Write a transaction just signed to its file, if any, with the keys of
-- its pool when it has one: into the PSBT the --input file held, which
-- keeps every field of it, or as a new PSBT for a transaction that came
-- from the wallet
writeSigned :: Maybe Pool -> Maybe Psbt -> SignedFile -> TxFile -> ExceptT String (DB IO) ()
writeSigned poolM sourceM signedFile signed =
  case signedFile of
    NoSignedFile -> return ()
    NewFile file -> write (writeFileBytes file)
    ReplacedFile file -> write (replaceFileBytes file)
  where
    write put = do
      accounts <- maybe (return []) poolFileAccounts poolM
      psbt <- liftEither $ case sourceM of
        Just source -> signedPsbt accounts source signed
        Nothing -> txFilePsbt accounts signed
      liftEitherIO $ put (encodePsbt psbt)

-- | Keep a transaction just signed: in place of the pending one when it was
-- given by its nosigHash, and in its file when there is one. It is given
-- back with its labels, as the answer shows it: the wallet's for a pending
-- transaction, and those the --input file carries for a transaction of a
-- file, which go on to the signed file. The file is what an offline
-- computer has.
keepSigned :: Signing -> SignedFile -> Pool -> TxFileRead -> TxSignData -> TxInfo -> ExceptT String (DB IO) TxInfo
keepSigned signing signedFile pool (TxFileRead file sourceM) signed info = do
  labelled <-
    lift $
      if fromWallet
        then fillTxInfo pool info
        else fillAddresses pool (labelsFromFile file info)
  when fromWallet $ void $ importPendingTx pool signed
  writeSigned (Just pool) sourceM signedFile (labelledTxFile pool.poolNetwork labelled signed)
  return labelled
  where
    fromWallet = isJust signing.nosigHash

-- | Sign a transaction of a portfolio with the keys of its mnemonic, or of
-- the shares of its backup, and answer with it: @signWalletTx@, which
-- send and sendall sign with too. The transaction is shown first, as show shows it.
cmdSignTx :: Config -> AmountUnit -> Signing -> IO Response
cmdSignTx cfg unit signing =
  runDBResponse cfg $ do
    (pool, signed) <- signWalletTx cfg signing $ \pool -> showToSign unit signing pool.poolNetwork (Just pool)
    txAnswer pool signed

-- | Sign a transaction of a portfolio with the keys of its mnemonic, or of
-- the shares of its backup, and keep it signed ('keepSigned'): all that
-- signtx does but its answer, and the second step of send and of
-- sendall. The keys are those of every account the portfolio has on the
-- network of the transaction, since a payment may spend the coins of
-- both. The pool of the transaction is given back with the transaction as
-- signed.
--
-- The transaction is shown first, by the action given, so that the user
-- sees what the mnemonic is typed for: the prompt is the confirmation,
-- and an interrupt stops it. The command says how it shows it. Every
-- refusal comes before that, and so before the user types a secret
-- ('transactionToSign', @refuseForMnemonic@).
signWalletTx ::
  Config ->
  Signing ->
  -- | Show the transaction to sign, given its pool and as its file reads
  (Pool -> TxFile -> ExceptT String (DB IO) ()) ->
  ExceptT String (DB IO) (Pool, TxInfo)
signWalletTx cfg signing showFirst = do
  (poolM, file@(TxFileRead txFile _)) <- transactionToSign cfg Nothing signing
  -- A file that involves no portfolio may be of a portfolio the database
  -- lacks, or a sweep
  pool <- liftMaybe (notInWallet <> ". The private keys of its coins sign a sweep: hw signsweep " <> signingNamed signing) poolM
  refuseForMnemonic signing pool (txFileData txFile)
  signedFile <- signedFileOf signing
  let net = pool.poolNetwork
  portfolio <- getPortfolio pool.poolPortfolio
  accounts <- lift $ poolAccounts pool
  showFirst pool txFile
  keysOfPool <- lift $ poolKeys pool
  (seed, keys) <- askSigningKeys terminal net (portfolioIndex portfolio.portfolioRow) keysOfPool
  -- The signed data names the seed, for the derivation records of its
  -- file
  let named = (txFileData txFile) {txSignDataFingerprint = Just (seedFingerprint seed)}
  (signed, info) <- liftEither $ signHeldWalletTx net (heldPoolOf (snd <$> accounts)) named keys
  (pool,) <$> keepSigned signing signedFile pool file signed info

-- | Send a signed transaction: a pending one of the wallet, by its id or
-- its nosigHash, or the one of a file. One the wallet holds goes on the
-- network of its portfolio, which the file or --network may only name; one
-- it does not hold goes on the network its file names, or on that of
-- --network or of the configuration when the file names none. See
-- 'sendSigned'. The user sees the transaction and says whether to send
-- it, unless --yes.
cmdBroadcast :: Config -> AmountUnit -> Maybe HashRef -> Maybe FilePath -> Bool -> Maybe Network -> IO Response
cmdBroadcast cfg unit namedM fileM yes givenM =
  case (namedM, fileM) of
    (Just named, Nothing) -> sending (heldTxRead <$> pendingToSend named)
    (Nothing, Just file) -> sending (readTxFileIn (commandNetwork cfg givenM) file)
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
        storeOn <- liftIO $ webStores cfg
        sendTxRead cfg storeOn unit yes givenM =<< readTx

-- | Send a signed transaction, read from the wallet or from a file, and
-- answer with it as sent: all that broadcast does once it has read its
-- transaction, and the last step of send and of sendall. See 'sendSigned'
-- for what is sent where. The user sees the transaction and says whether
-- to send it, unless the command says not to ask. broadcast says so with
-- --yes. send and sendall always do: they showed the transaction before
-- they asked for the mnemonic, and typing the mnemonic was the answer.
--
-- The labels of a file are left aside: the transaction is shown as the
-- wallet holds it.
sendTxRead ::
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
sendTxRead cfg storeOn unit yes givenM (TxFileRead (TxFile netM tsd _ _) _) = do
  named <- fileNetwork givenM netM
  -- A file of a hidden network, or of a transaction pending on one, is
  -- refused, as the pending transaction named by its hash is
  heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
  _ <- shownAmong cfg "The transaction" id (maybeToList named <> [held.pendingTxPool.poolNetwork | Just held <- [heldM]])
  activeM <- lift activePortfolioId
  sent <- sendSigned cfg storeOn beforeSending activeM named tsd
  case sent of
    SentPending place info -> do
      after <- syncAfterSending cfg storeOn (placeNetwork place) (txSignDataTx tsd)
      ResponseSentPending place info after <$> activePortfolioRef
    SentOutside net txid -> ResponseSentTx net txid <$> syncAfterSending cfg storeOn net (txSignDataTx tsd)
  where
    -- On stderr, with the question, to keep the --json answer clean
    beforeSending net placeM info =
      unless yes $ do
        seenM <- mapM seenFromPlace placeM
        naming <- lift walletNaming
        liftIO $ renderOn stderr =<< txAnswerDoc naming net seenM unit info
        confirmed <- askYes terminal "Send this transaction to the network? This cannot be undone."
        unless confirmed $ throwError "The transaction was not sent"

-- | Sync the portfolios that a transaction just sent touches, on its
-- network ('touchedPools'): the last step of every command that sends.
-- The type t'SyncAfter' says why.
--
-- The store lists the transaction by now, which is what the sending
-- waited for. A haskoin-store writes a transaction with the coins and
-- the balances it changes in one step, so this sync finds the change.
-- That was checked on the regtest chains before this was built: at the
-- moment the store first listed a payment, it gave its change as a coin
-- and no longer gave the coin spent, for 40 payments of 40 on Bitcoin
-- and as many on Bitcoin Cash.
--
-- What the wallet recorded of the sending is saved first ('keepSoFar'):
-- the transaction left the computer, so it stays marked as sent
-- whatever comes of the sync. A sync that fails is undone alone, and its
-- failure is a part of the answer. The command does not fail for it,
-- since it did what it was asked: the payment is sent, and a failure
-- here would tell a script, and the user, that it is not.
syncAfterSending :: Config -> (Network -> Store) -> Network -> Tx -> ExceptT String (DB IO) SyncAfter
syncAfterSending cfg storeOn net tx = do
  keepSoFar
  outcome <- lift . runExceptT . undoneIfRefused $ do
    pools <- touchedPools net tx
    if null pools then return Nothing else Just <$> onStatusLine (syncPools cfg storeOn pools Incremental . terminalWatcher)
  return $ case outcome of
    Left failure -> SyncAfter [] (Just (cs failure))
    Right Nothing -> nothingToSyncAfter
    Right (Just synced) -> SyncAfter (syncedPools synced) (cs . snd <$> listToMaybe (syncedDown synced))

-- | Run a sync with the status line of the terminal, which is gone once
-- the sync ends ('withStatusLine')
onStatusLine :: (StatusLine -> ExceptT String (DB IO) a) -> ExceptT String (DB IO) a
onStatusLine action = ExceptT $ withRunInIO (\run -> withStatusLine (\line -> run (runExceptT (action line))))

-- | What send and sendall tell the user after the transaction and before
-- the mnemonic. signtx signs and stops, and broadcast asks before it
-- sends. send and sendall ask nothing after the mnemonic, so the user must
-- know by then that typing it sends the payment.
sendNotice :: String
sendNotice = "Typing the mnemonic signs this transaction and sends it to the network. This cannot be undone."

-- | Make a payment in one command: prepare it as preparetx does, sign it
-- as signtx does, and send it as broadcast does (@prepareSignSend@). The
-- first step is that of preparetx (@preparePayment@), so the payment is
-- the one the three commands make of the same options. The command takes
-- the options of preparetx, without an --output file.
cmdSend :: Config -> AmountUnit -> PrepareTx -> IO Response
cmdSend cfg unit opts =
  prepareSignSend cfg unit $ \storeOn -> preparePayment cfg storeOn unit opts

-- | Send all the coins of the active portfolio in one command: prepare
-- the transaction as preparesendall does, sign it as signtx does, and
-- send it as broadcast does (@prepareSignSend@). The first step is that
-- of preparesendall (@prepareSendAll@), so the coins sent and what each
-- recipient gets are those the three commands make of the same options.
-- The command takes the options of preparesendall, without an --output
-- file. It is to preparesendall what send is to preparetx, and differs
-- from send in nothing but the transaction it prepares.
cmdSendAll :: Config -> AmountUnit -> PrepareSendAll -> IO Response
cmdSendAll cfg unit opts =
  prepareSignSend cfg unit $ \storeOn -> prepareSendAll cfg storeOn unit opts

-- | Prepare a transaction, sign it as signtx does and send it as broadcast
-- does, in one command: all that send and sendall do. They differ only in
-- how they prepare the transaction, which is the action given: that of
-- preparetx for send, and that of preparesendall for sendall. The action
-- is handed the stores of the command, and leaves the transaction pending
-- and unsigned, as its command does. The other two steps are those of
-- signtx and broadcast (@signWalletTx@ and @sendJustSigned@), so the
-- transaction and what the wallet holds of it afterwards are the same as
-- with the three commands run one after the other.
--
-- Such a command is for a wallet whose mnemonic is typed on the online
-- computer anyway. It asks for the mnemonic and needs the store, so it
-- cannot keep the mnemonic offline. The three commands stay for that.
--
-- The transaction is shown once, before the mnemonic is asked, as the
-- command that prepares it answers with it: with the coins it leaves in
-- the portfolio. Nothing of it changes between then and the sending, so
-- it is not shown again, and the answer on the screen is its txid alone.
-- Typing the mnemonic is the confirmation. broadcast asks whether to
-- send, and these commands do not: the user saw the transaction, was told
-- that the mnemonic sends it ('sendNotice'), and typed it.
--
-- What a failure leaves depends on whether the transaction was signed:
--
-- * Before the signature, nothing is kept. A refusal, a no to a question
--   and a mnemonic not typed all undo the pending transaction. The user
--   did not confirm the payment, and an unsigned transaction left behind
--   would lock its coins until a deletetx.
--
-- * After the signature, the signed transaction is kept, as signtx leaves
--   it (@sendJustSigned@). It is in the wallet before it is handed to the
--   network. A sending that fails, or that is interrupted while the store
--   waits for its nodes, may have reached the network all the same. The
--   coins must then stay locked, and broadcast must be able to send the
--   transaction again. The failure says so, and names the transaction.
--
-- The store is asked whether it is well before the mnemonic is asked, so
-- that a store that is down does not cost the user a mnemonic typed for
-- nothing.
prepareSignSend ::
  Config ->
  AmountUnit ->
  -- | Prepare the transaction, with the stores of the command
  ((Network -> Store) -> ExceptT String (DB IO) Prepared) ->
  IO Response
prepareSignSend cfg unit prepare =
  runDBResponse cfg $ do
    storeOn <- liftIO $ webStores cfg
    -- As preparetx or preparesendall: the transaction is pending, and its
    -- coins are locked
    prepared <- prepare storeOn
    -- A store that is down stops the command here, before the mnemonic
    fromStore $ checkHealth (storeOn prepared.preparedPool.poolNetwork)
    -- As signtx NOSIGHASH
    _ <- signWalletTx cfg (signingPending prepared.preparedHash) $ \_ _ -> showBeforeMnemonic prepared
    -- As broadcast TXID --yes
    sendJustSigned cfg storeOn unit prepared.preparedHash
  where
    -- The transaction as the command that prepares it answers with it,
    -- then, after a blank line, what typing the mnemonic does. On stderr,
    -- as the questions, to keep the --json answer clean.
    showBeforeMnemonic :: Prepared -> ExceptT String (DB IO) ()
    showBeforeMnemonic prepared = do
      place <- poolPlace prepared.preparedPool
      activeM <- activePortfolioRef
      naming <- lift walletNaming
      liftIO $ renderOn stderr =<< preparedDoc naming unit place prepared.preparedInfo prepared.preparedLeft activeM
      liftIO $ tell terminal ("\n" <> sendNotice)

-- | Send a pending transaction that the command just signed, as broadcast
-- TXID --yes does: the last step of send, of sendall, and of a sweep that
-- pays a portfolio. The user agreed to the sending before the signature,
-- so nothing is shown or asked here.
--
-- The signed transaction is saved first (@keepSoFar@): it is in the
-- wallet from here on, whatever comes of the sending. A sending that
-- fails says why, then what the wallet holds and what to do with it. The
-- user gave the secret and reads a failure, so the failure says that the
-- signature is not lost. The transaction is named by the short id of its
-- txid, as the next steps name one.
sendJustSigned :: Config -> (Network -> Store) -> AmountUnit -> TxHash -> ExceptT String (DB IO) Response
sendJustSigned cfg storeOn unit pendingHash = do
  keepSoFar
  signed <- requirePendingTx pendingHash
  sendTxRead cfg storeOn unit True Nothing (heldTxRead signed) `catchError` (throwError . withWhatIsKept signed)
  where
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
-- after. The last question is about the coins added: whether they are of
-- another group than the coins the transaction spends already, by their
-- tags (@confirmMixing@). The answer is that of preparetx.
cmdBumpFee :: Config -> AmountUnit -> BumpFee -> IO Response
cmdBumpFee cfg unit opts =
  runDBResponse cfg $ do
    -- Before anything is asked, not after
    for_ opts.output checkFileToWrite
    bumped <- walletTxHashOf opts.bumped
    pool <- bumpedPool bumped
    storeOn <- liftIO $ webStores cfg
    let net = pool.poolNetwork
        store = storeOn net
    unless net.replaceByFee $
      throwError "Bitcoin Cash replaces no transaction: its nodes keep the first one they see"
    stuck <- stuckTransaction store pool bumped
    liftEither $ checkReplaceable stuck
    case opts.feeRate of
      Just (FeeRateOf _) -> return ()
      _ ->
        liftIO . tell terminal $
          "The transaction pays " <> paidRateText (summaryFeeRate stuck.sentSummary)
            <> " satoshis per virtual byte: a replacement must pay at least "
            <> show (minimumRate stuck)
    feeByte <- chooseFeeRate cfg net store (minimumRate stuck) opts.feeRate
    when (feeByte < minimumRate stuck) $
      throwError $
        "The transaction pays " <> paidRateText (summaryFeeRate stuck.sentSummary)
          <> " sat/vB: give a --fee-rate of at least "
          <> show (minimumRate stuck)
    children <- spendingChildren store pool [stuck]
    refuseSpentElsewhere pool (stuck : children)
    plan <- bumpPlan cfg pool (stuck : children)
    -- A wallet behind the chain is asked about with the other questions,
    -- once nothing can refuse the replacement: it is built for a lock time
    -- of 0 meanwhile
    behind <- behindTheChain net
    gen <- liftIO initStdGen
    let (lockTime, buildGen) = lockTimeFor (either (const Nothing) id behind) gen
    -- The network takes no replacement that adds an unconfirmed coin
    confirmed <- spendableCoins pool 1
    candidates <- forM confirmed $ \coin -> (\(addr, path) -> (coin, addr, path)) <$> coinWithPath pool coin
    offered <- liftEither $ fundings buildGen plan candidates feeByte opts.recipientsPay
    built <- forM offered $ \funding -> do
      tsd <- buildBump cfg pool buildGen plan funding feeByte lockTime
      info <- txInfoOf pool tsd
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
    confirmMixing opts.yes unit pool (addsGroup (addedCoins plan tsd)) "Replace the transaction anyway?" "The transaction was not replaced" tsd
    inheritTags pool tsd
    preparedAnswer pool Nothing opts.output tsd []
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

-- | Give the replacement bumpfee prepared the tags of the transactions
-- it replaces, as it gets their labels: it is the same payment, and the
-- user who tagged it rent finds it under rent whichever of the two the
-- network keeps. A replaced transaction on the network is tagged under
-- its id, and one still pending under its nosigHash. The replacement is
-- pending, so it takes them under its own nosigHash.
inheritTags :: Pool -> TxSignData -> ExceptT String (DB IO) ()
inheritTags pool tsd = do
  let replacement = nosigHash (txSignDataTx tsd)
  forM_ tsd.txSignDataReplaces $ \replaced -> do
    pendingM <- findPendingTx replaced
    let taggedUnder = replaced : [nosigHash (txSignDataTx held.pendingTxData) | Just held <- [pendingM]]
    lift $ forM_ taggedUnder $ \hash -> copyTxTags pool hash pool replacement

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
      let coinOffers = [(added, tsd) | (CoinsAdded added, tsd) <- built]
      fromCoins <- offerCoins coinOffers
      -- Why no coin was offered, when none was
      held <- if null coinOffers then heldBackFrom (snd <$> built) else return Nothing
      fromRecipients <- maybe (offerReductions held [(way, tsd) | (RecipientsReduced way, tsd) <- built]) (return . Just) fromCoins
      liftMaybe "The transaction was not replaced" fromRecipients
  where
    -- What the portfolio holds and could not add, since it cannot spend
    -- it now: a coin that a tag or the user locks, or one without a
    -- block yet, which the network takes in no replacement. The coins of
    -- the transactions replaced are not counted, neither those they
    -- spend nor those they made: they are the coins of the replacement
    -- itself.
    heldBackFrom [] = return Nothing
    heldBackFrom (tsd : _) = do
      let own = (.outpoint) <$> (txSignDataTx tsd).inputs
          replaced = tsd.txSignDataReplaces
      waiting <- unspendableCoins pool 1
      return $ heldBackOf [coin | (coin, _) <- waiting, coin.outpoint `notElem` own, coin.outpoint.hash `notElem` replaced]
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
    offerReductions _ [] = return Nothing
    -- The one way --recipients-pay leaves: nothing to choose
    offerReductions held ((way, tsd) : _)
      | payer == RecipientsPay = takeShares held way tsd
    offerReductions held ways@((way, tsd) : _) = case way.reductionRecipients of
      [_] -> do
        liftIO $ renderOn stderr (reducedDoc net unit feeByte held (fst <$> ways))
        agreed <- agreedTo (reducedQuestion (fst <$> ways))
        return $ if agreed then Just tsd else Nothing
      _
        | yes, (shared, sharedTsd) : _ <- [(w, t) | (w, t) <- ways, w.reductionPaidBy == AllRecipients] ->
            takeShares held shared sharedTsd
        | otherwise -> do
            liftIO $ renderOn stderr (reducedDoc net unit feeByte held (fst <$> ways))
            -- No is the default: an empty answer keeps the transaction
            askChoice terminal (reducedQuestion (fst <$> ways)) ((["no", "n"], Nothing) : [(reductionAnswers net w, Just t) | (w, t) <- ways])
    -- All the recipients paying in equal shares, told and taken without a
    -- question, as --yes or --recipients-pay says
    takeShares held way tsd = do
      liftIO $ renderOn stderr (sharedDoc payer net unit feeByte held way)
      return (Just tsd)

{- Sweeps -}

-- | Sweep coins to the destinations: the coins at the given addresses,
-- typed or in a file, at the addresses of the given private keys, and the
-- coins given by outpoint. The keys are not needed here, only to sign: a
-- sweep can be prepared from addresses or coins alone. A sweep that pays
-- an address of the wallet is of the portfolio of that address, on its
-- network, and waits among its pending transactions to be signed by its
-- nosigHash (@sweepPlace@). One that pays no address of the wallet
-- involves no portfolio: hw is then a utility, which writes the sweep to
-- a file, on the --network given, to be signed from there. A destination
-- is an address, or an address and its amount, as for preparesendall. The
-- coins worth less than their fee are swept or left as the user answers,
-- or as --spend-dust or --leave-dust says, as for preparesendall too
-- (@coinsWithDust@). A sweep from an address of the wallet is refused
-- (@refuseOwnAddresses@).
cmdPrepareSweep :: Config -> AmountUnit -> PrepareSweep -> IO Response
cmdPrepareSweep cfg unit opts =
  runDBResponse cfg $ do
    -- Before the keys are typed, not after
    for_ opts.output checkFileToWrite
    (net, poolM) <- sweepPlace cfg opts
    when (isNothing poolM && isNothing opts.output) $
      throwError "The sweep does not involve this wallet: give an --output file to write it to"
    storeOn <- liftIO $ webStores cfg
    built <- buildSweep cfg (storeOn net) unit opts opts.keys net poolM
    info <- keepBuiltSweep net poolM opts.output built
    placeM <- mapM poolPlace poolM
    activeM <- if isJust poolM then activePortfolioRef else return Nothing
    return $ ResponseSweep placeM net info built.builtEmpty built.builtUnswept activeM

-- | Sweep the coins of private keys in one command: prepare the sweep as
-- preparesweep does, sign it as signsweep does, and send it as broadcast
-- does. Each step is that of its command (@buildSweep@ and
-- @keepBuiltSweep@, @signPoolSweep@ or 'signTxWithKeys', @sendTxRead@), so
-- the sweep and what the wallet holds of it afterwards are the same as
-- with the three commands run one after the other.
--
-- The command is to a sweep what send is to a payment
-- (@prepareSignSend@), and is for keys the user is willing to give to an
-- online computer. It differs from send in when it asks. send shows the
-- payment, and the mnemonic typed after it is the confirmation. Here the
-- keys come first: they say which coins the sweep takes, so there is no
-- sweep to show before them. The sweep is shown once they are read, as
-- preparesweep answers with it, and a question takes the place of the
-- confirmation, unless --yes. The sweep is shown with --yes too: the
-- answer is its txid alone, and the dust coins a sweep leaves must not
-- go unsaid.
--
-- Every coin swept is of a key given: the options name no address and no
-- coin, which preparesweep takes for a sweep signed elsewhere.
--
-- What a failure leaves is what it leaves of a payment of send. Before
-- the signature, nothing is kept: a refusal and a no undo the pending
-- sweep. A sweep that pays a portfolio is kept signed before it is handed
-- to the network (@sendJustSigned@). One that pays no portfolio is never
-- kept, as with the three commands, which hold it in files alone: the
-- user has the keys, and runs the command again.
--
-- The store is asked whether it is well before the keys are read
-- (@buildSweep@).
cmdSweep :: Config -> AmountUnit -> PrepareSweep -> IO Response
cmdSweep cfg unit opts =
  runDBResponse cfg $ do
    (net, poolM) <- sweepPlace cfg opts
    storeOn <- liftIO $ webStores cfg
    -- As preparesweep --key-file: pending and unsigned in the portfolio
    -- it pays. Without a file, the keys are typed, as for signsweep.
    built <- buildSweep cfg (storeOn net) unit opts (opts.keys <|> Just KeysOnTerminal) net poolM
    info <- keepBuiltSweep net poolM Nothing built
    confirm net poolM info built
    case poolM of
      Just pool -> do
        -- As signsweep NOSIGHASH
        let pendingHash = nosigHash (txSignDataTx built.builtSweep)
            signing = signingPending pendingHash
        (_, file) <- transactionToSign cfg Nothing signing
        _ <- signPoolSweep signing NoSignedFile pool file built.builtKeys
        -- As broadcast TXID --yes
        sendJustSigned cfg storeOn unit pendingHash
      Nothing -> do
        -- As signsweep --input and broadcast --input --yes, without the
        -- files
        (signed, _) <- liftEither $ signTxWithKeys net built.builtSweep [] noneHeld built.builtKeys
        sendTxRead cfg storeOn unit True (Just net) (TxFileRead (plainTxFile (Just net) signed) Nothing)
          `catchError` (throwError . (<> ". The wallet keeps nothing of a sweep that pays none of its portfolios: run sweep again to send it"))
  where
    -- The sweep as preparesweep answers with it, then the question. On
    -- stderr, as every question, to keep the --json answer clean.
    confirm :: Network -> Maybe Pool -> TxInfo -> BuiltSweep -> ExceptT String (DB IO) ()
    confirm net poolM info built = do
      seenM <- mapM (seenFromPlace <=< poolPlace) poolM
      naming <- lift walletNaming
      liftIO $ renderOn stderr =<< sweepDoc naming unit seenM net info built.builtEmpty built.builtUnswept
      unless opts.yes $ do
        liftIO $ tell terminal ""
        agreed <- askYes terminal "Sign this sweep and send it to the network? This cannot be undone."
        unless agreed $ throwError "The sweep was not signed, and nothing was sent"

-- | The network of a sweep, and the portfolio it pays, if any. A sweep
-- that pays an address of the wallet is of the portfolio of that address,
-- on its network: hw finds it from the destinations, and takes the active
-- portfolio when they are of several (@poolAmong@). Destinations of the
-- wallet on two networks are refused, since a sweep is on one network. A
-- sweep that pays no address of the wallet is of no portfolio, and its
-- --network must be given: the default network of the configuration does
-- not apply. The keys, the coins and the legacy addresses of Bitcoin Cash
-- read the same on Bitcoin, so a wrong guess would sweep the coins of the
-- other chain.
sweepPlace :: Config -> PrepareSweep -> ExceptT String (DB IO) (Network, Maybe Pool)
sweepPlace cfg opts = do
  -- The portfolio of a destination is found by its address, an amount
  -- after it or not, before the network to read the amounts on is known
  everyHolder <- accountsHolding (\net -> mapMaybe (recipientAddress net) opts.sweepTo)
  -- The destinations the wallet has on hidden networks alone are
  -- refused: read as outside the wallet, the sweep would pay an account
  -- of it that no answer shows
  holders <- shownAmong cfg "The sweep pays an address of the wallet that" (\(_, _, acc) -> accountNetwork acc) everyHolder
  case nub [accountPool acc | (_, _, acc) <- holders] of
    [] -> do
      -- A destination that reads on no network is a typo, of an address
      -- of the wallet perhaps: said here, before a --network and an
      -- --output are asked for a sweep that was never meant to leave it
      when (isNothing opts.network) $ liftEither (mapM_ (checkAddressReads allNets) opts.sweepTo)
      net <-
        liftMaybe
          "Give the --network of a sweep that pays no portfolio of the wallet: the default-network of the configuration file does not apply to it"
          opts.network
      return (net, Nothing)
    paid -> do
      -- The addresses of the wallet it pays tell its portfolio and its
      -- network
      networks <- sweepNetworks
      pool <- poolAmong opts.network "The destinations of the sweep" [p | p <- paid, p.poolNetwork `elem` networks]
      return (pool.poolNetwork, Just pool)
  where
    -- The networks a sweep that pays the wallet can be on: those that hold
    -- every destination the wallet holds. A sweep is on one network, and
    -- the destinations of the wallet on two networks are refused. The
    -- default network must not choose between them: a Bitcoin Cash
    -- address in its legacy form reads on Bitcoin too, and the sweep would
    -- pay it there, where no account of the wallet watches it. Test
    -- networks that share their keys hold the same addresses, and
    -- @poolAmong@ takes the one of --network, or asks for it.
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

-- | A sweep just built, not kept yet
data BuiltSweep = BuiltSweep
  { -- | The sweep, unsigned
    builtSweep :: !TxSignData,
    -- | The private keys read, if the command read any
    builtKeys :: ![SecKey],
    -- | The addresses given to sweep from that hold no coin
    -- ('addressesWithoutCoin')
    builtEmpty :: ![Address],
    -- | The dust coins the sweep leaves at their addresses
    builtUnswept :: ![UnsweptCoin]
  }

-- | Build a sweep on its network from the options, for the pool it pays,
-- if any: the first step of preparesweep and of sweep. The keys are read
-- from the source given, if any: preparesweep can do without them, and
-- sweep cannot.
--
-- Every refusal that needs no coin comes before the keys are read, and so
-- does the question about a high fee rate. The store is asked whether it
-- is well before both: a store that is down must not cost the user keys
-- typed for nothing, and the advice of the fee rates, to give a
-- --fee-rate, does not help a sweep, whose coins come from the store.
-- The question about the dust coins comes after the keys, which tell the
-- coins.
buildSweep :: Config -> Store -> AmountUnit -> PrepareSweep -> Maybe KeySource -> Network -> Maybe Pool -> ExceptT String (DB IO) BuiltSweep
buildSweep cfg store unit opts keySource net poolM = do
  typed <- liftEither $ mapM sweptAddress opts.sweepFrom
  listed <- maybe (return []) addressesOfFile opts.addressFile
  sweepTo <- liftEither $ mapM (readRecipient net unit) opts.sweepTo
  liftEither $ checkSharing sweepTo
  refuseOwnAddresses net (typed <> listed)
  -- A store that is down stops the command here: before the fee rate,
  -- which a --fee-rate could give without it, and before the keys
  fromStore $ withExceptT (<> ". A sweep reads its coins from the server, and cannot be prepared without it") (checkHealth store)
  feeByte <- chooseFeeRate cfg net store 1 opts.feeRate
  confirmFeeRate opts.yes feeByte
  keys <- maybe (return []) (readSecKeys terminal net anyKeys) keySource
  found <- fetchSweepCoins net store poolM keys (typed <> listed) opts.coins (fst <$> sweepTo)
  -- The coins of --coin and of the keys tell their addresses only now.
  -- The builder counts on this refusal: it reads no coin as the wallet's.
  refuseOwnAddresses net (mapMaybe (.address) found.coins)
  (swept, unswept) <- leavingDust feeByte sweepTo found
  -- The lock time is drawn from the best block of the store, which the
  -- coins come from: a sweep is prepared online, whatever the wallet
  -- knows of the chain. A store behind the chain gives none.
  best <- fromStore $ storeBestBlock store
  now <- liftIO getCurrentTime
  gen <- liftIO initStdGen
  let heightM = if isStale now (Just best.header.timestamp) then Nothing else Just best.height
      (lockTime, buildGen) = lockTimeFor heightM gen
  tsd <- withLabel opts.label <$> liftEither (buildSweepSignData net buildGen swept sweepTo feeByte lockTime)
  return
    BuiltSweep
      { builtSweep = tsd,
        builtKeys = keys,
        builtEmpty = addressesWithoutCoin (typed <> listed) found,
        builtUnswept = unsweptCoins HoldsTokens found.withTokens <> unswept
      }
  where
    -- The coins to sweep among those found, with or without the dust
    -- coins (@coinsWithDust@), and the dust coins left. The question is
    -- about every coin found, those of --coin too: the user who names a
    -- coin may not know what its input costs. The report and the question
    -- go to stderr, to keep the --json answer clean.
    leavingDust :: Natural -> [(Address, Share)] -> SweepCoins -> ExceptT String (DB IO) (SweepCoins, [UnsweptCoin])
    leavingDust feeByte sweepTo found = do
      sized <- liftEither $ sizedSweepCoins net found
      swept <- coinsWithDust opts.dustCoins feeByte sweepTo id (askAboutDust feeByte sweepTo) sized
      return (sweepingOnly ((.coin) <$> swept) found, unsweptCoins WorthLessThanItsFee ((.coin) <$> filter (`notElem` swept) sized))
    askAboutDust feeByte sweepTo worth dust = do
      let report = sweepDustReport feeByte sweepTo worth dust
      liftIO $ renderOn stderr (sweepDustReportDoc net unit report)
      askYes terminal (dustQuestion report)
    -- An address of --sweep-from. A private key in its place is the
    -- mistake of a user with a paper wallet, who is told where keys go.
    sweptAddress typed
      | hasSecKeyForm typed = Left (secKeyNotAddress <> ". Give the keys with --key-file, or the address of the key with --sweep-from")
      | otherwise = sweepableAddress net =<< textToAddrE net typed
    -- The addresses of the --address-file, read on the network of the
    -- sweep
    addressesOfFile file = liftEither . (mapM (sweepableAddress net) <=< parseAddresses net) =<< liftEitherIO (readTextFile file)

-- | Keep a sweep just built as preparesweep leaves it, and give its
-- summary as the answer shows it. One that pays a portfolio is pending in
-- it, unsigned, and in the --output file when there is one
-- (@storePrepared@). One that pays none is in its --output file alone:
-- the wallet does not hold it, so nothing keeps the time it was prepared,
-- and the summary takes the time of now, when the file is written.
keepBuiltSweep :: Network -> Maybe Pool -> Maybe FilePath -> BuiltSweep -> ExceptT String (DB IO) TxInfo
keepBuiltSweep net poolM output built = case poolM of
  Just pool -> storePrepared pool Nothing [] output built.builtSweep
  Nothing -> do
    info <- lift . fillContacts net =<< liftEither (parseOutsideTxSignData net built.builtSweep)
    writeOutput Nothing net output info built.builtSweep
    now <- liftIO getCurrentTime
    return $ case info.txInfoStatus of
      Pending pending -> info {txInfoStatus = Pending pending {pendingCreated = Just now}}
      OnChain _ -> info

-- | Refuse a sweep from addresses that the wallet holds on the network of
-- the sweep, in any portfolio ('heldOnNetwork'). A sweep is signed by
-- signsweep with private keys from outside the wallet, and the user has
-- none for an address of the wallet: the next step that preparesweep
-- prints could not be done, and the sweep, kept pending when it pays the
-- wallet too, would lock the coins it spends. sendall and preparetx do
-- the same job, with the rules of a payment and the mnemonic to sign.
-- Keeping the sweep and pointing to signtx was the other way: it would
-- leave two ways to do one thing. The refusal names every such address,
-- and where the wallet has them.
refuseOwnAddresses :: Network -> [Address] -> ExceptT String (DB IO) ()
refuseOwnAddresses net addrs = do
  own <- lift $ heldOnNetwork net addrs
  unless (null own) $ do
    places <- mapM poolPlace (nub (snd <$> own))
    let named = intercalateAnd (addrToString net . fst <$> own)
        subject = case own of
          [_] -> "The address " <> named <> " is"
          _ -> "The addresses " <> named <> " are"
    throwError $
      subject
        <> " of this wallet, in "
        <> intercalateAnd (placeText <$> places)
        <> ". A sweep is for coins from outside the wallet, which signsweep signs with their private keys. To move coins of this wallet, use sendall or preparetx: --spend-from chooses the coins of an address."

-- | Sign a sweep with private keys: a pending transaction of the wallet,
-- by its nosigHash, or one in a file. A sweep outside the wallet is signed
-- on the network its file names, or on that of --network or of the
-- configuration when the file names none, from its file to another, or in
-- its place.
cmdSignSweep :: Config -> AmountUnit -> Signing -> Maybe KeySource -> Maybe Network -> IO Response
cmdSignSweep cfg unit signing keysM givenM =
  runDBResponse cfg $ do
    (poolM, file@(TxFileRead txFile@(TxFile netM tsd _ _) sourceM)) <- transactionToSign cfg givenM signing
    forM_ poolM $ \pool -> refuseForPrivateKeys signing pool tsd
    net <- case poolM of
      Just pool -> return pool.poolNetwork
      Nothing -> commandNetwork cfg <$> fileNetwork givenM netM
    -- A key file that does not read stops the command here, before the
    -- sweep is shown: its error would come below a screen of transaction
    fromFile <- case keysM of
      Just source@(KeysInFile _) -> Just <$> keysOn net tsd source
      _ -> return Nothing
    signedFile <- signedFileOf signing
    -- Shown before the keys are asked for, as signtx shows it
    showToSign unit signing net poolM txFile
    -- Without a file, the keys are typed: signing cannot do without them
    keys <- maybe (keysOn net tsd KeysOnTerminal) return fromFile
    case poolM of
      Just pool -> txAnswer pool =<< signPoolSweep signing signedFile pool file keys
      Nothing -> do
        (signed, bare) <- liftEither $ signTxWithKeys net tsd [] noneHeld keys
        -- No address of the sweep is the wallet's: none has a label
        writeSigned Nothing sourceM signedFile (labelledTxFile net bare signed)
        ResponseOutsideTx net <$> lift (fillContacts net bare)
  where
    -- The coins of the sweep tell when the keys typed are complete
    keysOn net tsd source = do
      coins <- liftEither $ sweptAddresses net tsd
      readSecKeys terminal net (keysOfCoins (Sweep.keyAddresses net) coins) source

-- | Sign a sweep that pays a portfolio with the private keys of its
-- coins, and keep it signed (@keepSigned@): what signsweep does of such a
-- sweep once it has the keys, and the second step of sweep. The accounts
-- of the portfolio tell which outputs are its own.
signPoolSweep :: Signing -> SignedFile -> Pool -> TxFileRead -> [SecKey] -> ExceptT String (DB IO) TxInfo
signPoolSweep signing signedFile pool file@(TxFileRead txFile _) keys = do
  accounts <- lift $ poolAccounts pool
  keysOfPool <- lift $ poolKeys pool
  (signed, info) <- liftEither $ signTxWithKeys pool.poolNetwork (txFileData txFile) keysOfPool (heldPoolOf (snd <$> accounts)) keys
  keepSigned signing signedFile pool file signed info

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
-- shows (@shownSyncs@). It gives no sync of a hidden network, and names
-- none as not served or not synced, so a store that fails on a hidden
-- network alone does not make the command exit with an error. A network
-- the host does not serve is named and left out. So is one whose store is
-- down, or fails during the sync of one of its pools, which also makes
-- the command exit with an error once the others are synced: the sync of
-- that pool is undone, and the syncs of the other networks are kept
-- ('syncPools'). So is one whose store is not on the chain of the last
-- sync, which only --new-chain follows
-- ('Haskoin.Wallet.Sync.chainSeen'). The answer names the active
-- portfolio too: the steps after a sync work in it, and a pending
-- transaction in conflict in another portfolio needs a switch first.
cmdSync :: Config -> Maybe Network -> SyncMode -> IO Response
cmdSync cfg netM mode =
  withStatusLine $ \line ->
    runDBResponse cfg $ do
      -- A new chain is taken on one network at a time, the one the user
      -- names: the stores of the other networks are refused as before
      when (mode == NewChain && isNothing netM) $
        throwError "--new-chain goes with --network: it follows the new chain of the store of one network"
      portfolios <- lift getAllPortfolios
      when (null portfolios) $ throwError noWallets
      pools <- concat <$> mapM (lift . portfolioPools . (.portfolioId)) portfolios
      let chosen = [pool | pool <- pools, maybe True (== pool.poolNetwork) netM]
      storeOn <- liftIO $ webStores cfg
      synced <- syncPools cfg storeOn chosen mode (terminalWatcher line)
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
-- A path, such as bip84\/0\/3, names the address the public key of the
-- account derives there, in the active portfolio, on the --network or the
-- default network, as show derives it ('derivedAddressAt'). It needs no
-- row. The offline computer never gives out an address, since receive
-- runs online, and holds none: a path is how it signs. The signed message
-- names the address as typed, or, for a path, in the form the wallet
-- stores, so that the reader sees which address signed.
cmdSignMsg :: Config -> SignMsg -> IO Response
cmdSignMsg cfg opts =
  runDBResponse cfg $ do
    -- Before the mnemonic is asked, not after
    for_ opts.output checkFileToWrite
    (portfolio, acc, path, signer) <- case opts.address of
      NamedByAddress typed -> do
        (portfolio, accId, row) <- addressByTextOr (notHeldToSign typed) cfg opts.network typed
        acc <- getAccountById accId
        return (portfolio, acc, addrPath row, typed)
      NamedByPath kind addrType index -> do
        portfolio <- requireActivePortfolio
        (_, acc) <- chooseAccount cfg portfolio opts.network (Just kind)
        derived <- derivedAddressAt acc addrType index
        stored <- liftEither $ addrToTextE (accountNetwork acc) derived
        return (portfolio, acc, addrDeriv addrType :/ index, stored)
    let net = accountNetwork acc
    addr <- liftEither $ textToAddrE net signer
    -- Warned before the mnemonic is asked, so that the user can stop. On
    -- stderr, to keep the armored block and the --json output clean.
    liftIO $ mapM_ warn $ signingWarnings opts.message
    (_, accountPrvKey) <- askSigningKey terminal (accountKind acc) net (portfolioIndex portfolio.portfolioRow) (accountXPubKey acc)
    let addrKey = derivePath path accountPrvKey
    signature <- encodeSignature <$> liftEither (signMessage addr addrKey.key opts.message)
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
-- wallet is not needed: the address can be anyone's. hw reads the address
-- on the network it is written for, shown or hidden, so the user gives no
-- --network (@signerAddress@).
cmdVerifyMsg :: Config -> VerifyMsg -> IO Response
cmdVerifyMsg _ opts =
  runResponse $ do
    (address, signature, message) <- case opts.source of
      Given address signature message -> return (address, signature, message)
      FromFile file -> signedMessageFromFile file
    addr <- liftEither $ signerAddress opts.network address
    valid <- liftEither $ verifyMessage addr signature message
    -- A message that is hard to copy exactly may be why the signature does
    -- not check out. On stderr, to keep the --json answer clean.
    unless valid $ liftIO $ mapM_ warn $ messageWarnings message
    return $ ResponseVerifyMsg message signature address valid

-- | The address a message is said to be signed by, read on the --network
-- when it is given. Without it, the address is read on a network that
-- can read it, whether hw shows that network or hides it.
--
-- The text of an address does not always name one network
-- (@readingNetworks@). This does not matter here. Every network that
-- reads the text reads the same key hash from it, and the message is
-- hashed the same way on each network ('signedMessageHash'). So the first
-- network that reads the address gives the answer that any other would.
-- Asking for --network, as hw did before, only made the user name a
-- network that changes nothing.
--
-- A hidden network reads the address as a shown one does. The check
-- reads nothing of the wallet and answers with nothing of a network: the
-- address is anyone's, and the answer is valid or not. hw first refused
-- an address that only hidden networks read, as it refuses what the
-- wallet has on hidden networks alone. The user then had to show a
-- network of the own wallet, and hide it again, to check the proof of
-- somebody else. For the same reason the --network of verifymsg may name
-- a hidden network ('Haskoin.Wallet.Cli.Parser.networkNamedBy').
signerAddress :: Maybe Network -> Text -> Either String Address
signerAddress (Just net) typed = textToAddrE net typed
signerAddress Nothing typed = case readingNetworks typed of
  (_, addr) : _ -> Right addr
  [] -> Left (notAnAddress typed "any network")

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

-- | Copy the database to the given file, which must not exist. Without a
-- file, copy it into the user-backups directory next to the database, and
-- write its labels beside it, as exportwallet writes them
-- ('userBackup'). The two files take one name, which carries the format
-- version, the time, and how many wallets and portfolios the database
-- holds. hw never removes them, unlike the copies it makes by itself in
-- the auto-backups directory. The database is at the latest format version,
-- since every command refuses one of another version first
-- ("Haskoin.Wallet.Migration").
--
-- The labels go with the copy because a copy of the database only reads
-- in an hw of its format, and the labels in importwallet of any hw to
-- come, and in other wallets. A database with no wallet has an empty file
-- of labels, which keeps the two files of every backup together.
--
-- The copy, its name and its labels are read in one transaction, which
-- holds the database: they agree with each other. A copy to a file of the
-- user's choice stays the copy alone, as it was.
cmdBackup :: Config -> Maybe FilePath -> IO Response
cmdBackup cfg fileM = case fileM of
  Just file -> runResponse $ do
    let database = databaseFile cfg
    checkFileToWrite file
    liftEitherIO $ copyDatabase database file
    return $ ResponseFile file
  Nothing -> runDBResponse cfg $ do
    files <- backupNow userBackup cfg
    return $ ResponseBackup (backupDatabase files) (backupLabels files)

-- | Show the copies hw keeps of the database and of its labels: how many
-- of each kind the auto-backups and user-backups directories hold, and
-- the newest of each ('backupsShown'). It reads the two directories and
-- nothing else, so it needs no transaction of the database.
--
-- hw says nothing when it makes a copy, and only warns when it could
-- not. This is where the user checks that the copies are being made.
cmdShowBackups :: Config -> IO Response
cmdShowBackups cfg =
  runResponse $
    ResponseBackups
      <$> liftEitherIO (backupsShown (configAutoBackupDatabase cfg) (configAutoBackupLabels cfg) (databaseFile cfg))

-- | Put a copy of the database back in place of the database: the rows
-- of every table of the copy replace those of the database
-- ('replaceRowsFromAttached'). The answer says what the database holds
-- now, which is what the copy held.
--
-- The copy is checked first, as show checks it. It must be an SQLite
-- database of hw at the format version of this hw ('databaseToRestore').
-- The file of the labels of a backup is refused with where it goes,
-- since a backup has two files under one name.
--
-- Everything else happens in one transaction, which holds the database.
-- No other command changes the database between the question and the
-- restore, and a failure leaves the database as it was.
--
-- * The user is asked, with what the database holds and what the copy
--   holds, unless --yes. A database without a wallet has nothing to
--   lose, so nothing is asked then. That is the restore on a new
--   computer.
--
-- * The database is copied into auto-backups with its labels
--   (@backUpBeforeLosing@), unless it holds no wallet. A restore of the
--   wrong copy is then undone by a restore of that one.
--
-- * The rows of the copy go in.
--
-- A damaged database is another case ('databaseDamage'): hw cannot read
-- what it holds, so there is nothing to ask about and nothing to copy.
-- Its file is moved into broken-databases ('setDamagedAside'), a new
-- database takes its place, and the rows of the copy go in. The copy was
-- checked before, so the move happens only for a file hw can restore.
-- The answer names where the damaged database is. Nothing is asked: the
-- file is kept, and a restore of the wrong copy is undone by a restore
-- of another one.
--
-- No copy follows the restore (@backsUpBefore@). What the database holds
-- now is in the file it was restored from.
cmdRestoreDatabase :: Config -> FilePath -> Bool -> IO Response
cmdRestoreDatabase cfg file yes = do
  checked <- runExceptT $ do
    start <- liftEitherIO $ readFileStart databaseHeaderLength file
    unless (startsDatabase start) $
      throwError (file <> " is not an SQLite database. restoredatabase takes a copy of the database, such as the one backup wrote. The file of the labels of a backup goes to importwallet")
    copy <- liftEitherIO $ databaseToRestore file
    itself <- liftIO $ (==) <$> canonicalizePath file <*> canonicalizePath (databaseFile cfg)
    when itself $ throwError (file <> " is the database itself. Give a copy of it")
    return copy
  damage <- databaseDamage (databaseFile cfg)
  case (checked, damage) of
    (Left refusal, _) -> return (ResponseError (cs refusal))
    (Right copy, Nothing) -> runDBResponseOn cfg (runDBWithAttached cfg file) (restoring copy Nothing)
    (Right copy, Just _) -> either (return . ResponseError . cs) (overDamaged copy) =<< setDamagedAside (databaseFile cfg)
  where
    -- The damaged database is out of the way. The file that takes its
    -- place gets the tables of hw, then the rows of the copy. A refusal
    -- now leaves that new database, without a wallet.
    overDamaged copy moved = do
      response <- runDBResponseOn cfg (\action -> runDB cfg createDatabase >> runDBWithAttached cfg file action) (restoring copy (Just moved))
      return $ case response of
        ResponseError refusal -> ResponseError (refusal <> ". The damaged database was moved to " <> cs moved <> ", and the database is new, without a wallet")
        restored -> restored
    restoring copy movedM = do
      current <- heldNow
      let holdsWallet = not (null (databaseShownWallets current))
      when (holdsWallet && not yes) $ do
        agreed <- askYes terminal (replaceQuestion current copy)
        unless agreed $ throwError "The database was not restored"
      backup <- if holdsWallet then backUpBeforeLosing cfg "Nothing was restored" else return Nothing
      withExceptT (("The SQLite database " <> file <> " cannot be restored: ") <>) replaceRowsFromAttached
      restored <- heldNow
      return $ ResponseRestored file restored backup movedM
    replaceQuestion current copy =
      "The database holds "
        <> holdingText current
        <> ". The copy holds "
        <> holdingText copy
        <> ", and replaces all of it. "
        <> ( if configAutoBackupDatabase cfg
               then "hw copies the database into auto-backups first."
               else "The database is not copied first, since autobackup-database is false in the configuration."
           )
        <> " Replace the database?"

-- | How many wallets and portfolios a database holds, as a sentence
-- counts them: "2 wallets and 3 portfolios", or "no wallet"
holdingText :: DatabaseShown -> String
holdingText shown = case databaseShownWallets shown of
  [] -> "no wallet"
  wallets -> counted (length wallets) "wallet" <> " and " <> counted (length (concatMap walletShownPortfolios wallets)) "portfolio"
  where
    counted :: Int -> String -> String
    counted 1 thing = "1 " <> thing
    counted count thing = show count <> " " <> thing <> "s"
