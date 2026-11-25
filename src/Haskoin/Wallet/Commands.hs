{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The commands of hw. Each reads what it needs from the database, the
-- store and the user, and answers with a 'Response'. This module is the
-- way in: 'commandResponse' runs a command, and 'commandListing' gives the
-- listing of a command that lists. The commands are in the modules below
-- this one, a module for each kind of thing they work on, and this module
-- exports them again.
--
-- A command runs in one transaction of the database: one that fails
-- leaves the database as it was, and answers with its error
-- ("Haskoin.Wallet.Commands.Run"). It works in the active portfolio,
-- which the database keeps and switch changes, on the networks hw shows
-- ("Haskoin.Wallet.Commands.Scope"). A --network that names a hidden
-- network is refused before the command runs ('refusedNetwork').
--
-- A listing covers the whole portfolio unless the options narrow it, in a
-- section for each network, or for each account for the addresses
-- ("Haskoin.Wallet.Cli.Listing"). --reverse turns the rows of each section
-- around, and nothing else changes the order of a listing
-- ('ListingOrder'). --tag keeps the rows that have a tag, and leaves the
-- others out ('keptTagsOfListing'). A filter never changes what a row
-- shows.
--
-- Around a command, hw does two things by itself ('commandResponse'). It
-- copies the database or its labels after a command that changed them
-- ("Haskoin.Wallet.Commands.Backup"). It writes the journal it keeps for
-- ledger again, when the configuration asks for one
-- ("Haskoin.Wallet.Commands.Journal").
--
-- Where the commands are:
--
-- * "Haskoin.Wallet.Commands.Portfolio": newportfolio, recover,
--   checkmnemonic, mnemonicsheet, switch, portfolios, deleteportfolio and
--   deletewallet
-- * "Haskoin.Wallet.Commands.WalletFile": importwallet and exportwallet
-- * "Haskoin.Wallet.Commands.Defaults": setdefault and showdefault
-- * "Haskoin.Wallet.Commands.Address": receive, addresses and balances
-- * "Haskoin.Wallet.Commands.Show": show
-- * "Haskoin.Wallet.Commands.Label": label
-- * "Haskoin.Wallet.Commands.Tag": tag, untag, tags, renametag and
--   deletetag
-- * "Haskoin.Wallet.Commands.Contact": newcontact, contacts and
--   deletecontact
-- * "Haskoin.Wallet.Commands.Coin": coins, lockcoin and unlockcoin
-- * "Haskoin.Wallet.Commands.Transaction": txs, pendingtxs, importtx,
--   exporttx and deletetx
-- * "Haskoin.Wallet.Commands.Payment": preparetx, preparesendall and fees
-- * "Haskoin.Wallet.Commands.Signing": signtx
-- * "Haskoin.Wallet.Commands.Send": send, sendall and broadcast
-- * "Haskoin.Wallet.Commands.BumpFee": bumpfee
-- * "Haskoin.Wallet.Commands.Sweep": sweep, preparesweep and signsweep
-- * "Haskoin.Wallet.Commands.Sync": sync
-- * "Haskoin.Wallet.Commands.MessageSigning": signmsg and verifymsg
-- * "Haskoin.Wallet.Commands.Backup": backup, showbackups and
--   restoredatabase
-- * "Haskoin.Wallet.Commands.Export": --csv and exportsheet
-- * "Haskoin.Wallet.Commands.Journal": exportjournal
-- * "Haskoin.Wallet.Commands.Report": report
--
-- What several of them share is in "Haskoin.Wallet.Commands.Answer",
-- "Haskoin.Wallet.Commands.ShortId", "Haskoin.Wallet.Commands.TagFilter",
-- "Haskoin.Wallet.Commands.TagLock", "Haskoin.Wallet.Commands.Deletion"
-- and "Haskoin.Wallet.Commands.TxFile".
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
    cmdMnemonicSheet,
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

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..))
import Haskoin.Wallet.Amounts (AmountUnit)
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Address (addressesListing, balancesListing, cmdReceive)
import Haskoin.Wallet.Commands.Backup (backUpDatabase, backUpLabels, backsUpBefore, changesLabels, changesPortfolios, cmdBackup, cmdRestoreDatabase, cmdShowBackups)
import Haskoin.Wallet.Commands.BumpFee (cmdBumpFee)
import Haskoin.Wallet.Commands.Coin (cmdLockCoin, cmdUnlockCoin, coinsListing)
import Haskoin.Wallet.Commands.Contact (cmdDeleteContact, cmdNewContact, contactsListing)
import Haskoin.Wallet.Commands.Defaults (cmdSetDefault, cmdShowDefault)
import Haskoin.Wallet.Commands.Export (cmdCsv, cmdExportSheet)
import Haskoin.Wallet.Commands.Journal (cmdExportJournal, journalAfter, journalBeforeCommand)
import Haskoin.Wallet.Commands.Label (cmdLabel)
import Haskoin.Wallet.Commands.MessageSigning (cmdSignMsg, cmdVerifyMsg)
import Haskoin.Wallet.Commands.Payment (cmdFees, cmdPrepareSendAll, cmdPrepareTx)
import Haskoin.Wallet.Commands.Portfolio (cmdCheckMnemonic, cmdDeletePortfolio, cmdDeleteWallet, cmdMnemonicSheet, cmdNewPortfolio, cmdRecover, cmdSwitch, portfoliosListing)
import Haskoin.Wallet.Commands.Report (cmdReport)
import Haskoin.Wallet.Commands.Send (cmdBroadcast, cmdSend, cmdSendAll)
import Haskoin.Wallet.Commands.Show (cmdShow)
import Haskoin.Wallet.Commands.Signing (cmdSignTx)
import Haskoin.Wallet.Commands.Sweep (cmdPrepareSweep, cmdSignSweep, cmdSweep)
import Haskoin.Wallet.Commands.Sync (cmdSync)
import Haskoin.Wallet.Commands.Tag (cmdDeleteTag, cmdRenameTag, cmdTag, cmdTags, cmdUntag)
import Haskoin.Wallet.Commands.TagFilter (keptTagsOfListing)
import Haskoin.Wallet.Commands.Transaction (cmdDeleteTx, cmdExportTx, cmdImportTx, pendingTxsListing, txsListing)
import Haskoin.Wallet.Commands.WalletFile (cmdExportWallet, cmdImportWallet)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag

-- | The answer of hw to a command. After a command that added a wallet
-- or a portfolio, hw keeps a copy of the database ('backUpDatabase').
-- After a command that may change the labels, it keeps a copy of them
-- ('backUpLabels'). The configuration can turn either off. A command
-- that failed is not followed by a copy: it undid what it wrote, but for
-- the signed transaction of a send or a sendall. Nor is a command that
-- loses what the database holds, whose copy was made before it
-- ('backsUpBefore').
--
-- After a command that may change the journal hw keeps for ledger, hw
-- writes the journal again, when the configuration asks for one
-- ('journalAfter'). A command that failed changed nothing the journal
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
  CommandCheckMnemonic opts -> cmdCheckMnemonic cfg opts
  CommandMnemonicSheet opts -> cmdMnemonicSheet opts
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
