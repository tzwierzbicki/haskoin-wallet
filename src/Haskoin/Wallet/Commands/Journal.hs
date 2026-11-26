{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The journal for ledger ("Haskoin.Wallet.Export.Journal"): exportjournal,
-- which writes it to a file the user names, and the journal hw keeps by
-- itself.
--
-- When the key journal of the configuration is on, hw keeps the journal of
-- exportjournal in a file next to the database, and writes it again after
-- each command that changes it. The user includes that file in a journal
-- of their own, once, and the reports of ledger are then up to date with
-- no export to run. Run by hand, the export is the step a user forgets,
-- and a journal that is behind says nothing of being behind.
--
-- The file is hw's own, as its automatic backups are. hw never writes over
-- a file the user names, and this one the user does not name: hw replaces
-- it without asking, and removes it when the key is turned off.
--
-- Two rules keep the file. After a command that may change the journal, hw
-- writes it again ('journalAfter'). Before any command, hw writes it when
-- it is not there, or when it was written for other accounts of tags than
-- the configuration gives now ('journalBeforeCommand').
module Haskoin.Wallet.Commands.Journal
  ( cmdExportJournal,
    journalAfter,
    journalBeforeCommand,
  )
where

import Control.Exception (displayException)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Wrap (warn)
import Haskoin.Wallet.Commands.Run (afterCommand, besideCommand, runDBResponse)
import Haskoin.Wallet.Commands.Scope (listedPools)
import Haskoin.Wallet.Commands.TagFilter (txSelection)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Export.Journal (journalHeader, localJournal)
import Haskoin.Wallet.Export.TxSelection (TxSelection (..))
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Util
import System.Directory (doesPathExist, removeFile)
import UnliftIO.Exception (tryAny)

-- | Write the transactions of every portfolio to a new journal of
-- double-entry accounting, in the format of the ledger command line tool
-- ("Haskoin.Wallet.Export.Journal"), and answer with the file.
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
-- states no balances at its end ('txSelection').
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
-- configuration gives to tags ("Haskoin.Wallet.Export.Journal"). With a
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
  CommandCheckMnemonic _ -> False
  CommandMnemonicSheet _ -> False
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
-- succeeded, when the command may have changed it (@changesJournal@) and
-- the configuration asks for a journal.
--
-- setdefault changes the configuration file the command was started
-- with, so the configuration is read again after it. It may have turned
-- the journal on, which is then written at once. It may have shown or
-- hidden a network, which the journal must follow. It may have turned
-- the journal off, and the file is then removed (@dropJournal@).
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
-- ("Haskoin.Wallet.Export.Journal"), so the file changes only when the
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
-- ('journalAfter'). That leaves two gaps, both made by a change no
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
-- 'journalAfter', which knows which commands change it, so a command
-- that changes nothing still reads no transaction. A file hw cannot read
-- is left to it as well: hw cannot tell what it holds, and 'journalAfter'
-- says so when it fails to replace it.
--
-- It runs before the command and not after it. Every command passes
-- here once, while a listing on the screen and a command that fails
-- end in other places. A command that changes the journal then writes
-- it a second time, which costs one more reading of the transactions,
-- once.
--
-- A journal that cannot be written is told on stderr, and the command
-- still runs ('besideCommand').
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
