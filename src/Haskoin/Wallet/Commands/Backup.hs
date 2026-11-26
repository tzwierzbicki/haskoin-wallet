{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The copies of the database and of its labels, as the commands make
-- them: those hw makes by itself around a command, and the commands
-- backup, showbackups and restoredatabase. "Haskoin.Wallet.Database.Backup" writes
-- the files and says which ones are kept.
--
-- A command that adds a wallet or a portfolio is followed by a copy of the
-- database ('changesPortfolios', 'backUpDatabase'). A command that changes
-- the wallets, their descriptions, the labels, the tags, the address book
-- or the locked coins is followed by a copy of the labels, the file
-- exportwallet writes ('changesLabels', 'backUpLabels'). Both go into the
-- auto-backups directory next to the database, which keeps the recent
-- copies of each and a thinning trail of older ones, unless the
-- configuration turns them off.
--
-- A command that loses what the database holds copies it first, with its
-- labels, into the auto-backups directory too. deletewallet and
-- deleteportfolio do so before they delete, and restoredatabase before it
-- replaces the database ('backUpBeforeLosing'). That copy holds the
-- database as it was. Such a command has no copy after it
-- ('backsUpBefore'): one command makes one backup, of what is worth the
-- most. Every copy hw makes by itself is in that one directory. The
-- user-backups directory holds the copies of the backup command alone.
module Haskoin.Wallet.Commands.Backup
  ( backsUpBefore,
    changesPortfolios,
    backUpDatabase,
    backUpBeforeLosing,
    changesLabels,
    backUpLabels,
    cmdBackup,
    cmdShowBackups,
    cmdRestoreDatabase,
  )
where

import Control.Monad (unless, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Run (afterCommand, runDBResponse, runDBResponseOn, runResponse)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Backup (CopyStamp (..), autoBackupRetention, backupsShown, copyDatabase, rollBackup, rollDatabaseCopy, rollLabelCopy, userBackup)
import Haskoin.Wallet.Database.Failure (databaseDamage, setDamagedAside)
import Haskoin.Wallet.Database.File (DatabaseShown (..), WalletShown (..), databaseHeaderLength, databaseToRestore, heldNow, startsDatabase)
import Haskoin.Wallet.Database.Migration (createDatabase, latestDatabaseVersion)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Util
import Haskoin.Wallet.WalletFile (databaseRecords, importCreated)
import Haskoin.Wallet.WalletFile.Bip329 (recordsText)
import System.Directory (canonicalizePath)

-- | Whether the answer of a command says that it lost what the database
-- held: ResponseDeleted is the answer of deleteportfolio and deletewallet
-- alone, and ResponseRestored that of restoredatabase. hw makes its
-- backup before such a command ('backUpBeforeLosing'), and no copy after
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
-- ("Haskoin.Wallet.Database.Backup".'autoBackupRetention'). Without it, the copy
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
-- command ('backUpDatabase'), unless it lost what the database held:
-- its copy is then made before it ('backsUpBefore').
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
-- foresee is caught too, and told the same way (@tryAny@), rather than
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
-- ('backsUpBefore'), so this one is the newest copy until the next
-- one, and the last of its hour unless that one comes within the hour.
-- A user who wants it for good runs backup before the command, whose
-- copies hw never removes. The user-backups directory is for those
-- alone: a file there is one the user asked for.
--
-- A copy that cannot be made stops the command, with the text given
-- first, which says that nothing was done. The command has lost nothing
-- yet, so stopping is the safe answer. A copy after a command cannot
-- stop it any more, which is why that one only warns (@automaticCopy@).
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
-- a copy of the labels after such a command ('backUpLabels'), unless it
-- is deletewallet, deleteportfolio or restoredatabase, which have their
-- copy before them ('backsUpBefore').
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
-- ('Haskoin.Wallet.Database.PendingTx.changeTakesTags'). The file holds
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
  CommandCheckMnemonic _ -> False
  CommandMnemonicSheet _ -> False
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

-- | Copy the database to the given file, which must not exist. Without a
-- file, copy it into the user-backups directory next to the database, and
-- write its labels beside it, as exportwallet writes them
-- ('userBackup'). The two files take one name, which carries the format
-- version, the time, and how many wallets and portfolios the database
-- holds. hw never removes them, unlike the copies it makes by itself in
-- the auto-backups directory. The database is at the latest format version,
-- since every command refuses one of another version first
-- ("Haskoin.Wallet.Database.Migration").
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
--   ('backUpBeforeLosing'), unless it holds no wallet. A restore of the
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
-- No copy follows the restore ('backsUpBefore'). What the database holds
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
        agreed <- askYes terminal (ScriptOption "--yes") (replaceQuestion current copy)
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
