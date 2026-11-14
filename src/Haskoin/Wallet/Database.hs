{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The wallet database: the connection and its lock, and what every table
-- shares. The tables are those of the latest format version, defined in
-- "Haskoin.Wallet.Schema.V5" and exported again from here: a version that
-- changes the tables gets a new schema module, and this module moves to it
-- ("Haskoin.Wallet.Migration").
-- Each kind of row has a module of its own: "Haskoin.Wallet.Database.Portfolio"
-- for the wallets and their portfolios, "Haskoin.Wallet.Database.Account",
-- "Haskoin.Wallet.Database.Address", "Haskoin.Wallet.Database.Coin", "Haskoin.Wallet.Database.TxInfo",
-- "Haskoin.Wallet.Database.TxLabel", "Haskoin.Wallet.Database.RawTx",
-- "Haskoin.Wallet.Database.PendingTx" and "Haskoin.Wallet.Database.BestBlock".
--
-- The tables store what hw learnt from outside. From the store: the
-- balances of the addresses, the transaction summaries, the coins, the raw
-- transactions, the best block, and whether a pending transaction is in
-- conflict, which only the store can tell. From the user or a signer: the
-- wallets, the portfolios and their accounts, the descriptions, the
-- labels, the pending transactions, the coins the user locked and the
-- fingerprint of a seed.
--
-- What follows from those facts by the rules of the wallet is computed
-- when it is read, and stored nowhere: the confirmations, from the best
-- block; the coins the pending transactions lock, and the change addresses
-- they hold; whether a coin can be spent, and the spendable balances. A
-- stored copy would have to be updated by every write that changes what it
-- depends on, and would show a wrong value as soon as one write forgets.
--
-- A new kind of data is placed by the same question: did hw learn it, or
-- does hw decide it? A computed value may still be stored, so that the
-- listings stay fast, when one place writes everything it depends on: the
-- confirmed and unconfirmed totals of an account are the sums of its
-- address balances, which the sync writes, right before it writes the
-- totals. The amount of a summary is a column as well as a part of its
-- blob, written with it, so that txs can add up amounts for its balances
-- without decoding the blobs. The spendable balances depend on writes in many places, the
-- pending transactions of every portfolio on a network and the best block
-- among them, and are computed: from the few columns they need, and for
-- the addresses shown only.
module Haskoin.Wallet.Database
  ( module Haskoin.Wallet.Database,
    module Haskoin.Wallet.Schema.V5,
    module Haskoin.Wallet.Schema.KeyBytes,
  )
where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Lens ((&), (.~))
import Control.Monad
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Esqueleto.Legacy as E
import Database.Persist.SqlBackend.Internal (SqlBackend (..))
import Database.Persist.Sqlite (SqliteConnectionInfo, extraPragmas, fkEnabled, mkSqliteConnectionInfo, walEnabled, withSqliteConnInfo)
import Database.Sqlite (Error (..), SqliteException (..))
import Haskoin
import Haskoin.Wallet.Config
import Haskoin.Wallet.FileIO (privateFile)
import Haskoin.Wallet.Schema.KeyBytes
import Haskoin.Wallet.Schema.V5
import System.Directory (makeAbsolute)
import Numeric.Natural (Natural)
import UnliftIO.Exception (catch, throwIO)

{- The connection and its lock -}

type DB m = ReaderT SqlBackend (NoLoggingT (ResourceT m))

-- | Run a database action as one transaction that holds the database from
-- its first statement to its last. Every hw command, whether it reads or
-- writes, takes the write lock as it begins, so that commands run one at a
-- time: a sync cannot be undercut by a command slipping in while it waits
-- on the store, and what a command reads is what it writes against. A
-- command that finds the database held waits 'busyTimeout' for the other
-- one to finish, then fails with 'databaseInUse'.
runDB :: (MonadUnliftIO m) => Config -> DB m a -> m a
runDB cfg action = do
  dbFile <- liftIO $ databaseFile cfg
  -- A new database is made private before SQLite opens it, which would
  -- give it the mode the umask allows, readable by others as a rule
  liftIO $ privateFile dbFile
  runDBWith busyTimeout dbFile action

-- | How long a command waits for another one to release the database, in
-- milliseconds: enough for commands run back to back to follow each other,
-- too short to look stuck when a sync holds it
busyTimeout :: Int
busyTimeout = 3000

-- | 'runDB' on a database file, waiting the given milliseconds for the lock
runDBWith :: (MonadUnliftIO m) => Int -> FilePath -> DB m a -> m a
runDBWith timeout dbFile action =
  runResourceT . runNoLoggingT $
    withSqliteConnInfo connection $ \backend -> runSqlConn action (lockingAtOnce backend)
  where
    connection =
      mkSqliteConnectionInfo (cs dbFile)
        & extraPragmas .~ ["PRAGMA busy_timeout = " <> cs (show timeout) <> ";"]

-- | The connection with its transactions taking the write lock as they
-- begin rather than at their first write: SQLite's BEGIN IMMEDIATE
lockingAtOnce :: SqlBackend -> SqlBackend
lockingAtOnce backend =
  backend
    { connBegin = \prepare _ -> do
        statement <- prepare "BEGIN IMMEDIATE"
        _ <- stmtExecute statement []
        stmtReset statement
    }

-- | What a command answers when another hw command holds the database
databaseInUse :: String
databaseInUse = "The database is in use by another hw command; try again when it has finished"

-- | Run the action, answering 'databaseInUse' instead of failing when the
-- database is held by another command past the wait
catchDatabaseInUse :: (MonadUnliftIO m) => m a -> m (Either String a)
catchDatabaseInUse action = (Right <$> action) `catch` held
  where
    held failure = case failure of
      SqliteException ErrorBusy _ _ -> return $ Left databaseInUse
      SqliteException ErrorLocked _ _ -> return $ Left databaseInUse
      _ -> throwIO failure

{- Pools -}

-- | What a portfolio holds on one network: the coins of its accounts
-- there, which a payment spends together whatever their kind, and its
-- transactions. A portfolio has one pool for each network. The pool, not
-- the account, owns the summaries of transactions, their labels, the
-- pending transactions and the block the last sync reached: a transaction
-- that spends a BIP44 coin and pays a BIP84 address of the same portfolio
-- is one transaction of the pool, and moves nothing out of it.
--
-- A pool is the pair of its portfolio and its network. It has no row of
-- its own: the accounts of the portfolio on the network are its members,
-- and the rows it owns name the pair.
data Pool = Pool
  { poolPortfolio :: !DBPortfolioId,
    poolNetwork :: !Network
  }
  deriving (Eq, Show)

-- | The name of the network of a pool, as its rows store it
poolNetworkName :: Pool -> Text
poolNetworkName pool = cs pool.poolNetwork.name

{- Reading without changing anything -}

-- | Run a read of a database file that exists, for a caller that must
-- change nothing: the completion of the shell, the check of the version
-- of a database before hw opens it, and show of a database file, which
-- may be a backup ("Haskoin.Wallet.DatabaseFile"). The connection
-- does not create the file, refuses every write, and runs no migration. It
-- does not take the lock of the commands. A read runs alongside them, since
-- every hw database is in WAL mode: persistent sets it as hw opens one.
runDBReadOnly :: (MonadUnliftIO m) => FilePath -> DB m a -> m a
runDBReadOnly dbFile action = do
  uri <- liftIO $ fileUri dbFile
  runResourceT . runNoLoggingT $
    withSqliteConnInfo (connection uri) (runSqlConn action)
  where
    -- mode=rw opens only a file that exists. The connection could write,
    -- but query_only refuses it. A read-only connection would leave the
    -- -wal and -shm files of SQLite behind, which this one removes as it
    -- closes. persistent is kept from setting the journal mode, which is a
    -- write, and the foreign keys, which only writes check.
    connection :: String -> SqliteConnectionInfo
    connection uri =
      mkSqliteConnectionInfo (cs (uri <> "?mode=rw"))
        & walEnabled .~ False
        & fkEnabled .~ False
        & extraPragmas .~ ["PRAGMA query_only = ON;", "PRAGMA busy_timeout = " <> cs (show readOnlyBusyTimeout) <> ";"]

-- | How long a read waits while SQLite holds the file alone, in
-- milliseconds. A writer does not hold it, only a checkpoint or a recovery
-- for a moment. The wait is short, as the shell waits for the answer.
readOnlyBusyTimeout :: Int
readOnlyBusyTimeout = 100

-- | The file as a URI for SQLite, which then reads the options of the
-- connection after a ?: file:// and the absolute path. The characters a
-- URI gives a meaning, % ? and #, are written as their codes.
fileUri :: FilePath -> IO String
fileUri file = ("file://" <>) . concatMap escape <$> makeAbsolute file
  where
    escape '%' = "%25"
    escape '?' = "%3f"
    escape '#' = "%23"
    escape c = [c]

{- Undoing -}

-- | Run an action, then undo the writes it made, and only those. What the
-- command wrote before stays, and its transaction goes on. A savepoint of
-- SQLite marks the place to go back to. show uses this to take a file
-- of a wallet into the database as importwallet would, read what became
-- of it, and keep nothing of it ("Haskoin.Wallet.Labels"). The action
-- must not fail by an exception: the command fails with it then, and its
-- whole transaction is undone.
undoneAfter :: (MonadUnliftIO m) => DB m a -> DB m a
undoneAfter action = do
  rawExecute "SAVEPOINT undone" []
  result <- action
  rawExecute "ROLLBACK TO undone" []
  rawExecute "RELEASE undone" []
  return result

-- | Run an action that may refuse, and undo the writes it made when it
-- does, and only those: the database is then as the action found it. A
-- savepoint of SQLite marks the place to go back to, as for
-- 'undoneAfter'. importwallet takes a file into the database this way
-- ("Haskoin.Wallet.Labels"): a file that cannot be imported may be found
-- out once some of its portfolios are in, and none of them must stay.
--
-- A command undoes its whole transaction when it fails, so a refusal
-- never left anything behind for the user. This makes the refusal of the
-- action whole by itself, whoever calls it. Like 'undoneAfter', the
-- action must not fail by an exception.
--
-- sync puts the sync of each pool in one ("Haskoin.Wallet.Sync".'syncEach'):
-- a pool whose store fails is undone alone, and the pools synced before it
-- stay. Savepoints nest. A savepoint inside another one of the same name
-- is rolled back and released first, as SQLite takes the latest one of a
-- name.
undoneIfRefused :: (MonadUnliftIO m) => ExceptT e (DB m) a -> ExceptT e (DB m) a
undoneIfRefused action = ExceptT $ do
  rawExecute "SAVEPOINT refused" []
  result <- runExceptT action
  when (isLeft result) $ rawExecute "ROLLBACK TO refused" []
  rawExecute "RELEASE refused" []
  return result

{- The tables -}

-- | The tables of a new database, with their indexes
globalMigration :: (MonadUnliftIO m) => DB m ()
globalMigration = do
  void $ runMigrationQuiet migrateAll
  createIndexes

-- | The indexes persistent cannot declare, after the tables
createIndexes :: (MonadUnliftIO m) => DB m ()
createIndexes = forM_ tableIndexes $ \index -> rawExecute index []

{- Hashes and outpoints as the keys store them -}

-- | A transaction hash as the keys store it: the 32 bytes of its usual
-- hexadecimal form, so that a database tool shows the id as hw prints it
hashBytes :: TxHash -> KeyBytes
hashBytes = KeyBytes . BS.reverse . S.encode

-- | The transaction hash of a key
bytesToHash :: KeyBytes -> Either String TxHash
bytesToHash = S.decode . BS.reverse . keyBytes

-- | An outpoint as the keys store it: its 36 serialized bytes, the hash in
-- its internal order then the index, so that the coins of one transaction
-- are a range of keys
outpointBytes :: OutPoint -> KeyBytes
outpointBytes = KeyBytes . S.encode

-- | The outpoint of a key
bytesToOutpoint :: KeyBytes -> Either String OutPoint
bytesToOutpoint = S.decode . keyBytes

{- Query helpers -}

-- | How many values one query puts in an IN list at most: a thousand.
-- SQLite takes at most 32766 parameters in a statement, and the first
-- sync of a large wallet asks about every address of a pool at once,
-- which can be more. A query about more values goes in several, each one
-- short.
inListBatch :: Natural
inListBatch = 1000

-- like `maybe` but for a maybe/value sandwich
joinMaybe :: b -> (a -> b) -> Maybe (E.Value (Maybe a)) -> b
joinMaybe d f m =
  case m of
    Just (Value m') -> maybe d f m'
    Nothing -> d

flatMaybe :: Maybe (E.Value (Maybe a)) -> Maybe a
flatMaybe = joinMaybe Nothing Just
