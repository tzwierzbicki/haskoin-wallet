{-# LANGUAGE OverloadedStrings #-}

-- | The wallet database: the connection and its lock, and what every table
-- shares. The tables are those of the latest format version, defined in
-- "Haskoin.Wallet.Schema.V4" and exported again from here: a version that
-- changes the tables gets a new schema module, and this module moves to it.
-- Each kind of row has a module of its own: "Haskoin.Wallet.Account",
-- "Haskoin.Wallet.Address", "Haskoin.Wallet.Coin", "Haskoin.Wallet.TxInfo",
-- "Haskoin.Wallet.RawTx", "Haskoin.Wallet.PendingTx" and
-- "Haskoin.Wallet.BestBlock".
--
-- The tables store what hw learnt from outside. From the store: the
-- balances of the addresses, the transaction summaries, the coins, the raw
-- transactions, the best block, and whether a pending transaction is in
-- conflict, which only the store can tell. From the user or a signer: the
-- accounts, the labels, the pending transactions, the coins the user locked
-- and the fingerprint of a seed.
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
-- totals. The spendable balances depend on writes in many places, the
-- pending transactions of every account of a network and the best block
-- among them, and are computed: from the few columns they need, and for
-- the addresses shown only.
module Haskoin.Wallet.Database
  ( module Haskoin.Wallet.Database,
    module Haskoin.Wallet.Schema.V4,
    module Haskoin.Wallet.Schema.KeyBytes,
  )
where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Lens ((&), (.~))
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (ReaderT)
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Database.Esqueleto.Legacy as E
import Database.Persist.SqlBackend.Internal (SqlBackend (..))
import Database.Persist.Sqlite (extraPragmas, mkSqliteConnectionInfo, withSqliteConnInfo)
import Database.Sqlite (Error (..), SqliteException (..))
import Haskoin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Schema.KeyBytes
import Haskoin.Wallet.Schema.V4
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

-- like `maybe` but for a maybe/value sandwich
joinMaybe :: b -> (a -> b) -> Maybe (E.Value (Maybe a)) -> b
joinMaybe d f m =
  case m of
    Just (Value m') -> maybe d f m'
    Nothing -> d

flatMaybe :: Maybe (E.Value (Maybe a)) -> Maybe a
flatMaybe = joinMaybe Nothing Just
