{-# LANGUAGE OverloadedStrings #-}

-- | The wallet database: the connection and its lock, and what every table
-- shares. The tables are those of the latest format version, defined in
-- "Haskoin.Wallet.Schema.V7" and exported again from here: a version that
-- changes the tables gets a new schema module, and this module moves to it.
-- Each kind of row has a module of its own: "Haskoin.Wallet.Account",
-- "Haskoin.Wallet.Address", "Haskoin.Wallet.Coin", "Haskoin.Wallet.TxInfo",
-- "Haskoin.Wallet.RawTx", "Haskoin.Wallet.PendingTx" and
-- "Haskoin.Wallet.BestBlock".
module Haskoin.Wallet.Database
  ( module Haskoin.Wallet.Database,
    module Haskoin.Wallet.Schema.V7,
  )
where

import Conduit (MonadUnliftIO, ResourceT, runResourceT)
import Control.Lens ((&), (.~))
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader (ReaderT)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Esqueleto.Legacy as E
import Database.Persist.SqlBackend.Internal (SqlBackend (..))
import Database.Persist.Sqlite (extraPragmas, mkSqliteConnectionInfo, withSqliteConnInfo)
import Database.Sqlite (Error (..), SqliteException (..))
import Haskoin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Schema.V7
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

globalMigration :: (MonadUnliftIO m) => DB m ()
globalMigration = void $ runMigrationQuiet migrateAll

{- Outpoints as text -}

outpointText :: OutPoint -> Text
outpointText = encodeHex . S.encode

textToOutpoint :: Text -> Either String OutPoint
textToOutpoint t = do
  bs <- maybeToEither "textToOutpoint: invalid input" $ decodeHex t
  S.decode bs

{- Query helpers -}

-- like `maybe` but for a maybe/value sandwich
joinMaybe :: b -> (a -> b) -> Maybe (E.Value (Maybe a)) -> b
joinMaybe d f m =
  case m of
    Just (Value m') -> maybe d f m'
    Nothing -> d

flatMaybe :: Maybe (E.Value (Maybe a)) -> Maybe a
flatMaybe = joinMaybe Nothing Just
