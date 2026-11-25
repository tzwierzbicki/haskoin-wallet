{-# LANGUAGE OverloadedStrings #-}

-- | A database file as show shows it: the wallets and the portfolios of a
-- database of hw other than the one hw works in. It is for a copy the
-- user is about to put in place of the database: one that backup wrote,
-- or one of the auto-backups directory ("Haskoin.Wallet.Backup"). The
-- name of such a copy counts its wallets and its portfolios. This says
-- which ones they are.
--
-- restoredatabase reads a copy the same way before it puts it in place
-- of the database ('databaseToRestore'). It asks the user with what the
-- copy holds, and refuses the files show refuses. It also reads the
-- database itself that way, to say what the copy replaces ('heldNow').
--
-- = What is shown
--
-- The view holds what tells one database from another, and what the user
-- gets back by putting the copy in place: each wallet, with the
-- fingerprint of its seed and its description, and each of its
-- portfolios, with its description, the day it was created, and whether
-- it is the active one.
--
-- The rest of the database is left out on purpose.
--
-- * The history, the coins and the balances come back with a sync. A copy
--   holds them as its last sync left them. A balance shown from a copy
--   would read as the balance of today, which it is not.
--
-- * The labels are in the file of labels that goes with a copy, which
--   show shows too, as a file of a wallet ("Haskoin.Wallet.Labels").
--
-- The pending transactions are left out as well. They are the one thing a
-- copy holds that neither a sync nor a file of labels brings back.
--
-- = How a database file is told
--
-- By its first bytes, never by its name: every SQLite database starts
-- with the same 16 bytes ('startsDatabase'). show tells a transaction
-- file the same way. A copy the user renamed is still read as what it
-- is, and a file of labels named .sqlite by mistake is still read as a
-- file of labels.
--
-- = How it is read
--
-- Through a connection that writes nothing and runs no migration
-- ('Haskoin.Wallet.Database.runDBReadOnly'). The file is byte for byte
-- the same after it was shown. A copy hw made holds no write-ahead log,
-- so SQLite creates no file beside it either. A database file copied by
-- hand keeps the write-ahead log mode of the database. SQLite then makes
-- its two files beside it for the time of the reading, and removes them.
--
-- = Which versions are read
--
-- Only a database at the format version of this hw
-- ('latestDatabaseVersion'). The wallets and the portfolios are read with
-- the tables of this hw, which fit no other version. A database of
-- another version is refused, and the refusal gives its version.
--
-- That is the same answer hw gives today to such a database put in place
-- of its own, since hw 0.10 migrates none ("Haskoin.Wallet.Migration").
-- Once hw has a migration, the two will differ. hw will then migrate a
-- copy of the version before, and show will still refuse it. Reading the
-- two tables with plain SQL, whatever the version, was turned down: the
-- versions before 5 have no such tables, and a later version may change
-- them. The first migration should give show the tables of the version
-- it starts from.
module Haskoin.Wallet.DatabaseFile
  ( -- * Telling a database file
    databaseHeaderLength,
    startsDatabase,

    -- * What a database file holds
    DatabaseShown (..),
    WalletShown (..),
    PortfolioShown (..),
    databaseShown,
    databaseToRestore,
    heldNow,

    -- * In JSON
    databaseShownPairs,
    parseDatabaseShown,
  )
where

import Control.Exception (SomeException, displayException, fromException)
import Control.Monad (forM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Object, Pair, Parser)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Sqlite (Error (..), SqliteException (..))
import Haskoin (Fingerprint)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio (activePortfolioId, getPortfolios, getWallets, portfolioIndex, walletFingerprint, walletIndex)
import Haskoin.Wallet.Migration (StoredVersion (..), latestDatabaseVersion, storedVersion)
import Numeric.Natural (Natural)
import UnliftIO.Exception (tryAny)

{- Telling a database file -}

-- | What every SQLite database file starts with: these 15 characters and
-- a zero byte. The format of the file sets them, and no file of text, of
-- a transaction or of a wallet starts so.
databaseHeader :: ByteString
databaseHeader = "SQLite format 3\NUL"

-- | How many bytes of a file tell whether it is a database: 16
databaseHeaderLength :: Int
databaseHeaderLength = BS.length databaseHeader

-- | Whether the bytes a file starts with are those of an SQLite database
startsDatabase :: ByteString -> Bool
startsDatabase = (databaseHeader `BS.isPrefixOf`)

{- What a database file holds -}

-- | A database file as show shows it: its format version, and its
-- wallets in the order of their indexes. The version is always the one
-- of this hw, since show reads no other. The answer carries it so that
-- the screen and the JSON say which format the file is in.
data DatabaseShown = DatabaseShown
  { databaseShownVersion :: !Int,
    databaseShownWallets :: ![WalletShown]
  }
  deriving (Eq, Show)

-- | A wallet of a database file: its index, which names it there, the
-- fingerprint of its seed, which names it in any database, its
-- description, and its portfolios in the order of their indexes
data WalletShown = WalletShown
  { walletShownIndex :: !Natural,
    walletShownFingerprint :: !Fingerprint,
    walletShownDescription :: !Text,
    walletShownPortfolios :: ![PortfolioShown]
  }
  deriving (Eq, Show)

-- | A portfolio of a wallet of a database file: its index, whether it is
-- the active portfolio of that database, its description, and when it
-- was created
data PortfolioShown = PortfolioShown
  { portfolioShownIndex :: !Natural,
    -- | Whether the commands would work in it once the file is the
    -- database of hw
    portfolioShownActive :: !Bool,
    portfolioShownDescription :: !Text,
    portfolioShownCreated :: !UTCTime
  }
  deriving (Eq, Show)

-- | Read the wallets and the portfolios of a database file, which must
-- exist, without changing it. The answer is a refusal when the file is
-- not a database of hw at the format version of this hw, or when SQLite
-- cannot read it. Each refusal names the file as an SQLite database, so
-- that the user knows how show read it.
databaseShown :: FilePath -> IO (Either String DatabaseShown)
databaseShown = databaseRead ReadToShow

-- | Read the wallets and the portfolios of a database file that
-- restoredatabase is about to put in place of the database, without
-- changing it. It is read as show reads it, and refused as show refuses
-- it. The refusal says that the file cannot be restored.
databaseToRestore :: FilePath -> IO (Either String DatabaseShown)
databaseToRestore = databaseRead ReadToRestore

-- | The command a database file is read for, which the refusal of the
-- file names
data DatabaseReader = ReadToShow | ReadToRestore

-- | What the command would have done with the file, in its refusal
readerVerb :: DatabaseReader -> String
readerVerb ReadToShow = "shown"
readerVerb ReadToRestore = "restored"

-- | The name of the command, in its refusal
readerCommand :: DatabaseReader -> String
readerCommand ReadToShow = "show"
readerCommand ReadToRestore = "restoredatabase"

-- | Read the wallets and the portfolios of a database file for a
-- command, without changing the file
databaseRead :: DatabaseReader -> FilePath -> IO (Either String DatabaseShown)
databaseRead reader file = do
  result <- tryAny (runDBReadOnly file (runExceptT (heldBy reader)))
  return $ first refused (either (Left . failureReason) id result)
  where
    refused reason = "The SQLite database " <> file <> " cannot be " <> readerVerb reader <> ": " <> reason

-- | The wallets and the portfolios of the database of the connection,
-- when it is at the format version of this hw, or why it is not read.
-- restoredatabase reads the database of hw with it, in the transaction
-- that replaces it. That database is always at this version, since every
-- command refuses another one first.
heldNow :: ExceptT String (DB IO) DatabaseShown
heldNow = heldBy ReadToRestore

-- | The wallets and the portfolios of the database of the connection,
-- read for a command, or why that command does not read it
heldBy :: DatabaseReader -> ExceptT String (DB IO) DatabaseShown
heldBy reader = do
  stored <- lift storedVersion
  case stored of
    NoTables -> throwError "it has no tables, so it is not a database of hw"
    -- As any database of another program
    NoVersion -> throwError "it does not give a format version, so it is not a database of hw"
    AtVersion version
      | version /= latestDatabaseVersion -> throwError (otherVersion reader version)
      | otherwise -> lift $ do
          activeM <- activePortfolioId
          wallets <- getWallets
          DatabaseShown version <$> forM wallets (walletShown activeM)

-- | A wallet of the database of the connection with its portfolios, given
-- the key of the active portfolio, if the database has one
walletShown :: Maybe DBPortfolioId -> (DBWalletId, DBWallet) -> DB IO WalletShown
walletShown activeM (walletId, wallet) = do
  portfolios <- getPortfolios walletId
  return
    WalletShown
      { walletShownIndex = walletIndex wallet,
        walletShownFingerprint = walletFingerprint wallet,
        walletShownDescription = dBWalletDescription wallet,
        walletShownPortfolios =
          [ PortfolioShown
              { portfolioShownIndex = portfolioIndex row,
                portfolioShownActive = activeM == Just key,
                portfolioShownDescription = dBPortfolioDescription row,
                portfolioShownCreated = dBPortfolioCreated row
              }
            | (key, row) <- portfolios
          ]
      }

-- | Why a command does not read a database at another format version
-- than the one of this hw. It speaks of that command alone: whether hw
-- opens such a database in place of its own is for
-- "Haskoin.Wallet.Migration" to say.
otherVersion :: DatabaseReader -> Int -> String
otherVersion reader version =
  "it is a database of hw at format version "
    <> show version
    <> ", made by "
    <> (if version < latestDatabaseVersion then "an earlier" else "a newer")
    <> " hw, and "
    <> readerCommand reader
    <> " reads version "
    <> show latestDatabaseVersion
    <> " only"

-- | Why a database file could not be read, from the failure of its
-- reading. A database another program holds is said to be in use. Any
-- other failure of SQLite comes in its own words, such as "file is not a
-- database" for a file cut short.
failureReason :: SomeException -> String
failureReason failure = case fromException failure of
  Just (SqliteException ErrorBusy _ _) -> inUse
  Just (SqliteException ErrorLocked _ _) -> inUse
  Just refused@(SqliteException {}) -> "SQLite could not read it (" <> sqliteWords refused <> ")"
  Nothing -> displayException failure
  where
    inUse = "another program is using it"

{- In JSON -}

-- | The keys of a database file as the --json answer of show writes
-- them: its format version, and its wallets
databaseShownPairs :: DatabaseShown -> [Pair]
databaseShownPairs shown =
  [ "version" .= databaseShownVersion shown,
    "wallets" .= databaseShownWallets shown
  ]

-- | A database file back from the keys 'databaseShownPairs' writes
parseDatabaseShown :: Object -> Parser DatabaseShown
parseDatabaseShown o = DatabaseShown <$> o .: "version" <*> o .: "wallets"

instance ToJSON WalletShown where
  toJSON wallet =
    object
      [ "wallet" .= walletShownIndex wallet,
        "fingerprint" .= walletShownFingerprint wallet,
        "description" .= walletShownDescription wallet,
        "portfolios" .= walletShownPortfolios wallet
      ]

instance FromJSON WalletShown where
  parseJSON = withObject "wallet of a database file" $ \o ->
    WalletShown <$> o .: "wallet" <*> o .: "fingerprint" <*> o .: "description" <*> o .: "portfolios"

instance ToJSON PortfolioShown where
  toJSON portfolio =
    object
      [ "portfolio" .= portfolioShownIndex portfolio,
        "active" .= portfolioShownActive portfolio,
        "description" .= portfolioShownDescription portfolio,
        "created" .= portfolioShownCreated portfolio
      ]

instance FromJSON PortfolioShown where
  parseJSON = withObject "portfolio of a database file" $ \o ->
    PortfolioShown <$> o .: "portfolio" <*> o .: "active" <*> o .: "description" <*> o .: "created"
