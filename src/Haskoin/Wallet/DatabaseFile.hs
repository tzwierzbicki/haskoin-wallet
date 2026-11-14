{-# LANGUAGE OverloadedStrings #-}

-- | A database file as show shows it: the wallets and the portfolios of a
-- database of hw other than the one hw works in. It is for a copy the
-- user is about to put in place of the database: one that backup wrote,
-- or one of the auto-backups directory ("Haskoin.Wallet.Backup"). The
-- name of such a copy counts its wallets and its portfolios. This says
-- which ones they are.
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
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Database.Sqlite (Error (..), SqliteException (..))
import Haskoin (Fingerprint)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio (activePortfolioId, getPortfolios, getWallets, portfolioIndex, walletFingerprint, walletIndex)
import Haskoin.Wallet.Migration (databaseVersion, latestDatabaseVersion)
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
databaseShown file = do
  result <- tryAny (runDBReadOnly file (runExceptT heldNow))
  return $ first cannotShow (either (Left . failureReason) id result)
  where
    cannotShow reason = "The SQLite database " <> file <> " cannot be shown: " <> reason

-- | The wallets and the portfolios of the database of the connection,
-- when it is at the format version of this hw, or why it is not read
heldNow :: ExceptT String (DB IO) DatabaseShown
heldNow = do
  versionM <- databaseVersion
  case versionM of
    Nothing -> throwError "it has no tables, so it is not a database of hw"
    -- A database without the table of the version reads as version 1,
    -- the format of hw 0.8. Any database of another program reads so too.
    Just 1 -> throwError "it does not give a format version, so it is not a database of hw, or it is one of hw 0.8"
    Just version
      | version /= latestDatabaseVersion -> throwError (otherVersion version)
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

-- | Why show does not read a database at another format version than the
-- one of this hw. It speaks of show alone: whether hw opens such a
-- database in place of its own is for "Haskoin.Wallet.Migration" to say.
otherVersion :: Int -> String
otherVersion version =
  "it is a database of hw at format version "
    <> show version
    <> ", made by "
    <> (if version < latestDatabaseVersion then "an earlier" else "a newer")
    <> " hw, and show reads version "
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

-- | What SQLite says of a failure, without what persistent puts around
-- it: persistent writes the message of SQLite after a colon and a space.
-- When it has no message, the name of the failure stands for it.
sqliteWords :: SqliteException -> String
sqliteWords refused = case Text.stripPrefix ": " (seDetails refused) of
  Just said | not (Text.null said) -> cs said
  _ -> show (seError refused)

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
