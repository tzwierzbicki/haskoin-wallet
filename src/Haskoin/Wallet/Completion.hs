{-# LANGUAGE OverloadedStrings #-}

-- | What the shell completes from the wallet: the names of the accounts,
-- the hashes of the pending transactions at each step from preparetx to
-- sendtx, the coins and the addresses. Each command is offered what it
-- takes. The shell runs hw at each Tab, so a completion reads the database
-- and changes nothing. It writes no configuration file and creates no
-- database. It migrates nothing, writes nothing, and does not wait for
-- another command. When it cannot read the database, the shell offers
-- nothing for the word.
module Haskoin.Wallet.Completion
  ( accountNames,
    hashesToSign,
    idsToSend,
    pendingHashes,
    coinsToLock,
    coinsToUnlock,
    coinsToSpend,
    receivingAddresses,
    walletAddresses,
    addressesToSpendFrom,
    accountNamed,
    readForCompletion,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (runExceptT)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight, rights)
import Data.List (stripPrefix)
import Data.Maybe (isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network, Tx, addrToText, txHashToHex)
import Haskoin.Wallet.Account (accountNetwork, getAccountByName, getAccountNames, getAccounts)
import Haskoin.Wallet.Address (AddrType (..), addressesAfter)
import Haskoin.Wallet.Coin (CoinFilter (..), CoinLock (..), JsonCoin (..), coinText, coinsAfter)
import Haskoin.Wallet.Config (configAsItIs, databaseFileIn)
import Haskoin.Wallet.Database (DB, DBAccount, DBAccountId, DBAddress (..), runDBReadOnly)
import Haskoin.Wallet.Migration (databaseVersion, latestDatabaseVersion)
import Haskoin.Wallet.Parser.Completion (completionParts)
import Haskoin.Wallet.PendingTx (PendingTx (..), fromRow, pendingRows)
import Haskoin.Wallet.TxSignData (TxSignData (..), isSigned, shownHash)
import qualified System.Directory as D
import System.Environment (getArgs)
import Text.Read (readMaybe)
import UnliftIO.Exception (handleAny)

-- | The names of the accounts, for --account and the arguments that name
-- an account
accountNames :: IO [String]
accountNames = readForCompletion (map cs <$> getAccountNames)

{- The account typed -}

-- | The account named among the words typed before the one to complete,
-- if any. The shell passes hw those words as its arguments.
typedAccount :: IO (Maybe Text)
typedAccount = do
  (indexM, typed, _) <- completionParts <$> getArgs
  return $ do
    index <- readMaybe =<< indexM
    -- The words before the one to complete, without the name of hw
    accountNamed (drop 1 (take index typed))

-- | The account the words name with --account or -a, as optparse reads
-- them: --account NAME, --account=NAME, -a NAME or -aNAME. Words after --
-- are arguments.
accountNamed :: [String] -> Maybe Text
accountNamed = go . takeWhile (/= "--")
  where
    go :: [String] -> Maybe Text
    go (option : name : _) | option == "--account" || option == "-a" = Just (cs name)
    go (word : rest)
      | Just name <- stripPrefix "--account=" word = Just (cs name)
      | Just name@(_ : _) <- stripPrefix "-a" word = Just (cs name)
      | otherwise = go rest
    go [] = Nothing

-- | The account of the name, or every account without one. A name no
-- account has gives none.
accountsNamed :: Maybe Text -> DB IO [(DBAccountId, DBAccount)]
accountsNamed Nothing = getAccounts
accountsNamed (Just name) = either (const []) pure <$> runExceptT (getAccountByName (Just name))

{- The pending transactions -}

-- | The nosigHashes of the pending transactions to sign, for signtx and
-- signsweep: those neither signed nor sent, as the two refuse the others
hashesToSign :: IO [String]
hashesToSign = shownHashesOf (\p -> not (isSigned (txOf p)) && not (pendingTxOnline p))

-- | The ids of the pending transactions to send, for sendtx: those signed
-- and not sent yet. The sync sends one again should the network lose it.
-- One in conflict is left out: the network refuses it, and deletetx
-- frees its coins.
idsToSend :: IO [String]
idsToSend = shownHashesOf (\p -> isSigned (txOf p) && not (pendingTxOnline p) && not (pendingTxConflicted p))

-- | The hash of every pending transaction, for exporttx and deletetx
pendingHashes :: IO [String]
pendingHashes = shownHashesOf (const True)

-- | The pending transactions that pass the test, by the hash pendingtxs
-- shows: the nosigHash until a transaction is signed, then its id. A row
-- that does not read is left out.
shownHashesOf :: (PendingTx -> Bool) -> IO [String]
shownHashesOf keep = readForCompletion $ do
  held <- rights . map fromRow <$> pendingRows Nothing Nothing Nothing
  return [cs (txHashToHex (shownHash (txOf p))) | p <- held, keep p]

-- | The transaction a pending transaction holds
txOf :: PendingTx -> Tx
txOf = txSignDataTx . pendingTxData

{- The coins -}

-- | The coins free to lock, for lockcoin: those nothing locks, neither a
-- pending transaction nor the user, as lockcoin refuses the others
coinsToLock :: IO [String]
coinsToLock = coinsWhere (isNothing . jsonCoinLock)

-- | The coins the user locked, for unlockcoin, the only ones it frees
coinsToUnlock :: IO [String]
coinsToUnlock = coinsWhere ((== Just LockedByUser) . jsonCoinLock)

-- | The coins that can be spent now, for --coin. Coin control refuses a
-- choice none of whose coins can be spent.
coinsToSpend :: IO [String]
coinsToSpend = coinsWhere jsonCoinSpendable

-- | The coins that pass the test, as coins lists them: TXID:INDEX
coinsWhere :: (JsonCoin -> Bool) -> IO [String]
coinsWhere keep = do
  named <- typedAccount
  readForCompletion $ do
    coins <- walletCoins named
    return [coinText (jsonCoinOutpoint coin) | (_, coin) <- coins, keep coin]

-- | The coins of the account named, or of every account, as coins lists
-- them, each with the network of its account. Whether a coin is locked,
-- and whether it can be spent now, are those of the listing.
walletCoins :: Maybe Text -> DB IO [(Network, JsonCoin)]
walletCoins named = do
  accounts <- accountsNamed named
  fmap concat . forM accounts $ \(accId, acc) -> do
    let net = accountNetwork acc
    coins <- fromRight [] <$> runExceptT (coinsAfter net accId AllCoins Nothing Nothing)
    return [(net, coin) | coin <- coins]

{- The addresses -}

-- | The receiving addresses, for label: a change address takes no label
receivingAddresses :: IO [String]
receivingAddresses = addressesOf [AddrExternal]

-- | Every address of the wallet, for signmsg, which signs with the key of
-- any of them
walletAddresses :: IO [String]
walletAddresses = addressesOf [AddrExternal, AddrInternal]

-- | The addresses of the chains given, of the account named or of every
-- account
addressesOf :: [AddrType] -> IO [String]
addressesOf chains = do
  named <- typedAccount
  readForCompletion $ do
    accounts <- accountsNamed named
    rows <- concat <$> sequence [addressesAfter accId chain Nothing Nothing | (accId, _) <- accounts, chain <- chains]
    return (offeredAddress . dBAddressAddress <$> rows)

-- | The addresses that hold coins that can be spent now, for --spend-from.
-- Coin control refuses addresses that hold none.
addressesToSpendFrom :: IO [String]
addressesToSpendFrom = do
  named <- typedAccount
  readForCompletion $ do
    coins <- walletCoins named
    return $ nubOrd [offeredAddress addr | (net, coin) <- coins, jsonCoinSpendable coin, Just addr <- [addrToText net (jsonCoinAddress coin)]]

-- | An address as the shell offers it. A Bitcoin Cash address goes without
-- its prefix, such as bitcoincash:, because bash splits a word at a colon.
-- hw finds the address written either way.
offeredAddress :: Text -> String
offeredAddress = cs . snd . Text.breakOnEnd ":"

-- | Run a read of the wallet database for the shell, or give nothing. The
-- database is the one hw works on: ~/.hw/accounts.sqlite, or the file
-- the configuration names. It gives nothing when there is no database,
-- when the database is of another format version than this hw's, and on
-- any failure.
readForCompletion :: DB IO [a] -> IO [a]
readForCompletion action = handleAny (const (return [])) $ do
  -- The directory of hw, without creating it
  dir <- D.getAppUserDataDirectory "hw"
  configM <- configAsItIs dir
  case configM of
    Nothing -> return []
    Just cfg -> do
      let file = databaseFileIn dir cfg
      exists <- D.doesFileExist file
      if exists then runDBReadOnly file readCurrent else return []
  where
    -- A database of an older version is migrated by the next command, and
    -- one of a newer version is refused. Their tables may differ.
    readCurrent = do
      version <- runExceptT databaseVersion
      if version == Right (Just latestDatabaseVersion) then action else return []
