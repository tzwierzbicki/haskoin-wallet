{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the shell completes from the wallet: the names of the accounts,
-- the hashes of the pending transactions at each step from preparetx to
-- sendtx, the transactions bumpfee replaces, the coins, the addresses, and
-- what label labels.
-- Each command is offered what it takes, and only when few enough values
-- match what was typed: see 'offeredAtMost'. The shell runs hw at each
-- Tab, so a completion reads the database and changes nothing. It writes
-- no configuration file and creates no database. It migrates nothing,
-- writes nothing, and does not wait for another command. When it cannot
-- read the database, the shell offers nothing for the word.
module Haskoin.Wallet.Cli.Completion
  ( offeredAtMost,
    accountNames,
    hashesToSign,
    idsToSend,
    pendingHashes,
    idsToBump,
    coinsToLock,
    coinsToUnlock,
    coinsToSpend,
    labelTargets,
    walletAddresses,
    addressesToSpendFrom,
    accountNamed,
    readForCompletion,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Char (isHexDigit)
import Data.Either (fromRight, rights)
import Data.List (isPrefixOf, nub, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network (..), Tx, TxHash, txHash, txHashToHex)
import Haskoin.Wallet.Database.Account (accountNetwork, getAccountByName, getAccountNames, getAccounts)
import Haskoin.Wallet.Database.Address (AddrType (..), addressesStarting)
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..), coinAddressesStarting, coinText, coinsStarting, spendableAtAddresses, spendingOf, toJsonCoin)
import Haskoin.Wallet.Config (configAsItIs, databaseFileIn)
import Haskoin.Wallet.Database (DB, DBAccount, DBAccountId, runDBReadOnly)
import Haskoin.Wallet.Migration (databaseVersion, latestDatabaseVersion)
import Haskoin.Wallet.Cli.Parser.Completion (completionParts)
import Haskoin.Wallet.Database.PendingTx (PendingTx (..), fromRow, pendingRows, pendingTxs)
import Haskoin.Wallet.Database.RawTx (lookupRawTx)
import Haskoin.Wallet.Tx.BumpFee (marksReplaceable)
import Haskoin.Wallet.Database.TxInfo (mempoolSummaries, ownTransaction, txidsStarting)
import Haskoin.Wallet.Tx.SignData (TxSignData (..), isSigned, shownHash)
import Numeric.Natural (Natural)
import qualified System.Directory as D
import System.Environment (getArgs)
import UnliftIO.Exception (handleAny)

{- How many -}

-- | How many values the shell is offered at most for a word. Among more,
-- the user would not pick one from the list, and types more of the value
-- instead, or copies it from a listing. The database is asked for one
-- more than this, to know when there are too many, and for no more: a Tab
-- costs a large wallet no more than a small one.
offeredAtMost :: Int
offeredAtMost = 20

-- | How many values the database is asked for: one more than are offered
askedFor :: Natural
askedFor = fromIntegral offeredAtMost + 1

-- | The values, when they are few enough to offer: none past
-- 'offeredAtMost'
fewEnough :: [a] -> [a]
fewEnough values
  | length (take (offeredAtMost + 1) values) > offeredAtMost = []
  | otherwise = values

{- Accounts -}

-- | The names of the accounts, for --account and the arguments that name
-- an account
accountNames :: IO [String]
accountNames = readForCompletion (map cs <$> getAccountNames)

-- | The account named among the words typed before the one to complete,
-- if any. The shell passes hw those words as its arguments.
typedAccount :: IO (Maybe Text)
typedAccount = do
  (indexM, typed, _) <- completionParts <$> getArgs
  return $ do
    index <- indexM
    -- The words before the one to complete, without the name of hw
    accountNamed (drop 1 (take index typed))

-- | The account the words name with --account or -a, as optparse reads
-- them: --account NAME, --account=NAME, -a NAME or -aNAME. Words after --
-- are arguments. bash breaks --account=NAME into words, which
-- 'completionParts' joins again; zsh and fish pass it whole.
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
hashesToSign :: String -> IO [String]
hashesToSign = shownHashesOf (\p -> not (isSigned (txOf p)) && not p.pendingTxOnline)

-- | The ids of the pending transactions to send, for sendtx: those signed
-- and not sent yet. The sync sends one again should the network lose it.
-- One in conflict is left out: the network refuses it, and deletetx
-- frees its coins.
idsToSend :: String -> IO [String]
idsToSend = shownHashesOf (\p -> isSigned (txOf p) && not p.pendingTxOnline && not p.pendingTxConflicted)

-- | The hash of every pending transaction, for exporttx and deletetx
pendingHashes :: String -> IO [String]
pendingHashes = shownHashesOf (const True)

-- | The pending transactions that pass the test, by the hash pendingtxs
-- shows: the nosigHash until a transaction is signed, then its id. The
-- pending transactions are few, since the sync retires them, so they are
-- all read, and those that start with the word typed kept. A row that
-- does not read is left out.
shownHashesOf :: (PendingTx -> Bool) -> String -> IO [String]
shownHashesOf keep typed = readForCompletion $ do
  held <- rights . map fromRow <$> pendingRows Nothing Nothing Nothing
  return $ fewEnough [hash | p <- held, keep p, let hash = hexOf (shownHash (txOf p)), typed `isPrefixOf` hash]

-- | The transaction a pending transaction holds
txOf :: PendingTx -> Tx
txOf = txSignDataTx . pendingTxData

hexOf :: TxHash -> String
hexOf = cs . txHashToHex

{- The transactions to replace -}

-- | The ids of the transactions bumpfee replaces, for its TXID, as it
-- checks them: on a network that replaces transactions, those the account
-- sent and that are still pending, and those of its history that are in
-- the mempool and its own payments. One replaced already is left out, and
-- one marked not replaceable when hw holds its bytes to tell. They are
-- few, since the network confirms or drops them: those of the account
-- named, or of every account, are all read.
idsToBump :: String -> IO [String]
idsToBump typed = do
  named <- typedAccount
  readForCompletion $ do
    accounts <- accountsNamed named
    found <- forM [held | held@(_, acc) <- accounts, (accountNetwork acc).replaceByFee] $ \(accId, acc) ->
      fromRight [] <$> runExceptT (replaceableOf (accountNetwork acc) accId)
    return $ fewEnough [hash | tid <- concat found, let hash = hexOf tid, typed `isPrefixOf` hash]

-- | The transactions of the account bumpfee replaces, by their ids
replaceableOf :: Network -> DBAccountId -> ExceptT String (DB IO) [TxHash]
replaceableOf net accId = do
  held <- pendingTxs accId
  let replaced = concatMap (txSignDataReplaces . pendingTxData) held
      sent = [tx | p <- held, p.pendingTxOnline, not p.pendingTxConflicted, let tx = txOf p, isSigned tx, marksReplaceable tx]
      sentIds = [txHash tx | tx <- sent, txHash tx `notElem` replaced]
  inMempool <- mempoolSummaries net accId askedFor
  own <- forM [tid | (tid, summary) <- inMempool, ownTransaction summary, tid `notElem` replaced, tid `notElem` sentIds] $ \tid -> do
    rawM <- lookupRawTx tid
    return [tid | maybe True marksReplaceable rawM]
  return (sentIds <> concat own)

{- The coins -}

-- | The coins free to lock, for lockcoin: those nothing locks, neither a
-- pending transaction nor the user, as lockcoin refuses the others
coinsToLock :: String -> IO [String]
coinsToLock = coinsWhere (isNothing . jsonCoinLock)

-- | The coins the user locked, for unlockcoin, the only ones it frees
coinsToUnlock :: String -> IO [String]
coinsToUnlock = coinsWhere ((== Just LockedByUser) . jsonCoinLock)

-- | The coins that can be spent now, for --coin. Coin control refuses a
-- choice none of whose coins can be spent.
coinsToSpend :: String -> IO [String]
coinsToSpend = coinsWhere jsonCoinSpendable

-- | The coins that start with the word typed and pass the test, as coins
-- lists them: TXID:INDEX. Among more than 'offeredAtMost' that start
-- with the word, locked or not, none.
coinsWhere :: (JsonCoin -> Bool) -> String -> IO [String]
coinsWhere keep typed = do
  named <- typedAccount
  readForCompletion $ do
    coins <- fewEnough <$> walletCoinsStarting named typed
    return [coinText (jsonCoinOutpoint coin) | (_, coin) <- coins, keep coin]

-- | The coins of the account named, or of every account, that start with
-- the word typed, as coins lists them: the hexadecimal digits of the word
-- are matched in the database, which gives one more than 'offeredAtMost'
-- of them at most from each account, and a colon and an index after the
-- digits are matched here, among those. Whether a coin is locked, and
-- whether it can be spent now, are those of the listing.
walletCoinsStarting :: Maybe Text -> String -> DB IO [(Network, JsonCoin)]
walletCoinsStarting named typed = do
  accounts <- accountsNamed named
  fmap concat . forM accounts $ \(accId, acc) -> do
    let net = accountNetwork acc
    rows <- fewEnough <$> coinsStarting accId (takeWhile isHexDigit (take 64 typed)) askedFor
    listed <- fromRight [] <$> runExceptT (spendingOf net accId >>= \rules -> mapM (toJsonCoin rules) rows)
    return [(net, coin) | coin <- listed, typed `isPrefixOf` coinText (jsonCoinOutpoint coin)]

{- What label labels -}

-- | What label labels, for its first argument, among what starts with the
-- word typed: the receiving addresses, since a change address takes no
-- label; the pending transactions, by the hash pendingtxs shows; and the
-- transactions of the history, by their ids. label takes no --account, so
-- every account counts. Among more than 'offeredAtMost' of them all, none.
labelTargets :: String -> IO [String]
labelTargets typed = readForCompletion $ do
  accounts <- getAccounts
  addresses <- concat <$> sequence [addressesStarting accId AddrExternal (cs typed) askedFor | (accId, _) <- accounts]
  held <- rights . map fromRow <$> pendingRows Nothing Nothing Nothing
  let pending = [hash | p <- held, let hash = hexOf (shownHash (txOf p)), typed `isPrefixOf` hash]
  -- A word that is no hexadecimal number starts no txid
  history <-
    if all isHexDigit typed
      then concat <$> sequence [txidsStarting accId typed askedFor | (accId, _) <- accounts]
      else return []
  return $ fewEnough (nub ((offeredAddress <$> addresses) <> pending <> (hexOf <$> history)))

{- The addresses -}

-- | Every address of the wallet, for signmsg, which signs with the key of
-- any of them
walletAddresses :: String -> IO [String]
walletAddresses = addressesOf [AddrExternal, AddrInternal]

-- | The addresses of the chains given, of the account named or of every
-- account, that start with the word typed: the database gives one more
-- than 'offeredAtMost' of them at most from each chain of each account
addressesOf :: [AddrType] -> String -> IO [String]
addressesOf chains typed = do
  named <- typedAccount
  readForCompletion $ do
    accounts <- accountsNamed named
    found <- concat <$> sequence [addressesStarting accId chain (cs typed) askedFor | (accId, _) <- accounts, chain <- chains]
    return $ fewEnough (offeredAddress <$> found)

-- | The addresses that hold coins that can be spent now, for --spend-from,
-- among the addresses that hold coins and start with the word typed. Coin
-- control refuses addresses that hold none.
addressesToSpendFrom :: String -> IO [String]
addressesToSpendFrom typed = do
  named <- typedAccount
  readForCompletion $ do
    accounts <- accountsNamed named
    found <- forM accounts $ \(accId, acc) -> do
      candidates <- fewEnough <$> coinAddressesStarting accId (cs typed) askedFor
      spendable <- fromRight Map.empty <$> runExceptT (spendableAtAddresses (accountNetwork acc) accId candidates)
      return (Map.keys spendable)
    return $ fewEnough (offeredAddress <$> concat found)

-- | An address as the shell offers it. A Bitcoin Cash address goes without
-- its prefix, such as bitcoincash:, because bash splits a word at a colon.
-- hw finds the address written either way.
offeredAddress :: Text -> String
offeredAddress = cs . snd . Text.breakOnEnd ":"

{- Reading the database -}

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
