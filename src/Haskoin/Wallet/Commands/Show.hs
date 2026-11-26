{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | show: what a word names, as its shape tells. The word is an address of
-- the wallet or of the address book, a path, a coin, a transaction,
-- pending or on the chain, or a file: a transaction file, a file of a
-- wallet, or a database file. show changes nothing.
module Haskoin.Wallet.Commands.Show
  ( cmdShow,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Data.Text.Encoding (decodeUtf8')
import Haskoin
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (addressAnswer, coinAnswer, txAnswer)
import Haskoin.Wallet.Commands.Contact (NamedAddress (..), namedAddress, notHeldToShow)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.ShortId (shownViews, walletCoinOf, walletTxHashOf)
import Haskoin.Wallet.Commands.TxFile (TxFileRead (..), fileNetwork, txFileForm, txFileOfBytes)
import Haskoin.Wallet.Config
import Haskoin.Wallet.CutAddress (cutAddress)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.File (databaseHeaderLength, databaseShown, startsDatabase)
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (parseOutsideTxSignData, parseTxFile)
import Haskoin.Wallet.Util
import Haskoin.Wallet.WalletFile (AccountImport (..), FileShown (..), ImportScope (..), fileShown, holdsAnything, readWalletFile, wholeFile)
import System.Directory (doesPathExist)

-- | Show what the word names, as its shape tells ('Shown'), or without a
-- word the newest receiving address of the account. Nothing is generated
-- or stored.
--
-- A path and the newest address are of the account the options pick in
-- the active portfolio, as for receive. A path that starts with its type,
-- as the views write it (bip84\/0\/3), names its account itself. An address
-- and a coin tell where they are too, so --type is refused with them, and
-- with such a path, rather than ignored: the user who typed it may have
-- meant another thing to show. --network then only tells apart an address
-- or a coin the wallet has on several networks, as the test networks share
-- their keys. A transaction is shown as each
-- portfolio that holds it sees it, whichever is active. A file of a
-- wallet is read as importwallet reads it, with --wallet and --portfolio,
-- which name one wallet and one portfolio of the file, and go with no
-- other word. A database file takes no option: show lists every wallet
-- and every portfolio of it (@shownDatabaseFile@).
cmdShow :: Config -> Target -> ImportScope -> Maybe Shown -> IO Response
cmdShow cfg target fileScope shownM =
  runDBResponse cfg $ do
    when (fileScope /= wholeFile && not (namesFile shownM)) $ throwError walletFileScopeOnly
    case shownM of
      Just (ShownFile file) -> shownFile cfg target fileScope file
      Nothing -> newestReceiving cfg target
      Just (ShownPath Nothing addrType index) -> addressAtPath cfg target addrType index
      Just (ShownPath (Just kind) addrType index) -> do
        refuseType "a path that starts with its type"
        addressAtPath cfg (Target target.network (Just kind)) addrType index
      Just (ShownAddress typed) -> do
        refuseType "an address"
        named <- namedAddress (notHeldToShow cfg typed) cfg target.network typed
        case named of
          OwnAddress accId addr -> addressAnswer accId addr
          BookAddress net contact -> return (ResponseContact net contact)
      Just (ShownTx named) -> do
        refuseType "a transaction"
        shownTxs cfg target named
      Just (ShownCoin named) -> do
        refuseType "a coin"
        held <- walletCoin cfg target.network =<< walletCoinOf named
        coinAnswer held =<< shownCoin held
  where
    namesFile = \case
      Just (ShownFile _) -> True
      _ -> False
    refuseType what =
      when (isJust target.kind) $
        throwError ("show takes no --type with " <> what)

-- | The refusal of show --wallet or --portfolio with anything but a file
-- of a wallet. The two options name a wallet and a portfolio of such a
-- file, not where show looks.
walletFileScopeOnly :: String
walletFileScopeOnly =
  "show takes --wallet and --portfolio only with a file of a wallet, where they name one wallet and one portfolio of the file. The rest is shown in the active portfolio, or where it is"

-- | The newest receiving address of the account, the first one addresses
-- lists, to give out again
newestReceiving :: Config -> Target -> ExceptT String (DB IO) Response
newestReceiving cfg target = do
  portfolio <- requireActivePortfolio
  (accId, _) <- chooseAccount cfg portfolio target.network target.kind
  newest <- lift $ addressesAfter accId AddrExternal Nothing (Just 1)
  case newest of
    addr : _ -> addressAnswer accId addr
    [] -> throwError "The account has no receiving address yet: receive gives out the first one"

-- | The address of the account at an index of one of its chains, derived
-- from the public key of the account: what an offline computer shows to
-- check an address the online computer gave out. The address shown is
-- always the one the key derives. When the wallet holds it, it is shown
-- as receive shows one, with its label, its balance and its QR code. When
-- the wallet does not, it is shown cut, with its middle hidden and
-- without a code ("Haskoin.Wallet.CutAddress"). receive did not give it
-- out, and it may lie past the gap that a restored wallet looks at, so
-- nobody must be able to pay it. Its ends are enough to compare it with
-- the address the online computer shows.
addressAtPath :: Config -> Target -> AddrType -> KeyIndex -> ExceptT String (DB IO) Response
addressAtPath cfg target addrType index = do
  portfolio <- requireActivePortfolio
  (accId, acc) <- chooseAccount cfg portfolio target.network target.kind
  let path = addrDeriv addrType :/ index
  addr <- derivedAddressAt acc addrType index
  -- The row is looked up by the address derived, not by its path, so that
  -- the answer shows what the key derives in either case
  held <- lift $ storedAmong [addr] (accId, acc)
  case held of
    row : _ -> addressAnswer accId row
    [] -> ResponseDerivedAddr (accountRef portfolio acc) path <$> liftMaybe "An account derives no address of this kind" (cutAddress (accountNetwork acc) addr)

-- | A file: a database file, a transaction file or a file of a wallet,
-- as its bytes tell. The wallet keeps nothing of any of them.
--
-- A database is told first, by the bytes every SQLite database starts
-- with ('startsDatabase'), read alone: a database may be large, and show
-- opens it rather than read it whole. Any other file is read whole, and
-- is a transaction file when its bytes are in one of its forms
-- ('txFileForm'), or else a file of a wallet. The three kinds cannot be
-- taken for each other: the name of a file tells nothing, so a copy of
-- the database and the file of its labels are told apart even when the
-- user swapped their names.
--
-- show takes for a file any word of no other shape ('readShown'). A word
-- with no slash that names no file may have been meant as something else,
-- a short id with a digit missing for instance. The answer then names the
-- five kinds show takes. A word with a slash is a file path, and gets the
-- answer of any command that reads a missing file.
shownFile :: Config -> Target -> ImportScope -> FilePath -> ExceptT String (DB IO) Response
shownFile cfg target fileScope file = do
  exists <- liftIO $ doesPathExist file
  unless (exists || '/' `elem` file) $
    throwError $
      file
        <> " is not a path, an address, a transaction or a coin, and no file has that name: show takes a path such as /0/3,"
        <> " an address of the wallet, a txid or a nosigHash, a coin TXID:INDEX, or a file of a transaction, of a wallet or of a database,"
        <> " each hash in full or by its short id, its first 8 digits"
  when (isJust target.kind) $ throwError "show takes no --type with a file"
  start <- liftEitherIO $ readFileStart databaseHeaderLength file
  if startsDatabase start
    then shownDatabaseFile target fileScope file
    else do
      bytes <- liftEitherIO $ readFileBytes file
      case txFileForm bytes of
        Just _ -> do
          when (fileScope /= wholeFile) $ throwError walletFileScopeOnly
          shownTxFile cfg target file bytes
        Nothing -> shownWalletFile cfg target fileScope file bytes

-- | A database file: its wallets and their portfolios, read from the file
-- without changing it ("Haskoin.Wallet.Database.File"). It is what the
-- user looks at before putting a copy of the database in place of the
-- database. The file is read through a connection of its own, apart from
-- the one of the command, which stays on the database of hw.
--
-- No option narrows the view. A database holds few wallets and
-- portfolios, and the user wants to see all that comes back. So --network,
-- --wallet and --portfolio are refused rather than ignored, as show
-- refuses any option that does not go with its word. Each refusal says
-- that the file is an SQLite database: --wallet and --portfolio go with a
-- file of a wallet, and the user may have taken this file for one.
shownDatabaseFile :: Target -> ImportScope -> FilePath -> ExceptT String (DB IO) Response
shownDatabaseFile target fileScope file = do
  when (isJust target.network) $
    throwError (noOption "--network" <> ": its wallets and their portfolios are on every network")
  when (fileScope /= wholeFile) $
    throwError (noOption "--wallet and no --portfolio" <> ". They name one wallet and one portfolio of a BIP 329 file of a wallet. Every wallet and every portfolio of a database is shown")
  ResponseDatabaseFile file <$> liftEitherIO (databaseShown file)
  where
    noOption options = "show takes no " <> options <> " with " <> file <> ", which is an SQLite database"

-- | The transaction of a file, as the portfolio it involves sees it. One
-- that involves no portfolio is shown as outside the wallet, on the
-- network of --network or of the configuration when the file names none.
-- The labels shown are those the file carries, not those of this wallet,
-- since the file is what an offline computer has. hw finds the portfolio
-- ('transactionPool'): a transfer between two portfolios is shown as the
-- one that pays sees it. An address it pays that another portfolio of the
-- database holds is named so, and so is one of the address book
-- ('fillAddresses'): no file carries that.
shownTxFile :: Config -> Target -> FilePath -> BS.ByteString -> ExceptT String (DB IO) Response
shownTxFile cfg target file bytes = do
  TxFileRead txFile@(TxFile netM tsd _ _) _ <- txFileOfBytes (commandNetwork cfg target.network) file bytes
  named <- fileNetwork target.network netM
  activeM <- lift activePortfolioId
  (poolM, completed) <- transactionPool cfg activeM named tsd
  case poolM of
    Just pool -> do
      keys <- lift $ poolKeys pool
      info <- liftEither (parseTxFile pool.poolNetwork keys txFile {txFileData = completed})
      txAnswer pool =<< lift (fillAddresses pool info)
    Nothing -> do
      let net = commandNetwork cfg named
      ResponseOutsideTx net <$> (lift . fillContacts net =<< liftEither (parseOutsideTxSignData net tsd))

-- | A file of a wallet, as importwallet reads it: each wallet and each
-- portfolio as importwallet would leave it, with the labels and locks of
-- its accounts ("Haskoin.Wallet.WalletFile"). The file is refused when it
-- holds nothing a file of a wallet holds. It is no database and no
-- transaction file either, so the refusal names the three kinds.
--
-- The accounts of the file on the networks hw hides are left out, with
-- their labels and locks, as every answer leaves those networks out.
-- importwallet still imports them: the file is the whole wallet. An
-- account importwallet would leave aside is shown whatever its network,
-- since that is a problem of the file to tell.
shownWalletFile :: Config -> Target -> ImportScope -> FilePath -> BS.ByteString -> ExceptT String (DB IO) Response
shownWalletFile cfg target fileScope file bytes = do
  when (isJust target.network) $
    throwError "show takes no --network with a file of a wallet: each of its records names its network"
  content <- either (const (throwError neither)) return (decodeUtf8' bytes)
  let walletFile = readWalletFile content
  unless (holdsAnything walletFile) $ throwError neither
  shown <- fileShown fileScope walletFile
  return $ ResponseWalletFile file shown {shownAccounts = filter (onShownNetwork . fst) (shownAccounts shown)}
  where
    onShownNetwork = \case
      AccountAdded ref -> isShown cfg (accountRefNetwork ref)
      AccountHeld ref -> isShown cfg (accountRefNetwork ref)
      AccountLeftAside _ _ -> True
    neither = couldNotRead file "Neither a transaction file, a PSBT in bytes or in base64 text or the hexadecimal of a transaction, nor a file of a wallet that importwallet reads, nor an SQLite database"

-- | A transaction of the wallet, pending or on the chain, as each
-- portfolio that holds it sees it, on the networks hw shows, or on the
-- --network when given. The short id is matched among the transactions of
-- every portfolio, as label matches it.
shownTxs :: Config -> Target -> HashRef -> ExceptT String (DB IO) Response
shownTxs cfg target named = do
  hash <- walletTxHashOf named
  views <- shownViews cfg named =<< transactionViews hash
  let kept =
        [ (place, info)
          | (pool, place, info) <- views,
            maybe True (== pool.poolNetwork) target.network
        ]
  when (null kept) $
    throwError $
      "The transaction "
        <> hashRefText named
        <> " is not in the wallet"
        <> maybe "" ((" on " <>) . (.name)) target.network
  ResponseHeldTxs kept <$> activePortfolioRef
