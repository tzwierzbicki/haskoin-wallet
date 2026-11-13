{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the shell completes from the wallet: the indexes of the wallets
-- and of their portfolios, the hashes of the pending transactions at each
-- step from preparetx to broadcast, the transactions bumpfee replaces,
-- the coins, the addresses, what label labels, and what show shows.
-- Each command is offered what it takes, and only when few enough values
-- match what was typed: see 'offeredAtMost'. The shell runs hw at each
-- Tab, so a completion reads the database and changes nothing. It writes
-- no configuration file and creates no database. It migrates nothing,
-- writes nothing, and does not wait for another command. When it cannot
-- read the database, the shell offers nothing for the word.
--
-- Most values are offered from every portfolio: the commands that take
-- them find their portfolio by themselves. Coin control takes its coins
-- and addresses from the active portfolio alone, and refuses the others,
-- so --coin, --spend-from and --change-back are offered those of the
-- active portfolio ('Looked'). The options typed before the word narrow
-- what is offered ('Typed'): --wallet the portfolios of switch, and
-- --network and --type the accounts. The defaults of the configuration
-- are not applied. Without an option, every network hw shows counts, so
-- that the shell offers a value the user may still narrow down with an
-- option typed after it.
--
-- Nothing is offered from a hidden network: neither its name for
-- --network, nor an address, a coin or a transaction the wallet has on it
-- ("Haskoin.Wallet.Config"). The commands refuse them, and a Tab that
-- offered them would show what the user hid.
--
-- A value is offered in the form typed, so that the word goes on as the
-- user started it: a coin by the short id of its txid after a short id
-- and a colon, and a Bitcoin Cash address with its prefix after the
-- prefix. bash then keeps the part up to the colon and inserts the rest
-- ("Haskoin.Wallet.Cli.Parser.Completion").
module Haskoin.Wallet.Cli.Completion
  ( offeredAtMost,
    walletIndexes,
    portfolioIndexes,
    shownNetworkNames,
    hiddenNetworkNames,
    hashesToSign,
    idsToSend,
    pendingHashes,
    idsToBump,
    coinsToLock,
    coinsToUnlock,
    coinsToSpend,
    labelTargets,
    shownTargets,
    walletAddresses,
    activeAddresses,
    addressesToSpendFrom,
    Looked (..),
    Typed (..),
    optionsTyped,
    readForCompletion,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Char (isDigit, isHexDigit)
import Data.Either (fromRight, rights)
import Data.List (isPrefixOf, nub, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network (..), OutPoint (..), Tx, TxHash, allNets, netByName, txHash, txHashToHex)
import Haskoin.Wallet.Cli.Parser.Completion (completionParts)
import Haskoin.Wallet.Config (Config (..), configAsItIs, databaseFileIn, isShown)
import Haskoin.Wallet.Database (DB, DBAccount, DBAccountId, Pool (..), runDBReadOnly)
import Haskoin.Wallet.Database.Account (accountKind, accountNetwork, accountPool, getAccounts)
import Haskoin.Wallet.Database.Address (AddrType (..), addressesStarting)
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..), coinAddressesStarting, coinText, coinsStarting, spendableAtAddresses, spendingOf, toJsonCoin)
import Haskoin.Wallet.Database.PendingTx (PendingTx (..), fromRow, pendingRows, pendingTxs)
import Haskoin.Wallet.Database.Portfolio (Portfolio (..), activePortfolioId, getAllPortfolios, portfolioIndex, walletIndex)
import Haskoin.Wallet.Database.RawTx (lookupRawTx)
import Haskoin.Wallet.Database.TxInfo (mempoolSummaries, ownTransaction, txidsStarting)
import Haskoin.Wallet.Keys (AccountKind, indexFits, kindNamed, portfolioNetworks)
import Haskoin.Wallet.Migration (databaseVersion, latestDatabaseVersion)
import Haskoin.Wallet.Tx.BumpFee (marksReplaceable)
import Haskoin.Wallet.Tx.SignData (TxSignData (..), isSigned, shownHash)
import Haskoin.Wallet.Util (readNatural)
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

{- Wallets and portfolios -}

-- | The indexes of the wallets, for the --wallet of switch, exportwallet
-- and deletewallet
walletIndexes :: IO [String]
walletIndexes = readForCompletion $ \_ -> do
  portfolios <- getAllPortfolios
  return (nub [show (walletIndex p.portfolioWallet) | p <- portfolios])

-- | The indexes of the portfolios, for the argument and the --portfolio
-- of switch, and the --portfolio of deleteportfolio and exportwallet:
-- those of the wallet --wallet names among the words typed, or else of
-- the wallet of the active portfolio, where the three look without
-- --wallet. A database without an active portfolio has no portfolio, and
-- nothing is offered.
portfolioIndexes :: IO [String]
portfolioIndexes = do
  typed <- typedOptions
  readForCompletion $ \_ -> do
    portfolios <- getAllPortfolios
    activeM <- activePortfolioId
    let activeWallet = [walletIndex p.portfolioWallet | p <- portfolios, Just p.portfolioId == activeM]
        wallet = maybe (listToMaybe activeWallet) Just typed.typedWallet
    return (nub [show (portfolioIndex p.portfolioRow) | p <- portfolios, Just (walletIndex p.portfolioWallet) == wallet])

-- | What the options typed before the word to complete say: the wallet of
-- switch or of exportwallet, and the network and the type of the accounts
-- a value is offered from, each when its option was typed
data Typed = Typed
  { typedWallet :: !(Maybe Natural),
    typedNetwork :: !(Maybe Network),
    typedKind :: !(Maybe AccountKind)
  }
  deriving (Eq, Show)

-- | The options among the words typed before the one to complete. The
-- shell passes hw those words as its arguments.
typedOptions :: IO Typed
typedOptions = do
  (indexM, typed, _) <- completionParts <$> getArgs
  return $ case indexM of
    -- The words before the one to complete, without the name of hw
    Just index -> optionsTyped (drop 1 (take index typed))
    Nothing -> optionsTyped []

-- | What the words say with --wallet, --network and --type, as optparse
-- reads them: --wallet N or --wallet=N, and the short forms -n NET and
-- -t TYPE, with or without a space. --wallet has no short form. A wallet
-- is typed by its index or by its name, 2 or wallet2 ('typedIndex').
-- Words after -- are arguments.
-- bash breaks --wallet=N into words, which 'completionParts' joins again;
-- zsh and fish pass it whole. A value that does not read is as no option.
optionsTyped :: [String] -> Typed
optionsTyped typed =
  Typed
    { typedWallet = typedIndex "wallet" =<< valueOf "--wallet" Nothing,
      typedNetwork = netByName =<< valueOf "--network" (Just "-n"),
      typedKind = kindNamed . cs =<< valueOf "--type" (Just "-t")
    }
  where
    options = takeWhile (/= "--") typed
    valueOf :: String -> Maybe String -> Maybe String
    valueOf long shortM = go options
      where
        isOption word = word == long || Just word == shortM
        go (option : value : _) | isOption option = Just value
        go (word : rest)
          | Just value <- stripPrefix (long <> "=") word = Just value
          | Just short <- shortM, Just value@(_ : _) <- stripPrefix short word, take 2 word /= "--" = Just value
          | otherwise = go rest
        go [] = Nothing

-- | The index of a wallet or of a portfolio as typed: a number, or the
-- name the listings show, such as 2 or portfolio2, as the command line
-- reads it ('Haskoin.Wallet.Cli.Parser.indexOf'). The first argument is
-- the word the name starts with. An index of 2^31 or more names nothing,
-- and the command line refuses it.
typedIndex :: String -> String -> Maybe Natural
typedIndex what word = do
  index <- readNatural (cs (fromMaybe word (stripPrefix what word)))
  if indexFits index then Just index else Nothing

-- | The portfolios a value is offered from: every one, for a command that
-- finds its portfolio by itself, or the active one alone, for coin
-- control, which refuses the coins and addresses of another
data Looked = EveryPortfolio | ActivePortfolioOnly
  deriving (Eq, Show)

-- | The accounts of the portfolios looked in that the options name, on
-- the networks hw shows: those of the network and the type typed, each
-- narrowing the choice, or every such account of those portfolios without
-- any option
accountsTyped :: Config -> Looked -> Typed -> DB IO [(DBAccountId, DBAccount)]
accountsTyped cfg looked typed = do
  accounts <- getAccounts
  activeM <- activePortfolioId
  return
    [ (accId, acc)
      | (p, accId, acc) <- accounts,
        isShown cfg (accountNetwork acc),
        looked == EveryPortfolio || Just p.portfolioId == activeM,
        maybe True (== accountNetwork acc) typed.typedNetwork,
        maybe True (== accountKind acc) typed.typedKind
    ]

{- The networks -}

-- | The networks hw shows, for --network and the --hide-network of
-- setdefault: the commands refuse the others. Without a configuration
-- file they are those of a new one. A file that does not read offers
-- nothing.
shownNetworkNames :: IO [String]
shownNetworkNames = networkNamesWhere isShown

-- | The hidden networks, for the --show-network of setdefault
hiddenNetworkNames :: IO [String]
hiddenNetworkNames = networkNamesWhere (\cfg -> not . isShown cfg)

-- | The names of the networks of a portfolio that pass the test of the
-- configuration, in their order. The configuration is read as it is: a
-- completion writes no file.
networkNamesWhere :: (Config -> Network -> Bool) -> IO [String]
networkNamesWhere keep = handleAny (const (return [])) $ do
  dir <- D.getAppUserDataDirectory "hw"
  configM <- configAsItIs dir
  return [net.name | Just cfg <- [configM], net <- portfolioNetworks, keep cfg net]

-- | The pools of accounts, each once
poolsOf :: [(DBAccountId, DBAccount)] -> [Pool]
poolsOf accounts = nub (accountPool . snd <$> accounts)

{- The pending transactions -}

-- | The nosigHashes of the pending transactions to sign, for signtx and
-- signsweep: those neither signed nor sent, as the two refuse the others
hashesToSign :: String -> IO [String]
hashesToSign = shownHashesOf (\p -> not (isSigned (txOf p)) && not p.pendingTxOnline)

-- | The ids of the pending transactions to send, for broadcast: those signed
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
shownHashesOf keep typed = readForCompletion $ \cfg -> do
  held <- shownPending cfg
  return $ fewEnough [hash | p <- held, keep p, let hash = hexOf (shownHash (txOf p)), typed `isPrefixOf` hash]

-- | The pending transactions of every portfolio on the networks hw shows,
-- as pendingtxs lists them. A row that does not read is left out.
shownPending :: Config -> DB IO [PendingTx]
shownPending cfg = do
  held <- rights . map fromRow <$> pendingRows Nothing Nothing Nothing
  return [p | p <- held, isShown cfg p.pendingTxPool.poolNetwork]

-- | The transaction a pending transaction holds
txOf :: PendingTx -> Tx
txOf = txSignDataTx . pendingTxData

hexOf :: TxHash -> String
hexOf = cs . txHashToHex

{- The transactions to replace -}

-- | The ids of the transactions bumpfee replaces, for its TXID, as it
-- checks them: on a network that replaces transactions, those the
-- portfolio sent and that are still pending, and those of its history
-- that are in the mempool and its own payments. One replaced already is
-- left out, and one marked not replaceable when hw holds its bytes to
-- tell. They are few, since the network confirms or drops them: those of
-- every portfolio, as bumpfee finds the portfolio by the transaction, are
-- all read.
idsToBump :: String -> IO [String]
idsToBump typed = do
  options <- typedOptions
  readForCompletion $ \cfg -> do
    accounts <- accountsTyped cfg EveryPortfolio options
    found <- forM [pool | pool <- poolsOf accounts, pool.poolNetwork.replaceByFee] $ \pool ->
      fromRight [] <$> runExceptT (replaceableOf pool)
    return $ fewEnough [hash | tid <- concat found, let hash = hexOf tid, typed `isPrefixOf` hash]

-- | The transactions of the pool bumpfee replaces, by their ids
replaceableOf :: Pool -> ExceptT String (DB IO) [TxHash]
replaceableOf pool = do
  held <- pendingTxs pool
  let replaced = concatMap (txSignDataReplaces . pendingTxData) held
      sent = [tx | p <- held, p.pendingTxOnline, not p.pendingTxConflicted, let tx = txOf p, isSigned tx, marksReplaceable tx]
      sentIds = [txHash tx | tx <- sent, txHash tx `notElem` replaced]
  inMempool <- mempoolSummaries pool (Just askedFor)
  own <- forM [tid | (tid, summary) <- inMempool, ownTransaction summary, tid `notElem` replaced, tid `notElem` sentIds] $ \tid -> do
    rawM <- lookupRawTx tid
    return [tid | maybe True marksReplaceable rawM]
  return (sentIds <> concat own)

{- The coins -}

-- | The coins free to lock, for lockcoin: those nothing locks, neither a
-- pending transaction nor the user, as lockcoin refuses the others. A
-- coin tells its portfolio, so every portfolio counts.
coinsToLock :: String -> IO [String]
coinsToLock = coinsWhere EveryPortfolio (isNothing . jsonCoinLock)

-- | The coins the user locked, for unlockcoin, the only ones it frees
coinsToUnlock :: String -> IO [String]
coinsToUnlock = coinsWhere EveryPortfolio ((== Just LockedByUser) . jsonCoinLock)

-- | The coins of the active portfolio that can be spent now, for --coin.
-- Coin control refuses a coin of another portfolio, and a choice none of
-- whose coins can be spent.
coinsToSpend :: String -> IO [String]
coinsToSpend = coinsWhere ActivePortfolioOnly jsonCoinSpendable

-- | The coins of the portfolios looked in that start with the word typed
-- and pass the test, as the shell offers them ('offeredCoin'), each once.
-- Among more than 'offeredAtMost' that start with the word, locked or
-- not, none.
coinsWhere :: Looked -> (JsonCoin -> Bool) -> String -> IO [String]
coinsWhere looked keep typed = do
  options <- typedOptions
  readForCompletion $ \cfg -> do
    coins <- walletCoinsStarting cfg looked options typed
    return $ fewEnough (nub [offeredCoin typed (jsonCoinOutpoint coin) | coin <- coins, keep coin])

-- | The coins of the accounts the options name in the portfolios looked
-- in, that start with the word typed, as the shell offers them
-- ('offeredCoin'): the hexadecimal digits of the word are matched in the
-- database, which gives one more than 'offeredAtMost' of them at most
-- from each account, and a colon and an index after the digits are
-- matched here, among those. Whether a coin is locked, and whether it can
-- be spent now, are those of the listing.
walletCoinsStarting :: Config -> Looked -> Typed -> String -> DB IO [JsonCoin]
walletCoinsStarting cfg looked options typed = do
  accounts <- accountsTyped cfg looked options
  fmap concat . forM accounts $ \(accId, acc) -> do
    rows <- fewEnough <$> coinsStarting accId (takeWhile isHexDigit (take 64 typed)) askedFor
    listed <- fromRight [] <$> runExceptT (spendingOf (accountNetwork acc) >>= \rules -> mapM (toJsonCoin rules (accountPool acc)) rows)
    return [coin | coin <- listed, typed `isPrefixOf` offeredCoin typed (jsonCoinOutpoint coin)]

-- | A coin as the shell offers it for the word typed: as coins lists it,
-- TXID:INDEX, or by its short id when the word typed starts with a short
-- id and a colon, as in a1075db5:, so that the word goes on as it was
-- typed. The commands take a coin either way. Offering the coin in full
-- then would not do: bash keeps the a1075db5: typed and inserts only what
-- follows it ("Haskoin.Wallet.Cli.Parser.Completion").
offeredCoin :: String -> OutPoint -> String
offeredCoin typed outpoint = case break (== ':') typed of
  (short, ':' : _) | length short == shortIdLength -> short <> ":" <> show outpoint.index
  _ -> coinText outpoint
  where
    shortIdLength = 8

-- | Whether the word typed may start a coin: hexadecimal digits, then, if
-- anything, a colon and the digits of an index
startsLikeCoin :: String -> Bool
startsLikeCoin typed = case break (== ':') typed of
  (digits, ':' : index) -> all isHexDigit digits && all isDigit index
  (digits, _) -> all isHexDigit digits

{- What label labels -}

-- | What label labels, for its first argument, among what starts with the
-- word typed: the receiving addresses, since a change address takes no
-- label; the pending transactions, by the hash pendingtxs shows; and the
-- transactions of the history, by their ids. An address and a transaction
-- tell their portfolio, so every portfolio counts. Among more than
-- 'offeredAtMost' of them all, none. A path is not offered, as for show:
-- it is short to type. Nor is a description, the first word when it comes
-- alone, which is the user's own text.
labelTargets :: String -> IO [String]
labelTargets typed = readForCompletion $ \cfg -> do
  accounts <- accountsTyped cfg EveryPortfolio (optionsTyped [])
  addresses <- concat <$> sequence [addressesStarting accId AddrExternal (addressStart typed) askedFor | (accId, _) <- accounts]
  held <- shownPending cfg
  let pending = [hash | p <- held, let hash = hexOf (shownHash (txOf p)), typed `isPrefixOf` hash]
  -- A word that is no hexadecimal number starts no txid
  history <-
    if all isHexDigit typed
      then concat <$> sequence [txidsStarting pool typed askedFor | pool <- poolsOf accounts]
      else return []
  return $ fewEnough (nub (offeredAddresses typed addresses <> pending <> (hexOf <$> history)))

{- What show shows -}

-- | What show shows of the wallet, among what starts with the word typed:
-- the addresses of both chains, the pending transactions, by the hash
-- pendingtxs shows, the transactions of the history, by their ids, and the
-- coins, as coins lists them. Every portfolio counts, since an address, a
-- transaction and a coin tell where they are. Among more than
-- 'offeredAtMost' of them all, none. A path is not offered: it is short to
-- type, as receive shows it. The files show takes are offered beside these
-- by the parser of show, which knows what show reads as a file.
shownTargets :: String -> IO [String]
shownTargets typed = readForCompletion $ \cfg -> do
  accounts <- accountsTyped cfg EveryPortfolio (optionsTyped [])
  addresses <- concat <$> sequence [addressesStarting accId chain (addressStart typed) askedFor | (accId, _) <- accounts, chain <- [AddrExternal, AddrInternal]]
  held <- shownPending cfg
  let pending = [hash | p <- held, let hash = hexOf (shownHash (txOf p)), typed `isPrefixOf` hash]
  -- A word that is no hexadecimal number starts no txid, and one that is
  -- not one, or one with a colon and an index, starts no coin
  history <-
    if all isHexDigit typed
      then concat <$> sequence [txidsStarting pool typed askedFor | pool <- poolsOf accounts]
      else return []
  coins <-
    if startsLikeCoin typed
      then walletCoinsStarting cfg EveryPortfolio (optionsTyped []) typed
      else return []
  return $ fewEnough (nub (offeredAddresses typed addresses <> pending <> (hexOf <$> history) <> [offeredCoin typed (jsonCoinOutpoint coin) | coin <- coins]))

{- The addresses -}

-- | Every address of the wallet, for signmsg, which signs with the key of
-- any of them, and finds its portfolio by the address
walletAddresses :: String -> IO [String]
walletAddresses = addressesOf EveryPortfolio [AddrExternal, AddrInternal]

-- | Every address of the active portfolio, for --change-back, which
-- refuses an address of another portfolio
activeAddresses :: String -> IO [String]
activeAddresses = addressesOf ActivePortfolioOnly [AddrExternal, AddrInternal]

-- | The addresses of the chains given, of the accounts the options name
-- in the portfolios looked in, each once, that start with the word typed:
-- the database gives one more than 'offeredAtMost' of them at most from
-- each chain of each account
addressesOf :: Looked -> [AddrType] -> String -> IO [String]
addressesOf looked chains typed = do
  options <- typedOptions
  readForCompletion $ \cfg -> do
    accounts <- accountsTyped cfg looked options
    found <- concat <$> sequence [addressesStarting accId chain (addressStart typed) askedFor | (accId, _) <- accounts, chain <- chains]
    return $ fewEnough (nub (offeredAddresses typed found))

-- | The addresses of the active portfolio that hold coins that can be
-- spent now, for --spend-from, among the addresses that hold coins and
-- start with the word typed. Coin control refuses addresses of another
-- portfolio, and addresses that hold none.
addressesToSpendFrom :: String -> IO [String]
addressesToSpendFrom typed = do
  options <- typedOptions
  readForCompletion $ \cfg -> do
    accounts <- accountsTyped cfg ActivePortfolioOnly options
    found <- forM accounts $ \(accId, acc) -> do
      candidates <- fewEnough <$> coinAddressesStarting accId (addressStart typed) askedFor
      spendable <- fromRight Map.empty <$> runExceptT (spendableAtAddresses (accountNetwork acc) accId candidates)
      return (Map.keys spendable)
    return $ fewEnough (nub (offeredAddresses typed (concat found)))

-- | The start of an address the word typed gives, as the stored addresses
-- are matched on it ('addressesStarting'): after the prefix of a Bitcoin
-- Cash address, such as bitcoincash:, when the word starts with one,
-- since an address is matched after its prefix
addressStart :: String -> Text
addressStart typed = case cashAddrPrefixOf typed of
  Just prefix -> cs (drop (length prefix + 1) typed)
  Nothing -> cs typed

-- | The addresses found for the word typed, as the shell offers them, in
-- the form typed. A Bitcoin Cash address goes without its prefix, unless
-- the word starts with it: then it goes whole, and only an address with
-- that prefix is offered. hw finds the address written either way. bash
-- keeps the bitcoincash: of a word it broke at the colon, and inserts only
-- what follows ("Haskoin.Wallet.Cli.Parser.Completion").
offeredAddresses :: String -> [Text] -> [String]
offeredAddresses typed found = case cashAddrPrefixOf typed of
  Just _ -> [cs addr | addr <- found, cs typed `Text.isPrefixOf` addr]
  Nothing -> [cs (snd (Text.breakOnEnd ":" addr)) | addr <- found]

-- | The prefix of a Bitcoin Cash address the word typed starts with, before
-- its colon: bitcoincash, bchtest or bchreg
cashAddrPrefixOf :: String -> Maybe String
cashAddrPrefixOf typed = case break (== ':') typed of
  (prefix, ':' : _) | prefix `elem` prefixes -> Just prefix
  _ -> Nothing
  where
    prefixes = [cs prefix | net <- allNets, Just prefix <- [net.cashAddrPrefix]]

{- Reading the database -}

-- | Run a read of the wallet database for the shell, or give nothing. The
-- database is the one hw works on: ~/.hw/accounts.sqlite, or the file
-- the configuration names. The read is given the configuration, which
-- says which networks hw shows. It gives nothing when there is no
-- database, when the database is of another format version than this
-- hw's, and on any failure.
readForCompletion :: (Config -> DB IO [a]) -> IO [a]
readForCompletion action = handleAny (const (return [])) $ do
  -- The directory of hw, without creating it
  dir <- D.getAppUserDataDirectory "hw"
  configM <- configAsItIs dir
  case configM of
    Nothing -> return []
    Just cfg -> do
      let file = databaseFileIn dir cfg
      exists <- D.doesFileExist file
      if exists then runDBReadOnly file (readCurrent cfg) else return []
  where
    -- A database of another version is refused by every command. Its
    -- tables may differ.
    readCurrent cfg = do
      version <- runExceptT databaseVersion
      if version == Right (Just latestDatabaseVersion) then action cfg else return []
