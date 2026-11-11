{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The coins of the accounts, the unspent outputs the store knows at
-- their addresses: kept up to date by the sync, listed for the user and
-- offered to the transaction builder when they can be spent. Whether a
-- coin can be spent ('canSpend') is computed when it is read, from the
-- coins, the pending transactions and the best block, and stored nowhere:
-- so is the spendable balance of a pool and of its addresses.
--
-- A coin is at an address, so its row names the account of that address.
-- A payment spends from a pool, the accounts of a portfolio on one
-- network together: the coins of a pool are those of its accounts, BIP44
-- and BIP84 ones mixed ('spendableCoins'). A coin is keyed by its account
-- and its outpoint. The test networks share their keys, and a store that
-- serves two of them the same chain reports the same coin on both.
module Haskoin.Wallet.Database.Coin
  ( JsonCoin (..),
    CoinLock (..),
    coinOutpoint,
    coinText,
    readBlock,
    toJsonCoin,
    groupCoins,
    CoinFilter (..),
    coinsAfter,
    coinsStarting,
    coinsWith,
    coinAddressesStarting,
    coinBlock,
    Spending (..),
    spendingOf,
    spendableCoins,
    unspendableCoins,
    spendableAtAddresses,
    spendableBalance,
    coinBalancesAfter,
    coinBalancesAt,
    lockCoin,
    unlockCoin,
    walletCoin,
    shownCoin,
    coinRows,
    setLockedByUser,
    userLockedCoins,
    coinHolders,
    checkCoinOutput,
    checkListedCoins,
    checkCoinsPaid,
    coinParents,
    coinsSpentBy,
    Refreshed (..),
    refreshCoins,
    LeftReason (..),
    LeftCoin (..),
    leftReasons,
    leftReasonText,
    leftCoins,
    leftOfOtherType,
    labelsOfCoins,
    chosenCoins,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Aeson ((.:?), (.!=), FromJSON (..), ToJSON (..), Value (String), object, withObject, withText, (.:), (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Either (rights)
import Data.List (genericTake, intercalate, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (Single (..), rawSql)
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word32, Word64)
import Database.Esqueleto.Legacy (InnerJoin (..), Value (..), asc, desc, from, groupBy, having, in_, just, limit, on, orderBy, select, sum_, val, valList, where_, (&&.), (<.), (==.), (>.), (^.), (||.))
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.Config (Config, configDefaultNetwork)
import Haskoin.Wallet.Database.Account (accountNetwork, accountPool, getAccountById, getAccountsById, networksText, poolAccounts)
import Haskoin.Wallet.Database.Address (CoinBalance (..), addrPath)
import Haskoin.Wallet.Database.Portfolio (Place, poolPlace)
import Haskoin.Wallet.Keys (PoolPath (..), addressKind)
import Haskoin.Wallet.Database.PendingTx (lockedOutpoints, userLockedOutpoints)
import Haskoin.Wallet.Database.RawTx (getRawTx, lookupRawTx)
import Haskoin.Wallet.ShortId (ShortId, shortIdKey)
import Haskoin.Wallet.Database.TxInfo (coinLabel, coinLabels, liveSummaries, spentTxids, transactionTime, walletTransaction)
import Haskoin.Wallet.Util (rowLimit)
import Numeric.Natural (Natural)

{- Coins as the user sees them -}

data JsonCoin = JsonCoin
  { jsonCoinOutpoint :: !OutPoint,
    jsonCoinAddress :: !Address,
    jsonCoinValue :: !Word64,
    jsonCoinBlock :: !Store.BlockRef,
    jsonCoinConfirmations :: !Natural,
    -- | What holds it, if anything
    jsonCoinLock :: !(Maybe CoinLock),
    -- | The builder may spend it now ('canSpend')
    jsonCoinSpendable :: !Bool,
    -- | The label of the transaction that paid it, when the coin shows it
    -- ('coinLabel'), or none
    jsonCoinTxLabel :: !Text,
    -- | The label of the address that holds it, or none
    jsonCoinAddressLabel :: !Text,
    -- | Whether the address that holds it is a change address
    jsonCoinChange :: !Bool,
    -- | Where the address that holds it is in the portfolio: the kind of
    -- its account and its path below the key of that account, which the
    -- compact views show in place of the address
    jsonCoinPath :: !(Maybe PoolPath),
    -- | The time of the transaction that paid it ('transactionTime'),
    -- which its views start with, as the views of a transaction do
    jsonCoinTime :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

-- | What holds a coin back from spending: a pending transaction that spends
-- it, or the user, with lockcoin. A coin has one lock at most.
data CoinLock = LockedByTransaction | LockedByUser
  deriving (Eq, Show)

instance ToJSON CoinLock where
  toJSON LockedByTransaction = String "transaction"
  toJSON LockedByUser = String "user"

instance FromJSON CoinLock where
  parseJSON = withText "CoinLock" $ \case
    "transaction" -> return LockedByTransaction
    "user" -> return LockedByUser
    other -> fail $ "Unknown coin lock: " <> cs other

instance MarshalJSON Network JsonCoin where
  marshalValue net c =
    object
      [ "outpoint" .= jsonCoinOutpoint c,
        "address" .= marshalValue net (jsonCoinAddress c),
        "value" .= jsonCoinValue c,
        "block" .= jsonCoinBlock c,
        "confirmations" .= jsonCoinConfirmations c,
        "lock" .= jsonCoinLock c,
        "spendable" .= jsonCoinSpendable c,
        "txlabel" .= jsonCoinTxLabel c,
        "addresslabel" .= jsonCoinAddressLabel c,
        "change" .= jsonCoinChange c,
        "path" .= (pathInAccount <$> jsonCoinPath c),
        "type" .= (pathKind <$> jsonCoinPath c),
        "time" .= jsonCoinTime c
      ]
  unmarshalValue net =
    withObject "JsonCoin" $ \o ->
      JsonCoin
        <$> o .: "outpoint"
        <*> (unmarshalValue net =<< o .: "address")
        <*> o .: "value"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "lock"
        <*> o .: "spendable"
        <*> o .:? "txlabel" .!= ""
        <*> o .:? "addresslabel" .!= ""
        <*> o .:? "change" .!= False
        <*> (liftA2 PoolPath <$> o .:? "type" <*> o .:? "path")
        <*> o .:? "time"

-- | A coin row as the user sees it: its confirmations counted from the best
-- block, its lock, spendable when the builder may spend it now, the label
-- it shows, the time of its transaction, and the label, the kind and the
-- path of its address
toJsonCoin :: (MonadUnliftIO m) => Spending -> Pool -> DBCoin -> ExceptT String (DB m) JsonCoin
toJsonCoin rules pool row = do
  outpoint <- liftEither $ coinOutpoint row
  addr <- liftEither $ coinAddress rules.spendingNetwork outpoint row
  block <- liftEither $ readBlock outpoint (dBCoinBlockRef row)
  spendable <- canSpend rules 0 outpoint block
  label <- lift $ coinLabel pool outpoint.hash
  addrRowM <- lift $ fmap P.entityVal <$> P.getBy (UniqueAddress (dBCoinAccount row) (dBCoinAddress row))
  time <- lift $ transactionTime pool outpoint.hash
  return
    JsonCoin
      { jsonCoinOutpoint = outpoint,
        jsonCoinAddress = addr,
        jsonCoinValue = dBCoinValue row,
        jsonCoinBlock = block,
        jsonCoinConfirmations = getConfirmations rules.spendingBest block,
        jsonCoinLock = Map.lookup outpoint rules.spendingLocks,
        jsonCoinSpendable = spendable,
        jsonCoinTxLabel = label,
        jsonCoinAddressLabel = maybe "" dBAddressLabel addrRowM,
        jsonCoinChange = maybe False dBAddressInternal addrRowM,
        jsonCoinPath = PoolPath <$> addressKind addr <*> (addrPath <$> addrRowM),
        jsonCoinTime = time
      }

-- | The labels the coins show ('coinLabel'), by the transaction that paid
-- each, for what lists them
labelsOfCoins :: (MonadUnliftIO m) => Pool -> [Store.Unspent] -> DB m (Map TxHash Text)
labelsOfCoins pool coins = coinLabels pool ((.outpoint.hash) <$> coins)

{- Coin rows -}

-- | The outpoint of a coin row
coinOutpoint :: DBCoin -> Either String OutPoint
coinOutpoint = readOutpoint . dBCoinOutpoint

-- | An outpoint as the coin rows key it
readOutpoint :: KeyBytes -> Either String OutPoint
readOutpoint key = readStored "A stored coin has an outpoint that does not read" (bytesToOutpoint key)

-- | The address of a coin row, given its outpoint for the message
coinAddress :: Network -> OutPoint -> DBCoin -> Either String Address
coinAddress net outpoint row =
  maybeToEither (storedProblem outpoint $ "has an address that does not read: " <> cs (dBCoinAddress row)) $
    textToAddr net (dBCoinAddress row)

-- | The block of a coin as its row stores it, given its outpoint for the
-- message
readBlock :: OutPoint -> ByteString -> Either String Store.BlockRef
readBlock outpoint bytes =
  readStored (storedProblem outpoint "has a block that does not read") (S.decode bytes)

-- | The coin of a row as the store reported it, its script given by its
-- address: the refresh stores only coins whose script is the standard
-- script of their address ('groupCoins')
storedUnspent :: Network -> Ctx -> DBCoin -> Either String Store.Unspent
storedUnspent net ctx row = do
  outpoint <- coinOutpoint row
  addr <- coinAddress net outpoint row
  block <- readBlock outpoint (dBCoinBlockRef row)
  script <- maybeToEither (storedProblem outpoint "has an address without a standard script") $ standardScript ctx addr
  return
    Store.Unspent
      { Store.block = block,
        Store.outpoint = outpoint,
        Store.value = dBCoinValue row,
        Store.script = script,
        Store.address = Just addr
      }

-- | The standard output script of an address: the one script a coin the
-- wallet can spend has at it
standardScript :: Ctx -> Address -> Maybe ByteString
standardScript ctx addr = marshal ctx <$> addressToOutput addr

-- | What does not read in a stored coin, named by its outpoint
storedProblem :: OutPoint -> String -> String
storedProblem outpoint what = "The stored coin " <> coinText outpoint <> " " <> what

-- | A coin as hw writes it, and as --coin, lockcoin and unlockcoin take it:
-- the id of its transaction, a colon, and its index among the outputs
coinText :: OutPoint -> String
coinText outpoint = cs (txHashToHex outpoint.hash) <> ":" <> show outpoint.index

-- | A value read back from a row, or the given message when it does not read
readStored :: String -> Either String a -> Either String a
readStored message = either (const $ Left message) Right

-- | The coin rows of a pool: those of each of its accounts
poolCoins :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) [DBCoin]
poolCoins pool = lift $ do
  accounts <- map fst <$> poolAccounts pool
  map P.entityVal <$> P.selectList [DBCoinAccount P.<-. accounts] []

-- | The key of the row of a coin of an account
coinKey :: DBAccountId -> OutPoint -> P.Key DBCoin
coinKey accId outpoint = DBCoinKey accId (outpointBytes outpoint)

-- | Every row of a coin, by its outpoint: one for each account that holds
-- it. A coin is at one address, and an address in one account on a
-- network, so the rows are of accounts on different networks: the test
-- networks share their keys, and the same coin may be reported on two of
-- them.
coinRows :: (MonadUnliftIO m) => OutPoint -> DB m [DBCoin]
coinRows outpoint = map P.entityVal <$> P.selectList [DBCoinOutpoint P.==. outpointBytes outpoint] []

-- | The coins of the store by the text of their address, each coin once,
-- by its outpoint as the rows key it. A coin without an address, or at one
-- the network cannot write, is left out, and so is one whose script is not
-- the standard script of its address: the same address in other bytes,
-- which only a miner can put in a block, and which hw could not sign for.
-- The rows keep the address alone, which gives the script of every other.
groupCoins :: Network -> Ctx -> [Store.Unspent] -> Map Text (Map KeyBytes Store.Unspent)
groupCoins net ctx coins = Map.fromListWith Map.union (mapMaybe entry coins)
  where
    entry coin = do
      addr <- coin.address
      guard (standardScript ctx addr == Just coin.script)
      addrText <- addrToText net addr
      return (addrText, Map.singleton (outpointBytes coin.outpoint) coin)

{- Listing -}

-- | Which coins of a pool coins lists: all of them, or the locked ones
data CoinFilter = AllCoins | LockedCoins
  deriving (Eq, Show)

-- | The coins of the given accounts of a pool, spendable or not, in the
-- order of their block references as the store encodes them: the mempool
-- first, then the newest blocks first. Coins with the same block
-- reference, the outputs of one transaction for instance, follow the
-- order of their outpoints, so that the order is settled. Those after the
-- coin given, by its block and its outpoint, or the first ones, up to the
-- count, or all of them.
--
-- The accounts are those of the pool, or the one of a type alone. Each
-- account is read in that order from its own index, up to the count, and
-- the rows are merged: the first rows of the whole are among the first
-- rows of each. One query over two accounts would sort every coin of both
-- for each page.
coinsAfter :: (MonadUnliftIO m) => Pool -> [DBAccountId] -> CoinFilter -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> ExceptT String (DB m) [JsonCoin]
coinsAfter pool accIds shown afterM countM = do
  rules <- spendingOf pool.poolNetwork
  let lockedOnly = case shown of
        AllCoins -> []
        LockedCoins -> [DBCoinOutpoint P.<-. map outpointBytes (Map.keys rules.spendingLocks)]
      -- From the block of the coin given on, which the index on the block
      -- references finds, and past the coin itself within that block
      after = case afterM of
        Nothing -> []
        Just (block, outpoint) ->
          let blockBytes = S.encode block
           in (DBCoinBlockRef P.>=. blockBytes)
                : ([DBCoinBlockRef P.>. blockBytes] P.||. [DBCoinOutpoint P.>. outpointBytes outpoint])
      count = maybe [] (\n -> [P.LimitTo (rowLimit n)]) countM
  perAccount <- forM accIds $ \accId ->
    lift $
      P.selectList
        ((DBCoinAccount P.==. accId) : lockedOnly <> after)
        ([P.Asc DBCoinBlockRef, P.Asc DBCoinOutpoint] <> count)
  let merged = sortOn (\row -> (dBCoinBlockRef row, dBCoinOutpoint row)) (P.entityVal <$> concat perAccount)
  mapM (toJsonCoin rules pool) (maybe id genericTake countM merged)

-- | At most the given number of coin rows of the account whose transaction
-- id, as hw shows it, starts with the hexadecimal digits, by their
-- outpoints: for the shell. The key holds the id in the reverse order of
-- its digits, so the whole bytes typed are matched on the tail of the id
-- in the key, and a last digit alone on the byte before.
coinsStarting :: (MonadUnliftIO m) => DBAccountId -> String -> Natural -> DB m [DBCoin]
coinsStarting accId digits atMost = do
  rows <-
    rawSql
      (cs ("select ?? from d_b_coin where account = ?" <> conditions <> " order by outpoint limit ?"))
      (P.toPersistValue accId : parameters <> [P.toPersistValue (rowLimit atMost :: Int)])
  return (P.entityVal <$> rows)
  where
    typed = map toUpper digits
    bytes = length typed `div` 2
    -- The digits of the whole bytes, those of each byte together, in the
    -- order of the key, as hex() writes them
    wholeBytes = concat (reverse (pairs (take (2 * bytes) typed)))
    lastDigit = drop (2 * bytes) typed
    conditions =
      concat [" and hex(substr(outpoint, ?, ?)) = ?" | bytes > 0]
        <> concat [" and substr(hex(substr(outpoint, ?, 1)), 1, 1) = ?" | not (null lastDigit)]
    parameters =
      concat [[P.toPersistValue (33 - bytes), P.toPersistValue bytes, P.toPersistValue wholeBytes] | bytes > 0]
        <> concat [[P.toPersistValue (32 - bytes), P.toPersistValue lastDigit] | not (null lastDigit)]
    pairs (a : b : more) = [a, b] : pairs more
    pairs _ = []

-- | The coins of the wallet, of every account, each once, whose
-- transaction has the short id and that are at the index among its
-- outputs. The key holds the
-- txid in the reverse order of the digits hw shows ('outpointBytes'), then
-- the index in 4 bytes, the lowest first. So the key ends with the short
-- id reversed, then the index. No index of the keys reads the end of a
-- key: every coin is read, as 'coinsStarting' reads them for the shell.
coinsWith :: (MonadUnliftIO m) => ShortId -> Word32 -> DB m [OutPoint]
coinsWith short index = do
  rows <-
    rawSql
      "select outpoint from d_b_coin where substr(outpoint, 29, 8) = ?"
      [P.toPersistValue keyEnd]
  return $ nubOrd (rights (bytesToOutpoint . unSingle <$> rows))
  where
    keyEnd = BS.reverse (shortIdKey short) <> S.runPut (S.putWord32le index)

-- | The text of at most the given number of addresses of the account that
-- hold a coin and start with the text, as 'addressesStarting' matches it:
-- for the shell
coinAddressesStarting :: (MonadUnliftIO m) => DBAccountId -> Text -> Natural -> DB m [Text]
coinAddressesStarting accId start atMost = do
  rows <-
    rawSql
      "select distinct address from d_b_coin where account = ? and substr(address, instr(address, ':') + 1, ?) = ? order by address limit ?"
      [P.toPersistValue accId, P.toPersistValue (Text.length start), P.toPersistValue start, P.toPersistValue (rowLimit atMost :: Int)]
  return (unSingle <$> rows)

-- | The block of a coin of the accounts, the one --after names, or why
-- there is none
coinBlock :: (MonadUnliftIO m) => [DBAccountId] -> OutPoint -> ExceptT String (DB m) Store.BlockRef
coinBlock accIds outpoint = do
  rows <- lift $ catMaybes <$> mapM (\accId -> P.get (coinKey accId outpoint)) accIds
  case rows of
    row : _ -> liftEither $ readBlock outpoint (dBCoinBlockRef row)
    [] -> throwError $ "The coin " <> coinText outpoint <> " is not among the coins listed"

{- Spending -}

-- | What deciding whether the coins of a network can be spent reads once:
-- the network, the height of its best block, and the locks of its coins.
-- The rules are the same for every portfolio on the network.
data Spending = Spending
  { spendingNetwork :: !Network,
    spendingBest :: !(Maybe BlockHeight),
    spendingLocks :: !(Map OutPoint CoinLock)
  }

-- | The spending rules of a network as the database has them now
spendingOf :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) Spending
spendingOf net = do
  heightM <- bestHeight net
  locks <- coinLocks net
  return Spending {spendingNetwork = net, spendingBest = heightM, spendingLocks = locks}

-- | The locks of the coins of a network: those the pending transactions
-- spend, and those the user locked
coinLocks :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Map OutPoint CoinLock)
coinLocks net = do
  byTransactions <- lockedOutpoints net
  byUser <- userLockedOutpoints net
  return $ Map.fromSet (const LockedByTransaction) byTransactions <> Map.fromSet (const LockedByUser) byUser

-- | Whether the builder may spend a coin with at least the given
-- confirmations: nothing locks it, neither a pending transaction nor the
-- user, a coin from a coinbase has matured, and an unconfirmed coin comes
-- from a transaction the wallet trusts ('trustedTx'): one it made itself,
-- whether the coin is its change or what one of its portfolios paid another.
-- At zero confirmations, this says whether the coin can be spent now: the
-- spendable flag and balances.
canSpend :: (MonadUnliftIO m) => Spending -> Natural -> OutPoint -> Store.BlockRef -> ExceptT String (DB m) Bool
canSpend rules minConf outpoint block
  | confirmations < minConf || outpoint `Map.member` rules.spendingLocks = return False
  | confirmations == 0 = trustedTx rules.spendingNetwork outpoint.hash
  | confirmations < coinbaseMaturity = not <$> isCoinbaseTx outpoint.hash
  | otherwise = return True
  where
    confirmations = getConfirmations rules.spendingBest block

-- | The coins of a pool the builder may spend, with at least the
-- confirmations asked, as the store last reported them: those of every
-- account of the portfolio on the network, so that a payment can spend
-- BIP44 and BIP84 coins together
spendableCoins :: (MonadUnliftIO m) => Ctx -> Pool -> Natural -> ExceptT String (DB m) [Store.Unspent]
spendableCoins ctx pool minConf = do
  let net = pool.poolNetwork
  rules <- spendingOf net
  coins <- liftEither . mapM (storedUnspent net ctx) =<< poolCoins pool
  filterM (\coin -> canSpend rules minConf coin.outpoint coin.block) coins

-- | The coins the builder may not spend with at least the confirmations
-- asked, the others of 'spendableCoins', each with its lock. A coin without
-- a lock is not confirmed enough yet, or is a coinbase not yet mature.
unspendableCoins :: (MonadUnliftIO m) => Ctx -> Pool -> Natural -> ExceptT String (DB m) [(Store.Unspent, Maybe CoinLock)]
unspendableCoins ctx pool minConf = do
  let net = pool.poolNetwork
  rules <- spendingOf net
  coins <- liftEither . mapM (storedUnspent net ctx) =<< poolCoins pool
  held <- filterM (\coin -> not <$> canSpend rules minConf coin.outpoint coin.block) coins
  return [(coin, Map.lookup coin.outpoint rules.spendingLocks) | coin <- held]

-- | The value of the coins of the account that can be spent now, by the
-- text of their address, for the given addresses only: the index of the
-- coins by account and address makes a page of addresses as quick to show
-- in a large account as in a small one. An address without such a coin is
-- left out.
spendableAtAddresses :: (MonadUnliftIO m) => Network -> DBAccountId -> [Text] -> ExceptT String (DB m) (Map Text Word64)
spendableAtAddresses net accId addrs = do
  rules <- spendingOf net
  coins <- balanceColumns [accId] (Just addrs)
  Map.fromListWith (+) <$> spendableValues rules coins

-- | The value of all the coins of a pool that can be spent now: those of
-- every account of the portfolio on the network
spendableBalance :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) Word64
spendableBalance pool = do
  rules <- spendingOf pool.poolNetwork
  accounts <- lift $ map fst <$> poolAccounts pool
  coins <- balanceColumns accounts Nothing
  sum . map snd <$> spendableValues rules coins

-- | The addresses of the given accounts of a pool that hold coins, each
-- with the balance of its coins, the largest total first. Addresses with
-- the same total follow the order of their accounts, then of their
-- derivation paths, so that the order is settled. Those after the address
-- given, with its balance, or the first ones, up to the count, or all of
-- them. The order adds up every coin of the accounts, in one query; the
-- spendable values are read for the addresses listed only.
coinBalancesAfter :: (MonadUnliftIO m) => Network -> [DBAccountId] -> Maybe (DBAddress, CoinBalance) -> Maybe Natural -> ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalancesAfter net accIds = coinBalances net accIds Nothing

-- | Those of the given addresses of the accounts that hold coins, each
-- with the balance of its coins, in the order of 'coinBalancesAfter'
coinBalancesAt :: (MonadUnliftIO m) => Network -> [DBAccountId] -> [Text] -> ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalancesAt net accIds addrs = coinBalances net accIds (Just addrs) Nothing Nothing

-- | The addresses of the accounts that hold coins, each with the balance
-- of its coins, the largest total first: at the given addresses or at all
-- of them, after the given one or from the first, up to the count or all
-- of them
coinBalances ::
  (MonadUnliftIO m) =>
  Network ->
  [DBAccountId] ->
  Maybe [Text] ->
  Maybe (DBAddress, CoinBalance) ->
  Maybe Natural ->
  ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalances net accIds addrsM afterM countM = do
  rows <- lift . select . from $ \(c `InnerJoin` a) -> do
    on $ c ^. DBCoinAccount ==. a ^. DBAddressAccount &&. c ^. DBCoinAddress ==. a ^. DBAddressAddress
    where_ $ c ^. DBCoinAccount `in_` valList accIds
    forM_ addrsM $ \addrs -> where_ $ c ^. DBCoinAddress `in_` valList addrs
    groupBy (a ^. DBAddressAccount, a ^. DBAddressInternal, a ^. DBAddressIndex)
    let total = sum_ (c ^. DBCoinValue)
    -- After the address given: a smaller total, or the same total and an
    -- account, then a path, after its own
    forM_ afterM $ \(addr, balance) -> do
      let given = just (val (coinBalanceTotal balance))
          sameAccount = a ^. DBAddressAccount ==. val (dBAddressAccount addr)
          pathAfter =
            a ^. DBAddressInternal >. val (dBAddressInternal addr)
              ||. (a ^. DBAddressInternal ==. val (dBAddressInternal addr) &&. a ^. DBAddressIndex >. val (dBAddressIndex addr))
          placeAfter = a ^. DBAddressAccount >. val (dBAddressAccount addr) ||. (sameAccount &&. pathAfter)
      having $ total <. given ||. (total ==. given &&. placeAfter)
    orderBy [desc total, asc (a ^. DBAddressAccount), asc (a ^. DBAddressInternal), asc (a ^. DBAddressIndex)]
    forM_ countM $ limit . rowLimit
    return (a, total)
  -- An address of the page holds a coin at least, so its total is never null
  let held = [(P.entityVal a, fromMaybe 0 total) | (a, Value total) <- rows]
  rules <- spendingOf net
  coins <- balanceColumns accIds (Just (dBAddressAddress . fst <$> held))
  byAddress <- Map.fromListWith (<>) <$> mapM (coinShare rules) coins
  return [(addr, Map.findWithDefault (CoinBalance total 0 0 0) (dBAddressAddress addr) byAddress) | (addr, total) <- held]

-- | What a coin adds to the balance of its address: its value to the
-- total, and to the part its state puts it in. A locked coin cannot be
-- spent ('canSpend'), so no coin counts in two parts.
coinShare :: (MonadUnliftIO m) => Spending -> (OutPoint, Text, Word64, Store.BlockRef) -> ExceptT String (DB m) (Text, CoinBalance)
coinShare rules (outpoint, addr, value, block) = do
  spendable <- canSpend rules 0 outpoint block
  let lock = Map.lookup outpoint rules.spendingLocks
      partIf held = if held then value else 0
  return (addr, CoinBalance value (partIf spendable) (partIf (lock == Just LockedByTransaction)) (partIf (lock == Just LockedByUser)))

-- | What the spendable balances read of the coins of accounts of one
-- network, at the given addresses or at all of them: the outpoint,
-- address, value and block of each.
balanceColumns :: (MonadUnliftIO m) => [DBAccountId] -> Maybe [Text] -> ExceptT String (DB m) [(OutPoint, Text, Word64, Store.BlockRef)]
balanceColumns accIds addrsM = do
  rows <- lift . select . from $ \c -> do
    where_ $ c ^. DBCoinAccount `in_` valList accIds
    forM_ addrsM $ \addrs -> where_ $ c ^. DBCoinAddress `in_` valList addrs
    return (c ^. DBCoinOutpoint, c ^. DBCoinAddress, c ^. DBCoinValue, c ^. DBCoinBlockRef)
  liftEither $ forM rows $ \(Value key, Value addr, Value value, Value blockBytes) -> do
    outpoint <- readOutpoint key
    block <- readBlock outpoint blockBytes
    return (outpoint, addr, value, block)

-- | The address and value of those coins that can be spent now
spendableValues :: (MonadUnliftIO m) => Spending -> [(OutPoint, Text, Word64, Store.BlockRef)] -> ExceptT String (DB m) [(Text, Word64)]
spendableValues rules coins = do
  spendable <- filterM (\(outpoint, _, _, block) -> canSpend rules 0 outpoint block) coins
  return [(addr, value) | (_, addr, value, _) <- spendable]

-- | The confirmations a coinbase needs before it can be spent: consensus
-- lets a transaction spend it in the 100th block after its own, so from
-- the mempool once it has 100
coinbaseMaturity :: Natural
coinbaseMaturity = 100

-- | Whether the transaction is a coinbase, as the raw transaction stored for
-- its coins shows. One whose raw transaction is missing counts as an
-- ordinary transaction: the builder needs the raw transaction of every coin
-- it spends, and fails without it.
isCoinbaseTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) Bool
isCoinbaseTx tid = maybe False coinbase <$> lookupRawTx tid
  where
    -- A coinbase has a single input, which spends nothing
    coinbase tx = map (.outpoint) tx.inputs == [nullOutPoint]

-- | Whether the coins of a transaction can be spent before it confirms: it
-- is confirmed, or the wallet made it ('walletTransaction') from coins that
-- can be spent by this same rule. Whoever holds the keys of the inputs can
-- replace the transaction until it confirms, which also undoes every
-- transaction built on it. When the inputs are coins of the wallet's own
-- portfolios, that is the user: the change of a payment and what one
-- portfolio pays another are as safe as each other. A transaction the
-- sync has not stored for every portfolio that spends in it, or has found
-- dead, is not trusted.
--
-- The walk keeps what it found of each transaction, so that a parent two
-- transactions share is looked at once. A transaction met again while its
-- own parents are being looked at spends itself, through others or
-- directly: no chain holds such transactions, so a store that reports them
-- lies, and they are not trusted. The walk then ends, where it would
-- otherwise go round for ever.
trustedTx :: (MonadUnliftIO m) => Network -> TxHash -> ExceptT String (DB m) Bool
trustedTx net tid = evalStateT (trusted tid) Map.empty
  where
    -- What the walk found of each transaction: Nothing while its parents
    -- are being looked at, then whether it is trusted
    trusted :: (MonadUnliftIO m) => TxHash -> StateT (Map TxHash (Maybe Bool)) (ExceptT String (DB m)) Bool
    trusted t = do
      found <- gets (Map.lookup t)
      case found of
        Just (Just verdict) -> return verdict
        -- Met again on its own path: a cycle
        Just Nothing -> return False
        Nothing -> do
          modify (Map.insert t Nothing)
          verdict <- judge t
          modify (Map.insert t (Just verdict))
          return verdict
    judge t = do
      stored <- lift $ liveSummaries net t
      if any (Store.confirmed . fst) stored
        then return True
        else trustedUnconfirmed (snd <$> stored)
    -- The wallet made it, from transactions trusted by this same rule
    trustedUnconfirmed summaries
      | walletTransaction summaries = allM trusted (nubOrd (concatMap spentTxids summaries))
      | otherwise = return False
    -- Whether all are trusted, looking no further than the first that is not
    allM _ [] = return True
    allM f (x : xs) = do
      ok <- f x
      if ok then allM f xs else return False

{- Locks of the user -}

-- | Lock a coin of the wallet at the user's request: the builder no longer
-- offers it, and no pending transaction may spend it, until the user
-- unlocks it. Only a free coin can be locked: one a pending transaction
-- holds is refused, and so is one the user locked already. The lock stays
-- while the coin confirms, and goes with the coin once it is spent.
-- Returns where the coin is and the coin as coins shows it.
lockCoin :: (MonadUnliftIO m) => (Network, DBCoin) -> ExceptT String (DB m) (Place, JsonCoin)
lockCoin (net, row) = do
  outpoint <- liftEither $ coinOutpoint row
  locks <- coinLocks net
  case Map.lookup outpoint locks of
    Just LockedByTransaction -> throwError "The coin is already locked by a pending transaction"
    Just LockedByUser -> throwError "The coin is already locked"
    Nothing -> setLockedByUser (dBCoinAccount row) outpoint True
  shownCoin (net, row)

-- | Unlock a coin the user locked. A coin a pending transaction holds is
-- freed by deleting that transaction instead.
unlockCoin :: (MonadUnliftIO m) => (Network, DBCoin) -> ExceptT String (DB m) (Place, JsonCoin)
unlockCoin (net, row) = do
  outpoint <- liftEither $ coinOutpoint row
  locks <- coinLocks net
  case Map.lookup outpoint locks of
    Just LockedByUser -> setLockedByUser (dBCoinAccount row) outpoint False
    Just LockedByTransaction -> throwError "The coin is locked by a pending transaction, not by the user: deletetx frees it"
    Nothing -> throwError "The coin is not locked"
  shownCoin (net, row)

-- | The accounts that hold the coins, each once, on any network. A coin
-- the wallet does not hold has none.
coinHolders :: (MonadUnliftIO m) => [OutPoint] -> ExceptT String (DB m) [(DBAccountId, DBAccount)]
coinHolders outpoints = do
  rows <- lift $ concat <$> mapM coinRows outpoints
  getAccountsById $ nubOrd $ dBCoinAccount <$> rows

-- | The one row of a coin the user named, by its outpoint, with the
-- network of its account. A coin with rows on several networks names the
-- one on the network given, the --network of the command, or else the one
-- on the default network of the configuration. When neither settles it,
-- the user is asked for --network, with the networks named.
walletCoin :: (MonadUnliftIO m) => Config -> Maybe Network -> OutPoint -> ExceptT String (DB m) (Network, DBCoin)
walletCoin cfg netM outpoint = do
  rows <- lift $ coinRows outpoint
  found <- forM rows $ \row -> (,row) . accountNetwork <$> getAccountById (dBCoinAccount row)
  let onNetwork net = [held | held@(rowNet, _) <- found, rowNet == net]
      networks = nubOrdOn (.name) (fst <$> found)
  case (netM, found) of
    (_, []) -> throwError ("The wallet has no coin " <> coinText outpoint)
    (Just net, _) -> case onNetwork net of
      held : _ -> return held
      [] -> throwError $ "The coin " <> coinText outpoint <> " is on " <> networksText networks <> ", not on the --network " <> net.name
    (Nothing, [held]) -> return held
    (Nothing, _) -> case onNetwork (configDefaultNetwork cfg) of
      held : _ -> return held
      [] -> throwError $ "The coin " <> coinText outpoint <> " is on " <> networksText networks <> ": give the --network"

setLockedByUser :: (MonadUnliftIO m) => DBAccountId -> OutPoint -> Bool -> ExceptT String (DB m) ()
setLockedByUser accId outpoint locked =
  lift $ P.update (coinKey accId outpoint) [DBCoinLockedByUser P.=. locked]

-- | The coins of the account the user locked, in the order of their
-- outpoints: what exportwallet writes
userLockedCoins :: (MonadUnliftIO m) => DBAccountId -> DB m [DBCoin]
userLockedCoins accId =
  map P.entityVal <$> P.selectList [DBCoinAccount P.==. accId, DBCoinLockedByUser P.==. True] [P.Asc DBCoinOutpoint]

-- | A coin as coins shows it now, with where it is: what show answers. It
-- changes nothing.
shownCoin :: (MonadUnliftIO m) => (Network, DBCoin) -> ExceptT String (DB m) (Place, JsonCoin)
shownCoin (net, row) = do
  acc <- getAccountById (dBCoinAccount row)
  let pool = accountPool acc
  place <- poolPlace pool
  rules <- spendingOf net
  coin <- toJsonCoin rules pool row
  return (place, coin)

{- The coins against their transactions -}

-- | Refuse a coin the transaction it comes from does not pay as the store
-- reports it: the transaction is named by its hash, so its output is what
-- the coin holds, whatever the store says. A store that reported a coin
-- below its value would have hw build a payment whose fee takes the
-- difference: 1 btc reported as 0.0011 btc gave a fee of 0.9989 btc. The
-- transaction must be the coin's own, of the hash of its outpoint.
checkCoinOutput :: Tx -> Store.Unspent -> Either String ()
checkCoinOutput parent coin = do
  output <-
    maybe (Left (named <> ", but its transaction has " <> show (length parent.outputs) <> " outputs")) Right $
      listToMaybe (drop (fromIntegral coin.outpoint.index) parent.outputs)
  when (output.value /= coin.value) $
    Left (named <> " as worth " <> show coin.value <> " satoshis, but its transaction pays " <> show output.value)
  when (output.script /= coin.script) $
    Left (named <> " at another script than its transaction pays")
  where
    named = "The store reports the coin " <> coinText coin.outpoint

-- | Whether the raw transaction stored under the hash of its outpoint
-- pays each coin the store lists ('checkCoinOutput'): Left the reason to
-- refuse the answer of the store, for the first coin it does not pay. The
-- sync asks this once it has stored the raw transactions, before it
-- stores a coin. A coin whose raw transaction is not stored is not
-- checked: no builder spends it, since signing needs the raw transaction.
--
-- The refusal is given, not thrown: it is a failure of the store, which
-- the sync tells the store of ("Haskoin.Wallet.Store".'refusedAnswer').
-- A failure of the action itself is one of the database: a stored raw
-- transaction that does not read.
checkListedCoins :: (MonadUnliftIO m) => [Store.Unspent] -> ExceptT String (DB m) (Either String ())
checkListedCoins coins = do
  checks <- forM coins $ \coin -> do
    parentM <- lookupRawTx coin.outpoint.hash
    return $ maybe (Right ()) (`checkCoinOutput` coin) parentM
  return (sequence_ checks)

-- | The transactions the coins come from, each once, in the order of the
-- coins, as a builder puts them in the signing data: read from the
-- database, and each coin checked against its output ('checkCoinOutput'),
-- so that no transaction is built on a value that a store got wrong.
coinParents :: (MonadUnliftIO m) => [Store.Unspent] -> ExceptT String (DB m) [Tx]
coinParents coins = do
  parents <- mapM getRawTx $ nubOrd $ (.outpoint.hash) <$> coins
  liftEither $ checkCoinsPaid parents coins
  return parents

-- | Refuse a coin whose transaction is not among the transactions given,
-- by its hash, or does not pay it ('checkCoinOutput'): for coins and
-- transactions that come from the store together, as those of a sweep
checkCoinsPaid :: [Tx] -> [Store.Unspent] -> Either String ()
checkCoinsPaid parents coins =
  forM_ coins $ \coin -> do
    parent <-
      maybe (Left ("The store gave no transaction of the coin " <> coinText coin.outpoint)) Right $
        Map.lookup coin.outpoint.hash byHash
    checkCoinOutput parent coin
  where
    byHash = Map.fromList [(txHash tx, tx) | tx <- parents]

-- | The coins among the given ones that the inputs of the transaction
-- spend, in the order of the inputs
coinsSpentBy :: Tx -> [Store.Unspent] -> [Store.Unspent]
coinsSpentBy tx coins = mapMaybe (\input -> Map.lookup input.outpoint byOutpoint) tx.inputs
  where
    byOutpoint = Map.fromList [(c.outpoint, c) | c <- coins]

{- Refreshing -}

-- | What a refresh changed: how many coins were inserted, rewritten or
-- deleted, and the coins inserted and deleted
data Refreshed = Refreshed
  { refreshedCount :: !Int,
    insertedCoins :: ![DBCoin],
    deletedCoins :: ![DBCoin]
  }
  deriving (Eq, Show)

instance Semigroup Refreshed where
  a <> b =
    Refreshed
      { refreshedCount = a.refreshedCount + b.refreshedCount,
        insertedCoins = a.insertedCoins <> b.insertedCoins,
        deletedCoins = a.deletedCoins <> b.deletedCoins
      }

instance Monoid Refreshed where
  mempty = Refreshed 0 [] []

-- | Bring the coins of the account at the addresses in line with what the
-- store lists: insert the coins that are new, rewrite those whose block
-- changed, delete those the store no longer lists. A coin the store lists
-- twice is stored once. The value of a coin never changes, so a value that
-- differs is a stored one gone wrong, which a full sync repairs here.
refreshCoins ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  [Address] ->
  [Store.Unspent] ->
  ExceptT String (DB m) Refreshed
refreshCoins net ctx accId addrs unspents = mconcat <$> mapM refreshAddress addrs
  where
    listed = groupCoins net ctx unspents
    refreshAddress addr = do
      addrText <-
        liftMaybe "The coins of an address of another network cannot be refreshed" $
          addrToText net addr
      rows <- lift $ P.selectList [DBCoinAccount P.==. accId, DBCoinAddress P.==. addrText] []
      let stored = Map.fromList [(dBCoinOutpoint row, row) | P.Entity _ row <- rows]
          current = Map.findWithDefault Map.empty addrText listed
          gone = Map.elems $ stored `Map.difference` current
          new = Map.elems $ current `Map.difference` stored
          moved =
            [ coin
              | (coin, row) <- Map.elems (Map.intersectionWith (,) current stored),
                S.encode coin.block /= dBCoinBlockRef row || coin.value /= dBCoinValue row
            ]
      lift $ forM_ gone deleteCoin
      lift $ forM_ moved (rewriteCoin accId)
      inserted <- mapM (insertCoin net accId) new
      return
        Refreshed
          { refreshedCount = length gone + length moved + length new,
            insertedCoins = inserted,
            deletedCoins = gone
          }

-- | Store a coin of the account, at the address it pays
insertCoin :: (MonadUnliftIO m) => Network -> DBAccountId -> Store.Unspent -> ExceptT String (DB m) DBCoin
insertCoin net accId coin = do
  addr <-
    liftMaybe ("The coin " <> coinText coin.outpoint <> " pays no address of the network") $
      addrToText net =<< coin.address
  time <- liftIO getCurrentTime
  -- A row with this key holds the coin under another address, which only
  -- a database gone wrong does. The coin takes the row over, so that a
  -- full sync gets past it, and what the user locked stays locked.
  let key = coinKey accId coin.outpoint
  previousM <- lift $ P.get key
  let row =
        DBCoin
          { dBCoinAccount = accId,
            dBCoinOutpoint = outpointBytes coin.outpoint,
            dBCoinAddress = addr,
            dBCoinValue = coin.value,
            dBCoinBlockRef = S.encode coin.block,
            dBCoinLockedByUser = maybe False dBCoinLockedByUser previousM,
            dBCoinCreated = time
          }
  lift $ P.repsert key row
  return row

-- | Give a stored coin the block and the value the store now reports for
-- it. Whether the user locked it stays.
rewriteCoin :: (MonadUnliftIO m) => DBAccountId -> Store.Unspent -> DB m ()
rewriteCoin accId coin =
  P.update
    (coinKey accId coin.outpoint)
    [DBCoinBlockRef P.=. S.encode coin.block, DBCoinValue P.=. coin.value]

-- | Delete a stored coin
deleteCoin :: (MonadUnliftIO m) => DBCoin -> DB m ()
deleteCoin row = P.delete (DBCoinKey (dBCoinAccount row) (dBCoinOutpoint row))

{- The coins a transaction leaves -}

-- | Why a transaction just prepared leaves a coin of the portfolio: a coin
-- that coin control chose, or, for a send-all, any coin of the portfolio
-- on the network
data LeftReason
  = -- | Worth less than the fee to spend it, and left as the user chose
    WorthLessThanFee
  | -- | Locked, by the user or by a pending transaction
    Locked !CoinLock
  | -- | Not confirmed enough yet, or a coinbase not yet mature
    NotSpendableYet
  | -- | Spendable, but not among the coins chosen with --coin or --spend-from
    NotGiven
  | -- | Spendable, but of the type of address --coin-type leaves out
    OtherType
  deriving (Eq, Show)

instance ToJSON LeftReason where
  toJSON = String . cs . reasonName

instance FromJSON LeftReason where
  parseJSON = withText "LeftReason" $ \name ->
    case filter ((== cs name) . reasonName) leftReasons of
      [reason] -> return reason
      _ -> fail $ "Unknown reason a coin stays in the portfolio: " <> cs name

-- | Every reason, in the order the answer lists the coins in
leftReasons :: [LeftReason]
leftReasons = [WorthLessThanFee, Locked LockedByUser, Locked LockedByTransaction, NotSpendableYet, NotGiven, OtherType]

-- | The name of a reason in JSON
reasonName :: LeftReason -> String
reasonName = \case
  WorthLessThanFee -> "dust"
  Locked LockedByUser -> "lockedbyuser"
  Locked LockedByTransaction -> "lockedbytransaction"
  NotSpendableYet -> "notspendableyet"
  NotGiven -> "notgiven"
  OtherType -> "othertype"

-- | A reason as the user reads it
leftReasonText :: LeftReason -> String
leftReasonText = \case
  WorthLessThanFee -> "worth less than its fee"
  Locked LockedByUser -> "you locked it"
  Locked LockedByTransaction -> "a pending transaction spends it"
  NotSpendableYet -> "not spendable yet"
  NotGiven -> "not chosen with --coin or --spend-from"
  OtherType -> "not of the type --coin-type sends"

-- | Why a coin the builder may not spend stays, from its lock: without one,
-- it is not spendable yet
lockReason :: Maybe CoinLock -> LeftReason
lockReason = maybe NotSpendableYet Locked

-- | A coin of the portfolio a transaction leaves in it, and why
data LeftCoin = LeftCoin
  { leftCoinOutpoint :: !OutPoint,
    leftCoinValue :: !Word64,
    leftCoinReason :: !LeftReason,
    -- | The label the coin shows ('coinLabel'), or none
    leftCoinTxLabel :: !Text
  }
  deriving (Eq, Show)

instance ToJSON LeftCoin where
  toJSON coin =
    object
      [ "outpoint" .= leftCoinOutpoint coin,
        "value" .= leftCoinValue coin,
        "reason" .= leftCoinReason coin,
        "txlabel" .= leftCoinTxLabel coin
      ]

instance FromJSON LeftCoin where
  parseJSON = withObject "LeftCoin" $ \o ->
    LeftCoin <$> o .: "outpoint" <*> o .: "value" <*> o .: "reason" <*> o .:? "txlabel" .!= ""

-- | The coins of the portfolio a transaction leaves, by reason in the order
-- of 'leftReasons', the largest first: the dust coins left, the coins the
-- builder may not spend with their locks, and the spendable coins not
-- chosen with coin control. Each shows its label, among those given
-- ('labelsOfCoins').
leftCoins :: Map TxHash Text -> [Store.Unspent] -> [(Store.Unspent, Maybe CoinLock)] -> [Store.Unspent] -> [LeftCoin]
leftCoins labels dustLeft unspendable notGiven =
  concat [sortOn (Down . leftCoinValue) [left | left <- everyLeft, leftCoinReason left == reason] | reason <- leftReasons]
  where
    everyLeft =
      [leftOf WorthLessThanFee coin | coin <- dustLeft]
        <> [leftOf (lockReason lock) coin | (coin, lock) <- unspendable]
        <> [leftOf NotGiven coin | coin <- notGiven]
    leftOf :: LeftReason -> Store.Unspent -> LeftCoin
    leftOf reason coin = LeftCoin coin.outpoint coin.value reason (Map.findWithDefault "" coin.outpoint.hash labels)

-- | The spendable coins a send-all leaves because --coin-type sends the
-- other type, the largest first, each with its label among those given.
-- They come after the coins 'leftCoins' lists.
leftOfOtherType :: Map TxHash Text -> [Store.Unspent] -> [LeftCoin]
leftOfOtherType labels others =
  sortOn
    (Down . leftCoinValue)
    [LeftCoin coin.outpoint coin.value OtherType (Map.findWithDefault "" coin.outpoint.hash labels) | coin <- others]

{- Coin control -}

-- | The coins that coin control chose among the coins of the pool, by
-- their outpoints and by the addresses that hold them: those the builder
-- may spend now, and the others, each with its lock. The addresses count,
-- not their text. An outpoint the pool does not hold is refused, and so
-- is a choice none of whose coins can be spent, with why.
chosenCoins ::
  -- | The outpoints chosen
  [OutPoint] ->
  -- | The addresses chosen
  [Address] ->
  -- | The coins the builder may spend, from 'spendableCoins'
  [Store.Unspent] ->
  -- | The others, from 'unspendableCoins'
  [(Store.Unspent, Maybe CoinLock)] ->
  Either String ([Store.Unspent], [(Store.Unspent, Maybe CoinLock)])
chosenCoins outpoints addrs spendable unspendable = do
  forM_ outpoints $ \outpoint ->
    unless (outpoint `elem` held) $ Left $ "The portfolio has no coin " <> coinText outpoint <> " on the network of the payment"
  when (null chosenSpendable) $
    Left $ case chosenHeld of
      [] -> "The addresses chosen with --spend-from hold no coins"
      _ -> "None of the coins chosen can be spent now: " <> intercalate "; " (heldText <$> chosenHeld)
  return (chosenSpendable, chosenHeld)
  where
    held = (.outpoint) <$> (spendable <> (fst <$> unspendable))
    chosen coin = coin.outpoint `elem` outpoints || maybe False (`elem` addrs) coin.address
    chosenSpendable = filter chosen spendable
    chosenHeld = filter (chosen . fst) unspendable
    heldText (coin, lock) = coinText coin.outpoint <> ", " <> leftReasonText (lockReason lock)
