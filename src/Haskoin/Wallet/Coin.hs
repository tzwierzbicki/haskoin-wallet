{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The coins of an account, the unspent outputs the store knows at its
-- addresses: kept up to date by the sync, listed for the user and offered
-- to the transaction builder when they can be spent. Whether a coin can be
-- spent ('canSpend') is computed when it is read, from the coins, the
-- pending transactions and the best block, and stored nowhere: so is the
-- spendable balance of an account and of its addresses.
module Haskoin.Wallet.Coin
  ( JsonCoin (..),
    CoinLock (..),
    coinOutpoint,
    coinText,
    toJsonCoin,
    groupCoins,
    CoinFilter (..),
    coinsAfter,
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
    coinAccounts,
    Refreshed (..),
    refreshCoins,
    LeftReason (..),
    LeftCoin (..),
    leftReasonText,
    leftCoins,
    chosenCoins,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), object, withObject, withText, (.:), (.=))
import Data.ByteString (ByteString)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy (InnerJoin (..), Value (..), asc, desc, from, groupBy, having, in_, just, limit, on, orderBy, select, sum_, val, valList, where_, (&&.), (<.), (==.), (>.), (^.), (||.))
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Database
import Haskoin.Wallet.Account (accountNetwork, getAccountById, getAccountsById)
import Haskoin.Wallet.Address (CoinBalance (..))
import Haskoin.Wallet.PendingTx (lockedOutpoints, userLockedOutpoints)
import Haskoin.Wallet.RawTx (lookupRawTx)
import Haskoin.Wallet.TxInfo (MyInputs (..), TxSummary (..), liveSummary, ownTransaction)
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
    jsonCoinSpendable :: !Bool
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
        "spendable" .= jsonCoinSpendable c
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

-- | A coin row as the user sees it: its confirmations counted from the best
-- block, its lock, and spendable when the builder may spend it now
toJsonCoin :: (MonadUnliftIO m) => Spending -> DBCoin -> ExceptT String (DB m) JsonCoin
toJsonCoin rules row = do
  outpoint <- liftEither $ coinOutpoint row
  addr <- liftEither $ coinAddress rules.spendingNetwork outpoint row
  block <- liftEither $ readBlock outpoint (dBCoinBlockRef row)
  spendable <- canSpend rules 0 outpoint block
  return
    JsonCoin
      { jsonCoinOutpoint = outpoint,
        jsonCoinAddress = addr,
        jsonCoinValue = dBCoinValue row,
        jsonCoinBlock = block,
        jsonCoinConfirmations = getConfirmations rules.spendingBest block,
        jsonCoinLock = Map.lookup outpoint rules.spendingLocks,
        jsonCoinSpendable = spendable
      }

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

-- | The coin rows of an account
accountCoins :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) [DBCoin]
accountCoins accId = lift $ map P.entityVal <$> P.selectList [DBCoinAccount P.==. accId] []

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

-- | Which coins of an account coins lists: all of them, or the locked ones
data CoinFilter = AllCoins | LockedCoins
  deriving (Eq, Show)

-- | The coins of the account, spendable or not, in the order of their
-- block references as the store encodes them: the mempool first, then the
-- newest blocks first. Coins with the same block reference, the outputs of
-- one transaction for instance, follow the order of their outpoints, so
-- that the order is settled. Those after the coin given, by its block and
-- its outpoint, or the first ones, up to the count, or all of them.
coinsAfter :: (MonadUnliftIO m) => Network -> DBAccountId -> CoinFilter -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> ExceptT String (DB m) [JsonCoin]
coinsAfter net accId shown afterM countM = do
  rules <- spendingOf net accId
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
  rows <-
    lift $
      P.selectList
        ((DBCoinAccount P.==. accId) : lockedOnly <> after)
        ([P.Asc DBCoinBlockRef, P.Asc DBCoinOutpoint] <> count)
  mapM (toJsonCoin rules . P.entityVal) rows

-- | The block of a coin of the account, the one --after names, or why
-- there is none
coinBlock :: (MonadUnliftIO m) => DBAccountId -> OutPoint -> ExceptT String (DB m) Store.BlockRef
coinBlock accId outpoint = do
  rowM <- lift $ P.get (DBCoinKey (outpointBytes outpoint))
  case rowM of
    Just row | dBCoinAccount row == accId -> liftEither $ readBlock outpoint (dBCoinBlockRef row)
    _ -> throwError $ "The coin " <> coinText outpoint <> " is not in the account"

{- Spending -}

-- | What deciding whether the coins of an account can be spent reads once:
-- the account and its network, the height of the best block, and the
-- locks of the coins of the network
data Spending = Spending
  { spendingNetwork :: !Network,
    spendingAccount :: !DBAccountId,
    spendingBest :: !(Maybe BlockHeight),
    spendingLocks :: !(Map OutPoint CoinLock)
  }

-- | The spending rules of an account as the database has them now
spendingOf :: (MonadUnliftIO m) => Network -> DBAccountId -> ExceptT String (DB m) Spending
spendingOf net accId = do
  heightM <- bestHeight net
  locks <- coinLocks net
  return Spending {spendingNetwork = net, spendingAccount = accId, spendingBest = heightM, spendingLocks = locks}

-- | The locks of the coins of a network: those the pending transactions
-- spend, and those the user locked
coinLocks :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Map OutPoint CoinLock)
coinLocks net = do
  byTransactions <- lockedOutpoints net
  byUser <- userLockedOutpoints net
  return $ Map.fromSet (const LockedByTransaction) byTransactions <> Map.fromSet (const LockedByUser) byUser

-- | Whether the builder may spend a coin of the account with at least the
-- given confirmations: nothing locks it, neither a pending transaction nor
-- the user, a coin from a coinbase has matured, and an unconfirmed coin
-- comes from a transaction the account trusts ('trustedTx'), its own
-- change. At zero confirmations, this says whether the coin can be spent
-- now: the spendable flag and balances.
canSpend :: (MonadUnliftIO m) => Spending -> Natural -> OutPoint -> Store.BlockRef -> ExceptT String (DB m) Bool
canSpend rules minConf outpoint block
  | confirmations < minConf || outpoint `Map.member` rules.spendingLocks = return False
  | confirmations == 0 = trustedTx rules.spendingNetwork rules.spendingAccount outpoint.hash
  | confirmations < coinbaseMaturity = not <$> isCoinbaseTx outpoint.hash
  | otherwise = return True
  where
    confirmations = getConfirmations rules.spendingBest block

-- | The coins the builder may spend, with at least the confirmations asked,
-- as the store last reported them
spendableCoins :: (MonadUnliftIO m) => Network -> Ctx -> DBAccountId -> Natural -> ExceptT String (DB m) [Store.Unspent]
spendableCoins net ctx accId minConf = do
  rules <- spendingOf net accId
  coins <- liftEither . mapM (storedUnspent net ctx) =<< accountCoins accId
  filterM (\coin -> canSpend rules minConf coin.outpoint coin.block) coins

-- | The coins the builder may not spend with at least the confirmations
-- asked, the others of 'spendableCoins', each with its lock. A coin without
-- a lock is not confirmed enough yet, or is a coinbase not yet mature.
unspendableCoins :: (MonadUnliftIO m) => Network -> Ctx -> DBAccountId -> Natural -> ExceptT String (DB m) [(Store.Unspent, Maybe CoinLock)]
unspendableCoins net ctx accId minConf = do
  rules <- spendingOf net accId
  coins <- liftEither . mapM (storedUnspent net ctx) =<< accountCoins accId
  held <- filterM (\coin -> not <$> canSpend rules minConf coin.outpoint coin.block) coins
  return [(coin, Map.lookup coin.outpoint rules.spendingLocks) | coin <- held]

-- | The value of the coins of the account that can be spent now, by the
-- text of their address, for the given addresses only: the index of the
-- coins by account and address makes a page of addresses as quick to show
-- in a large account as in a small one. An address without such a coin is
-- left out.
spendableAtAddresses :: (MonadUnliftIO m) => Network -> DBAccountId -> [Text] -> ExceptT String (DB m) (Map Text Word64)
spendableAtAddresses net accId addrs = do
  rules <- spendingOf net accId
  coins <- balanceColumns accId (Just addrs)
  Map.fromListWith (+) <$> spendableValues rules coins

-- | The value of all the coins of the account that can be spent now
spendableBalance :: (MonadUnliftIO m) => Network -> DBAccountId -> ExceptT String (DB m) Word64
spendableBalance net accId = do
  rules <- spendingOf net accId
  coins <- balanceColumns accId Nothing
  sum . map snd <$> spendableValues rules coins

-- | The addresses of the account that hold coins, each with the balance
-- of its coins, the largest total first. Addresses with the same total
-- follow the order of their derivation paths, so that the order is
-- settled. Those after the address given, with its balance, or the first
-- ones, up to the count, or all of them. The order adds up every coin of
-- the account, in one query; the spendable values are read for the
-- addresses listed only.
coinBalancesAfter :: (MonadUnliftIO m) => Network -> DBAccountId -> Maybe (DBAddress, CoinBalance) -> Maybe Natural -> ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalancesAfter net accId = coinBalances net accId Nothing

-- | Those of the given addresses of the account that hold coins, each with
-- the balance of its coins, in the order of 'coinBalancesAfter'
coinBalancesAt :: (MonadUnliftIO m) => Network -> DBAccountId -> [Text] -> ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalancesAt net accId addrs = coinBalances net accId (Just addrs) Nothing Nothing

-- | The addresses of the account that hold coins, each with the balance of
-- its coins, the largest total first: at the given addresses or at all of
-- them, after the given one or from the first, up to the count or all of
-- them
coinBalances ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Maybe [Text] ->
  Maybe (DBAddress, CoinBalance) ->
  Maybe Natural ->
  ExceptT String (DB m) [(DBAddress, CoinBalance)]
coinBalances net accId addrsM afterM countM = do
  rows <- lift . select . from $ \(c `InnerJoin` a) -> do
    on $ c ^. DBCoinAccount ==. a ^. DBAddressAccount &&. c ^. DBCoinAddress ==. a ^. DBAddressAddress
    where_ $ c ^. DBCoinAccount ==. val accId
    forM_ addrsM $ \addrs -> where_ $ c ^. DBCoinAddress `in_` valList addrs
    groupBy (a ^. DBAddressAccount, a ^. DBAddressInternal, a ^. DBAddressIndex)
    let total = sum_ (c ^. DBCoinValue)
    -- After the address given: a smaller total, or the same total and a
    -- path after its own
    forM_ afterM $ \(addr, balance) -> do
      let given = just (val (coinBalanceTotal balance))
          pathAfter =
            a ^. DBAddressInternal >. val (dBAddressInternal addr)
              ||. (a ^. DBAddressInternal ==. val (dBAddressInternal addr) &&. a ^. DBAddressIndex >. val (dBAddressIndex addr))
      having $ total <. given ||. (total ==. given &&. pathAfter)
    orderBy [desc total, asc (a ^. DBAddressInternal), asc (a ^. DBAddressIndex)]
    forM_ countM $ limit . rowLimit
    return (a, total)
  -- An address of the page holds a coin at least, so its total is never null
  let held = [(P.entityVal a, fromMaybe 0 total) | (a, Value total) <- rows]
  spendable <- spendableAtAddresses net accId (dBAddressAddress . fst <$> held)
  return [(addr, CoinBalance total (Map.findWithDefault 0 (dBAddressAddress addr) spendable)) | (addr, total) <- held]

-- | What the spendable balances read of the coins of an account, at the
-- given addresses or at all of them: the outpoint, address, value and
-- block of each.
balanceColumns :: (MonadUnliftIO m) => DBAccountId -> Maybe [Text] -> ExceptT String (DB m) [(OutPoint, Text, Word64, Store.BlockRef)]
balanceColumns accId addrsM = do
  rows <- lift . select . from $ \c -> do
    where_ $ c ^. DBCoinAccount ==. val accId
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

-- | Whether the coins of a transaction of the account can be spent: it is
-- confirmed, or the account made it from coins that can be spent by this
-- same rule. Whoever pays the account can replace their transaction until
-- it confirms, which also undoes every transaction built on it. A
-- transaction the sync has not stored, or has found dead, is not trusted.
trustedTx :: (MonadUnliftIO m) => Network -> DBAccountId -> TxHash -> ExceptT String (DB m) Bool
trustedTx net accId tid = do
  found <- liveSummary net accId tid
  case found of
    Just (block, summary)
      | Store.confirmed block -> return True
      | ownTransaction summary -> and <$> mapM (trustedTx net accId) (spentTxs summary)
    _ -> return False
  where
    spentTxs summary =
      nubOrd [outpoint.hash | input <- Map.elems summary.summaryMyInputs, outpoint <- input.myInputsOutpoints]

{- Locks of the user -}

-- | Lock a coin of the wallet at the user's request: the builder no longer
-- offers it, and no pending transaction may spend it, until the user
-- unlocks it. Only a free coin can be locked: one a pending transaction
-- holds is refused, and so is one the user locked already. The lock stays
-- while the coin confirms, and goes with the coin once it is spent.
-- Returns the account of the coin and the coin as coins shows it.
lockCoin :: (MonadUnliftIO m) => OutPoint -> ExceptT String (DB m) (DBAccount, JsonCoin)
lockCoin outpoint = do
  (net, row) <- walletCoin outpoint
  locks <- coinLocks net
  case Map.lookup outpoint locks of
    Just LockedByTransaction -> throwError "The coin is already locked by a pending transaction"
    Just LockedByUser -> throwError "The coin is already locked"
    Nothing -> setLockedByUser outpoint True
  shownCoin net row

-- | Unlock a coin the user locked. A coin a pending transaction holds is
-- freed by deleting that transaction instead.
unlockCoin :: (MonadUnliftIO m) => OutPoint -> ExceptT String (DB m) (DBAccount, JsonCoin)
unlockCoin outpoint = do
  (net, row) <- walletCoin outpoint
  locks <- coinLocks net
  case Map.lookup outpoint locks of
    Just LockedByUser -> setLockedByUser outpoint False
    Just LockedByTransaction -> throwError "The coin is locked by a pending transaction, not by the user: deletetx frees it"
    Nothing -> throwError "The coin is not locked"
  shownCoin net row

-- | The accounts that hold the coins, each once. A coin the wallet does not
-- hold has none.
coinAccounts :: (MonadUnliftIO m) => [OutPoint] -> ExceptT String (DB m) [(DBAccountId, DBAccount)]
coinAccounts outpoints = do
  rows <- lift $ mapM (P.get . DBCoinKey . outpointBytes) outpoints
  getAccountsById $ nubOrd $ dBCoinAccount <$> catMaybes rows

-- | A coin of the wallet by its outpoint, with the network of its account
walletCoin :: (MonadUnliftIO m) => OutPoint -> ExceptT String (DB m) (Network, DBCoin)
walletCoin outpoint = do
  rowM <- lift $ P.get (DBCoinKey $ outpointBytes outpoint)
  row <- liftMaybe ("The wallet has no coin " <> coinText outpoint) rowM
  acc <- getAccountById (dBCoinAccount row)
  return (accountNetwork acc, row)

setLockedByUser :: (MonadUnliftIO m) => OutPoint -> Bool -> ExceptT String (DB m) ()
setLockedByUser outpoint locked =
  lift $ P.update (DBCoinKey $ outpointBytes outpoint) [DBCoinLockedByUser P.=. locked]

-- | A coin as coins shows it now, with its account
shownCoin :: (MonadUnliftIO m) => Network -> DBCoin -> ExceptT String (DB m) (DBAccount, JsonCoin)
shownCoin net row = do
  acc <- getAccountById (dBCoinAccount row)
  rules <- spendingOf net (dBCoinAccount row)
  coin <- toJsonCoin rules row
  return (acc, coin)

{- Refreshing -}

-- | What a refresh changed: how many coins were inserted, moved to another
-- block or deleted, and the coins inserted and deleted
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
-- store lists: insert the coins that are new, move those whose block
-- changed, delete those the store no longer lists. A coin the store lists
-- twice is stored once.
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
                S.encode coin.block /= dBCoinBlockRef row
            ]
      lift $ forM_ gone deleteCoin
      lift $ forM_ moved moveCoin
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
  let row =
        DBCoin
          { dBCoinAccount = accId,
            dBCoinOutpoint = outpointBytes coin.outpoint,
            dBCoinAddress = addr,
            dBCoinValue = coin.value,
            dBCoinBlockRef = S.encode coin.block,
            dBCoinLockedByUser = False,
            dBCoinCreated = time
          }
  lift $ P.insert_ row
  return row

-- | Move a stored coin to the block the store now reports for it
moveCoin :: (MonadUnliftIO m) => Store.Unspent -> DB m ()
moveCoin coin = P.update (DBCoinKey $ outpointBytes coin.outpoint) [DBCoinBlockRef P.=. S.encode coin.block]

-- | Delete a stored coin
deleteCoin :: (MonadUnliftIO m) => DBCoin -> DB m ()
deleteCoin row = P.delete (DBCoinKey $ dBCoinOutpoint row)

{- The coins a transaction leaves -}

-- | Why a transaction just prepared leaves a coin of the account: a coin
-- that coin control chose, or, for a send-all, any coin of the account
data LeftReason
  = -- | Worth less than the fee to spend it, and left as the user chose
    WorthLessThanFee
  | -- | Locked, by the user or by a pending transaction
    Locked !CoinLock
  | -- | Not confirmed enough yet, or a coinbase not yet mature
    NotSpendableYet
  | -- | Spendable, but not among the coins chosen with --coin or --spend-from
    NotGiven
  deriving (Eq, Show)

instance ToJSON LeftReason where
  toJSON = String . cs . reasonName

instance FromJSON LeftReason where
  parseJSON = withText "LeftReason" $ \name ->
    case filter ((== cs name) . reasonName) leftReasons of
      [reason] -> return reason
      _ -> fail $ "Unknown reason a coin stays in the account: " <> cs name

-- | Every reason, in the order the answer lists the coins in
leftReasons :: [LeftReason]
leftReasons = [WorthLessThanFee, Locked LockedByUser, Locked LockedByTransaction, NotSpendableYet, NotGiven]

-- | The name of a reason in JSON
reasonName :: LeftReason -> String
reasonName = \case
  WorthLessThanFee -> "dust"
  Locked LockedByUser -> "lockedbyuser"
  Locked LockedByTransaction -> "lockedbytransaction"
  NotSpendableYet -> "notspendableyet"
  NotGiven -> "notgiven"

-- | A reason as the user reads it
leftReasonText :: LeftReason -> String
leftReasonText = \case
  WorthLessThanFee -> "worth less than its fee"
  Locked LockedByUser -> "you locked it"
  Locked LockedByTransaction -> "a pending transaction spends it"
  NotSpendableYet -> "not spendable yet"
  NotGiven -> "not chosen with --coin or --spend-from"

-- | Why a coin the builder may not spend stays, from its lock: without one,
-- it is not spendable yet
lockReason :: Maybe CoinLock -> LeftReason
lockReason = maybe NotSpendableYet Locked

-- | A coin of the account a transaction leaves in it, and why
data LeftCoin = LeftCoin
  { leftCoinOutpoint :: !OutPoint,
    leftCoinValue :: !Word64,
    leftCoinReason :: !LeftReason
  }
  deriving (Eq, Show)

instance ToJSON LeftCoin where
  toJSON coin =
    object
      [ "outpoint" .= leftCoinOutpoint coin,
        "value" .= leftCoinValue coin,
        "reason" .= leftCoinReason coin
      ]

instance FromJSON LeftCoin where
  parseJSON = withObject "LeftCoin" $ \o ->
    LeftCoin <$> o .: "outpoint" <*> o .: "value" <*> o .: "reason"

-- | The coins of the account a transaction leaves, by reason in the order
-- of 'leftReasons', the largest first: the dust coins left, the coins the
-- builder may not spend with their locks, and the spendable coins not
-- chosen with coin control
leftCoins :: [Store.Unspent] -> [(Store.Unspent, Maybe CoinLock)] -> [Store.Unspent] -> [LeftCoin]
leftCoins dustLeft unspendable notGiven =
  concat [sortOn (Down . leftCoinValue) [left | left <- everyLeft, leftCoinReason left == reason] | reason <- leftReasons]
  where
    everyLeft =
      [leftOf WorthLessThanFee coin | coin <- dustLeft]
        <> [leftOf (lockReason lock) coin | (coin, lock) <- unspendable]
        <> [leftOf NotGiven coin | coin <- notGiven]
    leftOf :: LeftReason -> Store.Unspent -> LeftCoin
    leftOf reason coin = LeftCoin coin.outpoint coin.value reason

{- Coin control -}

-- | The coins that coin control chose among the coins of the account, by
-- their outpoints and by the addresses that hold them: those the builder
-- may spend now, and the others, each with its lock. The addresses count,
-- not their text. An outpoint the account does not hold is refused, and so
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
    unless (outpoint `elem` held) $ Left $ "The account has no coin " <> coinText outpoint
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
