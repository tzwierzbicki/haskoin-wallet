{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The addresses of an account: external ones to receive with and internal
-- ones for change, generated along the two chains of the account within
-- the gap of BIP44, with their labels and balances. The address at index i
-- of a chain has the derivation path chain/i, and is of the kind of the
-- account: P2PKH for a BIP44 account, P2WPKH for a BIP84 one
-- ("Haskoin.Wallet.Keys".'keyAddress'). An index whose child key is
-- invalid, which BIP32 says to skip, has no address and no row: the chain
-- goes on at the next index. An internal address is available for change
-- when no transaction has used it and no pending transaction pays it; the
-- pending transactions are read in "Haskoin.Wallet.Database.PendingTx", which asks
-- for the change address with them.
module Haskoin.Wallet.Database.Address where

import Conduit (MonadUnliftIO)
import Control.Exception (evaluate, try)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.Types as Json
import Data.List (genericTake, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Keys (AccountKind, keyAddress)
import Haskoin.Wallet.Util (addrToTextE, readNatural, rowLimit, textToAddrE)
import Numeric.Natural (Natural)

{- The two chains -}

-- | The chains of an account: external addresses to receive with, internal
-- ones for the change of its transactions
data AddrType = AddrInternal | AddrExternal
  deriving (Eq, Show)

isInternal :: AddrType -> Bool
isInternal AddrInternal = True
isInternal AddrExternal = False

-- | The next index of the chain: the indices the account has gone through,
-- its addresses and the indices skipped for an invalid child key
dBAccountCount :: AddrType -> DBAccount -> Int
dBAccountCount AddrInternal = dBAccountInternal
dBAccountCount AddrExternal = dBAccountExternal

dBAccountField :: AddrType -> EntityField DBAccount Int
dBAccountField AddrInternal = DBAccountInternal
dBAccountField AddrExternal = DBAccountExternal

addrDeriv :: AddrType -> SoftPath
addrDeriv AddrInternal = intDeriv
addrDeriv AddrExternal = extDeriv

{- Addresses in JSON -}

instance ToJSON DBAddress where
  toJSON addr = object (addressPairs addr [])

-- | An address in JSON with its spendable balance among its balances.
-- Nothing stores that balance: hw computes it from the coins wherever it
-- shows the balances of an address ("Haskoin.Wallet.Database.Coin").
addressWithSpendable :: DBAddress -> Word64 -> Json.Value
addressWithSpendable addr spendable = object (addressPairs addr ["spendable" .= spendable])

-- | An address and its spendable balance, as 'addressWithSpendable' writes them
parseAddressWithSpendable :: Json.Value -> Parser (DBAddress, Word64)
parseAddressWithSpendable value = do
  addr <- parseJSON value
  spendable <- withObject "DBAddress" (\o -> o .: "balance" >>= \bal -> bal .: "spendable") value
  return (addr, spendable)

-- | An address in JSON with the balance of its coins among its balances:
-- the total, the spendable and the unspendable values, and the parts of
-- the unspendable one, as balances --verbose shows them
addressWithCoinBalance :: DBAddress -> CoinBalance -> Json.Value
addressWithCoinBalance addr balance =
  object $
    addressPairs
      addr
      [ "total" .= coinBalanceTotal balance,
        "spendable" .= coinBalanceSpendable balance,
        "unspendable" .= coinBalanceUnspendable balance,
        "lockedbytx" .= coinBalanceLockedByTx balance,
        "lockedbyuser" .= coinBalanceLockedByUser balance,
        "notspendableyet" .= coinBalanceNotSpendableYet balance
      ]

-- | An address and the balance of its coins, as 'addressWithCoinBalance'
-- writes them. The unspendable value and the value not spendable yet
-- follow from the others.
parseAddressWithCoinBalance :: Json.Value -> Parser (DBAddress, CoinBalance)
parseAddressWithCoinBalance value = do
  addr <- parseJSON value
  balance <- withObject "DBAddress" (\o -> o .: "balance" >>= readBalance) value
  return (addr, balance)
  where
    readBalance bal = CoinBalance <$> bal .: "total" <*> bal .: "spendable" <*> bal .: "lockedbytx" <*> bal .: "lockedbyuser"

-- | The keys of an address, with more balances than those it stores when
-- they are given
addressPairs :: DBAddress -> [Pair] -> [Pair]
addressPairs addr moreBalances =
  [ "account" .= fromSqlKey (dBAddressAccount addr),
    "index" .= dBAddressIndex addr,
    "derivation" .= pathText (addrPath addr),
    "address" .= dBAddressAddress addr,
    "label" .= dBAddressLabel addr,
    "balance" .= object (balancePairs stored <> moreBalances),
    "internal" .= dBAddressInternal addr,
    "created" .= dBAddressCreated addr
  ]
  where
    stored =
      AddressBalance
        (dBAddressBalanceConfirmed addr)
        (dBAddressBalanceUnconfirmed addr)
        (dBAddressBalanceCoins addr)
        (dBAddressBalanceTxs addr)
        (dBAddressBalanceReceived addr)

instance FromJSON DBAddress where
  parseJSON =
    withObject "DBAddress" $ \o -> do
      bal <- o .: "balance"
      DBAddress
        <$> (toSqlKey <$> o .: "account")
        <*> o .: "internal"
        <*> o .: "index"
        <*> o .: "address"
        <*> o .: "label"
        <*> bal .: "confirmed"
        <*> bal .: "unconfirmed"
        <*> bal .: "coins"
        <*> bal .: "txs"
        <*> bal .: "received"
        <*> o .: "created"

-- | The balance of an address as the store reports it
data AddressBalance = AddressBalance
  { addrBalanceConfirmed :: !Word64,
    addrBalanceUnconfirmed :: !Word64,
    -- | The number of unspent outputs
    addrBalanceCoins :: !Word64,
    -- | The number of transactions
    addrBalanceTxs :: !Word64,
    -- | The total of every output paid to the address
    addrBalanceReceived :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AddressBalance where
  toJSON = object . balancePairs

balancePairs :: AddressBalance -> [Pair]
balancePairs b =
  [ "confirmed" .= addrBalanceConfirmed b,
    "unconfirmed" .= addrBalanceUnconfirmed b,
    "coins" .= addrBalanceCoins b,
    "txs" .= addrBalanceTxs b,
    "received" .= addrBalanceReceived b
  ]

instance FromJSON AddressBalance where
  parseJSON =
    withObject "accountbalance" $ \o ->
      AddressBalance
        <$> o .: "confirmed"
        <*> o .: "unconfirmed"
        <*> o .: "coins"
        <*> o .: "txs"
        <*> o .: "received"

-- | The balance of the coins at an address: the value of all of them, of
-- those that can be spent now, and of those that a pending transaction or
-- the user locks. The rest waits for a confirmation, or for a coinbase to
-- mature. hw computes it from the coins it stores
-- ("Haskoin.Wallet.Database.Coin"), where the store reports
-- 'AddressBalance'. The parts follow the states coins gives each coin, so
-- that balances --verbose tells why a value cannot be spent.
data CoinBalance = CoinBalance
  { coinBalanceTotal :: !Word64,
    coinBalanceSpendable :: !Word64,
    -- | The value of the coins a pending transaction spends
    coinBalanceLockedByTx :: !Word64,
    -- | The value of the coins the user locked with lockcoin
    coinBalanceLockedByUser :: !Word64
  }
  deriving (Show, Eq)

-- | The balance of the coins of an address is the sum of the balances of
-- each coin, part by part
instance Semigroup CoinBalance where
  a <> b =
    CoinBalance
      (coinBalanceTotal a + coinBalanceTotal b)
      (coinBalanceSpendable a + coinBalanceSpendable b)
      (coinBalanceLockedByTx a + coinBalanceLockedByTx b)
      (coinBalanceLockedByUser a + coinBalanceLockedByUser b)

-- | The value of the coins at the address that cannot be spent now: those
-- a pending transaction or the user locks, and those not spendable yet
coinBalanceUnspendable :: CoinBalance -> Word64
coinBalanceUnspendable balance = coinBalanceTotal balance - coinBalanceSpendable balance

-- | The value of the coins at the address that nothing locks, but that
-- cannot be spent yet: unconfirmed payments from other wallets, and
-- coinbases that have not matured
coinBalanceNotSpendableYet :: CoinBalance -> Word64
coinBalanceNotSpendableYet balance =
  coinBalanceUnspendable balance - coinBalanceLockedByTx balance - coinBalanceLockedByUser balance

{- Generating addresses -}

-- | A new receiving address, with its label. Every call gives the next
-- address of the external chain, within the gap: once the gap of them await
-- a payment, one of those is to be reused.
genExtAddress :: (MonadUnliftIO m) => Ctx -> Config -> DBAccountId -> Text -> ExceptT String (DB m) DBAddress
genExtAddress ctx cfg accId label = do
  addr <- genNextAddress ctx cfg accId AddrExternal
  setAddrLabel accId (dBAddressIndex addr) label

-- | The change address for a new transaction: the lowest internal address
-- that no transaction has used and that is not among the given reserved
-- paths, those the pending transactions pay their change to; the next
-- address of the chain when there is none. An address is available again as
-- soon as nothing holds it, so a deleted pending transaction gives its
-- change address back.
nextChangeAddress :: (MonadUnliftIO m) => Ctx -> Config -> DBAccountId -> [SoftPath] -> ExceptT String (DB m) DBAddress
nextChangeAddress ctx cfg accId reserved = do
  found <- lift . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccount ==. val accId
        &&. a ^. DBAddressInternal ==. val True
        &&. a ^. DBAddressBalanceTxs ==. val 0
        &&. a ^. DBAddressIndex `notIn` valList [fromIntegral i | Just (AddrInternal, i) <- chainIndex <$> reserved]
    orderBy [asc $ a ^. DBAddressIndex]
    limit 1
    return a
  maybe (genNextAddress ctx cfg accId AddrInternal) (return . entityVal) found

-- | The next address of a chain, within the gap
genNextAddress :: (MonadUnliftIO m) => Ctx -> Config -> DBAccountId -> AddrType -> ExceptT String (DB m) DBAddress
genNextAddress ctx cfg accId addrType = do
  checkGap cfg accId addrType
  appendAddress ctx accId addrType

-- | Generate the addresses of a chain up to the given index, as a scan of
-- the store found them used, or as the key path of a record of a file
-- says: the gap does not apply to used addresses
extendChainTo :: (MonadUnliftIO m) => Ctx -> DBAccountId -> AddrType -> Natural -> ExceptT String (DB m) ()
extendChainTo ctx accId addrType next = do
  acc <- getAccountById accId
  when (fromIntegral (dBAccountCount addrType acc) < next) $ do
    _ <- appendAddress ctx accId addrType
    extendChainTo ctx accId addrType next

-- | Add the next address of a chain to the account, whatever the gap. An
-- index whose child key is invalid is passed over, as BIP32 asks: the
-- chain moves on to the next index and the skipped one has no row.
appendAddress :: (MonadUnliftIO m) => Ctx -> DBAccountId -> AddrType -> ExceptT String (DB m) DBAddress
appendAddress ctx accId addrType = do
  acc <- getAccountById accId
  let next = dBAccountCount addrType acc
  addrM <- liftIO $ deriveChainAddress ctx (accountKind acc) (accountXPubKey ctx acc) (addrDeriv addrType) (fromIntegral next)
  lift $ P.update accId [dBAccountField addrType P.=. next + 1]
  case addrM of
    Just addr -> insertAddress (accountNetwork acc) accId addrType next addr
    Nothing -> appendAddress ctx accId addrType

-- | Refuse to generate past the gap of BIP44: no more than the gap of unused
-- addresses beyond the highest used one of the chain
checkGap :: (MonadUnliftIO m) => Config -> DBAccountId -> AddrType -> ExceptT String (DB m) ()
checkGap cfg accId addrType = do
  acc <- getAccountById accId
  usedM <- lift $ highestUsedIndex accId addrType
  let gap = configGap cfg
      allowed = maybe 0 (+ 1) usedM + fromIntegral gap
  when (dBAccountCount addrType acc >= allowed) $
    throwError $ case addrType of
      AddrExternal -> "The last " <> show gap <> " receiving addresses await a payment: reuse one of them, listed by hw addresses"
      AddrInternal -> "The last " <> show gap <> " change addresses are held by pending transactions: send or delete some of them"

-- | How far hw generates past the last used address of a chain on its
-- own: a thousand addresses. A scan looks at this many before it asks
-- whether to go on, while the store reports a used address in every
-- window ("Haskoin.Wallet.Sync"), and importacc derives no address from
-- the key path of a record beyond that many past the last used one
-- ('withinScanLimit'). A thousand is far more than an account gains
-- between two syncs. A store that lies reports used addresses without
-- end, and a file can name any index up to two billion: generating them
-- would fill the gap of this wallet for good, put the next receiving
-- address past any gap a wallet looks at when it recovers the seed, and
-- take days.
scanLimit :: Natural
scanLimit = 1000

-- | Whether an index of a chain is within 'scanLimit' of the last used
-- address of the chain, or of its start when none is used: how far
-- importacc derives an address from the key path of a record. The gap
-- does not apply, since the file says the address was used.
withinScanLimit :: (MonadUnliftIO m) => DBAccountId -> AddrType -> KeyIndex -> DB m Bool
withinScanLimit accId addrType index = do
  usedM <- highestUsedIndex accId addrType
  let allowed = maybe 0 (+ 1) usedM + fromIntegral scanLimit
  return (fromIntegral index < allowed)

-- | The highest index of the chain with transactions, if any
highestUsedIndex :: (MonadUnliftIO m) => DBAccountId -> AddrType -> DB m (Maybe Int)
highestUsedIndex accId addrType = do
  resM <- (flatMaybe <$>) . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccount ==. val accId
        &&. a ^. DBAddressInternal ==. val (isInternal addrType)
        &&. a ^. DBAddressBalanceTxs >. val 0
    return $ max_ $ a ^. DBAddressIndex
  return $ fromIntegral <$> resM

-- | Store an address of a chain, at its index, without a label or balances
insertAddress :: (MonadUnliftIO m) => Network -> DBAccountId -> AddrType -> Int -> Address -> ExceptT String (DB m) DBAddress
insertAddress net accId addrType index addr = do
  time <- liftIO getCurrentTime
  addrT <- liftEither $ addrToTextE net addr
  let row = DBAddress accId (isInternal addrType) index addrT "" 0 0 0 0 0 time
  lift $ P.insert_ row
  return row

-- | Label a receiving address of the account, by its index
setAddrLabel :: (MonadUnliftIO m) => DBAccountId -> Int -> Text -> ExceptT String (DB m) DBAddress
setAddrLabel accId idx label = do
  let key = DBAddressKey accId False idx
  row <- lift (P.get key) >>= liftMaybe ("Address " <> show idx <> " does not exist")
  lift $ P.update key [DBAddressLabel P.=. label]
  return row {dBAddressLabel = label}

-- | Label a receiving address of the wallet, written in any form
-- 'getAddressByText' reads. The address tells its account. A change
-- address takes no label.
labelAddress :: (MonadUnliftIO m) => Text -> Text -> ExceptT String (DB m) DBAddress
labelAddress addrTxt label = do
  (_, addr) <- getAddressByText addrTxt
  when (dBAddressInternal addr) $
    throwError "A change address takes no label. Only a receiving address does."
  setAddrLabel (dBAddressAccount addr) (dBAddressIndex addr) label

{- Addresses in the database -}

-- | Store the balances the store reports for addresses of the account. A
-- balance of an address of another account changes nothing.
updateAddressBalances :: (MonadUnliftIO m) => Network -> DBAccountId -> [Store.Balance] -> ExceptT String (DB m) ()
updateAddressBalances net accId storeBals =
  forM_ storeBals $ \s -> do
    addrT <- liftEither $ addrToTextE net s.address
    lift . update $ \a -> do
      set
        a
        [ DBAddressBalanceConfirmed =. val s.confirmed,
          DBAddressBalanceUnconfirmed =. val s.unconfirmed,
          DBAddressBalanceCoins =. val s.utxo,
          DBAddressBalanceTxs =. val s.txs,
          DBAddressBalanceReceived =. val s.received
        ]
      where_ $
        a ^. DBAddressAddress ==. val addrT
          &&. a ^. DBAddressAccount ==. val accId

-- | An address of the wallet, however it is written, with its account: see
-- 'addressesAmong'. An account key is in the wallet once, so an address is
-- of one account at most.
getAddressByText :: (MonadUnliftIO m) => Text -> ExceptT String (DB m) (DBAccount, DBAddress)
getAddressByText addrTxt =
  maybe (throwError "Address does not exist") return =<< lift (findAddressByText addrTxt)

-- | The address of the wallet a text names, as 'getAddressByText' finds
-- it, or Nothing when no account holds it
findAddressByText :: (MonadUnliftIO m) => Text -> DB m (Maybe (DBAccount, DBAddress))
findAddressByText addrTxt = do
  accounts <- getAccounts
  found <- forM accounts $ \(accId, acc) -> map (acc,) <$> addressesAmong [addrTxt] (accId, acc)
  return (listToMaybe (concat found))

-- | The accounts that have the addresses, each once, however the addresses
-- are written: see 'addressesAmong'. An address the wallet does not have
-- has none.
addressAccounts :: (MonadUnliftIO m) => [Text] -> ExceptT String (DB m) [(DBAccountId, DBAccount)]
addressAccounts addrs = accountsHolding (\net -> mapMaybe (textToAddr net) addrs)

-- | The accounts that have some of the addresses each account reads on its
-- network, each once: the addresses of a text that reads differently on
-- each network, such as a recipient with its amount
accountsHolding :: (MonadUnliftIO m) => (Network -> [Address]) -> ExceptT String (DB m) [(DBAccountId, DBAccount)]
accountsHolding readOn = lift $ filterM holds =<< getAccounts
  where
    holds held@(_, acc) = not . null <$> storedAmong (readOn (accountNetwork acc)) held

-- | The addresses of the account among the given ones. The account reads
-- them on its network, and compares the addresses themselves, not their
-- text: a Bitcoin Cash address is found written without its prefix, or in
-- the legacy form of Bitcoin, as well as in the form hw stores.
addressesAmong :: (MonadUnliftIO m) => [Text] -> (DBAccountId, DBAccount) -> DB m [DBAddress]
addressesAmong addrs held@(_, acc) = storedAmong (mapMaybe (textToAddr (accountNetwork acc)) addrs) held

-- | The addresses of the account among the given addresses of its network
storedAmong :: (MonadUnliftIO m) => [Address] -> (DBAccountId, DBAccount) -> DB m [DBAddress]
storedAmong addrs (accId, acc) = do
  let net = accountNetwork acc
      -- The text hw stores for each address
      stored = mapMaybe (addrToText net) addrs
  rows <- select . from $ \a -> do
    where_ $
      a ^. DBAddressAccount ==. val accId
        &&. a ^. DBAddressAddress `in_` valList stored
    return a
  return $ entityVal <$> rows

-- | The receiving addresses of the account that have a label, in the order
-- of the chain: what exportacc writes
labelledAddresses :: (MonadUnliftIO m) => DBAccountId -> DB m [DBAddress]
labelledAddresses accId = do
  rows <- select . from $ \a -> do
    where_ $
      a ^. DBAddressAccount ==. val accId
        &&. a ^. DBAddressInternal ==. val False
        &&. a ^. DBAddressLabel !=. val ""
    orderBy [asc (a ^. DBAddressIndex)]
    return a
  return $ entityVal <$> rows

-- | Those of the addresses that have transactions on the chain
usedAddresses :: (MonadUnliftIO m) => [Text] -> DB m [Text]
usedAddresses addrs = do
  rows <- select . from $ \a -> do
    where_ $
      a ^. DBAddressAddress `in_` valList addrs
        &&. a ^. DBAddressBalanceTxs >. val 0
    return $ a ^. DBAddressAddress
  return $ unValue <$> rows

-- | The text of at most the given number of addresses of the account on
-- the chain that start with the text, oldest first, as the shell offers
-- them. A Bitcoin Cash address is typed without its prefix, so the text
-- is matched after the colon of the stored address when it has one.
addressesStarting :: (MonadUnliftIO m) => DBAccountId -> AddrType -> Text -> Natural -> DB m [Text]
addressesStarting accId addrType start atMost = do
  rows <-
    rawSql
      "select address from d_b_address where account = ? and internal = ? and substr(address, instr(address, ':') + 1, ?) = ? order by \"index\" limit ?"
      [P.toPersistValue accId, P.toPersistValue (isInternal addrType), P.toPersistValue (Text.length start), P.toPersistValue start, P.toPersistValue (rowLimit atMost :: Int)]
  return (unSingle <$> rows)

-- | The addresses of a chain of the account, newest first: those after
-- the index given, or the first ones, up to the count, or all of them
addressesAfter :: (MonadUnliftIO m) => DBAccountId -> AddrType -> Maybe Int -> Maybe Natural -> DB m [DBAddress]
addressesAfter accId addrType afterM countM = do
  rows <- select . from $ \a -> do
    where_ $
      a ^. DBAddressAccount ==. val accId
        &&. a ^. DBAddressInternal ==. val (isInternal addrType)
    forM_ afterM $ \index -> where_ $ a ^. DBAddressIndex <. val index
    orderBy [desc (a ^. DBAddressIndex)]
    forM_ countM $ limit . rowLimit
    return a
  return $ entityVal <$> rows

-- | The row of an address of the account on the chain, the one --after
-- names, or why there is none
listedAddress :: (MonadUnliftIO m) => Network -> DBAccountId -> AddrType -> Text -> ExceptT String (DB m) DBAddress
listedAddress net accId addrType text = do
  _ <- liftEither $ textToAddrE net text
  rowM <- lift $ P.getBy (UniqueAddress text)
  case rowM of
    Just (Entity _ row)
      | dBAddressAccount row == accId && dBAddressInternal row == isInternal addrType -> return row
    _ -> throwError $ "The address " <> cs text <> " is not a " <> kind <> " address of the account"
  where
    kind = case addrType of
      AddrExternal -> "receiving"
      AddrInternal -> "change"

-- | Every address of the account, with its derivation path and its balance
allAddressesMap :: (MonadUnliftIO m) => Network -> DBAccountId -> ExceptT String (DB m) (Map Address SoftPath, Map Address AddressBalance)
allAddressesMap net accId = do
  rows <- lift . select . from $ \a -> do
    where_ $
      a ^. DBAddressAccount ==. val accId
    return a
  res <- forM rows $ \(Entity _ row) -> do
    (addr, path) <- liftEither $ fromDBAddr net row
    let bal = AddressBalance (dBAddressBalanceConfirmed row) (dBAddressBalanceUnconfirmed row) (dBAddressBalanceCoins row) (dBAddressBalanceTxs row) (dBAddressBalanceReceived row)
    return ((addr, path), (addr, bal))
  return (Map.fromList $ fst <$> res, Map.fromList $ snd <$> res)

-- | The derivation path of an address of the account, or Nothing for an
-- address that is not the account's
addressPath :: (MonadUnliftIO m) => Network -> DBAccountId -> Address -> ExceptT String (DB m) (Maybe SoftPath)
addressPath net accId addr = do
  addrT <- liftEither $ addrToTextE net addr
  rowM <- lift . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAddress ==. val addrT
        &&. a ^. DBAddressAccount ==. val accId
    return (a ^. DBAddressInternal, a ^. DBAddressIndex)
  return $ (\(Value internal, Value index) -> chainPath internal index) <$> rowM

-- | The addresses that belong to the account, with their derivation paths
walletPaths :: (MonadUnliftIO m) => Network -> DBAccountId -> [Address] -> ExceptT String (DB m) (Map Address SoftPath)
walletPaths net accId addrs = Map.fromList . catMaybes <$> mapM pathOf (nub addrs)
  where
    pathOf addr = fmap (\path -> (addr, path)) <$> addressPath net accId addr

{- Derivation paths -}

extDeriv :: SoftPath
extDeriv = Deriv :/ 0

intDeriv :: SoftPath
intDeriv = Deriv :/ 1

isIntPath :: SoftPath -> Bool
isIntPath p =
  case pathToList p of
    [1, _] -> True
    _ -> False

-- | A derivation path as JSON and the signing data write it
pathText :: SoftPath -> Text
pathText = cs . pathToStr

-- | The derivation path of an address, from its chain and index
chainPath :: Bool -> Int -> SoftPath
chainPath internal index = (if internal then intDeriv else extDeriv) :/ fromIntegral index

-- | The chain and index of a derivation path, for one of the two chains
chainIndex :: SoftPath -> Maybe (AddrType, KeyIndex)
chainIndex path =
  case pathToList path of
    [0, i] -> Just (AddrExternal, i)
    [1, i] -> Just (AddrInternal, i)
    _ -> Nothing

-- | The chain and index of a path the user types, written as addresses
-- shows a path: /0/i for the receiving address i, /1/i for the change
-- address i. Every other text is refused, with the two forms: a hardened
-- step, a third step, another chain, or an index of 2^31 or more, which
-- BIP32 keeps for hardened steps. The index is read here and not by the
-- path parser of haskoin-core, which reads it as a 32-bit number modulo
-- 2^32, and would take /0/4294967296 for /0/0.
readChainPath :: Text -> Either String (AddrType, KeyIndex)
readChainPath text =
  case Text.splitOn "/" text of
    ["", chain, index]
      | Just addrType <- lookup chain [("0", AddrExternal), ("1", AddrInternal)],
        Just i <- readNatural index,
        i < 0x80000000 ->
          Right (addrType, fromIntegral i)
    _ -> Left $ "The path " <> cs text <> " is not that of an address: /0/i is the receiving address i, and /1/i the change address i"

-- | The derivation path of a row
addrPath :: DBAddress -> SoftPath
addrPath addr = chainPath (dBAddressInternal addr) (dBAddressIndex addr)

-- | The address of a row with its derivation path
fromDBAddr :: Network -> DBAddress -> Either String (Address, SoftPath)
fromDBAddr net row = (,addrPath row) <$> textToAddrE net (dBAddressAddress row)

-- | The address of a chain of an account of the kind at an index, or
-- Nothing when the child key at that index is invalid: haskoin-core throws
-- for such a key, where BIP32 says to skip the index
deriveChainAddress :: Ctx -> AccountKind -> XPubKey -> SoftPath -> KeyIndex -> IO (Maybe Address)
deriveChainAddress ctx kind = deriveChainAddressBy (Just . keyAddress kind ctx) ctx

-- | 'deriveChainAddress', with the address of a key given by the function:
-- for the accounts hw does not hold, whose addresses are of no kind of
-- 'AccountKind'. Nothing as well when the function gives the key no
-- address, as for a kind of address the network does not have.
deriveChainAddressBy :: (XPubKey -> Maybe Address) -> Ctx -> XPubKey -> SoftPath -> KeyIndex -> IO (Maybe Address)
deriveChainAddressBy addressOf ctx pub chain i = do
  derived <- try $ evaluate $ derivePubPath ctx (chain :/ i) pub
  return $ case derived of
    Left (DerivationException _) -> Nothing
    Right child -> addressOf child

-- | The addresses of a chain of the public key of an account of the kind
-- from an index, as many as asked, with their indices; an index with an
-- invalid child key is left out
chainAddresses :: Ctx -> AccountKind -> XPubKey -> SoftPath -> Natural -> Natural -> IO [(Address, KeyIndex)]
chainAddresses ctx kind = chainAddressesBy (Just . keyAddress kind ctx) ctx

-- | 'chainAddresses', with the address of a key given by the function
chainAddressesBy :: (XPubKey -> Maybe Address) -> Ctx -> XPubKey -> SoftPath -> Natural -> Natural -> IO [(Address, KeyIndex)]
chainAddressesBy addressOf ctx pub chain firstIndex howMany =
  fmap catMaybes . forM (genericTake howMany [fromIntegral firstIndex ..]) $ \i ->
    fmap (\addr -> (addr, i)) <$> deriveChainAddressBy addressOf ctx pub chain i
