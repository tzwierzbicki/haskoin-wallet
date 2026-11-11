{-# LANGUAGE LambdaCase #-}
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
--
-- A row belongs to its account. The transactions and the payments work on
-- a pool, the accounts of a portfolio on one network together, so the
-- addresses of a pool are read here too, each with its 'PoolPath': the
-- kind of its account and its path below the key of that account
-- ('poolAddressesMap', 'poolPaths'). A row does not keep the kind of its
-- account. The form of its address tells it ('addrPoolPath'), so that the
-- views and the JSON answers name every address of a portfolio by its kind
-- and its path, as in bip84/0/3. A path the user types may start with its
-- kind the same way ('readTypedChainPath').
--
-- The test networks share their keys, so the same address may be in the
-- accounts of a portfolio on several of them. An address the user types
-- names one row only once the network is known ('addressByText'). So does
-- a path the user types, since a portfolio has an address at the same
-- path on each network it gave one out on ('heldAddressAt'). Both pick
-- the network by one rule ('chooseOnNetwork'). A command that takes an
-- address of the wallet takes its path too ('AddressNamed').
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
import Data.Char (isDigit)
import Data.List (genericTake, intercalate, nub)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
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
import Haskoin.Wallet.Database.Portfolio (Portfolio (..), portfolioText)
import Haskoin.Wallet.Keys (AccountKind, PoolPath (..), addressKind, checkKindOnNetwork, keyAddress, kindNamed, kindNames, poolPathText)
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

-- | An address of the network in JSON, as an answer gives it, with its
-- spendable balance among its balances. Nothing stores that balance: hw
-- computes it from the coins wherever it shows the balances of an address
-- ("Haskoin.Wallet.Database.Coin"). The type of its account comes with
-- its path ('addressTypePairs').
addressWithSpendable :: Network -> DBAddress -> Word64 -> Json.Value
addressWithSpendable net addr spendable = object (addressTypePairs net addr <> addressPairs addr ["spendable" .= spendable])

-- | An address and its spendable balance, as 'addressWithSpendable' writes them
parseAddressWithSpendable :: Json.Value -> Parser (DBAddress, Word64)
parseAddressWithSpendable value = do
  addr <- parseJSON value
  spendable <- withObject "DBAddress" (\o -> o .: "balance" >>= \bal -> bal .: "spendable") value
  return (addr, spendable)

-- | An address of the network in JSON, as balances gives it, with the
-- balance of its coins among its balances: the total, the spendable and
-- the unspendable values, and the parts of the unspendable one, as
-- balances --verbose shows them. The type of its account comes with its
-- path ('addressTypePairs').
addressWithCoinBalance :: Network -> DBAddress -> CoinBalance -> Json.Value
addressWithCoinBalance net addr balance =
  object $
    addressTypePairs net addr
      <> addressPairs
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

-- | The key of the type of the account of an address of the network,
-- beside its "derivation": a group of balances lists the addresses of both
-- accounts of a network, and each has one at /0/3. The coins and the
-- transactions write the path of an address of the wallet with its type
-- the same way. None for an address that does not read on the network,
-- which hw never stores. The row does not keep the type, so a row read
-- back from JSON ignores it.
addressTypePairs :: Network -> DBAddress -> [Pair]
addressTypePairs net addr = ["type" .= pathKind path | Just path <- [addrPoolPath net addr]]

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
-- window ("Haskoin.Wallet.Sync"), and importwallet derives no address from
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
-- importwallet derives an address from the key path of a record. The gap
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

-- | Label a receiving address of an account, given its row. A change
-- address takes no label.
labelAddress :: (MonadUnliftIO m) => DBAddress -> Text -> ExceptT String (DB m) DBAddress
labelAddress addr label = do
  when (dBAddressInternal addr) $
    throwError changeTakesNoLabel
  setAddrLabel (dBAddressAccount addr) (dBAddressIndex addr) label

-- | The refusal of a label for a change address. Its payments are the
-- change of the transactions of the wallet, which their own labels name.
changeTakesNoLabel :: String
changeTakesNoLabel = "A change address takes no label. Only a receiving address does."

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

-- | Every row of an address the user typed, however it is written, each
-- with its account and the portfolio of that account: see
-- 'addressesAmong'. A key is in the database once on a network, so an
-- address has one row at most on a network. It has a row on each test
-- network where the portfolio generated it, since those networks share
-- their keys.
findAddressByText :: (MonadUnliftIO m) => Text -> DB m [(Portfolio, DBAccountId, DBAddress)]
findAddressByText addrTxt = do
  accounts <- getAccounts
  fmap concat . forM accounts $ \(portfolio, accId, acc) ->
    map (portfolio,accId,) <$> addressesAmong [addrTxt] (accId, acc)

-- | The one row of an address the user typed, with its account and the
-- portfolio of that account. An address with rows on several networks
-- names the one on the network given, the --network of the command, or
-- else the one on the default network of the configuration. When neither
-- settles it, the user is asked for --network, with the networks named:
-- hw does not guess which chain an address is meant on.
addressByText :: (MonadUnliftIO m) => Config -> Maybe Network -> Text -> ExceptT String (DB m) (Portfolio, DBAccountId, DBAddress)
addressByText cfg netM addrTxt = addressByTextOr ("The wallet has no address " <> cs addrTxt) cfg netM addrTxt

-- | 'addressByText', with the refusal of an address the wallet does not
-- have given: a command can say there what the user may do instead
addressByTextOr :: (MonadUnliftIO m) => String -> Config -> Maybe Network -> Text -> ExceptT String (DB m) (Portfolio, DBAccountId, DBAddress)
addressByTextOr notHeld cfg netM addrTxt = do
  found <- lift $ findAddressByText addrTxt
  onNetworks <- forM found $ \row@(_, accId, _) -> (row,) . accountNetwork <$> getAccountById accId
  case chooseOnNetwork cfg netM onNetworks of
    Right row -> return row
    Left FoundNowhere -> throwError notHeld
    Left (NotOnGivenNetwork net networks) ->
      throwError $ "The address " <> cs addrTxt <> " is on " <> networksText networks <> ", not on the --network " <> net.name
    Left (OnSeveralNetworks networks) -> throwError (onSeveralNetworks addrTxt networks)

-- | The row of the address at a path of a portfolio, with its account:
-- what label and signmsg take for a path, such as bip84/0/3. The account
-- is of the type given, or else of the default type of its network
-- ('defaultKindOn'). The wallet must hold the address. A path past the
-- addresses the portfolio gave out has no row, and is refused: only
-- receive gives out addresses, within the gap, and only a payment takes
-- a change address.
--
-- A portfolio has an address at the same path on each network it gave one
-- out on, so a path names one row only once the network is known. It is
-- the one on the network given, or the only one, or else the one on the
-- default network, as for an address ('chooseOnNetwork'). A type given
-- that the network given does not have is refused, as 'chooseKind'
-- refuses it.
heldAddressAt :: (MonadUnliftIO m) => Config -> Portfolio -> Maybe Network -> Maybe AccountKind -> AddrType -> KeyIndex -> ExceptT String (DB m) (DBAccountId, DBAddress)
heldAddressAt cfg portfolio netM kindM addrType index = do
  forM_ ((,) <$> netM <*> kindM) $ \(net, kind) -> liftEither (checkKindOnNetwork net kind)
  found <- lift $ rowsAtPath cfg portfolio kindM addrType index
  case chooseOnNetwork cfg netM found of
    Right held -> return held
    Left FoundNowhere ->
      throwError $ owner <> " has no address at " <> typedPath <> ": " <> listedBy
    Left (NotOnGivenNetwork net networks) ->
      throwError $ owner <> " has an address at " <> typedPath <> " on " <> networksText networks <> ", not on the --network " <> net.name
    Left (OnSeveralNetworks networks) ->
      throwError $ owner <> " has an address at " <> typedPath <> " on " <> networksText networks <> ": give the --network"
  where
    owner = portfolioText portfolio
    typedPath = chainPathText kindM addrType index
    listedBy = case addrType of
      AddrExternal -> "addresses lists the addresses it gave out"
      AddrInternal -> "addresses --change lists the change addresses it took"

-- | The rows of a portfolio at a path of one of its chains, each with its
-- account and its network: one on each network where the portfolio has
-- an address there. The account is of the type given, or else of the
-- default type of its network ('defaultKindOn'), so that a path without
-- its type, as in /0/3, is of the account receive gives out addresses of.
rowsAtPath :: (MonadUnliftIO m) => Config -> Portfolio -> Maybe AccountKind -> AddrType -> KeyIndex -> DB m [((DBAccountId, DBAddress), Network)]
rowsAtPath cfg portfolio kindM addrType index = do
  accounts <- portfolioAccountRows portfolio.portfolioId
  fmap catMaybes . forM accounts $ \(accId, acc) -> do
    let net = accountNetwork acc
    if accountKind acc == fromMaybe (defaultKindOn cfg net) kindM
      then fmap (\row -> ((accId, row), net)) <$> addressAt accId addrType index
      else return Nothing

-- | The row of an account at an index of one of its chains, if the account
-- has an address there
addressAt :: (MonadUnliftIO m) => DBAccountId -> AddrType -> KeyIndex -> DB m (Maybe DBAddress)
addressAt accId addrType index = P.get (DBAddressKey accId (isInternal addrType) (fromIntegral index))

-- | A path as the user typed it, with the type of its account when it has
-- one: bip84/1/0, or /1/0
chainPathText :: Maybe AccountKind -> AddrType -> KeyIndex -> String
chainPathText kindM addrType index =
  maybe (pathToStr path) (\kind -> cs (poolPathText (PoolPath kind path))) kindM
  where
    path = addrDeriv addrType :/ index

{- An address the user names by its text or its path -}

-- | An address of the wallet as the user names it, where a command takes
-- an address of the wallet: the address itself, as typed, or its path in
-- the active portfolio, with the type of its account, as the views write
-- it (bip84/1/0). The views name every address of the wallet by its path,
-- so the user can type back what a view shows. A path is short to type,
-- and the network the command works on, or its --network, tells which
-- address it is.
data AddressNamed
  = NamedByAddress !Text
  | NamedByPath !AccountKind !AddrType !KeyIndex
  deriving (Eq, Show)

-- | An address of the wallet as the user types it, by its shape: a path
-- when it has the shape of one ('readTypedChainPath'), an address
-- otherwise. A path must start with the type of its account. The options
-- that read it have no --type to complete a path without it, and a
-- portfolio has an address at /1/0 in each of its accounts on a network.
-- A word of the shape of a path that is no path is refused, with the
-- forms to write.
readAddressNamed :: String -> Either String AddressNamed
readAddressNamed word = case readTypedChainPath (cs word) of
  Nothing -> Right (NamedByAddress (cs word))
  Just (Left refusal) -> Left refusal
  Just (Right (Just kind, addrType, index)) -> Right (NamedByPath kind addrType index)
  Just (Right (Nothing, _, _)) ->
    Left $
      "The path "
        <> word
        <> " needs the type of its account in front, as the views write it: "
        <> intercalate " or " [cs name <> word | name <- kindNames]

-- | How the messages name an address the user named: as typed, or by its
-- path with its type
addressNamedText :: AddressNamed -> String
addressNamedText = \case
  NamedByAddress typed -> cs typed
  NamedByPath kind addrType index -> chainPathText (Just kind) addrType index

-- | The address of a pool that the user named, when the pool holds it. An
-- address is read on the network of the pool, and found whatever its
-- form ('inPool'). A path is that of the account of its type in the pool.
poolAddressNamed :: (MonadUnliftIO m) => Pool -> AddressNamed -> ExceptT String (DB m) (Maybe Address)
poolAddressNamed pool = \case
  NamedByAddress typed -> do
    addr <- liftEither $ textToAddrE pool.poolNetwork typed
    held <- lift $ inPool pool addr
    return (if held then Just addr else Nothing)
  NamedByPath kind addrType index -> do
    accountM <- lift $ poolAccount pool kind
    rowM <- lift $ maybe (return Nothing) (\(accId, _) -> addressAt accId addrType index) accountM
    traverse (liftEither . textToAddrE pool.poolNetwork . dBAddressAddress) rowM

-- | Why none of the rows found, each on its network, is the one a command
-- means ('chooseOnNetwork')
data NetworkMiss
  = -- | No row was found, on any network
    FoundNowhere
  | -- | No row is on the --network given. The rows are on the networks
    -- listed.
    NotOnGivenNetwork !Network ![Network]
  | -- | The rows are on the networks listed, and none of them is the
    -- default network
    OnSeveralNetworks ![Network]

-- | The one row a command means among those found, each on its network:
-- the one on the --network given, or the only one found, or else the one
-- on the default network of the configuration. An address the user types
-- can be on several networks, since the test networks share their keys.
-- A portfolio has an address at a path on each network it gave one out
-- on. When none of the three settles it, the user is asked for the
-- --network: hw does not guess which chain is meant. The caller says what
-- was not found, in its own words.
chooseOnNetwork :: Config -> Maybe Network -> [(row, Network)] -> Either NetworkMiss row
chooseOnNetwork cfg netM found =
  case (netM, found) of
    (_, []) -> Left FoundNowhere
    (Just net, _) -> firstOn net (NotOnGivenNetwork net networks)
    (Nothing, [(row, _)]) -> Right row
    (Nothing, _) -> firstOn (configDefaultNetwork cfg) (OnSeveralNetworks networks)
  where
    networks = nub (snd <$> found)
    firstOn net missing = maybe (Left missing) Right (lookup net [(rowNet, row) | (row, rowNet) <- found])

-- | The refusal of an address the user typed that the wallet has on
-- several networks, when nothing says which one is meant: the test
-- networks share their keys, and hw does not guess the chain
onSeveralNetworks :: Text -> [Network] -> String
onSeveralNetworks addrTxt networks =
  "The address " <> cs addrTxt <> " is on " <> networksText (nub networks) <> ": give the --network"

-- | The section a listing goes on in after the address --after names,
-- among the sections of the listing, each given with its network and the
-- row the address has in it, if any: the index of the one section that
-- holds the address, with its row, or Nothing when none holds it.
--
-- The test networks share their keys, so the same address is in the
-- sections of several networks when the listing covers them. Starting in
-- the first of those would loop: its last row names the address again,
-- and the listing would go on in the first section each time. So an
-- address that several sections hold is refused, and the user is asked
-- for --network, as 'addressByText' asks for it.
sectionOfAddress :: Text -> [(Network, Maybe row)] -> Either String (Maybe (Int, row))
sectionOfAddress addrTxt sections =
  case [(i, net, row) | (i, (net, Just row)) <- zip [0 ..] sections] of
    [] -> Right Nothing
    [(i, _, row)] -> Right (Just (i, row))
    several -> Left (onSeveralNetworks addrTxt [net | (_, net, _) <- several])

-- | The accounts that have some of the addresses each account reads on its
-- network, each once, with their portfolios: the addresses of a text that
-- reads differently on each network, such as a recipient with its amount
accountsHolding :: (MonadUnliftIO m) => (Network -> [Address]) -> ExceptT String (DB m) [(Portfolio, DBAccountId, DBAccount)]
accountsHolding readOn = lift $ filterM holds =<< getAccounts
  where
    holds (_, accId, acc) = not . null <$> storedAmong (readOn (accountNetwork acc)) (accId, acc)

-- | The addresses of the account among the given ones. The account reads
-- them on its network, and compares the addresses themselves, not their
-- text: a Bitcoin Cash address is found written without its prefix, or in
-- the legacy form of Bitcoin, as well as in the form hw stores.
addressesAmong :: (MonadUnliftIO m) => [Text] -> (DBAccountId, DBAccount) -> DB m [DBAddress]
addressesAmong addrs held@(_, acc) = storedAmong (mapMaybe (textToAddr (accountNetwork acc)) addrs) held

-- | The addresses of the account among the given addresses of its network.
-- They are looked for 'inListBatch' at a time: the sync asks about every
-- address of a pool whose balance changed, every one on its first sync,
-- and SQLite takes no more than 32766 of them in one query.
storedAmong :: (MonadUnliftIO m) => [Address] -> (DBAccountId, DBAccount) -> DB m [DBAddress]
storedAmong = storedAmongBy inListBatch

-- | 'storedAmong', looking for the given number of addresses in each
-- query
storedAmongBy :: (MonadUnliftIO m) => Natural -> [Address] -> (DBAccountId, DBAccount) -> DB m [DBAddress]
storedAmongBy batch addrs (accId, acc) = do
  let net = accountNetwork acc
      -- The text hw stores for each address
      stored = mapMaybe (addrToText net) addrs
  fmap concat . forM (chunksOf (fromIntegral (max 1 batch)) stored) $ \some -> do
    rows <- select . from $ \a -> do
      where_ $
        a ^. DBAddressAccount ==. val accId
          &&. a ^. DBAddressAddress `in_` valList some
      return a
    return $ entityVal <$> rows

-- | The receiving addresses of the account that have a label, in the order
-- of the chain: what exportwallet writes
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

-- | Those of the addresses, among the addresses of the accounts, that
-- have transactions on the chain. The accounts are those of one network:
-- the same address on another network is another row, with a history of
-- its own.
usedAddresses :: (MonadUnliftIO m) => [DBAccountId] -> [Text] -> DB m [Text]
usedAddresses accIds addrs = do
  rows <- select . from $ \a -> do
    where_ $
      a ^. DBAddressAccount `in_` valList accIds
        &&. a ^. DBAddressAddress `in_` valList addrs
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
  rowM <- lift $ P.getBy (UniqueAddress accId text)
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

{- The addresses of a pool -}

-- | Every address of a pool, with its path in the pool and its balance:
-- the addresses of each account of the portfolio on the network, of
-- either kind. A summary of a transaction is made against all of them, so
-- that it tells what the transaction means to the portfolio.
poolAddressesMap :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) (Map Address PoolPath, Map Address AddressBalance)
poolAddressesMap pool = do
  accounts <- lift $ poolAccounts pool
  maps <- forM accounts $ \(accId, acc) -> do
    (paths, balances) <- allAddressesMap pool.poolNetwork accId
    return (Map.map (PoolPath (accountKind acc)) paths, balances)
  return (Map.unions (fst <$> maps), Map.unions (snd <$> maps))

-- | The path in the pool of an address of one of its accounts, with that
-- account, or Nothing for an address that is not the pool's
poolAddressPath :: (MonadUnliftIO m) => Pool -> Address -> ExceptT String (DB m) (Maybe (DBAccountId, PoolPath))
poolAddressPath pool addr = do
  accounts <- lift $ poolAccounts pool
  found <- forM accounts $ \(accId, acc) ->
    fmap (\path -> (accId, PoolPath (accountKind acc) path)) <$> addressPath pool.poolNetwork accId addr
  return (listToMaybe (catMaybes found))

-- | The addresses that belong to the pool, with their paths in it
poolPaths :: (MonadUnliftIO m) => Pool -> [Address] -> ExceptT String (DB m) (Map Address PoolPath)
poolPaths pool addrs = Map.fromList . catMaybes <$> mapM pathOf (nub addrs)
  where
    pathOf addr = fmap (\(_, path) -> (addr, path)) <$> poolAddressPath pool addr

-- | The row of an address of the pool, if one of its accounts has it
poolAddressRow :: (MonadUnliftIO m) => Pool -> Address -> DB m (Maybe DBAddress)
poolAddressRow pool addr =
  case addrToText pool.poolNetwork addr of
    Nothing -> return Nothing
    Just addrT -> do
      accounts <- poolAccounts pool
      rows <- forM accounts $ \(accId, _) -> P.getBy (UniqueAddress accId addrT)
      return (entityVal <$> listToMaybe (catMaybes rows))

-- | Whether an address is one of the pool
inPool :: (MonadUnliftIO m) => Pool -> Address -> DB m Bool
inPool pool addr = isJust <$> poolAddressRow pool addr

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

-- | The chain and index of a path the user types, without the type of its
-- account ('readTypedChainPath' reads that): /0/i for the receiving
-- address i, /1/i for the change address i. Every other text is refused,
-- with the two forms: a hardened step, a third step, another chain, or an
-- index of 2^31 or more, which BIP32 keeps for hardened steps. The index is read here and not by the
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
    _ -> Left (notChainPath text)

-- | The refusal of a path typed that is not that of an address, with the
-- two forms
notChainPath :: Text -> String
notChainPath text = "The path " <> cs text <> " is not that of an address: /0/i is the receiving address i, and /1/i the change address i"

-- | Whether a word has the shape of a path that 'readChainPath' reads or
-- refuses: a slash, and nothing but digits, slashes and the m, h and '
-- of a BIP32 path. So 0/3 and m/0/3 are paths, refused with the forms to
-- write, rather than names of files.
isChainPathShaped :: Text -> Bool
isChainPathShaped text = Text.elem '/' text && Text.all (`elem` ("0123456789/mMhH'" :: String)) text

-- | The type of account a path the user types starts with, if any, then
-- the path, as the views write it ('Haskoin.Wallet.Keys.poolPathText'):
-- bip84/0/3, or /0/3 without the type. Nothing for a word that is no path: neither of the
-- shape of 'isChainPathShaped', nor that shape after a word of the form
-- bipN. A path names an address of a portfolio only with its type, as a
-- portfolio has an address at /0/3 in each of its accounts on a network.
-- So what a view shows can be typed back. Without the type, the command
-- picks the account from its options. A type hw holds no account of,
-- such as bip49, is refused with the types it does hold, and so is a path
-- 'readChainPath' refuses, which the refusal names as typed, with its type.
readTypedChainPath :: Text -> Maybe (Either String (Maybe AccountKind, AddrType, KeyIndex))
readTypedChainPath text
  | isChainPathShaped text = Just (untyped <$> readChainPath text)
  | isTypeName typeName && isChainPathShaped rest && Text.isPrefixOf "/" rest = Just $ do
      kind <- maybe (Left unknownType) Right (kindNamed typeName)
      (addrType, index) <- either (const (Left (notChainPath text))) Right (readChainPath rest)
      return (Just kind, addrType, index)
  | otherwise = Nothing
  where
    (typeName, rest) = Text.breakOn "/" text
    untyped (addrType, index) = (Nothing, addrType, index)
    -- bip and digits, in any case, as the types are named
    isTypeName name = case Text.splitAt 3 (Text.toLower name) of
      ("bip", digits) -> not (Text.null digits) && Text.all isDigit digits
      _ -> False
    unknownType =
      "The path "
        <> cs text
        <> " is of no type of account hw holds: a path starts with "
        <> cs (Text.intercalate " or " kindNames)
        <> ", such as bip84/0/3, or with no type, such as /0/3"

-- | The derivation path of a row
addrPath :: DBAddress -> SoftPath
addrPath addr = chainPath (dBAddressInternal addr) (dBAddressIndex addr)

-- | Where the address of a row is in its portfolio: the type of its
-- account and its path below the key of that account ('PoolPath'). A
-- portfolio has up to two accounts on a network, so the path of the row
-- alone does not tell its address. The row does not store the type. The
-- form of the address tells it, since each type of account has addresses
-- of one form ('addressKind'), and hw reads it so rather than look up the
-- account. Nothing for an address that does not read on the network
-- given, which hw never stores.
addrPoolPath :: Network -> DBAddress -> Maybe PoolPath
addrPoolPath net addr = do
  address <- textToAddr net (dBAddressAddress addr)
  kind <- addressKind address
  return (PoolPath kind (addrPath addr))

-- | The address of a row with its derivation path
fromDBAddr :: Network -> DBAddress -> Either String (Address, SoftPath)
fromDBAddr net row = (,addrPath row) <$> textToAddrE net (dBAddressAddress row)

-- | The address the public key of an account derives at an index of one
-- of its chains, whether the wallet holds it or not: what show and
-- signmsg take a path for. They need no row, so they work on a computer
-- that never gave the address out, such as the offline one. An index
-- whose child key is invalid has no address, as BIP32 says, and is
-- refused.
derivedAddressAt :: (MonadIO m) => Ctx -> DBAccount -> AddrType -> KeyIndex -> ExceptT String m Address
derivedAddressAt ctx acc addrType index = do
  addrM <- liftIO $ deriveChainAddress ctx kind (accountXPubKey ctx acc) (addrDeriv addrType) index
  liftMaybe invalidIndex addrM
  where
    kind = accountKind acc
    invalidIndex =
      "There is no address at "
        <> chainPathText (Just kind) addrType index
        <> ": the key at that index is invalid, and the chain skips it"

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
