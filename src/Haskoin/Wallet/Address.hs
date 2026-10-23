{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The addresses of an account: external ones to receive with and internal
-- ones for change, generated along the two chains of the account within
-- the gap of BIP44, with their labels and balances. An internal address is
-- available for change when no transaction has used it and no pending
-- transaction pays it; the pending transactions are read in
-- "Haskoin.Wallet.PendingTx", which asks for the change address with them.
module Haskoin.Wallet.Address where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Util (Page (Page), addrToTextE, textToAddrE)
import Numeric.Natural (Natural)

{- The two chains -}

-- | The chains of an account: external addresses to receive with, internal
-- ones for the change of its transactions
data AddrType = AddrInternal | AddrExternal
  deriving (Eq, Show)

isInternal :: AddrType -> Bool
isInternal AddrInternal = True
isInternal AddrExternal = False

-- | How many addresses of the chain the account has generated
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
  toJSON addr =
    object
      [ "index" .= dBAddressIndex addr,
        "wallet" .= dBAddressAccountWallet addr,
        "account" .= dBAddressAccountDerivation addr,
        "derivation" .= dBAddressDerivation addr,
        "address" .= dBAddressAddress addr,
        "label" .= dBAddressLabel addr,
        "balance"
          .= AddressBalance
            (dBAddressBalanceConfirmed addr)
            (dBAddressBalanceUnconfirmed addr)
            (dBAddressBalanceCoins addr)
            (dBAddressBalanceTxs addr)
            (dBAddressBalanceReceived addr),
        "internal" .= dBAddressInternal addr,
        "created" .= dBAddressCreated addr
      ]

instance FromJSON DBAddress where
  parseJSON =
    withObject "DBAddress" $ \o -> do
      bal <- o .: "balance"
      DBAddress
        <$> o .: "index"
        <*> o .: "wallet"
        <*> o .: "account"
        <*> o .: "derivation"
        <*> o .: "address"
        <*> o .: "label"
        <*> bal .: "confirmed"
        <*> bal .: "unconfirmed"
        <*> bal .: "coins"
        <*> bal .: "txs"
        <*> bal .: "received"
        <*> o .: "internal"
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
  toJSON b =
    object
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
nextChangeAddress ctx cfg accId@(DBAccountKey wallet accDeriv) reserved = do
  found <- lift . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val True
        &&. a ^. DBAddressBalanceTxs ==. val 0
        &&. a ^. DBAddressDerivation `notIn` valList (pathText <$> reserved)
    orderBy [asc $ a ^. DBAddressIndex]
    limit 1
    return a
  maybe (genNextAddress ctx cfg accId AddrInternal) (return . entityVal) found

-- | The next address of a chain, within the gap
genNextAddress :: (MonadUnliftIO m) => Ctx -> Config -> DBAccountId -> AddrType -> ExceptT String (DB m) DBAddress
genNextAddress ctx cfg accId addrType = do
  checkGap cfg accId addrType
  appendAddress ctx accId addrType

-- | Generate the addresses of a chain up to the given count, as a scan of
-- the store found them used: the gap does not apply to used addresses
extendChainTo :: (MonadUnliftIO m) => Ctx -> DBAccountId -> AddrType -> Natural -> ExceptT String (DB m) ()
extendChainTo ctx accId addrType total = do
  acc <- getAccountById accId
  replicateM_ (fromIntegral total - dBAccountCount addrType acc) $ appendAddress ctx accId addrType

-- | Add the next address of a chain to the account, whatever the gap
appendAddress :: (MonadUnliftIO m) => Ctx -> DBAccountId -> AddrType -> ExceptT String (DB m) DBAddress
appendAddress ctx accId addrType = do
  acc <- getAccountById accId
  let next = dBAccountCount addrType acc
      (addr, _) = derivePathAddr ctx (accountXPubKey ctx acc) (addrDeriv addrType) (fromIntegral next)
  row <- insertAddress (accountNetwork acc) accId addrType next addr
  lift $ P.update accId [dBAccountField addrType P.=. next + 1]
  return row

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
      AddrExternal -> "The last " <> show gap <> " receiving addresses await a payment: reuse one of them, listed by addrs"
      AddrInternal -> "The last " <> show gap <> " change addresses are held by pending transactions: send or delete some of them"

-- | The highest index of the chain with transactions, if any
highestUsedIndex :: (MonadUnliftIO m) => DBAccountId -> AddrType -> DB m (Maybe Int)
highestUsedIndex (DBAccountKey wallet accDeriv) addrType = do
  resM <- (flatMaybe <$>) . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val (isInternal addrType)
        &&. a ^. DBAddressBalanceTxs >. val 0
    return $ max_ $ a ^. DBAddressIndex
  return $ fromIntegral <$> resM

-- | Store an address of a chain, at its index, without a label or balances
insertAddress :: (MonadUnliftIO m) => Network -> DBAccountId -> AddrType -> Int -> Address -> ExceptT String (DB m) DBAddress
insertAddress net (DBAccountKey wallet accDeriv) addrType index addr = do
  time <- liftIO getCurrentTime
  addrT <- liftEither $ addrToTextE net addr
  let path = pathText $ addrDeriv addrType :/ fromIntegral index
      row = DBAddress index wallet accDeriv path addrT "" 0 0 0 0 0 (isInternal addrType) time
  lift $ P.insert_ row
  return row

-- | Label a receiving address of the account, by its index
setAddrLabel :: (MonadUnliftIO m) => DBAccountId -> Int -> Text -> ExceptT String (DB m) DBAddress
setAddrLabel accId@(DBAccountKey wallet accDeriv) idx label = do
  acc <- getAccountById accId
  unless (idx < dBAccountExternal acc) $
    throwError $ "Address " <> show idx <> " does not exist"
  lift $ P.updateGet (DBAddressKey wallet accDeriv (pathText $ extDeriv :/ fromIntegral idx)) [DBAddressLabel P.=. label]

{- Addresses in the database -}

-- | Store the balances the store reports
updateAddressBalances :: (MonadUnliftIO m) => Network -> [Store.Balance] -> ExceptT String (DB m) ()
updateAddressBalances net storeBals =
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
      where_ $ a ^. DBAddressAddress ==. val addrT

-- | An address of the wallet, by its text, with its account
getAddressByText :: (MonadUnliftIO m) => Text -> ExceptT String (DB m) (DBAccount, DBAddress)
getAddressByText addrTxt = do
  Entity _ addr <- lift (P.getBy $ UniqueAddress addrTxt) >>= liftMaybe "Address does not exist"
  acc <- getAccountById $ DBAccountKey (dBAddressAccountWallet addr) (dBAddressAccountDerivation addr)
  return (acc, addr)

-- | Those of the addresses that have transactions on the chain
usedAddresses :: (MonadUnliftIO m) => [Text] -> DB m [Text]
usedAddresses addrs = do
  rows <- select . from $ \a -> do
    where_ $
      a ^. DBAddressAddress `in_` valList addrs
        &&. a ^. DBAddressBalanceTxs >. val 0
    return $ a ^. DBAddressAddress
  return $ unValue <$> rows

-- | A page of the addresses of a chain of the account, newest first
addressPage :: (MonadUnliftIO m) => DBAccountId -> AddrType -> Page -> DB m [DBAddress]
addressPage (DBAccountKey wallet accDeriv) addrType (Page lim off) = do
  rows <- select . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val (isInternal addrType)
    orderBy [desc (a ^. DBAddressIndex)]
    limit $ fromIntegral lim
    offset $ fromIntegral off
    return a
  return $ entityVal <$> rows

-- | Every address of the account, with its derivation path and its balance
allAddressesMap :: (MonadUnliftIO m) => Network -> DBAccountId -> ExceptT String (DB m) (Map Address SoftPath, Map Address AddressBalance)
allAddressesMap net (DBAccountKey wallet accDeriv) = do
  rows <- lift . select . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
    return a
  res <- forM rows $ \(Entity _ row) -> do
    (addr, path) <- liftEither $ fromDBAddr net row
    let bal = AddressBalance (dBAddressBalanceConfirmed row) (dBAddressBalanceUnconfirmed row) (dBAddressBalanceCoins row) (dBAddressBalanceTxs row) (dBAddressBalanceReceived row)
    return ((addr, path), (addr, bal))
  return (Map.fromList $ fst <$> res, Map.fromList $ snd <$> res)

-- | The derivation path of an address of the account, or Nothing for an
-- address that is not the account's
addressPath :: (MonadUnliftIO m) => Network -> DBAccountId -> Address -> ExceptT String (DB m) (Maybe SoftPath)
addressPath net (DBAccountKey wallet accDeriv) addr = do
  addrT <- liftEither $ addrToTextE net addr
  derivM <- lift . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAddress ==. val addrT
        &&. a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
    return $ a ^. DBAddressDerivation
  forM derivM $ liftEither . pathFromText . unValue

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

-- | A derivation path as the rows store it
pathText :: SoftPath -> Text
pathText = cs . pathToStr

-- | A derivation path as a row stores it
pathFromText :: Text -> Either String SoftPath
pathFromText = maybeToEither "The stored derivation path does not parse" . parseSoft . cs

-- | The derivation path of a row, from its chain and index
addrPath :: DBAddress -> SoftPath
addrPath addr = (if dBAddressInternal addr then intDeriv else extDeriv) :/ fromIntegral (dBAddressIndex addr)

-- | The address of a row with its derivation path
fromDBAddr :: Network -> DBAddress -> Either String (Address, SoftPath)
fromDBAddr net row = (,) <$> textToAddrE net (dBAddressAddress row) <*> pathFromText (dBAddressDerivation row)

-- | A page of the addresses of a chain of the public key, with their indices
chainAddresses :: Ctx -> XPubKey -> SoftPath -> Page -> [(Address, KeyIndex)]
chainAddresses ctx pub chain (Page lim off) =
  [(addr, i) | (addr, _, i) <- take (fromIntegral lim) (derivePathAddrs ctx pub chain (fromIntegral off))]
