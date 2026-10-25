{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The accounts of the wallet: a BIP44 account of a network, identified by
-- its key, with the fingerprint of its seed once hw has learnt it, its
-- balances and its address counts.
module Haskoin.Wallet.Account where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.List (sortOn)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E hiding (isNothing)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Keys (PublicSeed (..), bip44Deriv, xPubIndex)
import Numeric.Natural (Natural)

{- Accounts in JSON -}

instance ToJSON DBAccount where
  toJSON acc =
    object
      [ "name" .= dBAccountName acc,
        "index" .= dBAccountIndex acc,
        "network" .= dBAccountNetwork acc,
        "derivation" .= pathToStr (accountPath acc),
        "external" .= dBAccountExternal acc,
        "internal" .= dBAccountInternal acc,
        "xPubKey" .= dBAccountXPubKey acc,
        "fingerprint" .= dBAccountFingerprint acc,
        "balance" .= toJSON (AccountBalance confirm unconfirm coins),
        "created" .= dBAccountCreated acc
      ]
    where
      confirm = dBAccountBalanceConfirmed acc
      unconfirm = dBAccountBalanceUnconfirmed acc
      coins = dBAccountBalanceCoins acc

instance FromJSON DBAccount where
  parseJSON =
    withObject "DBAccount" $ \o -> do
      bal <- o .: "balance"
      DBAccount
        <$> o .: "name"
        <*> o .: "index"
        <*> o .: "network"
        <*> o .: "external"
        <*> o .: "internal"
        <*> o .: "xPubKey"
        <*> o .:? "fingerprint"
        <*> (bal .: "confirmed")
        <*> (bal .: "unconfirmed")
        <*> (bal .: "coins")
        <*> o .: "created"

data AccountBalance = AccountBalance
  { -- | confirmed balance
    accBalanceConfirmed :: !Word64,
    -- | unconfirmed balance
    accBalanceUnconfirmed :: !Word64,
    -- | number of unspent outputs
    accBalanceCoins :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AccountBalance where
  toJSON b =
    object
      [ "confirmed" .= accBalanceConfirmed b,
        "unconfirmed" .= accBalanceUnconfirmed b,
        "coins" .= accBalanceCoins b
      ]

instance FromJSON AccountBalance where
  parseJSON =
    withObject "accountbalance" $ \o ->
      AccountBalance
        <$> o .: "confirmed"
        <*> o .: "unconfirmed"
        <*> o .: "coins"

{- The fields of an account -}

accountIndex :: DBAccount -> Natural
accountIndex = fromIntegral . dBAccountIndex

accountNetwork :: DBAccount -> Network
accountNetwork =
  fromMaybe (error "Invalid Network in database")
    . netByName
    . cs
    . dBAccountNetwork

accountXPubKey :: Ctx -> DBAccount -> XPubKey
accountXPubKey ctx acc =
  fromMaybe (error "Invalid XPubKey in database") $
    xPubImport (accountNetwork acc) ctx (dBAccountXPubKey acc)

-- | The BIP44 path of the account, m/44'/coin'/index', from its network
-- and index
accountPath :: DBAccount -> HardPath
accountPath acc = bip44Deriv (accountNetwork acc) (accountIndex acc)

-- | The fingerprint of the seed of the account, when hw has learnt it
accountFingerprint :: DBAccount -> Maybe Fingerprint
accountFingerprint acc = eitherToMaybe . textToFingerprint =<< dBAccountFingerprint acc

{- Accounts in the database -}

-- | The account that holds the key, if any
accountWithKey :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> DB m (Maybe (DBAccountId, DBAccount))
accountWithKey net ctx key = fmap entityPair <$> P.getBy (UniqueXPubKey $ xPubExport net ctx key)

entityPair :: Entity a -> (P.Key a, a)
entityPair e = (entityKey e, entityVal e)

-- | The index of the next account of a seed on a network: the smallest one
-- whose key the database does not hold. An account deleted leaves its
-- index free, and creating it again gives the same account.
nextAccountIndex :: (MonadUnliftIO m) => Network -> Ctx -> PublicSeed -> DB m Natural
nextAccountIndex net ctx seed = go 0
  where
    go i = do
      held <- accountWithKey net ctx $ seedAccountKey seed net i
      maybe (return i) (const $ go (i + 1)) held

existsAccount :: (MonadUnliftIO m) => Text -> DB m Bool
existsAccount name = P.existsBy $ UniqueName name

-- | Store an account of a network under a name, from its public key, with
-- the fingerprint of its seed when known
insertAccount ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Text ->
  XPubKey ->
  Maybe Fingerprint ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
insertAccount net ctx name xpub fingerprintM = do
  existsName <- lift $ existsAccount name
  when existsName $ throwError $ "Account " <> cs name <> " already exists"
  holderM <- lift $ accountWithKey net ctx xpub
  forM_ holderM $ throwError . keyTaken . snd
  time <- liftIO getCurrentTime
  let account =
        DBAccount
          { dBAccountName = name,
            dBAccountIndex = fromIntegral $ xPubIndex xpub,
            dBAccountNetwork = cs net.name,
            dBAccountExternal = 0,
            dBAccountInternal = 0,
            dBAccountXPubKey = xPubExport net ctx xpub,
            dBAccountFingerprint = fingerprintToText <$> fingerprintM,
            dBAccountBalanceConfirmed = 0,
            dBAccountBalanceUnconfirmed = 0,
            dBAccountBalanceCoins = 0,
            dBAccountCreated = time
          }
  key <- lift $ P.insert account
  return (key, account)
  where
    -- The networks of Bitcoin and Bitcoin Cash have their own coin types;
    -- the test networks all use coin type 1 and share their account keys
    keyTaken holder
      | dBAccountNetwork holder == cs net.name =
          "Account " <> cs (dBAccountName holder) <> " already has this key"
      | otherwise =
          "Account " <> cs (dBAccountName holder) <> " on " <> cs (dBAccountNetwork holder)
            <> " already has this key: the test networks share their account keys, so a wallet holds one of them per account index"

-- | Delete an account with its addresses, summaries, coins and pending
-- transactions
deleteAccount :: (MonadUnliftIO m) => DBAccountId -> DB m ()
deleteAccount accId = do
  delete . from $ \a -> where_ $ a ^. DBAddressAccount ==. val accId
  delete . from $ \t -> where_ $ t ^. DBTxInfoAccount ==. val accId
  delete . from $ \c -> where_ $ c ^. DBCoinAccount ==. val accId
  delete . from $ \p -> where_ $ p ^. DBPendingTxAccount ==. val accId
  P.delete accId

-- When a name is provided, get that account or throw an error if it doesn't
-- exist. When no name is provided, return the account only if there is one
-- account.
getAccountByName ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
getAccountByName (Just name) = do
  aM <- lift $ P.getBy $ UniqueName name
  case aM of
    Just a -> return (entityKey a, entityVal a)
    _ -> throwError $ "The account " <> cs name <> " does not exist"
getAccountByName Nothing = do
  as <- lift getAccounts
  case as of
    [a] -> return a
    [] -> throwError "There are no accounts in the wallet"
    _ -> throwError "Specify which account to use"

getAccountById ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) DBAccount
getAccountById accId = liftMaybe "Invalid account" =<< lift (P.get accId)

-- | Every account, those of one seed together: by the fingerprint of the
-- seed, the accounts whose seed is not known yet last, then by network and
-- index
getAccounts :: (MonadUnliftIO m) => DB m [(DBAccountId, DBAccount)]
getAccounts = sortOn (order . snd) . map entityPair <$> P.selectList [] []
  where
    order acc = (isNothing (dBAccountFingerprint acc), dBAccountFingerprint acc, dBAccountNetwork acc, dBAccountIndex acc)

-- | Rename an account. Renaming it to its own name changes nothing.
renameAccount ::
  (MonadUnliftIO m) => Text -> Text -> ExceptT String (DB m) DBAccount
renameAccount oldName newName
  | oldName == newName = snd <$> getAccountByName (Just oldName)
  | otherwise = do
      e <- lift $ existsAccount newName
      if e
        then throwError $ "The account " <> cs newName <> " already exists"
        else do
          c <-
            lift . updateCount $ \a -> do
              set a [DBAccountName =. val newName]
              where_ $ a ^. DBAccountName ==. val oldName
          if c == 0
            then throwError $ "The account " <> cs oldName <> " does not exist"
            else snd <$> getAccountByName (Just newName)

-- | Add up the balances of the addresses of an account into the account
updateAccountBalances :: (MonadUnliftIO m) => DBAccountId -> DB m DBAccount
updateAccountBalances accId = do
  confirm <- selectSum DBAddressBalanceConfirmed
  unconfirm <- selectSum DBAddressBalanceUnconfirmed
  coins <- selectSum DBAddressBalanceCoins
  update $ \a -> do
    set
      a
      [ DBAccountBalanceConfirmed =. val (total confirm),
        DBAccountBalanceUnconfirmed =. val (total unconfirm),
        DBAccountBalanceCoins =. val (total coins)
      ]
    where_ $ a ^. DBAccountId ==. val accId
  fromJust <$> P.get accId
  where
    -- Zero when the account has no address yet
    total = joinMaybe 0 id
    selectSum field =
      selectOne . from $ \a -> do
        where_ $ a ^. DBAddressAccount ==. val accId
        return $ sum_ $ a ^. field

{- The fingerprint of the seed -}

-- | Record the fingerprint of a seed on each of its accounts in the
-- database, those whose key the seed derives. A command that holds the
-- mnemonic does this, so that every account of the seed, on every network,
-- shows the fingerprint.
recordFingerprint :: (MonadUnliftIO m) => Ctx -> PublicSeed -> DB m ()
recordFingerprint ctx seed = do
  accs <- getAccounts
  forM_ accs $ \(accId, acc) -> do
    let net = accountNetwork acc
        derived = xPubExport net ctx $ seedAccountKey seed net (accountIndex acc)
    when (derived == dBAccountXPubKey acc && dBAccountFingerprint acc /= Just fingerprint) $
      P.update accId [DBAccountFingerprint P.=. Just fingerprint]
  where
    fingerprint = fingerprintToText $ seedFingerprint seed

-- | Record the fingerprint of the seed of an account as the offline wallet
-- reports it, in an exported document or with a signed transaction. One
-- already recorded must be the same.
learnFingerprint :: (MonadUnliftIO m) => DBAccountId -> Fingerprint -> ExceptT String (DB m) ()
learnFingerprint accId fingerprint = do
  acc <- getAccountById accId
  case dBAccountFingerprint acc of
    Nothing -> lift $ P.update accId [DBAccountFingerprint P.=. Just reported]
    Just known
      | known == reported -> return ()
      | otherwise ->
          throwError $
            "The account " <> cs (dBAccountName acc) <> " is of the seed " <> cs known <> ", not of " <> cs reported
  where
    reported = fingerprintToText fingerprint

{- The export document -}

-- | An account as exportacc writes it and importacc reads it: its public
-- key, its network, its name and the fingerprint of its seed when known.
-- The documents of earlier versions of hw carry a wallet id instead, which
-- is ignored.
data PubKeyDoc = PubKeyDoc
  { documentPubKey :: !XPubKey,
    documentNetwork :: !Network,
    documentName :: !Text,
    documentFingerprint :: !(Maybe Fingerprint)
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx PubKeyDoc where
  unmarshalValue ctx =
    withObject "pubkeydocument" $ \o -> do
      netName <- o .: "network"
      net <- maybe (fail $ "Unknown network " <> netName) return $ netByName netName
      PubKeyDoc
        <$> (unmarshalValue (net, ctx) =<< o .: "xpubkey")
        <*> return net
        <*> o .: "name"
        <*> o .:? "fingerprint"

  marshalValue ctx (PubKeyDoc k net name fingerprint) =
    object
      [ "xpubkey" .= marshalValue (net, ctx) k,
        "network" .= net.name,
        "name" .= name,
        "fingerprint" .= fingerprint
      ]
