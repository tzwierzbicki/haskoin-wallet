{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The accounts of the wallet: an account of a network, of either kind
-- ("Haskoin.Wallet.Keys".'AccountKind'), identified by its key, with the
-- fingerprint of its seed once hw has learnt it, its balances and its
-- address counts.
module Haskoin.Wallet.Database.Account where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.Types as Json
import Data.Char (isControl)
import Data.List (sortOn)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E hiding (isNothing)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Descriptor (accountDescriptor, descriptorText, slip132Key)
import Haskoin.Wallet.Keys (AccountKind (..), PublicSeed (..), accountDeriv, checkKindOnNetwork, kindName, kindNamed, kindOfPurpose, kindPurpose, xPubIndex)
import Haskoin.Wallet.Util (visibleText)
import Numeric.Natural (Natural)

{- Accounts in JSON -}

instance ToJSON DBAccount where
  toJSON acc = object (accountPairs acc [])

-- | An account in JSON with its spendable balance among its balances.
-- Nothing stores that balance: hw computes it from the coins wherever it
-- shows the balances of an account ("Haskoin.Wallet.Database.Coin").
accountWithSpendable :: DBAccount -> Word64 -> Json.Value
accountWithSpendable acc spendable = object (accountPairs acc ["spendable" .= spendable])

-- | An account in JSON with its spendable balance, as 'accountWithSpendable'
-- writes it, and with the keys another wallet takes it by: its output
-- descriptor, and its key in the form of SLIP-132, null for a kind that has
-- none ('accountDescriptorText', 'accountSlip132Key'). The answers that show
-- an account with every detail write it so, as accounts and createacc do.
-- The answers of sync leave the keys out, as its screen does.
--
-- The two keys follow from the key, the kind, the path and the fingerprint
-- of the account, which the JSON gives too. So they are written here, on
-- the way out, and the answers of hw keep only the account. A copy kept
-- beside the account could only fall out of step with it.
-- 'parseAccountWithSpendable' reads neither back.
accountWithKeys :: DBAccount -> Word64 -> Json.Value
accountWithKeys acc spendable =
  object
    ( accountPairs acc ["spendable" .= spendable]
        <> [ "descriptor" .= accountDescriptorText acc,
             "slip132" .= accountSlip132Key acc
           ]
    )

-- | An account and its spendable balance, as 'accountWithSpendable' writes them
parseAccountWithSpendable :: Json.Value -> Parser (DBAccount, Word64)
parseAccountWithSpendable value = do
  acc <- parseJSON value
  spendable <- withObject "DBAccount" (\o -> o .: "balance" >>= \bal -> bal .: "spendable") value
  return (acc, spendable)

-- | The keys of an account, with more balances than those it stores when
-- they are given
accountPairs :: DBAccount -> [Pair] -> [Pair]
accountPairs acc moreBalances =
  [ "name" .= dBAccountName acc,
    "index" .= dBAccountIndex acc,
    "type" .= kindName (accountKind acc),
    "network" .= dBAccountNetwork acc,
    "derivation" .= pathToStr (accountPath acc),
    "external" .= dBAccountExternal acc,
    "internal" .= dBAccountInternal acc,
    "xPubKey" .= dBAccountXPubKey acc,
    "fingerprint" .= dBAccountFingerprint acc,
    "balance"
      .= object
        ( [ "confirmed" .= dBAccountBalanceConfirmed acc,
            "unconfirmed" .= dBAccountBalanceUnconfirmed acc,
            "coins" .= dBAccountBalanceCoins acc
          ]
            <> moreBalances
        ),
    "created" .= dBAccountCreated acc
  ]

-- An account written before hw had kinds names none: it is a BIP44 one
instance FromJSON DBAccount where
  parseJSON =
    withObject "DBAccount" $ \o -> do
      bal <- o .: "balance"
      kind <- maybe (return Bip44) kindOf =<< o .:? "type"
      network <- o .: "network"
      -- Read here, where it can fail: 'accountNetwork' takes the name of
      -- a row for a known network
      _ <- maybe (fail ("Unknown network " <> cs (visibleText network))) return (netByName (cs network))
      DBAccount
        <$> o .: "name"
        <*> o .: "index"
        <*> return (fromIntegral (kindPurpose kind))
        <*> return network
        <*> o .: "external"
        <*> o .: "internal"
        <*> o .: "xPubKey"
        <*> o .:? "fingerprint"
        <*> (bal .: "confirmed")
        <*> (bal .: "unconfirmed")
        <*> (bal .: "coins")
        <*> o .: "created"
    where
      kindOf name = maybe (fail ("Unknown account type " <> cs name)) return (kindNamed name)

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

-- | The kind of the account, from its purpose
accountKind :: DBAccount -> AccountKind
accountKind acc =
  fromMaybe (error ("Invalid account purpose in database: " <> show (dBAccountPurpose acc))) $
    kindOfPurpose (fromIntegral (dBAccountPurpose acc))

-- | The path of the account, m/purpose'/coin'/index', from its kind, its
-- network and its index
accountPath :: DBAccount -> HardPath
accountPath acc = accountDeriv (accountKind acc) (accountNetwork acc) (accountIndex acc)

-- | The fingerprint of the seed of the account, when hw has learnt it
accountFingerprint :: DBAccount -> Maybe Fingerprint
accountFingerprint acc = eitherToMaybe . textToFingerprint =<< dBAccountFingerprint acc

-- | The output descriptor of the account, as other wallets and Bitcoin
-- Core take an account without its private key ("Haskoin.Wallet.Descriptor"):
-- its key in the script function of its kind, pkh or wpkh, with the origin
-- of the key when hw knows the fingerprint of the seed, and both chains.
-- A descriptor without its origin still watches the account, but a
-- hardware wallet cannot match it to its seed.
accountDescriptorText :: DBAccount -> Text
accountDescriptorText acc =
  descriptorText (accountDescriptor (accountKind acc) (accountFingerprint acc) (accountPath acc) (dBAccountXPubKey acc))

-- | The key of the account with the prefix of SLIP-132 that marks its
-- kind, a zpub or a vpub for a bip84 account, for the wallets that take
-- only that form. None for a bip44 account, whose xpub such wallets take
-- as it is.
accountSlip132Key :: DBAccount -> Maybe Text
accountSlip132Key acc = slip132Key (accountKind acc) (dBAccountXPubKey acc)

{- Accounts in the database -}

-- | The account that holds the key, if any
accountWithKey :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> DB m (Maybe (DBAccountId, DBAccount))
accountWithKey net ctx key = fmap entityPair <$> P.getBy (UniqueXPubKey $ xPubExport net ctx key)

entityPair :: Entity a -> (P.Key a, a)
entityPair e = (entityKey e, entityVal e)

-- | The next account of a seed on a network, of a kind: the lowest index
-- whose key the database does not hold, with that key. An index whose
-- child key is invalid, which BIP32 says to skip, is passed over as for an
-- address. An account deleted leaves its index free, and creating it again
-- gives the same account. The kinds count apart: the first BIP84 account
-- of a seed is at index 0, whatever BIP44 accounts the seed has.
nextAccount :: (MonadUnliftIO m) => AccountKind -> Network -> Ctx -> PublicSeed -> DB m (Natural, XPubKey)
nextAccount kind net ctx seed = go 0
  where
    go i = do
      keyM <- liftIO $ seedAccountKey seed kind net i
      case keyM of
        Nothing -> go (i + 1)
        Just key -> do
          held <- accountWithKey net ctx key
          maybe (return (i, key)) (const $ go (i + 1)) held

-- | Whether an account of the wallet is of the seed: one derives from it,
-- on its network at its index, along the path of its kind
seedHasAccount :: (MonadUnliftIO m) => Ctx -> PublicSeed -> DB m Bool
seedHasAccount ctx seed = do
  accounts <- getAccounts
  derived <- forM accounts $ \(_, acc) -> liftIO $ seedAccountKey seed (accountKind acc) (accountNetwork acc) (accountIndex acc)
  return $ or [key == Just (accountXPubKey ctx acc) | ((_, acc), key) <- zip accounts derived]

existsAccount :: (MonadUnliftIO m) => Text -> DB m Bool
existsAccount name = P.existsBy $ UniqueName name

{- The name of an account -}

-- | The rules of a name: not empty, no line break or other control
-- character, which could not be typed back, and no space at either end,
-- which a quoting slip on the command line leaves
checkAccountName :: Text -> Either String ()
checkAccountName name
  | Text.null name = Left "The account name must not be empty"
  | Text.any isControl name = Left "The account name must not contain a line break or a control character"
  | Text.strip name /= name = Left "The account name must not start or end with a space"
  | otherwise = Right ()

-- | Refuse a name that breaks the rules, or that an account has already
checkNameFree :: (MonadUnliftIO m) => Text -> ExceptT String (DB m) ()
checkNameFree name = do
  liftEither $ checkAccountName name
  taken <- lift $ existsAccount name
  when taken $ throwError $ "The account " <> cs name <> " already exists"

{- Storing accounts -}

-- | Store an account of a kind on a network under a name, from its public
-- key, with the fingerprint of its seed when known. A kind the network
-- does not have is refused.
insertAccount ::
  (MonadUnliftIO m) =>
  AccountKind ->
  Network ->
  Ctx ->
  Text ->
  XPubKey ->
  Maybe Fingerprint ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
insertAccount kind net ctx name xpub fingerprintM = do
  checkNameFree name
  liftEither $ checkKindOnNetwork net kind
  holderM <- lift $ accountWithKey net ctx xpub
  forM_ holderM $ throwError . keyTaken . snd
  time <- liftIO getCurrentTime
  let account =
        DBAccount
          { dBAccountName = name,
            dBAccountIndex = fromIntegral $ xPubIndex xpub,
            dBAccountPurpose = fromIntegral (kindPurpose kind),
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

-- | Delete an account with its addresses, summaries, labels of
-- transactions, coins, pending transactions and the block its last sync
-- reached
deleteAccount :: (MonadUnliftIO m) => DBAccountId -> DB m ()
deleteAccount accId = do
  delete . from $ \a -> where_ $ a ^. DBAddressAccount ==. val accId
  delete . from $ \t -> where_ $ t ^. DBTxInfoAccount ==. val accId
  delete . from $ \l -> where_ $ l ^. DBTxLabelAccount ==. val accId
  delete . from $ \c -> where_ $ c ^. DBCoinAccount ==. val accId
  delete . from $ \p -> where_ $ p ^. DBPendingTxAccount ==. val accId
  delete . from $ \b -> where_ $ b ^. DBSyncedBlockAccount ==. val accId
  P.delete accId

-- | The account of a name, which must exist. Without a name, the one
-- account of the wallet, when it has exactly one.
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

-- | The account a command acts on, among the accounts its data points to:
-- the coins it spends or the transaction it reads, for instance. The
-- account --account names must be one of them, or the command stops with
-- the refusal of that name. Without --account, the account is the only one
-- the data points to, and the user is asked for --account when it points
-- to several. Data that points to no account leaves the choice to
-- --account, or to the only account of the wallet.
accountAmong ::
  (MonadUnliftIO m) =>
  (Text -> String) ->
  Maybe Text ->
  [(DBAccountId, DBAccount)] ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
accountAmong refused (Just name) candidates = do
  named <- getAccountByName (Just name)
  unless (null candidates || fst named `elem` (fst <$> candidates)) $
    throwError (refused name)
  return named
accountAmong _ Nothing candidates =
  case candidates of
    [] -> getAccountByName Nothing
    [one] -> return one
    _ -> throwError "Specify which account to use"

-- | The accounts of the ids, each with its id
getAccountsById :: (MonadUnliftIO m) => [DBAccountId] -> ExceptT String (DB m) [(DBAccountId, DBAccount)]
getAccountsById = mapM (\accId -> (,) accId <$> getAccountById accId)

-- | The account of the name, or every account without one
getAccountsByName ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  ExceptT String (DB m) [(DBAccountId, DBAccount)]
getAccountsByName (Just name) = (: []) <$> getAccountByName (Just name)
getAccountsByName Nothing = lift getAccounts

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

-- | The names of the accounts, in the order of their characters
getAccountNames :: (MonadUnliftIO m) => DB m [Text]
getAccountNames = map (dBAccountName . entityVal) <$> P.selectList [] [P.Asc DBAccountName]

-- | Rename an account, to a name that follows the rules and that no account
-- has. Renaming it to its own name changes nothing.
renameAccount ::
  (MonadUnliftIO m) => Text -> Text -> ExceptT String (DB m) DBAccount
renameAccount oldName newName
  | oldName == newName = snd <$> getAccountByName (Just oldName)
  | otherwise = do
      checkNameFree newName
      renamed <-
        lift . updateCount $ \a -> do
          set a [DBAccountName =. val newName]
          where_ $ a ^. DBAccountName ==. val oldName
      when (renamed == 0) $ throwError $ "The account " <> cs oldName <> " does not exist"
      snd <$> getAccountByName (Just newName)

-- | What the account holds, confirmed or not: the balance after its
-- newest transaction. A spend lowers the side of the balance its coin was
-- on, so the two add up to the coins that are left.
accountTotal :: DBAccount -> Integer
accountTotal acc = toInteger (dBAccountBalanceConfirmed acc) + toInteger (dBAccountBalanceUnconfirmed acc)

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
-- database, those whose key the seed derives, along the path of their
-- kind. A command that holds the mnemonic does this, so that every account
-- of the seed, on every network, shows the fingerprint.
recordFingerprint :: (MonadUnliftIO m) => Ctx -> PublicSeed -> DB m ()
recordFingerprint ctx seed = do
  accs <- getAccounts
  forM_ accs $ \(accId, acc) -> do
    let net = accountNetwork acc
    derivedM <- liftIO $ seedAccountKey seed (accountKind acc) net (accountIndex acc)
    when (fmap (xPubExport net ctx) derivedM == Just (dBAccountXPubKey acc) && dBAccountFingerprint acc /= Just fingerprint) $
      P.update accId [DBAccountFingerprint P.=. Just fingerprint]
  where
    fingerprint = fingerprintToText $ seedFingerprint seed

-- | Record the fingerprint of the seed of an account as a file of accounts
-- reports it, an exported document or the origin of a BIP 329 record. One
-- already recorded must be the same: the record is refused otherwise, and
-- the import goes on.
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

-- | Record the fingerprint of the seed of an account as a signed
-- transaction names it, when the account knows none. A fingerprint cannot
-- be checked from the public key of the account, so the word of the file
-- is all there is: it never replaces the one the account has, and a file
-- naming another is not refused for it. A counterparty holding a file the
-- user signed could otherwise plant a fingerprint against which the
-- genuine file is then refused. The seed itself sets it for sure
-- ('recordFingerprint'), when signtx or signmsg signs with it.
claimFingerprint :: (MonadUnliftIO m) => DBAccountId -> Fingerprint -> ExceptT String (DB m) ()
claimFingerprint accId fingerprint = do
  acc <- getAccountById accId
  when (isNothing (dBAccountFingerprint acc)) $
    lift $ P.update accId [DBAccountFingerprint P.=. Just (fingerprintToText fingerprint)]

{- The export document -}

-- | An account as exportacc wrote it before hw took the format of BIP 329
-- ("Haskoin.Wallet.Labels"), which importacc still reads: its public key,
-- its network, its name and the fingerprint of its seed when known. The
-- documents of earlier versions of hw carry a wallet id instead, which is
-- ignored.
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
