{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The accounts of the portfolios. An account is one derivation path on
-- one network, m/purpose'/coin'/index': it has its portfolio, whose index
-- is the last step of the path, its network, and its kind
-- ("Haskoin.Wallet.Keys".'AccountKind'), with its key, its balances and
-- its address counts. The user never creates or names an account. A whole
-- portfolio has every account of 'portfolioAccounts', made together from
-- the seed ('createPortfolio').
--
-- A command reaches an account through the portfolio it works in, then
-- the network and the type its options or the configuration give
-- ('chooseAccount'). A command that spends or lists transactions works on
-- a pool, the accounts of the portfolio on one network together
-- ('choosePool').
--
-- A portfolio is partial when it lacks some of its accounts. The step from
-- the database of hw 0.9 makes such portfolios, since a public account
-- key cannot derive the other accounts of its portfolio: the paths
-- between them are hardened. The seed fills them in ('fillWallet'), and so
-- does a file of the wallet. Until then a partial portfolio works with
-- the accounts it has, and a command that needs another one says how to
-- fill it in ('missingAccount').
--
-- The same seed must be one wallet. hw 0.9 could not tell that its
-- accounts on Bitcoin and on Bitcoin Cash were of one seed, so the step
-- from its database leaves them in two wallets. A command that holds the
-- seed finds every wallet whose keys the seed derives and merges them
-- into one ('adoptSeed').
--
-- A key is held by one account of a network at most. It is compared as a
-- key, its public key and chain code, and not as the text of an extended
-- key, which also names a child number that a file can change
-- ('accountWithKey'). The test networks share their keys, so the accounts
-- of a portfolio at one path hold one key, on each of those networks
-- ('checkSharedPath'). A portfolio is dated by its oldest account
-- ('insertAccount', 'mergePortfolios').
module Haskoin.Wallet.Database.Account where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (intercalate, nub, sortOn)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E hiding (isNothing)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Descriptor (accountDescriptor, descriptorText, slip132Key)
import Haskoin.Wallet.Keys
import Numeric.Natural (Natural)

{- The fields of an account -}

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

-- | What the portfolio of the account holds on its network, which the
-- account is a part of
accountPool :: DBAccount -> Pool
accountPool acc = Pool (dBAccountPortfolio acc) (accountNetwork acc)

-- | The path of an account of the portfolio, m/purpose'/coin'/index', from
-- its kind, its network and the index of the portfolio
accountPath :: Portfolio -> DBAccount -> HardPath
accountPath portfolio acc = accountDeriv (accountKind acc) (accountNetwork acc) (portfolioIndex portfolio.portfolioRow)

-- | How the answers name an account of the portfolio
accountRef :: Portfolio -> DBAccount -> AccountRef
accountRef portfolio acc = AccountRef (portfolioPlace portfolio (accountNetwork acc)) (accountKind acc)

-- | The output descriptor of an account of the portfolio, as other wallets
-- and Bitcoin Core take an account without its private key
-- ("Haskoin.Wallet.Descriptor"): its key in the script function of its
-- kind, pkh or wpkh, with the origin of the key when hw knows the
-- fingerprint of the seed, which the wallet of the portfolio keeps, and
-- both chains. A descriptor without its origin still watches the account,
-- but a hardware wallet cannot match it to its seed.
accountDescriptorText :: Portfolio -> DBAccount -> Text
accountDescriptorText portfolio acc =
  descriptorText (accountDescriptor (accountKind acc) (portfolioFingerprint portfolio) (accountPath portfolio acc) (dBAccountXPubKey acc))

-- | The key of the account with the prefix of SLIP-132 that marks its
-- kind, for the wallets that take only that form: a zpub or a vpub for a
-- bip84 account, and the xpub or the tpub of a bip44 account, which is
-- its form in SLIP-132 too. A key hw stores always has one: Nothing would
-- mean the row holds no extended key of BIP32.
accountSlip132Key :: DBAccount -> Maybe Text
accountSlip132Key acc = slip132Key (accountKind acc) (dBAccountXPubKey acc)

-- | What the account holds, confirmed or not: the balance after its
-- newest transaction. A spend lowers the side of the balance its coin was
-- on, so the two add up to the coins that are left.
accountTotal :: DBAccount -> Integer
accountTotal acc = toInteger (dBAccountBalanceConfirmed acc) + toInteger (dBAccountBalanceUnconfirmed acc)

{- Accounts in the database -}

getAccountById ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) DBAccount
getAccountById accId = liftMaybe "Invalid account" =<< lift (P.get accId)

-- | The accounts of the ids, each with its id
getAccountsById :: (MonadUnliftIO m) => [DBAccountId] -> ExceptT String (DB m) [(DBAccountId, DBAccount)]
getAccountsById = mapM (\accId -> (,) accId <$> getAccountById accId)

-- | The account that holds the key on the network, if any: the same
-- public key and chain code ('sameKey'), whatever depth, parent and child
-- number the key is written with. Its text would not do: a file that
-- writes the key of portfolio 0 with the child number 5' would give
-- portfolio 5 the addresses of portfolio 0. The accounts of the network
-- are compared one by one, as their keys are stored as text. A database
-- holds a few accounts on each network, one or two per portfolio.
--
-- The test networks share their keys, so a key is looked up on its
-- network: the same key on btctest and on btctest4 is right, for the
-- same account of a portfolio on the two.
accountWithKey :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> DB m (Maybe (DBAccountId, DBAccount))
accountWithKey net ctx key = do
  accounts <- P.selectList [DBAccountNetwork P.==. cs net.name] [P.Asc DBAccountId]
  return $ listToMaybe [entityPair e | e <- accounts, sameKey key (accountXPubKey ctx (P.entityVal e))]

-- | The accounts of a portfolio, in the order of the networks of a
-- portfolio and, on each, of the kinds, BIP44 first
portfolioAccountRows :: (MonadUnliftIO m) => DBPortfolioId -> DB m [(DBAccountId, DBAccount)]
portfolioAccountRows portfolioKey =
  sortOn (accountOrder . snd) . map entityPair <$> P.selectList [DBAccountPortfolio P.==. portfolioKey] []

-- | The place of an account among those of its portfolio: by network, then
-- by kind
accountOrder :: DBAccount -> (Int, Int)
accountOrder acc = (networkRank (accountNetwork acc), dBAccountPurpose acc)

-- | The accounts of a pool: those of its portfolio on its network, one of
-- each kind the portfolio has there, BIP44 first
poolAccounts :: (MonadUnliftIO m) => Pool -> DB m [(DBAccountId, DBAccount)]
poolAccounts pool =
  map entityPair
    <$> P.selectList
      [DBAccountPortfolio P.==. pool.poolPortfolio, DBAccountNetwork P.==. poolNetworkName pool]
      [P.Asc DBAccountPurpose]

-- | The account of a kind of a pool, if the portfolio has it
poolAccount :: (MonadUnliftIO m) => Pool -> AccountKind -> DB m (Maybe (DBAccountId, DBAccount))
poolAccount pool kind =
  fmap entityPair <$> P.getBy (UniqueAccount pool.poolPortfolio (poolNetworkName pool) (fromIntegral (kindPurpose kind)))

-- | The account of a pool that has an address of the kind of the path
poolAccountOf :: (MonadUnliftIO m) => Pool -> AccountKind -> ExceptT String (DB m) (DBAccountId, DBAccount)
poolAccountOf pool kind =
  liftMaybe ("The portfolio has no " <> cs (kindName kind) <> " account on " <> pool.poolNetwork.name) =<< lift (poolAccount pool kind)

-- | The pools of a portfolio: one for each network it has an account on,
-- in the order of the networks of a portfolio. A whole portfolio has all
-- eight, and a partial one those of the accounts it has.
portfolioPools :: (MonadUnliftIO m) => DBPortfolioId -> DB m [Pool]
portfolioPools portfolioKey = do
  accounts <- portfolioAccountRows portfolioKey
  return [Pool portfolioKey net | net <- nub (accountNetwork . snd <$> accounts)]

-- | Every account of the database on a network, by its key: the coins of
-- a network are those of its accounts
accountsOnNetwork :: (MonadUnliftIO m) => Network -> DB m [DBAccountId]
accountsOnNetwork net = P.selectKeysList [DBAccountNetwork P.==. cs net.name] []

-- | Every pool of the database on a network, each once
poolsOnNetwork :: (MonadUnliftIO m) => Network -> DB m [Pool]
poolsOnNetwork net = do
  accounts <- P.selectList [DBAccountNetwork P.==. cs net.name] []
  return [Pool key net | key <- nub (dBAccountPortfolio . P.entityVal <$> accounts)]

-- | Every account of the database, with its portfolio, in the order of
-- the wallets, of the portfolios of each, and of the accounts of each
getAccounts :: (MonadUnliftIO m) => DB m [(Portfolio, DBAccountId, DBAccount)]
getAccounts = do
  portfolios <- getAllPortfolios
  fmap concat . forM portfolios $ \portfolio ->
    map (\(accId, acc) -> (portfolio, accId, acc)) <$> portfolioAccountRows portfolio.portfolioId

-- | The portfolio of an account, with its wallet
portfolioOfAccount :: (MonadUnliftIO m) => DBAccount -> ExceptT String (DB m) Portfolio
portfolioOfAccount = getPortfolio . dBAccountPortfolio

-- | How the answers name the account of a row
accountRefOf :: (MonadUnliftIO m) => DBAccount -> ExceptT String (DB m) AccountRef
accountRefOf acc = (`accountRef` acc) <$> portfolioOfAccount acc

{- The account and the pool a command works on -}

-- | The type of account a command takes on a network when --type is not
-- given: the default type of the configuration, or bip44 on a network
-- that has no SegWit. The networks of Bitcoin Cash have BIP44 accounts
-- alone. A user who made bip84 the default for Bitcoin still works on
-- Bitcoin Cash without an option, so the default is ignored there rather
-- than refused.
defaultKindOn :: Config -> Network -> AccountKind
defaultKindOn cfg net
  | kindOnNetwork net (configDefaultType cfg) = configDefaultType cfg
  | otherwise = Bip44

-- | The type of account of a command on a network: the one --type names,
-- which the network must have, or the default one ('defaultKindOn').
-- Unlike a default, a type the user typed is refused where the network
-- does not have it: --type bip84 on Bitcoin Cash is a mistake to tell,
-- not to work around.
chooseKind :: Config -> Network -> Maybe AccountKind -> Either String AccountKind
chooseKind cfg net given = case given of
  Just kind -> kind <$ checkKindOnNetwork net kind
  Nothing -> Right (defaultKindOn cfg net)

-- | The account of the portfolio a command works on: on the network
-- --network names or the default one, of the type --type names or the
-- default one ('chooseKind'). A partial portfolio may lack it, and the
-- refusal then says how to fill the portfolio in ('missingAccount').
chooseAccount ::
  (MonadUnliftIO m) =>
  Config ->
  Portfolio ->
  Maybe Network ->
  Maybe AccountKind ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
chooseAccount cfg portfolio netM kindM = do
  let net = commandNetwork cfg netM
  kind <- liftEither $ chooseKind cfg net kindM
  found <- lift $ poolAccount (portfolioPool portfolio net) kind
  maybe (throwError (missingAccount portfolio net (Just kind))) return found

-- | What the portfolio holds on the network a command works on, the one
-- --network names or the default one: its pool, which must have an
-- account. A partial portfolio may have none on the network, and the
-- refusal then says how to fill the portfolio in.
choosePool :: (MonadUnliftIO m) => Config -> Portfolio -> Maybe Network -> ExceptT String (DB m) Pool
choosePool cfg portfolio netM = do
  let pool = portfolioPool portfolio (commandNetwork cfg netM)
  accounts <- lift $ poolAccounts pool
  when (null accounts) $ throwError (missingAccount portfolio pool.poolNetwork Nothing)
  return pool

-- | The refusal of an account a partial portfolio does not have yet: of a
-- kind on a network, or of any kind there. It names the portfolio and
-- says how to fill it in, since the user cannot create the account.
missingAccount :: Portfolio -> Network -> Maybe AccountKind -> String
missingAccount portfolio net kindM =
  cs (portfolioName (portfolioIndex portfolio.portfolioRow))
    <> " of "
    <> cs (walletName (walletIndex portfolio.portfolioWallet))
    <> " has no "
    <> maybe "" ((<> " ") . cs . kindName) kindM
    <> "account on "
    <> net.name
    <> " yet. "
    <> howToFillIn

-- | How a partial portfolio is filled in, as the refusals and the answers
-- tell it. A portfolio is partial when an upgrade from hw 0.9 made it,
-- with the accounts 0.9 kept, and when importwallet made it, from a file
-- that lacks it: the portfolio0 every wallet has is empty then. So the
-- text says what is missing, and not where it came from.
howToFillIn :: String
howToFillIn =
  "It is a partial portfolio, which lacks accounts. \
  \Fill it in with fillwallet on the computer that has the mnemonic, \
  \or with importwallet and a file of the whole wallet that exportwallet wrote there."

{- Storing accounts -}

-- | Store an account of a kind on a network in a portfolio, from its
-- public key. A kind the network does not have is refused, and so is a
-- key another account of the network holds: the same seed would then be
-- in the database twice. So is a key that another account of the
-- portfolio at the same path does not hold ('checkSharedPath').
--
-- The first account of a portfolio gives the portfolio its creation date,
-- since the date of a portfolio is that of its oldest account. A
-- portfolio is without accounts until then only when it was made empty:
-- the portfolio 0 that the upgrade from hw 0.9 or importwallet makes for
-- a wallet that has none. Its date was then that of its wallet, or of the
-- import, and the day it is filled in is when its accounts begin.
insertAccount ::
  (MonadUnliftIO m) =>
  DBPortfolioId ->
  AccountKind ->
  Network ->
  Ctx ->
  XPubKey ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
insertAccount portfolioKey kind net ctx xpub = do
  liftEither $ checkKindOnNetwork net kind
  checkKeyFree net ctx xpub
  checkSharedPath portfolioKey kind net ctx xpub
  time <- liftIO getCurrentTime
  firstOne <- lift $ null <$> P.selectKeysList [DBAccountPortfolio P.==. portfolioKey] [P.LimitTo 1]
  when firstOne $ lift $ P.update portfolioKey [DBPortfolioCreated P.=. time]
  let account =
        DBAccount
          { dBAccountPortfolio = portfolioKey,
            dBAccountNetwork = cs net.name,
            dBAccountPurpose = fromIntegral (kindPurpose kind),
            dBAccountExternal = 0,
            dBAccountInternal = 0,
            dBAccountXPubKey = xPubExport net ctx xpub,
            dBAccountBalanceConfirmed = 0,
            dBAccountBalanceUnconfirmed = 0,
            dBAccountBalanceCoins = 0,
            dBAccountCreated = time
          }
  key <- lift $ P.insert account
  return (key, account)

-- | Refuse a key an account of the network holds already, naming the
-- portfolio that has it
checkKeyFree :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> ExceptT String (DB m) ()
checkKeyFree net ctx xpub = do
  holderM <- lift $ accountWithKey net ctx xpub
  forM_ holderM $ \(_, holder) -> do
    owner <- portfolioOfAccount holder
    throwError $
      cs (portfolioName (portfolioIndex owner.portfolioRow))
        <> " of "
        <> cs (walletName (walletIndex owner.portfolioWallet))
        <> " already has this key on "
        <> net.name

-- | Refuse a key for an account of a portfolio when another account of
-- the portfolio at the same path holds another key. The test networks
-- share the coin type 1, so the bip44 accounts of a portfolio on the six
-- of them have one path, m/44'/1'/index', and the bip84 accounts on the
-- three test networks of Bitcoin another: each path gives one key. Two
-- keys there are not of one seed, and receive would give different
-- addresses on two networks that must give the same. A file of a wallet
-- that does so is refused, whether it makes the portfolio or fills it in.
-- The refusal names the path and the two networks.
checkSharedPath :: (MonadUnliftIO m) => DBPortfolioId -> AccountKind -> Network -> Ctx -> XPubKey -> ExceptT String (DB m) ()
checkSharedPath portfolioKey kind net ctx xpub = do
  held <- lift $ portfolioAccountRows portfolioKey
  let differing =
        [ other
          | (_, acc) <- held,
            let other = accountNetwork acc,
            accountKind acc == kind,
            other.bip44Coin == net.bip44Coin,
            other /= net,
            not (sameKey xpub (accountXPubKey ctx acc))
        ]
  forM_ (take 1 differing) $ \other -> do
    portfolio <- getPortfolio portfolioKey
    let path = accountDeriv kind net (portfolioIndex portfolio.portfolioRow)
    throwError $
      "The "
        <> cs (kindName kind)
        <> " account "
        <> pathToStr path
        <> " of "
        <> portfolioText portfolio
        <> " has one key on "
        <> other.name
        <> " and would get another on "
        <> net.name
        <> ": the networks of one path share its key"

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

{- Whole and partial portfolios -}

-- | The accounts a portfolio lacks, by network and kind, in the order of
-- 'portfolioAccounts': none for a whole portfolio
missingAccounts :: (MonadUnliftIO m) => DBPortfolioId -> DB m [(Network, AccountKind)]
missingAccounts portfolioKey = do
  held <- portfolioAccountRows portfolioKey
  let has (net, kind) = any (\(_, acc) -> accountNetwork acc == net && accountKind acc == kind) held
  return (filter (not . has) portfolioAccounts)

-- | Whether a portfolio lacks some of its accounts
isPartial :: (MonadUnliftIO m) => DBPortfolioId -> DB m Bool
isPartial portfolioKey = not . null <$> missingAccounts portfolioKey

-- | Add to a portfolio the accounts it lacks, with the keys the seed
-- derives at its index, and say how many were added. An account the
-- portfolio has must hold the key the seed derives for it: a portfolio of
-- another seed is refused, and nothing is added. Every account is
-- checked before the first one is added, those the portfolio has and
-- those it lacks, whose keys no other portfolio may hold: a refusal
-- leaves the portfolio as it was, whether or not the caller undoes its
-- transaction.
fillPortfolio :: (MonadUnliftIO m) => Ctx -> PublicSeed -> Portfolio -> ExceptT String (DB m) Natural
fillPortfolio ctx seed portfolio = do
  let index = portfolioIndex portfolio.portfolioRow
      name = cs (portfolioName index) :: String
  keys <-
    liftMaybe ("The seed has no keys for " <> name <> ": a key at that index is invalid, and BIP32 skips it")
      =<< liftIO (seedPortfolioKeys seed index)
  held <- lift $ portfolioAccountRows portfolio.portfolioId
  lacking <- fmap catMaybes . forM keys $ \(net, kind, key) ->
    case [acc | (_, acc) <- held, accountNetwork acc == net, accountKind acc == kind] of
      acc : _
        | accountXPubKey ctx acc == key -> return Nothing
        | otherwise ->
            throwError $
              "The " <> cs (kindName kind) <> " account of " <> name <> " on " <> net.name <> " is not of this seed: it has another key"
      [] -> return (Just (net, kind, key))
  forM_ lacking $ \(net, _, key) -> checkKeyFree net ctx key
  forM_ lacking $ \(net, kind, key) -> insertAccount portfolio.portfolioId kind net ctx key
  return (fromIntegral (length lacking))

-- | Create a whole portfolio of a wallet at an index, which must be free,
-- from the seed of the wallet: every account of 'portfolioAccounts'. The
-- portfolio is read again once it has them, since its first account gives
-- it its creation date ('insertAccount').
createPortfolio :: (MonadUnliftIO m) => Ctx -> PublicSeed -> (DBWalletId, DBWallet) -> Natural -> ExceptT String (DB m) Portfolio
createPortfolio ctx seed (walletId, wallet) index = do
  (key, row) <- insertPortfolio walletId index ""
  _ <- fillPortfolio ctx seed (Portfolio key row walletId wallet)
  getPortfolio key

-- | The lowest index at which the wallet has no portfolio and the seed
-- has keys, with those keys: where newportfolio creates the next
-- portfolio of a wallet. An index whose child key is invalid, which BIP32
-- says to skip, is passed over. A portfolio that was deleted leaves its
-- index free, and creating it again gives the same portfolio, with the
-- same keys.
nextPortfolioIndex :: (MonadUnliftIO m) => PublicSeed -> DBWalletId -> DB m Natural
nextPortfolioIndex seed walletId = do
  taken <- map (portfolioIndex . snd) <$> getPortfolios walletId
  let go index
        | index `elem` taken = go (index + 1)
        | otherwise = do
            keys <- liftIO (seedPortfolioKeys seed index)
            if isJust keys then return index else go (index + 1)
  go 0

-- | Fill in every portfolio of a wallet from its seed, and make its
-- portfolio 0 when it has none: a wallet always has portfolio 0. Returns
-- how many accounts were added. The wallet must be the seed's
-- ('adoptSeed').
fillWallet :: (MonadUnliftIO m) => Ctx -> PublicSeed -> (DBWalletId, DBWallet) -> ExceptT String (DB m) Natural
fillWallet ctx seed held@(walletId, _) = do
  first <- lift $ portfolioAt walletId 0
  made <- case first of
    Just _ -> return 0
    Nothing -> fromIntegral . length <$> (lift . portfolioAccountRows . (.portfolioId) =<< createPortfolio ctx seed held 0)
  portfolios <- lift $ walletPortfolios held
  filled <- sum <$> mapM (fillPortfolio ctx seed) portfolios
  return (made + filled)

{- The wallet of a seed -}

-- | Whether the seed derives the key of an account of a portfolio at the
-- index of the portfolio, along the path of its kind on its network
seedDerives :: Ctx -> PublicSeed -> Portfolio -> DBAccount -> IO Bool
seedDerives ctx seed portfolio acc = do
  derived <- seedAccountKey seed (accountKind acc) (accountNetwork acc) (portfolioIndex portfolio.portfolioRow)
  return (derived == Just (accountXPubKey ctx acc))

-- | The wallets of a seed: those whose master key has its fingerprint, and
-- those of no known fingerprint whose accounts the seed derives. The
-- second kind is how a wallet made from the accounts of hw 0.9 is
-- recognized, since that version kept no master fingerprint.
--
-- The keys decide, not the fingerprint, which is 32 bits of a hash. A
-- wallet with the fingerprint of the seed and a key the seed does not
-- derive is of another seed, and is refused rather than mixed with this
-- one. So is a wallet that holds keys of the seed among others.
seedWallets :: (MonadUnliftIO m) => Ctx -> PublicSeed -> ExceptT String (DB m) [(DBWalletId, DBWallet)]
seedWallets ctx seed = do
  wallets <- lift getWallets
  fmap concat . forM wallets $ \held@(_, wallet) -> do
    portfolios <- lift $ walletPortfolios held
    accounts <- fmap concat . forM portfolios $ \portfolio ->
      map ((portfolio,) . snd) <$> lift (portfolioAccountRows portfolio.portfolioId)
    derived <- liftIO $ mapM (uncurry (seedDerives ctx seed)) accounts
    let name = cs (walletName (walletIndex wallet)) :: String
        sameFingerprint = walletFingerprint wallet == Just (seedFingerprint seed)
    if
      | null accounts -> return [held | sameFingerprint]
      | and derived -> return [held]
      | or derived -> throwError (name <> " holds keys of this seed among keys of another: its accounts are not of one seed")
      | sameFingerprint -> throwError (name <> " has the fingerprint of this seed and other keys: it is of another seed with the same fingerprint")
      | otherwise -> return []

-- | What became of the wallets of a seed once a command holding the seed
-- took them over ('adoptSeed')
data Adopted = Adopted
  { -- | The wallet of the seed, with its fingerprint
    adoptedWallet :: !(DBWalletId, DBWallet),
    -- | The indexes of the wallets merged into it, which are gone
    adoptedMerged :: ![Natural]
  }
  deriving (Eq, Show)

-- | Make the wallets of a seed one wallet that knows its fingerprint, and
-- give that wallet, or Nothing when the database has no wallet of the
-- seed. The wallets of the seed ('seedWallets') are merged into the one
-- with the lowest index ('mergeWallets'), which then records the
-- fingerprint of the seed.
--
-- The commands that make portfolios from a seed do this first:
-- newportfolio with a mnemonic the user has already, recover and
-- fillwallet. The seed proves which wallets are its own, which nothing
-- else in the database of hw 0.9 does.
adoptSeed :: (MonadUnliftIO m) => Ctx -> PublicSeed -> ExceptT String (DB m) (Maybe Adopted)
adoptSeed ctx seed = do
  matches <- seedWallets ctx seed
  case matches of
    [] -> return Nothing
    (targetId, _) : others -> do
      forM_ others (mergeWallets targetId)
      lift $ P.update targetId [DBWalletFingerprint P.=. Just (fingerprintToText (seedFingerprint seed))]
      wallet <- getWalletById targetId
      return $ Just (Adopted (targetId, wallet) (walletIndex . snd <$> others))

-- | Merge a wallet into another one of the same seed, and delete it. Each
-- of its portfolios moves to the other wallet, or into the portfolio the
-- other wallet has at the same index ('mergePortfolios'). The description
-- of the wallet that stays keeps what the other one said.
--
-- The wallet that stays is given by its key, and its description is read
-- here, at each merge. A seed may have three wallets or more, merged one
-- after the other into the same one. A row read before the first merge
-- would hold the description from before it, and each merge would write
-- that one with its own: only the last description merged would stay.
mergeWallets :: (MonadUnliftIO m) => DBWalletId -> (DBWalletId, DBWallet) -> ExceptT String (DB m) ()
mergeWallets targetId (sourceId, source) = do
  portfolios <- lift $ getPortfolios sourceId
  forM_ portfolios $ \moved@(movedId, row) -> do
    existing <- lift $ portfolioAt targetId (portfolioIndex row)
    case existing of
      Nothing -> lift $ P.update movedId [DBPortfolioWallet P.=. targetId]
      Just kept -> mergePortfolios kept moved
  target <- getWalletById targetId
  lift $ P.update targetId [DBWalletDescription P.=. joinDescriptions [dBWalletDescription target, dBWalletDescription source]]
  lift $ P.delete sourceId

-- | Descriptions joined into one, each once, the empty ones left out: a
-- wallet or a portfolio made of several keeps what each was called
joinDescriptions :: [Text] -> Text
joinDescriptions = Text.intercalate ", " . nub . filter (not . Text.null)

-- | Merge a portfolio into another one of the same seed and index, and
-- delete it: its accounts move, with their addresses and coins, and so do
-- its transactions, their labels and its pending transactions. The two
-- must not have the same account: the keys would differ, or be twice in
-- the database.
--
-- A network both portfolios have an account on is a pool of two accounts
-- from then on, and its live summaries were made for one account alone.
-- They are dropped, with the block the sync reached, so that the next
-- sync makes them again for the pool. The dead summaries are kept, and
-- move to the portfolio that stays: the store no longer lists a dead
-- transaction, so no sync would make them again, and the history would
-- lose them for good. When both portfolios have the same transaction
-- dead, the summary of the one that stays is kept. The labels are kept
-- too, that of the portfolio that stays winning.
--
-- The portfolio that stays keeps the older of the two creation dates. A
-- portfolio without accounts gives no date: the empty portfolio 0 that
-- the upgrade from hw 0.9 makes for a wallet without an account at index
-- 0 bears the date of its wallet, which may be older than any account
-- the merge brings it. The date of a portfolio is that of its oldest
-- account.
mergePortfolios :: (MonadUnliftIO m) => (DBPortfolioId, DBPortfolio) -> (DBPortfolioId, DBPortfolio) -> ExceptT String (DB m) ()
mergePortfolios (keptId, kept) (movedId, moved) = do
  keptAccounts <- lift $ portfolioAccountRows keptId
  movedAccounts <- lift $ portfolioAccountRows movedId
  forM_ movedAccounts $ \(_, acc) ->
    when (any (\(_, other) -> dBAccountNetwork other == dBAccountNetwork acc && dBAccountPurpose other == dBAccountPurpose acc) keptAccounts) $
      throwError $
        "The two portfolios " <> cs (portfolioName (portfolioIndex kept)) <> " of the seed both have a " <> cs (kindName (accountKind acc)) <> " account on " <> cs (dBAccountNetwork acc) <> ": they cannot be merged"
  let shared = [network | network <- nub (dBAccountNetwork . snd <$> movedAccounts), network `elem` (dBAccountNetwork . snd <$> keptAccounts)]
  lift $ do
    forM_ shared $ \network -> do
      forM_ [keptId, movedId] $ \key -> do
        P.deleteWhere [DBTxInfoPortfolio P.==. key, DBTxInfoNetwork P.==. network, DBTxInfoDead P.==. False]
        P.deleteWhere [DBSyncedBlockPortfolio P.==. key, DBSyncedBlockNetwork P.==. network]
      -- A dead summary of the portfolio that stays wins over the same one
      -- of the other
      deads <- P.selectList [DBTxInfoPortfolio P.==. movedId, DBTxInfoNetwork P.==. network] []
      forM_ deads $ \(P.Entity deadKey dead) -> do
        P.delete deadKey
        heldM <- P.get (DBTxInfoKey keptId network (dBTxInfoTxid dead))
        when (isNothing heldM) $ P.insert_ (dead {dBTxInfoPortfolio = keptId})
      -- A label of the portfolio that stays wins over one of the other
      labels <- P.selectList [DBTxLabelPortfolio P.==. movedId, DBTxLabelNetwork P.==. network] []
      forM_ labels $ \(P.Entity labelKey label) -> do
        P.delete labelKey
        heldM <- P.get (DBTxLabelKey keptId network (dBTxLabelTxid label))
        when (isNothing heldM) $ P.insert_ (label {dBTxLabelPortfolio = keptId})
    P.updateWhere [DBAccountPortfolio P.==. movedId] [DBAccountPortfolio P.=. keptId]
    P.updateWhere [DBTxInfoPortfolio P.==. movedId] [DBTxInfoPortfolio P.=. keptId]
    P.updateWhere [DBTxLabelPortfolio P.==. movedId] [DBTxLabelPortfolio P.=. keptId]
    P.updateWhere [DBPendingTxPortfolio P.==. movedId] [DBPendingTxPortfolio P.=. keptId]
    P.updateWhere [DBSyncedBlockPortfolio P.==. movedId] [DBSyncedBlockPortfolio P.=. keptId]
    P.update
      keptId
      [ DBPortfolioDescription P.=. joinDescriptions [dBPortfolioDescription kept, dBPortfolioDescription moved],
        DBPortfolioCreated P.=. mergedCreated (keptAccounts, kept) (movedAccounts, moved)
      ]
    P.delete movedId

-- | The creation date of two portfolios merged into one: the older of
-- their dates, among those of the portfolios that have accounts. A
-- portfolio without accounts has no date of an account to give
-- ('mergePortfolios'). When neither has any, the older date stays.
mergedCreated :: ([a], DBPortfolio) -> ([a], DBPortfolio) -> UTCTime
mergedCreated (keptAccounts, kept) (movedAccounts, moved) =
  case [dBPortfolioCreated row | (accounts, row) <- [(keptAccounts, kept), (movedAccounts, moved)], not (null accounts)] of
    [] -> min (dBPortfolioCreated kept) (dBPortfolioCreated moved)
    dated -> minimum dated

{- Deleting -}

-- | Delete a portfolio with everything it holds: its accounts, their
-- addresses and coins, its summaries, the labels of its transactions, its
-- pending transactions and the blocks its syncs reached
deletePortfolio :: (MonadUnliftIO m) => DBPortfolioId -> DB m ()
deletePortfolio portfolioKey = do
  accounts <- P.selectKeysList [DBAccountPortfolio P.==. portfolioKey] []
  P.deleteWhere [DBAddressAccount P.<-. accounts]
  P.deleteWhere [DBCoinAccount P.<-. accounts]
  P.deleteWhere [DBTxInfoPortfolio P.==. portfolioKey]
  P.deleteWhere [DBTxLabelPortfolio P.==. portfolioKey]
  P.deleteWhere [DBPendingTxPortfolio P.==. portfolioKey]
  P.deleteWhere [DBSyncedBlockPortfolio P.==. portfolioKey]
  P.deleteWhere [DBAccountPortfolio P.==. portfolioKey]
  P.delete portfolioKey

-- | Delete a wallet with all of its portfolios ('deletePortfolio'). The
-- funds stay on the chain, and the mnemonic brings the wallet back.
deleteWallet :: (MonadUnliftIO m) => DBWalletId -> DB m ()
deleteWallet walletId = do
  portfolios <- P.selectKeysList [DBPortfolioWallet P.==. walletId] []
  forM_ portfolios deletePortfolio
  P.delete walletId

-- | Whether a portfolio has ever had any activity, or gave out an
-- address: an address on either chain, paid or not, a coin, a
-- transaction in its history, alive or dead, or a pending transaction.
-- deleteportfolio deletes only a portfolio that has none of these.
--
-- A new portfolio has no address, since hw generates none by itself. An
-- address is there because receive gave it out, because a file labelled
-- it, because a sync found it used, or because a payment took it for its
-- change. Each of these may bring a payment later: a receiving address
-- may be with someone who pays it next month, and a payment prepared here
-- may be signed and sent from another computer. A portfolio deleted would
-- not see that payment until recover finds the portfolio again, and its
-- labels would be lost. So an address counts, whether it was paid or not.
-- The reading that only counts what the last sync stored was rejected: it
-- lets the user delete a portfolio whose address they just handed out.
portfolioHasActivity :: (MonadUnliftIO m) => DBPortfolioId -> DB m Bool
portfolioHasActivity portfolioKey = do
  accounts <- P.selectKeysList [DBAccountPortfolio P.==. portfolioKey] []
  addresses <- P.count [DBAddressAccount P.<-. accounts]
  coins <- P.count [DBCoinAccount P.<-. accounts]
  summaries <- P.count [DBTxInfoPortfolio P.==. portfolioKey]
  pending <- P.count [DBPendingTxPortfolio P.==. portfolioKey]
  return (addresses + coins + summaries + pending > 0)

{- A portfolio as portfolios shows it -}

-- | A portfolio as portfolios shows it: how the answers name it, what the
-- user and the database say of it, and its figures on each network.
data PortfolioView = PortfolioView
  { viewRef :: !PortfolioRef,
    viewDescription :: !Text,
    -- | The description of its wallet
    viewWalletDescription :: !Text,
    -- | The fingerprint of the seed of its wallet, when hw knows it
    viewFingerprint :: !(Maybe Fingerprint),
    viewCreated :: !UTCTime,
    -- | The accounts it lacks, by network and type: none for a whole
    -- portfolio
    viewMissing :: ![(Network, AccountKind)],
    -- | Its figures on each network it has an account on, in the order of
    -- the networks of a portfolio
    viewNetworks :: ![NetworkView]
  }
  deriving (Eq, Show)

-- | What a portfolio holds on a network, as portfolios shows it: the
-- balances of its accounts there added up, its coins and its addresses
-- counted, and the keys of each account.
data NetworkView = NetworkView
  { networkViewNetwork :: !Network,
    networkViewConfirmed :: !Word64,
    networkViewUnconfirmed :: !Word64,
    -- | What can be spent now ("Haskoin.Wallet.Database.Coin")
    networkViewSpendable :: !Word64,
    networkViewCoins :: !Word64,
    -- | The addresses generated, receiving and change ones
    networkViewAddresses :: !Natural,
    networkViewAccounts :: ![AccountKeys]
  }
  deriving (Eq, Show)

-- | The keys of an account, as portfolios --verbose gives them for
-- another wallet to take the account by: its path, its key, its output
-- descriptor, and its key in the form of SLIP-132, which is its xpub for
-- a BIP44 account
data AccountKeys = AccountKeys
  { keysKind :: !AccountKind,
    keysPath :: !HardPath,
    keysXPubKey :: !Text,
    keysDescriptor :: !Text,
    keysSlip132 :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | The view of a portfolio, given what each of its pools can spend now,
-- which this module cannot compute: see
-- 'Haskoin.Wallet.Database.Coin.spendableBalance'
portfolioView :: (MonadUnliftIO m) => Portfolio -> (Pool -> ExceptT String (DB m) Word64) -> ExceptT String (DB m) PortfolioView
portfolioView portfolio spendableOf = do
  accounts <- lift $ portfolioAccountRows portfolio.portfolioId
  missing <- lift $ missingAccounts portfolio.portfolioId
  networks <- forM (nub (accountNetwork . snd <$> accounts)) $ \net -> do
    let onNet = [acc | (_, acc) <- accounts, accountNetwork acc == net]
    spendable <- spendableOf (portfolioPool portfolio net)
    return
      NetworkView
        { networkViewNetwork = net,
          networkViewConfirmed = sum (dBAccountBalanceConfirmed <$> onNet),
          networkViewUnconfirmed = sum (dBAccountBalanceUnconfirmed <$> onNet),
          networkViewSpendable = spendable,
          networkViewCoins = sum (dBAccountBalanceCoins <$> onNet),
          networkViewAddresses = fromIntegral (sum [dBAccountExternal acc + dBAccountInternal acc | acc <- onNet]),
          networkViewAccounts =
            [ AccountKeys (accountKind acc) (accountPath portfolio acc) (dBAccountXPubKey acc) (accountDescriptorText portfolio acc) (accountSlip132Key acc)
              | acc <- onNet
            ]
        }
  return
    PortfolioView
      { viewRef = portfolioRef portfolio,
        viewDescription = dBPortfolioDescription portfolio.portfolioRow,
        viewWalletDescription = dBWalletDescription portfolio.portfolioWallet,
        viewFingerprint = portfolioFingerprint portfolio,
        viewCreated = dBPortfolioCreated portfolio.portfolioRow,
        viewMissing = missing,
        viewNetworks = networks
      }

-- | Whether the view is of a partial portfolio, which lacks accounts
viewPartial :: PortfolioView -> Bool
viewPartial = not . null . viewMissing

instance ToJSON AccountKeys where
  toJSON keys =
    object
      [ "type" .= keysKind keys,
        "derivation" .= pathToStr (keysPath keys),
        "xPubKey" .= keysXPubKey keys,
        "descriptor" .= keysDescriptor keys,
        "slip132" .= keysSlip132 keys
      ]

instance FromJSON AccountKeys where
  parseJSON = withObject "account" $ \o -> do
    text <- o .: "derivation"
    path <- maybe (fail ("Not the path of an account: " <> text)) return (parseHard text)
    AccountKeys <$> o .: "type" <*> return path <*> o .: "xPubKey" <*> o .: "descriptor" <*> o .:? "slip132"

instance ToJSON NetworkView where
  toJSON view =
    object
      [ "network" .= (networkViewNetwork view).name,
        "balance"
          .= object
            [ "confirmed" .= networkViewConfirmed view,
              "unconfirmed" .= networkViewUnconfirmed view,
              "spendable" .= networkViewSpendable view,
              "coins" .= networkViewCoins view
            ],
        "addresses" .= networkViewAddresses view,
        "accounts" .= networkViewAccounts view
      ]

instance FromJSON NetworkView where
  parseJSON = withObject "network of a portfolio" $ \o -> do
    net <- networkNamedIn =<< o .: "network"
    balance <- o .: "balance"
    NetworkView net
      <$> balance .: "confirmed"
      <*> balance .: "unconfirmed"
      <*> balance .: "spendable"
      <*> balance .: "coins"
      <*> o .: "addresses"
      <*> o .: "accounts"

-- | The network of a name in JSON
networkNamedIn :: String -> Parser Network
networkNamedIn name = maybe (fail ("Unknown network " <> name)) return (netByName name)

instance ToJSON PortfolioView where
  toJSON view =
    object $
      portfolioRefPairs (viewRef view)
        <> [ "name" .= portfolioName (refPortfolio (viewRef view)),
             "description" .= viewDescription view,
             "walletdescription" .= viewWalletDescription view,
             "fingerprint" .= viewFingerprint view,
             "created" .= viewCreated view,
             "partial" .= viewPartial view,
             "missing" .= [object ["network" .= net.name, "type" .= kind] | (net, kind) <- viewMissing view],
             "networks" .= viewNetworks view
           ]

instance FromJSON PortfolioView where
  parseJSON = withObject "portfolio" $ \o ->
    PortfolioView
      <$> parsePortfolioRef o
      <*> o .: "description"
      <*> o .: "walletdescription"
      <*> o .:? "fingerprint"
      <*> o .: "created"
      <*> (mapM missing =<< o .: "missing")
      <*> o .: "networks"
    where
      missing = withObject "missing account" $ \m -> (,) <$> (networkNamedIn =<< m .: "network") <*> m .: "type"

-- | The networks of a text, as a sentence names them: "btc", "btc and
-- bch", "btc, bch and btctest"
networksText :: [Network] -> String
networksText nets =
  case reverse ((.name) <$> nets) of
    [] -> ""
    [one] -> one
    final : others -> intercalate ", " (reverse others) <> " and " <> final

-- | Whether nothing is known of the seed of a wallet
fingerprintUnknown :: DBWallet -> Bool
fingerprintUnknown = isNothing . dBWalletFingerprint
