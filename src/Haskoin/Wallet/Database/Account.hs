{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The accounts of the portfolios. An account is one derivation path on
-- one network, m/purpose'/coin'/index': it has its portfolio, whose index
-- is the last step of the path, its network, and its kind
-- ("Haskoin.Wallet.Keys".'AccountKind'), with its key, its balances and
-- its address counts. The user never creates or names an account. A
-- portfolio has every account of 'portfolioAccounts' from the moment it
-- exists: they are made together, from the seed of the wallet or from the
-- keys of a file of it ('createPortfolio', 'createPortfolioWith'). A
-- portfolio that lacks accounts would need the mnemonic to be completed,
-- since a public key of an account derives none of the others: their
-- paths are hardened. So hw never makes one.
--
-- A command reaches an account through the portfolio it works in, the
-- active one, then the network and the type its options or the
-- configuration give
-- ('chooseAccount'). A command that spends or lists transactions works on
-- a pool, the accounts of the portfolio on one network together
-- ('portfolioPool').
--
-- A seed has one wallet, the one with its fingerprint ('seedWallet'). The
-- fingerprint is 32 bits of a hash, and another seed may have it. So the
-- keys of the wallet are checked against those the seed derives, and a
-- wallet of the fingerprint with other keys is refused, rather than given
-- the portfolios of this seed.
--
-- A key is held by one account of a network at most. It is compared as a
-- key, its public key and chain code, and not as the text of an extended
-- key, which also names a child number that a file can change
-- ('accountWithKey'). The test networks share their keys, so the accounts
-- of a portfolio at one path hold one key, on each of those networks
-- ('checkSharedPath').
module Haskoin.Wallet.Database.Account where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (intercalate, nub, sortOn)
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
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
-- kind, pkh or wpkh, with the origin of the key, which names the
-- fingerprint of the seed that the wallet of the portfolio keeps, and
-- both chains. A hardware wallet matches the account to its seed by the
-- origin.
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
-- in the order of the networks of a portfolio, which is all eight
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
-- default one ('chooseKind'). A portfolio has an account of every type
-- each of its networks has, and 'chooseKind' refuses a type the network
-- does not have.
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
  poolAccountOf (portfolioPool portfolio net) kind

{- Storing accounts -}

-- | Store an account of a kind on a network in a portfolio, from its
-- public key. A kind the network does not have is refused, and so is a
-- key another account of the network holds: the same seed would then be
-- in the database twice. So is a key that another account of the
-- portfolio at the same path does not hold ('checkSharedPath'). A
-- portfolio gets its accounts as it is created ('createPortfolioWith').
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
-- that does so is refused. The refusal names the path and the two
-- networks.
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

{- Creating portfolios -}

-- | Create a whole portfolio of a wallet at an index, which must be free,
-- with the keys the seed of the wallet derives there: every account of
-- 'portfolioAccounts'. An index whose child key is invalid, which BIP32
-- says to skip, is refused: 'nextPortfolioIndex' passes it over.
createPortfolio :: (MonadUnliftIO m) => Ctx -> PublicSeed -> (DBWalletId, DBWallet) -> Natural -> ExceptT String (DB m) Portfolio
createPortfolio ctx seed held index = do
  keys <-
    liftMaybe ("The seed has no keys for " <> cs (portfolioName index) <> ": a key at that index is invalid, and BIP32 skips it")
      =<< liftIO (seedPortfolioKeys seed index)
  createPortfolioWith ctx held index "" keys

-- | Create a whole portfolio of a wallet at an index, which must be free,
-- with a description and the key of each of its accounts: newportfolio
-- and recover give the keys the seed derives ('createPortfolio'), and
-- importwallet those of a file. The portfolio and its twelve accounts are
-- written together, and the portfolio is dated as they are.
--
-- The keys must be those of the twelve accounts of 'portfolioAccounts',
-- each once: a portfolio has all of them or does not exist. A key that
-- another account of its network holds is refused ('checkKeyFree'), and
-- so are two keys for the accounts of one path, as the test networks
-- share theirs ('checkSharedPath'). These are checked before the first
-- row is written, so that a refusal leaves the database as it was.
--
-- The portfolio becomes the active one when the database has none
-- ('insertPortfolio'): the first portfolio of a database is where the
-- commands work, without a switch. A command that creates several creates
-- them in the order of their indexes, so that the first one, portfolio0
-- of a new wallet or the lowest index created, becomes active.
createPortfolioWith ::
  (MonadUnliftIO m) =>
  Ctx ->
  (DBWalletId, DBWallet) ->
  Natural ->
  Text ->
  [(Network, AccountKind, XPubKey)] ->
  ExceptT String (DB m) Portfolio
createPortfolioWith ctx (walletId, wallet) index description keys = do
  let given = [(net, kind) | (net, kind, _) <- keys]
      name = cs (portfolioName index) <> " of " <> cs (walletName (walletIndex wallet)) :: String
  unless (length given == length portfolioAccounts && all (`elem` given) portfolioAccounts) $
    throwError (name <> " cannot be created without a key for each of its twelve accounts, one for every network and type")
  forM_ keys $ \(net, _, key) -> checkKeyFree net ctx key
  forM_ (take 1 (sharedPathClashes keys)) $ \(kind, one, other) ->
    throwError $
      "The "
        <> cs (kindName kind)
        <> " account "
        <> pathToStr (accountDeriv kind one index)
        <> " of "
        <> name
        <> " has one key on "
        <> one.name
        <> " and would get another on "
        <> other.name
        <> ": the networks of one path share its key"
  (key, row) <- insertPortfolio walletId index description
  forM_ keys $ \(net, kind, xpub) -> insertAccount key kind net ctx xpub
  return (Portfolio key row walletId wallet)

-- | The accounts of the keys given that share a path and have two keys,
-- by their kind and their two networks: the test networks share the coin
-- type 1, so their accounts of one kind must hold one key ('checkSharedPath')
sharedPathClashes :: [(Network, AccountKind, XPubKey)] -> [(AccountKind, Network, Network)]
sharedPathClashes keys =
  [ (kind, one, other)
    | ((one, kind, key), i) <- zip keys [0 :: Int ..],
      ((other, otherKind, otherKey), j) <- zip keys [0 ..],
      i < j,
      otherKind == kind,
      other.bip44Coin == one.bip44Coin,
      not (sameKey key otherKey)
  ]

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

{- The wallet of a seed -}

-- | Whether the seed derives the key of an account of a portfolio at the
-- index of the portfolio, along the path of its kind on its network
seedDerives :: Ctx -> PublicSeed -> Portfolio -> DBAccount -> IO Bool
seedDerives ctx seed portfolio acc = do
  derived <- seedAccountKey seed (accountKind acc) (accountNetwork acc) (portfolioIndex portfolio.portfolioRow)
  return (derived == Just (accountXPubKey ctx acc))

-- | The wallet of a seed: the one whose master key has the fingerprint of
-- the seed, if the database has it. newportfolio --existing and recover
-- add their portfolios to it.
--
-- The fingerprint is 32 bits of a hash, so another seed may have it. The
-- portfolios of the wallet must hold the keys the seed derives at their
-- indexes: a wallet of the fingerprint with other keys is of another
-- seed, and is refused rather than given the portfolios of this one.
seedWallet :: (MonadUnliftIO m) => Ctx -> PublicSeed -> ExceptT String (DB m) (Maybe (DBWalletId, DBWallet))
seedWallet ctx seed = do
  foundM <- lift $ walletWithFingerprint (seedFingerprint seed)
  forM_ foundM $ \held@(_, wallet) -> do
    portfolios <- lift $ walletPortfolios held
    accounts <- fmap concat . forM portfolios $ \portfolio ->
      map ((portfolio,) . snd) <$> lift (portfolioAccountRows portfolio.portfolioId)
    derived <- liftIO $ mapM (uncurry (seedDerives ctx seed)) accounts
    unless (and derived) $
      throwError (cs (walletName (walletIndex wallet)) <> " has the fingerprint of this seed and other keys: it is of another seed with the same fingerprint. Nothing was changed")
  return foundM

{- Deleting -}

-- | Delete a portfolio with everything it holds: its accounts, their
-- addresses and coins, its summaries, the labels of its transactions, its
-- pending transactions and the blocks its syncs reached. When it is the
-- active portfolio, the database is left without one: deleteportfolio
-- refuses the active portfolio, and deletewallet the wallet of the active
-- portfolio unless it is the last, so this happens only when the last
-- wallet goes, and the next portfolio created becomes active.
deletePortfolio :: (MonadUnliftIO m) => DBPortfolioId -> DB m ()
deletePortfolio portfolioKey = do
  P.deleteWhere [DBActivePortfolioPortfolio P.==. portfolioKey]
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

-- | A portfolio as portfolios shows it: how the answers name it, whether
-- it is the active one, what the user and the database say of it, and
-- its figures on each network.
data PortfolioView = PortfolioView
  { viewRef :: !PortfolioRef,
    -- | Whether it is the active portfolio, the one the commands work in,
    -- which portfolios marks
    viewActive :: !Bool,
    viewDescription :: !Text,
    -- | The description of its wallet
    viewWalletDescription :: !Text,
    -- | The fingerprint of the seed of its wallet
    viewFingerprint :: !Fingerprint,
    viewCreated :: !UTCTime,
    -- | Its figures on each of its networks, in the order of the networks
    -- of a portfolio
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
  activeM <- lift activePortfolioId
  accounts <- lift $ portfolioAccountRows portfolio.portfolioId
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
        viewActive = activeM == Just portfolio.portfolioId,
        viewDescription = dBPortfolioDescription portfolio.portfolioRow,
        viewWalletDescription = dBWalletDescription portfolio.portfolioWallet,
        viewFingerprint = portfolioFingerprint portfolio,
        viewCreated = dBPortfolioCreated portfolio.portfolioRow,
        viewNetworks = networks
      }

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
             "active" .= viewActive view,
             "description" .= viewDescription view,
             "walletdescription" .= viewWalletDescription view,
             "fingerprint" .= viewFingerprint view,
             "created" .= viewCreated view,
             "networks" .= viewNetworks view
           ]

instance FromJSON PortfolioView where
  parseJSON = withObject "portfolio" $ \o ->
    PortfolioView
      <$> parsePortfolioRef o
      <*> o .: "active"
      <*> o .: "description"
      <*> o .: "walletdescription"
      <*> o .: "fingerprint"
      <*> o .: "created"
      <*> o .: "networks"

-- | The networks of a text, as a sentence names them: "btc", "btc and
-- bch", "btc, bch and btctest"
networksText :: [Network] -> String
networksText nets =
  case reverse ((.name) <$> nets) of
    [] -> ""
    [one] -> one
    final : others -> intercalate ", " (reverse others) <> " and " <> final
