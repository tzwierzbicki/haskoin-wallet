{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The wallets and their portfolios.
--
-- A wallet is the master key of one mnemonic. A portfolio holds every
-- account of a wallet at one account index, on every network and of
-- every kind. The commands work in the active portfolio, which the
-- database keeps, and never name an account: a command is told the
-- network and the type of address it works on, and the rest follows. The
-- user of one mnemonic works in portfolio 0 of wallet 0, which became
-- active as it was created.
--
-- Wallets and portfolios have no names of their own. Each is named by its
-- index, wallet0 and portfolio0, which the user cannot change, and each
-- has a description the user can ('setWalletDescription',
-- 'setPortfolioDescription'). A name the user picks would have to be
-- unique and typed on every command. An index is short, has a natural
-- default, and says where the portfolio is: portfolio 2 is the accounts
-- at the index 2' of their paths, in any wallet software.
--
-- A new wallet or portfolio takes the lowest free index ('lowestFree'). So
-- deleting wallet0 leaves wallet1 as it is, and the next wallet made is
-- wallet0 again.
--
-- This module holds the rows of the two tables, the active portfolio, the
-- one a command works in ('requireActivePortfolio', 'switchTo'), and how
-- the answers name a portfolio, a network of it, and an account of it
-- ('PortfolioRef', 'Place', 'AccountRef'). The accounts themselves are in
-- "Haskoin.Wallet.Database.Account".
module Haskoin.Wallet.Database.Portfolio where

import Conduit (MonadUnliftIO)
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Char (isControl)
import Data.Maybe (isJust, isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Keys (AccountKind, accountDeriv, indexFits, kindName)
import Numeric.Natural (Natural)

{- Names -}

-- | The name of a wallet, from its index: wallet0, wallet1 and so on
walletName :: Natural -> Text
walletName index = "wallet" <> cs (show index)

-- | The name of a portfolio, from its index: portfolio0, portfolio1 and
-- so on
portfolioName :: Natural -> Text
portfolioName index = "portfolio" <> cs (show index)

-- | The lowest number from 0 that is not among those taken: the index of
-- a new wallet, or of a new portfolio of a wallet. A wallet or a
-- portfolio that was deleted leaves its index free, and the next one made
-- takes it.
lowestFree :: [Natural] -> Natural
lowestFree taken = go 0
  where
    go index
      | index `elem` taken = go (index + 1)
      | otherwise = index

-- | The rules of a description: one line, with no control character,
-- which a terminal would not show as typed, and no space at either end,
-- which a quoting slip on the command line leaves. An empty description
-- is none.
checkDescription :: Text -> Either String ()
checkDescription description
  | Text.any isControl description = Left "The description must not contain a line break or a control character"
  | Text.strip description /= description = Left "The description must not start or end with a space"
  | otherwise = Right ()

{- How the answers name a place -}

-- | A portfolio as the answers name it: by the index of its wallet and its
-- own. The rows of the database are named by their keys, which mean
-- nothing to the user and differ from one computer to the next.
data PortfolioRef = PortfolioRef
  { refWallet :: !Natural,
    refPortfolio :: !Natural
  }
  deriving (Eq, Ord, Show)

-- | A portfolio on a network, as the answers name it: where coins and
-- transactions are
data Place = Place
  { placePortfolio :: !PortfolioRef,
    placeNetwork :: !Network
  }
  deriving (Eq, Show)

-- | An account of a portfolio, as the answers name it: by the portfolio,
-- the network and the kind. Its path follows from the three
-- ('accountRefPath').
data AccountRef = AccountRef
  { accountRefPlace :: !Place,
    accountRefKind :: !AccountKind
  }
  deriving (Eq, Show)

-- | Whether a view names the wallet of each portfolio. A database of one
-- wallet has no other to tell it from, so its name would only take room:
-- "portfolio0" says it all, and the user of one mnemonic never reads
-- "wallet0". A database of several wallets names every one of them,
-- wallet0 too, as in "portfolio1 of wallet0".
--
-- Every view follows the same rule: the field and the title that name a
-- portfolio, a portfolio after a path, and the status line of a sync
-- ('portfolioRefName').
--
-- The rule that came first left wallet0 out in every database, and
-- named the other wallets. It read wrong as soon as another wallet was
-- in sight. In a view of portfolio0 of wallet1, "portfolio1" was meant as
-- that of wallet0, and read as that of wallet1.
--
-- The naming follows from the wallets of the database ('walletNaming'),
-- not from those an answer shows. With two wallets of which one alone
-- shows, its name still tells which.
data WalletNaming = WithWallets | WithoutWallets
  deriving (Eq, Show)

-- | How the views name the wallets, from how many wallets the database
-- has
walletNamingOf :: Int -> WalletNaming
walletNamingOf count
  | count > 1 = WithWallets
  | otherwise = WithoutWallets

-- | A portfolio as a view names it: "portfolio0", or "portfolio0 of
-- wallet1" where the views name the wallets ('WalletNaming'). A sentence
-- names both always ('portfolioText').
portfolioRefName :: WalletNaming -> PortfolioRef -> Text
portfolioRefName naming ref = case naming of
  WithWallets -> name <> " of " <> walletName (refWallet ref)
  WithoutWallets -> name
  where
    name = portfolioName (refPortfolio ref)

-- | The network of an account an answer names
accountRefNetwork :: AccountRef -> Network
accountRefNetwork = placeNetwork . accountRefPlace

-- | The path of an account an answer names, m/purpose'/coin'/index': the
-- purpose of its kind, the coin type of its network, and the index of
-- its portfolio
accountRefPath :: AccountRef -> HardPath
accountRefPath ref =
  accountDeriv (accountRefKind ref) (accountRefNetwork ref) (refPortfolio (placePortfolio (accountRefPlace ref)))

-- In JSON, a place is written flat among the keys of what it places: the
-- indexes of the wallet and of the portfolio, then the network, then the
-- type and the path of the account.

-- | The keys of a portfolio in the JSON of an answer: the indexes of its
-- wallet and of the portfolio
portfolioRefPairs :: PortfolioRef -> [Pair]
portfolioRefPairs ref = ["wallet" .= refWallet ref, "portfolio" .= refPortfolio ref]

-- | A portfolio back from the keys 'portfolioRefPairs' writes
parsePortfolioRef :: Object -> Parser PortfolioRef
parsePortfolioRef o = PortfolioRef <$> o .: "wallet" <*> o .: "portfolio"

-- | The keys of a place in the JSON of an answer: those of its portfolio,
-- and the name of its network
placePairs :: Place -> [Pair]
placePairs place = portfolioRefPairs (placePortfolio place) <> ["network" .= (placeNetwork place).name]

-- | A place back from the keys 'placePairs' writes. A network hw does not
-- know is refused.
parsePlace :: Object -> Parser Place
parsePlace o = do
  name <- o .: "network"
  net <- maybe (fail ("Unknown network " <> name)) return (netByName name)
  (`Place` net) <$> parsePortfolioRef o

-- | The keys of an account in the JSON of an answer: those of its place,
-- the type of its addresses and its derivation path, which a script reads
-- without working it out
accountRefPairs :: AccountRef -> [Pair]
accountRefPairs ref =
  placePairs (accountRefPlace ref)
    <> [ "type" .= kindName (accountRefKind ref),
         "derivation" .= pathToStr (accountRefPath ref)
       ]

-- | An account back from the keys 'accountRefPairs' writes. The path is
-- not read: it follows from the place and the type.
parseAccountRef :: Object -> Parser AccountRef
parseAccountRef o = AccountRef <$> parsePlace o <*> o .: "type"

instance ToJSON PortfolioRef where
  toJSON = object . portfolioRefPairs

instance FromJSON PortfolioRef where
  parseJSON = withObject "portfolio" parsePortfolioRef

instance ToJSON Place where
  toJSON = object . placePairs

instance FromJSON Place where
  parseJSON = withObject "portfolio on a network" parsePlace

instance ToJSON AccountRef where
  toJSON = object . accountRefPairs

instance FromJSON AccountRef where
  parseJSON = withObject "account" parseAccountRef

{- Wallets -}

-- | The index of a wallet
walletIndex :: DBWallet -> Natural
walletIndex = fromIntegral . dBWalletIndex

-- | The fingerprint of the master key of a wallet. Every wallet has one:
-- hw creates a wallet from its seed, or from a file of the wallet, which
-- names it. No two wallets have the same.
walletFingerprint :: DBWallet -> Fingerprint
walletFingerprint wallet =
  either (const (error "Invalid fingerprint in database")) id $
    textToFingerprint (dBWalletFingerprint wallet)

-- | Every wallet, in the order of their indexes
getWallets :: (MonadUnliftIO m) => DB m [(DBWalletId, DBWallet)]
getWallets = map entityPair <$> P.selectList [] [P.Asc DBWalletIndex]

-- | How the views name the wallets now, from the wallets of the database
-- ('WalletNaming')
walletNaming :: (MonadUnliftIO m) => DB m WalletNaming
walletNaming = walletNamingOf . length <$> getWallets

-- | A row with its key
entityPair :: P.Entity a -> (P.Key a, a)
entityPair e = (P.entityKey e, P.entityVal e)

-- | The wallet of an index, if there is one. An index of 2^31 or more
-- has none ('indexFits'): the database keeps an index as a number of 64
-- bits, and a larger one would wrap around to the index of another
-- wallet.
walletAt :: (MonadUnliftIO m) => Natural -> DB m (Maybe (DBWalletId, DBWallet))
walletAt index
  | not (indexFits index) = return Nothing
  | otherwise = fmap entityPair <$> P.getBy (UniqueWalletIndex (fromIntegral index))

-- | The wallet of a key
getWalletById :: (MonadUnliftIO m) => DBWalletId -> ExceptT String (DB m) DBWallet
getWalletById walletId = liftMaybe "Invalid wallet" =<< lift (P.get walletId)

-- | The wallet whose master key has the fingerprint, if there is one.
-- This is how hw finds the wallet of a seed, and of a file of a wallet.
walletWithFingerprint :: (MonadUnliftIO m) => Fingerprint -> DB m (Maybe (DBWalletId, DBWallet))
walletWithFingerprint fingerprint =
  fmap entityPair <$> P.getBy (UniqueWalletFingerprint (fingerprintToText fingerprint))

-- | A new wallet, at the lowest free index, with the fingerprint of its
-- master key, and no description. The caller has looked for a wallet of
-- the fingerprint first ('walletWithFingerprint'): the database refuses a
-- second one.
insertWallet :: (MonadUnliftIO m) => Fingerprint -> DB m (DBWalletId, DBWallet)
insertWallet fingerprint = do
  taken <- map (walletIndex . snd) <$> getWallets
  time <- liftIO getCurrentTime
  let wallet =
        DBWallet
          { dBWalletIndex = fromIntegral (lowestFree taken),
            dBWalletFingerprint = fingerprintToText fingerprint,
            dBWalletDescription = "",
            dBWalletCreated = time
          }
  walletId <- P.insert wallet
  return (walletId, wallet)

-- | Set the description of a wallet, in place of the one it had. An
-- empty one removes it.
setWalletDescription :: (MonadUnliftIO m) => DBWalletId -> Text -> ExceptT String (DB m) ()
setWalletDescription walletId description = do
  liftEither $ checkDescription description
  lift $ P.update walletId [DBWalletDescription P.=. description]

{- Portfolios -}

-- | The index of a portfolio, which is the account index of its accounts
portfolioIndex :: DBPortfolio -> Natural
portfolioIndex = fromIntegral . dBPortfolioIndex

-- | The portfolios of a wallet, in the order of their indexes
getPortfolios :: (MonadUnliftIO m) => DBWalletId -> DB m [(DBPortfolioId, DBPortfolio)]
getPortfolios walletId = map entityPair <$> P.selectList [DBPortfolioWallet P.==. walletId] [P.Asc DBPortfolioIndex]

-- | The portfolio of a wallet at an index, if the wallet has it. An index
-- of 2^31 or more has none ('indexFits'): it is no account index, and as
-- a number of the database it would wrap around to the index of another
-- portfolio.
portfolioAt :: (MonadUnliftIO m) => DBWalletId -> Natural -> DB m (Maybe (DBPortfolioId, DBPortfolio))
portfolioAt walletId index
  | not (indexFits index) = return Nothing
  | otherwise = fmap entityPair <$> P.getBy (UniquePortfolio walletId (fromIntegral index))

-- | A new portfolio of a wallet at an index, which must be free and below
-- 2^31 ('indexFits'), with a description. It becomes the active portfolio
-- when the database has none ('activateIfNone'), as the first portfolio
-- of a database does, here where every portfolio row is written.
insertPortfolio :: (MonadUnliftIO m) => DBWalletId -> Natural -> Text -> ExceptT String (DB m) (DBPortfolioId, DBPortfolio)
insertPortfolio walletId index description = do
  liftEither $ checkDescription description
  unless (indexFits index) $ throwError (cs (portfolioName index) <> " cannot be: the index of a portfolio is below 2147483648, which is 2^31")
  taken <- lift $ portfolioAt walletId index
  when (isJust taken) $ throwError ("The wallet has " <> cs (portfolioName index) <> " already")
  time <- liftIO getCurrentTime
  let portfolio =
        DBPortfolio
          { dBPortfolioWallet = walletId,
            dBPortfolioIndex = fromIntegral index,
            dBPortfolioDescription = description,
            dBPortfolioCreated = time
          }
  key <- lift $ P.insert portfolio
  _ <- lift $ activateIfNone key
  return (key, portfolio)

-- | Set the description of a portfolio, in place of the one it had. An
-- empty one removes it.
setPortfolioDescription :: (MonadUnliftIO m) => DBPortfolioId -> Text -> ExceptT String (DB m) ()
setPortfolioDescription key description = do
  liftEither $ checkDescription description
  lift $ P.update key [DBPortfolioDescription P.=. description]

{- A portfolio with its wallet -}

-- | A portfolio as the commands hold it: its row and the row of its
-- wallet, each with its key. The wallet says the index that names it and
-- the fingerprint of its seed, which the accounts of the portfolio no
-- longer keep themselves.
data Portfolio = Portfolio
  { portfolioId :: !DBPortfolioId,
    portfolioRow :: !DBPortfolio,
    portfolioWalletId :: !DBWalletId,
    portfolioWallet :: !DBWallet
  }
  deriving (Eq, Show)

-- | How the answers name the portfolio
portfolioRef :: Portfolio -> PortfolioRef
portfolioRef portfolio = PortfolioRef (walletIndex portfolio.portfolioWallet) (portfolioIndex portfolio.portfolioRow)

-- | The fingerprint of the seed of the portfolio, that of its wallet
portfolioFingerprint :: Portfolio -> Fingerprint
portfolioFingerprint = walletFingerprint . (.portfolioWallet)

-- | What the portfolio holds on a network
portfolioPool :: Portfolio -> Network -> Pool
portfolioPool portfolio = Pool portfolio.portfolioId

-- | How the answers name what the portfolio holds on a network
portfolioPlace :: Portfolio -> Network -> Place
portfolioPlace portfolio = Place (portfolioRef portfolio)

-- | The portfolio of a key, with its wallet
getPortfolio :: (MonadUnliftIO m) => DBPortfolioId -> ExceptT String (DB m) Portfolio
getPortfolio portfolioKey = do
  row <- liftMaybe "Invalid portfolio" =<< lift (P.get portfolioKey)
  wallet <- getWalletById (dBPortfolioWallet row)
  return (Portfolio portfolioKey row (dBPortfolioWallet row) wallet)

-- | Every portfolio, with its wallet, in the order of the wallets and of
-- the portfolios of each
getAllPortfolios :: (MonadUnliftIO m) => DB m [Portfolio]
getAllPortfolios = do
  wallets <- getWallets
  concat <$> mapM walletPortfolios wallets

-- | The portfolios of a wallet, each with the wallet, in the order of
-- their indexes
walletPortfolios :: (MonadUnliftIO m) => (DBWalletId, DBWallet) -> DB m [Portfolio]
walletPortfolios (walletId, wallet) =
  map (\(key, row) -> Portfolio key row walletId wallet) <$> getPortfolios walletId

-- | How the answers name the place of a pool
poolPlace :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) Place
poolPlace pool = (`portfolioPlace` pool.poolNetwork) <$> getPortfolio pool.poolPortfolio

{- The active portfolio -}

-- The commands work in one portfolio, the active one, which no option
-- names. The command switch makes another one active, and nothing else
-- does, but the creation of the first portfolio of a database, which
-- becomes active as it is created ('activateIfNone'). The choice is kept
-- in the database, in the one row of its table: a fact the user decides,
-- which the next command finds as the last one left it.
--
-- Options and keys of the configuration file with defaults did this job
-- before, --wallet and --portfolio on most commands, and the keys
-- default-wallet and default-portfolio. They were turned down for a state
-- that is easy to keep track of: one portfolio at a time, which
-- portfolios marks, rather than a default and options that override it
-- on each command. A command line names a portfolio only where it names
-- its target, and never takes it from the state: switch, deletewallet
-- and deleteportfolio. exportwallet, importwallet and show of a file of a
-- wallet name one portfolio of the file.

-- | The key of the one row of the table of the active portfolio
activeSlot :: P.Key DBActivePortfolio
activeSlot = DBActivePortfolioKey 0

-- | The key of the active portfolio, if the database has one. A database
-- without any portfolio has none.
activePortfolioId :: (MonadUnliftIO m) => DB m (Maybe DBPortfolioId)
activePortfolioId = fmap dBActivePortfolioPortfolio <$> P.get activeSlot

-- | The active portfolio, with its wallet, if the database has one
activePortfolio :: (MonadUnliftIO m) => ExceptT String (DB m) (Maybe Portfolio)
activePortfolio = mapM getPortfolio =<< lift activePortfolioId

-- | The active portfolio, the one a command works in. A database without
-- any portfolio has none, and the command is refused ('noWallets').
requireActivePortfolio :: (MonadUnliftIO m) => ExceptT String (DB m) Portfolio
requireActivePortfolio = liftMaybe noWallets =<< activePortfolio

-- | How the answers name the active portfolio, if the database has one
activePortfolioRef :: (MonadUnliftIO m) => ExceptT String (DB m) (Maybe PortfolioRef)
activePortfolioRef = fmap portfolioRef <$> activePortfolio

-- | The refusal of a command that needs a portfolio when the database has
-- none
noWallets :: String
noWallets = "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"

-- | Make the portfolio of the key the active one, in place of the one
-- that was
setActivePortfolio :: (MonadUnliftIO m) => DBPortfolioId -> DB m ()
setActivePortfolio key = P.repsert activeSlot (DBActivePortfolio 0 key)

-- | Make the portfolio of the key the active one when the database has
-- none, and say whether it did. The first portfolio created in a
-- database, or after the last wallet was deleted, becomes active this
-- way. Once a portfolio is active, only switch changes it.
activateIfNone :: (MonadUnliftIO m) => DBPortfolioId -> DB m Bool
activateIfNone key = do
  current <- activePortfolioId
  case current of
    Just _ -> return False
    Nothing -> setActivePortfolio key >> return True

-- | The active portfolio after a command that may create portfolios:
-- newportfolio, recover and importwallet. The answer of such a command
-- says which portfolio is active, and whether the command made it so: a
-- user who adds a portfolio to a database that has one already keeps
-- working in the one that was active, and is told how to switch.
data ActiveAfter = ActiveAfter
  { activeAfterRef :: !PortfolioRef,
    -- | Whether the command made it active: it created the first
    -- portfolio of the database
    activeAfterActivated :: !Bool
  }
  deriving (Eq, Show)

-- | The keys of the active portfolio after a command in the JSON of its
-- answer: those of the portfolio, and whether the command activated it
instance ToJSON ActiveAfter where
  toJSON after = object (portfolioRefPairs after.activeAfterRef <> ["activated" .= after.activeAfterActivated])

instance FromJSON ActiveAfter where
  parseJSON = withObject "active portfolio" $ \o -> ActiveAfter <$> parsePortfolioRef o <*> o .: "activated"

-- | Run a command that may create portfolios, and give the active
-- portfolio after it, with whether the command activated it. Nothing when
-- the database has no portfolio after the command either.
withActiveAfter :: (MonadUnliftIO m) => ExceptT String (DB m) a -> ExceptT String (DB m) (a, Maybe ActiveAfter)
withActiveAfter action = do
  before <- lift activePortfolioId
  result <- action
  (,) result <$> activeAfterFrom before

-- | The active portfolio now, given the key of the one before a command
-- that may create portfolios: the command activated it when there was
-- none before
activeAfterFrom :: (MonadUnliftIO m) => Maybe DBPortfolioId -> ExceptT String (DB m) (Maybe ActiveAfter)
activeAfterFrom before = do
  after <- activePortfolioRef
  return ((\ref -> ActiveAfter ref (isNothing before)) <$> after)

{- Naming a wallet and a portfolio -}

-- | The wallet of an index the user typed, or a refusal that tells where
-- the wallets are listed, or how to make one in a database without any
-- ('noWallets')
walletNamed :: (MonadUnliftIO m) => Natural -> ExceptT String (DB m) (DBWalletId, DBWallet)
walletNamed index = do
  found <- lift $ walletAt index
  wallets <- lift getWallets
  case found of
    Just held -> return held
    Nothing
      | null wallets -> throwError noWallets
      | otherwise -> throwError ("There is no " <> cs (walletName index) <> ": portfolios lists the wallets")

-- | The portfolio of a wallet at an index the user typed, with the
-- wallet, or a refusal that tells where the portfolios are listed
portfolioNamed :: (MonadUnliftIO m) => (DBWalletId, DBWallet) -> Natural -> ExceptT String (DB m) Portfolio
portfolioNamed (walletId, wallet) index = do
  found <- lift $ portfolioAt walletId index
  case found of
    Just (key, row) -> return (Portfolio key row walletId wallet)
    Nothing -> throwError (cs (walletName (walletIndex wallet)) <> " has no " <> cs (portfolioName index) <> ": portfolios lists the portfolios")

-- | Make the portfolio --wallet and --portfolio name the active one, as
-- switch does, and give it. Without --wallet, the portfolio is one of the
-- wallet of the active portfolio: a user of one mnemonic types the index
-- of a portfolio alone. A wallet or a portfolio the database does not
-- have is refused, and so is any switch in a database without any
-- portfolio ('noWallets').
switchTo :: (MonadUnliftIO m) => Maybe Natural -> Natural -> ExceptT String (DB m) Portfolio
switchTo walletM index = do
  current <- requireActivePortfolio
  wallet <- case walletM of
    Just walletIx -> walletNamed walletIx
    Nothing -> return (current.portfolioWalletId, current.portfolioWallet)
  portfolio <- portfolioNamed wallet index
  lift $ setActivePortfolio portfolio.portfolioId
  return portfolio

-- | The command that makes a portfolio active, as a refusal or a next
-- step tells the user to run it: "switch 1 --wallet 0". It names the
-- wallet always, since the active portfolio may be of another wallet.
switchCommandText :: PortfolioRef -> String
switchCommandText ref = "switch " <> show (refPortfolio ref) <> " --wallet " <> show (refWallet ref)

-- | A portfolio as a sentence names it: "portfolio0 of wallet0"
portfolioText :: Portfolio -> String
portfolioText portfolio =
  cs (portfolioName (portfolioIndex portfolio.portfolioRow)) <> " of " <> cs (walletName (walletIndex portfolio.portfolioWallet))
