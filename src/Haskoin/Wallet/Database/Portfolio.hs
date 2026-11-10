{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The wallets and their portfolios.
--
-- A wallet is the master key of one mnemonic. A portfolio holds every
-- account of a wallet at one account index, on every network and of
-- every kind. The user of one mnemonic works in portfolio 0 of wallet 0
-- and never names an account: a command is told the network and the type
-- of address it works on, and the rest follows.
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
-- This module holds the rows of the two tables, the portfolio a command
-- works in as its options and the configuration say ('choosePortfolio'),
-- and how the answers name a portfolio, a network of it, and an account
-- of it ('PortfolioRef', 'Place', 'AccountRef'). The accounts themselves
-- are in "Haskoin.Wallet.Database.Account".
module Haskoin.Wallet.Database.Portfolio where

import Conduit (MonadUnliftIO)
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Char (isControl)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Config
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

-- | The fingerprint of the master key of a wallet, when hw knows it. It
-- does not for a wallet made from the accounts of hw 0.9, until the
-- wallet is filled in.
walletFingerprint :: DBWallet -> Maybe Fingerprint
walletFingerprint wallet = eitherToMaybe . textToFingerprint =<< dBWalletFingerprint wallet

-- | Every wallet, in the order of their indexes
getWallets :: (MonadUnliftIO m) => DB m [(DBWalletId, DBWallet)]
getWallets = map entityPair <$> P.selectList [] [P.Asc DBWalletIndex]

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

-- | The wallet whose master key has the fingerprint, if there is one
walletWithFingerprint :: (MonadUnliftIO m) => Fingerprint -> DB m (Maybe (DBWalletId, DBWallet))
walletWithFingerprint fingerprint =
  fmap entityPair <$> P.selectFirst [DBWalletFingerprint P.==. Just (fingerprintToText fingerprint)] []

-- | A new wallet, at the lowest free index, with the fingerprint of its
-- master key when known, and no description
insertWallet :: (MonadUnliftIO m) => Maybe Fingerprint -> DB m (DBWalletId, DBWallet)
insertWallet fingerprintM = do
  taken <- map (walletIndex . snd) <$> getWallets
  time <- liftIO getCurrentTime
  let wallet =
        DBWallet
          { dBWalletIndex = fromIntegral (lowestFree taken),
            dBWalletFingerprint = fingerprintToText <$> fingerprintM,
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
-- 2^31 ('indexFits'), with a description
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

-- | The fingerprint of the seed of the portfolio, that of its wallet,
-- when hw knows it
portfolioFingerprint :: Portfolio -> Maybe Fingerprint
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

{- The portfolio a command works in -}

-- | The --wallet and --portfolio of a command, as the user typed them.
-- Each is an index. The one left out takes the default of the
-- configuration ('chooseWallet', 'choosePortfolio'), which the parser
-- does not read.
data PortfolioChoice = PortfolioChoice
  { chosenWallet :: !(Maybe Natural),
    chosenPortfolio :: !(Maybe Natural)
  }
  deriving (Eq, Show)

-- | No --wallet and no --portfolio: the defaults
defaultPortfolio :: PortfolioChoice
defaultPortfolio = PortfolioChoice Nothing Nothing

-- | The wallet a command works on: the one --wallet names by its index,
-- or the default wallet of the configuration, wallet0 unless the key
-- default-wallet says otherwise. A wallet that does not exist is
-- refused. The refusal of a default names the key, so that the user
-- knows where the index came from.
chooseWallet :: (MonadUnliftIO m) => Config -> Maybe Natural -> ExceptT String (DB m) (DBWalletId, DBWallet)
chooseWallet cfg given = do
  wallets <- lift getWallets
  when (null wallets) $ throwError noWallets
  let index = fromMaybe (configDefaultWallet cfg) given
  case filter ((== index) . walletIndex . snd) wallets of
    found : _ -> return found
    [] -> throwError $ case given of
      Just _ -> "There is no " <> cs (walletName index) <> ": portfolios lists the wallets"
      Nothing ->
        "The key default-wallet of the configuration file names "
          <> cs (walletName index)
          <> ", which does not exist: give --wallet, or change the key with setdefault"

-- | The refusal of a command that needs a wallet when the database has
-- none
noWallets :: String
noWallets = "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"

-- | The portfolio a command works in: the one --portfolio names by its
-- index, in the wallet 'chooseWallet' gives, or the default portfolio of
-- the configuration, portfolio0 unless the key default-portfolio says
-- otherwise. A portfolio the wallet does not have is refused, and the
-- refusal of a default names the key.
choosePortfolio :: (MonadUnliftIO m) => Config -> PortfolioChoice -> ExceptT String (DB m) Portfolio
choosePortfolio cfg choice = do
  (walletId, wallet) <- chooseWallet cfg choice.chosenWallet
  let index = fromMaybe (configDefaultPortfolio cfg) choice.chosenPortfolio
      name = cs (walletName (walletIndex wallet)) :: String
  found <- lift $ portfolioAt walletId index
  case found of
    Just (key, row) -> return (Portfolio key row walletId wallet)
    Nothing -> throwError $ case choice.chosenPortfolio of
      Just _ -> name <> " has no " <> cs (portfolioName index) <> ": portfolios lists the portfolios"
      Nothing ->
        "The key default-portfolio of the configuration file names "
          <> cs (portfolioName index)
          <> ", which "
          <> name
          <> " does not have: give --portfolio, or change the key with setdefault"

-- | The portfolios a command that covers several works on: the one
-- --portfolio names, in the wallet --wallet names or the default one;
-- every portfolio of the wallet --wallet names; or, without either
-- option, every portfolio of every wallet. sync, portfolios and
-- exportsheet work on these: looking at one portfolio too many does no
-- harm, and a user of several wallets wants them all synced.
choosePortfolios :: (MonadUnliftIO m) => Config -> PortfolioChoice -> ExceptT String (DB m) [Portfolio]
choosePortfolios cfg choice = case (choice.chosenWallet, choice.chosenPortfolio) of
  (Nothing, Nothing) -> lift getAllPortfolios
  (_, Just _) -> pure <$> choosePortfolio cfg choice
  (Just _, Nothing) -> lift . walletPortfolios =<< chooseWallet cfg choice.chosenWallet

{- The portfolios a command that finds its portfolio may take -}

-- | The portfolios --wallet and --portfolio leave to a command that finds
-- its portfolio by itself, from a transaction or from addresses, with how
-- a refusal names them.
--
-- Such a command looks in every portfolio when neither option is given.
-- --wallet alone leaves every portfolio of that wallet. --portfolio leaves
-- one portfolio, of the default wallet without --wallet. So the options
-- only narrow the search, and never pick a default portfolio: the user
-- who types --wallet 0 means "somewhere in wallet0", not "in portfolio0 of
-- wallet0". A refusal names what the user typed: "wallet0" for --wallet
-- alone, "portfolio1 of wallet0" with --portfolio. 'choosePortfolio' is
-- for the other commands, which work in one portfolio with its defaults.
data Narrowed = Narrowed
  { -- | The portfolios left, in the order of their indexes
    narrowedPortfolios :: ![Portfolio],
    -- | How a sentence names them: "wallet0", or "portfolio1 of wallet0"
    narrowedText :: !String
  }
  deriving (Eq, Show)

-- | The portfolios the options of a command that finds its portfolio by
-- itself leave, as 'Narrowed' says. Nothing without either option: every
-- portfolio is left. A wallet or a portfolio the database does not have
-- is refused, as 'chooseWallet' and 'choosePortfolio' refuse it.
narrowedBy :: (MonadUnliftIO m) => Config -> PortfolioChoice -> ExceptT String (DB m) (Maybe Narrowed)
narrowedBy cfg choice = case (choice.chosenWallet, choice.chosenPortfolio) of
  (Nothing, Nothing) -> return Nothing
  (_, Just _) -> Just . onePortfolio <$> choosePortfolio cfg choice
  (Just _, Nothing) -> do
    held@(_, wallet) <- chooseWallet cfg choice.chosenWallet
    portfolios <- lift (walletPortfolios held)
    return (Just (Narrowed portfolios (cs (walletName (walletIndex wallet)))))

-- | One portfolio alone, as --portfolio leaves it, named in full
onePortfolio :: Portfolio -> Narrowed
onePortfolio portfolio = Narrowed [portfolio] (portfolioText portfolio)

-- | Whether the options leave the portfolio of the key: any portfolio
-- when they leave every one
allowedBy :: Maybe Narrowed -> DBPortfolioId -> Bool
allowedBy narrowedM key = maybe True (elem key . map (.portfolioId) . (.narrowedPortfolios)) narrowedM

-- | A portfolio as a sentence names it: "portfolio0 of wallet0"
portfolioText :: Portfolio -> String
portfolioText portfolio =
  cs (portfolioName (portfolioIndex portfolio.portfolioRow)) <> " of " <> cs (walletName (walletIndex portfolio.portfolioWallet))
