{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | The answers the commands share: a portfolio, an address, a coin and a
-- transaction as a 'Response' shows them, and the group of a section of a
-- listing.
--
-- An answer is read from the database once the command has done its work,
-- so that it shows what the command changed. The address, the coin or the
-- transaction may be of another portfolio than the active one, when the
-- command found it from what the user named. So these answers carry the
-- active portfolio, for the screen to say when the portfolio shown is
-- another one ("Haskoin.Wallet.Cli.Response").
module Haskoin.Wallet.Commands.Answer
  ( viewOfPortfolio,
    walletHiddenUse,
    fundsAt,
    addressAnswer,
    retaggedAnswer,
    txAnswer,
    seenFromPlace,
    SectionSync (..),
    sectionSync,
    groupOf,
    grouped,
    coinAnswer,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Trans.Class (lift)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Cli.PrettyPrinter (SeenFrom (..))
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock (poolEverSynced)
import Haskoin.Wallet.Database.ChangeChain (ChainRetagged (..), ChainStop (..))
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tags (ShownTag (..))
import Numeric.Natural (Natural)

-- | A portfolio as the answers show it, with what its coins are worth
-- and what it can spend now on each network hw shows. Its hidden
-- networks are left out of every view of it, and of its JSON. The view
-- only says whether the portfolio is used on one of them
-- ('viewUsedOnHidden'), without a name.
viewOfPortfolio :: Config -> Portfolio -> ExceptT String (DB IO) PortfolioView
viewOfPortfolio cfg portfolio = do
  view <- portfolioView portfolio coinTotals
  return
    view
      { viewNetworks = onShownNetworks cfg networkViewNetwork (viewNetworks view),
        viewUsedOnHidden = not (null (hiddenUse cfg view))
      }

-- | The hidden networks a portfolio is used on, given its view on every
-- network: those where it has addresses or coins ('networkInUse')
hiddenUse :: Config -> PortfolioView -> [Network]
hiddenUse cfg view = [networkViewNetwork net | net <- viewNetworks view, not (isShown cfg (networkViewNetwork net)), networkInUse net]

-- | The portfolios of a wallet that are used on hidden networks, each
-- with those networks: what recover tells of them
walletHiddenUse :: Config -> Natural -> ExceptT String (DB IO) [(PortfolioRef, [Network])]
walletHiddenUse cfg wallet = do
  portfolios <- lift . walletPortfolios =<< walletNamed wallet
  used <- forM portfolios $ \portfolio -> (portfolioRef portfolio,) . hiddenUse cfg <$> portfolioView portfolio coinTotals
  return [use | use@(_, _ : _) <- used]

-- | What hw knows of the funds of an address of a pool, given whether
-- the pool was ever synced and the totals of the coins read for its
-- addresses. The totals are those of the address, nothing when it holds
-- no coin. Before the first sync, they are not known: hw never asked
-- (the type t'AddressFunds').
fundsAt :: Bool -> Map Text CoinTotals -> DBAddress -> AddressFunds
fundsAt everSynced totals addr
  | everSynced = Holds (Map.findWithDefault mempty (dBAddressAddress addr) totals)
  | otherwise = NotSynced

-- | The answer showing an address of the account, with what hw knows of
-- its funds (@addressShown@) and its tags. The account may be of another portfolio than the active one:
-- show and label find the address in every portfolio.
addressAnswer :: DBAccountId -> DBAddress -> ExceptT String (DB IO) Response
addressAnswer = retaggedAnswer AddTags Nothing

-- | The answer of tag and of untag about an address of the account: the
-- address, as 'addressAnswer' shows it, then, when the command went down
-- the chain of its payments (--with-change), the change addresses that
-- followed it, each read again as it is now, and where the tags stopped
-- (@stopShown@). There may be none, and the answer then says so: the user
-- asked for them.
retaggedAnswer :: Retag -> Maybe ChainRetagged -> DBAccountId -> DBAddress -> ExceptT String (DB IO) Response
retaggedAnswer retag followedM accId addr = do
  acc <- getAccountById accId
  ref <- accountRefOf acc
  (_, funds, tags) <- addressShown addr
  changeM <- forM followedM $ \followed ->
    ChangeFollowed retag <$> mapM addressShown followed.chainChanged <*> mapM stopShown followed.chainStops
  ResponseAddress ref addr funds tags changeM <$> activePortfolioRef
  where
    stopShown :: ChainStop -> ExceptT String (DB IO) ChangeStop
    stopShown stop =
      ChangeStop
        <$> addressShown stop.stopAt
        <*> lift (shownTags stop.stopTags)
        <*> pure stop.stopPayment
        <*> mapM addressShown stop.stopSource

-- | An address as an answer shows it: with what hw knows of its funds,
-- the totals of its coins, what they are worth and what it can spend
-- now, or that its pool was never synced ('fundsAt'), and its tags
addressShown :: DBAddress -> ExceptT String (DB IO) (DBAddress, AddressFunds, [ShownTag])
addressShown addr = do
  acc <- getAccountById (dBAddressAccount addr)
  everSynced <- lift $ poolEverSynced (accountPool acc)
  totals <- coinTotalsAtAddresses (accountNetwork acc) (dBAddressAccount addr) [dBAddressAddress addr]
  tags <- lift $ addressTags addr
  return (addr, fundsAt everSynced totals addr, tags)

-- | The answer showing a transaction of a portfolio on a network. The
-- portfolio may be another one than the active one, when the command
-- found it from the transaction.
txAnswer :: Pool -> TxInfo -> ExceptT String (DB IO) Response
txAnswer pool info = ResponseTx <$> poolPlace pool <*> pure info <*> activePortfolioRef

-- | The portfolio of a place, with the active portfolio, as the view of a
-- transaction names them: what signtx and broadcast show before they ask
seenFromPlace :: Place -> ExceptT String (DB IO) SeenFrom
seenFromPlace place = SeenFrom (placePortfolio place) <$> activePortfolioRef

-- | How a listing counts the addresses of an account, for the widths of
-- its paths
accountCounts :: DBAccount -> AccountCounts
accountCounts acc = AccountCounts (accountKind acc) (fromIntegral (dBAccountExternal acc)) (fromIntegral (dBAccountInternal acc))

-- | How a section of a listing stands with the sync of its network.
--
-- A network where the portfolio never synced has no coin and no
-- transaction in the database, so its section of balances, coins or txs
-- has no row, and a section without rows shows nothing. The listing
-- would then say that there is nothing to display, of addresses hw
-- asked no server about. So such a section shows without a row, and
-- says that the network is not synced, when the accounts it covers have
-- an address there. A network without an address stays out, as
-- portfolios leaves it out: most users never touch most networks.
data SectionSync = SectionSync
  { -- | Whether a sync of the portfolio on the network ever finished
    sectionEverSynced :: !Bool,
    -- | Whether the section shows without a row, to say that its network
    -- is not synced ('Haskoin.Wallet.Cli.Listing.listingShownEmpty')
    sectionShownNotSynced :: !Bool
  }

-- | How the section of the accounts of a portfolio on a network stands
-- with the sync
sectionSync :: Portfolio -> Network -> [(DBAccountId, DBAccount)] -> ExceptT String (DB IO) SectionSync
sectionSync portfolio net accounts = do
  everSynced <- lift $ poolEverSynced (portfolioPool portfolio net)
  return (SectionSync everSynced (not everSynced && any (hasAddresses . snd) accounts))
  where
    hasAddresses acc = dBAccountExternal acc + dBAccountInternal acc > 0

-- | The group of a listing for the accounts of a portfolio on a network,
-- given how its section stands with the sync
groupOf :: Portfolio -> Network -> [(DBAccountId, DBAccount)] -> SectionSync -> [row] -> Group row
groupOf portfolio net accounts state = Group (portfolioPlace portfolio net) (accountCounts . snd <$> accounts) state.sectionEverSynced

-- | The answer of a section of a listing: its rows in their group, or an
-- answer without a group when it has no row, so that a section without
-- rows adds nothing to the whole listing. A section that shows as not
-- synced answers with its group, though it has no row.
grouped :: SectionSync -> ([group] -> Response) -> ([row] -> group) -> [row] -> Response
grouped state answer group rows
  | null rows && not state.sectionShownNotSynced = answer []
  | otherwise = answer [group rows]

-- | The answer showing a coin, with the account that holds it. The account
-- may be of another portfolio than the active one: show, lockcoin and
-- unlockcoin find the coin in every portfolio.
coinAnswer :: (Network, DBCoin) -> (Place, JsonCoin) -> ExceptT String (DB IO) Response
coinAnswer (_, row) (_, coin) = do
  ref <- accountRefOf =<< getAccountById (dBCoinAccount row)
  ResponseCoin ref coin <$> activePortfolioRef
