{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The addresses of a portfolio: receive, which gives out the next
-- receiving address, and the listings addresses and balances. show and
-- label work on one address the user names, in whichever portfolio holds
-- it ("Haskoin.Wallet.Commands.Show", "Haskoin.Wallet.Commands.Label").
module Haskoin.Wallet.Commands.Address
  ( cmdReceive,
    addressesListing,
    balancesListing,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (SectionSync (..), addressAnswer, fundsAt, groupOf, grouped, sectionSync)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.Scope (byNetwork, listedAccounts)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock (poolEverSynced)
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag

-- | Give out the next receiving address of the account --network and
-- --type pick in the active portfolio ('chooseAccount'), with the label
-- and the tags given for it
cmdReceive :: Config -> Receive -> IO Response
cmdReceive cfg opts =
  runDBResponse cfg $ do
    portfolio <- requireActivePortfolio
    (accId, _) <- chooseAccount cfg portfolio opts.target.network opts.target.kind
    addr <- genExtAddress cfg accId (fromMaybe "" opts.label)
    lift $ tagAddress addr opts.tags
    addressAnswer accId addr

-- | The addresses of a chain of a portfolio, a section for each of its
-- accounts on the networks hw shows, or for those --network and --type
-- keep, in the order of the networks and of the types. In each section
-- the newest come first, or the first address with --reverse, each with
-- what hw knows of its funds: the totals of its coins, what they are
-- worth and what it can spend of them now, or that the portfolio was
-- never synced on the network ('fundsAt'). An address names the one to go on after: the listing starts in the section
-- of its account. An address the test networks share is in the section of
-- each, and is refused without --network ('sectionOfAddress').
--
-- With a filter of tags, the listing keeps the addresses that have any
-- one of the tags, and those without a tag when asked for
-- ('keptAddressesAfter'). Each address comes with its tags, all of them,
-- kept or not.
addressesListing :: Config -> Portfolio -> Target -> Bool -> TagFilter -> Paging Text -> ExceptT String (DB IO) Sections
addressesListing cfg portfolio target internal kept paging = do
  _ <- requireTags kept.keptTags
  accounts <- listedAccounts cfg portfolio target.network target.kind
  -- An account without an address shows nothing, synced or not: its
  -- addresses say themselves that they are not synced
  synced <- forM accounts $ \(_, acc) -> (`SectionSync` False) <$> lift (poolEverSynced (accountPool acc))
  let addrType = if internal then AddrInternal else AddrExternal
      listed = zip accounts synced
      section startM ((accId, acc), state) =
        let net = accountNetwork acc
         in Listing
              { listingStart = startM,
                listingRows = \afterM countM -> do
                  addrs <- lift $ keptAddressesAfter (listedOrder paging) accId addrType kept afterM countM
                  totals <- coinTotalsAtAddresses net accId (dBAddressAddress <$> addrs)
                  tagsOf <- lift $ tagsOfAddressRows addrs
                  return [(addr, fundsAt state.sectionEverSynced totals addr, tagsOf addr) | addr <- addrs],
                listingCursor = \(addr, _, _) -> Right (dBAddressIndex addr),
                listingAnswer = grouped state ResponseAddresses (groupOf portfolio net [(accId, acc)] state),
                listingWhole = False,
                listingShownEmpty = False
              }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (listed, Nothing)
    Just text -> do
      tried <- forM accounts $ \(accId, acc) -> lift (runExceptT (listedAddress (accountNetwork acc) accId addrType text))
      found <- liftEither $ sectionOfAddress text (zip (accountNetwork . snd <$> accounts) (either (const Nothing) Just <$> tried))
      case (found, tried) of
        (Just (i, row), _) -> return (drop i listed, Just (dBAddressIndex row))
        -- The one account of the listing says why the address is not its own
        (Nothing, [Left refusal]) -> throwError refusal
        (Nothing, _) -> throwError ("The address " <> cs text <> " is not among the addresses listed")
  return $ Sections (length accounts > 1) paging.pagingMax (zipWith section (startM : repeat Nothing) from)

-- | The addresses of a portfolio that hold coins, receiving and change
-- addresses alike, each with the balance of its coins, a section for each
-- network hw shows, or for the one --network keeps. --type keeps the
-- addresses of one type. Each section comes with the total of its
-- network, and lists the largest balance first, or the smallest with
-- --reverse. An address names the one to go on after, with the balance it
-- holds now: the listing starts in the section of its network. An address
-- the test networks share may hold coins on several, and is then refused
-- without --network ('sectionOfAddress').
--
-- With a filter of tags, the listing keeps the addresses that have any
-- one of the tags, and those without a tag when asked for. The total of
-- each network adds up those addresses alone: what the portfolio holds
-- under the tags. So balances --tag savings answers how much the savings
-- hold, and balances --untagged how much is in no group.
--
-- The section of a network where the portfolio never synced says so
-- (the type t'SectionSync').
balancesListing :: Config -> Portfolio -> Target -> TagFilter -> Paging Text -> ExceptT String (DB IO) Sections
balancesListing cfg portfolio target kept paging = do
  _ <- requireTags kept.keptTags
  accounts <- listedAccounts cfg portfolio target.network target.kind
  let networks = byNetwork accounts
  sections <- forM networks $ \(net, onNet) -> do
    let accIds = fst <$> onNet
    -- The total of the network, over every address the section covers
    total <- foldMap snd <$> coinBalancesAfter UsualOrder net accIds kept Nothing Nothing
    state <- sectionSync portfolio net onNet
    return $ \startM ->
      Listing
        { listingStart = startM,
          listingRows = \afterM countM -> do
            rows <- coinBalancesAfter (listedOrder paging) net accIds kept afterM countM
            tagsOf <- lift $ tagsOfAddressRows (fst <$> rows)
            return [(addr, balance, tagsOf addr) | (addr, balance) <- rows],
          listingCursor = \(addr, balance, _) -> Right (addr, balance),
          listingAnswer = grouped state ResponseBalances (\rows -> (groupOf portfolio net onNet state rows, total)),
          listingWhole = False,
          listingShownEmpty = state.sectionShownNotSynced
        }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (sections, Nothing)
    Just text -> do
      held <- forM networks $ \(net, onNet) ->
        if isJust (textToAddr net text) then (net,) . listToMaybe <$> coinBalancesAt net (fst <$> onNet) [text] else return (net, Nothing)
      found <- liftEither $ sectionOfAddress text held
      case found of
        Just (i, row) -> return (drop i sections, Just row)
        Nothing -> throwError $ "The address " <> cs text <> " holds no coins among those listed"
  return $ Sections (length networks > 1) paging.pagingMax (zipWith ($) from (startM : repeat Nothing))
