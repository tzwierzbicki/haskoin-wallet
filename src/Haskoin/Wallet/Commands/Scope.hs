{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Where a command works: the accounts and the pools of a portfolio that it
-- covers, and what it keeps of the things the user named.
--
-- A command works in the active portfolio, which the database keeps and
-- switch changes. Inside it, --network names what the portfolio holds on
-- one network, its pool, and --type one of the two accounts of the pool. A
-- listing covers the whole portfolio unless these options narrow it
-- ('listedAccounts', 'listedPools').
--
-- Some commands find their portfolio by themselves, from what they are
-- given: a transaction, a coin, an address, the destinations of a sweep.
-- When several portfolios are candidates, the active one is taken when it
-- is one of them; otherwise the refusal names them, for the user to switch
-- to the one to use ('poolAmong'). Their answers carry the active
-- portfolio, so that the screen says when the portfolio shown is another
-- one, whose paths and labels the user might take for those of the active
-- one ("Haskoin.Wallet.Cli.Response").
--
-- A portfolio has accounts on eight networks, and the configuration lists
-- those hw shows ("Haskoin.Wallet.Config"). The others are hidden, not
-- disabled. sync and recover work on every network, and everything else
-- keeps to the shown networks. A listing has no section for a hidden
-- network, and an answer no line. A command that looks for what the user
-- named looks on the shown networks, so that a hidden network is never one
-- of several to tell apart with --network. What the wallet has on hidden
-- networks alone is refused, with the command that shows its network
-- ('shownAmong'): answering that the wallet does not have it would be
-- untrue. A --network that names a hidden network is refused before the
-- command runs ('Haskoin.Wallet.Commands.refusedNetwork').
module Haskoin.Wallet.Commands.Scope
  ( listedAccounts,
    shownAmong,
    byNetwork,
    poolAmong,
    listedPools,
  )
where

import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.List (nub)
import Haskoin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys

-- | The accounts of the portfolio a listing covers: every one on the
-- networks hw shows, or only those on --network, or of --type. A type the
-- network does not have is refused. Every other choice keeps an account:
-- a portfolio has every account of every network.
listedAccounts :: Config -> Portfolio -> Maybe Network -> Maybe AccountKind -> ExceptT String (DB IO) [(DBAccountId, DBAccount)]
listedAccounts cfg portfolio netM kindM = do
  for_ ((,) <$> netM <*> kindM) $ liftEither . uncurry checkKindOnNetwork
  accounts <- lift $ portfolioAccountRows portfolio.portfolioId
  return
    [ held
      | held@(_, acc) <- onShownNetworks cfg (accountNetwork . snd) accounts,
        maybe True (== accountNetwork acc) netM,
        maybe True (== accountKind acc) kindM
    ]

-- | What the user named, among where the wallet has it: the elements on
-- the networks hw shows, given the network of each. When the wallet has
-- it on hidden networks alone, the refusal names them, with the command
-- that shows one ('onHiddenNetworks'). Nothing found at all is left to
-- the caller, which says so in its own words. The text says what was
-- named, as in "The coin a1075db5:1".
shownAmong :: Config -> String -> (a -> Network) -> [a] -> ExceptT String (DB IO) [a]
shownAmong cfg what networkOf found =
  case (onShownNetworks cfg networkOf found, found) of
    ([], _ : _) -> throwError (onHiddenNetworks what (nub (networkOf <$> found)))
    (shown, _) -> return shown

-- | The accounts a listing covers, by network, in the order of the
-- networks of a portfolio: what the portfolio holds on each network is
-- listed together
byNetwork :: [(DBAccountId, DBAccount)] -> [(Network, [(DBAccountId, DBAccount)])]
byNetwork accounts =
  [ (net, [held | held@(_, acc) <- accounts, accountNetwork acc == net])
    | net <- nub (accountNetwork . snd <$> accounts)
  ]

-- | The one pool a command works on among those that hold what it names:
-- the destinations of a sweep. The pools are those on the networks hw
-- shows, which the caller narrowed them to ('shownAmong'). They are
-- narrowed to the --network, when given. When what is left is on several
-- networks, as the same key is on each test network, the --network is
-- asked for. The default network does not choose between them: a sweep
-- on the wrong one takes the coins of another chain, and the README
-- promises a refusal. When it
-- is in several portfolios, the pool of the active portfolio is taken, when
-- it is one of them: the user who switched to a portfolio means that one.
-- Otherwise the refusal names them, for the user to switch to the one to
-- use. The first text says what was named, as in "The destinations of the
-- sweep".
poolAmong :: Maybe Network -> String -> [Pool] -> ExceptT String (DB IO) Pool
poolAmong netM what held = do
  activeM <- lift activePortfolioId
  let fitting = nub (filter (\pool -> maybe True (== pool.poolNetwork) netM) held)
      networks = nub ((.poolNetwork) <$> fitting)
  case (fitting, netM) of
    ([only], _) -> return only
    ([], Just net) -> throwError (what <> " are on " <> networksText (nub ((.poolNetwork) <$> held)) <> ", not on the --network " <> net.name)
    _
      | length networks > 1 -> throwError (what <> " are on " <> networksText networks <> ": give the --network")
      | [ofActive] <- filter ((== activeM) . Just . (.poolPortfolio)) fitting -> return ofActive
      | otherwise -> do
          places <- mapM poolPlace fitting
          throwError (what <> " are in several portfolios, " <> intercalateAnd (placeText <$> places) <> ": switch to the one to use first")

-- | The pools of a portfolio a listing of transactions covers: every
-- network of the portfolio hw shows, or the --network alone
listedPools :: Config -> Portfolio -> Maybe Network -> ExceptT String (DB IO) [Pool]
listedPools cfg portfolio netM = do
  pools <- lift $ portfolioPools portfolio.portfolioId
  return [pool | pool <- onShownNetworks cfg (.poolNetwork) pools, maybe True (== pool.poolNetwork) netM]
