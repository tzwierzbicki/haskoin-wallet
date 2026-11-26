{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The coins of a portfolio: the listing coins, and lockcoin and
-- unlockcoin, which keep a coin out of the payments and give it back
-- ("Haskoin.Wallet.Database.Coin").
module Haskoin.Wallet.Commands.Coin
  ( coinsListing,
    cmdLockCoin,
    cmdUnlockCoin,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Haskoin
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (SectionSync (..), coinAnswer, groupOf, grouped, sectionSync)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.Scope (byNetwork, listedAccounts)
import Haskoin.Wallet.Commands.ShortId (coinNamed, walletCoinOf)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.ShortId

-- | The coins of a portfolio, or its locked ones, a section for each
-- network hw shows, or for the one --network keeps, in the order coins
-- lists them, or in its reverse with --reverse, the oldest coin first.
-- --type keeps the coins of one type. A coin names the one to
-- go on after, with its block: the listing starts in the section of its
-- network.
--
-- With a filter of tags, the listing keeps the coins at an address that
-- has any one of the tags, and those at an address without a tag when
-- asked for. A coin has no tag of its own: it is in the groups of its
-- address, which is what --spend-from-tag spends from.
--
-- The section of a network where the portfolio never synced says so
-- (the type t'SectionSync').
coinsListing :: Config -> Portfolio -> Target -> Bool -> TagFilter -> Paging CoinRef -> ExceptT String (DB IO) Sections
coinsListing cfg portfolio target locked kept paging = do
  _ <- requireTags kept.keptTags
  accounts <- listedAccounts cfg portfolio target.network target.kind
  let networks = byNetwork accounts
      shown = if locked then LockedCoins else AllCoins
  sections <- forM networks $ \(net, onNet) -> do
    state <- sectionSync portfolio net onNet
    return $ \startM ->
      Listing
        { listingStart = startM,
          listingRows = listedCoinsAfter (listedOrder paging) (portfolioPool portfolio net) (fst <$> onNet) shown kept,
          listingCursor = Right . snd,
          listingAnswer = grouped state ResponseCoins (groupOf portfolio net onNet state) . map fst,
          listingWhole = False,
          listingShownEmpty = state.sectionShownNotSynced
        }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (sections, Nothing)
    Just named -> do
      outpoint <- coinNamed ("The coin " <> coinRefText named <> " is not among those listed") named
      blocks <- forM networks $ \(_, onNet) -> lift (runExceptT (coinBlock (fst <$> onNet) outpoint))
      case [(i, block) | (i, Right block) <- zip [0 :: Int ..] blocks] of
        (i, block) : _ -> return (drop i sections, Just (block, outpoint))
        [] -> throwError ("The coin " <> coinRefText named <> " is not among those listed")
  return $ Sections (length networks > 1) paging.pagingMax (zipWith ($) from (startM : repeat Nothing))

-- | Lock a coin at the user's request, and show it. A coin the wallet has
-- on several networks is the one on the --network, or on the default
-- network ('walletCoin').
cmdLockCoin :: Config -> CoinRef -> Maybe Network -> IO Response
cmdLockCoin cfg named netM =
  runDBResponse cfg $ do
    held <- walletCoin cfg netM =<< walletCoinOf named
    coinAnswer held =<< lockCoin held

-- | Unlock a coin the user locked, and show it
cmdUnlockCoin :: Config -> CoinRef -> Maybe Network -> IO Response
cmdUnlockCoin cfg named netM =
  runDBResponse cfg $ do
    held <- walletCoin cfg netM =<< walletCoinOf named
    coinAnswer held =<< unlockCoin held
