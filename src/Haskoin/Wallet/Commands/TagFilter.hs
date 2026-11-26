{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The filters of tags, --tag, --address-tag and --untagged, as the
-- commands read them: the tags a listing keeps its rows by, as its first
-- line names them ('keptTagsOfListing'), and the transactions or the
-- addresses that a statement or a journal keeps ('txSelection',
-- 'addressSelection').
--
-- A word that is no tag of the database is refused here, before anything
-- is listed or written: the user most likely mistyped it. A filter only
-- leaves rows out. It never changes what a row shows.
module Haskoin.Wallet.Commands.TagFilter
  ( keptTagsOfListing,
    txSelection,
    addressSelection,
  )
where

import Control.Monad (forM, guard, join)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Haskoin
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Commands.Scope (listedPools)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Failure (catchDatabaseFailure)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Export.AddressSelection (AddressSelection (..))
import Haskoin.Wallet.Export.TxSelection (TxSelection (..))
import Haskoin.Wallet.Tags (Tag)

-- | The tags a listing command keeps the rows of, as the first line of
-- the listing names them on the screen, each with its colour. Nothing
-- for a listing asked for no tag, and for a command that lists nothing.
-- A word that is no tag of the database is refused here, before the
-- listing shows a line: the user most likely mistyped it
-- ('requireTags').
--
-- addresses and balances keep the addresses that have a tag. coins keeps
-- the coins at such an address: a coin has no tag of its own. txs and
-- pendingtxs keep the transactions that have a tag, with --tag, and
-- those at an address or at a contact that has one, with --address-tag.
-- contacts keeps the contacts that have a tag.
--
-- With --untagged, each keeps what has no tag, and the line says so in
-- the words of its listing: an address or a contact has none, a coin is
-- at an address that has none, and a transaction has none and is at no
-- contact that has one (the type t'Haskoin.Wallet.Database.Tag.TxTagFilter').
keptTagsOfListing :: Config -> Command -> IO (Either String (Maybe KeptTags))
keptTagsOfListing cfg cmd =
  fmap join . catchDatabaseFailure cfg . runDB cfg . runExceptT $ case cmd of
    CommandAddresses _ _ kept _ _ -> keptRowTagsNamed "Addresses" kept
    CommandBalances _ kept _ _ -> keptRowTagsNamed "Addresses" kept
    CommandCoins _ _ kept _ _ -> keptTagsNamed "Coins" [] kept.keptTags ("at addresses without a tag" <$ guard kept.keptUntagged)
    CommandTxs _ kept _ _ -> keptTxTagsNamed "Transactions" kept
    CommandPendingTxs _ kept _ _ -> keptTxTagsNamed "Pending transactions" kept
    CommandContacts _ kept _ _ -> keptRowTagsNamed "Contacts" kept
    _ -> return Nothing

-- | The tags something keeps its rows by, each with its colour, given
-- what it lists, the tags of the rows, the tags of their addresses, and
-- the words for the rows without a tag when it keeps them. Nothing when
-- it keeps every row. A word that is no tag of the database is refused
-- ('requireTags').
keptTagsNamed :: String -> [Tag] -> [Tag] -> Maybe String -> ExceptT String (DB IO) (Maybe KeptTags)
keptTagsNamed noun rowTags atAddresses withoutTag
  | null rowTags && null atAddresses && isNothing withoutTag = return Nothing
  | otherwise = Just <$> (KeptTags noun <$> requireTags rowTags <*> requireTags atAddresses <*> pure withoutTag)

-- | The tags a filter of rows keeps them by, under the name given for
-- what is listed ('keptTagsNamed'): the tags of the rows themselves, and
-- the rows without a tag when it keeps them. addresses, balances,
-- contacts and the balance statement of some addresses all say it in
-- these words.
keptRowTagsNamed :: String -> TagFilter -> ExceptT String (DB IO) (Maybe KeptTags)
keptRowTagsNamed noun kept = keptTagsNamed noun kept.keptTags [] ("without a tag" <$ guard kept.keptUntagged)

-- | The tags a filter of transactions keeps them by, under the name
-- given for what is listed ('keptTagsNamed'). The transactions without
-- a tag it keeps are those left to sort, and are named as such
-- (the type t'Haskoin.Wallet.Database.Tag.TxTagFilter'). txs, pendingtxs, and
-- the statement and the journal of some transactions all say it in
-- these words.
keptTxTagsNamed :: String -> TxTagFilter -> ExceptT String (DB IO) (Maybe KeptTags)
keptTxTagsNamed noun kept =
  keptTagsNamed noun kept.keptTxTags kept.keptAddressTags ("without a tag or a tagged contact" <$ guard kept.keptUntaggedTxs)

-- | The transactions a filter of tags keeps in the portfolios given, on
-- the networks hw shows, for a statement or a journal of some
-- transactions ("Haskoin.Wallet.Export.TxSelection"). Nothing for a filter
-- that keeps every transaction. A word that is no tag of the database
-- is refused, as a listing refuses it.
--
-- The database is asked about each portfolio on each network with the
-- condition of txs ('txidsKept'), in the transaction the caller runs.
-- So the selection is what txs lists with the same options in each
-- portfolio, and it agrees with the history the caller reads next.
txSelection :: Config -> [Portfolio] -> TxTagFilter -> ExceptT String (DB IO) (Maybe TxSelection)
txSelection cfg portfolios kept = do
  named <- keptTxTagsNamed "Transactions" kept
  forM named $ \tags -> do
    found <- forM portfolios $ \portfolio -> do
      pools <- listedPools cfg portfolio Nothing
      forM pools $ \pool -> do
        txids <- txidsKept pool kept
        return [(portfolioRef portfolio, pool.poolNetwork.name, txid) | txid <- txids]
    return (TxSelection (keptTagsText tags) (Set.fromList (concat (concat found))))

-- | The addresses a filter of tags keeps in the portfolios given, on the
-- networks hw shows, for a balance statement of some addresses
-- ("Haskoin.Wallet.Export.AddressSelection"). Nothing for a filter that keeps
-- every address. A word that is no tag of the database is refused, as a
-- listing refuses it.
--
-- Only the addresses that have a tag are read, each with its tags
-- ('accountAddressTags'): the selection takes any other address for one
-- without a tag.
addressSelection :: Config -> [Portfolio] -> TagFilter -> ExceptT String (DB IO) (Maybe AddressSelection)
addressSelection cfg portfolios kept = do
  named <- keptRowTagsNamed "Addresses" kept
  forM named $ \tags -> do
    found <- forM portfolios $ \portfolio -> do
      pools <- listedPools cfg portfolio Nothing
      forM pools $ \pool -> do
        accounts <- lift (poolAccounts pool)
        tagged <- lift (concat <$> mapM (accountAddressTags . fst) accounts)
        return [((portfolioRef portfolio, pool.poolNetwork.name, address), itsTags) | ((_, _, address), itsTags) <- tagged]
    return (AddressSelection (keptTagsText tags) kept (Map.fromList (concat (concat found))))
