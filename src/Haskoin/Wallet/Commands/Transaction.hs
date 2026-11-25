{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The transactions of the wallet: the listings txs and pendingtxs, and
-- importtx, exporttx and deletetx, which bring a pending transaction in
-- from a file, write one to a file, and delete one. A payment is prepared
-- in "Haskoin.Wallet.Commands.Payment", signed in
-- "Haskoin.Wallet.Commands.Signing" and sent in
-- "Haskoin.Wallet.Commands.Send".
module Haskoin.Wallet.Commands.Transaction
  ( txsListing,
    pendingTxsListing,
    cmdImportTx,
    cmdExportTx,
    cmdDeleteTx,
  )
where

import Control.Monad (forM, when, (<=<))
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (genericTake, sortOn)
import qualified Data.Map.Strict as Map
import Haskoin
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (SectionSync (..), groupOf, grouped, sectionSync, txAnswer)
import Haskoin.Wallet.Commands.Deletion (confirmDeletion, deleteTxNotice)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.Scope (listedPools)
import Haskoin.Wallet.Commands.ShortId (chainTxidOf, pendingHashOf, pendingTxNamed)
import Haskoin.Wallet.Commands.TxFile (TxFileRead (..), notInWallet, readTxFileIn, writeTxFile)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Store
import Haskoin.Wallet.Tags (ShownTag (..))
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (labelledTxFile)

-- | The transactions of a portfolio on the chain, a section for each
-- network hw shows, or for the one --network keeps, in the order txs
-- lists them, each with the balance after it on its network. A
-- transaction names the
-- one to go on after, with where it stands in txs ('Standing') and the
-- balance so far, so that a chunk adds up nothing. Only --after adds up the transactions above it.
-- With --reverse each section starts with the oldest transaction, and
-- each transaction keeps the balance it has in the usual order.
-- The listing starts in the section of the first network that has the
-- transaction.
--
-- With a filter of tags, the listing keeps the transactions that have
-- one of the tags asked for, and those at an address that has one of the
-- tags of addresses asked for, an address of the portfolio or a contact
-- of the address book, and those left to sort when asked for
-- ('keptTxsAfter'). A filter only leaves transactions out: each one left
-- shows the balance it shows in the whole listing, that of the portfolio
-- after it.
--
-- The section of a network where the portfolio never synced says so
-- (the type t'SectionSync').
txsListing :: Config -> Portfolio -> Maybe Network -> TxTagFilter -> Paging HashRef -> ExceptT String (DB IO) Sections
txsListing cfg portfolio netM kept paging = do
  _ <- requireTags (kept.keptTxTags <> kept.keptAddressTags)
  pools <- listedPools cfg portfolio netM
  sections <- forM pools $ \pool -> do
    accounts <- lift $ poolAccounts pool
    state <- sectionSync portfolio pool.poolNetwork accounts
    return $ \startM ->
      Listing
        { listingStart = startM,
          listingRows = keptTxsAfter (listedOrder paging) pool kept,
          listingCursor = Right . snd,
          listingAnswer = grouped state ResponseTxs (groupOf portfolio pool.poolNetwork accounts state) . map fst,
          listingWhole = False,
          listingShownEmpty = state.sectionShownNotSynced
        }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (sections, Nothing)
    Just named -> do
      txid <- chainTxidOf named
      cursors <- forM pools $ \pool -> lift (runExceptT (txsCursorAt (listedOrder paging) pool txid))
      case [(i, cursor) | (i, Right cursor) <- zip [0 :: Int ..] cursors] of
        (i, cursor) : _ -> return (drop i sections, Just cursor)
        [] -> throwError ("The transaction " <> hashRefText named <> " is not among those listed")
  return $ Sections (length pools > 1) paging.pagingMax (zipWith ($) from (startM : repeat Nothing))

-- | The pending transactions of every portfolio of every wallet, on
-- every network hw shows, or on the --network alone (@pendingListing@).
-- Unlike the other listings, pendingtxs does not keep to the active
-- portfolio. A pending transaction waits for the user: to be signed, sent
-- or deleted. One left in another portfolio would be forgotten, and the
-- coins it spends stay locked until then. There are few of them, so one
-- list of them all stays short. The commands that take one find its
-- portfolio by themselves, so the user acts on it without a switch.
--
-- pendingtxs keeps to the shown networks, as the other listings do. A
-- transaction pending on a hidden network locks coins of that network
-- alone, which no listing shows either.
--
-- With a filter of tags, the listing keeps the pending transactions the
-- filter keeps, as txs does (@pendingListing@).
pendingTxsListing :: Config -> Maybe Network -> TxTagFilter -> Paging HashRef -> ExceptT String (DB IO) Sections
pendingTxsListing cfg netM kept paging = do
  _ <- requireTags (kept.keptTxTags <> kept.keptAddressTags)
  pools <- onShownNetworks cfg (.poolNetwork) <$> lift allPools
  pendingListing (Just [pool | pool <- pools, maybe True (== pool.poolNetwork) netM]) kept paging

-- | The pending transactions of the pools, or of every pool, in one list
-- rather than in sections: by wallet, by portfolio and by network, in the
-- order of the networks of a portfolio, and the newest first on each, or
-- the oldest first with --reverse. The places keep their order, as the
-- sections of the other listings do. Each says where it is. The answer carries the active portfolio, for the
-- screen to name the portfolio after the paths of a transaction of
-- another one. The one to go on after is named by either of its
-- hashes. There are few of them, so the listing is read whole, and its
-- table sizes its columns to all of them. pendingtxs and exportsheet list
-- the pools on the networks hw shows, and pendingtxs those of one network
-- with --network.
--
-- A filter of tags keeps the transactions that have one of its tags, or
-- that spend from or pay an address of their portfolio that has one of its
-- tags of addresses, or a contact of the address book that has one, or that
-- are left to sort when those are asked for (the type
-- t'Haskoin.Wallet.Database.Tag.TxTagFilter'). The listing is read whole,
-- so the filter is applied to the transactions once read, each with its
-- tags. Each one kept is told the tags of addresses the listing keeps, for
-- its short views to show them (the type t'TxTags').
pendingListing :: Maybe [Pool] -> TxTagFilter -> Paging HashRef -> ExceptT String (DB IO) Sections
pendingListing poolsM kept paging = do
  start <- forM paging.pagingAfter (fmap snd . pendingCursor poolsM <=< pendingHashOf)
  activeM <- activePortfolioRef
  return . oneSection paging.pagingMax $
    Listing
      { listingStart = start,
        listingRows = \afterM countM -> do
          everyPending <- pendingAfter poolsM Nothing Nothing
          let newestFirst = [(place, toldKept info) | (place, info) <- everyPending, keeps info]
              held = sortOn placeOrder $ case listedOrder paging of
                UsualOrder -> newestFirst
                ReverseOrder -> reverse newestFirst
          let following = case afterM of
                Nothing -> held
                Just cursor -> drop 1 $ dropWhile ((/= Just cursor) . pendingHashOfRow) held
          return $ maybe id genericTake countM following,
        listingCursor = maybe (Left "pendingtxs lists the transactions the wallet holds only") Right . pendingHashOfRow,
        listingAnswer = ResponsePendingTxs activeM,
        listingWhole = True,
        listingShownEmpty = False
      }
  where
    -- By wallet, portfolio and network: the sort is stable, so it keeps
    -- the order of the transactions in each
    placeOrder (place, _) = (placePortfolio place, networkRank (placeNetwork place))
    pendingHashOfRow (_, info) = case txInfoStatus info of
      Pending pending -> Just (pendingNosigHash pending)
      OnChain _ -> Nothing
    -- Whether the filter keeps a transaction, from the tags it shows
    keeps info
      | not (isTxFiltered kept) = True
      | otherwise =
          let tags = txInfoTags info
              named shown = shownTag <$> shown
              tagsOfContacts = contactTags <$> Map.elems (txInfoContacts info)
           in any (`elem` kept.keptTxTags) (named (txOwnTags tags))
                || any (any (`elem` kept.keptAddressTags) . named) (Map.elems (txAddressTags tags))
                -- The contacts it pays or was paid from count as its
                -- addresses do
                || any (any (`elem` kept.keptAddressTags) . named) tagsOfContacts
                -- Left to sort: no tag of its own, and none of a contact
                || (kept.keptUntaggedTxs && null (txOwnTags tags) && all null tagsOfContacts)
    toldKept info = info {txInfoTags = (txInfoTags info) {txKeptAddressTags = kept.keptAddressTags}}

-- | Add the transaction of a file to the pending transactions of its
-- portfolio, and show it as the wallet holds it from now on: with the
-- labels of the wallet, not those of the file. hw finds the portfolio by
-- itself: the one that pays, when the transaction moves funds between
-- two, or the active one among several it cannot tell apart
-- ('transactionPool').
--
-- A file of a transaction the wallet holds already, in the same version
-- or signed where the file is not, changes nothing. The command says so
-- on stderr and answers with the transaction as the wallet holds it, as
-- any import does: importing a file twice is no mistake.
cmdImportTx :: Config -> FilePath -> IO Response
cmdImportTx cfg file =
  runDBResponse cfg $ do
    TxFileRead (TxFile netM tsd _ _) _ <- readTxFileIn (configDefaultNetwork cfg) file
    activeM <- lift activePortfolioId
    (poolM, completed) <- transactionPool cfg activeM netM tsd
    pool <- liftMaybe (notInWallet <> ". " <> sweepStaysInFile) poolM
    (pendingHash, did) <- importedTx pool completed
    held <- requirePendingTx pendingHash
    when (did == HeldAlready) $ liftIO $ tell terminal (heldAlreadyNotice held)
    txAnswer pool =<< pendingSummary held
  where
    sweepStaysInFile =
      "A sweep that pays no address of the wallet stays in its file: \
      \hw signsweep --input signs it there, and hw broadcast --input sends it"

-- | What importtx says of a file that brings nothing new: the wallet
-- holds the transaction, and how far along it is there
heldAlreadyNotice :: PendingTx -> String
heldAlreadyNotice held = "The wallet holds this transaction already, " <> state <> ": nothing was changed."
  where
    state
      | held.pendingTxOnline = "sent"
      | isSigned (txSignDataTx held.pendingTxData) = "signed and not sent"
      | otherwise = "not signed"

-- | Write a pending transaction to a file, with the labels of its
-- addresses as the wallet has them now
cmdExportTx :: Config -> HashRef -> FilePath -> IO Response
cmdExportTx cfg named file =
  runDBResponse cfg $ do
    held <- pendingTxNamed cfg named
    checkFileToWrite file
    let pool = held.pendingTxPool
    info <- txInfoOf pool held.pendingTxData
    writeTxFile (Just pool) file (labelledTxFile pool.poolNetwork info held.pendingTxData)
    return $ ResponseFile file

-- | Delete a pending transaction, named by either of its hashes. The
-- answer gives both, whichever the user typed.
--
-- The user is asked first, unless --yes, and told where the transaction
-- stands ('confirmDeletion', 'deleteTxNotice'). The question comes after
-- the store was asked: a transaction the server still knows is refused
-- without a question ('deleteTxAfter').
cmdDeleteTx :: Config -> HashRef -> Bool -> IO Response
cmdDeleteTx cfg named yes =
  runDBResponse cfg $ do
    held <- pendingTxNamed cfg named
    let tx = txSignDataTx held.pendingTxData
        txidM = if isSigned tx then Just (txHash tx) else Nothing
        asking found = confirmDeletion yes (deleteTxNotice found) "Delete the pending transaction?"
    storeOn <- liftIO $ webStores cfg
    (coins, addrs) <- deleteTxAfter storeOn asking (nosigHash tx)
    return $ ResponseDeleteTx (nosigHash tx) txidM coins addrs
