{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A listing on the screen: its sections, one for each network or each
-- account, with the rows of each as lines or as a table. The screen reads
-- a listing in chunks of rows ("Haskoin.Wallet.Cli.Listing"), and
-- 'chunkDoc' makes the document of one chunk.
--
-- This module is above those of what a listing lists: it takes the rows
-- and the columns of each from its module.
module Haskoin.Wallet.Cli.PrettyPrinter.Listing
  ( Rows (..),
    severalGroups,
    sectionsPrinter,
    keptTagsDoc,
    balanceTitle,
    notSyncedLine,
    listingDoc,
    chunkDoc,
    startsWithHeader,
  )
where

import Data.List (intersperse, nub)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Data.Time (utcToLocalZonedTime)
import Haskoin
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.Listing (ChunkAt (..), KeptTags (..), keptTagsWords)
import Haskoin.Wallet.Cli.PrettyPrinter.Address (addressRowDoc, addressTableColumns, balanceRowDoc, balanceTableColumns, listedAddressDoc, listedBalanceDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.Balance (balanceFigures, notSyncedWords)
import Haskoin.Wallet.Cli.PrettyPrinter.Base (columns, commaList, tagWord, text)
import Haskoin.Wallet.Cli.PrettyPrinter.Coin (coinRowDoc, coinTableColumns, listedCoinDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.Contact (contactRowDoc, contactTableColumns, listedContactDoc, noContacts)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (accountRefLine, activePaths, listedPaths, placeLine)
import Haskoin.Wallet.Cli.PrettyPrinter.Portfolio (listedPortfolioDoc, portfolioRowDocs, portfolioTableColumns)
import Haskoin.Wallet.Cli.PrettyPrinter.Table (tableHeader)
import Haskoin.Wallet.Cli.PrettyPrinter.Transaction (listedTxDoc, localTxTime, pendingRowDoc, pendingTableColumns, txColumns, txRowDoc)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Address (CoinBalance (..), chainPath)
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag (TagWidths (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (PoolPath (..), poolPathText)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, hsep, vcat)

-- | The rows of a section of a listing, as the screen shows them. A
-- listing over a portfolio has a section for each network, or for each
-- account for the addresses. Any other listing is one section.
data Rows = Rows
  { -- | What to say when the listing has no row at all
    rowsNone :: Doc Style,
    -- | The lines that say where the rows are, above the section: none
    -- in a listing of one place, which the command line already names
    rowsTitle :: [Doc Style],
    -- | The lines above the first row: the header of a table. Only the
    -- first chunk of a section shows them.
    rowsHeader :: [Doc Style],
    -- | A document per row
    rowsDocs :: [Doc Style],
    -- | Whether a blank line sets the rows apart
    rowsApart :: Bool
  }

-- | The rows of a listing whose elements take a few lines each: no
-- title, no header, and a blank line between two elements
apartRows :: Doc Style -> [Doc Style] -> Rows
apartRows none docs = Rows none [] [] docs True

-- | Whether an answer that lists comes in several groups, each of which
-- then says where it is
severalGroups :: Response -> Bool
severalGroups = \case
  ResponseAddresses groups -> length groups > 1
  ResponseBalances groups -> length groups > 1
  ResponseTxs groups -> length groups > 1
  ResponseCoins groups -> length groups > 1
  ResponseContacts groups -> length groups > 1
  _ -> False

-- | The sections of an answer that lists, or Nothing for an answer that
-- does not: one for each group of the answer, or one alone for the
-- portfolios and the pending transactions, which come in no group. An
-- answer without a group gives one section without rows, which says what
-- to say then.
--
-- The rows of a section have a blank line between them. A table has a
-- header instead, and a row under the other. The screen reads a listing
-- in chunks, so the paths of its addresses are padded to the longest path
-- of the accounts of the section on the chains listed, the same for every
-- chunk, rather than to the longest one shown. The columns of a table
-- have fixed widths for the same reason, and its column of tags the width
-- given, that of the widest tags of the database. The portfolios, addresses,
-- transactions and coins show in the view given, their times in the time
-- zone of the computer.
--
-- With the flag, each section says where it is in a line above its rows:
-- its portfolio, with its wallet or not as given, and its network, and
-- its account for the addresses. The
-- callers set it for a listing that may cover several places. The
-- sections of balances always have that line, which gives the total of
-- the network: the specification of the listing asks for a total for
-- each network, and a listing narrowed to one network has one too.
sectionsPrinter :: UnitChoice -> View -> WalletNaming -> TagWidths -> Bool -> Response -> IO (Maybe [Rows])
sectionsPrinter choice view naming tagsWide titled = \case
  ResponsePortfolios portfolios -> do
    let none = text "There are no portfolios to display"
        -- The listing is read whole: these are all the portfolios
        headed = portfolioTableColumns choice portfolios
    if view == Table
      then return $ Just [Rows none [] (tableHeader headed) (concatMap (portfolioRowDocs choice headed) portfolios) False]
      else Just . pure . apartRows none <$> mapM (listedPortfolioDoc view choice) portfolios
  ResponseAddresses groups ->
    sections (text "There are no addresses to display") groups $ \none g -> do
      let net = placeNetwork (groupPlace g)
          unit = unitOn net choice
          addrs = groupRows g
          pad = longestPathWidth (groupAccounts g) (nub [dBAddressInternal addr | (addr, _, _) <- addrs])
          -- addresses lists the addresses of one chain
          internal = or [dBAddressInternal addr | (addr, _, _) <- addrs]
          headed = addressTableColumns unit net (countsKind <$> groupAccounts g) pad tagsWide.addressTagsWidth internal
          title = [maybe (placeLine naming (groupPlace g)) (accountRefLine naming) (groupAccountRef g) | titled]
      if view == Table
        then return $ Rows none title (tableHeader headed) [addressRowDoc unit net headed addr funds tags | (addr, funds, tags) <- addrs] False
        else do
          docs <- mapM (\(addr, funds, tags) -> listedAddressDoc view net unit pad addr funds tags) addrs
          return $ Rows none title [] docs True
  ResponseBalances groups ->
    sections (text "There are no addresses with coins to display") groups $ \none (g, total) -> onceSynced none g $ do
      let net = placeNetwork (groupPlace g)
          unit = unitOn net choice
          addrs = groupRows g
          pad = longestPathWidth (groupAccounts g) [False, True]
          headed = balanceTableColumns unit net (countsKind <$> groupAccounts g) pad tagsWide.addressTagsWidth
          title = [balanceTitle naming unit (groupPlace g) total]
      return $
        if view == Table
          then Rows none title (tableHeader headed) [balanceRowDoc unit net headed addr balance tags | (addr, balance, tags) <- addrs] False
          else Rows none title [] [listedBalanceDoc view net unit pad addr balance tags | (addr, balance, tags) <- addrs] True
  ResponseTxs groups ->
    sections (text "There are no transactions to display") groups $ \none g -> onceSynced none g $ do
      let net = placeNetwork (groupPlace g)
          unit = unitOn net choice
          title = [placeLine naming (groupPlace g) | titled]
          headed = txColumns unit tagsWide.txTagsWidth
          -- txs lists the active portfolio
          paths = activePaths naming
      if view == Table
        then do
          rows <- mapM (\(ListedTx info balanceM) -> txRowDoc paths net unit headed balanceM <$> localTxTime info <*> pure info) (groupRows g)
          return $ Rows none title (tableHeader headed) rows False
        else do
          docs <- mapM (\(ListedTx info balanceM) -> listedTxDoc paths view net unit Nothing balanceM info) (groupRows g)
          return $ Rows none title [] docs True
  -- The listing covers every portfolio of every wallet, so each
  -- transaction says where it is, with its wallet when the database has
  -- several, and the paths of one of another portfolio than the active
  -- one name that portfolio
  ResponsePendingTxs activeM held -> do
    let none = text "There are no pending transactions"
        -- The listing is read whole: these are all its places
        headed = pendingTableColumns choice naming tagsWide.txTagsWidth (fst <$> held)
        pathsAt place = listedPaths naming activeM (placePortfolio place)
    if view == Table
      then do
        rows <- mapM (\(place, info) -> pendingRowDoc choice naming (pathsAt place) headed place <$> localTxTime info <*> pure info) held
        return $ Just [Rows none [] (tableHeader headed) rows False]
      else do
        docs <- mapM (\(place, info) -> listedTxDoc (pathsAt place) view (placeNetwork place) (unitOn (placeNetwork place) choice) (Just (placePortfolio place)) Nothing info) held
        return $ Just [apartRows none docs]
  ResponseCoins groups ->
    sections (text "There are no coins to display") groups $ \none g -> onceSynced none g $ do
      let net = placeNetwork (groupPlace g)
          unit = unitOn net choice
          coins = groupRows g
          headed = coinTableColumns unit net (countsKind <$> groupAccounts g) (longestPathWidth (groupAccounts g) [False, True]) tagsWide.addressTagsWidth
          title = [placeLine naming (groupPlace g) | titled]
      if view == Table
        then do
          rows <- mapM (\coin -> coinRowDoc net unit headed <$> traverse utcToLocalZonedTime (jsonCoinTime coin) <*> pure coin) coins
          return $ Rows none title (tableHeader headed) rows False
        else do
          docs <- mapM (listedCoinDoc view net unit) coins
          return $ Rows none title [] docs True
  -- The contacts of each network are read whole, so the columns of a
  -- table fit the widest cell of its section
  ResponseContacts groups ->
    sections noContacts groups $ \none (net, contacts) -> do
      let headed = contactTableColumns tagsWide.contactTagsWidth contacts
          title = [text net.name | titled]
      if view == Table
        then return $ Rows none title (tableHeader headed) (contactRowDoc headed <$> contacts) False
        else do
          docs <- mapM (listedContactDoc view) contacts
          return $ Rows none title [] docs True
  _ -> return Nothing
  where
    -- A section for each group, or one without rows for no group
    sections :: Doc Style -> [group] -> (Doc Style -> group -> IO Rows) -> IO (Maybe [Rows])
    sections none [] _ = return $ Just [apartRows none []]
    sections none groups rowsOf = Just <$> mapM (rowsOf none) groups
    -- The section of a group of balances, coins or txs, as given. A
    -- group without a row on a network that never synced has the line
    -- that says so instead ('notSyncedLine'), whether the sections say
    -- where they are or not. The line stands as its one row, since a
    -- section without rows shows nothing: no title and no header of a
    -- table come with it, with no row under them.
    onceSynced :: Doc Style -> Group row -> IO Rows -> IO Rows
    onceSynced none g rows
      | groupSynced g || not (null (groupRows g)) = rows
      | otherwise = return (Rows none [] [] [notSyncedLine naming (groupPlace g)] False)

-- | The first line of a listing that keeps the rows with some tags: what
-- it lists, and the tags it keeps, each in its colour, as in "Addresses
-- tagged kyc or cold". A listing of transactions may keep them by their
-- own tags, by the tags of their addresses, or by both: "Transactions
-- tagged rent, or at addresses tagged kyc". A listing that keeps the rows
-- without a tag says so the same way: "Addresses without a tag", or
-- after the tags it keeps too, "Addresses tagged kyc, or without a tag"
-- ('keptTagsWords'). A blank line follows, which sets the line apart
-- from the listing.
keptTagsDoc :: KeptTags -> Doc Style
keptTagsDoc kept = vcat [hsep (text kept.keptNoun : keptTagsWords text tagWord kept), mempty]

-- | The line above the addresses balances lists on a network: where they
-- are, then the value of every coin of the portfolio there, what it can
-- spend now, and the rest, which waits. The figures count every coin the
-- listing covers on the network, whatever part of them a page shows.
balanceTitle :: WalletNaming -> AmountUnit -> Place -> CoinBalance -> Doc Style
balanceTitle naming unit place total =
  columns
    [ placeLine naming place,
      commaList (balanceFigures unit (coinBalanceTotal total) (coinBalanceSpendable total))
    ]

-- | All that balances, coins and txs show of a network where the
-- portfolio has addresses and never synced: where it is, then that it is
-- not synced ('notSyncedWords'). hw asked no server what the addresses
-- hold there, and "There are no coins to display" would say that they
-- hold nothing.
notSyncedLine :: WalletNaming -> Place -> Doc Style
notSyncedLine naming place = columns [placeLine naming place, notSyncedWords]

-- | The width of the longest derivation path of the accounts on the
-- chains, False for the receiving one and True for the change one, with
-- the type of the account in front, as the views write it: that of the
-- newest address there
longestPathWidth :: [AccountCounts] -> [Bool] -> Natural
longestPathWidth accounts chains = fromIntegral $ maximum (0 : [width account internal | account <- accounts, internal <- chains])
  where
    width account internal =
      let count = if internal then countsInternal account else countsExternal account
          newest = PoolPath (countsKind account) (chainPath internal (fromIntegral (count - 1)))
       in if count == 0 then 0 else Text.length (poolPathText newest)

-- | A whole listing: its sections that have rows, a blank line between
-- them, or what to say when none has a row
listingDoc :: [Rows] -> Doc Style
listingDoc sections = case filter (not . null . rowsDocs) sections of
  [] -> maybe mempty rowsNone (listToMaybe sections)
  shown -> vcat (intersperse mempty (sectionDoc <$> shown))

-- | A section from its start: its title, set apart by a blank line when
-- its rows are, its header and its rows
sectionDoc :: Rows -> Doc Style
sectionDoc rows =
  vcat (rowsTitle rows <> [mempty | rowsApart rows, not (null (rowsTitle rows))] <> rowsHeader rows <> [rowsBody rows])

-- | The rows of a section alone, without its title and its header
rowsBody :: Rows -> Doc Style
rowsBody rows = vcat (if rowsApart rows then intersperse mempty (rowsDocs rows) else rowsDocs rows)

-- | A chunk of a listing, where the listing says it is (the type
-- t'ChunkAt'). The first chunk of a listing without rows says so. A chunk
-- that starts a section has the title of the section, if any, and the
-- header of its table, after a blank line that sets it apart from the
-- section before. A chunk that goes on in its section comes under the rows
-- before it, after the blank line that sets them apart if one does. An
-- answer that is not a listing is a failure here. A portfolio is named with
-- its wallet or not, as given, and the columns of tags of a table have
-- the widths given, both the same for every chunk of a listing.
chunkDoc :: UnitChoice -> View -> WalletNaming -> TagWidths -> ChunkAt -> Response -> IO (Doc Style)
chunkDoc unit view naming tagsWide at answer = do
  sectionsM <- sectionsPrinter unit view naming tagsWide (chunkTitled at) answer
  return $ case sectionsM of
    Just sections
      | all (null . rowsDocs) sections -> listingDoc sections
      | chunkStartsSection at -> vcat ([mempty | not (chunkFirst at)] <> [listingDoc sections])
      | otherwise -> vcat [vcat ([mempty | rowsApart rows] <> [rowsBody rows]) | rows <- sections]
    Nothing -> case answer of
      ResponseError err -> errorDoc err
      _ -> errorDoc "The answer is not a listing"

-- | Whether the first lines of the first chunk of a listing are the
-- header of a table, which the pager can then keep on the screen: a
-- table whose sections have no title. A title comes above the header, and
-- the pager keeps the first lines it is given.
startsWithHeader :: View -> ChunkAt -> Response -> Bool
startsWithHeader view at answer = view == Table && not (chunkTitled at) && not balances
  where
    balances = case answer of
      ResponseBalances _ -> True
      _ -> False
