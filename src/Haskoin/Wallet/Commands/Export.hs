{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The listings as files for a spreadsheet: --csv, which writes one
-- listing to a CSV file, and exportsheet, which writes every listing of
-- every portfolio to an OpenDocument spreadsheet. Both write the sheet of
-- a listing ("Haskoin.Wallet.Cli.Sheet"), so that the rows of one file fit
-- the other.
module Haskoin.Wallet.Commands.Export
  ( cmdCsv,
    cmdExportSheet,
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.String.Conversions (cs)
import Data.Time (getCurrentTime)
import Haskoin.Wallet.Cli.Csv (sheetCsv)
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Ods (workbookOds)
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Sheet (PortfolioSheets (..), Sheet, TxSheets, localListingSheet, localTxSheets, stacked, walletSheets)
import Haskoin.Wallet.Commands.Address (addressesListing, balancesListing)
import Haskoin.Wallet.Commands.Coin (coinsListing)
import Haskoin.Wallet.Commands.Contact (contactsListing)
import Haskoin.Wallet.Commands.Portfolio (portfoliosListing)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.Transaction (pendingTxsListing, txsListing)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Util

-- | Write every row of a listing to a new CSV file, for a spreadsheet,
-- and answer with the file ("Haskoin.Wallet.Cli.Csv"). The listing is
-- read whole, in one transaction, as --json reads it, then written: the
-- file is either whole or not there. A path that is taken is refused
-- before the listing is read. The file holds the sheet of the listing
-- ('localListingSheet'), the one exportsheet writes to the tab of the
-- listing, so that the rows of one file fit the other.
cmdCsv :: Config -> FilePath -> ExceptT String (DB IO) Sections -> IO Response
cmdCsv cfg file listing = do
  free <- runExceptT (checkFileToWrite file)
  case free of
    Left err -> return (ResponseError (cs err))
    Right () -> do
      answer <- readListing cfg listing
      sheetM <- localListingSheet answer
      case sheetM of
        -- The listing failed, and its answer says why
        Nothing -> return answer
        Just sheet -> either (ResponseError . cs) (const (ResponseFile file)) <$> writeFileBytes file (sheetCsv sheet)

-- | Write every listing of every portfolio to a new OpenDocument
-- spreadsheet, a tab each ("Haskoin.Wallet.Cli.Ods"), and answer with the
-- file. The tab of a listing holds its sheet, the one --csv writes, for
-- every portfolio ('walletSheets'). So the rows of a CSV file fit under
-- the headings of its tab. Two more tabs hold the addresses of the
-- transactions and of the pending transactions, a row for each address.
-- The last tab holds the address book, on the networks hw shows.
-- Each listing is read in its usual order ('everyRow'): a spreadsheet
-- sorts its rows as the reader likes.
--
-- The listings are read in one transaction, so that the tabs agree with
-- each other: the balances with the coins, the coins with the
-- transactions, and the addresses with their transactions. The
-- spreadsheet is the record of the whole wallet, so the active portfolio
-- does not narrow it. It holds what the listings show: the networks the
-- configuration hides are left out of it, as they are of each listing.
--
-- With --tag or --untagged, each tab holds what its listing gives with
-- the same options: the addresses of a tag with their balances and
-- their coins, and the transactions and the contacts of that tag
-- ('txFilterOfRows'). The tab of the portfolios has no tags and stays
-- whole. A word that is no tag of the database is refused before
-- anything is read.
cmdExportSheet :: Config -> FilePath -> TagFilter -> IO Response
cmdExportSheet cfg file kept =
  runDBResponse cfg $ do
    checkFileToWrite file
    _ <- requireTags kept.keptTags
    portfolios <- lift getAllPortfolios
    when (null portfolios) $ throwError noWallets
    summary <- sheetOf (portfoliosListing cfg)
    held <- mapM portfolioSheets portfolios
    -- The pending transactions of every portfolio in one tab, and their
    -- addresses in another
    pending <- txSheetsOf (pendingTxsListing cfg Nothing keptTxs everyRow)
    -- The address book, which is of no portfolio, in a tab of its own
    contacts <- sheetOf (contactsListing cfg Nothing kept everyRow)
    now <- liftIO getCurrentTime
    liftEitherIO $ writeFileBytes file (workbookOds now (walletSheets summary held pending contacts))
    return $ ResponseFile file
  where
    -- The transactions the tabs of transactions keep
    keptTxs = txFilterOfRows kept
    -- The listings of one portfolio, over every network hw shows: its
    -- receiving addresses then its change addresses, its balances, all
    -- its coins, and its transactions with their addresses
    portfolioSheets portfolio = do
      let whole = Target Nothing Nothing
      receiving <- sheetOf (addressesListing cfg portfolio whole False kept everyRow)
      change <- sheetOf (addressesListing cfg portfolio whole True kept everyRow)
      PortfolioSheets (stacked [receiving, change])
        <$> sheetOf (balancesListing cfg portfolio whole kept everyRow)
        <*> sheetOf (coinsListing cfg portfolio whole False kept everyRow)
        <*> txSheetsOf (txsListing cfg portfolio Nothing keptTxs everyRow)

-- | The sheet of a listing, read whole in the transaction the caller
-- runs, its local times on the clock of this computer
sheetOf :: ExceptT String (DB IO) Sections -> ExceptT String (DB IO) Sheet
sheetOf = sheetsOf localListingSheet

-- | The two sheets of a listing of transactions, read once: its
-- transactions and their addresses. Both come from one reading, so that
-- every address is of a transaction of the other sheet.
txSheetsOf :: ExceptT String (DB IO) Sections -> ExceptT String (DB IO) TxSheets
txSheetsOf = sheetsOf localTxSheets

-- | What the function given makes of the answer of a listing, read whole
-- in the transaction the caller runs: its sheet, or its sheets
sheetsOf :: (Response -> IO (Maybe sheets)) -> ExceptT String (DB IO) Sections -> ExceptT String (DB IO) sheets
sheetsOf sheetsFrom listing = do
  answer <- wholeListing listing
  made <- liftIO (sheetsFrom answer)
  -- Every listing has a sheet: an answer without one would be a bug
  maybe (throwError "The listing has no sheet") return made
