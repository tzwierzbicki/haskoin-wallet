{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | report: a balance statement, an account statement or a payment
-- confirmation, written to a PDF file. This module reads what the options
-- ask for: what the report covers, its day or its period, the prices that
-- value the balances, and the tags that narrow it. It then reads from the
-- database what the report is worked out from. The modules of the
-- reports work it out and lay it out
-- ("Haskoin.Wallet.Report.BalanceStatement",
-- "Haskoin.Wallet.Report.AccountStatement",
-- "Haskoin.Wallet.Report.PaymentConfirmation").
module Haskoin.Wallet.Commands.Report
  ( cmdReport,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime (..), TimeZone, UTCTime, ZonedTime (..), addDays, fromGregorian, getTimeZone, getZonedTime, localTimeToUTC, midnight, toGregorian, utc)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Parser
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.Scope (listedPools)
import Haskoin.Wallet.Commands.ShortId (shownViews, walletTxHashOf)
import Haskoin.Wallet.Commands.TagFilter (addressSelection, txSelection)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.BestBlock (BestBlock (..), getBest)
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Report.AccountStatement (AccountStatement (..), PeriodStart (..), accountBlocksUnnamed, accountStatement, accountStatementReport, accountsSelected, accountsValued, namingAccountBlocks)
import Haskoin.Wallet.Report.BalanceStatement (Statement (..), balanceStatement, holdingsSelected, lastBlocksUnnamed, namingLastBlocks, statementReport, valued)
import Haskoin.Wallet.Report.Layout (reportPdf)
import Haskoin.Wallet.Report.PaymentConfirmation (PaymentConfirmation (paymentCounterparty, paymentHolder, paymentPurpose), namingPaymentBlock, paymentBlocksUnnamed, paymentConfirmation, paymentReport, paymentValued)
import Haskoin.Wallet.Report.Statement (PortfolioHistory (..), StatementDate (..), StatementScope (..), SyncedTo (..), Valuation (..), longDate, statementTimes)
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Store
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

-- | Write a report to a new PDF file, and answer with the file
-- ("Haskoin.Wallet.Report.Layout"). The option of the report says which
-- one.
--
-- A report is of the whole database, as the spreadsheet and the journal
-- are: it is what the owner declares, and the active portfolio is only
-- where the commands work. It holds the networks hw shows. --wallet and
-- --portfolio narrow it down (@reportScope@). The history of every
-- portfolio is read all the same, since a narrowed statement is worked
-- out from all of them ("Haskoin.Wallet.Report.Statement").
--
-- --tag, --address-tag and --untagged keep an account statement to the
-- transactions txs lists with the same options in each portfolio
-- ('txSelection'). The statement is worked out whole first, then loses
-- the other lines, and with them its balances
-- ("Haskoin.Wallet.Report.AccountStatement").
--
-- --tag and --untagged keep a balance statement to the addresses
-- balances lists with the same options in each portfolio
-- ('addressSelection'). It too is worked out whole first, then loses
-- the other addresses, and is valued after that: the prices it needs
-- are those of the assets these addresses hold
-- ("Haskoin.Wallet.Report.BalanceStatement").
--
-- A payment confirmation is of one transaction, which it finds in every
-- portfolio, as show does. --wallet and --portfolio then say whose side
-- of the payment it is on
-- ("Haskoin.Wallet.Report.PaymentConfirmation").
--
-- Everything that can refuse the report comes first: the options that do
-- not go together, the days, the last sync, a missing price. The server
-- is asked last, for the hashes of the blocks the report names, and a
-- server that does not answer refuses nothing (@blockHashesAt@).
--
-- The file is written whole, and a path that is taken is refused, as
-- for every file hw writes.
cmdReport :: Config -> ReportOptions -> IO Response
cmdReport cfg opts =
  runDBResponse cfg $ do
    checkFileToWrite opts.file
    liftEither $ checkReportOptions opts
    valuation <- liftEither $ valuationOf opts
    now <- liftIO getZonedTime
    portfolios <- lift getAllPortfolios
    when (null portfolios) $ throwError noWallets
    scope <- reportScope opts.wallet opts.portfolio
    report <- case opts.kind of
      BalanceReport -> do
        date <- statementDateAt "--date" "a statement of now" now opts.date
        selected <- addressSelection cfg portfolios (TagFilter opts.kept.keptTxTags opts.kept.keptUntaggedTxs)
        counted <- statementAt cfg portfolios (\zoneAt -> balanceStatement scope zoneAt date)
        priced <- liftEither $ maybe Right valued valuation (maybe id holdingsSelected selected counted)
        hashes <- blockHashesAt cfg (lastBlocksUnnamed priced)
        let statement = (namingLastBlocks hashes priced) {statementHolder = holderOf opts}
        return (statementReport now statement)
      AccountReport -> do
        (start, end) <- statementPeriodAt now opts.from opts.to
        selected <- txSelection cfg portfolios opts.kept
        counted <- statementAt cfg portfolios (\zoneAt -> accountStatement scope zoneAt start end)
        priced <- liftEither $ maybe Right accountsValued valuation (maybe id accountsSelected selected counted)
        hashes <- blockHashesAt cfg (accountBlocksUnnamed priced)
        let statement = (namingAccountBlocks hashes priced) {accountsHolder = holderOf opts}
        return (accountStatementReport now statement)
      PaymentReport named -> do
        txid <- walletTxHashOf named
        histories <- paymentHistories cfg portfolios named txid
        counted <- statementOf histories (\zoneAt -> paymentConfirmation scope zoneAt txid opts.addresses)
        priced <- liftEither $ maybe Right paymentValued valuation counted
        hashes <- blockHashesAt cfg (paymentBlocksUnnamed priced)
        let payment =
              (namingPaymentBlock hashes priced)
                { paymentHolder = holderOf opts,
                  paymentCounterparty = maybe "" Text.strip opts.counterparty,
                  paymentPurpose = maybe "" Text.strip opts.purpose
                }
        return (paymentReport now payment)
    liftEitherIO $ writeFileBytes opts.file (reportPdf now report)
    return $ ResponseFile opts.file

-- | What a report covers, as --wallet and --portfolio say it: every
-- wallet without them, the portfolios of one wallet with --wallet, and
-- one portfolio with --portfolio. The wallet of that portfolio is the
-- one --wallet names, or the wallet of the active portfolio without
-- --wallet. The options mean what they mean for exportwallet
-- (@portfoliosToExport@), so that a user learns them once.
--
-- A wallet or a portfolio the database does not have is refused.
reportScope :: Maybe Natural -> Maybe Natural -> ExceptT String (DB IO) StatementScope
reportScope walletM portfolioM = case (walletM, portfolioM) of
  (Nothing, Nothing) -> return EveryWallet
  (Just index, Nothing) -> OneWallet index <$ walletNamed index
  (_, Just index) -> do
    wallet <- walletNamedOrActive walletM
    OnePortfolio . portfolioRef <$> portfolioNamed wallet index

-- | Who holds the wallets, as --holder says it, without the spaces
-- around it, or nothing
holderOf :: ReportOptions -> Text
holderOf opts = maybe "" Text.strip opts.holder

-- | Refuse an option that does not go with the report. A balance
-- statement is for a day, an account statement for a period, and a
-- payment confirmation for a transaction, which has its own day. The
-- other side of a payment, its purpose and its addresses go with a
-- payment confirmation alone. The tags go with the two statements:
-- they keep an account statement to some transactions and a balance
-- statement to some addresses. --address-tag goes with an account
-- statement alone, since --tag is already about the tags of the
-- addresses in a balance statement, as it is in balances. An option of
-- another report would be left aside without a word, and the report
-- would not be the one the user asked for.
--
-- An account statement kept to some transactions takes no price. A
-- price values a closing balance, and such a statement states none
-- ("Haskoin.Wallet.Report.AccountStatement").
checkReportOptions :: ReportOptions -> Either String ()
checkReportOptions opts = case opts.kind of
  BalanceReport
    | isJust opts.from || isJust opts.to ->
        Left "--from and --to go with --transactions: give the day of a balance statement with --date"
    | ofPaymentOnly -> Left paymentOnly
    | not (null opts.kept.keptAddressTags) ->
        Left "--address-tag goes with --transactions: it keeps an account statement to the transactions at the addresses of a tag. --tag keeps a balance statement to the addresses that have a tag"
  AccountReport
    | isJust opts.date ->
        Left "--date goes with --balances: give the period of an account statement with --from and --to"
    | ofPaymentOnly -> Left paymentOnly
    | selects && priced ->
        Left "--price, --currency and --price-source go with a statement of every transaction: they value the closing balances, and a statement kept to some transactions with --tag, --address-tag or --untagged states none"
  PaymentReport _
    | isJust opts.date || isJust opts.from || isJust opts.to ->
        Left "--date, --from and --to go with --balances and --transactions: a payment confirmation is dated as its transaction is"
    | selects -> Left statementOnly
  _ -> Right ()
  where
    ofPaymentOnly = isJust opts.counterparty || isJust opts.purpose || not (null opts.addresses)
    paymentOnly = "--counterparty, --purpose and --address go with --payment: they say who a payment was with, what it was for and which address it paid"
    selects = isTxFiltered opts.kept
    statementOnly = "--tag, --address-tag and --untagged go with --balances and --transactions: they keep a statement to some of the addresses or to some of the transactions"
    priced = not (null opts.prices) || isJust opts.currency || isJust opts.priceSource

-- | The prices a report values the balances at, as its options give
-- them, or none. The prices and their currency go together: a price
-- without a currency says nothing, and neither does a currency without a
-- price. The source of the prices goes with them too.
valuationOf :: ReportOptions -> Either String (Maybe Valuation)
valuationOf opts = case (opts.prices, opts.currency, opts.priceSource) of
  ([], Nothing, Nothing) -> Right Nothing
  ([], Just _, _) -> Left "--currency goes with --price: give the price of a coin of each network, such as --price btc=87500"
  ([], Nothing, Just _) -> Left "--price-source goes with --price: give the price of a coin of each network, such as --price btc=87500"
  (_, Nothing, _) -> Left "Give the currency of the prices with --currency, such as --currency CHF"
  (prices, Just currency, source) -> Right (Just (Valuation currency prices (maybe "" Text.strip source)))

-- | The date a statement stops at: the end of the day an option gives,
-- or the moment the report is made. The option is named, and so is what
-- the report is without it, for the message. A day that is not over is
-- refused. Its end is still to come, and a statement for it would be
-- one of now under another name: without the option, the report says
-- the time it is of.
--
-- The end of a day comes with the zone the clock of this computer has
-- at that midnight, which the report states.
statementDateAt :: String -> String -> ZonedTime -> Maybe Day -> ExceptT String (DB IO) StatementDate
statementDateAt _ _ _ Nothing = return AtPreparation
statementDateAt option without now (Just day)
  | day < today = AtEndOf day <$> liftIO (zoneAtEndOf day)
  | otherwise =
      throwError $
        "The day of "
          <> option
          <> " must be over, and today is "
          <> cs (longDate today)
          <> ": give an earlier day, or leave "
          <> option
          <> " out for "
          <> without
  where
    today = localDay (zonedTimeToLocalTime now)

-- | The period of an account statement, as --from and --to give it:
-- where it starts and where it stops.
--
-- Without --to, the period runs up to the moment the report is made.
-- Without --from, it starts on the first day of the month of its last
-- day. So no option gives the month that is running, as a bank sends
-- it, and --to alone gives the month of that day.
--
-- A period that stops before it starts is refused, and so is one that
-- starts on a day still to come. The start comes with the zone the
-- clock of this computer has at the midnight that starts its first day,
-- which is the midnight that ends the day before.
statementPeriodAt :: ZonedTime -> Maybe Day -> Maybe Day -> ExceptT String (DB IO) (PeriodStart, StatementDate)
statementPeriodAt now fromM toM = do
  end <- statementDateAt "--to" "a statement up to now" now toM
  let lastDay = fromMaybe today toM
      firstDay = fromMaybe (firstOfMonth lastDay) fromM
  when (firstDay > lastDay) . throwError $ case toM of
    Just _ -> "The period starts on " <> cs (longDate firstDay) <> " and stops on " <> cs (longDate lastDay) <> ": give a day of --from that is not after the day of --to"
    Nothing -> "The day of --from is still to come, and today is " <> cs (longDate today) <> ": give today or an earlier day"
  startZone <- liftIO (zoneAtEndOf (pred firstDay))
  return (PeriodStart firstDay startZone, end)
  where
    today = localDay (zonedTimeToLocalTime now)
    firstOfMonth day = let (year, month, _) = toGregorian day in fromGregorian year month 1

-- | The zone the clock of this computer has at the midnight that ends a
-- day. The zone of a moment is asked of the computer in UTC, and the
-- midnight is known on the local clock. So the moment is first worked
-- out as if the clock were on UTC, which is off by the offset of the
-- zone at most, and then again with the zone found there. The two agree
-- unless the clock changes within those hours, and the second is then
-- the right one.
zoneAtEndOf :: Day -> IO TimeZone
zoneAtEndOf day = do
  nearby <- getTimeZone (localTimeToUTC utc midnightAfter)
  getTimeZone (localTimeToUTC nearby midnightAfter)
  where
    midnightAfter = LocalTime (addDays 1 day) midnight

-- | A statement of the portfolios, as the database has them now, on the
-- networks hw shows. The statement is worked out by the function given,
-- from the zone of the clock at each time, the block the last sync of
-- each network reached, and the history of each portfolio: that of the
-- balance statement or that of the account statement
-- ("Haskoin.Wallet.Report.Statement").
--
-- The transactions of every portfolio are read whole, in the
-- transaction the caller runs ('wholeHistory'), so that the two sides of
-- a payment between portfolios agree. The best block of each network is
-- the one its last sync left. The days are read on the clock of this
-- computer, each with the zone the computer had on that day.
statementAt :: Config -> [Portfolio] -> ((UTCTime -> TimeZone) -> [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> Either String statement) -> ExceptT String (DB IO) statement
statementAt cfg portfolios worked = do
  histories <- forM portfolios $ \portfolio -> do
    pools <- listedPools cfg portfolio Nothing
    historyOf portfolio <$> forM pools (\pool -> (pool.poolNetwork,) <$> wholeHistory pool)
  statementOf histories worked

-- | A portfolio as a report reads it, with the transactions given for
-- each network
historyOf :: Portfolio -> [(Network, [ListedTx])] -> PortfolioHistory
historyOf portfolio = PortfolioHistory (portfolioRef portfolio) (dBWalletDescription portfolio.portfolioWallet) (dBPortfolioDescription portfolio.portfolioRow)

-- | What a report is worked out from, given the histories of the
-- portfolios: the function given gets the zone of the clock at each
-- time, the block the last sync of each network of the histories
-- reached, and the histories.
statementOf :: [PortfolioHistory] -> ((UTCTime -> TimeZone) -> [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> Either String statement) -> ExceptT String (DB IO) statement
statementOf histories worked = do
  -- The networks of the histories, each once, in the order they come.
  -- In a statement every portfolio has the same ones.
  let shown = nub (concatMap (map fst . historyPools) histories)
  networks <- forM shown $ \net -> do
    best <- getBest net
    return (net, (\block -> SyncedTo block.blockHeight block.blockHash (posixSecondsToUTCTime . fromIntegral <$> block.blockTime)) <$> best)
  zones <- liftIO $ Map.fromList <$> mapM (\time -> (time,) <$> getTimeZone time) (statementTimes networks histories)
  liftEither $ worked (\time -> Map.findWithDefault utc time zones) networks histories

-- | The portfolios as a payment confirmation reads them: each with the
-- one transaction named, as the portfolio sees it, on the networks hw
-- shows. A portfolio that has no part in the transaction has no
-- transaction here. Every portfolio is still there, since the
-- confirmation tells from all of them whether its side is the whole
-- database ("Haskoin.Wallet.Report.PaymentConfirmation").
--
-- The transaction is read alone, as show reads it ('transactionViews'),
-- and not with the whole history, which a statement needs and a
-- confirmation does not. A transaction the wallet does not have is
-- refused, and so is one on hidden networks alone.
paymentHistories :: Config -> [Portfolio] -> HashRef -> TxHash -> ExceptT String (DB IO) [PortfolioHistory]
paymentHistories cfg portfolios named txid = do
  views <- shownViews cfg named =<< transactionViews txid
  return
    [ historyOf portfolio [(pool.poolNetwork, [ListedTx info Nothing]) | (pool, place, info) <- views, placePortfolio place == portfolioRef portfolio]
      | portfolio <- portfolios
    ]

-- | The hashes of the blocks given, each on its network at its height,
-- as the server has them: those a statement names and the database does
-- not have. The server lists the blocks it knows at a height, and the
-- one of its main chain is taken.
--
-- A server that does not answer, or that has no such block, gives no
-- hash for it, and the statement says so. A report is of the balances
-- and of the transactions, which need no server: a hash that cannot be
-- had today must not hold it back.
blockHashesAt :: Config -> [(Network, BlockHeight)] -> ExceptT String (DB IO) [(Network, BlockHeight, BlockHash)]
blockHashesAt cfg blocks = do
  storeOn <- liftIO $ webStores cfg
  hashes <- forM blocks $ \(net, height) -> do
    answer <- liftIO $ runExceptT (storeBlocksAtHeights (storeOn net) [height])
    return [(net, height, headerHash block.header) | Right blocks' <- [answer], block <- take 1 (filter (.main) blocks')]
  return (concat hashes)
