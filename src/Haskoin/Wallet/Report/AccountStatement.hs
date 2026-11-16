{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The account statement of the command report: the transactions of
-- every portfolio over a period, with the balance after each, between
-- an opening and a closing balance, as a report
-- ("Haskoin.Wallet.Report.Layout").
--
-- It is the statement a bank sends each month, for a wallet no bank
-- holds. An accountant or a tax office reads it as they read any
-- statement of an account: what was there at the start, what came in,
-- what went out, and what was there at the end. The balance statement
-- ("Haskoin.Wallet.Report.BalanceStatement") says what the wallets held
-- on a day. This one says how they got there.
--
-- = What an account is
--
-- A portfolio on a network is one account: it has one balance in one
-- asset, and each of its transactions changes that balance. So the
-- statement has a table for each portfolio on each network, as a bank
-- has a statement for each account in each currency. The journal for
-- ledger draws the same line ("Haskoin.Wallet.Cli.Journal").
--
-- A payment from one portfolio to another is in both tables: sent in
-- the first, received in the second. Each table is then whole on its
-- own, and adds up from its opening balance to its closing balance.
--
-- = The period
--
-- A statement covers a period of whole days on the clock of the
-- computer, from the start of its first day (t'PeriodStart'). It stops
-- at the end of its last day, or at the moment it is made when that day
-- is not over (t'StatementDate'). Without any date, the command asks for
-- the month that is running, as a bank does.
--
-- The opening balance is the balance at the end of the day before the
-- period. The closing balance is the balance at the end of the period.
-- Both are worked out as the balance statement works out the balance of
-- a day, by the same rule ('lastBlockCounted'). So two things hold,
-- which a reader will check:
--
-- * The closing balance of a statement is the opening balance of the
--   statement of the period that follows it.
--
-- * The closing balance is the balance a balance statement gives for
--   the same day.
--
-- The rule counts whole blocks: every block up to the last one dated
-- in time. The transactions of the period are then those of the blocks
-- above the last block of the opening balance, up to the last block of
-- the closing balance. The dates of the blocks do not always follow
-- their order, so a line may now and then be dated a little outside the
-- period. Cutting by the date of each transaction instead would list
-- every line inside the period, and the balances would no longer be
-- states the chain went through. The statement says so in a note, and
-- names the blocks at both ends.
--
-- = The lines
--
-- A line is a confirmed transaction: its date, what it is, what the
-- portfolio received or sent, and the balance after it. An amount sent
-- includes the fee, since the fee left the portfolio with the payment.
-- The line states the fee when the portfolio paid it.
--
-- A transaction is described as the journal for ledger names it
-- ('transactionName'): by its label, or by the label of the contact it
-- pays, or by the label of the address it was received at, or else by
-- what it did. The two documents then call a transaction the same
-- thing.
--
-- Below the description, each line names the transaction by its
-- identifier on the chain, whole, and the block it is in. They are what
-- lets a reader check a line against the chain without trusting the
-- statement.
--
-- Unconfirmed transactions are left out, as in the balance statement,
-- and so are dead ones. A statement that stops now says in a note what
-- the unconfirmed ones would change.
--
-- = Values in a currency
--
-- The statement takes the prices the balance statement takes
-- (t'Valuation'). A price is the price of a moment, and the user gives
-- one for each asset. So the statement values the closing balances,
-- which are balances of one moment, and nothing else. The lines have
-- no value: each would need the price of its own day, which hw does not
-- have. The opening balances have none either, for the same reason.
--
-- = What it shares with the balance statement
--
-- The facts at the top, the first notes, the section that names the
-- blocks, the reference and the check of the prices are those of every
-- statement ("Haskoin.Wallet.Report.Statement"). The style is that of
-- every report.
module Haskoin.Wallet.Report.AccountStatement
  ( PeriodStart (..),
    AccountStatement (..),
    AssetActivity (..),
    PortfolioActivity (..),
    StatementLine (..),
    accountStatement,
    accountsValued,
    accountBlocksUnnamed,
    namingAccountBlocks,
    accountStatementReference,
    accountStatementReport,
    shortDate,
  )
where

import Control.Monad (forM, unless)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime (..), TimeZone, ZonedTime (..), defaultTimeLocale, formatTime)
import Haskoin
import Haskoin.Wallet.Cli.Journal (satoshisText, transactionName)
import Haskoin.Wallet.Cli.Sheet (ZoneAt)
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), WalletNaming (..), portfolioRefName)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.Price (priceText, valueAt, valueText)
import Haskoin.Wallet.Report.Statement
import Numeric.Natural (Natural)

{- The statement -}

-- | The start of the period of a statement: its first day, on the clock
-- of the computer, with the zone the clock has at the midnight that
-- starts that day, so that the statement can say which midnight it
-- means
data PeriodStart = PeriodStart !Day !TimeZone
  deriving (Eq, Show)

-- | The transactions of the wallets over a period. Each network is an
-- asset, with what happened to every portfolio on it.
data AccountStatement = AccountStatement
  { accountsStart :: !PeriodStart,
    -- | Where the period stops: at the end of its last day, or at the
    -- moment the statement is made
    accountsEnd :: !StatementDate,
    -- | What the statement covers: every wallet, one wallet or one
    -- portfolio
    accountsScope :: !StatementScope,
    -- | Whether the statement names the wallets: only when the database
    -- has several, as in every view
    accountsNaming :: !WalletNaming,
    -- | The wallets the statement covers, each with its description and
    -- how many portfolios it has
    accountsWallets :: ![(Natural, Text, Int)],
    -- | Who holds the wallets, as the user says it, or nothing
    accountsHolder :: !Text,
    -- | The prices the closing balances are valued at, when the user
    -- gave some
    accountsValuation :: !(Maybe Valuation),
    -- | The networks hw shows, in their order
    accountsAssets :: ![AssetActivity]
  }
  deriving (Eq, Show)

-- | What happened to the portfolios on a network over the period
data AssetActivity = AssetActivity
  { activityNetwork :: !Network,
    -- | The block the last sync reached. Nothing for a network that was
    -- never synced.
    activitySynced :: !(Maybe BlockNamed),
    -- | The last block the opening balances count. The transactions of
    -- the period are in the blocks above it. Nothing when no
    -- transaction came before the period.
    activityOpeningBlock :: !(Maybe BlockNamed),
    -- | The last block the statement counts, which the closing balances
    -- stop at. Nothing when the statement counts no transaction on the
    -- network.
    activityLastBlock :: !(Maybe BlockNamed),
    -- | The unconfirmed transactions a statement that stops now leaves
    -- out: how many, and what they add up to. A statement that stops at
    -- the end of a day has none: they all came after it.
    activityUnconfirmed :: !(Int, Integer),
    -- | Every portfolio, with or without a transaction
    activityPortfolios :: ![PortfolioActivity]
  }
  deriving (Eq, Show)

-- | The account of a portfolio on a network over the period: what it
-- held at the start, and its transactions, the oldest first
data PortfolioActivity = PortfolioActivity
  { activityRef :: !PortfolioRef,
    activityDescription :: !Text,
    activityOpening :: !Natural,
    activityLines :: ![StatementLine]
  }
  deriving (Eq, Show)

-- | A line of an account: a confirmed transaction, as its portfolio
-- sees it
data StatementLine = StatementLine
  { -- | The day its block is dated on, on the clock of the computer.
    -- Nothing for a block without a time.
    lineDay :: !(Maybe Day),
    -- | What the transaction is, as the journal for ledger names it
    lineName :: !Text,
    lineTxid :: !TxHash,
    lineBlock :: !BlockHeight,
    -- | What the portfolio gains, or loses below zero, fee included
    lineAmount :: !Integer,
    -- | The fee, when the portfolio paid it: every input of the
    -- transaction is a coin of the portfolio. It is part of the amount.
    lineFee :: !(Maybe Natural),
    -- | The balance of the portfolio on the network after the
    -- transaction
    lineBalance :: !Natural
  }
  deriving (Eq, Show)

-- | What a portfolio received over the period: its gains added up
activityReceived :: PortfolioActivity -> Integer
activityReceived activity = sum [lineAmount line | line <- activityLines activity, lineAmount line > 0]

-- | What a portfolio sent over the period, fees included: its losses
-- added up, as a number above zero
activitySent :: PortfolioActivity -> Integer
activitySent activity = sum [negate (lineAmount line) | line <- activityLines activity, lineAmount line < 0]

-- | What a portfolio held at the end of the period: the balance after
-- its last line, or its opening balance when it has no line
activityClosing :: PortfolioActivity -> Natural
activityClosing activity = case activityLines activity of
  [] -> activityOpening activity
  accountLines -> lineBalance (last accountLines)

-- | What the portfolios held on a network at the end of the period
assetClosing :: AssetActivity -> Natural
assetClosing = sum . map activityClosing . activityPortfolios

-- | Whether an account has something to list: a balance at the start,
-- or a transaction in the period
activityShown :: PortfolioActivity -> Bool
activityShown activity = activityOpening activity > 0 || not (null (activityLines activity))

-- | The transactions of a network in the period, each counted once: a
-- payment from one portfolio to another is one transaction, on two
-- accounts
assetTransactions :: AssetActivity -> Int
assetTransactions asset = length (nubOrd [lineTxid line | activity <- activityPortfolios asset, line <- activityLines activity])

{- Working it out -}

-- | The account statement of the portfolios a scope covers, over the
-- period that starts and stops as given, on the networks given, each
-- with the block its last sync reached. The histories are those of
-- every portfolio of the database, covered or not: the blocks at both
-- ends of the period are found among them all, and a payment to a
-- portfolio the statement leaves out is still known as a transfer
-- (t'StatementScope'). The function gives the zone of the clock of the
-- computer at a time, which says the day a block is dated on. The
-- period must not stop before it starts, which the command checks.
--
-- A statement that stops at the end of a day is refused when a network
-- was last synced on that day or before ('refuseStale'). Any statement
-- is refused when the history takes the balance of a portfolio below
-- zero, which a sound history never does.
accountStatement :: StatementScope -> ZoneAt -> PeriodStart -> StatementDate -> [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> Either String AccountStatement
accountStatement asked zoneAt start@(PeriodStart firstDay startZone) end networks histories = do
  mapM_ (uncurry (refuseStale zoneAt end)) networks
  assets <- forM networks $ \(net, synced) -> do
    let onNetwork shown = [(history, transactions) | history <- shown, (poolNet, transactions) <- historyPools history, poolNet == net]
        everyPool = onNetwork histories
        everyTransaction = concatMap (map listedTxInfo . snd) everyPool
        -- The opening balance is the balance at the end of the day
        -- before the period
        openingBlock = lastBlockCounted zoneAt (AtEndOf (pred firstDay) startZone) everyTransaction
        lastBlock = lastBlockCounted zoneAt end everyTransaction
        -- Every portfolio that sees each transaction, to name it
        seenBy = Map.fromListWith (flip (<>)) [(txid, [(historyRef history, info)]) | (history, transactions) <- everyPool, info <- listedTxInfo <$> transactions, Just txid <- [txInfoHash info]]
        pools = onNetwork covered
    portfolios <- forM pools $ \(history, transactions) ->
      portfolioActivity zoneAt net seenBy openingBlock lastBlock history transactions
    return
      AssetActivity
        { activityNetwork = net,
          activitySynced = syncedBlock zoneAt <$> synced,
          activityOpeningBlock = countedBlock zoneAt synced everyTransaction <$> openingBlock,
          activityLastBlock = countedBlock zoneAt synced everyTransaction <$> lastBlock,
          activityUnconfirmed = case end of
            AtPreparation -> unconfirmedLeftOut (concatMap snd pools)
            AtEndOf _ _ -> (0, 0),
          activityPortfolios = portfolios
        }
  return
    AccountStatement
      { accountsStart = start,
        accountsEnd = end,
        accountsScope = scope,
        accountsNaming = databaseNaming histories,
        accountsWallets = walletsOf covered,
        accountsHolder = "",
        accountsValuation = Nothing,
        accountsAssets = assets
      }
  where
    scope = scopeAmong histories asked
    -- The portfolios the statement shows
    covered = [history | history <- histories, covers scope (historyRef history)]

-- | The account of a portfolio on a network, given its transactions as
-- the history lists them, the newest first, the last block of the
-- opening balance and the last block counted, and every portfolio that
-- sees each transaction of the network.
--
-- The confirmed transactions are put in the order of the chain. Those
-- up to the last block of the opening balance add up to the opening
-- balance. Those above it, up to the last block counted, are the lines,
-- each with the balance after it.
portfolioActivity :: ZoneAt -> Network -> Map.Map TxHash [(PortfolioRef, TxInfo)] -> Maybe BlockHeight -> Maybe BlockHeight -> PortfolioHistory -> [ListedTx] -> Either String PortfolioActivity
portfolioActivity zoneAt net seenBy openingBlock lastBlock history transactions = do
  unless (all (>= 0) (opening : balances)) . Left $
    "The transactions of "
      <> cs (portfolioRefName WithWallets ref)
      <> " on "
      <> net.name
      <> " take its balance below zero: hw sync --full reads them again"
  return
    PortfolioActivity
      { activityRef = ref,
        activityDescription = historyDescription history,
        activityOpening = fromInteger opening,
        activityLines = zipWith line inPeriod balances
      }
  where
    ref = historyRef history
    -- The history lists the newest first. Turned around, a transaction
    -- comes after the ones it spends from, and the sort by block keeps
    -- that order inside a block.
    confirmed =
      sortOn
        (\(height, _, _, _) -> height)
        [ (height, time, status, info)
          | info <- reverse (listedTxInfo <$> transactions),
            Just (height, time) <- [confirmedIn info],
            OnChain status <- [txInfoStatus info]
        ]
    amountOf (_, _, _, info) = summaryAmount (txInfoSummary info)
    opening = sum [amountOf row | row@(height, _, _, _) <- confirmed, Just height <= openingBlock]
    inPeriod = [row | row@(height, _, _, _) <- confirmed, Just height > openingBlock, Just height <= lastBlock]
    -- The balance after each line
    balances = drop 1 (scanl (+) opening (amountOf <$> inPeriod))
    line (height, time, status, info) balance =
      StatementLine
        { lineDay = dayAt zoneAt <$> time,
          lineName = transactionName net status ((ref, info) NonEmpty.:| others),
          lineTxid = chainTxid status,
          lineBlock = height,
          lineAmount = summaryAmount summary,
          lineFee = if ownTransaction summary then Just (summaryFee summary) else Nothing,
          lineBalance = fromInteger balance
        }
      where
        summary = txInfoSummary info
        -- The other portfolios that see the transaction. This one comes
        -- first, so that its own label names the line.
        others = [seen | seen@(other, _) <- Map.findWithDefault [] (chainTxid status) seenBy, other /= ref]

{- Prices and block hashes -}

-- | The statement with its closing balances valued at the prices given.
-- It is refused when a price names a network the statement does not
-- cover, when a network has two prices, and when an asset the wallets
-- hold at the end of the period has no price ('checkPrices').
accountsValued :: Valuation -> AccountStatement -> Either String AccountStatement
accountsValued valuation statement = do
  checkPrices
    valuation
    (activityNetwork <$> accountsAssets statement)
    [activityNetwork asset | asset <- accountsAssets statement, assetClosing asset > 0]
  return statement {accountsValuation = Just valuation}

-- | The blocks a statement names whose hash it does not know yet, each
-- with its network: the last block of the opening balances and the last
-- block counted. They are what the command asks the server.
accountBlocksUnnamed :: AccountStatement -> [(Network, BlockHeight)]
accountBlocksUnnamed statement =
  [ (activityNetwork asset, height)
    | asset <- accountsAssets statement,
      -- The two blocks are one when the period has no transaction
      height <- nubOrd [blockNamedHeight block | Just block <- [activityOpeningBlock asset, activityLastBlock asset], blockNamedHash block == Nothing]
  ]

-- | The statement with the hashes given for the blocks it names, each
-- for a network and a height. A block whose hash the statement already
-- knows keeps it, and one no hash is given for stays without.
namingAccountBlocks :: [(Network, BlockHeight, BlockHash)] -> AccountStatement -> AccountStatement
namingAccountBlocks hashes statement = statement {accountsAssets = named <$> accountsAssets statement}
  where
    named asset =
      asset
        { activityOpeningBlock = withBlockHash hashes (activityNetwork asset) <$> activityOpeningBlock asset,
          activityLastBlock = withBlockHash hashes (activityNetwork asset) <$> activityLastBlock asset
        }

{- The reference -}

-- | The reference of a statement made at the time given: eight
-- hexadecimal digits in two groups, as in 3F9A-52C1, worked out from
-- what the statement states ('referenceOf'): its period, its holder,
-- its scope when it is narrowed down, the opening balance and every
-- line of each account, and the prices.
--
-- Two statements with the same reference state the same transactions
-- over the same period. The time a statement was made is left out, and
-- so are the blocks of the last sync: the statement of a month made
-- twice has one reference as long as nothing in it changed. A statement
-- that stops now is dated to the minute it was made, so each has its
-- own.
accountStatementReference :: ZonedTime -> AccountStatement -> Text
accountStatementReference prepared statement =
  referenceOf $
    ["from " <> shown firstDay, dateStated prepared (accountsEnd statement), "holder " <> accountsHolder statement]
      <> scopeStated (accountsScope statement)
      <> concat
        [ Text.unwords (account <> ["opening", shown (activityOpening activity)])
            : [Text.unwords (account <> [txHashToHex (lineTxid line), shown (lineAmount line)]) | line <- activityLines activity]
          | asset <- accountsAssets statement,
            activity <- activityPortfolios asset,
            let ref = activityRef activity
                account = [cs (activityNetwork asset).name, shown (refWallet ref), shown (refPortfolio ref)]
        ]
      <> valuationStated (accountsValuation statement)
  where
    PeriodStart firstDay _ = accountsStart statement
    shown :: (Show a) => a -> Text
    shown = Text.pack . show

{- The report -}

-- | A day written short, as in 14 Nov 2023, for the lines of a table.
-- The month is a word, so that no reader takes the day for the month.
shortDate :: Day -> Text
shortDate = Text.pack . formatTime defaultTimeLocale "%-d %b %Y"

-- | The statement as a report, made at the time given: the facts about
-- it, a summary by asset, the activity of each portfolio, the
-- transactions of each portfolio, the blocks it names, and the notes
-- that say who made it and how it was worked out.
--
-- A network without any transaction or balance is in the summary, with
-- zeros: that nothing happened to an asset is part of what the
-- statement says. It has no table of its own further down, and neither
-- has a portfolio that held nothing and did nothing.
--
-- With prices, the summary is followed by the value of each closing
-- balance, and their total.
accountStatementReport :: ZonedTime -> AccountStatement -> Report
accountStatementReport prepared statement =
  Report
    { reportKind = statementKind "transactions",
      reportTitle = title,
      reportSubtitle = period,
      reportFooter = title <> " \xB7 " <> period <> " \xB7 Ref. " <> reference,
      reportBlocks =
        [FactsBlock facts, SectionBlock "Summary", TableBlock summary]
          <> (if null valuation then [] else [GroupBlock "Value of the closing balances" "", TableBlock values])
          <> (if null active then [] else SectionBlock "Activity by portfolio" : concatMap byPortfolio active)
          <> (if null active then [] else SectionBlock "Transactions by portfolio" : concatMap transactionsOf active)
          <> (SectionBlock "Blockchain data" : concatMap blocksOf assets)
          <> [SectionBlock "Notes", NotesBlock notes]
    }
  where
    title = statementTitle "Account Statement"
    assets = accountsAssets statement
    naming = accountsNaming statement
    valuation = accountsValuation statement
    preparedTime = zonedTimeToLocalTime prepared
    reference = accountStatementReference prepared statement
    PeriodStart firstDay startZone = accountsStart statement
    -- The last day of the period: the day the statement is made, when
    -- it stops now
    lastDay = case accountsEnd statement of
      AtEndOf day _ -> day
      AtPreparation -> localDay preparedTime
    period = longDate firstDay <> " to " <> longDate lastDay
    -- The assets with an account to list
    active = [asset | asset <- assets, any activityShown (activityPortfolios asset)]
    -- An amount of satoshis in coins, with its eight decimals
    amount :: (Integral number) => number -> Text
    amount = satoshisText . toInteger
    currency = maybe "" valuationCurrency valuation

    facts =
      holderFacts (accountsHolder statement)
        <> [ Fact
               "Statement period"
               [ "From " <> longDate firstDay <> ", 00:00" <> inZone startZone,
                 case accountsEnd statement of
                   -- Midnight at the end of a day is written 24:00, so
                   -- that the date beside it is the last day of the
                   -- period
                   AtEndOf day zone -> "To " <> longDate day <> ", 24:00" <> inZone zone
                   AtPreparation -> "To " <> longTime preparedTime <> inZone (zonedTimeZone prepared)
               ]
           ]
        <> preparationFacts prepared reference
        <> coverageFacts (accountsScope statement) naming (accountsWallets statement) (activityNetwork <$> assets)
        <> valuationFacts valuation
        <> syncFacts [(activityNetwork asset, activitySynced asset) | asset <- assets]

    -- The four amounts of an account, or of several added up: what was
    -- there at the start, what came in, what went out, and what was
    -- there at the end
    amountCells activities =
      [ plainCell (amount (sum (toInteger . activityOpening <$> activities))),
        plainCell (amount (sum (activityReceived <$> activities))),
        plainCell (amount (sum (activitySent <$> activities))),
        plainCell (amount (sum (toInteger . activityClosing <$> activities)))
      ]
    -- The columns of a summary, after its first one: the shares are
    -- their widths in points
    amountColumns =
      [ Column "Opening balance" 90 AlignRight,
        Column "Received" 90 AlignRight,
        Column "Sent" 90 AlignRight,
        Column "Closing balance" 90 AlignRight
      ]

    summary =
      Table
        (Column "Asset" 122 AlignLeft : amountColumns)
        [ Row BodyRow (remarkCell (assetTitle (activityNetwork asset)) (counted (assetTransactions asset) "transaction") : amountCells (activityPortfolios asset))
          | asset <- assets
        ]

    -- The value of each closing balance at the price of its asset, and
    -- their total
    values =
      Table
        [ Column "Asset" 122 AlignLeft,
          Column "Closing balance" 120 AlignRight,
          Column ("Price (" <> currency <> ")") 120 AlignRight,
          Column ("Value (" <> currency <> ")") 120 AlignRight
        ]
        ( [ Row
              BodyRow
              [ plainCell (assetTitle net),
                plainCell (amount (assetClosing asset)),
                plainCell (maybe "" priceText (priceOf valuation net)),
                plainCell (maybe "" (\price -> valueText (valueAt price (assetClosing asset))) (priceOf valuation net))
              ]
            | asset <- assets,
              let net = activityNetwork asset
          ]
            <> [Row TotalRow [plainCell "Total value", plainCell "", plainCell "", plainCell (valueText totalValue)]]
        )
    totalValue = sum [valueAt price (assetClosing asset) | asset <- assets, Just price <- [priceOf valuation (activityNetwork asset)]]

    -- What happened to each portfolio on a network, and the total
    byPortfolio asset =
      [ GroupBlock (assetTitle net) "",
        TableBlock $
          Table
            (Column "Portfolio" 122 AlignLeft : amountColumns)
            ( [ Row BodyRow (remarkCell (portfolioRefName naming (activityRef activity)) (portfolioRemark activity) : amountCells [activity])
                | activity <- activityPortfolios asset
              ]
                <> [Row TotalRow (plainCell ("Total " <> assetName net) : amountCells (activityPortfolios asset))]
            )
      ]
      where
        net = activityNetwork asset
    -- The description of a portfolio, when it has one, and how many
    -- transactions it has in the period
    portfolioRemark activity =
      Text.intercalate " \xB7 " $
        [activityDescription activity | not (Text.null (activityDescription activity))]
          <> [counted (length (activityLines activity)) "transaction"]

    -- The account of each portfolio on a network, for those with
    -- something to list: its opening balance, its lines, then its
    -- totals and its closing balance
    transactionsOf asset =
      concat
        [ [ GroupBlock (portfolioRefName naming (activityRef activity) <> " \xB7 " <> assetTitle net) (activityDescription activity),
            TableBlock $
              Table
                [ Column "Date" 72 AlignLeft,
                  Column "Description" 180 AlignLeft,
                  Column "Received" 74 AlignRight,
                  Column "Sent" 74 AlignRight,
                  Column ("Balance (" <> assetUnit net <> ")") 82 AlignRight
                ]
                ( [Row BodyRow [plainCell (shortDate firstDay), plainCell "Opening balance", plainCell "", plainCell "", plainCell (amount (activityOpening activity))]]
                    <> (lineRow <$> activityLines activity)
                    <> [ Row
                           TotalRow
                           [ plainCell (shortDate lastDay),
                             plainCell "Closing balance",
                             plainCell (amount (activityReceived activity)),
                             plainCell (amount (activitySent activity)),
                             plainCell (amount (activityClosing activity))
                           ]
                       ]
                )
          ]
          | activity <- activityPortfolios asset,
            activityShown activity
        ]
      where
        net = activityNetwork asset
    -- A line of an account. What the portfolio gained is in the column
    -- of what it received, and what it lost in the column of what it
    -- sent. The identifier of the transaction is wider than any column,
    -- so it runs on below the amounts.
    lineRow line =
      Row
        BodyRow
        [ plainCell (maybe "" shortDate (lineDay line)),
          wideRemarkCell (lineName line) (lineRemark line),
          plainCell (if lineAmount line >= 0 then amount (lineAmount line) else ""),
          plainCell (if lineAmount line < 0 then amount (negate (lineAmount line)) else ""),
          plainCell (amount (lineBalance line))
        ]
    lineRemark line =
      Text.intercalate " \xB7 " $
        [txHashToHex (lineTxid line), "Block " <> Text.pack (show (lineBlock line))]
          <> ["Fee " <> amount fee | Just fee <- [lineFee line]]

    -- The three blocks the statement names on a network, each with its
    -- height, its date and its hash
    blocksOf asset =
      [ GroupBlock (assetTitle (activityNetwork asset)) "",
        FactsBlock
          [ Fact "Last block of the sync" (maybe ["Not synchronised"] blockLines (activitySynced asset)),
            Fact "Last block before the period" (maybe ["None: no transaction came before the period"] blockLines (activityOpeningBlock asset)),
            Fact "Last block counted" (maybe ["None: the statement counts no transaction"] blockLines (activityLastBlock asset))
          ]
      ]

    notes =
      preparationNotes (accountsScope statement) naming
        <> [ "The statement lists the confirmed transactions of each portfolio in the statement period, on each blockchain, between its opening balance and its closing balance. Each portfolio is an account on each blockchain. The balance on a line is the balance of the account after the transaction.",
             "An amount received is what a transaction added to the addresses of a portfolio, and an amount sent is what it took from them. An amount sent includes the transaction fee. A line states the fee when the portfolio paid all of it.",
             "A payment from one portfolio to another is listed in both accounts: sent by the first and received by the second. The totals of an asset count it on both sides."
               <> (if accountsScope statement == EveryWallet then "" else " A portfolio the statement does not cover has no account here, and a payment to it or from it without a label is still described as a transfer between portfolios."),
             "The opening balance counts the transactions whose block is dated before the first day of the period, on the clock of the computer that prepared the statement. "
               <> ( case accountsEnd statement of
                      AtEndOf _ _ -> "The closing balance counts those whose block is dated on the last day of the period or before."
                      AtPreparation -> "The closing balance counts every transaction confirmed in the blockchain data of the last synchronisation."
                  )
               <> " A block may be dated before the block that precedes it. Every block up to the last one dated in time is therefore counted, at each end of the period, so that each balance is a state the blockchain went through. A transaction may for that reason be listed with a date just outside the period. The section Blockchain data names the last block before the period and the last block counted.",
             Text.unwords ("Transactions that were not confirmed are not listed." : mapMaybe (\asset -> leftOutSentence (activityNetwork asset) (activityUnconfirmed asset)) assets),
             "A transaction is described by its label, or else by the label of the contact it pays, or else by the label of the address it was received at. Without a label, it is described by what it did. Below the description are the identifier of the transaction on the blockchain and the height of its block.",
             "The date of a transaction is the date of its block, on the clock of the computer that prepared the statement.",
             "Amounts are in units of each asset, to eight decimal places. One unit is 100,000,000 satoshis."
               <> (if null valuation then " The statement gives no value in a currency." else "")
           ]
        <> [ "Values are in "
               <> valuationCurrency prices
               <> ". The value of a closing balance is the balance times the price of its asset, rounded to the nearest hundredth. The holder gave the prices."
               <> (if Text.null (valuationSource prices) then "" else " Their source: " <> valuationSource prices <> ".")
               <> " The prices are those of the end of the period, so the transactions and the opening balances are not valued."
             | Just prices <- [valuation]
           ]
        <> [ blockNote,
             "The reference is worked out from the statement period, the holder, the opening balances, the transactions and the prices. Two statements with the same reference state the same."
           ]
