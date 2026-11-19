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
-- = What an asset received and sent
--
-- The amounts of an asset cover every account of the statement, so they
-- are those of the holder: what came from others, and what went to
-- others. A payment from one portfolio to another is neither. The reader
-- of a statement takes what was received for income, and adding up the
-- accounts would count such a payment as received and as sent each time
-- the holder moves money. So an asset counts each transaction once, by
-- what it changed for its portfolios together (@assetNetAmounts@). The
-- payment then counts for its fee, which did leave the holder.
--
-- The amounts still add up, from the opening balance to the closing
-- balance: the payment added the same amount to what was received and to
-- what was sent. Where the accounts of an asset are listed above their
-- total, a row states what was taken off both, so that each column adds
-- up too.
--
-- A column of transfers was the other choice. In the amounts of an asset
-- it would add up to nothing, and in those of an account it would need a
-- sign, or a column for each way, on a page with no room for six amounts.
--
-- The rule looks only at the accounts and the lines the statement lists.
-- A payment to a portfolio the statement leaves out went out of what it
-- covers, so it is sent, and the closing balance needs it to be.
--
-- = The period
--
-- A statement covers a period of whole days on the clock of the computer,
-- from the start of its first day (the type t'PeriodStart'). It stops at
-- the end of its last day, or at the moment it is made when that day is not
-- over (the type t'StatementDate'). Without any date, the command asks for
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
-- The line of a payment from one portfolio to another says more: that it
-- is a transfer, and which portfolio the other one is (@lineDescription@).
-- The journal has one entry for such a payment, whose postings name both
-- portfolios. An account has a line for its own side only, and a label
-- alone would let the reader take the line for a payment to somebody
-- else.
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
-- The statement takes the prices the balance statement takes (the type
-- t'Valuation'). A price is the price of a moment, and the user gives one
-- for each asset. So the statement values the closing balances, which are
-- balances of one moment, and nothing else. The lines have no value: each
-- would need the price of its own day, which hw does not have. The opening
-- balances have none either, for the same reason.
--
-- = A statement of some transactions
--
-- The holder may keep the statement to the transactions of some tags
-- ("Haskoin.Wallet.TxSelection"): those of a business, for the accountant
-- of that business. The statement is first worked out whole, and then
-- loses the lines the selection leaves out ('accountsSelected'). So a
-- line is the same in both, with the same date and the same description,
-- and the period is cut at the same blocks.
--
-- What it can no longer state is a balance. The lines left out moved
-- money too, and an opening balance with some of the lines below it would
-- add up to no closing balance. A reader who adds up a statement would
-- take that for an error. So a statement of some transactions has no
-- opening balance, no closing balance and no balance after a line. It
-- gives what the lines received, what they sent, and the difference. It
-- takes no price either, since a price values a closing balance.
--
-- Showing each line with the true balance of its portfolio after it was
-- the other choice, and it is what txs does on the screen. There the
-- reader knows the listing is filtered. A statement is handed to someone
-- who was not there when it was made.
--
-- It is titled a transaction statement, not an account statement, and
-- says in its facts and in its first note which transactions it lists.
-- The title is at the foot of every page, so a page on its own is not
-- taken for the statement of an account.
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
    accountsSelected,
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
import Data.Maybe (isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime (..), TimeZone, ZonedTime (..), defaultTimeLocale, formatTime)
import Haskoin
import Haskoin.Wallet.Cli.Journal (paymentReceived, paymentSent, satoshisText, saysWhatItDid, transactionName)
import Haskoin.Wallet.Cli.Sheet (ZoneAt)
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), WalletNaming (..), portfolioRefName)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.Price (priceText, valueAt, valueText)
import Haskoin.Wallet.Report.Statement
import Haskoin.Wallet.TxSelection (TxSelection (..), keepsTx)
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
    -- | Which transactions the statement lists, when it lists some only:
    -- what they have in common, in the words that follow "transactions",
    -- as in "tagged business". Nothing for the statement of every
    -- transaction.
    accountsSelection :: !(Maybe Text),
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
    -- transaction, every transaction counted. A statement of some
    -- transactions does not state it.
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

-- | What each transaction of a network changed for the portfolios of the
-- statement together: what they gained, or lost below zero. A payment
-- from one portfolio to another is one transaction, which changes the
-- portfolios together by its fee alone.
--
-- hw looks at a transaction the same way inside a portfolio, where a
-- payment with change is one loss and not a loss and a gain.
assetNetAmounts :: AssetActivity -> [Integer]
assetNetAmounts asset = Map.elems (Map.fromListWith (+) [(lineTxid line, lineAmount line) | activity <- activityPortfolios asset, line <- activityLines activity])

-- | What the portfolios of a network received from others over the
-- period: what moved from one of them to another is left out
assetReceived :: AssetActivity -> Integer
assetReceived asset = sum [gained | gained <- assetNetAmounts asset, gained > 0]

-- | What the portfolios of a network sent to others over the period,
-- fees included, as a number above zero. The fee of a payment from one
-- portfolio to another is part of it: it went to no portfolio.
assetSent :: AssetActivity -> Integer
assetSent asset = sum [negate lost | lost <- assetNetAmounts asset, lost < 0]

-- | What moved between the portfolios of a network over the period: what
-- the accounts received that the asset did not. The accounts sent that
-- much more than the asset too.
assetTransfers :: AssetActivity -> Integer
assetTransfers asset = sum (activityReceived <$> activityPortfolios asset) - assetReceived asset

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

-- | The account statement of the portfolios a scope covers, over the period
-- that starts and stops as given, on the networks given, each with the
-- block its last sync reached. The histories are those of every portfolio
-- of the database, covered or not: the blocks at both ends of the period
-- are found among them all, and a payment to a portfolio the statement
-- leaves out is still known as a transfer (the type t'StatementScope'). The
-- function gives the zone of the clock of the computer at a time, which
-- says the day a block is dated on. The period must not stop before it
-- starts, which the command checks.
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
      portfolioActivity zoneAt naming net seenBy openingBlock lastBlock history transactions
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
        accountsSelection = Nothing,
        accountsNaming = naming,
        accountsWallets = walletsOf covered,
        accountsHolder = "",
        accountsValuation = Nothing,
        accountsAssets = assets
      }
  where
    scope = scopeAmong histories asked
    naming = databaseNaming histories
    -- The portfolios the statement shows
    covered = [history | history <- histories, covers scope (historyRef history)]

-- | The account of a portfolio on a network, given its transactions as
-- the history lists them, the newest first, the last block of the
-- opening balance and the last block counted, and every portfolio that
-- sees each transaction of the network, which the lines name as the
-- database names its wallets.
--
-- The confirmed transactions are put in the order of the chain. Those
-- up to the last block of the opening balance add up to the opening
-- balance. Those above it, up to the last block counted, are the lines,
-- each with the balance after it.
portfolioActivity :: ZoneAt -> WalletNaming -> Network -> Map.Map TxHash [(PortfolioRef, TxInfo)] -> Maybe BlockHeight -> Maybe BlockHeight -> PortfolioHistory -> [ListedTx] -> Either String PortfolioActivity
portfolioActivity zoneAt naming net seenBy openingBlock lastBlock history transactions = do
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
          lineName = lineDescription (transferOf naming summary others) (summaryAmount summary) (transactionName net status ((ref, info) NonEmpty.:| others)),
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

{- A payment from one portfolio to another -}

-- | What makes a line a transfer: the transaction moved money between
-- its portfolio and other portfolios of the holder
data Transfer = Transfer
  { -- | Whether the whole of the line is a transfer. It is not when the
    -- transaction paid somebody else too, or was paid by somebody else
    -- too.
    transferWhole :: !Bool,
    -- | Where the money went or where it came from, as in "to
    -- portfolio1" and "from portfolio0"
    transferSide :: !Text
  }

-- | What makes a transfer of the line of a portfolio, given what the
-- transaction means to that portfolio, and every other portfolio that
-- sees the transaction. Nothing for a line that is none.
--
-- The line of a portfolio that lost money is a transfer to the
-- portfolios that gained some, and the line of a portfolio that gained
-- money a transfer from those that lost some. What the portfolios gained
-- and lost together tells whether others had a part in it. A line that
-- lost money is a transfer in whole when the portfolios together lost
-- the fee at most: nothing went to others. A line that gained money is
-- one when the portfolios together gained nothing: nothing came from
-- others.
transferOf :: WalletNaming -> TxSummary -> [(PortfolioRef, TxInfo)] -> Maybe Transfer
transferOf naming summary others
  | null otherSide = Nothing
  | otherwise =
      Just
        Transfer
          { transferWhole = if paid then together >= negate (toInteger (summaryFee summary)) else together <= 0,
            transferSide = (if paid then "to " else "from ") <> Text.intercalate ", " (portfolioRefName naming <$> otherSide)
          }
  where
    own = summaryAmount summary
    paid = own < 0
    amounts = [(other, summaryAmount (txInfoSummary info)) | (other, info) <- others]
    -- The portfolios the transaction changed the other way
    otherSide = [other | (other, amount) <- amounts, own /= 0, signum amount == negate (signum own)]
    together = own + sum (snd <$> amounts)

-- | The description of a line, given what makes it a transfer, the
-- amount of the line, and the name of its transaction.
--
-- A line that is no transfer has the name of its transaction. A transfer
-- keeps the label of its transaction, and says after it that it is a
-- transfer, and with which portfolio: the label alone would hide from
-- the reader that the money stayed with the holder. Without a label, it
-- is described as the transfer it is. The name of what the transaction
-- did would say less, and it describes the transaction, not the line: a
-- payment to somebody else and to another portfolio reads "Payment sent"
-- in the account that received a part of it.
lineDescription :: Maybe Transfer -> Integer -> Text -> Text
lineDescription Nothing _ name = name
lineDescription (Just transfer) amount name
  | not (saysWhatItDid name) = name <> " (" <> stated <> ")"
  | transferWhole transfer = "Transfer " <> transferSide transfer
  | otherwise = (if amount < 0 then paymentSent else paymentReceived) <> " (" <> stated <> ")"
  where
    stated = (if transferWhole transfer then "transfer " else "in part a transfer ") <> transferSide transfer

{- Some transactions only -}

-- | The statement of the transactions a selection keeps, given the
-- statement of every transaction: each account keeps the lines the
-- selection keeps for its portfolio, and the statement says which
-- transactions it lists.
--
-- A line is left as the whole statement worked it out. The balance it
-- holds counts the transactions left out too, and so does the opening
-- balance of its account: the report of a statement of some
-- transactions states neither ('accountStatementReport'). Such a
-- statement is not to be valued: the command refuses prices with a
-- selection.
accountsSelected :: TxSelection -> AccountStatement -> AccountStatement
accountsSelected selection statement =
  statement
    { accountsSelection = Just (selectionStated selection),
      accountsAssets = [asset {activityPortfolios = keptLines (activityNetwork asset) <$> activityPortfolios asset} | asset <- accountsAssets statement]
    }
  where
    keptLines net activity =
      activity {activityLines = [line | line <- activityLines activity, keepsTx selection (activityRef activity) net (lineTxid line)]}

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
-- line of each account, and the prices. A statement of some transactions
-- states which ones in place of the opening balances, which it does not
-- give.
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
      <> ["selection " <> stated | Just stated <- [accountsSelection statement]]
      <> concat
        [ [Text.unwords (account <> ["opening", shown (activityOpening activity)]) | isNothing (accountsSelection statement)]
            <> [Text.unwords (account <> [txHashToHex (lineTxid line), shown (lineAmount line)]) | line <- activityLines activity]
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
-- The summary and the total of an asset state what its portfolios
-- received from others and sent to others (@assetReceived@, @assetSent@).
-- A row above the total states what moved between them.
--
-- A network without any transaction or balance is in the summary, with
-- zeros: that nothing happened to an asset is part of what the
-- statement says. It has no table of its own further down, and neither
-- has a portfolio that held nothing and did nothing.
--
-- With prices, the summary is followed by the value of each closing
-- balance, and their total.
--
-- A statement of some transactions has the same sections under a title
-- of its own. Its tables give what the lines received and sent and the
-- net amount, where the others give balances, and its first note says
-- which transactions it lists and why it states no balance.
accountStatementReport :: ZonedTime -> AccountStatement -> Report
accountStatementReport prepared statement =
  Report
    { reportKind = statementKind (if whole then "transactions" else "transactions, selected by tag"),
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
    selection = accountsSelection statement
    -- Whether the statement lists every transaction, and so states
    -- balances
    whole = isNothing selection
    title = statementTitle (if whole then "Account Statement" else "Transaction Statement")
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
    active = [asset | asset <- assets, any listed (activityPortfolios asset)]
    -- Whether an account has something to list. A balance at the start
    -- is nothing to list in a statement that states no balance.
    listed activity = if whole then activityShown activity else not (null (activityLines activity))
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
        <> [Fact "Transactions" ["Only those " <> stated] | Just stated <- [selection]]
        <> preparationFacts prepared reference
        <> coverageFacts (accountsScope statement) naming (accountsWallets statement) (activityNetwork <$> assets)
        <> valuationFacts valuation
        <> syncFacts [(activityNetwork asset, activitySynced asset) | asset <- assets]

    -- The amounts of a row of a summary: what was there at the start,
    -- what came in, what went out, and what was there at the end. A
    -- statement of some transactions has no balance to give, and gives
    -- what came in, what went out, and the difference.
    amountCells opening received sent closing =
      plainCell . amount <$> if whole then [opening, received, sent, closing] else [received, sent, received - sent]
    -- The amounts of an account
    accountCells activity =
      amountCells (toInteger (activityOpening activity)) (activityReceived activity) (activitySent activity) (toInteger (activityClosing activity))
    -- The amounts of an asset: those of its accounts added up, less what
    -- moved from one account to another, which the holder neither
    -- received nor sent
    assetCells asset =
      amountCells (sum (toInteger . activityOpening <$> activityPortfolios asset)) (assetReceived asset) (assetSent asset) (toInteger (assetClosing asset))
    -- The row that leads from the accounts of an asset to its total: what
    -- moved between the accounts, taken off what they received and off
    -- what they sent. It leaves the balances as they are, and the net
    -- amount of a statement of some transactions too. An asset without a
    -- transfer has no such row.
    transfersRow asset =
      [ Row BodyRow (plainCell "Transfers between portfolios" : [plainCell "" | whole] <> [plainCell (amount (negate moved)), plainCell (amount (negate moved)), plainCell ""])
        | let moved = assetTransfers asset,
          moved > 0
      ]
    -- The columns of a summary, after its first one: the shares are
    -- their widths in points
    amountColumns =
      [ Column name 90 AlignRight
        | name <- if whole then ["Opening balance", "Received", "Sent", "Closing balance"] else ["Received", "Sent", "Net amount"]
      ]
    -- The first column of a summary, which takes the width the amounts
    -- leave it
    namedColumn name = Column name (482 - sum (columnShare <$> amountColumns)) AlignLeft

    summary =
      Table
        (namedColumn "Asset" : amountColumns)
        [ Row BodyRow (remarkCell (assetTitle (activityNetwork asset)) (counted (assetTransactions asset) "transaction") : assetCells asset)
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

    -- What happened to each portfolio on a network, and the total, which
    -- counts once what moved from one portfolio to another
    byPortfolio asset =
      [ GroupBlock (assetTitle net) "",
        TableBlock $
          Table
            (namedColumn "Portfolio" : amountColumns)
            ( [ Row BodyRow (remarkCell (portfolioRefName naming (activityRef activity)) (portfolioRemark activity) : accountCells activity)
                | activity <- activityPortfolios asset
              ]
                <> transfersRow asset
                <> [Row TotalRow (plainCell ("Total " <> assetName net) : assetCells asset)]
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
    -- totals and its closing balance. A statement of some transactions
    -- has the lines and their totals, without the column of balances,
    -- whose width goes to the descriptions.
    transactionsOf asset =
      concat
        [ [ GroupBlock (portfolioRefName naming (activityRef activity) <> " \xB7 " <> assetTitle net) (activityDescription activity),
            TableBlock $
              Table
                ( [ Column "Date" 72 AlignLeft,
                    Column "Description" (if whole then 180 else 262) AlignLeft,
                    Column "Received" 74 AlignRight,
                    Column "Sent" 74 AlignRight
                  ]
                    <> [Column ("Balance (" <> assetUnit net <> ")") 82 AlignRight | whole]
                )
                ( [Row BodyRow [plainCell (shortDate firstDay), plainCell "Opening balance", plainCell "", plainCell "", plainCell (amount (activityOpening activity))] | whole]
                    <> (lineRow <$> activityLines activity)
                    <> [ Row TotalRow $
                           (if whole then [plainCell (shortDate lastDay), plainCell "Closing balance"] else [plainCell "", plainCell "Total"])
                             <> [plainCell (amount (activityReceived activity)), plainCell (amount (activitySent activity))]
                             <> [plainCell (amount (activityClosing activity)) | whole]
                       ]
                )
          ]
          | activity <- activityPortfolios asset,
            listed activity
        ]
      where
        net = activityNetwork asset
    -- A line of an account. What the portfolio gained is in the column
    -- of what it received, and what it lost in the column of what it
    -- sent. The identifier of the transaction is wider than any column,
    -- so it runs on below the amounts.
    lineRow line =
      Row BodyRow $
        [ plainCell (maybe "" shortDate (lineDay line)),
          wideRemarkCell (lineName line) (lineRemark line),
          plainCell (if lineAmount line >= 0 then amount (lineAmount line) else ""),
          plainCell (if lineAmount line < 0 then amount (negate (lineAmount line)) else "")
        ]
          <> [plainCell (amount (lineBalance line)) | whole]
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
        <> [ case selection of
               Nothing -> "The statement lists the confirmed transactions of each portfolio in the statement period, on each blockchain, between its opening balance and its closing balance. Each portfolio is an account on each blockchain. The balance on a line is the balance of the account after the transaction."
               Just stated -> "The statement lists some of the transactions only: the confirmed transactions of each portfolio in the statement period, on each blockchain, that are " <> stated <> ". A tag is a word the holder gave to a transaction, to an address or to a contact, to sort the payments. The portfolios may have other transactions in the period, so the statement states no balance.",
             "An amount received is what a transaction added to the addresses of a portfolio, and an amount sent is what it took from them. An amount sent includes the transaction fee. A line states the fee when the portfolio paid all of it."
               <> (if whole then "" else " A net amount is what was received less what was sent."),
             "A payment from one portfolio to another is a transfer. It is listed "
               <> (if whole then "in both accounts" else "for each of the two portfolios it is selected for")
               <> ": sent by the first and received by the second. Each line names the other portfolio. The amounts received and sent of an asset are what its portfolios received from others and sent to others: they leave out what a transfer "
               <> (if whole then "" else "listed for both portfolios ")
               <> "moved, and count its fee as sent. Where there are transfers, the row Transfers between portfolios states what is left out."
               <> (if accountsScope statement == EveryWallet then "" else " A portfolio the statement does not cover has no account here. A transfer to it or from it is counted as sent or received, and its line still names that portfolio."),
             periodNote,
             Text.unwords ("Transactions that were not confirmed are not listed." : [sentence | whole, asset <- assets, Just sentence <- [leftOutSentence (activityNetwork asset) (activityUnconfirmed asset)]]),
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
             "The reference is worked out from the statement period, the holder, "
               <> (if whole then "the opening balances, the transactions and the prices" else "the selection and the transactions listed")
               <> ". Two statements with the same reference state the same."
           ]
    -- The note on where the period starts and stops. Both ends count
    -- whole blocks, which the balances of a statement of every
    -- transaction rest on. A statement of some transactions is cut at
    -- the same blocks, and says so without a balance.
    periodNote =
      ( if whole
          then
            "The opening balance counts the transactions whose block is dated before the first day of the period, on the clock of the computer that prepared the statement. "
              <> case accountsEnd statement of
                AtEndOf _ _ -> "The closing balance counts those whose block is dated on the last day of the period or before."
                AtPreparation -> "The closing balance counts every transaction confirmed in the blockchain data of the last synchronisation."
          else
            "The period starts after the last block dated before its first day, on the clock of the computer that prepared the statement. "
              <> case accountsEnd statement of
                AtEndOf _ _ -> "It stops at the last block dated on its last day or before."
                AtPreparation -> "It stops at the last block of the last synchronisation."
      )
        <> " A block may be dated before the block that precedes it. Every block up to the last one dated in time is therefore counted, at each end of the period, "
        <> (if whole then "so that each balance is a state the blockchain went through. A transaction may for that reason" else "so a transaction may")
        <> " be listed with a date just outside the period. The section Blockchain data names the last block before the period and the last block counted."
