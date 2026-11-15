{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The balance statement of the command report: what every portfolio
-- of every wallet held on a given day, by network and by address, as a
-- report ("Haskoin.Wallet.Cli.Report").
--
-- The statement is for a declaration of assets: a wealth tax, an
-- inventory at the end of a year, a file for an accountant. Such a
-- declaration is for a day that is over, most often the last day of a
-- year, and is made weeks later. So the statement can be for any past
-- day, and not only for now.
--
-- = Where the balances come from
--
-- The balances hw shows on the screen are those the server gave at the
-- last sync. They are the balances of now, and say nothing of a day in
-- the past.
--
-- The history does. The summary of each transaction says what it paid
-- to each address of the portfolio, and what it spent from each
-- ("Haskoin.Wallet.Database.TxInfo"). The balance of an address on a day
-- is what the transactions up to that day paid to it, less what they
-- spent from it. The statement is worked out that way for every date,
-- today included, so that one rule gives every statement.
--
-- The balances are worked out from the database alone, in one reading,
-- as the spreadsheet and the journal are. They are as good as the last
-- sync, and the statement says which block that sync reached.
--
-- = Which transactions count
--
-- Only confirmed transactions count. An unconfirmed one may never
-- confirm, and a dead one never moved any money. A statement of now
-- says in a note what the unconfirmed ones would add or take away.
--
-- A statement for a day counts the blocks dated on that day or before,
-- on the clock of the computer. A tax year ends at midnight where its
-- owner lives, and the journal for ledger dates its entries the same
-- way ("Haskoin.Wallet.Cli.Journal").
--
-- The dates of the blocks do not always follow their order: a block may
-- be dated a little before the one under it
-- ('Haskoin.Wallet.Database.TxInfo.Standing'). Counting each transaction
-- by the date of its own block could then count a payment and leave out
-- the payment it spends from, which is dated a minute later. Such a
-- statement would show balances the portfolio never held, and an
-- address below zero. So the statement finds the last block dated on
-- the day or before, among those that hold a transaction of the wallet,
-- and counts every block up to it ('lastBlockCounted'). What it shows
-- is then a state the chain went through.
--
-- That block is the same for every portfolio on a network. A payment
-- from one portfolio to another is in one block, so it counts for both
-- or for neither.
--
-- = A sync that is too old
--
-- A statement for a day is refused when the last sync of a network
-- stopped on that day or before it ('refuseStale'). The wallet then does
-- not know what happened up to the end of the day, and a statement
-- would leave it out without a word. The fix is a sync.
--
-- A statement of now cannot be refused that way: every sync is in the
-- past. It names the block the sync reached, and its date, for the
-- reader to judge.
--
-- = Who it is of, and who made it
--
-- A statement names its holder when the user gives one. It is off by
-- default: a file with a name beside every address is a map of
-- someone's coins.
--
-- The statement looks like one a bank sends, and no bank made it. So it
-- says who did: the holder, from public data, with nobody to vouch for
-- it. It says that the wallets are self-custody wallets, which no
-- exchange holds. It says that it covers these wallets and nothing
-- else. A reader must not take it for the confirmation of a third
-- party, or its total for all that the holder owns.
--
-- = Values in a currency
--
-- A tax on wealth is charged on a value. hw knows no price, and would
-- have to trust one more server for it. A tax office names the price it
-- wants, and most publish it. So the user gives the price of each asset
-- on the command line, with the currency, and the statement says where
-- the prices came from when it is told. The type is t'Valuation'.
--
-- With prices, the summary and the tables of the portfolios state a
-- value beside each balance, and the summary adds them up. Every asset
-- the wallets hold must have a price: a total that left one out would
-- look complete. The tables of the addresses state no value. A tax
-- office asks for the total, and a value on each address would only add
-- lines that do not add up to the cent.
--
-- A value is the balance times the price, rounded to the hundredth
-- ("Haskoin.Wallet.Price"). A total is worked out from the total
-- balance, as a reader checks it, and not from the lines above it.
--
-- = Blocks named by their hash
--
-- The statement names two blocks on each network: the one the last sync
-- reached, and the last one it counts. A height alone names a block
-- only on one branch of the chain. The hash names it without doubt, and
-- lets a reader check the statement against the chain.
--
-- The database keeps the hash of the block the sync reached. It keeps
-- no hash of an older block, so the command asks the server for that of
-- the last block counted ('namingLastBlocks'). It is the one thing a
-- report asks of a server. When the server does not answer, the
-- statement says that the hash is not available, and is still made.
--
-- = The reference
--
-- A statement has a reference, eight digits worked out from what it
-- states: its date, its holder, the balances and the prices
-- ('statementReference'). Two statements with the same reference state
-- the same. When a tax office asks about a statement, both sides can
-- tell which one it is. The time it was made and the blocks of the last
-- sync are left out of it: they change, and the holdings do not.
--
-- = What the statement leaves out
--
-- It names no key and no fingerprint, as the spreadsheet does not: an
-- account key in a file would show every address of the account to
-- whoever reads it.
--
-- The amounts are in coins with eight decimals, whatever --unit says. A
-- statement that goes to a third party is read in the unit everyone
-- knows.
module Haskoin.Wallet.Cli.BalanceStatement
  ( StatementDate (..),
    PortfolioHistory (..),
    SyncedTo (..),
    BlockNamed (..),
    Valuation (..),
    Statement (..),
    AssetHoldings (..),
    PortfolioHoldings (..),
    AddressHolding (..),
    balanceStatement,
    statementTimes,
    statementReport,
    statementReference,
    valued,
    lastBlocksUnnamed,
    namingLastBlocks,
    lastBlockCounted,
    assetName,
    assetUnit,
    longDate,
  )
where

import Control.Monad (forM, forM_, unless)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub, sortOn, (\\))
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, LocalTime (..), TimeZone, UTCTime, ZonedTime (..), defaultTimeLocale, formatTime, utcToLocalTime)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Journal (satoshisText)
import Haskoin.Wallet.Cli.Report
import Haskoin.Wallet.Cli.Sheet (ZoneAt, offsetText)
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), WalletNaming (..), portfolioRefName, walletName, walletNamingOf)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (PoolPath (..), poolPathText)
import Haskoin.Wallet.Price (Price, priceText, valueAt, valueText)
import Haskoin.Wallet.Util (addrToTextE)
import Numeric.Natural (Natural)

{- What the statement is made from -}

-- | The date a statement is for: the end of a day that is over, on the
-- clock of the computer, or the moment the statement is made. The end of
-- a day comes with the zone the clock has at that midnight, so that the
-- statement can say which midnight it means.
data StatementDate = AtEndOf !Day !TimeZone | AtPreparation
  deriving (Eq, Show)

-- | A portfolio as the statement reads it: how the answers name it, the
-- description of its wallet and its own, and its transactions on each
-- network hw shows, as the history gives them
-- ('Haskoin.Wallet.Database.TxInfo.wholeHistory')
data PortfolioHistory = PortfolioHistory
  { historyRef :: !PortfolioRef,
    historyWalletDescription :: !Text,
    historyDescription :: !Text,
    historyPools :: ![(Network, [ListedTx])]
  }
  deriving (Eq, Show)

-- | The block the last sync of a network reached, with its hash, and
-- the time of that block when the database has it
data SyncedTo = SyncedTo
  { syncedToHeight :: !BlockHeight,
    syncedToHash :: !BlockHash,
    syncedToTime :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

-- | The prices a statement values the balances at: the currency, the
-- price of a coin of each network that has one, and where the prices
-- come from, as the user says it, or nothing
data Valuation = Valuation
  { valuationCurrency :: !Text,
    valuationPrices :: ![(Network, Price)],
    valuationSource :: !Text
  }
  deriving (Eq, Show)

-- | A block as a statement names it: its height, its hash when it is
-- known, and the time of the block on the clock of the computer when it
-- is known
data BlockNamed = BlockNamed
  { blockNamedHeight :: !BlockHeight,
    blockNamedHash :: !(Maybe BlockHash),
    blockNamedTime :: !(Maybe LocalTime)
  }
  deriving (Eq, Show)

{- The statement -}

-- | What the wallets held at a date. Each network is an asset, with what
-- every portfolio holds of it.
data Statement = Statement
  { statementDate :: !StatementDate,
    -- | Whether the statement names the wallets: only when it covers
    -- several, as in every view
    statementNaming :: !WalletNaming,
    -- | The wallets the statement covers, each with its description and
    -- how many portfolios it has
    statementWallets :: ![(Natural, Text, Int)],
    -- | Who holds the wallets, as the user says it, or nothing
    statementHolder :: !Text,
    -- | The prices the balances are valued at, when the user gave some
    statementValuation :: !(Maybe Valuation),
    -- | The networks hw shows, in their order
    statementAssets :: ![AssetHoldings]
  }
  deriving (Eq, Show)

-- | What the portfolios hold on a network at the date of the statement
data AssetHoldings = AssetHoldings
  { assetNetwork :: !Network,
    -- | The block the last sync reached. Nothing for a network that was
    -- never synced.
    assetSynced :: !(Maybe BlockNamed),
    -- | The last block the statement counts, the highest one that holds
    -- a transaction it counts. Nothing when it counts no transaction on
    -- the network. Its hash is known when it is the block the last sync
    -- reached, and is otherwise asked of the server
    -- ('namingLastBlocks').
    assetLastBlock :: !(Maybe BlockNamed),
    -- | The unconfirmed transactions a statement of now leaves out: how
    -- many, and what they add up to. A statement for a past day has
    -- none: they all came after it.
    assetUnconfirmed :: !(Int, Integer),
    -- | Every portfolio, with or without a balance
    assetPortfolios :: ![PortfolioHoldings]
  }
  deriving (Eq, Show)

-- | What a portfolio holds on a network: its addresses with a balance,
-- in the order addresses lists them
data PortfolioHoldings = PortfolioHoldings
  { holdingsRef :: !PortfolioRef,
    holdingsDescription :: !Text,
    holdingsAddresses :: ![AddressHolding]
  }
  deriving (Eq, Show)

-- | An address with a balance at the date of the statement
data AddressHolding = AddressHolding
  { holdingAddress :: !Text,
    holdingPath :: !PoolPath,
    holdingLabel :: !Text,
    holdingBalance :: !Natural
  }
  deriving (Eq, Show)

-- | What a portfolio holds on a network, over its addresses
holdingsTotal :: PortfolioHoldings -> Natural
holdingsTotal = sum . map holdingBalance . holdingsAddresses

-- | What every portfolio holds on a network
assetTotal :: AssetHoldings -> Natural
assetTotal = sum . map holdingsTotal . assetPortfolios

-- | How many addresses have a balance on a network
assetFunded :: AssetHoldings -> Int
assetFunded = sum . map (length . holdingsAddresses) . assetPortfolios

{- Working it out -}

-- | The statement of the portfolios at a date, on the networks given,
-- each with the block its last sync reached. The function gives the zone
-- of the clock of the computer at a time, which says the day a block is
-- dated on.
--
-- A statement for a day is refused when a network was last synced on
-- that day or before ('refuseStale'), and any statement is refused when
-- the history spends more from an address than it received, which a
-- sound history never does.
balanceStatement :: ZoneAt -> StatementDate -> [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> Either String Statement
balanceStatement zoneAt date networks histories = do
  mapM_ (uncurry (refuseStale zoneAt date)) networks
  assets <- forM networks $ \(net, synced) -> do
    let pools = [(history, transactions) | history <- histories, (poolNet, transactions) <- historyPools history, poolNet == net]
        lastBlock = lastBlockCounted zoneAt date (concatMap (map listedTxInfo . snd) pools)
    portfolios <- forM pools $ \(history, transactions) ->
      PortfolioHoldings (historyRef history) (historyDescription history)
        <$> addressHoldings net (historyRef history) (countedSummaries lastBlock transactions)
    return
      AssetHoldings
        { assetNetwork = net,
          assetSynced = (\to -> BlockNamed (syncedToHeight to) (Just (syncedToHash to)) (localTimeAt <$> syncedToTime to)) <$> synced,
          assetLastBlock = lastBlockNamed synced (concatMap (map listedTxInfo . snd) pools) <$> lastBlock,
          assetUnconfirmed = case date of
            AtPreparation -> unconfirmedLeftOut (concatMap snd pools)
            AtEndOf _ _ -> (0, 0),
          assetPortfolios = portfolios
        }
  return
    Statement
      { statementDate = date,
        statementNaming = walletNamingOf (length wallets),
        statementWallets = wallets,
        statementHolder = "",
        statementValuation = Nothing,
        statementAssets = assets
      }
  where
    localTimeAt time = utcToLocalTime (zoneAt time) time
    -- The last block counted, with the time of its transactions. Its hash
    -- is known when it is the block the last sync reached.
    lastBlockNamed synced transactions height =
      BlockNamed
        height
        (listToMaybe [syncedToHash to | Just to <- [synced], syncedToHeight to == height])
        (listToMaybe [localTimeAt time | (confirmedAt, Just time) <- mapMaybe confirmedIn transactions, confirmedAt == height])
    -- The wallets of the portfolios, each once, in the order of their
    -- indexes
    wallets =
      [ (index, description, length [() | history <- histories, refWallet (historyRef history) == index])
        | (index, description) <- Map.toAscList (Map.fromList [(refWallet (historyRef history), historyWalletDescription history) | history <- histories])
      ]

-- | Refuse a statement for a day when the last sync of a network reached
-- a block dated on that day or before
refuseStale :: ZoneAt -> StatementDate -> Network -> Maybe SyncedTo -> Either String ()
refuseStale zoneAt date net synced =
  case (date, synced >>= syncedToTime) of
    (AtEndOf day _, Just time) ->
      unless (dayAt zoneAt time > day) . Left $
        "The last sync of "
          <> net.name
          <> " reached a block dated "
          <> cs (longDate (dayAt zoneAt time))
          <> ", and the statement is for the end of "
          <> cs (longDate day)
          <> ": it could miss transactions. Run hw sync first"
    _ -> Right ()

-- | The day a time falls on, on the clock of the computer
dayAt :: ZoneAt -> UTCTime -> Day
dayAt zoneAt time = localDay (utcToLocalTime (zoneAt time) time)

-- | The block of a live confirmed transaction: its height, and the time
-- of the block. Nothing for an unconfirmed transaction, for a dead one
-- and for one in preparation.
confirmedIn :: TxInfo -> Maybe (BlockHeight, Maybe UTCTime)
confirmedIn info = case txInfoStatus info of
  OnChain status
    | not (chainDead status),
      Store.BlockRef height _ <- chainBlock status ->
        Just (height, chainTime status)
  _ -> Nothing

-- | The last block a statement counts on a network, given the
-- transactions of every portfolio there: the highest block that is
-- dated on the day of the statement or before, among those of the
-- confirmed transactions. A statement of now counts every confirmed
-- transaction. Nothing when no transaction counts.
--
-- Every block up to that one counts, even one dated after the day: the
-- dates of the blocks do not always follow their order, and a statement
-- that left such a block out would show a state the chain never had.
lastBlockCounted :: ZoneAt -> StatementDate -> [TxInfo] -> Maybe BlockHeight
lastBlockCounted zoneAt date transactions =
  case [height | (height, timeM) <- mapMaybe confirmedIn transactions, datedInTime timeM] of
    [] -> Nothing
    heights -> Just (maximum heights)
  where
    datedInTime timeM = case date of
      AtPreparation -> True
      -- A block without a time cannot be said to be dated in time. It
      -- still counts when a later block is.
      AtEndOf day _ -> maybe False (\time -> dayAt zoneAt time <= day) timeM

-- | The summaries of the transactions of a pool that a statement counts:
-- the live confirmed ones up to the last block counted
countedSummaries :: Maybe BlockHeight -> [ListedTx] -> [TxSummary]
countedSummaries lastBlock transactions =
  [ txInfoSummary info
    | info <- listedTxInfo <$> transactions,
      Just (height, _) <- [confirmedIn info],
      Just height <= lastBlock
  ]

-- | The unconfirmed transactions of a network: how many, each counted
-- once even when two portfolios see it, and what they add up to over
-- the portfolios. A payment from one portfolio to another adds up to
-- its fee.
unconfirmedLeftOut :: [ListedTx] -> (Int, Integer)
unconfirmedLeftOut transactions = (length (nubOrd (fst <$> unconfirmed)), sum (snd <$> unconfirmed))
  where
    unconfirmed =
      [ (chainTxid status, summaryAmount (txInfoSummary info))
        | info <- listedTxInfo <$> transactions,
          OnChain status <- [txInfoStatus info],
          not (chainDead status),
          Store.MemRef _ <- [chainBlock status]
      ]

-- | The addresses of a portfolio with a balance, given the summaries of
-- the transactions that count: for each address, what they paid to it
-- less what they spent from it. The addresses come in the order
-- addresses lists them: the BIP44 ones first, the receiving ones before
-- the change ones, by index.
addressHoldings :: Network -> PortfolioRef -> [TxSummary] -> Either String [AddressHolding]
addressHoldings net ref summaries = do
  holdings <- forM (Map.toList balances) $ \(addr, balance) -> do
    text <- addrToTextE net addr
    unless (balance >= 0) . Left $
      "The transactions of "
        <> cs (portfolioRefName WithWallets ref)
        <> " on "
        <> net.name
        <> " spend more from "
        <> cs text
        <> " than it received: hw sync --full reads them again"
    return [AddressHolding text path label (fromInteger balance) | balance > 0, Just (path, label) <- [Map.lookup addr places]]
  return $ sortOn pathOrder (concat holdings)
  where
    paid = Map.unionsWith (+) [toInteger . myOutputsValue <$> summaryMyOutputs summary | summary <- summaries]
    spent = Map.unionsWith (+) [toInteger . myInputsValue <$> summaryMyInputs summary | summary <- summaries]
    balances = Map.unionWith (+) paid (negate <$> spent)
    -- Where each address is, and its label. An address with a balance
    -- was paid, so the outputs name it.
    places = Map.unions [(\outputs -> (myOutputsPath outputs, myOutputsLabel outputs)) <$> summaryMyOutputs summary | summary <- summaries]
    pathOrder holding = (pathKind (holdingPath holding), pathToList (pathInAccount (holdingPath holding)))

{- Prices and block hashes -}

-- | The statement with its balances valued at the prices given. It is
-- refused when a price names a network the statement does not cover,
-- when a network has two prices, and when an asset the wallets hold has
-- no price: the total value would leave that asset out and still look
-- complete.
valued :: Valuation -> Statement -> Either String Statement
valued valuation statement = do
  forM_ priced $ \net ->
    unless (net `elem` covered) . Left $
      "The statement does not cover " <> net.name <> ", which --price names: it covers " <> listed covered
  forM_ (priced \\ nub priced) $ \net ->
    Left ("--price names " <> net.name <> " twice: give one price for each network")
  forM_ [assetNetwork asset | asset <- statementAssets statement, assetTotal asset > 0] $ \net ->
    unless (net `elem` priced) . Left $
      "The wallets hold coins on " <> net.name <> ", which has no price: give one with --price " <> net.name <> "=PRICE, or the total value would leave them out"
  return statement {statementValuation = Just valuation}
  where
    priced = fst <$> valuationPrices valuation
    covered = assetNetwork <$> statementAssets statement
    listed :: [Network] -> String
    listed networks = cs (Text.intercalate ", " [cs net.name | net <- networks])

-- | The last blocks a statement counts whose hash it does not know yet,
-- each with its network: what the command asks the server
lastBlocksUnnamed :: Statement -> [(Network, BlockHeight)]
lastBlocksUnnamed statement =
  [ (assetNetwork asset, blockNamedHeight block)
    | asset <- statementAssets statement,
      Just block <- [assetLastBlock asset],
      blockNamedHash block == Nothing
  ]

-- | The statement with the hashes given for the last blocks it counts,
-- each for a network and a height. A block whose hash the statement
-- already knows keeps it, and one no hash is given for stays without.
namingLastBlocks :: [(Network, BlockHeight, BlockHash)] -> Statement -> Statement
namingLastBlocks hashes statement = statement {statementAssets = named <$> statementAssets statement}
  where
    named asset = asset {assetLastBlock = withHash (assetNetwork asset) <$> assetLastBlock asset}
    withHash net block = case blockNamedHash block of
      Just _ -> block
      Nothing -> block {blockNamedHash = listToMaybe [hash | (hashNet, height, hash) <- hashes, hashNet == net, height == blockNamedHeight block]}

-- | Every time the statement reads a day or a time of day from, in UTC:
-- those of the confirmed transactions, and those of the blocks the last
-- syncs reached. The caller asks the computer for the zone at each
-- ('ZoneAt').
statementTimes :: [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> [UTCTime]
statementTimes networks histories =
  nubOrd $
    [time | (_, Just synced) <- networks, Just time <- [syncedToTime synced]]
      <> [ time
           | history <- histories,
             (_, transactions) <- historyPools history,
             Just (_, Just time) <- confirmedIn . listedTxInfo <$> transactions
         ]

{- Naming things -}

-- | The name of the asset of a network, as a reader outside of hw knows
-- it. A test network has no name of its own, and is named after the
-- word hw has for it.
assetName :: Network -> Text
assetName net = case net.name of
  "btc" -> "Bitcoin"
  "bch" -> "Bitcoin Cash"
  name -> "Test network " <> cs name

-- | The unit of the amounts of a network: its name in capitals, as in
-- BTC and BCH. The journal for ledger names its commodities the same
-- way.
assetUnit :: Network -> Text
assetUnit net = Text.toUpper (cs net.name)

-- | An asset with its unit, as in Bitcoin (BTC)
assetTitle :: Network -> Text
assetTitle net = assetName net <> " (" <> assetUnit net <> ")"

-- | A day written in full, as in 31 December 2025. The month is a word,
-- so that no reader takes the day for the month.
longDate :: Day -> Text
longDate = Text.pack . formatTime defaultTimeLocale "%-d %B %Y"

-- | A time on a clock as a day and a time of day, as in 15 November
-- 2026, 14:32
longTime :: LocalTime -> Text
longTime time = longDate (localDay time) <> ", " <> Text.pack (formatTime defaultTimeLocale "%H:%M" (localTimeOfDay time))

-- | A count with its noun, in the singular for one: 1 portfolio, 2
-- portfolios
counted :: Int -> Text -> Text
counted count noun = Text.pack (show count) <> " " <> noun <> (if count == 1 then "" else "s")

{- The reference -}

-- | The reference of a statement made at the time given: eight
-- hexadecimal digits in two groups, as in 3F9A-52C1, worked out from
-- what the statement states. It is the start of the SHA-256 hash of its
-- date, its holder, the balance of every address, and the prices.
--
-- Two statements with the same reference state the same holdings at the
-- same date. The time a statement was made is left out, and so are the
-- blocks of the last sync: a statement for the end of a year made twice,
-- a week apart, has one reference as long as nothing in it changed. A
-- statement of now is dated to the minute it was made, so each has its
-- own.
statementReference :: ZonedTime -> Statement -> Text
statementReference prepared statement =
  Text.toUpper (Text.take 4 digest <> "-" <> Text.take 4 (Text.drop 4 digest))
  where
    digest = Text.pack (show (hashWith SHA256 (encodeUtf8 (Text.unlines stated))))
    -- What the statement states, a line for each thing, in a fixed
    -- order
    stated =
      [ case statementDate statement of
          AtEndOf day _ -> "end of " <> Text.pack (show day)
          AtPreparation -> "at " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (zonedTimeToLocalTime prepared)),
        "holder " <> statementHolder statement
      ]
        <> [ Text.unwords [cs (assetNetwork asset).name, shown (refWallet ref), shown (refPortfolio ref), holdingAddress holding, shown (holdingBalance holding)]
             | asset <- statementAssets statement,
               holdings <- assetPortfolios asset,
               let ref = holdingsRef holdings,
               holding <- holdingsAddresses holdings
           ]
        <> concat
          [ ("currency " <> valuationCurrency valuation)
              : [Text.unwords ["price", cs net.name, priceText price] | (net, price) <- valuationPrices valuation]
            | Just valuation <- [statementValuation statement]
          ]
    shown :: (Show a) => a -> Text
    shown = Text.pack . show

{- The report -}

-- | The statement as a report, made at the time given: the facts about
-- it, a summary by asset, what each portfolio holds, what each address
-- holds, the blocks it names, and the notes that say who made it and
-- how it was worked out.
--
-- A network without any balance is in the summary, with a balance of
-- zero: that the wallets hold none of an asset is part of what the
-- statement declares. It has no table of its own further down.
--
-- With prices, the summary and the tables of the portfolios have a
-- column of values, and the summary a total. The tables of the
-- addresses have none.
statementReport :: ZonedTime -> Statement -> Report
statementReport prepared statement =
  Report
    { reportKind = "Statement of holdings",
      reportTitle = "Balance Statement",
      reportSubtitle = asOf,
      reportFooter = "Balance Statement \xB7 " <> asOf <> " \xB7 Ref. " <> reference,
      reportBlocks =
        [FactsBlock facts, SectionBlock "Summary", TableBlock summary]
          <> (if null held then [] else SectionBlock "Holdings by portfolio" : concatMap byPortfolio held)
          <> (if null held then [] else SectionBlock "Holdings by address" : concatMap byAddress held)
          <> (SectionBlock "Blockchain data" : concatMap blocksOf assets)
          <> [SectionBlock "Notes", NotesBlock notes]
    }
  where
    assets = statementAssets statement
    naming = statementNaming statement
    valuation = statementValuation statement
    preparedTime = zonedTimeToLocalTime prepared
    reference = statementReference prepared statement
    -- The assets the wallets hold some of
    held = [asset | asset <- assets, assetTotal asset > 0]
    asOf = "As of " <> longDate statementDay
    statementDay = case statementDate statement of
      AtEndOf day _ -> day
      AtPreparation -> localDay preparedTime
    amount = satoshisText . toInteger
    count :: (Show number) => number -> Text
    count = Text.pack . show
    inZone zone = " (" <> offsetText zone <> ")"

    -- The price of an asset, when the statement has prices and one for
    -- it
    priceOf net = listToMaybe [price | Just prices <- [valuation], (priced, price) <- valuationPrices prices, priced == net]
    -- The value of a balance of an asset, as a cell: empty without a
    -- price
    valueCell net balance = plainCell (maybe "" (\price -> valueText (valueAt price balance)) (priceOf net))
    -- A column and its cells that are there only when the statement has
    -- prices
    whenValued :: [a] -> [a]
    whenValued = if null valuation then const [] else id
    currency = maybe "" valuationCurrency valuation

    facts =
      [Fact "Holder" [statementHolder statement] | not (Text.null (statementHolder statement))]
        <> [ Fact "Statement date" $ case statementDate statement of
               -- Midnight at the end of a day is written 24:00, so that
               -- the date beside it is the day the statement is for
               AtEndOf day zone -> [longDate day <> ", 24:00" <> inZone zone]
               AtPreparation -> [longTime preparedTime <> inZone (zonedTimeZone prepared)],
             Fact "Prepared on" [longTime preparedTime <> inZone (zonedTimeZone prepared)],
             Fact "Prepared by" ["The holder, from public blockchain data. Not audited."],
             Fact "Reference" [reference],
             Fact "Custody" ["Self-custody: the holder keeps the keys. No exchange or online wallet."],
             Fact "Wallets" walletLines,
             Fact "Assets" [Text.intercalate ", " (assetTitle . assetNetwork <$> assets)]
           ]
        <> [ Fact "Valuation" (("In " <> valuationCurrency prices <> ", at the prices the holder gave") : ["Source of the prices: " <> valuationSource prices | not (Text.null (valuationSource prices))])
             | Just prices <- [valuation]
           ]
        <> [Fact "Blockchain data" (syncedLine <$> assets)]
    walletLines = case (naming, statementWallets statement) of
      (WithoutWallets, [(_, description, portfolios)]) ->
        ("1 wallet, " <> counted portfolios "portfolio") : [description | not (Text.null description)]
      (_, wallets) ->
        [ walletName index <> ", " <> counted portfolios "portfolio" <> (if Text.null description then "" else ": " <> description)
          | (index, description, portfolios) <- wallets
        ]
    syncedLine asset = assetName (assetNetwork asset) <> ": " <> case assetSynced asset of
      Nothing -> "not synchronised"
      Just block -> "synchronised to block " <> count (blockNamedHeight block) <> maybe "" (\time -> ", dated " <> longTime time) (blockNamedTime block)

    summary =
      Table
        ( if null valuation
            then [Column "Asset" 4 AlignLeft, Column "Unit" 2 AlignLeft, Column "Funded addresses" 3 AlignRight, Column "Balance" 4 AlignRight]
            else
              -- Six columns share the width: the shares are their widths
              -- in points
              [ Column "Asset" 80 AlignLeft,
                Column "Unit" 60 AlignLeft,
                Column "Funded addresses" 88 AlignRight,
                Column "Balance" 86 AlignRight,
                Column ("Price (" <> currency <> ")") 78 AlignRight,
                Column ("Value (" <> currency <> ")") 90 AlignRight
              ]
        )
        ( [ Row BodyRow $
              [plainCell (assetName net), plainCell (assetUnit net), plainCell (count (assetFunded asset)), plainCell (amount (assetTotal asset))]
                <> whenValued [plainCell (maybe "" priceText (priceOf net)), valueCell net (assetTotal asset)]
            | asset <- assets,
              let net = assetNetwork asset
          ]
            <> whenValued [Row TotalRow [plainCell "Total value", plainCell "", plainCell "", plainCell "", plainCell "", plainCell (valueText totalValue)]]
        )
    -- The values of the assets added up, each as the summary states it
    totalValue = sum [valueAt price (assetTotal asset) | asset <- assets, Just price <- [priceOf (assetNetwork asset)]]

    -- What each portfolio holds of an asset, and the total
    byPortfolio asset =
      [ GroupBlock (assetTitle net) "",
        TableBlock $
          Table
            ( [Column "Portfolio" 4 AlignLeft, Column "Description" (if null valuation then 5 else 4) AlignLeft, Column "Funded addresses" 3 AlignRight, Column ("Balance (" <> assetUnit net <> ")") 3.4 AlignRight]
                <> whenValued [Column ("Value (" <> currency <> ")") 3.4 AlignRight]
            )
            ( [ Row BodyRow $
                  [plainCell (portfolioRefName naming (holdingsRef holdings)), plainCell (holdingsDescription holdings), plainCell (count (length (holdingsAddresses holdings))), plainCell (amount (holdingsTotal holdings))]
                    <> whenValued [valueCell net (holdingsTotal holdings)]
                | holdings <- assetPortfolios asset
              ]
                <> [ Row TotalRow $
                       [plainCell ("Total " <> assetName net), plainCell "", plainCell (count (assetFunded asset)), plainCell (amount (assetTotal asset))]
                         <> whenValued [valueCell net (assetTotal asset)]
                   ]
            )
      ]
      where
        net = assetNetwork asset

    -- What each address of each portfolio holds of an asset, for the
    -- portfolios that hold some
    byAddress asset =
      concat
        [ [ GroupBlock (portfolioRefName naming (holdingsRef holdings) <> " \xB7 " <> assetTitle net) (holdingsDescription holdings),
            TableBlock $
              Table
                [Column "Address" 9 AlignLeft, Column "Path" 3 AlignLeft, Column ("Balance (" <> assetUnit net <> ")") 4 AlignRight]
                ( [ Row BodyRow [Cell (holdingAddress holding) (holdingLabel holding), plainCell (poolPathText (holdingPath holding)), plainCell (amount (holdingBalance holding))]
                    | holding <- holdingsAddresses holdings
                  ]
                    <> [Row TotalRow [plainCell ("Total " <> portfolioRefName naming (holdingsRef holdings)), plainCell "", plainCell (amount (holdingsTotal holdings))]]
                )
          ]
          | holdings <- assetPortfolios asset,
            not (null (holdingsAddresses holdings))
        ]
      where
        net = assetNetwork asset

    -- The two blocks the statement names on a network, each with its
    -- height, its date and its hash
    blocksOf asset =
      [ GroupBlock (assetTitle (assetNetwork asset)) "",
        FactsBlock
          [ Fact "Last block of the sync" (maybe ["Not synchronised"] blockLines (assetSynced asset)),
            Fact "Last block counted" (maybe ["None: the statement counts no transaction"] blockLines (assetLastBlock asset))
          ]
      ]
    blockLines block =
      [ "Block " <> count (blockNamedHeight block) <> maybe "" (\time -> ", dated " <> longTime time) (blockNamedTime block),
        maybe "Hash not available: the server did not give it" (\hash -> "Hash " <> blockHashToHex hash) (blockNamedHash block)
      ]

    notes =
      [ "This statement was prepared by the holder of the wallets, from the data of public blockchains. No third party has audited or confirmed it.",
        "The wallets are self-custody wallets: the holder keeps the keys that control the assets. They are not accounts at an exchange, at a bank or with an online wallet service, and no third party holds the assets.",
        "The statement covers only the wallets it was prepared from. Assets the holder keeps elsewhere, such as at an exchange or in another wallet, are not part of it.",
        "The statement lists the balances held at the statement date by the addresses of the wallets, on each blockchain. A balance is what the confirmed transactions paid to an address, less what they spent from it.",
        case statementDate statement of
          AtEndOf _ _ ->
            "A transaction is counted when its block is dated on the statement date or before, on the clock of the computer that prepared the statement. A block may be dated before the block that precedes it. Every block up to the last one dated in time is therefore counted, so that the balances are a state the blockchain went through. The section Blockchain data names the last block counted."
          AtPreparation ->
            "Every transaction confirmed in the blockchain data of the last synchronisation is counted. The section Blockchain data names the last block counted.",
        Text.unwords ("Transactions that were not confirmed are not counted." : mapMaybe leftOut assets),
        "Amounts are in units of each asset, to eight decimal places. One unit is 100,000,000 satoshis."
          <> (if null valuation then " The statement gives no value in a currency." else ""),
        "A funded address is an address with a balance at the statement date. Addresses without a balance are not listed."
      ]
        <> [ "The path of an address is its place in its portfolio: the type of its account, bip44 for legacy addresses and bip84 for native SegWit addresses, then 0 for a receiving address or 1 for a change address, then its index."
             | not (null held)
           ]
        <> [ "Values are in "
               <> valuationCurrency prices
               <> ". A value is a balance times the price of its asset, rounded to the nearest hundredth. The holder gave the prices."
               <> (if Text.null (valuationSource prices) then "" else " Their source: " <> valuationSource prices <> ".")
               <> " The total of an asset is worked out from its total balance, so it may differ by a hundredth from the sum of the lines above it."
             | Just prices <- [valuation]
           ]
        <> [ "A block is named by its height and by its hash, which identifies it on the blockchain.",
             "The reference is worked out from the statement date, the holder, the balances and the prices. Two statements with the same reference state the same."
           ]
    leftOut asset = case assetUnconfirmed asset of
      (0, _) -> Nothing
      (unconfirmed, net) ->
        Just $
          "On "
            <> assetName (assetNetwork asset)
            <> ", "
            <> counted unconfirmed "unconfirmed transaction"
            <> (if unconfirmed == 1 then " was" else " were")
            <> " left out, which would change the balance by "
            <> (if net >= 0 then "+" else "")
            <> satoshisText net
            <> " "
            <> assetUnit (assetNetwork asset)
            <> "."
