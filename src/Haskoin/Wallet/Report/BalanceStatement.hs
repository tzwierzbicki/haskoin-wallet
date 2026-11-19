{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The balance statement of the command report: what every portfolio
-- of every wallet held on a given day, by network and by address, as a
-- report ("Haskoin.Wallet.Report.Layout").
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
-- The dates of the blocks do not always follow their order: a block may be
-- dated a little before the one under it (the type
-- t'Haskoin.Wallet.Database.TxInfo.Standing'). Counting each transaction by
-- the date of its own block could then count a payment and leave out the
-- payment it spends from, which is dated a minute later. Such a statement
-- would show balances the portfolio never held, and an address below zero.
-- So the statement finds the last block dated on the day or before, among
-- those that hold a transaction of the wallet, and counts every block up to
-- it ('lastBlockCounted'). What it shows is then a state the chain went
-- through.
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
-- ("Haskoin.Wallet.Report.Price"). A total is worked out from the total
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
--
-- = A statement of some addresses
--
-- The holder may keep the statement to the addresses of some tags
-- ("Haskoin.Wallet.AddressSelection"): what a group of coins held, for
-- the reader that group concerns. The statement is first worked out
-- whole, and then loses the addresses the selection leaves out
-- ('holdingsSelected'). So an address has the same balance in both, and
-- both count the same blocks.
--
-- Such a statement is still a balance statement: each balance it states
-- is the whole balance of its address, and its totals add up. What a
-- total no longer is, is what a portfolio or a wallet holds. So the
-- statement says which addresses it lists, under its title, in its facts
-- and in its first note. A total is named \"Total\" alone, and a portfolio
-- that holds nothing at the addresses kept is not listed: a line that
-- says it holds none would be read as its balance. Prices value the
-- balances listed, as they value those of any statement.
--
-- The tags are those of the time the statement is made, for a past day
-- too: hw keeps no history of them.
--
-- = What it shares with the account statement
--
-- The rule that says which transactions count, the prices, the
-- reference, and what a statement says of itself are those of every
-- statement. They are in "Haskoin.Wallet.Report.Statement", so that the
-- account statement ("Haskoin.Wallet.Report.AccountStatement") closes
-- on the balance this one states for the same day.
module Haskoin.Wallet.Report.BalanceStatement
  ( Statement (..),
    AssetHoldings (..),
    PortfolioHoldings (..),
    AddressHolding (..),
    balanceStatement,
    holdingsSelected,
    statementReport,
    statementReference,
    valued,
    lastBlocksUnnamed,
    namingLastBlocks,
  )
where

import Control.Monad (forM, unless)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (LocalTime (..), ZonedTime (..))
import Haskoin
import Haskoin.Wallet.AddressSelection (AddressSelection (..), keepsAddress)
import Haskoin.Wallet.Cli.Journal (satoshisText)
import Haskoin.Wallet.Cli.Sheet (ZoneAt)
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), WalletNaming (..), portfolioRefName)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (PoolPath (..), poolPathText)
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.Price (priceText, valueAt, valueText)
import Haskoin.Wallet.Report.Statement
import Haskoin.Wallet.Util (addrToTextE)
import Numeric.Natural (Natural)

{- The statement -}

-- | What the wallets held at a date. Each network is an asset, with what
-- every portfolio holds of it.
data Statement = Statement
  { statementDate :: !StatementDate,
    -- | What the statement covers: every wallet, one wallet or one
    -- portfolio
    statementScope :: !StatementScope,
    -- | Whether the statement names the wallets: only when the database
    -- has several, as in every view
    statementNaming :: !WalletNaming,
    -- | The wallets the statement covers, each with its description and
    -- how many portfolios it has
    statementWallets :: ![(Natural, Text, Int)],
    -- | Who holds the wallets, as the user says it, or nothing
    statementHolder :: !Text,
    -- | The prices the balances are valued at, when the user gave some
    statementValuation :: !(Maybe Valuation),
    -- | Which addresses the statement lists, when it lists some of them
    -- only, in the words of the selection ('addressesStated'). Nothing
    -- for the statement of every address.
    statementSelection :: !(Maybe Text),
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

-- | The statement of the portfolios a scope covers at a date, on the
-- networks given, each with the block its last sync reached. The histories
-- are those of every portfolio of the database, covered or not: the last
-- block counted is found among them all, so that a portfolio has the same
-- balance in every statement (the type t'StatementScope'). The function
-- gives the zone of the clock of the computer at a time, which says the day
-- a block is dated on.
--
-- A statement for a day is refused when a network was last synced on
-- that day or before ('refuseStale'), and any statement is refused when
-- the history spends more from an address than it received, which a
-- sound history never does.
balanceStatement :: StatementScope -> ZoneAt -> StatementDate -> [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> Either String Statement
balanceStatement asked zoneAt date networks histories = do
  mapM_ (uncurry (refuseStale zoneAt date)) networks
  assets <- forM networks $ \(net, synced) -> do
    let onNetwork shown = [(history, transactions) | history <- shown, (poolNet, transactions) <- historyPools history, poolNet == net]
        everyTransaction = concatMap (map listedTxInfo . snd) (onNetwork histories)
        lastBlock = lastBlockCounted zoneAt date everyTransaction
        pools = onNetwork covered
    portfolios <- forM pools $ \(history, transactions) ->
      PortfolioHoldings (historyRef history) (historyDescription history)
        <$> addressHoldings net (historyRef history) (countedSummaries lastBlock transactions)
    return
      AssetHoldings
        { assetNetwork = net,
          assetSynced = syncedBlock zoneAt <$> synced,
          assetLastBlock = countedBlock zoneAt synced everyTransaction <$> lastBlock,
          assetUnconfirmed = case date of
            AtPreparation -> unconfirmedLeftOut (concatMap snd pools)
            AtEndOf _ _ -> (0, 0),
          assetPortfolios = portfolios
        }
  return
    Statement
      { statementDate = date,
        statementScope = scope,
        statementNaming = databaseNaming histories,
        statementWallets = walletsOf covered,
        statementHolder = "",
        statementValuation = Nothing,
        statementSelection = Nothing,
        statementAssets = assets
      }
  where
    scope = scopeAmong histories asked
    -- The portfolios the statement shows
    covered = [history | history <- histories, covers scope (historyRef history)]

-- | The summaries of the transactions of a pool that a statement counts:
-- the live confirmed ones up to the last block counted
countedSummaries :: Maybe BlockHeight -> [ListedTx] -> [TxSummary]
countedSummaries lastBlock transactions =
  [ txInfoSummary info
    | info <- listedTxInfo <$> transactions,
      Just (height, _) <- [confirmedIn info],
      Just height <= lastBlock
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

{- Some addresses only -}

-- | The statement of the addresses a selection keeps, given the
-- statement of every address: each portfolio keeps the addresses the
-- selection keeps for it, and the statement says which addresses it
-- lists. An address is left as the whole statement worked it out, with
-- its whole balance.
--
-- The unconfirmed transactions a statement of now counted as left out
-- are those of the whole portfolios, and say nothing of the addresses
-- kept. The statement of some addresses gives no such count.
--
-- Value the statement after this step, not before: the assets that need
-- a price are those the addresses kept hold ('valued').
holdingsSelected :: AddressSelection -> Statement -> Statement
holdingsSelected selection statement =
  statement
    { statementSelection = Just (addressesStated selection),
      statementAssets = [asset {assetUnconfirmed = (0, 0), assetPortfolios = kept (assetNetwork asset) <$> assetPortfolios asset} | asset <- statementAssets statement]
    }
  where
    kept net holdings =
      holdings {holdingsAddresses = filter (keepsAddress selection (holdingsRef holdings) net . holdingAddress) (holdingsAddresses holdings)}

{- Prices and block hashes -}

-- | The statement with its balances valued at the prices given. It is
-- refused when a price names a network the statement does not cover,
-- when a network has two prices, and when an asset the wallets hold has
-- no price ('checkPrices').
valued :: Valuation -> Statement -> Either String Statement
valued valuation statement = do
  checkPrices
    valuation
    (assetNetwork <$> statementAssets statement)
    [assetNetwork asset | asset <- statementAssets statement, assetTotal asset > 0]
  return statement {statementValuation = Just valuation}

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
    named asset = asset {assetLastBlock = withBlockHash hashes (assetNetwork asset) <$> assetLastBlock asset}

{- The reference -}

-- | The reference of a statement made at the time given: eight
-- hexadecimal digits in two groups, as in 3F9A-52C1, worked out from
-- what the statement states ('referenceOf'): its date, its holder, its
-- scope when it is narrowed down, which addresses it lists when it
-- lists some only, the balance of every address, and the prices.
--
-- Two statements with the same reference state the same holdings at the
-- same date. The time a statement was made is left out, and so are the
-- blocks of the last sync: a statement for the end of a year made twice,
-- a week apart, has one reference as long as nothing in it changed. A
-- statement of now is dated to the minute it was made, so each has its
-- own.
statementReference :: ZonedTime -> Statement -> Text
statementReference prepared statement =
  referenceOf $
    [dateStated prepared (statementDate statement), "holder " <> statementHolder statement]
      <> scopeStated (statementScope statement)
      <> ["selection " <> stated | Just stated <- [statementSelection statement]]
      <> [ Text.unwords [cs (assetNetwork asset).name, shown (refWallet ref), shown (refPortfolio ref), holdingAddress holding, shown (holdingBalance holding)]
           | asset <- statementAssets statement,
             holdings <- assetPortfolios asset,
             let ref = holdingsRef holdings,
             holding <- holdingsAddresses holdings
         ]
      <> valuationStated (statementValuation statement)
  where
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
--
-- A statement of some addresses says so under its title, in its facts
-- and in its first note. It lists the portfolios that hold some at these
-- addresses, and names its totals \"Total\" alone: they are those of the
-- addresses listed, not of a portfolio or of an asset.
statementReport :: ZonedTime -> Statement -> Report
statementReport prepared statement =
  Report
    { reportKind = statementKind (if whole then "holdings" else "holdings, selected by tag"),
      reportTitle = title,
      reportSubtitle = asOf,
      reportFooter = title <> " \xB7 " <> asOf <> " \xB7 Ref. " <> reference,
      reportBlocks =
        [FactsBlock facts, SectionBlock "Summary", TableBlock summary]
          <> (if null held then [] else SectionBlock "Holdings by portfolio" : concatMap byPortfolio held)
          <> (if null held then [] else SectionBlock "Holdings by address" : concatMap byAddress held)
          <> (SectionBlock "Blockchain data" : concatMap blocksOf assets)
          <> [SectionBlock "Notes", NotesBlock notes]
    }
  where
    title = statementTitle "Balance Statement"
    assets = statementAssets statement
    naming = statementNaming statement
    valuation = statementValuation statement
    preparedTime = zonedTimeToLocalTime prepared
    reference = statementReference prepared statement
    selection = statementSelection statement
    whole = isNothing selection
    -- The name of a total: of what it adds up in the statement of every
    -- address, and bare in a statement of some of them
    totalOf name = if whole then "Total " <> name else "Total"
    -- The assets the wallets hold some of
    held = [asset | asset <- assets, assetTotal asset > 0]
    asOf = "As of " <> longDate statementDay
    statementDay = case statementDate statement of
      AtEndOf day _ -> day
      AtPreparation -> localDay preparedTime
    amount = satoshisText . toInteger
    count :: (Show number) => number -> Text
    count = Text.pack . show

    -- The value of a balance of an asset, as a cell: empty without a
    -- price
    valueCell net balance = plainCell (maybe "" (\price -> valueText (valueAt price balance)) (priceOf valuation net))
    -- A column and its cells that are there only when the statement has
    -- prices
    whenValued :: [a] -> [a]
    whenValued = if null valuation then const [] else id
    currency = maybe "" valuationCurrency valuation

    facts =
      holderFacts (statementHolder statement)
        <> [ Fact "Statement date" $ case statementDate statement of
               -- Midnight at the end of a day is written 24:00, so that
               -- the date beside it is the day the statement is for
               AtEndOf day zone -> [longDate day <> ", 24:00" <> inZone zone]
               AtPreparation -> [longTime preparedTime <> inZone (zonedTimeZone prepared)]
           ]
        <> [Fact "Addresses" ["Only those " <> stated] | Just stated <- [selection]]
        <> preparationFacts prepared reference
        <> coverageFacts (statementScope statement) naming (statementWallets statement) (assetNetwork <$> assets)
        <> valuationFacts valuation
        <> syncFacts [(assetNetwork asset, assetSynced asset) | asset <- assets]

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
                <> whenValued [plainCell (maybe "" priceText (priceOf valuation net)), valueCell net (assetTotal asset)]
            | asset <- assets,
              let net = assetNetwork asset
          ]
            <> whenValued [Row TotalRow [plainCell "Total value", plainCell "", plainCell "", plainCell "", plainCell "", plainCell (valueText totalValue)]]
        )
    -- The values of the assets added up, each as the summary states it
    totalValue = sum [valueAt price (assetTotal asset) | asset <- assets, Just price <- [priceOf valuation (assetNetwork asset)]]

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
                | holdings <- assetPortfolios asset,
                  whole || not (null (holdingsAddresses holdings))
              ]
                <> [ Row TotalRow $
                       [plainCell (totalOf (assetName net)), plainCell "", plainCell (count (assetFunded asset)), plainCell (amount (assetTotal asset))]
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
                ( [ Row BodyRow [remarkCell (holdingAddress holding) (holdingLabel holding), plainCell (poolPathText (holdingPath holding)), plainCell (amount (holdingBalance holding))]
                    | holding <- holdingsAddresses holdings
                  ]
                    <> [Row TotalRow [plainCell (totalOf (portfolioRefName naming (holdingsRef holdings))), plainCell "", plainCell (amount (holdingsTotal holdings))]]
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

    notes =
      preparationNotes (statementScope statement) naming
        <> [ case selection of
               Nothing -> "The statement lists the balances held at the statement date by the addresses of the wallets, on each blockchain. A balance is what the confirmed transactions paid to an address, less what they spent from it."
               Just stated ->
                 "The statement lists the balances held at the statement date by some of the addresses of the wallets only, on each blockchain: the addresses "
                   <> stated
                   <> " when the statement was prepared. A tag is a word the holder gave to an address, to group addresses. The wallets may hold more at other addresses, so a total is that of the addresses listed. A balance is what the confirmed transactions paid to an address, less what they spent from it.",
             case statementDate statement of
               AtEndOf _ _ ->
                 "A transaction is counted when its block is dated on the statement date or before, on the clock of the computer that prepared the statement. A block may be dated before the block that precedes it. Every block up to the last one dated in time is therefore counted, so that the balances are a state the blockchain went through. The section Blockchain data names the last block counted."
               AtPreparation ->
                 "Every transaction confirmed in the blockchain data of the last synchronisation is counted. The section Blockchain data names the last block counted.",
             Text.unwords ("Transactions that were not confirmed are not counted." : mapMaybe (\asset -> leftOutSentence (assetNetwork asset) (assetUnconfirmed asset)) assets),
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
        <> [ blockNote,
             "The reference is worked out from the statement date, the holder, "
               <> (if whole then "" else "the selection, ")
               <> "the balances and the prices. Two statements with the same reference state the same."
           ]
