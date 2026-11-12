{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | A listing as a sheet of a spreadsheet. A sheet is a row of headings,
-- then a row for each element. --csv writes the sheet of a listing to a
-- file ("Haskoin.Wallet.Cli.Csv"). exportsheet writes the sheets of the
-- six listings to one spreadsheet, a tab each ("Haskoin.Wallet.Cli.Ods").
--
-- A sheet is not the table of the screen, which --tabular shows. The
-- screen has 80 columns or so. A table there gives up what does not fit,
-- such as the fee and the hashes in full. A spreadsheet has room, and it
-- is where the user sorts, filters and sums. So a sheet gives every
-- figure hw has of an element, each in a column of its own. It gives
-- every identifier the commands take, every amount, the label of the
-- address apart from that of the transaction, and the addresses of a
-- transaction. It gives no colours and no paddings, and cuts nothing.
--
-- Each hash comes in full, then by its short id, the first 8 digits,
-- which the screen shows ("Haskoin.Wallet.ShortId"). The short id finds a
-- row of the sheet on the screen. The full hash finds it anywhere else.
--
-- Each cell has a kind ('Cell'), such as text, an amount in satoshis, a
-- count or a time. A CSV file holds text alone, so its writer decides
-- how each kind is written. An OpenDocument spreadsheet keeps each kind.
--
-- = One format for both files
--
-- The sheet of a listing is the same in its CSV file and in its tab of
-- the spreadsheet. Both have the same columns, in the same order, filled
-- the same way. Rows copied from one paste under the headings of the
-- other.
--
-- So the CSV file of a listing names the wallet and the portfolio on
-- every row. The listing covers one portfolio, and every row says the
-- same. The other choice was to leave them out of the CSV file, as hw
-- did before. The columns of the two files were then two apart, and no
-- row of one fitted the other.
--
-- The spreadsheet has two more tabs, which hold the addresses of the
-- transactions ('txAddressSheet'). No listing lists those, so --csv has
-- no file for them.
--
-- = The order of the columns
--
-- The columns follow four rules. A user who knows one sheet then finds
-- their way in the others.
--
-- 1. Every sheet starts with the account of the row. Its columns are
--    Wallet, Portfolio, Network and Type. A row of several accounts names
--    each type, as in "bip44, bip84". Such a row is a transaction between
--    the two accounts of a portfolio, or a portfolio on a network where
--    it has both. In a sheet of the addresses of transactions, Type is
--    the type of the account that holds the address of the row.
--
-- 2. Then comes what the listing is about, the most telling first. The
--    sheet of the coins goes on with the coin, in full and by its short
--    id. The sheet of the transactions goes on with the txid.
--
-- 3. A heading means one thing. Two sheets give the columns they share
--    in the same order. So the label of an address is Address label in
--    every sheet, and that of a transaction is Transaction label. The
--    screen calls Incoming, Outgoing and Internal the type of a
--    transaction. A sheet keeps Type for the type of the account, and
--    names that column Direction.
--
-- 4. The date the wallet created the element comes last, when it has
--    one. A portfolio, an address and a pending transaction have one. The
--    time of a transaction, or of the transaction that paid a coin, is
--    not such a date. A history is sorted by it, so it comes right after
--    the identifier.
--
-- The columns of a sheet are fixed, whatever the rows. A script, or a
-- sheet that refers to the columns of another, finds each column where it
-- was. A value unknown, or one that does not apply, leaves its cell
-- empty. The block of an unconfirmed transaction is one.
--
-- = Times
--
-- Each time takes three columns ('timeColumns').
--
-- The first gives it in UTC. One zone for every row sorts them right
-- across the changes of the clocks. It is also the time a block explorer
-- shows.
--
-- The second gives it on the clock of the computer, as the screen of hw
-- does.
--
-- The third says how far that clock was from UTC, as in UTC+01:00. The
-- offset is that of the day of the time, not of the day of the export. A
-- time of the summer is in summer time. Without this column, a local
-- time would not say which of the two hours it means when the clocks go
-- back.
--
-- The zone of each time comes from the computer, which is an action. The
-- columns take it as a function instead ('ZoneAt'). They stay pure, and
-- a test gives the zone it wants. 'localListingSheet' reads the zones of
-- the computer.
--
-- = The addresses of a transaction
--
-- A transaction spends from any number of addresses and pays any number.
-- A sheet has a fixed number of columns. No number of columns holds
-- every address of every transaction. So the addresses are given twice.
--
-- The row of a transaction names three of them, each with its path, its
-- label and its value ('mainAddressColumns'). They are the address it
-- spends the most from, the one it pays the most, and its change. It
-- counts the others in one column. Three addresses tell an ordinary
-- payment at a glance. More of them would widen every row for the few
-- transactions that have more.
--
-- A sheet of its own then has a row for each address of each transaction
-- ('txAddressSheet'). It leaves none out, whatever their number. A
-- spreadsheet works well on such rows. Its user filters on an address,
-- sums by label, and finds every payment to one address, even where that
-- address was not paid the most. The txid matches each row with the row
-- of its transaction.
--
-- = The spreadsheet of the wallet
--
-- exportsheet writes the six listings for every portfolio at once, and
-- the addresses of their transactions ('walletSheets'). The rows of each
-- portfolio follow those of the one before in each tab.
module Haskoin.Wallet.Cli.Sheet
  ( Cell (..),
    Sheet (..),
    ZoneAt,
    listingSheet,
    txAddressSheet,
    localListingSheet,
    localTxSheets,
    sheetTimes,
    TxSheets (..),
    PortfolioSheets (..),
    walletSheets,
    stacked,
  )
where

import Control.Monad (mfilter)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (LocalTime, TimeZone, UTCTime, getTimeZone, utc, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.PrettyPrinter (HeldAddress (..), TxAddress (..), coinStateText, paidTo, pendingStatusText, spentFrom, txTime, typeWord)
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Account (AccountKeys (..), NetworkView (..), PortfolioView (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..), addrPath, addrPoolPath, coinBalanceNotSpendableYet, isIntPath, pathText)
import Haskoin.Wallet.Database.Coin (JsonCoin (..), coinText)
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..), portfolioName, walletName)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (AccountKind, PoolPath (..), kindName)
import Haskoin.Wallet.ShortId (shortCoinText, shortHashText)
import Numeric.Natural (Natural)

-- | A cell of a sheet, with the kind of what it holds. A spreadsheet
-- treats each kind its own way: it keeps text as it is, sums numbers and
-- sorts times.
data Cell
  = -- | Text: a name, a label, an address, a hash or a word of state. It
    -- is never a number, nor a formula, whatever it starts with.
    TextCell !Text
  | -- | An amount in satoshis, below zero for what leaves the account.
    -- Satoshis are whole numbers: a spreadsheet holds every amount
    -- exactly, all 21 million bitcoins included, and no locale reads its
    -- decimal point the wrong way.
    SatoshiCell !Integer
  | -- | A whole number that counts something: confirmations, coins,
    -- addresses, a block height, satoshis per virtual byte
    CountCell !Natural
  | -- | A time in UTC
    TimeCell !UTCTime
  | -- | A time as a clock read it, in the zone the column beside it
    -- names. A spreadsheet has no time zones, so the cell holds the
    -- reading of the clock alone.
    LocalTimeCell !LocalTime
  | -- | Nothing: a value hw does not know, or one that does not apply
    EmptyCell
  deriving (Eq, Show)

-- | A sheet: the heading of each column, then the cells of each row, one
-- for each column
data Sheet = Sheet
  { sheetHeadings :: ![Text],
    sheetRows :: ![[Cell]]
  }
  deriving (Eq, Show)

-- | A column of the sheet of a listing: its heading, and its cell in the
-- row of an element. A heading goes with its cell, so that the two cannot
-- drift apart.
data Column element = Column !Text !(element -> Cell)

-- | The sheet of the elements, a row for each, under the columns given
sheetOf :: [Column element] -> [element] -> Sheet
sheetOf columns elements =
  Sheet
    { sheetHeadings = [heading | Column heading _ <- columns],
      sheetRows = [[cell element | Column _ cell <- columns] | element <- elements]
    }

-- | The columns of the elements that hold what the columns given read:
-- columns shared by two listings whose elements differ
through :: (element -> part) -> [Column part] -> [Column element]
through part columns = [Column heading (cell . part) | Column heading cell <- columns]

-- | The zone of the clock of the computer at a time. It is summer time
-- or not, as it was on that day. The sheets take it as a function, so
-- that they are made without asking the computer.
type ZoneAt = UTCTime -> TimeZone

-- | The sheet of the answer of a listing, which is one of portfolios,
-- addresses, balances, coins, txs and pendingtxs. Nothing for any other
-- answer, an error included. The rows of the groups of an answer follow
-- each other, each with the portfolio and the network of its group. The
-- function gives the zone each time is read in for its local columns.
listingSheet :: ZoneAt -> Response -> Maybe Sheet
listingSheet zoneAt = \case
  ResponsePortfolios portfolios -> Just (sheetOf (portfolioColumns zoneAt) (concatMap portfolioRows portfolios))
  ResponseAddresses groups -> Just (sheetOf (addressColumns zoneAt) (concatMap inPlace groups))
  ResponseBalances groups -> Just (sheetOf (balanceColumns zoneAt) (concatMap (inPlace . fst) groups))
  ResponseCoins groups -> Just (sheetOf (coinColumns zoneAt) (concatMap inPlace groups))
  ResponseTxs groups -> Just (sheetOf (txColumns zoneAt) (concatMap inPlace groups))
  ResponsePendingTxs _ held -> Just (sheetOf (pendingColumns zoneAt) held)
  _ -> Nothing

-- | The sheet of the addresses of the transactions of an answer of txs or
-- of pendingtxs. It has a row for each address each transaction spends
-- from or pays ('txAddressRows'), in the order of the transactions.
-- Nothing for any other answer. The function gives the zone each time is
-- read in for its local columns.
txAddressSheet :: ZoneAt -> Response -> Maybe Sheet
txAddressSheet zoneAt = \case
  ResponseTxs groups -> Just (sheetOf (txAddressRowColumns zoneAt) (concatMap (txAddressRows . fmap listedTxInfo) (concatMap inPlace groups)))
  ResponsePendingTxs _ held -> Just (sheetOf (pendingAddressRowColumns zoneAt) (concatMap txAddressRows held))
  _ -> Nothing

-- | The rows of a group of an answer, each with the portfolio and the
-- network of the group
inPlace :: Group row -> [(Place, row)]
inPlace g = (groupPlace g,) <$> groupRows g

-- | A sheet of an answer, its local times on the clock of this computer.
-- The function given makes the sheet with the zones it is given.
--
-- The zone of a time is read from the computer, and a sheet takes the
-- zones as a function. So the sheet is made twice. The first one, all in
-- UTC, tells which times the sheet holds. The second one has the zone of
-- each. A zone changes no time in UTC, so the times of the first sheet
-- are those of the second, and none is left without its zone.
withLocalZones :: (ZoneAt -> Response -> Maybe Sheet) -> Response -> IO (Maybe Sheet)
withLocalZones sheetWith answer = case sheetWith (const utc) answer of
  Nothing -> return Nothing
  Just inUtc -> do
    zones <- Map.fromList <$> mapM (\time -> (time,) <$> getTimeZone time) (nubOrd (sheetTimes inUtc))
    return $ sheetWith (\time -> Map.findWithDefault utc time zones) answer

-- | The sheet of the answer of a listing, its local times on the clock of
-- this computer ('listingSheet')
localListingSheet :: Response -> IO (Maybe Sheet)
localListingSheet = withLocalZones listingSheet

-- | The two sheets of an answer of txs or of pendingtxs, their local
-- times on the clock of this computer: the transactions ('listingSheet'),
-- and the addresses of each ('txAddressSheet'). Nothing for any other
-- answer.
localTxSheets :: Response -> IO (Maybe TxSheets)
localTxSheets answer = do
  transactions <- withLocalZones listingSheet answer
  addresses <- withLocalZones txAddressSheet answer
  return (TxSheets <$> transactions <*> addresses)

-- | Every time a sheet gives in UTC, in the order of its rows
sheetTimes :: Sheet -> [UTCTime]
sheetTimes sheet = [time | TimeCell time <- concat (sheetRows sheet)]

{- Cells -}

-- | Text in a cell, or an empty cell for no text, as a missing label
textCell :: Text -> Cell
textCell t
  | Text.null t = EmptyCell
  | otherwise = TextCell t

-- | An amount in satoshis in a cell
satoshiCell :: (Integral a) => a -> Cell
satoshiCell = SatoshiCell . toInteger

-- | A count in a cell
countCell :: (Integral a) => a -> Cell
countCell = CountCell . fromIntegral

-- | A derivation path in a cell, as hw writes it: /0/3
pathCell :: SoftPath -> Cell
pathCell = TextCell . pathText

-- | An address in full in a cell, as the network writes it
addressCell :: Network -> Address -> Cell
addressCell net addr = maybe EmptyCell TextCell (addrToText net addr)

-- | A hash in a cell, in hexadecimal, as the commands take it
hashCell :: TxHash -> Cell
hashCell = TextCell . txHashToHex

-- | The short id of a hash in a cell, its first 8 digits, as the screen
-- shows it and the commands take it too
shortHashCell :: TxHash -> Cell
shortHashCell = TextCell . cs . shortHashText

-- | Hashes in one cell, each in full, apart by a comma and a space. An
-- empty cell for none.
hashesCell :: [TxHash] -> Cell
hashesCell = textCell . Text.intercalate ", " . map txHashToHex

-- | The height of the block of a transaction in a cell, or an empty cell
-- for one in no block yet
blockCell :: Store.BlockRef -> Cell
blockCell = \case
  Store.BlockRef height _ -> countCell height
  Store.MemRef _ -> EmptyCell

{- Columns that several sheets share -}

-- | The first three columns of every sheet. They say where a row is, by
-- its wallet, its portfolio and its network. The type of the account
-- follows them ('typeColumn').
placeColumns :: [Column Place]
placeColumns =
  [ Column "Wallet" (TextCell . walletName . refWallet . placePortfolio),
    Column "Portfolio" (TextCell . portfolioName . refPortfolio . placePortfolio),
    Column "Network" (\place -> TextCell (cs (placeNetwork place).name))
  ]

-- | The fourth column of every sheet. It gives the type of the account
-- of a row, bip44 or bip84. A row of several accounts names each type,
-- apart by a comma and a space. The cell is empty for a row that has
-- none.
typeColumn :: (element -> [AccountKind]) -> Column element
typeColumn kinds = Column "Type" (textCell . Text.intercalate ", " . map kindName . kinds)

-- | The column of the chain of an address. It says receiving for an
-- address the user hands out, and change for one the wallet pays its
-- change to. The path tells it too, by its first number. A word is what
-- a filter of the spreadsheet picks.
chainColumn :: (element -> Bool) -> Column element
chainColumn isChange = Column "Chain" (TextCell . chainWord . isChange)

-- | The word for the chain of an address, given whether it is a change
-- address
chainWord :: Bool -> Text
chainWord isChange = if isChange then "change" else "receiving"

-- | The three columns of a time, under the name given. They give the
-- time in UTC, then on the clock of the computer, then how far that clock
-- was from UTC then ('offsetText'). All three are empty for a time hw
-- does not know.
timeColumns :: ZoneAt -> Text -> (element -> Maybe UTCTime) -> [Column element]
timeColumns zoneAt name timeOf =
  [ Column (name <> " (UTC)") (maybe EmptyCell TimeCell . timeOf),
    Column (name <> " (local)") (maybe EmptyCell (\time -> LocalTimeCell (utcToLocalTime (zoneAt time) time)) . timeOf),
    Column (name <> " (offset)") (maybe EmptyCell (TextCell . offsetText . zoneAt) . timeOf)
  ]

-- | How far the clock of a zone is from UTC, as UTC+01:00 or UTC-03:30.
-- The letters come first for the sake of the spreadsheet. It would read
-- +01:00 alone as a time of day, or as a formula.
offsetText :: TimeZone -> Text
offsetText zone = "UTC" <> Text.pack (formatTime defaultTimeLocale "%Ez" zone)

{- Portfolios -}

-- | The rows of a portfolio in the sheet of portfolios, one for each of
-- its networks
portfolioRows :: PortfolioView -> [(PortfolioView, NetworkView)]
portfolioRows portfolio = (portfolio,) <$> viewNetworks portfolio

-- | The columns of the portfolios, a row for each network of each. They
-- give where the portfolio is and the types of its accounts there,
-- whether it is the active one, its description and that of its wallet,
-- the paths of its accounts, what it can spend there now, the balances
-- the store gave, its coins and its addresses, and when it was created.
--
-- The column Active says yes or no. A filter of the spreadsheet picks
-- those words. It could not pick the * of the screen and an empty cell.
--
-- The sheet gives what the verbose view gives, but the keys of the
-- accounts and the fingerprint of the mnemonic. A key in a spreadsheet
-- would let anyone it reaches see every address and every payment of the
-- account. A fingerprint has 8 hexadecimal digits, and about one in 40
-- has no letter. A spreadsheet would read such a one as a number, and
-- drop its leading zeros, without a word. The verbose view of portfolios
-- gives both.
portfolioColumns :: ZoneAt -> [Column (PortfolioView, NetworkView)]
portfolioColumns zoneAt =
  through placeOf placeColumns
    <> [ typeColumn (map keysKind . networkViewAccounts . snd),
         Column "Active" $ \(portfolio, _) -> TextCell (if viewActive portfolio then "yes" else "no"),
         Column "Description" $ \(portfolio, _) -> textCell (viewDescription portfolio),
         Column "Wallet description" $ \(portfolio, _) -> textCell (viewWalletDescription portfolio),
         Column "Account paths" $ onNetwork (textCell . Text.intercalate ", " . map (cs . pathToStr . keysPath) . networkViewAccounts),
         Column "Spendable (sats)" $ onNetwork (satoshiCell . networkViewSpendable),
         Column "Confirmed (sats)" $ onNetwork (satoshiCell . networkViewConfirmed),
         Column "Unconfirmed (sats)" $ onNetwork (satoshiCell . networkViewUnconfirmed),
         Column "Coins" $ onNetwork (countCell . networkViewCoins),
         Column "Addresses" $ onNetwork (countCell . networkViewAddresses)
       ]
    <> timeColumns zoneAt "Created" (Just . viewCreated . fst)
  where
    placeOf (portfolio, net) = Place (viewRef portfolio) (networkViewNetwork net)
    onNetwork cell (_, net) = cell net

{- Addresses and balances -}

-- | The columns that name an address of a portfolio. They give where it
-- is, the type of its account, the address, its path there, its chain
-- and its label. addresses and balances start with them. The address
-- comes before its path. It is what another wallet or a block explorer
-- shows, and what every other sheet names it by. The path alone does not
-- name an address, since each account of the portfolio has one at
-- \/0\/3.
addressNameColumns :: [Column (Place, DBAddress)]
addressNameColumns =
  through fst placeColumns
    <> [ typeColumn (\(place, addr) -> maybe [] (pure . pathKind) (addrPoolPath (placeNetwork place) addr)),
         Column "Address" (TextCell . dBAddressAddress . snd),
         Column "Path" (pathCell . addrPath . snd),
         chainColumn (dBAddressInternal . snd),
         Column "Address label" (textCell . dBAddressLabel . snd)
       ]

-- | The columns of what the store counts at an address. addresses and
-- balances end with them. They give its confirmed and unconfirmed
-- balances, all it ever received, its coins and its transactions. Then
-- come the columns of when the wallet created the address.
addressStoreColumns :: ZoneAt -> [Column DBAddress]
addressStoreColumns zoneAt =
  [ Column "Confirmed (sats)" (satoshiCell . dBAddressBalanceConfirmed),
    Column "Unconfirmed (sats)" (satoshiCell . dBAddressBalanceUnconfirmed),
    Column "Received (sats)" (satoshiCell . dBAddressBalanceReceived),
    Column "Coins" (countCell . dBAddressBalanceCoins),
    Column "Transactions" (countCell . dBAddressBalanceTxs)
  ]
    <> timeColumns zoneAt "Created" (Just . dBAddressCreated)

-- | The columns of the addresses, each with what it can spend now. They
-- give what the verbose view gives.
addressColumns :: ZoneAt -> [Column (Place, (DBAddress, Word64))]
addressColumns zoneAt =
  through (fmap fst) addressNameColumns
    <> [Column "Spendable (sats)" $ \(_, (_, spendable)) -> satoshiCell spendable]
    <> through (fst . snd) (addressStoreColumns zoneAt)

-- | The columns of the addresses that hold coins, each with the balance
-- of its coins. They give what the verbose view gives, with the value
-- the wallet cannot spend in its three parts. Then come the columns of
-- the store that the sheet of the addresses has too. The two sheets
-- split the same coins two ways. This one splits them by what the wallet
-- can spend, and that one by what is confirmed.
balanceColumns :: ZoneAt -> [Column (Place, (DBAddress, CoinBalance))]
balanceColumns zoneAt =
  through (fmap fst) addressNameColumns
    <> through
      (snd . snd)
      [ Column "Total (sats)" (satoshiCell . coinBalanceTotal),
        Column "Spendable (sats)" (satoshiCell . coinBalanceSpendable),
        Column "Locked by tx (sats)" (satoshiCell . coinBalanceLockedByTx),
        Column "Locked by you (sats)" (satoshiCell . coinBalanceLockedByUser),
        Column "Not spendable yet (sats)" (satoshiCell . coinBalanceNotSpendableYet)
      ]
    <> through (fst . snd) (addressStoreColumns zoneAt)

{- Coins -}

-- | The columns of the coins. They give where the coin is and the type
-- of its account. Then the coin, in full and by its short id, as
-- lockcoin and --coin take it. Then the transaction that paid it, in
-- full and by its short id, and the index of its output there. Then the
-- time of that transaction, the value of the coin, its state, its
-- confirmations and its block. Then the address that holds it, with its
-- path, its chain and its label, and the label of the transaction.
--
-- The txid is in the name of the coin already. It has columns of its own
-- so that a coin is matched with its row in the sheet of the
-- transactions. That sheet names each transaction by its txid.
coinColumns :: ZoneAt -> [Column (Place, JsonCoin)]
coinColumns zoneAt =
  through fst placeColumns
    <> through
      snd
      ( [ typeColumn (maybe [] (pure . pathKind) . jsonCoinPath),
          Column "Coin" (TextCell . cs . coinText . jsonCoinOutpoint),
          Column "Short coin" (TextCell . cs . shortCoinText . jsonCoinOutpoint),
          Column "Txid" (\coin -> hashCell (jsonCoinOutpoint coin).hash),
          Column "Short txid" (\coin -> shortHashCell (jsonCoinOutpoint coin).hash),
          Column "Output index" (\coin -> countCell (jsonCoinOutpoint coin).index)
        ]
          <> timeColumns zoneAt "Time" jsonCoinTime
          <> [ Column "Value (sats)" (satoshiCell . jsonCoinValue),
               Column "State" (TextCell . cs . coinStateText "locked by a pending transaction"),
               Column "Confirmations" (countCell . jsonCoinConfirmations),
               Column "Block" (blockCell . jsonCoinBlock)
             ]
      )
    <> [Column "Address" (\(place, coin) -> addressCell (placeNetwork place) (jsonCoinAddress coin))]
    <> through
      snd
      [ Column "Path" (maybe EmptyCell (pathCell . pathInAccount) . jsonCoinPath),
        chainColumn jsonCoinChange,
        Column "Address label" (textCell . jsonCoinAddressLabel),
        Column "Transaction label" (textCell . jsonCoinTxLabel)
      ]

{- Transactions -}

-- The sheets of txs and of pendingtxs are made of the same columns, in
-- the same order. Each adds its own between them. txs adds the time of
-- the transaction, the balance after it and where it stands on the
-- chain. pendingtxs adds the nosigHash, what the transaction replaces
-- and when the wallet started holding it.

-- | The columns of the transactions of the portfolio, each with the
-- balance after it on its network. They give where the transaction is
-- and the types of the accounts it involves, its txid in full and by its
-- short id, its time, its direction, what it adds or takes, its fee, the
-- balance, where it stands, what replaced it, its label, its addresses
-- and its lock time.
txColumns :: ZoneAt -> [Column (Place, ListedTx)]
txColumns zoneAt =
  through held (txAccountColumns <> txidColumns)
    <> timeColumns zoneAt "Time" (txTime . listedTxInfo . snd)
    <> through held amountColumns
    <> [Column "Balance (sats)" (maybe EmptyCell SatoshiCell . listedTxBalance . snd)]
    <> through
      held
      ( [ statusColumn,
          Column "Confirmations" $ onChain (countCell . chainConfirmations),
          Column "Block" $ onChain (blockCell . chainBlock),
          Column "Replaced by" $ onChain (hashesCell . chainReplacedBy),
          txLabelColumn
        ]
          <> mainAddressColumns
          <> [lockTimeColumn]
      )
  where
    held = fmap listedTxInfo
    -- A cell of what the chain says of a transaction. txs lists no
    -- transaction in preparation, whose cell would be empty.
    onChain cell (_, info) = case txInfoStatus info of
      OnChain status -> cell status
      Pending _ -> EmptyCell

-- | The columns of the pending transactions. They give where the
-- transaction is and the types of the accounts it involves, both its
-- hashes, each in full and by its short id, its direction, what it adds
-- or takes, its fee, how far it got, what it replaces, its label, its
-- addresses, its lock time, and when the wallet started holding it. The
-- nosigHash comes first. It names the transaction before and after it is
-- signed, and the txid is empty until then.
pendingColumns :: ZoneAt -> [Column (Place, TxInfo)]
pendingColumns zoneAt =
  txAccountColumns
    <> nosigHashColumns
    <> txidColumns
    <> amountColumns
    <> [ Column "Change in fee (sats)" $ \(_, info) -> maybe EmptyCell satoshiCell (summaryChangeInFee (txInfoSummary info)),
         statusColumn,
         Column "Replaces" $ inPreparation (hashesCell . pendingReplaces),
         txLabelColumn
       ]
    <> mainAddressColumns
    <> [lockTimeColumn]
    <> timeColumns zoneAt "Created" (txTime . snd)

-- | A cell of what the wallet keeps of a transaction in preparation.
-- pendingtxs lists no other, whose cell would be empty.
inPreparation :: (PendingStatus -> Cell) -> (Place, TxInfo) -> Cell
inPreparation cell (_, info) = case txInfoStatus info of
  Pending status -> cell status
  OnChain _ -> EmptyCell

-- | The columns of the nosigHash of a transaction in preparation, in
-- full then by its short id
nosigHashColumns :: [Column (Place, TxInfo)]
nosigHashColumns =
  [ Column "Nosighash" $ inPreparation (hashCell . pendingNosigHash),
    Column "Short nosighash" $ inPreparation (shortHashCell . pendingNosigHash)
  ]

-- | The first four columns of a sheet of transactions. They give where
-- the transaction is, and the types of the accounts of the portfolio it
-- involves, those it spends from and those it pays.
txAccountColumns :: [Column (Place, TxInfo)]
txAccountColumns = through fst placeColumns <> [typeColumn (summaryKinds . txInfoSummary . snd)]

-- | The columns of the txid of a transaction, in full then by its short
-- id, both empty for one in preparation not signed yet
txidColumns :: [Column (Place, TxInfo)]
txidColumns =
  [ Column "Txid" $ \(_, info) -> maybe EmptyCell hashCell (txInfoHash info),
    Column "Short txid" $ \(_, info) -> maybe EmptyCell shortHashCell (txInfoHash info)
  ]

-- | The columns of what a transaction adds to the portfolio or takes
-- from it. They give its direction, its amount, fee included, its fee,
-- its fee rate and its size. The rate is counted on that size. The fee
-- of a payment received is the fee its payer paid.
amountColumns :: [Column (Place, TxInfo)]
amountColumns =
  directionColumn
    : through
      (txInfoSummary . snd)
      [ Column "Amount (sats)" (SatoshiCell . summaryAmount),
        Column "Fee (sats)" (satoshiCell . summaryFee),
        Column "Fee rate (sat/vB)" (countCell . summaryFeeByte),
        Column "Size (vB)" (countCell . summarySize)
      ]

-- | The column of the direction of a transaction: Incoming, Outgoing or
-- Internal, as the screen names its type
directionColumn :: Column (Place, TxInfo)
directionColumn = Column "Direction" $ \(_, info) -> TextCell (cs (typeWord (summaryType (txInfoSummary info))))

-- | The column of where a transaction stands ('statusText')
statusColumn :: Column (Place, TxInfo)
statusColumn = Column "Status" $ \(_, info) -> TextCell (statusText (txInfoStatus info))

-- | Where a transaction stands, in words. One on the chain is confirmed,
-- unconfirmed, or dead. A dead one was replaced or dropped, and never
-- confirms. One in preparation is as far as it got, which is one of not
-- signed, signed, sent and conflicted ('pendingStatusText').
statusText :: TxStatus -> Text
statusText = \case
  OnChain status
    | chainDead status -> "dead"
    | chainConfirmations status == 0 -> "unconfirmed"
    | otherwise -> "confirmed"
  Pending status -> cs (pendingStatusText status)

-- | The column of the label of a transaction
txLabelColumn :: Column (Place, TxInfo)
txLabelColumn = Column "Transaction label" $ \(_, info) -> textCell (txInfoLabel info)

-- | The column of the lock time of a transaction, as the transaction
-- holds it. It is the block the transaction waits for, or a Unix time
-- from 500,000,000 up. The cell is empty for a transaction that has
-- none. The lock time says the least of a transaction, so it comes after
-- its addresses.
lockTimeColumn :: Column (Place, TxInfo)
lockTimeColumn = Column "Lock time" $ \(_, info) -> case summaryLockTime (txInfoSummary info) of
  0 -> EmptyCell
  lockTime -> countCell lockTime

{- The addresses of a transaction -}

-- | The addresses of a transaction, by what it does with them, as its
-- verbose view lists them under From, To and Change. In each list the
-- address of the largest value comes first.
data TxAddresses = TxAddresses
  { -- | The addresses it spends from
    spentAddresses :: ![TxAddress],
    -- | The addresses it pays
    paidAddresses :: ![TxAddress],
    -- | The addresses it sends its change to
    changeAddresses :: ![TxAddress]
  }

-- | The addresses of a transaction ('spentFrom', 'paidTo'), the largest
-- value first under each role. An address whose value hw does not know,
-- one another wallet spends from, comes after those it knows.
--
-- What tells change from a payment is the direction of the transaction,
-- as in the verbose view. A payment the portfolio makes sends its change
-- back to the portfolio. A payment it receives has none here, since the
-- change of the payer is nothing to the portfolio. A transaction between
-- the addresses of the portfolio has none either, since no output of it
-- can be told to be the change.
txAddresses :: TxInfo -> TxAddresses
txAddresses info = TxAddresses (largestFirst (spentFrom info)) (largestFirst to) (largestFirst change)
  where
    (to, change) = paidTo info
    -- The sort keeps the order of addresses of the same value, so that
    -- the same transaction always gives the same order
    largestFirst = sortOn (Down . txAddressValue)

-- | The columns of the addresses the row of a transaction names. They
-- give the address it spends the most from, the one it pays the most,
-- its change, and the count of the addresses those leave out. The sheet
-- of the addresses has a row for each of them all ('txAddressSheet').
mainAddressColumns :: [Column (Place, TxInfo)]
mainAddressColumns =
  mainAddressOf "From" True spentAddresses
    <> mainAddressOf "To" True paidAddresses
    -- A change address has no label
    <> mainAddressOf "Change" False changeAddresses
    <> [Column "Other addresses" $ \(_, info) -> countCell (unnamedAddresses (txAddresses info))]

-- | How many addresses of a transaction its row does not name: all of
-- each role but the first
unnamedAddresses :: TxAddresses -> Int
unnamedAddresses addresses = sum [length (drop 1 (role addresses)) | role <- [spentAddresses, paidAddresses, changeAddresses]]

-- | The columns of the first address of a role, under the name given,
-- in the order of the sheet of the addresses. They give the address in
-- full, its path, its label when the flag asks for it, and what the
-- transaction spends from it or pays it.
--
-- An address of the portfolio comes with its path and its label. An
-- address of another portfolio comes without them. The row has no column
-- for the portfolio of a path, and a path alone would read as one of the
-- portfolio of the row. The sheet of the addresses names that portfolio.
-- The value is empty for an address another wallet spends from, since
-- its coins are nothing to this one.
mainAddressOf :: Text -> Bool -> (TxAddresses -> [TxAddress]) -> [Column (Place, TxInfo)]
mainAddressOf name labelled role =
  [ Column (name <> " address") $ \(place, info) -> maybe EmptyCell (addressCell (placeNetwork place) . txAddress) (mainOf info),
    Column (name <> " path") $ \(_, info) -> maybe EmptyCell (pathCell . pathInAccount . heldPath) (ownOf info)
  ]
    <> [Column (name <> " label") $ \(_, info) -> maybe EmptyCell (textCell . heldLabel) (ownOf info) | labelled]
    <> [Column (name <> " value (sats)") $ \(_, info) -> maybe EmptyCell satoshiCell (txAddressValue =<< mainOf info)]
  where
    mainOf = listToMaybe . role . txAddresses
    -- Where the address is, when it is of the portfolio of the
    -- transaction
    ownOf info = mfilter (isNothing . heldIn) (txAddressHeld =<< mainOf info)

{- The sheet of the addresses of transactions -}

-- | An address of a transaction, as a row of the sheet of the addresses
-- of transactions
data TxAddressRow = TxAddressRow
  { -- | The portfolio and the network of the transaction
    rowPlace :: !Place,
    rowTx :: !TxInfo,
    -- | What the transaction does with the address: from, to or change
    rowRole :: !Text,
    rowAddress :: !TxAddress
  }

-- | The rows of a transaction in the sheet of the addresses: one for each
-- address it spends from, then for each it pays, then for each it sends
-- change to, the largest value first under each role ('txAddresses'). So
-- the first row of a role is the address the row of the transaction
-- names. An input or an output that has no address, such as a note in
-- the chain, has no row.
txAddressRows :: (Place, TxInfo) -> [TxAddressRow]
txAddressRows (place, info) =
  [ TxAddressRow place info role address
  | (role, addresses) <- [("from", spentAddresses found), ("to", paidAddresses found), ("change", changeAddresses found)],
    address <- addresses
  ]
  where
    found = txAddresses info

-- | The transaction of a row, with where it is, as the columns of a
-- transaction read it
rowTxInPlace :: TxAddressRow -> (Place, TxInfo)
rowTxInPlace row = (rowPlace row, rowTx row)

-- | Where the address of a row is in the wallet, when the wallet holds it
rowHeld :: TxAddressRow -> Maybe HeldAddress
rowHeld = txAddressHeld . rowAddress

-- | The first four columns of a sheet of the addresses of transactions.
-- The first three say where the transaction is. Type is the type of the
-- account that holds the address, as in the sheet of the addresses. It
-- goes with the path of the row. It is empty for an address of another
-- wallet.
txAddressAccountColumns :: [Column TxAddressRow]
txAddressAccountColumns = through rowPlace placeColumns <> [typeColumn (maybe [] (pure . pathKind . heldPath) . rowHeld)]

-- | The columns of the address of a row: what the transaction does with
-- it, its value there, the address in full, then its path, its chain and
-- its label, and the wallet and the portfolio that hold it.
--
-- The value comes before the address, as in the sheet of the coins. The
-- role and the value then stand side by side, and read as "from 50000".
--
-- The path, the chain and the label are those of an address the wallet
-- holds. They are empty for an address of another wallet. The last two
-- columns name the portfolio that holds the address. For most rows it is
-- the portfolio of the transaction, and they repeat the first columns.
-- For a payment between two portfolios it is the other one. The row of a
-- transaction leaves the path of such an address out, since nothing
-- there says whose path it is. Here the two columns say it.
txAddressOwnColumns :: [Column TxAddressRow]
txAddressOwnColumns =
  [ Column "Role" (TextCell . rowRole),
    Column "Value (sats)" (maybe EmptyCell satoshiCell . txAddressValue . rowAddress),
    Column "Address" (\row -> addressCell (placeNetwork (rowPlace row)) (txAddress (rowAddress row))),
    Column "Path" (maybe EmptyCell (pathCell . pathInAccount . heldPath) . rowHeld),
    Column "Chain" (maybe EmptyCell (TextCell . chainWord . isIntPath . pathInAccount . heldPath) . rowHeld),
    Column "Address label" (maybe EmptyCell (textCell . heldLabel) . rowHeld),
    Column "Held by wallet" (maybe EmptyCell (TextCell . walletName . refWallet) . holder),
    Column "Held by portfolio" (maybe EmptyCell (TextCell . portfolioName . refPortfolio) . holder)
  ]
  where
    -- The portfolio that holds the address: another one when the address
    -- says so, or else the portfolio of the transaction
    holder row = fromMaybe (placePortfolio (rowPlace row)) . heldIn <$> rowHeld row

-- | The columns of the addresses of the transactions of txs. Each row
-- starts as the row of its transaction does, so that the two are
-- matched: where it is, the txid in full and by its short id, the time,
-- the direction and the status. Then come the columns of the address
-- ('txAddressOwnColumns'), and the label of the transaction.
--
-- The row repeats the time, the direction and the status of its
-- transaction. A spreadsheet joins two sheets poorly, and its user
-- filters these rows by date, or leaves the dead transactions out,
-- without looking anything up.
txAddressRowColumns :: ZoneAt -> [Column TxAddressRow]
txAddressRowColumns zoneAt =
  txAddressAccountColumns
    <> through rowTxInPlace txidColumns
    <> timeColumns zoneAt "Time" (txTime . rowTx)
    <> through rowTxInPlace [directionColumn, statusColumn]
    <> txAddressOwnColumns
    <> through rowTxInPlace [txLabelColumn]

-- | The columns of the addresses of the pending transactions: those of
-- 'txAddressRowColumns' that apply, in the same order. The nosigHash
-- comes before the txid, as in the sheet of the pending transactions. A
-- pending transaction has no time on the chain, and when the wallet
-- started holding it comes last.
pendingAddressRowColumns :: ZoneAt -> [Column TxAddressRow]
pendingAddressRowColumns zoneAt =
  txAddressAccountColumns
    <> through rowTxInPlace (nosigHashColumns <> txidColumns <> [directionColumn, statusColumn])
    <> txAddressOwnColumns
    <> through rowTxInPlace [txLabelColumn]
    <> timeColumns zoneAt "Created" (txTime . rowTx)

{- The spreadsheet of the wallet -}

-- | The two sheets of a listing of transactions: the transactions, a row
-- each, and their addresses, a row for each address of each
data TxSheets = TxSheets
  { transactionsSheet :: !Sheet,
    txAddressesSheet :: !Sheet
  }
  deriving (Eq, Show)

-- | The sheets of the listings of one portfolio that exportsheet writes:
-- its addresses, of both chains, the balances of those that hold coins,
-- its coins, locked or not, and its transactions with their addresses,
-- each over every network
data PortfolioSheets = PortfolioSheets
  { addressesSheet :: !Sheet,
    balancesSheet :: !Sheet,
    coinsSheet :: !Sheet,
    txsSheets :: !TxSheets
  }
  deriving (Eq, Show)

-- | The sheets of the spreadsheet of exportsheet, each with the name of
-- its tab, in the order of the help. They are the portfolios, then the
-- addresses, the balances, the coins and the transactions of each
-- portfolio given, then the pending transactions. Each of these six tabs
-- is the sheet of its listing as --csv writes it, and the rows of the
-- portfolios follow each other ('stacked'). The tab of the addresses of
-- the transactions comes right after that of the transactions, and
-- likewise for the pending ones. The longest name has 29 characters.
-- Excel takes 31 at most.
--
-- One sheet holds all the portfolios, rather than a tab for each. It
-- sorts and sums across the wallet, since every row names its wallet and
-- its portfolio. Amounts of Bitcoin and of Bitcoin Cash then share a
-- column, and the network tells them apart.
walletSheets :: Sheet -> [PortfolioSheets] -> TxSheets -> [(Text, Sheet)]
walletSheets portfolios held pending =
  [ ("Portfolios", portfolios),
    ("Addresses", ofEach addressesSheet),
    ("Balances", ofEach balancesSheet),
    ("Coins", ofEach coinsSheet),
    ("Transactions", ofEach (transactionsSheet . txsSheets)),
    ("Transaction addresses", ofEach (txAddressesSheet . txsSheets)),
    ("Pending transactions", transactionsSheet pending),
    ("Pending transaction addresses", txAddressesSheet pending)
  ]
  where
    ofEach part = stacked (part <$> held)

-- | Sheets of the same columns as one: the headings of the first, then
-- the rows of each, one sheet after the other. None makes a sheet of no
-- column.
stacked :: [Sheet] -> Sheet
stacked sheets =
  Sheet
    { sheetHeadings = concat (take 1 (sheetHeadings <$> sheets)),
      sheetRows = concatMap sheetRows sheets
    }
