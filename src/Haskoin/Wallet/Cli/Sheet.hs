{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | A listing as a sheet of a spreadsheet: a row of headings, then a row
-- for each element, as --csv writes it to a file ("Haskoin.Wallet.Cli.Csv").
--
-- A sheet is not the table of the screen, which --tabular shows. The
-- screen has 80 columns or so, and a table there gives up what does
-- not fit: the fee, and the hashes in full, of which it gives the short
-- ids alone. A spreadsheet has room, and is where the user sorts,
-- filters and sums. So a sheet gives what the answer in JSON gives: every
-- identifier the commands take, every amount, and the label of the
-- address apart from that of the transaction, each in a column of its
-- own. It gives no colours and no paddings, and cuts nothing. Each hash
-- comes in full, then by its short id, the first 8 digits, which the
-- screen shows ("Haskoin.Wallet.ShortId"): the short id finds a row of
-- the sheet on the screen, and the full hash finds it anywhere else.
--
-- Each cell has a kind ('Cell'): text, an amount in satoshis, a count, a
-- time. A CSV file holds text alone, so its writer decides how each kind
-- is written. A file with typed cells, such as an OpenDocument or an
-- Office Open XML spreadsheet, would keep each kind, and could be written
-- from the same sheets.
--
-- The columns of a sheet are fixed, whatever the rows: a script, or a
-- sheet that refers to the columns of another, finds each column where it
-- was. A value unknown, or one that does not apply, such as the block of
-- an unconfirmed transaction, leaves its cell empty.
--
-- A listing over a portfolio covers several networks, and for the
-- addresses both types. Its sheet has one row for each element, whatever
-- its network, with the network in a first column, and the type of an
-- address in a second: a sheet has no sections, and its user filters on a
-- column instead.
--
-- exportsheet writes the six listings to one spreadsheet, a tab each
-- ('walletSheets'), for every portfolio at once. The sheets of the
-- listings of one portfolio then get two first columns that name its
-- wallet and the portfolio, and the rows of each portfolio follow those
-- of the one before.
module Haskoin.Wallet.Cli.Sheet
  ( Cell (..),
    Sheet (..),
    listingSheet,
    PortfolioSheets (..),
    walletSheets,
    withPortfolioColumns,
    stacked,
  )
where

import Control.Monad (mfilter)
import Data.Maybe (isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.PrettyPrinter (HeldAddress (..), Payee (..), TxAddress (..), coinStateText, mainPayee, pendingStatusText, txTime, typeWord)
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Account (NetworkView (..), PortfolioView (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..), addrPath, addrPoolPath, coinBalanceNotSpendableYet, pathText)
import Haskoin.Wallet.Database.Coin (JsonCoin (..), coinText)
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..), portfolioName, walletName)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (PoolPath (..), kindName)
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
  | -- | A time, in UTC: a spreadsheet has no time zones, and one zone for
    -- every row sorts them right across the changes of the clocks
    TimeCell !UTCTime
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

-- | The sheet of the answer of a listing: portfolios, addresses,
-- balances, coins, txs or pendingtxs. Nothing for any other answer, an
-- error included. The rows of the groups of an answer follow each other,
-- each with the network of its group.
listingSheet :: Response -> Maybe Sheet
listingSheet = \case
  ResponsePortfolios portfolios -> Just (sheetOf portfolioColumns (concatMap portfolioRows portfolios))
  ResponseAddresses groups -> Just (sheetOf addressColumns (concatMap onNetwork groups))
  ResponseBalances groups -> Just (sheetOf balanceColumns (concatMap (onNetwork . fst) groups))
  ResponseCoins groups -> Just (sheetOf coinColumns (concatMap onNetwork groups))
  ResponseTxs groups -> Just (sheetOf txColumns (concatMap onNetwork groups))
  -- A sheet names the wallet of every row, in a column of its own
  ResponsePendingTxs _ held -> Just (sheetOf pendingColumns held)
  _ -> Nothing
  where
    -- The rows of a group, each with the network of the group
    onNetwork :: Group row -> [(Network, row)]
    onNetwork g = (placeNetwork (groupPlace g),) <$> groupRows g

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

-- | A time in a cell, or an empty cell for a time hw does not know
timeCell :: Maybe UTCTime -> Cell
timeCell = maybe EmptyCell TimeCell

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

-- | The height of the block of a transaction in a cell, or an empty cell
-- for one in no block yet
blockCell :: Store.BlockRef -> Cell
blockCell = \case
  Store.BlockRef height _ -> countCell height
  Store.MemRef _ -> EmptyCell

{- Portfolios -}

-- | The rows of a portfolio in the sheet of portfolios: one for each of
-- its networks
portfolioRows :: PortfolioView -> [(PortfolioView, NetworkView)]
portfolioRows portfolio = (portfolio,) <$> viewNetworks portfolio

-- | The columns of the portfolios, a row for each network of each: the
-- wallet, the portfolio, whether it is the active one, its description,
-- the network, what the portfolio can spend there now, the balances the
-- store gave, its coins and its addresses, and when the portfolio was
-- created. The column Active says yes or no, words that a filter of the
-- spreadsheet picks, rather than the * of the screen and an empty cell.
-- What the verbose
-- view gives, but the keys of the accounts and the fingerprint of the
-- mnemonic. A key in a spreadsheet would let anyone it reaches see every
-- address and every payment of the account. A fingerprint has 8
-- hexadecimal digits, and about one in 40 has no letter: a spreadsheet
-- would read it as a number, and drop its leading zeros, without a word.
-- The verbose view of portfolios gives both.
portfolioColumns :: [Column (PortfolioView, NetworkView)]
portfolioColumns =
  [ Column "Wallet" $ \(portfolio, _) -> TextCell (walletName (refWallet (viewRef portfolio))),
    Column "Portfolio" $ \(portfolio, _) -> TextCell (portfolioName (refPortfolio (viewRef portfolio))),
    Column "Active" $ \(portfolio, _) -> TextCell (if viewActive portfolio then "yes" else "no"),
    Column "Description" $ \(portfolio, _) -> textCell (viewDescription portfolio),
    Column "Network" $ onNetwork (\net -> TextCell (cs (networkViewNetwork net).name)),
    Column "Spendable (sats)" $ onNetwork (satoshiCell . networkViewSpendable),
    Column "Confirmed (sats)" $ onNetwork (satoshiCell . networkViewConfirmed),
    Column "Unconfirmed (sats)" $ onNetwork (satoshiCell . networkViewUnconfirmed),
    Column "Coins" $ onNetwork (countCell . networkViewCoins),
    Column "Addresses" $ onNetwork (countCell . networkViewAddresses),
    Column "Created (UTC)" $ \(portfolio, _) -> TimeCell (viewCreated portfolio)
  ]
  where
    onNetwork cell (_, net) = cell net

-- | The columns that say where a row of several portfolios is: its
-- wallet, its portfolio and its network
placeColumns :: [Column Place]
placeColumns =
  [ Column "Wallet" (TextCell . walletName . refWallet . placePortfolio),
    Column "Portfolio" (TextCell . portfolioName . refPortfolio . placePortfolio),
    Column "Network" (\place -> TextCell (cs (placeNetwork place).name))
  ]

-- | The column of the network of a row of a listing over a portfolio
networkColumn :: Column (Network, row)
networkColumn = Column "Network" $ \(net, _) -> TextCell (cs net.name)

{- Addresses and balances -}

-- | The columns that name an address of the portfolio: its network, the
-- type of its account, its path there, the address and its label.
-- addresses and balances start with them. The path alone does not name
-- an address: each account of the portfolio has one at /0/3.
addressNameColumns :: [Column (Network, DBAddress)]
addressNameColumns =
  [ Column "Network" $ \(net, _) -> TextCell (cs net.name),
    Column "Type" $ \(net, addr) -> maybe EmptyCell (TextCell . kindName . pathKind) (addrPoolPath net addr),
    Column "Path" (pathCell . addrPath . snd),
    Column "Address" (TextCell . dBAddressAddress . snd),
    Column "Label" (textCell . dBAddressLabel . snd)
  ]

-- | The columns of the addresses, each with what it can spend now: what
-- the verbose view gives
addressColumns :: [Column (Network, (DBAddress, Word64))]
addressColumns =
  through (fmap fst) addressNameColumns
    <> through
      snd
      [ Column "Spendable (sats)" $ \(_, spendable) -> satoshiCell spendable,
        Column "Confirmed (sats)" $ \(addr, _) -> satoshiCell (dBAddressBalanceConfirmed addr),
        Column "Unconfirmed (sats)" $ \(addr, _) -> satoshiCell (dBAddressBalanceUnconfirmed addr),
        Column "Received (sats)" $ \(addr, _) -> satoshiCell (dBAddressBalanceReceived addr),
        Column "Coins" $ \(addr, _) -> countCell (dBAddressBalanceCoins addr),
        Column "Transactions" $ \(addr, _) -> countCell (dBAddressBalanceTxs addr),
        Column "Created (UTC)" $ \(addr, _) -> TimeCell (dBAddressCreated addr)
      ]

-- | The columns of the addresses that hold coins, each with the balance
-- of its coins: what the verbose view gives, the value the wallet cannot
-- spend in its three parts
balanceColumns :: [Column (Network, (DBAddress, CoinBalance))]
balanceColumns =
  through (fmap fst) addressNameColumns
    <> through
      snd
      [ Column "Total (sats)" $ \(_, balance) -> satoshiCell (coinBalanceTotal balance),
        Column "Spendable (sats)" $ \(_, balance) -> satoshiCell (coinBalanceSpendable balance),
        Column "Locked by tx (sats)" $ \(_, balance) -> satoshiCell (coinBalanceLockedByTx balance),
        Column "Locked by you (sats)" $ \(_, balance) -> satoshiCell (coinBalanceLockedByUser balance),
        Column "Not spendable yet (sats)" $ \(_, balance) -> satoshiCell (coinBalanceNotSpendableYet balance),
        Column "Coins" $ \(addr, _) -> countCell (dBAddressBalanceCoins addr)
      ]

{- Coins -}

-- | The columns of the coins: the network, the coin, in full and by its
-- short id, as lockcoin and --coin take it, the time of the transaction
-- that paid it, its value, its state and its confirmations, the address
-- that holds it, with the type of its account and its path there, and
-- both labels
coinColumns :: [Column (Network, JsonCoin)]
coinColumns =
  networkColumn
    : through
      snd
      [ Column "Coin" (TextCell . cs . coinText . jsonCoinOutpoint),
        Column "Short coin" (TextCell . cs . shortCoinText . jsonCoinOutpoint),
        Column "Time (UTC)" (timeCell . jsonCoinTime),
        Column "Value (sats)" (satoshiCell . jsonCoinValue),
        Column "State" (TextCell . cs . coinStateText "locked by a pending transaction"),
        Column "Confirmations" (countCell . jsonCoinConfirmations),
        Column "Block" (blockCell . jsonCoinBlock)
      ]
    <> [Column "Address" (\(net, coin) -> addressCell net (jsonCoinAddress coin))]
    <> through
      snd
      [ Column "Type" (maybe EmptyCell (TextCell . kindName . pathKind) . jsonCoinPath),
        Column "Path" (maybe EmptyCell (pathCell . pathInAccount) . jsonCoinPath),
        Column "Address label" (textCell . jsonCoinAddressLabel),
        Column "Transaction label" (textCell . jsonCoinTxLabel)
      ]

{- Transactions -}

-- | The columns of the transactions of the portfolio, each with the
-- balance after it on its network: the network, when, what it adds or
-- takes, where it stands, its txid in full and by its short id, and what
-- it is for
txColumns :: [Column (Network, ListedTx)]
txColumns =
  [networkColumn, Column "Time (UTC)" (timeCell . txTime . listedTxInfo . snd)]
    <> through held amountColumns
    <> [Column "Balance (sats)" (maybe EmptyCell SatoshiCell . listedTxBalance . snd)]
    <> through
      held
      ( [ statusColumn,
          Column "Confirmations" $ \(_, info) -> case txInfoStatus info of
            OnChain status -> countCell (chainConfirmations status)
            Pending _ -> EmptyCell,
          Column "Block" $ \(_, info) -> case txInfoStatus info of
            OnChain status -> blockCell (chainBlock status)
            Pending _ -> EmptyCell
        ]
          <> txidColumns
          <> purposeColumns
      )
  where
    held = fmap listedTxInfo

-- | The columns of the pending transactions, each with where it is: its
-- wallet, its portfolio and its network, when the wallet started holding
-- it, what it adds or takes, how far it got, both its hashes, each in
-- full and by its short id, and what it is for
pendingColumns :: [Column (Place, TxInfo)]
pendingColumns =
  through fst placeColumns
    <> [Column "Created (UTC)" $ \(_, info) -> timeCell (txTime info)]
    <> through
      held
      ( amountColumns
          <> [ statusColumn,
               Column "Nosighash" $ \(_, info) -> maybe EmptyCell hashCell (nosigHashOf info),
               Column "Short nosighash" $ \(_, info) -> maybe EmptyCell shortHashCell (nosigHashOf info)
             ]
          <> txidColumns
          <> purposeColumns
      )
  where
    held (place, info) = (placeNetwork place, info)
    nosigHashOf info = case txInfoStatus info of
      Pending status -> Just (pendingNosigHash status)
      OnChain _ -> Nothing

-- | The columns of what a transaction adds to the portfolio or takes
-- from it: its type, the types of the addresses of the portfolio it
-- involves, its amount, fee included, its fee and its fee rate. The fee
-- of a payment received is the fee its payer paid.
amountColumns :: [Column (Network, TxInfo)]
amountColumns =
  [ Column "Type" $ \(_, info) -> TextCell (cs (typeWord (summaryType (txInfoSummary info)))),
    Column "Address types" $ \(_, info) -> textCell (Text.intercalate ", " (kindName <$> summaryKinds (txInfoSummary info))),
    Column "Amount (sats)" $ \(_, info) -> SatoshiCell (summaryAmount (txInfoSummary info)),
    Column "Fee (sats)" $ \(_, info) -> satoshiCell (summaryFee (txInfoSummary info)),
    Column "Fee rate (sat/vB)" $ \(_, info) -> countCell (summaryFeeByte (txInfoSummary info))
  ]

-- | The column of where a transaction stands ('statusText')
statusColumn :: Column (Network, TxInfo)
statusColumn = Column "Status" $ \(_, info) -> TextCell (statusText (txInfoStatus info))

-- | The columns of the txid of a transaction, in full then by its short
-- id, both empty for one in preparation not signed yet
txidColumns :: [Column (Network, TxInfo)]
txidColumns =
  [ Column "Txid" $ \(_, info) -> maybe EmptyCell hashCell (txInfoHash info),
    Column "Short txid" $ \(_, info) -> maybe EmptyCell shortHashCell (txInfoHash info)
  ]

-- | Where a transaction stands, in words. One on the chain is confirmed,
-- unconfirmed, or dead: replaced or dropped, never to confirm. One in
-- preparation is as far as it got: not signed, signed, sent or
-- conflicted ('pendingStatusText').
statusText :: TxStatus -> Text
statusText = \case
  OnChain status
    | chainDead status -> "dead"
    | chainConfirmations status == 0 -> "unconfirmed"
    | otherwise -> "confirmed"
  Pending status -> cs (pendingStatusText status)

-- | The columns of what a transaction is for: its label, then the
-- address it pays the most ('mainPayee'), in full. An address of the
-- portfolio comes with its path and its label. An address of another
-- portfolio comes without them: the sheet has no column for the portfolio
-- of a path, and a path alone would read as one of the portfolio of the
-- sheet. The views say what is left as "and 2 more": a count of the other
-- addresses paid.
purposeColumns :: [Column (Network, TxInfo)]
purposeColumns =
  [ Column "Label" $ \(_, info) -> textCell (txInfoLabel info),
    Column "Address" $ \(net, info) -> maybe EmptyCell (addressCell net . txAddress . payeeRow) (payeeOf info),
    Column "Path" $ \(_, info) -> maybe EmptyCell (pathCell . pathInAccount . heldPath) (ownPayee info),
    Column "Address label" $ \(_, info) -> maybe EmptyCell (textCell . heldLabel) (ownPayee info),
    Column "Other addresses" $ \(_, info) -> countCell (maybe 0 payeeOthers (payeeOf info))
  ]
  where
    payeeOf = mainPayee
    -- The path and the label of the address paid, when it is of the
    -- portfolio of the transaction
    ownPayee info = mfilter (isNothing . heldIn) (txAddressHeld . payeeRow =<< payeeOf info)

{- The spreadsheet of the wallet -}

-- | The sheets of the listings of one portfolio that exportsheet writes:
-- its addresses, of both chains, the balances of those that hold coins,
-- its coins, locked or not, and its transactions, each over every network
data PortfolioSheets = PortfolioSheets
  { addressesSheet :: !Sheet,
    balancesSheet :: !Sheet,
    coinsSheet :: !Sheet,
    txsSheet :: !Sheet
  }
  deriving (Eq, Show)

-- | The sheets of the spreadsheet of exportsheet, each with the name of
-- its tab, in the order of the help: the portfolios, the addresses, the
-- balances, the coins and the transactions of each portfolio given, and
-- the pending transactions. A sheet of one portfolio gets two first
-- columns that name its wallet and the portfolio
-- ('withPortfolioColumns'), as the portfolios and the pending
-- transactions name theirs, and the rows of the portfolios follow each
-- other ('stacked'). One sheet for all of them, rather than a tab for
-- each portfolio, sorts and sums across the wallet. Amounts of Bitcoin
-- and of Bitcoin Cash then share a column: the network tells them apart.
walletSheets :: Sheet -> [(PortfolioRef, PortfolioSheets)] -> Sheet -> [(Text, Sheet)]
walletSheets portfolios held pending =
  [ ("Portfolios", portfolios),
    ("Addresses", ofEach addressesSheet),
    ("Balances", ofEach balancesSheet),
    ("Coins", ofEach coinsSheet),
    ("Transactions", ofEach txsSheet),
    ("Pending transactions", pending)
  ]
  where
    ofEach part = stacked [withPortfolioColumns ref (part sheets) | (ref, sheets) <- held]

-- | A sheet with two first columns, Wallet and Portfolio, that name the
-- portfolio given on every row
withPortfolioColumns :: PortfolioRef -> Sheet -> Sheet
withPortfolioColumns ref sheet =
  Sheet
    { sheetHeadings = "Wallet" : "Portfolio" : sheetHeadings sheet,
      sheetRows = ([TextCell (walletName (refWallet ref)), TextCell (portfolioName (refPortfolio ref))] <>) <$> sheetRows sheet
    }

-- | Sheets of the same columns as one: the headings of the first, then
-- the rows of each, one sheet after the other. None makes a sheet of no
-- column.
stacked :: [Sheet] -> Sheet
stacked sheets =
  Sheet
    { sheetHeadings = concat (take 1 (sheetHeadings <$> sheets)),
      sheetRows = concatMap sheetRows sheets
    }
