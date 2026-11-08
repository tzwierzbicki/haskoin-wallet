{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A listing as a sheet of a spreadsheet: a row of headings, then a row
-- for each element, as --csv writes it to a file ("Haskoin.Wallet.Cli.Csv").
--
-- A sheet is not the table of the screen, which --tabular shows. The
-- screen has 80 columns or so, and a table there gives up what does
-- not fit: the txid, the coin, the fee, the hashes of a pending
-- transaction. A spreadsheet has room, and is where the user sorts,
-- filters and sums. So a sheet gives what the answer in JSON gives: every
-- identifier the commands take, every amount, and the label of the
-- address apart from that of the transaction, each in a column of its
-- own. It gives no colours and no paddings, and cuts nothing.
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
-- exportsheet writes the six listings to one spreadsheet, a tab each
-- ('walletSheets'), for every account at once. The sheets of the listings
-- of one account then get a first column that names it, and the rows of
-- each account follow those of the one before.
module Haskoin.Wallet.Cli.Sheet
  ( Cell (..),
    Sheet (..),
    listingSheet,
    AccountSheets (..),
    walletSheets,
    withAccountColumn,
    stacked,
  )
where

import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.PrettyPrinter (Payee (..), TxAddress (..), coinStateText, mainPayee, pendingStatusText, txTime, typeWord)
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..), DBAddress (..))
import Haskoin.Wallet.Database.Account (accountNetwork, accountPath)
import Haskoin.Wallet.Database.Address (CoinBalance (..), addrPath, coinBalanceNotSpendableYet, pathText)
import Haskoin.Wallet.Database.Coin (JsonCoin (..), coinText)
import Haskoin.Wallet.Database.TxInfo
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

-- | The sheet of the answer of a listing: accounts, addresses, balances,
-- coins, txs or pendingtxs. Nothing for any other answer, an error
-- included.
listingSheet :: Response -> Maybe Sheet
listingSheet = \case
  ResponseAccounts accs -> Just (sheetOf accountColumns accs)
  ResponseAddresses _ addrs -> Just (sheetOf addressColumns addrs)
  ResponseBalances _ addrs -> Just (sheetOf balanceColumns addrs)
  ResponseCoins acc coins -> Just (sheetOf (coinColumns (accountNetwork acc)) coins)
  ResponseTxs acc txs -> Just (sheetOf (txColumns (accountNetwork acc)) txs)
  ResponsePendingTxs _ held -> Just (sheetOf pendingColumns held)
  _ -> Nothing

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

-- | The height of the block of a transaction in a cell, or an empty cell
-- for one in no block yet
blockCell :: Store.BlockRef -> Cell
blockCell = \case
  Store.BlockRef height _ -> countCell height
  Store.MemRef _ -> EmptyCell

{- Accounts -}

-- | The columns of the accounts, each with what it can spend now: what
-- the verbose view gives, but the key of the account and the fingerprint
-- of its seed. A key in a spreadsheet would let anyone it reaches see
-- every address and every payment of the account. A fingerprint has 8
-- hexadecimal digits, and about one in 40 has no letter: a spreadsheet
-- would read it as a number, and drop its leading zeros, without a word.
-- The verbose view and descriptor give it.
accountColumns :: [Column (DBAccount, Word64)]
accountColumns =
  [ Column "Account" $ \(acc, _) -> TextCell (dBAccountName acc),
    Column "Network" $ \(acc, _) -> TextCell (cs (accountNetwork acc).name),
    Column "Path" $ \(acc, _) -> TextCell (cs (pathToStr (accountPath acc))),
    Column "Spendable (sats)" $ \(_, spendable) -> satoshiCell spendable,
    Column "Confirmed (sats)" $ \(acc, _) -> satoshiCell (dBAccountBalanceConfirmed acc),
    Column "Unconfirmed (sats)" $ \(acc, _) -> satoshiCell (dBAccountBalanceUnconfirmed acc),
    Column "Coins" $ \(acc, _) -> countCell (dBAccountBalanceCoins acc),
    Column "Receiving addresses" $ \(acc, _) -> countCell (dBAccountExternal acc),
    Column "Change addresses" $ \(acc, _) -> countCell (dBAccountInternal acc),
    Column "Created (UTC)" $ \(acc, _) -> TimeCell (dBAccountCreated acc)
  ]

{- Addresses and balances -}

-- | The columns that name an address of the account: its path, the
-- address and its label. addresses and balances start with them.
addressNameColumns :: [Column DBAddress]
addressNameColumns =
  [ Column "Path" (pathCell . addrPath),
    Column "Address" (TextCell . dBAddressAddress),
    Column "Label" (textCell . dBAddressLabel)
  ]

-- | The columns of the addresses, each with what it can spend now: what
-- the verbose view gives
addressColumns :: [Column (DBAddress, Word64)]
addressColumns =
  through fst addressNameColumns
    <> [ Column "Spendable (sats)" $ \(_, spendable) -> satoshiCell spendable,
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
balanceColumns :: [Column (DBAddress, CoinBalance)]
balanceColumns =
  through fst addressNameColumns
    <> [ Column "Total (sats)" $ \(_, balance) -> satoshiCell (coinBalanceTotal balance),
         Column "Spendable (sats)" $ \(_, balance) -> satoshiCell (coinBalanceSpendable balance),
         Column "Locked by tx (sats)" $ \(_, balance) -> satoshiCell (coinBalanceLockedByTx balance),
         Column "Locked by you (sats)" $ \(_, balance) -> satoshiCell (coinBalanceLockedByUser balance),
         Column "Not spendable yet (sats)" $ \(_, balance) -> satoshiCell (coinBalanceNotSpendableYet balance),
         Column "Coins" $ \(addr, _) -> countCell (dBAddressBalanceCoins addr)
       ]

{- Coins -}

-- | The columns of the coins: the coin, as lockcoin and --coin take it,
-- the time of the transaction that paid it, its value, its state and
-- its confirmations, the address that holds it, and both labels
coinColumns :: Network -> [Column JsonCoin]
coinColumns net =
  [ Column "Coin" (TextCell . cs . coinText . jsonCoinOutpoint),
    Column "Time (UTC)" (timeCell . jsonCoinTime),
    Column "Value (sats)" (satoshiCell . jsonCoinValue),
    Column "State" (TextCell . cs . coinStateText "locked by a pending transaction"),
    Column "Confirmations" (countCell . jsonCoinConfirmations),
    Column "Block" (blockCell . jsonCoinBlock),
    Column "Address" (addressCell net . jsonCoinAddress),
    Column "Path" (maybe EmptyCell pathCell . jsonCoinPath),
    Column "Address label" (textCell . jsonCoinAddressLabel),
    Column "Transaction label" (textCell . jsonCoinTxLabel)
  ]

{- Transactions -}

-- | The columns of the transactions of the account, each with the
-- balance after it: when, what it adds or takes, where it stands, its
-- txid, and what it is for
txColumns :: Network -> [Column ListedTx]
txColumns net =
  [Column "Time (UTC)" (timeCell . txTime . listedTxInfo)]
    <> through held amountColumns
    <> [Column "Balance (sats)" (maybe EmptyCell SatoshiCell . listedTxBalance)]
    <> through
      held
      ( [ statusColumn,
          Column "Confirmations" $ \(_, info) -> case txInfoStatus info of
            OnChain status -> countCell (chainConfirmations status)
            Pending _ -> EmptyCell,
          Column "Block" $ \(_, info) -> case txInfoStatus info of
            OnChain status -> blockCell (chainBlock status)
            Pending _ -> EmptyCell,
          txidColumn
        ]
          <> purposeColumns
      )
  where
    held tx = (net, listedTxInfo tx)

-- | The columns of the pending transactions, each with its account: its
-- account, when the wallet started holding it, what it adds or takes, how
-- far it got, both its hashes, and what it is for
pendingColumns :: [Column (DBAccount, TxInfo)]
pendingColumns =
  [ Column "Account" $ \(acc, _) -> TextCell (dBAccountName acc),
    Column "Created (UTC)" $ \(_, info) -> timeCell (txTime info)
  ]
    <> through
      held
      ( amountColumns
          <> [ statusColumn,
               Column "Nosighash" $ \(_, info) -> case txInfoStatus info of
                 Pending status -> hashCell (pendingNosigHash status)
                 OnChain _ -> EmptyCell,
               txidColumn
             ]
          <> purposeColumns
      )
  where
    held (acc, info) = (accountNetwork acc, info)

-- | The columns of what a transaction adds to the account or takes from
-- it: its type, its amount, fee included, its fee and its fee rate. The
-- fee of a payment received is the fee its payer paid.
amountColumns :: [Column (Network, TxInfo)]
amountColumns =
  [ Column "Type" $ \(_, info) -> TextCell (cs (typeWord (summaryType (txInfoSummary info)))),
    Column "Amount (sats)" $ \(_, info) -> SatoshiCell (summaryAmount (txInfoSummary info)),
    Column "Fee (sats)" $ \(_, info) -> satoshiCell (summaryFee (txInfoSummary info)),
    Column "Fee rate (sat/vB)" $ \(_, info) -> countCell (summaryFeeByte (txInfoSummary info))
  ]

-- | The column of where a transaction stands ('statusText')
statusColumn :: Column (Network, TxInfo)
statusColumn = Column "Status" $ \(_, info) -> TextCell (statusText (txInfoStatus info))

-- | The column of the txid of a transaction, empty for one in
-- preparation not signed yet
txidColumn :: Column (Network, TxInfo)
txidColumn = Column "Txid" $ \(_, info) -> maybe EmptyCell hashCell (txInfoHash info)

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
-- wallet comes with its path and its label. The views say what is left
-- as "and 2 more": a count of the other addresses paid.
purposeColumns :: [Column (Network, TxInfo)]
purposeColumns =
  [ Column "Label" $ \(_, info) -> textCell (txInfoLabel info),
    Column "Address" $ \(net, info) -> maybe EmptyCell (addressCell net . txAddress . payeeRow) (payeeOf info),
    Column "Path" $ \(_, info) -> maybe EmptyCell (pathCell . fst) (ownPayee info),
    Column "Address label" $ \(_, info) -> maybe EmptyCell (textCell . snd) (ownPayee info),
    Column "Other addresses" $ \(_, info) -> countCell (maybe 0 payeeOthers (payeeOf info))
  ]
  where
    payeeOf = mainPayee . txInfoSummary
    -- The path and the label of the address paid, when it is the wallet's
    ownPayee info = txAddressOwn . payeeRow =<< payeeOf info

{- The spreadsheet of the wallet -}

-- | The sheets of the listings of one account that exportsheet writes: its
-- addresses, of both chains, the balances of those that hold coins, its
-- coins, locked or not, and its transactions
data AccountSheets = AccountSheets
  { addressesSheet :: !Sheet,
    balancesSheet :: !Sheet,
    coinsSheet :: !Sheet,
    txsSheet :: !Sheet
  }
  deriving (Eq, Show)

-- | The sheets of the spreadsheet of exportsheet, each with the name of
-- its tab, in the order of the help: the accounts, the addresses, the
-- balances, the coins and the transactions of each account given, and
-- the pending transactions. A sheet of one account gets a first column
-- that names it ('withAccountColumn'), as the accounts and the pending
-- transactions name theirs, and the rows of the accounts follow each
-- other ('stacked'). One sheet for all of them, rather than a tab for each
-- account, sorts and sums across the wallet. Amounts of Bitcoin and of
-- Bitcoin Cash then share a column: the account tells them apart.
walletSheets :: Sheet -> [(DBAccount, AccountSheets)] -> Sheet -> [(Text, Sheet)]
walletSheets accounts held pending =
  [ ("Accounts", accounts),
    ("Addresses", ofEach addressesSheet),
    ("Balances", ofEach balancesSheet),
    ("Coins", ofEach coinsSheet),
    ("Transactions", ofEach txsSheet),
    ("Pending transactions", pending)
  ]
  where
    ofEach part = stacked [withAccountColumn acc (part sheets) | (acc, sheets) <- held]

-- | A sheet with a first column, Account, that names the account given on
-- every row
withAccountColumn :: DBAccount -> Sheet -> Sheet
withAccountColumn acc sheet =
  Sheet
    { sheetHeadings = "Account" : sheetHeadings sheet,
      sheetRows = (TextCell (dBAccountName acc) :) <$> sheetRows sheet
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
