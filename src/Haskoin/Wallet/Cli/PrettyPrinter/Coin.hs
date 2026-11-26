{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about coins: a coin in its compact view, its verbose view
-- and its table.
module Haskoin.Wallet.Cli.PrettyPrinter.Coin
  ( listedCoinDoc,
    compactCoinDoc,
    verboseCoinDoc,
    coinAnswerDoc,
    coinTableColumns,
    coinRowDoc,
  )
where

import Data.Maybe (isNothing)
import qualified Data.Text as Text
import Data.Time (ZonedTime, utcToLocalZonedTime)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.PrettyPrinter.AddressWords (fullAddressWords, ownAddressNote, ownAddressNoteBelow, ownAddressWord, trailingPathWord, withNotesBelow)
import Haskoin.Wallet.Cli.PrettyPrinter.Balance (heldAmountCell)
import Haskoin.Wallet.Cli.PrettyPrinter.Base (amountCell, amountWidth, block, cappedConfirmations, columns, commaList, field, fullConfirmations, labelWord, minuteWidth, minuteWord, secondText, styled, tagMarks, text)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (PathNaming (..), SeenFrom (..), accountRefField, ownPathsAlone, ownPathText, pathsSeenFrom)
import Haskoin.Wallet.Cli.PrettyPrinter.Table (Column (..), Side (..), addressWidth, amountHeading, column, confsCell, tableLine, tagsCell, tagsColumn, timeCell)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.ShortId (shortCoinText)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, hsep, indent, vcat, (<+>))

{- Coins -}

-- A coin shows in one of three views. The compact and the verbose views
-- start with the same first line: the time of the transaction that paid
-- it, its value, its state and its confirmations, as a transaction starts
-- with its time. Only the count of the confirmations differs: the compact
-- view stops it at 999+, and the verbose view gives it in full. The table
-- gives a coin one row ('coinRowDoc').

-- | A coin as coins lists it, in the view asked for, with the time of
-- its transaction in the time zone of the computer. coins lists the
-- active portfolio, so the paths are alone ('ownPathsAlone'). lockcoin and
-- unlockcoin answer with the verbose view, as the commands that answer
-- with one element do ('coinAnswerDoc').
listedCoinDoc :: View -> Network -> AmountUnit -> JsonCoin -> IO (Doc Style)
listedCoinDoc view net unit coin = do
  time <- traverse utcToLocalZonedTime (jsonCoinTime coin)
  return $ case view of
    Compact -> compactCoinDoc ownPathsAlone net unit time coin
    Verbose -> verboseCoinDoc ownPathsAlone net unit time coin
    -- The table needs the account for the width of its paths: rowsPrinter
    -- lays it out ('coinRowDoc')
    Table -> compactCoinDoc ownPathsAlone net unit time coin

-- | The first line of a coin in the verbose view: the time of the
-- transaction that paid it, its value, then its state and its
-- confirmations, written by the function given, in the words of
-- @coinLineCells@
coinLine :: (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
coinLine countText unit time coin = columns (coinLineCells FullLockWords countText unit time coin)

-- | What the first line of a coin gives: the time of the transaction that
-- paid it, its value, then its state, in the words asked for a coin a
-- pending transaction holds, and its confirmations, written by the
-- function given. The time and the value keep the columns of the first
-- line of a transaction, so that the values of a listing line up.
coinLineCells :: LockWords -> (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> [Doc Style]
coinLineCells wording countText unit time coin =
  [ minuteWord time,
    amountCell (Just Incoming) "" unit (fromIntegral (jsonCoinValue coin)),
    commaList [coinState wording coin, confirmations]
  ]
  where
    confirmations
      | jsonCoinConfirmations coin == 0 = styled Waiting "unconfirmed"
      | otherwise = text (countText (jsonCoinConfirmations coin))

-- | Whether a coin can be spent now, in the colour of its state
-- ('coinStateText'): green when it is spendable, yellow when it waits
coinState :: LockWords -> JsonCoin -> Doc Style
coinState wording coin = styled style (coinStateText wording coin)
  where
    style = if isNothing (jsonCoinLock coin) && jsonCoinSpendable coin then Good else Waiting

-- | The address that holds a coin, as the verbose view gives it: in full,
-- then its path, with its label, or the mark of a change address, on the
-- line below, as every view that gives an address in full does. The
-- compact view keeps them on the line of the address instead
-- ('compactCoinDoc').
coinAddressDoc :: PathNaming -> Network -> JsonCoin -> Doc Style
coinAddressDoc paths net coin =
  withNotesBelow
    (fullAddressWords paths net (jsonCoinAddress coin) (jsonCoinPath coin))
    (ownAddressNoteBelow (jsonCoinAddressLabel coin) (jsonCoinChange coin) <> [hsep (tagMarks tags) | let tags = jsonCoinAddressTags coin, not (null tags)])

-- | A coin in two lines, as the compact view of a transaction is laid
-- out. The first gives the short id of the coin, which lockcoin and
-- --coin take ("Haskoin.Wallet.ShortId"), first, as in the table, then
-- the time of the transaction that paid it, its value, its state and its
-- confirmations, counted up to 999+. A coin a pending transaction holds
-- is "locked by tx", in the words of the table, so that the line keeps to
-- 80 columns. The second line gives the address that holds the coin in
-- full, then its path, then its label, or the mark of a change address. It
-- is the one compact view that gives an address of the wallet in full:
-- what tells a coin apart is the address that holds it, which another
-- wallet or a block explorer shows, more than its path. The label of the
-- address stays, since a user may sort coins by the labels of their
-- addresses. It stays on the line of the address too, so that every coin
-- takes two lines: the one exception to the rule that puts the label of
-- an address in full on the line below it ('withNotesBelow'). A label
-- ends the line, so a long one pushes nothing, and it is whole, as on a
-- row of a table. The label of the transaction that paid the coin says
-- less of the coin, and only the verbose view gives it. The path is
-- written as the paths of the view are (the type t'PathNaming'): it names its
-- portfolio in a report on the coins of another portfolio than the active
-- one.
compactCoinDoc :: PathNaming -> Network -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
compactCoinDoc paths net unit time coin =
  vcat
    [ columns (shortCoinWord coin : coinLineCells FewLockWords cappedConfirmations unit time coin),
      indent 2 ("at" <+> columns (fullAddressWords paths net (jsonCoinAddress coin) (jsonCoinPath coin) : ownAddressNote (jsonCoinAddressLabel coin) (jsonCoinChange coin) <> addressTagMarks))
    ]
  where
    -- The tags of the address, after its label: a coin is in the groups
    -- of its address
    addressTagMarks = [hsep (tagMarks tags) | let tags = jsonCoinAddressTags coin, not (null tags)]

-- | The short id of a coin at the start of a line, faint, padded to the
-- width of a coin at one of the first ten outputs of its transaction,
-- a1075db5:1. A coin further down pushes the rest of its line.
shortCoinWord :: JsonCoin -> Doc Style
shortCoinWord coin = styled Muted (block shortCoinWidth (shortCoinText (jsonCoinOutpoint coin)))

-- | The width of the short id of a coin at one of the first ten outputs of
-- its transaction: 8 digits, a colon and one digit
shortCoinWidth :: Natural
shortCoinWidth = 10

-- | A coin with every detail: its first line, with the full count of its
-- confirmations, then its fields. The label of its transaction comes
-- first, as in the verbose view of a transaction, then the coin in full
-- and its short id, the block that holds it, the time of its transaction to the second with its
-- time zone, and its address in full with its path. The label of the
-- address, or the mark of a change address, is on the line below the
-- address, as in the verbose view of a transaction. An unconfirmed coin
-- is in no block yet, and shows none. The path is written as the paths of
-- the view are (the type t'PathNaming').
verboseCoinDoc :: PathNaming -> Network -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
verboseCoinDoc paths net unit time coin =
  vcat $
    coinLine fullConfirmations unit time coin
      : ( indent 2
            <$> [field 8 "Label" <> labelWord label | let label = jsonCoinTxLabel coin, not (Text.null label)]
              <> [ field 8 "Coin" <> text (coinText (jsonCoinOutpoint coin)),
                   field 8 "Short id" <> text (shortCoinText (jsonCoinOutpoint coin))
                 ]
              <> [field 8 "Block" <> text (show height) | Store.BlockRef height _ <- [jsonCoinBlock coin]]
              <> [field 8 "Time" <> text (secondText t) | Just t <- [time]]
              <> [field 8 "Address" <> coinAddressDoc paths net coin]
        )

-- | A coin as an answer shows it: with every detail, as coins --verbose
-- does, then the account that holds it, as the answer of an address ends.
-- The account says when it is not of the active portfolio given
-- ('Haskoin.Wallet.Cli.PrettyPrinter.Place.seenPortfolioDoc'), and the
-- path of the address of the coin then names its portfolio (the type
-- t'PathNaming'). Each portfolio is named with its wallet or not, as
-- given. The time is in the time zone of the computer.
coinAnswerDoc :: WalletNaming -> AmountUnit -> Maybe PortfolioRef -> AccountRef -> JsonCoin -> IO (Doc Style)
coinAnswerDoc naming unit activeM ref coin = do
  time <- traverse utcToLocalZonedTime (jsonCoinTime coin)
  let paths = pathsSeenFrom naming (SeenFrom (placePortfolio (accountRefPlace ref)) activeM)
  return $ vcat [verboseCoinDoc paths (accountRefNetwork ref) unit time coin, indent 2 (accountRefField naming 8 activeM ref)]

{- The table of coins -}

-- The table of coins gives what the compact view gives: the short id of
-- the coin first, the time of the transaction that paid it, its value,
-- its state, its confirmations, the address that holds it in full with
-- its path, and the label of the address. The coin in full, TXID:INDEX,
-- takes 66 characters at least, which would make a row too wide. The
-- verbose view gives it.

-- | The columns of the table of coins. The address takes the width of
-- the widest address the account can have ('addressWidth'), and the path
-- the width given, that of the longest path of the account on its two
-- chains, since coins lists the coins of both.
coinTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> Natural -> [Column]
coinTableColumns unit net kinds pad tagsWide =
  [ column "Coin" coinIdWidth LeftSide,
    column "Time" minuteWidth LeftSide,
    column (amountHeading "Value" unit) amountWidth RightSide,
    column "State" coinStateWidth LeftSide,
    column "Confs" 4 RightSide,
    column "Address" (addressWidth net kinds) LeftSide,
    column "Path" pad LeftSide,
    tagsColumn tagsWide,
    column "Label" 0 LeftSide
  ]

-- | The width of the column of the short ids of coins: 8 digits, a colon
-- and an index of up to three digits. A coin past the thousandth output of
-- its transaction pushes the rest of its row.
coinIdWidth :: Natural
coinIdWidth = 12

-- | The width of the column of the states of coins: that of "not
-- spendable yet", the longest
coinStateWidth :: Natural
coinStateWidth = 17

-- | A coin on one row of its table, under the columns given: its short
-- id, faint, the time of the transaction that paid it, its value in
-- green, its state, with "locked by tx" for a coin a pending transaction
-- holds, its confirmations, the address in the colour of the wallet's
-- addresses, its path, faint, then the label of the address in that
-- colour, or that it is a change address. The address and its label share a colour, as in
-- the compact view.
coinRowDoc :: Network -> AmountUnit -> [Column] -> Maybe ZonedTime -> JsonCoin -> Doc Style
coinRowDoc net unit headed time coin =
  tableLine $
    zip
      headed
      [ styled Muted (shortCoinText (jsonCoinOutpoint coin)),
        timeCell time,
        heldAmountCell unit (jsonCoinValue coin),
        coinState FewLockWords coin,
        confsCell (jsonCoinConfirmations coin),
        ownAddressWord net (jsonCoinAddress coin),
        -- coins lists the active portfolio
        maybe mempty (trailingPathWord . ownPathText ownPathsAlone) (jsonCoinPath coin),
        tagsCell (jsonCoinAddressTags coin),
        -- The label has a column of its own
        hsep (ownAddressNote (jsonCoinAddressLabel coin) (jsonCoinChange coin))
      ]
