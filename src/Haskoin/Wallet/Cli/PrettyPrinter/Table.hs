{-# LANGUAGE OverloadedStrings #-}

-- | The tables that --tabular shows: a column, a row, the header, and the
-- cells that several tables share. Each table has its columns and its rows
-- in the module of what it lists.
module Haskoin.Wallet.Cli.PrettyPrinter.Table
  ( Side (..),
    Column (..),
    column,
    amountHeading,
    amountHeadingOf,
    tableHeader,
    tableLine,
    widestCell,
    shortIdWidth,
    confsCell,
    timeCell,
    tagsColumn,
    tagsCell,
    addressWidth,
  )
where

import Data.List (intercalate)
import Data.String.Conversions (cs)
import Data.Time (ZonedTime)
import Haskoin
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.PrettyPrinter.Base (cappedCount, minuteText, shownWidth, styled, tagWord, text)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.Tags (ShownTag (..))
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, hcat, hsep, pretty)

{- Tables -}

-- accounts, txs, addresses, coins, balances and pendingtxs show each
-- element on one row with --tabular, in columns, under a header that names
-- them. The screen reads a listing in
-- chunks of rows (see "Haskoin.Wallet.Cli.Listing"), and only the first
-- chunk has the header. A column therefore cannot take the width of its
-- widest cell: a later chunk would not line up with the first one. Each
-- column has a width fixed in advance instead, wide enough for nearly
-- every cell. A cell wider than its column pushes the rest of its row.
--
-- A label has no width to fix in advance. It goes in the last column,
-- which ends the row and pushes nothing. Nothing in a table is cut: a
-- label, an address and the name of an account show whole, however long.
-- On a terminal, less keeps each row on one line and scrolls it sideways
-- ("Haskoin.Wallet.Cli.Pager"), so a long cell makes its row wider
-- without breaking the columns. A cut would save room on the screen, but
-- hide what tells two labels apart, and an address read in part is how a
-- lookalike passes for the right one.
--
-- A table has no frame, only lines between the columns and one under the
-- header. A frame needs a line under the last row, and the screen does not
-- know which chunk is the last before it shows it.

-- | Which side of its column a cell keeps to: words to the left, numbers
-- to the right, so that their points line up
data Side = LeftSide | RightSide

-- | A column of a table: its heading, its width, and the side its cells
-- keep to. Make one with 'column', which fits the heading in.
data Column = Column
  { columnHeading :: !String,
    columnWidth :: !Natural,
    columnSide :: !Side
  }

-- | A column with its heading, as wide as its cells need or as its
-- heading, whichever is wider. The last column of a table is given no
-- width of its own: its cells run past its heading.
column :: String -> Natural -> Side -> Column
column heading width = Column heading (max width (fromIntegral (shownWidth heading)))

-- | The heading of a column of amounts, which names their unit, so that
-- the cells hold numbers alone: "Amount (btc)", "Amount (sats)". The unit
-- is in the plural, as an amount of 2 shows it.
amountHeading :: String -> AmountUnit -> String
amountHeading name unit = amountHeadingOf name [unit]

-- | The heading of a column of amounts of several networks, which names
-- the unit of each ('unitsOn'): "Total (btc\/bch)" when the rows hold
-- whole coins of Bitcoin and of Bitcoin Cash, and "Total (sats)" in
-- satoshis, the same on both. The column of the networks tells the unit
-- of a row.
amountHeadingOf :: String -> [AmountUnit] -> String
amountHeadingOf name units = name <> " (" <> intercalate "/" [cs (showUnit unit 2) | unit <- units] <> ")"

-- | The header of a table: the heading of each column, then a line under
-- them
tableHeader :: [Column] -> [Doc Style]
tableHeader headed =
  [ tableLine [(headedColumn, styled Field (columnHeading headedColumn)) | headedColumn <- headed],
    tableRule headed
  ]

-- | A row of a table: each cell on its side of its column, padded to the
-- width of the column, and a faint line between two cells. The last cell
-- gets no padding when it keeps to the left, and an empty one no space
-- before it either: a line ends without spaces.
tableLine :: [(Column, Doc Style)] -> Doc Style
tableLine cells = hcat (zipWith cellAt [1 ..] cells)
  where
    cellAt :: Int -> (Column, Doc Style) -> Doc Style
    cellAt i (cellColumn, doc)
      | i == 1 = padded i cellColumn doc
      | i == length cells && null (renderPlain doc) = " " <> bar
      | otherwise = " " <> bar <> " " <> padded i cellColumn doc
    bar = styled Muted "│"
    padded i cellColumn doc =
      let missing = pretty (replicate (fromIntegral (columnWidth cellColumn) - shownWidth (renderPlain doc)) ' ')
       in case columnSide cellColumn of
            LeftSide
              | i == length cells -> doc
              | otherwise -> doc <> missing
            RightSide -> missing <> doc

-- | The line under the header of a table: a dash under each character of
-- the header, and a cross under each line between two columns. A column
-- has a space on each side of its cells, except on the left of the first
-- one and on the right of the last one.
tableRule :: [Column] -> Doc Style
tableRule headed = styled Muted (intercalate "┼" (zipWith dashes [1 ..] headed))
  where
    dashes :: Int -> Column -> String
    dashes i ruledColumn = replicate (fromIntegral (columnWidth ruledColumn) + spaceBefore i + spaceAfter i) '─'
    spaceBefore i = if i > 1 then 1 else 0
    spaceAfter i = if i < length headed then 1 else 0

-- | The width of the widest cell of a column, which a listing read whole
-- can size its columns to
widestCell :: (a -> String) -> [a] -> Natural
widestCell cell elements = fromIntegral (maximum (0 : [shownWidth (cell e) | e <- elements]))

{- The cells that several tables share -}

-- | The width of a column of short ids: 8 digits
shortIdWidth :: Natural
shortIdWidth = 8

-- | A number of confirmations in a column of a table, up to "999+". None
-- is 0, in the colour of what waits.
confsCell :: Natural -> Doc Style
confsCell 0 = styled Waiting "0"
confsCell n = text (cappedCount n)

-- | The time of a transaction in a column of a table, to the minute
timeCell :: Maybe ZonedTime -> Doc Style
timeCell = maybe (styled Muted "time unknown") (text . minuteText)

-- | The column of the tags, in every table that has one, as wide as
-- given: the widest set of tags that a row of its kind has in the
-- database (the type t'Haskoin.Wallet.Database.Tag.TagWidths'), so that
-- no row runs past it and the label after it stays in line. It comes
-- right before the last column, which holds a label or a description, a
-- text of any length that a table does not cut.
tagsColumn :: Natural -> Column
tagsColumn tagsWide = column "Tags" tagsWide LeftSide

-- | Tags in a cell of a table, or after the name of a field: a space
-- between them, each in its colour. The column or the field says they are
-- tags, so they need no mark.
tagsCell :: [ShownTag] -> Doc Style
tagsCell = hsep . map tagWord

-- | The width of the widest address the account can have. Its addresses
-- are all of its kind, and bech32 and cashaddr write every address of a
-- kind with as many characters. Base58, which writes the P2PKH addresses
-- of Bitcoin, writes a hash shorter when its first bits are zero. A hash of
-- all ones takes the most characters, so its address is the widest.
addressWidth :: Network -> [AccountKind] -> Natural
addressWidth net kinds = fromIntegral (maximum (0 : [length (addrToString net (widest kind)) | kind <- kinds]))
  where
    widest kind = case kind of
      Bip44 -> PubKeyAddress allOnes
      Bip84 -> WitnessPubKeyAddress allOnes
    allOnes = "ffffffffffffffffffffffffffffffffffffffff"
