{-# LANGUAGE OverloadedStrings #-}

-- | The style of the reports of hw, and the layout that puts a report on
-- its pages. Every report of the command report is written through this
-- module, so they all look the same: the same title block, the same
-- tables, the same footer.
--
-- = What a report is made of
--
-- A report is a title and a list of blocks, from top to bottom: a
-- section heading, a list of facts, a table, a list of notes. The types
-- are t'Report' and t'Block'. A module that makes a report says what it
-- holds, in these blocks, and nothing of where each line goes
-- ("Haskoin.Wallet.Report.BalanceStatement"). This module places the blocks
-- on pages ('reportPages'), and "Haskoin.Wallet.Report.Pdf" writes the
-- pages to a file.
--
-- The split keeps the style in one place. A new report gets it for free,
-- and a change of the style changes every report at once.
--
-- = The style
--
-- A report goes to a tax office or to an accountant. It has to read as a
-- statement of a bank or of an accounting firm does: plain, exact, and
-- easy to check.
--
-- * One family of type in two weights, in black and in one dark blue.
--   Colour says what is a title, and nothing else.
--
-- * Numbers stand in columns, aligned on the right. Every amount has all
--   its decimals, so the decimal points line up down a column.
--
-- * A total has a rule above it and a double rule below it, as on a
--   balance sheet.
--
-- * Every page says which report it is of, and its number among how
--   many, so that a page that went missing is noticed.
--
-- * The report names no program and has no logo. It is the statement of
--   its owner, not an advertisement.
--
-- = Pages
--
-- The layout goes down the page and starts a new one when the next
-- thing does not fit. A table that goes on to the next page starts
-- there with its column titles again. A heading is never the last thing
-- on a page: it moves to the next one with what it is the heading of.
-- A row of a table and a note are never cut in two.
--
-- A text that is wider than its place goes on to the next line, at a
-- space where there is one ('wrapText'). Nothing is cut short: a label
-- reads whole, on as many lines as it needs.
module Haskoin.Wallet.Report.Layout
  ( Report (..),
    Block (..),
    Fact (..),
    Table (..),
    Column (..),
    Align (..),
    Row (..),
    RowKind (..),
    Cell (..),
    plainCell,
    remarkCell,
    wideRemarkCell,
    reportPages,
    reportPdf,
    wrapText,
    contentLeft,
    contentRight,
  )
where

import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (ZonedTime)
import Haskoin.Wallet.Report.Pdf

{- What a report holds -}

-- | A report: what its first page says at the top, what every page says
-- at the bottom, and its blocks from the first to the last
data Report = Report
  { -- | The kind of document, in small capitals above the title, such as
    -- "Statement of holdings"
    reportKind :: !Text,
    -- | The title, in large type
    reportTitle :: !Text,
    -- | The line below the title, such as the date the report is for
    reportSubtitle :: !Text,
    -- | What the foot of every page says on the left, so that a page on
    -- its own still tells which report it is of
    reportFooter :: !Text,
    reportBlocks :: ![Block]
  }
  deriving (Eq, Show)

-- | A part of a report. The blocks follow each other down the pages.
data Block
  = -- | The heading of a section of the report
    SectionBlock !Text
  | -- | The heading of a group inside a section, with a remark beside it
    -- in lighter type, or none: the name of a portfolio, and its
    -- description
    GroupBlock !Text !Text
  | -- | Facts about the report, a name and its value each
    FactsBlock ![Fact]
  | TableBlock !Table
  | -- | Notes, numbered from 1 in the order given
    NotesBlock ![Text]
  deriving (Eq, Show)

-- | A fact: its name, and its value on one line or on several
data Fact = Fact
  { factName :: !Text,
    factLines :: ![Text]
  }
  deriving (Eq, Show)

-- | A table: its columns, and its rows, each with a cell for each
-- column
data Table = Table
  { tableColumns :: ![Column],
    tableRows :: ![Row]
  }
  deriving (Eq, Show)

-- | A column of a table: its title, its share of the width of the table
-- among the shares of all the columns, and the side its cells are
-- aligned on. Amounts and counts are aligned on the right.
data Column = Column
  { columnTitle :: !Text,
    columnShare :: !Double,
    columnAlign :: !Align
  }
  deriving (Eq, Show)

-- | The side the cells of a column are aligned on
data Align = AlignLeft | AlignRight
  deriving (Eq, Show)

-- | A row of a table. A total is in bold, with a rule above it and a
-- double rule below it.
data Row = Row
  { rowKind :: !RowKind,
    rowCells :: ![Cell]
  }
  deriving (Eq, Show)

-- | What a row of a table is: one of its rows, or the total of the rows
-- above it
data RowKind = BodyRow | TotalRow
  deriving (Eq, Show)

-- | A cell of a table: its text, and a remark below it in smaller,
-- lighter type, or none. The remark is where the label of an address
-- goes: it may be long, and takes as many lines as it needs under the
-- address.
data Cell = Cell
  { cellText :: !Text,
    cellRemark :: !Text,
    -- | Whether the remark may run on below the cells to the right of
    -- this one, as far as the right edge of the table
    -- ('wideRemarkCell')
    cellRemarkRunsOn :: !Bool
  }
  deriving (Eq, Show)

-- | A cell with its text alone
plainCell :: Text -> Cell
plainCell text = Cell text "" False

-- | A cell with its text and a remark below it, which stays in the
-- column of the cell
remarkCell :: Text -> Text -> Cell
remarkCell text remark = Cell text remark False

-- | A cell with its text and a remark below it that runs on below the
-- cells to its right, as far as the right edge of the table. It is for
-- a remark that is wider than any column and must still read whole on
-- one line, such as the identifier of a transaction below its
-- description.
--
-- The remark starts at the left edge of its cell, whatever side the
-- column is aligned on. The cells to its right must have no remark of
-- their own, or the two would be written one over the other.
wideRemarkCell :: Text -> Text -> Cell
wideRemarkCell text remark = Cell text remark True

{- The page -}

-- The measures below are in points, 72 to the inch, from the top left
-- corner of the page.

-- | The paper of the reports
paper :: Paper
paper = a4

-- | The left and right edges of what a page holds: margins of 20
-- millimetres
contentLeft, contentRight :: Double
contentLeft = 56.7
contentRight = paperWidth paper - 56.7

contentWidth :: Double
contentWidth = contentRight - contentLeft

-- | Where the blocks start on the first page, below the title block,
-- and on every other page
firstPageTop, otherPageTop :: Double
firstPageTop = 150
otherPageTop = 66

-- | The lowest a block may reach, above the footer
contentBottom :: Double
contentBottom = paperHeight paper - 74

{- The colours and the type -}

-- | The dark blue of the titles and of the heavy rules
navy :: Colour
navy = Colour 0.09 0.16 0.30

-- | The near black of the text
ink :: Colour
ink = Colour 0.11 0.12 0.14

-- | The grey of what comes second: column titles, remarks, the footer
grey :: Colour
grey = Colour 0.40 0.43 0.47

-- | The light grey of the rules between the rows of a table
hairline :: Colour
hairline = Colour 0.83 0.85 0.88

-- | The sizes of the type
bodySize, remarkSize, columnTitleSize, noteSize, footerSize :: Double
bodySize = 8.5
remarkSize = 7.5
columnTitleSize = 6.8
noteSize = 8
footerSize = 7.5

-- | A line of running text, with no spacing between its characters
textAt :: Double -> Double -> Font -> Double -> Colour -> Text -> Mark
textAt x baseline font size colour = TextMark x baseline font size colour 0

-- | A line of text that ends at a place: its left end follows from its
-- width
textEndingAt :: Double -> Double -> Font -> Double -> Colour -> Text -> Mark
textEndingAt right baseline font size colour text =
  textAt (right - textWidth font size 0 text) baseline font size colour text

{- Wrapping -}

-- | A text on as many lines as it needs to fit a width, in a font at a
-- size. A line ends at a space where it can. A word wider than the
-- width, such as a long address, goes on to the next line where it
-- stops fitting. A text with no word gives no line.
wrapText :: Font -> Double -> Double -> Text -> [Text]
wrapText font size width = reverse . finish . foldl' place ([], "") . concatMap pieces . Text.words
  where
    fits text = textWidth font size 0 text <= width
    -- A word that fits is one piece. A wider one is cut where it stops
    -- fitting, as often as it takes.
    pieces word
      | fits word = [word]
      | otherwise =
          let fitting = max 1 (length (takeWhile fits (drop 1 (Text.inits word))))
              (start, rest) = Text.splitAt fitting word
           in start : pieces rest
    place (done, line) word
      | Text.null line = (done, word)
      | fits (line <> " " <> word) = (done, line <> " " <> word)
      | otherwise = (line : done, word)
    finish (done, line)
      | Text.null line = done
      | otherwise = line : done

{- The layout -}

-- | Where the layout is: the pages that are full, the newest first, the
-- marks of the page it is on, the newest first, and how far down that
-- page it has come
data Layout = Layout
  { layoutDone :: ![[Mark]],
    layoutPage :: ![Mark],
    layoutY :: !Double
  }

-- | Draw marks on the page the layout is on
draw :: [Mark] -> Layout -> Layout
draw marks layout = layout {layoutPage = reverse marks <> layoutPage layout}

-- | Move down the page
moveDown :: Double -> Layout -> Layout
moveDown height layout = layout {layoutY = layoutY layout + height}

-- | Start a new page
newPage :: Layout -> Layout
newPage layout = Layout (reverse (layoutPage layout) : layoutDone layout) [] otherPageTop

-- | Whether the layout is at the top of a page, where a block needs no
-- room above it
atPageTop :: Layout -> Bool
atPageTop layout = null (layoutPage layout)

-- | Start a new page unless the height given still fits on this one
ensureRoom :: Double -> Layout -> Layout
ensureRoom height layout
  | layoutY layout + height <= contentBottom = layout
  | atPageTop layout = layout
  | otherwise = newPage layout

-- | Leave room above a block, unless it starts a page
gapAbove :: Double -> Layout -> Layout
gapAbove height layout
  | atPageTop layout = layout
  | otherwise = moveDown height layout

-- | The pages of a report, each as its marks, with the title block on
-- the first one and the footer on each
reportPages :: Report -> [[Mark]]
reportPages report = zipWith framed [1 ..] bodies
  where
    -- The marks of the page being laid out are kept the newest first
    start = Layout [] (reverse (titleBlock report)) firstPageTop
    end = foldl' (flip placeBlock) start (reportBlocks report)
    bodies = reverse (reverse (layoutPage end) : layoutDone end)
    framed number body = pageTop number <> body <> footer report number (length bodies)
    -- The first page has the rule of its title block. The others start
    -- with a thinner one.
    pageTop :: Int -> [Mark]
    pageTop 1 = []
    pageTop _ = [BoxMark contentLeft 42 contentWidth 1 navy]

-- | The report as the bytes of its PDF file, made at the time given
reportPdf :: ZonedTime -> Report -> BS.ByteString
reportPdf created report =
  pdfDocument (DocumentInfo (reportTitle report <> ", " <> reportSubtitle report) created) paper (reportPages report)

-- | The top of the first page: a heavy rule, the kind of document in
-- small capitals, the title and the line below it
titleBlock :: Report -> [Mark]
titleBlock report =
  [ BoxMark contentLeft 42 contentWidth 3 navy,
    TextMark contentLeft 70 Bold 7.5 grey 1.2 (Text.toUpper (reportKind report)),
    textAt contentLeft 99 Bold 25 navy (reportTitle report),
    textAt contentLeft 120 Regular 12 ink (reportSubtitle report)
  ]

-- | The foot of a page: a thin rule, the report on the left, and the
-- number of the page among how many on the right
footer :: Report -> Int -> Int -> [Mark]
footer report number count =
  [ RuleMark contentLeft contentRight (paperHeight paper - 56) 0.5 hairline,
    textAt contentLeft baseline Regular footerSize grey (reportFooter report),
    textEndingAt contentRight baseline Regular footerSize grey ("Page " <> shown number <> " of " <> shown count)
  ]
  where
    baseline = paperHeight paper - 43
    shown = Text.pack . show

-- | Put a block on the pages
placeBlock :: Block -> Layout -> Layout
placeBlock block = case block of
  SectionBlock title -> placeSection title
  GroupBlock title remark -> placeGroup title remark
  FactsBlock facts -> placeFacts facts
  TableBlock table -> placeTable table
  NotesBlock notes -> placeNotes notes

-- | A section heading. It asks for the room of a short table below it,
-- so that it never stands alone at the foot of a page.
placeSection :: Text -> Layout -> Layout
placeSection title = heading . gapAbove 26 . ensureRoom 120
  where
    heading layout =
      let top = layoutY layout
       in moveDown 26 (draw [textAt contentLeft (top + 12) Bold 12.5 navy title] layout)

-- | The heading of a group, with its remark beside it. A remark too long
-- for the line goes below, on as many lines as it needs.
placeGroup :: Text -> Text -> Layout -> Layout
placeGroup title remark = heading . gapAbove 12 . ensureRoom 80
  where
    titleWidth = textWidth Bold 9.5 0 title
    besideLeft = contentLeft + titleWidth + 10
    fitsBeside = besideLeft + textWidth Regular bodySize 0 remark <= contentRight
    heading layout
      | Text.null remark = moveDown 20 (draw [titleMark] layout)
      | fitsBeside = moveDown 20 (draw [titleMark, textAt besideLeft baseline Regular bodySize grey remark] layout)
      | otherwise =
          let below = wrapText Regular bodySize contentWidth remark
              marks = [textAt contentLeft (baseline + 12 * line) Regular bodySize grey text | (line, text) <- zip [1 ..] below]
           in moveDown (20 + 12 * fromIntegral (length below)) (draw (titleMark : marks) layout)
      where
        baseline = layoutY layout + 11
        titleMark = textAt contentLeft baseline Bold 9.5 ink title

-- | The facts, each on its line: the name in grey, then the value. A
-- value of several lines keeps its name on the first one.
placeFacts :: [Fact] -> Layout -> Layout
placeFacts facts start = moveDown 6 (foldl' (flip placeFact) start facts)
  where
    valueLeft = contentLeft + 118
    leading = 13.5
    placeFact fact layout =
      let valueLines = concatMap (orBlank . wrapText Regular bodySize (contentRight - valueLeft)) (factLines fact)
          height = leading * fromIntegral (max 1 (length valueLines))
          room = ensureRoom height layout
          baseline = layoutY room + 10
          marks =
            textAt contentLeft baseline Regular bodySize grey (factName fact)
              : [textAt valueLeft (baseline + leading * line) Regular bodySize ink text | (line, text) <- zip [0 ..] valueLines]
       in moveDown height (draw marks room)
    orBlank [] = [""]
    orBlank textLines = textLines

-- | A column of a table as the layout places it: where it starts and
-- ends, with a gutter kept clear on the side its neighbour is
data Placed = Placed
  { placedColumn :: !Column,
    placedLeft :: !Double,
    placedRight :: !Double
  }

-- | The room kept clear between two columns
gutter :: Double
gutter = 12

-- | The columns of a table across the width of the page, each with its
-- share
placedColumns :: [Column] -> [Placed]
placedColumns columns = zipWith3 Placed columns lefts (drop 1 lefts)
  where
    total = sum (columnShare <$> columns)
    widths = [contentWidth * columnShare column / total | column <- columns]
    lefts = scanl (+) contentLeft widths

-- | Where the text of a column goes: from its left edge for a column
-- aligned on the left, up to its right edge for one aligned on the
-- right, with the gutter kept clear on the other side
textLeft, textRight :: Placed -> Double
textLeft placed = case columnAlign (placedColumn placed) of
  AlignLeft -> placedLeft placed
  AlignRight -> placedLeft placed + gutter
textRight placed = case columnAlign (placedColumn placed) of
  AlignLeft -> placedRight placed - gutter
  AlignRight -> placedRight placed

-- | A line of a cell in its column, on the side the column is aligned on
cellLine :: Placed -> Double -> Font -> Double -> Colour -> Text -> Mark
cellLine placed baseline font size colour text = case columnAlign (placedColumn placed) of
  AlignLeft -> textAt (textLeft placed) baseline font size colour text
  AlignRight -> textEndingAt (textRight placed) baseline font size colour text

-- | The titles of the columns, in small grey capitals, with a rule below
-- them. They come at the top of a table, and again on each page the
-- table goes on to.
placeColumnTitles :: [Placed] -> Layout -> Layout
placeColumnTitles columns layout = moveDown 18 (draw (titles <> [rule]) layout)
  where
    top = layoutY layout
    spacing = 0.5
    titles =
      [ case columnAlign (placedColumn placed) of
          AlignLeft -> TextMark (textLeft placed) (top + 10) Bold columnTitleSize grey spacing title
          -- The spacing after the last letter is not part of what shows
          AlignRight -> TextMark (textRight placed - textWidth Bold columnTitleSize spacing title + spacing) (top + 10) Bold columnTitleSize grey spacing title
        | placed <- columns,
          let title = Text.toUpper (columnTitle (placedColumn placed))
      ]
    rule = RuleMark contentLeft contentRight (top + 17) 0.8 navy

-- | The lines of a cell as the layout writes them: those of its text,
-- those of its remark, and whether the remark runs on below the cells
-- to its right
data CellLines = CellLines
  { linesOfText :: ![Text],
    linesOfRemark :: ![Text],
    _linesRemarkRunsOn :: !Bool
  }

-- | A table: the titles of its columns, then its rows. A row that does
-- not fit on the page starts the next one, below the titles again.
placeTable :: Table -> Layout -> Layout
placeTable table start = moveDown 8 (foldl' (flip placeRow) titled (zip rows (drop 1 (map (Just . rowKind) rows) <> [Nothing])))
  where
    rows = tableRows table
    columns = placedColumns (tableColumns table)
    -- The titles are never left alone at the foot of a page: they
    -- start the next one with the first row
    firstRowHeight = maybe 0 (rowHeight . rowLines) (listToMaybe rows)
    titled = placeColumnTitles columns (ensureRoom (18 + firstRowHeight) start)
    bodyLeading = 11.5
    remarkLeading = 9.5
    -- The lines of each cell of a row: those of its text, then those of
    -- its remark
    rowLines row =
      [ CellLines (wrapText font bodySize width (cellText cell)) (wrapText Regular remarkSize remarkWidth (cellRemark cell)) (cellRemarkRunsOn cell)
        | (placed, cell) <- zip columns (rowCells row),
          let width = textRight placed - textLeft placed
              -- A remark that runs on has the width up to the right
              -- edge of the table
              remarkWidth = if cellRemarkRunsOn cell then contentRight - textLeft placed else width
      ]
      where
        font = rowFont row
    rowFont row = case rowKind row of
      BodyRow -> Regular
      TotalRow -> Bold
    -- A row is as high as its highest cell
    rowHeight cellLines =
      let textLines = maximum (1 : (length . linesOfText <$> cellLines))
          remarkLines = maximum (0 : (length . linesOfRemark <$> cellLines))
       in 18 + bodyLeading * fromIntegral (textLines - 1) + remarkLeading * fromIntegral remarkLines
    -- A row comes with the kind of the row after it, or none for the
    -- last row. A total has its own rule above it, so the row before it
    -- draws none below.
    placeRow (row, next) layout =
      let cellLines = rowLines row
          height = rowHeight cellLines
          room =
            if layoutY layout + height <= contentBottom
              then layout
              else placeColumnTitles columns (newPage layout)
          top = layoutY room
          textLineCount = maximum (1 : (length . linesOfText <$> cellLines))
          remarkTop = top + 12 + bodyLeading * fromIntegral (textLineCount - 1) + 10
          -- A remark that runs on starts at the left edge of its cell.
          -- Any other remark is on the side its column is aligned on.
          remarkLine placed runsOn baseline text
            | runsOn = textAt (textLeft placed) baseline Regular remarkSize grey text
            | otherwise = cellLine placed baseline Regular remarkSize grey text
          cells =
            concat
              [ [cellLine placed (top + 12 + bodyLeading * line) (rowFont row) bodySize ink text | (line, text) <- zip [0 ..] textLines]
                  <> [remarkLine placed runsOn (remarkTop + remarkLeading * line) text | (line, text) <- zip [0 ..] remarkLines]
                | (placed, CellLines textLines remarkLines runsOn) <- zip columns cellLines
              ]
          rules = case rowKind row of
            BodyRow -> [RuleMark contentLeft contentRight (top + height) 0.4 hairline | next /= Just TotalRow]
            TotalRow ->
              [ RuleMark contentLeft contentRight top 0.7 ink,
                RuleMark contentLeft contentRight (top + height) 0.5 ink,
                RuleMark contentLeft contentRight (top + height + 1.8) 0.5 ink
              ]
       in moveDown height (draw (cells <> rules) room)

-- | The notes, numbered, each on as many lines as it needs. A note is
-- not cut in two by the end of a page.
placeNotes :: [Text] -> Layout -> Layout
placeNotes notes start = foldl' (flip placeNote) start (zip [1 :: Int ..] notes)
  where
    textLeftEdge = contentLeft + 16
    leading = 11.5
    placeNote (number, note) layout =
      let noteLines = wrapText Regular noteSize (contentRight - textLeftEdge) note
          height = leading * fromIntegral (length noteLines) + 5
          room = ensureRoom height layout
          baseline = layoutY room + 9
          marks =
            textAt contentLeft baseline Regular noteSize grey (Text.pack (show number) <> ".")
              : [textAt textLeftEdge (baseline + leading * line) Regular noteSize ink text | (line, text) <- zip [0 ..] noteLines]
       in moveDown height (draw marks room)
