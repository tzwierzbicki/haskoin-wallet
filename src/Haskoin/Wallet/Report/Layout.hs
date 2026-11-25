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
-- * The first page has the logo of hw at its top right, small, in the
--   colours of the report (@logoMarks@). Nothing else names a program.
--   A report is the statement of its owner, not an advertisement.
--
-- * Nothing runs into what is beside it. A text wider than its place
--   goes on to the next line, the title of a column included.
--
-- = Pages
--
-- The layout goes down the page and starts a new one when the next
-- thing does not fit. A table that goes on to the next page starts
-- there with its column titles again. A row of a table and a note are
-- never cut in two.
--
-- A heading is never the last thing on a page. It moves to the next one
-- with the start of what it heads: the first row of a table below the
-- titles of its columns, the first fact, or the first note. A heading
-- may head another heading, as a section heads its first group. All of
-- them then move together, with the start of what the last one heads.
-- What comes after that start may go on to the next page.
--
-- The layout measures that start exactly (@startHeight@). A fixed
-- allowance was tried first, such as the room of a short table below a
-- section heading. It left a heading alone whenever the start was
-- higher than the allowance, and whenever the heading below it asked
-- for its own room and did not get it.
--
-- A text that is wider than its place goes on to the next line, at a
-- space where there is one ('wrapText'). Nothing is cut short: a label
-- reads whole, on as many lines as it needs.
--
-- = A page laid out by hand
--
-- A sheet to write a mnemonic on is no report: it is one page whose parts
-- have fixed places, with lines to write on that take the room left
-- ("Haskoin.Wallet.Report.MnemonicSheet"). It places its own marks, and
-- takes from here what makes a page of hw look like one: the measures of
-- the page, the colours, the title block with the logo ('titleMarks'),
-- the foot ('footerMarks') and the numbered notes ('notesMarks').
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

    -- * The style, for a page that is laid out by hand
    paper,
    contentLeft,
    contentRight,
    contentBottom,
    navy,
    ink,
    grey,
    hairline,
    bodySize,
    textAt,
    textEndingAt,
    titleMarks,
    footerMarks,
    notesHeight,
    notesMarks,
  )
where

import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (ZonedTime)
import Haskoin.Wallet.Logo (logoName, nameBits)
import Haskoin.Wallet.Report.Pdf

{- What a report holds -}

-- | A report: what its first page says at the top, what every page says
-- at the bottom, and its blocks from the first to the last
data Report = Report
  { -- | The kind of document, in small capitals above the title, such as
    -- "Statement of cryptocurrency holdings"
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
wrapText font size = wrapSpacedText font size 0

-- | 'wrapText' for a text written with room between its characters, as
-- the titles of the columns are: the room counts in the width of a line
wrapSpacedText :: Font -> Double -> Double -> Double -> Text -> [Text]
wrapSpacedText font size spacing width = reverse . finish . foldl' place ([], "") . concatMap pieces . Text.words
  where
    fits text = textWidth font size spacing text <= width
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

-- | Whether the height given still fits on the page the layout is on.
--
-- A hundredth of a point of slack is allowed. The room for a heading
-- and for what it heads is asked for at once, and again for each part
-- as it is placed. The same heights added in another order may differ
-- in their last digit, and must still give the same answer.
fitsOnPage :: Double -> Layout -> Bool
fitsOnPage height layout = layoutY layout + height <= contentBottom + 0.01

-- | Start a new page unless the height given still fits on this one.
-- At the top of a page there is no more room to be had on another one.
ensureRoom :: Double -> Layout -> Layout
ensureRoom height layout
  | fitsOnPage height layout = layout
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
    start = Layout [] (reverse (titleMarks (reportKind report) (reportTitle report) (reportSubtitle report))) firstPageTop
    end = placeBlocks (reportBlocks report) start
    bodies = reverse (reverse (layoutPage end) : layoutDone end)
    framed number body = pageTop number <> body <> footerMarks (reportFooter report) (pageOf number (length bodies))
    pageOf :: Int -> Int -> Text
    pageOf number count = "Page " <> Text.pack (show number) <> " of " <> Text.pack (show count)
    -- The first page has the rule of its title block. The others start
    -- with a thinner one.
    pageTop :: Int -> [Mark]
    pageTop 1 = []
    pageTop _ = [BoxMark contentLeft 42 contentWidth 1 navy]

-- | The report as the bytes of its PDF file, made at the time given
reportPdf :: ZonedTime -> Report -> BS.ByteString
reportPdf created report =
  pdfDocument (DocumentInfo (reportTitle report <> ", " <> reportSubtitle report) created) paper (reportPages report)

-- | The baseline of the kind of document, the first line of text of the
-- first page
kindBaseline :: Double
kindBaseline = 70

-- | The top of a first page: a heavy rule, the kind of document in
-- small capitals, the title and the line below it, and the logo at the
-- top right
titleMarks :: Text -> Text -> Text -> [Mark]
titleMarks kind title subtitle =
  [ BoxMark contentLeft 42 contentWidth 3 navy,
    TextMark contentLeft kindBaseline Bold 7.5 grey 1.2 (Text.toUpper kind),
    textAt contentLeft 99 Bold 25 navy title,
    textAt contentLeft 120 Regular 12 ink subtitle
  ]
    <> logoMarks

-- | The side of a square of the logo, and the room from the left of one
-- square to the left of the next, across and down
logoSquare, logoPitch :: Double
logoSquare = 4.4
logoPitch = 6.2

-- | The size of the name of the logo, and the room between the squares
-- and the name. At this size the letter h is as high as the two rows of
-- squares.
logoNameSize, logoNameGap :: Double
logoNameSize = 14.5
logoNameGap = 7

-- | The logo of hw, at the top right of the first page: the bits of its
-- name as two rows of squares, and the name after them
-- ("Haskoin.Wallet.Logo"). A set bit is a square in the dark blue of the
-- titles. A clear bit is one in the light grey of the hairlines.
--
-- The logo ends at the right edge of the page, and stands on the line
-- of the kind of document. That line has room on its right in every
-- report. The line of the title does not: the title of a payment
-- confirmation reaches under the place of the logo.
--
-- The logo is on the first page only. The other pages have the name of
-- the report at their foot, which is what tells them apart.
--
-- It is made of the marks every page is made of, filled boxes and a
-- text, so the PDF writer needs nothing new for it.
logoMarks :: [Mark]
logoMarks = squares <> [textAt nameLeft kindBaseline Bold logoNameSize navy name]
  where
    name = Text.pack logoName
    rows = nameBits logoName
    -- From the right: the name, the room before it, then the squares
    nameLeft = contentRight - textWidth Bold logoNameSize 0 name
    squaresWidth = logoPitch * 7 + logoSquare
    squaresLeft = nameLeft - logoNameGap - squaresWidth
    -- The last row stands on the baseline, and the rows above it follow
    -- from there
    lastRowTop = kindBaseline - logoSquare
    rowTop row = lastRowTop - logoPitch * fromIntegral (length rows - 1 - row)
    squares =
      [ BoxMark (squaresLeft + logoPitch * fromIntegral column) (rowTop row) logoSquare logoSquare (if set then navy else hairline)
        | (row, bits) <- zip [0 :: Int ..] rows,
          (column, set) <- zip [0 :: Int ..] bits
      ]

-- | The foot of a page: a thin rule, a text on the left and one on the
-- right. A report has its name on the left, and the number of the page
-- among how many on the right.
footerMarks :: Text -> Text -> [Mark]
footerMarks left right =
  [ RuleMark contentLeft contentRight (paperHeight paper - 56) 0.5 hairline,
    textAt contentLeft baseline Regular footerSize grey left,
    textEndingAt contentRight baseline Regular footerSize grey right
  ]
  where
    baseline = paperHeight paper - 43

-- | Put the blocks on the pages, from the first to the last. Before
-- each block the layout makes sure its start fits on the page, and
-- starts a new page when it does not (@startHeight@).
placeBlocks :: [Block] -> Layout -> Layout
placeBlocks [] layout = layout
placeBlocks blocks@(block : following) layout =
  placeBlocks following (placeBlock block (ensureRoom (startHeight blocks) layout))

-- | How high the start of the blocks given is: what must be on one
-- page, from the top of the first block.
--
-- A heading must not be the last thing on a page, so its start is
-- itself and the start of what follows it. A section that heads a group
-- that heads a table thus starts with both headings, the titles of the
-- columns and the first row. The start of any other block is the first
-- thing in it that is not cut in two.
--
-- The room above a heading is counted, though a heading at the top of
-- a page has none. A heading that moves to a new page then asks for a
-- little more than it takes, which a new page always has.
startHeight :: [Block] -> Double
startHeight [] = 0
startHeight (block : following) = case block of
  SectionBlock _ -> sectionGap + sectionHeight + startHeight following
  GroupBlock title remark -> groupGap + groupHeight title remark + startHeight following
  FactsBlock facts -> maybe 0 factHeight (listToMaybe facts)
  TableBlock table -> tableStartHeight table
  NotesBlock notes -> maybe 0 noteHeight (listToMaybe notes)

-- | Put a block on the page the layout is on. 'placeBlocks' has made
-- sure that its start fits there.
placeBlock :: Block -> Layout -> Layout
placeBlock block = case block of
  SectionBlock title -> placeSection title
  GroupBlock title remark -> placeGroup title remark
  FactsBlock facts -> placeFacts facts
  TableBlock table -> placeTable table
  NotesBlock notes -> placeNotes notes

-- | The room above the heading of a section, and how high the heading
-- is with the room below it
sectionGap, sectionHeight :: Double
sectionGap = 26
sectionHeight = 26

-- | A section heading
placeSection :: Text -> Layout -> Layout
placeSection title = heading . gapAbove sectionGap
  where
    heading layout =
      let top = layoutY layout
       in moveDown sectionHeight (draw [textAt contentLeft (top + 12) Bold 12.5 navy title] layout)

-- | The room above the heading of a group
groupGap :: Double
groupGap = 12

-- | The size of the title of a group
groupTitleSize :: Double
groupTitleSize = 9.5

-- | Where the remark of a group starts when it is beside the title
groupRemarkLeft :: Text -> Double
groupRemarkLeft title = contentLeft + textWidth Bold groupTitleSize 0 title + 10

-- | The lines of the remark of a group that go below its title: none
-- when there is no remark, or when it fits beside the title
groupRemarkBelow :: Text -> Text -> [Text]
groupRemarkBelow title remark
  | Text.null remark = []
  | groupRemarkLeft title + textWidth Regular bodySize 0 remark <= contentRight = []
  | otherwise = wrapText Regular bodySize contentWidth remark

-- | The room from one line of a remark below the title of a group to
-- the next
groupRemarkLeading :: Double
groupRemarkLeading = 12

-- | How high the heading of a group is, with the lines of its remark
-- that go below the title
groupHeight :: Text -> Text -> Double
groupHeight title remark = 20 + groupRemarkLeading * fromIntegral (length (groupRemarkBelow title remark))

-- | The heading of a group, with its remark beside it. A remark too long
-- for the line goes below, on as many lines as it needs.
placeGroup :: Text -> Text -> Layout -> Layout
placeGroup title remark = heading . gapAbove groupGap
  where
    heading layout = moveDown (groupHeight title remark) (draw (titleMark : remarkMarks) layout)
      where
        baseline = layoutY layout + 11
        titleMark = textAt contentLeft baseline Bold groupTitleSize ink title
        below = groupRemarkBelow title remark
        remarkMarks
          | Text.null remark = []
          | null below = [textAt (groupRemarkLeft title) baseline Regular bodySize grey remark]
          | otherwise = [textAt contentLeft (baseline + groupRemarkLeading * line) Regular bodySize grey text | (line, text) <- zip [1 ..] below]

-- | Where the value of a fact starts, to the right of its name
factValueLeft :: Double
factValueLeft = contentLeft + 118

-- | The room from one line of a fact to the next
factLeading :: Double
factLeading = 13.5

-- | The lines of the value of a fact as the layout writes them: each
-- line given, on as many lines as it needs. A line without a word stays
-- as an empty line.
factValueLines :: Fact -> [Text]
factValueLines fact = concatMap (orBlank . wrapText Regular bodySize (contentRight - factValueLeft)) (factLines fact)
  where
    orBlank [] = [""]
    orBlank textLines = textLines

-- | How high a fact is: a line for each line of its value, and one line
-- for a fact without a value
factHeight :: Fact -> Double
factHeight fact = factLeading * fromIntegral (max 1 (length (factValueLines fact)))

-- | The facts, each on its line: the name in grey, then the value. A
-- value of several lines keeps its name on the first one. A fact is not
-- cut in two by the end of a page.
placeFacts :: [Fact] -> Layout -> Layout
placeFacts facts start = moveDown 6 (foldl' (flip placeFact) start facts)
  where
    placeFact fact layout =
      let height = factHeight fact
          room = ensureRoom height layout
          baseline = layoutY room + 10
          marks =
            textAt contentLeft baseline Regular bodySize grey (factName fact)
              : [textAt factValueLeft (baseline + factLeading * line) Regular bodySize ink text | (line, text) <- zip [0 ..] (factValueLines fact)]
       in moveDown height (draw marks room)

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

-- | The room between the characters of the title of a column
titleSpacing :: Double
titleSpacing = 0.5

-- | The room from one line of the title of a column to the next
titleLeading :: Double
titleLeading = 8.5

-- | The title of each column, in capitals, on as many lines as it needs
-- to stay in its column. A title wider than its column would otherwise
-- run into the title beside it: "Balance (BTCTEST4)" is wider than a
-- column of amounts.
columnTitleLines :: [Placed] -> [[Text]]
columnTitleLines columns =
  [ wrapSpacedText Bold columnTitleSize titleSpacing (textRight placed - textLeft placed) (Text.toUpper (columnTitle (placedColumn placed)))
    | placed <- columns
  ]

-- | How high the titles of the columns are, with the rule below them:
-- 18 points for titles of one line, and a line more for each line the
-- longest title needs
columnTitlesHeight :: [Placed] -> Double
columnTitlesHeight columns = 18 + titleLeading * fromIntegral (titleLineCount - 1)
  where
    titleLineCount = maximum (1 : (length <$> columnTitleLines columns))

-- | The titles of the columns, in small grey capitals, with a rule below
-- them. They come at the top of a table, and again on each page the
-- table goes on to. A title of several lines ends on the line of the
-- others, just above the rule, so that the last lines stand in one row.
placeColumnTitles :: [Placed] -> Layout -> Layout
placeColumnTitles columns layout = moveDown height (draw (titles <> [rule]) layout)
  where
    top = layoutY layout
    height = columnTitlesHeight columns
    -- The baseline of the last line of every title
    lastBaseline = top + height - 8
    titles =
      concat
        [ [titleLine placed (lastBaseline - titleLeading * fromIntegral linesBelow) text | (linesBelow, text) <- zip [length titleLines - 1, length titleLines - 2 ..] titleLines]
          | (placed, titleLines) <- zip columns (columnTitleLines columns)
        ]
    titleLine placed baseline text = case columnAlign (placedColumn placed) of
      AlignLeft -> TextMark (textLeft placed) baseline Bold columnTitleSize grey titleSpacing text
      -- The spacing after the last letter is not part of what shows
      AlignRight -> TextMark (textRight placed - textWidth Bold columnTitleSize titleSpacing text + titleSpacing) baseline Bold columnTitleSize grey titleSpacing text
    rule = RuleMark contentLeft contentRight (top + height - 1) 0.8 navy

-- | The lines of a cell as the layout writes them: those of its text,
-- those of its remark, and whether the remark runs on below the cells
-- to its right
data CellLines = CellLines
  { linesOfText :: ![Text],
    linesOfRemark :: ![Text],
    _linesRemarkRunsOn :: !Bool
  }

-- | The font of a row: a total is in bold
rowFont :: Row -> Font
rowFont row = case rowKind row of
  BodyRow -> Regular
  TotalRow -> Bold

-- | The room from one line of the text of a cell to the next, and from
-- one line of its remark to the next
bodyLeading, remarkLeading :: Double
bodyLeading = 11.5
remarkLeading = 9.5

-- | The lines of each cell of a row in the columns given: those of its
-- text, then those of its remark
rowLines :: [Placed] -> Row -> [CellLines]
rowLines columns row =
  [ CellLines (wrapText (rowFont row) bodySize width (cellText cell)) (wrapText Regular remarkSize remarkWidth (cellRemark cell)) (cellRemarkRunsOn cell)
    | (placed, cell) <- zip columns (rowCells row),
      let width = textRight placed - textLeft placed
          -- A remark that runs on has the width up to the right edge of
          -- the table
          remarkWidth = if cellRemarkRunsOn cell then contentRight - textLeft placed else width
  ]

-- | How high a row is: as high as its highest cell
rowHeight :: [CellLines] -> Double
rowHeight cellLines = 18 + bodyLeading * fromIntegral (textLines - 1) + remarkLeading * fromIntegral remarkLines
  where
    textLines = maximum (1 : (length . linesOfText <$> cellLines))
    remarkLines = maximum (0 : (length . linesOfRemark <$> cellLines))

-- | How high the start of a table is: the titles of its columns and its
-- first row. The titles are never left alone at the foot of a page,
-- and neither is a heading above them.
tableStartHeight :: Table -> Double
tableStartHeight table = columnTitlesHeight columns + maybe 0 (rowHeight . rowLines columns) (listToMaybe (tableRows table))
  where
    columns = placedColumns (tableColumns table)

-- | A table: the titles of its columns, then its rows. A row that does
-- not fit on the page starts the next one, below the titles again.
placeTable :: Table -> Layout -> Layout
placeTable table start = moveDown 8 (foldl' (flip placeRow) titled (zip rows (drop 1 (map (Just . rowKind) rows) <> [Nothing])))
  where
    rows = tableRows table
    columns = placedColumns (tableColumns table)
    titled = placeColumnTitles columns start
    -- A row comes with the kind of the row after it, or none for the
    -- last row. A total has its own rule above it, so the row before it
    -- draws none below.
    placeRow (row, next) layout =
      let cellLines = rowLines columns row
          height = rowHeight cellLines
          room =
            if fitsOnPage height layout
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

-- | Where the text of a note starts, to the right of its number
noteTextLeft :: Double
noteTextLeft = contentLeft + 16

-- | The room from one line of a note to the next
noteLeading :: Double
noteLeading = 11.5

-- | The lines of a note as the layout writes them
noteLines :: Text -> [Text]
noteLines = wrapText Regular noteSize (contentRight - noteTextLeft)

-- | How high a note is, with the room below it
noteHeight :: Text -> Double
noteHeight note = noteLeading * fromIntegral (length (noteLines note)) + 5

-- | A note below the height given: its number, then its text on as many
-- lines as it needs
noteMarks :: Int -> Double -> Text -> [Mark]
noteMarks number top note =
  textAt contentLeft baseline Regular noteSize grey (Text.pack (show number) <> ".")
    : [textAt noteTextLeft (baseline + noteLeading * line) Regular noteSize ink text | (line, text) <- zip [0 ..] (noteLines note)]
  where
    baseline = top + 9

-- | The notes, numbered, each on as many lines as it needs. A note is
-- not cut in two by the end of a page.
placeNotes :: [Text] -> Layout -> Layout
placeNotes notes start = foldl' (flip placeNote) start (zip [1 ..] notes)
  where
    placeNote (number, note) layout =
      let height = noteHeight note
          room = ensureRoom height layout
       in moveDown height (draw (noteMarks number (layoutY room) note) room)

-- | How high notes are together, each with the room below it
notesHeight :: [Text] -> Double
notesHeight = sum . map noteHeight

-- | Notes numbered from 1, below the height given, on one page: for a
-- page laid out by hand, which knows that they fit ('notesHeight')
notesMarks :: Double -> [Text] -> [Mark]
notesMarks top notes = concat (zipWith3 noteMarks [1 ..] (scanl (+) top (noteHeight <$> notes)) notes)
