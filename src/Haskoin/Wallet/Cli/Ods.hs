{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Sheets as an OpenDocument spreadsheet (ODS, ISO/IEC 26300), the
-- format of LibreOffice, as exportsheet writes the wallet: a tab for each
-- listing. Excel and Google Sheets open it too.
--
-- Unlike a CSV file ("Haskoin.Wallet.Cli.Csv"), a spreadsheet keeps the
-- kind of each cell ('Cell'), so nothing is left to the guesses of the
-- program that opens it:
--
-- * A text is a string, whatever it holds. A label that starts with = is
--   never run as a formula, and one made of digits keeps its leading
--   zeros. Labels are kept as they are, with no apostrophe in front.
-- * An amount in satoshis is a number, shown with the separators of
--   thousands of the computer: 5,000,000, or 5 000 000.
-- * A time is a time, in UTC, shown as 2023-11-14 22:13:20. It sorts and
--   counts as one.
--
-- The headings are bold, and the header stays at the top of the sheet as
-- its rows scroll, as less keeps the header of a table. A column is as wide
-- as its widest cell ('columnMillimetres'), since LibreOffice does not size
-- the columns of a file it opens.
--
-- The file is a zip archive of XML documents: the type of the file, a
-- manifest, the sheets ('contentXml') and the settings of the view
-- ('settingsXml'). The type comes first and is stored as it is, so that it
-- shows at byte 30 of the file, where OpenDocument says to look for it. No
-- Haskell library writes OpenDocument, and the few elements a sheet needs
-- are written here. An Office Open XML file (XLSX) would take the same
-- sheets; this format was asked for, and the library that writes XLSX
-- binds a C library the builds of hw do not have.
module Haskoin.Wallet.Cli.Ods
  ( workbookOds,
    contentXml,
    settingsXml,
    tableXml,
    cellXml,
    paragraphXml,
    columnMillimetres,
  )
where

import Codec.Archive.Zip (Archive (..), CompressionMethod (..), Entry (..), fromArchive, toEntry)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, transpose)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Haskoin.Wallet.Cli.Sheet (Cell (..), Sheet (..))
import Text.DocLayout (realLength)

-- | The sheets, each with its name, as the bytes of an OpenDocument
-- spreadsheet, its parts dated at the time given
workbookOds :: UTCTime -> [(Text, Sheet)] -> ByteString
workbookOds time sheets =
  BL.toStrict . fromArchive $
    Archive
      { zEntries =
          [ storedEntry "mimetype" seconds (BL.fromStrict (encodeUtf8 spreadsheetType)),
            xmlEntry "META-INF/manifest.xml" manifestXml,
            xmlEntry "content.xml" (contentXml sheets),
            xmlEntry "settings.xml" (settingsXml (fst <$> sheets))
          ],
        zSignature = Nothing,
        zComment = BL.empty
      }
  where
    seconds = floor (utcTimeToPOSIXSeconds time)
    xmlEntry path xml = toEntry path seconds (BL.fromStrict (encodeUtf8 xml))

-- | The media type of an OpenDocument spreadsheet, which the file starts
-- with
spreadsheetType :: Text
spreadsheetType = "application/vnd.oasis.opendocument.spreadsheet"

-- | An entry of the archive stored as it is, not compressed, as
-- OpenDocument asks of the type of the file. toEntry compresses an entry
-- when that makes it smaller.
storedEntry :: FilePath -> Integer -> BL.ByteString -> Entry
storedEntry path seconds contents =
  (toEntry path seconds contents)
    { eCompressionMethod = NoCompression,
      eCompressedSize = fromIntegral (BL.length contents),
      eCompressedData = contents
    }

-- | The manifest of the archive: what each part of it is
manifestXml :: Text
manifestXml =
  Text.concat
    [ xmlDeclaration,
      "<manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\" manifest:version=\"1.3\">",
      "<manifest:file-entry manifest:full-path=\"/\" manifest:version=\"1.3\" manifest:media-type=\"",
      spreadsheetType,
      "\"/>",
      "<manifest:file-entry manifest:full-path=\"content.xml\" manifest:media-type=\"text/xml\"/>",
      "<manifest:file-entry manifest:full-path=\"settings.xml\" manifest:media-type=\"text/xml\"/>",
      "</manifest:manifest>"
    ]

-- | The first line of an XML document
xmlDeclaration :: Text
xmlDeclaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

{- The sheets -}

-- | The sheets of the spreadsheet, each a table with its name: the
-- styles they use, then each table
contentXml :: [(Text, Sheet)] -> Text
contentXml sheets =
  Text.concat $
    [ xmlDeclaration,
      "<office:document-content",
      " xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\"",
      " xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\"",
      " xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\"",
      " xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\"",
      " xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\"",
      " xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\"",
      " office:version=\"1.3\">",
      "<office:automatic-styles>",
      cellStylesXml
    ]
      <> map columnStyleXml (nub (concatMap (columnMillimetres . snd) sheets))
      <> ["</office:automatic-styles>", "<office:body><office:spreadsheet>"]
      <> map tableXml sheets
      <> ["</office:spreadsheet></office:body></office:document-content>"]

-- | The styles of the cells: a bold heading, an amount in satoshis with
-- the separators of thousands, and a time to the second
cellStylesXml :: Text
cellStylesXml =
  Text.concat
    [ "<number:number-style style:name=\"grouped\">",
      "<number:number number:decimal-places=\"0\" number:min-integer-digits=\"1\" number:grouping=\"true\"/>",
      "</number:number-style>",
      "<number:date-style style:name=\"to-the-second\">",
      "<number:year number:style=\"long\"/><number:text>-</number:text>",
      "<number:month number:style=\"long\"/><number:text>-</number:text>",
      "<number:day number:style=\"long\"/><number:text> </number:text>",
      "<number:hours number:style=\"long\"/><number:text>:</number:text>",
      "<number:minutes number:style=\"long\"/><number:text>:</number:text>",
      "<number:seconds number:style=\"long\"/>",
      "</number:date-style>",
      "<style:style style:name=\"heading\" style:family=\"table-cell\"><style:text-properties fo:font-weight=\"bold\"/></style:style>",
      "<style:style style:name=\"satoshis\" style:family=\"table-cell\" style:data-style-name=\"grouped\"/>",
      "<style:style style:name=\"time\" style:family=\"table-cell\" style:data-style-name=\"to-the-second\"/>"
    ]

-- | The style of a column of the width given, in millimetres, named after
-- it
columnStyleXml :: Int -> Text
columnStyleXml millimetres =
  "<style:style style:name=\"" <> columnStyleName millimetres <> "\" style:family=\"table-column\">"
    <> "<style:table-column-properties style:column-width=\""
    <> showText millimetres
    <> "mm\"/></style:style>"

-- | The name of the style of a column of the width given, in millimetres
columnStyleName :: Int -> Text
columnStyleName millimetres = "width-" <> showText millimetres

-- | A sheet as a table with its name: its columns, each as wide as its
-- cells, its headings in a header row, then a row for each of its rows
tableXml :: (Text, Sheet) -> Text
tableXml (name, sheet) =
  Text.concat $
    ["<table:table table:name=\"" <> escapeXml name <> "\">"]
      <> ["<table:table-column table:style-name=\"" <> columnStyleName width <> "\"/>" | width <- columnMillimetres sheet]
      <> ["<table:table-header-rows>", rowXml (headingXml <$> sheetHeadings sheet), "</table:table-header-rows>"]
      <> [rowXml (cellXml <$> row) | row <- sheetRows sheet]
      <> ["</table:table>"]

-- | A row of a table, of the cells given
rowXml :: [Text] -> Text
rowXml cells = "<table:table-row>" <> Text.concat cells <> "</table:table-row>"

-- | A heading, in bold
headingXml :: Text -> Text
headingXml heading = tableCell [("table:style-name", "heading"), ("office:value-type", "string")] (paragraphXml heading)

-- | A cell of a table, of the kind of the cell of the sheet. A number or a
-- time shows as its style says, whatever its paragraph holds: the
-- paragraph is what a program shows that reads no style.
cellXml :: Cell -> Text
cellXml = \case
  TextCell t -> tableCell [("office:value-type", "string")] (paragraphXml t)
  SatoshiCell amount -> tableCell [("table:style-name", "satoshis"), ("office:value-type", "float"), ("office:value", showText amount)] (showText amount)
  CountCell n -> tableCell [("office:value-type", "float"), ("office:value", showText n)] (showText n)
  TimeCell time ->
    tableCell
      [("table:style-name", "time"), ("office:value-type", "date"), ("office:date-value", Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time))]
      (Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time))
  EmptyCell -> "<table:table-cell/>"

-- | A cell with its attributes and its paragraph
tableCell :: [(Text, Text)] -> Text -> Text
tableCell attributes paragraph =
  "<table:table-cell"
    <> Text.concat [" " <> name <> "=\"" <> escapeXml value <> "\"" | (name, value) <- attributes]
    <> "><text:p>"
    <> paragraph
    <> "</text:p></table:table-cell>"

-- | A text as the paragraph of a cell, every character kept. XML has its
-- characters escaped. OpenDocument collapses spaces in a row, and drops
-- those at the start or the end of a paragraph, so a space is written as
-- it is only between two characters: the others are counted in a text:s
-- element. A tab and a line break have elements of their own. A character
-- XML cannot hold, a control character such as ESC, becomes U+FFFD, the
-- replacement character.
paragraphXml :: Text -> Text
paragraphXml = Text.concat . pieces False . Text.unpack
  where
    pieces :: Bool -> String -> [Text]
    pieces _ [] = []
    pieces afterText s@(' ' : _) =
      let (spaces, rest) = span (== ' ') s
          literal = afterText && not (null rest)
       in [" " | literal] <> spacesElement (length spaces - fromEnum literal) <> pieces False rest
    pieces _ ('\t' : rest) = "<text:tab/>" : pieces False rest
    pieces _ ('\r' : '\n' : rest) = "<text:line-break/>" : pieces False rest
    pieces _ (c : rest)
      | c == '\n' || c == '\r' = "<text:line-break/>" : pieces False rest
      | otherwise = escapeChar c : pieces True rest
    spacesElement :: Int -> [Text]
    spacesElement 0 = []
    spacesElement 1 = ["<text:s/>"]
    spacesElement n = ["<text:s text:c=\"" <> showText n <> "\"/>"]

-- | A text in an attribute or a paragraph, its characters escaped
escapeXml :: Text -> Text
escapeXml = Text.concatMap escapeChar

-- | A character as XML writes it: the five that XML gives a meaning as
-- entities, one XML cannot hold as U+FFFD, the others as they are
escapeChar :: Char -> Text
escapeChar = \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&apos;"
  c
    | c < ' ' && c `notElem` ['\t', '\n', '\r'] -> "\xFFFD"
    | c == '\xFFFE' || c == '\xFFFF' -> "\xFFFD"
    | otherwise -> Text.singleton c

{- Widths -}

-- | The width of each column of a sheet, in millimetres: 2 for each
-- character of its widest cell, its heading included, and 3 for the
-- margins. A wide character, as in Chinese or Japanese, counts two. The
-- widest cell is counted up to 64 characters, those of a txid: a longer
-- label shows its start, and the rest when the user widens the column.
columnMillimetres :: Sheet -> [Int]
columnMillimetres sheet =
  [ 2 * min 64 (maximum (map shownLength column)) + 3
  | column <- transpose (map TextCell (sheetHeadings sheet) : sheetRows sheet)
  ]

-- | How many characters a cell shows: its longest line for a text, an
-- amount with its separators of thousands, a time to the second
shownLength :: Cell -> Int
shownLength = \case
  TextCell t -> maximum (0 : map realLength (Text.lines t))
  SatoshiCell amount ->
    let digits = length (show (abs amount))
     in digits + (digits - 1) `div` 3 + (if amount < 0 then 1 else 0)
  CountCell n -> length (show n)
  TimeCell _ -> 19
  EmptyCell -> 0

{- The view -}

-- | The settings of the view of LibreOffice: the first row of each sheet,
-- its header, frozen at the top, so that it stays as the rows scroll
-- (vertical split mode 2, at row 1). The first sheet is the one shown.
settingsXml :: [Text] -> Text
settingsXml names =
  Text.concat $
    [ xmlDeclaration,
      "<office:document-settings",
      " xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\"",
      " xmlns:config=\"urn:oasis:names:tc:opendocument:xmlns:config:1.0\"",
      " office:version=\"1.3\">",
      "<office:settings><config:config-item-set config:name=\"ooo:view-settings\">",
      "<config:config-item-map-indexed config:name=\"Views\"><config:config-item-map-entry>",
      item "ViewId" "string" "view1",
      "<config:config-item-map-named config:name=\"Tables\">"
    ]
      <> map frozenHeader names
      <> ["</config:config-item-map-named>"]
      <> [item "ActiveTable" "string" (escapeXml first) | first : _ <- [names]]
      <> ["</config:config-item-map-entry></config:config-item-map-indexed>", "</config:config-item-set></office:settings></office:document-settings>"]
  where
    item :: Text -> Text -> Text -> Text
    item name kind value = "<config:config-item config:name=\"" <> name <> "\" config:type=\"" <> kind <> "\">" <> value <> "</config:config-item>"
    frozenHeader name =
      Text.concat
        [ "<config:config-item-map-entry config:name=\"" <> escapeXml name <> "\">",
          item "HorizontalSplitMode" "short" "0",
          item "VerticalSplitMode" "short" "2",
          item "HorizontalSplitPosition" "int" "0",
          item "VerticalSplitPosition" "int" "1",
          item "ActiveSplitRange" "short" "2",
          item "PositionLeft" "int" "0",
          item "PositionRight" "int" "0",
          item "PositionTop" "int" "0",
          item "PositionBottom" "int" "1",
          "</config:config-item-map-entry>"
        ]

-- | A number as text
showText :: (Show a) => a -> Text
showText = Text.pack . show
