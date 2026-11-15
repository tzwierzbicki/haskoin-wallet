{-# LANGUAGE OverloadedStrings #-}

-- | Pages of text and lines written as a PDF file, for the reports of hw
-- ("Haskoin.Wallet.Cli.Report").
--
-- A report goes to people who have no hw: a tax office, an accountant, a
-- bank. They open it, print it and file it. PDF is the one format that
-- looks the same on every computer and on paper, so the reports are PDF
-- files and nothing else.
--
-- = What this module does
--
-- A page is a list of marks: a line of text at a place, a horizontal
-- rule, a filled box. This module writes the pages to the bytes of a
-- file. It knows nothing of reports. The layout, which decides where
-- each mark goes, is in "Haskoin.Wallet.Cli.Report".
--
-- The layout needs the width of a text before it places it: an amount is
-- aligned on its right edge, and a long label is cut into lines. So this
-- module also measures text ('textWidth').
--
-- = The writer is made by hand
--
-- A PDF file of text and lines is a short list of numbered objects: a
-- catalog, the pages, their contents, the fonts, and a table that says
-- where each object starts. A library for the format brings a layout
-- engine and its dependencies with it, and hw needs neither. The
-- spreadsheet of exportsheet is written by hand for the same reason
-- ("Haskoin.Wallet.Cli.Ods").
--
-- The bytes follow from the pages and from the document information
-- alone. The file holds no random identifier and no clock reading of its
-- own: the time of creation is given by the caller. So the same report
-- written twice is the same file, and a test can state its bytes.
--
-- = The fonts are inside the file
--
-- The file carries its two fonts. It then looks the same wherever it is
-- opened and printed, and it shows every script the fonts have. A file
-- can also name one of the fourteen fonts every PDF reader has, and
-- carry none. The first reports did. Each reader then draws a look-alike
-- of its own, and the text is held to the letters of Western Europe.
--
-- Only the glyphs the document uses are carried, in a subset of each
-- font ("Haskoin.Wallet.Cli.Font"). A statement is then some tens of
-- kilobytes, where the two whole fonts would add eight hundred.
--
-- A text is written as the numbers of its glyphs in the font, two bytes
-- each, which is how PDF takes the text of an embedded font of any
-- script. A reader cannot tell the characters from those numbers. So
-- each font comes with a table that gives the character of each glyph,
-- and a reader can search the text and copy it ('toUnicodeMap').
--
-- A character the font does not have is written as a question mark
-- ('shownText'). Only the texts of the user can hold one: a label, a
-- description, the name of the holder.
--
-- = Coordinates
--
-- A mark is placed in points, 72 to the inch, from the top left corner
-- of the page, down and to the right. That is how a layout works, from
-- the top of a page to its bottom. PDF counts from the bottom left
-- corner, and the writer turns the heights around.
module Haskoin.Wallet.Cli.Pdf
  ( Font (..),
    Colour (..),
    Mark (..),
    Paper (..),
    a4,
    DocumentInfo (..),
    textWidth,
    shownText,
    glyphsOf,
    pdfDocument,
    pageContent,
    pdfDate,
  )
where

import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace, ord)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Normalize as Normalize
import Data.Time (ZonedTime (..), defaultTimeLocale, formatTime, timeZoneMinutes)
import Haskoin.Wallet.Cli.Font (Typeface (..), advanceOf, glyphOf, subsetFont)
import Haskoin.Wallet.Cli.Typefaces (boldTypeface, regularTypeface)
import Text.Printf (printf)

{- What a page holds -}

-- | The two fonts of the reports: Liberation Sans and its bold
-- ("Haskoin.Wallet.Cli.Typefaces"). One family in two weights is enough
-- for a statement, and keeps every report in the same style.
data Font = Regular | Bold
  deriving (Eq, Show)

-- | A colour as its parts of red, green and blue, each from 0 to 1
data Colour = Colour !Double !Double !Double
  deriving (Eq, Show)

-- | One thing drawn on a page. Every place is in points from the top
-- left corner of the page.
data Mark
  = -- | A line of text. Its place is the left end of its baseline, the
    -- line the letters stand on. The spacing is the room added after each
    -- character, in points: 0 for running text, a little more for a
    -- title in capitals.
    TextMark
      { textX :: !Double,
        textBaseline :: !Double,
        textFont :: !Font,
        textSize :: !Double,
        textColour :: !Colour,
        textSpacing :: !Double,
        textOf :: !Text
      }
  | -- | A horizontal rule from one place to another at one height, with
    -- its thickness
    RuleMark
      { ruleLeft :: !Double,
        ruleRight :: !Double,
        ruleY :: !Double,
        ruleThickness :: !Double,
        ruleColour :: !Colour
      }
  | -- | A filled box, from its top left corner, with its width and its
    -- height
    BoxMark
      { boxLeft :: !Double,
        boxTop :: !Double,
        boxWidth :: !Double,
        boxHeight :: !Double,
        boxColour :: !Colour
      }
  deriving (Eq, Show)

-- | The size of the paper, in points
data Paper = Paper
  { paperWidth :: !Double,
    paperHeight :: !Double
  }
  deriving (Eq, Show)

-- | A4, 210 by 297 millimetres: the paper of every country but a few
a4 :: Paper
a4 = Paper 595.28 841.89

-- | What a PDF reader shows about the file, outside its pages: the title
-- in the bar of the window, and the time the file was made. The file
-- names no program that made it: a report carries no brand.
data DocumentInfo = DocumentInfo
  { documentTitle :: !Text,
    documentCreated :: !ZonedTime
  }
  deriving (Show)

{- Text and its glyphs -}

-- | The font file of a font of the reports
typefaceOf :: Font -> Typeface
typefaceOf Regular = regularTypeface
typefaceOf Bold = boldTypeface

-- | The name of a font, as its file gives it for PostScript
postScriptName :: Font -> BS.ByteString
postScriptName Regular = "LiberationSans"
postScriptName Bold = "LiberationSans-Bold"

-- | A text as a mark shows it, character for character.
--
-- The text is first put in its composed form, so that an "e" followed by
-- a combining accent becomes the one accented letter the font has. A
-- space of any kind, a tab or a line break among them, becomes a plain
-- space: a mark is one line. A character the font does not have becomes
-- a question mark, and so does a control character.
shownText :: Font -> Text -> Text
shownText font = Text.map shown . Normalize.normalize Normalize.NFC
  where
    shown char
      | isSpace char = ' '
      | isJust (glyphOf (typefaceOf font) char) = char
      | otherwise = '?'

-- | The glyphs that draw a text in a font, one for each character it
-- shows ('shownText')
glyphsOf :: Font -> Text -> [Int]
glyphsOf font text = [fromMaybe 0 (glyphOf (typefaceOf font) char) | char <- Text.unpack (shownText font text)]

-- | The width of a text in points, in a font at a size, with the spacing
-- added after each of its characters. A character the font does not
-- have has the width of the question mark written in its place.
textWidth :: Font -> Double -> Double -> Text -> Double
textWidth font size spacing text =
  fromIntegral (sum (advanceOf typeface <$> glyphs)) * size / fromIntegral (typefaceUnitsPerEm typeface)
    + spacing * fromIntegral (length glyphs)
  where
    typeface = typefaceOf font
    glyphs = glyphsOf font text

{- The content of a page -}

-- | The marks of a page as the drawing operators of PDF, a line for each
-- mark. The heights are turned around here: a mark counts from the top
-- of the paper, and PDF from its bottom. A text is the numbers of its
-- glyphs in hexadecimal, four digits each.
pageContent :: Paper -> [Mark] -> BS.ByteString
pageContent paper = BS8.unlines . map operators
  where
    fromBottom y = paperHeight paper - y
    operators mark = BS8.unwords $ case mark of
      TextMark x baseline font size colour spacing text ->
        [ "BT",
          fontName font,
          number size,
          "Tf",
          fill colour,
          number spacing,
          "Tc",
          number x,
          number (fromBottom baseline),
          "Td",
          "<" <> BS.concat (hex16 <$> glyphsOf font text) <> ">",
          "Tj",
          "ET"
        ]
      RuleMark left right y thickness colour ->
        [ number thickness,
          "w",
          stroke colour,
          number left,
          number (fromBottom y),
          "m",
          number right,
          number (fromBottom y),
          "l",
          "S"
        ]
      BoxMark left top width height colour ->
        [ fill colour,
          number left,
          number (fromBottom (top + height)),
          number width,
          number height,
          "re",
          "f"
        ]
    fill (Colour red green blue) = BS8.unwords [number red, number green, number blue, "rg"]
    stroke (Colour red green blue) = BS8.unwords [number red, number green, number blue, "RG"]

-- | The name a page gives a font in its resources
fontName :: Font -> BS.ByteString
fontName Regular = "/F1"
fontName Bold = "/F2"

-- | A number as PDF reads it, to a hundredth, without the zeros that add
-- nothing: 12, 12.5, 0.25. A hundredth of a point is far below what a
-- printer can place.
number :: Double -> BS.ByteString
number value =
  BS8.pack $ sign <> show whole <> (if hundredths == 0 then "" else "." <> decimals)
  where
    scaled = round (value * 100) :: Integer
    sign = if scaled < 0 then "-" else ""
    (whole, hundredths) = abs scaled `divMod` 100
    decimals = dropTrailingZero (printf "%02d" hundredths)
    dropTrailingZero digits = if last digits == '0' then init digits else digits

-- | A number of 16 bits as four hexadecimal digits
hex16 :: Int -> BS.ByteString
hex16 value = BS.pack [hexDigit (value `shiftR` 12), hexDigit (value `shiftR` 8), hexDigit (value `shiftR` 4), hexDigit value]
  where
    hexDigit nibble = BS.index "0123456789ABCDEF" (nibble .&. 0x0F)

-- | Bytes as a string of PDF, between parentheses. A backslash and a
-- parenthesis are escaped.
literalString :: BS.ByteString -> BS.ByteString
literalString bytes = "(" <> BS.concatMap escaped bytes <> ")"
  where
    escaped byte
      | byte == 0x5C = "\\\\"
      | byte == 0x28 = "\\("
      | byte == 0x29 = "\\)"
      | otherwise = BS.singleton byte

{- The file -}

-- | The pages as the bytes of a PDF file, on the paper given, with the
-- title and the time of creation given.
--
-- The file is a list of numbered objects, then a table of where each one
-- starts, which a reader uses to jump to a page. The objects come in a
-- fixed order: the catalog, the list of the pages, the document
-- information, five objects for each of the two fonts ('fontObjects'),
-- then two objects for each page, the page and its content.
--
-- The content of a page is written as it is, not compressed. Most PDF
-- files compress it, and would be smaller. A statement is some tens of
-- kilobytes either way, and compression would bring a library with it.
-- Written plain, the pages are text a test can read.
pdfDocument :: DocumentInfo -> Paper -> [[Mark]] -> BS.ByteString
pdfDocument info paper pages =
  BSL.toStrict . Builder.toLazyByteString $
    Builder.byteString header
      <> foldMap Builder.byteString objects
      <> Builder.byteString (crossReference <> trailer)
  where
    -- The second line holds four bytes above 127, which tell a program
    -- that moves the file that it is not plain text
    header = "%PDF-1.4\n%\xE2\xE3\xCF\xD3\n"
    pageCount = length pages
    -- The numbers of the objects. The fonts come after the three fixed
    -- ones, five objects each, and the pages after the fonts, each with
    -- its content right after it.
    catalogId, pagesId, infoId :: Int
    catalogId = 1
    pagesId = 2
    infoId = 3
    fontId Regular = 4
    fontId Bold = 9
    pageId index = 14 + 2 * index
    contentId index = 15 + 2 * index
    bodies =
      [ dictionary [("/Type", "/Catalog"), ("/Pages", reference pagesId), ("/Lang", "(en)"), ("/ViewerPreferences", "<< /DisplayDocTitle true >>")],
        dictionary
          [ ("/Type", "/Pages"),
            ("/Count", BS8.pack (show pageCount)),
            ("/Kids", "[" <> BS8.unwords [reference (pageId index) | index <- [0 .. pageCount - 1]] <> "]")
          ],
        dictionary
          [ ("/Title", textString (documentTitle info)),
            ("/CreationDate", literalString (pdfDate (documentCreated info)))
          ]
      ]
        <> concat [fontObjects font (fontId font) (shownIn font) | font <- [Regular, Bold]]
        <> concat [[pageObject index, stream [] (pageContent paper marks)] | (index, marks) <- zip [0 :: Int ..] pages]
    -- The characters the pages show in a font, each with its glyph
    shownIn font =
      IntMap.fromList
        [ (glyph, char)
          | page <- pages,
            TextMark _ _ markFont _ _ _ text <- page,
            markFont == font,
            (glyph, char) <- zip (glyphsOf font text) (Text.unpack (shownText font text))
        ]
    pageObject index =
      dictionary
        [ ("/Type", "/Page"),
          ("/Parent", reference pagesId),
          ("/MediaBox", BS8.unwords ["[0 0", number (paperWidth paper), number (paperHeight paper) <> "]"]),
          ("/Resources", "<< /Font << /F1 " <> reference (fontId Regular) <> " /F2 " <> reference (fontId Bold) <> " >> >>"),
          ("/Contents", reference (contentId index))
        ]
    objects = [BS8.pack (show objectId) <> " 0 obj\n" <> body <> "\nendobj\n" | (objectId, body) <- zip [1 :: Int ..] bodies]
    -- Where each object starts, counted in bytes from the start of the
    -- file
    offsets = init (scanl (+) (BS.length header) (BS.length <$> objects))
    crossReferenceStart = BS.length header + sum (BS.length <$> objects)
    objectCount = length objects
    -- Each line of the table is twenty bytes long, as the format asks.
    -- The first one stands for the object 0, which no file has.
    crossReference =
      "xref\n0 "
        <> BS8.pack (show (objectCount + 1))
        <> "\n0000000000 65535 f \n"
        <> BS.concat [BS8.pack (printf "%010d 00000 n \n" offset) | offset <- offsets]
    trailer =
      "trailer\n"
        <> dictionary [("/Size", BS8.pack (show (objectCount + 1))), ("/Root", reference catalogId), ("/Info", reference infoId)]
        <> "\nstartxref\n"
        <> BS8.pack (show crossReferenceStart)
        <> "\n%%EOF\n"

-- | A reference to an object, by its number
reference :: Int -> BS.ByteString
reference objectId = BS8.pack (show objectId) <> " 0 R"

-- | A dictionary of PDF: names with their values, between angle brackets
dictionary :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
dictionary entries = "<< " <> BS8.unwords [key <> " " <> value | (key, value) <- entries] <> " >>"

-- | A stream of PDF: bytes with the dictionary that says how long they
-- are, and what else the entries given say of them
stream :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString -> BS.ByteString
stream entries bytes =
  dictionary (("/Length", BS8.pack (show (BS.length bytes))) : entries) <> "\nstream\n" <> bytes <> "\nendstream"

-- | A text of the document information, as a string of PDF in UTF-16,
-- written in hexadecimal. This form takes every character.
textString :: Text -> BS.ByteString
textString text = "<FEFF" <> BS.concatMap hexByte (Text.encodeUtf16BE text) <> ">"
  where
    hexByte byte = BS.drop 2 (hex16 (fromIntegral byte))

-- | A time as the document information of PDF writes it, with its offset
-- from UTC: @D:20261115143200+01'00'@
pdfDate :: ZonedTime -> BS.ByteString
pdfDate zoned =
  BS8.pack $ formatTime defaultTimeLocale "D:%Y%m%d%H%M%S" (zonedTimeToLocalTime zoned) <> offset
  where
    minutes = timeZoneMinutes (zonedTimeZone zoned)
    (hours, rest) = abs minutes `divMod` 60
    offset = printf "%s%02d'%02d'" (if minutes < 0 then "-" else "+" :: String) hours rest

{- A font in the file -}

-- | The five objects of a font, from the number given on, for the
-- glyphs the document shows in it, each with its character.
--
-- PDF asks for this chain to show a TrueType font by the numbers of its
-- glyphs. The first object is the font the pages name. It says that a
-- text is glyph numbers of two bytes, and points to the second and to
-- the fifth. The second is the font itself, with the widths of its
-- glyphs. The third describes it: its name and its sizes. The fourth is
-- the font file, a subset with the glyphs shown alone. The fifth gives
-- the character of each glyph, for a reader that searches or copies the
-- text.
--
-- The name of the font starts with six letters and a plus sign, as the
-- format asks of a subset. The letters follow from the glyphs kept, so
-- that the same document gives the same file.
fontObjects :: Font -> Int -> IntMap.IntMap Char -> [BS.ByteString]
fontObjects font firstId shown =
  [ dictionary
      [ ("/Type", "/Font"),
        ("/Subtype", "/Type0"),
        ("/BaseFont", name),
        ("/Encoding", "/Identity-H"),
        ("/DescendantFonts", "[" <> reference (firstId + 1) <> "]"),
        ("/ToUnicode", reference (firstId + 4))
      ],
    dictionary
      [ ("/Type", "/Font"),
        ("/Subtype", "/CIDFontType2"),
        ("/BaseFont", name),
        ("/CIDSystemInfo", "<< /Registry (Adobe) /Ordering (Identity) /Supplement 0 >>"),
        ("/FontDescriptor", reference (firstId + 2)),
        ("/DW", "1000"),
        ("/W", "[" <> BS8.unwords [BS8.pack (show glyph) <> " [" <> number (thousandths (advanceOf typeface glyph)) <> "]" | glyph <- IntMap.keys shown] <> "]"),
        ("/CIDToGIDMap", "/Identity")
      ],
    dictionary
      [ ("/Type", "/FontDescriptor"),
        ("/FontName", name),
        -- The flag of a font whose glyphs are letters, not symbols
        ("/Flags", "32"),
        ("/FontBBox", "[" <> BS8.unwords (number . thousandths <$> [left, bottom, right, top]) <> "]"),
        ("/ItalicAngle", "0"),
        ("/Ascent", number (thousandths (typefaceAscent typeface))),
        ("/Descent", number (thousandths (typefaceDescent typeface))),
        ("/CapHeight", number (thousandths (typefaceCapHeight typeface))),
        -- The thickness of the upright strokes, which the font file does
        -- not state: a reader uses it only when it cannot use the font
        ("/StemV", if font == Bold then "140" else "88"),
        ("/FontFile2", reference (firstId + 3))
      ],
    stream [("/Length1", BS8.pack (show (BS.length subset)))] subset,
    stream [] (toUnicodeMap font shown)
  ]
  where
    typeface = typefaceOf font
    glyphs = IntSet.fromList (IntMap.keys shown)
    subset = subsetFont typeface glyphs
    name = "/" <> subsetTag glyphs <> "+" <> postScriptName font
    (left, bottom, right, top) = typefaceBox typeface
    -- A measure of the font in thousandths of the size of a text, the
    -- unit PDF states the measures of a font in
    thousandths :: Int -> Double
    thousandths units = fromIntegral units * 1000 / fromIntegral (typefaceUnitsPerEm typeface)

-- | Six capital letters that follow from a set of glyphs: the tag of a
-- subset of a font
subsetTag :: IntSet.IntSet -> BS.ByteString
subsetTag glyphs = BS8.pack (take 6 (letters (IntSet.foldl' mixed 7 glyphs)))
  where
    mixed total glyph = (total * 31 + glyph + 1) `mod` (26 ^ (6 :: Int))
    letters value = toEnum (ord 'A' + value `mod` 26) : letters (value `div` 26)

-- | The table that gives the character of each glyph of a font, in the
-- language PDF takes it in. It is named after the font, F1 or F2. A
-- block of the table holds a hundred glyphs at most.
toUnicodeMap :: Font -> IntMap.IntMap Char -> BS.ByteString
toUnicodeMap font shown =
  BS8.unlines $
    [ "/CIDInit /ProcSet findresource begin",
      "12 dict begin",
      "begincmap",
      "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def",
      "/CMapName " <> fontName font <> "-UCS def",
      "/CMapType 2 def",
      "1 begincodespacerange",
      "<0000> <FFFF>",
      "endcodespacerange"
    ]
      <> concat
        [ [BS8.pack (show (length block)) <> " beginbfchar"]
            <> ["<" <> hex16 glyph <> "> <" <> hex16 (ord char) <> ">" | (glyph, char) <- block]
            <> ["endbfchar"]
          | block <- chunksOf 100 (IntMap.toAscList shown)
        ]
      <> [ "endcmap",
           "CMapName currentdict /CMap defineresource pop",
           "end",
           "end"
         ]
