{-# LANGUAGE OverloadedStrings #-}

-- | The PDF files of the reports: how a text is shown and measured, how
-- a mark of a page is drawn, the fonts a file carries, and the parts a
-- PDF reader needs to open the file.
--
-- The glyph numbers and the widths stated here are those of the fonts
-- in the folder fonts, read apart from hw with a script. A digit is 1139
-- units wide where the size of a text is 2048 units.
module Haskoin.Wallet.Report.PdfSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone (..), ZonedTime (..), fromGregorian)
import Haskoin.Wallet.Report.Pdf
import Haskoin.Wallet.TestUtils
import Test.Hspec

-- | Black, and a mid grey
black, midGrey :: Colour
black = Colour 0 0 0
midGrey = Colour 0.5 0.5 0.5

-- | A width in hundredths of a point, rounded: the widths are sums of
-- fractions, and are compared to the hundredth
hundredths :: Double -> Integer
hundredths width = round (width * 100)

-- | 15 Nov 2026 at 14:32:07 on a clock of Central Europe in winter
madeAt :: ZonedTime
madeAt = ZonedTime (LocalTime (fromGregorian 2026 11 15) (TimeOfDay 14 32 7)) cet

-- | A file of two pages, titled "Bal". The first page says "First page"
-- in the regular font, and the second "Second page" in bold.
twoPages :: BS.ByteString
twoPages =
  pdfDocument
    (DocumentInfo "Bal" madeAt)
    a4
    [ [TextMark 56.7 100 Regular 8.5 black 0 "First page"],
      [TextMark 56.7 100 Bold 8.5 black 0 "Second page"]
    ]

-- | Whether the bytes hold the others
holds :: BS.ByteString -> BS.ByteString -> Bool
holds part whole = part `BS.isInfixOf` whole

-- | How many times the bytes hold the others
countOf :: BS.ByteString -> BS.ByteString -> Int
countOf part whole = case BS.breakSubstring part whole of
  (_, rest) | BS.null rest -> 0
  (_, rest) -> 1 + countOf part (BS.drop (BS.length part) rest)

-- | The number on the line after the word startxref: where the file says
-- its table of objects starts
tableStart :: BS.ByteString -> Int
tableStart file = case BS8.lines (snd (BS.breakSubstring "startxref\n" file)) of
  _ : place : _ -> read (BS8.unpack place)
  _ -> 0

-- | The places the table of a file gives for its objects, from the
-- object 1 on: the first ten digits of each line that ends with n
objectStarts :: BS.ByteString -> [Int]
objectStarts file =
  [ read (BS8.unpack (BS.take 10 line))
    | line <- BS8.lines (BS.drop (tableStart file) file),
      " 00000 n " `BS.isSuffixOf` line
  ]

-- | For each stream of a file, whether the bytes that follow it, at the
-- length the file states for it, are the end of the stream
streamsEndWhereStated :: BS.ByteString -> [Bool]
streamsEndWhereStated file = case BS.breakSubstring "<< /Length " file of
  (_, rest)
    | BS.null rest -> []
    | otherwise ->
        let stated = read (BS8.unpack (BS8.takeWhile (/= ' ') (BS.drop 11 rest)))
            content = BS.drop 8 (snd (BS.breakSubstring "\nstream\n" rest))
         in ("\nendstream" `BS.isPrefixOf` BS.drop stated content) : streamsEndWhereStated (BS.drop stated content)

spec :: Spec
spec = do
  describe "A text as a mark shows it" $ do
    it "keeps the characters the font has, accented and Cyrillic letters among them" $
      -- "Caf" and e with an acute accent, then "Zarplata" in Cyrillic
      shownText Regular "Caf\233 \1047\1072\1088\1087\1083\1072\1090\1072" `shouldBe` "Caf\233 \1047\1072\1088\1087\1083\1072\1090\1072"

    it "takes a letter followed by a combining accent for the one accented letter" $
      -- e, then the combining acute accent U+0301
      shownText Regular "e\769" `shouldBe` "\233"

    it "writes a question mark for a character the font does not have" $
      -- Two Japanese characters
      shownText Regular "Tokyo \26085\26412" `shouldBe` "Tokyo ??"

    it "writes a line break and a tab as spaces, since a mark is one line" $
      shownText Regular "a\nb\tc" `shouldBe` "a b c"

  describe "The glyphs of a text" $ do
    it "are those the font has for its characters, one each" $
      glyphsOf Regular "Rent" `shouldBe` [53, 72, 81, 87]

    it "are that of the question mark for a character the font does not have" $
      glyphsOf Regular "\26085" `shouldBe` [34]

  describe "The width of a text" $ do
    -- Nine digits of 1139 units and a full stop of 569 are 10820 units:
    -- at 10 points, 10820 x 10 / 2048 = 52.83 points
    it "adds up the widths of its characters at its size" $
      hundredths (textWidth Regular 10 0 "0.00050000") `shouldBe` 5283

    it "is the same for every digit, so that amounts line up" $
      -- 4 x 1139 x 10 / 2048 = 22.25 points
      (hundredths (textWidth Regular 10 0 "1111"), hundredths (textWidth Regular 10 0 "8888")) `shouldBe` (2225, 2225)

    -- "Total" is 4553 units in the regular font and 4892 in bold:
    -- 22.23 and 23.89 points at 10 points
    it "is larger in bold" $
      (hundredths (textWidth Regular 10 0 "Total"), hundredths (textWidth Bold 10 0 "Total")) `shouldBe` (2223, 2389)

    it "grows with the room added after each character" $
      -- A and B are 1366 units each: 13.34 points, and 1 point after each
      hundredths (textWidth Regular 10 1 "AB") `shouldBe` 1534

    it "counts a character the font does not have as the question mark written for it" $
      -- A question mark is 1139 units: 5.56 points at 10 points
      hundredths (textWidth Regular 10 0 "\26085") `shouldBe` 556

  describe "A mark of a page as PDF draws it" $ do
    -- A4 is 841.89 points high: 100 points from the top is 741.89 from
    -- the bottom. The glyphs of "Rent" are 53, 72, 81 and 87: 0035,
    -- 0048, 0051 and 0057 in hexadecimal.
    it "is a line of text with its font, its size, its colour, its place counted from the bottom of the paper, and the numbers of its glyphs" $
      pageContent a4 [TextMark 56.7 100 Regular 8.5 black 0 "Rent"]
        `shouldBe` "BT /F1 8.5 Tf 0 0 0 rg 0 Tc 56.7 741.89 Td <0035004800510057> Tj ET\n"

    it "names the bold font and the room between the characters of a title" $
      pageContent a4 [TextMark 56.7 100 Bold 7.5 midGrey 1.2 "Rent"]
        `shouldBe` "BT /F2 7.5 Tf 0.5 0.5 0.5 rg 1.2 Tc 56.7 741.89 Td <0035004800510057> Tj ET\n"

    it "is a rule of its thickness and colour from its left end to its right end" $
      pageContent a4 [RuleMark 56.7 538.58 100 0.5 midGrey]
        `shouldBe` "0.5 w 0.5 0.5 0.5 RG 56.7 741.89 m 538.58 741.89 l S\n"

    -- A box 3 points high whose top is 42 points down ends 45 points
    -- down: 841.89 - 45 = 796.89 from the bottom
    it "is a filled box, placed by its bottom left corner" $
      pageContent a4 [BoxMark 56.7 42 481.88 3 black]
        `shouldBe` "0 0 0 rg 56.7 796.89 481.88 3 re f\n"

    -- The glyph of A is 36, 0024, and that of the digit 0 is 19, 0013
    it "has a line for each mark, in their order" $
      pageContent a4 [TextMark 0 0 Regular 10 black 0 "A", TextMark 0 0 Regular 10 black 0 "0"]
        `shouldBe` "BT /F1 10 Tf 0 0 0 rg 0 Tc 0 841.89 Td <0024> Tj ET\nBT /F1 10 Tf 0 0 0 rg 0 Tc 0 841.89 Td <0013> Tj ET\n"

  describe "A time in the information of a PDF file" $ do
    it "is written with its offset from UTC" $
      pdfDate madeAt `shouldBe` "D:20261115143207+01'00'"

    it "has a minus sign and the minutes of a clock behind UTC by hours and a half" $
      -- Newfoundland in winter is three hours and a half behind UTC
      pdfDate (ZonedTime (LocalTime (fromGregorian 2026 1 5) (TimeOfDay 9 0 0)) (TimeZone (-210) False "NST"))
        `shouldBe` "D:20260105090000-03'30'"

  describe "A PDF file" $ do
    it "starts with the header of PDF and ends with the end-of-file mark" $
      (BS.take 9 twoPages, BS.drop (BS.length twoPages - 6) twoPages) `shouldBe` ("%PDF-1.4\n", "%%EOF\n")

    it "says where its table of objects starts" $
      BS.take 5 (BS.drop (tableStart twoPages) twoPages) `shouldBe` "xref\n"

    -- Three objects for the catalog, the list of pages and the
    -- information, five for each of the two fonts, then a page and its
    -- content for each page: 3 + 10 + 4 = 17
    it "says in its table where each of its seventeen objects starts" $
      [BS8.takeWhile (/= '\n') (BS.drop start twoPages) | start <- objectStarts twoPages]
        `shouldBe` [BS8.pack (show number <> " 0 obj") | number <- [1 :: Int .. 17]]

    it "counts its pages, and each is of the size of the paper" $
      (holds "/Count 2" twoPages, length (filter (holds "/Type /Page /Parent 2 0 R /MediaBox [0 0 595.28 841.89]") (BS8.lines twoPages)))
        `shouldBe` (True, 2)

    it "says how long each of its streams is" $
      streamsEndWhereStated twoPages `shouldBe` replicate 6 True

    it "holds what each page says, which is read back through the tables of its fonts" $
      pdfTexts twoPages `shouldBe` ["First page", "Second page"]

    it "gives its title and the time it was made" $
      -- The title is in UTF-16: FEFF, then B, a and l as 0042, 0061 and
      -- 006C
      (holds "/Title <FEFF00420061006C>" twoPages, holds "/CreationDate (D:20261115143207+01'00')" twoPages) `shouldBe` (True, True)

    it "names no program that made it" $
      (holds "/Producer" twoPages, holds "/Creator" twoPages) `shouldBe` (False, False)

  describe "The fonts of a PDF file" $ do
    it "are the regular font and the bold one, each named as a subset" $
      (countOf "+LiberationSans " twoPages, countOf "+LiberationSans-Bold " twoPages) `shouldBe` (3, 3)

    it "are carried in the file, each as a TrueType font" $
      -- A TrueType file starts with the bytes 0, 1, 0, 0
      countOf (BS.pack ([0x0A] <> BS.unpack "stream" <> [0x0A, 0, 1, 0, 0])) twoPages `shouldBe` 2

    it "are shown by the numbers of their glyphs" $
      (countOf "/Subtype /CIDFontType2" twoPages, countOf "/Encoding /Identity-H" twoPages, countOf "/CIDToGIDMap /Identity" twoPages) `shouldBe` (2, 2, 2)

    -- The glyph of F is 41, 0029, and F is the character 0046
    it "come with the character of each glyph shown, for a reader that searches the text" $
      holds "<0029> <0046>\n" twoPages `shouldBe` True

    -- A glyph of 1139 units in a font of 2048 is 556.15 thousandths of
    -- the size: the "a" of "page", the glyph 68
    it "state the width of each glyph shown, in thousandths of the size" $
      holds " 68 [556.15] " twoPages `shouldBe` True

    -- The two font files are 825 kilobytes together
    it "hold the glyphs the pages show and no other, so that the file stays small" $
      BS.length twoPages `shouldSatisfy` (< 40000)
