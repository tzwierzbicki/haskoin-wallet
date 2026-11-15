{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | TrueType fonts, read for the reports of hw: which glyph draws a
-- character, how wide it is, and a smaller copy of the font that holds
-- the glyphs of one document alone.
--
-- A report carries its fonts inside its PDF file
-- ("Haskoin.Wallet.Cli.Pdf"). The file then looks the same on every
-- computer, down to the shape of each letter, and shows every script
-- the font has: Latin, Greek and Cyrillic, with their accents. A file
-- that only names a font leaves the choice of a look-alike to each
-- reader, and is held to the letters of Western Europe.
--
-- = What is read
--
-- A TrueType file is a list of tables. This module reads the few a PDF
-- needs. The table cmap says which glyph draws each character. The
-- table hmtx says how far each glyph moves the pen, which is the width
-- the layout measures a text with. The tables head, hhea and OS/2 give
-- the sizes a PDF states about a font. The outlines themselves, in
-- glyf, are not read: they are copied.
--
-- Kerning, the small shifts between pairs such as A and V, is not
-- applied. It would need the tables of positioning and a shaping
-- engine. A statement is mostly figures, which no font kerns.
--
-- = The subset
--
-- A font of 2600 glyphs is 400 kilobytes, and a statement uses a
-- hundred of them. So the PDF carries a subset ('subsetFont'): a
-- TrueType file with the outlines of the glyphs the document shows, and
-- no other. A statement with its two fonts is then some tens of
-- kilobytes.
--
-- The glyphs keep their numbers in the subset. The text of the pages
-- names the glyphs by those numbers, and a subset that renumbered them
-- would need the text rewritten to match. The glyphs left out are kept
-- as empty outlines, which cost four bytes each, and the tables stop
-- after the highest glyph used.
--
-- A glyph can be made of others: an accented letter is its letter and
-- its accent. The subset keeps the parts of every glyph it keeps
-- ('withComponents').
--
-- The other choice was a library. The ones on Hackage that subset fonts
-- bring a rendering engine or a C library with them, and this is two
-- hundred lines.
module Haskoin.Wallet.Cli.Font
  ( Typeface (..),
    readTypeface,
    glyphOf,
    advanceOf,
    subsetFont,
    withComponents,
    embedFile,
  )
where

import Data.Bits (shiftL, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Unsafe (unsafePackAddressLen)
import Data.Char (ord)
import Data.Int (Int16)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Language.Haskell.TH (Exp, Q, integerL, litE, runIO, stringPrimL)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.IO.Unsafe (unsafePerformIO)

{- A font as it is read -}

-- | A TrueType font: what the layout and the PDF writer need of it
data Typeface = Typeface
  { -- | The tables of the file, by their four-letter tags
    typefaceTables :: !(Map.Map BS.ByteString BS.ByteString),
    -- | How many units the font divides the size of a text into: a width
    -- of 1139 units in a font of 2048 is 0.556 of the size
    typefaceUnitsPerEm :: !Int,
    typefaceGlyphCount :: !Int,
    -- | How many glyphs have a width of their own in hmtx. The glyphs
    -- after them all have the width of the last one.
    typefaceWidthCount :: !Int,
    -- | The glyph of each character the font has, by the code of the
    -- character
    typefaceGlyphs :: !(IntMap.IntMap Int),
    -- | The box that holds every glyph: left, bottom, right, top, in
    -- units
    typefaceBox :: !(Int, Int, Int, Int),
    -- | How far the letters reach above the baseline and below it, in
    -- units: the second is below zero
    typefaceAscent :: !Int,
    typefaceDescent :: !Int,
    -- | The height of a capital letter, in units
    typefaceCapHeight :: !Int
  }

-- | A font read from the bytes of its file. A file that is not a
-- TrueType font with outlines, or that lacks a table read here, is
-- refused: the fonts of hw are compiled into it, so a refusal is a
-- mistake in the build, and says which.
readTypeface :: BS.ByteString -> Either String Typeface
readTypeface file = do
  if word32 file 0 == 0x00010000 then Right () else Left "not a TrueType font with outlines"
  let tableCount = word16 file 4
      tables =
        Map.fromList
          [ (BS.take 4 (BS.drop record file), BS.take (word32 file (record + 12)) (BS.drop (word32 file (record + 8)) file))
            | index <- [0 .. tableCount - 1],
              let record = 12 + 16 * index
          ]
      table tag = maybe (Left ("no table " <> show tag)) Right (Map.lookup tag tables)
  head' <- table "head"
  hhea <- table "hhea"
  maxp <- table "maxp"
  os2 <- table "OS/2"
  cmap <- table "cmap"
  mapM_ table ["hmtx", "loca", "glyf"]
  glyphs <- characterMap cmap
  return
    Typeface
      { typefaceTables = tables,
        typefaceUnitsPerEm = word16 head' 18,
        typefaceGlyphCount = word16 maxp 4,
        typefaceWidthCount = word16 hhea 34,
        typefaceGlyphs = glyphs,
        typefaceBox = (int16 head' 36, int16 head' 38, int16 head' 40, int16 head' 42),
        typefaceAscent = int16 hhea 4,
        typefaceDescent = int16 hhea 6,
        -- The height of the capitals is in the table from its version 2
        -- on. An older table gives the ascent in its place.
        typefaceCapHeight = if word16 os2 0 >= 2 then int16 os2 88 else int16 hhea 4
      }

-- | The glyph of each character, from the table cmap: its subtable for
-- Unicode on Windows, in the format 4, which every font has for the
-- characters of the first 65536. The format lists runs of characters,
-- each with the shift that gives their glyphs, or with the place of a
-- list of them.
characterMap :: BS.ByteString -> Either String (IntMap.IntMap Int)
characterMap cmap =
  case [offset | index <- [0 .. word16 cmap 2 - 1], let record = 4 + 8 * index, word16 cmap record == 3, word16 cmap (record + 2) == 1, let offset = word32 cmap (record + 4), word16 cmap offset == 4] of
    [] -> Left "no Unicode character map in the format 4"
    start : _ ->
      let segments = word16 cmap (start + 6) `div` 2
          ends = start + 14
          starts = ends + 2 * segments + 2
          deltas = starts + 2 * segments
          ranges = deltas + 2 * segments
          glyphsOfSegment segment =
            let first = word16 cmap (starts + 2 * segment)
                lastCode = word16 cmap (ends + 2 * segment)
                delta = word16 cmap (deltas + 2 * segment)
                rangePlace = ranges + 2 * segment
                range = word16 cmap rangePlace
                glyph code
                  | range == 0 = (code + delta) .&. 0xFFFF
                  | otherwise = case word16 cmap (rangePlace + range + 2 * (code - first)) of
                      0 -> 0
                      listed -> (listed + delta) .&. 0xFFFF
             in [(code, glyph code) | code <- [first .. lastCode], code /= 0xFFFF, glyph code /= 0]
       in Right (IntMap.fromList (concatMap glyphsOfSegment [0 .. segments - 1]))

-- | The glyph that draws a character, or nothing when the font does not
-- have the character
glyphOf :: Typeface -> Char -> Maybe Int
glyphOf typeface char = IntMap.lookup (ord char) (typefaceGlyphs typeface)

-- | How far a glyph moves the pen, in units of the font
advanceOf :: Typeface -> Int -> Int
advanceOf typeface glyph = word16 (tableOf typeface "hmtx") (4 * min glyph (typefaceWidthCount typeface - 1))

-- | A table of a font, or no bytes for one it does not have
tableOf :: Typeface -> BS.ByteString -> BS.ByteString
tableOf typeface tag = Map.findWithDefault BS.empty tag (typefaceTables typeface)

{- Reading numbers -}

-- The numbers of a font file are big-endian. A place past the end of the
-- bytes reads as zero: the fonts read here are those hw is built with,
-- and the tests check what is read from them.

word16 :: BS.ByteString -> Int -> Int
word16 bytes place = byteAt bytes place `shiftL` 8 + byteAt bytes (place + 1)

word32 :: BS.ByteString -> Int -> Int
word32 bytes place = word16 bytes place `shiftL` 16 + word16 bytes (place + 2)

-- | A number of 16 bits that may be below zero
int16 :: BS.ByteString -> Int -> Int
int16 bytes place = fromIntegral (fromIntegral (word16 bytes place) :: Int16)

byteAt :: BS.ByteString -> Int -> Int
byteAt bytes place
  | place >= 0 && place < BS.length bytes = fromIntegral (BS.index bytes place)
  | otherwise = 0

{- The subset -}

-- | The bytes of a glyph in the table of outlines: none for a glyph
-- without an outline, such as the space
glyphData :: Typeface -> Int -> BS.ByteString
glyphData typeface glyph = BS.take (end - start) (BS.drop start (tableOf typeface "glyf"))
  where
    loca = tableOf typeface "loca"
    -- The table of places is in the long format when the header says
    -- so, and otherwise holds half of each place in 16 bits
    long = word16 (tableOf typeface "head") 50 == 1
    place index = if long then word32 loca (4 * index) else 2 * word16 loca (2 * index)
    start = place glyph
    end = place (glyph + 1)

-- | The glyphs a glyph is made of, when it is made of others. Such a
-- glyph starts with a count of contours below zero, then lists its
-- parts. Each part has flags, the number of its glyph, and where to put
-- it, in a size the flags tell.
componentsOf :: Typeface -> Int -> [Int]
componentsOf typeface glyph
  | BS.length outline >= 10 && int16 outline 0 < 0 = parts 10
  | otherwise = []
  where
    outline = glyphData typeface glyph
    parts place =
      let flags = word16 outline place
          -- Two numbers that place the part, of 16 bits or of 8
          placing = if flags .&. 0x0001 /= 0 then 4 else 2
          -- A scale, two scales or a matrix of four numbers
          scaling
            | flags .&. 0x0008 /= 0 = 2
            | flags .&. 0x0040 /= 0 = 4
            | flags .&. 0x0080 /= 0 = 8
            | otherwise = 0
          more = flags .&. 0x0020 /= 0
       in word16 outline (place + 2) : (if more then parts (place + 4 + placing + scaling) else [])

-- | Glyphs with the glyphs they are made of, and those these are made
-- of in turn, and the glyph 0, which a reader draws for a missing one
withComponents :: Typeface -> IntSet.IntSet -> IntSet.IntSet
withComponents typeface = grow . IntSet.insert 0
  where
    grow glyphs =
      let parts = IntSet.fromList (concatMap (componentsOf typeface) (IntSet.toList glyphs))
          larger = IntSet.union glyphs parts
       in if IntSet.size larger == IntSet.size glyphs then glyphs else grow larger

-- | A TrueType file with the outlines of the glyphs given, and of their
-- parts, and no other. The glyphs keep their numbers. The tables of
-- outlines, of places and of widths stop after the highest glyph kept.
--
-- The file has the tables a PDF reader draws from: the header, the
-- sizes, the widths, the places, the outlines, and the three tables of
-- instructions that fit the outlines to a grid of pixels. It also has
-- the names, a short table of the font's measures, and two tables with
-- nothing in them, of characters and of glyph names. A PDF reader finds
-- the glyphs by their numbers and needs none of these, but some refuse
-- a font without them.
subsetFont :: Typeface -> IntSet.IntSet -> BS.ByteString
subsetFont typeface used = fontFile (Map.toAscList tables)
  where
    kept = withComponents typeface used
    glyphCount = IntSet.findMax kept + 1
    -- The outlines one after the other, each on a multiple of four
    -- bytes, and the place of each: a glyph left out takes no room
    outlines = [if IntSet.member glyph kept then padded (glyphData typeface glyph) else BS.empty | glyph <- [0 .. glyphCount - 1]]
    places = scanl (+) 0 (BS.length <$> outlines)
    widthCount = min (typefaceWidthCount typeface) glyphCount
    hmtx = tableOf typeface "hmtx"
    tables =
      Map.fromList $
        [ ("glyf", BS.concat outlines),
          ("loca", built (foldMap (Builder.word32BE . fromIntegral) places)),
          -- The places are written in the long format
          ("head", patched16 50 1 (patched32 8 0 (tableOf typeface "head"))),
          ("hhea", patched16 34 widthCount (tableOf typeface "hhea")),
          ("maxp", patched16 4 glyphCount (tableOf typeface "maxp")),
          -- A width and a margin for each glyph that has its own width,
          -- then the margin alone of the glyphs after them
          ("hmtx", BS.take (4 * widthCount) hmtx <> BS.take (2 * (glyphCount - widthCount)) (BS.drop (4 * typefaceWidthCount typeface) hmtx)),
          -- The format 3 of the table of glyph names has no name: its
          -- header alone, with the measures of the underline
          ("post", patched32 0 0x00030000 (BS.take 32 (tableOf typeface "post"))),
          ("cmap", emptyCharacterMap)
        ]
          <> [(tag, table) | tag <- ["cvt ", "fpgm", "prep", "OS/2", "name"], Just table <- [Map.lookup tag (typefaceTables typeface)]]

-- | A table of characters that maps none: one run that ends the list, as
-- the format 4 asks. The PDF names the glyphs by their numbers.
emptyCharacterMap :: BS.ByteString
emptyCharacterMap =
  built . foldMap Builder.word16BE $
    -- The table: version 0, one subtable, for Unicode on Windows, 12
    -- bytes in
    [0, 1, 3, 1, 0, 12]
      -- The subtable: format 4, 24 bytes long, language 0, one run, and
      -- the three numbers that speed up a search of the runs
      <> [4, 24, 0, 2, 2, 0, 0]
      -- The run: it ends at 0xFFFF, a padding, starts at 0xFFFF, shifts
      -- by 1, and has no list
      <> [0xFFFF, 0, 0xFFFF, 1, 0]

-- | The tables as a font file: the header, the list of the tables with
-- the place, the length and the checksum of each, then the tables, each
-- on a multiple of four bytes. The header of the font holds a checksum
-- of the whole file, worked out last.
fontFile :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
fontFile tables = patched32 adjustmentPlace adjustment file
  where
    tableCount = length tables
    -- The largest power of two that is at most the number of tables
    power = last (takeWhile (\exponent' -> 2 ^ exponent' <= tableCount) [0 :: Int ..])
    directoryLength = 12 + 16 * tableCount
    starts = scanl (+) directoryLength (BS.length . padded . snd <$> tables)
    file =
      built $
        Builder.word32BE 0x00010000
          <> foldMap (Builder.word16BE . fromIntegral) [tableCount, 16 * 2 ^ power, power, 16 * tableCount - 16 * 2 ^ power]
          <> mconcat
            [ Builder.byteString tag <> foldMap Builder.word32BE [checksum table, fromIntegral start, fromIntegral (BS.length table)]
              | ((tag, table), start) <- zip tables starts
            ]
          <> foldMap (Builder.byteString . padded . snd) tables
    -- The checksum of the file is kept in the header of the font, 8
    -- bytes into the table head, as what brings the sum of the file to
    -- a fixed number
    adjustmentPlace = sum [start + 8 | ((tag, _), start) <- zip tables starts, tag == "head"]
    adjustment = fromIntegral (0xB1B0AFBA - checksum file)

-- | The sum of the bytes of a table taken four at a time as numbers of
-- 32 bits, which wraps around: the checksum of the format
checksum :: BS.ByteString -> Word32
checksum = go 0 . padded
  where
    go total bytes
      | BS.null bytes = total
      | otherwise = go (total + fromIntegral (word32 bytes 0)) (BS.drop 4 bytes)

-- | Bytes with zeros added up to a multiple of four
padded :: BS.ByteString -> BS.ByteString
padded bytes = bytes <> BS.replicate ((4 - BS.length bytes `mod` 4) `mod` 4) 0

-- | Bytes with a number of 16 bits, or of 32, written at a place
patched16, patched32 :: Int -> Int -> BS.ByteString -> BS.ByteString
patched16 place number bytes = BS.take place bytes <> built (Builder.word16BE (fromIntegral number)) <> BS.drop (place + 2) bytes
patched32 place number bytes = BS.take place bytes <> built (Builder.word32BE (fromIntegral number)) <> BS.drop (place + 4) bytes

built :: Builder.Builder -> BS.ByteString
built = BSL.toStrict . Builder.toLazyByteString

{- Fonts compiled into hw -}

-- | The bytes of a file, read as hw is compiled and kept inside the
-- program: a splice of Template Haskell. The fonts of the reports come
-- with hw this way ("Haskoin.Wallet.Cli.Typefaces"). A font file
-- installed beside the program would be lost the day the program is
-- copied alone to another computer, and a report would then fail there.
--
-- The path is read from the folder of the package, where the build
-- runs. The compiler is told that the module depends on the file, and
-- compiles it again when the file changes.
embedFile :: FilePath -> Q Exp
embedFile path = do
  addDependentFile path
  bytes <- runIO (BS.readFile path)
  [|unsafePerformIO (unsafePackAddressLen $(litE (integerL (fromIntegral (BS.length bytes)))) $(litE (stringPrimL (BS.unpack bytes))))|]
