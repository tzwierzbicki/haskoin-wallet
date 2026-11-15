{-# LANGUAGE OverloadedStrings #-}

-- | The fonts of the reports as hw reads them, and the subset of a font
-- a PDF file carries.
--
-- The glyph numbers and the widths stated here are those of the files
-- in the folder fonts, read apart from hw with a script that walks the
-- tables of the files.
module Haskoin.Wallet.Cli.FontSpec (spec) where

import Data.Bits (shiftL)
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet
import Data.Word (Word32)
import Haskoin.Wallet.Cli.Font
import Haskoin.Wallet.Cli.Typefaces (boldTypeface, regularTypeface)
import Test.Hspec

-- | The subset of the regular font with the glyph of "a" alone, which is
-- the glyph 68
subsetOfA :: BS.ByteString
subsetOfA = subsetFont regularTypeface (IntSet.singleton 68)

-- | The tags of the tables of a font file, as its header lists them
tableTags :: BS.ByteString -> [BS.ByteString]
tableTags file = [BS.take 4 (BS.drop (12 + 16 * index) file) | index <- [0 .. fromIntegral (BS.index file 5) - 1]]

-- | The sum of a file taken four bytes at a time as numbers of 32 bits,
-- wrapping around
wordSum :: BS.ByteString -> Word32
wordSum bytes
  | BS.null bytes = 0
  | otherwise = sum [fromIntegral byte `shiftL` shift | (byte, shift) <- zip (BS.unpack (BS.take 4 bytes)) [24, 16, 8, 0]] + wordSum (BS.drop 4 bytes)

spec :: Spec
spec = do
  describe "A font of the reports" $ do
    it "divides the size of a text into 2048 units, and has 2620 glyphs" $
      (typefaceUnitsPerEm regularTypeface, typefaceGlyphCount regularTypeface, typefaceGlyphCount boldTypeface) `shouldBe` (2048, 2620, 2620)

    it "has a glyph for a digit, for a small letter and for a capital" $
      [glyphOf regularTypeface char | char <- "0aA"] `shouldBe` [Just 19, Just 68, Just 36]

    -- e with an acute accent, the Cyrillic Zhe and the euro sign
    it "has the letters of Western Europe, Cyrillic letters and the euro sign" $
      [glyphOf regularTypeface char | char <- "\233\1046\8364"] `shouldBe` [Just 171, Just 967, Just 2088]

    it "has no glyph for a Japanese character" $
      glyphOf regularTypeface '\26085' `shouldBe` Nothing

    -- A digit and an "a" are 1139 units wide, a space 569
    it "gives every digit the same width" $
      [advanceOf regularTypeface glyph | glyph <- [19 .. 28]] `shouldBe` replicate 10 1139

    it "gives a space half the width of a digit" $
      advanceOf regularTypeface 3 `shouldBe` 569

    -- A capital A is 1366 units wide in the regular font and 1479 in bold
    it "is wider in bold" $
      (advanceOf regularTypeface 36, advanceOf boldTypeface 36) `shouldBe` (1366, 1479)

    it "is refused when its bytes are not those of a TrueType font" $
      either id (const "read") (readTypeface "not a font") `shouldBe` "not a TrueType font with outlines"

  describe "The glyphs a subset keeps" $ do
    it "are the ones asked for, and the glyph 0 that stands for a missing one" $
      IntSet.toList (withComponents regularTypeface (IntSet.fromList [68])) `shouldBe` [0, 68]

    -- e with an acute accent, the glyph 171, is made of the glyph of e,
    -- 72, and of the glyph of the accent, 118
    it "include the letter and the accent an accented letter is made of" $
      IntSet.toList (withComponents regularTypeface (IntSet.fromList [171])) `shouldBe` [0, 72, 118, 171]

  describe "The subset of a font" $ do
    it "is a TrueType file" $
      BS.take 4 subsetOfA `shouldBe` BS.pack [0, 1, 0, 0]

    it "has the tables a PDF reader draws from, in the order of their names" $
      tableTags subsetOfA `shouldBe` ["OS/2", "cmap", "cvt ", "fpgm", "glyf", "head", "hhea", "hmtx", "loca", "maxp", "name", "post", "prep"]

    -- The format asks that the numbers of 32 bits of a font file add up
    -- to this one
    it "adds up to the checksum the format asks of a font file" $
      wordSum subsetOfA `shouldBe` 0xB1B0AFBA

    it "stops after the highest glyph it keeps" $
      either (const 0) typefaceGlyphCount (readTypeface subsetOfA) `shouldBe` 69

    it "keeps the width of a glyph it keeps" $
      either (const 0) (`advanceOf` 68) (readTypeface subsetOfA) `shouldBe` 1139

    -- The font file is 410820 bytes
    it "is a small part of the font" $
      BS.length subsetOfA `shouldSatisfy` (< 20000)
