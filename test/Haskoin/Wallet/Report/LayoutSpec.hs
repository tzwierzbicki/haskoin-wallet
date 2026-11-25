{-# LANGUAGE OverloadedStrings #-}

-- | The layout of the reports: how a text is cut into lines, what every
-- page holds at its top and at its foot, and how the blocks of a report
-- are placed down the pages.
module Haskoin.Wallet.Report.LayoutSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Report.Pdf (Colour (..), Font (..), Mark (..), textWidth)
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.TestUtils (marksOf, textsOf)
import Test.Hspec

-- The pages are A4, 595.28 by 841.89 points, with margins of 56.7
-- points on the left and on the right. So what a page holds ends 538.58
-- points from the left. The rule of the footer is 56 points above the
-- bottom of the paper, 785.89 points down.
--
-- The blocks start 150 points down the first page, and may reach down
-- to 767.89 points. A table of one-line rows takes 18 points for the
-- titles of its columns, 18 for each row, and 8 below its last row.
--
-- The first page starts with four texts: the kind of document, the
-- title, the line below it, and the name of the logo. The blocks come
-- after them, so a test of a block skips four texts.

-- | A report with the blocks given, titled as a balance statement
reportOf :: [Block] -> Report
reportOf = Report "Statement of cryptocurrency holdings" "Digital Asset Balance Statement" "As of 31 December 2025" "Digital Asset Balance Statement, as of 31 December 2025"

-- | The first page of a report with the blocks given
firstPage :: [Block] -> [Mark]
firstPage = concat . take 1 . reportPages . reportOf

-- | Where a line of text ends, in hundredths of a point from the left
-- of the page
rightEnd :: Mark -> Integer
rightEnd mark = round ((textX mark + textWidth (textFont mark) (textSize mark) (textSpacing mark) (textOf mark)) * 100)

-- | A table of addresses and balances with the rows given
addressTable :: [Row] -> Block
addressTable = TableBlock . Table [Column "Address" 3 AlignLeft, Column "Balance" 1 AlignRight]

-- | A table of a hundred rows, named row 1 to row 100, each with the
-- balance 0.00050000
hundredRows :: Block
hundredRows = addressTable [Row BodyRow [plainCell ("row " <> Text.pack (show number)), plainCell "0.00050000"] | number <- [1 :: Int .. 100]]

-- | The names of the rows of 'hundredRows' on a page
rowNames :: [Mark] -> [Text]
rowNames = filter (Text.isPrefixOf "row ") . textsOf

spec :: Spec
spec = do
  -- In the regular font at 10 points an "a" is 5.56 points wide and a
  -- space 2.78. So "aaaa" is 22.25 points, "aaaa aaaa" is 47.27, and
  -- "aaaa aaaa aaaa" is 72.29.
  describe "A text cut into lines" $ do
    it "stays on one line when it fits" $
      wrapText Regular 10 80 "aaaa aaaa aaaa" `shouldBe` ["aaaa aaaa aaaa"]

    it "goes on to the next line at a space when it is too wide" $
      wrapText Regular 10 50 "aaaa aaaa aaaa" `shouldBe` ["aaaa aaaa", "aaaa"]

    -- Five "a" are 27.8 points and six are 33.36
    it "cuts a word wider than the line where it stops fitting, and drops none of it" $
      wrapText Regular 10 30 "aaaaaaaaaaaa" `shouldBe` ["aaaaa", "aaaaa", "aa"]

    it "has no line for a text without a word" $
      wrapText Regular 10 80 "  " `shouldBe` []

  describe "The first page of a report" $ do
    it "starts with the kind of document in capitals, the title and the line below it" $
      take 3 (textsOf (firstPage [])) `shouldBe` ["STATEMENT OF CRYPTOCURRENCY HOLDINGS", "Digital Asset Balance Statement", "As of 31 December 2025"]

    it "is there even for a report without a block" $
      length (reportPages (reportOf [])) `shouldBe` 1

  -- The logo is the bits of the name hw as squares of 4.4 points, with
  -- the name after them. h is 0x68, 0110 1000, and w is 0x77, 0111 0111.
  describe "The logo of a report" $ do
    -- Dark blue is a set bit, and the light grey of the hairlines a
    -- clear one
    it "draws the bits of h, then those of w, as squares: dark blue for a set bit, light grey for a clear one" $
      [ case colour of
          Colour 0.09 0.16 0.30 -> '1'
          Colour 0.83 0.85 0.88 -> '0'
          _ -> '?'
        | BoxMark _ _ 4.4 4.4 colour <- firstPage []
      ]
        `shouldBe` "0110100001110111"

    -- The kind of document is on the line 70 points down. A square is
    -- 4.4 points high, so the row of w starts at 65.6 to stand on that
    -- line. The row of h starts 6.2 points higher, at 59.4.
    it "has a row of eight squares for each letter, the last one standing on the line of the kind of document" $
      [(hundredths, length [() | BoxMark _ top 4.4 4.4 _ <- firstPage [], round (top * 100) == hundredths]) | hundredths <- [5940, 6560 :: Integer]]
        `shouldBe` [(5940, 8), (6560, 8)]

    it "has the name hw after the squares, on the line of the kind of document" $
      textBaseline <$> (marksOf "hw" (firstPage []) <> marksOf "STATEMENT OF CRYPTOCURRENCY HOLDINGS" (firstPage [])) `shouldBe` [70, 70]

    it "ends at the right edge of the page" $
      rightEnd <$> marksOf "hw" (firstPage []) `shouldBe` [53858]

    -- The right edge of the last square of a row is the one furthest to
    -- the right
    it "keeps its squares to the left of its name" $
      maximum [left + 4.4 | BoxMark left _ 4.4 4.4 _ <- firstPage []] `shouldSatisfy` (< maximum (textX <$> marksOf "hw" (firstPage [])))

    -- 'hundredRows' takes three pages
    it "is on the first page only" $
      [length (marksOf "hw" page) | page <- reportPages (reportOf [hundredRows])] `shouldBe` [1, 0, 0]

  describe "The foot of a page" $ do
    it "names the report, and the page among how many" $
      drop 4 (textsOf (firstPage [])) `shouldBe` ["Digital Asset Balance Statement, as of 31 December 2025", "Page 1 of 1"]

    -- A row is 18 points high. The first page has 33 of them below its
    -- title block, and each page after it 37: 33 + 37 + 30 = 100.
    it "counts the pages of a report that needs several" $
      [last (textsOf page) | page <- reportPages (reportOf [hundredRows])] `shouldBe` ["Page 1 of 3", "Page 2 of 3", "Page 3 of 3"]

    it "ends the page number at the right edge of the page" $
      rightEnd <$> marksOf "Page 1 of 1" (firstPage []) `shouldBe` [53858]

    it "is below everything else on the page" $
      maximum [textBaseline mark | page <- reportPages (reportOf [hundredRows]), mark <- page, isRow mark] `shouldSatisfy` (< 785.89)

  describe "A table" $ do
    it "starts with the titles of its columns in capitals" $
      take 2 (drop 4 (textsOf (firstPage [hundredRows]))) `shouldBe` ["ADDRESS", "BALANCE"]

    -- The table is 482 points wide, so the second column is 82 points
    -- wide, of which 12 are kept clear: 70 points. The title is 83
    -- points wide in capitals, so its two words do not fit on one line.
    it "writes the title of a column on two lines when it is wider than its column, both ending at the right edge of the column" $ do
      let page = firstPage [TableBlock (Table [Column "Description" 400 AlignLeft, Column "Balance (BTCTEST4)" 82 AlignRight] [Row BodyRow [plainCell "a", plainCell "1"]])]
          upper = marksOf "BALANCE" page
          lower = marksOf "(BTCTEST4)" page
      (length upper, length lower, zipWith (<) (textBaseline <$> upper) (textBaseline <$> lower), marksOf "BALANCE (BTCTEST4)" page)
        `shouldBe` (1, 1, [True], [])

    -- The table starts 150 points down the first page, and a title of
    -- one line is written 10 points below: 150 + 10 = 160. A second line
    -- comes 8.5 points lower, at 168.5. The title of one line moves down
    -- to that line, so that every title ends just above the rule.
    it "keeps the last line of every title on one line, and a title of one line moves down to it" $ do
      let page = firstPage [TableBlock (Table [Column "Description" 400 AlignLeft, Column "Balance (BTCTEST4)" 82 AlignRight] [Row BodyRow [plainCell "a", plainCell "1"]])]
      (textBaseline <$> marksOf "BALANCE" page, textBaseline <$> marksOf "(BTCTEST4)" page, textBaseline <$> marksOf "DESCRIPTION" page)
        `shouldBe` ([160], [168.5], [168.5])

    it "starts again with the titles of its columns on each page it goes on to" $
      [take 2 (textsOf page) | page <- drop 1 (reportPages (reportOf [hundredRows]))] `shouldBe` [["ADDRESS", "BALANCE"], ["ADDRESS", "BALANCE"]]

    it "has every row once, in order, over its pages" $
      concatMap rowNames (reportPages (reportOf [hundredRows])) `shouldBe` ["row " <> Text.pack (show number) | number <- [1 :: Int .. 100]]

    it "ends every amount at the right edge of its column, whatever its length" $ do
      let page = firstPage [addressTable [Row BodyRow [plainCell "a", plainCell "0.00050000"], Row BodyRow [plainCell "b", plainCell "1234.00000000"]]]
      (rightEnd <$> marksOf "0.00050000" page, rightEnd <$> marksOf "1234.00000000" page) `shouldBe` ([53858], [53858])

    it "starts every text of a column aligned on the left at the left edge of the column" $ do
      let page = firstPage [addressTable [Row BodyRow [plainCell "a", plainCell "1"], Row BodyRow [plainCell "bbb", plainCell "2"]]]
      (textX <$> marksOf "a" page, textX <$> marksOf "bbb" page) `shouldBe` ([56.7], [56.7])

    it "writes the remark of a cell below its text, in smaller type" $ do
      let page = firstPage [addressTable [Row BodyRow [remarkCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" "Cold storage", plainCell "0.00050000"]]]
          address = marksOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" page
          remark = marksOf "Cold storage" page
      (textSize <$> address, textSize <$> remark, zipWith (>) (textBaseline <$> remark) (textBaseline <$> address), zipWith (==) (textX <$> remark) (textX <$> address))
        `shouldBe` ([8.5], [7.5], [True], [True])

    it "writes a remark too long for its column on several lines, and cuts nothing off" $ do
      let label = Text.unwords (replicate 30 "salary")
          page = firstPage [addressTable [Row BodyRow [remarkCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" label, plainCell "0.00050000"]]]
          remarkLines = [text | text <- textsOf page, "salary" `Text.isPrefixOf` text]
      (length remarkLines > 1, Text.unwords remarkLines) `shouldBe` (True, label)

    -- The first column has a quarter of the width of the table, 120
    -- points, and the 64 digits are more than twice as wide at 7.5
    -- points. The table is 482 points wide.
    it "writes a remark that runs on whole on one line, from the left of its cell, though it is wider than its column" $ do
      let txid = Text.replicate 64 "0"
          narrowFirst rows = TableBlock (Table [Column "Description" 1 AlignLeft, Column "Balance" 3 AlignRight] rows)
          runningOn = firstPage [narrowFirst [Row BodyRow [wideRemarkCell "Payment received" txid, plainCell "0.00050000"]]]
          keptIn = firstPage [narrowFirst [Row BodyRow [remarkCell "Payment received" txid, plainCell "0.00050000"]]]
      (textX <$> marksOf txid runningOn, length (marksOf txid keptIn)) `shouldBe` ([56.7], 0)

    it "writes a remark that runs on on several lines when it is wider than the table, and cuts nothing off" $ do
      let label = Text.unwords (replicate 30 "salary")
          page = firstPage [addressTable [Row BodyRow [wideRemarkCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" label, plainCell "0.00050000"]]]
          remarkLines = [text | text <- textsOf page, "salary" `Text.isPrefixOf` text]
      (length remarkLines > 1, Text.unwords remarkLines) `shouldBe` (True, label)

    it "writes a total in bold" $ do
      let page = firstPage [addressTable [Row BodyRow [plainCell "a", plainCell "1"], Row TotalRow [plainCell "Total", plainCell "1"]]]
      (textFont <$> marksOf "Total" page, textFont <$> marksOf "a" page) `shouldBe` ([Bold], [Regular])

    -- The rules within 15 points of the line of the total: one above
    -- it, then two below it
    it "puts a rule above a total and a double rule below it" $ do
      let page = firstPage [addressTable [Row BodyRow [plainCell "a", plainCell "1"], Row TotalRow [plainCell "Total", plainCell "1"]]]
      [y > textBaseline total | total <- marksOf "Total" page, RuleMark _ _ y _ _ <- page, abs (y - textBaseline total) < 15] `shouldBe` [False, True, True]

  describe "A heading" $ do
    it "stays on the page when there is room below it for what it heads" $ do
      let pages = reportPages (reportOf [addressTable [Row BodyRow [plainCell "a", plainCell "1"]], SectionBlock "Notes", NotesBlock ["One note."]])
      [number | (number, page) <- zip [1 :: Int ..] pages, "Notes" `elem` textsOf page] `shouldBe` [1]

    -- A table of 30 rows ends at 150 + 18 + 540 + 8 = 716, which leaves
    -- 51.89 points. A section heading takes 26 points above it and 26
    -- for itself, so it does not fit: it goes to the next page with its
    -- note.
    it "moves to the next page with what it heads when little room is left" $ do
      let rows = [Row BodyRow [plainCell "a", plainCell "1"] | _ <- [1 :: Int .. 30]]
          pages = reportPages (reportOf [addressTable rows, SectionBlock "Notes", NotesBlock ["One note."]])
      [(number, "One note." `elem` textsOf page) | (number, page) <- zip [1 :: Int ..] pages, "Notes" `elem` textsOf page] `shouldBe` [(2, True)]

    -- A table of 26 rows ends at 150 + 18 + 468 + 8 = 644, which leaves
    -- 123.89 points. The section heading takes 26 + 26 = 52, and the
    -- group heading 12 above it and 20 for itself. Both fit, with 39.89
    -- points to spare. The fact of four lines takes 4 x 13.5 = 54, and
    -- does not fit below them. So nothing of the section is on the first
    -- page.
    it "of a section moves to the next page with the group it heads, when the first fact of the group does not fit below them" $ do
      let rows = [Row BodyRow [plainCell "a", plainCell "1"] | _ <- [1 :: Int .. 26]]
          block = Fact "Last block counted" ["Block 800001", "dated 14 November 2023", "Hash", "00000002"]
          pages = reportPages (reportOf [addressTable rows, SectionBlock "Blockchain data", GroupBlock "Bitcoin (BTC)" "", FactsBlock [block]])
      [filter (`elem` ["Blockchain data", "Bitcoin (BTC)", "Last block counted"]) (textsOf page) | page <- pages]
        `shouldBe` [[], ["Blockchain data", "Bitcoin (BTC)", "Last block counted"]]

    -- A table of 25 rows ends at 626, which leaves 141.89 points. The
    -- two headings and the fact of four lines take 52 + 32 + 54 = 138.
    it "of a section stays on the page with the group it heads, when the first fact of the group fits below them" $ do
      let rows = [Row BodyRow [plainCell "a", plainCell "1"] | _ <- [1 :: Int .. 25]]
          block = Fact "Last block counted" ["Block 800001", "dated 14 November 2023", "Hash", "00000002"]
          pages = reportPages (reportOf [addressTable rows, SectionBlock "Blockchain data", GroupBlock "Bitcoin (BTC)" "", FactsBlock [block]])
      [filter (`elem` ["Blockchain data", "Bitcoin (BTC)", "Last block counted"]) (textsOf page) | page <- pages]
        `shouldBe` [["Blockchain data", "Bitcoin (BTC)", "Last block counted"]]

    -- A table of 28 rows ends at 150 + 18 + 504 + 8 = 680, which leaves
    -- 87.89 points. The group heading takes 12 + 20 = 32 and the titles
    -- of the columns 18, which leaves 37.89.
    --
    -- The first row has a transaction identifier of 64 digits below its
    -- text. The first column is a quarter of the 481.88 points of the
    -- table, less 12 kept clear: 108.47 points. A digit is 4.17 points
    -- wide at 7.5 points, so 26 fit on a line at most, and the
    -- identifier takes three lines. The row is 18 + 3 x 9.5 = 46.5
    -- points high, and does not fit.
    it "of a group moves to the next page with the table it heads, when the first row of the table does not fit below it" $ do
      let rows = [Row BodyRow [plainCell "a", plainCell "1"] | _ <- [1 :: Int .. 28]]
          account = TableBlock (Table [Column "Description" 1 AlignLeft, Column "Amount" 3 AlignRight] [Row BodyRow [remarkCell "Payment received" (Text.replicate 64 "0"), plainCell "0.00050000"]])
          pages = reportPages (reportOf [addressTable rows, GroupBlock "portfolio0" "", account])
      [filter (`elem` ["portfolio0", "DESCRIPTION", "Payment received"]) (textsOf page) | page <- pages]
        `shouldBe` [[], ["portfolio0", "DESCRIPTION", "Payment received"]]

    -- The same table of 28 rows leaves 87.89 points. The group heading,
    -- the titles of the columns and a first row of one line take
    -- 32 + 18 + 18 = 68. The second row is 46.5 points high and does not
    -- fit in the 19.89 points left: it starts the next page, below the
    -- titles of the columns again.
    it "of a group stays on the page with the first row of its table, though the second row goes on to the next page" $ do
      let rows = [Row BodyRow [plainCell "a", plainCell "1"] | _ <- [1 :: Int .. 28]]
          account =
            TableBlock $
              Table
                [Column "Description" 1 AlignLeft, Column "Amount" 3 AlignRight]
                [ Row BodyRow [plainCell "Opening balance", plainCell "0.00000000"],
                  Row BodyRow [remarkCell "Payment received" (Text.replicate 64 "0"), plainCell "0.00050000"]
                ]
          pages = reportPages (reportOf [addressTable rows, GroupBlock "portfolio0" "", account])
      [filter (`elem` ["portfolio0", "DESCRIPTION", "Opening balance", "Payment received"]) (textsOf page) | page <- pages]
        `shouldBe` [["portfolio0", "DESCRIPTION", "Opening balance"], ["DESCRIPTION", "Payment received"]]

    it "of a group has its remark beside it, on the same line" $ do
      let page = firstPage [GroupBlock "portfolio0" "Savings"]
          title = marksOf "portfolio0" page
          remark = marksOf "Savings" page
      (zipWith (==) (textBaseline <$> remark) (textBaseline <$> title), zipWith (>) (textX <$> remark) (textX <$> title)) `shouldBe` ([True], [True])

  describe "The facts of a report" $
    it "each have their name, then their value, on as many lines as it has" $
      take 5 (drop 4 (textsOf (firstPage [FactsBlock [Fact "Prepared on" ["15 November 2026"], Fact "Wallets" ["wallet0", "wallet1"]]])))
        `shouldBe` ["Prepared on", "15 November 2026", "Wallets", "wallet0", "wallet1"]

  describe "The notes of a report" $
    it "are numbered from 1, in their order" $
      take 4 (drop 4 (textsOf (firstPage [NotesBlock ["First.", "Second."]]))) `shouldBe` ["1.", "First.", "2.", "Second."]
  where
    -- A text of a row of 'hundredRows': its name or its balance
    isRow mark = case mark of
      TextMark _ _ _ _ _ _ text -> "row " `Text.isPrefixOf` text || text == "0.00050000"
      _ -> False
