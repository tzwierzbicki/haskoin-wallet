{-# LANGUAGE OverloadedStrings #-}

-- | The layout of the reports: how a text is cut into lines, what every
-- page holds at its top and at its foot, and how the blocks of a report
-- are placed down the pages.
module Haskoin.Wallet.Report.LayoutSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Report.Pdf (Font (..), Mark (..), textWidth)
import Haskoin.Wallet.Report.Layout
import Test.Hspec

-- The pages are A4, 595.28 by 841.89 points, with margins of 56.7
-- points on the left and on the right. So what a page holds ends 538.58
-- points from the left. The rule of the footer is 56 points above the
-- bottom of the paper, 785.89 points down.

-- | A report with the blocks given, titled as a balance statement
reportOf :: [Block] -> Report
reportOf = Report "Statement of holdings" "Balance Statement" "As of 31 December 2025" "Balance Statement, as of 31 December 2025"

-- | The first page of a report with the blocks given
firstPage :: [Block] -> [Mark]
firstPage = concat . take 1 . reportPages . reportOf

-- | The texts of a page, in the order they are drawn
textsOf :: [Mark] -> [Text]
textsOf page = [text | TextMark _ _ _ _ _ _ text <- page]

-- | The marks of a page that draw the text given
marksOf :: Text -> [Mark] -> [Mark]
marksOf text page = [mark | mark@(TextMark _ _ _ _ _ _ shown) <- page, shown == text]

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
      take 3 (textsOf (firstPage [])) `shouldBe` ["STATEMENT OF HOLDINGS", "Balance Statement", "As of 31 December 2025"]

    it "is there even for a report without a block" $
      length (reportPages (reportOf [])) `shouldBe` 1

  describe "The foot of a page" $ do
    it "names the report, and the page among how many" $
      drop 3 (textsOf (firstPage [])) `shouldBe` ["Balance Statement, as of 31 December 2025", "Page 1 of 1"]

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
      take 2 (drop 3 (textsOf (firstPage [hundredRows]))) `shouldBe` ["ADDRESS", "BALANCE"]

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

    -- Thirty rows of 18 points below the title block leave less than a
    -- tenth of the page: the heading goes to the next page with its note
    it "moves to the next page with what it heads when little room is left" $ do
      let rows = [Row BodyRow [plainCell "a", plainCell "1"] | _ <- [1 :: Int .. 30]]
          pages = reportPages (reportOf [addressTable rows, SectionBlock "Notes", NotesBlock ["One note."]])
      [(number, "One note." `elem` textsOf page) | (number, page) <- zip [1 :: Int ..] pages, "Notes" `elem` textsOf page] `shouldBe` [(2, True)]

    it "of a group has its remark beside it, on the same line" $ do
      let page = firstPage [GroupBlock "portfolio0" "Savings"]
          title = marksOf "portfolio0" page
          remark = marksOf "Savings" page
      (zipWith (==) (textBaseline <$> remark) (textBaseline <$> title), zipWith (>) (textX <$> remark) (textX <$> title)) `shouldBe` ([True], [True])

  describe "The facts of a report" $
    it "each have their name, then their value, on as many lines as it has" $
      take 5 (drop 3 (textsOf (firstPage [FactsBlock [Fact "Prepared on" ["15 November 2026"], Fact "Wallets" ["wallet0", "wallet1"]]])))
        `shouldBe` ["Prepared on", "15 November 2026", "Wallets", "wallet0", "wallet1"]

  describe "The notes of a report" $
    it "are numbered from 1, in their order" $
      take 4 (drop 3 (textsOf (firstPage [NotesBlock ["First.", "Second."]]))) `shouldBe` ["1.", "First.", "2.", "Second."]
  where
    -- A text of a row of 'hundredRows': its name or its balance
    isRow mark = case mark of
      TextMark _ _ _ _ _ _ text -> "row " `Text.isPrefixOf` text || text == "0.00050000"
      _ -> False
