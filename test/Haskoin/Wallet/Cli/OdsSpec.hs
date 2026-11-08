{-# LANGUAGE OverloadedStrings #-}

-- | How exportsheet writes sheets as an OpenDocument spreadsheet: the
-- archive, the cells of each kind, the paragraphs of text, the widths of
-- the columns, and the frozen header
module Haskoin.Wallet.Cli.OdsSpec (spec) where

import Codec.Archive.Zip (eRelativePath, toArchive, zEntries)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Haskoin.Wallet.Cli.Ods (cellXml, columnMillimetres, paragraphXml, settingsXml, tableXml, workbookOds)
import Haskoin.Wallet.Cli.Sheet (Cell (..), Sheet (..))
import Haskoin.Wallet.TestUtils (seenTime)
import Test.Hspec

spec :: Spec
spec = do
  describe "An OpenDocument spreadsheet" $ do
    let ods = workbookOds seenTime [("Coins", Sheet ["Coins"] [[CountCell 3]])]

    -- A zip archive starts with PK 3 4. Its first entry, mimetype, has a
    -- header of 30 bytes, then its name of 8 bytes, then its contents.
    it "is a zip archive whose first entry, the type of the file, shows as it is at byte 30" $ do
      BS.take 4 ods `shouldBe` "PK\x03\x04"
      BS.take 54 (BS.drop 30 ods) `shouldBe` "mimetypeapplication/vnd.oasis.opendocument.spreadsheet"

    it "holds the type, the manifest, the sheets and the settings of the view, in that order" $
      map eRelativePath (zEntries (toArchive (BL.fromStrict ods)))
        `shouldBe` ["mimetype", "META-INF/manifest.xml", "content.xml", "settings.xml"]

  describe "A cell of a spreadsheet" $ do
    it "is a string for a text" $
      cellXml (TextCell "Rent of October")
        `shouldBe` "<table:table-cell office:value-type=\"string\"><text:p>Rent of October</text:p></table:table-cell>"

    -- A string cell is never run: the apostrophe of a CSV file is not needed
    it "keeps a text that starts with = as it is, as a string" $
      cellXml (TextCell "=1+1")
        `shouldBe` "<table:table-cell office:value-type=\"string\"><text:p>=1+1</text:p></table:table-cell>"

    it "is a number with the separators of thousands for an amount in satoshis" $
      cellXml (SatoshiCell (-41000))
        `shouldBe` "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"-41000\"><text:p>-41000</text:p></table:table-cell>"

    it "is a plain number for a count" $
      cellXml (CountCell 800000)
        `shouldBe` "<table:table-cell office:value-type=\"float\" office:value=\"800000\"><text:p>800000</text:p></table:table-cell>"

    -- seenTime is 14 Nov 2023 22:13:20 UTC
    it "is a date for a time, shown to the second" $
      cellXml (TimeCell seenTime)
        `shouldBe` "<table:table-cell table:style-name=\"time\" office:value-type=\"date\" office:date-value=\"2023-11-14T22:13:20\"><text:p>2023-11-14 22:13:20</text:p></table:table-cell>"

    it "is an empty element for an empty cell" $
      cellXml EmptyCell `shouldBe` "<table:table-cell/>"

  describe "The paragraph of a text" $ do
    it "escapes the characters XML gives a meaning" $
      paragraphXml "Rent & <fees> \"Lyon\" 'flat'" `shouldBe` "Rent &amp; &lt;fees&gt; &quot;Lyon&quot; &apos;flat&apos;"

    -- OpenDocument drops spaces at the start and the end, and collapses
    -- those in a row: they are counted in text:s instead
    it "keeps the spaces at its start, in a row and at its end" $
      paragraphXml "  Rent   of October " `shouldBe` "<text:s text:c=\"2\"/>Rent <text:s text:c=\"2\"/>of October<text:s/>"

    it "keeps a tab and a line break, each as its element" $
      paragraphXml "Rent\tOctober\nLyon" `shouldBe` "Rent<text:tab/>October<text:line-break/>Lyon"

    it "puts the replacement character for a control character, which XML cannot hold" $
      paragraphXml "Rent\ESC[31m" `shouldBe` "Rent\xFFFD[31m"

  describe "A sheet as a table" $
    -- "Coins" has 5 characters: 2 × 5 + 3 = 13 millimetres
    it "has its columns, its headings in a header row, then a row for each of its rows" $
      tableXml ("Coins", Sheet ["Coins"] [[CountCell 3]])
        `shouldBe` Text.concat
          [ "<table:table table:name=\"Coins\">",
            "<table:table-column table:style-name=\"width-13\"/>",
            "<table:table-header-rows><table:table-row>",
            "<table:table-cell table:style-name=\"heading\" office:value-type=\"string\"><text:p>Coins</text:p></table:table-cell>",
            "</table:table-row></table:table-header-rows>",
            "<table:table-row><table:table-cell office:value-type=\"float\" office:value=\"3\"><text:p>3</text:p></table:table-cell></table:table-row>",
            "</table:table>"
          ]

  describe "The width of a column" $ do
    -- "Coins" has 5 characters: 2 × 5 + 3 = 13. The address has 34:
    -- 2 × 34 + 3 = 71.
    it "is 2 millimetres for each character of its widest cell, its heading included, and 3 for the margins" $
      columnMillimetres (Sheet ["Coins", "Address"] [[CountCell 3, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]])
        `shouldBe` [13, 71]

    -- 東京の家賃 has 5 characters, which take 10 columns: 2 × 10 + 3 = 23
    it "counts a wide character twice" $
      columnMillimetres (Sheet ["Label"] [[TextCell "東京の家賃"]]) `shouldBe` [23]

    -- 5000000 shows as 5,000,000, of 9 characters: 2 × 9 + 3 = 21.
    -- -41000 shows as -41,000, of 7: 2 × 7 + 3 = 17.
    it "counts an amount with its separators of thousands and its sign" $
      columnMillimetres (Sheet ["Sats", "Paid"] [[SatoshiCell 5000000, SatoshiCell (-41000)]]) `shouldBe` [21, 17]

    -- A txid has 64 characters: 2 × 64 + 3 = 131
    it "stops growing at 64 characters, those of a txid" $
      columnMillimetres (Sheet ["Label"] [[TextCell (Text.replicate 100 "a")]]) `shouldBe` [131]

  describe "The settings of the view" $
    it "freeze the header row of each sheet at the top, and show the first sheet" $
      settingsXml ["Coins"]
        `shouldBe` Text.concat
          [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
            "<office:document-settings xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:config=\"urn:oasis:names:tc:opendocument:xmlns:config:1.0\" office:version=\"1.3\">",
            "<office:settings><config:config-item-set config:name=\"ooo:view-settings\">",
            "<config:config-item-map-indexed config:name=\"Views\"><config:config-item-map-entry>",
            "<config:config-item config:name=\"ViewId\" config:type=\"string\">view1</config:config-item>",
            "<config:config-item-map-named config:name=\"Tables\">",
            "<config:config-item-map-entry config:name=\"Coins\">",
            "<config:config-item config:name=\"HorizontalSplitMode\" config:type=\"short\">0</config:config-item>",
            "<config:config-item config:name=\"VerticalSplitMode\" config:type=\"short\">2</config:config-item>",
            "<config:config-item config:name=\"HorizontalSplitPosition\" config:type=\"int\">0</config:config-item>",
            "<config:config-item config:name=\"VerticalSplitPosition\" config:type=\"int\">1</config:config-item>",
            "<config:config-item config:name=\"ActiveSplitRange\" config:type=\"short\">2</config:config-item>",
            "<config:config-item config:name=\"PositionLeft\" config:type=\"int\">0</config:config-item>",
            "<config:config-item config:name=\"PositionRight\" config:type=\"int\">0</config:config-item>",
            "<config:config-item config:name=\"PositionTop\" config:type=\"int\">0</config:config-item>",
            "<config:config-item config:name=\"PositionBottom\" config:type=\"int\">1</config:config-item>",
            "</config:config-item-map-entry>",
            "</config:config-item-map-named>",
            "<config:config-item config:name=\"ActiveTable\" config:type=\"string\">Coins</config:config-item>",
            "</config:config-item-map-entry></config:config-item-map-indexed>",
            "</config:config-item-set></office:settings></office:document-settings>"
          ]
