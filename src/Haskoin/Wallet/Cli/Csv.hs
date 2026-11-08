{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A sheet as a CSV file, as --csv writes a listing for a spreadsheet:
-- the fields of RFC 4180, apart by commas, each line ended by CR LF, a
-- field quoted when it holds a comma, a quote or a line break.
--
-- CSV has no kinds of values: a spreadsheet guesses the kind of each field
-- as it opens the file. The writer keeps the guesses right:
--
-- * Amounts are in satoshis, whole numbers with no decimal point. A
--   spreadsheet set to a decimal comma, as in French or German, would
--   read 0.00050000 as text, and its sums would leave it out.
-- * Times are in UTC, as 2023-11-14T22:13:20, the form of ISO 8601,
--   which LibreOffice reads as a time with its default settings. With a
--   space in place of the T, it reads text, unless "Detect special
--   numbers" is ticked as it imports the file. An offset after the time,
--   such as +01:00, would make it read text too.
-- * A text that starts with =, +, -, @, a tab or a carriage return gets
--   an apostrophe in front ('formulaGuard'). A spreadsheet would run it as
--   a formula, and a label can come from a file: from another wallet
--   through importacc, or from the online computer through a transaction
--   file. A formula there could send the rows of the sheet to a web
--   server. The apostrophe shows in the cell, which is the price.
-- * The file starts with the byte order mark of UTF-8, the sign Excel
--   needs to read UTF-8 rather than the code page of the computer, which
--   would garble a label in another script. LibreOffice reads it too. A
--   script reads a listing with --json instead.
--
-- A spreadsheet still reads as a number a text made of digits alone, and
-- drops its leading zeros: a label "007" becomes 7. Nothing in a CSV file
-- can stop it, so the sheets leave out the one text hw makes that is
-- often digits alone, the fingerprint of a seed
-- ("Haskoin.Wallet.Cli.Sheet").
--
-- A spreadsheet set to a decimal comma also expects semicolons between
-- the fields of a CSV file. Excel then shows each row in one cell, unless
-- the file is imported with commas named. LibreOffice asks. Commas are
-- what RFC 4180 and most tools expect.
module Haskoin.Wallet.Cli.Csv
  ( sheetCsv,
    cellField,
    formulaGuard,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Haskoin.Wallet.Cli.Sheet (Cell (..), Sheet (..))

-- | The sheet as the bytes of a CSV file: the byte order mark, the line
-- of headings, then a line for each row
sheetCsv :: Sheet -> ByteString
sheetCsv sheet = byteOrderMark <> encodeUtf8 (Text.concat (map csvLine (headings : rows)))
  where
    headings = cellField . TextCell <$> sheetHeadings sheet
    rows = map cellField <$> sheetRows sheet

-- | The byte order mark of UTF-8, U+FEFF in UTF-8
byteOrderMark :: ByteString
byteOrderMark = BS.pack [0xEF, 0xBB, 0xBF]

-- | A line of the file: its fields apart by commas, then CR LF
csvLine :: [Text] -> Text
csvLine fields = Text.intercalate "," fields <> "\r\n"

-- | A cell as a field of the file: text guarded and quoted as it needs,
-- amounts and counts as whole numbers, a time in UTC to the second in the
-- form of ISO 8601, and nothing for an empty cell
cellField :: Cell -> Text
cellField = \case
  TextCell t -> quoted (formulaGuard t)
  SatoshiCell amount -> Text.pack (show amount)
  CountCell n -> Text.pack (show n)
  TimeCell time -> Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time)
  EmptyCell -> ""

-- | A text that a spreadsheet would run as a formula, with an apostrophe
-- in front: one that starts with =, +, - or @, or with a tab or a carriage
-- return, which a spreadsheet may skip to reach one. The characters are
-- those OWASP names. Any other text is as it was.
formulaGuard :: Text -> Text
formulaGuard t = case Text.uncons t of
  Just (c, _) | c `elem` ['=', '+', '-', '@', '\t', '\r'] -> "'" <> t
  _ -> t

-- | A field in quotes, its quotes doubled, when it holds a comma, a quote
-- or a line break: as it is otherwise
quoted :: Text -> Text
quoted t
  | Text.any (`elem` [',', '"', '\r', '\n']) t = "\"" <> Text.replace "\"" "\"\"" t <> "\""
  | otherwise = t
