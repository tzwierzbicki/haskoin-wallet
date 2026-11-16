{-# LANGUAGE TemplateHaskell #-}

-- | The two fonts of the reports of hw, compiled into the program:
-- Liberation Sans, regular and bold.
--
-- Liberation Sans has the widths of Helvetica and of Arial, letter for
-- letter, so a report looks as a reader expects a statement to look. It
-- has the letters of Latin, Greek and Cyrillic, some 2300 characters. It
-- is free to ship and to embed in a document, under the SIL Open Font
-- License, whose text is in the folder fonts beside the two files.
--
-- It has no Chinese, Japanese or Korean, and no Arabic. A font with all
-- of these is tens of megabytes. A character the font does not have is
-- written as a question mark ("Haskoin.Wallet.Report.Pdf").
--
-- The files are read as hw is compiled ('embedFile'), so the program
-- needs no file beside it to write a report.
module Haskoin.Wallet.Report.Typefaces
  ( regularTypeface,
    boldTypeface,
  )
where

import qualified Data.ByteString as BS
import Haskoin.Wallet.Report.Font (Typeface, embedFile, readTypeface)

-- | Liberation Sans, for the text of a report
regularTypeface :: Typeface
regularTypeface = compiledIn "LiberationSans-Regular" $(embedFile "fonts/LiberationSans-Regular.ttf")

-- | Liberation Sans Bold, for the titles and the totals
boldTypeface :: Typeface
boldTypeface = compiledIn "LiberationSans-Bold" $(embedFile "fonts/LiberationSans-Bold.ttf")

-- | A font read from the bytes compiled into hw. The bytes are those of
-- a file of the repository, which the tests read too: one that does not
-- read is a mistake in the build, and stops the program with its name.
compiledIn :: String -> BS.ByteString -> Typeface
compiledIn name bytes = either (\reason -> error ("The font " <> name <> " of hw does not read: " <> reason)) id (readTypeface bytes)
