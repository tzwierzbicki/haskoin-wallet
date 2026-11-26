{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The parts every answer of hw is made of: text in a style, the name of a
-- field, a sentence that wraps, a list, an amount, a time, a number of
-- confirmations and a tag.
--
-- Every other module of "Haskoin.Wallet.Cli.PrettyPrinter" builds its
-- documents from these, and this module uses none of them. That is what
-- keeps the answers alike: an amount, a time or a label is written in one
-- place, and looks the same in a view, in a table and in a question.
module Haskoin.Wallet.Cli.PrettyPrinter.Base
  ( renderIO,
    renderOn,
    text,
    styled,
    field,
    block,
    rblock,
    shownWidth,
    withStyle,
    columns,
    labelWord,
    cellsLine,
    countPrinter,
    commaList,
    orList,
    counted,
    andList,
    andListWords,
    sentence,
    wordsOf,
    amountPrinter,
    neutralAmount,
    amountIn,
    changeSign,
    amountCell,
    amountWidth,
    valueText,
    rateDoc,
    minuteText,
    minuteWidth,
    minuteWord,
    secondText,
    dayText,
    agoText,
    fullConfirmations,
    cappedConfirmations,
    cappedCount,
    tagWord,
    tagMarks,
    fileHeadingDoc,
  )
where

import Data.List (intercalate, intersperse)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, ZonedTime, diffUTCTime)
import Data.Time.Format
import Data.Word
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Tags (ShownTag (..), tagText)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, annotate, fillSep, hcat, indent, pretty, vcat, (<+>))
import System.IO
import Text.DocLayout (realLength)

{- Rendering -}

-- | Print the document on stdout
renderIO :: Doc Style -> IO ()
renderIO = renderOn stdout

-- | Print the document on the handle: stdout for an answer, stderr for
-- what a command shows before it asks the user. It is styled on a
-- terminal, and plain otherwise.
renderOn :: Handle -> Doc Style -> IO ()
renderOn h doc = do
  output <- outputOf h
  putDoc h output doc

{- The parts of an answer -}

-- | Text as it is, unstyled
text :: String -> Doc Style
text = pretty

-- | Text in a style
styled :: Style -> String -> Doc Style
styled style = annotate style . pretty

-- | The name of a field padded to the width, with its colon, before its
-- value: "Fee      : "
field :: Natural -> Text -> Doc Style
field n name = styled Field (block n (cs name) <> ":") <> " "

-- | A text padded with spaces after it to the width given, in columns of
-- the terminal ('shownWidth')
block :: Natural -> String -> String
block n str =
  case n `safeSubtract` fromIntegral (shownWidth str) of
    Just missing -> str <> replicate (fromIntegral missing) ' '
    _ -> str

-- | A text padded with spaces before it to the width given, in columns of
-- the terminal ('shownWidth')
rblock :: Natural -> String -> String
rblock n str =
  case n `safeSubtract` fromIntegral (shownWidth str) of
    Just missing -> replicate (fromIntegral missing) ' ' <> str
    _ -> str

-- | How many columns of the terminal a text takes. A character of most
-- scripts takes one, one of the East Asian scripts or an emoji two, and a
-- combining accent none. Counting characters would put a cell after such
-- a text out of its column, so every padding counts columns. A character of ambiguous width counts one, as most terminals
-- show it.
shownWidth :: String -> Int
shownWidth = realLength

-- | The document in the style
withStyle :: Doc Style -> Style -> Doc Style
withStyle doc style = annotate style doc

-- | Cells side by side, two spaces apart
columns :: [Doc Style] -> Doc Style
columns = hcat . intersperse "  "

-- | The label of an address or of a transaction, quoted, which sets it
-- apart on a line. A label may come from a file, so every character that
-- is not text, a line break included, shows as an escape ('visibleText'):
-- a label cannot write lines of its own, such as a recipient the
-- transaction does not pay.
labelWord :: Text -> Doc Style
labelWord label = styled UserText ("\"" <> cs (visibleText label) <> "\"")

-- | Cells side by side, two spaces apart, each with the width of its
-- column and its text. Each is padded to the width of its column, but the
-- last: a line ends without spaces. A right-aligned cell comes padded on
-- its left, and its text holds that padding, so that it gets none on its
-- right.
cellsLine :: [(Int, String, Doc Style)] -> Doc Style
cellsLine cells = hcat (intersperse "  " (padded cells))
  where
    padded [] = []
    padded [(_, _, doc)] = [doc]
    padded ((width, content, doc) : rest) = (doc <> pretty (replicate (width - shownWidth content) ' ')) : padded rest

{- Sentences and lists -}

-- | A count, such as of transactions or coins
countPrinter :: Natural -> Doc Style
countPrinter = text . show

-- | Items one after the other, a comma between them
commaList :: [Doc Style] -> Doc Style
commaList = hcat . intersperse ", "

-- | Names in a sentence, "or" before the last: "btc", "btc or bch",
-- "btc, bch or btctest4"
orList :: [String] -> String
orList names = case reverse names of
  [] -> ""
  [name] -> name
  lastName : before -> intercalate ", " (reverse before) <> " or " <> lastName

-- | A count with its noun: "no coins", "1 coin", "3 coins"
counted :: Natural -> String -> String -> String
counted 0 _ plural = "no " <> plural
counted 1 singular _ = "1 " <> singular
counted n _ plural = show n <> " " <> plural

-- | Names in a sentence, "and" before the last: "btc", "btc and bch",
-- "btc, bch and btctest4"
andList :: [String] -> String
andList names = case reverse names of
  [] -> ""
  [name] -> name
  lastName : before -> intercalate ", " (reverse before) <> " and " <> lastName

-- | Names as 'andList' writes them, as the words of a sentence that wraps
-- ('sentence'). Each name is one word, so a line breaks between two names
-- and never inside one, even a name with spaces such as
-- "btc (bip44 and bip84)". The "and" goes with the last name, so that a
-- line does not end with it.
andListWords :: [String] -> [Doc Style]
andListWords names = case reverse names of
  [] -> []
  [name] -> [text name]
  lastName : nextToLast : earlier -> (text . (<> ",") <$> reverse earlier) <> [text nextToLast, text ("and " <> lastName)]

-- | A sentence of words and amounts, wrapped at the width of the screen
sentence :: [Doc Style] -> Doc Style
sentence = fillSep

-- | The words of a text, for a sentence
wordsOf :: String -> [Doc Style]
wordsOf = map pretty . words

{- Amounts -}

-- | An amount the wallet holds or gets, in green unless zero
amountPrinter :: AmountUnit -> Word64 -> Doc Style
amountPrinter unit = integerAmountPrinter unit . fromIntegral

-- | An amount coloured by its sign: green when it comes in, red when it
-- goes out
integerAmountPrinter :: AmountUnit -> Integer -> Doc Style
integerAmountPrinter unit amnt
  | amnt > 0 = amountIn (Just Incoming) unit amnt
  | amnt < 0 = amountIn (Just Outgoing) unit amnt
  | otherwise = amountIn Nothing unit amnt

-- | An amount neither in nor out, such as a fee: unstyled
neutralAmount :: AmountUnit -> Integer -> Doc Style
neutralAmount = amountIn Nothing

-- | The number of an amount in the style, if any, then its unit
amountIn :: Maybe Style -> AmountUnit -> Integer -> Doc Style
amountIn style unit amnt = maybe id annotate style (pretty (showAmount unit amnt)) <+> pretty (showUnit unit amnt)

-- | The style and the sign before the number of what a transaction adds
-- to the account or takes from it: green with a plus when it comes in,
-- red when it goes out, where the number has its minus already
changeSign :: Integer -> (Maybe Style, String)
changeSign amnt
  | amnt > 0 = (Just Incoming, "+")
  | amnt < 0 = (Just Outgoing, "")
  | otherwise = (Nothing, "")

-- | An amount in a column: its number after the sign given, right-aligned
-- in a column wide enough for 9 999 bitcoins in any unit, in the style if
-- any, then its unit padded to its plural, so that the amounts of a
-- listing line up. A larger amount pushes the rest of its line.
amountCell :: Maybe Style -> String -> AmountUnit -> Integer -> Doc Style
amountCell style sign unit amnt =
  maybe id annotate style (amountNumber sign unit amnt)
    <+> pretty (block unitWidth (cs (showUnit unit amnt)))
  where
    unitWidth = fromIntegral (Text.length (showUnit unit 2))

-- | The number of an amount, as it stands in the column of the amounts
amountNumber :: String -> AmountUnit -> Integer -> Doc Style
amountNumber sign unit amnt = pretty (rblock amountWidth (sign <> cs (showAmount unit amnt)))

-- | The width of the number of an amount in a column: that of 9 999
-- bitcoins with their sign in any unit, such as "-9999.00000000". More
-- is rare in a wallet. An amount that does not fit pushes the rest of its
-- line, rather than every line making room for 21 million bitcoins.
amountWidth :: Natural
amountWidth = 14

-- | A value of an address, as its column shows it: "0.00050000 btc"
valueText :: AmountUnit -> Natural -> String
valueText unit v = cs (showAmount unit (fromIntegral v) <> " " <> showUnit unit (fromIntegral v))

-- | A fee rate that is asked for, in satoshis per virtual byte: a whole
-- number
rateDoc :: Natural -> Doc Style
rateDoc feeByte = text (show feeByte <> " sat/vB")

{- Times -}

-- | A time to the minute, as the first line of a transaction shows it:
-- "31 Oct 2026 09:12", always 'minuteWidth' characters
minuteText :: ZonedTime -> String
minuteText = formatTime defaultTimeLocale "%d %b %Y %H:%M"

-- | The width of a time to the minute, in characters
minuteWidth :: Natural
minuteWidth = 17

-- | A time to the minute, as a first line starts with it, in a column of
-- 'minuteWidth' characters: "31 Oct 2026 09:12", or "time unknown",
-- faint, when hw does not know the time
minuteWord :: Maybe ZonedTime -> Doc Style
minuteWord = maybe (styled Muted (block minuteWidth "time unknown")) (text . minuteText)

-- | A time to the second, with its time zone: "31 Oct 2026 09:12:40 CET"
secondText :: ZonedTime -> String
secondText = formatTime defaultTimeLocale "%d %b %Y %H:%M:%S %Z"

-- | A time to the day, as the date a portfolio was created shows
dayText :: ZonedTime -> String
dayText = formatTime defaultTimeLocale "%d %b %Y"

-- | How long before now a time was, in round words: "3 minutes ago",
-- "2 hours ago", "5 days ago". A time to come reads as just past.
agoText :: UTCTime -> UTCTime -> String
agoText now time
  | seconds < 60 = "less than a minute ago"
  | minutes < 120 = count minutes "minute"
  | hours < 48 = count hours "hour"
  | otherwise = count days "day"
  where
    seconds = max 0 (floor (diffUTCTime now time)) :: Integer
    minutes = seconds `div` 60
    hours = minutes `div` 60
    days = hours `div` 24
    count n unit = show n <> " " <> unit <> (if n == 1 then "" else "s") <> " ago"

{- Confirmations -}

-- | A number of confirmations in full: "1 confirmation", "1234
-- confirmations". The verbose views give it, since they give every
-- detail.
fullConfirmations :: Natural -> String
fullConfirmations n = counted n "confirmation" "confirmations"

-- | A number of confirmations up to 999: "1 confirmation", "998
-- confirmations", and "999+ confirmations" for more. The compact views
-- give it. Past a few blocks, how many more no longer matters, and a
-- count that grows without end would push the rest of a line. The
-- verbose views give the full count, and the JSON answers the number.
cappedConfirmations :: Natural -> String
cappedConfirmations n
  | n > countCap = cappedCount n <> " confirmations"
  | otherwise = fullConfirmations n

-- | A number up to @countCap@, alone: "998", and "999+" for more. The
-- table of txs gives the confirmations this way, under a heading that
-- names them.
cappedCount :: Natural -> String
cappedCount n
  | n > countCap = show countCap <> "+"
  | otherwise = show n

-- | The largest number of confirmations every view but the verbose one
-- counts to
countCap :: Natural
countCap = 999

{- Tags -}

-- | A tag in its colour, or in the colour of the text when it has none
tagWord :: ShownTag -> Doc Style
tagWord shown = maybe id (annotate . TagColour) (shownTagColor shown) (text (cs (tagText (shownTag shown))))

-- | Tags on a line of prose, each between brackets: [kyc] [cold]. The
-- brackets tell a tag from the words around it, a label or a path, on a
-- screen without colours too. They are faint, and only the word has the
-- colour of the tag. A hash sign was not used: a user who copies #kyc
-- to a command line starts a comment of the shell.
tagMarks :: [ShownTag] -> [Doc Style]
tagMarks = map (\shown -> styled Muted "[" <> tagWord shown <> styled Muted "]")

{- Files show tells by their kind -}

-- | The first lines of a file show shows by what it holds: the kind of
-- the file in bold, with the file as the user named it, then a line that
-- says what the file is and what follows. show takes two files that both
-- hold wallets, a BIP 329 file of labels and a copy of the database, and
-- a backup has one of each, with the same time in their names. Their
-- views both start with wallets and portfolios. So each view names its
-- kind first, in the same place, in the words the user knows the format
-- by: "BIP 329 file" and "SQLite database".
fileHeadingDoc :: String -> FilePath -> Doc Style -> Doc Style
fileHeadingDoc kind file about =
  vcat [columns [styled Title kind, styled UserText file], indent 2 about]
