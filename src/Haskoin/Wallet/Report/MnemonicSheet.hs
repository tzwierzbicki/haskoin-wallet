{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A sheet of paper to write a mnemonic on by hand, or the shares of a
-- backup, as a PDF file to print.
--
-- = Why hw makes it
--
-- The words of a wallet are shown once, by newportfolio, and the paper
-- they are written on is the only backup of the funds. A blank page
-- invites mistakes: a word left out, two words swapped, a page nobody can
-- tell from a shopping list ten years later. The sheet has a numbered
-- line for each word, so that a missing word shows and the order is
-- kept. It says what it is, which wallet it is of, whether a passphrase
-- goes with it, and how to keep it.
--
-- = The sheet is blank
--
-- hw never prints the words. A file with the words in it would be the
-- secret on a disk, in the queue of a printer and in the memory of the
-- printer, which is what a mnemonic written by hand avoids. So a sheet
-- holds nothing secret, and can be printed on any printer, before the
-- wallet is made or after.
--
-- What is not secret may be printed, and is when hw knows it: the
-- fingerprint of the wallet and the day it was made, on the sheet
-- newportfolio writes for the wallet it creates (the type t'SheetWallet').
-- The command mnemonicsheet knows no wallet, and leaves these to the pen.
--
-- = One sheet for each kind of secret
--
-- A sheet has exactly the lines its secret needs (the type t'SheetKind'):
-- 12 to 24 for a BIP39 mnemonic, and 20 or 33 for a share of a SLIP-39
-- backup. A backup in shares gets a page for each share, since the shares
-- are kept apart: each page stands alone, with its own title, its number
-- among the shares, how many of them restore the wallet, and the notes.
--
-- = The layout
--
-- The sheet has the style of the reports
-- ("Haskoin.Wallet.Report.Layout"), from which it takes the title block,
-- the foot and the notes. It is not laid out as a report is, block after
-- block down the pages: its parts have fixed places on one page. The
-- notes stand at the foot, and the lines for the words take all the room
-- between the fields and the notes, in two columns. So 12 words get wide
-- lines far apart, and the 33 words of a long share still fit on one
-- page, 7.5 millimetres apart, as the lines of a ruled notebook are.
module Haskoin.Wallet.Report.MnemonicSheet
  ( -- * What a sheet is for
    SheetKind (..),
    sheetKindOf,
    sheetOptions,
    SheetWallet (..),

    -- * Its pages
    SheetPage (..),
    sheetPages,
    pageMarks,
    sheetPdf,

    -- * The sheet hw keeps for a wallet
    walletSheetFile,
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, ZonedTime, defaultTimeLocale, formatTime)
import Haskoin (Fingerprint, fingerprintToText)
import Haskoin.Wallet.Keys.Entropy (defaultEntropyBytes, mnemonicWordCount)
import Haskoin.Wallet.Keys.Shares (shareScheme, shareWordCount, thresholdNeedsShares)
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.Pdf
import Numeric.Natural (Natural)
import System.FilePath (takeDirectory, (</>))

{- What a sheet is for -}

-- | The secret a sheet has lines for
data SheetKind
  = -- | A BIP39 mnemonic of this many words
    MnemonicSheet !Int
  | -- | The shares of a SLIP-39 backup: how many of them rebuild the seed,
    -- how many there are, and the words of each
    SharesSheet !Int !Int !Int
  deriving (Eq, Show)

-- | The sheet for the secret newportfolio makes with the same options:
-- the bytes of --entropy, the number of --shares and their --threshold.
-- An option left out means what it means there. Options newportfolio
-- refuses are refused here in the same words, so that a sheet is never
-- printed for a secret hw would not make.
sheetKindOf :: Maybe Natural -> Maybe Natural -> Maybe Natural -> Either String SheetKind
sheetKindOf entropyM sharesM thresholdM = case sharesM of
  Nothing -> do
    when (isJust thresholdM) $ Left thresholdNeedsShares
    MnemonicSheet <$> mnemonicWordCount bytes
  Just count -> do
    (threshold, shares) <- shareScheme count thresholdM
    SharesSheet threshold shares <$> shareWordCount bytes
  where
    bytes = fromMaybe defaultEntropyBytes entropyM

-- | The options of mnemonicsheet that give a sheet of this kind, for a
-- message that tells the user how to write it again. The default entropy
-- is left out, and so is the threshold of a single share.
sheetOptions :: SheetKind -> [String]
sheetOptions = \case
  -- Four bytes of entropy for every three words
  MnemonicSheet count -> entropy (count * 4 `div` 3)
  -- A share of 20 words is of 16 bytes, one of 33 words of 32
  SharesSheet threshold shares count ->
    entropy (if count > 20 then 32 else 16)
      <> ["--shares", show shares]
      <> concat [["--threshold", show threshold] | shares > 1]
  where
    entropy :: Int -> [String]
    entropy bytes = concat [["--entropy", show bytes] | fromIntegral bytes /= defaultEntropyBytes]

-- | What hw knows of the wallet a sheet is for, and prints in its fields:
-- the fingerprint of its seed, which names the wallet in any database,
-- and the day it was made. Neither is a secret. The name of the wallet
-- stays a line to write on: wallet0 is its place in one database, and
-- the owner knows it by a name of their own.
data SheetWallet = SheetWallet
  { sheetFingerprint :: !Fingerprint,
    sheetDay :: !Day
  }
  deriving (Eq, Show)

{- The pages -}

-- | What a page of a sheet says, apart from its lines. A sheet for a
-- mnemonic is one page. A sheet for a backup in shares has a page for
-- each share.
data SheetPage = SheetPage
  { -- | The kind of secret, in small capitals above the title
    pageKind :: !Text,
    -- | The title, which the foot of the page repeats
    pageTitle :: !Text,
    pageSubtitle :: !Text,
    -- | The heading of the lines for the words
    pageWordsTitle :: !Text,
    -- | How many numbered lines the page has, one for each word
    pageWordCount :: !Int,
    -- | The notes on how to keep the page
    pageNotes :: ![Text]
  }
  deriving (Eq, Show)

-- | The pages of a sheet: one for a mnemonic, and one for each share of a
-- backup in shares, in the order newportfolio numbers the shares
sheetPages :: SheetKind -> [SheetPage]
sheetPages = \case
  MnemonicSheet count ->
    [ SheetPage
        { pageKind = "BIP39 mnemonic \183 " <> shown count <> " words",
          pageTitle = "Mnemonic Backup Sheet",
          pageSubtitle = wholeSecretSubtitle count,
          pageWordsTitle = "The " <> spelled count <> " words",
          pageWordCount = count,
          pageNotes = wholeSecretNotes "BIP39 list of 2048 English words"
        }
    ]
  -- A single share is the whole backup, as a mnemonic is
  SharesSheet _ 1 count ->
    [ (sharePage 1 1 count)
        { pageSubtitle = wholeSecretSubtitle count,
          pageWordsTitle = "The " <> spelled count <> " words",
          pageNotes = wholeSecretNotes slip39List
        }
    ]
  SharesSheet threshold shares count ->
    [ (sharePage number shares count)
        { pageSubtitle = capitalized (spelled count) <> " words, written by hand. " <> restoring threshold shares <> ".",
          pageWordsTitle = "The " <> spelled count <> " words of this share",
          pageNotes = shareNotes threshold shares
        }
      | number <- [1 .. shares]
    ]

-- | The page of a share by its number among the shares, without what
-- depends on how many shares restore the wallet
sharePage :: Int -> Int -> Int -> SheetPage
sharePage number shares count =
  SheetPage
    { pageKind = "SLIP-39 share \183 " <> shown count <> " words",
      pageTitle = "Backup Share " <> shown number <> " of " <> shown shares,
      pageSubtitle = "",
      pageWordsTitle = "",
      pageWordCount = count,
      pageNotes = []
    }

-- | The line below the title of a page that holds the whole secret
wholeSecretSubtitle :: Int -> Text
wholeSecretSubtitle count = "The " <> spelled count <> " words that restore a wallet, written by hand"

-- | Which shares restore the wallet, as a clause: any of the threshold,
-- or all of them when the threshold is their number
restoring :: Int -> Int -> Text
restoring threshold shares
  | threshold == shares = "The " <> shown shares <> " shares together restore the wallet"
  | otherwise = "Any " <> shown threshold <> " of the " <> shown shares <> " shares restore the wallet"

-- | The list a share takes its words from
slip39List :: Text
slip39List = "SLIP-39 list of 1024 English words"

-- | The notes of a page that holds the whole secret, a mnemonic or a
-- single share, given the list its words are from
wholeSecretNotes :: Text -> [Text]
wholeSecretNotes list =
  [ "Whoever reads these words can spend the funds of the wallet. Keep this sheet where only you can reach it.",
    "Keep a second copy in another safe place, in case this one is lost or destroyed.",
    neverTyped,
    writtenWithPen,
    wordsOf list,
    "A passphrase is part of the secret: without it, these words give another wallet. Keep it apart from this sheet.",
    "Before you send funds to the wallet, check this sheet with hw checkmnemonic, which asks for the words."
  ]

-- | The notes of the page of a share among several. They say what the
-- shares do together, since the holder of one share may know nothing
-- else of the backup: how many of them restore the wallet, and how many
-- can be lost.
shareNotes :: Int -> Int -> [Text]
shareNotes threshold shares =
  [ restoring threshold shares <> ", so whoever reads " <> enough <> " of them can spend its funds. Fewer reveal nothing about it.",
    "Keep each share in a different safe place. " <> losses,
    neverTyped,
    writtenWithPen,
    wordsOf slip39List,
    "A passphrase is part of the secret: without it, the shares give another wallet. Keep it apart from the shares.",
    "Before you send funds to the wallet, check the shares with hw checkmnemonic, which asks for their words."
  ]
  where
    spare = shares - threshold
    enough
      | spare == 0 = "all"
      | otherwise = shown threshold
    losses
      | spare == 0 = "Every share is needed: if one is lost, so are the funds."
      | spare == 1 = "If 1 share is lost, the others still restore the wallet. If more are lost, so are the funds."
      | otherwise = "If up to " <> shown spare <> " shares are lost, the others still restore the wallet. If more are lost, so are the funds."

neverTyped, writtenWithPen :: Text
neverTyped = "Never type the words into a website or a phone, and never photograph this sheet."
writtenWithPen = "Write with a pen, in block capitals. Then read the words back against the screen, in order."

-- | The note on the list the words are from. Both lists are made so that
-- four letters are enough to tell a word, which is what saves a word
-- that was written badly.
wordsOf :: Text -> Text
wordsOf list = "Every word is in the " <> list <> ". Its first four letters tell it from all the others."

shown :: Int -> Text
shown = Text.pack . show

-- | The number of words of a secret as a word, as a title writes it.
-- These are all the lengths hw makes. Another one is written in digits.
spelled :: Int -> Text
spelled = \case
  12 -> "twelve"
  15 -> "fifteen"
  18 -> "eighteen"
  20 -> "twenty"
  21 -> "twenty-one"
  24 -> "twenty-four"
  33 -> "thirty-three"
  count -> shown count

capitalized :: Text -> Text
capitalized text = Text.toUpper (Text.take 1 text) <> Text.drop 1 text

{- The layout of a page -}

-- | The sheet as the bytes of its PDF file, made at the time given: the
-- pages of its kind, with what hw knows of the wallet in their fields
sheetPdf :: ZonedTime -> Maybe SheetWallet -> SheetKind -> BS.ByteString
sheetPdf created walletM kind =
  pdfDocument (DocumentInfo (documentTitleOf kind) created) paper (pageMarks walletM <$> sheetPages kind)

-- | The title a PDF reader shows for the file
documentTitleOf :: SheetKind -> Text
documentTitleOf = \case
  MnemonicSheet count -> "Mnemonic Backup Sheet, " <> shown count <> " words"
  SharesSheet _ 1 count -> "Backup Share Sheet, 1 share of " <> shown count <> " words"
  SharesSheet threshold shares count ->
    "Backup Share Sheets, " <> shown threshold <> " of " <> shown shares <> " shares of " <> shown count <> " words"

-- | The marks of a page, from its top to its foot: the title block, the
-- fields, the lines for the words, the notes and the foot.
--
-- The notes end where the blocks of a report end, above the foot, and
-- the lines take all the room that is left above them.
pageMarks :: Maybe SheetWallet -> SheetPage -> [Mark]
pageMarks walletM page =
  titleMarks (pageKind page) (pageTitle page) (pageSubtitle page)
    <> fieldsMarks walletM
    <> headingMarks wordsHeadingBaseline (pageWordsTitle page) "One word on each line, in block capitals"
    <> concatMap (wordLineMarks rows rowHeight) [1 .. pageWordCount page]
    <> headingMarks notesHeadingBaseline "Keeping this sheet safe" ""
    <> notesMarks notesTop (pageNotes page)
    <> footerMarks (pageTitle page) "Keep offline \183 Do not copy, scan or photograph"
  where
    -- The last note needs no room below it
    notesTop = contentBottom - notesHeight (pageNotes page) + 5
    notesHeadingBaseline = notesTop - 18
    wordsBottom = notesHeadingBaseline - 34
    -- The words go down the first column, then down the second, which
    -- is one line shorter when their number is odd
    rows = (pageWordCount page + 1) `div` 2
    rowHeight = (wordsBottom - wordsTop) / fromIntegral rows

-- | The page is laid out in two columns, for the fields and for the
-- words, with this room between them
columnGap, columnWidth :: Double
columnGap = 34
columnWidth = (contentRight - contentLeft - columnGap) / 2

columnLeft, columnRight :: Int -> Double
columnLeft column = contentLeft + fromIntegral column * (columnWidth + columnGap)
columnRight column = columnLeft column + columnWidth

-- | A rule to write on
writingRule :: Double -> Double -> Double -> Mark
writingRule left right y = RuleMark left right y 0.6 grey

-- | The heading of a part of the page, with a remark at the right in
-- lighter type, or none, over a rule
headingMarks :: Double -> Text -> Text -> [Mark]
headingMarks baseline title remark =
  [ textAt contentLeft baseline Bold 12.5 navy title,
    textEndingAt contentRight baseline Regular bodySize grey remark,
    RuleMark contentLeft contentRight (baseline + 9) 0.8 navy
  ]

{- The fields -}

-- | Where what is written of a field starts, to the right of its name
fieldValueLeft :: Int -> Double
fieldValueLeft column = columnLeft column + 62

-- | A field: its name, a rule to write its value on, and the value above
-- the rule when hw knows it
fieldMarks :: Int -> Double -> Text -> Maybe Text -> [Mark]
fieldMarks column baseline name valueM =
  [ textAt (columnLeft column) baseline Regular bodySize grey name,
    writingRule (fieldValueLeft column) (columnRight column) (baseline + 3)
  ]
    <> [textAt (fieldValueLeft column + 2) baseline Regular 10 ink value | Just value <- [valueM]]

-- | A box to tick, with its text after it. A page has filled boxes and
-- no outlines, so the frame is a grey box with a white one inside.
tickBoxMarks :: Double -> Double -> Text -> [Mark]
tickBoxMarks left baseline text =
  [ BoxMark left top side side grey,
    BoxMark (left + line) (top + line) (side - 2 * line) (side - 2 * line) (Colour 1 1 1),
    textAt (left + side + 5) baseline Regular bodySize ink text
  ]
  where
    side = 8.5
    line = 0.7
    top = baseline - side + 1

-- | The fields below the title: the name the owner gives the wallet, its
-- fingerprint, the day, and whether a passphrase goes with the words.
--
-- The passphrase has two boxes and no line: the sheet must say that there
-- is one, or the words alone restore an empty wallet and the funds look
-- lost, and it must not hold the passphrase itself. hw ticks neither box,
-- even for a wallet it just made with a passphrase. Nothing in the
-- database tells that a wallet has one, and a file in the data directory
-- would.
fieldsMarks :: Maybe SheetWallet -> [Mark]
fieldsMarks walletM =
  fieldMarks 0 firstBaseline "Wallet" Nothing
    <> fieldMarks 0 secondBaseline "Date" (longDay . sheetDay <$> walletM)
    <> fieldMarks 1 firstBaseline "Fingerprint" (fingerprintToText . sheetFingerprint <$> walletM)
    <> [textAt (columnLeft 1) secondBaseline Regular bodySize grey "Passphrase"]
    <> tickBoxMarks (fieldValueLeft 1) secondBaseline "None"
    <> tickBoxMarks (fieldValueLeft 1 + 52) secondBaseline "Yes, kept elsewhere"
  where
    firstBaseline = 164
    secondBaseline = 192
    -- As the reports write a day: 31 December 2025
    longDay = Text.pack . formatTime defaultTimeLocale "%-d %B %Y"

{- The words -}

-- | The baseline of the heading of the words, and where their lines
-- start below its rule
wordsHeadingBaseline, wordsTop :: Double
wordsHeadingBaseline = 236
wordsTop = wordsHeadingBaseline + 9

-- | The line of a word, given how many lines a column has and how high
-- each is: the number of the word, then the rule the word is written on.
--
-- A line has the height given, with its rule near its foot: 12 points
-- above it on a sheet of 12 words, and a fifth of the height where the
-- lines are closer, so that the first word keeps its room below the
-- heading. The number is as large as the line allows, up to the 17 points
-- it has on a sheet of 12 words, so that it never crowds the line above.
wordLineMarks :: Int -> Double -> Int -> [Mark]
wordLineMarks rows rowHeight number =
  [ textEndingAt (left + 24) (lineY - 4 * numberSize / 17) Bold numberSize navy (shown number),
    writingRule (left + 36) (columnRight column) lineY
  ]
  where
    (column, row) = (number - 1) `divMod` rows
    left = columnLeft column
    numberSize = min 17 (rowHeight / 2)
    lineY = wordsTop + rowHeight * fromIntegral (row + 1) - min 12 (rowHeight / 5)

{- The sheet hw keeps for a wallet -}

-- | The file of the sheet of a wallet, given the database, the index of
-- the wallet and the fingerprint of its seed: in the directory
-- mnemonic-sheets next to the database, ~\/.hw\/mnemonic-sheets for the
-- database of hw.
--
-- The name starts with the wallet, as in wallet0, so that the sheets of
-- two wallets never take the same path. The fingerprint follows, since
-- an index is the place of a wallet in one database: after a wallet is
-- deleted, or the database restored from a copy, the same index may name
-- another wallet. A sheet left from before then still says whose it is.
walletSheetFile :: FilePath -> Natural -> Fingerprint -> FilePath
walletSheetFile database index fingerprint =
  takeDirectory database </> "mnemonic-sheets" </> ("wallet" <> show index <> "-" <> cs (fingerprintToText fingerprint) <> "-mnemonic-sheet.pdf")
