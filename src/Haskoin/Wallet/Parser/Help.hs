{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The help of the program and of its commands, and the report of a
-- command line that cannot be parsed, laid out from the specifications of
-- "Haskoin.Wallet.Parser.Args". The layout is the one optparse-applicative
-- printed before: 80 columns, the help of an option at column 28.
module Haskoin.Wallet.Parser.Help
  ( programHelp,
    commandHelp,
    usageError,
    usageParts,
    putHelp,
  )
where

import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes, fromMaybe)
import Haskoin.Wallet.Parser.Args
import qualified Options.Applicative.Help.Chunk as O
import qualified Options.Applicative.Help.Types as O
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color)
import qualified Prettyprinter.Render.Terminal as Terminal
import qualified Prettyprinter.Render.Text as Text
import System.Console.ANSI (hSupportsANSI)
import System.IO (Handle, hPutStrLn)

{- The layout -}

-- | The width of a line
lineWidth :: Int
lineWidth = 80

-- | The columns the names of an option are padded to. With the indent of
-- the rows and a space, its help starts at column 28.
nameWidth :: Int
nameWidth = 25

-- | The row of -h,--help, which every help lists
helpRow :: OptDoc
helpRow =
  OptDoc
    { names = [Short 'h', Long "help"],
      metavar = Nothing,
      help = Just "Show this help text",
      defaultShown = Nothing
    }

{- The help -}

-- | The help of the program: its usage, what it does, its options, its
-- commands by group, and its details
programHelp :: Program cmd opts -> Doc AnsiStyle
programHelp program =
  sections $
    [ Just (programUsage program),
      Just (indent 2 (paragraph program.intro)),
      Just (titled "Available options:" (map optionRow (helpRow : program.globals.rows)))
    ]
      <> map (Just . groupRows) program.groups
      <> [paragraphs program.details]
  where
    groupRows :: CommandGroup cmd -> Doc AnsiStyle
    groupRows commandGroup = titled (pretty commandGroup.title) (map commandRow commandGroup.commands)

-- | The help of a command: its usage, its summary, its options and its
-- details
commandHelp :: Program cmd opts -> CommandSpec cmd -> Doc AnsiStyle
commandHelp program command =
  sections
    [ Just (commandUsage program command),
      Just (indent 2 (summaryOf command)),
      Just (titled "Available options:" (map optionRow (command.args.rows <> [helpRow]))),
      paragraphs command.details
    ]

-- | The report of a command line optparse-applicative could not parse:
-- what is wrong, as it says it, then the usage of the command named, or
-- of the program when none was
usageError :: O.ParserHelp -> Program cmd opts -> Maybe (CommandSpec cmd) -> Doc AnsiStyle
usageError parserHelp program command =
  sections
    [ O.unChunk (O.helpError parserHelp),
      O.unChunk (O.helpSuggestions parserHelp),
      Just (maybe (programUsage program) (commandUsage program) command),
      Just (indent 2 (maybe (paragraph program.intro) summaryOf command))
    ]

programUsage :: Program cmd opts -> Doc AnsiStyle
programUsage program =
  usageLine program.name (program.commandUsage : usageParts program.globals.usage)

commandUsage :: Program cmd opts -> CommandSpec cmd -> Doc AnsiStyle
commandUsage program command =
  usageLine (program.name <> " " <> command.name) (usageParts command.args.usage)

-- | "Usage: hw preparetx", then the parts of the usage, wrapped under the
-- first of them
usageLine :: String -> [String] -> Doc AnsiStyle
usageLine command [] = "Usage:" <+> pretty command
usageLine command parts = "Usage:" <+> pretty command <+> align (fillSep (map pretty parts))

-- | A heading, and its rows under it
titled :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
titled heading rows = vcat (heading : rows)

-- | A row of the help: the names of an option, or of a command, padded to
-- 'nameWidth', then its help, wrapped under itself. Names too wide for the
-- padding put the help on the next line.
row :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
row names description = indent 2 (fillBreak nameWidth names <+> align description)

optionRow :: OptDoc -> Doc AnsiStyle
optionRow doc = row (pretty (rowNames doc)) (fillSep (helpWords <> defaultShown))
  where
    helpWords = map pretty (words (fromMaybe "" doc.help))
    -- The default is kept on one line
    defaultShown = [parens ("default:" <+> pretty v) | Just v <- [doc.defaultShown]]

commandRow :: CommandSpec a -> Doc AnsiStyle
commandRow command = row (pretty command.name) (summaryOf command)

-- | The summary of a command, marked with a red ! when it asks for a
-- mnemonic or private keys
summaryOf :: CommandSpec a -> Doc AnsiStyle
summaryOf command
  | command.offline = annotate (color Red) "! " <> paragraph command.summary
  | otherwise = paragraph command.summary

-- | The names of an option in its row, -a,--account NAME, or the metavar
-- of an argument
rowNames :: OptDoc -> String
rowNames doc = unwords (filter (not . null) [intercalate "," (map nameText doc.names), fromMaybe "" doc.metavar])

-- | Words wrapped to the width
paragraph :: String -> Doc AnsiStyle
paragraph = fillSep . map pretty . words

-- | Paragraphs separated by blank lines, each reflowed, except one whose
-- lines all start with a space, such as a list or an example, kept as
-- written. Nothing when there is no text.
paragraphs :: String -> Maybe (Doc AnsiStyle)
paragraphs text
  | null blocks = Nothing
  | otherwise = Just (sections (map (Just . block) blocks))
  where
    blocks = filter (not . null) (splitWhen (all isSpace) (lines text))
    block ls
      | all (" " `isPrefixOf`) ls = vcat (map pretty ls)
      | otherwise = paragraph (unwords ls)

-- | The sections of a help, separated by blank lines. A section may be
-- missing.
sections :: [Maybe (Doc AnsiStyle)] -> Doc AnsiStyle
sections = concatWith (\a b -> a <> hardline <> hardline <> b) . catMaybes

{- The usage line -}

-- | The parts of a usage line, one for each option in a row. An optional
-- one is in brackets. A choice is in parentheses, its alternatives
-- separated by bars, as is a required option with several names.
usageParts :: Usage -> [String]
usageParts = \case
  NoUsage -> []
  Sequence a b -> usageParts a <> usageParts b
  Optional NoUsage -> []
  usage -> [required usage]

-- | A usage that has to be typed, as a part of the line
required :: Usage -> String
required usage = case usage of
  Item doc | length doc.names > 1 -> parenthesized (bare usage)
  Choice _ _ -> parenthesized (bare usage)
  _ -> bare usage
  where
    parenthesized s = "(" <> s <> ")"

-- | A usage without the parentheses around it
bare :: Usage -> String
bare = \case
  NoUsage -> ""
  Item doc -> itemText doc
  Sequence a b -> unwords (usageParts (Sequence a b))
  Choice a b -> intercalate " | " (map required (alternatives (Choice a b)))
  Optional inner -> "[" <> bare inner <> "]"

alternatives :: Usage -> [Usage]
alternatives (Choice a b) = alternatives a <> alternatives b
alternatives usage = [usage]

-- | An option in the usage line, -a|--account NAME, or the metavar of an
-- argument
itemText :: OptDoc -> String
itemText doc = unwords (filter (not . null) [intercalate "|" (map nameText doc.names), fromMaybe "" doc.metavar])

{- Printing -}

-- | Print a help on the handle, in colour when it is a terminal
putHelp :: Handle -> Doc AnsiStyle -> IO ()
putHelp h doc = do
  colour <- hSupportsANSI h
  let stream = layoutPretty (LayoutOptions (AvailablePerLine lineWidth 1.0)) doc
  if colour
    then Terminal.renderIO h stream
    else Text.renderIO h stream
  hPutStrLn h ""
