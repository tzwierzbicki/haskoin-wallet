{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The help of the program and of its commands, and the report of a
-- command line that cannot be parsed, laid out from the specifications of
-- "Haskoin.Wallet.Parser.Args" and styled with "Haskoin.Wallet.Style".
--
-- Each thing the user may type has one look wherever it appears: a
-- command, an option, a metavar. The help finds them in its own text too,
-- so that a paragraph that names signtx shows it as the list of commands
-- does. A command that asks for a mnemonic or private keys has a colour of
-- its own. Printed plain, the list of commands marks it with ! instead.
module Haskoin.Wallet.Parser.Help
  ( programHelp,
    commandHelp,
    usageError,
    usageParts,
    mentionedCommands,
  )
where

import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, nub)
import Data.List.Split (splitWhen)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import qualified Data.Set as Set
import Haskoin.Wallet.Parser.Args
import Haskoin.Wallet.Style
import qualified Options.Applicative.Help.Chunk as O
import qualified Options.Applicative.Help.Types as O
import Prettyprinter
import qualified Prettyprinter.Render.String as String

{- The layout -}

-- | The columns the names of an option are padded to. With the indent of
-- the rows and a space, its help starts at column 28.
nameWidth :: Int
nameWidth = 25

-- | The row of -h, --help, which every help lists
helpRow :: OptDoc
helpRow =
  OptDoc
    { names = [Short 'h', Long "help"],
      metavar = Nothing,
      help = Just "Show this help text",
      defaultShown = Nothing,
      subheading = Nothing
    }

{- The help -}

-- | The help of the program: what it does, its usage, its options, its
-- commands by group, and its details
programHelp :: Output -> Program cmd opts -> Doc Style
programHelp output program =
  sections $
    [ Just (fillSep (proseWords known program.intro <> offlineLegend output)),
      Just (programUsage program),
      Just (optionsSection known program.globals.rows)
    ]
      <> map (Just . groupRows) program.groups
      <> [paragraphs known program.details]
  where
    known = knownWords program
    groupRows :: CommandGroup cmd -> Doc Style
    groupRows commandGroup = titled (pretty commandGroup.title) (map (commandRow output) commandGroup.commands)

-- | The help of a command: what it does, whether to run it offline, its
-- usage, its arguments and options, its details, and the commands they
-- name
commandHelp :: Program cmd opts -> CommandSpec cmd -> Doc Style
commandHelp program command =
  sections
    [ Just (vcat (annotate Title (paragraph command.summary) : offlineNote)),
      Just (commandUsage program command),
      titledIfAny "Arguments:" (map (optionRow known) arguments),
      Just (optionsSection known options),
      paragraphs known command.details,
      seeAlso (commandNamed program) (filter (/= command.name) (mentionedCommands program command.details))
    ]
  where
    known = knownWords program
    (arguments, options) = partitionRows command.args.rows
    -- A sentence on each line, rather than one wrapped mid-sentence
    offlineNote =
      [ annotate OfflineNote (vcat ["This command asks for a mnemonic or private keys.", "Run it on an offline computer if you can."])
      | command.offline
      ]

-- | The report of a command line optparse-applicative could not parse:
-- what is wrong, as it says it, its suggestions, the usage of the command
-- named, or of the program when none was, and where to read more
usageError :: O.ParserHelp -> Program cmd opts -> Maybe (CommandSpec cmd) -> Doc Style
usageError parserHelp program command =
  sections
    [ fmap ((annotate Failure "Error:" <+>) . plainDoc) (O.unChunk (O.helpError parserHelp)),
      fmap (vcat . map (literalLine known) . lines . plainText) (O.unChunk (O.helpSuggestions parserHelp)),
      Just (maybe (programUsage program) (commandUsage program) command),
      Just (annotate Muted (fillSep (map pretty (words "Type") <> [typed] <> map pretty (words readMore))))
    ]
  where
    known = knownWords program
    typed = case command of
      Just c -> pretty program.name <+> commandName c <+> annotate Option "--help"
      Nothing -> pretty program.name <+> annotate Option "--help"
    readMore = case command of
      Just c -> "for the help of " <> c.name <> "."
      Nothing -> "for the list of commands."
    -- The documents of optparse-applicative have its own styles
    plainText = String.renderString . layoutPretty (LayoutOptions Unbounded) . unAnnotate
    plainDoc = pretty . plainText

-- | The legend of the colour of the commands that ask for a mnemonic or
-- private keys, or of the ! that marks them when the help is plain
offlineLegend :: Output -> [Doc Style]
offlineLegend output = case output of
  Styled -> map pretty (words "The commands in") <> [annotate OfflineCommand (pretty offlineColourName)] <> rest
  Plain -> map pretty (words "The commands marked ! ") <> rest
  where
    rest = map pretty (words "ask for a mnemonic or private keys. Run them on an offline computer if you can.")

programUsage :: Program cmd opts -> Doc Style
programUsage program =
  vcat
    [ usageLine (pretty program.name) (annotate Metavar (pretty program.commandUsage) : usageParts program.globals.usage),
      -- Under the first, as a second way to call the program
      indent 7 (pretty program.name <+> annotate Metavar (pretty program.commandUsage) <+> annotate Option "--help")
    ]

commandUsage :: Program cmd opts -> CommandSpec cmd -> Doc Style
commandUsage program command =
  usageLine (pretty program.name <+> commandName command) (usageParts command.args.usage)

-- | "Usage: hw preparetx", then the parts of the usage, wrapped under the
-- first of them
usageLine :: Doc Style -> [Doc Style] -> Doc Style
usageLine command [] = annotate Heading "Usage:" <+> command
usageLine command parts = annotate Heading "Usage:" <+> command <+> align (fillSep parts)

-- | A heading, and its rows under it
titled :: Doc Style -> [Doc Style] -> Doc Style
titled heading rows = vcat (annotate Heading heading : rows)

-- | A heading and its rows, or nothing without rows
titledIfAny :: Doc Style -> [Doc Style] -> Maybe (Doc Style)
titledIfAny _ [] = Nothing
titledIfAny heading rows = Just (titled heading rows)

-- | The rows of the arguments, which have no name, and those of the
-- options
partitionRows :: [OptDoc] -> ([OptDoc], [OptDoc])
partitionRows rows = (filter (null . (.names)) rows, filter (not . null . (.names)) rows)

-- | The options: those without a subheading, then -h, --help, then each
-- subheading with its options under it. The subheadings come in the order
-- of their first option.
optionsSection :: KnownWords -> [OptDoc] -> Doc Style
optionsSection known options =
  sections (Just general : map (Just . subsection) subheadings)
  where
    general = titled "Options:" (map (optionRow known) (filter (isNothing . (.subheading)) options <> [helpRow]))
    subheadings = nub (mapMaybe (.subheading) options)
    subsection title =
      vcat $
        indent 2 (annotate Subheading (pretty title))
          : map (optionRow known) (filter ((== Just title) . (.subheading)) options)

-- | A row of the help: the names of an option, or of a command, padded to
-- 'nameWidth', then its help, wrapped under itself. Names too wide for the
-- padding put the help on the next line.
row :: Doc Style -> Doc Style -> Doc Style
row names description = indent 2 (fillBreak nameWidth names <+> align description)

optionRow :: KnownWords -> OptDoc -> Doc Style
optionRow known doc = row (rowNames doc) (fillSep (proseWords known (fromMaybe "" doc.help) <> defaultShown))
  where
    -- The default is kept on one line
    defaultShown = [annotate Muted (parens ("default:" <+> pretty v)) | Just v <- [doc.defaultShown]]

-- | The names of an option in its row, -a, --account NAME, or the metavar
-- of an argument. A long name without a short one is indented as if it
-- had one, so that the long names line up.
rowNames :: OptDoc -> Doc Style
rowNames doc = hsep (namesPart <> metavarPart)
  where
    shorts = [n | n@(Short _) <- doc.names]
    longs = [n | n@(Long _) <- doc.names]
    styledNames = map (annotate Option . pretty . nameText) (shorts <> longs)
    namesPart
      | null doc.names = []
      | null shorts = [indent 4 (hcat styledNames)]
      | otherwise = [hcat (punctuate ", " styledNames)]
    metavarPart = [annotate Metavar (pretty v) | Just v <- [doc.metavar]]

-- | A command in the list of the program: its name, then its summary.
-- Printed plain, the summary of an offline command starts with !.
commandRow :: Output -> CommandSpec a -> Doc Style
commandRow output command = row (commandName command) (mark <> paragraph command.summary)
  where
    mark
      | command.offline && output == Plain = "! "
      | otherwise = mempty

-- | The name of a command, in the colour of its kind
commandName :: CommandSpec a -> Doc Style
commandName command = annotate (commandStyle command.offline) (pretty command.name)

commandStyle :: Bool -> Style
commandStyle offline = if offline then OfflineCommand else Command

-- | The commands the details of a command name, as "See also:"
seeAlso :: (String -> Maybe (CommandSpec cmd)) -> [String] -> Maybe (Doc Style)
seeAlso commandOf names = case mapMaybe commandOf names of
  [] -> Nothing
  commands -> Just (annotate Heading "See also:" <+> align (fillSep (punctuate "," (map commandName commands))))

commandNamed :: Program cmd opts -> String -> Maybe (CommandSpec cmd)
commandNamed program word = Map.lookup word (commandMap program)

commandMap :: Program cmd opts -> Map.Map String (CommandSpec cmd)
commandMap program = Map.fromList [(c.name, c) | g <- program.groups, c <- g.commands]

{- The words of the text -}

-- | What the help recognizes in its text, to style it: the commands, by
-- name, with whether each one is offline, those of them named by an
-- everyday word, the options of every command, and the metavars
data KnownWords = KnownWords
  { commands :: Map.Map String Bool,
    everyday :: Set.Set String,
    options :: Set.Set String,
    metavars :: Set.Set String
  }

knownWords :: Program cmd opts -> KnownWords
knownWords program =
  KnownWords
    { commands = Map.map (.offline) (commandMap program),
      everyday = Set.fromList program.everydayNames,
      options = Set.fromList [nameText n | r <- allRows, n <- r.names],
      metavars = Set.fromList (program.commandUsage : [v | r <- allRows, Just v <- [r.metavar]])
    }
  where
    allRows = helpRow : program.globals.rows <> [r | g <- program.groups, c <- g.commands, r <- c.args.rows]

-- | What a word of the text is, once the punctuation around it is set
-- aside
data WordKind = CommandWord Bool | OptionWord | MetavarWord | PlainWord

-- | A word of the text, without the punctuation around it, and what it is.
-- A word in backticks loses them: it is a command, or else a plain word.
classify :: KnownWords -> String -> (String, String, WordKind, String)
classify known word = (lead, core, kind, trail)
  where
    (lead, rest) = span (`elem` ("(\"" :: String)) word
    (trailRev, coreRev) = span (`elem` (".,;:)\"" :: String)) (reverse rest)
    body = reverse coreRev
    trail = reverse trailRev
    quoted = "`" `isPrefixOf` body && "`" `isSuffixOf` body && length body > 2
    core = if quoted then drop 1 (init body) else body
    kind
      | quoted = maybe PlainWord CommandWord (Map.lookup core known.commands)
      | Just offline <- Map.lookup core known.commands, not (Set.member core known.everyday) = CommandWord offline
      | Set.member core known.options = OptionWord
      | Set.member core known.metavars = MetavarWord
      | otherwise = PlainWord

-- | A word of the text, styled for what it is
styledWord :: KnownWords -> String -> Doc Style
styledWord known word = pretty lead <> styled <> pretty trail
  where
    (lead, core, kind, trail) = classify known word
    styled = case kind of
      CommandWord offline -> annotate (commandStyle offline) (pretty core)
      OptionWord -> annotate Option (pretty core)
      MetavarWord -> annotate Metavar (pretty core)
      PlainWord -> pretty core

-- | The words of a text, each styled
proseWords :: KnownWords -> String -> [Doc Style]
proseWords known = map (styledWord known) . words

-- | The commands a text names, each once, in the order it names them
mentionedCommands :: Program cmd opts -> String -> [String]
mentionedCommands program text = nub [core | w <- words text, (_, core, CommandWord _, _) <- [classify known w]]
  where
    known = knownWords program

-- | A line kept as written, its words styled and its spaces kept
literalLine :: KnownWords -> String -> Doc Style
literalLine known = hcat . map piece . spansOf
  where
    piece s
      | all isSpace s = pretty s
      | otherwise = styledWord known s
    spansOf [] = []
    spansOf s@(c : _) = let (a, b) = span ((== isSpace c) . isSpace) s in a : spansOf b

{- Paragraphs -}

-- | Words wrapped to the width, unstyled
paragraph :: String -> Doc Style
paragraph = fillSep . map pretty . words

-- | Paragraphs separated by blank lines, each reflowed, except one whose
-- lines all start with a space, such as a list or an example, kept as
-- written. Their words are styled. Nothing when there is no text.
paragraphs :: KnownWords -> String -> Maybe (Doc Style)
paragraphs known text
  | null blocks = Nothing
  | otherwise = Just (sections (map (Just . block) blocks))
  where
    blocks = filter (not . null) (splitWhen (all isSpace) (lines text))
    block ls
      | all (" " `isPrefixOf`) ls = vcat (map (literalLine known) ls)
      | otherwise = fillSep (proseWords known (unwords ls))

-- | The sections of a help, separated by blank lines. A section may be
-- missing.
sections :: [Maybe (Doc Style)] -> Doc Style
sections = concatWith (\a b -> a <> hardline <> hardline <> b) . catMaybes

{- The usage line -}

-- | The parts of a usage line, one for each option in a row. An optional
-- one is in brackets. A choice is in parentheses, its alternatives
-- separated by bars, as is a required option with several names. What may
-- be given several times is followed by ...
usageParts :: Usage -> [Doc Style]
usageParts = \case
  NoUsage -> []
  Sequence a b -> usageParts a <> usageParts b
  Optional NoUsage -> []
  usage -> [required usage]

-- | A usage that has to be typed, as a part of the line
required :: Usage -> Doc Style
required usage = case usage of
  Item doc | length doc.names > 1 -> parenthesized (bare usage)
  Choice _ _ -> parenthesized (bare usage)
  _ -> bare usage
  where
    parenthesized d = "(" <> d <> ")"

-- | A usage without the parentheses around it
bare :: Usage -> Doc Style
bare = \case
  NoUsage -> mempty
  Item doc -> itemDoc doc
  Sequence a b -> hsep (usageParts (Sequence a b))
  Choice a b -> concatWith (\x y -> x <+> "|" <+> y) (map required (alternatives (Choice a b)))
  -- [-c|--coin TXHASH:INDEX]..., rather than [-c|--coin TXHASH:INDEX...]
  Optional (Repeated inner) -> "[" <> bare inner <> "]..."
  Optional inner -> "[" <> bare inner <> "]"
  Repeated inner -> required inner <> "..."

alternatives :: Usage -> [Usage]
alternatives (Choice a b) = alternatives a <> alternatives b
alternatives usage = [usage]

-- | An option in the usage line, -a|--account NAME, or the metavar of an
-- argument
itemDoc :: OptDoc -> Doc Style
itemDoc doc = hsep (namesPart <> metavarPart)
  where
    namesPart
      | null doc.names = []
      | otherwise = [concatWith (\a b -> a <> "|" <> b) (map (annotate Option . pretty . nameText) doc.names)]
    metavarPart = [annotate Metavar (pretty v) | Just v <- [doc.metavar]]
