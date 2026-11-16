{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The setup of hw: the questions plain hw asks the first time it is run,
-- when the data directory has no configuration file, and the file it
-- writes from the answers.
--
-- hw needs three choices that it cannot make well for its user. They are
-- the networks to show, the default network and the default type of
-- address ("Haskoin.Wallet.Config"). hw used to write a file of defaults
-- the first time any command ran. A user then found out about these
-- settings only by reading the documentation, often after receiving on a
-- type of address they did not want. So the setup asks, and explains each
-- choice in a few sentences before it does. Every other setting keeps its
-- default, and stays in the file as a comment.
--
-- A fourth question asks whether hw keeps a journal for ledger, the
-- accounting program ('journalQuestion'). hw could go without asking:
-- no journal is a sound default, and it stays the default answer. The
-- question is there because the journal is a feature a user does not go
-- looking for. A user who keeps accounts learns here that hw can feed
-- them, and one who does not presses Enter. The question says what
-- ledger is, since most users of a wallet have never met it.
--
-- Only plain hw runs the setup. A command run before it is refused, and
-- told to run plain hw ('Haskoin.Wallet.Config.notSetUp'). A command does
-- not start the setup by itself: the questions would come in the middle
-- of what the user asked for, and a script would stop on them.
--
-- = Menus on a terminal
--
-- On a terminal, each question is a menu. The networks are a checklist:
-- the arrow keys move a cursor, Space checks or unchecks the row under
-- it, and Enter confirms. The two defaults are lists of which one row is
-- picked with Enter. These are the keys most command-line tools take for
-- such a menu, so a user who has met one knows them, and the menu shows
-- them below its rows for the others. A checklist suits the networks
-- better than a typed list: all eight are shown with what they are, and
-- none has to be spelled.
--
-- A row shows its state in characters, @>@ for the cursor and @[x]@ for a
-- check, and not in colour alone. The menus then read the same with
-- NO_COLOR set, and for a user who does not tell the colours apart.
--
-- The menu is drawn again after each key, over itself. hw moves the
-- cursor of the terminal up by the number of lines the menu took on the
-- screen. A row wider than a narrow terminal wraps and takes several of
-- them, which hw counts from the width of the terminal ('screenLines').
--
-- = Typed answers elsewhere
--
-- Without a terminal on both the input and the error output, there are no
-- keys to read and no cursor to move. The setup then asks the same
-- questions as lines to type, with the default of each in brackets
-- ('OnLines'). A script answers them on the input, a line for each, and
-- an empty line takes a default. A script may also write the
-- configuration file itself, since a file that is there means hw is set
-- up.
--
-- Both ways ask the same questions. Each one is a t'Question', and both
-- ways make what they show from its rows, so that they cannot drift apart.
--
-- The questions go to the error output, as every prompt of hw does
-- ("Haskoin.Wallet.Cli.Prompt"). What the setup wrote goes to the output.
--
-- = After the setup
--
-- A user who has just set hw up has no wallet yet, and may not know
-- which command makes one. So the setup ends with the commands to run
-- next ('firstSteps'): newportfolio, for a new mnemonic or for one the
-- user has already.
module Haskoin.Wallet.Cli.Setup
  ( -- * Running the setup
    needsSetup,
    setupMain,
    Asking (..),
    Screen (..),
    askChoices,
    setupCancelled,

    -- * What the user chose
    Choices (..),
    choiceKeys,
    choiceRows,
    doneDoc,
    firstSteps,

    -- * The questions
    Question (..),
    Row (..),
    networksQuestion,
    defaultNetworkQuestion,
    defaultTypeQuestion,
    journalQuestion,
    journalNote,
    networkTitle,

    -- * A menu
    Menu (..),
    menuOf,
    Key (..),
    keysOf,
    endsInsideEscape,
    pressedOnChecklist,
    pressedOnList,
    checkedRows,
    someChecked,
    pickedRow,
    checklistLines,
    listLines,
    screenLines,

    -- * Typed answers
    readChecked,
  )
where

import Control.Exception (IOException, bracket, try)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Default (def)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network (..), bch, bchRegTest, bchTest, bchTest4, btc, btcRegTest, btcTest, btcTest4)
import Haskoin.Wallet.Cli.PrettyPrinter (hwLine, keyRowsDoc, optionWord, stepsPrinter)
import Haskoin.Wallet.Cli.Prompt (Terminal (..), askAnswer, askChoice, terminal)
import Haskoin.Wallet.Cli.Style (Output, Style (..), outputOf, putDoc, renderPlain)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Keys (AccountKind (..), kindName, portfolioNetworks)
import Haskoin.Wallet.TomlEdit (tomlValue)
import Prettyprinter
import System.Console.ANSI (hClearFromCursorToScreenEnd, hCursorUpLine, hHideCursor, hShowCursor, hSupportsANSI)
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hGetEcho, hIsTerminalDevice, hSetBuffering, hSetEcho, hWaitForInput, stderr, stdin, stdout)
import qualified Toml

{- Running the setup -}

-- | Whether this run of hw is the setup: hw was typed alone, and its data
-- directory has no configuration file. With a file, plain hw shows its
-- help, as it always did.
needsSetup :: IO Bool
needsSetup = do
  args <- getArgs
  if null args then not <$> (hasConfigFile =<< hwDataDirectory) else return False

-- | Run the setup: ask the four questions, write the configuration file
-- of ~/.hw from the answers, and say where it is and how to change it
-- later. A setup that is cancelled, or that fails, writes nothing and
-- ends hw as a failure.
--
-- The setup writes the configuration file and nothing else. A journal
-- the user asked for is written by the next command
-- ('Haskoin.Wallet.Commands.journalIfMissing'), and the setup says so
-- ('journalNote'). The journal is read from the database, which the
-- setup does not open: a database of an earlier hw may be there, and
-- the command that meets it says what to do.
--
-- A ~/.hw that links to a directory that is not there is refused before
-- the first question. Its drive is not mounted, and the answers could
-- not be written.
setupMain :: IO ()
setupMain = do
  dir <- hwDataDirectory
  done <- runExceptT $ do
    checkDataDirectoryLink dir
    asking <- liftIO askingHere
    choices <- askChoices asking
    file <- writeFirstConfig dir (choiceKeys choices)
    return (file, choices)
  case done of
    Left problem -> die problem
    Right (file, choices) -> do
      output <- outputOf stdout
      putDoc stdout output (doneDoc file (journalFile def {configDataDirectory = dir}) choices)

-- | How the setup asks its questions
data Asking
  = -- | With menus, on a terminal that takes keys and cursor moves
    OnMenus !Screen
  | -- | With lines typed, on anything else: a pipe, a file, a terminal
    -- that takes no cursor moves
    OnLines !Terminal

-- | The terminal the menus are on: where the keys come from, where the
-- menus are drawn, and whether they are drawn in colour
data Screen = Screen
  { screenInput :: !Handle,
    screenOutput :: !Handle,
    screenStyle :: !Output
  }

-- | How to ask in this run of hw. The menus need a terminal to read keys
-- from, and one that moves its cursor to draw them on. The questions are
-- drawn on the error output, where every prompt of hw goes.
askingHere :: IO Asking
askingHere = do
  keys <- hIsTerminalDevice stdin
  moves <- hSupportsANSI stderr
  style <- outputOf stderr
  return $ if keys && moves then OnMenus (Screen stdin stderr style) else OnLines terminal

-- | Ask the four questions, in the way given, and give what the user
-- chose. The default network is asked among the networks just chosen,
-- since it must be one hw shows.
askChoices :: Asking -> ExceptT String IO Choices
askChoices asking = do
  say asking introDoc
  shown <- askSeveral asking networksQuestion
  defaultNetwork <- askOne asking (defaultNetworkQuestion shown)
  defaultType <- askOne asking defaultTypeQuestion
  journal <- askOne asking journalQuestion
  -- A line between the last answer and what the setup wrote
  say asking mempty
  return (Choices shown defaultNetwork defaultType journal)

-- | What ends a setup the user left with Escape or Ctrl-D
setupCancelled :: String
setupCancelled = "The setup was cancelled. Nothing was written."

{- What the user chose -}

-- | What the user chose in the setup
data Choices = Choices
  { -- | The networks hw shows, in the order of the networks of a
    -- portfolio
    chosenShown :: ![Network],
    -- | The default network, one of those shown
    chosenNetwork :: !Network,
    -- | The default type of address
    chosenType :: !AccountKind,
    -- | Whether hw keeps a journal for ledger
    chosenJournal :: !Bool
  }
  deriving (Eq, Show)

-- | The keys of the configuration file the choices set, with their
-- values, in the order the file shows them
choiceKeys :: Choices -> [(Text, Toml.Value)]
choiceKeys choices =
  [ (defaultNetworkKey, Toml.Text (networkName choices.chosenNetwork)),
    (shownNetworksKey, Toml.List (Toml.Text . networkName <$> choices.chosenShown)),
    (defaultTypeKey, Toml.Text (kindName choices.chosenType)),
    (journalKey, Toml.Bool choices.chosenJournal)
  ]

-- | The choices as the setup shows them once they are written: each key
-- of the file with its value, as setdefault and showdefault show them
choiceRows :: Choices -> [(Text, Text)]
choiceRows choices =
  [ (shownNetworksKey, Text.intercalate ", " (networkName <$> choices.chosenShown)),
    (defaultNetworkKey, networkName choices.chosenNetwork),
    (defaultTypeKey, kindName choices.chosenType),
    (journalKey, tomlValue (Toml.Bool choices.chosenJournal))
  ]

-- | What the setup says once the file is written: where it is, what it
-- sets, where the journal will be when the user asked for one
-- ('journalNote'), the two ways to change the settings later, and the
-- commands to run next ('firstSteps'). setdefault is in the colour of a
-- command, so that it stands out as what to type.
--
-- The first path is the configuration file, and the second the journal
-- hw keeps ('Haskoin.Wallet.Config.journalFile').
--
-- The steps come last, apart from the rest. They are what the user does
-- now, so they are the last lines on the screen. The settings above them
-- are for later.
doneDoc :: FilePath -> FilePath -> Choices -> Doc Style
doneDoc file journal choices =
  vsep $
    ["hw is set up. Your settings were written to" <+> annotate UserText (pretty file)]
      <> keyRowsDoc (choiceRows choices)
      <> [journalNote journal | choices.chosenJournal]
      <> [ mempty,
           fillSep $
             plain "To change them later, run"
               <> [annotate Command "hw setdefault" <> ","]
               <> plain "or edit that file. It lists every setting of hw, with its default.",
           mempty
         ]
      <> maybeToList (stepsPrinter firstSteps)
  where
    -- The words of a sentence, which the line breaks between
    plain :: Text -> [Doc Style]
    plain = map pretty . Text.words

-- | What the setup says of the journal the user asked for: where the
-- file will be, for the user to include it in a journal of their own,
-- and that the next command writes it. The setup writes no journal
-- itself ('setupMain'), so it does not say that the file is there.
journalNote :: FilePath -> Doc Style
journalNote journal =
  "Your next command writes the journal for ledger to" <+> annotate UserText (pretty journal)

-- | What to run once hw is set up, each step with what it does and its
-- command, as the commands of a payment show their next steps.
--
-- A new user has no wallet, and no command but newportfolio works
-- without one. So the setup names it, in its two forms: a new mnemonic,
-- or a mnemonic the user has already. The help comes last, for the
-- rest. The steps are the same for every answer of the setup. They do
-- not look at the database either: the setup runs before hw opens it.
firstSteps :: [(String, Doc Style)]
firstSteps =
  [ ("Next, create a wallet, with a new mnemonic to write down", hwLine "newportfolio" []),
    ("Or create the wallet of a mnemonic you have already", hwLine "newportfolio" [optionWord "--existing"]),
    ("To see what else hw does", hsep ["hw", optionWord "--help"])
  ]

{- The questions -}

-- | A question of the setup: what it asks, why it matters, and the rows
-- to choose from. The menus and the typed answers are both made from it.
data Question a = Question
  { -- | The question, on a line of its own
    questionTitle :: !Text,
    -- | What the choice changes, in a paragraph of a few sentences
    questionWhy :: !Text,
    -- | What the typed answer is asked with, before the default in
    -- brackets
    questionPrompt :: !Text,
    -- | What can be chosen. The first row is the default of a list.
    questionRows :: ![Row a]
  }

-- | A row of a question
data Row a = Row
  { -- | What choosing the row gives
    rowValue :: !a,
    -- | Its name, as shown and as typed
    rowName :: !Text,
    -- | A few words about it, shown after its name
    rowNote :: !Text,
    -- | Whether a checklist starts with the row checked
    rowChecked :: !Bool
  }
  deriving (Eq, Show)

-- | The first question: the networks hw shows. btc and bch start
-- checked, as in a configuration that does not say
-- ('configShownNetworks').
networksQuestion :: Question Network
networksQuestion =
  Question
    { questionTitle = "1. Which networks do you work with?",
      questionWhy =
        "A wallet of hw has accounts on eight networks: Bitcoin, Bitcoin Cash, and three test networks of each. \
        \hw shows the networks you check here, and hides the others. \
        \A hidden network is left out of every answer, and no command can name it. \
        \It is not lost. hw keeps it up to date, and you can show it later.",
      questionPrompt = "Networks to show, separated by spaces",
      questionRows = [Row net (networkName net) (networkTitle net) (net `elem` [btc, bch]) | net <- portfolioNetworks]
    }

-- | What a network is, in a few words, for a user who knows its coin but
-- not its short name
networkTitle :: Network -> Text
networkTitle net = maybe "" snd (listToMaybe [titled | titled@(named, _) <- titles, named.name == net.name])
  where
    titles =
      [ (btc, "Bitcoin"),
        (bch, "Bitcoin Cash"),
        (btcTest, "Bitcoin, test network 3"),
        (btcTest4, "Bitcoin, test network 4"),
        (btcRegTest, "Bitcoin, a test chain of your own"),
        (bchTest, "Bitcoin Cash, test network 3"),
        (bchTest4, "Bitcoin Cash, test network 4"),
        (bchRegTest, "Bitcoin Cash, a test chain of your own")
      ]

-- | The second question: the default network, among the networks the
-- user just chose to show. The first of them is the default answer. hw
-- always has a default network, so the question offers no way out of
-- one.
defaultNetworkQuestion :: [Network] -> Question Network
defaultNetworkQuestion shown =
  Question
    { questionTitle = "2. Which network is the default?",
      questionWhy =
        "Many commands work on one network. receive, for instance, gives out an address of one network. \
        \Such a command takes the default network when you do not give it --network. \
        \The option still names another network for one command.",
      questionPrompt = "Default network: " <> orList (networkName <$> shown),
      questionRows = [Row net (networkName net) (networkTitle net) False | net <- shown]
    }

-- | The third question: the default type of address. bip44 comes first,
-- as in a configuration that does not say ('configDefaultType').
defaultTypeQuestion :: Question AccountKind
defaultTypeQuestion =
  Question
    { questionTitle = "3. Which type of address is the default?",
      questionWhy =
        "On Bitcoin, hw holds two types of address, in two accounts of the same wallet. \
        \bip44 is the legacy type. Its addresses start with 1, and every service can pay them. \
        \bip84 is native SegWit. Its addresses start with bc1q, and spending from them costs less in fees. \
        \A command that works on one type, such as receive, takes the default type when you do not give it --type. \
        \Bitcoin Cash has bip44 addresses only, whatever you choose here.",
      questionPrompt = "Default type of address: " <> orList (kindName <$> [Bip44, Bip84]),
      questionRows =
        [ Row Bip44 (kindName Bip44) "Legacy addresses, which start with 1 on Bitcoin" False,
          Row Bip84 (kindName Bip84) "Native SegWit addresses, which start with bc1q: lower fees" False
        ]
    }

-- | The fourth question: whether hw keeps a journal for ledger. No
-- comes first, as in a configuration that does not say
-- ('configJournal'): the journal tells the whole history in plain text,
-- so hw writes it only for a user who asks.
--
-- The question names ledger with what it is and where it comes from. A
-- user of a wallet may never have met it, and the word alone could be
-- taken for something else. It then says what ledger is for, since the
-- journal is worth keeping only to a user who wants those reports.
journalQuestion :: Question Bool
journalQuestion =
  Question
    { questionTitle = "4. Should hw keep a journal for ledger, the accounting program?",
      questionWhy =
        "ledger is a free accounting program of the command line (ledger-cli.org). \
        \It keeps books by double entry from a text file you write, the journal. \
        \Its reports say what each account holds, what you earned and what you spent, over any period. \
        \With yes, hw keeps a journal of your transactions in its data directory, and writes it again after each command that changes it. \
        \You include that file in your own journal once, and your books follow your wallet. \
        \With no, hw exportjournal still writes the journal when you ask.",
      questionPrompt = "Keep a journal for ledger: " <> orList (rowName <$> rows),
      questionRows = rows
    }
  where
    rows =
      [ Row False "no" "hw keeps no journal" False,
        Row True "yes" "hw keeps a journal for ledger up to date" False
      ]

-- | Names as a question lists them: "btc", "btc or bch", "btc, bch or
-- btctest4"
orList :: [Text] -> Text
orList names = case reverse names of
  [] -> ""
  [only] -> only
  final : others -> Text.intercalate ", " (reverse others) <> " or " <> final

{- Asking a question -}

-- | Show something to the user of the setup, where the questions go
say :: Asking -> Doc Style -> ExceptT String IO ()
say asking doc = liftIO $ case asking of
  OnMenus screen -> putDoc screen.screenOutput screen.screenStyle doc
  OnLines _ -> do
    style <- outputOf stderr
    putDoc stderr style doc

-- | What the setup says before its first question
introDoc :: Doc Style
introDoc =
  vsep
    [ annotate Title "Welcome to hw.",
      fillSep (pretty <$> Text.words "hw has no configuration file yet. Four questions set it up, and you can change every answer later.")
    ]

-- | The title of a question and why it matters, above its rows
questionDoc :: Question a -> Doc Style
questionDoc question =
  vsep
    [ mempty,
      annotate Subheading (pretty question.questionTitle),
      mempty,
      indent 3 (fillSep (pretty <$> Text.words question.questionWhy)),
      mempty
    ]

-- | Ask a question of which several rows can be chosen, and give those
-- chosen, in the order of the rows. At least one must be: hw would
-- otherwise show nothing.
askSeveral :: Asking -> Question a -> ExceptT String IO [a]
askSeveral asking question = do
  say asking (questionDoc question)
  case asking of
    OnMenus screen -> do
      chosen <- runMenu screen checklistLines pressedOnChecklist someChecked (menuOf question.questionRows)
      liftIO $ showAnswer screen (names chosen)
      return (rowValue <$> chosen)
    OnLines term -> do
      say asking (vsep (staticChecklist question.questionRows))
      let checkedNames = Text.unwords [row.rowName | row <- question.questionRows, row.rowChecked]
          prompt = Text.unpack (question.questionPrompt <> " [" <> checkedNames <> "] ")
      askAnswer term prompt (readChecked question.questionRows)
  where
    names rows = Text.intercalate ", " (rowName <$> rows)

-- | Ask a question of which one row is chosen, and give it. The first
-- row is the default.
askOne :: Asking -> Question a -> ExceptT String IO a
askOne asking question = do
  say asking (questionDoc question)
  case asking of
    OnMenus screen -> do
      chosen <- runMenu screen listLines pressedOnList pickedRow (menuOf question.questionRows)
      liftIO $ showAnswer screen chosen.rowName
      return chosen.rowValue
    OnLines term -> do
      say asking (vsep (staticList question.questionRows))
      askChoice term (Text.unpack question.questionPrompt) [([row.rowName], row.rowValue) | row <- question.questionRows]

-- | The answer to a menu, on the line where the menu was. The transcript
-- of the setup then keeps each question with its answer, and not the
-- menus.
showAnswer :: Screen -> Text -> IO ()
showAnswer screen answer =
  putDoc screen.screenOutput screen.screenStyle (indent 3 (annotate Good (pretty answer)))

{- A menu -}

-- | A menu: its rows, and the row the cursor is on, from 0. A checklist
-- keeps its checks in its rows ('rowChecked').
data Menu a = Menu
  { menuRows :: ![Row a],
    menuCursor :: !Int
  }
  deriving (Eq, Show)

-- | A menu as it is first shown, with the cursor on its first row
menuOf :: [Row a] -> Menu a
menuOf rows = Menu rows 0

-- | A key as a menu takes it
data Key
  = -- | The Up arrow, or k
    KeyUp
  | -- | The Down arrow, or j
    KeyDown
  | -- | The space bar
    KeySpace
  | -- | Enter
    KeyEnter
  | -- | Escape alone, or Ctrl-D: leave the setup
    KeyCancel
  | -- | Any other key, which a menu ignores
    KeyOther
  deriving (Eq, Show)

-- | The keys in the bytes a terminal sent, in their order.
--
-- An arrow key is an escape sequence: Escape, then @[A@ or @[B@, or @OA@
-- or @OB@ on a terminal in application mode. k and j move too, as they do
-- in less. Escape followed by nothing is the Escape key. Any other escape
-- sequence, such as that of the Right arrow or of a function key, is read
-- whole and ignored. Its bytes are then not taken for keys of their own:
-- the B that ends a sequence is not a letter typed.
keysOf :: BS.ByteString -> [Key]
keysOf bytes = case C8.uncons bytes of
  Nothing -> []
  Just ('\ESC', rest) -> case C8.uncons rest of
    Just (introducer, body)
      | introducer `elem` ['[', 'O'] ->
          -- The parameters of the sequence, then the byte that ends it
          let (_, ending) = C8.span (\c -> c >= ' ' && c <= '?') body
           in case C8.uncons ending of
                Just ('A', after) -> KeyUp : keysOf after
                Just ('B', after) -> KeyDown : keysOf after
                Just (_, after) -> KeyOther : keysOf after
                Nothing -> [KeyOther]
    _ -> KeyCancel : keysOf rest
  Just (char, rest) -> key char : keysOf rest
  where
    key char = case char of
      'k' -> KeyUp
      'j' -> KeyDown
      ' ' -> KeySpace
      '\r' -> KeyEnter
      '\n' -> KeyEnter
      '\EOT' -> KeyCancel
      _ -> KeyOther

-- | Whether the bytes end inside an escape sequence: on Escape itself, or
-- on what opens a sequence. A slow link may deliver an arrow key in two
-- parts. The reader of the keys then waits a moment for the rest, and
-- does not take the first part for the Escape key.
endsInsideEscape :: BS.ByteString -> Bool
endsInsideEscape bytes = any (`BS.isSuffixOf` bytes) ["\ESC", "\ESC[", "\ESCO"]

-- | A checklist after a key: Up and Down move the cursor, which stops at
-- the first and the last row, and Space checks or unchecks the row under
-- it. Any other key leaves it as it is.
pressedOnChecklist :: Key -> Menu a -> Menu a
pressedOnChecklist key menu = case key of
  KeySpace -> menu {menuRows = [if at == menu.menuCursor then row {rowChecked = not row.rowChecked} else row | (at, row) <- zip [0 ..] menu.menuRows]}
  _ -> pressedOnList key menu

-- | A list after a key: Up and Down move the cursor, which stops at the
-- first and the last row. Any other key leaves it as it is.
pressedOnList :: Key -> Menu a -> Menu a
pressedOnList key menu = case key of
  KeyUp -> menu {menuCursor = max 0 (menu.menuCursor - 1)}
  KeyDown -> menu {menuCursor = min (length menu.menuRows - 1) (menu.menuCursor + 1)}
  _ -> menu

-- | The rows of a checklist that are checked, in their order
checkedRows :: Menu a -> [Row a]
checkedRows menu = filter rowChecked menu.menuRows

-- | The answer of a checklist when Enter is pressed: its checked rows, or
-- what to tell the user when none is
someChecked :: Menu a -> Either String [Row a]
someChecked menu = case checkedRows menu of
  [] -> Left "Check at least one network: press Space on its row."
  checked -> Right checked

-- | The answer of a list when Enter is pressed: the row under the cursor
pickedRow :: Menu a -> Either String (Row a)
pickedRow menu = maybe (Left "There is nothing to choose from") Right (listToMaybe (drop menu.menuCursor menu.menuRows))

-- | The lines of a checklist: a row on each, with @>@ on the row under
-- the cursor and @[x]@ on a checked one, then the keys it takes
checklistLines :: Menu a -> [Doc Style]
checklistLines menu =
  [ rowLine (at == menu.menuCursor) (Just row.rowChecked) pad row
    | (at, row) <- zip [0 ..] menu.menuRows
  ]
    <> [mempty, hint "Up and Down move. Space checks or unchecks. Enter confirms."]
  where
    pad = nameWidth menu.menuRows

-- | The lines of a list: a row on each, with @>@ on the row under the
-- cursor, then the keys it takes
listLines :: Menu a -> [Doc Style]
listLines menu =
  [ rowLine (at == menu.menuCursor) Nothing pad row
    | (at, row) <- zip [0 ..] menu.menuRows
  ]
    <> [mempty, hint "Up and Down move. Enter confirms."]
  where
    pad = nameWidth menu.menuRows

-- | The rows of a checklist as the typed answers show them: without a
-- cursor, with the checks a default answer keeps
staticChecklist :: [Row a] -> [Doc Style]
staticChecklist rows = [rowLine False (Just row.rowChecked) (nameWidth rows) row | row <- rows] <> [mempty]

-- | The rows of a list as the typed answers show them: without a cursor
staticList :: [Row a] -> [Doc Style]
staticList rows = [rowLine False Nothing (nameWidth rows) row | row <- rows] <> [mempty]

-- | The width of the longest name of the rows
nameWidth :: [Row a] -> Int
nameWidth rows = maximum (0 : (Text.length . rowName <$> rows))

-- | A row of a menu on its line: whether the cursor is on it, its check
-- when it has one, its name padded to the width given, and its note
rowLine :: Bool -> Maybe Bool -> Int -> Row a -> Doc Style
rowLine underCursor check pad row =
  (if underCursor then annotate Selected else id) $
    " "
      <+> (if underCursor then ">" else " ")
      <+> maybe mempty box check
      <> pretty (Text.justifyLeft pad ' ' row.rowName)
      <> "  "
      <> annotate Muted (pretty row.rowNote)
  where
    box checked = (if checked then annotate Good "[x]" else "[ ]") <> " "

-- | The keys of a menu, below its rows
hint :: Text -> Doc Style
hint keys = indent 3 (annotate Muted (pretty keys))

-- | Show a menu on the screen and change it as keys are pressed, until
-- Enter gives an answer. The menu is drawn again after each key, over
-- itself, and is gone once it is answered. An answer that is refused is
-- told below the menu, and the menu stays. Escape, Ctrl-D and the end of
-- the input cancel the setup.
--
-- The keys are read one by one, without Enter and without echo, and the
-- cursor of the terminal is hidden meanwhile. The terminal gets back what
-- it had when the menu is done, and when hw is interrupted with Ctrl-C.
runMenu :: Screen -> (Menu a -> [Doc Style]) -> (Key -> Menu a -> Menu a) -> (Menu a -> Either String b) -> Menu a -> ExceptT String IO b
runMenu screen linesOf pressed answerOf start =
  maybe (throwError setupCancelled) return =<< liftIO (withKeys screen (shown Nothing start))
  where
    shown complaint menu = do
      let drawn = linesOf menu <> [indent 3 (annotate Failure (pretty said)) | Just said <- [complaint :: Maybe String]]
      putDoc screen.screenOutput screen.screenStyle (vsep drawn)
      hFlush screen.screenOutput
      keys <- readKeys screen.screenInput
      -- Back to the first line of the menu, which is then drawn again or
      -- replaced by its answer
      columns <- fmap Terminal.width <$> Terminal.hSize screen.screenOutput
      hCursorUpLine screen.screenOutput (screenLines columns (length . renderPlain <$> drawn))
      hClearFromCursorToScreenEnd screen.screenOutput
      after menu keys
    after menu keys = case keys of
      [] -> shown Nothing menu
      KeyCancel : _ -> return Nothing
      KeyEnter : _ -> case answerOf menu of
        Right answer -> return (Just answer)
        Left complaint -> shown (Just complaint) menu
      key : others -> after (pressed key menu) others

-- | How many lines of the screen the lines of a menu take, given the
-- width of the terminal, when it is known, and the length of each line.
-- A line longer than the terminal is wide wraps onto the next ones. An
-- empty line still takes one. Without a width, each line counts as one.
screenLines :: Maybe Int -> [Int] -> Int
screenLines columnsM lengths = sum (taken <$> lengths)
  where
    taken len = case columnsM of
      Just columns | columns > 0 -> max 1 ((len + columns - 1) `div` columns)
      _ -> 1

-- | Run an action that reads keys from the screen: each key as it is
-- pressed, without echo, with the cursor hidden. What the terminal had
-- before is put back afterwards, whatever happens.
withKeys :: Screen -> IO a -> IO a
withKeys screen action = bracket enter leave (const action)
  where
    input = screen.screenInput
    enter = do
      buffering <- hGetBuffering input
      echo <- hGetEcho input
      hSetBuffering input NoBuffering
      hSetEcho input False
      hHideCursor screen.screenOutput
      return (buffering, echo)
    leave (buffering, echo) = do
      hShowCursor screen.screenOutput
      hFlush screen.screenOutput
      hSetEcho input echo
      hSetBuffering input buffering

-- | The keys pressed since the last read, at least one: the read waits
-- for a key. The end of the input reads as a cancel. When the bytes end
-- inside an escape sequence, the rest is waited for a twentieth of a
-- second ('endsInsideEscape').
readKeys :: Handle -> IO [Key]
readKeys input = do
  bytes <- BS.hGetSome input 64
  if BS.null bytes
    then return [KeyCancel]
    else do
      rest <- if endsInsideEscape bytes then followingBytes else return BS.empty
      return (keysOf (bytes <> rest))
  where
    followingBytes = do
      more <- try (hWaitForInput input 50) :: IO (Either IOException Bool)
      if more == Right True then BS.hGetSome input 64 else return BS.empty

{- Typed answers -}

-- | The rows a typed answer to a checklist names, in the order of the
-- rows, each once. The names are separated by spaces or commas, in any
-- case. An empty answer takes the rows checked by default. A name that is
-- none of the rows is refused, with the names there are.
readChecked :: [Row a] -> Text -> Either String [a]
readChecked rows typed
  | null names = Right [row.rowValue | row <- rows, row.rowChecked]
  | otherwise = do
      mapM_ known names
      return [row.rowValue | row <- rows, Text.toLower row.rowName `elem` names]
  where
    names = filter (not . Text.null) (Text.split (`elem` [' ', ',', '\t']) (Text.toLower typed))
    known name =
      unless (name `elem` (Text.toLower . rowName <$> rows)) $
        Left (show name <> " is none of " <> intercalate ", " (Text.unpack . rowName <$> rows))
