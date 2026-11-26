{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The questions hw asks about its settings, and the two ways it asks
-- them. The setup of a first run asks four of them
-- ("Haskoin.Wallet.Cli.Setup"), and setconfig without an option asks
-- them all ("Haskoin.Wallet.Cli.SetConfig"). The questions themselves
-- are in "Haskoin.Wallet.Cli.Question.Settings".
--
-- A setting is easier to choose than to look up. A question says what
-- the setting changes in a few sentences, then shows every value it
-- takes, so that none has to be spelled or found in the documentation.
--
-- = Three kinds of question
--
-- A question with rows is a t'Question'. Several of its rows can be
-- chosen ('askSeveral'), as for the networks hw shows, or one
-- ('askOne'), as for the unit. A question whose answers no list could
-- hold, the address of a server or a number, is an t'Entry': its answer
-- is written ('askEntry').
--
-- Each question starts on an answer: the rows checked, the row under the
-- cursor, the value of an entry. Enter alone takes it. The setup starts
-- on the defaults of hw, and setconfig on the values hw has now
-- ('startingOn', 'checking'), so that a user who presses Enter to the
-- end changes nothing.
--
-- = Menus on a terminal, typed answers elsewhere
--
-- On a terminal, a question with rows is a menu
-- ("Haskoin.Wallet.Cli.Question.Menu"): a checklist for several rows, a
-- list for one. A checklist suits the networks better than a typed list:
-- all eight are shown with what they are, and none has to be spelled.
--
-- Without a terminal on both the input and the error output, there are no
-- keys to read and no cursor to move. The same questions are then asked
-- as lines to type, with the answer each starts on in brackets
-- ('OnLines'). A script answers them on the input, a line for each, and
-- an empty line takes the answer in brackets.
--
-- Both ways ask the same questions. Each one is a t'Question', and both
-- ways make what they show from its rows, so that they cannot drift apart.
-- An entry is a line to type in both ways.
--
-- The questions go to the error output, as every prompt of hw does
-- ("Haskoin.Wallet.Cli.Prompt"). What was written goes to the output.
module Haskoin.Wallet.Cli.Question
  ( -- * A question
    Question (..),
    startingOn,
    checking,
    Entry (..),
    orList,

    -- * Asking
    Asking (..),
    Way (..),
    askingHere,
    say,
    askSeveral,
    askOne,
    askEntry,
    cancelledBy,

    -- * Typed answers
    readChecked,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.List (elemIndex, intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Cli.Prompt (ScriptAnswer (ScriptLine), Terminal, askAnswer, askChoiceFrom, cancelled, notUtf8, terminal, terminalOn)
import Haskoin.Wallet.Cli.Question.Menu
import Haskoin.Wallet.Cli.Style (Style (..), outputOf, putDoc)
import Prettyprinter
import System.Console.ANSI (hSupportsANSI)
import System.IO (hIsTerminalDevice, stderr, stdin)

{- A question -}

-- | A question with rows to choose from: what it asks, why it matters,
-- and the rows. The menus and the typed answers are both made from it.
data Question a = Question
  { -- | The question, on a line of its own, after its number
    questionTitle :: !Text,
    -- | What the choice changes, in a paragraph of a few sentences
    questionWhy :: !Text,
    -- | What the typed answer is asked with, before the answer it starts
    -- on, in brackets
    questionPrompt :: !Text,
    -- | What can be chosen
    questionRows :: ![Row a],
    -- | The row the cursor of its menu starts on, from 0. It is the row
    -- a list starts on: the one Enter picks at once, and the one an
    -- empty typed answer picks. A checklist starts on the rows that are
    -- checked ('rowChecked').
    questionCursor :: !Int
  }

-- | The question of which one row is chosen, starting on the row of the
-- value given, as setconfig starts on what hw has now. A value that no
-- row has leaves the question as it is: the default network that the
-- user just hid, for instance.
startingOn :: (Eq a) => a -> Question a -> Question a
startingOn value question =
  maybe question (\at -> question {questionCursor = at}) (elemIndex value (rowValue <$> question.questionRows))

-- | The question of which several rows are chosen, starting with the
-- rows of the values given checked, and no other
checking :: (Eq a) => [a] -> Question a -> Question a
checking values question =
  question {questionRows = [row {rowChecked = row.rowValue `elem` values} | row <- question.questionRows]}

-- | A question answered with a value the user writes, where no list of
-- rows could hold the answers: the address of a server, a number
data Entry a = Entry
  { -- | The question, on a line of its own, after its number
    entryTitle :: !Text,
    -- | What the value changes, in a paragraph of a few sentences
    entryWhy :: !Text,
    -- | What the answer is asked with, before the value it starts on, in
    -- brackets
    entryPrompt :: !Text,
    -- | The value the question starts on, which an empty answer keeps
    entryNow :: !a,
    -- | A value as the brackets show it, and as it is written
    entryName :: !(a -> Text),
    -- | The value an answer writes, or why it is refused
    entryRead :: !(Text -> Either String a)
  }

-- | Names as a question lists them: "btc", "btc or bch", "btc, bch or
-- btctest4"
orList :: [Text] -> Text
orList names = case reverse names of
  [] -> ""
  [only] -> only
  final : others -> Text.intercalate ", " (reverse others) <> " or " <> final

{- Asking -}

-- | How questions are asked, and for what
data Asking = Asking
  { -- | What asks, as the sentence that ends the questions early names
    -- it: "The setup"
    askingFor :: !String,
    askingWay :: !Way
  }

-- | The two ways to ask a question
data Way
  = -- | With menus, on a terminal that takes keys and cursor moves
    OnMenus !Screen
  | -- | With lines typed, on anything else: a pipe, a file, a terminal
    -- that takes no cursor moves
    OnLines !Terminal

-- | How to ask in this run of hw, for what is named. The menus need a
-- terminal to read keys from, and one that moves its cursor to draw them
-- on. The questions are drawn on the error output, where every prompt of
-- hw goes.
askingHere :: String -> IO Asking
askingHere asksFor = do
  keys <- hIsTerminalDevice stdin
  moves <- hSupportsANSI stderr
  style <- outputOf stderr
  return . Asking asksFor $ if keys && moves then OnMenus (Screen stdin stderr style) else OnLines terminal

-- | What ends questions the user left with Escape or Ctrl-D
cancelledBy :: Asking -> String
cancelledBy asking = asking.askingFor <> " was cancelled. Nothing was written."

-- | Show something to the user who is asked, where the questions go
say :: Asking -> Doc Style -> ExceptT String IO ()
say asking doc = liftIO $ case asking.askingWay of
  OnMenus screen -> putDoc screen.screenOutput screen.screenStyle doc
  OnLines _ -> do
    style <- outputOf stderr
    putDoc stderr style doc

-- | The number and the title of a question and why it matters, above
-- what it is answered with
questionDoc :: Int -> Text -> Text -> Doc Style
questionDoc number title why =
  vsep
    [ mempty,
      annotate Subheading (pretty number <> "." <+> pretty title),
      mempty,
      indent 3 (fillSep (pretty <$> Text.words why)),
      mempty
    ]

-- | Ask a question of which several rows can be chosen, under the number
-- given, and give those chosen, in the order of the rows. At least one
-- must be: hw would otherwise show nothing.
askSeveral :: Asking -> Int -> Question a -> ExceptT String IO [a]
askSeveral asking number question = do
  say asking (questionDoc number question.questionTitle question.questionWhy)
  case asking.askingWay of
    OnMenus screen -> do
      chosen <- answered asking =<< liftIO (runMenu screen checklistLines pressedOnChecklist someChecked (menuStart question))
      liftIO $ showAnswer screen (names chosen)
      return (rowValue <$> chosen)
    OnLines term -> do
      say asking (vsep (staticChecklist question.questionRows))
      let checkedNames = Text.unwords [row.rowName | row <- question.questionRows, row.rowChecked]
          prompt = Text.unpack (question.questionPrompt <> " [" <> checkedNames <> "] ")
      typedAnswer asking $ askAnswer term ScriptLine prompt (readChecked question.questionRows)
  where
    names rows = Text.intercalate ", " (rowName <$> rows)

-- | Ask a question of which one row is chosen, under the number given,
-- and give it. Enter alone picks the row the question starts on
-- ('questionCursor').
askOne :: Asking -> Int -> Question a -> ExceptT String IO a
askOne asking number question = do
  say asking (questionDoc number question.questionTitle question.questionWhy)
  case asking.askingWay of
    OnMenus screen -> do
      chosen <- answered asking =<< liftIO (runMenu screen listLines pressedOnList pickedRow (menuStart question))
      liftIO $ showAnswer screen chosen.rowName
      return chosen.rowValue
    OnLines term -> do
      say asking (vsep (staticList question.questionRows))
      typedAnswer asking $
        askChoiceFrom question.questionCursor term ScriptLine (Text.unpack question.questionPrompt) [(typedNames question.questionRows row, row.rowValue) | row <- question.questionRows]

-- | Ask a question whose answer is written, under the number given, and
-- give the value. An empty answer keeps the value the question starts
-- on. It is a line to type in both ways of asking: a menu has no row for
-- an address or a number. On a terminal, Ctrl-D leaves the questions, as
-- it leaves a menu.
askEntry :: Asking -> Int -> Entry a -> ExceptT String IO a
askEntry asking number entry = do
  say asking (questionDoc number entry.entryTitle entry.entryWhy)
  case asking.askingWay of
    OnMenus screen -> withExceptT leftOnTerminal (written (terminalOn screen.screenInput screen.screenOutput))
    OnLines term -> written term
  where
    written term = typedAnswer asking (askAnswer term ScriptLine prompt kept)
    prompt = Text.unpack (entry.entryPrompt <> " [" <> entry.entryName entry.entryNow <> "] ")
    kept typed
      | Text.null typed = Right entry.entryNow
      | otherwise = entry.entryRead typed
    leftOnTerminal problem
      | problem == cancelled = cancelledBy asking
      | otherwise = problem

-- | The menu of a question as it is first shown, with the cursor on the
-- row the question starts on
menuStart :: Question a -> Menu a
menuStart question = (menuOf question.questionRows) {menuCursor = question.questionCursor}

-- | The answer of a menu, or the end of the questions when the user left
-- the menu ('cancelledBy')
answered :: Asking -> Maybe b -> ExceptT String IO b
answered asking = maybe (throwError (cancelledBy asking)) return

-- | The answers that pick a row of a list when typed, in capitals or in
-- small letters: its name, and the first letter of its name when no
-- other row starts with it. So y and n answer a question of yes and no,
-- as they answer every other yes or no question of hw: a script that
-- typed y for the journal got no configuration file.
typedNames :: [Row a] -> Row a -> [Text]
typedNames rows row = row.rowName : [letter | [_] <- [filter (letter `Text.isPrefixOf`) (rowName <$> rows)]]
  where
    letter = Text.take 1 row.rowName

-- | A typed answer to a question, which allows three answers
-- ('Haskoin.Wallet.Cli.Prompt.attempts'). The third one refused ends
-- the questions with its mistake, as it ends any question. A script that
-- had a good answer on its next line could not tell why hw stopped
-- there, so the error adds what stopped and that nothing was written,
-- as it says when the questions are cancelled ('cancelledBy'). An input
-- that ended, or that is not text, is no refused answer, and its error
-- stays as it is.
typedAnswer :: Asking -> ExceptT String IO a -> ExceptT String IO a
typedAnswer asking = withExceptT $ \problem ->
  if problem `elem` [cancelled, notUtf8]
    then problem
    else problem <> ". " <> asking.askingFor <> " stopped after three answers it could not read. Nothing was written."

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
