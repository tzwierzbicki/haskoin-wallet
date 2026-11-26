{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A menu on a terminal: the rows of a question
-- ("Haskoin.Wallet.Cli.Question"), a cursor that the arrow keys move, and
-- an answer given with Enter. A checklist checks several rows with Space,
-- and a list picks the row under the cursor.
--
-- These are the keys most command-line tools take for such a menu, so a
-- user who has met one knows them, and the menu shows them below its rows
-- for the others.
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
-- What a key does to a menu is pure ('pressedOnChecklist',
-- 'pressedOnList'), and so are its lines ('checklistLines', 'listLines').
-- 'runMenu' alone reads the keys and draws.
module Haskoin.Wallet.Cli.Question.Menu
  ( -- * The rows
    Row (..),
    staticChecklist,
    staticList,

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

    -- * On the screen
    Screen (..),
    runMenu,
    showAnswer,
  )
where

import Control.Exception (IOException, bracket, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Cli.Style (Output, Style (..), putDoc, renderPlain)
import Prettyprinter
import System.Console.ANSI (hClearFromCursorToScreenEnd, hCursorUpLine, hHideCursor, hShowCursor)
import qualified System.Console.Terminal.Size as Terminal
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hGetEcho, hSetBuffering, hSetEcho, hWaitForInput)

{- The rows -}

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

{- A menu -}

-- | A menu: its rows, and the row the cursor is on, from 0. A checklist
-- keeps its checks in its rows ('rowChecked').
data Menu a = Menu
  { menuRows :: ![Row a],
    menuCursor :: !Int
  }
  deriving (Eq, Show)

-- | A menu as it is first shown, with the cursor on its first row. A
-- question may start its menu on another row
-- ('Haskoin.Wallet.Cli.Question.questionCursor').
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
  | -- | Escape alone, or Ctrl-D: leave the questions
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

{- On the screen -}

-- | The terminal the menus are on: where the keys come from, where the
-- menus are drawn, and whether they are drawn in colour
data Screen = Screen
  { screenInput :: !Handle,
    screenOutput :: !Handle,
    screenStyle :: !Output
  }

-- | Show a menu on the screen and change it as keys are pressed, until
-- Enter gives an answer. The menu is drawn again after each key, over
-- itself, and is gone once it is answered. An answer that is refused is
-- told below the menu, and the menu stays. Escape, Ctrl-D and the end of
-- the input cancel the menu, which then gives no answer: the caller says
-- what was cancelled.
--
-- The keys are read one by one, without Enter and without echo, and the
-- cursor of the terminal is hidden meanwhile. The terminal gets back what
-- it had when the menu is done, and when hw is interrupted with Ctrl-C.
runMenu :: Screen -> (Menu a -> [Doc Style]) -> (Key -> Menu a -> Menu a) -> (Menu a -> Either String b) -> Menu a -> IO (Maybe b)
runMenu screen linesOf pressed answerOf start =
  withKeys screen (shown Nothing start)
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

-- | The answer to a menu, on the line where the menu was. The transcript
-- of the questions then keeps each one with its answer, and not the
-- menus.
showAnswer :: Screen -> Text -> IO ()
showAnswer screen answer =
  putDoc screen.screenOutput screen.screenStyle (indent 3 (annotate Good (pretty answer)))
