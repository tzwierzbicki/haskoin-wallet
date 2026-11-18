-- | Text that hw writes for a person, outside the documents of an answer:
-- an error, a question, a warning. Each is a sentence or a few, written
-- as one line where the code makes it, which would run past the edge of
-- the screen and be cut where the terminal breaks it, in the middle of a
-- word. Here it is broken between words, at the width hw lays its
-- answers out for ('screenColumns').
--
-- The width is fixed, and not that of the terminal, as for the answers
-- and the help: a text then reads the same on a screen, in a file and
-- in a test.
module Haskoin.Wallet.Cli.Wrap
  ( screenColumns,
    wrapText,
    warn,
    dieWith,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Data.List.Split (splitOn)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

-- | The width hw writes for, in columns
screenColumns :: Int
screenColumns = 80

-- | The text with its long lines broken between words, so that none is
-- wider than 'screenColumns' when its words allow.
--
-- The lines the text has are kept: a message of several lines stays in
-- its lines. A line that starts with a space is kept whole, however
-- long: it is a command to copy, or a row of a table. A word wider than
-- the screen, such as the path of a file, has a line of its own and
-- runs past the edge, so that it can still be copied in one piece.
--
-- The spaces a line ends with are kept after its last word: a question
-- ends with one, and the answer is typed after it.
wrapText :: String -> String
wrapText = intercalate "\n" . concatMap wrapLine . splitOn "\n"

-- | The lines a line of a text takes on the screen
wrapLine :: String -> [String]
wrapLine line
  | take 1 line == " " || length line <= screenColumns = [line]
  | otherwise = case filled (splitOn " " text) of
      [] -> [line]
      rows -> init rows <> [last rows <> trailing]
  where
    text = dropWhileEnd isSpace line
    trailing = drop (length text) line

-- | Words put on lines, each line taking the words that fit
filled :: [String] -> [String]
filled [] = []
filled (word : rest) = go word rest
  where
    go row [] = [row]
    go row (next : others)
      | length row + 1 + length next <= screenColumns = go (row <> " " <> next) others
      | otherwise = row : go next others

-- | Write a message for the user on stderr, wrapped: a warning that
-- stops nothing
warn :: String -> IO ()
warn = hPutStrLn stderr . wrapText

-- | End hw as a failure, with a message on stderr, wrapped
dieWith :: String -> IO a
dieWith = die . wrapText
