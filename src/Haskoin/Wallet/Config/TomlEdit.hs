{-# LANGUAGE OverloadedStrings #-}

-- | Reading the text of a TOML file, and setting one of its keys while
-- every other character of the text stays as it is.
--
-- The configuration file of hw is TOML, so that its user can write
-- comments in it ("Haskoin.Wallet.Config"). setconfig changes keys of
-- that file. No TOML library for Haskell writes a file back with its
-- comments: they all read the file into values, and write the values out
-- again. So this module does not write the file out again. It finds where
-- the value of the key is in the text, and puts the new value there
-- ('setTomlKey').
--
-- The place of a value is not guessed from the look of a line. The parser
-- of the library says where each value starts, and its lexer says where
-- the value ends (@valueEnd@). A value in quotes that holds a @#@, or an
-- array on several lines, is then read as TOML reads it.
--
-- A key the file does not have is added as a new line. It goes below the
-- comment that shows its default, such as @# unit = "btc"@, when the file
-- has one, since the words about the key are there. Otherwise it goes at
-- the end of the file.
--
-- Every change is checked before it is given back. The new text is read
-- again, and must hold the settings of the old text with the one key
-- changed, and nothing else. A change that fails this check is refused,
-- with a reason, and the caller tells the user which line to write by
-- hand. Three cases are refused before the check, since their change
-- would lose something or land in the wrong place:
--
-- * a value that is a table, which no setting setconfig writes is
-- * a value with a comment inside, such as an array on several lines with
--   a comment after one of its elements, since the new value would
--   replace the comment too
-- * a key to add to a file that has a table and no comment showing the
--   default of the key, since a line at the end of such a file would be a
--   key of the table
module Haskoin.Wallet.Config.TomlEdit
  ( TomlFile (..),
    readToml,
    setTomlKey,
    tomlLine,
    tomlValue,
    atLine,
  )
where

import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Char (isLetter)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Prettyprinter (LayoutOptions (..), PageWidth (..), layoutPretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Toml.Pretty (TomlDoc, prettySimpleKey, prettyValue)
import Toml.Semantics (SemanticError (..), Table' (..), Value, Value' (..), forgetValueAnns, semantics, valueAnn)
import Toml.Syntax (Context (..), Expr (..), Located (..), Position (..), Token (..), parseRawToml, scanToken)

{- Reading -}

-- | The text of a TOML file, read
data TomlFile = TomlFile
  { -- | The keys at the top of the file, those of no table, each with
    -- the place of the key and its value. The value knows the place it
    -- starts at in the text. A table of the file is one of these keys
    -- too, and its value holds the keys of the table.
    tomlKeys :: Map Text (Position, Value' Position),
    -- | The line of the first table header of the file, such as
    -- @[journal-accounts]@, when it has one. The keys below that line
    -- belong to a table. hw writes no table, so a file hw wrote has
    -- none until its user writes one.
    tomlFirstTable :: Maybe Int
  }

-- | Read the text of a TOML file, or say where it is not TOML and why, as
-- in "at line 3, column 8: parse error: unexpected '='"
readToml :: Text -> Either String TomlFile
readToml text = do
  expressions <- first notSyntax (parseRawToml text)
  MkTable keys <- first notMeaning (semantics expressions)
  return
    TomlFile
      { tomlKeys = keys,
        tomlFirstTable = listToMaybe [posLine place | Just place <- tableHeader <$> expressions]
      }
  where
    notSyntax (Located place reason) =
      "at line " <> show (posLine place) <> ", column " <> show (posColumn place) <> ": " <> reason <> quotesHint place
    -- The mistake most often made in a TOML file is a text without its
    -- quotes, as in: unit = sat. The parser then finds a word where a
    -- value goes, and calls it a bare key. The hint is given when the
    -- word comes right after an equals sign.
    quotesHint place
      | "=" `Text.isSuffixOf` Text.stripEnd (beforeOnLine place) && wordAt place = ". A text goes in quotes, as in \"btc\"."
      | otherwise = ""
    beforeOnLine place = Text.take (posColumn place - 1) (lineAt place)
    lineAt place = fromMaybe "" (listToMaybe (drop (posLine place - 1) (Text.lines text)))
    wordAt place = maybe False (isLetter . fst) (Text.uncons (Text.drop (posColumn place - 1) (lineAt place)))
    -- The library tells three ways a key can be defined twice apart. They
    -- all mean that the file gives the key two values.
    notMeaning problem = atLine (posLine (errorAnn problem)) ("the key " <> show (errorKey problem) <> " is set twice")
    tableHeader expression = case expression of
      TableExpr ((place, _) :| _) -> Just place
      ArrayTableExpr ((place, _) :| _) -> Just place
      KeyValExpr _ _ -> Nothing

-- | A problem of a file, with the line it is at, as in "at line 12: the
-- key ..."
atLine :: Int -> String -> String
atLine line problem = "at line " <> show line <> ": " <> problem

-- | The settings of a file as values alone, without their places, to
-- compare what two texts say
valuesOf :: TomlFile -> Map Text Value
valuesOf file = forgetValueAnns . snd <$> tomlKeys file

{- Writing -}

-- | The line that sets a key to a value, as in @unit = "sat"@, without
-- the end of the line. A text is written in quotes, and a list on one
-- line, whatever its length.
tomlLine :: Text -> Value -> Text
tomlLine key value = onOneLine (prettySimpleKey key <+> "=" <+> prettyValue value)

-- | A value as a TOML file writes it, as in @"sat"@ or @["btc", "bch"]@
tomlValue :: Value -> Text
tomlValue = onOneLine . prettyValue

-- | What the library writes, on one line whatever its length
onOneLine :: TomlDoc -> Text
onOneLine = renderStrict . layoutPretty (LayoutOptions Unbounded)

-- | Set a key of the text of a TOML file to a value, and give the text
-- back with every other character as it was. Or say why the text was not
-- changed. The reason is the end of a sentence that starts with the name
-- of the file, as in "has a table at line 9".
--
-- A key the text has gets the new value where the old one was. What
-- comes after the value on its line, a comment for instance, stays. A
-- key the text does not have is added as a new line (@addLine@).
setTomlKey :: Text -> Value -> Text -> Either String Text
setTomlKey key value text = do
  file <- first ("is not a TOML file, " <>) (readToml text)
  changed <- case Map.lookup key (tomlKeys file) of
    Just (_, old) -> replaceValue key old value text
    Nothing -> addLine file key (tomlLine key value) text
  -- The check of every change: the new text says what the old one said,
  -- but for this key
  let asAsked = (valuesOf <$> readToml changed) == Right (Map.insert key value (valuesOf file))
  unless asAsked $ Left "would not read as before with this key changed"
  return changed

-- | Put a new value in the place of the value of a key. The key and the
-- equals sign before it stay, and so does what follows the value.
replaceValue :: Text -> Value' Position -> Value -> Text -> Either String Text
replaceValue key old value text = do
  when (isTable old) $ Left ("has a table named " <> show key)
  let start = valueAnn old
      (before, fromValue) = Text.splitAt (posIndex start) text
  (end, commented) <- valueEnd (Located start fromValue)
  when commented $
    Left ("has a comment inside the value of " <> show key <> ", at line " <> show (posLine start) <> ", which the change would remove")
  let after = Text.drop (posIndex end - posIndex start) fromValue
  return (before <> tomlValue value <> after)
  where
    isTable (Table' _ _) = True
    -- An array of tables, written with [[name]] headers
    isTable (List' _ elements) = any isTable elements
    isTable _ = False

-- | Where a value ends, given the text from its first character on, and
-- whether a comment sits inside it.
--
-- The lexer of the library reads the value, one token after another. A
-- text, a number or a boolean is one token, and ends where the token
-- does. An array ends at the bracket that closes it, so its tokens are
-- read until that bracket, counting the arrays inside it. The lexer skips
-- the comments, so a @#@ in the gap between two tokens is the start of
-- one. A @#@ inside a text in quotes is part of its token, and no gap.
--
-- A table written inside braces is not followed to its end. No setting
-- this module writes is one, and its keys are read by other rules than
-- its values.
valueEnd :: Located Text -> Either String (Position, Bool)
valueEnd start = do
  (token, rest) <- next start
  case locThing token of
    TokSquareO -> closing (1 :: Int) False rest
    TokCurlyO -> Left "has a table inside braces"
    _ -> Right (locPosition rest, False)
  where
    next = first (const "could not be read again") . scanToken ValueContext
    -- The arrays still open, whether a comment was seen, and the text
    -- after the last token read
    closing 0 commented rest = Right (locPosition rest, commented)
    closing open commented rest = do
      (token, after) <- next rest
      let gap = Text.take (posIndex (locPosition token) - posIndex (locPosition rest)) (locThing rest)
          seen = commented || Text.any (== '#') gap
      case locThing token of
        TokSquareO -> closing (open + 1) seen after
        TokSquareC -> closing (open - 1) seen after
        TokCurlyO -> Left "has a table inside braces"
        TokEOF -> Left "ends inside a list"
        _ -> closing open seen after

-- | Add the line of a key the text does not have.
--
-- The line goes below the last comment that shows a value of the key, as
-- in @# unit = "btc"@. The file hw writes has such a comment for each
-- setting, below the words about it, so the setting lands where the user
-- reads about it, and the default stays in sight. Only a comment above
-- the first table of the file counts. Below it, the line would set a key
-- of that table.
--
-- Without such a comment, the line goes at the end of the file. A file
-- with a table is then refused, for the same reason.
addLine :: TomlFile -> Text -> Text -> Text -> Either String Text
addLine file key line text =
  case (shownAsComment, tomlFirstTable file) of
    (below : _, _) -> Right (Text.intercalate "\n" (take below textLines <> [line] <> drop below textLines))
    ([], Nothing) -> Right (atEnd <> line <> "\n")
    ([], Just table) -> Left ("has a table at line " <> show table <> ", and the key " <> show key <> " must come before it")
  where
    textLines = Text.splitOn "\n" text
    -- The lines above the first table, each with its number
    aboveTables = maybe id (\table -> take (table - 1)) (tomlFirstTable file) (zip [1 ..] textLines)
    -- The numbers of the comments that show a value of the key, the last
    -- one first
    shownAsComment = reverse [number | (number, content) <- aboveTables, showsKey content]
    showsKey content = isJust $ do
      comment <- Text.stripPrefix "#" (Text.stripStart content)
      afterKey <- Text.stripPrefix key (Text.stripStart comment)
      Text.stripPrefix "=" (Text.stripStart afterKey)
    -- The text, ending with the end of a line unless it is empty
    atEnd
      | Text.null text || "\n" `Text.isSuffixOf` text = text
      | otherwise = text <> "\n"
