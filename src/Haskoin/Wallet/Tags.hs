{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tags: the words a user puts on addresses and transactions to sort
-- them, such as kyc or savings. A label says what one thing is for, in a
-- sentence. A tag names a group that many things belong to, so the
-- commands can work on the whole group: a listing keeps the things with a
-- tag, and a payment spends the coins at the addresses with a tag.
--
-- This module holds the rules of the word itself, and of its colour. The
-- database keeps which things have which tags
-- ("Haskoin.Wallet.Database.Tag").
--
-- = The rules of a tag
--
-- A tag is one word, so that several tags fit on a command line and in
-- one cell of a spreadsheet, a space between them. It is made of the
-- letters a to z, the digits, the dash and the underscore, and starts
-- with a letter or a digit. It has 'tagMaxLength' characters at most.
--
-- * Capital letters are not refused. They are turned into small ones
--   ('readTag'), so that Kyc and kyc are one tag, and the user never has
--   to remember how a tag was typed first.
--
-- * No space, slash, colon or other sign. A tag is typed on a command
--   line, completed by the shell, and written to files. A word of these
--   characters needs no quotes in any shell, and is never taken for a
--   path, which has slashes, or for a coin, which has a colon.
--
-- * No accented letters. The same accented letter can be written as one
--   character or as two, which look the same on the screen. Two tags that
--   look the same would then be two tags. Refusing them was chosen over
--   normalizing them: a tag is a short handle, not a text.
--
-- * It does not start with a dash, which a command line reads as an
--   option.
--
-- = Colours
--
-- A tag may have a colour, one of 'tagColors', which the screen shows it
-- in. The colours are the six dull colours of a terminal that are neither
-- black nor white, as the rest of hw uses them
-- ("Haskoin.Wallet.Cli.Style"). A colour is a help to the eye and nothing
-- more: no command behaves differently for it, and a tag without one is
-- shown in the colour of the text.
module Haskoin.Wallet.Tags
  ( -- * Tags
    Tag,
    tagText,
    readTag,
    tagMaxLength,
    tagsText,

    -- * Colours
    TagColor (..),
    tagColors,
    colorName,
    readTagColor,
    readColorChoice,
    noColorWord,

    -- * Tags as the answers show them
    ShownTag (..),
    plainTag,
    shownTagsText,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Char (isAsciiLower, isDigit)
import Data.List (intercalate)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text

{- Tags -}

-- | A tag, as hw keeps it: a word that follows the rules of this module.
-- Only 'readTag' makes one, so every tag in the program is a valid one.
newtype Tag = Tag Text
  deriving (Eq, Ord, Show)

-- | The word of a tag, as hw stores it and shows it
tagText :: Tag -> Text
tagText (Tag word) = word

-- | The longest a tag may be, in characters. It is long enough for two or
-- three words joined by dashes, and short enough to keep a row of a
-- listing on one line.
tagMaxLength :: Int
tagMaxLength = 32

-- | The tag a word typed by the user names, or why it names none. Capital
-- letters are turned into small ones. Every other departure from the
-- rules is refused, and the refusal says which rule.
readTag :: Text -> Either String Tag
readTag typed
  | Text.null word = Left "A tag cannot be empty"
  | Text.length word > tagMaxLength =
      Left (refused <> " is too long: a tag has " <> show tagMaxLength <> " characters at most")
  | Text.any (== ' ') word =
      Left (refused <> " has a space: a tag is one word. Give several tags one after the other.")
  | not (Text.all allowed word) =
      Left (refused <> " is not a tag: a tag is made of the letters a to z, digits, - and _")
  | not (startsRight word) =
      Left (refused <> " is not a tag: a tag starts with a letter or a digit")
  | otherwise = Right (Tag word)
  where
    word = Text.toLower typed
    refused = "\"" <> cs typed <> "\""
    allowed c = isAsciiLower c || isDigit c || c == '-' || c == '_'
    startsRight = maybe False (\(c, _) -> isAsciiLower c || isDigit c) . Text.uncons

instance ToJSON Tag where
  toJSON = toJSON . tagText

instance FromJSON Tag where
  parseJSON = withText "Tag" (either fail return . readTag)

-- | Several tags on one line, a space between them: how a spreadsheet
-- cell and a message write them. A tag has no space, so the line splits
-- back into its tags.
tagsText :: [Tag] -> Text
tagsText = Text.unwords . map tagText

{- Colours -}

-- | The colour of a tag on the screen
data TagColor = TagRed | TagGreen | TagYellow | TagBlue | TagMagenta | TagCyan
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every colour a tag can have, in the order the help lists them
tagColors :: [TagColor]
tagColors = [minBound .. maxBound]

-- | The name of a colour, as the user types it and as hw stores it
colorName :: TagColor -> Text
colorName = \case
  TagRed -> "red"
  TagGreen -> "green"
  TagYellow -> "yellow"
  TagBlue -> "blue"
  TagMagenta -> "magenta"
  TagCyan -> "cyan"

-- | The colour a stored name stands for, or Nothing for no colour, or for
-- a name this hw does not know. A database written by a later hw may hold
-- a colour this one lacks, and its tag is then shown without a colour
-- rather than refused.
readTagColor :: Text -> Maybe TagColor
readTagColor name = lookup name [(colorName color, color) | color <- tagColors]

-- | The word that removes the colour of a tag, where a colour is asked
noColorWord :: Text
noColorWord = "none"

-- | What the user asks of the colour of a tag, as typed after --color: a
-- colour, or none to remove the one the tag has. Capital letters are
-- taken as small ones, as for a tag. Any other word is refused, with the
-- colours to choose from.
readColorChoice :: Text -> Either String (Maybe TagColor)
readColorChoice typed
  | word == noColorWord = Right Nothing
  | Just color <- readTagColor word = Right (Just color)
  | otherwise =
      Left $
        "\""
          <> cs typed
          <> "\" is not a colour of a tag. Choose one of "
          <> intercalate ", " (cs . colorName <$> tagColors)
          <> ", or "
          <> cs noColorWord
          <> " to remove the colour."
  where
    word = Text.toLower typed

instance ToJSON TagColor where
  toJSON = toJSON . colorName

instance FromJSON TagColor where
  parseJSON = withText "TagColor" $ \name ->
    maybe (fail ("Unknown colour of a tag: " <> cs name)) return (readTagColor name)

{- Tags as the answers show them -}

-- | A tag as an answer shows it: the tag, with the colour it has now. The
-- colour comes with each tag, so that the screen needs nothing else to
-- show it, and so that a script reads it from the JSON of the answer.
data ShownTag = ShownTag
  { shownTag :: !Tag,
    shownTagColor :: !(Maybe TagColor)
  }
  deriving (Eq, Ord, Show)

-- | A tag shown without a colour
plainTag :: Tag -> ShownTag
plainTag tag = ShownTag tag Nothing

-- | The tags of an answer on one line, a space between them, without
-- their colours: what a spreadsheet cell holds
shownTagsText :: [ShownTag] -> Text
shownTagsText = tagsText . map shownTag

instance ToJSON ShownTag where
  toJSON shown = object ["name" .= shownTag shown, "color" .= shownTagColor shown]

instance FromJSON ShownTag where
  parseJSON = withObject "ShownTag" $ \o -> ShownTag <$> o .: "name" <*> o .:? "color"
