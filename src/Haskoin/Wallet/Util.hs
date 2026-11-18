{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Small helpers with no meaning of their own in the wallet, shared by
-- modules that should not depend on one another.
module Haskoin.Wallet.Util where

import Control.Monad (forM, (<=<))
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as Pretty
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (GeneralCategory (..), generalCategory, isAlphaNum, isAscii)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Haskoin
import Numeric (showHex)
import Numeric.Natural (Natural)
import System.Random (StdGen, uniformShuffleList)

{- JSON -}

-- | A JSON document as hw writes it, to a file or to the screen: indented
-- by two spaces and ending with a newline
encodeJsonPretty :: (JSON.ToJSON a) => a -> BS.ByteString
encodeJsonPretty =
  BL.toStrict
    . Pretty.encodePretty'
      Pretty.defConfig
        { Pretty.confIndent = Pretty.Spaces 2,
          Pretty.confTrailingNewline = True
        }

{- Rows -}

-- | A count of rows as a query of the database takes it, in 64 bits. A
-- larger count is as good as the largest one, past every row, rather than
-- wrapping around: 2^64 + 1 read as 1 gave a single row.
rowLimit :: (Num a) => Natural -> a
rowLimit n = fromIntegral (min n (fromIntegral (maxBound :: Int64)))

{- Text from outside -}

-- | A text from outside the wallet, such as a label of a file, made safe
-- to show on a terminal: every character that is an order rather than
-- text is written as an escape. A terminal obeys control characters, so a
-- label holding them could move the cursor, erase what hw printed and
-- write a recipient of its choice in its place; a format character such
-- as U+202E turns the text after it around. Those are the control
-- characters, the format characters but the two joiners, and the line and
-- paragraph separators. A line break shows as \n, a carriage return as \r
-- and a tab as \t, any other by its code point in hexadecimal: \x1b,
-- \u202e, \U000e0001. The zero-width joiner and non-joiner stay, since
-- emoji and some scripts need them, and they move nothing.
visibleText :: Text -> Text
visibleText text
  | Text.any isHidden text = Text.concatMap visibleChar text
  | otherwise = text
  where
    visibleChar c
      | isHidden c = Text.pack (escapeOf c)
      | otherwise = Text.singleton c

-- | Whether a character is an order to the terminal or to the layout,
-- which 'visibleText' writes as an escape
isHidden :: Char -> Bool
isHidden c = case generalCategory c of
  Control -> True
  Format -> c `notElem` ['\x200C', '\x200D']
  LineSeparator -> True
  ParagraphSeparator -> True
  _ -> False

-- | The escape of a character, as 'visibleText' writes it
escapeOf :: Char -> String
escapeOf c = case c of
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _
    | code <= 0xff -> "\\x" <> hexDigits 2
    | code <= 0xffff -> "\\u" <> hexDigits 4
    | otherwise -> "\\U" <> hexDigits 8
  where
    code = fromEnum c
    hexDigits width = let digits = showHex code "" in replicate (width - length digits) '0' <> digits

{- Errors -}

-- | The result of an IO action that may fail, as an error of the monad
liftEitherIO :: (MonadIO m) => IO (Either String a) -> ExceptT String m a
liftEitherIO = liftEither <=< liftIO

{- Addresses -}

-- Addresses as text, with an error instead of Nothing when the address
-- cannot be written on the network.

addrToTextE :: Network -> Address -> Either String Text
addrToTextE net a =
  maybeToEither "Invalid Address in addrToTextE" (addrToText net a)

-- | An address as the user types it, which must be one of the network
textToAddrE :: Network -> Text -> Either String Address
textToAddrE net a = maybeToEither (notAnAddress a net.name) (textToAddr net a)

-- | The refusal of a text that is no address on a network, or on a set of
-- them, as the error names it. The error names the text, so that the user
-- sees the typo, unless the text has the form of a private key
-- ('hasSecKeyForm'): an error goes to the screen, to the log of a script
-- and to --json, where a key must never be.
notAnAddress :: Text -> String -> String
notAnAddress typed networks
  | hasSecKeyForm typed = secKeyNotAddress
  | otherwise = "The address " <> Text.unpack typed <> " is not valid on " <> networks

-- | What hw answers to a private key given where an address goes
secKeyNotAddress :: String
secKeyNotAddress = "This is a private key, not an address"

-- | Whether a word has the form of a private key in WIF or of a minikey,
-- valid or not. A user with a paper wallet has a key in hand, and may give
-- it where its address goes.
--
-- The form is the length and the first character: 51 characters after 5
-- or 9, 52 after K, L or c, on every network, and 22 or 30 after S for a
-- minikey. The checksum is left aside on purpose: a key with one character
-- wrong is the key all the same, found again in moments by trying the
-- characters of that place. No address has this form, since none starts
-- with one of these characters.
--
-- A key in 64 hexadecimal digits is not seen: a transaction id has the
-- same form, and is what a user most often holds in that form.
hasSecKeyForm :: Text -> Bool
hasSecKeyForm word = Text.all isAsciiAlphaNum word && (wif || minikey)
  where
    wif = startsWith "59" 51 || startsWith "KLc" 52
    minikey = startsWith "S" 22 || startsWith "S" 30
    startsWith firsts len = Text.length word == len && Text.any (== Text.head word) firsts

-- | Whether the character is a letter or a digit of ASCII, which the text
-- of a private key is made of
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

-- | The text of an error, with every private key in it replaced by a
-- mention of one ('hasSecKeyForm'). A command given a key in the place of
-- something else quotes what was typed in its refusal: an address the
-- wallet does not have, a hash that is not valid, an argument it cannot
-- parse. No command knows that it holds a key, so the key is taken out
-- where the error is printed, whatever wrote it: the error line of
-- "Haskoin.Wallet.Cli.Style" for the screen, and the JSON of an error
-- answer in "Haskoin.Wallet.Cli.Response".
--
-- The words looked at are the runs of letters and digits, so that a key is
-- found with punctuation against it, as in KEY:0 or in quotes.
hidingSecKeys :: Text -> Text
hidingSecKeys = Text.concat . map hidden . Text.groupBy (\one other -> isAsciiAlphaNum one == isAsciiAlphaNum other)
  where
    hidden run
      | hasSecKeyForm run = "(a private key, not shown)"
      | otherwise = run

-- | The addresses of a text, as an --address-file holds them: every
-- word that is an address of the network, on any line, as 'wordsOfLines'
-- reads them. A line with no address is named by its number and its text,
-- or by its number alone when it holds a private key ('hasSecKeyForm'),
-- which the error would bring from the file to the screen.
parseAddresses :: Network -> Text -> Either String [Address]
parseAddresses net = wordsOfLines noAddress (textToAddr net)
  where
    noAddress number line
      | any hasSecKeyForm (Text.words line) =
          "Line " <> show number <> " holds a private key, not an address. Give the keys with --key-file, or the address of the key in the --address-file"
      | otherwise = "No address on line " <> show number <> ": " <> Text.unpack (visibleText line)

-- | An address as it is written in a message or on the screen, or a
-- mention of the mismatch when it cannot be written on the network
addrToString :: Network -> Address -> String
addrToString net a = maybe "an address of another network" Text.unpack (addrToText net a)

-- | Whether the network can write the address: a SegWit address has no
-- text on Bitcoin Cash, and a CashAddr one none on Bitcoin
onNetwork :: Network -> Address -> Bool
onNetwork net = isJust . addrToText net

{- Files of words -}

-- | The items of a text, as a file of private keys or of addresses holds
-- them: every word that reads as one, on any line. A line starting with #
-- is a comment. A line with words but no item is an error, so that a
-- mistyped item alone on its line is not passed over. Another word on the
-- line of an item is a label, left alone.
--
-- The caller writes the error, from the number of the line and its text,
-- since it knows whether the text may be a secret. A text it shows comes
-- from a file, and must be made safe for a terminal ('visibleText').
wordsOfLines :: (Int -> Text -> String) -> (Text -> Maybe a) -> Text -> Either String [a]
wordsOfLines noItem readWord text = concat <$> mapM itemsOf (zip [1 ..] (Text.lines text))
  where
    itemsOf (number, line)
      | null (Text.words line) || "#" `Text.isPrefixOf` Text.stripStart line = Right []
      | null items = Left (noItem number line)
      | otherwise = Right items
      where
        items = mapMaybe readWord (Text.words line)

{- Numbers -}

-- | The difference of two numbers, or Nothing when it would be negative
safeSubtract :: (Integral a) => a -> a -> Maybe a
safeSubtract a b
  | b > a = Nothing
  | otherwise = Just $ a - b

-- | A natural number in decimal digits, and nothing else
readNatural :: Text -> Maybe Natural
readNatural txt =
  case Read.decimal txt of
    Right (n, rest) | Text.null rest -> Just n
    _ -> Nothing

{- Random -}

-- | Shuffle a list, drawing from the 'StdGen' kept in a state monad: the
-- Fisher-Yates shuffle of the random library, for the transaction builders
randomShuffle :: (MonadState StdGen m) => [a] -> m [a]
randomShuffle xs = state (uniformShuffleList xs)

{- Address maps in JSON -}

-- Maps keyed by address are written with the address text of the network as
-- the key.

-- | The map with its addresses written as text. An address the network
-- cannot write is left out: the summary builders keep the outputs at such
-- addresses apart, as raw outputs, so that none reaches here.
mapAddrText :: Network -> Map Address v -> Map Text v
mapAddrText net m = Map.fromList [(t, v) | (a, v) <- Map.assocs m, Just t <- [addrToText net a]]

-- | The map with its keys read as addresses of the network. The parser
-- fails on a text that is not one.
mapTextAddr :: Network -> Map Text v -> Parser (Map Address v)
mapTextAddr net m = Map.fromList <$> mapM key (Map.assocs m)
  where
    key (t, v) = case textToAddr net t of
      Just a -> return (a, v)
      Nothing -> fail $ "Invalid address " <> Text.unpack t

-- | The labels of transactions by their ids, as hw writes them in JSON: an
-- object of txid to label
txidLabelsJson :: Map TxHash Text -> JSON.Value
txidLabelsJson = JSON.toJSON . Map.mapKeys txHashToHex

-- | The labels of transactions by their ids, read back from
-- 'txidLabelsJson'
parseTxidLabels :: JSON.Value -> Parser (Map TxHash Text)
parseTxidLabels value = do
  byHex <- JSON.parseJSON value
  fmap Map.fromList . forM (Map.assocs byHex) $ \(hex, label) ->
    case hexToTxHash hex of
      Just tid -> return (tid, label :: Text)
      Nothing -> fail $ "Invalid txid " <> Text.unpack hex
