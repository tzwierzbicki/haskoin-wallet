{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Haskoin.Wallet.Amounts where

import Control.Arrow (second)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read as Read
import Haskoin.Wallet.Util (safeSubtract)
import Numeric.Natural (Natural)

data AmountUnit
  = UnitBitcoin
  | UnitBit
  | UnitSatoshi
  deriving (Eq, Show)

{- Amounts as text -}

showUnit :: AmountUnit -> Integer -> Text
showUnit unit amnt
  | unit == UnitSatoshi = strUnit -- satoshi is always singular
  | abs amnt == 1 = strUnit
  | otherwise = strUnit <> "s" -- plural form bitcoins and bits
  where
    strUnit =
      case unit of
        UnitBitcoin -> "bitcoin"
        UnitBit -> "bit"
        UnitSatoshi -> "satoshi"

showAmount :: AmountUnit -> Natural -> Text
showAmount unit amnt =
  case unit of
    UnitBitcoin ->
      let (q, r) = amnt `divMod` 100000000
       in addSep (showT q) <> "." <> removeEnd (padStart 8 "0" (showT r))
    UnitBit ->
      let (q, r) = amnt `divMod` 100
       in addSep (showT q) <> "." <> padStart 2 "0" (showT r)
    UnitSatoshi -> addSep (showT amnt)
  where
    removeEnd = dropPatternEnd "0000" . dropPatternEnd "000000"
    addSep = Text.intercalate "'" . chunksOfEnd 3
    showT = Text.pack . show

readAmount :: AmountUnit -> Text -> Maybe Natural
readAmount unit amntStr =
  case unit of
    UnitBitcoin -> do
      guard $ Text.length r <= 8
      a <- readNatural q
      b <- readNatural $ padEnd 8 "0" r
      return $ a * 100000000 + b
    UnitBit -> do
      guard $ Text.length r <= 2
      a <- readNatural q
      b <- readNatural $ padEnd 2 "0" r
      return $ a * 100 + b
    UnitSatoshi -> readNatural str
  where
    str = dropAmountSep amntStr
    (q, r) = second (Text.drop 1) $ Text.breakOn "." str

readNatural :: Text -> Maybe Natural
readNatural txt =
  case Read.decimal txt of
    Right (res, "") -> Just res
    _ -> Nothing

dropAmountSep :: Text -> Text
dropAmountSep = Text.filter (`notElem` [' ', '_', '\''])

-- | Like 'showAmount' but will display a minus sign for negative amounts
showIntegerAmount :: AmountUnit -> Integer -> Text
showIntegerAmount unit i
  | i < 0 = "-" <> showAmount unit (fromIntegral $ abs i)
  | otherwise = showAmount unit $ fromIntegral i

-- | Like 'readAmount' but can parse a negative amount
readIntegerAmount :: AmountUnit -> Text -> Maybe Integer
readIntegerAmount unit txt =
  case Text.uncons txt of
    Just ('-', rest) -> negate . toInteger <$> readAmount unit rest
    _ -> toInteger <$> readAmount unit txt

{- Text helpers -}

-- | Pad a text at the start with a character up to a length
padStart :: Int -> Text -> Text -> Text
padStart n c t =
  Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c <> t

-- | Pad a text at the end with a character up to a length
padEnd :: Int -> Text -> Text -> Text
padEnd n c t =
  t <> Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c

-- | Drop a suffix from a text, when it ends with it
dropPatternEnd :: Text -> Text -> Text
dropPatternEnd p t = fromMaybe t $ Text.stripSuffix p t

-- | Cut a text into pieces of a length, counting from its end, so that the
-- first piece is the short one
chunksOfEnd :: Int -> Text -> [Text]
chunksOfEnd n t = reverse $ Text.reverse <$> Text.chunksOf n (Text.reverse t)
