{-# LANGUAGE OverloadedStrings #-}

-- | Amounts as the user types and reads them, in bitcoins, bits or
-- satoshis. A unit is a number of decimal places of satoshis, 8 for a
-- bitcoin and 2 for a bit, and the rest follows from it: an amount is read
-- with at most as many decimals, and shown with exactly as many.
module Haskoin.Wallet.Amounts
  ( AmountUnit (..),
    readUnit,
    showUnit,
    readAmount,
    showAmount,
    maxMoney,
  )
where

import Control.Monad (when)
import Data.Char (digitToInt, isDigit, isLetter)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

data AmountUnit
  = UnitBitcoin
  | UnitBit
  | UnitSatoshi
  deriving (Eq, Show)

-- | The decimal places of a unit in satoshis: a bitcoin is 10^8 of them, a
-- bit 10^2
places :: AmountUnit -> Int
places UnitBitcoin = 8
places UnitBit = 2
places UnitSatoshi = 0

-- | The satoshis of one unit
oneUnit :: AmountUnit -> Integer
oneUnit unit = 10 ^ places unit

-- | The satoshis of the 21 million bitcoins there will ever be, the most an
-- amount can have (MAX_MONEY in Bitcoin Core)
maxMoney :: Integer
maxMoney = 21000000 * oneUnit UnitBitcoin

{- Units -}

-- | A unit as typed after an amount or with --unit, in any case
readUnit :: Text -> Maybe AmountUnit
readUnit name =
  lookup (Text.toLower name) [("btc", UnitBitcoin), ("bit", UnitBit), ("bits", UnitBit), ("sat", UnitSatoshi), ("sats", UnitSatoshi)]

-- | The name of a unit after an amount of satoshis shown in it, as the
-- user types it: btc, and bit or sat for exactly one unit, bits or sats
-- otherwise
showUnit :: AmountUnit -> Integer -> Text
showUnit unit amount = case unit of
  UnitBitcoin -> "btc"
  UnitBit -> plural "bit"
  UnitSatoshi -> plural "sat"
  where
    plural name = if abs amount == oneUnit unit then name else name <> "s"

{- Amounts -}

-- | An amount of satoshis shown in a unit, with all the decimals of the
-- unit, and a minus sign when it is negative
showAmount :: AmountUnit -> Integer -> Text
showAmount unit amount = sign <> Text.pack (show whole) <> decimals
  where
    sign = if amount < 0 then "-" else ""
    (whole, part) = abs amount `quotRem` oneUnit unit
    decimals
      | places unit == 0 = ""
      | otherwise = "." <> Text.justifyRight (places unit) '0' (Text.pack (show part))

-- | The satoshis of an amount as typed: digits, with a point before at
-- most as many decimals as the unit has, then the unit, or the given one
-- when none is typed. The amount is at most the 21 million bitcoins there
-- will ever be: more would wrap around in the 64 bits of an output. No
-- separator is read, since one typed for a point makes another amount.
readAmount :: AmountUnit -> Text -> Either String Word64
readAmount defaultUnit typed = do
  unit <- case Text.strip suffix of
    "" -> Right defaultUnit
    name
      | Text.all isLetter name -> maybe (Left unknownUnit) Right (readUnit name)
      | otherwise -> Left notNumber
  (whole, decimals) <- case Text.splitOn "." number of
    [w] -> Right (w, "")
    [w, d] -> Right (w, d)
    _ -> Left notNumber
  when (Text.null whole && Text.null decimals) $ Left notNumber
  when (Text.length decimals > places unit) $ Left (tooManyDecimals unit)
  let satoshis = digits whole * oneUnit unit + digits (Text.justifyLeft (places unit) '0' decimals)
  when (satoshis > maxMoney) $ Left tooMuch
  return (fromInteger satoshis)
  where
    (number, suffix) = Text.span (\c -> isDigit c || c == '.') (Text.strip typed)
    digits = Text.foldl' (\n c -> 10 * n + toInteger (digitToInt c)) 0
    amount = "The amount " <> Text.unpack typed
    notNumber = amount <> " is not a number: write digits, with a point before the decimals"
    unknownUnit = amount <> " has a unit hw does not know: btc, bit, bits, sat or sats"
    tooManyDecimals unit
      | places unit == 0 = amount <> " has decimals, but an amount in " <> units unit <> " is whole"
      | otherwise = amount <> " has more than " <> show (places unit) <> " decimals, the most an amount in " <> units unit <> " has"
    units unit = Text.unpack (showUnit unit 0)
    tooMuch = amount <> " is more than the 21 million bitcoins there will ever be"
