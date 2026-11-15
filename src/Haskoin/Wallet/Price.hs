{-# LANGUAGE OverloadedStrings #-}

-- | The price of a coin in a currency, as the user gives it, and the
-- value of a balance at that price.
--
-- hw knows no price. A price would come from a server, which is one more
-- service to trust, and a tax office names the price it wants: most
-- publish the rate of the last day of the year. So a report that states
-- a value takes the price from the command line, and says so
-- ("Haskoin.Wallet.Cli.BalanceStatement").
--
-- A price is kept as the digits the user typed, not as a floating
-- number. A value is then worked out in whole numbers, and is exact to
-- the last cent: a statement for a tax office must not be off by a
-- rounding of the machine.
module Haskoin.Wallet.Price
  ( Price (..),
    readPrice,
    readNetworkPrice,
    readCurrency,
    priceText,
    valueAt,
    valueText,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)

-- | A price of one coin: a whole number, and how many of its last digits
-- are decimals. 87500.5 is the number 875005 with 1 decimal.
data Price = Price
  { priceNumber :: !Integer,
    priceDecimals :: !Int
  }
  deriving (Eq, Show)

-- | A price as typed: digits, with a point before the decimals if it has
-- some, as in 87500 and 87500.50. It has no sign, no unit and no
-- separator of thousands: 87,500 could be read as 87.5 in half of the
-- world. Eight decimals are the most a price takes, and twelve digits
-- before the point.
readPrice :: Text -> Either String Price
readPrice typed =
  case Text.splitOn "." typed of
    [whole] | wellFormed whole "" -> Right (Price (number whole) 0)
    [whole, decimals] | wellFormed whole decimals, not (Text.null decimals) -> Right (Price (number (whole <> decimals)) (Text.length decimals))
    _ -> Left ("The price " <> Text.unpack typed <> " does not read: give a number with a point before its decimals, such as 87500 or 87500.50")
  where
    wellFormed whole decimals =
      not (Text.null whole)
        && Text.all isDigit (whole <> decimals)
        && Text.length whole <= 12
        && Text.length decimals <= 8
    number = Text.foldl' (\total digit -> total * 10 + toInteger (fromEnum digit - fromEnum '0')) 0

-- | The price of a network as --price takes it: the name of the network,
-- an equals sign, and the price, as in btc=87500. The name is read by
-- the function given.
readNetworkPrice :: (String -> Either String network) -> Text -> Either String (network, Price)
readNetworkPrice readNetwork typed =
  case Text.splitOn "=" typed of
    [name, price] -> (,) <$> readNetwork (Text.unpack name) <*> readPrice price
    _ -> Left ("The price " <> Text.unpack typed <> " does not read: give a network and its price, such as btc=87500")

-- | The code of a currency: three letters, as in CHF, EUR and USD,
-- written in capitals whatever the case typed
readCurrency :: Text -> Either String Text
readCurrency typed
  | Text.length typed == 3 && Text.all (\char -> isAsciiLower char || isAsciiUpper char) typed = Right (Text.toUpper typed)
  | otherwise = Left ("The currency " <> Text.unpack typed <> " does not read: give its code of three letters, such as CHF, EUR or USD")

-- | A price as a report writes it: with a comma between its thousands,
-- and its decimals as typed, two at least. 87500.5 reads 87,500.50.
priceText :: Price -> Text
priceText (Price number decimals) = grouped whole <> "." <> Text.justifyLeft 2 '0' fraction
  where
    digits = Text.justifyRight (decimals + 1) '0' (Text.pack (show number))
    (whole, fraction) = Text.splitAt (Text.length digits - decimals) digits

-- | The value of a balance of satoshis at a price, in hundredths of the
-- currency, rounded to the nearest one, a half upwards. A coin is
-- 100000000 satoshis.
valueAt :: Price -> Natural -> Integer
valueAt (Price number decimals) satoshis = (2 * numerator + denominator) `div` (2 * denominator)
  where
    -- In hundredths: the satoshis times the price, times 100
    numerator = toInteger satoshis * number * 100
    denominator = 10 ^ (decimals + 8)

-- | A value in hundredths of a currency as a report writes it, with a
-- comma between its thousands and two decimals: 123456789 reads
-- 1,234,567.89
valueText :: Integer -> Text
valueText hundredths = grouped (Text.pack (show whole)) <> "." <> Text.justifyRight 2 '0' (Text.pack (show cents))
  where
    (whole, cents) = hundredths `divMod` 100

-- | The digits of a whole number with a comma between each group of
-- three, from the right
grouped :: Text -> Text
grouped = Text.intercalate "," . reverse . map Text.reverse . Text.chunksOf 3 . Text.reverse
