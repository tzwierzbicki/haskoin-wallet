{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Amounts as the user types and reads them, in whole coins, bits or
-- satoshis. A unit is a number of decimal places of satoshis, 8 for a
-- whole coin and 2 for a bit, and the rest follows from it: an amount is
-- read with at most as many decimals, and shown with exactly as many.
--
-- The whole coin has the name of its chain: btc on the networks of
-- Bitcoin, bch on those of Bitcoin Cash. The two are the same number of
-- satoshis and not the same money, so an amount of Bitcoin Cash never
-- shows as btc, and an amount typed in the coin of the other chain is
-- refused ('readAmount'): a legacy address is valid on both chains, and
-- the unit is then the one word that says which coin the user means to
-- pay. The bit and the satoshi keep their names on both chains.
--
-- The user chooses one unit for every network, with --unit or in the
-- configuration (the type t'UnitChoice'). Such a choice sets the size of
-- the unit, so btc and bch both stand for the whole coin there, and each
-- network shows its own name ('unitOn'). A wallet with both chains has
-- one configuration: a strict key would refuse every command of one of
-- its chains. A command that reads amounts is strict about --unit alone,
-- which the user typed for that command ('paymentUnit').
--
-- There are three sizes on purpose. The whole coin and the satoshi are
-- the units amounts are quoted in. The bit, a millionth of a coin, is the
-- unit of BIP176, for those who want two decimals.
--
-- The millibitcoin (mBTC), 100,000 satoshis, is left out. Few quote
-- amounts in it now. It also sits a factor of 1000 from both the bitcoin
-- and the bit. An amount typed in the wrong one of two such units pays a
-- thousand times too much or too little. A new unit is one more line of
-- @places@, so one can be added when users ask for it. A unit taken away
-- would break their configuration files and scripts.
module Haskoin.Wallet.Amounts
  ( AmountUnit (..),
    coinUnit,
    readUnit,
    unitName,
    showUnit,
    UnitChoice (..),
    chosenUnit,
    unitOn,
    unitsOn,
    paymentUnit,
    readAmount,
    showAmount,
    showCoins,
    maxMoney,
  )
where

import Control.Monad (when)
import Data.Char (digitToInt, isDigit, isLetter)
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin (Network (..))

-- | A unit of amounts, as the user names it and as hw shows it after an
-- amount. The first two are the whole coin, under the name of each chain.
data AmountUnit
  = -- | The bitcoin, 100,000,000 satoshis: 8 decimals. The whole coin of
    -- the networks of Bitcoin.
    UnitBitcoin
  | -- | The whole coin of the networks of Bitcoin Cash, 100,000,000
    -- satoshis: 8 decimals
    UnitBitcoinCash
  | -- | The bit, 100 satoshis: 2 decimals
    UnitBit
  | -- | The satoshi: no decimal
    UnitSatoshi
  deriving (Eq, Ord, Show)

-- | The decimal places of a unit in satoshis: a whole coin is 10^8 of
-- them, a bit 10^2
places :: AmountUnit -> Int
places UnitBitcoin = 8
places UnitBitcoinCash = 8
places UnitBit = 2
places UnitSatoshi = 0

-- | The satoshis of one unit
oneUnit :: AmountUnit -> Integer
oneUnit unit = 10 ^ places unit

-- | The satoshis of the 21 million coins there will ever be on a chain,
-- the most an amount can have (MAX_MONEY in Bitcoin Core). Bitcoin Cash
-- kept the limit of Bitcoin.
maxMoney :: Integer
maxMoney = 21000000 * oneUnit UnitBitcoin

{- Units -}

-- | The unit of the whole coin of a network: bch on a network of Bitcoin
-- Cash, a test network too, and btc on the others. The networks of
-- Bitcoin Cash are those with a prefix for CashAddr.
coinUnit :: Network -> AmountUnit
coinUnit net
  | isJust net.cashAddrPrefix = UnitBitcoinCash
  | otherwise = UnitBitcoin

-- | Whether a unit is a whole coin, of either chain
isCoin :: AmountUnit -> Bool
isCoin unit = unit `elem` [UnitBitcoin, UnitBitcoinCash]

-- | Whether a unit is the whole coin of other networks than this one: btc
-- on a network of Bitcoin Cash, or bch on a network of Bitcoin
coinOfAnotherChain :: Network -> AmountUnit -> Bool
coinOfAnotherChain net unit = isCoin unit && unit /= coinUnit net

-- | The chain a whole coin is the unit of, as a refusal names it
chainOf :: AmountUnit -> String
chainOf UnitBitcoinCash = "Bitcoin Cash"
chainOf _ = "Bitcoin"

-- | The names of the units of a network, as a refusal lists those to give
-- in place of another: "btc, bit or sat"
unitsToGive :: Network -> String
unitsToGive net = Text.unpack (unitName (coinUnit net)) <> ", bit or sat"

-- | A unit as typed after an amount, with --unit or in the configuration
-- file, in any case
readUnit :: Text -> Maybe AmountUnit
readUnit name =
  lookup
    (Text.toLower name)
    [ ("btc", UnitBitcoin),
      ("bch", UnitBitcoinCash),
      ("bit", UnitBit),
      ("bits", UnitBit),
      ("sat", UnitSatoshi),
      ("sats", UnitSatoshi)
    ]

-- | The name of a unit on its own, as the help and the configuration file
-- write it: btc, bch, bit or sat. 'readUnit' reads it back.
unitName :: AmountUnit -> Text
unitName UnitBitcoin = "btc"
unitName UnitBitcoinCash = "bch"
unitName UnitBit = "bit"
unitName UnitSatoshi = "sat"

-- | The name of a unit after an amount of satoshis shown in it, as the
-- user types it: btc or bch, and bit or sat for exactly one unit, bits or
-- sats otherwise
showUnit :: AmountUnit -> Integer -> Text
showUnit unit amount
  | isCoin unit || abs amount == oneUnit unit = unitName unit
  | otherwise = unitName unit <> "s"

{- The unit of a command -}

-- | The unit the user chose for the amounts of a command, before the
-- network of an amount is known. It tells where the choice comes from,
-- since only a unit typed for this command is held against the network
-- of a payment ('paymentUnit').
data UnitChoice
  = -- | The unit --unit names, for this command
    AskedUnit AmountUnit
  | -- | The unit of the configuration, for every command and every network
    DefaultUnit AmountUnit
  deriving (Eq, Show)

-- | The unit of a choice, as the user named it
chosenUnit :: UnitChoice -> AmountUnit
chosenUnit (AskedUnit unit) = unit
chosenUnit (DefaultUnit unit) = unit

-- | The unit the amounts of a network are shown in, for a choice: the
-- whole coin under the name of the network, whichever of btc and bch the
-- choice names, or the bit or the satoshi. A listing may hold several
-- networks, and each of its amounts takes the unit of its own network.
unitOn :: Network -> UnitChoice -> AmountUnit
unitOn net choice
  | isCoin (chosenUnit choice) = coinUnit net
  | otherwise = chosenUnit choice

-- | The units the amounts of several networks are shown in, each once,
-- btc before bch: what the heading of a table over those networks names.
-- With no network, the unit as the user named it.
unitsOn :: [Network] -> UnitChoice -> [AmountUnit]
unitsOn [] choice = [chosenUnit choice]
unitsOn nets choice = sort (nub [unitOn net choice | net <- nets])

-- | The unit of a command that reads amounts on a network: the unit the
-- amounts typed without one are read in, and the one the command shows
-- its amounts in. A --unit that names the coin of the other chain is
-- refused, as --unit bch for a payment on btc: the user who types it
-- counts in a coin the payment does not move. The unit of the
-- configuration is not held against the network: it is one for every
-- network ('unitOn').
paymentUnit :: Network -> UnitChoice -> Either String AmountUnit
paymentUnit net choice = case choice of
  AskedUnit unit
    | coinOfAnotherChain net unit ->
        Left $
          "The unit " <> name unit <> ", given with --unit, is the unit of " <> chainOf unit
            <> (", and the network is " <> net.name <> ": give --unit " <> unitsToGive net)
  _ -> Right (unitOn net choice)
  where
    name = Text.unpack . unitName

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

-- | An amount of satoshis in whole coins, with its eight decimals and no
-- unit, as in 0.00050000: the number a report or a journal writes beside
-- the name it gives the coin of the network
showCoins :: Integer -> Text
showCoins = showAmount UnitBitcoin

-- | The satoshis of an amount as typed on a network: digits, with a point
-- before at most as many decimals as the unit has, then the unit, or the
-- given one when none is typed. The whole coin of the other chain is no
-- unit of the network: 0.5bch is refused on btc, and 0.5btc on bch. The
-- amount is at most the 21 million coins there will ever be: more would
-- wrap around in the 64 bits of an output. No separator is read, since
-- one typed for a point makes another amount.
readAmount :: Network -> AmountUnit -> Text -> Either String Word64
readAmount net defaultUnit typed = do
  unit <- case Text.strip suffix of
    "" -> Right defaultUnit
    name
      | Text.all isLetter name -> maybe (Left unknownUnit) unitOfNetwork (readUnit name)
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
    coin = Text.unpack (unitName (coinUnit net))
    unitOfNetwork unit
      | coinOfAnotherChain net unit = Left (anotherChain unit)
      | otherwise = Right unit
    amount = "The amount " <> Text.unpack typed
    notNumber = amount <> " is not a number: write digits, with a point before the decimals"
    unknownUnit = amount <> " has a unit hw does not know: " <> coin <> ", bit, bits, sat or sats"
    anotherChain unit =
      amount <> " is in " <> Text.unpack (unitName unit) <> ", the unit of " <> chainOf unit
        <> (", and the network is " <> net.name <> ": give it in " <> unitsToGive net)
    tooManyDecimals unit
      | places unit == 0 = amount <> " has decimals, but an amount in " <> units unit <> " is whole"
      | otherwise = amount <> " has more than " <> show (places unit) <> " decimals, the most an amount in " <> units unit <> " has"
    units unit = Text.unpack (showUnit unit 0)
    tooMuch = amount <> " is more than the 21 million " <> coins <> " there will ever be"
    coins = if coinUnit net == UnitBitcoin then "bitcoins" else coin
