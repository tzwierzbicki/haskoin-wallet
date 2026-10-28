{-# LANGUAGE OverloadedStrings #-}

-- | A recipient as the user types it on the command line: an address, or an
-- address and its amount after a colon, ADDRESS:AMOUNT. preparetx needs the
-- amount of every recipient ('readPayment'). preparesendall also takes an
-- address without one, which receives what remains once the others are
-- paid ('readRecipient').
module Haskoin.Wallet.Recipient
  ( Share (..),
    readRecipient,
    readPayment,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Address, Network)
import Haskoin.Wallet.Amounts (AmountUnit, readAmount)
import Haskoin.Wallet.Util (textToAddrE)
import Numeric.Natural (Natural)

-- | What a recipient gets
data Share
  = -- | The amount the user typed, in satoshis
    Fixed !Natural
  | -- | A part of what remains once the fixed amounts and the fee are paid
    Remainder
  deriving (Eq, Show)

-- | A recipient as typed: an address, which shares what remains, or an
-- address and its amount after a colon, ADDRESS:AMOUNT. The amount is in
-- the unit given, unless it names one. The whole text is read as an address
-- first, since a Bitcoin Cash address may begin with its prefix and a
-- colon, as bitcoincash:qp…; otherwise the amount is what follows the last
-- colon.
readRecipient :: Network -> AmountUnit -> Text -> Either String (Address, Share)
readRecipient net unit typed =
  case textToAddrE net typed of
    Right addr -> Right (addr, Remainder)
    Left notAnAddress
      | Text.null beforeColon -> Left notAnAddress
      | otherwise -> do
          addr <- textToAddrE net (Text.dropEnd 1 beforeColon)
          satoshis <- readAmount unit amount
          return (addr, Fixed (fromIntegral satoshis))
  where
    -- The text up to and with the last colon, and what follows it
    (beforeColon, amount) = Text.breakOnEnd ":" typed

-- | A recipient of a payment as typed, ADDRESS:AMOUNT, which must have its
-- amount. An address alone is refused with how to write it: it is also how
-- the recipients of hw were typed before, the amount in a word of its own.
readPayment :: Network -> AmountUnit -> Text -> Either String (Address, Natural)
readPayment net unit typed = do
  recipient <- readRecipient net unit typed
  case recipient of
    (addr, Fixed amount) -> Right (addr, amount)
    (_, Remainder) ->
      Left $ "The recipient " <> Text.unpack typed <> " has no amount: write it ADDRESS:AMOUNT, as in " <> Text.unpack typed <> ":0.001"
