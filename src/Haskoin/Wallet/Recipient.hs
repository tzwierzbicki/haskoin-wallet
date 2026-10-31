{-# LANGUAGE OverloadedStrings #-}

-- | A recipient as the user types it on the command line: an address, or an
-- address and its amount after a colon, ADDRESS:AMOUNT. preparetx needs the
-- amount of every recipient ('readPayment'). preparesendall and the
-- --sweep-to addresses of preparesweep also take an address without one,
-- which receives a part of what remains once the others and the fee are
-- paid ('readRecipient', 'shareAmounts').
module Haskoin.Wallet.Recipient
  ( Share (..),
    readRecipient,
    readPayment,
    recipientAddress,
    checkSharing,
    shareAmounts,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Address, Network, textToAddr)
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
  case (textToAddrE net typed, splitAtLastColon typed) of
    (Right addr, _) -> Right (addr, Remainder)
    (Left notAnAddress, Nothing) -> Left notAnAddress
    (Left _, Just (address, amount)) -> do
      addr <- textToAddrE net address
      satoshis <- readAmount unit amount
      return (addr, Fixed (fromIntegral satoshis))

-- | The text before the last colon, and the text after it, when there is
-- a colon
splitAtLastColon :: Text -> Maybe (Text, Text)
splitAtLastColon typed
  | Text.null withColon = Nothing
  | otherwise = Just (Text.dropEnd 1 withColon, after)
  where
    -- The text up to and with the last colon, and what follows it
    (withColon, after) = Text.breakOnEnd ":" typed

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

-- | The address of a recipient as typed, read as 'readRecipient' reads it,
-- whether its amount reads or not: preparesweep looks for the account it
-- pays with it, before it knows the network to read the amounts on
recipientAddress :: Network -> Text -> Maybe Address
recipientAddress net typed = textToAddr net typed <|> (textToAddr net . fst =<< splitAtLastColon typed)

{- What each recipient gets -}

-- | Refuse recipients that leave no one to receive what remains
checkSharing :: [(Address, Share)] -> Either String ()
checkSharing recipients =
  unless (Remainder `elem` (snd <$> recipients)) $
    Left "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"

-- | The amount of each recipient, in their order, when coins worth the
-- total pay them and the fee: the fixed amounts as typed, and what remains
-- shared equally among the others, the first of them also getting what does
-- not divide. The recipients without an amount pay the fee. Nothing is
-- built when an output would be below the dust limit, or when the coins do
-- not cover the fixed amounts and the fee.
shareAmounts ::
  -- | The value of the coins
  Natural ->
  -- | The fee
  Natural ->
  -- | The dust limit
  Natural ->
  [(Address, Share)] ->
  Either String [(Address, Natural)]
shareAmounts total fee dust recipients = do
  checkSharing recipients
  when (any (< dust) fixedAmounts) $
    Left $ "A fixed amount is below the dust limit of " <> show dust <> " satoshis"
  when (total < fixedTotal + fee) $
    Left $ case fixedAmounts of
      [] -> "The coins, " <> show total <> " satoshis, do not cover the fee, " <> show fee
      _ -> "The coins, " <> show total <> " satoshis, do not cover the fixed amounts, " <> show fixedTotal <> ", and the fee, " <> show fee
  when (each < dust) $
    Left $
      "What remains for each address without an amount, " <> show each
        <> " satoshis, is below the dust limit of "
        <> show dust
  -- What does not divide goes to the first address without an amount
  return $ snd $ mapAccumL give extra recipients
  where
    fixedAmounts = [amount | (_, Fixed amount) <- recipients]
    fixedTotal = sum fixedAmounts
    sharing = length [() | (_, Remainder) <- recipients]
    (each, extra) = (total - fixedTotal - fee) `quotRem` fromIntegral sharing
    give left (addr, Fixed amount) = (left, (addr, amount))
    give left (addr, Remainder) = (0, (addr, each + left))
