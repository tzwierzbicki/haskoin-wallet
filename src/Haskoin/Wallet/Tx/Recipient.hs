{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A recipient as the user types it on the command line: an address, or an
-- address and its amount after a colon, ADDRESS:AMOUNT. preparetx needs the
-- amount of every recipient ('readPayment'). preparesendall and the
-- --sweep-to addresses of preparesweep also take an address without one,
-- which receives a part of what remains once the others and the fee are
-- paid ('readRecipient', 'shareAmounts').
--
-- An address given twice is refused ('checkPaidOnce'), and so is change
-- that goes to an address the payment pays ('checkChangeApart'). Each
-- would be two outputs to one address. That is most often a recipient
-- pasted twice, which would be paid twice. The user who wants several
-- coins at one address asks for them with --repeat-address. The
-- alternatives were turned down. Joining the amounts into one output
-- pays a recipient pasted twice without a word. Building the outputs
-- without a question, as hw did, leaves the user to spot the address
-- twice in the transaction.
module Haskoin.Wallet.Tx.Recipient
  ( Share (..),
    readRecipient,
    readPayment,
    recipientAddress,
    checkAddressReads,
    checkPaidOnce,
    checkChangeApart,
    checkSharing,
    shareAmounts,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, guard, unless, when)
import Data.List (mapAccumL)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Address, Network (..), allNets, textToAddr)
import Haskoin.Wallet.Amounts (AmountUnit, readAmount)
import Haskoin.Wallet.Util (addrToTextE, notAnAddress, textToAddrE)
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
-- the unit given, unless it names one, which must be a unit of the network
-- ('readAmount'). The whole text is read as an address
-- first, since a Bitcoin Cash address may begin with its prefix and a
-- colon, as bitcoincash:qp…; otherwise the amount is what follows the last
-- colon.
readRecipient :: Network -> AmountUnit -> Text -> Either String (Address, Share)
readRecipient net unit typed =
  case (textToAddrE net typed, withAmount typed) of
    (Right addr, _) -> Right (addr, Remainder)
    (Left refusal, Nothing) -> Left refusal
    (Left _, Just (address, amount)) -> do
      addr <- textToAddrE net address
      satoshis <- readAmount net unit amount
      return (addr, Fixed (fromIntegral satoshis))

-- | The address and the amount of a recipient typed as ADDRESS:AMOUNT: the
-- texts around its last colon. The prefix of an address of Bitcoin Cash,
-- as in bitcoincash:qp…, is no address before an amount: with a typo in
-- such an address, the refusal named the prefix, \"The address
-- bitcoincash is not valid\".
withAmount :: Text -> Maybe (Text, Text)
withAmount typed = do
  (address, amount) <- splitAtLastColon typed
  guard (address `notElem` mapMaybe (.cashAddrPrefix) allNets)
  return (address, amount)

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

-- | Refuse a recipient as typed whose address reads on none of the
-- networks: a typo, which preparesweep tells before it knows the network
-- of the sweep, and so before it asks for one
checkAddressReads :: [Network] -> Text -> Either String ()
checkAddressReads networks typed =
  unless (any (\net -> isJust (recipientAddress net typed)) networks) $
    Left (notAnAddress (maybe typed fst (withAmount typed)) "any network")

-- | The address of a recipient as typed, read as 'readRecipient' reads it,
-- whether its amount reads or not: preparesweep looks for the account it
-- pays with it, before it knows the network to read the amounts on
recipientAddress :: Network -> Text -> Maybe Address
recipientAddress net typed = textToAddr net typed <|> (textToAddr net . fst =<< splitAtLastColon typed)

{- An address paid in several outputs -}

-- | Refuse recipients that name an address more than once, unless the
-- user asked for an output each (--repeat-address, the flag). The
-- addresses are compared as read, so an address of Bitcoin Cash typed
-- with its prefix and without it is one address. The refusal names the
-- first address that comes a second time.
checkPaidOnce :: Network -> Bool -> [(Address, a)] -> Either String ()
checkPaidOnce net repeats recipients =
  unless repeats . forM_ (firstRepeated (fst <$> recipients)) $ \addr -> do
    typed <- addrToTextE net addr
    Left ("The address " <> Text.unpack typed <> " is given more than once. " <> toRepeat <> " Otherwise, give it once.")

-- | Refuse change that goes to an address the payment pays, unless the
-- user asked for an output each (--repeat-address, the flag): the
-- address of --change-back, most often, when it is also a recipient. The
-- change is given when the payment has some, so a payment without change
-- is never refused here.
checkChangeApart :: Network -> Bool -> [(Address, a)] -> Maybe Address -> Either String ()
checkChangeApart net repeats recipients change =
  unless repeats . forM_ change $ \addr ->
    when (addr `elem` (fst <$> recipients)) $ do
      typed <- addrToTextE net addr
      Left ("The change goes to the address " <> Text.unpack typed <> ", which the payment also pays. " <> toRepeat)

-- | How to ask for several outputs to one address, as both refusals end
toRepeat :: String
toRepeat = "To pay it in several outputs, each a coin of its own, add --repeat-address."

-- | The first of the values that comes a second time, if any does
firstRepeated :: (Ord a) => [a] -> Maybe a
firstRepeated = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `Set.member` seen = Just x
      | otherwise = go (Set.insert x seen) xs

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
-- built when an output would be below its dust limit, or when the coins do
-- not cover the fixed amounts and the fee. The dust limit of the output
-- paying an address is given by the function: it depends on the kind of
-- the output ('Haskoin.Wallet.Tx.Size.outputDustLimit').
shareAmounts ::
  -- | The value of the coins
  Natural ->
  -- | The fee
  Natural ->
  -- | The dust limit of the output paying an address
  (Address -> Natural) ->
  [(Address, Share)] ->
  Either String [(Address, Natural)]
shareAmounts total fee dustOf recipients = do
  checkSharing recipients
  forM_ [(addr, amount) | (addr, Fixed amount) <- recipients] $ \(addr, amount) ->
    when (amount < dustOf addr) $
      Left $ "A fixed amount is below the dust limit of " <> show (dustOf addr) <> " satoshis"
  when (total < fixedTotal + fee) $
    Left $ case fixedAmounts of
      [] -> "The coins, " <> show total <> " satoshis, do not cover the fee, " <> show fee
      _ -> "The coins, " <> show total <> " satoshis, do not cover the fixed amounts, " <> show fixedTotal <> ", and the fee, " <> show fee
  forM_ [addr | (addr, Remainder) <- recipients] $ \addr ->
    when (each < dustOf addr) $
      Left $
        "What remains for each address without an amount, " <> show each
          <> " satoshis, is below the dust limit of "
          <> show (dustOf addr)
  -- What does not divide goes to the first address without an amount
  return $ snd $ mapAccumL give extra recipients
  where
    fixedAmounts = [amount | (_, Fixed amount) <- recipients]
    fixedTotal = sum fixedAmounts
    sharing = length [() | (_, Remainder) <- recipients]
    (each, extra) = (total - fixedTotal - fee) `quotRem` fromIntegral sharing
    give left (addr, Fixed amount) = (left, (addr, amount))
    give left (addr, Remainder) = (0, (addr, each + left))
