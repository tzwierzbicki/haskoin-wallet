{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What bumpfee says before it asks: the payments that spend the one it
-- replaces, the coins it adds, and who pays the higher fee.
module Haskoin.Wallet.Cli.PrettyPrinter.BumpFee
  ( childrenDoc,
    addedCoinsDoc,
    reducedDoc,
    sharedDoc,
  )
where

import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Haskoin
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.PrettyPrinter.Base (columns, commaList, counted, neutralAmount, rateDoc, rblock, sentence, styled, text, wordsOf)
import Haskoin.Wallet.Cli.PrettyPrinter.Payment (reportCoinDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (PathNaming (..))
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.ShortId (shortHashText)
import Haskoin.Wallet.Tx.BumpFee (PaidBy (..), Reduction (..), Sent, sentId)
import Haskoin.Wallet.Tx.CoinSelection (FeePayer (..))
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, indent, vcat, (<+>))

-- | What bumpfee says of the transactions that spend the change of the one
-- to replace, before it asks to replace them all: their short ids, as txs
-- shows them
childrenDoc :: [Sent] -> Doc Style
childrenDoc children =
  vcat $
    sentence (wordsOf "Replacing the transaction removes those of the portfolio that spend its change:")
      : [indent 2 (styled Muted (shortHashText (sentId child))) | child <- children]

-- | What bumpfee says of the coins to add, before it asks to add them: each
-- as coins shows it ('reportCoinDoc'). bumpfee replaces a transaction in
-- its own portfolio, which may be another one than the active one, and
-- the paths of its coins then name it.
addedCoinsDoc :: PathNaming -> Network -> AmountUnit -> Natural -> [JsonCoin] -> IO (Doc Style)
addedCoinsDoc paths net unit feeByte added = do
  coins <- mapM (reportCoinDoc paths net unit []) added
  return . vcat . intersperse mempty $ header : coins
  where
    them = if length added == 1 then "this coin" else "these coins"
    header = sentence $ wordsOf "The coins of the transaction cannot pay its fee at" <> [rateDoc feeByte <> ","] <> wordsOf ("and " <> them <> " of the portfolio would:")

-- | What bumpfee says of the ways the recipients can pay, those of
-- 'Haskoin.Wallet.Tx.BumpFee.reductions', before it asks which: each
-- recipient, numbered when there are several, with what it receives, then
-- what it would receive paying alone, and its share when all can pay, a
-- blank line between them
--
-- It starts with why the recipients are asked: the coins of the
-- transaction cannot pay, and no coin is added (@noCoinAdded@).
reducedDoc :: Network -> AmountUnit -> Natural -> Maybe HeldBack -> [Reduction] -> Doc Style
reducedDoc _ _ _ _ [] = mempty
reducedDoc net unit feeByte held ways@(way : _) =
  vcat . intersperse mempty $ header : rows
  where
    amount v = neutralAmount unit (fromIntegral v)
    missing = [amount way.reductionMissing, "missing"]
    recipients = [(addr, before) | (addr, before, _) <- way.reductionRecipients]
    several = length recipients > 1
    -- What each recipient gets paying it alone, by its position, when it can
    alone = Map.fromList [(i, after) | w <- ways, OneRecipient i <- [w.reductionPaidBy], (_, _, after) <- [w.reductionRecipients !! i]]
    -- What each gets paying its share, when all can
    shares = listToMaybe [[after | (_, _, after) <- w.reductionRecipients] | w <- ways, w.reductionPaidBy == AllRecipients]
    header =
      sentence $
        wordsOf "The coins of the transaction cannot pay its fee at"
          <> [rateDoc feeByte <> ","]
          <> endedWith "." (noCoinAdded unit held)
          <> who
    who
      | several =
          wordsOf "The" <> missing <> wordsOf ("can come out of what the recipients receive, one of them paying it alone" <> maybe ":" (const ", or all of them in equal shares:") shares)
      | otherwise = wordsOf "The recipient can pay the" <> missing <> wordsOf "out of what it receives:"
    -- The numbers of the recipients, padded to the widest, and what each
    -- would receive under its address
    width = length (show (length recipients))
    rows
      | several =
          [ vcat
              [ indent 2 (columns [text (rblock (fromIntegral width) (show (i + 1))), text (addrToString net addr), amount before]),
                indent (4 + width) (commaList (aloneDoc i : shareDoc i))
              ]
            | (i, (addr, before)) <- zip [0 :: Int ..] recipients
          ]
      | otherwise =
          [ vcat [indent 2 (columns [text (addrToString net addr), amount before]), indent 4 (amount after <+> "paying it")]
            | (addr, before, after) <- way.reductionRecipients
          ]
    aloneDoc i = maybe "too little to pay it alone" (\after -> amount after <+> "paying it alone") (Map.lookup i alone)
    shareDoc i = [amount (afters !! i) <+> "paying its share" | Just afters <- [shares]]

-- | The words that say that bumpfee adds no coin of the portfolio, with
-- what the portfolio holds and cannot spend now when it holds some. The
-- user is about to have the recipients pay, or --yes did it already, and
-- "no coin is added" alone hid that the portfolio had coins: under a tag
-- that locks them, locked with lockcoin, or without a block yet. coins
-- says which, and unlocking them lets the sender pay the fee.
noCoinAdded :: AmountUnit -> Maybe HeldBack -> [Doc Style]
noCoinAdded _ Nothing = wordsOf "and no coin of the portfolio is added"
noCoinAdded unit (Just held) =
  wordsOf "and no coin of the portfolio is added: it holds"
    <> [neutralAmount unit (fromIntegral held.heldBackValue)]
    <> wordsOf ("more, in " <> counted held.heldBackCoins "coin" "coins" <> " it cannot spend now, and hw coins says why")

-- | The words of a sentence, the last one followed by a sign
endedWith :: Doc Style -> [Doc Style] -> [Doc Style]
endedWith sign said = case reverse said of
  [] -> []
  lastWord : before -> reverse ((lastWord <> sign) : before)

-- | What bumpfee says in place of 'reducedDoc' when all the recipients pay
-- in equal shares without a question. The payer tells why. When the
-- sender pays, --yes took the shares, since neither the change nor the
-- coins of the portfolio could pay what is missing. When the recipients
-- pay, --recipients-pay asked them to pay what the higher fee adds
-- ('Haskoin.Wallet.Tx.BumpFee.sharedReduction'). Nothing is asked, so the
-- recipients are not numbered, and no other way is shown. Each recipient
-- comes with what it receives, then what it receives paying its share, a
-- blank line between them. A lone recipient pays it all.
sharedDoc :: FeePayer -> Network -> AmountUnit -> Natural -> Maybe HeldBack -> Reduction -> Doc Style
sharedDoc payer net unit feeByte held way =
  vcat . intersperse mempty $ header : rows
  where
    amount v = neutralAmount unit (fromIntegral v)
    paid = amount way.reductionMissing
    several = length way.reductionRecipients > 1
    who
      | several = wordsOf "the recipients pay the"
      | otherwise = wordsOf "the recipient pays the"
    shares
      | several = wordsOf "in equal shares, out of what they receive:"
      | otherwise = wordsOf "out of what it receives:"
    header = sentence $ case payer of
      SenderPays ->
        wordsOf "The coins of the transaction cannot pay its fee at"
          <> [rateDoc feeByte <> ","]
          <> endedWith "." (noCoinAdded unit held)
          <> wordsOf "With --yes,"
          <> who
          <> [paid, "missing"]
          <> shares
      RecipientsPay ->
        wordsOf "As --recipients-pay asks,"
          <> who
          <> [paid]
          <> wordsOf "the higher fee adds at"
          <> [rateDoc feeByte <> ","]
          <> shares
    paying = if several then "paying its share" else "paying it"
    rows =
      [ vcat [indent 2 (columns [text (addrToString net addr), amount before]), indent 4 (amount after <+> paying)]
        | (addr, before, after) <- way.reductionRecipients
      ]
