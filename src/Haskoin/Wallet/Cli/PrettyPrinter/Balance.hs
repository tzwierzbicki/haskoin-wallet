{-# LANGUAGE OverloadedStrings #-}

-- | The amounts of a balance, as every view writes them: what coins are
-- worth, what can be spent now and what waits, on a line and in a column
-- of a table. The portfolios, the syncs, the addresses and the coins all
-- show a balance, and show it alike.
module Haskoin.Wallet.Cli.PrettyPrinter.Balance
  ( balanceFigures,
    totalFigure,
    unspendableFigure,
    shortBalanceFigures,
    notSyncedWords,
    waitingAmount,
    waitingAmountCell,
    heldAmountCell,
  )
where

import Data.String.Conversions (cs)
import Data.Word
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.PrettyPrinter.Base (amountIn, amountPrinter, neutralAmount, styled, text)
import Haskoin.Wallet.Cli.Style
import Prettyprinter (Doc, annotate, (<+>))

-- | What coins are worth, in the three amounts of every short view:
-- all of them, those that can be spent now, and the rest, which waits or
-- is locked. The first is the sum of the two others, so no reader can
-- add them wrongly or take a part for the whole. balances --verbose says
-- why the rest cannot be spent.
--
-- The amounts follow what can be spent, and not what is confirmed: an
-- unconfirmed amount would be a part of the total that overlaps both
-- others, since the wallet can spend the unconfirmed change of its own
-- payments.
balanceFigures :: AmountUnit -> Word64 -> Word64 -> [Doc Style]
balanceFigures unit total spendable =
  [ totalFigure unit total,
    "spendable" <+> amountPrinter unit spendable,
    unspendableFigure unit (total - spendable)
  ]

-- | The first of the 'balanceFigures': what coins are worth
totalFigure :: AmountUnit -> Word64 -> Doc Style
totalFigure unit total = "total" <+> amountPrinter unit total

-- | The last of the 'balanceFigures': what cannot be spent now, in the
-- colour of what waits
unspendableFigure :: AmountUnit -> Word64 -> Doc Style
unspendableFigure unit rest = "unspendable" <+> waitingAmount unit rest

-- | 'balanceFigures' on a line that has more to say after them, as that
-- of a portfolio and that of a sync: when every coin can be spent, the
-- rest is left out. The line always starts with the total and what can
-- be spent of it.
shortBalanceFigures :: AmountUnit -> Word64 -> Word64 -> [Doc Style]
shortBalanceFigures unit total spendable
  | total == spendable = take 2 figures
  | otherwise = figures
  where
    figures = balanceFigures unit total spendable

-- | What a view writes in place of the amounts and the counts of an
-- address of a pool that never synced (the type
-- t'Haskoin.Wallet.Database.Address.AddressFunds'), and in place of
-- those of a portfolio on such a network, as faint as \"unused\": the
-- words sync uses for a network it did not sync
notSyncedWords :: Doc Style
notSyncedWords = styled Muted "not synced"

-- | An amount that waits to be spent: in the colour of what waits, unless
-- it is zero. Unconfirmed amounts take it too: what another wallet pays
-- cannot be spent before it confirms, and the wallet does not tell its
-- own unconfirmed coins apart in a balance.
waitingAmount :: AmountUnit -> Word64 -> Doc Style
waitingAmount unit 0 = neutralAmount unit 0
waitingAmount unit v = amountIn (Just Waiting) unit (fromIntegral v)

-- | An amount that waits to be spent, as a number in a column of a table:
-- yellow, unless it is zero, as 'waitingAmount'
waitingAmountCell :: AmountUnit -> Word64 -> Doc Style
waitingAmountCell unit amnt = (if amnt > 0 then annotate Waiting else id) (text (cs (showAmount unit (fromIntegral amnt))))

-- | An amount the wallet holds, as a number in a column of a table: green,
-- unless it is zero
heldAmountCell :: AmountUnit -> Word64 -> Doc Style
heldAmountCell unit amnt = (if amnt > 0 then annotate Incoming else id) (text (cs (showAmount unit (fromIntegral amnt))))
