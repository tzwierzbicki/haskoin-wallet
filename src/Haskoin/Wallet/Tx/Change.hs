-- | Which output of a payment is its change.
--
-- A transaction does not say which of its outputs is the change. hw
-- knows it while it builds a payment, and keeps no record of it. Such a
-- record would not reach every place that shows the payment: the
-- computer that signs reads a file, and a wallet that sync or recover
-- fills reads the chain, and neither has it. One payment would then read
-- differently on two computers. So the change is read from the
-- transaction itself, by one rule, which gives the same answer for a
-- pending transaction, for a file, before a signature and in the history.
--
-- The rule ('changeAmong'): the change of a payment is what it pays an
-- address of the portfolio on a change chain, or what it sends back to an
-- address it spends from. It is one output at most. What the payment pays
-- any other address of the portfolio is a payment, as what it pays
-- another wallet is.
--
-- Every view of a transaction follows it ("Haskoin.Wallet.Tx.Addresses"),
-- and so does bumpfee ("Haskoin.Wallet.Tx.BumpFee"): what a view lists
-- as the change is what a replacement takes its higher fee from.
--
-- The rule reads what the transaction does, and not what the user meant.
-- Three payments read differently from what was typed:
--
-- * Change that --change-back sent to a receiving address the payment
--   does not spend from is a payment. The user checks one line more.
-- * A recipient that is a change address of the portfolio is the change.
-- * A recipient the payment also spends from is the change, in a payment
--   that pays no change address.
--
-- Each still shows its address, its path and its amount. The rule is
-- narrow on purpose: an output listed as a payment is one the user reads
-- before signing, and an output listed as change is one the eye skips.
module Haskoin.Wallet.Tx.Change
  ( changeOf,
    changeAmong,
  )
where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Haskoin (Address)
import Haskoin.Wallet.Database.Address (isIntPath)
import Haskoin.Wallet.Database.TxInfo (MyOutputs (..), TxSummary (..), TxType (..))
import Haskoin.Wallet.Keys (PoolPath (..))

-- | The change of a payment the portfolio makes, if it has any: its
-- address and where that address is in the portfolio.
--
-- Only a payment that leaves the portfolio has change here. A payment
-- the portfolio receives has the change of its payer, which is nothing
-- to the portfolio. A transaction between the addresses of the portfolio
-- has none either: every view lists all it pays as paid, and marks a
-- change address as one.
changeOf :: TxSummary -> Maybe (Address, PoolPath)
changeOf summary
  | summaryType summary == TxDebit =
      changeAmong summary [(addr, myOutputsPath paid) | (addr, paid) <- Map.assocs (summaryMyOutputs summary)]
  | otherwise = Nothing

-- | The change of a transaction among the outputs to the portfolio
-- given, each an address with its path: one on a change chain, or one
-- back to an address the transaction spends from.
--
-- An address that several outputs pay is never the change. The user
-- asked for those outputs (--repeat-address), to have several coins at
-- the address, and nothing tells which of them, if any, is the change.
--
-- A transaction has one change output at most. When two outputs could be
-- it, as when a payment pays an address it spends from and sends its
-- change to a new change address, the one on a change chain is the
-- change, since it is where hw sends change by itself. The other is a
-- payment the user typed. Between two outputs of the same sort, the one
-- of the lower path is the change. Nothing tells them apart, and the
-- path, unlike the place of an output in a transaction, which hw
-- shuffles, gives every view and bumpfee the same answer.
--
-- The outputs are given, and not read from the summary, for bumpfee: it
-- looks for the change among what a transaction pays that no other
-- transaction it replaces spends.
changeAmong :: TxSummary -> [(Address, PoolPath)] -> Maybe (Address, PoolPath)
changeAmong summary outputs = listToMaybe (sortOn rank (filter mayBeChange outputs))
  where
    onChangeChain = isIntPath . pathInAccount . snd
    spentFrom (addr, _) = addr `Map.member` summaryMyInputs summary
    paidSeveralTimes (addr, _) = addr `Map.member` summaryRepeatedOutputs summary
    mayBeChange output = not (paidSeveralTimes output) && (onChangeChain output || spentFrom output)
    rank output = (not (onChangeChain output), snd output)
