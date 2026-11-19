# The view of a transaction

What `hw` prints for one transaction: the first line, the fields, the `From`
and `To` lines, in show, txs --verbose, pendingtxs and before signing or
sending. The memory `output-views` has the decisions of the maintainer.

## Where the view comes from

- One `TxSummary` (`Database/TxInfo.hs`) says what a transaction does to one
  portfolio. Every view is drawn from it, so a view is from the side of the
  portfolio. `--json` prints the whole summary, the plain view a part of it.
- The type: `txType amount fee`, but `summarize` in `Tx/Signing.hs` gives
  `TxExternal` to a transaction with no input and no output of the
  portfolio. `typeWord` in `Cli/PrettyPrinter.hs` has the word of each type.
- In `Cli/PrettyPrinter.hs`: `verboseTxDoc` is the whole view, `headlineDoc`
  its first line (`headlineTime` for the time), `spentFrom` and `paidTo` the
  address lists, `spendsField` the `Spends` line. `txAnswerDoc` is the view
  as an answer.
- The listings have their own first line (`compactHeadline`, `timeCell`):
  a change of `headlineDoc` does not reach them.

## A transaction that is not sent yet

- `txInPreparation` (`Database/TxInfo.hs`) is true for the status `Pending`:
  held as pending, or read from a file. `spentFrom` and `paidTo` then list
  every address with its amount. For `OnChain`, a payment received lists the
  addresses of the portfolio alone, and an input of another wallet has no
  amount.
- "from a file" in place of the time: a `Pending` status whose
  `pendingCreated` is `Nothing`. preparesweep sets the time by hand for a
  sweep the wallet does not hold.
- The contacts of the addresses shown: `shownCounterparts` and `fillContacts`
  in `Database/TxInfo.hs`.

## What else a change here reaches

- `Cli/Sheet.hs` uses `spentFrom`, `paidTo` and `typeWord`: the address tab
  of the spreadsheets of txs and pendingtxs follows them. Run `Cli.Sheet`.
- The vectors of `test/data/tx-sweep/*.json` pin the `"type"` and the
  `"amount"` of a summary. A new type word breaks the spec `Tx.Sweep`.
- A new constructor of `TxType` or of `Response`: also `Arbitrary.hs`, and
  `placeOf` and `txOf` in `Workflow/TestUtils.hs`.

## Tests

- Layout tests: `verboseSpec` in `Cli/PrettyPrinterSpec.hs`, literal lines on
  the summaries of `TestUtils.hs`: `paymentSummary`, `spendSummary`,
  `splitSummary` (pays the portfolio and another wallet), `externalSummary`
  (no portfolio). `txHeldPending` and `txOfFile` make a `TxInfo` not sent yet.
- A command shows the transaction on stderr before a question (`beforeSending`
  in `sendTxRead`): a workflow test reads `runErr` up to the question.
