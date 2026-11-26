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
  portfolio. `typeWord` in `Database/TxInfo.hs` has the word of each type.
- In `Cli/PrettyPrinter/Transaction.hs`: `verboseTxDoc` is the whole view, `headlineDoc`
  its first line (`headlineTime` for the time), `spendsField` the `Spends`
  line. `txAnswerDoc` is the view as an answer.
- `spentFrom` and `paidTo` of `Tx/Addresses.hs` make the address lists, for
  the screen and for the spreadsheets.
- A summary keeps one total for an address. What each output pays an address
  that several outputs pay is in `summaryRepeatedOutputs` (`paidByAddress`).
  A view that lists outputs takes `paidOutputs` or `eachOutput` of
  `Tx/Addresses.hs`. One that names an address once takes `paidTo`.
- The listings have their own first line (`compactHeadline`, `timeCell`):
  a change of `headlineDoc` does not reach them.
- The fee rate is not a field: `summaryFeeRate` makes a `PaidRate`
  (`Tx/Size.hs`) from the fee and the size, for the screen, the sheets and
  the JSON `feebyte`. The stored summary is that JSON, so it carries
  `feebyte`, which is never read. In a test, use `feeRateText` of `TestUtils.hs`.
  `paymentSummary` and `spendSummary` pay 1000 for 226 vB: "4.4 sat/vB".

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

- `Export/Sheet.hs` uses `spentFrom`, `paidTo` and `typeWord`: the address
  tab of the spreadsheets of txs and pendingtxs follows them. Run
  `Export.Sheet`.
- The vectors of `test/data/tx-sweep/*.json` pin the `"type"` and the
  `"amount"` of a summary. A new type word breaks the spec `Tx.Sweep`.
- The vectors of `tx-signing` and `tx-sweep` pin `feeByte` as the JSON number
  hw writes (44.2, or 10). `tools/verify_vectors.py` computes it apart from
  hw (`paid_rate`), and does not know the type `external` of a sweep vector.
- A new constructor of `TxType` or of `Response`: also `Arbitrary.hs`, and
  `placeOf` and `txOf` in `Workflow/TestUtils.hs`.
- A new field of `TxSummary`: `Arbitrary.hs`, the summaries of `TestUtils.hs`,
  `stuckSummary` of `Tx/BumpFeeSpec.hs`, and, for its JSON key, the two
  literals of `Database/TxInfoSpec.hs` that pin every key.

## Tests

- Layout tests: `verboseSpec` in `Cli/PrettyPrinterSpec.hs`, literal lines on
  the summaries of `TestUtils.hs`: `paymentSummary`, `spendSummary`,
  `splitSummary` (pays the portfolio and another wallet), `externalSummary`
  (no portfolio), `twoOutputsSummary` (two outputs to one address).
  `txHeldPending` and `txOfFile` make a `TxInfo` not sent yet.
- A command shows the transaction on stderr before a question (`beforeSending`
  in `sendTxRead`): a workflow test reads `runErr` up to the question.
- The lines of one address in a view a command printed: `shownAfter` of
  `Workflow/TestUtils.hs` gives what follows the address on each, sorted. An
  address the transaction spends from too is also on its `From` line.
