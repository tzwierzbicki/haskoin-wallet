# Sweeps

`preparesweep` and `signsweep`: a transaction that spends coins from outside
the wallet with their private keys. For the view of a sweep, see
`claude-notes/transaction-view.md`.

## Where things are

- `cmdPrepareSweep` in `Commands.hs` does the IO. `Tx/Sweep.hs` has the
  parts: `fetchSweepCoins` (asks the store), `buildSweepSignData`,
  `addressesWithoutCoin`, `signTxWithKeys`.
- A sweep that pays an address of the wallet is of the portfolio of that
  address: kept as pending (`storePrepared`), and a payment received
  (`TxCredit`) in its summary.
- A sweep that pays no portfolio is of none: `parseOutsideTxSignData`, type
  `TxExternal`, nothing kept. It needs `--network` and `--output`.
- The answer is `ResponseSweep`: the place when a portfolio holds the sweep,
  and the addresses given that hold no coin. `sweepDoc` in
  `Cli/PrettyPrinter.hs` prints it, `nextStepsPrinter` its next steps.
- `addressesWithoutCoin` takes the addresses of `--sweep-from` and of the
  `--address-file` only. A key of a `--key-file` has an address of each kind,
  and a key must not be printed.

## Tests

- `Workflow/SweepSpec.hs`: `withChain oldKeyChain`, `writeKeyFile` and
  `wikiKeyWif` of `Workflow/TestUtils.hs`. The coin of the old key holds
  30000 at 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK.
- The signing vectors of `test/data/tx-sweep/` (its `README.md` says where
  they come from) pin the summary of each sweep, type and amount included.
