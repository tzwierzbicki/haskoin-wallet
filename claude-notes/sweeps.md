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

- `refuseOwnAddresses` in `Commands.hs` refuses a sweep from an address the
  wallet holds on the network of the sweep (`heldOnNetwork`), twice: for the
  addresses given, before the keys, and for the coins found. So the builder
  gets no coin of the wallet from `preparesweep`, though it still reads one.

## The size and the fee

- `sweptInputKind` counts a P2PKH coin for an uncompressed key, 180 bytes,
  unless its address is in `compressedAddrs` (the keys given) or in
  `walletAddrs`. So a sweep from `--sweep-from`, `--address-file` or `--coin`
  alone pays 32 bytes more for each legacy coin than one from `--key-file`.
- The view before signing does not have that count: `estimatedSize` in
  `Tx/Signing.hs` reads it back from the fee over the rate of the file. A
  change of the size rule of the builder must keep the two in step, or the
  view shows another rate than the one asked.

## The dust coins

- `Tx/Dust.hs` has the rule and the report for preparesendall and for
  preparesweep, on a coin with its input (`SizedCoin`). `coinsWithDust` in
  `Commands.hs` is the one decision of both: option, question, or neither.
- The sweep sizes its coins with `sizedSweepCoins`, the same count as
  `sweepTxSize`. A coin left must go through `sweepingOnly`, which drops
  its parent transaction too, and is named in `ResponseSweep` (`UnsweptCoin`).
- The question comes after the keys are read: the keys tell the addresses.
  `addressesWithoutCoin` reads the coins found, not the coins swept.

## Tests

- `Workflow/SweepSpec.hs`: `withChain oldKeyChain`, `writeKeyFile` and
  `wikiKeyWif` of `Workflow/TestUtils.hs`. The coin of the old key holds
  30000 at 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK. `uncompressedKeyChain` holds
  the same at `wikiUncompressedAddr`, the address of its uncompressed key.
- `smallCoinsChain` of `Workflow/SweepSpec.hs` adds 600 and 900 at
  `externalAddr2`: dust at 10 sat/vB, where a coin without its key costs 1800.
- The signing vectors of `test/data/tx-sweep/` (its `README.md` says where
  they come from) pin the summary of each sweep, type and amount included.
