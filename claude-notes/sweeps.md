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
  addresses given, before the keys, and for the coins found. `Tx/Sweep.hs`
  counts on it: a sweep it builds has no input path, and the paths of
  `SweepCoins` are of destinations only (`destinationPaths`).

## The keys

- `readSecKeys` (`Cli/Prompt.hs`) reads a key file whole, a pipe to its end
  (`askToEnd`), and a terminal line by line (`askKeyLines`) until its
  `KeysWanted` says the keys are complete, or an empty line. preparesweep
  gives `anyKeys`, and signsweep `keysOfCoins` on `sweptAddresses`.
- Keys on a pipe leave no line to a later question: a test that pipes keys
  to `preparesweep --key-file -` gives `--spend-dust` or `--leave-dust`.
- Keys typed on a terminal are tested with `hwAtKeyboard`, a step for each
  prompt. A paste of several lines is the keys of one step.
- A WIF of no coin for a test: `otherKeyWif` of `Workflow/SweepSpec.hs`, the
  key of the number 1. `KwDiBf89…7Bqcefezr` of the sweep vectors is the key
  of 17, not of 1.

## The size and the fee

- `sweptInputKind` counts a P2PKH coin for an uncompressed key, 180 bytes,
  unless its address is in `compressedAddrs` (the keys given). So a sweep
  from `--sweep-from`, `--address-file` or `--coin` alone pays 32 bytes more
  for each legacy coin than one from `--key-file`.
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
  `hw/btc/sweep-with-wallet-coin` is the one test of a sweep file that holds
  a coin of the wallet, which `signsweep` and `show` still read.
- In `Tx/SweepSpec.hs`, two builder tests pin lengths of real signatures
  (71 or 72 bytes): a change of the coins or the amounts of the fixture
  moves them. Read the new values from the run, and check the parts add up.
