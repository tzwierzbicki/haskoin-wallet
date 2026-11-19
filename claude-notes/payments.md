# Payments and pending transactions

Building a payment (preparetx, send, sendall, bumpfee), keeping it as a
pending transaction, its change address and the coins it holds.

## The change address

- No row says that a change address is taken. `changeAddress` in
  `Database/PendingTx.hs` gives the lowest change address that has no
  transaction and that no pending transaction pays.
- `insertPendingTx` is where every new pending transaction takes one
  (preparetx, send, bumpfee, importtx), and `removePendingTx` where it is
  given back.

## What a pending transaction holds

- The coins, in `Database/PendingTx.hs`: those it spends (`lockedOutpoints`),
  and those of the payments it replaces (`replacedOutpoints`).

## Between the build and the keeping

- A question about the transaction a payment builds: ask between the build
  and `keepPreparedLeaving`, which writes the `--output` file. `txInfoOf pool
  tsd` gives its summary, with the tags of its addresses, before anything is
  kept, as `confirmMixing` in `Commands.hs` does.
- `storePrepared` in `Commands.hs` keeps a transaction just prepared among
  the pending ones and writes the `--output` file. `keepPreparedLeaving` and
  `cmdPrepareSweep` go through it.

## Tests

- A payment shuffles its outputs: a workflow test finds the change by its
  address among `inMempool`, as `changeOn` in `Workflow/BumpFeeSpec.hs` does.
