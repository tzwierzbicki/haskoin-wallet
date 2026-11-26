# Payments and pending transactions

Building a payment (preparetx, send, sendall, bumpfee), keeping it as a
pending transaction, its change address and the coins it holds.

## The change address

- No row says that a change address is taken. `changeAddress` in
  `Database/PendingTx.hs` gives the lowest change address that has no
  transaction and that no pending transaction pays.
- A change address that a pending transaction pays needs its row:
  `nextChangeAddress` leaves out the held paths among the rows only, and
  then generates the next index without a look at them. For a file of
  another computer, `generatePaidAddresses` makes the row in `insertPendingTx`.
- `insertPendingTx` refuses a coin the wallet lacks before an address it
  lacks: with no coins, as offline, `importtx` stops at the coin.
- `insertPendingTx` is where every new pending transaction takes one
  (preparetx, send, bumpfee, importtx), and `removePendingTx` where it is
  given back.

## What a pending transaction holds

- The coins, in `Database/PendingTx.hs`: those it spends (`lockedOutpoints`),
  and those of the payments it replaces (`replacedOutpoints`).
- A new pending transaction that spends such a coin: `heldCoinRefusal`, from
  `insertPendingTx`. A pending transaction is named by `pendingShortId`: the
  start of its nosigHash, and of its txid once it is signed. A test that
  signs first takes the txid for the message.

## The selection of the coins

- A search of `Tx/CoinSelection.hs` goes up to 100000 steps, so a step must
  cost the same whatever the coins taken: it keeps their sums (`Taken`, and
  `SizeTally` of `Tx/Size.hs`). `txSize` walks every input: never in a step.
- `preparetx` slow on a large wallet: time its steps before a guess. Reading
  1000 coins takes 0.3 s (`fetchWalletCoins`, a query for each path).

## An address paid in several outputs

- The refusals are `checkPaidOnce` and `checkChangeApart` of
  `Tx/Recipient.hs`. The first runs when the recipients are read, before the
  fee question. The second runs after `buildTxSignData`, on `changePaid` of
  `PreparedPayment`: only the build knows whether there is change.
- `bumpPlan` of `Tx/BumpFee.hs` gives each transaction it replaces one change
  output, one on a change chain first. The other outputs to the portfolio
  are payments it keeps, and so is every output of `summaryRepeatedOutputs`.

## Between the build and the keeping

- A question about the transaction a payment builds: ask between the build
  and `keepPreparedLeaving`, which writes the `--output` file. `txInfoOf pool
  tsd` gives its summary, with the tags of its addresses, before anything is
  kept, as `confirmMixing` in `Commands/Payment.hs` does.
- `storePrepared` in `Commands/Payment.hs` keeps a transaction just prepared
  among the pending ones and writes the `--output` file. `keepPreparedLeaving`
  and `cmdPrepareSweep` go through it.

## A transaction handed to the store

- The store taking a transaction proves nothing: it must list it afterwards
  (`listedSoon` in `Store.hs`). `broadcast` and the part of `sync` that sends
  again (`syncPending`) both wait for that, ten seconds at most.
- In a unit test, the fake store of `TestUtils.hs` takes a transaction and
  never lists it, so a `sync` tells it as not sent again. `takingPublished`
  makes a store that lists what it took. A fake store never pauses
  (`storeListingPause` is 0), so such a test does not wait.
- On a live chain, `startStoreWithoutNodeRpc` of `RegtestUtils.hs` gives the
  store that takes a payment Bitcoin Core refuses, and never lists it.

## A payment sent, and the sync that follows

- `sendTxRead` in `Commands/Send.hs` ends every sending (send, sendall, sweep,
  broadcast) with `syncAfterSending`. It syncs `touchedPools` of
  `Database/Coin.hs`, and its failure is data (`SyncAfter` in `Sync.hs`),
  never an error. The answers: `ResponseSentPending`, `ResponseSentTx`.
- When that sync failed, the payment stays pending, with `pendingTxOnline`,
  until a sync finds it (`retirePendingTx`). The wallet then lists the coins
  it spent and not its change: `sentHolds` in `Database/PendingTx.hs` gives
  those coins, and `notSpendableNow` in `Database/Coin.hs` their total.

## Tests

- A test of a payment sent and still pending needs that sync to fail:
  `withChainAwayAfterSending` of `HwTestUtils.hs` in a workflow test,
  `payUnseen` of `RegtestUtils.hs` on a live chain. With `withChain`, the
  payment is in the history: `latestTx` of `Workflow/TestUtils.hs` reads it.
- A payment sent that a sync found in conflict and that still holds a coin:
  spend two coins, then `takingTx` a rival that spends one of them, as
  `beforeSyncSpec` in `Workflow/PaymentSpec.hs` does.
- A payment shuffles its outputs: a workflow test finds the change by its
  address among `inMempool`, as `changeOn` in `Workflow/BumpFeeSpec.hs` does.
