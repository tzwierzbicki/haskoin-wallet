# Coins

The coins of the wallet: what `sync` does to a coin, and the lock of
`lockcoin`. The locks of tags are in `claude-notes/tags.md`, and the coins a
pending transaction holds in `claude-notes/payments.md`.

## A coin whose payment is replaced

- Which transaction replaced which: only the raw transactions tell, since a
  summary keeps the inputs of the wallet alone. `lockHeirs` in
  `Database/Coin.hs` reads them inside `refreshCoins`, where the raw
  transaction of a coin that is gone is still stored (`pruneRawTxs` runs later).
- The lock of `lockcoin` is a column of the coin row, so a new txid is a new
  row: `lockHeirs` moves it, at the same address only, and `sync` names both
  coins (`syncResLocksMoved`).

## A coin of Bitcoin Cash that holds tokens

- haskoin-store 2.0.0 lists it under the plain address of its script, with
  the token prefix in `script`. `groupCoins` in `Database/Coin.hs` leaves it
  out: its script is not the standard script of its address. It is no coin
  of the wallet, but `txs` counts its amount, from the outputs of the store.
- `spentCoins` in `Tx/Signing.hs` refuses it too: the prefix does not decode
  as a `ScriptOutput`. `sweep` meets it first: see `claude-notes/sweeps.md`.
- To test one: the fake store reads every address as on btc (`addressOf` in
  `HwTestUtils.hs`), so a token output has no address there. A prefix: the
  byte 0xef, 32 bytes of category, 0x10, then an amount such as 0x64.

## Tests

- A test that reads the raw transaction of a coin: `insertRawTx` stores it
  under its real hash, and most stores of `SyncSpec` list it under
  `fakeTxHash n`, so `lookupRawTx` finds none. Use a real id, as
  `realPaymentStore` does, or the `Chain` of a workflow test.
- A payment that its sender replaces: `unsettledPayment` and
  `raisedFeePayment` in `TestUtils.hs`. `takingTx` in `HwTestUtils.hs` puts
  the second in the mempool of a `Chain` in the place of the first, and
  `raiseFeeFromNode` in `RegtestUtils.hs` does it on btcreg.
