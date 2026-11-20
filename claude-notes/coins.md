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

- `holdsTokens` of `CashTokens.hs` is the one test of it. haskoin-store 2.0.0
  lists the coin at the plain address of its script, with the token prefix
  in `script`. `sync` keeps it as a coin, with the column `tokens`, which
  gives the lock `LockedByTokens`, the first of `coinLocks`. Nothing opens it.
- A new command that picks coins of the wallet needs no check: it reads
  `coinLocks`. Code that takes coins from the store itself does need one, as
  `fetchSweepCoins` and `spentCoins` of `Tx/Signing.hs` have. `groupCoins`
  compares `lockingScript`, the script without the prefix.
- `sync --full` writes the column again: `refreshCoins` counts a coin whose
  column is wrong as moved. A test of `CoinSpec.hs` pins it.
- To test one: `tokenFunding`, `tokenCoin` and `holdingTokens` of
  `TestUtils.hs`, `keyTokenFunding` of `Workflow/TestUtils.hs`. On a live
  chain: `withTokenWallet`, `payTokensFromNode` and `tokensOf` of
  `RegtestUtils.hs`. The fake store gives a token output its plain address.
- A new part of `CoinBalance` or a new field of `DBCoin`, `SweepCoins` or
  `SyncRes`: many specs build these by position, and the pinned JSON, CSV
  and spreadsheet rows of `AddressSpec`, `SheetSpec` and `Workflow/OutputSpec`
  list every part. The build names the first kind, a test run the second.

## Tests

- A test that reads the raw transaction of a coin: `insertRawTx` stores it
  under its real hash, and most stores of `SyncSpec` list it under
  `fakeTxHash n`, so `lookupRawTx` finds none. Use a real id, as
  `realPaymentStore` does, or the `Chain` of a workflow test.
- A payment that its sender replaces: `unsettledPayment` and
  `raisedFeePayment` in `TestUtils.hs`. `takingTx` in `HwTestUtils.hs` puts
  the second in the mempool of a `Chain` in the place of the first, and
  `raiseFeeFromNode` in `RegtestUtils.hs` does it on btcreg.
