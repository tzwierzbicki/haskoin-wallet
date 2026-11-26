# The balance of a portfolio

The amounts `hw` shows for a portfolio on a network: the line of
`portfolios`, of `sync` and below the txid of a sending, and the title of
`balances`. Also the amounts of one address in `addresses`. The memory
`output-views` has the decisions of the maintainer.

## Where the amounts come from

- Two splits of what a portfolio holds. By what it can spend: `coinTotals`
  (`Database/Coin.hs`) adds up the coins of a pool, the total and the
  spendable part. By what is confirmed: the two balances of the store, which
  `sync` writes on the address and account rows.
- The short views give the first split only. `NetworkView` (`portfolios`) and
  `SyncRes` (`sync`, and the sync after a sending) carry both, and `--json`,
  `--csv` and `portfolios --verbose` give both.
- `Database/Address.hs` imports `Database/Account.hs`, so `NetworkView` cannot
  hold a `CoinBalance`: `CoinTotals` of `Account.hs` is the type for it, and
  `portfolioView` takes the function that computes it.
- An address of `addresses`, and of the answers of `receive`, `label`, `tag`
  and `show`, comes with a `CoinTotals` too: `coinTotalsAtAddresses` reads the
  coins of one page of addresses, and `totalsByAddress` serves the pool as well.

## Where they are printed

- `balanceFigures` of `Cli/PrettyPrinter/Balance.hs` writes "total, spendable,
  unspendable" for `balances`. `shortBalanceFigures` leaves out a rest of
  zero, for `networkFigures` (`portfolios`) and `syncHoldsDoc` (`sync`,
  `syncedAfterDoc`). The table is `portfolioTableColumns`.
- The line of `addresses` is another one (`compactAddressDocOf`): the total,
  the unspendable amount unless zero, what the address received, its
  transactions. `verboseAddressDoc` and `addressRowDoc` give the spendable one.
- An address of a pool that never synced is `NotSynced` (`AddressFunds`,
  `Database/Address.hs`), set by `fundsAt` of `Commands/Answer.hs` from
  `poolEverSynced`. Each address view, `addressColumns` of `Export/Sheet.hs` and
  the JSON (`addressWithFunds`, key `synced`) branch on it.
- `portfolios`: `networkViewSynced` of `NetworkView`, a plain flag, set by
  `portfolioView`. `networkFigures`, `verbosePortfolioDoc`, `portfolioRowDocs`,
  `portfolioColumns` and the JSON key `synced` branch on it.
- `portfolios` nests: `listedWalletDocs` (`Cli/PrettyPrinter/Portfolio.hs`)
  groups the views by wallet, and `walletDoc` puts the portfolios under
  `walletLine`. That line is also the one of `show FILE`, `importwallet` and
  `restoredatabase`: search the tests and the README for `fingerprint bc560f95`.
- `balances`, `coins`, `txs`: `groupSynced` of `Group` (`Cli/Response.hs`, JSON
  key `synced`). `sectionSync` of `Commands/Answer.hs` gives a network that
  never synced, with addresses, a group without rows. Such a section shows only
  with `listingShownEmpty` (`Cli/Listing.hs`); `onceSynced` of `sectionsPrinter`
  makes `notSyncedLine` its one row, or `listingDoc` drops it.

## Tests

- A change of the line breaks literal lines in `Cli/PrettyPrinterSpec`,
  `Workflow/PortfolioSpec`, `Workflow/SendSpec`, `Workflow/SweepCommandSpec`,
  `Workflow/TokenSpec` and `Workflow/OutputSpec` (its colour test too), and
  `console` blocks all over the README: search it for `spendable 0.`.
- An unconfirmed payment from another wallet on the fake chain:
  `modifyIORef chain (takingTx (fundingTx 9 [(externalAddr 1, 20000)]))`, as
  `twoUnconfirmed` of `Workflow/TestUtils` does after a `send`.
- A new row in the verbose view of an address moves the QR code tests of
  `Workflow/OutputSpec`, which count its lines (`codeUnderAddress`: 4, their
  homes never synced). The address tests are `addressesHoldSpec` and
  `notSyncedSpec` there, read by `addressFundsOf`.
- A home of `withHome`, or one that ran no `sync`, reads `not synced` at each
  address, in `portfolios`, `balances`, `coins` and `txs`. For `unused` or amounts, sync on
  a chain: `Chain 800002 []` has no transaction. A wallet built in the
  database needs `updateSyncedBlock` on its pool (`Database/AccountSpec`).
- A network that never synced beside one that did: `withChainServing [btc] []`,
  then `receive --network bch` (`notSyncedPortfolioSpec`, `Workflow/OutputSpec`).
- A group of a listing in a unit test: `mainGroup rows`, or `notSyncedGroup`
  for a network that never synced (`TestUtils.hs`).
