# The balance of a portfolio

The amounts `hw` shows for a portfolio on a network: the line of
`portfolios`, of `sync` and below the txid of a sending, and the title of
`balances`. The memory `output-views` has the decisions of the maintainer.

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

## Where they are printed

- `balanceFigures` of `Cli/PrettyPrinter.hs` writes "total, spendable,
  unspendable" for `balances`. `shortBalanceFigures` leaves out a rest of
  zero, for `networkFigures` (`portfolios`) and `syncHoldsDoc` (`sync`,
  `syncedAfterDoc`). The table is `portfolioTableColumns`.
- The line of `addresses` is another one (`compactAddressDocOf`): what the
  address can spend, and what it received.

## Tests

- A change of the line breaks literal lines in `Cli/PrettyPrinterSpec`,
  `Workflow/PortfolioSpec`, `Workflow/SendSpec`, `Workflow/SweepCommandSpec`,
  `Workflow/TokenSpec` and `Workflow/OutputSpec` (its colour test too), and
  `console` blocks all over the README: search it for `spendable 0.`.
- An unconfirmed payment from another wallet on the fake chain:
  `modifyIORef chain (takingTx (fundingTx 9 [(externalAddr 1, 20000)]))`, as
  `twoUnconfirmed` of `Workflow/PortfolioSpec` does after a `send`.
