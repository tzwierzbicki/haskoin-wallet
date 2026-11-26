# Amounts and units

The unit an amount is shown or typed in: `--unit`, the key `unit`, a unit
typed after an amount, btc on Bitcoin and bch on Bitcoin Cash. The memory
`amount-units` has the decisions of the maintainer.

## Where the unit is decided

- Two types in `Amounts.hs`. `UnitChoice` is what the user chose, before a
  network is known: `commandUnit` of `Config.hs` makes it, and `Main.hs`
  hands it to the commands and the printers. `AmountUnit` is the unit of an
  amount on its network, which `unitOn net` gives: btc and bch swap there.
- The functions that an answer or a command calls take the choice and
  resolve it with their network (`txAnswerDoc`, `preparedDoc`, `sweepDoc`,
  `sectionsPrinter` for each group, `syncResultDoc`). Their helpers take an
  `AmountUnit`. A view over several networks resolves on each row.
- A command that reads amounts calls `paymentUnit net choice` once it knows
  its network: `preparePayment`, `prepareSendAll`, `buildSweep`. It refuses
  the `--unit` of the other chain. A new command that reads amounts needs
  the call, and its line in the Amounts section of the README.
- A table names the unit after each amount, never in a heading. A column
  of amounts is `amountColumn` of `Table.hs`, and its cells are `amountCell`
  of `Base.hs`, which the compact views use too (`heldAmountCell`,
  `waitingAmountCell`, `signedAmountCell`). Its width is `amountCellWidth`.
- The reports and the journal write whole coins with `showCoins`, beside
  their own name of the coin (`assetUnit`, `commodityOf`): no unit of hw.

## Tests

- In a unit test, a function that takes the choice gets `DefaultUnit
  UnitBitcoin`, and one that takes an `AmountUnit` on bch `UnitBitcoinCash`.
- A new name or refusal of a unit breaks literals in `AmountsSpec`,
  `Tx/RecipientSpec`, `ConfigSpec` (`newFile`, the file hw writes),
  `Workflow/PaymentSpec`, `Workflow/TokenSpec`, `Regtest/PaymentSpec`
  (`coin`, by chain) and the `console` blocks of the README.
- A change of the cells of the tables breaks some 110 row literals of
  `Cli/PrettyPrinterSpec`, most in a test without the header of its table,
  and those of `Workflow/OutputSpec`, `TagSpec` and `PortfolioSpec`.
- A portfolio with a coin on bch in a workflow test: `withChain
  (confirmedChain cashCoinTx)`, then `cashSynced` of `bitcoinCashSpec`
  (`Workflow/PaymentSpec`). `TokenSpec` has one with a token coin.
