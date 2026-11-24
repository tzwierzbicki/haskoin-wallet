# Hidden networks

What `hw` does with a network that the key `shown-networks` leaves out: the
answers, the refusals, `recover` and `verifymsg`. The memory `hidden-networks`
has the decisions of the maintainer.

## Where a hidden network is left out

- A new view, answer or lookup filters with `onShownNetworks` (`Config.hs`).
  What the user names that is on hidden networks alone goes through
  `shownAmong` (`Commands.hs`), which refuses with `onHiddenNetworks`.
- A new command with `--network` gets a line in `networkNamedBy`
  (`Cli/Parser.hs`), which has no wildcard. `refusedNetwork` refuses a hidden
  network there before the command runs. `setdefault` and `verifymsg` answer
  `Nothing` on purpose.
- An address typed that no wallet has: `readingNetworks` (`Commands.hs`) gives
  the networks that read it. `notGeneratedOrKept`, `contactNetwork` and
  `signerAddress` branch on it.

## Where a hidden network is told

- `portfolios`: `viewOfPortfolio` (`Commands.hs`) drops the hidden networks of
  `portfolioView` and sets `viewUsedOnHidden`. `compactPortfolioDoc` writes
  "nothing on the shown networks" from it. `portfolioView` alone leaves it
  false.
- `recover`: `walletHiddenUse` gives the last field of `ResponseRecover`.
  `usedOnHiddenDoc` prints it, and `showNetworksStep` is its next step.

## Tests

- `Workflow/HiddenNetworksSpec.hs`. The home of a workflow test shows every
  network: `setShownNetworks home [...]` hides the others. A unit test takes
  `showingEveryNetwork` (`TestUtils.hs`).
- A portfolio used on a hidden network alone: `withChain (confirmedChain
  cashCoinTx)`, `setShownNetworks home ["btc"]`, then `recover` with `seedOnce`.
- A field added to `ResponseRecover` is positional in some 25 places:
  `Cli/Response.hs`, `Cli/PrettyPrinter.hs`, `Commands.hs`, `Arbitrary.hs`,
  `Cli/ResponseSpec`, `Cli/PrettyPrinterSpec` and `Workflow/TestUtils.hs`.
