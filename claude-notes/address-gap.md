# The address gap

The gap of BIP44: how many unused addresses `receive` gives out, and how far
the scan of `sync` and `recover` looks for used addresses.

## The rule

- The gap is counted from the last used address of a chain, never from the
  last address the chain has. `afterLastUsed` of `Database/Address.hs` is the
  one place that says where: `checkGap`, `withinScanLimit` and `watchGap` of
  `Sync.hs` read it.
- The scan is `usedChainAddresses` of `Sync.hs`. It asks about every address
  the chain has from the last used one (`ChainState`), then the gap past the
  last used address found, each address once. A chain of a seed with no
  account, as `recover` scans, is `newChain`.

## Tests

- A change of what the scan asks moves pinned values of `SyncSpec.hs`: the
  requests ("Scanning a chain", "Watching the gap", the funds at other
  paths), the `LookingForAddresses` counts of "The steps a sync tells", and
  the "as far as N" of the question of a store that reports every address.
- That N is the used addresses found past the ones the chain has: 1002 for a
  gap of 3 on a new chain, 1001 when the account has one address, 1019 for
  the gap of 20 and one address.
- The test wallet has ten written-out addresses for each chain, so the scan
  tests set `configGap = 3`. `account` has the receiving address 0, never
  paid: the gap still counts from the start of the chain.
