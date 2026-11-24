# The chain of the server at a sync

What `sync` keeps of the chain of its last sync, the server it refuses for
being on another chain, and `sync --new-chain`.

## The rule

- A pool keeps two blocks in one row (`Database/BestBlock.hs`): the block the
  sync reached, and its settled block, 100 below (`settledHeightBelow`).
  `chainSeen` of `Sync.hs` is the pure check, and `seenChain` asks the store
  about every height in one question (`storeBlocksAtHeights`).
- `SyncMode` replaces the flag of a full sync. `NewChain` is a full sync that
  skips the refusal. `Full` refuses as `Incremental` does.
- A chain of 100 blocks or fewer has its first block as settled block, which
  every chain of the network shares: no refusal comes from it.

## Tests

- A store of `TestUtils.hs` has `deepBlock` at every height below the blocks
  it lists. For another chain, list another block at the height of the best
  block less 100. For a server that is behind, lower `fakeBest`.
- The `Chain` of `HwTestUtils.hs` names a block by its height alone: it can be
  shorter, never another chain at the same height.
- A regtest that calls `startAgain` under a wallet that synced: `sync` then
  refuses the chain. Follow it with `sync --network NET --new-chain`, as
  `Regtest/ReorgSpec.hs` does.

## The message

- A failure of a network in `sync` and `recover` is wrapped by `wrappedAfter`
  of `Cli/Style.hs`, as an error is. A command it gives is on a line of its
  own, after `\n` and two spaces. In a test, `unwrapped` leaves three spaces
  before that command: check the sentence and the command line apart.
