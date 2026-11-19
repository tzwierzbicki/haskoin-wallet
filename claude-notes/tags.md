# Tags

Tags on addresses, transactions and contacts: the tags of a change address,
the tags that lock their coins, and the `--tag` filters of reports and exports.

## The tags of a change address

- Never put unasked: `changeTakesTags` in `Database/PendingTx.hs`
  (`--tag-change`), `retagChangeChain` in `Database/ChangeChain.hs`
  (`tag --with-change`), `untagFreedChange`.
- `touchedBy` names as change the internal addresses a transaction pays, and
  no other: change `--change-back` sent to a receiving address is not in it.
  It can also be the address the payment spends from, so addresses loop.
- The transactions at an address: `txidsAtQuery` in `Database/ChangeChain.hs`,
  then `liveSummary`. A summary alone tells spending from paying.

## A tag that locks its coins

- The lock is a column of the tag row, nothing per coin. `userLocks` in
  `Database/PendingTx.hs` reads the lockcoin and tag locks
  (`Database/Coin.hs` imports that module).
- A payment opens its `--spend-from-tag` tags: `spendableCoinsOpening`,
  `buildTxSignData`, then `importPreparedTx`, which keeps them in the pending
  row. `refuseLockedCoins` asks again at signtx and broadcast.
- A command that may lock: `tellingStopped`.
- Which coins a tag locks: `tagLocks` joins the rows of `DBAddressTag` to
  the coins by account and address text, which the tag row holds. It does
  not read the row of the address.

## Tags in the wallet file

- A tag row can come before the row of its address: `importwallet` keeps the
  tags of a change address, or of one past the scan limit, without generating
  it (`labelAddressRecord` in `Labels.hs`, `tagChainAddress`). A query over
  `DBAddressTag` must not count on the address row.
- A tag that nothing has: on the first xpub record of `databaseRecords` only
  (`namingUnusedTags` in `Labels.hs`). `takeTagSettings` makes the tags of a
  record tags of the database, those of the key records whatever the scope.

## Filters of tags

- A filter of tags on what `wholeHistory` reads (a statement, the journal):
  its rows have no tags of own addresses. `txSelection` in `Commands.hs` asks
  the database (`txidsKept`), and the pure modules ask `keepsTx` of
  `TxSelection.hs`. For addresses, `addressSelection` and `keepsAddress` of
  `AddressSelection.hs` do the same.
- The query plan of `txKept` in `Database/Tag.hs` is pinned: see
  `claude-notes/database.md`.
