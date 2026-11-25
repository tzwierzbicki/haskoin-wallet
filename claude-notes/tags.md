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

- Where `tag --with-change` stops: `stopsOf` in `Database/ChangeChain.hs`,
  from the payments that pay each change address (`Payer`). An address below
  a stop has no payer that spends from the group, so only the first address
  that could not take the tag is a stop.

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

- The tags in the reason of a coin (`leftReasonText`): `tagLocks opened net`
  gives, by outpoint, the locking tags that the payment did not name.
  `chosenCoins` and `leftCoinsOf` take them. `CoinLock` holds no tag.

## Coins of different tags

- The rule of the question is in `Tx/Mixing.hs`: `joinsOutsideTags` for a
  payment, with the tags of `--spend-from-tag`, and `addsGroup` for bumpfee.
  `confirmMixing` in `Commands/Payment.hs` asks.

## The Tags column of a table

- Its width is read once for a listing, before the first chunk: `tagWidths`
  in `Database/Tag.hs`, through `listingTagWidths`, `printListing` of
  `Main.hs` and `chunkDoc`. In `PrettyPrinterSpec`, `TagWidths 12 12 12`
  keeps the literals written when the column was 12 wide.

## Tags in the wallet file

- A tag row can come before the row of its address: `importwallet` keeps the
  tags of a change address, or of one past the scan limit, without generating
  it (`labelAddressRecord` in `WalletFile.hs`, `tagChainAddress`). A query over
  `DBAddressTag` must not count on the address row.
- A tag that nothing has: on the first xpub record of `databaseRecords` only
  (`namingUnusedTags` in `WalletFile.hs`). `takeTagSettings` makes the tags of a
  record tags of the database, those of the key records whatever the scope.

## Filters of tags

- A filter of tags on what `wholeHistory` reads (a statement, the journal): its
  rows have no tags of own addresses. `txSelection` in `Commands/TagFilter.hs`
  asks the database (`txidsKept`), and the pure modules ask `keepsTx` of
  `TxSelection.hs`. For addresses, `addressSelection` and `keepsAddress` of
  `AddressSelection.hs` do the same.
- The query plan of `txKept` in `Database/Tag.hs` is pinned: see
  `claude-notes/database.md`.
