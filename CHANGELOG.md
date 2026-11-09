# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

These changes will be released as version 0.10.0. Most of `hw` was rewritten
for this version. It adds native SegWit accounts, backups in shares, PSBT
files, coin control and fee bumping. Many commands and options have new names,
so read [Before you upgrade](#before-you-upgrade) first.

### Before you upgrade

- **The database is upgraded on the first command.** `hw` 0.10 upgrades a
  database of 0.8 or 0.9, after it backs it up to `~/.hw/migration-backups`.
  Do not run an older `hw` on the upgraded database. `hw` 0.8 would rewrite
  parts of it in a format 0.10 cannot read.
- **Transaction files are now PSBTs.** `hw` 0.10 reads no JSON transaction file
  of an earlier version. Prepare such a transaction again.
- **Scripts that call `hw` need checking.** Commands and options were renamed,
  and many answers changed. See [Renamed](#renamed) and [Removed](#removed).
- **A passphrase with accents is now normalized**, as BIP39 asks. Accounts that
  earlier versions made from such a passphrase still work, with a warning.
  Move their funds to an account made with 0.10.
- **A mnemonic split into parts** with `--split` is no longer read. Move its
  funds with `hw` 0.9, as the README says under
  [A mnemonic split into parts](README.md#a-mnemonic-split-into-parts).
- **An existing wallet keeps its file permissions.** Earlier versions often
  left it readable by other users. Make it private with `chmod 700 ~/.hw` and
  `chmod 600 ~/.hw/accounts.sqlite`.
- **The license is now MIT**, the license of haskoin-core. Released versions
  stay under the Unlicense.

### Added

Accounts and backups:

- Native SegWit accounts, as BIP84 derives them: `createacc --type bip84`.
  Their payments pay about 40% less in fees.
- Backups in shares, in the SLIP39 format: `mnemonic --shares N --threshold T`.
  Any T of the N shares rebuild the seed. Every command that asks for a
  mnemonic takes shares too.
- `recover` finds the accounts of a mnemonic that were paid, adds them and
  syncs them. Without `--network`, it searches Bitcoin and Bitcoin Cash both.
  It also tells of funds at paths `hw` cannot spend from, such as wrapped
  SegWit and Taproot accounts.
- `accounts --verbose` shows each account as an output descriptor (BIP380),
  and a bip84 account with its key as a zpub too. Its lines wrap on a
  terminal, so that a descriptor can be copied whole. `importacc` reads
  descriptors and extended public keys.
- `exportacc` and `importacc` use the BIP329 format. The file carries the
  accounts, their labels and their locked coins. `show FILE` shows what
  `importacc` would make of such a file, and changes nothing.

Addresses:

- `receive` shows a QR code of the new address on the terminal.
- `show` shows one thing of the wallet, as the word typed tells: the address at
  a path such as `/0/3`, an address, a transaction by its txid or nosigHash, a
  coin `TXID:INDEX`, or a file, any other word. Without a word, it shows the
  newest receiving address. A file is a transaction file, as its account sees
  it, or a file of accounts, as `importacc` would take each account, with the
  labels of the file under it.
  An address the wallet holds comes with its QR code, to give it out again.
  Run `show /0/3` on the offline computer to check an address the online one
  gives out: it derives the address from the public key. An address at a path
  the wallet does not hold is shown cut, its middle hidden behind dots, in JSON
  too. Its ends carry 128 bits, enough to compare, and nobody can pay it.
- `balances` lists the addresses that hold coins.
- `label` labels transactions too, and the prepare commands take `--label`.

Payments:

- Coin control. `--coin TXID:INDEX` and `--spend-from ADDRESS` choose the coins
  to spend. `--select` picks among them.
- `--strategy` sets how coins are selected. The default, `least-waste`, keeps
  the coins that waste the least fee.
- `lockcoin` and `unlockcoin` keep a coin from being spent. `coins --locked`
  lists the locked coins.
- `fees` shows the fee rates of recent blocks. `--fee-rate low`, `median` or
  `high` takes one of them. Without `--fee-rate`, the prepare commands ask.
  There is no default rate any more.
- `preparesendall` pays all the coins of an account. It asks about the coins
  worth less than their fee. `--spend-dust` and `--leave-dust` answer in
  advance.
- `bumpfee` replaces a stuck Bitcoin payment with one that pays a higher fee.
  The Bitcoin transactions `hw` builds are now marked replaceable (BIP125).
- `preparetx --change-back` sends the change back to an address it spends
  from. The key `change-back` of the configuration makes it the default.

Transaction files:

- Transaction files are PSBTs (BIP174). A hardware wallet or another wallet
  can sign them in place of the offline computer.
- `hw` reads, signs and sends the PSBTs of other wallets, in version 0 or 2
  (BIP370), in bytes or in base64.
- `broadcast --input` also takes a signed transaction in hexadecimal.

Messages:

- `signmsg` signs a message in the Bitcoin Signed Message format.
- `verifymsg` checks such a signature against its address.

Using `hw`:

- The help is laid out again, with a colour for each kind of thing to type.
  The commands that take a secret and need no server are marked.
- Listings show a compact view, two lines for each transaction or coin, and
  `--verbose` shows every detail. On a
  terminal they go through a pager, which keeps each line on one line of the
  screen, so that the columns line up.
- The compact views count the confirmations up to `999+ confirmations`.
  `--verbose` and the JSON answers give the full count.
- Transactions name the addresses of the wallet by their derivation path, such
  as `/0/3`, and show them and their labels in blue. `--verbose` gives every
  address in full. `coins` gives the address of a coin in full, with its path.
- `txs` shows the balance after each transaction.
- `--tabular` shows each element of a listing on one row of a table, in
  columns under a header: `accounts`, `addresses`, `balances`, `coins`, `txs`
  and `pendingtxs`. Nothing in a table is cut. `less` keeps the header of a
  table taller than the screen on the screen.
- `--csv FILE` writes a listing to a new CSV file, for a spreadsheet: every row,
  with the identifiers and amounts of the JSON answer in columns of their own.
  Amounts are in satoshis and times in UTC. A label that a spreadsheet would
  run as a formula gets an apostrophe in front.
- `exportsheet FILE` writes the six listings of the wallet, or of one account
  with `--account`, to a new OpenDocument spreadsheet, a tab each. Each cell
  keeps its kind: text, a number of satoshis, or a time in UTC. The header of
  each tab stays at the top as its rows scroll.
- Short ids: the first 8 digits of a hash name a transaction, a pending
  transaction or a coin (`a1075db5:1`) wherever the whole hash does. Each row
  of a table and each element of a compact view starts with it. `--verbose`
  gives the hash in full, then its short id, and `--csv` and `exportsheet` give both. When a short id matches
  two hashes of the wallet, `hw` names both and asks for the full hash.
  `preparesweep --coin` takes whole txids only. The JSON answers and the files
  keep whole hashes.
- `sync` and `recover` show what they are doing on a status line.
- Tab completion in bash, zsh and fish. It completes commands, options,
  account names, coins, addresses and pending transactions.
- `hw --version` shows the version of `hw` and of its database format.
- `--unit btc|bit|sat` sets the unit of amounts. An amount can carry its own
  unit, as in `1000sat`.
- The key `default-network` of the configuration sets the network of a command
  given no `--network`. `recover` searches Bitcoin and Bitcoin Cash instead.
- The key `unit` of the configuration sets the unit of the amounts of a
  command given no `--unit`.
- The key `listing-view` of the configuration sets the view of the six
  listings given no `--compact`, `--verbose` or `--tabular`. It is `compact`,
  `verbose` or `tabular`. `--compact` shows the compact view whatever the key
  says.

### Changed

- `sync` replaces `syncacc` and `discoveracc`. The first sync of an account
  finds its used addresses. When a server is down, `sync` still syncs the
  accounts of the other networks, and names those it left out.
- `backup` copies the whole database, labels and pending transactions
  included. To restore it, put the copy in place of `~/.hw/accounts.sqlite`.
- An account is known by its key, and no longer belongs to a wallet. `hw`
  records the fingerprint of its seed once it knows it.
- `createacc` asks for the mnemonic and the passphrase twice. A typo can then
  not create an account of another seed. Secrets are typed with no echo.
- `signtx` and `signsweep` show the transaction before they ask for the secret.
- Fees and sizes are in virtual bytes (sat/vB).
- New transactions are version 2, with a lock time near the current block
  height. Most wallets build theirs the same way.
- Each output takes the dust limit of its kind, as nodes relay it.
- `--min-conf` defaults to 0. Unconfirmed coins can be spent when the wallet
  made their transaction: its own change, and what one account pays another.
  An unconfirmed payment from someone else waits for a confirmation.
- `hw` asks before it takes a fee rate above 100 sat/vB. A transaction without
  a fee is refused.
- `hw` runs one command at a time on a database. A second command waits three
  seconds, then stops.
- `hw` never overwrites a file. It writes a file whole and to the disk before
  the file takes its name, so a crash leaves no half-written file.
- New mnemonics take their entropy from `/dev/random` where the system has
  one. `hw` refuses entropy with a visible pattern.
- `hw` skips an index whose child key is invalid, as BIP32 asks.

### Renamed

| 0.9 | 0.10 |
|---|---|
| `syncacc` | `sync` |
| `discoveracc` | `recover`, which now finds every paid account |
| `testacc` | `checkmnemonic` |
| `sendtx` | `broadcast` |
| `reviewtx FILE` | `show FILE` |
| `addrs`, `addrs --internal` | `addresses`, `addresses --change` |
| `version` | `hw --version` |
| `--satoshi`, `--bit` | `--unit sat`, `--unit bit` |
| `--fee` | `--fee-rate` |
| `--minconf` | `--min-conf` |
| `--recipientpay` | `--recipients-pay` |
| `--offset`, `--limit` | `--after ROW`, `--max N` |
| `--sweepfrom`, `--sweepto` | `--sweep-from`, `--sweep-to` |
| `--prvkeys` | `--key-file` |
| `--addrfile` | `--address-file` |

`label` now takes the address instead of its index.

### Removed

- `restore`. Put a backup in place of the database instead.
- `rolldice`. Roll real dice and type them in with `mnemonic --dice`.
- `--dust`. Each output takes the dust limit of its kind.
- `createacc --derivation`. `createacc` takes the next free index of the seed.
- `--split`, which split a mnemonic into parts and read them back. `hw` 0.10
  neither makes nor reads such parts. Use backups in shares.
- The key `recovery-gap` of the configuration, with `discoveracc`. Raise `gap`
  for one sync instead.
- The JSON transaction files of earlier versions.

### Security

An independent review of 0.9.4 and a security review of 0.10.0 led to these
changes:

- **`hw` checks the server's answers.** It refuses data about addresses and
  transactions it did not ask about. It checks each coin against the
  transaction it comes from. It reads no answer of more than 64 MiB.
- **`hw` asks before it follows a server that may lie.** `sync` and `recover`
  ask after a thousand used addresses in a row. `recover` asks after every 20
  paid accounts.
- **A transaction file cannot hide funds.** `hw` refuses a key of the account
  at any path but `/0/n` and `/1/n`. It refuses an output of the account more
  than a thousand addresses past the last one it knows.
- **Text from a file cannot rewrite the screen.** Labels and other text are
  shown with their control characters written out.
- **Secrets are never printed.** `hw` writes no core file. On Linux, no other
  program of the user can read its memory.
- **New wallets are private.** A new `~/.hw` is readable by its owner alone,
  and so is every file `hw` writes.
