# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

These changes will be released as version 0.10.0. Most of `hw` was rewritten
for this version. It replaces single accounts with wallets and portfolios. It
adds native SegWit addresses, backups in shares, PSBT files, coin control and
fee bumping. Many commands and options have new names, so read
[Before you upgrade](#before-you-upgrade) first.

### Before you upgrade

- **The database is upgraded on the first command.** `hw` 0.10 upgrades a
  database of 0.8 or 0.9, after it backs it up to `~/.hw/migration-backups`.
  Do not run an older `hw` on the upgraded database. `hw` 0.8 would rewrite
  parts of it in a format 0.10 cannot read.
- **Accounts become portfolios.** A portfolio holds every account of a
  mnemonic at one account index, on every network and of both types of
  address. The upgrade creates a portfolio for every account index the
  database uses, and keeps the gaps between them. The names of the accounts
  become the descriptions of their portfolios.
- **The portfolios of an upgraded database are partial.** The upgrade asks for
  no mnemonic, so a portfolio holds only the accounts 0.9 had. It works with
  them. A command that needs another account, such as `receive --type bip84`,
  is refused until the portfolio is filled in. `hw` 0.9 kept no fingerprint of
  the master key, so the accounts of one mnemonic on Bitcoin, on Bitcoin Cash
  and on the test networks also start in separate wallets.
- **`fillwallet` fills the portfolios in.** Run it on the computer that has
  the mnemonic, once for each mnemonic. It finds the wallets of the mnemonic
  by their keys, whatever their index. It adds the accounts the portfolios
  lack, and brings the wallets of one mnemonic into one. For an online
  computer that never sees the mnemonic, run `exportwallet` on the offline
  computer afterwards, and `importwallet` on the online one. Fill a wallet in
  before you export it. Until then its file names the keys without their
  origin, which another database cannot place, and `exportwallet` warns about
  it. The README tells the whole story under
  [Accounts become portfolios](README.md#accounts-become-portfolios).
- **Transaction files are now PSBTs.** `hw` 0.10 reads no JSON transaction file
  of an earlier version. Prepare such a transaction again.
- **Account files are now wallet files.** `importwallet` refuses an account
  file that `exportacc` 0.9 wrote. Write the wallet again with `exportwallet`.
- **Scripts that call `hw` need checking.** Commands and options were renamed,
  and many answers changed. See [Renamed](#renamed) and [Removed](#removed).
- **A passphrase with accents is now normalized**, as BIP39 asks. Accounts that
  earlier versions made from such a passphrase still work, with a warning.
  Move their funds to a wallet made with 0.10.
- **A mnemonic split into parts** with `--split` is no longer read. Move its
  funds with `hw` 0.9, as the README says under
  [A mnemonic split into parts](README.md#a-mnemonic-split-into-parts).
- **An existing database keeps its file permissions.** Earlier versions often
  left it readable by other users. Make it private with `chmod 700 ~/.hw` and
  `chmod 600 ~/.hw/accounts.sqlite`.
- **The license is now MIT**, the license of haskoin-core. Released versions
  stay under the Unlicense.

### Added

Wallets and portfolios:

- Wallets and portfolios. `hw` keeps a wallet for each mnemonic, and
  portfolios in each wallet. A portfolio holds every account of the mnemonic
  at one account index: on Bitcoin, on Bitcoin Cash and on their test
  networks, with legacy and native SegWit addresses. With one mnemonic, you
  work in `portfolio0` of `wallet0` and never create or pick an account.
- Four options say where a command works: `--wallet`, `--portfolio`,
  `--network` and `--type`. Their defaults are `wallet0`, `portfolio0`, `btc`
  and `bip44`. The keys `default-wallet`, `default-portfolio`,
  `default-network` and `default-type` of the configuration change them, and
  `setdefault` writes those keys.
- The commands that find the portfolio of a transaction by themselves, such as
  `show`, `importtx` and `signtx`, take `--wallet` and `--portfolio` to narrow
  the search only. `--wallet` alone looks in every portfolio of that wallet. A
  transfer between two portfolios is kept and signed in the one that pays it.
- Native SegWit addresses, as BIP84 derives them: `receive --type bip84`.
  Their payments pay about 40% less in fees. The networks of Bitcoin Cash have
  `bip44` addresses only. They take `bip44` whatever the default type says,
  and refuse `--type bip84`.
- Backups in shares, in the SLIP39 format:
  `newportfolio --shares N --threshold T`. Any T of the N shares rebuild the
  seed. Every command that asks for a mnemonic takes shares too.
- `newportfolio --existing` adds a portfolio to the wallet of a mnemonic you
  have, at the lowest free index.
- `recover` finds the portfolios of a mnemonic that were paid, on every
  network, creates them whole and syncs them. It also tells of funds at paths
  `hw` cannot spend from, such as wrapped SegWit and Taproot accounts.
- `fillwallet` fills in the partial portfolios of an upgraded database from
  the mnemonic of their wallet, and merges the wallets of one mnemonic. It
  finds those wallets by their keys, whatever their index. `--wallet` checks
  that the mnemonic is of that wallet.
- `portfolios --verbose` shows each account as an output descriptor (BIP380),
  and with its key in the form of SLIP132: a zpub for a `bip84` account, and
  the xpub of a `bip44` one. Its lines wrap on a terminal, so that a
  descriptor can be copied whole.
- `exportwallet` and `importwallet` use the BIP329 format. The file carries
  every portfolio of a wallet, with its labels and its locked coins.
  `importwallet` merges a portfolio of the file into the one the database has,
  and adds the accounts it lacks. `--portfolio` writes or reads one portfolio
  only. `importwallet` also sets the labels of a BIP329 file of another
  wallet. `show FILE` shows what `importwallet` would make of a file, and
  changes nothing. A transaction record without an origin labels the
  transaction only in the portfolios that have it, on its network.
- `setdescription` describes a portfolio, or a wallet with `--of-wallet`.
- `deletewallet` deletes a wallet with all of its portfolios. The fingerprint
  of the wallet confirms it. When it deletes the default wallet, it writes the
  new default to `default-wallet`, and sets `default-portfolio` back to 0 when
  the new default wallet does not have that portfolio.

Addresses:

- `receive` shows a QR code of the new address on the terminal.
- `show` shows one thing of the wallet, as the word typed tells: the address at
  a path such as `/0/3`, an address, a transaction by its txid or nosigHash, a
  coin `TXID:INDEX`, or a file, any other word. Without a word, it shows the
  newest receiving address. A file is a transaction file, as its portfolio
  sees it, or a file of a wallet, as `importwallet` would take it, with the
  labels of the file under each account.
  An address the wallet holds comes with its QR code, to give it out again.
  Run `show /0/3` on the offline computer to check an address the online one
  gives out: it derives the address from the public key. An address at a path
  the wallet does not hold is shown cut, its middle hidden behind dots, in JSON
  too. Its ends carry 128 bits, enough to compare, and nobody can pay it.
- `balances` lists the addresses that hold coins, grouped by network, with a
  total for each network.
- `label` labels transactions too, and the prepare commands take `--label`.

Payments:

- A payment spends the coins of a portfolio on one network, legacy and native
  SegWit together.
- Coin control. `--coin TXID:INDEX` and `--spend-from ADDRESS` choose the coins
  to spend. `--select` picks among them. `--coin-type TYPE` keeps the coins of
  one type, `bip44` or `bip84`.
- `--strategy` sets how coins are selected. The default, `least-waste`, keeps
  the coins that waste the least fee.
- `lockcoin` and `unlockcoin` keep a coin from being spent. `coins --locked`
  lists the locked coins.
- `fees` shows the fee rates of recent blocks. `--fee-rate low`, `median` or
  `high` takes one of them. Without `--fee-rate`, the prepare commands ask.
  There is no default rate any more.
- `preparesendall` pays all the coins of a portfolio on a network. It asks
  about the coins worth less than their fee. `--spend-dust` and `--leave-dust`
  answer in advance.
- `bumpfee` replaces a stuck Bitcoin payment with one that pays a higher fee.
  The Bitcoin transactions `hw` builds are now marked replaceable (BIP125).
- `preparetx --change-back ADDRESS` sends the change to an address of the
  portfolio, instead of a new change address. The address may have been used
  already, or be the change of another pending transaction.
- `preparetx --change-type TYPE` sets the type of the new change address.
  Without it, the change takes the type of the coins spent when they are all
  of one type, or else the default type.

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
  `--verbose` shows every detail. On a terminal they go through a pager, which
  keeps each line on one line of the screen, so that the columns line up.
- `addresses`, `balances`, `coins` and `txs` list a whole portfolio, in a
  section for each network, and for each type as well for `addresses`. A title
  line says where each section is. `--network` and `--type` narrow a listing
  down. `pendingtxs` lists the pending transactions of a portfolio in one list,
  sorted by network.
- The compact views count the confirmations up to `999+ confirmations`.
  `--verbose` and the JSON answers give the full count.
- Transactions name the addresses of the wallet by their type and derivation
  path, such as `bip84/0/3`, and show them and their labels in blue.
  `--verbose` gives every address in full. `coins` gives the address of a coin
  in full, with its path.
- `txs` shows the balance of the portfolio on the network after each
  transaction.
- `--tabular` shows each element of a listing on one row of a table, in
  columns under a header: `portfolios`, `addresses`, `balances`, `coins`, `txs`
  and `pendingtxs`. Nothing in a table is cut. `less` keeps the header of a
  table taller than the screen at the top, when the table has no section
  titles.
- `--csv FILE` writes a listing to a new CSV file, for a spreadsheet: every row,
  with the identifiers and amounts of the JSON answer in columns of their own.
  Amounts are in satoshis and times in UTC. A label that a spreadsheet would
  run as a formula gets an apostrophe in front.
- `exportsheet FILE` writes the six listings of every portfolio, or of those
  `--wallet` and `--portfolio` name, to a new OpenDocument spreadsheet, a tab
  each. Each cell keeps its kind: text, a number of satoshis, or a time in
  UTC. The header of each tab stays at the top as its rows scroll.
- Short ids: the first 8 digits of a hash name a transaction, a pending
  transaction or a coin (`a1075db5:1`) wherever the whole hash does. Each row
  of a table and each element of a compact view starts with it. `--verbose`
  gives the hash in full, then its short id, and `--csv` and `exportsheet` give
  both. When a short id matches two hashes of the wallet, `hw` names both and
  asks for the full hash.
  `preparesweep --coin` takes whole txids only. The JSON answers and the files
  keep whole hashes.
- `sync` and `recover` show what they are doing on a status line.
- Tab completion in bash, zsh and fish. It completes commands, options,
  wallets and portfolios, coins, addresses and pending transactions.
- `hw --version` shows the version of `hw` and of its database format.
- `--unit btc|bit|sat` sets the unit of amounts. An amount can carry its own
  unit, as in `1000sat`.
- The key `unit` of the configuration sets the unit of the amounts of a
  command given no `--unit`.
- The key `listing-view` of the configuration sets the view of the six
  listings given no `--compact`, `--verbose` or `--tabular`. It is `compact`,
  `verbose` or `tabular`. `--compact` shows the compact view whatever the key
  says.

### Changed

- `newportfolio` replaces `mnemonic` and `createacc`. It makes the mnemonic
  and creates its wallet in one step, with a whole portfolio. You no longer
  create an account for a network or a type of address.
- `newportfolio --existing` asks for the mnemonic and the passphrase twice. A
  typo can then not create a portfolio of another seed. Secrets are typed with
  no echo.
- `sync` replaces `syncacc` and `discoveracc`. It syncs every portfolio on
  every network, and the first sync of a portfolio finds its used addresses.
  It names the networks the server does not serve. When the server of a
  network is down, `sync` still syncs the other networks, names the one it
  left out, and exits with an error. `recover` does the same. A server that
  fails in the middle of the work is treated the same way: what `sync` wrote
  for the portfolio it was working on is undone, the syncs done before stay,
  and the other networks go on. `recover` keeps the portfolios it found.
- A transaction is shown as its portfolio sees it: `Incoming` when funds enter
  the portfolio, `Outgoing` when they leave it, and `Internal` when they move
  between its addresses.
- Wallets and portfolios have fixed names that follow their index, such as
  `wallet0` and `portfolio0`. A description, set with `setdescription`, takes
  the place of the name of an account.
- A wallet is the master key of one mnemonic, and `hw` records its
  fingerprint. A wallet of an upgraded database learns it from `fillwallet`
  or `importwallet`.
- `deleteportfolio` replaces `deleteacc` for a portfolio that was never used.
  It refuses `portfolio0`, a portfolio that is not the last of its wallet, and
  one that has had activity or gave out an address. `deletewallet` deletes a
  whole wallet.
- `checkmnemonic` checks a mnemonic against every account of a portfolio.
- `backup` copies the whole database, labels and pending transactions
  included. To restore it, put the copy in place of `~/.hw/accounts.sqlite`.
- `signtx` and `signsweep` show the transaction before they ask for the secret.
- Fees and sizes are in virtual bytes (sat/vB).
- New transactions are version 2, with a lock time near the current block
  height. Most wallets build theirs the same way.
- Each output takes the dust limit of its kind, as nodes relay it.
- `--min-conf` defaults to 0. Unconfirmed coins can be spent when the wallet
  made their transaction: its own change, and what one portfolio pays another.
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
| `mnemonic`, `createacc` | `newportfolio`, and `newportfolio --existing` for a mnemonic you have |
| `accounts` | `portfolios` |
| `renameacc` | `setdescription` |
| `deleteacc` | `deleteportfolio`, and `deletewallet` for a whole wallet |
| `exportacc`, `importacc` | `exportwallet`, `importwallet` |
| `--account NAME` | `--portfolio N` and `--wallet N`, with `--network` and `--type` for an account |
| `syncacc` | `sync` |
| `discoveracc` | `recover`, which now finds every paid portfolio |
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

- Account names. A portfolio is named after its index, and `setdescription`
  describes it.
- `createacc --network` and `createacc --derivation`. A portfolio has an
  account on every network, and `newportfolio --existing` takes the lowest
  free index of the wallet.
- The import of a single account. `importwallet` reads whole wallets, as
  `exportwallet` writes them. An account file of 0.9, an extended public key
  or an output descriptor is one account. `importwallet` refuses a file that
  holds nothing else, and leaves aside such a key in a BIP329 file of another
  wallet.
- `restore`. Put a backup in place of the database instead.
- `rolldice`. Roll real dice and type them in with `newportfolio --dice`.
- `--dust`. Each output takes the dust limit of its kind.
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
  paid portfolios.
- **A wallet file cannot give one key to two portfolios.** `importwallet`
  compares the keys themselves, not their text, which names an index a file
  can change. It refuses a key the database holds for another account, and
  accounts of one path, such as the bip44 accounts of the test networks, with
  different keys. It cannot prove that the keys of a file are those of your
  mnemonic: import only a file you trust, as the README says.
- **An index of 2^31 or more is refused.** `--wallet`, `--portfolio` and the
  keys `default-wallet` and `default-portfolio` take an index below 2^31, the
  hardened steps of BIP32. A larger one wrapped around to a smaller one, and
  named another portfolio.
- **A transaction file cannot hide funds.** `hw` refuses a key of an account
  at any path but `/0/n` and `/1/n`. It refuses an output of an account more
  than a thousand addresses past the last one it knows.
- **Text from a file cannot rewrite the screen.** Labels and other text are
  shown with their control characters written out.
- **Secrets are never printed.** `hw` writes no core file. On Linux, no other
  program of the user can read its memory.
- **New wallets are private.** A new `~/.hw` is readable by its owner alone,
  and so is every file `hw` writes.
