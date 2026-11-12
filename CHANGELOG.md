# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

These changes will be released as version 0.10.0. Most of `hw` was rewritten
for this version. It replaces single accounts with wallets and portfolios, and
the commands work in one active portfolio, which `switch` changes. It adds
native SegWit addresses, backups in shares, PSBT files, coin control and fee
bumping. It does not read the database of an earlier version, and many
commands and options have new names, so read
[Before you upgrade](#before-you-upgrade) first.

### Before you upgrade

- **`hw` 0.10 does not read the database of 0.9.** It refuses a database of
  0.8 or 0.9, writes nothing to it, and says how to go on. `hw` 0.9 kept
  single accounts and no fingerprint of the master key, so only the mnemonic
  can make the whole portfolios of 0.10. Move the old database aside and
  create the wallet again from its mnemonic. On an online computer, `recover`
  finds the portfolios that were paid. To keep the mnemonic offline, run
  `newportfolio --existing` on the offline computer, once for each account
  index the old wallet used, from 0 up to the highest.
  Then carry the portfolios to the online computer with `exportwallet` and
  `importwallet`. The README tells the steps under
  [Upgrading from an earlier version](README.md#upgrading-from-an-earlier-version).
- **The labels of the old database are not carried over.** Nor are the names
  of its accounts and its pending transactions. The old file is left as it is,
  so `hw` 0.9 can still read it and show them.
- **Accounts become portfolios.** A portfolio holds every account of a
  mnemonic at one account index, on every network and of both types of
  address. An account of 0.9 comes back in the portfolio of its index.
  `label` with the description alone gives the active portfolio a
  description in place of the name of an account. No command names an account with `--account` any more. The
  commands work in the active portfolio, so a script that worked on one
  account runs `hw switch N` before its commands.
- **Transaction files are now PSBTs.** `hw` 0.10 reads no JSON transaction file
  of an earlier version. Prepare such a transaction again.
- **Account files are now wallet files.** `importwallet` refuses an account
  file that `exportacc` 0.9 wrote. Write the wallet again with `exportwallet`.
- **Scripts that call `hw` need checking.** Commands and options were renamed,
  and many answers changed. See [Renamed](#renamed) and [Removed](#removed).
- **A passphrase with accents is now normalized**, as BIP39 asks. `recover`
  still finds the accounts that earlier versions made from such a passphrase,
  and `signtx` signs for them, with a warning. In a new database,
  `newportfolio --existing` creates the wallet of the passphrase normalized,
  which does not hold those funds. Move their funds to a wallet made with 0.10, as the README says under
  [A passphrase with accents](README.md#a-passphrase-with-accents).
- **A mnemonic split into parts** with `--split` is no longer read. Move its
  funds with `hw` 0.9, as the README says under
  [A mnemonic split into parts](README.md#a-mnemonic-split-into-parts).
- **A directory `~/.hw` of an earlier version keeps its permissions.** Earlier
  versions often left it and the database readable by other users. `hw` 0.10
  makes its new database private. Make the rest private with `chmod 700 ~/.hw`
  and `chmod 600 ~/.hw/accounts-old.sqlite`.
- **The license is now MIT**, the license of haskoin-core. Released versions
  stay under the Unlicense.

### Added

Wallets and portfolios:

- Wallets and portfolios. `hw` keeps a wallet for each mnemonic, and
  portfolios in each wallet. A portfolio holds every account of the mnemonic
  at one account index: on Bitcoin, on Bitcoin Cash and on their test
  networks, with legacy and native SegWit addresses. With one mnemonic, you
  work in `portfolio0` of `wallet0` and never create or pick an account.
- The active portfolio. The commands work in one portfolio, which the
  database keeps. You always know where `hw` works, and never type it. The
  first portfolio created in a database becomes the active one. After that,
  only `switch` changes it.
- `switch N` makes portfolio N the active one, with `--wallet N` for a
  portfolio of another wallet. The portfolio goes without an option, since
  it is the one value `switch` needs: `hw switch 1`, or `hw switch
  portfolio1` by its name. `--portfolio N` names it too, for a script that
  names each value by its option. It refuses a wallet or a portfolio the
  database does not have, and needs no server. The next steps and refusals
  that send you to another portfolio write the switch as `hw switch 1
  --wallet 0`.
- `portfolios` marks the active portfolio with a `*`. Its JSON gives
  `"active"` for each portfolio, and its CSV file and the tab of the
  portfolios of `exportsheet` a column `Active`. `newportfolio`, `recover` and
  `importwallet` say which portfolio is active, and give the `switch` to a
  portfolio they created that is not.
- Two options pick an account in the active portfolio: `--network` and
  `--type`. Their defaults are `btc` and `bip44`. The keys `default-network`
  and `default-type` of the configuration change them, and `setdefault`
  writes those keys.
- `portfolios`, `pendingtxs`, `sync` and `exportsheet` cover every portfolio
  of every wallet. The commands given a transaction, a coin or an address find its
  portfolio by themselves, such as `show`, `importtx`, `signtx` and `label`.
  When several portfolios are candidates, they take the active one, or name
  them and ask for a `switch`. A transfer between two portfolios is kept and
  signed in the one that pays it.
- The details of an address, a coin or a transaction say when they are about
  another portfolio than the active one, in yellow on a terminal:
  `Portfolio: portfolio0, not the active portfolio1`. Their paths and labels
  are of that portfolio. `show`, `label`, `lockcoin`, `unlockcoin`,
  `importtx`, `signtx`, `broadcast`, `bumpfee` and `preparesweep` say it, and
  so does the transaction that `signtx` shows before it asks for the
  mnemonic. Their JSON gives no active portfolio.
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
- `portfolios --verbose` shows each account as an output descriptor (BIP380),
  and with its key in the form of SLIP132: a zpub for a `bip84` account, and
  the xpub of a `bip44` one. Its lines wrap on a terminal, so that a
  descriptor can be copied whole.
- `exportwallet` and `importwallet` use the BIP329 format. The file carries
  every portfolio of a wallet, with its labels and its locked coins.
  `exportwallet` writes the wallet of the active portfolio.
  `importwallet` creates each portfolio of the file that the database lacks,
  whole, and checks that the portfolios it has hold the keys of the file.
  `--portfolio` writes or reads one portfolio only. `importwallet` also sets the labels of a BIP329 file of another
  wallet. `show FILE` shows what `importwallet` would make of a file, and
  changes nothing. A transaction record without an origin labels the
  transaction only in the portfolios that have it, on its network.
- `label` with the description alone describes the active portfolio, or its
  wallet with `--of-wallet`. One command sets every label and description.
  `label` refuses a word alone that reads as an address, a path or a
  transaction id, since its label is most likely missing.
- `deletewallet` deletes a wallet with all of its portfolios. `--wallet` names
  the wallet, and its fingerprint confirms it. It refuses the wallet of the
  active portfolio while the database has another wallet.

Addresses:

- `receive` shows a QR code of the new address on the terminal.
- `show` shows one thing of the wallet, as the word typed tells: the address at
  a path such as `bip84/0/3`, an address, a transaction by its txid or
  nosigHash, a coin `TXID:INDEX`, or a file, any other word. A path takes the
  type of its account in front, as every view writes it. Without the type,
  as in `/0/3`, it is of the type `--type` picks. Without a word, it shows the
  newest receiving address. A file is a transaction file, as its portfolio
  sees it, or a file of a wallet, as `importwallet` would take it, with the
  labels of the file under each account.
  An address the wallet holds comes with its QR code, to give it out again.
  Run `show bip84/0/3` on the offline computer, with the path the online one
  gave out, to check the address: it derives the address from the public key.
  An address at a path the wallet does not hold is shown cut, its middle
  hidden behind dots, in JSON too. Its ends carry 128 bits, enough to compare,
  and nobody can pay it.
- `balances` lists the addresses that hold coins, grouped by network, with a
  total for each network.
- `label` labels transactions too, and the prepare commands take `--label`.
- `label` takes a path of the active portfolio, such as `bip84/0/3`, or `/0/3`
  with `--type`. The portfolio must have given out the address. On several
  networks, `--network` picks one. Without it, `label` takes the only one, or
  else the one on the default network.

Payments:

- A payment spends the coins of a portfolio on one network, legacy and native
  SegWit together.
- Coin control, with choosers and filters. The choosers, `--coin TXID:INDEX`
  and `--spend-from ADDRESS`, choose the coins of the active portfolio to
  spend, and they add up. `--select` lets the strategy select among them. The
  filters, `--min-conf` and the new `--coin-type TYPE`, narrow the coins down,
  every coin of the portfolio or the coins chosen. A filter never adds a coin.
  `--coin-type` keeps the coins of one type, `bip44` or `bip84`.
- `--spend-from` and `--change-back` take the path of an address of the
  active portfolio too, as the views write it, such as `bip44/1/0`. The path
  needs its type. It tells the networks where the portfolio has an address
  at it, as an address tells its network.
- `--strategy` sets how coins are selected. The default, `least-waste`, keeps
  the coins that waste the least fee.
- `lockcoin` and `unlockcoin` keep a coin from being spent. `coins --locked`
  lists the locked coins. They show the coin with every detail, then the
  account that holds it, as `show TXID:INDEX` does. Their JSON answer is of
  the type `coin`, with the `account` and the `coin`.
- `fees` shows the fee rates of recent blocks. `--fee-rate low`, `median` or
  `high` takes one of them. Without `--fee-rate`, the prepare commands ask.
  There is no default rate any more.
- `preparesendall` pays all the coins of a portfolio on a network. It asks
  about the coins worth less than their fee. `--spend-dust` and `--leave-dust`
  answer in advance.
- `bumpfee` replaces a stuck Bitcoin payment with one that pays a higher fee.
  The Bitcoin transactions `hw` builds are now marked replaceable (BIP125).
  `pendingtxs` names the transactions a replacement replaces in each of its
  views, the table included, in yellow on a terminal.
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

- `signmsg` signs a message in the Bitcoin Signed Message format. It takes
  the address, or its path in the active portfolio, such as `bip84/0/3`. It
  derives the address at a path from the public key, so the offline computer,
  which gives out no address, signs by path.
- `verifymsg` checks such a signature against its address.

Using `hw`:

- The help is laid out again, with a colour for each kind of thing to type.
  The commands that take a secret and need no server are marked.
- Listings show a compact view, two lines for each transaction or coin, and
  `--verbose` shows every detail. On a terminal they go through a pager, which
  keeps each line on one line of the screen, so that the columns line up.
- A coin looks the same wherever `hw` shows it. The questions of `bumpfee` and
  `preparesendall` and the coins a payment leaves show each coin as `coins`
  does, then what they say of it. The JSON of a coin a payment leaves has the
  keys of a coin of `coins --json`, and its reason.
- `addresses`, `balances`, `coins` and `txs` list a whole portfolio, in a
  section for each network, and for each type as well for `addresses`. A title
  line says where each section is. `--network` and `--type` narrow a listing
  down. `pendingtxs` lists the pending transactions of every portfolio of
  every wallet in one list, sorted by wallet, portfolio and network. Its views
  name the wallet of each transaction when the database has more than one, the
  table in a column of its own. After a `sync` that found a transaction in
  conflict, the next steps are `pendingtxs` and `deletetx`, without a
  `switch`.
- The compact views count the confirmations up to `999+ confirmations`.
  `--verbose` and the JSON answers give the full count.
- Every view writes the path of an address of the wallet with the type of its
  account in front, such as `bip84/0/3`. A portfolio has an address at `/0/3`
  in each of its accounts on a network, and `balances` and `coins` list both
  accounts of a network together. The JSON answers give the `"type"` of an
  address beside its path, and the CSV files a column `Type`.
- Transactions name the addresses of the wallet by their type and derivation
  path, and show them and their labels in blue. `--verbose` gives every
  address in full, with its path. `coins` gives the address of a coin in full,
  with its path.
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
- `exportsheet FILE` writes the six listings of every portfolio to a new
  OpenDocument spreadsheet, a tab each. Each cell keeps its kind: text, a
  number of satoshis, or a time in UTC. The header of each tab stays at the
  top as its rows scroll.
- Short ids: the first 8 digits of a hash name a transaction, a pending
  transaction or a coin (`a1075db5:1`) wherever the whole hash does. Each row
  of a table and each element of a compact view starts with it. `--verbose`
  gives the hash in full, then its short id, and `--csv` and `exportsheet` give
  both. When a short id matches two hashes of the wallet, `hw` names both and
  asks for the full hash.
  `preparesweep --coin` takes whole txids only. The JSON answers and the files
  keep whole hashes.
- `sync` and `recover` show what they are doing on a status line. It starts
  with the network whose server they are checking.
- Tab completion in bash, zsh and fish. It completes commands, options,
  wallets and portfolios, coins, addresses and pending transactions. A coin
  completes after the short id of its txid and a colon, as in `a1075db5:`.
  In bash too, which splits a word at a colon typed as it is: `hw` joins the
  word again.
- `hw --version` shows the version of `hw` and of its database format.
- `--unit btc|bit|sat` sets the unit of amounts. An amount can carry its own
  unit, as in `1000sat`.
- The key `unit` of the configuration sets the unit of the amounts of a
  command given no `--unit`. `setdefault --unit` writes it.
- The key `listing-view` of the configuration sets the view of the six
  listings given no `--compact`, `--verbose` or `--tabular`. It is `compact`,
  `verbose` or `tabular`. `--compact` shows the compact view whatever the key
  says. `setdefault` writes the key from `--compact`, `--verbose` or
  `--tabular`.

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
  When `hw` cannot reach the server at all, because the connection fails or
  times out or its TLS handshake fails, it checks no further network there.
  Those networks are named with the same error.
- `hw` reads the answers of haskoin-store 1.9.0, and those of older versions.
  Version 1.9.0 writes the number of raw transactions before them, and sends
  its errors in binary. With such a server, `sync`, `recover`, `preparesweep`
  and `bumpfee` failed with "gave an answer hw could not read", and every
  error of the server showed as "an error hw could not read", without its
  reason. The public server can run both versions at once, one on each
  network.
- A transaction is shown as its portfolio sees it: `Incoming` when funds enter
  the portfolio, `Outgoing` when they leave it, and `Internal` when they move
  between its addresses.
- `txs` and `coins` list by time. The unconfirmed transactions or coins come
  first, then the others, and each part lists the newest first, by the time
  shown. A coin shows the time of the transaction that paid it. A dead
  transaction is listed at its time among the others. Rows of the same time
  keep the order of their blocks. The exports of both follow the same order.
  The listings went by block alone before. A payment that waited in the
  mempool while later ones confirmed was then listed above them, and a
  transaction that died unconfirmed stayed at the top for good.
- Wallets and portfolios have fixed names that follow their index, such as
  `wallet0` and `portfolio0`. A description, set with `label`, takes the place
  of the name of an account.
- A wallet is the master key of one mnemonic, and `hw` records the
  fingerprint of that key. Output descriptors, wallet files and transaction
  files name every key of the wallet by it.
- `deleteportfolio` replaces `deleteacc` for a portfolio that was never used.
  `--portfolio` names a portfolio of the wallet of the active portfolio. It
  refuses `portfolio0`, the active portfolio, a portfolio that is not the last
  of its wallet, and one that has had activity or gave out an address.
  `deletewallet` deletes a whole wallet.
- `checkmnemonic` checks a mnemonic against every account of the active
  portfolio.
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
| `renameacc` | `label DESCRIPTION` |
| `deleteacc` | `deleteportfolio`, and `deletewallet` for a whole wallet |
| `exportacc`, `importacc` | `exportwallet`, `importwallet` |
| `--account NAME` | The active portfolio, which `switch N` changes, with `--wallet N` for another mnemonic. `--network` and `--type` pick an account in it. |
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

`label` now takes the address, or its path such as `bip44/0/3`, instead of its
index.

### Removed

- Account names. A portfolio is named after its index, and `label` describes
  it.
- `createacc --network` and `createacc --derivation`. A portfolio has an
  account on every network, and `newportfolio --existing` takes the lowest
  free index of the wallet.
- The import of a single account. `importwallet` reads whole wallets, as
  `exportwallet` writes them. An account file of 0.9, an extended public key
  or an output descriptor is one account. `importwallet` refuses a file that
  holds nothing else, and leaves aside such a key in a BIP329 file of another
  wallet.
- The upgrade of the database of an earlier version. `hw` 0.10 reads only a
  database of its own format, version 5. It refuses a database of 0.8 or 0.9,
  or a copy of one, and leaves it as it is.
- `restore`, and the backup files that `backup` 0.9 wrote. A backup is now a
  copy of the database. Put it in place of the database to restore it.
- `rolldice`. Roll real dice and type them in with `newportfolio --dice`.
- `--dust`. Each output takes the dust limit of its kind.
- `--split`, which split a mnemonic into parts and read them back. `hw` 0.10
  neither makes nor reads such parts. Use backups in shares.
- The key `recovery-gap` of the configuration, with `discoveracc`. Raise `gap`
  for one sync instead.
- The JSON transaction files of earlier versions.

### Fixed

- `sync` sees a replacement that leaves the balance of an address as it was.
  A fee bump of a payment to another portfolio pays the same amount to the
  same address, and the portfolio paid missed the replacement. It showed the
  replaced transaction as dead, without the one that took its place, and kept
  the coin of the replaced one. A replacement that spends the same coin with
  no change went missing the same way. `sync` looked only at the addresses
  whose balance changed. It now also looks every time at the addresses of the
  transactions not yet in a block.
- `sync` counts a transaction it finds replaced or dropped among the
  transactions updated. It said "nothing new" while `txs` showed the
  transaction dead.

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
- **An index of 2^31 or more is refused.** `--wallet` and `--portfolio` take
  an index below 2^31, the hardened steps of BIP32. A larger one wrapped
  around to a smaller one, and named another portfolio.
- **A transaction file cannot hide funds.** `hw` refuses a key of an account
  at any path but `/0/n` and `/1/n`. It refuses an output of an account more
  than a thousand addresses past the last one it knows.
- **Text from a file cannot rewrite the screen.** Labels and other text are
  shown with their control characters written out.
- **Secrets are never printed.** `hw` writes no core file. On Linux, no other
  program of the user can read its memory.
- **New wallets are private.** A new `~/.hw` is readable by its owner alone,
  and so is every file `hw` writes.
