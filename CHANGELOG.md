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

- **`hw` 0.10 does not read the database of 0.9.** It keeps a new database,
  `~/.hw/portfolios.sqlite`, and leaves `~/.hw/accounts.sqlite` as it is.
  Create the wallet again from its mnemonic. `recover` finds the portfolios
  that were paid. The README gives the steps, for a mnemonic kept offline too,
  under [Upgrading from an earlier version](README.md#upgrading-from-an-earlier-version).
- **Labels, account names and pending transactions are not carried over.**
  `hw` 0.9 can still show them from the old file.
- **Accounts become portfolios.** A portfolio holds every account of a
  mnemonic at one account index, on every network and with both types of
  address. No command takes `--account` any more. A script that worked on one
  account runs `hw switch N` before its commands.
- **The files of 0.9 are not read.** Transaction files are now PSBTs, so
  prepare a pending transaction again. Account files are now wallet files, so
  write them again with `exportwallet`.
- **Scripts that call `hw` need checking.** Many commands and options were
  renamed, and many answers changed. See [Renamed](#renamed) and
  [Removed](#removed).
- **The settings are in a new file.** `hw` reads `~/.hw/config.toml` and no
  longer reads `~/.hw/config.json`. Run `hw` alone first, which sets the new
  file up. Then set again what you had changed, such as `host`. The README
  shows the new file under
  [The configuration file](README.md#the-configuration-file).
- **The test networks are hidden.** Show one before you use it, as in
  `hw setdefault --show-network btctest4`.
- **A passphrase with accents is now normalized**, as BIP39 asks. `recover`
  still finds the funds that earlier versions kept under such a passphrase,
  and `signtx` signs for them. Move these funds to a wallet made with 0.10, as
  the README says under
  [A passphrase with accents](README.md#a-passphrase-with-accents).
- **A mnemonic split into parts with `--split` is no longer read.** Move its
  funds with `hw` 0.9, as the README says under
  [A mnemonic split into parts](README.md#a-mnemonic-split-into-parts).
- **An old `~/.hw` keeps its permissions.** Earlier versions often left it
  readable by other users. Make it private with `chmod 700 ~/.hw` and
  `chmod 600 ~/.hw/accounts.sqlite`.
- **The license is now MIT**, the license of haskoin-core. Released versions
  stay under the Unlicense.

### Added

Wallets and portfolios:

- **Wallets and portfolios.** `hw` keeps a wallet for each mnemonic, and
  portfolios in each wallet. A portfolio holds every account of the mnemonic
  at one account index. It covers Bitcoin, Bitcoin Cash and their test
  networks, with legacy and native SegWit addresses. With one mnemonic, you
  work in `portfolio0` of `wallet0` and never create an account.
- **The active portfolio.** The commands work in one portfolio, which the
  database keeps. `switch` changes it, as in `hw switch 1 --wallet 0`.
  `--network` and `--type` pick an account in it. A command given a
  transaction, a coin or an address finds its portfolio by itself. A payment
  between two portfolios names the address of the other one by its path, as
  in `bip84/0/5 of portfolio1`.
- **Shown and hidden networks.** `hw` shows `btc` and `bch`, and leaves the
  six test networks out of every answer. `setdefault --show-network` and
  `--hide-network` change the list.
- **Native SegWit addresses**, as BIP84 derives them, with
  `receive --type bip84`. Their payments pay about 40% less in fees. Bitcoin
  Cash keeps `bip44` addresses only.
- **Backups in shares**, in the SLIP39 format, with
  `newportfolio --shares N --threshold T`. Any T of the N shares rebuild the
  seed. Every command that asks for a mnemonic takes shares too.
- **A master private key in place of the mnemonic.** Every command that asks
  for a mnemonic takes the master private key of the wallet too, as BIP32
  writes it, with no passphrase after it. A signer can then sign from a file
  that holds this key alone, as in `hw signtx NOSIGHASH < FILE`.
  `newportfolio --show-master-key` prints this key for a new mnemonic.
- **`recover`** finds the portfolios of a mnemonic that were paid, on every
  network, then creates and syncs them. It also tells of funds at paths `hw`
  cannot spend from, such as wrapped SegWit and Taproot accounts.
- **Output descriptors** (BIP380). `portfolios --verbose` shows each account
  as a descriptor, and with its zpub or xpub.
- **Wallet files in the BIP329 format.** `exportwallet` writes the wallets with
  their labels and locked coins. `importwallet` reads them back, and also
  takes the labels of a BIP329 file of another wallet.
- **Automatic backups.** `hw` copies the database into `~/.hw/auto-backups`
  after each command that adds a wallet or a portfolio. It copies the labels
  after each command that changes them. It copies both before
  `deletewallet`, `deleteportfolio` and `restoredatabase`. It keeps the
  copies by their age, for up to a year. `showbackups` tells how many there
  are and how old the newest is.
- **A damaged database.** When SQLite cannot read the database, every command
  names the newest backup and the command that restores it. `restoredatabase`
  moves the damaged file into `~/.hw/broken-databases` and puts the backup in
  its place.
- **`deletewallet`** deletes a wallet with all of its portfolios.
  `deletewallet` and `deleteportfolio` back up the database before they
  delete.
- **`label` describes a portfolio or a wallet, and labels transactions.** The
  prepare commands take `--label`.
- **A setup on the first run.** Plain `hw` asks which networks to show, the
  default network, the default type of address and whether to keep a journal
  for ledger, and writes the configuration file from the answers. A command
  run before it is refused.
- **The configuration file is now TOML**, so that it can hold comments. It
  lists every setting as a comment, with its values and its default.
  `setdefault` changes a value in place and keeps the rest of the file.
  `showdefault` shows the settings and the active portfolio.

Addresses:

- **`show`** shows one thing of the wallet. It takes the path of an address,
  an address, a transaction, a coin or a file. The file can be a transaction
  file, a wallet file or a copy of the database. On the offline computer,
  `show bip84/0/3` checks the address the online computer gave out.
- **QR codes.** `receive` and `show` show an address of the wallet as a QR
  code on the terminal.
- **`balances`** lists the addresses that hold coins, with a total for each
  network.
- **Coins that hold tokens are kept safe.** A coin of Bitcoin Cash that holds
  tokens (CashTokens) is listed and counted, and `hw` never spends it, in a
  payment or in a sweep: its tokens would be destroyed. Move it with a wallet
  that handles tokens, with the same mnemonic.
- **Tags.** `tag` puts one-word tags on addresses and transactions, such as
  `kyc` or `rent`, and `--color` gives a tag a colour. The listings show them,
  and keep the rows of a tag with `--tag` and `--address-tag`. `--untagged`
  keeps the rows without a tag, so `txs --untagged` lists what is left to
  sort. `untag`, `tags`, `renametag` and `deletetag` manage them.
  `exportwallet` and `importwallet` carry them.
  `--tag` on `send`, `sendall`, `preparetx` and `preparesendall` tags a
  payment as it is made.
  The statements of `report`, `exportjournal` and `exportsheet` take `--tag`
  and `--untagged` too, for a statement, a journal or a spreadsheet of one tag.
  `tag --with-change` also tags the change down the chain of the payments of
  an address, and `untag --with-change` takes the tags off it.
  `tag TAG --lock-coins` makes a tag lock its coins, those that arrive later
  too: only a payment with `--spend-from-tag TAG` spends them, so everyday
  payments never take from savings. `tag TAG --unlock-coins` frees them.
  A payment that would spend coins of different tags together, such as `kyc`
  and `p2p`, names the groups and asks first. `--yes` answers for a script.
- **An address book.** `newcontact` keeps an address of somebody else with a
  label and tags, and `contacts` lists them, each address in one piece and in
  groups of four characters. A transaction shows such an address underlined,
  with its label, and the Tab key writes it where a payment takes a recipient.
  `label`, `tag`, `untag` and `show` take a contact by its address, and
  `txs --address-tag` lists the transactions at the contacts of a tag.
  `exportwallet`, `importwallet` and `exportsheet` carry the address book.

Payments:

- **`send` and `sendall`** prepare, sign and send a payment in one command.
  They need the mnemonic on the online computer. `preparetx`, `signtx` and
  `broadcast` stay the way to pay from cold storage.
- **`sweep`** prepares, signs and sends a sweep in one command. It reads the
  private keys on the online computer. `preparesweep`, `signsweep` and
  `broadcast` stay the way to sweep with the keys kept offline.
- **A payment sent is in the wallet at once.** `send`, `sendall`, `sweep` and
  `broadcast` sync the portfolios of the transaction once it is sent. Its
  change is there for the next payment, with no `sync` in between.
- **`preparesendall`** pays all the coins of a portfolio on a network. It asks
  about the coins worth less than their fee.
- **Coin control.** `--coin` and `--spend-from` choose the coins to spend, and
  `--spend-from-tag` the coins at every address that has a tag.
  `--min-conf` and `--coin-type` narrow them down. `lockcoin` and
  `unlockcoin` keep a coin from being spent. `signtx` and `broadcast` refuse
  a transaction that spends a locked coin, of `lockcoin` or of a tag.
- **Coin selection strategies**, with `--strategy`. The default,
  `least-waste`, keeps the coins that waste the least fee.
- **Fee rates of the network.** `fees` shows the rates of recent blocks, and
  `--fee-rate low`, `median` or `high` takes one of them. Without
  `--fee-rate`, the prepare commands ask. There is no default rate any more.
- **Fee bumping.** `bumpfee` replaces a stuck Bitcoin payment with one that
  pays a higher fee. The Bitcoin transactions `hw` builds are now marked
  replaceable (BIP125). `txs` and `show` name the transaction that replaced a
  dead one. A pending replacement locks the coins of the payment it replaces.
- **Where the change goes.** `--change-back ADDRESS` sends it to an address of
  the portfolio, and `--change-back largest` or `smallest` to the address of
  the largest or the smallest coin spent. `--change-type` sets the type of a
  new change address. `--tag-change` gives a new change address the tags that
  every coin spent has, so the change of a tagged group stays in it.
- **PSBT files** (BIP174). A hardware wallet or another wallet can sign them in
  place of the offline computer. `hw` reads, signs and sends the PSBTs of
  other wallets too.
- **Signed messages.** `signmsg` signs a message in the Bitcoin Signed Message
  format, and `verifymsg` checks it. The offline computer signs by path, such
  as `bip84/0/3`.

Using `hw`:

- **Listings in three views.** The compact view gives two lines for each
  transaction or coin, `--verbose` every detail, and `--tabular` a table. On a
  terminal they go through a pager. `addresses`, `balances`, `coins` and `txs`
  list a whole portfolio, in a section for each network. `txs` shows the
  balance after each transaction. `--reverse` lists the rows in the reverse
  order. The keys `listing-view` and `listing-reverse` set the defaults.
- **Addresses of the wallet are named by their path**, such as `bip84/0/3`.
  Transactions show them and their labels in blue.
- **Spreadsheet exports.** `--csv FILE` writes a listing to a CSV file.
  `exportsheet FILE` writes the listings of every portfolio to an OpenDocument
  spreadsheet, a tab each.
- **A journal for ledger.** `exportjournal FILE` writes the transactions of
  every portfolio to a journal of double-entry accounting, in the format of
  the ledger command line tool. A portfolio is an account, a fee is an
  expense, and a payment between two portfolios is one entry. With
  `setdefault --journal`, `hw` keeps that journal in `~/.hw/portfolios.ledger`
  and writes it again after each command that changes it. A tag can stand for
  an account: the table `journal-accounts` of the configuration file files
  the payments with a tag, and those to or from a contact with it, under an
  account such as `Expenses:Housing:Rent`.
- **PDF reports.** `report FILE --balances` writes a balance statement to a
  PDF file, for a tax office or an accountant: the balance of each asset, of
  each portfolio and of each address that holds coins. `--date YYYY-MM-DD`
  gives the balances at the end of a day in the past, such as the last day of
  a year. `report FILE --transactions` writes an account statement, as a bank
  sends one: the transactions of each portfolio over the month that is
  running, or from `--from` to `--to`, with the balance after each, between an
  opening and a closing balance. `report FILE --payment TXID` writes a payment
  confirmation, for the person you paid or for your books: what one
  transaction paid, to which addresses and on which day, with its block.
  A payment you received gets a receipt. `--wallet` and `--portfolio` narrow a
  report down to one wallet or to one portfolio. `--holder` names the holder,
  and `--price` with `--currency` adds values at prices you give. The file
  carries its fonts.
- **Short ids.** The first 8 digits of a hash name a transaction or a coin, as
  in `a1075db5:1`, wherever the whole hash does.
- **Tab completion** in bash, zsh and fish. It completes commands, options,
  wallets, portfolios, coins, addresses, tags and pending transactions.
- **Units of amounts.** `--unit btc|bit|sat` sets the unit, and the key `unit`
  its default. An amount can carry its own unit, as in `1000sat`.
- **`sync` and `recover` tell more.** A status line shows what they are doing.
  They say what is wrong with a server that is not healthy. `sync` names each
  transaction the server did not publish again, and gives the `deletetx` that
  frees its coins.
- **A new help**, with a colour for each kind of thing to type. A help longer
  than the screen goes through a pager.
- **A logo.** The name `hw` as its bits, a row of eight squares for each
  letter. The help of `hw` starts with it, and each PDF report has it at the
  top right of its first page.

### Changed

- **`newportfolio` replaces `mnemonic` and `createacc`.** It makes the
  mnemonic and a whole portfolio in one step. `newportfolio --existing` asks
  for the mnemonic and the passphrase twice, so a typo cannot create a
  portfolio of another seed. It warns when the mnemonic is of no wallet of a
  database that has one, and with `--match` it refuses such a mnemonic.
- **`sync` replaces `syncacc` and `discoveracc`.** It syncs every portfolio on
  every network, and the first sync of a portfolio finds its used addresses.
  It looks 20 addresses past the last used one, as a restored wallet does, so
  two computers with the same mnemonic find the same payments. When the server
  of one network is down, `sync` still syncs the others, then exits with an
  error.
- **`hw` needs a haskoin-store of version 2.0.0.** It supports no older
  server. It talks to the server in JSON, the one format that the server
  answers in, so `hw` 0.9 does not work with such a server.
- **`hw` explains what the server answered.** It shows the errors of the
  server, and why `broadcast` could not publish a transaction. It said only
  the name of the error before.
- **A transaction is shown as its portfolio sees it**, as `Incoming`,
  `Outgoing` or `Internal`.
- **`txs` and `coins` list in the order of the chain**, the newest first. The
  unconfirmed transactions or coins come first.
- **Transactions are built as most wallets build them.** They are version 2,
  with a lock time near the current block height. Fees and sizes are in
  virtual bytes (sat/vB), and a fee rate that is not a whole number shows one
  decimal. Each output takes the dust limit of its kind of address.
- **`--min-conf` defaults to 0.** An unconfirmed coin can be spent when the
  wallet made its transaction. A payment from someone else waits for a
  confirmation.
- **The commands that delete ask first.** `deleteportfolio`, `deletewallet`,
  `deletetx`, `deletetag` and `deletecontact` say what is about to go, and
  delete only after a yes. `--yes` answers in advance, for a script.
- **`hw` asks before it takes a fee rate above 100 sat/vB.** It refuses a
  transaction without a fee.
- **`preparesweep` asks about the coins worth less than their fee**, as
  `preparesendall` does. It swept them without a word, and each one lowered
  what the sweep paid.
- **`signtx` and `signsweep` show the transaction before they ask for the
  secret**, in full: every address it spends from and every address it pays,
  each with its amount.
- **The private keys of a sweep take several lines.** `signsweep` and
  `preparesweep --key-file -` read a list of keys pasted on a terminal, and a
  key file on a pipe. They read one line before.
- **`backup` copies the whole database**, labels and pending transactions
  included. Without a file, it writes the copy into `~/.hw/user-backups`,
  where `hw` removes nothing.
- **`restoredatabase FILE` replaces `restore`.** It puts a copy of the
  database back, whole, once you confirm. It backs up the database it
  replaces first, so a restore can be undone.
- **`hw` runs one command at a time on a database.** A second command waits
  three seconds, then stops.
- **`hw` never overwrites a file**, and a crash leaves no half-written file.
- **New mnemonics take their entropy from `/dev/random`** where the system has
  one. `hw` refuses entropy with a visible pattern.

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
| The network `bchtest`, the testnet3 of Bitcoin Cash | `bchtest3`, as haskoin-core and haskoin-store name it |

`label` now takes the address, or its path such as `bip44/0/3`, instead of its
index.

### Deprecated

- **The testnet3 of Bitcoin, `btctest`.** It still works. Bitcoin Core has
  deprecated testnet3, and haskoin-core will remove the network. `hw` will
  remove it then. Test on `btctest4` instead.

### Removed

- `restore`, and the backup files that `backup` 0.9 wrote. A backup is now a
  copy of the database, and `restoredatabase` puts it back.
- `rolldice`. Roll real dice and type them in with `newportfolio --dice`.
- `--split`. Use backups in shares instead.
- `--dust`. Each output takes the dust limit of its kind.
- `createacc --network` and `createacc --derivation`. A portfolio has an
  account on every network.
- The import of a single account, such as an extended public key or an output
  descriptor. `importwallet` reads whole wallets, as `exportwallet` writes
  them.
- The key `database-file` of the configuration. The database is always in
  `~/.hw`. To keep it on another disk, make `~/.hw` a link to a directory
  there.
- The key `recovery-gap` of the configuration. Raise `gap` for one sync
  instead.

### Fixed

- **A `host` without a slash at the end now works.** Every call to a server
  such as `http://localhost:3000` failed before.
- **`sync` sees a replacement that pays the same amount to the same address.**
  It showed the replaced transaction as dead, and kept its coin.
- **A sweep of a coin of an uncompressed key pays the fee rate asked.** Its
  fee came out too low, and a node could refuse the sweep.

### Security

An independent review of 0.9.4 and a security review of 0.10.0 led to these
changes:

- **`hw` checks the server's answers.** It refuses data about addresses and
  transactions it did not ask about. It checks each coin against the
  transaction it comes from. It reads no answer of more than 64 MiB.
- **`hw` asks before it follows a server that may lie.** `sync` and `recover`
  ask after a thousand used addresses in a row. `recover` asks after every 20
  paid portfolios.
- **`sync` refuses a server on another chain.** It keeps the block a hundred
  below the one it reached, and refuses a server that does not have it: a
  server far behind the wallet, or another one than before. Its answers would
  show every payment as dropped. `sync --new-chain` follows it, as after a
  test network was reset.
- **A wallet file cannot give one key to two portfolios.** `importwallet`
  refuses a key the database holds for another account. It cannot prove that
  the keys of a file are those of your mnemonic, so import only a file you
  trust.
- **An index of 2^31 or more is refused.** Such an index wrapped around to a
  smaller one, and named another portfolio.
- **A transaction file cannot hide funds.** `hw` refuses a key of an account
  at any path but `/0/n` and `/1/n`. It refuses an output more than a
  thousand addresses past the last one it knows.
- **Text from a file cannot rewrite the screen.** Labels and other text are
  shown with their control characters written out.
- **Secrets are never printed.** `hw` writes no core file. On Linux, no other
  program of the user can read its memory.
- **A secret pasted with a line too many does not reach the shell.** `hw`
  discards what was typed ahead of a secret and not read. The shell showed
  such a line, ran it as a command and kept it in its history.
- **New wallets are private.** A new `~/.hw` is readable by its owner alone,
  and so is every file `hw` writes.
