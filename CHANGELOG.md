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

- **`hw` 0.10 does not read the database of 0.9.** It keeps its database in a
  new file, `~/.hw/portfolios.sqlite`, and never opens `~/.hw/accounts.sqlite`,
  the database of 0.8 and 0.9. The old file stays as it is, and `hw` 0.10
  starts with no wallet. A database of 0.8 or 0.9 that `hw` is made to open,
  under the new name or through `database-file`, is refused, with how to go
  on. `hw` 0.9 kept single accounts and no fingerprint of the master key, so
  only the mnemonic can make the whole portfolios of 0.10. Create the wallet
  again from its mnemonic. On an online computer, `recover`
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
- **The test networks are hidden.** `hw` shows `btc` and `bch`, and refuses
  `--network` with another network. To work on a test network, show it first:
  `hw setdefault --show-network btctest`.
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
  and `chmod 600 ~/.hw/accounts.sqlite`.
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
- Shown and hidden networks. A portfolio has accounts on eight networks, and
  `hw` shows `btc` and `bch` in a new configuration. The six test networks
  are hidden. A hidden network is left out of every answer: the listings,
  `portfolios`, `exportsheet`, tab completion, and what `sync` and `recover`
  say once done. `--network` cannot name it, and the refusal gives the
  command that shows it. The same holds for an address, a coin or a
  transaction that the wallet has on hidden networks only. A hidden network
  is not disabled: `sync` and `recover` still work on it, and their status
  line names it. They do not report a hidden network whose server is down,
  and do not exit with an error for it.
- The key `shown-networks` of the configuration lists the networks `hw`
  shows. `setdefault --show-network NET` and `setdefault --hide-network NET`
  change it. The default network is always shown: `setdefault` refuses to
  hide it, and `hw` does not start with a file whose `default-network` is
  not in `shown-networks`.
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
- A path alone, such as `bip84/0/3`, is always of the active portfolio. A
  path of another portfolio names it, as in `bip84/0/3 of portfolio1`. A
  payment between two portfolios shows the address of the other portfolio as
  an address of the wallet: in blue, with its path, its portfolio and its
  label there, in place of a stranger's address. The short views write
  `to bip84/0/5 of portfolio1`. In an answer about another portfolio than the
  active one, every path names its portfolio. This covers the details of a
  transaction, a coin and an address, the transaction `signtx` and `broadcast`
  show before they ask, the coins `bumpfee` and a prepared transaction report,
  and `pendingtxs`. In JSON, a transaction lists these addresses under
  `otherportfolios`. `hw` knows an address of a portfolio once that portfolio
  gave it out, in the same database.
- A view names a portfolio alone when the database has one wallet, as in
  `portfolio1`. With several wallets it names the wallet of every portfolio,
  `wallet0` too, as in `portfolio1 of wallet0`. This holds wherever a view
  names a portfolio: the `Portfolio` and `Account` fields, the note about the
  active portfolio, the titles of the listings, the portfolio after a path,
  the answer of `sync` and `recover`, and their status line. A payment
  between two wallets then tells the wallet of each address, as in
  `bip44/1/0 of portfolio1 of wallet0`. `portfolios` and the messages in a
  sentence always name the wallet. `show FILE` of a file of a wallet names
  the wallet of each account when the file holds several wallets.
- Native SegWit addresses, as BIP84 derives them: `receive --type bip84`.
  Their payments pay about 40% less in fees. The networks of Bitcoin Cash have
  `bip44` addresses only. They take `bip44` whatever the default type says,
  and refuse `--type bip84`.
- Backups in shares, in the SLIP39 format:
  `newportfolio --shares N --threshold T`. Any T of the N shares rebuild the
  seed. Every command that asks for a mnemonic takes shares too.
- `newportfolio --existing` adds a portfolio to the wallet of a mnemonic you
  have, at the lowest free index.
- `newportfolio --one-line` prints the words of the new mnemonic on one line,
  without numbers, to copy them into another program. Each share of a backup
  gets a line of its own. Without it, the words come numbered, four on a line,
  to write them down. `--existing` refuses it, as it refuses the other options
  of a new mnemonic.
- `newportfolio` names the accounts of the new portfolio on the networks it
  shows, each network with the types of its accounts:
  `3, on btc (bip44 and bip84) and bch (bip44)`. The count then adds up on
  the line. The portfolio also has the accounts of the hidden networks.
- `recover` finds the portfolios of a mnemonic that were paid, on every
  network, creates them whole and syncs them. It also tells of funds at paths
  `hw` cannot spend from, such as wrapped SegWit and Taproot accounts.
- `portfolios --verbose` shows each account as an output descriptor (BIP380),
  and with its key in the form of SLIP132: a zpub for a `bip84` account, and
  the xpub of a `bip44` one. Its lines wrap on a terminal, so that a
  descriptor can be copied whole.
- `exportwallet` and `importwallet` use the BIP329 format. `exportwallet`
  writes every portfolio of every wallet to one file, with its labels and its
  locked coins, whichever portfolio is active. `--wallet` writes one wallet
  only. `--portfolio` writes one portfolio only, of the wallet `--wallet`
  names, or else of the wallet of the active portfolio.
  `importwallet` finds each wallet of the file by the fingerprint of its
  mnemonic. The file does not carry the index of a wallet. A wallet of the
  file may have another index in the database, as after `recover` took the
  mnemonics in another order, and its labels still go to it.
  `importwallet` creates each portfolio of the file that the database lacks,
  whole, and checks that the portfolios it has hold the keys of the file.
  `importwallet --wallet` reads one wallet of the file only, and
  `--portfolio` one portfolio only. `--wallet` takes the fingerprint of the
  wallet there, such as `bc560f95`, since a file holds no index. In a file of
  several wallets, `--portfolio` alone is refused, and `importwallet` asks for
  the wallet too. `show FILE` takes the same two options. The step that tells
  to import a file again names the wallet and the portfolio of the first
  import. `importwallet` also sets the labels of a BIP329 file of another
  wallet. `show FILE` shows what `importwallet` would make of a file, and
  changes nothing. A transaction record without an origin labels the
  transaction only in the portfolios that have it, on its network.
  The answer of `importwallet` counts the labels it set on wallets,
  portfolios, addresses and transactions. The label of a wallet or a
  portfolio is its description. A wallet or a portfolio that keeps a
  description of its own is counted under `Left aside`.
- `hw` backs up the database and the labels by itself, into
  `~/.hw/auto-backups`, and keeps the ten newest copies of each. It copies
  the database after each command that adds or deletes a wallet or a
  portfolio. It copies the labels after each command that changes the
  wallets, their descriptions, the labels or the locked coins, in the file
  `exportwallet` writes without options. `importwallet` reads such a copy
  back after the database is lost. The name of a copy gives its time and how
  many wallets and portfolios it holds, and for the database its format
  version too. `hw` removes the older copies it made there, and leaves alone
  every other file of the directory. The copies are on by default. The keys
  `autobackup-database` and `autobackup-labels` of the configuration turn
  them off, and the key `autobackup-copies` sets how many of each `hw` keeps.
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
- `send ADDRESS:AMOUNT...` makes a payment in one command. It prepares the
  transaction as `preparetx` does, shows it, asks for the mnemonic, signs as
  `signtx` does, and sends as `broadcast` does. It takes the options of
  `preparetx`, all but `--output`, and makes the same payment from them. The
  transaction is shown once, before the mnemonic, and typing the mnemonic is
  the confirmation: `send` asks nothing more, and answers with the txid.
  A payment that is not signed leaves nothing in the wallet. A transaction
  that is signed and cannot be sent stays pending, as `signtx` leaves one, and
  the failure names it for `broadcast` and `deletetx`. `send` needs the
  mnemonic on the online computer, so the three commands stay the way to pay
  from cold storage.
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
  `--yes` takes the change, else confirmed coins of the portfolio, else the
  recipients, in equal shares when there are several. With
  `--recipients-pay`, the recipients pay what the higher fee adds, and the
  change keeps its amount.
  `pendingtxs` names the transactions a replacement replaces in each of its
  views, the table included, in yellow on a terminal.
- `txs` and `show TXID` name the transaction that replaced a dead one, as in
  `replaced by 2b69e4f4`. The note is on the second line of the compact view,
  first in the description of the table, and in full in the `Status` field of
  the verbose view. `txs --json` gives the txids under `replacedby`. The
  replacement is the live transaction that spends a coin the dead one spent,
  so the note is never stored and follows the network. When a miner confirms
  the stuck payment after all, the replacement turns dead and names that
  payment. A payment that spent the change of a replaced payment names the
  same replacement. A dead payment received names none.
- The verbose view says `dropped from the network` for a dead payment of the
  portfolio that nothing replaced. Any other dead transaction still says
  `replaced, or dropped from the network`.
- `bumpfee` with the txid of a dead payment names the transaction that
  replaced it, as the one to bump instead.
- `preparetx --change-back ADDRESS` sends the change to an address of the
  portfolio, instead of a new change address. The address may have been used
  already, or be the change of another pending transaction.
- `preparetx --change-back largest` sends the change back to the address of
  the largest coin the payment spends. You cannot know in advance which coins
  the strategy selects, so this names an address the payment spends from
  without knowing it. The coin is the largest of the type of the change, so
  the fee is counted for the right output. `--change-type` sets that type,
  and a payment that spends no coin of it is refused. Of coins of equal
  value, the one at the lower path wins. `largest` is a word after the option,
  not a bare `--change-back`, since the option would take the next word on
  the line as its address.
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
- `verifymsg` checks such a signature against its address. It reads the
  address on the network it is written for, so it needs no `--network`.

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
  with every identifier and amount in a column of its own. Amounts are in
  satoshis. A label that a spreadsheet would run as a formula gets an
  apostrophe in front.
- `exportsheet FILE` writes the six listings of every portfolio to a new
  OpenDocument spreadsheet, a tab each. Each cell keeps its kind: text, a
  number of satoshis, or a time. The header of each tab stays at the top as
  its rows scroll.
- The CSV file of a listing and its tab in the spreadsheet have the same
  columns, in the same order, so rows copied from one fit the other. Every
  listing starts with `Wallet`, `Portfolio`, `Network` and `Type`, goes on with
  what it lists, and ends with the date `hw` created the element when it has
  one. Two listings give the columns they share under the same headings, in
  the same order.
- Each time of a CSV file or of the spreadsheet takes three columns: in UTC,
  on the clock of the computer, and the offset of that clock from UTC, as in
  `UTC+01:00`.
- The CSV file and the tab of `txs` and of `pendingtxs` name three addresses of
  each transaction, each with its path, label and value: the one it spends the
  most from, the one it pays the most, and its change. `Other addresses` counts
  the rest. `Replaced by` names what replaced a dead transaction, and
  `Replaces` what a replacement of `bumpfee` replaces.
- The spreadsheet of `exportsheet` has two more tabs, `Transaction addresses`
  and `Pending transaction addresses`. They have a row for every address of
  every transaction: its role, `from`, `to` or `change`, its value, the
  address, its path, chain and label, and the portfolio that holds it. Each
  row repeats the txid, the time, the direction, the status and the label of
  its transaction.
- Short ids: the first 8 digits of a hash name a transaction, a pending
  transaction or a coin (`a1075db5:1`) wherever the whole hash does. Each row
  of a table and each element of a compact view starts with it. `--verbose`
  gives the hash in full, then its short id, and `--csv` and `exportsheet` give
  both. When a short id matches two hashes of the wallet, `hw` names both and
  asks for the full hash.
  `preparesweep --coin` takes whole txids only. The JSON answers and the files
  keep whole hashes.
- `sync` and `recover` show what they are doing on a status line. It starts
  with the network whose server they are checking. The line fits the width of
  the terminal. On a terminal of less than 80 columns it wrapped, and each
  step then left a line behind it on the screen.
- `sync` names each transaction it sent again that the server did not
  publish, with what the server said, and goes on. Its next step is the
  `deletetx` that frees the coins of that transaction, by its short id. The
  JSON answer lists them under `"notpublished"`, each with its `"txid"` and
  the `"reason"`. A haskoin-store says so from version 1.11, for a
  transaction whose coin is spent or gone, or whose fee the network no
  longer relays.
- `sync` and `recover` say what a server that is not well reports as wrong:
  how far its index is behind its chain, that it has no peer or too few, how
  many transactions wait to be indexed, or how old its last block and its
  last transaction are. `hw` said only that the health check had failed.
- `preparetx` and `preparesendall` say when the portfolio holds coins it
  cannot spend now, with their count and their sum, when they refuse a
  payment for lack of coins. `hw coins` says why each is held back. The
  refusal read as if the portfolio held nothing more.
- Tests that run `hw` against real nodes and real servers. A podman image
  holds Bitcoin Core, Bitcoin Cash Node and two haskoin-store 1.12.1
  servers, on regtest chains of both currencies. `regtest/build.sh services`
  builds it, from Ubuntu Base and the releases of the nodes, each checked
  against its signatures. The tests of `Haskoin.Wallet.Regtest` start one
  container each, and cover what the fake store of the other tests cannot
  show: payments and coins through reorganizations, replacements and rival
  payments, what the server answers about a transaction no node takes, and
  servers that stop in the middle of the work. They are skipped, with the
  reason, when podman or the image is missing, or when the image was built
  from an older recipe. See `regtest/README.md`.
- Tab completion in bash, zsh and fish. It completes commands, options,
  wallets and portfolios, coins, addresses and pending transactions. A hash
  completes to its short id, and a coin to the short id of its txid, its
  colon and its index, as in `a1075db5:1`. A ninth digit typed completes the
  whole hash. Two hashes that share a short id complete in full, since the
  commands refuse it. A coin completes after the short id of its txid and a
  colon, as in `a1075db5:`. In bash too, which splits a word at a colon typed
  as it is: `hw` joins the word again. A file name completes after `~/` from
  the home directory, and keeps its `~/`.
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
- `hw` talks to the server in JSON. haskoin-store answers in JSON alone from
  version 1.10. It no longer gives the binary answers that `hw` 0.9 asks for,
  so `hw` 0.9 does not work with such a server. Every older haskoin-store
  answers in JSON too when asked. So `hw` 0.10 works with every version, and
  with a public server that runs several versions at once, one on each
  network.
- `hw` says what the server answered when it answers with an error. An error
  `hw` does not know is shown with its name and its message. A newer
  haskoin-store may answer one, and the public server answers `Bad Gateway`
  for a network whose server is down. An answer that is no error of
  haskoin-store is shown with its HTTP status: 502 for the page of a proxy in
  front of a server that is down, 429 for a server that limits the requests.
  An error that arrives in several pieces is read whole, up to 64 KiB.
- `hw` builds against haskoin-store-data 2.0.1, and reads the answers of the
  server as strictly as that version does. An answer with a time that is no
  time, or an address that is neither an address nor `null`, is refused as an
  answer `hw` could not read.
- A transaction is shown as its portfolio sees it: `Incoming` when funds enter
  the portfolio, `Outgoing` when they leave it, and `Internal` when they move
  between its addresses.
- `txs` and `coins` list in the order of the chain, the newest first. The
  unconfirmed transactions or coins come first, then the others by their
  block and their place in it. The balance of the portfolio changed in that
  order, so the running balance of `txs` shows only balances the portfolio
  held, and the balance beside a confirmed transaction stays as it is. The
  times shown can run a little out of order, since a block can be dated before
  the block under it. A coin shows the time of the transaction that paid it.
  The exports of both follow the same order.
- `txs` lists an unconfirmed transaction above the ones it spends from, even
  when the server saw them in the same second. On Bitcoin Cash, whose blocks
  are sorted by transaction id, it does the same inside a block.
- A dead transaction stays where it was when the wallet last saw it alive. One
  that was confirmed keeps the place of its block. One that died unconfirmed
  stays above the blocks the wallet knew then, and goes down the list as
  later blocks come. It stayed at the top for good before.
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
  portfolio. It exits with an error when they do not match, as `verifymsg`
  does for a signature that is not valid, so that a script can tell.
- `backup` copies the whole database, labels and pending transactions
  included. To restore it, put the copy in place of `~/.hw/portfolios.sqlite`.
  Without a file, it writes the copy into `~/.hw/user-backups`, with the
  labels beside it in the file `exportwallet` writes, and `hw` never removes
  them. The name of both files gives the format version, the time, and how
  many wallets and portfolios the database holds.
- `hw` keeps its connections to the server open for the length of a command.
  It opened a new one for every request before, and each one read the
  certificates of the system and did the TLS handshake again. A `sync` or a
  `recover` sends about a hundred requests, and no longer pays that for each.
- `broadcast` asks the server for the transaction as soon as it has sent it,
  then every second for ten seconds. It waited a second before the first
  check, so every `broadcast` took a second at least.
- `broadcast` says in words what a haskoin-store of version 1.11 or later
  answers about a transaction it did not publish. Such a server answers only
  once a node has announced the transaction back. `hw` gives its reason when
  the server refused the transaction. It says that the fee may be too low
  when no node announced the transaction in the time the server waits. It
  says that the server could not hand the transaction to a node when it
  reached none. The transaction stays signed and not sent in each case. `hw`
  showed the name of the error before, such as `ServerTimeout`.
- `sync` shows no next step when it found no new transaction. It pointed to
  `txs` after every sync.
- `preparetx` says what a recipient would get, and the dust limit of its kind
  of address, when it refuses a payment below that limit. The limit is 546
  satoshis for a legacy address and 294 for a native SegWit one. `hw` said
  only that an output was smaller than the dust value.
- `hw` says that the connection was lost, or that the server closed it
  without an answer, when a server goes away in the middle of an answer. It
  showed the name of the exception before.
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

- A `host` without a slash at the end now works. `hw` put the name of the
  network right after the host, so `http://localhost:3000` became
  `http://localhost:3000btc/...`. Every call to the server failed with an
  address that is not valid. `hw` now adds the slash when the host has none.
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
- `sync` no longer fails on a transaction the server does not publish again.
  It sends again each transaction you sent that the server has lost. A
  haskoin-store from version 1.11 answers with an error when no node takes
  the transaction. `sync` took that for a failure of the server, and the sync
  of the whole network failed on every run until the transaction was deleted.
  It now reports the transaction and goes on. A server that gives no answer
  still fails the sync of its network.
- `sync` marks a pending transaction in conflict when the payment its coin
  came from is gone. A reorganization can replace a payment with a rival one,
  and the server then forgets the first. The pending transaction that spent
  its coin stayed as it was, and `sync` sent it again on every run. `sync` now
  marks it when the portfolio holds the payment as dead and the server no
  longer returns it. The mark goes away if the payment comes back.
- `broadcast` of a payment that `sync` has already seen on the network says
  that the payment was sent, and points to `txs` and `bumpfee`. It said that
  no pending transaction had that id.
- `preparetx`, `preparesendall`, `preparesweep` and `bumpfee` show the time
  the transaction was prepared. Their answer started with "time unknown". So
  did the transaction `signtx` and `broadcast` show before they ask.
- `recover` gives the `switch` to the first portfolio it found paid when the
  empty `portfolio0` of a new wallet became the active one. It gave no step,
  and `txs` then listed nothing.
- `txs` names the receiving address of a transaction between addresses of
  the portfolio. It named the address paid the most, which is the change
  address when the change is the larger amount.

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
