# Haskoin Wallet

[![Hackage](https://img.shields.io/hackage/v/haskoin-wallet.svg)](https://hackage.haskell.org/package/haskoin-wallet)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jprupp/haskoin-wallet/blob/master/LICENSE)

Haskoin Wallet (`hw`) is a command-line wallet for Bitcoin and Bitcoin Cash,
made for cold storage. The mnemonic can stay on an offline computer, while an
online computer watches the accounts and prepares the payments. A payment is
prepared without any key, signed on the offline computer, and sent from the
online one. The two computers exchange files: the public key of each account,
and the transactions to sign.

`hw` keeps its accounts in a SQLite database, and asks a [haskoin-store] server
about the chain. The database holds public keys only. Your mnemonic is never
written to disk. `hw` suits small wallets managed by hand.

What `hw` does:

- [Mnemonics](#hw-mnemonic) as [BIP39] describes, or backups in shares as
  [SLIP39] describes.
- [Accounts](#hw-createacc) as [BIP44] derives them, with legacy addresses, or
  as [BIP84] does, with native SegWit addresses, which pay lower fees.
- [QR codes of addresses](#hw-receive) on the terminal, for a payer to scan.
- [Signing on an offline computer](#signing-on-an-offline-computer), or with a
  hardware wallet, through [transaction files](#transaction-files).
- [Coin control](#hw-preparetx), and [fee rates](#hw-fees) read from recent
  blocks.
- [Fee bumping](#hw-bumpfee) on Bitcoin ([BIP125]).
- [Sweeps](#sweeps) of coins whose private keys you hold.
- [Message signing](#messages) in the Bitcoin Signed Message format.
- [Labels and accounts](#hw-exportacc) shared with other wallets.
- [JSON answers](#options-of-every-command) from every command, for scripts.
- [CSV files](#listings) of every listing, and a
  [spreadsheet](#hw-exportsheet) of the whole wallet.

## Contents

- [Install](#install)
- [Quick start](#quick-start)
  - [A first payment](#a-first-payment)
  - [Signing on an offline computer](#signing-on-an-offline-computer)
- [Keep your funds safe](#keep-your-funds-safe)
- [Using `hw`](#using-hw)
  - [Getting help](#getting-help)
  - [Options of every command](#options-of-every-command)
  - [Amounts](#amounts)
  - [Listings](#listings)
  - [Short ids](#short-ids)
  - [Files](#files)
  - [One command at a time](#one-command-at-a-time)
- [Commands](#commands), each with its examples and what is good to know
  - [Mnemonics and accounts](#mnemonics-and-accounts)
  - [Addresses](#addresses)
  - [Coins](#coins)
  - [Payments](#payments)
  - [Transactions](#transactions)
  - [Sweeps](#sweeps)
  - [Messages](#messages)
  - [The wallet](#the-wallet)
- [Transaction files](#transaction-files)
- [Configuration](#configuration)
- [Shell completion](#shell-completion)
- [Upgrading from an earlier version](#upgrading-from-an-earlier-version)

## Install

You need [Stack] and the secp256k1 library. On Debian or Ubuntu:

```console
sudo apt install libsecp256k1-dev
git clone https://github.com/jprupp/haskoin-wallet.git
cd haskoin-wallet
stack build
stack test
stack install
```

`stack install` copies `hw` to `~/.local/bin`. Put that directory on your
`PATH`, then check the install with `hw --help`.

The Tab key can complete what you type after `hw`, once you set up
[shell completion](#shell-completion).

## Quick start

Read [Keep your funds safe](#keep-your-funds-safe) before you put real funds in
the wallet.

The examples in this README write `ADDRESS`, `NOSIGHASH`, `TXID`, `HASH` and
`TXHASH` where you put your own values.

### A first payment

```console
hw mnemonic                                 # write the words down
hw createacc main                           # type the mnemonic and passphrase
hw receive --label "first deposit"          # give this address to the payer
hw sync                                     # once the payment is sent
hw preparetx ADDRESS:0.0005 --fee-rate median
hw signtx NOSIGHASH                         # type the mnemonic again
hw broadcast TXID                           # the txid signtx shows
hw sync
```

`preparetx` prints the nosigHash of the new transaction, and the commands to
run next. A nosigHash is a hash of the transaction without its signatures. It
names the transaction until it is signed. `signtx` then prints its txid, the id
the network knows it by, and `pendingtxs` shows the txid from then on. The
commands take either hash, in full or by its [short id](#short-ids), its first 8
digits, which the next steps and the listings show.

### Signing on an offline computer

1. On the offline computer, create the account and export its public key:

   ```console
   hw mnemonic --dice
   hw createacc vault
   hw exportacc --account vault vault.jsonl
   ```

2. On the online computer, import the account and prepare a payment to a file:

   ```console
   hw importacc vault.jsonl
   hw sync
   hw preparetx ADDRESS:0.01 --fee-rate median --output unsigned.psbt
   ```

3. Back on the offline computer, check the transaction and sign it:

   ```console
   hw show unsigned.psbt
   hw signtx --input unsigned.psbt --output signed.psbt
   ```

4. On the online computer, send it:

   ```console
   hw broadcast --input signed.psbt
   hw sync
   ```

A hardware wallet, or another wallet with the keys, can sign the file in place
of the offline computer. See [Transaction files](#transaction-files).

#### Check an address before you give it out

An online computer that was tampered with can show an address of its own in
place of one of the account. The offline computer holds the same account and
derives the same addresses, without the mnemonic. It did not give the address
out, so it shows it cut, with a dot for each character of its middle. Compare
the two ends with the address the online computer shows.

```console
hw receive                                  # on the online computer: /0/7 and the address
hw show /0/7                                # on the offline computer: the same ends?
```

The ends are enough, since nobody can make another address with the same ends.
The middle is hidden so that nobody pays an address that the wallet never gave
out. See [`show`](#hw-show).

## Keep your funds safe

- **The mnemonic is the only key to the funds.** `hw` never stores it. Without
  the database, the mnemonic still spends the funds, and
  [`recover`](#hw-recover) finds the accounts again. Without the mnemonic, the
  funds are lost.
- **The mnemonic is in memory while a command runs.** `hw` cannot wipe it from
  there, so it protects its memory as a whole. It writes no core file when it
  crashes. On Linux, no other program of yours can read its memory or attach to
  it as a debugger. The administrator of the computer still can. Type a
  mnemonic on an offline computer if you can.
- **The database and the account files are private.** They hold no private key.
  But the public keys in them reveal every address and balance of your
  accounts. `hw` makes `~/.hw` and the database yours alone to read. A wallet
  made by `hw` 0.9 or earlier keeps the permissions it had:
  [make it private](#a-wallet-readable-by-others).
- **An online computer can show a wrong address.** One that was tampered with
  shows an address of its own, and the payer pays the attacker.
  [Check an address](#check-an-address-before-you-give-it-out) with `show`
  on the offline computer before you give it out.
- **`hw` takes the server's word for the chain.** It checks that the server
  answers about the addresses and transactions it asked about and no others,
  and that the counts agree, but it cannot tell a hidden payment from none.
  It checks each coin against the transaction it comes from, which it keeps,
  so a server cannot change what a coin is worth. It reads no answer of more
  than 64 MiB.
  After a thousand used addresses in a row, `sync` and `recover` ask whether to
  keep looking, since a server that lies reports them without end. `recover`
  asks the same after every 20 accounts the server reports paid. Run your own
  haskoin-store to trust no one else.

## Using `hw`

### Getting help

`hw --help` lists the commands, and `hw COMMAND --help` explains one.
`hw --version` shows the version of `hw` and of its database format.

The commands in magenta in the list ask for a mnemonic or private keys. Run them
on an offline computer if you can. When the help is not shown on a terminal, or
with `NO_COLOR` set, they are marked `!` instead.

[`recover`](#hw-recover) and [`preparesweep --key-file`](#hw-preparesweep) take a
mnemonic or private keys too. They need the server, so they run on an online
computer and are not marked. Their sections say how to keep the secret offline.

### Options of every command

These options work with every command, before or after it:

| Option | What it does |
|---|---|
| `--unit UNIT` | Sets the unit of the amounts shown, and of the amounts typed without a unit: `btc`, `bit` or `sat`. A bit is 100 satoshis. The default is the key `unit` of the [configuration](#configuration), or `btc`. |
| `--json` | Prints the answer as JSON. Questions and warnings go to stderr. |
| `--no-pager` | Prints a listing as it is, through no pager. |

### Amounts

**An amount without a unit is in bitcoins**, unless `--unit` or the
configuration names another unit. `ADDRESS:1000` asks for 1000 BTC, not 1000
satoshis. Write the unit, as in `ADDRESS:1000sat`, or give `--unit sat`.

An amount can end with `btc`, `bit`, `bits`, `sat` or `sats`. Bitcoins take up
to 8 decimals, bits up to 2, and satoshis none. `ADDRESS:0.00001`,
`ADDRESS:10bits` and `ADDRESS:1000sat` pay the same.

If you count in satoshis or in bits, set the key `unit` of the
[configuration](#configuration). Amounts are then shown in that unit. An
amount typed without a unit is read in it too. A script should give `--unit`,
or write the unit after each amount, so that the configuration cannot change
what it pays. JSON answers give every amount in satoshis, whatever the unit.

### Listings

Six commands list what the wallet holds: `accounts`, `addresses`, `balances`,
`coins`, `txs` and `pendingtxs`. By default, they show each element in a few
lines. This is the compact view. `--verbose` gives every detail of each
element. `--tabular` gives each element one row of a table. The section of each
command says what its views show.

If you always want the same view, set the key `listing-view` of the
[configuration](#configuration) to `verbose` or `tabular`. `--compact` then
brings back the compact view for one listing. The key changes the screen alone.
`--json` and `--csv` answer as they would without it. A command that answers
with one element, such as `lockcoin`, keeps its own view.

On a terminal, a listing such as `hw txs` goes through `less`. The arrow keys
scroll it, `/` searches it and `q` quits it. A listing that fits on the screen
is shown as it is. `HW_PAGER`, the key `pager` of `config.json` or `PAGER` name
another pager.

Each line stays on one line of the screen, so that the columns of a view line
up. A line wider than the screen is cut at its edge, which a `>` marks, and the
right and left arrow keys scroll to the rest of it. `less` then stays open, so
that you can. The compact and verbose views are laid out for 80 columns, so
this happens mostly to the rows of a table, with `--tabular`. The one exception
is `accounts --verbose`, whose lines wrap, so that you can copy the descriptor
of an account whole. A table taller
than the screen also keeps its header at the top while its rows scroll, with
`less` 608 or later. When you set `LESS` yourself, `hw` leaves it as it is.

Scripts read a listing a page at a time with `--max`, then `--after` the last
row they got.

```console
hw txs --max 20 --json
hw txs --max 20 --after TXID --json
```

`--csv FILE` writes a listing to a new file instead, for a spreadsheet. Each
element takes a row, and the columns hold what the JSON answer holds, such as
the txid, the coin and the fee, each in a column of its own. Each hash comes in
full, then by its [short id](#short-ids). Amounts are in
satoshis, as whole numbers, whatever `--unit` says. Times are in UTC, as
`2023-11-14T22:13:20`, whatever the time zone of the computer. `--after` and
`--max` choose the rows, as they do on the screen.

```console
hw txs --csv txs.csv
hw coins --csv coins.csv
```

> **Good to know**
>
> - **A spreadsheet guesses what each cell of a CSV file holds.** A label made
>   of digits alone, such as `007`, becomes the number 7. The fingerprint of a
>   seed is often made of digits alone, so the file of `accounts` leaves it
>   out. `accounts --verbose` gives it. About one short txid in 43 is made of
>   digits alone too, such as `01234567`, and becomes a number without its
>   leading zero. The full hash in the column before it stays text. A short
>   coin stays text, since its colon makes it no number. `exportsheet` keeps
>   every short id as text.
> - **A label that starts with `=`, `+`, `-` or `@` gets an apostrophe in
>   front.** A spreadsheet would run it as a formula, and a label can come from
>   a file of another wallet. A formula could send your rows to a web server.
>   The apostrophe shows in the cell.
> - **A spreadsheet set to a decimal comma may expect semicolons.** Excel may
>   then show each row in one cell. Import the file with the comma as the
>   separator rather than open it. LibreOffice asks which separator to use.
> - **The file tells your history.** Its addresses, amounts and labels tell
>   whoever reads it what you hold and whom you paid. `hw` makes it readable
>   by you alone.

### Short ids

A txid has 64 digits, too many for a table and for typing. The listings and the
compact views name a transaction by its short id instead: the first 8 digits of
its txid, such as `a1075db5`. A pending transaction has the short id of its
nosigHash until it is signed, then that of its txid. A coin has the short id of
its transaction, a colon and its index, such as `a1075db5:1`. `--verbose` gives
the hash in full, then its short id.

Every command that takes a hash or a coin takes its short id too:

```console
hw signtx a1075db5
hw lockcoin a1075db5:1
hw label a1075db5 "rent of October"
hw txs --after a1075db5
```

A short id is the start of the hash itself, not a number the wallet hands out.
It means the same on every computer, the offline one included, and in a block
explorer, where the txid starts with it. A typo in it almost never matches
another transaction, so `hw` refuses it rather than act on the wrong one.

Two hashes share their first 8 digits about once in 4 billion pairs. When a
short id matches two or more hashes that a command can take, `hw` names each in
full and asks for the full hash. It never picks one. A part of a hash longer
than 8 digits is refused: give the short id or all 64 digits.

> **Good to know**
>
> - **`preparesweep --coin` takes the txid in full.** The wallet does not hold
>   the coins it sweeps, and the server looks up a whole txid only.
> - **Files and scripts get whole hashes.** The transaction files, the files of
>   labels and the answers in JSON give each hash in full. Another wallet, or a
>   script, needs the exact hash. `--csv` and `exportsheet` give both forms.

### Files

**`hw` never overwrites a file.** `--output`, `--csv`, `backup` and `exportsheet`
refuse a file that exists, even one that appeared while the command ran. `signtx` and `signsweep`
replace their `--input` file only when you agree.

A file `hw` writes is whole on the disk before it takes its name, so a crash or
a power loss leaves no half-written file. It is readable by its owner alone.

### One command at a time

A second `hw` command on the same database waits three seconds, then stops with
"The database is in use by another hw command".

## Commands

The commands marked `!` ask for a mnemonic or private keys. Run them on an
offline computer if you can. [`recover`](#hw-recover) and
[`preparesweep --key-file`](#hw-preparesweep) take them too, but they need the
server, so they are not marked.

| Command | What it does |
|---|---|
| **[Mnemonics and accounts](#mnemonics-and-accounts)** | |
| [`mnemonic`](#hw-mnemonic) `!` | Make a new mnemonic, or a backup in shares |
| [`createacc`](#hw-createacc) `!` | Create an account from a mnemonic |
| [`recover`](#hw-recover) | Find the accounts of a mnemonic that were paid |
| [`checkmnemonic`](#hw-checkmnemonic) `!` | Test a mnemonic or a backup against an account |
| [`exportacc`](#hw-exportacc) | Write the accounts and their labels to a file |
| [`importacc`](#hw-importacc) | Add accounts and labels from a file, a descriptor or a key |
| [`accounts`](#hw-accounts) | Show the accounts, their balances and descriptors |
| [`renameacc`](#hw-renameacc) | Rename an account |
| [`deleteacc`](#hw-deleteacc) | Delete an account from the wallet |
| [`sync`](#hw-sync) | Update the accounts from the server |
| **[Addresses](#addresses)** | |
| [`receive`](#hw-receive) | Get a new address to receive a payment |
| [`show`](#hw-show) | Show an address, a transaction or a coin, or a transaction file |
| [`addresses`](#hw-addresses) | List the receiving addresses, or the change addresses |
| [`balances`](#hw-balances) | List the addresses that hold coins |
| [`label`](#hw-label) | Label a receiving address or a transaction |
| **[Coins](#coins)** | |
| [`coins`](#hw-coins) | List the coins of an account |
| [`lockcoin`](#hw-lockcoin) | Keep a coin from being spent |
| [`unlockcoin`](#hw-unlockcoin) | Free a locked coin |
| **[Payments](#payments)** | |
| [`fees`](#hw-fees) | Show the fee rates of recent blocks |
| [`preparetx`](#hw-preparetx) | Prepare a payment, with coin control if you like |
| [`preparesendall`](#hw-preparesendall) | Prepare a payment of all the coins of an account |
| [`signtx`](#hw-signtx) `!` | Sign a pending transaction or a transaction file |
| [`broadcast`](#hw-broadcast) | Send a signed transaction to the network |
| [`bumpfee`](#hw-bumpfee) | Replace a stuck payment with one that pays a higher fee |
| **[Transactions](#transactions)** | |
| [`txs`](#hw-txs) | List the transactions of an account |
| [`pendingtxs`](#hw-pendingtxs) | List the pending transactions |
| [`exporttx`](#hw-exporttx) | Write a pending transaction to a file |
| [`importtx`](#hw-importtx) | Add a transaction file to the pending transactions |
| [`deletetx`](#hw-deletetx) | Delete a pending transaction and free its coins |
| **[Sweeps](#sweeps)** | |
| [`preparesweep`](#hw-preparesweep) | Prepare a transaction that moves coins from outside the wallet |
| [`signsweep`](#hw-signsweep) `!` | Sign a sweep with the private keys of its coins |
| **[Messages](#messages)** | |
| [`signmsg`](#hw-signmsg) `!` | Sign a message with the key of an address |
| [`verifymsg`](#hw-verifymsg) | Check the signature of a message |
| **[The wallet](#the-wallet)** | |
| [`backup`](#hw-backup) | Copy the wallet database to a backup file |
| [`exportsheet`](#hw-exportsheet) | Write every listing of the wallet to a spreadsheet |

### Mnemonics and accounts

#### `hw mnemonic`

Prints a new mnemonic, made from the entropy of the operating system, and
stores nothing.

```console
hw mnemonic --entropy 32 --dice
hw mnemonic --shares 3 --threshold 2
hw mnemonic --shares 1
```

| Option | What it does |
|---|---|
| `--entropy BYTES` | Sets the bytes of entropy: 16, 20, 24, 28 or 32, for 12, 15, 18, 21 or 24 words. The default is 16. A backup in shares takes 16 or 32. |
| `--dice` | Mixes in rolls of a die. |
| `--shares N` | Prints the N shares of a backup instead of a mnemonic, in the [SLIP39] format. |
| `--threshold T` | Sets how many shares rebuild the seed. |

**Backups in shares.** Any `--threshold` of the shares rebuild the seed, and
fewer tell nothing of it. With `--shares 3 --threshold 2`, losing one share
loses nothing, and finding one gives nothing away. A share is a list of 20
words, or of 33 with `--entropy 32`. `--shares 1` makes a single share, which is
the whole backup.

The commands that ask for a mnemonic take the shares in its place. Type one, and
`hw` asks for the next until it has enough.

> **Good to know**
>
> - **A backup in shares is not a mnemonic.** A wallet that reads [SLIP39]
>   shares restores it. One that reads only BIP39 mnemonics cannot.
> - **The passphrase of a backup in shares** has printable ASCII characters
>   only.

#### `hw createacc`

`hw createacc NAME` creates an account from a mnemonic and passphrase, each
typed twice, or from the shares of a backup, typed once, and their passphrase.
It stores the public key of the account, at the next free index of the seed on
the network.

```console
hw createacc savings
hw createacc spending --type bip84
hw createacc cash --network bch
```

An account is of one of two kinds:

| Kind | Addresses | On Bitcoin, they start with | Derivation path |
|---|---|---|---|
| `bip44`, the default | Legacy P2PKH | `1` | `m/44'/coin'/index'` |
| `bip84` | Native SegWit P2WPKH | `bc1q` | `m/84'/coin'/index'` |

The payments of a `bip84` account pay about 40% less in fees. Other wallets make
`bip84` accounts by default. Bitcoin Cash has no SegWit, so it has `bip44`
accounts only. `hw` can pay any kind of address from either kind of account.

`createacc` answers with every detail of the new account, as
[`accounts --verbose`](#hw-accounts) shows it, its output descriptor last.

#### `hw recover`

Finds the accounts of a mnemonic, or of a backup in shares, that were paid. It
adds them and syncs them. It needs the mnemonic and the server at the same time.

```console
hw recover
hw recover --network bch --account-gap 20
```

Without `--network`, `recover` searches Bitcoin, then Bitcoin Cash. The
`default-network` of the configuration does not apply. A seed is often paid on
both, as every seed from before the fork of 2017 is, and a search of one
network would say nothing of the other. A test network is searched only when
`--network` names it.

The accounts are named after their network and index: `btc0`, `btc1`, `bch0`
and so on for the `bip44` accounts, and `btc0-segwit`, `btc1-segwit` and so on
for the `bip84` ones. The search stops after 10 accounts in a row without a
payment, or the number `--account-gap` gives, for each kind of account apart.
After a thousand used addresses in a row, it asks whether to keep looking, as
[`sync`](#hw-sync) does. On a terminal, a line tells which network and account
it is looking at.

After every 20 accounts of a kind the server reports paid, `recover` asks
whether to keep looking. A server that lies reports them without end. A no stops
the search, and no account is added.

`recover` also looks where other wallets keep their accounts, and tells of funds
it cannot spend:

- The `bip49` accounts of wrapped SegWit on Bitcoin.
- The `bip86` accounts of Taproot on Bitcoin.
- The `bip44` accounts of Bitcoin on Bitcoin Cash, where a wallet that held
  Bitcoin at the fork of 2017 has its Bitcoin Cash.

It adds no such account. Move the funds with a wallet that holds such accounts,
to an address of `hw`.

> **Good to know**
>
> - **`recover` takes the mnemonic on an online computer.** It needs the server
>   at the same time, so it cannot run on an offline one. A computer that was
>   tampered with can steal a mnemonic typed on it. To keep the mnemonic
>   offline, create the accounts on the offline computer with
>   [`createacc`](#hw-createacc) instead. Copy them with
>   [`exportacc`](#hw-exportacc) and [`importacc`](#hw-importacc), and run
>   [`sync`](#hw-sync).
> - **A network the server does not answer for is left out.** `recover` still
>   searches the other one. Its answer ends with the network it left out, and
>   it exits with an error, as `sync` does. Run `recover` again once the
>   server answers. The accounts found the first time are skipped.

#### `hw checkmnemonic`

Checks that a mnemonic and passphrase give the public key of an account. Use it
to test a mnemonic you wrote down, or the shares of a backup. Nothing is signed.

```console
hw checkmnemonic --account savings
```

When they do not match, it says if they give the key at another path. An account
imported from its key alone may have been given the wrong `--type` or
`--network`, and `checkmnemonic` tells which to import it again with.

#### `hw exportacc`

`hw exportacc FILE` writes the accounts and their labels to a file, which
[`importacc`](#hw-importacc) adds to another wallet. `--account` writes one
account instead of all of them.

```console
hw exportacc --account vault vault.jsonl    # on the offline computer
hw exportacc labels.jsonl                   # every account, for another wallet
```

The file is in the format of [BIP 329], which other wallets read and write. An
account goes in as its public key, its name, its network, its kind and the
fingerprint of its seed, then the labels of its receiving addresses and
transactions, and the coins locked with `lockcoin`. The file holds no private
key.

#### `hw importacc`

`hw importacc FILE` adds the accounts and labels of a file to the wallet.

```console
hw importacc vault.jsonl                    # on the online computer
hw importacc sparrow-labels.jsonl --network btc
hw importacc coldcard-export.json --network btc
hw renameacc btc0 coldcard
hw importacc ledger-xpub.txt --network btc --type bip84
```

It reads these files:

- The file of `exportacc`, or a [BIP 329] file of another wallet.
- A descriptor on a line.
- A key alone.
- The file of Bitcoin Core's `listdescriptors`.
- The generic JSON of a Coldcard.

**Accounts of a BIP 329 file.** `importacc` creates the accounts the wallet does
not have, named after the label of their key. It renames those it has when the
file is one of `hw`, since other wallets label a key with the name of its
signer.

**Accounts of a descriptor or a key.** An account added this way is named after
its network, index and kind, as `recover` names one. `renameacc` renames it.

**Labels and locks.** `importacc` sets the labels and locks on what the wallet
has. It labels a transaction before `sync` fetches it when the file says which
account it is of. It derives an address from the path the file gives, up to a
thousand past the last used address of its chain. What it cannot place, it
counts: run `sync`, then `importacc` again. Labels of inputs, coins and change
addresses are left aside, since `hw` keeps none.

**The network.** A file from another wallet or a descriptor names no network for
its keys, so give `--network`, or set a default network.

**The kind of account.** `hw` holds `bip44` and `bip84` accounts:

| The file gives | The account |
|---|---|
| A `pkh(...)` or a `wpkh(...)` descriptor | It is a `bip44` or a `bip84` account, as the descriptor says, with the origin of its key or without. |
| A descriptor of another kind, such as `sh(wpkh(...))` or `tr(...)` | It is refused, and the import goes on. |
| A `zpub` or a `vpub`, which some wallets write for a `bip84` account | It is a `bip84` account. |
| A `zpub` with `--type bip44` | It is refused, as the two contradict. |
| An `xpub` or a `tpub` alone | It is a `bip44` account, or a `bip84` one with `--type bip84`. |

> **Good to know**
>
> - **An `xpub` alone says nothing of its path.** The answer notes that its kind
>   was taken on trust, and what tells when it was wrong: a sync that finds none
>   of its transactions. `deleteacc`, then import it again with the other
>   `--type`.
> - **`importacc` derives no address over a thousand past the last used one of
>   its chain.** A file names any path, and a wrong one would have `hw` derive
>   addresses for days. Such an address is counted as not in the wallet yet, and
>   the answer says why.

#### `hw accounts`

Shows the accounts with their balances. Each takes two lines: its name, network
and derivation path, then what it can spend, what is unconfirmed and how many
coins it holds. The spendable balance leaves out locked coins, coinbase coins
not yet mature, and unconfirmed payments from other wallets.

`--verbose` adds the fingerprint of the seed, the addresses, when the account
was created, and its confirmed and unconfirmed balances. It ends with the
account as an output descriptor, the format of [BIP 380] in which other wallets
take an account without its private key. Paste the descriptor into a wallet
that takes one, to watch the account there.

`--tabular` shows each account on one row of a table: its name, network and
derivation path, what it can spend, what is unconfirmed, and its coins. The
first column is as wide as the longest name, which no row cuts.

`--csv FILE` writes the accounts to a [CSV file](#listings): the name, network
and path of each, what it can spend, its confirmed and unconfirmed balances,
its coins, how many receiving and change addresses it has, and when it was
created. The file holds no key and no descriptor, since they would show every
address and payment of the account to anyone the file reaches.

```console
hw accounts
hw accounts --verbose
hw accounts --verbose --account vault       # the descriptor of vault, for another wallet
hw accounts --tabular
hw accounts --csv accounts.csv
hw accounts --account savings --unit sat
```

**The descriptor.** A `bip44` account gives
`pkh([bc560f95/44h/0h/0h]xpub.../<0;1>/*)#vq597j87`. It carries the public key
of the account, the fingerprint of its seed and its derivation path, the two
chains of addresses, and a checksum. It holds no private key. When `hw` does
not know the fingerprint, the descriptor names no origin, which a hardware
wallet needs, and the row `Fingerprint` says `unknown`. A descriptor names no
network: Bitcoin and Bitcoin Cash write the same keys, and every test network
writes `tpub` keys.

A `bip84` account gives `wpkh([bc560f95/84h/0h/0h]xpub.../<0;1>/*)#5nktextl`,
the script function naming the kind. It is shown with its key as a `zpub` as
well, or a `vpub` on a test network, in the row `SLIP-132`. Some wallets take
only that form for a native SegWit account, and make a legacy wallet of an
`xpub`. The key is the same, only its prefix differs.

`--json` gives the descriptor of each account as `descriptor`, and its `zpub`
as `slip132`, `null` for a `bip44` account. [`createacc`](#hw-createacc) shows
the descriptor of the new account too. [`importacc`](#hw-importacc) reads a
descriptor back.

> **Good to know**
>
> - **The lines of `accounts --verbose` wrap on a terminal.** A descriptor is
>   wider than most screens. The pager wraps these lines rather than cut them
>   at the edge of the screen, so that you can copy a descriptor whole. Its row
>   comes last, so that the wrapped lines break no column of the others.

#### `hw renameacc`

`hw renameacc OLD NEW` renames an account.

```console
hw renameacc savings rainy-day
```

#### `hw deleteacc`

`hw deleteacc NAME NETWORK DERIVPATH` removes an account with its addresses,
labels, transactions and coins. The network and the path, as `accounts` shows
them, confirm the choice. The funds stay on the chain.

```console
hw deleteacc savings btc "/44'/0'/0'"
```

> **Good to know**
>
> - **Quote derivation paths.** They hold `'`, which the shell reads as a quote.
>   Write `"/44'/0'/0'"`.

#### `hw sync`

Updates the accounts from the server: balances, transactions, coins and pending
transactions. It also finds addresses that another wallet of the same seed used.
For each account, it shows its balances, the block it is synced to, and what
changed. Run it before you prepare a payment and after you send one.

```console
hw sync
hw sync --account savings --full
```

`--full` downloads the account's data again and writes it anew. Use it if the
wallet shows something wrong. It keeps labels, locked coins and pending transactions.
A reorganization of the chain does not need it: `sync` sees one by itself.

On a terminal, a line tells what `sync` is doing while it works, with a bar for
the steps whose size it knows. The line goes to stderr and is gone when `sync`
ends, so scripts never see it.

> **Good to know**
>
> - **A network that does not answer is left out.** When the server does not
>   answer for a network, `sync` leaves out the accounts of that network and
>   says why, and still syncs the others.
> - **`sync` asks after a thousand used addresses in a row** whether to keep
>   looking, since a server that lies reports them without end. A `sync` that
>   cannot ask stops there.

### Addresses

#### `hw receive`

Gives the next receiving address of an account. On a terminal, its QR code comes
under it, for the payer to scan.

```console
hw receive --label "invoice 42"
```

> **Good to know**
>
> - **`receive` stops after 20 unused addresses in a row.** A wallet restored
>   from the mnemonic would not look further. Reuse one of them, or raise `gap`
>   in the [configuration file](#configuration).
> - **The code holds a payment URI** as [BIP 321] describes, with the address
>   and nothing else. The label stays in the wallet.
> - **The code takes 37 columns and 19 lines.** On a narrower terminal, its lines
>   wrap and tear it apart.
> - **The code is black on white, whatever the theme of the terminal.** With
>   `NO_COLOR` set, it takes the colours of the terminal. On a dark theme it is
>   then light on dark, which some scanners cannot read.
> - **Scripts get the text alone.** Piped into another program, or with `--json`,
>   `receive` shows no code.

#### `hw show`

`hw show [WORD]` shows one thing of the wallet, or a transaction file, and
changes nothing. The word tells what it is, by its shape:

| Word | What `show` shows |
|---|---|
| none | The newest receiving address of the account, the first one `addresses` lists |
| `/0/i` or `/1/i` | The receiving address `i`, or the change address `i`, derived from the public key, and cut when the wallet does not hold it |
| An address | That address of the wallet, a change address too |
| A txid or a nosigHash | That transaction, pending or in the history |
| `TXID:INDEX` | That coin, with every detail, as `coins --verbose` shows it |
| Any other word | That [transaction file](#transaction-files), as its account sees it |

A hash goes in full or by its [short id](#short-ids).

```console
hw show                                     # the newest receiving address
hw show /0/7                                # the address at a path
hw show /1/2 --account vault
hw show ADDRESS
hw show a1075db5                            # a transaction by its short id
hw show a1075db5:1                          # a coin
hw show unsigned.psbt                       # a transaction file
```

**An address.** An address the wallet holds is shown as `receive` shows a new
one, with its QR code on a terminal. Use it to give an address out again.
`show` hands out no new address.

**A path.** `show` derives the address at a path from the public key of the
account. It needs no mnemonic and no server, so it runs on the offline computer
too. There it checks an address the online computer gave out: see
[Check an address](#check-an-address-before-you-give-it-out).

An address the wallet holds is shown as any other. One the wallet does not
hold, as on the offline computer, is shown cut. A dot stands for each character
of its middle, and a note says how to check it:

```console
$ hw show /0/308
/0/308  169h8ryuSwhG..........Cpzv9Q8PGum5
  Account  : vault
  Network  : btc

Compare both ends with the address that receive gave out at /0/308 on the online
computer. If they match, that address is of this account, and you can give it
out. If they differ, the online computer may have been tampered with, and you
should not give the address out. Nobody can make another address with the same
ends. This screen is for checking only, so the middle of the address is hidden.
Give out the address as receive showed it.
```

The ends carry at least 128 bits. To make an address with the same ends, an
attacker would have to try about 2^128 keys, which no computer can. The middle
hides enough that over a million addresses fit the cut address, so nobody can
pay it. That matters because the wallet never gave the address out. It may
lie past the gap of 20 that a restored wallet looks at, and no `sync` would
find a payment there.

**A transaction.** A transaction is shown in full, as each account that holds
it sees it. A payment between two accounts of the wallet is shown once for
each. `--account` keeps the view of one account.

**A transaction file.** `show` shows the transaction in a file and the account
it belongs to, without changing the wallet. Run it on the offline computer
before you sign, and on the online computer before you send. The labels it
shows are those the file carries, so an offline computer sees the labels of the
online wallet. A file of another wallet that involves no account of the wallet
needs `--network`: see [Files of other wallets](#files-of-other-wallets).

> **Good to know**
>
> - **An address the wallet does not hold is refused**, so the code never pays
>   anyone else.
> - **An address at a path the wallet does not hold is cut, and has no QR
>   code**, even within the gap. `--json` gives it cut too, as `cutaddress`,
>   so that no script pays it.
> - **Look at the path as well as the ends.** A tampered online computer picks
>   the path it shows, so it can match an address of its own against any
>   address of the account. A path far past the addresses you gave out is a
>   sign of that. `show` takes any path, so that a large account can be checked
>   too.
> - **Without a word, the address may have been paid already.** `show` gives
>   out the newest address again, as it is. Run `receive` for a new one.
> - **`--account` goes with a path, a transaction, a file, or no word.** An
>   address and a coin tell their account, so `show` refuses `--account` with
>   them. `--network` goes with a file only.
> - **A file that involves two accounts needs `--account`.** A transaction the
>   wallet holds is shown once for each account instead.
> - **A file named like a hash, an address or a path is taken for one.** Type
>   its directory before it, as in `hw show ./a1075db5`. Tab offers such a file
>   once you type `./`.
> - **A word that is none of these and names no file is refused**, with the
>   forms `show` takes. A short id with a digit missing gets this answer, for
>   instance.

#### `hw addresses`

Lists the receiving addresses, the newest first, or the change addresses with
`--change`. Each takes two lines: its path, the address and its label, then what
it can spend and what it received, or that it is unused. `--verbose` adds its
balances, its coins and when it was created.

`--tabular` shows each address on one row of a table, as `txs --tabular` does.
The columns are the path, the address, what it can spend, what it received, in
how many transactions, and its label. An unused address
leaves its amounts empty. The table of change addresses has no label column,
since change addresses take no label.

`--csv FILE` writes the addresses to a [CSV file](#listings): the path, the
address and the label of each, what it can spend, its confirmed and
unconfirmed balances, what it received, its coins and transactions, and when
it was created.

```console
hw addresses
hw addresses --change
hw addresses --verbose
hw addresses --tabular
hw addresses --change --tabular
hw addresses --csv addresses.csv
hw addresses --max 10 --after ADDRESS
```

#### `hw balances`

Lists the addresses that hold coins, the largest total first.

`--verbose` gives the total and the number of coins of each address, what it
can spend now, and what it cannot in three parts. They are what pending
transactions lock, what you locked with `lockcoin`, and what is not spendable
yet, which waits for a confirmation.

`--tabular` shows each address on one row of a table: its path, the address,
the value of its coins, what it can spend now and the rest, and its label, or
`change address`.

`--csv FILE` writes the addresses to a [CSV file](#listings): the path, the
address and the label of each, its total, what it can spend, the three parts it
cannot, and its coins.

```console
hw balances
hw balances --verbose
hw balances --tabular
hw balances --csv balances.csv
```

#### `hw label`

`hw label ADDRESS LABEL` sets the label of a receiving address, and
`hw label TXID LABEL` the label of a transaction. An empty label, `""`, removes
the label.

```console
hw label ADDRESS "rent from Bob"
hw label NOSIGHASH "rent of October"
hw label TXID "salary"
hw label TXID ""                            # removes the label
```

**An address.** The address tells its account. Change addresses take no label.

**A transaction.** Name a pending one by the hash `pendingtxs` shows, and one of
the history by its txid. Either goes in full or by its [short id](#short-ids). A
pending transaction keeps its label in its file, through signing on an offline
computer, and in the history once the network has it. A label set while the file
is away stays when the signed file comes back.

`preparetx`, `preparesendall` and `preparesweep` take a `--label`, and `bumpfee`
gives a replacement the labels of the transactions it replaces. A transaction
between two accounts of the wallet gets the label in both.

### Coins

#### `hw coins`

Lists the coins of an account. Each takes two lines:

1. The [short id](#short-ids) of the coin, such as `a1075db5:1`, which
   `lockcoin` and `--coin` take. Then the time of the transaction that paid it,
   its value, its state, spendable or locked, and its confirmations, up to
   `999+ confirmations`. A coin a pending transaction spends is `locked by tx`.
2. The address that holds it, in full, with its derivation path, then the label
   of the address, or `change address` when it is one.

This is the one listing that gives an address of the wallet in full in its
compact view. What tells a coin apart is the address that holds it, more than
its path. It is also the one view that keeps the label of an address in full
on the line of the address, so that each coin takes two lines. Everywhere else
an address is in full, its label is on the line below, and where a view gives
only the path, the label stays on the same line.

`--verbose` starts each coin with the time of its transaction, and gives the
full count of the confirmations on the first line. The label of the transaction
that paid the coin, the coin in full and its short id, the block that holds it,
the time of the transaction to the second and the address in full with its path
follow as fields. The label of the address is on the line below it. An
unconfirmed coin is in no block yet, and shows none. The change of a payment
you made shows no label of its transaction: the label says what the payment was
for, not what its change is.

`--tabular` shows each coin on one row of a table: the short id of the coin,
the time of its transaction, its value, its state, its confirmations up to
`999+`, the address that holds it with its path, and the label of the address.

`--csv FILE` writes the coins to a [CSV file](#listings): the coin as
`TXID:INDEX`, in full and by its short id, the time of its transaction, its
value, state, confirmations and block, the address that holds it with its path,
and the labels of the address and of the transaction.

[`show TXID:INDEX`](#hw-show) shows one coin, with every detail.

```console
hw coins
hw coins --verbose
hw coins --tabular
hw coins --csv coins.csv
hw coins --locked
```

> **Good to know**
>
> - **Unconfirmed payments from others cannot be spent.** The sender can still
>   replace them until they confirm. The unconfirmed coins of your own
>   transactions can be spent: their change, and what one of your accounts pays
>   another.

#### `hw lockcoin`

Keeps a coin from being spent. It takes the coin by its
[short id](#short-ids), as `coins` shows it, or in full. It shows the coin with
every detail, as `coins --verbose` does.

```console
hw lockcoin TXHASH:1
hw lockcoin a1075db5:1                      # the short id coins shows
```

#### `hw unlockcoin`

Frees a coin locked with `lockcoin`. It shows the coin with every detail, as
`coins --verbose` does.

```console
hw unlockcoin TXHASH:1
```

### Payments

#### `hw fees`

Shows the low, median and high fee rates of the latest six blocks, in satoshis
per virtual byte. It is the unit the network charges fees in and every wallet
shows: the bytes of a signature in a witness count a quarter. The sizes `hw`
shows are in virtual bytes as well.

The commands that take `--fee-rate` show the same rates and ask when you give
none. Type a level, or a number of satoshis per virtual byte.

```console
hw fees
hw fees --network bch
```

> **Good to know**
>
> - **Choosing a fee rate needs the server.** Without `--fee-rate`, `hw` reads
>   the rates of recent blocks, and stops if it cannot reach the server.
> - **A rate above 100 satoshis per virtual byte asks for a confirmation,**
>   since it is more often a typo.

#### `hw preparetx`

`hw preparetx ADDRESS:AMOUNT...` prepares a payment, unsigned, and keeps it as a
pending transaction. It needs no mnemonic.

```console
hw preparetx ADDRESS:0.0005 --fee-rate median --label "rent of October"
hw preparetx ADDRESS1:10000sat ADDRESS2:250bits --fee-rate 5
hw preparetx ADDRESS:0.01 --spend-from MYADDRESS --change-back --fee-rate low
hw preparetx ADDRESS:0.01 --spend-from MYADDRESS --select --fee-rate low
hw preparetx ADDRESS:0.01 --recipients-pay --fee-rate high --output unsigned.psbt
```

| Option | What it does |
|---|---|
| `--fee-rate RATE` | Sets the fee rate: a number of satoshis per virtual byte, or `low`, `median` or `high`. Without it, `hw` shows [the rates](#hw-fees) and asks. |
| `--recipients-pay` | Takes the fee out of the amounts paid, in equal shares. |
| `--coin TXID:INDEX` | Chooses a coin to spend, in full or by its [short id](#short-ids). Every coin chosen is spent. Repeat it for several coins. |
| `--spend-from ADDRESS` | Chooses the coins at an address to spend. Every coin chosen is spent. Repeat it for several addresses. |
| `--select` | Spends only what the payment needs among the coins chosen, as `--strategy` selects them. |
| `--strategy STRATEGY` | Sets how the coins are selected: `least-waste`, the default, `random-draw`, `largest-first` or `smallest-first`. |
| `--min-conf N` | Sets the confirmations a coin needs to be spent. The default is 0. |
| `--change-back` | Sends the change back to an address the payment spends from. |
| `--new-change` | Sends the change to a new change address, even when the configuration file sets `change-back`. |
| `--output FILE` | Also writes the transaction to a [file](#transaction-files). |
| `--label LABEL` | Gives the transaction a label, which goes with it to its file and stays once the network has it. |
| `--yes` | Answers yes to the questions on a high fee rate and on a wallet behind the chain. |

> **Good to know**
>
> - **An amount without a unit is in bitcoins**, or in the unit of the
>   configuration. See [Amounts](#amounts).
> - **No output is smaller than what nodes relay.** That is 546 satoshis for a
>   legacy output, and 294 for a native SegWit one. A smaller payment is
>   refused, and smaller change goes to the fee.
> - **A transaction takes at most 100,000 virtual bytes**, the largest that
>   nodes relay. That is about 675 legacy coins, or 1,470 native SegWit ones.
>   `hw` refuses to prepare a larger one. Send the coins in several
>   transactions, choosing them with `--coin` or `--spend-from`.
> - **A wallet behind the chain asks before preparing.** The transaction gets
>   the lock time of the best block the wallet knows, as the transactions of
>   most wallets do. When that block is more than eight hours old, `preparetx`,
>   `preparesendall` and `bumpfee` ask whether to go on with a lock time of 0,
>   or to sync first. A `sync` first is the better answer: it also finds the
>   coins spent or received since. `--yes` goes on without asking.

#### `hw preparesendall`

`hw preparesendall ADDRESS[:AMOUNT]...` prepares a payment of every coin the
account can spend, with no change. An address with an amount gets that amount.
The addresses without one share the rest and pay the fee.

```console
hw preparesendall ADDRESS --fee-rate low
hw preparesendall ADDRESS1:0.1 ADDRESS2 --fee-rate median --leave-dust
```

`hw` asks whether to spend the coins worth less than their fee, unless
`--spend-dust` or `--leave-dust` answers.

`--fee-rate`, `--coin`, `--spend-from`, `--min-conf`, `--output`, `--label` and
`--yes` work as for [`preparetx`](#hw-preparetx).

#### `hw signtx`

Signs a pending transaction by its nosigHash, in full or by its
[short id](#short-ids), or a transaction file. It first
shows the transaction, as [`show`](#hw-show) does, then asks for the
mnemonic and passphrase of the account, or for the shares of its backup. Check
what it shows before you type the mnemonic, and stop it with Ctrl-C if it is
not the payment you mean. Once signed, it prints the txid and the file it wrote.
`signsweep` shows the transaction in the same way before it asks for the keys.

```console
hw signtx NOSIGHASH
hw signtx --input unsigned.psbt --output signed.psbt
```

#### `hw broadcast`

Shows a signed transaction, asks, and sends it. It then waits up to ten seconds
for the server to see it.

```console
hw broadcast TXID
hw broadcast --input signed.psbt
```

> **Good to know**
>
> - **`sync` sends again what you sent.** A transaction that the network refuses
>   for good, for a fee too low to relay for example, keeps coming back until
>   you delete it with [`deletetx`](#hw-deletetx).

#### `hw bumpfee`

`hw bumpfee TXID` prepares a replacement of an unconfirmed payment that pays a
higher fee. It works on Bitcoin only, since Bitcoin Cash does not replace
transactions. Sign and send the replacement like any payment.

```console
hw bumpfee TXID --fee-rate high
hw signtx NOSIGHASH
hw broadcast TXID                           # the txid of the replacement
```

The extra fee comes out of the change. When the change cannot pay it, `hw`
offers to add confirmed coins of the account, or to take what is missing from
what the recipients receive, asking which of them pays.

### Transactions

#### `hw txs`

Lists the transactions of an account, as of the last sync.
[`show TXID`](#hw-show) shows one of them in full. Each takes two lines:

1. Its [short id](#short-ids), the first 8 digits of its txid, then its time, in
   the time zone of the computer, its amount and its status.
2. The balance of the account after the transaction, then what the transaction
   is for.

What a transaction is for is said as the last column of the table says it:

- A payment received gives the path of the address it was paid at, then the
  label of the payment, or else the label of the address, as in
  `at /0/3 "Salary"`. A change address says `change address` instead.
- A payment gives its label, or else the address it pays, as in
  `to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU`.
- Either says how many other addresses it pays, as in `and 2 more`.

Nothing in it is cut, a long label or address included. On a terminal, the
addresses of the wallet and their labels are blue, which sets them apart from
the addresses of others.

The amount has a plus sign when it comes in. The status counts the
confirmations up to `999+ confirmations`. An unconfirmed transaction shows its
fee rate too. The balance of the newest transaction is what the account holds,
confirmed or not. A dead transaction, replaced or dropped from the network,
shows no balance and does not count.

`--verbose` starts each transaction with its time, then its type: `Outgoing`,
`Incoming`, or `Internal` for one between addresses of the account. It shows
the label as a field, the txid in full and its short id, every address in full
with its amount, and its path for an address of the wallet, the fee and the
balance. It gives
the full count of the confirmations. The label of an address of the wallet is
on the line below it, under the address. A change address the transaction
spends from or pays says `change address` there instead. An address the
transaction spends from adds the labels of the coins spent, as `coins` shows
them, after the word `from`. Before you sign, they tell which coins a payment
spends, and so which payments it links. Long labels wrap at the edge of the
screen, each line again under the address.

`--tabular` shows each transaction on one row of a table, as a spreadsheet
does. The columns are the short id, the time, the type, the amount, the balance
after it and the confirmations. The confirmations are a number alone, up to
`999+`. An unconfirmed transaction has 0, and a dead one says `dead`. The last
column, Description, says what the transaction is for, in the words of the
compact view. The header names the unit of the amounts, so the cells hold
numbers alone. The table leaves out the fee rate. `txs` without `--tabular`
shows it.

`--csv FILE` writes the transactions to a [CSV file](#listings): the time, type,
amount, fee and fee rate of each, the balance after it, its status,
confirmations and block, its txid in full and by its short id, and its label.
Then come the address it pays the most, its path and label when it is one of the
wallet, and how many other addresses it pays.

```console
hw txs
hw txs --verbose
hw txs --tabular
hw txs --csv txs.csv
hw txs --max 20 --after TXID
```

> **Good to know**
>
> - **A row of the table is wide.** It takes 84 columns before its last
>   cell, which takes what its label or address needs. `less` keeps the row
>   on one line, and the right arrow key scrolls to its end. The columns keep
>   the same widths from the first row to the last, so that a long listing
>   lines up. A rare cell that does not fit, such as an amount of 10 000
>   bitcoins or more, pushes the rest of its row.

#### `hw pendingtxs`

Lists the pending transactions: prepared, signed, or sent and not yet seen by
`sync`. They take two lines each, as in `txs`. The first starts with the
[short id](#short-ids) of the nosigHash until the transaction is signed, then
with that of its txid. It gives the time the transaction was created, its
amount and its status, with its fee rate. The second gives its account when
every account is listed, the transactions a replacement replaces, and what the
transaction is for. A conflicted one can no longer be sent.
[`show HASH`](#hw-show) shows one of them in full.

`--tabular` shows each transaction on one row of a table: its short id, its
account when every account is listed, the time it was created, its type, its
amount, its status, its fee rate in sat/vB, and what it is for, as
`txs --tabular` says it. The account column is as wide as the longest name.

`--csv FILE` writes the pending transactions to a [CSV file](#listings): the
account of each, when it was created, its type, amount, fee and fee rate, its
status, its nosigHash and its txid once signed, each in full and by its short
id, its label, and the address it pays
the most, as `txs --csv` gives it.

```console
hw pendingtxs
hw pendingtxs --verbose
hw pendingtxs --tabular
hw pendingtxs --csv pending.csv
```

> **Good to know**
>
> - **A pending transaction locks its coins.** They stay locked until `sync`
>   sees the transaction on the network, or until `deletetx` deletes it. Delete
>   the transactions you abandon.

#### `hw exporttx`

`hw exporttx HASH FILE` writes a pending transaction to a
[file](#transaction-files).

```console
hw exporttx NOSIGHASH unsigned.psbt
```

#### `hw importtx`

`hw importtx FILE` adds a [transaction file](#transaction-files) to the pending
transactions, or replaces the pending one with its signed version. It keeps the
labels of the wallet.

```console
hw importtx signed.psbt
```

#### `hw deletetx`

`hw deletetx HASH` deletes a pending transaction and frees its coins. Name it by
its nosigHash, or by its txid once it is signed, in full or by its
[short id](#short-ids). A transaction you sent can be
deleted only once the server no longer knows it.

```console
hw deletetx HASH
```

### Sweeps

#### `hw preparesweep`

Prepares a transaction that moves coins from outside the wallet. The keys are
needed only to sign, with [`hw signsweep`](#hw-signsweep). A sweep that pays an
address of this wallet is kept as a pending transaction.

```console
hw preparesweep --sweep-from OLDADDRESS --sweep-to MYADDRESS --fee-rate median
hw signsweep NOSIGHASH --key-file keys.txt
hw broadcast TXID
```

| Coins swept | Option |
|---|---|
| The coins at an address | `--sweep-from ADDRESS` |
| The coins at the addresses listed in a file | `--address-file FILE` |
| The coins at the addresses of the keys in a file | `--key-file FILE` |
| A coin given by its txid in full and its index | `--coin TXID:INDEX` |

A sweep that pays no address of this wallet needs `--network` and `--output`,
and no account. It goes from file to file:

```console
hw preparesweep --sweep-from OLDADDRESS --sweep-to ADDRESS --network btc --fee-rate median --output sweep.psbt
hw signsweep --input sweep.psbt --key-file keys.txt --output signed.psbt
hw broadcast --input signed.psbt
```

> **Good to know**
>
> - **`--key-file` reads the private keys on an online computer.**
>   `preparesweep` needs the server, so it cannot run on an offline one. A
>   computer that was tampered with can steal the keys. They are needed only to
>   sign, so keep them on an offline computer if you can. Name the coins with
>   `--sweep-from`, `--address-file` or `--coin` instead.
> - **Private keys and legacy addresses read the same on Bitcoin and Bitcoin
>   Cash.** That is why a sweep that pays no account of the wallet needs
>   `--network`, and the default network does not apply to it.
> - **A sweep takes the coins that have an address**: legacy, native SegWit and
>   SegWit nested in a script address. A coin paid to a public key itself, as
>   the first blocks hold them, has no address, and `hw` does not sweep it.

#### `hw signsweep`

Signs a sweep with the private keys of its coins, by its nosigHash or from a
file. `--key-file -` types the keys on the terminal instead of reading a file,
here and in `preparesweep`.

```console
hw signsweep NOSIGHASH --key-file keys.txt
hw signsweep --input sweep.psbt --key-file keys.txt --output signed.psbt
```

### Messages

#### `hw signmsg`

`hw signmsg ADDRESS MESSAGE` signs a message with the key of an address of the
wallet. `--output` writes the signed message to a file instead of printing it.

```console
hw signmsg MYADDRESS "I control this address" --output signed.txt
```

From a P2PKH address, any wallet verifies the signature. From a native SegWit
address, the signature has the header byte of BIP 137, which the wallets that
sign from such an address give and verify. Some wallets verify no signature from
a SegWit address.

> **Good to know**
>
> - **A message is signed byte for byte.** A space or a line break added at
>   either end makes the signature fail. `hw` warns about them.
> - **`signmsg` and `verifymsg` work with P2PKH and SegWit addresses of one
>   key,** not with Taproot ones.

#### `hw verifymsg`

Checks a signature from `hw` or another wallet, from any P2PKH or SegWit address
of one key, whatever header byte of [BIP 137] the wallet that made it chose, 27
to 42. Bitcoin Core reads other header bytes too, so for a P2PKH address it may
find valid a signature that `hw` does not, or the other way around.

```console
hw verifymsg --input signed.txt
hw verifymsg ADDRESS SIGNATURE "I control this address"
```

### The wallet

#### `hw backup`

Copies the database, to a file you name or into `~/.hw/backups`.

```console
hw backup
hw backup ~/hw-2026-10-30.sqlite
```

To restore a backup, replace `~/.hw/accounts.sqlite` with the copy, or point
`database-file` in `~/.hw/config.json` to it.

#### `hw exportsheet`

`hw exportsheet FILE` writes the six listings to a new spreadsheet, a tab each:
the accounts, the addresses, the balances, the coins, the transactions and the
pending transactions. The file is an OpenDocument spreadsheet (`.ods`), the format of
LibreOffice, which Excel and Google Sheets open too. Each tab holds every row
of its listing, in the columns of its [CSV file](#listings).

The tabs hold every account, or the one `--account` names. The addresses, the
balances, the coins and the transactions start with a column that names the
account of each row, so that one tab sorts and sums across the wallet. The
addresses of both chains share a tab, the receiving ones first.

Each cell keeps its kind, unlike in a CSV file. A label is text, whatever it
starts with, and keeps its leading zeros. An amount is a number of satoshis,
shown with separators of thousands. A time is a date, in UTC. The header of
each tab stays at the top as its rows scroll. The six listings are read at one
moment, so the tabs agree with each other.

```console
hw exportsheet wallet.ods
hw exportsheet savings.ods --account savings
```

> **Good to know**
>
> - **Amounts of Bitcoin and of Bitcoin Cash share a column** when the wallet
>   holds both. The Account column tells them apart: filter on it before you
>   sum.
> - **The spreadsheet tells your history.** `hw` makes it readable by you
>   alone. A spreadsheet program may still keep copies, such as backups and
>   recent files.

## Transaction files

`--output`, `exporttx`, `--input` and `importtx` carry a transaction in a file.
The file is a PSBT, the format of [BIP 174], so other wallets show it too. A
hardware wallet, or another wallet with the keys, can sign it in place of the
offline computer, and `broadcast --input` takes the signed file back,
finalized or not.

### What a file holds

The standard fields carry:

- The transaction.
- The whole transaction each coin comes from.
- The keys of the account in the inputs still to sign and in the outputs, with
  their derivation paths.
- The sighash to sign with.
- Once signed, the final script of each input. A signed input names no key any
  more, as the format asks of a finalized input and as other wallets write it.
  The file still says which address of the account the input spends from, in a
  field of `hw`.

What is `hw`'s own goes in the proprietary fields of the format, under the
identifier `hw`, which other software leaves aside:

- The network.
- The fee rate the transaction was built for.
- The transactions a replacement replaces.
- The label of the transaction.
- The labels of the account's addresses in it.
- The labels of the coins it spends, as the wallet has them when it writes the
  file.
- The path of the address each input spends from, such as `/0/5`. The format
  lets a signed input keep the fields it does not define, so this one stays
  once the input is signed, by `hw` or by another wallet. `hw` then knows the
  input as the account's on any computer that holds the account, such as an
  offline one that never handed out an address. It derives the address at that
  path and checks that the coin pays it.

Anyone who handles the file can change its labels. `hw` shows a label with its
control characters written out, such as `\x1b` for an escape or `\n` for a
line break, so that a label cannot rewrite what the screen shows, a recipient
for instance.

The file names the keys by the fingerprint of the seed, which a hardware wallet
needs to find its own. The wallet knows it once it created the account, signed
with it, or imported it with its origin. `preparetx` says so when it writes a
file without it. The fingerprint a signed file names is taken on its word only
by a wallet that knows none: it never replaces the one the wallet has.

### Files of other wallets

The files of other wallets are read the same way, in bytes or in base64 text, in
version 0 of the format or in version 2 ([BIP 370]). A file is signed back in
its version.

- **`importtx` and `broadcast --input`** take a file another signer returned,
  finalized or not. `hw` finalizes an input left signed and not final, and
  refuses a signature that does not verify. It keeps from the version it holds
  what such a file may leave out: the paths of its signed inputs, when another
  wallet prepared it, the fee rate and the label.
- **`broadcast --input`** also takes the signed transaction alone, in
  hexadecimal as a wallet gives it once finalized, when the wallet holds the
  transaction.
- **`signtx --input`** signs a file another wallet prepared for an account of
  the wallet, with the sighash ALL only, and only when every input that is not
  the account's is signed already. The signed file keeps every field of it,
  but those of the inputs signed that served the signers, and gains the path
  of the address each of them spends from.

Such a file names no network. The account it involves tells it. A file that
involves no account of the wallet, for `show`, `broadcast` or `signsweep`,
needs `--network`, or the `default-network` of the configuration file.

### Paths `hw` refuses

`hw` signs for the addresses of the account alone, at `/0/n` and `/1/n`. A file
that names a key of the account at any other path is refused, by `show` as
by `signtx`. An output there would be the account's, and yet never seen by it,
since it scans those two chains alone.

An output of the account more than a thousand addresses past the last one of
its chain that the wallet holds, or that the transaction spends from, is
refused too. No scan of `hw` or of another wallet looks that far, so a file
could hide the change there, at `/1/2000000000` for instance. An offline
computer that never gives out an address holds none of them. On such a
computer, a wallet with more than a thousand change addresses can see a genuine
file refused when no coin it spends is near its change. Prepare the payment
again with a recent change coin among its coins, chosen with `--coin`.

## Configuration

On its first command, `hw` creates the directory `~/.hw`. It holds the database
`accounts.sqlite` and the settings `config.json`:

| Key | Default | What it sets |
|---|---|---|
| `host` | `https://api.haskoin.com/` | The haskoin-store server |
| `default-network` | `btc` | The network of a command given no `--network`. `recover` searches `btc` and `bch` instead. |
| `unit` | `btc` | The unit of the amounts of a command given no `--unit`: `btc`, `bit` or `sat` |
| `listing-view` | `compact` | The [view of a listing](#listings) given no `--compact`, `--verbose` or `--tabular`: `compact`, `verbose` or `tabular` |
| `gap` | `20` | How many unused addresses in a row `receive` gives out and `sync` looks at |
| `change-back` | `false` | Whether `preparetx` sends the change back to an address it spends from |
| `parallel-requests` | `1` | How many requests go to the server at once. The public server takes one at a time, so keep it at 1 unless you run your own haskoin-store. |
| `database-file` | `null` | Another database, instead of `~/.hw/accounts.sqlite` |
| `pager` | `null` | The pager a listing goes through on a terminal, instead of `less` or `PAGER`. `""` is none. |

The networks are `btc`, `bch`, `btctest`, `btctest4`, `bchtest4`, `bchtest`,
`btcreg` and `bchreg`. `hw` warns about a key it does not know.

## Shell completion

The Tab key can complete what you type after `hw`. It completes the commands,
the options, and the values `hw` knows in advance, such as units, networks and
file names. It also completes [what your wallet holds](#what-it-offers).

### Set up your shell

Your shell needs a small script for it, which `hw` writes. Set it up once for
your shell, then open a new terminal.

**bash.** Add this line to `~/.bashrc`:

```bash
source <(hw --bash-completion-script hw)
```

**zsh.** Write the script to a directory of your own:

```console
mkdir -p ~/.zfunc
hw --zsh-completion-script hw > ~/.zfunc/_hw
```

Then add this directory to `fpath` in `~/.zshrc`, before `compinit` runs. With
Oh My Zsh, that is before the line that loads `oh-my-zsh.sh`. Without such a
framework, run `compinit` yourself:

```zsh
fpath=(~/.zfunc $fpath)
autoload -Uz compinit && compinit
```

**fish.** Write the script where fish looks for completions:

```console
mkdir -p ~/.config/fish/completions
hw --fish-completion-script hw > ~/.config/fish/completions/hw.fish
```

To check it, type `hw prepare` and press Tab. bash needs Tab twice. The shell
lists `preparesendall`, `preparesweep` and `preparetx`.

The script asks `hw` what to complete at each Tab. A newer `hw` completes its
new commands and options with the same script. The script finds `hw` by its
name, so `hw` must be on your `PATH`.

### What it offers

Each command is offered what it takes from your wallet:

- The names of your accounts.
- The hashes of your pending transactions. `signtx` offers those to sign,
  `broadcast` those signed and not sent yet, and `exporttx` and `deletetx`
  all of them.
- The transactions `bumpfee` can replace: your own payments on Bitcoin that
  are not in a block yet.
- Your coins. `lockcoin` offers the free ones, `unlockcoin` those you locked,
  and `--coin` those you can spend now.
- Your addresses. `label` offers the receiving addresses, `show` and `signmsg`
  all of them, and `--spend-from` those that hold coins you can spend now.
- Your transactions. `label` and `show` offer them by their txid, and the
  pending ones by the hash `pendingtxs` shows. `show` offers your coins too.

`show` also offers file names, as for any file. It leaves out a file whose name
looks like a hash, an address or a path, since `show` would read it as one.
Type `./` first, and Tab offers it.

After `--account`, `bumpfee`, `--coin` and `--spend-from` offer the
transactions, coins and addresses of that account only.

`hw` offers at most 20 values for a word. When more of them start with what
you typed, it offers none: type more of the value, or copy it from a listing.
Tab after the 8 digits of a [short id](#short-ids) completes the whole hash.
The command takes the short id as it is too.
Only the values that match are read, so a large wallet completes as fast as a
small one.

To complete what your wallet holds, `hw` reads your database. It changes
nothing in it, and it does not wait for another `hw` command, such as a long
`sync`.

### Colons in bash

bash splits a word at a colon. So a Bitcoin Cash address is offered without
its prefix, such as `bitcoincash:`, and `hw` reads it either way. A coin of a
transaction that paid you several coins completes up to its colon in bash,
which bash writes `\:`. Press Tab again to list the coins, or type the index.
The backslash goes when the command runs, as it does in a name with a space,
`my\ savings`.

## Upgrading from an earlier version

Use `hw` 0.10. Never use version 0.8 or earlier.

[CHANGELOG.md](CHANGELOG.md) lists what changed in each version, and the
commands and options that were renamed or removed.

`hw` 0.10 upgrades the database of 0.8 and 0.9 on its first command. It backs
the database up to `~/.hw/migration-backups` first. Do not run an older `hw`
on the upgraded database. `hw` 0.8 would rewrite parts of it in a format that
0.10 cannot read.

### Accounts derived wrongly

Earlier versions have known bugs in the accounts they derive. Your funds are
not lost, but take care when you move them:

- Versions before 0.8.0 derive the wrong account when the wallet holds more
  than one mnemonic. The first account of a second mnemonic comes out as
  `/44'/0'/1'` instead of `/44'/0'/0'`. Write down the derivation path of each
  account you use, so that you can find it again.
- Version 0.7.0 derives the wrong account from a split mnemonic. Use the
  0.7.0 binary to move the funds to a new account created with 0.10.

### A mnemonic split into parts

Versions up to 0.9 could split a mnemonic with `mnemonic --split` into parts
that were all needed to rebuild it. `hw` 0.10 neither makes nor reads such
parts. Use `hw` 0.9 to move the funds of such a mnemonic to an account of
`hw` 0.10.

Run `hw` 0.9 with a `HOME` of its own, such as `HOME=$(mktemp -d)`. It then
makes a wallet apart and leaves the upgraded one alone. There,
`createacc --split N` asks for each of the N parts. Sync the account with
`syncacc`. Then pay its funds to an address of `hw` 0.10 with `preparetx`,
`signtx --split N` and `sendtx`.

[Backups in shares](#hw-mnemonic) take the place of parts. A chosen number of
shares is enough, and a wrong share is told.

### A passphrase with accents

A passphrase with accents or other non-ASCII letters is normalized as BIP39
asks since `hw` 0.10. Earlier versions did not normalize it, so their accounts
come from another seed than other wallets find. `hw` 0.10 still finds and signs
those accounts, and warns. Move their funds to an account created with 0.10.

### A wallet readable by others

`hw` 0.9 and earlier let the system give `~/.hw` and the database the
permissions of your umask, which often lets the other users of the computer
read them. `hw` 0.10 makes a new wallet private, and leaves an existing one as
it is. Make yours private:

```console
chmod 700 ~/.hw
chmod 600 ~/.hw/accounts.sqlite
```

### Transaction files of an earlier version

`hw` 0.10 reads no JSON transaction file of an earlier `hw`. Prepare the
transaction again.

[BIP39]: https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
[SLIP39]: https://github.com/satoshilabs/slips/blob/master/slip-0039.md
[BIP44]: https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki
[BIP84]: https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki
[BIP125]: https://github.com/bitcoin/bips/blob/master/bip-0125.mediawiki
[BIP 137]: https://github.com/bitcoin/bips/blob/master/bip-0137.mediawiki
[BIP 174]: https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki
[BIP 321]: https://github.com/bitcoin/bips/blob/master/bip-0321.mediawiki
[BIP 329]: https://github.com/bitcoin/bips/blob/master/bip-0329.mediawiki
[BIP 370]: https://github.com/bitcoin/bips/blob/master/bip-0370.mediawiki
[BIP 380]: https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki
[haskoin-store]: https://github.com/haskoin/haskoin-store
[Stack]: https://docs.haskellstack.org
