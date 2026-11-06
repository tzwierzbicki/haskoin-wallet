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
- [Signing on an offline computer](#signing-on-an-offline-computer), or with a
  hardware wallet, through [transaction files](#transaction-files).
- [Coin control](#hw-preparetx), and [fee rates](#hw-fees) read from recent
  blocks.
- [Fee bumping](#hw-bumpfee) on Bitcoin ([BIP125]).
- [Sweeps](#sweeps) of coins whose private keys you hold.
- [Message signing](#messages) in the Bitcoin Signed Message format.
- [Labels and accounts](#hw-exportacc) shared with other wallets.
- [JSON answers](#options-of-every-command) from every command, for scripts.

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
hw sendtx TXID                              # the txid signtx shows
hw sync
```

`preparetx` prints the nosigHash of the new transaction, and the commands to
run next. A nosigHash is a hash of the transaction without its signatures. It
names the transaction until it is signed. `signtx` then prints its txid, the id
the network knows it by, and `pendingtxs` shows the txid from then on. The
commands take either hash.

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
   hw reviewtx unsigned.psbt
   hw signtx --input unsigned.psbt --output signed.psbt
   ```

4. On the online computer, send it:

   ```console
   hw sendtx --input signed.psbt
   hw sync
   ```

A hardware wallet, or another wallet with the keys, can sign the file in place
of the offline computer. See [Transaction files](#transaction-files).

#### Check an address before you give it out

An online computer that was tampered with can show an address of its own in
place of one of the account. The offline computer holds the same account and
derives the same addresses, without the mnemonic.

```console
hw receive                                  # on the online computer: /0/7 and the address
hw checkaddr /0/7                           # on the offline computer: the same address?
```

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
  accounts.
- **An online computer can show a wrong address.** One that was tampered with
  shows an address of its own, and the payer pays the attacker.
  [Check an address](#check-an-address-before-you-give-it-out) with `checkaddr`
  on the offline computer before you give it out.
- **`hw` takes the server's word for the chain.** It checks that the server
  answers about the addresses and transactions it asked about and no others,
  and that the counts agree, but it cannot tell a hidden payment from none.
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
| `--unit UNIT` | Sets the unit of the amounts shown, and of the amounts typed without a unit: `btc`, `bit` or `sat`. The default is `btc`. A bit is 100 satoshis. |
| `--json` | Prints the answer as JSON. Questions and warnings go to stderr. |
| `--no-pager` | Prints a listing as it is, through no pager. |

### Amounts

**An amount without a unit is in bitcoins.** `ADDRESS:1000` asks for 1000 BTC,
not 1000 satoshis. Write the unit, as in `ADDRESS:1000sat`, or give
`--unit sat`.

An amount can end with `btc`, `bit`, `bits`, `sat` or `sats`. Bitcoins take up
to 8 decimals, bits up to 2, and satoshis none. `ADDRESS:0.00001`,
`ADDRESS:10bits` and `ADDRESS:1000sat` pay the same.

### Listings

On a terminal, a listing such as `hw txs` goes through `less`. The arrow keys
scroll it, `/` searches it and `q` quits it. A listing that fits on the screen
is shown as it is. `HW_PAGER`, the key `pager` of `config.json` or `PAGER` name
another pager.

Scripts read a listing a page at a time with `--max`, then `--after` the last
row they got.

```console
hw txs --max 20 --json
hw txs --max 20 --after TXID --json
```

### Files

**`hw` never overwrites a file.** `--output` and `backup` refuse a file that
exists, even one that appeared while the command ran. `signtx` and `signsweep`
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
| [`descriptor`](#hw-descriptor) | Show an account as an output descriptor |
| [`accounts`](#hw-accounts) | Show the accounts and their balances |
| [`renameacc`](#hw-renameacc) | Rename an account |
| [`deleteacc`](#hw-deleteacc) | Delete an account from the wallet |
| [`sync`](#hw-sync) | Update the accounts from the server |
| **[Addresses](#addresses)** | |
| [`receive`](#hw-receive) | Get a new address to receive a payment |
| [`addresses`](#hw-addresses) | List the receiving addresses, or the change addresses |
| [`balances`](#hw-balances) | List the addresses that hold coins |
| [`label`](#hw-label) | Label a receiving address or a transaction |
| [`checkaddr`](#hw-checkaddr) | Show the address at a derivation path, to check it offline |
| **[Coins](#coins)** | |
| [`coins`](#hw-coins) | List the coins of an account |
| [`lockcoin`](#hw-lockcoin) | Keep a coin from being spent |
| [`unlockcoin`](#hw-unlockcoin) | Free a locked coin |
| **[Payments](#payments)** | |
| [`fees`](#hw-fees) | Show the fee rates of recent blocks |
| [`preparetx`](#hw-preparetx) | Prepare a payment, with coin control if you like |
| [`preparesendall`](#hw-preparesendall) | Prepare a payment of all the coins of an account |
| [`signtx`](#hw-signtx) `!` | Sign a pending transaction or a transaction file |
| [`sendtx`](#hw-sendtx) | Send a signed transaction to the network |
| [`bumpfee`](#hw-bumpfee) | Replace a stuck payment with one that pays a higher fee |
| **[Transactions](#transactions)** | |
| [`txs`](#hw-txs) | List the transactions of an account |
| [`pendingtxs`](#hw-pendingtxs) | List the pending transactions |
| [`reviewtx`](#hw-reviewtx) | Show the transaction in a file |
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

#### `hw recover`

Finds the accounts of a mnemonic, or of a backup in shares, that were paid. It
adds them and syncs them. It needs the mnemonic and the server at the same time.

```console
hw recover --network btc --account-gap 20
```

The accounts are named `btc0`, `btc1` and so on for the `bip44` accounts, and
`btc0-segwit`, `btc1-segwit` and so on for the `bip84` ones. The search stops
after 10 accounts in a row without a payment, or the number `--account-gap`
gives, for each kind of account apart. After a thousand used addresses in a row,
it asks whether to keep looking, as [`sync`](#hw-sync) does. On a terminal, a
line tells which account it is looking at.

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
| A `pkh(...)` or a `wpkh(...)` descriptor | It is a `bip44` or a `bip84` account, as the descriptor says. |
| A descriptor of another kind, such as `sh(wpkh(...))` | It is refused, and the import goes on. |
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

#### `hw descriptor`

Shows each account as an output descriptor, the format of [BIP 380] in which
other wallets take an account without its private key. Paste it into a wallet
that takes a descriptor, to watch the account there. `--account` shows one
account.

```console
hw descriptor --account vault               # for a wallet that takes a descriptor
hw descriptor --account spending            # a bip84 account, with its zpub for one that takes a key
```

A `bip44` account gives `pkh([bc560f95/44h/0h/0h]xpub.../<0;1>/*)#vq597j87`. It
carries the public key of the account, the fingerprint of its seed and its
derivation path, the two chains of addresses, and a checksum. When `hw` does not
know the fingerprint, the descriptor names no origin, which a hardware wallet
needs, and the answer says so.

A `bip84` account gives `wpkh([bc560f95/84h/0h/0h]xpub.../<0;1>/*)#5nktextl`, the
script function naming the kind. It is shown with its key as a `zpub` as well,
or a `vpub` on a test network, in the row `SLIP-132`. Some wallets take only
that form for a native SegWit account, and make a legacy wallet of an `xpub`.
The key is the same, only its prefix differs.

[`importacc`](#hw-importacc) reads a descriptor back.

#### `hw accounts`

Shows the accounts with their balances. Each takes two lines: its name, network
and derivation path, then what it can spend, what is unconfirmed and how many
coins it holds. The spendable balance leaves out locked coins, coinbase coins
not yet mature, and unconfirmed payments from other wallets.

`--verbose` adds the fingerprint of the seed, the addresses, when the account
was created, and its confirmed and unconfirmed balances.

```console
hw accounts
hw accounts --verbose
hw accounts --account savings --unit sat
```

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

`--full` downloads everything again, which is rarely needed.

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

Gives the next receiving address of an account.

```console
hw receive --label "invoice 42"
```

> **Good to know**
>
> - **`receive` stops after 20 unused addresses in a row.** A wallet restored
>   from the mnemonic would not look further. Reuse one of them, or raise `gap`
>   in the [configuration file](#configuration).

#### `hw addresses`

Lists the receiving addresses, the newest first, or the change addresses with
`--change`. Each takes two lines: its path, the address and its label, then what
it can spend and what it received, or that it is unused. `--verbose` adds its
balances, its coins and when it was created.

```console
hw addresses
hw addresses --change
hw addresses --verbose
hw addresses --max 10 --after ADDRESS
```

#### `hw balances`

Lists the addresses that hold coins, the largest total first.

```console
hw balances
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
the history by its txid. A pending transaction keeps its label in its file,
through signing on an offline computer, and in the history once the network has
it. A label set while the file is away stays when the signed file comes back.

`preparetx`, `preparesendall` and `preparesweep` take a `--label`, and `bumpfee`
gives a replacement the labels of the transactions it replaces. A transaction
between two accounts of the wallet gets the label in both.

#### `hw checkaddr`

`hw checkaddr PATH` shows the address of an account at a derivation path, `/0/i`
for the receiving address `i` and `/1/i` for the change address `i`, derived
from the public key. Any other path is refused. It needs no mnemonic and no
server, hands out no address, and stores nothing. Run it on the offline computer
to check an address the online computer gave out.

```console
hw checkaddr /0/7
hw checkaddr /1/2 --account vault
```

### Coins

#### `hw coins`

Lists the coins of an account. Each takes three lines:

1. Its value with its state, spendable or locked, and its confirmations.
2. The address that holds it, with its label, or `change address` when it is
   one.
3. The coin as `TXID:INDEX`.

A coin paid by a transaction you labelled shows the label on a line of its own,
below the first, as `txs` does. The change of a payment you made shows no label.
The label says what the payment was for, not what its change is.

```console
hw coins
hw coins --locked
```

> **Good to know**
>
> - **Unconfirmed payments from others cannot be spent.** The sender can still
>   replace them until they confirm. The unconfirmed coins of your own
>   transactions can be spent: their change, and what one of your accounts pays
>   another.

#### `hw lockcoin`

Keeps a coin from being spent.

```console
hw lockcoin TXHASH:1
```

#### `hw unlockcoin`

Frees a coin locked with `lockcoin`.

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
| `--coin TXID:INDEX` | Chooses a coin to spend. Every coin chosen is spent. Repeat it for several coins. |
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
> - **An amount without a unit is in bitcoins.** See [Amounts](#amounts).
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

Signs a pending transaction by its nosigHash, or a transaction file. It asks for
the mnemonic and passphrase of the account, or for the shares of its backup.

```console
hw signtx NOSIGHASH
hw signtx --input unsigned.psbt --output signed.psbt
```

#### `hw sendtx`

Shows a signed transaction, asks, and sends it. It then waits up to ten seconds
for the server to see it.

```console
hw sendtx TXID
hw sendtx --input signed.psbt
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
hw sendtx TXID                              # the txid of the replacement
```

The extra fee comes out of the change. When the change cannot pay it, `hw`
offers to add confirmed coins of the account, or to take what is missing from
what the recipients receive, asking which of them pays.

### Transactions

#### `hw txs`

Lists the transactions of an account, as of the last sync. Each takes three
lines:

1. Its time, in the time zone of the computer, its type, its amount and its
   status.
2. The address paid.
3. The txid.

An unconfirmed transaction shows its fee rate. A transaction with a label shows
it on a line of its own, below the first.

`--verbose` shows the label as a field, every address with its amount, path and
label, and the fee. A change address the transaction spends from or pays says
`change address` in place of a label. An address the transaction spends from
ends with the labels of the coins spent, as `coins` shows them, after the word
`from`.

```console
hw txs
hw txs --verbose
hw txs --max 20 --after TXID
```

#### `hw pendingtxs`

Lists the pending transactions: prepared, signed, or sent and not yet seen by
`sync`. They take three lines each, as in `txs`. Each is shown by its nosigHash
until it is signed, then by its txid. A conflicted one can no longer be sent.

```console
hw pendingtxs
hw pendingtxs --verbose
```

> **Good to know**
>
> - **A pending transaction locks its coins.** They stay locked until `sync`
>   sees the transaction on the network, or until `deletetx` deletes it. Delete
>   the transactions you abandon.

#### `hw reviewtx`

`hw reviewtx FILE` shows the transaction in a file and the account it belongs
to, without changing the wallet. The labels it shows are those the file carries,
so an offline computer sees the labels of the online wallet.

```console
hw reviewtx unsigned.psbt
```

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
its nosigHash, or by its txid once it is signed. A transaction you sent can be
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
hw sendtx TXID
```

| Coins swept | Option |
|---|---|
| The coins at an address | `--sweep-from ADDRESS` |
| The coins at the addresses listed in a file | `--address-file FILE` |
| The coins at the addresses of the keys in a file | `--key-file FILE` |
| A coin given by its transaction and index | `--coin TXID:INDEX` |

A sweep that pays no address of this wallet needs `--network` and `--output`,
and no account. It goes from file to file:

```console
hw preparesweep --sweep-from OLDADDRESS --sweep-to ADDRESS --network btc --fee-rate median --output sweep.psbt
hw signsweep --input sweep.psbt --key-file keys.txt --output signed.psbt
hw sendtx --input signed.psbt
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
of one key, whatever header byte the wallet that made it chose.

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

## Transaction files

`--output`, `exporttx`, `--input` and `importtx` carry a transaction in a file.
The file is a PSBT, the format of [BIP 174], so other wallets show it too. A
hardware wallet, or another wallet with the keys, can sign it in place of the
offline computer, and `sendtx --input` takes the signed file back, finalized or
not.

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

The file names the keys by the fingerprint of the seed, which a hardware wallet
needs to find its own. The wallet knows it once it created the account, signed
with it, or imported it with its origin. `preparetx` says so when it writes a
file without it.

### Files of other wallets

The files of other wallets are read the same way, in bytes or in base64 text, in
version 0 of the format or in version 2 ([BIP 370]). A file is signed back in
its version.

- **`importtx` and `sendtx --input`** take a file another signer returned,
  finalized or not. `hw` finalizes an input left signed and not final, and
  refuses a signature that does not verify. It keeps from the version it holds
  what such a file may leave out: the paths of its signed inputs, when another
  wallet prepared it, the fee rate and the label.
- **`sendtx --input`** also takes the signed transaction alone, in hexadecimal
  as a wallet gives it once finalized, when the wallet holds the transaction.
- **`signtx --input`** signs a file another wallet prepared for an account of
  the wallet, with the sighash ALL only, and only when every input that is not
  the account's is signed already. The signed file keeps every field of it,
  but those of the inputs signed that served the signers, and gains the path
  of the address each of them spends from.

Such a file names no network. The account it involves tells it. A file that
involves no account of the wallet, for `reviewtx`, `sendtx` or `signsweep`,
needs `--network`, or the `default-network` of the configuration file.

### Paths `hw` refuses

`hw` signs for the addresses of the account alone, at `/0/n` and `/1/n`. A file
that names a key of the account at any other path is refused, by `reviewtx` as
by `signtx`. An output there would be the account's, and yet never seen by it,
since it scans those two chains alone.

## Configuration

On its first command, `hw` creates the directory `~/.hw`. It holds the database
`accounts.sqlite` and the settings `config.json`:

| Key | Default | What it sets |
|---|---|---|
| `host` | `https://api.haskoin.com/` | The haskoin-store server |
| `default-network` | `btc` | The network of a command given no `--network` |
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
  `sendtx` those signed and not sent yet, and `exporttx` and `deletetx` all of
  them.
- The transactions `bumpfee` can replace: your own payments on Bitcoin that
  are not in a block yet.
- Your coins. `lockcoin` offers the free ones, `unlockcoin` those you locked,
  and `--coin` those you can spend now.
- Your addresses. `label` offers the receiving addresses, `signmsg` all of
  them, and `--spend-from` those that hold coins you can spend now.
- Your transactions. `label` offers them by their txid, and the pending ones
  by the hash `pendingtxs` shows.

After `--account`, `bumpfee`, `--coin` and `--spend-from` offer the
transactions, coins and addresses of that account only.

`hw` offers at most 20 values for a word. When more of them start with what
you typed, it offers none: type more of the value, or copy it from a listing.
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

Versions up to 0.9 split a mnemonic with `mnemonic --parts` into parts that
were all needed to rebuild it. `hw` 0.10 makes no such parts, and reads them
with `--parts`. Give `--parts N` to every command that asks for a mnemonic
split so, and `hw` asks for each of the N parts.

[Backups in shares](#hw-mnemonic) take the place of parts. A chosen number of
shares is enough, and a wrong share is told.

### A passphrase with accents

A passphrase with accents or other non-ASCII letters is normalized as BIP39
asks since `hw` 0.10. Earlier versions did not normalize it, so their accounts
come from another seed than other wallets find. `hw` 0.10 still finds and signs
those accounts, and warns. Move their funds to an account created with 0.10.

### Transaction files of an earlier version

`hw` 0.10 reads no JSON transaction file of an earlier `hw`. Prepare the
transaction again.

[BIP39]: https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
[SLIP39]: https://github.com/satoshilabs/slips/blob/master/slip-0039.md
[BIP44]: https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki
[BIP84]: https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki
[BIP125]: https://github.com/bitcoin/bips/blob/master/bip-0125.mediawiki
[BIP 174]: https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki
[BIP 329]: https://github.com/bitcoin/bips/blob/master/bip-0329.mediawiki
[BIP 370]: https://github.com/bitcoin/bips/blob/master/bip-0370.mediawiki
[BIP 380]: https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki
[haskoin-store]: https://github.com/haskoin/haskoin-store
[Stack]: https://docs.haskellstack.org
