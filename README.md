# Haskoin Wallet

[![Hackage](https://img.shields.io/hackage/v/haskoin-wallet.svg)](https://hackage.haskell.org/package/haskoin-wallet)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jprupp/haskoin-wallet/blob/master/LICENSE)

Haskoin Wallet (`hw`) is a command-line wallet for Bitcoin and Bitcoin Cash,
made for cold storage. The mnemonic can stay on an offline computer, while an
online computer watches the funds and prepares the payments. A payment is
prepared without any key, signed on the offline computer, and sent from the
online one. The two computers exchange files: the public keys of the wallet,
and the transactions to sign.

`hw` keeps a wallet for each mnemonic, and portfolios in each wallet. A
portfolio holds every account of the mnemonic at one account index, on every
network and of both types of address. The commands work in one portfolio, the
active one. With one mnemonic, that is `portfolio0`, and you never create or
pick an account. You only say which network and which type of address you
want.

`hw` keeps its wallets in a SQLite database, and asks a [haskoin-store] server
about the chain. The database holds public keys only. Your mnemonic is never
written to disk. `hw` suits small wallets managed by hand.

What `hw` does:

- [Mnemonics](#hw-newportfolio) as [BIP39] describes, or backups in shares as
  [SLIP39] describes.
- [Portfolios](#where-a-command-works) that hold the accounts of a mnemonic.
  [BIP44] derives those with legacy addresses, and [BIP84] those with native
  SegWit addresses, which pay lower fees. A payment spends both together.
- [QR codes of addresses](#hw-receive) on the terminal, for a payer to scan.
- [Signing on an offline computer](#signing-on-an-offline-computer), or with a
  hardware wallet, through [transaction files](#transaction-files).
- [Payments in one command](#hw-send), for a wallet whose mnemonic you type
  on the online computer.
- [Coin control](#hw-preparetx), and [fee rates](#hw-fees) read from recent
  blocks.
- [Fee bumping](#hw-bumpfee) on Bitcoin ([BIP125]).
- [Sweeps](#sweeps) of coins whose private keys you hold.
- [Message signing](#messages) in the Bitcoin Signed Message format.
- [Labels](#hw-exportwallet) in the format of [BIP 329], which other wallets
  read and write, and an [output descriptor](#hw-portfolios) of each account,
  to watch it in another wallet.
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
  - [Where a command works](#where-a-command-works)
  - [Amounts](#amounts)
  - [Listings](#listings)
  - [Short ids](#short-ids)
  - [Files](#files)
  - [One command at a time](#one-command-at-a-time)
- [Commands](#commands), each with its examples and what is good to know
  - [Wallets and portfolios](#wallets-and-portfolios)
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

`stack test` runs most tests against a store that the tests fake, and needs
nothing else. Some tests run `hw` against real nodes and real haskoin-store
servers, on regtest chains of Bitcoin and Bitcoin Cash in a container. They
cover what a fake cannot show: reorganizations, replaced payments, and what
the server answers about a transaction no node takes. They are skipped,
and reported as pending with the reason, until you build their image with
`regtest/build.sh services`, which needs [podman]. See
[regtest/README.md](regtest/README.md).

The Tab key can complete what you type after `hw`, once you set up
[shell completion](#shell-completion).

## Quick start

Read [Keep your funds safe](#keep-your-funds-safe) before you put real funds in
the wallet.

The examples in this README write `ADDRESS`, `NOSIGHASH`, `TXID`, `HASH` and
`TXHASH` where you put your own values.

### A first payment

```console
hw newportfolio                             # write the words down
hw receive --label "first deposit"          # give this address to the payer
hw sync                                     # once the payment is sent
hw preparetx ADDRESS:0.0005 --fee-rate median
hw signtx NOSIGHASH                         # type the mnemonic
hw broadcast TXID                           # the txid signtx shows
hw sync
```

`newportfolio` makes a mnemonic, shows it once, and creates `wallet0` with its
`portfolio0`. It is the first portfolio of the database, so it becomes the
active one, and every command above works in it. The portfolio has an account
for every network and for both types of address from the start, so there is no
account to create. `receive`
gives a legacy address on Bitcoin. Two options ask for another kind of
address, and `setdefault` changes what `receive` gives without them:

```console
hw receive --type bip84                     # a native SegWit address, for lower fees
hw receive --network bch                    # an address on Bitcoin Cash
hw setdefault --type bip84                  # native SegWit from now on
```

[Where a command works](#where-a-command-works) explains wallets, portfolios,
the active portfolio and these options.

`preparetx` prints the nosigHash of the new transaction, and the commands to
run next. A nosigHash is a hash of the transaction without its signatures. It
names the transaction until it is signed. `signtx` then prints its txid, the id
the network knows it by, and `pendingtxs` shows the txid from then on. The
commands take either hash, in full or by its [short id](#short-ids), its first 8
digits, which the next steps and the listings show.

[`send`](#hw-send) takes the three steps of the payment in one command. It
shows the transaction, asks for the mnemonic, and sends:

```console
hw send ADDRESS:0.0005 --fee-rate median    # check the payment, type the mnemonic
hw sync
```

It makes the same payment as the three commands. It asks for the mnemonic on
the online computer, so it is not for funds whose mnemonic you keep offline.

### Signing on an offline computer

1. On the offline computer, create the wallet and export its public keys:

   ```console
   hw newportfolio --dice
   hw exportwallet wallet.jsonl
   ```

2. On the online computer, import the wallet and prepare a payment to a file:

   ```console
   hw importwallet wallet.jsonl
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

The file of `exportwallet` holds public keys and no private key. The online
computer can then give out addresses, watch the funds and prepare payments,
and it never sees the mnemonic.

Each computer keeps its own [active portfolio](#the-active-portfolio). On
both, the first portfolio created becomes the active one, so both work in
`portfolio0`. `signtx` works whichever portfolio is active. It finds the
portfolio from the transaction.

A hardware wallet, or another wallet with the keys, can sign the file in place
of the offline computer. See [Transaction files](#transaction-files).

#### Check an address before you give it out

An online computer that was tampered with can show an address of its own in
place of one of the wallet. The offline computer holds the same portfolio and
derives the same addresses, without the mnemonic. It did not give the address
out, so it shows it cut, with a dot for each character of its middle. Compare
the two ends with the address the online computer shows.

```console
hw receive                                  # on the online computer: bip44/0/7 and the address
hw show bip44/0/7                           # on the offline computer: the same ends?
```

Give `show` the path as `receive` wrote it, with its type: `bip44/0/7`, or
`bip84/0/7` for an address of `receive --type bip84`. The type names the
account, since each account of a network has an address at `/0/7`. Give
`show` the `--network` you gave `receive` too. Both computers must also have
the same portfolio active, since each command works in the active one.
`portfolios` marks it with a `*` on each.

The ends are enough, since nobody can make another address with the same ends.
The middle is hidden so that nobody pays an address that the wallet never gave
out. See [`show`](#hw-show).

## Keep your funds safe

- **The mnemonic is the only key to the funds.** `hw` never stores it. Without
  the database, the mnemonic still spends the funds, and
  [`recover`](#hw-recover) finds the portfolios again. Without the mnemonic,
  the funds are lost.
- **The mnemonic is in memory while a command runs.** `hw` cannot wipe it from
  there, so it protects its memory as a whole. It writes no core file when it
  crashes. On Linux, no other program of yours can read its memory or attach to
  it as a debugger. The administrator of the computer still can. Type a
  mnemonic on an offline computer if you can.
- **`send` asks for the mnemonic on the online computer.** It needs the
  server, so it cannot run offline. A computer that was tampered with can
  steal a mnemonic typed on it. For funds in cold storage, pay in three steps
  and [sign on the offline computer](#signing-on-an-offline-computer).
- **The database and the wallet files are private.** They hold no private key.
  But the public keys in them reveal every address and balance of your
  portfolios. `hw` makes `~/.hw` and the database yours alone to read. A
  directory `~/.hw` made by `hw` 0.9 or earlier keeps the permissions it had,
  and so does the old database in it.
  [Make them private](#a-wallet-readable-by-others).
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
  asks the same after every 20 portfolios the server reports paid. Run your own
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
| `--unit UNIT` | Sets the unit of the amounts shown, and of the amounts typed without a unit: `btc`, `bit` or `sat`. A bit is 100 satoshis. The default is the key `unit` of the [configuration](#configuration), or `btc`. [`setdefault --unit`](#hw-setdefault) sets that key. |
| `--json` | Prints the answer as JSON. Questions and warnings go to stderr. |
| `--no-pager` | Prints a listing as it is, through no pager. |

### Where a command works

`hw` never asks you to create an account or to name one. It keeps a wallet for
each mnemonic, and portfolios in each wallet. A portfolio holds every account
of the wallet at one account index, on every network and of both types of
address. The commands work in one portfolio, the active one, which the
database keeps. With one mnemonic, that is `portfolio0` of `wallet0`, and you
never name it. You only say the network and the type of address you want, and
each has a default.

| Term | What it is |
|---|---|
| Wallet | The master key of one mnemonic. A mnemonic with a passphrase gives another master key, so it is another wallet. |
| Portfolio | All the accounts of a wallet that have the same account index. `portfolio0` holds the accounts at index 0. |
| Active portfolio | The portfolio the commands work in. The first portfolio created becomes active, and only [`switch`](#hw-switch) changes it. |
| Account | One derivation path, `m/purpose'/coin type'/index'`, on one network. |
| Type | The type of address of an account: `bip44` or `bip84`. |

**Why portfolios.** A mnemonic gives one account for each purpose, each coin
and each index. A wallet built on single accounts makes you create each one
and name it. Every command must then be told which account it means. Most
people only ever use the first index. So `hw` groups the accounts by their
index, and creates all the accounts of an index at once. A new portfolio is
ready for Bitcoin and Bitcoin Cash, with legacy and native SegWit addresses,
and there is nothing to set up. A second portfolio is for funds you keep apart,
such as savings. A payment spends the coins of one portfolio only, so it never
mixes the two.

The design gives up two things. An account has no name of its own, since the
network and the type already tell it apart in its portfolio. A key alone, such
as an extended public key from another wallet, cannot be imported either. It
is one account, and it does not say which portfolio it belongs to.

#### What a portfolio holds

A portfolio is created whole. It has twelve accounts, which come from five
keys. In these paths, `i` is the index of the portfolio:

| Path | Type | Networks |
|---|---|---|
| `m/44'/0'/i'` | `bip44` | `btc` |
| `m/84'/0'/i'` | `bip84` | `btc` |
| `m/44'/145'/i'` | `bip44` | `bch` |
| `m/44'/1'/i'` | `bip44` | Every test network: `btctest`, `btctest4`, `btcreg`, `bchtest`, `bchtest4` and `bchreg` |
| `m/84'/1'/i'` | `bip84` | The test networks of Bitcoin: `btctest`, `btctest4` and `btcreg` |

The type says which addresses an account has:

| Type | Addresses | On Bitcoin, they start with |
|---|---|---|
| `bip44`, the default | Legacy P2PKH | `1` |
| `bip84` | Native SegWit P2WPKH | `bc1q` |

A payment that spends `bip84` coins pays about 40% less in fees. Other wallets
make `bip84` accounts by default. `hw` can pay any kind of address from either
type.

**The test networks share their keys.** Every test network uses the coin type
`1'`. So `btctest` and `btctest4`, for example, derive the same keys and the
same addresses. This causes no mix-up. `hw` keeps the addresses, the coins and
the transactions of each network apart, and knows which addresses it gave out
on each.

**Names and descriptions.** Wallets and portfolios have fixed names that
follow their index: `wallet0`, `wallet1`, `portfolio0`, `portfolio1` and so on.
You cannot rename them. The name of a portfolio is its account index, so it is
the same on every computer that holds the wallet. A name you chose would have
to be kept in step between the offline and the online computer. A description
says what a wallet or a portfolio is for, in your own words.
[`label`](#hw-label) sets it, given the description alone, and `portfolios`
shows it.

A new wallet takes the lowest free index. The other wallets keep their names
when one is deleted. When `wallet0` is deleted, `wallet1` is still `wallet1`,
and the next new wallet becomes `wallet0` again. The fingerprint of its master
key is what tells a wallet apart for good, and `portfolios --verbose` shows
it.

#### The active portfolio

The database keeps one portfolio as the active one. Most commands work in it.
`receive` gives out its addresses, `txs` lists its transactions, and
`send` and `preparetx` pay from it. None of them takes an option to name
another portfolio.

The first portfolio created in a database becomes the active one, whether
`newportfolio`, `recover` or `importwallet` creates it. A command that creates
several makes the first one active: `portfolio0` of a new wallet, or else the
lowest index it creates. A portfolio created later does not become active. The
answer of the command says which portfolio is still active, and its next step
is the `switch` to the new one. After that, only [`switch`](#hw-switch)
changes the active portfolio:

```console
hw switch 1                                 # portfolio1 of the same wallet
hw switch 0 --wallet 1                      # portfolio0 of wallet1, another mnemonic
```

The portfolio stays active for every command until the next `switch`.
[`portfolios`](#hw-portfolios) marks it with a `*`:

```console
$ hw portfolios
* portfolio0  wallet0  "daily spending"
    created 11 Nov 2026
    btc  spendable 0.00000000 btc, no coins, 1 address

  portfolio1  wallet0  "savings"
    created 11 Nov 2026
    btc  spendable 0.00000000 btc, no coins, 1 address
```

**Why one active portfolio.** You always know where `hw` works, and you never
type it. An option on every command would make you name the portfolio each
time. A slip of the finger would then give out an address of the wrong
portfolio, or pay from it. A default in the configuration file is hard to keep
track of. It sits in another file than the portfolios, so it can name a
portfolio that was deleted, or one of another database. The active portfolio
is kept in the database itself, beside the portfolios. So it always names one
that exists. `switch` refuses a portfolio the database does not have.
`deleteportfolio` refuses the active portfolio. `deletewallet` refuses its
wallet while the database has another wallet to switch to.

The price is a state that the command line does not show. `portfolios` shows
it, and so do the answers that create a portfolio. A script that needs a given
portfolio runs `switch` before its commands, and the switch stays after the
script ends.

`switch` names a wallet and a portfolio by their index, `0` for `wallet0`, or
by their name, as in `hw switch portfolio1`. An index is below 2147483648,
which is 2^31. The index of a portfolio is the last step of the paths of its
accounts, and [BIP32] has that many hardened steps. `hw` refuses a larger
index.

#### The two options

Two options say where a command works in the active portfolio. They pick one
of its accounts. Each has a default, and a key of the
[configuration](#configuration) changes it.

| Option | What it picks | Default | Configuration key |
|---|---|---|---|
| `--network NET`, or `-n` | `btc` or `bch`, or another [network you show](#shown-and-hidden-networks) | `btc` | `default-network` |
| `--type TYPE`, or `-t` | `bip44` or `bip84` | `bip44` | `default-type` |

```console
hw receive                                  # the active portfolio, on btc, bip44
hw receive --type bip84                     # the same portfolio, a native SegWit address
hw receive --network bch                    # the same portfolio, on Bitcoin Cash
```

[`setdefault`](#hw-setdefault) writes the defaults to the configuration file,
so that you do not have to edit it:

```console
hw setdefault --type bip84                  # native SegWit addresses from now on
hw setdefault --network bch                 # work on Bitcoin Cash
```

The default type is `bip44`, the one type every network has.

The network and the type keep their defaults in the configuration file. They
are a preference, the same for every portfolio. They also never name something
that a command can delete. The portfolio is part of the wallet, so the
database keeps it.

**Bitcoin Cash has `bip44` only.** The networks of Bitcoin Cash, `bch`,
`bchtest`, `bchtest4` and `bchreg`, have no SegWit. They take `bip44` whatever
`default-type` says, and they refuse `--type bip84`. So a default of `bip84`
is safe for a user of both chains. `hw receive --network bch` still gives an
address of Bitcoin Cash.

#### Shown and hidden networks

A portfolio has accounts on eight networks, and most people work on two of
them. So `hw` shows some networks and hides the others. A new configuration
shows `btc` and `bch`, and hides the six test networks. The answers then hold
no line about a test network that you never use.

A hidden network is not disabled. `hw` still keeps it up to date:

| What `hw` does | On a shown network | On a hidden network |
|---|---|---|
| `sync` and `recover` work on it | Yes | Yes |
| The status line of `sync` and `recover` names it | Yes | Yes |
| The answers of the commands show it | Yes | No |
| `--network` can name it | Yes | No |

`sync` updates the hidden networks with the others, and `recover` still finds
a portfolio that was paid on one of them. So a network is ready the day you
show it, with nothing to catch up on. The status line names every network,
since it tells what you are waiting for. Everything else leaves the hidden
networks out. A listing has no section for them, `portfolios` has no line for
them, and `sync` and `recover` say nothing of them once they are done, not
even that their server is down. The JSON answers, the CSV files, the
spreadsheet of `exportsheet` and [tab completion](#shell-completion) leave
them out as the screen does.

[`setdefault`](#hw-setdefault) shows and hides networks, so that you do not
have to edit the configuration file:

```console
hw setdefault --show-network btctest4       # show a hidden network
hw setdefault --hide-network bch            # hide a network
```

A command that names a hidden network is refused. It says which networks `hw`
shows, and gives the command that shows this one too:

```console
$ hw receive --network btctest4
Error: The network btctest4 is hidden. hw shows btc and bch. To show btctest4 too, run: hw setdefault --show-network btctest4
```

A command that is given an address, a coin or a transaction looks for it on
the shown networks. When the wallet has it on hidden networks only, the
command is refused the same way. Answering that the wallet does not have it
would be untrue.

```console
$ hw show mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih
Error: The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4, which are hidden. To show one of them, run for instance: hw setdefault --show-network btctest
```

**The default network is always shown.** A command given no `--network` works
on the default network, so `hw` must show it. `setdefault` refuses to hide it,
and refuses a hidden network as the default. One command does both changes:

```console
hw setdefault --show-network btctest4 --network btctest4    # show it, and make it the default
hw setdefault --hide-network btc --network bch              # hide the default, and name another
```

**Show one test network, not all of them.** The test networks
[share their keys](#what-a-portfolio-holds), so an address of `btctest4` is
also one of `btctest`. With both shown, a command given such an address may
ask for `--network`. With `btctest4` shown alone, it never asks.

**Why the file lists the networks to show.** It could list the networks to
hide instead. A list of shown networks says what you see, in one place. It
also keeps a network that a later `hw` adds hidden until you ask for it.

Hiding a network deletes nothing. Its addresses, coins, transactions and
labels stay in the database, and [`exportwallet`](#hw-exportwallet) and
[`backup`](#hw-backup) still write them. A hidden network still counts for
[`deleteportfolio`](#hw-deleteportfolio), which keeps a portfolio that was
used on any network.

#### What each command takes

The commands do not all work in the active portfolio. A command about the whole
wallet, such as `sync`, covers every portfolio. A command that is given a
transaction, a coin or an address already knows where it belongs, so it needs
no `switch`.

| Commands | Where they work |
|---|---|
| `receive`, and `show` with a path or with no word | On one account of the active portfolio. `--network` and `--type` pick it, each with its default. |
| `send`, `preparetx`, `preparesendall` | They spend the coins of the active portfolio on one network, of both types together. The filter `--coin-type` narrows them down to one type. The coins and addresses of the choosers, `--coin` and `--spend-from`, must be in the active portfolio. |
| `addresses`, `balances`, `coins`, `txs` | They list the whole active portfolio, in a section for each [shown network](#shown-and-hidden-networks). `--network` and `--type` show one network or one type only. `txs` takes no `--type`. |
| `checkmnemonic`, `label` with a description alone, and `label` and `signmsg` with a path | On the active portfolio. `label DESCRIPTION --of-wallet` describes its wallet. |
| `portfolios`, `pendingtxs`, `sync`, `exportsheet` | They cover every portfolio of every wallet. `--network` narrows `pendingtxs` and `sync` down to one network. `sync` works on the hidden networks too, and the three others leave them out. |
| `exportwallet` | It writes every portfolio of every wallet, on the hidden networks too. `--wallet` writes one wallet only. `--portfolio` writes one portfolio only, of the wallet `--wallet` names, or else of the wallet of the active portfolio. |
| `signtx`, `signsweep`, `importtx`, `bumpfee`, `preparesweep`, and `show` with a transaction or a transaction file | They find the portfolio from the transaction, or from the destinations of a sweep. The active portfolio settles a choice between several, as the next paragraph says. |
| `lockcoin`, `unlockcoin`, `signmsg` and `show` with an address or a coin, and `label` with an address or a transaction | An address and a coin tell where they are. A transaction gets its label in every portfolio that holds it. `--network` is needed only when the wallet has the address or the coin on several shown test networks. |
| `importwallet`, and `show` with a file of a wallet | They find each wallet from the file. `--wallet` names one wallet of the file, by its fingerprint, and `--portfolio` one portfolio. They then read only that. |
| `switch` | Its argument names the portfolio to make active, or `--portfolio` does, and `--wallet` names its wallet. Without `--wallet`, it is the wallet of the active portfolio. |
| `deleteportfolio`, `deletewallet` | They never take what to delete from the active portfolio. `deleteportfolio --portfolio` names a portfolio of the wallet of the active portfolio, and refuses the active one. `deletewallet --wallet` names a wallet, and refuses the wallet of the active portfolio while another wallet exists. |

A transaction can involve several portfolios. A transfer between two portfolios
belongs to the one that pays it, since that one locks the coins and signs. When
several portfolios pay, or when none pays and the transaction pays several,
`hw` takes the active portfolio if it is one of them. Otherwise it names them,
and stops:

```console
$ hw importtx signed.psbt
Error: The transaction spends from several portfolios, portfolio0 of wallet0 on btc and portfolio0 of wallet1 on btc: switch to the one to use first
```

`switch` to the one you mean, and run the command again. The destinations of a
sweep follow the same rule. The active portfolio settles nothing else. A
transfer stays with the portfolio that pays it, whichever portfolio is active.

A command that finds the portfolio by itself may answer about another
portfolio than the active one. The paths and labels it shows are then those of
that portfolio. The field that names the portfolio says so, in yellow on a
terminal. For a transaction it reads
`Portfolio: portfolio0, not the active portfolio1`. For an address or a coin,
the `Account` field reads `portfolio0, not the active portfolio1  btc  bip84`.
A transfer is shown as the portfolio that pays it sees it, even while the
portfolio it pays is active. That portfolio signs it, so what `signtx` shows
before the mnemonic matches what the mnemonic signs.

#### Paths of another portfolio

A path alone, such as `bip84/0/3`, is always an address of the active
portfolio. That is the address `show`, `label`, `signmsg`, `--spend-from` and
`--change-back` take for it. A path of any other portfolio names that
portfolio after it, as in `bip84/0/3 of portfolio1`, so you never take it for
one you can type. When the database has several wallets, it names the wallet
too, as in `bip84/0/3 of portfolio1 of wallet0`. See
[How the views name a portfolio](#how-the-views-name-a-portfolio).

This shows in two cases. The first is a payment between two portfolios. `hw`
knows the address it pays in the other portfolio, and shows it as an address of
the wallet, not as a stranger's:

```console
$ hw show a1075db5
14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  Txid     : a1075db5c0ffee00000000000000000000000000000000000000000000000001
  Short id : a1075db5
  Time     : 14 Nov 2023 23:13:20 CET
  Fee      : 0.00001000 btc, 4 sat/vB
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0
  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0
```

The second is an answer about another portfolio than the active one. Every path
in it then names its portfolio, those of the active portfolio too. Such a view
holds paths of two portfolios, and a path alone would be read as either. Here
`portfolio1` is active, and the same payment is still shown as `portfolio0`
sees it:

```console
$ hw show a1075db5
14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations
  Portfolio: portfolio0, not the active portfolio1
  ...
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0
  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0 of portfolio0
```

An address of another portfolio is blue on a terminal, as every address of the
wallet. Blue says the address is yours, which is what you check first of an
address you pay. The field it is under, `To` here, says the payment goes
there. Its value is not green, since the amount leaves the portfolio of the
view. Its label, if it has one in its portfolio, is on the line below it.

The short views name such an address by its path and its portfolio, in place
of the address: `to bip44/0/0 of portfolio1`. The label of the payment comes
after it. An address of another wallet is replaced by the label of the
payment instead. The path stays here because it tells that the payment went to
you.

The answers about one address, one coin or one transaction follow the rule,
and so does `pendingtxs`, which lists every portfolio. `addresses`,
`balances`, `coins` and `txs` list the active portfolio, so their own paths
are alone. In `--json`, a transaction gives these addresses under
`otherportfolios`, each with its `wallet`, `portfolio`, `type`, `path` and
`label`. They stay under `otheroutputs` and `otherinputs` as well, with their
values.

> **Good to know**
>
> - **`hw` knows an address of a portfolio once that portfolio gave it out.**
>   An address further down a chain, which no `receive` gave and no `sync`
>   found, shows as an address of another wallet. An offline computer that
>   never ran `receive` knows no address, so every address of another
>   portfolio shows that way there.
> - **The other portfolio must be in the same database.** A portfolio kept on
>   another computer is another wallet to this one.
> - **The row of a transaction in a spreadsheet does not name it.**
>   `exportsheet` and `--csv` give the address of another portfolio without a
>   path there, as an address of another wallet. The tab of the addresses of
>   transactions of [`exportsheet`](#hw-exportsheet) names the portfolio that
>   holds it, with its path.

#### How the views name a portfolio

A listing and the details of an address, a coin or a transaction name the
portfolio they are about. With one wallet in the database, they name the
portfolio alone, as in `portfolio1`. A user of one mnemonic reads no wallet
name there. With several wallets, they name the wallet of every portfolio,
`wallet0` too, as in `portfolio1 of wallet0`.

The rule is the same wherever a view names a portfolio. That covers the
`Portfolio` and `Account` fields, the note about the active portfolio, the
titles of the listings and the portfolio after a path. It also covers the
answer of `sync` and `recover`, and their status line.

A portfolio named alone would be misread with several wallets. Here
`portfolio0` of `wallet0` pays `portfolio0` of `wallet1`, which is active.
`show` gives the view of each. In the second view, `portfolio0` alone after
the first path would read as the portfolio of that view:

```console
$ hw show c3e5a7b9
14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations
  Portfolio: portfolio0 of wallet0, not the active portfolio0 of wallet1
  ...
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0 of wallet0
  To       : 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2  0.00040000 btc  bip44/0/0 of portfolio0 of wallet1
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0 of portfolio0 of wallet0

14 Nov 2023 23:13  Incoming     +0.00040000 btc  2 confirmations
  Portfolio: portfolio0 of wallet1
  ...
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0 of wallet0
  To       : 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2  0.00040000 btc  bip44/0/0
```

Three things do not follow the count of the wallets in the database:

- `portfolios` always names the wallet of each portfolio. It is where you
  tell the wallets apart.
- A message in a sentence names both, as in `Created portfolio0 of wallet0`.
  So do the errors, and the step that switches to another portfolio.
- [`show FILE`](#hw-show) of a file of a wallet counts the wallets of the
  file. It names the wallet of each account when the file holds several.

The next steps a command suggests work in the active portfolio, so they name
no portfolio. A step about another portfolio starts with the `switch` to it.
That is the step after a command creates a portfolio that does not become
active. It is also the first step after a `sync` that finds a pending
transaction in conflict in another portfolio, or new transactions in other
portfolios alone.

#### What the accounts of a portfolio share

On a network, the accounts of a portfolio work as one:

- **A payment spends both types together.** `preparetx` and `send` select
  among every coin of the portfolio on the network, `bip44` and `bip84`
  alike. You can move to native SegWit addresses and still spend your legacy
  coins, with no transfer between accounts first.
- **A transaction is shown as the portfolio sees it.** It is `Incoming` when
  funds enter the portfolio, and `Outgoing` when they leave it. It is
  `Internal` when funds move inside the portfolio, from a `bip44` address to a
  `bip84` one for example. A payment from `portfolio0` to `portfolio1` is
  outgoing in `portfolio0` and incoming in `portfolio1`.
- **A balance is that of a network.** On `btc`, it adds up the `bip44` and
  the `bip84` account. Balances of two networks are never added, since they
  are different coins.

> **Good to know**
>
> - **The listings do not take the default network or type.** Without
>   `--network` and `--type`, `addresses`, `balances`, `coins` and `txs` show
>   the whole portfolio, whatever `default-network` and `default-type` say.
>   The two keys set the account `receive` gives an address of. They also set
>   the network a payment spends on, and the type of its change when it spends
>   coins of both types.
> - **Some commands find the portfolio themselves.** `signtx`, `importtx`,
>   `bumpfee` and `preparesweep` take the portfolio of the transaction,
>   whichever portfolio is active. A transaction between two portfolios
>   belongs to the one that pays. `show` shows a transaction file as that
>   portfolio sees it, and a transaction of the wallet as each portfolio that
>   holds it sees it. A view of another portfolio than the active one says
>   so.
> - **Coin control works in the active portfolio.** `--coin` and
>   `--spend-from` take the coins and addresses of the active portfolio only.
>   One of another portfolio is refused, with the portfolio that holds it and
>   the switch to it.
> - **A sweep that pays no portfolio needs `--network`.** The default network
>   does not apply to it. See [`preparesweep`](#hw-preparesweep).

### Amounts

**An amount without a unit is in bitcoins**, unless `--unit` or the
configuration names another unit. `ADDRESS:1000` asks for 1000 BTC, not 1000
satoshis. Write the unit, as in `ADDRESS:1000sat`, or give `--unit sat`.

An amount can end with `btc`, `bit`, `bits`, `sat` or `sats`. Bitcoins take up
to 8 decimals, bits up to 2, and satoshis none. `ADDRESS:0.00001`,
`ADDRESS:10bits` and `ADDRESS:1000sat` pay the same.

If you count in satoshis or in bits, make it your unit with
[`setdefault`](#hw-setdefault), as in `hw setdefault --unit sat`. It writes the
key `unit` of the [configuration](#configuration). Amounts are then shown in
that unit. An amount typed without a unit is read in it too. A script should give `--unit`,
or write the unit after each amount, so that the configuration cannot change
what it pays. JSON answers give every amount in satoshis, whatever the unit.

### Listings

Six commands list what the wallet holds: `portfolios`, `addresses`, `balances`,
`coins`, `txs` and `pendingtxs`. By default, they show each element in a few
lines. This is the compact view. `--verbose` gives every detail of each
element. `--tabular` gives each element one row of a table. The section of each
command says what its views show.

`addresses`, `balances`, `coins` and `txs` list the whole active portfolio, so
their rows come in sections. There is a section for each
[shown network](#shown-and-hidden-networks), and `addresses` has one for each
type on a network as well. A title line above each section says where its rows
are:

```console
$ hw addresses
portfolio0  btc  bip44

bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"
  unused

portfolio0  btc  bip84

bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"
  unused

portfolio0  bch  bip44

bip44/0/0  bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u
  unused
```

A section without rows is left out. `--network` and `--type` narrow a listing
down. A listing narrowed to one place has no title, since the command line
already says where it is. `balances` keeps its title, which gives the total of
the network.

Every view writes the path of an address with the type of its account in
front, such as `bip84/0/0`. The path inside the account, `/0/0`, would name an
address in each account of a network, and `balances` and `coins` list both
accounts of `btc` together. The type costs five columns, and saves you from
telling the two apart by the form of the address. The network is in the title
of the section. You can type a path back where a command takes an address of
the wallet: `show`, `label`, `signmsg`, `--spend-from` and `--change-back`
take `bip84/0/0` as well as the address. A path is one of the active
portfolio. The network the command works on tells which address it is, and
`--network` picks one when the portfolio has an address at the path on
several networks. `show` and `signmsg` derive the address at a path from the
public key, so they take any path, on the offline computer too. The others
take the path of an address the portfolio holds.

If you always want the same view, make it the default with
[`setdefault`](#hw-setdefault), as in `hw setdefault --tabular`. It writes the
key `listing-view` of the [configuration](#configuration). `--compact` then
brings back the compact view for one listing, and `hw setdefault --compact`
brings it back for good. The key changes the screen alone.
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
is `portfolios --verbose`, whose lines wrap, so that you can copy the
descriptor of an account whole. A table taller than the screen also keeps its
header at the top while its rows scroll, with `less` 608 or later. A table in
sections does not, since each section has a title and a header of its own.
When you set `LESS` yourself, `hw` leaves it as it is.

Scripts read a listing a page at a time with `--max`, then `--after` the last
row they got. `--max` counts the rows over all the sections. `--after` goes on
in the section that holds the row, then with the sections that follow.

The test networks share their keys, so one address can be in the sections of
several of them. `addresses --after` and `balances --after` refuse such an
address and ask for `--network`, naming the networks that have it. Going on in
the first of those sections would show the same rows again and again. Page
through one test network at a time with `--network`.

```console
hw txs --max 20 --json
hw txs --max 20 --after TXID --json
```

`--csv FILE` writes a listing to a new file instead, for a spreadsheet. Each
element takes a row. The columns hold every figure `hw` has of it, such as the
txid, the coin and the fee, each in a column of its own. Each hash comes in
full, then by its [short id](#short-ids). Amounts are in satoshis, as whole
numbers, whatever `--unit` says. `--after` and `--max` choose the rows, as they
do on the screen.

```console
hw txs --csv txs.csv
hw coins --csv coins.csv
```

The file of a listing has the columns of its tab in the spreadsheet of
[`exportsheet`](#hw-exportsheet): the same columns, in the same order, filled
the same way. Rows copied from one fit under the headings of the other. The
columns follow four rules in every listing, so that knowing one file tells you
where to look in the others.

- **The account comes first.** Every row starts with `Wallet`, `Portfolio`,
  `Network` and `Type`, though a listing covers one portfolio. A row of several
  accounts names each type, as in `bip44, bip84`.
- **Then comes what the listing is about.** The file of `coins` goes on with
  the coin, and that of `txs` with the txid.
- **A heading means one thing.** Two listings give the columns they share
  under the same headings, in the same order. The label of an address is
  `Address label` everywhere, and that of a transaction is `Transaction label`.
  `Type` is the type of the account. So the column that says `Incoming`,
  `Outgoing` or `Internal` is `Direction`, which the screen calls the type of
  a transaction.
- **The date `hw` created the element comes last.** A portfolio, an address
  and a pending transaction have one. The time of a transaction is what a
  history is sorted by, so it comes right after the txid.

Each time takes three columns. `Time (UTC)` gives it in UTC, as
`2023-11-14T22:13:20`, whatever the time zone of the computer. `Time (local)`
gives it on the clock of the computer, in the same form, as the screen shows
it. `Time (offset)` says how far that clock was from UTC at that time, as in
`UTC+01:00`. A time of the summer has the offset of summer time.

> **Good to know**
>
> - **A spreadsheet guesses what each cell of a CSV file holds.** A label made
>   of digits alone, such as `007`, becomes the number 7. The fingerprint of a
>   mnemonic is often made of digits alone, so the file of `portfolios` leaves
>   it out. `portfolios --verbose` gives it. About one short txid in 43 is made
>   of digits alone too, such as `01234567`, and becomes a number without its
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
> - **A cell that names several things holds a comma.** The two types of a
>   row, the paths of two accounts and the txids of two replacements are apart
>   by a comma and a space. The CSV file puts such a cell in quotes, as the
>   format asks. A script that splits each line at its commas must mind the
>   quotes, or read `--json` instead.
> - **The offset of a local time starts with `UTC`.** A spreadsheet would read
>   `+01:00` alone as a time of day, or as a formula.
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

[Tab](#shell-completion) completes a hash or a coin to its short id.

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

**`hw` never overwrites a file.** `--output`, `--csv`, `exportwallet`,
`exporttx`, `backup` and `exportsheet` refuse a file that exists, even one that
appeared while the command ran. `signtx` and `signsweep` replace their
`--input` file only when you agree.

A file `hw` writes is whole on the disk before it takes its name, so a crash or
a power loss leaves no half-written file. It is readable by its owner alone.

### One command at a time

A second `hw` command on the same database waits three seconds, then stops with
"The database is in use by another hw command".

## Commands

The commands marked `!` ask for a mnemonic or private keys. Run them on an
offline computer if you can. [`recover`](#hw-recover), [`send`](#hw-send) and
[`preparesweep --key-file`](#hw-preparesweep) take them too, but they need the
server, so they are not marked.

| Command | What it does |
|---|---|
| **[Wallets and portfolios](#wallets-and-portfolios)** | |
| [`newportfolio`](#hw-newportfolio) `!` | Make a mnemonic and its wallet, or add a portfolio to a wallet |
| [`recover`](#hw-recover) | Find the portfolios of a mnemonic that were paid |
| [`checkmnemonic`](#hw-checkmnemonic) `!` | Check a mnemonic and passphrase against the active portfolio |
| [`exportwallet`](#hw-exportwallet) | Write every wallet and its labels to a file |
| [`importwallet`](#hw-importwallet) | Add the portfolios and labels of a file |
| [`portfolios`](#hw-portfolios) | Show the portfolios, their balances and descriptors |
| [`switch`](#hw-switch) | Make another portfolio the active one |
| [`setdefault`](#hw-setdefault) | Set the defaults of the commands, and the networks shown |
| [`deleteportfolio`](#hw-deleteportfolio) | Delete a portfolio that was never used |
| [`deletewallet`](#hw-deletewallet) | Delete a wallet with all of its portfolios |
| [`sync`](#hw-sync) | Update the portfolios from the network |
| **[Addresses](#addresses)** | |
| [`receive`](#hw-receive) | Get a new address to receive a payment |
| [`show`](#hw-show) | Show an address, a transaction or a coin, or a file of a transaction or of a wallet |
| [`addresses`](#hw-addresses) | List the receiving addresses of a portfolio, or its change addresses |
| [`balances`](#hw-balances) | List the addresses of a portfolio that hold coins |
| [`label`](#hw-label) | Label an address or a transaction, or describe a portfolio or a wallet |
| **[Coins](#coins)** | |
| [`coins`](#hw-coins) | List the coins of a portfolio |
| [`lockcoin`](#hw-lockcoin) | Keep a coin from being spent |
| [`unlockcoin`](#hw-unlockcoin) | Unlock a coin locked with `lockcoin` |
| **[Payments](#payments)** | |
| [`fees`](#hw-fees) | Show the fee rates of recent blocks |
| [`send`](#hw-send) | Prepare, sign and send a payment in one command |
| [`preparetx`](#hw-preparetx) | Prepare a payment, without signing it |
| [`preparesendall`](#hw-preparesendall) | Prepare a payment of all the coins of a portfolio |
| [`signtx`](#hw-signtx) `!` | Sign a pending transaction or a transaction file |
| [`broadcast`](#hw-broadcast) | Send a signed transaction to the network |
| [`bumpfee`](#hw-bumpfee) | Replace a stuck payment with one that pays a higher fee |
| **[Transactions](#transactions)** | |
| [`txs`](#hw-txs) | List the transactions of a portfolio |
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

### Wallets and portfolios

#### `hw newportfolio`

Makes a new mnemonic and its wallet, or adds a portfolio to the wallet of a
mnemonic you have. One command does both, because either way it ends with a
whole portfolio, ready to receive.

```console
hw newportfolio                             # a new mnemonic, its wallet and portfolio0
hw newportfolio --entropy 32 --dice         # 24 words, with rolls of a die mixed in
hw newportfolio --shares 3 --threshold 2    # a backup in shares instead of a mnemonic
hw newportfolio --shares 1
hw newportfolio --existing                  # one more portfolio in a wallet you have
```

| Option | What it does |
|---|---|
| `--existing` | Asks for a mnemonic you have already, and adds a portfolio to its wallet, instead of making a new mnemonic. |
| `--entropy BYTES` | Sets the bytes of entropy: 16, 20, 24, 28 or 32, for 12, 15, 18, 21 or 24 words. The default is 16. A backup in shares takes 16 or 32. |
| `--dice` | Mixes in rolls of a die. |
| `--shares N` | Prints the N shares of a backup instead of a mnemonic, in the [SLIP39] format. |
| `--threshold T` | Sets how many shares rebuild the seed. |

**A new mnemonic.** Without `--existing`, `newportfolio` makes a mnemonic from
the entropy of the operating system. It asks for a passphrase, twice to catch a
typo. Leave it empty for none. It then shows the mnemonic, once, and creates
the wallet with its `portfolio0`. A new mnemonic is a new wallet, which takes
the lowest free index. In a new database, that is `wallet0`.

```console
$ hw newportfolio
Mnemonic of 12 words, from /dev/random
   1. ...        2. ...        3. ...        4. ...
   5. ...        6. ...        7. ...        8. ...
   9. ...       10. ...       11. ...       12. ...

Created portfolio0 of wallet0
  Fingerprint: bc560f95
  Accounts   : 12, on btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4 and bchreg
portfolio0 of wallet0 is the active portfolio: the commands work in it.
```

Write the mnemonic down and keep copies in safe places, away from computers and
phones. `hw` stores the public keys of the portfolio only, never the mnemonic.
The fingerprint names the mnemonic from then on, without telling anything of
it. It is the four bytes that transaction files and output descriptors write
before a key of the mnemonic.

**A mnemonic you have.** With `--existing`, `newportfolio` asks for the
mnemonic and its passphrase, each twice, or for the shares of a backup. It adds
a portfolio to the wallet of that mnemonic, at the lowest free index. That is
`portfolio1` when the wallet has `portfolio0`. It is `portfolio1` too when the
wallet has `portfolio0` and `portfolio2`. The wallet of the mnemonic is the one
with the fingerprint of its master key. When the database has no wallet of
the mnemonic yet, it creates the wallet with its `portfolio0`.

**The active portfolio.** The first portfolio of a database becomes the
[active one](#the-active-portfolio), where the commands work. A portfolio
added later does not. `newportfolio` then says which portfolio is still
active, and gives the `switch` to the new one:

```console
$ hw newportfolio --existing
...
Created portfolio1 of wallet0
  Fingerprint: bc560f95
  Accounts   : 12, on btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4 and bchreg
The active portfolio is still portfolio0 of wallet0.

To work in portfolio1 of wallet0, make it the active one:
  hw switch 1 --wallet 0
```

A new portfolio does not take over. The commands you type next keep working
where they did, and you switch when you mean it.

**A portfolio is created whole.** It gets an account for every network, the
test networks included, and for both types of address. They are the twelve
accounts of [What a portfolio holds](#what-a-portfolio-holds). You never create
an account afterwards. `receive --network` and `--type` pick one.

**Backups in shares.** Any `--threshold` of the shares rebuild the seed, and
fewer tell nothing of it. With `--shares 3 --threshold 2`, losing one share
loses nothing, and finding one gives nothing away. A share is a list of 20
words, or of 33 with `--entropy 32`. `--shares 1` makes a single share, which is
the whole backup.

The commands that ask for a mnemonic take the shares in its place. Type one, and
`hw` asks for the next until it has enough.

> **Good to know**
>
> - **The mnemonic is shown once, and never asked back.** `newportfolio` does
>   not make you type the new mnemonic again. Test what you wrote down with
>   [`checkmnemonic`](#hw-checkmnemonic) before you rely on it.
> - **A passphrase makes another wallet.** A mnemonic with a passphrase gives
>   another master key than the mnemonic alone. The passphrase is part of the
>   secret, and must be kept too.
> - **To keep the keys offline, run `newportfolio` on the offline computer.**
>   Then copy the wallet to the online computer with
>   [`exportwallet`](#hw-exportwallet) and [`importwallet`](#hw-importwallet).
> - **`--entropy`, `--dice`, `--shares` and `--threshold` make a new
>   mnemonic.** They are refused with `--existing`.
> - **A backup in shares is not a mnemonic.** A wallet that reads [SLIP39]
>   shares restores it. One that reads only BIP39 mnemonics cannot.
> - **The passphrase of a backup in shares** has printable ASCII characters
>   only.
> - **A passphrase with accents keeps to the wallet `recover` made.** `hw`
>   normalizes a passphrase as BIP39 asks, and `hw` 0.9 did not. `recover`
>   makes a wallet of the passphrase as typed when only that one finds funds.
>   When the database has such a wallet, and none of the passphrase normalized,
>   `--existing` adds to it and says so. In a database without either, it
>   creates the wallet of the passphrase normalized. It then warns that a
>   wallet of an earlier `hw` has other keys, and points to `recover`. See
>   [A passphrase with accents](#a-passphrase-with-accents).

#### `hw recover`

Finds the portfolios of a mnemonic, or of a backup in shares, that were paid.
It creates them and syncs them. It needs the mnemonic and the server at the
same time.

```console
hw recover
hw recover --portfolio-gap 20
```

`recover` checks the portfolios in order from `portfolio0`. For each one, it
asks the server about the receiving addresses of every account: on every
network, the test networks included, and of both types of address. When any
account of a portfolio was ever paid, it creates the whole portfolio, with
every account, and syncs it. It finds whole portfolios rather than single
accounts, so the wallet it brings back is ready on every network, as a new one
is.

The search covers every network, whatever `default-network` says. A mnemonic
is often paid on both chains, as every mnemonic from before the fork of 2017
is. A search of one network would say nothing of the other.

The search covers the [hidden networks](#shown-and-hidden-networks) too. A
portfolio that was paid on a hidden network alone is still found, created and
synced. The answer leaves the hidden networks out. It does not name them as
searched, gives no sync of them and no funds found on them, and does not say
that the server of one is down or missing.

The wallet is the one of the mnemonic. `recover` finds it by the fingerprint
of its master key, or creates it when the database does not have it. A new
wallet gets its `portfolio0` whether that one was paid or not, since a wallet
always has `portfolio0`. The portfolios the wallet has already are skipped. A
wallet with the fingerprint of the mnemonic and other keys is refused, and
nothing changes. A fingerprint is four bytes, which two mnemonics may share.

In a database without any portfolio, the first portfolio `recover` creates
becomes the [active one](#the-active-portfolio). That is `portfolio0` of the
new wallet, even when only a later portfolio was paid. The next step of
`recover` is then the `switch` to the first portfolio it found paid. In a
database that has an active portfolio, it stays active. `recover` then says
so, and gives the same step.

The search stops after 10 portfolios in a row where nothing was found, or the
number `--portfolio-gap` gives. [BIP44] stops at the first unused account. `hw`
looks further, since a portfolio created on an offline computer may never have
been paid before a later one was. After a thousand used addresses in a row, it
asks whether to keep looking, as [`sync`](#hw-sync) does. On a terminal, a line
tells which portfolio it is looking at. Before the mnemonic, the line tells the
network whose server `recover` is checking.

After every 20 portfolios the server reports paid, `recover` asks whether to
keep looking. A server that lies reports them without end. A no stops the
search, and nothing is added.

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
>   offline, create the portfolios on the offline computer with
>   [`newportfolio --existing`](#hw-newportfolio) instead. Copy them with
>   [`exportwallet`](#hw-exportwallet) and [`importwallet`](#hw-importwallet),
>   and run [`sync`](#hw-sync).
> - **A network the server does not serve is named, and is no error.** A
>   server seldom serves every network. The public one has no regtest chain,
>   for example. `recover` names those networks in one line and searches the
>   others.
> - **A network whose server is down is an error.** `recover` says so,
>   searches the other networks, then exits with an error. A portfolio that
>   was paid on that network alone was not found. Run `recover` again once the
>   server answers. The portfolios found the first time are skipped.
> - **A hidden network whose server is down is no error.** `recover` says
>   nothing of it. A portfolio that was paid on that network alone was not
>   found. Show the network and run `recover` again if you expect funds
>   there.
> - **A portfolio found on a hidden network looks empty.** `recover` creates
>   it and names it, and `portfolios` shows it without a line for that
>   network. [`setdefault --show-network`](#hw-setdefault) shows what it
>   holds there.
> - **A server `hw` cannot reach is down for every network.** When the
>   connection fails or times out, or its TLS handshake fails, `recover`
>   checks no further network there. They would fail the same way, and a
>   timeout takes 30 seconds each time. They are named as not searched, with
>   that error. With no network left, `recover` stops before it asks for the
>   mnemonic. A `host` of `https://` for a server that speaks plain HTTP
>   fails this way.
> - **A server can also fail during the search.** `recover` then searches that
>   network no further, syncs nothing there, and goes on with the others. It
>   keeps the portfolios it found, names the network with the error as not
>   searched, and exits with an error. The same holds when the server fails
>   while `recover` syncs the portfolios found, or looks for the funds at the
>   paths of other wallets. Only the networks searched in full are named as
>   searched. Any other error, such as a no to one of its questions, stops
>   `recover`, and nothing is added.
> - **A passphrase with accents is tried both ways.** `hw` normalizes a
>   passphrase as BIP39 asks, and earlier versions did not. When nothing is
>   found, `recover` also tries the passphrase as typed, and says so if that
>   finds portfolios. See [A passphrase with accents](#a-passphrase-with-accents).

#### `hw checkmnemonic`

Checks that a mnemonic and passphrase give the public key of every account of
the active portfolio. Use it to test a mnemonic you wrote down, or the shares
of a backup, before you rely on it. Nothing is signed, and nothing is changed.

```console
hw checkmnemonic                            # against the active portfolio
hw switch 0 --wallet 1                      # to check the mnemonic of wallet1
hw checkmnemonic
```

To check another portfolio, switch to it first.

It answers that the mnemonic and passphrase matched the portfolio, or that
they did not. A check never changes the wallet, even when the mnemonic
matches.

#### `hw exportwallet`

`hw exportwallet FILE` writes every portfolio of every wallet to a file, with
its labels. [`importwallet`](#hw-importwallet) adds the file to another
database. `--wallet` and `--portfolio` write less:

```console
hw exportwallet wallet.jsonl                # every wallet, on the offline computer
hw exportwallet family.jsonl --wallet 1     # every portfolio of wallet1
hw exportwallet savings.jsonl --portfolio 1 # portfolio1 of the wallet of the active portfolio
hw exportwallet savings.jsonl --wallet 1 --portfolio 2    # portfolio2 of wallet1
```

| Options | What the file holds |
|---|---|
| none | Every portfolio of every wallet |
| `--wallet M` | Every portfolio of `walletM` |
| `--portfolio N` | `portfolioN` of the wallet of the [active portfolio](#the-active-portfolio) |
| `--wallet M --portfolio N` | `portfolioN` of `walletM` |

`--portfolio` alone looks in the wallet of the active portfolio, as
[`switch`](#hw-switch) does. A user of one mnemonic never names a wallet.
Without `--portfolio`, the active portfolio plays no part. The same command
writes the same file whichever portfolio you last switched to.

**Why every wallet by default.** One file then holds the whole database. It
moves every wallet to another computer in one step. It also keeps every label
before you delete a wallet, or lose the computer. A file of one wallet at a
time would need a `switch` and an export for each mnemonic, and a wallet you
forget would be left out without a word. `--wallet` is for the file that must
not show the other wallets.

**Several wallets in one file.** [BIP 329] foresees them. Each record has an
`origin`, which names its wallet by the fingerprint of the mnemonic. The BIP
says to use it to tell apart the labels of different wallets in the same
export. `hw` writes it on every record. The file does not carry the index of a
wallet, so it does not say which one is `wallet0`.
[`importwallet`](#hw-importwallet) finds each wallet by its fingerprint,
whatever index the wallet has in the database that reads the file.

The file does not carry the active portfolio either. Each database keeps its
own, and [`importwallet`](#hw-importwallet) says which one it is after the
import.

The file is in the format of [BIP 329], which other wallets read and write. Each
account goes in as its public key, with its path, its network, its type and the
fingerprint of the mnemonic. Then come the labels of its receiving addresses
and of its transactions, and the coins locked with `lockcoin`. The descriptions
of the wallets and of their portfolios go in too. The file holds no private key.

The file carries every account of each portfolio. The computer that has the
mnemonic derives every account once, and the computer that watches the chain
needs nothing else, whichever network or type of address you use later.

> **Good to know**
>
> - **The file shows every address of every wallet.** It holds no private
>   key, but its public keys reveal every address and balance of the
>   portfolios. `hw` makes it readable by you alone.
> - **A file of several wallets links them.** Whoever reads it learns that one
>   person holds them all. Before you hand a file to someone else, write the
>   one wallet they need with `--wallet`.

#### `hw importwallet`

`hw importwallet FILE` adds the portfolios and labels of a file to the
database. The file is the one [`exportwallet`](#hw-exportwallet) writes.

```console
hw importwallet wallet.jsonl                # on the online computer
hw importwallet wallet.jsonl --wallet bc560f95               # one wallet of the file
hw importwallet wallet.jsonl --wallet bc560f95 --portfolio 1 # one portfolio of that wallet
hw importwallet wallet.jsonl --portfolio 1  # one portfolio, in a file of one wallet
hw importwallet labels.jsonl                # the labels of another wallet
```

[`show FILE`](#hw-show) shows what `importwallet` would make of a file, and
changes nothing. Run it first to see the portfolios and labels the file holds.

```console
$ hw importwallet wallet.jsonl
wallet0  bc560f95  created
  portfolio0  created, 12 accounts added
  portfolio1  created, 12 accounts added

portfolio0 of wallet0 is the active portfolio: the commands work in it.

Labels set: 1 wallet, 2 portfolios, 2 addresses
```

**The active portfolio.** In a database without any portfolio, the first
portfolio `importwallet` creates becomes the
[active one](#the-active-portfolio). That is `portfolio0` of the file, as
above. In a database that has an active portfolio, it stays active.
`importwallet` then says so, and its next step is the `switch` to the first
portfolio it created.

**Wallets and portfolios.** `importwallet` finds each wallet by the fingerprint
the file gives, and each portfolio by its index. It creates a portfolio the
database does not have, whole, with the twelve accounts of the file. The
database can then watch the portfolio and prepare its payments without the
mnemonic. This is how an online computer works with an offline signer.

**Several wallets.** A file holds every wallet of the database that wrote it,
unless `exportwallet --wallet` wrote one alone. `importwallet` takes each
wallet of the file by itself. It creates a wallet the database does not have,
at the lowest free index, in the order of the file. So an empty database gets
the wallets in the order they had.

The index of a wallet is not in the file. A wallet may be `wallet0` on the
computer that wrote the file and `wallet1` on this one. That happens when you
run [`recover`](#hw-recover) or `newportfolio --existing` with the mnemonics
in another order. The fingerprint still finds the wallet, and its labels and
its description go to it. Here the file names the wallet of `bc560f95` first,
which was `wallet0` where the file was written:

```console
$ hw importwallet wallet.jsonl
wallet1  bc560f95  already in the database
  portfolio0  nothing to add

wallet0  4435aa1c  already in the database
  portfolio0  nothing to add

Labels set: 2 wallets, 2 addresses
```

**A portfolio the database has** is left as it is, and the answer says
`nothing to add`. `importwallet` never makes a second portfolio with the same
index. Such a portfolio would hold the same keys, since the mnemonic and the
index decide them. A portfolio has every account from the start, so
`importwallet` adds no account to one either. The keys of the file must be the
ones the portfolio holds. Otherwise `importwallet` refuses and changes nothing,
since the file and the database are then not of one mnemonic.

`importwallet` also refuses three other files, and changes nothing:

- **A portfolio without all of its accounts.** A portfolio the database does
  not have is created whole, or not at all. A file that gives only some of its
  twelve accounts is refused, with the advice to write the file again on the
  computer that has the whole portfolio.
- **A key the database holds, at another index.** The text of a key names its
  index, and a file can write any index there. The same key at another index
  would give a second portfolio the addresses of the first. `importwallet`
  compares the keys themselves, and refuses a key that another account of the
  network holds.
- **Two keys for one path.** The test networks share the coin type `1'`, so
  the bip44 accounts of a portfolio on the six of them have one path,
  `m/44'/1'/0'`, and one key. So do the bip84 accounts of the three test
  networks of Bitcoin. A file that gives them different keys is refused, with
  the path and the networks: `receive` would give different addresses on
  networks that must give the same.

**A key alone is not imported.** `importwallet` imports whole wallets. An
extended public key or an output descriptor from another wallet is one account.
It does not say which portfolio it belongs to, and `hw` keeps no account
outside a portfolio. A [BIP 329] file of another wallet may hold such a key as
a record. `importwallet` leaves the record aside and says why, and still sets
the labels of the file. A file that holds a key or a descriptor alone, and no
record, is refused.

**One wallet or one portfolio of the file.** `--wallet` and `--portfolio`
read a part of the file, its keys and its labels:

| Options | What is read |
|---|---|
| none | Every wallet of the file, with every portfolio |
| `--wallet FINGERPRINT` | That wallet, with every portfolio the file has of it |
| `--portfolio N` | `portfolioN`, in a file of one wallet |
| `--wallet FINGERPRINT --portfolio N` | `portfolioN` of that wallet |

The records of the rest of the file are not read. The answer counts them on
one line, and `--json` gives the count as `otherportfolios`. Here the file
holds one wallet, and the database had its `portfolio0` already:

```console
$ hw importwallet wallet.jsonl --portfolio 1
wallet0  bc560f95  already in the database
  portfolio1  created, 12 accounts added

The active portfolio is still portfolio0 of wallet0.

Labels set: 1 wallet, 1 portfolio, 1 address
Left aside: 1 record of the portfolios --wallet or --portfolio leaves out

To work in portfolio1 of wallet0, make it the active one:
  hw switch 1 --wallet 0
```

Here `--portfolio` names a portfolio of the file, not the portfolio a command
works in.

**A wallet of a file is named by its fingerprint.** `--wallet` takes the
fingerprint here, such as `bc560f95`. The `--wallet` of the other commands
takes an index. A file holds no index, and `wallet0` on the computer that
wrote it may be `wallet1` on this one. An index would then name the wrong
wallet without a word. The fingerprint is the same on every computer.
[`show FILE`](#hw-show) lists the wallets of a file with their fingerprints.

**In a file of several wallets, `--portfolio` needs `--wallet`.** The index
alone would name a portfolio of each wallet, and those have nothing in common
but their index. `importwallet` refuses, and gives the fingerprints to choose
from:

```console
$ hw importwallet wallet.jsonl --portfolio 1
Error: The file holds the wallets bc560f95 and 4435aa1c, and --portfolio alone does not say which of them portfolio1 is of. Name the wallet by its fingerprint too, as in --wallet bc560f95 --portfolio 1. Nothing was changed
$ hw importwallet wallet.jsonl --wallet bc560f95 --portfolio 1
wallet0  bc560f95  already in the database
  portfolio1  created, 12 accounts added

The active portfolio is still portfolio0 of wallet0.

Labels set: 1 address
Left aside: 1 record of the portfolios --wallet or --portfolio leaves out

To work in portfolio1 of wallet0, make it the active one:
  hw switch 1 --wallet 0
```

A wallet always has `portfolio0`. So `importwallet` creates a wallet only from
a file that brings its `portfolio0`. In a database that does not have the
wallet yet, `--portfolio 1` is refused, and so is a file that
`exportwallet --portfolio 1` wrote. Import the whole wallet instead.

A `--wallet` or a `--portfolio` the file has nothing of is refused, with what
the file has: `The file has no wallet 12345678: it has the wallets bc560f95
and 4435aa1c`, or `The file has no portfolio2: it has portfolio0 and
portfolio1`. A value that matches nothing is more likely a typo than a wish to
import nothing. A [BIP 329] file of another wallet holds no key, so there the
wallet and the portfolio must be named by the origin of a record.

**Labels and locks.** `importwallet` sets the labels and locks of the file on
the addresses, transactions and coins the wallet has. It labels a transaction
before `sync` fetches it when the origin of its record says which portfolio it
is of. A record without an origin labels the transaction in the portfolios that
have it, on the network it names: the network alone names no portfolio. It derives
an address from the path the file gives, up to a thousand past the last used
address of its chain. What it cannot place, it counts: run `sync`, then
`importwallet` again. Labels of inputs, coins and change addresses are left
aside, since `hw` keeps none. A [BIP 329] file of another wallet sets its
labels the same way.

The `Labels set` line counts the wallets, portfolios, addresses and
transactions that took a label of the file. The label of a wallet or a
portfolio is its description. Each wallet and each portfolio counts once,
though the file writes its description with each of its keys. One that has
that description already counts too, as when you import the same file again. `--json` gives the counts as `walletlabels`,
`portfoliolabels`, `addresslabels` and `txlabels`.

> **Good to know**
>
> - **Import only a file you trust.** Without the mnemonic, `importwallet`
>   cannot prove that the keys of a file are those of your mnemonic. A file
>   that names the fingerprint of your wallet may still carry the keys of
>   someone else. It would add a portfolio of them, and `receive` would then
>   give out addresses of theirs.
>   Import only a file you wrote with `exportwallet` on your own signer, or
>   one that reached you over a channel you trust. Before you give out an
>   address of a new account, check one address of it on the signer, as
>   [Check an address before you give it out](#check-an-address-before-you-give-it-out)
>   tells: `hw show TYPE/0/0 --network NETWORK`, such as `bip84/0/0`, there
>   must show the ends of the first address `receive` gives here on that
>   network and type.
> - **A description you set is kept.** A wallet or a portfolio that has a
>   description keeps it. One that has none, or that `importwallet` creates,
>   takes the description of the file. When the file gives another one, the
>   answer says so under `Left aside`, as
>   `1 label of a portfolio that keeps its own`. `--json` counts them as
>   `walletlabelskept` and `portfoliolabelskept`. To take the description of
>   the file, set it with [`label`](#hw-label).
> - **`importwallet` derives no address over a thousand past the last used one
>   of its chain.** A file names any path, and a wrong one would have `hw` derive
>   addresses for days. Such an address is counted as not in the wallet yet, and
>   the answer says why.
> - **A file without any record is refused.** A key alone, a descriptor on a
>   line and an account file of an earlier version are such files.
>   `importwallet` stops with an error and changes nothing. In a file that
>   does hold records, a line that is not one is counted, and the import goes
>   on.

#### `hw portfolios`

Shows the portfolios of every wallet, with their balances. Each portfolio
starts with a `*` when it is the [active one](#the-active-portfolio), or a
space, then its name, its wallet and its description. Then come the date it
was created, and a line for each network it has addresses or coins on: what it
can spend there, what is unconfirmed, and how many coins and addresses it has.

```console
$ hw portfolios
* portfolio0  wallet0  "daily spending"
    created 10 Nov 2026
    btc  spendable 0.00150000 btc, 3 coins, 5 addresses
    bch  spendable 0.00000000 btc, no coins, 1 address

  portfolio1  wallet0  "savings"
    created 10 Nov 2026
    btc  spendable 0.25000000 btc, 1 coin, 1 address
```

The `*` is green on a terminal. It takes a column of its own, so that the names
line up whichever portfolio is active.

The figures are those of a network. On `btc`, they add up the `bip44` and the
`bip84` account, since a payment spends both. The spendable balance leaves out
locked coins, coinbase coins not yet mature, and unconfirmed payments from
other wallets. A network the portfolio never used has no line, so a portfolio
with twelve accounts still reads in a few lines.

`portfolios` lists every portfolio of every wallet, whichever one is active.
The portfolios of one wallet come together.

`--verbose` adds the fingerprint of the mnemonic and the time the portfolio was
created. It lists every [shown network](#shown-and-hidden-networks) of the
portfolio, each with its confirmed, unconfirmed and spendable balances, its
coins and its addresses. Under each
network come its accounts, each with its type, its derivation path, its
output descriptor and its key in the form of SLIP-132. A descriptor is the
format of [BIP 380] in which other wallets take an account without its private
key. Paste the descriptor into a wallet that takes one, to watch the account
there.

`--tabular` gives one row of a table to each network a portfolio has addresses
or coins on. The first column has no heading, and holds the `*` of the active
portfolio. Then come the wallet, the portfolio, the network, what it can spend,
what is unconfirmed, its coins, its addresses, and the description of the
portfolio. The first columns are as wide as their longest cell, which no row
cuts.

```console
$ hw portfolios --tabular
  │ Wallet  │ Portfolio  │ Network │ Spendable (btc) │ Unconfirmed (btc) │ Coins │ Addresses │ Description
──┼─────────┼────────────┼─────────┼─────────────────┼───────────────────┼───────┼───────────┼────────────
* │ wallet0 │ portfolio0 │ btc     │      0.00000000 │        0.00000000 │     0 │         1 │ "daily spending"
  │ wallet0 │ portfolio1 │ btc     │      0.00000000 │        0.00000000 │     0 │         1 │ "savings"
```

`--csv FILE` writes one row for each shown network of each portfolio to a
[CSV file](#listings). Its columns are the wallet, the portfolio, the network
and the types of the accounts of the portfolio there, `Active`, the
description of the portfolio and that of its wallet, the paths of its accounts,
what it can spend, its confirmed and unconfirmed balances, its coins and
addresses, and when the portfolio was created. `Active` says `yes` for the
active portfolio, and `no` for the others. The file holds no key and no
descriptor, since they would show every address and payment of the portfolio
to anyone the file reaches. `--json` says whether each portfolio is the active
one, as `"active": true` or `false`.

```console
hw portfolios
hw portfolios --verbose                     # the descriptors, for another wallet
hw portfolios --tabular
hw portfolios --csv portfolios.csv
hw portfolios --unit sat
```

**The descriptor.** A `bip44` account gives
`pkh([bc560f95/44h/0h/0h]xpub.../<0;1>/*)#vq597j87`. It carries the public key
of the account, the fingerprint of the mnemonic and the derivation path, the
two chains of addresses, and a checksum. It holds no private key. A hardware
wallet matches the account to its mnemonic by the fingerprint and the path.
A descriptor names no network: Bitcoin and Bitcoin Cash write the same keys,
and every test network writes `tpub` keys.

A `bip84` account gives `wpkh([bc560f95/84h/0h/0h]xpub.../<0;1>/*)#5nktextl`,
the script function naming the type.

**The key in the form of SLIP-132.** Each account is shown with its key in the
row `SLIP-132` as well. A `bip84` account shows it as a `zpub`, or a `vpub` on
a test network. Some wallets take only that form for a native SegWit account,
and make a legacy wallet of an `xpub`. The key is the same, only its prefix
differs. A `bip44` account shows its `xpub` there, or its `tpub` on a test
network, since SLIP-132 gives `bip44` accounts the prefixes of BIP32. So every
account has a key to paste into a wallet that asks for that form.

`--json` gives the descriptor of each account as `descriptor`, and its key in
the form of SLIP-132 as `slip132`. A descriptor goes out only.
[`importwallet`](#hw-importwallet) takes whole wallets, and reads no
descriptor back.

> **Good to know**
>
> - **The lines of `portfolios --verbose` wrap on a terminal.** A descriptor is
>   wider than most screens. The pager wraps these lines rather than cut them
>   at the edge of the screen, so that you can copy a descriptor whole.
> - **[`balances`](#hw-balances) repeats these totals.** It gives the same
>   figures for a network, beside the addresses they add up.

#### `hw switch`

`hw switch PORTFOLIO [--wallet N]` makes another portfolio the
[active one](#the-active-portfolio). The commands work in it from then on,
until the next `switch`. The database keeps it.

```console
hw switch 1                                 # portfolio1 of the same wallet
hw switch portfolio1                        # the same, by its name
hw switch 0 --wallet 1                      # portfolio0 of wallet1
```

`PORTFOLIO` names the portfolio, and must be given. `--wallet` names its
wallet. Without `--wallet`, the portfolio is one of the wallet of the active
portfolio. So after `hw switch 0 --wallet 1`, the command `hw switch 2` goes
to `portfolio2` of `wallet1`.

**Why the portfolio needs no option.** It is the one value `switch` must have,
and you type `switch` often. So it goes without an option, as the word of
`show` does. The wallet stays an option: most databases have one wallet, and
`hw switch 0 1` would leave you to remember which number is which.
`--portfolio N` names the portfolio too, for a script that names each value by
its option. `hw switch --portfolio 1` is `hw switch 1`. Give the portfolio
once: `switch` refuses it as both its argument and `--portfolio`.

The answer shows the portfolio as `portfolios` shows it, with its `*`:

```console
$ hw switch 1
The active portfolio is now portfolio1 of wallet0.
* portfolio1  wallet0  "savings"
    created 11 Nov 2026
    btc  spendable 0.00000000 btc, no coins, 1 address
```

`switch` refuses a wallet or a portfolio the database does not have, and the
active portfolio stays as it was:

```console
$ hw switch 5
Error: wallet0 has no portfolio5: portfolios lists the portfolios
$ hw switch 0 --wallet 2
Error: There is no wallet2: portfolios lists the wallets
```

`switch` needs no server, so it works on an offline computer too.

The commands that find their portfolio by themselves need no `switch`. They
are `importtx`, `signtx`, `signsweep`, `bumpfee`, `preparesweep`, `label`,
`lockcoin`, `unlockcoin`, `signmsg`, and `show` of a transaction, a coin or an
address. When they cannot tell between several portfolios, they take the
active one if it is among them. Otherwise they name them, and you switch to
the one to use. See [What each command takes](#what-each-command-takes).

> **Good to know**
>
> - **A database without any portfolio has no active portfolio.** `switch`
>   is refused there, with `There is no wallet yet: newportfolio makes one, and
>   recover or importwallet bring one back`. The first portfolio created
>   becomes active by itself.
> - **The switch stays.** It is written to the database, not to the
>   configuration file. After `hw switch 1` for one payment,
>   `receive` gives out addresses of `portfolio1` too. Switch back when you are
>   done. A script that switches leaves the next commands you type in the
>   portfolio it switched to.
> - **Each computer has its own.** The offline and the online computer each
>   keep their active portfolio, and the file of `exportwallet` does not carry
>   it. To check an address of another portfolio with [`show`](#hw-show),
>   switch to that portfolio on both.

#### `hw setdefault`

Writes defaults to the [configuration file](#configuration), so that you do
not have to edit it. Give each option you want as the default, as you would
give it to a command:

| Option | The default it sets | Key |
|---|---|---|
| `--network NET` | The network | `default-network` |
| `--type TYPE` | The type of address | `default-type` |
| `--unit UNIT` | The unit of the amounts | `unit` |
| `--compact`, `--verbose` or `--tabular` | The [view of the listings](#listings) | `listing-view` |
| `--show-network NET`, `--hide-network NET` | The [networks `hw` shows](#shown-and-hidden-networks) | `shown-networks` |

```console
hw setdefault --type bip84                  # give out native SegWit addresses
hw setdefault --network bch                 # work on Bitcoin Cash
hw setdefault --unit sat                    # show and read amounts in satoshis
hw setdefault --tabular                     # show the listings as tables
hw setdefault --network bch --type bip84    # two defaults at once
hw setdefault --show-network btctest4       # show a hidden network
hw setdefault --hide-network bch            # hide a network
```

It answers with the keys it wrote:

```console
$ hw setdefault --network bch --type bip84
default-network: bch
default-type   : bip84
Written to /home/you/.hw/config.json
```

`--show-network` and `--hide-network` change the list of the networks `hw`
shows. Each may be given several times, and the two go together. The answer
gives the whole list, as the file now has it:

```console
$ hw setdefault --show-network btctest4
shown-networks: btc, bch, btctest4
Written to /home/you/.hw/config.json
```

The other keys of the file stay as they are.
[The two options](#the-two-options) says what the network and the type do.
[Shown and hidden networks](#shown-and-hidden-networks) says what hiding a
network does. [Amounts](#amounts) and [Listings](#listings) say what the unit
and the view do.

> **Good to know**
>
> - **The portfolio is no default.** The commands work in the active
>   portfolio, which the database keeps. [`switch`](#hw-switch) changes it.
> - **The default network is always shown.** `setdefault` refuses a hidden
>   network as the default, and refuses to hide the default network. Give
>   both options in one command: `hw setdefault --show-network btctest4
>   --network btctest4`.
> - **Showing a shown network changes nothing.** Neither does hiding a hidden
>   one. `setdefault` writes the list and shows it, without an error. The same
>   network given to both options is refused.
> - **`--show-network` is where the help names every network.** The help of
>   `--network` names `btc` and `bch`, the networks of a new configuration.
> - **A default of `bip84` leaves Bitcoin Cash on `bip44`.** The networks of
>   Bitcoin Cash have `bip44` addresses only, whatever `default-type` says.
> - **An option still wins for one command.** `hw txs --verbose` or
>   `hw txs --unit btc` shows that one listing its own way, whatever the
>   defaults say.
> - **`hw setdefault --help` does not list `--unit`.** It is one of the
>   [options of every command](#options-of-every-command), which `hw --help`
>   lists. It goes before or after `setdefault`, as with any command.
> - **Each computer has its own defaults.** They are settings of the
>   configuration file, not of the database. The file of `exportwallet` does
>   not carry them.

#### `hw deleteportfolio`

`hw deleteportfolio --portfolio N` deletes a portfolio that was never used, in
the wallet of the active portfolio. It deletes a portfolio only when all of
these are true:

- It is not `portfolio0`.
- It is not the [active portfolio](#the-active-portfolio).
- It has the highest index in its wallet. `portfolio1` cannot be deleted while
  `portfolio2` exists.
- It has never had any activity, and gave out no address.

Otherwise it refuses, and says which rule stopped it.

```console
hw deleteportfolio --portfolio 2
hw switch 0 --wallet 1                      # to delete a portfolio of wallet1
hw deleteportfolio --portfolio 1
```

Each rule has its reason. A wallet always keeps `portfolio0`. Deleting it
would be pointless, since a new `portfolio0` would be the same portfolio, with
the same keys. The active portfolio is where the commands work, so the
database must keep it. Switch to another portfolio before you delete it:

```console
$ hw deleteportfolio --portfolio 1
Error: portfolio1 of wallet0 is the active portfolio: switch to another portfolio first
```

The highest index goes first, so that the portfolios of a wallet keep no gap
behind them. A portfolio that was paid is kept, since `recover` would only
find it again.

Activity is a coin, a transaction, or a pending transaction. An address counts
too, whether it was paid or not. A new portfolio has no address, since `hw`
generates one only when [`receive`](#hw-receive) gives it out, when a payment
takes one for its change, or when a file or a `sync` names it. Once you have
given out an address, someone may pay it next month. A portfolio deleted would
not see that payment until `recover` finds the portfolio again, and its labels
would be lost. So `hw` keeps it.

> **Good to know**
>
> - **`--portfolio` must be given.** A deletion names what it deletes, and
>   never takes it from the active portfolio. So a portfolio is never deleted
>   by accident.
> - **It looks in the wallet of the active portfolio.** To delete a portfolio
>   of another wallet, switch to a portfolio of that wallet first. A portfolio that wallet does not have is
>   refused, as in `wallet0 has no portfolio3: portfolios lists the
>   portfolios`.
> - **To stop using a mnemonic, delete its whole wallet** with
>   [`deletewallet`](#hw-deletewallet).

#### `hw deletewallet`

`hw deletewallet FINGERPRINT --wallet N` removes a wallet from the database,
with all of its portfolios and their addresses, labels, transactions, coins and
pending transactions. It is for a mnemonic you no longer use. Unlike
`deleteportfolio`, it deletes a wallet that has had activity. The funds stay on
the chain, and the mnemonic can still spend them.

```console
hw portfolios --verbose                     # shows the fingerprint of each wallet
hw deletewallet 4435aa1c --wallet 1
```

`--wallet` names the wallet and must be given. The fingerprint must be that of
the wallet, as `portfolios --verbose` shows it. It confirms which wallet to
delete. An index alone is easy to mistype, and the fingerprint tells one
mnemonic from another. `deletewallet` never takes the wallet from the active
portfolio, so that no wallet is deleted by accident.

The other wallets keep their names. When `wallet0` is deleted, `wallet1` is
still `wallet1`, and the next new wallet becomes `wallet0` again.

`deletewallet` refuses the wallet of the
[active portfolio](#the-active-portfolio) while the database has another
wallet. The commands would otherwise be left with no portfolio to work in.
Switch to a portfolio of another wallet first:

```console
$ hw deletewallet bc560f95 --wallet 0
Error: wallet0 holds the active portfolio, portfolio0 of wallet0: switch to a portfolio of another wallet first
```

The last wallet can be deleted, active portfolio and all. The database then
has no active portfolio, and the first portfolio created after it becomes the
active one:

```console
$ hw deletewallet bc560f95 --wallet 0
wallet0 has been deleted, with all of its portfolios. The database has no wallet left: the first portfolio created next becomes the active one
```

> **Good to know**
>
> - **The labels are lost.** [`exportwallet`](#hw-exportwallet) writes them to
>   a file first.
> - **`recover` brings the wallet back.** The mnemonic still finds every
>   portfolio that was paid, but not the labels, the locks or the pending
>   transactions.

#### `hw sync`

Updates the portfolios from the server: balances, transactions, coins and
pending transactions. It also finds addresses that another wallet of the same
mnemonic used. Run it before you prepare a payment and after you send one.

```console
hw sync
hw sync --network btc                       # one network only
hw sync --full
```

`sync` covers every portfolio of every wallet, on every network, the test
networks included, whichever portfolio is active. A portfolio has an account
on each network, and a payment may arrive on any of them. A payment to a
portfolio you are not working in is still seen. `--network` syncs one network
only.

`sync` works on the [hidden networks](#shown-and-hidden-networks) too, so that
a network is up to date the day you show it. Its answer leaves them out. It
gives no block for a hidden network, and does not say that the server of one
is down or missing.

Each portfolio and network that holds or changed anything gets a block of its
own: its balances, the block it is synced to, and what changed. The networks
where a portfolio holds nothing and nothing changed share one line. So do the
networks the server does not serve. With several wallets in the database,
each line names the wallet of its portfolio, as in `portfolio0 of wallet0  btc`,
and so does the status line. Here `hw` has one wallet and shows `btc`, `bch`,
`btctest4` and `btcreg`:

```console
$ hw sync
portfolio0  btc
  spendable 0.00150000 btc, 3 coins
  synced at block 975304
  1 transaction and 1 coin updated

portfolio0  nothing on bch and btctest4
portfolio1  nothing on btc, bch and btctest4

Not served by the store: btcreg
```

A transaction counts as updated when `sync` finds it new or in another block.
It also counts when the network replaced or dropped it, and `txs` then shows
it dead.

`sync` also looks at the [pending transactions](#hw-pendingtxs). One the
network lists is retired. One you sent that the server no longer knows is sent
again. One whose coin another transaction spent is marked in conflict. So is
one whose coin is gone, as after a reorganization that removed the payment the
coin came from.

The server may answer that it did not publish a transaction `sync` sent
again. `sync` then goes on, and names the transaction with what the server
said:

```console
$ hw sync
portfolio0  btc
  spendable 0.00100000 btc, 1 coin
  synced at block 975310
  3f2a9c1e not sent again: The store at https://api.haskoin.com/ refused the transaction: bad-txns-inputs-missingorspent

Next, delete the pending transaction that was not sent again, to free its coins:
  hw deletetx 3f2a9c1e
```

The transaction stays pending, and holds its coins, until you delete it. Only
you know whether to give it up: the same transaction may be worth sending
again later, when the fees of the network are lower for example.

`--full` downloads the data of the portfolios again and writes it anew. Use it
if the wallet shows something wrong. It keeps labels, locked coins and pending
transactions. A reorganization of the chain does not need it: `sync` sees one
by itself.

On a terminal, a line tells what `sync` is doing while it works, with a bar for
the steps whose size it knows. It starts with the network whose server `sync`
is checking. It names the hidden networks as it works on them, since it tells
what you are waiting for. The line goes to stderr and is gone when `sync` ends,
so scripts never see it.

> **Good to know**
>
> - **A network the server does not serve is no error.** A server seldom
>   serves every network. The public one has no regtest chain, for example.
>   `sync` names those networks in one line and syncs the others.
> - **A network whose server is down is an error.** `sync` leaves out that
>   network and says why. It still syncs the other networks, then exits with
>   an error, so that a script can tell.
> - **A hidden network whose server is down is no error.** `sync` says
>   nothing of it, and exits as a success when the shown networks are synced.
>   You asked not to hear about that network. It is synced the next time its
>   server answers.
> - **`sync --network` refuses a hidden network.** Show the network first.
>   `sync` without the option already syncs it.
> - **A server `hw` cannot reach is down for every network.** When the
>   connection fails or times out, or its TLS handshake fails, `sync` checks no
>   further network there. They would fail the same way, and a timeout takes
>   30 seconds each time. They are named as not synced, with that error. With
>   no network left, `sync` stops with that error.
> - **A server can also fail in the middle of a sync.** The portfolio `sync`
>   was working on is then left as it was before, and the other portfolios of
>   that network are not synced. The portfolios synced before stay synced, and
>   so do the other networks. `sync` names the network with the error, as not
>   synced, and exits with an error. Any other error, such as a no to one of
>   its questions, stops `sync`, and nothing is kept.
> - **A server that serves no network at all is an error too.** That is what a
>   wrong `host` in the configuration looks like.
> - **`sync` asks after a thousand used addresses in a row** whether to keep
>   looking, since a server that lies reports them without end. A `sync` that
>   cannot ask stops there.
> - **`sync` sends again what you sent.** A transaction the server no longer
>   knows is sent again, since nodes forget unconfirmed transactions. See
>   [`broadcast`](#hw-broadcast).
> - **A transaction the server does not publish again does not stop `sync`.**
>   The server answered, so what `sync` read from it stands. `sync` says so
>   on every run until you delete the transaction. A haskoin-store waits for
>   its nodes before it answers, 30 seconds by default, so such a `sync`
>   takes that much longer. A server that gives no answer at all is down, and
>   the sync of its network fails as for any other question.
> - **`sync` shows no next step when it found nothing new.** The steps after
>   a `sync` are there for what it found: new transactions, a conflict, or a
>   transaction that was not sent again.
> - **A pending transaction in conflict may be in another portfolio.** `sync`
>   and [`pendingtxs`](#hw-pendingtxs) both cover every portfolio. So the next
>   steps of `sync` are `pendingtxs`, which shows the transaction wherever it
>   is, and `deletetx`, which frees its coins, with no `switch`.
> - **New transactions may be in another portfolio.** `txs` lists the active
>   portfolio alone. When `sync` finds new transactions in other portfolios
>   alone, its next steps start with the `switch` to them, before `txs`.

### Addresses

#### `hw receive`

Gives the next receiving address of an account of the
[active portfolio](#the-active-portfolio). `--network` and `--type` pick the
account. Without them, it is a `bip44` address on `btc`, or what
[the defaults](#the-two-options) say. On a terminal, the QR code of the
address comes under it, for the payer to scan.

```console
hw receive --label "invoice 42"
hw receive --type bip84                     # a native SegWit address
hw receive --network bch                    # an address on Bitcoin Cash
hw switch 1                                 # then an address of portfolio1
hw receive --type bip84
```

The answer ends with the account of the address: its portfolio, its network
and its type.

```console
$ hw receive --label "Wages"
bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"
  Transactions: 0
  Received    : 0.00000000 btc
  Confirmed   : 0.00000000 btc
  Unconfirmed : 0.00000000 btc
  Spendable   : 0.00000000 btc
  Coins       : 0
  Created     : 10 Nov 2026 15:08:37 CET
  Account     : portfolio0  btc  bip44
```

You never create an account first. A portfolio has one for every network and
type already. The path `bip44/0/0` gives the type of the account, then the
path inside it. The full path of this address is `m/44'/0'/0'/0/0`, where the
third step is the index of the portfolio.

> **Good to know**
>
> - **`receive` stops after 20 unused addresses in a row** in an account. A
>   wallet restored from the mnemonic would not look further. Reuse one of
>   them, or raise `gap` in the [configuration file](#configuration).
> - **Bitcoin Cash has `bip44` addresses only.** `receive --network bch` gives
>   one whatever `default-type` says, and `--type bip84` is refused there.
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

`hw show [WORD]` shows one thing of the wallet, or a file of a transaction or
of a wallet, and changes nothing. The word tells what it is, by its shape:

| Word | What `show` shows |
|---|---|
| none | The newest receiving address of the account of the active portfolio, the first one `addresses` lists |
| `bip84/0/i` or `bip84/1/i` | The receiving address `i`, or the change address `i`, of the account of that type, derived from the public key, and cut when the wallet does not hold it. Without the type, as `/0/i`, of the account `--type` picks |
| An address | That address of the wallet, a change address too |
| A txid or a nosigHash | That transaction, pending or in the history |
| `TXID:INDEX` | That coin, with every detail, as `coins --verbose` shows it, then the account that holds it |
| Any other word | That file: a [transaction file](#transaction-files), as its portfolio sees it, or a file of a wallet, as [`importwallet`](#hw-importwallet) would take it |

A hash goes in full or by its [short id](#short-ids).

```console
hw show                                     # the newest receiving address
hw show bip84/0/7                           # the address at a path of the bip84 account
hw show /0/7                                # the same path, of the account --type picks
hw show bip44/1/2                           # a change address
hw show ADDRESS
hw show a1075db5                            # a transaction by its short id
hw show a1075db5:1                          # a coin
hw show unsigned.psbt                       # a transaction file
hw show wallet.jsonl                        # a file of a wallet
```

**An address.** An address the wallet holds is shown as `receive` shows a new
one, with its QR code on a terminal. Use it to give an address out again.
`show` hands out no new address. The address may be of any portfolio. Its
`Account` field says when it is not of the active one.

**A path.** `show` derives the address at a path from the public key of the
account. A path starts with the type of its account, as the views write it,
such as `bip84/0/7`. A path without a type, such as `/0/7`, is of the account
that `--type` picks, as for `receive`, and `--type` is refused with a path that
names its type. `--network` picks the network, in the active portfolio. It
needs no mnemonic and no server, so it runs on the offline computer too. There it checks an address the online computer gave
out: see [Check an address](#check-an-address-before-you-give-it-out).

An address the wallet holds is shown as any other. One the wallet does not
hold, as on the offline computer, is shown cut. A dot stands for each character
of its middle, and a note says how to check it:

```console
$ hw show bip44/0/308
bip44/0/308  169h8ryuSwhG..........Cpzv9Q8PGum5
  Account  : portfolio0  btc  bip44

Compare both ends with the address that receive gave out at bip44/0/308 on the
online computer. If they match, that address is of this account, and you can
give it out. If they differ, the online computer may have been tampered with,
and you should not give the address out. Nobody can make another address with
the same ends. This screen is for checking only, so the middle of the address is
hidden. Give out the address as receive showed it.
```

The ends carry at least 128 bits. To make an address with the same ends, an
attacker would have to try about 2^128 keys, which no computer can. The middle
hides enough that over a million addresses fit the cut address, so nobody can
pay it. That matters because the wallet never gave the address out. It may
lie past the gap of 20 that a restored wallet looks at, and no `sync` would
find a payment there.

**A transaction.** A transaction is shown in full, as each portfolio that
holds it sees it, whichever portfolio is active. A payment between two
portfolios of the wallet is shown once for each, outgoing in one and incoming
in the other. Each view names the address of the other portfolio with its path
and that portfolio, as in `bip84/0/5 of portfolio1`. The view of a portfolio
that is not the active one says so in its `Portfolio` field, and every path in
it names its portfolio. See
[Paths of another portfolio](#paths-of-another-portfolio). A payment between
two wallets names the wallet of every portfolio in both views, as in
`bip84/0/5 of portfolio1 of wallet0`. See
[How the views name a portfolio](#how-the-views-name-a-portfolio).

**A transaction file.** `show` shows the transaction in a file as its
portfolio sees it, with the name of that portfolio, without changing the
wallet. It finds the portfolio by itself, as `importtx` does. A transaction
between two portfolios is shown as the one that pays sees it, with its paths
and labels, whichever portfolio is active. When that one is not active, the
`Portfolio` field says so, and every path names its portfolio. The address
it pays in the other portfolio is named with its path and that portfolio when
the database knows it, since no file carries that. Run `show` on the offline computer before you sign,
and on the online computer before you send. The labels it shows are those the
file carries, so an offline computer sees the labels of the online wallet. A
file of another wallet that involves no portfolio of the wallet needs
`--network`: see
[Files of other wallets](#files-of-other-wallets).

**A file of a wallet.** A file that is neither a PSBT nor the hexadecimal of a
transaction is read as `importwallet` reads it: the file of `exportwallet`, or a
[BIP 329] file of another wallet. `show` shows what `importwallet` would make
of it, and changes nothing. Each wallet of the file comes with its portfolios.
The line of a portfolio says whether `importwallet` creates it, with its twelve
accounts, or finds it in the database already, with nothing to add. Then each
account comes with the labels
of its addresses and transactions, and the coins it locks or unlocks. Run it
before `importwallet`, to see what the file holds:

```console
$ hw show wallet.jsonl
wallet0  bc560f95  not in the database, and importwallet creates it
  portfolio0  importwallet creates it, 12 accounts to add
  portfolio1  importwallet creates it, 12 accounts to add

portfolio0  btc  bip44
  Address labels
    bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"

portfolio0  btc  bip84
  Address labels
    bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"
```

The line of an account names its portfolio alone when the file holds one
wallet, as above. When the file holds several wallets, it names the wallet
too, as in `portfolio0 of wallet1  btc  bip44`. The first lines of the view
name each wallet of the file either way, with its fingerprint.
`hw show wallet.jsonl --wallet bc560f95` shows one wallet of the file alone,
and `--portfolio` one portfolio, as `importwallet` would read them.

The labels of no account of the file come last, then what `hw` keeps no label
for and the lines that are not records, as `importwallet` counts them. A key
that `importwallet` would leave aside is shown with the reason. A file that
`importwallet` would refuse is refused here too, such as a file that holds a
key alone and no record.

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
> - **Give `show` the path as `receive` wrote it.** A portfolio has an address
>   at `/0/7` in each of its accounts. `receive` writes the path with the type
>   of its account, such as `bip84/0/7`, and `show bip84/0/7` derives the
>   address of that account. `show /0/7` derives the one of the default type.
>   Give `show` the `--network` of `receive` too. The row `Account` says which
>   account each command used.
> - **Without a word, the address may have been paid already.** `show` gives
>   out the newest address again, as it is. Run `receive` for a new one.
> - **`--wallet` and `--portfolio` go with a file of a wallet only.** They
>   name one wallet and one portfolio of the file, and narrow what is shown,
>   as they narrow what [`importwallet`](#hw-importwallet) reads. `--wallet`
>   takes the fingerprint of the wallet, such as `bc560f95`, since a file
>   holds no index. In a file of several wallets, `--portfolio` needs
>   `--wallet` beside it. The records of the portfolios left out are counted
>   on one line. A `--wallet` or a `--portfolio` the file has nothing of is
>   refused, as `importwallet` refuses it. With any other word, `show` refuses
>   the two options: "show takes --wallet and --portfolio only with a file of
>   a wallet, where they name one wallet and one portfolio of the file. The
>   rest is shown in the active portfolio, or where it is". A path and no word
>   are of the active portfolio. A transaction, an address and a coin tell
>   where they are.
> - **`--type` goes with a path, or no word.** It names the account, with
>   `--network`. `show` refuses it with anything else.
> - **`--network` goes with most words.** With a path or no word, it names the
>   account. With a transaction file, it is the network of a file that names
>   none. With an address or a coin, it tells apart one that the wallet has on
>   several test networks. `show` refuses it with a file of a wallet, since each
>   record of such a file names its network.
> - **A file that involves several portfolios may need a `switch`.** A
>   transaction between two portfolios is shown as the one that pays sees it,
>   whichever portfolio is active. When several portfolios pay, or when none
>   pays and the transaction pays several, `show` takes the active one among
>   them. When the active one is none of them, `show` names them. Switch to
>   one of them, and run `show` again.
> - **A file named like a hash, an address or a path is taken for one.** Type
>   its directory before it, as in `hw show ./a1075db5`. Tab offers such a file
>   once you type `./`.
> - **A word that is none of these and names no file is refused**, with the
>   forms `show` takes. A short id with a digit missing gets this answer, for
>   instance.
> - **A file of a wallet goes by the origins of its records.** A record goes
>   under the account its origin and its network name. `importwallet` places a
>   label by what the wallet holds, so it may place one that `show` shows
>   apart.

#### `hw addresses`

Lists the receiving addresses of the active portfolio, or its change addresses
with `--change`. The addresses come in sections, one for each network and type: the
`bip44` addresses on `btc`, then the `bip84` ones, then those on `bch`, and so
on. A title line above each section names the portfolio, the network and the
type. A section the portfolio has no address in is left out. `--network` and
`--type` show one network or one type only.

In each section, the newest addresses come first. Each takes two lines: its
type and its path, such as `bip44/0/0`, the address and its label, then what
it can spend and what it received, or that it is unused. `--verbose` adds its balances, its coins and when it was
created.

```console
$ hw addresses --network btc
portfolio0  btc  bip44

bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"
  unused

portfolio0  btc  bip84

bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"
  unused
```

`--tabular` shows each address on one row of a table, as `txs --tabular` does.
The columns are the type and the path, the address, what it can spend, what it
received, in how many transactions, and its label. Each section has its title and its own
table. An unused address leaves its amounts empty. The table of change
addresses has no label column, since change addresses take no label.

`--csv FILE` writes the addresses to a [CSV file](#listings): the wallet, the
portfolio, the network and the type of each, the address, its path, its chain
and its label, what it can spend, its confirmed and unconfirmed balances, what
it received, its coins and transactions, and when it was created. `Chain` says
`receiving` or `change`.

```console
hw addresses
hw addresses --network btc --type bip84     # one account, with no title line
hw addresses --change
hw addresses --verbose
hw addresses --tabular
hw addresses --change --tabular
hw addresses --csv addresses.csv
hw addresses --max 10 --after ADDRESS
```

> **Good to know**
>
> - **A path names its account by its type.** Each account has its own
>   `/0/0`, so a path starts with the type of its account, such as
>   `bip84/0/0`. The title of the section gives the portfolio and the network.
> - **`--after` goes on in the section of the address.** The listing then
>   goes on with the sections that follow.

#### `hw balances`

Lists the addresses of the active portfolio that hold coins, grouped by
network. The title line of each network gives its total: the value of every
coin of the portfolio there, what it can spend now, and the rest. On `btc`, the `bip44`
and the `bip84` addresses are listed together, as a payment spends them.
`--network` and `--type` show one network or one type only.

```console
$ hw balances
portfolio0  btc  total 0.00170000 btc, spendable 0.00150000 btc, unspendable 0.00020000 btc

bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"
  total 0.00120000 btc, spendable 0.00120000 btc, unspendable 0.00000000 btc

bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"
  total 0.00050000 btc, spendable 0.00030000 btc, unspendable 0.00020000 btc
```

On each network, the largest total comes first. Each address takes two lines:
its type and its path, the address and its label, or `change address`, then
its three amounts. The type tells the two `/0/0` apart. The unspendable
amount counts the coins a pending transaction or `lockcoin` locks, the
unconfirmed payments from other wallets, and the coinbase coins not yet
mature.

The totals repeat those of [`portfolios`](#hw-portfolios), on purpose. They are
the same figures, beside the addresses they add up. A total is that of one
network, since the coins of two networks are never added.

`--verbose` gives the total and the number of coins of each address, what it
can spend now, and what it cannot in three parts. They are what pending
transactions lock, what you locked with `lockcoin`, and what is not spendable
yet, which waits for a confirmation.

`--tabular` shows each address on one row of a table: its type and its path,
the address, the value of its coins, what it can spend now and the rest, and
its label, or `change address`.

`--csv FILE` writes the addresses to a [CSV file](#listings): the wallet, the
portfolio, the network and the type of each, the address, its path, its chain
and its label, its total, what it can spend, and the three parts it cannot.
Then come the columns the file of [`addresses`](#hw-addresses) ends with: its
confirmed and unconfirmed balances, what it received, its coins and
transactions, and when it was created. The two files split the same coins two
ways: this one by what you can spend, that one by what is confirmed.

```console
hw balances
hw balances --network bch
hw balances --type bip44                    # what the legacy addresses still hold
hw balances --verbose
hw balances --tabular
hw balances --csv balances.csv
```

#### `hw label`

`hw label [ADDRESS|TXID|PATH] LABEL` sets the label of a receiving address or
of a transaction. With the label alone, it sets the description of the active
portfolio, or of its wallet with `--of-wallet`. Each takes the place of the one
it had. An empty label, `""`, removes it.

| Words | What `label` labels |
|---|---|
| `ADDRESS LABEL` | That receiving address of the wallet |
| `bip84/0/i LABEL` | The receiving address `i` of the account of that type, in the active portfolio. Without the type, as `/0/i`, of the account `--type` picks |
| `TXID LABEL` | That transaction, by its txid, or by its nosigHash while it is pending |
| `DESCRIPTION` | The active portfolio, or its wallet with `--of-wallet` |

```console
hw label ADDRESS "rent from Bob"
hw label bip84/0/3 "rent from Bob"          # the address at a path
hw label /0/3 "rent from Bob" --type bip84  # the same path, of the account --type picks
hw label NOSIGHASH "rent of October"
hw label TXID "salary"
hw label TXID ""                            # removes the label
hw label "daily spending"                   # describes the active portfolio
hw label "paper backup in the safe" --of-wallet
hw switch 1                                 # then describe portfolio1
hw label savings
```

A label and a description do the same job. They say what a thing is for, in
your own words. So one command sets them all, and the words you type tell which
thing.

**An address.** The address tells its portfolio and its account, so `label`
takes no `--type`, and labels it whichever portfolio is active. Change
addresses take no label.

**A path.** A path names an address of the active portfolio, as the views write
it, such as `bip84/0/3`. It is a path of the receiving chain, `/0/i`, since
change addresses take no label. The portfolio must have given the address out,
as `addresses` lists them. A path without a type, such as `/0/3`, is of the
account that `--type` picks, or of the default type. `--type` is refused with
a path that names its type.

**A transaction.** Name a pending one by the hash `pendingtxs` shows, and one of
the history by its txid. Either goes in full or by its [short id](#short-ids). A
pending transaction keeps its label in its file, through signing on an offline
computer, and in the history once the network has it. A label set while the file
is away stays when the signed file comes back.

`send`, `preparetx`, `preparesendall` and `preparesweep` take a `--label`, and
`bumpfee` gives a replacement the labels of the transactions it replaces. A
transaction between two portfolios of the wallet gets the label in both.

**A portfolio or a wallet.** With the label alone, `label` describes the active
portfolio, or its wallet with `--of-wallet`. `portfolios` shows the
description, and the file of [`exportwallet`](#hw-exportwallet) carries it.
`label` answers with the portfolio, as `portfolios` shows it:

```console
$ hw label "paper backup in the safe" --of-wallet
* portfolio0  wallet0  "daily spending"
    wallet0 is "paper backup in the safe"
    created 11 Nov 2026
    btc  spendable 0.00000000 btc, no coins, 1 address
```

Wallets and portfolios have fixed names, which follow their indexes and cannot
be changed. To describe another portfolio, switch to it first. To describe
another wallet, switch to one of its portfolios.

> **Good to know**
>
> - **`--network` picks one of several.** The test networks share their keys,
>   so the wallet may have the same address on several of them. A portfolio
>   has an address at the same path on each network it gave one out on.
>   `--network` then says which one to label. Without it, `label` takes the
>   only one, or else the one on the default network.
> - **A word alone is a description.** If you leave out the label, as in
>   `hw label bip84/0/3`, the word you typed would replace the description of
>   the portfolio. So `label` refuses a word alone that reads as an address, a
>   path or a transaction id, and says to give the label after it. Such a word
>   cannot be a description.
> - **A description is free text on one line, and names nothing.** No command
>   takes one in place of an index, so two portfolios may have the same
>   description.

### Coins

#### `hw coins`

Lists the coins of the active portfolio, grouped by network. On `btc`, the
`bip44` and the `bip84` coins are listed together, as a payment spends them.
`--network`
and `--type` show one network or one type only. On each network, the coins
are listed in the order of the chain, the newest first, as [`txs`](#hw-txs)
lists the transactions. The unconfirmed coins come first, then the others by
their block and their position in it. The times shown follow that order
nearly, since a block can be dated before the block under it. Each coin takes
two lines:

1. The [short id](#short-ids) of the coin, such as `a1075db5:1`, which
   `lockcoin` and `--coin` take. Then the time of the transaction that paid it,
   its value, its state, spendable or locked, and its confirmations, up to
   `999+ confirmations`. A coin a pending transaction spends is `locked by tx`.
2. The address that holds it, in full, with its type and its derivation path,
   such as `bip44/0/0`, then the label of the address, or `change address`
   when it is one.

This is the one listing that gives an address of the wallet in full in its
compact view. What tells a coin apart is the address that holds it, more than
its path. The path gives the type of the coin, which the address tells too: a
legacy address holds a `bip44` coin, and a native SegWit one a `bip84` coin. It is also the one view
that keeps the label of an address in full on the line of the address, so that
each coin takes two lines. Everywhere else an address is in full, its label is
on the line below, and where a view gives only the path, the label stays on
the same line.

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

`--csv FILE` writes the coins to a [CSV file](#listings): the wallet, the
portfolio, the network and the type of each, the coin as `TXID:INDEX`, in full
and by its short id, its txid in full and by its short id, the index of its
output, the time of its transaction, its value, state, confirmations and
block, the address that holds it with its path and chain, and the labels of
the address and of the transaction. The txid has columns of its own, so that a
coin is matched with its transaction in the file of [`txs`](#hw-txs).

[`show TXID:INDEX`](#hw-show) shows one coin, with every detail, and the
account that holds it.

```console
hw coins
hw coins --network btc --type bip44         # the legacy coins on Bitcoin
hw coins --verbose
hw coins --tabular
hw coins --csv coins.csv
hw coins --locked
```

> **Good to know**
>
> - **Unconfirmed payments from others cannot be spent.** The sender can still
>   replace them until they confirm. The unconfirmed coins of your own
>   transactions can be spent: their change, and what one of your portfolios
>   pays another.

#### `hw lockcoin`

Keeps a coin from being spent. It takes the coin by its
[short id](#short-ids), as `coins` shows it, or in full. It shows the coin with
every detail, as `coins --verbose` does, then the account that holds it. That
field says when the account is not of the active portfolio.

```console
hw lockcoin TXHASH:1
hw lockcoin a1075db5:1                      # the short id coins shows
```

A coin tells where it is, so `lockcoin` locks it whichever portfolio is
active. The test networks share their keys, so the wallet may have the
same coin on several of them. `--network` then says which one to lock.

#### `hw unlockcoin`

Frees a coin locked with `lockcoin`. It shows the coin as `lockcoin` does,
and takes `--network` as `lockcoin` does.

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

#### `hw send`

`hw send ADDRESS:AMOUNT...` makes a payment in one command. It prepares the
transaction as [`preparetx`](#hw-preparetx) does, shows it, asks for the
mnemonic, signs the transaction as [`signtx`](#hw-signtx) does, and sends it as
[`broadcast`](#hw-broadcast) does. The payment is the one the three commands
make from the same options.

```console
hw send ADDRESS:0.0005 --fee-rate median --label "rent of October"
hw send ADDRESS1:10000sat ADDRESS2:250bits --fee-rate 5
hw send ADDRESS:0.01 --network bch --fee-rate low
hw send ADDRESS:0.01 --spend-from MYADDRESS --change-back MYADDRESS --fee-rate low
```

`send` needs the mnemonic and the server at the same time, so it runs on an
online computer. Use it for a wallet whose mnemonic you are willing to type
there. For funds in cold storage, pay in three steps and
[sign on the offline computer](#signing-on-an-offline-computer): `send` cannot
keep the mnemonic offline.

**The options** are those of [`preparetx`](#hw-preparetx), all but `--output`,
since `send` writes no file. They mean the same, and they build the same
transaction: the coins, coin control, the change, the lock time and the fee are
those of `preparetx`. Without `--fee-rate`, `send` shows the rates and asks.

**What you see.** `send` shows the transaction once, as `preparetx` answers
with it, with the chosen coins it could not spend below it. It then says that
typing the mnemonic signs the transaction and sends it, and asks for the
mnemonic:

```console
$ hw send 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0002 --fee-rate 10 --label "rent of October"
13 Nov 2026 17:45  Outgoing     -0.00022260 btc  not signed, 10 sat/vB
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  Label    : "rent of October"
  NoSigHash: f0ce68dc4e82845905a7c9ee8ae5d6c51d1451e21908a7e596689e8960e5e0b5
  Short id : f0ce68dc
  Created  : 13 Nov 2026 17:45:20 CET
  Fee      : 0.00002260 btc, 10 sat/vB
  Lock time: block 799947
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0
  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0

Typing the mnemonic signs this transaction and sends it to the network. This cannot be undone.
Mnemonic, or a share of a backup:
Passphrase, or nothing if the seed has none:
Sent, txid 8d9d52e56cbaafc4f2705852018b2211faae911f125be96282a693c663215a1a

Next, sync the wallet to follow the transaction:
  hw sync
```

Check the transaction there: the addresses, the amounts and the fee. Typing
the mnemonic is the confirmation. `send` asks nothing more before it sends,
and sending cannot be undone. Ctrl-C at the mnemonic stops it. The answer is
the txid alone, since nothing of the transaction changed after you read it.
`--json` answers with the transaction as sent, as `broadcast --json` does.

**When the payment does not go through.** What `send` leaves depends on
whether the transaction was signed.

- **Not signed: nothing is kept.** When `send` refuses the payment, when you
  answer no to a question, and when you stop at the mnemonic, the wallet is
  as it was. It has no new pending transaction, and no coin is locked. A
  mnemonic that does not match the portfolio is asked again, three times in
  all.
- **Signed and not sent: the transaction stays pending.** `send` keeps the
  signed transaction in the wallet before anything leaves the computer. When
  the sending fails, `send` says why, as `broadcast` does, and names the
  transaction by the [short id](#short-ids) of its txid. The transaction is
  then as `signtx` leaves one. `broadcast` sends it again, and
  [`deletetx`](#hw-deletetx) deletes it and frees its coins.

```console
$ hw send ADDRESS:0.0002 --fee-rate 10
...
Error: The store at https://api.haskoin.com/ refused the transaction: bad-txns-inputs-missingorspent. The transaction is signed and stays pending: broadcast 8d9d52e5 sends it again, and deletetx 8d9d52e5 deletes it and frees its coins
```

> **Good to know**
>
> - **`send` checks the server before it asks for the mnemonic.** A server
>   that does not answer stops it there, so you do not type the mnemonic for
>   nothing.
> - **`--yes` does not answer for the mnemonic.** It answers the two
>   questions of `preparetx`, on a high fee rate and on a wallet behind the
>   chain. A script types the mnemonic and the passphrase on the input of
>   `send`, a line each.
> - **The transaction shown is not signed yet.** It reads `not signed` and has
>   a nosigHash, as the answer of `preparetx` does. Its txid comes with the
>   answer.
> - **An interrupt after the mnemonic leaves the transaction pending.** Once
>   the mnemonic is typed, the transaction is signed and kept. If you stop
>   `send` while it waits for the server, [`pendingtxs`](#hw-pendingtxs) shows
>   the transaction signed. The next `sync` retires it if the network took
>   it. Otherwise send it with `broadcast`, or delete it with `deletetx`.
> - **`send` is for a payment of `preparetx` only.** A payment of everything
>   and a fee bump still take three steps: `preparesendall` or `bumpfee`,
>   then `signtx` and `broadcast`.

#### `hw preparetx`

`hw preparetx ADDRESS:AMOUNT...` prepares a payment, unsigned, and keeps it as a
pending transaction. It needs no mnemonic. It spends the coins of the
[active portfolio](#the-active-portfolio) on one network, the one `--network`
names or the default one. To pay from another portfolio, switch to it first.
[`send`](#hw-send) takes the same options, and goes on to sign the transaction
and send it.

```console
hw preparetx ADDRESS:0.0005 --fee-rate median --label "rent of October"
hw preparetx ADDRESS1:10000sat ADDRESS2:250bits --fee-rate 5
hw preparetx ADDRESS:0.01 --network bch --fee-rate low
hw preparetx ADDRESS:0.01 --coin-type bip44 --fee-rate low
hw preparetx ADDRESS:0.01 --spend-from MYADDRESS --change-back MYADDRESS --fee-rate low
hw preparetx ADDRESS:0.01 --change-back largest --fee-rate low   # back to the largest coin spent
hw preparetx ADDRESS:0.01 --spend-from MYADDRESS --select --fee-rate low
hw preparetx ADDRESS:0.01 --spend-from bip44/1/0 --fee-rate low   # an address by its path
hw preparetx ADDRESS:0.01 --recipients-pay --fee-rate high --output unsigned.psbt
```

| Option | What it does |
|---|---|
| `--network NET` | Names the network of the payment. Without it, it is the network of the coins chosen with `--coin` or `--spend-from`, or else the default network. With them, it only tells apart the test networks, which share their keys. |
| `--fee-rate RATE` | Sets the fee rate: a number of satoshis per virtual byte, or `low`, `median` or `high`. Without it, `hw` shows [the rates](#hw-fees) and asks. |
| `--recipients-pay` | Takes the fee out of the amounts paid, in equal shares. |
| `--min-conf N` | Filter: keeps the coins with at least N confirmations, among every coin or among those chosen. The default is 0. |
| `--coin-type TYPE` | Filter: keeps the coins of one type, `bip44` or `bip84`, among every coin or among those chosen. Without it, coins of both types. |
| `--strategy STRATEGY` | Sets how the coins are selected: `least-waste`, the default, `random-draw`, `largest-first` or `smallest-first`. |
| `--coin TXID:INDEX` | Chooser: adds a coin to the coins chosen, in full or by its [short id](#short-ids). Repeat it for several coins. |
| `--spend-from ADDRESS` | Chooser: adds the coins at an address to the coins chosen, given by itself or by its path, such as `bip44/1/0`. Repeat it for several addresses. |
| `--select` | Spends only what the payment needs among the coins chosen, as `--strategy` selects them. Without it, every coin chosen is spent. |
| `--change-back ADDRESS` | Sends the change to this address of the active portfolio, given by itself or by its path, instead of a new change address. |
| `--change-back largest` | Sends the change back to the address of the largest coin the payment spends, among the coins of the type of the change. |
| `--change-type TYPE` | Sets the type of the change, `bip44` or `bip84`: that of the new change address, or of the coin `--change-back largest` picks. |
| `--output FILE` | Also writes the transaction to a [file](#transaction-files). |
| `--label LABEL` | Gives the transaction a label, which goes with it to its file and stays once the network has it. |
| `--yes` | Answers yes to the questions on a high fee rate and on a wallet behind the chain. |

**The coins.** `preparetx` selects among every coin of the portfolio on the
network. So one transaction can spend `bip44` and `bip84` coins together. You
can move to native SegWit addresses and keep spending your legacy coins, with
no transfer between accounts first. A SegWit coin costs less to spend, and the
selection counts each coin at its own cost.

**Coin control.** Its options are of two kinds, choosers and filters.

- **The choosers, `--coin` and `--spend-from`, choose coins, and they add
  up.** The coins chosen are those of every `--coin` and of every
  `--spend-from`. The payment spends every one of them that can be spent, and
  no other, whatever `--strategy` says. With `--select`, the strategy selects
  among them instead.
- **The filters, `--min-conf` and `--coin-type`, narrow the coins down.**
  Without a chooser, they narrow every coin of the portfolio, and the strategy
  selects among the coins they keep. With a chooser, they narrow the coins
  chosen. A filter never adds a coin.

So `--coin-type bip44 --spend-from MYADDRESS` spends the coins at that address
only, not every `bip44` coin. To spend the coins at two addresses, give
`--spend-from` twice. A coin chosen with `--coin` or `--spend-from` that
`--coin-type` leaves out is refused, since the two options disagree about it.
A chosen coin with too few confirmations for `--min-conf` is not spent, and the
answer lists it as [`coins`](#hw-coins) shows it, with the reason.

**The change.** The change goes to a new change address of the portfolio. Its
type is the first of these that applies:

1. The type `--change-type` asks for.
2. The type of the coins the payment spends, when they are all of one type. A
   payment then stays within one type of address.
3. The `default-type` of the configuration, or `bip44`, when the coins are of
   both types.

On a network of Bitcoin Cash, every coin is `bip44`, so the change is too. An
option that asks for `bip84` there is refused.

**`--change-back ADDRESS`** sends the change to that address instead. It must
be an address of the active portfolio on the network of the payment. A receiving
address and a change address will both do, of either type. The address may
have been used already, or be the change of another pending transaction: you
chose it. A new change address that `hw` takes by itself is always one that
is free. `hw` refuses any other address, since change sent elsewhere would be
a payment. Some banks accept payments only from addresses they have checked,
and `--change-back` keeps the funds at such an address. The address has a
type of its own, so `--change-back ADDRESS` and `--change-type` do not go
together.

**`--change-back largest`** sends the change back to the address of the
largest coin the payment spends. You do not know in advance which coins the
strategy will select, so you cannot name such an address yourself. Use it to
keep the change at an address the payment already shows, instead of showing
a new one.

- **The coin is of the type of the change.** The change takes its type by the
  rule above, and the largest coin of that type gets it. The fee is counted
  for an output of that type, so the change cannot go to a coin of the other
  type. In a payment of one type, and on Bitcoin Cash, that is the largest
  coin spent. In a payment that spends both types, it is the largest coin of
  the `default-type`, even when a coin of the other type is larger.
- **`--change-type` sets the type.** `--change-back largest --change-type
  bip84` sends the change to the largest `bip84` coin spent. When the payment
  spends no `bip84` coin, `preparetx` refuses it and prepares nothing. Add
  `--coin-type bip84` to spend only `bip84` coins, or leave out
  `--change-type`.
- **Coins of equal value.** The coin at the lower path wins: a receiving
  address before a change address, then the lower index.
- **A payment without change** sends nothing back, and is never refused for
  it.

**An address by its path.** `--spend-from` and `--change-back` take the path of
an address of the active portfolio, as the views write it, such as
`bip44/1/0`. The path needs its type, since a payment spends both types and
`preparetx` has no `--type`. The portfolio must have the address, as
`addresses` and `addresses --change` list them. A path tells the networks
where the portfolio has an address at it, as an address tells its network.

> **Good to know**
>
> - **An amount without a unit is in bitcoins**, or in the unit of the
>   configuration. See [Amounts](#amounts).
> - **No output is smaller than what nodes relay.** That is 546 satoshis for a
>   legacy output, and 294 for a native SegWit one. A smaller payment is
>   refused, with what the recipient would get and the limit of its kind of
>   address. Smaller change goes to the fee.
> - **A transaction takes at most 100,000 virtual bytes**, the largest that
>   nodes relay. That is about 675 legacy coins, or 1,470 native SegWit ones.
>   `hw` refuses to prepare a larger one. Send the coins in several
>   transactions, choosing them with `--coin` or `--spend-from`.
> - **A wallet behind the chain asks before preparing.** The transaction gets
>   the lock time of the best block the wallet knows, as the transactions of
>   most wallets do. When that block is more than eight hours old,
>   `preparetx`, `send`, `preparesendall` and `bumpfee` ask whether to go on
>   with a lock time of 0, or to sync first. A `sync` first is the better
>   answer: it also finds the coins spent or received since. `--yes` goes on
>   without asking.
> - **`--coin` and `--spend-from` take the coins of the active portfolio.** A
>   coin or an address of another portfolio is refused, with the portfolio
>   that holds it and the switch to it, as in `The coin a1075db5:1 is in
>   portfolio1 of wallet0, not in the active portfolio: run switch 1 --wallet
>   0 first`. Every payment spends from one portfolio, and coin
>   control keeps to the one you work in.
> - **`--coin` and `--spend-from` tell the network.** The coins and the
>   addresses chosen are on one network, and `preparetx` pays there. A path
>   is on each network where the portfolio has an address at it.
>   `--network` must agree with them when given. It is needed when the coins
>   are on several test networks and none of them is the default network.
> - **The type of the change and the coins depend on each other.** The fee
>   depends on the type of the change output, and the type of the change on the
>   coins chosen. With coins of both types to choose from, `hw` chooses for
>   change of the default type first. When the coins chosen are all of the
>   other type, it chooses again among the coins of that type.
> - **Change sent back to an address the payment spends from is plain to
>   see.** `--change-back largest` always does that, and `--change-back` with
>   such an address too. The address shows on both sides of the transaction,
>   so anyone who reads it knows which output is your change. A new change
>   address keeps that uncertain.

#### `hw preparesendall`

`hw preparesendall ADDRESS[:AMOUNT]...` prepares a payment of every coin the
portfolio can spend on the network, of both types of address, with no change.
That is the spendable balance `portfolios` shows for that network. An address
with an amount gets that amount. The addresses without one share the rest and
pay the fee.

```console
hw preparesendall ADDRESS --fee-rate low
hw preparesendall ADDRESS1:0.1 ADDRESS2 --fee-rate median --leave-dust
hw preparesendall ADDRESS --network bch --fee-rate low
hw preparesendall MYSEGWITADDRESS --coin-type bip44 --fee-rate low
```

`hw` asks whether to spend the coins worth less than their fee, unless
`--spend-dust` or `--leave-dust` answers. It shows each of those coins as
[`coins`](#hw-coins) does, then the fee rates it is worth spending at.

The filter `--coin-type` sends the coins of one type only. The last example
moves every legacy coin of a portfolio to a native SegWit address of it, one
that `receive --type bip84` gave.

The answer lists every coin the transaction leaves in the portfolio, as
`coins` shows it, then the reason: dust, locked by you or by a pending
transaction, not spendable yet, not chosen with `--coin` or `--spend-from`, or
not of the type `--coin-type` sends. With `--json`, each coin left has the keys
of a coin of `coins --json`, and `reason` beside them.

`--network`, `--fee-rate`, `--output`, `--label` and `--yes` work as for
[`preparetx`](#hw-preparetx). So do the choosers and the filters of coin
control, `--coin`, `--spend-from`, `--min-conf` and `--coin-type`. The choosers
add up, and the filters narrow the coins down. The choosers send every coin
chosen that can be spent, dust included. The coins of `--coin` and
`--spend-from` must be in the active portfolio. They tell the network, and
`--network` must agree with them.

#### `hw signtx`

Signs a pending transaction by its nosigHash, in full or by its
[short id](#short-ids), or a transaction file. It first
shows the transaction, as [`show`](#hw-show) does, then asks for the
mnemonic and passphrase of the wallet, or for the shares of its backup. Check
what it shows before you type the mnemonic, and stop it with Ctrl-C if it is
not the payment you mean. Once signed, it prints the txid and the file it wrote.
`signsweep` shows the transaction in the same way before it asks for the keys.

```console
hw signtx NOSIGHASH
hw signtx --input unsigned.psbt --output signed.psbt
```

A transaction that spends `bip44` and `bip84` coins together is signed at once.
The mnemonic gives the keys of both accounts.

`signtx` finds the portfolio from the transaction, whichever portfolio is
active. A transfer between two portfolios is signed by the one that pays it.
What `signtx` shows first is then the view of that portfolio, and it says so
when that portfolio is not the active one.
When it cannot tell between several portfolios, it takes the active one if it
is among them. Otherwise it names them, and you switch to the one to use.
[What each command takes](#what-each-command-takes) gives the rule.

On an offline computer, a file of a portfolio the database does not have is
refused: "The transaction does not involve this wallet". That is the case of a
computer where you made `portfolio0` alone, given a file of `portfolio1`.
`newportfolio --existing` there adds the next portfolio of the mnemonic, and
`importwallet` adds the portfolios of a file of the whole wallet.

#### `hw broadcast`

Shows a signed transaction, asks, and sends it. It then waits up to ten seconds
for the server to see it. `--yes` sends without showing the transaction and
asking first, for scripts.

```console
hw broadcast TXID
hw broadcast --input signed.psbt
```

The server hands the transaction to its nodes. A haskoin-store of version 1.11
or later answers only once a node has announced the transaction back, and
says so when none did. `broadcast` then fails with what the server said, and
the transaction stays signed and not sent. The server says why when it can
tell that no node will take the transaction, as for a coin that is spent
already:

```console
$ hw broadcast 3f2a9c1e --yes
The store at https://api.haskoin.com/ refused the transaction: bad-txns-inputs-missingorspent
```

A node says nothing of a transaction it refuses for its fee. The server waits
for its nodes, 30 seconds unless its operator set another time, then answers
that it heard nothing:

```console
The store at https://api.haskoin.com/ handed the transaction to a node, and no node announced it in the time the store waits. A node says nothing of a transaction it refuses, so its fee may be too low for the network
```

A haskoin-store before version 1.11 answers at once, whatever came of the
transaction. The wait of `broadcast` is then the only sign that the network
took it.

> **Good to know**
>
> - **`sync` sends again what you sent.** When the server does not publish it
>   again, [`sync`](#hw-sync) says so on every run, with what the server
>   said, until you delete the transaction with [`deletetx`](#hw-deletetx).
>   That is what happens to a transaction the network refuses for good, for
>   a fee too low to relay for example.
> - **A payment that `sync` has seen is no longer pending.** `broadcast` has
>   nothing to send for it, and says so. [`txs`](#hw-txs) shows where it
>   stands. On Bitcoin, [`bumpfee`](#hw-bumpfee) replaces a payment that
>   stays unconfirmed.
> - **The server keeps a transaction its nodes dropped.** A haskoin-store
>   holds an unconfirmed transaction for days after the nodes forgot it.
>   `txs` then shows the payment unconfirmed for as long. `hw` does not send
>   it again, since `sync` saw it. On Bitcoin, `bumpfee` gets it moving
>   again. On Bitcoin Cash, wait for the server to drop it. `txs` then shows
>   it dead, its coins are back, and you can prepare the payment again.

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
offers to add confirmed coins of the portfolio, of either type, or to take what
is missing from what the recipients receive, asking which of them pays. It
shows the coins it offers to add as [`coins`](#hw-coins) does.

`bumpfee` finds the portfolio from the transaction, as `signtx` does: the one
that holds it, or among several the one that pays it. When it cannot tell, it
takes the active portfolio if it is one of them. Otherwise it names them, and
you switch to the one that pays.

`--output FILENAME` also writes the replacement to a new file, a PSBT, as
`preparetx --output` does. `--yes` answers yes to every question but the choice
of which recipient pays, which it cannot make for you.

Once the replacement is sent and `sync` has seen it, [`txs`](#hw-txs) shows the
stuck payment dead, and names the replacement on it: `replaced by 2b69e4f4`.
To bump again, name the replacement. `bumpfee` with the txid of the dead
payment refuses, and names the replacement to bump instead.

> **Good to know**
>
> - **Change sent back with `--change-back` may count as a payment.** Change
>   sent back to an address the payment spends from is still change, and pays
>   the higher fee. `--change-back largest` always sends it to such an
>   address. Change sent to another receiving address cannot be told
>   from a payment to yourself. The replacement keeps it, and does not take the
>   extra fee out of it.

### Transactions

#### `hw txs`

Lists the transactions of the active portfolio, grouped by network, as of the
last sync. `--network` shows one network only. On each network, the
transactions are listed in the order of the chain, the newest first. The
unconfirmed ones come first, each above the ones it spends from. The confirmed
ones follow, by their block and their place in it. The balance of the
portfolio changed in that order, so the balance shown after each transaction
is one the portfolio held. [`show TXID`](#hw-show) shows one of them in full.

A transaction is shown as the portfolio sees it, whichever of its accounts it
touches. One that spends `bip44` and `bip84` coins together is one
transaction, with one amount. Each takes two lines:

1. Its [short id](#short-ids), the first 8 digits of its txid, then its time, in
   the time zone of the computer, its amount and its status.
2. The balance of the portfolio on the network after the transaction, then what
   the transaction is for.

What a transaction is for is said as the last column of the table says it:

- A payment received gives the address it was paid at, by its type and its
  path, then the label of the payment, or else the label of the address, as in
  `at bip84/0/3 "Salary"`. A change address says `change address` instead.
- A payment gives its label, or else the address it pays, as in
  `to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU`.
- A payment to another portfolio of the wallet gives the path of the address
  it pays and that portfolio, then its label, as in
  `to bip84/0/5 of portfolio1 "Savings"`. See
  [Paths of another portfolio](#paths-of-another-portfolio).
- Each says how many other addresses it pays, as in `and 2 more`.

The type comes before the path because a portfolio has an address at `/0/3` in
each of its accounts on a network, as in every view. Nothing in the line is
cut, a long label or address included. On a terminal, the addresses of the
wallet and their labels are blue, which sets them apart from the addresses of
others.

The amount has a plus sign when it comes in. The status counts the
confirmations up to `999+ confirmations`. An unconfirmed transaction shows its
fee rate too. The balance of the newest transaction is what the portfolio holds
on the network, confirmed or not. A dead transaction, replaced or dropped from
the network, shows no balance and does not count.

A dead transaction that another one replaced names that one first on its second
line, by its short id. Here a payment was bumped with
[`bumpfee`](#hw-bumpfee), and the replacement waits for a block:

```console
2b69e4f4  14 Nov 2026 23:13     -0.00024520 btc  unconfirmed, 20 sat/vB
  balance 0.00025480 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
89c70fb2  14 Nov 2026 23:02     -0.00022260 btc  dead
  replaced by 2b69e4f4, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
```

The note tells a payment that went out in another transaction from one that
never went out. `hw` reads it from the coins each time it shows the
transaction. The replacement is the live transaction that spends a coin the
dead one spent. A dead transaction that names none was dropped, when the
portfolio made it. See the notes below for what the note cannot tell.

`--verbose` starts each transaction with its time, then its type:

| Type | Meaning |
|---|---|
| `Incoming` | Funds enter the portfolio. |
| `Outgoing` | Funds leave the portfolio. |
| `Internal` | Funds move inside the portfolio, from a `bip44` address to a `bip84` address for example. |

The type is relative to the portfolio, not to an account. A move between the
two accounts of a portfolio changes nothing of what you hold, so it is
internal. A payment from `portfolio0` to `portfolio1` is outgoing in
`portfolio0`, and incoming in `portfolio1`.

The verbose view then gives the network, and the types of the portfolio's
addresses the transaction involves. It shows the label as a field, the txid in
full and its short id, every address in full with its amount, and its path for
an address of the wallet, the fee and the balance. It gives the full count of
the confirmations. The label of an address of the wallet is on the line below
it, under the address. A change address the transaction spends from or pays
says `change address` there instead. An address the transaction spends from
adds the labels of the coins spent, as `coins` shows them, after the word
`from`. Before you sign, they tell which coins a payment spends, and so which
payments it links. Long labels wrap at the edge of the screen, each line again
under the address.

A dead transaction has a `Status` field in the verbose view, which says what
became of it:

| Status | Meaning |
|---|---|
| `replaced by` | Another transaction took its place. The txid of each replacement follows in full, on the lines below. |
| `dropped from the network` | A payment of the portfolio that nothing replaced. It never went out. |
| `replaced, or dropped from the network` | Any other dead transaction, such as a payment received. The portfolio cannot tell which. |

`--tabular` shows each transaction on one row of a table, as a spreadsheet
does. The columns are the short id, the time, the type, the amount, the balance
after it and the confirmations. The confirmations are a number alone, up to
`999+`. An unconfirmed transaction has 0, and a dead one says `dead`. The last
column, Description, says what the transaction is for, in the words of the
compact view. The description of a dead transaction that was replaced starts
with its replacement, as in `replaced by 2b69e4f4, to 1JCq…`. The header names
the unit of the amounts, so the cells hold numbers alone. The table leaves out
the fee rate. `txs` without `--tabular` shows it.

`--csv FILE` writes the transactions to a [CSV file](#listings). Each row
starts with the wallet, the portfolio and the network of the transaction, and
the types of address it involves. Then come its txid, in full and by its short
id, its time, its direction, amount, fee, fee rate and size, the balance after
it, its status, confirmations and block, and its label. `Replaced by` gives
the txid of the transaction that replaced a dead one, in full.

A transaction can spend from any number of addresses and pay any number, and a
file has a fixed number of columns. So the file names three addresses of each
transaction, as `--verbose` lists them under From, To and Change: the one it
spends the most from, the one it pays the most, and its change. Each has its
path and label when it is one of the portfolio, and its value. `Other
addresses` counts the addresses that have no column of their own. The lock time
comes last. The spreadsheet of [`exportsheet`](#hw-exportsheet) lists every
address of every transaction, in a tab of its own.

```console
hw txs
hw txs --network bch
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
> - **The balance is that of one network.** It adds up the accounts of the
>   portfolio on that network. Each section of the listing has its own
>   running balance.
> - **The list follows the chain, not the times.** A confirmed transaction
>   has the time of its block, which its miner set. An unconfirmed one has
>   the time the server first saw it. A block is now and then dated before
>   the block under it, by a minute or two on Bitcoin. Miners of test
>   networks often date blocks ahead, by up to two hours, so a time can be in
>   the future. The times then run out of order down the list, and the
>   confirmations never do. Listed by time, a payment could come under the
>   one it spends from, with a balance the portfolio never held beside it.
> - **The balance beside a confirmed transaction is final.** It changes only
>   if the chain is reorganised, or when a sync finds an older transaction of
>   the portfolio at an address generated since.
> - **A dead transaction stays where it was** when the wallet last saw it
>   alive. One that was confirmed keeps the place of its block. One that was
>   unconfirmed stays above the blocks the wallet knew then, and below the
>   ones that came later. So it goes down the list as blocks come, and a
>   block that confirmed another transaction in its place comes above it.
> - **`replaced by` follows the network, not `bumpfee`.** The network keeps
>   one of a payment and its replacement, and a miner may still confirm the
>   stuck payment. The replacement then turns dead, and it is the one that
>   says `replaced by`, with the id of the payment that confirmed. The note is
>   never stored, so it is always as of the last sync. It also names a
>   replacement made on another computer that holds the same keys.
> - **A replacement need not pay the same recipients.** It is the transaction
>   that took the coins. One made by `bumpfee` pays the recipients of the
>   payment. Any other may pay somebody else, so look at it with
>   [`show`](#hw-show) before you count a dead payment as made.
> - **A payment that spent the change of a replaced payment names the same
>   replacement.** It fell with that payment. `bumpfee` replaces both with one
>   transaction that pays the recipients of both.
> - **A dead payment you received names no replacement.** Only the coins of
>   the portfolio tell what replaced a transaction, and the payer spent their
>   own. When the payer bumps the fee, the replacement is in the list too,
>   paid at the same address.
> - **A payment that no sync saw leaves no dead transaction.** When you bump a
>   payment before any sync has seen it, the history never holds it. `txs`
>   then shows the replacement alone.
> - **The CSV file and the spreadsheet name the replacement by its txid in
>   full.** Their Status column says `dead`, and `Replaced by` gives the txid.
>   Several replacements share the cell, apart by a comma and a space. `txs
>   --json` gives them under `replacedby`.
> - **The CSV file leaves the value of a payer's address empty.** In a payment
>   you received, the addresses under `From` are of another wallet. Its coins
>   are nothing to yours, so `hw` shows none of their values, as on the screen.
> - **A transaction between your own addresses fills no `Change` column.** No
>   output of it can be told to be the change. The address it pays the most is
>   under `To`, and `Other addresses` counts the rest.
> - **A block of Bitcoin Cash is sorted by transaction id.** It can put a
>   payment before the one it spends from. `txs` lists such a payment right
>   above the one it spends from, and moves no other, so the balance stays
>   right. `coins` keeps the order of the block.
> - **A server older than haskoin-store 1.9.0 keeps the time it first saw a
>   transaction** once the transaction is confirmed. A payment that waited
>   for its block then shows an older time than the transactions under it.

#### `hw pendingtxs`

Lists the pending transactions of every portfolio of every wallet: prepared,
signed, or sent and not yet seen by `sync`. Unlike the other listings, it does
not keep to the [active portfolio](#the-active-portfolio). Each pending
transaction waits for you to sign, send or delete it, and one forgotten in
another portfolio keeps its coins locked. The commands that take one find its
portfolio by themselves, so none needs a `switch`. `--network` shows one
network only. A transaction pending on a
[hidden network](#shown-and-hidden-networks) is not listed: it locks coins of
that network alone.

The paths of a transaction of another portfolio than the active one name that
portfolio, as in `bip44/1/0 of portfolio1`. See
[Paths of another portfolio](#paths-of-another-portfolio).

They take two lines each, as in `txs`. The first starts with the
[short id](#short-ids) of the nosigHash until the transaction is signed, then
with that of its txid. It gives the time the transaction was created, its
amount and its status, with its fee rate. The second says where the
transaction is, by its portfolio and its network, and by its wallet too when
the database has more than one, as in `portfolio0 of wallet1 btc`. It then
names the
transactions a replacement replaces, in yellow on a terminal, and says what the
transaction is for. `--verbose` names those transactions in full, in yellow too.
A conflicted one can no longer be sent. [`show HASH`](#hw-show) shows one of them
in full.

It is one list rather than a list in sections, sorted by wallet and portfolio,
then by network in the order `portfolios` shows the networks, and the newest
first on each network. Each transaction names its place instead. The tab of
pending transactions of [`exportsheet`](#hw-exportsheet) holds them too.

`--tabular` shows each transaction on one row of a table: its short id, its
wallet when the database has more than one, its portfolio and its network, the
time it was created, its type, its amount, its status, its fee rate in sat/vB,
and what it is for, as `txs --tabular` says it. The description of a
replacement starts with the transactions it replaces, in yellow, as in
`replaces a1075db5, to bc1q…`. The wallet, portfolio and network columns are
as wide as their longest cell.

`--csv FILE` writes the pending transactions to a [CSV file](#listings), in the
columns of [`txs --csv`](#hw-txs) that apply to them, in the same order. The
nosigHash comes before the txid, each in full and by its short id, and the
txid is empty until the transaction is signed. `Change in fee` gives the
change the fee took in, too small for an output. `Replaces` gives the txids of
the transactions a replacement of `bumpfee` replaces. The file has no time on
the chain, no balance, confirmations or block. When the transaction was
created comes last.

```console
hw pendingtxs
hw pendingtxs --verbose
hw pendingtxs --tabular
hw pendingtxs --csv pending.csv
hw pendingtxs --network bch
```

> **Good to know**
>
> - **A pending transaction locks its coins.** They stay locked until `sync`
>   sees the transaction on the network, or until `deletetx` deletes it. Delete
>   the transactions you abandon.
> - **A replacement is yellow until the network takes one transaction.** The
>   network keeps only one of a replacement and the transactions it replaces.
>   Until a block holds one of them, the payment stands in both, and the
>   replaced one is still in `txs`. Once `sync` has seen the replacement on
>   the network, it leaves this list. [`txs`](#hw-txs) then shows the replaced
>   transaction dead, with the words `replaced by` and the short id of the
>   replacement.
> - **The wallet is named only when there are several.** A database of one
>   wallet has no other to tell it from, so the views leave its name out.
>   With two wallets or more, every view names the wallet of each
>   transaction, `wallet0` too. Every view of `hw` follows this rule: see
>   [How the views name a portfolio](#how-the-views-name-a-portfolio).

#### `hw exporttx`

`hw exporttx HASH FILE` writes a pending transaction to a
[file](#transaction-files).

```console
hw exporttx NOSIGHASH unsigned.psbt
```

#### `hw importtx`

`hw importtx FILE` adds a [transaction file](#transaction-files) to the pending
transactions of a portfolio, or replaces the pending one with its signed
version. It keeps the labels of the wallet.

```console
hw importtx signed.psbt
```

`hw` finds the portfolio by itself, from the addresses the transaction spends
from and pays. A transaction that moves funds between two portfolios goes to
the portfolio that pays, since that one must lock the coins and sign. That
holds whichever portfolio is active.

`hw` cannot tell when several portfolios pay, or when none pays and the
transaction pays several. It then takes the active portfolio if it is one of
them. Otherwise it names them, and imports nothing. Switch to the one to use,
and import the file again.

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

`--label LABEL` gives the sweep a label, which goes with it to its `--output`
file. `--yes` takes a fee rate above 100 satoshis per virtual byte without
asking, as for `preparetx`.

A sweep that pays an address of this wallet belongs to the portfolio of that
address, on its network. `hw` finds both from the `--sweep-to` addresses. When
those addresses belong to several portfolios, the sweep is of the active one
if it is among them. Otherwise `hw` names them, and you switch to the one to
keep the sweep in. A sweep is on one network, so `--sweep-to` addresses of
this wallet on two networks are refused.

A sweep that pays no address of this wallet needs `--network` and `--output`,
and no portfolio. It goes from file to file:

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
>   Cash.** That is why a sweep that pays no portfolio of the wallet needs
>   `--network`, and the default network does not apply to it. A wrong guess
>   would sweep the coins of the other chain.
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

A sweep that pays no portfolio of this wallet needs no portfolio here either.
It is signed on the network its file names.

### Messages

#### `hw signmsg`

`hw signmsg ADDRESS|PATH MESSAGE` signs a message with the key of an address of
the wallet. It asks for the mnemonic of the wallet of that address. `--output`
writes the signed message to a file instead of printing it.

An address must be one the wallet holds, one this computer gave out. The
offline computer gives out none, since `receive` runs online. There, give the
path of the address instead, with its type, as `addresses` shows it online,
such as `bip84/0/3`. `signmsg` derives the address at the path from the public
key of the account, as [`show`](#hw-show) does. It is in the active portfolio,
on `--network` or the default network. The signed message names that address,
so you see which one signed.

```console
hw signmsg MYADDRESS "I control this address" --output signed.txt
hw signmsg bip84/0/3 "I control this address"   # on the offline computer
hw signmsg bip44/0/0 "I control this address" --network bch
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
> - **The address tells its portfolio.** `signmsg` signs with it whichever
>   portfolio is active. `--network` is needed only when the wallet has the
>   address on several test networks, which share their keys, and none of them
>   is the default network.
> - **Sign with an address you gave out.** A path takes any index, and the
>   signed message shows its address in full. An address far past the ones you
>   gave out may lie past the gap of 20 that a restored wallet looks at, and no
>   `sync` would find a payment there.

#### `hw verifymsg`

Checks a signature from `hw` or another wallet, from any P2PKH or SegWit address
of one key, whatever header byte of [BIP 137] the wallet that made it chose, 27
to 42. Some wallets read other header bytes too. So for a P2PKH address,
another wallet may find valid a signature that `hw` does not, or the other way
around.

```console
hw verifymsg --input signed.txt
hw verifymsg ADDRESS SIGNATURE "I control this address"
```

### The wallet

#### `hw backup`

Copies the database, to a file you name or into `~/.hw/backups`. The copy holds
every wallet and portfolio, with their addresses, labels, coins, transactions
and pending transactions. It holds no private key.

```console
hw backup
hw backup ~/hw-2026-10-30.sqlite
```

To restore a backup, replace `~/.hw/accounts.sqlite` with the copy, or point
`database-file` in `~/.hw/config.json` to it. Then run any command. A copy of
the database of an earlier version is refused, and left as it is, as the
database itself is. So is a copy made by a newer `hw`.
[Upgrading from an earlier version](#upgrading-from-an-earlier-version) tells
how to create the wallet again instead.

#### `hw exportsheet`

`hw exportsheet FILE` writes the six listings to a new spreadsheet, a tab each:
the portfolios, the addresses, the balances, the coins, the transactions and
the pending transactions. The file is an OpenDocument spreadsheet (`.ods`), the
format of LibreOffice, which Excel and Google Sheets open too. Each tab holds
every row of its listing, in the columns of its [CSV file](#listings), in the
same order. Rows copied from a CSV file fit under the headings of its tab, and
the other way round.

Two more tabs hold the addresses of the transactions. `Transaction addresses`
comes after the transactions, and `Pending transaction addresses` after the
pending ones. The row of a transaction has room for three addresses, and
counts the others. These tabs have a row for every address instead, however
many a transaction spends from or pays. So you can filter on an address, sum
by label, or find every payment to one address, even where it was not the one
paid the most.

| Column | What it holds |
|---|---|
| `Wallet`, `Portfolio`, `Network` | Where the transaction is |
| `Type` | The type of the account that holds the address, empty for an address of another wallet |
| `Txid`, `Short txid` | The transaction, which matches the row with its row in the tab of the transactions. A pending transaction has its nosigHash first |
| `Time`, `Direction`, `Status` | Those of the transaction, repeated on each of its rows. A pending transaction has no time here, and ends with when it was created |
| `Role` | `from` for an address it spends from, `to` for one it pays, `change` for its change, as [`show`](#hw-show) lists them |
| `Value (sats)` | What the transaction spends from the address or pays it |
| `Address`, `Path`, `Chain`, `Address label` | The address in full, then where it is and its label, when the wallet holds it |
| `Held by wallet`, `Held by portfolio` | The portfolio that holds the address, empty for an address of another wallet |
| `Transaction label` | The label of the transaction, repeated on each of its rows |

Under each role, the address of the largest value comes first. It is the one
the row of the transaction names. `--csv` writes no file of these rows.

The tabs hold every portfolio of every wallet, whichever one is active, on the
[networks `hw` shows](#shown-and-hidden-networks). A hidden network has no row
in any tab. Each
row starts with the columns that say where it is: its wallet, its portfolio,
its network and its type. So one tab sorts and sums across the wallet. The tab
of the portfolios says which one is active, in its column Active. The
addresses of both chains share a tab, the receiving ones first, and the column
Chain tells them apart.

Each cell keeps its kind, unlike in a CSV file. A label is text, whatever it
starts with, and keeps its leading zeros. An amount is a number of satoshis,
shown with separators of thousands. A time is a date. It comes in UTC, then
on the clock of the computer, and a third column says how far that clock was
from UTC. The header of each tab stays at the top as its rows scroll. The
listings are read at one moment, so the tabs agree with each other.

```console
hw exportsheet wallet.ods
```

> **Good to know**
>
> - **Amounts of Bitcoin and of Bitcoin Cash share a column** when the wallet
>   holds both. The Network column tells them apart: filter on it before you
>   sum.
> - **Filter the dead transactions out before you sum the addresses.** The
>   tab of the addresses keeps the rows of a dead transaction, as the tab of
>   the transactions keeps the transaction. Their Status says `dead`.
> - **A payment between two of your portfolios is in the tab twice.** It is a
>   transaction of each portfolio, so each lists its addresses. The first
>   columns say whose transaction a row is of, and `Held by portfolio` says
>   who holds the address.
> - **A payment you received lists your own addresses under `to`.** The other
>   addresses its payer paid, their change among them, are nothing to your
>   portfolio and have no row. An address the payer spent from has a row
>   without a value.
> - **An input or an output without an address has no row.** A note written
>   in the chain pays no address, for instance.
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
- The keys of the portfolio in the inputs still to sign and in the outputs,
  with their derivation paths.
- The sighash to sign with.
- Once signed, the final script of each input. A signed input names no key any
  more, as the format asks of a finalized input and as other wallets write it.
  The file still says which address of the portfolio the input spends from, in
  a field of `hw`.

What is `hw`'s own goes in the proprietary fields of the format, under the
identifier `hw`, which other software leaves aside:

- The network.
- The fee rate the transaction was built for.
- The transactions a replacement replaces.
- The label of the transaction.
- The labels of the portfolio's addresses in it.
- The labels of the coins it spends, as the wallet has them when it writes the
  file.
- The path of the address each input spends from, such as `/0/5`. The format
  lets a signed input keep the fields it does not define, so this one stays
  once the input is signed, by `hw` or by another wallet. `hw` then knows the
  input as the portfolio's on any computer that holds the portfolio, such as an
  offline one that never handed out an address. It derives the address at that
  path in each account of the portfolio, and checks which one the coin pays.

Anyone who handles the file can change its labels. `hw` shows a label with its
control characters written out, such as `\x1b` for an escape or `\n` for a
line break, so that a label cannot rewrite what the screen shows, a recipient
for instance.

The file names the keys by the fingerprint of the mnemonic and their full
path, which a hardware wallet needs to find its own. Every wallet has its
fingerprint from the moment it exists. `newportfolio` and `recover` take it
from the mnemonic, and `importwallet` from the file of the wallet. A file of
another wallet may name a key of the portfolio by the fingerprint of the
account's own key instead. `hw` reads it, and names the fingerprint of the
mnemonic in the file it signs.

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
- **`signtx --input`** signs a file another wallet prepared for a portfolio of
  the wallet, with the sighash ALL only, and only when every input that is not
  the portfolio's is signed already. The signed file keeps every field of it,
  but those of the inputs signed that served the signers, and gains the path
  of the address each of them spends from.

Such a file names no network. The portfolio it involves tells it. The test
networks share their keys, so a file may involve the same portfolio on several
of them. `--network`, or the default network, then decides. A file that
involves no portfolio of the wallet, for `show`, `broadcast` or `signsweep`,
is read on `--network`, or on the `default-network` of the configuration file.

A file is refused when it involves no portfolio on the network it is read on,
and involves one on another network. `hw` then says "The transaction involves
an account of another network". The network named is the wrong one, as with a
wrong `--network` or a file of the other chain. `hw` does not take such a file
for a transaction outside the wallet, since it would then be signed or sent on
the wrong chain.

### Paths `hw` refuses

`hw` signs for the addresses of an account alone, at `/0/n` and `/1/n`. A file
that names a key of an account at any other path is refused, by `show` as
by `signtx`. An output there would be the account's, and yet never seen by it,
since `hw` scans those two chains alone.

An output of an account more than a thousand addresses past the last one of
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
| `host` | `https://api.haskoin.com/` | The address of the haskoin-store server, with or without a slash at the end |
| `default-network` | `btc` | The network of a command given no `--network`. It must be one of `shown-networks`. |
| `shown-networks` | `["btc", "bch"]` | The [networks `hw` shows](#shown-and-hidden-networks). The others are hidden: left out of every answer, and refused as a `--network`, while `sync` and `recover` still work on them. |
| `default-type` | `bip44` | The type of address of a command given no `--type`: `bip44` or `bip84`. The networks of Bitcoin Cash take `bip44` whatever it says. |
| `unit` | `btc` | The unit of the amounts of a command given no `--unit`: `btc`, `bit` or `sat` |
| `listing-view` | `compact` | The [view of a listing](#listings) given no `--compact`, `--verbose` or `--tabular`: `compact`, `verbose` or `tabular` |
| `gap` | `20` | How many unused addresses in a row `receive` gives out and `sync` looks at, in each account |
| `parallel-requests` | `1` | How many requests go to the server at once. The public server takes one at a time, so keep it at 1 unless you run your own haskoin-store. |
| `database-file` | `null` | Another database, instead of `~/.hw/accounts.sqlite` |
| `pager` | `null` | The pager a listing goes through on a terminal, instead of `less` or `PAGER`. `""` is none. |

`hw` talks to the server in JSON, which every version of haskoin-store
answers. A server of version 1.10 or later answers in nothing else.

When the server answers with an error, `hw` shows it and names the server. An
error `hw` does not know is shown with its name and its message. A newer
haskoin-store may answer one. So does the public server, for a network whose
server is down:

```console
The store at https://api.haskoin.com/ answered with an error: StringError "Bad Gateway"
```

`sync` and `recover` first ask the server whether it is well, and work on no
network whose server says it is not. `hw` says what the server reports as
wrong, so that you can tell a server that is catching up from one that lost
its nodes:

```console
The store at http://localhost:3000/ says it is not healthy: its index is 6 blocks behind the chain it knows, and it has no peer
```

A server also says it is not well when its last block or its last transaction
is older than it allows. A test network can stay that quiet for hours. The
message then gives both ages, as in `its last block is 5 hours old, and it
allows 4 hours`. The operator of such a server raises its `--block-timeout`
and `--tx-timeout`.

An answer that is no error of haskoin-store at all, such as the error page of
another proxy, is shown with its HTTP status alone:

```console
The store at http://localhost:3000/ answered with an error hw could not read: HTTP 502
```

A proxy answers `502` when the server behind it is down, and `429` when it
limits the requests. `hw` does not show the page that came with the status,
which is written for a browser.

The two `default-` keys say which account of the portfolio a command takes when
no option does. `unit` and `listing-view` say how the answers look, and
`shown-networks` which networks they show.
[`setdefault`](#hw-setdefault) writes these five keys, so that you do not have
to edit the file. It is the one command that changes the file.

The portfolio a command works in is not in the file. It is the
[active portfolio](#the-active-portfolio), which the database keeps and
[`switch`](#hw-switch) changes.

The default network and type do not apply everywhere. `sync` and `recover`
cover every network, the hidden ones too. The listings `addresses`,
`balances`, `coins` and `txs` show the whole portfolio on the shown networks,
and `pendingtxs` every portfolio. A sweep that pays no portfolio needs its
`--network`.

The networks are `btc`, `bch`, `btctest`, `btctest4`, `btcreg`, `bchtest`,
`bchtest4` and `bchreg`. `shown-networks` lists some of them, at least one,
and `hw` keeps them in this order. `hw` does not start with a file whose
`default-network` is not in `shown-networks`, or whose `shown-networks` names
no network or one it does not know. It says which key to change:

```console
$ hw portfolios
The configuration file /home/you/.hw/config.json could not be read: Error in $: The key "default-network" names btctest4, which the key "shown-networks" does not list. Add "btctest4" to "shown-networks", or set "default-network" to btc or bch
```

A file without the key `shown-networks` shows `btc` and `bch`, as a new one
does. `hw` warns about a key it does not know.

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

- The networks `hw` shows after `--network`. `setdefault --show-network`
  offers the hidden ones, and `setdefault --hide-network` the shown ones.
- The indexes of your wallets after `--wallet`, for `switch`, `exportwallet`
  and `deletewallet`.
- The indexes of your portfolios after `switch`, and after `--portfolio` for
  `switch`, `deleteportfolio` and `exportwallet`. They are those of the wallet
  that `--wallet` names, or else of the wallet of the active portfolio.
  The `--wallet` and `--portfolio` of `importwallet` and of `show` name a
  wallet and a portfolio of a file, which the database does not know, so they
  are offered no value.
- The hashes of your pending transactions. `signtx` offers those to sign,
  `broadcast` those signed and not sent yet, and `exporttx` and `deletetx`
  all of them.
- The transactions `bumpfee` can replace: your own payments on Bitcoin that
  are not in a block yet.
- Your coins. `lockcoin` offers the free ones, `unlockcoin` those you locked,
  and `--coin` those of the active portfolio that you can spend now. After a
  short id and a colon, such as `a1075db5:`, Tab completes the index.
- Your addresses. `label` offers the receiving addresses, `show` and `signmsg`
  all of them, `--spend-from` those of the active portfolio that hold coins
  you can spend now, and `--change-back` every address of the active
  portfolio, and the word `largest`.
- Your transactions. `label` and `show` offer them by their txid, and the
  pending ones by the hash `pendingtxs` shows. `show` offers your coins too.

`show` also offers file names, as for any file. It leaves out a file whose name
looks like a hash, an address or a path, since `show` would read it as one.
Type `./` first, and Tab offers it.

Most values come from every portfolio, since the commands that take them find
their portfolio by themselves. Coin control is the exception. It refuses the
coins and addresses of another portfolio than the active one, so `--coin`,
`--spend-from` and `--change-back` offer those of the active portfolio alone.

The options typed before the word narrow what is offered. `--network` and
`--type` narrow it down to the accounts they pick. The defaults of the
configuration are not applied here. Without an option, every shown network
counts, so that Tab offers a value you may still narrow down with an option
typed after it. Nothing is offered from a
[hidden network](#shown-and-hidden-networks).

Hashes and coins are offered by their [short ids](#short-ids), such as
`a1075db5` and `a1075db5:1`, which every command takes. Eight digits are easier
to tell apart in a list than 64. Type a ninth digit, and Tab completes the whole
hash instead. Two hashes that share a short id are offered in full, since the
command would refuse their short id. The same goes for a short id that also
names something Tab does not offer, such as a coin of another portfolio after
`--coin`.

`hw` offers at most 20 values for a word. When more of them start with what
you typed, it offers none: type more of the value, or copy it from a listing.
Only the values that match are read, so a large wallet completes as fast as a
small one.

To complete what your wallet holds, `hw` reads your database. It changes
nothing in it, and it does not wait for another `hw` command, such as a long
`sync`.

### Colons in bash

bash splits a word at a colon. `hw` joins the word again, so a colon you type
completes as in zsh and fish. `a1075db5:` and Tab offers the indexes of the
coins of that transaction, and `bitcoincash:q` and Tab completes the address.
bash keeps what you typed up to the colon, and adds the rest.

A Bitcoin Cash address typed without its prefix, such as `bitcoincash:`, is
offered without it, and `hw` reads it either way. A coin of a transaction that
paid you several coins completes up to its colon, which bash writes `\:`. Press
Tab again to list the coins, or type the index. The backslash goes when the
command runs, as it does in a name with a space, `my\ savings`.

## Upgrading from an earlier version

Use `hw` 0.10. Never use version 0.8 or earlier.

[CHANGELOG.md](CHANGELOG.md) lists what changed in each version, and the
commands and options that were renamed or removed.

`hw` 0.10 does not read the database of an earlier version. You create the
wallet again from its mnemonic, once. Your funds are not at risk. They are on
the chain, and the mnemonic finds them again.

### Why the database is not upgraded

`hw` 0.9 kept single accounts, which you created one by one. `hw` 0.10 keeps
[portfolios](#where-a-command-works) of twelve accounts each. The accounts that
0.9 did not have are behind hardened steps of their paths, so only the
mnemonic can derive them. `hw` 0.9 also kept no fingerprint of the master key,
and the files of 0.10 name every key by it. An upgrade without the mnemonic
would have left half-made wallets, to be completed from the mnemonic later.
Creating the wallet again from the mnemonic is the same work, and leaves
nothing half made.

### What `hw` 0.10 says

Every command refuses a database of `hw` 0.8 or 0.9. It writes nothing to the
file, and makes no copy of it. The message names the file, and the commands to
run next:

```console
$ hw portfolios
The database /home/you/.hw/accounts.sqlite was made by an earlier version of hw, at format version 2.
hw 0.10.0 does not read it, and has left it as it is. To go on, move it aside
and create the wallet again from its mnemonic:

  mv /home/you/.hw/accounts.sqlite /home/you/.hw/accounts-old.sqlite
  hw recover

recover finds the portfolios of the mnemonic that were paid. On an offline
computer, run hw newportfolio --existing instead, once for each account index
the old wallet used, from 0 up to the highest. Then carry the portfolios to the
online computer with exportwallet and importwallet. The labels of the old
database are not carried over.
```

A database of `hw` 0.8 is at format version 1, and one of 0.9 at version 2.
`hw` 0.10 reads version 5, which `hw --version` shows. A database of a build of
0.10 made before its release, at version 3 or 4, is refused the same way.

### Create the wallet again

**On an online computer.** Move the old database aside, as the message says.
Then run [`recover`](#hw-recover), which asks for the mnemonic:

```console
mv ~/.hw/accounts.sqlite ~/.hw/accounts-old.sqlite
hw recover
```

`recover` finds the portfolios of the mnemonic that were paid, on every
network, and creates each one whole. It then syncs them. Run it once for each
mnemonic the old database held. Each mnemonic becomes a wallet of its own.

**With an offline computer.** `recover` needs the server, so it takes the
mnemonic on the online computer. To keep the mnemonic offline, create the
portfolios on the offline computer, and carry them over in a file:

1. On the offline computer, move the old database aside. Create the portfolios
   from the mnemonic, and write them to a file:

   ```console
   mv ~/.hw/accounts.sqlite ~/.hw/accounts-old.sqlite
   hw newportfolio --existing                  # portfolio0
   hw newportfolio --existing                  # portfolio1, for a second account
   hw exportwallet wallet.jsonl
   ```

2. On the online computer, move the old database aside. Read the file, then
   sync:

   ```console
   mv ~/.hw/accounts.sqlite ~/.hw/accounts-old.sqlite
   hw importwallet wallet.jsonl
   hw sync
   ```

Each `newportfolio --existing` adds the portfolio at the lowest free index,
`portfolio0` first. Run it once for each account index the old wallet used,
and for the free indexes between them. The index is the third step of the path
of an account. An account at `/44'/0'/2'` is at index 2, so it needs three
runs, for `portfolio0`, `portfolio1` and `portfolio2`. The first `sync` then
finds the addresses each portfolio used.

> **Good to know**
>
> - **The old file is left as it is.** `hw` 0.9 can still read it. Keep it
>   until the new wallet shows all of your funds.
> - **The labels of the old database are not carried over.** Nor are the names
>   of its accounts and its pending transactions. `hw` 0.9 still shows them, as
>   [Reading the old database](#reading-the-old-database) tells.
> - **A backup file of `hw` 0.9 is not read either.** The `backup` command of
>   0.9 wrote a file that only its `restore` reads.
> - **A passphrase with accents needs care on an offline computer.** See
>   [A passphrase with accents](#a-passphrase-with-accents).
> - **A mnemonic split into parts cannot be typed into `hw` 0.10.** See
>   [A mnemonic split into parts](#a-mnemonic-split-into-parts).
> - **The configuration file of 0.9 still reads.** `hw` 0.10 names the keys it
>   no longer has, such as `recovery-gap`, and leaves them aside.
> - **The test networks are hidden.** `hw` 0.10 shows `btc` and `bch`. An
>   account of 0.9 on a test network comes back in its portfolio, and `sync`
>   updates it, but no answer shows it until you
>   [show its network](#shown-and-hidden-networks):
>   `hw setdefault --show-network btctest`.
> - **A configuration file of a build of 0.10 made before its release still
>   reads too.** It may hold the keys `default-wallet` and
>   `default-portfolio`, which `hw` 0.10 names and leaves aside. The
>   [active portfolio](#the-active-portfolio) takes their place. Run
>   [`switch`](#hw-switch) to choose it.

### Accounts become portfolios

`hw` 0.9 kept single accounts, which you created and named one by one. `hw`
0.10 keeps [portfolios](#where-a-command-works). A portfolio holds every
account of a mnemonic at one account index, on every network and of both types
of address. So an account of 0.9 comes back in the portfolio of its index.
Take three accounts of one mnemonic:

| 0.9 account | Network | Path | It comes back in |
|---|---|---|---|
| `main` | `btc` | `/44'/0'/0'` | `portfolio0` |
| `savings` | `btc` | `/44'/0'/1'` | `portfolio1` |
| `cash` | `bch` | `/44'/145'/0'` | `portfolio0` |

`main` and `cash` share `portfolio0`, which shows a line for each network:

```console
$ hw portfolios
* portfolio0  wallet0
    created 11 Nov 2026
    btc  spendable 0.01200000 btc, 2 coins, 14 addresses
    bch  spendable 0.00000000 btc, no coins, 3 addresses

  portfolio1  wallet0
    created 11 Nov 2026
    btc  spendable 0.25000000 btc, 1 coin, 2 addresses
```

The names of the accounts stay in the old database. A portfolio has a
description in their place, which [`label`](#hw-label) sets on the active
portfolio, given the description alone:

```console
hw label main
hw switch 1
hw label savings
hw switch 0                                 # back to portfolio0
```

**What you type now.** The commands that worked on accounts have new names,
and `--account` is gone. You no longer name the account a command works on.
The commands work in the [active portfolio](#the-active-portfolio), which
[`switch`](#hw-switch) changes:

| In 0.9 | In 0.10 |
|---|---|
| `mnemonic`, then `createacc` | `newportfolio` |
| `createacc`, for one more account of a mnemonic | `newportfolio --existing` |
| `accounts` | `portfolios` |
| `renameacc` | `label DESCRIPTION` |
| `deleteacc` | `deleteportfolio`, or `deletewallet` for a whole mnemonic |
| `exportacc`, `importacc` | `exportwallet`, `importwallet` |
| `--account NAME` | `hw switch N` before the commands, with `--wallet N` for another mnemonic, then `--network` and `--type` on each command |

### Reading the old database

The old file keeps the labels, the names of the accounts and the pending
transactions of `hw` 0.9. `hw` 0.10 left it as it is, so `hw` 0.9 still reads
it. Run `hw` 0.9 with a `HOME` of its own, on a copy of the file:

```console
mkdir -p ~/hw-0.9/.hw
cp ~/.hw/accounts-old.sqlite ~/hw-0.9/.hw/accounts.sqlite
HOME=~/hw-0.9 hw-0.9 accounts               # hw-0.9 is the binary of hw 0.9
HOME=~/hw-0.9 hw-0.9 addrs --account main   # the labels of the addresses of main
```

Set again the labels you want to keep with [`label`](#hw-label), which takes
the address, or its path, such as `bip44/0/3`. A pending transaction cannot be carried over. Prepare it again
with `hw` 0.10. One that `hw` 0.9 signed already can still be sent with its
`sendtx`.

Keep `hw` 0.9 to that copy. It does not know the database of `hw` 0.10, and
must not be run on it.

The commands of `hw` 0.9 that ask the server, such as `syncacc` and `sendtx`,
need a haskoin-store older than version 1.10. `hw` 0.9 reads the binary answers
of the server, and a haskoin-store of version 1.10 or later answers in JSON
alone. Reading the old file, as above, asks no server.

### Accounts derived wrongly

Earlier versions have known bugs in the accounts they derive. Your funds are
not lost, but take care when you move them:

- Versions before 0.8.0 derive the wrong account when the wallet holds more
  than one mnemonic. The first account of a second mnemonic comes out as
  `/44'/0'/1'` instead of `/44'/0'/0'`. Write down the derivation path of each
  account you use, so that you can find it again.
- Version 0.7.0 derives the wrong account from a split mnemonic. Use the
  0.7.0 binary to move the funds to an address of `hw` 0.10.

### A mnemonic split into parts

Versions up to 0.9 could split a mnemonic with `mnemonic --split` into parts
that were all needed to rebuild it. `hw` 0.10 neither makes nor reads such
parts, so it cannot create the wallet of such a mnemonic again. Use `hw` 0.9 to
move the funds of such a mnemonic to an address of `hw` 0.10.

Run `hw` 0.9 on a copy of the [old database](#reading-the-old-database), which
holds the account. Or run it with an empty `HOME` of its own, such as
`HOME=$(mktemp -d)`, where `createacc --split N` asks for each of the N parts.
Either way, sync the account with `syncacc`. Then pay its funds to an address
of `hw` 0.10 with `preparetx`, `signtx --split N` and `sendtx`.

[Backups in shares](#hw-newportfolio) take the place of parts. A chosen number of
shares is enough, and a wrong share is told.

### A passphrase with accents

A passphrase with accents or other non-ASCII letters is normalized as BIP39
asks since `hw` 0.10. Earlier versions did not normalize it, so their accounts
come from another seed than other wallets find.

[`recover`](#hw-recover) finds those accounts all the same. When the passphrase
normalized finds nothing, it tries the passphrase as typed, and says so.
`signtx` and `signmsg` then sign for them, and warn. Move their funds to a
wallet created with 0.10.

`newportfolio --existing` asks no server, so it cannot tell which of the two
seeds holds funds. In a new database, it creates the wallet of the passphrase
normalized, which holds none of the funds of an earlier version, and warns so.
On an offline computer, move those funds with `hw` 0.9 first, from a copy of
the [old database](#reading-the-old-database), to an address of a wallet
created with 0.10. Or run `recover` on the online computer, which takes the
mnemonic there.

### A wallet readable by others

`hw` 0.9 and earlier let the system give `~/.hw` and the database the
permissions of your umask, which often lets the other users of the computer
read them. `hw` 0.10 makes its new database private. It leaves a directory
`~/.hw` made by an earlier version as it is, and the old database in it too.
Make them private:

```console
chmod 700 ~/.hw
chmod 600 ~/.hw/accounts-old.sqlite
```

### Transaction files of an earlier version

`hw` 0.10 reads no JSON transaction file of an earlier `hw`. Prepare the
transaction again.

[BIP32]: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
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
[podman]: https://podman.io
[Stack]: https://docs.haskellstack.org
