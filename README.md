# Haskoin Wallet

[![Hackage](https://img.shields.io/hackage/v/haskoin-wallet.svg)](https://hackage.haskell.org/package/haskoin-wallet)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jprupp/haskoin-wallet/blob/master/LICENSE)

Haskoin Wallet (`hw`) is a command-line wallet for Bitcoin and Bitcoin Cash,
made for cold storage. The mnemonic can stay on an offline computer, while an
online computer watches the funds and prepares the payments. The online
computer prepares a payment without any key. The offline computer signs it,
and the online computer sends it. The two computers exchange files: the
public keys of the wallet, and the transactions to sign.

`hw` keeps its wallets in a SQLite database, and asks a [haskoin-store] server
about the chain. The database holds public keys only. `hw` never writes your
mnemonic to disk. `hw` suits small wallets managed by hand.

`hw` keeps a wallet for each mnemonic, and portfolios in each wallet. A
portfolio holds all the accounts of the mnemonic that have the same account
index, on every network and for both types of address, legacy and native
SegWit. The commands work in one portfolio, the active portfolio. With one
mnemonic, that is `portfolio0`, and you never create or pick an account. You
only say which network and which type of address you want.
[Where a command works](#where-a-command-works) explains this model.

What `hw` does:

- [Mnemonics](#hw-newportfolio) as [BIP39] describes, or backups in shares as
  [SLIP39] describes.
- [Portfolios](#where-a-command-works) that hold the accounts of a mnemonic.
  The accounts with legacy addresses follow [BIP44]. The accounts with native
  SegWit addresses, which pay lower fees, follow [BIP84]. A payment can spend
  the coins of both together.
- [QR codes of addresses](#hw-receive) on the terminal, for a payer to scan.
- [Signing on an offline computer](#signing-on-an-offline-computer), or with a
  hardware wallet, through [transaction files](#transaction-files).
- [Payments in one command](#hw-send), for a wallet whose mnemonic you type
  on the online computer.
- [Coin control](#hw-preparetx), and [fee rates](#hw-fees) read from recent
  blocks.
- [Fee bumping](#hw-bumpfee) on Bitcoin ([BIP125]).
- [Sweeps](#sweeps) of coins whose private keys you hold, in
  [one command](#hw-sweep) or with the keys kept offline.
- [Message signing](#messages) in the Bitcoin Signed Message format.
- [Tags](#tags-on-addresses-and-transactions) that group addresses and
  transactions. A listing can show only the rows that have a tag, and a
  payment can spend only the coins at the addresses that have a tag.
  `send --tag rent` tags a payment as you make it, and `txs --untagged` lists
  the transactions that have no tag yet. A statement, a journal or a
  spreadsheet can cover one tag only, to give to an accountant for example.
- [An address book](#the-address-book) of the addresses of other people, each
  with a label. The view of a transaction underlines these addresses. The Tab
  key completes them where you type the recipient of a payment.
- [Labels](#hw-exportwallet) in the format of [BIP 329], which other wallets
  read and write, and an [output descriptor](#hw-portfolios) of each account,
  to watch it in another wallet.
- [JSON answers](#options-of-every-command) from every command, for scripts.
- [CSV files](#listings) of every listing, and a
  [spreadsheet](#hw-exportsheet) of the whole wallet.
- [A journal](#hw-exportjournal) of the transactions for [ledger], the
  command-line tool for double-entry accounting. `hw` keeps it up to date
  when you ask it to. A tag can stand for an account of the journal, so that
  a payment tagged `rent` is recorded as rent.
- [PDF reports](#hw-report) to print or to hand to a tax office. A balance
  statement lists every portfolio and address, for today or for a day in the
  past, such as the last day of a year. An account statement lists the
  transactions of a month or of any period, with the balance after each. A
  payment confirmation states what one transaction paid, for the person you
  paid or for your books.

## Contents

- [Install](#install)
- [Quick start](#quick-start)
  - [Set up `hw`](#set-up-hw)
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
  - [Tags on addresses and transactions](#tags-on-addresses-and-transactions)
  - [The address book](#the-address-book)
  - [Files](#files)
  - [Automatic backups](#automatic-backups)
  - [A damaged database](#a-damaged-database)
  - [A journal kept up to date](#a-journal-kept-up-to-date)
  - [One command at a time](#one-command-at-a-time)
  - [Scripts](#scripts)
- [Commands](#commands), each with its examples and what is good to know
  - [Wallets and portfolios](#wallets-and-portfolios)
  - [Addresses](#addresses)
  - [Coins](#coins)
  - [Payments](#payments)
  - [Transactions](#transactions)
  - [Tags](#tags)
  - [Address book](#address-book)
  - [Sweeps](#sweeps)
  - [Messages](#messages)
  - [The wallet](#the-wallet)
- [Transaction files](#transaction-files)
- [Configuration](#configuration)
  - [The setup](#the-setup)
  - [The configuration file](#the-configuration-file)
  - [Keep `~/.hw` on another disk](#keep-hw-on-another-disk)
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
`PATH`, then check the install with `hw --help`. The first time, run `hw`
alone to [set it up](#set-up-hw).

`stack test` runs most tests against a fake haskoin-store server that the
tests provide, and needs nothing else. Some tests run `hw` against real nodes
and real haskoin-store servers, on regtest chains of Bitcoin and Bitcoin Cash
in a container. They cover what a fake server cannot show: reorganizations,
replaced payments, and what the server answers about a transaction that every
node refuses. These tests need an image, which `regtest/build.sh services`
builds with [podman]. Until you build it, `stack test` skips them and reports
them as pending, with the reason. See [regtest/README.md](regtest/README.md).

The Tab key can complete what you type after `hw`, once you set up
[shell completion](#shell-completion).

## Quick start

Read [Keep your funds safe](#keep-your-funds-safe) before you put real funds in
the wallet.

The examples in this README write `ADDRESS`, `NOSIGHASH`, `TXID`, `HASH` and
`TXHASH` where you put your own values.

### Set up `hw`

The first time, run `hw` alone, without a command. It asks four questions,
and explains each one before it asks:

```console
$ hw
Welcome to hw.
hw has no configuration file yet. Four questions set it up, and you can change
every answer later.

1. Which networks do you work with?

   A wallet of hw has accounts on eight networks: Bitcoin, Bitcoin Cash, and
   three test networks of each. hw shows the networks you check here, and hides
   the others. A hidden network is left out of every answer, and no command can
   name it. It is not lost. hw keeps it up to date, and you can show it later.

  > [x] btc       Bitcoin
    [x] bch       Bitcoin Cash
    [ ] btctest   Bitcoin, test network 3 (deprecated)
    [ ] btctest4  Bitcoin, test network 4
    [ ] btcreg    Bitcoin, a test chain of your own
    [ ] bchtest3  Bitcoin Cash, test network 3
    [ ] bchtest4  Bitcoin Cash, test network 4
    [ ] bchreg    Bitcoin Cash, a test chain of your own

   Up and Down move. Space checks or unchecks. Enter confirms.
```

The arrow keys move the `>`. Space checks or unchecks the network under it,
and Enter confirms. The next three questions are lists: move to a row and
press Enter.

| Question | What it sets | If you only press Enter |
|---|---|---|
| Which networks do you work with? | The [networks `hw` shows](#shown-and-hidden-networks) | `btc` and `bch` |
| Which network is the default? | The network that a command uses when you give no `--network` | `btc` |
| Which type of address is the default? | The type of address that a command uses when you give no `--type` | `bip44` |
| Should `hw` keep a journal for ledger, the accounting program? | Whether `hw` keeps a [journal of your transactions](#a-journal-kept-up-to-date) up to date | `no` |

The last question is for people who keep accounts. [ledger] is a free
command-line accounting program. It keeps double-entry books in a text file,
the journal. Its reports say what each account holds, what you earned and
what you spent. If you answer `yes`, `hw` writes your transactions to such a
journal and keeps it up to date. Answer `no` if you are not sure.
[`setdefault --journal`](#hw-setdefault) turns the journal on later.

`hw` then writes your answers to its configuration file, and says so. Here
the answer to the third question was `bip84`:

```console
hw is set up. Your settings were written to /home/you/.hw/config.toml
shown-networks : btc, bch
default-network: btc
default-type   : bip84
journal        : false

To change them later, run hw setdefault, or edit that file. It lists every
setting of hw, with its default.

Next, create a wallet, with a new mnemonic to write down:
  hw newportfolio
Or create the wallet of a mnemonic you have already:
  hw newportfolio --existing
To see what else hw does:
  hw --help
```

Nothing here is final. [`setdefault`](#hw-setdefault) changes each answer
later, and [The setup](#the-setup) says more about the questions.

The last lines say what to run next. `hw` has no wallet yet, and
[`newportfolio`](#hw-newportfolio) creates one.
[A first payment](#a-first-payment) starts there.

Until `hw` is set up, it refuses every command:

```console
$ hw portfolios
hw is not set up yet: there is no configuration file at /home/you/.hw/config.toml. Run hw without a command to set it up.
```

### A first payment

```console
hw newportfolio                             # write the words down
hw receive --label "first deposit"          # give this address to the payer
hw sync                                     # once the payment is sent
hw preparetx ADDRESS:0.0005 --fee-rate median
hw signtx NOSIGHASH                         # type the mnemonic
hw broadcast TXID                           # the txid signtx shows
```

`newportfolio` first asks for a passphrase for the new wallet. Press Enter for
a wallet without one. [`newportfolio`](#hw-newportfolio) says what a
passphrase changes. It then makes a mnemonic, shows it once, and creates
`wallet0` with its `portfolio0`. This is the first portfolio of the database, so it becomes the
active portfolio, and every command above works in it. The portfolio has an
account for every network and for both types of address from the start, so
there is no account to create. If you only pressed Enter at each question of
the setup, `receive` gives a legacy address on Bitcoin. Two options ask for
another kind of address. `setdefault` changes the kind of address that
`receive` gives without them:

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
commands take either hash, in full or by its [short id](#short-ids): its first
8 digits. The listings show the short id, and so do the next steps that a
command suggests.

[`send`](#hw-send) does the three steps of the payment in one command. It
shows the transaction, asks for the mnemonic, and sends the payment:

```console
hw send ADDRESS:0.0005 --fee-rate median    # check the payment, type the mnemonic
```

It makes the same payment as the three commands. The command that sends a
payment, `broadcast` or `send`, syncs the wallet afterwards: the payment is in
the history at once, and its change is there for the next payment. It asks for the mnemonic on
the online computer, so it is not for funds whose mnemonic you keep offline.
[`sendall`](#hw-sendall) sends every coin of the portfolio in one command, the
same way.

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

An online computer that was tampered with can show an address of the attacker
in place of an address of the wallet. The offline computer holds the same
portfolio and derives the same addresses, without the mnemonic. The offline
computer did not give the address out itself, so it shows the address cut:
its two ends, with a dot for each character of its middle. Compare the two
ends with the address that the online computer shows.

```console
hw receive                                  # on the online computer: bip44/0/7 and the address
hw show bip44/0/7                           # on the offline computer: the same ends?
```

Give `show` the path as `receive` printed it, with its type: `bip44/0/7`, or
`bip84/0/7` for an address of `receive --type bip84`. The type names the
account, since each account of a network has an address at `/0/7`. If you
gave `receive` a `--network`, give `show` the same one. Both computers must
also have the same portfolio active, since each command works in the active
portfolio. On each computer, `portfolios` marks it with a `*`.

Give the path, and not the address. A computer holds the addresses that it
generated itself, and the offline computer generated none. So `show ADDRESS`
refuses the address there, although it is an address of the wallet, and says
to give its path.

The ends are enough, since nobody can make another address with the same ends.
`hw` hides the middle so that nobody pays an address that the wallet never
gave out. See [`show`](#hw-show).

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
- **A secret pasted with a line too many does not reach the shell.** A
  terminal gives the shell what a program did not read. The shell shows it,
  runs it as a command and keeps it in its history. A line pasted after a
  mnemonic would end there, and so would the last lines of a list of private
  keys. So a command that asked for a secret discards what is left on the
  terminal when it ends. A command that asks for no secret discards nothing:
  what you type while it works is for the shell.
- **`send` and `sendall` ask for the mnemonic on the online computer.** They
  need the server, so they cannot run offline. A computer that was tampered
  with can steal a mnemonic typed on it. For funds in cold storage, pay in
  three steps and
  [sign on the offline computer](#signing-on-an-offline-computer).
- **`sweep` reads the private keys on the online computer.** It needs the
  server too. For keys that must stay offline, sweep in three steps:
  [`preparesweep`](#hw-preparesweep) with the addresses of the keys,
  [`signsweep`](#hw-signsweep) on the offline computer, and `broadcast`.
- **The mnemonic does not bring back your labels.** It brings back the funds,
  but not the descriptions and labels you typed. `hw` copies them, and the
  database, to `~/.hw/auto-backups` after each change
  ([Automatic backups](#automatic-backups)). [`backup`](#hw-backup) writes
  both to `~/.hw/user-backups`. Keep a copy of those directories on another
  disk too.
- **The database and the wallet files are private.** They hold no private key.
  But the public keys in them reveal every address and balance of your
  portfolios. `hw` makes `~/.hw` and the database readable by you alone. A
  directory `~/.hw` made by `hw` 0.9 or earlier keeps the permissions it had,
  and so does the old database in it.
  [Make them private](#a-wallet-readable-by-others).
- **An online computer can show a wrong address.** A computer that was
  tampered with can show an address of the attacker, and the payer then pays
  the attacker. [Check an address](#check-an-address-before-you-give-it-out)
  with `show` on the offline computer before you give it out.
- **`hw` trusts what the server says about the chain.** It checks that the
  server answers about the addresses and transactions it asked about and no
  others, and that the counts agree. But it cannot detect a payment that the
  server hides. `hw` keeps the transaction that each coin comes from, and
  checks the coin against it, so a server cannot change what a coin is worth.
  `hw` reads no answer of more than 64 MiB. A server that lies can report
  used addresses without end. So after a thousand used addresses in a row,
  `sync` and `recover` ask whether to keep looking. `recover` asks the same
  after every 20 portfolios that the server reports as paid. `sync` also
  refuses a server whose chain is not the one of the last sync: one that is
  far behind the wallet, or that `host` names by mistake. That check catches a
  server that is wrong, and a server that lies can pass it. To trust no one
  else, run your own haskoin-store.

## Using `hw`

### Getting help

`hw --help` lists the commands, and `hw COMMAND --help` explains one.
`hw --version` shows the version of `hw` and of its database format.

The commands in magenta in the list ask for a mnemonic or private keys. Run
them on an offline computer if you can. When the help is not shown on a
terminal, or with `NO_COLOR` set, they are marked `!` instead.

The help of `hw` starts with its logo. It is the name `hw` as a computer stores
it: the byte of `h` is `0x68` and the byte of `w` is `0x77`. Each letter is a
row of eight squares, a square for each bit. On a terminal the squares are
cyan, strong for a set bit and faint for a clear one. When the help is not
shown on a terminal, or with `NO_COLOR` set, a clear bit is a low line instead:

```console
$ NO_COLOR=1 hw --help

▁ ▄ ▄ ▁ ▄ ▁ ▁ ▁
▁ ▄ ▄ ▄ ▁ ▄ ▄ ▄  hw

hw is a BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash. It stores public
keys only. The commands marked ! ask for a mnemonic or private keys. Run them on
an offline computer if you can.
...
```

The help of a command has no logo. It starts with what the command does.

On a terminal, a help goes through `less`, as a [listing](#listings) does. A
long help, such as that of `preparetx`, then starts at its first line. The
arrow keys scroll it, `/` searches it and `q` quits it. A help that fits on
the screen is shown as it is. The same applies to the help that `hw` shows
when you type a command without its arguments, or `hw` alone once it is
[set up](#set-up-hw). `--no-pager` prints a help as it is. The environment
variable `HW_PAGER` or `PAGER` names another pager, for a help and for a
listing. The lines of a help wrap at the edge of the screen. A help that is
piped or written to a file goes through no pager.

[`recover`](#hw-recover), [`send`](#hw-send), [`sendall`](#hw-sendall),
[`sweep`](#hw-sweep) and [`preparesweep --key-file`](#hw-preparesweep) take a
mnemonic or private keys too. They need the server, so they run on an online
computer and are not marked. Their sections say how to keep the secret offline.

### Options of every command

These options work with every command, before or after it:

| Option | What it does |
|---|---|
| `--unit UNIT` | Sets the unit of the amounts shown, and of the amounts typed without a unit: `btc`, `bit` or `sat`. A bit is 100 satoshis. Without it, the unit is the one that the key `unit` of the [configuration](#configuration) names, or `btc`. [`setdefault --unit`](#hw-setdefault) sets that key. In [`hw showdefault --unit`](#hw-showdefault), the option takes no value and shows that key. |
| `--json` | Prints the answer as JSON. Questions and warnings go to stderr. |
| `--no-pager` | Prints a listing or a help as it is, through no pager. |

### Where a command works

`hw` never asks you to create an account or to name one. It keeps a wallet for
each mnemonic, and portfolios in each wallet. A portfolio holds all the
accounts of the wallet that have the same account index, on every network and
for both types of address. The commands work in one portfolio, the active
portfolio. The database records which one it is. With one mnemonic, the
active portfolio is `portfolio0` of `wallet0`, and you never name it. You only
say the network and the type of address you want, and each has a default.

| Term | What it is |
|---|---|
| Wallet | The master key of one mnemonic. A mnemonic with a passphrase gives another master key, so it is another wallet. |
| Portfolio | All the accounts of a wallet that have the same account index. `portfolio0` holds the accounts at index 0. |
| Active portfolio | The portfolio the commands work in. The first portfolio created becomes active, and only [`switch`](#hw-switch) changes it. |
| Account | One derivation path, `m/purpose'/coin type'/index'`, on one network. |
| Type | The type of address of an account: `bip44` or `bip84`. |

**Why portfolios.** A mnemonic gives one account for each purpose, each coin
and each index. A wallet built on single accounts makes you create each one
and name it. You must then tell every command which account you mean. Most
people only ever use the first index. So `hw` groups the accounts by their
index, and creates all the accounts of an index at once. A new portfolio is
ready for Bitcoin and Bitcoin Cash, with legacy and native SegWit addresses,
and there is nothing to set up. A second portfolio is for funds you keep apart,
such as savings. A payment spends the coins of one portfolio only, so it never
mixes the two.

The design gives up two things. An account has no name of its own, since the
network and the type already tell it apart in its portfolio. You also cannot
import a single key, such as an extended public key from another wallet. Such
a key is one account, and it does not say which portfolio it belongs to.

#### What a portfolio holds

`hw` creates a portfolio with all its accounts at once. A portfolio has twelve
accounts, which come from five keys. In these paths, `i` is the index of the
portfolio:

| Path | Type | Networks |
|---|---|---|
| `m/44'/0'/i'` | `bip44` | `btc` |
| `m/84'/0'/i'` | `bip84` | `btc` |
| `m/44'/145'/i'` | `bip44` | `bch` |
| `m/44'/1'/i'` | `bip44` | Every test network: `btctest`, `btctest4`, `btcreg`, `bchtest3`, `bchtest4` and `bchreg` |
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
You cannot rename them. The name of a portfolio comes from its account index,
so it is the same on every computer that holds the wallet. With a name of
your choice, you would have to keep the name the same on the offline and the
online computer. A description says what a wallet or a portfolio is for, in
your own words. [`label`](#hw-label) sets it when you give it the description
and nothing else, and `portfolios` shows it.

A new wallet takes the lowest free index. When you delete a wallet, the other
wallets keep their names. After you delete `wallet0`, `wallet1` is still
`wallet1`, and the next new wallet becomes `wallet0`. So a name does not
identify a wallet for ever. The fingerprint of its master key does, and
`portfolios --verbose` shows it.

#### The active portfolio

The database keeps one portfolio as the active one. Most commands work in it.
`receive` gives out its addresses, `txs` lists its transactions, and
`send` and `preparetx` pay from it. None of them takes an option to name
another portfolio.

The first portfolio created in a database becomes the active one, whether
`newportfolio`, `recover` or `importwallet` creates it. A command that creates
several makes the first one active: `portfolio0` of a new wallet, or else the
lowest index it creates. A portfolio that you create later does not become
active. The answer of the command then says which portfolio is still active,
and gives the `switch` command that makes the new one active. After the first
portfolio, only [`switch`](#hw-switch) changes the active portfolio:

```console
hw switch 1                                 # portfolio1 of the same wallet
hw switch --wallet 1                        # portfolio0 of wallet1, another mnemonic
```

The portfolio stays active for every command until the next `switch`.
[`portfolios`](#hw-portfolios) marks it with a `*`:

```console
$ hw portfolios
* portfolio0  wallet0  "daily spending"
    created 11 Nov 2026
    btc  total 0.00000000 btc, spendable 0.00000000 btc, no coins, 1 address

  portfolio1  wallet0  "savings"
    created 11 Nov 2026
    btc  total 0.00000000 btc, spendable 0.00000000 btc, no coins, 1 address
```

[`showdefault --portfolio`](#hw-showdefault) names it alone:

```console
$ hw showdefault --portfolio
The active portfolio is portfolio0 of wallet0.
```

**Why one active portfolio.** You always know where `hw` works, and you never
type it. An option on every command would make you name the portfolio each
time. A slip of the finger would then give out an address of the wrong
portfolio, or pay from it. A default in the configuration file would be hard
to keep right. That file is separate from the database, so the default could
name a portfolio that was deleted, or a portfolio of another database. `hw`
keeps the active portfolio in the database itself, beside the portfolios. So
it always names a portfolio that exists. `switch` refuses a portfolio that the
database does not have. `deleteportfolio` refuses the active portfolio.
`deletewallet` refuses the wallet of the active portfolio while the database
has another wallet to switch to.

The cost is that the command line does not show which portfolio is active.
`portfolios` and `showdefault` show it, and so do the answers of the commands
that create a portfolio. A script that needs a given portfolio runs `switch`
before its commands, and that portfolio stays active after the script ends.

`switch` takes a wallet and a portfolio by their index, `0` for `wallet0`, or
by their name, as in `hw switch portfolio1`. An index must be below
2147483648, which is 2^31. The index of a portfolio is the last step of the
paths of its accounts, and [BIP32] allows that many hardened steps. `hw`
refuses a larger index.

#### The two options

Two options say which account of the active portfolio a command works on:
`--network` and `--type`. Each has a default, and a key of the
[configuration](#configuration) changes that default.

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

[`showdefault`](#hw-showdefault) shows them, as in
`hw showdefault --network --type`.

The table gives the defaults that apply when the configuration file does not
set these keys. The [setup](#set-up-hw) asks you for both, and writes your
answers to the file. The default type is `bip44` because it is the one type
that every network has.

The defaults of the network and the type are in the configuration file, while
the active portfolio is in the database. The network and the type are a
preference, the same for every portfolio, and they never name something that
a command can delete. The active portfolio names a part of the wallet, so the
database keeps it.

**Bitcoin Cash has `bip44` only.** The networks of Bitcoin Cash, `bch`,
`bchtest3`, `bchtest4` and `bchreg`, have no SegWit. They use `bip44` whatever
`default-type` says, and they refuse `--type bip84`. So a default of `bip84`
is safe if you use both chains: `hw receive --network bch` still gives an
address of Bitcoin Cash.

#### Shown and hidden networks

A portfolio has accounts on eight networks, and most people work on two of
them. So `hw` shows some networks and hides the others. The
[setup](#set-up-hw) asks which ones. Unless you check others there, `hw`
shows `btc` and `bch`, and hides the six test networks. The answers of the
commands then have no line about a test network that you never use.

One of the test networks is deprecated: `btctest`, the testnet3 of Bitcoin.
It still works, and the setup marks it. Bitcoin Core has deprecated testnet3
for testnet4, and haskoin-core, the library that `hw` is built on, has
deprecated the network and will remove it. `hw` will remove it then. Test on
`btctest4` instead. The testnet3 of Bitcoin Cash, `bchtest3`, is not
deprecated: Bitcoin Cash Node still runs it.

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
them, and `sync` and `recover` say nothing about them when they finish, not
even that their server is down. The JSON answers, the CSV files, the
spreadsheet of `exportsheet`, the journal of `exportjournal`, the reports of
`report` and [tab completion](#shell-completion) leave them out too.

[`setdefault`](#hw-setdefault) shows and hides networks, so that you do not
have to edit the configuration file:

```console
hw setdefault --show-network btctest4       # show a hidden network
hw setdefault --hide-network bch            # hide a network
```

`hw` refuses a command that names a hidden network. The error says which
networks `hw` shows, and gives the command that shows the hidden one too:

```console
$ hw receive --network btctest4
Error: The network btctest4 is hidden. hw shows btc and bch. To show btctest4
too, run: hw setdefault --show-network btctest4
```

A command that is given an address, a coin or a transaction looks for it on
the shown networks. When the wallet has it on hidden networks only, `hw`
refuses the command the same way. Answering that the wallet does not have it
would be untrue. [`verifymsg`](#hw-verifymsg) takes anyone's address. It too
looks at the shown networks only, and refuses an address that is valid on
hidden networks only.

```console
$ hw show mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih
Error: The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and
btctest4, which are hidden. To show one of them, run for instance: hw setdefault
--show-network btctest
```

**The default network is always shown.** A command works on the default
network when you give no `--network`, so `hw` must show it. `setdefault`
refuses to hide it, and refuses a hidden network as the default. One command
can make both changes:

```console
hw setdefault --show-network btctest4 --network btctest4    # show it, and make it the default
hw setdefault --hide-network btc --network bch              # hide the default, and name another
```

**Show one test network, not all of them.** The test networks
[share their keys](#what-a-portfolio-holds), so an address of `btctest4` is
also one of `btctest`. With both shown, a command given such an address may
ask for `--network`. With `btctest4` shown alone, it never asks.

**Why the file lists the networks to show.** It could list the networks to
hide instead. A list of shown networks says what you see, in one place. With
such a list, a network that a later version of `hw` adds also stays hidden
until you ask for it.

Hiding a network deletes nothing. Its addresses, coins, transactions and
labels stay in the database, and [`exportwallet`](#hw-exportwallet) and
[`backup`](#hw-backup) still write them. A hidden network still counts for
[`deleteportfolio`](#hw-deleteportfolio), which refuses to delete a portfolio
that was used on any network.

#### What each command takes

Not every command works in the active portfolio. A command about the whole
wallet, such as `sync`, covers every portfolio. A command that is given a
transaction, a coin or an address finds the portfolio that it belongs to, so
it needs no `switch`.

| Commands | Where they work |
|---|---|
| `receive`, and `show` with a path or with no argument | On one account of the active portfolio. `--network` and `--type` pick it, each with its default. |
| `send`, `sendall`, `preparetx`, `preparesendall` | They spend the coins of the active portfolio on one network, of both types together. `--coin-type` narrows them down to one type. The coins and addresses that `--coin` and `--spend-from` name must be in the active portfolio. `--spend-from-tag` spends the coins at the addresses of the active portfolio that have the [tag](#tags-on-addresses-and-transactions). |
| `addresses`, `balances`, `coins`, `txs` | They list the whole active portfolio, in a section for each [shown network](#shown-and-hidden-networks). `--network` and `--type` show one network or one type only. `txs` takes no `--type`. `--tag` lists only the rows that have a [tag](#tags-on-addresses-and-transactions), and `--untagged` only the rows that have none. |
| `checkmnemonic`, `label` with a description alone, and `label`, `tag`, `untag` and `signmsg` with a path | On the active portfolio. `label DESCRIPTION --of-wallet` describes its wallet. |
| `portfolios`, `pendingtxs`, `sync`, `exportsheet`, `exportjournal`, `report` | They cover every portfolio of every wallet. `--network` narrows `pendingtxs` and `sync` down to one network. `--wallet` and `--portfolio` narrow `report` down to one wallet or to one portfolio, as they narrow `exportwallet`. `sync` works on the hidden networks too, and the five others leave them out. |
| `tags`, `renametag`, `deletetag`, and `tag` with a tag alone | They work on the whole database. A [tag](#tags-on-addresses-and-transactions) is the same in every portfolio of every wallet, with one colour. |
| `newcontact`, `contacts`, `deletecontact`, and `label`, `tag`, `untag` and `show` with the address of a contact | They work on the [address book](#the-address-book), which belongs to the whole database and not to a portfolio. `--network` picks the network of a contact whose address is valid on several networks. |
| `exportwallet` | It writes every portfolio of every wallet, on the hidden networks too. `--wallet` writes one wallet only. `--portfolio` writes one portfolio only, of the wallet `--wallet` names, or else of the wallet of the active portfolio. |
| `signtx`, `signsweep`, `importtx`, `bumpfee`, `sweep`, `preparesweep`, and `show` with a transaction or a transaction file | They find the portfolio from the transaction, or from the destinations of a sweep. When several portfolios match, the active portfolio decides, as the paragraph below this table explains. |
| `lockcoin`, `unlockcoin`, `signmsg` and `show` with an address or a coin, and `label`, `tag` and `untag` with an address or a transaction | They find the portfolio from the address or the coin. A transaction gets its label and its tags in every portfolio that holds it. You need `--network` only when the wallet has the address or the coin on several shown test networks. |
| `importwallet`, and `show` with a file of a wallet | They find each wallet from the file. `--wallet` names one wallet of the file, by its fingerprint, and `--portfolio` one portfolio. The command then reads only that wallet or that portfolio. |
| `show` with a database file | It lists every wallet and every portfolio of the file, whichever portfolio is active, and takes no option. |
| `switch` | Its argument names the portfolio to make active, or `--portfolio` does, and `--wallet` names its wallet. Without `--wallet`, it is the wallet of the active portfolio. Without a portfolio, it is `portfolio0` of the wallet `--wallet` names. |
| `deleteportfolio`, `deletewallet` | The active portfolio never decides what they delete. Each takes what to delete as its argument. `deleteportfolio` takes a portfolio of the wallet of the active portfolio, and refuses the active one. `deletewallet` takes a wallet, and refuses the wallet of the active portfolio while another wallet exists. |

A transaction can involve several portfolios. A transfer between two portfolios
belongs to the one that pays it, since that one locks the coins and signs. When
several portfolios pay, or when none pays and the transaction pays several,
`hw` uses the active portfolio if it is one of them. Otherwise it names them,
and stops:

```console
$ hw importtx signed.psbt
Error: The transaction spends from several portfolios, portfolio0 of wallet0 on
btc and portfolio0 of wallet1 on btc: switch to the one to use first
```

Run `switch` to make the right one active, and run the command again. The
destinations of a sweep follow the same rule. The active portfolio decides in
these cases only. A transfer stays with the portfolio that pays it, whichever
portfolio is active.

A command that finds the portfolio by itself may answer about another
portfolio than the active one. The paths and labels it shows are then those of
that portfolio. The field that names the portfolio says so, in yellow on a
terminal. For a transaction it reads
`Portfolio: portfolio0, not the active portfolio1`. For an address or a coin,
the `Account` field reads `portfolio0, not the active portfolio1  btc  bip84`.
`hw` shows a transfer as the paying portfolio sees it, even when the receiving
portfolio is active. The paying portfolio signs the transfer, so what `signtx`
shows before it asks for the mnemonic matches what the mnemonic signs.

#### Paths of another portfolio

A path alone, such as `bip84/0/3`, always means an address of the active
portfolio. `show`, `label`, `tag`, `untag`, `signmsg`, `--spend-from` and
`--change-back` read a path that way. When `hw` shows a path of another
portfolio, it names that portfolio after the path, as in
`bip84/0/3 of portfolio1`. So you never mistake it for a path that you can
type. When the database has several wallets, `hw` names the wallet too, as in
`bip84/0/3 of portfolio1 of wallet0`. See
[How the views name a portfolio](#how-the-views-name-a-portfolio).

You see such paths in two cases. The first is a payment between two
portfolios. `hw` knows the address that the payment pays in the other
portfolio, and shows it as an address of the wallet, not as a stranger's:

```console
$ hw show a1075db5
14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  Txid     : a1075db5c0ffee00000000000000000000000000000000000000000000000001
  Short id : a1075db5
  Time     : 14 Nov 2023 23:13:20 CET
  Fee      : 0.00001000 btc, 4.4 sat/vB
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0
  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0
```

The second is an answer about another portfolio than the active one. Every
path in the answer then names its portfolio, even a path of the active
portfolio. Such a view holds paths of two portfolios, so a path alone could
mean either. Here `portfolio1` is active, and the same payment is still shown
as `portfolio0` sees it:

```console
$ hw show a1075db5
14 Nov 2023 23:13  Outgoing     -0.00041000 btc  2 confirmations
  Portfolio: portfolio0, not the active portfolio1
  ...
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0
  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00040000 btc  bip44/0/0 of portfolio1
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00009000 btc  bip44/1/0 of portfolio0
```

An address of another portfolio is blue on a terminal, like every address of
the wallet. Blue means that the address is yours, which is the first thing to
check about an address you pay. The name of its field, `To` here, says that
the payment goes there. The amount is not green, since it leaves the portfolio
of the view. The label of the address, if it has one in its portfolio, is on
the line below.

The short views name such an address by its path and its portfolio, in place
of the address: `to bip44/0/0 of portfolio1`. The label of the payment comes
after it. For an address of another wallet, the short views show the label of
the payment in place of the address. For an address of another portfolio, they
keep the path, because it tells you that the payment went to you.

The answers about one address, one coin or one transaction follow this rule,
and so does `pendingtxs`, which lists every portfolio. `addresses`,
`balances`, `coins` and `txs` list the active portfolio, so their own paths
name no portfolio. In `--json`, a transaction gives these addresses under
`otherportfolios`, each with its `wallet`, `portfolio`, `type`, `path` and
`label`. They stay under `otheroutputs` and `otherinputs` as well, with their
values.

> **Good to know**
>
> - **`hw` knows an address of a portfolio once that portfolio gave it out.**
>   An address further along its chain, which no `receive` gave out and no
>   `sync` found, is shown as an address of another wallet. An offline
>   computer that never ran `receive` knows no address, so it shows every
>   address of another portfolio that way.
> - **The other portfolio must be in the same database.** To this database, a
>   portfolio kept on another computer is another wallet.
> - **The row of a transaction in a spreadsheet does not name the other
>   portfolio.** In that row, `exportsheet` and `--csv` give an address of
>   another portfolio without a path, like an address of another wallet. In
>   the file of [`exportsheet`](#hw-exportsheet), the tab that lists the
>   addresses of the transactions names the portfolio that holds the address,
>   with its path.

#### How the views name a portfolio

A listing and the details of an address, a coin or a transaction name the
portfolio they are about. With one wallet in the database, they name the
portfolio alone, as in `portfolio1`. So with one mnemonic you never see a
wallet name there. With several wallets, they name the wallet of every
portfolio,
`wallet0` too, as in `portfolio1 of wallet0`.

The rule is the same wherever a view names a portfolio. That covers the
`Portfolio` and `Account` fields, the note about the active portfolio, the
titles of the listings and the portfolio after a path. It also covers the
answer of `sync` and `recover`, and their status line.

With several wallets, a portfolio named alone could be misread. Here
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

These cases do not follow the count of the wallets in the database:

- `portfolios` always names the wallet of each portfolio. It is where you
  tell the wallets apart.
- A message that is a sentence names the portfolio and its wallet, as in
  `Created portfolio0 of wallet0`. So do the errors, and a suggested `switch`
  to another portfolio.
- [`show FILE`](#hw-show) of a file of a wallet counts the wallets of the
  file, not of the database. It names the wallet of each account when the
  file holds several.
- [`show FILE`](#hw-show) of a database file names each wallet of the file on
  a line of its own, with its portfolios under it.

The next steps a command suggests work in the active portfolio, so they name
no portfolio. A step about another portfolio starts with the `switch` to it.
This happens after a command creates a portfolio that does not become active.
It also happens after a `sync` that finds a pending transaction in conflict in
another portfolio, or new transactions in other portfolios only.

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
- **A balance is for one network.** On `btc`, it adds up the `bip44` and
  the `bip84` account. `hw` never adds the balances of two networks, since
  they are different coins.

> **Good to know**
>
> - **The listings do not use the default network or type.** Without
>   `--network` and `--type`, `addresses`, `balances`, `coins` and `txs` show
>   the whole portfolio, whatever `default-network` and `default-type` say.
>   The two keys set the account from which `receive` gives an address. They
>   also set the network a payment spends on, and the type of its change when
>   it spends coins of both types.
> - **Some commands find the portfolio themselves.** `signtx`, `importtx`,
>   `bumpfee`, `sweep` and `preparesweep` use the portfolio of the
>   transaction, whichever portfolio is active. A transaction between two portfolios
>   belongs to the one that pays. `show` shows a transaction file as that
>   portfolio sees it, and a transaction of the wallet as each portfolio that
>   holds it sees it. A view of another portfolio than the active one says
>   so.
> - **Coin control works in the active portfolio.** `--coin` and `--spend-from`
>   take the coins and addresses of the active portfolio only. `hw` refuses a
>   coin or an address of another portfolio. The error names the portfolio
>   that holds it and gives the `switch` to that portfolio. A
>   [tag](#tags-on-addresses-and-transactions) may be on addresses of several
>   portfolios. `--spend-from-tag` spends the coins at those of the active
>   portfolio, and leaves the others alone.
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
that unit. An amount typed without a unit is read in it too. A script should
give `--unit`, or write the unit after each amount, so that the configuration
cannot change what it pays. JSON answers give every amount in satoshis,
whatever the unit.
The PDF files of [`report`](#hw-report) give every amount in coins with
eight decimals, since they go to people who count in no other unit.

### Listings

Six commands list what the wallet holds: `portfolios`, `addresses`, `balances`,
`coins`, `txs` and `pendingtxs`. [`contacts`](#hw-contacts) lists the
[address book](#the-address-book) the same way, with the same views and
options. This section explains what the listings share: their views, their
sections, the pager, the paths of addresses, the tags, the order of the rows,
the pages for scripts and the CSV files. The section of each command says what
its views show.

**Views.** A listing has three views. By default, it shows each element in a
few lines. This is the compact view. `--verbose` shows every detail of each
element. `--tabular` shows each element on one row of a table.

If you always want the same view, make it the default with
[`setdefault`](#hw-setdefault), as in `hw setdefault --tabular`. It writes the
key `listing-view` of the [configuration](#configuration). `--compact` then
brings back the compact view for one listing, and `hw setdefault --compact`
brings it back for good. The key changes only what the screen shows. `--json`
and `--csv` write the same thing with or without it. A command that shows one
element only, such as `lockcoin`, keeps its own view.

**Sections.** `addresses`, `balances`, `coins` and `txs` list the whole active
portfolio, so their rows come in sections. There is a section for each
[shown network](#shown-and-hidden-networks), and `addresses` also has one for
each type of address on a network. A title line above each section names the
portfolio, the network and the type of its rows:

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

`hw` leaves out a section that has no rows. `--network` and `--type` limit a
listing to one network or to one type. When they leave one section only, the
listing has no title, since the command line already says which section it
is. `balances` is the exception: it keeps its title, which gives the total of
the network.

**The pager.** On a terminal, `hw` shows a listing such as `hw txs` through
the pager `less`. The arrow keys scroll the listing, `/` searches it and `q`
quits it. A listing that fits on the screen is printed as it is, without the
pager. To use another pager, name it in the environment variable `HW_PAGER` or
`PAGER`. `hw` reads `HW_PAGER` first, so that variable names a pager for `hw`
alone. An empty value, or `cat`, turns the pager off, as `--no-pager` does for
one command. The [configuration file](#the-configuration-file) has no setting
for the pager.

The pager does not wrap a line, so that the columns of a view stay lined up.
It cuts a line wider than the screen at the edge of the screen, and marks the
cut with a `>`. The right and left arrow keys scroll to the rest of the line.
`less` then stays open even for a short listing, so that you can scroll. The
compact and verbose views are laid out for 80 columns, so this happens mostly
to the rows of a table, with `--tabular`. The one exception is
`portfolios --verbose`: its lines wrap, so that you can copy the whole
descriptor of an account.

With `less` 608 or later, a table taller than the screen keeps its header at
the top while its rows scroll. Two kinds of table do not. A table in sections
has a title and a header in each section. A table that `--tag` or `--untagged`
filters starts with the line that says which rows it shows. `hw` sets the
options of `less` in the environment variable `LESS`. When you set `LESS`
yourself, `hw` leaves it as it is.

**Paths.** Every view writes the path of an address with the type of its
account in front, such as `bip84/0/0`. The path inside the account, `/0/0`, is
not enough. Each account of a network has an address at `/0/0`, and `balances`
and `coins` list both accounts of `btc` together. The type takes five columns.
In return, you do not have to tell the two accounts apart by the form of the
address. The path does not name the network: the title of the section does.

You can type a path where a command takes an address of the wallet. `show`,
`label`, `tag`, `untag`, `signmsg`, `--spend-from` and `--change-back` take
`bip84/0/0` as well as the address. A path names an address of the active
portfolio, on the network the command works on. When the portfolio has an
address at the path on several networks, `--network` says which one you mean.
`show` and `signmsg` derive the address from the public key of the account, so
they take any path, on the offline computer too. The other commands take only
the path of an address that the portfolio holds.

**Tags.** A [tag](#tags-on-addresses-and-transactions) is a word you put on
addresses and transactions to group them. Every listing except `portfolios`
shows the tags of its rows. The compact and the verbose views write each tag
between brackets after the label, as in `[kyc]`. A table has a `Tags` column.
It is the last column but one, before the label or the description. It is as
wide as the widest set of tags in the database: that of an address in the
tables of addresses, balances and coins, that of a transaction in those of
`txs` and `pendingtxs`, and that of a contact in the table of `contacts`. So
the labels after it stay lined up, whatever the tags of a row. A table never
cuts text.

`--tag TAG` shows only the rows that have the tag. Repeat it for several tags:
the listing then shows the rows that have any one of them. The first line of
the listing names the tags, so that you know that rows are left out:

```console
$ hw addresses --tag kyc
Addresses tagged kyc

portfolio0  btc  bip44

bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"  [cold] [kyc]
  unused
```

| Listing | What `--tag TAG` shows | What `--untagged` shows |
|---|---|---|
| `addresses`, `balances` | The addresses that have the tag | The addresses that have no tag |
| `coins` | The coins at an address that has the tag. A coin has no tag of its own. | The coins at an address that has no tag |
| `contacts` | The contacts that have the tag | The contacts that have no tag |
| `txs`, `pendingtxs` | The transactions that have the tag. `--address-tag TAG` shows the transactions that spend from or pay an address of their portfolio that has the tag. It also shows those that pay a contact of the [address book](#the-address-book) that has the tag, or that such a contact paid. You can combine the two options: the listing shows a transaction that matches any one of them. | The transactions left to sort: those that have no tag, and that neither pay a contact that has a tag nor were paid by one |

`--untagged` shows only the rows that have no tag: what you have not put in a
group yet. The first line says so, as in `Addresses without a tag`. With both
`--tag` and `--untagged`, the listing shows the rows of both. Each of these
options adds rows, and none of them removes a row that another one shows.

A filter only leaves rows out. A row that the listing shows is the same as in
the whole listing, with all of its tags. `hw` refuses a word that is not a tag
of the database, because an empty listing would not tell you that you mistyped
the tag. `--after` and `--max` page through the rows that the filter shows.
`--json` and `--csv` give those rows only too, without the line that names the
tags.

**Order.** A listing has one order. `txs` and `coins` follow the chain, the
newest first. `addresses` puts the newest address first, `balances` the
largest total, and `pendingtxs` the newest transaction. `--reverse` reverses
that order. So `hw txs --reverse` starts with the oldest transaction and ends
with the newest, as a bank statement does. It reverses the rows of each
section, and the sections keep their order: `btc` still comes before `bch`. It
applies to the screen, to `--json` and to `--csv`. `portfolios` takes no
`--reverse`: it lists the wallets and their portfolios by their numbers.

```console
hw txs --reverse
hw addresses --reverse
```

If you always want the reverse order, make it the default with
`hw setdefault --reverse`. It turns on the key `listing-reverse` of the
[configuration](#configuration), which is off unless you set it.
`--no-reverse` then brings back the usual order for one listing, and
`hw setdefault --no-reverse` brings it back for good. Like `listing-view`, the
key changes only what the screen shows. `--json` and `--csv` keep the usual
order unless you give `--reverse`. A script that pages through a listing then
reads the same rows whatever the configuration file says.

`--reverse` is the only option that changes the order of a listing. `hw` does
not sort by another column, such as the amount or the label. The balance that
`txs` shows after each transaction only makes sense in the order of the chain.
Every other order would also need an index of its own to stay fast in a large
wallet. To sort or filter by any column, write the listing with `--csv` or
with [`exportsheet`](#hw-exportsheet), and open it in a spreadsheet.

**Pages.** A script can read a listing one page at a time. `--max` says how
many rows a page has at most. `--after` names the last row of the page before,
and the next page starts after that row. With `--reverse`, the rows after a
row are those that follow it in the reverse order, so a script pages through
either order the same way. `--max` counts the rows of all the sections
together. `--after` continues in the section that holds the row, then in the
sections that follow.

```console
hw txs --max 20 --json
hw txs --max 20 --after TXID --json
```

The test networks share their keys, so one address can be in the sections of
several test networks. `addresses --after` and `balances --after` refuse such
an address. They name the networks that have it, and ask for `--network`. If
the listing continued in the first of those sections, the script would get the
same rows again and again. Page through one test network at a time with
`--network`.

**CSV files.** `--csv FILE` writes a listing to a new file for a spreadsheet,
instead of showing it. Each element takes one row. The columns hold everything
`hw` knows about the element, such as the txid, the coin and the fee, each in
a column of its own. A hash takes two columns: the hash in full, then its
[short id](#short-ids). Amounts are whole numbers of satoshis, whatever
`--unit` says. The fee rate of a transaction is the one number with a decimal,
written with a point, as in `8.6`. `--after` and `--max` choose the rows, and
`--reverse` their order, as they do on the screen.

```console
hw txs --csv txs.csv
hw coins --csv coins.csv
```

The file of a listing has the same columns as the tab of that listing in the
spreadsheet of [`exportsheet`](#hw-exportsheet): in the same order, filled the
same way. Rows copied from one fit under the headings of the other. The
columns follow four rules in every listing. When you know one file, you know
where to look in the others.

- **The account comes first.** Every row starts with `Wallet`, `Portfolio`,
  `Network` and `Type`, even though a listing covers one portfolio. A row that
  covers several accounts names each type, as in `bip44, bip84`. The file of
  `contacts` starts with `Network` alone, since a contact belongs to no
  portfolio.
- **Then comes what the listing is about.** The file of `coins` continues with
  the coin, and the file of `txs` with the txid.
- **A heading means one thing.** The columns that two listings share have the
  same headings, in the same order. The label of an address is `Address label`
  everywhere, and the label of a transaction is `Transaction label`. The
  [tags](#tags-on-addresses-and-transactions) come right after the label, as
  `Address tags` and `Transaction tags`. The tags of a row share one cell, in
  alphabetical order, with a space between them. A tag contains no space, so a
  filter of the spreadsheet that looks for a word finds the rows that have the
  tag. `Type` is always the type of the account. So the column that says
  `Incoming`, `Outgoing` or `Internal` is `Direction`, although the screen
  calls it the type of a transaction.
- **The date when `hw` created the element comes last.** A portfolio, an
  address and a pending transaction have such a date. The time of a
  transaction is different: a history is sorted by it, so it comes right after
  the txid.

Each time takes three columns. `Time (UTC)` gives it in UTC, as
`2023-11-14T22:13:20`, whatever the time zone of the computer. `Time (local)`
gives it in the time zone of the computer, in the same form. It is the time
the screen shows. `Time (offset)` says how far the local time was from UTC at
that time, as in `UTC+01:00`. A time in summer has the offset of summer time.

> **Good to know**
>
> - **A spreadsheet guesses what each cell of a CSV file holds.** A label made
>   of digits alone, such as `007`, becomes the number 7. The fingerprint of a
>   mnemonic is often made of digits alone, so the file of `portfolios` leaves
>   it out. `portfolios --verbose` gives it. About one short txid in 43 is made
>   of digits alone too, such as `01234567`, and becomes a number without its
>   leading zero. The full hash in the column before it stays text. The short
>   id of a coin stays text, since it has a colon. `exportsheet` keeps every
>   short id as text.
> - **A label that starts with `=`, `+`, `-` or `@` gets an apostrophe in
>   front.** Without it, a spreadsheet would run the label as a formula. A
>   label can come from a file of another wallet, and a formula could send
>   your rows to a web server. The apostrophe shows in the cell.
> - **A spreadsheet set to a decimal comma may expect semicolons.** Excel may
>   then show each row in one cell. Import the file with the comma as the
>   separator instead of opening it. LibreOffice asks which separator to use.
>   Such a spreadsheet may also take a fee rate, written `8.6`, for text. The
>   file of [`exportsheet`](#hw-exportsheet) holds the number itself.
> - **A cell that names several things holds a comma.** A comma and a space
>   separate the two types of a row, the paths of two accounts and the txids
>   of two replacements. The CSV file puts such a cell in quotes, as the
>   format asks. A script that splits each line at its commas must handle the
>   quotes, or read `--json` instead. Tags are the exception: a space
>   separates them, since no tag contains a space.
> - **The offset of a local time starts with `UTC`.** A spreadsheet would read
>   `+01:00` alone as a time of day, or as a formula.
> - **The file reveals your history.** Its addresses, amounts and labels tell
>   whoever reads it what you hold and whom you paid. `hw` makes the file
>   readable by you alone.

### Short ids

A txid has 64 digits, too many for a table and for typing. The listings and the
compact views name a transaction by its short id instead: the first 8 digits of
its txid, such as `a1075db5`. A pending transaction has no txid until it is
signed. Until then, it has the short id of its nosigHash, the hash of the
transaction without its signatures. A coin has the short id of its
transaction, a colon and its index, such as `a1075db5:1`. `--verbose` gives
the hash in full, then its short id.

Every command that takes a hash or a coin takes its short id too:

```console
hw signtx a1075db5
hw lockcoin a1075db5:1
hw label a1075db5 "rent of October"
hw tag a1075db5 rent
hw txs --after a1075db5
```

The [Tab key](#shell-completion) completes a hash or a coin to its short id.

A short id is the start of the hash itself, not a number the wallet hands out.
It means the same on every computer, the offline one included, and in a block
explorer, where the txid starts with it. A typo in it almost never matches
another transaction, so `hw` refuses it rather than act on the wrong one.

Two hashes share their first 8 digits about once in 4 billion pairs. When a
short id matches two or more hashes that a command can take, `hw` names each in
full and asks for the full hash. It never picks one. `hw` refuses a part of a
hash longer than 8 digits: give the short id or all 64 digits.

> **Good to know**
>
> - **`preparesweep --coin` takes the txid in full.** The wallet does not hold
>   the coins it sweeps, and the server looks up a whole txid only.
> - **Files and scripts get whole hashes.** The transaction files, the files of
>   labels and the answers in JSON give each hash in full. Another wallet, or a
>   script, needs the exact hash. `--csv` and `exportsheet` give both forms.

### Tags on addresses and transactions

A tag is a word you put on addresses and transactions to group them, such as
`kyc`, `savings` or `rent`. The other commands then work on the whole group. A
listing can show only the rows that have a tag, and a payment can spend only
the coins at the addresses that have a tag.

```console
hw tag ADDRESS kyc                          # put an address in the group kyc
hw receive --tag kyc                        # a new address, tagged from the start
hw addresses --tag kyc                      # the receiving addresses of the group
hw balances --tag kyc                       # what the group holds
hw send ADDRESS:0.01 --spend-from-tag kyc --tag-change --fee-rate low   # pay from the group, and keep the change in it
hw tag ADDRESS kyc --with-change            # the address, and the change of the payments made from it
hw tag savings --lock-coins                 # keep the coins of a group out of everyday payments
hw send ADDRESS:0.01 --tag rent --fee-rate low   # a payment, tagged as you make it
hw txs --untagged                           # the transactions left to sort
hw report business.pdf --transactions --tag business   # a statement of one tag
hw tags                                     # every tag, with what has it
```

**A tag is not a label.** A [label](#hw-label) says what one thing is for, in
your own words, and no command acts on it. A tag names a group that many
things belong to, and the commands take it as an option. An address or a
transaction has one label, and any number of tags. A change address takes
tags, although it takes no label. It needs no label, since the label of its
payment already says what the change is for. It needs tags, since the change
of a payment belongs to a group as much as the coins you received.

**The change stays in its group when you ask.** A coin of a group does not
stay at the address you tagged. A payment spends it, and what is left goes to
a change address, which has no tag. `hw` does not tag that address by itself.
A tag may describe the address alone, such as what it receives, so only you
know whether the change still belongs to the group. Two options tag the
change.

- **`--tag-change`, as you pay.** On [`send`](#hw-send) and
  [`preparetx`](#hw-preparetx), the new change address takes the tags that
  every coin spent has. A payment with `--spend-from-tag kyc --tag-change`
  spends coins that all have `kyc`, so its change is tagged `kyc`.
  `balances --tag kyc` still counts it, and the next `--spend-from-tag kyc`
  spends it.
- **`tag --with-change`, afterwards.** [`tag`](#hw-tag) tags the address, then
  the change of every payment that spent from it, then the change of the
  payments that spent that change, and so on down to the coins you hold now.
  Use it to make a group out of an address that has a history.
  [`untag --with-change`](#hw-untag) takes a tag off the same way.

Both follow one rule. A change address takes a tag only when every coin that
the payment spent has the tag. A payment that spends a `kyc` coin and a coin
without the tag mixes the two, and its change belongs to neither group. A tag
passes from the coins a payment spends to its change, and never the other
way. A tag on a change address says nothing about the addresses its coins
came from.

**When to use them.** Coins are not all alike to their owner. Some came from
an exchange that knows your name, and others from a sale between two people.
A payment that spends both tells whoever reads the chain that one person
holds them. Tag the addresses of each kind, and `--spend-from-tag` pays from
one kind alone. Before a payment spends coins of different tags together,
`hw` [asks first](#hw-preparetx). So it never joins two kinds unless you
agree. Tags sort a history too. Tag the payments of a kind, such as `rent`,
and `txs --tag rent` lists them.

**Tag a payment as you make it.** You know what a payment is for at the
moment you make it. `--tag` on [`send`](#hw-send), [`sendall`](#hw-sendall),
[`preparetx`](#hw-preparetx) and [`preparesendall`](#hw-preparesendall) tags
the new transaction at that moment. `hw tag TXID rent` does the same
afterwards. The tag stays on the transaction through signing and sending, and
in the history afterwards. If you gave the tag an
[account of the journal](#hw-exportjournal), the journal files the payment
under that account without another step.

**What is left to sort.** `hw txs --untagged` lists the transactions that no
tag sorts yet. Tag those you recognize, and list again. The listing gets
shorter until it is empty, and the month is sorted. A transaction counts as
sorted when it has a tag of its own, or when a contact that it pays, or that
paid it, has a tag. So tag your landlord once, and the rent payments no
longer show in this listing. The tags of your own addresses do not count.
`kyc` on an address says which group its coins are in, not what a payment to
it was for. The other listings take `--untagged` too: `coins --untagged`
lists the coins in no group, and `balances --untagged` adds them up.

**One tag for your accountant.** The commands that write files for others
take the same options. So you can hand over one group, and not your whole
wallet:

- `hw report business.pdf --transactions --tag business` writes a
  [statement](#hw-report) of the transactions tagged `business`.
- `hw exportjournal business.ledger --tag business` writes a
  [journal](#hw-exportjournal) of them.
- `hw exportsheet business.ods --tag business` writes a
  [spreadsheet](#hw-exportsheet) of everything that has the tag.
- `hw report kyc.pdf --balances --tag kyc --date 2025-12-31` states what the
  addresses tagged `kyc` held on that day. `hw balances --tag kyc` lists what
  they hold now.

Each file holds what the listings show with the same options, so look at
`hw txs --tag business` first.

**A tag can lock its coins.** A tag alone protects no coin. A payment without
`--spend-from-tag` selects among every coin of the portfolio, tagged or not,
so a payment for groceries may spend your savings.
`hw tag savings --lock-coins` stops that. From then on, no payment spends a
coin at an address tagged `savings`, unless the payment names the tag with
`--spend-from-tag`:

```console
hw tag savings --lock-coins                 # the tag locks its coins
hw send ADDRESS:0.01 --fee-rate low         # spends other coins, never those of savings
hw send ADDRESS:0.5 --spend-from-tag savings --fee-rate low   # spends them, this once
hw tag savings --unlock-coins               # the tag no longer locks its coins
```

The lock is on the tag, not on each coin. A coin that arrives tomorrow at an
address that has the tag is locked at once. [`lockcoin`](#hw-lockcoin) cannot
do that, since it locks only the coins you name, one by one.
[`tags`](#hw-tags) says which tags lock their coins, [`coins`](#hw-coins)
shows each of their coins as `locked by tag`, and [`balances`](#hw-balances)
counts them as unspendable. [`tag`](#hw-tag) gives the rules of the lock.

For funds that must never mix, a second
[portfolio](#where-a-command-works) is safer still, since a payment spends
from one portfolio only.

**The word.** A tag is one word, made of the letters `a` to `z`, digits, `-`
and `_`. It starts with a letter or a digit, and has 32 characters at most.
Capital letters are taken as small ones, so `Kyc` and `kyc` are one tag.
Spaces, accented letters and other signs are refused.

These rules keep a tag easy to type. Such a word needs no quotes in any shell,
and several fit on a command line and in one cell of a spreadsheet. It is
never taken for a path, which has slashes, or for a coin, which has a colon.
Accented letters are refused because one can be written as one character or
as two that look the same. Two tags that look alike would then be two groups.

A tag exists from the first time you use it. No command creates one first. It
belongs to the whole database, not to a portfolio. Addresses tagged `kyc` in
two portfolios are in one group, under one colour. [`tags`](#hw-tags) lists
every tag, [`renametag`](#hw-renametag) gives one a new name, and
[`deletetag`](#hw-deletetag) deletes one everywhere.

**Colours.** A tag can have a colour: `red`, `green`, `yellow`, `blue`,
`magenta` or `cyan`. The screen then shows the tag in that colour.
`hw tag kyc --color red` sets it, and `--color none` removes it. A colour is
only a help to the eye. No command behaves differently for it. A tag without
one is shown in the colour of the text.

**How they are shown.** The compact and the verbose views write each tag
between brackets after the label, as in `"Wages"  [cold] [kyc]`. The brackets
set a tag apart from the words around it, even on a screen without colours. A
table has a `Tags` column, and a [CSV file](#listings) the columns
`Address tags` and `Transaction tags`. With `--json`, an address and a
transaction give their tags under `tags`, each with its `name` and its
`color`. A coin gives the tags of its address under `addresstags`.

**What takes a tag.**

| Command or option | What it does |
|---|---|
| [`tag`](#hw-tag), [`untag`](#hw-untag) | Put tags on an address, a transaction or a contact of the [address book](#the-address-book), and take them off |
| `--tag TAG` on [`contacts`](#hw-contacts), and on [`newcontact`](#hw-newcontact) | On `contacts`, lists only the contacts that have the tag. On `newcontact`, tags the new contact |
| [`receive --tag TAG`](#hw-receive) | Tags the new address |
| `--tag TAG` on [`addresses`](#hw-addresses), [`balances`](#hw-balances) and [`coins`](#hw-coins) | Lists only the rows of the addresses that have the tag |
| `--tag TAG` on [`txs`](#hw-txs) and [`pendingtxs`](#hw-pendingtxs) | Lists only the transactions that have the tag |
| `--address-tag TAG` on `txs` and `pendingtxs` | Lists only the transactions that spend from or pay an address that has the tag, and those that pay or were paid from a contact that has it |
| `--untagged` on `addresses`, `balances`, `coins` and `contacts` | Lists only the rows that have no tag. For a coin, its address has no tag |
| `--untagged` on `txs` and `pendingtxs` | Lists only the transactions left to sort: those that have no tag, and that neither pay nor were paid from a contact that has one |
| `--tag TAG`, `--address-tag TAG` and `--untagged` on [`report --transactions`](#hw-report) and [`exportjournal`](#hw-exportjournal) | Write to the statement or the journal only the transactions that `txs` lists with the same options |
| `--tag TAG` and `--untagged` on [`report --balances`](#hw-report) | Write to the balance statement only the addresses that `balances` lists with the same options |
| `--tag TAG` and `--untagged` on [`exportsheet`](#hw-exportsheet) | Write to each tab only the rows that its listing shows with the same option |
| `--tag TAG` on [`send`](#hw-send), [`preparetx`](#hw-preparetx), [`sendall`](#hw-sendall) and [`preparesendall`](#hw-preparesendall) | Tags the payment as it is made. It chooses no coin |
| `--spend-from-tag TAG` on `send`, `preparetx`, `sendall` and `preparesendall` | Spends the coins at the addresses that have the tag. `--coin-type` narrows them down to one type |
| `--lock-coins` and `--unlock-coins` on [`tag`](#hw-tag), with a tag alone | Make the tag lock its coins, or stop it from locking them. A payment does not spend the coins that a tag locks, unless `--spend-from-tag` names the tag |
| `--tag-change` on `send` and `preparetx` | Tags the new change address with the tags that every coin spent has |
| `send`, `preparetx`, `sendall`, `preparesendall` and [`bumpfee`](#hw-bumpfee) | Ask before they spend together coins that do not all have the same tags. `--yes` answers in advance |
| `--with-change` on [`tag`](#hw-tag) and [`untag`](#hw-untag) | Also tags the change of the payments that spent from the address, then the change of the payments that spent that change, and so on. On `untag`, takes the tags off them |
| [`exportwallet`](#hw-exportwallet), [`importwallet`](#hw-importwallet) | Write the tags to a file with the labels, and read them back |
| `--csv`, [`exportsheet`](#hw-exportsheet) | Give the tags of each row in a column |
| [`exportjournal`](#hw-exportjournal) | Writes the tags of a transaction on its entry, as tags of ledger. Files a payment under the account you gave its tag, as in `Expenses:Housing:Rent` for `rent` |

The tags are in the database. `hw` copies them with the labels after each
command that changes them, as [Automatic backups](#automatic-backups) says.

> **Good to know**
>
> - **The change of a payment has no tag unless you ask.** `--tag-change`
>   tags it as you pay, and `tag --with-change` tags it afterwards. The change
>   of a payment that mixes groups takes only the tags that every coin spent
>   has, so neither option puts it in one of the groups. To put it in a group
>   all the same, tag the change address itself, by the path that the answer
>   of the payment shows, as in `hw tag bip84/1/0 kyc`. `untag` takes off a
>   tag you do not want on it.
> - **On a payment, `--tag` and `--spend-from-tag` do different things.**
>   `--tag rent` says what the payment is for, and puts the tag on the
>   transaction. `--spend-from-tag kyc` says which coins pay it, and puts no
>   tag on the transaction. A payment can take both.
> - **A new receiving address has no tag.** `receive --tag` gives it one. An
>   address that `sync` finds used has none either.
> - **`--untagged` looks at the tags, not at the journal.** A transaction
>   with a tag counts as sorted, so `txs --untagged` leaves it out, whether or
>   not the tag has an [account of the journal](#hw-exportjournal). When the
>   tag has no account, the journal still posts the transaction to
>   `Expenses:Unknown` or `Income:Unknown`.
>   `ledger -f hw.ledger register Unknown` lists those.
> - **A tag stays when nothing has it any more.** It keeps its colour, for the
>   day you use it again, and `tags` shows it as `not used`. `deletetag`
>   deletes it.
> - **A transaction file carries no tag.** The tags of a pending transaction
>   stay in the database where you set them, with `tag` or with
>   `preparetx --tag`. They are still there when the signed file comes back
>   from the offline computer. [`show FILE`](#hw-show) on the offline computer
>   shows none.

### The address book

The address book keeps the addresses of others: the people and the services
you pay, or that pay you. Each entry is a contact. A contact is one address,
with a label that says whose it is, and
[tags](#tags-on-addresses-and-transactions).

```console
hw newcontact ADDRESS Landlord              # keep an address, with its label
hw newcontact ADDRESS Alice --tag friends   # keep it with a tag
hw contacts                                 # list the address book
hw label ADDRESS "Mr Smith"                 # change the label of a contact
hw tag ADDRESS rent                         # tag a contact
hw show ADDRESS                             # show one contact
hw deletecontact ADDRESS                    # delete it
```

**What it is for.** `hw` knows the addresses of its own wallets, and shows
them apart in a transaction. Every other address looks like a stranger's, even
the one you pay every month. A contact gives such an address a name. The
history then says whom each payment went to, and a payment is easier to check
before you sign it.

**In a transaction.** `txs`, `pendingtxs` and `show` underline an address of
the address book, and write the label of its contact after it:

```console
$ hw pendingtxs
...
  portfolio0 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU "Landlord"
```

`--verbose` puts the label on the line below the address, with the tags of the
contact, as it does for an address of the wallet:

```console
$ hw txs --network btc --verbose
...
  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
             "Landlord"  [rent]
...
```

The answers of `send`, `preparetx` and `signtx` show a transaction the same
way. So you see the name of the contact you pay before you type the mnemonic.

**The history of a contact.** Tag a contact, and `txs --address-tag TAG` lists
the transactions of the active portfolio that pay it or that it paid. When
several contacts have the tag, the listing shows the transactions of all of
them, such as every payment to your `suppliers`. Each transaction names the
contact and the tag that put it in the listing, on a line of its own:

```console
$ hw txs --network btc --address-tag rent
Transactions at addresses tagged rent

...
  balance 0.00050000 btc, at bip44/0/0
  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU "Landlord" [rent]
```

The second line says that the payment was received at `bip44/0/0`. The
contact that paid it comes on the line below, since an address in full with
its label would make the second line too long. For a payment you made, the
second line already names the address that the payment pays the most. When
that address is the contact, its tag follows it there.

You may add or tag the contact long after the transaction. `hw` keeps the
addresses that each transaction pays and was paid from, so the listing finds
the past transactions of a new contact at once.

The underline tells a contact from a stranger at a glance. On a screen without
styles, the label after the address does. The words `address book` follow a
contact that has no label. A payment that has a label of its own shows that
label after the address, in place of the label of the contact, since it says
what the payment is for.

**The address, twice.** Every view of a contact gives its address in one piece,
and again in groups of four characters:

```console
$ hw contacts --network btc
bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  "Alice, savings"  [friends]
  bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4

1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  "Landlord"  [rent]
  1JCq 8Aa9 d9rg 4T4X V93R V3DM xd5u 7GkS SU
```

The first form is for copying. A double click selects it whole. The second is
for comparing. Read it against the address the payee gave you, one group at a
time. People compare an address in one piece by its first and last characters
only, and a look-alike address is made to pass exactly that check. A Bitcoin
Cash address keeps its prefix in one piece before the groups, as in
`bitcoincash: qpm2 qszn hks2`.

**Paying a contact.** A payment takes the address of a contact, as it takes
any address. The Tab key writes it for you, once
[shell completion](#shell-completion) is set up. Type the start of the address
and press Tab. In bash, you can type the start of the label instead:

```console
hw send Land<Tab>                 # becomes: hw send 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\:
```

The Tab key writes the address and the colon, and you type the amount after
it. bash writes that colon as `\:`, and removes the backslash when the command
runs. For `sendall` and `preparesendall`, the Tab key writes the address
alone, since their recipient has no amount: it takes what remains.

**What a contact is.**

- **One address on one network.** Somebody with two addresses is two contacts,
  which may share a label or a tag. Most addresses are valid on one network
  only. A legacy address is valid on `btc` and on `bch`, and
  [`newcontact`](#hw-newcontact) then takes the default network, unless
  `--network` names another.
- **An address of somebody else.** An address of a wallet of the database is
  refused. It has a path, a label and tags already.
- **Of the whole database.** The address book does not belong to a portfolio.
  The same contacts show whichever portfolio is active, and they stay when a
  portfolio or a wallet is deleted.

**What works on a contact.**

| Command or option | What it does |
|---|---|
| [`newcontact`](#hw-newcontact), [`deletecontact`](#hw-deletecontact) | Add an address to the address book, and delete one |
| [`contacts`](#hw-contacts) | Lists the address book. `--tag` lists only the contacts that have a tag, and `--untagged` only those that have none |
| [`label ADDRESS LABEL`](#hw-label) | Changes the label of a contact |
| [`tag`](#hw-tag), [`untag`](#hw-untag) | Put tags on a contact, and take them off |
| [`show ADDRESS`](#hw-show) | Shows one contact |
| [`txs`](#hw-txs), [`pendingtxs`](#hw-pendingtxs), `show`, and the answers of the payment commands | Underline the address of a contact, and give its label |
| `--address-tag TAG` on `txs` and `pendingtxs` | Lists only the transactions that pay a contact that has the tag, or that such a contact paid |
| The Tab key | Writes the address of a contact where a payment takes a recipient |
| [`exportwallet`](#hw-exportwallet), [`importwallet`](#hw-importwallet) | Write the address book to a file with the labels, and read it back |
| `contacts --csv`, [`exportsheet`](#hw-exportsheet) | Write the address book to a spreadsheet |
| [`exportjournal`](#hw-exportjournal) | Names the entry of a payment after the contact it pays, and writes the label of the contact below the amount it was paid. Files that payment under the account you gave a tag of the contact. Files a payment received under the account of a tag of the contact it came from |

The address book is in the database. `hw` copies it with the labels after each
command that changes it, as [Automatic backups](#automatic-backups) says.

> **Good to know**
>
> - **`hw` cannot tell whose an address is.** A contact says only what you
>   typed. When you add it, compare the address in fours with the address the
>   payee gave you, over a channel you trust. After that, the label vouches
>   for the address each time you pay it.
> - **A payment takes an address, never a label.** `hw send Landlord:0.001` is
>   refused. Two contacts may share a label, and a command line must say
>   exactly where the funds go. The Tab key writes the address for you.
> - **Each computer has its own address book.** A transaction file carries no
>   contact. On an offline computer, [`show FILE`](#hw-show) and `signtx`
>   underline the addresses of the address book of that computer. Copy the
>   address book there with `exportwallet` and `importwallet`, or check the
>   address itself.
> - **An address the wallet holds is shown as the wallet's.** A wallet of the
>   database may come to hold the address of a contact, as when
>   `importwallet` adds that wallet. The views then show the address by its
>   path, in blue. The contact stays in the address book until you delete it.
> - **`--address-tag` looks at the addresses a transaction shows.** A
>   payment received shows the addresses it was paid from, and not what its
>   payer paid to others in the same transaction. So a payout that pays you
>   and one of your contacts is not listed for that contact.

### Files

**`hw` never overwrites a file.** `--output`, `--csv`, `exportwallet`,
`exporttx`, `backup`, `exportsheet`, `exportjournal` and `report` refuse a
file that exists, even one that appeared while the command ran. `signtx` and
`signsweep` replace their `--input` file only when you agree. The only files
`hw` removes are the older ones of its own
[automatic backups](#automatic-backups).

The one exception is a file of `hw`'s own, which you do not name: the
[journal it keeps](#a-journal-kept-up-to-date) for ledger in `~/.hw`, when you
turn it on. `hw` replaces it after each command that changes it, and removes
it when you turn it off.

`hw` writes a file in full before it gives the file its name, so a crash or a
power loss leaves no half-written file. The file is readable by its owner
alone.

**`hw` writes files only, never the standard output.** Many programs read `-`
as the standard output. `hw` refuses `-` as the name of a file to write: the
answer of a command already goes to the standard output, and a stream is not
readable by its owner alone. For a file that is really named `-`, give its
path, `./-`.

### Automatic backups

`hw` backs up your wallets by itself. After each command that changes your
wallets, your labels, your [tags](#tags-on-addresses-and-transactions) or
your [address book](#the-address-book), it writes a copy into
`~/.hw/auto-backups`. It keeps the recent copies, and fewer and fewer of the
older ones. There are two kinds of copy:

- **The database**, after each command that adds a wallet or a portfolio:
  `newportfolio`, `recover` and `importwallet`. `recover` and `importwallet`
  make a copy only when they create a portfolio.
- **The labels**, after each command that changes the labels, the tags or
  the address book. A copy of the labels is the file that
  [`exportwallet`](#hw-exportwallet) writes without options. It holds every
  portfolio of every wallet, with their descriptions, the labels and the
  tags of their addresses and transactions, the coins locked with
  [`lockcoin`](#hw-lockcoin), and the address book.

```console
$ ls ~/.hw/auto-backups
portfolios-labels-2026-11-14T09-12-05.317Z-wallets-1-portfolios-2.jsonl
portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-2.jsonl
portfolios-labels-2026-11-14T10-20-48.902Z-wallets-1-portfolios-3.jsonl
portfolios-version-5-2026-11-14T09-11-40.250Z-wallets-1-portfolios-1.sqlite
portfolios-version-5-2026-11-14T09-12-05.104Z-wallets-1-portfolios-2.sqlite
portfolios-version-5-2026-11-14T10-20-48.689Z-wallets-1-portfolios-3.sqlite
```

Each name gives the time of the copy and how many wallets and portfolios it
holds. The name of a copy of the database also gives its format version. The
time is in UTC, to the millisecond. So the names sort in the order in which
`hw` made the copies, whatever your time zone, and two commands in the same
second make two files.

`hw` makes a copy after the change, so the newest copy holds the wallet as
the last command left it. When you create your third portfolio, the copy
holds three portfolios. `hw` writes the new copy first, and then removes the
copies it no longer keeps.

Three commands remove data: `deleteportfolio`, `deletewallet` and
`restoredatabase`. `hw` makes a copy of the database and of the labels before
each of them. That copy is the last one that holds what the command removed,
and the answer of the command names its two files. It is in
`~/.hw/auto-backups` with the other copies, and `hw` keeps it by the same
rules.

These three commands make no copy after they are done. Each command makes
one backup, of what is worth the most, and here that is the data the command
removes. What the database holds afterwards is in another file already. After
a deletion, the copy made before holds everything that is left. After a
restore, the file you restored from holds everything that the database holds.
A copy made after would also take the place of the copy made before as the
last copy of its hour, which is one of the copies that `hw` keeps longest.

**Which copies are kept.** For each kind, `hw` keeps a copy when at least one
row of this table keeps it:

| Kept | Which copies |
|---|---|
| The 10 newest | Whatever their times |
| 24 by the hour | The last copy of each hour |
| 7 by the day | The last copy of each day |
| 4 by the week | The last copy of each week, which starts on Monday |
| 12 by the month | The last copy of each month |

Each row reaches back over one unit of the next row: the hourly copies cover
a day, the daily copies a week, the weekly copies a month and the monthly
copies a year. One copy often counts for several rows. So the directory holds
at most 57 copies of each kind, and usually far fewer.

The ten newest copies let you step back through your last ten changes. The
other rows keep older copies, which a plain count of ten would lose. Each
label you set makes a copy. So a script that sets twenty labels makes twenty
copies in a few seconds. If `hw` kept the ten newest copies only, that script
would push out every copy made before it ran. With the other rows, the last
copy of the hour before the script stays, and so do the copies of the days
and months before. Backup programs call this scheme grandfather-father-son.

`hw` counts the hours, days, weeks and months in which it made a copy, in
UTC, as the names give them. It does not count back from today. So a wallet
left alone for two years loses no copy, since only a newer copy pushes an
older one out. The numbers of the table are fixed, and no key of the
configuration changes them.

These commands make a copy of the labels: `newportfolio`, `recover`,
`importwallet`, `label`, `tag`, `untag`, `renametag`, `deletetag`,
`newcontact`, `deletecontact`, `receive` with `--label` or `--tag`,
`lockcoin`, `unlockcoin`, `preparetx` with `--tag-change`, `signtx`,
`signsweep`, `send`, `sendall`, `sweep`, `broadcast`, `importtx` and
`deletetx`. `hw` does not write a copy of the labels that is equal to the
newest one. So a
command that changes nothing pushes out no older copy.

A command that fails makes no copy. `hw` prints nothing when it makes a copy.
When it cannot make one, it warns on stderr, and the command itself is still
done:

```console
$ hw label "Savings"
The labels were not backed up. Another hw command took the database before the
copy was made. The command itself is done. To make a copy now, run: hw backup
```

[`showbackups`](#hw-showbackups) tells how many copies there are of each kind,
and how long ago the newest was made. Run it to check that the copies are
being made.

Before you restore a copy, give it to [`show`](#hw-show) to see what it
holds. For a copy of the database, `show` lists the wallets and their
portfolios. For a copy of the labels, it shows what `importwallet` would do
with the file.

To restore a copy of the database, run
[`restoredatabase`](#hw-restoredatabase) with it. To restore a copy of the
labels, run [`importwallet`](#hw-importwallet) with it in a new database.
`importwallet` brings back the portfolios, their labels and their tags, and
a `sync` then brings back their history.

The copies are on by default. To turn them off, set `autobackup-database` or
`autobackup-labels` to `false` in the [configuration](#configuration).

**Files of your own in `auto-backups`.** `hw` removes the older copies it made
there, and nothing else. It treats a file as one of its copies only when all
of these are true:

- Its whole name has the form `hw` writes. That is `portfolios-version-`,
  the version, the time and the two counts, ending in `.sqlite`. Or it is
  `portfolios-labels-`, the time and the two counts, ending in `.jsonl`.
  Every part is written as `hw` writes it, with nothing added.
- It is a regular file, not a directory or a link.
- It is not empty. A crash or a power loss can leave an empty file under
  such a name, in place of a copy that was not finished. `hw` does not count
  it among the copies it keeps, and removes it when it makes the next copy.

A file under any other name stays where it is, and `hw` does not count it
among the copies it keeps. So a file you name yourself is safe there, such
as `before-the-move.sqlite`. A copy that `hw` made is safe too, once you add
a word to its name, as in
`portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-2-keep.jsonl`.
Still, the better place for a file you want to keep is outside this directory.
[`backup`](#hw-backup) writes the same two files to `~/.hw/user-backups`,
where `hw` removes nothing.

> **Good to know**
>
> - **`preparetx` makes no copy of the labels, unless it has
>   `--tag-change`.** Nor do `preparesendall`, `preparesweep` and `bumpfee`.
>   The file of the labels holds only the transactions that are signed. So
>   the label and the tags of a pending transaction go into a copy once
>   `signtx`, `signsweep`, `send`, `sendall` or `sweep` signs the
>   transaction.
>   `preparetx --tag-change` makes a copy because it tags a change address,
>   and the file holds every tagged address.
> - **`sync` makes no copy.** In the file of the labels, a sync changes only
>   what the chain tells: the height, the fee and the value of a transaction,
>   and the locked coins that were spent. A sync after `importwallet` brings
>   all of that back.
> - **A `send` or a `sendall` that could not send its transaction makes no
>   copy.** The transaction stays signed and pending, with its label. The
>   [`broadcast`](#hw-broadcast) or [`deletetx`](#hw-deletetx) that you run
>   next makes the copy.
> - **A wallet you delete disappears from the copies with time.** The copy
>   made right before `deletewallet` is the last one that holds it. The
>   copies made after later changes do not hold it, and they push out the
>   older ones with time, by the table above. To keep a wallet for good, run
>   [`backup`](#hw-backup) before you delete it: `hw` never removes a file in
>   `~/.hw/user-backups`.
> - **After a deletion, the newest copy still holds what was deleted.** That
>   stays so until the next command that makes a copy. To bring back the
>   database as it is now from that copy, restore it and delete again.
> - **After a restore, the newest copy holds the database that was
>   replaced.** `hw` makes no copy of the restored database: the file you
>   restored from is that copy, so keep it. To have a copy of the restored
>   database in `~/.hw` too, run [`backup`](#hw-backup).
> - **`hw` treats a file moved in from `user-backups` as one of its copies.**
>   A copy of the database or of the labels has the same form of name in both
>   directories. Once you move it into `auto-backups`, `hw` removes it when
>   no row of the table above keeps it.
> - **The copies stay with the database.** `auto-backups` is in `~/.hw`, next
>   to the database. When `~/.hw` is a
>   [link to another disk](#keep-hw-on-another-disk), an encrypted one for
>   instance, the copies are on that disk too.
> - **The copies show every address of every wallet**, as a file of
>   `exportwallet` does. When `hw` creates `auto-backups`, it lets only you
>   enter the directory. It makes each copy readable by you alone.

### A damaged database

A copy of `~/.hw/portfolios.sqlite` that stopped halfway, a full disk or a
file put there by mistake can leave a file that SQLite cannot read. `hw`
then tells you so, and how to get your wallet back. Every command stops with
the same answer, and changes nothing:

```console
$ hw portfolios
The database /home/you/.hw/portfolios.sqlite is damaged: SQLite cannot read it
(database disk image is malformed). To put the newest backup in its place, run:

  hw restoredatabase /home/you/.hw/auto-backups/portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-2.sqlite

restoredatabase first moves the damaged database into the directory
/home/you/.hw/broken-databases. hw showbackups shows the other backups.
```

The words between the parentheses are those of SQLite. The backup that `hw`
names is the newest copy of the database in `~/.hw/auto-backups` and
`~/.hw/user-backups`, by the time in its name. Two commands still run on a
damaged database: [`showbackups`](#hw-showbackups), to see the other backups,
and [`restoredatabase`](#hw-restoredatabase), to put one in place:

```console
$ hw restoredatabase ~/.hw/auto-backups/portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-2.sqlite
The database was restored from /home/you/.hw/auto-backups/portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-2.sqlite

wallet0  bc560f95
  * portfolio0  created 21 Oct 2026  "Savings"
    portfolio1  created 28 Oct 2026

The damaged database it replaced was moved to /home/you/.hw/broken-databases/portfolios-2026-11-16T08-12-45.310Z.sqlite

Next, sync the wallet, to bring the history of the copy up to date:
  hw sync
```

`restoredatabase` asks nothing here, and makes no backup first: `hw` cannot
read what the damaged database holds. It moves the file instead, and deletes
nothing. Three steps bring the wallet back as it was:

```console
hw restoredatabase FILE     # the newest copy of the database
hw sync                     # its history, up to date
hw importwallet FILE        # the newest copy of the labels in ~/.hw/auto-backups
```

The last step matters when you changed a label, a tag or a contact after the
copy of the database was made. `hw` copies the database when a command adds a
wallet or a portfolio, and the labels after each change to them. So the
newest copy of the labels is often the newer one.

**The directory `broken-databases`.** The damaged database goes to
`~/.hw/broken-databases`, under the name of the database and the time of the
move, as in `portfolios-2026-11-16T08-12-45.310Z.sqlite`. The files that
SQLite keeps beside a database, `portfolios.sqlite-wal` and
`portfolios.sqlite-shm`, go with it under the same name. `hw` never reads the
directory again and never removes a file there. Delete the files once your
wallet is back.

**Why `hw` does not move the file at once.** `hw` could move a damaged
database aside as soon as it sees one, and start a new database. The next
command would then show a wallet without a portfolio, with no word of why. So
`hw` changes nothing until you run `restoredatabase`, and repeats its answer
until then. `restoredatabase` is also safer than a move by hand: a
`portfolios.sqlite-wal` left behind holds changes of the old database, and
SQLite would apply them to the new one.

**Without a backup of the database.** When `hw` has no copy of the database
in its two directories, its answer says so. It takes a backup you keep
elsewhere with `restoredatabase FILE`. Without one, it gives the commands that
move the database aside and create the wallet again, in two ways:

```console
$ hw portfolios
The database /home/you/.hw/portfolios.sqlite is damaged: SQLite cannot read it
(file is not a database). hw has no backup of it in the directories
/home/you/.hw/auto-backups and /home/you/.hw/user-backups.

With a backup kept elsewhere, run hw restoredatabase FILE, which first moves the
damaged database into the directory /home/you/.hw/broken-databases.

Without one, move the damaged database aside and create the wallet again:

  mv /home/you/.hw/portfolios.sqlite /home/you/.hw/portfolios-old.sqlite

Then, from the newest copy of its labels, which brings back the portfolios,
their labels and their tags:

  hw importwallet /home/you/.hw/auto-backups/portfolios-labels-2026-11-15T17-40-02.551Z-wallets-1-portfolios-2.jsonl
  hw sync
  hw importwallet /home/you/.hw/auto-backups/portfolios-labels-2026-11-15T17-40-02.551Z-wallets-1-portfolios-2.jsonl

The second importwallet sets the labels of what the sync found. Or, from the
mnemonic, which brings back the portfolios that were paid, without their labels:

  hw recover
```

The first way needs no mnemonic. [`importwallet`](#hw-importwallet) runs
twice because a label of a transaction or of an address that the new database
does not have yet waits for the [`sync`](#hw-sync) to find it. The second way
is [`recover`](#hw-recover). When `hw` has no copy of the labels either, it
gives the second way only. A database that another build of `hw` made, with
other tables, gets the same two ways: no backup of the database helps there,
since that build made the backups too.

> **Good to know**
>
> - **`hw` tells a damaged database by what SQLite says, and checks nothing
>   more.** A file that SQLite reads is taken as sound.
> - **Other failures of the database get a sentence too.** A full disk, or a
>   database file that you may not write, stops the command with the words of
>   SQLite, as in `SQLite could not use the database
>   /home/you/.hw/portfolios.sqlite (attempt to write a readonly database)`.
>   Nothing is moved then.

### A journal kept up to date

A journal is a text file that lists your transactions as accounting entries.
[ledger], the command-line accounting program, reads it and makes reports
from it. [`exportjournal`](#hw-exportjournal) writes such a journal once, and
the file is out of date after the next sync. `hw` can keep a journal up to
date for you instead. The last question of the [setup](#set-up-hw) asks
whether you want it. `setdefault --journal` turns it on later:

```console
$ hw setdefault --journal
journal: true
Written to /home/you/.hw/config.toml
hw now keeps a journal for ledger at /home/you/.hw/portfolios.ledger
```

`hw` writes the journal to `~/.hw/portfolios.ledger` at once, next to the
database. It then writes the journal again after each command that changes
what it holds. The file is the one `exportjournal` writes, byte for byte.
Include it once in a journal of your own, and your reports are up to date
after every sync:

```
include ~/.hw/portfolios.ledger
```

| Command | What it changes in the journal |
|---|---|
| [`sync`](#hw-sync), [`recover`](#hw-recover) | The transactions of the chain: new ones, confirmations, and the ones replaced or dropped |
| [`label`](#hw-label) | The name of an entry |
| [`tag`](#hw-tag), [`untag`](#hw-untag), [`renametag`](#hw-renametag), [`deletetag`](#hw-deletetag) | The tags of an entry, and the account a tag files a payment under |
| [`newcontact`](#hw-newcontact), [`deletecontact`](#hw-deletecontact) | The contact that an entry names. The tags of a contact can also file a payment under an account |
| [`importwallet`](#hw-importwallet) | Labels, tags and contacts of a file |
| [`deletewallet`](#hw-deletewallet), [`deleteportfolio`](#hw-deleteportfolio), [`restoredatabase`](#hw-restoredatabase) | The portfolios the journal covers |
| [`setdefault`](#hw-setdefault) `--show-network`, `--hide-network` | The networks the journal covers |

No other command changes the journal. A payment you prepare, sign or send is
not in it yet. It enters the journal with the sync that finds it on the chain.

**The next command writes a journal that is missing.** Before each command,
`hw` looks for the file, and writes it when it is not there. So `hw` writes
the journal at your first command after the setup, after you set
`journal = true` in the configuration file by hand, and after the file was
deleted. Any command does this, even a listing. `hw` does not wait for a
sync, because ledger refuses a journal that includes a missing file.

**The next command also writes the journal again when its accounts changed.**
The table `journal-accounts` of the configuration file
[gives tags an account](#hw-exportjournal), and you write it by hand. The
journal names these tags and their accounts in its first comment. Before each
command, `hw` compares that comment with the configuration file, and writes
the journal again when they differ. So a line you add to the table shows in
ledger after your next command, whatever the command.

`hw` does not write a journal that did not change, so the file keeps the time
of its last real change. `hw` puts the new file in place of the old one in
one step, so ledger never reads half a journal. `--journal` sets the key
`journal` of the [configuration file](#configuration).

`hw setdefault --no-journal` turns the journal off, and `hw` removes the
file. If the file stayed, your own journal would still include it, and it
would fall further behind your wallet with each sync.

> **Good to know**
>
> - **The file belongs to `hw`, not to you.** `hw` replaces it without
>   asking. It never does that to a file you name. Do not edit the file, and
>   keep your own entries in your own journal.
> - **`hw` keeps no journal unless you ask.** The file tells your whole
>   history in plain text, so `hw` makes it readable by you alone.
> - **A journal that cannot be written does not stop a command.** The
>   command is still done, and `hw` warns on stderr that the journal is out
>   of date. `hw setdefault --journal` writes it again.
> - **Most commands leave a journal that is in place alone.** Any command
>   writes a journal that is missing, or one whose accounts differ from the
>   configuration file. Otherwise, only the commands of the table write the
>   journal again. A listing then reads no transaction for it.
> - **A journal turned off by hand stays where it is.** Only
>   `setdefault --no-journal` removes the file. Delete it yourself after you
>   set `journal = false` in the configuration file.
> - **The dates follow the time zone of the computer.** When the time zone
>   changes, the journal keeps the dates of the old zone. `hw` writes the
>   dates of the new zone at the next command that changes the journal.
> - **`hw` reads and writes the whole journal each time.** This takes longer
>   as the history grows. With many thousands of transactions, each command
>   of the table may take a second or so longer.
> - **The offline computer needs no journal.** It has no history to write.

### One command at a time

Only one `hw` command works on a database at a time. A second command on the
same database waits three seconds, then stops with the error "The database is
in use by another hw command".

### Scripts

`hw` runs in a script as it runs on a terminal. Give `--json` for an answer
that a program can read. The questions, the warnings and the status line go to
stderr, so stdout holds the answer alone. A listing or a help goes through no
pager when stdout is not a terminal.

```console
hw preparetx ADDRESS:0.001 --fee-rate median --yes --json
hw bumpfee TXID --fee-rate high --yes --json
printf '%s\n%s\n' "$MNEMONIC" "$PASSPHRASE" | hw signtx NOSIGHASH --json
```

**The exit code.** `hw` exits with 0 when the command did what it was asked,
and with 1 when it did not. These are failures:

- an error, such as a command line `hw` cannot read;
- a no to a question, or an input that ended before the answer;
- a signature that [`verifymsg`](#hw-verifymsg) finds not valid;
- a mnemonic that [`checkmnemonic`](#hw-checkmnemonic) finds does not match;
- a `sync` or a `recover` that left out a network because its server is down,
  or on another chain than at the last sync.

**Secrets come on the input.** No option takes a mnemonic, a passphrase or a
private key. An option would show them to the other users of the computer in
the list of processes, and keep them in the history of the shell. A script
writes them to the standard input of `hw` instead, one on each line. An empty
line means no passphrase. `printf` is built into the shell, so it shows them
in no list of processes either.

| Command | What it reads, one on each line |
|---|---|
| `newportfolio` | The passphrase, twice when it is not empty |
| `newportfolio --existing` | The mnemonic twice, then the passphrase, twice when it is not empty |
| `recover`, `checkmnemonic`, `signtx`, `send`, `sendall`, `signmsg` | The mnemonic, then the passphrase |
| `sweep`, `signsweep`, `preparesweep --key-file -` | The private keys, one or more on each line, to the end of the input |

The shares of a backup take the place of the mnemonic: one share on each
line, as many as the backup needs. `hw` reads them once, even for
`newportfolio --existing`. When `hw` cannot read a mnemonic, for example
because a word is mistyped, it asks for the mnemonic again. It also asks again
when a mnemonic or a passphrase does not match the portfolio of `signtx`,
`send`, `sendall` or `signmsg`. In a script the input has ended by then, so
the command stops with an error.

**The private keys of a sweep are the last thing a command reads.** A pipe
has an end, so `sweep`, `signsweep` and `preparesweep --key-file -` read the
keys from every line up to it, as they read a key file. The same file works
both ways:

```console
hw signsweep NOSIGHASH --key-file keys.txt
hw signsweep NOSIGHASH < keys.txt
```

No answer to a question can follow the keys on the input. `preparesweep` and
`sweep` ask about the dust coins after they have read them, so give
`--spend-dust` or `--leave-dust`. `sweep` then asks whether to sign and send,
so give it `--yes` too:

```console
hw sweep --sweep-to MYADDRESS --fee-rate median --leave-dust --yes --json < keys.txt
```

**A master private key takes the place of the mnemonic and the passphrase.**
It is one line: the key as [BIP32] writes it, which starts with `xprv`. `hw`
asks for no passphrase after it, because the passphrase is already part of
the key. `hw` reads it once, even for `newportfolio --existing`. A signer
that must sign without anyone typing can keep this key in a file:

```console
hw signtx NOSIGHASH < master-key.txt        # the file holds the key, on one line
```

- The master private key signs for one wallet, as the mnemonic and the
  passphrase of that wallet do. It does not give the mnemonic back, so it is
  not a backup of it. It gives nothing of the wallets that the same mnemonic
  makes with another passphrase.
- `hw` stores no key, so the file is yours to keep. Make it readable by you
  alone, on an encrypted disk.
- [`newportfolio --show-master-key`](#hw-newportfolio) prints the key of a
  new mnemonic, when it makes the mnemonic. For a mnemonic you have already,
  `hw` has no command that shows the key. Take it from a tool that derives it
  from the mnemonic, on the offline computer.
- A key that starts with `tprv` is the same master key, as a tool set to a
  test network writes it. `hw` reads it for every network.
- `hw` refuses the private key of an account, which is derived from the
  master key and is not one.

A script or a file that holds a mnemonic or a master private key holds the
key to the funds. Keep it on the offline computer if you can, as
[Keep your funds safe](#keep-your-funds-safe) says.

**An error shows no private key.** An error quotes what was typed, and the
log of a script keeps it. When a private key is given by mistake where an
address, a hash or another word goes, `hw` writes `(a private key, not
shown)` in its place, on the screen and in `--json`. `hw` knows a key in WIF
and a minikey by their form, even with a typo. It cannot tell a key in 64
hexadecimal digits from the id of a transaction, so it shows that one.

**Options answer the questions.** Each question below has an option that
answers it in advance. Give every option that applies. A question that no
option answers reads the next line of the input. That may be the line of the
mnemonic, and the command then stops with an error.

| Question | Commands | Option |
|---|---|---|
| Which fee rate | `send`, `sendall`, `preparetx`, `preparesendall`, `bumpfee`, `sweep`, `preparesweep` | `--fee-rate` |
| A fee rate above 100 sat/vB | The same | `--yes` |
| The wallet is behind the chain | `send`, `sendall`, `preparetx`, `preparesendall`, `bumpfee` | `--yes` |
| Spend coins that do not all have the same tags | `send`, `sendall`, `preparetx`, `preparesendall`, `bumpfee` | `--yes` |
| Spend the coins worth less than their fee | `sendall`, `preparesendall`, `sweep`, `preparesweep` | `--spend-dust` or `--leave-dust` |
| Sign the sweep and send it | `sweep` | `--yes` |
| Add coins, replace the payments that spend the change, reduce what a lone recipient receives, or have several pay in equal shares | `bumpfee` | `--yes` |
| Send the transaction | `broadcast` | `--yes` |
| Replace the database with a copy | `restoredatabase` | `--yes` |
| Delete from the database | `deleteportfolio`, `deletewallet`, `deletetx`, `deletetag`, `deletecontact` | `--yes` |
| Replace the `--input` file | `signtx`, `signsweep` | `--output`, to write the signed file elsewhere |

**The setup reads its answers on the input.** When the input is not a
terminal, the [setup](#the-setup) reads the answers to its four questions
from it, one on each line. So a script that installs `hw` writes four lines.
An empty line takes the default of its question:

```console
printf '%s\n' 'btc bch' btc bip84 yes | hw   # the networks, the default network, the default type, a journal for ledger
printf '\n\n\n\n' | hw                        # btc and bch, btc, bip44, no journal
```

A script can also write `~/.hw/config.toml` itself, with the keys it wants.
`hw` is set up as soon as that file is there.

**Two questions have no option, on purpose.** In a script the input has ended,
so either one stops the command with an error.

- After a thousand used addresses in a row, `sync` and `recover` ask whether to
  keep looking. `recover` asks the same after every 20 portfolios it finds
  paid. A server that lies can report used addresses without end, so `--yes`
  does not answer this question. Run the command on a terminal then, to see
  what the server reports.
- `bumpfee` asks which recipient pays alone when equal shares would leave one
  below the dust limit. `--yes` picks no recipient. Run it on a terminal then,
  to choose who pays.

## Commands

The commands marked `!` ask for a mnemonic or private keys. Run them on an
offline computer if you can. [`recover`](#hw-recover), [`send`](#hw-send),
[`sendall`](#hw-sendall), [`sweep`](#hw-sweep) and
[`preparesweep --key-file`](#hw-preparesweep) ask
for a mnemonic or private keys too. They need the server, so they cannot run
offline, and they are not marked.

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
| [`showdefault`](#hw-showdefault) | Show the defaults of the commands, and the active portfolio |
| [`deleteportfolio`](#hw-deleteportfolio) | Delete a portfolio that was never used |
| [`deletewallet`](#hw-deletewallet) | Delete a wallet with all of its portfolios |
| [`sync`](#hw-sync) | Update the portfolios from the network |
| **[Addresses](#addresses)** | |
| [`receive`](#hw-receive) | Get a new address to receive a payment |
| [`show`](#hw-show) | Show an address, a transaction or a coin, or a file of a transaction, of a wallet or of a database |
| [`addresses`](#hw-addresses) | List the receiving addresses of a portfolio, or its change addresses |
| [`balances`](#hw-balances) | List the addresses of a portfolio that hold coins |
| [`label`](#hw-label) | Label an address, a transaction or a contact, or describe a portfolio or a wallet |
| **[Coins](#coins)** | |
| [`coins`](#hw-coins) | List the coins of a portfolio |
| [`lockcoin`](#hw-lockcoin) | Keep a coin from being spent |
| [`unlockcoin`](#hw-unlockcoin) | Unlock a coin locked with `lockcoin` |
| **[Payments](#payments)** | |
| [`fees`](#hw-fees) | Show the fee rates of recent blocks |
| [`send`](#hw-send) | Prepare, sign and send a payment in one command |
| [`sendall`](#hw-sendall) | Prepare, sign and send all the coins in one command |
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
| **[Tags](#tags)** | |
| [`tag`](#hw-tag) | Put tags on an address, a transaction or a contact, or colour or lock a tag |
| [`untag`](#hw-untag) | Take tags off an address, a transaction or a contact |
| [`tags`](#hw-tags) | List the tags, with their colours and what has them |
| [`renametag`](#hw-renametag) | Give a tag a new name, or merge it into another tag |
| [`deletetag`](#hw-deletetag) | Delete a tag from every address, transaction and contact |
| **[Address book](#address-book)** | |
| [`newcontact`](#hw-newcontact) | Add an address of somebody else to the address book |
| [`contacts`](#hw-contacts) | List the contacts of the address book |
| [`deletecontact`](#hw-deletecontact) | Delete a contact from the address book |
| **[Sweeps](#sweeps)** | |
| [`sweep`](#hw-sweep) | Prepare, sign and send a sweep in one command |
| [`preparesweep`](#hw-preparesweep) | Prepare a transaction that moves coins from outside the wallet |
| [`signsweep`](#hw-signsweep) `!` | Sign a sweep with the private keys of its coins |
| **[Messages](#messages)** | |
| [`signmsg`](#hw-signmsg) `!` | Sign a message with the key of an address |
| [`verifymsg`](#hw-verifymsg) | Check the signature of a message |
| **[The wallet](#the-wallet)** | |
| [`backup`](#hw-backup) | Copy the wallet database to a backup file |
| [`showbackups`](#hw-showbackups) | Show the backups `hw` keeps, and how old the newest are |
| [`restoredatabase`](#hw-restoredatabase) | Put a copy of the database back in place of the database |
| [`exportsheet`](#hw-exportsheet) | Write every listing of the wallet to a spreadsheet |
| [`exportjournal`](#hw-exportjournal) | Write the transactions of the wallet to a journal for ledger |
| [`report`](#hw-report) | Write a report of the wallet as a PDF file |

### Wallets and portfolios

#### `hw newportfolio`

Makes a new mnemonic and its wallet, or adds a portfolio to the wallet of a
mnemonic you have already. Either way, the result is a whole portfolio that is
ready to receive, so one command does both. `hw` never writes the mnemonic to
disk. It stores only the public keys of the portfolio.

```console
hw newportfolio                             # a new mnemonic, its wallet and portfolio0
hw newportfolio --entropy 32 --dice         # 24 words, with rolls of a die mixed in
hw newportfolio --shares 3 --threshold 2    # a backup in shares instead of a mnemonic
hw newportfolio --shares 1
hw newportfolio --one-line                  # the words on one line, to copy them
hw newportfolio --show-master-key           # the master private key too, for a script
hw newportfolio --existing                  # one more portfolio in a wallet you have
hw newportfolio --existing --match          # the same, and never a new wallet
```

| Option | What it does |
|---|---|
| `--existing` | Asks for a mnemonic you have already, and adds a portfolio to its wallet, instead of making a new mnemonic. |
| `--match` | With `--existing`, refuses a mnemonic that is of no wallet of the database, instead of creating a new wallet. |
| `--entropy BYTES` | Sets how many bytes of entropy the mnemonic has: 16, 20, 24, 28 or 32, for 12, 15, 18, 21 or 24 words. The default is 16. A backup in shares takes 16 or 32. |
| `--dice` | Mixes rolls of a six-sided die, which you type, into the entropy of the operating system. |
| `--shares N` | Prints the N shares of a backup instead of a mnemonic, in the [SLIP39] format. |
| `--threshold T` | Sets how many of the shares are needed to rebuild the seed. |
| `--one-line` | Prints the words on one line, without numbers, to copy them. Each share of a backup gets a line of its own. |
| `--show-master-key` | Also prints the master private key of the new wallet, which a script gives in place of the mnemonic and the passphrase. |

**A new mnemonic.** Without `--existing`, `newportfolio` makes a mnemonic from
the entropy of the operating system. It first asks for a passphrase, and asks
twice to catch a typo. Leave it empty if you want none. It then shows the
mnemonic, once, and creates the wallet with its `portfolio0`. A new mnemonic
is a new wallet, which takes the lowest free index. In a new database, that is
`wallet0`.

```console
$ hw newportfolio
Mnemonic of 12 words, from /dev/random
   1. ...        2. ...        3. ...        4. ...
   5. ...        6. ...        7. ...        8. ...
   9. ...       10. ...       11. ...       12. ...

Created portfolio0 of wallet0
  Fingerprint: bc560f95
  Accounts   : 3, on btc (bip44 and bip84) and bch (bip44)
portfolio0 of wallet0 is the active portfolio: the commands work in it.
```

Write the mnemonic down and keep copies in safe places, away from computers and
phones. `hw` stores the public keys of the portfolio only, never the mnemonic.
The fingerprint is a short code that identifies the mnemonic from then on, and
reveals nothing about it. Transaction files and output descriptors write these
four bytes before a key of the mnemonic.

**Words to copy.** `newportfolio` numbers the words and prints four on a line,
because writing them down is the safe way to keep them. With `--one-line`, it
prints them on one line instead, with a space between them and no numbers, so
that you can copy them into another program. The line has no indent. When you
select the whole line, you copy the words and nothing else. A share of a
backup gets a line of its own, under its number.

```console
$ hw newportfolio --one-line
Mnemonic of 12 words, from /dev/random
... ... ... ... ... ... ... ... ... ... ... ...

Created portfolio0 of wallet0
...
```

A line of 24 words is wider than most terminals. The terminal wraps it on the
screen, and still copies it as one line.

**The master private key.** With `--show-master-key`, `newportfolio` also
prints the master private key of the new wallet, under the words. It is the
key that [BIP32] derives from the mnemonic and the passphrase, and it starts
with `xprv`. The commands that ask for the mnemonic take it in place of the
mnemonic and the passphrase, so a signer can sign from a file that holds it:
see [Scripts](#scripts). `hw` shows it here and nowhere else, because this is
where the mnemonic is made, and no other tool then has to see the mnemonic.

```console
$ hw newportfolio --show-master-key
Mnemonic of 12 words, from /dev/random
   1. ...        2. ...        3. ...        4. ...
   5. ...        6. ...        7. ...        8. ...
   9. ...       10. ...       11. ...       12. ...

Master private key
xprv...

Created portfolio0 of wallet0
...
```

The key is alone on its line, which has no indent. It has 111 characters, so
the terminal wraps it on the screen, and still copies it as one line. With
`--json`, the answer has it under `masterkey`. A backup in shares has a master
private key too, and `--shares` shows it the same way.

Whoever has this key can spend the funds of the wallet. It is not a backup:
it does not give the mnemonic back. Write the mnemonic down all the same.

**A mnemonic you have.** With `--existing`, `newportfolio` asks for the
mnemonic and its passphrase, each twice, or for the shares of a backup. It adds
a portfolio to the wallet of that mnemonic, at the lowest free index. That is
`portfolio1` when the wallet has `portfolio0`. It is `portfolio1` too when the
wallet has `portfolio0` and `portfolio2`. `hw` finds the wallet of the mnemonic
by the fingerprint of its master key. When the database has no wallet for the
mnemonic yet, `newportfolio` creates the wallet with its `portfolio0`.

**A mnemonic of no wallet here.** A mnemonic with another passphrase is
another wallet. So `hw` cannot tell a mnemonic that you bring to this database
from the mnemonic of a wallet it has, typed with a mistake in the passphrase.
Both give a master key that the database does not know, and `--existing`
creates a wallet for both. A passphrase typed twice with the same mistake, for
example with Caps Lock on, makes such a wallet. Funds sent to it can be spent
only with the passphrase as it was mistyped. For this reason, when
`--existing` creates a wallet in a database that has one already,
`newportfolio` says so, and gives the command that deletes the new wallet:

```console
$ hw newportfolio --existing
...
Created portfolio0 of wallet1
  Fingerprint: 4435aa1c
  Accounts   : 3, on btc (bip44 and bip84) and bch (bip44)
The active portfolio is still portfolio0 of wallet0.

This mnemonic and passphrase are those of no wallet here: a new wallet1 was
created. If you meant wallet0, the mnemonic or the passphrase was not the same:
delete wallet1 with hw deletewallet 1

To work in portfolio0 of wallet1, make it the active one:
  hw switch 0 --wallet 1
```

When you meant to create this wallet, there is nothing to do. With `--json`,
the answer has the sentence under `warning`.

**Only a wallet you have.** When you mean to add a portfolio to a wallet of
the database, give `--match` too. `newportfolio` then refuses a mnemonic and
passphrase that are those of no wallet there, and creates nothing:

```console
$ hw newportfolio --existing --match
...
Error: This mnemonic and passphrase are those of no wallet here: nothing was
created. Without --match, newportfolio --existing creates a new wallet
```

A script that adds a portfolio should give `--match`: a wrong passphrase then
stops it with an error. In a database without a wallet no mnemonic can match,
so `--match` is refused there before the mnemonic is asked.

**The active portfolio.** The first portfolio of a database becomes the
[active one](#the-active-portfolio), where the commands work. A portfolio
added later does not. `newportfolio` then says which portfolio is still
active, and prints the `switch` command that makes the new one active:

```console
$ hw newportfolio --existing
...
Created portfolio1 of wallet0
  Fingerprint: bc560f95
  Accounts   : 3, on btc (bip44 and bip84) and bch (bip44)
The active portfolio is still portfolio0 of wallet0.

To work in portfolio1 of wallet0, make it the active one:
  hw switch 1 --wallet 0
```

A new portfolio does not become active by itself. The commands you type next
keep working in the same portfolio as before, until you switch.

**A portfolio is created whole.** It gets an account for every network, the
test networks included, and for both types of address.
[What a portfolio holds](#what-a-portfolio-holds) lists these twelve accounts.
You never create an account afterwards. The options `--network` and `--type`
of a command, such as `receive`, choose the account.

The answer counts only the accounts on the
[networks `hw` shows](#shown-and-hidden-networks), and names the types of each
network. With `btc` and `bch` shown, that is
`3, on btc (bip44 and bip84) and bch (bip44)`. Bitcoin Cash has `bip44`
accounts only. The portfolio also has the accounts of the hidden test
networks. They appear once you show their network.

**Backups in shares.** With `--shares`, `newportfolio` prints a backup in
several shares instead of a mnemonic. Any `--threshold` of the shares rebuild
the seed, and fewer reveal nothing about it. With `--shares 3 --threshold 2`,
any two shares rebuild the seed. You can lose one share, and a person who
finds one share learns nothing. A share is a list of 20 words, or of 33 with
`--entropy 32`. `--shares 1` makes a single share, which is the whole backup.

The commands that ask for a mnemonic take the shares in its place. Type one
share, and `hw` asks for the next one until it has enough.

> **Good to know**
>
> - **The mnemonic is shown once.** `newportfolio` does not ask you to type
>   the new mnemonic again, so it cannot catch a mistake in what you wrote
>   down. Test what you wrote down with [`checkmnemonic`](#hw-checkmnemonic)
>   before you rely on it.
> - **A passphrase makes another wallet.** A mnemonic with a passphrase gives
>   a different master key from the mnemonic alone. The passphrase is part of
>   the secret, and you must keep it too.
> - **To keep the keys offline, run `newportfolio` on the offline computer.**
>   Then copy the wallet to the online computer with
>   [`exportwallet`](#hw-exportwallet) and [`importwallet`](#hw-importwallet).
> - **`--entropy`, `--dice`, `--shares`, `--threshold`, `--one-line` and
>   `--show-master-key` are for a new mnemonic.** They are refused with
>   `--existing`. `--match` is for a mnemonic you have, and is refused
>   without `--existing`.
> - **A copied mnemonic passes through the clipboard.** Other programs can read
>   it there, and a clipboard manager may keep it in its history. Prefer
>   writing the words down to copying them.
> - **A backup in shares is not a mnemonic.** A wallet that reads [SLIP39]
>   shares restores it. One that reads only BIP39 mnemonics cannot.
> - **The passphrase of a backup in shares** has printable ASCII characters
>   only.
> - **A passphrase with accents can give two wallets.** `hw` normalizes a
>   passphrase as BIP39 asks. `hw` 0.9 did not, so it derived other keys from
>   a passphrase with accents. `recover` creates the wallet of the passphrase
>   as typed when only that wallet has funds. When the database has that
>   wallet, and not the wallet of the normalized passphrase, `--existing` adds
>   the portfolio to it and says so. When the database has neither,
>   `--existing` creates the wallet of the normalized passphrase. It then
>   warns that a wallet made by `hw` 0.9 has other keys, and points to
>   `recover`. See [A passphrase with accents](#a-passphrase-with-accents).

#### `hw recover`

Finds the portfolios of a mnemonic, or of a backup in shares, that were paid.
It creates them and syncs them. `recover` needs the mnemonic and the server at
the same time, so you type the mnemonic on an online computer. `hw` does not
store it. The first point of Good to know, below, says how to keep the
mnemonic offline instead.

```console
hw recover
hw recover --portfolio-gap 20
```

`recover` checks the portfolios in order from `portfolio0`. For each one, it
asks the server about the receiving addresses of every account: on every
network, the test networks included, and of both types of address. When any
account of a portfolio was ever paid, it creates the whole portfolio, with
every account, and syncs it. It finds whole portfolios rather than single
accounts, so the wallet it brings back is ready on every network, like a new
wallet.

The search covers every network, whatever `default-network` says. A mnemonic
often has funds on both chains: every mnemonic that held Bitcoin before the
fork of 2017 does. A search of one network would miss the other.

The search covers the [hidden networks](#shown-and-hidden-networks) too.
`recover` still finds, creates and syncs a portfolio that was paid on a hidden
network only. The answer leaves the hidden networks out. It does not name them
as searched. It reports no sync and no funds on them. It does not say when the
server of one is down or missing.

`recover` adds the portfolios to the wallet of the mnemonic. It finds that
wallet by the fingerprint of its master key, or creates it when the database
does not have it. A new wallet gets its `portfolio0` whether that portfolio
was paid or not, since a wallet always has `portfolio0`. `recover` skips the
portfolios the wallet has already. A fingerprint is four bytes, so two
mnemonics may share one. When the database has a wallet with the fingerprint
of the mnemonic but with other keys, `recover` refuses, and nothing changes.

In a database without any portfolio, the first portfolio `recover` creates
becomes the [active one](#the-active-portfolio). That is `portfolio0` of the
new wallet, even when only a later portfolio was paid. `recover` then prints,
as the next step, the `switch` command to the first portfolio it found paid.
In a database that has an active portfolio, that portfolio stays active.
`recover` says so, and prints the same next step.

The search stops after 10 portfolios in a row where nothing was found.
`--portfolio-gap` sets another number. [BIP44] says to stop at the first
unused account. `hw` looks further, because you may have created a portfolio
on an offline computer and never paid it, and then paid a later one. After a
thousand used addresses in a row, `recover` asks whether to keep looking, as
[`sync`](#hw-sync) does. On a terminal, a status line says which portfolio
`recover` is searching. Before `recover` asks for the mnemonic, the line names
the network whose server it is checking.

After every 20 portfolios the server reports paid, `recover` asks whether to
keep looking. A server that lies can keep reporting paid portfolios without
end. If you answer no, the search stops and nothing is added.

`recover` also looks at the paths where other wallets keep their accounts, and
reports funds there that `hw` cannot spend:

- The `bip49` accounts of wrapped SegWit on Bitcoin.
- The `bip86` accounts of Taproot on Bitcoin.
- The `bip44` accounts of Bitcoin on Bitcoin Cash, where a wallet that held
  Bitcoin at the fork of 2017 has its Bitcoin Cash.

`recover` adds no such account. To use these funds, send them to an address
of `hw` with a wallet that holds such accounts.

> **Good to know**
>
> - **`recover` takes the mnemonic on an online computer.** It needs the server
>   at the same time, so it cannot run on an offline one. A computer that was
>   tampered with can steal a mnemonic typed on it. To keep the mnemonic
>   offline, create the portfolios on the offline computer with
>   [`newportfolio --existing`](#hw-newportfolio) instead. Copy them to the
>   online computer with [`exportwallet`](#hw-exportwallet) and
>   [`importwallet`](#hw-importwallet), and run [`sync`](#hw-sync) there.
> - **An empty answer often means a mistyped passphrase.** A mnemonic with
>   another passphrase, or with none, is another wallet, which was never paid.
>   `recover` then says that no portfolio was paid, and to check the
>   passphrase. Nothing is created, so run `recover` again.
> - **A network the server does not serve is named, and is no error.** A
>   server seldom serves every network. The public one has no regtest chain,
>   for example. `recover` names those networks in one line and searches the
>   others.
> - **A network whose server is down is an error.** `recover` says so,
>   searches the other networks, then exits with an error. It cannot find a
>   portfolio that was paid on that network only. Run `recover` again once the
>   server answers. The second run skips the portfolios found the first time.
> - **A hidden network whose server is down is no error.** `recover` says
>   nothing about it. It cannot find a portfolio that was paid on that network
>   only. Show the network and run `recover` again if you expect funds there.
> - **A portfolio found on a hidden network looks empty.** `recover` creates
>   it and names it, and `portfolios` shows it without a line for that
>   network. [`setdefault --show-network`](#hw-setdefault) shows what it
>   holds there.
> - **A server `hw` cannot reach counts as down for every network.** When the
>   connection fails or times out, or the TLS handshake fails, `recover`
>   checks no other network on that server. The other networks would fail the
>   same way, and each timeout takes 30 seconds. `recover` names them as not
>   searched, with that error. When no network is left, `recover` stops before
>   it asks for the mnemonic. For example, a `host` that starts with
>   `https://` for a server that speaks plain HTTP fails this way.
> - **A server can also fail during the search.** `recover` then stops
>   searching that network, syncs nothing there, and goes on with the others.
>   It keeps the portfolios it found, names the network with the error as not
>   searched, and exits with an error. The same holds when the server fails
>   while `recover` syncs the portfolios found, or looks for the funds at the
>   paths of other wallets. Only the networks searched in full are named as
>   searched. Any other error, such as a no to one of its questions, stops
>   `recover`, and nothing is added.
> - **A passphrase with accents is tried both ways.** `hw` normalizes a
>   passphrase as BIP39 asks, and earlier versions did not. When `recover`
>   finds nothing with the normalized passphrase, it also tries the passphrase
>   as typed, and says so when that finds portfolios. See
>   [A passphrase with accents](#a-passphrase-with-accents).

#### `hw checkmnemonic`

Checks that a mnemonic and passphrase give the public key of every account of
the active portfolio. Use it to test a mnemonic you wrote down, or the shares
of a backup, before you rely on it. `checkmnemonic` signs nothing, changes
nothing in the wallet and does not store what you type.

```console
hw checkmnemonic                            # against the active portfolio
hw switch 0 --wallet 1                      # to check the mnemonic of wallet1
hw checkmnemonic
```

To check another portfolio, switch to it first.

It answers that the mnemonic and passphrase matched the portfolio, or that
they did not. When they did not, `hw` exits with an error, so that a script
can tell. When they are those of another wallet of the database, the answer
names that wallet, so you need not switch to each wallet in turn:

```console
$ hw checkmnemonic
Mnemonic, or a share of a backup:
Passphrase, or nothing if the seed has none:
The mnemonic and passphrase did not match portfolio0 of wallet0: they match wallet1
```

#### `hw exportwallet`

`hw exportwallet FILE` writes every portfolio of every wallet to a file, with
its labels and its [tags](#tags-on-addresses-and-transactions), then the
[address book](#the-address-book).
[`importwallet`](#hw-importwallet) adds the file to another database. The file
holds public keys and no private key. Its public keys still reveal every
address and balance, so keep the file private. `--wallet` and `--portfolio`
write only a part of the database:

```console
hw exportwallet wallet.jsonl                # every wallet, on the offline computer
hw exportwallet family.jsonl --wallet 1     # every portfolio of wallet1
hw exportwallet savings.jsonl --portfolio 1 # portfolio1 of the wallet of the active portfolio
hw exportwallet savings.jsonl --wallet 1 --portfolio 2    # portfolio2 of wallet1
```

| Options | What the file holds |
|---|---|
| none | Every portfolio of every wallet, and the address book |
| `--wallet M` | Every portfolio of `walletM`, without the address book |
| `--portfolio N` | `portfolioN` of the wallet of the [active portfolio](#the-active-portfolio), without the address book |
| `--wallet M --portfolio N` | `portfolioN` of `walletM`, without the address book |

`--portfolio` alone names a portfolio of the wallet of the active portfolio,
as it does for [`switch`](#hw-switch). So with one mnemonic you never name a
wallet. Without `--portfolio`, the active portfolio plays no part: the same
command writes the same file whichever portfolio is active.

**Why every wallet by default.** One file then holds the whole database. It
moves every wallet to another computer in one step. It also saves every label,
in case you delete a wallet or lose the computer. If each file held one
wallet, you would need a `switch` and an export for each mnemonic, and nothing
would tell you about a wallet you forgot. `--wallet` is for a file that must
not show the other wallets.

**Several wallets in one file.** [BIP 329] allows this. Each record has an
`origin`, which names its wallet by the fingerprint of the mnemonic. The BIP
says to use it to tell apart the labels of different wallets in the same
export. `hw` writes it on every record. The file does not carry the index of a
wallet, so it does not say which one is `wallet0`.
[`importwallet`](#hw-importwallet) finds each wallet by its fingerprint,
whatever index the wallet has in the database that reads the file.

The file does not carry the active portfolio either. Each database keeps its
own, and [`importwallet`](#hw-importwallet) says which one it is after the
import.

The file is in the format of [BIP 329], which other wallets read and write. It
holds no private key. For each account, it holds:

- The public key of the account, with its path, its network, its type and the
  fingerprint of the mnemonic.
- The labels of its receiving addresses and of its transactions.
- The tags of its addresses and of its transactions. Each tag comes with its
  colour and with whether it locks its coins.
- The coins locked with `lockcoin`.

The file also holds the descriptions of the wallets and of their portfolios.

**Tags in the file.** [BIP 329] defines no tags. `hw` writes them under three
keys of its own, on the record of the address or of the transaction. The key
`tags` lists the tags of that address or transaction. `tagcolors` gives the
colour of each tag that has one. `taglocks` lists the tags that
[lock their coins](#hw-tag):

```json
{"type":"addr","ref":"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj","label":"Wages","origin":"pkh([bc560f95/44'/0'/0'])","network":"btc","keypath":"/0/0","tags":["cold","kyc"],"tagcolors":{"kyc":"red"},"taglocks":["cold"]}
```

A tag that nothing has is in a file of the whole database too. No record of
an address or of a transaction names it, so the first key record of the file
does, under the same three keys. Here, `hw tag savings --lock-coins` made the
tag `savings`, and no address has it yet:

```json
{"type":"xpub","ref":"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah","origin":"pkh([bc560f95/44'/0'/0'])","network":"btc","purpose":44,"tags":["savings"],"taglocks":["savings"]}
```

So a wallet made again from its file, or from a copy of its labels, has
every tag that [`tags`](#hw-tags) listed, and has the lock before the first
address takes the tag.

**The address book.** A file of the whole database ends with a record for
each contact. It is an `addr` record without an origin, since a contact
belongs to no wallet. It has the label of the contact, its tags, its network,
and two more keys of `hw`, `contact` and `added`:

```json
{"type":"addr","ref":"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU","label":"Landlord","network":"btc","contact":true,"added":"2026-11-14T09:05:33.482913604Z"}
```

The BIP lets an `addr` record label any address, so another wallet reads it as
the label of that address. The key `contact` tells `importwallet` to put the
address in the address book. Without it, `hw` could not tell the record from
the label of an address of the wallet that it has not derived yet. The key
`added` is the date you added the contact, in UTC, so that a wallet made
again from the file shows each contact with the date it had. A file
narrowed with `--wallet` or `--portfolio` leaves the address book out, since
such a file is made to show less than the whole database.

Another wallet ignores the keys that `hw` adds, and still reads the label.
This is why `hw` does not write the tags into the label itself, after a sign:
that would change the label the other wallet shows. `hw` writes the colour on
each record that has the tag, and not once on a line of its own. So every line
of the file stays a record that the BIP defines, with the keys of `hw` added.
An address or a transaction that has tags and no label gets a record too,
without a label. A change address gets a record when it has tags.

`hw` also writes this file by itself to `~/.hw/auto-backups`, after each
command that changes the labels. It keeps the recent copies and fewer of the
older ones ([Automatic backups](#automatic-backups)). [`backup`](#hw-backup)
writes the file to `~/.hw/user-backups`, beside its copy of the database.

The file carries every account of each portfolio. So the computer that has the
mnemonic derives the accounts once. The computer that watches the chain then
needs nothing more from it, whichever network or type of address you use
later.

> **Good to know**
>
> - **The file shows every address of every wallet.** It holds no private
>   key, but its public keys reveal every address and balance of the
>   portfolios. `hw` makes it readable by you alone.
> - **A file of several wallets links them.** Whoever reads it learns that one
>   person holds them all. Before you hand a file to someone else, write the
>   one wallet they need with `--wallet`.
> - **A tag that nothing has is written in a file of the whole database
>   only.** Such a file holds every tag that [`tags`](#hw-tags) lists, also
>   one shown as `not used`. A file of one wallet or of one portfolio holds
>   the tags that its addresses and its transactions have, and no other.
> - **The tags of a pending transaction are written once it is signed,** as
>   its label is.

#### `hw importwallet`

`hw importwallet FILE` adds the portfolios, the labels and the
[tags](#tags-on-addresses-and-transactions) of a file to the database. The file
is the one [`exportwallet`](#hw-exportwallet) writes. It holds public keys
only, so `importwallet` asks for no mnemonic. Without the mnemonic, it cannot
prove that the keys are yours, so import only a file you trust. The first
point of Good to know, below, explains.

```console
hw importwallet wallet.jsonl                # on the online computer
hw importwallet wallet.jsonl --wallet bc560f95               # one wallet of the file
hw importwallet wallet.jsonl --wallet bc560f95 --portfolio 1 # one portfolio of that wallet
hw importwallet wallet.jsonl --portfolio 1  # one portfolio, in a file of one wallet
hw importwallet labels.jsonl                # the labels of another wallet
```

[`show FILE`](#hw-show) shows what `importwallet` would do with a file, and
changes nothing. Run it first to see the portfolios and labels the file
holds. It ends with the lines that `importwallet` would end its answer with,
such as `Labels set` below.

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
above. In a database that has an active portfolio, that portfolio stays
active. `importwallet` then says so, and prints, as the next step, the
`switch` command to the first portfolio it created.

**Wallets and portfolios.** `importwallet` finds each wallet by the fingerprint
the file gives, and each portfolio by its index. When the database does not
have a portfolio of the file, `importwallet` creates it whole, with the twelve
accounts of the file. The database can then watch the portfolio and prepare
its payments without the mnemonic. This is how an online computer works with
an offline signer.

**Several wallets.** A file holds every wallet of the database that wrote it,
unless `exportwallet --wallet` wrote one alone. `importwallet` handles each
wallet of the file separately. When the database does not have a wallet,
`importwallet` creates it at the lowest free index, in the order of the file.
So an empty database gets the wallets in the order they had.

The index of a wallet is not in the file. A wallet may be `wallet0` on the
computer that wrote the file and `wallet1` on this one. That happens when you
run [`recover`](#hw-recover) or `newportfolio --existing` with the mnemonics
in another order. `importwallet` still finds the wallet by its fingerprint,
and gives it its labels and its description. Here the file names the wallet of
`bc560f95` first, which was `wallet0` where the file was written:

```console
$ hw importwallet wallet.jsonl
wallet1  bc560f95  already in the database
  portfolio0  nothing to add

wallet0  4435aa1c  already in the database
  portfolio0  nothing to add

Labels set: 2 wallets, 2 addresses
```

**A portfolio the database has already** stays as it is, and the answer says
`nothing to add`. `importwallet` never makes a second portfolio with the same
index. Such a portfolio would hold the same keys, since the mnemonic and the
index decide them. A portfolio has every account from the start, so
`importwallet` adds no account to one either. The keys of the file must be the
ones the portfolio holds. Otherwise `importwallet` refuses and changes nothing,
since the file and the database then come from different mnemonics.

`importwallet` also refuses five other files, and changes nothing:

- **A portfolio without all of its accounts.** A portfolio the database does
  not have is created whole, or not at all. A file that gives only some of its
  twelve accounts is refused, with the advice to write the file again on the
  computer that has the whole portfolio.
- **A damaged file.** When a line of a refused file was not read, the error
  starts with that line and the reason. A file of twelve lines with one
  damaged key gives eleven accounts, and the error says which line to look
  at: `Line 1 of the file was not read. The key does not read: its checksum
  is wrong, as when a character of the key was changed, added or lost`. A
  file that was cut has a last line that is no record: `Line 7 of the file
  was not read. It is not a record: the line may be cut or damaged`. When
  several lines were not read, the error gives the first, and counts the
  others. Take a good copy of the file.
- **A file from which no key is imported.** When `importwallet` cannot read
  any key of the file, it brings no wallet, and the error says so: `None of
  the keys of the file was imported`, then the first line that was not read,
  with the reason. This protects a script that restores a wallet: such a file
  is an error, not an import that did nothing. A key record of another
  wallet, which names no network, is not concerned, as the next paragraph
  explains.
- **A key the database holds, at another index.** The text of a key names its
  index, and a file can write any index there. The same key at another index
  would give a second portfolio the addresses of the first. `importwallet`
  compares the keys themselves, and refuses a key that another account of the
  network holds.
- **A key that a wallet of another fingerprint holds.** A key comes from one
  mnemonic, so two wallets with different fingerprints cannot both hold it:
  one of the two fingerprints is wrong. The error names both, as in
  `portfolio0 of wallet0 already has this key on btc, and the fingerprint of
  wallet0 is deadbeef, not bc560f95`. `importwallet` gives a wallet the
  fingerprint that its file names. So this happens after you import a file
  that names a wrong fingerprint. Delete that wallet with
  [`deletewallet`](#hw-deletewallet), then import the right file.
- **Two keys for one path.** The test networks share the coin type `1'`, so
  the bip44 accounts of a portfolio on the six of them have one path,
  `m/44'/1'/0'`, and one key. So do the bip84 accounts of the three test
  networks of Bitcoin. A file that gives them different keys is refused, and
  the error names the path and the networks. With different keys, `receive`
  would give different addresses on networks that must give the same ones.

**A key alone is not imported.** `importwallet` imports whole wallets. An
extended public key or an output descriptor from another wallet is one account.
It does not say which portfolio it belongs to, and `hw` keeps no account
outside a portfolio. A [BIP 329] file of another wallet may hold such a key as
a record. `importwallet` leaves the record aside and says why, and still sets
the labels of the file. A file that holds a key or a descriptor alone, and no
record, is refused.

**One wallet or one portfolio of the file.** With `--wallet` and
`--portfolio`, `importwallet` reads only a part of the file: the keys and the
labels of that part.

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
wallet, and nothing would tell you. The fingerprint is the same on every
computer.
[`show FILE`](#hw-show) lists the wallets of a file with their fingerprints.

**In a file of several wallets, `--portfolio` needs `--wallet`.** The index
alone would name a portfolio of each wallet, and those have nothing in common
but their index. `importwallet` refuses, and gives the fingerprints to choose
from:

```console
$ hw importwallet wallet.jsonl --portfolio 1
Error: The file holds the wallets bc560f95 and 4435aa1c, and --portfolio alone
does not say which of them portfolio1 is of. Name the wallet by its fingerprint
too, as in --wallet bc560f95 --portfolio 1. Nothing was changed
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
a file that holds its `portfolio0`. In a database that does not have the
wallet yet, `--portfolio 1` is refused, and so is a file that
`exportwallet --portfolio 1` wrote. Import the whole wallet instead.

When the file has nothing of the wallet or of the portfolio you name,
`importwallet` refuses, and says what the file has: `The file has no wallet
12345678: it has the wallets bc560f95 and 4435aa1c`, or `The file has no
portfolio2: it has portfolio0 and portfolio1`. A value that matches nothing is
more likely a typo than a wish to import nothing. A [BIP 329] file of another
wallet holds no key. There, a wallet or a portfolio is in the file when the
origin of a record names it.

**Labels and locks.** `importwallet` sets the labels and locks of the file on
the addresses, transactions and coins the wallet has. It can also label two
things that the wallet does not have yet:

- **An address the wallet has not derived yet.** `importwallet` derives it
  from the path the file gives, up to a thousand addresses past the last used
  address of its chain.
- **A transaction that `sync` has not fetched yet.** `importwallet` labels it
  at once when the origin of its record says which portfolio it belongs to. A
  record without an origin names a network only, and a network does not
  identify a portfolio. Such a record labels the transaction in the portfolios
  that have it already, on that network.

`importwallet` counts the records it cannot apply. Run `sync`, then
`importwallet` again. It leaves aside the labels of inputs, coins and change
addresses, since `hw` keeps none. A [BIP 329] file of another wallet sets its
labels the same way.

**Tags.** `importwallet` puts the tags of the file on their addresses at
once, also on an address that the wallet has not generated yet. Such a tag
waits for its address. When a `sync` finds the address, the coins there are
under the tag from the start, and locked when the tag
[locks its coins](#hw-tag). So the savings of a wallet made again from its
file are locked as they were, change addresses included, and the tags need no
second import.

The `Labels set` line counts the wallets, portfolios, addresses and
transactions that got a label from the file. The label of a wallet or a
portfolio is its description. Each wallet and each portfolio counts once,
though the file writes its description with each of its keys. One that has
that description already counts too, as when you import the same file again.
`--json` gives the counts as `walletlabels`, `portfoliolabels`,
`addresslabels` and `txlabels`.

**Labels that the file replaces.** A label of the file replaces the label
that an address, a transaction or a contact has in the database. So a copy of
the labels puts back a label that you changed since. When the import replaces
a label, a `Labels replaced` line counts them:

```console
$ hw importwallet wallet.jsonl
...
Labels set: 1 address, 1 transaction
Labels replaced: 1 address
```

To see which labels would go, run [`show`](#hw-show) on the file first: it
names each label that the import would replace. A record that has the label
of the database already replaces nothing, and a record without a label leaves
the label as it is. A wallet or a portfolio follows the opposite rule: it
keeps the description it has, as Good to know, below, explains. `--json`
gives the counts as `addresslabelsreplaced`, `txlabelsreplaced` and
`contactlabelsreplaced`.

**Tags.** `importwallet` adds the tags of the file to the tags that its
addresses and its transactions have already. It removes no tag, so importing a
file twice, or an older file, removes nothing. A tag that has no colour takes
the colour the file gives it. A tag that has a colour keeps it. When the file
says that a tag [locks its coins](#hw-tag), the tag locks them after the
import. The file never unlocks a tag: an older file, written before you gave
`--lock-coins`, leaves the lock on. When a lock of the file falls on a coin
that a pending transaction spends, `importwallet` locks the coin and
[says so](#hw-tag): `signtx` and `broadcast` then refuse that transaction.
`importwallet` skips a word of the file
that is not a valid tag by the
[rules of `hw`](#tags-on-addresses-and-transactions). When the file tagged
something, a `Tags set` line follows the `Labels set` line. It counts the
addresses and the transactions that got tags from the file:

```console
$ hw importwallet wallet.jsonl
...
Labels set: 2 addresses
Tags set: 2 addresses
```

`--json` gives the two counts as `addresstags` and `txtags`. A transaction
counts once for each portfolio that takes its tags.

**The address book.** `importwallet` puts the contacts of the file in the
[address book](#the-address-book), each on its network. It adds a contact that
the address book does not have. A contact that the address book has takes the
label of the file, when the file gives one, and gets the tags of the file
besides its own. So you can bring a deleted contact back: import the copy of
the labels that `hw` made before the [`deletecontact`](#hw-deletecontact).
A contact that `importwallet` adds has the date the file gives it, the day
you added it on the computer that wrote the file. A contact that the address
book has keeps its date. A file without dates, as another wallet or an
earlier `hw` wrote it, gives the date of the import.
Contacts belong to no portfolio, so `importwallet` reads them whatever
`--wallet` and `--portfolio` say. An `Address book` line then counts them:

```console
$ hw importwallet wallet.jsonl
...
Labels set: none
Address book: 1 contact
```

A contact of the file that is an address of a wallet of the database is not
added, and is counted under `Left aside`. `--json` gives the two counts as
`contacts` and `contactsown`.

> **Good to know**
>
> - **Import only a file you trust.** Without the mnemonic, `importwallet`
>   cannot prove that the keys of a file are those of your mnemonic. A file
>   that names the fingerprint of your wallet may still carry the keys of
>   someone else. `importwallet` would then add a portfolio with their keys,
>   and `receive` would give out their addresses.
>   Import only a file you wrote with `exportwallet` on your own signer, or
>   one that reached you over a channel you trust. Before you give out an
>   address of a new account, check one of its addresses on the signer, as
>   [Check an address before you give it out](#check-an-address-before-you-give-it-out)
>   explains. On the signer, run `hw show TYPE/0/0 --network NETWORK`, with a
>   path such as `bip84/0/0`. It must show the same two ends as the first
>   address that `receive` gives here on that network and type.
> - **A description you set is kept.** A wallet or a portfolio that has a
>   description keeps it. One that has none, or that `importwallet` creates,
>   takes the description of the file. When the file gives a different
>   description for one that keeps its own, the answer says so under
>   `Left aside`, as
>   `1 label of a portfolio that keeps its own`. `--json` counts them as
>   `walletlabelskept` and `portfoliolabelskept`. To take the description of
>   the file, set it with [`label`](#hw-label).
> - **A tag is on its address before the wallet has the address.**
>   `importwallet` derives a receiving address from the path the file gives.
>   It does not derive a change address just because a file names it, nor an
>   address more than a thousand past the last used one. It keeps the tags of
>   such an address all the same, and counts the address under `Tags set`.
>   [`tags`](#hw-tags) counts it too, and [`addresses`](#hw-addresses) lists
>   it once a `sync` found it used. `importwallet` first checks that the
>   address is the one the account has at the path of the record.
> - **`importwallet` derives no address more than a thousand past the last
>   used one of its chain.** A file can name any path, and a wrong one would
>   have `hw` derive addresses for days. Such an address is counted as not in
>   the wallet yet, and the answer says why.
> - **A file without any record is refused.** A key alone, a descriptor on a
>   line and an account file of an earlier version are such files.
>   `importwallet` stops with an error and changes nothing. In a file that
>   does hold records, a line that is not one is counted, and the import goes
>   on.

#### `hw portfolios`

Shows every portfolio of every wallet, with its balances. A wallet is the
master key of one mnemonic, and a portfolio holds the accounts of a wallet that
have the same account index ([Where a command works](#where-a-command-works)).
Each portfolio starts with a `*` when it is the
[active one](#the-active-portfolio), or a space, then its name, its wallet and
its description. Then come the date it was created, and one line for each
network where it has addresses or coins. The line shows the total that the
portfolio holds there, what it can spend of it now, and how many coins and
addresses it has.

```console
$ hw portfolios
* portfolio0  wallet0  "daily spending"
    created 10 Nov 2026
    btc  total 0.00170000 btc, spendable 0.00150000 btc, unspendable 0.00020000 btc, 4 coins, 5 addresses
    bch  total 0.00000000 btc, spendable 0.00000000 btc, no coins, 1 address

  portfolio1  wallet0  "savings"
    created 10 Nov 2026
    btc  total 0.25000000 btc, spendable 0.25000000 btc, 1 coin, 1 address
```

The `*` is green on a terminal. It takes a column of its own, so that the names
line up whichever portfolio is active.

**The amounts.** The total is the value of every coin of the portfolio on the
network, confirmed or not. The spendable amount is the part of it that a
payment can spend now. It leaves out locked coins, coinbase coins that are not
yet mature, and unconfirmed payments from other wallets. It counts the
unconfirmed coins of your own transactions, such as the change of a payment.
When the portfolio cannot spend all that it holds, the line adds the
unspendable amount, which is the rest: the total is always the spendable
amount plus the unspendable amount. [`balances`](#hw-balances) gives the same
three amounts, and `balances --verbose` says why the rest cannot be spent.
[`sync`](#hw-sync) and the commands that send a transaction show the same
amounts for each portfolio.

**Before the first sync.** `hw` learns what a portfolio holds from the
server, when you run [`sync`](#hw-sync). Until a portfolio has synced on a
network, `hw` knows nothing about what it holds there. The line of the
network then reads `not synced` in place of the amounts and the coins, and
still counts the addresses:

```console
$ hw portfolios
* portfolio0  wallet0
    created 23 Nov 2026
    btc  not synced, 1 address
```

`not synced` does not mean that the portfolio holds nothing. A computer that
signs offline never syncs, so its portfolios stay `not synced`. A network
that the server does not serve stays `not synced` too. The other views say
the same thing in their own way. `--verbose` shows one row,
`Balance: not synced`, in place of the amounts and the coins. `--tabular`
writes `not synced` under the total, and leaves the other amounts and the
coins empty. `--csv` leaves them all empty. With `--json`, each network has
the key `synced`, which is then `false`: its balances are all zero, and say
nothing of what the portfolio holds. [`switch`](#hw-switch) and
[`label`](#hw-label) show a portfolio the same way.

On `btc`, each figure adds up the `bip44` account and the `bip84` account,
since a payment spends both. A network that the portfolio never used has no
line, so a portfolio with twelve accounts still takes a few lines.

`portfolios` lists every portfolio, whichever one is active. It lists the
portfolios of one wallet together.

`--verbose` adds the fingerprint of the mnemonic and the time the portfolio was
created. It lists every [shown network](#shown-and-hidden-networks) of the
portfolio, each with its total, its spendable and unspendable amounts, its
confirmed and unconfirmed balances, its coins and its addresses. The confirmed
and the unconfirmed balances are those that the server gave at the last sync.
They also add up to the total: they split it by what a block has confirmed,
and not by what the portfolio can spend. Under each network come its accounts,
each with its type, its derivation path, its output descriptor and its key in
the form of SLIP-132. An output descriptor is a line of text in the format of
[BIP 380]. It describes an account without its private key, for another wallet
to read. Paste the descriptor into a wallet that takes one, to watch the
account there.

`--tabular` shows a table, with one row for each network where a portfolio has
addresses or coins. The first column has no heading, and holds the `*` of the
active portfolio. Then come the wallet, the portfolio, the network, the total
that the portfolio holds there, what it can spend, what it cannot spend, its
coins, its addresses, and its description. Each of the first columns is as
wide as its longest cell, so no cell is cut.

```console
$ hw portfolios --tabular
  │ Wallet  │ Portfolio  │ Network │    Total (btc) │ Spendable (btc) │ Unspendable (btc) │ Coins │ Addresses │ Description
──┼─────────┼────────────┼─────────┼────────────────┼─────────────────┼───────────────────┼───────┼───────────┼────────────
* │ wallet0 │ portfolio0 │ btc     │     0.00000000 │      0.00000000 │        0.00000000 │     0 │         1 │ "daily spending"
  │ wallet0 │ portfolio1 │ btc     │     0.00000000 │      0.00000000 │        0.00000000 │     0 │         1 │ "savings"
```

`--csv FILE` writes one row for each shown network of each portfolio to a
[CSV file](#listings). Its columns are the wallet, the portfolio, the network
and the types of the accounts of the portfolio there, `Active`, the
description of the portfolio and that of its wallet, the paths of its accounts,
the total it holds, what it can spend, its confirmed and unconfirmed balances,
its coins and addresses, and when the portfolio was created. `Active` says
`yes` for the active portfolio, and `no` for the others. The file holds no key
and no descriptor. With them, anyone who gets the file could see every address
and every payment of the portfolio. `--json` says whether each portfolio is the
active one, as `"active": true` or `false`. It gives the amounts of each
network under `"balance"`, in satoshis: `total`, `spendable`, `confirmed` and
`unconfirmed`.

```console
hw portfolios
hw portfolios --verbose                     # the descriptors, for another wallet
hw portfolios --tabular
hw portfolios --csv portfolios.csv
hw portfolios --unit sat
```

**The descriptor.** A `bip44` account gives
`pkh([bc560f95/44h/0h/0h]xpub.../<0;1>/*)#vq597j87`. The descriptor carries the
public key of the account, the fingerprint of the mnemonic, the derivation
path, the two chains of addresses, receiving and change, and a checksum. It
holds no private key. A hardware wallet matches the account to its mnemonic by
the fingerprint and the path. A descriptor does not say which network it is
for: Bitcoin and Bitcoin Cash both write their keys as `xpub`, and every test
network writes them as `tpub`.

A `bip84` account gives `wpkh([bc560f95/84h/0h/0h]xpub.../<0;1>/*)#5nktextl`.
The function at the start, `pkh` or `wpkh`, names the type of address.

**The key in the form of SLIP-132.** Each account also has a row `SLIP-132`. It
shows the same public key with the prefix that SLIP-132 gives its type of
address: a `zpub` for a `bip84` account, or a `vpub` on a test network. Some
wallets take only that form for a native SegWit account, and make a legacy
wallet when you give them an `xpub`. Only the prefix differs: the key is the
same. For a `bip44` account the row shows the `xpub`, or the `tpub` on a test
network, since SLIP-132 gives `bip44` accounts the prefixes of BIP32. So every
account has a key to paste into a wallet that asks for that form.

`--json` gives the descriptor of each account as `descriptor`, and its key in
the form of SLIP-132 as `slip132`. `hw` writes descriptors and reads none:
[`importwallet`](#hw-importwallet) takes whole wallets, and no descriptor.

> **Good to know**
>
> - **The lines of `portfolios --verbose` wrap on a terminal.** A descriptor is
>   wider than most screens. So the pager wraps these lines and does not cut
>   them at the edge of the screen. You can then copy a descriptor whole.
> - **[`balances`](#hw-balances) repeats these totals.** It gives the same
>   figures for a network, beside the addresses they add up.

#### `hw switch`

`hw switch [PORTFOLIO] [--wallet N]` makes another portfolio the
[active one](#the-active-portfolio). The commands work in that portfolio from
then on, until the next `switch`, since the database keeps the choice.

```console
hw switch 1                                 # portfolio1 of the same wallet
hw switch portfolio1                        # the same, by its name
hw switch 0 --wallet 1                      # portfolio0 of wallet1
hw switch --wallet 1                        # the same
```

`PORTFOLIO` names the portfolio to make active, by its index or by its name.
`--wallet` names its wallet. Without `--wallet`, `switch` looks in the wallet
of the active portfolio. So after `hw switch 0 --wallet 1`, the command
`hw switch 2` goes to `portfolio2` of `wallet1`. With `--wallet` and no
`PORTFOLIO`, `switch` goes to `portfolio0` of that wallet. With neither,
`switch` refuses to run.

**Why `--wallet` alone means `portfolio0`.** Every wallet has a `portfolio0`,
and most wallets have only that one. So you can move to another mnemonic
without remembering which portfolios it has. `switch` does not go back to the
portfolio of that wallet that was active last. The database does not keep that
portfolio, and the same command would go to different places at different
times. With neither value, `switch` has no sure place to go, and it says so.

**Why the portfolio needs no option.** The portfolio is the value `switch`
needs most, and you type `switch` often. So you give it without an option, as
you give `show` the thing to show. The wallet stays an option: most databases
have one wallet, and `hw switch 0 1` would leave you to remember which number
is which. `--portfolio N` also names the portfolio, for a script that names
each value by its option. `hw switch --portfolio 1` does the same as
`hw switch 1`. Give the portfolio once: `switch` refuses a command that gives
both `PORTFOLIO` and `--portfolio`.

The answer shows the portfolio as `portfolios` shows it, with its `*`:

```console
$ hw switch 1
The active portfolio is now portfolio1 of wallet0.
* portfolio1  wallet0  "savings"
    created 11 Nov 2026
    btc  total 0.00000000 btc, spendable 0.00000000 btc, no coins, 1 address
```

`switch` refuses a wallet or a portfolio that the database does not have, and
the active portfolio stays as it was:

```console
$ hw switch 5
Error: wallet0 has no portfolio5: portfolios lists the portfolios
$ hw switch 0 --wallet 2
Error: There is no wallet2: portfolios lists the wallets
$ hw switch
Error: Name the portfolio to make active, such as switch 1, or its wallet, such
as switch --wallet 1 for portfolio0 of wallet1
```

`switch` needs no server, so it works on an offline computer too.

Some commands find their portfolio from what you give them, and need no
`switch` first. They are `importtx`, `signtx`, `bumpfee`, `sweep`,
`preparesweep`, `signsweep`, `label`, `lockcoin`, `unlockcoin`, `signmsg`, and
`show` when
it shows a transaction, a coin or an address. When such a command cannot tell
which of several portfolios to use, it uses the active one if it is among them.
Otherwise it names them, and you switch to the one to use. See
[What each command takes](#what-each-command-takes).

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
> - **Each computer has its own active portfolio.** The offline computer and
>   the online computer each keep theirs, and the file of `exportwallet` does
>   not carry it. To check an address of another portfolio with
>   [`show`](#hw-show), switch to that portfolio on both.

#### `hw setdefault`

Writes defaults to the [configuration file](#configuration), so that you do not
have to edit it. Give it each option with the value you want as the default, as
you would give the option to a command:

| Option | The default it sets | Key |
|---|---|---|
| `--network NET` | The network | `default-network` |
| `--type TYPE` | The type of address | `default-type` |
| `--unit UNIT` | The unit of the amounts | `unit` |
| `--compact`, `--verbose` or `--tabular` | The [view of the listings](#listings) | `listing-view` |
| `--reverse` or `--no-reverse` | Whether the [listings](#listings) list their rows in the reverse order on the screen | `listing-reverse` |
| `--show-network NET`, `--hide-network NET` | The [networks `hw` shows](#shown-and-hidden-networks) | `shown-networks` |
| `--journal` or `--no-journal` | Whether `hw` keeps a [journal for ledger](#a-journal-kept-up-to-date) up to date | `journal` |

```console
hw setdefault --type bip84                  # give out native SegWit addresses
hw setdefault --network bch                 # work on Bitcoin Cash
hw setdefault --unit sat                    # show and read amounts in satoshis
hw setdefault --tabular                     # show the listings as tables
hw setdefault --reverse                     # list the oldest transaction first
hw setdefault --network bch --type bip84    # two defaults at once
hw setdefault --show-network btctest4       # show a hidden network
hw setdefault --hide-network bch            # hide a network
hw setdefault --journal                     # keep a journal for ledger up to date
hw setdefault --no-journal                  # keep no journal, and remove its file
```

It answers with the keys it wrote:

```console
$ hw setdefault --network bch --type bip84
default-network: bch
default-type   : bip84
Written to /home/you/.hw/config.toml
```

`--show-network` and `--hide-network` change the list of the networks that `hw`
shows. You can give each one several times, and both in one command. The answer
gives the whole list, as the file now has it:

```console
$ hw setdefault --show-network btctest4
shown-networks: btc, bch, btctest4
Written to /home/you/.hw/config.toml
```

`--journal` makes `hw` keep a journal of your transactions for [ledger], the
command-line accounting program. `hw` writes the journal again after each
command that changes it. `--no-journal` turns the journal off, and `hw` removes
the file. [A journal kept up to date](#a-journal-kept-up-to-date) tells the
rest. The answer of `--journal` says where the journal is, so that you can
include it in a journal of your own:

```console
$ hw setdefault --journal
journal: true
Written to /home/you/.hw/config.toml
hw now keeps a journal for ledger at /home/you/.hw/portfolios.ledger
```

`setdefault` leaves the rest of the file as it is, your comments too:
[The configuration file](#the-configuration-file) says how.
[`showdefault`](#hw-showdefault) shows the defaults as they are now. Other
sections say what each default does:

- [The two options](#the-two-options): the network and the type.
- [Shown and hidden networks](#shown-and-hidden-networks): what hiding a
  network does.
- [Amounts](#amounts) and [Listings](#listings): the unit and the view.

> **Good to know**
>
> - **The portfolio is no default.** The commands work in the active
>   portfolio, which the database keeps. [`switch`](#hw-switch) changes it.
> - **The default network is always shown.** `setdefault` refuses a hidden
>   network as the default, and refuses to hide the default network. Give
>   both options in one command: `hw setdefault --show-network btctest4
>   --network btctest4`.
> - **`setdefault` does not set `hw` up.** It changes a configuration file that
>   exists already. The [setup](#set-up-hw) writes the first file: run `hw`
>   alone, without a command.
> - **Showing a shown network changes nothing.** Neither does hiding a hidden
>   one. `setdefault` writes the list and shows it, without an error. It
>   refuses the same network given to both options.
> - **The help names every network under `--show-network` only.** The help of
>   `--network` names `btc` and `bch`, the two networks that are checked when
>   the setup starts.
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

#### `hw showdefault`

Shows the defaults that [`setdefault`](#hw-setdefault) writes, and the
[active portfolio](#the-active-portfolio). It shows the defaults as `hw` reads
them from the [configuration file](#configuration). Together they say where a
command works when you give it no option, and how it answers. Without options,
`showdefault` shows them all:

```console
$ hw showdefault
default-network: btc
default-type   : bip84
unit           : sat
listing-view   : compact
listing-reverse: false
shown-networks : btc, bch
journal        : false
Read from /home/you/.hw/config.toml

The active portfolio is portfolio1 of wallet0.
```

Each option shows one of them. The options of the defaults are those of
`setdefault`, without a value. The view, the reverse order and the networks
have one option each here, where `setdefault` has several:

| Option | What it shows | Key |
|---|---|---|
| `--network` | The default network | `default-network` |
| `--type` | The default type of address | `default-type` |
| `--unit` | The unit of the amounts | `unit` |
| `--view` | The [view of the listings](#listings) | `listing-view` |
| `--reverse` | Whether the [listings](#listings) are reversed | `listing-reverse` |
| `--shown-networks` | The [networks `hw` shows](#shown-and-hidden-networks) | `shown-networks` |
| `--journal` | Whether `hw` keeps a [journal for ledger](#a-journal-kept-up-to-date) | `journal` |
| `--portfolio` | The active portfolio, with its wallet | |
| `--wallet` | The wallet of the active portfolio | |

```console
$ hw showdefault --network --type
default-network: btc
default-type   : bip84
Read from /home/you/.hw/config.toml

$ hw showdefault --wallet
The active wallet is wallet0.
```

`showdefault` shows the keys in the order of the table, whatever the order of
the options. With both `--portfolio` and `--wallet`, it shows the portfolio,
which names its wallet too.

With `--json`, the answer gives the defaults as the answer of `setdefault`
does, with the path of the file. The key `activeportfolio` gives the active
portfolio as two indexes: that of its wallet, and its own. The key
`activewallet` gives the index of its wallet. Each of these two keys is there
only when you ask for it, and is `null` in a database without a wallet:

```console
$ hw showdefault --portfolio --json
{
  "activeportfolio": {
    "portfolio": 1,
    "wallet": 0
  },
  "defaults": [],
  "file": "/home/you/.hw/config.toml",
  "type": "showdefault"
}
```

> **Good to know**
>
> - **`--unit` takes no value after `showdefault`.** `hw showdefault --unit`
>   shows the unit, and `hw showdefault --unit sat` is refused, since
>   `showdefault` shows no amount. Before the command, `--unit UNIT` is the
>   [option of every command](#options-of-every-command), and changes
>   nothing here.
> - **A key that the file does not have shows its default.** `showdefault`
>   shows the value that `hw` works with: the value of the key, or else the
>   default that the [configuration table](#configuration) gives.
> - **A database without a wallet has no active portfolio.** `showdefault`
>   says so, and how to make one, and does not fail: `There is no wallet yet:
>   newportfolio makes one, and recover or importwallet bring one back.`
> - **The wallet is always named.** The answer names the wallet of the active
>   portfolio even in a database of one wallet, where the listings leave it
>   out.

#### `hw deleteportfolio`

`hw deleteportfolio PORTFOLIO` deletes a portfolio that was never used. Such a
portfolio has no address, coin or transaction, so nothing is lost with it. Name
the portfolio by its index, or by the name `portfolios` shows.
`deleteportfolio` looks for it in the wallet of the
[active portfolio](#the-active-portfolio). It deletes a portfolio only when all
of these are true:

- It is not `portfolio0`.
- It is not the active portfolio.
- It has the highest index in its wallet. `portfolio1` cannot be deleted while
  `portfolio2` exists.
- It has never had any activity, and gave out no address.

Otherwise it refuses, and says which rule stopped it.

```console
hw deleteportfolio 2                        # portfolio2
hw deleteportfolio portfolio2               # the same, by the name portfolios shows
hw switch 0 --wallet 1                      # to delete a portfolio of wallet1
hw deleteportfolio 1
```

`deleteportfolio` asks before it deletes, and names the portfolio that it is
about to delete. Any answer but `y` or `yes` deletes nothing, and `hw` exits
with an error:

```console
$ hw deleteportfolio 1
portfolio1 of wallet0 will be deleted from the database.
It was never used, so no address, coin or transaction goes with it.
hw copies the database and its labels into auto-backups first.
Delete portfolio1 of wallet0? [y/N] y
portfolio1 of wallet0 has been deleted
...
```

`--yes` answers the question in advance, for [scripts](#scripts).

Each rule has its reason. A wallet always keeps `portfolio0`: a new
`portfolio0` would be the same portfolio, with the same keys, so deleting it
would be pointless. The active portfolio is where the commands work, so the
database must keep it. Switch to another portfolio before you delete it:

```console
$ hw deleteportfolio 1
Error: portfolio1 of wallet0 is the active portfolio: switch to another
portfolio first
```

Only the highest index can go, so that the indexes of a wallet keep no gap. A
portfolio that was paid stays, since `recover` would only find it again.

Activity is a coin, a transaction or a pending transaction. An address counts
too, whether it was paid or not. A new portfolio has no address, since `hw`
generates one only when [`receive`](#hw-receive) gives it out, when a payment
uses one for its change, or when a file or a `sync` names it. Once you have
given out an address, someone may pay it next month. A deleted portfolio would
not see that payment until `recover` finds the portfolio again, and its labels
would be lost. So `hw` keeps a portfolio that has an address.

> **Good to know**
>
> - **You must give the portfolio.** A command that deletes never takes the
>   active portfolio by default. So you never delete a portfolio by accident.
> - **It looks in the wallet of the active portfolio.** To delete a portfolio
>   of another wallet, switch to a portfolio of that wallet first. `hw` refuses
>   a portfolio that the wallet does not have, as in `wallet0 has no
>   portfolio3: portfolios lists the portfolios`.
> - **`hw` backs up the database first.** Before it deletes, `hw` copies the
>   database and its labels into `~/.hw/auto-backups`, with its other
>   [automatic backups](#automatic-backups). Its answer names the two files.
>   It makes no copy after the deletion. With `autobackup-database` set to
>   `false` in the [configuration](#configuration), `hw` makes no backup.
> - **To stop using a mnemonic, delete its whole wallet** with
>   [`deletewallet`](#hw-deletewallet).

#### `hw deletewallet`

`hw deletewallet WALLET` removes a wallet from the database, with all of its
portfolios and their addresses, labels, transactions, coins and pending
transactions. It is for a mnemonic you no longer use. Unlike
`deleteportfolio`, it deletes a wallet that has had activity. The funds stay on
the chain, and the mnemonic can still spend them.

```console
hw deletewallet 1                           # wallet1
hw deletewallet wallet1                     # the same, by the name portfolios shows
```

You must give the wallet, by its index or by its name. `deletewallet` never
takes the wallet of the active portfolio by default, so you never delete a
wallet by accident.

`deletewallet` asks before it deletes. It names the wallet that it is about to
delete, with its description, its fingerprint and how many portfolios it has.
An index is easy to mistype, so check there that it is the wallet you mean. Any
answer but `y` or `yes` deletes nothing, and `hw` exits with an error. `--yes`
answers the question in advance, for [scripts](#scripts).

Before it deletes, `hw` copies the database and its labels into
`~/.hw/auto-backups`, with its other [automatic backups](#automatic-backups),
and names the two files under its answer:

```console
$ hw deletewallet 1
wallet1 will be deleted from the database, with its portfolio.
Its description is "Business", and its fingerprint is 4435aa1c.
Every address, label, transaction, coin and pending transaction of the wallet
goes with it.
The funds stay on the chain, and the mnemonic can still spend them.
hw copies the database and its labels into auto-backups first.
Delete wallet1? [y/N] y
wallet1 has been deleted, with all of its portfolios
Before that, the database was copied to /home/you/.hw/auto-backups/portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite
Its labels are in /home/you/.hw/auto-backups/portfolios-labels-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.jsonl
```

The name of each file counts the wallets and the portfolios that the database
held before the deletion. The copy of the database keeps what nothing else
brings back: the labels of the wallet and its pending transactions.
[`restoredatabase`](#hw-restoredatabase) with that copy brings the whole
database back as it was. [`importwallet`](#hw-importwallet) with the file of
labels brings back only the wallet and its labels, into the database as it is
now.

When the newest copy of the labels in `~/.hw/auto-backups` holds the same
labels already, `hw` writes no second one, and the answer names that copy. Its
name then has an earlier time than the copy of the database.

`hw` makes no copy after the deletion, so these two files stay the newest
copies until the next change. They are automatic backups, so `hw` keeps them
by [the rules for those](#automatic-backups): newer copies push them out with
time. To keep the wallet for good, run [`backup`](#hw-backup) before you
delete it.

The other wallets keep their names. When `wallet0` is deleted, `wallet1` is
still `wallet1`, and the next new wallet becomes `wallet0` again.

`deletewallet` refuses the wallet of the
[active portfolio](#the-active-portfolio) while the database has another
wallet. The commands would otherwise be left with no portfolio to work in.
Switch to a portfolio of another wallet first:

```console
$ hw deletewallet 0
Error: wallet0 holds the active portfolio, portfolio0 of wallet0: switch to a
portfolio of another wallet first
```

You can delete the last wallet, with the active portfolio in it. The database
then has no active portfolio, and the first portfolio created after that
becomes the active one:

```console
$ hw deletewallet 0
...
Delete wallet0? [y/N] y
wallet0 has been deleted, with all of its portfolios. The database has no wallet left: the first portfolio created next becomes the active one.
Before that, the database was copied to /home/you/.hw/auto-backups/portfolios-version-5-2026-11-14T10-06-02.481Z-wallets-1-portfolios-2.sqlite
Its labels are in /home/you/.hw/auto-backups/portfolios-labels-2026-11-14T10-06-02.481Z-wallets-1-portfolios-2.jsonl
```

> **Good to know**
>
> - **The wallet stays in the backups for a time.** A deleted wallet is still
>   in the two files that the answer names, with every address it had. The
>   other [automatic backups](#automatic-backups) made before the deletion
>   hold it too. Newer copies replace them with time. To erase the wallet from
>   this computer now, remove the copies that hold it.
> - **Without the backup, the labels are lost.** With `autobackup-database` set
>   to `false` in the [configuration](#configuration), `hw` makes no backup
>   first. Before you delete, write the labels to a file with
>   [`backup`](#hw-backup) or [`exportwallet`](#hw-exportwallet).
> - **A backup that cannot be made stops the deletion.** When `hw` cannot make
>   the backup, as when the disk is full, it deletes nothing and says why.
> - **`recover` brings the wallet back.** The mnemonic still finds every
>   portfolio that was paid, but not the labels, the locks or the pending
>   transactions. The backup brings those back.

#### `hw sync`

Updates the portfolios from the server: balances, transactions, coins and
pending transactions. It also finds addresses that another wallet of the same
mnemonic used. Run it before you prepare a payment and after you send one.

```console
hw sync
hw sync --network btc                       # one network only
hw sync --full
hw sync --network btctest4 --new-chain      # after the test network was reset
```

`sync` covers every portfolio of every wallet, on every network, the test
networks included, whichever portfolio is active. A portfolio has an account on
each network, and a payment may arrive on any of them. So `sync` also sees a
payment to a portfolio that you are not working in. `--network` syncs one
network only.

`sync` also works on the [hidden networks](#shown-and-hidden-networks), so that
a network is up to date the day you show it. Its answer leaves them out: it
shows nothing for a hidden network, and does not say that the server of one is
down or missing.

`sync` also looks for used addresses that an account does not have yet.
Another wallet of the same mnemonic may have used them, or someone who derives
addresses from the public key of the account. In each account, `sync` looks 20
addresses past the last used one: the `gap` of the
[configuration file](#configuration). Each used address that it finds moves
the search 20 addresses further. `sync` stops at 20 unused addresses in a row,
as a wallet restored from the mnemonic does, and misses a used address that
comes after them. The count starts at the last used address, not at the last
address `receive` gave out. So two computers with the same mnemonic and the
same `gap` find the same payments. To look further, raise `gap` for one sync.

Each portfolio and network that holds something, or where something changed,
gets a few lines of its own: the total that the portfolio holds there and what
it can spend of it, as [`portfolios`](#hw-portfolios) gives them, the block it
is synced to, and what changed. The networks where a portfolio holds nothing
and nothing changed share one line. So do the networks that the server does
not serve. With several wallets in the database, each line names the wallet of
its portfolio, as in `portfolio0 of wallet0  btc`. In this example, the
database has one wallet, and `hw` shows `btc`, `bch`, `btctest4` and `btcreg`:

```console
$ hw sync
portfolio0  btc
  total 0.00150000 btc, spendable 0.00150000 btc, 3 coins
  synced at block 975304
  1 transaction and 1 coin updated

portfolio0  nothing on bch and btctest4
portfolio1  nothing on btc, bch and btctest4

Not served by the store: btcreg
```

A transaction counts as updated when `sync` finds it new or in another block.
It also counts when the network replaced or dropped it, and `txs` then shows it
as dead.

When the sender of a payment replaced it, its coin is gone and the coin of the
replacement takes its place. If you had locked the first coin with
[`lockcoin`](#hw-lockcoin), `sync` moves the lock to the second, and says so
in a line that names both coins.

When a new coin of Bitcoin Cash holds tokens, `sync` says so, with the amount
of the coin: `hw` counts such a coin and never spends it. See
[Coins that hold tokens](#coins-that-hold-tokens).

`sync` also checks each [pending transaction](#hw-pendingtxs):

- When the network lists the transaction, it is no longer pending, and `sync`
  removes it from the pending transactions.
- When you sent the transaction and the server no longer knows it, `sync` sends
  it again.
- When another transaction spent one of its coins, `sync` marks it in conflict.
  It does the same when one of its coins is gone, as after a reorganization
  that removed the payment the coin came from.

The server may answer that it did not publish a transaction that `sync` sent
again. `sync` then goes on, and names the transaction with what the server
said:

```console
$ hw sync
portfolio0  btc
  total 0.00100000 btc, spendable 0.00100000 btc, 1 coin
  synced at block 975310
  3f2a9c1e not sent again: The store at https://api.haskoin.com/ refused the transaction: bad-txns-inputs-missingorspent

Next, delete the pending transaction that was not sent again, to free its coins:
  hw deletetx 3f2a9c1e
```

The server may also take the transaction and then not list it: its node
refused the transaction and gave the server no reason. So `sync` waits up to
ten seconds for the server to list what it sent again, and counts as sent
again only what the server lists. It names each transaction that is still
missing in the same way, on one line:

```console
  3f2a9c1e not sent again: The transaction was handed to the network but the store has not seen it after 10 seconds. A node says nothing of a transaction it refuses, so its fee may be too low for the network
```

The transaction stays pending, and holds its coins, until you delete it. Only
you know whether to give it up: the same transaction may be worth sending
again later, when the fees of the network are lower for example.

`--full` downloads the data of the portfolios again and writes it anew. Use it
if the wallet shows something wrong. It keeps labels, locked coins and pending
transactions. A reorganization of the chain does not need it: `sync` sees one
by itself.

`sync` refuses a server whose chain is not the one of the last sync. From one
sync to the next, it keeps two blocks of each network: the block it reached,
and the block a hundred blocks below it. A reorganization replaces the last
blocks of a chain, and never reaches a hundred blocks down. So a server that
does not have the deeper block is far behind the wallet, or on another chain.
It may be a new server that still reads the chain, or `host` may name another
server than before. Following it would show the payments of the wallet as
dropped from the network, although nothing happened to them. `sync` leaves
that network as it was, and says why:

```console
$ hw sync
portfolio0  nothing on bch

btc  not synced: The store at http://localhost:3000/ has reached block 412230 of
btc only, and the wallet synced as far as block 975304. A store that is new or
still reads the chain is that far behind, and so is another server than the one
of the last sync. hw did not sync btc with it. If the chain of this store is the
right one, as after a test network was reset, follow it with:
  hw sync --network btc --new-chain
```

Wait for the server to catch up, or correct `host`, and run `sync` again.
`--new-chain` is for the day the chain of the server is the right one, which
happens on a test network that was reset, and on a regtest chain that you
started again. It follows the server on the network that `--network` names,
and downloads everything again as `--full` does. The transactions that the new
chain does not have are then shown as dead.

On a terminal, a status line says what `sync` is doing while it works, with a
bar for the steps whose size it knows. The line starts with the network whose
server `sync` is checking. It also names the hidden networks as `sync` works on
them, since it says what you are waiting for. With several wallets in the
database, it names the wallet of each portfolio too. The line goes to stderr
and is gone when `sync` ends, so scripts never see it.

> **Good to know**
>
> - **A network the server does not serve is no error.** A server seldom
>   serves every network. The public one has no regtest chain, for example.
>   `sync` names those networks in one line and syncs the others.
> - **A network whose server is down is an error.** `sync` leaves out that
>   network and says why. It still syncs the other networks, then exits with
>   an error, so that a script can tell.
> - **A hidden network whose server is down is no error.** `sync` says nothing
>   about it, and exits as a success when the shown networks are synced. You
>   asked not to hear about that network. `sync` updates it the next time its
>   server answers.
> - **`sync --network` refuses a hidden network.** Show the network first.
>   `sync` without the option already syncs it.
> - **A server that `hw` cannot reach is down for every network.** When the
>   connection fails or times out, or its TLS handshake fails, `sync` checks no
>   other network on that server. The other networks would fail the same way,
>   and a timeout takes 30 seconds each time. `sync` names them as not synced,
>   with that error. When no network is left, `sync` stops with that error.
> - **A server can also fail in the middle of a sync.** `sync` then leaves the
>   portfolio it was working on as it was before, and does not sync the other
>   portfolios of that network. The portfolios synced before stay synced, and
>   so do the other networks. `sync` names the network with the error, as not
>   synced, and exits with an error. Any other error, such as a no to one of
>   its questions, stops `sync`, and nothing is kept.
> - **A server that serves no network at all is an error too.** That is what a
>   wrong `host` in the configuration looks like.
> - **A server on another chain is an error.** `sync` leaves out a network
>   whose server does not have the block a hundred below the one the last sync
>   reached. It says so, syncs the other networks, and exits with an error.
>   A server that is behind the wallet by fewer blocks is followed, as a
>   reorganization is.
> - **`--new-chain` needs `--network`.** A new chain is taken on one network
>   at a time. The first sync of a portfolio on a network takes the chain of
>   the server as it is: it has no block to compare with.
> - **The check is against a server that is wrong, not against one that
>   lies.** A server that lies can still say that it has the block. See
>   [Keep your funds safe](#keep-your-funds-safe).
> - **`sync` asks after a thousand used addresses in a row** whether to keep
>   looking, since a server that lies reports them without end. A `sync` that
>   cannot ask stops there.
> - **`sync` sends again what you sent.** When the server no longer knows a
>   transaction that you sent, `sync` sends it again, since nodes forget
>   unconfirmed transactions. See [`broadcast`](#hw-broadcast).
> - **A transaction the server does not publish again does not stop `sync`.**
>   The server answered, so what `sync` read from it stands. `sync` names the
>   transaction on every run until you delete it. A server that gives no
>   answer at all is down, and the sync of its network fails, as it does for
>   any other request.
> - **`sync` checks that what it sent again is on the network.** It counts a
>   transaction as sent again only once the server lists it, and waits up to
>   ten seconds for that. A `sync` with a transaction that the network
>   refuses in silence takes that much longer.
> - **`sync` shows no next step when it found nothing new.** The steps after
>   a `sync` are there for what it found: new transactions, a conflict, or a
>   transaction that was not sent again.
> - **A pending transaction in conflict may be in another portfolio.** `sync`
>   and [`pendingtxs`](#hw-pendingtxs) both cover every portfolio. So the next
>   steps of `sync` are `pendingtxs`, which shows the transaction wherever it
>   is, and `deletetx`, which frees its coins, with no `switch`.
> - **New transactions may be in another portfolio.** `txs` lists the
>   transactions of the active portfolio only. When `sync` finds new
>   transactions only in other portfolios, its next steps start with the
>   `switch` to them, before `txs`.

### Addresses

#### `hw receive`

Gives a new address to hand to a payer: the next receiving address of one
account of the [active portfolio](#the-active-portfolio). `--network` and
`--type` pick the account. Without them, it is a `bip44` address on `btc`, or
what [the defaults](#the-two-options) say. On a terminal, `receive` prints
the QR code of the address under it, for the payer to scan.

```console
hw receive --label "invoice 42"
hw receive --label "invoice 42" --tag kyc   # a label, and a tag
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
  Total       : 0.00000000 btc
  Spendable   : 0.00000000 btc
  Unspendable : 0.00000000 btc
  Confirmed   : 0.00000000 btc
  Unconfirmed : 0.00000000 btc
  Coins       : 0
  Created     : 10 Nov 2026 15:08:37 CET
  Account     : portfolio0  btc  bip44
```

These amounts are what the last [`sync`](#hw-sync) found. Before the first
sync of the portfolio on the network, as right after `newportfolio`, `hw` has
not asked the server about the address. One line then stands in place of the
amounts. It does not say that the address is empty: see
[`addresses`](#hw-addresses).

```console
$ hw receive --label "Wages"
bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"
  Balance     : not synced
  Created     : 10 Nov 2026 15:08:37 CET
  Account     : portfolio0  btc  bip44
```

You never create an account first. A portfolio already has one for every
network and type. The path `bip44/0/0` gives the type of the account, then
the path inside it. The full path of this address is `m/44'/0'/0'/0/0`, where
the third step is the index of the portfolio.

`--label` gives the new address a label, to recognize the payment later.
`--tag` puts a [tag](#tags-on-addresses-and-transactions) on it: a word that
puts the address in a group, such as `kyc`. Repeat `--tag` for several tags.
[`label`](#hw-label) and [`tag`](#hw-tag) do the same later, on an address
you gave out before. The answer shows the tags at the end of its first line,
each between brackets. When you tag an address as you give it out, the coins
it receives are in the group from the start:

```console
$ hw receive --label "invoice 42" --tag kyc --tag rent
bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  "invoice 42"  [kyc] [rent]
  Transactions: 0
  ...
  Account     : portfolio0  btc  bip44
```

> **Good to know**
>
> - **`receive` stops after 20 unused addresses in a row** in an account. A
>   wallet restored from the mnemonic stops looking after that many unused
>   addresses, so it would miss a payment to a later one. Give out one of the
>   unused addresses again, or raise `gap` in the
>   [configuration file](#configuration).
> - **Bitcoin Cash has `bip44` addresses only.** `receive --network bch` gives
>   one whatever `default-type` says, and `--type bip84` is refused there.
> - **The QR code holds a payment URI** as [BIP 321] describes, with the
>   address and nothing else. The label and the tags stay in the wallet.
> - **The QR code takes 37 columns and 19 lines.** On a narrower terminal, its
>   lines wrap, and it cannot be scanned.
> - **The QR code is black on white, whatever the theme of the terminal.** With
>   `NO_COLOR` set, it takes the colours of the terminal. On a dark theme it is
>   then light on dark, which some scanners cannot read.
> - **Scripts get the text alone.** When the answer is piped into another
>   program, or with `--json`, `receive` prints no QR code.

#### `hw show`

`hw show [WORD]` shows one thing and changes nothing: an address, a
transaction or a coin of the wallet, or a file. The file can be a transaction
file, a file of a wallet or a database file. `show` tells what the word is by
its shape:

| Word | What `show` shows |
|---|---|
| none | The newest receiving address of the account of the active portfolio, which is the first one that `addresses` lists |
| `bip84/0/i` or `bip84/1/i` | The receiving address number `i`, or the change address number `i`, of the account of that type. `show` derives it from the public key, and hides its middle when the wallet does not hold the address. A path without the type, such as `/0/i`, is in the account that `--type` picks |
| An address | That address of the wallet, a change address too, or that contact of the [address book](#the-address-book) |
| A txid or a nosigHash | That transaction, pending or in the history |
| `TXID:INDEX` | That coin, with every detail, as `coins --verbose` shows it, then the account that holds it |
| Any other word | That file: a [transaction file](#transaction-files), as its portfolio sees it, a file of a wallet, as [`importwallet`](#hw-importwallet) would take it, or a database file, with its wallets and portfolios |

Give a hash in full or by its [short id](#short-ids), its first 8 digits.

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
hw show copy.sqlite                         # a copy of the database
```

**An address.** `show` prints an address of the wallet the way `receive`
prints a new one, with its QR code on a terminal. Its
[tags](#tags-on-addresses-and-transactions) come at the end of its first
line, each between brackets. Use it to give an address out again: `show`
gives out no new address. The address can be in any portfolio. Its `Account`
field says so when it is not in the active one.

When no wallet of the database holds the address, `show` looks for it in the
[address book](#the-address-book). It prints the contact as
[`newcontact`](#hw-newcontact) does: the address in one piece, then in groups
of four characters, with its label, its tags and its network. `show` refuses
an address that is in neither, and names the command that adds it to the
address book.

An address of the wallet can be in neither. A computer holds the addresses
that it generated itself: those that `receive` gave out there, and those that
a `sync` found paid. The same wallet on another computer, such as the offline
computer, has not generated them. `show` cannot tell such an address from an
address of somebody else, so its refusal says what this computer knows, and
how to check the address by its path:

```console
$ hw show 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
Error: The wallet has not generated the address
1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ on this computer, and it is not in the
address book. To check an address that another computer of the wallet gave out,
give its path, as receive printed it: hw show PATH. To add an address of
somebody else to the address book, run: hw newcontact
1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ LABEL
```

`show` does not search the addresses that the accounts would derive next. A
search has an end, so it would find the address in a small wallet and refuse
it again in a large one. A path works whatever the size of the wallet.

**A path.** A path names an address by its place in an account, such as
`bip84/0/7`. It starts with the type of its account, the way the views write
a path. A path without the type, such as `/0/7`, is in the account that
`--type` picks, and `show` refuses `--type` with a path that names its type.
`--network` picks the network. The path is always in the active portfolio.
`show` derives the address from the public key of the account. That needs no
mnemonic and no server, so it works on the offline computer too. Use it there
to check an address that the online computer gave out: see
[Check an address](#check-an-address-before-you-give-it-out).

When the wallet holds the address at the path, in its database, `show` prints
it as any other address. When it does not, `show` prints the address cut: its
two ends, with a dot for each character of its middle, and no QR code. This
is the case on the offline computer, which gave no address out. A note under
the cut address says how to check it:

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

The two ends are enough for the check. They carry at least 128 bits. To make
an address with the same ends, an attacker would have to try about 2^128
keys, which no computer can do. `show` hides the middle so that nobody pays
the address from this screen: over a million addresses fit a cut address.
That matters because the wallet never gave the address out. The address may
lie past the gap, the 20 unused addresses in a row that a restored wallet
looks at, and no `sync` would find a payment there.

**A transaction.** `show` prints a transaction in full, as each portfolio
that holds it sees it, whichever portfolio is active. It prints a payment
between two portfolios of the wallet twice, once for each: outgoing in one
and incoming in the other. Each of the two views names the address of the
other portfolio with its path and that portfolio, as in
`bip84/0/5 of portfolio1`. When a view is that of a portfolio that is not the
active one, its `Portfolio` field says so, and every path in it names its
portfolio. See [Paths of another portfolio](#paths-of-another-portfolio). In
a payment between two wallets, both views name the wallet of every portfolio,
as in `bip84/0/5 of portfolio1 of wallet0`. See
[How the views name a portfolio](#how-the-views-name-a-portfolio). The tags
of the transaction are in a `Tags` field. The tags of its addresses are on
the line below each address, as [`txs --verbose`](#hw-txs) shows them.

**A transaction file.** Run `show` on a
[transaction file](#transaction-files) on the offline computer before you
sign, and on the online computer before you send. `show` changes nothing in
the wallet. It finds the portfolio of the transaction by itself, as
`importtx` does, and prints the transaction as that portfolio sees it, with
the name of the portfolio. It prints a transaction between two portfolios as
the portfolio that pays sees it, with its paths and labels, whichever
portfolio is active. When the portfolio that pays is not the active one, the
`Portfolio` field says so, and every path names its portfolio. No file names
the address that the transaction pays in the other portfolio. `show` names
that address with its path and its portfolio when the database knows it.

The labels that `show` prints are those that the file carries, so an offline
computer sees the labels of the online wallet. A file carries no tag, so a
transaction shown from a file shows none. A file of another wallet that
involves no portfolio of the wallet needs `--network`: see
[Files of other wallets](#files-of-other-wallets).

**A transaction that is not sent yet.** `show` prints a pending transaction
and a transaction file in full: every address that the transaction spends
from, with its amount, and every address that it pays. You may be about to
sign it or to send it, so `hw` leaves nothing out. `signtx`, `signsweep`,
`sweep` and `broadcast` show the same before they ask. Here is a sweep that pays an
address of the wallet and an address of another wallet:

```console
$ hw show 31676431
19 Nov 2026 02:22  Incoming     +0.00010000 btc  not signed, 10 sat/vB
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  NoSigHash: 3167643163f54a3211e9f783a63f2c65111d3773ae16a919e3a089f080d4f239
  Short id : 31676431
  Created  : 19 Nov 2026 02:22:20 CET
  Spends   : 0.00030000 btc of coins that are not this portfolio's
  Fee      : 0.00002260 btc, 10 sat/vB
  Lock time: block 800002
  From     : 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK  0.00030000 btc
  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00010000 btc  bip44/0/0
             1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00017740 btc
```

- **The first line** gives what the portfolio gains or loses, as for every
  transaction. Here the portfolio gains 0.0001 btc of the 0.0003 btc that
  the sweep spends.
- **`Spends`** gives the total of the coins that the transaction spends and
  that are not the portfolio's. A payment of the portfolio has no such
  field. Check this total against the `To` lines and the fee: `hw` cannot
  tell a sweep of your own coins from the payment of somebody else, and a
  sweep file could pay a part of your coins to an address you do not know.
- **`External`** is the type of a transaction that involves no portfolio of
  the wallet, such as a sweep between two addresses outside it. Its first
  line gives the total that it spends, fee included, without a sign: the
  portfolio neither gains nor loses.
- **`from a file`** takes the place of the time when the wallet does not
  hold the transaction as pending. A file keeps no time.

Once a `sync` has seen the transaction on the network, it is history.
[`txs`](#hw-txs) and `show` then print it as the portfolio sees it, and a
payment received lists the addresses of the portfolio alone.

**A file of a wallet.** Run `show` on a file of a wallet before
`importwallet`, to see what the file holds. A file of a wallet is the file
that `exportwallet` writes, or a [BIP 329] file of another wallet. `show`
reads as a file of a wallet any file that is not a database, a PSBT or the
hexadecimal of a transaction. It prints what `importwallet` would do with the
file, and changes nothing. First comes each wallet of the file with its
portfolios. The line of a portfolio says whether `importwallet` creates it,
with its twelve accounts, or finds it in the database already, with nothing
to add. Then comes each account, with the labels and the tags of its
addresses and transactions, and the coins it locks or unlocks. The title of
a section reads `Address labels and tags`, or `Transaction labels and tags`,
when one of its records has tags:

```console
$ hw show wallet.jsonl
BIP 329 file  wallet.jsonl
  A file of a wallet, shown as importwallet would take it

wallet0  bc560f95  not in the database, and importwallet creates it  "Family"
  portfolio0  importwallet creates it, 12 accounts to add  "Savings"
  portfolio1  importwallet creates it, 12 accounts to add  "Spending"

portfolio0  btc  bip44
  Address labels
    bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"

portfolio0  btc  bip84
  Address labels
    bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"

Labels set: 1 wallet, 2 portfolios, 2 addresses
```

The line of a wallet or a portfolio ends with the description that the file
gives it, when it gives one.

The view ends with the lines that [`importwallet`](#hw-importwallet) would
end its answer with, in the same words: the labels it would set, those it
would replace, what the wallet does not have yet, and what it would leave
aside. Nothing is set: `show` says what the import would do.

`show` also says what the import would change in a database that has the
wallet. A wallet or a portfolio keeps the description it has: `show` then
prints the description of the file with `left aside`, and the description
that stays. A label of the file replaces the label that an address, a
transaction or a contact has, and `show` names the label that would go:

```console
$ hw show wallet.jsonl
BIP 329 file  wallet.jsonl
  A file of a wallet, shown as importwallet would take it

wallet0  bc560f95  in the database  "Family"
  portfolio0  nothing to add  "Savings"  left aside: it keeps "offline copy"
  portfolio1  nothing to add  "Spending"

portfolio0  btc  bip44
  Address labels
    bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"  replaces "Wages, renamed offline"

portfolio0  btc  bip84
  Address labels
    bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"

Labels set: 1 wallet, 1 portfolio, 2 addresses
Labels replaced: 1 address
Left aside: 1 label of a portfolio that keeps its own
```

A record with an empty label removes the label, and its line reads
`removes "Wages, renamed offline"`. `--json` gives the labels that would go
under `replaces`, on each record. It gives the description of the file as
`label` on each wallet and each portfolio, with `keptdescription` when the
one of the database stays.

**A file of labels of another wallet.** A [BIP 329] file of another wallet
holds labels, and no key that `hw` imports. Its labels are of use when they
name addresses and transactions of your wallet, as when the other wallet has
the same mnemonic. `show` says which ones do. A line ends with
`not in the wallet yet` when the wallet does not have the address, the
transaction or the coin of the record:

```console
$ hw show labels.jsonl
BIP 329 file  labels.jsonl
  A file of a wallet, shown as importwallet would take it

Labels without an account in the file
  Address labels
    1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"
    1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  "Landlord"  not in the wallet yet

Labels set: 1 address
Not in the wallet yet: 1 address
```

Here the first address is one of the wallet, and `importwallet` would label
it. The second is not, or not yet: a [`sync`](#hw-sync) may still find it.
When every line ends this way and `Labels set` reads `none`, the file does
not concern this wallet.

A line ends with `left aside: the wallet does not have its account` when the
origin of its record names an account of another wallet. The line of a
contact ends with `left aside: an address of the wallet` when its address is
your own, which the address book does not take. `--json` gives what
`importwallet` would make of each record under `status`: `set`,
`notinwallet`, `otheraccount` or `ownaddress`. It gives the counts under the
keys of the answer of `importwallet --json`, such as `addresslabels` and
`addressesmissing`.

The first line says that the file is a BIP 329 file, and names it, so that
you do not take it for a copy of the database, which holds wallets and
portfolios too.

The line of an account names its portfolio alone when the file holds one
wallet, as above. When the file holds several wallets, it names the wallet
too, as in `portfolio0 of wallet1  btc  bip44`. The lines under the heading
name each wallet of the file either way, with its fingerprint.
`hw show wallet.jsonl --wallet bc560f95` shows only that wallet of the file,
and `--portfolio` only one portfolio, as `importwallet` would read them with
the same options.

The labels that belong to no account of the file come after the accounts.
Then come the tags of the file that nothing has, which a
[file of the whole database](#hw-exportwallet) holds. They come under
`Tags not used`, each with its colour and with whether it locks its coins,
as [`tags`](#hw-tags) lists it after the import:

```console
Tags not used
  by-hand
  savings  blue  locks its coins
```

`--json` gives them as `unusedtags`, each with its `name`, its `color` and
`lockscoins`. The tags that an address, a transaction or a contact has are on
its line. Last come the counts of `importwallet`, with the records that `hw`
keeps no label for and the lines that are not records. `show` does not list
those records: the label of an input, of a coin or of a change address.
`show` prints a key that `importwallet` would leave aside, with the reason.
It refuses a file that `importwallet` would refuse, with the same words. One
such file gives a part of a portfolio that the database does not have.
Another holds a bare key on a line, which is not a record of [BIP 329].

**A database file.** Run `show` on a copy of the database before you restore
it with [`restoredatabase`](#hw-restoredatabase), to see which wallets and
portfolios come back. The copy is one that [`backup`](#hw-backup) wrote, or
one of the [automatic backups](#automatic-backups). `show` recognizes a
database of `hw` by its first bytes, which every SQLite database starts with.
It lists the wallets of the file and the portfolios of each:

```console
$ hw show ~/.hw/user-backups/portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite
SQLite database  /home/you/.hw/user-backups/portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-2-portfolios-3.sqlite
  A database of hw at format version 5, with 2 wallets and 3 portfolios

wallet0  bc560f95  "Family"
  * portfolio0  created 01 Nov 2026  "Savings"
    portfolio1  created 03 Nov 2026  "Spending"

wallet1  4435aa1c
    portfolio0  created 10 Nov 2026
```

The first line says that the file is an SQLite database, and names it. The
second gives its format version and counts what it holds. Each wallet then
comes with the fingerprint of its seed and its description, and each of its
portfolios with the day it was created and its description. A `*` marks the
active portfolio of that database: the one that the commands would work in
once the copy is restored.

`show` prints nothing else of a database, on purpose. The name of a copy
already counts its wallets and portfolios, and `show` says which ones they
are. The history, the coins and the balances of a copy are as old as its last
`sync`, and a `sync` brings them up to date once the copy is restored. The
labels are in the file of labels beside the copy, which `show` reads as a
BIP 329 file.

`show` opens the file to read only. It writes nothing to it and migrates
nothing. The file is byte for byte the same afterwards, and the database of
`hw` keeps nothing of it. `--json` gives the same wallets and portfolios, in
an answer of the type `databasefile`.

> **Good to know**
>
> - **`show` tells the kind of a file from what it holds, not from its
>   name.** A database starts with the 16 bytes that every SQLite database
>   starts with. A transaction file is a PSBT or hexadecimal. `show` reads any
>   other file as a BIP 329 file of a wallet. So it tells the two files of a
>   [`backup`](#hw-backup) apart even when each has the name of the other. The
>   first line of the answer says which one you are looking at:
>   `SQLite database` or `BIP 329 file`.
> - **A database file takes no option.** `show` lists every wallet and every
>   portfolio of it, and refuses `--network`, `--type`, `--wallet` and
>   `--portfolio` with it. The refusal says that the file is an SQLite
>   database, since `--wallet` and `--portfolio` go with a BIP 329 file.
> - **Only a database of this format version is shown.** A database of
>   another version is refused, with its version, as "The SQLite database
>   old.sqlite cannot be shown: it is a database of hw at format version 2,
>   made by an earlier hw, and show reads version 5 only". An SQLite database
>   of another program is refused too.
> - **The pending transactions of a copy are not shown.** They come back with
>   the copy. They are the one thing a copy holds that neither a `sync` nor
>   the file of labels brings back.
> - **A database copied by hand gets two files beside it while it is read.**
>   `hw` keeps its database in the write-ahead log mode of SQLite, and a
>   plain copy of `portfolios.sqlite` keeps that mode. SQLite then makes
>   `-wal` and `-shm` files beside it to read it, and removes them when
>   `show` is done. A copy that `hw` made needs none.
> - **`show` refuses an address that the wallet has not generated on this
>   computer and that is not in the address book**, so that its QR code never
>   pays somebody else. The address can still be an address of the wallet, as
>   on the offline computer: `show` it by its path there.
> - **`show` cuts the address at a path that the wallet does not hold, and
>   prints no QR code**, even within the gap. `--json` gives it cut too, under
>   the key `cutaddress`, so that no script pays it.
> - **Look at the path as well as the ends.** A tampered online computer picks
>   the path it shows, so it can match an address of its own against any
>   address of the account. A path far past the addresses you gave out is a
>   sign of tampering. `show` takes any path, so that you can check a large
>   account too.
> - **Give `show` the path as `receive` wrote it.** A portfolio has an address
>   at `/0/7` in each of its accounts. `receive` writes the path with the type
>   of its account, such as `bip84/0/7`, and `show bip84/0/7` derives the
>   address of that account. `show /0/7` derives the one of the default type.
>   Give `show` the `--network` of `receive` too. The row `Account` says which
>   account each command used.
> - **Without a word, the address may have been paid already.** `show` prints
>   the newest address again, whether a payment has reached it or not. Run
>   `receive` for a new one.
> - **`--wallet` and `--portfolio` go with a file of a wallet only.** They
>   show only one wallet or one portfolio of the file, as they make
>   [`importwallet`](#hw-importwallet) read only one. `--wallet` takes the
>   fingerprint of the wallet, such as `bc560f95`, since a file holds no
>   index. In a file of several wallets, `--portfolio` needs `--wallet` with
>   it. One line counts the records of the portfolios left out. `show`
>   refuses a `--wallet` or a `--portfolio` that the file has no record of, as
>   `importwallet` does. With any other word, `show` refuses the two options:
>   "show takes --wallet and --portfolio only with a file of a wallet, where
>   they name one wallet and one portfolio of the file. The rest is shown in
>   the active portfolio, or where it is". A path, or no word, is in the
>   active portfolio. For a transaction, an address or a coin, `show` finds
>   the portfolio by itself.
> - **`--type` goes with a path, or no word.** It names the account, with
>   `--network`. `show` refuses it with anything else.
> - **`--network` goes with most words.** With a path or no word, it names the
>   account. With a transaction file that names no network, it gives the
>   network. With an address or a coin, it says which one to show when the
>   wallet has it on several test networks. `show` refuses it with a file of
>   a wallet, since each record of such a file names its network.
> - **A file that involves several portfolios may need a `switch`.** `show`
>   prints a transaction between two portfolios as the portfolio that pays
>   sees it, whichever portfolio is active. When several portfolios pay, or
>   when none pays and the transaction pays several portfolios, `show` takes
>   the active portfolio among them. When the active portfolio is not one of
>   them, `show` names them. Switch to one of them, and run `show` again.
> - **A file named like a hash, an address or a path is taken for one.** Type
>   its directory before it, as in `hw show ./a1075db5`. Tab offers such a file
>   once you type `./`.
> - **A word that is none of these and names no file is refused**, with the
>   forms `show` takes. A short id with a digit missing gets this answer, for
>   instance.
> - **`show` sorts a file of a wallet by the origins of its records.** It
>   puts a record under the account that the origin and the network of the
>   record name. `importwallet` places a label by what the wallet holds
>   instead, so it may place a label that `show` prints apart from the
>   accounts.

#### `hw addresses`

Lists the receiving addresses of the active portfolio, or its change addresses
with `--change`. The addresses come in sections, one for each network and
type: the `bip44` addresses on `btc`, then the `bip84` ones, then those on
`bch`, and so on. A title line above each section names the portfolio, the
network and the type. A section in which the portfolio has no address is left
out. `--network` and `--type` show one network or one type only.

In each section, the newest address comes first. `--reverse` lists each
section from its first address instead. Each address takes two lines. The
first line gives its path, such as `bip44/0/0`, which starts with the type of
its account. Then come the address, its label and its
[tags](#tags-on-addresses-and-transactions), each between brackets, as in
`[kyc]`. The second line gives the total of the address, what it received,
and how many transactions it has: those that paid it and those that spent
from it. An address that no transaction has paid yet is `unused`.

```console
$ hw addresses --network btc
portfolio0  btc  bip44

bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"
  unused

portfolio0  btc  bip84

bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"
  unused
```

**Before the first sync.** `hw` learns what an address received from the
server, when you run [`sync`](#hw-sync). Until a portfolio has synced on a
network, `hw` knows nothing about its addresses there. Their second line then
reads `not synced`, and not `unused`:

```console
$ hw addresses --network btc --type bip44
bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj
  not synced
```

`not synced` does not mean that nobody paid the address. A computer that
signs offline never syncs, so every address stays `not synced` there. Read
what an address holds on the online computer. The other views say the same
thing in their own way. `--verbose` shows one line, `Balance: not synced`, in
place of the amounts and the counts. `--tabular` and `--csv` leave them
empty, the number of transactions too. With `--json`, each address has the
key `synced`, which is then `false`: its balances are all zero, and say
nothing of what the address holds. [`receive`](#hw-receive),
[`show`](#hw-show), [`label`](#hw-label) and [`tag`](#hw-tag) show an address
the same way. [`portfolios`](#hw-portfolios), [`balances`](#hw-balances),
[`coins`](#hw-coins) and [`txs`](#hw-txs) say `not synced` for the whole
network.

**The amounts.** The total of an address is the value of its coins. When the
address cannot spend all of it now, the second line names the unspendable
amount after the total. That amount counts the coins that are locked or that
wait for a confirmation, as [`balances`](#hw-balances) counts them. The
address can spend the rest. So an address with a locked coin does not read as
an address that a payment emptied, whose total is zero. Here
[`lockcoin`](#hw-lockcoin) locked the coin of the second address:

```console
$ hw addresses --network btc --type bip44
bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg
  total 0.00030000 btc, unspendable 0.00030000 btc, received 0.00030000 btc, 1 transaction

bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj
  total 0.00050000 btc, received 0.00050000 btc, 1 transaction
```

`--verbose` gives the total of each address, what it can spend of it and what
it cannot, then its confirmed and unconfirmed balances. These two pairs split
the same total in two ways, as in [`portfolios --verbose`](#hw-portfolios).
It also gives the coins of the address and when it was created.

`--tabular` shows each address on one row of a table. The columns are the
type and the path, the address, its total, what it can spend of it, what it
received, how many transactions it has, its tags, and its label. Each section
has its title and its own table. An unused address leaves its amounts empty.
The table of change addresses has no label column, since change addresses
take no label. Its last column is `Tags`. Here the two addresses have two
tags each:

```console
$ hw addresses --network btc --tabular
portfolio0  btc  bip44
Path      │ Address                            │    Total (btc) │ Spendable (btc) │ Received (btc) │   Txs │ Tags         │ Label
──────────┼────────────────────────────────────┼────────────────┼─────────────────┼────────────────┼───────┼──────────────┼──────
bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │                │                 │                │     0 │ cold kyc     │ "Wages"

portfolio0  btc  bip84
Path      │ Address                                    │    Total (btc) │ Spendable (btc) │ Received (btc) │   Txs │ Tags         │ Label
──────────┼────────────────────────────────────────────┼────────────────┼─────────────────┼────────────────┼───────┼──────────────┼──────
bip84/0/0 │ bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc │                │                 │                │     0 │ cold savings │ "Savings plan"
```

`--tag TAG` lists only the addresses that have the tag, which
[`tag`](#hw-tag) puts on them. Repeat it for several tags: the listing then
shows the addresses that have any one of them, and its first line names those
tags. Each address still shows all of its tags. `addresses` refuses a word
that is not a tag:

```console
$ hw addresses --tag kyc --tag savings
Addresses tagged kyc or savings

portfolio0  btc  bip44

bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"  [cold] [kyc]
  unused

portfolio0  btc  bip84

bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"  [cold] [savings]
  unused
$ hw addresses --tag nope
Error: There is no tag nope. hw tags lists the tags.
```

`--untagged` lists only the addresses that have no tag: those you have put in
no group yet. The first line of the listing is then `Addresses without a tag`.
With `--tag` as well, the listing shows both.

`--csv FILE` writes the addresses to a [CSV file](#listings): the wallet, the
portfolio, the network and the type of each, the address, its path, its chain,
its label and its tags, its total, what it can spend of it, its confirmed and
unconfirmed balances, what it received, its coins and transactions, and when
it was created. The column `Chain` says `receiving` or `change`. The tags
share one cell, with a space between them.

```console
hw addresses
hw addresses --network btc --type bip84     # one account, with no title line
hw addresses --change
hw addresses --verbose
hw addresses --tabular
hw addresses --change --tabular
hw addresses --tag kyc                      # the addresses that have the tag
hw addresses --tag kyc --tag savings        # those that have either tag
hw addresses --change --tag kyc             # the change addresses that have it
hw addresses --untagged                     # the addresses that have no tag
hw addresses --csv addresses.csv
hw addresses --max 10 --after ADDRESS
hw addresses --reverse                      # from the first address of each section
```

> **Good to know**
>
> - **A path names its account by its type.** Each account has its own
>   `/0/0`, so a path starts with the type of its account, such as
>   `bip84/0/0`. The title of the section gives the portfolio and the network.
> - **`--after ADDRESS` starts after that address, in its section.** The
>   listing then goes on with the sections that follow.

#### `hw balances`

Lists the addresses of the active portfolio that hold coins, receiving and
change addresses together, grouped by network. The title line of each network
gives three amounts: the total, which is the value of every coin of the
portfolio there, what the portfolio can spend now, and the rest. On `btc`,
the `bip44` and the `bip84` addresses are listed together, since a payment
spends both. `--network` and `--type` show one network or one type only.

```console
$ hw balances
portfolio0  btc  total 0.00170000 btc, spendable 0.00150000 btc, unspendable 0.00020000 btc

bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"
  total 0.00120000 btc, spendable 0.00120000 btc, unspendable 0.00000000 btc

bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"
  total 0.00050000 btc, spendable 0.00030000 btc, unspendable 0.00020000 btc
```

On each network, the address with the largest total comes first, or the one
with the smallest with `--reverse`. Each address takes two lines. The first
line gives its path, which starts with the type of its account, then the
address and its label, or `change address`, and its
[tags](#tags-on-addresses-and-transactions), each between brackets. The type
tells apart the two addresses at `/0/0`, one in each account. The second line
gives the three amounts of the address. The unspendable amount counts the
coins that a pending transaction, `lockcoin` or a [tag](#hw-tag) locks, the
coins that [hold tokens](#coins-that-hold-tokens), the unconfirmed payments
from other wallets, and the coinbase coins with fewer than 100 confirmations.

The totals repeat those of [`portfolios`](#hw-portfolios), on purpose. They
are the same figures, beside the addresses that add up to them. Each total is
for one network, since `hw` never adds the coins of two networks.

**Before the first sync.** Until the portfolio has synced on a network, `hw`
knows nothing about what its addresses hold there, and has no coin to list.
When the portfolio has addresses on such a network, the title line of the
network reads `not synced` in place of the amounts, with no address under it:

```console
$ hw balances
portfolio0  btc  not synced
```

`not synced` does not mean that the addresses hold nothing. A computer that
signs offline never syncs: read what the addresses hold on the online
computer. `--tabular` shows the same line. With `--json`, the group of such
a network has the key `synced`, which is then `false`, a total of zeros and
no address. A `--csv` file has a row for each address that holds coins, so
it has none for such a network. A network where the portfolio has no address
has no line, as in `portfolios`.

`--verbose` gives the total and the number of coins of each address, what it
can spend now, and what it cannot spend, in four parts. The parts are what
pending transactions lock, what you locked with `lockcoin`, what a tag of the
address locks, and what is not spendable yet, which waits for a confirmation.
An address with a coin that [holds tokens](#coins-that-hold-tokens) has a
fifth part, `Locked by tokens`.

`--tabular` shows each address on one row of a table: its type and its path,
the address, the value of its coins, what it can spend now and the rest, its
tags, and its label, or `change address`.

`--tag TAG` lists only the addresses that have the tag. Repeat it for several
tags: the listing then shows the addresses that have any one of them, and its
first line names those tags. The total of each network then adds up those
addresses alone. So `hw balances --tag savings` tells what the savings hold,
and what a payment with `--spend-from-tag savings` can spend. For a tag that
[locks its coins](#hw-tag), the listing counts that amount as unspendable,
since no other payment spends it:

```console
$ hw balances --tag savings
Addresses tagged savings

portfolio0  btc  total 0.00050000 btc, spendable 0.00030000 btc, unspendable 0.00020000 btc

bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"  [savings]
  total 0.00050000 btc, spendable 0.00030000 btc, unspendable 0.00020000 btc
```

`--untagged` lists only the addresses that have no tag. The total of each
network then tells what is in no group. With `--tag` as well, the listing
shows both.

`--csv FILE` writes the addresses to a [CSV file](#listings): the wallet, the
portfolio, the network and the type of each, the address, its path, its chain,
its label and its tags, its total, what it can spend, and the parts it
cannot spend: the four above, then what its coins that hold tokens are worth.
Then come the columns that the file of
[`addresses`](#hw-addresses) ends with: its confirmed and unconfirmed
balances, what it received, its coins and transactions, and when it was
created. The two files split the same coins two ways: by what you can spend,
and by what is confirmed. This one also says why the rest cannot be spent.

```console
hw balances
hw balances --network bch
hw balances --type bip44                    # what the legacy addresses still hold
hw balances --verbose
hw balances --tabular
hw balances --tag savings                   # what the addresses of a tag hold
hw balances --untagged                      # what is in no group
hw balances --csv balances.csv
hw balances --reverse                       # the smallest total first
```

#### `hw label`

`hw label [ADDRESS|TXID|PATH] LABEL` sets the label of a receiving address, of
a transaction, or of a contact of the [address book](#the-address-book). With
the label alone, it sets the description of the active portfolio, or of its
wallet with `--of-wallet`. The new label or description replaces the old one.
An empty label, `""`, removes it.

| Words | What `label` labels |
|---|---|
| `ADDRESS LABEL` | That receiving address of the wallet, or that contact of the address book |
| `bip84/0/i LABEL` | The receiving address number `i` of the account of that type, in the active portfolio. A path without the type, such as `/0/i`, is in the account that `--type` picks |
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
your own words. So one command sets them all, and the words you type tell
`label` which thing to label. To put several addresses or transactions in a
group that the other commands work on, give them a
[tag](#tags-on-addresses-and-transactions) instead.

**An address.** `label` finds the portfolio and the account of an address by
itself. So it takes no `--type` with an address, and it labels the address
whichever portfolio is active. Change addresses take no label. They take
tags, with [`tag`](#hw-tag).

**A contact.** When no wallet of the database holds the address, `label`
looks for it in the address book, and labels its contact. The label says
whose address it is. `label` answers with the contact, as
[`newcontact`](#hw-newcontact) does. `label` refuses an address that is in
neither, and names the command that adds it. An address of the wallet that
this computer has not generated is in neither, so the refusal does not say
whose address it is:

```console
$ hw label 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ Zoe
Error: The wallet has not generated the address
1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ on this computer, and it is not in the
address book. To add an address of somebody else to the address book, run: hw
newcontact 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ LABEL
```

**A path.** A path names an address of the active portfolio, the way the views
write it, such as `bip84/0/3`. It must be a path of the receiving chain,
`/0/i`, since change addresses take no label. The portfolio must have given
the address out: `addresses` lists those it gave out. A path without the
type, such as `/0/3`, is in the account that `--type` picks, or in the
account of the default type. `label` refuses `--type` with a path that names
its type.

**A transaction.** Name a pending transaction by the hash that `pendingtxs`
shows, and a transaction of the history by its txid. Give either hash in full
or by its [short id](#short-ids). A pending transaction keeps its label in
its file while an offline computer signs it, and in the history once the
network has the transaction. A label that you set while the file is away
stays when the signed file comes back.

`send`, `sendall`, `preparetx`, `preparesendall`, `sweep` and `preparesweep`
take a `--label`, and `bumpfee` gives a replacement the labels of the transactions
it replaces. A transaction between two portfolios of the wallet gets the
label in both.

**A portfolio or a wallet.** With the label alone, `label` describes the active
portfolio, or its wallet with `--of-wallet`. `portfolios` shows the
description, and the file of [`exportwallet`](#hw-exportwallet) carries it.
`label` answers with the portfolio, as `portfolios` shows it:

```console
$ hw label "paper backup in the safe" --of-wallet
* portfolio0  wallet0  "daily spending"
    wallet0 is "paper backup in the safe"
    created 11 Nov 2026
    btc  total 0.00000000 btc, spendable 0.00000000 btc, no coins, 1 address
```

Wallets and portfolios have fixed names, such as `wallet0` and `portfolio0`,
which follow their indexes and cannot be changed. To describe another
portfolio, switch to it first. To describe another wallet, switch to one of
its portfolios.

> **Good to know**
>
> - **`--network` is needed only when several addresses match.** The test
>   networks share their keys, so the wallet may have the same address on
>   several of them. A portfolio also has an address at the same path on each
>   network where it gave one out. `--network` then says which address to
>   label. Without it, `label` takes the only one, or else the one on the
>   default network.
> - **A word alone is a description.** If you leave out the label, as in
>   `hw label bip84/0/3`, the word you typed would replace the description of
>   the portfolio. So `label` refuses a word alone that reads as an address, a
>   path or a transaction id, and says to give the label after it. Such a word
>   cannot be a description.
> - **A description is free text on one line, and it is not a name.** No
>   command takes a description in place of an index, so two portfolios may
>   have the same description.

### Coins

#### `hw coins`

Lists the coins of the active portfolio, grouped by network. A coin is an
unspent output of a transaction, written `TXID:INDEX`. On `btc`, the `bip44`
and the `bip84` coins are listed together, since a payment spends both.
`--network` and `--type` show one network or one type only.

On each network, the coins are listed in the order of the chain, the newest
first, as [`txs`](#hw-txs) lists the transactions. The unconfirmed coins come
first, then the others by their block and their position in it. `--reverse`
lists the oldest coins first, and the unconfirmed ones last. The times shown
follow that order only nearly, since the time of a block can be earlier than
the time of the block before it. Each coin takes two lines:

1. The [short id](#short-ids) of the coin, such as `a1075db5:1`, which
   `lockcoin` and `--coin` take. Then the time of the transaction that paid it,
   its value, its state, spendable or locked, and its confirmations, up to
   `999+ confirmations`. A coin that a pending transaction spends is
   `locked by tx`. So is a coin of a payment that a pending
   [replacement](#hw-bumpfee) replaces, since the coin goes away once the
   replacement is sent. A coin that you locked with `lockcoin` is
   `locked by you`.
   A coin at an address that has a [tag that locks its coins](#hw-tag) is
   `locked by tag`. A coin of Bitcoin Cash that
   [holds tokens](#coins-that-hold-tokens) is `locked by tokens`.
2. The address that holds it, in full, with its type and its derivation path,
   such as `bip44/0/0`, then the label of the address, or `change address` when
   it is one, and the [tags](#tags-on-addresses-and-transactions) of the
   address, each between brackets.

`coins` is the only listing whose compact view gives an address of the wallet
in full. The address that holds a coin tells the coin apart better than its
path does. The address also tells the type of the coin, as the path does: a
legacy address holds a `bip44` coin, and a native SegWit address a `bip84`
coin. `coins` is also the only view that keeps the whole label of an address
on the line of the address, so that each coin takes two lines. In the other
views, the label goes on the line below an address shown in full, and it
stays on the same line where the view gives only the path.

`--verbose` starts each coin with the time of its transaction, and gives the
full count of the confirmations on the first line. It says in full what locks
a coin: `locked by a pending transaction` for a coin that a pending
transaction spends, and `locked by a pending replacement` for a coin of a
payment that a pending transaction replaces. Its fields follow: the
label of the transaction that paid the coin, the coin in full and its short
id, the block that holds it, the time of the transaction to the second, and
the address in full with its path. The label and the tags of the address are
on the line below the address. An unconfirmed coin is in no block yet, and
shows none. A coin has no label and no tag of its own: it is in the groups of
its address. The change of a payment you made shows no label of its
transaction, since that label says what the payment was for, not what its
change is.

`--tabular` shows each coin on one row of a table: the short id of the coin,
the time of its transaction, its value, its state, its confirmations up to
`999+`, the address that holds it with its path, the tags of the address, and
its label.

`--tag TAG` lists only the coins at the addresses that have the tag. Repeat
it for several tags: the listing then shows the coins at the addresses that
have any one of them. These are the coins that
[`--spend-from-tag`](#hw-preparetx) chooses, so list them before you pay from
a tag. The first line of the listing names the tags, as in
`Coins at addresses tagged kyc or cold`.

`--untagged` lists only the coins at the addresses that have no tag: the
coins that no `--spend-from-tag` chooses. The first line is then
`Coins at addresses without a tag`. With `--tag` as well, the listing shows
both.

`--csv FILE` writes the coins to a [CSV file](#listings): the wallet, the
portfolio, the network and the type of each, the coin as `TXID:INDEX`, in full
and by its short id, its txid in full and by its short id, the index of its
output, the time of its transaction, its value, state, confirmations and
block, the address that holds it with its path and chain, the label and the
tags of the address, and the label of the transaction. The txid has columns of
its own, so that you can match a coin with its transaction in the file of
[`txs`](#hw-txs).

[`show TXID:INDEX`](#hw-show) shows one coin, with every detail, and the
account that holds it.

**Before the first sync.** Until the portfolio has synced on a network, `hw`
knows nothing about what its addresses hold there, and has no coin to list.
When the portfolio has addresses on such a network, the listing shows one
line for it, in every view, and no coin:

```console
$ hw coins
portfolio0  btc  not synced
```

`not synced` does not mean that the addresses hold no coin
([`addresses`](#hw-addresses) says more). With `--json`, the group of such a
network has the key `synced`, which is then `false`, and no coin. A `--csv`
file has no row for it. A network where the portfolio has no address has no
line.

```console
hw coins
hw coins --network btc --type bip44         # the legacy coins on Bitcoin
hw coins --verbose
hw coins --tabular
hw coins --csv coins.csv
hw coins --locked                           # the coins a transaction, lockcoin or a tag locks
hw coins --tag kyc                          # the coins --spend-from-tag kyc chooses
hw coins --untagged                         # the coins at untagged addresses
hw coins --reverse                          # the oldest coins first
```

> **Good to know**
>
> - **Unconfirmed payments from others cannot be spent.** The sender can still
>   replace them until they confirm. The unconfirmed coins of your own
>   transactions can be spent: their change, and what one of your portfolios
>   pays another.

#### `hw lockcoin`

Keeps a coin from being spent, until [`unlockcoin`](#hw-unlockcoin) frees it.
Name the coin by its [short id](#short-ids), as `coins` shows it, or in full.
`lockcoin` answers with the coin in every detail, as `coins --verbose` shows
it, then the account that holds it. That field says so when the account is
not in the active portfolio.

```console
hw lockcoin TXHASH:1
hw lockcoin a1075db5:1                      # the short id coins shows
```

`lockcoin` finds the coin by itself, so it locks the coin whichever portfolio
is active. The test networks share their keys, so the wallet may have the
same coin on several of them. `--network` then says which coin to lock.

`lockcoin` locks the coin you name, and no coin that arrives after it. To
keep a whole group of addresses out of your payments, such as savings, make
the tag of the group lock its coins with
[`hw tag TAG --lock-coins`](#hw-tag). The two locks are separate. You can
lock with `lockcoin` a coin that a tag locks. The coin then stays locked even
for a payment that names the tag with `--spend-from-tag`.

The lock stays while the coin confirms. Until a block holds a payment, its
sender can replace it, to raise its fee. The same money then arrives as
another coin, with another id, at the same address. [`sync`](#hw-sync) moves
the lock to that coin, and names both coins:

```console
$ hw sync
portfolio0  btc
  total 0.03000000 btc, spendable 0.00000000 btc, unspendable 0.03000000 btc, 1 coin
  synced at block 975304
  2 transactions and 2 coins updated
  lock moved from 527a7424:1 to eb711dff:0, the coin that replaced it
```

`unlockcoin` then takes the new id. The lock goes away with the coin in the
other cases: when a transaction made elsewhere spends the coin, when the
network drops its payment, and when the replacement no longer pays the
address.

The lock holds against every transaction, and not only against the payments
you prepare. [`importtx`](#hw-importtx), [`signtx`](#hw-signtx) and
[`broadcast`](#hw-broadcast) refuse a transaction that spends the coin. So a
file that was prepared before the lock, or on another computer, does not
spend it:

```console
$ hw signtx --input old.psbt --output signed.psbt
Error: The transaction spends the coin 3b273748:1, which is locked by you:
unlockcoin 3b273748:1 unlocks it
```

> **Good to know**
>
> - **`lockcoin` refuses a coin that a pending transaction spends.** The lock
>   would say that nothing spends the coin, while a payment that does waits
>   to be signed or sent. `lockcoin` names that transaction. Delete it with
>   [`deletetx`](#hw-deletetx), then lock the coin:
>
>   ```console
>   $ hw lockcoin 3b273748:1
>   Error: The pending transaction 885c608d spends the coin already: deletetx
>   885c608d frees it
>   ```
>
>   `lockcoin` also refuses a coin of a payment that a pending
>   [replacement](#hw-bumpfee) replaces, and names the replacement. The coin
>   goes away once the replacement is sent.
> - **The locks are those of this computer.** An offline computer that never
>   ran `sync` holds no coin, so it has no lock to check, and signs. The
>   computer that sends the transaction checks its own locks.

#### `hw unlockcoin`

Frees a coin that [`lockcoin`](#hw-lockcoin) locked. It answers with the coin
in every detail, and takes `--network`, as `lockcoin` does.

```console
hw unlockcoin TXHASH:1
```

`unlockcoin` removes only the lock of `lockcoin`. To free a coin that a
pending transaction locks, delete that transaction with
[`deletetx`](#hw-deletetx). `unlockcoin` refuses a coin that a
[tag locks](#hw-tag), and says what unlocks it:

```console
$ hw unlockcoin 3b273748:1
Error: The coin is locked by a tag of its address, not by lockcoin. hw tags
shows the tags that lock their coins: hw tag TAG --unlock-coins unlocks them,
and --spend-from-tag TAG spends them
```

#### Coins that hold tokens

On Bitcoin Cash, a coin can hold tokens (CashTokens) beside its satoshis. The
tokens are written in the output of the coin, and whoever spends the coin
decides where they go. `hw` does not handle tokens. A payment of `hw` has no
output that carries them, so a payment that spent such a coin would destroy
its tokens. `hw` therefore never spends a coin that holds tokens.

Such a coin is still a coin of the wallet, at one of its addresses, so `hw`
shows it and counts it:

- [`sync`](#hw-sync) tells when one arrives, in the lines of its portfolio,
  with what the new coins hold:

  ```console
  $ hw sync
  portfolio0  bch
    total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc, 2 coins
    synced at block 800002
    1 transaction and 2 coins updated
    1 new coin holds tokens (CashTokens): 0.00030000 btc, which hw never spends
      a wallet that handles tokens moves it, with the same mnemonic
  ```

- [`coins`](#hw-coins) lists it as `locked by tokens`.
- [`balances`](#hw-balances) counts it in the total and in the unspendable
  amount, and `balances --verbose` gives it as `Locked by tokens`.
  [`portfolios`](#hw-portfolios) and `sync` count it in the same two amounts.
  The totals of `addresses` and `txs` count it too, so every view agrees with
  the chain.

Nothing opens this lock. The payment commands leave the coin where it is, and
a `sendall` names it among the coins that stay. [`unlockcoin`](#hw-unlockcoin)
refuses it, and so does `--coin`. [`preparesweep`](#hw-preparesweep) and
`sweep` leave such a coin at the address of its key, sweep the other coins,
and name the coin below the sweep. `signtx` and `signsweep` refuse a
transaction made elsewhere that spends one.

To move the coin and its tokens, use a wallet that handles the tokens of
Bitcoin Cash, with the same mnemonic and passphrase: the coin is at an
address of the `bip44` account of Bitcoin Cash, `m/44'/145'/N'` for portfolio
`N`. Once that wallet has spent the coin, the next `sync` removes it from
`hw`.

> **Good to know**
>
> - **The satoshis of the coin stay with its tokens.** A coin that holds
>   tokens usually holds few satoshis. `hw` cannot spend them without the
>   tokens.
> - **`hw` gives no token-aware address.** A wallet that handles tokens writes
>   its address in a form that starts with `z`, to tell a sender that it can
>   take tokens. `hw` shows the plain form alone, which starts with `q`. Both
>   forms name the same script, so tokens sent to an address of `hw` arrive
>   all the same.

### Payments

#### `hw fees`

Shows the low, median and high fee rates of the latest six blocks, in satoshis
per virtual byte. That is the unit in which the network charges fees, and the
unit every wallet shows. In it, a byte of a signature in a witness counts as
a quarter of a byte. The sizes that `hw` shows are in virtual bytes as well.

The commands that take `--fee-rate` show the same rates and ask for one when
you give none. Answer with a level, `low`, `median` or `high`, or with a
number of satoshis per virtual byte.

```console
hw fees
hw fees --network bch
```

> **Good to know**
>
> - **Choosing a fee rate needs the server.** Without `--fee-rate`, `hw` reads
>   the rates of recent blocks, and stops if it cannot reach the server.
> - **`hw` asks you to confirm a rate above 100 satoshis per virtual byte,**
>   since such a rate is more often a typo than a choice.

#### `hw send`

`hw send ADDRESS:AMOUNT...` makes a payment in one command. It prepares the
transaction as [`preparetx`](#hw-preparetx) does, shows it, asks for the
mnemonic, signs the transaction as [`signtx`](#hw-signtx) does, and sends it as
[`broadcast`](#hw-broadcast) does. The result is the same as when you run the
three commands one after the other with the same options.

```console
hw send ADDRESS:0.0005 --fee-rate median --label "rent of October"
hw send ADDRESS:0.0005 --fee-rate median --tag rent   # tag the payment as you make it
hw send ADDRESS1:10000sat ADDRESS2:250bits --fee-rate 5
hw send ADDRESS:0.01 --network bch --fee-rate low
hw send ADDRESS:0.01 --spend-from MYADDRESS --change-back MYADDRESS --fee-rate low
hw send ADDRESS:0.01 --spend-from-tag kyc --tag-change --fee-rate low   # pay from a tag, and keep the change in it
hw send ADDRESS:0.01 --spend-from-tag kyc --coin-type bip84 --fee-rate low   # pay from the SegWit coins of a tag
```

`send` needs the mnemonic and the server at the same time, so it runs on an
online computer. Use it for a wallet whose mnemonic you are willing to type
there. For funds in cold storage, pay in three steps and
[sign on the offline computer](#signing-on-an-offline-computer): `send` cannot
keep the mnemonic offline.

**The options** are those of [`preparetx`](#hw-preparetx), all but `--output`,
since `send` writes no file. They mean the same, and they build the same
transaction. The section of `preparetx` explains the coins, coin control, the
change, the lock time and the fee. Without `--fee-rate`, `send` shows the
rates and asks.

**What you see.** `send` shows the transaction once, in the form of the
answer of `preparetx`. Below it, `send` lists the chosen coins that it could
not spend. It then warns that typing the mnemonic signs the transaction and
sends it, and asks for the mnemonic:

```console
$ hw send 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0002 --fee-rate 10 --label "rent of October" --tag rent
13 Nov 2026 17:45  Outgoing     -0.00022260 btc  not signed, 10 sat/vB
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  Label    : "rent of October"
  Tags     : rent
  NoSigHash: f0ce68dc4e82845905a7c9ee8ae5d6c51d1451e21908a7e596689e8960e5e0b5
  Short id : f0ce68dc
  Created  : 13 Nov 2026 17:45:20 CET
  Fee      : 0.00002260 btc, 10 sat/vB
  Lock time: block 799947
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0
  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0

Typing the mnemonic signs this transaction and sends it to the network. This
cannot be undone.
Mnemonic, or a share of a backup:
Passphrase, or nothing if the seed has none:
Sent, txid 8d9d52e56cbaafc4f2705852018b2211faae911f125be96282a693c663215a1a

portfolio0  btc
  total 0.00027740 btc, spendable 0.00027740 btc, 1 coin
```

Check the transaction at that point: the addresses, the amounts and the fee.
The label and the tags are those of `--label` and `--tag`. Typing the
mnemonic is the confirmation. `send` asks nothing more before it sends, and
sending cannot be undone. To stop, press Ctrl-C when `send` asks for the
mnemonic. The answer is the txid, since the transaction did not change after
you read it, then the portfolio that paid, with what it holds and can spend
now. With `--json`, the answer is the transaction as sent, as with
`broadcast --json`. It has two more keys: `synced`, with the sync of each
portfolio as `sync --json` gives one, and `syncerror`, which says why the sync
failed, or is null.

**The sync after the payment.** `hw` learns its coins from the server alone.
So `send` syncs the wallet once the payment is sent: the portfolio that paid,
and any other portfolio of the wallet that the payment pays, on the network of
the payment. The payment is then in [`txs`](#hw-txs) and no longer pending,
and its change is a coin of the wallet. The next payment can spend the change
at once, before it has a block: the wallet made that payment itself. The
lines below the txid give each of these portfolios with what it holds and what
it can spend, as [`portfolios`](#hw-portfolios) does.
[`sendall`](#hw-sendall), [`sweep`](#hw-sweep) and
[`broadcast`](#hw-broadcast) do the same. [`sync`](#hw-sync) covers the other
portfolios and the other networks.

When that sync fails, because the server stopped answering for instance, the
payment is still sent. `send` says so, names `sync` as the next step, and
exits as a success:

```console
Sent, txid 8d9d52e56cbaafc4f2705852018b2211faae911f125be96282a693c663215a1a

The transaction is sent, and the wallet could not sync after it: The store at
https://api.haskoin.com/ answered with an error hw could not read: HTTP 502

Next, sync the wallet to follow the transaction:
  hw sync
```

Until that `sync`, the payment stays [pending](#hw-pendingtxs) and marked as
sent. The wallet still lists the coins that the payment spent, which
[`coins`](#hw-coins) shows as `locked by tx`, and it does not have the change
yet. A payment made in between can find too few coins. `hw` then refuses it,
says what the payment sent took, and names the command that brings the
change:

```console
$ hw send 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ:0.0001 --fee-rate 10
Error: The coins do not cover the amount and the fee. A transaction sent from
this wallet took 0.00050000 btc of the portfolio on btc, in 1 coin: run hw sync
to get its change
```

**When the payment does not go through.** What `send` leaves in the wallet
depends on whether the transaction was signed.

- **Not signed: nothing is kept.** When `send` refuses the payment, when you
  answer no to a question, and when you stop at the mnemonic, the wallet is
  as it was. It has no new pending transaction, no coin is locked, and `hw`
  does not create a new tag that `--tag` named. When the mnemonic does not
  match the portfolio, `send` asks for it again, three times in all.
- **Signed and not sent: the transaction stays pending.** `send` keeps the
  signed transaction in the wallet before anything leaves the computer. When
  the sending fails, `send` says why, as `broadcast` does, and names the
  transaction by the [short id](#short-ids) of its txid. The transaction is
  then signed and pending, as it is after `signtx`. `broadcast` sends it
  again, and [`deletetx`](#hw-deletetx) deletes it and frees its coins.

```console
$ hw send ADDRESS:0.0002 --fee-rate 10
...
Error: The store at https://api.haskoin.com/ refused the transaction:
bad-txns-inputs-missingorspent. The transaction is signed and stays pending:
broadcast 8d9d52e5 sends it again, and deletetx 8d9d52e5 deletes it and frees
its coins
```

> **Good to know**
>
> - **`send` checks the server before it asks for the mnemonic.** A server
>   that does not answer stops it there, so you do not type the mnemonic for
>   nothing.
> - **`--yes` does not replace the mnemonic.** It answers yes to the three
>   questions that `preparetx` may ask: about a high fee rate, about a wallet
>   behind the chain, and about coins of different tags. `send` still asks
>   for the mnemonic. A script writes the mnemonic and the passphrase to the
>   standard input of `send`, one line each, as [Scripts](#scripts) shows.
> - **The transaction shown is not signed yet.** It reads `not signed` and has
>   a nosigHash, as the answer of `preparetx` does. The answer of `send`
>   gives its txid, once it is sent.
> - **If you interrupt `send` after the mnemonic, the transaction stays
>   pending.** Once you have typed the mnemonic, `send` signs the transaction
>   and keeps it. If you stop `send` while it waits for the server,
>   [`pendingtxs`](#hw-pendingtxs) shows the transaction as signed. When the
>   network took it, the next `sync` finds it and removes it from the pending
>   transactions. Otherwise send it with `broadcast`, or delete it with
>   `deletetx`.
> - **`send` syncs the portfolios of the payment alone.** It does not look
>   at the other networks or at the portfolios that the payment does not
>   touch, so it stays fast. On a terminal, the status line of
>   [`sync`](#hw-sync) shows while it works.
> - **`send` makes the payments that `preparetx` prepares, and no others.**
>   To send every coin in one command, use [`sendall`](#hw-sendall), which
>   does for `preparesendall` what `send` does for `preparetx`. A fee bump
>   still takes three steps: `bumpfee`, then `signtx` and `broadcast`.

#### `hw sendall`

`hw sendall ADDRESS[:AMOUNT]...` sends every coin the portfolio can spend on
the network, in one command. It prepares the transaction as
[`preparesendall`](#hw-preparesendall) does, shows it, asks for the mnemonic,
signs the transaction as [`signtx`](#hw-signtx) does, and sends it as
[`broadcast`](#hw-broadcast) does. The result is the same as when you run the
three commands one after the other with the same options.

```console
hw sendall ADDRESS --fee-rate low
hw sendall ADDRESS1:0.1 ADDRESS2 --fee-rate median --leave-dust
hw sendall ADDRESS --network bch --fee-rate low --label "closing the account"
hw sendall MYCOLDADDRESS --fee-rate low --tag cold   # tag the transaction as you make it
hw sendall MYSEGWITADDRESS --coin-type bip44 --fee-rate low
hw sendall ADDRESS --spend-from-tag kyc --fee-rate low   # empty the addresses of a tag
hw sendall ADDRESS --spend-from-tag kyc --coin-type bip44 --fee-rate low   # its legacy coins only
```

`sendall` does for `preparesendall` what [`send`](#hw-send) does for
`preparetx`. `send` and `sendall` differ only in the transaction they
prepare. After that, `sendall` does everything the way `send` does. Like
`send`, it needs the mnemonic and the server at the same time, so it runs on
an online computer. For funds in cold storage, use `preparesendall --output`
and [sign on the offline computer](#signing-on-an-offline-computer).

**The options** are those of [`preparesendall`](#hw-preparesendall), all but
`--output`, since `sendall` writes no file. They mean the same, and they build
the same transaction. An address with an amount gets that amount. The
addresses without one share the rest and pay the fee. Coin control, with its
choosers and its filters, `--spend-dust` and `--leave-dust` work as for
`preparesendall`.

**What you see.** `sendall` asks the questions of `preparesendall`: the fee
rate when you give no `--fee-rate`, and whether to spend the dust coins,
which are worth no more than the fee to spend them. It then shows the
transaction once, in the form of the answer of `preparesendall`. Below it,
`sendall` lists every coin that it leaves in the portfolio. It then warns
that typing the mnemonic signs the transaction and sends it, and asks for the
mnemonic:

```console
$ hw sendall 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU --fee-rate 10 --label "closing the account"
14 Nov 2026 23:21  Outgoing     -0.00050000 btc  not signed, 10 sat/vB
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  Label    : "closing the account"
  NoSigHash: 241f0958481c6758715bb1dae1c488199af406b239cb3839c5767a94f0fef691
  Short id : 241f0958
  Created  : 14 Nov 2026 23:21:57 CET
  Fee      : 0.00001920 btc, 10 sat/vB
  Lock time: block 800002
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0
  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00048080 btc

1 coin stays in the portfolio:

3b273748:1  14 Nov 2023 23:13      0.00030000 btc  locked by you, 3 confirmations
  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  bip44/0/1
  you locked it

Typing the mnemonic signs this transaction and sends it to the network. This
cannot be undone.
Mnemonic, or a share of a backup:
Passphrase, or nothing if the seed has none:
Sent, txid 747fbb653f5e226ed66c8b59d2d60c90c342dd6b641bd64f4a02b553c8303964

portfolio0  btc
  total 0.00030000 btc, spendable 0.00000000 btc, unspendable 0.00030000 btc, 1 coin
```

Check the transaction at that point: the addresses, the amounts, the fee, and
the coins that stay. Typing the mnemonic is the confirmation. `sendall` asks
nothing more before it sends, and sending cannot be undone. To stop, press
Ctrl-C when `sendall` asks for the mnemonic. The answer is the txid, then the
portfolio and what it holds now: here, the coin that you locked. `sendall`
syncs the wallet for that, as [`send`](#hw-send) does after a payment. With
`--json`, the answer is the transaction as sent.

**When the payment does not go through,** `sendall` leaves what `send`
leaves. A payment that was not signed leaves nothing. The wallet has no new
pending transaction, and no coin is locked. A transaction that was signed and
could not be sent stays signed and pending, as it is after `signtx`. The
error names it by the [short id](#short-ids) of its txid, so that you can
send it again with `broadcast` or delete it with [`deletetx`](#hw-deletetx).
The section of [`send`](#hw-send) says more about both cases.

> **Good to know**
>
> - **If you answer no to the question about the dust coins, `sendall` does
>   not stop.** It leaves those coins in the portfolio and sends the others,
>   as `preparesendall` does. The transaction shown next lists them among the
>   coins that stay. To stop at that point, press Ctrl-C when `sendall` asks
>   for the mnemonic.
> - **`--yes` does not answer the question about the dust coins, and does not
>   replace the mnemonic.** It answers yes to the questions about a high fee
>   rate, about a wallet behind the chain, and about coins of different tags.
>   `--spend-dust` or `--leave-dust` answers the question about the dust
>   coins. A script writes the mnemonic and the passphrase to the standard
>   input of `sendall`, one line each, as [Scripts](#scripts) shows.
> - **`sendall` sends what the portfolio can spend now.** A coin that is not
>   spendable yet and a locked coin stay, and so do the coins of the other
>   portfolios. Run [`sync`](#hw-sync) first, and read the list of the coins
>   that stay before you type the mnemonic.
> - **`sendall` checks the server before it asks for the mnemonic,** as `send`
>   does. If you interrupt it after the mnemonic, the transaction stays signed
>   and pending.

#### `hw preparetx`

`hw preparetx ADDRESS:AMOUNT...` prepares a payment without signing it, and
keeps it as a pending transaction. It needs no mnemonic. It spends the coins
of the [active portfolio](#the-active-portfolio) on one network, the one
`--network` names or the default one. To pay from another portfolio, switch to
it first. [`send`](#hw-send) takes the same options, and goes on to sign the
transaction and send it.

A recipient may be a contact of the [address book](#the-address-book). The Tab
key completes its address. The answer underlines that address and names the
contact after it, so that you see whom the payment goes to before you sign.

```console
hw preparetx ADDRESS:0.0005 --fee-rate median --label "rent of October"
hw preparetx ADDRESS:0.0005 --fee-rate median --tag rent --tag flat-2   # tag the payment as you make it
hw preparetx ADDRESS1:10000sat ADDRESS2:250bits --fee-rate 5
hw preparetx ADDRESS:0.01 --network bch --fee-rate low
hw preparetx ADDRESS:0.01 --coin-type bip44 --fee-rate low
hw preparetx ADDRESS:0.01 --spend-from MYADDRESS --change-back MYADDRESS --fee-rate low
hw preparetx ADDRESS:0.01 --change-back largest --fee-rate low   # back to the largest coin spent
hw preparetx ADDRESS:0.01 --change-back smallest --fee-rate low  # back to the smallest coin spent
hw preparetx ADDRESS:0.01 --spend-from MYADDRESS --select --fee-rate low
hw preparetx ADDRESS:0.01 --spend-from bip44/1/0 --fee-rate low   # an address by its path
hw preparetx ADDRESS:0.01 --spend-from-tag kyc --fee-rate low     # the coins at the addresses of a tag
hw preparetx ADDRESS:0.01 --spend-from-tag kyc --tag-change --fee-rate low   # and the change stays in the tag
hw preparetx ADDRESS:0.01 --spend-from-tag bank --change-back largest --fee-rate low   # and the change back to one of its addresses
hw preparetx ADDRESS:0.01 --recipients-pay --fee-rate high --output unsigned.psbt
```

The table marks the options of coin control as choosers or filters. A chooser
names coins to spend, and a filter narrows the coins that may be spent. Coin
control, below, explains both.

| Option | What it does |
|---|---|
| `--network NET` | Names the network of the payment. Without it, the network is that of the coins chosen with `--coin`, `--spend-from` or `--spend-from-tag`, or else the default network. With those options, you need it only to tell apart the test networks, which share their keys. |
| `--fee-rate RATE` | Sets the fee rate: a number of satoshis per virtual byte, or `low`, `median` or `high`. Without it, `hw` shows [the rates](#hw-fees) and asks. |
| `--recipients-pay` | Makes the recipients pay the fee. `hw` takes it out of the amounts paid, in equal shares. |
| `--min-conf N` | Filter: spends only the coins that have at least N confirmations, among every coin or among the chosen coins. The default is 0. |
| `--coin-type TYPE` | Filter: spends only the coins of one type, `bip44` or `bip84`, among every coin or among the chosen coins. Without it, the payment spends coins of both types. |
| `--strategy STRATEGY` | Sets how the coins are selected: `least-waste`, the default, `random-draw`, `largest-first` or `smallest-first`. |
| `--coin TXID:INDEX` | Chooser: adds a coin to the chosen coins. Give its txid in full or by its [short id](#short-ids). Repeat it for several coins. |
| `--spend-from ADDRESS` | Chooser: adds the coins at an address to the chosen coins. Give the address or its path, such as `bip44/1/0`. Repeat it for several addresses. |
| `--spend-from-tag TAG` | Chooser: adds to the chosen coins the coins at every address of the portfolio that has the [tag](#tags-on-addresses-and-transactions). Repeat it for several tags. It is the only way to spend the coins of a tag that [locks its coins](#hw-tag). |
| `--select` | Spends only the coins the payment needs among the chosen coins, as `--strategy` selects them. Without it, the payment spends every chosen coin. |
| `--change-back ADDRESS` | Sends the change to this address of the active portfolio instead of a new change address. Give the address or its path. |
| `--change-back largest` | Sends the change back to the address of the largest coin the payment spends. Only the coins that have the type of the change count. |
| `--change-back smallest` | Sends the change back to the address of the smallest coin the payment spends. Only the coins that have the type of the change count. |
| `--tag-change` | Tags the new change address with the [tags](#tags-on-addresses-and-transactions) that all the coins the payment spends share. Without it, the change address gets no tag. `hw` refuses it with `--change-back`. |
| `--change-type TYPE` | Sets the type of address the change goes to, `bip44` or `bip84`. It is the type of the new change address, or of the coin that `--change-back largest` or `smallest` picks. |
| `--output FILE` | Also writes the transaction to a [file](#transaction-files). |
| `--label LABEL` | Gives the transaction a label. The `--output` file carries the label, and the label stays on the transaction once the network has it. |
| `--tag TAG` | Puts a [tag](#tags-on-addresses-and-transactions) on the transaction, as [`tag`](#hw-tag) does afterwards. Repeat it for several tags. It does not choose the coins to spend: `--spend-from-tag` does. |
| `--yes` | Answers yes to the questions about a high fee rate, about a wallet behind the chain, and about coins of different tags. |

**What the payment is for.** `--label` and `--tag` record it, at the moment
you make the payment. A label is a text of your own, such as
`"rent of October"`. A [tag](#tags-on-addresses-and-transactions) is one word
that puts the payment in a group, such as `rent`.
[`txs --tag rent`](#hw-txs) then lists the payments of the group, and the
[journal](#hw-exportjournal) can file them under an account. `--tag` does
what [`tag`](#hw-tag) does afterwards with the hash of the transaction. `hw`
creates a tag that does not exist yet, and reads capital letters as small
ones. Repeat the option for several tags:

```console
$ hw preparetx ADDRESS:0.0005 --fee-rate median --tag "my rent"
Error: option --tag: "my rent" has a space: a tag is one word. Repeat --tag for
several tags, as in --tag my --tag rent.
...
```

The label and the tags stay with the transaction once it is signed, sent and
found by a `sync`. The `--output` file carries the label, and not the tags.
The tags stay in the wallet that prepared the transaction, and they are still
there when the signed file comes back.

**The coins.** `preparetx` selects the coins to spend among all the coins of
the portfolio on the network. So one transaction can spend `bip44` and `bip84`
coins together. You can move to native SegWit addresses and keep spending your
legacy coins, with no transfer between accounts first. A SegWit coin costs
less to spend, and the selection counts each coin at its own cost.

**Coin control.** Coin control lets you decide which coins the payment spends.
Its options are of two kinds, choosers and filters.

- **A chooser names coins to spend, and the choosers add up.** The choosers
  are `--coin`, `--spend-from` and `--spend-from-tag`. The chosen coins are
  those of every `--coin`, of every `--spend-from` and of every
  `--spend-from-tag`. The payment spends every chosen coin that can be spent,
  and no other coin, whatever `--strategy` says. With `--select`, the strategy
  selects among the chosen coins instead.
- **A filter narrows the coins that may be spent, and never adds a coin.** The
  filters are `--min-conf` and `--coin-type`. Without a chooser, a filter
  applies to every coin of the portfolio, and the strategy selects among the
  coins that pass it. With a chooser, a filter applies to the chosen coins
  only.

So `--coin-type bip44 --spend-from MYADDRESS` spends the coins at that address
only, not every `bip44` coin. `--coin-type bip44 --spend-from-tag kyc` spends
the `bip44` coins of the tag only, and leaves its `bip84` ones. To spend the
coins at two addresses, give `--spend-from` twice.

The two filters treat a chosen coin that they leave out differently. `hw`
refuses a coin of the other type than `--coin-type` when `--coin` names it,
or when it is at an address that `--spend-from` names, since the two options
disagree about it. `hw` does not refuse a tag for that reason. A tag names a
group, and the filter narrows the group as it narrows the coins of the
portfolio. A chosen coin that has too few confirmations for `--min-conf` is
not refused either. The payment does not spend it, and the answer lists it as
[`coins`](#hw-coins) shows it, with the reason.

**`--spend-from-tag TAG`** chooses the coins at every address of the portfolio
that has the [tag](#tags-on-addresses-and-transactions). You put a tag on an
address with [`tag`](#hw-tag). The option works as `--spend-from` does, for a
whole group of addresses at once. Use it to pay with one kind of coins only,
such as the coins that an exchange knows to be yours. Repeat it for several
tags, and the coins of all of them are chosen.
[`coins --tag TAG`](#hw-coins) lists the coins of a tag, and
[`balances --tag TAG`](#hw-balances) adds them up. As with every chooser, the
payment spends every coin of the tag that can be spent. Add `--select` to
spend only the coins the payment needs, and `--coin-type` to spend only the
coins of one type.

`hw` refuses a word that is not a tag. It also refuses a tag that no address
of the portfolio has on the network of the payment. You asked to spend
certain coins, and a payment built from other coins would not be the one you
asked for:

```console
$ hw preparetx ADDRESS:0.01 --spend-from-tag kyx --fee-rate low
Error: There is no tag kyx. hw tags lists the tags.
$ hw preparetx ADDRESS:0.01 --spend-from-tag rent --fee-rate low
Error: No address of portfolio0 of wallet0 has the tag rent: hw addresses --tag
rent lists those of the active portfolio
```

A tag can [lock its coins](#hw-tag), with `hw tag TAG --lock-coins`. A payment
then spends the coins at the addresses that have that tag only when
`--spend-from-tag` names the tag. Without it, the strategy selects among the
other coins, and `hw` refuses a payment that needs the locked coins. `--coin`
and `--spend-from` do not open the lock either. When they choose a coin that
a tag locks, the payment does not spend it, and the answer lists it with the
reason, which names the tag: `the tag savings locks it`. When no other coin
was chosen, `hw` refuses the payment, and gives the option that opens the
lock. An address can have two tags that lock. Both must then be named, and
the reason names each tag that the payment did not name:

```console
$ hw preparetx ADDRESS:0.0006 --fee-rate 10
Error: The coins do not cover the amount and the fee. The portfolio holds
0.00030000 btc more on btc, in 1 coin it cannot spend now: hw coins says why
$ hw preparetx ADDRESS:0.0002 --coin 3b273748:1 --fee-rate 10
Error: None of the coins chosen can be spent now:
3b273748d0c04a80d77cc5530af411b3b907480b5d25f1146ae684dd9ef753ab:1, the tag
savings locks it. Only a payment that names the tag spends its coins:
--spend-from-tag savings
```

A tag has an option of its own, `--spend-from-tag`. It is not one more kind
of word that `--spend-from` takes. Otherwise `hw` would look for a mistyped
address as a tag, and the error would be about a tag you never meant.

**Coins of different tags.** `preparetx` asks before a payment spends coins
that do not all have the same tags. A
[tag](#tags-on-addresses-and-transactions) names a group of coins, such as
`kyc` for the coins bought at an exchange that knows your name, and `p2p` for
the coins bought from a person. The selection picks coins by their value,
whatever their tags, so a payment may spend coins of both groups. A
transaction shows that its coins have one owner, so it joins the two groups
for good: whoever knows that one coin is yours knows that the others are
yours too. `preparetx` names each group, with its coins and what they are
worth, and asks before it prepares anything. Here the payment needs both
coins of the portfolio:

```console
$ hw preparetx 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0006 --fee-rate 10
The transaction spends coins that do not all have the same tags:
  [kyc]  1 coin  0.00050000 btc
  [p2p]  1 coin  0.00030000 btc
A transaction shows that its coins have one owner, so it joins these groups.
Prepare the transaction anyway? [y/N] y
17 Nov 2026 12:42  Outgoing     -0.00063740 btc  not signed, 10 sat/vB
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  NoSigHash: e0a970a1ad60c17f3df6a588e3fc7ebd41505b57fe79b0b7e620474c568f4451
  Short id : e0a970a1
  Created  : 17 Nov 2026 12:42:03 CET
  Fee      : 0.00003740 btc, 10 sat/vB
  Lock time: block 800002
  From     : 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  0.00030000 btc  bip44/0/1
             [p2p]
             1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0
             [kyc]
  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00060000 btc
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00016260 btc  bip44/1/0

Next, sign it:
  hw signtx e0a970a1
Or export it, to sign it on an offline computer:
  hw exporttx e0a970a1 FILE
```

- **The tags must be the same.** The coins without a tag are a group too.
  When only your exchange coins are tagged `kyc`, `preparetx` asks about a
  payment that spends one of them with another coin, and shows the other
  coin under `no tag`. Coins tagged `kyc cold` and coins tagged `kyc` are two
  groups as well, since `hw` cannot know which tag says where a coin came
  from. `preparetx` asks nothing when the coins all have the same tags, or no
  tag at all.
- **`--spend-from-tag` answers for the tags it names.** With
  `--spend-from-tag kyc`, you chose the group yourself. `preparetx` then asks
  only when a coin that the payment spends does not have `kyc`, as when
  `--coin` adds one. It asks nothing about the other tags of the `kyc`
  coins, such as `cold` on one of them. With two tags, as in
  `--spend-from-tag kyc --spend-from-tag p2p`, the payment spends the coins
  of either tag, so `preparetx` asks unless every coin has both.
- **If you answer no, `preparetx` prepares nothing.** `hw` exits with an
  error, and the wallet is as it was. Then choose the coins yourself:
  `--spend-from-tag kyc` pays from one group alone, and `--coin` and
  `--spend-from` name the coins or their addresses.
- **`--yes` answers the question in advance**, for [scripts](#scripts). The
  transaction prepared still shows the tags under each address it spends
  from.
- **Every command that builds a payment asks this question.**
  [`send`](#hw-send), [`sendall`](#hw-sendall) and
  [`preparesendall`](#hw-preparesendall) ask as `preparetx` does.
  [`bumpfee`](#hw-bumpfee) asks when the coins it adds do not have the tags
  of the coins the payment spends already.

**The change.** The change goes to a new change address of the portfolio. Its
type is the first of these that applies:

1. The type `--change-type` asks for.
2. The type of the coins the payment spends, when they are all of one type. A
   payment then stays within one type of address.
3. The `default-type` of the configuration, or `bip44`, when the coins are of
   both types.

On a network of Bitcoin Cash, every coin is `bip44`, so the change is too.
`hw` refuses an option that asks for `bip84` there.

**The tags of the change.** The new change address has no
[tag](#tags-on-addresses-and-transactions), whatever tags the coins spent
have. With `--tag-change`, it gets the tags that all the coins the payment
spends share. A coin has the tags of its address. The change of a payment
that spends the coins of one group then stays in that group. This is an
option, and not what every payment does, because only you know what a tag
means. `kyc` says where the coins came from, and that is as true of their
change. `donations` on a receiving address says what the address is for, and
the change of a donation you spend is not a donation. In this example, the
only coin of the portfolio is at an address tagged `kyc`:

```console
$ hw preparetx 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0002 --tag-change --fee-rate 10
16 Nov 2026 21:52  Outgoing     -0.00022260 btc  not signed, 10 sat/vB
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  NoSigHash: cb585cb95038fcbc1323b1133f2f4a934c601de030eaa98987172bb402794b1c
  Short id : cb585cb9
  Created  : 16 Nov 2026 21:52:25 CET
  Fee      : 0.00002260 btc, 10 sat/vB
  Lock time: block 800002
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0
             [kyc]
  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0
             [kyc]

Next, sign it:
  hw signtx cb585cb9
Or export it, to sign it on an offline computer:
  hw exporttx cb585cb9 FILE
```

- **Every coin must have the tag.** With coins tagged `kyc cold` and `kyc`,
  the change is tagged `kyc`. With a coin tagged `kyc` and a coin without a
  tag, the change gets no tag. The payment mixes the group with other coins,
  so `hw` cannot say that its change belongs to the group. It then says
  `--tag-change put no tag on the change address: the coins the payment spends share no tag`.
- **With or without coin control.** `--spend-from-tag kyc` spends coins that
  all have the tag, so the change gets it. In a payment that selects among
  every coin, the change gets the tags that the selected coins happen to
  share.
- **A new change address only.** The address that `--change-back` names keeps
  the tags it has, so `hw` refuses `--tag-change` with `--change-back`.
- **The payments made before.** [`tag --with-change`](#hw-tag) tags the change
  of the payments that already spent from an address, by the same rule.
- **You can change the tags afterwards.** [`untag`](#hw-untag) takes a tag off
  the change address, and [`tag`](#hw-tag) puts another on it.
- **Deleting the payment takes the tags off.** [`deletetx`](#hw-deletetx)
  frees the change address for the next payment, and takes its tags off.

**`--change-back ADDRESS`** sends the change to that address instead of a new
change address. Use it to keep the funds at an address you chose: some banks
accept payments only from addresses they have checked. The address must be an
address of the active portfolio on the network of the payment. It can be a
receiving address or a change address, of either type. It may have been used
already, or be the change address of another pending transaction, since you
chose it. A new change address that `hw` takes by itself is always a free
one. `hw` refuses any other address, since change sent elsewhere would be a
payment. The address has a type of its own, so `--change-back ADDRESS` and
`--change-type` do not go together.

Every form of `--change-back` sends the change to an address you already
have. That address keeps its tags, and gets no new tag.

**`--change-back largest`** sends the change back to the address of the
largest coin the payment spends. You do not know in advance which coins the
strategy will select, so you cannot name such an address yourself. Use it to
keep the change at an address that the payment already reveals, instead of
revealing a new one.

- **The coin has the type of the change.** The type of the change follows the
  rule above, and the change goes to the address of the largest coin of that
  type. `hw` counts the fee for an output of that type, so the change cannot
  go to an address of the other type. In a payment that spends coins of one
  type, and on Bitcoin Cash, the coin is the largest coin spent. In a payment
  that spends both types, it is the largest coin of the `default-type`, even
  when a coin of the other type is larger.
- **`--change-type` sets the type.** `--change-back largest --change-type
  bip84` sends the change to the largest `bip84` coin spent. When the payment
  spends no `bip84` coin, `preparetx` refuses it and prepares nothing. Add
  `--coin-type bip84` to spend only `bip84` coins, or leave out
  `--change-type`.
- **Coins of equal value.** `hw` takes the coin at the lower path: a
  receiving address before a change address, then the lower index.
- **A payment without change** sends nothing back, and `hw` never refuses it
  for that.
- **With `--spend-from-tag`, the change goes back to an address that has the
  tag.** Every coin spent is at an address that has the tag, so the largest
  coin is too. `--spend-from-tag bank --change-back largest` pays from the
  addresses a bank has checked, and keeps the change at one of them.

**`--change-back smallest`** works the same way with the smallest coin. It
sends the change back to the address of the smallest coin the payment spends,
by the same rules. `largest` keeps the funds at the address that held the
most. `smallest` adds to the address that held the least.

**An address by its path.** `--spend-from` and `--change-back` take the path
of an address of the active portfolio in place of the address. Write the path
as the views show it, such as `bip44/1/0`. The path needs its type, since a
payment spends both types and `preparetx` has no `--type`. The portfolio must
have the address already: `addresses` and `addresses --change` list its
addresses. Like an address, a path tells `preparetx` the network of the
payment: the networks where the portfolio has an address at that path.

> **Good to know**
>
> - **An amount without a unit is in bitcoins**, or in the unit of the
>   configuration. See [Amounts](#amounts).
> - **Nodes do not relay an output that is too small.** The limit is 546
>   satoshis for a legacy output, and 294 for a native SegWit one. `hw`
>   refuses a payment below the limit, and says what the recipient would get
>   and the limit for its kind of address. Change below the limit goes to
>   the fee.
> - **A transaction takes at most 100,000 virtual bytes**, the largest that
>   nodes relay. That is about 675 legacy coins, or 1,470 native SegWit ones.
>   `hw` refuses to prepare a larger one. Send the coins in several
>   transactions, choosing them with `--coin`, `--spend-from` or
>   `--spend-from-tag`.
> - **`hw` asks before it prepares a payment when the wallet is behind the
>   chain.** The transaction gets the lock time of the best block the wallet
>   knows, as the transactions of most wallets do. When that block is more
>   than eight hours old, `preparetx`, `send`, `preparesendall`, `sendall` and
>   `bumpfee` ask whether to go on with a lock time of 0, or to sync first.
>   Running `sync` first is the better choice: it also finds the coins spent
>   or received since. `--yes` goes on without asking.
> - **`--coin` and `--spend-from` take the coins of the active portfolio.**
>   `hw` refuses a coin or an address of another portfolio. The error names
>   the portfolio that holds it and the `switch` command that makes it
>   active, as in `The coin a1075db5:1 is in portfolio1 of wallet0, not in the
>   active portfolio: run switch 1 --wallet 0 first`. Every payment spends
>   from one portfolio, and coin control stays in the active one. A tag
>   exists in the whole database, so `--spend-from-tag` takes the addresses
>   of the active portfolio that have the tag, and leaves those of the other
>   portfolios.
> - **`--coin` and `--spend-from` set the network of the payment.** The chosen
>   coins and addresses are on one network, and `preparetx` pays on that
>   network. A path counts for each network where the portfolio has an
>   address at that path, and a tag for each network where the portfolio has
>   an address with that tag. When you give `--network`, it must agree with
>   them. You need `--network` when the coins are on several test networks
>   and none of them is the default network.
> - **The type of the change and the coins depend on each other.** The fee
>   depends on the type of the change output, and the type of the change
>   depends on the coins selected. When there are coins of both types to
>   select from, `hw` first selects for change of the default type. When the
>   coins it selected are all of the other type, it selects again among the
>   coins of that type.
> - **Anyone can recognize change that goes back to an address the payment
>   spends from.** `--change-back largest` and `smallest` always send it
>   there, and so does `--change-back` with such an address. The address is
>   on both sides of the transaction, so anyone who reads the transaction
>   knows which output is your change. With a new change address, nobody can
>   be sure which output is the change. `--tag-change` still keeps that new
>   address in the group of its coins, by its tags.
> - **`--tag` does not choose the coins to spend.** It says what the payment
>   is for. `--spend-from-tag` chooses the coins at the addresses that have a
>   tag, and puts no tag on the transaction. A payment tagged `rent` still
>   selects among every coin of the portfolio.
> - **A refused payment creates no tag.** When `preparetx` prepares nothing,
>   `hw` does not create a new tag that `--tag` named, and
>   [`tags`](#hw-tags) does not list it.
> - **The change of a payment that mixes groups leaves the groups.** With
>   `--tag-change`, the change gets only the tags that all the coins spent
>   share, and a coin from outside a group does not have the tag of the
>   group. This is the kind of payment that `preparetx` asks about before it
>   prepares it. To keep a payment and its change within one group, use
>   `--spend-from-tag` with `--tag-change`.

#### `hw preparesendall`

`hw preparesendall ADDRESS[:AMOUNT]...` prepares a payment of every coin the
portfolio can spend on the network, of both types of address, with no change.
That is the spendable balance `portfolios` shows for that network. An address
with an amount gets that amount. The addresses without one share the rest and
pay the fee. [`sendall`](#hw-sendall) takes the same options, and goes on to
sign the transaction and send it.

```console
hw preparesendall ADDRESS --fee-rate low
hw preparesendall ADDRESS1:0.1 ADDRESS2 --fee-rate median --leave-dust
hw preparesendall ADDRESS --network bch --fee-rate low
hw preparesendall ADDRESS --spend-from-tag kyc --fee-rate low   # empty the addresses of a tag
hw preparesendall MYSEGWITADDRESS --coin-type bip44 --fee-rate low
```

A dust coin is a coin worth no more than the fee to spend it. `hw` asks
whether to spend the dust coins, unless `--spend-dust` or `--leave-dust`
answers in advance. It shows each dust coin as [`coins`](#hw-coins) does,
then the fee rates at which the coin is worth spending.

Every coin sent goes into one transaction. When the coins do not all have the
same [tags](#tags-on-addresses-and-transactions), `hw` names each group and
asks before it prepares anything, as [`preparetx`](#hw-preparetx) does, unless
`--yes` answers in advance. `--spend-from-tag` sends the coins of one tag.

`--network`, `--fee-rate`, `--output`, `--label`, `--tag` and `--yes` work as
for [`preparetx`](#hw-preparetx). So does coin control, which limits the coins
sent. Its options are of two kinds:

- **A chooser names coins to send.** The choosers are `--coin`,
  `--spend-from` and `--spend-from-tag`, and they add up. With a chooser,
  `preparesendall` sends only the chosen coins: every one that can be spent,
  dust coins included. The coins of `--coin` and `--spend-from` must be in
  the active portfolio. They set the network of the payment, and `--network`
  must agree with them.
- **A filter narrows the coins sent, and never adds a coin.** The filters are
  `--min-conf` and `--coin-type`.

The filter `--coin-type` sends the coins of one type only. The last example
moves every legacy coin of a portfolio to a native SegWit address of the same
portfolio, one that `receive --type bip84` gave. The chooser
`--spend-from-tag` sends only the coins at the addresses that have a
[tag](#tags-on-addresses-and-transactions), which empties a group of
addresses. With both, as in `--spend-from-tag kyc --coin-type bip44`,
`preparesendall` sends the `bip44` coins of the tag and leaves its `bip84`
ones.

The answer lists every coin that the transaction leaves in the portfolio, as
`coins` shows it, then the reason: dust, locked by you, by a tag or by a
pending transaction, not spendable yet, not chosen with `--coin`,
`--spend-from` or `--spend-from-tag`, or not of the type `--coin-type` sends.
With `--json`, each coin left has the keys of a coin of `coins --json`, and
`reason` beside them. `lockingtags` lists the tags that lock the coin against
this payment, and is empty for every other reason.

#### `hw signtx`

Signs a pending transaction, or a transaction file. Name a pending transaction
by its nosigHash, in full or by its [short id](#short-ids). `signtx` first
shows the transaction, as [`show`](#hw-show) does. It then asks for the
mnemonic and the passphrase of the wallet, or for the shares of its backup.
Check what it shows before you type the mnemonic, and stop it with Ctrl-C if
it is not the payment you mean. Once the transaction is signed, `signtx`
prints the txid and the file it wrote. `signsweep` shows the transaction in
the same way before it asks for the keys. A script can give the master private
key of the wallet in place of the mnemonic and the passphrase: see
[Scripts](#scripts).

```console
hw signtx NOSIGHASH
hw signtx --input unsigned.psbt --output signed.psbt
```

Without `--output`, `signtx` offers to replace the `--input` file with the
signed transaction, since the unsigned file is of no use once it is signed.
It asks this before it asks for the mnemonic. No option answers this
question, so a script must give `--output`. Otherwise the question takes the
line of the mnemonic as its answer, reads it as a no, and `signtx` stops.

One mnemonic signs a transaction that spends `bip44` and `bip84` coins
together, since it gives the keys of both accounts.

`signtx` signs the coins of the wallet, and asks for the mnemonic only when
it has something to sign. It refuses a [sweep](#hw-preparesweep), which
spends no coin of the wallet, and names the command that signs it:
`hw signsweep` with the same hash or the same file. It also refuses a
transaction that spends a coin from outside the wallet beside coins of the
wallet, while that coin is not signed: the other signer signs first, and
`signtx` last.

`signtx` refuses a transaction that spends a locked coin, before it asks for
the mnemonic: a coin you locked with [`lockcoin`](#hw-lockcoin), or a coin
that a [tag locks](#hw-tag), unless the payment named the tag with
`--spend-from-tag`. This holds for a pending transaction and for a file. The
refusal names the coin, the lock, and the command that unlocks the coin.

`signtx` finds the portfolio from the transaction, whichever portfolio is
active. When a transaction moves funds between two portfolios, the portfolio
that pays signs it. `signtx` then shows the transaction as that portfolio
sees it, and says so when that portfolio is not the active one. When
`signtx` cannot tell between several portfolios, it takes the active one if
it is among them. Otherwise it names them, and you switch to the one to use.
[What each command takes](#what-each-command-takes) gives the rule.

On an offline computer, `signtx` refuses a file of a portfolio that the
database does not have: "The transaction does not involve this wallet". That
happens on a computer where you made `portfolio0` alone, when you give it a
file of `portfolio1`. There, `newportfolio --existing` adds the next
portfolio of the mnemonic, and `importwallet` adds the portfolios of a file
of the whole wallet.

#### `hw broadcast`

Sends a signed transaction to the network. Sending cannot be undone, so
`broadcast` first shows the transaction and asks. After it sends, it waits up
to ten seconds for the server to list the transaction. `--yes` sends without
showing the transaction and without asking first, for scripts.

```console
hw broadcast TXID
hw broadcast --input signed.psbt
```

A file may hold a transaction that the wallet does not hold as pending, such
as a payment that another computer with the same wallet prepared.
`broadcast --input` finds its portfolio as [`show`](#hw-show) does, shows the
transaction as that portfolio sees it, and sends it on the network of that
portfolio.

Once the server lists the transaction, `broadcast` syncs the wallet, as
[`send`](#hw-send) does: the portfolio that paid, and any other portfolio of
the wallet that the transaction pays, on the network of the transaction. The
payment is then in [`txs`](#hw-txs) and no longer pending, and its change is
there for the next payment. `broadcast` ends with these portfolios, and what
each holds and can spend:

```console
$ hw broadcast 89c70fb2 --yes
22 Nov 2026 23:15  Outgoing     -0.00022260 btc  sent, 10 sat/vB
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  Txid     : 89c70fb28c9c6d302c3c623e1050e0e1e357686c4101d905f65772d2f274db14
  Short id : 89c70fb2
  Created  : 22 Nov 2026 23:15:34 CET
  Fee      : 0.00002260 btc, 10 sat/vB
  Lock time: block 800002
  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0
  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc
  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0

portfolio0  btc
  total 0.00027740 btc, spendable 0.00027740 btc, 1 coin
```

When that sync fails, the transaction is still sent. `broadcast` says so,
names [`sync`](#hw-sync) as the next step, and exits as a success. The
transaction stays pending and marked as sent until a sync finds it.

The server hands the transaction to a node. When the server can tell that
no node will accept the transaction, it says why, as for a coin that is spent
already. `broadcast` then fails with what the server said, and the
transaction stays signed and not sent:

```console
$ hw broadcast 3f2a9c1e --yes
The store at https://api.haskoin.com/ refused the transaction:
bad-txns-inputs-missingorspent
```

When a node refuses the transaction and says why, the server passes the
reason on, as for a fee under what the network relays:

```console
$ hw broadcast 3f2a9c1e --yes
The store at https://api.haskoin.com/ refused the transaction: A node refused
the transaction: min relay fee not met, 2260 < 4520
```

The server cannot always tell. A node of Bitcoin gives its reason over its
RPC alone, which the operator of the server may connect or not. Without it,
the node says nothing of a transaction that it refuses for its fee, and the
server answers as for a transaction that the node took. So `broadcast` waits
ten seconds for the server to list the transaction, and fails when it does
not:

```console
The transaction was handed to the network but the store has not seen it after
10 seconds. It stays signed and not online: run broadcast again later
```

> **Good to know**
>
> - **`broadcast` refuses a transaction that spends a locked coin.** That is
>   a coin you locked with [`lockcoin`](#hw-lockcoin), or a coin that a
>   [tag locks](#hw-tag), unless the payment named the tag with
>   `--spend-from-tag`. It holds for a pending transaction and for a file,
>   with `--yes` too, and `broadcast` refuses before it asks. A lock that
>   comes after a transaction was signed still stops it here.
> - **`sync` sends again a transaction you sent, when the server has lost
>   it.** That is a transaction that is still pending: the sync after its
>   sending failed. When the transaction is not on the network after that,
>   [`sync`](#hw-sync) says so on every run, with what the server said or
>   with the fact that the server does not list it, until you delete the
>   transaction with [`deletetx`](#hw-deletetx). That happens to a
>   transaction that the network refuses for good, for example because its
>   fee is too low to relay.
> - **A payment that `sync` has seen is no longer pending.** `broadcast` has
>   nothing to send for it, and says so. [`txs`](#hw-txs) shows its state. On
>   Bitcoin, [`bumpfee`](#hw-bumpfee) replaces a payment that stays
>   unconfirmed.
> - **The server keeps a transaction that its nodes dropped.** A haskoin-store
>   holds an unconfirmed transaction for days after the nodes forgot it.
>   `txs` shows the payment as unconfirmed for that time. `hw` does not send
>   it again, since `sync` saw it. On Bitcoin, replace it with `bumpfee`. On
>   Bitcoin Cash, wait for the server to drop it. `txs` then shows it as
>   dead, its coins are spendable again, and you can prepare the payment
>   again.

#### `hw bumpfee`

`hw bumpfee TXID` prepares a replacement for an unconfirmed payment. The
replacement pays a higher fee. Use it when a payment is stuck because its fee
is too low. It works on Bitcoin only, since Bitcoin Cash does not replace
transactions. Sign and send the replacement like any payment.

```console
hw bumpfee TXID --fee-rate high
hw signtx NOSIGHASH
hw broadcast TXID                           # the txid of the replacement
hw bumpfee TXID --fee-rate high --yes       # asks nothing, in most cases
hw bumpfee TXID --fee-rate high --recipients-pay   # the recipients pay it
```

The extra fee comes out of the change. When the change cannot pay it, `hw`
offers to add confirmed coins of the portfolio, of either type. It shows the
coins it offers to add as [`coins`](#hw-coins) does. When no coin can be
added, or when you decline them, `hw` offers to take what is missing from
what the recipients receive, and asks which of them pays. `bumpfee` adds no
coin that a payment could not spend, such as a coin that you or a tag locked.
When it has no coin to offer and the portfolio holds such coins, it says how
much they are worth, so that you can unlock one of them instead of making a
recipient pay.

`--yes` takes the first of these that can pay:

1. the change;
2. confirmed coins of the portfolio, added;
3. a lone recipient, who receives less;
4. all the recipients, in equal shares, the first one also paying the
   remainder.

When equal shares would leave a recipient below the dust limit, `hw` still
asks which one pays alone. `--yes` picks no recipient.

With `--recipients-pay`, the recipients pay the extra fee, in equal shares,
as they pay the whole fee with
[`preparetx --recipients-pay`](#hw-preparetx). The change keeps its amount,
and `hw` adds no coin, even with `--yes`. `hw` does not ask who pays.

`bumpfee` finds the portfolio from the transaction, whichever portfolio is
active, as `signtx` does. It is the portfolio that holds the transaction.
When several portfolios hold it, it is the one that pays it. When `bumpfee`
cannot tell, it takes the active portfolio if it is one of them. Otherwise it
names them, and you switch to the one that pays.

`--output FILENAME` also writes the replacement to a new file, a PSBT, as
`preparetx --output` does. `--yes` also answers the other questions: about a
high fee rate, about a wallet behind the chain, about the payments that spend
the change, and about coins of different tags.

While the replacement is pending, it locks the coins of the stuck payment:
its change, and what it paid another portfolio of the wallet. The network
keeps only one of the two transactions. Once the replacement is sent, the
stuck payment is gone, and its coins with it. A payment that spent one of
them would be removed too, and its recipient would never be paid. So `hw`
spends none of them: [`coins`](#hw-coins) shows them `locked by tx`, and a
payment that names one with `--coin` is refused:

```console
$ hw preparetx ADDRESS:0.0004 --fee-rate 10 --coin e9be205d:1
Error: None of the coins chosen can be spent now:
e9be205d737e395af2f7ad06e5a9a2485bf394ffa490ebff9602fe25cda2df36:1, a pending
transaction replaces the payment it comes from
```

[`deletetx`](#hw-deletetx) deletes the replacement and frees those coins.
They are also free again when a block confirms the stuck payment before you
send the replacement: nothing replaces a confirmed payment, so delete the
replacement.

Once the replacement is sent, [`txs`](#hw-txs) shows the stuck payment as
dead, and names the replacement on it: `replaced by 2b69e4f4`. To raise the fee again, give `bumpfee` the txid of the
replacement. `bumpfee` refuses the txid of the dead payment, and names the
replacement to bump instead.

The replacement takes the [tags](#tags-on-addresses-and-transactions) of the
payment it replaces, as it takes its label. It is the same payment, so you find
it under the same tag whichever of the two the network keeps.

The change keeps its address, and that address keeps its tags: `bumpfee` puts
no tag on an address and takes none off. When `bumpfee` adds a coin from
outside the group of the change, you can take the tag off the change address
with [`untag`](#hw-untag). When the payment had no change and the
replacement has some, the change goes to a new change address, which has no
tag.

A coin that `bumpfee` adds may not have the same tags as the coins the
payment spends already. It is then of another group, and the replacement
joins the two groups, as a [payment that spends both](#hw-preparetx) does.
`bumpfee` names the groups and asks once more, after the question about the
coin, before it replaces anything. It does not ask again about the groups
that the payment joined when you prepared it.

> **Good to know**
>
> - **Change sent back with `--change-back` may count as a payment.** Change
>   sent back to an address the payment spends from is still change, and pays
>   the extra fee. `--change-back largest` and `smallest` always send it to
>   such an address. `hw` cannot tell change sent to another receiving
>   address from a payment to yourself. The replacement keeps it, and does
>   not take the extra fee out of it.

### Transactions

#### `hw txs`

Lists the transactions of the active portfolio, grouped by network, as of the
last sync. `--network` shows one network only. On each network, the
transactions are listed in the order of the chain, the newest first. The
unconfirmed ones come first, each above the ones it spends from. The confirmed
ones follow, by their block and their place in it. The balance of the
portfolio changed in that order. So the balance beside each transaction is a
balance that the portfolio really held. `--reverse` lists the same rows from
the last to the first: the oldest transaction comes first and the unconfirmed
ones last, each with the same balance. [`show TXID`](#hw-show) shows one
transaction in full.

**Before the first sync.** Until the portfolio has synced on a network, `hw`
knows nothing about its addresses there, and has no transaction to list. When
the portfolio has addresses on such a network, the listing shows one line for
it, in every view, and no transaction:

```console
$ hw txs
portfolio0  btc  not synced
```

`not synced` does not mean that nobody paid the addresses
([`addresses`](#hw-addresses) says more). With `--json`, the group of such a
network has the key `synced`, which is then `false`, and no transaction. A
`--csv` file has no row for it. A network where the portfolio has no address
has no line.

`txs` shows a transaction as the portfolio sees it, whichever accounts of the
portfolio it touches. A transaction that spends `bip44` and `bip84` coins
together is one transaction, with one amount. In the compact view, each
transaction takes two lines:

1. Its [short id](#short-ids), the first 8 digits of its txid, then its time,
   in the time zone of the computer, its amount and its status.
2. The balance of the portfolio on the network after the transaction, then what
   the transaction is for, then its
   [tags](#tags-on-addresses-and-transactions).

The second line says what the transaction is for in the words below. The last
column of the table uses the same words.

- A payment received names the address that was paid, by its type and its
  path. Then comes the label of the payment, or else the label of the address,
  as in `at bip84/0/3 "Salary"`. A change address has no label, and says
  `change address` instead.
- A payment you made gives its label, or else the address it pays, as in
  `to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU`.
- A payment to a contact of the [address book](#the-address-book) always gives
  the address, underlined on a terminal. Then comes the label of the payment,
  or else the label of the contact, as in
  `to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU "Landlord"`. When the contact has no
  label either, the line says `address book` there.
- A payment to another portfolio of the wallet names the address it pays by
  its path and its portfolio, then gives its label, as in
  `to bip84/0/5 of portfolio1 "Savings"`. See
  [Paths of another portfolio](#paths-of-another-portfolio).
- When the transaction pays other addresses too, the line says how many, as in
  `and 2 more`.

As in every view, the type comes before the path, because each account of the
portfolio on a network has an address at `/0/3`. `txs` cuts nothing in the
line, not even a long label or address. On a terminal, the addresses of the
wallet and their labels are blue, which sets them apart from the addresses of
others. The tags of the transaction end the line, each between brackets, as in
`balance 0.00050000 btc, at bip44/0/0, [salary]`.

The amount has a plus sign when funds come in. The status counts the
confirmations up to `999+ confirmations`. An unconfirmed transaction shows its
fee rate too: its fee over its size, in satoshis per virtual byte. A rate that
is not a whole number shows one decimal, as in `8.6 sat/vB`. The decimal is
cut, not rounded up, so a rate just below 1 never reads as 1, which is the
lowest rate many nodes take.

The balance beside the newest transaction is what the portfolio holds on the
network now, confirmed or not. A dead transaction is one that was replaced, or
dropped from the network. It shows no balance, and the balances do not count
it.

When another transaction replaced a dead transaction, the second line of the
dead one starts with the short id of its replacement. Here
[`bumpfee`](#hw-bumpfee) replaced a payment with one that pays a higher fee,
and the replacement waits for a block:

```console
2b69e4f4  14 Nov 2026 23:13     -0.00024520 btc  unconfirmed, 20 sat/vB
  balance 0.00025480 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
89c70fb2  14 Nov 2026 23:02     -0.00022260 btc  dead
  replaced by 2b69e4f4, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
```

The note `replaced by` tells you whether a dead payment went out in another
transaction or never went out. `hw` does not store the note. It reads it from
the coins each time it shows the transaction: the replacement is the live
transaction that spends a coin the dead one spent. A dead payment of the
portfolio that names no replacement was dropped. See Good to know below for
what the note cannot tell.

`--verbose` starts each transaction with its time, then its type:

| Type | Meaning |
|---|---|
| `Incoming` | Funds enter the portfolio. |
| `Outgoing` | Funds leave the portfolio. |
| `Internal` | Funds move inside the portfolio, from a `bip44` address to a `bip84` address for example. |

The type is relative to the portfolio, not to an account. A move between the
two accounts of a portfolio does not change what the portfolio holds, so it is
internal. A payment from `portfolio0` to `portfolio1` is outgoing in
`portfolio0`, and incoming in `portfolio1`. A transaction file that involves
no portfolio of the wallet has a type of its own, `External`: see
[`show`](#hw-show).

The verbose view then shows:

- the network, and the types of the addresses of the portfolio that the
  transaction involves;
- the label and the tags of the transaction, as fields;
- the txid in full, then its short id;
- every address in full, with its amount, and with its path when it is an
  address of the wallet;
- the fee, the balance, and the full count of the confirmations.

The line below an address of the wallet gives the label and the tags of that
address. For a change address, it says `change address` instead. An address of
the [address book](#the-address-book) is underlined, and the line below it
gives the label and the tags of its contact. Below an address that the
transaction spends from, the line adds the labels of the coins spent, as
`coins` shows them, after the word `from`. Before you sign, these labels tell
you which coins a payment spends, and so which earlier payments it links. A
long label wraps at the edge of the screen, and each of its lines starts under
the address.

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
`999+`. An unconfirmed transaction has 0, and a dead one says `dead`. The
`Tags` column gives the tags of the transaction. The last
column, Description, says what the transaction is for, in the words of the
compact view. The description of a dead transaction that was replaced starts
with its replacement, as in `replaced by 2b69e4f4, to 1JCq…`. The header names
the unit of the amounts, so the cells hold numbers alone. The table leaves out
the fee rate. `txs` without `--tabular` shows it.

**Tags.** A transaction has tags of its own, which [`tag TXID`](#hw-tag) puts
on it, and its addresses have theirs. The two say different things. The tag of
a transaction says what the payment was, such as `rent`. The tag of an address
says which group its coins are in, such as `kyc`. The compact view and the
table show only the tags of the transaction, because a transaction of five
addresses would otherwise fill its line with their tags. `--verbose` shows
both.

Three options filter the listing:

- `--tag TAG` shows only the transactions that have the tag.
- `--address-tag TAG` shows only the transactions that spend from or pay an
  address of the portfolio that has the tag. It also shows those that pay a
  contact of the [address book](#the-address-book) that has the tag, or that
  such a contact paid.
- `--untagged` shows only the transactions left to sort: those that have no
  tag, and that neither pay a contact that has a tag nor were paid by one.

You can repeat the first two, and combine the three. The listing then shows
the transactions that match any one of them. Its first line names the tags, as
in `Transactions tagged salary, or at addresses tagged kyc`. With
`--address-tag`, the compact view and the table also name each address that
has one of the tags you asked for, so that you see why the transaction is in
the listing. They name an address of the portfolio by its path, with that tag
beside it. They name a contact by its address, underlined, with its label and
that tag.

When the second line of the compact view already names the address, the tag
follows it there, as in `balance 0.00050000 btc, at bip44/0/0 [kyc]`. Any
other address takes a line of its own below, so that the second line stays
short:

```console
$ hw txs --network btc --address-tag rent
Transactions at addresses tagged rent

...
  balance 0.00050000 btc, at bip44/0/0
  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU "Landlord" [rent]
```

A row of the table is one line, so these addresses go in its Description
cell, after a comma, as in
`at bip44/0/0, 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU "Landlord" [rent]`.

`--untagged` is for a regular review, at the end of the month for example. It
lists the transactions that no tag sorts yet, and its first line says which
transactions those are:

```console
$ hw txs --network btc --untagged
Transactions without a tag or a tagged contact

...
  balance 0.00080000 btc, at bip44/0/1
```

Tag what you recognize, with [`tag`](#hw-tag) on the transaction or on its
contact, and list again. The listing gets shorter until it is empty. The tag
of a contact counts, because it says what the payments of the contact are for:
tag your landlord `rent` once, and no rent is left to sort. The tag of an
address of the portfolio does not count. It says which group the coins are in,
not what a payment is for, so a payment from a `kyc` address is still to sort.

A filter leaves transactions out, and does not change the ones it shows. Each
still shows the balance of the portfolio after it, as in the whole listing.
`--json` gives the tags of a transaction under `tags`, and those of its
addresses under `addresstags`, by address.

`--csv FILE` writes the transactions to a [CSV file](#listings). Each row
starts with the wallet, the portfolio and the network of the transaction, and
the types of address it involves. Then come its txid, in full and by its short
id, its time, its direction, amount, fee, fee rate and size, the balance after
it, its status, confirmations and block, its label and its tags. `Replaced by`
gives the txid of the transaction that replaced a dead one, in full.

A transaction can spend from any number of addresses and pay any number, but a
file has a fixed number of columns. So the file names three addresses of each
transaction: the one it spends the most from, the one it pays the most, and
its change. `--verbose` lists them under From, To and Change. Each has its
value, and its path, label and tags when it is an address of the portfolio.
`Other addresses` counts the addresses that have no column of their own. The
lock time comes last. The spreadsheet of [`exportsheet`](#hw-exportsheet)
lists every address of every transaction, in a tab of its own.

```console
hw txs
hw txs --network bch
hw txs --verbose
hw txs --tabular
hw txs --tag rent                           # the transactions tagged rent
hw txs --address-tag kyc                    # those at an address tagged kyc
hw txs --tag rent --address-tag kyc         # those that match either
hw txs --untagged                           # those left to sort
hw txs --csv txs.csv
hw txs --max 20 --after TXID
hw txs --reverse                            # the oldest transaction first
```

> **Good to know**
>
> - **A row of the table is wide.** It takes 91 columns before its last
>   cell, and more when a transaction has tags, since the `Tags` column is as
>   wide as the widest tags of a transaction. The last cell takes what its
>   label or address needs. `less` keeps the row
>   on one line, and the right arrow key scrolls to its end. The columns keep
>   the same widths from the first row to the last, so that a long listing
>   lines up. A rare cell that does not fit, such as an amount of 10 000
>   bitcoins or more, pushes the rest of its row to the right.
> - **The balance is that of one network.** It adds up the accounts of the
>   portfolio on that network. Each section of the listing has its own
>   running balance.
> - **The balances of a filtered listing do not follow from its rows.** With
>   `--tag`, `--address-tag` or `--untagged`, each balance still counts every
>   transaction of the portfolio, those left out too. To add up the
>   transactions of a tag, write them with `--csv` and sum the
>   `Amount (sats)` column.
> - **The list follows the chain, not the times.** A confirmed transaction
>   has the time of its block, which its miner set. An unconfirmed one has
>   the time the server first saw it. Now and then, a block is dated before
>   the block it follows, by a minute or two on Bitcoin. Miners of test
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
>   only one of a payment and its replacement, and a miner may still confirm
>   the stuck payment. The replacement is then the dead one. It says
>   `replaced by`, with the id of the payment that confirmed. `hw` never
>   stores the note, so it is always as of the last sync. It also names a
>   replacement made on another computer that holds the same keys.
> - **A replacement need not pay the same recipients.** It is the transaction
>   that took the coins. One made by `bumpfee` pays the recipients of the
>   payment. Any other may pay somebody else, so look at it with
>   [`show`](#hw-show) before you count a dead payment as made.
> - **A payment that spent the change of a replaced payment names the same
>   replacement.** It died with that payment. `bumpfee` replaces both with one
>   transaction that pays the recipients of both.
> - **A dead payment you received names no replacement.** `hw` finds a
>   replacement from the coins of the portfolio, and the payer spent coins of
>   their own. When the payer bumps the fee, the replacement is in the list
>   too, paid at the same address.
> - **A payment that no sync saw leaves no dead transaction.** That is a
>   payment whose sending could not sync the wallet. When you bump it before
>   any sync has seen it, the history never holds it. `txs` then shows the
>   replacement alone.
> - **The CSV file and the spreadsheet name the replacement by its txid in
>   full.** Their Status column says `dead`, and `Replaced by` gives the txid.
>   Several replacements share the cell, with a comma and a space between
>   them. `txs --json` gives them under `replacedby`.
> - **The CSV file leaves the value of a payer's address empty.** In a payment
>   you received, the addresses under `From` belong to another wallet. The
>   coins of that wallet do not concern yours, so `hw` shows none of their
>   values, as on the screen.
> - **A transaction between your own addresses fills no `Change` column.**
>   `hw` cannot tell which of its outputs is the change. The address it pays
>   the most is under `To`, and `Other addresses` counts the rest.
> - **A block of Bitcoin Cash is sorted by transaction id.** It can put a
>   payment before the one it spends from. `txs` lists such a payment right
>   above the one it spends from, and moves no other, so the balance stays
>   right. `coins` keeps the order of the block.

#### `hw pendingtxs`

Lists the pending transactions of every portfolio of every wallet. A pending
transaction is one that you prepared, signed, or sent, and that no sync has
seen on the network yet. The command that sends a transaction syncs the
wallet right away, so a sent transaction stays in this list only when that
sync failed. Unlike the other listings, `pendingtxs` is not
limited to the [active portfolio](#the-active-portfolio). Each pending
transaction waits for you to sign, send or delete it, and one that you forgot
in another portfolio keeps its coins locked. The commands that take a pending
transaction find its portfolio by themselves, so none needs a `switch`.
`--network` shows one network only. `pendingtxs` does not list a transaction
that is pending on a [hidden network](#shown-and-hidden-networks): it locks
coins of that network alone.

In the compact view, each pending transaction takes two lines, as in `txs`:

1. Its [short id](#short-ids). Until the transaction is signed, this is the
   short id of its nosigHash, the hash of the transaction without its
   signatures. Then it is the short id of its txid. The line then gives the
   time the transaction was created, its amount and its status, with its fee
   rate.
2. Where the transaction is: its portfolio and its network, and its wallet too
   when the database has more than one, as in `portfolio0 of wallet1 btc`. A
   replacement then names the transactions it replaces, in yellow on a
   terminal. Then comes what the transaction is for, as in `txs`, with the
   label of a contact of the [address book](#the-address-book) after the
   address it pays. The [tags](#tags-on-addresses-and-transactions) of the
   transaction end the line.

`--verbose` names the replaced transactions in full, in yellow too. A
transaction with the status `conflicted` can no longer be sent.
[`show HASH`](#hw-show) shows one pending transaction in full.

When a transaction belongs to another portfolio than the active one, its paths
name that portfolio, as in `bip44/1/0 of portfolio1`. See
[Paths of another portfolio](#paths-of-another-portfolio).

`pendingtxs` shows one list, not a list in sections, since each transaction
names its portfolio and its network. The list is sorted by wallet and
portfolio, then by network, in the order `portfolios` shows the networks. On
each network, the newest transaction comes first, or the oldest with
`--reverse`. The tab of pending transactions of
[`exportsheet`](#hw-exportsheet) holds them too.

Three options filter the listing, as they do for [`txs`](#hw-txs). `--tag TAG`
shows only the pending transactions that have the tag. `--address-tag TAG`
shows only those that spend from or pay an address of their portfolio that has
the tag, and those that pay or were paid from a contact of the address book
that has it. `--untagged` shows only those left to sort: those that have no
tag, and that neither pay a contact that has a tag nor were paid by one.
[`tag`](#hw-tag) puts tags on a pending transaction, by either of its hashes.
The transaction keeps them when it is signed, and once the network has it.

`--tabular` shows each transaction on one row of a table: its short id, its
wallet when the database has more than one, its portfolio and its network, the
time it was created, its type, its amount, its status, its fee rate in sat/vB,
its tags, and what it is for, as `txs --tabular` says it. The description of a
replacement starts with the transactions it replaces, in yellow, as in
`replaces a1075db5, to bc1q…`. The wallet, portfolio and network columns are
as wide as their longest cell.

`--csv FILE` writes the pending transactions to a [CSV file](#listings). The
file has the columns of [`txs --csv`](#hw-txs) that apply to a pending
transaction, in the same order. The nosigHash comes before the txid, each in
full and by its short id, and the txid is empty until the transaction is
signed. `Change in fee` gives the change that was too small for an output, and
that the fee took in. `Replaces` gives the txids of the transactions that a
replacement made by `bumpfee` replaces. The file has no time on the chain, no
balance, no confirmations and no block. The time the transaction was created
comes last.

```console
hw pendingtxs
hw pendingtxs --verbose
hw pendingtxs --tabular
hw pendingtxs --tag rent                    # the pending transactions tagged rent
hw pendingtxs --untagged                    # those left to sort
hw pendingtxs --csv pending.csv
hw pendingtxs --network bch
hw pendingtxs --reverse                     # the oldest first on each network
```

> **Good to know**
>
> - **A pending transaction locks its coins.** They stay locked until `sync`
>   sees the transaction on the network, or until `deletetx` deletes it. Delete
>   the transactions you abandon. A [replacement](#hw-bumpfee) also locks the
>   coins of the payment it replaces.
> - **A replacement is yellow until the network takes one transaction.** The
>   network keeps only one of a replacement and the transactions it replaces.
>   Until a block holds one of them, any of them may still make the payment,
>   and the replaced one is still in `txs`. Once `sync` has seen the
>   replacement on the network, the replacement leaves this list.
>   [`txs`](#hw-txs) then shows the replaced transaction dead, with the words
>   `replaced by` and the short id of the replacement.
> - **The wallet is named only when there are several.** In a database of one
>   wallet, the views leave the name of the wallet out, since there is no
>   other wallet to tell it from. With two wallets or more, every view names
>   the wallet of each transaction, `wallet0` too. Every view of `hw` follows
>   this rule: see
>   [How the views name a portfolio](#how-the-views-name-a-portfolio).

#### `hw exporttx`

`hw exporttx HASH FILE` writes a pending transaction to a
[file](#transaction-files), signed or not, to carry it to another computer.
Name the transaction by its nosigHash, or by its txid once it is signed, in
full or by its [short id](#short-ids).

```console
hw exporttx NOSIGHASH unsigned.psbt
```

#### `hw importtx`

`hw importtx FILE` adds a [transaction file](#transaction-files) to the pending
transactions of a portfolio. When the wallet already holds the transaction,
the file takes its place, as when an offline computer returns it signed. The
wallet keeps its own labels.

A file that brings nothing new changes nothing, and is no error: the file of
the version that the wallet holds, or the unsigned file of a transaction that
the wallet holds signed. `importtx` then says so before it shows the
transaction, with how far along the transaction is: not signed, signed and
not sent, or sent.

```console
The wallet holds this transaction already, signed and not sent: nothing was
changed.
```

```console
hw importtx signed.psbt
```

`hw` finds the portfolio by itself, from the addresses the transaction spends
from and pays. A transaction that moves funds between two portfolios goes to
the portfolio that pays, since that one must lock the coins and sign. That
holds whichever portfolio is active.

**A file from another computer of the wallet.** `importtx` takes a file that
`preparetx` wrote on another computer that holds the portfolio, once this one
is synced and has the coins the transaction spends. The payment has a new
change address, which this computer has not generated yet. `importtx`
generates it from the path the file gives, and holds it for the transaction:
the next payment made here takes the next change address.

`importtx` refuses a file that spends a coin the wallet does not hold. The
error names the coin, and says why the wallet does not hold it and what to do:

| The wallet | What `importtx` says |
|---|---|
| has never seen the coin | `The transaction spends the coin a16ab47f...080e:0, which the wallet does not have`, in full. Run [`sync`](#hw-sync), then import the file again. An offline computer never has the coins: there, [`signtx --input`](#hw-signtx) signs the file without importing it. |
| has the transaction in its history | `The network has this transaction already: it is in the history of portfolio0 of wallet0 on btc, and there is nothing to import`. This is the file of a payment that was sent since. |
| saw another transaction spend the coin | `The transaction spends the coin a16ab47f:0, which the transaction 9e87df76 spent already`. [`show 9e87df76`](#hw-show) shows that transaction. It may be this very payment: the unsigned file of a legacy payment has another id than the signed payment. |
| saw the network drop the transaction that made the coin | `The transaction spends the coin a16ab47f:0, but the network dropped the transaction that made this coin, so the coin does not exist`. This happens when the sender replaced that transaction. |

`importtx` also refuses a file that spends a coin that another pending
transaction spends already. The error names the coin and that transaction,
with its portfolio when it is not the active one:
`The transaction spends the coin a16ab47f:0, which the pending transaction
885c608d spends already. hw deletetx 885c608d deletes that transaction and
frees the coin`.

`importtx` also refuses a file that pays an address of the portfolio more
than a thousand past the last used address of its chain, further than a
payment of `hw` goes. A file can name any path, and `hw` does not generate
that far on its word.

In two cases `hw` cannot tell which portfolio the transaction belongs to: when
several portfolios pay, and when none pays and the transaction pays several.
It then takes the active portfolio if it is one of them. Otherwise it names
them, and imports nothing. Make the one to use active with
[`switch`](#hw-switch), and import the file again.

#### `hw deletetx`

`hw deletetx HASH` deletes a pending transaction and frees its coins. Name it
by its nosigHash, or by its txid once it is signed, in full or by its
[short id](#short-ids). A transaction you sent can be deleted only once the
server no longer knows it. The [tags](#tags-on-addresses-and-transactions) of
the transaction are deleted with it.

```console
hw deletetx HASH
```

`deletetx` asks before it deletes. It names the transaction by its short id,
and says what state it is in: not signed, signed and not sent, sent and
dropped by the server, or in conflict with another transaction. Any answer but
`y` or `yes` deletes nothing, and `hw` exits with an error:

```console
$ hw deletetx 3f2a9c1e
The pending transaction 3f2a9c1e will be deleted from the wallet, with its label
and its tags.
It is signed, and was not sent.
The coins it spends can be spent again, and its change address is free again.
Delete the pending transaction? [y/N] y
...
```

`--yes` answers the question in advance, for [scripts](#scripts). `hw` refuses
a transaction that the server still knows, without asking.

The change address of the deleted transaction is free again, and loses its
[tags](#tags-on-addresses-and-transactions). The next payment takes that
address for its own change. With `--tag-change`, that payment gives the
address the tags of its own coins.

### Tags

A tag is a word you put on addresses and transactions to group them.
[Tags on addresses and transactions](#tags-on-addresses-and-transactions) says
what tags are for, how they differ from labels, and which options take one.

#### `hw tag`

`hw tag ADDRESS|TXID|PATH TAG...` puts tags on an address, on a transaction or
on a contact of the [address book](#the-address-book). The tags that are
there already stay. With one word and an option, `tag` changes a tag itself
instead: `--color` gives the tag a colour, and `--lock-coins` makes it lock
its coins.

| Words | What `tag` does |
|---|---|
| `ADDRESS TAG...` | Tags that address of the wallet, a receiving or a change address, or that contact of the address book |
| `bip84/0/i TAG...` | Tags the receiving address `i` of the account of that type, in the active portfolio, or the change address `i` with `bip84/1/i`. Without the type, as in `/0/i`, the account is the one `--type` picks |
| `TXID TAG...` | Tags that transaction, by its txid, or by its nosigHash while it is pending |
| `TAG --color COLOR` | Gives the tag a colour, and tags nothing |
| `TAG --lock-coins` | Makes the tag lock its coins, and tags nothing |
| `TAG --unlock-coins` | Stops the tag from locking its coins |

```console
hw tag ADDRESS kyc cold                     # two tags on an address
hw tag bip84/0/3 kyc                        # the address at a path
hw tag /0/3 kyc --type bip84                # the same path, of the account --type picks
hw tag bip84/1/0 kyc                        # a change address
hw tag ADDRESS kyc --with-change            # the address, and its change down the chain
hw tag TXID rent                            # a transaction
hw tag a1075db5 rent                        # the same, by its short id
hw tag ADDRESS friends                      # a contact of the address book, by its address
hw tag ADDRESS kyc --color red              # tag the address, and colour the tag
hw tag kyc --color red                      # colour the tag kyc
hw tag kyc --color none                     # remove its colour
hw tag savings --lock-coins                 # the tag savings locks its coins
hw tag savings --unlock-coins               # it no longer locks them
```

`tag` answers with the address, as [`show`](#hw-show) shows it, or with the
transaction, as each portfolio that holds it sees it. The tags come in
alphabetical order. A tag that is there already stays, so tagging twice does
no harm. When `tag` only changes a tag, it answers with the line that
[`tags`](#hw-tags) shows for that tag:

```console
$ hw tag bip44/0/0 kyc cold
bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  "Wages"  [cold] [kyc]
  Transactions: 0
  ...
  Account     : portfolio0  btc  bip44
$ hw tag kyc --color red
kyc  red  1 address
$ hw tag savings --lock-coins
savings  green  1 address, locks its coins
```

**The word.** A tag is one word, made of the letters `a` to `z`, digits, `-`
and `_`, with 32 characters at most. Capital letters are taken as small ones,
so `Kyc` and `kyc` are one tag. Spaces, accents and other signs are refused.
An address or a transaction can have several tags. A tag exists from the
first time you use it. You do not create it first.

**What to tag.** The first word names it, and [`label`](#hw-label) takes the
same first word. `tag` tells what a word is by its place, not by its shape.
With several words, the first is what to tag, and the others are the tags.
With one word and `--color`, `--lock-coins` or `--unlock-coins`, the word is
a tag. `tag` refuses a tag typed first, and says the order to follow:

```console
$ hw tag kyc 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj
Error: 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj reads as an address, not as a tag.
What to tag comes first, then the tags: ADDRESS|TXID|PATH TAG
...
```

**An address.** `tag` finds the portfolio and the account of an address from
the address itself. So it takes no `--type`, and it tags the address
whichever portfolio is active. A change address takes tags too, although it
takes no label: a tag says which group its coins belong to.
[`addresses --change`](#hw-addresses) lists the change addresses.

**The change down the chain, with `--with-change`.** A coin does not stay at
the address that received it. A payment spends it, and what is left goes to a
change address. The next payment moves it to another change address. So when
you tag an address that has a history, the group may hold no coin: the coins
are at change addresses that have no tag. `--with-change` tags those too. It
tags the address, then the change of every payment that spent from it, then
the change of the payments that spent that change, and so on down to the
coins you hold now. The answer lists the change addresses it tagged, each
with its total, so you see where the coins of the group are:

```console
$ hw tag 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj kyc --with-change
bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  [kyc]
  Transactions: 2
  ...
  Account     : portfolio0  btc  bip44

Tagged with it, down the chain of its payments: 2 change addresses
bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  [kyc]
  total 0.00000000 btc, received 0.00027740 btc, 2 transactions
bip44/1/1  1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6  [kyc]
  total 0.00015480 btc, received 0.00015480 btc, 1 transaction
```

- **Down the chain, never up.** A tag passes from the coins a payment spends
  to its change: what is left of a `kyc` coin is `kyc`. It does not pass the
  other way. `hw tag bip44/1/0 cold --with-change` tags that change address
  and the change down the chain from it. It tags none of the addresses its
  coins came from.
- **Every coin must have the tag.** A change address takes a tag only when
  every coin that the payment spent has the tag.
  [`--tag-change`](#hw-preparetx) follows the same rule. The change of a
  payment that also spent a coin from outside the group takes no tag. Tag the
  address of that coin too, with `--with-change`, and the change of that
  payment then takes the tag.
- **Where the chain stops.** The answer also lists each change address that
  such a payment kept the tag from, with the payment and the address of the
  coin from outside the group. Without that list, a balance of the group
  smaller than expected would have no explanation. It names the first
  address that could not take the tag, and none of those below it:

  ```console
  $ hw tag 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj kyc --with-change
  ...
  No change address down the chain of its payments was tagged with it

  Not tagged, where the chain stops: 1 change address
  bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn
    total 0.00016260 btc, received 0.00016260 btc, 1 transaction
    The payment aa87c9a3 also spends a coin at bip44/0/1, without the tag kyc
  ```
- **The whole chain, every time.** `tag --with-change` looks at every change
  address down the chain from the address, whatever tags they have.
  Afterwards, each of them that the rule allows has the tag. So it completes
  a chain tagged in part, as when some of its payments had `--tag-change` and
  others not. It also puts back a tag you took off one of them by hand.
- **Change sent back with `--change-back`.** When a payment sent its change
  back to a change address, even the one it spent from, `tag` follows it as
  any change. When a payment sent its change back to a receiving address,
  `tag` does not follow it: a receiving address has the tags you give it and
  no others. Tag that address as well, with `--with-change`, to continue down
  the chain from it.
- **Pending payments count.** The chain goes through a payment that waits to
  be signed or sent, as it goes through a payment of the history. A payment
  that the network dropped, or that another one replaced, moved no coin, so
  `tag` leaves it out.
- **Only when you ask.** Without `--with-change`, `tag` tags the address alone.
  The option is refused with a transaction, with a contact and with a tag
  alone, which have no chain of change.
- **With `--json`**, the answer lists those change addresses under
  `changetagged`, each as [`addresses`](#hw-addresses) gives an address, and
  `untag` lists them under `changeuntagged`. `tag` lists the change addresses
  where a tag stopped under `changenottagged`: each has its `address`, the
  `tags` it did not take, the `txid` of the payment, and under `spentfrom`
  the address of the coin from outside the group, or `null` for a coin that
  is not of the portfolio.

**A path.** A path names an address of the active portfolio, as the views
write it, such as `bip84/0/3`, or `bip84/1/0` for a change address. The
portfolio must hold the address. A path without a type, such as `/0/3`, is
in the account that `--type` picks, or in the account of the default type.
`--type` is refused with a path that names its type.

**A transaction.** Name a pending transaction by the hash that `pendingtxs`
shows, and a transaction of the history by its txid. Give either in full or
by its [short id](#short-ids). `tag` tags a transaction in every portfolio
that holds it. A pending transaction keeps its tags through signing, and in
the history once the network has it. `--tag` on [`send`](#hw-send) and on
[`preparetx`](#hw-preparetx) tags a payment as you make it, so no `tag` has
to follow. A payment to another portfolio of the database has those tags
there too, and its label, once `sync` has found it.
[`bumpfee`](#hw-bumpfee) gives a replacement the tags of the payment it
replaces. [`deletetx`](#hw-deletetx) deletes the tags of a pending
transaction with it.

**A colour.** `--color` gives a colour to each tag of the command: `red`,
`green`, `yellow`, `blue`, `magenta` or `cyan`. The screen then shows the tag
in that colour. `none` removes the colour. A tag has one colour in the whole
database, on everything that has the tag. So a new colour shows on every
address and transaction that has the tag. You can colour a tag before
anything has it. A colour is only a help to the eye. No command behaves
differently for it.

**A lock on its coins.** `--lock-coins` makes a tag lock its coins. No payment
then spends a coin at an address that has the tag, unless the payment names
the tag with [`--spend-from-tag`](#hw-preparetx). Use it for savings, and for
any coins that an everyday payment must not take by accident.
`--unlock-coins` takes the lock off, and every payment may spend the coins
again. Giving either option twice changes nothing.

```console
$ hw tag bip44/0/1 savings
...
$ hw tag savings --lock-coins
savings  green  1 address, locks its coins
$ hw coins
portfolio0  btc

3b273748:0  14 Nov 2023 23:13      0.00050000 btc  spendable, 3 confirmations
  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0  [kyc]

3b273748:1  14 Nov 2023 23:13      0.00030000 btc  locked by tag, 3 confirmations
  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  bip44/0/1  [savings]
```

`hw` has three locks, and a different action takes each one off. A pending
transaction locks the coins it spends, until it confirms or you delete it.
`lockcoin` locks a coin you name, until you run `unlockcoin`. A tag locks the
coins at the addresses that have it, until `--unlock-coins`. A payment that
names the tag still spends them.

- **The lock is on the tag, and nothing is written on a coin.** `hw` reads the
  tags of the address each time it selects coins. So a coin that arrives
  later at an address that has the tag is locked at once, and so are the
  coins of an address you tag afterwards. Take the tag off an address with
  [`untag`](#hw-untag), and its coins are free. This is the difference from
  [`lockcoin`](#hw-lockcoin), which locks the coins you name and no others.
- **Only the tags of addresses lock coins.** A coin is at an address, so a tag
  locks the coins at the addresses that have it. The same tag on a
  transaction or on a contact locks nothing: neither the coins the
  transaction paid you, nor its change.
- **Only `--spend-from-tag` opens the lock.** Naming the coin with `--coin`,
  or its address with `--spend-from`, does not spend it. The payment leaves
  the coin and says why, with the tag that locks it. When the payment chose
  no other coin, `hw` refuses it, and gives the `--spend-from-tag` that opens
  the lock. When two tags of an address lock, the payment must name both.
  [`sendall`](#hw-sendall) leaves the coin too, and lists it among the
  coins that stay. [`importtx`](#hw-importtx), [`signtx`](#hw-signtx) and
  [`broadcast`](#hw-broadcast) refuse a transaction that spends it, and
  [`bumpfee`](#hw-bumpfee) adds no such coin to a replacement. In short,
  only a payment that names the tag spends the coins that the tag locks.
- **The lock wins over a payment prepared before it.** A pending transaction
  may spend a coin that comes under the lock afterwards. The command that
  brings the lock says so, on the error output, and `signtx` and `broadcast`
  then refuse that transaction:

  ```console
  $ hw tag savings --lock-coins
  The pending transaction 885c608d spends the coin 3b273748:1, which is locked now. It can no longer be signed or sent: deletetx 885c608d deletes it
  savings  green  1 address, locks its coins
  $ hw signtx 885c608d
  Error: The transaction spends the coin 3b273748:1, which the tag savings locks. A payment made with --spend-from-tag savings spends it, and hw tag savings --unlock-coins unlocks it
  ```

  Delete the transaction with [`deletetx`](#hw-deletetx), or unlock the tag
  for the time it takes to sign and send it. The same line is printed when
  you put a tag that locks on an address, when
  [`renametag --merge`](#hw-renametag) merges a tag into one that locks, and
  when [`importwallet`](#hw-importwallet) brings a lock. A transaction that
  was sent already is on the network, and no lock stops it: the line then
  ends with "It was sent already, and the lock does not stop it". A payment
  that named the tag with `--spend-from-tag` is not stopped. `hw` keeps the
  tags that a pending payment named, and the payment stays valid when the tag
  is renamed.
- **A tag can lock its coins before any address has it.** The tag need not
  exist yet: `hw tag savings --lock-coins` makes it. Tag the addresses
  afterwards, with `tag` or with [`receive --tag`](#hw-receive), and what
  they receive is locked from the start.
- **Two tags that lock need both names.** When an address has two tags that
  lock their coins, a payment spends from it only with `--spend-from-tag` for
  each of the two tags. Naming one tag leaves the lock of the other in place.
- **The change is not locked unless you ask.** The change of a payment made
  with `--spend-from-tag savings` goes to a new change address, which has no
  tag, so any payment may spend it. Add `--tag-change`, and the change takes
  the tag, and stays under its lock.
- **`lockcoin` and a tag are two locks.** When both lock a coin, a payment
  that names the tag still leaves the coin, until `unlockcoin` frees it.
  [`unlockcoin`](#hw-unlockcoin) does not take off the lock of a tag.
- **The lock holds in the whole database.** A tag is the same tag in every
  portfolio of every wallet, and so is its lock.
  [`renametag`](#hw-renametag) keeps the lock. [`deletetag`](#hw-deletetag)
  says, before it asks, that the coins will be free. The
  [file of a wallet](#hw-exportwallet) carries the lock.

> **Good to know**
>
> - **`--lock-coins` goes with a tag alone.** `hw tag ADDRESS savings
>   --lock-coins` is refused. It would look as if it locked the coins of that
>   address only, but the lock is on every address that has the tag. Tag the
>   address first, then run `hw tag savings --lock-coins`. `--lock-coins` and
>   `--unlock-coins` together are refused, and so is `--unlock-coins` with a
>   word that is no tag.
> - **A word alone needs an option.** `hw tag kyc` has nothing to tag and
>   nothing to change. `hw` refuses it, and says how to give the tags and how
>   to colour the tag. `hw tag ADDRESS` is refused too, since its tags were
>   left out.
> - **What to tag comes first.** `hw tag kyc cold` is refused, since `kyc` is
>   not an address, a path or the id of a transaction. With the words the
>   other way round, as in `hw tag kyc ADDRESS`, the refusal says that the
>   address reads as an address, not as a tag, and that what to tag comes
>   first. A path or a txid in full among the tags is refused the same way,
>   by `tag` and by `untag`.
> - **A new tag that looks like a transaction id is not coloured.** A word of
>   8 hexadecimal digits, such as `deadbeef` or `20261115`, has the shape of a
>   [short id](#short-ids). With `hw tag 20261115 --color red`, you most
>   likely left out the tags of a transaction. So `hw` refuses it while no
>   such tag exists. Put the tag on something first, and you can then colour
>   it as any other.
> - **`--network` picks one of several.** The test networks share their keys,
>   so the wallet may have the same address on several of them. `--network`
>   then says which one to tag, as for [`label`](#hw-label). It is refused
>   with a transaction and with a tag alone.
> - **`tag` gives no new address.** The portfolio must hold the address
>   already. `addresses` and `addresses --change` list the addresses it holds.
>   To tag a new address, use [`receive --tag`](#hw-receive).

#### `hw untag`

`hw untag ADDRESS|TXID|PATH TAG...` takes tags off an address, a transaction
or a contact of the [address book](#the-address-book), and leaves its other
tags. The first word names what to take the tags off: an address of the
wallet or of the address book, a path of the active portfolio, or a
transaction. [`tag`](#hw-tag) takes the same first word.

```console
hw untag ADDRESS kyc                        # the address is no longer in the group kyc
hw untag bip84/0/3 kyc                      # the address at a path
hw untag ADDRESS kyc --with-change          # the address, and its change down the chain
hw untag TXID rent tax                      # take two tags off a transaction
```

`untag` answers with the address or the transaction as it is now, as `tag`
does. `untag` refuses a tag that the address or the transaction does not
have, and changes nothing. You most likely mistyped the tag, or named the
wrong address. When it refuses one of several tags, it takes none of them
off:

```console
$ hw untag bip84/0/0 cold
bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc  "Savings plan"  [savings]
  Transactions: 0
  ...
  Account     : portfolio0  btc  bip84
$ hw untag bip84/0/0 cold
Error: The address bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc does not have the
tag cold
```

A transaction that several portfolios hold loses the tags in each.

**`--with-change`** also takes the tags off the change of every payment that
spent from the address, then off the change of the payments that spent that
change, and so on. [`tag --with-change`](#hw-tag) puts tags on the same
change addresses. Use it to take a tag put by mistake off a whole chain at
once. It looks at every change address down the chain from the address, so
none of them has the tag afterwards. The answer lists those that lost it.
Without the option, `untag` takes the tags off the address alone. When you
forgot the option the first time, give the same command again with it: the
address has lost the tag, and `hw` takes it off the change addresses, and
says so. It refuses only when no address of the chain has the tag.

The tag itself stays, with its colour, even when nothing has it any more.
[`tags`](#hw-tags) then shows it as `not used`. `hw` keeps it because a tag
deleted with its last use would lose its colour, and you would find it
without one the day you used it again. [`deletetag`](#hw-deletetag) deletes a
tag everywhere.

#### `hw tags`

`hw tags` lists every tag of the database, in alphabetical order. Each
line gives the tag in its colour, the name of that colour, and how many
addresses, transactions and contacts of the
[address book](#the-address-book) have it. A tag that
[locks its coins](#hw-tag) says so at the end of its line:

```console
$ hw tags
cold             2 addresses
kyc      red     1 address
rent     yellow  not used
savings  green   1 address, locks its coins
```

A tag that addresses, transactions and contacts have reads
`12 addresses, 3 transactions, 1 contact`. The name of the colour is there
for a screen without colours, and so that you can give another tag the same
colour.

The counts cover every portfolio of every wallet, since a tag belongs to the
whole database. A transaction that two portfolios hold counts once: a payment
from one of your portfolios to another is one transaction with the tag, though
each of the two lists it. [`renametag`](#hw-renametag) and
[`deletetag`](#hw-deletetag) count the same way. A tag that nothing has reads
`not used`. It stays until [`deletetag`](#hw-deletetag) deletes it.

To see what has a tag, list it in the active portfolio:

```console
hw addresses --tag kyc
hw addresses --change --tag kyc
hw coins --tag kyc
hw txs --tag rent
hw contacts --tag friends                   # the contacts of a tag, whichever portfolio is active
```

Two commands change a tag itself, on everything that has it. `tag` sets its
colour and its lock, and `renametag` its name:

```console
hw tag kyc --color blue                     # give it a colour, or another one
hw tag kyc --color none                     # remove its colour
hw tag savings --lock-coins                 # make it lock its coins
hw tag savings --unlock-coins               # or stop it
hw renametag kyc verified                   # give it a new name
```

When the database has no tag, `tags` says how to make one:

```console
$ hw tags
There are no tags.
To put one on an address or a transaction, run: hw tag ADDRESS|TXID|PATH TAG
```

> **Good to know**
>
> - **`tags` takes none of the options of a listing.** It has one view, and no
>   `--tabular`, `--csv` or `--max`. `--json` gives each tag with its `name`,
>   its `color`, the counts `addresses`, `txs` and `contacts`, and
>   `lockscoins`, which is true for a tag that locks its coins.
> - **A tag stays when its portfolio is deleted.**
>   [`deleteportfolio`](#hw-deleteportfolio) and
>   [`deletewallet`](#hw-deletewallet) delete the addresses and the
>   transactions of a portfolio, and the tags come off them. The tag itself
>   stays, with its colour and its lock, and may then read `not used`.

#### `hw renametag`

`hw renametag TAG NEW-NAME` gives a tag a new name in the whole database: on
every address and every transaction that has it, in every portfolio of every
wallet, and on every contact of the [address book](#the-address-book). The tag
keeps its colour, and its lock when it [locks its coins](#hw-tag). The answer
says how many addresses, transactions and contacts have it:

```console
$ hw renametag kyc verified
Renamed the tag kyc to verified
  It is on 2 addresses, 1 transaction
```

The new name follows [the rules of a tag](#tags-on-addresses-and-transactions):
one word, made of the letters `a` to `z`, digits, `-` and `_`, with 32
characters at most. Capital letters are taken as small ones.

**Merging two tags.** `renametag` refuses a name that another tag has
already, and names the option that merges the two:

```console
$ hw renametag saving savings
Error: There is a tag savings already. To merge saving into it, add --merge: hw
renametag saving savings --merge
```

`--merge` makes one tag of the two. Everything that had the first tag then
has the second one, and the first tag is gone. Use it for a tag that you
typed two ways, such as `saving` and `savings`:

```console
$ hw renametag saving savings --merge
Merged the tag saving into savings
  It is on 3 addresses
```

The counts are those of the two tags together. An address that had both has
`savings` once. The second tag keeps its colour, or takes the colour of the
first when it has none. It locks its coins when either tag did, and the
answer then ends with `and it locks its coins`. A merge never takes a lock
off, since that would free coins you had put away. When the merge puts a
lock on a coin that a pending transaction spends, `renametag` says so, as
[`tag`](#hw-tag) does.

`renametag` cannot split a merged tag again, since nothing records any more
which of the two tags each address or transaction had. So `renametag` merges
only with `--merge`, and a mistyped new name cannot fold one group into
another. With `--merge`, the second word must be a tag that exists.
`renametag` refuses any other word, since you most likely mistyped it:

```console
$ hw renametag saving savngs --merge
Error: There is no tag savngs to merge saving into. hw tags lists the tags. To
rename saving, leave out --merge.
```

`renametag` asks no question, since it changes no funds, no key and no label.
`hw` keeps a copy of the labels and the tags after each command that changes
them, in `~/.hw/auto-backups`, unless the configuration turns the copies off
([Automatic backups](#automatic-backups)). The copy made before a merge has
the two tags as they were.

> **Good to know**
>
> - **Only the name changes.** To change the colour of a tag, use
>   [`hw tag TAG --color COLOR`](#hw-tag).
> - **To undo a merge, restore the tags of an earlier copy.** Delete the
>   merged tag with [`deletetag`](#hw-deletetag). Then run
>   [`importwallet`](#hw-importwallet) with the copy made before the merge,
>   which puts the two tags back on what had them. That copy is not the
>   newest, since the merge made one too. Check it first with
>   [`show FILE`](#hw-show). `importwallet` also sets the labels of that copy,
>   as they were then.
> - **A name of two words is refused.** A tag is one word. Join the words with
>   `-` or `_`, as in `hw renametag kyc know-your-customer`.
> - **The same name in capital letters is refused.** `hw renametag kyc KYC`
>   changes nothing, since `KYC` and `kyc` are one tag.
> - **Scripts and saved commands keep the old name.** Once the tag is
>   `verified`, `hw` refuses `hw send --spend-from-tag kyc` in a script, as it
>   refuses any word that is no tag. Change the name there too.
> - **The configuration file keeps the old name.** A line of the table
>   `journal-accounts` that gives the old name
>   [an account of the journal](#hw-exportjournal) no longer files anything.
>   Change the name on that line yourself.
> - **`--json` tells a merge from a rename.** The answer has the type
>   `tagrenamed` or `tagmerged`. Both give the old name under `from`, and the
>   tag as it is now under `tag`, as [`tags`](#hw-tags) gives it.

#### `hw deletetag`

`hw deletetag TAG` deletes a tag from the whole database, with its colour. It
takes the tag off every address and every transaction that has it, in every
portfolio of every wallet, and off every contact of the
[address book](#the-address-book). The answer says how many had it.

`deletetag` asks before it deletes. It says how many addresses, transactions
and contacts lose the tag, and names the command that takes a tag off one of
them only:

```console
$ hw deletetag cold
The tag cold will be deleted from the whole database.
It comes off 1 address, and no label or other tag changes.
To take a tag off one address, transaction or contact and keep the tag, use
untag instead.
Delete the tag cold? [y/N] y
Deleted the tag cold
  It was on 1 address
```

Any answer but `y` or `yes` deletes nothing, and `hw` exits with an error.
`--yes` answers the question in advance, for [scripts](#scripts).

`deletetag` also deletes the lock of a tag that
[locks its coins](#hw-tag). Before it asks, it then adds the line
`It locks its coins. Once it is deleted, any payment may spend the coins at its addresses.`

[`untag`](#hw-untag) takes a tag off one address or one transaction, and
keeps the tag. `deletetag` is for a tag you no longer want at all. For a tag
you mistyped, [`renametag`](#hw-renametag) gives it its right name. A word
that is no tag is refused, since there is nothing to delete:

```console
$ hw deletetag cold
Error: There is no tag cold. hw tags lists the tags.
```

Nothing else is deleted. The addresses, the transactions and their labels stay
as they are. `hw` keeps a copy of the labels and the tags after each command
that changes them, in `~/.hw/auto-backups`, unless the configuration turns
the copies off ([Automatic backups](#automatic-backups)).
[`importwallet`](#hw-importwallet) with the copy made before the deletion
brings the tag back, on what had it.

> **Good to know**
>
> - **The copy to restore from is not the newest.** `deletetag` makes a copy
>   too, without the tag. Take the one before it, and check it first with
>   [`show FILE`](#hw-show), which shows the tags of each address.
>   `importwallet` also sets the labels of that copy, as they were then.

### Address book

The address book keeps the addresses of others, each with a label and tags.
[The address book](#the-address-book) says what it is for, how a transaction
shows a contact, and how to pay one.

#### `hw newcontact`

`hw newcontact ADDRESS [LABEL]` adds an address of somebody else to the address
book, with a label that says whose it is. `--tag` gives the new contact tags,
as [`receive --tag`](#hw-receive) does for a new address.

```console
hw newcontact ADDRESS Landlord              # an address with its label
hw newcontact ADDRESS "Alice, savings"      # quote a label that has spaces
hw newcontact ADDRESS Alice --tag friends   # with a tag
hw newcontact ADDRESS                       # without a label
hw newcontact ADDRESS Bob --network bch     # a legacy address, on Bitcoin Cash
```

The answer shows the contact with every detail. Its first line gives the
address in one piece, to copy, then the label and the tags. `In fours` gives
the address in groups of four characters:

```console
$ hw newcontact 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU Landlord --tag rent
1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  "Landlord"  [rent]
  In fours: 1JCq 8Aa9 d9rg 4T4X V93R V3DM xd5u 7GkS SU
  Network : btc
  Added   : 15 Nov 2026 14:38:05 CET
```

Compare the groups with the address the payee gave you, one group at a time,
before you trust the contact. `hw` cannot tell whose an address is. It only
keeps what you typed.

**The network.** A contact is on one network. Most addresses are valid on one
network only, and `newcontact` takes that one, whatever the default network.
A legacy address is valid on `btc` and on `bch`, and an address of a test
network is valid on the other test networks. `newcontact` then takes the
default network, when the address is valid there, unless `--network` names
another. `hw` keeps a Bitcoin Cash address in its CashAddr form, with its
prefix, whichever form you typed:

```console
$ hw newcontact bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a Bob --tag friends
bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a  "Bob"  [friends]
  In fours: bitcoincash: qpm2 qszn hks2 3z76 29mm s6s4 cwef 74vc wvy2 2gdx 6a
  Network : bch
  Added   : 15 Nov 2026 14:38:05 CET
```

**What is refused.** `newcontact` refuses an address that is in the address
book already, and shows the label it has. It also refuses an address of a
wallet of the database, and shows where the address is: the address book is
for the addresses of others.

```console
$ hw newcontact 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU Somebody
Error: 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is in the address book already, as
"Landlord". To change its label, run: hw label
1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU LABEL
$ hw newcontact 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj Me
Error: 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is an address of the wallet: bip44/0/0
of portfolio0 of wallet0. The address book is for the addresses of others. To
label it, run: hw label 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj LABEL
```

**Changing a contact.** [`label`](#hw-label) changes its label, and
[`tag`](#hw-tag) and [`untag`](#hw-untag) its tags, each by the address of the
contact. [`show`](#hw-show) shows it, and
[`deletecontact`](#hw-deletecontact) deletes it:

```console
hw label ADDRESS "Mr Smith"
hw label ADDRESS ""                         # removes the label
hw tag ADDRESS rent kyc
hw untag ADDRESS kyc
hw show ADDRESS
```

> **Good to know**
>
> - **The label is optional, and worth giving.** A contact without one is
>   shown with the words `address book` after its address in a transaction.
>   [`contacts`](#hw-contacts) lists it after the contacts that have a label.
> - **`newcontact` needs no portfolio.** It works in a database without a
>   wallet, and whichever portfolio is active.
> - **An address the wallet has not given out yet is not known as its own.**
>   `hw` knows an address of a portfolio once `receive` gave it out or a
>   `sync` found it used. So `newcontact` accepts an address that the
>   portfolio would give out later, as if it were an address of somebody else.
>   Once the portfolio gives the address out, the views show it as the
>   wallet's. Delete the contact then.

#### `hw contacts`

`hw contacts` lists the address book. The contacts come in sections, one for
each [shown network](#shown-and-hidden-networks) that has some, under a line
that names the network. In each section, the contacts are sorted by label,
without regard to capital letters. The contacts without a label come last,
sorted by address. `--reverse` reverses the order in each section.

Each contact takes two lines: its address in one piece, with its label and its
[tags](#tags-on-addresses-and-transactions), then its address in groups of
four characters.

```console
$ hw contacts
btc

bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  "Alice, savings"  [friends]
  bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4

1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  "Landlord"  [rent]
  1JCq 8Aa9 d9rg 4T4X V93R V3DM xd5u 7GkS SU

bch

bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a  "Bob"  [friends]
  bitcoincash: qpm2 qszn hks2 3z76 29mm s6s4 cwef 74vc wvy2 2gdx 6a
```

`--network` lists only the contacts on one network, and the section then has
no title line. `--verbose` also shows when each contact was added:

```console
$ hw contacts --verbose --network btc
bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  "Alice, savings"  [friends]
  In fours: bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4
  Added   : 15 Nov 2026 14:38:05 CET

1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  "Landlord"  [rent]
  In fours: 1JCq 8Aa9 d9rg 4T4X V93R V3DM xd5u 7GkS SU
  Added   : 15 Nov 2026 14:38:05 CET
```

`--tabular` shows each contact on one row of a table: its address, the address
in fours, its tags and its label. The two address columns are as wide as the
longest address of the section:

```console
$ hw contacts --tabular
btc
Address                                    │ In fours                                             │ Tags    │ Label
───────────────────────────────────────────┼──────────────────────────────────────────────────────┼─────────┼──────
bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4 │ bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4 │ friends │ "Alice, savings"
1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU         │ 1JCq 8Aa9 d9rg 4T4X V93R V3DM xd5u 7GkS SU           │ rent    │ "Landlord"

bch
Address                                                │ In fours                                                          │ Tags    │ Label
───────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┼─────────┼──────
bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a │ bitcoincash: qpm2 qszn hks2 3z76 29mm s6s4 cwef 74vc wvy2 2gdx 6a │ friends │ "Bob"
```

`--tag TAG` lists only the contacts that have the tag. Repeat it for several
tags, and the listing shows the contacts that have any one of them. Its first
line then names those tags. A word that is no tag is refused:

```console
$ hw contacts --tag friends
Contacts tagged friends

btc

bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  "Alice, savings"  [friends]
  bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4

bch

bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a  "Bob"  [friends]
  bitcoincash: qpm2 qszn hks2 3z76 29mm s6s4 cwef 74vc wvy2 2gdx 6a
```

`--untagged` lists only the contacts that have no tag, under the line
`Contacts without a tag`. With `--tag`, the listing shows those as well.

`--csv FILE` writes the contacts to a [CSV file](#listings): the network of
each, its address, the address in fours, its label and its tags, then when it
was added. The label and the tags are under `Contact label` and
`Contact tags`, since `Address label` is the label of an address of the
wallet. The tags share one cell, with a space between them.

```console
hw contacts
hw contacts --network btc                   # one network, with no title line
hw contacts --verbose
hw contacts --tabular
hw contacts --tag friends                   # the contacts of a tag
hw contacts --tag friends --tag rent        # those that have either tag
hw contacts --untagged                      # the contacts that have no tag
hw contacts --csv contacts.csv
hw contacts --max 10 --after ADDRESS
hw contacts --reverse                       # from the last label of each section
```

An empty address book says how to add a contact:

```console
$ hw contacts
There are no contacts to display
To add an address of somebody else to the address book, run: hw newcontact ADDRESS LABEL
```

> **Good to know**
>
> - **The listing is the same whichever portfolio is active.** The address
>   book belongs to the whole database.
> - **`--json` gives the contacts in groups, one for each network.** Each
>   contact has its `address`, its `label`, its `tags`, each with its `name`
>   and its `color`, and `created`, when it was added. The address in fours is
>   not in the JSON, since a script compares the address itself.
> - **A transaction gives its contacts in JSON too.** With `--json`, a
>   transaction lists the addresses that are contacts under `contacts`, by
>   address, each with its label and its tags. They stay among `otheroutputs`
>   and `otherinputs` as well, which hold their values.

#### `hw deletecontact`

`hw deletecontact ADDRESS` deletes one contact from the address book, with its
label, and takes its tags off it. It asks first, and shows the contact with
its label. The answer shows the contact as it was:

```console
$ hw deletecontact bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a
The contact bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a "Bob" will be
deleted from the address book.
Its tags come off it, and the tags themselves stay.
The transactions that paid the address stay in the history, and show it as the
address of a stranger.
Delete the contact? [y/N] y
Deleted from the address book
  bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a  "Bob"  [friends]
```

Nothing else changes. The transactions that paid the address stay in the
history. From then on, they show the address alone, as they show the address
of a stranger. The tags themselves stay, on everything else that has them.
`deletecontact` refuses an address that the address book does not have:

```console
$ hw deletecontact bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a
Error: The address book has no contact
bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a. hw contacts lists them.
```

Any answer but `y` or `yes` to the question deletes nothing, and `hw` exits
with an error. `--yes` answers it in advance, for [scripts](#scripts).

`hw` keeps a copy of the labels and of the address book after each command
that changes them, in `~/.hw/auto-backups`, unless the configuration turns the
copies off ([Automatic backups](#automatic-backups)).
[`importwallet`](#hw-importwallet) with the copy made before the deletion
brings the contact back.

> **Good to know**
>
> - **`--network` picks the contact when the address is a contact on several
>   networks.** A legacy address may be a contact on `btc` and on `bch`.
>   Without `--network`, `deletecontact` takes the only one, or else the one
>   on the default network.
> - **The copy to restore from is not the newest.** `deletecontact` makes a
>   copy too, without the contact. Take the one before it, and check it first
>   with [`show FILE`](#hw-show), which lists the address book of the file.

### Sweeps

#### `hw sweep`

`hw sweep --sweep-to ADDRESS` moves coins whose private keys you hold, from
an old wallet for example, to new addresses, in one command. It prepares the
sweep as [`preparesweep`](#hw-preparesweep) does, shows it, signs it with the
private keys as [`signsweep`](#hw-signsweep) does, and sends it as
[`broadcast`](#hw-broadcast) does. The result is the same as when you run the
three commands one after the other with the same options.

```console
hw sweep --sweep-to MYADDRESS --fee-rate median                       # type the keys when asked
hw sweep --key-file keys.txt --sweep-to MYADDRESS --fee-rate median
hw sweep --key-file keys.txt --sweep-to MYADDRESS --fee-rate 5 --label "old paper wallet"
hw sweep --key-file keys.txt --sweep-to ADDRESS1:0.001 --sweep-to ADDRESS2 --fee-rate low
hw sweep --key-file keys.txt --sweep-to ADDRESS --network bch --fee-rate low   # to an address of no portfolio
```

`sweep` needs the private keys and the server at the same time, so it runs on
an online computer. A computer that was tampered with can steal a private key
that it reads. Use `sweep` for keys that you are willing to give to this
computer. To keep the keys offline, sweep in three steps instead:
`preparesweep` here, `signsweep` on the offline computer, and `broadcast`
here.

**The keys.** `--key-file FILE` names the file that holds the keys. Without
it, or with `--key-file -`, `sweep` asks for them on the terminal, where they
are not shown. Type one or more on a line, or paste a list that has a key on
each line. `sweep` says how many keys it has read after each line, and an
empty line ends them, as for `preparesweep --key-file -`. `sweep` takes every
coin at the addresses of the keys.

**The options** are those of [`preparesweep`](#hw-preparesweep), all but
`--output`, `--sweep-from`, `--address-file` and `--coin`. `sweep` writes no
file. It signs at once, so every coin it takes needs its key, and the keys
name the coins. The other options mean the same, and they build the same
sweep. The section of `preparesweep` explains the destinations, the dust
coins, the portfolio and the network of a sweep, and the fee. Without
`--fee-rate`, `sweep` shows the rates and asks.

**What you see.** `sweep` reads the keys first, because the keys say which
coins the sweep takes. It then shows the sweep once, in the form of the
answer of `preparesweep`, with the dust coins that it leaves below it. Then
it asks whether to sign the sweep and send it:

```console
$ hw sweep --sweep-to 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj --fee-rate 10 --label "old paper wallet"
Private keys (WIF, minikey or hex, separated by spaces):
1 private key read. More private keys, or Enter to continue:
20 Nov 2026 03:03  Incoming     +0.00028080 btc  not signed, 10 sat/vB
  Portfolio: portfolio0
  Network  : btc
  Types    : bip44
  Label    : "old paper wallet"
  NoSigHash: a2cd3280638ce0b9dd7b2470ac91a45c24841f191da20bf4059e8254d79a61a5
  Short id : a2cd3280
  Created  : 20 Nov 2026 03:03:49 CET
  Spends   : 0.00030000 btc of coins that are not this portfolio's
  Fee      : 0.00001920 btc, 10 sat/vB
  Lock time: block 800002
  From     : 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK  0.00030000 btc
  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00028080 btc  bip44/0/0

Sign this sweep and send it to the network? This cannot be undone. [y/N] y
Sent, txid b6ddfd44eacd18390edf77f67492098e3484bdbeee69a6b85700606ee1add35b

portfolio0  btc
  total 0.00028080 btc, spendable 0.00000000 btc, unspendable 0.00028080 btc, 1 coin
```

Check the sweep at that point: every `From` line, every `To` line and the
fee. Any answer but yes stops the command: nothing is signed, and nothing is
sent. `--yes` signs and sends without the question, for a script, and `sweep`
shows the sweep all the same. The answer is the txid, since the sweep did not
change after you read it. With `--json`, the answer is the transaction as
sent, as with `broadcast --json`.

`sweep` then syncs the portfolio that the sweep pays, as
[`send`](#hw-send) does after a payment, and shows what the portfolio holds.
The sweep is in [`txs`](#hw-txs) at once. Its coin comes from keys outside
the wallet, so it is `unconfirmed`, and `hw` spends it once a block has it.
When that sync fails, the sweep is still sent: `sweep` says so, names
[`sync`](#hw-sync) as the next step, and exits as a success.

**A sweep that pays no portfolio of this wallet** needs `--network`, as for
`preparesweep`. `hw` is then a tool that needs no portfolio, and the wallet
keeps nothing of the sweep. The answer names the network and the txid:

```console
$ hw sweep --key-file keys.txt --sweep-to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU --network btc --fee-rate 10 --yes
...
Sent     : 5dfcc57e972afa9d2a5bd425c3a035e7d24c15ab2b15c5d55b5c4f8701a5b23a
  Short id : 5dfcc57e
  Network  : btc
```

**When the sweep does not go through.** What `sweep` leaves in the wallet
depends on whether the sweep was signed, and on what it pays.

- **Not signed: nothing is kept.** When `sweep` refuses the sweep, and when
  your answer to a question stops it, the wallet is as it was. It has no new
  pending transaction.
- **Signed and not sent, to a portfolio: the sweep stays pending.** `sweep`
  keeps the signed sweep in the wallet before anything leaves the computer.
  When the sending fails, `sweep` says why, as `broadcast` does, and names
  the transaction by the [short id](#short-ids) of its txid. The sweep is
  then signed and pending, as it is after `signsweep`. `broadcast` sends it
  again, and [`deletetx`](#hw-deletetx) deletes it.
- **Signed and not sent, to no portfolio: nothing is kept.** The three
  commands keep such a sweep in files, and `sweep` writes none. You have the
  keys, so run `sweep` again.

```console
$ hw sweep --key-file keys.txt --sweep-to MYADDRESS --fee-rate 10
...
Error: The store at https://api.haskoin.com/ refused the transaction:
bad-txns-inputs-missingorspent. The transaction is signed and stays pending:
broadcast b6ddfd44 sends it again, and deletetx b6ddfd44 deletes it and frees
its coins
```

> **Good to know**
>
> - **`sweep` checks the server before it reads the keys.** A server that
>   does not answer stops it there, so you do not type the keys for nothing.
> - **`--yes` answers two questions.** It takes a fee rate above 100 satoshis
>   per virtual byte without asking, and it signs and sends without asking.
>   It does not answer the question about the dust coins: `--spend-dust` or
>   `--leave-dust` does.
> - **Keys on the standard input leave no line for an answer.** `sweep` reads
>   them to the end of the input. A script gives `--yes`, and `--spend-dust`
>   or `--leave-dust`, as [Scripts](#scripts) shows. With `--key-file FILE`,
>   the input stays free for the answers.
> - **A yes pasted with the keys answers nothing.** `hw` discards what was
>   typed ahead once it has read the keys, so the question waits for an
>   answer that you type after you have read the sweep.
> - **`sweep` takes the coins of its keys, and no others.** To sweep one coin
>   of a key and leave the rest, or to sweep an address whose key is
>   elsewhere, use `preparesweep` with `--coin` or `--sweep-from`.
> - **A sweep takes no coin of this wallet**, as the section of
>   [`preparesweep`](#hw-preparesweep) says. A key that holds no coin is not
>   named. When no key holds a coin, `sweep` says that there are no coins to
>   sweep.

#### `hw preparesweep`

Prepares a sweep: a transaction that moves coins from outside the wallet,
whose private keys you hold, to new addresses. The coins of an old wallet are
an example. Preparing needs no key. The keys are needed only to sign, with
[`hw signsweep`](#hw-signsweep). `hw` keeps a sweep that pays an address of
this wallet as a pending transaction. [`hw sweep`](#hw-sweep) does the three
steps in one command, for keys that you are willing to give to the online
computer.

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
file. `preparesweep` asks before it takes a fee rate above 100 satoshis per
virtual byte, as `preparetx` does. `--yes` answers yes in advance.

A dust coin is a coin worth no more than the fee to spend it, and an old
wallet often holds some. Each one lowers what the sweep pays. `preparesweep`
lists the dust coins, with the fee rates at which each is worth spending. It
then tells what the `--sweep-to` addresses get with the dust coins and
without them, and asks whether to spend them too. The answer is for all of
them: any answer but yes leaves them at their addresses, and the sweep takes
the other coins. `--spend-dust` or `--leave-dust` answers in advance, as for
[`preparesendall`](#hw-preparesendall).

`preparesweep` answers with the sweep in full, as [`show`](#hw-show) prints a
transaction that is not sent yet: every address it sweeps, with its amount,
the total it spends, and every address it pays. It names below the sweep what
the sweep leaves out: each address given with `--sweep-from` or in the
`--address-file` that holds no coin, each coin that
[holds tokens](#coins-that-hold-tokens), and each dust coin that stays, with
its txid in full, as `--coin` takes it:

```console
1 address given holds no coin and is not in the sweep:
  1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2

1 coin is worth less than its fee and is not in the sweep:
  5e2c4f1b9a7d3e6f8b0c1d2a4f6e8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a:0
    at 1GAehh7TsJAHuUAeKZcXf5CnwuGuGgyX2S  0.00000600 btc
```

With `--json`, the answer gives the addresses under `emptyaddrs`, the dust
coins under `dustcoins` and the coins that hold tokens under `tokencoins`,
each coin with its `outpoint`, its `address` and its `value`.

A sweep that pays an address of this wallet belongs to the portfolio of that
address, on its network. `hw` finds both from the `--sweep-to` addresses. When
those addresses belong to several portfolios, `hw` keeps the sweep in the
active portfolio if it is one of them. Otherwise `hw` names the portfolios,
and prepares nothing. Use `switch` to make active the portfolio that must keep
the sweep, and run the command again. A sweep is on one network, so `hw`
refuses `--sweep-to` addresses of this wallet on two networks. The test
networks of one coin share their keys, so the wallet can hold one address on
two of them: `hw` then asks for `--network`, whatever the default network.

`hw` holds one pending sweep of a coin. To prepare a sweep again, at another
fee rate for example, delete the first one with [`deletetx`](#hw-deletetx).
Until then, `preparesweep` refuses, and names the coin and the pending sweep:

```console
Error: The transaction spends the coin
cdde9e9d9d163f3dbb6f644c5231915dff702a7445cbb6b184ab8de435ff2f36:0, which the
pending transaction 28119c6b spends already. hw deletetx 28119c6b deletes that
transaction and frees the coin
```

The same sweep prepared again, with the same options, is the transaction that
the wallet holds: `preparesweep` then names it with its next step, `signsweep`
or `broadcast`.

A sweep that pays no address of this wallet belongs to no portfolio. It needs
`--network` and `--output`, and goes from file to file:

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
> - **`--sweep-from` and `--address-file` take addresses, never private
>   keys.** With a paper wallet, the key is what you have in hand. `hw` refuses
>   a private key given where an address goes, and does not print it back: an
>   error goes to the screen and to the log of a script, where a key must not
>   be. Give the address of the key, or give the key with `--key-file`.
> - **A sweep takes no coin of this wallet.** `preparesweep` refuses an
>   address that a portfolio of this wallet has on the network of the sweep,
>   and a coin of `--coin` or of a key that is at such an address.
>   `signsweep` signs with the private keys of the coins, and you hold none
>   for an address of the wallet. To move the coins of the wallet, use
>   [`sendall`](#hw-sendall) or [`preparetx`](#hw-preparetx):
>   `--spend-from` chooses the coins of an address.
> - **A private key or a legacy address is written the same way on Bitcoin
>   and Bitcoin Cash.** That is why a sweep that pays no portfolio of the
>   wallet needs `--network`, and the default network does not apply to it. A
>   wrong guess would sweep the coins of the other chain.
> - **Whether a coin is dust depends on the fee rate, and on its key.** A
>   coin that is dust at 10 satoshis per virtual byte may be worth sweeping at
>   1, so a lower `--fee-rate` can take it. Without the keys, a legacy coin
>   counts for its larger size, so it is dust up to a higher value. `--yes`
>   does not answer the question about the dust coins: it is about the fee
>   rate alone.
> - **A key of the `--key-file` that holds no coin is not named.** A key has
>   an address of each type, and most of them hold nothing. Only the
>   addresses that you give are named when they hold no coin. The `From`
>   lines of the sweep show which addresses are swept.
> - **Without the keys, the fee of a legacy coin counts its larger size.**
>   `hw` cannot tell from an address if its key is compressed, and the coin of
>   an uncompressed key, as old paper wallets have, takes 32 bytes more to
>   spend. So `hw` counts the larger size. The fee is never below the rate you
>   ask, and a coin of a compressed key pays about a fifth more. Give
>   `--key-file` for the exact fee. SegWit keys are always compressed, so the
>   fee of a SegWit coin is exact either way.
> - **A sweep takes legacy, native SegWit and nested SegWit coins, and no
>   Taproot coin.** Those are the coins that one private key signs, in the
>   ways `hw` signs. `preparesweep` refuses a Taproot address (`bc1p…`) given
>   with `--sweep-from` or in the `--address-file`, before it reads the keys.
>   For a key of the `--key-file`, it looks at the legacy address and at the
>   two SegWit addresses of the key: coins at the Taproot address of the key
>   are not found, and stay where they are. A coin paid to a public key
>   itself, as in the first blocks of the chain, has no address, and `hw`
>   does not sweep it either.
> - **`preparesweep` and `sweep` need the server, with or without
>   `--fee-rate`.** The coins of a sweep are outside the wallet, so `hw`
>   reads them from the server each time.

#### `hw signsweep`

Signs a sweep with the private keys of its coins. Name a pending sweep by its
nosigHash, or give a sweep file with `--input`. `--key-file FILE` names the
file that holds the keys. Without it, `signsweep` asks for the keys on the
terminal. `--key-file -` asks for them there too, here and in `preparesweep`.

```console
hw signsweep NOSIGHASH --key-file keys.txt
hw signsweep --input sweep.psbt --key-file keys.txt --output signed.psbt
```

`signsweep` first shows the sweep in full, as [`show`](#hw-show) prints a
transaction that is not sent yet, then asks for the keys. Check every `To`
line and the `Spends` total before you give them, above all for a file that
you did not prepare yourself: the keys sign for every address that the sweep
pays. Stop it with Ctrl-C if an address is not one you mean.

**Typing the keys.** The keys are not shown as you type them. Type one or
more on a line, with spaces between them, or paste a list that has a key on
each line. `signsweep` knows the coins of the sweep, so it stops asking as
soon as every coin has its key: one line and one Enter for most sweeps.
While a coin has no key, it says how many coins have theirs and asks for
another line. For a sweep of two coins, with a key on each line:

```console
Private keys (WIF, minikey or hex, separated by spaces):
1 of the 2 coins has its key. More private keys, or Enter to stop:
```

An empty line stops the question, and `signsweep` then names a coin that has
no key. `preparesweep --key-file -` cannot know which keys are still to come,
because the keys tell it which coins to sweep. It says how many keys it has
read after each line, and an empty line ends them:

```console
Private keys (WIF, minikey or hex, separated by spaces):
1 private key read. More private keys, or Enter to continue:
```

When the keys come from a pipe, both commands read them to its end, as the
[Scripts](#scripts) section says.

**A key that does not read.** `hw` reads no key at all when one of them is
wrong, on the terminal, from a pipe or from a file. The error gives the place
of the word and the reason, and never the word itself: a key with one
character wrong is almost the key.

```console
Error: Word 2 of line 1 has the form of a private key, but its checksum is
wrong, as when a character is mistyped. No key was read
```

The other reasons are a key of another network, a key in WIF or in
hexadecimal digits with a character lost or added, and hexadecimal digits
written after `0x`. In a key file, a word that has nothing of a key is a note
beside the keys of its line, and a line that starts with `#` is a comment.

`signsweep` signs the coins from outside the wallet, and asks for the keys
only when it has something to sign. It refuses a payment of the wallet, whose
coins the mnemonic signs, and names the command that signs it: `hw signtx`
with the same hash or the same file. A key file that cannot be read stops
`signsweep` before it shows the sweep.

Without `--output`, `signsweep` offers to replace the `--input` file, as
`signtx` does. No option answers this question, so a script must give
`--output`.

A sweep that pays no portfolio of this wallet needs no portfolio here either.
`signsweep` signs it on the network that its file names.

> **Good to know**
>
> - **An empty line ends the keys typed on a terminal.** A list pasted with
>   an empty line in its middle stops there, and `hw` forgets the lines after
>   it. Paste the list without empty lines, or give its file with
>   `--key-file`, which passes over empty lines and comments.
> - **Keys pasted beyond what `signsweep` needs do not reach the shell.**
>   `signsweep` stops reading once every coin has its key, and `hw` discards
>   the lines after that, as
>   [Keep your funds safe](#keep-your-funds-safe) says of every secret.

### Messages

#### `hw signmsg`

`hw signmsg ADDRESS|PATH MESSAGE` signs a message with the key of an address of
the wallet, to prove that you control the address. It asks for the mnemonic of
the wallet of that address. `--output` writes the signed message to a file
instead of printing it.

The address must be one that the wallet holds: one that this computer gave
out. The offline computer gives out no address, since `receive` runs on the
online computer. On the offline computer, give the path of the address
instead, with its type, such as `bip84/0/3`. `addresses` shows the path on the
online computer. `signmsg` derives the address at the path from the public key
of the account, as [`show`](#hw-show) does. The path is in the active
portfolio, on `--network` or the default network. The signed message names the
address, so you see which address signed.

```console
hw signmsg MYADDRESS "I control this address" --output signed.txt
hw signmsg bip84/0/3 "I control this address"   # on the offline computer
hw signmsg bip44/0/0 "I control this address" --network bch
```

Any wallet can verify a signature made from a P2PKH address. A signature made
from a native SegWit address has the header byte of BIP 137. The wallets that
sign from such an address write and verify that byte. Some wallets verify no
signature from a SegWit address.

> **Good to know**
>
> - **A message is signed byte for byte.** A space or a line break added at
>   either end makes the signature fail. `hw` warns about them.
> - **`signmsg` and `verifymsg` work with P2PKH and SegWit addresses of one
>   key,** not with Taproot ones.
> - **`signmsg` finds the portfolio of an address by itself.** It signs with
>   the address whichever portfolio is active. `--network` is needed only when
>   the wallet has the address on several test networks, which share their
>   keys, and none of them is the default network.
> - **Sign with an address you gave out.** A path takes any index, and the
>   signed message shows its address in full. An address far past the ones you
>   gave out may lie past the gap of 20 addresses that a restored wallet looks
>   at. No `sync` would then find a payment to it.

#### `hw verifymsg`

Checks that the key of an address signed a message. The signature can come
from `hw` or from another wallet, and the address can be any P2PKH address, or
any SegWit address of one key. `verifymsg` takes every header byte of
[BIP 137], 27 to 42, whichever the wallet that signed chose. Some wallets read
other header bytes too. So for a P2PKH address, another wallet may find valid
a signature that `hw` does not, or the other way around.

```console
hw verifymsg --input signed.txt
hw verifymsg ADDRESS SIGNATURE "I control this address"
```

`--input` reads the address, the signature and the message from a file that
holds the output of `signmsg`.

`verifymsg` needs no `--network`. It finds the network from the address
itself, whatever the default network. Some addresses are valid on several
networks. One that starts with `1` is valid on `btc` and `bch`, and one that
starts with `m` is valid on every test network. This does not change the
answer, since a message is signed the same way on every network. With
`--network`, `verifymsg` reads the address on that network alone. `hw` refuses
an address that is valid on [hidden networks](#shown-and-hidden-networks)
only, and names the command that shows a network.

### The wallet

#### `hw backup`

`hw backup` copies the database to a file you name, or into
`~/.hw/user-backups`. The copy holds every wallet and portfolio, with their
addresses, labels, tags, coins, transactions and pending transactions. It
holds no private key.

```console
hw backup
hw backup ~/hw-2026-10-30.sqlite
```

Without a file name, `backup` writes two files into `~/.hw/user-backups`: the
copy of the database, and a file of its labels. The file of the labels is
the one that [`exportwallet`](#hw-exportwallet) writes without options. `hw`
names the two files as it names the [automatic backups](#automatic-backups).
The name of the copy of the database starts with `portfolios-version-` and
its format version. The name of the file of the labels starts with
`portfolios-labels-`. Both names then give the time of the backup in UTC, and
how many wallets and portfolios the database holds:

```console
$ hw backup
The database was copied to /home/you/.hw/user-backups/portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-3.sqlite
Its labels were written to /home/you/.hw/user-backups/portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-3.jsonl
```

Only an `hw` with the same database format can read the copy of the
database. The file of the labels lasts longer:
[`importwallet`](#hw-importwallet) of any later `hw` can read it, and so can
other wallets. `hw` never removes a file in `~/.hw/user-backups`. It keeps
its own [automatic backups](#automatic-backups) apart, in
`~/.hw/auto-backups`, where it removes the older ones. When you name a file,
`backup` writes the copy of the database only, without the labels.

[`show`](#hw-show) with a copy lists the wallets and the portfolios it holds,
and does not change the copy. Run it before you restore a backup, to see what
comes back. The first line of its answer reads `SQLite database` for a copy
of the database, and `BIP 329 file` for a file of labels. So you can tell the
two files of a backup apart:

```console
$ hw show ~/hw-2026-10-30.sqlite
SQLite database  /home/you/hw-2026-10-30.sqlite
  A database of hw at format version 5, with 1 wallet and 2 portfolios

wallet0  bc560f95
  * portfolio0  created 21 Oct 2026  "Savings"
    portfolio1  created 28 Oct 2026
```

[`restoredatabase`](#hw-restoredatabase) replaces the database with the copy.
To restore the file of the labels, give it to
[`importwallet`](#hw-importwallet) instead, which reads it into a database of
any version.

`hw` writes into `~/.hw/user-backups` only when you run `backup`. The copies
it makes by itself all go to `~/.hw/auto-backups`. That includes the copy it
makes before a command that removes data: before
[`deletewallet`](#hw-deletewallet) and
[`deleteportfolio`](#hw-deleteportfolio) delete, and before `restoredatabase`
replaces the database. [`showbackups`](#hw-showbackups) counts the backups of
both directories, and names the newest of each.

#### `hw showbackups`

`hw showbackups` shows the backups that `hw` keeps, and changes nothing. It
runs on [a damaged database](#a-damaged-database) too. It
reads the two directories of backups:

- `~/.hw/auto-backups`, the [copies `hw` makes by itself](#automatic-backups)
  of the database and of the labels.
- `~/.hw/user-backups`, the copies [`backup`](#hw-backup) wrote.

In each directory, and for each kind of copy, the database and the labels,
it gives the number of copies, how long ago `hw` made the newest one, and
the name of that one:

```console
$ hw showbackups
Automatic backups  /home/you/.hw/auto-backups
  Database: 3 copies, the newest made 2 days ago
            portfolios-version-5-2026-11-12T09-11-40.250Z-wallets-1-portfolios-3.sqlite
  Labels  : 12 copies, the newest made 5 minutes ago
            portfolios-labels-2026-11-14T10-20-48.902Z-wallets-1-portfolios-3.jsonl

User backups  /home/you/.hw/user-backups
  Database: 1 copy, made 21 days ago
            portfolios-version-5-2026-10-24T08-00-00.000Z-wallets-1-portfolios-2.sqlite
```

Run it to check that `hw` makes its copies. `hw` prints nothing when it makes
a copy. When it cannot make one, it warns on stderr, and a script may never
show you that warning. When the newest copy is older than your last change,
`hw` missed a copy. Run [`backup`](#hw-backup) to make one now. A deletion
and a restore are the exception: `hw` makes their copy right before them, and
none after.

Give one of these files to [`show`](#hw-show) to list the wallets and the
portfolios of a copy, or to [`restoredatabase`](#hw-restoredatabase) to
restore it.

With `--json`, the answer gives the same under the keys `automatic` and
`user`. Each kind has its count under `copies`, and the file and the time of
the newest copy under `newest`, which is `null` when there is none. The two
automatic kinds also say whether they are on, under `enabled`.

> **Good to know**
>
> - **The answer says when a kind is turned off.** With `autobackup-database`
>   or `autobackup-labels` set to `false`, that kind has one more line, which
>   starts with `Turned off`. The line comes below the copies that `hw` made
>   while the kind was on.
> - **`showbackups` counts only the copies of `hw`.** It leaves out a file of
>   your own in one of the directories. `hw` never removes such a file
>   either.
> - **A user backup counts once.** Each has two files, the copy of the
>   database and the file of its labels. `showbackups` counts the copies of
>   the database.

#### `hw restoredatabase`

`hw restoredatabase FILE` replaces the database with a copy of it. The copy
is a file that [`backup`](#hw-backup) wrote, or a `.sqlite` file of
`~/.hw/auto-backups` or `~/.hw/user-backups`.
[`showbackups`](#hw-showbackups) names the newest copy in each directory.
Before you restore a copy, [`show`](#hw-show) lists the wallets and the
portfolios it holds.

```console
hw show ~/hw-2026-10-30.sqlite              # what the copy holds
hw restoredatabase ~/hw-2026-10-30.sqlite
hw sync                                     # bring its history up to date
```

`restoredatabase` replaces the whole database. Everything the copy holds
comes back: the wallets, their portfolios, the addresses, the labels, the
tags, the coins, the transactions, the pending transactions and the active
portfolio. Everything that the database holds and the copy does not is lost,
such as a wallet you created after you made the copy. So `restoredatabase`
asks first, and says what the database and the copy hold:

```console
$ hw restoredatabase ~/hw-2026-10-30.sqlite
The database holds 1 wallet and 3 portfolios. The copy holds 1 wallet and 2
portfolios, and replaces all of it. hw copies the database into auto-backups
first. Replace the database? [y/N] y
The database was restored from /home/you/hw-2026-10-30.sqlite

wallet0  bc560f95
  * portfolio0  created 21 Oct 2026  "Savings"
    portfolio1  created 28 Oct 2026

The database it replaced was copied to /home/you/.hw/auto-backups/portfolios-version-5-2026-11-14T10-05-33.123Z-wallets-1-portfolios-3.sqlite
Its labels are in /home/you/.hw/auto-backups/portfolios-labels-2026-11-14T10-05-33.123Z-wallets-1-portfolios-3.jsonl

Next, sync the wallet, to bring the history of the copy up to date:
  hw sync
```

Before it replaces the database, `hw` copies it and its labels into
`~/.hw/auto-backups`, with its other [automatic backups](#automatic-backups).
So if you restore the wrong copy, run `restoredatabase` with that backup to
undo it. `hw` keeps that backup by the rules for the automatic backups, so
newer copies push it out with time.

`hw` makes no copy after the restore. The file you restored from holds
everything that the database holds now, so keep that file. The backup made
before the restore stays the newest copy in `~/.hw/auto-backups` until the
next command that makes one.

`--yes` answers the question in advance, for [scripts](#scripts). A database
without a wallet has nothing to lose, so `hw` asks nothing and makes no
backup of it. That is the case when you restore on a new computer. `hw` then
makes no copy at all, and `~/.hw/auto-backups` stays empty until the next
command that makes a copy. To have a copy on the new computer at once, run
[`backup`](#hw-backup) after the restore.

The copy holds the history as its last sync left it, so run
[`sync`](#hw-sync) afterwards. The settings in `~/.hw/config.toml` are not
part of the database, and stay as they are.

`restoredatabase` also replaces a database that SQLite cannot read. It asks
nothing then, and moves the damaged file into `~/.hw/broken-databases` in
place of a backup. [A damaged database](#a-damaged-database) shows it.

**Why a command, and not a file put in place.** You could restore a copy by
hand, by putting it in place of `~/.hw/portfolios.sqlite`. That has two
risks. First, a command that was cut short, by a crash for instance, leaves a
file `portfolios.sqlite-wal` beside the database. That file holds changes
that belong to the old database, and SQLite would apply them to the new file.
Second, another `hw` command may be running, and may still write to the old
file. `restoredatabase` avoids both. It writes the rows of the copy into the
database through SQLite, in one transaction, under the lock that every
command takes. A restore that fails half way leaves the database as it was.

> **Good to know**
>
> - **`hw` restores only a copy of its own database format.** The copy must
>   have the format version of this `hw`, which `hw --version` shows. `hw`
>   refuses a copy of the database of an earlier version, and one of a newer
>   `hw`. [Upgrading from an earlier version](#upgrading-from-an-earlier-version)
>   tells how to create the wallet again instead.
> - **The file of the labels is not a database.** A backup has two files with
>   the same time in their names. `restoredatabase` takes the `.sqlite` one,
>   and refuses the `.jsonl` one. Give that one to
>   [`importwallet`](#hw-importwallet).
> - **The copy is left as it is.** `hw` reads it, and writes nothing to it.
> - **With `autobackup-database` set to `false`, `hw` keeps no backup of the
>   database it replaces.** The question says so. Run [`backup`](#hw-backup)
>   first.
> - **`hw` cannot restore into a database of another version.** When every
>   command refuses the database in `~/.hw` for its format version, move
>   `portfolios.sqlite` aside first, with the files `portfolios.sqlite-wal`
>   and `portfolios.sqlite-shm` if they are there. `hw` then starts a new
>   database, and `restoredatabase` fills it. A
>   [damaged database](#a-damaged-database) is another matter: `hw` moves
>   that one itself.
> - **The newest copy of the labels may be newer than the copy of the
>   database.** `hw` copies the database only when a command adds a wallet or
>   a portfolio, or is about to remove one, and the labels after each change
>   to them. After the restore, run [`importwallet`](#hw-importwallet) with
>   the newest copy of the labels to bring the newer labels in. The restore
>   itself makes no copy of the labels after it, so the newest one is still
>   the one from before the restore.

#### `hw exportsheet`

`hw exportsheet FILE` writes the whole wallet to a new spreadsheet. Each of
the six listings has a tab: the portfolios, the addresses, the balances, the
coins, the transactions and the pending transactions. A last tab,
`Address book`, holds the contacts of the
[address book](#the-address-book), in the columns of `contacts --csv`. The
file is an OpenDocument spreadsheet (`.ods`), the format of LibreOffice,
which Excel and Google Sheets open too. Each tab holds every row of its
listing, in the same columns and the same order as its
[CSV file](#listings). So rows copied from a CSV file fit under the headings
of its tab, and the other way round.

Two more tabs hold the addresses of the transactions. `Transaction addresses`
comes after the transactions, and `Pending transaction addresses` after the
pending ones. In the tab of the transactions, the row of a transaction names
three addresses at most, and counts the others. These two tabs have a row
for every address that a transaction spends from or pays, however many there
are. So you can filter on an address, sum by label or by
[tag](#tags-on-addresses-and-transactions), or find every payment to one
address, even when the transaction paid more to another address.

| Column | What it holds |
|---|---|
| `Wallet`, `Portfolio`, `Network` | Where the transaction is |
| `Type` | The type of the account that holds the address, empty for an address of another wallet |
| `Txid`, `Short txid` | The transaction. They match the row with the row of the transaction in the tab of the transactions. A pending transaction has its nosigHash first |
| `Time`, `Direction`, `Status` | Those of the transaction, repeated on each of its rows. A pending transaction has no time here. Its row ends with the time it was created |
| `Role` | `from` for an address that the transaction spends from, `to` for one it pays, and `change` for its change address, as [`show`](#hw-show) lists them |
| `Value (sats)` | What the transaction spends from the address or pays it |
| `Address`, `Path`, `Chain`, `Address label`, `Address tags` | The address in full. Then its path, its chain, its label and its tags, when the wallet holds the address |
| `Held by wallet`, `Held by portfolio` | The portfolio that holds the address, empty for an address of another wallet |
| `Transaction label`, `Transaction tags` | The label and the tags of the transaction, repeated on each of its rows |

Under each role, the address with the largest value comes first. It is the
address that the row of the transaction names, in the tab of the
transactions. Only the spreadsheet has these rows: no `--csv` option writes
them to a file.

In every tab, the tags of a row share one cell, with a space between them. A
column for each tag would not work, since the columns of a tab would then
change with the tags of the wallet. To find the rows that have a tag, filter
the spreadsheet on a word in that cell. Without `--tag` or `--untagged`, the
tabs hold every row, whatever its tags.

The tabs hold every portfolio of every wallet, whichever one is active, on the
[networks `hw` shows](#shown-and-hidden-networks). A hidden network has no row
in any tab. Each row starts with the columns that say where it belongs: its
wallet, its portfolio, its network and its type. So you can sort and sum one
tab across every wallet. In the tab of the portfolios, the column `Active`
says which portfolio is active. The receiving addresses and the change
addresses share a tab, the receiving ones first, and the column `Chain` tells
them apart.

Each cell has a type, unlike in a CSV file. A label is text, whatever it
starts with, and keeps its leading zeros. An amount is a number of satoshis,
shown with thousands separators. A time is a date and time. Each time has
three columns: the time in UTC, the time on the clock of the computer, and
how far that clock was from UTC. The header of each tab stays at the top as
its rows scroll. `hw` reads all the listings at one moment, so the tabs
agree with each other.

```console
hw exportsheet wallet.ods
```

**The rows of a tag.** With `--tag` or `--untagged`, each tab holds only the
rows that its listing shows with the same option. The spreadsheet then holds
one group of your [tags](#tags-on-addresses-and-transactions), and not the
whole wallet:

```console
hw exportsheet business.ods --tag business    # all that has the tag business
hw exportsheet unsorted.ods --untagged        # all that has no tag
```

| Tab | With `--tag TAG`, it holds | With `--untagged`, it holds |
|---|---|---|
| `Addresses`, `Balances`, `Coins` | The addresses that have the tag, what they hold and their coins | The addresses that have no tag, what they hold and their coins |
| `Transactions`, `Pending transactions`, and the tabs of their addresses | The transactions that have the tag | The transactions left to sort, as [`txs --untagged`](#hw-txs) lists them |
| `Address book` | The contacts that have the tag | The contacts that have no tag |
| `Portfolios` | Every portfolio | Every portfolio |

You can give `--tag` several times. A tab then holds the rows that have any
one of the tags. With `--untagged` too, it also holds the rows that have no
tag. `exportsheet` has no `--address-tag`. To find the transactions by the
tags of their addresses, filter the spreadsheet on the column `From tags`,
`To tags` or `Change tags`.

> **Good to know**
>
> - **Amounts of Bitcoin and of Bitcoin Cash share a column** when the wallet
>   holds both. The Network column tells them apart: filter on it before you
>   sum.
> - **Filter the dead transactions out before you sum the addresses.** A dead
>   transaction is one that was replaced or dropped. The tab
>   `Transaction addresses` still holds its rows, as the tab of the
>   transactions still holds the transaction. Their column `Status` reads
>   `dead`.
> - **A payment between two of your portfolios is in the tab twice.** It is a
>   transaction of each portfolio, so each lists its addresses. The first
>   columns say which portfolio a row belongs to, and `Held by portfolio`
>   says which portfolio holds the address.
> - **A payment you received lists your own addresses under `to`.** The other
>   addresses that the payer paid, the payer's change among them, do not
>   concern your portfolio and have no row. An address that the payer spent
>   from has a row without a value. A pending transaction is not sent yet,
>   so it has a row for every address, each with its value, as
>   [`show`](#hw-show) prints it.
> - **A spreadsheet of a tag does not say that it leaves rows out.** A tab
>   has headings and rows, and no room for a note. Name the file after what
>   it holds, such as `business.ods`.
> - **An input or an output without an address has no row.** For instance, an
>   output that only writes a note in the chain pays no address.
> - **The spreadsheet tells your history.** `hw` makes it readable by you
>   alone. A spreadsheet program may still keep copies, such as backups and
>   recent files.

#### `hw exportjournal`

`hw exportjournal FILE` writes the transactions of the wallet to a new
journal for [ledger], the command-line accounting program. A journal is a
plain text file that lists each transaction as an accounting entry: which
accounts the money came from, and which accounts it went to. ledger reads
the file and makes reports from it, such as what each account holds and what
you spent. The journal follows the rules of double-entry accounting.

`hw` knows what happened on the chain: what each portfolio received and paid,
the fee of each payment, and which payments went from one of your portfolios
to another. It does not know what a coin was worth on that day. It knows
what the money was for only when one of your tags says so, as **Tags as
categories** explains below. An accounting program keeps the rest. So the
journal goes one way: `hw` writes it, and you include it in a journal of
your own, where you keep your prices and your reports.

```console
$ hw exportjournal hw.ledger
Written to hw.ledger
```

Each transaction is one entry. An entry has a line for each account it
changes, called a posting, and its postings add up to zero. In this example,
a salary comes in, a rent goes to a contact of the
[address book](#the-address-book), and a payment moves money from
`portfolio0` to `portfolio1`:

```
2023-11-14 * (a16ab47f) Salary of November
    ; txid: a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e
    ; time: 2023-11-14 22:13:20 UTC
    ; block: 800000
    Assets:hw:wallet0:portfolio0   0.00050000 BTC
    Income:Unknown                -0.00050000 BTC

2023-11-14 * (ab6c4eff) Landlord
    ; txid: ab6c4eff375442b68eab368ceeed561b04b7d6e309564b8cf95cd584565f7388
    ; time: 2023-11-14 22:13:20 UTC
    ; block: 800003
    ; :rent:
    Expenses:Unknown               0.00010000 BTC
    ; address: 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    ; contact: Landlord
    Expenses:Fees:Mining           0.00002260 BTC
    Assets:hw:wallet0:portfolio0  -0.00012260 BTC

2023-11-14 ! (60d5fa22) Transfer between portfolios
    ; txid: 60d5fa22cea9fbcd0e78c8fca744b004e55c29a13e58306ca0fb7f0878f9b69d
    ; time: 2023-11-14 22:13:20 UTC
    Assets:hw:wallet0:portfolio1   0.00020000 BTC
    Expenses:Fees:Mining           0.00002260 BTC
    Assets:hw:wallet0:portfolio0  -0.00022260 BTC

2023-11-14 * Balances of hw
    Assets:hw:wallet0:portfolio0   0.00000000 BTC = 0.00015480 BTC
    Assets:hw:wallet0:portfolio1   0.00000000 BTC = 0.00020000 BTC
```

**The accounts.** A portfolio is an account. The other accounts say where its
money came from and where it went.

| Account | What is posted to it |
|---|---|
| `Assets:hw:wallet0:portfolio0` | What the portfolio gains or loses, fee included. The name is made of the indexes of the wallet and of the portfolio, which never change, so the account keeps its name |
| `Income:Unknown` | What comes from others, when no tag says where from. `hw` cannot tell a salary from a refund |
| `Expenses:Unknown` | What goes to others, when no tag says what it is for. Each address paid has a posting. The address is below the posting, with the label of its contact when the address book has it |
| `Expenses:Fees:Mining` | The fee of a transaction the wallet made |
| An account of yours, such as `Expenses:Housing:Rent` | What a payment with a tag pays or receives, once you give the tag that account |

The network is not a part of the account. It is the commodity of the amounts,
which is the word of ledger for a currency. The commodity is the name of the
network in capitals, such as `BTC` and `BCH`. So one account holds the coins
of a portfolio on every network, as the portfolio does in `hw`, and ledger
never adds the coins of a test network to those of Bitcoin. ledger asks for
double quotes around a commodity with a digit, so the coins of `btctest4`
are written `"BTCTEST4"`.

**The journal covers every portfolio of every wallet**, whichever portfolio
is active. A payment from one of your portfolios to another is one entry,
which moves the money between their two accounts. Its fee is the only
expense. A journal of one portfolio alone would count that money as spent.

**Tags as categories.** `hw` cannot tell a rent from a gift, but you can. Put
a [tag](#tags-on-addresses-and-transactions) on the payment, and give the tag
an account. The table `journal-accounts` of the
[configuration file](#the-configuration-file) gives accounts to tags, with a
line for each tag:

```toml
[journal-accounts]
rent = "Expenses:Housing:Rent"
salary = "Income:Salary"
```

`hw` then posts a payment that has the tag `rent` to `Expenses:Housing:Rent`,
and no longer to `Expenses:Unknown`. `hw send ADDRESS:0.01 --tag rent` tags
the payment as you make it, and `hw tag TXID rent` tags it afterwards. `hw`
posts a payment to a contact that has the tag `rent` to the same account. So
tag the landlord once, and `hw` files every rent. A payment that comes from a
contact works the same way: tag your employer `salary`, and `hw` files every
salary. [`hw txs --untagged`](#hw-txs) lists the transactions that no tag
sorts yet, so you see what is left to tag. With this table, and the salary
tagged `salary`, the first two entries of the example become:

```
2023-11-14 * (a16ab47f) Salary of November
    ; txid: a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e
    ; time: 2023-11-14 22:13:20 UTC
    ; block: 800000
    ; :salary:
    Assets:hw:wallet0:portfolio0   0.00050000 BTC
    Income:Salary                 -0.00050000 BTC

2023-11-14 * (ab6c4eff) Landlord
    ; txid: ab6c4eff375442b68eab368ceeed561b04b7d6e309564b8cf95cd584565f7388
    ; time: 2023-11-14 22:13:20 UTC
    ; block: 800003
    ; :rent:
    Expenses:Housing:Rent          0.00010000 BTC
    ; address: 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    ; contact: Landlord
    Expenses:Fees:Mining           0.00002260 BTC
    Assets:hw:wallet0:portfolio0  -0.00012260 BTC
```

| What has the tag | What the tag files under its account |
|---|---|
| The transaction | All that the transaction pays to others, or receives from them |
| The contact of an address that the transaction pays | What the transaction pays to that address, when no tag of the transaction has an account |
| A contact that a payment you received came from | All that the payment brings, when no tag of the transaction has an account |

The tags of the transaction come before the tags of a contact. A tag on a
transaction is about that one payment. A tag on a contact is about every
payment to the contact or from it. So when you tag the deposit you paid to
the landlord `deposit`, `hw` does not file it as a rent.

The journal names the tags and their accounts in its first comment, so that
its reader knows why a payment is where it is:

```
; The transactions of every portfolio of hw on the chain, as a journal for ledger.
; hw writes this file whole each time. Do not edit it: your changes would be lost.
;
; Tags that stand for an account, from the configuration of hw:
;   rent    Expenses:Housing:Rent
;   salary  Income:Salary
```

An account is made of names with a colon between them. A name is made of
letters, digits, single spaces and the signs `- _ & ' . / +`, and the account
starts with a letter. `hw` runs no command while the table has an account of
another form, or a word that cannot be a tag. Its message says which line to
change:

```console
$ hw exportjournal hw.ledger
The configuration file /home/you/.hw/config.toml could not be read, at line 9:
the tag "rent" of "journal-accounts" has "Expenses::Rent", which is no account
of a journal: a name is missing before or after a colon
```

**An entry.** Its first line gives a date, a state, a code and a name. Its
notes and its tags are on the lines below.

| Part | What it holds |
|---|---|
| The date | The day of the transaction on the clock of the computer. The note `time` gives the time in UTC |
| The state | `*` for a confirmed transaction, which ledger calls cleared, and `!` for an unconfirmed one, which it calls pending |
| The code | The [short id](#short-ids) of the transaction, which finds it in `hw`, as in `hw show ab6c4eff` |
| The name | The label of the transaction. When it has none, the label of the contact that the payment pays. When there is none either, the label of the address that received the payment. Otherwise `Payment received`, `Payment sent`, `Internal transfer` or `Transfer between portfolios` |
| The notes | `txid`, the id in full, `time`, and `block`, the height of the block of a confirmed transaction |
| The tags | The [tags](#tags-on-addresses-and-transactions) of the transaction, between colons, as ledger writes tags |

**What the journal leaves out.** A dead transaction, one that was replaced or
dropped, has no entry, since its money never moved. A
[pending transaction](#hw-pendingtxs) has none either, since it is not on the
chain yet. The journal leaves out the
[hidden networks](#shown-and-hidden-networks), as `txs` does.

**The order.** The entries of one network come together, in the order of the
chain, the oldest first. The entries of the next network follow them.

**The balances.** The last entry states what each portfolio holds on each
network, as [`txs`](#hw-txs) shows it beside the newest transaction. It moves
no money, and has the date of the newest entry. ledger adds up the entries of
the file, and refuses the file when a sum differs from the balance stated. So
ledger refuses a file that was cut short, or that someone edited by hand.

**The transactions of a tag.** With `--tag`, `--address-tag` or `--untagged`,
the journal holds only some of the transactions: those that
[`txs`](#hw-txs) lists with the same options. Use them for the journal you
hand to the accountant of a business, who must not see the rest:

```console
hw exportjournal business.ledger --tag business        # the transactions tagged business
hw exportjournal faucets.ledger --address-tag faucet   # those at an address or a contact tagged faucet
hw exportjournal unsorted.ledger --untagged            # those left to sort
```

The journal holds these transactions from every portfolio. A payment between
two of your portfolios keeps its whole entry when the options select either
side of it, so the entry still adds up. The first comment of the file says
which transactions it holds:

```
; The transactions of every portfolio of hw on the chain, as a journal for ledger.
; hw writes this file whole each time. Do not edit it: your changes would be lost.
;
; Only the transactions tagged business are in this file.
; It states no balances at its end: the transactions left out moved money too.
```

Such a journal has no entry `Balances of hw`. The transactions it leaves out
moved money too, so its entries do not add up to what the portfolios hold, and
ledger would refuse the file. The journal that `hw`
[keeps up to date](#a-journal-kept-up-to-date) always holds every
transaction.

**The same history gives the same file**, byte for byte. The file does not
say when it was written. So a journal you keep under version control changes
when your history or the table `journal-accounts` changes, and at no other
time.

**The declarations.** After its first comment, the file declares the
commodities, the accounts, and the names of the notes and of the tags it
uses. ledger asks for these declarations when you run it with its strict
checks, such as `--pedantic`. The file passes those checks as it is.

**Reports.** These commands of ledger run on the file as `hw` wrote it:

```console
ledger -f hw.ledger balance                     # what each portfolio holds, what came in and what went out
ledger -f hw.ledger register Assets:hw          # every movement of the portfolios, with a running total
ledger -f hw.ledger --cleared balance Assets    # what the portfolios hold, confirmed transactions only
ledger -f hw.ledger register Expenses:Fees      # the fees paid, one by one, and their total
ledger -f hw.ledger balance Expenses:Housing    # what your tags filed under Expenses:Housing
ledger -f hw.ledger register %rent              # the transactions tagged rent
ledger -f hw.ledger register @Landlord          # the entries named Landlord
ledger -f hw.ledger print code ab6c4eff         # one transaction, by its short id
ledger -f hw.ledger --pedantic balance          # the same balance, with the strict checks of ledger
```

**In a journal of your own.** Include the file, and add what `hw` does not
know. An `alias` gives the account of a portfolio a name of your own. A `P`
line gives a price, and the option `-X` of ledger values a report at that
price. An entry of your own says what an unknown posting was: the entry
below moves the salary out of `Income:Unknown`. A tag with an account does
the same for every payment that has the tag, and you write no entry.

```
alias Assets:hw:wallet0:portfolio0=Assets:Bitcoin:Spending
alias Assets:hw:wallet0:portfolio1=Assets:Bitcoin:Savings

commodity EUR
    format 1000.00 EUR

P 2023-11-14 BTC 33000.00 EUR

include hw.ledger

2023-11-14 * Salary of November
    Income:Unknown   0.00050000 BTC
    Income:Salary   -0.00050000 BTC
```

```console
ledger -f my.ledger balance Assets              # the portfolios under their new names
ledger -f my.ledger -X EUR balance Assets       # the same, valued in euros at the last price
ledger -f my.ledger balance Income              # the salary, out of Income:Unknown
```

> **Good to know**
>
> - **`hw` writes the journal whole each time, to a new file.** A transaction
>   that confirms, one that is replaced, and a reorganisation of the chain
>   all show in the next file. So delete the old file, then run
>   `exportjournal` again, or let `hw`
>   [keep the journal up to date](#a-journal-kept-up-to-date). Do not edit
>   the file: you lose your changes when you delete it. Keep them in a
>   journal of your own.
> - **Post nothing else to the accounts of the portfolios.** The balances at
>   the end count the entries of `hw` alone. An entry of your own that posts
>   to `Assets:hw:wallet0:portfolio0` makes ledger refuse the file. Post to
>   `Income:Unknown` or `Expenses:Unknown` instead, as the example above does.
> - **A payment with two tags that have different accounts is not filed.**
>   `hw` cannot tell which account you mean, and cannot split the amount. The
>   payment stays in `Expenses:Unknown` or `Income:Unknown`, where you see
>   it. When the two tags have the same account, `hw` files the payment
>   there.
> - **A tag has one account, whichever way the money goes.** `hw` posts a
>   payment you received with the tag `rent` to `Expenses:Housing:Rent`, as a
>   negative amount. So a refund lowers the expense, as accounting expects.
> - **A tag files what goes to others or comes from them, and nothing else.**
>   The fee stays in `Expenses:Fees:Mining`, and the accounts of the
>   portfolios do not change. A payment between your own addresses has nothing
>   to file.
> - **A payment you received has one posting, however many addresses it came
>   from.** When one of those addresses is a contact, the tags of that
>   contact file the whole payment, since a transaction most often has one
>   sender. When those addresses are contacts whose tags have two different
>   accounts, the payment stays in `Income:Unknown`.
> - **A line of the table needs its header.** The configuration file that
>   `hw` writes shows `# [journal-accounts]` as a comment. `hw` does not read
>   a line for a tag below it until you remove the `#` of that header. `hw`
>   warns about such a line on each command, as
>   [Configuration](#the-configuration-file) shows.
> - **No tag files a payment under `Assets:hw`.** These are the accounts of
>   the portfolios, whose balances the journal states. `hw` refuses such a
>   line of the table.
> - **A tag you rename keeps its old name in the table.** `hw` does not
>   write the table. After [`renametag`](#hw-renametag), change the line
>   yourself, or its payments go back to the unknown accounts.
> - **The table stays on this computer.** It is in the configuration file,
>   not in the database, so [`exportwallet`](#hw-exportwallet) does not
>   carry it. The names of your accounts belong to your books. A tag of the
>   table need not be a tag of the wallet yet.
> - **The journal holds no price and no cost.** `hw` would have to ask a
>   server for prices, and the rules for the cost of a coin differ from one
>   country to the next.
> - **The fee of a payment you received is not yours.** Its payer paid it,
>   and it has no posting. A transaction that also spends coins of another
>   wallet has no posting for its fee either, since `hw` cannot tell who paid
>   it. `hw` then posts all that left the portfolio to `Expenses:Unknown`, or
>   to the account of a tag of the transaction.
> - **The dates may run a little out of order.** A block may be dated before
>   the block under it, as [`txs`](#hw-txs) explains. The entries keep the
>   order of the chain. That is why the balances are stated once, at the end:
>   a tool that checks balances by date would fail on a balance stated between
>   two such entries.
> - **A transaction without a label is named after an address label only
>   when it pays no one else.** The change of a payment may come back to a
>   labelled address of yours, and that label does not say who the payment
>   went to.
> - **A label is written on one line.** A line break in a label becomes a
>   space, and so does a run of spaces. A label can come from a file, and a
>   line break would let it add lines to the journal.
> - **The journal tells your history.** `hw` makes it readable by you alone.

#### `hw report`

`hw report FILE` writes a report to a new PDF file. Print it, keep it with
your records, or hand it to a tax office, an accountant or the other side of
a payment. There are three reports, in one style, and an option says which
one to write:

| Option | Report |
|---|---|
| `--balances` | A balance statement: what every portfolio of every wallet held on a day, on each network, and what each of its addresses held |
| `--transactions` | An account statement: the transactions of every portfolio over a period, with the balance after each, between an opening and a closing balance |
| `--payment TXID` | A payment confirmation: what one transaction paid, to which addresses and on which day, with what lets anyone check it on the chain |

```console
$ hw report balances-2025.pdf --balances --date 2025-12-31
Written to balances-2025.pdf
$ hw report statement-2025.pdf --transactions --from 2025-01-01 --to 2025-12-31
Written to statement-2025.pdf
$ hw report rent-november.pdf --payment a1075db5
Written to rent-november.pdf
```

| Option | What it adds |
|---|---|
| `--date YYYY-MM-DD` | With `--balances`: the statement is for the end of that day. Without it, the statement is for now. |
| `--from YYYY-MM-DD` | With `--transactions`: the first day of the period. Without it, the first day of the month of `--to`, or of the current month. |
| `--to YYYY-MM-DD` | With `--transactions`: the last day of the period. Without it, the period runs up to now. |
| `--tag TAG` | With `--transactions`: only the transactions that have this [tag](#tags-on-addresses-and-transactions). With `--balances`: only the addresses that have it. Give it once for each tag. |
| `--address-tag TAG` | With `--transactions`: only the transactions that spend from or pay an address that has this tag, or a contact that has it. |
| `--untagged` | With `--transactions`: only the transactions left to sort, as [`txs --untagged`](#hw-txs) lists them. With `--balances`: only the addresses that have no tag. |
| `--wallet M` | Reports on `walletM` only. Without it, every wallet, or with `--portfolio`, the wallet of the active portfolio. With `--payment`: the side the confirmation is for. |
| `--portfolio N` | Reports on `portfolioN` only, of the wallet `--wallet` names, or of the wallet of the active portfolio. With `--payment`: the side the confirmation is for. |
| `--holder NAME` | Names who holds the wallets. Without it, the report names nobody. With `--payment`: the payer of a payment you sent, the recipient of one you received. |
| `--price NETWORK=PRICE` | The price of one coin of a network, such as `btc=87500`. Give it once for each network. With `--payment`: the one price of the network of the payment. |
| `--currency CODE` | The currency of the prices, such as `CHF`. It goes with `--price`. |
| `--price-source TEXT` | Where the prices come from, to say on the report. It goes with `--price`. |
| `--counterparty NAME` | With `--payment`: names the other side. It is the recipient of a payment you sent, and the payer of one you received. |
| `--purpose TEXT` | With `--payment`: what the payment is for, such as the number of an invoice. |
| `--address ADDRESS` | With `--payment`: confirms only what the transaction pays to this address. Give it once for each address to list. |

`hw` works out the three reports from the transactions it has synced, so run
[`sync`](#hw-sync) first. All three count confirmed transactions only.

**The balance statement** says what you held on one day. It is the
declaration of assets that a wealth tax asks for. It goes from the general
to the detail:

- The facts: the holder, the date the statement is for, when and by whom it
  was made, its reference, the wallets and the assets it covers.
- A summary: the balance of each asset, Bitcoin and Bitcoin Cash, and how many
  addresses hold coins.
- The balance of each portfolio, with its description, and the total of each
  asset.
- The balance of each address that holds coins, with its
  [path](#hw-addresses) and its label, and the total of each portfolio.
- The blocks the statement rests on, each with its height, its date and its
  hash.
- Notes that say who prepared the statement and how the balances were worked
  out.

Without `--date`, the balance statement is for now. With `--date`, it is for
the end of that day on the clock of this computer. So `--date 2025-12-31`
tells what the wallet held as the year 2025 ended. The day must be over. The
statement writes that moment as `31 December 2025, 24:00`, with the offset of
the clock from UTC that night.

The balance of an address on a day is what the transactions up to that day
paid to it, less what they spent from it. A statement for a day counts the
blocks dated on that day or before. A statement for now says in a note how
many unconfirmed transactions it left out, and what they would change.

**A statement of some addresses.** With `--tag`, the balance statement lists
only the addresses that have a [tag](#tags-on-addresses-and-transactions).
With `--untagged`, it lists only the addresses that have none. These are the
addresses that [`balances`](#hw-balances) lists with the same options. Use
them for the statement of one group of coins. For instance, the accountant
of a business gets what the coins of the business held as the year ended,
and does not see the rest of the wallet.

```console
$ hw report kyc-2025.pdf --balances --tag kyc --date 2025-12-31
Written to kyc-2025.pdf
```

Run `hw balances` with the same options first. The statement lists the same
addresses, with what they held on its day. You can give `--tag` several
times. With `--untagged` too, the statement also lists the addresses that
have no tag.

Such a statement is still a balance statement, and keeps its title. Each
address has its whole balance, and the totals add up. It differs in how it
describes itself:

- The line above the title reads "Statement of cryptocurrency holdings,
  selected by tag". The facts say which addresses it lists, as in "Only those
  tagged kyc", and so does the first note on the figures.
- A total is named `Total` alone. It is the total of the addresses listed,
  and not what a portfolio or the wallet holds.
- A portfolio that holds nothing at these addresses is not listed.
- A statement for now gives no count of the unconfirmed transactions it left
  out.

It takes `--price`, as any balance statement does, and then needs a price
for each asset that these addresses hold. It does not take `--address-tag`,
which selects transactions.

**The account statement** lists the transactions of a period, with the
balance after each. It is like the statement a bank sends each month, for a
wallet that no bank holds. Each portfolio on each network is one account.
The statement goes from the general to the detail too:

- The facts: the holder, the period, when and by whom the statement was made,
  its reference, the wallets and the assets it covers.
- A summary: for each asset, the opening balance, what was received from
  others, what was sent to others, the closing balance, and how many
  transactions the period has.
- The same four amounts for each portfolio, and the total of each asset. A
  row above the total states what moved between the portfolios.
- The account of each portfolio: its opening balance, then each transaction
  with its date, what it is, what the portfolio received or sent, and the
  balance after it, then the totals and the closing balance.
- The blocks the statement rests on, and notes that say how it was worked
  out.

Without `--from` and `--to`, the account statement is for the current month,
up to now. `--from` gives the first day of the period and `--to` its
last day, on the clock of this computer. `--from` alone gives a period that
runs up to now. `--to` alone gives the month of that day, from its first day:

```console
$ hw report october.pdf --transactions --to 2025-10-31
Written to october.pdf
```

The opening balance is the balance a balance statement gives for the day
before the period. The closing balance is the one it gives for the last day.
So the statements of two months in a row fit together: the first closes on
the balance that the second opens with.

Each line describes its transaction by its [label](#hw-label). When the
transaction has none, the line uses the label of the
[contact](#hw-contacts) it pays, or else the label of the address that
received it. Without any label, the description is `Payment received`,
`Payment sent` or `Internal transfer`. The journal of
[`exportjournal`](#hw-exportjournal) names its entries the same way. Below
the description are the txid in full and the height of the block, so that a
reader can check the line against the chain. An amount sent includes the
fee, and the line states the fee when the portfolio paid it.

A payment from one portfolio to another is a transfer: the money stays
yours. It is a line of both accounts, sent by the first and received by the
second, and each account adds up on its own. Each of the two lines names the
other portfolio: `Transfer to portfolio1` and `Transfer from portfolio0`.
With a label, the line keeps it, as in `Nest egg (transfer to portfolio1)`.
The amounts of an asset leave the transfer out: they are what your
portfolios received from others and sent to others. Only its fee counts,
as sent. So a reader who takes "Received" for income is not misled. In the
table of the portfolios, the row `Transfers between portfolios` takes what
moved off both columns, between the portfolios and their total:

```
PORTFOLIO                      OPENING BALANCE    RECEIVED        SENT  CLOSING BALANCE
portfolio0                          0.00000000  0.00050000  0.00041000       0.00009000
portfolio1                          0.00000000  0.00040000  0.00000000       0.00040000
Transfers between portfolios                   -0.00040000 -0.00040000
Total Bitcoin                       0.00000000  0.00050000  0.00001000       0.00049000
```

**A statement of some transactions.** With `--tag`, `--address-tag` or
`--untagged`, the account statement lists only some of the transactions:
those that [`txs`](#hw-txs) lists with the same options. Use them for the
statement you hand to the accountant of a business, who must not see the
rest of the wallet:

```console
$ hw report business-2025.pdf --transactions --tag business --from 2025-01-01 --to 2025-12-31
Written to business-2025.pdf
```

The three options add up, as they do in `txs`: the statement lists the
transactions that match any one of them. Run `hw txs` with the same options
first. Of the transactions that `txs` lists, the statement has those that
are confirmed and in its period, for each portfolio.

Such a statement differs from a whole account statement:

- It is titled "Digital Asset Transaction Statement", under the line
  "Statement of cryptocurrency transactions, selected by tag". Its facts say
  which transactions it lists, as in "Only those tagged business", and so
  does its first note on the figures.
- It states no balance: no opening balance, no closing balance, and none
  after a line. The transactions it leaves out changed the balances too. So
  an opening balance and the lines listed would not add up to the closing
  balance.
- Its summaries give what the lines received, what they sent, and the net
  amount, for each asset and for each portfolio. The lines of each portfolio
  end with their totals.
- It does not take `--price`, since a price values a closing balance.

A payment from one of your portfolios to another can be listed for both
portfolios. A tag on the transaction lists it for both. A tag on an address
lists it for the portfolio of that address only.

**The payment confirmation** states what one transaction paid. It is like
the advice a bank prints for one transfer, for a wallet that no bank holds.
Send it to the person you paid, or file it with the invoice it settles.
`--payment` names the transaction by its txid or its
[short id](#short-ids), as [`txs`](#hw-txs) lists it:

```console
$ hw report rent-november.pdf --payment a1075db5 \
     --holder "Jane Doe" --counterparty "ACME Ltd" \
     --purpose "Invoice 2025-014"
Written to rent-november.pdf
```

For a payment to one address, the confirmation fits on one page. It holds:

- The facts: the payer, the recipient, the purpose, the date of the payment,
  the amount paid, the asset, when and by whom the confirmation was made, and
  its reference.
- Each address paid with its amount, and their total. Below them, the fee and
  what left the wallet in all.
- The txid, whole, the block of the transaction with its height, its date and
  its hash, and how many confirmations it has.
- Notes that say who prepared the confirmation, and how anyone can check the
  payment on the chain.

For a payment you received, the report is a receipt instead, titled "Digital
Asset Payment Receipt". It lists what the transaction paid to your addresses,
and states no fee, since the payer paid it.

`--holder` names you, and `--counterparty` the other side: the recipient of a
payment you sent, the payer of one you received. `--purpose` says what the
payment is for. The report shows a name and a purpose only when you give
them.

A transaction may pay several addresses, for instance when you made it with
several recipients. With `--address`, the confirmation lists only what the
transaction pays to that address, so that one supplier does not read what
another was paid. Give it once for each address to list:

```console
$ hw report acme.pdf --payment a1075db5 \
     --address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
Written to acme.pdf
```

The confirmation then states what the other payments of the transaction add
up to, without their addresses.

**One wallet or one portfolio.** A report covers every portfolio of every
wallet, whichever portfolio is active. `--wallet` and `--portfolio` narrow
it down to one wallet or to one portfolio, as they do for the file of
[`exportwallet`](#hw-exportwallet):

```console
hw report all.pdf --balances                          # every wallet
hw report business.pdf --balances --wallet 1          # every portfolio of wallet1
hw report savings.pdf --transactions --portfolio 2    # portfolio2 of the wallet of the active portfolio
hw report till.pdf --transactions --wallet 1 --portfolio 0   # portfolio0 of wallet1
```

A statement that is narrowed down says so. Its facts read `wallet1 only` or
`portfolio2 only`, and a note says that the holder has other wallets and
portfolios. Its reference differs from that of the report of everything.

For a payment confirmation, the two options say which side the report is for.
Without them, the report is for all your wallets together. A payment from
one of your portfolios to another then pays nobody else, so `hw` refuses it.
With them, the same transaction is a payment. Name the portfolio that paid,
and the report is a confirmation. Name the one that was paid, and it is a
receipt:

```console
hw report sent.pdf --payment a1075db5 --portfolio 0       # portfolio0 paid: a confirmation
hw report received.pdf --payment a1075db5 --portfolio 1   # portfolio1 was paid: a receipt
```

**The value.** `hw` knows no price and asks no server for one. A tax office
names the price it wants, and most publish it. Give the price of a coin of
each network with `--price`, and their currency with `--currency`:

```console
$ hw report balances-2025.pdf --balances --date 2025-12-31 \
     --holder "Jane Doe" \
     --price btc=87500 --price bch=420.50 --currency CHF \
     --price-source "Rate list of the tax administration, 31 December 2025"
Written to balances-2025.pdf
```

The balance statement then states the price and the value of each asset and
their total, and the value of each portfolio. The account statement states the
price and the value of each closing balance, and their total. A payment
confirmation states the value of each amount and of their total, with the
price beside it. A payment is in one asset, so it takes one price, that of
its network. You choose the price for the day of the payment. A value is the
balance times the price, rounded to the hundredth. Type a price with a point
before its decimals and no thousands separator, as in `87500.50`.

**The title.** A report says at its top what kind of asset it is about. A
balance statement is titled "Digital Asset Balance Statement", under the line
"Statement of cryptocurrency holdings". An account statement is titled
"Digital Asset Account Statement", under the line "Statement of cryptocurrency
transactions". A payment confirmation is titled "Digital Asset Payment
Confirmation", under the line "Confirmation of a cryptocurrency payment". The
title is at the foot of each page too. So nobody takes a report for the
statement of a bank account, even from one page of it.

**Who made it.** A report looks like one a bank sends, and no bank made it.
So it says that the holder prepared it from public blockchain data, and that
nobody audited it. It says that the wallets are self-custody wallets, whose
keys the holder keeps, and not accounts at an exchange or with an online
wallet. It says that it covers these wallets alone. A payment confirmation
says the same of the payer, or of the recipient for a receipt. It also says
that the blockchain does not tell who holds an address, so the names on it
are as you gave them.

**The reference.** A report has a reference such as `3F9A-52C1`, in its facts
and at the foot of each page. `hw` works it out from the date or the period,
the holder, the balances or the transactions, and the prices. For a payment
confirmation, it works the reference out from the transaction, the names,
the purpose, the amounts and the price. Two reports with the same reference
state the same thing, so you and a tax office can tell which one you both
have. A report for a past day or a past period keeps its reference however
often you make it, as long as nothing in it changes. So does a payment
confirmation, while its transaction gains confirmations.

**The blocks.** A report names the blocks it rests on, on each network, by
their height and their hash: the block the last sync reached, and the last
block it counts. An account statement also names the last block before its
period. A payment confirmation names the block of its transaction. The hash
lets a reader check the report against the chain. `hw` asks the server for
the hashes it does not have. This is the only thing that a report asks of a
server.

**The look.** A report is a plain document on A4 paper, with one typeface, in
black and one dark blue. Every amount is in coins with eight decimals, in a
column aligned on the right, and a total stands between a rule and a double
rule. Each page names its report, and gives its number and the number of
pages.
A heading is never the last thing on a page: it moves to the next page with
the start of what it heads, such as the first row of a table. The first page
has the logo of `hw` at its top right, small, in the dark blue and the grey
of the report: the bits of the two letters as two rows of squares, then the
name. Nothing else names `hw`. The other pages do not, and the properties of
the file name no program. The file carries its typeface, Liberation Sans, so
it looks the same on every computer and on paper. The typeface is compiled
into `hw`. It is free to ship and to embed, under the SIL Open Font License,
whose text is in the folder `fonts` of the source.

> **Good to know**
>
> - **A balance statement of a tag uses the tags as they are now, even for a
>   day in the past.** `hw` keeps no history of the tags. A statement for the
>   end of last year lists the addresses that have the tag today, with what
>   they held that day. An address you tagged since then is in it, and an
>   address you took the tag off is not.
> - **`hw` refuses a statement for a day when the last sync stopped on that
>   day or before it.** The wallet then knows nothing of the rest of the day,
>   and the statement could miss a payment. Run `sync`, then `report` again.
>   The same holds for the last day of an account statement.
> - **`--date` is the day of a balance statement, and `--from` and `--to` the
>   period of an account statement.** `hw` refuses a date option that does
>   not go with the report, so that it never ignores one without telling
>   you. A payment confirmation takes none: its date is that of its
>   transaction. In the same way, `--counterparty`, `--purpose` and
>   `--address` go with `--payment` alone.
> - **A payment confirmation needs a transaction in a block.** A transaction
>   that is not confirmed yet could still be replaced, and the report would
>   confirm a payment that never happened. Run `sync` once the transaction
>   is in a block. `hw` also refuses a transaction that was replaced or
>   dropped.
> - **A payment confirmation uses none of your labels.** A statement
>   describes a transaction by its [label](#hw-label) or by a
>   [contact](#hw-contacts), since it goes to your accountant. A confirmation
>   goes to the other side, and a label is a private note. It shows the names
>   and the purpose you type, and nothing else.
> - **A payment confirmation lists no change.** What the transaction returns
>   to you is no payment. A reader who looks the transaction up sees one
>   output more than the confirmation lists, and a note says why.
> - **`hw` writes no confirmation for a transaction that spends coins of
>   yours and coins of others.** It cannot tell who paid what, so it refuses
>   the transaction.
> - **A transaction between your own addresses is no payment.** `hw` refuses
>   it, and tells how to confirm it when it goes from one portfolio to
>   another.
> - **A payment from one portfolio to another is in both accounts** of an
>   account statement, and in neither amount of the asset. The asset counts
>   its fee alone, as sent.
> - **A transfer is taken off only when the statement has both accounts.**
>   With `--wallet` or `--portfolio`, a transfer to a portfolio left out is
>   counted as sent, and one from it as received: for what the statement
>   covers, the money went out or came in. A statement by tag takes a
>   transfer off when it lists it for both portfolios.
> - **A payment to another portfolio and to somebody else at once** is a
>   transfer in part. Its line says so, as in
>   `Payment sent (in part a transfer to portfolio1)`, and the asset counts
>   as sent what went to others.
> - **A portfolio has the same figures in a narrowed report as in the report
>   of everything.** `hw` works every report out from the history of every
>   portfolio, then shows the portfolios that `--wallet` and `--portfolio`
>   name. So the line of a payment to a portfolio left out still names
>   that portfolio, as in `Transfer to portfolio1`.
> - **`--wallet` or `--portfolio` that names everything changes nothing.** In
>   a database of one wallet, `--wallet 0` gives the report of everything,
>   with the same reference.
> - **An account statement values its closing balances alone.** The prices
>   are those of the end of the period, and each transaction would need the
>   price of its own day.
> - **A line of an account statement may be dated just outside the period.**
>   The statement counts whole blocks at both ends, as the note on the dates
>   of blocks below explains.
> - **Every network the wallet holds coins on needs a price**, once you give
>   one. A total that left an asset out would look complete. A network
>   without coins needs none.
> - **The addresses have no value beside them.** A tax office asks for the
>   total. The total of an asset is worked out from its total balance, so it
>   may differ by a hundredth from the sum of the portfolios above it.
> - **The statement shows the test networks `hw` shows.** A price of 0 for a
>   test network keeps its coins out of the total. To leave a test network
>   out, [hide it](#shown-and-hidden-networks) and write the statement again.
> - **A server that does not answer does not stop a report.** `hw` writes the
>   report all the same, and the report says that the hash of a block is not
>   available.
> - **A block may be dated before the block under it**, as
>   [`txs`](#hw-txs) explains. A statement for a day finds the last block
>   dated on that day or before, and counts every block up to it. So it may
>   count a block dated a few minutes after midnight. Counting each block by
>   its own date could keep a payment and leave out the payment it spends
>   from, and show a balance the wallet never held.
> - **The balances are those of the history.** `hw` adds up the transactions
>   it has synced, and does not take the balances the server gives. A
>   statement of now counts confirmed transactions alone, so it agrees with
>   [`balances`](#hw-balances) when nothing is unconfirmed.
> - **An address without a balance is not listed**, and a portfolio without
>   any is listed with a balance of zero. A network the wallet holds nothing
>   on is in the summary, with a balance of zero, and has no other table. In
>   an account statement, a portfolio that held nothing and had no
>   transaction has no account of its own.
> - **The typeface has the letters of Latin, Greek and Cyrillic.** A
>   character of another script in a label, a description or the name of the
>   holder, such as a Chinese or an Arabic one, is written as a question
>   mark.
> - **A report names no key and no fingerprint.** A balance statement holds
>   the addresses that hold coins, their paths and their labels. An account
>   statement holds the txids, and no address. A payment confirmation holds
>   one txid and the addresses it lists, without their paths.
> - **A report tells what you own**, and names you when you give `--holder`.
>   `hw` makes the file readable by you alone.
> - **`hw` never overwrites a report.** Give a new file name, or delete the
>   old file first.

## Transaction files

A transaction file carries one transaction from a computer to another, so
that an offline computer or another wallet can sign it. The option `--output`
and the command `exporttx` write such a file. The option `--input` and the
command `importtx` read one.

The file is a PSBT, the format of [BIP 174], so other wallets read it too. A
hardware wallet, or another wallet that has the keys, can sign the file in
place of the offline computer. `broadcast --input` then takes the signed
file, finalized or not.

### What a file holds

The standard fields of the format hold:

- The transaction.
- For each coin it spends, the whole transaction that the coin comes from.
- The keys of the portfolio, with their derivation paths, in the inputs that
  are not signed yet and in the outputs.
- The sighash to sign with.
- For each signed input, its final script. A signed input no longer names a
  key. The format asks this of a finalized input, and other wallets write it
  the same way. A field of `hw` still says which address of the portfolio the
  input spends from.

`hw` puts its own data in the proprietary fields of the format, under the
identifier `hw`. Other software ignores these fields. They hold:

- The network.
- The fee rate the transaction was built for.
- For a replacement, the transactions that it replaces.
- The label of the transaction.
- The labels of the addresses of the portfolio that the transaction spends
  from or pays.
- The labels of the coins it spends, as they are when `hw` writes the file.
- The path of the address that each input spends from, such as `/0/5`. This
  field stays after the input is signed, by `hw` or by another wallet, since
  the format lets a signed input keep the fields it does not define. With it,
  `hw` recognizes the input as the portfolio's on any computer that holds the
  portfolio, even an offline one that never gave out an address. `hw` derives
  the address at that path in each account of the portfolio, and checks which
  of them the coin pays.

The file carries no [tag](#tags-on-addresses-and-transactions). The tags of a
pending transaction and of its addresses stay in the database where you set
them.

Anyone who handles the file can change its labels. So `hw` shows a label with
its control characters written out, such as `\x1b` for an escape or `\n` for
a line break. A label then cannot rewrite what the screen shows, such as a
recipient.

The file names each key by the fingerprint of the mnemonic and by the full
path of the key. A hardware wallet needs both to find its own keys. `hw`
knows the fingerprint of every wallet from the moment it creates the wallet:
`newportfolio` and `recover` take it from the mnemonic, and `importwallet`
takes it from the file of the wallet. A file of another wallet may instead
name a key of the portfolio by the fingerprint of the account's own key. `hw`
reads such a file, and writes the fingerprint of the mnemonic in the file it
signs.

### Files of other wallets

`hw` reads the transaction files of other wallets as it reads its own. A file
can be in bytes or in base64 text, and in version 0 of the format or in
version 2 ([BIP 370]). `hw` writes the signed file in the version of the file
it read.

- **`importtx` and `broadcast --input`** take a file that another signer
  returned, finalized or not. `hw` finalizes an input that is signed but not
  final, once it has verified its signature. It takes one input at a time, as
  the format asks: an input whose signature does not verify stays as it is,
  and `hw` reads it as not signed. Such a file may lack the paths of its
  signed inputs, the fee rate and the label. `hw` keeps them from the pending
  transaction it holds.
- **`broadcast --input`** also takes the signed transaction alone, in
  hexadecimal, as a wallet gives it once finalized. The wallet must hold the
  transaction as pending.
- **`signtx --input`** signs a file that another wallet prepared for a
  portfolio of the wallet. It signs with the sighash ALL only, and only when
  every input that is not the portfolio's is signed already. It refuses a
  file in which an input has a signature of another sighash than the input
  asks for, since the format lets no wallet finalize such an input. The
  signed file keeps every field of the file it read, except the fields of the
  signed inputs that only the signers needed. `hw` adds the path of the
  address that each signed input spends from.

A file of another wallet names no network. `hw` takes the network from the
portfolio that the file involves. The test networks share their keys, so a
file may involve the same portfolio on several of them. `--network`, or the
default network, then decides. `show`, `broadcast` and `signsweep` also take
a file that involves no portfolio of the wallet. They read it on `--network`,
or else on the `default-network` of the configuration file.

`hw` refuses a file that involves no portfolio on the network it is read on,
but involves one on another network. It says "The transaction involves an
account of another network". The cause is a wrong `--network`, or a file of
the other chain. `hw` does not treat such a file as a transaction from
outside the wallet, since it would then sign or send it on the wrong chain.

### Paths `hw` refuses

`hw` signs only for the addresses of an account, which are at the paths
`/0/n` and `/1/n`. `show` and `signtx` both refuse a file that names a key of
an account at any other path. An output at such a path would belong to the
account, but the wallet would never see it, since `hw` scans those two chains
only.

`hw` also refuses an output that is too far along a chain of an account. The
limit is a thousand addresses past the last address of that chain that the
wallet holds, or that the transaction spends from. No scan of `hw` or of
another wallet looks that far, so a file could hide the change there, at
`/1/2000000000` for instance.

This limit can refuse a genuine file on an offline computer. A wallet that
never gives out an address holds none, so the limit counts from the coins
that the payment spends. When the wallet has used more than a thousand change
addresses, and the payment spends no coin near its change, `hw` refuses the
file. Prepare the payment again, and add a recent change coin to its coins
with `--coin`.

## Configuration

`hw` keeps everything in the directory `~/.hw`, which the
[setup](#the-setup) creates: the database `portfolios.sqlite`, the settings
in `config.toml`, the [backups](#automatic-backups), and the
[journal for ledger](#a-journal-kept-up-to-date) `portfolios.ledger` when you
ask for one. To keep this directory on an encrypted disk or a removable
drive, see [Keep `~/.hw` on another disk](#keep-hw-on-another-disk).

The settings of `config.toml` are:

| Key | Default | What it sets |
|---|---|---|
| `host` | `https://api.haskoin.com/` | The address of the haskoin-store server, with or without a slash at the end |
| `default-network` | `btc` | The network that a command uses when you give no `--network`. It must be one of `shown-networks`. |
| `shown-networks` | `["btc", "bch"]` | The [networks `hw` shows](#shown-and-hidden-networks). `hw` hides the others: it leaves them out of every answer, and refuses them as a `--network`. `sync` and `recover` still work on them. |
| `default-type` | `bip44` | The type of address that a command uses when you give no `--type`: `bip44` or `bip84`. The networks of Bitcoin Cash use `bip44` whatever this key says. |
| `unit` | `btc` | The unit of the amounts when you give no `--unit`: `btc`, `bit` or `sat` |
| `listing-view` | `compact` | The [view of a listing](#listings) when you give no `--compact`, `--verbose` or `--tabular`: `compact`, `verbose` or `tabular` |
| `listing-reverse` | `false` | Whether a [listing](#listings) shows its rows in the reverse order on the screen when you give no `--reverse` or `--no-reverse`: `true` or `false`. The reverse order shows the oldest transaction first, for instance. `--json` and `--csv` keep the usual order. |
| `gap` | `20` | How many unused addresses in a row `receive` gives out, and how far past the last used address of each account `sync` looks: a whole number from 1 to 1000. `sync` asks the server about this many addresses of each account every time, so a larger gap makes every `sync` slower. |
| `parallel-requests` | `1` | How many requests go to the server at once: a whole number from 1 to 100. The public server takes one at a time, so keep it at 1 unless you run your own haskoin-store. |
| `autobackup-database` | `true` | Whether `hw` [copies the database](#automatic-backups) into `~/.hw/auto-backups` after each command that adds a wallet or a portfolio. The same key covers the backup that `hw` makes there before `deletewallet`, `deleteportfolio` and `restoredatabase`. |
| `autobackup-labels` | `true` | Whether `hw` [copies the labels and the tags](#automatic-backups) into `~/.hw/auto-backups` after each command that changes them |
| `journal` | `false` | Whether `hw` keeps a [journal of the transactions for ledger](#a-journal-kept-up-to-date) in `~/.hw/portfolios.ledger`, and writes it again after each command that changes it |
| `journal-accounts` | no tag | A table that gives each tag an [account of the journal for ledger](#hw-exportjournal), with one line for each tag, as in `rent = "Expenses:Housing:Rent"`. The journal posts a payment that has the tag, or that pays a contact with the tag, to that account instead of `Expenses:Unknown` or `Income:Unknown`. |

`hw` talks to the server in JSON. It needs a haskoin-store of version 2.0.0,
and supports no older server.

When the server answers with an error, `hw` shows the error and names the
server. `hw` shows an error that it does not know with its name and its
message. A newer haskoin-store may answer with such an error. So does the
public server, for a network whose server is down:

```console
The store at https://api.haskoin.com/ answered with an error: StringError "Bad
Gateway"
```

`sync` and `recover` first ask the server whether it is healthy. They skip a
network whose server says it is not. `hw` then says what the server reports
as wrong, so that you can tell a server that is catching up from one that
lost its nodes:

```console
The store at http://localhost:3000/ says it is not healthy: its index is 6
blocks behind the chain it knows, and it has no peer
```

A server also says it is not healthy when its last block or its last
transaction is older than the age it allows. A test network can stay quiet
for hours. The message then gives both ages, as in `its last block is 5 hours
old, and it allows 4 hours`. To allow more, the operator of the server raises
its `--block-timeout` and `--tx-timeout`.

A healthy server may still not have the chain that the wallet synced with:
after you change `host`, for example, or when a server starts again with an
empty index. [`sync`](#hw-sync) refuses such a server, and says how to follow
it when its chain is the right one.

A haskoin-store keeps its unconfirmed transactions in a Redis server. When
that server is down, haskoin-store answers no other request, and the message
says `its Redis server does not answer`. When its database stops taking
writes, the server imports no new block, and the message says `its database
takes no writes, so it imports nothing`.

Some answers are not an error of haskoin-store at all, such as the error page
of a proxy. `hw` shows such an answer with its HTTP status only:

```console
The store at http://localhost:3000/ answered with an error hw could not read:
HTTP 502
```

A proxy answers `502` when the server behind it is down, and `429` when it
limits the requests. `hw` does not show the page that came with the status,
which is written for a browser.

[`setdefault`](#hw-setdefault) writes seven of these keys, so that you do not
have to edit the file:

- `default-network` and `default-type` say which account of the portfolio a
  command uses when no option says it.
- `unit`, `listing-view` and `listing-reverse` say how the answers look.
- `shown-networks` says which networks the answers show.
- `journal` says whether `hw` keeps a journal for ledger.

`setdefault` is the only command that changes the file.
[`showdefault`](#hw-showdefault) shows the seven keys. For a key that the
file does not set, it shows the value that `hw` uses. You set the other keys
and the table `journal-accounts` by hand, as
[The configuration file](#the-configuration-file) explains.

The file does not say which portfolio a command works in. That is the
[active portfolio](#the-active-portfolio): the database keeps it, and
[`switch`](#hw-switch) changes it.

The default network and the default type do not apply everywhere. `sync` and
`recover` cover every network, the hidden ones too. The listings `addresses`,
`balances`, `coins` and `txs` show the whole portfolio on the shown networks,
and `pendingtxs` shows every portfolio. A sweep that pays no portfolio has no
default network: give it `--network`.

The networks are `btc`, `bch`, `btctest`, `btctest4`, `btcreg`, `bchtest3`,
`bchtest4` and `bchreg`. `shown-networks` lists at least one of them, and
`hw` keeps them in the order above. `hw` runs no command with a file whose
`default-network` is not in `shown-networks`, or whose `shown-networks` names
no network or a network that `hw` does not know. It says which key to change:

```console
$ hw portfolios
The configuration file /home/you/.hw/config.toml could not be read, at line 4:
the key "default-network" names btctest4, which the key "shown-networks" does
not list. Add "btctest4" to "shown-networks", or set "default-network" to "btc"
or "bch"
```

With a file that has no key `shown-networks`, `hw` shows `btc` and `bch`.

### The setup

The setup is four questions that create the configuration file. Run `hw`
alone, without a command. It runs the setup when `~/.hw/config.toml` does not
exist. [Set up `hw`](#set-up-hw) shows the first question on the screen.

`hw` needs the configuration file before it runs a command. The file says
which networks `hw` shows, and which network and type of address a command
uses when no option says it. These are your choices, so `hw` asks for them.
It does not write a file of defaults without telling you.

| Question | Key it sets | What you choose |
|---|---|---|
| Which networks do you work with? | `shown-networks` | One network or more, among the eight. `btc` and `bch` start checked. |
| Which network is the default? | `default-network` | One of the networks you just chose. The default is the first of them. |
| Which type of address is the default? | `default-type` | `bip44` or `bip84`. The default is `bip44`. |
| Should `hw` keep a journal for ledger, the accounting program? | `journal` | `no` or `yes`. The default is `no`. |

**On a terminal, each question is a menu.** The `>` starts on the first row,
which is the default. These keys work in the menu:

| Key | What it does |
|---|---|
| Up and Down, or `k` and `j` | Move the `>` to another row |
| Space | Check or uncheck the network under the `>`, in the first question |
| Enter | Confirm the networks checked, or pick the row under the `>` |
| Escape, or Ctrl-D | Leave the setup. Nothing is written. |

The first question needs at least one network checked. Once you confirm,
`hw` replaces the menu with your answer, so the screen keeps the four
questions and what you chose.

**Elsewhere, each question is a line to type.** When the input or the error
output of `hw` is not a terminal, `hw` cannot read single keys. It then asks
for each answer on a line, with the default in brackets:

```console
Networks to show, separated by spaces [btc bch] 
Default network: btc or bch [btc] 
Default type of address: bip44 or bip84 [bip44] 
Keep a journal for ledger: no or yes [no] 
```

An empty line takes the default. When `hw` cannot accept an answer, it asks
the question again, three times at most. [Scripts](#scripts) shows how a
script answers.

**What the setup writes.** The setup writes `~/.hw/config.toml` with the four
keys set, and every other setting as a comment, as
[The configuration file](#the-configuration-file) explains. It creates
`~/.hw` when needed, and it writes nothing else. If you asked for a journal,
your next command writes it, whatever the command. If you leave the setup, or
if its input ends before the last answer, it writes nothing and `hw` exits
with 1.

**What the setup says at the end.** The setup prints the path of the file it
wrote and the four keys it set, on the standard output. If you asked for a
journal, it says where the journal will be, so that you can include it in a
journal of your own:

```console
journal        : true
Your next command writes the journal for ledger to /home/you/.hw/portfolios.ledger
```

It then names the commands to run next. They are
[`newportfolio`](#hw-newportfolio), which creates a wallet with a new
mnemonic, and `newportfolio --existing`, which creates the wallet of a
mnemonic you already have. The setup creates no wallet itself. A mnemonic is
a secret to write down with care, and that is a step of its own.

> **Good to know**
>
> - **The setup runs once.** Once the configuration file exists, plain `hw`
>   shows its help. Change an answer with [`setdefault`](#hw-setdefault), or
>   in the file.
> - **Only plain `hw` starts it.** `hw` refuses a command that you run before
>   the setup, and tells you to run `hw`. The questions never appear in the
>   middle of a command, where they would block a script.
> - **`hw --help` and `hw --version` need no setup.** Neither does the Tab
>   key.
> - **A file you write yourself sets `hw` up too.** `hw` only checks that
>   `~/.hw/config.toml` exists, not that the setup ran. An empty file takes
>   every default.
> - **The answers stay on this computer.** They are settings of the
>   configuration file. An offline and an online computer each run their own
>   setup.

### The configuration file

`~/.hw/config.toml` is a [TOML](https://toml.io/) file. Each setting is a line
with a key, an equals sign and a value. A text goes in quotes. A number, `true`
and `false` do not. A list goes in square brackets. What follows a `#` is a
comment, which `hw` does not read, so you can note why a setting is what it is:

```toml
# My own server, on the computer under the desk
host = "http://localhost:3000/"
parallel-requests = 4

gap = 50  # the old wallet gave out many addresses in a row
shown-networks = ["btc", "bch", "btctest4"]
```

**The file `hw` writes sets only what you chose.** The [setup](#the-setup)
writes the file with your four answers set. The file also describes every
setting in a comment: what the setting is for, the values it takes, its
default, and a line that sets it to that default:

```toml
# The unit of the amounts of a command given no --unit: of the amounts shown,
# and of those typed without a unit. A bit is 100 satoshis. hw setdefault
# --unit sets it.
# Accepted: "btc", "bit", "sat"
# Default: "btc"
# unit = "btc"
```

Each of your answers is a line below the comment of its key, where
`setdefault` would add it too. To set another key, remove the `#` before the
last line of its comment and change the value. You can also write a line of
your own anywhere above the table at the end of the file.

A setting that the file does not set takes its default. This is why the file
sets only what you chose. When a later version of `hw` changes a default, you
get the new default, unless you chose a value yourself. A file with every
default written as a value would keep the old defaults for ever.

**`setdefault` keeps your comments.** It changes the value of a key where it
is, and leaves every other character of the file as it is. For a key that the
file does not set, it adds a line below the comment that describes the key:

```console
$ hw setdefault --unit sat
unit: sat
Written to /home/you/.hw/config.toml
```

```toml
# Accepted: "btc", "bit", "sat"
# Default: "btc"
# unit = "btc"
unit = "sat"
```

When the file has no such comment, the new line goes at the end of the file.

**One setting is a table.** `journal-accounts` gives each of your tags an
account of the [journal for ledger](#hw-exportjournal). You may have many
tags, so this setting is a table: a header, and a line for each tag. The file
that `hw` writes shows the table last, with its header as a comment. To use
the table, remove the `#` before the header, and write your lines below it:

```toml
[journal-accounts]
rent = "Expenses:Housing:Rent"
salary = "Income:Salary"
```

`hw` reads a line for a tag only under the header. While the header is still
a comment, `hw` ignores the line, and each command tells you what to remove:

```console
$ hw portfolios
The configuration file /home/you/.hw/config.toml has lines under the header
[journal-accounts], which is still a comment, so hw left them aside: rent.
Remove the # before [journal-accounts] to give each tag its account.
...
```

In TOML, every line below a header belongs to its table. So keep the table at
the end of the file. `hw` runs no command when a setting is written below the
header, since it would otherwise read the setting as a tag:

```console
$ hw portfolios
The configuration file /home/you/.hw/config.toml could not be read, at line 9:
the key "journal-accounts" has a line for "unit", which is a setting of hw and
cannot have an account. If you meant the setting, move the line above the header
[journal-accounts]: every line below a header belongs to its table.
```

**The version.** The first setting of the file is `version = 1`. It says which
format the file is in, and `hw` writes it. Leave it as it is. `hw` reads a
file without it as version 1. `hw` runs no command with a file of a higher
version. Such a file is in the format of a later `hw`, and its keys may mean
something else.

**A file `hw` cannot read stops it**, before the command runs. The message
names the file, the line and the values that the key takes:

```console
$ hw portfolios
The configuration file /home/you/.hw/config.toml could not be read, at line 12:
the key "unit" must be a unit: "btc", "bit", "sat"
```

A key that `hw` does not know does not stop it. `hw` ignores the key and
warns about it on each command. Such a key is most often mistyped. Without
the warning, the setting you meant would keep its default and you would not
know:

```console
$ hw portfolios
The configuration file /home/you/.hw/config.toml has keys hw does not know, left
aside: gaps
```

> **Good to know**
>
> - **A text needs its quotes.** `unit = sat` is not TOML, and `hw` says so,
>   with the line and the column. Write `unit = "sat"`.
> - **A list may take several lines**, with a comma after each element.
>   `setdefault` does not change such a list when it has a comment inside it,
>   since the new value would replace the comment. `hw` then leaves the whole
>   file as it is, and gives you the line to write by hand.
> - **No other setting is in a table.** A line such as `[backups]` starts a
>   table in TOML, and every key below it belongs to the table. `hw` reads the
>   table `journal-accounts` and no other. It warns about another table as it
>   does about a key it does not know. `setdefault` adds a key to a file with
>   a table only when a comment above the table describes the key. Otherwise
>   it gives you the line to write by hand. The file that `hw` writes has such
>   a comment for every key, so `setdefault` still works there after you
>   write the table.
> - **A tag with the name of a setting cannot have an account.** `hw` refuses
>   a line `host = ...` in the table `journal-accounts`. Such a line is most
>   often the setting, written below the header by mistake.
> - **The pager is not a setting.** `HW_PAGER` or `PAGER` name it in the
>   environment, for a listing and for a help, as [Listings](#listings)
>   explains.
> - **Earlier versions kept the settings in `config.json`.** `hw` 0.10 does
>   not read that file, and leaves it as it is. Set what you had changed in
>   `config.toml`, by hand or with `setdefault`.

### Keep `~/.hw` on another disk

To keep the wallet on an encrypted disk or a removable drive, put the
directory `~/.hw` there, and leave a link to it in its place. `hw` works
through the link, so the database, the settings and the backups all go to that
disk.

Move the directory while no `hw` command runs:

```console
mv ~/.hw /media/you/vault/hw
ln -s /media/you/vault/hw ~/.hw
```

Before the [setup](#the-setup), create the directory yourself instead, then
run `hw`:

```console
mkdir -m 700 /media/you/vault/hw
ln -s /media/you/vault/hw ~/.hw
```

`hw` has no option for another directory, and no key of the configuration names
another database. Everything stays in one directory, so the backups are always
next to the database. A link moves it all at once.

> **Good to know**
>
> - **Make the directory yours alone to enter.** `hw` makes `~/.hw` private
>   when it creates it. It does not change the permissions of a directory
>   that exists already, and that includes a directory that a link leads to.
>   `mkdir -m 700` creates a private directory, and `chmod 700` makes private
>   a directory you made before.
> - **Link to a directory on the disk, not to the disk itself.** A disk that
>   is not mounted often leaves an empty directory where it mounts. With a
>   link to that directory, `hw` would find no configuration file, and say
>   that it is not set up. A setup run at that moment would start a new,
>   empty wallet on the disk of the computer. A directory on the disk does
>   not exist until the disk is mounted. Every command then stops, the setup
>   too:
>
>   ```console
>   $ hw portfolios
>   The directory /home/you/.hw links to /media/you/vault/hw, which does not exist. If it is on a drive, mount the drive and try again.
>   ```

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

At each Tab, the script asks `hw` what to complete. So the same script still
works after you upgrade `hw`, and completes the new commands and options. The
script finds `hw` by its name, so `hw` must be on your `PATH`.

### What it offers

Tab offers each command the values that it takes from your wallet:

- The networks `hw` shows, after `--network`. `setdefault --show-network`
  offers the hidden networks, and `setdefault --hide-network` the shown ones.
- The indexes of your wallets, after `--wallet` for `switch` and
  `exportwallet`, and after `deletewallet`.
- The indexes of your portfolios, after `switch` and `deleteportfolio`, and
  after `--portfolio` for `switch` and `exportwallet`. They are the
  portfolios of the wallet that `--wallet` names, or else of the wallet of
  the active portfolio. For `importwallet` and `show`, `--wallet` and
  `--portfolio` name a wallet and a portfolio of a file. The database does
  not know them, so Tab offers no value there.
- The hashes of your pending transactions. `signtx` offers those to sign,
  `broadcast` those signed and not sent yet, and `exporttx` and `deletetx`
  all of them.
- The transactions `bumpfee` can replace: your own payments on Bitcoin that
  are not in a block yet.
- Your coins. `lockcoin` offers the coins that you have not locked and that
  no pending transaction locks. `unlockcoin` offers the coins you locked.
  `--coin` offers the coins of the active portfolio that you can spend now.
  After a short id and a colon, such as `a1075db5:`, Tab completes the index.
- Your addresses. `label` offers the receiving addresses. `show`, `signmsg`,
  `tag` and `untag` offer all of them. `--spend-from` offers the addresses of
  the active portfolio that hold coins you can spend now. `--change-back`
  offers every address of the active portfolio, and the words `largest` and
  `smallest`.
- Your transactions. `label`, `tag`, `untag` and `show` offer them by their
  txid, and the pending ones by the hash `pendingtxs` shows. `show` offers
  your coins too. `report --payment` offers the transactions of the history.
- The contacts of your [address book](#the-address-book). `send`, `sendall`,
  `preparetx` and `preparesendall` offer them for a recipient, and
  `deletecontact`, `label`, `tag`, `untag`, `show` and `contacts --after` for
  the contact they work on. For `send` and `preparetx`, Tab writes the
  recipient with the colon that comes before its amount, and no space after
  it. In bash, you can also type the start of a label. When your word starts
  no address of the address book and starts the label of one contact only,
  Tab replaces it with the address of that contact. Capital and small letters
  count as the same: `hw send Land` and Tab write the address of the
  landlord. When the word starts the labels of several contacts, Tab
  completes nothing, so type more of the label. zsh and fish complete a
  contact from the start of its address only.
- Your [tags](#tags-on-addresses-and-transactions), wherever a command takes
  one: after `--tag`, `--address-tag` and `--spend-from-tag`, after
  `renametag` and `deletetag`, and after what `tag` and `untag` name. `tag`
  offers them for its first word too, since that word is a tag when
  `--color` colours it. A tag typed with a capital letter completes to its
  small letters. `--color` offers the six colours and `none`.

A file name completes from the working directory, or from your home directory
after `~/`. The `~/` stays as you typed it, and the shell expands it when the
command runs. `~alice/`, the home of another user, completes nothing.

`show` offers file names too. It leaves out a file whose name looks like a
hash, an address or a path, since `show` would read that name as one. Type
`./` first, and Tab offers the file.

Most values come from every portfolio, since the commands that take them find
their portfolio by themselves. Coin control is the exception. It refuses the
coins and addresses of another portfolio than the active one, so `--coin`,
`--spend-from` and `--change-back` offer those of the active portfolio only.

Tags belong to the whole database. So Tab offers every tag, whichever
portfolio is active, and whatever `--network` and `--type` say.
`--spend-from-tag` may therefore offer a tag that no address of the active
portfolio has, and the command then refuses it. The contacts belong to the
whole database too. After `--network`, Tab offers only the contacts of that
network.

The options that you type before the word narrow what Tab offers. `--network`
and `--type` narrow it to the accounts they name. The defaults of the
configuration do not apply here. Without an option, Tab offers the values of
every shown network, since you may still type the option after the word. Tab
offers nothing from a [hidden network](#shown-and-hidden-networks).

Tab offers hashes and coins by their [short ids](#short-ids), such as
`a1075db5` and `a1075db5:1`, which every command takes. Eight digits are
easier to tell apart in a list than 64. Type a ninth digit, and Tab completes
the whole hash instead. Tab offers two hashes that share a short id in full,
since the command would refuse their short id. It does the same for a short
id that also names something Tab does not offer, such as a coin of another
portfolio after `--coin`.

`hw` offers at most 20 values for a word. When more than 20 start with what
you typed, it offers none: type more of the value, or copy it from a listing.
`hw` reads only the values that match, so a large wallet completes as fast as
a small one. The labels of the address book are the exception. `hw` reads
them all to complete a label, since an address book is small.

To complete what your wallet holds, `hw` reads your database. It changes
nothing in it, and it does not wait for another `hw` command to end, such as
a long `sync`.

### Colons in bash

bash splits a word at a colon. `hw` joins the word again, so a word with a
colon completes as it does in zsh and fish. Type `a1075db5:` and Tab, and
bash offers the indexes of the coins of that transaction. Type
`bitcoincash:q` and Tab, and bash completes the address. bash keeps what you
typed up to the colon, and adds the rest.

When you type a Bitcoin Cash address without its prefix, such as
`bitcoincash:`, Tab offers the address without the prefix. `hw` reads the
address either way.

When a transaction paid you several coins, Tab completes its short id up to
the colon, which bash writes `\:`. Press Tab again to list the coins, or type
the index. bash removes the backslash when the command runs, as it does in a
name with a space, such as `my\ savings`.

## Upgrading from an earlier version

Use `hw` 0.10. Never use version 0.8 or earlier.

`hw` 0.10 does not read the database of an earlier version, nor its
configuration file. You create the wallet again from its mnemonic, once. Your
funds are not at risk. They are on the chain, and the mnemonic finds them
again.

What to do:

1. Before you install `hw` 0.10, keep the binary of `hw` 0.9 under another
   name, such as `hw-0.9`. Only that version
   [reads the old labels](#reading-the-old-database), and moves the funds of
   [a mnemonic split into parts](#a-mnemonic-split-into-parts).
2. Install `hw` 0.10, and
   [create the wallet again](#create-the-wallet-again) from its mnemonic.
3. In `hw` 0.10, set the labels you want to keep and the settings you had
   changed. `hw` 0.10 carries neither over.
4. Keep the old database until the new wallet shows all of your funds. `hw`
   0.10 leaves it as it is.

The sections below give the commands, the reasons and the special cases.
[CHANGELOG.md](CHANGELOG.md) lists what changed in each version, and the
commands and options that were renamed or removed.

### Create the wallet again

**On each computer, set `hw` up first.** `hw` 0.10 does not read the
`config.json` of 0.9, so it has no configuration file yet. Run `hw` alone,
and answer its [four questions](#set-up-hw):

```console
hw
```

**On an online computer.** Run [`recover`](#hw-recover), which asks for the
mnemonic:

```console
hw recover
```

`recover` finds the portfolios of the mnemonic that were paid, on every
network. It creates each of them with all of its accounts, then syncs them.
Run it once for each mnemonic the old database held. Each mnemonic becomes a
wallet of its own.

**With an offline computer.** `recover` needs the server, so you would type
the mnemonic on the online computer. To keep the mnemonic offline, create the
portfolios on the offline computer, and carry them over in a file:

1. On the offline computer, create the portfolios from the mnemonic, and write
   them to a file:

   ```console
   hw newportfolio --existing                  # portfolio0
   hw newportfolio --existing                  # portfolio1, for a second account
   hw exportwallet wallet.jsonl
   ```

2. On the online computer, read the file, then sync:

   ```console
   hw importwallet wallet.jsonl
   hw sync
   ```

Each `newportfolio --existing` adds the portfolio at the lowest free index,
`portfolio0` first. Run it once for each account index from 0 up to the
highest one that the old wallet used, even when the old wallet skipped an
index. The index is the third step of the path of an account. An account at
`/44'/0'/2'` is at index 2, so it needs three runs, for `portfolio0`,
`portfolio1` and `portfolio2`. The first `sync` then finds the addresses that
each portfolio used.

> **Good to know**
>
> - **The old database is left as it is**, in `~/.hw/accounts.sqlite`. `hw`
>   0.9 can still read it. Keep it until the new wallet shows all of your
>   funds.
> - **The labels of the old database are not carried over.** Nor are the names
>   of its accounts and its pending transactions. `hw` 0.9 still shows them, as
>   [Reading the old database](#reading-the-old-database) explains.
> - **A backup file of `hw` 0.9 is not read either.** The `backup` command of
>   0.9 wrote a file that only its `restore` reads.
> - **A passphrase with accents needs care on an offline computer.** See
>   [A passphrase with accents](#a-passphrase-with-accents).
> - **A mnemonic split into parts cannot be typed into `hw` 0.10.** See
>   [A mnemonic split into parts](#a-mnemonic-split-into-parts).
> - **The configuration file of 0.9 is not read.** `hw` 0.10 keeps its
>   settings in a new file, [`~/.hw/config.toml`](#the-configuration-file),
>   and leaves `config.json` as it is. Set what you had changed in the new
>   file. The keys `recovery-gap` and `database-file` are gone, as are the
>   batch sizes.
> - **The test networks are hidden.** `hw` 0.10 shows `btc` and `bch`. An
>   account of 0.9 on a test network comes back in its portfolio, and `sync`
>   updates it, but no answer shows it until you
>   [show its network](#shown-and-hidden-networks):
>   `hw setdefault --show-network btctest`.
> - **A database of a build of 0.10 made before its release is in
>   `accounts.sqlite` too.** If it is at format version 5, give it the new
>   name, and `hw` 0.10 reads it:
>   `mv ~/.hw/accounts.sqlite ~/.hw/portfolios.sqlite`. `hw` 0.10 refuses one
>   at version 3 or 4, as it refuses a database of 0.9.
> - **A configuration file of a build of 0.10 made before its release is
>   `config.json` too**, and is not read either. Set what you had changed in
>   [`config.toml`](#the-configuration-file). The keys `default-wallet` and
>   `default-portfolio` are gone: the
>   [active portfolio](#the-active-portfolio) takes their place, and
>   [`switch`](#hw-switch) chooses it. The key `autobackup-copies` is gone
>   too: `hw` now keeps the [automatic backups](#automatic-backups) by their
>   age.

### What `hw` 0.10 does with the old database

`hw` 0.10 never opens the old database, and starts with no wallet. `hw` 0.9
and earlier kept their database in `~/.hw/accounts.sqlite`. `hw` 0.10 keeps
its own in a new file, `~/.hw/portfolios.sqlite`, named after what it holds.
The old file stays where it is, as it is, and `hw` 0.9 can still read it.

So `hw` 0.10 shows no portfolio at first, with the old database next to its
own:

```console
$ hw portfolios
There are no portfolios to display
```

Do not give the old file the new name. `hw` 0.10 would then open it, and
every command refuses a database of `hw` 0.8 or 0.9. The command writes
nothing to the file, and makes no copy of it. Its message names the file, and
the commands to run next:

```console
$ hw portfolios
The database /home/you/.hw/portfolios.sqlite was made by an earlier version of
hw, at format version 2. hw 0.10.0 does not read it, and has left it as it is.
To go on, move it aside and create the wallet again from its mnemonic:

  mv /home/you/.hw/portfolios.sqlite /home/you/.hw/portfolios-old.sqlite
  hw recover

recover finds the portfolios of the mnemonic that were paid. On an offline
computer, run hw newportfolio --existing instead, once for each account index
the old wallet used, from 0 up to the highest. Then carry the portfolios to the
online computer with exportwallet and importwallet. The labels of the old
database are not carried over.
```

A database of `hw` 0.8 is at format version 1, and one of 0.9 at version 2.
`hw` 0.10 reads version 5, which `hw --version` shows. A build of 0.10 made
before its release wrote a database at version 3 or 4. `hw` 0.10 refuses such
a database the same way.

A build of 0.10 made before its release can also leave a database at version
5 that lacks a table or a column of this `hw`. The command that needs it then
stops with the words of SQLite, and with the commands that
[create the wallet again](#a-damaged-database) from the newest copy of its
labels or from its mnemonic.

### Why the database is not upgraded

An upgrade of the database would need the mnemonic too. `hw` 0.9 kept single
accounts, which you created one by one. `hw` 0.10 keeps
[portfolios](#where-a-command-works) of twelve accounts each. The paths of
the accounts that 0.9 did not have go through hardened steps, so only the
mnemonic can derive them. `hw` 0.9 also kept no fingerprint of the master
key, and the files of 0.10 name every key by that fingerprint.

An upgrade without the mnemonic would leave half-made wallets, for you to
complete from the mnemonic later. Creating the wallet again from the mnemonic
is the same work, and leaves nothing half made.

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
    btc  total 0.01200000 btc, spendable 0.01200000 btc, 2 coins, 14 addresses
    bch  total 0.00000000 btc, spendable 0.00000000 btc, no coins, 3 addresses

  portfolio1  wallet0
    created 11 Nov 2026
    btc  total 0.25000000 btc, spendable 0.25000000 btc, 1 coin, 2 addresses
```

The names of the accounts stay in the old database. A portfolio has a
description instead. [`label`](#hw-label) sets the description of the active
portfolio when you give it the description alone:

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

The old file, `~/.hw/accounts.sqlite`, keeps the labels, the names of the
accounts and the pending transactions of `hw` 0.9. `hw` 0.10 left it as it is,
so `hw` 0.9 still reads it. Run `hw` 0.9 with a `HOME` of its own, on a copy of
the file:

```console
mkdir -p ~/hw-0.9/.hw
cp ~/.hw/accounts.sqlite ~/hw-0.9/.hw/accounts.sqlite
HOME=~/hw-0.9 hw-0.9 accounts               # hw-0.9 is the binary of hw 0.9
HOME=~/hw-0.9 hw-0.9 addrs --account main   # the labels of the addresses of main
```

Set the labels you want to keep again in `hw` 0.10, with
[`label`](#hw-label). It takes the address, or its path, such as
`bip44/0/3`.

A pending transaction cannot be carried over. Prepare it again with `hw`
0.10. If `hw` 0.9 signed it already, `hw` 0.9 can still send it with
`sendtx`.

Run `hw` 0.9 on that copy only. It does not know the database of `hw` 0.10,
so never run it on that database.

The commands of `hw` 0.9 that ask the server, such as `syncacc` and `sendtx`,
need a haskoin-store older than version 1.10. `hw` 0.9 reads the binary
answers of the server, and a haskoin-store of version 1.10 or later answers
in JSON only. Reading the old file, as above, needs no server.

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

`hw` 0.10 cannot create the wallet of a mnemonic that you only have in
parts. Use `hw` 0.9 to move the funds of such a mnemonic to an address of
`hw` 0.10.

Versions up to 0.9 could split a mnemonic into parts with `mnemonic --split`.
All the parts were needed to rebuild it. `hw` 0.10 neither makes nor reads
such parts.

To move the funds, run `hw` 0.9 on a copy of the
[old database](#reading-the-old-database), which holds the account. Or run it
with an empty `HOME` of its own, such as `HOME=$(mktemp -d)`, where
`createacc --split N` asks for each of the N parts. Either way, sync the
account with `syncacc`. Then pay its funds to an address of `hw` 0.10 with
`preparetx`, `signtx --split N` and `sendtx`.

[Backups in shares](#hw-newportfolio) take the place of parts. A chosen
number of shares is enough to rebuild the mnemonic, and `hw` tells you when a
share is wrong.

### A passphrase with accents

Since `hw` 0.10, a passphrase with accents or other non-ASCII letters is
normalized as BIP39 asks. Earlier versions did not normalize it. So the
accounts they made come from another seed than the one other wallets, and
`hw` 0.10, derive from the same passphrase.

[`recover`](#hw-recover) still finds those accounts. When the normalized
passphrase finds nothing, `recover` tries the passphrase as you typed it, and
says so. `signtx` and `signmsg` then sign for those accounts, with a warning.
Move their funds to a wallet created with 0.10.

`newportfolio --existing` asks no server, so it cannot tell which of the two
seeds holds funds. In a new database, it creates the wallet of the normalized
passphrase. It warns that this wallet holds none of the funds of an earlier
version. If you sign on an offline computer, first move those funds with `hw`
0.9, from a copy of the [old database](#reading-the-old-database), to an
address of a wallet created with 0.10. Or run `recover` on the online
computer, where you then type the mnemonic.

### A wallet readable by others

`hw` 0.9 and earlier created `~/.hw` and the database with the permissions of
your umask, which often let the other users of the computer read them. `hw`
0.10 makes its new database private. It does not change a directory `~/.hw`
made by an earlier version, nor the old database in it. Make them private:

```console
chmod 700 ~/.hw
chmod 600 ~/.hw/accounts.sqlite
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
[ledger]: https://ledger-cli.org/
[podman]: https://podman.io
[Stack]: https://docs.haskellstack.org
