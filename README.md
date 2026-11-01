# Haskoin Wallet

Haskoin Wallet (`hw`) is a command-line wallet for Bitcoin and Bitcoin Cash.
It makes mnemonics as [BIP39] describes, and derives accounts and addresses as
[BIP44] does. It keeps its accounts in a SQLite database, and asks a
[haskoin-store] server about the chain. The database holds public keys only.
Your mnemonic is never written to disk.

`hw` is made for cold storage. The mnemonic can stay on an offline computer,
while an online computer watches the accounts and prepares the payments. A
payment is prepared without any key, signed on the offline computer, and sent
from the online one. The two computers exchange files: the public key of each
account, and the transactions to sign.

Besides payments, `hw` has coin control, fee rates read from recent blocks,
fee bumping on Bitcoin ([BIP125]), sweeps of coins whose private keys you hold,
and message signing in the Bitcoin Signed Message format. Every command can
answer in JSON, for scripts. `hw` suits small wallets managed by hand.

## Build and install

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

### Completion

The Tab key can complete what you type after `hw`. It completes the commands,
the options, and the values `hw` knows in advance, such as units, networks and
file names. It also completes what your wallet holds, and each command is
offered what it takes:

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

To complete what your wallet holds, `hw` reads your database. It changes
nothing in it, and it does not wait for another `hw` command, such as a long
`sync`.

bash splits a word at a colon. So a Bitcoin Cash address is offered without
its prefix, such as `bitcoincash:`, and `hw` reads it either way. A coin of a
transaction that paid you several coins completes up to its colon in bash,
which bash writes `\:`. Press Tab again to list the coins, or type the index.
The backslash goes when the command runs, as it does in a name with a space,
`my\ savings`.

### Configuration

On its first command, `hw` creates the directory `~/.hw`. It holds the
database `accounts.sqlite` and the settings `config.json`:

| Key | Default | What it sets |
|---|---|---|
| `host` | `https://api.haskoin.com/` | The haskoin-store server |
| `default-network` | `btc` | The network of a command given no `--network` |
| `gap` | `20` | How many unused addresses in a row `receive` gives out and `sync` looks at |
| `change-back` | `false` | Whether `preparetx` sends the change back to an address it spends from |
| `parallel-requests` | `1` | How many requests go to the server at once. The public server takes one. |
| `addr-batch`, `coin-batch`, `tx-full-batch` | `100` | How many addresses, coins or transactions one request asks about |
| `database-file` | `null` | Another database, instead of `~/.hw/accounts.sqlite` |
| `pager` | `null` | The pager a listing goes through on a terminal, instead of `less` or `PAGER`. `""` is none. |

The networks are `btc`, `bch`, `btctest`, `bchtest4`, `bchtest`, `btcreg` and
`bchreg`. `hw` warns about a key it does not know.

## Usage

`hw --help` lists the commands, and `hw COMMAND --help` explains one. The
commands in magenta in the list ask for a mnemonic or private keys. Run them on
an offline computer if you can. When the help is not shown on a terminal, or
with `NO_COLOR` set, they are marked `!` instead.

Three options work with every command, before or after it:

- `--unit btc|bit|sat` sets the unit of the amounts shown, and of the amounts
  typed without a unit. The default is `btc`. A bit is 100 satoshis.
- `--json` prints the answer as JSON. Questions and warnings go to stderr.
- `--no-pager` prints a listing as it is, through no pager.

On a terminal, a listing such as `hw txs` goes through `less`. The arrow keys
scroll it, `/` searches it and `q` quits it. A listing that fits on the screen
is shown as it is. `HW_PAGER`, the key `pager` of `config.json` or `PAGER`
name another pager. Scripts read a listing a page at a time with `--max`, then
`--after` the last row they got.

```console
hw txs --max 20 --json
hw txs --max 20 --after TXID --json
```

The examples below write `ADDRESS`, `NOSIGHASH`, `TXID`, `HASH` and `TXHASH`
where you put your own values.

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

On the offline computer, create the account and export its public key:

```console
hw mnemonic --dice
hw createacc vault
hw exportacc --account vault vault.json
```

On the online computer, import the account and prepare a payment to a file:

```console
hw importacc vault.json
hw sync
hw preparetx ADDRESS:0.01 --fee-rate median --output unsigned.json
```

Back on the offline computer, check the transaction and sign it:

```console
hw reviewtx unsigned.json
hw signtx --input unsigned.json --output signed.json
```

On the online computer, send it:

```console
hw sendtx --input signed.json
hw sync
```

### Mnemonics and accounts

**`hw mnemonic`** prints a new mnemonic, made from the entropy of the
operating system, and stores nothing. `--entropy 32` gives 24 words instead of
12. `--dice` mixes in rolls of a die. `--parts N` prints N parts, which are all
needed to rebuild the mnemonic.

```console
hw mnemonic --entropy 32 --dice
hw mnemonic --parts 3
```

**`hw createacc NAME`** creates an account from a mnemonic and passphrase,
each typed twice. It stores the public key of the account, at the next free
index of the seed on the network.

```console
hw createacc savings
hw createacc cash --network bch
hw createacc vault --parts 3
```

**`hw recover`** finds the accounts of a mnemonic that were paid. It adds
them, named `btc0`, `btc1` and so on, and syncs them. It stops after 10
accounts in a row without a payment, or the number `--account-gap` gives. It
needs the mnemonic and the server at the same time.

```console
hw recover --network btc --account-gap 20
```

**`hw checkmnemonic`** checks that a mnemonic and passphrase give the public
key of an account. Use it to test a mnemonic you wrote down. Nothing is signed.

```console
hw checkmnemonic --account savings
```

**`hw exportacc FILE`** writes the public key of an account to a file, and
**`hw importacc FILE`** adds it to another wallet. The file holds no private
key.

```console
hw exportacc --account vault vault.json     # on the offline computer
hw importacc vault.json                     # on the online computer
```

**`hw accounts`** shows the accounts with their balances. Each takes two
lines: its name, network and derivation path, then what it can spend, what is
unconfirmed and how many coins it holds. The spendable balance leaves out
locked coins, coinbase coins not yet mature, and unconfirmed payments from
other wallets. `--verbose` adds the fingerprint of the seed, the addresses,
when the account was created, and its confirmed and unconfirmed balances.

```console
hw accounts
hw accounts --verbose
hw accounts --account savings --unit sat
```

**`hw renameacc OLD NEW`** renames an account.

```console
hw renameacc savings rainy-day
```

**`hw deleteacc NAME NETWORK DERIVPATH`** removes an account with its
addresses, labels, transactions and coins. The network and the path, as
`accounts` shows them, confirm the choice. The funds stay on the chain.

```console
hw deleteacc savings btc "/44'/0'/0'"
```

**`hw sync`** updates the accounts from the server: balances, transactions,
coins and pending transactions. It also finds addresses that another wallet
of the same seed used. For each account, it shows its balances, the block it
is synced to, and what changed. Run it before you prepare a payment and after
you send one. `--full` downloads everything again, which is rarely needed. When the
server does not answer for a network, `sync` leaves out the accounts of that
network and says why, and still syncs the others.

```console
hw sync
hw sync --account savings --full
```

### Addresses

**`hw receive`** gives the next receiving address of an account.

```console
hw receive --label "invoice 42"
```

**`hw addresses`** lists the receiving addresses, the newest first, or the
change addresses with `--change`. Each takes two lines: its path, the address
and its label, then what it can spend and what it received, or that it is
unused. `--verbose` adds its balances, its coins and when it was created.

```console
hw addresses
hw addresses --change
hw addresses --verbose
hw addresses --max 10 --after ADDRESS
```

**`hw balances`** lists the addresses that hold coins, the largest total first.

```console
hw balances
```

**`hw label ADDRESS LABEL`** sets the label of a receiving address. The
address tells its account. Change addresses take no label. `label` labels a
transaction too: see [Transactions](#transactions). An empty label, `""`,
removes the label.

```console
hw label ADDRESS "rent from Bob"
```

### Coins

**`hw coins`** lists the coins of an account. Each takes three lines: its
value with its state, spendable or locked, and its confirmations; the address
that holds it, with its label, or `change address` when it is one; and the
coin as `TXID:INDEX`. A coin paid by a transaction you labelled shows the
label on a line of its own, below the first, as `txs` does. The change of a
payment you made shows no label: the label says what the payment was for, not
what its change is.
**`hw lockcoin`** keeps a coin from being spent, and **`hw unlockcoin`** frees
it again.

```console
hw coins
hw lockcoin TXHASH:1
hw coins --locked
hw unlockcoin TXHASH:1
```

### Payments

**`hw fees`** shows the low, median and high fee rates of the latest six
blocks, in satoshis per byte. The commands that take `--fee-rate` show the same
rates and ask when you give none.

```console
hw fees
hw fees --network bch
```

**`hw preparetx ADDRESS:AMOUNT...`** prepares a payment, unsigned, and keeps it
as a pending transaction. It needs no mnemonic. `--fee-rate` takes a number or
`low`, `median` or `high`. `--recipients-pay` takes the fee out of the amounts
paid. `--coin` and `--spend-from` choose the coins to spend, every one of them.
`--select` spends only what the payment needs among them, as `--strategy`
selects them. `--change-back` sends the change back to an address the payment
spends from. `--output` also writes the transaction to a file. `--label` gives
the transaction a label, which goes with it to its file and stays once the
network has it.

```console
hw preparetx ADDRESS:0.0005 --fee-rate median --label "rent of October"
hw preparetx ADDRESS1:10000sat ADDRESS2:250bits --fee-rate 5
hw preparetx ADDRESS:0.01 --spend-from MYADDRESS --change-back --fee-rate low
hw preparetx ADDRESS:0.01 --spend-from MYADDRESS --select --fee-rate low
hw preparetx ADDRESS:0.01 --recipients-pay --fee-rate high --output unsigned.json
```

**`hw preparesendall ADDRESS[:AMOUNT]...`** prepares a payment of every coin
the account can spend, with no change. An address with an amount gets that
amount. The addresses without one share the rest and pay the fee. hw asks
whether to spend the coins worth less than their fee, unless `--spend-dust` or
`--leave-dust` answers.

```console
hw preparesendall ADDRESS --fee-rate low
hw preparesendall ADDRESS1:0.1 ADDRESS2 --fee-rate median --leave-dust
```

**`hw signtx`** signs a pending transaction by its nosigHash, or a transaction
file. It asks for the mnemonic and passphrase of the account.

```console
hw signtx NOSIGHASH
hw signtx --input unsigned.json --output signed.json
```

**`hw sendtx`** shows a signed transaction, asks, and sends it. It then waits
up to ten seconds for the server to see it.

```console
hw sendtx TXID
hw sendtx --input signed.json
```

**`hw bumpfee TXID`** prepares a replacement of an unconfirmed payment that
pays a higher fee, on Bitcoin only. The extra fee comes out of the change. When
the change cannot pay it, `hw` offers to add confirmed coins of the account, or
to take what is missing from what the recipients receive, asking which of them
pays. Sign and send the replacement like any payment.

```console
hw bumpfee TXID --fee-rate high
hw signtx NOSIGHASH
hw sendtx TXID                              # the txid of the replacement
```

### Transactions

**`hw txs`** lists the transactions of an account, as of the last sync. Each
takes three lines. The first gives its time, in the time zone of the computer,
its type, its amount and its status. The second gives the address paid, and
the third the txid. An unconfirmed transaction shows its fee rate. A
transaction with a label shows it on a line of its own, below the first.
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

**`hw pendingtxs`** lists the pending transactions: prepared, signed, or sent
and not yet seen by `sync`. They take three lines each, as in `txs`. Each is
shown by its nosigHash until it is signed, then by its txid. A conflicted one
can no longer be sent.

```console
hw pendingtxs
hw pendingtxs --verbose
```

**`hw label TXID LABEL`** sets the label of a transaction. Name a pending one
by the hash `pendingtxs` shows, and one of the history by its txid. A pending
transaction keeps its label in its file, through signing on an offline
computer, and in the history once the network has it. A label set while the
file is away stays when the signed file comes back. `preparetx`,
`preparesendall` and `preparesweep` take a `--label`, and `bumpfee` gives a
replacement the labels of the transactions it replaces. A transaction between
two accounts of the wallet gets the label in both.

```console
hw label NOSIGHASH "rent of October"
hw label TXID "salary"
hw label TXID ""                            # removes the label
```

**`hw reviewtx FILE`** shows the transaction in a file and the account it
belongs to, without changing the wallet. The labels it shows are those the
file carries, so an offline computer sees the labels of the online wallet.

```console
hw reviewtx unsigned.json
```

**`hw exporttx HASH FILE`** writes a pending transaction to a file.
**`hw importtx FILE`** adds a transaction file to the pending transactions, or
replaces the pending one with its signed version. A transaction file carries
the label of the transaction, the labels of the account's addresses in it, and
the labels of the coins it spends, as the wallet has them when it writes the
file. `importtx` keeps the labels of the wallet.

```console
hw exporttx NOSIGHASH unsigned.json
hw importtx signed.json
```

**`hw deletetx HASH`** deletes a pending transaction and frees its coins. Name
it by its nosigHash, or by its txid once it is signed. A transaction you sent
can be deleted only once the server no longer knows it.

```console
hw deletetx HASH
```

### Sweeps

**`hw preparesweep`** prepares a transaction that moves coins from outside the
wallet: the coins at `--sweep-from` addresses or at those listed in an
`--address-file`, at the addresses of the keys in a `--key-file`, or given with
`--coin`. The keys
are needed only to sign, with **`hw signsweep`**. A sweep that pays an address
of this wallet is kept as a pending transaction.

```console
hw preparesweep --sweep-from OLDADDRESS --sweep-to MYADDRESS --fee-rate median
hw signsweep NOSIGHASH --key-file keys.txt
hw sendtx TXID
```

A sweep that pays no address of this wallet needs `--network` and `--output`,
and no account. It goes from file to file:

```console
hw preparesweep --key-file keys.txt --sweep-to ADDRESS --network btc --fee-rate median --output sweep.json
hw signsweep --input sweep.json --key-file keys.txt --output signed.json
hw sendtx --input signed.json
```

`--key-file -` types the keys on the terminal instead of reading a file.

### Messages

**`hw signmsg ADDRESS MESSAGE`** signs a message with the key of an address of
the wallet. `--output` writes the signed message to a file instead of printing
it. **`hw verifymsg`** checks a signature from `hw`, Bitcoin Core or another
wallet, from any P2PKH address.

```console
hw signmsg MYADDRESS "I control this address" --output signed.txt
hw verifymsg --input signed.txt
hw verifymsg ADDRESS SIGNATURE "I control this address"
```

### The wallet

**`hw backup`** copies the database, to a file you name or into
`~/.hw/backups`. **`hw --version`** shows the version of `hw` and of its
database format.

```console
hw backup
hw backup ~/hw-2026-10-30.sqlite
hw --version
```

To restore a backup, replace `~/.hw/accounts.sqlite` with the copy, or point
`database-file` in `~/.hw/config.json` to it.

## Gotchas

- **An amount without a unit is in bitcoins.** `ADDRESS:1000` asks for 1000
  BTC, not 1000 satoshis. Write the unit, as in `ADDRESS:1000sat`, or give
  `--unit sat`.
- **Quote derivation paths.** They hold `'`, which the shell reads as a quote.
  Write `"/44'/0'/0'"`.
- **The mnemonic is the only key to the funds.** `hw` never stores it. Without
  the database, the mnemonic still spends the funds, and `recover` finds the
  accounts again. Without the mnemonic, the funds are lost.
- **Give the same `--parts` to every command that asks for a split mnemonic.**
  `hw` then asks for each part.
- **The database and the account files are private.** They hold no private
  key. But the public keys in them reveal every address and balance of your
  accounts.
- **A pending transaction locks its coins.** They stay locked until `sync` sees
  the transaction on the network, or until `deletetx` deletes it. Delete the
  transactions you abandon.
- **`sync` sends again what you sent.** A transaction that the network refuses
  for good, for a fee too low to relay for example, keeps coming back until
  you delete it with `deletetx`.
- **Unconfirmed payments from others cannot be spent.** The sender can still
  replace them until they confirm. Only the unconfirmed change of your own
  transactions can be spent.
- **`receive` stops after 20 unused addresses in a row.** A wallet restored
  from the mnemonic would not look further. Reuse one of them, or raise `gap`
  in the configuration file.
- **Choosing a fee rate needs the server.** Without `--fee-rate`, `hw` reads the
  rates of recent blocks, and stops if it cannot reach the server. A rate above
  100 satoshis per byte asks for a confirmation, since it is more often a typo.
- **The addresses of `hw` are legacy P2PKH.** They start with `1` on Bitcoin.
  `hw` can pay any kind of address. `signmsg` and `verifymsg` work with P2PKH
  addresses only.
- **Bitcoin Cash does not replace transactions,** so `bumpfee` works on Bitcoin
  only.
- **Private keys and legacy addresses read the same on Bitcoin and Bitcoin
  Cash.** That is why a sweep that pays no account of the wallet needs
  `--network`, and the default network does not apply to it.
- **A message is signed byte for byte.** A space or a line break added at either
  end makes the signature fail. `hw` warns about them.
- **`hw` never overwrites a file.** `--output` and `backup` refuse a file that
  exists. `signtx` and `signsweep` replace their `--input` file only when you
  agree.
- **One command at a time.** A second `hw` command on the same database stops
  with "The database is in use by another hw command".
- **The public server takes one request at a time.** Keep `parallel-requests`
  at 1, unless you run your own haskoin-store.
- **A passphrase with accents or other non-ASCII letters** is normalized as
  BIP39 asks since `hw` 0.10. Earlier versions did not normalize it, so their
  accounts come from another seed than other wallets find. `hw` 0.10 still
  finds and signs those accounts, and warns. Move their funds to an account
  created with 0.10.
- **A transaction file from an earlier `hw` names no network.** `hw` 0.10 still
  reads it when the transaction involves an account of the wallet. Otherwise,
  as for a sweep to outside addresses, prepare it again.

## Deprecated versions

Use `hw` 0.10. Never use version 0.8 or earlier.

`hw` 0.10 upgrades the database of 0.8 and 0.9 on its first command. It backs
the database up to `~/.hw/migration-backups` first. Do not run an older `hw`
on the upgraded database. `hw` 0.8 would rewrite parts of it in a format that
0.10 cannot read.

Earlier versions have known bugs in the accounts they derive. Your funds are
not lost, but take care when you move them:

- Versions before 0.8.0 derive the wrong account when the wallet holds more
  than one mnemonic. The first account of a second mnemonic comes out as
  `/44'/0'/1'` instead of `/44'/0'/0'`. Write down the derivation path of each
  account you use, so that you can find it again.
- Version 0.7.0 derives the wrong account from a split mnemonic. Use the
  0.7.0 binary to move the funds to a new account created with 0.10.

[BIP39]: https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
[BIP44]: https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki
[BIP125]: https://github.com/bitcoin/bips/blob/master/bip-0125.mediawiki
[haskoin-store]: https://github.com/haskoin/haskoin-store
[Stack]: https://docs.haskellstack.org
