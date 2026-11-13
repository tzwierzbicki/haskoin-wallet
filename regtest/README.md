# The regtest chains of haskoin-wallet

This folder builds one container that runs two regtest chains and everything
`hw` needs to work on them. The tests of `Haskoin.Wallet.Regtest` run the `hw`
of the build against it. Nothing in it touches a real network.

| Part | What it is | Where it listens in the container |
|---|---|---|
| Bitcoin Core 32.1 | the node of `btcreg` | RPC 18443, peers 18444 |
| Bitcoin Cash Node 29.2.0 | the node of `bchreg` | RPC 18453, peers 18454 |
| haskoin-store 1.12.1 | one for each chain, fed by its node | 3001 and 3002 |
| nginx | serves both stores under one host, as `hw` asks | 3000 |
| `hw` | the wallet, in the second image only | |

## Why real chains

The other tests of `hw` talk to a store that the tests fake. A fake answers
what a test tells it to. That is how a store that lies is tested, and a
failure at one exact step of a sync. It is also the limit of a fake: it
answers as its author thought a store answers.

haskoin-store 1.11 began to answer with an error for a transaction no node
takes. The fakes went on taking every transaction. A `sync` that failed for
good on such a transaction passed every test. Only a real store showed it.

So the two kinds of test stay, each for what it does best:

- **The fake stores** test every rule of the wallet, fast, and every answer a
  store could give, the wrong ones too. Most tests are of this kind, and
  `stack test` needs nothing else to run them.
- **The regtest chains** test that the wallet and the real programs agree. A
  node checks every signature `hw` makes. A reorganization is one a node
  made. What the store says of a transaction is what a store says.

The regtest tests do not replace the fakes. A real store cannot be told to
report a thousand used addresses in a row, or to fail on the third request.

## Build the image

```console
regtest/build.sh services
```

Build with an installed podman, as `sudo apt install podman` gives it. Its
containers then run under the AppArmor profile of podman.

`build.sh` first runs `verify.sh`, which downloads Ubuntu Base and the two
nodes and checks their signatures. The header of `verify.sh` says which keys
it trusts and why. No image of a registry is used: the first stage starts from
nothing and unpacks Ubuntu Base 26.04.1. haskoin-store is compiled from the
commit the `Containerfile` names.

| Command | Image | What it holds |
|---|---|---|
| `regtest/build.sh services` | `localhost/hw-regtest-services` | the nodes, the stores and the proxy |
| `regtest/build.sh` | both, with `localhost/hw-regtest` | the same, and the `hw` of the working tree |

The tests need the first image alone. The second stands alone, to try `hw`
by hand in a container.

The first build compiles haskoin-store, which takes most of an hour. Later
builds run only the steps that changed.

## Run the tests

```console
scripts/dev test Regtest
```

`stack test` runs them too, with all the others. Each spec starts one
container, and every test starts from two empty chains. Nothing is downloaded
or built by a test.

The tests are skipped, each marked pending with the reason, when:

- podman is missing, or cannot run a container here;
- the image is not built;
- the image was built from another recipe than the one in this folder;
- `HW_NO_REGTEST` is set.

The image carries a hash of its recipe in a label: the `Containerfile` and
every file under `rootfs/`. The tests make the same hash from the files of the
working tree. After a change to one of those files, build the image again, or
the tests are skipped and say so.

| Variable | What it sets |
|---|---|
| `HW_NO_REGTEST` | Set to anything, it skips the regtest tests |
| `HW_REGTEST_IMAGE` | Another image than `localhost/hw-regtest-services:latest` |

A test that is interrupted may leave its container behind. Each one carries
the label `hw-regtest-test`, and the next run removes those whose test process
is gone. To remove them by hand:

```console
podman rm --force $(podman ps --quiet --filter label=hw-regtest-test)
```

## What the tests cover

Every case runs on both chains, unless it is about something only Bitcoin
has: native SegWit, Taproot addresses and `bumpfee`.

| Spec | What it covers |
|---|---|
| `ReceiveSpec` | The addresses `hw` gives out, against those Bitcoin Core derives. A payment before and after its block, at both types of address, and on both networks at one key. |
| `PaymentSpec` | Payments a node checks: legacy, native SegWit and both together, made in three commands and with `send` alone, a payment of everything, a transfer between portfolios, coins of a coinbase, `bumpfee`. |
| `RelaySpec` | The limits of what nodes relay: 1 satoshi per byte, the dust limit of each kind of address, a payment from unconfirmed change, a lock time on a fresh chain. The fee and the size are read back from the node. |
| `OfflineSpec` | A payment prepared online, signed on a computer that never saw the chain, and sent from the file. |
| `SweepSpec` | The coins of a private key outside the wallet: compressed and uncompressed, legacy, native and nested SegWit, to the wallet or from file to file. |
| `PendingSpec` | Pending transactions whose coin was spent elsewhere, or went with a reorganization. Payments the network forgot, or takes no more, and what `sync` and `broadcast` say then. |
| `ReorgSpec` | Payments received and made, through blocks replaced with and without them, rival payments, the first chain coming back, a `bumpfee` undone, a full sync, a chain that starts again. |
| `RecoverSpec` | `recover` by the gap of addresses and of portfolios, with and without the passphrase, after a reorganization, and with funds at paths `hw` cannot spend. |
| `ManySpec` | An address paid 120 times, which is more than the store gives in one page, and one payment that spends its 120 coins. |
| `StoreFailureSpec` | A store that is down, a proxy that is down, a node that is down, a store that calls itself unhealthy, during `sync` and `recover`. |

The helpers are in `test/Haskoin/Wallet/RegtestUtils.hs`: the histories the
tests start from, the commands of a wallet, and the commands of the nodes.

What the regtest tests leave to the fake stores:

- The rules that need no real store: coin selection, coin control, labels,
  the views, the files, the hidden networks, shell completion.
- A store that lies or fails at one step. A real one does neither on demand.
- The main networks and the public test networks. Their keys, addresses and
  rules are tested from fixed vectors.

## Run the container by hand

```console
podman run --detach --name hw-regtest --tmpfs /data:rw,size=2g,mode=1777 \
  --publish 127.0.0.1:3000:3000 --publish 127.0.0.1:18443:18443 --publish 127.0.0.1:18453:18453 \
  localhost/hw-regtest-services
podman exec hw-regtest regtest-ctl status
```

The chains live in `/data`. On a tmpfs they are gone with the container, so
every run starts from the first block.

`hw` on the host then takes `"host": "http://127.0.0.1:3000/"` in its
`config.json`, and shows the two networks after
`hw setdefault --show-network btcreg --show-network bchreg`. Give that `hw` a
home of its own, with `HOME`, so that it never opens your wallet. The RPC user
and password of both nodes are `test`.

Every command below runs with `podman exec hw-regtest`.

| Command | What it does |
|---|---|
| `btc-cli ...`, `bch-cli ...` | `bitcoin-cli` of each node |
| `regtest-ctl status` | a line for each service and each chain |
| `regtest-ctl mine btc 101` | mines blocks, then waits for the store |
| `regtest-ctl wait-synced [btc\|bch]` | waits until the store has the best block and the mempool of its node |
| `regtest-ctl stop\|start\|kill\|restart SERVICE` | takes a service down, or brings it back |
| `regtest-ctl restart-node btc\|bch` | starts a node again, with the options of the environment, and waits for its store |
| `regtest-ctl forget-mempool btc\|bch` | makes a node forget its mempool |
| `regtest-ctl reset` | empties both chains and starts again from the first block |
| `regtest-ctl logs SERVICE` | the end of its log |

The services are `node-btc`, `node-bch`, `store-btc`, `store-bch` and
`proxy`.

The image with `hw` inside also has `hw-home`, which makes a home for one
wallet user, set up for these chains:

```console
podman exec hw-regtest hw-home /work/alice
podman exec -i --env HOME=/work/alice hw-regtest hw newportfolio
podman exec --env HOME=/work/alice hw-regtest hw receive
```

## Rules of a regtest chain

These cost time to find. A test that breaks one of them waits for a timeout,
or sees a wallet that looks wrong and is right. The helpers of
`RegtestUtils.hs` keep them, so a test that uses the helpers need not.

- **Wait for the store after every change of the chain.** The store learns
  of a block or a transaction a moment after its node. `regtest-ctl
  wait-synced` waits until the store has the best block and the mempool of
  its node, and says it is healthy. `regtest-ctl mine` does so by itself.
- **A chain that replaces another must be longer.** A node keeps the chain
  it saw first until another one has more work, and so does the store. After
  `invalidateblock` takes N blocks off, mine N+1 at least. With N, the
  store stays on the old chain for good.
- **The store does not follow its node backwards.** After
  `invalidateblock` alone, the node is on a shorter chain and the store
  still has the old one. Nothing is wrong. Mine the longer chain.
- **The store is unhealthy for up to a second after a deep change.** It
  checks its health once a second. After a reorganization of more than
  three blocks, the last check may still say the index is behind, and `hw`
  then refuses to sync. `wait-synced` waits for this too.
- **The wallet of a node puts its own transactions back.** At start, a
  node adds to its mempool every unconfirmed transaction of its wallet,
  those it received too. To make a node forget a transaction, pay an
  address that is not of its wallet.
- **The store keeps a transaction that its node dropped.** It stays in the
  mempool of the store, for 14 days by default. `hw` then shows it
  unconfirmed. Start the store again to make it forget.
- **Start the nodes with a set clock.** A hundred blocks mined in one
  second are dated up to 95 seconds ahead. `-mocktime` at start, and
  `setmocktime` before each block, give every block a time that follows from
  its height. The wallet of Bitcoin Core ignores blocks dated before it was
  made, so the clock must be set from the start, not after.
- **A node that starts again needs its clock again.** It refuses to start
  when its best block is more than two hours ahead of its clock.
  `regtest-ctl restart-node` passes `BTC_NODE_OPTIONS` and
  `BCH_NODE_OPTIONS` again.
- **Open a new connection after a node starts again.** A connection kept
  open to the published port of a node that started again waits thirty
  seconds for an answer that never comes.
- **The store never calls itself unhealthy for a quiet chain here.** A
  haskoin-store fails its health check when the last block is older than
  four hours, or the last transaction older than half an hour. `hw` then
  refuses to sync. The stores of this container run with both limits at a
  hundred years. Set `STORE_BLOCK_TIMEOUT` and `STORE_TX_TIMEOUT`, in
  seconds, to test that case.
- **The store waits thirty seconds for a node to announce a transaction.**
  A test of a transaction that no node announces sets
  `STORE_PUBLISH_TIMEOUT` lower. The tests run with 3 seconds.

## What haskoin-store does that a test must know

These were found with the tests, and checked again on haskoin-store 1.12.1.
The helpers work around them. A fix belongs in haskoin-store.

- **It drops a transaction that a reorganization put back in the mempool.**
  When the blocks that replace others leave a transaction out, the node
  keeps it in its mempool. The store first shows it unconfirmed, then drops
  it within a second, and its node does not announce it again. `hw` then
  shows the payment dead until a block holds it. Posting the transaction
  again does not bring it back. The store hands it to its node, which has
  it already and does not announce it, so the store answers a timeout. The
  helpers wait a second after a reorganization, and start the store again
  with `--sync-mempool` when it lacks a transaction of its node.
- **It never compares its mempool with the one of its node.** A transaction
  its node dropped stays in the store, as told above. From 1.12.1, posting
  that transaction again hands it to a node again, and the node takes it
  back.
- **On Bitcoin, it answers 200 to a transaction it lists, even when the
  node refuses it.** Such a transaction is handed to the node again when it
  is posted, as above. Bitcoin Core says nothing of a transaction it
  refuses. The stores here have no RPC of their node, so the store takes
  that silence for a yes. Bitcoin Cash Node says why it refuses, and the
  store answers 400 with the reason.
- **On Bitcoin Cash, it keeps the first of two rival transactions.** When
  its node holds the second alone, the store still has the first, and
  answers a timeout to the second, which its node took. To the first, it
  answers from 1.12.1 with the refusal of its node, `txn-mempool-conflict`.
- **It forgets a transaction that a reorganization replaced.** It then
  returns nothing for its id. `hw` keeps such a transaction, dead.

## Good to know

- **A service that stops does not stop the container.** A test can kill a
  store and start it again.
- **`BTC_NODE_OPTIONS`, `BCH_NODE_OPTIONS`, `BTC_STORE_OPTIONS` and
  `BCH_STORE_OPTIONS`** add options to a service. Give them with `--env`.
- **The same scripts run without a container.** Every path and port of
  `regtest-ctl` can be set from the environment, so the nodes and the stores
  also run as plain processes of a computer that has the binaries. The
  header of `rootfs/usr/local/lib/regtest/common.sh` lists the variables.
- **The compiler is in a cache of the build**, not in a layer. Podman keeps
  it under `/var/tmp/buildah-cache-UID`, about 4 GB. A build that fails
  goes on from what it compiled. Remove the folder to compile from nothing.
