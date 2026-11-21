# The regtest chains of haskoin-wallet

This folder builds one container that runs two regtest chains and everything
`hw` needs to work on them. The tests of `Haskoin.Wallet.Regtest` run the `hw`
of the build against it. Nothing in it touches a real network.

| Part | What it is | Where it listens in the container |
|---|---|---|
| Bitcoin Core 32.1 | the node of `btcreg` | RPC 18443, peers 18444 |
| Bitcoin Cash Node 29.2.0 | the node of `bchreg` | RPC 18453, peers 18454 |
| haskoin-store 2.0.0 | one for each chain, fed by its node | 3001 and 3002 |
| Valkey | the Redis of both stores, which keep their mempools in it | 6379 |
| nginx | serves both stores under one host, as `hw` asks | 3000 |
| `hw` | the wallet, in the second image only | |

## Why real chains

The other tests of `hw` talk to a store that the tests fake. A fake answers
what a test tells it to. This lets a test cover a store that lies, or a
failure at one exact step of a sync. It is also the limit of a fake: it
answers as its author thought a store answers.

Here is a bug that the fakes missed. haskoin-store 1.11 began to answer with
an error for a transaction that no node accepts. The fakes went on accepting
every transaction. So a `sync` that failed for good on such a transaction
passed every test. Only a real store showed the failure.

So the two kinds of test stay, each for what it does best:

- **The fake stores** test every rule of the wallet, fast, and every answer a
  store could give, the wrong ones too. Most tests are of this kind, and
  `stack test` needs nothing else to run them.
- **The regtest chains** test that the wallet and the real programs agree. A
  real node checks every signature `hw` makes, and makes every
  reorganization. A real store answers about every transaction.

The regtest tests do not replace the fakes. A real store cannot be told to
report a thousand used addresses in a row, or to fail on the third request.

## Build the image

```console
regtest/build.sh services
```

Build with the podman of the system package, as `sudo apt install podman`
installs it. Its containers then run under the AppArmor profile of podman.

`build.sh` first runs `verify.sh`, which downloads Ubuntu Base and the two
nodes and checks their signatures. The header of `verify.sh` says which keys
it trusts and why. The build pulls no image from a registry: the first stage
starts from nothing and unpacks Ubuntu Base 26.04.1. It compiles
haskoin-store from the commit that the `Containerfile` names.

| Command | Image | What it holds |
|---|---|---|
| `regtest/build.sh services` | `localhost/hw-regtest-services` | the nodes, the stores and the proxy |
| `regtest/build.sh` | both, with `localhost/hw-regtest` | the same, and the `hw` of the working tree |

The tests need the first image only. The second image is for trying `hw` by
hand in a container. It needs no other image.

The first build compiles haskoin-store, which takes most of an hour. Later
builds run only the steps that changed.

## Run the tests

```console
haskell-dev test Regtest
```

`haskell-dev` is the development loop of
[jprupp/tools](https://github.com/jprupp/tools), on your `PATH`. Without it,
`stack test` runs these tests too, with all the other tests.

Each spec starts one container, and every test starts from two empty chains.
A test downloads nothing and builds nothing.

The containers are run by [podman-test](https://github.com/jprupp/podman-test),
a library that the haskoin packages share. It starts a container, waits until
its chains are ready, and removes it when the tests of the spec end. The
image and its recipe stay in this folder.

The tests are skipped, each marked pending with the reason, when:

- podman is missing, or cannot run a container here;
- the image is not built;
- the image was built from another recipe than the one in this folder;
- the container does not start, or its chains are not ready within a minute;
- `HW_NO_REGTEST` or `PODMAN_TEST_SKIP` is set.

The recipe of the image is the `Containerfile` and every file under
`rootfs/`. The image carries a hash of its recipe in a label. The tests
compute the same hash from the files of the working tree. After a change to
one of those files, build the image again. Otherwise the tests are skipped,
and say so.

| Variable | What it sets |
|---|---|
| `HW_NO_REGTEST` | When it is set to any value but `0`, the regtest tests are skipped |
| `PODMAN_TEST_SKIP` | When it is set to any value, every test that uses podman-test is skipped, in this package and in the others |
| `HW_REGTEST_IMAGE` | The image to use instead of `localhost/hw-regtest-services:latest` |
| `PODMAN_TEST_PODMAN` | The podman that the tests run, instead of the first one on the `PATH` |

A test run that is interrupted may leave its containers behind. podman-test
runs each container in a pod of its own, which carries the label
`podman-test.run`, and the next run removes the pods whose test process is
gone. To list them, and to remove them by hand:

```console
podman pod ps --filter label=podman-test.run
podman pod rm --force $(podman pod ps --quiet --filter label=podman-test.run)
```

The second command also removes the pods of a test run that is still going,
in this package or in another one that uses podman-test.

## What the tests cover

Every case runs on both chains, unless it is about something only Bitcoin
has: native SegWit, Taproot addresses and `bumpfee`.

| Spec | What it covers |
|---|---|
| `ReceiveSpec` | The addresses `hw` gives out, against those Bitcoin Core derives. A payment before and after it is in a block, a payment whose sender raises the fee after the user locked its coin, payments to both types of address, and the same key paid on both networks. |
| `PaymentSpec` | Payments that a node checks: legacy, native SegWit and both together, each made in three commands and with `send` alone. A payment of everything, made in three commands and with `sendall` alone. A transfer between portfolios, coins of a coinbase, `bumpfee`. |
| `RelaySpec` | The limits of what nodes relay: 1 satoshi per byte, the dust limit of each kind of address, a payment from unconfirmed change, a lock time on a fresh chain. The fee and the size are read back from the node. |
| `OfflineSpec` | A payment prepared online, signed on a computer that never saw the chain, and sent from the file. |
| `SweepSpec` | The coins of a private key outside the wallet: compressed and uncompressed, legacy, native and nested SegWit, to the wallet or from file to file. |
| `PendingSpec` | Pending transactions whose coin was spent elsewhere, or was removed by a reorganization. Payments that the network forgot, or no longer accepts, and what `sync` and `broadcast` say then. |
| `ReorgSpec` | Payments received and made, when their blocks are replaced by blocks that hold them and by blocks that do not. Rival payments, the first chain coming back, a `bumpfee` undone, a full sync, a chain that starts again. |
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

To use `hw` on the host with this container, set
`host = "http://127.0.0.1:3000/"` in its `config.toml`. Then show the two
networks with `hw setdefault --show-network btcreg --show-network bchreg`.
Give that `hw` a home of its own, with `HOME`, so that it never opens your
wallet. The RPC user and password of both nodes are `test`.

Run each command below after `podman exec hw-regtest`.

| Command | What it does |
|---|---|
| `btc-cli ...`, `bch-cli ...` | `bitcoin-cli` of each node |
| `regtest-ctl status` | a line for each service and each chain |
| `regtest-ctl mine btc 101` | mines blocks, then waits for the store |
| `regtest-ctl wait-synced [btc\|bch]` | waits until the store has the best block and the mempool of its node |
| `regtest-ctl stop\|start\|kill\|restart SERVICE` | takes a service down, or brings it back |
| `regtest-ctl restart-node btc\|bch` | starts a node again, with the options of the environment, and waits for its store |
| `regtest-ctl forget-mempool btc\|bch` | makes a node forget its mempool |
| `regtest-ctl refill-mempool btc\|bch` | empties the mempool of a store, which then asks its node for it again |
| `regtest-ctl reset` | empties both chains and starts again from the first block |
| `regtest-ctl logs SERVICE` | the end of its log |

The services are `valkey`, `node-btc`, `node-bch`, `store-btc`, `store-bch`
and `proxy`.

The image with `hw` inside also has `hw-home`, which makes a home for one
wallet user, set up for these chains:

```console
podman exec hw-regtest hw-home /work/alice
podman exec -i --env HOME=/work/alice hw-regtest hw newportfolio
podman exec --env HOME=/work/alice hw-regtest hw receive
```

## Rules of a regtest chain

These rules took time to find. A test that breaks one of them waits for a
timeout, or sees a wallet that looks wrong but is right. The helpers of
`RegtestUtils.hs` follow the rules, so a test that uses the helpers follows
them too.

- **Wait for the store after every change of the chain.** The store learns
  of a block or a transaction a moment after its node. `regtest-ctl
  wait-synced` waits until the store has the best block and the mempool of
  its node, and says it is healthy. `regtest-ctl mine` does so by itself.
- **A chain that replaces another must be longer.** A node keeps the chain
  it saw first until another one has more work, and so does the store. After
  `invalidateblock` takes N blocks off, mine at least N+1. With N blocks,
  the store stays on the old chain for good.
- **The store does not follow its node backwards.** After
  `invalidateblock` alone, the node is on a shorter chain and the store
  still has the old one. Nothing is wrong. Mine the longer chain.
- **The store is unhealthy for up to a second after a deep change.** It
  checks its health once a second. After a reorganization of more than
  three blocks, the last check may still say the index is behind, and `hw`
  then refuses to sync. `wait-synced` waits for this too.
- **The wallet of a node puts its own transactions back.** At start, a
  node adds to its mempool every unconfirmed transaction of its wallet,
  including those it received. To make a node forget a transaction, pay an
  address that its wallet does not hold.
- **The store keeps a transaction that its node dropped.** It stays in the
  mempool of the store, for 14 days by default. `hw` then shows it
  unconfirmed. `regtest-ctl refill-mempool` makes the store forget it: the
  store keeps its mempool in Valkey alone, and when its keys are gone it
  asks its node for its mempool again, within two seconds. Starting the
  store again does not empty its mempool.
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
- **The store answers at once to a transaction it is handed.** It answers
  202 as soon as its node took the bytes, before the node says whether it
  keeps the transaction. So the answer does not tell that the transaction is
  in the mempool: `wait-synced` does.

## What haskoin-store does that a test must know

These were seen with the tests on haskoin-store 2.0.0. The helpers take them
into account. A change of them belongs in haskoin-store.

- **It tells why a node refuses a transaction, because it has the RPC of
  its node here.** `regtest-ctl` starts each store with `--node-rpc`. The
  store then hands a transaction to its node over RPC, and answers 400 with
  the words of the node: `min relay fee not met, 2260 < 4520` from Bitcoin
  Core, `min relay fee not met (code 66)` from Bitcoin Cash Node.
- **Without the RPC, on Bitcoin, it answers 202 to a transaction that the
  node refuses.** Bitcoin Core says nothing to a peer about a transaction it
  refuses. `hw broadcast` then waits its ten seconds and fails, and a `hw
  sync` that sends the transaction again waits as long, then tells it as
  not sent again. Bitcoin Cash Node tells a peer why, so the store still
  answers 400. Set `STORE_NODE_RPC` to `off` to start a store without the
  RPC, as `startStoreWithoutNodeRpc` of the tests does.
- **It must be told what its node grants it.** Each node whitelists its
  store with `-whitelist=127.0.0.1`, which grants `noban`, `mempool` and
  `relay`. `regtest-ctl` names the three with `--peer-permission`. The
  changelog of haskoin-store says that without `mempool`, a store of Bitcoin
  never asks its node for its mempool.
- **It keeps a transaction that a reorganization put back in the mempool.**
  When the blocks that replace others leave a transaction out, the node
  keeps it in its mempool, and the store shows it unconfirmed. `wait-synced`
  of the tests still checks that the store has every transaction of its
  node, and calls `refill-mempool` once when one is missing.
- **It never compares its mempool with the one of its node.** A transaction
  its node dropped stays in the store, as the rules above say.
- **It has no option that empties or fills its mempool at start.** A store
  given an option that it does not know does not start. The tests then fail
  with "The store of btc is not in step with its node after a minute".

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
  it under `/var/tmp/buildah-cache-UID`, about 4 GB. After a build fails,
  the next build continues from what was compiled. Remove the folder to
  compile everything again.
