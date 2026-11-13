# Shared by the scripts of the regtest container. Sourced, never run.
#
# Every path and port below is the one of the container, unless the
# environment gives another. The same scripts then run the services as
# plain processes of a computer, without a container: see run_proxy in
# regtest-ctl for the one part that differs.

DATA=${REGTEST_DATA:-/data}
RUN=${REGTEST_RUN:-/run/regtest}
LOG=${REGTEST_LOG:-/var/log/regtest}

# Where the binaries are
BTC_HOME=${BTC_HOME:-/opt/bitcoin-core}
BCH_HOME=${BCH_HOME:-/opt/bitcoin-cash-node}
HASKOIN_STORE=${HASKOIN_STORE:-/usr/local/bin/haskoin-store}

RPC_USER=test
RPC_PASSWORD=test

# One chain is a node, and a store that indexes it. The proxy serves both
# stores under one host, each at the path of its network, as hw asks.
# Bitcoin Core also listens one port above its peer port, for Tor, so that
# port stays free: 18445 here.
BTC_RPC_PORT=${BTC_RPC_PORT:-18443}
BTC_P2P_PORT=${BTC_P2P_PORT:-18444}
BCH_RPC_PORT=${BCH_RPC_PORT:-18453}
BCH_P2P_PORT=${BCH_P2P_PORT:-18454}
BTC_STORE_PORT=${BTC_STORE_PORT:-3001}
BCH_STORE_PORT=${BCH_STORE_PORT:-3002}
PROXY_PORT=${PROXY_PORT:-3000}

# The address the nodes take RPC calls on. In the container it is every
# address, since podman publishes the port.
RPC_BIND=${RPC_BIND:-0.0.0.0}
RPC_ALLOW=${RPC_ALLOW:-0.0.0.0/0}

SERVICES="node-btc node-bch store-btc store-bch proxy"

# The store calls itself unhealthy when the last block is older than
# --block-timeout, or the last transaction older than --tx-timeout. A
# regtest chain is as old as its last test, and its first block is of 2011.
# So both limits are a hundred years here, unless the caller sets them.
STORE_BLOCK_TIMEOUT=${STORE_BLOCK_TIMEOUT:-3153600000}
STORE_TX_TIMEOUT=${STORE_TX_TIMEOUT:-3153600000}

# How long the store waits for a node to announce back a transaction it
# was handed, in seconds, before it answers that none did. The store waits
# 30 seconds by default. A node of Bitcoin says nothing of a transaction
# it refuses, so a test of a refusal waits that long.
STORE_PUBLISH_TIMEOUT=${STORE_PUBLISH_TIMEOUT:-30}

die() { printf '%s\n' "$*" >&2; exit 1; }

# chain_of NAME: btc or bch, from btc, btcreg, node-btc, store-bch...
chain_of() {
  case "$1" in
    *btc*) echo btc ;;
    *bch*) echo bch ;;
    *) die "No such chain: $1. The chains are btc and bch." ;;
  esac
}

# node_cli CHAIN ARGS: the bitcoin-cli of the node of a chain
node_cli() {
  local chain=$1; shift
  case "$chain" in
    btc) "$BTC_HOME/bin/bitcoin-cli" -regtest -rpcuser=$RPC_USER -rpcpassword=$RPC_PASSWORD -rpcport=$BTC_RPC_PORT "$@" ;;
    bch) "$BCH_HOME/bin/bitcoin-cli" -regtest -rpcuser=$RPC_USER -rpcpassword=$RPC_PASSWORD -rpcport=$BCH_RPC_PORT "$@" ;;
  esac
}

# store_url CHAIN: where the store of a chain answers, behind no proxy
store_url() {
  case "$1" in
    btc) echo "http://127.0.0.1:$BTC_STORE_PORT" ;;
    bch) echo "http://127.0.0.1:$BCH_STORE_PORT" ;;
  esac
}
