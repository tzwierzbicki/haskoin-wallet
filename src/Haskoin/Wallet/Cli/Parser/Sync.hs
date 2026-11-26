{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of sync, with its options and its help.
-- "Haskoin.Wallet.Commands.Sync" runs it.
module Haskoin.Wallet.Cli.Parser.Sync
  ( syncCommand,
  )
where

import Haskoin.Wallet.Cli.Command (Command (..))
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (networkOptionWith)
import Haskoin.Wallet.Sync (SyncMode (..))
import Text.RawString.QQ (r)

-- | sync, which updates the portfolios from the network
syncCommand :: CommandSpec Command
syncCommand =
  CommandSpec
    { name = "sync",
      offline = False,
      summary = "Update the portfolios from the network",
      details =
        [r|
`sync` updates the portfolios from the server. It asks for the latest
balances, transactions and coins of every portfolio of every wallet, on every
network, the test networks included. Run it before you prepare a payment and
after you send one. For each portfolio and network that holds something or
where something changed, it shows the balances as `portfolios` does, the
block it is synced to, and what changed. --network syncs one network only.

On a terminal, a status line says what `sync` is doing while it works, with a
bar for the steps whose size it knows. The line goes to stderr and is gone
when `sync` ends, so scripts never see it.

`sync` also works on the networks that hw hides, so that a network is up to
date the day you show it with setconfig --show-network. The status line
names the hidden networks as `sync` works on them. The answer leaves them
out, and a hidden network whose server is down is no error.

`sync` first looks for used addresses that each account does not have yet.
Someone who pays to addresses derived from the public key of the account, or
another wallet of the same mnemonic, may have used them. `sync` looks past
the last used address of each account, as far as the key gap of the
configuration file says, 20 addresses by default. It adds each used address
that it finds, with the addresses before it, and then looks as far past that
one. So the first sync of a portfolio finds every address used so far. It
stops at 20 unused addresses in a row, as a wallet restored from the mnemonic
does, and misses a used address that comes after them. The count starts at
the last used address, not at the last address hw `receive` gave out. So two
computers with the same mnemonic and the same gap find the same payments. To
look further, raise the gap for one sync. The same gap limits how many unused
addresses in a row hw `receive` gives out.

When the server reports a thousand used addresses in a row, and more after
them, `sync` asks whether to keep looking. A server that lies reports used
addresses without end. The addresses that `sync` would then generate are past
any gap that a wallet restoring the mnemonic looks at. Answer yes if the
wallet was used that much. A no is for a report you do not believe: `sync`
stops, and keeps nothing of what it found. A `sync` that cannot ask stops
there, and generates none of them.

`sync` also checks each pending transaction. When the network lists the
transaction, it is no longer pending, and `sync` removes it from the pending
transactions. It does so even when the network lists it under another id, as
when a miner or another signer changed its signatures. When you sent the
transaction and the server no longer knows it, `sync` sends it again, since
nodes forget unconfirmed transactions. When another transaction spent one of
its coins, `sync` marks it conflicted. It does the same when one of its coins
is gone, as after a reorganization that removed the payment the coin came
from. `sync` leaves alone a signed transaction that you never sent.

After a conflict, the next steps that `sync` prints are `pendingtxs` and
deletetx. `pendingtxs` lists the pending transactions of every portfolio, so
it shows the conflicted one wherever it is. `txs` lists the transactions of
the active portfolio only. So when `sync` finds new transactions only in
other portfolios, its next steps start with the `switch` to them.

`sync` sends a sent transaction again on every sync while the server does not
list it, unless the transaction is conflicted. The server may answer that it
did not publish the transaction. `sync` then goes on. It names the
transaction with what the server said, and gives the deletetx command that
frees its coins. This happens on every sync for a transaction that the
network refuses for good, for example because its fee is below what the nodes
relay. A haskoin-store server waits for its nodes before it gives that
answer, 30 seconds by default, so such a sync takes that long. When a server
gives no answer at all, the sync of its network fails, as it does for any
other request.

A server seldom serves every network: the public one has no regtest chain,
for example. `sync` names in one line the networks that the server does not
serve, and syncs the others. That is no error.

When the server of a network is down, `sync` leaves out that network and says
why. It still syncs the other networks, then exits with an error, so that a
script can tell.

`sync` keeps two blocks of each network from one sync to the next: the block
it reached, and the block a hundred blocks below it. A reorganization replaces
the last blocks of a chain, and `sync` follows one by itself. It never reaches
a hundred blocks down. So when the server does not have the deeper block, the
server is far behind the wallet or on another chain: a new server that still
reads the chain, for example, or a host that names another server. Following
it would show the payments of the wallet as dropped from the network. `sync`
then leaves that network as it was and says why, as for a server that is down.
When the chain of the server is the right one, as after a test network was
reset, --new-chain follows it.
|],
      args =
        CommandSync
          <$> networkOptionWith "Sync only this network. Without it, sync every network, the hidden ones too."
          <*> (syncMode <$> fullOption <*> newChainOption)
    }

-- | The sync that --full and --new-chain ask for. A sync on a new chain
-- is a full one already, so --full adds nothing to it.
syncMode :: Bool -> Bool -> SyncMode
syncMode _ True = NewChain
syncMode True False = Full
syncMode False False = Incremental

-- | --new-chain of sync: follow a server whose chain is not the one of
-- the last sync
newChainOption :: Args Bool
newChainOption =
  switch $
    long "new-chain"
      <> help
        [r|
Follow the server of --network although its chain is not the one of the last
sync, and download everything again as --full does. `sync` refuses such a
server without it. Use it only when you know why the chain changed, as after a
test network was reset: the transactions that the new chain does not have are
marked as dead.
|]

-- | --full of sync: download everything again, in place of what changed
fullOption :: Args Bool
fullOption =
  switch $
    long "full"
      <> help
        [r|
Download all the balances, transactions and coins of the portfolios again,
instead of only what changed, and write them anew. Use it if the wallet shows
something wrong. It replaces any damaged copy of a raw transaction, and marks
as dead a transaction that the server no longer lists. It keeps what the
server cannot tell: labels, locked coins and pending transactions. A
reorganization of the chain does not need it: `sync` sees one by itself.
|]
