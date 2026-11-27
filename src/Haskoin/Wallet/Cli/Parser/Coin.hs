{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the coins: coins, lockcoin and unlockcoin, each
-- with its options and its help. "Haskoin.Wallet.Commands.Coin" runs them.
module Haskoin.Wallet.Cli.Parser.Coin
  ( coinsCommand,
    lockCoinCommand,
    unlockCoinCommand,
  )
where

import Haskoin
import Haskoin.Wallet.Cli.Command (Command (..))
import Haskoin.Wallet.Cli.Completion (coinsToLock, coinsToUnlock)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (addressTagFilterOption, afterOption, coinReader, listingOutputOption, listingTargetOption, networkOptionWith, pagingOptions)
import Haskoin.Wallet.ShortId (CoinRef (..))
import Text.RawString.QQ (r)

{- coins -}

-- | coins, which lists the coins of a portfolio
coinsCommand :: CommandSpec Command
coinsCommand =
  CommandSpec
    { name = "coins",
      offline = False,
      summary = "List the coins of a portfolio",
      details =
        [r|
A coin is an unspent output of a transaction, written TXID:INDEX. `coins`
lists every coin of the active portfolio, grouped by network. On btc, the
bip44 and the bip84 coins are listed together, since a payment spends both.
--network and --type show one network or one type only.

On each network, the unconfirmed coins come first, then the confirmed ones
from the newest to the oldest. --reverse lists the oldest coins first, and
the unconfirmed ones last.

Until the portfolio has synced on a network, hw has asked no server about
its addresses there, and has no coin to list. When the portfolio has
addresses on such a network, the listing shows one line for it, which names
the network and reads not synced. This does not say that the addresses hold
nothing. --json gives the group of such a network "synced": false, and no
coin. A computer that signs offline never syncs: read the coins on the
online computer.

Each coin takes two lines. The first line starts with the short id of the
coin: the first 8 digits of its txid, a colon, and its index, such as
a1075db5:1. lockcoin, --coin and `show` take the short id, or the coin in
full. Then come the time of the transaction that paid the coin, in the time
zone of the computer, its value, its state and its confirmations, up to 999+
confirmations. The second line gives the address that holds the coin, then
its path, such as bip84/0/3, which starts with the type of the account. Then
come the label of the address and its tags, each between brackets. The
address shows its type too: a bip84 one starts with bc1q on Bitcoin. A coin
at a change address says so on that line.

The state says whether the wallet can spend the coin now. A spendable coin
meets three conditions. Nothing locks it. If it is unconfirmed, it comes
from a transaction of the wallet: it is its change, or what another
portfolio of the wallet paid. If it is a coinbase coin, it has 100
confirmations.

Two kinds of coins that nothing locks wait. An unconfirmed payment from
another wallet says "not spendable yet", and becomes spendable once it
confirms. A coinbase coin is a coin that the first transaction of a block
pays, as the reward of the miner of that block. The network lets nobody
spend it before it has 100 confirmations. Until then it says that it is
mined and counts them, as in "mined, 99 of 100 confirmations".

Three things lock a coin. A pending transaction locks the coins it spends,
until the network has it or deletetx deletes it. A replacement, which
bumpfee prepares, also locks the coins of the payment it replaces: they go
away once the replacement is sent. Such a coin says "locked by tx". You lock
a coin yourself with lockcoin, until unlockcoin frees it. A
tag that locks its coins locks every coin at the addresses that have the
tag. Such a coin says "locked by tag": a payment made with --spend-from-tag
TAG spends it, and hw tag TAG --unlock-coins frees the coins of the tag.

--verbose starts each coin with the time of its transaction, and gives the
full count of its confirmations on its first line. A coin that a pending
transaction spends says "locked by a pending transaction", and a coin of a
payment that a pending transaction replaces says "locked by a pending
replacement". The fields of the coin follow: the label of the transaction
that paid it, the coin in full and its short id, the block that holds it,
the time of its transaction to the second, and the address in full with its
path. The label and the tags of the address are on the line below the
address. An unconfirmed coin is in no block yet. A coin has no label and no
tag of its own: it is in the groups of its address. The change of a payment
you made shows no label of its transaction, since that label says what the
payment was for, not what its change is.

--tabular shows each coin on one row of a table, under a header that names
the columns: the short id of the coin, the time of its transaction, its
value, its state, its confirmations up to 999+, the address that holds it
with its path, the tags of the address, and its label. A coin locked by a
pending transaction says "locked by tx". A coinbase coin with fewer than 100
confirmations says "mined, needs 100", beside the confirmations it has.

--tag TAG lists only the coins at the addresses that have the tag. Repeat it
for several tags: the listing then shows the coins at the addresses that
have any one of them, and its first line names those tags. These are the
coins that --spend-from-tag spends.

--untagged lists only the coins at the addresses that have no tag: the coins
that no --spend-from-tag chooses. With --tag as well, the listing shows
both.

--csv FILENAME writes the coins to a new CSV file, for a spreadsheet, one
coin on each row. The columns are its wallet, portfolio, network and type,
the coin as TXID:INDEX, in full and by its short id, its txid in full and by
its short id, the index of its output, the time of its transaction, its
value, its state, its confirmations and block, the address that holds it
with its path, chain, label and tags, and the label of its transaction. The
state is worded as --verbose words it, but for a coinbase coin with fewer
than 100 confirmations, which says "mined, needs 100" as in a table.
Amounts are in satoshis. Each time takes three columns: in UTC, in local
time, and the offset of that local time from UTC.
|],
      args =
        CommandCoins
          <$> listingTargetOption
          <*> lockedOption
          <*> addressTagFilterOption "coins at an address that has"
          <*> listingOutputOption
            "Show every detail of each coin: the full count of its confirmations, the label of its transaction, the coin in full and its short id, the block that holds it, the time of its transaction, and its address in full with its path."
            "Show each coin on one row of a table: its short id, the time of its transaction, its value, state and confirmations, its address and path, and the tags and the label of the address."
            "coins"
          <*> pagingOptions (afterOption "TXID:INDEX" coinReader "Start after this coin, with its txid in full or by its short id")
    }

-- | --locked of coins: only the locked coins
lockedOption :: Args Bool
lockedOption =
  switch $
    long "locked"
      <> help "List only the locked coins: those a pending transaction locks, those locked with lockcoin, and those a tag locks"

{- lockcoin and unlockcoin -}

-- | lockcoin, which keeps a coin from being spent
lockCoinCommand :: CommandSpec Command
lockCoinCommand =
  CommandSpec
    { name = "lockcoin",
      offline = False,
      summary = "Keep a coin from being spent",
      details =
        [r|
lockcoin keeps one coin from being spent. preparetx and preparesendall leave
a locked coin out, and --coin refuses it. No imported transaction and no
sweep may spend it either. signtx and broadcast refuse a transaction that
spends it, so a file prepared before the lock, or on another computer, does
not spend it. unlockcoin frees it.

lockcoin refuses a coin that is locked already, by lockcoin or by a pending
transaction: one that spends it, or one that replaces the payment it comes
from. It names that transaction. deletetx frees the coins of a pending
transaction. The lock stays while the coin confirms. It goes away with the
coin if a transaction made elsewhere spends the coin. `coins` --locked lists
the locked coins.

Until a block holds a payment, its sender can replace it, to raise its fee.
The same money then arrives as another coin, with another id, at the same
address. sync moves the lock to that coin, and names both coins.

lockcoin locks one coin, and not the next coin that its address receives. To
hold back every coin of a group of addresses, the coins that arrive later
too, make the tag of the group lock its coins: hw tag TAG --lock-coins. The
two locks are separate. You can lock with lockcoin a coin that a tag locks.
The coin then stays locked even for a payment that names the tag with
--spend-from-tag.

lockcoin shows the coin it locked with every detail, as `coins` --verbose
does.

lockcoin finds the coin in any portfolio, whichever is active. --network is
needed only when the wallet has the same coin on several networks. That can
happen because the test networks share their keys. --network then says which
coin to lock.
|],
      args = CommandLockCoin <$> coinArg coinsToLock <*> heldNetworkOption "coin"
    }

-- | unlockcoin, which unlocks a coin locked with lockcoin
unlockCoinCommand :: CommandSpec Command
unlockCoinCommand =
  CommandSpec
    { name = "unlockcoin",
      offline = False,
      summary = "Unlock a coin locked with lockcoin",
      details =
        [r|
unlockcoin frees a coin that lockcoin locked, and no other. To free a coin
that a pending transaction locks, delete that transaction with deletetx. A
coin that a tag locks stays locked: a payment that names the tag with
--spend-from-tag spends it, and hw tag TAG --unlock-coins frees every coin
of the tag.

unlockcoin shows the coin it freed with every detail, as `coins` --verbose
does.
|],
      args = CommandUnlockCoin <$> coinArg coinsToUnlock <*> heldNetworkOption "coin"
    }

-- | A coin of the wallet, as coins lists it, with the coins the shell
-- offers for it
coinArg :: (String -> IO [String]) -> Args CoinRef
coinArg offered =
  argument coinReader $
    metavar "TXID:INDEX"
      <> completer (mkCompleter offered)
      <> help "The coin, as `coins` lists it: the id of its transaction, in full or by its short id, a colon, and its index among the outputs"

-- | The network of an address or a coin the user names, when the wallet
-- has it on several networks
heldNetworkOption :: String -> Args (Maybe Network)
heldNetworkOption what =
  networkOptionWith ("Needed only when the wallet has the " <> what <> " on several networks. This can happen on the test networks, which share their keys. Without it, the one on the default network.")
