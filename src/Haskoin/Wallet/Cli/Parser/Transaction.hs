{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the transactions the wallet holds: txs, pendingtxs,
-- exporttx, importtx and deletetx, each with its options and its help.
-- "Haskoin.Wallet.Commands.Transaction" runs them.
module Haskoin.Wallet.Cli.Parser.Transaction
  ( txsCommand,
    pendingTxsCommand,
    exportTxCommand,
    importTxCommand,
    deleteTxCommand,
  )
where

import Haskoin.Wallet.Cli.Command (Command (..))
import Haskoin.Wallet.Cli.Completion (pendingHashes)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (afterOption, deleteYesOption, fileArgument, hashRefReader, listingOutputOption, networkOptionWith, pagingOptions, txTagFilterOptionsOf)
import Haskoin.Wallet.Database.Tag (TxTagFilter (..))
import Haskoin.Wallet.ShortId (HashRef (..))
import Text.RawString.QQ (r)

{- txs -}

-- | txs, which lists the transactions of a portfolio
txsCommand :: CommandSpec Command
txsCommand =
  CommandSpec
    { name = "txs",
      offline = False,
      summary = "List the transactions of a portfolio",
      details =
        [r|
`txs` lists the transactions of the active portfolio, grouped by network.
--network shows one network only. `txs` shows a transaction as the portfolio
sees it, whichever accounts of the portfolio it touches. A transaction that
spends bip44 and bip84 coins together is one transaction.

The list is as of the last sync. On each network, the transactions follow the
order of the chain, the newest first: the unconfirmed ones, then the confirmed
ones from the newest block to the oldest. --reverse reverses that order: the
oldest transactions come first and the unconfirmed ones last, each with the
same balance beside it. The networks keep their order. The transactions you
prepared, and those you sent that `sync` has not seen yet, are not in this
list: pendingtxs lists them. `show` TXID shows one transaction in full.

Until the portfolio has synced on a network, hw has asked no server about
its addresses there, and has no transaction to list. When the portfolio has
addresses on such a network, the listing shows one line for it, which names
the network and reads not synced. This does not say that nobody paid the
addresses. --json gives the group of such a network "synced": false, and no
transaction. A computer that signs offline never syncs: read the
transactions on the online computer.

In the compact view, each transaction takes two lines. The first line starts
with the short id of the transaction: the first 8 digits of its txid. The
other commands take the short id in place of the txid. The line then gives the
time, the amount and the status of the transaction. The time is in the time
zone of the computer. The amount includes the fee, and has a plus sign when
funds come in. The status gives the confirmations, up to 999+ confirmations.
--verbose gives the full count. An unconfirmed transaction shows its fee rate
too, which tells how soon a block may take it. A dead transaction is one that
was replaced, or dropped from the network.

The second line gives the balance of the portfolio on the network after the
transaction. The balance beside the newest transaction is what the portfolio
holds there now, confirmed or not. A dead transaction shows no balance, and
the balances do not count it. Then comes what the transaction is for. A
payment received names the address that was paid, by its type and its path,
such as bip84/0/3. Then comes the label of the payment, or else the label of
the address. A payment you made gives its label, or else the address it pays.
When it pays several addresses, it gives the one it pays the most. Nothing in
the line is cut, not even a long label or address. On a terminal, the
addresses of the wallet and their labels are blue. The tags of the transaction
end the line, each between brackets, as in [rent].

When another transaction replaced a dead transaction, the second line of the
dead one starts with the short id of its replacement, as in replaced by
2b69e4f4. The replacement is the live transaction that spends a coin the dead
one spent. hw does not store this note. It reads it from the coins at each
listing, so the note follows the network. When a miner confirms a stuck
payment after all, the replacement of that payment is the dead one, and names
the payment. A dead payment of yours that names no replacement was dropped. A
dead payment you received never names one. A replacement that bumpfee made
pays the recipients of the payment. Any other replacement may pay somebody
else.

With --verbose, each transaction starts with its time, then its type:

  Incoming    funds enter the portfolio
  Outgoing    funds leave the portfolio
  Internal    funds move between addresses of the portfolio, from a bip44
              address to a bip84 one for instance

A payment to another portfolio of the wallet is Outgoing in this portfolio and
Incoming in the other one. Then come the network, and the types of the
addresses of the portfolio that the transaction involves. The txid is in full,
then its short id. Every address is in full, and an address of the wallet has
its path after it. The line below an address of the wallet gives the label and
the tags of that address. Below an address that the transaction spends from,
the line adds the labels of the coins spent, after the word from. A long label
wraps under the address. The tags of the transaction are in a Tags field. A
dead transaction has a Status field, which names its replacements by their
txids in full.

With --tabular, each transaction takes one row of a table, under a header
that names the columns. The first column, Txid, gives the short id. Each
amount has its unit after it. The Confs column gives the
confirmations as a number alone, up to 999+. An unconfirmed transaction has 0
there, and the table does not show its fee rate. A dead transaction says dead.
The Tags column gives the tags of the transaction. The last column,
Description, says what the transaction is for, in the words of the compact
view. The description of a dead transaction that was replaced names its
replacement first.

A tag is a word you put on transactions and addresses to group them. A
transaction has tags of its own, which `tag` TXID puts on it, and its
addresses have theirs. The compact view and the table show only the tags of
the transaction. --verbose shows both. Three options filter the listing:

  --tag TAG           shows only the transactions that have the tag
  --address-tag TAG   shows only the transactions that spend from or pay an
                      address of the portfolio that has the tag, and those
                      that pay or were paid from a contact of the address
                      book that has it
  --untagged          shows only the transactions left to sort: those that
                      have no tag, and that neither pay a contact that has a
                      tag nor were paid by one

You can repeat the first two, and combine the three: the listing then shows
the transactions that match any one of them. Its first line names the tags.
With --address-tag, the compact view and the table also show each address that
has one of the tags you asked for, with that tag beside it. They name an
address of the portfolio by its path, and a contact by its address and its
label. A filter leaves transactions out, and does not change the ones it
shows: each still shows the balance of the portfolio after it, as in the whole
listing.

Use --untagged to sort your transactions. Tag what you recognize, the
transaction or its contact, and list again: the listing gets shorter until
nothing is left. The tags of the addresses of the portfolio do not count for
--untagged. They say which group the coins are in, not what a payment is for.

With --csv FILENAME, hw writes the transactions to a new CSV file for a
spreadsheet, one on each row. The columns are the wallet, the portfolio and
the network of the transaction, the types of address it involves, its txid in
full and by its short id, its time, its direction (Incoming, Outgoing or
Internal), its amount, fee, fee rate and size, the balance after it, its
status, confirmations and block, the transaction that replaced it when it is
dead, its label and its tags.

Then come three addresses of the transaction: the one it spends the most from,
the one it pays the most, and its change. Each has its value, and its path,
label and tags when it is an address of the portfolio. One more column counts
the addresses that have no column of their own. The lock time comes last.
Amounts are in satoshis. Each time takes three columns: the time in UTC, the
time in local time, and the offset of that local time from UTC. The
spreadsheet of exportsheet lists every address of every transaction, in a tab
of its own.
|],
      args =
        CommandTxs
          <$> networkOptionWith "List only the transactions on this network. Without it, list those on every shown network of the portfolio."
          <*> txTagFilterOptions
          <*> listingOutputOption
            txVerboseHelp
            "Show each transaction on one row of a table: its short id, time, type, amount, balance, confirmations and tags. The last column says what the transaction is for: its label, the path of an address of the wallet, or the address paid."
            "transactions"
          <*> pagingOptions (afterOption "TXID" hashRefReader "Start after this transaction, by its txid or its short id")
    }

-- | The help of --verbose, the same in txs and in pendingtxs
txVerboseHelp :: String
txVerboseHelp = "Show every detail of each transaction: its label, its hash in full and its short id, each address it spends from or pays, with its amount, path and label, the labels of the coins spent as `coins` shows them, and the fee."

-- | The --tag, --address-tag and --untagged options of a listing of
-- transactions: the tags of transactions and the tags of addresses to
-- keep the rows of, and whether to keep the transactions left to sort.
-- Together, and repeated, they keep the transactions that match any one
-- of them.
txTagFilterOptions :: Args TxTagFilter
txTagFilterOptions = txTagFilterOptionsOf "Filter: only" "the listing" " Each transaction shows the tag beside the address that has it."

{- pendingtxs -}

-- | pendingtxs, which lists the pending transactions
pendingTxsCommand :: CommandSpec Command
pendingTxsCommand =
  CommandSpec
    { name = "pendingtxs",
      offline = False,
      summary = "List the pending transactions",
      details =
        [r|
A pending transaction is one that preparetx, preparesendall, bumpfee or
preparesweep made, or that importtx added, and that the network does not list
yet. It is not signed, signed and not sent, or sent. A sent transaction leaves
this list once a sync finds it on the network. The command that sends it runs
that sync, so a sent transaction stays here only when that sync failed, and
`sync` then takes it off. `show` HASH shows one pending transaction in full.

pendingtxs lists the pending transactions of every portfolio of every wallet,
not those of the active portfolio alone. Each one waits for you to sign, send
or delete it. The commands that take a pending transaction find its portfolio
by themselves, so none needs a switch first. The list is sorted by wallet and
portfolio, then by network, in the order portfolios shows the networks. On
each network, the newest transaction comes first, or the oldest with
--reverse. --network shows the transactions of one network only.

In the compact view, each transaction takes two lines. The first line starts
with the short id of the transaction: the first 8 digits of its hash. For a
transaction that is not signed, the hash is its nosigHash, the hash of the
transaction without its signatures. For a signed one, it is its txid. The
other commands take that hash, in full or by its short id, to name a pending
transaction. The nosigHash of a signed transaction works too. --verbose gives
the hash in full, then its short id. The first line then gives the time the
transaction was created, its amount, its status (not signed, signed and not
sent, or sent) and its fee rate.

The second line says where the transaction is: its portfolio and its network.
When the database has more than one wallet, it names the wallet too, as in
portfolio0 of wallet1. A replacement that bumpfee made then names the
transactions it replaces, in yellow on a terminal. The network keeps only one
of a replacement and the transactions it replaces. Then comes what the
transaction is for, in the words of txs: its label, or else the address it
pays. The line names an address of the wallet by its type and its derivation
path. The tags of the transaction end the line. --verbose names the replaced
transactions in full, in yellow too.

Three options filter the listing, as they do for txs. --tag TAG shows only the
pending transactions that have the tag. --address-tag TAG shows only those
that spend from or pay an address of their portfolio that has the tag, and
those that pay or were paid from a contact of the address book that has it.
--untagged shows only those left to sort: those that have no tag, and that
neither pay a contact that has a tag nor were paid by one. `tag` puts tags on
a pending transaction, by either of its hashes. The transaction keeps them
once the network has it.

The status conflicted means that the transaction spends a coin that another
transaction has spent, or a coin that no longer exists, as the server said at
the last sync. The transaction cannot be sent while that lasts. Delete it with
deletetx, which frees its coins.

With --tabular, each transaction takes one row of a table, under a header that
names the columns: its short id, its wallet when there are several, its
portfolio and its network, the time it was created, its type, its amount, its
status, its fee rate in sat/vB, its tags, and what it is for, as txs --tabular
says it. The description of a replacement starts with the transactions it
replaces, in yellow.

With --csv FILENAME, hw writes the pending transactions to a new CSV file for
a spreadsheet, one on each row. The file has the columns of txs --csv that
apply to a pending transaction, in the same order. The nosigHash comes before
the txid, each in full and by its short id. The file has no time on the chain,
no balance, no confirmations and no block. It adds two columns: the change
that was too small for an output and that the fee took in, and the
transactions that a replacement replaces. The time the transaction was created
comes last. Amounts are in satoshis. Each time takes three columns: the time
in UTC, the time in local time, and the offset of that local time from UTC.
|],
      args =
        CommandPendingTxs
          <$> networkOptionWith "List only the pending transactions on this network. Without it, list those on every shown network of every portfolio."
          <*> txTagFilterOptions
          <*> listingOutputOption
            txVerboseHelp
            "Show each transaction on one row of a table: its short id, wallet when there are several, portfolio, network, time, type, amount, status, fee rate and tags, then what it is for."
            "pending transactions"
          <*> pagingOptions (afterOption "HASH" hashRefReader "Start after this pending transaction, by either of its hashes or its short id")
    }

{- exporttx -}

-- | exporttx, which writes a pending transaction to a file
exportTxCommand :: CommandSpec Command
exportTxCommand =
  CommandSpec
    { name = "exporttx",
      offline = False,
      summary = "Write a pending transaction to a file",
      details = importExportTxDetails,
      args =
        CommandExportTx
          <$> pendingHashArg
          <*> fileArgument "The file to write. It must not exist yet."
    }

-- | The details that the helps of exporttx and importtx share: the two
-- commands are the two ends of one file
importExportTxDetails :: String
importExportTxDetails =
  [r|
exporttx writes a pending transaction to a file, signed or not, to carry it to
another computer, such as the offline computer that signs it. Name the
transaction by the hash that pendingtxs shows: its nosigHash, or its txid once
it is signed, in full or by its short id.

The file is a PSBT, the format of BIP 174, which most wallets read. It
carries the transaction, the whole transaction that each of its coins comes
from, the public keys of the portfolio in it with their derivation paths, and
the signatures once it is signed.

hw adds what the format has no field for, so that the offline computer can
show it: the network of the transaction, the fee rate it was built for, the
transactions that a replacement replaces, the label of the transaction, the
labels of the addresses of the portfolio in it, and the labels of the coins it
spends, as the wallet has them when it writes the file. They go in the
proprietary fields of the format, under the identifier hw, which other
software ignores. bitcoin-cli decodepsbt shows the file too.

importtx adds the transaction of a file to the pending transactions of a
portfolio, and locks the coins it spends. When the wallet holds the
transaction already, the file takes its place, as when an offline computer
returns it signed. hw checks the signatures first. The wallet keeps its own
labels, and shows the transaction with them. A file that brings nothing new
changes nothing, and is no error: importtx says that the wallet holds the
transaction already.

importtx takes a file that preparetx wrote on another computer that holds the
portfolio, once this one is synced and has the coins the transaction spends.
It generates the new change address of the payment from the path the file
gives, up to a thousand addresses past the last used address of its chain,
and holds it for the transaction.

hw finds the portfolio by itself, from the addresses the transaction spends
from and pays. A transaction that moves funds between two portfolios goes to
the portfolio that pays. In two cases hw cannot tell which portfolio it is:
when several portfolios pay, and when none pays and the transaction pays
several. It then takes the active portfolio if that is one of them. Otherwise
it names them, and imports nothing: switch to the one to use, and import the
file again.

importtx takes a file that another wallet or a hardware wallet signed the
same way. The file can be in bytes or in base64 text, finalized or not. hw
finalizes an input that is signed and not finalized, and refuses a signature
that does not verify. Such a file may leave out the paths of the addresses of
the portfolio, once the signatures are final, the fee rate and the label. The
wallet keeps them from the version it holds. importtx also takes a file that
another wallet prepared for a portfolio of this wallet. Sign it with signtx.

broadcast --input sends a signed file without importtx.
|]

-- | A pending transaction, by either of its hashes, in full or by its
-- short id
pendingHashArg :: Args HashRef
pendingHashArg =
  argument hashRefReader $
    metavar "HASH"
      <> completer (mkCompleter pendingHashes)
      <> help "A pending transaction, by the hash pendingtxs shows: its nosigHash, or its txid once it is signed, in full or by its short id, the first 8 digits"

{- importtx -}

-- | importtx, which adds a transaction file to the pending transactions
importTxCommand :: CommandSpec Command
importTxCommand =
  CommandSpec
    { name = "importtx",
      offline = False,
      summary = "Add a transaction file to the pending transactions",
      details = importExportTxDetails,
      args =
        CommandImportTx
          <$> fileArgument "The transaction file to add"
    }

{- deletetx -}

-- | deletetx, which deletes a pending transaction and frees its coins
deleteTxCommand :: CommandSpec Command
deleteTxCommand =
  CommandSpec
    { name = "deletetx",
      offline = False,
      summary = "Delete a pending transaction and free its coins",
      details =
        [r|
deletetx removes a pending transaction from the wallet. The coins it spends
can then be spent again, and its change address is free again. That address
loses its tags, since the next payment takes it for its own change.

A transaction sent with broadcast can be deleted only once the server no longer
knows it. That happens when the nodes dropped it from their mempools, or never
accepted it. While the server lists the transaction, it may still confirm, and
the next `sync` takes it off the pending transactions.

Delete a transaction that pendingtxs marks as conflicted. Another transaction
spent one of its coins, or the coin no longer exists, so it can no longer be
sent.

deletetx asks before it deletes, and says whether the transaction is signed
or was sent. Any answer but y or yes deletes nothing. --yes answers the
question in advance, for a script.
|],
      args = CommandDeleteTx <$> pendingHashArg <*> deleteYesOption
    }
