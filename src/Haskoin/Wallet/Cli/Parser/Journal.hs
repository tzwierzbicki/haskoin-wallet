{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of exportjournal, which writes the history of the
-- wallet as a journal for ledger. "Haskoin.Wallet.Commands.Journal" runs
-- it.
module Haskoin.Wallet.Cli.Parser.Journal
  ( exportJournalCommand,
  )
where

import Haskoin.Wallet.Cli.Command (Command (..))
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (fileArgument, txTagFilterOptionsOf)
import Text.RawString.QQ (r)

-- | exportjournal, which writes the transactions of the wallet to a journal
-- for ledger
exportJournalCommand :: CommandSpec Command
exportJournalCommand =
  CommandSpec
    { name = "exportjournal",
      offline = False,
      summary = "Write the transactions of the wallet to a journal for ledger",
      details =
        [r|
exportjournal writes the transactions of every portfolio of every wallet to a
new journal for ledger, the command-line accounting program. A journal is a
plain text file that lists each transaction as an accounting entry, by the
rules of double-entry accounting. Include the file in a journal of your own,
and keep your prices and your reports there.

Each transaction is one entry. A portfolio is an account, such as
Assets:hw:wallet0:portfolio0. The network is the commodity of the amounts,
such as BTC or BCH. hw posts what comes from others to Income:Unknown, and
what goes to others to Expenses:Unknown. It posts the fee of a transaction
the wallet made to Expenses:Fees:Mining. A payment between two portfolios is
one entry that moves the money from one account to the other.

A tag can stand for an account of your own. In config.toml, a line such as
rent = "Expenses:Housing:Rent" in the table journal-accounts posts the
payments that have the tag rent to that account. The README explains it.

The name of an entry is the label of its transaction. When the transaction
has none, the name is the label of the contact that the payment pays, or else
the label of the address that received it. The code of an entry is the short
id of the transaction. Its notes give the txid, the time in UTC and the
block, and its tags are those of the transaction. Below the posting of each
payment are the address paid and the label of its contact.

A confirmed transaction is cleared (*), and an unconfirmed one is pending (!).
The journal leaves out the dead transactions, and the transactions that are
prepared but not on the chain yet. An entry has the date of its transaction
on the clock of this computer.

The last entry states the balance of each portfolio. ledger refuses the file
when its entries do not add up to these balances, for instance after someone
edited the file by hand.

With --tag, --address-tag or --untagged, the journal holds only some of the
transactions: those that txs lists with the same options. Use them for the
payments of a business, for instance, to hand to the accountant of that
business:

  hw exportjournal business.ledger --tag business

The journal then holds these transactions from every portfolio, and its first
comment says which ones. A payment between two portfolios keeps its whole
entry when the options select either side of it. Such a journal states no
balance at its end, since the transactions it leaves out moved money too. The
journal that hw keeps up to date always holds every transaction.

The file holds no price and no cost. The file must not exist yet. hw writes
the whole journal each time, so delete the old file first. hw makes the file
readable by you alone, since it tells your whole history.

To have hw keep a journal up to date instead, run hw setconfig --journal. hw
then writes it to ~/.hw/portfolios.ledger after each command that changes it.
|],
      args =
        CommandExportJournal
          <$> fileArgument "The journal to write, such as hw.ledger. It must not exist yet."
          <*> txTagFilterOptionsOf "Write only" "the journal" ""
    }
