{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of exportsheet, which writes the wallet as a
-- spreadsheet. "Haskoin.Wallet.Commands.Export" runs it.
module Haskoin.Wallet.Cli.Parser.Export
  ( exportSheetCommand,
  )
where

import Control.Applicative (Alternative (..))
import Haskoin.Wallet.Cli.Command (Command (..))
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (fileArgument, tagOption, untaggedOption)
import Haskoin.Wallet.Database.Tag (TagFilter (..))
import Text.RawString.QQ (r)

-- | exportsheet, which writes every listing of the wallet to a spreadsheet
exportSheetCommand :: CommandSpec Command
exportSheetCommand =
  CommandSpec
    { name = "exportsheet",
      offline = False,
      summary = "Write every listing of the wallet to a spreadsheet",
      details =
        [r|
exportsheet writes the whole wallet to a new spreadsheet. Each listing has a
tab: the portfolios, the addresses, the balances, the coins, the transactions
and the pending transactions. A last tab holds the address book. The file is
in the OpenDocument format of LibreOffice (.ods), which Excel and Google
Sheets open too. Each tab holds every row of its listing, in the same columns
and the same order as its --csv file. So rows copied from one fit the other.

Two more tabs hold the addresses of the transactions and of the pending
transactions. In the tab of the transactions, the row of a transaction names
three addresses: the one it spends the most from, the one it pays the most,
and its change. These two tabs have a row for every address of every
transaction. A row gives the role of the address (from, to or change), its
value and the address. When the wallet holds the address, the row also gives
its path, its chain and its label, and the wallet and the portfolio that hold
it. Each row repeats the txid, the time, the direction and the status of its
transaction. Only the spreadsheet has these rows: no --csv option writes
them.

The tabs hold every portfolio of every wallet. Each row starts with the
columns that say where it belongs: its wallet, its portfolio, its network and
its type. The tab of the portfolios says which one is active. The receiving
addresses and the change addresses share a tab, the receiving ones first.
Each time has three columns: the time in UTC, the local time, and the offset
of that local time from UTC.

Each cell has a type. A label is text, whatever it starts with. An amount is
a number of satoshis. A time is a date and time. The header of each tab stays
at the top as its rows scroll.

With --tag or --untagged, each tab holds only the rows that its listing shows
with the same option. With --tag business, the tabs of the addresses, of the
balances and of the coins hold only the addresses that have the tag. The tabs
of the transactions hold only the transactions that have it, and the address
book only the contacts that have it:

  hw exportsheet business.ods --tag business

With --untagged, each tab holds only the rows that have no tag. The tabs of
the transactions then hold the transactions left to sort, as txs --untagged
lists them. With both options, each tab holds both groups. The tab of the
portfolios always holds every portfolio. The spreadsheet does not say that it
leaves rows out, so name the file after what it holds.

hw reads all the listings at one moment, so the tabs agree with each other.
The file must not exist yet. hw makes it readable by you alone, since it
tells your whole history.
|],
      args =
        CommandExportSheet
          <$> fileArgument "The spreadsheet to write, such as wallet.ods. It must not exist yet."
          <*> sheetTagFilterOptions
    }

-- | The --tag and --untagged options of exportsheet: the tags to keep
-- the rows of in every tab, and whether to keep the rows without a tag.
-- They are the two options every listing takes, and each tab reads them
-- as its listing does: the tags of an address for the addresses, the
-- balances and the coins, those of a transaction for the transactions,
-- and those of a contact for the address book.
--
-- exportsheet has no --address-tag. In txs it keeps the transactions at
-- the addresses of a tag, and a tab of addresses already keeps its rows
-- by the tags of the address with --tag. One command with both would
-- have two options for one thing in half of its tabs.
sheetTagFilterOptions :: Args TagFilter
sheetTagFilterOptions =
  TagFilter
    <$> many (tagOption "Write only the rows that have this tag, in each tab: the addresses that have it, with their balances and their coins, and the transactions and the contacts that have it. Repeat for several tags: each tab then includes the rows that have any one of them. `tags` lists the tags.")
    <*> untaggedOption "Write only the rows without a tag, in each tab: the addresses that have none, with their balances and their coins, the contacts that have none, and the transactions left to sort, as txs --untagged lists them. With --tag, each tab includes those as well."
