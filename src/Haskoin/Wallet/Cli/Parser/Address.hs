{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the addresses: receive, addresses and balances,
-- each with its options and its help. "Haskoin.Wallet.Commands.Address"
-- runs them.
module Haskoin.Wallet.Cli.Parser.Address
  ( receiveCommand,
    addressesCommand,
    balancesCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.Text (Text)
import Haskoin.Wallet.Cli.Command (Command (..), Receive (..), Target (..))
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (addressTagFilterOption, afterOption, listingOutputOption, listingTargetOption, networkMaybeOption, pagingOptions, tagOption, typeOptionWith)
import Text.RawString.QQ (r)

{- receive -}

-- | receive, which gives a new address to receive a payment at
receiveCommand :: CommandSpec Command
receiveCommand =
  CommandSpec
    { name = "receive",
      offline = False,
      summary = "Get a new address to receive a payment",
      details =
        [r|
`receive` gives a new address to hand to a payer: the next receiving address
of one account of the active portfolio. --network and --type pick the account.
Without them, it is the account of the default network and type: a bip44
address on btc, unless the configuration file says otherwise. You never
create an account first. A portfolio already has one for every network and
type.

The address is at the path M/44'/coin'/index'/0/i, or M/84'/coin'/index'/0/i
for bip84, where index is the index of the portfolio. Change does not go to
these addresses. The wallet sends the change of its payments to other
addresses, at .../1/i, and `addresses` --change lists them.

`receive` gives out at most 20 unused addresses in a row. The key gap of the
configuration file changes that number. The limit protects a wallet restored
from the mnemonic: it stops looking after that many unused addresses in a
row, so it would miss a payment to a later address. When that many addresses
wait for a payment, `receive` refuses. Give out one of them again instead:
`addresses` lists them, and `show` shows one again with its QR code.

On a terminal, `receive` prints the QR code of the address under it, for the
payer to scan. The code holds a payment URI with the address and nothing
else, so it does not hold the label. When the answer is piped into another
program, or with --json, `receive` prints the text alone, without the code.

--label gives the new address a label, to recognize the payment later. --tag
puts a tag on it: a word that puts the address in a group, such as kyc.
Repeat --tag for several tags. `label` and `tag` do the same later, on an
address you gave out before.
|],
      args = CommandReceive <$> (Receive <$> accountTargetOption <*> labelOption <*> many (tagOption "A tag to put on the new address, as `tag` does later. Repeat for several tags."))
    }

-- | --label of receive: the label of the new address
labelOption :: Args (Maybe Text)
labelOption =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "LABEL"
      <> help "A label for the address, to recognize the payment later"

-- | Where a command that works on one account of the active portfolio
-- looks: the network and the type, each with its default
accountTargetOption :: Args Target
accountTargetOption =
  Target
    <$> networkMaybeOption
    <*> typeOptionWith "Without it, the default-type of the configuration file, or bip44. The networks of Bitcoin Cash have bip44 only, and refuse bip84."

{- addresses -}

-- | addresses, which lists the receiving addresses of a portfolio, or its
-- change addresses
addressesCommand :: CommandSpec Command
addressesCommand =
  CommandSpec
    { name = "addresses",
      offline = False,
      summary = "List the receiving addresses of a portfolio, or its change addresses",
      details =
        [r|
`addresses` lists every receiving address of the active portfolio. --change
lists its change addresses instead. The addresses come in sections, one for
each network and type: the bip44 addresses on btc, then the bip84 ones, then
those on bch, and so on. A section in which the portfolio has no address is
left out. --network and --type show one network or one type only.

In each section, the newest address comes first. --reverse lists each
section from its first address instead. The sections keep their order.

Each address takes two lines. The first line gives its path, such as
bip84/0/3, then the address, its label, and its tags, each between brackets,
as in [kyc]. The path starts with the type of the account. After the type,
/0/i is the receiving address number i, and /1/i the change address number
i. The second line gives the total of the address, which is the value of
its coins, then what it received, and how many transactions it has: those
that paid it and those that spent from it. An address that no transaction
has paid yet is unused.

Until the portfolio has synced on a network, hw has asked no server about
its addresses there. Their second line then reads not synced, which does not
say that nobody paid them. --verbose, --tabular and --csv show no amount and
no count for such an address, and --json gives it "synced": false. A
computer that signs offline never syncs: read what an address holds on the
online computer.

When the address cannot spend all of its total now, the second line names
the unspendable amount after the total: a coin that is locked, or that waits
for a confirmation. The address can spend the rest. So an address with a
locked coin does not read as one that a payment emptied, whose total is
zero. The help of `balances` says what counts as unspendable.

--verbose adds what the address can spend of its total and what it cannot,
its confirmed and unconfirmed balances, how many coins it holds, and when it
was created.

--tabular shows each address on one row of a table, under a header that
names the columns: the path, the address, its total, what it can spend of
it, what it received, how many transactions it has, its tags, and its label.
Each amount has its unit after it. An address that no transaction has
paid leaves its amounts empty. Change addresses have no label, so their
table has no column for one. The tags and the label are the last two
columns. Many tags or a long label then make only the end of a row longer,
and the columns before them stay lined up.

--tag TAG lists only the addresses that have the tag. A tag is a word that
`tag` puts on addresses to group them. Repeat --tag for several tags: the
listing then shows the addresses that have any one of them, and its first
line names those tags. A word that is not a tag is refused.

--untagged lists only the addresses that have no tag: those you have put in
no group yet. With --tag as well, the listing shows both.

--csv FILENAME writes the addresses to a new CSV file, for a spreadsheet,
one address on each row. The columns are its wallet, portfolio, network and
type, the address, its path, its chain (receiving or change), its label and
its tags, its total, what it can spend of it, its confirmed and unconfirmed
balances, what it received, its coins and its transactions, and when it was
created. The tags
share one cell, with a space between them. Amounts are in satoshis. Each
time takes three columns: in UTC, in local time, and the offset of that
local time from UTC.

--after ADDRESS starts the listing after that address. The listing goes on
with the rest of its section, then with the sections that follow. With
--reverse, it goes on in the reverse order.
|],
      args =
        CommandAddresses
          <$> listingTargetOption
          <*> changeAddressesOption
          <*> addressTagFilterOption "addresses that have"
          <*> listingOutputOption
            "Show every detail of each address: its transactions, what it received, its total, what it can spend of it and what it cannot, its confirmed and unconfirmed balances, its coins, and when it was created."
            "Show each address on one row of a table: its path, the address, its total, what it can spend of it and what it received, its transactions, its tags and its label."
            "addresses"
          <*> pagingOptions (afterOption "ADDRESS" str "Start after this address")
    }

-- | --change of addresses: the change addresses, in place of the receiving
-- ones
changeAddressesOption :: Args Bool
changeAddressesOption =
  switch $
    long "change"
      <> help "List the change addresses instead of the receiving ones"

{- balances -}

-- | balances, which lists the addresses of a portfolio that hold coins
balancesCommand :: CommandSpec Command
balancesCommand =
  CommandSpec
    { name = "balances",
      offline = False,
      summary = "List the addresses of a portfolio that hold coins",
      details =
        [r|
`balances` lists the addresses of the active portfolio that hold coins,
receiving and change addresses together, grouped by network. The title line
of each network gives the total of its addresses. These totals repeat those
of `portfolios`, on purpose: they are the same figures, beside the addresses
that add up to them. On btc, the bip44 and the bip84 addresses are listed
together. --network and --type show one network or one type only.

On each network, the address with the largest total comes first, or the one
with the smallest with --reverse.

Each address takes two lines. The first line gives its path, such as
bip84/0/3, then the address and its label, or the words "change address",
and its tags, each between brackets. The path starts with the type of the
account. The type tells apart the addresses of the bip44 and the bip84
accounts, which both have a /0/3. After the type, /0/i is the receiving
address number i, and /1/i the change address number i. The second line
gives three amounts:

  total          the value of all the coins at the address
  spendable      what the address can spend now, as the help of
                 `portfolios` explains
  unspendable    the rest

The unspendable amount counts the coins locked by a pending transaction, by
lockcoin or by a tag that locks its coins, the unconfirmed payments from
other wallets, and the coinbase coins with fewer than 100 confirmations.
`coins` tells which of these applies to each coin.

Until the portfolio has synced on a network, hw has asked no server what its
addresses hold there, and has no coin to list. When the portfolio has
addresses on such a network, the title line of the network reads not synced
in place of the total, with no address under it. This does not say that the
addresses hold nothing. --json gives the group of such a network
"synced": false, and no address. A computer that signs offline never syncs:
read what the addresses hold on the online computer.

--verbose gives the fields of each address: the total, the number of its
coins, the spendable amount, then the unspendable amount in four parts. The
parts are the value locked by pending transactions, the value locked with
lockcoin, the value locked by a tag, and the value not spendable yet, which
waits for a confirmation, or for 100 of them when it is a coinbase coin.

--tabular shows each address on one row of a table, under a header that
names the columns: the path, the address, the three amounts, its tags, and
its label, or "change address". Each amount has its unit after it.

--tag TAG lists only the addresses that have the tag. Repeat it for several
tags: the listing then shows the addresses that have any one of them, and
its first line names those tags. The total of each network then adds up
those addresses alone, so balances --tag savings tells what the savings
hold.

--untagged lists only the addresses that have no tag. The total of each
network then tells what is in no group. With --tag as well, the listing
shows both.

--csv FILENAME writes the addresses to a new CSV file, for a spreadsheet,
one address on each row. The columns are its wallet, portfolio, network and
type, the address, its path, its chain, its label and its tags, its total,
what it can spend, and the four parts it cannot. Then come the columns that
the file of addresses --csv ends with: its confirmed and unconfirmed
balances, what it received, its coins and its transactions, and when it was
created. Amounts are in satoshis. Each time takes three columns: in UTC, in
local time, and the offset of that local time from UTC.
|],
      args =
        CommandBalances
          <$> listingTargetOption
          <*> addressTagFilterOption "addresses that have"
          <*> listingOutputOption
            "Show every detail of each address: its total and its coins, its spendable amount, what pending transactions lock, what you lock, what a tag locks, and what is not spendable yet."
            "Show each address on one row of a table: its path, the address, its total, spendable and unspendable amounts, its tags and its label."
            "addresses that hold coins"
          <*> pagingOptions (afterOption "ADDRESS" str "Start after this address")
    }
