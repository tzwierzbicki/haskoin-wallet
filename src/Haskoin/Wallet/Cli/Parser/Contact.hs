{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the address book: newcontact, contacts and
-- deletecontact, each with its options and its help.
-- "Haskoin.Wallet.Commands.Contact" runs them.
module Haskoin.Wallet.Cli.Parser.Contact
  ( newContactCommand,
    contactsCommand,
    deleteContactCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Haskoin.Wallet.Cli.Command (Command (..), NewContact (..))
import Haskoin.Wallet.Cli.Completion (contactAddresses)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (addressTagFilterOption, deleteYesOption, listingOutputHolding, networkOptionWith, pagingOptions, tagOption)
import Text.RawString.QQ (r)

-- | newcontact, which adds an address of somebody else to the address book
newContactCommand :: CommandSpec Command
newContactCommand =
  CommandSpec
    { name = "newcontact",
      offline = False,
      summary = "Add an address of somebody else to the address book",
      details =
        [r|
newcontact adds a contact to the address book. The address book keeps the
addresses of others: the people and the services you pay, or that pay you. A
contact is one address, with a label that says whose it is, and tags.

  hw newcontact ADDRESS Landlord               add an address with its label
  hw newcontact ADDRESS "Alice, savings"       quote a label with spaces
  hw newcontact ADDRESS Alice --tag friends    add it with a tag
  hw newcontact ADDRESS                        add it without a label

The answer shows the address twice. The first line gives it in one piece, to
copy. The line In fours gives it in groups of four characters. Compare the
groups with the address the payee gave you, one group at a time, before you
trust the contact. People compare an address in one piece by its two ends
only, and a look-alike address is made to have the same ends.

What a contact does:

  txs, pendingtxs, `show`    a transaction that pays the address, or that
                           spends from it, shows it underlined, with its
                           label
  `send`, preparetx          the same before you sign a payment to it
  the Tab key              writes the address where a payment takes a
                           recipient: type the start of the address and
                           press Tab. In bash, the start of the label works
                           too: Ali and Tab write the address of Alice.

The address book is for the addresses of others only. newcontact refuses an
address of a wallet of the database. Such an address has a path, a label and
tags already, which `label` and `tag` set.

A contact is one address. Somebody with two addresses is two contacts, which
may share a label, or a tag.

The address book belongs to the whole database, not to a portfolio. The same
contacts show whichever portfolio is active, and they stay when a portfolio or
a wallet is deleted.

A contact is on one network. Most addresses are valid on one network only, and
newcontact takes that one. A legacy address is valid on btc and on bch, and an
address of a test network is valid on the other test networks. newcontact then
takes the default network, unless --network names another.

`label` changes the label of a contact, and `tag` and `untag` change its tags.
Each takes the address of the contact. `show` shows one contact, `contacts`
lists them all, and deletecontact deletes one. exportwallet writes the address
book to its file when it writes every wallet, and importwallet reads it back.
|],
      args =
        CommandNewContact
          <$> ( NewContact
                  <$> strArgument (metavar "ADDRESS" <> help "The address of somebody else to keep. A Bitcoin Cash address may be in either of its forms.")
                  <*> optional (strArgument (metavar "LABEL" <> help "Whose address it is, in your own words. Quote it if it has spaces. `label` changes it later."))
                  <*> many (tagOption "A tag for the contact, as `tag` puts one. Repeat for several tags.")
                  <*> networkOptionWith "The network of the contact, for an address that is valid on several. Without it, the only shown network the address is valid on, or else the default network."
              )
    }

-- | contacts, which lists the contacts of the address book
contactsCommand :: CommandSpec Command
contactsCommand =
  CommandSpec
    { name = "contacts",
      offline = False,
      summary = "List the contacts of the address book",
      details =
        [r|
`contacts` lists the address book: the addresses of others that newcontact
added, each with its label and its tags. The listing has a section for each
network that has contacts. In a section, the contacts are sorted by label,
without regard to capital letters. The contacts without a label come last,
sorted by address. --reverse reverses the order in each section.

Each contact takes two lines. The first gives its address in one piece, to
copy, then its label, and each of its tags between brackets. The second gives
the same address in groups of four characters, to compare with the address the
payee gave you, one group at a time:

  bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  "Alice"  [friends]
    bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4

A Bitcoin Cash address keeps its prefix in one piece before the groups.

--verbose also shows when each contact was added. --tabular shows each contact
on one row of a table: its address, the address in fours, its tags and its
label.

--tag TAG lists only the contacts that have the tag. Repeat it for several
tags: the listing then shows the contacts that have any one of them, and its
first line names those tags. --untagged lists only the contacts that have no
tag. With --tag, the listing shows those as well. --network lists only the
contacts on one network.

--csv FILENAME writes the contacts to a new CSV file, for a spreadsheet. Each
contact takes one row: its network, its address, the address in fours, its
label, its tags, and when it was added. Each time comes in UTC, then in local
time, then with the offset of that local time from UTC.

The address book belongs to the whole database, so the listing is the same
whichever portfolio is active.
|],
      args =
        CommandContacts
          <$> networkOptionWith "Only the contacts on this network. Without it, those on every shown network."
          <*> addressTagFilterOption "contacts that have"
          <*> listingOutputHolding
            "times in UTC and in local time"
            "Show every detail of each contact: its address, the address in fours, its label and its tags, and when it was added."
            "Show each contact on one row of a table: its address, the address in fours, its tags and its label."
            "contacts"
          <*> pagingOptions
            ( optional . strOption $
                long "after"
                  <> metavar "ADDRESS"
                  <> completer (mkCompleter contactAddresses)
                  <> help "Start after the contact with this address, to read the listing in pages of --max rows"
            )
    }

-- | deletecontact, which deletes a contact from the address book
deleteContactCommand :: CommandSpec Command
deleteContactCommand =
  CommandSpec
    { name = "deletecontact",
      offline = False,
      summary = "Delete a contact from the address book",
      details =
        [r|
deletecontact deletes one contact from the address book, with its label, and
takes its tags off it. The answer shows the contact as it was.

Nothing else changes. The transactions that paid the address stay in the
history. From then on, they show the address alone, as they show the address
of a stranger. The tags themselves stay, on everything else that has them.

deletecontact asks before it deletes, and shows the contact with its label.
Any answer but y or yes deletes nothing. --yes answers the question in
advance, for a script.

hw keeps a copy of the labels and of the address book after each command that
changes them, in the auto-backups directory, unless the configuration turns
the copies off. importwallet brings the contacts of such a copy back.
|],
      args =
        CommandDeleteContact
          <$> strArgument (metavar "ADDRESS" <> completer (mkCompleter contactAddresses) <> help "The address of the contact to delete, as `contacts` lists it")
          <*> networkOptionWith "The network of the contact, when the address book has the address on several networks. Without it, the only contact with this address, or else the one on the default network."
          <*> deleteYesOption
    }
