{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the tags: tag, untag, tags, renametag and
-- deletetag, each with its options and its help, and the readers of a tag
-- typed as an argument. "Haskoin.Wallet.Commands.Tag" runs them.
module Haskoin.Wallet.Cli.Parser.Tag
  ( tagCommand,
    untagCommand,
    tagsCommand,
    renameTagCommand,
    deleteTagCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.List (intercalate)
import Data.String.Conversions (cs)
import Haskoin.Wallet.Cli.Command (Command (..), RenameTag (..), TagOptions (..), Target (..), Untag (..), labelledShapeOf)
import Haskoin.Wallet.Cli.Completion (tagNames, tagTargets, untagTargets)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (deleteYesOption, namedAddressNetworkOption, tagReader, typeOptionWith)
import Haskoin.Wallet.Tags (Tag, colorName, noColorWord, readColorChoice, readTag, tagColors)
import Text.RawString.QQ (r)

-- | tag, which puts tags on an address, a transaction or a contact, or sets
-- the colour or the lock of a tag
tagCommand :: CommandSpec Command
tagCommand =
  CommandSpec
    { name = "tag",
      offline = False,
      summary = "Put tags on an address, a transaction or a contact, or colour or lock a tag",
      details =
        [r|
A tag is a word you put on addresses and transactions to group them, such as
kyc, savings or rent. A label says what one thing is for. A tag names a group,
and the other commands work on the whole group. A listing can show only the
rows that have a tag. A payment can spend only the coins at the addresses that
have a tag.

`tag` adds tags to an address, a transaction or a contact. The first word
names what to tag, and the tags follow. The tags that are there already stay:

  hw tag ADDRESS kyc cold        tag a receiving or a change address
  hw tag bip84/0/3 kyc           tag the address at a path of the active
                                 portfolio
  hw tag TXID rent               tag a transaction, by its txid, or its
                                 nosigHash while pending

A tag is one word, made of the letters a to z, digits, - and _, with 32
characters at most. Capital letters are taken as small ones, so Kyc and kyc
are one tag. Spaces, accents and other signs are refused. An address or a
transaction can have several tags. A tag exists from the first time you use
it. You do not create it first.

The first word is an address of the wallet, a path with the type of its
account, such as bip84/0/3, or a transaction, in full or by its short id.
`label` takes the same first word. A change address takes tags too, although
it takes no label: a tag says which group its coins belong to.

`tag` finds an address or a transaction in any portfolio, whichever is
active. A path names an address of the active portfolio. A transaction that
several portfolios have is tagged in each of them. The test networks share
their keys, so the wallet can have one address on several networks. --network
then says which one to tag.

--color gives a colour to each tag of the command: red, green, yellow, blue,
magenta or cyan. The screen then shows the tag in that colour. none removes
the colour. With --color and one word only, the word is the tag to colour,
and nothing is tagged:

  hw tag kyc --color red         colour the tag kyc
  hw tag kyc --color none        remove its colour

A tag has one colour in the whole database, on everything that has the tag. A
colour is only a help to the eye. No command behaves differently for it.

--lock-coins makes a tag lock its coins. From then on, no payment spends the
coins at an address that has the tag, unless the payment names the tag with
--spend-from-tag. The coins that arrive later are locked too. Use it for
savings, so that an everyday payment never spends them. Give --lock-coins with
one word only: the word is the tag, and nothing is tagged:

  hw tag savings --lock-coins      the tag savings locks its coins
  hw send ADDRESS:0.01             leaves the coins of savings
  hw send ADDRESS:0.01 --spend-from-tag savings
                                   spends them, this once
  hw tag savings --unlock-coins    the tag no longer locks its coins

lockcoin locks one coin, and not the coins that arrive after it. A tag locks
every coin at the addresses that have it, and it can lock its coins before
any address has it.

Only --spend-from-tag with the tag spends a coin that the tag locks. --coin
with the coin, or --spend-from with its address, does not spend it. A sweep or
an imported transaction may not spend it either. When an address has two tags
that lock their coins, a payment must name both tags to spend from it.

The lock wins over a payment prepared before it. When a pending transaction
spends a coin that comes under the lock, tag says so, and signtx and
broadcast then refuse that transaction. deletetx deletes it. A transaction
that was sent already is on the network, and no lock stops it. A payment that
named the tag with --spend-from-tag is not stopped.

`coins` shows such a coin as locked by tag, `balances` --verbose counts it
apart, and `tags` says which tags lock their coins. Only a tag on an address
locks coins. The same tag on a transaction or on a contact locks nothing.

The change of a payment made with --spend-from-tag goes to a new change
address. That address has no tag, so nothing locks the change. Add
--tag-change to tag it, so that it stays locked.

What tags do:

  --tag TAG              on addresses, balances and coins, lists only the rows
                         of the addresses that have the tag. On txs and
                         pendingtxs, lists only the transactions that have it.
  --address-tag TAG      on txs and pendingtxs, lists only the transactions
                         that spend from or pay an address that has the tag,
                         or a contact of the address book that has it
  --spend-from-tag TAG   on `send`, preparetx, sendall and preparesendall,
                         spends the coins at the addresses that have the tag
  receive --tag TAG      tags the new address
  tag TAG --lock-coins   locks the coins at the addresses that have the tag:
                         only a payment that names the tag spends them

hw puts no tag on the change of a payment by itself. Two options tag the
change, so that it stays in the group of the coins spent:

  --tag-change           on `send` and preparetx, tags the new change address
                         with the tags that every coin spent has
  tag --with-change      tags the change of the payments made before

--with-change tags the address, then the change of every payment that spent
from it, then the change of the payments that spent that change, and so on
down to the coins you hold now. The answer lists the change addresses it
tagged, then those where the tag stopped, each with the payment and the
address that stopped it.

  hw tag ADDRESS kyc --with-change   the address, and its change down the chain

A change address takes a tag only when every coin that the payment spent has
the tag. --tag-change follows the same rule. So the change of a payment that
also spent a coin from outside the group takes no tag. Each run looks at every
change address down the chain from the address, whatever tags it has. So a
second run completes a chain that was tagged in part. A tag passes from the
coins a payment spends to its change, and never the other way: a tag on a
change address says nothing about the addresses its coins came from.

A contact of the address book takes tags too. Name it by its address: hw tag
ADDRESS friends. `contacts` --tag TAG then lists the contacts that have the
tag, and txs --address-tag TAG lists the transactions that pay them or that
they paid.

`untag` takes tags off, `tags` lists them, renametag gives one a new name, and
deletetag deletes one everywhere. exportwallet writes the tags to its file,
and importwallet reads them back.
|],
      args =
        CommandTag
          <$> ( TagOptions
                  <$> strArgument
                    ( metavar "ADDRESS|TXID|PATH"
                        <> completer (mkCompleter tagTargets)
                        <> help "What to tag: an address of the wallet, the address of a contact of the address book, a path of the active portfolio, such as bip84/0/3, or a transaction. Name a transaction by its txid, or by the hash pendingtxs shows, in full or by its short id. With --color, --lock-coins or --unlock-coins and no tag after it, this word is the tag to change."
                    )
                  <*> many (argument tagArgReader (metavar "TAG" <> completer (mkCompleter tagNames) <> help "A tag to put on it. Give several tags one after the other."))
                  <*> optional
                    ( option (eitherReader (readColorChoice . cs)) $
                        long "color"
                          <> metavar "COLOR"
                          <> completeWith (cs <$> (map colorName tagColors <> [noColorWord]))
                          <> help "The colour of each tag given: red, green, yellow, blue, magenta or cyan, or none to remove it. A tag has one colour, on everything that has the tag."
                    )
                  <*> switch
                    ( long "lock-coins"
                        <> help "With a tag alone: the tag locks its coins from now on. No payment spends the coins at an address that has the tag, unless the payment names the tag with --spend-from-tag. The coins that arrive later are locked too."
                    )
                  <*> switch
                    ( long "unlock-coins"
                        <> help "With a tag alone: the tag no longer locks its coins."
                    )
                  <*> withChangeOption "Also tag the change of the payments that spent from the address, then the change of the payments that spent that change, and so on down to the coins held now. A change address takes a tag only when every coin that the payment spent has the tag. With an address or a path only."
                  <*> ( Target
                          <$> namedAddressNetworkOption
                          <*> typeOptionWith "With a path without its type, such as /0/3, the type of its account. Without it, the default-type of the configuration file, or bip44."
                      )
              )
    }

-- | untag, which takes tags off an address, a transaction or a contact
untagCommand :: CommandSpec Command
untagCommand =
  CommandSpec
    { name = "untag",
      offline = False,
      summary = "Take tags off an address, a transaction or a contact",
      details =
        [r|
`untag` takes tags off an address, a transaction or a contact, and leaves its
other tags. The first word names what to take the tags off: an address of the
wallet or of the address book, a path of the active portfolio, or a
transaction. `tag` takes the same first word.

  hw untag ADDRESS kyc           the address is no longer in the group kyc
  hw untag TXID rent tax         take two tags off a transaction

`untag` refuses a tag that the address or the transaction does not have, and
changes nothing: you most likely mistyped the tag. A transaction that several
portfolios have loses the tags in each.

--with-change also takes the tags off the change of every payment that spent
from the address, then off the change of the payments that spent that change,
and so on. `tag` --with-change puts tags on the same change addresses. Use it
to take a tag put by mistake off the whole chain at once:

  hw untag ADDRESS kyc --with-change

It does so even when the address has lost the tag already, and is refused
only when no address of the chain has the tag.

The tag itself stays, with its colour, even when nothing has it any more.
`tags` then shows it as not used. deletetag deletes a tag everywhere.
|],
      args =
        CommandUntag
          <$> ( Untag
                  <$> strArgument
                    ( metavar "ADDRESS|TXID|PATH"
                        <> completer (mkCompleter untagTargets)
                        <> help "What to take the tags off: an address of the wallet, the address of a contact of the address book, a path of the active portfolio, such as bip84/0/3, or a transaction. Name a transaction by its txid, or by the hash pendingtxs shows, in full or by its short id."
                    )
                  <*> some (argument tagArgReader (metavar "TAG" <> completer (mkCompleter tagNames) <> help "A tag to take off it. Give several tags one after the other."))
                  <*> withChangeOption "Also take the tags off the change of the payments that spent from the address, then off the change of the payments that spent that change, and so on down. With an address or a path only."
                  <*> ( Target
                          <$> namedAddressNetworkOption
                          <*> typeOptionWith "With a path without its type, such as /0/3, the type of its account. Without it, the default-type of the configuration file, or bip44."
                      )
              )
    }

-- | tags, which lists the tags, with their colours and what has them
tagsCommand :: CommandSpec Command
tagsCommand =
  CommandSpec
    { name = "tags",
      offline = False,
      summary = "List the tags, with their colours and what has them",
      details =
        [r|
`tags` lists every tag of the database, in alphabetical order. Each line gives
the tag in its colour, the name of that colour, and how many addresses,
transactions and contacts of the address book have it. A tag that locks its
coins says so at the end of its line:

  cold     blue  2 addresses
  friends        3 contacts
  kyc      red   12 addresses, 3 transactions
  rent           not used
  savings  blue  4 addresses, locks its coins

No payment spends the coins at the addresses that have such a tag, unless the
payment names the tag with --spend-from-tag. The help of `tag` says more.

The counts cover every portfolio of every wallet, since a tag belongs to the
whole database. A transaction that two portfolios have counts once. A tag that
nothing has reads not used. It stays until deletetag deletes it.

To see what has a tag, run `addresses` --tag TAG, `coins` --tag TAG or txs
--tag TAG in the active portfolio, or `contacts` --tag TAG.

To change a tag:

  hw tag kyc --color blue        give it a colour, or another one
  hw tag kyc --color none        remove its colour
  hw tag kyc --lock-coins        make it lock its coins
  hw tag kyc --unlock-coins      stop it from locking them
  hw renametag kyc verified      give it a new name
|],
      args = pure CommandTags
    }

-- | renametag, which gives a tag a new name, or merges it into another tag
renameTagCommand :: CommandSpec Command
renameTagCommand =
  CommandSpec
    { name = "renametag",
      offline = False,
      summary = "Give a tag a new name, or merge it into another tag",
      details =
        [r|
renametag gives a tag a new name in the whole database: on every address and
every transaction that has it, in every portfolio of every wallet, and on
every contact of the address book. The tag keeps its colour. The answer says
how many addresses, transactions and contacts have it.

  hw renametag kyc verified              the tag kyc is now verified
  hw renametag saving savings --merge    saving and savings become one tag

The new name follows the rules of a tag, as `tag` gives them: one word, made
of the letters a to z, digits, - and _, with 32 characters at most. Capital
letters are taken as small ones.

renametag refuses a name that another tag has already. With --merge, it merges
the two tags instead. Everything that had the first tag then has the second
one, and the first tag is gone. Use it for a tag that you typed two ways, such
as saving and savings. The second tag keeps its colour, or takes the colour of
the first when it has none.

renametag cannot split a merged tag again, since nothing records any more
which of the two tags each address or transaction had. So renametag merges
only with --merge. With --merge, the second word must be a tag that exists.
renametag refuses any other word, since you most likely mistyped it.

Only the name changes. To change the colour of a tag, run hw tag TAG --color
COLOR.

hw keeps a copy of the labels and the tags after each command that changes
them, in the auto-backups directory, unless the configuration turns the copies
off. The copy made before a merge has the two tags as they were. To undo a
merge, delete the merged tag with deletetag, then run importwallet with that
copy.
|],
      args =
        CommandRenameTag
          <$> ( RenameTag
                  <$> argument tagReader (metavar "TAG" <> completer (mkCompleter tagNames) <> help "The tag to rename, as `tags` lists it")
                  <*> argument newTagNameReader (metavar "NEW-NAME" <> completer (mkCompleter tagNames) <> help "Its new name: a word that follows the rules of a tag. With --merge, the tag to merge it into.")
                  <*> switch (long "merge" <> help "Merge the tag into the tag that has the new name already. Without it, a name that another tag has is refused.")
              )
    }

-- | deletetag, which deletes a tag from every address, transaction and
-- contact
deleteTagCommand :: CommandSpec Command
deleteTagCommand =
  CommandSpec
    { name = "deletetag",
      offline = False,
      summary = "Delete a tag from every address, transaction and contact",
      details =
        [r|
deletetag deletes a tag from the whole database, with its colour. It takes the
tag off every address and every transaction that has it, in every portfolio of
every wallet, and off every contact of the address book. The answer says how
many had it.

`untag` takes a tag off one address or one transaction, and keeps the tag.
deletetag is for a tag you no longer want at all. For a tag you mistyped,
renametag gives it its right name.

deletetag asks before it deletes, and says how many addresses, transactions
and contacts lose the tag. Any answer but y or yes deletes nothing. --yes
answers the question in advance, for a script.

Nothing else is deleted: the addresses, the transactions and their labels stay
as they are. hw keeps a copy of the labels and the tags after each command
that changes them, in the auto-backups directory, unless the configuration
turns the copies off. importwallet brings the tags of such a copy back.
|],
      args =
        CommandDeleteTag
          <$> argument tagReader (metavar "TAG" <> completer (mkCompleter tagNames) <> help "The tag to delete, as `tags` lists it")
          <*> deleteYesOption
    }

-- | The --with-change option of tag and of untag, with the help of the
-- command about it: whether the command goes down the chain of change of
-- the address ("Haskoin.Wallet.Database.ChangeChain"). It is an option,
-- and not what tag always does: a tag on an address may say something of
-- that address alone, such as what it receives, and the user who wants
-- the group to follow its coins says so.
withChangeOption :: String -> Args Bool
withChangeOption what = switch (long "with-change" <> help what)

-- | A tag typed as an argument of tag or untag, after what to tag. A word
-- that is no tag and reads as an address, a path or a hash was most
-- likely typed in the wrong place, the tags before what they go on. The
-- refusal then says the order, rather than which rule of a tag the
-- address breaks.
tagArgReader :: ReadM Tag
tagArgReader = eitherReader $ \word -> case (readTag (cs word), labelledShapeOf word) of
  (Right tag, _) -> Right tag
  (Left _, Just shape) -> Left (word <> " reads as " <> shape <> ", not as a tag. What to tag comes first, then the tags: ADDRESS|TXID|PATH TAG")
  (Left refusal, Nothing) -> Left refusal

-- | The new name of a tag, as renametag takes it: a tag, read as any other.
-- A name with a space has a refusal of its own. The refusal of 'readTag'
-- goes on to say that several tags are given one after the other, which is
-- right for the command tag and wrong for a name. The user who types a name
-- of two words wants to know how to keep both.
newTagNameReader :: ReadM Tag
newTagNameReader = eitherReader $ \word ->
  if ' ' `elem` word
    then Left ("\"" <> word <> "\" has a space: the name of a tag is one word. Join its words with - or _, as in " <> joined word <> ".")
    else readTag (cs word)
  where
    joined = intercalate "-" . words
