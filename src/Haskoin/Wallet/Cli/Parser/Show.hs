{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of show, which takes one word and shows what it names:
-- an address, a transaction, a coin, a path or a file.
-- "Haskoin.Wallet.Commands.Show" runs it.
module Haskoin.Wallet.Cli.Parser.Show
  ( showCommand,
  )
where

import Control.Applicative (optional)
import Haskoin.Wallet.Cli.Command (Command (..), Shown (..), Target (..), readShown)
import Haskoin.Wallet.Cli.Completion (shownTargets)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Completion (completeFile)
import Haskoin.Wallet.Cli.Parser.Options (fileScopeOptions, networkOptionWith, typeOptionWith)
import Text.RawString.QQ (r)

-- | show, which shows one thing by its name: an address, a transaction or a
-- coin, or a file of a transaction, of a wallet or of a database
showCommand :: CommandSpec Command
showCommand =
  CommandSpec
    { name = "show",
      offline = False,
      summary = "Show an address, a transaction or a coin, or a file of a transaction, of a wallet or of a database",
      details =
        [r|
`show` shows one thing and changes nothing: an address, a transaction or a
coin of the wallet, or a file. The file can be a transaction file, a file of
a wallet or a database file. `show` tells what the word is by its shape:

  TYPE/0/i       the receiving address number i of the account of that
                 type, such as bip84/0/3
  TYPE/1/i       the change address number i of the account of that type
  ADDRESS        an address of the wallet, or a contact of the address book
  HASH           a transaction, by its txid, or its nosigHash while pending
  TXID:INDEX     a coin
  FILENAME       a file, any other word

Give a hash in full or by its short id, its first 8 digits. A file whose
name looks like one of the other words needs its directory before it, such
as ./a1075db5. With no word, `show` shows the newest receiving address of
the account, which is the first one that `addresses` lists. It gives out no
new address. --network and --type pick the account in the active portfolio,
as they do for `receive`.

`show` prints an address of the wallet the way `receive` prints a new one:
its path, its label and what it received. On a terminal, the QR code of the
address comes under them. Use it to give an
address out again. Any address of the wallet works, a change address too.
For an address of the address book, `show` prints the contact. It refuses
any other address, so that its QR code never pays somebody else. That
includes an address of the wallet that this computer has not generated, as
on the offline computer: give its path there.

A path names an address by its place in an account, such as bip84/0/3. It
starts with the type of its account, the way every view writes a path. A
path without the type, such as /0/3, is in the account that --type picks.
`show` refuses --type with a path that has its type. `show` derives the
address from the public key of the account. That needs no mnemonic and no
server, so it works on an offline computer as well as online.

Use a path on the offline computer to check an address that the online
computer gave out. An online computer that was tampered with can show an
address of its own in place of an address of the account, and the payer
would pay the attacker. The offline computer holds the same portfolio, since
newportfolio created it there, so it derives the same addresses. Run
`receive` on the online computer. Then run `show` on the offline computer
with the path that `receive` printed, such as bip84/0/3, and compare the two
addresses.

When the wallet holds the address at the path, in its database, `show`
prints it as any address of the wallet. When it does not, `show` prints the
address cut: its two ends, with a dot for each character of the middle, and
no QR code. This is the case on the offline computer, which gave no address
out. Compare the two ends with the address that the online computer shows.
The ends are enough, since nobody can make another address with the same
ends: it would take about 2^128 tries. `show` hides the middle so that
nobody pays the address from this screen. `receive` did not give it out
here, and it may lie past the gap, the run of unused addresses that a
restored wallet looks at. Look at the path too. A tampered online computer
picks the path it shows, and a path far past the addresses you gave out is a
sign of tampering.

`show` prints a transaction in full, as each portfolio that holds it sees
it. A payment between two portfolios is printed twice, once for each. `show`
prints a coin with every detail, as `coins` --verbose does.

Use `show` on a transaction file to check its transaction before you sign
it, or before you send it. `show` finds the portfolio of the transaction by
itself, as importtx does. It prints the transaction as that portfolio sees
it, with the name of the portfolio. It prints a transaction between two
portfolios as the portfolio that pays sees it. The wallet keeps nothing from
the file. A transaction file is a PSBT (BIP 174), which preparetx --output,
exporttx, signtx or preparesweep wrote. A transaction that involves no
portfolio, such as a sweep to addresses outside the wallet, is printed on
the network that its file names.

A transaction that is not sent yet, pending or in a file, is printed in full:
every address it spends from, with its amount, and every address it pays. You
may be about to sign it or to send it. A Spends field gives the total of the
coins it spends that are not the portfolio's, as a sweep does: check it
against the addresses it pays. A transaction that involves no portfolio is
External, and its first line gives the total it spends. Where the time goes,
"from a file" means that the wallet does not hold the transaction.

`show` reads a PSBT that another wallet wrote the same way: in bytes or in
base64 text, of version 0 or of version 2 (BIP 370). Such a file names no
network, so `show` reads it on the network of --network, or on the default
network.

The labels that `show` prints for a transaction file come from the file: the
label of the transaction, those of its addresses and those of the coins it
spends, as the wallet that wrote the file had them. They are not the labels
of this wallet. An offline computer has no labels of its own, so it shows
those of the online wallet this way.

A file of a wallet is the file that exportwallet writes and importwallet
reads. It holds BIP 329 records. `show` reads as a file of a wallet any file
that is not a database, a PSBT or the hexadecimal of a transaction. It
prints what importwallet would do with the file, and does not do it. First
comes
each wallet of the file with its portfolios. Each portfolio is either one
that importwallet would create, with its accounts, or one that the database
has already. Then comes each account, with the labels of its addresses and
transactions, and the coins it locks or unlocks. `show` puts a record under
the account that the origin and the network of the record name. The labels
that belong to no account of the file come after the accounts. Then come the
tags of the file that nothing has, under "Tags not used", each with its
colour and with whether it locks its coins, and last the records that hw
keeps no label for. `show` refuses a file that importwallet would refuse,
with the reason.

A database file is a copy of the database of hw: one that `backup` wrote, or
one of ~/.hw/auto-backups. `show` recognizes it by its first bytes, which
every SQLite database starts with. Run `show` on a copy before you restore
it with restoredatabase, to see which wallets and portfolios come back.
`show` lists each wallet of the copy, with the fingerprint of its seed and
its description. Under each wallet come its portfolios, each with the day it
was created and its description. A * marks the active portfolio of that
database. `show` leaves the file as it is. It does not print the history or
the balances of the copy: a sync brings them up to date once the copy is
restored. `show` refuses a database whose format version is not that of this
hw, and names its version.

The first line of the answer says which kind of file `show` read: "BIP 329
file" for a file of a wallet, "SQLite database" for a database. A backup has
one file of each kind, and both hold wallets and portfolios. `show` tells
the kind of a file from what it holds, never from its name.

The options that `show` takes depend on the word. With a path or with no
word, --network and --type pick the account, in the active portfolio. With
an address or a coin, `show` finds the account by itself, so it refuses
--type. --network is then needed only when the wallet has the address or the
coin on several networks, which can happen because the test networks share
their keys. With a transaction file that names no network, --network gives
the network. With a file of a wallet, --wallet and --portfolio show one
wallet or one portfolio of the file only, as they make importwallet read one
only. --wallet names a wallet of the file by its fingerprint. In a file of
several wallets, --portfolio needs --wallet with it. `show` refuses
--network with a file of a wallet, since each record of the file names its
network. It refuses --wallet and --portfolio with anything else. A database
file takes none of these options: `show` lists every wallet and portfolio
of it.
|],
      args =
        (\shown target fileScope -> CommandShow target fileScope shown)
          <$> optional shownArg
          <*> ( Target
                  <$> networkOptionWith "With a path or with nothing to show, the network of the account. With a transaction file that names no network, the network to read it on. With an address or a coin, the network to find it on, when the wallet has it on several. Without it, the default-network of the configuration file, or btc."
                  <*> typeOptionWith "With a path without its type, such as /0/3, or with nothing to show, the type of the account. Without it, the default-type of the configuration file, or bip44."
              )
          <*> fileScopeOptions
            "With a file of a wallet, show only this wallet of the file, as importwallet --wallet reads it."
            "With a file of a wallet, show only this portfolio of the file, as importwallet --portfolio reads it."
    }

-- | What show shows, read by its shape ('readShown'), with what the shell
-- offers for it ('shownCompletion')
shownArg :: Args Shown
shownArg =
  argument (eitherReader readShown) $
    metavar "PATH|ADDRESS|HASH|TXID:INDEX|FILENAME"
      <> completer (mkCompleter shownCompletion)
      <> help "What to show: a path such as bip84/0/3, an address of the wallet or of the address book, a transaction by its txid or nosigHash, a coin, or a file: a transaction file, a file of a wallet or a database file. Give a hash in full or by its short id, its first 8 digits. Without it, `show` shows the newest receiving address."

-- | What the shell offers for show. It offers the addresses, transactions
-- and coins of the wallet ('shownTargets'), and the files and directories,
-- as for a FILENAME. It leaves out a file whose name show reads as
-- something else, a file named a1075db5 for instance. show would take that
-- word for a short id. The file is offered once .\/ is typed before it.
-- Offering .\/a1075db5 for the word a1 would not do, since zsh drops a value
-- that does not start with the word typed.
shownCompletion :: String -> IO [String]
shownCompletion typed = do
  held <- shownTargets typed
  files <- completeFile typed
  return (held <> filter readAsFile files)
  where
    readAsFile name = readShown name == Right (ShownFile name)
