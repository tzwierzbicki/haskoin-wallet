{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the file of a wallet: exportwallet and
-- importwallet, each with its options and its help. The two share the text
-- that describes the file. "Haskoin.Wallet.Commands.WalletFile" runs them.
module Haskoin.Wallet.Cli.Parser.WalletFile
  ( exportWalletCommand,
    importWalletCommand,
  )
where

import Haskoin.Wallet.Cli.Command (Command (..))
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (fileArgument, fileScopeOptions, portfolioOptionWith, walletOptionWith)
import Text.RawString.QQ (r)

-- | exportwallet, which writes every wallet and its labels to a file
exportWalletCommand :: CommandSpec Command
exportWalletCommand =
  CommandSpec
    { name = "exportwallet",
      offline = False,
      summary = "Write every wallet and its labels to a file",
      details = importExportWalletDetails,
      args =
        CommandExportWallet
          <$> walletOptionWith "Write only this wallet. Without it, every wallet, or with --portfolio, the wallet of the active portfolio."
          <*> portfolioOptionWith "Write only this portfolio. It is a portfolio of the wallet that --wallet names, or else of the wallet of the active portfolio. Without it, every portfolio."
          <*> fileArgument "The file to write. It must not exist yet."
    }

-- | importwallet, which adds the portfolios and the labels of a file of
-- exportwallet to the database
importWalletCommand :: CommandSpec Command
importWalletCommand =
  CommandSpec
    { name = "importwallet",
      offline = False,
      summary = "Add the portfolios and labels of a file",
      details = importExportWalletDetails,
      args =
        CommandImportWallet
          <$> fileScopeOptions
            "Read only this wallet of the file. Without it, every wallet of the file."
            "Read only this portfolio. Without it, every portfolio. In a file of several wallets, name the wallet too, with --wallet."
          <*> fileArgument "The file exportwallet wrote"
    }

-- | The details that the helps of exportwallet and importwallet share: the
-- two commands are the two ends of one file
importExportWalletDetails :: String
importExportWalletDetails =
  [r|
exportwallet writes every portfolio of every wallet to a file, with its
labels and its tags. importwallet adds such a file to another database. The
file holds public keys and no private key, and neither command asks for the
mnemonic. The public keys still reveal every address and balance, so keep the
file private.

The file is in the format of BIP 329, which other wallets read and write. One
file holds the whole database, whichever portfolio is active. Each account
goes in as its public key, with its path, its network, its type and the
fingerprint of the mnemonic. Then come the labels of its receiving addresses
and of its transactions, and the tags of its addresses and of its
transactions. Each tag comes with its colour and with whether it locks its
coins. The coins locked with lockcoin go in too, and so do the descriptions
of the wallets and of their portfolios.

BIP 329 defines no tags. hw writes them under three keys of its own, tags,
tagcolors and taglocks, which other wallets ignore. A tag that nothing has is
written too, on the first key of the file, since no address names it. Only a
file of the whole database has such tags.

hw also writes this file by itself to ~/.hw/auto-backups after each command
that changes the labels. It keeps the recent copies, and fewer of the older
ones. backup writes the file to ~/.hw/user-backups, beside its copy of the
database.

  hw exportwallet all.jsonl                           every wallet
  hw exportwallet one.jsonl --wallet 1                every portfolio of wallet1
  hw exportwallet one.jsonl --portfolio 2             portfolio2 of the wallet
                                                      of the active portfolio
  hw exportwallet one.jsonl --wallet 1 --portfolio 2  portfolio2 of wallet1

A file of several wallets shows its reader that one person holds them all.
Write a wallet alone with --wallet when the file must not show the others.

importwallet reads such a file. It finds each wallet by the fingerprint the
file gives, and each portfolio by its index. The file does not say which
wallet is wallet0. A wallet of the file may have another index in the
database, and its labels still go to it. When the database does not have a
portfolio of the file, importwallet creates it whole, with the twelve
accounts of the file. The database can then watch the portfolio and prepare
its payments without the mnemonic. This is how an online computer works with
an offline signer. --wallet reads one wallet of the file only, and
--portfolio one portfolio only.

In a database without any portfolio, the first portfolio importwallet creates
becomes the active one, where the commands work. In a database that has an
active portfolio, that portfolio stays active, and importwallet says how to
switch to a portfolio it created.

A portfolio the database has already stays as it is. importwallet never
makes a second portfolio with the same index, and adds no account to one: a
portfolio has every account from the start. The keys of the file must be the
ones the portfolio holds. Otherwise importwallet refuses and changes nothing,
since the file and the database then come from different mnemonics.

importwallet also refuses three other files, and changes nothing. The first
is a file that would create a portfolio without all twelve of its accounts.
The second is a file with a key that another account of the database holds,
whatever index the file writes it at. The third is a file with two keys for
accounts of one path: the test networks share the path m/44'/1'/0', so their
bip44 accounts hold one key.

importwallet cannot prove that the keys of a file are those of your mnemonic:
only the mnemonic could. A file that names your fingerprint may still carry
the keys of someone else, and the addresses that receive gives out would then
be theirs. Import only a file you wrote with exportwallet on your own signer,
or one that reached you over a channel you trust.

importwallet imports whole wallets. A key alone, such as an xpub or an output
descriptor from another wallet, is one account, and it does not say which
portfolio it belongs to. importwallet refuses a file that holds such a key
and no record. In a BIP 329 file of another wallet, it leaves an xpub record
aside, and the answer says why.

With --wallet and --portfolio, importwallet reads only a part of the file:
the keys and the labels of that part. --wallet names one wallet of the file
by its fingerprint. --portfolio names one portfolio by its index.

  hw importwallet all.jsonl --wallet bc560f95                one wallet
  hw importwallet all.jsonl --wallet bc560f95 --portfolio 1  one portfolio of it
  hw importwallet one.jsonl --portfolio 1                    in a file of one
                                                             wallet

In a file of one wallet, --portfolio is enough. In a file of several wallets,
--portfolio alone is refused. It would name a portfolio of each wallet, so
importwallet asks for --wallet too, and lists the fingerprints of the file.

A wallet of a file is named by its fingerprint, and not by an index as on the
other commands. The file holds no index, and wallet0 on the computer that
wrote it may be wallet1 here. `show` lists the wallets of a file with their
fingerprints.

importwallet does not read the records of the rest of the file. The answer
counts them.

A wallet always has portfolio0. So importwallet creates a wallet that the
database does not have only from a file that holds its portfolio0, and it
refuses --portfolio 1 for such a wallet. Import the whole wallet instead.

importwallet sets the labels and the locks of the file on the addresses,
transactions and coins the wallet has. When the wallet has not derived an
address yet, importwallet derives it from the path the file gives, up to a
thousand addresses past the last used address of its chain. When `sync` has
not fetched a transaction yet, importwallet labels it at once if the origin
of its record says which portfolio it belongs to. A transaction record
without an origin labels the transaction in the portfolios that have it
already, on its network. The answer counts what hw cannot apply: run `sync`,
then importwallet again. importwallet leaves aside the labels of inputs, of
coins and of change addresses, since hw keeps none. A BIP 329 file of another
wallet sets its labels the same way.

importwallet adds the tags of the file to the tags that its addresses and
its transactions have already. It removes no tag. A tag that has no colour
takes the colour the file gives it, and one that has a colour keeps it. An
address takes the tags of the file at once, also a change address or a far
address that the wallet has not generated yet. When a `sync` finds that
address, its coins are under the tag from the start, and locked when the tag
locks its coins.

`show` FILE shows what importwallet would add from a file, and changes
nothing.

The fingerprint of a wallet identifies its mnemonic. It is the four bytes
that PSBTs and output descriptors write before a key of the mnemonic. Each
record of the file names its account with it, as in
pkh([bc560f95/44'/0'/0']) or wpkh([bc560f95/84'/0'/0']).
|]
