{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the copies of the database: backup, showbackups and
-- restoredatabase, each with its options and its help.
-- "Haskoin.Wallet.Commands.Backup" runs them.
module Haskoin.Wallet.Cli.Parser.Backup
  ( backupCommand,
    showBackupsCommand,
    restoreDatabaseCommand,
  )
where

import Control.Applicative (optional)
import Haskoin.Wallet.Cli.Command (Command (..))
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (fileArgument, yesOption)
import Text.RawString.QQ (r)

{- backup -}

-- | backup, which copies the wallet database to a backup file
backupCommand :: CommandSpec Command
backupCommand =
  CommandSpec
    { name = "backup",
      offline = False,
      summary = "Copy the wallet database to a backup file",
      details =
        [r|
`backup` copies the whole database to a file: wallets, portfolios, addresses,
labels, tags, coins, transactions and pending transactions. The copy holds no
private key.

Without a FILENAME, `backup` writes two files into ~/.hw/user-backups: the
copy of the database, and a file of its labels, which is the file
exportwallet writes. Both names give the time in UTC, and how many wallets
and portfolios the database holds. The name of the copy also gives the format
version of the database. hw never removes these files. With a FILENAME,
`backup` writes the copy of the database only.

`show` FILENAME lists the wallets and the portfolios of a copy, and does not
change it. Run it before you restore a backup, to see what comes back. Its
first line reads "SQLite database" for a copy of the database, and "BIP 329
file" for a file of labels, so that you can tell the two files of a backup
apart.

restoredatabase replaces the database with a copy. To restore the file of the
labels, give it to importwallet instead, which reads it into a database of
any version.

hw also copies the database and the labels into ~/.hw/auto-backups by itself.
It copies the database after each command that adds a wallet or a portfolio,
and the labels after each command that changes them. It copies both before
deletewallet, deleteportfolio and restoredatabase, and makes no copy after
these three. For each kind, it keeps the ten newest copies. It also keeps the
last copy of each hour for a day, of each day for a week, of each week for a
month, and of each month for a year. hw removes the other copies it made
there, and no other file. showbackups tells how many copies there are, and
how old the newest is. In config.toml, the keys autobackup-database and
autobackup-labels turn these copies off.
|],
      args =
        CommandBackup
          <$> optional (fileArgument "The file to copy the database to. It must not exist yet. Without it, the copy and its labels go to ~/.hw/user-backups.")
    }

{- showbackups -}

-- | showbackups, which shows the copies hw keeps. A copy that hw could
-- not make is a warning on stderr when it happens, which a script may
-- never show. Here the user sees later that the newest copy is older
-- than it should be. It also names the files, for the user who looks for
-- one to give to restoredatabase.
showBackupsCommand :: CommandSpec Command
showBackupsCommand =
  CommandSpec
    { name = "showbackups",
      offline = False,
      summary = "Show the backups hw keeps, and how old the newest are",
      details =
        [r|
showbackups tells what the two directories of backups hold, and changes
nothing:

  ~/.hw/auto-backups   the copies hw makes by itself, of the database and of
                       the labels
  ~/.hw/user-backups   the copies `backup` wrote

In each directory, and for each kind of copy, the database and the labels,
it gives the number of copies, the name of the newest one, and how long ago
hw made it. It also says when the key autobackup-database or
autobackup-labels of config.toml turns a kind off.

showbackups runs when the database is damaged too, since it reads the two
directories alone. restoredatabase then puts the newest copy back.

Run it to check that hw makes its copies. hw prints nothing when it makes a
copy, and warns on stderr when it cannot make one. When the newest copy is
older than your last change, hw missed a copy. Run `backup` to make one now.
A deletion and a restore are the exception: hw makes their copy right before
them, and none after.

`show` FILENAME lists the wallets and the portfolios of a copy of the
database, and restoredatabase FILENAME restores it.
|],
      args = pure CommandShowBackups
    }

{- restoredatabase -}

-- | restoredatabase, which puts a copy of the database back in place of
-- the database. Its name says what is restored. A backup has two files,
-- and the file of the labels goes to importwallet instead.
restoreDatabaseCommand :: CommandSpec Command
restoreDatabaseCommand =
  CommandSpec
    { name = "restoredatabase",
      offline = False,
      summary = "Put a copy of the database back in place of the database",
      details =
        [r|
restoredatabase replaces the whole database with a copy of it. Everything the
copy holds comes back: the wallets, their portfolios, the addresses, the
labels, the tags, the coins, the transactions and the pending transactions.
The copy is a file that `backup` wrote, or a .sqlite file of
~/.hw/auto-backups or ~/.hw/user-backups. showbackups names the newest copy
in each directory. Before you restore a copy, `show` FILENAME lists the
wallets and the portfolios it holds.

Everything that the database holds and the copy does not is lost, such as a
wallet you created after you made the copy. So restoredatabase asks first,
and says what the database and the copy hold. It asks nothing when the
database holds no wallet. --yes answers yes in advance, for scripts.

Before it replaces the database, hw copies it and its labels into
~/.hw/auto-backups, with the copies it makes after each change. The answer
names the two files. So if you restore the wrong copy, run restoredatabase
with that backup to undo it. hw makes no such backup of a database without a
wallet, or when the key autobackup-database of config.toml is false. hw
makes no copy after the restore: the file you restored from holds what the
database holds now.

A damaged database, a file that SQLite cannot read, is replaced too. hw
cannot read what it holds, so it asks nothing and copies nothing. It moves
the file into ~/.hw/broken-databases first, under a name with the time, and
the answer says where it is. hw never removes a file there.

hw restores only a copy that has the format version of this hw, which
hw --version shows. It refuses a copy of an earlier hw, and one of a newer
hw. hw does not change the copy.

The copy holds the history as its last sync left it. Run sync afterwards.
The settings in ~/.hw/config.toml are not part of the database, and stay as
they are.

hw can only restore into a database it can open. When hw refuses the
database in ~/.hw, move ~/.hw/portfolios.sqlite aside first, with the files
portfolios.sqlite-wal and portfolios.sqlite-shm if they are there.
|],
      args =
        CommandRestoreDatabase
          <$> fileArgument "The copy of the database to restore. hw does not change it."
          <*> yesOption "Replace the database without asking first"
    }
