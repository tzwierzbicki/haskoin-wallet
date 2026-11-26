{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the settings: setconfig, which writes a key of the
-- configuration file for each option, or asks about every setting when it
-- is given none, and showconfig, which shows the settings.
-- "Haskoin.Wallet.Commands.Config" runs them.
module Haskoin.Wallet.Cli.Parser.Config
  ( setConfigCommand,
    showConfigCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.List (intercalate)
import Haskoin
import Haskoin.Wallet.Cli.Command (Command (..), SetConfig (..), ShowConfig (..))
import Haskoin.Wallet.Cli.Completion (hiddenNetworkNames, shownNetworkNames)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (everyNetwork, networkOptionWith, orderFlags, readNetwork, typeOptionWith, viewFlags)
import Haskoin.Wallet.Keys (portfolioNetworks)
import Haskoin.Wallet.Testnet3 (isDeprecatedNetwork)
import Text.RawString.QQ (r)

{- setconfig -}

-- | setconfig, which changes the settings of hw: by a question for each,
-- or by the options that name some of them
setConfigCommand :: CommandSpec Command
setConfigCommand =
  CommandSpec
    { name = "setconfig",
      offline = False,
      summary = "Change the settings of hw, by questions or by options",
      details =
        [r|
setconfig writes settings to the configuration file, ~/.hw/config.toml, so
that you do not have to edit it. Without an option, it asks about each of the
twelve settings of hw, as the setup of the first run does:

  hw setconfig                     answer a question for each setting

Each question explains its setting and starts on the value that hw has now, so
Enter keeps it. hw writes the answers that differ after the last question.
Escape cancels, and nothing is written. Five settings have a question and no
option: the two automatic backups, the address of the server, the number of
requests at once and the gap.

Seven settings are the default of an option, or have options of their own.
In a script, or to change one of them, give setconfig each option with the
value you want as the default, as you would give the option to a command:

  hw setconfig --type bip84        give out native SegWit addresses
  hw setconfig --network bch       work on Bitcoin Cash
  hw setconfig --unit sat          show and read amounts in satoshis
  hw setconfig --tabular           show the listings as tables
  hw setconfig --reverse           list the oldest transaction first

--unit sets the unit of the amounts that hw shows, and of the amounts that
you type without a unit. This help does not list it: hw --help lists it with
the options of every command. --compact, --verbose and --tabular set the view
of the listings: portfolios, addresses, balances, txs, pendingtxs and coins.
A view option given to a listing still sets the view of that one listing.

--reverse makes addresses, balances, coins, txs and pendingtxs list their
rows in the reverse order on the screen. It does for every listing what
--reverse does for one. --no-reverse brings back the usual order. Given to a
listing, it brings back the usual order for that one listing. With --json or
--csv, a listing keeps the usual order unless you give it --reverse. So a
script reads the same rows whatever the configuration file says.

setconfig also sets which networks hw shows. A portfolio has accounts on
eight networks. Unless you chose others in the setup, hw shows btc and bch,
and hides the six test networks:

  hw setconfig --show-network btctest4     show a hidden network
  hw setconfig --hide-network bch          hide a network

hw leaves a hidden network out of every answer, and no command can name it.
A hidden network is not disabled: sync and recover still work on it, so the
wallet is up to date on it the day you show it. You can give each of the two
options several times. The default network is always shown. To hide it, name
another default network with --network in the same command.

setconfig also says whether hw keeps a journal of your transactions for
ledger, the command-line accounting program:

  hw setconfig --journal           keep ~/.hw/portfolios.ledger up to date
  hw setconfig --no-journal        stop, and remove that file

With --journal, hw writes the journal to ~/.hw/portfolios.ledger at once. It
is the journal that exportjournal writes. hw then writes the file again after
each command that changes the journal, such as sync, label, tag and
newcontact. Include the file in a journal of your own, and never edit it. hw
leaves a journal that did not change as it is. hw keeps no journal unless you
ask for one.

The options write the keys default-network, default-type, unit, listing-view,
listing-reverse, shown-networks and journal. The questions also write
autobackup-database, autobackup-labels, host, parallel-requests and gap.
setconfig leaves the rest of the file as it is, your comments too. You write
the table journal-accounts in the file yourself. showconfig shows the
settings as they are now. The portfolio that the commands work in is not a
setting of the file. It is the active portfolio, which the database keeps and
switch changes.
|],
      args =
        CommandSetConfig
          <$> ( SetConfig
                  <$> networkOptionWith "It becomes the default network. It may be a hidden network when --show-network shows it in the same command."
                  <*> typeOptionWith "It becomes the default type."
                  -- --unit is read with the options of every command
                  -- ('withUnitToSet')
                  <*> pure Nothing
                  <*> optional
                    ( viewFlags
                        "The listings show each element in a few lines, as they do when no view is set."
                        "The listings show every detail of each element."
                        "The listings show each element on one row of a table."
                    )
                  <*> orderFlags
                    "The listings list their rows in the reverse order on the screen, the oldest transaction first."
                    "The listings list their rows in the usual order, as they do when no order is set."
                  <*> many (shownNetworkOption "show-network" hiddenNetworkNames ("A network to show from now on: " <> everyNetwork <> ". " <> deprecatedNetworks <> " Repeat the option for several networks."))
                  <*> many (shownNetworkOption "hide-network" shownNetworkNames "A network to hide from now on. sync and recover still work on it. Repeat the option for several networks.")
                  <*> optional
                    ( flag' True (long "journal" <> help "Keep a journal of the transactions for ledger in ~/.hw/portfolios.ledger, and update it after each command that changes it.")
                        <|> flag' False (long "no-journal" <> help "Keep no journal from now on, and remove ~/.hw/portfolios.ledger.")
                    )
                  -- The five settings that the questions of setconfig
                  -- set, and no option: the two automatic backups, the
                  -- server, the requests at once and the gap
                  <*> pure Nothing
                  <*> pure Nothing
                  <*> pure Nothing
                  <*> pure Nothing
                  <*> pure Nothing
              )
    }

-- | The sentence of the help of --show-network on the networks that hw
-- still works on and no longer recommends ('isDeprecatedNetwork'). It
-- stands where the user picks a network to show, as in the setup.
deprecatedNetworks :: String
deprecatedNetworks = intercalate " and " names <> (if length names == 1 then " is deprecated." else " are deprecated.")
  where
    names = [net.name | net <- portfolioNetworks, isDeprecatedNetwork net]

-- | A network setconfig shows or hides, by the long name of its option.
-- The shell offers the networks the option changes something for: the
-- hidden ones to show, and the shown ones to hide.
shownNetworkOption :: String -> IO [String] -> String -> Args Network
shownNetworkOption name offered said =
  option (eitherReader readNetwork) $
    long name
      <> metavar "NETWORK"
      <> completer (listIOCompleter offered)
      <> help said

{- showconfig -}

-- | showconfig, the twin of setconfig: it shows what setconfig writes,
-- and the active portfolio, which switch changes. The two together say
-- where a command without options works, and how it answers.
--
-- Without a switch it shows the twelve settings setconfig asks about. Its
-- switches take the names of the options of setconfig, without a value,
-- so only the settings that have an option have a switch.
--
-- --unit is also the --unit UNIT of every command. After showconfig, the
-- parser takes --unit for the switch of showconfig, since the options of
-- the command come before those of every command (@programInfo@ of
-- "Haskoin.Wallet.Cli.Parser.Run"). So hw showconfig --unit shows the
-- unit, and hw showconfig --unit sat is refused: showconfig shows no
-- amount for a unit to change.
showConfigCommand :: CommandSpec Command
showConfigCommand =
  CommandSpec
    { name = "showconfig",
      offline = False,
      summary = "Show the settings of hw, and the active portfolio",
      details =
        [r|
showconfig shows the settings that setconfig writes, and the active
portfolio. It shows the settings as hw reads them from the configuration
file, ~/.hw/config.toml. The active portfolio is the portfolio that the
commands work in. Give the option of each value you want to see, or no option
to see the twelve settings and the active portfolio:

  hw showconfig                  every setting, and the active portfolio
  hw showconfig --network        the default network
  hw showconfig --type --unit    the default type and the unit
  hw showconfig --portfolio      the active portfolio, with its wallet
  hw showconfig --wallet         the wallet of the active portfolio

--network, --type and --unit are the options of setconfig, without a value.
--view shows the view of the listings, which setconfig sets with --compact,
--verbose or --tabular. --reverse shows whether the listings list their rows
in the reverse order, which setconfig sets with --reverse or --no-reverse.
--shown-networks shows the networks that hw shows. --journal shows whether hw
keeps a journal for ledger, which setconfig sets with --journal or
--no-journal. The five settings that setconfig sets by a question alone have
no option here either: showconfig without an option shows them with the
others. For a key that the file does not have, showconfig shows the value
that hw uses without it.

The active portfolio is not a key of the file. The database keeps it, and
switch changes it.
|],
      args =
        CommandShowConfig
          <$> ( ShowConfig
                  <$> switch (long "network" <> help "The default network: the key default-network.")
                  <*> switch (long "type" <> help "The default type of address: the key default-type.")
                  <*> switch (long "unit" <> help "The unit of the amounts: the key unit. After showconfig, --unit takes no value.")
                  <*> switch (long "view" <> help "The view of the listings: the key listing-view.")
                  <*> switch (long "reverse" <> help "Whether the listings list their rows in the reverse order: the key listing-reverse.")
                  <*> switch (long "shown-networks" <> help "The networks that hw shows: the key shown-networks.")
                  <*> switch (long "journal" <> help "Whether hw keeps a journal for ledger in ~/.hw/portfolios.ledger: the key journal.")
                  <*> switch (long "portfolio" <> help "The active portfolio, with its wallet.")
                  <*> switch (long "wallet" <> help "The wallet of the active portfolio.")
              )
    }
