{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the defaults: setdefault, which writes a key of the
-- configuration file, and showdefault, which shows what setdefault writes.
-- "Haskoin.Wallet.Commands.Defaults" runs them.
module Haskoin.Wallet.Cli.Parser.Defaults
  ( setDefaultCommand,
    showDefaultCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.List (intercalate)
import Haskoin
import Haskoin.Wallet.Cli.Command (Command (..), SetDefault (..), ShowDefault (..))
import Haskoin.Wallet.Cli.Completion (hiddenNetworkNames, shownNetworkNames)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (everyNetwork, networkOptionWith, orderFlags, readNetwork, typeOptionWith, viewFlags)
import Haskoin.Wallet.Keys (portfolioNetworks)
import Haskoin.Wallet.Testnet3 (isDeprecatedNetwork)
import Text.RawString.QQ (r)

{- setdefault -}

-- | setdefault, which sets the defaults of the commands and the networks
-- shown
setDefaultCommand :: CommandSpec Command
setDefaultCommand =
  CommandSpec
    { name = "setdefault",
      offline = False,
      summary = "Set the defaults of the commands, and the networks shown",
      details =
        [r|
setdefault writes defaults to the configuration file, ~/.hw/config.toml, so
that you do not have to edit it. Give it each option with the value you want
as the default, as you would give the option to a command:

  hw setdefault --type bip84        give out native SegWit addresses
  hw setdefault --network bch       work on Bitcoin Cash
  hw setdefault --unit sat          show and read amounts in satoshis
  hw setdefault --tabular           show the listings as tables
  hw setdefault --reverse           list the oldest transaction first

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

setdefault also sets which networks hw shows. A portfolio has accounts on
eight networks. Unless you chose others in the setup, hw shows btc and bch,
and hides the six test networks:

  hw setdefault --show-network btctest4     show a hidden network
  hw setdefault --hide-network bch          hide a network

hw leaves a hidden network out of every answer, and no command can name it.
A hidden network is not disabled: sync and recover still work on it, so the
wallet is up to date on it the day you show it. You can give each of the two
options several times. The default network is always shown. To hide it, name
another default network with --network in the same command.

setdefault also says whether hw keeps a journal of your transactions for
ledger, the command-line accounting program:

  hw setdefault --journal           keep ~/.hw/portfolios.ledger up to date
  hw setdefault --no-journal        stop, and remove that file

With --journal, hw writes the journal to ~/.hw/portfolios.ledger at once. It
is the journal that exportjournal writes. hw then writes the file again after
each command that changes the journal, such as sync, label, tag and
newcontact. Include the file in a journal of your own, and never edit it. hw
leaves a journal that did not change as it is. hw keeps no journal unless you
ask for one.

setdefault writes the keys default-network, default-type, unit, listing-view,
listing-reverse, shown-networks and journal. It leaves the other keys of the
file as they are. showdefault shows the defaults as they are now. The
portfolio that the commands work in is not a default of the file. It is the
active portfolio, which the database keeps and switch changes.
|],
      args =
        CommandSetDefault
          <$> ( SetDefault
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
              )
    }

-- | The sentence of the help of --show-network on the networks that hw
-- still works on and no longer recommends ('isDeprecatedNetwork'). It
-- stands where the user picks a network to show, as in the setup.
deprecatedNetworks :: String
deprecatedNetworks = intercalate " and " names <> (if length names == 1 then " is deprecated." else " are deprecated.")
  where
    names = [net.name | net <- portfolioNetworks, isDeprecatedNetwork net]

-- | A network setdefault shows or hides, by the long name of its option.
-- The shell offers the networks the option changes something for: the
-- hidden ones to show, and the shown ones to hide.
shownNetworkOption :: String -> IO [String] -> String -> Args Network
shownNetworkOption name offered said =
  option (eitherReader readNetwork) $
    long name
      <> metavar "NETWORK"
      <> completer (listIOCompleter offered)
      <> help said

{- showdefault -}

-- | showdefault, the twin of setdefault: it shows what setdefault writes,
-- and the active portfolio, which switch changes. The two together say
-- where a command without options works, and how it answers.
--
-- Its switches take the names of the options of setdefault, without a
-- value. --unit is also the --unit UNIT of every command. After
-- showdefault, the parser takes --unit for the switch of showdefault, since
-- the options of the command come before those of every command
-- (@programInfo@ of "Haskoin.Wallet.Cli.Parser.Run"). So hw showdefault
-- --unit shows the unit, and hw showdefault --unit sat is refused:
-- showdefault shows no amount for a unit to change.
showDefaultCommand :: CommandSpec Command
showDefaultCommand =
  CommandSpec
    { name = "showdefault",
      offline = False,
      summary = "Show the defaults of the commands, and the active portfolio",
      details =
        [r|
showdefault shows the defaults that setdefault writes, and the active
portfolio. It shows the defaults as hw reads them from the configuration
file, ~/.hw/config.toml. The active portfolio is the portfolio that the
commands work in. Give the option of each value you want to see, or no option
to see them all:

  hw showdefault                  every default, and the active portfolio
  hw showdefault --network        the default network
  hw showdefault --type --unit    the default type and the unit
  hw showdefault --portfolio      the active portfolio, with its wallet
  hw showdefault --wallet         the wallet of the active portfolio

--network, --type and --unit are the options of setdefault, without a value.
--view shows the view of the listings, which setdefault sets with --compact,
--verbose or --tabular. --reverse shows whether the listings list their rows
in the reverse order, which setdefault sets with --reverse or --no-reverse.
--shown-networks shows the networks that hw shows. --journal shows whether hw
keeps a journal for ledger, which setdefault sets with --journal or
--no-journal. For a key that the file does not have, showdefault shows the
value that hw uses without it.

The active portfolio is not a key of the file. The database keeps it, and
switch changes it.
|],
      args =
        CommandShowDefault
          <$> ( ShowDefault
                  <$> switch (long "network" <> help "The default network: the key default-network.")
                  <*> switch (long "type" <> help "The default type of address: the key default-type.")
                  <*> switch (long "unit" <> help "The unit of the amounts: the key unit. After showdefault, --unit takes no value.")
                  <*> switch (long "view" <> help "The view of the listings: the key listing-view.")
                  <*> switch (long "reverse" <> help "Whether the listings list their rows in the reverse order: the key listing-reverse.")
                  <*> switch (long "shown-networks" <> help "The networks that hw shows: the key shown-networks.")
                  <*> switch (long "journal" <> help "Whether hw keeps a journal for ledger in ~/.hw/portfolios.ledger: the key journal.")
                  <*> switch (long "portfolio" <> help "The active portfolio, with its wallet.")
                  <*> switch (long "wallet" <> help "The wallet of the active portfolio.")
              )
    }
