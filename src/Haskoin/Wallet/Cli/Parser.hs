{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of hw: its commands, each with its options and its
-- help, and the options that go with any command. A command is a
-- t'CommandSpec'. Each of its options is made once, as the row of the help
-- that describes it and the parser that reads it, with the builders of
-- "Haskoin.Wallet.Cli.Parser.Args". "Haskoin.Wallet.Cli.Parser.Run" runs the
-- command line, and "Haskoin.Wallet.Cli.Parser.Help" lays out the help.
--
-- This module is the way in. 'program' lists every command, in the groups
-- of the help, and 'parserMain' reads the command line into a 'Command'
-- and the t'GlobalOptions'. The types a command line is read into are in
-- "Haskoin.Wallet.Cli.Command", which this module exports again.
--
-- The commands are in the modules below this one. Each has the name of the
-- module of "Haskoin.Wallet.Commands" that runs its commands:
--
-- * "Haskoin.Wallet.Cli.Parser.Portfolio": newportfolio, recover,
--   checkmnemonic, mnemonicsheet, portfolios, switch, deleteportfolio and
--   deletewallet
-- * "Haskoin.Wallet.Cli.Parser.WalletFile": exportwallet and importwallet
-- * "Haskoin.Wallet.Cli.Parser.Defaults": setdefault and showdefault
-- * "Haskoin.Wallet.Cli.Parser.Address": receive, addresses and balances
-- * "Haskoin.Wallet.Cli.Parser.Show": show
-- * "Haskoin.Wallet.Cli.Parser.Label": label
-- * "Haskoin.Wallet.Cli.Parser.Tag": tag, untag, tags, renametag and
--   deletetag
-- * "Haskoin.Wallet.Cli.Parser.Contact": newcontact, contacts and
--   deletecontact
-- * "Haskoin.Wallet.Cli.Parser.Coin": coins, lockcoin and unlockcoin
-- * "Haskoin.Wallet.Cli.Parser.Transaction": txs, pendingtxs, exporttx,
--   importtx and deletetx
-- * "Haskoin.Wallet.Cli.Parser.Payment": preparetx, preparesendall and fees
-- * "Haskoin.Wallet.Cli.Parser.Signing": signtx
-- * "Haskoin.Wallet.Cli.Parser.Send": send, sendall and broadcast
-- * "Haskoin.Wallet.Cli.Parser.BumpFee": bumpfee
-- * "Haskoin.Wallet.Cli.Parser.Sweep": sweep, preparesweep and signsweep
-- * "Haskoin.Wallet.Cli.Parser.Sync": sync
-- * "Haskoin.Wallet.Cli.Parser.MessageSigning": signmsg and verifymsg
-- * "Haskoin.Wallet.Cli.Parser.Backup": backup, showbackups and
--   restoredatabase
-- * "Haskoin.Wallet.Cli.Parser.Export": exportsheet
-- * "Haskoin.Wallet.Cli.Parser.Journal": exportjournal
-- * "Haskoin.Wallet.Cli.Parser.Report": report
--
-- The options that the commands of several of these modules take are in
-- "Haskoin.Wallet.Cli.Parser.Options".
module Haskoin.Wallet.Cli.Parser
  ( -- * The command line
    GlobalOptions (..),
    parserMain,
    program,

    -- * What a command line is read into
    module Haskoin.Wallet.Cli.Command,
  )
where

import Control.Applicative (optional)
import Data.List (intercalate)
import Data.String.Conversions (cs)
import Data.Version (showVersion)
import Haskoin.Wallet.Amounts (AmountUnit, readUnit)
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Parser.Address (addressesCommand, balancesCommand, receiveCommand)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Backup (backupCommand, restoreDatabaseCommand, showBackupsCommand)
import Haskoin.Wallet.Cli.Parser.BumpFee (bumpFeeCommand)
import Haskoin.Wallet.Cli.Parser.Coin (coinsCommand, lockCoinCommand, unlockCoinCommand)
import Haskoin.Wallet.Cli.Parser.Contact (contactsCommand, deleteContactCommand, newContactCommand)
import Haskoin.Wallet.Cli.Parser.Defaults (setDefaultCommand, showDefaultCommand)
import Haskoin.Wallet.Cli.Parser.Export (exportSheetCommand)
import Haskoin.Wallet.Cli.Parser.Journal (exportJournalCommand)
import Haskoin.Wallet.Cli.Parser.Label (labelCommand)
import Haskoin.Wallet.Cli.Parser.MessageSigning (signMsgCommand, verifyMsgCommand)
import Haskoin.Wallet.Cli.Parser.Payment (feesCommand, prepareSendAllCommand, prepareTxCommand)
import Haskoin.Wallet.Cli.Parser.Portfolio (checkMnemonicCommand, deletePortfolioCommand, deleteWalletCommand, mnemonicSheetCommand, newPortfolioCommand, portfoliosCommand, recoverCommand, switchCommand)
import Haskoin.Wallet.Cli.Parser.Report (reportCommand)
import Haskoin.Wallet.Cli.Parser.Run (runProgram)
import Haskoin.Wallet.Cli.Parser.Send (broadcastCommand, sendAllCommand, sendCommand)
import Haskoin.Wallet.Cli.Parser.Show (showCommand)
import Haskoin.Wallet.Cli.Parser.Signing (signTxCommand)
import Haskoin.Wallet.Cli.Parser.Sweep (prepareSweepCommand, signSweepCommand, sweepCommand)
import Haskoin.Wallet.Cli.Parser.Sync (syncCommand)
import Haskoin.Wallet.Cli.Parser.Tag (deleteTagCommand, renameTagCommand, tagCommand, tagsCommand, untagCommand)
import Haskoin.Wallet.Cli.Parser.Transaction (deleteTxCommand, exportTxCommand, importTxCommand, pendingTxsCommand, txsCommand)
import Haskoin.Wallet.Cli.Parser.WalletFile (exportWalletCommand, importWalletCommand)
import Haskoin.Wallet.Database.Migration (latestDatabaseVersion)
import qualified Paths_haskoin_wallet as Paths
import Text.RawString.QQ (r)

-- | The options that go with any command
data GlobalOptions = GlobalOptions
  { -- | The unit of the amounts shown and typed, when --unit is given.
    -- Without it, the unit is that of the configuration file, which is
    -- read after the command line.
    globalUnit :: !(Maybe AmountUnit),
    -- | Answer in JSON
    globalJson :: !Bool,
    -- | Print a listing as it is, through no pager. A help obeys the
    -- same option, but it is shown before the options are parsed
    -- ("Haskoin.Wallet.Cli.Parser.Run").
    globalNoPager :: !Bool
  }
  deriving (Eq, Show)

-- | The command typed, with the options that go with any command. A
-- request for help, and a command line that cannot be parsed, print what
-- they have to and leave.
parserMain :: IO (Command, GlobalOptions)
parserMain = withUnitToSet <$> runProgram program

-- | setdefault writes the unit that --unit names. --unit is one of the
-- options of every command, which the parser of a command does not see. A
-- second --unit of setdefault alone would clash with it. So the unit is
-- handed to setdefault here, after both are parsed. Every other command
-- keeps the options as they are.
withUnitToSet :: (Command, GlobalOptions) -> (Command, GlobalOptions)
withUnitToSet (CommandSetDefault opts, globals) =
  (CommandSetDefault opts {unit = globals.globalUnit}, globals)
withUnitToSet parsed = parsed

-- | hw: its commands by group, the options every command takes, and the
-- help of the program
program :: Program Command GlobalOptions
program =
  Program
    { name = "hw",
      -- The database has the format of this hw: every command refuses one
      -- of another version first
      version =
        intercalate
          "\n"
          [ "Software version: " <> showVersion Paths.version,
            "Database version: " <> show latestDatabaseVersion
          ],
      commandUsage = "COMMAND",
      -- The help adds which commands to run on an offline computer
      intro =
        [r|
hw is a BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash. It stores
public keys only.
|],
      everydayNames = ["newportfolio", "recover", "portfolios", "sync", "receive", "show", "addresses", "balances", "label", "tag", "tags", "contacts", "coins", "fees", "send", "sweep", "backup"],
      details =
        [r|
To start, run hw alone, without a command. It asks four questions: which
networks to show, the default network, the default type of address, and
whether to keep a journal for ledger. It writes ~/.hw/config.toml from your
answers. hw refuses every command until that file is there. Then create a
wallet and receive a payment:

  hw newportfolio                make a mnemonic and its wallet
  hw receive                     get an address to give to the payer
  hw sync                        update the portfolios from the network

A payment takes three steps:

  hw preparetx ADDRESS:AMOUNT    prepare it, without any key
  hw signtx NOSIGHASH            sign it with the mnemonic
  hw broadcast TXID              send it to the network

preparetx shows the nosigHash of the new transaction. A nosigHash is a hash
of the transaction without its signatures. It names the transaction until it
is signed. signtx shows its txid, the id the network knows it by.

`send` does the three steps in one command, on an online computer where you
type the mnemonic. sendall does the same for a payment of every coin:

  hw send ADDRESS:AMOUNT         prepare it, sign it and send it
  hw sendall ADDRESS             the same, for all the coins

To sign on an offline computer, create the portfolio there with
`newportfolio`. Copy it to the online computer with exportwallet and
importwallet. Then carry each transaction between the two computers with
preparetx --output, signtx --input and broadcast --input.

`recover` and preparesweep --key-file take a mnemonic or private keys too, but
they need the server, so they run on an online computer. Their help says how to
keep the secret offline.

A command that takes a hash, or a coin TXID:INDEX, also takes its short id:
the first 8 digits of the hash, such as a1075db5, or a1075db5:1 for a coin.
The listings show the short id. When two transactions of the wallet share
those 8 digits, hw names both and asks for the full hash.

hw keeps a wallet for each mnemonic, and portfolios in each wallet. A portfolio
holds all the accounts of the wallet that have the same account index:
portfolio0 holds the accounts at index 0, on every network and for both types
of address. You never pick an account. You only say the network and the type
of address you want.

The commands work in one portfolio, the active portfolio. The first portfolio
you create becomes active. So with one mnemonic you work in portfolio0 of
wallet0 and never name it. `portfolios` marks the active portfolio with a *.
switch makes another portfolio active, until the next switch:

  hw switch 1               portfolio1 of the same wallet
  hw switch --wallet 1      portfolio0 of wallet1

Two options say which account of the active portfolio a command works on.
Each has a default. A key of the configuration file holds that default, and
setdefault sets it:

  Option            Values                        Default      Key
  --network NET     btc or bch                    btc          default-network
  --type TYPE       bip44 or bip84                bip44        default-type

A bip44 account has legacy addresses, which start with 1 on Bitcoin. A bip84
account has native SegWit addresses, which start with bc1q. Bitcoin Cash has
bip44 accounts only: it uses bip44 whatever the default says, and refuses
--type bip84.

A portfolio has accounts on eight networks. hw shows btc and bch, and hides
the six test networks, unless you change it with setdefault --show-network
and --hide-network. A hidden network is left out of every answer, and
--network cannot name it. `sync` and `recover` still work on it.

A payment spends the coins of the active portfolio on one network, of both
types together. hw shows a transaction as the portfolio sees it: Incoming,
Outgoing, or Internal when it moves funds between addresses of the portfolio.

A tag is a word you put on addresses and transactions to group them, as in
hw tag ADDRESS kyc. With --tag, a listing shows only the rows that have the
tag. With --untagged, it shows only the rows that have no tag. With
--spend-from-tag, a payment spends the coins at the addresses that have the
tag. `tags` lists the tags.

The address book keeps the addresses of other people, each with a label, as
in hw newcontact ADDRESS Landlord. The view of a transaction then underlines
such an address and shows its label. The Tab key completes it where you type
the recipient of a payment. `contacts` lists the contacts.

On a terminal, a listing such as `txs` goes through less: scroll it, search
it with /, and quit it with q. A listing that fits on the screen is shown as
it is. A line wider than the screen is cut at the edge of the screen, and the
right arrow key scrolls to the rest of the line. --csv FILENAME writes the
listing to a new file for a spreadsheet instead, with every row whole. A help
longer than the screen goes through less too, such as this one.

The options --unit, --json and --no-pager go before or after the command.

The settings are in ~/.hw/config.toml: the server, the default network, the
unit, the view of the listings, the address gap, the backups hw makes by
itself, and others. setdefault changes the defaults of the commands in that
file. showdefault shows them, with the active portfolio.

The Tab key completes the commands, the options, and what the wallet holds:
wallets and portfolios, the hashes of pending transactions, coins, addresses,
tags and the contacts of the address book. The shell needs a script for
this, which hw writes. In bash, add this line to ~/.bashrc:

  source <(hw --bash-completion-script hw)

The README explains it for zsh and fish.

Type hw COMMAND --help for the help of a command.
|],
      globals = GlobalOptions <$> unitOption <*> jsonOption <*> noPagerOption,
      noPagerName = noPagerLongName,
      groups =
        [ CommandGroup
            "Wallets and portfolios"
            [ newPortfolioCommand,
              recoverCommand,
              checkMnemonicCommand,
              mnemonicSheetCommand,
              exportWalletCommand,
              importWalletCommand,
              portfoliosCommand,
              switchCommand,
              setDefaultCommand,
              showDefaultCommand,
              deletePortfolioCommand,
              deleteWalletCommand,
              syncCommand
            ],
          CommandGroup
            "Addresses"
            [ receiveCommand,
              showCommand,
              addressesCommand,
              balancesCommand,
              labelCommand
            ],
          CommandGroup
            "Coins"
            [ coinsCommand,
              lockCoinCommand,
              unlockCoinCommand
            ],
          CommandGroup
            "Payments"
            [ feesCommand,
              sendCommand,
              sendAllCommand,
              prepareTxCommand,
              prepareSendAllCommand,
              signTxCommand,
              broadcastCommand,
              bumpFeeCommand
            ],
          CommandGroup
            "Transactions"
            [ txsCommand,
              pendingTxsCommand,
              exportTxCommand,
              importTxCommand,
              deleteTxCommand
            ],
          CommandGroup
            "Tags"
            [ tagCommand,
              untagCommand,
              tagsCommand,
              renameTagCommand,
              deleteTagCommand
            ],
          CommandGroup
            "Address book"
            [ newContactCommand,
              contactsCommand,
              deleteContactCommand
            ],
          CommandGroup
            "Sweeps"
            [ sweepCommand,
              prepareSweepCommand,
              signSweepCommand
            ],
          CommandGroup
            "Messages"
            [ signMsgCommand,
              verifyMsgCommand
            ],
          CommandGroup
            "The wallet"
            [ backupCommand,
              showBackupsCommand,
              restoreDatabaseCommand,
              exportSheetCommand,
              exportJournalCommand,
              reportCommand
            ]
        ]
    }

-- | --json, an option of every command: the answer as JSON, for scripts
jsonOption :: Args Bool
jsonOption =
  switch $
    short 'j'
      <> long "json"
      <> help "Print the answer as JSON, for scripts. Questions and warnings go to stderr."

-- | The long name of --no-pager. The option is parsed with the others
-- for a listing. A help is shown before anything is parsed, so the
-- program gives the runner this name to look for.
noPagerLongName :: String
noPagerLongName = "no-pager"

-- | --no-pager, an option of every command: a listing or a help printed as
-- it is, through no pager
noPagerOption :: Args Bool
noPagerOption =
  switch $
    long noPagerLongName
      <> help
        [r|
Print a listing or a help as it is, through no pager. Without it, on a
terminal, a listing or a help goes through less, or through the pager that
the environment variable HW_PAGER or PAGER names.
|]

-- | --unit, an option of every command: the unit of the amounts of this
-- command, in place of the one of the configuration
unitOption :: Args (Maybe AmountUnit)
unitOption =
  optional . option (maybeReader (readUnit . cs)) $
    short 'u'
      <> long "unit"
      <> metavar "UNIT"
      <> completeWith ["btc", "bit", "sat"]
      <> help
        [r|
The unit of the amounts shown, and of the amounts typed without a unit: btc,
bit or sat. A bit is 100 satoshis. Without it, the unit is the one that the
key unit of config.toml names, or btc. setdefault --unit sets that key. In
hw showdefault --unit, the option takes no value and shows that key.
|]
