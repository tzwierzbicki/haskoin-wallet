{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of label, with its options and its help.
-- "Haskoin.Wallet.Commands.Label" runs it.
module Haskoin.Wallet.Cli.Parser.Label
  ( labelCommand,
  )
where

import Data.String.Conversions (cs)
import Haskoin.Wallet.Cli.Command (Command (..), Label (..), Target (..))
import Haskoin.Wallet.Cli.Completion (labelTargets)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (namedAddressNetworkOption, typeOptionWith)
import Text.RawString.QQ (r)

-- | label, which labels an address, a transaction or a contact, or
-- describes the active portfolio or its wallet
labelCommand :: CommandSpec Command
labelCommand =
  CommandSpec
    { name = "label",
      offline = False,
      summary = "Label an address, a transaction or a contact, or describe a portfolio or a wallet",
      details =
        [r|
`label` gives a label to a receiving address, to a transaction or to a
contact of the address book. The new label replaces the one it had. After
the word portfolio or the word wallet, the label is a description instead.
An empty LABEL, "", removes the label or the description.

The word before the label tells what to label:

  ADDRESS        a receiving address of the wallet, or a contact of the
                 address book
  TYPE/0/i       the receiving address number i of the account of that
                 type, in the active portfolio, such as bip84/0/3
  TXID           a transaction, by its txid, or its nosigHash while pending
  portfolio      the active portfolio: the label is its description
  wallet         the wallet of the active portfolio: the label is its
                 description

`portfolios` shows the description of a portfolio and that of its wallet. A
description says what a portfolio or a wallet is for, in your own words.
The names of wallets and portfolios, such as wallet0 and portfolio0, are
fixed: they follow their indexes and cannot be changed. To describe another
portfolio, switch to it first. `label` always takes the two words. It
refuses a label alone, and so never takes a mistyped address, whose label
was left out, for a description.

The label of an address says what the payments to it are for. `addresses`,
`balances` and the transactions that pay the address show it. Only a
receiving address takes a label, so a path must be of the form /0/i. The
portfolio must have given the address out: `addresses` lists those it gave
out. A path starts with the type of its account, the way every view writes
a path. A path without the type, such as /0/3, is in the account that
--type picks, or in the account of the default-type.

The label of a transaction shows in txs and pendingtxs, in place of the
address that a payment pays, and as a Label field with --verbose. Name a
pending transaction by the hash that pendingtxs shows: its nosigHash, or
its txid once it is signed. Name a transaction that is on the network by
its txid, as txs shows it. Give either hash in full or by its short id, its
first 8 digits. preparetx, preparesendall and preparesweep set a label with
--label. The label of a pending transaction goes with it to the files of
exporttx and --output, and stays once the network has the transaction.
bumpfee gives a replacement the labels of the transactions it replaces. A
transaction gets the label in every portfolio that has it, such as both
portfolios of a payment between two portfolios.

A contact of the address book is labelled the same way, with hw label
ADDRESS LABEL. Its label says whose address it is. The address must be in
the address book first: newcontact adds it.

`label` finds the portfolio of an address or of a transaction by itself,
whichever portfolio is active. A path is always in the active portfolio.
--network is needed only when the wallet has the same address on several
networks. That can happen because the test networks share their keys. A
portfolio also has an address at the same path on each network where it
gave one out. --network then says which address to label. Without it,
`label` takes the only one, or else the one on the default network.
|],
      args =
        CommandLabel
          <$> ( (\(labelled, label) -> Label labelled (cs <$> label))
                  <$> secondLeftToCommand
                    ( metavar "ADDRESS|TXID|PATH|portfolio|wallet"
                        <> completer (mkCompleter labelTargets)
                        <> help "What to label: a receiving address of the wallet, as `addresses` lists it, a contact of the address book by its address, a transaction by its txid or the hash pendingtxs shows, in full or by its short id, the first 8 digits, or a path of the active portfolio, such as bip84/0/3. The word portfolio is the active portfolio, and the word wallet is its wallet: the label is then its description."
                    )
                    (metavar "LABEL" <> help "The new label or description, or \"\" to remove it. Quote it if it has spaces.")
                  <*> ( Target
                          <$> namedAddressNetworkOption
                          <*> typeOptionWith "With a path without its type, such as /0/3, the type of its account. Without it, the default-type of the configuration file, or bip44."
                      )
              )
    }
