{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the sweeps: sweep, preparesweep and signsweep, each
-- with its options and its help. A sweep spends coins of addresses that
-- are not of the wallet, so its options name the coins by their whole
-- txid, and the keys by a file. "Haskoin.Wallet.Commands.Sweep" runs them.
module Haskoin.Wallet.Cli.Parser.Sweep
  ( sweepCommand,
    prepareSweepCommand,
    signSweepCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Cli.Command (Command (..), PrepareSweep (..), readCoinRef)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (coinOptionFields, dustCoinsOption, feeOption, fileCompletion, fileNetworkOption, networkOptionWith, outputFileMaybeOption, repeatAddressOption, txLabelOption, txLabelOptionSaying, underFee, yesOption)
import Haskoin.Wallet.Cli.Parser.Signing (signingOptions)
import Haskoin.Wallet.Keys (KeySource (..))
import Haskoin.Wallet.ShortId (CoinRef (..), HashRef (..))
import Text.RawString.QQ (r)

{- sweep -}

-- | sweep: preparesweep, signsweep and broadcast in one command, as send
-- is preparetx, signtx and broadcast. It is not marked as a command to
-- run offline, since it needs the server. Its help warns that the private
-- keys are read on an online computer, as the help of send does for the
-- mnemonic.
--
-- It takes the options of preparesweep but five. --output goes, since it
-- writes no file. --sweep-from, --address-file and --coin go, since they
-- name coins without their keys: preparesweep adds such coins to those of
-- the keys, and a command that signs at once could sign none of them.
-- --key-file stays, and the keys are typed without it.
sweepCommand :: CommandSpec Command
sweepCommand =
  CommandSpec
    { name = "sweep",
      offline = False,
      summary = "Prepare, sign and send a sweep in one command",
      details =
        [r|
`sweep` moves coins whose private keys you hold, from an old wallet for
example, to new addresses, in one command. It prepares the sweep as
preparesweep does, shows it, signs it with the private keys as signsweep does,
and sends it as broadcast does. The result is the same as when you run the
three commands one after the other with the same options.

`sweep` needs the private keys and the server at the same time, so it runs on
an online computer and not on an offline one. A computer that was tampered
with can steal a private key that it reads. Use `sweep` for keys that you are
willing to give to this computer. To keep the keys offline, sweep in three
steps instead: preparesweep --output here, signsweep --input on the offline
computer, and broadcast --input here.

Give the file that holds the private keys with --key-file. Or type them on
the terminal, where they are not shown, with --key-file - or without
--key-file. `sweep` takes every coin at the addresses of the keys. Give each
destination with --sweep-to, as for preparesweep.

`sweep` takes the options of preparesweep, all but --output, --sweep-from,
--address-file and --coin. It signs at once, so every coin it takes needs its
key, and the keys name the coins. The help of preparesweep explains the
destinations, the dust coins, the portfolio and the network of a sweep, the
lock time and the fee.

The command reads the keys first, since the keys say which coins the sweep
takes. It then shows the sweep in full, and asks whether to sign it and send
it. Check it then: the addresses, the amounts and the fee. Sending cannot be
undone. Any answer but yes stops the command. --yes signs and sends without
asking, for scripts. It also takes a fee rate above 100 satoshis per virtual
byte without asking. The sweep is shown all the same.

A sweep that is not signed leaves nothing in the wallet. When `sweep` refuses
the sweep, and when your answer to a question stops it, the wallet is as it
was before, with no new pending transaction.

`sweep` checks that the server answers before it reads the keys. A sweep that
pays an address of this wallet is kept as a pending transaction once it is
signed, before anything leaves the computer. If the sending then fails,
`sweep` says why. The sweep stays signed and pending, as it is after
signsweep. broadcast sends it again by its txid, and deletetx deletes it.

A sweep that pays no address of this wallet needs --network, as for
preparesweep. The wallet keeps nothing of it, signed or not. If the sending
fails, run `sweep` again.

`sweep` answers with the txid of the transaction it sent, and does not show
the sweep again. It then syncs the portfolio that the sweep pays, and shows
what the portfolio holds. The coin of the sweep comes from keys outside the
wallet, so hw spends it once a block has it. When that sync fails, the sweep
is still sent: `sweep` says so and exits as a success, and `sync` brings the
wallet up to date.

A script gives the keys with --key-file, or writes them to the standard input
of `sweep`. Keys on the standard input are read to its end, so no line is
left for an answer: give --yes, and --spend-dust or --leave-dust.
|],
      args =
        CommandSweep
          <$> ( PrepareSweep
                  <$> sweepNetworkOption
                  <*> underCoinsSwept prvKeysMaybeOption
                  <*> pure []
                  <*> pure Nothing
                  <*> some sweepToOption -- some: at least one
                  <*> sweepRepeatOption
                  <*> pure []
                  <*> underCoinsSwept (dustCoinsOption "at their addresses")
                  <*> pure Nothing
                  <*> txLabelOptionSaying "The label stays on a sweep that pays this wallet, once the network has it."
                  <*> underFee feeOption
                  <*> underFee (yesOption "Sign the sweep and send it without asking first, and take a fee rate above 100 satoshis per virtual byte without asking")
              )
    }

-- | The network of a sweep, when given. A sweep that pays a portfolio of
-- the wallet is on the network of the addresses it pays. One that pays
-- none needs it.
sweepNetworkOption :: Args (Maybe Network)
sweepNetworkOption =
  networkOptionWith "Needed by a sweep that pays no portfolio of this wallet. The default-network of the configuration file does not apply to it."

{- preparesweep -}

-- | preparesweep, which prepares a transaction that moves coins from
-- outside the wallet
prepareSweepCommand :: CommandSpec Command
prepareSweepCommand =
  CommandSpec
    { name = "preparesweep",
      offline = False,
      summary = "Prepare a transaction that moves coins from outside the wallet",
      details =
        [r|
A sweep moves coins whose private keys you hold, from an old wallet for
example, to new addresses. preparesweep prepares the sweep, and needs no key.
signsweep then signs it with the private keys, and broadcast sends it. `sweep`
does the three steps in one command, for keys that you are willing to give to
an online computer.

Four options say which coins to sweep. Give any of them:

  --sweep-from ADDRESS     every coin at the address
  --address-file FILENAME  every coin at the addresses listed in the file
  --key-file FILENAME      every coin at the addresses of the keys in the file
  --coin TXID:INDEX        one coin, with its txid in full

--coin takes the txid in full: the wallet does not hold the coin, so a short
id would name none. The --address-file holds addresses separated by spaces or
line breaks, as the --key-file holds keys. Use it when an old wallet has more
addresses than a command line takes.

A dust coin is a coin worth no more than the fee to spend it. Sweeping it
lowers what the destinations get. Whether a coin is dust depends on the fee
rate. preparesweep lists the dust coins, tells what the destinations get with
them and without them, and asks whether to spend them. The answer is for all
of them: any answer but yes leaves them at their addresses. --spend-dust or
--leave-dust answers in advance, for scripts. --yes does not answer this
question.

preparesweep answers with the sweep in full: every address it sweeps, with
its amount, and every address it pays. It names below the sweep each address
of --sweep-from or of the --address-file that holds no coin, and each dust
coin it leaves, which are not in the sweep.

A sweep is for coins from outside this wallet. preparesweep refuses an address
that a portfolio of this wallet has on the network of the sweep: signsweep
signs with the private keys of the coins, and you hold none for an address of
the wallet. Use sendall or preparetx to move the coins of the wallet.
--spend-from chooses the coins of an address.

preparesweep needs the server, so it runs on an online computer and not on an
offline one. With --key-file, the private keys are read on that computer too,
and one that was tampered with can steal them. The keys are needed only to
sign, with signsweep, so keep them on an offline computer if you can. Name the
coins with --sweep-from, --address-file or --coin instead.

Without the keys, hw cannot tell if a legacy coin has a compressed key, so it
counts the larger size. The fee is never below the rate you ask, and a coin of
a compressed key pays about a fifth more. Give --key-file for the exact fee.

A sweep takes legacy coins, native SegWit coins, and SegWit coins nested in a
script address: the coins that one private key signs. It takes no Taproot
coin. preparesweep refuses a Taproot address, and finds no coin at the Taproot
address of a key of the --key-file. A coin paid to a public key itself, as in
the first blocks of the chain, has no address, and is not swept.

Give each destination with --sweep-to, as ADDRESS or ADDRESS:AMOUNT, written as
for preparetx. An address with an amount gets exactly that amount. The
addresses without an amount share the rest in equal parts, and they pay the
fee. So give at least one address without an amount. When the rest does not
cover the fee, nothing is prepared. preparesendall shares the coins the same
way. hw refuses a destination given more than once, which is most often an
address pasted twice. With --repeat-address, the address gets an output each
time it is given, each a coin of its own.

A sweep that pays an address of this wallet belongs to the portfolio of that
address, on its network. hw finds the portfolio from the --sweep-to addresses,
however an address is written, such as a Bitcoin Cash address with or without
its prefix, or in its legacy form. When those addresses belong to several
portfolios, hw keeps the sweep in the active portfolio if it is one of them.
Otherwise hw names the portfolios, and prepares nothing: switch to the one to
keep the sweep in. A sweep is on one network, so hw refuses addresses of this
wallet on two networks. When the wallet has one address on two test networks,
give the --network. hw keeps the sweep as a pending transaction, to sign with
signsweep by its nosigHash. --output also writes it to a file. To prepare the
sweep again, at another fee rate for example, delete the first one with
deletetx.

A sweep that pays no address of this wallet needs no portfolio, even in a
database that has none. It needs --network and --output, and is signed from the
file. The default-network of the configuration file does not apply. Private
keys, coins and the legacy addresses of Bitcoin Cash are written the same way
on Bitcoin, so a wrong guess would sweep the coins of the other chain.

The sweep gets a lock time, as a payment of preparetx does. hw draws it from
the best block of the server, not from the best block the wallet knows, since
a sweep is prepared online.

A fee rate above 100 satoshis per virtual byte is more often a typo than a
choice, so preparesweep asks before it takes one. It asks before the private
keys are typed. --yes takes it without asking, for scripts.
|],
      args =
        CommandPrepareSweep
          <$> ( PrepareSweep
                  <$> sweepNetworkOption
                  <*> underCoinsSwept sweepKeysOption
                  <*> underCoinsSwept (many sweepFromOption)
                  <*> underCoinsSwept addressFileOption
                  <*> some sweepToOption -- some: at least one
                  <*> sweepRepeatOption
                  <*> underCoinsSwept (many sweepCoinOption)
                  <*> underCoinsSwept (dustCoinsOption "at their addresses")
                  <*> outputFileMaybeOption
                  <*> txLabelOption
                  <*> underFee feeOption
                  <*> underFee highFeeYesOption
              )
    }

-- | The options that tell which coins a sweep takes, under their
-- subheading of the help
underCoinsSwept :: Args a -> Args a
underCoinsSwept = optionGroup "Coins swept"

-- | --sweep-from of preparesweep: an address whose coins to sweep
sweepFromOption :: Args Text
sweepFromOption =
  strOption $
    short 'w'
      <> long "sweep-from"
      <> metavar "ADDRESS"
      <> help "An address whose coins to sweep. Repeat for several addresses."

-- | --address-file of preparesweep: a file of the addresses whose coins to
-- sweep
addressFileOption :: Args (Maybe FilePath)
addressFileOption =
  optional . strOption $
    long "address-file"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "A file of addresses whose coins to sweep, separated by spaces or line breaks"

-- | --sweep-to of preparesweep: a destination of the coins, alone or with
-- its amount
sweepToOption :: Args Text
sweepToOption =
  strOption $
    short 't'
      <> long "sweep-to"
      <> metavar "ADDRESS[:AMOUNT]"
      <> help "A destination. An ADDRESS alone shares what remains with the other addresses given alone. ADDRESS:AMOUNT gets exactly that amount. Repeat for several destinations."

-- | Whether a destination of a sweep may be given more than once
sweepRepeatOption :: Args Bool
sweepRepeatOption = repeatAddressOption "Pay a destination given more than once an output each time, each a coin of its own. Without it, hw refuses a destination given twice, which is most often a mistake."

-- | A coin to sweep, by its outpoint, with its txid in full. The wallet
-- does not hold it, and has none to offer.
sweepCoinOption :: Args OutPoint
sweepCoinOption =
  option fullCoinReader $
    coinOptionFields "A coin to sweep: the id of its transaction in full, a colon, and its index among the outputs. Repeat for several coins."

-- | Whether preparesweep takes a high fee rate without asking
highFeeYesOption :: Args Bool
highFeeYesOption = yesOption "Take a fee rate above 100 satoshis per virtual byte without asking"

-- | A coin outside the wallet, as a block explorer writes it: TXID:INDEX,
-- the txid in full. preparesweep fetches it from the server, which looks a
-- transaction up by its whole txid: its short id would name nothing there,
-- and the wallet, which does not hold the coin, cannot tell its txid.
fullCoinReader :: ReadM OutPoint
fullCoinReader = eitherReader $ \s -> case readCoinRef s of
  Just (CoinRef (FullHash hash) index) -> Right (OutPoint hash index)
  Just (CoinRef (ShortHash _) _) -> Left ("The coin " <> s <> " needs its txid in full: the wallet does not hold a coin it sweeps, and the server looks up a whole txid only")
  -- Not the refusal of a coin of the wallet, which offers the short id
  -- that the line above refuses, and names the listing of `coins`
  Nothing -> Left ("The coin " <> s <> " is not valid: write it TXID:INDEX, with the 64 digits of the txid, a colon, and the index of the coin among the outputs")

{- signsweep -}

-- | signsweep, which signs a sweep with the private keys of its coins
signSweepCommand :: CommandSpec Command
signSweepCommand =
  CommandSpec
    { name = "signsweep",
      offline = True,
      summary = "Sign a sweep with the private keys of its coins",
      details =
        [r|
signsweep signs a sweep that preparesweep made. Its coins are not the
wallet's, so signing needs their private keys. Give the file that holds them
with --key-file. Or type them on the terminal, where they are not shown, with
--key-file - or without --key-file.

hw reads no key when one of them is wrong. The error gives the place of the
word and the reason, such as a wrong checksum or a key of another network,
and never the word itself. signsweep refuses a payment of the wallet before
it asks for the keys: the mnemonic signs it, with signtx.

Give a pending sweep by its NOSIGHASH. The signed sweep then takes its place
in the wallet, shown by its txid. Or give a sweep file with --input. The
signed sweep then goes to the --output file. Without --output, hw offers to
replace the --input file. It asks this before it asks for the keys. --yes
answers that question in advance, so a script gives --yes or --output. hw
shows a sweep of a file with the labels the file carries, and writes them to
the signed file.

A sweep that pays no portfolio of this wallet needs no portfolio here either.
hw signs it on the network that its file names. A file of another wallet names
no network: hw signs it on --network, or on the default network.
|],
      args =
        CommandSignSweep
          <$> signingOptions
          <*> prvKeysMaybeOption
          <*> fileNetworkOption
    }

-- | The private keys of signsweep and of sweep, which sign with them, and
-- ask for them on the terminal without a file. The help says nothing of
-- an online computer: signsweep can run offline, and the help of sweep
-- says where it runs.
prvKeysMaybeOption :: Args (Maybe KeySource)
prvKeysMaybeOption = keyFileOption keyFileHelp

-- | The private keys of preparesweep. It needs the server, so they are
-- read on an online computer, and it can do without them.
sweepKeysOption :: Args (Maybe KeySource)
sweepKeysOption = keyFileOption (keyFileHelp <> " This puts them on an online computer, so leave it out if you can.")

-- | The help of the option that names the file of the private keys, as
-- signsweep and sweep give it
keyFileHelp :: String
keyFileHelp = "A file of private keys in WIF, minikey or hex format, separated by spaces or line breaks. Give - to type them on the terminal instead."

-- | The private keys: a file, or - for the terminal
keyFileOption :: String -> Args (Maybe KeySource)
keyFileOption what =
  optional . option (eitherReader source) $
    short 'k'
      <> long "key-file"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help what
  where
    source "-" = Right KeysOnTerminal
    source file = Right (KeysInFile file)
