{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of signtx, and the options that say what to sign
-- ('signingOptions'), which signsweep takes too
-- ("Haskoin.Wallet.Cli.Parser.Sweep"). "Haskoin.Wallet.Commands.Signing"
-- runs signtx.
module Haskoin.Wallet.Cli.Parser.Signing
  ( signingOptions,
    signTxCommand,
  )
where

import Control.Applicative (optional)
import Haskoin.Wallet.Cli.Command (Command (..), Signing (..))
import Haskoin.Wallet.Cli.Completion (hashesToSign)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (hashRefReader, inputFileMaybeOption, outputFileMaybeOption, yesOption)
import Haskoin.Wallet.ShortId (HashRef (..))
import Text.RawString.QQ (r)

-- | signtx, which signs a pending transaction or a transaction file
signTxCommand :: CommandSpec Command
signTxCommand =
  CommandSpec
    { name = "signtx",
      offline = True,
      summary = "Sign a pending transaction or a transaction file",
      details =
        [r|
signtx shows the transaction, as `show` does, then asks for the mnemonic and
the passphrase of the wallet, and signs the transaction. Check what it shows
before you type the mnemonic. When the mnemonic and the passphrase do not
match the portfolio, signtx asks for them again. You can type the shares of a
backup in place of the mnemonic. One mnemonic signs a transaction that spends
bip44 and bip84 coins together, since it gives the keys of both accounts.

A script can give the master private key of the wallet, which starts with
xprv, in place of the mnemonic and the passphrase. It is one line on the
input of signtx, with no passphrase after it.

There are two ways to name the transaction. Give a pending transaction by its
NOSIGHASH, in full or by its short id, which is its first 8 digits. The
signed transaction then replaces the unsigned one in the wallet, and hw shows
it by its txid. Or give a transaction file with --input, on an offline
computer for example. signtx then writes the signed transaction to the
--output file.

Without --output, hw offers to replace the --input file with the signed
transaction, since the unsigned file is of no use once it is signed. hw asks
this before it asks for the mnemonic. --yes answers the question in advance,
so a script gives --yes or --output. Otherwise the question takes the line of
the mnemonic as its answer, reads it as a no, and signtx stops.

signtx shows a transaction of a file with the labels that the file carries,
and writes them to the signed file. As the PSBT format requires, a signed
input no longer names its key. The file still says which address of the
portfolio each input spends from, in a field of hw. So `show` shows the
signed file as the payment it is, on the offline computer too.

hw finds the portfolio of a file by itself, whichever portfolio is active, as
importtx does. When a transaction moves funds between two portfolios, the
portfolio that pays signs it. hw refuses a file of a portfolio that this
database does not have yet. That happens, for example, on an offline computer
that holds portfolio0 alone. There, newportfolio --existing adds the next
portfolio of the mnemonic, and importwallet adds the portfolios of a wallet
file.

signtx signs in the same way a file that another wallet prepared for the
portfolio. The file can be in bytes or in base64 text, of version 0 or of
version 2, that of BIP 370. The signed file keeps its version and all its
fields, except the fields of the signed inputs that only a signer needs. hw
signs with the sighash ALL only. It signs only when every input that does not
belong to the portfolio is signed already.

signtx signs the coins of the wallet. It refuses a sweep, which spends none,
before it asks for the mnemonic: signsweep signs a sweep, with the private
keys of its coins.

signtx refuses a transaction that spends a locked coin, before it asks for
the mnemonic: a coin that lockcoin locked, or a coin that a tag locks, unless
the payment named the tag with --spend-from-tag. This holds for a pending
transaction and for a file. An offline computer that holds no coin has no
lock to check.

Signing sends nothing. To check the signed transaction, give its txid or its
file to `show`. Then send it with broadcast, or delete it with deletetx.
|],
      args =
        CommandSignTx
          <$> signingOptions
    }

-- | What signtx and signsweep sign
signingOptions :: Args Signing
signingOptions =
  Signing
    <$> optional nosigHashArg
    <*> inputFileMaybeOption
    <*> outputFileMaybeOption
    <*> yesOption "Replace the --input file with the signed transaction without asking first, when no --output file is given"

-- | A pending transaction to sign, by its nosigHash, in full or by its
-- short id
nosigHashArg :: Args HashRef
nosigHashArg =
  argument hashRefReader $
    metavar "NOSIGHASH"
      <> completer (mkCompleter hashesToSign)
      <> help "The nosigHash of a pending transaction, as pendingtxs shows it: in full, or its short id, the first 8 digits"
