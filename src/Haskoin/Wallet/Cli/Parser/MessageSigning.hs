{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the signed messages: signmsg and verifymsg, each
-- with its options and its help. "Haskoin.Wallet.Commands.MessageSigning"
-- runs them.
module Haskoin.Wallet.Cli.Parser.MessageSigning
  ( signMsgCommand,
    verifyMsgCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Cli.Command (Command (..), SignedMessageSource (..), SignMsg (..), VerifyMsg (..))
import Haskoin.Wallet.Cli.Completion (walletAddresses)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (everyNetwork, fileCompletion, networkOptionSaying, networkOptionWith)
import Haskoin.Wallet.Database.Address (AddressNamed, readAddressNamed)
import Text.RawString.QQ (r)

{- signmsg -}

-- | signmsg, which signs a message with the key of an address
signMsgCommand :: CommandSpec Command
signMsgCommand =
  CommandSpec
    { name = "signmsg",
      offline = True,
      summary = "Sign a message with the key of an address",
      details =
        [r|
signmsg signs a message with the key of an address of the wallet, to prove
that you control the address. It asks for the mnemonic of the wallet of that
address, or for the shares of its backup. The signature uses the Bitcoin
Signed Message format, the legacy format of BIP 322. Any wallet can verify a
signature made from a P2PKH address, which starts with 1. A signature made
from a native SegWit address, which starts with bc1q, has the header byte of
BIP 137. The wallets that sign from such an address write and verify that
byte. Some wallets verify no signature from a SegWit address.

The message is signed exactly as given. hw warns about spaces or line breaks
at its start or end. Copying a message easily changes them, and the signature
would then fail to verify.

The address must be one that the wallet holds: one that this computer gave
out. The offline computer gives out no address, since receive runs on the
online computer. On the offline computer, give the path of the address
instead, with its type, such as bip84/0/3. addresses shows the path on the
online computer. signmsg derives the address at the path from the public key
of the account, as show does. The path is in the active portfolio, on
--network or the default network. The signed message names the address, so
you see which address signed. Sign with an address you gave out: a payment to
an address far past the others would not be found.

signmsg prints the signed message as a block of text: the message between two
marker lines, then the address and the signature. With --output, hw writes
that block to a file instead of printing it. verifymsg --input can then check
the file. It also reads the --json answer of signmsg, saved to a file.
|],
      args =
        CommandSignMsg
          <$> (SignMsg <$> signAddressArg <*> signMsgArg <*> signedMessageOutputOption <*> signNetworkOption)
    }

-- | The address whose key signs the message of signmsg, or its path in the
-- active portfolio
signAddressArg :: Args AddressNamed
signAddressArg =
  argument (eitherReader readAddressNamed) $
    metavar "ADDRESS|PATH"
      <> completer (mkCompleter walletAddresses)
      <> help "The address whose key signs the message. It must be an address that this wallet holds. Or give a path, such as bip84/0/3. It names the address at that path in the active portfolio, whether the wallet holds it or not. Use a path on the offline computer."

-- | The network of signmsg: that of an address the wallet has on several,
-- or that of the account of a path, which the path does not say
signNetworkOption :: Args (Maybe Network)
signNetworkOption =
  networkOptionWith "With an address: the network to find the address on, when the wallet has it on several networks. Without it, the only network that has the address, or else the default network. With a path: the network of the account of the path. Without it, the default-network of the configuration file, or btc."

-- | The message signmsg signs
signMsgArg :: Args Text
signMsgArg =
  strArgument $
    metavar "MESSAGE"
      <> help "The message to sign. Quote it if it has spaces."

-- | The file to write the armored block to. It is -o, as the file of a
-- transaction is in the other commands.
signedMessageOutputOption :: Args (Maybe FilePath)
signedMessageOutputOption =
  optional . strOption $
    short 'o'
      <> long "output"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "Write the signed message to this file instead of printing it. The file must not exist yet."

{- verifymsg -}

-- | verifymsg, which checks the signature of a message
verifyMsgCommand :: CommandSpec Command
verifyMsgCommand =
  CommandSpec
    { name = "verifymsg",
      offline = False,
      summary = "Check the signature of a message",
      details =
        [r|
verifymsg checks that the key of ADDRESS signed MESSAGE. SIGNATURE is the
signature that signmsg or another wallet made. The address can be anyone's:
a P2PKH address, which starts with 1, or a SegWit address of one key, native
or wrapped, which starts with bc1q or 3. A signature from a SegWit address
is valid when its key gives the address, whatever header byte the wallet
that signed chose.

verifymsg needs no --network. hw finds the network from the address itself,
among all the networks, those it hides too: the check reads nothing of the
wallet. A message is signed the same way on every network, so the network
does not change the answer.

Give the message exactly as it was signed. A single changed byte makes the
signature invalid. When it is invalid, hw says so and exits with an error, so
that a script can tell.

With --input, hw reads the address, the signature and the message from a file
that holds the output of signmsg. The file holds the block of text that
signmsg prints, which other wallets write too, or the --json answer of
signmsg. In the block, the message is everything between the first marker
line and the signature marker line, exactly as it is. After the signature
marker come the address, then the signature.
|],
      args = CommandVerifyMsg <$> (VerifyMsg <$> source <*> verifyNetworkOption)
    }
  where
    source =
      (Given <$> verifyAddressArg <*> signatureArg <*> verifyMsgArg)
        <|> (FromFile <$> signedMessageFileOption)

-- | The network of verifymsg, which the user seldom needs: hw reads the
-- address on any network that reads it. It is kept for a user who wants
-- the address read on one network alone, and refused when it is not an
-- address there. It may be a hidden network, unlike the --network of the
-- other commands ('Haskoin.Wallet.Cli.Command.networkNamedBy'), so its
-- help does not say that it is one of the networks hw shows.
verifyNetworkOption :: Args (Maybe Network)
verifyNetworkOption =
  networkOptionSaying ("Read the address as an address of this network only: " <> everyNetwork <> ". It may be a network hw hides. Without it, hw takes the address on any network where it is valid. The answer is the same on each network.")

-- | --input of verifymsg: the file that holds what signmsg wrote, in place
-- of the three arguments
signedMessageFileOption :: Args FilePath
signedMessageFileOption =
  strOption $
    short 'i'
      <> long "input"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "Read the address, the signature and the message from this file. It holds the output of signmsg: the block of text that signmsg prints, or its --json answer."

-- | The address verifymsg checks the signature against
verifyAddressArg :: Args Text
verifyAddressArg =
  strArgument $
    metavar "ADDRESS"
      <> help "The address that is said to have signed the message"

-- | The signature verifymsg checks, in base64
signatureArg :: Args Text
signatureArg =
  strArgument $
    metavar "SIGNATURE"
      <> help "The signature in base64, as signmsg writes it"

-- | The message verifymsg checks the signature of
verifyMsgArg :: Args Text
verifyMsgArg =
  strArgument $
    metavar "MESSAGE"
      <> help "The message, exactly as it was signed"
