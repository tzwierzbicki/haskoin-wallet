{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the commands that send a transaction: send and
-- sendall, which prepare, sign and send a payment in one command, and
-- broadcast, which sends a signed one. send and sendall take the options
-- of preparetx and preparesendall ("Haskoin.Wallet.Cli.Parser.Payment").
-- "Haskoin.Wallet.Commands.Send" runs them.
module Haskoin.Wallet.Cli.Parser.Send
  ( sendCommand,
    sendAllCommand,
    broadcastCommand,
  )
where

import Control.Applicative (optional)
import Data.List (intercalate)
import Data.Text (Text)
import Haskoin.Wallet.Cli.Command (Command (..))
import Haskoin.Wallet.Cli.Completion (idsToSend)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (fileNetworkOption, hashRefReader, inputFileMaybeOption, txLabelOptionSaying, yesOption)
import Haskoin.Wallet.Cli.Parser.Payment (paymentOptions, sendAllOptions)
import Haskoin.Wallet.ShortId (HashRef (..))
import Text.RawString.QQ (r)

{- send -}

-- | send: preparetx, signtx and broadcast in one command. It is not marked
-- as a command to run offline, since it needs the server. Its help warns
-- that the mnemonic is typed on an online computer, as the help of
-- recover does. The help is the text sendall shares
-- (@inOneCommandDetails@).
sendCommand :: CommandSpec Command
sendCommand =
  CommandSpec
    { name = "send",
      offline = False,
      summary = "Prepare, sign and send a payment in one command",
      details =
        inOneCommandDetails
          InOneCommand
            { inOneName = "send",
              inOnePrepare = "preparetx",
              inOneOpening = "`send` makes a payment in one command.",
              inOneExplained = "the coins, coin control, the change, the lock time and the fee",
              inOneQuestions = "--yes answers yes to the three questions that preparetx may ask: about a fee rate above 100 satoshis per virtual byte, about a wallet that is behind the chain, and about coins that do not all have the same tags."
            },
      args = CommandSend <$> paymentOptions (pure Nothing) sentLabelOption
    }

-- | What differs between the helps of send and sendall, the two commands
-- that prepare a transaction, sign it and send it
data InOneCommand = InOneCommand
  { -- | The name of the command
    inOneName :: !String,
    -- | The command that prepares the same transaction, unsigned
    inOnePrepare :: !String,
    -- | What the command does, in a sentence
    inOneOpening :: !String,
    -- | What the help of the preparing command explains, and this one
    -- leaves to it
    inOneExplained :: !String,
    -- | The questions of the preparing command, and the options that
    -- answer them
    inOneQuestions :: !String
  }

-- | The help of send or sendall. The two commands differ only in the
-- transaction they prepare, so their helps are one text, with the few
-- words that differ given. A rule of one that changes then changes in the
-- help of the other. The name is in backticks, since send is an everyday
-- word that the help would not take for a command otherwise.
inOneCommandDetails :: InOneCommand -> String
inOneCommandDetails h =
  intercalate
    "\n\n"
    [ unwords
        [ h.inOneOpening,
          "It prepares the transaction as " <> h.inOnePrepare <> " does, shows it, asks for the mnemonic, signs the transaction as signtx does, and sends it as broadcast does.",
          "The result is the same as when you run the three commands one after the other with the same options."
        ],
      unwords
        [ name <> " needs the mnemonic and the server at the same time, so it runs on an online computer and not on an offline one.",
          "A computer that was tampered with can steal a mnemonic typed on it.",
          "Use " <> name <> " for a wallet whose mnemonic you are willing to type on this computer.",
          "To keep the mnemonic offline, pay in three steps instead: " <> h.inOnePrepare <> " --output here, signtx --input on the offline computer, and broadcast --input here."
        ],
      unwords
        [ name <> " takes the options of " <> h.inOnePrepare <> ", all but --output, and builds the same transaction from them.",
          "The help of " <> h.inOnePrepare <> " explains " <> h.inOneExplained <> ". Among the options of coin control, a chooser names coins to spend, and a filter narrows the coins that may be spent."
        ],
      unwords
        [ "The command shows the transaction once, before it asks for the mnemonic.",
          "Check it then: the addresses, the amounts and the fee.",
          "Typing the mnemonic is the confirmation.",
          name <> " asks nothing more before it sends, and sending cannot be undone.",
          "To stop, press Ctrl-C when it asks for the mnemonic.",
          "When the mnemonic does not match the portfolio, the command asks for it again, three times in all."
        ],
      unwords
        [ "A payment that is not signed leaves nothing in the wallet.",
          "When " <> name <> " refuses the payment, when your answer to a question stops it, and when you stop at the mnemonic, the wallet is as it was before.",
          "It has no new pending transaction, and no coin is locked."
        ],
      unwords
        [ name <> " checks that the server answers before it asks for the mnemonic.",
          "Once the transaction is signed, the wallet keeps it as a pending transaction, before anything leaves the computer.",
          "If the sending then fails, for example when no node accepts the transaction, " <> name <> " says why. The transaction stays signed and pending, as it is after signtx.",
          "broadcast sends it again by its txid, and deletetx deletes it and frees its coins.",
          "The help of broadcast explains what the server answers, and how long it waits."
        ],
      unwords
        [ name <> " answers with the txid of the transaction it sent, and does not show the transaction again.",
          "It then syncs the portfolio that paid, and any other portfolio of the wallet that the transaction pays, and shows what each holds and can spend.",
          "So the transaction is in `txs` at once, and its change is there for the next payment.",
          "When that sync fails, the transaction is still sent: " <> name <> " says so and exits as a success, and `sync` brings the wallet up to date.",
          "On Bitcoin, bumpfee replaces a payment that stays unconfirmed."
        ],
      unwords
        [ h.inOneQuestions,
          "The command still asks for the mnemonic.",
          "A script writes the mnemonic to the standard input of " <> name <> "."
        ]
    ]
  where
    name = "`" <> h.inOneName <> "`"

{- sendall -}

-- | sendall: preparesendall, signtx and broadcast in one command. It is to
-- preparesendall what send is to preparetx: the same steps after the
-- transaction is prepared, and the same help but for the words about that
-- transaction (@inOneCommandDetails@). Like send, it is not marked as a
-- command to run offline, since it needs the server.
sendAllCommand :: CommandSpec Command
sendAllCommand =
  CommandSpec
    { name = "sendall",
      offline = False,
      summary = "Prepare, sign and send all the coins in one command",
      details =
        inOneCommandDetails
          InOneCommand
            { inOneName = "sendall",
              inOnePrepare = "preparesendall",
              inOneOpening = "`sendall` sends every coin that the active portfolio can spend now on one network, in one command. It does for all the coins what `send` does for one payment.",
              inOneExplained = "the recipients and what each gets, the dust coins, coin control, the lock time and the fee",
              inOneQuestions = "--yes answers yes to three questions that preparesendall may ask: about a fee rate above 100 satoshis per virtual byte, about a wallet that is behind the chain, and about coins that do not all have the same tags. --spend-dust or --leave-dust answers the fourth question, about the dust coins, which are worth no more than the fee to spend them. If you answer no to that question, `sendall` does not stop: it leaves those coins in the portfolio and sends the others."
            },
      args = CommandSendAll <$> sendAllOptions (pure Nothing) sentLabelOption
    }

-- | The label of a transaction send or sendall sends, which has no
-- --output file to go to
sentLabelOption :: Args (Maybe Text)
sentLabelOption = txLabelOptionSaying "The label stays on the transaction once the network has it."

{- broadcast -}

-- | broadcast, which sends a signed transaction to the network
broadcastCommand :: CommandSpec Command
broadcastCommand =
  CommandSpec
    { name = "broadcast",
      offline = False,
      summary = "Send a signed transaction to the network",
      details =
        [r|
broadcast sends a signed transaction to the network, through the server.
Sending cannot be undone. So broadcast first shows the transaction, as `show`
does, and sends it only if you answer yes. Its answer is then the txid, since
you just read the transaction. --yes sends without asking, for scripts, and
answers with the transaction, marked as sent.

The server hands the transaction to a node. When the server can tell that no
node will accept the transaction, it says why, as for a coin that is spent
already, or for a fee below what the node relays. broadcast then fails with
what the server said, and the transaction stays signed and not sent. The
server cannot always tell: a node of Bitcoin gives its reason only to a
server that its operator connected to the RPC of the node.

After the server has taken the transaction, broadcast waits up to ten seconds
for the server to list it in its mempool. If the server does not list it, hw
records nothing, and broadcast says so. Run broadcast again later.

Give a pending transaction by its TXID, which signtx and pendingtxs show, in
full or by its short id. Its nosigHash works too. Once the server lists the
transaction, hw marks it as sent.

broadcast then syncs the portfolio that paid, and any other portfolio of the
wallet that the transaction pays. That sync finds the transaction on the
network: the transaction is no longer pending, `txs` lists it, and its change
is a coin of the wallet. broadcast ends with what each of these portfolios
holds and can spend. `send`, `sendall` and `sweep` do the same.

When that sync fails, the transaction is still sent. broadcast says so, names
`sync` as the next step, and exits as a success. The transaction stays
pending and marked as sent. From then on, `sync` sends it again whenever the
server loses it, and removes it from the pending transactions once the
network lists it. When the transaction is not on the network after that,
`sync` says so on every run, with what the server said, until you delete the
transaction with deletetx.

A payment that a sync has seen on the network is no longer pending. broadcast
has nothing to send for it, and says so. `txs` shows its state.

Or give a signed file with --input, such as the file that an offline computer
returns. When the wallet holds the same transaction as pending, the signed
file replaces it, and broadcast sends it as described above. When the wallet
does not hold it, as for a sweep of coins from outside the wallet, hw checks
the transaction and sends it, and keeps no pending transaction for it. A
payment of one of its portfolios that it does not hold, prepared on another
computer for instance, is shown as that portfolio sees it, and sent on its
network. The sync that follows brings it into the history of that portfolio.

The file is a PSBT, in bytes or in base64 text, finalized or not, as another
wallet or a hardware wallet returns it. broadcast also takes a file that holds
the signed transaction alone, in hexadecimal, as a wallet gives it once it is
finalized. It sends such a file only when the wallet holds the transaction. A
file that hw wrote names its network. A file of another wallet names none.
When the wallet holds the transaction, hw knows its network. Otherwise hw
sends the transaction on --network, or on the default network.

broadcast refuses a transaction that spends a locked coin, before it asks: a
coin that lockcoin locked, or a coin that a tag locks, unless the payment
named the tag with --spend-from-tag. This holds for a pending transaction and
for a file, with --yes too.
|],
      args =
        CommandBroadcast
          <$> optional signedTxidArg
          <*> inputFileMaybeOption
          <*> yesOption "Send the transaction without asking first"
          <*> fileNetworkOption
    }

-- | A signed pending transaction to send, by its txid. Its nosigHash works
-- too, as the user may have noted it before signing. Either goes in full
-- or by its short id.
signedTxidArg :: Args HashRef
signedTxidArg =
  argument hashRefReader $
    metavar "TXID"
      <> completer (mkCompleter idsToSend)
      <> help "The txid of a signed pending transaction, as signtx and pendingtxs show it, in full or by its short id, the first 8 digits. Its nosigHash works too."
