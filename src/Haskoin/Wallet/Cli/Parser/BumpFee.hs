{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of bumpfee, which prepares the replacement of a payment
-- with a higher fee. "Haskoin.Wallet.Commands.BumpFee" runs it.
module Haskoin.Wallet.Cli.Parser.BumpFee
  ( bumpFeeCommand,
  )
where

import Control.Applicative (optional)
import Haskoin.Wallet.Cli.Command (BumpFee (..), Command (..))
import Haskoin.Wallet.Cli.Completion (idsToBump)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (hashRefReader, outputFileMaybeOption, recipientsPayOption, yesOption)
import Haskoin.Wallet.ShortId (HashRef (..))
import Haskoin.Wallet.Tx.FeeRates (FeeRateChoice, readFeeRateChoice)
import Text.RawString.QQ (r)

-- | bumpfee, which replaces a stuck payment with one that pays a higher fee
bumpFeeCommand :: CommandSpec Command
bumpFeeCommand =
  CommandSpec
    { name = "bumpfee",
      offline = False,
      summary = "Replace a stuck payment with one that pays a higher fee",
      details =
        [r|
bumpfee prepares a replacement for a payment that the network has not
confirmed yet. Use it when a payment is stuck because its fee is too low. The
replacement pays a higher fee, as BIP 125 allows on Bitcoin. It spends the
same coins and pays the recipients the same amounts. The extra fee comes out
of the change.

bumpfee finds the portfolio of the transaction, whichever portfolio is
active. It is the portfolio that holds the transaction. When several
portfolios hold it, it is the one that pays it. When hw cannot tell, it takes
the active portfolio if that is one of them. Otherwise it names them, and you
switch to the one that pays.

When the change cannot pay the extra fee, hw offers to add confirmed coins of
the portfolio, of either type. When no coin can be added, or when you decline
them, hw offers to take what is missing from what the recipients receive.
Either one recipient pays it alone, or all the recipients pay it in equal
shares, and the first one also pays the remainder. hw asks which, and leaves
no recipient below the dust limit.

The change keeps its address, and that address keeps its tags: bumpfee puts
no tag on an address and takes none off. When the payment had no change and
the replacement has some, the change goes to a new change address, which has
no tag.

The coins that hw adds may not have the same tags as the coins the payment
spends already. The replacement then joins two groups of coins, as any
payment that spends coins of both groups does. hw names the groups and asks
once more before it replaces anything.

--yes answers yes to every question, for scripts. For the extra fee, it takes
the first of these that can pay: the change, then coins of the portfolio,
then a lone recipient, then all the recipients in equal shares. When equal
shares would leave a recipient below the dust limit, hw still asks which
recipient pays alone, since --yes never picks a recipient.

With --recipients-pay, the recipients pay the extra fee, in equal shares, as
they pay the whole fee with preparetx --recipients-pay. The change keeps its
amount, and hw adds no coin, even with --yes. hw does not ask who pays.

Another transaction of the portfolio may spend the change of the stuck
payment. Replacing the stuck payment would make that transaction invalid too.
So hw offers to replace both with one transaction that pays the recipients of
both.

hw keeps the replacement as a pending transaction. While it is pending, it
locks the coins of the stuck payment, such as its change: they go away once
the replacement is sent, so no payment may spend them. `coins` shows them as
"locked by tx", and deletetx frees them.

Sign the replacement with signtx and send it with broadcast. broadcast then
removes the transactions it replaces from the pending transactions, and syncs
the portfolio. txs then shows the stuck payment as dead, and names the
replacement on it. To raise the fee again, give bumpfee the txid of the
replacement.

The network does not tell hw when it refuses a replacement. For example, a
replacement must also pay the fees of transactions that hw cannot see, such
as a transaction in which a recipient already spends the payment. When a
replacement never confirms, delete it with deletetx, run `sync`, and run
bumpfee again with a higher fee rate.

bumpfee cannot replace every transaction. It cannot replace one that was sent
before hw marked its transactions as replaceable, or one that spends coins of
another wallet, such as a sweep. It works on Bitcoin only, since Bitcoin Cash
does not replace transactions.

When the stuck transaction pays several addresses of the wallet besides its
recipients, with its change among them, the replacement has one change output
for all of them. Change that --change-back sent back to an address the
payment spends from is still change, and pays the extra fee. --change-back
largest and smallest always send the change to such an address. Change that
--change-back sent to another receiving address counts as a payment: the
replacement keeps it, and does not reduce it.

bumpfee gives the replacement a lock time, the height of the best block the
wallet knows, as preparetx does. When that block was mined more than eight
hours ago, the wallet is behind the chain. bumpfee then asks whether to
prepare the replacement anyway, with a lock time of 0.
|],
      args =
        CommandBumpFee
          <$> ( BumpFee
                  <$> bumpedArg
                  <*> replacementFeeOption
                  <*> recipientsPayOption "The recipients pay the extra fee, in equal shares taken from their amounts. The change keeps its amount, and hw adds no coin of the portfolio. hw does not ask who pays."
                  <*> outputFileMaybeOption
                  <*> yesOption "Answer yes in advance to every question: about a fee rate above 100 satoshis per virtual byte, a wallet that is behind the chain, adding coins, spending coins that do not all have the same tags, reducing what a lone recipient receives, and replacing the transactions that spend the change. When no coin can be added, several recipients pay in equal shares. hw still asks which recipient pays alone when equal shares would leave one below the dust limit."
              )
    }

-- | The transaction bumpfee replaces, in full or by its short id
bumpedArg :: Args HashRef
bumpedArg =
  argument hashRefReader $
    metavar "TXID"
      <> completer (mkCompleter idsToBump)
      <> help "The id of the transaction to replace, as txs or pendingtxs shows it: in full, or its short id, the first 8 digits"

-- | The fee rate of a replacement, when given, as for preparetx: it must be
-- above the rate of the transaction it replaces
replacementFeeOption :: Args (Maybe FeeRateChoice)
replacementFeeOption =
  optional . option (eitherReader readFeeRateChoice) $
    short 'f'
      <> long "fee-rate"
      <> metavar "RATE"
      <> completeWith ["low", "median", "high"]
      <> help "The fee rate of the replacement: a number of satoshis per virtual byte, or low, median or high for the rates that recent blocks paid, which `fees` shows. It must be above the rate of the transaction it replaces. Without it, hw shows those rates and asks for a level or a number. hw asks you to confirm a rate above 100."
