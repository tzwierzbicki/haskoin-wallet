{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of report, which writes a report as a PDF file, and
-- the readers of its options: a day, a price and a payment.
-- "Haskoin.Wallet.Commands.Report" runs it.
module Haskoin.Wallet.Cli.Parser.Report
  ( reportCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.String.Conversions (cs)
import Data.Time (Day, fromGregorianValid)
import Haskoin
import Haskoin.Wallet.Cli.Command (Command (..), ReportKind (..), ReportOptions (..))
import Haskoin.Wallet.Cli.Completion (tagNames, txidsToConfirm)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (fileArgument, hashRefReader, portfolioOptionWith, readNetwork, tagOption, tagReader, untaggedOption, walletOptionWith)
import Haskoin.Wallet.Database.Tag (TxTagFilter (..))
import Haskoin.Wallet.Report.Price (Price, readCurrency, readNetworkPrice)
import Haskoin.Wallet.ShortId (HashRef (..))
import Text.RawString.QQ (r)
import Text.Read (readMaybe)

-- | report, which writes a report as a PDF file. One command writes
-- every report, and an option says which: the reports share their file
-- argument, their holder, their prices and their style, and a command
-- for each would repeat them all. The option of the report is always
-- asked for, so that a command line written today means the same once
-- there are more reports.
--
-- The two statements take their dates with options of their own: --date
-- for the day of a balance statement, --from and --to for the period of
-- an account statement. One option for both would mean a day in one
-- report and the end of a period in the other.
--
-- The payment confirmation is of one transaction, so its option takes
-- the transaction: a second option to name it would have to be given
-- every time. It has three options of its own, for what the chain does
-- not tell: who the other side is (--counterparty), what the payment
-- was for (--purpose), and which of the addresses a transaction pays
-- the confirmation is about (--address).
reportCommand :: CommandSpec Command
reportCommand =
  CommandSpec
    { name = "report",
      offline = False,
      summary = "Write a report of the wallet as a PDF file",
      details =
        [r|
report writes a report to a new PDF file, to print or to hand to a tax office,
an accountant or the other side of a payment. There are three reports, and an
option says which one to write:

  --balances       a balance statement: what every portfolio of every wallet
                   held on a day, on each network, and what each of its
                   addresses held

  --transactions   an account statement: the transactions of every portfolio
                   over a period, with the balance after each, between an
                   opening and a closing balance

  --payment TXID   a payment confirmation: what one transaction paid, to
                   which addresses and on which day, with what lets anyone
                   check it on the chain

  hw report balances.pdf --balances --date 2025-12-31
  hw report 2025.pdf --transactions --from 2025-01-01 --to 2025-12-31
  hw report rent.pdf --payment a1075db5

hw works out all three from the transactions the wallet has synced, so run
sync first. All three count confirmed transactions only. The amounts are in
coins with eight decimals, whatever --unit says. A report names no key. Its
first page has the small logo of hw, and nothing else in it names hw.

This help has a part for each report. The parts after them are about what
all three share: the prices, the holder and the blocks, then one wallet or
one portfolio.

The balance statement:

The balance statement says what you held on one day. It is the declaration of
assets that a wealth tax asks for. It opens with a summary by asset, Bitcoin
and Bitcoin Cash. It then gives the balance of each portfolio, and the
balance of each address that holds coins, with its path and its label. Its
notes say how the balances were worked out.

Without --date, the statement is for now. With --date, it is for the end of
that day, on the clock of this computer. So --date 2025-12-31 gives what the
wallet held as the year 2025 ended. The day must be over.

For a day in the past, the statement counts the blocks dated on that day or
before. hw refuses the statement when the last sync stopped on that day or
before it, since the statement could miss transactions.

With --tag, the balance statement lists only the addresses that have a tag.
With --untagged, it lists only the addresses that have none. These are the
addresses that `balances` lists with the same options. Use them for the
statement of one group of coins, such as what the coins of a business held as
the year ended:

  hw report kyc.pdf --balances --tag kyc --date 2025-12-31

Run `balances` with the same options first. The statement lists the same
addresses, with what they held on its day. It uses the tags that the
addresses have now, even for a day in the past. Such a statement says which
addresses it lists, and its totals are those of these addresses alone. It
takes --price, as any balance statement does. It does not take --address-tag,
which goes with --transactions.

The account statement:

The account statement lists the transactions of a period, with the balance
after each. It is like the statement a bank sends each month, for a wallet
that no bank holds. Each portfolio on each network is one account. The
statement opens with a summary by asset and by portfolio. Then, for each
account, it gives the opening balance, each transaction with its date, what
the portfolio received or sent and the balance after it, and at the end the
totals and the closing balance.

Without --from and --to, the statement is for the current month, up to now.
--from gives the first day of the period and --to its last day, on the clock
of this computer:

  hw report 2025.pdf --transactions --from 2025-01-01 --to 2025-12-31

--from alone gives a period that runs up to now. --to alone gives the month of
that day, from its first day: --to 2025-10-31 gives October 2025. The day of
--to must be over.

The opening balance is the balance a balance statement gives for the day
before the period, and the closing balance the one it gives for the last day.
So the statements of two months in a row fit together: the first closes on
the balance that the second opens with.

Each line describes its transaction by its label. When the transaction has
none, the line uses the label of the contact it pays, or else the label of
the address that received it. The journal of exportjournal names its entries
the same way. Below the description are the txid and the block of the
transaction. An amount sent includes the fee, and the line states the fee
when the portfolio paid it.

A payment from one portfolio to another is a transfer. It is in both accounts,
sent by the first and received by the second, and each line names the other
portfolio. The amounts of an asset are what the portfolios received from
others and sent to others: they leave the transfer out, and count its fee as
sent. A row of the summary by portfolio states what they leave out.

A statement of some transactions:

With --tag, --address-tag or --untagged, the account statement lists only
some of the transactions: those that txs lists with the same options. Use
them for the payments of a business, for instance, to hand to the accountant
of that business:

  hw report business.pdf --transactions --tag business \
     --from 2025-01-01 --to 2025-12-31

--tag selects the transactions that have a tag. --address-tag selects those
that spend from or pay an address that has a tag, or a contact that has it.
--untagged selects those left to sort. When you give several of them, or one
of them several times, the statement lists the transactions that match any
one of them. Run txs with the same options first. Of the transactions that
txs lists, the statement has those that are confirmed and in its period, for
each portfolio.

Such a statement is titled a transaction statement, and says which
transactions it lists. It states no balance, since the transactions it leaves
out changed the balances too. It gives what its lines received and sent, and
the net amount, for each asset and each portfolio. It does not take --price,
since a price values a closing balance.

The payment confirmation:

The payment confirmation states what one transaction paid. It is like the
advice a bank prints for one transfer, for a wallet that no bank holds. Send
it to the person you paid, or file it with the invoice it settles. --payment
names the transaction, by its txid or its short id, as txs lists it:

  hw report rent.pdf --payment a1075db5 \
     --holder "Jane Doe" --counterparty "ACME Ltd" --purpose "Invoice 2025-014"

It states the day of the payment, each address paid with its amount, their
total, the fee, the txid, and the block of the transaction with its hash and
its confirmations. Its notes say how anyone can check the payment on the
chain.

For a payment you received, the report is a receipt instead. It lists what the
transaction paid to your addresses, and states no fee.

--holder names you, and --counterparty the other side: the recipient of a
payment you sent, the payer of one you received. --purpose says what the
payment is for. The report uses no label and no contact of the wallet, since
they are your private notes. It names only what these options give.

A transaction may pay several addresses. With --address, the confirmation
lists only what the transaction pays to that address. Give it once for each
address to list. The report then says that the transaction makes other
payments, and what they add up to.

The transaction must be in a block, since one that is not confirmed yet could
still be replaced. hw refuses a transaction between your own addresses, which
pays nobody. A payment from one of your portfolios to another counts as a
payment once --wallet or --portfolio names the side the report is for. Name
the portfolio that paid for a confirmation, or the one that was paid for a
receipt.

With --price and --currency, the report states the value of each amount. A
payment is in one asset, so it takes one price, that of its network.

Prices, the holder and the blocks:

hw knows no price and asks no server for one. To have a report state values,
give the price of a coin of each network with --price, and their currency with
--currency:

  hw report balances.pdf --balances --date 2025-12-31 \
     --price btc=87500 --price bch=420.50 --currency CHF \
     --price-source "Rate list of the tax administration, 31 December 2025"

The balance statement then states the value of each asset and their total, and
the value of each portfolio. The account statement states the value of each
closing balance and their total. The prices are those of the end of the
period, so it values no transaction. Every network the wallets hold coins on
needs a price. A value is the balance times the price, rounded to the
hundredth.

The title of a report says that it is about digital assets, under a line that
says cryptocurrency, so that nobody takes it for the statement of a bank
account.

--holder names who holds the wallets. Without it the report names nobody. A
report says that the holder prepared it, that no third party audited it, that
the wallets are self-custody wallets, and that it covers these wallets alone.

A report has a reference of eight digits. hw works it out from the date or
the period of the report, its holder, its balances or its transactions, and
its prices. For a payment confirmation, hw works it out from the transaction,
the names, the purpose, the amounts and the price. Two reports with the same
reference state the same thing.

A report names the blocks it was worked out from by their height and their
hash: the block the last sync reached, the last block it counts, and for an
account statement the last block before the period. hw asks the server for the
hashes it does not have. When the server does not answer, hw writes the
report all the same, and the report says that the hash is not available.

One wallet or one portfolio:

A report covers every portfolio of every wallet, on the networks hw shows,
whichever portfolio is active. --wallet narrows it down to the portfolios of
one wallet, and --portfolio to one portfolio:

  hw report business.pdf --balances --wallet 1
  hw report savings.pdf --transactions --portfolio 2

--portfolio alone names a portfolio of the wallet of the active portfolio.
With --wallet, it names a portfolio of that wallet. A report that is narrowed
down says so in its facts and in its notes, as in "wallet1 only". A portfolio
has the same balances in it as in the report of everything.

The file must not exist yet. hw makes it readable by you alone, since it
tells what you own.
|],
      args =
        (\file kind date from to wallet portfolio holder prices currency source counterparty purpose addresses kept -> CommandReport (ReportOptions file kind date from to wallet portfolio holder prices currency source counterparty purpose addresses kept))
          <$> fileArgument "The PDF file to write, such as balances.pdf. It must not exist yet."
          <*> ( flag' BalanceReport (long "balances" <> help "Write a balance statement: the balance of every portfolio and of every address that holds coins.")
                  <|> flag' AccountReport (long "transactions" <> help "Write an account statement: the transactions of every portfolio over a period, with the balance after each.")
                  <|> PaymentReport <$> reportPaymentOption
              )
          <*> reportDayOption "date" "With --balances: the statement is for the end of this day, on the clock of this computer. The day must be over. Without it, the statement is for now."
          <*> reportDayOption "from" "With --transactions: the first day of the period, on the clock of this computer. Without it, the first day of the month of --to, or of the current month."
          <*> reportDayOption "to" "With --transactions: the last day of the period, on the clock of this computer. The day must be over. Without it, the period runs up to now."
          <*> walletOptionWith "Report on this wallet only. Without it, every wallet, or with --portfolio, the wallet of the active portfolio. With --payment: the side the confirmation is for."
          <*> portfolioOptionWith "Report on this portfolio only, of the wallet --wallet names, or of the wallet of the active portfolio. Without it, every portfolio. With --payment: the side the confirmation is for."
          <*> optional (strOption (long "holder" <> metavar "NAME" <> help "Who holds the wallets, to name on the report: a name, with a tax number or an address if the reader needs one. Without it, the report names nobody. With --payment: the payer of a payment sent, the recipient of a payment received."))
          <*> many reportPriceOption
          <*> optional (option (eitherReader (readCurrency . cs)) (long "currency" <> metavar "CODE" <> help "The currency of the prices, by its code of three letters, such as CHF, EUR or USD. It goes with --price."))
          <*> optional (strOption (long "price-source" <> metavar "TEXT" <> help "Where the prices come from, to say on the report, such as the list of rates of your tax office. It goes with --price."))
          <*> optional (strOption (long "counterparty" <> metavar "NAME" <> help "With --payment: the other side of the payment, to name on the report. It is the recipient of a payment sent, and the payer of a payment received. Without it, the report does not name that side."))
          <*> optional (strOption (long "purpose" <> metavar "TEXT" <> help "With --payment: what the payment is for, to say on the report, such as the number of an invoice."))
          <*> many (strOption (long "address" <> metavar "ADDRESS" <> help "With --payment: confirm only what the transaction pays to this address. Give it once for each address to list. Without it, the report lists every address the payment pays."))
          <*> reportTagFilterOptions
    }

-- | The transaction a payment confirmation is of, with --payment: its
-- txid, in full or by its short id. The shell is offered the
-- transactions of the history.
reportPaymentOption :: Args HashRef
reportPaymentOption =
  option hashRefReader $
    long "payment"
      <> metavar "TXID"
      <> completer (mkCompleter txidsToConfirm)
      <> help "Write a payment confirmation: what this transaction paid, to which addresses and on which day. Give its txid as txs lists it, in full or by its short id, the first 8 digits. It must be in a block."

-- | The price of a coin of a network, with --price, given once for each
-- network. hw asks no server for a price: the user gives the one their
-- tax office asks for.
reportPriceOption :: Args (Network, Price)
reportPriceOption =
  option (eitherReader (readNetworkPrice readNetwork . cs)) $
    long "price"
      <> metavar "NETWORK=PRICE"
      <> help "The price of one coin of a network in the currency of --currency, such as btc=87500 or bch=420.50. Give it once for each network the wallets hold coins on. The report then states the value of each balance and their total. With --payment: the one price of the network of the payment."

-- | A day a report takes, with the option named: the day of a balance
-- statement with --date, and the first and the last day of the period
-- of an account statement with --from and --to. The command checks that
-- the day goes with the report and that it is over where it must be,
-- since the parser has no clock.
reportDayOption :: String -> String -> Args (Maybe Day)
reportDayOption name said =
  optional . option (eitherReader readReportDay) $
    long name
      <> metavar "YYYY-MM-DD"
      <> help said

-- | A day typed as its year, its month and its day, as in 2025-12-31:
-- the one way to write a date that no country reads differently. A day
-- the calendar does not have, such as 2025-02-30, is refused.
readReportDay :: String -> Either String Day
readReportDay typed =
  maybe (Left ("The date " <> typed <> " does not read: give a day as its year, its month and its day, such as 2025-12-31")) Right $ do
    let parts = splitOn "-" typed
    guard (map length parts == [4, 2, 2])
    [year, month, day] <- mapM digits parts
    fromGregorianValid (fromIntegral year) month day
  where
    -- A number made of digits alone: no sign, no space
    digits :: String -> Maybe Int
    digits part
      | all isDigit part = readMaybe part
      | otherwise = Nothing

-- | The --tag, --address-tag and --untagged options of report. An
-- account statement reads them as txs does, as the transactions to
-- keep. A balance statement reads --tag and --untagged as balances
-- does, as the addresses to keep. So one help text says both, where the
-- options of a listing say one.
reportTagFilterOptions :: Args TxTagFilter
reportTagFilterOptions =
  TxTagFilter
    <$> many (tagOption "With --transactions: only the transactions that have this tag. With --balances: only the addresses that have it. Repeat for several tags: the statement then includes what has any one of them. `tags` lists the tags.")
    <*> many
      ( option tagReader $
          long "address-tag"
            <> metavar "TAG"
            <> completer (mkCompleter tagNames)
            <> help "With --transactions: only the transactions that spend from or pay an address of the portfolio that has this tag, or that pay or were paid from a contact of the address book that has it. Repeat for several tags, and combine with --tag: the statement then includes the transactions that match any one of them."
      )
    <*> untaggedOption "With --transactions: only the transactions left to sort: those that have no tag, and that neither pay nor were paid from a contact of the address book that has one. With --balances: only the addresses that have no tag. With --tag or --address-tag, the statement includes those as well."
