{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the user asks of hw: a 'Command' with its options. The command line
-- is read into it ("Haskoin.Wallet.Cli.Parser"), and the commands run it
-- ("Haskoin.Wallet.Commands").
--
-- The types are apart from the parser, whose help texts and option readers
-- are most of the command line. A module that runs a command, or prints its
-- answer, needs the types alone.
--
-- The words a command reads by itself are here with their types: what
-- label and show take as their first argument ('readLabelled',
-- 'readShown'), and a coin ('readCoinRef').
module Haskoin.Wallet.Cli.Command where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, toLower)
import Data.List (find, stripPrefix)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (Day)
import Data.Word (Word32)
import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit)
import Haskoin.Wallet.Cli.Listing (ListingOrder (..), Paging (..))
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Database.Address (AddrType, AddressNamed, readAddressNamed, readTypedChainPath)
import Haskoin.Wallet.Database.Tag (TagFilter (..), TxTagFilter (..))
import Haskoin.Wallet.Keys (AccountKind (..), KeySource (..))
import Haskoin.Wallet.Report.Price (Price)
import Haskoin.Wallet.ShortId (CoinRef (..), HashRef (..), readHashRef)
import Haskoin.Wallet.Sync (SyncMode (..))
import Haskoin.Wallet.Tags (Tag, TagColor)
import Haskoin.Wallet.Tx.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Tx.FeeRates (FeeRateChoice)
import Haskoin.Wallet.Util (readNatural)
import Haskoin.Wallet.WalletFile (ImportScope (..))
import Numeric.Natural (Natural)

-- | A command of hw, with its options. Options that could be taken for one
-- another, being many or of the same type, come in a record of their own,
-- read by name; the others come as arguments of distinct types.
data Command
  = CommandNewPortfolio !NewPortfolio
  | CommandRecover !Recover
  | -- | Check a mnemonic against the active portfolio
    CommandCheckMnemonic !CheckMnemonic
  | -- | The sheet to write, to print it and write a mnemonic on it by hand
    CommandMnemonicSheet !MnemonicSheetOptions
  | -- | What of the file to import, when --wallet and --portfolio name
    -- one wallet and one portfolio of it, and the file
    CommandImportWallet !ImportScope !FilePath
  | -- | The one wallet to export, when --wallet names it, the one
    -- portfolio, when --portfolio names it, and the file to write.
    -- Without either, every portfolio of every wallet.
    CommandExportWallet !(Maybe Natural) !(Maybe Natural) !FilePath
  | -- | How to show each portfolio, or the CSV file to write them to
    CommandPortfolios !ListingOutput
  | -- | The wallet of the portfolio to make active, when --wallet names
    -- it, and the portfolio, when the argument or --portfolio names it.
    -- --wallet alone is portfolio0 of that wallet
    -- ('Haskoin.Wallet.Database.Portfolio.switchTo').
    CommandSwitch !(Maybe Natural) !(Maybe Natural)
  | CommandSetConfig !SetConfig
  | CommandShowConfig !ShowConfig
  | -- | The portfolio to delete, by its index, in the wallet of the
    -- active portfolio, and whether --yes answers the question that
    -- every command that deletes asks first
    CommandDeletePortfolio !Natural !Bool
  | -- | The wallet to delete, by its index, and whether --yes answers
    -- the question
    CommandDeleteWallet !Natural !Bool
  | CommandReceive !Receive
  | -- | Where to look, what of a file of a wallet to show, when --wallet
    -- and --portfolio name one wallet and one portfolio of it, and what
    -- to show, or Nothing for the newest receiving address of the account
    CommandShow !Target !ImportScope !(Maybe Shown)
  | -- | Where to look, whether the change addresses, which addresses to
    -- keep by their tags, how to show each address or the CSV file to
    -- write them to, and the page
    CommandAddresses !Target !Bool !TagFilter !ListingOutput !(Paging Text)
  | -- | Where to look, which addresses to keep by their tags, how to
    -- show each address or the CSV file to write them to, and the page
    CommandBalances !Target !TagFilter !ListingOutput !(Paging Text)
  | CommandLabel !Label
  | -- | The tags to put on an address or a transaction, or the tag to
    -- colour
    CommandTag !TagOptions
  | -- | The tags to take off an address or a transaction
    CommandUntag !Untag
  | -- | The tag to rename in the whole database, and its new name
    CommandRenameTag !RenameTag
  | -- | The tag to delete from the whole database, and whether --yes
    -- answers the question
    CommandDeleteTag !Tag !Bool
  | -- | List the tags of the database
    CommandTags
  | -- | The address of somebody else to add to the address book
    CommandNewContact !NewContact
  | -- | The one network to list the contacts of, or every one, which
    -- contacts to keep by their tags, how to show each contact or the
    -- CSV file to write them to, and the page
    CommandContacts !(Maybe Network) !TagFilter !ListingOutput !(Paging Text)
  | -- | The contact to delete from the address book, by its address,
    -- its network when the address book has the address on several, and
    -- whether --yes answers the question
    CommandDeleteContact !Text !(Maybe Network) !Bool
  | -- | The one network to list, or every one, which transactions to
    -- keep by their tags, how to show each transaction or the CSV file
    -- to write them to, and the page
    CommandTxs !(Maybe Network) !TxTagFilter !ListingOutput !(Paging HashRef)
  | CommandPrepareTx !PrepareTx
  | -- | The payment to prepare, sign and send in one command, with the
    -- options of preparetx. It writes no file, so the options name no
    -- --output file.
    CommandSend !PrepareTx
  | -- | The payment of all the coins to prepare, sign and send in one
    -- command, with the options of preparesendall. It writes no file, so
    -- the options name no --output file.
    CommandSendAll !PrepareSendAll
  | CommandPrepareSendAll !PrepareSendAll
  | -- | The one network to list, or every one, which transactions to
    -- keep by their tags, how to show each transaction or the CSV file
    -- to write them to, and the page
    CommandPendingTxs !(Maybe Network) !TxTagFilter !ListingOutput !(Paging HashRef)
  | -- | The transaction file
    CommandImportTx !FilePath
  | -- | The pending transaction, by either of its hashes, in full or by
    -- its short id, and the file to write it to
    CommandExportTx !HashRef !FilePath
  | -- | The pending transaction to delete, by either of its hashes, in
    -- full or by its short id, and whether --yes answers the question
    CommandDeleteTx !HashRef !Bool
  | -- | What to sign
    CommandSignTx !Signing
  | CommandFees !(Maybe Network)
  | CommandSignMsg !SignMsg
  | CommandVerifyMsg !VerifyMsg
  | -- | Where to look, whether the locked coins only, which coins to
    -- keep by the tags of their address, how to show each coin or the
    -- CSV file to write them to, and the page
    CommandCoins !Target !Bool !TagFilter !ListingOutput !(Paging CoinRef)
  | -- | The coin, and its network when the wallet has it on several
    CommandLockCoin !CoinRef !(Maybe Network)
  | CommandUnlockCoin !CoinRef !(Maybe Network)
  | -- | A pending transaction, by its txid or its nosigHash, in full or by
    -- its short id, or the file of a signed one, whether to send it without asking, and the network
    -- of a file that names none and that the wallet does not hold
    CommandBroadcast !(Maybe HashRef) !(Maybe FilePath) !Bool !(Maybe Network)
  | CommandBumpFee !BumpFee
  | -- | The one network to sync, or every one, and whether to sync from
    -- the start. Every portfolio is synced.
    CommandSync !(Maybe Network) !SyncMode
  | -- | The file of the copy, or none for the user-backups directory of hw
    CommandBackup !(Maybe FilePath)
  | CommandShowBackups
  | -- | The copy of the database to put in place of the database, and
    -- whether --yes answers the question of the command in advance
    CommandRestoreDatabase !FilePath !Bool
  | -- | The spreadsheet to write the listings of every portfolio to, and
    -- which rows to keep in each tab by their tags
    CommandExportSheet !FilePath !TagFilter
  | -- | The journal to write the transactions of every portfolio to, and
    -- which transactions to keep by their tags
    CommandExportJournal !FilePath !TxTagFilter
  | -- | The report to write, as a PDF file
    CommandReport !ReportOptions
  | -- | The sweep to prepare, sign and send in one command, with the
    -- options of preparesweep that it keeps. It writes no file, and every
    -- coin it takes is of a key given, so the options name no --output
    -- file, no address and no coin. Without a file of keys, the keys are
    -- typed on the terminal.
    CommandSweep !PrepareSweep
  | CommandPrepareSweep !PrepareSweep
  | -- | What to sign, the file of the private keys, typed on the terminal
    -- without one, and the network of a file that names none and involves
    -- no portfolio
    CommandSignSweep !Signing !(Maybe KeySource) !(Maybe Network)
  deriving (Eq, Show)

-- | The options of report: the PDF file to write, which report it holds,
-- and what the report is for. A balance statement is for a day, which
-- --date gives. An account statement is for a period, which --from and
-- --to give. Without them, the first is for the moment it is made, and
-- the second for the month that is running. A payment confirmation is
-- for one transaction, which its option names. The command refuses an
-- option that does not go with the report. --wallet and --portfolio
-- narrow a report down to one wallet or to one portfolio, as they
-- narrow the file of exportwallet. The other options say what hw cannot
-- know: who holds the wallets, what a coin is worth, and for a payment,
-- who the other side is and what it was for. --tag, --address-tag and
-- --untagged keep an account statement to some transactions, as they
-- keep the listing of txs to them.
data ReportOptions = ReportOptions
  { file :: !FilePath,
    kind :: !ReportKind,
    -- | The day a balance statement is for (--date)
    date :: !(Maybe Day),
    -- | The first day of the period of an account statement (--from)
    from :: !(Maybe Day),
    -- | The last day of the period of an account statement (--to)
    to :: !(Maybe Day),
    -- | The one wallet the report covers (--wallet), or with
    -- --portfolio, the wallet of that portfolio
    wallet :: !(Maybe Natural),
    -- | The one portfolio the report covers (--portfolio)
    portfolio :: !(Maybe Natural),
    -- | Who holds the wallets, to name on the report (--holder)
    holder :: !(Maybe Text),
    -- | The price of a coin of a network, for each --price
    prices :: ![(Network, Price)],
    -- | The currency of the prices (--currency), which goes with them
    currency :: !(Maybe Text),
    -- | Where the prices come from, to say on the report
    -- (--price-source)
    priceSource :: !(Maybe Text),
    -- | The other side of a payment, to name on its confirmation
    -- (--counterparty)
    counterparty :: !(Maybe Text),
    -- | What a payment is for, to say on its confirmation (--purpose)
    purpose :: !(Maybe Text),
    -- | The addresses a payment confirmation lists, as typed, for each
    -- --address. None lists every address the payment pays. Their
    -- network is that of the transaction, which the command finds.
    addresses :: ![Text],
    -- | The transactions an account statement keeps, by their tags
    -- (--tag, --address-tag and --untagged). The filter that keeps
    -- every transaction gives the statement of an account, with its
    -- balances. A balance statement reads --tag and --untagged as
    -- balances does, as the addresses to keep, and takes no
    -- --address-tag.
    kept :: !TxTagFilter
  }
  deriving (Eq, Show)

-- | A report of the command report. Each has an option that asks for
-- it, and the command takes exactly one of them.
data ReportKind
  = -- | What every portfolio held at a date, by network and by address
    -- (--balances)
    BalanceReport
  | -- | The transactions of every portfolio over a period, with the
    -- balance after each (--transactions)
    AccountReport
  | -- | The confirmation of the payment a transaction made, named by
    -- its txid in full or by its short id (--payment)
    PaymentReport !HashRef
  deriving (Eq, Show)

-- | Where a command works in the active portfolio, as its options say:
-- the account, by --network and --type. An option left out takes its
-- default from the configuration, or, for a listing, leaves the listing
-- whole: without --network or --type, addresses, balances and coins cover
-- the whole portfolio. The portfolio is the active one, which no option
-- names ("Haskoin.Wallet.Database.Portfolio").
data Target = Target
  { network :: !(Maybe Network),
    kind :: !(Maybe AccountKind)
  }
  deriving (Eq, Show)

-- | Where a listing goes: on the screen, in the view asked for, or to a
-- new CSV file with --csv, for a spreadsheet ("Haskoin.Wallet.Export.Csv").
-- On the screen, Nothing means that no option asked for a view: the
-- listing then takes the key listing-view of the configuration, which
-- the parser does not read ('viewOf'). A file is not a view: it has no
-- colours and no widths, and holds every row whole, more columns than a
-- table. The screen and the file are a choice, as the views are.
data ListingOutput = OnScreen !(Maybe View) | CsvFile !FilePath
  deriving (Eq, Show)

-- | The output a listing command asks for. Nothing for another command.
listingOutputOf :: Command -> Maybe ListingOutput
listingOutputOf (CommandPortfolios output) = Just output
listingOutputOf (CommandAddresses _ _ _ output _) = Just output
listingOutputOf (CommandBalances _ _ output _) = Just output
listingOutputOf (CommandTxs _ _ output _) = Just output
listingOutputOf (CommandPendingTxs _ _ output _) = Just output
listingOutputOf (CommandCoins _ _ _ output _) = Just output
listingOutputOf (CommandContacts _ _ output _) = Just output
listingOutputOf _ = Nothing

-- | The CSV file a listing command is asked to write, with --csv
csvFileOf :: Command -> Maybe FilePath
csvFileOf cmd = case listingOutputOf cmd of
  Just (CsvFile file) -> Just file
  _ -> Nothing

-- | The network the --network of the command names, when it has the
-- option and the user gave it. Every command but setconfig is refused a
-- hidden network there, before it runs, with the way to show the network
-- ('Haskoin.Wallet.Config.hiddenNetworkRefusal'). setconfig is not asked:
-- its --network is checked against the networks it writes, so that one
-- command shows a network and makes it the default. verifymsg is not
-- asked either: its --network says how to read the address of somebody
-- else, and the command reads nothing of the wallet.
networkNamedBy :: Command -> Maybe Network
networkNamedBy = \case
  CommandReceive opts -> opts.target.network
  CommandShow target _ _ -> target.network
  CommandAddresses target _ _ _ _ -> target.network
  CommandBalances target _ _ _ -> target.network
  CommandLabel opts -> opts.target.network
  CommandTag opts -> opts.target.network
  CommandUntag opts -> opts.target.network
  CommandNewContact opts -> opts.network
  CommandContacts netM _ _ _ -> netM
  CommandDeleteContact _ netM _ -> netM
  CommandTxs netM _ _ _ -> netM
  CommandPrepareTx opts -> opts.network
  CommandSend opts -> opts.network
  CommandSendAll opts -> opts.network
  CommandPrepareSendAll opts -> opts.network
  CommandPendingTxs netM _ _ _ -> netM
  CommandFees netM -> netM
  CommandSignMsg opts -> opts.network
  -- Reads anyone's address, on a hidden network too
  CommandVerifyMsg _ -> Nothing
  CommandCoins target _ _ _ _ -> target.network
  CommandLockCoin _ netM -> netM
  CommandUnlockCoin _ netM -> netM
  CommandBroadcast _ _ _ netM -> netM
  CommandSync netM _ -> netM
  CommandSweep opts -> opts.network
  CommandPrepareSweep opts -> opts.network
  CommandSignSweep _ _ netM -> netM
  -- Checked by setconfig itself
  CommandSetConfig _ -> Nothing
  -- Its --network asks for the default network, and names none
  CommandShowConfig _ -> Nothing
  -- The commands without a --network
  CommandNewPortfolio _ -> Nothing
  CommandRecover _ -> Nothing
  CommandCheckMnemonic _ -> Nothing
  CommandMnemonicSheet _ -> Nothing
  CommandImportWallet {} -> Nothing
  CommandExportWallet {} -> Nothing
  CommandPortfolios _ -> Nothing
  CommandSwitch _ _ -> Nothing
  CommandDeletePortfolio _ _ -> Nothing
  CommandDeleteWallet _ _ -> Nothing
  CommandImportTx _ -> Nothing
  CommandExportTx _ _ -> Nothing
  CommandDeleteTx _ _ -> Nothing
  CommandSignTx _ -> Nothing
  CommandBumpFee _ -> Nothing
  CommandBackup _ -> Nothing
  CommandShowBackups -> Nothing
  CommandRestoreDatabase _ _ -> Nothing
  CommandExportSheet _ _ -> Nothing
  CommandExportJournal _ _ -> Nothing
  CommandReport _ -> Nothing
  CommandRenameTag _ -> Nothing
  CommandDeleteTag _ _ -> Nothing
  CommandTags -> Nothing

-- | How the command shows each element it lists, as asked of portfolios,
-- addresses, balances, txs, pendingtxs and coins, or else in the listing
-- view given, the key listing-view of the configuration. The other
-- commands have one view, which the key does not change: it is about
-- listings. An answer with one element, such as the coin of lockcoin,
-- shows it with every detail whatever the view. A listing written to a CSV
-- file answers with the file, in the compact view.
viewOf :: View -> Command -> View
viewOf listingView cmd
  | Just (OnScreen asked) <- listingOutputOf cmd = fromMaybe listingView asked
viewOf _ _ = Compact

-- | Whether an address in an answer comes with its QR code, on a terminal
data AddressCode = WithCode | WithoutCode
  deriving (Eq, Show)

-- | A command that gives out an address shows its QR code, for a payer to
-- scan. label also answers with an address, which it does not give out.
addressCodeOf :: Command -> AddressCode
addressCodeOf (CommandReceive _) = WithCode
addressCodeOf (CommandShow {}) = WithCode
addressCodeOf _ = WithoutCode

-- | How the screen shows a transaction a command answers with: in full,
-- or by its id alone for one the command showed in full already, before
-- it asked for the secret. signtx and signsweep answer with the id of the
-- transaction they signed and the file it was written to, if any. send,
-- sendall and sweep answer with the id of the transaction they sent.
data TxShown = FullTx | SignedTx !(Maybe FilePath) | SentTx
  deriving (Eq, Show)

-- | How the screen lays out the words of a new mnemonic, and of each share
-- of a new backup. Numbered columns suit writing the words down on paper,
-- which is the safe way to keep them, so they are the default. A line of
-- words alone suits copying them into another program, where the numbers
-- would have to be deleted one by one.
data WordsLayout
  = -- | Four words on a line, each after its number
    NumberedColumns
  | -- | All the words on one line, a space between them, without numbers
    OneLine
  deriving (Eq, Show)

-- | newportfolio lays out the words of its new secret on one line with
-- --one-line. No other command shows a secret.
wordsLayoutOf :: Command -> WordsLayout
wordsLayoutOf (CommandNewPortfolio opts) | opts.oneLine = OneLine
wordsLayoutOf _ = NumberedColumns

-- | The file a signing command writes is its --output file, or the --input
-- file it replaces with the user's agreement
txShownOf :: Command -> TxShown
txShownOf (CommandSignTx signing) = SignedTx (signing.output <|> signing.input)
txShownOf (CommandSignSweep signing _ _) = SignedTx (signing.output <|> signing.input)
txShownOf (CommandSend _) = SentTx
txShownOf (CommandSendAll _) = SentTx
txShownOf (CommandSweep _) = SentTx
txShownOf _ = FullTx

-- | Whether the pager wraps the lines of the listing the command shows in
-- the view given, rather than cut them at the edge of the screen
-- ("Haskoin.Wallet.Cli.Pager"). Only portfolios in the verbose view wraps.
-- Its rows hold the output descriptor of each account, wider than
-- most screens, which the user copies into another wallet. A cut
-- descriptor could not be copied from the screen. Every other view keeps
-- its columns whole by cutting its lines.
wrapsLines :: View -> Command -> Bool
wrapsLines Verbose (CommandPortfolios _) = True
wrapsLines _ _ = False

-- | The options of newportfolio
data NewPortfolio = NewPortfolio
  { -- | Whether to ask for a mnemonic the user has already (--existing),
    -- instead of making a new one
    existing :: !Bool,
    -- | Whether --existing refuses a mnemonic that is of no wallet of the
    -- database, instead of creating its wallet (--match)
    match :: !Bool,
    -- | The bytes of entropy of a new mnemonic, when --entropy is given:
    -- 16 without it
    entropy :: !(Maybe Natural),
    -- | Whether dice rolls are mixed in
    dice :: !Bool,
    -- | The shares of a backup to make in place of a mnemonic
    shares :: !(Maybe Natural),
    -- | The number of shares that rebuild the seed
    threshold :: !(Maybe Natural),
    -- | Whether the words of the new secret are printed on one line,
    -- without numbers, to copy them (--one-line) ('WordsLayout')
    oneLine :: !Bool,
    -- | Whether the master private key of the new secret is printed
    -- after its words (--show-master-key)
    showMasterKey :: !Bool
  }
  deriving (Eq, Show)

-- | The options of recover
newtype Recover = Recover
  { -- | The portfolios in a row without a payment that end the search
    portfolioGap :: Natural
  }
  deriving (Eq, Show)

-- | The options of mnemonicsheet: the PDF file to write, and the secret
-- the sheet has lines for. The secret is told by the options of
-- newportfolio that make it, so that the user learns them once, and the
-- sheet fits the secret the same options give.
data MnemonicSheetOptions = MnemonicSheetOptions
  { file :: !FilePath,
    -- | The bytes of entropy of the secret, when --entropy is given
    entropy :: !(Maybe Natural),
    -- | The number of shares of a backup, for a page each, in place of
    -- the sheet of a mnemonic
    shares :: !(Maybe Natural),
    -- | The number of shares that rebuild the seed
    threshold :: !(Maybe Natural)
  }
  deriving (Eq, Show)

-- | The options of checkmnemonic
newtype CheckMnemonic = CheckMnemonic
  { -- | Whether the master private key of the wallet is printed, when the
    -- secret matches the portfolio (--show-master-key)
    showMasterKey :: Bool
  }
  deriving (Eq, Show)

-- | The options of setconfig: the settings to write to the configuration
-- file, each when its option is given. A default is set with the option
-- whose default it is, so that a user types what they would type to a
-- command for one run.
--
-- setconfig given no option at all ('noSettings') asks its user about
-- every setting instead, and is then run as the setconfig of the answers
-- that change something ("Haskoin.Wallet.Cli.SetConfig"). The last five
-- fields are settings that only these questions set: no command takes
-- them as an option for one run, so no option of setconfig names them.
data SetConfig = SetConfig
  { network :: !(Maybe Network),
    kind :: !(Maybe AccountKind),
    -- | The unit of the amounts. --unit goes with every command, so the
    -- parser of setconfig does not read it.
    -- 'Haskoin.Wallet.Cli.Parser.parserMain' takes it from the options of
    -- every command (@withUnitToSet@).
    unit :: !(Maybe AmountUnit),
    -- | The view of the listings, from --compact, --verbose or --tabular
    view :: !(Maybe View),
    -- | The order of the listings on the screen, from --reverse or
    -- --no-reverse
    listingOrder :: !(Maybe ListingOrder),
    -- | The hidden networks to show from now on, one for each
    -- --show-network
    showNetworks :: ![Network],
    -- | The networks to hide from now on, one for each --hide-network
    hideNetworks :: ![Network],
    -- | Whether hw keeps a journal for ledger next to the database from
    -- now on, from --journal or --no-journal
    journal :: !(Maybe Bool),
    -- | Whether hw copies the database by itself from now on
    autoBackupDatabase :: !(Maybe Bool),
    -- | Whether hw copies the labels by itself from now on
    autoBackupLabels :: !(Maybe Bool),
    -- | The address of the server hw asks about the chain
    host :: !(Maybe Text),
    -- | How many requests go to the server at once
    parallelRequests :: !(Maybe Natural),
    -- | The gap of BIP44
    gap :: !(Maybe Natural)
  }
  deriving (Eq, Show)

-- | setconfig given no option: no setting to write. The user is then
-- asked about every setting.
noSettings :: SetConfig
noSettings =
  SetConfig
    { network = Nothing,
      kind = Nothing,
      unit = Nothing,
      view = Nothing,
      listingOrder = Nothing,
      showNetworks = [],
      hideNetworks = [],
      journal = Nothing,
      autoBackupDatabase = Nothing,
      autoBackupLabels = Nothing,
      host = Nothing,
      parallelRequests = Nothing,
      gap = Nothing
    }

-- | The options of showconfig: what to show, each by a switch. The
-- switches of the settings are named after the options of setconfig
-- that set them, so that a user types the same word to set a default and
-- to see it: --network, --type and --unit. The view of the listings,
-- which setconfig sets with --compact, --verbose or --tabular, is asked
-- with --view, and the networks hw shows with --shown-networks, the name
-- of their key. --reverse asks whether the listings are reversed, which
-- setconfig sets with --reverse or --no-reverse. --journal asks whether
-- hw keeps a journal for ledger, which setconfig sets with --journal or
-- --no-journal. No switch at all asks for everything ('showsEverything'):
-- these seven settings, the five that setconfig sets by a question alone
-- and that have no switch, and the active portfolio.
--
-- The fields are named apart from those of t'SetConfig', so that a
-- record update of one is never ambiguous.
data ShowConfig = ShowConfig
  { askedNetwork :: !Bool,
    askedType :: !Bool,
    -- | --unit after showconfig is this switch, and not the --unit UNIT
    -- that every command takes
    -- ('Haskoin.Wallet.Cli.Parser.Config.showConfigCommand')
    askedUnit :: !Bool,
    askedView :: !Bool,
    askedReverse :: !Bool,
    askedShownNetworks :: !Bool,
    askedJournal :: !Bool,
    -- | The active portfolio, with its wallet
    askedPortfolio :: !Bool,
    -- | The wallet of the active portfolio alone
    askedWallet :: !Bool
  }
  deriving (Eq, Show)

-- | Whether showconfig was given no switch, and so shows every setting
-- and the active portfolio
showsEverything :: ShowConfig -> Bool
showsEverything opts =
  not (or [opts.askedNetwork, opts.askedType, opts.askedUnit, opts.askedView, opts.askedReverse, opts.askedShownNetworks, opts.askedJournal, opts.askedPortfolio, opts.askedWallet])

-- | The options of receive
data Receive = Receive
  { target :: !Target,
    label :: !(Maybe Text),
    -- | The tags of the new address (--tag), as its label is given with
    -- it
    tags :: ![Tag]
  }
  deriving (Eq, Show)

-- | The options of tag. One command puts tags on an address or on a
-- transaction, as label labels either, and sets a tag up: its colour,
-- and whether it locks its coins.
--
-- The words typed tell which. With several words, the first names what to
-- tag, as for label, and the others are the tags. With one word, there is
-- nothing to tag: the word is a tag, --color gives it a colour, and
-- --lock-coins and --unlock-coins say whether it locks its coins. The
-- place of a word decides what it is, not its shape, so a tag may look
-- like anything a tag is allowed to look like, a short id included.
data TagOptions = TagOptions
  { -- | The first word: what to tag when tags follow it, or else the tag
    -- to colour
    first :: !String,
    -- | The tags to put on what the first word names
    tags :: ![Tag],
    -- | The colour to give each tag, or Nothing in it to remove the
    -- colour (--color). Nothing when the option is not given: the tags
    -- keep the colours they have.
    color :: !(Maybe (Maybe TagColor)),
    -- | Whether the tag typed alone is to lock its coins from now on
    -- (--lock-coins): the payments then leave the coins at its addresses
    -- unless they name it
    lockCoins :: !Bool,
    -- | Whether the tag typed alone is to stop locking its coins
    -- (--unlock-coins). The two are two options and not one with a
    -- value, as lockcoin and unlockcoin are two commands. Giving both is
    -- refused by the command, with its reason.
    unlockCoins :: !Bool,
    -- | Whether the change down the chain of the payments of the address
    -- takes the tags too (--with-change)
    withChange :: !Bool,
    -- | The network of an address or a path the wallet has on several
    -- networks, and the type of a path typed without its type
    target :: !Target
  }
  deriving (Eq, Show)

-- | The options of renametag: the tag to rename, its new name, and
-- whether the new name may be that of another tag.
--
-- The two are tags already, read by the rules of a tag
-- ('Haskoin.Wallet.Tags.readTag'): a new name that is no tag is refused
-- before the command runs.
data RenameTag = RenameTag
  { -- | The tag to rename, as tags lists it
    oldTag :: !Tag,
    -- | Its new name
    newTag :: !Tag,
    -- | Whether to merge the tag into the tag that has the new name
    -- already (--merge), instead of refusing that name
    merge :: !Bool
  }
  deriving (Eq, Show)

-- | The options of untag: what to take tags off, as the word names it,
-- the tags, whether the change down the chain of the payments of the
-- address loses them too (--with-change), and where to look, as for tag
data Untag = Untag
  { tagged :: !String,
    tags :: ![Tag],
    withChange :: !Bool,
    target :: !Target
  }
  deriving (Eq, Show)

-- | The options of newcontact: the address of somebody else to keep in
-- the address book ("Haskoin.Wallet.Contacts")
data NewContact = NewContact
  { -- | The address, as typed
    address :: !Text,
    -- | Its label, the word after the address, when one is given
    label :: !(Maybe Text),
    -- | Its tags (--tag), as receive gives a new address its tags
    tags :: ![Tag],
    -- | The network of the contact, when the address reads on several
    network :: !(Maybe Network)
  }
  deriving (Eq, Show)

-- | The options of label. One command sets every label and description
-- the user gives, since they do one job: say what a thing is for, in the
-- user's words. The word before the label tells which thing: an address,
-- a path or a transaction ('readLabelled'), or the active portfolio or
-- its wallet, by the words portfolio and wallet ('readDescribed').
--
-- The word is never left out. label once took a word alone for the
-- description of the active portfolio. A user who forgot the label of an
-- address, and mistyped the address, then lost that description for the
-- word typed, and was told nothing. No rule on the shape of a word tells
-- a mistyped address from a description, so the description has a word
-- before it too.
data Label = Label
  { -- | What to label, as typed before the label
    labelled :: !String,
    -- | The new label or description. An empty one removes it. Nothing
    -- when it was left out: the parser lets it through, and label refuses
    -- it with what to type.
    label :: !(Maybe Text),
    -- | The network of an address or a path the wallet has on several
    -- networks, and the type of a path typed without its type
    target :: !Target
  }
  deriving (Eq, Show)

-- | What label describes, with a description in place of a label: the
-- active portfolio, or the wallet of the active portfolio
data Described = DescribedPortfolio | DescribedWallet
  deriving (Eq, Show)

-- | What a word typed for label describes: the active portfolio for the
-- word portfolio, and its wallet for the word wallet, in small letters or
-- in capitals. Nothing for any other word, a name with its index too,
-- such as portfolio0: label describes the active one only, and refuses
-- such a name ('indexedName').
readDescribed :: String -> Maybe Described
readDescribed word = case map toLower word of
  "portfolio" -> Just DescribedPortfolio
  "wallet" -> Just DescribedWallet
  _ -> Nothing

-- | The portfolio or the wallet that a word typed for label names with
-- its index, as the views write it: portfolio1 or wallet0. label takes
-- the bare words only ('readDescribed'), so it refuses such a name, and
-- says how to describe that one.
indexedName :: String -> Maybe Described
indexedName word = find named [DescribedPortfolio, DescribedWallet]
  where
    named described = case stripPrefix (describedWord described) (map toLower word) of
      Just index -> not (null index) && all isDigit index
      Nothing -> False

-- | The word that names it for label
describedWord :: Described -> String
describedWord DescribedPortfolio = "portfolio"
describedWord DescribedWallet = "wallet"

-- | What label labels, as the word before the label names it
data Labelled
  = -- | A transaction, by its txid, or while it is pending by either of
    -- its hashes, in full or by its short id
    LabelledTx !HashRef
  | -- | A receiving address, as typed
    LabelledAddress !Text
  | -- | The receiving address at a path of the active portfolio, \/0\/i,
    -- perhaps after the type of its account, as in bip84\/0\/3. A change
    -- path, \/1\/i, is read too, so that label can say why it refuses it.
    LabelledPath !(Maybe AccountKind) !AddrType !KeyIndex
  deriving (Eq, Show)

-- | What a word typed for label names, by its shape, as for show
-- ('readShown'). A path is made of digits and slashes, perhaps after its
-- type, and a word of that shape that is no path is refused with the
-- forms to write ('readTypedChainPath'). A hash has 64 hexadecimal
-- digits, and its short id 8, which no address has. Any other word is an
-- address, which the wallet looks for.
readLabelled :: String -> Either String Labelled
readLabelled word
  | Just path <- readTypedChainPath (cs word) = (\(kindM, addrType, index) -> LabelledPath kindM addrType index) <$> path
  | Just hash <- readHashRef (cs word) = Right (LabelledTx hash)
  | otherwise = Right (LabelledAddress (cs word))

-- | Whether a word reads as an address on one of the networks, its
-- checksum too. This is all hw knows of a word that names nothing else:
-- one that does not read as an address is told so, and not called an
-- address the wallet lacks, which would send the user to look for it in
-- another wallet. hw does not guess that such a word is a mistyped
-- address: no rule tells one from any other word.
readsAsAddress :: Text -> Bool
readsAsAddress typed = any (\net -> isJust (textToAddr net typed)) allNets

-- | What a word typed for label or tag names, when it has the shape of
-- something they work on: a path, a hash, or an address of one of the
-- networks. Nothing for any other word. A refusal uses it to say what
-- the word reads as: a word of such a shape typed alone, as in hw label
-- bip84\/0\/3, most likely had its label or its tags left out.
labelledShapeOf :: String -> Maybe String
labelledShapeOf word = case readLabelled word of
  Right (LabelledPath {}) -> Just "a path"
  Right (LabelledTx _) -> Just "the id of a transaction"
  Right (LabelledAddress typed) | readsAsAddress typed -> Just "an address"
  _ -> Nothing

-- | What show shows, as the word typed names it. One command takes the
-- five, so that the user need not know which command shows what. The word
-- tells. The shapes of the five do not overlap, so a word is never taken
-- for one kind when it was typed as another ('readShown').
data Shown
  = -- | The address at a path of a chain of an account, \/0\/i or \/1\/i,
    -- perhaps after the type of the account, as in bip84\/0\/i
    ShownPath !(Maybe AccountKind) !AddrType !KeyIndex
  | -- | An address of the wallet, as typed
    ShownAddress !Text
  | -- | A transaction, pending or on the chain, by its txid or, while it
    -- is pending, its nosigHash, in full or by its short id
    ShownTx !HashRef
  | -- | A coin, TXID:INDEX, the txid in full or by its short id
    ShownCoin !CoinRef
  | -- | A file, of which the wallet keeps nothing: a transaction file, a
    -- file of a wallet, or a database file. The bytes of the file tell
    -- which, once it is read, not the word.
    ShownFile !FilePath
  deriving (Eq, Show)

-- | What a word typed for show names, by its shape. The four kinds of the
-- wallet have strict shapes, and any other word names a file.
--
-- * A path is made of digits and slashes, such as \/0\/3, perhaps after the
--   type of its account, as the views write it: bip84\/0\/3. A word of
--   those, perhaps with the m, h and ' of a BIP32 path, is a path or is
--   refused as one ('readTypedChainPath'). So 0\/3 and m\/0\/3 are told how
--   to write a path, and so is bip49\/0\/3, a type hw holds no account of.
-- * A coin has a colon after 8 or 64 hexadecimal digits.
-- * A hash has 8 or 64 hexadecimal digits and nothing else, as for label.
-- * An address reads as an address on one of the networks. Its checksum
--   tells it from a file name.
-- * Every other word names a file, such as unsigned.psbt or
--   .\/unsigned.psbt. A file whose name has one of the other shapes goes
--   with its directory, as .\/a1075db5.
--
-- A word that is none of the five, such as a short id of 7 digits, is
-- taken for a file. show then names the five kinds when no file has that
-- name.
readShown :: String -> Either String Shown
readShown word
  | Just path <- readTypedChainPath (cs word) = (\(kindM, addrType, index) -> ShownPath kindM addrType index) <$> path
  | Just coin <- readCoinRef word = Right (ShownCoin coin)
  | Just hash <- readHashRef (cs word) = Right (ShownTx hash)
  | readsAsAddress (cs word) = Right (ShownAddress (cs word))
  | otherwise = Right (ShownFile word)

-- | A coin, TXID:INDEX, with the txid in full or by its short id. Nothing
-- for any other word.
readCoinRef :: String -> Maybe CoinRef
readCoinRef s = case break (== ':') s of
  (hash, ':' : index) -> CoinRef <$> readHashRef (cs hash) <*> readOutputIndex index
  _ -> Nothing

-- | The index of an output in its transaction, which has 32 bits. A larger
-- number names no output, rather than wrapping around to another one.
readOutputIndex :: String -> Maybe Word32
readOutputIndex s = case readNatural (cs s) of
  Just n | n <= fromIntegral (maxBound :: Word32) -> Just (fromIntegral n)
  _ -> Nothing

-- | Where --change-back of preparetx sends the change
data ChangeBack
  = -- | To the address of the largest coin the payment spends, of the
    -- type of the change (--change-back largest). The user cannot know
    -- in advance which coins the strategy selects, so this form names an
    -- address the payment spends from without knowing it.
    BackToLargestCoin
  | -- | To the address of the smallest coin the payment spends, of the
    -- type of the change (--change-back smallest): the mirror of the
    -- form above
    BackToSmallestCoin
  | -- | To the address of the portfolio named, by itself or by its path
    BackToNamed !AddressNamed
  deriving (Eq, Show)

-- | The word --change-back reads: largest or smallest, or else an address
-- or a path. Neither word can be taken for an address, which is far
-- longer, nor for a path, which has slashes. Each is a keyword like the
-- low of --fee-rate, and not a bare
-- --change-back: optparse-applicative takes the word after an option as
-- its value, so a bare --change-back would swallow the next option or
-- recipient.
--
-- A tag is not read here. The change stays in the group of a tag with
-- --tag-change: a new change address then takes the tags of the coins
-- spent ('Haskoin.Wallet.Database.PendingTx.changeTakesTags').
readChangeBack :: String -> Either String ChangeBack
readChangeBack word
  | word == "largest" = Right BackToLargestCoin
  | word == "smallest" = Right BackToSmallestCoin
  | otherwise = BackToNamed <$> readAddressNamed word

-- | The options of preparetx. send takes the same ones, but --output: it
-- builds the same payment from them, and writes no file.
data PrepareTx = PrepareTx
  { -- | The addresses paid, each with its amount, as typed: ADDRESS:AMOUNT
    recipients :: ![Text],
    -- | The network of the payment, which the active portfolio pays from
    network :: !(Maybe Network),
    -- | The fee rate: a number of satoshis per virtual byte or a level of the
    -- rates recent blocks paid, or none to ask the user from those rates
    feeRate :: !(Maybe FeeRateChoice),
    feePayer :: !FeePayer,
    -- | A filter of coin control: the confirmations a coin needs to be
    -- spent (--min-conf)
    minConf :: !Natural,
    -- | A filter of coin control: the one type of coins the payment may
    -- spend (--coin-type), or any. It narrows the coins down, and never
    -- adds one.
    coinType :: !(Maybe AccountKind),
    strategy :: !CoinSelection,
    -- | The file the transaction is also written to (--output). send has
    -- no such option, and gives none.
    output :: !(Maybe FilePath),
    -- | The label of the transaction (--label)
    label :: !(Maybe Text),
    -- | The tags of the transaction (--tag), put on it as the command
    -- tag puts them. They say what the payment is for, and choose no coin:
    -- --spend-from-tag does that.
    tags :: ![Tag],
    -- | A chooser of coin control: the coins chosen by their outpoints, in
    -- full or by their short ids (--coin). They add up with those at the
    -- addresses of --spend-from.
    coins :: ![CoinRef],
    -- | A chooser of coin control: the addresses whose coins are chosen
    -- (--spend-from), by themselves or by their paths. They add up with
    -- the coins of --coin.
    spendFrom :: ![AddressNamed],
    -- | A chooser of coin control: the tags whose addresses have their
    -- coins chosen (--spend-from-tag). They add up with the coins of the
    -- other choosers.
    spendFromTags :: ![Tag],
    -- | Whether the strategy selects among the chosen coins (--select),
    -- instead of every one of them being spent
    select :: !Bool,
    -- | Whether a fee rate above 100 satoshis per virtual byte is taken, a
    -- wallet behind the chain pays, and coins that do not all have the
    -- same tags are spent together, without asking (--yes)
    yes :: !Bool,
    -- | Where the change goes instead of a new change address
    -- (--change-back): an address of the portfolio, by itself or by its
    -- path, or that of the largest or of the smallest coin spent
    changeBack :: !(Maybe ChangeBack),
    -- | The type of the change (--change-type), when the command says:
    -- that of the new change address, or of the coin --change-back
    -- largest or smallest picks. Otherwise the coins spent say.
    changeType :: !(Maybe AccountKind),
    -- | Whether the new change address takes the tags that every coin
    -- spent has (--tag-change)
    tagChange :: !Bool
  }
  deriving (Eq, Show)

-- | The options of bumpfee
data BumpFee = BumpFee
  { -- | The transaction to replace, by its id, in full or by its short id
    bumped :: !HashRef,
    -- | The fee rate of the replacement, as for preparetx
    feeRate :: !(Maybe FeeRateChoice),
    -- | Who pays what the change cannot: the sender, with coins of the
    -- portfolio it agrees to add or the recipients it picks, or all the
    -- recipients in equal shares, with no question (--recipients-pay)
    recipientsPay :: !FeePayer,
    output :: !(Maybe FilePath),
    -- | Whether every question is answered yes (--yes)
    yes :: !Bool
  }
  deriving (Eq, Show)

-- | The options of preparesendall. sendall takes the same ones, but
-- --output: it builds the same transaction from them, and writes no file.
data PrepareSendAll = PrepareSendAll
  { -- | The recipients as typed: an address, or an address and its amount,
    -- ADDRESS:AMOUNT
    recipients :: ![Text],
    -- | The network of the payment, which the active portfolio pays from
    network :: !(Maybe Network),
    -- | The fee rate: a number of satoshis per virtual byte or a level of the
    -- rates recent blocks paid, or none to ask the user from those rates
    feeRate :: !(Maybe FeeRateChoice),
    -- | A filter of coin control: the confirmations a coin needs to be
    -- sent (--min-conf)
    minConf :: !Natural,
    -- | A filter of coin control: the one type of coins to send
    -- (--coin-type), or both. It narrows the coins down, and never adds
    -- one.
    coinType :: !(Maybe AccountKind),
    -- | The file the transaction is also written to (--output). sendall
    -- has no such option, and gives none.
    output :: !(Maybe FilePath),
    -- | The label of the transaction (--label)
    label :: !(Maybe Text),
    -- | The tags of the transaction (--tag), as for preparetx
    tags :: ![Tag],
    -- | A chooser of coin control: the coins to send, instead of all of
    -- them, in full or by their short ids (--coin). They add up with those
    -- at the addresses of --spend-from.
    coins :: ![CoinRef],
    -- | A chooser of coin control: the addresses whose coins to send,
    -- instead of all of them, by themselves or by their paths
    -- (--spend-from). They add up with the coins of --coin.
    spendFrom :: ![AddressNamed],
    -- | A chooser of coin control: the tags whose addresses have their
    -- coins sent, instead of all of them (--spend-from-tag). They add up
    -- with the coins of the other choosers.
    spendFromTags :: ![Tag],
    -- | What to do with the coins worth less than their fee, or Nothing to
    -- ask the user
    dustCoins :: !(Maybe DustCoins),
    -- | Whether a fee rate above 100 satoshis per virtual byte is taken, a
    -- wallet behind the chain pays, and coins that do not all have the
    -- same tags are sent together, without asking (--yes)
    yes :: !Bool
  }
  deriving (Eq, Show)

-- | What preparesendall, sendall and preparesweep do with the coins worth
-- less than the fee to spend them
data DustCoins = SpendDust | LeaveDust
  deriving (Eq, Show)

-- | The options of signmsg
data SignMsg = SignMsg
  { -- | The address of the wallet that signs, by itself or by its path
    address :: !AddressNamed,
    message :: !Text,
    -- | The file the armored block is written to, instead of the screen
    output :: !(Maybe FilePath),
    -- | The network of an address or a path the wallet has on several
    -- networks
    network :: !(Maybe Network)
  }
  deriving (Eq, Show)

-- | What verifymsg checks: the address the message is said to be signed
-- from, the signature in base64 and the message, given as they are or in a
-- file holding the output of signmsg
data SignedMessageSource
  = Given !Text !Text !Text
  | FromFile !FilePath
  deriving (Eq, Show)

-- | The options of verifymsg
data VerifyMsg = VerifyMsg
  { source :: !SignedMessageSource,
    -- | The network the address is read on, when given. Without it, any
    -- network hw shows that reads the address, which gives the same answer.
    network :: !(Maybe Network)
  }
  deriving (Eq, Show)

-- | The options of preparesweep, and those of sweep, which names no
-- address to sweep from, no coin and no --output file
data PrepareSweep = PrepareSweep
  { -- | The network of a sweep that pays no portfolio of the wallet
    network :: !(Maybe Network),
    -- | The private keys of the addresses swept, if at hand. sweep always
    -- reads them: on the terminal when no source is given.
    keys :: !(Maybe KeySource),
    sweepFrom :: ![Text],
    -- | A file of more addresses to sweep from, for more of them than the
    -- command line takes
    addressFile :: !(Maybe FilePath),
    -- | The destinations as typed: an address, or an address and its
    -- amount, ADDRESS:AMOUNT
    sweepTo :: ![Text],
    -- | The coins swept, by outpoint
    coins :: ![OutPoint],
    -- | What to do with the coins worth less than their fee, or Nothing to
    -- ask the user
    dustCoins :: !(Maybe DustCoins),
    output :: !(Maybe FilePath),
    -- | The label of the sweep (--label)
    label :: !(Maybe Text),
    -- | The fee rate, as for preparetx
    feeRate :: !(Maybe FeeRateChoice),
    -- | Whether a fee rate above 100 satoshis per virtual byte is taken without
    -- asking (--yes). For sweep, also whether the sweep is signed and sent
    -- without asking.
    yes :: !Bool
  }
  deriving (Eq, Show)

-- | What signtx and signsweep sign, and where the signed transaction goes:
-- a pending transaction by its nosigHash, or an --input file signed to an
-- --output file. The transaction tells its portfolio.
data Signing = Signing
  { nosigHash :: !(Maybe HashRef),
    input :: !(Maybe FilePath),
    output :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- | A pending transaction to sign, named by its nosigHash in full, with
-- no file to read or to write: what signtx NOSIGHASH signs. send and
-- sendall have their own transaction signed this way.
signingPending :: TxHash -> Signing
signingPending hash = Signing {nosigHash = Just (FullHash hash), input = Nothing, output = Nothing}
