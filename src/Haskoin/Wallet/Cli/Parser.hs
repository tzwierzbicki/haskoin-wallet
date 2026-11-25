{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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
module Haskoin.Wallet.Cli.Parser where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (guard)
import Data.Char (isDigit, toLower)
import Data.List (find, intercalate, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, fromGregorianValid)
import Data.Version (showVersion)
import Data.Word (Word32)
import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit, readUnit)
import Haskoin.Wallet.Tx.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Cli.Completion
  ( RecipientForm (..),
    activeAddresses,
    addressesToSpendFrom,
    contactAddresses,
    coinsToLock,
    coinsToSpend,
    coinsToUnlock,
    hashesToSign,
    hiddenNetworkNames,
    idsToBump,
    idsToSend,
    labelTargets,
    pendingHashes,
    portfolioIndexes,
    recipientContacts,
    shownNetworkNames,
    shownTargets,
    tagNames,
    tagTargets,
    txidsToConfirm,
    untagTargets,
    walletAddresses,
    walletIndexes,
  )
import Haskoin.Wallet.Database.Address (AddrType, AddressNamed, readAddressNamed, readTypedChainPath)
import Haskoin.Wallet.Database.Tag (TagFilter (..), TxTagFilter (..))
import Haskoin.Wallet.Tx.FeeRates (FeeRateChoice, readFeeRateChoice)
import Haskoin.Wallet.Keys (AccountKind (..), KeySource (..), indexFits, kindNamed, kindNames, portfolioNetworks)
import Haskoin.Wallet.WalletFile (ImportScope (..))
import Haskoin.Wallet.Database.Migration (latestDatabaseVersion)
import Haskoin.Wallet.Report.Price (Price, readCurrency, readNetworkPrice)
import Haskoin.Wallet.ShortId (CoinRef (..), HashRef (..), readHashRef)
import Haskoin.Wallet.Sync (SyncMode (..))
import Haskoin.Wallet.Testnet3 (isDeprecatedNetwork)
import Haskoin.Wallet.Tags (Tag, TagColor, colorName, noColorWord, readColorChoice, readTag, tagColors)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Completion (completeFile)
import Haskoin.Wallet.Cli.Parser.Run (runProgram)
import Haskoin.Wallet.Cli.Listing (ListingOrder (..), Paging (..))
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Util (readNatural)
import Numeric.Natural (Natural)
import qualified Paths_haskoin_wallet as Paths
import Text.RawString.QQ (r)
import Text.Read (readMaybe)

{- Commands -}

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
  | CommandSetDefault !SetDefault
  | CommandShowDefault !ShowDefault
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
-- new CSV file with --csv, for a spreadsheet ("Haskoin.Wallet.Cli.Csv").
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
-- option and the user gave it. Every command but setdefault is refused a
-- hidden network there, before it runs, with the way to show the network
-- ('Haskoin.Wallet.Config.hiddenNetworkRefusal'). setdefault is not asked:
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
  -- Checked by setdefault itself
  CommandSetDefault _ -> Nothing
  -- Its --network asks for the default network, and names none
  CommandShowDefault _ -> Nothing
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

-- | The options of setdefault: the defaults to write to the configuration
-- file, each when its option is given. Each default is set with the option
-- whose default it is, so that a user types what they would type to a
-- command for one run.
data SetDefault = SetDefault
  { network :: !(Maybe Network),
    kind :: !(Maybe AccountKind),
    -- | The unit of the amounts. --unit goes with every command, so the
    -- parser of setdefault does not read it. 'parserMain' takes it from
    -- the options of every command ('withUnitToSet').
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
    journal :: !(Maybe Bool)
  }
  deriving (Eq, Show)

-- | The options of showdefault: what to show, each by a switch. The
-- switches of the defaults are named after the options of setdefault
-- that set them, so that a user types the same word to set a default and
-- to see it: --network, --type and --unit. The view of the listings,
-- which setdefault sets with --compact, --verbose or --tabular, is asked
-- with --view, and the networks hw shows with --shown-networks, the name
-- of their key. --reverse asks whether the listings are reversed, which
-- setdefault sets with --reverse or --no-reverse. --journal asks whether
-- hw keeps a journal for ledger, which setdefault sets with --journal or
-- --no-journal. No switch at all asks for everything ('showsEverything').
--
-- The fields are named apart from those of t'SetDefault', so that a
-- record update of one is never ambiguous.
data ShowDefault = ShowDefault
  { askedNetwork :: !Bool,
    askedType :: !Bool,
    -- | --unit after showdefault is this switch, and not the --unit UNIT
    -- that every command takes ('showDefaultCommand')
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

-- | Whether showdefault was given no switch, and so shows every default
-- and the active portfolio
showsEverything :: ShowDefault -> Bool
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
-- ("Haskoin.Wallet.Tags".'readTag'): a new name that is no tag is refused
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

-- | What signtx and signsweep sign
signingOptions :: Args Signing
signingOptions =
  Signing
    <$> optional nosigHashArg
    <*> inputFileMaybeOption
    <*> outputFileMaybeOption

{- The program -}

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

{- newportfolio -}

-- | newportfolio, which makes a mnemonic and its wallet, or adds a
-- portfolio to a wallet
newPortfolioCommand :: CommandSpec Command
newPortfolioCommand =
  CommandSpec
    { name = "newportfolio",
      offline = True,
      summary = "Make a mnemonic and its wallet, or add a portfolio to a wallet",
      details =
        [r|
newportfolio makes a new mnemonic and creates its wallet, with portfolio0.
With --existing, it asks for a mnemonic you have already, and adds a
portfolio to its wallet. Either way, hw never writes the mnemonic or a
private key to disk. It stores only the public keys of the portfolio.

For a new mnemonic, newportfolio first asks for a passphrase, and asks twice
to catch a typo. Leave it empty if you want none. A mnemonic with a
passphrase gives another master key, so it is another wallet. The passphrase
is part of the secret, and you must keep it too.

It then prints the mnemonic, once, with the portfolio it created. Write the
mnemonic down and keep copies in safe places, away from computers and phones.
Whoever has the mnemonic can spend the funds. If you lose it, the funds are
lost. checkmnemonic checks what you wrote down against the portfolio.

newportfolio also writes a sheet to print, to write the new mnemonic on by
hand, and names the file. The sheet is a PDF file in ~/.hw/mnemonic-sheets,
with a numbered line for each word, and the fingerprint of the new wallet. It
holds no word of the mnemonic. With --shares, it has a page for each share.
mnemonicsheet writes such a sheet ahead of time, to have it printed before
the mnemonic is on the screen.

newportfolio numbers the words and prints four on a line, to write them
down. With --one-line, it prints them on one line, without numbers, to copy
them into another program. A share of a backup gets a line of its own. A
copied mnemonic passes through the clipboard, where other programs can read
it.

With --show-master-key, newportfolio also prints the master private key of
the new wallet, which starts with xprv. The commands that ask for the
mnemonic take this key in place of the mnemonic and the passphrase, on one
line of their input, so that a script can sign with it. Whoever has the key
can spend the funds of the wallet. It is not a backup: it does not give the
mnemonic back, so write the mnemonic down all the same.

The entropy comes from the operating system, read from /dev/random where the
machine has it. With --dice, hw also mixes in rolls of a die, so that you do
not have to trust the system alone. Run this command on an offline computer
if you can.

With --shares, hw prints the shares of a backup instead of a mnemonic, in the
SLIP-39 format. Any --threshold of the shares rebuild the seed, and fewer
reveal nothing about it. With --shares 3 --threshold 2, any two of the three
shares rebuild it. A share is a list of 20 words, or of 33 with --entropy 32.
A single share, with --shares 1, is the whole backup, as a mnemonic is.

The commands that ask for the mnemonic take the shares in its place. Type one
share, and hw asks for the next one until it has enough. A backup in shares
is not a BIP39 mnemonic, and cannot be turned into one. A wallet that reads
SLIP-39 shares restores it, and one that reads only BIP39 mnemonics cannot.
The passphrase of a backup in shares has printable ASCII characters only, as
the format asks.

With --existing, newportfolio asks for a mnemonic you have already, and its
passphrase, each twice. It takes the shares of a backup in place of the
mnemonic. It then adds a portfolio to the wallet of that mnemonic, at the
lowest free index. That is portfolio1 when the wallet has portfolio0, and
portfolio1 too when it has portfolio0 and portfolio2. When the database has
no wallet for the mnemonic yet, newportfolio creates the wallet with its
portfolio0.

A passphrase typed with a mistake, twice the same, gives another wallet.
When --existing creates a wallet in a database that has one already, it
says that the mnemonic and passphrase are those of no wallet there, and
gives the command that deletes the new wallet. With --match, it refuses
such a mnemonic and creates nothing. Give --match when you mean to add a
portfolio to a wallet the database has.

The first portfolio of a database becomes the active one, where the commands
work. A portfolio added later does not. newportfolio then says which
portfolio is still active, and switch makes the new one active.

A portfolio is created whole. It gets an account for every network, the test
networks included, and for both types of address. These accounts have five
keys:

  m/44'/0'/i'     bip44   btc
  m/84'/0'/i'     bip84   btc
  m/44'/145'/i'   bip44   bch
  m/44'/1'/i'     bip44   every test network
  m/84'/1'/i'     bip84   the test networks of Bitcoin

The test networks share their keys, and hw keeps their addresses and coins
apart. You never create an account. The options --network and --type of a
command, such as `receive`, choose the account.

To keep the private keys on an offline computer, run newportfolio there. Then
copy the wallet to an online computer with exportwallet and importwallet.

hw normalizes a passphrase with accents or other non-ASCII letters as BIP39
asks, so that other wallets find the same seed.
|],
      args =
        CommandNewPortfolio
          <$> (NewPortfolio <$> existingOption <*> matchOption <*> entropyOption <*> diceOption <*> sharesOption newSharesHelp <*> thresholdOption newThresholdHelp <*> oneLineOption <*> showMasterKeyOption newMasterKeyHelp)
    }

-- | Whether newportfolio asks for a mnemonic the user has already
existingOption :: Args Bool
existingOption =
  switch $
    long "existing"
      <> help "Ask for a mnemonic you have already, and add a portfolio to its wallet, instead of making a new mnemonic"

-- | Whether newportfolio --existing refuses a mnemonic that is of no
-- wallet of the database. Without it, such a mnemonic gets a new wallet,
-- which is what a user who brings a mnemonic to this database wants, and
-- not what one who mistyped the passphrase of a wallet wants. Only the
-- user knows which, so this is how to say it before the command runs.
matchOption :: Args Bool
matchOption =
  switch $
    long "match"
      <> help "With --existing, refuse a mnemonic that is of no wallet of the database, instead of creating a new wallet"

-- | --entropy of newportfolio: the bytes of entropy of the new mnemonic,
-- which decide how many words it has. mnemonicsheet takes it too, for the
-- sheet of that mnemonic.
entropyOption :: Args (Maybe Natural)
entropyOption =
  optional . option (maybeReader f) $
    short 'e'
      <> long "entropy"
      <> metavar "BYTES"
      <> completeWith valid
      <> help
        [r|
The number of bytes of entropy: 16, 20, 24, 28 or 32, for a mnemonic of 12,
15, 18, 21 or 24 words. A backup in shares takes 16 or 32, for shares of 20
or 33 words. The default is 16.
|]
  where
    valid = ["16", "20", "24", "28", "32"]
    f s
      | s `elem` valid = fromIntegral <$> readNatural (cs s)
      | otherwise = Nothing

-- | --dice of newportfolio: rolls of a die, mixed into the entropy of the
-- system
diceOption :: Args Bool
diceOption =
  switch $
    short 'd'
      <> long "dice"
      <> help
        [r|
Mix rolls of a six-sided die into the entropy of the system. hw asks for the
rolls. Type the faces, as many on a line as you like. hw does not show what
you type, and says after each line how many rolls are left. With --shares, hw
asks for enough rolls for the seed and for each random value that splits it,
and uses no roll twice.
|]

-- | --shares: the number of shares of a backup, in place of a mnemonic,
-- with the help of the command about it. newportfolio makes the shares,
-- and mnemonicsheet a page to write each of them on.
sharesOption :: String -> Args (Maybe Natural)
sharesOption about =
  optional . option (eitherReader $ upToSixteen "--shares" . cs) $
    short 's'
      <> long "shares"
      <> metavar "INT"
      <> help about

-- | What --shares does in newportfolio
newSharesHelp :: String
newSharesHelp =
  [r|
Make a backup in INT shares, from 1 to 16, in place of a mnemonic. With more
than one share, --threshold gives how many of them are needed to rebuild the
seed.
|]

-- | What --shares does in mnemonicsheet
sheetSharesHelp :: String
sheetSharesHelp =
  [r|
Write a page for each of the INT shares of a backup, from 1 to 16, in place of
the sheet of a mnemonic. With more than one share, --threshold gives how many
of them are needed to rebuild the seed.
|]

-- | --threshold: the number of shares that rebuild the seed of a backup,
-- with the help of the command about it
thresholdOption :: String -> Args (Maybe Natural)
thresholdOption about =
  optional . option (eitherReader $ upToSixteen "--threshold" . cs) $
    long "threshold"
      <> metavar "INT"
      <> help about

-- | What --threshold does in newportfolio
newThresholdHelp :: String
newThresholdHelp =
  [r|
The number of shares needed to rebuild the seed, from 2 to the number of
shares. Fewer shares reveal nothing about it.
|]

-- | What --threshold does in mnemonicsheet
sheetThresholdHelp :: String
sheetThresholdHelp =
  [r|
The number of shares needed to rebuild the seed, from 2 to the number of
shares. Each page says it.
|]

-- | Whether newportfolio prints the words of the new secret on one line.
-- It has no short letter. It is rare, and a letter keeps one meaning
-- across commands.
oneLineOption :: Args Bool
oneLineOption =
  switch $
    long "one-line"
      <> help
        [r|
Print the words of the new mnemonic on one line, without numbers, to copy
them. With --shares, each share is on a line of its own.
|]

-- | Whether a command prints the master private key of the secret it has
-- in hand, with the help of the command about it. hw stores no key, so a
-- signer that must sign without anyone typing keeps this key in a file of
-- its own and gives it to the commands that ask for the mnemonic. This
-- option is where that key comes from: no other tool has to see the
-- mnemonic to derive it. newportfolio takes it for the secret it makes,
-- and checkmnemonic for the secret of a wallet made before. It has no
-- short letter, so that nobody shows the key by a slip of the finger.
showMasterKeyOption :: String -> Args Bool
showMasterKeyOption about = switch $ long "show-master-key" <> help about

-- | What --show-master-key does in newportfolio
newMasterKeyHelp :: String
newMasterKeyHelp =
  [r|
Also print the master private key of the new wallet, which starts with xprv.
The commands that ask for the mnemonic take it in place of the mnemonic and
the passphrase. Whoever has it can spend the funds of the wallet.
|]

-- | What --show-master-key does in checkmnemonic
checkedMasterKeyHelp :: String
checkedMasterKeyHelp =
  [r|
Also print the master private key of the wallet, which starts with xprv, when
the mnemonic and the passphrase match. The commands that ask for the mnemonic
take it in place of the mnemonic and the passphrase. Whoever has it can spend
the funds of the wallet.
|]

-- | A number of shares, from 1 to 16: the format has four bits for it
upToSixteen :: String -> Text -> Either String Natural
upToSixteen name typed =
  case readNatural typed of
    Just n
      | n >= 1 && n <= 16 -> Right n
      | otherwise -> Left ("The " <> name <> " value has to be between 1 and 16")
    Nothing -> Left ("Could not parse the " <> name <> " option")

-- | A kind of account as typed: bip44 or bip84
readKind :: Text -> Either String AccountKind
readKind typed = maybe (Left ("The type of an account is " <> intercalate " or " (cs <$> kindNames))) Right (kindNamed typed)

-- | The network, when given. Without it, a command that needs one takes the
-- default-network of the configuration file, or btc.
networkMaybeOption :: Args (Maybe Network)
networkMaybeOption = networkOptionWith "Without it, the default-network of the configuration file, or btc."

-- | The network of a sweep, when given. A sweep that pays a portfolio of
-- the wallet is on the network of the addresses it pays. One that pays
-- none needs it.
sweepNetworkOption :: Args (Maybe Network)
sweepNetworkOption =
  networkOptionWith "Needed by a sweep that pays no portfolio of this wallet. The default-network of the configuration file does not apply to it."

-- | The network of a transaction file, when given. A file hw wrote names
-- its network. A file of another wallet names none, and is read on this
-- network.
fileNetworkOption :: Args (Maybe Network)
fileNetworkOption =
  networkOptionWith "For a file that names no network, such as a file of another wallet. Without it, the default-network of the configuration file, or btc."

-- | The network of an address or a coin the user names, when the wallet
-- has it on several networks
heldNetworkOption :: String -> Args (Maybe Network)
heldNetworkOption what =
  networkOptionWith ("Needed only when the wallet has the " <> what <> " on several networks. This can happen on the test networks, which share their keys. Without it, the one on the default network.")

-- | The network of an address of the wallet the user names, by itself or
-- by its path, when the wallet has it on several networks: the test
-- networks share their keys, and a portfolio has an address at the same
-- path on each network it gave one out on
-- ('Haskoin.Wallet.Database.Address.chooseOnNetwork')
namedAddressNetworkOption :: Args (Maybe Network)
namedAddressNetworkOption =
  networkOptionWith "With an address or a path, the network to find it on. It is needed only when the wallet has the address on several networks. Without it, the only network that has it, or else the default network."

-- | The network, when given, with the help of the command about it. The
-- help names the networks the setup checks at first, and says where
-- the others are: the command line is read before the configuration, so
-- the help cannot name the ones this user shows. The shell offers those
-- ('shownNetworkNames'). A hidden network reads here as any other, and is
-- refused once the configuration is read
-- ('Haskoin.Wallet.Config.hiddenNetworkRefusal').
networkOptionWith :: String -> Args (Maybe Network)
networkOptionWith without =
  networkOptionSaying ("The network. It is one of the networks hw shows: btc and bch, and those that setdefault --show-network added. " <> without)

-- | The network, when given, with the whole help of the option
networkOptionSaying :: String -> Args (Maybe Network)
networkOptionSaying said =
  optional . option (eitherReader readNetwork) $
    short 'n'
      <> long "network"
      <> metavar "NETWORK"
      <> completer (listIOCompleter shownNetworkNames)
      <> help said

-- | The network of a name typed, or the names a network has
readNetwork :: String -> Either String Network
readNetwork typed = maybe (Left ("Invalid network name. Select one of the following: " <> everyNetwork)) Right (netByName typed)

{- recover -}

-- | recover, which finds the portfolios of a mnemonic that were paid
recoverCommand :: CommandSpec Command
recoverCommand =
  CommandSpec
    { name = "recover",
      offline = False,
      summary = "Find the portfolios of a mnemonic that were paid",
      details =
        [r|
`recover` asks for a mnemonic, or for the shares of a backup. It searches for
the portfolios of that mnemonic that were paid, creates them and syncs them.
hw does not store the mnemonic.

`recover` needs the mnemonic and the server at the same time, so it runs on an
online computer and not on an offline one. A computer that was tampered with
can steal a mnemonic typed on it. To keep the mnemonic offline, create the
portfolios on the offline computer with newportfolio --existing instead. Copy
them to the online computer with exportwallet and importwallet, and run
`sync` there.

`recover` checks the portfolios in order from portfolio0. For each one, it
asks the server about the receiving addresses of every account: on every
network, the test networks included, and of both types of address. When any
account of a portfolio was ever paid, `recover` creates the whole portfolio,
with every account, and syncs it. It skips the portfolios the wallet has
already.

`recover` adds the portfolios to the wallet of the mnemonic, which hw finds
by the fingerprint of its master key. A new wallet gets its portfolio0
whether that portfolio was paid or not, since a wallet always has portfolio0.
A fingerprint is four bytes, so two mnemonics may share one. When the
database has a wallet of the same fingerprint whose keys are not those of the
mnemonic, `recover` refuses.

In a database without any portfolio, the first portfolio `recover` creates
becomes the active one, where the commands work. That is portfolio0 of the
new wallet. In a database that has an active portfolio, that portfolio stays
active, and `recover` says how to switch to a portfolio it created.

The server may not serve every network. The public one has no regtest chain,
for example. `recover` names the networks the server does not serve, and
searches the others. When the server of a network is down, `recover` says so,
searches the other networks, then exits with an error. It cannot find a
portfolio that was paid on that network only.

`recover` searches the networks hw hides too, and its status line names them.
Its answer leaves them out of what it searched, synced and found. It only
names the hidden networks that a portfolio of the wallet is used on, with the
command that shows them. It does not say when the server of a hidden network
is down, and that is then no error.

`recover` also looks at the paths where other wallets keep their accounts,
and reports funds there that hw cannot spend. On Bitcoin, those are the bip49
accounts of wrapped SegWit, m/49', which hardware wallets made before native
SegWit, and the bip86 accounts of Taproot, m/86'. On Bitcoin Cash, those are
the bip44 accounts of Bitcoin, m/44'/0', where a wallet that held Bitcoin at
the fork of 2017 has its Bitcoin Cash. `recover` adds no such account. To use
these funds, send them to an address of this wallet with a wallet that holds
such accounts.

The search stops after --portfolio-gap portfolios in a row where nothing was
found. BIP44 says to stop at the first such account. hw looks further,
because a portfolio created on an offline computer may never have been paid
before a later one was. On a terminal, a status line says which portfolio
`recover` is searching, as `sync` does.

After every 20 portfolios the server reports paid, `recover` asks whether to
keep looking. A server that lies can keep reporting paid portfolios without
end. Answer yes if the mnemonic has that many portfolios. A no is for a report
you do not believe: the search stops and nothing is added, not even the
portfolios found before the question.

When `recover` finds nothing at all, it says to check the passphrase. A
mnemonic with another passphrase, or with none, is another wallet, so a
mistyped passphrase finds no payment.
|],
      args =
        CommandRecover
          <$> (Recover <$> portfolioGapOption)
    }

-- | --portfolio-gap of recover: how many portfolios in a row with nothing
-- found end the search
portfolioGapOption :: Args Natural
portfolioGapOption =
  option (eitherReader portfolioGap) $
    long "portfolio-gap"
      <> metavar "INT"
      <> value 10
      <> showDefault
      <> help
        [r|
The search ends after this many portfolios in a row where nothing was found,
from 1 to 100. A larger gap finds a paid portfolio that comes after more
portfolios that were created on an offline computer and never paid.
|]
  where
    portfolioGap :: String -> Either String Natural
    portfolioGap arg =
      case readNatural (cs arg) of
        Just n | n >= 1 && n <= maxPortfolioGap -> Right n
        _ -> Left $ "The portfolio gap is a number of portfolios, from 1 to " <> show maxPortfolioGap
    -- Every portfolio searched is a scan of the receiving chain of each of
    -- its accounts at the store. A larger gap keeps recover asking about
    -- portfolios nobody created: 100 is ten times the default.
    maxPortfolioGap :: Natural
    maxPortfolioGap = 100

{- checkmnemonic -}

-- | checkmnemonic, which checks a mnemonic and its passphrase against the
-- active portfolio
checkMnemonicCommand :: CommandSpec Command
checkMnemonicCommand =
  CommandSpec
    { name = "checkmnemonic",
      offline = True,
      summary = "Check a mnemonic and passphrase against the active portfolio",
      details =
        [r|
checkmnemonic asks for the mnemonic and the passphrase, and checks that they
give the public key of every account of the active portfolio. Use it to test a
mnemonic you wrote down, or the shares of a backup, before you rely on it. It
signs nothing, changes nothing in the wallet and does not store what you
type. To check another portfolio, switch to it first.

When the mnemonic and the passphrase do not match the portfolio, hw says so
and exits with an error, so that a script can tell. When they are those of
another wallet of the database, hw names that wallet.

With --show-master-key, checkmnemonic also prints the master private key of
the wallet, which starts with xprv, as newportfolio --show-master-key does
for a new mnemonic. Use it for a wallet made without that option. The key is
printed only when the mnemonic and the passphrase match the portfolio, so it
is never the key of another wallet. The commands that ask for the mnemonic
take this key in place of the mnemonic and the passphrase, on one line of
their input, so that a script can sign with it. Whoever has the key can spend
the funds of the wallet. It is not a backup: it does not give the mnemonic
back.
|],
      args = CommandCheckMnemonic . CheckMnemonic <$> showMasterKeyOption checkedMasterKeyHelp
    }

{- mnemonicsheet -}

-- | mnemonicsheet, which writes a sheet to print, to write a mnemonic on
-- by hand, or the shares of a backup. It takes the options of
-- newportfolio that decide what the secret looks like, with the same
-- names and the same defaults: the sheet for a secret is asked for as the
-- secret is.
mnemonicSheetCommand :: CommandSpec Command
mnemonicSheetCommand =
  CommandSpec
    { name = "mnemonicsheet",
      offline = False,
      summary = "Write a sheet to print, to write a mnemonic on by hand",
      details =
        [r|
mnemonicsheet writes a sheet to a new PDF file. Print it, and write the
mnemonic of a wallet on it by hand. The sheet has a numbered line for each
word, so that no word is left out and the order is kept. It has fields for
the name of the wallet, the date and the fingerprint, two boxes that say
whether a passphrase goes with the words, and notes on how to keep the sheet
safe.

The sheet is blank. hw never writes the words of a mnemonic to a file, so the
sheet holds no secret, and any printer can print it.

Give mnemonicsheet the options you give newportfolio. The sheet then has the
lines of the secret that newportfolio makes with them:

  hw mnemonicsheet sheet.pdf                 a mnemonic of 12 words
  hw mnemonicsheet sheet.pdf --entropy 32    a mnemonic of 24 words
  hw mnemonicsheet shares.pdf --shares 3 --threshold 2
                                             3 shares of 20 words

With --shares, the file has a page for each share, since you keep the shares
in different places. Each page has the number of its share, and says how many
shares restore the wallet and how many you can lose.

newportfolio shows the mnemonic once. Print the sheet before you run it. Or
print the sheet that newportfolio writes for the wallet it creates, while the
words are on the screen: it is in ~/.hw/mnemonic-sheets, and the answer of
newportfolio names the file. That sheet also has the fingerprint of the
wallet and the date on it.

The sheet fits a mnemonic made by another program too. A BIP39 mnemonic has
12, 15, 18, 21 or 24 words, for an --entropy of 16, 20, 24, 28 or 32. A share
of a SLIP-39 backup has 20 or 33 words, for an --entropy of 16 or 32.

The file must not exist yet.
|],
      args =
        CommandMnemonicSheet
          <$> ( MnemonicSheetOptions
                  <$> fileArgument "The PDF file to write, such as sheet.pdf. It must not exist yet."
                  <*> entropyOption
                  <*> sharesOption sheetSharesHelp
                  <*> thresholdOption sheetThresholdHelp
              )
    }

{- The options that say where a command works -}

-- | The --wallet option, with the help of the command about it
walletOptionWith :: String -> Args (Maybe Natural)
walletOptionWith = optional . walletIndexOption

-- | The --portfolio option, with the help of the command about it
portfolioOptionWith :: String -> Args (Maybe Natural)
portfolioOptionWith = optional . portfolioIndexOption

-- | The --wallet option, given, with the help of the command about it
walletIndexOption :: String -> Args Natural
walletIndexOption what =
  option (eitherReader (indexOf "wallet")) $
    long "wallet"
      <> metavar "INT"
      <> completer (listIOCompleter walletIndexes)
      <> help (what <> " A wallet is named by its index, as `portfolios` shows it: 0 for wallet0.")

-- | The --portfolio option, given, with the help of the command about it
portfolioIndexOption :: String -> Args Natural
portfolioIndexOption what =
  option (eitherReader (indexOf "portfolio")) $
    short 'p'
      <> long "portfolio"
      <> metavar "INT"
      <> completer (listIOCompleter portfolioIndexes)
      <> help (what <> " A portfolio is named by its index: 0 for portfolio0.")

-- | The index of a wallet or of a portfolio, as the value of its option
-- is typed: a number, or the name the listings show, such as 2 or
-- portfolio2. The first argument is the word the name starts with, wallet
-- or portfolio. A refusal names the value as the value of the option,
-- "The --portfolio value" ('indexNamed').
indexOf :: String -> String -> Either String Natural
indexOf what = indexNamed what ("The --" <> what <> " value")

-- | The index of a wallet or of a portfolio, as typed, with how a refusal
-- names the value: "The --portfolio value" for an option, or "The
-- portfolio" for the argument of switch. The first argument is the word
-- the name starts with, wallet or portfolio. An index of 2^31 or more is
-- refused ('indexFits'): it names no wallet and no portfolio, and the
-- database would read it as another, smaller one.
indexNamed :: String -> String -> String -> Either String Natural
indexNamed what valueName typed =
  case readNatural (cs (fromMaybe typed (stripPrefix what typed))) of
    Nothing -> Left (valueName <> " " <> typed <> " is not an index, such as 0, or a name, such as " <> what <> "0")
    Just index
      | indexFits index -> Right index
      | otherwise -> Left (valueName <> " " <> typed <> " is too large: the index of a " <> what <> " is below 2147483648, which is 2^31")

-- | The --wallet and --portfolio options of importwallet and show, which
-- name one wallet and one portfolio of a file of a wallet, each with the
-- help of the command about it
fileScopeOptions :: String -> String -> Args ImportScope
fileScopeOptions whatWallet whatPortfolio =
  ImportScope <$> fileWalletOption whatWallet <*> filePortfolioOption whatPortfolio

-- | The --wallet option of importwallet and show, which names one wallet
-- of a file of a wallet, with the help of the command about it.
--
-- The value is the fingerprint of the wallet, where the --wallet of the
-- other commands takes an index. A file names its wallets by fingerprint
-- and holds no index. The index of this database would name another
-- wallet when the two databases number their wallets differently. The
-- shell offers no value for it. The file tells the fingerprints, and the
-- command line may name the file after the option.
fileWalletOption :: String -> Args (Maybe Fingerprint)
fileWalletOption what =
  optional . option (eitherReader fileWalletTyped) $
    long "wallet"
      <> metavar "FINGERPRINT"
      <> help (what <> " A wallet of a file is named by its fingerprint, 8 hexadecimal digits such as bc560f95. `show` lists the wallets of a file.")

-- | The fingerprint of a wallet of a file, as the value of --wallet is
-- typed, in either case. An index such as 0 is refused with the reason,
-- for the user who names a wallet as the other commands do.
fileWalletTyped :: String -> Either String Fingerprint
fileWalletTyped typed =
  either (const (Left notFingerprint)) Right (textToFingerprint (Text.toLower (Text.strip (cs typed))))
  where
    notFingerprint =
      "The --wallet value "
        <> typed
        <> " is not a fingerprint. A file names each wallet by its fingerprint, 8 hexadecimal digits such as bc560f95, and not by an index: show lists the wallets of a file"

-- | The --portfolio option of importwallet and show, which names one
-- portfolio of a file of a wallet, with the help of the command about it.
-- The shell offers no index for it: the file tells them, which the
-- command line may name after the option, and the database does not.
filePortfolioOption :: String -> Args (Maybe Natural)
filePortfolioOption what =
  optional . option (eitherReader (indexOf "portfolio")) $
    short 'p'
      <> long "portfolio"
      <> metavar "INT"
      <> help (what <> " A portfolio is named by its index: 0 for portfolio0.")

-- | The --with-change option of tag and of untag, with the help of the
-- command about it: whether the command goes down the chain of change of
-- the address ("Haskoin.Wallet.Database.ChangeChain"). It is an option,
-- and not what tag always does: a tag on an address may say something of
-- that address alone, such as what it receives, and the user who wants
-- the group to follow its coins says so.
withChangeOption :: String -> Args Bool
withChangeOption what = switch (long "with-change" <> help what)

-- | The --type option, with the help of the command about it
typeOptionWith :: String -> Args (Maybe AccountKind)
typeOptionWith what =
  optional . option (eitherReader (readKind . cs)) $
    short 't'
      <> long "type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help ("The type of address: bip44 for legacy P2PKH addresses, or bip84 for native SegWit P2WPKH addresses, which pay lower fees. " <> what)

-- | Where a command that works on one account of the active portfolio
-- looks: the network and the type, each with its default
accountTargetOption :: Args Target
accountTargetOption =
  Target
    <$> networkMaybeOption
    <*> typeOptionWith "Without it, the default-type of the configuration file, or bip44. The networks of Bitcoin Cash have bip44 only, and refuse bip84."

-- | Where a listing of the active portfolio looks: the network and the
-- type, each of which narrows the listing when it is given
listingTargetOption :: Args Target
listingTargetOption =
  Target
    <$> networkOptionWith "Only this network. Without it, every shown network of the portfolio."
    <*> typeOptionWith "Only the addresses of this type. Without it, both types."

{- exportwallet and importwallet -}

-- | exportwallet, which writes every wallet and its labels to a file
exportWalletCommand :: CommandSpec Command
exportWalletCommand =
  CommandSpec
    { name = "exportwallet",
      offline = False,
      summary = "Write every wallet and its labels to a file",
      details = importExportWalletDetails,
      args =
        CommandExportWallet
          <$> walletOptionWith "Write only this wallet. Without it, every wallet, or with --portfolio, the wallet of the active portfolio."
          <*> portfolioOptionWith "Write only this portfolio. It is a portfolio of the wallet that --wallet names, or else of the wallet of the active portfolio. Without it, every portfolio."
          <*> fileArgument "The file to write. It must not exist yet."
    }

-- | importwallet, which adds the portfolios and the labels of a file of
-- exportwallet to the database
importWalletCommand :: CommandSpec Command
importWalletCommand =
  CommandSpec
    { name = "importwallet",
      offline = False,
      summary = "Add the portfolios and labels of a file",
      details = importExportWalletDetails,
      args =
        CommandImportWallet
          <$> fileScopeOptions
            "Read only this wallet of the file. Without it, every wallet of the file."
            "Read only this portfolio. Without it, every portfolio. In a file of several wallets, name the wallet too, with --wallet."
          <*> fileArgument "The file exportwallet wrote"
    }

-- | The details that the helps of exportwallet and importwallet share: the
-- two commands are the two ends of one file
importExportWalletDetails :: String
importExportWalletDetails =
  [r|
exportwallet writes every portfolio of every wallet to a file, with its
labels and its tags. importwallet adds such a file to another database. The
file holds public keys and no private key, and neither command asks for the
mnemonic. The public keys still reveal every address and balance, so keep the
file private.

The file is in the format of BIP 329, which other wallets read and write. One
file holds the whole database, whichever portfolio is active. Each account
goes in as its public key, with its path, its network, its type and the
fingerprint of the mnemonic. Then come the labels of its receiving addresses
and of its transactions, and the tags of its addresses and of its
transactions. Each tag comes with its colour and with whether it locks its
coins. The coins locked with lockcoin go in too, and so do the descriptions
of the wallets and of their portfolios.

BIP 329 defines no tags. hw writes them under three keys of its own, tags,
tagcolors and taglocks, which other wallets ignore. A tag that nothing has is
written too, on the first key of the file, since no address names it. Only a
file of the whole database has such tags.

hw also writes this file by itself to ~/.hw/auto-backups after each command
that changes the labels. It keeps the recent copies, and fewer of the older
ones. backup writes the file to ~/.hw/user-backups, beside its copy of the
database.

  hw exportwallet all.jsonl                           every wallet
  hw exportwallet one.jsonl --wallet 1                every portfolio of wallet1
  hw exportwallet one.jsonl --portfolio 2             portfolio2 of the wallet
                                                      of the active portfolio
  hw exportwallet one.jsonl --wallet 1 --portfolio 2  portfolio2 of wallet1

A file of several wallets shows its reader that one person holds them all.
Write a wallet alone with --wallet when the file must not show the others.

importwallet reads such a file. It finds each wallet by the fingerprint the
file gives, and each portfolio by its index. The file does not say which
wallet is wallet0. A wallet of the file may have another index in the
database, and its labels still go to it. When the database does not have a
portfolio of the file, importwallet creates it whole, with the twelve
accounts of the file. The database can then watch the portfolio and prepare
its payments without the mnemonic. This is how an online computer works with
an offline signer. --wallet reads one wallet of the file only, and
--portfolio one portfolio only.

In a database without any portfolio, the first portfolio importwallet creates
becomes the active one, where the commands work. In a database that has an
active portfolio, that portfolio stays active, and importwallet says how to
switch to a portfolio it created.

A portfolio the database has already stays as it is. importwallet never
makes a second portfolio with the same index, and adds no account to one: a
portfolio has every account from the start. The keys of the file must be the
ones the portfolio holds. Otherwise importwallet refuses and changes nothing,
since the file and the database then come from different mnemonics.

importwallet also refuses three other files, and changes nothing. The first
is a file that would create a portfolio without all twelve of its accounts.
The second is a file with a key that another account of the database holds,
whatever index the file writes it at. The third is a file with two keys for
accounts of one path: the test networks share the path m/44'/1'/0', so their
bip44 accounts hold one key.

importwallet cannot prove that the keys of a file are those of your mnemonic:
only the mnemonic could. A file that names your fingerprint may still carry
the keys of someone else, and the addresses that receive gives out would then
be theirs. Import only a file you wrote with exportwallet on your own signer,
or one that reached you over a channel you trust.

importwallet imports whole wallets. A key alone, such as an xpub or an output
descriptor from another wallet, is one account, and it does not say which
portfolio it belongs to. importwallet refuses a file that holds such a key
and no record. In a BIP 329 file of another wallet, it leaves an xpub record
aside, and the answer says why.

With --wallet and --portfolio, importwallet reads only a part of the file:
the keys and the labels of that part. --wallet names one wallet of the file
by its fingerprint. --portfolio names one portfolio by its index.

  hw importwallet all.jsonl --wallet bc560f95                one wallet
  hw importwallet all.jsonl --wallet bc560f95 --portfolio 1  one portfolio of it
  hw importwallet one.jsonl --portfolio 1                    in a file of one
                                                             wallet

In a file of one wallet, --portfolio is enough. In a file of several wallets,
--portfolio alone is refused. It would name a portfolio of each wallet, so
importwallet asks for --wallet too, and lists the fingerprints of the file.

A wallet of a file is named by its fingerprint, and not by an index as on the
other commands. The file holds no index, and wallet0 on the computer that
wrote it may be wallet1 here. `show` lists the wallets of a file with their
fingerprints.

importwallet does not read the records of the rest of the file. The answer
counts them.

A wallet always has portfolio0. So importwallet creates a wallet that the
database does not have only from a file that holds its portfolio0, and it
refuses --portfolio 1 for such a wallet. Import the whole wallet instead.

importwallet sets the labels and the locks of the file on the addresses,
transactions and coins the wallet has. When the wallet has not derived an
address yet, importwallet derives it from the path the file gives, up to a
thousand addresses past the last used address of its chain. When `sync` has
not fetched a transaction yet, importwallet labels it at once if the origin
of its record says which portfolio it belongs to. A transaction record
without an origin labels the transaction in the portfolios that have it
already, on its network. The answer counts what hw cannot apply: run `sync`,
then importwallet again. importwallet leaves aside the labels of inputs, of
coins and of change addresses, since hw keeps none. A BIP 329 file of another
wallet sets its labels the same way.

importwallet adds the tags of the file to the tags that its addresses and
its transactions have already. It removes no tag. A tag that has no colour
takes the colour the file gives it, and one that has a colour keeps it. An
address takes the tags of the file at once, also a change address or a far
address that the wallet has not generated yet. When a `sync` finds that
address, its coins are under the tag from the start, and locked when the tag
locks its coins.

`show` FILE shows what importwallet would add from a file, and changes
nothing.

The fingerprint of a wallet identifies its mnemonic. It is the four bytes
that PSBTs and output descriptors write before a key of the mnemonic. Each
record of the file names its account with it, as in
pkh([bc560f95/44'/0'/0']) or wpkh([bc560f95/84'/0'/0']).
|]

{- portfolios -}

-- | portfolios, which shows the portfolios, their balances and their
-- descriptors
portfoliosCommand :: CommandSpec Command
portfoliosCommand =
  CommandSpec
    { name = "portfolios",
      offline = False,
      summary = "Show the portfolios, their balances and descriptors",
      details =
        [r|
`portfolios` lists every portfolio of every wallet, with its balances. A
wallet is the master key of one mnemonic. A portfolio holds the accounts of a
wallet that have the same account index: portfolio0 holds the accounts at
index 0, on every network.

Each portfolio starts with its name, its wallet, its description and the date
it was created. Then comes one line for each network where the portfolio has
funds or addresses. The line shows the total of what the portfolio holds
there, what it can spend of it now, and how many coins and addresses it has.
When the portfolio cannot spend all of it, the line also shows the unspendable
amount, which is the rest of the total. On btc, each figure is the sum of the
bip44 account and the bip84 account. The portfolios of one wallet are listed
together.

A * before the name marks the active portfolio, the one the commands work in.
switch makes another one active.

The spendable balance is what the portfolio can spend now on that network. It
leaves out the coins that a pending transaction, lockcoin or a tag locks, the
coinbase coins with fewer than 100 confirmations, and the unconfirmed payments
from other wallets. It counts the unconfirmed coins of the wallet's own
transactions: its change, and what one of its portfolios pays another.
preparetx can spend all of it, unless --min-conf asks for confirmations.
balances --verbose says why the portfolio cannot spend the rest.

Until a portfolio has synced on a network, hw has asked no server what it
holds there. The line of the network then reads not synced in place of the
amounts and the coins, which does not say that the portfolio holds nothing.
--verbose shows one row, Balance: not synced. --tabular writes not synced
under the total, --csv leaves the amounts and the coins empty, and --json
gives the network "synced": false. A computer that signs offline never
syncs: read what a portfolio holds on the online computer.

--verbose shows every detail. It adds the fingerprint of the mnemonic, and it
lists every network that hw shows. Each network has the total, the spendable
amount and the unspendable amount, then the confirmed and unconfirmed
balances that the server gave at the last sync. These two also add up to the
total: they split it by what a block has confirmed, and not by what the
portfolio can spend. Under each network come its accounts,
each with its derivation path and its output descriptor. An output descriptor
is a line of text in the format of BIP 380. It describes an account without
its private key, for another wallet to read:

  pkh([bc560f95/44h/0h/0h]xpub6CHK45s.../<0;1>/*)#vq597j87
  wpkh([bc560f95/84h/0h/0h]xpub6CQF8An.../<0;1>/*)#5nktextl

The first line describes a bip44 account, and the second a bip84 account.
Paste a descriptor into a wallet that takes one, to watch the account there.
A descriptor carries the public key of the account, the fingerprint of the
mnemonic, the derivation path, the two chains of addresses, receiving and
change, and a checksum. It holds no private key. A hardware wallet matches
the account to its mnemonic by the fingerprint and the path.

A descriptor does not say which network it is for. Bitcoin and Bitcoin Cash
both write their keys as xpub, and every test network writes them as tpub.

Each account also has a row SLIP-132. It shows the same public key with the
prefix that SLIP-132 gives its type of address: a zpub for a bip84 account, or
a vpub on a test network. Some wallets take only that form for a native SegWit
account, and make a legacy wallet when you give them an xpub. Only the prefix
differs: the key is the same. For a bip44 account the row shows the xpub, or
the tpub on a test network, since SLIP-132 gives the prefixes of BIP32 to
bip44 accounts.

A descriptor is wider than most screens. So on a terminal, the pager wraps
the lines of --verbose and does not cut them at the edge of the screen. You
can then copy a descriptor whole.

--tabular shows a table, with one row for each network of each portfolio. A
header names the columns: the * of the active portfolio, the wallet, the
portfolio, the network, the total of what the portfolio holds there, what it
can spend, what it cannot spend, how many coins and addresses it has, and its
description. The header also names the unit of the amounts.

--csv FILENAME writes a new CSV file for a spreadsheet, with one row for each
network of each portfolio. Its columns are the wallet, the portfolio, the
network, the types of the accounts of the portfolio on that network, whether
the portfolio is the active one, its description and the description of its
wallet, the paths of its accounts, the total of what it holds, what it can
spend, its confirmed and unconfirmed balances, its coins and addresses, and
when it was created.
Amounts are in satoshis. The time has three columns: in UTC, in local time,
and the offset of that local time from UTC. The file holds no key and no
descriptor. With them, anyone who gets the file could see every address and
every payment of the portfolio.
|],
      args =
        CommandPortfolios
          <$> listingOutputOption
            "Show every detail of each portfolio: the fingerprint of its mnemonic, every network with each of its balances, and the path, output descriptor and SLIP-132 key of every account."
            "Show each network of each portfolio on one row of a table: the wallet, the portfolio, the network, the total it holds, what it can spend, what it cannot spend, its coins and its addresses."
            "portfolios"
    }

{- switch -}

-- | switch, which makes another portfolio the active one
switchCommand :: CommandSpec Command
switchCommand =
  CommandSpec
    { name = "switch",
      offline = False,
      summary = "Make another portfolio the active one",
      details =
        [r|
switch makes another portfolio the active one. The commands work in the
active portfolio: receive gives out its addresses, `txs` lists its
transactions, and preparetx pays from it. A portfolio holds the accounts of a
wallet that have the same account index, and a wallet is the master key of
one mnemonic.

PORTFOLIO names the portfolio to make active. --wallet names its wallet.
Without --wallet, switch looks in the wallet of the active portfolio. With
--wallet and no PORTFOLIO, switch makes portfolio0 of that wallet active.

  hw switch 1               portfolio1 of the same wallet
  hw switch portfolio1      the same, by the name `portfolios` shows
  hw switch 0 --wallet 1    portfolio0 of wallet1
  hw switch --wallet 1      the same: portfolio0 when no portfolio is given

The portfolio stays active for every command until the next switch, since the
database keeps it. `portfolios` marks it with a *. The answer of switch names
the portfolio and the wallet that are now active.

--portfolio also names the portfolio, as an option. It is for a script that
names each value by its option. hw switch --portfolio 1 does the same as
hw switch 1. Give the portfolio once, as PORTFOLIO or with --portfolio.

The first portfolio of a database becomes active by itself, when
newportfolio, `recover` or importwallet creates it. A portfolio that these
commands add later does not become active. They say which portfolio is
active, and how to switch to the new one. switch needs no server, so it also
works on an offline computer.

Some commands find their portfolio from what you give them, and need no
switch first: importtx, signtx, bumpfee, `sweep`, preparesweep, signsweep,
label, lockcoin, unlockcoin, signmsg, and `show` when it shows a transaction,
a transaction file, a coin or an address. A transaction can involve several
portfolios. When hw cannot tell which one to use, it uses the active one if
it is among them. Otherwise it names them, and you switch to the one to use.
|],
      args =
        switchOf
          <$> optional (switchPortfolioArg <|> portfolioIndexOption "The portfolio to make active, as an option instead of PORTFOLIO. Without either, portfolio0 of the wallet that --wallet names.")
          <*> walletOptionWith "The wallet of the portfolio. Without it, the wallet of the active portfolio."
    }
  where
    -- The portfolio comes first on the command line, and the wallet after
    -- it, since the wallet is often left out. The portfolio is left out
    -- too when --wallet is given, for portfolio0 of that wallet.
    switchOf portfolioM walletM = CommandSwitch walletM portfolioM

-- | The portfolio switch makes active, as its argument: an index, such as
-- 1, or the name the listings show, such as portfolio1. It is the value
-- switch needs most, so it goes without an option, and --portfolio is
-- the same value for a script that names each value by its option. The
-- shell offers the portfolios of the wallet, as for --portfolio.
switchPortfolioArg :: Args Natural
switchPortfolioArg =
  argument (eitherReader (indexNamed "portfolio" "The portfolio")) $
    metavar "PORTFOLIO"
      <> completer (listIOCompleter portfolioIndexes)
      <> help "The portfolio to make active, by its index, such as 1, or by its name, such as portfolio1."

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

-- | The names of every network of a portfolio, as the help of
-- --show-network lists them. It is the one place of the help that names
-- them all.
everyNetwork :: String
everyNetwork = intercalate ", " ((.name) <$> portfolioNetworks)

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

{- deleteportfolio -}

-- | deleteportfolio, which deletes a portfolio that was never used
deletePortfolioCommand :: CommandSpec Command
deletePortfolioCommand =
  CommandSpec
    { name = "deleteportfolio",
      offline = False,
      summary = "Delete a portfolio that was never used",
      details =
        [r|
deleteportfolio deletes a portfolio that was never used. Such a portfolio has
no address, coin or transaction, so nothing is lost with it. It refuses a
portfolio that was used.

A portfolio holds the accounts of a wallet that have the same account index.
PORTFOLIO names the one to delete, by its index or by the name `portfolios`
shows. You must give it. deleteportfolio looks for it in the wallet of the
active portfolio.

  hw deleteportfolio 1             portfolio1
  hw deleteportfolio portfolio1    the same, by its name

It deletes a portfolio only when all of these are true:

  it is not portfolio0
  it is not the active portfolio
  it has the highest index in its wallet
  it has never had any activity, and gave out no address

Otherwise it refuses, and says which rule stopped it. Each rule has its
reason. A wallet always keeps portfolio0: a new portfolio0 would be the same
portfolio, with the same keys, so deleting it would be pointless. The
commands work in the active portfolio, so switch to another one before you
delete it. To delete a portfolio of another wallet, switch to a portfolio of
that wallet first. Only the highest index can go, so that the indexes of a
wallet keep no gap. A portfolio that was paid stays, since `recover` would
only find it again.

Activity is a coin, a transaction or a pending transaction. An address counts
too, whether it was paid or not. A new portfolio has no address: hw generates
one only when receive gives it out, when a payment uses one for its change,
or when a file or a sync names it. Someone may pay such an address later, and
a deleted portfolio would not see the payment.

deleteportfolio asks before it deletes, and names the portfolio that it is
about to delete. Any answer but y or yes deletes nothing. --yes answers the
question in advance, for a script.

Before it deletes, hw copies the database and its labels into
~/.hw/auto-backups, with the copies it makes after each change. Its answer
names the two files. hw makes no copy after the deletion. hw makes no copy
at all when the key autobackup-database of config.toml is false.

To stop using a mnemonic, delete its whole wallet with deletewallet.
|],
      args =
        CommandDeletePortfolio
          <$> deletePortfolioArg
          <*> deleteYesOption
    }

-- | The portfolio deleteportfolio deletes, as its argument: an index,
-- such as 1, or the name the listings show, such as portfolio1. It is
-- the one value the command needs, so it goes without an option, as the
-- wallet of deletewallet does ('deleteWalletArg'). The parser refuses
-- the command without it: a command that deletes names its target, and
-- never takes it from the active portfolio. The shell offers the
-- portfolios of the wallet of the active portfolio, where the command
-- looks.
deletePortfolioArg :: Args Natural
deletePortfolioArg =
  argument (eitherReader (indexNamed "portfolio" "The portfolio")) $
    metavar "PORTFOLIO"
      <> completer (listIOCompleter portfolioIndexes)
      <> help "The portfolio to delete, by its index, such as 1, or by its name, such as portfolio1. It is a portfolio of the wallet of the active portfolio."

{- deletewallet -}

-- | deletewallet, which deletes a wallet with all of its portfolios
deleteWalletCommand :: CommandSpec Command
deleteWalletCommand =
  CommandSpec
    { name = "deletewallet",
      offline = False,
      summary = "Delete a wallet with all of its portfolios",
      details =
        [r|
deletewallet removes a wallet from the database, with all of its portfolios
and their addresses, labels, transactions, coins and pending transactions.
The funds stay on the chain, and the mnemonic can still spend them. A wallet
is the master key of one mnemonic: use deletewallet for a mnemonic that you
no longer use. Unlike deleteportfolio, it deletes a wallet that has had
activity.

WALLET names the wallet, by its index or by the name `portfolios` shows. You
must give it: deletewallet never takes the wallet of the active portfolio by
default.

  hw deletewallet 1          wallet1
  hw deletewallet wallet1    the same, by its name

deletewallet asks before it deletes. It names the wallet that it is about to
delete, with its description, its fingerprint and how many portfolios it has,
so that you can check that it is the right one. Any answer but y or yes
deletes nothing. --yes answers the question in advance, for a script.

Before it deletes, hw copies the database and its labels into
~/.hw/auto-backups, with the copies it makes after each change. Its answer
names the two files, which hold the deleted wallet, its labels and its
pending transactions. restoredatabase brings the whole database back from
the copy of the database. importwallet brings back the wallet and its labels
from the file of labels. hw makes no copy after the deletion, so these two
files stay the newest until the next change. hw keeps them as it keeps every
automatic copy: newer copies push them out with time. To keep the wallet for
good, run `backup` before you delete it. To erase the wallet from the
computer, remove the copies that hold it. hw makes no copy at all when the
key autobackup-database of config.toml is false.

The other wallets keep their names. When you delete wallet0, wallet1 is still
wallet1, and the next new wallet becomes wallet0 again.

deletewallet also removes the blank sheet that newportfolio wrote for the
wallet in ~/.hw/mnemonic-sheets. A sheet you printed is yours to keep.

deletewallet refuses the wallet of the active portfolio while the database
has another wallet: switch to a portfolio of another wallet first. It deletes
the last wallet, with the active portfolio. The database then has no active
portfolio, and the first portfolio created after that becomes active.
|],
      args =
        CommandDeleteWallet
          <$> deleteWalletArg
          <*> deleteYesOption
    }

-- | The wallet deletewallet deletes, as its argument: an index, such as
-- 1, or the name the listings show, such as wallet1. It is the one value
-- the command needs, so it goes without an option, as the portfolio of
-- switch does ('switchPortfolioArg'), and that of deleteportfolio
-- ('deletePortfolioArg'). The parser refuses the command without it: a
-- command that deletes names its target, and never takes it from the
-- active portfolio. The shell offers the wallets of the database.
deleteWalletArg :: Args Natural
deleteWalletArg =
  argument (eitherReader (indexNamed "wallet" "The wallet")) $
    metavar "WALLET"
      <> completer (listIOCompleter walletIndexes)
      <> help "The wallet to delete, by its index, such as 1, or by its name, such as wallet1, as `portfolios` shows it."

-- | The --yes of the commands that delete from the database for good:
-- deleteportfolio, deletewallet, deletetx, deletetag and deletecontact.
-- Each one says what it is about to delete and asks first, and --yes
-- answers in advance, for a script.
deleteYesOption :: Args Bool
deleteYesOption = yesOption "Delete without asking first"

{- receive -}

-- | receive, which gives a new address to receive a payment at
receiveCommand :: CommandSpec Command
receiveCommand =
  CommandSpec
    { name = "receive",
      offline = False,
      summary = "Get a new address to receive a payment",
      details =
        [r|
`receive` gives a new address to hand to a payer: the next receiving address
of one account of the active portfolio. --network and --type pick the account.
Without them, it is the account of the default network and type: a bip44
address on btc, unless the configuration file says otherwise. You never
create an account first. A portfolio already has one for every network and
type.

The address is at the path M/44'/coin'/index'/0/i, or M/84'/coin'/index'/0/i
for bip84, where index is the index of the portfolio. Change does not go to
these addresses. The wallet sends the change of its payments to other
addresses, at .../1/i, and `addresses` --change lists them.

`receive` gives out at most 20 unused addresses in a row. The key gap of the
configuration file changes that number. The limit protects a wallet restored
from the mnemonic: it stops looking after that many unused addresses in a
row, so it would miss a payment to a later address. When that many addresses
wait for a payment, `receive` refuses. Give out one of them again instead:
`addresses` lists them, and `show` shows one again with its QR code.

On a terminal, `receive` prints the QR code of the address under it, for the
payer to scan. The code holds a payment URI with the address and nothing
else, so it does not hold the label. When the answer is piped into another
program, or with --json, `receive` prints the text alone, without the code.

--label gives the new address a label, to recognize the payment later. --tag
puts a tag on it: a word that puts the address in a group, such as kyc.
Repeat --tag for several tags. `label` and `tag` do the same later, on an
address you gave out before.
|],
      args = CommandReceive <$> (Receive <$> accountTargetOption <*> labelOption <*> many (tagOption "A tag to put on the new address, as `tag` does later. Repeat for several tags."))
    }

-- | A tag typed on the command line, as its option or its argument reads
-- it: capital letters are taken as small ones, and a word that is no tag
-- is refused, with the rule it breaks ("Haskoin.Wallet.Tags".'readTag')
tagReader :: ReadM Tag
tagReader = eitherReader (readTag . cs)

-- | A tag typed as an argument of tag or untag, after what to tag. A word
-- that is no tag and reads as an address, a path or a hash was most
-- likely typed in the wrong place, the tags before what they go on. The
-- refusal then says the order, rather than which rule of a tag the
-- address breaks.
tagArgReader :: ReadM Tag
tagArgReader = eitherReader $ \word -> case (readTag (cs word), labelledShapeOf word) of
  (Right tag, _) -> Right tag
  (Left _, Just shape) -> Left (word <> " reads as " <> shape <> ", not as a tag. What to tag comes first, then the tags: ADDRESS|TXID|PATH TAG")
  (Left refusal, Nothing) -> Left refusal

-- | The new name of a tag, as renametag takes it: a tag, read as any other.
-- A name with a space has a refusal of its own. The refusal of 'readTag'
-- goes on to say that several tags are given one after the other, which is
-- right for the command tag and wrong for a name. The user who types a name
-- of two words wants to know how to keep both.
newTagNameReader :: ReadM Tag
newTagNameReader = eitherReader $ \word ->
  if ' ' `elem` word
    then Left ("\"" <> word <> "\" has a space: the name of a tag is one word. Join its words with - or _, as in " <> joined word <> ".")
    else readTag (cs word)
  where
    joined = intercalate "-" . words

-- | A tag after --tag, read as any other. A word with a space has a refusal
-- of its own, as the new name of renametag has ('newTagNameReader'). The
-- refusal of 'readTag' says to give several tags one after the other, which
-- is how the command tag takes them. After an option, the second word would
-- be read as an argument of the command: a recipient of send, for one. So
-- this refusal shows the option repeated.
tagOptionReader :: ReadM Tag
tagOptionReader = eitherReader $ \word ->
  if ' ' `elem` word
    then Left ("\"" <> word <> "\" has a space: a tag is one word. Repeat --tag for several tags, as in " <> repeated word <> ".")
    else readTag (cs word)
  where
    repeated = unwords . map ("--tag " <>) . words

-- | The --tag option, with the help that says what it does in its
-- command, and the tags of the database offered to the shell
tagOption :: String -> Args Tag
tagOption what =
  option tagOptionReader $
    long "tag"
      <> metavar "TAG"
      <> completer (mkCompleter tagNames)
      <> help what

-- | The --tag and --untagged options of a listing of addresses, of coins
-- or of contacts: the tags to keep the rows of, and whether to keep the
-- rows without a tag. Several tags keep the rows that have any one of
-- them, and --untagged adds its rows to theirs.
addressTagFilterOption :: String -> Args TagFilter
addressTagFilterOption listed =
  TagFilter
    <$> many (tagOption ("Filter: only the " <> listed <> " this tag. Repeat for several tags: the listing then shows those that have any one of them. `tags` lists the tags."))
    <*> untaggedOption ("Filter: only the " <> listed <> " no tag. With --tag, the listing shows those as well.")

-- | The --untagged option of a listing, with the help that says which
-- rows it keeps there.
--
-- It is an option of its own, not a word after --tag such as none: a
-- word after --tag is a tag, and none may be one.
untaggedOption :: String -> Args Bool
untaggedOption what = switch (long "untagged" <> help what)

-- | The --tag, --address-tag and --untagged options of a listing of
-- transactions: the tags of transactions and the tags of addresses to
-- keep the rows of, and whether to keep the transactions left to sort.
-- Together, and repeated, they keep the transactions that match any one
-- of them.
txTagFilterOptions :: Args TxTagFilter
txTagFilterOptions = txTagFilterOptionsOf "Filter: only" "the listing" " Each transaction shows the tag beside the address that has it."

-- | The --tag, --address-tag and --untagged options of a command that
-- keeps transactions by their tags: a listing, the account statement of
-- report, the journal of exportjournal. They mean the same in each, so
-- they are parsed and explained here once. The words given fit the help
-- to the command: how each help starts, as in "Filter: only", what keeps
-- the transactions, as in "the listing", and what the command shows of
-- the tag of an address, as a sentence after a space, or nothing.
txTagFilterOptionsOf :: String -> String -> String -> Args TxTagFilter
txTagFilterOptionsOf lead keeper shown =
  TxTagFilter
    <$> many (tagOption (lead <> " the transactions that have this tag. Repeat for several tags, and combine with --address-tag: " <> keeper <> " then includes the transactions that match any one of them."))
    <*> many
      ( option tagReader $
          long "address-tag"
            <> metavar "TAG"
            <> completer (mkCompleter tagNames)
            <> help (lead <> " the transactions that spend from or pay an address of the portfolio that has this tag, or that pay or were paid from a contact of the address book that has it. Repeat for several tags, and combine with --tag: " <> keeper <> " then includes the transactions that match any one of them." <> shown)
      )
    <*> untaggedOption (lead <> " the transactions left to sort: those that have no tag, and that neither pay nor were paid from a contact of the address book that has one. With --tag or --address-tag, " <> keeper <> " includes those as well.")

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

-- | The --tag and --untagged options of exportsheet: the tags to keep
-- the rows of in every tab, and whether to keep the rows without a tag.
-- They are the two options every listing takes, and each tab reads them
-- as its listing does: the tags of an address for the addresses, the
-- balances and the coins, those of a transaction for the transactions,
-- and those of a contact for the address book.
--
-- exportsheet has no --address-tag. In txs it keeps the transactions at
-- the addresses of a tag, and a tab of addresses already keeps its rows
-- by the tags of the address with --tag. One command with both would
-- have two options for one thing in half of its tabs.
sheetTagFilterOptions :: Args TagFilter
sheetTagFilterOptions =
  TagFilter
    <$> many (tagOption "Write only the rows that have this tag, in each tab: the addresses that have it, with their balances and their coins, and the transactions and the contacts that have it. Repeat for several tags: each tab then includes the rows that have any one of them. `tags` lists the tags.")
    <*> untaggedOption "Write only the rows without a tag, in each tab: the addresses that have none, with their balances and their coins, the contacts that have none, and the transactions left to sort, as txs --untagged lists them. With --tag, each tab includes those as well."

-- | --label of receive: the label of the new address
labelOption :: Args (Maybe Text)
labelOption =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "LABEL"
      <> help "A label for the address, to recognize the payment later"

{- show -}

-- | show, which shows one thing by its name: an address, a transaction or a
-- coin, or a file of a transaction, of a wallet or of a database
showCommand :: CommandSpec Command
showCommand =
  CommandSpec
    { name = "show",
      offline = False,
      summary = "Show an address, a transaction or a coin, or a file of a transaction, of a wallet or of a database",
      details =
        [r|
`show` shows one thing and changes nothing: an address, a transaction or a
coin of the wallet, or a file. The file can be a transaction file, a file of
a wallet or a database file. `show` tells what the word is by its shape:

  TYPE/0/i       the receiving address number i of the account of that
                 type, such as bip84/0/3
  TYPE/1/i       the change address number i of the account of that type
  ADDRESS        an address of the wallet, or a contact of the address book
  HASH           a transaction, by its txid, or its nosigHash while pending
  TXID:INDEX     a coin
  FILENAME       a file, any other word

Give a hash in full or by its short id, its first 8 digits. A file whose
name looks like one of the other words needs its directory before it, such
as ./a1075db5. With no word, `show` shows the newest receiving address of
the account, which is the first one that `addresses` lists. It gives out no
new address. --network and --type pick the account in the active portfolio,
as they do for `receive`.

`show` prints an address of the wallet the way `receive` prints a new one:
its path, its label and what it received. On a terminal, the QR code of the
address comes under them. Use it to give an
address out again. Any address of the wallet works, a change address too.
For an address of the address book, `show` prints the contact. It refuses
any other address, so that its QR code never pays somebody else. That
includes an address of the wallet that this computer has not generated, as
on the offline computer: give its path there.

A path names an address by its place in an account, such as bip84/0/3. It
starts with the type of its account, the way every view writes a path. A
path without the type, such as /0/3, is in the account that --type picks.
`show` refuses --type with a path that has its type. `show` derives the
address from the public key of the account. That needs no mnemonic and no
server, so it works on an offline computer as well as online.

Use a path on the offline computer to check an address that the online
computer gave out. An online computer that was tampered with can show an
address of its own in place of an address of the account, and the payer
would pay the attacker. The offline computer holds the same portfolio, since
newportfolio created it there, so it derives the same addresses. Run
`receive` on the online computer. Then run `show` on the offline computer
with the path that `receive` printed, such as bip84/0/3, and compare the two
addresses.

When the wallet holds the address at the path, in its database, `show`
prints it as any address of the wallet. When it does not, `show` prints the
address cut: its two ends, with a dot for each character of the middle, and
no QR code. This is the case on the offline computer, which gave no address
out. Compare the two ends with the address that the online computer shows.
The ends are enough, since nobody can make another address with the same
ends: it would take about 2^128 tries. `show` hides the middle so that
nobody pays the address from this screen. `receive` did not give it out
here, and it may lie past the gap, the run of unused addresses that a
restored wallet looks at. Look at the path too. A tampered online computer
picks the path it shows, and a path far past the addresses you gave out is a
sign of tampering.

`show` prints a transaction in full, as each portfolio that holds it sees
it. A payment between two portfolios is printed twice, once for each. `show`
prints a coin with every detail, as `coins` --verbose does.

Use `show` on a transaction file to check its transaction before you sign
it, or before you send it. `show` finds the portfolio of the transaction by
itself, as importtx does. It prints the transaction as that portfolio sees
it, with the name of the portfolio. It prints a transaction between two
portfolios as the portfolio that pays sees it. The wallet keeps nothing from
the file. A transaction file is a PSBT (BIP 174), which preparetx --output,
exporttx, signtx or preparesweep wrote. A transaction that involves no
portfolio, such as a sweep to addresses outside the wallet, is printed on
the network that its file names.

A transaction that is not sent yet, pending or in a file, is printed in full:
every address it spends from, with its amount, and every address it pays. You
may be about to sign it or to send it. A Spends field gives the total of the
coins it spends that are not the portfolio's, as a sweep does: check it
against the addresses it pays. A transaction that involves no portfolio is
External, and its first line gives the total it spends. Where the time goes,
"from a file" means that the wallet does not hold the transaction.

`show` reads a PSBT that another wallet wrote the same way: in bytes or in
base64 text, of version 0 or of version 2 (BIP 370). Such a file names no
network, so `show` reads it on the network of --network, or on the default
network.

The labels that `show` prints for a transaction file come from the file: the
label of the transaction, those of its addresses and those of the coins it
spends, as the wallet that wrote the file had them. They are not the labels
of this wallet. An offline computer has no labels of its own, so it shows
those of the online wallet this way.

A file of a wallet is the file that exportwallet writes and importwallet
reads. It holds BIP 329 records. `show` reads as a file of a wallet any file
that is not a database, a PSBT or the hexadecimal of a transaction. It
prints what importwallet would do with the file, and does not do it. First
comes
each wallet of the file with its portfolios. Each portfolio is either one
that importwallet would create, with its accounts, or one that the database
has already. Then comes each account, with the labels of its addresses and
transactions, and the coins it locks or unlocks. `show` puts a record under
the account that the origin and the network of the record name. The labels
that belong to no account of the file come after the accounts. Then come the
tags of the file that nothing has, under "Tags not used", each with its
colour and with whether it locks its coins, and last the records that hw
keeps no label for. `show` refuses a file that importwallet would refuse,
with the reason.

A database file is a copy of the database of hw: one that `backup` wrote, or
one of ~/.hw/auto-backups. `show` recognizes it by its first bytes, which
every SQLite database starts with. Run `show` on a copy before you restore
it with restoredatabase, to see which wallets and portfolios come back.
`show` lists each wallet of the copy, with the fingerprint of its seed and
its description. Under each wallet come its portfolios, each with the day it
was created and its description. A * marks the active portfolio of that
database. `show` leaves the file as it is. It does not print the history or
the balances of the copy: a sync brings them up to date once the copy is
restored. `show` refuses a database whose format version is not that of this
hw, and names its version.

The first line of the answer says which kind of file `show` read: "BIP 329
file" for a file of a wallet, "SQLite database" for a database. A backup has
one file of each kind, and both hold wallets and portfolios. `show` tells
the kind of a file from what it holds, never from its name.

The options that `show` takes depend on the word. With a path or with no
word, --network and --type pick the account, in the active portfolio. With
an address or a coin, `show` finds the account by itself, so it refuses
--type. --network is then needed only when the wallet has the address or the
coin on several networks, which can happen because the test networks share
their keys. With a transaction file that names no network, --network gives
the network. With a file of a wallet, --wallet and --portfolio show one
wallet or one portfolio of the file only, as they make importwallet read one
only. --wallet names a wallet of the file by its fingerprint. In a file of
several wallets, --portfolio needs --wallet with it. `show` refuses
--network with a file of a wallet, since each record of the file names its
network. It refuses --wallet and --portfolio with anything else. A database
file takes none of these options: `show` lists every wallet and portfolio
of it.
|],
      args =
        (\shown target fileScope -> CommandShow target fileScope shown)
          <$> optional shownArg
          <*> ( Target
                  <$> networkOptionWith "With a path or with nothing to show, the network of the account. With a transaction file that names no network, the network to read it on. With an address or a coin, the network to find it on, when the wallet has it on several. Without it, the default-network of the configuration file, or btc."
                  <*> typeOptionWith "With a path without its type, such as /0/3, or with nothing to show, the type of the account. Without it, the default-type of the configuration file, or bip44."
              )
          <*> fileScopeOptions
            "With a file of a wallet, show only this wallet of the file, as importwallet --wallet reads it."
            "With a file of a wallet, show only this portfolio of the file, as importwallet --portfolio reads it."
    }

-- | What show shows, read by its shape ('readShown'), with what the shell
-- offers for it ('shownCompletion')
shownArg :: Args Shown
shownArg =
  argument (eitherReader readShown) $
    metavar "PATH|ADDRESS|HASH|TXID:INDEX|FILENAME"
      <> completer (mkCompleter shownCompletion)
      <> help "What to show: a path such as bip84/0/3, an address of the wallet or of the address book, a transaction by its txid or nosigHash, a coin, or a file: a transaction file, a file of a wallet or a database file. Give a hash in full or by its short id, its first 8 digits. Without it, `show` shows the newest receiving address."

-- | What the shell offers for show. It offers the addresses, transactions
-- and coins of the wallet ('shownTargets'), and the files and directories,
-- as for a FILENAME. It leaves out a file whose name show reads as
-- something else, a file named a1075db5 for instance. show would take that
-- word for a short id. The file is offered once .\/ is typed before it.
-- Offering .\/a1075db5 for the word a1 would not do, since zsh drops a value
-- that does not start with the word typed.
shownCompletion :: String -> IO [String]
shownCompletion typed = do
  held <- shownTargets typed
  files <- completeFile typed
  return (held <> filter readAsFile files)
  where
    readAsFile name = readShown name == Right (ShownFile name)

{- addresses -}

-- | addresses, which lists the receiving addresses of a portfolio, or its
-- change addresses
addressesCommand :: CommandSpec Command
addressesCommand =
  CommandSpec
    { name = "addresses",
      offline = False,
      summary = "List the receiving addresses of a portfolio, or its change addresses",
      details =
        [r|
`addresses` lists every receiving address of the active portfolio. --change
lists its change addresses instead. The addresses come in sections, one for
each network and type: the bip44 addresses on btc, then the bip84 ones, then
those on bch, and so on. A section in which the portfolio has no address is
left out. --network and --type show one network or one type only.

In each section, the newest address comes first. --reverse lists each
section from its first address instead. The sections keep their order.

Each address takes two lines. The first line gives its path, such as
bip84/0/3, then the address, its label, and its tags, each between brackets,
as in [kyc]. The path starts with the type of the account. After the type,
/0/i is the receiving address number i, and /1/i the change address number
i. The second line gives the total of the address, which is the value of
its coins, then what it received, and how many transactions it has: those
that paid it and those that spent from it. An address that no transaction
has paid yet is unused.

Until the portfolio has synced on a network, hw has asked no server about
its addresses there. Their second line then reads not synced, which does not
say that nobody paid them. --verbose, --tabular and --csv show no amount and
no count for such an address, and --json gives it "synced": false. A
computer that signs offline never syncs: read what an address holds on the
online computer.

When the address cannot spend all of its total now, the second line names
the unspendable amount after the total: a coin that is locked, or that waits
for a confirmation. The address can spend the rest. So an address with a
locked coin does not read as one that a payment emptied, whose total is
zero. The help of `balances` says what counts as unspendable.

--verbose adds what the address can spend of its total and what it cannot,
its confirmed and unconfirmed balances, how many coins it holds, and when it
was created.

--tabular shows each address on one row of a table, under a header that
names the columns: the path, the address, its total, what it can spend of
it, what it received, how many transactions it has, its tags, and its label.
The amounts are in the unit that the header names. An address that no transaction has
paid leaves its amounts empty. Change addresses have no label, so their
table has no column for one. The tags and the label are the last two
columns. Many tags or a long label then make only the end of a row longer,
and the columns before them stay lined up.

--tag TAG lists only the addresses that have the tag. A tag is a word that
`tag` puts on addresses to group them. Repeat --tag for several tags: the
listing then shows the addresses that have any one of them, and its first
line names those tags. A word that is not a tag is refused.

--untagged lists only the addresses that have no tag: those you have put in
no group yet. With --tag as well, the listing shows both.

--csv FILENAME writes the addresses to a new CSV file, for a spreadsheet,
one address on each row. The columns are its wallet, portfolio, network and
type, the address, its path, its chain (receiving or change), its label and
its tags, its total, what it can spend of it, its confirmed and unconfirmed
balances, what it received, its coins and its transactions, and when it was
created. The tags
share one cell, with a space between them. Amounts are in satoshis. Each
time takes three columns: in UTC, in local time, and the offset of that
local time from UTC.

--after ADDRESS starts the listing after that address. The listing goes on
with the rest of its section, then with the sections that follow. With
--reverse, it goes on in the reverse order.
|],
      args =
        CommandAddresses
          <$> listingTargetOption
          <*> changeAddressesOption
          <*> addressTagFilterOption "addresses that have"
          <*> listingOutputOption
            "Show every detail of each address: its transactions, what it received, its total, what it can spend of it and what it cannot, its confirmed and unconfirmed balances, its coins, and when it was created."
            "Show each address on one row of a table: its path, the address, its total, what it can spend of it and what it received, its transactions, its tags and its label."
            "addresses"
          <*> pagingOptions (afterOption "ADDRESS" str "Start after this address")
    }

-- | --change of addresses: the change addresses, in place of the receiving
-- ones
changeAddressesOption :: Args Bool
changeAddressesOption =
  switch $
    long "change"
      <> help "List the change addresses instead of the receiving ones"

-- | The options that page a listing: the row to start after, as the
-- listing shows it, how many rows to show at most, and whether to show
-- them in the reverse order. A script reads a listing a page at a time
-- with them. Without them, every row is shown, in the order of the
-- configuration on the screen and in the usual order elsewhere.
pagingOptions :: Args (Maybe after) -> Args (Paging after)
pagingOptions after =
  Paging
    <$> after
    <*> maxOption
    <*> orderFlags
      "List the rows in the reverse order, the last one first. setdefault --reverse makes it the order of every listing on the screen."
      "List the rows in the usual order. Use it when setdefault --reverse, or the key listing-reverse of config.toml, reversed the listings."

-- | The --reverse and --no-reverse options, with the help of each: the
-- rows of a listing in the reverse order, from the last to the first in
-- each group, or in the usual one. They are the only options that change
-- the order of a listing ('ListingOrder'). A listing takes them for
-- itself, and setdefault for every listing on the screen, as the two
-- take the options of the views. --no-reverse is there for the user
-- whose configuration reverses the listings, and who wants one of them
-- in the usual order. Neither has a short letter: -r is
-- --recipients-pay, and a letter keeps one meaning.
orderFlags :: String -> String -> Args (Maybe ListingOrder)
orderFlags reverseHelp usualHelp =
  optional $
    flag' ReverseOrder (long "reverse" <> help reverseHelp)
      <|> flag' UsualOrder (long "no-reverse" <> help usualHelp)

-- | The --after option of a listing: the row to start after, read as the
-- listing shows it, with the help that names the row
afterOption :: String -> ReadM after -> String -> Args (Maybe after)
afterOption name reader what =
  optional . option reader $
    long "after"
      <> metavar name
      <> help (what <> ", to read the listing in pages of --max rows")

-- | --max of addresses: the most rows the listing shows
maxOption :: Args (Maybe Natural)
maxOption =
  optional . option (maybeReader $ readCount . cs) $
    long "max"
      <> metavar "INT"
      <> help "Show this many rows at most. Without it, every row."
  where
    -- A listing of no row at all is not asked for
    readCount s = case readNatural s of
      Just n | n > 0 -> Just n
      _ -> Nothing

{- balances -}

-- | balances, which lists the addresses of a portfolio that hold coins
balancesCommand :: CommandSpec Command
balancesCommand =
  CommandSpec
    { name = "balances",
      offline = False,
      summary = "List the addresses of a portfolio that hold coins",
      details =
        [r|
`balances` lists the addresses of the active portfolio that hold coins,
receiving and change addresses together, grouped by network. The title line
of each network gives the total of its addresses. These totals repeat those
of `portfolios`, on purpose: they are the same figures, beside the addresses
that add up to them. On btc, the bip44 and the bip84 addresses are listed
together. --network and --type show one network or one type only.

On each network, the address with the largest total comes first, or the one
with the smallest with --reverse.

Each address takes two lines. The first line gives its path, such as
bip84/0/3, then the address and its label, or the words "change address",
and its tags, each between brackets. The path starts with the type of the
account. The type tells apart the addresses of the bip44 and the bip84
accounts, which both have a /0/3. After the type, /0/i is the receiving
address number i, and /1/i the change address number i. The second line
gives three amounts:

  total          the value of all the coins at the address
  spendable      what the address can spend now, as the help of
                 `portfolios` explains
  unspendable    the rest

The unspendable amount counts the coins locked by a pending transaction, by
lockcoin or by a tag that locks its coins, the unconfirmed payments from
other wallets, and the coinbase coins with fewer than 100 confirmations.
`coins` tells which of these applies to each coin.

Until the portfolio has synced on a network, hw has asked no server what its
addresses hold there, and has no coin to list. When the portfolio has
addresses on such a network, the title line of the network reads not synced
in place of the total, with no address under it. This does not say that the
addresses hold nothing. --json gives the group of such a network
"synced": false, and no address. A computer that signs offline never syncs:
read what the addresses hold on the online computer.

--verbose gives the fields of each address: the total, the number of its
coins, the spendable amount, then the unspendable amount in four parts. The
parts are the value locked by pending transactions, the value locked with
lockcoin, the value locked by a tag, and the value not spendable yet, which
waits for a confirmation.

--tabular shows each address on one row of a table, under a header that
names the columns: the path, the address, the three amounts, its tags, and
its label, or "change address". The amounts are in the unit that the header
names.

--tag TAG lists only the addresses that have the tag. Repeat it for several
tags: the listing then shows the addresses that have any one of them, and
its first line names those tags. The total of each network then adds up
those addresses alone, so balances --tag savings tells what the savings
hold.

--untagged lists only the addresses that have no tag. The total of each
network then tells what is in no group. With --tag as well, the listing
shows both.

--csv FILENAME writes the addresses to a new CSV file, for a spreadsheet,
one address on each row. The columns are its wallet, portfolio, network and
type, the address, its path, its chain, its label and its tags, its total,
what it can spend, and the four parts it cannot. Then come the columns that
the file of addresses --csv ends with: its confirmed and unconfirmed
balances, what it received, its coins and its transactions, and when it was
created. Amounts are in satoshis. Each time takes three columns: in UTC, in
local time, and the offset of that local time from UTC.
|],
      args =
        CommandBalances
          <$> listingTargetOption
          <*> addressTagFilterOption "addresses that have"
          <*> listingOutputOption
            "Show every detail of each address: its total and its coins, its spendable amount, what pending transactions lock, what you lock, what a tag locks, and what is not spendable yet."
            "Show each address on one row of a table: its path, the address, its total, spendable and unspendable amounts, its tags and its label."
            "addresses that hold coins"
          <*> pagingOptions (afterOption "ADDRESS" str "Start after this address")
    }

{- label -}

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

{- tag, untag, tags, renametag and deletetag -}

-- | tag, which puts tags on an address, a transaction or a contact, or sets
-- the colour or the lock of a tag
tagCommand :: CommandSpec Command
tagCommand =
  CommandSpec
    { name = "tag",
      offline = False,
      summary = "Put tags on an address, a transaction or a contact, or colour or lock a tag",
      details =
        [r|
A tag is a word you put on addresses and transactions to group them, such as
kyc, savings or rent. A label says what one thing is for. A tag names a group,
and the other commands work on the whole group. A listing can show only the
rows that have a tag. A payment can spend only the coins at the addresses that
have a tag.

`tag` adds tags to an address, a transaction or a contact. The first word
names what to tag, and the tags follow. The tags that are there already stay:

  hw tag ADDRESS kyc cold        tag a receiving or a change address
  hw tag bip84/0/3 kyc           tag the address at a path of the active
                                 portfolio
  hw tag TXID rent               tag a transaction, by its txid, or its
                                 nosigHash while pending

A tag is one word, made of the letters a to z, digits, - and _, with 32
characters at most. Capital letters are taken as small ones, so Kyc and kyc
are one tag. Spaces, accents and other signs are refused. An address or a
transaction can have several tags. A tag exists from the first time you use
it. You do not create it first.

The first word is an address of the wallet, a path with the type of its
account, such as bip84/0/3, or a transaction, in full or by its short id.
`label` takes the same first word. A change address takes tags too, although
it takes no label: a tag says which group its coins belong to.

`tag` finds an address or a transaction in any portfolio, whichever is
active. A path names an address of the active portfolio. A transaction that
several portfolios have is tagged in each of them. The test networks share
their keys, so the wallet can have one address on several networks. --network
then says which one to tag.

--color gives a colour to each tag of the command: red, green, yellow, blue,
magenta or cyan. The screen then shows the tag in that colour. none removes
the colour. With --color and one word only, the word is the tag to colour,
and nothing is tagged:

  hw tag kyc --color red         colour the tag kyc
  hw tag kyc --color none        remove its colour

A tag has one colour in the whole database, on everything that has the tag. A
colour is only a help to the eye. No command behaves differently for it.

--lock-coins makes a tag lock its coins. From then on, no payment spends the
coins at an address that has the tag, unless the payment names the tag with
--spend-from-tag. The coins that arrive later are locked too. Use it for
savings, so that an everyday payment never spends them. Give --lock-coins with
one word only: the word is the tag, and nothing is tagged:

  hw tag savings --lock-coins      the tag savings locks its coins
  hw send ADDRESS:0.01             leaves the coins of savings
  hw send ADDRESS:0.01 --spend-from-tag savings
                                   spends them, this once
  hw tag savings --unlock-coins    the tag no longer locks its coins

lockcoin locks one coin, and not the coins that arrive after it. A tag locks
every coin at the addresses that have it, and it can lock its coins before
any address has it.

Only --spend-from-tag with the tag spends a coin that the tag locks. --coin
with the coin, or --spend-from with its address, does not spend it. A sweep or
an imported transaction may not spend it either. When an address has two tags
that lock their coins, a payment must name both tags to spend from it.

The lock wins over a payment prepared before it. When a pending transaction
spends a coin that comes under the lock, tag says so, and signtx and
broadcast then refuse that transaction. deletetx deletes it. A transaction
that was sent already is on the network, and no lock stops it. A payment that
named the tag with --spend-from-tag is not stopped.

`coins` shows such a coin as locked by tag, `balances` --verbose counts it
apart, and `tags` says which tags lock their coins. Only a tag on an address
locks coins. The same tag on a transaction or on a contact locks nothing.

The change of a payment made with --spend-from-tag goes to a new change
address. That address has no tag, so nothing locks the change. Add
--tag-change to tag it, so that it stays locked.

What tags do:

  --tag TAG              on addresses, balances and coins, lists only the rows
                         of the addresses that have the tag. On txs and
                         pendingtxs, lists only the transactions that have it.
  --address-tag TAG      on txs and pendingtxs, lists only the transactions
                         that spend from or pay an address that has the tag,
                         or a contact of the address book that has it
  --spend-from-tag TAG   on `send`, preparetx, sendall and preparesendall,
                         spends the coins at the addresses that have the tag
  receive --tag TAG      tags the new address
  tag TAG --lock-coins   locks the coins at the addresses that have the tag:
                         only a payment that names the tag spends them

hw puts no tag on the change of a payment by itself. Two options tag the
change, so that it stays in the group of the coins spent:

  --tag-change           on `send` and preparetx, tags the new change address
                         with the tags that every coin spent has
  tag --with-change      tags the change of the payments made before

--with-change tags the address, then the change of every payment that spent
from it, then the change of the payments that spent that change, and so on
down to the coins you hold now. The answer lists the change addresses it
tagged, then those where the tag stopped, each with the payment and the
address that stopped it.

  hw tag ADDRESS kyc --with-change   the address, and its change down the chain

A change address takes a tag only when every coin that the payment spent has
the tag. --tag-change follows the same rule. So the change of a payment that
also spent a coin from outside the group takes no tag. Each run looks at every
change address down the chain from the address, whatever tags it has. So a
second run completes a chain that was tagged in part. A tag passes from the
coins a payment spends to its change, and never the other way: a tag on a
change address says nothing about the addresses its coins came from.

A contact of the address book takes tags too. Name it by its address: hw tag
ADDRESS friends. `contacts` --tag TAG then lists the contacts that have the
tag, and txs --address-tag TAG lists the transactions that pay them or that
they paid.

`untag` takes tags off, `tags` lists them, renametag gives one a new name, and
deletetag deletes one everywhere. exportwallet writes the tags to its file,
and importwallet reads them back.
|],
      args =
        CommandTag
          <$> ( TagOptions
                  <$> strArgument
                    ( metavar "ADDRESS|TXID|PATH"
                        <> completer (mkCompleter tagTargets)
                        <> help "What to tag: an address of the wallet, the address of a contact of the address book, a path of the active portfolio, such as bip84/0/3, or a transaction. Name a transaction by its txid, or by the hash pendingtxs shows, in full or by its short id. With --color, --lock-coins or --unlock-coins and no tag after it, this word is the tag to change."
                    )
                  <*> many (argument tagArgReader (metavar "TAG" <> completer (mkCompleter tagNames) <> help "A tag to put on it. Give several tags one after the other."))
                  <*> optional
                    ( option (eitherReader (readColorChoice . cs)) $
                        long "color"
                          <> metavar "COLOR"
                          <> completeWith (cs <$> (map colorName tagColors <> [noColorWord]))
                          <> help "The colour of each tag given: red, green, yellow, blue, magenta or cyan, or none to remove it. A tag has one colour, on everything that has the tag."
                    )
                  <*> switch
                    ( long "lock-coins"
                        <> help "With a tag alone: the tag locks its coins from now on. No payment spends the coins at an address that has the tag, unless the payment names the tag with --spend-from-tag. The coins that arrive later are locked too."
                    )
                  <*> switch
                    ( long "unlock-coins"
                        <> help "With a tag alone: the tag no longer locks its coins."
                    )
                  <*> withChangeOption "Also tag the change of the payments that spent from the address, then the change of the payments that spent that change, and so on down to the coins held now. A change address takes a tag only when every coin that the payment spent has the tag. With an address or a path only."
                  <*> ( Target
                          <$> namedAddressNetworkOption
                          <*> typeOptionWith "With a path without its type, such as /0/3, the type of its account. Without it, the default-type of the configuration file, or bip44."
                      )
              )
    }

-- | untag, which takes tags off an address, a transaction or a contact
untagCommand :: CommandSpec Command
untagCommand =
  CommandSpec
    { name = "untag",
      offline = False,
      summary = "Take tags off an address, a transaction or a contact",
      details =
        [r|
`untag` takes tags off an address, a transaction or a contact, and leaves its
other tags. The first word names what to take the tags off: an address of the
wallet or of the address book, a path of the active portfolio, or a
transaction. `tag` takes the same first word.

  hw untag ADDRESS kyc           the address is no longer in the group kyc
  hw untag TXID rent tax         take two tags off a transaction

`untag` refuses a tag that the address or the transaction does not have, and
changes nothing: you most likely mistyped the tag. A transaction that several
portfolios have loses the tags in each.

--with-change also takes the tags off the change of every payment that spent
from the address, then off the change of the payments that spent that change,
and so on. `tag` --with-change puts tags on the same change addresses. Use it
to take a tag put by mistake off the whole chain at once:

  hw untag ADDRESS kyc --with-change

It does so even when the address has lost the tag already, and is refused
only when no address of the chain has the tag.

The tag itself stays, with its colour, even when nothing has it any more.
`tags` then shows it as not used. deletetag deletes a tag everywhere.
|],
      args =
        CommandUntag
          <$> ( Untag
                  <$> strArgument
                    ( metavar "ADDRESS|TXID|PATH"
                        <> completer (mkCompleter untagTargets)
                        <> help "What to take the tags off: an address of the wallet, the address of a contact of the address book, a path of the active portfolio, such as bip84/0/3, or a transaction. Name a transaction by its txid, or by the hash pendingtxs shows, in full or by its short id."
                    )
                  <*> some (argument tagArgReader (metavar "TAG" <> completer (mkCompleter tagNames) <> help "A tag to take off it. Give several tags one after the other."))
                  <*> withChangeOption "Also take the tags off the change of the payments that spent from the address, then off the change of the payments that spent that change, and so on down. With an address or a path only."
                  <*> ( Target
                          <$> namedAddressNetworkOption
                          <*> typeOptionWith "With a path without its type, such as /0/3, the type of its account. Without it, the default-type of the configuration file, or bip44."
                      )
              )
    }

-- | tags, which lists the tags, with their colours and what has them
tagsCommand :: CommandSpec Command
tagsCommand =
  CommandSpec
    { name = "tags",
      offline = False,
      summary = "List the tags, with their colours and what has them",
      details =
        [r|
`tags` lists every tag of the database, in alphabetical order. Each line gives
the tag in its colour, the name of that colour, and how many addresses,
transactions and contacts of the address book have it. A tag that locks its
coins says so at the end of its line:

  cold     blue  2 addresses
  friends        3 contacts
  kyc      red   12 addresses, 3 transactions
  rent           not used
  savings  blue  4 addresses, locks its coins

No payment spends the coins at the addresses that have such a tag, unless the
payment names the tag with --spend-from-tag. The help of `tag` says more.

The counts cover every portfolio of every wallet, since a tag belongs to the
whole database. A transaction that two portfolios have counts once. A tag that
nothing has reads not used. It stays until deletetag deletes it.

To see what has a tag, run `addresses` --tag TAG, `coins` --tag TAG or txs
--tag TAG in the active portfolio, or `contacts` --tag TAG.

To change a tag:

  hw tag kyc --color blue        give it a colour, or another one
  hw tag kyc --color none        remove its colour
  hw tag kyc --lock-coins        make it lock its coins
  hw tag kyc --unlock-coins      stop it from locking them
  hw renametag kyc verified      give it a new name
|],
      args = pure CommandTags
    }

-- | renametag, which gives a tag a new name, or merges it into another tag
renameTagCommand :: CommandSpec Command
renameTagCommand =
  CommandSpec
    { name = "renametag",
      offline = False,
      summary = "Give a tag a new name, or merge it into another tag",
      details =
        [r|
renametag gives a tag a new name in the whole database: on every address and
every transaction that has it, in every portfolio of every wallet, and on
every contact of the address book. The tag keeps its colour. The answer says
how many addresses, transactions and contacts have it.

  hw renametag kyc verified              the tag kyc is now verified
  hw renametag saving savings --merge    saving and savings become one tag

The new name follows the rules of a tag, as `tag` gives them: one word, made
of the letters a to z, digits, - and _, with 32 characters at most. Capital
letters are taken as small ones.

renametag refuses a name that another tag has already. With --merge, it merges
the two tags instead. Everything that had the first tag then has the second
one, and the first tag is gone. Use it for a tag that you typed two ways, such
as saving and savings. The second tag keeps its colour, or takes the colour of
the first when it has none.

renametag cannot split a merged tag again, since nothing records any more
which of the two tags each address or transaction had. So renametag merges
only with --merge. With --merge, the second word must be a tag that exists.
renametag refuses any other word, since you most likely mistyped it.

Only the name changes. To change the colour of a tag, run hw tag TAG --color
COLOR.

hw keeps a copy of the labels and the tags after each command that changes
them, in the auto-backups directory, unless the configuration turns the copies
off. The copy made before a merge has the two tags as they were. To undo a
merge, delete the merged tag with deletetag, then run importwallet with that
copy.
|],
      args =
        CommandRenameTag
          <$> ( RenameTag
                  <$> argument tagReader (metavar "TAG" <> completer (mkCompleter tagNames) <> help "The tag to rename, as `tags` lists it")
                  <*> argument newTagNameReader (metavar "NEW-NAME" <> completer (mkCompleter tagNames) <> help "Its new name: a word that follows the rules of a tag. With --merge, the tag to merge it into.")
                  <*> switch (long "merge" <> help "Merge the tag into the tag that has the new name already. Without it, a name that another tag has is refused.")
              )
    }

-- | deletetag, which deletes a tag from every address, transaction and
-- contact
deleteTagCommand :: CommandSpec Command
deleteTagCommand =
  CommandSpec
    { name = "deletetag",
      offline = False,
      summary = "Delete a tag from every address, transaction and contact",
      details =
        [r|
deletetag deletes a tag from the whole database, with its colour. It takes the
tag off every address and every transaction that has it, in every portfolio of
every wallet, and off every contact of the address book. The answer says how
many had it.

`untag` takes a tag off one address or one transaction, and keeps the tag.
deletetag is for a tag you no longer want at all. For a tag you mistyped,
renametag gives it its right name.

deletetag asks before it deletes, and says how many addresses, transactions
and contacts lose the tag. Any answer but y or yes deletes nothing. --yes
answers the question in advance, for a script.

Nothing else is deleted: the addresses, the transactions and their labels stay
as they are. hw keeps a copy of the labels and the tags after each command
that changes them, in the auto-backups directory, unless the configuration
turns the copies off. importwallet brings the tags of such a copy back.
|],
      args =
        CommandDeleteTag
          <$> argument tagReader (metavar "TAG" <> completer (mkCompleter tagNames) <> help "The tag to delete, as `tags` lists it")
          <*> deleteYesOption
    }

{- newcontact, contacts and deletecontact -}

-- | newcontact, which adds an address of somebody else to the address book
newContactCommand :: CommandSpec Command
newContactCommand =
  CommandSpec
    { name = "newcontact",
      offline = False,
      summary = "Add an address of somebody else to the address book",
      details =
        [r|
newcontact adds a contact to the address book. The address book keeps the
addresses of others: the people and the services you pay, or that pay you. A
contact is one address, with a label that says whose it is, and tags.

  hw newcontact ADDRESS Landlord               add an address with its label
  hw newcontact ADDRESS "Alice, savings"       quote a label with spaces
  hw newcontact ADDRESS Alice --tag friends    add it with a tag
  hw newcontact ADDRESS                        add it without a label

The answer shows the address twice. The first line gives it in one piece, to
copy. The line In fours gives it in groups of four characters. Compare the
groups with the address the payee gave you, one group at a time, before you
trust the contact. People compare an address in one piece by its two ends
only, and a look-alike address is made to have the same ends.

What a contact does:

  txs, pendingtxs, `show`    a transaction that pays the address, or that
                           spends from it, shows it underlined, with its
                           label
  `send`, preparetx          the same before you sign a payment to it
  the Tab key              writes the address where a payment takes a
                           recipient: type the start of the address and
                           press Tab. In bash, the start of the label works
                           too: Ali and Tab write the address of Alice.

The address book is for the addresses of others only. newcontact refuses an
address of a wallet of the database. Such an address has a path, a label and
tags already, which `label` and `tag` set.

A contact is one address. Somebody with two addresses is two contacts, which
may share a label, or a tag.

The address book belongs to the whole database, not to a portfolio. The same
contacts show whichever portfolio is active, and they stay when a portfolio or
a wallet is deleted.

A contact is on one network. Most addresses are valid on one network only, and
newcontact takes that one. A legacy address is valid on btc and on bch, and an
address of a test network is valid on the other test networks. newcontact then
takes the default network, unless --network names another.

`label` changes the label of a contact, and `tag` and `untag` change its tags.
Each takes the address of the contact. `show` shows one contact, `contacts`
lists them all, and deletecontact deletes one. exportwallet writes the address
book to its file when it writes every wallet, and importwallet reads it back.
|],
      args =
        CommandNewContact
          <$> ( NewContact
                  <$> strArgument (metavar "ADDRESS" <> help "The address of somebody else to keep. A Bitcoin Cash address may be in either of its forms.")
                  <*> optional (strArgument (metavar "LABEL" <> help "Whose address it is, in your own words. Quote it if it has spaces. `label` changes it later."))
                  <*> many (tagOption "A tag for the contact, as `tag` puts one. Repeat for several tags.")
                  <*> networkOptionWith "The network of the contact, for an address that is valid on several. Without it, the only shown network the address is valid on, or else the default network."
              )
    }

-- | contacts, which lists the contacts of the address book
contactsCommand :: CommandSpec Command
contactsCommand =
  CommandSpec
    { name = "contacts",
      offline = False,
      summary = "List the contacts of the address book",
      details =
        [r|
`contacts` lists the address book: the addresses of others that newcontact
added, each with its label and its tags. The listing has a section for each
network that has contacts. In a section, the contacts are sorted by label,
without regard to capital letters. The contacts without a label come last,
sorted by address. --reverse reverses the order in each section.

Each contact takes two lines. The first gives its address in one piece, to
copy, then its label, and each of its tags between brackets. The second gives
the same address in groups of four characters, to compare with the address the
payee gave you, one group at a time:

  bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  "Alice"  [friends]
    bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4

A Bitcoin Cash address keeps its prefix in one piece before the groups.

--verbose also shows when each contact was added. --tabular shows each contact
on one row of a table: its address, the address in fours, its tags and its
label.

--tag TAG lists only the contacts that have the tag. Repeat it for several
tags: the listing then shows the contacts that have any one of them, and its
first line names those tags. --untagged lists only the contacts that have no
tag. With --tag, the listing shows those as well. --network lists only the
contacts on one network.

--csv FILENAME writes the contacts to a new CSV file, for a spreadsheet. Each
contact takes one row: its network, its address, the address in fours, its
label, its tags, and when it was added. Each time comes in UTC, then in local
time, then with the offset of that local time from UTC.

The address book belongs to the whole database, so the listing is the same
whichever portfolio is active.
|],
      args =
        CommandContacts
          <$> networkOptionWith "Only the contacts on this network. Without it, those on every shown network."
          <*> addressTagFilterOption "contacts that have"
          <*> listingOutputHolding
            "times in UTC and in local time"
            "Show every detail of each contact: its address, the address in fours, its label and its tags, and when it was added."
            "Show each contact on one row of a table: its address, the address in fours, its tags and its label."
            "contacts"
          <*> pagingOptions
            ( optional . strOption $
                long "after"
                  <> metavar "ADDRESS"
                  <> completer (mkCompleter contactAddresses)
                  <> help "Start after the contact with this address, to read the listing in pages of --max rows"
            )
    }

-- | deletecontact, which deletes a contact from the address book
deleteContactCommand :: CommandSpec Command
deleteContactCommand =
  CommandSpec
    { name = "deletecontact",
      offline = False,
      summary = "Delete a contact from the address book",
      details =
        [r|
deletecontact deletes one contact from the address book, with its label, and
takes its tags off it. The answer shows the contact as it was.

Nothing else changes. The transactions that paid the address stay in the
history. From then on, they show the address alone, as they show the address
of a stranger. The tags themselves stay, on everything else that has them.

deletecontact asks before it deletes, and shows the contact with its label.
Any answer but y or yes deletes nothing. --yes answers the question in
advance, for a script.

hw keeps a copy of the labels and of the address book after each command that
changes them, in the auto-backups directory, unless the configuration turns
the copies off. importwallet brings the contacts of such a copy back.
|],
      args =
        CommandDeleteContact
          <$> strArgument (metavar "ADDRESS" <> completer (mkCompleter contactAddresses) <> help "The address of the contact to delete, as `contacts` lists it")
          <*> networkOptionWith "The network of the contact, when the address book has the address on several networks. Without it, the only contact with this address, or else the one on the default network."
          <*> deleteYesOption
    }

{- txs -}

-- | txs, which lists the transactions of a portfolio
txsCommand :: CommandSpec Command
txsCommand =
  CommandSpec
    { name = "txs",
      offline = False,
      summary = "List the transactions of a portfolio",
      details =
        [r|
`txs` lists the transactions of the active portfolio, grouped by network.
--network shows one network only. `txs` shows a transaction as the portfolio
sees it, whichever accounts of the portfolio it touches. A transaction that
spends bip44 and bip84 coins together is one transaction.

The list is as of the last sync. On each network, the transactions follow the
order of the chain, the newest first: the unconfirmed ones, then the confirmed
ones from the newest block to the oldest. --reverse reverses that order: the
oldest transactions come first and the unconfirmed ones last, each with the
same balance beside it. The networks keep their order. The transactions you
prepared, and those you sent that `sync` has not seen yet, are not in this
list: pendingtxs lists them. `show` TXID shows one transaction in full.

Until the portfolio has synced on a network, hw has asked no server about
its addresses there, and has no transaction to list. When the portfolio has
addresses on such a network, the listing shows one line for it, which names
the network and reads not synced. This does not say that nobody paid the
addresses. --json gives the group of such a network "synced": false, and no
transaction. A computer that signs offline never syncs: read the
transactions on the online computer.

In the compact view, each transaction takes two lines. The first line starts
with the short id of the transaction: the first 8 digits of its txid. The
other commands take the short id in place of the txid. The line then gives the
time, the amount and the status of the transaction. The time is in the time
zone of the computer. The amount includes the fee, and has a plus sign when
funds come in. The status gives the confirmations, up to 999+ confirmations.
--verbose gives the full count. An unconfirmed transaction shows its fee rate
too, which tells how soon a block may take it. A dead transaction is one that
was replaced, or dropped from the network.

The second line gives the balance of the portfolio on the network after the
transaction. The balance beside the newest transaction is what the portfolio
holds there now, confirmed or not. A dead transaction shows no balance, and
the balances do not count it. Then comes what the transaction is for. A
payment received names the address that was paid, by its type and its path,
such as bip84/0/3. Then comes the label of the payment, or else the label of
the address. A payment you made gives its label, or else the address it pays.
When it pays several addresses, it gives the one it pays the most. Nothing in
the line is cut, not even a long label or address. On a terminal, the
addresses of the wallet and their labels are blue. The tags of the transaction
end the line, each between brackets, as in [rent].

When another transaction replaced a dead transaction, the second line of the
dead one starts with the short id of its replacement, as in replaced by
2b69e4f4. The replacement is the live transaction that spends a coin the dead
one spent. hw does not store this note. It reads it from the coins at each
listing, so the note follows the network. When a miner confirms a stuck
payment after all, the replacement of that payment is the dead one, and names
the payment. A dead payment of yours that names no replacement was dropped. A
dead payment you received never names one. A replacement that bumpfee made
pays the recipients of the payment. Any other replacement may pay somebody
else.

With --verbose, each transaction starts with its time, then its type:

  Incoming    funds enter the portfolio
  Outgoing    funds leave the portfolio
  Internal    funds move between addresses of the portfolio, from a bip44
              address to a bip84 one for instance

A payment to another portfolio of the wallet is Outgoing in this portfolio and
Incoming in the other one. Then come the network, and the types of the
addresses of the portfolio that the transaction involves. The txid is in full,
then its short id. Every address is in full, and an address of the wallet has
its path after it. The line below an address of the wallet gives the label and
the tags of that address. Below an address that the transaction spends from,
the line adds the labels of the coins spent, after the word from. A long label
wraps under the address. The tags of the transaction are in a Tags field. A
dead transaction has a Status field, which names its replacements by their
txids in full.

With --tabular, each transaction takes one row of a table, under a header
that names the columns. The first column, Txid, gives the short id. The
amounts are in the unit the header names. The Confs column gives the
confirmations as a number alone, up to 999+. An unconfirmed transaction has 0
there, and the table does not show its fee rate. A dead transaction says dead.
The Tags column gives the tags of the transaction. The last column,
Description, says what the transaction is for, in the words of the compact
view. The description of a dead transaction that was replaced names its
replacement first.

A tag is a word you put on transactions and addresses to group them. A
transaction has tags of its own, which `tag` TXID puts on it, and its
addresses have theirs. The compact view and the table show only the tags of
the transaction. --verbose shows both. Three options filter the listing:

  --tag TAG           shows only the transactions that have the tag
  --address-tag TAG   shows only the transactions that spend from or pay an
                      address of the portfolio that has the tag, and those
                      that pay or were paid from a contact of the address
                      book that has it
  --untagged          shows only the transactions left to sort: those that
                      have no tag, and that neither pay a contact that has a
                      tag nor were paid by one

You can repeat the first two, and combine the three: the listing then shows
the transactions that match any one of them. Its first line names the tags.
With --address-tag, the compact view and the table also show each address that
has one of the tags you asked for, with that tag beside it. They name an
address of the portfolio by its path, and a contact by its address and its
label. A filter leaves transactions out, and does not change the ones it
shows: each still shows the balance of the portfolio after it, as in the whole
listing.

Use --untagged to sort your transactions. Tag what you recognize, the
transaction or its contact, and list again: the listing gets shorter until
nothing is left. The tags of the addresses of the portfolio do not count for
--untagged. They say which group the coins are in, not what a payment is for.

With --csv FILENAME, hw writes the transactions to a new CSV file for a
spreadsheet, one on each row. The columns are the wallet, the portfolio and
the network of the transaction, the types of address it involves, its txid in
full and by its short id, its time, its direction (Incoming, Outgoing or
Internal), its amount, fee, fee rate and size, the balance after it, its
status, confirmations and block, the transaction that replaced it when it is
dead, its label and its tags.

Then come three addresses of the transaction: the one it spends the most from,
the one it pays the most, and its change. Each has its value, and its path,
label and tags when it is an address of the portfolio. One more column counts
the addresses that have no column of their own. The lock time comes last.
Amounts are in satoshis. Each time takes three columns: the time in UTC, the
time in local time, and the offset of that local time from UTC. The
spreadsheet of exportsheet lists every address of every transaction, in a tab
of its own.
|],
      args =
        CommandTxs
          <$> networkOptionWith "List only the transactions on this network. Without it, list those on every shown network of the portfolio."
          <*> txTagFilterOptions
          <*> listingOutputOption
            txVerboseHelp
            "Show each transaction on one row of a table: its short id, time, type, amount, balance, confirmations and tags. The last column says what the transaction is for: its label, the path of an address of the wallet, or the address paid."
            "transactions"
          <*> pagingOptions (afterOption "TXID" hashRefReader "Start after this transaction, by its txid or its short id")
    }

-- | Where a listing goes, and how it shows each element: in a few lines
-- with --compact, with every detail with --verbose, on one row of a table
-- with --tabular, or on one row of a new CSV file with --csv. The options
-- are a choice: a table has no room for every detail, and a file is not
-- the screen. Every listing offers them. Without any of them, the listing
-- takes the view of the configuration, compact unless the key listing-view
-- names another. --compact exists for a user whose configuration names
-- another view: it brings the compact view back for one listing. The
-- help of --verbose comes first,
-- then that of --tabular, then the name of what the listing lists, for
-- those of --compact and --csv.
listingOutputOption :: String -> String -> String -> Args ListingOutput
listingOutputOption verboseHelp tabularHelp listed =
  listingOutputHolding "amounts in satoshis and times in UTC and in local time" verboseHelp tabularHelp listed

-- | 'listingOutputOption', with what the help of --csv says the file
-- holds, given first. The listings of the wallet all hold amounts and
-- times. The address book holds no amount, and its help must not
-- promise any.
listingOutputHolding :: String -> String -> String -> String -> Args ListingOutput
listingOutputHolding csvHolds verboseHelp tabularHelp listed =
  fromMaybe (OnScreen Nothing)
    <$> optional
      ( OnScreen . Just <$> viewFlags compactHelp verboseHelp tabularHelp
          <|> CsvFile <$> strOption (long "csv" <> metavar "FILENAME" <> fileCompletion <> help csvHelp)
      )
  where
    compactHelp = "Show the " <> listed <> " in a few lines each. This view is the default, unless setdefault or the key listing-view of config.toml names another view."
    csvHelp = "Write the " <> listed <> " to this file, one on each row, as CSV for a spreadsheet, with " <> csvHolds <> ". The file must not exist yet."

-- | The view of a listing, from one of --compact, --verbose and
-- --tabular, with their help in that order. The listings read them, and
-- setdefault reads them for the view it makes the default, so that both
-- take the same words.
viewFlags :: String -> String -> String -> Args View
viewFlags compactHelp verboseHelp tabularHelp =
  flag' Compact (long "compact" <> help compactHelp)
    <|> flag' Verbose (short 'v' <> long "verbose" <> help verboseHelp)
    <|> flag' Table (long "tabular" <> help tabularHelp)

-- | The help of --verbose, the same in txs and in pendingtxs
txVerboseHelp :: String
txVerboseHelp = "Show every detail of each transaction: its label, its hash in full and its short id, each address it spends from or pays, with its amount, path and label, the labels of the coins spent as `coins` shows them, and the fee."

{- preparetx -}

-- | preparetx, which prepares a payment without signing it
prepareTxCommand :: CommandSpec Command
prepareTxCommand =
  CommandSpec
    { name = "preparetx",
      offline = False,
      summary = "Prepare a payment, without signing it",
      details =
        [r|
A payment takes three steps. preparetx is the first: it builds the
transaction and does not sign it. It needs no mnemonic, so it can run on an
online computer. signtx then signs the transaction, on an offline computer if
you like, and broadcast sends it to the network. `send` does the three steps
in one command, with the same options, on a computer where you type the
mnemonic.

hw keeps the new transaction as a pending transaction, which pendingtxs
lists. --output also writes it to a file, a PSBT (BIP 174), that you can carry
to an offline computer. Until the transaction is signed, hw names it by its
nosigHash, a hash of the transaction without its signatures. Once it is
signed, hw shows it by its txid instead. The commands take either hash, in
full or by its short id, which is its first 8 digits. While the transaction
is pending, its coins are locked: no other payment spends them. deletetx
deletes the transaction and frees its coins.

--label and --tag record what the payment is for, at the moment you make it.
A label is a text of your own. A tag is a word you put on transactions and
addresses to group them, such as rent. --tag TAG puts the tag on the
transaction, as the command `tag` does afterwards with the hash of the
transaction. Repeat --tag for several tags. hw creates a tag that does not
exist yet. The label and the tags stay with the transaction after it is
signed, sent and found by a sync. The --output file carries the label but not
the tags, which stay in the wallet that prepared the transaction. --tag does
not choose the coins to spend. --spend-from-tag, explained below, does.

On Bitcoin, preparetx marks the transaction as replaceable (BIP 125). Until
the transaction confirms, bumpfee can replace it with one that pays a higher
fee. Bitcoin Cash does not replace transactions.

preparetx selects the coins to spend among all the coins of the active
portfolio on the network of the payment. One transaction can spend bip44 and
bip84 coins together. A bip84 coin, which is SegWit, costs less fee to spend,
and the selection counts each coin at its own cost. --strategy sets how the
coins are selected.

Coin control lets you decide which coins the payment spends. Its options are
of two kinds, choosers and filters.

A chooser names coins to spend. --coin names one coin. --spend-from names the
coins at an address. --spend-from-tag names the coins at the addresses that
have a tag. The choosers add up: the chosen coins are those of every --coin,
of every --spend-from and of every --spend-from-tag. With a chooser,
preparetx spends every chosen coin that can be spent, and no other coin,
whatever --strategy says. The chosen coins must be in the active portfolio.
hw refuses a coin or an address of another portfolio, and names the portfolio
that holds it. --spend-from takes an address, or the path of an address of
the active portfolio. Write the path with its type, as the views show it,
such as bip44/1/0.

--spend-from-tag TAG chooses the coins at every address of the portfolio that
has the tag. It works as --spend-from does, for a whole group of addresses at
once. Repeat it for several tags, and the coins of all of them are chosen.
`coins` --tag TAG lists the coins of a tag. hw refuses a word that is not a
tag. It also refuses a tag that no address of the portfolio has on the
network of the payment.

A tag can lock its coins, with hw tag TAG --lock-coins. A payment then does
not spend the coins at the addresses that have that tag. --spend-from-tag TAG
is the only way to spend them. --coin and --spend-from do not open the lock.
When they choose a coin that a tag locks, preparetx does not spend the coin,
and the answer lists it with the reason.

A filter narrows the coins that may be spent, and never adds a coin. The
filters are --min-conf and --coin-type. Without a chooser, a filter applies to
every coin of the portfolio, and the strategy selects among the coins that
pass it. With a chooser, a filter applies to the chosen coins only. So
--coin-type bip44 with --spend-from spends only the coins at that address,
not every bip44 coin of the portfolio. With --spend-from-tag, it spends only
the bip44 coins of the tag. hw refuses a coin of the other type that --coin
or --spend-from names, since the two options disagree about it.

With a chooser and --select, preparetx spends only the coins the payment
needs among the chosen coins, instead of all of them. The strategy then
selects among the chosen coins, as it selects among all the coins of the
portfolio when there is no chooser. It leaves a chosen coin that is worth
less than the fee to spend it. With or without --select, the answer lists the
chosen coins that cannot be spent, because they are locked or have too few
confirmations, each with the reason. A coin that --select left because the
payment did not need it is not in that list. When no chosen coin can be
spent, preparetx prepares nothing.

The change goes to a new change address of the portfolio. The type of that
address is the first of these that applies:

  1. The type that --change-type asks for.
  2. The type of the coins the payment spends, when they all have one type.
     A payment then stays within one type of address.
  3. The default-type of the configuration file, or bip44, when the payment
     spends coins of both types.

On Bitcoin Cash every coin is bip44, so the change is too, and hw refuses
--change-type bip84.

The new change address has no tag, whatever tags the coins spent have. With
--tag-change, the change address gets the tags that all the coins the payment
spends share. A coin has the tags of its address. The change of a payment
that spends the coins of one group then stays in that group:

  hw preparetx ADDRESS:0.01 --spend-from-tag kyc --tag-change

`balances` --tag kyc still counts that change, and the next --spend-from-tag
kyc spends it. --tag-change also works without coin control: the change
address gets the tags that the selected coins share. A tag that one of the
coins lacks is left out. So when a payment mixes the coins of a group with
other coins, its change leaves the group, and hw says that --tag-change put
no tag on it. The answer shows the tags under the change address. untag takes
a tag off, and deletetx takes the tags off when
it deletes the transaction. hw refuses --tag-change with --change-back, since
the address of --change-back keeps the tags it has. For the payments you made
before, `tag` --with-change tags their change.

--change-back ADDRESS sends the change to that address instead of a new
change address. Use it to keep the funds at an address you chose: some banks
accept payments only from addresses they have checked. The address must be an
address of the active portfolio on the network of the payment. It can be a
receiving address or a change address, of either type. You can give its path
instead, such as bip84/1/0. The address may have been used already, or be the
change address of another pending transaction, since you chose it. hw refuses
any other address: change sent elsewhere would be a payment. An address has a
type of its own, so --change-back ADDRESS takes no --change-type. Every form
of --change-back sends the change to an address you already have. That
address keeps the tags it has, and gets no new tag.

--change-back largest sends the change back to the address of the largest
coin the payment spends. You cannot name that address yourself, since you do
not know in advance which coins the strategy selects. Only the coins that
have the type of the change count. That type follows the rule above, so that
the fee is counted for the right kind of output. When two coins have the same
value, hw takes the one at the lower path, and a receiving address before a
change address. With --change-type, hw refuses a payment that spends no coin
of that type, and prepares nothing. A payment that has no change sends
nothing back, and hw never refuses it for that. With --spend-from-tag, the
change goes back to an address that has the tag. So --spend-from-tag bank
--change-back largest pays from the addresses a bank has checked, and keeps
the change at one of them.

--change-back smallest works the same way with the smallest coin. It sends
the change back to the address of the smallest coin the payment spends, by
the same rules.

Nodes do not relay an output that is too small. The limit is 546 satoshis for
a legacy P2PKH output, and 294 for a native SegWit P2WPKH output. hw refuses
a payment below the limit, and adds change below the limit to the fee.

preparetx gives the transaction a lock time: the height of the best block the
wallet knows. The transaction cannot be mined into a block below that height.
Most wallets set the lock time this way, so the transaction looks like
theirs. One time in ten, the lock time is up to 99 blocks lower, so that it
does not show how long the signing took. When the best block was mined more
than eight hours ago, the wallet is behind the chain, and a sync may find
coins that were spent or received since. preparetx then asks whether to
prepare the transaction anyway, with a lock time of 0.

A fee rate above 100 satoshis per virtual byte is more often a typo than a
choice, so preparetx asks before it takes one.

preparetx also asks before a payment spends coins that do not all have the
same tags. A tag names a group of coins, such as kyc for the coins bought at
an exchange. The selection picks coins by their value, whatever their tags,
so a payment may spend coins of several groups. A transaction shows that its
coins have one owner, so such a payment joins the groups for good. preparetx
names each group, with its coins and what they are worth, and asks before it
prepares anything. The coins without a tag are a group too. With any answer
but yes, preparetx prepares nothing. To pay with the coins of one tag only,
use --spend-from-tag: preparetx then asks only when a coin spent does not
have a tag that the option names.

--yes answers yes to these three questions in advance, for scripts.
|],
      args = CommandPrepareTx <$> paymentOptions outputFileMaybeOption txLabelOption
    }

-- | The options of a payment, which preparetx and send share. They are
-- written once, so that the two commands read the same options in the
-- same way, and send builds the payment preparetx would build.
--
-- The two options given are the ones that differ. preparetx writes its
-- transaction to an --output file, and send has no such option. The help
-- of --label says where the label goes, which is not to a file for send.
-- --tag is the same for both ('txTagsOption').
paymentOptions :: Args (Maybe FilePath) -> Args (Maybe Text) -> Args PrepareTx
paymentOptions fileOption txLabel =
  PrepareTx
    <$> some paymentArg
    <*> paymentNetworkOption
    <*> underFee feeOption
    <*> underFee rcptPayOption
    <*> underCoins minConfOption
    <*> underCoins coinTypeOption
    <*> underCoins coinSelectionOption
    <*> fileOption
    <*> txLabel
    <*> txTagsOption
    <*> underCoins (many coinOption)
    <*> underCoins (many spendFromOption)
    <*> underCoins (many spendFromTagOption)
    <*> underCoins selectOption
    <*> underFee prepareYesOption
    <*> underChange changeBackOption
    <*> underChange changeTypeOption
    <*> underChange tagChangeOption

-- | The options of the fee of a payment or a sweep, under their
-- subheading of the help
underFee :: Args a -> Args a
underFee = optionGroup "Fee"

-- | The options that choose the coins a payment spends, under their
-- subheading of the help
underCoins :: Args a -> Args a
underCoins = optionGroup "Coins"

-- | The options that say where the change of a payment goes, under their
-- subheading of the help
underChange :: Args a -> Args a
underChange = optionGroup "Change"

-- | The network of a payment, which the coins chosen tell when they are
-- given
paymentNetworkOption :: Args (Maybe Network)
paymentNetworkOption =
  networkOptionWith "The payment spends the coins of the active portfolio on this network. Without it, the network is that of the coins and addresses that --coin and --spend-from name, or else the default-network of the configuration file, or btc. With --coin or --spend-from, you need it only to tell apart the test networks, which share their keys."

-- | A filter of coin control: the one type of coins a payment may spend.
-- It narrows every coin of the portfolio, or the coins the choosers
-- chose, and never adds one. The help says so, since a user may read it
-- as one more way to choose coins, adding up with --spend-from.
coinTypeOption :: Args (Maybe AccountKind)
coinTypeOption =
  optional . option (eitherReader (readKind . cs)) $
    long "coin-type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help "Filter: spend only the coins of this type, bip44 or bip84. It applies to every coin of the portfolio, or to the coins that --coin, --spend-from and --spend-from-tag choose. hw refuses a coin of the other type that --coin or --spend-from names. Without it, the payment spends coins of both types."

-- | Where the change of preparetx goes in place of a new change address:
-- an address of the portfolio, or that of the largest or of the smallest
-- coin spent. The shell is offered the keywords with the addresses.
changeBackOption :: Args (Maybe ChangeBack)
changeBackOption =
  optional . option (eitherReader readChangeBack) $
    long "change-back"
      <> metavar "ADDRESS|PATH|largest|smallest"
      <> completeWith ["largest", "smallest"]
      <> completer (mkCompleter activeAddresses)
      <> help "Send the change to this address instead of a new change address. A path names the address too, such as bip84/1/0. It must be an address of the active portfolio, on the network of the payment. It may have been used already, or be the change address of another pending transaction. The word largest sends the change back to the address of the largest coin the payment spends, and smallest to the address of the smallest one. Only the coins that have the type of the change count. Without it, the change goes to a new change address."

-- | Whether the new change address of a payment takes the tags of the
-- coins spent. It is a switch the user sets for a payment, and not what
-- every payment does: only the user knows whether a tag says something of
-- the coins, which their change is still of, or of the address alone.
tagChangeOption :: Args Bool
tagChangeOption =
  switch $
    long "tag-change"
      <> help "Tag the new change address with the tags that all the coins the payment spends share, so that the change of a group of coins stays in the group. A coin has the tags of its address. A tag that one of the coins lacks is left out. Without it, the change address gets no tag. hw refuses it with --change-back, whose address keeps its tags."

-- | The type of the change of preparetx, when the command says
changeTypeOption :: Args (Maybe AccountKind)
changeTypeOption =
  optional . option (eitherReader (readKind . cs)) $
    long "change-type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help "The type of address the change goes to, bip44 or bip84. It is the type of the new change address, or of the coin that --change-back largest or smallest picks. Without it, the change has the type of the coins spent when they all have one type, or else the default-type of the configuration file, or bip44."

-- | A recipient of preparetx, ADDRESS:AMOUNT
paymentArg :: Args Text
paymentArg =
  strArgument $
    metavar "ADDRESS:AMOUNT"
      <> completer (mkCompleter (recipientContacts WithAmount))
      <> help
        [r|
A recipient and the amount it gets. Repeat for several recipients. The amount
is in the unit of --unit or of config.toml, bitcoins by default, unless it ends
with btc, bit, bits, sat or sats. ADDRESS:0.00001, ADDRESS:10bits and
ADDRESS:1000sat pay the same. Bitcoins take up to 8 decimals, bits up to 2, and
satoshis none. Separators such as 1'000 are refused. A Bitcoin Cash address
can keep its prefix, as in bitcoincash:qp...:0.001. The Tab key completes the
address of a contact of the address book, when you type the start of the
address or, in bash, the start of its label.
|]

{- send -}

-- | send: preparetx, signtx and broadcast in one command. It is not marked
-- as a command to run offline, since it needs the server. Its help warns
-- that the mnemonic is typed on an online computer, as the help of
-- recover does. The help is the text sendall shares
-- ('inOneCommandDetails').
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

{- preparesendall -}

-- | preparesendall, which prepares a payment of all the coins of a
-- portfolio
prepareSendAllCommand :: CommandSpec Command
prepareSendAllCommand =
  CommandSpec
    { name = "preparesendall",
      offline = False,
      summary = "Prepare a payment of all the coins of a portfolio",
      details =
        [r|
preparesendall prepares a transaction that sends every coin the active
portfolio can spend now on one network. It sends the coins of both types of
address, and the transaction has no change. The amount sent is the spendable
balance that `portfolios` shows for that network. Like preparetx,
preparesendall needs no mnemonic and does not sign. hw keeps the transaction
as a pending transaction, for signtx to sign. sendall prepares the same
transaction, signs it and sends it in one command, with the same options, on
a computer where you type the mnemonic.

A recipient written with an amount, ADDRESS:AMOUNT, gets exactly that amount.
The recipients written without an amount share the rest in equal parts, and
they pay the fee. So give at least one recipient without an amount. When the
rest does not cover the fee, preparesendall prepares nothing. It never
reduces a fixed amount.

A dust coin is a coin worth no more than the fee to spend it. Spending it
lowers what the recipients get. Whether a coin is dust depends on the fee
rate. preparesendall lists the dust coins, explains what happens when you
spend them or leave them, and asks whether to spend them. Any answer but yes
leaves them in the portfolio. --spend-dust or --leave-dust answers in
advance, for scripts.

Coin control limits the coins that preparesendall sends. Its options are of
two kinds, choosers and filters, as for preparetx.

A chooser names coins to send. --coin names one coin, --spend-from the coins
at an address, and --spend-from-tag the coins at the addresses that have a
tag. The choosers add up. With a chooser, preparesendall sends only the
chosen coins: every one that can be spent, dust coins included. --spend-dust
and --leave-dust do not apply then. The chosen coins must be in the active
portfolio.

A filter narrows the coins sent, and never adds a coin. The filters are
--min-conf and --coin-type. A filter applies to every coin of the portfolio,
or to the chosen coins when you give a chooser.

For example, to move every legacy coin of a portfolio to a native SegWit
address of the same portfolio, give --coin-type bip44 and an address that
`receive` --type bip84 gave. To empty the addresses that have a tag, give
--spend-from-tag TAG. A tag can lock its coins. The coins of such a tag stay
in the portfolio, unless --spend-from-tag names that tag.

The answer lists every coin that the transaction leaves in the portfolio,
with the reason: dust, locked by you, by a tag or by a pending transaction,
not spendable yet, not chosen with --coin, --spend-from or --spend-from-tag,
or not of the type --coin-type sends.

preparesendall gives the transaction a lock time, the height of the best
block the wallet knows, as preparetx does. When that block was mined more
than eight hours ago, the wallet is behind the chain. preparesendall then
asks whether to prepare the transaction anyway, with a lock time of 0.

A fee rate above 100 satoshis per virtual byte is more often a typo than a
choice, so preparesendall asks before it takes one.

When the coins it sends do not all have the same tags, preparesendall names
each group of coins and asks before it prepares anything. A transaction shows
that its coins have one owner, so it joins the groups for good. To send the
coins of one tag only, use --spend-from-tag: preparesendall then asks only
when a coin sent does not have a tag that the option names.

--yes answers yes to the last three questions in advance, for scripts.
--spend-dust or --leave-dust answers the question about the dust coins.
|],
      args = CommandPrepareSendAll <$> sendAllOptions outputFileMaybeOption txLabelOption
    }

-- | The options of a payment of all the coins, which preparesendall and
-- sendall share. They are written once, so that the two commands read the
-- same options in the same way, and sendall builds the transaction
-- preparesendall would build.
--
-- The two options given are the ones that differ, as for preparetx and
-- send ('paymentOptions'). preparesendall writes its transaction to an
-- --output file, and sendall has no such option. The help of --label says
-- where the label goes, which is not to a file for sendall.
sendAllOptions :: Args (Maybe FilePath) -> Args (Maybe Text) -> Args PrepareSendAll
sendAllOptions fileOption txLabel =
  PrepareSendAll
    <$> some sendAllRecipientArg
    <*> paymentNetworkOption
    <*> underFee feeOption
    <*> underCoins minConfOption
    <*> underCoins coinTypeOption
    <*> fileOption
    <*> txLabel
    <*> txTagsOption
    <*> underCoins (many coinOption)
    <*> underCoins (many spendFromOption)
    <*> underCoins (many spendFromTagOption)
    <*> underCoins (dustCoinsOption "in the portfolio")
    <*> underFee prepareYesOption

{- sendall -}

-- | sendall: preparesendall, signtx and broadcast in one command. It is to
-- preparesendall what send is to preparetx: the same steps after the
-- transaction is prepared, and the same help but for the words about that
-- transaction ('inOneCommandDetails'). Like send, it is not marked as a
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

-- | A recipient of preparesendall: an address alone, which shares what
-- remains, or an address with its amount
sendAllRecipientArg :: Args Text
sendAllRecipientArg =
  strArgument $
    metavar "ADDRESS[:AMOUNT]"
      <> completer (mkCompleter (recipientContacts AddressAlone))
      <> help
        [r|
A recipient. An address alone shares what remains with the other recipients
that have no amount. ADDRESS:AMOUNT gets a fixed amount, written as for
preparetx. Repeat for several recipients, and give at least one without an
amount. The Tab key completes the address of a contact of the address book,
when you type the start of the address or, in bash, the start of its label.
|]

-- | What preparesendall, sendall and preparesweep do with the coins worth
-- less than their fee, without asking. The help says where the coins stay
-- when they are left: in the portfolio, or at the addresses swept.
dustCoinsOption :: String -> Args (Maybe DustCoins)
dustCoinsOption staying =
  optional $
    flag' SpendDust (long "spend-dust" <> help "Spend the dust coins too, without asking. A dust coin is worth no more than the fee to spend it.")
      <|> flag' LeaveDust (long "leave-dust" <> help ("Leave the dust coins " <> staying <> ", without asking"))

-- | A chooser of coin control: an address of the portfolio whose coins to
-- spend. Its coins add up with those of the other choosers.
spendFromOption :: Args AddressNamed
spendFromOption =
  option (eitherReader readAddressNamed) $
    long "spend-from"
      <> metavar "ADDRESS|PATH"
      <> completer (mkCompleter addressesToSpendFrom)
      <> help "Chooser: spend the coins at this address of the active portfolio. A path names the address too, such as bip44/1/0. Repeat for several addresses. Their coins add up with the coins of --coin and --spend-from-tag."

-- | A chooser of coin control: a tag whose addresses have their coins
-- spent. It is --spend-from for a whole group of addresses, named once.
-- Its coins add up with those of the other choosers.
--
-- The tag has an option of its own, and is not one more word --spend-from
-- reads. A mistyped address would otherwise be looked for as a tag, and
-- the refusal would be about a tag the user never meant. The shell also
-- offers tags here and addresses there, each list short.
spendFromTagOption :: Args Tag
spendFromTagOption =
  option tagReader $
    long "spend-from-tag"
      <> metavar "TAG"
      <> completer (mkCompleter tagNames)
      <> help "Chooser: spend the coins at every address of the portfolio that has this tag. Repeat for several tags: the coins of all of them are chosen. They add up with the coins of --coin and --spend-from. It is the only way to spend the coins of a tag that locks its coins."

-- | Whether the strategy selects among the coins the choosers chose,
-- instead of every one of them being spent. It is neither a chooser nor a
-- filter: it changes what is done with the coins chosen.
selectOption :: Args Bool
selectOption =
  switch $
    long "select"
      <> help "With --coin, --spend-from or --spend-from-tag, spend only the coins the payment needs among the chosen coins, instead of all of them. --strategy selects them."

-- | A chooser of coin control: a coin to spend, as coins lists it, in full
-- or by its short id. It adds up with the coins of the other choosers.
coinOption :: Args CoinRef
coinOption =
  option coinReader $
    coinOptionFields "Chooser: spend this coin. A chooser names coins to spend, and the coins of all the choosers add up: --coin, --spend-from and --spend-from-tag. Give the coin as `coins` lists it, by its short id or with its txid in full. Repeat for several coins."
      <> completer (mkCompleter coinsToSpend)

-- | A coin to sweep, by its outpoint, with its txid in full. The wallet
-- does not hold it, and has none to offer.
sweepCoinOption :: Args OutPoint
sweepCoinOption =
  option fullCoinReader $
    coinOptionFields "A coin to sweep: the id of its transaction in full, a colon, and its index among the outputs. Repeat for several coins."

-- | The name, the metavar and the help of the --coin option, with the
-- help of the command about it
coinOptionFields :: String -> Mod OptionFields a
coinOptionFields what =
  short 'c'
    <> long "coin"
    <> metavar "TXID:INDEX"
    <> help what

-- | The fee rate, when given: a number of satoshis per virtual byte, or a level
-- of the rates recent blocks paid
feeOption :: Args (Maybe FeeRateChoice)
feeOption =
  optional . option (eitherReader readFeeRateChoice) $
    short 'f'
      <> long "fee-rate"
      <> metavar "RATE"
      <> completeWith ["low", "median", "high"]
      <> help "The fee rate: a number of satoshis per virtual byte, or low, median or high for the rates that recent blocks paid, which `fees` shows. Without it, hw shows those rates and asks for a level or a number, median by default. hw asks you to confirm a rate above 100."

-- | Whether preparetx, send, preparesendall and sendall take a high fee
-- rate, prepare while the wallet is behind the chain, and spend together
-- coins that do not all have the same tags, without asking
prepareYesOption :: Args Bool
prepareYesOption = yesOption "Answer yes in advance to the three questions the command may ask: about a fee rate above 100 satoshis per virtual byte, about a wallet that is behind the chain, and about coins that do not all have the same tags"

-- | Whether preparesweep takes a high fee rate without asking
highFeeYesOption :: Args Bool
highFeeYesOption = yesOption "Take a fee rate above 100 satoshis per virtual byte without asking"

-- | Whether the recipients of a payment pay its fee (--recipients-pay)
rcptPayOption :: Args FeePayer
rcptPayOption = recipientsPayOption "The recipients pay the fee, in equal shares taken from their amounts"

-- | --recipients-pay, with the help given. preparetx and send take it for
-- the fee of a payment, and bumpfee for what the change of a stuck one
-- cannot pay of the higher fee. It means the same in each: the recipients
-- pay in equal shares, the first also paying the remainder.
recipientsPayOption :: String -> Args FeePayer
recipientsPayOption what =
  flag SenderPays RecipientsPay $
    short 'r'
      <> long "recipients-pay"
      <> help what

-- | A filter of coin control: the confirmations a coin needs to be spent.
-- Like --coin-type, it narrows every coin of the portfolio, or the coins
-- the choosers chose. A chosen coin it leaves out is listed with why,
-- not refused, since a coin gains confirmations while the user waits.
minConfOption :: Args Natural
minConfOption =
  option (maybeReader $ readNatural . cs) $
    short 'm'
      <> long "min-conf"
      <> metavar "INT"
      <> value 0
      <> showDefault
      <> help "Filter: spend only the coins that have at least this many confirmations. A filter narrows the coins that may be spent and never adds one. It applies to every coin of the portfolio, or to the coins that --coin, --spend-from and --spend-from-tag choose. At 0, the payment may spend unconfirmed coins that the wallet's own transactions made: its change, and what its portfolios pay each other. It never spends an unconfirmed payment from somebody else."

-- | --strategy of preparetx: how the coins of the payment are chosen
coinSelectionOption :: Args CoinSelection
coinSelectionOption =
  option (eitherReader f) $
    long "strategy"
      <> metavar "STRATEGY"
      <> value LeastWaste
      <> showDefaultWith strategyName
      <> completeWith (strategyName <$> strategies)
      <> help
        [r|
How hw selects the coins to spend. least-waste tries the other strategies,
and an exact match that needs no change. It keeps the coins that waste the
least fee, now and later. random-draw spends coins in random order.
largest-first spends the fewest coins, for the lowest fee now. smallest-first
spends the smallest coins first, to consolidate them. With --coin,
--spend-from or --spend-from-tag, the payment spends every chosen coin and
the strategy does not apply, unless --select is given.
|]
  where
    strategies = [LeastWaste, RandomDraw, LargestFirst, SmallestFirst]
    f s =
      maybeToEither ("Invalid strategy. Select one of the following: " <> names) $
        find ((== s) . strategyName) strategies
    names = intercalate ", " (strategyName <$> strategies)

-- | The name of a coin selection strategy on the command line
strategyName :: CoinSelection -> String
strategyName LeastWaste = "least-waste"
strategyName RandomDraw = "random-draw"
strategyName LargestFirst = "largest-first"
strategyName SmallestFirst = "smallest-first"

-- | --output of the commands that prepare or sign a transaction: the file
-- to write it to, as a PSBT
outputFileMaybeOption :: Args (Maybe FilePath)
outputFileMaybeOption =
  optional . strOption $
    short 'o'
      <> long "output"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "Write the transaction to this file, as a PSBT (BIP 174). The file must not exist yet."

-- | The label of a transaction to prepare. -l labels an address for
-- receive, and a transaction here: one letter, one meaning.
txLabelOption :: Args (Maybe Text)
txLabelOption = txLabelOptionSaying "The --output file carries the label, and the label stays on the transaction once the network has it."

-- | The label of a transaction send or sendall sends, which has no
-- --output file to go to
sentLabelOption :: Args (Maybe Text)
sentLabelOption = txLabelOptionSaying "The label stays on the transaction once the network has it."

-- | --label for a transaction, with what the help of the command says of
-- where the label goes
txLabelOptionSaying :: String -> Args (Maybe Text)
txLabelOptionSaying whereItGoes =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "LABEL"
      <> help ("A label for the transaction, to recognize it later. " <> whereItGoes)

-- | The tags of a transaction to prepare or to send: --tag, once for
-- each tag. The four commands that build a payment take it, with one
-- help. The tags are kept in the wallet and in no file, so the help has
-- nothing to say that differs between a command with an --output file
-- and one without, as the help of --label has.
--
-- The moment a user pays is the moment they know what the payment is for. A
-- tag put later, with the command tag and the id of the transaction, is the
-- step that is skipped.
--
-- The help tells --tag from --spend-from-tag, which is also a tag on the
-- line of a payment. The first says what the payment is. The second
-- chooses the coins that pay it.
txTagsOption :: Args [Tag]
txTagsOption =
  many . tagOption $
    "A tag for the transaction, to say what the payment is for. The command `tag` puts one in the same way afterwards. Repeat for several tags. hw creates a tag that does not exist yet. The tag stays on the transaction once the network has it. It does not choose the coins to spend: --spend-from-tag does."

{- pendingtxs -}

-- | pendingtxs, which lists the pending transactions
pendingTxsCommand :: CommandSpec Command
pendingTxsCommand =
  CommandSpec
    { name = "pendingtxs",
      offline = False,
      summary = "List the pending transactions",
      details =
        [r|
A pending transaction is one that preparetx, preparesendall, bumpfee or
preparesweep made, or that importtx added, and that the network does not list
yet. It is not signed, signed and not sent, or sent. A sent transaction leaves
this list once a sync finds it on the network. The command that sends it runs
that sync, so a sent transaction stays here only when that sync failed, and
`sync` then takes it off. `show` HASH shows one pending transaction in full.

pendingtxs lists the pending transactions of every portfolio of every wallet,
not those of the active portfolio alone. Each one waits for you to sign, send
or delete it. The commands that take a pending transaction find its portfolio
by themselves, so none needs a switch first. The list is sorted by wallet and
portfolio, then by network, in the order portfolios shows the networks. On
each network, the newest transaction comes first, or the oldest with
--reverse. --network shows the transactions of one network only.

In the compact view, each transaction takes two lines. The first line starts
with the short id of the transaction: the first 8 digits of its hash. For a
transaction that is not signed, the hash is its nosigHash, the hash of the
transaction without its signatures. For a signed one, it is its txid. The
other commands take that hash, in full or by its short id, to name a pending
transaction. The nosigHash of a signed transaction works too. --verbose gives
the hash in full, then its short id. The first line then gives the time the
transaction was created, its amount, its status (not signed, signed and not
sent, or sent) and its fee rate.

The second line says where the transaction is: its portfolio and its network.
When the database has more than one wallet, it names the wallet too, as in
portfolio0 of wallet1. A replacement that bumpfee made then names the
transactions it replaces, in yellow on a terminal. The network keeps only one
of a replacement and the transactions it replaces. Then comes what the
transaction is for, in the words of txs: its label, or else the address it
pays. The line names an address of the wallet by its type and its derivation
path. The tags of the transaction end the line. --verbose names the replaced
transactions in full, in yellow too.

Three options filter the listing, as they do for txs. --tag TAG shows only the
pending transactions that have the tag. --address-tag TAG shows only those
that spend from or pay an address of their portfolio that has the tag, and
those that pay or were paid from a contact of the address book that has it.
--untagged shows only those left to sort: those that have no tag, and that
neither pay a contact that has a tag nor were paid by one. `tag` puts tags on
a pending transaction, by either of its hashes. The transaction keeps them
once the network has it.

The status conflicted means that the transaction spends a coin that another
transaction has spent, or a coin that no longer exists, as the server said at
the last sync. The transaction cannot be sent while that lasts. Delete it with
deletetx, which frees its coins.

With --tabular, each transaction takes one row of a table, under a header that
names the columns: its short id, its wallet when there are several, its
portfolio and its network, the time it was created, its type, its amount, its
status, its fee rate in sat/vB, its tags, and what it is for, as txs --tabular
says it. The description of a replacement starts with the transactions it
replaces, in yellow.

With --csv FILENAME, hw writes the pending transactions to a new CSV file for
a spreadsheet, one on each row. The file has the columns of txs --csv that
apply to a pending transaction, in the same order. The nosigHash comes before
the txid, each in full and by its short id. The file has no time on the chain,
no balance, no confirmations and no block. It adds two columns: the change
that was too small for an output and that the fee took in, and the
transactions that a replacement replaces. The time the transaction was created
comes last. Amounts are in satoshis. Each time takes three columns: the time
in UTC, the time in local time, and the offset of that local time from UTC.
|],
      args =
        CommandPendingTxs
          <$> networkOptionWith "List only the pending transactions on this network. Without it, list those on every shown network of every portfolio."
          <*> txTagFilterOptions
          <*> listingOutputOption
            txVerboseHelp
            "Show each transaction on one row of a table: its short id, wallet when there are several, portfolio, network, time, type, amount, status, fee rate and tags, then what it is for."
            "pending transactions"
          <*> pagingOptions (afterOption "HASH" hashRefReader "Start after this pending transaction, by either of its hashes or its short id")
    }

{- exporttx -}

-- | exporttx, which writes a pending transaction to a file
exportTxCommand :: CommandSpec Command
exportTxCommand =
  CommandSpec
    { name = "exporttx",
      offline = False,
      summary = "Write a pending transaction to a file",
      details = importExportTxDetails,
      args =
        CommandExportTx
          <$> pendingHashArg
          <*> fileArgument "The file to write. It must not exist yet."
    }

-- | The details that the helps of exporttx and importtx share: the two
-- commands are the two ends of one file
importExportTxDetails :: String
importExportTxDetails =
  [r|
exporttx writes a pending transaction to a file, signed or not, to carry it to
another computer, such as the offline computer that signs it. Name the
transaction by the hash that pendingtxs shows: its nosigHash, or its txid once
it is signed, in full or by its short id.

The file is a PSBT, the format of BIP 174, which most wallets read. It
carries the transaction, the whole transaction that each of its coins comes
from, the public keys of the portfolio in it with their derivation paths, and
the signatures once it is signed.

hw adds what the format has no field for, so that the offline computer can
show it: the network of the transaction, the fee rate it was built for, the
transactions that a replacement replaces, the label of the transaction, the
labels of the addresses of the portfolio in it, and the labels of the coins it
spends, as the wallet has them when it writes the file. They go in the
proprietary fields of the format, under the identifier hw, which other
software ignores. bitcoin-cli decodepsbt shows the file too.

importtx adds the transaction of a file to the pending transactions of a
portfolio, and locks the coins it spends. When the wallet holds the
transaction already, the file takes its place, as when an offline computer
returns it signed. hw checks the signatures first. The wallet keeps its own
labels, and shows the transaction with them. A file that brings nothing new
changes nothing, and is no error: importtx says that the wallet holds the
transaction already.

importtx takes a file that preparetx wrote on another computer that holds the
portfolio, once this one is synced and has the coins the transaction spends.
It generates the new change address of the payment from the path the file
gives, up to a thousand addresses past the last used address of its chain,
and holds it for the transaction.

hw finds the portfolio by itself, from the addresses the transaction spends
from and pays. A transaction that moves funds between two portfolios goes to
the portfolio that pays. In two cases hw cannot tell which portfolio it is:
when several portfolios pay, and when none pays and the transaction pays
several. It then takes the active portfolio if that is one of them. Otherwise
it names them, and imports nothing: switch to the one to use, and import the
file again.

importtx takes a file that another wallet or a hardware wallet signed the
same way. The file can be in bytes or in base64 text, finalized or not. hw
finalizes an input that is signed and not finalized, and refuses a signature
that does not verify. Such a file may leave out the paths of the addresses of
the portfolio, once the signatures are final, the fee rate and the label. The
wallet keeps them from the version it holds. importtx also takes a file that
another wallet prepared for a portfolio of this wallet. Sign it with signtx.

broadcast --input sends a signed file without importtx.
|]

-- | A pending transaction to sign, by its nosigHash, in full or by its
-- short id
nosigHashArg :: Args HashRef
nosigHashArg =
  argument hashRefReader $
    metavar "NOSIGHASH"
      <> completer (mkCompleter hashesToSign)
      <> help "The nosigHash of a pending transaction, as pendingtxs shows it: in full, or its short id, the first 8 digits"

-- | A pending transaction, by either of its hashes, in full or by its
-- short id
pendingHashArg :: Args HashRef
pendingHashArg =
  argument hashRefReader $
    metavar "HASH"
      <> completer (mkCompleter pendingHashes)
      <> help "A pending transaction, by the hash pendingtxs shows: its nosigHash, or its txid once it is signed, in full or by its short id, the first 8 digits"

-- | A signed pending transaction to send, by its txid. Its nosigHash works
-- too, as the user may have noted it before signing. Either goes in full
-- or by its short id.
signedTxidArg :: Args HashRef
signedTxidArg =
  argument hashRefReader $
    metavar "TXID"
      <> completer (mkCompleter idsToSend)
      <> help "The txid of a signed pending transaction, as signtx and pendingtxs show it, in full or by its short id, the first 8 digits. Its nosigHash works too."

{- importtx -}

-- | importtx, which adds a transaction file to the pending transactions
importTxCommand :: CommandSpec Command
importTxCommand =
  CommandSpec
    { name = "importtx",
      offline = False,
      summary = "Add a transaction file to the pending transactions",
      details = importExportTxDetails,
      args =
        CommandImportTx
          <$> fileArgument "The transaction file to add"
    }

{- deletetx -}

-- | deletetx, which deletes a pending transaction and frees its coins
deleteTxCommand :: CommandSpec Command
deleteTxCommand =
  CommandSpec
    { name = "deletetx",
      offline = False,
      summary = "Delete a pending transaction and free its coins",
      details =
        [r|
deletetx removes a pending transaction from the wallet. The coins it spends
can then be spent again, and its change address is free again. That address
loses its tags, since the next payment takes it for its own change.

A transaction sent with broadcast can be deleted only once the server no longer
knows it. That happens when the nodes dropped it from their mempools, or never
accepted it. While the server lists the transaction, it may still confirm, and
the next `sync` takes it off the pending transactions.

Delete a transaction that pendingtxs marks as conflicted. Another transaction
spent one of its coins, or the coin no longer exists, so it can no longer be
sent.

deletetx asks before it deletes, and says whether the transaction is signed
or was sent. Any answer but y or yes deletes nothing. --yes answers the
question in advance, for a script.
|],
      args = CommandDeleteTx <$> pendingHashArg <*> deleteYesOption
    }

{- signtx -}

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
this before it asks for the mnemonic. No option answers this question, so a
script must give --output. Otherwise the question takes the line of the
mnemonic as its answer, reads it as a no, and signtx stops.

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

-- | --input of the commands that sign or send a transaction: the file to
-- read it from
inputFileMaybeOption :: Args (Maybe FilePath)
inputFileMaybeOption =
  optional . strOption $
    short 'i'
      <> long "input"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "Read the transaction from this file"

{- coins -}

-- | coins, which lists the coins of a portfolio
coinsCommand :: CommandSpec Command
coinsCommand =
  CommandSpec
    { name = "coins",
      offline = False,
      summary = "List the coins of a portfolio",
      details =
        [r|
A coin is an unspent output of a transaction, written TXID:INDEX. `coins`
lists every coin of the active portfolio, grouped by network. On btc, the
bip44 and the bip84 coins are listed together, since a payment spends both.
--network and --type show one network or one type only.

On each network, the unconfirmed coins come first, then the confirmed ones
from the newest to the oldest. --reverse lists the oldest coins first, and
the unconfirmed ones last.

Until the portfolio has synced on a network, hw has asked no server about
its addresses there, and has no coin to list. When the portfolio has
addresses on such a network, the listing shows one line for it, which names
the network and reads not synced. This does not say that the addresses hold
nothing. --json gives the group of such a network "synced": false, and no
coin. A computer that signs offline never syncs: read the coins on the
online computer.

Each coin takes two lines. The first line starts with the short id of the
coin: the first 8 digits of its txid, a colon, and its index, such as
a1075db5:1. lockcoin, --coin and `show` take the short id, or the coin in
full. Then come the time of the transaction that paid the coin, in the time
zone of the computer, its value, its state and its confirmations, up to 999+
confirmations. The second line gives the address that holds the coin, then
its path, such as bip84/0/3, which starts with the type of the account. Then
come the label of the address and its tags, each between brackets. The
address shows its type too: a bip84 one starts with bc1q on Bitcoin. A coin
at a change address says so on that line.

The state says whether the wallet can spend the coin now. A spendable coin
meets three conditions. Nothing locks it. If it is unconfirmed, it comes
from a transaction of the wallet: it is its change, or what another
portfolio of the wallet paid. If it is a coinbase coin, it has 100
confirmations. The other coins become spendable once they confirm, or for a
coinbase coin, once it has 100 confirmations.

Three things lock a coin. A pending transaction locks the coins it spends,
until the network has it or deletetx deletes it. A replacement, which
bumpfee prepares, also locks the coins of the payment it replaces: they go
away once the replacement is sent. Such a coin says "locked by tx". You lock
a coin yourself with lockcoin, until unlockcoin frees it. A
tag that locks its coins locks every coin at the addresses that have the
tag. Such a coin says "locked by tag": a payment made with --spend-from-tag
TAG spends it, and hw tag TAG --unlock-coins frees the coins of the tag.

--verbose starts each coin with the time of its transaction, and gives the
full count of its confirmations on its first line. A coin that a pending
transaction spends says "locked by a pending transaction", and a coin of a
payment that a pending transaction replaces says "locked by a pending
replacement". The fields of the coin follow: the label of the transaction
that paid it, the coin in full and its short id, the block that holds it,
the time of its transaction to the second, and the address in full with its
path. The label and the tags of the address are on the line below the
address. An unconfirmed coin is in no block yet. A coin has no label and no
tag of its own: it is in the groups of its address. The change of a payment
you made shows no label of its transaction, since that label says what the
payment was for, not what its change is.

--tabular shows each coin on one row of a table, under a header that names
the columns: the short id of the coin, the time of its transaction, its
value, its state, its confirmations up to 999+, the address that holds it
with its path, the tags of the address, and its label. A coin locked by a
pending transaction says "locked by tx".

--tag TAG lists only the coins at the addresses that have the tag. Repeat it
for several tags: the listing then shows the coins at the addresses that
have any one of them, and its first line names those tags. These are the
coins that --spend-from-tag spends.

--untagged lists only the coins at the addresses that have no tag: the coins
that no --spend-from-tag chooses. With --tag as well, the listing shows
both.

--csv FILENAME writes the coins to a new CSV file, for a spreadsheet, one
coin on each row. The columns are its wallet, portfolio, network and type,
the coin as TXID:INDEX, in full and by its short id, its txid in full and by
its short id, the index of its output, the time of its transaction, its
value, its state, its confirmations and block, the address that holds it
with its path, chain, label and tags, and the label of its transaction.
Amounts are in satoshis. Each time takes three columns: in UTC, in local
time, and the offset of that local time from UTC.
|],
      args =
        CommandCoins
          <$> listingTargetOption
          <*> lockedOption
          <*> addressTagFilterOption "coins at an address that has"
          <*> listingOutputOption
            "Show every detail of each coin: the full count of its confirmations, the label of its transaction, the coin in full and its short id, the block that holds it, the time of its transaction, and its address in full with its path."
            "Show each coin on one row of a table: its short id, the time of its transaction, its value, state and confirmations, its address and path, and the tags and the label of the address."
            "coins"
          <*> pagingOptions (afterOption "TXID:INDEX" coinReader "Start after this coin, with its txid in full or by its short id")
    }

-- | --locked of coins: only the locked coins
lockedOption :: Args Bool
lockedOption =
  switch $
    long "locked"
      <> help "List only the locked coins: those a pending transaction locks, those locked with lockcoin, and those a tag locks"

{- lockcoin and unlockcoin -}

-- | lockcoin, which keeps a coin from being spent
lockCoinCommand :: CommandSpec Command
lockCoinCommand =
  CommandSpec
    { name = "lockcoin",
      offline = False,
      summary = "Keep a coin from being spent",
      details =
        [r|
lockcoin keeps one coin from being spent. preparetx and preparesendall leave
a locked coin out, and --coin refuses it. No imported transaction and no
sweep may spend it either. signtx and broadcast refuse a transaction that
spends it, so a file prepared before the lock, or on another computer, does
not spend it. unlockcoin frees it.

lockcoin refuses a coin that is locked already, by lockcoin or by a pending
transaction: one that spends it, or one that replaces the payment it comes
from. It names that transaction. deletetx frees the coins of a pending
transaction. The lock stays while the coin confirms. It goes away with the
coin if a transaction made elsewhere spends the coin. `coins` --locked lists
the locked coins.

Until a block holds a payment, its sender can replace it, to raise its fee.
The same money then arrives as another coin, with another id, at the same
address. sync moves the lock to that coin, and names both coins.

lockcoin locks one coin, and not the next coin that its address receives. To
hold back every coin of a group of addresses, the coins that arrive later
too, make the tag of the group lock its coins: hw tag TAG --lock-coins. The
two locks are separate. You can lock with lockcoin a coin that a tag locks.
The coin then stays locked even for a payment that names the tag with
--spend-from-tag.

lockcoin shows the coin it locked with every detail, as `coins` --verbose
does.

lockcoin finds the coin in any portfolio, whichever is active. --network is
needed only when the wallet has the same coin on several networks. That can
happen because the test networks share their keys. --network then says which
coin to lock.
|],
      args = CommandLockCoin <$> coinArg coinsToLock <*> heldNetworkOption "coin"
    }

-- | unlockcoin, which unlocks a coin locked with lockcoin
unlockCoinCommand :: CommandSpec Command
unlockCoinCommand =
  CommandSpec
    { name = "unlockcoin",
      offline = False,
      summary = "Unlock a coin locked with lockcoin",
      details =
        [r|
unlockcoin frees a coin that lockcoin locked, and no other. To free a coin
that a pending transaction locks, delete that transaction with deletetx. A
coin that a tag locks stays locked: a payment that names the tag with
--spend-from-tag spends it, and hw tag TAG --unlock-coins frees every coin
of the tag.

unlockcoin shows the coin it freed with every detail, as `coins` --verbose
does.
|],
      args = CommandUnlockCoin <$> coinArg coinsToUnlock <*> heldNetworkOption "coin"
    }

-- | A coin of the wallet, as coins lists it, with the coins the shell
-- offers for it
coinArg :: (String -> IO [String]) -> Args CoinRef
coinArg offered =
  argument coinReader $
    metavar "TXID:INDEX"
      <> completer (mkCompleter offered)
      <> help "The coin, as `coins` lists it: the id of its transaction, in full or by its short id, a colon, and its index among the outputs"

-- | A coin as coins lists it, and as a block explorer often writes it: the
-- id of its transaction, in full or by its short id, a colon, and its
-- index among the outputs
coinReader :: ReadM CoinRef
coinReader = eitherReader $ \s -> maybe (Left (notACoin s)) Right (readCoinRef s)

-- | A coin, TXID:INDEX, with the txid in full or by its short id. Nothing
-- for any other word.
readCoinRef :: String -> Maybe CoinRef
readCoinRef s = case break (== ':') s of
  (hash, ':' : index) -> CoinRef <$> readHashRef (cs hash) <*> readOutputIndex index
  _ -> Nothing

-- | The refusal of a word that is not a coin
notACoin :: String -> String
notACoin s = "The coin " <> s <> " is not valid: write it TXID:INDEX, as coins lists it, the txid in full or by its short id"

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

-- | A hash as the user types it: in full, 64 hexadecimal digits, or by
-- its short id, its first 8 ("Haskoin.Wallet.ShortId")
hashRefReader :: ReadM HashRef
hashRefReader = eitherReader $ \s -> maybe (Left (notAHash s)) Right (readHashRef (cs s))
  where
    notAHash s = "The hash " <> s <> " is not valid: give its 64 digits, or its short id, its first 8 digits, as the listings show it"

-- | The index of an output in its transaction, which has 32 bits. A larger
-- number names no output, rather than wrapping around to another one.
readOutputIndex :: String -> Maybe Word32
readOutputIndex s = case readNatural (cs s) of
  Just n | n <= fromIntegral (maxBound :: Word32) -> Just (fromIntegral n)
  _ -> Nothing

{- bumpfee -}

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
does, and sends it only if you answer yes. --yes sends it without showing it
and without asking, for scripts.

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
          <*> yesOption "Send the transaction without showing it and without asking first"
          <*> fileNetworkOption
    }

-- | --yes, which answers yes to the question of a command without asking
-- it, for a script. The help says what the command then does.
yesOption :: String -> Args Bool
yesOption what =
  switch $
    short 'y'
      <> long "yes"
      <> help what

{- sync -}

-- | sync, which updates the portfolios from the network
syncCommand :: CommandSpec Command
syncCommand =
  CommandSpec
    { name = "sync",
      offline = False,
      summary = "Update the portfolios from the network",
      details =
        [r|
`sync` updates the portfolios from the server. It asks for the latest
balances, transactions and coins of every portfolio of every wallet, on every
network, the test networks included. Run it before you prepare a payment and
after you send one. For each portfolio and network that holds something or
where something changed, it shows the balances as `portfolios` does, the
block it is synced to, and what changed. --network syncs one network only.

On a terminal, a status line says what `sync` is doing while it works, with a
bar for the steps whose size it knows. The line goes to stderr and is gone
when `sync` ends, so scripts never see it.

`sync` also works on the networks that hw hides, so that a network is up to
date the day you show it with setdefault --show-network. The status line
names the hidden networks as `sync` works on them. The answer leaves them
out, and a hidden network whose server is down is no error.

`sync` first looks for used addresses that each account does not have yet.
Someone who pays to addresses derived from the public key of the account, or
another wallet of the same mnemonic, may have used them. `sync` looks past
the last used address of each account, as far as the key gap of the
configuration file says, 20 addresses by default. It adds each used address
that it finds, with the addresses before it, and then looks as far past that
one. So the first sync of a portfolio finds every address used so far. It
stops at 20 unused addresses in a row, as a wallet restored from the mnemonic
does, and misses a used address that comes after them. The count starts at
the last used address, not at the last address hw `receive` gave out. So two
computers with the same mnemonic and the same gap find the same payments. To
look further, raise the gap for one sync. The same gap limits how many unused
addresses in a row hw `receive` gives out.

When the server reports a thousand used addresses in a row, and more after
them, `sync` asks whether to keep looking. A server that lies reports used
addresses without end. The addresses that `sync` would then generate are past
any gap that a wallet restoring the mnemonic looks at. Answer yes if the
wallet was used that much. A no is for a report you do not believe: `sync`
stops, and keeps nothing of what it found. A `sync` that cannot ask stops
there, and generates none of them.

`sync` also checks each pending transaction. When the network lists the
transaction, it is no longer pending, and `sync` removes it from the pending
transactions. It does so even when the network lists it under another id, as
when a miner or another signer changed its signatures. When you sent the
transaction and the server no longer knows it, `sync` sends it again, since
nodes forget unconfirmed transactions. When another transaction spent one of
its coins, `sync` marks it conflicted. It does the same when one of its coins
is gone, as after a reorganization that removed the payment the coin came
from. `sync` leaves alone a signed transaction that you never sent.

After a conflict, the next steps that `sync` prints are `pendingtxs` and
deletetx. `pendingtxs` lists the pending transactions of every portfolio, so
it shows the conflicted one wherever it is. `txs` lists the transactions of
the active portfolio only. So when `sync` finds new transactions only in
other portfolios, its next steps start with the `switch` to them.

`sync` sends a sent transaction again on every sync while the server does not
list it, unless the transaction is conflicted. The server may answer that it
did not publish the transaction. `sync` then goes on. It names the
transaction with what the server said, and gives the deletetx command that
frees its coins. This happens on every sync for a transaction that the
network refuses for good, for example because its fee is below what the nodes
relay. A haskoin-store server waits for its nodes before it gives that
answer, 30 seconds by default, so such a sync takes that long. When a server
gives no answer at all, the sync of its network fails, as it does for any
other request.

A server seldom serves every network: the public one has no regtest chain,
for example. `sync` names in one line the networks that the server does not
serve, and syncs the others. That is no error.

When the server of a network is down, `sync` leaves out that network and says
why. It still syncs the other networks, then exits with an error, so that a
script can tell.

`sync` keeps two blocks of each network from one sync to the next: the block
it reached, and the block a hundred blocks below it. A reorganization replaces
the last blocks of a chain, and `sync` follows one by itself. It never reaches
a hundred blocks down. So when the server does not have the deeper block, the
server is far behind the wallet or on another chain: a new server that still
reads the chain, for example, or a host that names another server. Following
it would show the payments of the wallet as dropped from the network. `sync`
then leaves that network as it was and says why, as for a server that is down.
When the chain of the server is the right one, as after a test network was
reset, --new-chain follows it.
|],
      args =
        CommandSync
          <$> networkOptionWith "Sync only this network. Without it, sync every network, the hidden ones too."
          <*> (syncMode <$> fullOption <*> newChainOption)
    }

-- | The sync that --full and --new-chain ask for. A sync on a new chain
-- is a full one already, so --full adds nothing to it.
syncMode :: Bool -> Bool -> SyncMode
syncMode _ True = NewChain
syncMode True False = Full
syncMode False False = Incremental

-- | --new-chain of sync: follow a server whose chain is not the one of
-- the last sync
newChainOption :: Args Bool
newChainOption =
  switch $
    long "new-chain"
      <> help
        [r|
Follow the server of --network although its chain is not the one of the last
sync, and download everything again as --full does. `sync` refuses such a
server without it. Use it only when you know why the chain changed, as after a
test network was reset: the transactions that the new chain does not have are
marked as dead.
|]

-- | --full of sync: download everything again, in place of what changed
fullOption :: Args Bool
fullOption =
  switch $
    long "full"
      <> help
        [r|
Download all the balances, transactions and coins of the portfolios again,
instead of only what changed, and write them anew. Use it if the wallet shows
something wrong. It replaces any damaged copy of a raw transaction, and marks
as dead a transaction that the server no longer lists. It keeps what the
server cannot tell: labels, locked coins and pending transactions. A
reorganization of the chain does not need it: `sync` sees one by itself.
|]

{- backup -}

-- | backup, which copies the wallet database to a backup file
backupCommand :: CommandSpec Command
backupCommand =
  CommandSpec
    { name = "backup",
      offline = False,
      summary = "Copy the wallet database to a backup file",
      details =
        [r|
`backup` copies the whole database to a file: wallets, portfolios, addresses,
labels, tags, coins, transactions and pending transactions. The copy holds no
private key.

Without a FILENAME, `backup` writes two files into ~/.hw/user-backups: the
copy of the database, and a file of its labels, which is the file
exportwallet writes. Both names give the time in UTC, and how many wallets
and portfolios the database holds. The name of the copy also gives the format
version of the database. hw never removes these files. With a FILENAME,
`backup` writes the copy of the database only.

`show` FILENAME lists the wallets and the portfolios of a copy, and does not
change it. Run it before you restore a backup, to see what comes back. Its
first line reads "SQLite database" for a copy of the database, and "BIP 329
file" for a file of labels, so that you can tell the two files of a backup
apart.

restoredatabase replaces the database with a copy. To restore the file of the
labels, give it to importwallet instead, which reads it into a database of
any version.

hw also copies the database and the labels into ~/.hw/auto-backups by itself.
It copies the database after each command that adds a wallet or a portfolio,
and the labels after each command that changes them. It copies both before
deletewallet, deleteportfolio and restoredatabase, and makes no copy after
these three. For each kind, it keeps the ten newest copies. It also keeps the
last copy of each hour for a day, of each day for a week, of each week for a
month, and of each month for a year. hw removes the other copies it made
there, and no other file. showbackups tells how many copies there are, and
how old the newest is. In config.toml, the keys autobackup-database and
autobackup-labels turn these copies off.
|],
      args =
        CommandBackup
          <$> optional (fileArgument "The file to copy the database to. It must not exist yet. Without it, the copy and its labels go to ~/.hw/user-backups.")
    }

{- showbackups -}

-- | showbackups, which shows the copies hw keeps. A copy that hw could
-- not make is a warning on stderr when it happens, which a script may
-- never show. Here the user sees later that the newest copy is older
-- than it should be. It also names the files, for the user who looks for
-- one to give to restoredatabase.
showBackupsCommand :: CommandSpec Command
showBackupsCommand =
  CommandSpec
    { name = "showbackups",
      offline = False,
      summary = "Show the backups hw keeps, and how old the newest are",
      details =
        [r|
showbackups tells what the two directories of backups hold, and changes
nothing:

  ~/.hw/auto-backups   the copies hw makes by itself, of the database and of
                       the labels
  ~/.hw/user-backups   the copies `backup` wrote

In each directory, and for each kind of copy, the database and the labels,
it gives the number of copies, the name of the newest one, and how long ago
hw made it. It also says when the key autobackup-database or
autobackup-labels of config.toml turns a kind off.

showbackups runs when the database is damaged too, since it reads the two
directories alone. restoredatabase then puts the newest copy back.

Run it to check that hw makes its copies. hw prints nothing when it makes a
copy, and warns on stderr when it cannot make one. When the newest copy is
older than your last change, hw missed a copy. Run `backup` to make one now.
A deletion and a restore are the exception: hw makes their copy right before
them, and none after.

`show` FILENAME lists the wallets and the portfolios of a copy of the
database, and restoredatabase FILENAME restores it.
|],
      args = pure CommandShowBackups
    }

{- restoredatabase -}

-- | restoredatabase, which puts a copy of the database back in place of
-- the database. Its name says what is restored. A backup has two files,
-- and the file of the labels goes to importwallet instead.
restoreDatabaseCommand :: CommandSpec Command
restoreDatabaseCommand =
  CommandSpec
    { name = "restoredatabase",
      offline = False,
      summary = "Put a copy of the database back in place of the database",
      details =
        [r|
restoredatabase replaces the whole database with a copy of it. Everything the
copy holds comes back: the wallets, their portfolios, the addresses, the
labels, the tags, the coins, the transactions and the pending transactions.
The copy is a file that `backup` wrote, or a .sqlite file of
~/.hw/auto-backups or ~/.hw/user-backups. showbackups names the newest copy
in each directory. Before you restore a copy, `show` FILENAME lists the
wallets and the portfolios it holds.

Everything that the database holds and the copy does not is lost, such as a
wallet you created after you made the copy. So restoredatabase asks first,
and says what the database and the copy hold. It asks nothing when the
database holds no wallet. --yes answers yes in advance, for scripts.

Before it replaces the database, hw copies it and its labels into
~/.hw/auto-backups, with the copies it makes after each change. The answer
names the two files. So if you restore the wrong copy, run restoredatabase
with that backup to undo it. hw makes no such backup of a database without a
wallet, or when the key autobackup-database of config.toml is false. hw
makes no copy after the restore: the file you restored from holds what the
database holds now.

A damaged database, a file that SQLite cannot read, is replaced too. hw
cannot read what it holds, so it asks nothing and copies nothing. It moves
the file into ~/.hw/broken-databases first, under a name with the time, and
the answer says where it is. hw never removes a file there.

hw restores only a copy that has the format version of this hw, which
hw --version shows. It refuses a copy of an earlier hw, and one of a newer
hw. hw does not change the copy.

The copy holds the history as its last sync left it. Run sync afterwards.
The settings in ~/.hw/config.toml are not part of the database, and stay as
they are.

hw can only restore into a database it can open. When hw refuses the
database in ~/.hw, move ~/.hw/portfolios.sqlite aside first, with the files
portfolios.sqlite-wal and portfolios.sqlite-shm if they are there.
|],
      args =
        CommandRestoreDatabase
          <$> fileArgument "The copy of the database to restore. hw does not change it."
          <*> yesOption "Replace the database without asking first"
    }

{- exportsheet -}

-- | exportsheet, which writes every listing of the wallet to a spreadsheet
exportSheetCommand :: CommandSpec Command
exportSheetCommand =
  CommandSpec
    { name = "exportsheet",
      offline = False,
      summary = "Write every listing of the wallet to a spreadsheet",
      details =
        [r|
exportsheet writes the whole wallet to a new spreadsheet. Each listing has a
tab: the portfolios, the addresses, the balances, the coins, the transactions
and the pending transactions. A last tab holds the address book. The file is
in the OpenDocument format of LibreOffice (.ods), which Excel and Google
Sheets open too. Each tab holds every row of its listing, in the same columns
and the same order as its --csv file. So rows copied from one fit the other.

Two more tabs hold the addresses of the transactions and of the pending
transactions. In the tab of the transactions, the row of a transaction names
three addresses: the one it spends the most from, the one it pays the most,
and its change. These two tabs have a row for every address of every
transaction. A row gives the role of the address (from, to or change), its
value and the address. When the wallet holds the address, the row also gives
its path, its chain and its label, and the wallet and the portfolio that hold
it. Each row repeats the txid, the time, the direction and the status of its
transaction. Only the spreadsheet has these rows: no --csv option writes
them.

The tabs hold every portfolio of every wallet. Each row starts with the
columns that say where it belongs: its wallet, its portfolio, its network and
its type. The tab of the portfolios says which one is active. The receiving
addresses and the change addresses share a tab, the receiving ones first.
Each time has three columns: the time in UTC, the local time, and the offset
of that local time from UTC.

Each cell has a type. A label is text, whatever it starts with. An amount is
a number of satoshis. A time is a date and time. The header of each tab stays
at the top as its rows scroll.

With --tag or --untagged, each tab holds only the rows that its listing shows
with the same option. With --tag business, the tabs of the addresses, of the
balances and of the coins hold only the addresses that have the tag. The tabs
of the transactions hold only the transactions that have it, and the address
book only the contacts that have it:

  hw exportsheet business.ods --tag business

With --untagged, each tab holds only the rows that have no tag. The tabs of
the transactions then hold the transactions left to sort, as txs --untagged
lists them. With both options, each tab holds both groups. The tab of the
portfolios always holds every portfolio. The spreadsheet does not say that it
leaves rows out, so name the file after what it holds.

hw reads all the listings at one moment, so the tabs agree with each other.
The file must not exist yet. hw makes it readable by you alone, since it
tells your whole history.
|],
      args =
        CommandExportSheet
          <$> fileArgument "The spreadsheet to write, such as wallet.ods. It must not exist yet."
          <*> sheetTagFilterOptions
    }

{- exportjournal -}

-- | exportjournal, which writes the transactions of the wallet to a journal
-- for ledger
exportJournalCommand :: CommandSpec Command
exportJournalCommand =
  CommandSpec
    { name = "exportjournal",
      offline = False,
      summary = "Write the transactions of the wallet to a journal for ledger",
      details =
        [r|
exportjournal writes the transactions of every portfolio of every wallet to a
new journal for ledger, the command-line accounting program. A journal is a
plain text file that lists each transaction as an accounting entry, by the
rules of double-entry accounting. Include the file in a journal of your own,
and keep your prices and your reports there.

Each transaction is one entry. A portfolio is an account, such as
Assets:hw:wallet0:portfolio0. The network is the commodity of the amounts,
such as BTC or BCH. hw posts what comes from others to Income:Unknown, and
what goes to others to Expenses:Unknown. It posts the fee of a transaction
the wallet made to Expenses:Fees:Mining. A payment between two portfolios is
one entry that moves the money from one account to the other.

A tag can stand for an account of your own. In config.toml, a line such as
rent = "Expenses:Housing:Rent" in the table journal-accounts posts the
payments that have the tag rent to that account. The README explains it.

The name of an entry is the label of its transaction. When the transaction
has none, the name is the label of the contact that the payment pays, or else
the label of the address that received it. The code of an entry is the short
id of the transaction. Its notes give the txid, the time in UTC and the
block, and its tags are those of the transaction. Below the posting of each
payment are the address paid and the label of its contact.

A confirmed transaction is cleared (*), and an unconfirmed one is pending (!).
The journal leaves out the dead transactions, and the transactions that are
prepared but not on the chain yet. An entry has the date of its transaction
on the clock of this computer.

The last entry states the balance of each portfolio. ledger refuses the file
when its entries do not add up to these balances, for instance after someone
edited the file by hand.

With --tag, --address-tag or --untagged, the journal holds only some of the
transactions: those that txs lists with the same options. Use them for the
payments of a business, for instance, to hand to the accountant of that
business:

  hw exportjournal business.ledger --tag business

The journal then holds these transactions from every portfolio, and its first
comment says which ones. A payment between two portfolios keeps its whole
entry when the options select either side of it. Such a journal states no
balance at its end, since the transactions it leaves out moved money too. The
journal that hw keeps up to date always holds every transaction.

The file holds no price and no cost. The file must not exist yet. hw writes
the whole journal each time, so delete the old file first. hw makes the file
readable by you alone, since it tells your whole history.

To have hw keep a journal up to date instead, run hw setdefault --journal. hw
then writes it to ~/.hw/portfolios.ledger after each command that changes it.
|],
      args =
        CommandExportJournal
          <$> fileArgument "The journal to write, such as hw.ledger. It must not exist yet."
          <*> txTagFilterOptionsOf "Write only" "the journal" ""
    }

{- report -}

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

{- fees -}

-- | fees, which shows the fee rates of recent blocks
feesCommand :: CommandSpec Command
feesCommand =
  CommandSpec
    { name = "fees",
      offline = False,
      summary = "Show the fee rates of recent blocks",
      details =
        [r|
fees shows three fee rates, which it reads from the latest six blocks that
hold fee-paying transactions:

  low      a quarter of the virtual bytes in those blocks paid less
  median   half of the virtual bytes paid less
  high     nine tenths of the virtual bytes paid less

The rates are in satoshis per virtual byte, the unit in which the network
charges fees. In that unit, a byte of a signature in a witness counts as a
quarter of a byte.

A transaction that pays the high rate almost surely gets into the next block.
One that pays the low rate usually does when the network is calm. The age of
the latest block tells how fresh the rates are. When the latest block was
mined an hour ago, the network is slow, and the rates may be too low.

The rates show what got into blocks, not how long the transactions waited.
When many transactions arrive suddenly after a quiet period, the rates are
too low for a while. If a payment is then stuck, bumpfee replaces it with one
that pays a higher fee.

preparetx, preparesendall, bumpfee and preparesweep show the same rates, and
ask which one to take when you give no --fee-rate. --fee-rate low, median or
high takes one of the rates without asking.
|],
      args = CommandFees <$> networkMaybeOption
    }

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
-- other commands ('networkNamedBy'), so its help does not say that it is
-- one of the networks hw shows.
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
                  <*> pure []
                  <*> underCoinsSwept (dustCoinsOption "at their addresses")
                  <*> pure Nothing
                  <*> txLabelOptionSaying "The label stays on a sweep that pays this wallet, once the network has it."
                  <*> underFee feeOption
                  <*> underFee (yesOption "Sign the sweep and send it without asking first, and take a fee rate above 100 satoshis per virtual byte without asking")
              )
    }

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
way.

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
replace the --input file. It asks this before it asks for the keys. No option
answers that question, so a script must give --output. hw shows a sweep of a
file with the labels the file carries, and writes them to the signed file.

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

{- Argument helpers -}

-- | A file name as an argument, with the help given. The shell completes it
-- with file names.
fileArgument :: String -> Args FilePath
fileArgument desc =
  strArgument $
    metavar "FILENAME"
      <> fileCompletion
      <> help desc

-- | The shell completes a file name with the files and directories
fileCompletion :: (HasCompleter f) => Mod f a
fileCompletion = completer (mkCompleter completeFile)
