{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of hw: its commands, each with its options and its
-- help, and the options that go with any command. A command is a
-- 'CommandSpec'. Each of its options is made once, as the row of the help
-- that describes it and the parser that reads it, with the builders of
-- "Haskoin.Wallet.Cli.Parser.Args". "Haskoin.Wallet.Cli.Parser.Run" runs the
-- command line, and "Haskoin.Wallet.Cli.Parser.Help" lays out the help.
module Haskoin.Wallet.Cli.Parser where

import Control.Applicative (Alternative (..), optional)
import Data.List (find, intercalate, stripPrefix)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Data.Word (Word32)
import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit, readUnit)
import Haskoin.Wallet.Tx.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Cli.Completion
  ( activeAddresses,
    addressesToSpendFrom,
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
    shownNetworkNames,
    shownTargets,
    walletAddresses,
    walletIndexes,
  )
import Haskoin.Wallet.Database.Address (AddrType, AddressNamed, readAddressNamed, readTypedChainPath)
import Haskoin.Wallet.Tx.FeeRates (FeeRateChoice, readFeeRateChoice)
import Haskoin.Wallet.Keys (AccountKind (..), KeySource (..), indexFits, kindNamed, kindNames, portfolioNetworks)
import Haskoin.Wallet.Labels (ImportScope (..))
import Haskoin.Wallet.Migration (latestDatabaseVersion)
import Haskoin.Wallet.ShortId (CoinRef (..), HashRef (..), readHashRef)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Completion (completeFile)
import Haskoin.Wallet.Cli.Parser.Run (runProgram)
import Haskoin.Wallet.Cli.Listing (Paging (..))
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.Util (readNatural)
import Numeric.Natural (Natural)
import qualified Paths_haskoin_wallet as Paths
import Text.RawString.QQ (r)

{- Commands -}

-- | A command of hw, with its options. Options that could be taken for one
-- another, being many or of the same type, come in a record of their own,
-- read by name; the others come as arguments of distinct types.
data Command
  = CommandNewPortfolio !NewPortfolio
  | CommandRecover !Recover
  | -- | Check a mnemonic against the active portfolio
    CommandCheckMnemonic
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
    -- it, and the portfolio, which the argument or --portfolio names
    CommandSwitch !(Maybe Natural) !Natural
  | CommandSetDefault !SetDefault
  | CommandShowDefault !ShowDefault
  | -- | The portfolio to delete, in the wallet of the active portfolio,
    -- which the user must name
    CommandDeletePortfolio !(Maybe Natural)
  | -- | The wallet to delete, which the user must name, and its
    -- fingerprint, typed to confirm the deletion
    CommandDeleteWallet !(Maybe Natural) !Text
  | CommandReceive !Receive
  | -- | Where to look, what of a file of a wallet to show, when --wallet
    -- and --portfolio name one wallet and one portfolio of it, and what
    -- to show, or Nothing for the newest receiving address of the account
    CommandShow !Target !ImportScope !(Maybe Shown)
  | -- | Where to look, whether the change addresses, how to show each
    -- address or the CSV file to write them to, and the page
    CommandAddresses !Target !Bool !ListingOutput !(Paging Text)
  | -- | Where to look, how to show each address or the CSV file to write
    -- them to, and the page
    CommandBalances !Target !ListingOutput !(Paging Text)
  | CommandLabel !Label
  | -- | The one network to list, or every one, how to show each
    -- transaction or the CSV file to write them to, and the page
    CommandTxs !(Maybe Network) !ListingOutput !(Paging HashRef)
  | CommandPrepareTx !PrepareTx
  | -- | The payment to prepare, sign and send in one command, with the
    -- options of preparetx. It writes no file, so the options name no
    -- --output file.
    CommandSend !PrepareTx
  | CommandPrepareSendAll !PrepareSendAll
  | -- | The one network to list, or every one, how to show each
    -- transaction or the CSV file to write them to, and the page
    CommandPendingTxs !(Maybe Network) !ListingOutput !(Paging HashRef)
  | -- | The transaction file
    CommandImportTx !FilePath
  | -- | The pending transaction, by either of its hashes, in full or by
    -- its short id, and the file to write it to
    CommandExportTx !HashRef !FilePath
  | -- | The pending transaction, by either of its hashes, in full or by
    -- its short id
    CommandDeleteTx !HashRef
  | -- | What to sign
    CommandSignTx !Signing
  | CommandFees !(Maybe Network)
  | CommandSignMsg !SignMsg
  | CommandVerifyMsg !VerifyMsg
  | -- | Where to look, whether the locked coins only, how to show each
    -- coin or the CSV file to write them to, and the page
    CommandCoins !Target !Bool !ListingOutput !(Paging CoinRef)
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
    CommandSync !(Maybe Network) !Bool
  | -- | The file of the copy, or none for the user-backups directory of hw
    CommandBackup !(Maybe FilePath)
  | -- | The spreadsheet to write the listings of every portfolio to
    CommandExportSheet !FilePath
  | CommandPrepareSweep !PrepareSweep
  | -- | What to sign, the file of the private keys, typed on the terminal
    -- without one, and the network of a file that names none and involves
    -- no portfolio
    CommandSignSweep !Signing !(Maybe KeySource) !(Maybe Network)
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
listingOutputOf (CommandAddresses _ _ output _) = Just output
listingOutputOf (CommandBalances _ output _) = Just output
listingOutputOf (CommandTxs _ output _) = Just output
listingOutputOf (CommandPendingTxs _ output _) = Just output
listingOutputOf (CommandCoins _ _ output _) = Just output
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
-- command shows a network and makes it the default.
networkNamedBy :: Command -> Maybe Network
networkNamedBy = \case
  CommandReceive opts -> opts.target.network
  CommandShow target _ _ -> target.network
  CommandAddresses target _ _ _ -> target.network
  CommandBalances target _ _ -> target.network
  CommandLabel opts -> opts.target.network
  CommandTxs netM _ _ -> netM
  CommandPrepareTx opts -> opts.network
  CommandSend opts -> opts.network
  CommandPrepareSendAll opts -> opts.network
  CommandPendingTxs netM _ _ -> netM
  CommandFees netM -> netM
  CommandSignMsg opts -> opts.network
  CommandVerifyMsg opts -> opts.network
  CommandCoins target _ _ _ -> target.network
  CommandLockCoin _ netM -> netM
  CommandUnlockCoin _ netM -> netM
  CommandBroadcast _ _ _ netM -> netM
  CommandSync netM _ -> netM
  CommandPrepareSweep opts -> opts.network
  CommandSignSweep _ _ netM -> netM
  -- Checked by setdefault itself
  CommandSetDefault _ -> Nothing
  -- Its --network asks for the default network, and names none
  CommandShowDefault _ -> Nothing
  -- The commands without a --network
  CommandNewPortfolio _ -> Nothing
  CommandRecover _ -> Nothing
  CommandCheckMnemonic -> Nothing
  CommandImportWallet {} -> Nothing
  CommandExportWallet {} -> Nothing
  CommandPortfolios _ -> Nothing
  CommandSwitch _ _ -> Nothing
  CommandDeletePortfolio _ -> Nothing
  CommandDeleteWallet _ _ -> Nothing
  CommandImportTx _ -> Nothing
  CommandExportTx _ _ -> Nothing
  CommandDeleteTx _ -> Nothing
  CommandSignTx _ -> Nothing
  CommandBumpFee _ -> Nothing
  CommandBackup _ -> Nothing
  CommandExportSheet _ -> Nothing

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
-- transaction they signed and the file it was written to, if any. send
-- answers with the id of the transaction it sent.
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
    oneLine :: !Bool
  }
  deriving (Eq, Show)

-- | The options of recover
newtype Recover = Recover
  { -- | The portfolios in a row without a payment that end the search
    portfolioGap :: Natural
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
    -- | The hidden networks to show from now on, one for each
    -- --show-network
    showNetworks :: ![Network],
    -- | The networks to hide from now on, one for each --hide-network
    hideNetworks :: ![Network]
  }
  deriving (Eq, Show)

-- | The options of showdefault: what to show, each by a switch. The
-- switches of the defaults are named after the options of setdefault
-- that set them, so that a user types the same word to set a default and
-- to see it: --network, --type and --unit. The view of the listings,
-- which setdefault sets with --compact, --verbose or --tabular, is asked
-- with --view, and the networks hw shows with --shown-networks, the name
-- of their key. No switch at all asks for everything ('showsEverything').
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
    askedShownNetworks :: !Bool,
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
  not (or [opts.askedNetwork, opts.askedType, opts.askedUnit, opts.askedView, opts.askedShownNetworks, opts.askedPortfolio, opts.askedWallet])

-- | The options of receive
data Receive = Receive
  { target :: !Target,
    label :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | The options of label. One command sets every label and description
-- the user gives, since they do one job: say what a thing is for, in the
-- user's words. The words typed tell which thing. A word before the label
-- names an address, a path or a transaction. Without one, the label is the
-- description of the active portfolio, or of its wallet.
data Label = Label
  { -- | What to label, as typed before the label ('readLabelled'). Nothing
    -- when the label comes alone: it is then the description of the
    -- active portfolio, or of its wallet with --of-wallet.
    labelled :: !(Maybe String),
    -- | The new label or description. An empty one removes it.
    label :: !Text,
    -- | Whether a description is that of the wallet of the active
    -- portfolio (--of-wallet), not that of the portfolio
    ofWallet :: !Bool,
    -- | The network of an address or a path the wallet has on several
    -- networks, and the type of a path typed without its type
    target :: !Target
  }
  deriving (Eq, Show)

-- | What label labels, as the word before the label names it
data Labelled
  = -- | A transaction, by its txid, or while it is pending by either of
    -- its hashes, in full or by its short id
    LabelledTx !HashRef
  | -- | A receiving address, as typed
    LabelledAddress !Text
  | -- | The receiving address at a path of the active portfolio, /0/i,
    -- perhaps after the type of its account, as in bip84/0/3. A change
    -- path, /1/i, is read too, so that label can say why it refuses it.
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

-- | What a word typed alone for label names, when it has the shape of
-- something label labels: a path, a hash, or an address of one of the
-- networks. Nothing for any other word. label takes a word alone for a
-- description, so a word of such a shape is most likely a label left out,
-- as in hw label bip84/0/3. label refuses it rather than replace the
-- description the portfolio had.
labelledShapeOf :: String -> Maybe String
labelledShapeOf word = case readLabelled word of
  Right (LabelledPath {}) -> Just "a path"
  Right (LabelledTx _) -> Just "the id of a transaction"
  Right (LabelledAddress typed) | any (\net -> isJust (textToAddr net typed)) allNets -> Just "an address"
  _ -> Nothing

-- | What show shows, as the word typed names it. One command takes the
-- five, so that the user need not know which command shows what. The word
-- tells. The shapes of the five do not overlap, so a word is never taken
-- for one kind when it was typed as another ('readShown').
data Shown
  = -- | The address at a path of a chain of an account, /0/i or /1/i,
    -- perhaps after the type of the account, as in bip84/0/i
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
-- * A path is made of digits and slashes, such as /0/3, perhaps after the
--   type of its account, as the views write it: bip84/0/3. A word of
--   those, perhaps with the m, h and ' of a BIP32 path, is a path or is
--   refused as one ('readTypedChainPath'). So 0/3 and m/0/3 are told how
--   to write a path, and so is bip49/0/3, a type hw holds no account of.
-- * A coin has a colon after 8 or 64 hexadecimal digits.
-- * A hash has 8 or 64 hexadecimal digits and nothing else, as for label.
-- * An address reads as an address on one of the networks. Its checksum
--   tells it from a file name.
-- * Every other word names a file, such as unsigned.psbt or
--   ./unsigned.psbt. A file whose name has one of the other shapes goes
--   with its directory, as ./a1075db5.
--
-- A word that is none of the five, such as a short id of 7 digits, is
-- taken for a file. show then names the five kinds when no file has that
-- name.
readShown :: String -> Either String Shown
readShown word
  | Just path <- readTypedChainPath (cs word) = (\(kindM, addrType, index) -> ShownPath kindM addrType index) <$> path
  | Just coin <- readCoinRef word = Right (ShownCoin coin)
  | Just hash <- readHashRef (cs word) = Right (ShownTx hash)
  | any (\net -> isJust (textToAddr net (cs word))) allNets = Right (ShownAddress (cs word))
  | otherwise = Right (ShownFile word)

-- | Where --change-back of preparetx sends the change
data ChangeBack
  = -- | To the address of the largest coin the payment spends, of the
    -- type of the change (--change-back largest). The user cannot know
    -- in advance which coins the strategy selects, so this form names an
    -- address the payment spends from without knowing it.
    BackToLargestCoin
  | -- | To the address of the portfolio named, by itself or by its path
    BackToNamed !AddressNamed
  deriving (Eq, Show)

-- | The word --change-back reads: largest, or else an address or a path.
-- largest cannot be taken for an address, which is far longer than seven
-- letters, nor for a path, which has slashes. It is a keyword like the
-- low of --fee-rate, and not a bare
-- --change-back: optparse-applicative takes the word after an option as
-- its value, so a bare --change-back would swallow the next option or
-- recipient.
readChangeBack :: String -> Either String ChangeBack
readChangeBack word
  | word == "largest" = Right BackToLargestCoin
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
    -- | A chooser of coin control: the coins chosen by their outpoints, in
    -- full or by their short ids (--coin). They add up with those at the
    -- addresses of --spend-from.
    coins :: ![CoinRef],
    -- | A chooser of coin control: the addresses whose coins are chosen
    -- (--spend-from), by themselves or by their paths. They add up with
    -- the coins of --coin.
    spendFrom :: ![AddressNamed],
    -- | Whether the strategy selects among the chosen coins (--select),
    -- instead of every one of them being spent
    select :: !Bool,
    -- | Whether a fee rate above 100 satoshis per virtual byte is taken, and a
    -- wallet behind the chain pays, without asking (--yes)
    yes :: !Bool,
    -- | Where the change goes instead of a new change address
    -- (--change-back): an address of the portfolio, by itself or by its
    -- path, or that of the largest coin spent
    changeBack :: !(Maybe ChangeBack),
    -- | The type of the change (--change-type), when the command says:
    -- that of the new change address, or of the largest coin with
    -- --change-back largest. Otherwise the coins spent say.
    changeType :: !(Maybe AccountKind)
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

-- | The options of preparesendall
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
    output :: !(Maybe FilePath),
    -- | The label of the transaction (--label)
    label :: !(Maybe Text),
    -- | A chooser of coin control: the coins to send, instead of all of
    -- them, in full or by their short ids (--coin). They add up with those
    -- at the addresses of --spend-from.
    coins :: ![CoinRef],
    -- | A chooser of coin control: the addresses whose coins to send,
    -- instead of all of them, by themselves or by their paths
    -- (--spend-from). They add up with the coins of --coin.
    spendFrom :: ![AddressNamed],
    -- | What to do with the coins worth less than their fee, or Nothing to
    -- ask the user
    dustCoins :: !(Maybe DustCoins),
    -- | Whether a fee rate above 100 satoshis per virtual byte is taken without
    -- asking (--yes)
    yes :: !Bool
  }
  deriving (Eq, Show)

-- | What preparesendall does with the coins worth less than the fee to
-- spend them
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

-- | The options of preparesweep
data PrepareSweep = PrepareSweep
  { -- | The network of a sweep that pays no portfolio of the wallet
    network :: !(Maybe Network),
    -- | The private keys of the addresses swept, if at hand
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
    output :: !(Maybe FilePath),
    -- | The label of the sweep (--label)
    label :: !(Maybe Text),
    -- | The fee rate, as for preparetx
    feeRate :: !(Maybe FeeRateChoice),
    -- | Whether a fee rate above 100 satoshis per virtual byte is taken without
    -- asking (--yes)
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
-- no file to read or to write: what signtx NOSIGHASH signs. send has its
-- own transaction signed this way.
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
    -- | Print a listing as it is, through no pager
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
      everydayNames = ["newportfolio", "recover", "portfolios", "sync", "receive", "show", "addresses", "balances", "label", "coins", "fees", "send", "backup"],
      details =
        [r|
A payment takes three steps:

  hw preparetx ADDRESS:AMOUNT    prepare it, without any key
  hw signtx NOSIGHASH            sign it with the mnemonic
  hw broadcast TXID              send it to the network

preparetx shows the nosigHash of the new transaction. signtx shows its txid,
the id the network knows it by.

`send` takes the three steps in one command, on an online computer where you
type the mnemonic:

  hw send ADDRESS:AMOUNT         prepare it, sign it and send it

A command that takes a hash, or a coin TXID:INDEX, also takes its short id:
the first 8 digits of the hash, as the listings show it, such as a1075db5, or
a1075db5:1 for a coin. When two transactions of the wallet share those 8
digits, hw names both and asks for the full hash.

hw keeps a wallet for each mnemonic, and portfolios in each wallet. A portfolio
holds every account of the wallet at one account index: portfolio0 holds the
accounts at index 0, on every network and of both types of address. You never
pick an account. You only say the network and the type of address you want.

The commands work in the active portfolio. The first portfolio you create
becomes active, so with one mnemonic you work in portfolio0 of wallet0 and
never name it. `portfolios` marks the active one with a *, and switch makes
another one active, until the next switch:

  hw switch 1               portfolio1 of the same wallet
  hw switch 0 --wallet 1    portfolio0 of wallet1

Two options say where a command works in the portfolio. Each has a default,
which a key of the configuration file changes, and which setdefault sets:

  --network NET     btc or bch                    btc          default-network
  --type TYPE       bip44 or bip84                bip44        default-type

A portfolio has accounts on eight networks. hw shows btc and bch, and hides
the six test networks, unless you change it with setdefault --show-network
and --hide-network. A hidden network is left out of every answer, and
--network cannot name it, while `sync` and `recover` still work on it.

A bip44 account has legacy addresses, which start with 1 on Bitcoin. A bip84
account has native SegWit addresses, which start with bc1q. Bitcoin Cash has
bip44 accounts only: it takes bip44 whatever the default says, and refuses
--type bip84.

A payment spends the coins of a portfolio on one network, of both types
together. A transaction is shown as the portfolio sees it: Incoming, Outgoing,
or Internal when it moves funds between addresses of the portfolio.

To sign on an offline computer, create the portfolio there with newportfolio.
Copy it to the online computer with exportwallet and importwallet. Then carry
each transaction between them with preparetx --output, signtx --input and
broadcast --input.

`recover` and preparesweep --key-file take a mnemonic or private keys too, but
they need the server, so they run on an online computer. Their help says how to
keep the secret offline.

On a terminal, a listing such as `txs` goes through less: scroll it, search
it with /, and quit it with q. One that fits on the screen is shown as it
is. A line wider than the screen is cut at its edge, and the right arrow key
scrolls to the rest of it. --csv FILENAME writes a listing to a new file for a
spreadsheet instead, every row whole.

The options --unit, --json and --no-pager go before or after the command.
The settings, such as the server, the default network, the unit, the view of
the listings, the address gap, the pager and the backups hw makes by itself,
are in ~/.hw/config.json. setdefault writes the defaults among them, and
showdefault shows them with the active portfolio.

The Tab key completes the commands, the options, and what the wallet holds:
wallets and portfolios, the hashes of pending transactions, coins and addresses. The
shell needs a script for it, which hw writes. In bash, add this line to
~/.bashrc:

  source <(hw --bash-completion-script hw)

The README explains it for zsh and fish.

Type hw COMMAND --help for the help of a command.
|],
      globals = GlobalOptions <$> unitOption <*> jsonOption <*> noPagerOption,
      groups =
        [ CommandGroup
            "Wallets and portfolios"
            [ newPortfolioCommand,
              recoverCommand,
              checkMnemonicCommand,
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
            "Sweeps"
            [ prepareSweepCommand,
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
              exportSheetCommand
            ]
        ]
    }

jsonOption :: Args Bool
jsonOption =
  switch $
    short 'j'
      <> long "json"
      <> help "Print the answer as JSON, for scripts. Questions and warnings go to stderr."

noPagerOption :: Args Bool
noPagerOption =
  switch $
    long "no-pager"
      <> help
        [r|
Print a listing as it is. On a terminal, it goes through less otherwise, or
through the pager that HW_PAGER, the key pager of config.json or PAGER names.
|]

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
bit or sat. A bit is 100 satoshis. The default is the key unit of
config.json, or btc. setdefault --unit sets that key. After showdefault,
--unit takes no value, and shows that key.
|]

{- newportfolio -}

newPortfolioCommand :: CommandSpec Command
newPortfolioCommand =
  CommandSpec
    { name = "newportfolio",
      offline = True,
      summary = "Make a mnemonic and its wallet, or add a portfolio to a wallet",
      details =
        [r|
Without --existing, newportfolio makes a new mnemonic. A new mnemonic is a new
wallet, so it creates that wallet with its portfolio0.

newportfolio first asks for a passphrase, twice to catch a typo. Leave it
empty for none. A mnemonic with a passphrase gives another master key, so it
is another wallet: the passphrase is part of the secret, and must be kept too.

It then prints the mnemonic, once, with the portfolio it created. Write the
mnemonic down and keep copies in safe places, away from computers and phones.
Whoever has the mnemonic can spend the funds. If you lose it, the funds are
lost. hw stores the public keys of the portfolio only, never the mnemonic.
checkmnemonic checks what you wrote down against the portfolio.

The words come numbered, four on a line, to write them down. With
--one-line, hw prints them on one line, without numbers, to copy them into
another program. A share of a backup gets a line of its own. A copied
mnemonic passes through the clipboard, where other programs can read it.

The entropy comes from the operating system, read from /dev/random where the
machine has it. With --dice, hw also mixes in rolls of a die, so that you do
not have to trust the system alone. Run this command on an offline computer
if you can.

With --shares, hw prints the shares of a backup instead of a mnemonic, in the
SLIP-39 format. Any --threshold of the shares rebuild the seed, and fewer
tell nothing of it: with --shares 3 --threshold 2, any two of three. A share
is a list of 20 words, or of 33 with --entropy 32. The commands that ask for
the mnemonic take the shares in its place: type one, and hw asks for the
next until it has enough. A single share, with --shares 1, is the whole
backup, as a mnemonic is. A backup in shares is not a BIP39 mnemonic, and no
mnemonic comes of it. A wallet that reads SLIP-39 shares restores it, and one
that reads only BIP39 mnemonics cannot. Its passphrase has printable ASCII
characters only, as its format asks.

With --existing, newportfolio asks for a mnemonic you have already, and its
passphrase, each twice. It takes the shares of a backup in place of the
mnemonic. It then adds a portfolio to the wallet of that mnemonic, at the
lowest free index: portfolio1 when the wallet has portfolio0, and portfolio1
too when it has portfolio0 and portfolio2. When the database has no wallet of
the mnemonic yet, it creates the wallet with its portfolio0.

The first portfolio of a database becomes the active one, where the commands
work. A portfolio added later does not: newportfolio says which portfolio is
still active, and switch makes the new one active.

A portfolio is created whole: an account for every network, the test networks
included, and for both types of address. Those are five keys:

  m/44'/0'/i'     bip44   btc
  m/84'/0'/i'     bip84   btc
  m/44'/145'/i'   bip44   bch
  m/44'/1'/i'     bip44   every test network
  m/84'/1'/i'     bip84   the test networks of Bitcoin

The test networks share their keys, and hw keeps their addresses and coins
apart. You never create an account: `receive` --network and --type pick one.

The private keys are never stored. To keep them on an offline computer, run
newportfolio there. Then copy the wallet to an online computer with
exportwallet and importwallet.

A passphrase with accents or other non-ASCII letters is normalized as BIP39
asks, so that other wallets find the same seed. hw 0.9 did not normalize it,
and `recover` finds the funds of such a seed. If the database has a wallet of
the passphrase as typed, and none of the normalized one, --existing keeps to
that wallet and says so. In a database with neither, --existing makes the
wallet of the normalized passphrase. It then warns that a wallet hw 0.9 made
from this passphrase has other keys, which `recover` finds.
|],
      args =
        CommandNewPortfolio
          <$> (NewPortfolio <$> existingOption <*> entropyOption <*> diceOption <*> sharesOption <*> thresholdOption <*> oneLineOption)
    }

-- | Whether newportfolio asks for a mnemonic the user has already
existingOption :: Args Bool
existingOption =
  switch $
    long "existing"
      <> help "Ask for a mnemonic you have already, and add a portfolio to its wallet, instead of making a new mnemonic"

entropyOption :: Args (Maybe Natural)
entropyOption =
  optional . option (maybeReader f) $
    short 'e'
      <> long "entropy"
      <> metavar "BYTES"
      <> completeWith valid
      <> help
        [r|
The bytes of entropy: 16, 20, 24, 28 or 32, for a mnemonic of 12, 15, 18, 21
or 24 words. A backup in shares takes 16 or 32, for shares of 20 or 33 words.
The default is 16.
|]
  where
    valid = ["16", "20", "24", "28", "32"]
    f s
      | s `elem` valid = fromIntegral <$> readNatural (cs s)
      | otherwise = Nothing

diceOption :: Args Bool
diceOption =
  switch $
    short 'd'
      <> long "dice"
      <> help
        [r|
Mix rolls of a six-sided die into the entropy of the system. hw asks for the
rolls. Type the faces, as many on a line as you like. They are not shown, and
hw tells how many rolls are left after each line. With --shares, hw asks for
enough rolls for the seed and for each random value that splits it, and uses
no roll twice.
|]

-- | The shares of a backup that newportfolio makes in place of a mnemonic
sharesOption :: Args (Maybe Natural)
sharesOption =
  optional . option (eitherReader $ upToSixteen "--shares" . cs) $
    short 's'
      <> long "shares"
      <> metavar "INT"
      <> help
        [r|
Make a backup in INT shares, from 1 to 16, in place of a mnemonic. With more
than one share, --threshold gives the number of them that rebuild the seed.
|]

-- | The number of shares that rebuild the seed of a backup
thresholdOption :: Args (Maybe Natural)
thresholdOption =
  optional . option (eitherReader $ upToSixteen "--threshold" . cs) $
    long "threshold"
      <> metavar "INT"
      <> help
        [r|
The number of shares that rebuild the seed, from 2 to the number of shares.
Fewer of them tell nothing of it.
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
  networkOptionWith "For a file that names no network, as a file of another wallet does. Without it, the default-network of the configuration file, or btc."

-- | The network of an address or a coin the user names, when the wallet
-- has it on several networks
heldNetworkOption :: String -> Args (Maybe Network)
heldNetworkOption what =
  networkOptionWith ("Needed only when the wallet has the " <> what <> " on several networks: the test networks share their keys. Without it, the one on the default network.")

-- | The network of an address of the wallet the user names, by itself or
-- by its path, when the wallet has it on several networks: the test
-- networks share their keys, and a portfolio has an address at the same
-- path on each network it gave one out on ('chooseOnNetwork')
namedAddressNetworkOption :: Args (Maybe Network)
namedAddressNetworkOption =
  networkOptionWith "With an address or a path, the network to find it on, when the wallet has it on several. Without it, the only one, or else the one on the default network."

-- | The network, when given, with the help of the command about it. The
-- help names the networks hw shows in a new configuration, and says where
-- the others are: the command line is read before the configuration, so
-- the help cannot name the ones this user shows. The shell offers those
-- ('shownNetworkNames'). A hidden network reads here as any other, and is
-- refused once the configuration is read
-- ('Haskoin.Wallet.Config.hiddenNetworkRefusal').
networkOptionWith :: String -> Args (Maybe Network)
networkOptionWith without =
  optional . option (eitherReader readNetwork) $
    short 'n'
      <> long "network"
      <> metavar "NETWORK"
      <> completer (listIOCompleter shownNetworkNames)
      <> help ("The network, among those hw shows: btc and bch, and those setdefault --show-network added. " <> without)

-- | The network of a name typed, or the names a network has
readNetwork :: String -> Either String Network
readNetwork typed = maybe (Left ("Invalid network name. Select one of the following: " <> everyNetwork)) Right (netByName typed)

{- recover -}

recoverCommand :: CommandSpec Command
recoverCommand =
  CommandSpec
    { name = "recover",
      offline = False,
      summary = "Find the portfolios of a mnemonic that were paid",
      details =
        [r|
`recover` needs the mnemonic and the server at the same time, so it runs on an
online computer and not on an offline one. A computer that was tampered with
can steal a mnemonic typed on it. To keep the mnemonic offline, create the
portfolios on the offline computer with newportfolio --existing instead. Copy
them with exportwallet and importwallet, and run `sync`.

`recover` asks for the mnemonic, or for the shares of a backup, then searches
for the portfolios of its seed that were paid. It checks the portfolios in
order from portfolio0, and asks the server about the receiving addresses of
every account of each: on every network, the test networks included, and of
both types of address. When any account of a portfolio was ever paid, the
whole portfolio is created, with every account, and synced. Portfolios the
wallet has already are skipped.

The wallet is the one of the mnemonic, which hw finds by the fingerprint of
its master key. A new wallet gets its portfolio0 whether that one was paid or
not: a wallet always has portfolio0. A wallet of the same fingerprint whose
keys are not those of the mnemonic is refused: a fingerprint is four bytes,
which two mnemonics may share.

In a database without any portfolio, the first portfolio `recover` creates
becomes the active one, where the commands work: portfolio0 of the new
wallet. In a database that has an active portfolio, it stays active, and
`recover` says how to switch to a portfolio it created.

The server may not serve every network: the public one has no regtest chain.
`recover` names the networks the server does not serve, and searches the
others. When the server of a network is down, `recover` says so, searches the
others, then exits with an error: a portfolio that was paid on that network
alone was not found.

`recover` searches the networks hw hides too, and its status line names them.
Its answer leaves them out: it says nothing of what it found there, nor of a
hidden network whose server is down, which is then no error.

`recover` also looks where other wallets keep their accounts, and tells of
funds it cannot spend. On Bitcoin, those are the bip49 accounts of wrapped
SegWit, m/49', which hardware wallets made before native SegWit, and the
bip86 accounts of Taproot, m/86'. On Bitcoin Cash, those are the bip44
accounts of Bitcoin, m/44'/0', where a wallet that held Bitcoin at the fork
of 2017 has its Bitcoin Cash. No such account is added. Move its funds with
a wallet that holds such accounts, to an address of this wallet.

The search stops after --portfolio-gap portfolios in a row where nothing was
found. BIP44 stops at the first such account. hw looks further, since
portfolios created on an offline computer may never have been paid. On a
terminal, a line tells which portfolio `recover` is looking at, as `sync`
does.

After every 20 portfolios the server reports paid, `recover` asks whether to
keep looking. A server that lies reports them without end. A no stops the
search, and nothing is added.

hw normalizes a passphrase as BIP39 asks, and hw 0.9 did not. When nothing is
found, `recover` also tries the passphrase as typed. It says so if that finds
portfolios.
|],
      args =
        CommandRecover
          <$> (Recover <$> portfolioGapOption)
    }

portfolioGapOption :: Args Natural
portfolioGapOption =
  option (eitherReader portfolioGap) $
    long "portfolio-gap"
      <> metavar "INT"
      <> value 10
      <> showDefault
      <> help
        [r|
How many portfolios in a row where nothing was found end the search, from 1 to
100. A larger gap finds portfolios created on a signer that were never paid
before a later one was.
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
mnemonic you wrote down, or the shares of a backup, before you rely on it.
Nothing is signed, and nothing is changed. To check another portfolio, switch
to it first.

When they do not match, hw says so and exits with an error, so that a script
can tell.
|],
      args = pure CommandCheckMnemonic
    }

{- The options that say where a command works -}

-- | The --wallet option, with the help of the command about it
walletOptionWith :: String -> Args (Maybe Natural)
walletOptionWith = optional . walletIndexOption

-- | The --portfolio option, with the help of the command about it
portfolioOptionWith :: String -> Args (Maybe Natural)
portfolioOptionWith = optional . portfolioIndexOption

-- | The --wallet option of a command that needs it, deletewallet: the
-- usage line shows it without brackets, and the parser refuses the
-- command without it. A command that deletes names its target, and never
-- takes it from the active portfolio.
requiredWalletOption :: String -> Args (Maybe Natural)
requiredWalletOption = fmap Just . walletIndexOption

-- | The --portfolio option of a command that needs it, deleteportfolio,
-- as 'requiredWalletOption' is for --wallet
requiredPortfolioOption :: String -> Args (Maybe Natural)
requiredPortfolioOption = fmap Just . portfolioIndexOption

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

-- | The --type option, with the help of the command about it
typeOptionWith :: String -> Args (Maybe AccountKind)
typeOptionWith what =
  optional . option (eitherReader (readKind . cs)) $
    short 't'
      <> long "type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help ("The type of address: bip44, legacy P2PKH addresses, or bip84, native SegWit P2WPKH addresses, which pay lower fees. " <> what)

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
          <*> portfolioOptionWith "Write only this portfolio, of the wallet --wallet names, or of the wallet of the active portfolio. Without it, every portfolio."
          <*> fileArgument "The file to write. It must not exist yet."
    }

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
            "Read only this portfolio. Without it, every portfolio. In a file of several wallets, name the wallet too."
          <*> fileArgument "The file exportwallet wrote"
    }

importExportWalletDetails :: String
importExportWalletDetails =
  [r|
exportwallet writes every portfolio of every wallet to a file, with its
labels, in the format of BIP 329, which other wallets read and write. One file
holds the whole database, whichever portfolio is active. Each account goes in
as its public key, with its path, its network, its type and the fingerprint of
the mnemonic. Then come the labels of its receiving addresses and of its
transactions, and the coins locked with lockcoin. The descriptions of the
wallets and of their portfolios go in too. The file holds no private key.
hw also writes this file by itself to ~/.hw/auto-backups after each command
that changes the labels, and keeps the ten newest copies, or as many as the
key autobackup-copies of config.json says. backup writes it to
~/.hw/user-backups, beside its copy of the database.

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
database, and its labels still go to it. A portfolio the database does not
have is created whole, with the twelve accounts of the file. The database can
then watch the portfolio and prepare its payments without the mnemonic: this
is how an online computer works with an offline signer. --wallet reads one
wallet of the file only, and --portfolio one portfolio only.

In a database without any portfolio, the first portfolio importwallet creates
becomes the active one, where the commands work. In a database that has an
active portfolio, it stays active, and importwallet says how to switch to a
portfolio it created.

A portfolio the database has already is left as it is. importwallet never
makes a second portfolio with the same index, and adds no account to one: a
portfolio has every account from the start. The keys of the file must be the
ones the portfolio holds. Otherwise importwallet refuses and changes nothing:
the file and the database are not of one mnemonic.

importwallet also refuses, and changes nothing, a file that would create a
portfolio without all twelve of its accounts, a key that another account of
the database holds, whatever index the file writes it at, and two keys for
accounts of one path: the test networks share the path m/44'/1'/0', so their
bip44 accounts hold one key.

importwallet cannot prove that the keys of a file are those of your mnemonic:
only the mnemonic could. A file that names your fingerprint may still carry
the keys of someone else, and the addresses receive would give out would be
theirs. Import only a file you wrote with exportwallet on your own signer, or
one that reached you over a channel you trust.

importwallet imports whole wallets. A key alone, an xpub or an output
descriptor from another wallet, is one account: it does not say which
portfolio it belongs to. A file that holds such a key and no record is
refused. An xpub record of another wallet, in a BIP 329 file, is left aside,
and the answer says why.

--wallet and --portfolio read a part of the file, its keys and its labels.
--wallet names one wallet of the file by its fingerprint. --portfolio names
one portfolio by its index.

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

The records of the rest of the file are counted in the answer and not read. A
wallet always has portfolio0, so a wallet the database does not have is
created only from a file that brings its portfolio0: importwallet --portfolio
1 of such a wallet is refused. Import the whole wallet instead.

The labels and the locks of the file are set on the addresses, transactions
and coins the wallet has. An address the wallet has not generated yet is
derived from the path the file gives, up to a thousand past the last used
address of its chain, and a transaction the origin of its record places in a
portfolio is labelled before `sync` fetches it. A transaction record without
an origin is labelled in the portfolios that have the transaction, on its
network. What hw cannot place is counted in the
answer: run `sync`, then importwallet again. The labels of inputs, of coins
and of change addresses are left aside, since hw keeps none. A BIP 329 file
of another wallet sets its labels the same way.

`show` FILE shows what importwallet would make of a file, and changes
nothing.

The fingerprint is the four bytes that PSBTs and output descriptors write
before a key of the mnemonic. Each record of the file names its account by
it, as in pkh([bc560f95/44'/0'/0']) or wpkh([bc560f95/84'/0'/0']).
|]

{- portfolios -}

portfoliosCommand :: CommandSpec Command
portfoliosCommand =
  CommandSpec
    { name = "portfolios",
      offline = False,
      summary = "Show the portfolios, their balances and descriptors",
      details =
        [r|
`portfolios` lists every portfolio of every wallet. Each starts with its name,
portfolio0 and so on, its wallet, its description and the date it was
created. Then comes a line for each network the portfolio has funds or
addresses on: what it can spend there, what is unconfirmed, and how many coins
and addresses it has. On btc, the figures add up the bip44 and the bip84
account. The portfolios of one wallet come together.

A * before its name marks the active portfolio, the one the commands work in.
switch makes another one active.

The spendable balance is what the portfolio can spend now on that network. It
leaves out the coins locked by a pending transaction or by lockcoin, the
coinbase coins with fewer than 100 confirmations, and the unconfirmed payments
from other wallets. The unconfirmed coins of the wallet's own transactions
count: its change, and what one of its portfolios pays another. preparetx can
spend all of it, unless --min-conf asks for confirmations.

--verbose lists every shown network of the portfolio, with the confirmed and
unconfirmed balances the server gave at the last sync, and the fingerprint of
the mnemonic. Under each network come its accounts, each with its
path and its output descriptor, the format of BIP 380 in which wallets take an
account without its private key:

  pkh([bc560f95/44h/0h/0h]xpub6CHK45s.../<0;1>/*)#vq597j87
  wpkh([bc560f95/84h/0h/0h]xpub6CQF8An.../<0;1>/*)#5nktextl

The first is a bip44 account, the second a bip84 one. Paste it into a wallet
that takes a descriptor, to watch the account there. The descriptor carries
the public key of the account, the fingerprint of the mnemonic and the
derivation path, and the two chains of addresses, with a checksum. It holds no
private key. A hardware wallet matches the account to its mnemonic by the
fingerprint and the path.

A descriptor does not name its network. Bitcoin and Bitcoin Cash write the
same keys, and every test network writes tpub keys.

Each account also shows its key in the form of SLIP-132, in the row SLIP-132:
a zpub for a bip84 account, or a vpub on a test network. Some wallets take only
that form for a native SegWit account, and make a legacy wallet of an xpub.
The key is the same, only its prefix differs. A bip44 account shows its xpub
there, or its tpub on a test network, since SLIP-132 gives the prefix of
BIP32 to bip44 accounts.

A descriptor is wider than most screens. On a terminal, the pager wraps the
lines of --verbose rather than cut them at the edge of the screen, so that a
descriptor can be copied whole.

With --tabular, each network of each portfolio takes one row of a table, under
a header that names the columns: the * of the active portfolio, the wallet,
the portfolio, the network, what it can spend, what is unconfirmed, how many
coins and addresses it has, and the description of the portfolio. The amounts
are in the unit the header names.

With --csv FILENAME, each network of each portfolio takes one row of a new CSV
file, for a spreadsheet: the wallet, the portfolio, the network and the types
of its accounts there, whether it is the active portfolio, its description and
that of its wallet, the paths of its accounts, what it can spend, its confirmed
and unconfirmed balances, its coins and addresses, and when the portfolio was
created. Amounts are in satoshis. Each time comes in UTC, then in local time,
then with the offset of that local time from UTC. The file holds no key and no
descriptor: anyone it reaches could see every address and every payment of the
portfolio with them.
|],
      args =
        CommandPortfolios
          <$> listingOutputOption
            "Show every detail of each portfolio: the fingerprint of its mnemonic, every network with each of its balances, and the path, output descriptor and SLIP-132 key of every account."
            "Show each network of each portfolio on one row of a table: the wallet, the portfolio, the network, what it can spend, what is unconfirmed, its coins and its addresses."
            "portfolios"
    }

{- switch -}

switchCommand :: CommandSpec Command
switchCommand =
  CommandSpec
    { name = "switch",
      offline = False,
      summary = "Make another portfolio the active one",
      details =
        [r|
The commands work in the active portfolio: receive gives out its addresses,
`txs` lists its transactions, and preparetx pays from it. switch makes the
portfolio PORTFOLIO names the active one, in the wallet --wallet names, or in
the wallet of the active portfolio without --wallet. It stays active for every
command until the next switch: the database keeps it. `portfolios` marks it
with a *.

  hw switch 1               portfolio1 of the same wallet
  hw switch portfolio1      the same, by the name `portfolios` shows
  hw switch 0 --wallet 1    portfolio0 of wallet1

--portfolio names the portfolio as an option instead, for a script that names
each value by its option. hw switch --portfolio 1 is hw switch 1. Give the
portfolio once, as PORTFOLIO or with --portfolio.

The first portfolio of a database becomes active as newportfolio, `recover` or
importwallet creates it. A portfolio they add later does not: they say which
portfolio is active, and how to switch to the new one. switch needs no server,
so it works on an offline computer too.

The commands that find their portfolio by themselves need no switch: importtx,
signtx, signsweep, bumpfee, preparesweep, label, lockcoin, unlockcoin, signmsg,
and `show` of a transaction, a transaction file, a coin or an address. When a
transaction involves several portfolios and hw cannot tell which one to take,
it takes the active one, or names them for you to switch to the one to use.
|],
      args =
        switchOf
          <$> (switchPortfolioArg <|> portfolioIndexOption "The portfolio to make active, as an option instead of PORTFOLIO.")
          <*> walletOptionWith "The wallet of the portfolio. Without it, the wallet of the active portfolio."
    }
  where
    -- The portfolio comes first on the command line, and the wallet after
    -- it, since the wallet is often left out
    switchOf portfolio walletM = CommandSwitch walletM portfolio

-- | The portfolio switch makes active, as its argument: an index, such as
-- 1, or the name the listings show, such as portfolio1. It is the one
-- value switch needs, so it goes without an option, and --portfolio is
-- the same value for a script that names each value by its option. The
-- shell offers the portfolios of the wallet, as for --portfolio.
switchPortfolioArg :: Args Natural
switchPortfolioArg =
  argument (eitherReader (indexNamed "portfolio" "The portfolio")) $
    metavar "PORTFOLIO"
      <> completer (listIOCompleter portfolioIndexes)
      <> help "The portfolio to make active, by its index, such as 1, or by its name, such as portfolio1."

{- setdefault -}

setDefaultCommand :: CommandSpec Command
setDefaultCommand =
  CommandSpec
    { name = "setdefault",
      offline = False,
      summary = "Set the defaults of the commands, and the networks shown",
      details =
        [r|
setdefault writes defaults to the configuration file, ~/.hw/config.json, so
that you do not have to edit it. Give each option you want as the default, as
you would give it to a command:

  hw setdefault --type bip84        give out native SegWit addresses
  hw setdefault --network bch       work on Bitcoin Cash
  hw setdefault --unit sat          show and read amounts in satoshis
  hw setdefault --tabular           show the listings as tables

--unit sets the unit of the amounts, shown and typed. hw --help lists it with
the options of every command. --compact, --verbose and --tabular set the view
of the listings: portfolios, addresses, balances, txs, pendingtxs and coins.
One of them given to a listing still changes its view for that listing.

setdefault also sets which networks hw shows. A portfolio has accounts on
eight networks. A new configuration shows btc and bch, and hides the six test
networks:

  hw setdefault --show-network btctest4     show a hidden network
  hw setdefault --hide-network bch          hide a network

A hidden network is left out of every answer, and a command cannot name it.
It is not disabled. sync and recover still work on it, so the wallet is up to
date on it the day you show it. Each option may be given several times. The
default network is always shown: to hide it, name another default with
--network in the same command.

The keys are default-network, default-type, unit, listing-view and
shown-networks. The other keys of the file stay as they are. showdefault shows
them. The portfolio the commands work in is no default of the file. It is the
active portfolio, which switch changes.
|],
      args =
        CommandSetDefault
          <$> ( SetDefault
                  <$> networkOptionWith "It becomes the default network. --show-network in the same command may show it."
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
                  <*> many (shownNetworkOption "show-network" hiddenNetworkNames ("A network to show from now on: " <> everyNetwork <> ". It may be given several times."))
                  <*> many (shownNetworkOption "hide-network" shownNetworkNames "A network to hide from now on. sync and recover still work on it. It may be given several times.")
              )
    }

-- | The names of every network of a portfolio, as the help of
-- --show-network lists them. It is the one place of the help that names
-- them all.
everyNetwork :: String
everyNetwork = intercalate ", " ((.name) <$> portfolioNetworks)

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
-- showdefault, the parser takes --unit for the switch of showdefault,
-- since the options of the command come before those of every command
-- ('Haskoin.Wallet.Cli.Parser.Run.programInfo'). So hw showdefault --unit
-- shows the unit, and hw showdefault --unit sat is refused: showdefault
-- shows no amount for a unit to change.
showDefaultCommand :: CommandSpec Command
showDefaultCommand =
  CommandSpec
    { name = "showdefault",
      offline = False,
      summary = "Show the defaults of the commands, and the active portfolio",
      details =
        [r|
showdefault shows the defaults that setdefault writes, as hw reads them from
the configuration file, ~/.hw/config.json, and the active portfolio. Give the
option of each default you want to see, or none to see them all:

  hw showdefault                  every default, and the active portfolio
  hw showdefault --network        the default network
  hw showdefault --type --unit    the default type and the unit
  hw showdefault --portfolio      the active portfolio, with its wallet
  hw showdefault --wallet         the wallet of the active portfolio

The options are those of setdefault, without a value: --network, --type and
--unit. --view asks for the view of the listings, which setdefault sets with
--compact, --verbose or --tabular. --shown-networks asks for the networks hw
shows. A key that the file does not have shows the value hw takes without it.

The active portfolio is no key of the file. It is in the database, and switch
changes it.
|],
      args =
        CommandShowDefault
          <$> ( ShowDefault
                  <$> switch (long "network" <> help "The default network, of the key default-network.")
                  <*> switch (long "type" <> help "The default type of address, of the key default-type.")
                  <*> switch (long "unit" <> help "The unit of the amounts, of the key unit. After showdefault, --unit takes no value.")
                  <*> switch (long "view" <> help "The view of the listings, of the key listing-view.")
                  <*> switch (long "shown-networks" <> help "The networks hw shows, of the key shown-networks.")
                  <*> switch (long "portfolio" <> help "The active portfolio, with its wallet.")
                  <*> switch (long "wallet" <> help "The wallet of the active portfolio.")
              )
    }

{- deleteportfolio -}

deletePortfolioCommand :: CommandSpec Command
deletePortfolioCommand =
  CommandSpec
    { name = "deleteportfolio",
      offline = False,
      summary = "Delete a portfolio that was never used",
      details =
        [r|
deleteportfolio deletes the portfolio --portfolio names, which must be given,
in the wallet of the active portfolio. It deletes a portfolio only when all of
these are true:

  it is not portfolio0
  it is not the active portfolio
  it has the highest index in its wallet
  it has never had any activity, and gave out no address

Otherwise it refuses, and says which rule stopped it. A wallet always keeps
portfolio0: deleting it would be pointless, since a new portfolio0 would be
the same portfolio, with the same keys. The commands work in the active
portfolio, so switch to another one before you delete it. To delete a
portfolio of another wallet, switch to a portfolio of that wallet first. The
highest index goes first, so that the portfolios of a wallet keep no gap
behind them. A portfolio that was paid is kept: `recover` would only find it
again.

Activity is a coin, a transaction, or a pending transaction. An address
counts too, paid or not. A new portfolio has none: hw generates an address
only when receive gives it out, when a payment takes one for its change, or
when a file or a sync names it. Someone may pay such an address later, and
the portfolio deleted would not see the payment.

To stop using a mnemonic, delete its whole wallet with deletewallet.
|],
      args =
        CommandDeletePortfolio
          <$> requiredPortfolioOption "The portfolio to delete, in the wallet of the active portfolio. It must be given."
    }

{- deletewallet -}

deleteWalletCommand :: CommandSpec Command
deleteWalletCommand =
  CommandSpec
    { name = "deletewallet",
      offline = False,
      summary = "Delete a wallet with all of its portfolios",
      details =
        [r|
deletewallet removes a wallet from the database, with all of its portfolios
and their addresses, labels, transactions, coins and pending transactions. It
is for a mnemonic you no longer use. Unlike deleteportfolio, it deletes a
wallet that has had activity.

--wallet names the wallet and must be given. The FINGERPRINT must be that of
the wallet, as `portfolios` --verbose shows it. It confirms which wallet to
delete.

The funds stay on the chain, and the mnemonic can still spend them. The labels
are lost: exportwallet writes them to a file first.

The other wallets keep their names. When wallet0 is deleted, wallet1 is still
wallet1, and the next new wallet becomes wallet0 again.

The wallet of the active portfolio is deleted only when it is the last wallet:
switch to a portfolio of another wallet first. Once the last wallet is
deleted, the database has no active portfolio, and the first portfolio
created after it becomes active.
|],
      args =
        (\fingerprint wallet -> CommandDeleteWallet wallet fingerprint)
          <$> textArg "FINGERPRINT" "The fingerprint of the wallet, 8 hexadecimal digits, as `portfolios` --verbose shows it"
          <*> requiredWalletOption "The wallet to delete. It must be given."
    }

{- receive -}

receiveCommand :: CommandSpec Command
receiveCommand =
  CommandSpec
    { name = "receive",
      offline = False,
      summary = "Get a new address to receive a payment",
      details =
        [r|
Each call gives the next receiving address of an account of the active
portfolio: the one --network and --type pick, a bip44 address on btc without
them. It is
at the path M/44'/coin'/index'/0/i, or M/84'/coin'/index'/0/i for bip84, where
index is that of the portfolio. The wallet pays the change of its transactions
to other addresses, at .../1/i. `addresses` --change lists them.

You never create an account. A portfolio has one for every network and type
already.

`receive` gives out at most 20 unused addresses in a row, or as many as the gap
of the configuration file says. A wallet restored from the mnemonic stops
looking after that many unused addresses, and would miss later payments. Once that many
addresses await a payment, `receive` refuses. Give out one of them again.
`addresses` lists them, and `show` shows one again with its QR code.

On a terminal, the QR code of the address comes under it, for the payer to
scan. The code holds a payment URI with the address and nothing else, so it does
not hold the label. Piped into another program, or with --json, `receive` gives
the text alone.
|],
      args = CommandReceive <$> (Receive <$> accountTargetOption <*> labelOption)
    }

labelOption :: Args (Maybe Text)
labelOption =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "LABEL"
      <> help "A label for the address, to recognize the payment later"

{- show -}

showCommand :: CommandSpec Command
showCommand =
  CommandSpec
    { name = "show",
      offline = False,
      summary = "Show an address, a transaction or a coin, or a file of a transaction, of a wallet or of a database",
      details =
        [r|
`show` shows one address, transaction or coin of the wallet, or a file of a
transaction, of a wallet or of a database, and changes nothing. The word given
tells what it is:

  TYPE/0/i       the receiving address i of the account of the type, such
                 as bip84/0/3
  TYPE/1/i       the change address i of the account of the type
  ADDRESS        an address of the wallet
  HASH           a transaction, by its txid, or its nosigHash while pending
  TXID:INDEX     a coin
  FILENAME       a file, any other word

A hash goes in full or by its short id, its first 8 digits. A file whose name
looks like one of the others goes with its directory, such as ./a1075db5. With
no word, `show` shows the newest receiving address of the account, the first
one `addresses` lists. It hands out no new address. The account is the one
--network and --type pick in the active portfolio, as for `receive`.

An address the wallet holds is shown as `receive` shows a new one: its path,
its label and what it received, and on a terminal its QR code under them. Use
it to give an address out again. Any address of the wallet will do, a change
address too. An address the wallet does not hold is refused, so the code never
pays anyone else.

A path starts with the type of its account, as every view writes it. Without
the type, as in /0/3, it is of the account --type picks, and --type is refused
with a path that has its type. A path is derived from the public key of the
account. It needs no mnemonic and nothing of the server, so it runs on an
offline computer as well as online.
Use it on the offline computer to check an address the online computer gave
out. An online computer that was tampered with can show an address of its own
in place of one of the account, and the payer would pay the attacker. The
offline computer holds the same portfolio, since newportfolio made it there,
and derives the same addresses. Run `receive` online, then `show` on the offline
computer with the path `receive` showed, such as bip84/0/3, and compare the two
addresses.

The address at a path the wallet holds is shown as any address of the wallet.
One the wallet does not hold, as on the offline computer, is shown cut: its two
ends, with a dot for each character of the middle, and no QR code. Compare the
ends with the address the online computer shows. They are enough, since nobody
can make another address with the same ends: it would take about 2^128 tries.
The middle is hidden so that nobody pays the address. `receive` did not give it
out, and it may lie past the gap that a restored wallet looks at. Look at the
path too. A tampered online computer picks the path it shows, and a path far
past the addresses you gave out is a sign of that.

A transaction is shown in full, as each portfolio that holds it sees it: a
payment between two portfolios once for each. A coin is shown with every
detail, as `coins` --verbose shows it.

A transaction file is shown as its portfolio sees it, with the name of that
portfolio, which hw finds by itself, as importtx does. A transaction between
two portfolios is shown as the one that pays sees it. The wallet keeps nothing
of it. The file comes from preparetx
--output, exporttx, signtx or preparesweep: a PSBT (BIP 174). Use it to check
a transaction before you sign it, or before you send it. A transaction that
involves no portfolio, such as a sweep to addresses outside the wallet, is
shown on the network its file names.

A file of another wallet is shown the same way: a PSBT in bytes, or in base64
text, of version 0 or of version 2, that of BIP 370. Such a file names no
network. It is read on --network, or on the default network.

The labels shown for a file, the transaction's, those of its addresses and
those of the coins it spends, are the ones the file carries, as the wallet that
wrote it had them. They are not those of this wallet, which an offline computer
does not have.

A file that is neither a PSBT nor the hexadecimal of a transaction is read as
a file of a wallet, the file exportwallet writes and importwallet reads: BIP
329 records. `show` shows what importwallet would make of it. Each wallet of
the file comes with its portfolios: created with their accounts, or found in
the database already. Then each account comes
with the labels of its addresses and transactions, and the coins it locks or
unlocks. A record goes under the account its origin and its network name. The
labels of no account of the file come last, then what hw keeps no label for.
A file that importwallet would refuse is refused here too, with the reason.

A file that starts as an SQLite database does is read as a database of hw: a
copy that `backup` wrote, or one of ~/.hw/auto-backups. `show` lists each wallet
of it, with the fingerprint of its seed and its description, and each of its
portfolios, with the day it was created and its description. A * marks the
active portfolio of that database. Run it before you put a copy in place of
the database, to see which wallets and portfolios come back. The file stays as
it is. Its history and its balances are not shown: a sync brings them up to
date once the copy is in place. A database of another format version than this
hw is refused, with its version.

The first line says which kind of file is shown: "BIP 329 file" for a file of
a wallet, "SQLite database" for a database. A backup has one of each, and both
hold wallets and portfolios. `show` tells them by what they hold, never by
their names.

--network and --type name the account of a path and of the newest address, in
the active portfolio. With a file of a wallet, --wallet and --portfolio narrow
what is shown as they narrow what importwallet reads. --wallet names a wallet
of the file by its fingerprint, and a file of several wallets needs it beside
--portfolio. --network is refused with such a file: each of its records names
its network. --wallet and --portfolio go with such a file only. A database
takes none of these options: every wallet and portfolio of it is shown. An
address and a coin tell where they are, so `show` refuses --type with them.
--network then only tells apart an address or a coin the wallet has on several
networks, since the test networks share their keys.
|],
      args =
        (\shown target fileScope -> CommandShow target fileScope shown)
          <$> optional shownArg
          <*> ( Target
                  <$> networkOptionWith "With a path or with nothing to show, the network of the account. With a file, the network of a file that names none. With an address or a coin, the network to find it on, when the wallet has it on several. Without it, the default-network of the configuration file, or btc."
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
      <> help "What to show: a path such as bip84/0/3, an address of the wallet, a transaction by its txid or nosigHash, a coin, or a file of a transaction, of a wallet or of a database. A hash goes in full or by its short id, the first 8 digits. Without it, the newest receiving address."

-- | What the shell offers for show. It offers the addresses, transactions
-- and coins of the wallet ('shownTargets'), and the files and directories,
-- as for a FILENAME. It leaves out a file whose name show reads as
-- something else, a file named a1075db5 for instance. show would take that
-- word for a short id. The file is offered once ./ is typed before it.
-- Offering ./a1075db5 for the word a1 would not do, since zsh drops a value
-- that does not start with the word typed.
shownCompletion :: String -> IO [String]
shownCompletion typed = do
  held <- shownTargets typed
  files <- completeFile typed
  return (held <> filter readAsFile files)
  where
    readAsFile name = readShown name == Right (ShownFile name)

{- addresses -}

addressesCommand :: CommandSpec Command
addressesCommand =
  CommandSpec
    { name = "addresses",
      offline = False,
      summary = "List the receiving addresses of a portfolio, or its change addresses",
      details =
        [r|
`addresses` lists every receiving address of the active portfolio, grouped by
network and by type: the bip44 addresses on btc, then the bip84 ones, then
those on bch, and so on. A group the portfolio has no address in is left out.
--network and --type show one network or one type only.

In each group, the newest addresses come first. Each takes two lines. The
first gives the type of its account and its derivation path, such as
bip84/0/3, /0/i for a receiving address and /1/i for a change address, then
the address and its label. The second gives what it can spend now, as
`portfolios` describes, and what it received in how many transactions. An
address no transaction has paid yet is unused.

--verbose adds the confirmed and unconfirmed balances of the address, how many
coins it holds, and when it was created.

With --tabular, each address takes one row of a table, under a header that
names the columns: the path, the address, what it can spend, what it received
and in how many transactions, and its label. The amounts are in the unit the
header names. An address no transaction has paid leaves its amounts empty.
Change addresses have no label, so their table has no column for one.

With --csv FILENAME, each address takes one row of a new CSV file, for a
spreadsheet: its wallet, portfolio, network and type, the address, its path,
its chain, receiving or change, and its label, what it can spend, its confirmed
and unconfirmed balances, what it received, its coins and its transactions, and
when it was created. Amounts are in satoshis. Each time comes in UTC, then in
local time, then with the offset of that local time from UTC.

--after names an address of the listing. The listing goes on after it, in its
group, then with the groups that follow.
|],
      args =
        CommandAddresses
          <$> listingTargetOption
          <*> changeAddressesOption
          <*> listingOutputOption
            "Show every detail of each address: its transactions, what it received, its balances, its coins, and when it was created."
            "Show each address on one row of a table: its path, the address, what it can spend and what it received, its transactions and its label."
            "addresses"
          <*> pagingOptions (afterOption "ADDRESS" str "Start after this address")
    }

changeAddressesOption :: Args Bool
changeAddressesOption =
  switch $
    long "change"
      <> help "List the change addresses instead of the receiving ones"

-- | The options that page a listing: the row to start after, as the
-- listing shows it, and how many rows to show at most. A script reads a
-- listing a page at a time with them. Without them, every row is shown.
pagingOptions :: Args (Maybe after) -> Args (Paging after)
pagingOptions after = Paging <$> after <*> maxOption

-- | The --after option of a listing: the row to start after, read as the
-- listing shows it, with the help that names the row
afterOption :: String -> ReadM after -> String -> Args (Maybe after)
afterOption name reader what =
  optional . option reader $
    long "after"
      <> metavar name
      <> help (what <> ", to read the listing --max rows at a time")

maxOption :: Args (Maybe Natural)
maxOption =
  optional . option (maybeReader $ readCount . cs) $
    long "max"
      <> metavar "INT"
      <> help "How many rows to show at most. Every row without it."
  where
    -- A listing of no row at all is not asked for
    readCount s = case readNatural s of
      Just n | n > 0 -> Just n
      _ -> Nothing

{- balances -}

balancesCommand :: CommandSpec Command
balancesCommand =
  CommandSpec
    { name = "balances",
      offline = False,
      summary = "List the addresses of a portfolio that hold coins",
      details =
        [r|
`balances` lists the receiving and change addresses of the active portfolio
that hold coins, grouped by network, with the total of each network after its
addresses. The totals
repeat those of `portfolios`, on purpose: the same figures, beside the
addresses they add up. On btc, the bip44 and the bip84 addresses are listed
together. --network and --type show one network or one type only.

On each network, the largest total comes first. Each takes two lines. The
first gives the type of its account and its derivation path, such as
bip84/0/3, /0/i for a receiving address and /1/i for a change address, then
the address and its label, or that it is a change address. The type tells
apart the addresses of the bip44 and the bip84 accounts, which both have a
/0/3. The second gives three amounts.
The total is the value of all
its coins. The spendable amount is what it can spend now, as `portfolios`
describes. The unspendable amount is the rest. It counts the coins locked by a
pending transaction or by lockcoin, the unconfirmed payments from other
wallets, and the coinbase coins with fewer than 100 confirmations. `coins`
tells which applies to each coin.

With --verbose, each address gives its fields: the total, the number of its
coins, the spendable amount, then the unspendable amount in three parts. They
are the value locked by pending transactions, the value locked with lockcoin,
and the value not spendable yet, which waits for a confirmation.

With --tabular, each address takes one row of a table, under a header that
names the columns: the path, the address, the three amounts, and the label, or
that it is a change address. The amounts are in the unit the header names.

With --csv FILENAME, each address takes one row of a new CSV file, for a
spreadsheet: its wallet, portfolio, network and type, the address, its path,
its chain and its label, its total, what it can spend and the three parts it
cannot. Then come the columns addresses --csv ends with: its confirmed and
unconfirmed balances, what it received, its coins and its transactions, and
when it was created. Amounts are in satoshis. Each time comes in UTC, then in
local time, then with the offset of that local time from UTC.
|],
      args =
        CommandBalances
          <$> listingTargetOption
          <*> listingOutputOption
            "Show every detail of each address: its total and its coins, its spendable amount, and what pending transactions lock, what you lock, and what is not spendable yet."
            "Show each address on one row of a table: its path, the address, its total, spendable and unspendable amounts, and its label."
            "addresses that hold coins"
          <*> pagingOptions (afterOption "ADDRESS" str "Start after this address")
    }

{- label -}

labelCommand :: CommandSpec Command
labelCommand =
  CommandSpec
    { name = "label",
      offline = False,
      summary = "Label an address or a transaction, or describe a portfolio or a wallet",
      details =
        [r|
`label` gives a receiving address or a transaction a label, in place of the
one it had. With the label alone, it describes the active portfolio instead.
An empty LABEL, "", removes the label or the description.

The word before the label tells what to label:

  ADDRESS        a receiving address of the wallet
  TYPE/0/i       the receiving address i of the account of the type, in the
                 active portfolio, such as bip84/0/3
  TXID           a transaction, by its txid, or its nosigHash while pending

With no word before it, the label is the description of the active
portfolio, which `portfolios` shows. With --of-wallet, it is the description
of the wallet of the active portfolio. Wallets and portfolios have fixed
names, wallet0 and portfolio0, which follow their indexes and cannot be
changed. A description says what one is for, in your own words. To describe
another portfolio, switch to it first. A word alone that reads as an address,
a path or a hash is refused, rather than taken for a description. Its label
is most likely missing.

The label of an address names the payments it receives. `addresses`,
`balances` and the transactions that pay the address show it. A change
address takes no label, so a path is that of a receiving address, /0/i. The
address must be one the portfolio gave out, as `addresses` lists them. A path
starts with the type of its account, as every view writes it. Without the
type, as in /0/3, it is of the account --type picks, or of the default-type.

The label of a transaction shows in txs and pendingtxs, in place of the
address a payment pays, and as a Label field with --verbose. preparetx,
preparesendall and preparesweep take one with --label. A pending transaction
is named by the hash pendingtxs shows, its nosigHash or its txid once signed.
Its label goes with it to the files of exporttx and --output, and stays once
the network has it. bumpfee gives a replacement the labels of the
transactions it replaces. A transaction on the network is named by its txid,
as txs shows it. It gets the label in every portfolio that has it, such as
both ends of a payment between two portfolios. Either hash goes in full or by
its short id, its first 8 digits.

An address and a transaction tell where they are, so `label` finds their
portfolio, whichever is active. A path is in the active portfolio. The wallet
may have the same address on several networks, since the test networks share
their keys. A portfolio has an address at the same path on each network it
gave one out on. --network then says which one to label. Without it, `label`
takes the only one, or else the one on the default network.
|],
      args =
        CommandLabel
          <$> ( (\(labelled, label) ofWallet target -> Label labelled (cs label) ofWallet target)
                  <$> optionalBefore
                    ( metavar "ADDRESS|TXID|PATH"
                        <> completer (mkCompleter labelTargets)
                        <> help "What to label: a receiving address of the wallet, as `addresses` lists it, a transaction by its txid or the hash pendingtxs shows, in full or by its short id, the first 8 digits, or a path of the active portfolio, such as bip84/0/3. Without it, the label is the description of the active portfolio."
                    )
                    (metavar "LABEL" <> help "The new label or description, or \"\" to remove it. Quote it if it has spaces.")
                  <*> switch (long "of-wallet" <> help "With the label alone, describe the wallet of the active portfolio, not the portfolio")
                  <*> ( Target
                          <$> namedAddressNetworkOption
                          <*> typeOptionWith "With a path without its type, such as /0/3, the type of its account. Without it, the default-type of the configuration file, or bip44."
                      )
              )
    }

{- txs -}

txsCommand :: CommandSpec Command
txsCommand =
  CommandSpec
    { name = "txs",
      offline = False,
      summary = "List the transactions of a portfolio",
      details =
        [r|
`txs` lists the transactions of the active portfolio, grouped by network.
--network shows one network only. A transaction is shown as the portfolio sees
it, whichever of its accounts it touches: one that spends bip44 and bip84
coins together is one transaction.

The list is as of the last sync. On each network, unconfirmed transactions
come first, then the newest. The transactions you prepared, and those you sent that `sync` has
not seen yet, are in pendingtxs. `show` TXID shows one transaction in full.

Each transaction takes two lines. The first starts with its short id, the first
8 digits of its txid, which the other commands take in place of the txid. It
then gives its time, its amount and its status. The time is in the time zone of
the computer. The amount includes the fee, with a plus sign when it comes in.
The status gives the confirmations, up to 999+ confirmations. --verbose gives
the full count. An unconfirmed transaction shows its fee rate too, which tells
how soon a block may take it. A dead one was replaced, or dropped from the
network.

The second line gives the balance of the portfolio on the network after the
transaction. The newest one shows what the portfolio holds there, confirmed or
not. A dead transaction shows no balance, and does not count. Then comes what
the transaction is for. A payment received gives the address it was paid at by
its type and its path, such as bip84/0/3, then the label of the payment, or
else the label of the address. A payment
gives its label, or else the address it pays, the largest when there are
several. Nothing in it is cut, a long label or address included. On a
terminal, the addresses of the wallet and their labels are blue.

A dead transaction that another one replaced names it first on its second
line, by its short id, as in replaced by 2b69e4f4. The replacement is the live
transaction that spends a coin the dead one spent. hw reads it from the coins
at each listing, so the note follows the network. When a miner confirms a
stuck payment after all, its replacement turns dead and names the payment. A
dead payment of yours that names no replacement was dropped. A dead payment
you received never names one. A replacement that bumpfee made pays the
recipients of the payment. Any other may pay somebody else.

With --verbose, each transaction starts with its time, then its type:
Incoming when funds enter the portfolio, Outgoing when they leave it, or
Internal when they move between its addresses, from a bip44 address to a bip84
one for instance. A payment to another portfolio of the wallet is Outgoing
here and Incoming there. Then come the network and the types of the
portfolio's addresses the transaction involves. The txid is in full, then its
short id. Every address is in full, and an address of
the wallet has its path after it. Its label is on the line below, with the labels of the coins it
spends, after the word from. Long labels wrap under the address. A dead
transaction has a Status field, which names its replacements by their txids in
full.

With --tabular, each transaction takes one row of a table, under a header
that names the columns. The first, Txid, gives the short id. The amounts are in
the unit the header names. The Confs column gives the confirmations as a number
alone, up to 999+. An unconfirmed transaction has 0, and gives no fee rate. A
dead one says dead. The last column, Description, says what the transaction is
for, in the words of the compact view. The description of a dead transaction
that was replaced names its replacement first.

With --csv FILENAME, each transaction takes one row of a new CSV file, for a
spreadsheet: its wallet, portfolio and network, the types of address it
involves, its txid in full and by its short id, its time, its direction,
Incoming, Outgoing or Internal, its amount, fee, fee rate and size, the balance
after it, its status, confirmations and block, the transaction that replaced
it when it is dead, and its label.

Then come three of its addresses: the one it spends the most from, the one it
pays the most, and its change. Each has its path and label when it is one of
the portfolio, and its value. One more column counts the addresses that have
no column of their own. exportsheet lists every address of every transaction
in a tab of its own. The lock time comes last. Amounts are in satoshis. Each
time comes in UTC, then in local time, then with the offset of that local time
from UTC.
|],
      args =
        CommandTxs
          <$> networkOptionWith "Only the transactions on this network. Without it, those on every shown network of the portfolio."
          <*> listingOutputOption
            txVerboseHelp
            "Show each transaction on one row of a table: its short id, time, type, amount, balance and confirmations, then what it is for: its label, the path of an address of the wallet, or the address paid."
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
  fromMaybe (OnScreen Nothing)
    <$> optional
      ( OnScreen . Just <$> viewFlags compactHelp verboseHelp tabularHelp
          <|> CsvFile <$> strOption (long "csv" <> metavar "FILENAME" <> fileCompletion <> help csvHelp)
      )
  where
    compactHelp = "Show the " <> listed <> " in a few lines each. It is the default, unless setdefault or the key listing-view of config.json names another view."
    csvHelp = "Write the " <> listed <> " to this file, one on each row, as CSV for a spreadsheet, with amounts in satoshis and times in UTC and in local time. The file must not exist yet."

-- | The view of a listing, from one of --compact, --verbose and
-- --tabular, with their help in that order. The listings read them, and
-- setdefault reads them for the view it makes the default, so that both
-- take the same words.
viewFlags :: String -> String -> String -> Args View
viewFlags compactHelp verboseHelp tabularHelp =
  flag' Compact (long "compact" <> help compactHelp)
    <|> flag' Verbose (short 'v' <> long "verbose" <> help verboseHelp)
    <|> flag' Table (long "tabular" <> help tabularHelp)

txVerboseHelp :: String
txVerboseHelp = "Show every detail of each transaction: its label, its hash in full and its short id, each address it spends from or pays, with its amount, path and label, the labels of the coins spent as `coins` shows them, and the fee."

{- preparetx -}

prepareTxCommand :: CommandSpec Command
prepareTxCommand =
  CommandSpec
    { name = "preparetx",
      offline = False,
      summary = "Prepare a payment, without signing it",
      details =
        [r|
A payment takes three steps. preparetx builds the transaction, unsigned. It
needs no mnemonic, so it can run on an online computer. signtx then signs it,
on an offline computer if you like, and broadcast sends it to the network.
`send` takes the three steps in one command, with the same options, on a
computer where you type the mnemonic.

The new transaction is kept as a pending transaction, which pendingtxs lists.
--output also writes it to a file, a PSBT (BIP 174), to carry to an offline
computer. A pending transaction is named by its nosigHash, a hash of the
transaction without its signatures. Once signed, it is shown by its txid instead, and the commands
take either hash, in full or by its short id, its first 8 digits. The coins it spends stay locked while it is pending.
deletetx deletes it and frees them.

On Bitcoin, the transaction is marked replaceable (BIP 125). Until it
confirms, bumpfee can replace it with one that pays a higher fee. Bitcoin Cash
does not replace transactions.

preparetx selects among every coin of the active portfolio on the --network,
so one transaction can spend bip44 and bip84 coins together. A SegWit coin
costs less to spend, and the selection counts each coin at its own cost.

Coin control has two kinds of options, choosers and filters. The choosers,
--coin and --spend-from, choose the coins to spend. They add up. The coins
chosen are those of every --coin and of every --spend-from. They must be in
the active portfolio. A coin or an address of another portfolio is refused,
with the portfolio that holds it. --spend-from takes an address, or its path
in the active portfolio with its type, as the views write it, such as
bip44/1/0.

The filters, --min-conf and --coin-type, narrow the coins down. Without a
chooser, they narrow every coin of the portfolio, and the strategy selects
among the coins they keep. With a chooser, they narrow the coins chosen. A
filter never adds a coin. So --coin-type bip44 with --spend-from spends the
coins at that address only, not every bip44 coin. A chosen coin of another
type than --coin-type is refused, since the two options disagree about it.

With a chooser, preparetx spends every chosen coin that can be spent, and no
other, whatever --strategy says. With --select, it spends only what the
payment needs among them. The strategy then selects among the chosen coins as
it selects among all the coins of the portfolio without a chooser, and a
chosen coin worth less than its fee stays. The chosen coins that cannot be
spent, because they are locked or not confirmed enough, are listed with the
reason, with or without --select. The list tells such a coin from one that
--select left because the payment did not need it. When none can be spent,
nothing is prepared.

Change: the change goes to a new change address of the portfolio. Its type is
the first of these that applies. The type --change-type asks for. Else the
type of the coins the payment spends, when they are all of one type, so that a
payment stays within one type of address. Else, for coins of both types, the
default-type of the configuration file, or bip44. On Bitcoin Cash every coin
is bip44, so the change is too, and --change-type bip84 is refused.

--change-back ADDRESS sends the change to that address instead. It must be an
address of the portfolio on the network of the payment, a receiving or a
change address of either type. Its path will do too, such as bip84/1/0. It may have been used already, or be the
change of another pending transaction: you chose it. hw refuses any other
address: change sent elsewhere would be a payment. Some banks accept payments
only from addresses they have checked, and --change-back keeps the funds at
such an address. An address has a type of its own, so --change-back ADDRESS
takes no --change-type.

--change-back largest sends the change back to the address of the largest
coin the payment spends. You do not know in advance which coins the strategy
selects, so this names an address the payment spends from without knowing
it. The coin is the largest of those of the type of the change, which
follows the rule above, so the fee is counted for the right output. Of two
coins of equal value, the one at the lower path wins, a receiving address
before a change address. With --change-type, a payment that spends no coin of
that type is refused, and nothing is prepared. A payment without change
sends nothing back, and is never refused for it.

No output is smaller than what nodes relay for its kind: 546 satoshis for a
legacy P2PKH output, 294 for a native SegWit P2WPKH one. A smaller payment is
refused, and smaller change goes to the fee.

The transaction gets the lock time of the best block the wallet knows, as
most wallets give theirs: it cannot be mined into a block below it, and it
looks like theirs. One time in ten, the lock time is up to 99 blocks
lower, so that the time signing takes does not show. When the best block was
mined more than eight hours ago, the wallet is behind the chain, and a sync may
find coins spent or received since. preparetx then asks whether to prepare the
transaction anyway, with a lock time of 0.

A fee rate above 100 satoshis per virtual byte is more often a typo than a choice, so
preparetx asks before it takes one. --yes answers both questions, for scripts.
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
    <*> underCoins (many coinOption)
    <*> underCoins (many spendFromOption)
    <*> underCoins selectOption
    <*> underFee prepareYesOption
    <*> underChange changeBackOption
    <*> underChange changeTypeOption

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
  networkOptionWith "The payment spends the coins of the active portfolio on this network. Without it, the network of the coins and addresses of --coin and --spend-from, or else the default-network of the configuration file, or btc. With --coin or --spend-from, this only tells apart the test networks, which share their keys."

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
      <> help "Filter: spend only the coins of this type, bip44 or bip84, among every coin or among those --coin and --spend-from choose. A chosen coin of the other type is refused. Without it, coins of both types."

-- | Where the change of preparetx goes in place of a new change address:
-- an address of the portfolio, or that of the largest coin spent. The
-- shell is offered the keyword with the addresses.
changeBackOption :: Args (Maybe ChangeBack)
changeBackOption =
  optional . option (eitherReader readChangeBack) $
    long "change-back"
      <> metavar "ADDRESS|PATH|largest"
      <> completeWith ["largest"]
      <> completer (mkCompleter activeAddresses)
      <> help "Send the change to this address instead of a new change address, or to the address at this path, such as bip84/1/0. It must be an address of the active portfolio, on the network of the payment. It may have been used, or be the change of another pending transaction. largest sends the change back to the address of the largest coin the payment spends, among the coins of the type of the change."

-- | The type of the change of preparetx, when the command says
changeTypeOption :: Args (Maybe AccountKind)
changeTypeOption =
  optional . option (eitherReader (readKind . cs)) $
    long "change-type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help "The type of the change, bip44 or bip84: that of the new change address, or of the coin --change-back largest picks. Without it, the type of the coins spent when they are all of one type, or else the default-type of the configuration file, or bip44."

-- | A recipient of preparetx, ADDRESS:AMOUNT
paymentArg :: Args Text
paymentArg =
  strArgument $
    metavar "ADDRESS:AMOUNT"
      <> help
        [r|
A recipient and the amount it gets. Repeat for several recipients. The amount
is in the unit of --unit or of config.json, bitcoins by default, unless it ends
with btc, bit, bits, sat or sats. ADDRESS:0.00001, ADDRESS:10bits and
ADDRESS:1000sat pay the same. Bitcoins take up to 8 decimals, bits up to 2, and
satoshis none. Separators such as 1'000 are refused. A Bitcoin Cash address
can keep its prefix, as in bitcoincash:qp...:0.001.
|]

{- send -}

-- | send: preparetx, signtx and broadcast in one command. It is not marked
-- as a command to run offline, since it needs the server. Its help warns
-- that the mnemonic is typed on an online computer, as the help of
-- recover does.
sendCommand :: CommandSpec Command
sendCommand =
  CommandSpec
    { name = "send",
      offline = False,
      summary = "Prepare, sign and send a payment in one command",
      details =
        [r|
`send` makes a payment in one command. It prepares the transaction as
preparetx does, shows it, asks for the mnemonic, signs the transaction as
signtx does, and sends it as broadcast does. The result is the one of the
three commands run one after the other with the same options.

`send` needs the mnemonic and the server at the same time, so it runs on an
online computer and not on an offline one. A computer that was tampered with
can steal a mnemonic typed on it. Use `send` for a wallet whose mnemonic you
are willing to type on this computer. To keep the mnemonic offline, pay in
three steps instead: preparetx --output here, signtx --input on the offline
computer, and broadcast --input here.

`send` takes the options of preparetx, all but --output, and builds the same
transaction from them. The help of preparetx explains the coins, coin control,
the change, the lock time and the fee.

The transaction is shown once, before the mnemonic is asked. Check it then:
the addresses, the amounts and the fee. Typing the mnemonic is the
confirmation. `send` asks nothing more before it sends, and sending cannot be
undone. Ctrl-C at the mnemonic stops it. A mnemonic that does not match the
portfolio is asked again, three times in all.

Nothing is kept of a payment that is not signed. When `send` refuses the
payment, when you answer no to a question, and when you stop at the mnemonic,
the wallet is left as it was. It has no new pending transaction, and no coin
is locked.

`send` checks that the server answers before it asks for the mnemonic. Once
the transaction is signed, the wallet keeps it as a pending transaction,
before anything leaves the computer. If the sending then fails, as when no node
takes the transaction, `send` says why, and the transaction stays signed and
pending, as signtx leaves it. broadcast sends it again by its txid, and
deletetx deletes it and frees its coins. The help of broadcast says what the
server answers, and how long it waits.

`send` answers with the txid of the transaction it sent, not with the
transaction again. `sync` then follows it. On Bitcoin, bumpfee replaces a
payment that stays unconfirmed.

--yes answers the two questions of preparetx, on a fee rate above 100 satoshis
per virtual byte and on a wallet behind the chain. The mnemonic is still
asked. A script types it on the input of `send`.
|],
      args = CommandSend <$> paymentOptions (pure Nothing) sentLabelOption
    }

{- preparesendall -}

prepareSendAllCommand :: CommandSpec Command
prepareSendAllCommand =
  CommandSpec
    { name = "preparesendall",
      offline = False,
      summary = "Prepare a payment of all the coins of a portfolio",
      details =
        [r|
preparesendall prepares a transaction that sends every coin the active
portfolio can spend now on the --network, of both types of address, with no
change. That is
the spendable balance `portfolios` shows for that network. Like
preparetx, it needs no mnemonic. The transaction waits among the pending
transactions for signtx.

A recipient given with an amount, ADDRESS:AMOUNT, gets exactly that amount.
The recipients without an amount share the rest in equal parts, and they pay
the fee. So give at least one of them. When the rest does not cover the fee,
nothing is prepared. A fixed amount is never cut.

A coin worth no more than the fee to spend it is a dust coin. Spending it
lowers what the recipients get. Whether a coin is dust depends on the fee
rate. preparesendall lists the dust coins, tells what spending or leaving them
means, and asks whether to spend them. Any answer but yes leaves them in the
portfolio. --spend-dust or --leave-dust answers in advance, for scripts.

The choosers and the filters of coin control work as for preparetx. The
choosers, --coin and --spend-from, add up. preparesendall then sends only the
coins chosen, every one that can be spent, dust included. They must be in the
active portfolio. --spend-dust and --leave-dust do not apply then. The
filters, --min-conf and --coin-type, narrow the coins sent down, every coin of
the portfolio or the coins chosen. A filter never adds a coin. To move every
legacy coin of a portfolio to a native SegWit address of it, send --coin-type
bip44 to an address `receive` --type bip84 gave.

The answer lists every coin the transaction leaves in the portfolio, with the
reason: dust, locked by you or by a pending transaction, not spendable yet,
not chosen with --coin or --spend-from, or not of the type --coin-type sends.

The transaction gets a lock time as preparetx gives one. When the best block
the wallet knows was mined more than eight hours ago, preparesendall asks
whether to prepare the transaction anyway, with a lock time of 0, as preparetx
does.

A fee rate above 100 satoshis per virtual byte is more often a typo than a choice, so
preparesendall asks before it takes one. --yes answers both questions, for
scripts.
|],
      args =
        CommandPrepareSendAll
          <$> ( PrepareSendAll
                  <$> some sendAllRecipientArg
                  <*> paymentNetworkOption
                  <*> underFee feeOption
                  <*> underCoins minConfOption
                  <*> underCoins coinTypeOption
                  <*> outputFileMaybeOption
                  <*> txLabelOption
                  <*> underCoins (many coinOption)
                  <*> underCoins (many spendFromOption)
                  <*> underCoins dustCoinsOption
                  <*> underFee prepareYesOption
              )
    }

sendAllRecipientArg :: Args Text
sendAllRecipientArg =
  strArgument $
    metavar "ADDRESS[:AMOUNT]"
      <> help
        [r|
A recipient that shares what remains, or ADDRESS:AMOUNT for a fixed amount,
written as for preparetx. Repeat for several recipients. Give at least one
without an amount.
|]

-- | What to do with the coins worth less than their fee, without asking
dustCoinsOption :: Args (Maybe DustCoins)
dustCoinsOption =
  optional $
    flag' SpendDust (long "spend-dust" <> help "Spend the coins worth less than their fee too, without asking")
      <|> flag' LeaveDust (long "leave-dust" <> help "Leave the coins worth less than their fee in the portfolio, without asking")

-- | A chooser of coin control: an address of the portfolio whose coins to
-- spend. Its coins add up with those of the other choosers.
spendFromOption :: Args AddressNamed
spendFromOption =
  option (eitherReader readAddressNamed) $
    long "spend-from"
      <> metavar "ADDRESS|PATH"
      <> completer (mkCompleter addressesToSpendFrom)
      <> help "Chooser: spend the coins at this address of the portfolio, or at this path, such as bip44/1/0. Repeat for several addresses. They add up with the coins of --coin."

-- | Whether the strategy selects among the coins the choosers chose,
-- instead of every one of them being spent. It is neither a chooser nor a
-- filter: it changes what is done with the coins chosen.
selectOption :: Args Bool
selectOption =
  switch $
    long "select"
      <> help "Spend only what the payment needs among the coins --coin and --spend-from choose, as --strategy selects them, instead of every one of them"

-- | A chooser of coin control: a coin to spend, as coins lists it, in full
-- or by its short id. It adds up with the coins of the other choosers.
coinOption :: Args CoinRef
coinOption =
  option coinReader $
    coinOptionFields "Chooser: spend this coin, as `coins` lists it, by its short id or with its txid in full. Repeat for several coins. They add up with the coins of --spend-from."
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
      <> help "The fee rate in satoshis per virtual byte, or low, median or high for that rate of recent blocks, as `fees` shows them. Without it, hw shows those rates and asks for a level or a number, median by default. A rate above 100 needs a confirmation."

-- | Whether preparetx and preparesendall take a high fee rate, and prepare
-- while the wallet is behind the chain, without asking
prepareYesOption :: Args Bool
prepareYesOption = yesOption "Answer yes to both questions: a fee rate above 100 satoshis per virtual byte, and a wallet behind the chain"

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
      <> help "Filter: spend only the coins with at least this many confirmations, among every coin or among those --coin and --spend-from choose. At 0, the unconfirmed coins of the wallet's own transactions are spent, its change and what its portfolios pay each other, but no unconfirmed payment from others."

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
How the coins to spend are selected. least-waste tries the other strategies and
an exact match without change, and keeps the coins that waste the least fee,
now and later. random-draw spends coins in random order.
largest-first spends the fewest coins, for the lowest fee now. smallest-first
spends the smallest coins first, to consolidate them. With --coin or
--spend-from, every chosen coin is spent and the strategy does not apply,
unless --select has it select among the chosen coins.
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

outputFileMaybeOption :: Args (Maybe FilePath)
outputFileMaybeOption =
  optional . strOption $
    short 'o'
      <> long "output"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "Write the transaction to this file, as a PSBT (BIP 174). It must not exist yet."

-- | The label of a transaction to prepare. -l labels an address for
-- receive, and a transaction here: one letter, one meaning.
txLabelOption :: Args (Maybe Text)
txLabelOption = txLabelOptionSaying "It goes with the transaction to its --output file, and stays once the network has it."

-- | The label of a transaction send sends, which has no --output file to
-- go to
sentLabelOption :: Args (Maybe Text)
sentLabelOption = txLabelOptionSaying "It stays once the network has the transaction."

-- | --label for a transaction, with what the help of the command says of
-- where the label goes
txLabelOptionSaying :: String -> Args (Maybe Text)
txLabelOptionSaying whereItGoes =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "LABEL"
      <> help ("A label for the transaction, to recognize it later. " <> whereItGoes)

{- pendingtxs -}

pendingTxsCommand :: CommandSpec Command
pendingTxsCommand =
  CommandSpec
    { name = "pendingtxs",
      offline = False,
      summary = "List the pending transactions",
      details =
        [r|
A pending transaction is one that preparetx, preparesendall, bumpfee or
preparesweep made, or that importtx added. It is unsigned, signed, or sent.
`sync` retires it once the network lists it. `show` HASH shows one of them in
full.

pendingtxs lists the pending transactions of every portfolio of every wallet,
not those of the active portfolio alone. Each one waits for you to sign, send
or delete it, and the commands that take one find its portfolio by
themselves, without a switch. They come in one list, by wallet and portfolio,
then by network in the order portfolios shows the networks, and the newest
first on each network. --network shows those of one network only.

Each takes two lines. The first starts with the short id of the transaction,
the first 8 digits of its hash. The hash is the nosigHash of an unsigned
transaction, and the txid of a signed one. The other commands name a pending
transaction by that hash, in full or by its short id. The nosigHash of a signed
one works too. --verbose gives the hash in full, then its short id. The first
line then gives the time the transaction was created, its amount and its
status: not signed, signed and not sent, or sent, with its fee rate.

The second line says where the transaction is: its portfolio and its network.
When the database has more than one wallet, it names the wallet too, as in
portfolio0 of wallet1.
A replacement that bumpfee made then names the transactions it replaces, in
yellow on a terminal: the network keeps only one of them. Last comes what the
transaction is for, as txs says it: its label, or else the address it pays. An
address of the wallet is named by its type and its derivation path. --verbose
names the transactions replaced in full, in yellow too.

A transaction marked conflicted spends a coin that another transaction has
spent, or that is gone, as the server said at the last sync. It cannot be sent
while that lasts. Delete it with deletetx, which frees its coins.

With --tabular, each transaction takes one row of a table, under a header that
names the columns: its short id, its wallet when there are several, its
portfolio and its network, the time it was created, its type, its amount, its
status, its fee rate in sat/vB, and what it is for, as txs --tabular says it. The description of a replacement
names first, in yellow, the transactions it replaces.

With --csv FILENAME, each transaction takes one row of a new CSV file, for a
spreadsheet, in the columns of txs --csv that apply to it, in the same order.
Its nosigHash comes before its txid, each in full and by its short id. It has
no time on the chain, no balance, confirmations or block. It adds the change
its fee took in, and the transactions it replaces. When it was created comes
last. Amounts are in satoshis. Each time comes in UTC, then in local time,
then with the offset of that local time from UTC.
|],
      args =
        CommandPendingTxs
          <$> networkOptionWith "Only the pending transactions on this network. Without it, those on every shown network of every portfolio."
          <*> listingOutputOption
            txVerboseHelp
            "Show each transaction on one row of a table: its short id, wallet when there are several, portfolio, network, time, type, amount, status and fee rate, then what it is for."
            "pending transactions"
          <*> pagingOptions (afterOption "HASH" hashRefReader "Start after this pending transaction, by either of its hashes or its short id")
    }

{- exporttx -}

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

importExportTxDetails :: String
importExportTxDetails =
  [r|
exporttx writes a pending transaction to a file, signed or not, to carry it to
an offline computer. Name it by the hash pendingtxs shows: its nosigHash, or
its txid once it is signed, in full or by its short id.

The file is a PSBT, the format of BIP 174 that most wallets
read. It carries the transaction, the whole transaction each of its coins
comes from, the keys of the portfolio in it with their derivation paths, and
the signatures once signed. What is hw's own goes in the proprietary fields
of the format, under the identifier hw, which other software leaves aside:
the network of the transaction, the fee rate it was built for, the
transactions a replacement replaces, the label of the transaction, the
labels of the portfolio's addresses in it, and the labels of the coins it
spends, as the wallet has them when it writes the file, for the offline
computer to show. bitcoin-cli decodepsbt shows the file too.

importtx adds the transaction of a file to the pending transactions of a
portfolio, and locks the coins it spends. hw finds the portfolio by itself,
from the addresses the transaction spends from and pays. A transaction that
moves funds between two portfolios goes to the portfolio that pays. When hw
cannot tell, because several portfolios pay it, or none pays and it pays
several, it takes the active portfolio when that is one of them. Otherwise it
names them: switch to the one to use, and import the file again. When the
wallet holds the
transaction already, the file takes its place, as when an offline computer
returns it signed. The signatures are checked first. The wallet keeps its own
labels, and shows the transaction with them.

A file another wallet or a hardware wallet signed is imported the same way,
in bytes or in base64 text, finalized or not: hw finalizes an input left
signed, and refuses a signature that does not verify. What the file leaves
out, the paths of the portfolio's addresses once the signatures are final, the
fee rate and the label, the wallet keeps from the version it holds. A file
another wallet prepared for a portfolio of this wallet is imported too, and
signed with signtx.

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

deleteTxCommand :: CommandSpec Command
deleteTxCommand =
  CommandSpec
    { name = "deletetx",
      offline = False,
      summary = "Delete a pending transaction and free its coins",
      details =
        [r|
deletetx removes a pending transaction from the wallet. The coins it spends
can then be spent again, and its change address is free again.

A transaction sent with broadcast can be deleted only once the server no longer
knows it. That happens when the nodes dropped it from their mempools, or never
accepted it. While the server lists it, it may still confirm, and the next
`sync` retires it.

Delete a transaction that pendingtxs marks as conflicted. One of its coins was
spent by another transaction, or is gone, so it can no longer be sent.
|],
      args = CommandDeleteTx <$> pendingHashArg
    }

{- signtx -}

signTxCommand :: CommandSpec Command
signTxCommand =
  CommandSpec
    { name = "signtx",
      offline = True,
      summary = "Sign a pending transaction or a transaction file",
      details =
        [r|
signtx asks for the mnemonic and the passphrase of the wallet, and signs the
transaction. It asks again while they do not match the portfolio. It takes the
shares of a backup in place of the mnemonic. A transaction that spends bip44
and bip84 coins together is signed at once: the mnemonic gives the keys of
both accounts.

Give a pending transaction by its NOSIGHASH, in full or by its short id. The
signed transaction then takes its place in the wallet, shown by its txid. Or give a transaction file with
--input, on an offline computer for example. The signed transaction then goes
to the --output file. Without --output, hw offers to replace the --input file,
since the unsigned file is of no use once signed. It asks this before the
mnemonic. No option answers that question, so a script gives --output: the
question would take the line of the mnemonic as its answer, read it as a no,
and stop. A transaction of a file is shown with the labels the file carries,
which go on to the signed file. A signed input names no key any more, as the
format asks. The file still says which address of the portfolio it spends from,
in a field of hw, so `show` shows the signed file as the payment it is, on
the offline computer too.

hw finds the portfolio of a file by itself, as importtx does. A transfer
between two portfolios is signed by the one that pays it. A file of a
portfolio this database does not have yet is refused, as on an offline
computer that holds portfolio0 alone. newportfolio --existing adds the next
portfolio of the mnemonic there, and importwallet the portfolios of a file.

A file another wallet prepared for the portfolio is signed the same way, in
bytes or in base64 text, of version 0 or of version 2, that of BIP 370. The
signed file keeps its version and every field of it, but those of the inputs
signed that served the signers. hw signs with the
sighash ALL only, and only when every input that is not the portfolio's is
signed already.

Signing sends nothing. Check the transaction with `show`, given its txid or its
file. Then send it with broadcast, or delete it with deletetx.
|],
      args =
        CommandSignTx
          <$> signingOptions
    }

inputFileMaybeOption :: Args (Maybe FilePath)
inputFileMaybeOption =
  optional . strOption $
    short 'i'
      <> long "input"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "Read the transaction from this file"

{- coins -}

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
bip44 and the bip84 coins are listed together, as a payment spends them.
--network and --type show one network or one type only.

On each network, the unconfirmed coins come first, then the newest. Each takes two lines. The first
starts with the short id of the coin:
the first 8 digits of its txid, a colon, and its index, such as a1075db5:1.
lockcoin, --coin and `show` take it, or the coin in full. The first line then gives the
time of the transaction that paid the coin, in the time zone of the computer,
its value, its state and its confirmations, up to 999+ confirmations. A coin
that a pending transaction spends is "locked by tx". The second line gives the
address that holds the coin, then the type of its account and its derivation
path, such as bip84/0/3, then the label of the address. The address shows its
type too: a bip84 one starts with bc1q on Bitcoin. A coin at a change address
says so there.

With --verbose, each coin starts with the time of its transaction, and gives the
full count of its confirmations on its first line. A coin that a pending
transaction spends is locked by a pending transaction. Its fields follow: the label of the transaction that paid it, the
coin in full and its short id, the block that holds it, the time of its transaction to the second, and
the address in full with its path. The label of the address is on the line
below it. An unconfirmed coin is in no block yet. Coins have no labels of their
own. The change of a payment you made shows no label of its transaction: the
label says what the payment was for, not what its change is.

With --tabular, each coin takes one row of a table, under a header that names
the columns: the short id of the coin, the time of its transaction, its value,
its state, its confirmations up to 999+, the address that holds it with its
path, and the label of the address. A coin locked by a pending transaction says
"locked by tx".

With --csv FILENAME, each coin takes one row of a new CSV file, for a
spreadsheet: its wallet, portfolio, network and type, the coin, TXID:INDEX, in
full and by its short id, its txid in full and by its short id, the index of
its output, the time of its transaction, its value, its state, its
confirmations and block, the address that holds it with its path, chain and
label, and the label of its transaction. Amounts are in satoshis. Each time
comes in UTC, then in local time, then with the offset of that local time from
UTC.

A coin is spendable when the wallet can spend it now. Nothing locks it, a
coinbase coin has 100 confirmations, and an unconfirmed coin comes from a
transaction of the wallet: its change, or what another portfolio of the wallet
paid. The other coins become spendable once they confirm, or for a coinbase,
once it matures.

A pending transaction locks the coins it spends, until the network lists it or
deletetx deletes it. You can also lock a coin yourself with lockcoin, until
unlockcoin.
|],
      args =
        CommandCoins
          <$> listingTargetOption
          <*> lockedOption
          <*> listingOutputOption
            "Show every detail of each coin: the full count of its confirmations, the label of its transaction, the coin in full and its short id, the block that holds it, the time of its transaction, and its address in full with its path."
            "Show each coin on one row of a table: its short id, the time of its transaction, its value, state and confirmations, its address and path, and the label of the address."
            "coins"
          <*> pagingOptions (afterOption "TXID:INDEX" coinReader "Start after this coin, with its txid in full or by its short id")
    }

lockedOption :: Args Bool
lockedOption =
  switch $
    long "locked"
      <> help "List only the locked coins: those a pending transaction spends, and those locked with lockcoin"

{- lockcoin and unlockcoin -}

lockCoinCommand :: CommandSpec Command
lockCoinCommand =
  CommandSpec
    { name = "lockcoin",
      offline = False,
      summary = "Keep a coin from being spent",
      details =
        [r|
A locked coin is held back. preparetx and preparesendall do not spend it, and
--coin refuses it. No imported transaction or sweep may spend it either.
unlockcoin frees it.

Only a free coin can be locked. A coin that a pending transaction spends is
locked already, and deletetx frees it. The lock stays while the coin confirms,
and goes with the coin once it is spent elsewhere. `coins` --locked lists the
locked coins.

lockcoin shows the coin it locked with every detail, as `coins` --verbose does.

A coin tells where it is. The test networks share their keys, so the wallet
may have the same coin on several of them: --network then says which one to
lock.
|],
      args = CommandLockCoin <$> coinArg coinsToLock <*> heldNetworkOption "coin"
    }

unlockCoinCommand :: CommandSpec Command
unlockCoinCommand =
  CommandSpec
    { name = "unlockcoin",
      offline = False,
      summary = "Unlock a coin locked with lockcoin",
      details =
        [r|
Only a coin locked with lockcoin can be unlocked. A coin that a pending
transaction spends is freed by deleting that transaction with deletetx.

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
  Nothing -> Left (notACoin s)

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

bumpFeeCommand :: CommandSpec Command
bumpFeeCommand =
  CommandSpec
    { name = "bumpfee",
      offline = False,
      summary = "Replace a stuck payment with one that pays a higher fee",
      details =
        [r|
bumpfee prepares a transaction that replaces a payment of a portfolio that
the network has not confirmed yet. The replacement pays a higher fee, as BIP
125 allows on Bitcoin. It spends the same coins and pays the recipients the
same amounts. The higher fee comes out of the change.

hw finds the portfolio by the transaction: the one that holds it, or among
several the one that pays it. When it cannot tell, it takes the active
portfolio when that is one of them, or names them for you to switch to the
one that pays.

When the change cannot pay the higher fee, hw offers to add confirmed coins of
the portfolio, of either type. When none would do, or when you decline them, hw offers to take
what is missing from what the recipients receive instead: one of them pays it
alone, or all of them pay it in equal shares, the first also paying the
remainder. hw asks which, and leaves no recipient below the dust limit.

--yes takes what pays first: the change, then coins of the portfolio, then a
lone recipient, then all the recipients in equal shares. When equal shares
would leave a recipient below the dust limit, hw still asks which one pays
alone, since --yes picks no recipient.

--recipients-pay has the recipients pay what the higher fee adds, in equal
shares, as it has them pay the fee with preparetx. The change keeps its amount,
and no coin is added, even with --yes. hw asks nothing about who pays.

Another transaction of the portfolio may spend the change of the stuck one.
Replacing the stuck one would remove it too, so hw offers to replace both with
one transaction that pays the recipients of both.

The replacement is a pending transaction. Sign it with signtx and send it with
broadcast, which then drops the transactions it replaces from the pending
transactions. Once sync has seen the replacement on the network, txs shows the
stuck payment dead, and names the replacement on it. To bump again, name the
replacement.

The network does not tell hw when it refuses a replacement. A replacement
must also pay the fees of transactions hw cannot see, such as a recipient's
spend of the payment. When a replacement never confirms, delete it with
deletetx, run `sync`, and bump again at a higher fee rate.

Some transactions cannot be bumped. One sent before hw marked its transactions
replaceable cannot. Neither can one that spends coins of another wallet, such
as a sweep. Bitcoin Cash does not replace transactions. When the stuck
transaction pays several addresses of the wallet besides its recipients, its
change among them, the replacement gets one change output for all of them.
Change that --change-back sent back to an address the payment spends from is
still change, and pays the higher fee. --change-back largest always sends it
to such an address. Change that --change-back sent to
another receiving address is a payment to the replacement: it is kept, and
not reduced.

The replacement gets a lock time as preparetx gives one. When the best block
the wallet knows was mined more than eight hours ago, bumpfee asks whether to
prepare it anyway, with a lock time of 0, as preparetx does.
|],
      args =
        CommandBumpFee
          <$> ( BumpFee
                  <$> bumpedArg
                  <*> replacementFeeOption
                  <*> recipientsPayOption "The recipients pay what the higher fee adds, in equal shares taken from their amounts. The change keeps its amount, and no coin of the portfolio is added. Nothing is asked about who pays."
                  <*> outputFileMaybeOption
                  <*> yesOption "Answer yes to every question: a fee rate above 100 satoshis per virtual byte, a wallet behind the chain, adding coins, reducing what a lone recipient receives, and replacing the transactions that spend the change. When no coin can be added, several recipients pay in equal shares. hw still asks which one pays alone when equal shares would leave one below the dust limit."
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
      <> help "The fee rate of the replacement in satoshis per virtual byte, or low, median or high for that rate of recent blocks, as `fees` shows them. It must be above the rate of the transaction replaced. Without it, hw shows those rates and asks for a level or a number. A rate above 100 needs a confirmation."

{- broadcast -}

broadcastCommand :: CommandSpec Command
broadcastCommand =
  CommandSpec
    { name = "broadcast",
      offline = False,
      summary = "Send a signed transaction to the network",
      details =
        [r|
Sending cannot be undone. broadcast shows the transaction, as `show` does, and
sends it only if you answer yes. --yes sends it without showing it or asking,
for scripts.

The server hands the transaction to its nodes. A haskoin-store of version 1.11
or later answers once a node has announced the transaction back. It waits for
that, 30 seconds unless its operator set another time. It says why when it can
tell that no node will take the transaction, as for a coin spent already. It
says that no node announced the transaction when the nodes kept silent, as
they do for a fee below what they relay. broadcast then fails with what the
server said, and the transaction stays signed and not sent. An older server
answers at once, whatever came of the transaction.

broadcast then waits up to ten seconds for the server to list the transaction
in its mempool. If the server does not list it, nothing is recorded, and
broadcast says so. Run broadcast again later.

Give a pending transaction by its TXID, as signtx and pendingtxs show it, in
full or by its short id. Its nosigHash works too. Once the server sees it, it
is marked as sent. `sync` then sends it again whenever the server loses it,
and retires it once the network lists it. When the server does not publish it
again, `sync` says so on every sync, with what the server said, until you
delete the transaction with deletetx.

A payment that `sync` has seen on the network is no longer pending, so
broadcast has nothing to send for it, and says so. `txs` shows where it
stands.

Give a signed file with --input, such as the one an offline computer returns.
When the wallet holds its transaction as pending, the file takes its place and
is sent as above. Otherwise, a sweep of outside coins for example, hw checks
the transaction and sends it, and changes nothing in the wallet.

The file is a PSBT, in bytes or in base64 text, as another wallet or a
hardware wallet returns it, finalized or not. The signed transaction alone,
in hexadecimal as a wallet gives it once finalized, is sent too, when the
wallet holds its transaction. A file hw wrote names its network. One of
another wallet names none: when the wallet holds its transaction, the
wallet tells it; otherwise it is sent on --network, or on the default
network.
|],
      args =
        CommandBroadcast
          <$> optional signedTxidArg
          <*> inputFileMaybeOption
          <*> yesOption "Send without showing the transaction and asking first"
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

syncCommand :: CommandSpec Command
syncCommand =
  CommandSpec
    { name = "sync",
      offline = False,
      summary = "Update the portfolios from the network",
      details =
        [r|
`sync` asks the server for the latest balances, transactions and coins of
every portfolio, on every network, the test networks included. For each
portfolio and network that holds or changed anything, it shows the balances
as `portfolios` does, the block it is synced to, and what changed. --network
syncs one network only.

On a terminal, a line tells what `sync` is doing while it works, with a bar for
the steps whose size it knows. The line goes to stderr and is gone when `sync`
ends, so scripts never see it.

`sync` works on the networks hw hides too, so that a network is up to date the
day you show it with setdefault --show-network. The line names them as it
works. The answer leaves them out, and a hidden network whose server is down
is no error.

It first looks for used addresses past the ones each account has, as far as
the gap of the configuration file, 20 by default. Someone paying to the
account's public key, or another wallet of the same mnemonic, may have used
them. `sync` adds each used address it finds, with the ones before it. It
keeps looking while it finds more, so the first sync of a portfolio finds
every address used so far. It misses an address that comes after more unused ones in
a row than the gap. Raise the gap for one sync to look further. The same gap
limits how many unused addresses hw `receive` gives out.

When the server reports used addresses a thousand deep, and more beyond,
`sync` asks whether to keep looking. A server that lies reports them without
end, and the addresses it would make `sync` generate are past any gap a wallet
restoring the mnemonic looks at. A `sync` that cannot ask stops there, and
generates none of them.

`sync` also checks the pending transactions. One the network lists is retired,
even under another id, as when a miner or another signer changed its
signatures. One you sent that the server no longer knows is sent again, since
nodes forget unconfirmed transactions. One whose coin another transaction
spent is marked conflicted. So is one whose coin is gone, as after a
reorganization that removed the payment the coin came from. A signed
transaction you never sent is left alone. `pendingtxs` lists every portfolio, so the next steps
`sync` prints after a conflict are `pendingtxs` and deletetx, wherever the
conflicted one is. `txs` lists the active portfolio alone, so when `sync` finds
new transactions in other portfolios alone, its next steps start with the
`switch` to them.

A sent transaction is sent again on every sync while the server does not list
it, unless it is conflicted. When the server answers that it did not publish
it, `sync` goes on. It names the transaction with what the server said, and
gives the deletetx that frees its coins. That happens on every sync for a
transaction the network refuses for good, for a fee below what the nodes
relay for example. A haskoin-store waits for its nodes before it says so, 30
seconds by default, and such a sync takes that long. A server that gives no
answer at all fails the sync of its network, as on any other question.

A server seldom serves every network: the public one has no regtest chain.
`sync` names the networks the server does not serve in one line, and syncs
the others. That is no error.

When the server of a network is down, `sync` leaves out that network and says
why. It still syncs the other networks, then exits with an error, so that a
script can tell.
|],
      args =
        CommandSync
          <$> networkOptionWith "Only this network. Without it, every network, the hidden ones too."
          <*> fullOption
    }

fullOption :: Args Bool
fullOption =
  switch $
    long "full"
      <> help
        [r|
Download the balances, transactions and coins of the portfolios again and write
them anew, instead of only what changed, and replace any damaged copy of a raw
transaction. A transaction the server no longer lists is marked as dead. Use it
if the wallet shows something wrong. It keeps what the server cannot tell:
labels, locked coins and pending transactions. It is not needed for a
reorganization of the chain, which `sync` sees by itself.
|]

{- backup -}

backupCommand :: CommandSpec Command
backupCommand =
  CommandSpec
    { name = "backup",
      offline = False,
      summary = "Copy the wallet database to a backup file",
      details =
        [r|
The copy holds the whole database: wallets, portfolios, addresses, labels,
coins, transactions and pending transactions. It holds no private key.

Without a FILENAME, the copy goes to ~/.hw/user-backups, and its labels go
beside it, in the file exportwallet writes. Both files are named after the
format version of the database, the time in UTC, and how many wallets and
portfolios it holds. hw never removes them.

`show` FILENAME lists the wallets and the portfolios of a copy, and leaves it
as it is. Run it before you restore a backup, to see what comes back. It says
"SQLite database" of the copy and "BIP 329 file" of the file of its labels, so
that you can tell the two files of a backup apart.

To restore a backup, replace ~/.hw/portfolios.sqlite with the copy, while no
hw command runs. Then run any command. A copy of the database of hw 0.9 or
earlier is refused, and left as it is: create the wallet again from its
mnemonic instead. So is a copy of the database of a newer hw. The file of the
labels goes to importwallet, in a database of any version.

hw also copies the database and the labels into ~/.hw/auto-backups by itself,
and keeps the ten newest of each. It copies the database after each command
that adds or deletes a wallet or a portfolio, and the labels after each
command that changes them. hw removes the older copies it made there, and no
other file. In config.json, the key autobackup-copies sets how many copies hw
keeps of each, and the keys autobackup-database and autobackup-labels turn
these copies off.
|],
      args =
        CommandBackup
          <$> optional (fileArgument "The file to copy the database to. It must not exist yet. Without it, the copy and its labels go to ~/.hw/user-backups.")
    }

{- exportsheet -}

exportSheetCommand :: CommandSpec Command
exportSheetCommand =
  CommandSpec
    { name = "exportsheet",
      offline = False,
      summary = "Write every listing of the wallet to a spreadsheet",
      details =
        [r|
exportsheet writes the portfolios, the addresses, the balances, the coins, the
transactions and the pending transactions to a new spreadsheet, a tab each, in
the OpenDocument format of LibreOffice (.ods). Excel and Google Sheets open it
too. Each tab holds every row of its listing, in the columns of its --csv
file, in the same order: rows copied from one fit the other.

Two more tabs hold the addresses of the transactions and of the pending
transactions. The row of a transaction names three addresses: the one it
spends the most from, the one it pays the most, and its change. These tabs
have a row for every address of every transaction instead: its role, from, to
or change, its value, the address, its path, chain and label when the wallet
holds it, and the wallet and portfolio that hold it. Each row repeats the
txid, the time, the direction and the status of its transaction. --csv writes
no file of these rows.

The tabs hold every portfolio of every wallet. Each row starts with the
columns that say where it is: its wallet, its portfolio, its network and its
type. The tab of the portfolios says which one is active. The addresses of both
chains share a tab, the receiving ones first. Each time comes in UTC, then in
local time, then with the offset of that local time from UTC.

Each cell keeps its kind. A label is text, whatever it starts with, an amount
is a number of satoshis, and a time is a time. The header of each tab stays at
the top as its rows scroll.

The listings are read at one moment, so the tabs agree with each other.
The file must not exist yet. It is readable by you alone, since it tells your
whole history.
|],
      args =
        CommandExportSheet
          <$> fileArgument "The spreadsheet to write, such as wallet.ods. It must not exist yet."
    }

{- fees -}

feesCommand :: CommandSpec Command
feesCommand =
  CommandSpec
    { name = "fees",
      offline = False,
      summary = "Show the fee rates of recent blocks",
      details =
        [r|
fees reads the latest six blocks that hold fee-paying transactions. It shows
three rates, in satoshis per virtual byte, the unit the network charges
fees in, where the bytes of a signature in a witness count a quarter:

  low      a quarter of the virtual bytes in those blocks paid less
  median   half of the virtual bytes paid less
  high     nine tenths of the virtual bytes paid less

The high rate almost surely gets into the next block. The low rate usually
does when the network is calm. The age of the latest block tells how fresh
the rates are. A block mined an hour ago means a slow network, and rates that
may be too low.

The rates show what got into blocks, not how long transactions waited. After
a quiet spell, a sudden backlog makes them too low for a while. bumpfee then
helps.

preparetx, preparesendall, bumpfee and preparesweep show the same rates, and
ask which to take when --fee-rate is not given. --fee-rate low, median or high
takes one without asking.
|],
      args = CommandFees <$> networkMaybeOption
    }

{- signmsg -}

signMsgCommand :: CommandSpec Command
signMsgCommand =
  CommandSpec
    { name = "signmsg",
      offline = True,
      summary = "Sign a message with the key of an address",
      details =
        [r|
signmsg asks for the mnemonic of the wallet of the address, or for the
shares of its backup, and signs the message with the key of that address. The signature uses the Bitcoin Signed
Message format, the legacy format of BIP 322. From a P2PKH address, which
starts with 1, any wallet can verify it. From a native SegWit address,
which starts with bc1q, the signature has the header byte of BIP 137, which
the wallets that sign from such an address give and verify. Some wallets
verify no signature from a SegWit address.

The message is signed exactly as given. hw warns about spaces or line breaks
at its start or end. Copying a message easily changes them, and the signature
would then fail to verify.

An address must be one the wallet holds, one this computer gave out. The
offline computer gives out none, since receive runs online. There, give the
path of the address instead, with its type, as addresses shows it online,
such as bip84/0/3. signmsg derives the address at the path from the public
key of the account, in the active portfolio, on --network or the default
network, as show does. The signed message names that address. Sign with an
address you gave out: a payment to an address far past the others would not
be found.

With --output, hw writes the armored block to a file instead of printing it.
verifymsg --input can then check the file. It also reads the --json answer,
saved to a file.
|],
      args =
        CommandSignMsg
          <$> (SignMsg <$> signAddressArg <*> signMsgArg <*> signedMessageOutputOption <*> signNetworkOption)
    }

signAddressArg :: Args AddressNamed
signAddressArg =
  argument (eitherReader readAddressNamed) $
    metavar "ADDRESS|PATH"
      <> completer (mkCompleter walletAddresses)
      <> help "The address whose key signs. It must be an address this wallet holds. A path, such as bip84/0/3, names the address the active portfolio derives there, held or not, as on the offline computer."

-- | The network of signmsg: that of an address the wallet has on several,
-- or that of the account of a path, which the path does not say
signNetworkOption :: Args (Maybe Network)
signNetworkOption =
  networkOptionWith "With an address, the network to find it on, when the wallet has it on several. Without it, the only one, or else the one on the default network. With a path, the network of its account. Without it, the default-network of the configuration file, or btc."

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
      <> help "Write the armored block to this file instead of printing it. It must not exist yet."

{- verifymsg -}

verifyMsgCommand :: CommandSpec Command
verifyMsgCommand =
  CommandSpec
    { name = "verifymsg",
      offline = False,
      summary = "Check the signature of a message",
      details =
        [r|
verifymsg checks that the key of ADDRESS signed MESSAGE, from the SIGNATURE
that signmsg or another wallet gave. The address can be anyone's:
a P2PKH address, which starts with 1, or a SegWit address of one key, native
or wrapped, which starts with bc1q or 3. A signature from a SegWit address
is valid when its key gives the address, whatever header byte the wallet
that made it chose.

hw reads the address on the network it is written for, among those it
shows, so it needs no --network. A message is signed the same way on every
network, so the network does not change the answer. An address that only
hidden networks read is refused, with the command that shows one.

Give the message exactly as it was signed. A single changed byte makes the
signature invalid. When it is invalid, hw says so and exits with an error, so
that a script can tell.

With --input, hw reads the three from a file holding the output of signmsg:
its armored block, which other wallets write too, or its --json answer. In a
block, the message is everything between the first marker and the signature
marker, exactly as it is. After the signature marker come the address, then
the signature.
|],
      args = CommandVerifyMsg <$> (VerifyMsg <$> source <*> verifyNetworkOption)
    }
  where
    source =
      (Given <$> verifyAddressArg <*> signatureArg <*> verifyMsgArg)
        <|> (FromFile <$> signedMessageFileOption)

-- | The network of verifymsg, which the user seldom needs: hw reads the
-- address on any network it shows that reads it. It is kept for a user
-- who wants the address read on one network alone, and refused when it
-- is not an address there.
verifyNetworkOption :: Args (Maybe Network)
verifyNetworkOption =
  networkOptionWith "Read the address on this network alone. Without it, hw reads it on any network it shows that reads it. Each gives the same answer."

signedMessageFileOption :: Args FilePath
signedMessageFileOption =
  strOption $
    short 'i'
      <> long "input"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "Read the address, the signature and the message from this file: the output of signmsg, as its armored block or its --json answer"

verifyAddressArg :: Args Text
verifyAddressArg =
  strArgument $
    metavar "ADDRESS"
      <> help "The address the message is said to be signed by"

signatureArg :: Args Text
signatureArg =
  strArgument $
    metavar "SIGNATURE"
      <> help "The signature in base64, as signmsg writes it"

verifyMsgArg :: Args Text
verifyMsgArg =
  strArgument $
    metavar "MESSAGE"
      <> help "The message, exactly as it was signed"

{- preparesweep -}

prepareSweepCommand :: CommandSpec Command
prepareSweepCommand =
  CommandSpec
    { name = "preparesweep",
      offline = False,
      summary = "Prepare a transaction that moves coins from outside the wallet",
      details =
        [r|
A sweep moves coins whose private keys you hold, from an old wallet for
example, to new addresses. preparesweep takes every coin at the --sweep-from
addresses and at those listed in the --address-file, every coin at the
addresses of the keys in the --key-file, and the --coin coins, each with its
txid in full: the wallet does not hold them, so a short id would name none.
Give any of them. The --address-file holds addresses separated by spaces or line breaks,
as the --key-file holds keys. An old wallet may have more addresses than a
command line takes.

preparesweep needs the server, so it runs on an online computer and not on an
offline one. With --key-file, the private keys are read on that computer too,
and one that was tampered with can steal them. The keys are needed only to
sign, with signsweep, so keep them on an offline computer if you can. Name the
coins with --sweep-from, --address-file or --coin instead. Without the keys, hw
counts every key as compressed, so the fee of a coin of an uncompressed key
comes out a little low.

A sweep takes the coins that have an address: legacy coins, native SegWit
coins, and SegWit coins nested in a script address. A coin paid to a public
key itself, as the first blocks hold them, has no address, and is not swept.

Give each destination with --sweep-to, as ADDRESS or ADDRESS:AMOUNT, written as
for preparetx. An address with an amount gets exactly that amount. The
addresses without one share the rest in equal parts, and they pay the fee. So
give at least one of them. When the rest does not cover the fee, nothing is
prepared. preparesendall shares the coins the same way.

A sweep that pays an address of this wallet belongs to the portfolio of that
address, on its network. hw finds the portfolio from the --sweep-to addresses,
however an address is written, such as a Bitcoin Cash address with or without
its prefix, or in its legacy form. When those addresses belong to several
portfolios, the sweep is of the active one when it is among them. Otherwise hw
names them: switch to the one to keep the sweep in. A sweep is on one network,
so addresses of this wallet on two networks are refused. The sweep is kept as a
pending transaction, to sign with signsweep by its nosigHash. --output also
writes it to a file.

A sweep that pays no address of this wallet needs no portfolio, even in a
database that has none. It needs --network and --output, and is signed from the
file. The default-network of the configuration file does not apply. Private
keys, coins and the legacy addresses of Bitcoin Cash read the same on
Bitcoin, so a wrong guess would sweep the coins of the other chain.

The sweep gets a lock time as preparetx gives a payment one, drawn from the
best block of the server rather than from the one the wallet knows: a sweep is
prepared online.

A fee rate above 100 satoshis per virtual byte is more often a typo than a choice, so
preparesweep asks before it takes one. It asks before the private keys are
typed. --yes takes it without asking, for scripts.
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

sweepFromOption :: Args Text
sweepFromOption =
  strOption $
    short 'w'
      <> long "sweep-from"
      <> metavar "ADDRESS"
      <> help "An address whose coins to sweep. Repeat for several addresses."

addressFileOption :: Args (Maybe FilePath)
addressFileOption =
  optional . strOption $
    long "address-file"
      <> metavar "FILENAME"
      <> fileCompletion
      <> help "A file of addresses whose coins to sweep, separated by spaces or line breaks"

sweepToOption :: Args Text
sweepToOption =
  strOption $
    short 't'
      <> long "sweep-to"
      <> metavar "ADDRESS[:AMOUNT]"
      <> help "A destination that shares what remains, or ADDRESS:AMOUNT for a fixed amount. Repeat for several destinations."

{- signsweep -}

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

Give a pending sweep by its NOSIGHASH. The signed sweep then takes its place
in the wallet, shown by its txid. Or give a sweep file with --input. The
signed sweep then goes to the --output file. Without --output, hw offers to
replace the --input file. It asks this before the keys. No option answers
that question, so a script gives --output. A sweep of a file is
shown with the labels the file carries, which go on to the signed file.

A sweep that pays no portfolio of this wallet needs no portfolio here either.
It is signed on the network its file names. A file of another wallet names
none: it is signed on --network, or on the default network.
|],
      args =
        CommandSignSweep
          <$> signingOptions
          <*> prvKeysMaybeOption
          <*> fileNetworkOption
    }

-- | The private keys of signsweep, which can run on an offline computer
prvKeysMaybeOption :: Args (Maybe KeySource)
prvKeysMaybeOption = keyFileOption keyFileHelp

-- | The private keys of preparesweep. It needs the server, so they are
-- read on an online computer, and it can do without them.
sweepKeysOption :: Args (Maybe KeySource)
sweepKeysOption = keyFileOption (keyFileHelp <> " This puts them on an online computer, so leave it out if you can.")

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

-- | A text argument, with its name in the usage line and its help
textArg :: String -> String -> Args Text
textArg name desc =
  argument str $
    metavar name
      <> help desc

fileArgument :: String -> Args FilePath
fileArgument desc =
  strArgument $
    metavar "FILENAME"
      <> fileCompletion
      <> help desc

-- | The shell completes a file name with the files and directories
fileCompletion :: (HasCompleter f) => Mod f a
fileCompletion = completer (mkCompleter completeFile)
