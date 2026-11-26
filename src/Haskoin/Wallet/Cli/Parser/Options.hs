{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The options that commands of several modules of the command line take:
-- the network, the wallet and the portfolio, the tags, the paging and the
-- output of a listing, the fee and the files of a payment, and the answer
-- given in advance to a question.
--
-- An option is here once, with its help, so that it reads the same in
-- every command that takes it. A command whose help must say more gives
-- its own words to the option, as with 'networkOptionWith'. An option
-- that the commands of one module alone take is in that module.
module Haskoin.Wallet.Cli.Parser.Options
  ( readKind,
    networkMaybeOption,
    fileNetworkOption,
    namedAddressNetworkOption,
    networkOptionWith,
    networkOptionSaying,
    readNetwork,
    walletOptionWith,
    portfolioOptionWith,
    portfolioIndexOption,
    indexNamed,
    fileScopeOptions,
    typeOptionWith,
    listingTargetOption,
    everyNetwork,
    deleteYesOption,
    tagReader,
    tagOption,
    addressTagFilterOption,
    untaggedOption,
    txTagFilterOptionsOf,
    pagingOptions,
    orderFlags,
    afterOption,
    listingOutputOption,
    listingOutputHolding,
    viewFlags,
    underFee,
    dustCoinsOption,
    coinOptionFields,
    feeOption,
    recipientsPayOption,
    repeatAddressOption,
    outputFileMaybeOption,
    txLabelOption,
    txLabelOptionSaying,
    inputFileMaybeOption,
    coinReader,
    hashRefReader,
    yesOption,
    fileArgument,
    fileCompletion,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.List (intercalate, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Command (DustCoins (..), ListingOutput (..), Target (..), readCoinRef)
import Haskoin.Wallet.Cli.Completion (portfolioIndexes, shownNetworkNames, tagNames, walletIndexes)
import Haskoin.Wallet.Cli.Listing (ListingOrder (..), Paging (..))
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Completion (completeFile)
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Database.Tag (TagFilter (..), TxTagFilter (..))
import Haskoin.Wallet.Keys (AccountKind (..), indexFits, kindNamed, kindNames, portfolioNetworks)
import Haskoin.Wallet.ShortId (CoinRef (..), HashRef (..), readHashRef)
import Haskoin.Wallet.Tags (Tag, readTag)
import Haskoin.Wallet.Tx.CoinSelection (FeePayer (..))
import Haskoin.Wallet.Tx.FeeRates (FeeRateChoice, readFeeRateChoice)
import Haskoin.Wallet.Util (readNatural)
import Haskoin.Wallet.WalletFile (ImportScope (..))
import Numeric.Natural (Natural)

{- Networks -}

-- | The network, when given. Without it, a command that needs one takes the
-- default-network of the configuration file, or btc.
networkMaybeOption :: Args (Maybe Network)
networkMaybeOption = networkOptionWith "Without it, the default-network of the configuration file, or btc."

-- | The network of a transaction file, when given. A file hw wrote names
-- its network. A file of another wallet names none, and is read on this
-- network.
fileNetworkOption :: Args (Maybe Network)
fileNetworkOption =
  networkOptionWith "For a file that names no network, such as a file of another wallet. Without it, the default-network of the configuration file, or btc."

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
  networkOptionSaying ("The network. It is one of the networks hw shows: btc and bch, and those that setconfig --show-network added. " <> without)

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

-- | The names of every network of a portfolio, as the help of
-- --show-network lists them. It is the one place of the help that names
-- them all.
everyNetwork :: String
everyNetwork = intercalate ", " ((.name) <$> portfolioNetworks)

{- The options that say where a command works -}

-- | A kind of account as typed: bip44 or bip84
readKind :: Text -> Either String AccountKind
readKind typed = maybe (Left ("The type of an account is " <> intercalate " or " (cs <$> kindNames))) Right (kindNamed typed)

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

-- | The --type option, with the help of the command about it
typeOptionWith :: String -> Args (Maybe AccountKind)
typeOptionWith what =
  optional . option (eitherReader (readKind . cs)) $
    short 't'
      <> long "type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help ("The type of address: bip44 for legacy P2PKH addresses, or bip84 for native SegWit P2WPKH addresses, which pay lower fees. " <> what)

-- | Where a listing of the active portfolio looks: the network and the
-- type, each of which narrows the listing when it is given
listingTargetOption :: Args Target
listingTargetOption =
  Target
    <$> networkOptionWith "Only this network. Without it, every shown network of the portfolio."
    <*> typeOptionWith "Only the addresses of this type. Without it, both types."

{- Tags -}

-- | A tag typed on the command line, as its option or its argument reads
-- it: capital letters are taken as small ones, and a word that is no tag
-- is refused, with the rule it breaks ("Haskoin.Wallet.Tags".'readTag')
tagReader :: ReadM Tag
tagReader = eitherReader (readTag . cs)

-- | A tag after --tag, read as any other. A word with a space has a refusal
-- of its own, as the new name of renametag has (@newTagNameReader@). The
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

{- Listings -}

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
      "List the rows in the reverse order, the last one first. setconfig --reverse makes it the order of every listing on the screen."
      "List the rows in the usual order. Use it when setconfig --reverse, or the key listing-reverse of config.toml, reversed the listings."

-- | The --reverse and --no-reverse options, with the help of each: the
-- rows of a listing in the reverse order, from the last to the first in
-- each group, or in the usual one. They are the only options that change
-- the order of a listing ('ListingOrder'). A listing takes them for
-- itself, and setconfig for every listing on the screen, as the two
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
    compactHelp = "Show the " <> listed <> " in a few lines each. This view is the default, unless setconfig or the key listing-view of config.toml names another view."
    csvHelp = "Write the " <> listed <> " to this file, one on each row, as CSV for a spreadsheet, with " <> csvHolds <> ". The file must not exist yet."

-- | The view of a listing, from one of --compact, --verbose and
-- --tabular, with their help in that order. The listings read them, and
-- setconfig reads them for the view it makes the default, so that both
-- take the same words.
viewFlags :: String -> String -> String -> Args View
viewFlags compactHelp verboseHelp tabularHelp =
  flag' Compact (long "compact" <> help compactHelp)
    <|> flag' Verbose (short 'v' <> long "verbose" <> help verboseHelp)
    <|> flag' Table (long "tabular" <> help tabularHelp)

{- Payments -}

-- | The options of the fee of a payment or a sweep, under their
-- subheading of the help
underFee :: Args a -> Args a
underFee = optionGroup "Fee"

-- | What preparesendall, sendall and preparesweep do with the coins worth
-- less than their fee, without asking. The help says where the coins stay
-- when they are left: in the portfolio, or at the addresses swept.
dustCoinsOption :: String -> Args (Maybe DustCoins)
dustCoinsOption staying =
  optional $
    flag' SpendDust (long "spend-dust" <> help "Spend the dust coins too, without asking. A dust coin is worth no more than the fee to spend it.")
      <|> flag' LeaveDust (long "leave-dust" <> help ("Leave the dust coins " <> staying <> ", without asking"))

-- | --repeat-address, with the help given: whether a transaction may pay
-- an address in several outputs. The commands that build a transaction
-- from addresses the user types take it, each with the help of what it
-- reads: recipients, or the destinations of a sweep. It is a switch and
-- no question: a script cannot answer a question about one address, and
-- the user who wants several coins at an address knows it when typing
-- the command.
repeatAddressOption :: String -> Args Bool
repeatAddressOption what =
  switch $
    long "repeat-address"
      <> help what

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

-- | --label for a transaction, with what the help of the command says of
-- where the label goes
txLabelOptionSaying :: String -> Args (Maybe Text)
txLabelOptionSaying whereItGoes =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "LABEL"
      <> help ("A label for the transaction, to recognize it later. " <> whereItGoes)

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

{- Hashes and coins -}

-- | A coin as coins lists it, and as a block explorer often writes it: the
-- id of its transaction, in full or by its short id, a colon, and its
-- index among the outputs
coinReader :: ReadM CoinRef
coinReader = eitherReader $ \s -> maybe (Left (notACoin s)) Right (readCoinRef s)

-- | The refusal of a word that is not a coin
notACoin :: String -> String
notACoin s = "The coin " <> s <> " is not valid: write it TXID:INDEX, as coins lists it, the txid in full or by its short id"

-- | A hash as the user types it: in full, 64 hexadecimal digits, or by
-- its short id, its first 8 ("Haskoin.Wallet.ShortId")
hashRefReader :: ReadM HashRef
hashRefReader = eitherReader $ \s -> maybe (Left (notAHash s)) Right (readHashRef (cs s))
  where
    notAHash s = "The hash " <> s <> " is not valid: give its 64 digits, or its short id, its first 8 digits, as the listings show it"

{- Questions answered in advance -}

-- | The --yes of the commands that delete from the database for good:
-- deleteportfolio, deletewallet, deletetx, deletetag and deletecontact.
-- Each one says what it is about to delete and asks first, and --yes
-- answers in advance, for a script.
deleteYesOption :: Args Bool
deleteYesOption = yesOption "Delete without asking first"

-- | --yes, which answers yes to the question of a command without asking
-- it, for a script. The help says what the command then does.
yesOption :: String -> Args Bool
yesOption what =
  switch $
    short 'y'
      <> long "yes"
      <> help what

{- Files -}

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
