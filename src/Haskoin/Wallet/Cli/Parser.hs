{-# LANGUAGE DuplicateRecordFields #-}
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
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Version (showVersion)
import Data.Word (Word32)
import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit, readUnit)
import Haskoin.Wallet.Tx.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Cli.Completion
  ( accountNames,
    addressesToSpendFrom,
    coinsToLock,
    coinsToSpend,
    coinsToUnlock,
    hashesToSign,
    idsToBump,
    idsToSend,
    labelTargets,
    pendingHashes,
    shownTargets,
    walletAddresses,
  )
import Haskoin.Wallet.Database.Address (AddrType, readChainPath)
import Haskoin.Wallet.Tx.FeeRates (FeeRateChoice, readFeeRateChoice)
import Haskoin.Wallet.Keys (AccountKind (..), KeySource (..), kindNamed, kindNames)
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
  = CommandMnemonic !NewMnemonic
  | -- | The name of the new account, its network, unless the default one,
    -- and its kind
    CommandCreateAcc !Text !(Maybe Network) !AccountKind
  | CommandRecover !Recover
  | -- | The account whose mnemonic to check
    CommandCheckMnemonic !(Maybe Text)
  | -- | The network of a key the file places on none, unless the default
    -- one, the kind of a key the file says no kind of, unless BIP44, and
    -- the file of accounts and labels to import
    CommandImportAcc !(Maybe Network) !(Maybe AccountKind) !FilePath
  | -- | The account, or every one, and the file to write them to
    CommandExportAcc !(Maybe Text) !FilePath
  | CommandRenameAcc !RenameAcc
  | -- | The account, or every one, and how to show each, or the CSV file
    -- to write them to
    CommandAccounts !(Maybe Text) !ListingOutput
  | -- | The name of the account, with its network and its path to confirm
    -- the deletion
    CommandDeleteAcc !Text !Network !HardPath
  | CommandReceive !Receive
  | -- | The account --account names, what to show, or Nothing for the
    -- newest receiving address of the account, and the network of a
    -- transaction file that names none and involves no account
    CommandShow !(Maybe Text) !(Maybe Shown) !(Maybe Network)
  | -- | The account, whether its change addresses, how to show each
    -- address or the CSV file to write them to, and the page
    CommandAddresses !(Maybe Text) !Bool !ListingOutput !(Paging Text)
  | -- | The account, how to show each address or the CSV file to write
    -- them to, and the page
    CommandBalances !(Maybe Text) !ListingOutput !(Paging Text)
  | CommandLabel !Label
  | -- | The account, how to show each transaction or the CSV file to
    -- write them to, and the page
    CommandTxs !(Maybe Text) !ListingOutput !(Paging HashRef)
  | CommandPrepareTx !PrepareTx
  | CommandPrepareSendAll !PrepareSendAll
  | -- | The account, or every one, how to show each transaction or the
    -- CSV file to write them to, and the page
    CommandPendingTxs !(Maybe Text) !ListingOutput !(Paging HashRef)
  | -- | The account, and the transaction file
    CommandImportTx !(Maybe Text) !FilePath
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
  | -- | The account, whether the locked coins only, how to show each
    -- coin or the CSV file to write them to, and the page
    CommandCoins !(Maybe Text) !Bool !ListingOutput !(Paging CoinRef)
  | CommandLockCoin !CoinRef
  | CommandUnlockCoin !CoinRef
  | -- | A pending transaction, by its txid or its nosigHash, in full or by
    -- its short id, or the file of a signed one, whether to send it without asking, and the network
    -- of a file that names none and that the wallet does not hold
    CommandBroadcast !(Maybe HashRef) !(Maybe FilePath) !Bool !(Maybe Network)
  | CommandBumpFee !BumpFee
  | -- | The account, or every one, and whether to sync from the start
    CommandSync !(Maybe Text) !Bool
  | -- | The file of the copy, or none for the backups directory of hw
    CommandBackup !(Maybe FilePath)
  | -- | The account, or every one, and the spreadsheet to write their
    -- listings to
    CommandExportSheet !(Maybe Text) !FilePath
  | CommandPrepareSweep !PrepareSweep
  | -- | What to sign, the file of the private keys, typed on the terminal
    -- without one, and the network of a file that names none and involves
    -- no account
    CommandSignSweep !Signing !(Maybe KeySource) !(Maybe Network)
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
listingOutputOf (CommandAccounts _ output) = Just output
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

-- | How the command shows each element it lists, as asked of accounts,
-- addresses, balances, txs, pendingtxs and coins, or else in the listing
-- view given, the key listing-view of the configuration. The other
-- commands have one view, which the key does not change: it is about
-- listings. A listing written to a CSV file answers with the file, in the
-- compact view.
viewOf :: View -> Command -> View
viewOf listingView cmd
  | Just (OnScreen asked) <- listingOutputOf cmd = fromMaybe listingView asked
-- The one coin they answer with, with every detail, as the other commands
-- that answer with one element show it
viewOf _ (CommandLockCoin _) = Verbose
viewOf _ (CommandUnlockCoin _) = Verbose
viewOf _ (CommandShow {}) = Verbose
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
-- or by its id and the file it was written to, if any, for one just
-- signed, which signtx and signsweep showed in full before they asked for
-- the secret
data TxShown = FullTx | SignedTx !(Maybe FilePath)
  deriving (Eq, Show)

-- | The file a signing command writes is its --output file, or the --input
-- file it replaces with the user's agreement
txShownOf :: Command -> TxShown
txShownOf (CommandSignTx signing) = SignedTx (signing.output <|> signing.input)
txShownOf (CommandSignSweep signing _ _) = SignedTx (signing.output <|> signing.input)
txShownOf _ = FullTx

-- | Whether the pager wraps the lines of the listing the command shows in
-- the view given, rather than cut them at the edge of the screen
-- ("Haskoin.Wallet.Cli.Pager"). Only accounts in the verbose view wraps.
-- Its rows end with the output descriptor of each account, wider than
-- most screens, which the user copies into another wallet. A cut
-- descriptor could not be copied from the screen. Every other view keeps
-- its columns whole by cutting its lines.
wrapsLines :: View -> Command -> Bool
wrapsLines Verbose (CommandAccounts _ _) = True
wrapsLines _ _ = False

-- | The options of mnemonic
data NewMnemonic = NewMnemonic
  { -- | The bytes of entropy
    entropy :: !Natural,
    -- | Whether dice rolls are mixed in
    dice :: !Bool,
    -- | The shares of a backup to make in place of a mnemonic
    shares :: !(Maybe Natural),
    -- | The number of shares that rebuild the seed
    threshold :: !(Maybe Natural)
  }
  deriving (Eq, Show)

-- | The options of recover
data Recover = Recover
  { -- | The one network to search, when given. Without it, recover
    -- searches Bitcoin and Bitcoin Cash, whatever the default network.
    network :: !(Maybe Network),
    -- | The accounts in a row without a payment that end the search
    accountGap :: !Natural
  }
  deriving (Eq, Show)

-- | The options of renameacc
data RenameAcc = RenameAcc
  { old :: !Text,
    new :: !Text
  }
  deriving (Eq, Show)

-- | The options of receive
data Receive = Receive
  { account :: !(Maybe Text),
    label :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | The options of label
data Label = Label
  { -- | What to label. It names its account too.
    labelled :: !Labelled,
    -- | The new label. An empty one removes the label.
    label :: !Text
  }
  deriving (Eq, Show)

-- | What label labels: a transaction, by its txid, or while it is pending
-- by either of its hashes, in full or by its short id; or a receiving
-- address, as typed
data Labelled
  = LabelledTx !HashRef
  | LabelledAddress !Text
  deriving (Eq, Show)

-- | What a word typed for label names. A hash has 64 hexadecimal digits,
-- and its short id 8, which no address has, so any other word is an
-- address.
readLabelled :: String -> Labelled
readLabelled word = maybe (LabelledAddress (cs word)) LabelledTx (readHashRef (cs word))

-- | What show shows, as the word typed names it. One command takes the
-- five, so that the user need not know which command shows what. The word
-- tells. The shapes of the five do not overlap, so a word is never taken
-- for one kind when it was typed as another ('readShown').
data Shown
  = -- | The address at a path of a chain of the account, /0/i or /1/i
    ShownPath !AddrType !KeyIndex
  | -- | An address of the wallet, as typed
    ShownAddress !Text
  | -- | A transaction, pending or on the chain, by its txid or, while it
    -- is pending, its nosigHash, in full or by its short id
    ShownTx !HashRef
  | -- | A coin, TXID:INDEX, the txid in full or by its short id
    ShownCoin !CoinRef
  | -- | A transaction file, which the wallet does not keep
    ShownFile !FilePath
  deriving (Eq, Show)

-- | What a word typed for show names, by its shape. The four kinds of the
-- wallet have strict shapes, and any other word names a file.
--
-- * A path is made of digits and slashes, such as /0/3. A word of those,
--   perhaps with the m, h and ' of a BIP32 path, is a path or is refused
--   as one. So 0/3 and m/0/3 are told how to write a path.
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
  | isPathShaped = uncurry ShownPath <$> readChainPath (cs word)
  | Just coin <- readCoinRef word = Right (ShownCoin coin)
  | Just hash <- readHashRef (cs word) = Right (ShownTx hash)
  | any (\net -> isJust (textToAddr net (cs word))) allNets = Right (ShownAddress (cs word))
  | otherwise = Right (ShownFile word)
  where
    isPathShaped = '/' `elem` word && all (`elem` ("0123456789/mMhH'" :: String)) word

-- | The options of preparetx
data PrepareTx = PrepareTx
  { -- | The addresses paid, each with its amount, as typed: ADDRESS:AMOUNT
    recipients :: ![Text],
    account :: !(Maybe Text),
    -- | The fee rate: a number of satoshis per virtual byte or a level of the
    -- rates recent blocks paid, or none to ask the user from those rates
    feeRate :: !(Maybe FeeRateChoice),
    feePayer :: !FeePayer,
    -- | The confirmations a coin needs to be spent
    minConf :: !Natural,
    strategy :: !CoinSelection,
    output :: !(Maybe FilePath),
    -- | The label of the transaction (--label)
    label :: !(Maybe Text),
    -- | Coin control: the coins chosen by their outpoints, in full or by
    -- their short ids (--coin)
    coins :: ![CoinRef],
    -- | Coin control: the addresses whose coins are chosen (--spend-from)
    spendFrom :: ![Text],
    -- | Whether the strategy selects among the chosen coins (--select),
    -- instead of every one of them being spent
    select :: !Bool,
    -- | Whether a fee rate above 100 satoshis per virtual byte is taken without
    -- asking (--yes)
    yes :: !Bool,
    -- | Whether the change goes back to an address the payment spends
    -- from (--change-back) or to a new change address (--new-change), when
    -- the command says. Otherwise the configuration file says.
    changeBack :: !(Maybe Bool)
  }
  deriving (Eq, Show)

-- | The options of bumpfee
data BumpFee = BumpFee
  { -- | The transaction to replace, by its id, in full or by its short id
    bumped :: !HashRef,
    account :: !(Maybe Text),
    -- | The fee rate of the replacement, as for preparetx
    feeRate :: !(Maybe FeeRateChoice),
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
    account :: !(Maybe Text),
    -- | The fee rate: a number of satoshis per virtual byte or a level of the
    -- rates recent blocks paid, or none to ask the user from those rates
    feeRate :: !(Maybe FeeRateChoice),
    -- | The confirmations a coin needs to be sent
    minConf :: !Natural,
    output :: !(Maybe FilePath),
    -- | The label of the transaction (--label)
    label :: !(Maybe Text),
    -- | The coins to send, instead of all of them, in full or by their
    -- short ids
    coins :: ![CoinRef],
    -- | The addresses whose coins to send, instead of all of them
    spendFrom :: ![Text],
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
  { -- | The address of the wallet that signs
    address :: !Text,
    message :: !Text,
    -- | The file the armored block is written to, instead of the screen
    output :: !(Maybe FilePath)
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
    -- | The network the address is read on, when given
    network :: !(Maybe Network)
  }
  deriving (Eq, Show)

-- | The options of preparesweep
data PrepareSweep = PrepareSweep
  { account :: !(Maybe Text),
    -- | The network of a sweep that pays no account of the wallet
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
-- --output file
data Signing = Signing
  { account :: !(Maybe Text),
    nosigHash :: !(Maybe HashRef),
    input :: !(Maybe FilePath),
    output :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- | What signtx and signsweep sign
signingOptions :: Args Signing
signingOptions =
  Signing
    <$> txAccountOption
    <*> optional nosigHashArg
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
parserMain = runProgram program

-- | hw: its commands by group, the options every command takes, and the
-- help of the program
program :: Program Command GlobalOptions
program =
  Program
    { name = "hw",
      -- The database has the format of this hw: every command migrates it
      -- first, and refuses one of a newer hw
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
      everydayNames = ["mnemonic", "recover", "accounts", "sync", "receive", "show", "addresses", "balances", "label", "coins", "fees", "backup"],
      details =
        [r|
A payment takes three steps:

  hw preparetx ADDRESS:AMOUNT    prepare it, without any key
  hw signtx NOSIGHASH            sign it with the mnemonic
  hw broadcast TXID              send it to the network

preparetx shows the nosigHash of the new transaction. signtx shows its txid,
the id the network knows it by.

A command that takes a hash, or a coin TXID:INDEX, also takes its short id:
the first 8 digits of the hash, as the listings show it, such as a1075db5, or
a1075db5:1 for a coin. When two transactions of the wallet share those 8
digits, hw names both and asks for the full hash.

To sign on an offline computer, create the account there with createacc. Copy
it to the online computer with exportacc and importacc. Then carry each
transaction between them with preparetx --output, signtx --input and
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
the listings, the address gap and the pager, are in ~/.hw/config.json.

The Tab key completes the commands, the options, and what the wallet holds:
account names, the hashes of pending transactions, coins and addresses. The
shell needs a script for it, which hw writes. In bash, add this line to
~/.bashrc:

  source <(hw --bash-completion-script hw)

The README explains it for zsh and fish.

Type hw COMMAND --help for the help of a command.
|],
      globals = GlobalOptions <$> unitOption <*> jsonOption <*> noPagerOption,
      groups =
        [ CommandGroup
            "Mnemonics and accounts"
            [ mnemonicCommand,
              createAccCommand,
              recoverCommand,
              checkMnemonicCommand,
              exportAccCommand,
              importAccCommand,
              accountsCommand,
              renameAccCommand,
              deleteAccCommand,
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
config.json, or btc.
|]

{- mnemonic -}

mnemonicCommand :: CommandSpec Command
mnemonicCommand =
  CommandSpec
    { name = "mnemonic",
      offline = True,
      summary = "Generate a new mnemonic, or a backup in shares",
      details =
        [r|
hw prints the mnemonic and stores nothing. Write it down and keep copies in
safe places, away from computers and phones. Whoever has the mnemonic can
spend the funds. If you lose it, the funds are lost.

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
backup, as a mnemonic is.

A backup in shares is not a BIP39 mnemonic, and no mnemonic comes of it. A
wallet that reads SLIP-39 shares restores it, and one that reads only BIP39
mnemonics cannot.
|],
      args =
        CommandMnemonic
          <$> (NewMnemonic <$> entropyOption <*> diceOption <*> sharesOption <*> thresholdOption)
    }

entropyOption :: Args Natural
entropyOption =
  option (maybeReader f) $
    short 'e'
      <> long "entropy"
      <> metavar "BYTES"
      <> value 16
      <> showDefault
      <> completeWith valid
      <> help
        [r|
The bytes of entropy: 16, 20, 24, 28 or 32, for a mnemonic of 12, 15, 18, 21
or 24 words. A backup in shares takes 16 or 32, for shares of 20 or 33 words.
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

-- | The shares of a backup that mnemonic makes in place of a mnemonic
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

-- | A number of shares, from 1 to 16: the format has four bits for it
upToSixteen :: String -> Text -> Either String Natural
upToSixteen name typed =
  case readNatural typed of
    Just n
      | n >= 1 && n <= 16 -> Right n
      | otherwise -> Left ("The " <> name <> " value has to be between 1 and 16")
    Nothing -> Left ("Could not parse the " <> name <> " option")

{- createacc -}

createAccCommand :: CommandSpec Command
createAccCommand =
  CommandSpec
    { name = "createacc",
      offline = True,
      summary = "Create an account from a mnemonic",
      details =
        [r|
createacc asks for the mnemonic and the passphrase, each twice to catch a
typo. It takes the shares of a backup in place of the mnemonic: type one,
and hw asks for the next until it has enough. Shares are typed once, since
their checksums catch a typo, and their passphrase has printable ASCII
characters only, as their format asks.

It derives the account key m/44'/coin'/index', or m/84'/coin'/index'
with --type bip84, and stores only its public key. It takes the first index
of the seed that has no account of that kind in the wallet on that network.
For example, the first Bitcoin account is m/44'/0'/0', and the first native
SegWit one is m/84'/0'/0'.

A bip44 account has legacy P2PKH addresses, which start with 1 on Bitcoin,
and is the default. A bip84 account has native SegWit P2WPKH addresses,
which start with bc1q, and its payments pay about 40% less in fees. Other
wallets create bip84 accounts by default. Bitcoin Cash has no SegWit, so it
has bip44 accounts only.

The private keys are never stored. To keep them on an offline computer,
create the account there. Then copy it to an online computer with exportacc
and importacc.

createacc answers with every detail of the new account, as accounts --verbose
shows it, its output descriptor last. Another wallet takes the account by its
descriptor, to watch it.

A passphrase with accents or other non-ASCII letters is normalized as BIP39
asks, so that other wallets find the same seed. Older versions of hw did not
normalize it. If the wallet has accounts of this mnemonic made that way, and
none of the normalized seed, createacc keeps to the old seed and says so.
|],
      args =
        CommandCreateAcc
          <$> textArg "NAME" "The name of the new account"
          <*> networkMaybeOption
          <*> accountTypeOption
    }

-- | The kind of the new account: bip44 unless given
accountTypeOption :: Args AccountKind
accountTypeOption =
  option (eitherReader (readKind . cs)) $
    short 't'
      <> long "type"
      <> metavar "TYPE"
      <> value Bip44
      <> showDefaultWith (const "bip44")
      <> completeWith (cs <$> kindNames)
      <> help "The kind of account: bip44, with legacy P2PKH addresses, or bip84, with native SegWit P2WPKH addresses, which pay lower fees"

-- | The kind of an account the file says no kind of, when given
importTypeOption :: Args (Maybe AccountKind)
importTypeOption =
  optional . option (eitherReader (readKind . cs)) $
    short 't'
      <> long "type"
      <> metavar "TYPE"
      <> completeWith (cs <$> kindNames)
      <> help "The kind of account of a key the file says no kind of, a bare xpub or tpub: bip44 or bip84. Without it, bip44. A descriptor, a zpub and a file of hw say their kind themselves, and a zpub with --type bip44 is refused."

-- | A kind of account as typed: bip44 or bip84
readKind :: Text -> Either String AccountKind
readKind typed = maybe (Left ("The type of an account is " <> intercalate " or " (cs <$> kindNames))) Right (kindNamed typed)

-- | The network, when given. Without it, a command that needs one takes the
-- default-network of the configuration file.
networkMaybeOption :: Args (Maybe Network)
networkMaybeOption = networkOptionWith "Without it, the default-network of the configuration file."

-- | The one network recover searches, when given. Without it, recover
-- searches Bitcoin and Bitcoin Cash: the default-network of the
-- configuration file does not apply, since a seed is often paid on both.
recoverNetworkOption :: Args (Maybe Network)
recoverNetworkOption =
  networkOptionWith "Only it is searched. Without it, btc and bch are searched, whatever the default-network of the configuration file."

-- | The network of a sweep, when given. A sweep that pays an account of the
-- wallet is on the network of that account. One that pays none needs it.
sweepNetworkOption :: Args (Maybe Network)
sweepNetworkOption =
  networkOptionWith "Needed by a sweep that pays no account of this wallet. The default-network of the configuration file does not apply to it."

-- | The network of a transaction file, when given. A file hw wrote names
-- its network, and a file that involves an account of the wallet is on
-- the network of that account. A file of another wallet that involves
-- none needs it.
fileNetworkOption :: Args (Maybe Network)
fileNetworkOption =
  networkOptionWith "For a file that names no network and involves no account of this wallet, as a file of another wallet does. Without it, the default-network of the configuration file."

-- | The network, when given, with the help of the command about it
networkOptionWith :: String -> Args (Maybe Network)
networkOptionWith without =
  optional . option (eitherReader (f . netByName)) $
    short 'n'
      <> long "network"
      <> metavar "NETWORK"
      <> completeWith ((.name) <$> allNets)
      <> help ("The network: " <> nets <> ". " <> without)
  where
    nets = intercalate ", " ((.name) <$> allNets)
    f :: Maybe Network -> Either String Network
    f Nothing =
      Left $ "Invalid network name. Select one of the following: " <> nets
    f (Just res) = Right res

{- recover -}

recoverCommand :: CommandSpec Command
recoverCommand =
  CommandSpec
    { name = "recover",
      offline = False,
      summary = "Find the accounts of a mnemonic that were paid",
      details =
        [r|
`recover` needs the mnemonic and the server at the same time, so it runs on an
online computer and not on an offline one. A computer that was tampered with
can steal a mnemonic typed on it. To keep the mnemonic offline, create the
accounts on the offline computer with createacc instead. Copy them with
exportacc and importacc, and run `sync`.

`recover` asks for the mnemonic, or for the shares of a backup, then searches
for the accounts of its seed that were paid. It checks the accounts in order
from index 0, and asks the server about the receiving addresses of each. Each
account that was paid is added, named after the network and its index, such
as btc0 or bch0, and synced. Accounts the wallet has already are skipped.

Without --network, `recover` searches Bitcoin, then Bitcoin Cash. The
default-network of the configuration file does not apply. A seed is often paid
on both, as every seed from before the fork of 2017 is. A test network is
searched only when --network names it. When the server does not answer for
one of the two, the other is still searched. The answer then names the one
left out, and hw exits with an error, as `sync` does.

On Bitcoin, the bip44 accounts of the seed are searched first, then the
bip84 ones, which most other wallets make: a bip84 account found is named
with -segwit after the index, such as btc0-segwit. Bitcoin Cash has bip44
accounts only.

`recover` also looks where other wallets keep their accounts, and tells of
funds it cannot spend. On Bitcoin, those are the bip49 accounts of wrapped
SegWit, m/49', which hardware wallets made before native SegWit, and the
bip86 accounts of Taproot, m/86'. On Bitcoin Cash, those are the bip44
accounts of Bitcoin, m/44'/0', where a wallet that held Bitcoin at the fork
of 2017 has its Bitcoin Cash. No such account is added. Move its funds with
a wallet that holds such accounts, to an address of this wallet.

The search stops after --account-gap accounts in a row without a payment,
counted for each kind of account apart. BIP44 stops at the first such
account. hw looks further, since accounts created on an offline computer
may never have been paid. On a terminal, a line tells which network and
account `recover` is looking at, as `sync` does.

After every 20 accounts of a kind the server reports paid, `recover` asks
whether to keep looking. A server that lies reports them without end. A no
stops the search, and no account is added.

hw normalizes a passphrase as BIP39 asks, and older versions did not. When
nothing is found, `recover` also tries the passphrase as typed. It says so if
that finds accounts.
|],
      args =
        CommandRecover
          <$> (Recover <$> recoverNetworkOption <*> accountGapOption)
    }

accountGapOption :: Args Natural
accountGapOption =
  option (eitherReader accountGap) $
    long "account-gap"
      <> metavar "INT"
      <> value 10
      <> showDefault
      <> help
        [r|
How many accounts in a row without a payment end the search, from 1 to 100.
BIP44 uses 1. A larger gap finds accounts created on a signer that were never
paid.
|]
  where
    accountGap :: String -> Either String Natural
    accountGap arg =
      case readNatural (cs arg) of
        Just n | n >= 1 && n <= maxAccountGap -> Right n
        _ -> Left $ "The account gap is a number of accounts, from 1 to " <> show maxAccountGap
    -- Every account searched is a scan of its receiving chain at the store.
    -- A larger gap keeps recover asking about accounts nobody created: 100
    -- is ten times the default.
    maxAccountGap :: Natural
    maxAccountGap = 100

{- deleteacc -}

deleteAccCommand :: CommandSpec Command
deleteAccCommand =
  CommandSpec
    { name = "deleteacc",
      offline = False,
      summary = "Delete an account from the wallet",
      details =
        [r|
deleteacc removes the account with its addresses, labels, transactions, coins
and pending transactions. The NETWORK and the DERIVPATH must be those of the
account, as `accounts` shows them. They confirm which account to delete.

The funds stay on the chain, and the mnemonic can still spend them. The labels
are lost.
|],
      args =
        CommandDeleteAcc
          <$> accountArg "NAME" "The name of the account to delete"
          <*> networkArg
          <*> fullDerivationOption
    }

fullDerivationOption :: Args HardPath
fullDerivationOption =
  argument (maybeReader parseHard) $
    metavar "DERIVPATH"
      <> help "The derivation path of the account, as `accounts` shows it. Quote it for the shell, as in \"/44'/0'/3'\"."

networkArg :: Args Network
networkArg =
  argument (eitherReader (f . netByName)) $
    metavar "NETWORK"
      <> completeWith ((.name) <$> allNets)
      <> help ("The network of the account: " <> nets)
  where
    nets = intercalate ", " ((.name) <$> allNets)
    f :: Maybe Network -> Either String Network
    f Nothing =
      Left $ "Invalid network name. Select one of the following: " <> nets
    f (Just res) = Right res

{- checkmnemonic -}

checkMnemonicCommand :: CommandSpec Command
checkMnemonicCommand =
  CommandSpec
    { name = "checkmnemonic",
      offline = True,
      summary = "Check a mnemonic and passphrase against an account",
      details =
        [r|
checkmnemonic asks for the mnemonic and the passphrase, and checks that they
give the public key of the account. Use it to test a mnemonic you wrote down,
or the shares of a backup, before you rely on it. Nothing is signed. When they match, hw records the
fingerprint of the seed, which `accounts` shows.

When they do not match, hw looks for the key of the account at the other
paths it knows. An account imported from its key alone was given its kind
and its network by you, and the key may be of another kind, such as bip84,
or of another network. checkmnemonic then says so, with the --type or the
--network to import the key again with, after deleteacc.
|],
      args =
        CommandCheckMnemonic
          <$> accountOption
    }

-- | The account of a command, which the wallet's only account is without it
accountOption :: Args (Maybe Text)
accountOption = accountOptionWith "The account to use. Needed only when the wallet has more than one."

-- | The account of a command that acts on every account without it
onlyAccountOption :: Args (Maybe Text)
onlyAccountOption = accountOptionWith "Only this account. Without it, every account."

-- | The account of a transaction, which hw finds in the transaction when
-- it can
txAccountOption :: Args (Maybe Text)
txAccountOption = accountOptionWith "The account of the transaction. Needed only when hw cannot tell it from the transaction."

-- | The --account option, with the help of the command about it
accountOptionWith :: String -> Args (Maybe Text)
accountOptionWith what =
  optional . strOption $
    short 'a'
      <> long "account"
      <> metavar "NAME"
      <> completer (listIOCompleter accountNames)
      <> help what

{- importacc -}

importAccCommand :: CommandSpec Command
importAccCommand =
  CommandSpec
    { name = "importacc",
      offline = False,
      summary = "Add the accounts and labels of a file",
      details = importExportAccDetails,
      args =
        CommandImportAcc
          <$> importNetworkOption
          <*> importTypeOption
          <*> fileArgument "The file exportacc wrote, or a file of another wallet: BIP 329 labels, an output descriptor on a line, the file of Bitcoin Core's listdescriptors, or the generic JSON of a Coldcard"
    }

-- | The network of the accounts a file of another wallet creates, which
-- names none
importNetworkOption :: Args (Maybe Network)
importNetworkOption =
  networkOptionWith "For an account the file names no network for, as a file of another wallet does. Without it, the default-network of the configuration file."

importExportAccDetails :: String
importExportAccDetails =
  [r|
exportacc writes the accounts to a file, with their labels, in the format of
BIP 329, which other wallets read and write. An account goes in as its public
key M/44'/coin'/index', or M/84'/coin'/index' for a bip84 account, its name,
its network, its kind and, when hw knows it, the fingerprint of its seed.
Then come the labels of its receiving addresses and of its transactions, and
the coins locked with lockcoin. The file holds no private key. Without
--account, every account goes in the file.

importacc reads such a file, from hw or from another wallet. An account the
wallet does not have is added, named after the label of its key. The wallet
can then watch it and prepare its payments without the mnemonic: this is how
an online computer works with an offline signer. An account the wallet has
learns the fingerprint of its seed, and takes the name a file of hw gives it.
`show` FILE shows what importacc would make of a file, and changes nothing.

importacc also reads the output descriptors other wallets hand an account
over by: a descriptor on a line, such as pkh([bc560f95/44h/0h/0h]xpub.../<0;1>/*)
for a bip44 account or wpkh([bc560f95/84h/0h/0h]xpub.../<0;1>/*) for a bip84
one, a key alone, the file of Bitcoin Core's listdescriptors, or the generic
JSON of a Coldcard. hw holds bip44
and bip84 accounts, so a descriptor of another kind, such as sh(wpkh(...)),
is refused, and the import goes on. A key alone says its kind when it is a
zpub or a vpub, which some wallets write for a bip84 account; an xpub or a
tpub alone is taken for a bip44 account, or for a bip84 one with --type
bip84. A zpub with --type bip44 is refused, as the two contradict. An xpub
alone says nothing of its path, so the answer notes that its kind was taken
on trust, and what tells when it was wrong: a sync that finds none of its
transactions. deleteacc, then import it again with the other --type. A
descriptor names no network: give --network, or set a default
network. An account a descriptor adds is named after its network, its index
and its kind, as recover names one, since a descriptor carries no name:
renameacc renames it. accounts --verbose shows the descriptor of each account
of this wallet.

The labels and the locks of the file are set on the addresses, transactions
and coins the wallet has. An address the wallet has not generated yet is
derived from the path the file gives, up to a thousand past the last used
address of its chain, and a transaction the file places in an account is
labelled before `sync` fetches it. What hw cannot place is counted in the
answer: run `sync`, then importacc again. The labels of inputs, of coins and
of change addresses are left aside, since hw keeps none.

The fingerprint is the four bytes that PSBTs and output descriptors write
before a key of the seed. Each record of the file names its account by it, as
in pkh([bc560f95/44'/0'/0']) or wpkh([bc560f95/84'/0'/0']). `accounts`
groups the accounts of one seed by it.
|]

{- exportacc -}

exportAccCommand :: CommandSpec Command
exportAccCommand =
  CommandSpec
    { name = "exportacc",
      offline = False,
      summary = "Write the accounts and their labels to a file",
      details = importExportAccDetails,
      args =
        CommandExportAcc
          <$> onlyAccountOption
          <*> fileArgument "The file to write. It must not exist yet."
    }

{- renameacc -}

renameAccCommand :: CommandSpec Command
renameAccCommand =
  CommandSpec
    { name = "renameacc",
      offline = False,
      summary = "Rename an account",
      details = "",
      args =
        CommandRenameAcc
          <$> (RenameAcc <$> accountArg "OLD" "The current name of the account" <*> textArg "NEW" "The new name")
    }

-- | The name of an account, as an argument
accountArg :: String -> String -> Args Text
accountArg name desc =
  argument str $
    metavar name
      <> completer (listIOCompleter accountNames)
      <> help desc

{- accounts -}

accountsCommand :: CommandSpec Command
accountsCommand =
  CommandSpec
    { name = "accounts",
      offline = False,
      summary = "Show the accounts, their balances and descriptors",
      details =
        [r|
Each account takes two lines. The first gives its name, its network and its
derivation path, as deleteacc asks for them. The second gives what it can
spend, what is unconfirmed, and how many coins it holds. The accounts of one
seed come together.

The spendable balance is what the account can spend now. It leaves out the
coins locked by a pending transaction or by lockcoin, the coinbase coins with
fewer than 100 confirmations, and the unconfirmed payments from other wallets.
The unconfirmed coins of the wallet's own transactions count: its change, and
what one of its accounts pays another. preparetx can spend all of it, unless
--min-conf asks for confirmations.

--verbose adds the fingerprint of the seed when known, how many receiving and
change addresses the account has, when it was created, and the confirmed and
unconfirmed balances the server gave at the last sync. It ends with the
account as an output descriptor, the format of BIP 380 in which wallets take
an account without its private key:

  pkh([bc560f95/44h/0h/0h]xpub6CHK45s.../<0;1>/*)#vq597j87
  wpkh([bc560f95/84h/0h/0h]xpub6CQF8An.../<0;1>/*)#5nktextl

The first is a bip44 account, the second a bip84 one. Paste it into a wallet
that takes a descriptor, to watch the account there. The descriptor carries
the public key of the account, the fingerprint of its seed and its derivation
path, and the two chains of addresses, with a checksum. It holds no private
key. When hw does not know the fingerprint of the seed, the descriptor names
no origin, which a hardware wallet needs to match it. importacc reads a
descriptor back.

A descriptor does not name its network. Bitcoin and Bitcoin Cash write the
same keys, and every test network writes tpub keys.

A bip84 account also shows its key in the form of SLIP-132, a zpub, or a vpub
on a test network, in the row SLIP-132. Some wallets take only that form for a
native SegWit account, and make a legacy wallet of an xpub. The key is the
same, only its prefix differs. A bip44 account has no such row, since such
wallets take its xpub as it is.

A descriptor is wider than most screens. On a terminal, the pager wraps the
lines of --verbose rather than cut them at the edge of the screen, so that a
descriptor can be copied whole.

With --tabular, each account takes one row of a table, under a header that
names the columns: its name, its network, its derivation path, what it can
spend, what is unconfirmed, and how many coins it holds. The amounts are in the
unit the header names. The first column is as wide as the longest name.

With --csv FILENAME, each account takes one row of a new CSV file, for a
spreadsheet: its name, network and path, what it can spend, its confirmed and
unconfirmed balances, its coins, how many receiving and change addresses it
has, and when it was created. Amounts are in satoshis, and times in UTC. The
file holds no key and no descriptor: anyone it reaches could see every
address and every payment of the account with them.
|],
      args =
        CommandAccounts
          <$> onlyAccountOption
          <*> listingOutputOption
            "Show every detail of each account: the fingerprint of its seed, its addresses, when it was created, each of its balances, and its output descriptor."
            "Show each account on one row of a table: its name, network and path, what it can spend, what is unconfirmed, and its coins."
            "accounts"
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
Each call gives the next receiving address of the account, at the path
M/44'/coin'/index'/0/i. The wallet pays the change of its transactions to other
addresses, at M/44'/coin'/index'/1/i. `addresses` --change lists them.

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
      args = CommandReceive <$> (Receive <$> accountOption <*> labelOption)
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
      summary = "Show an address, a transaction or a coin, or a file of a transaction or of accounts",
      details =
        [r|
`show` shows one address, transaction or coin of the wallet, or a file of a
transaction or of accounts, and changes nothing. The word given tells what it
is:

  /0/i or /1/i   the receiving address i, or the change address i
  ADDRESS        an address of the wallet
  HASH           a transaction, by its txid, or its nosigHash while pending
  TXID:INDEX     a coin
  FILENAME       a file, any other word

A hash goes in full or by its short id, its first 8 digits. A file whose name
looks like one of the others goes with its directory, such as ./a1075db5. With
no word, `show` shows the newest receiving address of the account, the first
one `addresses` lists. It hands out no new address.

An address the wallet holds is shown as `receive` shows a new one: its path,
its label and what it received, and on a terminal its QR code under them. Use
it to give an address out again. Any address of the wallet will do, a change
address too. An address the wallet does not hold is refused, so the code never
pays anyone else.

A path is derived from the public key of the account. It needs no mnemonic and
nothing of the server, so it runs on an offline computer as well as online.
Use it on the offline computer to check an address the online computer gave
out. An online computer that was tampered with can show an address of its own
in place of one of the account, and the payer would pay the attacker. The
offline computer holds the same account, since createacc made it there, and
derives the same addresses. Run `receive` online, then `show` on the offline
computer with the path `receive` showed, and compare the two addresses.

The address at a path the wallet holds is shown as any address of the wallet.
One the wallet does not hold, as on the offline computer, is shown cut: its two
ends, with a dot for each character of the middle, and no QR code. Compare the
ends with the address the online computer shows. They are enough, since nobody
can make another address with the same ends: it would take about 2^128 tries.
The middle is hidden so that nobody pays the address. `receive` did not give it
out, and it may lie past the gap that a restored wallet looks at. Look at the
path too. A tampered online computer picks the path it shows, and a path far
past the addresses you gave out is a sign of that.

A transaction is shown in full, as each account that holds it sees it: a
payment between two accounts of the wallet once for each. A coin is shown with
every detail, as `coins` --verbose shows it.

A transaction file is shown as its account sees it, with the name of that
account. The wallet keeps nothing of it. The file comes from preparetx
--output, exporttx, signtx or preparesweep: a PSBT (BIP 174). Use it to check
a transaction before you sign it, or before you send it. A transaction that
involves no account, such as a sweep to addresses outside the wallet, is shown
on the network its file names.

A file of another wallet is shown the same way: a PSBT in bytes, or in base64
text, of version 0 or of version 2, that of BIP 370. Such a file names no
network. The account it involves tells it. One that involves no account needs
--network, or the default-network of the configuration file.

The labels shown for a file, the transaction's, those of its addresses and
those of the coins it spends, are the ones the file carries, as the wallet that
wrote it had them. They are not those of this wallet, which an offline computer
does not have.

A file that is neither a PSBT nor the hexadecimal of a transaction is read as
a file of accounts, the file exportacc writes and importacc reads: BIP 329
labels or output descriptors, from hw or from another wallet. `show` shows what
importacc would make of it. Each account comes as
`accounts` lists it, with whether the wallet holds it, or why importacc would
refuse it. Under it come the labels of its addresses and transactions, and the
coins it locks or unlocks. A record goes under the account its origin names. In
a file of one account, a record without an origin goes under that account too.
The labels of no account of the file come last, then what hw keeps no label
for. An account the file names no network for is taken on --network, or the
default-network of the configuration file, and a key that says no kind is
taken for a bip44 account, as importacc takes them.

--account names the account of a path and of the newest address. With a
transaction or a transaction file, it keeps the view of that account. An
address, a coin and a file of accounts tell their accounts, so `show` refuses
--account with them. --network goes with a file only.
|],
      args =
        (\shown account network -> CommandShow account shown network)
          <$> optional shownArg
          <*> showAccountOption
          <*> showNetworkOption
    }

-- | What show shows, read by its shape ('readShown'), with what the shell
-- offers for it ('shownCompletion')
shownArg :: Args Shown
shownArg =
  argument (eitherReader readShown) $
    metavar "PATH|ADDRESS|HASH|TXID:INDEX|FILENAME"
      <> completer (mkCompleter shownCompletion)
      <> help "What to show: a path such as /0/3, an address of the wallet, a transaction by its txid or nosigHash, a coin, or a file of a transaction or of accounts. A hash goes in full or by its short id, the first 8 digits. Without it, the newest receiving address."

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

-- | The --account of show, which goes with some of what it shows only
showAccountOption :: Args (Maybe Text)
showAccountOption =
  accountOptionWith "The account of the path or of the newest address. Needed only when the wallet has more than one. With a transaction or a transaction file, the account whose view to show."

-- | The --network of show, which goes with a file only. What else show
-- shows is of an account, on the network of the account.
showNetworkOption :: Args (Maybe Network)
showNetworkOption =
  networkOptionWith "With a file only, as a file of another wallet needs it: for a transaction file that names no network and involves no account of this wallet, or for an account that a file of accounts names no network for. Without it, the default-network of the configuration file."

{- addresses -}

addressesCommand :: CommandSpec Command
addressesCommand =
  CommandSpec
    { name = "addresses",
      offline = False,
      summary = "List the receiving addresses of an account, or its change addresses",
      details =
        [r|
The newest addresses come first. Each takes two lines. The first gives its
derivation path, /0/i for a receiving address and /1/i for a change address,
the address and its label. The second gives what it can spend now, as
`accounts` describes, and what it received in how many transactions. An
address no transaction has paid yet is unused.

--verbose adds the confirmed and unconfirmed balances of the address, how many
coins it holds, and when it was created.

With --tabular, each address takes one row of a table, under a header that
names the columns: the path, the address, what it can spend, what it received
and in how many transactions, and its label. The amounts are in the unit the
header names. An address no transaction has paid leaves its amounts empty.
Change addresses have no label, so their table has no column for one.

With --csv FILENAME, each address takes one row of a new CSV file, for a
spreadsheet: its path, the address and its label, what it can spend, its
confirmed and unconfirmed balances, what it received, its coins and its
transactions, and when it was created. Amounts are in satoshis, and times in UTC.
|],
      args =
        CommandAddresses
          <$> accountOption
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
      summary = "List the addresses of an account that hold coins",
      details =
        [r|
`balances` lists the receiving and change addresses that hold coins, the
largest total first. Each takes two lines. The first gives its derivation
path, /0/i for a receiving address and /1/i for a change address, the address
and its label, or that it is a change address. The second gives three amounts.
The total is the value of all
its coins. The spendable amount is what it can spend now, as `accounts`
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
spreadsheet: its path, the address and its label, its total, what it can spend,
the three parts it cannot, and its coins. Amounts are in satoshis.
|],
      args =
        CommandBalances
          <$> accountOption
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
      summary = "Set the label of a receiving address or of a transaction",
      details =
        [r|
`label` gives a receiving address or a transaction a label, in place of the
one it had. An empty LABEL, "", removes it.

The label of an address names the payments it receives. `addresses`,
`balances` and the transactions that pay the address show it. A change
address takes no label.

The label of a transaction shows in txs and pendingtxs, in place of the
address a payment pays, and as a Label field with --verbose. preparetx, preparesendall and preparesweep
take one with --label. A pending transaction is named by the hash pendingtxs
shows, its nosigHash or its txid once signed. Its label goes with it to the
files of exporttx and --output, and stays once the network has it. bumpfee
gives a replacement the labels of the transactions it replaces. A transaction
on the network is named by its txid, as txs shows it. It gets the label in
every account that has it, such as both ends of a payment between two
accounts. Either hash goes in full or by its short id, its first 8 digits.
|],
      args =
        CommandLabel
          <$> (Label <$> labelledArg <*> textArg "LABEL" "The new label, or \"\" to remove it")
    }

-- | The address or the transaction to label, which also tells its account
labelledArg :: Args Labelled
labelledArg =
  argument (readLabelled <$> str) $
    metavar "ADDRESS|TXID"
      <> completer (mkCompleter labelTargets)
      <> help "A receiving address of the wallet, as `addresses` lists it, or a transaction, by its txid or the hash pendingtxs shows, in full or by its short id, the first 8 digits. Change addresses take no label."

{- txs -}

txsCommand :: CommandSpec Command
txsCommand =
  CommandSpec
    { name = "txs",
      offline = False,
      summary = "List the transactions of an account",
      details =
        [r|
The list is as of the last sync. Unconfirmed transactions come first, then
the newest. The transactions you prepared, and those you sent that `sync` has
not seen yet, are in pendingtxs. `show` TXID shows one transaction in full.

Each transaction takes two lines. The first starts with its short id, the first
8 digits of its txid, which the other commands take in place of the txid. It
then gives its time, its amount and its status. The time is in the time zone of
the computer. The amount includes the fee, with a plus sign when it comes in.
The status gives the confirmations, up to 999+ confirmations. --verbose gives
the full count. An unconfirmed transaction shows its fee rate too, which tells
how soon a block may take it. A dead one was replaced, or dropped from the
network.

The second line gives the balance of the account after the transaction. The
newest one shows what the account holds, confirmed or not. A dead transaction
shows no balance, and does not count. Then comes what the transaction is for. A
payment received gives the path of the address it was paid at, such as /0/3,
then the label of the payment, or else the label of the address. A payment
gives its label, or else the address it pays, the largest when there are
several. Nothing in it is cut, a long label or address included. On a
terminal, the addresses of the wallet and their labels are blue.

With --verbose, each transaction starts with its time, then its type: Outgoing,
Incoming, or Internal for a transaction between addresses of the account. The
txid is in full, then its short id. Every address is in full, and an address of
the wallet has its path after it. Its label is on the line below, with the labels of the coins it
spends, after the word from. Long labels wrap under the address.

With --tabular, each transaction takes one row of a table, under a header
that names the columns. The first, Txid, gives the short id. The amounts are in
the unit the header names. The Confs column gives the confirmations as a number
alone, up to 999+. An unconfirmed transaction has 0, and gives no fee rate. A
dead one says dead. The last column, Description, says what the transaction is
for, in the words of the compact view.

With --csv FILENAME, each transaction takes one row of a new CSV file, for a
spreadsheet: its time, type, amount, fee and fee rate, the balance after it,
its status, confirmations and block, its txid in full and by its short id, and
its label. Then come the address
it pays the most, its path and label when it is one of the wallet, and how many
other addresses it pays. Amounts are in satoshis, and times in UTC.
|],
      args =
        CommandTxs
          <$> accountOption
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
-- another view: it brings the compact view back for one listing, as
-- --new-change does for change-back. The help of --verbose comes first,
-- then that of --tabular, then the name of what the listing lists, for
-- those of --compact and --csv.
listingOutputOption :: String -> String -> String -> Args ListingOutput
listingOutputOption verboseHelp tabularHelp listed =
  fromMaybe (OnScreen Nothing)
    <$> optional
      ( flag' (OnScreen (Just Compact)) (long "compact" <> help compactHelp)
          <|> flag' (OnScreen (Just Verbose)) (short 'v' <> long "verbose" <> help verboseHelp)
          <|> flag' (OnScreen (Just Table)) (long "tabular" <> help tabularHelp)
          <|> CsvFile <$> strOption (long "csv" <> metavar "FILENAME" <> fileCompletion <> help csvHelp)
      )
  where
    compactHelp = "Show the " <> listed <> " in a few lines each. It is the default, unless the key listing-view of config.json names another view."
    csvHelp = "Write the " <> listed <> " to this file, one on each row, as CSV for a spreadsheet, with amounts in satoshis and times in UTC. The file must not exist yet."

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

The new transaction is kept as a pending transaction, which pendingtxs lists.
--output also writes it to a file, a PSBT (BIP 174), to carry to an offline
computer. A pending transaction is named by its nosigHash, a hash of the
transaction without its signatures. Once signed, it is shown by its txid instead, and the commands
take either hash, in full or by its short id, its first 8 digits. The coins it spends stay locked while it is pending.
deletetx deletes it and frees them.

On Bitcoin, the transaction is marked replaceable (BIP 125). Until it
confirms, bumpfee can replace it with one that pays a higher fee. Bitcoin Cash
does not replace transactions.

Coin control: --coin and --spend-from choose the coins to spend. preparetx then
spends every one of them that can be spent, and no other, whatever --strategy
says. With --select, it spends only what the payment needs among them: the
strategy selects among the chosen coins as it selects among all the coins of
the account without coin control, and a chosen coin worth less than its fee
stays. The chosen coins that cannot be spent, because they are locked or not
confirmed enough, are listed with the reason, with or without --select. The
list tells such a coin from one that --select left because the payment did
not need it. When none can be spent, nothing is prepared.

Change: the change goes to a new change address, unless --change-back sends it
back to an address the payment spends from. Some banks accept payments only
from addresses they have checked, and --change-back keeps the funds at such an
address. When the coins come from several addresses, preparetx lists them and
asks which one gets the change. With a single --spend-from address, the change
goes back to it without a question. Setting change-back to true in the
configuration file makes --change-back the default. --new-change then sends the
change of one payment to a new change address.

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
      args =
        CommandPrepareTx
          <$> ( PrepareTx
                  <$> some paymentArg
                  <*> payingAccountOption
                  <*> underFee feeOption
                  <*> underFee rcptPayOption
                  <*> underCoins minConfOption
                  <*> underCoins coinSelectionOption
                  <*> outputFileMaybeOption
                  <*> txLabelOption
                  <*> underCoins (many coinOption)
                  <*> underCoins (many spendFromOption)
                  <*> underCoins selectOption
                  <*> underFee prepareYesOption
                  <*> optionGroup "Change" changeBackOption
              )
    }

-- | The options of the fee of a payment or a sweep, under their
-- subheading of the help
underFee :: Args a -> Args a
underFee = optionGroup "Fee"

-- | The options that choose the coins a payment spends, under their
-- subheading of the help
underCoins :: Args a -> Args a
underCoins = optionGroup "Coins"

-- | The account a payment spends from, which coin control can tell
payingAccountOption :: Args (Maybe Text)
payingAccountOption =
  accountOptionWith "The account to pay from. Needed only when the wallet has more than one, and no --coin or --spend-from tells it."

-- | Where the change of preparetx goes, when the command says
changeBackOption :: Args (Maybe Bool)
changeBackOption =
  optional $
    flag' True (long "change-back" <> help "Send the change back to an address the payment spends from")
      <|> flag' False (long "new-change" <> help "Send the change to a new change address, even when the configuration file sets change-back")

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

{- preparesendall -}

prepareSendAllCommand :: CommandSpec Command
prepareSendAllCommand =
  CommandSpec
    { name = "preparesendall",
      offline = False,
      summary = "Prepare a payment of all the coins of an account",
      details =
        [r|
preparesendall prepares a transaction that sends every coin the account can
spend now, with no change. That is the spendable balance `accounts` shows. Like
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
account. --spend-dust or --leave-dust answers in advance, for scripts.

--coin and --spend-from send only the coins chosen, every one that can be
spent, dust included. --spend-dust and --leave-dust do not apply then.

The answer lists every coin the transaction leaves in the account, with the
reason: dust, locked by you or by a pending transaction, not spendable yet, or
not chosen with --coin or --spend-from.

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
                  <*> payingAccountOption
                  <*> underFee feeOption
                  <*> underCoins minConfOption
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
      <|> flag' LeaveDust (long "leave-dust" <> help "Leave the coins worth less than their fee in the account, without asking")

-- | An address of the account whose coins to spend
spendFromOption :: Args Text
spendFromOption =
  strOption $
    long "spend-from"
      <> metavar "ADDRESS"
      <> completer (mkCompleter addressesToSpendFrom)
      <> help "Coin control: spend the coins at this address of the account. Repeat for several addresses."

-- | Whether the strategy selects among the coins of coin control, instead
-- of every one of them being spent
selectOption :: Args Bool
selectOption =
  switch $
    long "select"
      <> help "Coin control: spend only what the payment needs among the coins of --coin and --spend-from, as --strategy selects them, instead of every one of them"

-- | A coin to spend, as coins lists it, in full or by its short id
coinOption :: Args CoinRef
coinOption =
  option coinReader $
    coinOptionFields "Coin control: spend this coin, as `coins` lists it, by its short id or with its txid in full. Repeat for several coins."
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

rcptPayOption :: Args FeePayer
rcptPayOption =
  flag SenderPays RecipientsPay $
    short 'r'
      <> long "recipients-pay"
      <> help "The recipients pay the fee, in equal shares taken from their amounts"

minConfOption :: Args Natural
minConfOption =
  option (maybeReader $ readNatural . cs) $
    short 'm'
      <> long "min-conf"
      <> metavar "INT"
      <> value 0
      <> showDefault
      <> help "The confirmations a coin needs to be spent. At 0, the unconfirmed coins of the wallet's own transactions are spent, its change and what its accounts pay each other, but no unconfirmed payment from others."

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
txLabelOption =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "LABEL"
      <> help "A label for the transaction, to recognize it later. It goes with the transaction to its --output file, and stays once the network has it."

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
`sync` retires it once the network lists it. The newest come first. `show`
HASH shows one of them in full.

Each takes two lines. The first starts with the short id of the transaction,
the first 8 digits of its hash. The hash is the nosigHash of an unsigned
transaction, and the txid of a signed one. The other commands name a pending
transaction by that hash, in full or by its short id. The nosigHash of a signed
one works too. --verbose gives the hash in full, then its short id. The first
line then gives the time the transaction was created, its amount and its
status: not signed, signed and not sent, or sent, with its fee rate.

The second line gives the account when every account is listed. A replacement
that bumpfee made then names the transactions it replaces. Last comes what the
transaction is for, as txs says it: its label, or else the address it pays. An
address of the wallet is named by its derivation path.

A transaction marked conflicted spends a coin that another transaction has
spent, or that is gone, as the server said at the last sync. It cannot be sent
while that lasts. Delete it with deletetx, which frees its coins.

With --tabular, each transaction takes one row of a table, under a header that
names the columns: its short id, its account when every account is listed, the
time it was created, its type, its amount, its status, its fee rate in sat/vB,
and what it is for, as txs --tabular says it. The account column is as wide as
the longest name.

With --csv FILENAME, each transaction takes one row of a new CSV file, for a
spreadsheet: its account, when it was created, its type, amount, fee and fee
rate, its status, its nosigHash and its txid once signed, each in full and by
its short id, and its label. Then come
the address it pays the most, its path and label when it is one of the wallet,
and how many other addresses it pays. Amounts are in satoshis, and times in UTC.
|],
      args =
        CommandPendingTxs
          <$> onlyAccountOption
          <*> listingOutputOption
            txVerboseHelp
            "Show each transaction on one row of a table: its short id, account, time, type, amount, status and fee rate, then what it is for."
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
comes from, the keys of the account in it with their derivation paths, and
the signatures once signed. What is hw's own goes in the proprietary fields
of the format, under the identifier hw, which other software leaves aside:
the network of the transaction, the fee rate it was built for, the
transactions a replacement replaces, the label of the transaction, the
labels of the account's addresses in it, and the labels of the coins it
spends, as the wallet has them when it writes the file, for the offline
computer to show. bitcoin-cli decodepsbt shows the file too.

importtx adds the transaction of a file to the pending transactions of its
account, and locks the coins it spends. When the wallet holds it already, the
file takes its place, as when an offline computer returns it signed. The
signatures are checked first. The wallet keeps its own labels, and shows the
transaction with them.

A file another wallet or a hardware wallet signed is imported the same way,
in bytes or in base64 text, finalized or not: hw finalizes an input left
signed, and refuses a signature that does not verify. What the file leaves
out, the paths of the account's addresses once the signatures are final, the
fee rate and the label, the wallet keeps from the version it holds. A file
another wallet prepared for an account of this wallet is imported too, and
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
          <$> txAccountOption
          <*> fileArgument "The transaction file to add"
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
signtx asks for the mnemonic and the passphrase of the account, and signs the
transaction. It asks again while they do not match the account. It takes the
shares of a backup in place of the mnemonic.

Give a pending transaction by its NOSIGHASH, in full or by its short id. The
signed transaction then takes its place in the wallet, shown by its txid. Or give a transaction file with
--input, on an offline computer for example. The signed transaction then goes
to the --output file. Without --output, hw offers to replace the --input file,
since the unsigned file is of no use once signed. It asks this before the
mnemonic. A transaction of a file is shown with the labels the file carries,
which go on to the signed file. A signed input names no key any more, as the
format asks. The file still says which address of the account it spends from,
in a field of hw, so `show` shows the signed file as the payment it is, on
the offline computer too.

A file another wallet prepared for the account is signed the same way, in
bytes or in base64 text, of version 0 or of version 2, that of BIP 370. The
signed file keeps its version and every field of it, but those of the inputs
signed that served the signers. hw signs with the
sighash ALL only, and only when every input that is not the account's is
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
      summary = "List the coins of an account",
      details =
        [r|
A coin is an unspent output of a transaction, written TXID:INDEX. The
unconfirmed coins come first, then the newest. Each takes two lines. The first
starts with the short id of the coin:
the first 8 digits of its txid, a colon, and its index, such as a1075db5:1.
lockcoin, --coin and `show` take it, or the coin in full. The first line then gives the
time of the transaction that paid the coin, in the time zone of the computer,
its value, its state and its confirmations, up to 999+ confirmations. A coin
that a pending transaction spends is "locked by tx". The second line gives the
address that holds the coin, with its derivation path, then the label of the
address. A coin at a change address says so there.

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
spreadsheet: the coin, TXID:INDEX, in full and by its short id, the time of its
transaction, its value, its
state, its confirmations and block, the address that holds it with its path
and label, and the label of its transaction. Amounts are in satoshis, and times in UTC.

A coin is spendable when the wallet can spend it now. Nothing locks it, a
coinbase coin has 100 confirmations, and an unconfirmed coin comes from a
transaction of the wallet: its change, or what another account of the wallet
paid. The other coins become spendable once they confirm, or for a coinbase,
once it matures.

A pending transaction locks the coins it spends, until the network lists it or
deletetx deletes it. You can also lock a coin yourself with lockcoin, until
unlockcoin.
|],
      args =
        CommandCoins
          <$> accountOption
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
|],
      args = CommandLockCoin <$> coinArg coinsToLock
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
      args = CommandUnlockCoin <$> coinArg coinsToUnlock
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
bumpfee prepares a transaction that replaces a payment of the account that
the network has not confirmed yet. The replacement pays a higher fee, as BIP
125 allows on Bitcoin. It spends the same coins and pays the recipients the
same amounts. The higher fee comes out of the change.

When the change cannot pay the higher fee, hw offers to add confirmed coins of
the account. When none would do, or when you decline them, hw offers to take
what is missing from what the recipients receive instead: one of them pays it
alone, or all of them pay it in equal shares, the first also paying the
remainder. hw asks which, and leaves no recipient below the dust limit.

Another transaction of the account may spend the change of the stuck one.
Replacing the stuck one would remove it too, so hw offers to replace both with
one transaction that pays the recipients of both.

The replacement is a pending transaction. Sign it with signtx and send it with
broadcast, which then drops the transactions it replaces from the pending
transactions.

The network does not tell hw when it refuses a replacement. A replacement
must also pay the fees of transactions hw cannot see, such as a recipient's
spend of the payment. When a replacement never confirms, delete it with
deletetx, run `sync`, and bump again at a higher fee rate.

Some transactions cannot be bumped. One sent before hw marked its transactions
replaceable cannot. Neither can one that spends coins of another wallet, such
as a sweep. Bitcoin Cash does not replace transactions. When the stuck
transaction pays several addresses of the wallet besides its recipients, its
change among them, the replacement gets one change output for all of them.

The replacement gets a lock time as preparetx gives one. When the best block
the wallet knows was mined more than eight hours ago, bumpfee asks whether to
prepare it anyway, with a lock time of 0, as preparetx does.
|],
      args =
        CommandBumpFee
          <$> ( BumpFee
                  <$> bumpedArg
                  <*> txAccountOption
                  <*> replacementFeeOption
                  <*> outputFileMaybeOption
                  <*> yesOption "Answer yes to every question: a fee rate above 100 satoshis per virtual byte, a wallet behind the chain, adding coins, reducing what a lone recipient receives, and replacing the transactions that spend the change. Which of several recipients pays is still asked."
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

The server passes the transaction to the nodes without checking it. broadcast
then waits up to ten seconds for the server to see it in its mempool. That is
the only sign that the network accepted it. If the server does not see it,
nothing is recorded, and broadcast says so. The network may have refused the
transaction, for a fee below what the nodes relay for example. Or it may just
be slow. Run broadcast again later.

Give a pending transaction by its TXID, as signtx and pendingtxs show it, in
full or by its short id. Its nosigHash works too. Once the server sees it, it is marked as sent. `sync`
then sends it again whenever the server loses it, and retires it once the
network lists it. A transaction the network refuses for good is sent again on
every sync, until you delete it with deletetx.

Give a signed file with --input, such as the one an offline computer returns.
When the wallet holds its transaction as pending, the file takes its place and
is sent as above. Otherwise, a sweep of outside coins for example, hw checks
the transaction and sends it, and changes nothing in the wallet.

The file is a PSBT, in bytes or in base64 text, as another wallet or a
hardware wallet returns it, finalized or not. The signed transaction alone,
in hexadecimal as a wallet gives it once finalized, is sent too, when the
wallet holds its transaction. A file hw wrote names its network. One of
another wallet names none: when the wallet holds its transaction, the
account tells it; otherwise give --network, or a default-network in the
configuration file.
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
      summary = "Update the accounts from the network",
      details =
        [r|
`sync` asks the server for the latest balances, transactions and coins of the
accounts. For each account, it shows its balances as `accounts` does, the
block it is synced to, and what changed.

On a terminal, a line tells what `sync` is doing while it works, with a bar for
the steps whose size it knows. The line goes to stderr and is gone when `sync`
ends, so scripts never see it.

It first looks for used addresses past the ones each account has, as far as
the gap of the configuration file, 20 by default. Someone paying to the
account's public key, or another wallet of the same seed, may have used them.
`sync` adds each used address it finds, with the ones before it. It keeps
looking while it finds more, so the first sync of an account finds every
address used so far. It misses an address that comes after more unused ones in
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
spent, or that is gone, is marked conflicted. A signed transaction you never
sent is left alone.

A sent transaction is sent again on every sync while the server does not list
it, unless it is conflicted. So one the network refuses for good, for a fee
below what the nodes relay for example, keeps coming back. Delete it with
deletetx.

When the server does not answer for a network, `sync` leaves out the accounts
of that network and says why. It still syncs the other accounts, then exits
with 1.
|],
      args = CommandSync <$> onlyAccountOption <*> fullOption
    }

fullOption :: Args Bool
fullOption =
  switch $
    long "full"
      <> help
        [r|
Download the balances, transactions and coins of the account again and write
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
The copy holds the whole wallet: accounts, addresses, labels, coins,
transactions and pending transactions. It holds no private key. Without a
FILENAME, the copy goes to ~/.hw/backups, named after the format version of the
database and the time.

To restore a backup, replace ~/.hw/accounts.sqlite with the copy, or point
database-file in ~/.hw/config.json to it. Then run any command. A copy made by
an older hw is upgraded, after hw backs it up again to ~/.hw/migration-backups.
A copy made by a newer hw is refused.
|],
      args =
        CommandBackup
          <$> optional (fileArgument "The file to copy the database to. It must not exist yet. Without it, the copy goes to ~/.hw/backups.")
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
exportsheet writes the accounts, the addresses, the balances, the coins, the
transactions and the pending transactions to a new spreadsheet, a tab each, in
the OpenDocument format of LibreOffice (.ods). Excel and Google Sheets open it
too. Each tab holds every row of its listing, in the columns of its --csv file.

The tabs hold every account, or the one --account names. Those of the
addresses, the balances, the coins and the transactions start with a column
that names the account of each row. The addresses of both chains share a tab,
the receiving ones first.

Each cell keeps its kind. A label is text, whatever it starts with, an amount
is a number of satoshis, and a time is a time, in UTC. The header of each tab
stays at the top as its rows scroll.

The six listings are read at one moment, so the tabs agree with each other.
The file must not exist yet. It is readable by you alone, since it tells your
whole history.
|],
      args =
        CommandExportSheet
          <$> onlyAccountOption
          <*> fileArgument "The spreadsheet to write, such as wallet.ods. It must not exist yet."
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
signmsg asks for the mnemonic of the account of the ADDRESS, or for the
shares of its backup, and signs the message with the key of that address. The signature uses the Bitcoin Signed
Message format, the legacy format of BIP 322. From a P2PKH address, which
starts with 1, any wallet can verify it. From a native SegWit address,
which starts with bc1q, the signature has the header byte of BIP 137, which
the wallets that sign from such an address give and verify; some wallets
verify no signature from a SegWit address.

The message is signed exactly as given. hw warns about spaces or line breaks
at its start or end. Copying a message easily changes them, and the signature
would then fail to verify.

With --output, hw writes the armored block to a file instead of printing it.
verifymsg --input can then check the file. It also reads the --json answer,
saved to a file.
|],
      args =
        CommandSignMsg
          <$> (SignMsg <$> signAddressArg <*> signMsgArg <*> signedMessageOutputOption)
    }

signAddressArg :: Args Text
signAddressArg =
  strArgument $
    metavar "ADDRESS"
      <> completer (mkCompleter walletAddresses)
      <> help "The address whose key signs. It must be an address of this wallet."

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
that made it chose. The --network only tells hw how
to read the address.

Give the message exactly as it was signed. A single changed byte makes the
signature invalid. When it is invalid, hw says so and exits with an error, so
that a script can tell.

With --input, hw reads the three from a file holding the output of signmsg:
its armored block, which other wallets write too, or its --json answer. In a
block, the message is everything between the first marker and the signature
marker, exactly as it is. After the signature marker come the address, then
the signature.
|],
      args = CommandVerifyMsg <$> (VerifyMsg <$> source <*> networkMaybeOption)
    }
  where
    source =
      (Given <$> verifyAddressArg <*> signatureArg <*> verifyMsgArg)
        <|> (FromFile <$> signedMessageFileOption)

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

A sweep that pays an address of this wallet belongs to the account of that
address, on its network. hw finds the account however the address is written,
such as a Bitcoin Cash address with or without its prefix, or in its legacy
form. The sweep is kept as a pending transaction, to sign with signsweep by
its nosigHash. --output also writes it to a file.

A sweep that pays no address of this wallet needs no account, even in a
wallet that has none. It needs --network and --output, and is signed from the
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
                  <$> accountOptionWith "The account the sweep pays. Needed only when the --sweep-to addresses are in several accounts."
                  <*> sweepNetworkOption
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
replace the --input file. It asks this before the keys. A sweep of a file is
shown with the labels the file carries, which go on to the signed file.

A sweep that pays no account of this wallet needs no account here either. It
is signed on the network its file names. A file of another wallet names
none: give --network, or a default-network in the configuration file.
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
