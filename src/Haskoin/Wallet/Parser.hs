{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of hw: its commands, each with its options and its
-- help, and the options that go with any command. A command is a
-- 'CommandSpec'. Each of its options is made once, as the row of the help
-- that describes it and the parser that reads it, with the builders of
-- "Haskoin.Wallet.Parser.Args". "Haskoin.Wallet.Parser.Run" runs the
-- command line, and "Haskoin.Wallet.Parser.Help" lays out the help.
module Haskoin.Wallet.Parser where

import Control.Applicative (Alternative (..), optional)
import Data.List (find, intercalate)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32)
import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit (..), readUnit)
import Haskoin.Wallet.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.FeeRates (FeeRateChoice, readFeeRateChoice)
import Haskoin.Wallet.Keys (KeySource (..))
import Haskoin.Wallet.Parser.Args
import Haskoin.Wallet.Parser.Run (runProgram)
import Haskoin.Wallet.Util (Page (Page), readNatural)
import Numeric.Natural (Natural)
import Text.RawString.QQ (r)

{- Commands -}

-- | A command of hw, with its options. Options that could be taken for one
-- another, being many or of the same type, come in a record of their own,
-- read by name; the others come as arguments of distinct types.
data Command
  = CommandMnemonic !NewMnemonic
  | -- | The name of the new account, its network, unless the default one,
    -- and the parts the mnemonic is typed in
    CommandCreateAcc !Text !(Maybe Network) !Natural
  | CommandRecover !Recover
  | -- | The account, and the parts the mnemonic is typed in
    CommandTestAcc !(Maybe Text) !Natural
  | -- | The account document to import
    CommandImportAcc !FilePath
  | -- | The account, and the file to write its document to
    CommandExportAcc !(Maybe Text) !FilePath
  | CommandRenameAcc !RenameAcc
  | -- | The account, or every one
    CommandAccounts !(Maybe Text)
  | -- | The name of the account, with its network and its path to confirm
    -- the deletion
    CommandDeleteAcc !Text !Network !HardPath
  | CommandReceive !Receive
  | -- | The account, whether its change addresses, and the page
    CommandAddrs !(Maybe Text) !Bool !Page
  | -- | The account, and the page
    CommandBalances !(Maybe Text) !Page
  | -- | The account, the index of the receiving address, and its label
    CommandLabel !(Maybe Text) !Int !Text
  | -- | The account, and the page
    CommandTxs !(Maybe Text) !Page
  | CommandPrepareTx !PrepareTx
  | CommandPrepareSendAll !PrepareSendAll
  | -- | The account, and the page
    CommandPendingTxs !(Maybe Text) !Page
  | -- | The account, and the transaction file
    CommandReviewTx !(Maybe Text) !FilePath
  | -- | The account, and the transaction file
    CommandImportTx !(Maybe Text) !FilePath
  | -- | The nosigHash of the pending transaction, and the file to write it to
    CommandExportTx !TxHash !FilePath
  | -- | The nosigHash of the pending transaction
    CommandDeleteTx !TxHash
  | -- | What to sign, and the parts the mnemonic is typed in
    CommandSignTx !Signing !Natural
  | CommandFees !(Maybe Network)
  | CommandSignMsg !SignMsg
  | CommandVerifyMsg !VerifyMsg
  | -- | The account, whether the locked coins only, and the page
    CommandCoins !(Maybe Text) !Bool !Page
  | CommandLockCoin !OutPoint
  | CommandUnlockCoin !OutPoint
  | -- | The nosigHash of a pending transaction, or the file of a signed
    -- one, and whether to send it without asking
    CommandSendTx !(Maybe TxHash) !(Maybe FilePath) !Bool
  | CommandBumpFee !BumpFee
  | -- | The account, or every one, and whether to sync from the start
    CommandSync !(Maybe Text) !Bool
  | -- | The file of the copy, or none for the backups directory of hw
    CommandBackup !(Maybe FilePath)
  | CommandVersion
  | CommandPrepareSweep !PrepareSweep
  | -- | What to sign, and the file of the private keys, typed on the
    -- terminal without one
    CommandSignSweep !Signing !(Maybe KeySource)
  deriving (Eq, Show)

-- | The options of mnemonic
data NewMnemonic = NewMnemonic
  { -- | The bytes of entropy
    entropy :: !Natural,
    -- | Whether dice rolls are mixed in
    dice :: !Bool,
    -- | The parts the mnemonic is split in
    parts :: !Natural
  }
  deriving (Eq, Show)

-- | The options of recover
data Recover = Recover
  { -- | The network, unless the default one
    network :: !(Maybe Network),
    -- | The accounts in a row without a payment that end the search
    accountGap :: !Natural,
    -- | The parts the mnemonic is typed in
    parts :: !Natural
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

-- | The options of preparetx
data PrepareTx = PrepareTx
  { -- | The addresses paid, each with its amount, as typed: ADDRESS:AMOUNT
    recipients :: ![Text],
    account :: !(Maybe Text),
    -- | The fee rate: a number of satoshis per byte or a level of the
    -- rates recent blocks paid, or none to ask the user from those rates
    feeRate :: !(Maybe FeeRateChoice),
    -- | The dust limit, in satoshis
    dust :: !Natural,
    feePayer :: !FeePayer,
    -- | The confirmations a coin needs to be spent
    minConf :: !Natural,
    strategy :: !CoinSelection,
    output :: !(Maybe FilePath),
    -- | Coin control: the coins chosen by their outpoints (--coin)
    coins :: ![OutPoint],
    -- | Coin control: the addresses whose coins are chosen (--spendfrom)
    spendFrom :: ![Text],
    -- | Whether the strategy selects among the chosen coins (--select),
    -- instead of every one of them being spent
    select :: !Bool,
    -- | Whether a fee rate above 100 satoshis per byte is taken without
    -- asking (--yes)
    yes :: !Bool,
    -- | Whether the change goes back to an address the payment spends
    -- from (--changeback) or to a new change address (--newchange), when
    -- the command says. Otherwise the configuration file says.
    changeBack :: !(Maybe Bool)
  }
  deriving (Eq, Show)

-- | The options of bumpfee
data BumpFee = BumpFee
  { -- | The transaction to replace, by its id
    bumped :: !TxHash,
    account :: !(Maybe Text),
    -- | The fee rate of the replacement, as for preparetx
    feeRate :: !(Maybe FeeRateChoice),
    -- | The dust limit, in satoshis
    dust :: !Natural,
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
    -- | The fee rate: a number of satoshis per byte or a level of the
    -- rates recent blocks paid, or none to ask the user from those rates
    feeRate :: !(Maybe FeeRateChoice),
    -- | The dust limit, in satoshis
    dust :: !Natural,
    -- | The confirmations a coin needs to be sent
    minConf :: !Natural,
    output :: !(Maybe FilePath),
    -- | The coins to send, instead of all of them
    coins :: ![OutPoint],
    -- | The addresses whose coins to send, instead of all of them
    spendFrom :: ![Text],
    -- | What to do with the coins worth less than their fee, or Nothing to
    -- ask the user
    dustCoins :: !(Maybe DustCoins),
    -- | Whether a fee rate above 100 satoshis per byte is taken without
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
    -- | The parts the mnemonic is typed in
    parts :: !Natural
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
    -- | The fee rate, as for preparetx
    feeRate :: !(Maybe FeeRateChoice),
    -- | The dust limit, in satoshis
    dust :: !Natural,
    -- | Whether a fee rate above 100 satoshis per byte is taken without
    -- asking (--yes)
    yes :: !Bool
  }
  deriving (Eq, Show)

-- | What signtx and signsweep sign, and where the signed transaction goes:
-- a pending transaction by its nosigHash, or an --input file signed to an
-- --output file
data Signing = Signing
  { account :: !(Maybe Text),
    nosigHash :: !(Maybe TxHash),
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

-- | The command typed, with the unit of its amounts and whether it answers
-- in JSON. A request for help, and a command line that cannot be parsed,
-- print what they have to and leave.
parserMain :: IO (Command, AmountUnit, Bool)
parserMain = do
  (cmd, (unit, json)) <- runProgram program
  return (cmd, unit, json)

-- | hw: its commands by group, the options every command takes, and the
-- help of the program
program :: Program Command (AmountUnit, Bool)
program =
  Program
    { name = "hw",
      commandUsage = "COMMAND",
      -- The help adds which commands to run on an offline computer
      intro =
        [r|
hw is a BIP44 wallet for Bitcoin and Bitcoin Cash. It stores public keys
only.
|],
      everydayNames = ["mnemonic", "recover", "accounts", "sync", "receive", "balances", "label", "coins", "fees", "backup", "version"],
      details =
        [r|
A payment takes three steps:

  hw preparetx ADDRESS:AMOUNT    prepare it, without any key
  hw signtx NOSIGHASH            sign it with the mnemonic
  hw sendtx NOSIGHASH            send it to the network

To sign on an offline computer, create the account there with createacc. Copy
it to the online computer with exportacc and importacc. Then carry each
transaction between them with preparetx --output, signtx --input and sendtx
--input.

The options --unit and --json go before or after the command. The settings,
such as the server, the default network and the address gap, are in
~/.hw/config.json.

Type hw COMMAND --help for the help of a command.
|],
      globals = (,) <$> unitOption <*> jsonOption,
      groups =
        [ CommandGroup
            "Mnemonics and accounts"
            [ mnemonicCommand,
              createAccCommand,
              recoverCommand,
              testAccCommand,
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
              addrsCommand,
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
              sendTxCommand,
              bumpFeeCommand
            ],
          CommandGroup
            "Transactions"
            [ txsCommand,
              pendingTxsCommand,
              reviewTxCommand,
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
              versionCommand
            ]
        ]
    }

jsonOption :: Args Bool
jsonOption =
  switch $
    short 'j'
      <> long "json"
      <> help "Print the answer as JSON, for scripts. Questions and warnings go to stderr."

unitOption :: Args AmountUnit
unitOption =
  option (maybeReader (readUnit . cs)) $
    short 'u'
      <> long "unit"
      <> metavar "UNIT"
      <> value UnitBitcoin
      <> completeWith ["btc", "bit", "sat"]
      <> help
        [r|
The unit of the amounts shown, and of the amounts typed without a unit: btc,
bit or sat (default: btc). A bit is 100 satoshis.
|]

{- mnemonic -}

mnemonicCommand :: CommandSpec Command
mnemonicCommand =
  CommandSpec
    { name = "mnemonic",
      offline = True,
      summary = "Generate a new mnemonic",
      details =
        [r|
hw prints the mnemonic and stores nothing. Write it down and keep copies in
safe places, away from computers and phones. Whoever has the mnemonic can
spend the funds. If you lose it, the funds are lost.

The entropy comes from the operating system, read from /dev/random where the
machine has it. With --dice, hw also mixes in rolls of a die, so that you do
not have to trust the system alone. Run this command on an offline computer
if you can.

With --split, hw prints several parts instead of one mnemonic. The commands
that ask for the mnemonic then need every part, in any order, with the same
--split.
|],
      args =
        CommandMnemonic
          <$> (NewMnemonic <$> entropyOption <*> diceOption <*> splitInOption)
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
      <> help "The bytes of entropy: 16, 20, 24, 28 or 32, for a mnemonic of 12, 15, 18, 21 or 24 words"
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
hw tells how many rolls are left after each line.
|]

-- | The parts mnemonic splits the mnemonic into
splitInOption :: Args Natural
splitInOption =
  partsOption
    [r|
Split the mnemonic into INT parts, from 1 to 12. All the parts are needed to
rebuild it, in any order. With --dice, hw asks for enough rolls for every
part, and uses no roll twice.
|]

-- | The parts a mnemonic is typed in, when mnemonic --split made it
mnemonicPartsOption :: Args Natural
mnemonicPartsOption =
  partsOption "The number of parts of the mnemonic, when `mnemonic` --split made it. hw asks for each part."

-- | The --split option, with the help of the command about it
partsOption :: String -> Args Natural
partsOption what =
  option (eitherReader $ f . cs) $
    short 's'
      <> long "split"
      <> metavar "INT"
      <> value 1
      <> showDefault
      <> help what
  where
    f s =
      case readNatural s of
        Just n ->
          if n >= 1 && n <= 12
            then Right n
            else Left "The --split value has to be between 1 and 12"
        Nothing -> Left "Could not parse the --split option"

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
typo. It derives the account key m/44'/coin'/index' and stores only its public
key. It takes the first index of the seed that has no account in the wallet
on that network. For example, the first Bitcoin account is m/44'/0'/0'.

The private keys are never stored. To keep them on an offline computer,
create the account there. Then copy it to an online computer with exportacc
and importacc.

A passphrase with accents or other non-ASCII letters is normalized as BIP39
asks, so that other wallets find the same seed. Older versions of hw did not
normalize it. If the wallet has accounts of this mnemonic made that way, and
none of the normalized seed, createacc keeps to the old seed and says so.
|],
      args =
        CommandCreateAcc
          <$> textArg "NAME" "The name of the new account"
          <*> networkMaybeOption
          <*> mnemonicPartsOption
    }

-- | The network, when given. Without it, a command that needs one takes the
-- default-network of the configuration file.
networkMaybeOption :: Args (Maybe Network)
networkMaybeOption = networkOptionWith "Without it, the default-network of the configuration file."

-- | The network of a sweep, when given. A sweep that pays an account of the
-- wallet is on the network of that account. One that pays none needs it.
sweepNetworkOption :: Args (Maybe Network)
sweepNetworkOption =
  networkOptionWith "Needed by a sweep that pays no account of this wallet. The default-network of the configuration file does not apply to it."

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
      offline = True,
      summary = "Find the accounts of a mnemonic that were paid",
      details =
        [r|
`recover` asks for the mnemonic, then searches the network for the accounts of
its seed that were paid. It checks the accounts in order from index 0, and
asks the server about the receiving addresses of each. Each account that was
paid is added, named after the network and its index, such as btc0, and
synced. Accounts the wallet has already are skipped.

The search stops after --account-gap accounts in a row without a payment.
BIP44 stops at the first such account. hw looks further, since accounts
created on an offline computer may never have been paid.

`recover` needs the mnemonic and the server at the same time. To keep the
mnemonic offline, create the accounts on the offline computer with createacc
instead. Copy them with exportacc and importacc, and run `sync`.

hw normalizes a passphrase as BIP39 asks, and older versions did not. When
nothing is found, `recover` also tries the passphrase as typed. It says so if
that finds accounts.
|],
      args =
        CommandRecover
          <$> (Recover <$> networkMaybeOption <*> accountGapOption <*> mnemonicPartsOption)
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
          <$> textArg "NAME" "The name of the account to delete"
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

{- testacc -}

testAccCommand :: CommandSpec Command
testAccCommand =
  CommandSpec
    { name = "testacc",
      offline = True,
      summary = "Check a mnemonic and passphrase against an account",
      details =
        [r|
testacc asks for the mnemonic and the passphrase, and checks that they give
the public key of the account. Use it to test a mnemonic you wrote down,
before you rely on it. Nothing is signed. When they match, hw records the
fingerprint of the seed, which `accounts` shows.
|],
      args =
        CommandTestAcc
          <$> accountOption
          <*> mnemonicPartsOption
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
      <> completer (mkCompleter accountCompleter)
      <> help what

{- importacc -}

importAccCommand :: CommandSpec Command
importAccCommand =
  CommandSpec
    { name = "importacc",
      offline = False,
      summary = "Add an account from a file written by exportacc",
      details = importExportAccDetails,
      args =
        CommandImportAcc
          <$> fileArgument "The account file, written by exportacc"
    }

importExportAccDetails :: String
importExportAccDetails =
  [r|
exportacc writes an account to a file: its public key M/44'/coin'/index', its
name, its network and, when hw knows it, the fingerprint of its seed. The file
holds no private key. importacc adds the account to another wallet, which can
then watch it and prepare its payments without the mnemonic. This is how an
online computer works with an offline signer.

The fingerprint is the four bytes that PSBTs and output descriptors write
before a key of the seed. `accounts` groups the accounts of one seed by it.
Importing the file of an account the wallet has already records the
fingerprint, when the wallet did not know it.
|]

{- exportacc -}

exportAccCommand :: CommandSpec Command
exportAccCommand =
  CommandSpec
    { name = "exportacc",
      offline = False,
      summary = "Write the public key of an account to a file",
      details = importExportAccDetails,
      args =
        CommandExportAcc
          <$> accountOption
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
      <> completer (mkCompleter accountCompleter)
      <> help desc

accountCompleter :: String -> IO [String]
accountCompleter _ = return []

{- accounts -}

accountsCommand :: CommandSpec Command
accountsCommand =
  CommandSpec
    { name = "accounts",
      offline = False,
      summary = "Show the accounts and their balances",
      details =
        [r|
Each account shows the fingerprint of its seed when known, its derivation
path, its network, and how many receiving (external) and change (internal)
addresses it has. The accounts of one seed come together.

The confirmed and unconfirmed balances are those the server gave at the last
sync. The spendable balance is what the account can spend now. It leaves out
the coins locked by a pending transaction or by lockcoin, the coinbase coins
with fewer than 100 confirmations, and the unconfirmed payments from other
wallets. The unconfirmed change of the wallet's own transactions counts.
preparetx can spend all of it, unless --minconf asks for confirmations.
|],
      args = CommandAccounts <$> onlyAccountOption
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
addresses, at M/44'/coin'/index'/1/i. addrs --internal lists them.

`receive` gives out at most 20 unused addresses in a row, or as many as the gap
of the configuration file says. A wallet restored from the mnemonic stops
looking after that many unused addresses, and would miss later payments. Once that many
addresses await a payment, `receive` refuses. Give out one of them again. addrs
lists them.
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

{- addrs -}

addrsCommand :: CommandSpec Command
addrsCommand =
  CommandSpec
    { name = "addrs",
      offline = False,
      summary = "List the receiving addresses of an account, or its change addresses",
      details =
        [r|
The newest addresses come first. Each shows its derivation path, /0/i for a
receiving address and /1/i for a change address, and its label. Then come its
number of transactions, what it received, and what it can spend now, as
`accounts` describes.
|],
      args =
        CommandAddrs
          <$> accountOption
          <*> internalOption
          <*> (Page <$> limitOption <*> offsetOption)
    }

internalOption :: Args Bool
internalOption =
  switch $
    long "internal"
      <> help "List the change addresses instead of the receiving ones"

offsetOption :: Args Natural
offsetOption =
  option (maybeReader $ readNatural . cs) $
    short 'o'
      <> long "offset"
      <> metavar "INT"
      <> value 0
      <> showDefault
      <> help "How many rows to skip first. --limit 10 --offset 20 shows rows 21 to 30."

limitOption :: Args Natural
limitOption =
  option (maybeReader $ readNatural . cs) $
    short 'l'
      <> long "limit"
      <> metavar "INT"
      <> value 5
      <> showDefault
      <> help "How many rows to show. 0 shows them all."

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
largest total first. Each shows its derivation path, /0/i for a receiving
address and /1/i for a change address, and three amounts. The total is the
value of all its coins. The spendable amount is what it can spend now, as
`accounts` describes. The unspendable amount is the rest. It counts the coins
locked by a pending transaction or by lockcoin, the unconfirmed payments from
other wallets, and the coinbase coins with fewer than 100 confirmations.
`coins` tells which applies to each coin.
|],
      args =
        CommandBalances
          <$> accountOption
          <*> (Page <$> limitOption <*> offsetOption)
    }

{- label -}

labelCommand :: CommandSpec Command
labelCommand =
  CommandSpec
    { name = "label",
      offline = False,
      summary = "Set the label of a receiving address",
      details = "",
      args =
        CommandLabel
          <$> accountOption
          <*> addrIndexArg
          <*> textArg "LABEL" "The new label"
    }

addrIndexArg :: Args Int
addrIndexArg =
  argument (maybeReader readAddrIndex) $
    metavar "INDEX"
      <> help
        [r|
The index i of the receiving address, the last number of its path /0/i in
addrs. The first address has the index 0. Change addresses have no label.
|]
  where
    -- An address is derived at a soft index of BIP32, below 2^31. A larger
    -- number names no address, rather than wrapping around to another one.
    readAddrIndex :: String -> Maybe Int
    readAddrIndex s = case readNatural (cs s) of
      Just n | n < 2 ^ (31 :: Int) -> Just (fromIntegral n)
      _ -> Nothing

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
not seen yet, are in pendingtxs.
|],
      args =
        CommandTxs
          <$> accountOption
          <*> (Page <$> limitOption <*> offsetOption)
    }

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
on an offline computer if you like, and sendtx sends it to the network.

The new transaction is kept as a pending transaction, which pendingtxs lists.
--output also writes it to a file, to carry to an offline computer. A pending
transaction is named by its nosigHash, a hash of the transaction without its
signatures. It stays the same once the transaction is signed. The coins it
spends stay locked while it is pending. deletetx deletes it and frees them.

On Bitcoin, the transaction is marked replaceable (BIP 125). Until it
confirms, bumpfee can replace it with one that pays a higher fee. Bitcoin Cash
does not replace transactions.

Coin control: --coin and --spendfrom choose the coins to spend. preparetx then
spends every one of them that can be spent, and no other, whatever --strategy
says. With --select, it spends only what the payment needs among them: the
strategy selects among the chosen coins as it selects among all the coins of
the account without coin control, and a chosen coin worth less than its fee
stays. The chosen coins that cannot be spent, because they are locked or not
confirmed enough, are listed with the reason, with or without --select. The
list tells such a coin from one that --select left because the payment did
not need it. When none can be spent, nothing is prepared.

Change: the change goes to a new change address, unless --changeback sends it
back to an address the payment spends from. Some banks accept payments only
from addresses they have checked, and --changeback keeps the funds at such an
address. When the coins come from several addresses, preparetx lists them and
asks which one gets the change. With a single --spendfrom address, the change
goes back to it without a question. Setting change-back to true in the
configuration file makes --changeback the default. --newchange then sends the
change of one payment to a new change address.

A fee rate above 100 satoshis per byte is more often a typo than a choice, so
preparetx asks before it takes one. --yes takes it without asking, for
scripts.
|],
      args =
        CommandPrepareTx
          <$> ( PrepareTx
                  <$> some paymentArg
                  <*> payingAccountOption
                  <*> feeOption
                  <*> dustOption
                  <*> rcptPayOption
                  <*> minConfOption
                  <*> coinSelectionOption
                  <*> outputFileMaybeOption
                  <*> many coinOption
                  <*> many spendFromOption
                  <*> selectOption
                  <*> highFeeYesOption
                  <*> changeBackOption
              )
    }

-- | The account a payment spends from, which coin control can tell
payingAccountOption :: Args (Maybe Text)
payingAccountOption =
  accountOptionWith "The account to pay from. Needed only when the wallet has more than one, and no --coin or --spendfrom tells it."

-- | Where the change of preparetx goes, when the command says
changeBackOption :: Args (Maybe Bool)
changeBackOption =
  optional $
    flag' True (long "changeback" <> help "Send the change back to an address the payment spends from")
      <|> flag' False (long "newchange" <> help "Send the change to a new change address, even when the configuration file sets change-back")

-- | A recipient of preparetx, ADDRESS:AMOUNT
paymentArg :: Args Text
paymentArg =
  strArgument $
    metavar "ADDRESS:AMOUNT"
      <> help
        [r|
A recipient and the amount it gets. Repeat for several recipients. The amount
is in bitcoins, or in the --unit given, unless it ends with btc, bit, bits, sat
or sats. ADDRESS:0.00001, ADDRESS:10bits and ADDRESS:1000sat pay the same.
Bitcoins take up to 8 decimals, bits up to 2, and satoshis none. Separators
such as 1'000 are refused. A Bitcoin Cash address can keep its prefix, as in
bitcoincash:qp...:0.001.
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
account. --spenddust or --leavedust answers in advance, for scripts.

--coin and --spendfrom send only the coins chosen, every one that can be
spent, dust included. --spenddust and --leavedust do not apply then.

The answer lists every coin the transaction leaves in the account, with the
reason: dust, locked by you or by a pending transaction, not spendable yet, or
not chosen with --coin or --spendfrom.

A fee rate above 100 satoshis per byte is more often a typo than a choice, so
preparesendall asks before it takes one. --yes takes it without asking, for
scripts.
|],
      args =
        CommandPrepareSendAll
          <$> ( PrepareSendAll
                  <$> some sendAllRecipientArg
                  <*> payingAccountOption
                  <*> feeOption
                  <*> dustOption
                  <*> minConfOption
                  <*> outputFileMaybeOption
                  <*> many coinOption
                  <*> many spendFromOption
                  <*> dustCoinsOption
                  <*> highFeeYesOption
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
    flag' SpendDust (long "spenddust" <> help "Spend the coins worth less than their fee too, without asking")
      <|> flag' LeaveDust (long "leavedust" <> help "Leave the coins worth less than their fee in the account, without asking")

-- | An address of the account whose coins to spend
spendFromOption :: Args Text
spendFromOption =
  strOption $
    long "spendfrom"
      <> metavar "ADDRESS"
      <> help "Coin control: spend the coins at this address of the account. Repeat for several addresses."

-- | Whether the strategy selects among the coins of coin control, instead
-- of every one of them being spent
selectOption :: Args Bool
selectOption =
  switch $
    long "select"
      <> help "Coin control: spend only what the payment needs among the coins of --coin and --spendfrom, as --strategy selects them, instead of every one of them"

-- | A coin to spend, as coins lists it
coinOption :: Args OutPoint
coinOption = coinOptionWith "Coin control: spend this coin, as `coins` lists it. Repeat for several coins."

-- | A coin to sweep, by its outpoint
sweepCoinOption :: Args OutPoint
sweepCoinOption = coinOptionWith "A coin to sweep: the id of its transaction, a colon, and its index among the outputs. Repeat for several coins."

-- | The --coin option, with the help of the command about it
coinOptionWith :: String -> Args OutPoint
coinOptionWith what =
  option coinReader $
    short 'c'
      <> long "coin"
      <> metavar "TXHASH:INDEX"
      <> help what

-- | The fee rate, when given: a number of satoshis per byte, or a level
-- of the rates recent blocks paid
feeOption :: Args (Maybe FeeRateChoice)
feeOption =
  optional . option (eitherReader readFeeRateChoice) $
    short 'f'
      <> long "feerate"
      <> metavar "RATE"
      <> completeWith ["low", "median", "high"]
      <> help "The fee rate in satoshis per byte, or low, median or high for that rate of recent blocks, as `fees` shows them. Without it, hw shows those rates and asks, median by default. A rate above 100 needs a confirmation."

-- | Whether preparetx and preparesweep take a high fee rate without asking
highFeeYesOption :: Args Bool
highFeeYesOption = yesOption "Take a fee rate above 100 satoshis per byte without asking"

dustOption :: Args Natural
dustOption =
  option (maybeReader $ readNatural . cs) $
    short 'd'
      <> long "dust"
      <> metavar "SATOSHIS"
      <> value 546
      <> showDefault
      <> help "The smallest output, in satoshis. A smaller payment is refused, and smaller change goes to the fee."

rcptPayOption :: Args FeePayer
rcptPayOption =
  flag SenderPays RecipientsPay $
    short 'r'
      <> long "recipientpay"
      <> help "The recipients pay the fee, in equal shares taken from their amounts"

minConfOption :: Args Natural
minConfOption =
  option (maybeReader $ readNatural . cs) $
    short 'm'
      <> long "minconf"
      <> metavar "INT"
      <> value 0
      <> showDefault
      <> help "The confirmations a coin needs to be spent. At 0, the unconfirmed change of the wallet's own transactions is spent, but no unconfirmed payment from others."

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
now and later, as Bitcoin Core does. random-draw spends coins in random order.
largest-first spends the fewest coins, for the lowest fee now. smallest-first
spends the smallest coins first, to consolidate them. With --coin or
--spendfrom, every chosen coin is spent and the strategy does not apply,
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
      <> action "file"
      <> help "Write the transaction to this file. It must not exist yet."

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
`sync` retires it once the network lists it. The newest come first, each with
its account, its nosigHash and when it was created. The other commands name a
pending transaction by its nosigHash.

A transaction marked conflicted spends a coin that another transaction has
spent, or that is gone, as the server said at the last sync. It cannot be sent
while that lasts. Delete it with deletetx, which frees its coins.
|],
      args =
        CommandPendingTxs
          <$> onlyAccountOption
          <*> (Page <$> limitOption <*> offsetOption)
    }

{- reviewtx -}

reviewTxCommand :: CommandSpec Command
reviewTxCommand =
  CommandSpec
    { name = "reviewtx",
      offline = False,
      summary = "Show the transaction in a file",
      details =
        [r|
reviewtx shows a transaction file as its account sees it, and changes nothing
in the wallet. The file comes from preparetx --output, exporttx, signtx or
preparesweep. Use it to check a transaction before you sign it, or before you
send it. A transaction that involves no account, such as a sweep to addresses
outside the wallet, is shown on the network its file names.
|],
      args =
        CommandReviewTx
          <$> txAccountOption
          <*> fileArgument "The transaction file to show"
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
          <$> nosigHashArg
          <*> fileArgument "The file to write. It must not exist yet."
    }

importExportTxDetails :: String
importExportTxDetails =
  [r|
exporttx writes a pending transaction to a file, signed or not, to carry it to
an offline computer. The transaction is named by its nosigHash, a hash of the
transaction without its signatures. It stays the same once the transaction is
signed. The file names the network of the transaction.

importtx adds the transaction of a file to the pending transactions of its
account, and locks the coins it spends. When the wallet holds it already, the
file takes its place, as when an offline computer returns it signed. The
signatures are checked first.

sendtx --input sends a signed file without importtx.
|]

nosigHashArg :: Args TxHash
nosigHashArg =
  argument (maybeReader $ hexToTxHash . cs) $
    metavar "NOSIGHASH"
      <> help "The nosigHash of a pending transaction, as pendingtxs shows it"

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

A transaction sent with sendtx can be deleted only once the server no longer
knows it. That happens when the nodes dropped it from their mempools, or never
accepted it. While the server lists it, it may still confirm, and the next
`sync` retires it.

Delete a transaction that pendingtxs marks as conflicted. One of its coins was
spent by another transaction, or is gone, so it can no longer be sent.
|],
      args = CommandDeleteTx <$> nosigHashArg
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
transaction. It asks again while they do not match the account.

Give a pending transaction by its NOSIGHASH. The signed transaction then takes
its place in the wallet. Or give a transaction file with --input, on an
offline computer for example. The signed transaction then goes to the --output
file. Without --output, hw offers to replace the --input file, since the
unsigned file is of no use once signed. It asks this before the mnemonic.

Signing sends nothing. Check the transaction with reviewtx or pendingtxs.
Then send it with sendtx, or delete it with deletetx.
|],
      args =
        CommandSignTx
          <$> signingOptions
          <*> mnemonicPartsOption
    }

inputFileMaybeOption :: Args (Maybe FilePath)
inputFileMaybeOption =
  optional . strOption $
    short 'i'
      <> long "input"
      <> metavar "FILENAME"
      <> action "file"
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
A coin is an unspent output of a transaction, written TXHASH:INDEX. The
unconfirmed coins come first, then the newest.

A coin is spendable when the wallet can spend it now. Nothing locks it, a
coinbase coin has 100 confirmations, and an unconfirmed coin is the change of
a transaction of the wallet. The other coins become spendable once they
confirm, or for a coinbase, once it matures.

A pending transaction locks the coins it spends, until the network lists it or
deletetx deletes it. You can also lock a coin yourself with lockcoin, until
unlockcoin.
|],
      args =
        CommandCoins
          <$> accountOption
          <*> lockedOption
          <*> (Page <$> limitOption <*> offsetOption)
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
|],
      args = CommandLockCoin <$> coinArg
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
|],
      args = CommandUnlockCoin <$> coinArg
    }

-- | A coin of the wallet, as coins lists it
coinArg :: Args OutPoint
coinArg =
  argument coinReader $
    metavar "TXHASH:INDEX"
      <> help "The coin, as `coins` lists it: the id of its transaction, a colon, and its index among the outputs"

-- | A coin as coins lists it, and as a block explorer often writes it: the
-- id of its transaction, a colon, and its index among the outputs
coinReader :: ReadM OutPoint
coinReader = eitherReader $ \s -> maybe (Left (notACoin s)) Right (readCoin s)
  where
    readCoin s = case break (== ':') s of
      (hash, ':' : index) -> OutPoint <$> hexToTxHash (cs hash) <*> readOutputIndex index
      _ -> Nothing
    notACoin s = "The coin " <> s <> " is not valid: write it TXHASH:INDEX, as coins lists it"

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
the account. Another transaction of the account may spend the change of the
stuck one. Replacing the stuck one would remove it too, so hw offers to
replace both with one transaction that pays the recipients of both.

The replacement is a pending transaction. Sign it with signtx and send it with
sendtx. sendtx then drops the transactions it replaces from the pending
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
|],
      args =
        CommandBumpFee
          <$> ( BumpFee
                  <$> bumpedArg
                  <*> txAccountOption
                  <*> replacementFeeOption
                  <*> dustOption
                  <*> outputFileMaybeOption
                  <*> yesOption "Answer yes to every question: a fee rate above 100 satoshis per byte, adding coins, and replacing the transactions that spend the change"
              )
    }

-- | The transaction bumpfee replaces
bumpedArg :: Args TxHash
bumpedArg =
  argument (maybeReader $ hexToTxHash . cs) $
    metavar "TXID"
      <> help "The id of the transaction to replace, as txs or pendingtxs shows it"

-- | The fee rate of a replacement, when given, as for preparetx: it must be
-- above the rate of the transaction it replaces
replacementFeeOption :: Args (Maybe FeeRateChoice)
replacementFeeOption =
  optional . option (eitherReader readFeeRateChoice) $
    short 'f'
      <> long "feerate"
      <> metavar "RATE"
      <> completeWith ["low", "median", "high"]
      <> help "The fee rate of the replacement in satoshis per byte, or low, median or high for that rate of recent blocks, as `fees` shows them. It must be above the rate of the transaction replaced. Without it, hw shows those rates and asks. A rate above 100 needs a confirmation."

{- sendtx -}

sendTxCommand :: CommandSpec Command
sendTxCommand =
  CommandSpec
    { name = "sendtx",
      offline = False,
      summary = "Send a signed transaction to the network",
      details =
        [r|
Sending cannot be undone. sendtx shows the transaction, as reviewtx does, and
sends it only if you answer yes. --yes sends it without showing it or asking,
for scripts.

The server passes the transaction to the nodes without checking it. sendtx
then waits up to ten seconds for the server to see it in its mempool. That is
the only sign that the network accepted it. If the server does not see it,
nothing is recorded, and sendtx says so. The network may have refused the
transaction, for a fee below what the nodes relay for example. Or it may just
be slow. Run sendtx again later.

Give a pending transaction by its NOSIGHASH. Once the server sees it, it is
marked as sent. `sync` then sends it again whenever the server loses it, and
retires it once the network lists it. A transaction the network refuses for
good is sent again on every sync, until you delete it with deletetx.

Give a signed file with --input, such as the one an offline computer returns.
When the wallet holds its transaction as pending, the file takes its place and
is sent as above. Otherwise, a sweep of outside coins for example, hw checks
the transaction and sends it, and changes nothing in the wallet.

A file names its network. A file from an earlier hw names none. It can be
sent only when the wallet holds its transaction. Otherwise, prepare the
transaction again.
|],
      args =
        CommandSendTx
          <$> optional nosigHashArg
          <*> inputFileMaybeOption
          <*> yesOption "Send without showing the transaction and asking first"
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
accounts.

It first looks for used addresses past the ones each account has, as far as
the gap of the configuration file, 20 by default. Someone paying to the
account's public key, or another wallet of the same seed, may have used them.
`sync` adds each used address it finds, with the ones before it. It keeps
looking while it finds more, so the first sync of an account finds every
address used so far. It misses an address that comes after more unused ones in
a row than the gap. Raise the gap for one sync to look further. The same gap
limits how many unused addresses hw `receive` gives out.

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
|],
      args = CommandSync <$> onlyAccountOption <*> fullOption
    }

fullOption :: Args Bool
fullOption =
  switch $
    long "full"
      <> help
        [r|
Download all the account data again, instead of only what changed. A full
sync also marks as dead the transactions the server no longer lists. It is
rarely needed, since a reorganization of the chain triggers one by itself.
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

{- version -}

versionCommand :: CommandSpec Command
versionCommand =
  CommandSpec
    { name = "version",
      offline = False,
      summary = "Show the version of hw and of its database format",
      details = "",
      args = pure CommandVersion
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
three rates, in satoshis per byte:

  low      a quarter of the bytes in those blocks paid less
  median   half of the bytes paid less
  high     nine tenths of the bytes paid less

The high rate almost surely gets into the next block. The low rate usually
does when the network is calm. The age of the latest block tells how fresh
the rates are. A block mined an hour ago means a slow network, and rates that
may be too low.

The rates show what got into blocks, not how long transactions waited. After
a quiet spell, a sudden backlog makes them too low for a while. bumpfee then
helps.

preparetx, preparesendall, bumpfee and preparesweep show the same rates, and
ask which to take when --feerate is not given. --feerate low, median or high
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
signmsg asks for the mnemonic of the account of the ADDRESS, and signs the
message with the key of that address. The signature uses the Bitcoin Signed
Message format, the legacy format of BIP 322. Bitcoin Core and other wallets
can verify it.

The message is signed exactly as given. hw warns about spaces or line breaks
at its start or end. Copying a message easily changes them, and the signature
would then fail to verify.

Save the output, the armored block or the --json answer, to a file. verifymsg
--input can then check it.
|],
      args =
        CommandSignMsg
          <$> (SignMsg <$> signAddressArg <*> signMsgArg <*> mnemonicPartsOption)
    }

signAddressArg :: Args Text
signAddressArg =
  strArgument $
    metavar "ADDRESS"
      <> help "The address whose key signs. It must be an address of this wallet."

signMsgArg :: Args Text
signMsgArg =
  strArgument $
    metavar "MESSAGE"
      <> help "The message to sign. Quote it if it has spaces."

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
that signmsg, Bitcoin Core or another wallet gave. The address can be anyone's.
It must be a P2PKH address, the only kind this format signs with. The
--network only tells hw how to read it.

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
      <> action "file"
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
example, to new addresses. preparesweep takes every coin at the --sweepfrom
addresses and at those of an --addressfile file, every coin at the addresses
of the --prvkeys keys, and the --coin coins. Give any of them. The file holds
addresses separated by spaces or line breaks, as a --prvkeys file holds keys:
an old wallet may have more addresses than a command line takes.

The private keys are needed only to sign, with signsweep, so they can stay on
another computer. Without them, hw counts every key as compressed, so the fee
of a coin of an uncompressed key comes out a little low.

Give each destination with --sweepto, as ADDRESS or ADDRESS:AMOUNT, written as
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

A fee rate above 100 satoshis per byte is more often a typo than a choice, so
preparesweep asks before it takes one. It asks before the private keys are
typed. --yes takes it without asking, for scripts.
|],
      args =
        CommandPrepareSweep
          <$> ( PrepareSweep
                  <$> accountOptionWith "The account the sweep pays. Needed only when the --sweepto addresses are in several accounts."
                  <*> sweepNetworkOption
                  <*> prvKeysMaybeOption
                  <*> many sweepFromOption
                  <*> addressFileOption
                  <*> some sweepToOption -- some: at least one
                  <*> many sweepCoinOption
                  <*> outputFileMaybeOption
                  <*> feeOption
                  <*> dustOption
                  <*> highFeeYesOption
              )
    }

sweepFromOption :: Args Text
sweepFromOption =
  strOption $
    short 'w'
      <> long "sweepfrom"
      <> metavar "ADDRESS"
      <> help "An address whose coins to sweep. Repeat for several addresses."

addressFileOption :: Args (Maybe FilePath)
addressFileOption =
  optional . strOption $
    long "addressfile"
      <> metavar "FILENAME"
      <> action "file"
      <> help "A file of addresses whose coins to sweep, separated by spaces or line breaks"

sweepToOption :: Args Text
sweepToOption =
  strOption $
    short 't'
      <> long "sweepto"
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
wallet's, so signing needs their private keys. Give them in a --prvkeys file.
Or type them on the terminal, where they are not shown, with --prvkeys - or
without --prvkeys.

Give a pending sweep by its NOSIGHASH. The signed sweep then takes its place
in the wallet. Or give a sweep file with --input. The signed sweep then goes
to the --output file. Without --output, hw offers to replace the --input file.
It asks this before the keys.

A sweep that pays no account of this wallet needs no account here either. It
is signed on the network its file names. A file from an earlier hw names
none, and must be prepared again.
|],
      args =
        CommandSignSweep
          <$> signingOptions
          <*> prvKeysMaybeOption
    }

-- | The private keys: a file, or - for the terminal
prvKeysMaybeOption :: Args (Maybe KeySource)
prvKeysMaybeOption =
  optional . option (eitherReader source) $
    short 'k'
      <> long "prvkeys"
      <> metavar "FILENAME"
      <> action "file"
      <> help "A file of private keys in WIF, minikey or hex format, separated by spaces or line breaks. Give - to type them on the terminal instead."
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
      <> action "file"
      <> help desc
