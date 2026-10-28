{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Haskoin.Wallet.Parser where

import Data.List (find, intercalate)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32)
import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit (..), readUnit)
import Haskoin.Wallet.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Keys (KeySource (..))
import Haskoin.Wallet.TxSize (defaultFeeRate)
import Haskoin.Wallet.Util (Page (Page), readNatural)
import Numeric.Natural (Natural)
import Options.Applicative
import Options.Applicative.Help.Pretty
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
  | CommandSignMsg !SignMsg
  | -- | The account, whether the locked coins only, and the page
    CommandCoins !(Maybe Text) !Bool !Page
  | CommandLockCoin !OutPoint
  | CommandUnlockCoin !OutPoint
  | -- | The nosigHash of a pending transaction, or the file of a signed
    -- one, and whether to send it without asking
    CommandSendTx !(Maybe TxHash) !(Maybe FilePath) !Bool
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
  { -- | The addresses paid, each with its amount as typed
    recipients :: ![(Text, Text)],
    account :: !(Maybe Text),
    -- | The fee rate, in satoshis per byte
    feeByte :: !Natural,
    -- | The dust limit, in satoshis
    dust :: !Natural,
    feePayer :: !FeePayer,
    -- | The confirmations a coin needs to be spent
    minConf :: !Natural,
    strategy :: !CoinSelection,
    output :: !(Maybe FilePath),
    -- | The coins to spend, instead of choosing them
    coins :: ![OutPoint],
    -- | Whether a fee rate above 100 satoshis per byte is taken without
    -- asking (--yes)
    yes :: !Bool
  }
  deriving (Eq, Show)

-- | The options of preparesendall
data PrepareSendAll = PrepareSendAll
  { -- | The recipients as typed: an address, or an address and its amount,
    -- ADDRESS:AMOUNT
    recipients :: ![Text],
    account :: !(Maybe Text),
    -- | The fee rate, in satoshis per byte
    feeByte :: !Natural,
    -- | The dust limit, in satoshis
    dust :: !Natural,
    -- | The confirmations a coin needs to be sent
    minConf :: !Natural,
    output :: !(Maybe FilePath),
    -- | The coins to send, instead of all of them
    coins :: ![OutPoint],
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

-- | The options of preparesweep
data PrepareSweep = PrepareSweep
  { account :: !(Maybe Text),
    -- | The network of a sweep that pays no account of the wallet
    network :: !(Maybe Network),
    -- | The private keys of the addresses swept, if at hand
    keys :: !(Maybe KeySource),
    sweepFrom :: ![Text],
    sweepTo :: ![Text],
    -- | The coins swept, by outpoint
    coins :: ![OutPoint],
    output :: !(Maybe FilePath),
    -- | The fee rate, in satoshis per byte
    feeByte :: !Natural,
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
signingOptions :: Parser Signing
signingOptions =
  Signing
    <$> accountOption
    <*> optional nosigHashArg
    <*> inputFileMaybeOption
    <*> outputFileMaybeOption

parserMain :: IO (Command, AmountUnit, Bool)
parserMain =
  customExecParser
    (prefs $ showHelpOnEmpty <> helpIndent 25)
    programParser

programParser :: ParserInfo (Command, AmountUnit, Bool)
programParser = do
  let cmd = (,,) <$> (commandParser <**> helper) <*> unitOption <*> jsonOption
  info cmd $
    fullDesc
      <> progDesc
        [r|
hw is a BIP-44 command-line wallet for bitcoin and bitcoin-cash. It allows
sensitive commands (!) to be run on a separate offline computer. For more
information on a command, type "hw COMMAND --help".
|]

jsonOption :: Parser Bool
jsonOption =
  switch $
    short 'j'
      <> long "json"
      <> help
        [r|
Display the result as JSON.
|]

unitOption :: Parser AmountUnit
unitOption =
  option (maybeReader (readUnit . cs)) $
    short 'u'
      <> long "unit"
      <> metavar "UNIT"
      <> value UnitBitcoin
      <> completeWith ["btc", "bit", "sat"]
      <> help
        [r|
The unit of the amounts shown, and of the amounts typed without a unit: btc, bit
or sat (default: btc).
|]

commandParser :: Parser Command
commandParser =
  asum
    [ hsubparser $
        mconcat
          [ commandGroup "Mnemonic and account management",
            command "mnemonic" mnemonicParser,
            command "createacc" createAccParser,
            command "recover" recoverParser,
            command "testacc" testAccParser,
            command "renameacc" renameAccParser,
            command "accounts" accountsParser,
            command "sync" syncParser,
            command "deleteacc" deleteAccParser,
            metavar "COMMAND",
            style (const "COMMAND --help")
          ],
      hsubparser $
        mconcat
          [ commandGroup "Address management",
            command "receive" receiveParser,
            command "addrs" addrsParser,
            command "label" labelParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Transaction management",
            command "txs" txsParser,
            command "preparetx" prepareTxParser,
            command "preparesendall" prepareSendAllParser,
            command "pendingtxs" pendingTxsParser,
            command "signtx" signTxParser,
            command "sendtx" sendTxParser,
            command "deletetx" deleteTxParser,
            command "coins" coinsParser,
            command "lockcoin" lockCoinParser,
            command "unlockcoin" unlockCoinParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Import/export commands",
            command "exportacc" exportAccParser,
            command "importacc" importAccParser,
            command "reviewtx" reviewTxParser,
            command "exporttx" exportTxParser,
            command "importtx" importTxParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Backup commands",
            command "backup" backupParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Utilities",
            command "version" versionParser,
            command "signmsg" signMsgParser,
            command "preparesweep" prepareSweepParser,
            command "signsweep" signSweepParser,
            hidden
          ]
    ]

offline :: Doc -> Maybe Doc
offline s = Just $ annotate (color Red) "! " <> s

{- Mnemonic Parser -}

mnemonicParser :: ParserInfo Command
mnemonicParser = do
  let cmd =
        CommandMnemonic
          <$> (NewMnemonic <$> entropyOption <*> diceOption <*> splitInOption)
  info cmd $
    progDescDoc (offline "Generate a mnemonic")
      <> footer
        [r|
Generate a mnemonic from the entropy of the operating system, read directly from
/dev/random where the machine has it. If you use the --dice option, hw then asks
for dice rolls and mixes their entropy with the system entropy. You should ideally run this
command on an offline (air-gapped) computer. The mnemonic will NOT be stored on
disk. It will only be printed to the screen and you should write it down and
keep it in a secure location. Make multiple copies of your mnemonic. Do not
store your mnemonic on a digital medium. If you lose your mnemonic, you lose
your funds. If you choose to use the --split option, the mnemonic will be split
into different pieces such that ALL the pieces will be required for signing.
|]

entropyOption :: Parser Natural
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
Amount of entropy to use in bytes. Valid values are 16, 20, 24, 28 or 32.
|]
  where
    valid = ["16", "20", "24", "28", "32"]
    f s
      | s `elem` valid = fromIntegral <$> readNatural (cs s)
      | otherwise = Nothing

diceOption :: Parser Bool
diceOption =
  switch $
    short 'd'
      <> long "dice"
      <> help
        [r|
Provide additional entropy using 6-sided dice. The entropy will be mixed with
the system entropy. Type the faces as many on a line as you like: they are not
shown, and hw tells how many rolls are left after each line.
|]

splitInOption :: Parser Natural
splitInOption =
  option (eitherReader $ f . cs) $
    short 's'
      <> long "split"
      <> metavar "INT"
      <> value 1
      <> help
        [r|
The mnemonic is split in different pieces and reconstructed using bitwise xor.
All the pieces are required for signing. The ordering of the pieces is not
important. The pieces are made from the system entropy. With --dice, hw asks for
enough rolls to mix with every piece as well, and uses no roll twice.
|]
  where
    f s =
      case readNatural s of
        Just n ->
          if n >= 1 && n <= 12
            then Right n
            else Left "The --split value has to be between 1 and 12"
        Nothing -> Left "Could not parse the --split option"

{- CreateAcc Parser -}

createAccParser :: ParserInfo Command
createAccParser = do
  let cmd =
        CommandCreateAcc
          <$> textArg "Name of the new account"
          <*> networkMaybeOption
          <*> splitInOption
  info cmd $
    progDescDoc (offline "Create a new account")
      <> footer
        [r|
An account corresponds to the BIP-32 extended key derivation
m/44'/coin'/account'. For example, the bitcoin account 0 would be m/44'/0'/0'.
An account is tied to a private key and thus to a mnemonic. The derived account
m/44'/0'/0' would be different given two different mnemonics. To help manage
your accounts in hw, they are identified by a name. The command `createacc` will
ask for a mnemonic and save the derived public key M/44'/coin'/account' on disk,
at the first index of the seed without an account on the network. An account
exported from another wallet comes back with `importacc` at the index of its
key.
The private keys are never stored anywhere while using hw. If you are using an
offline computer, you can then export your account and import it on an online
computer.
|]

-- | The network, when given. Without it, a command that needs one takes the
-- default-network of the configuration file.
networkMaybeOption :: Parser (Maybe Network)
networkMaybeOption = networkOptionWith "Without it, the default-network of the configuration file"

-- | The network of a sweep, when given. A sweep that pays an account of the
-- wallet is on the network of that account. One that pays none needs it.
sweepNetworkOption :: Parser (Maybe Network)
sweepNetworkOption =
  networkOptionWith "Needed by a sweep that pays no account of this wallet: the default-network of the configuration file does not apply to it"

-- | The network, when given, with the help of the command about it
networkOptionWith :: String -> Parser (Maybe Network)
networkOptionWith without =
  optional . option (eitherReader (f . netByName)) $
    short 'n'
      <> long "network"
      <> metavar "TEXT"
      <> completeWith ((.name) <$> allNets)
      <> help ("One of the networks " <> nets <> ". " <> without)
  where
    nets = intercalate ", " ((.name) <$> allNets)
    f :: Maybe Network -> Either String Network
    f Nothing =
      Left $ "Invalid network name. Select one of the following: " <> nets
    f (Just res) = Right res

{- Recover Parser -}

recoverParser :: ParserInfo Command
recoverParser = do
  let cmd = CommandRecover <$> (Recover <$> networkMaybeOption <*> accountGapOption <*> splitInOption)
  info cmd $
    progDescDoc (offline "Recover the accounts of a seed that were paid")
      <> footer
        [r|
`recover` finds the accounts of a seed on a network as BIP44 describes:
account after account from index 0, the receiving addresses of each one are
looked up in the store, within the gap, and an account that was paid is
created, named after the network and its index, btc0 for instance, and synced.
The search ends after a run of accounts without a payment, ten by default. The
standard stops at the first one, but hw creates accounts on an offline signer,
where some may never receive anything, so one empty account is not the end of
the seed here. Give --account-gap to look further or less far. An account the
wallet has already is passed over.

The command needs the mnemonic and the network at once. On an air-gapped
setup, create the accounts on the signer with `createacc`, export them with
`exportacc`, import them on the online computer and `sync` them instead.
|]

accountGapOption :: Parser Natural
accountGapOption =
  option (eitherReader accountGap) $
    long "account-gap"
      <> metavar "INT"
      <> value 10
      <> showDefault
      <> help
        [r|
How many accounts in a row without a payment end the search. One is the rule
of BIP44; more covers accounts created on a signer that were never paid. At
most 100.
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

{- DeleteAcc Parser -}

deleteAccParser :: ParserInfo Command
deleteAccParser = do
  let cmd =
        CommandDeleteAcc
          <$> textArg "Name of the account to delete"
          <*> networkArg
          <*> fullDerivationOption
  info cmd $
    progDesc "Delete an account"
      <> footer
        [r|
Delete an account from the wallet. You must specify the name, the network and
the full derivation path as confirmation to delete an account.
|]

fullDerivationOption :: Parser HardPath
fullDerivationOption =
  argument (maybeReader parseHard) $
    metavar "DERIVPATH"
      <> help
        [r|
Specify the full derivation path of the account to delete. For example:
/44'/0'/3'.
|]

networkArg :: Parser Network
networkArg =
  argument (eitherReader (f . netByName)) $
    metavar "NETWORK"
      <> completeWith ((.name) <$> allNets)
      <> help ("Specify the network of the account to delete: " <> nets)
  where
    nets = intercalate ", " ((.name) <$> allNets)
    f :: Maybe Network -> Either String Network
    f Nothing =
      Left $ "Invalid network name. Select one of the following: " <> nets
    f (Just res) = Right res

{- TestAcc Parser -}

testAccParser :: ParserInfo Command
testAccParser = do
  let cmd =
        CommandTestAcc
          <$> accountOption
          <*> splitInOption
  info cmd $
    progDescDoc (offline "Check the validity of your mnemonic/passphrase")
      <> footer
        [r|
`testacc` will prompt for your mnemonic/passphrase and check that the derived
public key M/44'/coin'/account' matches with the account stored on disk.
|]

accountOption :: Parser (Maybe Text)
accountOption =
  optional . strOption $
    short 'a'
      <> long "account"
      <> metavar "TEXT"
      <> completer (mkCompleter accountCompleter)
      <> help
        [r|
Specify the account name if the wallet has more than one account.
|]

{- ImportAcc Parser -}

importAccParser :: ParserInfo Command
importAccParser = do
  let cmd =
        CommandImportAcc
          <$> fileArgument "Path to the account file"
  info cmd $
    progDesc "Import an account file"
      <> footer importExportAccFooter

importExportAccFooter :: String
importExportAccFooter =
  [r|
When working in an online/offline environment, you can `importacc` a public key
file (on an online computer) that was exported with `exportacc` (on an offline
computer). This will allow the online computer to monitor transactions without
requiring access to the private keys. The file contains the public key
derivation M/44'/coin'/account' along with the account name, the network and,
when the exporting wallet knows it, the fingerprint of the seed: the four bytes
that PSBTs and output descriptors write before a key of the seed, under which
`accounts` groups the accounts of one seed. Importing the file of an account
the wallet already has records that fingerprint.
|]

{- ExportAcc Parser -}

exportAccParser :: ParserInfo Command
exportAccParser = do
  let cmd =
        CommandExportAcc
          <$> accountOption
          <*> fileArgument "File where the account data will be saved"
  info cmd $
    progDesc "Export account data to a file"
      <> footer importExportAccFooter

{- RenameAcc Parser -}

renameAccParser :: ParserInfo Command
renameAccParser = do
  let cmd =
        CommandRenameAcc
          <$> (RenameAcc <$> accountArg "Old account name" <*> textArg "New account name")
  info cmd $ progDesc "Rename an account"

accountArg :: String -> Parser Text
accountArg desc =
  argument str $
    metavar "TEXT"
      <> completer (mkCompleter accountCompleter)
      <> help desc

accountCompleter :: String -> IO [String]
accountCompleter _ = return []

{- Accounts Parser -}

accountsParser :: ParserInfo Command
accountsParser = do
  let cmd = CommandAccounts <$> accountOption
  info cmd $
    progDesc "Display account information"
      <> footer
        [r|
The confirmed and unconfirmed balances are those the store reported at the last
`sync`. The spendable balance is what the wallet can spend now: the coins
neither a pending transaction nor the user locked, a coinbase once it has 100
confirmations, and an unconfirmed coin only when it is the change of a
transaction of the wallet. `preparetx` spends all of it, unless `--minconf`
asks for confirmations.
|]

{- Receive Parser -}

receiveParser :: ParserInfo Command
receiveParser = do
  let cmd =
        CommandReceive <$> (Receive <$> accountOption <*> labelOption)
  info cmd $
    progDesc "Get a new address for receiving a payment"
      <> footer
        [r|
There are two types of addresses in a BIP-44 wallet: internal and external addresses.
Internal addresses are managed by the wallet, which pays the change of its
transactions to them; `addrs --internal` lists them. The `receive` command
produces external addresses that are meant for receiving payments. Internal addresses
are derived under the BIP-32 path M/44'/coin'/account'/1/address. External addresses
use the path M/44'/coin'/account'/0/address.

Every call gives a new address, up to the gap of BIP-44: once the last twenty
addresses all await a payment, `receive` refuses and one of them is to be
reused, as listed by `addrs`.
|]

labelOption :: Parser (Maybe Text)
labelOption =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "TEXT"
      <> help "Specify a label for the address"

{- Addrs Parser -}

addrsParser :: ParserInfo Command
addrsParser = do
  let cmd =
        CommandAddrs
          <$> accountOption
          <*> internalOption
          <*> (Page <$> limitOption <*> offsetOption)
  info cmd $
    progDesc "List the receiving addresses of an account, or its change addresses"
      <> footer
        [r|
Each address shows its transactions, what it received, and what it can spend
now, by the rules `accounts` describes.
|]

internalOption :: Parser Bool
internalOption =
  switch $
    long "internal"
      <> help "List the internal addresses, where the change of the account's transactions goes, instead of the receiving ones"

offsetOption :: Parser Natural
offsetOption =
  option (maybeReader $ readNatural . cs) $
    short 'o'
      <> long "offset"
      <> metavar "INT"
      <> value 0
      <> showDefault
      <> help "Offset the result set"

limitOption :: Parser Natural
limitOption =
  option (maybeReader $ readNatural . cs) $
    short 'l'
      <> long "limit"
      <> metavar "INT"
      <> value 5
      <> showDefault
      <> help
        [r|
Limit the result set. If the result set is very large, you can specify the
--limit and --offset options to view the required data. For example, to skip the
first 20 values and then display the following 10, use --limit=10 and
--offset=20. A limit of 0 lists every row.
|]

{- Label Parser -}

labelParser :: ParserInfo Command
labelParser = do
  let cmd =
        CommandLabel
          <$> accountOption
          <*> addrIndexArg
          <*> textArg "The new label for the address"
  info cmd $ progDesc "Set the label of a receiving address, by its index in addrs"

addrIndexArg :: Parser Int
addrIndexArg =
  argument (maybeReader readAddrIndex) $
    metavar "INT"
      <> help
        [r|
The index of the receiving address, as `addrs` lists it: the first address has
the index 0. The change addresses, those of `addrs --internal`, have no label.
|]
  where
    -- An address is derived at a soft index of BIP32, below 2^31. A larger
    -- number names no address, rather than wrapping around to another one.
    readAddrIndex :: String -> Maybe Int
    readAddrIndex s = case readNatural (cs s) of
      Just n | n < 2 ^ (31 :: Int) -> Just (fromIntegral n)
      _ -> Nothing

{- Txs Parser -}

txsParser :: ParserInfo Command
txsParser = do
  let cmd =
        CommandTxs
          <$> accountOption
          <*> (Page <$> limitOption <*> offsetOption)
  info cmd $ progDesc "Display the transactions of an account"

{- PrepareTx Parser -}

prepareTxParser :: ParserInfo Command
prepareTxParser = do
  let options =
        PrepareTx
          <$> some recipientArg
          <*> accountOption
          <*> feeOption
          <*> dustOption
          <*> rcptPayOption
          <*> minConfOption
          <*> coinSelectionOption
          <*> outputFileMaybeOption
          <*> many coinOption
          <*> highFeeYesOption
  info (CommandPrepareTx <$> options) $
    progDesc "Prepare an unsigned transaction for making a payment"
      <> footer
        [r|
In hw, sending a transaction happens in separate steps. First, an unsigned
transaction is prepared using `preparetx`. No mnemonic or private keys are
required for this step and `preparetx` can be run on an online computer. The new
transaction will be stored in the local wallet database as a "pending"
transaction but it can also be exported as a file using the --output option. The
pending transactions can be inspected either with `reviewtx` or `pendingtxs`.
The new transaction is not signed. The next step will be to sign it. Any coins
spent by it will be locked to prevent spending them twice. If you change your
mind about sending this transaction, you can call `deletetx` to remove it and
free up the locked coins. All the pending transactions are referenced by their
noSigHash (a hash of the transaction without its signatures).

A fee rate above 100 satoshis per byte, ten times the default, is more often a
typo than a choice, so `preparetx` asks before it takes one. --yes takes it
without asking, for a script.
|]

recipientArg :: Parser (Text, Text)
recipientArg = (,) <$> addressArg <*> amountArg

addressArg :: Parser Text
addressArg =
  strArgument $
    metavar "ADDRESS"
      <> help
        [r|
Recipient address. You can provide multiple "ADDRESS AMOUNT" pairs. 
|]

amountArg :: Parser Text
amountArg =
  strArgument $
    metavar "AMOUNT"
      <> help
        [r|
Recipient amount, in bitcoins unless it ends with a unit (btc, bit, bits, sat or
sats) or the --unit option says otherwise. For example, 0.00001, 0.00001btc,
10bits and 1000sat are the same amount. Bitcoins have up to 8 decimals, bits up
to 2, and satoshis are whole. Separators such as 1'000 are refused.
|]

{- PrepareSendAll Parser -}

prepareSendAllParser :: ParserInfo Command
prepareSendAllParser = do
  let options =
        PrepareSendAll
          <$> some sendAllRecipientArg
          <*> accountOption
          <*> feeOption
          <*> dustOption
          <*> minConfOption
          <*> outputFileMaybeOption
          <*> many coinOption
          <*> dustCoinsOption
          <*> highFeeYesOption
  info (CommandPrepareSendAll <$> options) $
    progDesc "Prepare an unsigned transaction sending all the coins of an account"
      <> footer
        [r|
`preparesendall` prepares a transaction that sends every coin the account can
spend now to the addresses given, with no change: the spendable balance that
`accounts` shows. Like `preparetx`, it needs no mnemonic, and the transaction
waits among the pending transactions to be signed with `signtx`.

An address can be given an amount after a colon, ADDRESS:AMOUNT, which it gets
as typed. The addresses without an amount share what remains in equal parts,
and they pay the fee: give at least one. When what remains does not cover the
fee, nothing is prepared: a fixed amount is never cut. A Bitcoin Cash address
can keep its prefix, as in bitcoincash:qp...:0.001.

A coin worth no more than the fee to spend it is a dust coin: spending it
lowers what the addresses get. Whether a coin is dust depends on the fee rate.
`preparesendall` lists the dust coins with what spending them or leaving them
means, and asks whether to spend them too. Any answer but yes leaves them in
the account. --spenddust or --leavedust answers for a script.

--coin sends the coins given, all of them, dust included, and only them. The
answer lists every coin the transaction leaves in the account, after it, each
with why: worth less than its fee, locked by you or by a pending transaction,
not spendable yet, or not given with --coin.

A fee rate above 100 satoshis per byte, ten times the default, is more often a
typo than a choice, so `preparesendall` asks before it takes one. --yes takes
it without asking, for a script.
|]

sendAllRecipientArg :: Parser Text
sendAllRecipientArg =
  strArgument $
    metavar "ADDRESS[:AMOUNT]"
      <> help
        [r|
An address that shares what remains, or an address and the amount it gets,
ADDRESS:AMOUNT. Give at least one address without an amount.
|]

-- | What to do with the coins worth less than their fee, without asking
dustCoinsOption :: Parser (Maybe DustCoins)
dustCoinsOption =
  optional $
    flag' SpendDust (long "spenddust" <> help "Spend the coins worth less than their fee too, without asking")
      <|> flag' LeaveDust (long "leavedust" <> help "Leave the coins worth less than their fee in the account, without asking")

-- | A coin to spend, as coins lists it
coinOption :: Parser OutPoint
coinOption =
  option coinReader $
    short 'c'
      <> long "coin"
      <> metavar "TXHASH:INDEX"
      <> help "Coin control: specify coins to spend"

feeOption :: Parser Natural
feeOption =
  option (maybeReader $ readNatural . cs) $
    short 'f'
      <> long "feerate"
      <> metavar "SAT/BYTE"
      <> value defaultFeeRate
      <> showDefault
      <> help "The fee rate, in satoshis per byte. hw asks before it takes one above 100."

-- | Whether preparetx and preparesweep take a high fee rate without asking
highFeeYesOption :: Parser Bool
highFeeYesOption = yesOption "Take a fee rate above 100 satoshis per byte without asking"

dustOption :: Parser Natural
dustOption =
  option (maybeReader $ readNatural . cs) $
    short 'd'
      <> long "dust"
      <> metavar "INT"
      <> value 546
      <> showDefault
      <> help "Amount (in satoshi) below which an output is considered dust"

rcptPayOption :: Parser FeePayer
rcptPayOption =
  flag SenderPays RecipientsPay $
    short 'r'
      <> long "recipientpay"
      <> help "The transaction fee will be deducted from the recipient amounts"

minConfOption :: Parser Natural
minConfOption =
  option (maybeReader $ readNatural . cs) $
    short 'm'
      <> long "minconf"
      <> metavar "INT"
      <> value 0
      <> showDefault
      <> help "Minimum number of confirmations required to spend a coin; an unconfirmed coin must be change from a transaction of the wallet"

coinSelectionOption :: Parser CoinSelection
coinSelectionOption =
  option (eitherReader f) $
    long "strategy"
      <> metavar "STRATEGY"
      <> value LeastWaste
      <> showDefaultWith strategyName
      <> completeWith (strategyName <$> strategies)
      <> help
        [r|
How the coins to spend are chosen. least-waste keeps, among the strategies
below and an exact match without change, the coins that waste the least fee
now and later, like Bitcoin Core. random-draw spends coins in random order.
largest-first spends the fewest coins, for the smallest fee. smallest-first
spends the smallest coins first, to consolidate them.
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

outputFileMaybeOption :: Parser (Maybe FilePath)
outputFileMaybeOption =
  optional . strOption $
    short 'o'
      <> long "output"
      <> metavar "FILENAME"
      <> action "file"
      <> help "Write the result to this file"

{- PendingTxs Parser -}

pendingTxsParser :: ParserInfo Command
pendingTxsParser = do
  let cmd =
        CommandPendingTxs
          <$> accountOption
          <*> (Page <$> limitOption <*> offsetOption)
  info cmd $
    progDesc "Display the pending transactions of an account"
      <> footer
        [r|
Pending transactions have been created with the `preparetx` command. They are
either unsigned and waiting to be signed, or signed and waiting to be sent to
the network, or sent and waiting for the network to list them, at which point
`sync` retires them. One reported as conflicted spends a coin that another
transaction has spent, or that is gone, as the store answered at the last
`sync`: it cannot be sent while that lasts, and should be deleted.
|]

{- ReviewTx Parser -}

reviewTxParser :: ParserInfo Command
reviewTxParser = do
  let cmd =
        CommandReviewTx
          <$> accountOption
          <*> fileArgument "Path to the transaction file to review"
  info cmd $
    progDesc "Review a transaction file"
      <> footer
        [r|
Review a pending transaction file that has been created with the --output option
of the `preparetx` command or the `exporttx` command. You can review a
transaction file this way before signing it on an offline computer for example.
|]

{- ExportTx Parser -}

exportTxParser :: ParserInfo Command
exportTxParser = do
  let cmd =
        CommandExportTx
          <$> nosigHashArg
          <*> fileArgument "File where the transaction data will be saved"
  info cmd $
    progDesc "Export pending transaction data to a file"
      <> footer importExportTxFooter

importExportTxFooter :: String
importExportTxFooter =
  [r|
A transaction that has been prepared with `preparetx` can be exported to a file
so that it can be signed on an offline computer. The transaction is identified
by its nosigHash (a hash of the transaction without its signatures) as this
identifier doesn't change when the transaction is unsigned or signed. Once
signed, you can `importtx` the transaction back into the computer that
prepared the transaction. 
|]

nosigHashArg :: Parser TxHash
nosigHashArg =
  argument (maybeReader $ hexToTxHash . cs) $
    metavar "NOSIGHASH"
      <> help "The nosigHash of the transaction"

{- ImportTx Parser -}

importTxParser :: ParserInfo Command
importTxParser = do
  let cmd =
        CommandImportTx
          <$> accountOption
          <*> fileArgument "Path to the transaction file to import"
  info cmd $
    progDesc "Import a pending transaction"
      <> footer importExportTxFooter

{- DeleteTx Parser -}

deleteTxParser :: ParserInfo Command
deleteTxParser = do
  let cmd = CommandDeleteTx <$> nosigHashArg
  info cmd $
    progDesc "Delete a pending transaction"
      <> footer
        [r|
A transaction that has been prepared with `preparetx` locks the coins it
spends, so that no other transaction spends them. `deletetx` removes a pending
transaction from the database and frees its coins. A transaction that was sent
with `sendtx` can only be deleted, abandoned, when the store no longer knows
it: dropped from the nodes' mempools, or never accepted. One that the store
still lists may yet confirm, and `sync` retires it once it does. A
transaction `pendingtxs` reports as conflicted, one of its coins having been
spent by another transaction or gone, can no longer be sent and should be
deleted.
|]

{- SignTx Parser -}

signTxParser :: ParserInfo Command
signTxParser = do
  let cmd =
        CommandSignTx
          <$> signingOptions
          <*> splitInOption
  info cmd $
    progDescDoc (offline "Sign a transaction")
      <> footer
        [r|
The next step after preparing a transaction is to sign it. This step can happen
on an offline computer if you export the unsigned transaction to a file. If the
transaction is stored in the wallet, it can be signed by using its nosigHash.
This will sign and replace the transaction in-place. Otherwise, give the --input
file of the unsigned transaction, and the --output file to write the signed one
to. Without --output, hw asks to replace the --input file with the signed
transaction, before it asks for the mnemonic: the unsigned file is of no use
once the transaction is signed. The transaction still hasn't been uploaded to
the network. You may inspect it using either the `reviewtx` or the `pendingtxs`
command. The transaction can then be sent to the network with `sendtx` or
deleted with `deletetx`.
|]

inputFileMaybeOption :: Parser (Maybe FilePath)
inputFileMaybeOption =
  optional . strOption $
    short 'i'
      <> long "input"
      <> metavar "FILENAME"
      <> action "file"
      <> help "Read the input from this file"

{- Coins Parser -}

coinsParser :: ParserInfo Command
coinsParser = do
  let cmd =
        CommandCoins
          <$> accountOption
          <*> lockedOption
          <*> (Page <$> limitOption <*> offsetOption)
  info cmd $
    progDesc "List the account coins (unspent outputs)"
      <> footer
        [r|
These are all the coins in the account. A coin is locked by a pending
transaction that spends it, from `preparetx` until the network lists that
transaction, or until `deletetx` deletes it and frees the coin. A coin can also
be locked by the user with `lockcoin`, until `unlockcoin`. A coin is spendable
when the wallet can spend it now: nothing locks it, a coinbase has 100
confirmations, and an unconfirmed coin is the change of a transaction of the
wallet. One that is not spendable yet becomes so once it confirms or, for a
coinbase, matures.
|]

lockedOption :: Parser Bool
lockedOption =
  switch $
    long "locked"
      <> help "List only the locked coins: those a pending transaction spends, and those the user locked"

{- LockCoin and UnlockCoin Parsers -}

lockCoinParser :: ParserInfo Command
lockCoinParser = do
  let cmd = CommandLockCoin <$> coinArg
  info cmd $
    progDesc "Lock a coin, so that no transaction spends it"
      <> footer
        [r|
A coin the user locks is held back: `preparetx` does not spend it, coin control
refuses it, and no imported transaction or sweep may spend it, until
`unlockcoin` frees it. Only a free coin can be locked: a coin a pending
transaction spends is locked already, and `deletetx` frees it. The lock stays
while the coin confirms, and goes with the coin once it is spent elsewhere.
`coins --locked` lists the locked coins.
|]

unlockCoinParser :: ParserInfo Command
unlockCoinParser = do
  let cmd = CommandUnlockCoin <$> coinArg
  info cmd $
    progDesc "Unlock a coin locked with lockcoin"
      <> footer
        [r|
Only a coin the user locked with `lockcoin` can be unlocked. A coin a pending
transaction spends is freed by deleting that transaction with `deletetx`.
|]

-- | A coin of the wallet, as coins lists it
coinArg :: Parser OutPoint
coinArg =
  argument coinReader $
    metavar "TXHASH:INDEX"
      <> help "The coin: the id of its transaction and its index among the outputs, as coins lists it"

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

{- SendTx Parser -}

sendTxParser :: ParserInfo Command
sendTxParser = do
  let cmd =
        CommandSendTx
          <$> optional nosigHashArg
          <*> inputFileMaybeOption
          <*> yesOption "Send the transaction without showing it and asking first"
  info cmd $
    progDesc "Send (upload) a signed transaction to the network"
      <> footer
        [r|
Once a transaction is signed, it can be sent to the network. This step is
irreversible and the transaction will become effective. `sendtx` shows the
transaction first, as `reviewtx` does, and sends it only if you answer yes.
--yes sends it without showing it or asking, for a script.

The transaction is handed to the store, which passes it to the network's
nodes without checking it, so `sendtx` then waits up to ten seconds for the
store to see it in its mempool, the only sign that the network accepted it. If
the store does not see it, nothing is recorded and `sendtx` says so: the
transaction may have been refused, for a fee below what the nodes relay for
instance, or may just be slow to come back. Run `sendtx` again later.

A pending transaction of the wallet is sent by its NOSIGHASH and, once seen,
marked as online; `sync` then sends it again should the store lose it, and
retires it once it is listed. It is sent again by every `sync` for as long
as the store does not list it: a transaction the network refuses for good, for
a fee below what the nodes relay for instance, keeps coming back until it is
deleted with `deletetx`, which frees its coins.

A signed transaction in a file is sent with --input. One the wallet holds as
pending, the file an offline signer returns, takes the place of the version
held and is sent as by its NOSIGHASH: marked as online, its raw transaction
stored. One the wallet does not hold, a sweep of outside coins for instance,
is only checked and uploaded, and nothing in the wallet is updated.

The network of a file is the one it names, as hw writes them, and that of the
account of a pending transaction. The file of an earlier hw names none, and is
prepared again to be sent when the wallet does not hold its transaction.
|]

-- | --yes, which answers yes to the question of a command without asking
-- it, for a script. The help says what the command then does.
yesOption :: String -> Parser Bool
yesOption what =
  switch $
    short 'y'
      <> long "yes"
      <> help what

{- Sync Parser -}

syncParser :: ParserInfo Command
syncParser = do
  let cmd = CommandSync <$> accountOption <*> fullOption
  info cmd $
    progDesc "Download account data from the network"
      <> footer
        [r|
The `sync` command will download the latest address balances, transactions and
coins from the network, for one account or for all of them. It will also update
the account balances.

Before that, it looks at the gap of addresses past the ones the account has,
twenty by default (the `gap` key of the configuration file): an address found
used there, by someone paying from the account's public key or by another wallet
of the same seed, is generated along with the ones before it and synced like
the others. The scan goes on while a window of the gap has a used address, so
the first sync of a new account recovers every address its payers used, with
its transactions and coins. A payer who derived further ahead than the gap,
another wallet of the seed with a larger gap for instance, lands beyond that
watch: raise the `gap` key for one sync to look further. The same key bounds
how many unused receiving addresses `receive` hands out.

It also looks after the pending transactions: one that was sent with `sendtx`
and that the network now lists is retired, even under another id, its
signatures changed by a miner or made by another signer; one that was sent but
that the store
no longer knows is sent again, since nodes forget unconfirmed transactions; one
whose coin was spent by another transaction, or is gone, is reported as
conflicted. The store is asked about every coin a pending transaction spends,
those of other accounts and those outside the wallet, a sweep's, alike. A signed
transaction that was never sent is left alone. A sent transaction is sent again
on every sync for as long as the store does not list it, unless it is in
conflict, so one the network refuses for good, for a fee below what the nodes
relay for instance, keeps coming back until it is deleted with `deletetx`.
|]

fullOption :: Parser Bool
fullOption =
  switch $
    long "full"
      <> showDefault
      <> help
        [r|
Force the sync to re-download all the account data instead of only the deltas
(the data that has changed). A full sync also marks as dead the transactions
the store no longer lists. It should normally not be required: a reorganisation
of the chain forces one.
|]

{- Backup Parser -}

backupParser :: ParserInfo Command
backupParser = do
  let cmd =
        CommandBackup
          <$> optional (fileArgument "File to copy the database to (default: a file in ~/.hw/backups)")
  info cmd $
    progDesc "Copy the wallet database to a backup file"
      <> footer
        [r|
Without a file, the copy goes to ~/.hw/backups, named after the format version
of the database and the time of the copy. The copy is complete: accounts,
addresses, labels, coins, transactions and pending transactions.

To restore a backup, replace ~/.hw/accounts.sqlite with the copy, or point
"database-file" in ~/.hw/config.json to it, then run any command. A copy made
by an older hw is migrated first, after being backed up again into
~/.hw/migration-backups. A copy made by a newer hw is refused.
|]

{- Version Parser -}

versionParser :: ParserInfo Command
versionParser = do
  let cmd = pure CommandVersion
  info cmd $ progDesc "Display the version of hw"

{- SignMsg Parser -}

signMsgParser :: ParserInfo Command
signMsgParser = do
  let cmd =
        CommandSignMsg
          <$> (SignMsg <$> signAddressArg <*> signMsgArg <*> splitInOption)
  info cmd $
    progDescDoc (offline "Sign a user-defined message")
      <> footer
        [r|
Sign a user-defined message using the private key associated with a given
address. The signature uses the Bitcoin Signed Message format (the legacy
format of BIP-322), which can be verified by Bitcoin Core and other wallets.
The message is signed exactly as provided. A warning is shown when it contains
leading or trailing whitespace or line breaks, as these are easily altered
when copying the message and the signature would then fail to verify.
|]

signAddressArg :: Parser Text
signAddressArg =
  strArgument $
    metavar "ADDRESS"
      <> help
        [r|
Sign a message using the private key of this address
|]

signMsgArg :: Parser Text
signMsgArg =
  strArgument $
    metavar "TEXT"
      <> help
        [r|
The message to be signed
|]

{- PrepareSweep Parser -}

prepareSweepParser :: ParserInfo Command
prepareSweepParser = do
  let options =
        PrepareSweep
          <$> accountOption
          <*> sweepNetworkOption
          <*> prvKeysMaybeOption
          <*> many sweepFromOption
          <*> some sweepToOption -- some: at least one
          <*> many coinOption
          <*> outputFileMaybeOption
          <*> feeOption
          <*> dustOption
          <*> highFeeYesOption
  info (CommandPrepareSweep <$> options) $
    progDesc "Prepare a transaction to sweep funds"
      <> footer
        [r|
`preparesweep` will prepare a transaction that sweeps coins that are not in
this wallet: every coin found at the --sweepfrom addresses, every coin found at
the addresses of the private keys given with --prvkeys, and the coins given
with --coin. Give any of the three. The private keys are not needed to prepare
the sweep, only to sign it with `signsweep`, so they can stay on another
computer; when given, in a file or typed on the terminal with --prvkeys -, they
can be in WIF, minikey or hexadecimal format, and the fee of a coin of an
uncompressed key is estimated exactly. The typical use
case for this command is to migrate an old wallet to a new mnemonic. You can
send the funds to 1 or multiple addresses, in this wallet or not. You can use
the shorthand format -t ADDR1 -t ADDR2 to specify the --sweepto addresses.

A sweep that pays an address of this wallet is of the account of that address,
found without --account unless the sweep pays several accounts, on its network.
The address finds its account however it is written: a Bitcoin Cash address
with or without its prefix, or in its legacy form. The sweep is stored as a
pending transaction, to be signed with `signsweep` by its nosigHash, and
written to the --output file when one is given.

A sweep that pays no address of this wallet needs no account, even in a wallet
that has none: hw is then a utility. It needs the --network it is for and an
--output file, which names the network and is signed from there. The
default-network of the configuration file does not apply to it. Private keys,
coins and the legacy addresses of Bitcoin Cash read the same on Bitcoin, so a
wrong guess would sweep the coins of the other chain.

A fee rate above 100 satoshis per byte, ten times the default, is more often a
typo than a choice, so `preparesweep` asks before it takes one, and before the
private keys are typed. --yes takes it without asking, for a script.
|]

sweepFromOption :: Parser Text
sweepFromOption =
  strOption $
    short 'w'
      <> long "sweepfrom"
      <> metavar "ADDRESS"
      <> help "Addresses to sweep the coins from"

addressFileOption :: Parser (Maybe FilePath)
addressFileOption =
  optional . strOption $
    long "addrfile"
      <> metavar "FILENAME"
      <> action "file"
      <> help "File containing addresses to sweep from"

sweepToOption :: Parser Text
sweepToOption =
  strOption $
    short 't'
      <> long "sweepto"
      <> metavar "ADDRESS"
      <> help "Addresses to sweep to"

{- SignSweep Parser -}

signSweepParser :: ParserInfo Command
signSweepParser = do
  let cmd =
        CommandSignSweep
          <$> signingOptions
          <*> prvKeysMaybeOption
  info cmd $
    progDesc "Sign a sweep transaction"
      <> footer
        [r|
Sign a transaction that was prepared with the `preparesweep` command. As the
coins of this transaction are not from this wallet, the private keys for signing
are passed in a separate --prvkeys file, or typed on the terminal without echo
with --prvkeys - or when no file is given. The sweep transaction can be
referenced by its nosigHash if it exists in the wallet, or you can pass it as a
file with --input, in which case it need not involve this wallet at all. The
signed transaction goes to the --output file or, without one, replaces the
--input file if you agree, as hw asks before the keys. A sweep outside the
wallet needs no account: it is signed on the network its file names. The file
of an earlier hw names none, and is prepared again.
|]

-- | The private keys: a file, or - for the terminal
prvKeysMaybeOption :: Parser (Maybe KeySource)
prvKeysMaybeOption =
  optional . option (eitherReader source) $
    short 'k'
      <> long "prvkeys"
      <> metavar "FILENAME"
      <> action "file"
      <> help "File with the private keys in WIF, minikey or hex format, one or more per line; - to type them on the terminal"
  where
    source "-" = Right KeysOnTerminal
    source file = Right (KeysInFile file)


{- Argument parser helpers -}

textArg :: String -> Parser Text
textArg desc =
  argument str $
    metavar "TEXT"
      <> help desc

fileArgument :: String -> Parser FilePath
fileArgument desc =
  strArgument $
    metavar "FILENAME"
      <> action "file"
      <> help desc
