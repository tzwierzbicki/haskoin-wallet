{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The command line of the wallets and the portfolios: newportfolio,
-- recover, checkmnemonic, mnemonicsheet, portfolios, switch,
-- deleteportfolio and deletewallet, each with its options and its help.
-- "Haskoin.Wallet.Commands.Portfolio" runs them.
module Haskoin.Wallet.Cli.Parser.Portfolio
  ( newPortfolioCommand,
    recoverCommand,
    checkMnemonicCommand,
    mnemonicSheetCommand,
    portfoliosCommand,
    switchCommand,
    deletePortfolioCommand,
    deleteWalletCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin.Wallet.Cli.Command (CheckMnemonic (..), Command (..), MnemonicSheetOptions (..), NewPortfolio (..), Recover (..))
import Haskoin.Wallet.Cli.Completion (portfolioIndexes, walletIndexes)
import Haskoin.Wallet.Cli.Parser.Args
import Haskoin.Wallet.Cli.Parser.Options (deleteYesOption, fileArgument, indexNamed, listingOutputOption, portfolioIndexOption, walletOptionWith)
import Haskoin.Wallet.Util (readNatural)
import Numeric.Natural (Natural)
import Text.RawString.QQ (r)

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

{- portfolios -}

-- | portfolios, which shows the wallets, with their fingerprints, and the
-- portfolios of each, with their balances and their descriptors
portfoliosCommand :: CommandSpec Command
portfoliosCommand =
  CommandSpec
    { name = "portfolios",
      offline = False,
      summary = "Show the wallets and their portfolios: fingerprints, balances and descriptors",
      details =
        [r|
`portfolios` lists every wallet, and under each wallet its portfolios with
their balances. A wallet is the master key of one mnemonic. A portfolio holds
the accounts of a wallet that have the same account index: portfolio0 holds
the accounts at index 0, on every network.

Each wallet starts with a line that has its name, its fingerprint and its
description:

  wallet0  fingerprint bc560f95  "paper backup in the safe"

The fingerprint is a short code of 8 hexadecimal digits that identifies the
mnemonic. It is the same on every computer that holds the wallet, and the
name of the wallet is not.

The portfolios of the wallet come under that line. Each portfolio starts with
its name, its description and the date it was created. Then comes one line
for each network where the portfolio has funds or addresses. The line shows
the total of what the portfolio holds there, what it can spend of it now, and
how many coins and addresses it has. When the portfolio cannot spend all of
it, the line also shows the unspendable amount, which is the rest of the
total. On btc, each figure is the sum of the bip44 account and the bip84
account.

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

--verbose shows every detail of each portfolio. It gives the time the
portfolio was created, and it lists every network that hw shows. Each network
has the total, the spendable amount and the unspendable amount, then the
confirmed and unconfirmed balances that the server gave at the last sync.
These two also add up to the total: they split it by what a block has
confirmed, and not by what the portfolio can spend. Under each network come
its accounts, each with its derivation path and its output descriptor. An
output descriptor is a line of text in the format of BIP 380. It describes an
account without its private key, for another wallet to read:

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
header names the columns: the * of the active portfolio, the wallet, its
fingerprint, the portfolio, the network, the total of what the portfolio
holds there, what it can spend, what it cannot spend, how many coins and
addresses it has, and its description. Each amount has its unit after it: btc
on a row of Bitcoin, bch on a row of Bitcoin Cash.

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
            "Show every detail of each portfolio: the time it was created, every network with each of its balances, and the path, output descriptor and SLIP-132 key of every account."
            "Show each network of each portfolio on one row of a table: the wallet, its fingerprint, the portfolio, the network, the total it holds, what it can spend, what it cannot spend, its coins and its addresses."
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
