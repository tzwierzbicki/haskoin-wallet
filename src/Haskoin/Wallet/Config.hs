{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The configuration of hw: the file config.toml of the data directory,
-- which the setup writes with every setting as a comment the first time
-- hw is run, and the paths of that directory. Two of its keys say where in a portfolio a
-- command works when its options do not: the network and the type of
-- account ('configDefaultNetwork' and 'configDefaultType'). Three more say
-- how the answers look: the unit of the amounts, the view of the listings
-- and their order ('configUnit', 'configListingView' and
-- 'configListingReverse'). One says which networks hw shows
-- ('configShownNetworks'), and one whether hw keeps a journal of the
-- transactions for ledger ('configJournal'). setdefault changes these
-- seven in the file ('setConfigKeys'), and showdefault shows them. Two say whether hw
-- copies the database and its labels by itself after the commands that
-- change them ('configAutoBackupDatabase' and 'configAutoBackupLabels').
-- One is a table, which gives tags an account of that journal
-- ('configJournalAccounts').
-- How many of these copies hw keeps is no key of the file. hw keeps them
-- by their age, on a ladder that one number cannot say
-- ('Haskoin.Wallet.Database.Backup.autoBackupRetention').
--
-- A portfolio has accounts on eight networks, and most users work on two
-- of them. So the file lists the networks hw shows, btc and bch unless
-- the user chose others, and the others are hidden. A hidden network is
-- not disabled. sync and recover still work on it, so that showing it
-- later needs no catching up. It is left out of every answer, and a
-- command cannot name it ('isShown', 'hiddenNetworkRefusal'). Two
-- answers still say that a portfolio is used on one, since saying that
-- the portfolio is unused would be untrue: portfolios, without its name,
-- and recover, with its name and the command that shows it. One command
-- reads an address there: verifymsg, which reads nothing of the wallet.
-- The key
-- lists the networks to show, not those to hide. A network a later hw
-- adds then stays hidden until the user asks for it, and the file says
-- what the user sees.
--
-- The portfolio a command works in is no key of the file: it is the
-- active portfolio, which the database keeps and switch changes
-- ("Haskoin.Wallet.Database.Portfolio"). The pager of a listing or of a
-- help is no key either. The environment names it, as it does for other programs
-- ("Haskoin.Wallet.Cli.Pager").
--
-- = A file the user can write in
--
-- The file is TOML: a key, an equals sign and a value on each line, and
-- comments after a @#@. Its user edits it by hand for the settings
-- setdefault does not write, and wants to note why a setting is what it
-- is. JSON, which the file was in before, has no comments. TOML also has
-- a type for each value, a list for the networks, and a written standard,
-- which a file of loose @key=value@ lines has not. YAML has all of these
-- too, but reads a text without quotes as a number or a boolean when it
-- looks like one, and gives meaning to the indentation. Twelve flat keys
-- and one table need none of that.
--
-- The file hw writes sets its version and what the user chose in the
-- setup: the networks to show, the default network, the default type and
-- whether hw keeps a journal ('newConfigFileWith'). Every setting is in it as a comment, with what
-- it is for, the values it takes and its default ('newConfigFile'). A
-- setting takes its default until the user sets it.
-- hw can then change a default in a later version, and the users who
-- never chose a value get the new one. A file with every default written
-- as a value would keep the old ones for ever, since hw could not tell a
-- value the user chose from one it wrote.
--
-- hw writes the file only in its setup, which plain hw runs when there is
-- no file ("Haskoin.Wallet.Cli.Setup"). A command run before that is
-- refused and told so ('notSetUp'). The file is where the user says which
-- networks hw shows and where the commands work, so hw does not make it
-- up without asking.
--
-- A key hw does not know is left aside, and a warning names it
-- ('readConfig'). It is most often a mistyped key, whose setting would
-- otherwise keep its default without a word. A value hw cannot take is
-- refused, with the line it is at and the values the key takes.
--
-- setdefault changes the file in place. It replaces the value of a key,
-- or adds its line, and leaves every other character of the file as it
-- is, the comments of the user too ("Haskoin.Wallet.Config.TomlEdit").
--
-- = One table
--
-- Every setting is a key with one value, but one. The key
-- journal-accounts is a table: a line for each tag, with the account of
-- the journal the tag stands for ("Haskoin.Wallet.Config.JournalAccounts"). A
-- user may have twenty of them, and a table reads as the list it is.
--
-- In TOML, every line below the header of a table belongs to the table,
-- down to the next header. So the table is the last setting of the file
-- hw writes ('configSettings'), and its comment says to keep it there. A
-- setting written below it by mistake would be read as a tag. hw refuses
-- it, and says to move it above the header ('journalAccountsRead').
--
-- setdefault does not write the table. The user writes its lines by
-- hand, as they write their own journal.
--
-- = The version of the file
--
-- The key version says which format the file is in. It is 1, and a file
-- without the key reads as version 1. hw refuses a file of a higher
-- version, which a later hw wrote and whose keys may not mean what this
-- hw takes them to mean.
--
-- The version is insurance, and may never change. A new key needs no new
-- version: a file without it takes its default. A key that is gone needs
-- none either: hw warns about it. Only a key that keeps its name and
-- changes its meaning would need one, and such a change is better made
-- under a new name.
--
-- = One directory for everything
--
-- hw keeps everything in one data directory, ~\/.hw: this file, the
-- database portfolios.sqlite, and the directories of its copies
-- ("Haskoin.Wallet.Database.Backup"). Every path hw works on comes from the
-- directory the configuration was read from ('configDataDirectory').
--
-- No key of the file puts the database somewhere else. Such a key would
-- split what hw keeps. The copies would go next to the database, while
-- the configuration stayed in ~\/.hw. A backup the key pointed at, to
-- restore it, would become the live database, and get copies of its own
-- beside it, in the directory of the backups.
--
-- A user who wants the wallet on another disk, an encrypted one for
-- instance, makes ~\/.hw a link to a directory there. The link moves
-- everything at once, and hw needs no option for it. hw refuses a link
-- that leads to no directory, rather than fail later on a file it cannot
-- write ('openDataDirectory').
module Haskoin.Wallet.Config where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Default
import Data.List (intercalate, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network (..), bch, btc, netByName)
import Haskoin.Store.WebClient
import Haskoin.Wallet.Amounts (AmountUnit (..), readUnit, unitName)
import Haskoin.Wallet.Config.JournalAccounts (TagAccounts, noTagAccounts, readAccountName)
import Haskoin.Wallet.Config.TomlEdit (TomlFile (..), atLine, readToml, setTomlKey, tomlLine, tomlValue)
import Haskoin.Wallet.Config.View (View (..), readView, viewName, viewNames)
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys (AccountKind (..), kindName, kindNamed, kindNames, portfolioNetworks)
import Haskoin.Wallet.Tags (Tag, readTag, tagMaxLength, tagText)
import Numeric.Natural (Natural)
import qualified System.Directory as D
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import Toml (Position (..), Value, Value' (..), valueAnn)
import Toml.Semantics (Table' (..))

{- The data directory -}

-- | The path of the data directory of hw, ~\/.hw, whether it is there or
-- not
hwDataDirectory :: IO FilePath
hwDataDirectory = D.getAppUserDataDirectory "hw"

-- | A data directory, ready for a command, or why it cannot be used.
--
-- A directory that is missing is created, so that its owner alone can
-- enter it ('privateDirectory'). The database it holds is not for the
-- other users of the computer. A directory that is there keeps its
-- permissions.
--
-- The directory may be a link to a directory elsewhere. A link that leads
-- nowhere is refused, and so is a link to something that is not a
-- directory. hw does not create what a link names. Such a link most
-- often names a directory on a drive that is not mounted. A new
-- directory there would hold a new, empty wallet, on another disk than
-- the one the user chose. Without this refusal, hw would stop anyway, on
-- a configuration file it cannot write, with a message that does not say
-- why.
--
-- A directory the system does not let hw look at or create is a message
-- too, with the reason the system gives. That happens when the home
-- directory is missing or closed to writing. hw does not end on an
-- exception then.
--
-- Only the setup creates the directory, since it is what writes the
-- first file in it ("Haskoin.Wallet.Cli.Setup"). A command that finds no
-- configuration file is refused before it creates anything
-- ('readConfig'), after the link is checked ('checkDataDirectoryLink').
openDataDirectory :: FilePath -> ExceptT String IO FilePath
openDataDirectory dir = do
  checkDataDirectoryLink dir
  liftEither . first notCreated =<< liftIO (systemReason (privateDirectory dir))
  return dir
  where
    notCreated reason = "The directory " <> dir <> " could not be created: " <> reason

-- | Refuse a data directory that is a link to nothing, or to something
-- that is not a directory ('openDataDirectory' says why). Nothing is
-- created. A path with nothing at it passes: it is no link.
checkDataDirectoryLink :: FilePath -> ExceptT String IO ()
checkDataDirectoryLink dir = do
  linked <- liftEither . first notRead =<< liftIO (systemReason (isSymbolicLink dir))
  when linked $ do
    target <- liftIO $ D.getSymbolicLinkTarget dir
    isDirectory <- liftIO $ D.doesDirectoryExist dir
    -- doesPathExist follows the link, so it is False for a link that
    -- leads nowhere
    exists <- liftIO $ D.doesPathExist dir
    unless isDirectory . throwError $
      if exists
        then "The directory " <> dir <> " links to " <> target <> ", which is not a directory"
        else "The directory " <> dir <> " links to " <> target <> ", which does not exist. If it is on a drive, mount the drive and try again."
  where
    notRead reason = "The directory " <> dir <> " could not be read: " <> reason

-- | Whether a path is a symbolic link. A path with nothing at it is no
-- link.
isSymbolicLink :: FilePath -> IO Bool
isSymbolicLink path =
  D.pathIsSymbolicLink path `catchIOError` \failure ->
    if isDoesNotExistError failure then return False else ioError failure

-- | The configuration file of a data directory
configFile :: FilePath -> FilePath
configFile dir = dir </> "config.toml"

-- | The database hw works on: portfolios.sqlite in the data directory,
-- named after what it holds, the portfolios of every wallet
databaseFile :: Config -> FilePath
databaseFile cfg = configDataDirectory cfg </> "portfolios.sqlite"

-- | The journal hw keeps for ledger when the configuration asks for it
-- ('configJournal'): portfolios.ledger in the data directory, next to the
-- database it is written from and named after it.
--
-- The path is fixed, and no key of the file moves it. The user includes
-- the file in a journal of their own, by its path, and a path that never
-- changes keeps that line right. It also keeps the file where hw alone
-- writes: hw replaces this file without asking, which it never does to a
-- file the user names.
journalFile :: Config -> FilePath
journalFile cfg = configDataDirectory cfg </> "portfolios.ledger"

{- The configuration -}

-- | The settings of hw, and the data directory they were read from. Each
-- setting has a default, which the file shows in a comment and which a
-- key missing from the file keeps.
data Config = Config
  { -- | The data directory: ~\/.hw for hw. It holds the configuration
    -- file, the database and the directories of its copies, so every path
    -- hw works on comes from it. It is no key of the file, since the file
    -- is in it. 'readConfig' sets it to the directory it read the file
    -- from. A test sets a scratch directory of its own, which keeps it
    -- away from the wallet of the user. The defaults ('def') have the
    -- empty path, which is the current directory.
    configDataDirectory :: FilePath,
    -- | The web address of the haskoin-store that hw asks about the chain.
    -- It may end with a slash or not. The name of the network goes after
    -- it, with a slash between ('Haskoin.Wallet.Store.resourceUrl').
    configHost :: Text,
    -- | The gap of BIP44: how many unused addresses hw generates past the
    -- last used one of a chain, and looks at for payments on a sync
    configGap :: Natural,
    -- | How many requests the sync sends to the store at a time. The
    -- public store at api.haskoin.com refuses a second request while one
    -- is in flight (503 Service Temporarily Unavailable), so the default
    -- is one; a store of your own may allow more.
    configParallelRequests :: Natural,
    -- | The network of a command that needs one when --network is not
    -- given: btc unless the user chose another. A sweep that pays no
    -- portfolio does not take it: it needs its --network. sync and
    -- recover do not take it either: they go over every network.
    configDefaultNetwork :: Network,
    -- | The networks hw shows: btc and bch unless the user chose others.
    -- The other networks of a portfolio are hidden. Every answer leaves
    -- them out, and a command that names one is refused, with the way to
    -- show it. sync and recover still work on them, so that the wallet is
    -- up to date on a network the day the user shows it. Only the status
    -- line of the two names a hidden network, since it says what the user
    -- is waiting for. The default network is always one of these: a file
    -- where it is not is refused ('checkDefaultShown'). They are kept in
    -- the order of the networks of a portfolio, each once
    -- ('inPortfolioOrder').
    configShownNetworks :: [Network],
    -- | The type of account a command takes when --type is not given:
    -- bip44 unless the user chose another. receive gives out an address
    -- of that type, and a payment that spends coins of both types sends
    -- its change to an address of that type. The networks of Bitcoin Cash
    -- have BIP44 accounts alone, and take bip44 whatever this says
    -- ('Haskoin.Wallet.Database.Account.defaultKindOn').
    configDefaultType :: AccountKind,
    -- | The unit of the amounts when --unit is not given, for a user who
    -- counts in satoshis or in bits. It is the default of --unit, so it
    -- sets the unit of the amounts shown and of those typed without a
    -- unit. Splitting the two would make "1000" typed mean another amount
    -- than "1000" shown. JSON answers count satoshis whatever the unit, so
    -- a script that reads them is not affected.
    configUnit :: AmountUnit,
    -- | The view of a listing when no --compact, --verbose or --tabular
    -- is given, for a user who always wants the same one. The option
    -- overrides it for one listing, and --compact brings back the compact
    -- view. It sets the screen alone. --json and --csv answer the same
    -- whatever it says, so a script that reads them is not affected. The
    -- commands that answer with one element keep their own view.
    configListingView :: View,
    -- | Whether a listing lists its rows in the reverse order when no
    -- --reverse or --no-reverse is given, for a user who always reads
    -- the oldest transaction first. It is off unless the user turns it
    -- on. The option overrides it for one listing, and --no-reverse
    -- brings back the usual order. Like the view, it sets the screen
    -- alone. --json and --csv keep the usual order whatever it says,
    -- unless --reverse is given, so a script that pages through a
    -- listing gets the same rows on every computer. portfolios has one
    -- order.
    configListingReverse :: Bool,
    -- | Whether hw keeps copies of the database. After each command that
    -- adds a wallet or a portfolio, hw copies the database to
    -- the auto-backups directory next to it, and keeps the recent ones
    -- and a thinning trail of older ones ("Haskoin.Wallet.Database.Backup"). It is
    -- on unless the user turns it off. Such a command changes what the
    -- mnemonics rebuild, so a copy then keeps every portfolio as it
    -- stands.
    --
    -- The key also covers the copy hw makes before it loses something:
    -- before deletewallet and deleteportfolio delete, and before
    -- restoredatabase replaces the database. That copy goes to the
    -- auto-backups directory too, with its labels, and no copy follows
    -- such a command ("Haskoin.Wallet.Commands"). A user who turned the
    -- copies off asked hw to write no copy by itself, so none is written
    -- then either.
    configAutoBackupDatabase :: Bool,
    -- | Whether hw keeps copies of the labels. After each command that
    -- can change what exportwallet writes, hw writes the whole export to a
    -- new file in the auto-backups directory next to the database, and keeps
    -- the recent ones and a thinning trail of older ones
    -- ("Haskoin.Wallet.Database.Backup"). It is on unless the user turns it off. A
    -- database lost with its computer then still leaves every label and
    -- description the user typed, in a file importwallet reads.
    configAutoBackupLabels :: Bool,
    -- | Whether hw keeps a journal of the transactions for the ledger
    -- command line tool, next to the database ('journalFile'), and writes
    -- it again after each command that changes it
    -- ("Haskoin.Wallet.Commands"). The user includes the file in a
    -- journal of their own, and it is then always up to date, with no
    -- export to run.
    --
    -- It is off unless the user turns it on. The file tells the whole
    -- history in plain text, and most users never run ledger. The offline
    -- computer has no history to write either.
    configJournal :: Bool,
    -- | The accounts of the journal that tags stand for, as in rent for
    -- @Expenses:Housing:Rent@: the table journal-accounts. The journal
    -- posts a payment with such a tag, or to a contact with such a tag,
    -- to the account of the tag, where it would post it to an unknown
    -- account ("Haskoin.Wallet.Config.JournalAccounts"). It gives no tag an
    -- account unless the user writes the table.
    --
    -- It counts for the journal of exportjournal and for the one hw
    -- keeps alike, so the two stay the same file. A tag of the table
    -- need not be a tag of the database. The file is read without the
    -- database, and a user may write the table before the first tag.
    configJournalAccounts :: TagAccounts
  }
  deriving (Eq, Show)

{- The settings of the file -}

-- | The version of the format of the configuration file that this hw
-- reads and writes
configVersion :: Integer
configVersion = 1

-- | The name of the key that holds the version of the file
versionKey :: Text
versionKey = "version"

-- | A setting of the configuration file, as the file hw writes shows it
-- to its user: in a comment, with what it is for, the values it takes
-- and its default.
--
-- The settings are listed once, here ('configSettings'). The file hw writes,
-- the keys hw knows and the values a refusal names all come from this
-- list and from the names the readers take. So the comments of the file
-- cannot name a value that hw refuses.
data Setting = Setting
  { -- | The key, as the file writes it
    settingKey :: Text,
    -- | What the setting is for, in sentences
    settingPurpose :: Text,
    -- | The values it takes, when they can be listed or said in a few
    -- words. The address of a server is not listed.
    settingAccepted :: Maybe Text,
    -- | Its default, and how the file shows it
    settingDefault :: SettingDefault
  }

-- | The default of a setting, as the file hw writes shows it
data SettingDefault
  = -- | A value, which the file shows on a line that sets the key to it,
    -- as in @# unit = "btc"@
    DefaultValue Value
  | -- | A table without a line, for a setting that is a table. The file
    -- shows its header alone, as in @# [journal-accounts]@, and the user
    -- writes the lines below it. The text says what an empty table
    -- means, as in "no tag".
    DefaultEmptyTable Text

-- | The settings of the file, in the order the file hw writes shows
-- them, each with the default of the configuration. The one setting that
-- is a table is the last of them, as TOML asks.
configSettings :: [Setting]
configSettings =
  [ Setting
      "host"
      "The address of the haskoin-store server that hw asks about the chain, with or without a slash at the end."
      Nothing
      (DefaultValue (Toml.Text (configHost def))),
    Setting
      defaultNetworkKey
      "The network of a command given no --network. It must be one of shown-networks. hw setdefault --network sets it."
      (Just (quotedNames networkNames))
      (DefaultValue (Toml.Text (networkName (configDefaultNetwork def)))),
    Setting
      shownNetworksKey
      "The networks hw shows. The others are hidden: left out of every answer, and refused as a --network, while sync and recover still work on them. hw setdefault --show-network and --hide-network change it."
      (Just ("a list of one or more of " <> quotedNames networkNames))
      (DefaultValue (Toml.List (Toml.Text . networkName <$> configShownNetworks def))),
    Setting
      defaultTypeKey
      "The type of address of a command given no --type. The networks of Bitcoin Cash take bip44 whatever it says. hw setdefault --type sets it."
      (Just (quotedNames kindNames))
      (DefaultValue (Toml.Text (kindName (configDefaultType def)))),
    Setting
      unitKey
      "The unit of the amounts of a command given no --unit: of the amounts shown, and of those typed without a unit. A bit is 100 satoshis. hw setdefault --unit sets it."
      (Just (quotedNames unitNames))
      (DefaultValue (Toml.Text (unitName (configUnit def)))),
    Setting
      listingViewKey
      "The view of a listing given no --compact, --verbose or --tabular. hw setdefault with one of these options sets it."
      (Just (quotedNames viewNames))
      (DefaultValue (Toml.Text (viewName (configListingView def)))),
    Setting
      listingReverseKey
      "Whether a listing given no --reverse or --no-reverse lists its rows in the reverse order on the screen: the oldest transaction first, for instance. --json and --csv keep the usual order. hw setdefault --reverse and --no-reverse set it."
      (Just trueOrFalse)
      (DefaultValue (Toml.Bool (configListingReverse def))),
    Setting
      "gap"
      "How many unused addresses in a row receive gives out and sync looks at, in each account."
      (Just (wholeNumberUpTo mostGap))
      (DefaultValue (Toml.Integer (toInteger (configGap def)))),
    Setting
      "parallel-requests"
      "How many requests go to the server at once. The public server takes one at a time, so keep it at 1 unless you run your own haskoin-store."
      (Just (wholeNumberUpTo mostParallelRequests))
      (DefaultValue (Toml.Integer (toInteger (configParallelRequests def)))),
    Setting
      "autobackup-database"
      "Whether hw copies the database into auto-backups after each command that adds a wallet or a portfolio. It also covers the backup hw makes there before deletewallet, deleteportfolio and restoredatabase."
      (Just trueOrFalse)
      (DefaultValue (Toml.Bool (configAutoBackupDatabase def))),
    Setting
      "autobackup-labels"
      "Whether hw copies the labels into auto-backups after each command that changes them."
      (Just trueOrFalse)
      (DefaultValue (Toml.Bool (configAutoBackupLabels def))),
    Setting
      journalKey
      "Whether hw keeps a journal of the transactions for ledger, the accounting tool, in portfolios.ledger next to the database, and writes it again after each command that changes it. hw setdefault --journal and --no-journal set it."
      (Just trueOrFalse)
      (DefaultValue (Toml.Bool (configJournal def))),
    -- The table comes last. Every line below its header belongs to it,
    -- so a setting shown after it would be set inside the table.
    Setting
      journalAccountsKey
      "The accounts your tags stand for in the journal for ledger. To give a tag an account, remove the # before [journal-accounts] below, and write a line for the tag under it. hw does not read a line under a header that is still a comment. A payment with the tag, or to a contact with the tag, is then posted to that account, where it would go to Expenses:Unknown or Income:Unknown. Keep this table the last setting of the file: every line below its header belongs to it."
      (Just "lines such as rent = \"Expenses:Housing:Rent\"")
      (DefaultEmptyTable "no tag")
  ]

-- | The values of a count with its largest one, as the file hw writes and
-- the refusals say them
wholeNumberUpTo :: Natural -> Text
wholeNumberUpTo most = "a whole number from 1 to " <> cs (show most)

-- | The values of a switch, as the file hw writes them
trueOrFalse :: Text
trueOrFalse = "true, false"

-- | The largest gap the file takes. A sync asks the store about this many
-- addresses of each chain of each account, every time, and receive gives
-- out as many in a row: a thousand is fifty times the gap of BIP44, and
-- as far as hw generates on its own ('Haskoin.Wallet.Database.Address.scanLimit').
-- A gap in the billions is a sync that never ends, with no word.
mostGap :: Natural
mostGap = 1000

-- | The most requests at a time the file takes. A store of one's own
-- serves a few dozen at once at best, so a hundred leaves room, and a
-- larger number can only be a mistake.
mostParallelRequests :: Natural
mostParallelRequests = 100

-- | The names the file takes for a network, in the order of the networks
-- of a portfolio
networkNames :: [Text]
networkNames = networkName <$> portfolioNetworks

-- | The name of a network, as the file writes it
networkName :: Network -> Text
networkName net = cs net.name

-- | The names the file writes for a unit. It reads the plurals bits and
-- sats too, as --unit does.
unitNames :: [Text]
unitNames = unitName <$> [UnitBitcoin, UnitBit, UnitSatoshi]

-- | Names as the file takes them, each in quotes: "btc", "bit", "sat"
quotedNames :: [Text] -> Text
quotedNames names = Text.intercalate ", " [Text.pack (show name) | name <- names]

-- | The keys of the configuration file hw knows: its version and its
-- settings
knownKeys :: [Text]
knownKeys = versionKey : (settingKey <$> configSettings)

{- The file hw writes -}

-- | The configuration file of a data directory that has none, before the
-- setup sets in it what the user chose ('newConfigFileWith').
--
-- It sets its version and nothing else. Every setting is in it as a
-- comment: what it is for, the values it takes, its default, and a line
-- that sets it to that default, ready to change. The setting that is a
-- table has the header of the table for that line. A user who opens the
-- file then sees every setting there is, and hw still takes each default
-- from its own code, not from the file. The module header says why.
--
-- setdefault adds the line of a setting below the comment that shows it
-- ('Haskoin.Wallet.Config.TomlEdit.setTomlKey').
newConfigFile :: Text
newConfigFile = Text.unlines (opening <> concatMap shown configSettings)
  where
    opening =
      commentLines "The settings of hw."
        <> ["#"]
        <> commentLines "A line that starts with # is a comment. Each setting is written below as a comment, with its default. hw takes the default of a setting until you set it. To set one, remove the # before its line and change its value, or add a line of your own. hw setdefault sets some of them for you, and keeps your comments."
        <> [""]
        <> commentLines "The version of the format of this file. Leave it as it is."
        <> [tomlLine versionKey (Toml.Integer configVersion)]
    shown setting =
      [""]
        <> commentLines (settingPurpose setting)
        <> maybe [] (commentLines . ("Accepted: " <>)) (settingAccepted setting)
        <> case settingDefault setting of
          DefaultValue value ->
            commentLines ("Default: " <> tomlValue value)
              <> ["# " <> tomlLine (settingKey setting) value]
          DefaultEmptyTable meaning ->
            commentLines ("Default: " <> meaning)
              <> ["# [" <> settingKey setting <> "]"]

-- | The lines of a comment that says a text, each within 78 columns: the
-- words of the text, as many on a line as fit
commentLines :: Text -> [Text]
commentLines text = ("# " <>) <$> filled (Text.words text)
  where
    filled [] = []
    filled (word : others) =
      let (line, left) = fill word others in line : filled left
    fill line (word : others)
      | Text.length line + 1 + Text.length word <= 76 = fill (line <> " " <> word) others
    fill line left = (line, left)

{- Reading the text of the file -}

-- | The configuration the text of a configuration file gives, with the
-- keys of the text hw does not know. Or why the text is refused, with the
-- line it is refused at, as in "at line 12: the key ...".
--
-- A key the text does not have takes its default. The data directory is
-- no key: 'readConfig' sets it.
--
-- The version is checked first. A file of a later version is not read
-- any further, since its keys may mean something else.
configOfText :: Text -> Either String (Config, [Text])
configOfText text = do
  keys <- tomlKeys <$> readToml text
  let key name field reader = maybe (Right (field def)) (valueOf name reader) (Map.lookup name keys)
  _ <- maybe (Right ()) (valueOf versionKey versionRead) (Map.lookup versionKey keys)
  cfg <-
    Config (configDataDirectory def)
      <$> key "host" configHost hostRead
      <*> key "gap" configGap (countUpTo mostGap)
      <*> key "parallel-requests" configParallelRequests (countUpTo mostParallelRequests)
      <*> key defaultNetworkKey configDefaultNetwork networkRead
      <*> key shownNetworksKey configShownNetworks shownNetworksRead
      <*> key defaultTypeKey configDefaultType (nameRead "a type of address" kindNames kindNamed)
      <*> key unitKey configUnit (nameRead "a unit" unitNames readUnit)
      <*> key listingViewKey configListingView (nameRead "a view" viewNames readView)
      <*> key listingReverseKey configListingReverse switchRead
      <*> key "autobackup-database" configAutoBackupDatabase switchRead
      <*> key "autobackup-labels" configAutoBackupLabels switchRead
      <*> key journalKey configJournal switchRead
      <*> maybe (Right (configJournalAccounts def)) (journalAccountsRead . snd) (Map.lookup journalAccountsKey keys)
  -- The refusal of a hidden default network is about two keys. It gives
  -- the line of the first of them the file has.
  let lineOfKey name = posLine . fst <$> Map.lookup name keys
      lineOfDefault = fromMaybe 1 (lineOfKey defaultNetworkKey <|> lineOfKey shownNetworksKey)
  first (atLine lineOfDefault) (checkDefaultShown (Map.member defaultNetworkKey keys) cfg)
  return (cfg, [name | name <- Map.keys keys, name `notElem` knownKeys])
  where
    -- The value of a key as its reader takes it, or the refusal of the
    -- reader, which ends a sentence that starts with the key
    valueOf name reader (_, value) =
      first (\refusal -> atLine (posLine (valueAnn value)) ("the key " <> show name <> " " <> refusal)) (reader value)

-- | What reads the value of a key, or refuses it with the end of a
-- sentence that starts with the key, as in "must be a unit: ..."
type ValueReader a = Value' Position -> Either String a

-- | The version of the file: 1, the one this hw reads. A higher one is
-- the format of a later hw.
versionRead :: ValueReader ()
versionRead value = case value of
  Integer' _ version
    | version == configVersion -> Right ()
    | version > configVersion ->
        Left ("is " <> show version <> ", the format of a later hw. This hw reads version " <> show configVersion <> ".")
  _ -> Left ("must be " <> show configVersion)

-- | The address of the store, a text
hostRead :: ValueReader Text
hostRead value = case value of
  Text' _ host -> Right host
  _ -> Left "must be the address of a server, in quotes"

-- | A count from one to the largest the key takes. It cannot be zero: a
-- gap of zero would refuse every new address, and zero requests at a time
-- would send none. Nor can it be past any use ('mostGap',
-- 'mostParallelRequests').
countUpTo :: Natural -> ValueReader Natural
countUpTo most value = case value of
  Integer' _ count | count >= 1 && count <= toInteger most -> Right (fromInteger count)
  _ -> Left ("must be " <> cs (wholeNumberUpTo most))

-- | A switch, on or off
switchRead :: ValueReader Bool
switchRead value = case value of
  Bool' _ switch -> Right switch
  _ -> Left "must be true or false"

-- | One of a few names, such as a unit. The refusal lists the names the
-- file writes, in quotes, as they are typed.
nameRead :: String -> [Text] -> (Text -> Maybe a) -> ValueReader a
nameRead what names readName value = case value of
  Text' _ name | Just known <- readName name -> Right known
  _ -> Left ("must be " <> what <> ": " <> cs (quotedNames names))

-- | The default network, by its name
networkRead :: ValueReader Network
networkRead = nameRead "a network" networkNames (netByName . cs)

-- | The networks the key shown-networks lists by their names, in the
-- order of the networks of a portfolio. A name hw does not know is
-- refused, rather than left aside: the user who mistyped a network would
-- wonder why it stays hidden. An empty list is refused too, since hw
-- would then show nothing.
shownNetworksRead :: ValueReader [Network]
shownNetworksRead value = case value of
  List' _ [] -> Left ("must list at least one network: " <> known)
  List' _ names -> inPortfolioOrder <$> mapM network names
  _ -> Left ("must be a list of networks, as in [\"btc\", \"bch\"]. The networks are: " <> known)
  where
    known = cs (quotedNames networkNames)
    network element = case element of
      Text' _ name | Just net <- netByName (cs name) -> Right net
      Text' _ name -> Left ("lists " <> show name <> ", which is no network. The networks are: " <> known)
      _ -> Left ("must list networks by their names, in quotes: " <> known)

-- | The accounts the table journal-accounts gives to tags, or why the
-- table is refused, with the line it is refused at. It is told apart
-- from the other readers, which give the line of their key: a table has
-- many lines, and the refusal gives the one that is wrong.
--
-- A key of the table is a tag, read as a command reads one: capital
-- letters are taken as small ones ('readTag'). Two keys that are then
-- one tag are refused, at the line of the second, since the file would
-- give that tag two accounts. A word that cannot be a tag is refused,
-- rather than left aside with a warning. It would never file a payment,
-- and the user would wonder why.
--
-- A key of the table that is the name of a setting of hw is refused too
-- ('knownKeys'). In TOML, a line below the header of a table belongs to
-- the table. A user who adds @unit = "sat"@ at the end of the file,
-- below the table, would otherwise give a tag named unit the account
-- sat, and keep the unit they meant to change, without a word. The
-- price is that a tag named as a setting, such as host, cannot have an
-- account.
--
-- The value of a key is the name of an account, which must be one the
-- journal can write ('readAccountName').
--
-- TOML writes a table in three ways: under a header, inside braces, or
-- as keys with a dot. They read as the same table here.
journalAccountsRead :: Value' Position -> Either String TagAccounts
journalAccountsRead value = case value of
  Table' _ (MkTable entries) -> foldM filed Map.empty (sortOn lineOf (Map.toAscList entries))
  _ ->
    refusedAt (valueAnn value) $
      "the key " <> table <> " must be a table of tags, each with an account of the journal, as in " <> example
  where
    table = show journalAccountsKey
    example = "rent = \"Expenses:Housing:Rent\""
    refusedAt :: Position -> String -> Either String a
    refusedAt place problem = Left (atLine (posLine place) problem)
    -- A word of the file in double quotes, as the file writes it
    quoted :: Text -> String
    quoted text = "\"" <> cs text <> "\""
    -- The lines are read from the top of the file down, so that a
    -- refusal about two lines is at the second
    lineOf :: (Text, (Position, Value' Position)) -> Int
    lineOf (_, (place, _)) = posIndex place
    -- The table so far, with the account of one more line of the file
    filed :: Map Tag Text -> (Text, (Position, Value' Position)) -> Either String (Map Tag Text)
    filed accounts (word, (place, written)) = do
      when (word `elem` knownKeys) $ refusedAt place (aSetting word)
      tag <- either (const (refusedAt place (noTag word))) Right (readTag word)
      when (tag `Map.member` accounts) $
        refusedAt place ("the key " <> table <> " gives the tag " <> cs (tagText tag) <> " an account twice")
      account <- case written of
        Text' at name -> either (refusedAt at . noAccount word name) Right (readAccountName name)
        _ -> refusedAt (valueAnn written) ("the tag " <> quoted word <> " of " <> table <> " must have an account of the journal, in quotes, as in " <> example)
      return (Map.insert tag account accounts)
    noTag :: Text -> String
    noTag word =
      "the key "
        <> table
        <> " has a line for "
        <> quoted word
        <> ", which is no tag. A tag is made of the letters a to z, digits, - and _, starts with a letter or a digit, and has "
        <> show tagMaxLength
        <> " characters at most."
    aSetting :: Text -> String
    aSetting word =
      "the key "
        <> table
        <> " has a line for "
        <> quoted word
        <> ", which is a setting of hw and cannot have an account. If you meant the setting, move the line above the header ["
        <> cs journalAccountsKey
        <> "]: every line below a header belongs to its table."
    noAccount :: Text -> Text -> String -> String
    noAccount word name reason =
      "the tag " <> quoted word <> " of " <> table <> " has " <> quoted name <> ", which is no account of a journal: " <> reason

-- | Refuse a configuration whose default network is hidden. A command
-- without --network works on the default network, so it would work on a
-- network hw shows nothing of. The refusal names both ways out.
--
-- The first argument says whether the file has the key default-network.
-- When it does not, the default network is the default of the key, and
-- the refusal says so: it is then about the list of shown networks, the
-- one key of the two that the user wrote, on the line the refusal gives.
checkDefaultShown :: Bool -> Config -> Either String ()
checkDefaultShown defaultIsSet cfg
  | isShown cfg (configDefaultNetwork cfg) = Right ()
  | otherwise = Left (fault <> ". Add " <> show name <> " to \"shown-networks\", or set \"default-network\" to " <> intercalate " or " (show . (.name) <$> configShownNetworks cfg))
  where
    name = (configDefaultNetwork cfg).name
    fault
      | defaultIsSet = "the key \"default-network\" names " <> name <> ", which the key \"shown-networks\" does not list"
      | otherwise = "the key \"shown-networks\" does not list " <> name <> ", the default network when the key \"default-network\" is not set"

instance Default Config where
  def =
    Config
      { configDataDirectory = "",
        configHost = cs (def :: ApiConfig).host,
        configGap = 20,
        configParallelRequests = 1,
        configDefaultNetwork = btc,
        configShownNetworks = [btc, bch],
        configDefaultType = Bip44,
        configUnit = UnitBitcoin,
        configListingView = Compact,
        configListingReverse = False,
        configAutoBackupDatabase = True,
        configAutoBackupLabels = True,
        configJournal = False,
        configJournalAccounts = noTagAccounts
      }

-- | The network of a command that needs one: the one --network names, or
-- the default network of the configuration
commandNetwork :: Config -> Maybe Network -> Network
commandNetwork cfg = fromMaybe (configDefaultNetwork cfg)

-- | The unit of the amounts of a command: the one --unit names, or the
-- unit of the configuration
commandUnit :: Config -> Maybe AmountUnit -> AmountUnit
commandUnit cfg given = fromMaybe (configUnit cfg) given

{- The networks hw shows -}

-- | Whether hw shows the network: it is one of those the key
-- shown-networks lists
isShown :: Config -> Network -> Bool
isShown cfg net = net `elem` configShownNetworks cfg

-- | Those of the elements that are on a network hw shows, given the
-- network of each. Every answer goes through this, and so does every
-- search for what the user names: a hidden network then never comes up
-- as a candidate, and never has to be told apart with --network.
onShownNetworks :: Config -> (a -> Network) -> [a] -> [a]
onShownNetworks cfg networkOf = filter (isShown cfg . networkOf)

-- | The networks given, each once, in the order of the networks of a
-- portfolio: how the configuration keeps the networks it shows, and how
-- setdefault writes them, whatever the order they were typed in. A
-- network that is none of a portfolio's comes after them, rather than be
-- dropped without a word, should a name ever read as one.
inPortfolioOrder :: [Network] -> [Network]
inPortfolioOrder networks = ofPortfolio <> others
  where
    ofPortfolio = [net | net <- portfolioNetworks, net `elem` networks]
    others = nub [net | net <- networks, net `notElem` portfolioNetworks]

-- | The command that shows a hidden network, as a refusal gives it to the
-- user to run
showNetworkCommand :: Network -> String
showNetworkCommand net = "hw setdefault --show-network " <> net.name

-- | The refusal of a --network that names a hidden network. It says which
-- networks hw shows, and gives the command that shows this one too. hw
-- does not show the network for the one command instead. A user who hid
-- a network asked not to see it, and a typo such as btctest for btc would
-- otherwise work on a network the user does not watch.
hiddenNetworkRefusal :: Config -> Network -> String
hiddenNetworkRefusal cfg net =
  "The network "
    <> net.name
    <> " is hidden. hw shows "
    <> networksText (configShownNetworks cfg)
    <> ". To show "
    <> net.name
    <> " too, run: "
    <> showNetworkCommand net

-- | The refusal of what the user named, an address or a transaction for
-- instance, that the wallet has on hidden networks alone. Saying that the
-- wallet does not have it would be untrue, and showing it would show a
-- hidden network. So the refusal names the networks, and gives the command
-- that shows the first of them. The first text says what was named, as in
-- "The address tb1q...".
onHiddenNetworks :: String -> [Network] -> String
onHiddenNetworks what networks = case inPortfolioOrder networks of
  [] -> what <> " is on a hidden network"
  [only] -> what <> " is on " <> only.name <> ", which is hidden. To show " <> only.name <> ", run: " <> showNetworkCommand only
  several@(lowest : _) ->
    what
      <> " is on "
      <> networksText several
      <> ", which are hidden. To show one of them, run for instance: "
      <> showNetworkCommand lowest

-- | The networks of a text, as a sentence names them: "btc", "btc and
-- bch", "btc, bch and btctest"
networksText :: [Network] -> String
networksText networks = case reverse ((.name) <$> networks) of
  [] -> ""
  [only] -> only
  final : others -> intercalate ", " (reverse others) <> " and " <> final

{- Reading the file -}

-- | The configuration of a data directory, with the warnings to show the
-- user: the keys of the file hw does not know, which it leaves aside. The
-- configuration works on that directory ('configDataDirectory').
--
-- A directory without a configuration file is refused, with the way to
-- set hw up ('notSetUp'). hw does not write a file of defaults by itself
-- there. The setup asks the user which networks to show and where the
-- commands work, and a file written without a word would skip these
-- questions for good ("Haskoin.Wallet.Cli.Setup"). Nothing is created:
-- the directory may not even be there.
readConfig :: FilePath -> ExceptT String IO (Config, [String])
readConfig dir = do
  let file = configFile dir
  exists <- liftIO $ hasConfigFile dir
  unless exists $ throwError (notSetUp file)
  text <- liftEither =<< liftIO (readTextFile file)
  (cfg, unknown) <- liftEither $ first (configNotRead file) (configOfText text)
  return (cfg {configDataDirectory = dir}, unknownKeysWarning file text unknown)

-- | The refusal of a configuration file that does not read, with where
-- and why, as in "... could not be read, at line 12: the key ..."
configNotRead :: FilePath -> String -> String
configNotRead file problem = "The configuration file " <> file <> " could not be read, " <> problem

-- | The warnings about the keys of the file hw does not know, given the
-- text of the file and those keys. Such a key is most often mistyped,
-- and its setting would otherwise keep its default without a word. A
-- table of the file is such a key too, unless it is journal-accounts,
-- the one table hw reads.
--
-- The keys under a header [journal-accounts] that is still a comment
-- have a warning of their own ('underCommentedTable'). The file hw
-- writes shows that header as a comment, and a user who writes a line
-- for a tag below it without removing the # has written a key hw does
-- not know. "Left aside" alone does not tell them why, and they look for
-- the fault in the journal, which never saw the line. So this warning
-- says what to remove.
unknownKeysWarning :: FilePath -> Text -> [Text] -> [String]
unknownKeysWarning file text unknown =
  [ "The configuration file "
      <> file
      <> " has lines under the header ["
      <> cs journalAccountsKey
      <> "], which is still a comment, so hw left them aside: "
      <> named underHeader
      <> ". Remove the # before ["
      <> cs journalAccountsKey
      <> "] to give each tag its account."
    | not (null underHeader)
  ]
    <> ["The configuration file " <> file <> " has keys hw does not know, left aside: " <> named others | not (null others)]
  where
    underHeader = underCommentedTable text unknown
    others = filter (`notElem` underHeader) unknown
    named :: [Text] -> String
    named keys = intercalate ", " (cs <$> keys)

-- | Among the keys of the file hw does not know, those written under a
-- header [journal-accounts] that is still a comment, as in
--
-- > # [journal-accounts]
-- > rent = "Expenses:Housing:Rent"
--
-- They are in the order of the file.
--
-- The lines of the text are read as they are, not as TOML. The answer
-- only chooses the words of a warning, and the TOML that was read no
-- longer has its comments. A line counts when it is below the commented
-- header and starts with one of the keys, then an equals sign.
underCommentedTable :: Text -> [Text] -> [Text]
underCommentedTable text unknown =
  [key | line <- drop 1 belowHeader, Just key <- [keyOf line], key `elem` unknown]
  where
    belowHeader = dropWhile (not . isCommentedHeader) (Text.lines text)
    -- "# [journal-accounts]", with any spaces around the sign
    isCommentedHeader line = case Text.stripPrefix "#" (Text.strip line) of
      Just commented -> Text.strip (Text.dropWhile (== '#') commented) == "[" <> journalAccountsKey <> "]"
      Nothing -> False
    -- The key of a line that sets one, without the quotes TOML allows
    -- around it
    keyOf line = case Text.breakOn "=" line of
      (before, after) | not (Text.null after) -> Just (Text.dropAround (== '"') (Text.strip before))
      _ -> Nothing

-- | Whether a data directory has a configuration file: whether hw was
-- set up there
hasConfigFile :: FilePath -> IO Bool
hasConfigFile dir = D.doesFileExist (configFile dir)

-- | The refusal of a command run before hw was set up: the data
-- directory has no configuration file, whose path is given. Plain hw,
-- without a command, runs the setup that writes it.
notSetUp :: FilePath -> String
notSetUp file = "hw is not set up yet: there is no configuration file at " <> file <> ". Run hw without a command to set it up."

-- | The configuration of hw, from ~\/.hw, with its warnings. A ~\/.hw that
-- is a link to nothing is refused first: its drive is not mounted, and
-- the user is not told to set up a wallet they already have.
initConfig :: IO (Either String (Config, [String]))
initConfig = runExceptT $ do
  dir <- liftIO hwDataDirectory
  checkDataDirectoryLink dir
  readConfig dir

{- Writing the first file -}

-- | The text of the first configuration file of a data directory: the
-- file hw writes ('newConfigFile'), with the keys given set to what the
-- user chose in the setup. Each key gets a line below the comment that
-- shows its default, as setdefault would add it
-- ('Haskoin.Wallet.Config.TomlEdit.setTomlKey'). The settings the user was not
-- asked about stay comments, and keep taking the defaults of hw.
newConfigFileWith :: [(Text, Value)] -> Either String Text
newConfigFileWith = foldM (\text (key, value) -> setTomlKey key value text) newConfigFile

-- | Write the first configuration file of a data directory, with the
-- keys given set ('newConfigFileWith'), and give its path. The directory
-- is created when missing ('openDataDirectory'). A file that is there
-- already is not written over: the refusal of the system says so.
writeFirstConfig :: FilePath -> [(Text, Value)] -> ExceptT String IO FilePath
writeFirstConfig dir chosen = do
  let file = configFile dir
  text <- liftEither (newConfigFileWith chosen)
  _ <- openDataDirectory dir
  liftEither =<< liftIO (writeTextFile file text)
  return file

-- | The configuration of a data directory as it stands, for a reader that
-- must change nothing. It is the defaults when there is no file, and
-- Nothing when the file cannot be read. Unlike 'readConfig', it writes no
-- file and leaves out the warnings. It works on that directory too.
configAsItIs :: FilePath -> IO (Maybe Config)
configAsItIs dir = do
  let file = configFile dir
  exists <- D.doesFileExist file
  parsed <-
    if exists
      then either (const Nothing) (Just . fst) . (>>= configOfText) <$> readTextFile file
      else return (Just def)
  return $ (\cfg -> cfg {configDataDirectory = dir}) <$> parsed

{- Changing the file -}

-- | Set keys of the configuration file of a data directory, and leave
-- every other character of the file as it is: the other keys, those hw
-- does not know too, and the comments. Each key gets its new value where
-- the old one was, or a new line when the file does not set it
-- ('Haskoin.Wallet.Config.TomlEdit.setTomlKey'). The values must be ones the
-- keys take: the caller writes what 'configOfText' reads back.
--
-- setdefault writes the defaults the user chooses this way. The user then
-- never has to edit the file by hand for them.
--
-- A file hw cannot change this way is left as it is, whole: no key of
-- the command is written. The refusal says why, and gives the line to
-- write by hand.
setConfigKeys :: FilePath -> [(Text, Value)] -> ExceptT String IO ()
setConfigKeys dir changes = do
  let file = configFile dir
      set text (key, value) = first (notChanged file key value) (setTomlKey key value text)
  text <- liftEither =<< liftIO (readTextFile file)
  changed <- liftEither (foldM set text changes)
  liftEither =<< liftIO (replaceTextFile file changed)

-- | The refusal of a change hw cannot make to the configuration file by
-- itself, with the line that makes it by hand
notChanged :: FilePath -> Text -> Value -> String -> String
notChanged file key value reason =
  "The configuration file " <> file <> " " <> reason <> ". hw left it as it is. Change it by hand, with this line: " <> cs (tomlLine key value)

-- | The names of the two keys that say where in a portfolio a command
-- works
defaultNetworkKey, defaultTypeKey :: Text
defaultNetworkKey = "default-network"
defaultTypeKey = "default-type"

-- | The names of the three keys that say how the answers look: the unit
-- of the amounts, the view of the listings and whether they list their
-- rows in the reverse order
unitKey, listingViewKey, listingReverseKey :: Text
unitKey = "unit"
listingViewKey = "listing-view"
listingReverseKey = "listing-reverse"

-- | The name of the key that lists the networks hw shows
shownNetworksKey :: Text
shownNetworksKey = "shown-networks"

-- | The name of the key that says whether hw keeps a journal for ledger
journalKey :: Text
journalKey = "journal"

-- | The name of the table that gives tags an account of the journal
journalAccountsKey :: Text
journalAccountsKey = "journal-accounts"
