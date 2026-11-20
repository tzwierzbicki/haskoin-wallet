{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The configuration file of hw: the file hw writes, what its keys
-- take and what they refuse, its version, the keys that say where a
-- command works, the networks hw shows, the table that gives tags an
-- account of the journal, the data directory around the
-- file, which may be a link to a directory elsewhere, the first file of
-- a directory, which the setup writes, and the keys a command changes in
-- it
module Haskoin.Wallet.ConfigSpec (spec) where

import Control.Monad.Except (runExceptT)
import Data.Default (def)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Haskoin (bch, btc, btcRegTest, btcTest4)
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.Config
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils (configIn, configWith, inScratchDirectory, kycTag, rentTag)
import Haskoin.Wallet.Testnet3 (bchTestnet3, btcTestnet3)
import System.Directory (createDirectory, createDirectoryLink, createFileLink, doesDirectoryExist, doesFileExist, doesPathExist)
import System.FilePath ((</>))
import Test.Hspec
import Text.RawString.QQ (r)
import qualified Toml

spec :: Spec
spec = do
  newFileSpec
  keySpec
  versionSpec
  defaultsSpec
  shownNetworksSpec
  journalAccountsSpec
  directorySpec
  firstFileSpec
  changeSpec

-- | The file of every setting as a comment, which the setup starts from
newFileSpec :: Spec
newFileSpec =
  describe "The configuration file hw writes" $ do
    it "shows every setting in a comment: what it is for, the values it takes and its default" $
      newConfigFile `shouldBe` newFile

    it "sets nothing but its version, so it reads as the default configuration, without a key hw does not know" $
      configOfText newFile `shouldBe` Right (def, [])

    -- The lines of the file that show a setting, with the # before each
    -- one removed
    it "shows each setting on a line that sets its default once the # before it is removed" $
      configOfText everyDefaultSet `shouldBe` Right (def, [])

keySpec :: Spec
keySpec =
  describe "A key of the configuration file" $ do
    it "takes its default when missing, so an empty file is the default configuration" $
      decodeConfig "" `shouldBe` Right def

    it "sets its value alone, the others keeping their defaults" $
      decodeConfig "gap = 30" `shouldBe` Right def {configGap = 30}

    it "may have a comment after its value" $
      decodeConfig "gap = 30  # my old wallet used many addresses" `shouldBe` Right def {configGap = 30}

    it "sets nothing on a line that starts with #, which is a comment" $
      decodeConfig "# gap = 30" `shouldBe` Right def

    it "sets the address of the server hw asks about the chain" $
      configHost <$> decodeConfig "host = \"http://localhost:3000/\"" `shouldBe` Right "http://localhost:3000/"

    it "cannot be an address of the server that is not a text" $
      decodeConfig "host = 3000"
        `shouldBe` Left "at line 1: the key \"host\" must be the address of a server, in quotes"

    it "sets the number of parallel requests" $
      configParallelRequests <$> decodeConfig "parallel-requests = 2" `shouldBe` Right 2

    -- For a user who counts in satoshis: the amounts are shown in them, and
    -- read in them when typed without a unit
    it "sets the unit of the amounts when --unit is not given" $
      configUnit <$> decodeConfig "unit = \"sat\"" `shouldBe` Right UnitSatoshi

    it "names a unit as --unit does, by its plural too, in any case" $
      configUnit <$> decodeConfig "unit = \"Bits\"" `shouldBe` Right UnitBit

    it "cannot be a unit hw does not know, and the refusal lists the units as they are typed" $
      decodeConfig "unit = \"mbtc\""
        `shouldBe` Left "at line 1: the key \"unit\" must be a unit: \"btc\", \"bit\", \"sat\""

    -- For a user who always wants a table: the listings show one without
    -- --tabular
    it "sets the view of the listings when no option asks for one" $
      configListingView <$> decodeConfig "listing-view = \"tabular\"" `shouldBe` Right Table

    it "names each view as the option that asks for it does" $ do
      configListingView <$> decodeConfig "listing-view = \"compact\"" `shouldBe` Right Compact
      configListingView <$> decodeConfig "listing-view = \"verbose\"" `shouldBe` Right Verbose

    -- "table" is what the help calls the view, but not its name
    it "cannot be a view hw does not know, naming the views it knows" $
      decodeConfig "listing-view = \"table\""
        `shouldBe` Left "at line 1: the key \"listing-view\" must be a view: \"compact\", \"verbose\", \"tabular\""

    it "lists the rows of the listings in their usual order unless the file says otherwise" $
      configListingReverse <$> decodeConfig "" `shouldBe` Right False

    -- For a user who always reads the oldest transaction first
    it "lists the rows of the listings in the reverse order when the file says so" $
      configListingReverse <$> decodeConfig "listing-reverse = true" `shouldBe` Right True

    it "cannot reverse the listings by anything but true or false" $
      decodeConfig "listing-reverse = \"yes\""
        `shouldBe` Left "at line 1: the key \"listing-reverse\" must be true or false"

    it "cannot be a gap of zero, which would refuse every new address" $
      decodeConfig "gap = 0"
        `shouldBe` Left "at line 1: the key \"gap\" must be a whole number from 1 to 1000"

    it "cannot be zero requests at a time, which would send none" $
      decodeConfig "parallel-requests = 0"
        `shouldBe` Left "at line 1: the key \"parallel-requests\" must be a whole number from 1 to 100"

    -- A sync looks at this many addresses of each chain of each account,
    -- so a gap in the billions never ends (finding J-14 of the usability
    -- review of 0.10)
    it "cannot be a gap above 1000, which a sync would never finish looking at" $
      decodeConfig "gap = 1001"
        `shouldBe` Left "at line 1: the key \"gap\" must be a whole number from 1 to 1000"

    it "can be a gap of 1000, the largest" $
      configGap <$> decodeConfig "gap = 1000" `shouldBe` Right 1000

    it "cannot be the gap of four billion addresses that made a sync run without end" $
      decodeConfig "gap = 4294967296"
        `shouldBe` Left "at line 1: the key \"gap\" must be a whole number from 1 to 1000"

    -- No server takes that many, and the largest number a file can hold
    -- made a sync take all the memory of the computer (finding J-13)
    it "cannot be more than 100 requests at a time" $
      decodeConfig "parallel-requests = 101"
        `shouldBe` Left "at line 1: the key \"parallel-requests\" must be a whole number from 1 to 100"

    it "can be 100 requests at a time, the most" $
      configParallelRequests <$> decodeConfig "parallel-requests = 100" `shouldBe` Right 100

    it "cannot be the nine billion billion requests at a time that took all the memory" $
      decodeConfig "parallel-requests = 9223372036854775807"
        `shouldBe` Left "at line 1: the key \"parallel-requests\" must be a whole number from 1 to 100"

    it "must be a number when its default is one" $
      decodeConfig "gap = \"twenty\""
        `shouldBe` Left "at line 1: the key \"gap\" must be a whole number from 1 to 1000"

    it "cannot be a count with decimals" $
      decodeConfig "gap = 20.5"
        `shouldBe` Left "at line 1: the key \"gap\" must be a whole number from 1 to 1000"

    it "turns the copies of the database off when false" $
      configAutoBackupDatabase <$> decodeConfig "autobackup-database = false" `shouldBe` Right False

    it "turns the copies of the labels off when false" $
      configAutoBackupLabels <$> decodeConfig "autobackup-labels = false" `shouldBe` Right False

    it "must be true or false when its default is one of them" $
      decodeConfig "autobackup-labels = \"no\""
        `shouldBe` Left "at line 1: the key \"autobackup-labels\" must be true or false"

    -- The journal tells the whole history in plain text, and most users
    -- never run ledger
    it "keeps no journal for ledger unless the file says otherwise" $
      configJournal <$> decodeConfig "" `shouldBe` Right False

    it "keeps a journal for ledger when the file says so" $
      configJournal <$> decodeConfig "journal = true" `shouldBe` Right True

    it "cannot turn the journal on by anything but true or false" $
      decodeConfig "journal = \"yes\""
        `shouldBe` Left "at line 1: the key \"journal\" must be true or false"

    -- The gap is on the third line of the file
    it "is refused with the line it is at" $
      decodeConfig "# My settings\nunit = \"sat\"\ngap = 0\n"
        `shouldBe` Left "at line 3: the key \"gap\" must be a whole number from 1 to 1000"

    it "cannot be set twice, and the refusal gives the line of the second time" $
      decodeConfig "gap = 30\ngap = 40\n"
        `shouldBe` Left "at line 2: the key \"gap\" is set twice"

    -- The word sat starts at the eighth character of the line
    it "cannot be a text without quotes, and the refusal says where the quotes go" $
      decodeConfig "unit = sat"
        `shouldBe` Left "at line 1, column 8: parse error: unexpected bare key. A text goes in quotes, as in \"btc\"."

    it "is told apart from a key hw does not know, which is left aside and named" $
      configOfText "gap = 30\ngaps = 5\n" `shouldBe` Right (def {configGap = 30}, ["gaps"])

    -- hw reads one table, journal-accounts, and no other
    it "is not in a table of another name: such a table is named as a key hw does not know" $
      configOfText "gap = 30\n\n[backups]\nkeep = 2\n" `shouldBe` Right (def {configGap = 30}, ["backups"])

-- | The key that says which format the file is in
versionSpec :: Spec
versionSpec =
  describe "The version of the configuration file" $ do
    it "is 1, a key hw knows" $
      configOfText "version = 1\ngap = 30\n" `shouldBe` Right (def {configGap = 30}, [])

    it "may be left out: the file then reads as version 1" $
      configOfText "gap = 30\n" `shouldBe` Right (def {configGap = 30}, [])

    it "cannot be higher: the file is in the format of a later hw, and is refused" $
      decodeConfig "version = 2\n"
        `shouldBe` Left "at line 1: the key \"version\" is 2, the format of a later hw. This hw reads version 1."

    -- A gap of zero is refused in a file of version 1. Here the file is
    -- of another format, where the key may mean something else.
    it "is looked at before any other key of the file" $
      decodeConfig "gap = 0\nversion = 2\n"
        `shouldBe` Left "at line 2: the key \"version\" is 2, the format of a later hw. This hw reads version 1."

    it "cannot be lower than 1" $
      decodeConfig "version = 0\n" `shouldBe` Left "at line 1: the key \"version\" must be 1"

    it "cannot be a text" $
      decodeConfig "version = \"1\"\n" `shouldBe` Left "at line 1: the key \"version\" must be 1"

-- | The two keys that say where in the active portfolio a command works
-- when its options do not: the network and the type of account
defaultsSpec :: Spec
defaultsSpec =
  describe "Where a command works without an option" $ do
    it "is on btc in a file without the key default-network" $
      configDefaultNetwork <$> decodeConfig "" `shouldBe` Right btc

    it "is in the bip44 account in a file without the key default-type" $
      configDefaultType <$> decodeConfig "" `shouldBe` Right Bip44

    it "is on the network the key default-network names" $
      configDefaultNetwork <$> decodeConfig "default-network = \"bch\"" `shouldBe` Right bch

    it "is in the account of the type the key default-type names" $
      configDefaultType <$> decodeConfig "default-type = \"bip84\"" `shouldBe` Right Bip84

    it "takes each key alone, the other keeping its default" $
      decodeConfig "default-type = \"bip84\"" `shouldBe` Right def {configDefaultType = Bip84}

    it "cannot be on a network hw does not know, and the refusal lists the networks as they are typed" $
      decodeConfig "default-network = \"doge\""
        `shouldBe` Left "at line 1: the key \"default-network\" must be a network: \"btc\", \"bch\", \"btctest\", \"btctest4\", \"btcreg\", \"bchtest\", \"bchtest4\", \"bchreg\""

    -- hw looks for the funds of a bip49 account when it recovers a seed,
    -- and holds no such account
    it "cannot be in an account of a type hw does not hold, naming the types it holds" $
      decodeConfig "default-type = \"bip49\""
        `shouldBe` Left "at line 1: the key \"default-type\" must be a type of address: \"bip44\", \"bip84\""

    it "is on the network --network names, whatever the default network" $
      commandNetwork (configWith "default-network = \"bch\"") (Just btcTestnet3) `shouldBe` btcTestnet3

    it "is on the default network without --network" $
      commandNetwork (configWith "default-network = \"bch\"") Nothing `shouldBe` bch

-- | The key that lists the networks hw shows. The others are hidden: left
-- out of every answer, while sync and recover still work on them.
shownNetworksSpec :: Spec
shownNetworksSpec =
  describe "The networks hw shows" $ do
    it "are btc and bch in a file without the key shown-networks" $
      configShownNetworks <$> decodeConfig "" `shouldBe` Right [btc, bch]

    it "are those the key lists" $
      configShownNetworks <$> decodeConfig "shown-networks = [\"btc\", \"btctest4\"]" `shouldBe` Right [btc, btcTest4]

    it "may be listed on several lines, with a comment after one of them" $
      configShownNetworks <$> decodeConfig "shown-networks = [\n  \"btc\",\n  \"btctest4\", # where I try things\n]\n"
        `shouldBe` Right [btc, btcTest4]

    -- btc comes before btctest4 in a portfolio, and is listed twice
    it "are kept in the order of the networks of a portfolio, each once, whatever the order of the file" $
      configShownNetworks <$> decodeConfig "shown-networks = [\"btctest4\", \"btc\", \"btc\"]" `shouldBe` Right [btc, btcTest4]

    it "cannot be none: hw would show nothing" $
      decodeConfig "shown-networks = []"
        `shouldBe` Left "at line 1: the key \"shown-networks\" must list at least one network: \"btc\", \"bch\", \"btctest\", \"btctest4\", \"btcreg\", \"bchtest\", \"bchtest4\", \"bchreg\""

    -- A mistyped network would otherwise stay hidden without a word
    it "cannot include a network hw does not know, which is named with the networks it knows" $
      decodeConfig "shown-networks = [\"btc\", \"doge\"]"
        `shouldBe` Left "at line 1: the key \"shown-networks\" lists \"doge\", which is no network. The networks are: \"btc\", \"bch\", \"btctest\", \"btctest4\", \"btcreg\", \"bchtest\", \"bchtest4\", \"bchreg\""

    it "must be a list, even of one network, and the refusal shows how a list is written" $
      decodeConfig "shown-networks = \"btc\""
        `shouldBe` Left "at line 1: the key \"shown-networks\" must be a list of networks, as in [\"btc\", \"bch\"]. The networks are: \"btc\", \"bch\", \"btctest\", \"btctest4\", \"btcreg\", \"bchtest\", \"bchtest4\", \"bchreg\""

    -- A command without --network works on the default network
    it "must include the default network: a file that hides it is refused, with both ways out" $
      decodeConfig "default-network = \"btctest4\""
        `shouldBe` Left "at line 1: the key \"default-network\" names btctest4, which the key \"shown-networks\" does not list. Add \"btctest4\" to \"shown-networks\", or set \"default-network\" to \"btc\" or \"bch\""

    -- The default network is btc when no key names it, so the line given
    -- is the one of the list that leaves btc out
    it "must include btc when no key names another default network, and the refusal gives the line of the list" $
      decodeConfig "gap = 30\nshown-networks = [\"bch\"]\n"
        `shouldBe` Left "at line 2: the key \"default-network\" names btc, which the key \"shown-networks\" does not list. Add \"btc\" to \"shown-networks\", or set \"default-network\" to \"bch\""

    it "may leave out btc, when the default network is another one they include" $
      decodeConfig "default-network = \"bch\"\nshown-networks = [\"bch\"]\n"
        `shouldBe` Right def {configDefaultNetwork = bch, configShownNetworks = [bch]}

    it "include a network the key lists, and no other" $ do
      let cfg = configWith "shown-networks = [\"btc\", \"btctest4\"]"
      (isShown cfg <$> [btc, bch, btcTestnet3, btcTest4]) `shouldBe` [True, False, False, True]

    -- Each element is given with its network: a number here
    it "keep what is on one of them, in its order, and leave out the rest" $ do
      let cfg = configWith "shown-networks = [\"btc\", \"btctest4\"]"
      onShownNetworks cfg snd [(1 :: Int, btcTest4), (2, bch), (3, btc), (4, btcTestnet3)] `shouldBe` [(1, btcTest4), (3, btc)]

    it "are written in the order of the networks of a portfolio, each once" $
      inPortfolioOrder [btcRegTest, bchTestnet3, btc, btcTest4, btc] `shouldBe` [btc, btcTest4, btcRegTest, bchTestnet3]

    it "do not include a hidden network named with --network, which is refused with the networks shown and the command that shows it" $
      hiddenNetworkRefusal def btcTest4
        `shouldBe` "The network btctest4 is hidden. hw shows btc and bch. To show btctest4 too, run: hw setdefault --show-network btctest4"

    it "do not include what the wallet has on one hidden network, which is refused with the command that shows the network" $
      onHiddenNetworks "The coin a1075db5:1" [btcTest4]
        `shouldBe` "The coin a1075db5:1 is on btctest4, which is hidden. To show btctest4, run: hw setdefault --show-network btctest4"

    -- The test networks share their keys: one address is on several. The
    -- first in the order of a portfolio is the one offered.
    it "do not include what the wallet has on several hidden networks, which are all named" $
      onHiddenNetworks "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih" [btcTest4, btcTestnet3]
        `shouldBe` "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4, which are hidden. To show one of them, run for instance: hw setdefault --show-network btctest"

    it "are named as a sentence names them" $ do
      networksText [btc] `shouldBe` "btc"
      networksText [btc, bch] `shouldBe` "btc and bch"
      networksText [btc, bch, btcTestnet3] `shouldBe` "btc, bch and btctest"

-- | The table that gives tags an account of the journal for ledger. A
-- payment with such a tag is posted to its account, where it would go to
-- Expenses:Unknown or Income:Unknown.
journalAccountsSpec :: Spec
journalAccountsSpec =
  describe "The table journal-accounts" $ do
    it "gives no tag an account in a file without it" $
      configJournalAccounts <$> decodeConfig "" `shouldBe` Right Map.empty

    it "gives each tag the account of its line" $
      configJournalAccounts <$> decodeConfig "[journal-accounts]\nrent = \"Expenses:Housing:Rent\"\nkyc = \"Expenses:Fees:Exchange\"\n"
        `shouldBe` Right (Map.fromList [(rentTag, "Expenses:Housing:Rent"), (kycTag, "Expenses:Fees:Exchange")])

    it "is a key hw knows, and leaves the settings above it as they are" $
      configOfText "gap = 30\n\n[journal-accounts]\nrent = \"Expenses:Housing:Rent\"\n"
        `shouldBe` Right (def {configGap = 30, configJournalAccounts = Map.fromList [(rentTag, "Expenses:Housing:Rent")]}, [])

    it "may be empty, with its header alone" $
      decodeConfig "[journal-accounts]\n" `shouldBe` Right def

    -- TOML writes a table in three ways, which mean the same
    it "may be written on one line inside braces" $
      configJournalAccounts <$> decodeConfig "journal-accounts = { rent = \"Expenses:Housing:Rent\" }\n"
        `shouldBe` Right (Map.fromList [(rentTag, "Expenses:Housing:Rent")])

    it "may be written as keys with a dot, without a header" $
      configJournalAccounts <$> decodeConfig "journal-accounts.rent = \"Expenses:Housing:Rent\"\n"
        `shouldBe` Right (Map.fromList [(rentTag, "Expenses:Housing:Rent")])

    it "takes a tag written with capital letters as the tag in small letters, as a command does" $
      configJournalAccounts <$> decodeConfig "[journal-accounts]\nRent = \"Expenses:Housing:Rent\"\n"
        `shouldBe` Right (Map.fromList [(rentTag, "Expenses:Housing:Rent")])

    it "must be a table, and the refusal shows a line of one" $
      decodeConfig "journal-accounts = \"Expenses:Housing:Rent\"\n"
        `shouldBe` Left "at line 1: the key \"journal-accounts\" must be a table of tags, each with an account of the journal, as in rent = \"Expenses:Housing:Rent\""

    -- A tag is one word
    it "cannot have a line for a word that is no tag, which is refused with the rules of a tag" $
      decodeConfig "[journal-accounts]\n\"my rent\" = \"Expenses:Housing:Rent\"\n"
        `shouldBe` Left "at line 2: the key \"journal-accounts\" has a line for \"my rent\", which is no tag. A tag is made of the letters a to z, digits, - and _, starts with a letter or a digit, and has 32 characters at most."

    -- Rent and rent are one tag, which the third line gives a second account
    it "cannot give a tag two accounts, and the refusal gives the line of the second" $
      decodeConfig "[journal-accounts]\nrent = \"Expenses:Housing:Rent\"\nRent = \"Expenses:Rent\"\n"
        `shouldBe` Left "at line 3: the key \"journal-accounts\" gives the tag rent an account twice"

    it "cannot give a tag anything but the name of an account in quotes" $
      decodeConfig "[journal-accounts]\nrent = true\n"
        `shouldBe` Left "at line 2: the tag \"rent\" of \"journal-accounts\" must have an account of the journal, in quotes, as in rent = \"Expenses:Housing:Rent\""

    it "cannot give a tag a name the journal cannot write as an account, and the refusal says why" $
      decodeConfig "[journal-accounts]\nrent = \"Expenses::Rent\"\n"
        `shouldBe` Left "at line 2: the tag \"rent\" of \"journal-accounts\" has \"Expenses::Rent\", which is no account of a journal: a name is missing before or after a colon"

    -- The journal states the balance of each portfolio at its end, and a
    -- payment posted there a second time would break it
    it "cannot file a payment under the account of a portfolio" $
      decodeConfig "[journal-accounts]\nrent = \"Assets:hw:wallet0:portfolio0\"\n"
        `shouldBe` Left "at line 2: the tag \"rent\" of \"journal-accounts\" has \"Assets:hw:wallet0:portfolio0\", which is no account of a journal: the accounts under Assets:hw are those of the portfolios, whose balances the journal states"

    -- In TOML, the line of the unit belongs to the table above it. It
    -- would give a tag named unit the account sat, and leave the unit of
    -- the amounts as it was, without a word.
    it "cannot have a line for a setting of hw, which was most likely written below the table by mistake" $
      decodeConfig "[journal-accounts]\nrent = \"Expenses:Housing:Rent\"\nunit = \"sat\"\n"
        `shouldBe` Left "at line 3: the key \"journal-accounts\" has a line for \"unit\", which is a setting of hw and cannot have an account. If you meant the setting, move the line above the header [journal-accounts]: every line below a header belongs to its table."

directorySpec :: Spec
directorySpec =
  describe "The data directory" $ do
    it "holds the database, portfolios.sqlite" $
      databaseFile (configIn "/home/someone/.hw") `shouldBe` "/home/someone/.hw/portfolios.sqlite"

    it "holds the journal hw keeps for ledger, portfolios.ledger, next to the database" $
      journalFile (configIn "/home/someone/.hw") `shouldBe` "/home/someone/.hw/portfolios.ledger"

    it "holds the configuration file, config.toml" $
      configFile "/home/someone/.hw" `shouldBe` "/home/someone/.hw/config.toml"

    -- Only the setup writes the file: it asks the user what to put in it
    it "is refused without a configuration file, with the way to set hw up, and gets no file" $
      inScratchDirectory $ \dir -> do
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Left ("hw is not set up yet: there is no configuration file at " <> (dir </> "config.toml") <> ". Run hw without a command to set it up.")
        doesFileExist (dir </> "config.toml") `shouldReturn` False

    it "is not created by a command that finds it missing" $
      inScratchDirectory $ \dir -> do
        _ <- runExceptT $ readConfig (dir </> ".hw")
        doesPathExist (dir </> ".hw") `shouldReturn` False

    it "has a configuration file once one is written in it, and not before" $
      inScratchDirectory $ \dir -> do
        hasConfigFile dir `shouldReturn` False
        Text.IO.writeFile (dir </> "config.toml") "gap = 30\n"
        hasConfigFile dir `shouldReturn` True

    it "keeps the configuration file it has, and reads it" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) twoRequestsFile
        cfg <- runExceptT $ readConfig dir
        configParallelRequests . fst <$> cfg `shouldBe` Right 2
        Text.IO.readFile (configFile dir) `shouldReturn` twoRequestsFile

    it "refuses a configuration file that does not read, naming it, the line and the reason" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "gap = 0\n"
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Left ("The configuration file " <> configFile dir <> " could not be read, at line 1: the key \"gap\" must be a whole number from 1 to 1000")

    -- The configuration read works on the directory it was read from
    it "reads the defaults from an empty configuration file, without a warning" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) ""
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (configIn dir, [])

    it "warns about a mistyped key, whose setting would otherwise keep its default without a word" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "gaps = 30\n"
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (configIn dir, ["The configuration file " <> configFile dir <> " has keys hw does not know, left aside: gaps"])

    -- The pager is no key of the file: HW_PAGER names it
    it "warns once about all the keys it does not know, in the order of the alphabet, and reads the others" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "pager = \"less\"\ngap = 30\ngaps = 5\n"
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right ((configIn dir) {configGap = 30}, ["The configuration file " <> configFile dir <> " has keys hw does not know, left aside: gaps, pager"])

    -- The file hw writes shows the header of the table as a comment. A
    -- line for a tag written below it, with the # left in place, is a key
    -- at the top of the file, which gives no tag an account.
    it "warns that the header [journal-accounts] is still a comment when a line for a tag is under it, and says to remove the #" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "journal = true\n\n# Default: no tag\n# [journal-accounts]\nfaucet = \"Income:Faucet\"\n"
        cfg <- runExceptT $ readConfig dir
        cfg
          `shouldBe` Right
            ( (configIn dir) {configJournal = True},
              [ "The configuration file "
                  <> configFile dir
                  <> " has lines under the header [journal-accounts], which is still a comment, so hw left them aside: faucet. Remove the # before [journal-accounts] to give each tag its account."
              ]
            )

    it "names every line under the commented header [journal-accounts], in the order of the file" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "#[journal-accounts]\nsalary = \"Income:Salary\"\nfaucet = \"Income:Faucet\"\n"
        warnings <- fmap snd <$> runExceptT (readConfig dir)
        warnings
          `shouldBe` Right
            [ "The configuration file "
                <> configFile dir
                <> " has lines under the header [journal-accounts], which is still a comment, so hw left them aside: salary, faucet. Remove the # before [journal-accounts] to give each tag its account."
            ]

    -- gaps is above the commented header, so it is a mistyped key as any
    -- other
    it "keeps the warning about a mistyped key apart from the one about the commented header" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "gaps = 5\n# [journal-accounts]\nfaucet = \"Income:Faucet\"\n"
        warnings <- fmap snd <$> runExceptT (readConfig dir)
        warnings
          `shouldBe` Right
            [ "The configuration file "
                <> configFile dir
                <> " has lines under the header [journal-accounts], which is still a comment, so hw left them aside: faucet. Remove the # before [journal-accounts] to give each tag its account.",
              "The configuration file " <> configFile dir <> " has keys hw does not know, left aside: gaps"
            ]

    it "reads the line of a tag without a warning once the # before the header is removed" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "[journal-accounts]\nfaucet = \"Income:Faucet\"\n"
        warnings <- fmap snd <$> runExceptT (readConfig dir)
        warnings `shouldBe` Right []

    -- hw read its settings from this file before, and does not any more
    it "does not read a config.json, and leaves it as it is" $
      inScratchDirectory $ \dir -> do
        writeFile (dir </> "config.json") "{\"gap\": 30}"
        Text.IO.writeFile (configFile dir) ""
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right (configIn dir, [])
        readFile (dir </> "config.json") `shouldReturn` "{\"gap\": 30}"

    it "is not set up by a config.json alone" $
      inScratchDirectory $ \dir -> do
        writeFile (dir </> "config.json") "{\"gap\": 30}"
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Left ("hw is not set up yet: there is no configuration file at " <> (dir </> "config.toml") <> ". Run hw without a command to set it up.")

    it "is created when missing" $
      inScratchDirectory $ \dir -> do
        opened <- runExceptT $ openDataDirectory (dir </> ".hw")
        opened `shouldBe` Right (dir </> ".hw")
        doesDirectoryExist (dir </> ".hw") `shouldReturn` True

    -- ~/.hw made a link to a directory on another disk, to keep the
    -- wallet there
    it "may be a link to a directory elsewhere, which then gets the configuration file" $
      inScratchDirectory $ \dir -> do
        createDirectory (dir </> "drive")
        createDirectoryLink (dir </> "drive") (dir </> ".hw")
        opened <- runExceptT $ openDataDirectory (dir </> ".hw")
        opened `shouldBe` Right (dir </> ".hw")
        _ <- runExceptT $ writeFirstConfig (dir </> ".hw") []
        doesFileExist (dir </> "drive" </> "config.toml") `shouldReturn` True

    -- A command looks at the link before it says that hw is not set up:
    -- the wallet is on the drive
    it "is refused by its link alone when the link leads nowhere, and nothing is created" $
      inScratchDirectory $ \dir -> do
        createDirectoryLink (dir </> "drive") (dir </> ".hw")
        checked <- runExceptT $ checkDataDirectoryLink (dir </> ".hw")
        checked `shouldBe` Left ("The directory " <> (dir </> ".hw") <> " links to " <> (dir </> "drive") <> ", which does not exist. If it is on a drive, mount the drive and try again.")
        doesPathExist (dir </> "drive") `shouldReturn` False

    it "passes the check of its link when it is not there at all, and is not created by it" $
      inScratchDirectory $ \dir -> do
        checked <- runExceptT $ checkDataDirectoryLink (dir </> ".hw")
        checked `shouldBe` Right ()
        doesPathExist (dir </> ".hw") `shouldReturn` False

    -- A drive that is not mounted leaves such a link. A new directory
    -- there would hold a new, empty wallet on another disk.
    it "is refused when it links to a directory that does not exist, which it does not create" $
      inScratchDirectory $ \dir -> do
        createDirectoryLink (dir </> "drive") (dir </> ".hw")
        opened <- runExceptT $ openDataDirectory (dir </> ".hw")
        opened `shouldBe` Left ("The directory " <> (dir </> ".hw") <> " links to " <> (dir </> "drive") <> ", which does not exist. If it is on a drive, mount the drive and try again.")
        doesPathExist (dir </> "drive") `shouldReturn` False

    it "is refused when it links to a file" $
      inScratchDirectory $ \dir -> do
        writeFile (dir </> "notes.txt") "my notes\n"
        createFileLink (dir </> "notes.txt") (dir </> ".hw")
        opened <- runExceptT $ openDataDirectory (dir </> ".hw")
        opened `shouldBe` Left ("The directory " <> (dir </> ".hw") <> " links to " <> (dir </> "notes.txt") <> ", which is not a directory")

    -- The home directory it goes in is not there
    it "is refused when it cannot be created, with the reason the system gives" $
      inScratchDirectory $ \dir -> do
        opened <- runExceptT $ openDataDirectory (dir </> "no-home" </> ".hw")
        opened `shouldBe` Left ("The directory " <> (dir </> "no-home" </> ".hw") <> " could not be created: no such file or directory")

-- | The file the setup writes in a data directory that has none, from
-- what the user chose
firstFileSpec :: Spec
firstFileSpec =
  describe "The first configuration file of a data directory" $ do
    it "reads as the configuration the user chose" $
      (configOfText =<< newConfigFileWith chosenKeys)
        `shouldBe` Right (def {configShownNetworks = [btc, btcTest4], configDefaultNetwork = btcTest4, configDefaultType = Bip84}, [])

    it "sets each choice on a line below the comment that shows the default of its key" $
      (Text.lines <$> newConfigFileWith [("default-type", "bip84")])
        `shouldSatisfy` either (const False) (isInfixOf ["# Default: \"bip44\"", "# default-type = \"bip44\"", "default-type = \"bip84\"", ""])

    it "is the file of every setting as a comment, with those lines more and no other change" $
      (filter (/= "default-type = \"bip84\"") . Text.lines <$> newConfigFileWith [("default-type", "bip84")])
        `shouldBe` Right (Text.lines newFile)

    it "is that file as it is when nothing was chosen" $
      newConfigFileWith [] `shouldBe` Right newFile

    it "is written in a data directory that is created for it, and its path is given" $
      inScratchDirectory $ \dir -> do
        written <- runExceptT $ writeFirstConfig (dir </> ".hw") chosenKeys
        written `shouldBe` Right (dir </> ".hw" </> "config.toml")
        cfg <- runExceptT $ readConfig (dir </> ".hw")
        (configDefaultNetwork . fst <$> cfg) `shouldBe` Right btcTest4

    -- The file may hold what the user wrote in it
    it "is not written over a configuration file that is there" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) twoRequestsFile
        written <- runExceptT $ writeFirstConfig dir chosenKeys
        written `shouldBe` Left ("The file " <> configFile dir <> " could not be written: file exists")
        Text.IO.readFile (configFile dir) `shouldReturn` twoRequestsFile

    -- A directory stands where the file goes
    it "says why it could not be written" $
      inScratchDirectory $ \dir -> do
        createDirectory (configFile dir)
        written <- runExceptT $ writeFirstConfig dir chosenKeys
        written `shouldBe` Left ("The file " <> configFile dir <> " could not be written: is a directory")
  where
    -- btc and btctest4 shown, btctest4 the default network, bip84 the
    -- default type
    chosenKeys =
      [ ("shown-networks", Toml.List ["btc", "btctest4"]),
        ("default-network", "btctest4"),
        ("default-type", "bip84")
      ]

-- | The keys a command changes in the file: setdefault writes the defaults
-- the user chooses
changeSpec :: Spec
changeSpec =
  describe "Setting keys of the configuration file" $ do
    it "names the two keys that say where in a portfolio a command works as the file writes them" $
      [defaultNetworkKey, defaultTypeKey] `shouldBe` ["default-network", "default-type"]

    it "names the keys of the unit, of the view of the listings and of their reverse order as the file writes them" $
      [unitKey, listingViewKey, listingReverseKey] `shouldBe` ["unit", "listing-view", "listing-reverse"]

    it "names the key of the networks hw shows as the file writes it" $
      shownNetworksKey `shouldBe` "shown-networks"

    it "names the key of the journal hw keeps as the file writes it" $
      journalKey `shouldBe` "journal"

    it "names the table that gives tags an account of the journal as the file writes it" $
      journalAccountsKey `shouldBe` "journal-accounts"

    -- The table is the last setting of the file hw writes. The user
    -- removed the # before its header and wrote a line below it.
    it "adds a key above the table of the file hw writes, below the comment that shows its default, and the table keeps its lines" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) (Text.replace "# [journal-accounts]\n" "[journal-accounts]\nrent = \"Expenses:Housing:Rent\"\n" newFile)
        changed <- runExceptT $ setConfigKeys dir [("unit", "sat")]
        changed `shouldBe` Right ()
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right ((configIn dir) {configUnit = UnitSatoshi, configJournalAccounts = Map.fromList [(rentTag, "Expenses:Housing:Rent")]}, [])

    it "changes the keys given, which the configuration then reads" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) newFile
        changed <- runExceptT $ setConfigKeys dir [("default-type", "bip84"), ("default-network", "bch")]
        changed `shouldBe` Right ()
        cfg <- runExceptT $ readConfig dir
        cfg `shouldBe` Right ((configIn dir) {configDefaultType = Bip84, configDefaultNetwork = bch}, [])

    -- The file sets a gap of 30 with a comment, and has a mistyped key
    it "leaves every other character of the file as it is: the comments, and the keys hw does not know" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "# My settings\ngap = 30  # many addresses\ndefault-network = \"btc\"\n\ngaps = 5\n"
        changed <- runExceptT $ setConfigKeys dir [("default-network", "bch")]
        changed `shouldBe` Right ()
        Text.IO.readFile (configFile dir)
          `shouldReturn` "# My settings\ngap = 30  # many addresses\ndefault-network = \"bch\"\n\ngaps = 5\n"

    it "adds a key the file did not have at its end, and no other" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "gap = 30\n"
        changed <- runExceptT $ setConfigKeys dir [("default-type", "bip84")]
        changed `shouldBe` Right ()
        Text.IO.readFile (configFile dir) `shouldReturn` "gap = 30\ndefault-type = \"bip84\"\n"

    -- The file is then the one hw wrote, with one line more
    it "adds a key to the file hw writes below the comment that shows its default" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) newFile
        changed <- runExceptT $ setConfigKeys dir [("unit", "sat")]
        changed `shouldBe` Right ()
        written <- Text.lines <$> Text.IO.readFile (configFile dir)
        written `shouldSatisfy` isInfixOf ["# Default: \"btc\"", "# unit = \"btc\"", "unit = \"sat\"", ""]
        filter (/= "unit = \"sat\"") written `shouldBe` Text.lines newFile

    it "changes nothing when no key is given" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) twoRequestsFile
        changed <- runExceptT $ setConfigKeys dir []
        changed `shouldBe` Right ()
        Text.IO.readFile (configFile dir) `shouldReturn` twoRequestsFile

    -- The gap could be set, but the unit would land in the table
    it "writes none of the keys when one of them cannot be written, and gives the line to write by hand" $
      inScratchDirectory $ \dir -> do
        Text.IO.writeFile (configFile dir) "gap = 30\n\n[backups]\nkeep = 2\n"
        changed <- runExceptT $ setConfigKeys dir [("gap", Toml.Integer 40), ("unit", "sat")]
        changed
          `shouldBe` Left ("The configuration file " <> configFile dir <> " has a table at line 3, and the key \"unit\" must come before it. hw left it as it is. Change it by hand, with this line: unit = \"sat\"")
        Text.IO.readFile (configFile dir) `shouldReturn` "gap = 30\n\n[backups]\nkeep = 2\n"

-- | What hw makes of the text of a configuration file
decodeConfig :: Text -> Either String Config
decodeConfig = fmap fst . configOfText

-- | The file of a data directory that has none, before the setup sets in
-- it what the user chose
newFile :: Text
newFile =
  [r|# The settings of hw.
#
# A line that starts with # is a comment. Each setting is written below as a
# comment, with its default. hw takes the default of a setting until you set
# it. To set one, remove the # before its line and change its value, or add a
# line of your own. hw setdefault sets some of them for you, and keeps your
# comments.

# The version of the format of this file. Leave it as it is.
version = 1

# The address of the haskoin-store server that hw asks about the chain, with
# or without a slash at the end.
# Default: "https://api.haskoin.com/"
# host = "https://api.haskoin.com/"

# The network of a command given no --network. It must be one of
# shown-networks. hw setdefault --network sets it.
# Accepted: "btc", "bch", "btctest", "btctest4", "btcreg", "bchtest",
# "bchtest4", "bchreg"
# Default: "btc"
# default-network = "btc"

# The networks hw shows. The others are hidden: left out of every answer, and
# refused as a --network, while sync and recover still work on them. hw
# setdefault --show-network and --hide-network change it.
# Accepted: a list of one or more of "btc", "bch", "btctest", "btctest4",
# "btcreg", "bchtest", "bchtest4", "bchreg"
# Default: ["btc", "bch"]
# shown-networks = ["btc", "bch"]

# The type of address of a command given no --type. The networks of Bitcoin
# Cash take bip44 whatever it says. hw setdefault --type sets it.
# Accepted: "bip44", "bip84"
# Default: "bip44"
# default-type = "bip44"

# The unit of the amounts of a command given no --unit: of the amounts shown,
# and of those typed without a unit. A bit is 100 satoshis. hw setdefault
# --unit sets it.
# Accepted: "btc", "bit", "sat"
# Default: "btc"
# unit = "btc"

# The view of a listing given no --compact, --verbose or --tabular. hw
# setdefault with one of these options sets it.
# Accepted: "compact", "verbose", "tabular"
# Default: "compact"
# listing-view = "compact"

# Whether a listing given no --reverse or --no-reverse lists its rows in the
# reverse order on the screen: the oldest transaction first, for instance.
# --json and --csv keep the usual order. hw setdefault --reverse and
# --no-reverse set it.
# Accepted: true, false
# Default: false
# listing-reverse = false

# How many unused addresses in a row receive gives out and sync looks at, in
# each account.
# Accepted: a whole number from 1 to 1000
# Default: 20
# gap = 20

# How many requests go to the server at once. The public server takes one at a
# time, so keep it at 1 unless you run your own haskoin-store.
# Accepted: a whole number from 1 to 100
# Default: 1
# parallel-requests = 1

# Whether hw copies the database into auto-backups after each command that
# adds a wallet or a portfolio. It also covers the backup hw makes there
# before deletewallet, deleteportfolio and restoredatabase.
# Accepted: true, false
# Default: true
# autobackup-database = true

# Whether hw copies the labels into auto-backups after each command that
# changes them.
# Accepted: true, false
# Default: true
# autobackup-labels = true

# Whether hw keeps a journal of the transactions for ledger, the accounting
# tool, in portfolios.ledger next to the database, and writes it again after
# each command that changes it. hw setdefault --journal and --no-journal set
# it.
# Accepted: true, false
# Default: false
# journal = false

# The accounts your tags stand for in the journal for ledger. To give a tag an
# account, remove the # before [journal-accounts] below, and write a line for
# the tag under it. hw does not read a line under a header that is still a
# comment. A payment with the tag, or to a contact with the tag, is then
# posted to that account, where it would go to Expenses:Unknown or
# Income:Unknown. Keep this table the last setting of the file: every line
# below its header belongs to it.
# Accepted: lines such as rent = "Expenses:Housing:Rent"
# Default: no tag
# [journal-accounts]
|]

-- | The lines of the file hw writes that show a setting, each without
-- the # before it
everyDefaultSet :: Text
everyDefaultSet =
  [r|host = "https://api.haskoin.com/"
default-network = "btc"
shown-networks = ["btc", "bch"]
default-type = "bip44"
unit = "btc"
listing-view = "compact"
listing-reverse = false
gap = 20
parallel-requests = 1
autobackup-database = true
autobackup-labels = true
journal = false
[journal-accounts]
|]

-- | A file asking for two requests at a time, with a comment
twoRequestsFile :: Text
twoRequestsFile = "# My own store takes two requests at once\nparallel-requests = 2\n"
