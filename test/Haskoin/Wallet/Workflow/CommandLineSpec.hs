{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The command line of hw as the user meets it: the help, the version,
-- a command line hw cannot parse, among them the commands and options
-- that portfolios and the active portfolio replaced, the completion the
-- shell asks for, and how a command that fails says so and leaves nothing
-- behind.
module Haskoin.Wallet.Workflow.CommandLineSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (modifyIORef)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Database (databaseInUse, runDBWith)
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (MnemonicPass (..))
import Haskoin.Wallet.Migration (databaseVersion)
import Haskoin.Wallet.Migration.TestUtils (version2Database)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Async (async, wait)

spec :: Spec
spec = prepareContext $ \ctx -> do
  commandLineSpec ctx

commandLineSpec :: Ctx -> Spec
commandLineSpec ctx = do
  describe "The help" $ do
    it "of a command is shown with --help, its summary first, and hw exits with success" $
      withHome $ \home -> do
        run <- hw home ["signtx", "--help"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldSatisfy` Text.isPrefixOf "Sign a pending transaction or a transaction file\n"
        runOut run `shouldSatisfy` Text.isInfixOf "Usage: hw signtx"
        runOut run `shouldSatisfy` Text.isInfixOf "-i, --input FILENAME"

    it "of a command that asks for a mnemonic says to run it on an offline computer" $
      withHome $ \home -> do
        run <- hw home ["signtx", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "This command asks for a mnemonic or private keys.\nRun it on an offline computer if you can.\n"

    it "of a command lists its options under their subheadings" $
      withHome $ \home -> do
        run <- hw home ["preparetx", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n\n  Change\n      --change-back "

    -- --help is the last of them, and Fee the first subheading of preparetx
    it "of a command lists the options without a subheading first" $
      withHome $ \home -> do
        run <- hw home ["preparetx", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "Show this help text\n\n  Fee\n  -f, --fee-rate RATE "

    -- The text of preparetx names them in this order. It names send in
    -- backticks, since send is an everyday word too.
    it "of a command ends with the commands its text names" $
      withHome $ \home -> do
        run <- hw home ["preparetx", "--help"] []
        runOut run `shouldSatisfy` Text.isSuffixOf "See also: signtx, broadcast, send, pendingtxs, deletetx, bumpfee\n"

    it "of the program lists the commands by group with --help" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldSatisfy` Text.isInfixOf "Usage: hw COMMAND"
        runOut run `shouldSatisfy` Text.isInfixOf "Payments\n  fees "

    -- In a pipe, the help has no colours to show them
    it "marks the commands that ask for a mnemonic or private keys with ! when plain" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "The commands marked ! ask for a mnemonic or private keys."
        runOut run `shouldSatisfy` Text.isInfixOf "! Make a mnemonic and its wallet, or add a portfolio"
        runOut run `shouldSatisfy` (not . Text.isInfixOf "! Send a signed transaction")

    -- recover takes a mnemonic, and preparesweep private keys, but neither
    -- can run on an offline computer. A mark would sit before the summary.
    it "does not mark a command that needs the server, even one that takes a secret" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "  recover                   Find the portfolios of a mnemonic that were paid\n"
        runOut run `shouldSatisfy` Text.isInfixOf "  preparesweep              Prepare a transaction that moves coins"

    it "of the program names the commands that take a secret on an online computer" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "recover and preparesweep --key-file take a mnemonic or private keys too"

    it "of recover does not say to run it on an offline computer" $
      withHome $ \home -> do
        run <- hw home ["recover", "--help"] []
        runOut run `shouldSatisfy` (not . Text.isInfixOf "Run it on an offline computer")

    it "of recover warns that the mnemonic is typed on an online computer" $
      withHome $ \home -> do
        run <- hw home ["recover", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "recover needs the mnemonic and the server at the same time, so it runs on an\nonline computer"

    it "of recover says how to keep the mnemonic offline" $
      withHome $ \home -> do
        run <- hw home ["recover", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "To keep the mnemonic offline, create the\nportfolios on the offline computer with newportfolio --existing instead."

    it "of preparesweep warns that --key-file reads the keys on an online computer" $
      withHome $ \home -> do
        run <- hw home ["preparesweep", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "preparesweep needs the server, so it runs on an online computer"
        runOut run `shouldSatisfy` Text.isInfixOf "With --key-file, the private keys are read on that computer too"

    it "of preparesweep says how to keep the keys offline" $
      withHome $ \home -> do
        run <- hw home ["preparesweep", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "Name the coins\nwith --sweep-from, --address-file or --coin instead."

    it "of the --key-file of preparesweep says to leave it out if one can" $
      withHome $ \home -> do
        run <- hw home ["preparesweep", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "on an online computer, so leave it out if you can."

    -- signsweep can run on an offline computer, so the warning is not its
    it "of the --key-file of signsweep says nothing of an online computer" $
      withHome $ \home -> do
        run <- hw home ["signsweep", "--help"] []
        runOut run `shouldSatisfy` (not . Text.isInfixOf "online computer")

    it "has no colour codes when written to a pipe" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runOut run `shouldSatisfy` (not . Text.isInfixOf "\ESC")

    -- The help of every option starts at column 28
    it "of the program lists --version under --help" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "  -h, --help                Show this help text\n      --version             Show the version of hw\n"

    -- --unit and --json go before or after the command, and --help too
    it "of a command is shown with --help after its options" $
      withHome $ \home -> do
        run <- hw home ["--unit", "sat", "sync", "--json", "--help"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldSatisfy` Text.isInfixOf "Usage: hw sync"

    it "of a command is shown when it is typed alone and needs arguments, as a failure" $
      withHome $ \home -> do
        run <- hw home ["preparetx"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isPrefixOf "Prepare a payment, without signing it\n"
        runErr run `shouldSatisfy` Text.isInfixOf "Options:"

    it "of the program is shown when nothing is typed, as a failure" $
      withHome $ \home -> do
        run <- hw home [] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Wallets and portfolios"

    -- label runs with --help as the label, and finds no address in an
    -- empty wallet
    it "is not shown for a --help after --, which is an argument" $
      withHome $ \home -> do
        run <- hw home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--", "--help"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The wallet has no address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"

    -- The groups of the README, in its order
    it "of the program lists the commands in their groups, wallets and portfolios first" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        filter (`elem` helpGroups) (Text.lines (runOut run)) `shouldBe` helpGroups

    it "of the program lists the commands of wallets and portfolios in their group, in the order of the README" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        let commands = ["newportfolio", "recover", "checkmnemonic", "exportwallet", "importwallet", "portfolios", "switch", "setdefault", "deleteportfolio", "deletewallet", "sync"]
            group = takeWhile (/= "Addresses") (drop 1 (dropWhile (/= "Wallets and portfolios") (Text.lines (runOut run))))
        [word | line <- group, Just word <- [listToMaybe (Text.words line)], word `elem` commands] `shouldBe` commands

  describe "The version" $ do
    it "of hw and of its database format is shown with --version, and hw exits with success" $
      withHome $ \home -> do
        run <- hw home ["--version"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldBe` "Software version: 0.10.0\nDatabase version: 5\n"

    it "is shown without touching the database" $
      withHome $ \home -> do
        _ <- hw home ["--version"] []
        doesFileExist (home </> ".hw" </> "accounts.sqlite") `shouldReturn` False

    it "is not an option of a command, which refuses it" $
      withHome $ \home -> do
        run <- hw home ["sync", "--version"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid option `--version'"

  describe "A command line hw cannot parse" $ do
    it "names --tabular when it follows --verbose in txs, since the two are a choice" $
      withHome $ \home -> do
        run <- hw home ["txs", "--verbose", "--tabular"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid option `--tabular'"

    it "names --csv when it follows --tabular in txs, since the screen and the file are a choice" $
      withHome $ \home -> do
        run <- hw home ["txs", "--tabular", "--csv", home </> "txs.csv"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid option `--csv'"

    it "names the unknown command, and suggests the closest one" $
      withHome $ \home -> do
        run <- hw home ["preparetxx"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid argument `preparetxx'"
        runErr run `shouldSatisfy` Text.isInfixOf "Did you mean this?\n    preparetx"

    it "says what is wrong with the value of an option, then the usage of the command" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--shares", "20"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Error: option --shares: The --shares value has to be between 1 and 16"
        runErr run `shouldSatisfy` Text.isInfixOf "Usage: hw newportfolio"

    it "ends with the help to read" $
      withHome $ \home -> do
        run <- hw home ["newportfolio", "--shares", "20"] []
        runErr run `shouldSatisfy` Text.isSuffixOf "Type hw newportfolio --help for the help of newportfolio.\n"

    it "refuses a word of 9 digits for a hash, which is neither a short id nor a whole hash" $
      withHome $ \home -> do
        run <- hw home ["deletetx", "a1075db5c"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "The hash a1075db5c is not valid: give its 64 digits, or its short id, its first 8 digits, as the listings show it"

    -- preparesweep fetches the coin from the server, which looks up whole
    -- txids only
    it "refuses a coin to sweep given by its short id" $
      withHome $ \home -> do
        run <- hw home ["preparesweep", "--sweep-to", textOf (otherAddr 0), "--coin", "a1075db5:0"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "The coin a1075db5:0 needs its txid in full: the wallet does not hold a coin it sweeps, and the server looks up a whole txid only"

    it "says what a command misses" $
      withHome $ \home -> do
        run <- hw home ["exporttx", Text.unpack (txHashToHex (fakeTxHash 1))] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Missing: FILENAME"
        runErr run `shouldSatisfy` Text.isInfixOf "Usage: hw exporttx HASH FILENAME"

    it "says what a command misses, even when an option of every command follows" $
      withHome $ \home -> do
        run <- hw home ["label", "--json"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Missing: LABEL"

    -- newportfolio, portfolios, importwallet, exportwallet, label and
    -- deleteportfolio replace them
    it "refuses the commands of accounts that portfolios replaced, as commands it does not know" $
      withHome $ \home ->
        forM_ ["createacc", "renameacc", "deleteacc", "importacc", "exportacc", "accounts", "mnemonic"] $ \command -> do
          run <- hw home [command] []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf ("Invalid argument `" <> Text.pack command <> "'")

    -- fillwallet filled in the portfolios the step from hw 0.9 left
    -- partial: hw 0.10 reads no database of hw 0.9, and has no partial
    -- portfolio
    it "refuses fillwallet, as a command it does not know" $
      withHome $ \home -> do
        run <- hw home ["fillwallet"] seedOnce
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid argument `fillwallet'"

    it "refuses --account, which the active portfolio replaces" $
      withHome $ \home -> do
        run <- hw home ["receive", "--account", "main"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid option `--account'"

    -- The commands work in the active portfolio, which switch changes
    it "refuses --wallet and --portfolio for the commands that work in the active portfolio" $
      withHome $ \home ->
        refusesOptions home ["--wallet", "--portfolio"] ["receive", "addresses", "balances", "coins", "txs", "pendingtxs", "send", "preparetx", "preparesendall", "checkmnemonic", "setdefault"]

    it "refuses --wallet and --portfolio for the commands that cover every portfolio" $
      withHome $ \home ->
        refusesOptions home ["--wallet", "--portfolio"] ["portfolios", "sync", "exportsheet"]

    it "refuses --wallet and --portfolio for the commands that find their portfolio by themselves" $
      withHome $ \home ->
        refusesOptions home ["--wallet", "--portfolio"] ["importtx", "signtx", "signsweep", "bumpfee", "preparesweep", "label", "lockcoin", "unlockcoin", "signmsg"]

    -- Its --portfolio names a portfolio of the wallet of the active
    -- portfolio. The other commands that keep --portfolio take --wallet
    -- beside it.
    it "refuses --wallet for deleteportfolio" $
      withHome $ \home ->
        refusesOptions home ["--wallet"] ["deleteportfolio"]

    -- deletewallet deletes a whole wallet
    it "refuses --portfolio for deletewallet" $
      withHome $ \home ->
        refusesOptions home ["--portfolio"] ["deletewallet"]

    it "refuses --new-change, which --change-back ADDRESS replaces" $
      withHome $ \home -> do
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--new-change"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid option `--new-change'"

    -- recover searches every network
    it "refuses --network for recover, and --account-gap, which --portfolio-gap replaces" $
      withHome $ \home ->
        forM_ [("--network", "btc"), ("--account-gap", "2")] $ \(option, value) -> do
          run <- hw home ["recover", option, value] []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf ("Invalid option `" <> Text.pack option <> "'")

  describe "The completion the shell asks for" $ do
    it "offers an option of every command after the command" $
      withHome $ \home -> do
        run <- hw home (tabAfter ["txs", "-nbtc", "--js"]) []
        runOut run `shouldBe` "--json\n"

    it "offers --help after the command" $
      withHome $ \home -> do
        run <- hw home (tabAfter ["txs", "--he"]) []
        runOut run `shouldBe` "--help\n"

    it "offers --version right after hw" $
      withHome $ \home -> do
        run <- hw home (tabAfter ["--v"]) []
        runOut run `shouldBe` "--version\n"

    -- txs has --verbose
    it "does not offer --version after a command, which refuses it" $
      withHome $ \home -> do
        run <- hw home (tabAfter ["txs", "--v"]) []
        runOut run `shouldBe` "--verbose\n"

    it "offers the indexes of the portfolios of the wallet of the active portfolio after the --portfolio of switch" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        run <- hw home (tabAfter ["switch", "--portfolio", ""]) []
        runOut run `shouldBe` "0\n1\n"

    -- wallet1 has its portfolio0 alone, wallet0 portfolio0 and portfolio1
    it "offers the indexes of the portfolios of the wallet of the active portfolio after a switch to another wallet" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        switchTo home 1 0
        run <- hw home (tabAfter ["switch", "--portfolio", ""]) []
        runOut run `shouldBe` "0\n"

    it "offers the indexes of the portfolios after the --portfolio of deleteportfolio and of exportwallet too" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        forM_ ["deleteportfolio", "exportwallet"] $ \command -> do
          run <- hw home (tabAfter [command, "--portfolio", ""]) []
          (command, runOut run) `shouldBe` (command, "0\n1\n")

    -- The wallet is one of the file, named by its fingerprint, which the
    -- database may not have
    it "offers no value after the --wallet of importwallet and of show, which names a wallet of a file" $
      withHome $ \home -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        forM_ ["importwallet", "show"] $ \command -> do
          run <- hw home (tabAfter [command, "--wallet", ""]) []
          (command, runOut run) `shouldBe` (command, "")

    -- The portfolio is one of the file, which the database may not have
    it "offers no index after the --portfolio of importwallet and of show, which names a portfolio of a file" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        forM_ ["importwallet", "show"] $ \command -> do
          run <- hw home (tabAfter [command, "--portfolio", ""]) []
          (command, runOut run) `shouldBe` (command, "")

    it "offers the indexes of the wallets after the --wallet of switch" $
      withHome $ \home -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        run <- hw home (tabAfter ["switch", "--wallet", ""]) []
        runOut run `shouldBe` "0\n1\n"

    it "offers the indexes of the wallets after the --wallet of deletewallet" $
      withHome $ \home -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        run <- hw home (tabAfter ["deletewallet", "--wallet", ""]) []
        runOut run `shouldBe` "0\n1\n"

    it "offers the indexes of the wallets after the --wallet of exportwallet" $
      withHome $ \home -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        run <- hw home (tabAfter ["exportwallet", "--wallet", ""]) []
        runOut run `shouldBe` "0\n1\n"

    -- wallet1 has its portfolio0 alone, wallet0 portfolio0 and portfolio1
    it "offers after the --portfolio of exportwallet the portfolios of the wallet its --wallet names" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        run <- hw home (tabAfter ["exportwallet", "--wallet", "1", "--portfolio", ""]) []
        runOut run `shouldBe` "0\n"

    -- wallet1 has its portfolio0 alone, wallet0 portfolio0 and portfolio1
    it "offers the indexes of the portfolios of the wallet --wallet names before --portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        run <- hw home (tabAfter ["switch", "--wallet", "1", "--portfolio", ""]) []
        runOut run `shouldBe` "0\n"

    -- The index replaces the word typed, the option before it
    it "offers the index of a portfolio after -p in the same word, -p before it, only when the wallet has it" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        run <- hw home (tabAfter ["switch", "-p1"]) []
        runOut run `shouldBe` "-p1\n"
        missing <- hw home (tabAfter ["switch", "-p2"]) []
        runOut missing `shouldBe` ""

    -- zsh and fish keep --portfolio= one word, which the index offered
    -- replaces whole
    it "offers the indexes of the portfolios after --portfolio= in the same word, --portfolio= before each" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        run <- hw home ("--bash-completion-enriched" : tabAfter ["switch", "--portfolio="]) []
        runOut run `shouldBe` "--portfolio=0\n--portfolio=1\n"

    -- The home of a test shows every network
    it "offers the networks that start with the word typed after --network" $
      withHome $ \home -> do
        run <- hw home (tabAfter ["receive", "--network", "btct"]) []
        runOut run `shouldBe` "btctest\nbtctest4\n"

    -- The commands refuse a hidden network
    it "offers the networks hw shows after --network, and no hidden one" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch", "btctest4"]
        run <- hw home (tabAfter ["receive", "--network", ""]) []
        runOut run `shouldBe` "btc\nbch\nbtctest4\n"

    it "offers the hidden networks after setdefault --show-network" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        run <- hw home (tabAfter ["setdefault", "--show-network", ""]) []
        runOut run `shouldBe` "btctest\nbtctest4\nbtcreg\nbchtest\nbchtest4\nbchreg\n"

    it "offers the networks hw shows after setdefault --hide-network" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        run <- hw home (tabAfter ["setdefault", "--hide-network", ""]) []
        runOut run `shouldBe` "btc\nbch\n"

    -- The address was handed out on bch, which is then hidden
    it "offers no address of a hidden network" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        shown <- hw home (tabAfter ["show", "qzvz"]) []
        runOut shown `shouldBe` "qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u\n"
        setShownNetworks home ["btc"]
        hidden <- hw home (tabAfter ["show", "qzvz"]) []
        runOut hidden `shouldBe` ""

    it "offers the two types after --type" $
      withHome $ \home -> do
        run <- hw home (tabAfter ["receive", "--type", ""]) []
        runOut run `shouldBe` "bip44\nbip84\n"

    -- txs has --verbose, which takes no value
    it "offers nothing after the = of an option that takes no value" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ("--bash-completion-enriched" : tabAfter ["txs", "--verbose=m"]) []
        runOut run `shouldBe` ""

    -- preparetx shows the nosigHash of the transaction it prepares
    it "offers the nosigHash of a transaction to sign after signtx" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home (tabAfter ["signtx", take 4 nosig]) []
        runOut run `shouldBe` Text.pack nosig <> "\n"

    -- Once signed, the transaction is shown by its id, which would be
    -- offered
    it "does not offer a signed transaction after signtx" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["signtx", take 4 txid]) []
        runOut run `shouldBe` ""

    it "offers the id of a signed transaction after broadcast" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["broadcast", take 4 txid]) []
        runOut run `shouldBe` Text.pack txid <> "\n"

    it "does not offer a transaction not signed yet after broadcast" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home (tabAfter ["broadcast", take 4 nosig]) []
        runOut run `shouldBe` ""

    -- The sync sends it again should the network lose it
    it "does not offer a transaction sent already after broadcast" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        txid <- sentPayment home
        run <- hw home (tabAfter ["broadcast", take 4 txid]) []
        runOut run `shouldBe` ""

    -- Another transaction of the mempool spends the coin of the payment,
    -- which the sync finds
    it "does not offer a transaction in conflict after broadcast, which the network would refuse" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- paymentSigned home
        let other = spendingTx ctx [(txHash (payment ctx), 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        _ <- hw home ["sync"] []
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["broadcast", take 4 txid]) []
        runOut run `shouldBe` ""

    it "offers a transaction in conflict after deletetx, which frees its coins" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- paymentSigned home
        let other = spendingTx ctx [(txHash (payment ctx), 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        _ <- hw home ["sync"] []
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["deletetx", take 4 txid]) []
        runOut run `shouldBe` Text.pack txid <> "\n"

    it "offers a pending transaction after exporttx by the hash pendingtxs shows, its id once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["exporttx", take 4 txid]) []
        runOut run `shouldBe` Text.pack txid <> "\n"

    -- The coin of the paid chain, as coins lists it, TXID:0, and as it is
    -- offered to bash, which inserts a word as it is: TXID\:0
    let paidTxid = Text.unpack (txHashToHex (txHash (payment ctx)))
        paidCoin = paidTxid <> ":0"
        offeredCoin = Text.pack paidTxid <> "\\:0\n"

    it "offers the coins free to lock after lockcoin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["lockcoin", take 4 paidCoin]) []
        runOut run `shouldBe` offeredCoin

    it "does not offer a coin the user locked after lockcoin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", paidCoin] []
        run <- hw home (tabAfter ["lockcoin", take 4 paidCoin]) []
        runOut run `shouldBe` ""

    it "offers the coins the user locked after unlockcoin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", paidCoin] []
        run <- hw home (tabAfter ["unlockcoin", take 4 paidCoin]) []
        runOut run `shouldBe` offeredCoin

    it "does not offer a free coin after unlockcoin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["unlockcoin", take 4 paidCoin]) []
        runOut run `shouldBe` ""

    it "offers the coins that can be spent now after --coin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["preparetx", "--coin", take 4 paidCoin]) []
        runOut run `shouldBe` offeredCoin

    -- send takes the options of preparetx, with what the shell offers
    -- for each
    it "offers the coins that can be spent now after --coin of send too" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["send", "--coin", take 4 paidCoin]) []
        runOut run `shouldBe` offeredCoin

    it "does not offer a locked coin after --coin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", paidCoin] []
        run <- hw home (tabAfter ["preparetx", "--coin", take 4 paidCoin]) []
        runOut run `shouldBe` ""

    -- bash completes the two coins up to their colon, which it escapes
    it "offers the coins of a transaction after the colon bash escaped" $ do
      let twoCoins = fundingTx ctx 1 [(externalAddr 0, 50000), (externalAddr 0, 20000)]
          txid = Text.unpack (txHashToHex (txHash twoCoins))
      withChain ctx (Chain 800002 [(twoCoins, Just 800000)]) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["lockcoin", txid <> "\\:"]) []
        runOut run `shouldBe` Text.pack (txid <> "\\:0\n" <> txid <> "\\:1\n")

    -- A colon typed as it is breaks the word in bash, which keeps the
    -- TXID: and inserts what is offered after it
    it "offers the indexes of the coins of a transaction after a colon typed after its txid, through the script of bash" $ do
      let twoCoins = fundingTx ctx 1 [(externalAddr 0, 50000), (externalAddr 0, 20000)]
          txid = Text.unpack (txHashToHex (txHash twoCoins))
      withChain ctx (Chain 800002 [(twoCoins, Just 800000)]) $ \home _ -> do
        synced home
        bashCompletion home ["lockcoin", txid, ":"] `shouldReturn` ["0", "1"]

    it "offers the indexes of the coins of a transaction after a colon typed after the short id of its txid, through the script of bash" $ do
      let twoCoins = fundingTx ctx 1 [(externalAddr 0, 50000), (externalAddr 0, 20000)]
          txid = Text.unpack (txHashToHex (txHash twoCoins))
      withChain ctx (Chain 800002 [(twoCoins, Just 800000)]) $ \home _ -> do
        synced home
        bashCompletion home ["lockcoin", take 8 txid, ":"] `shouldReturn` ["0", "1"]

    it "completes the index typed after the colon of a coin, through the script of bash" $ do
      let twoCoins = fundingTx ctx 1 [(externalAddr 0, 50000), (externalAddr 0, 20000)]
          txid = Text.unpack (txHashToHex (txHash twoCoins))
      withChain ctx (Chain 800002 [(twoCoins, Just 800000)]) $ \home _ -> do
        synced home
        bashCompletion home ["lockcoin", take 8 txid, ":", "1"] `shouldReturn` ["1"]

    -- zsh and fish pass the word whole, and get the coin as it was typed,
    -- by the short id of its txid
    it "offers the coins of a transaction after the short id of its txid and a colon, for zsh and fish" $ do
      let twoCoins = fundingTx ctx 1 [(externalAddr 0, 50000), (externalAddr 0, 20000)]
          short = take 8 (Text.unpack (txHashToHex (txHash twoCoins)))
      withChain ctx (Chain 800002 [(twoCoins, Just 800000)]) $ \home _ -> do
        synced home
        run <- hw home ("--bash-completion-enriched" : tabAfter ["lockcoin", short <> ":"]) []
        runOut run `shouldBe` Text.pack (short <> ":0\n" <> short <> ":1\n")

    -- Twenty-one payments to the first receiving address, one coin each
    let manyCoins n = Chain 800002 [(fundingTx ctx i [(externalAddr 0, 1000)], Just 800000) | i <- [1 .. n]]
        coinOf i = Text.unpack (txHashToHex (txHash (fundingTx ctx i [(externalAddr 0, 1000)])))

    it "offers nothing when more than twenty coins start with the word typed" $
      withChain ctx (manyCoins 21) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["lockcoin", ""]) []
        Text.lines (runOut run) `shouldSatisfy` all (Text.isPrefixOf "-")

    it "offers twenty coins" $
      withChain ctx (manyCoins 20) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["lockcoin", ""]) []
        length (filter (not . Text.isPrefixOf "-") (Text.lines (runOut run))) `shouldBe` 20

    -- The eight digits typed are those of one of the twenty-one
    it "offers the coins that start with the word typed, among more than twenty" $
      withChain ctx (manyCoins 21) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["lockcoin", take 8 (coinOf 7)]) []
        runOut run `shouldBe` Text.pack (coinOf 7) <> "\\:0\n"

    -- The first receiving address is paid, and twenty more are given out
    it "offers nothing when more than twenty addresses start with the word typed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        replicateM_ 20 (answer home ["receive"] [])
        run <- hw home (tabAfter ["label", "1"]) []
        runOut run `shouldBe` ""

    it "offers twenty addresses" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        replicateM_ 19 (answer home ["receive"] [])
        run <- hw home (tabAfter ["label", "1"]) []
        length (Text.lines (runOut run)) `shouldBe` 20

    it "offers the addresses that start with the word typed, among more than twenty" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        replicateM_ 20 (answer home ["receive"] [])
        run <- hw home (tabAfter ["label", "1KEn"]) []
        runOut run `shouldBe` "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    it "offers the id of a transaction of the history after label" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["label", take 4 paidTxid]) []
        runOut run `shouldBe` Text.pack paidTxid <> "\n"

    it "offers a pending transaction by the hash pendingtxs shows after label" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["label", take 4 txid]) []
        runOut run `shouldBe` Text.pack txid <> "\n"

    it "offers the id of a payment the portfolio sent after bumpfee" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        txid <- sentPayment home
        run <- hw home (tabAfter ["bumpfee", take 4 txid]) []
        runOut run `shouldBe` Text.pack txid <> "\n"

    it "does not offer a payment not sent yet after bumpfee" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["bumpfee", take 4 txid]) []
        runOut run `shouldBe` ""

    -- The sync finds the payment in the mempool and retires it from the
    -- pending transactions: it is in the history then
    it "offers a sent payment the sync found in the mempool after bumpfee" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        txid <- sentPayment home
        _ <- hw home ["sync"] []
        run <- hw home (tabAfter ["bumpfee", take 4 txid]) []
        runOut run `shouldBe` Text.pack txid <> "\n"

    it "does not offer a payment in a block after bumpfee, which nothing can replace" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        txid <- sentPayment home
        modifyIORef chain mine
        _ <- hw home ["sync"] []
        run <- hw home (tabAfter ["bumpfee", take 4 txid]) []
        runOut run `shouldBe` ""

    -- Only its sender can replace the payment received
    it "does not offer a payment received after bumpfee" $
      withChain ctx (Chain 800002 [(payment ctx, Nothing)]) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["bumpfee", take 4 paidTxid]) []
        runOut run `shouldBe` ""

    -- bash breaks --wallet=1 into --wallet, = and 1. wallet0 has
    -- portfolio0 and portfolio1, wallet1 portfolio0 alone.
    it "reads the wallet of --wallet=INDEX among the words typed, as bash breaks it" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        run <- hw home (tabAfter ["switch", "--wallet", "=", "1", "--portfolio", ""]) []
        runOut run `shouldBe` "0\n"
        other <- hw home (tabAfter ["switch", "--wallet", "=", "0", "--portfolio", ""]) []
        runOut other `shouldBe` "0\n1\n"

    -- The coin is of portfolio0, and portfolio1, which has no coins, is
    -- active: coin control refuses a coin of another portfolio
    it "offers only the coins of the active portfolio after --coin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        run <- hw home (tabAfter ["preparetx", "--coin", take 4 paidCoin]) []
        runOut run `shouldBe` ""

    it "offers the receiving addresses after label" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home (tabAfter ["label", "1KEn"]) []
        runOut run `shouldBe` "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    -- The payment of preparetx sends its change to the first change
    -- address, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn
    it "does not offer a change address after label" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home (tabAfter ["label", "17Ki"]) []
        runOut run `shouldBe` ""

    it "offers the receiving addresses of the wallet after show" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["show", "1KEn"]) []
        runOut run `shouldBe` "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    it "offers the change addresses too after show" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home (tabAfter ["show", "17Ki"]) []
        runOut run `shouldBe` "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn\n"

    -- show offers the txid beside its coin, so Tab stops at the txid, and
    -- the user types the colon. bash passes an escaped one as TXID\:, and
    -- the one coin of the wallet in the paid chain is at index 0.
    it "offers the coins of a transaction after show and the colon bash escaped" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        bashCompletion home ["show", txidArg (payment ctx) <> "\\:"] `shouldReturn` [Text.pack (txidArg (payment ctx) <> "\\:0")]

    -- The one coin of the wallet in the paid chain, at index 0
    it "offers the index of a coin after show, the short id of its txid and a colon, through the script of bash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        bashCompletion home ["show", take 8 (txidArg (payment ctx)), ":"] `shouldReturn` ["0"]

    -- The payment of the paid chain, by its txid, and its one coin of the
    -- wallet, TXID:0, which bash takes as it is: TXID\:0
    it "offers the transactions and the coins of the wallet after show, by their first digits" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["show", take 4 (txidArg (payment ctx))]) []
        runOut run `shouldBe` Text.pack (txidArg (payment ctx)) <> "\n" <> Text.pack (txidArg (payment ctx)) <> "\\:0\n"

    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is the first receiving address of
    -- main. The script of bash runs in the home.
    it "offers the files after show, with the addresses of the wallet" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        writeFile (home </> "1KEn-notes.psbt") ""
        (sort <$> bashCompletion home ["show", "1KEn"]) `shouldReturn` ["1KEn-notes.psbt", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]

    it "offers a directory after show with a /, so that the path goes on into it" $
      withHome $ \home -> do
        createDirectory (home </> "docs")
        bashCompletion home ["show", "do"] `shouldReturn` ["docs/"]

    -- show would take the bare name for a short id
    it "leaves out after show a file named like a short id, and offers it once ./ is typed" $
      withHome $ \home -> do
        newTestPortfolio home
        writeFile (home </> "a1075db5") ""
        bashCompletion home ["show", "a1"] `shouldReturn` []
        bashCompletion home ["show", "./a1"] `shouldReturn` ["./a1075db5"]

    it "offers the change addresses too after signmsg" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home (tabAfter ["signmsg", "17Ki"]) []
        runOut run `shouldBe` "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn\n"

    -- The paid chain pays the first receiving address
    it "offers the addresses that hold coins that can be spent now after --spend-from" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["preparetx", "--spend-from", "1KEn"]) []
        runOut run `shouldBe` "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    it "offers largest after --change-back" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["preparetx", "--change-back", "la"]) []
        runOut run `shouldBe` "largest\n"

    it "offers the addresses of the active portfolio after --change-back, and not largest when the word typed does not start it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["preparetx", "--change-back", "1KEn"]) []
        runOut run `shouldBe` "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    -- The second receiving address, 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg, is
    -- given out and paid nothing
    it "does not offer an address without coins that can be spent now after --spend-from" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["receive"] []
        run <- hw home (tabAfter ["preparetx", "--spend-from", "1AVj"]) []
        runOut run `shouldBe` ""

    -- hw stores it as bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u,
    -- and finds it without the prefix
    it "offers a Bitcoin Cash address without its prefix, as bash would split the word at the colon" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        run <- hw home (tabAfter ["label", "q"]) []
        runOut run `shouldBe` "qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u\n"

    -- bash breaks the word at the colon typed, keeps bitcoincash: and
    -- inserts what is offered after it
    it "completes a Bitcoin Cash address typed with its prefix, through the script of bash" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        bashCompletion home ["label", "bitcoincash", ":", "q"] `shouldReturn` ["qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"]

    -- zsh and fish replace the word whole, so the prefix stays
    it "offers a Bitcoin Cash address typed with its prefix with that prefix, for zsh and fish" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        run <- hw home ("--bash-completion-enriched" : tabAfter ["label", "bitcoincash:q"]) []
        runOut run `shouldBe` "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u\n"

    -- bash passes the word after a space as an empty word
    it "offers the indexes of the portfolios after a space, through the script of bash" $
      withHome $ \home -> do
        newTestPortfolio home
        bashCompletion home ["switch", "--portfolio", ""] `shouldReturn` ["0"]

    -- bash breaks --portfolio=0 into --portfolio, = and 0, and inserts the
    -- index in place of the 0
    it "offers the indexes of the portfolios after --portfolio=, which bash breaks at the =, through the script of bash" $
      withHome $ \home -> do
        newTestPortfolio home
        bashCompletion home ["switch", "--portfolio", "=", "0"] `shouldReturn` ["0"]
        bashCompletion home ["switch", "--portfolio", "="] `shouldReturn` ["0"]

    -- The script of optparse-applicative expanded a* to the files of the
    -- directory it matches, axb here
    it "passes a word with a star to hw as typed, and escapes the star of the file offered, through the script of bash" $
      withHome $ \home -> do
        writeFile (home </> "a*b") ""
        createDirectory (home </> "axb")
        bashCompletion home ["importtx", "a*"] `shouldReturn` ["a\\*b"]

    -- bash inserts a word as it is, and would take my savings.psbt for two
    it "escapes a space in the name of a file for bash" $
      withHome $ \home -> do
        writeFile (home </> "my savings.psbt") ""
        bashCompletion home ["importtx", "my"] `shouldReturn` ["my\\ savings.psbt"]

    -- zsh and fish complete the files themselves, and are offered the
    -- directories
    it "does not escape the name of a directory for zsh and fish, which quote a word themselves" $
      withHome $ \home -> do
        createDirectory (home </> "my docs")
        run <- hw home ("--bash-completion-enriched" : tabAfter ["importtx", "my"]) []
        runOut run `shouldBe` "my docs/\n"

    it "offers a directory with a / for a FILENAME, and a file without, through the script of bash" $
      withHome $ \home -> do
        createDirectory (home </> "docs")
        writeFile (home </> "doc.json") ""
        bashCompletion home ["importtx", "doc"] `shouldReturn` ["doc.json", "docs/"]

    it "offers a directory with a / to zsh and fish too" $
      withHome $ \home -> do
        createDirectory (home </> "docs")
        run <- hw home ("--bash-completion-enriched" : tabAfter ["importtx", "doc"]) []
        runOut run `shouldBe` "docs/\n"

    it "offers the files after --output of signmsg, through the script of bash" $
      withHome $ \home -> do
        createDirectory (home </> "docs")
        writeFile (home </> "doc.json") ""
        bashCompletion home ["signmsg", "--output", "doc"] `shouldReturn` ["doc.json", "docs/"]

    -- zsh and fish keep --output=doc one word
    it "offers the files after --output= of signmsg in the same word, --output= before each" $
      withHome $ \home -> do
        createDirectory (home </> "docs")
        writeFile (home </> "doc.json") ""
        run <- hw home ("--bash-completion-enriched" : tabAfter ["signmsg", "--output=doc"]) []
        runOut run `shouldBe` "--output=doc.json\n--output=docs/\n"

    it "offers the addresses after the --output file of signmsg" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home (tabAfter ["signmsg", "--output", "signed.txt", "1KEn"]) []
        runOut run `shouldBe` "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    -- The script of optparse-applicative added a / to any value that
    -- names a directory, a portfolio 0 beside a directory named 0 for
    -- instance
    it "writes a script for fish that offers the lines hw prints, by the name given, as they are" $
      withHome $ \home -> do
        run <- hw home ["--fish-completion-script", "/opt/bin/hw"] []
        runOut run
          `shouldBe` Text.unlines
            [ "function _hw",
              "    set -l words (commandline --tokenize --current-process)",
              "    # The words before the cursor, counted: the index of the one to complete",
              "    set -l index (count (commandline --tokenize --cut-at-cursor --current-process))",
              "    set -l request --bash-completion-enriched --bash-completion-index $index",
              "    for word in $words",
              "        set -a request --bash-completion-word $word",
              "    end",
              "    /opt/bin/hw $request",
              "end",
              "",
              "complete --no-files --command hw --arguments '(_hw)'"
            ]

    -- .hw is the directory of hw in the home. The options of importtx are
    -- offered with the files for an empty word, as for any first argument.
    it "offers a hidden file for a FILENAME only to a word that starts with a dot" $
      withHome $ \home -> do
        createDirectory (home </> "docs")
        run <- hw home (tabAfter ["importtx", ""]) []
        Text.lines (runOut run) `shouldSatisfy` (\offered -> "docs/" `elem` offered && ".hw/" `notElem` offered)
        dotted <- hw home (tabAfter ["importtx", "."]) []
        runOut dotted `shouldBe` ".hw/\n"

    -- A command holds the database from its first statement to its last,
    -- as a sync does while it waits on the store
    it "offers the indexes of the portfolios while a command holds the database" $
      withHome $ \home -> do
        newTestPortfolio home
        held <- newEmptyMVar
        release <- newEmptyMVar
        holder <- async $ runDBWith 100 (home </> ".hw" </> "accounts.sqlite") $ do
          liftIO $ putMVar held ()
          liftIO $ takeMVar release
        takeMVar held
        run <- hw home (tabAfter ["switch", "--portfolio", ""]) []
        putMVar release ()
        wait holder
        runOut run `shouldBe` "0\n"

    it "leaves the files of hw as they were" $
      withHome $ \home -> do
        newTestPortfolio home
        let database = home </> ".hw" </> "accounts.sqlite"
        stored <- BS.readFile database
        _ <- hw home (tabAfter ["switch", "--portfolio", ""]) []
        BS.readFile database `shouldReturn` stored
        sort <$> listDirectory (home </> ".hw") `shouldReturn` ["accounts.sqlite", "config.json"]

    it "offers nothing, and creates nothing, in a home where hw never ran" $
      withHome $ \home -> do
        removeDirectoryRecursive (home </> ".hw")
        run <- hw home (tabAfter ["switch", "--portfolio", ""]) []
        runOut run `shouldBe` ""
        doesDirectoryExist (home </> ".hw") `shouldReturn` False

    -- A database of hw 0.9, which every command refuses
    it "offers nothing from a database of an older version, which it leaves at that version" $
      withHome $ \home -> do
        inDatabaseFile (home </> ".hw" </> "accounts.sqlite") version2Database
        run <- hw home (tabAfter ["switch", "--portfolio", ""]) []
        runOut run `shouldBe` ""
        runDBWith 100 (home </> ".hw" </> "accounts.sqlite") (runExceptT databaseVersion) `shouldReturn` Right (Just 2)

  describe "A command that fails" $ do
    it "exits with 1, and tells why" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["switch", "--portfolio", "7"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "wallet0 has no portfolio7: portfolios lists the portfolios"

    -- A script may read the exit code or the JSON: both tell the failure.
    -- hw exited with 0 on an error in JSON before.
    it "exits with 1 with --json too, the error in its JSON" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["switch", "--portfolio", "7", "--json"] []
        runExit run `shouldBe` ExitFailure 1
        (errorOf <$> hwJson home ["switch", "--portfolio", "7"] []) `shouldReturn` Right "wallet0 has no portfolio7: portfolios lists the portfolios"

    it "refuses an amount, with its reason" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "1.5sat", "--fee-rate", "10"] [])
          `shouldReturn` Right "The amount 1.5sat has decimals, but an amount in sats is whole"

    it "is refused an option it does not know, before the database is touched" $
      withHome $ \home -> do
        run <- hw home ["portfolios", "--unit", "eur"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --unit: cannot parse value `eur'"
        doesFileExist (home </> ".hw" </> "accounts.sqlite") `shouldReturn` False

    -- An output has an index of 32 bits. The index 2^32 wrapped around to
    -- 0, and hw spent the first output of the transaction instead.
    it "is refused a --coin index no output has, rather than spending another coin" $
      withHome $ \home -> do
        let coin = Text.unpack (txHashToHex (fakeTxHash 1)) <> ":4294967296"
        forM_ [["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"], ["preparesweep", "--sweep-to", textOf (otherAddr 0), "--fee-rate", "10"]] $ \command -> do
          run <- hw home (command <> ["--coin", coin]) []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf ("option --coin: The coin " <> Text.pack coin <> " is not valid: write it TXID:INDEX, as coins lists it")

    -- The hash and the index were one argument with a space, which needed
    -- quotes; a block explorer writes TXID:INDEX
    it "is refused a --coin written as two words" $
      withHome $ \home -> do
        let hash = Text.unpack (txHashToHex (fakeTxHash 1))
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", hash, "0", "--fee-rate", "10"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf ("option --coin: The coin " <> Text.pack hash <> " is not valid: write it TXID:INDEX, as coins lists it")

    it "is cancelled when the input ends before the answer, and leaves nothing" $
      withHome $ \home -> do
        (errorOf <$> hwJson home ["newportfolio", "--existing"] [mnemonicWords testMnemonic]) `shouldReturn` Right "Cancelled: the input ended before the answer"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    -- A rate of 2^64 wrapped around to zero: preparetx made a transaction
    -- without fee, which signtx refused
    it "refuses a fee rate past 32 bits, rather than wrapping around" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "18446744073709551616"] [])
          `shouldReturn` Right "The fee rate must be at most 4294967295 satoshis per virtual byte"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- preparetx takes a change address before it chooses the coins, which
    -- do not cover the 100000 satoshis here. The failure undoes it, with
    -- everything else the command wrote.
    it "leaves nothing of what it wrote before it failed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.001", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"
        (addressesOf <$> hwJson home ["addresses", "--change"] []) `shouldReturn` Right []
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- The store of the test home refuses every connection. hw crashed with
    -- the exception of the web client before, and wrote no JSON. No
    -- network can be synced, so the sync fails with the failure of the
    -- store.
    it "tells when the store is out of reach, in its JSON too" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["sync"] [])
          `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed"

    -- The answer is not the JSON hw asked for. hw crashed on such an
    -- answer before, with the error of the reader.
    it "tells when the store gives an answer it cannot read, as a server that is not a haskoin-store does" $
      withServerAnswering "not a store" $ \home url -> do
        newTestPortfolio home
        failed <- errorOf <$> hwJson home ["sync"] []
        failed `shouldSatisfy` either (const False) (Text.isPrefixOf ("The store at " <> Text.pack url <> " gave an answer hw could not read: "))

    -- 64 MiB is 67108864 bytes. The web client read a body in full: a
    -- store sending one without end filled the memory in seconds.
    it "refuses an answer of the store one byte over 64 MiB, before it decodes it" $
      withServerSending (BL.replicate 67108865 0) $ \home url ->
        (errorOf <$> hwJson home ["fees"] [])
          `shouldReturn` Right (Text.pack ("The store at " <> url <> " sent an answer of more than 64 MiB, which hw does not read"))

    it "reads an answer of the store of 64 MiB, whatever it then makes of it" $
      withServerSending (BL.replicate 67108864 0) $ \home url ->
        (errorOf <$> hwJson home ["fees"] [])
          `shouldNotReturn` Right (Text.pack ("The store at " <> url <> " sent an answer of more than 64 MiB, which hw does not read"))

    -- hw crashed with the exception of the system before, and wrote no JSON
    it "tells when a file it reads does not exist, naming it" $
      withHome $ \home -> do
        newTestPortfolio home
        forM_ ["show", "importtx", "importwallet"] $ \command ->
          (errorOf <$> hwJson home [command, home </> "missing.json"] [])
            `shouldReturn` Right (noSuchFile (home </> "missing.json"))

    -- The JSON file of an earlier hw is such a file: this version reads
    -- it no more. show reads files of wallets too, and names them as well
    -- ("Haskoin.Wallet.Workflow.ShowSpec").
    it "tells when a file it reads is no transaction file, naming it and the forms a transaction file takes" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "earlier.json"
        writeFile file "{\"tx\": \"0100000000000000000000\", \"txinputs\": []}\n"
        (errorOf <$> hwJson home ["importtx", file] [])
          `shouldReturn` Right ("The file " <> Text.pack file <> " could not be read: Not a PSBT, in bytes or in base64 text, nor the hexadecimal of a transaction")

    it "tells when a file it writes has no directory to go in, naming it" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "missing" </> "wallet.jsonl"
        (errorOf <$> hwJson home ["exportwallet", file] [])
          `shouldReturn` Right ("The file " <> Text.pack file <> " could not be written: no such file or directory")

    -- preparetx writes its file last, once the transaction is pending and
    -- its change address taken: the failure undoes both
    it "keeps nothing of a transaction whose file could not be written" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let file = home </> "missing" </> "unsigned.psbt"
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file, "--fee-rate", "10"] [])
          `shouldReturn` Right ("The file " <> Text.pack file <> " could not be written: no such file or directory")
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
        (addressesOf <$> hwJson home ["addresses", "--change"] []) `shouldReturn` Right []

    -- The other command holds the database the whole time: hw waits its
    -- three seconds, then gives up
    it "waits for another command that holds the database, and says so when it does not finish" $
      withHome $ \home -> do
        held <- newEmptyMVar
        release <- newEmptyMVar
        holder <- async $ runDBWith 100 (home </> ".hw" </> "accounts.sqlite") $ do
          liftIO $ putMVar held ()
          liftIO $ takeMVar release
        takeMVar held
        run <- hw home ["portfolios"] []
        putMVar release ()
        wait holder
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed (Text.pack databaseInUse)

-- | The groups of the commands in the help of the program, in their order,
-- as the README has them
helpGroups :: [Text]
helpGroups = ["Wallets and portfolios", "Addresses", "Coins", "Payments", "Transactions", "Sweeps", "Messages", "The wallet"]

-- | Check that each command refuses each option as one it does not have,
-- before it reads anything: hw exits with 1 and names the option. The
-- option comes right after the command, so that nothing else the command
-- misses is told first. The name of the command and of the option go with
-- the check, so that a failure tells which they were.
refusesOptions :: FilePath -> [String] -> [String] -> IO ()
refusesOptions home options commands =
  forM_ commands $ \command -> forM_ options $ \option -> do
    run <- hw home [command, option, "1"] []
    (command, option, runExit run) `shouldBe` (command, option, ExitFailure 1)
    (command, option, Text.isInfixOf ("Invalid option `" <> Text.pack option <> "'") (runErr run)) `shouldBe` (command, option, True)
