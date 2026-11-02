{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The command line of hw as the user meets it: the help, the version,
-- a command line hw cannot parse, the completion the shell asks for, and
-- how a command that fails says so and leaves nothing behind.
module Haskoin.Wallet.Workflow.CommandLineSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef)
import Data.List (sort)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Database (databaseInUse, runDBWith)
import Haskoin.Wallet.Database.Account (renameAccount)
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (MnemonicPass (..))
import Haskoin.Wallet.Migration (databaseVersion)
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

    -- The text of preparetx names them in this order
    it "of a command ends with the commands its text names" $
      withHome $ \home -> do
        run <- hw home ["preparetx", "--help"] []
        runOut run `shouldSatisfy` Text.isSuffixOf "See also: signtx, sendtx, pendingtxs, deletetx, bumpfee\n"

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
        runOut run `shouldSatisfy` Text.isInfixOf "! Generate a new mnemonic"
        runOut run `shouldSatisfy` (not . Text.isInfixOf "! Send a signed transaction")

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
        runErr run `shouldSatisfy` Text.isInfixOf "Mnemonics and accounts"

    -- label runs with --help as the label, and finds no address in an
    -- empty wallet
    it "is not shown for a --help after --, which is an argument" $
      withHome $ \home -> do
        run <- hw home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--", "--help"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "Address does not exist"

  describe "The version" $ do
    it "of hw and of its database format is shown with --version, and hw exits with success" $
      withHome $ \home -> do
        run <- hw home ["--version"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldBe` "Software version: 0.10.0\nDatabase version: 4\n"

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
    it "names the unknown command, and suggests the closest one" $
      withHome $ \home -> do
        run <- hw home ["preparetxx"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid argument `preparetxx'"
        runErr run `shouldSatisfy` Text.isInfixOf "Did you mean this?\n    preparetx"

    it "says what is wrong with the value of an option, then the usage of the command" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--parts", "20"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Error: option --parts: The --parts value has to be between 1 and 12"
        runErr run `shouldSatisfy` Text.isInfixOf "Usage: hw mnemonic"

    it "ends with the help to read" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--parts", "20"] []
        runErr run `shouldSatisfy` Text.isSuffixOf "Type hw mnemonic --help for the help of mnemonic.\n"

    it "says what a command misses" $
      withHome $ \home -> do
        run <- hw home ["exporttx", Text.unpack (txHashToHex (fakeTxHash 1))] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Missing: FILENAME"
        runErr run `shouldSatisfy` Text.isInfixOf "Usage: hw exporttx HASH FILENAME"

    it "says what a command misses, even when an option of every command follows" $
      withHome $ \home -> do
        run <- hw home ["renameacc", "old", "--json"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Missing: NEW"

  describe "The completion the shell asks for" $ do
    it "offers an option of every command after the command" $
      withHome $ \home -> do
        run <- hw home (tabAfter ["txs", "-atest3", "--js"]) []
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

    it "offers the names of the accounts that start with the word typed after --account" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        run <- hw home (tabAfter ["txs", "--account", "s"]) []
        runOut run `shouldBe` "second\n"

    it "offers the names of the accounts for an argument that names one" $
      withHome $ \home -> do
        createMain home
        run <- hw home (tabAfter ["renameacc", "m"]) []
        runOut run `shouldBe` "main\n"

    -- Each name replaces the word typed, the option with the start of the
    -- name
    it "offers the names of the accounts after -a in the same word, -a before each" $
      withHome $ \home -> do
        createMain home
        run <- hw home (tabAfter ["txs", "-am"]) []
        runOut run `shouldBe` "-amain\n"

    -- zsh and fish keep --account=m one word, which the name offered
    -- replaces whole
    it "offers the names of the accounts after --account= in the same word, --account= before each" $
      withHome $ \home -> do
        createMain home
        run <- hw home ("--bash-completion-enriched" : tabAfter ["txs", "--account=m"]) []
        runOut run `shouldBe` "--account=main\n"

    it "offers every name after --account= alone" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        run <- hw home ("--bash-completion-enriched" : tabAfter ["txs", "--account="]) []
        runOut run `shouldBe` "--account=main\n--account=second\n"

    -- txs has --verbose, which takes no value
    it "offers nothing after the = of an option that takes no value" $
      withHome $ \home -> do
        createMain home
        run <- hw home ("--bash-completion-enriched" : tabAfter ["txs", "--verbose=m"]) []
        runOut run `shouldBe` ""

    -- bash completes my savings up to its space, which it escapes
    it "offers a name with a space after the space bash escaped, escaped for bash again" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "my savings"] seedTwice
        run <- hw home (tabAfter ["txs", "--account", "my\\ s"]) []
        runOut run `shouldBe` "my\\ savings\n"

    -- hw runs without a locale here. The name is passed to the database
    -- rather than typed, as the arguments of the tests may not take an
    -- accent.
    it "offers a name with accents as it is" $
      withHome $ \home -> do
        createMain home
        _ <- inDatabaseFile (home </> ".hw" </> "accounts.sqlite") (runExceptT (renameAccount "main" "épargne"))
        run <- hw home (tabAfter ["txs", "--account", ""]) []
        runOut run `shouldBe` "épargne\n"

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

    it "offers the id of a signed transaction after sendtx" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["sendtx", take 4 txid]) []
        runOut run `shouldBe` Text.pack txid <> "\n"

    it "does not offer a transaction not signed yet after sendtx" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home (tabAfter ["sendtx", take 4 nosig]) []
        runOut run `shouldBe` ""

    -- The sync sends it again should the network lose it
    it "does not offer a transaction sent already after sendtx" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        txid <- sentPayment home
        run <- hw home (tabAfter ["sendtx", take 4 txid]) []
        runOut run `shouldBe` ""

    -- Another transaction of the mempool spends the coin of the payment,
    -- which the sync finds
    it "does not offer a transaction in conflict after sendtx, which the network would refuse" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- paymentSigned home
        let other = spendingTx ctx [(txHash (payment ctx), 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        _ <- hw home ["sync"] []
        txid <- txidOf =<< onePendingTx home
        run <- hw home (tabAfter ["sendtx", take 4 txid]) []
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

    it "reads an account name bash escaped among the words typed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["renameacc", "main", "my savings"] []
        run <- hw home (tabAfter ["preparetx", "--account", "my\\ savings", "--coin", take 4 paidCoin]) []
        runOut run `shouldBe` offeredCoin

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

    it "offers the id of a payment the account sent after bumpfee" $
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

    -- bash breaks --account=main into --account, = and main
    it "reads the account of --account=NAME among the words typed, as bash breaks it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home (tabAfter ["preparetx", "--account", "=", "main", "--coin", take 4 paidCoin]) []
        runOut run `shouldBe` offeredCoin

    -- The account second has no coins
    it "offers only the coins of the account named with --account after --coin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        run <- hw home (tabAfter ["preparetx", "--account", "second", "--coin", take 4 paidCoin]) []
        runOut run `shouldBe` ""

    it "offers the receiving addresses after label" $
      withHome $ \home -> do
        createMain home
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
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        _ <- answer home ["receive"] []
        run <- hw home (tabAfter ["label", "q"]) []
        runOut run `shouldBe` "qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u\n"

    -- bash passes the word after a space as an empty word
    it "offers the account names after a space, through the script of bash" $
      withHome $ \home -> do
        createMain home
        bashCompletion home ["txs", "--account", ""] `shouldReturn` ["main"]

    -- bash breaks --account=m into --account, = and m, and inserts the
    -- name in place of the m
    it "offers the account names after --account=, which bash breaks at the =, through the script of bash" $
      withHome $ \home -> do
        createMain home
        bashCompletion home ["txs", "--account", "=", "m"] `shouldReturn` ["main"]
        bashCompletion home ["txs", "--account", "="] `shouldReturn` ["main"]

    -- The script of optparse-applicative expanded a* to the files of the
    -- directory it matches, axb here
    it "passes a word with a star to hw as typed, and escapes the star of the name offered, through the script of bash" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "a*b"] seedTwice
        createDirectory (home </> "axb")
        bashCompletion home ["txs", "--account", "a*"] `shouldReturn` ["a\\*b"]

    -- bash inserts a word as it is, and would take my savings for two
    it "escapes a space in a name for bash" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "my savings"] seedTwice
        bashCompletion home ["txs", "--account", "my"] `shouldReturn` ["my\\ savings"]

    it "does not escape a name for zsh and fish, which quote a word themselves" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "my savings"] seedTwice
        run <- hw home ("--bash-completion-enriched" : tabAfter ["txs", "--account", "my"]) []
        runOut run `shouldBe` "my savings\n"

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
        createMain home
        _ <- answer home ["receive"] []
        run <- hw home (tabAfter ["signmsg", "--output", "signed.txt", "1KEn"]) []
        runOut run `shouldBe` "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    -- The script of optparse-applicative added a / to any value that
    -- names a directory, an account named src for instance
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
    it "offers the names of the accounts while a command holds the database" $
      withHome $ \home -> do
        createMain home
        held <- newEmptyMVar
        release <- newEmptyMVar
        holder <- async $ runDBWith 100 (home </> ".hw" </> "accounts.sqlite") $ do
          liftIO $ putMVar held ()
          liftIO $ takeMVar release
        takeMVar held
        run <- hw home (tabAfter ["txs", "--account", ""]) []
        putMVar release ()
        wait holder
        runOut run `shouldBe` "main\n"

    it "leaves the files of hw as they were" $
      withHome $ \home -> do
        createMain home
        let database = home </> ".hw" </> "accounts.sqlite"
        stored <- BS.readFile database
        _ <- hw home (tabAfter ["txs", "--account", ""]) []
        BS.readFile database `shouldReturn` stored
        sort <$> listDirectory (home </> ".hw") `shouldReturn` ["accounts.sqlite", "config.json"]

    it "offers nothing, and creates nothing, in a home where hw never ran" $
      withHome $ \home -> do
        removeDirectoryRecursive (home </> ".hw")
        run <- hw home (tabAfter ["txs", "--account", ""]) []
        runOut run `shouldBe` ""
        doesDirectoryExist (home </> ".hw") `shouldReturn` False

    -- The next command migrates it
    it "offers nothing from a database of an older version, which it leaves at that version" $
      withHome $ \home -> do
        version3Main (home </> ".hw" </> "accounts.sqlite")
        run <- hw home (tabAfter ["txs", "--account", ""]) []
        runOut run `shouldBe` ""
        runDBWith 100 (home </> ".hw" </> "accounts.sqlite") (runExceptT databaseVersion) `shouldReturn` Right (Just 3)

  describe "A command that fails" $ do
    it "exits with 1, and tells why" $
      withHome $ \home -> do
        run <- hw home ["receive", "--account", "nope"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The account nope does not exist"

    -- A script may read the exit code or the JSON: both tell the failure.
    -- hw exited with 0 on an error in JSON before.
    it "exits with 1 with --json too, the error in its JSON" $
      withHome $ \home -> do
        run <- hw home ["receive", "--account", "nope", "--json"] []
        runExit run `shouldBe` ExitFailure 1
        (errorOf <$> hwJson home ["receive", "--account", "nope"] []) `shouldReturn` Right "The account nope does not exist"

    it "refuses an amount, with its reason" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "1.5sat", "--fee-rate", "10"] [])
          `shouldReturn` Right "The amount 1.5sat has decimals, but an amount in sats is whole"

    it "is refused an option it does not know, before the database is touched" $
      withHome $ \home -> do
        run <- hw home ["accounts", "--unit", "eur"] []
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
        (errorOf <$> hwJson home ["createacc", "main"] [mnemonicWords testMnemonic]) `shouldReturn` Right "Cancelled: the input ended before the answer"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

    -- A rate of 2^64 wrapped around to zero: preparetx made a transaction
    -- without fee, which signtx refused
    it "refuses a fee rate past 32 bits, rather than wrapping around" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "18446744073709551616"] [])
          `shouldReturn` Right "The fee rate must be at most 4294967295 satoshis per byte"
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
    -- the exception of the web client before, and wrote no JSON. The sync
    -- leaves the account out, with the failure of its store.
    it "tells when the store is out of reach, in its JSON too" $
      withHome $ \home -> do
        createMain home
        (notSyncedOf <$> hwJson home ["sync"] [])
          `shouldReturn` Right [("main", "The store at http://127.0.0.1:1/ could not be reached: the connection failed")]

    -- The web client decodes an answer only once hw uses it: hw crashed
    -- there before, with the error of the decoder
    it "tells when the store gives an answer it cannot read, as a server that is not a haskoin-store does" $
      withServerAnswering "not a store" $ \home url -> do
        createMain home
        notSynced <- notSyncedOf <$> hwJson home ["sync"] []
        (map fst <$> notSynced) `shouldBe` Right ["main"]
        (map snd <$> notSynced) `shouldSatisfy` either (const False) (all (Text.isPrefixOf ("The store at " <> Text.pack url <> " gave an answer hw could not read: ")))

    -- hw crashed with the exception of the system before, and wrote no JSON
    it "tells when a file it reads does not exist, naming it" $
      withHome $ \home -> do
        createMain home
        forM_ ["reviewtx", "importtx", "importacc"] $ \command ->
          (errorOf <$> hwJson home [command, home </> "missing.json"] [])
            `shouldReturn` Right (noSuchFile (home </> "missing.json"))

    it "tells when a file it writes has no directory to go in, naming it" $
      withHome $ \home -> do
        createMain home
        let file = home </> "missing" </> "main.json"
        (errorOf <$> hwJson home ["exportacc", file] [])
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
        run <- hw home ["accounts"] []
        putMVar release ()
        wait holder
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed (Text.pack databaseInUse)
