{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The command line of hw, before anything runs: the usage lines its help
-- shows, what label takes, the commands its texts name, and what the
-- arguments ask for
module Haskoin.Wallet.Cli.ParserSpec (spec) where

import Haskoin (txHashToHex)
import Haskoin.Wallet.Cli.Parser (Labelled (..), program, readCoinRef, readLabelled)
import Haskoin.Wallet.Cli.Parser.Args (Args (..), CommandSpec (..))
import Haskoin.Wallet.Cli.Parser.Help (mentionedCommands, usageParts)
import Haskoin.Wallet.Cli.Parser.Completion (completionParts)
import Haskoin.Wallet.Cli.Parser.Run (Request (..), commandNamed, requestOf)
import Haskoin.Wallet.Cli.Style (renderPlain)
import Haskoin.Wallet.ShortId (HashRef (..), coinRefText, shortIdText)
import Test.Hspec

spec :: Spec
spec = do
  describe "The usage line of a command" $ do
    it "puts an option that may be left out in brackets, its names separated by a bar" $
      usageOf "receive" `shouldBe` "[-a|--account NAME] [-l|--label LABEL]"

    -- The recipients and the coins may be repeated, and the strategy has a
    -- default
    it "lists the options of preparetx as they are typed" $
      usageOf "preparetx"
        `shouldBe` "ADDRESS:AMOUNT... [-a|--account NAME] [-f|--fee-rate RATE] [-r|--recipients-pay] [-m|--min-conf INT] [--strategy STRATEGY] [-o|--output FILENAME] [-l|--label LABEL] [-c|--coin TXID:INDEX]... [--spend-from ADDRESS]... [--select] [-y|--yes] [--change-back | --new-change]"

    it "follows what may be given several times with ..., after the brackets of an optional one" $
      usageOf "preparesendall"
        `shouldBe` "ADDRESS[:AMOUNT]... [-a|--account NAME] [-f|--fee-rate RATE] [-m|--min-conf INT] [-o|--output FILENAME] [-l|--label LABEL] [-c|--coin TXID:INDEX]... [--spend-from ADDRESS]... [--spend-dust | --leave-dust] [-y|--yes]"

    it "offers txs --compact, --verbose, --tabular and --csv as a choice, since a table has no room for every detail and a file is not the screen" $
      usageOf "txs" `shouldBe` "[-a|--account NAME] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after TXID] [--max INT]"

    it "offers addresses --compact, --verbose, --tabular and --csv as a choice too" $
      usageOf "addresses" `shouldBe` "[-a|--account NAME] [--change] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after ADDRESS] [--max INT]"

    it "offers accounts --compact, --verbose, --tabular and --csv as a choice" $
      usageOf "accounts" `shouldBe` "[-a|--account NAME] [--compact | (-v|--verbose) | --tabular | --csv FILENAME]"

    it "offers coins --compact, --verbose, --tabular and --csv as a choice" $
      usageOf "coins" `shouldBe` "[-a|--account NAME] [--locked] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after TXID:INDEX] [--max INT]"

    it "offers pendingtxs --compact, --verbose, --tabular and --csv as a choice" $
      usageOf "pendingtxs" `shouldBe` "[-a|--account NAME] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after HASH] [--max INT]"

    it "offers balances --compact, --verbose, --tabular and --csv as a choice" $
      usageOf "balances" `shouldBe` "[-a|--account NAME] [--compact | (-v|--verbose) | --tabular | --csv FILENAME] [--after ADDRESS] [--max INT]"

    it "gives showaddr the address to show and no option" $
      usageOf "showaddr" `shouldBe` "ADDRESS"

    it "puts the argument of checkaddr before its option" $
      usageOf "checkaddr" `shouldBe` "PATH [-a|--account NAME]"

    it "puts a choice in parentheses, and a required option with two names too" $
      usageOf "verifymsg" `shouldBe` "(ADDRESS SIGNATURE MESSAGE | (-i|--input FILENAME)) [-n|--network NETWORK]"

    it "shows an option that has to be given at least once as required" $
      usageOf "preparesweep"
        `shouldBe` "[-a|--account NAME] [-n|--network NETWORK] [-k|--key-file FILENAME] [-w|--sweep-from ADDRESS]... [--address-file FILENAME] (-t|--sweep-to ADDRESS[:AMOUNT])... [-c|--coin TXID:INDEX]... [-o|--output FILENAME] [-l|--label LABEL] [-f|--fee-rate RATE] [-y|--yes]"

  describe "What label labels" $ do
    it "is a transaction for a word of 64 hexadecimal digits" $
      case readLabelled "ab00000000000000000000000000000000000000000000000000000000000001" of
        LabelledTx (FullHash tid) -> txHashToHex tid `shouldBe` "ab00000000000000000000000000000000000000000000000000000000000001"
        other -> expectationFailure $ "Not a transaction: " <> show other

    it "is a transaction by its short id, for a word of 8 hexadecimal digits" $
      case readLabelled "a1075db5" of
        LabelledTx (ShortHash short) -> shortIdText short `shouldBe` "a1075db5"
        other -> expectationFailure $ "Not a short id: " <> show other

    -- No address has 9 characters, so label refuses it as an address
    it "is an address for a word of 9 hexadecimal digits, which is neither a short id nor a txid" $
      readLabelled "a1075db5c" `shouldBe` LabelledAddress "a1075db5c"

    it "is an address, as typed, for any other word" $ do
      readLabelled "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `shouldBe` LabelledAddress "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
      readLabelled "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u" `shouldBe` LabelledAddress "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"

  describe "A coin as the user types it" $ do
    it "is TXID:INDEX with the txid in full" $
      (coinRefText <$> readCoinRef "a1075db5c0ffee00000000000000000000000000000000000000000000000001:1")
        `shouldBe` Just "a1075db5c0ffee00000000000000000000000000000000000000000000000001:1"

    it "is the short id of its txid, a colon and its index" $
      (coinRefText <$> readCoinRef "a1075db5:1") `shouldBe` Just "a1075db5:1"

    it "is nothing without its index" $
      readCoinRef "a1075db5" `shouldBe` Nothing

  describe "The commands a text of the help names" $ do
    it "are those named in it, in its order" $
      mentionedCommands program "Sign it with signtx, then send it with sendtx." `shouldBe` ["signtx", "sendtx"]

    it "are each named once, however often the text names them" $
      mentionedCommands program "signtx, then sendtx, then signtx again" `shouldBe` ["signtx", "sendtx"]

    it "include a command named by an everyday word only when it is in backticks" $
      mentionedCommands program "The coins of the last sync, as `coins` lists them." `shouldBe` ["coins"]

  describe "What the arguments ask for" $ do
    it "is the help of the command named, with --help anywhere before --" $
      requestOf unitOptions ["--unit", "sat", "sync", "--json", "--help"] `shouldBe` HelpOf (Just "sync")

    it "is the help of the program, with --help and no command" $
      requestOf unitOptions ["-h"] `shouldBe` HelpOf Nothing

    it "takes the word after --unit for a unit, not for a command" $
      requestOf unitOptions ["--unit", "sync", "--help"] `shouldBe` HelpOf Nothing

    it "leaves a --help after -- to the command, as an argument" $
      requestOf unitOptions ["signmsg", "a", "--", "--help"] `shouldBe` Parse

    it "leaves the words of a shell completion to the completion, --help among them" $
      requestOf unitOptions ["--bash-completion-index", "2", "--bash-completion-word", "hw", "--bash-completion-word", "--help"] `shouldBe` Parse

    it "is the help of the program when nothing is typed" $
      requestOf unitOptions [] `shouldBe` NothingTyped

    it "is the version of the program, with --version and no command" $
      requestOf unitOptions ["--version"] `shouldBe` Version

    -- No command takes --version: the parser refuses it
    it "leaves a --version after a command to the parser" $
      requestOf unitOptions ["sync", "--version"] `shouldBe` Parse

    it "is the help, with both --help and --version" $
      requestOf unitOptions ["--version", "--help"] `shouldBe` HelpOf Nothing

  describe "The words of a shell completion" $ do
    it "are read without the backslashes the shell typed to escape a space or a colon" $
      completionParts ["--bash-completion-index", "2", "--bash-completion-word", "hw", "--bash-completion-word", "my\\ savings", "--bash-completion-word", "0a1a\\:"]
        `shouldBe` (Just 2, ["hw", "my savings", "0a1a:"], [])

    it "are read the same from zsh and fish, which ask for descriptions" $
      completionParts ["--bash-completion-enriched", "--bash-completion-index", "1", "--bash-completion-word", "hw", "--bash-completion-word", "my\\ savings"]
        `shouldBe` (Just 1, ["hw", "my savings"], ["--bash-completion-enriched"])

    -- bash breaks --account=main at the =, into three words
    it "are joined where bash broke a word at the = of --option=value, the index moved along" $
      completionParts (wordsTyped 6 ["hw", "preparetx", "--account", "=", "main", "--coin", ""])
        `shouldBe` (Just 5, ["hw", "preparetx", "--account", "main", "--coin", ""], [])

    it "complete the value after the = bash broke a word at" $
      completionParts (wordsTyped 4 ["hw", "txs", "--account", "=", "m"])
        `shouldBe` (Just 3, ["hw", "txs", "--account", "m"], [])

    it "complete an empty value for an = under the cursor, after which bash inserts the value" $
      completionParts (wordsTyped 3 ["hw", "txs", "--account", "="])
        `shouldBe` (Just 3, ["hw", "txs", "--account", ""], [])

    it "ask for the script of bash with the name to run hw by" $
      requestOf unitOptions ["--bash-completion-script", "/usr/local/bin/hw"] `shouldBe` BashScript "/usr/local/bin/hw"

    it "ask for the script of fish with the name to run hw by" $
      requestOf unitOptions ["--fish-completion-script", "/usr/local/bin/hw"] `shouldBe` FishScript "/usr/local/bin/hw"

    -- zsh's script is the one of optparse-applicative
    it "leave the script of zsh to the parser" $
      requestOf unitOptions ["--zsh-completion-script", "hw"] `shouldBe` Parse
  where
    usageOf name = case commandNamed program name of
      Just command -> unwords (map renderPlain (usageParts command.args.usage))
      Nothing -> error $ "hw has no command " <> name
    -- The options of hw that take a value
    unitOptions = ["-u", "--unit"]
    -- The arguments the script of bash passes for the words, with the
    -- index of the one to complete
    wordsTyped :: Int -> [String] -> [String]
    wordsTyped index typed = ["--bash-completion-index", show index] <> concatMap (\w -> ["--bash-completion-word", w]) typed
