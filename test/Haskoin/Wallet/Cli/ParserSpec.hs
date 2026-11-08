{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The command line of hw, before anything runs: the usage lines its help
-- shows, what label takes, the commands its texts name, and what the
-- arguments ask for
module Haskoin.Wallet.Cli.ParserSpec (spec) where

import Haskoin (txHashToHex)
import Haskoin.Wallet.Cli.Parser (Labelled (..), program, readLabelled)
import Haskoin.Wallet.Cli.Parser.Args (Args (..), CommandSpec (..))
import Haskoin.Wallet.Cli.Parser.Help (mentionedCommands, usageParts)
import Haskoin.Wallet.Cli.Parser.Completion (completionParts)
import Haskoin.Wallet.Cli.Parser.Run (Request (..), commandNamed, requestOf)
import Haskoin.Wallet.Cli.Style (renderPlain)
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

    it "offers txs --verbose and --tabular as a choice, since a table has no room for every detail" $
      usageOf "txs" `shouldBe` "[-a|--account NAME] [(-v|--verbose) | --tabular] [--after TXID] [--max INT]"

    it "offers addresses --verbose and --tabular as a choice too" $
      usageOf "addresses" `shouldBe` "[-a|--account NAME] [--change] [(-v|--verbose) | --tabular] [--after ADDRESS] [--max INT]"

    it "offers accounts --verbose and --tabular as a choice" $
      usageOf "accounts" `shouldBe` "[-a|--account NAME] [(-v|--verbose) | --tabular]"

    it "offers coins --verbose and --tabular as a choice" $
      usageOf "coins" `shouldBe` "[-a|--account NAME] [--locked] [(-v|--verbose) | --tabular] [--after TXID:INDEX] [--max INT]"

    it "offers pendingtxs --verbose and --tabular as a choice" $
      usageOf "pendingtxs" `shouldBe` "[-a|--account NAME] [(-v|--verbose) | --tabular] [--after HASH] [--max INT]"

    it "offers balances --verbose and --tabular as a choice" $
      usageOf "balances" `shouldBe` "[-a|--account NAME] [(-v|--verbose) | --tabular] [--after ADDRESS] [--max INT]"

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
        LabelledTx tid -> txHashToHex tid `shouldBe` "ab00000000000000000000000000000000000000000000000000000000000001"
        other -> expectationFailure $ "Not a transaction: " <> show other

    it "is an address, as typed, for any other word" $ do
      readLabelled "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `shouldBe` LabelledAddress "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
      readLabelled "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u" `shouldBe` LabelledAddress "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"

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
