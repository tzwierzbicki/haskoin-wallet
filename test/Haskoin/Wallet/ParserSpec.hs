{-# LANGUAGE OverloadedRecordDot #-}

-- | The command line of hw, before anything runs: the usage lines its help
-- shows, and what the arguments ask for
module Haskoin.Wallet.ParserSpec (spec) where

import Haskoin.Wallet.Parser (program)
import Haskoin.Wallet.Parser.Args (Args (..), CommandSpec (..))
import Haskoin.Wallet.Parser.Help (usageParts)
import Haskoin.Wallet.Parser.Run (Request (..), commandNamed, requestOf)
import Test.Hspec

spec :: Spec
spec = do
  describe "The usage line of a command" $ do
    it "puts an option that may be left out in brackets, its names separated by a bar" $
      usageOf "receive" `shouldBe` "[-a|--account NAME] [-l|--label LABEL]"

    -- The argument is repeated, the coins may be, and the strategy has a
    -- default: an argument shows once, an option with a default in brackets
    it "lists the options of preparetx as they are typed" $
      usageOf "preparetx"
        `shouldBe` "ADDRESS:AMOUNT [-a|--account NAME] [-f|--feerate RATE] [-d|--dust SATOSHIS] [-r|--recipientpay] [-m|--minconf INT] [--strategy STRATEGY] [-o|--output FILENAME] [-c|--coin TXHASH:INDEX] [--spendfrom ADDRESS] [--select] [-y|--yes] [--changeback | --newchange]"

    it "puts a choice in parentheses, and a required option with two names too" $
      usageOf "verifymsg" `shouldBe` "(ADDRESS SIGNATURE MESSAGE | (-i|--input FILENAME)) [-n|--network NETWORK]"

    it "shows an option that has to be given at least once as required" $
      usageOf "preparesweep"
        `shouldBe` "[-a|--account NAME] [-n|--network NETWORK] [-k|--prvkeys FILENAME] [-w|--sweepfrom ADDRESS] [--addressfile FILENAME] (-t|--sweepto ADDRESS[:AMOUNT]) [-c|--coin TXHASH:INDEX] [-o|--output FILENAME] [-f|--feerate RATE] [-d|--dust SATOSHIS] [-y|--yes]"

    it "is empty for a command without options" $
      usageOf "version" `shouldBe` ""

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
  where
    usageOf name = case commandNamed program name of
      Just command -> unwords (usageParts command.args.usage)
      Nothing -> error $ "hw has no command " <> name
    -- The options of hw that take a value
    unitOptions = ["-u", "--unit"]
