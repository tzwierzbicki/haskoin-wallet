{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the shell completes: the values each option of where a command
-- works offers, and the options typed before the word to complete, which
-- narrow what the wallet offers. The completions that read the database,
-- the indexes of the wallets and portfolios among them, are tested by
-- running hw in a home of its own, in "Haskoin.Wallet.Workflow.CommandLineSpec".
module Haskoin.Wallet.Cli.CompletionSpec (spec) where

import Data.List (sort)
import Haskoin (bch)
import Haskoin.Wallet.Cli.Completion (Typed (..), optionsTyped)
import Haskoin.Wallet.Cli.Parser (program)
import Haskoin.Wallet.Cli.Parser.Args (Args (..), CommandSpec (..))
import Haskoin.Wallet.Cli.Parser.Run (commandNamed)
import Haskoin.Wallet.Keys (AccountKind (..))
import qualified Options.Applicative as O
import Test.Hspec

spec :: Spec
spec = do
  describe "The options typed before the word to complete" $ do
    it "name the wallet after --wallet" $
      optionsTyped ["switch", "--wallet", "1"] `shouldBe` Typed (Just 1) Nothing Nothing

    -- zsh and fish pass --wallet=1 whole; bash breaks it, and the words
    -- are joined again before they are read
    it "name the wallet in --wallet=1" $
      optionsTyped ["switch", "--wallet=1"] `shouldBe` Typed (Just 1) Nothing Nothing

    -- The command line takes wallet1 for 1, as the listings name it
    it "name the wallet by the name the listings give it, as the command line takes it" $
      optionsTyped ["switch", "--wallet", "wallet1"] `shouldBe` Typed (Just 1) Nothing Nothing

    it "name the network after --network or -n, and the type after --type or -t" $ do
      optionsTyped ["receive", "--network", "bch", "--type", "bip44"] `shouldBe` Typed Nothing (Just bch) (Just Bip44)
      optionsTyped ["receive", "-n", "bch", "-t", "bip84"] `shouldBe` Typed Nothing (Just bch) (Just Bip84)

    it "name all three together, whatever their order" $
      optionsTyped ["receive", "-t", "bip84", "--network", "bch", "--wallet", "2"] `shouldBe` Typed (Just 2) (Just bch) (Just Bip84)

    -- -w is --sweep-from on preparesweep
    it "name no wallet after -w, since --wallet has no short form" $
      optionsTyped ["preparesweep", "-w", "1"] `shouldBe` Typed Nothing Nothing Nothing

    it "name nothing by a value that names no network or no type, as if the option were not typed" $
      optionsTyped ["receive", "--network", "btcx", "--type", "bip49"] `shouldBe` Typed Nothing Nothing Nothing

    it "name nothing after --, where every word is an argument" $
      optionsTyped ["signmsg", "--", "--wallet", "1", "-n", "bch"] `shouldBe` Typed Nothing Nothing Nothing

  describe "What the shell is offered for an option" $ do
    -- The networks of a portfolio: btc, bch and the six test networks
    it "is every network for --network" $
      sort <$> offeredFor "receive" ["--network", ""]
        `shouldReturn` ["bch", "bchreg", "bchtest", "bchtest4", "btc", "btcreg", "btctest", "btctest4"]

    it "is the networks that start with what was typed for --network" $
      sort <$> offeredFor "addresses" ["-n", "btct"] `shouldReturn` ["btctest", "btctest4"]

    it "is the two types for --type" $
      sort <$> offeredFor "coins" ["--type", ""] `shouldReturn` ["bip44", "bip84"]

-- | What optparse-applicative offers the shell for the last word typed
-- after the command named, as the completion script of bash asks it: the
-- index of the word, then the words, hw first. Only the completer of the
-- option of the last word runs, so no database is read.
offeredFor :: String -> [String] -> IO [String]
offeredFor name typed =
  case commandNamed program name of
    Nothing -> fail ("hw has no command " <> name)
    Just command ->
      case O.execParserPure O.defaultPrefs (O.info command.args.parser mempty) completionArgs of
        O.CompletionInvoked completion -> lines <$> O.execCompletion completion "hw"
        _ -> fail "The words were not read as a request of the shell"
  where
    completionArgs = ["--bash-completion-index", show (length typed)] <> concatMap (\w -> ["--bash-completion-word", w]) ("hw" : typed)
