{-# LANGUAGE OverloadedStrings #-}

-- | What the shell completes reads the words typed before the word to
-- complete. The completions themselves are tested by running hw, in
-- "Haskoin.Wallet.WorkflowSpec".
module Haskoin.Wallet.CompletionSpec (spec) where

import Haskoin.Wallet.Completion (accountNamed)
import Test.Hspec

spec :: Spec
spec =
  describe "The account named among the words typed" $ do
    it "is the word after --account" $
      accountNamed ["preparetx", "--account", "savings", "--coin"] `shouldBe` Just "savings"

    it "is the word after -a" $
      accountNamed ["preparetx", "-a", "savings", "--coin"] `shouldBe` Just "savings"

    it "is the rest of a word that starts with -a" $
      accountNamed ["preparetx", "-asavings", "--coin"] `shouldBe` Just "savings"

    it "is the rest of a word that starts with --account=" $
      accountNamed ["preparetx", "--account=savings", "--coin"] `shouldBe` Just "savings"

    it "is none after --, where every word is an argument" $
      accountNamed ["signmsg", "--", "-a", "savings"] `shouldBe` Nothing

    it "is none without --account or -a" $
      accountNamed ["preparetx", "--coin"] `shouldBe` Nothing
