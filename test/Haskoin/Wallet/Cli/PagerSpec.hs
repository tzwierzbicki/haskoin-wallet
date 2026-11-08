{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Cli.PagerSpec (spec) where

import Haskoin.Wallet.Cli.Pager (Shown (..), lessOptions, pagerNamed, readLessVersion, tableShown)
import Test.Hspec

spec :: Spec
spec = do
  describe "The pager of a listing" $ do
    it "is less when nothing names one" $
      pagerNamed Nothing Nothing Nothing `shouldBe` Just "less"

    it "is the one HW_PAGER names first, then the key pager of config.json, then PAGER" $ do
      pagerNamed (Just "most") (Just "less -S") (Just "more") `shouldBe` Just "most"
      pagerNamed Nothing (Just "less -S") (Just "more") `shouldBe` Just "less -S"
      pagerNamed Nothing Nothing (Just "more") `shouldBe` Just "more"

    it "is none when the first name set is empty, blank or cat" $ do
      pagerNamed (Just "") (Just "less") (Just "more") `shouldBe` Nothing
      pagerNamed Nothing (Just "  ") (Just "more") `shouldBe` Nothing
      pagerNamed Nothing Nothing (Just "cat") `shouldBe` Nothing

    it "gives less -F, -R and -X for lines of text, whatever its version" $
      lessOptions TextLines (Just 668) `shouldBe` "FRX"

    it "adds -S for a table, so that its rows are cut at the edge of the screen rather than wrapped" $
      lessOptions TableRows (Just 668) `shouldBe` "FRXS"

    -- With --header, less never quits by itself: a short table gets none
    it "keeps the header of a table taller than the screen on the screen with less 608, which added --header" $
      lessOptions TallTableRows (Just 608) `shouldBe` "FRXS --header=2"

    it "gives a tall table no --header with a less older than 608" $
      lessOptions TallTableRows (Just 607) `shouldBe` "FRXS"

    it "gives a tall table no --header when the version of less is unknown" $
      lessOptions TallTableRows Nothing `shouldBe` "FRXS"

  -- less keeps the last row of the screen for its prompt: 23 rows of a
  -- screen of 24 show the table
  describe "How tall a table is for less" $ do
    it "is short when it fits on the rows of the screen above the prompt" $
      tableShown (Just 24) 23 `shouldBe` TableRows

    it "is tall when it takes as many lines as the screen has rows" $
      tableShown (Just 24) 24 `shouldBe` TallTableRows

    it "is short on a screen of unknown height, so that a short table still closes by itself" $
      tableShown Nothing 500 `shouldBe` TableRows

  describe "The version of less" $ do
    it "is the number after less on the first line of less --version" $
      readLessVersion "less 668 (GNU regular expressions)\nCopyright (C) 1984-2024  Mark Nudelman\n" `shouldBe` Just 668

    it "is none for what another pager prints" $
      readLessVersion "more from util-linux 2.40.2\n" `shouldBe` Nothing
