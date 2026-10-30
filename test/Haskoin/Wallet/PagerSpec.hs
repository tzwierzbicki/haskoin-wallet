{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.PagerSpec (spec) where

import Haskoin.Wallet.Pager (pagerNamed)
import Test.Hspec

spec :: Spec
spec =
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
