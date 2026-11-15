{-# LANGUAGE OverloadedStrings #-}

-- | The rules of the address book that need no database: the address of
-- a contact in groups of four characters, and the order contacts lists
-- the address book in.
module Haskoin.Wallet.ContactsSpec (spec) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Contacts
import Haskoin.Wallet.TestUtils (seenTime)
import Test.Hspec

-- | A contact with the address and the label given, and no tag
contact :: Text -> Text -> Contact
contact address label = Contact address label [] seenTime

-- | The labels of contacts, in the order contacts lists them
labelsInOrder :: [Contact] -> [Text]
labelsInOrder = map contactLabel . sortOn contactOrder

spec :: Spec
spec = do
  describe "An address in fours" $ do
    -- 42 characters: ten groups of four, and two left
    it "is written in groups of four characters, a space between them" $
      inFours "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
        `shouldBe` "bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4"

    -- 34 characters: eight groups of four, and two left
    it "ends with a shorter group when it does not divide by four" $
      inFours "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
        `shouldBe` "1JCq 8Aa9 d9rg 4T4X V93R V3DM xd5u 7GkS SU"

    it "keeps the prefix of a Bitcoin Cash address in one piece, before the groups" $
      inFours "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"
        `shouldBe` "bitcoincash: qpm2 qszn hks2 3z76 29mm s6s4 cwef 74vc wvy2 2gdx 6a"

    it "is the address again once its spaces are taken out" $
      Text.filter (/= ' ') (inFours "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a")
        `shouldBe` "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"

  describe "The order of the address book" $ do
    it "is that of the labels, in the order of the alphabet" $
      labelsInOrder [contact "1c" "Zoe", contact "1a" "Alice", contact "1b" "Mallory"]
        `shouldBe` ["Alice", "Mallory", "Zoe"]

    it "takes capital and small letters alike" $
      labelsInOrder [contact "1c" "bob", contact "1a" "Carol", contact "1b" "alice"]
        `shouldBe` ["alice", "bob", "Carol"]

    it "puts a contact without a label after those that have one" $
      labelsInOrder [contact "1a" "", contact "1b" "Zoe", contact "1c" "Alice"]
        `shouldBe` ["Alice", "Zoe", ""]

    it "puts two contacts of one label in the order of their addresses" $
      map contactAddress (sortOn contactOrder [contact "1c" "Alice", contact "1a" "Alice", contact "1b" "Alice"])
        `shouldBe` ["1a", "1b", "1c"]
