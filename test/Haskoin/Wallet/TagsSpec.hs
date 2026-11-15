{-# LANGUAGE OverloadedStrings #-}

-- | The rules of a tag and of its colour, as the user types them: which
-- words are tags, how capital letters are taken, and which colours a tag
-- can have
module Haskoin.Wallet.TagsSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Either (isLeft)
import qualified Data.Text as Text
import Haskoin.Wallet.Tags
import Test.Hspec

spec :: Spec
spec = do
  describe "A tag typed by the user" $ do
    it "is the word itself when it is made of small letters" $
      (tagText <$> readTag "savings") `shouldBe` Right "savings"

    it "takes capital letters as small ones, so that Kyc and kyc are one tag" $ do
      (tagText <$> readTag "Kyc") `shouldBe` Right "kyc"
      readTag "KYC" `shouldBe` readTag "kyc"

    it "may hold digits, dashes and underscores" $
      (tagText <$> readTag "tax-2026_q1") `shouldBe` Right "tax-2026_q1"

    it "may be made of digits alone" $
      (tagText <$> readTag "2026") `shouldBe` Right "2026"

    it "is refused when it is empty" $
      readTag "" `shouldBe` Left "A tag cannot be empty"

    it "is refused a space, since a tag is one word" $
      readTag "cold storage" `shouldBe` Left "\"cold storage\" has a space: a tag is one word. Give several tags one after the other."

    it "is refused a slash, which would read as a path" $
      readTag "bank/kyc" `shouldBe` Left "\"bank/kyc\" is not a tag: a tag is made of the letters a to z, digits, - and _"

    it "is refused a colon, which would read as a coin" $
      readTag "a:1" `shouldSatisfy` isLeft

    it "is refused an accented letter, which can be written in two ways that look the same" $
      readTag "impôts" `shouldBe` Left "\"impôts\" is not a tag: a tag is made of the letters a to z, digits, - and _"

    it "is refused a dash in front, which a command line reads as an option" $
      readTag "-kyc" `shouldBe` Left "\"-kyc\" is not a tag: a tag starts with a letter or a digit"

    -- 32 letters are kept, and 33 are refused
    it "has 32 characters at most" $ do
      (tagText <$> readTag (Text.replicate 32 "a")) `shouldBe` Right "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      readTag (Text.replicate 33 "a") `shouldBe` Left "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" is too long: a tag has 32 characters at most"

  describe "Several tags on one line" $
    it "are written with a space between them, as a cell of a spreadsheet holds them" $
      (tagsText <$> mapM readTag ["kyc", "cold"]) `shouldBe` Right "kyc cold"

  describe "The colour of a tag" $ do
    it "is one of six, by its name" $
      (colorName <$> tagColors) `shouldBe` ["red", "green", "yellow", "blue", "magenta", "cyan"]

    it "is read from its name, whatever the capital letters" $ do
      readColorChoice "red" `shouldBe` Right (Just TagRed)
      readColorChoice "RED" `shouldBe` Right (Just TagRed)

    it "is removed by the word none" $
      readColorChoice "none" `shouldBe` Right Nothing

    it "is refused another word, with the colours to choose from" $
      readColorChoice "pink" `shouldBe` Left "\"pink\" is not a colour of a tag. Choose one of red, green, yellow, blue, magenta, cyan, or none to remove the colour."

  describe "A tag in JSON" $ do
    it "is written with its name and its colour" $
      (encode . (`ShownTag` Just TagRed) <$> readTag "kyc") `shouldBe` Right "{\"color\":\"red\",\"name\":\"kyc\"}"

    it "is written with a null colour when it has none" $
      (encode . plainTag <$> readTag "kyc") `shouldBe` Right "{\"color\":null,\"name\":\"kyc\"}"

    it "reads back as it was written" $ do
      let shown = (`ShownTag` Just TagGreen) <$> readTag "savings"
      (decode . encode <$> shown) `shouldBe` (Just <$> shown)
