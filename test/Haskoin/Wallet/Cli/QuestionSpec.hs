{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A question of hw about a setting, before it is asked: the rows a
-- typed answer names, and the answer a question starts on when hw has a
-- value already. "Haskoin.Wallet.Cli.Question.MenuSpec" has the rules of
-- the menus, and "Haskoin.Wallet.Cli.Question.SettingsSpec" the question
-- of each setting.
module Haskoin.Wallet.Cli.QuestionSpec (spec) where

import Data.Text (Text)
import Haskoin (Network (..), bch, btc, btcTest4)
import Haskoin.Wallet.Cli.Question
import Haskoin.Wallet.Cli.Question.Menu (Row (..))
import Haskoin.Wallet.Cli.Question.Settings (defaultNetworkQuestion, defaultTypeQuestion, networksQuestion)
import Haskoin.Wallet.Keys (AccountKind (..))
import Test.Hspec

spec :: Spec
spec = do
  typedSpec
  startSpec

{- Typed answers -}

-- | The names of the networks a typed answer to the first question gives
typedNetworks :: Text -> Either String [String]
typedNetworks typed = map (.name) <$> readChecked networksQuestion.questionRows typed

typedSpec :: Spec
typedSpec =
  describe "A typed answer to the question about the networks" $ do
    it "is btc and bch when empty, the networks checked by default" $
      typedNetworks "" `shouldBe` Right ["btc", "bch"]

    it "names the networks to show, separated by spaces" $
      typedNetworks "btc btctest4" `shouldBe` Right ["btc", "btctest4"]

    it "names them separated by commas too" $
      typedNetworks "bch, btctest4" `shouldBe` Right ["bch", "btctest4"]

    it "gives them in the order of the networks of a portfolio, each once, whatever the order typed" $
      typedNetworks "btctest4 btc btc" `shouldBe` Right ["btc", "btctest4"]

    it "reads a name in any case" $
      typedNetworks "BTC" `shouldBe` Right ["btc"]

    it "is refused with a name that is no network, with the names there are" $
      typedNetworks "btc doge"
        `shouldBe` Left "\"doge\" is none of btc, bch, btctest, btctest4, btcreg, bchtest3, bchtest4, bchreg"

{- The answer a question starts on -}

-- setconfig asks about a setting that has a value already, and Enter
-- alone must keep it
startSpec :: Spec
startSpec =
  describe "A question about a setting that hw has a value for" $ do
    -- bip44 is row 0, and bip84 row 1
    it "starts on the row of that value, which Enter then keeps" $
      (startingOn Bip84 defaultTypeQuestion).questionCursor `shouldBe` 1

    it "starts on its first row when the value is the one of that row" $
      (startingOn Bip44 defaultTypeQuestion).questionCursor `shouldBe` 0

    -- The default network was hidden by the answer before
    it "starts on its first row when none of its rows has the value" $
      (startingOn btcTest4 (defaultNetworkQuestion [btc, bch])).questionCursor `shouldBe` 0

    it "keeps its rows in their order, wherever it starts" $
      (rowName <$> (startingOn Bip84 defaultTypeQuestion).questionRows) `shouldBe` ["bip44", "bip84"]

    it "starts with the networks hw shows checked, and no other" $
      [(row.rowName, row.rowChecked) | row <- (checking [bch, btcTest4] networksQuestion).questionRows]
        `shouldBe` [ ("btc", False),
                     ("bch", True),
                     ("btctest", False),
                     ("btctest4", True),
                     ("btcreg", False),
                     ("bchtest3", False),
                     ("bchtest4", False),
                     ("bchreg", False)
                   ]
