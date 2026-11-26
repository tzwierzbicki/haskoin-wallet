{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The question of each setting of hw, before it is asked: its rows, its
-- words, and what an answer that is written may be. The setup asks four
-- of them ("Haskoin.Wallet.Workflow.SetupSpec"), and setconfig without
-- an option all twelve ("Haskoin.Wallet.Workflow.SetConfigSpec").
module Haskoin.Wallet.Cli.Question.SettingsSpec (spec) where

import qualified Data.Text as Text
import Haskoin (Network (..), bch, btc, btcTest4)
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.Question
import Haskoin.Wallet.Cli.Question.Menu (Row (..))
import Haskoin.Wallet.Cli.Question.Settings
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Keys (AccountKind (..))
import Test.Hspec

spec :: Spec
spec = do
  questionsSpec
  otherQuestionsSpec
  serverSpec

{- The questions -}

questionsSpec :: Spec
questionsSpec =
  describe "The questions of the setup" $ do
    it "offer the eight networks of a portfolio, with btc and bch checked" $
      [(row.rowName, row.rowChecked) | row <- networksQuestion.questionRows]
        `shouldBe` [ ("btc", True),
                     ("bch", True),
                     ("btctest", False),
                     ("btctest4", False),
                     ("btcreg", False),
                     ("bchtest3", False),
                     ("bchtest4", False),
                     ("bchreg", False)
                   ]

    it "say what each network is, after its name" $
      (rowNote <$> networksQuestion.questionRows)
        `shouldBe` [ "Bitcoin",
                     "Bitcoin Cash",
                     "Bitcoin, test network 3 (deprecated)",
                     "Bitcoin, test network 4",
                     "Bitcoin, a test chain of your own",
                     "Bitcoin Cash, test network 3",
                     "Bitcoin Cash, test network 4",
                     "Bitcoin Cash, a test chain of your own"
                   ]

    -- The default network must be one hw shows
    it "offer the networks just chosen as the default network, in their order, and no other" $
      (rowName <$> (defaultNetworkQuestion [bch, btcTest4]).questionRows) `shouldBe` ["bch", "btctest4"]

    it "give the network a row of the default network names" $
      ((.name) . rowValue <$> (defaultNetworkQuestion [bch, btcTest4]).questionRows) `shouldBe` ["bch", "btctest4"]

    it "ask for the default network with the networks to choose from, the last after an or" $
      (defaultNetworkQuestion [btc, bch, btcTest4]).questionPrompt `shouldBe` "Default network: btc, bch or btctest4"

    -- The one network shown is the only default there can be
    it "ask for the default network with one name when one network is shown" $
      (defaultNetworkQuestion [bch]).questionPrompt `shouldBe` "Default network: bch"

    it "offer bip44 and bip84 as the default type of address, bip44 first" $
      [(row.rowName, row.rowValue) | row <- defaultTypeQuestion.questionRows]
        `shouldBe` [("bip44", Bip44), ("bip84", Bip84)]

    -- hw always has a default network and a default type
    it "offer no way to choose no default network or no default type" $
      filter (== "none") ((rowName <$> (defaultNetworkQuestion [btc, bch]).questionRows) <> (rowName <$> defaultTypeQuestion.questionRows))
        `shouldBe` []

    -- hw keeps no journal unless its user asks, so no is what Enter takes
    it "offer no and yes for the journal, no first" $
      [(row.rowName, row.rowValue) | row <- journalQuestion.questionRows]
        `shouldBe` [("no", False), ("yes", True)]

    -- The word ledger alone could be taken for something else
    it "name ledger as the accounting program in the title of the question about the journal" $
      journalQuestion.questionTitle `shouldBe` "Should hw keep a journal for ledger, the accounting program?"

    -- Most users of a wallet have never met ledger
    it "say what ledger is, where it comes from and what it is for, before anything about hw" $
      journalQuestion.questionWhy
        `shouldSatisfy` Text.isPrefixOf
          "ledger is a free accounting program of the command line (ledger-cli.org). \
          \It keeps books by double entry from a text file you write, the journal. \
          \Its reports say what each account holds, what you earned and what you spent, over any period. "

    it "say that exportjournal still writes the journal for a user who answers no" $
      journalQuestion.questionWhy `shouldSatisfy` Text.isSuffixOf "With no, hw exportjournal still writes the journal when you ask."

    it "ask for the journal with no and yes to choose from" $
      journalQuestion.questionPrompt `shouldBe` "Keep a journal for ledger: no or yes"

{- The questions setconfig adds -}

otherQuestionsSpec :: Spec
otherQuestionsSpec =
  describe "The questions of the settings the setup does not ask about" $ do
    -- btc and bch both stand for whole coins, as --unit takes them
    it "offer btc, bch, bit and sat as the unit, btc first" $
      [(row.rowName, row.rowValue) | row <- unitQuestion.questionRows]
        `shouldBe` [("btc", UnitBitcoin), ("bch", UnitBitcoinCash), ("bit", UnitBit), ("sat", UnitSatoshi)]

    it "ask for the unit with the four names to choose from" $
      unitQuestion.questionPrompt `shouldBe` "Unit of the amounts: btc, bch, bit or sat"

    it "offer compact, verbose and tabular as the view of the listings, compact first" $
      [(row.rowName, row.rowValue) | row <- listingViewQuestion.questionRows]
        `shouldBe` [("compact", Compact), ("verbose", Verbose), ("tabular", Table)]

    it "offer no and yes for the reverse order of the listings, no first" $
      [(row.rowName, row.rowValue) | row <- listingReverseQuestion.questionRows]
        `shouldBe` [("no", False), ("yes", True)]

    -- hw copies the database unless its user turns that off
    it "offer yes and no for the copies of the database, yes first" $
      [(row.rowName, row.rowValue) | row <- autoBackupDatabaseQuestion.questionRows]
        `shouldBe` [("yes", True), ("no", False)]

    it "offer yes and no for the copies of the labels, yes first" $
      [(row.rowName, row.rowValue) | row <- autoBackupLabelsQuestion.questionRows]
        `shouldBe` [("yes", True), ("no", False)]

    it "start every question on its first row, the default of hw" $
      [unitQuestion.questionCursor, listingViewQuestion.questionCursor, listingReverseQuestion.questionCursor, autoBackupDatabaseQuestion.questionCursor, autoBackupLabelsQuestion.questionCursor]
        `shouldBe` [0, 0, 0, 0, 0]

{- The answers that are written -}

serverSpec :: Spec
serverSpec =
  describe "An answer that is written, about the server" $ do
    it "is the address of a server that starts with https://" $
      readHost "https://store.example.org/" `shouldBe` Right "https://store.example.org/"

    -- A haskoin-store on the same computer
    it "is the address of a server that starts with http://" $
      readHost "http://localhost:3000/" `shouldBe` Right "http://localhost:3000/"

    -- hw can send a request to a web address only
    it "is refused as an address without http:// or https://" $
      readHost "localhost:3000" `shouldBe` Left "Type the address of a server, which starts with http:// or https://"

    it "is refused as an address that names no server after https://" $
      readHost "https://" `shouldBe` Left "Type the address of a server, which starts with http:// or https://"

    it "is refused as an address with a space" $
      readHost "https://store.example.org/ now" `shouldBe` Left "The address of a server has no space"

    it "shows the address the question starts on as it is" $
      (hostEntry "http://localhost:3000/").entryName "http://localhost:3000/" `shouldBe` "http://localhost:3000/"

    it "is a gap from 1 to 1000" $
      ((gapEntry 20).entryRead <$> ["1", "20", "1000"]) `shouldBe` [Right 1, Right 20, Right 1000]

    -- A gap of zero would refuse every new address
    it "is refused as a gap of 0, or past 1000" $
      ((gapEntry 20).entryRead <$> ["0", "1001"])
        `shouldBe` [Left "Type a whole number from 1 to 1000", Left "Type a whole number from 1 to 1000"]

    it "is refused as a gap that is no whole number in digits" $
      ((gapEntry 20).entryRead <$> ["twenty", "2.5", "-3"])
        `shouldBe` replicate 3 (Left "Type a whole number from 1 to 1000")

    it "is a number of requests at once from 1 to 100" $
      ((parallelRequestsEntry 1).entryRead <$> ["1", "100"]) `shouldBe` [Right 1, Right 100]

    it "is refused as 0 requests at once, or more than 100" $
      ((parallelRequestsEntry 1).entryRead <$> ["0", "101"])
        `shouldBe` [Left "Type a whole number from 1 to 100", Left "Type a whole number from 1 to 100"]

    it "asks for the gap and the requests with the numbers they take" $
      [(gapEntry 20).entryPrompt, (parallelRequestsEntry 1).entryPrompt]
        `shouldBe` ["Gap, from 1 to 1000", "Requests at once, from 1 to 100"]
