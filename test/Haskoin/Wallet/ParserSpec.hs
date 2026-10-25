-- | The command line: the options whose values have rules of their own
module Haskoin.Wallet.ParserSpec (spec) where

import Data.List (isInfixOf)
import Haskoin.Wallet.Parser
import Numeric.Natural (Natural)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, getParseResult, renderFailure)
import Test.Hspec

spec :: Spec
spec =
  describe "The account index of createacc" $ do
    it "goes up to 2147483647, the last hardened index of BIP32" $ do
      indexGiven ["createacc", "main", "--derivation", "2147483647"] `shouldBe` Just (Just 2147483647)
      indexGiven ["createacc", "main", "-d", "0"] `shouldBe` Just (Just 0)

    it "is taken from the seed when not given" $
      indexGiven ["createacc", "main"] `shouldBe` Just Nothing

    it "refuses a larger number, which no account can have, and says the range" $ do
      indexGiven ["createacc", "main", "--derivation", "2147483648"] `shouldBe` Nothing
      failure ["createacc", "main", "--derivation", "2147483648"] `shouldSatisfy` ("An account index is a number from 0 to 2147483647" `isInfixOf`)
      indexGiven ["createacc", "main", "--derivation", "-1"] `shouldBe` Nothing

-- | The account index the command line gives createacc, if it parses
indexGiven :: [String] -> Maybe (Maybe Natural)
indexGiven args = do
  (CommandCreateAcc _ _ index _, _, _) <- getParseResult (execParserPure defaultPrefs programParser args)
  return index

-- | What hw prints when the command line does not parse
failure :: [String] -> String
failure args =
  case execParserPure defaultPrefs programParser args of
    Failure f -> fst (renderFailure f "hw")
    _ -> ""
