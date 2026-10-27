{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The helpers the wallet shares: what the other modules rely on them for,
-- stated once here
module Haskoin.Wallet.UtilSpec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalState, execState)
import Data.Aeson (object, (.=))
import Data.Aeson.Types (parseEither)
import Data.Int (Int64)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import Haskoin
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  jsonSpec
  pageSpec
  errorSpec
  addressSpec
  numberSpec
  shuffleSpec
  addressMapSpec

jsonSpec :: Spec
jsonSpec =
  describe "A JSON document as hw writes it" $
    it "is indented by two spaces, nested objects further, and ends with a newline" $
      cs (encodeJsonPretty (object ["gap" .= (20 :: Int), "pending" .= object ["online" .= True]]))
        `shouldBe` ([r|{
  "gap": 20,
  "pending": {
    "online": true
  }
}
|] :: String)

errorSpec :: Spec
errorSpec =
  describe "The result of an IO action that may fail" $ do
    it "is the value when the action succeeded" $
      runExceptT (liftEitherIO (return (Right 'a'))) `shouldReturn` Right 'a'

    it "is the error of the monad when the action failed" $
      runExceptT (liftEitherIO (return (Left "The file is not JSON")))
        `shouldReturn` (Left "The file is not JSON" :: Either String Char)

addressSpec :: Spec
addressSpec =
  describe "An address as text" $ do
    it "is written on its network and read back" $ do
      addrToTextE btc (externalAddr 0) `shouldBe` Right "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
      textToAddrE btc "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `shouldBe` Right (externalAddr 0)

    it "is not read from a text that is no address, which is named" $
      textToAddrE btc "nonsense" `shouldBe` Left "The address nonsense is not valid on btc"

    it "is not read from the form of another network" $
      textToAddrE btc bchExternalAddrText `shouldBe` Left ("The address " <> cs bchExternalAddrText <> " is not valid on btc")

    it "has a form on both Bitcoin and Bitcoin Cash when it pays a public key hash" $ do
      -- The same hash, written the way of each network
      addrToTextE btc bchExternalAddr `shouldBe` Right "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ"
      addrToTextE bch (externalAddr 0) `shouldBe` Right "bitcoincash:qryq64fz8ch3qcfk8twj57crv0pgt0ef353f374c3t"
      onNetwork btc bchExternalAddr `shouldBe` True

    it "has no form on Bitcoin Cash when it is a SegWit address" $ do
      addrToTextE bch segwitAddr `shouldBe` Left "Invalid Address in addrToTextE"
      onNetwork bch segwitAddr `shouldBe` False
      onNetwork btc segwitAddr `shouldBe` True

    it "is written in a message as its text, or as an address of another network" $ do
      addrToString btc segwitAddr `shouldBe` "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
      addrToString bch segwitAddr `shouldBe` "an address of another network"

pageSpec :: Spec
pageSpec =
  describe "A page of a listing, as the database takes it" $ do
    it "has the limit and the offset typed" $
      pageBounds (Page 5 20) `shouldBe` (5 :: Int64, 20)

    -- Larger, they wrapped around: an offset of 2^64 showed the first page,
    -- and a limit of 2^64 + 1 a single row
    it "stops at the largest number of 64 bits, past every row, rather than wrapping around" $
      pageBounds (Page (2 ^ (64 :: Int)) (2 ^ (64 :: Int))) `shouldBe` (9223372036854775807 :: Int64, 9223372036854775807)

numberSpec :: Spec
numberSpec =
  describe "A safe subtraction" $ do
    it "is the difference when it is not negative" $ do
      safeSubtract (5 :: Natural) 3 `shouldBe` Just 2
      safeSubtract (5 :: Natural) 5 `shouldBe` Just 0

    it "is nothing when the difference would be negative, where a natural number would throw" $
      safeSubtract (3 :: Natural) 5 `shouldBe` Nothing

shuffleSpec :: Spec
shuffleSpec =
  describe "The shuffle" $ do
    it "keeps every element once, in another order" $ do
      sort (shuffled 1) `shouldBe` [1 .. 10]
      shuffled 1 `shouldNotBe` [1 .. 10]

    it "gives the same order for the same seed" $
      shuffled 1 `shouldBe` shuffled 1

    it "draws from the generator, so that the next draw differs" $
      show (execState (randomShuffle [1 .. 10 :: Int]) (mkStdGen 1)) `shouldNotBe` show (mkStdGen 1)
  where
    shuffled seed = evalState (randomShuffle [1 .. 10 :: Int]) (mkStdGen seed)

addressMapSpec :: Spec
addressMapSpec =
  describe "A map keyed by address, in JSON" $ do
    it "is written with the text of each address as the key, and read back" $ do
      let paid = Map.fromList [(externalAddr 0, 50000 :: Int), (otherAddr 0, 40000)]
          written = Map.fromList [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", 50000), ("1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", 40000)]
      mapAddrText btc paid `shouldBe` written
      parseEither (mapTextAddr btc) written `shouldBe` Right paid

    it "leaves out an address the network cannot write" $
      mapAddrText bch (Map.fromList [(segwitAddr, 1 :: Int), (externalAddr 0, 2)])
        `shouldBe` Map.fromList [("bitcoincash:qryq64fz8ch3qcfk8twj57crv0pgt0ef353f374c3t", 2)]

    it "is not read when a key is no address of the network" $
      parseEither (mapTextAddr btc) (Map.fromList [("nonsense", 1 :: Int)])
        `shouldBe` Left "Error in $: Invalid address nonsense"
