{-# LANGUAGE OverloadedStrings #-}

-- | The keys of the database: hashes and outpoints as the rows store them
module Haskoin.Wallet.DatabaseSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec =
  describe "The keys of the database" $ do
    it "store a transaction hash as the bytes of its hexadecimal form, and read it back" $ do
      encodeHex (keyBytes (hashBytes (fakeTxHash 1))) `shouldBe` "0000000000000000000000000000000000000000000000000000000000000001"
      bytesToHash (hashBytes (fakeTxHash 1)) `shouldBe` Right (fakeTxHash 1)

    it "store an outpoint as its 36 serialized bytes, the hash first, and read it back" $ do
      let outpoint = OutPoint (fakeTxHash 1) 7
      BS.length (keyBytes (outpointBytes outpoint)) `shouldBe` 36
      BS.take 32 (keyBytes (outpointBytes outpoint)) `shouldBe` S.encode (fakeTxHash 1)
      bytesToOutpoint (outpointBytes outpoint) `shouldBe` Right outpoint

    it "put the outpoints of one transaction between its lowest and highest index" $ do
      let outpoint = OutPoint (fakeTxHash 1) 7
      outpointBytes outpoint `shouldSatisfy` (>= outpointBytes (OutPoint (fakeTxHash 1) minBound))
      outpointBytes outpoint `shouldSatisfy` (<= outpointBytes (OutPoint (fakeTxHash 1) maxBound))
      outpointBytes (OutPoint (fakeTxHash 2) 0) `shouldSatisfy` (> outpointBytes (OutPoint (fakeTxHash 1) maxBound))
