{-# LANGUAGE OverloadedStrings #-}

-- | Short ids: the first 8 digits of a hash, which the commands take in
-- place of the hash, and what a command does when two hashes share one
module Haskoin.Wallet.ShortIdSpec (spec) where

import qualified Data.ByteString as BS
import Haskoin (OutPoint (..))
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.TestUtils (a1075db5First, a1075db5Second, b2d4f6a8Txid, typedShortId)
import Test.Hspec

spec :: Spec
spec = do
  shortIdSpec
  typedSpec
  rangeSpec
  pickSpec

-- | The short id a1075db5, as the user types it
a1075db5 :: ShortId
a1075db5 = typedShortId "a1075db5"

shortIdSpec :: Spec
shortIdSpec =
  describe "The short id of a hash" $ do
    it "is the first 8 digits of the txid, as hw shows the txid" $
      shortHashText a1075db5First `shouldBe` "a1075db5"

    it "of a coin is the short id of its txid, a colon, and its index" $
      shortCoinText (OutPoint a1075db5First 1) `shouldBe` "a1075db5:1"

    it "is shared by txids that start with the same 8 digits" $
      startsWith a1075db5 a1075db5Second `shouldBe` True

    it "is not that of a txid that starts with other digits" $
      startsWith a1075db5 b2d4f6a8Txid `shouldBe` False

typedSpec :: Spec
typedSpec =
  describe "A hash as the user types it" $ do
    it "is the hash in full, for 64 hexadecimal digits" $
      readHashRef "a1075db5c0ffee00000000000000000000000000000000000000000000000001" `shouldBe` Just (FullHash a1075db5First)

    it "is a short id, for 8 hexadecimal digits" $
      (hashRefText <$> readHashRef "a1075db5") `shouldBe` Just "a1075db5"

    it "is read in capitals too, and written in lower case" $
      (hashRefText <$> readHashRef "A1075DB5") `shouldBe` Just "a1075db5"

    it "is nothing for 7 digits, fewer than a short id" $
      readHashRef "a1075db" `shouldBe` Nothing

    it "is nothing for 9 digits, more than a short id and fewer than a hash" $
      readHashRef "a1075db5c" `shouldBe` Nothing

    it "is nothing for 8 characters that are not all hexadecimal digits" $
      readHashRef "a1075dbz" `shouldBe` Nothing

    it "names a coin as typed, by its short id and its index" $
      coinRefText (CoinRef (ShortHash a1075db5) 1) `shouldBe` "a1075db5:1"

rangeSpec :: Spec
rangeSpec =
  describe "The keys that start with a short id" $ do
    it "run from the short id itself to below the next short id" $
      shortIdRange a1075db5 `shouldBe` (BS.pack [0xa1, 0x07, 0x5d, 0xb5], Just (BS.pack [0xa1, 0x07, 0x5d, 0xb6]))

    -- a1ffffff + 1 = a2000000
    it "end where the digits before the last ones go up by one, when the last ones are all f" $
      (shortIdRange <$> readShortId "a1ffffff") `shouldBe` Just (BS.pack [0xa1, 0xff, 0xff, 0xff], Just (BS.pack [0xa2, 0x00, 0x00, 0x00]))

    it "have no end for ffffffff, the last short id" $
      (snd . shortIdRange <$> readShortId "ffffffff") `shouldBe` Just Nothing

pickSpec :: Spec
pickSpec = do
  describe "The hash a short id names, among those a command can name" $ do
    it "is the one that has the short id" $
      pickHash "none" transactionsNamed a1075db5 [b2d4f6a8Txid, a1075db5First] `shouldBe` Right a1075db5First

    it "is the error the command gives, when none has the short id" $
      pickHash "The wallet has no transaction a1075db5" transactionsNamed a1075db5 [b2d4f6a8Txid]
        `shouldBe` Left "The wallet has no transaction a1075db5"

    it "is the one found twice, which counts once" $
      pickHash "none" transactionsNamed a1075db5 [a1075db5First, a1075db5First] `shouldBe` Right a1075db5First

    it "is refused when two have the short id, each named in full in the order of its digits, and the full txid asked for" $
      pickHash "none" transactionsNamed a1075db5 [a1075db5Second, a1075db5First]
        `shouldBe` Left
          "The short id a1075db5 matches 2 transactions:\n\
          \  a1075db5c0ffee00000000000000000000000000000000000000000000000001\n\
          \  a1075db5c0ffee00000000000000000000000000000000000000000000000002\n\
          \Give the full txid."

    -- A pending transaction is named by its nosigHash too, which is no txid
    it "asks for the full hash when the two are pending transactions" $
      pickHash "none" pendingNamed a1075db5 [a1075db5First, a1075db5Second]
        `shouldBe` Left
          "The short id a1075db5 matches 2 pending transactions:\n\
          \  a1075db5c0ffee00000000000000000000000000000000000000000000000001\n\
          \  a1075db5c0ffee00000000000000000000000000000000000000000000000002\n\
          \Give the full hash."

  describe "The coin a short id names, with its index" $ do
    it "is the coin of the transaction with the short id at that index" $
      pickCoin "none" a1075db5 1 [OutPoint a1075db5First 0, OutPoint a1075db5First 1, OutPoint b2d4f6a8Txid 1]
        `shouldBe` Right (OutPoint a1075db5First 1)

    it "is the error the command gives, when no coin of such a transaction is at the index" $
      pickCoin "The wallet has no coin a1075db5:2" a1075db5 2 [OutPoint a1075db5First 1]
        `shouldBe` Left "The wallet has no coin a1075db5:2"

    it "is refused when two transactions with the short id have a coin at the index, each coin named with its full txid" $
      pickCoin "none" a1075db5 1 [OutPoint a1075db5Second 1, OutPoint a1075db5First 1]
        `shouldBe` Left
          "The short id a1075db5:1 matches 2 coins:\n\
          \  a1075db5c0ffee00000000000000000000000000000000000000000000000001:1\n\
          \  a1075db5c0ffee00000000000000000000000000000000000000000000000002:1\n\
          \Give the coin with its full txid."
