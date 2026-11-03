{-# LANGUAGE OverloadedStrings #-}

-- | The format of BIP 329: the records of the test vectors of the BIP, the
-- lines a file holds and what hw makes of each, the lines hw writes, and
-- the key paths and outpoints of the records. The origins of the records
-- are tested with the descriptors, in "Haskoin.Wallet.DescriptorSpec".
module Haskoin.Wallet.Bip329Spec (spec) where

import Data.Either (isLeft)
import qualified Data.Text.IO as Text
import Haskoin
import Haskoin.Wallet.Bip329
import Haskoin.Wallet.Descriptor (Origin (..), parseOrigin)
import Test.Hspec

spec :: Spec
spec = do
  vectorsSpec
  linesSpec
  writingSpec
  pathSpec

{- The test vectors -}

-- | The records of the test vectors of the BIP, in test/data/bip329
vectorRecords :: IO [Record]
vectorRecords = do
  content <- Text.readFile "test/data/bip329/bip-0329.jsonl"
  return [r | (_, line) <- readLines content, Right (LineRecord r) <- [parseLine line]]

vectorsSpec :: Spec
vectorsSpec =
  describe "The test vectors of BIP 329" $ do
    -- One record of each type, then a second transaction
    it "are eight records, read in the order of their lines" $ do
      content <- Text.readFile "test/data/bip329/bip-0329.jsonl"
      map fst (readLines content) `shouldBe` [1 .. 8]
      (map recordType <$> vectorRecords)
        `shouldReturn` [TxRecord, AddrRecord, PubkeyRecord, InputRecord, OutputRecord, XpubRecord, SpscanRecord, TxRecord]

    it "give each record its reference and its label" $
      (map (\r -> (recordRef r, recordLabel r)) <$> vectorRecords)
        `shouldReturn` [ ("f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd", Just "Transaction"),
                         ("bc1q34aq5drpuwy3wgl9lhup9892qp6svr8ldzyy7c", Just "Address"),
                         ("0283409659355b6d1cc3c32decd5d561abaac86c37a353b52895a5e6c196d6f448", Just "Public Key"),
                         ("f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd:0", Just "Input"),
                         ("f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd:1", Just "Output"),
                         ("xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8", Just "Extended Public Key"),
                         ("spscan1q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zsq9q6qjevn2kmdrnpuxt0v6h2kr2a2epkr0g6nk55ftf0xcxtddazgkrth3e", Just "Silent Payments Scan Key Expression"),
                         ("f546156d9044844e02b181026a1a407abfca62e7ea1159f87bbeaa77b4286c74", Just "Account #1 Transaction")
                       ]

    it "say the output is not spendable, and nothing of the other records" $
      (map recordSpendable <$> vectorRecords)
        `shouldReturn` [Nothing, Nothing, Nothing, Nothing, Just False, Nothing, Nothing, Nothing]

    -- The two transactions are of accounts 0 and 1 of one seed, in BIP84
    -- wallets, which hw does not hold
    it "tell the two transactions apart by the origins of their accounts" $ do
      origins <- map recordOrigin . filter ((== TxRecord) . recordType) <$> vectorRecords
      origins `shouldBe` [Just "wpkh([d34db33f/84'/0'/0'])", Just "wpkh([d34db33f/84'/0'/1'])"]
      map (>>= parseOrigin) origins
        `shouldBe` [ Just (Origin ["wpkh"] "d34db33f" (toGeneric (Deriv :| 84 :| 0 :| 0 :: HardPath))),
                     Just (Origin ["wpkh"] "d34db33f" (toGeneric (Deriv :| 84 :| 0 :| 1 :: HardPath)))
                   ]

{- The lines of a file -}

-- | The transaction of the vectors, as a record with its reference alone
txVector :: Record
txVector = record TxRecord "f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd"

linesSpec :: Spec
linesSpec =
  describe "A line of a file" $ do
    it "is a record of its type and reference, with the fields it has" $
      parseLine "{\"type\": \"tx\", \"ref\": \"f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd\", \"label\": \"Transaction\", \"origin\": \"wpkh([d34db33f/84'/0'/0'])\"}"
        `shouldBe` Right (LineRecord txVector {recordLabel = Just "Transaction", recordOrigin = Just "wpkh([d34db33f/84'/0'/0'])"})

    it "is of another type when the BIP does not define its type, whatever its reference" $
      parseLine "{\"type\": \"future\", \"ref\": {\"not\": \"validated\"}, \"label\": \"Unknown\"}"
        `shouldBe` Right (LineOfOtherType "future")

    it "needs a type and a reference" $ do
      parseLine "{\"ref\": \"f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd\", \"label\": \"Transaction\"}" `shouldSatisfy` isLeft
      parseLine "{\"type\": \"tx\", \"label\": \"Transaction\"}" `shouldSatisfy` isLeft

    it "must be a JSON object" $ do
      parseLine "Transaction f91d0a8a" `shouldSatisfy` isLeft
      parseLine "[\"tx\", \"f91d0a8a\"]" `shouldSatisfy` isLeft

    it "has a label of text, or none" $ do
      parseLine "{\"type\": \"tx\", \"ref\": \"f91d0a8a\", \"label\": 42}" `shouldSatisfy` isLeft
      parseLine "{\"type\": \"tx\", \"ref\": \"f91d0a8a\", \"label\": null}"
        `shouldBe` Right (LineRecord (record TxRecord "f91d0a8a"))

    -- The BIP warns against the text, and some files have it
    it "reads a spendable flag written as the text true or false" $ do
      parseLine "{\"type\": \"output\", \"ref\": \"f91d0a8a:1\", \"spendable\": \"false\"}"
        `shouldBe` Right (LineRecord (record OutputRecord "f91d0a8a:1") {recordSpendable = Just False})
      parseLine "{\"type\": \"output\", \"ref\": \"f91d0a8a:1\", \"spendable\": \"true\"}"
        `shouldBe` Right (LineRecord (record OutputRecord "f91d0a8a:1") {recordSpendable = Just True})
      parseLine "{\"type\": \"output\", \"ref\": \"f91d0a8a:1\", \"spendable\": \"yes\"}" `shouldSatisfy` isLeft

    it "reads the height, fee and value of a transaction, and passes over the time and rate" $
      parseLine "{\"type\": \"tx\", \"ref\": \"f91d0a8a\", \"height\": 800000, \"time\": \"2025-01-23T11:40:35Z\", \"fee\": 1000, \"value\": -41000, \"rate\": {\"USD\": 105620.00}}"
        `shouldBe` Right (LineRecord (record TxRecord "f91d0a8a") {recordHeight = Just 800000, recordFee = Just 1000, recordValue = Just (-41000)})

    it "reads the key path of an address, and the network hw gives a key" $ do
      parseLine "{\"type\": \"addr\", \"ref\": \"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\", \"keypath\": \"/0/5\"}"
        `shouldBe` Right (LineRecord (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") {recordKeypath = Just "/0/5"})
      parseLine "{\"type\": \"xpub\", \"ref\": \"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah\", \"network\": \"btc\"}"
        `shouldBe` Right (LineRecord (record XpubRecord "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah") {recordNetwork = Just "btc"})

    -- The second line is blank, the third holds spaces, and the first ends
    -- as Windows ends a line
    it "is numbered as a text editor numbers it, blank lines and carriage returns passed over, and read without the spaces around it" $
      readLines "{\"type\": \"tx\", \"ref\": \"a\"}\r\n\n   \n  {\"type\": \"addr\", \"ref\": \"b\"}  \nnot a record\n"
        `shouldBe` [ (1, "{\"type\": \"tx\", \"ref\": \"a\"}"),
                     (4, "{\"type\": \"addr\", \"ref\": \"b\"}"),
                     (5, "not a record")
                   ]

{- Writing -}

writingSpec :: Spec
writingSpec =
  describe "A record hw writes" $ do
    it "starts with its type and its reference, then the fields it has, in one compact object" $
      recordLine txVector {recordLabel = Just "Transaction", recordOrigin = Just "pkh([bc560f95/44'/0'/0'])"}
        `shouldBe` "{\"type\":\"tx\",\"ref\":\"f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd\",\"label\":\"Transaction\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\"}"

    it "has only its type and reference when it has nothing else" $
      recordLine (record AddrRecord "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj")
        `shouldBe` "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\"}"

    it "writes a coin as an output with whether it is spendable, its key path, height and value" $
      recordLine
        (record OutputRecord "f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd:1")
          { recordSpendable = Just False,
            recordKeypath = Just "/0/0",
            recordHeight = Just 800000,
            recordValue = Just 50000
          }
        `shouldBe` "{\"type\":\"output\",\"ref\":\"f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd:1\",\"spendable\":false,\"keypath\":\"/0/0\",\"height\":800000,\"value\":50000}"

    it "writes the network of a key after its origin" $
      recordLine
        (record XpubRecord "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah")
          { recordLabel = Just "main",
            recordOrigin = Just "pkh([bc560f95/44'/0'/0'])",
            recordNetwork = Just "btc"
          }
        `shouldBe` "{\"type\":\"xpub\",\"ref\":\"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah\",\"label\":\"main\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\"}"

    it "writes a label as it is, in UTF-8, with a line break in it escaped" $ do
      recordLine (record AddrRecord "a") {recordLabel = Just "Caf\233 \9749"}
        `shouldBe` "{\"type\":\"addr\",\"ref\":\"a\",\"label\":\"Caf\233 \9749\"}"
      recordLine (record AddrRecord "a") {recordLabel = Just "two\nlines"}
        `shouldBe` "{\"type\":\"addr\",\"ref\":\"a\",\"label\":\"two\\nlines\"}"

    it "reads back as it was written" $ do
      let written =
            [ txVector {recordLabel = Just "Transaction", recordOrigin = Just "pkh([bc560f95/44'/0'/0'])", recordHeight = Just 800000, recordFee = Just 1000, recordValue = Just (-41000)},
              (record OutputRecord "f91d0a8a:1") {recordSpendable = Just True, recordKeypath = Just "/1/3", recordValue = Just 9000},
              (record XpubRecord "xpub6CHK") {recordLabel = Just "main", recordNetwork = Just "btc"}
            ]
      map (parseLine . recordLine) written `shouldBe` map (Right . LineRecord) written

    it "ends every line of a file with a line feed" $
      recordsText [record TxRecord "a", record AddrRecord "b"]
        `shouldBe` "{\"type\":\"tx\",\"ref\":\"a\"}\n{\"type\":\"addr\",\"ref\":\"b\"}\n"

{- Key paths and outpoints -}

-- | The id of the transaction of the vectors
vectorTxid :: TxHash
vectorTxid = maybe (error "vectorTxid") id (hexToTxHash "f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd")

pathSpec :: Spec
pathSpec = do
  describe "The key path of a record" $ do
    it "is the path of the address from the account, written as hw shows it" $
      keypathText (Deriv :/ 1 :/ 123) `shouldBe` "/1/123"

    it "reads back as written" $
      parseKeypath "/1/123" `shouldBe` Just (Deriv :/ 1 :/ 123)

    it "is not read as a whole descriptor, nor with hardened steps" $ do
      parseKeypath "wpkh([d34db33f/84'/0'/0'/1/123])" `shouldBe` Nothing
      parseKeypath "/0'/5" `shouldBe` Nothing
      parseKeypath "" `shouldBe` Nothing

  describe "The outpoint of a record" $ do
    it "is the id of the transaction, a colon, and the index" $
      outpointText (OutPoint vectorTxid 1) `shouldBe` "f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd:1"

    it "reads back as written, up to the largest index an output can have" $ do
      readOutpoint "f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd:1" `shouldBe` Just (OutPoint vectorTxid 1)
      readOutpoint "f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd:4294967295" `shouldBe` Just (OutPoint vectorTxid 4294967295)
      readOutpoint "f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd:4294967296" `shouldBe` Nothing

    it "needs the id and the index" $ do
      readOutpoint "f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd" `shouldBe` Nothing
      readOutpoint "f91d0a8a:1" `shouldBe` Nothing
      readOutpoint ":1" `shouldBe` Nothing
