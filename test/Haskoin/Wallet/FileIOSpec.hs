{-# LANGUAGE OverloadedStrings #-}

-- | The JSON files hw reads and writes, and the message of a file it cannot
-- read or write: the file, with the reason the system or the decoder gives
module Haskoin.Wallet.FileIOSpec (spec) where

import Control.Exception (finally)
import Data.Aeson (Value, object, (.=))
import Data.List (isPrefixOf)
import Data.Text (Text)
import Haskoin (btc)
import Haskoin.Wallet.Coin (JsonCoin)
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.TestUtils (inScratchDirectory)
import System.Directory (createDirectory, getPermissions, listDirectory, setOwnerWritable, setPermissions)
import System.FilePath ((</>))
import Test.Hspec

-- | A document of the tests
document :: Value
document = object ["name" .= ("main" :: Text), "index" .= (0 :: Int)]

-- | Another one, to replace the first
renamed :: Value
renamed = object ["name" .= ("savings" :: Text), "index" .= (0 :: Int)]

-- | Whether a failure starts with the text
failsWith :: String -> Either String a -> Bool
failsWith start = either (start `isPrefixOf`) (const False)

spec :: Spec
spec = do
  describe "A JSON file" $
    it "is read back as the document written" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "doc.json"
        writeJsonFile file document `shouldReturn` Right ()
        readJsonFile file `shouldReturn` Right document

  describe "A JSON file that cannot be read" $ do
    it "is named, with the reason: it does not exist" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "missing.json"
        (readJsonFile file :: IO (Either String Value))
          `shouldReturn` Left ("The file " <> file <> " could not be read: no such file or directory")

    it "is named, with the reason: it is a directory" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "folder.json"
        createDirectory file
        (readJsonFile file :: IO (Either String Value))
          `shouldReturn` Left ("The file " <> file <> " could not be read: is a directory")

    -- The reason is the decoder's, in its own words
    it "is named, with the reason of the decoder, when it is not JSON" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "notes.txt"
        writeFile file "not json"
        result <- readJsonFile file :: IO (Either String Value)
        result `shouldSatisfy` failsWith ("The file " <> file <> " could not be read: ")

    it "is named, with the reason of the decoder, when it is not the document expected" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "doc.json"
        writeJsonFile file document `shouldReturn` Right ()
        result <- readMarshalFile btc file :: IO (Either String JsonCoin)
        result `shouldSatisfy` failsWith ("The file " <> file <> " could not be read: ")

  describe "A JSON file that cannot be written" $
    it "is named, with the reason: its directory does not exist" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "missing" </> "doc.json"
        writeJsonFile file document
          `shouldReturn` Left ("The file " <> file <> " could not be written: no such file or directory")

  -- As signtx and signsweep replace the --input file with the signed
  -- transaction
  describe "A JSON file replaced" $ do
    it "reads as the new document" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "doc.json"
        writeJsonFile file document `shouldReturn` Right ()
        replaceJsonFile file renamed `shouldReturn` Right ()
        readJsonFile file `shouldReturn` Right renamed

    -- The new document is written next to the file, then takes its place
    it "leaves no other file next to it" $
      inScratchDirectory $ \dir -> do
        writeJsonFile (dir </> "doc.json") document `shouldReturn` Right ()
        replaceJsonFile (dir </> "doc.json") renamed `shouldReturn` Right ()
        listDirectory dir `shouldReturn` ["doc.json"]

    -- A directory that cannot be written takes no new file
    it "is left as it was when it cannot be replaced, and named with the reason" $
      inScratchDirectory $ \dir -> do
        let folder = dir </> "read-only"
            file = folder </> "doc.json"
        createDirectory folder
        writeJsonFile file document `shouldReturn` Right ()
        writable <- getPermissions folder
        setPermissions folder (setOwnerWritable False writable)
        replaced <- replaceJsonFile file renamed `finally` setPermissions folder writable
        replaced `shouldBe` Left ("The file " <> file <> " could not be written: permission denied")
        readJsonFile file `shouldReturn` Right document
        listDirectory folder `shouldReturn` ["doc.json"]
