{-# LANGUAGE OverloadedStrings #-}

-- | The JSON files hw reads and writes, and the message of a file it cannot
-- read or write: the file, with the reason the system or the decoder gives
module Haskoin.Wallet.FileIOSpec (spec) where

import Control.Exception (ErrorCall (..), finally, throwIO)
import Data.Aeson (Value, object, (.=))
import Data.Bits ((.&.))
import Data.List (isPrefixOf)
import Data.Text (Text)
import Haskoin (btc)
import Haskoin.Wallet.Database.Coin (JsonCoin)
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.TestUtils (inScratchDirectory, underUmask)
import System.Directory (createDirectory, getPermissions, listDirectory, setOwnerWritable, setPermissions)
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus)
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

  describe "A JSON file that cannot be written" $ do
    it "is named, with the reason: its directory does not exist" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "missing" </> "doc.json"
        writeJsonFile file document
          `shouldReturn` Left ("The file " <> file <> " could not be written: no such file or directory")

    -- The path was free when the command checked it, and a file took it
    -- while the command did its work
    it "is named, with the reason, when a file took its path, which is kept" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "doc.json"
        writeFile file "kept"
        writeJsonFile file document
          `shouldReturn` Left ("The file " <> file <> " could not be written: file exists")
        readFile file `shouldReturn` "kept"

    it "is named, with the reason: a directory stands at its path" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "doc.json"
        createDirectory file
        writeJsonFile file document
          `shouldReturn` Left ("The file " <> file <> " could not be written: is a directory")

  describe "A new file" $ do
    -- It is written next to its path, then takes it
    it "leaves no other file next to it" $
      inScratchDirectory $ \dir -> do
        writeJsonFile (dir </> "doc.json") document `shouldReturn` Right ()
        listDirectory dir `shouldReturn` ["doc.json"]

    it "is its owner's alone to read and write" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "doc.json"
        writeJsonFile file document `shouldReturn` Right ()
        -- The permission bits of rw-------
        (.&. 0o777) . fileMode <$> getFileStatus file `shouldReturn` 0o600

    it "is not left at its path, nor next to it, when it could not be filled" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "doc.json"
        placeFile AtFreePath file (\_ -> throwIO (ErrorCall "no contents"))
          `shouldThrow` (== ErrorCall "no contents")
        listDirectory dir `shouldReturn` []

  -- Under the umask 022, a directory would be made rwxr-xr-x and a file
  -- rw-r--r--, which other users of the computer can read
  describe "The data directory and the database, made private" $ do
    it "is a directory its owner alone can enter, whatever the umask lets" $
      inScratchDirectory $ \dir -> underUmask 0o022 $ do
        privateDirectory (dir </> ".hw")
        -- The permission bits of rwx------
        (.&. 0o777) . fileMode <$> getFileStatus (dir </> ".hw") `shouldReturn` 0o700

    it "is an empty file its owner alone can read and write, whatever the umask lets" $
      inScratchDirectory $ \dir -> underUmask 0o022 $ do
        privateFile (dir </> "accounts.sqlite")
        (.&. 0o777) . fileMode <$> getFileStatus (dir </> "accounts.sqlite") `shouldReturn` 0o600
        readFile (dir </> "accounts.sqlite") `shouldReturn` ""

    it "leaves a file that exists as it is" $
      inScratchDirectory $ \dir -> do
        writeFile (dir </> "accounts.sqlite") "a database"
        privateFile (dir </> "accounts.sqlite")
        readFile (dir </> "accounts.sqlite") `shouldReturn` "a database"

  describe "A file replaced" $
    it "is left as it was when the new one could not be filled" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "doc.json"
        writeJsonFile file document `shouldReturn` Right ()
        placeFile InPlaceOfFile file (\_ -> throwIO (ErrorCall "no contents"))
          `shouldThrow` (== ErrorCall "no contents")
        readJsonFile file `shouldReturn` Right document
        listDirectory dir `shouldReturn` ["doc.json"]

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
