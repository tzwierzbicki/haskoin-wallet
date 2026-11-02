{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The files hw reads and writes: the bytes of a transaction file, and
-- JSON documents, written pretty-printed so that a user can read them, at a
-- path that must be free, or in place of a file the user agreed to replace.
-- A file that cannot be read or written is a message naming it, with the
-- reason the system gives, never an exception.
module Haskoin.Wallet.FileIO where

import Control.Exception (onException, try)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..))
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import GHC.IO.Exception (IOException (..))
import Haskoin
import Haskoin.Wallet.Util
import qualified System.Directory as D
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openBinaryTempFile)

{- Paths -}

checkPathFree :: (MonadIO m) => FilePath -> ExceptT String m ()
checkPathFree path = do
  exist <- liftIO $ D.doesPathExist path
  when exist $ throwError $ "Path '" <> path <> "' already exists"

{- Failures -}

-- | An action on files, or the reason the system gives for its failure, as
-- in "no such file or directory"
systemReason :: IO a -> IO (Either String a)
systemReason action = first reason <$> try action
  where
    reason :: IOException -> String
    reason e = case ioe_description e of
      c : rest -> toLower c : rest
      [] -> show e

-- | The message of a file that could not be read, and why
couldNotRead :: FilePath -> String -> String
couldNotRead file reason = "The file " <> file <> " could not be read: " <> reason

-- | The message of a file that could not be written, and why
couldNotWrite :: FilePath -> String -> String
couldNotWrite file reason = "The file " <> file <> " could not be written: " <> reason

{- Files of bytes -}

-- | Write bytes to a file, or say why they could not be written
writeFileBytes :: FilePath -> ByteString -> IO (Either String ())
writeFileBytes file bytes = first (couldNotWrite file) <$> systemReason (C8.writeFile file bytes)

-- | Replace a file with bytes, or say why it could not be replaced. The
-- bytes are written to a new file next to it first, which then takes its
-- place: a write that fails leaves the file as it was.
replaceFileBytes :: FilePath -> ByteString -> IO (Either String ())
replaceFileBytes file bytes = first (couldNotWrite file) <$> systemReason replace
  where
    replace = do
      (new, h) <- openBinaryTempFile (takeDirectory file) (takeFileName file)
      let write = C8.hPut h bytes >> hClose h >> D.renameFile new file
      write `onException` (hClose h >> D.removeFile new)

-- | Read the bytes of a file, or say why they could not be read
readFileBytes :: FilePath -> IO (Either String ByteString)
readFileBytes file = first (couldNotRead file) <$> systemReason (C8.readFile file)

{- JSON files -}

-- | Write a JSON document to a file, or say why it could not be written
writeJsonFile :: (ToJSON a) => FilePath -> a -> IO (Either String ())
writeJsonFile file doc = writeFileBytes file (encodeJsonPretty doc)

-- | Replace a file with a JSON document, or say why it could not be
-- replaced: see 'replaceFileBytes'
replaceJsonFile :: (ToJSON a) => FilePath -> a -> IO (Either String ())
replaceJsonFile file doc = replaceFileBytes file (encodeJsonPretty doc)

-- | Read a JSON document from a file, or say why it could not be read: the
-- system could not read the file, or it is not the document expected
readJsonFile :: (FromJSON a) => FilePath -> IO (Either String a)
readJsonFile file = do
  bytes <- readFileBytes file
  return $ bytes >>= first (couldNotRead file) . eitherDecodeStrict'

{- Text files -}

-- | Read a text file as UTF-8, or say why it could not be read
readTextFile :: FilePath -> IO (Either String Text)
readTextFile file = do
  bytes <- systemReason (C8.readFile file)
  return $ first (couldNotRead file) (decoded =<< bytes)
  where
    decoded = either (const $ Left "The file is not UTF-8 text") Right . decodeUtf8'

-- | Write a text file as UTF-8, or say why it could not be written
writeTextFile :: FilePath -> Text -> IO (Either String ())
writeTextFile file content = first (couldNotWrite file) <$> systemReason (C8.writeFile file (encodeUtf8 content))

writeMarshalFile :: (MarshalJSON s a) => s -> FilePath -> a -> IO (Either String ())
writeMarshalFile s file a = writeJsonFile file $ marshalValue s a

readMarshalFile :: (MarshalJSON s a) => s -> FilePath -> IO (Either String a)
readMarshalFile s file = do
  valueE <- readJsonFile file
  return $ valueE >>= first (couldNotRead file) . parseEither (unmarshalValue s)
