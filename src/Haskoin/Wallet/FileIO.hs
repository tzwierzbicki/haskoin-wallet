{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The files hw reads and writes: JSON documents, written pretty-printed
-- so that a user can read them, at a path that must be free. A file that
-- cannot be read or written is a message naming it, with the reason the
-- system gives, never an exception.
module Haskoin.Wallet.FileIO where

import Control.Exception (try)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..))
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import GHC.IO.Exception (IOException (..))
import Haskoin
import Haskoin.Wallet.Util
import qualified System.Directory as D

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

{- JSON files -}

-- | Write a JSON document to a file, or say why it could not be written
writeJsonFile :: FilePath -> Value -> IO (Either String ())
writeJsonFile file doc = first (couldNotWrite file) <$> systemReason (C8.writeFile file (encodeJsonPretty doc))

-- | Read a JSON document from a file, or say why it could not be read: the
-- system could not read the file, or it is not the document expected
readJsonFile :: (FromJSON a) => FilePath -> IO (Either String a)
readJsonFile file = do
  bytes <- systemReason (C8.readFile file)
  return $ first (couldNotRead file) (eitherDecodeStrict' =<< bytes)

writeMarshalFile :: (MarshalJSON s a) => s -> FilePath -> a -> IO (Either String ())
writeMarshalFile s file a = writeJsonFile file $ marshalValue s a

readMarshalFile :: (MarshalJSON s a) => s -> FilePath -> IO (Either String a)
readMarshalFile s file = do
  valueE <- readJsonFile file
  return $ valueE >>= first (couldNotRead file) . parseEither (unmarshalValue s)
