{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The files hw reads and writes: the bytes of a transaction file, and
-- JSON documents, written pretty-printed so that a user can read them, at a
-- path that must be free, or in place of a file the user agreed to replace.
-- Every file is written whole next to its path first, and put on the disk,
-- before it takes the path in one step ('placeFile'): a crash leaves no
-- part of a file behind. A file that cannot be read or written is a
-- message naming it, with the reason the system gives, never an exception.
module Haskoin.Wallet.FileIO where

import Control.Exception (bracket, catch, onException, throwIO, try)
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
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Directory (createDirectory)
import System.Posix.IO (OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.Unistd (fileSynchronise)

{- Paths -}

-- | Refuse a path that is taken, before a command does its work. The write
-- checks again, since a file can take the path in the meantime.
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

{- Writing a file -}

-- | Where a file is written
data Placement
  = -- | At a path that must be free. A file that took the path since it
    -- was checked is not written over.
    AtFreePath
  | -- | In place of the file at the path, which the user agreed to
    -- replace
    InPlaceOfFile
  deriving (Eq, Show)

-- | Write a file: the given action fills a new file next to its path,
-- whose contents are put on the disk before it takes the path in one
-- step, and the directory is put on the disk after, so that a crash
-- leaves either the whole file or none of it. A free path is taken first,
-- with an empty file, so that no file written there in the meantime is
-- lost: a crash before the step leaves that empty file. A write that fails
-- is the exception of the system, and leaves nothing behind. The file can
-- be read and written by its owner alone, the mode a new file is made
-- with.
placeFile :: Placement -> FilePath -> (FilePath -> IO ()) -> IO ()
placeFile placement file fill = do
  claimed <- case placement of
    AtFreePath -> True <$ claimPath file
    InPlaceOfFile -> return False
  let unclaim = when claimed $ ignoringFailure (D.removeFile file)
  new <- newFileNextTo file `onException` unclaim
  (fill new >> syncFile new >> D.renameFile new file)
    `onException` (ignoringFailure (D.removeFile new) >> unclaim)
  syncDirectory (takeDirectory file)

-- | Take a path with an empty file, which the file written replaces. The
-- system refuses with "file exists" when the path is taken, of a
-- directory too: hw says "is a directory" of one, as it would of a
-- write into it.
claimPath :: FilePath -> IO ()
claimPath file = create `catch` taken
  where
    create = closeFd =<< openFd file WriteOnly defaultFileFlags {creat = Just 0o600, exclusive = True}
    taken e
      | isAlreadyExistsError e = do
          isDirectory <- D.doesDirectoryExist file
          throwIO $ if isDirectory then e {ioe_description = "is a directory"} else e
      | otherwise = throwIO e

-- | Create the directory, if missing, so that its owner alone can enter
-- it, mode 0700, whatever the umask allows: the data directory of hw,
-- whose database holds the keys of the accounts, every address and the
-- history. An existing directory is left as it is.
privateDirectory :: FilePath -> IO ()
privateDirectory dir = do
  exists <- D.doesDirectoryExist dir
  unless exists $ createDirectory dir 0o700 `catch` alreadyThere

-- | Create an empty file, if missing, that its owner alone can read and
-- write, mode 0600: the database before SQLite opens it, which takes an
-- empty file for a new database and gives its journals the mode of the
-- database. An existing file is left as it is.
privateFile :: FilePath -> IO ()
privateFile file = (closeFd =<< openFd file WriteOnly defaultFileFlags {creat = Just 0o600, exclusive = True}) `catch` alreadyThere

-- | Nothing to do for a path made already, by another hw for instance
alreadyThere :: IOException -> IO ()
alreadyThere e = unless (isAlreadyExistsError e) (throwIO e)

-- | A new, empty file next to the given one, named after it, that no
-- other process can have made or opened
newFileNextTo :: FilePath -> IO FilePath
newFileNextTo file = do
  (new, h) <- openBinaryTempFile (takeDirectory file) (takeFileName file)
  new <$ hClose h

-- | Put the contents of a file on the disk
syncFile :: FilePath -> IO ()
syncFile file = bracket (openFd file ReadOnly defaultFileFlags) closeFd fileSynchronise

-- | Put the entries of a directory on the disk, so that a file just made
-- or moved there is found after a crash. A filesystem that cannot sync a
-- directory, or a directory hw cannot open, is no reason to refuse a
-- write that went through: the failure is ignored, as SQLite ignores it.
syncDirectory :: FilePath -> IO ()
syncDirectory dir = ignoringFailure $ bracket (openFd dir ReadOnly defaultFileFlags) closeFd fileSynchronise

-- | An action whose failure is not reported: the cleanup after a write
-- that failed, which must not hide the reason, and the sync of a directory
ignoringFailure :: IO () -> IO ()
ignoringFailure action = void (try action :: IO (Either IOException ()))

{- Files of bytes -}

-- | Write bytes to a new file, or say why they could not be written
writeFileBytes :: FilePath -> ByteString -> IO (Either String ())
writeFileBytes file bytes = first (couldNotWrite file) <$> systemReason (placeFile AtFreePath file (`C8.writeFile` bytes))

-- | Replace a file with bytes, or say why it could not be replaced: a
-- write that fails leaves the file as it was
replaceFileBytes :: FilePath -> ByteString -> IO (Either String ())
replaceFileBytes file bytes = first (couldNotWrite file) <$> systemReason (placeFile InPlaceOfFile file (`C8.writeFile` bytes))

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

-- | Write a new text file as UTF-8, or say why it could not be written
writeTextFile :: FilePath -> Text -> IO (Either String ())
writeTextFile file content = writeFileBytes file (encodeUtf8 content)

writeMarshalFile :: (MarshalJSON s a) => s -> FilePath -> a -> IO (Either String ())
writeMarshalFile s file a = writeJsonFile file $ marshalValue s a

readMarshalFile :: (MarshalJSON s a) => s -> FilePath -> IO (Either String a)
readMarshalFile s file = do
  valueE <- readJsonFile file
  return $ valueE >>= first (couldNotRead file) . parseEither (unmarshalValue s)
