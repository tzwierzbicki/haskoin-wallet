{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The files hw reads and writes: JSON documents, written pretty-printed
-- so that a user can read them, at a path that must be free.
module Haskoin.Wallet.FileIO where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..))
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Char8 as C8
import Haskoin
import Haskoin.Wallet.Util
import qualified System.Directory as D

{- Paths -}

checkPathFree :: (MonadIO m) => FilePath -> ExceptT String m ()
checkPathFree path = do
  exist <- liftIO $ D.doesPathExist path
  when exist $ throwError $ "Path '" <> path <> "' already exists"

{- JSON files -}

writeJsonFile :: FilePath -> Value -> IO ()
writeJsonFile filePath doc = C8.writeFile filePath $ encodeJsonPrettyLn doc

readJsonFile :: (FromJSON a) => FilePath -> IO (Either String a)
readJsonFile = eitherDecodeFileStrict'

writeMarshalFile :: (MarshalJSON s a) => s -> FilePath -> a -> IO ()
writeMarshalFile s filePath a = writeJsonFile filePath $ marshalValue s a

readMarshalFile :: (MarshalJSON s a) => s -> FilePath -> IO (Either String a)
readMarshalFile s filePath = do
  vE <- readJsonFile filePath
  return $ parseEither (unmarshalValue s) =<< vE
