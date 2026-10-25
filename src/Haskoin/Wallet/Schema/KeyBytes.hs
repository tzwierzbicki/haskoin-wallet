{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bytes as the keys of the database store them, from format version 7
-- on: a blob in SQLite. Persistent derives text forms for every
-- single-column key, for web routes and JSON, and plain bytes have none;
-- this type gives them hexadecimal ones, which hw itself never uses.
--
-- The type is part of the format of version 7 and of the versions after
-- it: its database form never changes. A format that stores keys
-- differently gets a new type.
module Haskoin.Wallet.Schema.KeyBytes where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.ByteString (ByteString)
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Haskoin.Util (decodeHex, encodeHex)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Web.PathPieces (PathPiece (..))

newtype KeyBytes = KeyBytes {keyBytes :: ByteString}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (PersistField, PersistFieldSql)

instance PathPiece KeyBytes where
  toPathPiece = encodeHex . keyBytes
  fromPathPiece = fmap KeyBytes . decodeHex

instance ToHttpApiData KeyBytes where
  toUrlPiece = encodeHex . keyBytes

instance FromHttpApiData KeyBytes where
  parseUrlPiece = maybe (Left "The key is not hexadecimal") (Right . KeyBytes) . decodeHex

instance ToJSON KeyBytes where
  toJSON = toJSON . encodeHex . keyBytes

instance FromJSON KeyBytes where
  parseJSON = withText "KeyBytes" $ maybe (fail "The key is not hexadecimal") (return . KeyBytes) . decodeHex
