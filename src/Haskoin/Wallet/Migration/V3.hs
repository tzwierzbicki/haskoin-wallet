{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Migration.V3 where

import Conduit (MonadUnliftIO)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Database.Persist.Sql (rawExecute)
import Haskoin.Wallet.Database

{- The step to the format version 3 from version 2 (hw 0.9): the version
   table holds the number of the format version instead of a text -}

-- | hw 0.9 stored its version as a text, so the version table is dropped and
-- created again with a number column; the caller stamps the version
migrateDB :: (MonadUnliftIO m) => ExceptT String (DB m) ()
migrateDB = lift $ do
  rawExecute "drop table d_b_version" []
  globalMigration
