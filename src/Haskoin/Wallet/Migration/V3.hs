{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The database format of version 3, written by hw 0.10, and the step that
-- brings a database of version 2 (hw 0.9) to it: the version table holds a
-- number instead of a text.
--
-- The step creates the version table of "Haskoin.Wallet.Schema.V3". The
-- summaries of version 3 are those of version 2 with optional fields added,
-- which the live code reads as they are, so the step leaves the blobs alone
-- and needs no summary types.
module Haskoin.Wallet.Migration.V3 where

import Conduit (MonadUnliftIO)
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import qualified Database.Persist as P
import Database.Persist.Sql (rawExecute, runMigrationQuiet)
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Schema.V3

{- The step -}

-- | Bring a database of version 2 to version 3: hw 0.9 stored its version as
-- a text, so the version table is dropped, created again with a number
-- column, and filled with the number 3
migrateDB :: (MonadUnliftIO m) => ExceptT String (DB m) ()
migrateDB = lift $ do
  rawExecute "drop table d_b_version" []
  void $ runMigrationQuiet migrateAll
  P.insert_ $ DBVersion 3
