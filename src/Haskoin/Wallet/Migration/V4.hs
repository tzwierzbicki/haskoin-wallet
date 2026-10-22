{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The database format of version 4, and the step that brings a database
-- of version 3 to it: the transaction summaries gained a column saying
-- whether the transaction is dead, replaced or dropped from the network,
-- where the sync used to delete the summary.
--
-- The step works on the tables of "Haskoin.Wallet.Schema.V4". The summaries
-- themselves are unchanged, so no summary types are needed here.
module Haskoin.Wallet.Migration.V4 where

import Conduit (MonadUnliftIO)
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import qualified Database.Persist as P
import Database.Persist.Sql (runMigrationQuiet)
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Schema.V4

-- | Bring a database of version 3 to version 4: add the dead column to the
-- transaction summaries, false for every existing one, and store the
-- version 4
migrateDB :: (MonadUnliftIO m) => ExceptT String (DB m) ()
migrateDB = lift $ do
  void $ runMigrationQuiet migrateAll
  P.deleteWhere ([] :: [P.Filter DBVersion])
  P.insert_ $ DBVersion 4
