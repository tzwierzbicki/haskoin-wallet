{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The database format of version 5, and the step that brings a database
-- of version 4 to it: the coins lost their locked column. Whether a coin is
-- locked, spent by a pending transaction, is now read from the pending
-- transactions themselves, which the column could only disagree with.
--
-- The step works on the tables of "Haskoin.Wallet.Schema.V5": persistent
-- rebuilds the coin table without the column and keeps every row.
module Haskoin.Wallet.Migration.V5 where

import Conduit (MonadUnliftIO)
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import qualified Database.Persist as P
import Database.Persist.Sql (runMigrationUnsafeQuiet)
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Schema.V5

-- | Bring a database of version 4 to version 5: drop the locked column of
-- the coins, and store the version 5. Dropping a column has persistent copy
-- the coins to a new table, which it only does when told the migration is
-- meant to be unsafe; the database was backed up before this step.
migrateDB :: (MonadUnliftIO m) => ExceptT String (DB m) ()
migrateDB = lift $ do
  void $ runMigrationUnsafeQuiet migrateAll
  P.deleteWhere ([] :: [P.Filter DBVersion])
  P.insert_ $ DBVersion 5
