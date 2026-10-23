{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The database format of version 6, and the step that brings a database
-- of version 5 to it: the addresses lost their free column. Whether an
-- internal address is available for change, with no transaction on the
-- chain and no pending transaction paying it, is now read from its
-- transactions and from the pending transactions, which the column could
-- only disagree with.
--
-- The step works on the tables of "Haskoin.Wallet.Schema.V6": persistent
-- rebuilds the address table without the column and keeps every row.
module Haskoin.Wallet.Migration.V6 where

import Conduit (MonadUnliftIO)
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import qualified Database.Persist as P
import Database.Persist.Sql (runMigrationUnsafeQuiet)
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Schema.V6

-- | Bring a database of version 5 to version 6: drop the free column of the
-- addresses, and store the version 6. Dropping a column has persistent copy
-- the addresses to a new table, which it only does when told the migration
-- is meant to be unsafe; the database was backed up before this step.
migrateDB :: (MonadUnliftIO m) => ExceptT String (DB m) ()
migrateDB = lift $ do
  void $ runMigrationUnsafeQuiet migrateAll
  P.deleteWhere ([] :: [P.Filter DBVersion])
  P.insert_ $ DBVersion 6
