-- | What the shell completes from the wallet: the names of the accounts,
-- and the hashes of the pending transactions at each step from preparetx
-- to sendtx. The shell runs hw at each Tab, so a completion reads the
-- database and changes nothing. It writes no configuration file and
-- creates no database. It migrates nothing, writes nothing, and does not
-- wait for another command. When it cannot read the database, the shell
-- offers nothing for the word.
module Haskoin.Wallet.Completion
  ( accountNames,
    hashesToSign,
    idsToSend,
    pendingHashes,
    readForCompletion,
  )
where

import Control.Monad.Except (runExceptT)
import Data.Either (rights)
import Data.String.Conversions (cs)
import Haskoin (Tx, txHashToHex)
import Haskoin.Wallet.Account (getAccountNames)
import Haskoin.Wallet.Config (configAsItIs, databaseFileIn)
import Haskoin.Wallet.Database (DB, runDBReadOnly)
import Haskoin.Wallet.Migration (databaseVersion, latestDatabaseVersion)
import Haskoin.Wallet.PendingTx (PendingTx (..), fromRow, pendingRows)
import Haskoin.Wallet.TxSignData (TxSignData (..), isSigned, shownHash)
import qualified System.Directory as D
import UnliftIO.Exception (handleAny)

-- | The names of the accounts, for --account and the arguments that name
-- an account
accountNames :: IO [String]
accountNames = readForCompletion (map cs <$> getAccountNames)

{- The pending transactions -}

-- | The nosigHashes of the pending transactions to sign, for signtx and
-- signsweep: those neither signed nor sent, as the two refuse the others
hashesToSign :: IO [String]
hashesToSign = shownHashesOf (\p -> not (isSigned (txOf p)) && not (pendingTxOnline p))

-- | The ids of the pending transactions to send, for sendtx: those signed
-- and not sent yet. The sync sends one again should the network lose it.
idsToSend :: IO [String]
idsToSend = shownHashesOf (\p -> isSigned (txOf p) && not (pendingTxOnline p))

-- | The hash of every pending transaction, for exporttx and deletetx
pendingHashes :: IO [String]
pendingHashes = shownHashesOf (const True)

-- | The pending transactions that pass the test, by the hash pendingtxs
-- shows: the nosigHash until a transaction is signed, then its id. A row
-- that does not read is left out.
shownHashesOf :: (PendingTx -> Bool) -> IO [String]
shownHashesOf keep = readForCompletion $ do
  held <- rights . map fromRow <$> pendingRows Nothing Nothing Nothing
  return [cs (txHashToHex (shownHash (txOf p))) | p <- held, keep p]

-- | The transaction a pending transaction holds
txOf :: PendingTx -> Tx
txOf = txSignDataTx . pendingTxData

-- | Run a read of the wallet database for the shell, or give nothing. The
-- database is the one hw works on: ~/.hw/accounts.sqlite, or the file
-- the configuration names. It gives nothing when there is no database,
-- when the database is of another format version than this hw's, and on
-- any failure.
readForCompletion :: DB IO [a] -> IO [a]
readForCompletion action = handleAny (const (return [])) $ do
  -- The directory of hw, without creating it
  dir <- D.getAppUserDataDirectory "hw"
  configM <- configAsItIs dir
  case configM of
    Nothing -> return []
    Just cfg -> do
      let file = databaseFileIn dir cfg
      exists <- D.doesFileExist file
      if exists then runDBReadOnly file readCurrent else return []
  where
    -- A database of an older version is migrated by the next command, and
    -- one of a newer version is refused. Their tables may differ.
    readCurrent = do
      version <- runExceptT databaseVersion
      if version == Right (Just latestDatabaseVersion) then action else return []
