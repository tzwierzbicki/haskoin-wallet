{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Synchronising an account with the store: address discovery, balances,
-- transactions and coins
module Haskoin.Wallet.Sync where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Default (def)
import Data.List (nub, sort, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

data SyncRes = SyncRes
  { syncResAccount :: !DBAccount,
    syncResBlockHash :: !BlockHash,
    syncResBlockHeight :: !BlockHeight,
    syncResTxUpdates :: !Natural,
    syncResCoinUpdates :: !Natural
  }
  deriving (Eq, Show)

discoverAddrs ::
  (MonadIO m) =>
  Network ->
  Ctx ->
  Config ->
  XPubKey ->
  ExceptT String m (Natural, Natural)
discoverAddrs net ctx cfg pub = do
  let recoveryGap = configRecoveryGap cfg
  e <- go extDeriv 0 (Page recoveryGap 0)
  i <- go intDeriv 0 (Page recoveryGap 0)
  return (fromIntegral e, fromIntegral i)
  where
    go path d page@(Page lim off) = do
      let addrs = addrsDerivPage ctx path page pub
          req = GetAddrsBalance $ fst <$> addrs
      let host = apiHost net cfg
      Store.SerialList bals <- liftExcept $ apiCall ctx host req
      let vBals = filter ((/= 0) . (.txs)) bals
      if null vBals
        then return d
        else do
          let dMax = findMax addrs $ (.address) <$> vBals
          go path (dMax + 1) (Page lim (off + lim))
    -- Find the largest ID amongst the addresses that have a positive balance
    findMax :: [(Address, SoftPath)] -> [Address] -> Int
    findMax addrs balAddrs =
      let fAddrs = filter ((`elem` balAddrs) . fst) addrs
       in fromIntegral $ maximum $ last . pathToList . snd <$> fAddrs

sync ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  Network ->
  DBAccountId ->
  Bool ->
  ExceptT String (DB m) SyncRes
sync ctx cfg net accId full = do
  let host = apiHost net cfg
  -- Check API health
  checkHealth ctx net cfg
  -- Get the new best block before starting the sync
  best <- liftExcept $ apiCall ctx host (GetBlockBest def)
  -- Get the addresses from our local database
  (addrPathMap, addrBalMap) <- allAddressesMap net accId
  -- Fetch the address balances online
  Store.SerialList storeBals <-
    liftExcept . apiBatch ctx (configAddrBatch cfg) host $
      GetAddrsBalance (Map.keys addrBalMap)
  -- Filter only those addresses whose balances have changed
  balsToUpdate <-
    if full
      then return storeBals
      else liftEither $ filterAddresses storeBals addrBalMap
  let addrsToUpdate = (.address) <$> balsToUpdate
  -- Update balances
  updateAddressBalances net balsToUpdate
  newAcc <- lift $ updateAccountBalances accId
  -- Get a list of our confirmed txs in the local database
  -- Use an empty list when doing a full sync
  confirmedTxs <- if full then return [] else getConfirmedTxs accId True
  -- Fetch the txids of the addresses to update
  aTids <- searchAddrTxs net ctx cfg confirmedTxs addrsToUpdate
  -- We also want to check if there is any change in unconfirmed txs
  uTids <- getConfirmedTxs accId False
  let tids = nub $ uTids <> aTids
  -- Fetch the full transactions
  Store.SerialList txs <-
    liftExcept $ apiBatch ctx (configTxFullBatch cfg) host (GetTxs tids)
  -- Convert them to TxInfo and store them in the local database
  let txInfos = storeToTxInfo addrPathMap (fromIntegral best.height) <$> txs
  resTxInfo <- forM txInfos $ repsertTxInfo net ctx accId
  -- Fetch and update coins
  Store.SerialList storeCoins <-
    liftExcept . apiBatch ctx (configCoinBatch cfg) host $
      GetAddrsUnspent addrsToUpdate def
  (coinCount, newCoins) <- refreshCoins net accId addrsToUpdate storeCoins
  -- Get the dependent tranactions of the new coins
  depTxsHash <-
    if full
      then return $ (.outpoint.hash) <$> storeCoins
      else mapM (liftEither . coinToTxHash) newCoins
  Store.RawResultList rawTxs <-
    liftExcept
      . apiBatch ctx (configTxFullBatch cfg) host
      $ GetTxsRaw
      $ nub depTxsHash
  lift $ forM_ rawTxs insertRawTx
  -- Remove pending transactions if they are online
  pendingTids <- pendingTxHashes accId
  let toRemove = filter ((`elem` tids) . fst) pendingTids
  forM_ toRemove $ \(_, key) -> lift $ deletePendingTxOnline key
  -- Update the best block for this network
  lift $ updateBest net (headerHash best.header) best.height
  return $
    SyncRes
      newAcc
      (headerHash best.header)
      (fromIntegral best.height)
      (fromIntegral $ length $ filter id $ snd <$> resTxInfo)
      (fromIntegral coinCount)

-- Filter addresses that need to be updated
filterAddresses ::
  [Store.Balance] ->
  Map Address AddressBalance ->
  Either String [Store.Balance]
filterAddresses sBals aMap
  | sort ((.address) <$> sBals) /= sort (Map.keys aMap) =
      Left "Sync: addresses do not match"
  | otherwise =
      Right $ filter f sBals
  where
    f s =
      let b = fromJust $ s.address `Map.lookup` aMap
       in s.txs /= addrBalanceTxs b
            || s.confirmed /= addrBalanceConfirmed b
            || s.unconfirmed /= addrBalanceUnconfirmed b
            || s.utxo /= addrBalanceCoins b

searchAddrTxs ::
  (MonadIO m) =>
  Network ->
  Ctx ->
  Config ->
  [TxHash] ->
  [Address] ->
  ExceptT String m [TxHash]
searchAddrTxs _ _ _ _ [] = return []
searchAddrTxs net ctx cfg confirmedTxs as
  | length as > fromIntegral (configAddrBatch cfg) =
      nub . concat <$> mapM (go Nothing 0) (chunksOf (configAddrBatch cfg) as)
  | otherwise =
      nub <$> go Nothing 0 as
  where
    go hashM offset' xs = do
      Store.SerialList txRefs <-
        liftExcept $
          apiCall
            ctx
            (apiHost net cfg)
            ( GetAddrsTxs
                xs
                def
                  { limit = Just $ fromIntegral (configTxBatch cfg),
                    start = StartParamHash <$> hashM,
                    offset = offset'
                  }
            )
      -- Remove txs that we already have
      let tids = ((.txid) <$> txRefs) \\ confirmedTxs
      -- Either we have reached the end of the stream, or we have hit some
      -- txs in confirmedTxs. In both cases, we can stop the search.
      if length tids < fromIntegral (configTxBatch cfg)
        then return tids
        else do
          let lastId = (last tids).get
          rest <- go (Just lastId) 1 xs
          return $ tids <> rest

coinToTxHash :: DBCoin -> Either String TxHash
coinToTxHash coin =
  maybeToEither "coinToTxHash: Invalid outpoint" $ do
    bs <- decodeHex $ dBCoinOutpoint coin
    op <- eitherToMaybe (S.decode bs) :: Maybe OutPoint
    return op.hash
