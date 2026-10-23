{-# LANGUAGE OverloadedRecordDot #-}

-- | The pending transactions of an account, from preparation to the
-- network: stored with their signing data, holding the coins they spend,
-- signed in place, sent and watched until the sync retires them. The
-- commands of hw are thin layers over the functions here.
module Haskoin.Wallet.PendingTx where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.Database
import Haskoin.Wallet.RawTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (parseTxSignData, verifySignedTx)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

{- What a transaction touches -}

-- | What a transaction of the wallet touches, read from its summary
data Touched = Touched
  { -- | The coins it spends
    coinsSpent :: ![OutPoint],
    -- | Its internal addresses, where its change goes
    changeAddresses :: ![Address],
    -- | Its other addresses of the wallet, paid or spent from
    otherAddresses :: ![Address]
  }

touchedBy :: TxInfo -> Touched
touchedBy txInfo =
  Touched
    { coinsSpent = (.outpoint) <$> concatMap myInputsSigInput (Map.elems txInfo.txInfoMyInputs),
      changeAddresses = Map.keys change,
      otherAddresses = nub $ Map.keys paid <> Map.keys txInfo.txInfoMyInputs
    }
  where
    (change, paid) = Map.partition (isIntPath . myOutputsPath) txInfo.txInfoMyOutputs

{- Pending transactions in the database -}

-- | A pending transaction as the database holds it
data PendingTx = PendingTx
  { pendingTxKey :: !DBPendingTxId,
    pendingTxAccount :: !DBAccountId,
    pendingTxData :: !TxSignData,
    -- | Sent to the network by the user
    pendingTxOnline :: !Bool
  }

-- | A row read back. A row whose signing data does not decode is an error,
-- never a transaction that is not there.
fromRow :: Entity DBPendingTx -> Either String PendingTx
fromRow (Entity key row) = do
  tsd <- Json.eitherDecode $ BS.fromStrict $ dBPendingTxBlob row
  return
    PendingTx
      { pendingTxKey = key,
        pendingTxAccount = DBAccountKey (dBPendingTxAccountWallet row) (dBPendingTxAccountDerivation row),
        pendingTxData = tsd,
        pendingTxOnline = dBPendingTxOnline row
      }

-- | The signing data as a row stores it
blob :: TxSignData -> BS.ByteString
blob = BS.toStrict . Json.encode

-- | The answer to a hash that is not among the pending transactions
noPendingTx :: String
noPendingTx = "The pending transaction does not exist"

-- | The pending transaction with the given nosigHash, if there is one
lookupPendingTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) (Maybe PendingTx)
lookupPendingTx pendingHash = do
  rowM <- lift $ P.getEntity $ DBPendingTxKey $ txHashToHex pendingHash
  liftEither $ mapM fromRow rowM

-- | The pending transaction with the given nosigHash, which must exist
requirePendingTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) PendingTx
requirePendingTx pendingHash = lookupPendingTx pendingHash >>= liftMaybe noPendingTx

-- | The rows of an account, newest first: all of them, or a page
pendingRows :: (MonadUnliftIO m) => DBAccountId -> Maybe Page -> DB m [Entity DBPendingTx]
pendingRows (DBAccountKey wallet accDeriv) pageM =
  select . from $ \p -> do
    where_ $
      p ^. DBPendingTxAccountWallet ==. val wallet
        &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
    orderBy [desc (p ^. DBPendingTxCreated)]
    forM_ pageM $ \(Page lim off) -> do
      limit $ fromIntegral lim
      offset $ fromIntegral off
    return p

-- | The pending transactions of an account, newest first
pendingTxs :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) [PendingTx]
pendingTxs accId = liftEither . mapM fromRow =<< lift (pendingRows accId Nothing)

-- | A page of the pending transactions of an account, as summaries
pendingTxPage :: (MonadUnliftIO m) => Ctx -> DBAccountId -> Page -> ExceptT String (DB m) [TxInfo]
pendingTxPage ctx accId page = do
  acc <- getAccountById accId
  held <- liftEither . mapM fromRow =<< lift (pendingRows accId (Just page))
  mapM (pendingSummary ctx acc) held

-- | The summary of a pending transaction: the transaction as the wallet
-- sees it, its pending state and the labels of its addresses
pendingSummary :: (MonadUnliftIO m) => Ctx -> DBAccount -> PendingTx -> ExceptT String (DB m) TxInfo
pendingSummary ctx acc p = do
  let net = accountNetwork acc
      tx = txSignDataTx p.pendingTxData
  txInfo <- liftEither $ parseTxSignData net ctx (accountXPubKey ctx acc) p.pendingTxData
  conflicted <- lift $ conflictedInputs txInfo
  let pending = TxInfoPending (nosigHash tx) (isSigned tx) p.pendingTxOnline (not (null conflicted))
  lift $ fillTxInfoLabels net txInfo {txInfoPending = Just pending}

-- | The coins locked by the pending transactions of an account: every
-- outpoint one of them spends. A coin is locked exactly as long as a
-- pending transaction spends it; nothing else records it.
lockedOutpoints :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) (Set OutPoint)
lockedOutpoints accId = do
  held <- pendingTxs accId
  return $ Set.fromList [input.outpoint | p <- held, input <- (txSignDataTx p.pendingTxData).inputs]

-- | Whether the wallet holds the coin. The coin table is read here rather
-- than through "Haskoin.Wallet.Coin", which needs this module for the locks.
coinExists :: (MonadUnliftIO m) => OutPoint -> DB m Bool
coinExists outpoint = isJust <$> P.get (DBCoinKey $ outpointText outpoint)

-- | The coins of the wallet that a transaction spends and that are gone,
-- spent by another transaction: the transaction can no longer be sent
conflictedInputs :: (MonadUnliftIO m) => TxInfo -> DB m [OutPoint]
conflictedInputs txInfo = filterM (fmap not . coinExists) (touchedBy txInfo).coinsSpent

-- | Whether a pending transaction spends a coin of the wallet that is gone
inConflict :: (MonadUnliftIO m) => Ctx -> DBAccount -> PendingTx -> ExceptT String (DB m) Bool
inConflict ctx acc p = do
  txInfo <- liftEither $ parseTxSignData (accountNetwork acc) ctx (accountXPubKey ctx acc) p.pendingTxData
  not . null <$> lift (conflictedInputs txInfo)

{- Importing -}

-- | Store a transaction of the account as pending, which locks the coins it
-- spends and marks its change address busy. A signed transaction must
-- carry valid signatures: they are checked here, so that sending has
-- nothing left to check. Importing a transaction the account already
-- holds replaces it with the newer version, signed where it was unsigned;
-- the transaction itself, its inputs and outputs, cannot change, since the
-- hash identifying it leaves the signatures out.
importPendingTx :: (MonadUnliftIO m) => Network -> Ctx -> DBAccountId -> TxSignData -> ExceptT String (DB m) TxHash
importPendingTx net ctx accId tsd = do
  acc <- getAccountById accId
  heldM <- lookupPendingTx pendingHash
  forM_ heldM $ \held ->
    when (held.pendingTxAccount /= accId) $
      throwError "The transaction is pending in another account"
  txInfo <- liftEither $ parseTxSignData net ctx (accountXPubKey ctx acc) tsd
  when (isSigned tx) $ liftEither $ verifySignedTx net ctx tsd
  case heldM of
    Just held -> replaceVersion held tsd
    Nothing -> insertPendingTx net accId tsd txInfo
  return pendingHash
  where
    tx = txSignDataTx tsd
    pendingHash = nosigHash tx

-- | Replace the version an account holds of a transaction with the one
-- imported: the signed version of an unsigned transaction, or another
-- valid signature of a signed one
replaceVersion :: (MonadUnliftIO m) => PendingTx -> TxSignData -> ExceptT String (DB m) ()
replaceVersion held tsd = do
  when held.pendingTxOnline $
    throwError "The transaction is already online"
  when (isSigned heldTx && not (isSigned tx)) $
    throwError "Can not replace a signed transaction with an unsigned one"
  when (heldTx == tx) $
    throwError "The transaction already exists"
  lift $ P.update held.pendingTxKey [DBPendingTxBlob P.=. blob tsd]
  where
    heldTx = txSignDataTx held.pendingTxData
    tx = txSignDataTx tsd

-- | Store a transaction the account does not hold yet
insertPendingTx :: (MonadUnliftIO m) => Network -> DBAccountId -> TxSignData -> TxInfo -> ExceptT String (DB m) ()
insertPendingTx net accId@(DBAccountKey wallet accDeriv) tsd txInfo = do
  -- A pending transaction is retired when the sync sees it under one of
  -- the wallet's addresses; one touching none would wait forever
  unless (involvesWallet txInfo) $
    throwError "The transaction does not involve this wallet"
  let touched = touchedBy txInfo
  -- The coins it spends must exist and be free of any other pending
  -- transaction; storing it is what locks them
  locked <- lockedOutpoints accId
  forM_ touched.coinsSpent $ \outpoint -> do
    inWallet <- lift $ coinExists outpoint
    unless inWallet $ throwError "A coin referenced by the transaction does not exist"
    when (outpoint `Set.member` locked) $ throwError "A coin referenced by the transaction is locked"
  -- Its addresses must be generated already, and its change addresses free
  let addrs = nub $ touched.changeAddresses <> touched.otherAddresses
  known <- walletPaths net accId addrs
  when (Map.size known /= length addrs) $
    throwError "Some referenced addresses do not exist"
  changeAddrsT <- liftEither $ mapM (addrToTextE net) touched.changeAddresses
  busy <- lift $ busyAddresses changeAddrsT
  unless (null busy) $
    throwError "Some of the internal output addresses are not free"
  _ <- lift $ setAddrsFree AddrBusy changeAddrsT
  time <- liftIO getCurrentTime
  lift $ P.insert_ $ DBPendingTx wallet accDeriv (txHashToHex (nosigHash (txSignDataTx tsd))) (blob tsd) False time

{- Deleting and retiring -}

-- | Remove a pending transaction, whatever its state: the coins it spent are
-- free again by its absence, and its change addresses are freed, unless the
-- chain has used them. Returns how many coins of the wallet it was holding
-- and how many addresses were freed.
removePendingTx :: (MonadUnliftIO m) => Ctx -> PendingTx -> ExceptT String (DB m) (Natural, Natural)
removePendingTx ctx held = do
  acc <- getAccountById held.pendingTxAccount
  let net = accountNetwork acc
  txInfo <- liftEither $ parseTxSignData net ctx (accountXPubKey ctx acc) held.pendingTxData
  let touched = touchedBy txInfo
  heldCoins <- lift $ filterM coinExists touched.coinsSpent
  changeAddrsT <- liftEither $ mapM (addrToTextE net) touched.changeAddresses
  freed <- lift $ setAddrsFree AddrFree changeAddrsT
  lift $ P.delete held.pendingTxKey
  return (fromIntegral (length heldCoins), fromIntegral freed)

-- | Retire a pending transaction the network has seen, as the sync does:
-- the row goes and nothing else changes. Its coins are gone, or going with
-- the sync, and its change address was used on the chain, so it stays busy.
retirePendingTx :: (MonadUnliftIO m) => PendingTx -> DB m ()
retirePendingTx p = P.delete p.pendingTxKey

-- | Mark a pending transaction as sent to the network by the user. The
-- number of rows changed: one, or none for a hash that is not pending.
setPendingTxOnline :: (MonadUnliftIO m) => TxHash -> DB m Natural
setPendingTxOnline nosigH =
  fmap fromIntegral . updateCount $ \p -> do
    set p [DBPendingTxOnline =. val True]
    where_ $ p ^. DBPendingTxNosigHash ==. val (txHashToHex nosigH)

{- Sending and abandoning -}

-- | Send a pending transaction of the wallet and wait for the store to see
-- it. Once it does, the transaction is marked online, for the sync to send
-- again should the store lose it, and its raw transaction is stored. If the
-- store does not see it, nothing is recorded and the answer says so. The
-- signatures were checked when the transaction was imported.
sendPending :: (MonadUnliftIO m) => Ctx -> (Network -> Store) -> TxHash -> ExceptT String (DB m) (DBAccount, TxInfo)
sendPending ctx storeOn nosigH = do
  held <- requirePendingTx nosigH
  acc <- getAccountById held.pendingTxAccount
  let signedTx = txSignDataTx held.pendingTxData
      store = storeOn (accountNetwork acc)
  unless (isSigned signedTx) $ throwError "The transaction is not signed"
  -- The summary as it will read once sent, made before anything leaves
  summary <- pendingSummary ctx acc held {pendingTxOnline = True}
  fromStore $ storeHealth store
  accepted <- fromStore $ broadcast store signedTx
  unless accepted $ throwError notAccepted
  _ <- lift $ setPendingTxOnline nosigH
  lift $ insertRawTx signedTx
  return (acc, summary)

-- | Delete a pending transaction, freeing the coins it held. One the user
-- sent is abandoned only when the store does not know it: a transaction in
-- the network's mempool or in the chain may still confirm, and the sync
-- retires it once it is listed.
deleteTx :: (MonadUnliftIO m) => Ctx -> (Network -> Store) -> TxHash -> ExceptT String (DB m) (Natural, Natural)
deleteTx ctx storeOn nosigH = do
  held <- requirePendingTx nosigH
  let txid = txHash (txSignDataTx held.pendingTxData)
  when held.pendingTxOnline $ do
    acc <- getAccountById held.pendingTxAccount
    known <- fromStore $ storeTransactions (storeOn (accountNetwork acc)) [txid]
    when (any (\t -> t.txid == txid && not t.deleted) known) $
      throwError "The transaction is in the network's mempool or in the chain: syncacc retires it once it is listed"
  freed <- removePendingTx ctx held
  -- The raw transaction stored when it was sent goes with it, unless a
  -- coin of the wallet comes from it after all
  when held.pendingTxOnline $ void $ lift $ dropRawTxsWithoutCoins [txid]
  return freed
