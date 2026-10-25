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
import Haskoin.Wallet.Config (Config)
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
    -- | Its internal addresses, where its change goes, and their paths
    changeAddresses :: ![Address],
    changePaths :: ![SoftPath],
    -- | Its other addresses of the wallet, paid or spent from
    otherAddresses :: ![Address]
  }

touchedBy :: TxSummary -> Touched
touchedBy summary =
  Touched
    { coinsSpent = concatMap myInputsOutpoints (Map.elems summary.summaryMyInputs),
      changeAddresses = Map.keys change,
      changePaths = myOutputsPath <$> Map.elems change,
      otherAddresses = nub $ Map.keys paid <> Map.keys summary.summaryMyInputs
    }
  where
    (change, paid) = Map.partition (isIntPath . myOutputsPath) summary.summaryMyOutputs

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
        pendingTxAccount = dBPendingTxAccount row,
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
  rowM <- lift $ P.getEntity $ DBPendingTxKey $ hashBytes pendingHash
  liftEither $ mapM fromRow rowM

-- | The pending transaction with the given nosigHash, which must exist
requirePendingTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) PendingTx
requirePendingTx pendingHash = lookupPendingTx pendingHash >>= liftMaybe noPendingTx

-- | The rows of an account, newest first: all of them, or a page
pendingRows :: (MonadUnliftIO m) => DBAccountId -> Maybe Page -> DB m [Entity DBPendingTx]
pendingRows accId pageM =
  select . from $ \p -> do
    where_ $
      p ^. DBPendingTxAccount ==. val accId
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

-- | The transaction of the signing data as the account sees it: parsed
-- against the account's key, with the labels of its addresses
txInfoOf :: (MonadUnliftIO m) => Ctx -> DBAccount -> TxSignData -> ExceptT String (DB m) TxInfo
txInfoOf ctx acc tsd = do
  info <- liftEither $ parseTxSignData (accountNetwork acc) ctx (accountXPubKey ctx acc) tsd
  lift $ fillTxInfoLabels (accountNetwork acc) info

-- | The summary of a pending transaction: the transaction as the wallet
-- sees it, with its status as held
pendingSummary :: (MonadUnliftIO m) => Ctx -> DBAccount -> PendingTx -> ExceptT String (DB m) TxInfo
pendingSummary ctx acc p = do
  info <- txInfoOf ctx acc p.pendingTxData
  conflicted <- lift $ conflictedInputs info.txInfoSummary
  return info {txInfoStatus = heldStatus p (not (null conflicted)) info.txInfoStatus}

-- | The status of a held transaction: what its signing data says, and what
-- only the database knows, whether the user sent it and whether a coin it
-- spends is gone. The signing data describes a transaction in preparation;
-- one on the chain is not held and is left as it is.
heldStatus :: PendingTx -> Bool -> TxStatus -> TxStatus
heldStatus p conflicted (Pending status) =
  Pending status {pendingOnline = p.pendingTxOnline, pendingConflicted = conflicted}
heldStatus _ _ onChain = onChain

-- | The coins locked by the pending transactions of an account: every
-- outpoint one of them spends. A coin is locked exactly as long as a
-- pending transaction spends it; nothing else records it.
lockedOutpoints :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) (Set OutPoint)
lockedOutpoints accId = do
  held <- pendingTxs accId
  return $ Set.fromList [input.outpoint | p <- held, input <- (txSignDataTx p.pendingTxData).inputs]

-- | The change addresses the pending transactions of an account pay, as
-- derivation paths: held as long as a transaction is pending, like the
-- coins it spends
reservedChangePaths :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) [SoftPath]
reservedChangePaths accId = do
  held <- pendingTxs accId
  return $ nub [path | p <- held, path <- p.pendingTxData.txSignDataOutputPaths, isIntPath path]

-- | The change address for a new transaction of the account: the lowest
-- internal address that no transaction has used and no pending transaction
-- pays, or the next one of the chain
changeAddress :: (MonadUnliftIO m) => Ctx -> Config -> DBAccountId -> ExceptT String (DB m) DBAddress
changeAddress ctx cfg accId = nextChangeAddress ctx cfg accId =<< reservedChangePaths accId

-- | Whether the wallet holds the coin. The coin table is read here rather
-- than through "Haskoin.Wallet.Coin", which needs this module for the locks.
coinExists :: (MonadUnliftIO m) => OutPoint -> DB m Bool
coinExists outpoint = isJust <$> P.get (DBCoinKey $ outpointBytes outpoint)

-- | The coins of the wallet that a transaction spends and that are gone,
-- spent by another transaction: the transaction can no longer be sent
conflictedInputs :: (MonadUnliftIO m) => TxSummary -> DB m [OutPoint]
conflictedInputs summary = filterM (fmap not . coinExists) (touchedBy summary).coinsSpent

-- | Whether a pending transaction spends a coin of the wallet that is gone
inConflict :: (MonadUnliftIO m) => Ctx -> DBAccount -> PendingTx -> ExceptT String (DB m) Bool
inConflict ctx acc p = do
  info <- liftEither $ parseTxSignData (accountNetwork acc) ctx (accountXPubKey ctx acc) p.pendingTxData
  not . null <$> lift (conflictedInputs info.txInfoSummary)

{- Importing -}

-- | Store a transaction of the account as pending, which locks the coins it
-- spends and holds its change address. A signed transaction must
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
  info <- liftEither $ parseTxSignData (accountNetwork acc) ctx (accountXPubKey ctx acc) tsd
  when (isSigned tx) $ do
    liftEither $ verifySignedTx net ctx tsd
    -- The signer names the seed of the account, which this wallet may not know
    forM_ (txSignDataFingerprint tsd) $ learnFingerprint accId
  case heldM of
    Just held -> replaceVersion held tsd
    Nothing -> insertPendingTx net accId tsd info.txInfoSummary
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
insertPendingTx :: (MonadUnliftIO m) => Network -> DBAccountId -> TxSignData -> TxSummary -> ExceptT String (DB m) ()
insertPendingTx net accId tsd summary = do
  -- A pending transaction is retired when the sync sees it under one of
  -- the wallet's addresses; one touching none would wait forever
  unless (involvesWallet summary) $
    throwError "The transaction does not involve this wallet"
  let touched = touchedBy summary
  -- The coins it spends must exist and be free of any other pending
  -- transaction; storing it is what locks them
  locked <- lockedOutpoints accId
  forM_ touched.coinsSpent $ \outpoint -> do
    inWallet <- lift $ coinExists outpoint
    unless inWallet $ throwError "A coin referenced by the transaction does not exist"
    when (outpoint `Set.member` locked) $ throwError "A coin referenced by the transaction is locked"
  -- Its addresses must be generated already, and its change addresses
  -- neither used nor paid by another pending transaction
  let addrs = nub $ touched.changeAddresses <> touched.otherAddresses
  known <- walletPaths net accId addrs
  when (Map.size known /= length addrs) $
    throwError "Some referenced addresses do not exist"
  used <- lift . usedAddresses =<< liftEither (mapM (addrToTextE net) touched.changeAddresses)
  reserved <- reservedChangePaths accId
  unless (null used && all (`notElem` reserved) touched.changePaths) $
    throwError "Some of the internal output addresses are not free"
  time <- liftIO getCurrentTime
  lift $ P.insert_ $ DBPendingTx accId (hashBytes (nosigHash (txSignDataTx tsd))) (blob tsd) False time

{- Deleting and retiring -}

-- | Remove a pending transaction, whatever its state: the coins it spent and
-- the change addresses it paid are available again by its absence. Returns
-- how many coins of the wallet it was holding and how many change
-- addresses it paid.
removePendingTx :: (MonadUnliftIO m) => Ctx -> PendingTx -> ExceptT String (DB m) (Natural, Natural)
removePendingTx ctx held = do
  acc <- getAccountById held.pendingTxAccount
  txInfo <- liftEither $ parseTxSignData (accountNetwork acc) ctx (accountXPubKey ctx acc) held.pendingTxData
  let touched = touchedBy txInfo.txInfoSummary
  heldCoins <- lift $ filterM coinExists touched.coinsSpent
  lift $ P.delete held.pendingTxKey
  return (fromIntegral (length heldCoins), fromIntegral (length touched.changeAddresses))

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
    where_ $ p ^. DBPendingTxNosigHash ==. val (hashBytes nosigH)

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

-- | What sending a signed transaction from a file did
data Sent
  = -- | A pending transaction of the wallet was sent, as by its nosigHash
    SentPending DBAccount TxInfo
  | -- | A transaction the wallet does not hold was handed to the store of
    -- the network
    SentOutside Network TxHash
  deriving (Eq, Show)

-- | Send a signed transaction from a file, with the network it is for when
-- given. One the wallet holds as pending, the file an offline signer
-- returns, takes the place of the version held and is sent like one sent by
-- its nosigHash: marked online, its raw transaction stored. One the wallet
-- does not hold, a sweep of outside coins for instance, is checked against
-- the coins it spends and handed to the store of the given network, and
-- nothing in the wallet is updated.
sendSigned :: (MonadUnliftIO m) => Ctx -> (Network -> Store) -> Maybe Network -> TxSignData -> ExceptT String (DB m) Sent
sendSigned ctx storeOn netM tsd = do
  let signedTx = txSignDataTx tsd
      pendingHash = nosigHash signedTx
  unless (isSigned signedTx) $ throwError "The transaction is not signed"
  heldM <- lookupPendingTx pendingHash
  case heldM of
    Just held -> do
      acc <- getAccountById held.pendingTxAccount
      let net = accountNetwork acc
      forM_ netM $ \given ->
        when (given /= net) $ throwError "The transaction is pending in an account of another network"
      when (held.pendingTxData /= tsd) $ void $ importPendingTx net ctx held.pendingTxAccount tsd
      uncurry SentPending <$> sendPending ctx storeOn pendingHash
    Nothing -> do
      net <- liftMaybe "Give the --network of a transaction the wallet does not hold" netM
      liftEither $ verifySignedTx net ctx tsd
      fromStore $ storeHealth (storeOn net)
      accepted <- fromStore $ broadcast (storeOn net) signedTx
      unless accepted $ throwError notAccepted
      return $ SentOutside net (txHash signedTx)

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
      throwError "The transaction is in the network's mempool or in the chain: sync retires it once it is listed"
  freed <- removePendingTx ctx held
  -- The raw transaction stored when it was sent goes with it, unless a
  -- coin of the wallet comes from it after all
  when held.pendingTxOnline $ void $ lift $ dropRawTxsWithoutCoins [txid]
  return freed
