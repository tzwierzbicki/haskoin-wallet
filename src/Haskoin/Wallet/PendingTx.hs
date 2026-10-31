{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Containers.ListUtils (nubOrd)
import Data.List (find, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
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
import Haskoin.Wallet.TxSigning (parseOutsideTxSignData, parseTxSignData, verifySignedTx)
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
    pendingTxOnline :: !Bool,
    -- | A coin it spends was spent by another transaction, or is gone, as
    -- the store answered at the last sync
    pendingTxConflicted :: !Bool,
    -- | When it was prepared or imported. A new version of it, signed for
    -- instance, keeps the time.
    pendingTxCreated :: !UTCTime
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
        pendingTxOnline = dBPendingTxOnline row,
        pendingTxConflicted = dBPendingTxConflicted row,
        pendingTxCreated = dBPendingTxCreated row
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

-- | The pending transaction a command names, if there is one: by its
-- nosigHash, or by its id once it is signed, as hw shows it then. The rows
-- are keyed by the nosigHash. An id is looked for among all the rows, which
-- are few: the sync retires a transaction once the network lists it.
findPendingTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) (Maybe PendingTx)
findPendingTx named = do
  byNosig <- lookupPendingTx named
  case byNosig of
    Just held -> return (Just held)
    Nothing -> do
      held <- liftEither . mapM fromRow =<< lift (pendingRows Nothing Nothing Nothing)
      return $ find (signedWithId named) held

-- | The pending transaction a command names, which must exist: by its
-- nosigHash, or by its id once it is signed
requirePendingTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) PendingTx
requirePendingTx named = findPendingTx named >>= liftMaybe noPendingTx

-- | Whether the pending transaction is signed and has the id. An unsigned
-- transaction has no id yet: the hash of its bytes changes once signed.
signedWithId :: TxHash -> PendingTx -> Bool
signedWithId tid p = isSigned tx && txHash tx == tid
  where
    tx = txSignDataTx p.pendingTxData

-- | The rows of an account, or of every account, newest first, and those
-- created at the same time by their nosigHashes, so that the order is
-- settled: those after the transaction given, by when it was created and
-- its nosigHash, or the first ones, up to the count, or all of them
pendingRows :: (MonadUnliftIO m) => Maybe DBAccountId -> Maybe (UTCTime, TxHash) -> Maybe Natural -> DB m [Entity DBPendingTx]
pendingRows accIdM afterM countM =
  select . from $ \p -> do
    forM_ accIdM $ \accId ->
      where_ $ p ^. DBPendingTxAccount ==. val accId
    forM_ afterM $ \(created, nosigH) ->
      where_ $
        p ^. DBPendingTxCreated <=. val created
          &&. (p ^. DBPendingTxCreated <. val created ||. p ^. DBPendingTxNosigHash <. val (hashBytes nosigH))
    orderBy [desc (p ^. DBPendingTxCreated), desc (p ^. DBPendingTxNosigHash)]
    forM_ countM $ limit . rowLimit
    return p

-- | The pending transactions of an account, newest first
pendingTxs :: (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) [PendingTx]
pendingTxs accId = liftEither . mapM fromRow =<< lift (pendingRows (Just accId) Nothing Nothing)

-- | The pending transactions of an account as summaries, newest first:
-- those after the transaction given, or the first ones, up to the count,
-- or all of them
pendingTxsAfter :: (MonadUnliftIO m) => Ctx -> DBAccountId -> Maybe (UTCTime, TxHash) -> Maybe Natural -> ExceptT String (DB m) [TxInfo]
pendingTxsAfter ctx accId afterM countM = map snd <$> pendingAfter ctx (Just accId) afterM countM

-- | The pending transactions of an account, or of every account, newest
-- first, each as a summary with its account: those after the transaction
-- given, or the first ones, up to the count, or all of them
pendingAfter :: (MonadUnliftIO m) => Ctx -> Maybe DBAccountId -> Maybe (UTCTime, TxHash) -> Maybe Natural -> ExceptT String (DB m) [(DBAccount, TxInfo)]
pendingAfter ctx accIdM afterM countM = do
  held <- liftEither . mapM fromRow =<< lift (pendingRows accIdM afterM countM)
  forM held $ \p -> do
    acc <- getAccountById p.pendingTxAccount
    (acc,) <$> pendingSummary ctx acc p

-- | Where the listing goes on after the pending transaction --after names,
-- by either of its hashes: when it was created, and its nosigHash, which
-- orders the transactions created at the same time. Or why the account, or
-- any account, does not hold it.
pendingCursor :: (MonadUnliftIO m) => Maybe DBAccountId -> TxHash -> ExceptT String (DB m) (UTCTime, TxHash)
pendingCursor accIdM named = do
  p <- requirePendingTx named
  case accIdM of
    Just accId | p.pendingTxAccount /= accId -> throwError "The pending transaction is not in the account"
    _ -> return (p.pendingTxCreated, nosigHash (txSignDataTx p.pendingTxData))

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
  return info {txInfoStatus = heldStatus p info.txInfoStatus}

-- | The status of a held transaction: what its signing data says, and what
-- only the database knows, whether the user sent it, whether the last sync
-- found it in conflict, and when it was created. The signing data
-- describes a transaction in preparation; one on the chain is not held and
-- is left as it is.
heldStatus :: PendingTx -> TxStatus -> TxStatus
heldStatus p (Pending status) =
  Pending
    status
      { pendingOnline = p.pendingTxOnline,
        pendingConflicted = p.pendingTxConflicted,
        pendingCreated = Just p.pendingTxCreated
      }
heldStatus _ onChain = onChain

-- | The coins locked by the pending transactions of the wallet on a
-- network: every outpoint one of them spends, whatever its account. A coin
-- is locked exactly as long as a pending transaction spends it; nothing
-- else records it. The transaction of one account can spend the coin of
-- another, a sweep of its addresses for instance, and the coin is then
-- locked in both. The network keeps the chains apart: a transaction from
-- before the Bitcoin Cash fork has the same id on both, and spending its
-- coin on one chain leaves it free on the other.
lockedOutpoints :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Set OutPoint)
lockedOutpoints net = do
  accounts <- lift $ P.selectKeysList [DBAccountNetwork P.==. cs net.name] []
  held <- liftEither . mapM fromRow =<< lift (P.selectList [DBPendingTxAccount P.<-. accounts] [])
  return $ Set.fromList [input.outpoint | p <- held, input <- (txSignDataTx p.pendingTxData).inputs]

-- | The coins of the wallet on a network that the user locked with
-- lockcoin: no pending transaction may spend them, as the builder may not.
-- The coin table is read here, as in 'coinExists', for the import to
-- refuse them.
userLockedOutpoints :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Set OutPoint)
userLockedOutpoints net = do
  accounts <- lift $ P.selectKeysList [DBAccountNetwork P.==. cs net.name] []
  rows <- lift $ P.selectList [DBCoinAccount P.<-. accounts, DBCoinLockedByUser P.==. True] []
  Set.fromList <$> mapM (liftEither . bytesToOutpoint . dBCoinOutpoint . P.entityVal) rows

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

-- | Whether the wallet holds the coin on the network: a transaction from
-- before the Bitcoin Cash fork has the same id on both chains. The coin
-- table is read here rather than through "Haskoin.Wallet.Coin", which needs
-- this module for the locks.
coinExists :: (MonadUnliftIO m) => Network -> OutPoint -> DB m Bool
coinExists net outpoint = do
  coinM <- P.get (DBCoinKey $ outpointBytes outpoint)
  accountM <- join <$> forM coinM (P.get . dBCoinAccount)
  return $ maybe False ((== cs net.name) . dBAccountNetwork) accountM

{- Conflicts -}

-- | The inputs of a transaction that the network no longer lets it spend,
-- from what the store reports of the transactions they come from: an
-- output another transaction spends, an output of a transaction the store
-- has dropped, or an output its transaction does not have. A transaction
-- the store has not seen proves nothing: it may not have reached the store
-- yet, or the store may be a new one. The coins of every account count,
-- and those outside the wallet, a sweep's for instance, alike. A coin spent
-- by one of the given transactions, those a replacement replaces while
-- they are not in a block, is no conflict either.
conflictedInputs :: [Store.Transaction] -> Set TxHash -> Tx -> [OutPoint]
conflictedInputs parents replaced tx = filter inConflict ((.outpoint) <$> tx.inputs)
  where
    known = Map.fromList [(parent.txid, parent) | parent <- parents]
    inConflict :: OutPoint -> Bool
    inConflict outpoint =
      case Map.lookup outpoint.hash known of
        Nothing -> False
        Just parent
          | parent.deleted -> True
          | otherwise ->
              case listToMaybe (drop (fromIntegral outpoint.index) parent.outputs) of
                -- Spent by this very transaction, which the network has, or
                -- by one it replaces, is fine
                Just output -> maybe False (\spender -> spender.txid /= txHash tx && spender.txid `Set.notMember` replaced) output.spender
                Nothing -> True

-- | The transactions other than this one that spend its inputs, as the
-- store reports the transactions they come from. One of them may be this
-- very transaction under another id, its signatures changed: a miner can
-- re-encode them, and another signer makes others. The sync tells by the
-- nosigHash.
rivalSpenders :: [Store.Transaction] -> Tx -> [TxHash]
rivalSpenders parents tx = nubOrd (mapMaybe rival ((.outpoint) <$> tx.inputs))
  where
    live = Map.fromList [(parent.txid, parent) | parent <- parents, not parent.deleted]
    rival :: OutPoint -> Maybe TxHash
    rival outpoint = do
      parent <- Map.lookup outpoint.hash live
      output <- listToMaybe (drop (fromIntegral outpoint.index) parent.outputs)
      spender <- output.spender
      guard (spender.txid /= txHash tx)
      return spender.txid

-- | Record whether a pending transaction is in conflict, as the sync found it
setPendingTxConflicted :: (MonadUnliftIO m) => PendingTx -> Bool -> DB m ()
setPendingTxConflicted p conflicted = P.update p.pendingTxKey [DBPendingTxConflicted P.=. conflicted]

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
    Nothing -> insertPendingTx net accId tsd info.txInfoSummary =<< replacedBy ctx accId tsd
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

-- | Store a transaction the account does not hold yet. A replacement takes
-- over what the transactions it replaces hold: the coins they spend, gone
-- from the coins of the account once they are on the network, and locked by
-- them while they are pending; and the addresses of the wallet they pay,
-- its change address among them.
insertPendingTx :: (MonadUnliftIO m) => Network -> DBAccountId -> TxSignData -> TxSummary -> Replaced -> ExceptT String (DB m) ()
insertPendingTx net accId tsd summary replaced = do
  -- A pending transaction is retired when the sync sees it under one of
  -- the wallet's addresses; one touching none would wait forever
  unless (involvesWallet summary) $
    throwError "The transaction does not involve this wallet"
  let touched = touchedBy summary
      tx = txSignDataTx tsd
  -- The coins of the account it spends must exist
  forM_ touched.coinsSpent $ \outpoint ->
    unless (outpoint `Set.member` replaced.replacedSpent) $ do
      inWallet <- lift $ coinExists net outpoint
      unless inWallet $ throwError "A coin referenced by the transaction does not exist"
  -- No other pending transaction, of any account, may spend one of its
  -- inputs, nor may it spend a coin the user locked; storing it is what
  -- locks them
  let heldByReplaced = Set.fromList [input.outpoint | p <- replaced.replacedRows, input <- (txSignDataTx p.pendingTxData).inputs]
      pathsOfReplaced = [path | p <- replaced.replacedRows, path <- p.pendingTxData.txSignDataOutputPaths]
  locked <- (`Set.difference` heldByReplaced) <$> lockedOutpoints net
  when (any ((`Set.member` locked) . (.outpoint)) tx.inputs) $
    throwError "A coin referenced by the transaction is locked"
  lockedByUser <- userLockedOutpoints net
  when (any ((`Set.member` lockedByUser) . (.outpoint)) tx.inputs) $
    throwError "A coin referenced by the transaction is locked by the user: unlockcoin frees it"
  -- Its addresses must be generated already, and its change addresses
  -- neither used nor paid by another pending transaction
  let addrs = nub $ touched.changeAddresses <> touched.otherAddresses
  known <- walletPaths net accId addrs
  when (Map.size known /= length addrs) $
    throwError "Some referenced addresses do not exist"
  used <- lift . usedAddresses =<< liftEither (mapM (addrToTextE net) (filter (`notElem` replaced.replacedPaid) touched.changeAddresses))
  reserved <- filter (`notElem` pathsOfReplaced) <$> reservedChangePaths accId
  unless (null used && all (`notElem` reserved) touched.changePaths) $
    throwError "Some of the internal output addresses are not free"
  time <- liftIO getCurrentTime
  lift $ P.insert_ $ DBPendingTx accId (hashBytes (nosigHash tx)) (blob tsd) False False time

{- Replacements -}

-- | What the transactions a replacement replaces hold in its account: the
-- coins they spend, the addresses of the wallet they pay, and the rows of
-- those still pending. Each is a transaction of the account not yet in a
-- block: pending and sent, or on the network. One the network dropped,
-- whose summary the sync marked dead, holds nothing any more. A
-- transaction that replaces none holds nothing either.
data Replaced = Replaced
  { replacedSpent :: !(Set OutPoint),
    replacedPaid :: ![Address],
    replacedRows :: ![PendingTx]
  }

-- | What the transactions the signing data replaces hold in the account.
-- One in a block, or not of the account, is refused: it cannot be replaced.
replacedBy :: (MonadUnliftIO m) => Ctx -> DBAccountId -> TxSignData -> ExceptT String (DB m) Replaced
replacedBy ctx accId tsd = do
  acc <- getAccountById accId
  let net = accountNetwork acc
  held <- pendingTxs accId
  found <- forM tsd.txSignDataReplaces $ \tid ->
    case [p | p <- held, txHash (txSignDataTx p.pendingTxData) == tid] of
      p : _ -> do
        info <- liftEither $ parseTxSignData net ctx (accountXPubKey ctx acc) p.pendingTxData
        return $ Just (info.txInfoSummary, Just p)
      [] -> do
        live <- liveSummary net accId tid
        case live of
          Just (block, summary)
            | Store.confirmed block -> throwError $ "The transaction " <> cs (txHashToHex tid) <> " it replaces is in a block already: delete this replacement with deletetx"
            | otherwise -> return $ Just (summary, Nothing)
          Nothing -> do
            dead <- lift $ isJust <$> P.get (DBTxInfoKey accId (hashBytes tid))
            unless dead $ throwError $ "The transaction " <> cs (txHashToHex tid) <> " it replaces is not one of the account"
            return Nothing
  let summaries = fst <$> catMaybes found
  return
    Replaced
      { replacedSpent = Set.fromList [outpoint | s <- summaries, input <- Map.elems s.summaryMyInputs, outpoint <- input.myInputsOutpoints],
        replacedPaid = nub $ concatMap (Map.keys . (.summaryMyOutputs)) summaries,
        replacedRows = mapMaybe snd (catMaybes found)
      }

{- Deleting and retiring -}

-- | Remove a pending transaction, whatever its state: the coins it spent and
-- the change addresses it paid are available again by its absence. Returns
-- how many coins of the wallet, of any account, it was holding and how many
-- change addresses it paid.
removePendingTx :: (MonadUnliftIO m) => Ctx -> PendingTx -> ExceptT String (DB m) (Natural, Natural)
removePendingTx ctx held = do
  acc <- getAccountById held.pendingTxAccount
  txInfo <- liftEither $ parseTxSignData (accountNetwork acc) ctx (accountXPubKey ctx acc) held.pendingTxData
  let touched = touchedBy txInfo.txInfoSummary
      spent = (.outpoint) <$> (txSignDataTx held.pendingTxData).inputs
  heldCoins <- lift $ filterM (coinExists (accountNetwork acc)) spent
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

-- | What sending does last before a transaction goes on the network, for
-- good: given the transaction and its network, it may stop the sending
-- with an error. sendtx shows the transaction there and asks the user.
type BeforeSending m = Network -> TxInfo -> ExceptT String (DB m) ()

-- | Send a pending transaction of the wallet and wait for the store to see
-- it. Once it does, the transaction is marked online, for the sync to send
-- again should the store lose it, and its raw transaction is stored. If the
-- store does not see it, nothing is recorded and the answer says so. The
-- signatures were checked when the transaction was imported. The
-- transaction is named by either of its hashes.
sendPending :: (MonadUnliftIO m) => Ctx -> (Network -> Store) -> BeforeSending m -> TxHash -> ExceptT String (DB m) (DBAccount, TxInfo)
sendPending ctx storeOn beforeSending named = do
  held <- requirePendingTx named
  acc <- getAccountById held.pendingTxAccount
  let signedTx = txSignDataTx held.pendingTxData
      store = storeOn (accountNetwork acc)
  unless (isSigned signedTx) $ throwError "The transaction is not signed"
  -- A replacement may no longer replace anything: a transaction it
  -- replaces confirmed
  replaced <- replacedBy ctx held.pendingTxAccount held.pendingTxData
  beforeSending (accountNetwork acc) =<< pendingSummary ctx acc held
  -- The summary as it will read once sent, made before anything leaves
  summary <- pendingSummary ctx acc held {pendingTxOnline = True}
  fromStore $ storeHealth store
  accepted <- fromStore $ broadcast store signedTx
  unless accepted $ throwError notAccepted
  _ <- lift $ setPendingTxOnline (nosigHash signedTx)
  lift $ insertRawTx signedTx
  -- The pending transactions it replaces are gone from the network once it
  -- spreads, and the sync would send them again. Their raw transactions go
  -- with them, unless a coin of the wallet comes from one after all.
  lift $ forM_ replaced.replacedRows retirePendingTx
  void $ lift $ dropRawTxsWithoutCoins [txHash (txSignDataTx p.pendingTxData) | p <- replaced.replacedRows]
  return (acc, summary)

-- | The account of the wallet a transaction to sign, review or import
-- involves, as 'accountAmong' chooses it, on the network given, if any. A
-- transaction pending in the wallet is of the account that holds it.
-- Another is of the accounts that it involves: those whose keys derive the
-- addresses it spends or pays, at the paths it gives. One that involves
-- none, a sweep between addresses outside the wallet, is of no account,
-- and --account may not name one.
transactionAccount :: (MonadUnliftIO m) => Ctx -> Maybe Text -> Maybe Network -> TxSignData -> ExceptT String (DB m) (Maybe (DBAccountId, DBAccount))
transactionAccount ctx nameM netM tsd = do
  heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
  case heldM of
    Just held -> do
      holder <- getAccountById held.pendingTxAccount
      unless (onTxNetwork holder) $
        throwError "The transaction is pending in an account of another network"
      Just
        <$> accountAmong
          (\name -> "The transaction is pending in the account " <> cs (dBAccountName holder) <> ", not in " <> cs name)
          nameM
          [(held.pendingTxAccount, holder)]
    Nothing -> do
      involved <- filter (involves . snd) <$> lift getAccounts
      unless (all (onTxNetwork . snd) involved) $
        throwError "The transaction involves an account of another network"
      case involved of
        [] -> do
          forM_ nameM $ \name -> getAccountByName (Just name) >> throwError (notInvolved name)
          return Nothing
        _ -> Just <$> accountAmong notInvolved nameM involved
  where
    onTxNetwork acc = maybe True (== accountNetwork acc) netM
    involves acc =
      either (const False) (involvesWallet . txInfoSummary) $
        parseTxSignData (accountNetwork acc) ctx (accountXPubKey ctx acc) tsd
    notInvolved :: Text -> String
    notInvolved name = "The transaction does not involve the account " <> cs name

-- | What sending a signed transaction from a file did
data Sent
  = -- | A pending transaction of the wallet was sent, as by its hash
    SentPending DBAccount TxInfo
  | -- | A transaction the wallet does not hold was handed to the store of
    -- the network
    SentOutside Network TxHash
  deriving (Eq, Show)

-- | Send a signed transaction from a file, with the network it is for when
-- given. One the wallet holds as pending, the file an offline signer
-- returns, takes the place of the version held and is sent like one sent by
-- its hash: marked online, its raw transaction stored. One the wallet
-- does not hold, a sweep of outside coins for instance, is checked against
-- the coins it spends and handed to the store of the given network, and
-- nothing in the wallet is updated.
sendSigned :: (MonadUnliftIO m) => Ctx -> (Network -> Store) -> BeforeSending m -> Maybe Network -> TxSignData -> ExceptT String (DB m) Sent
sendSigned ctx storeOn beforeSending netM tsd = do
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
      uncurry SentPending <$> sendPending ctx storeOn beforeSending pendingHash
    Nothing -> do
      net <- liftMaybe noFileNetwork netM
      liftEither $ verifySignedTx net ctx tsd
      beforeSending net =<< liftEither (parseOutsideTxSignData net ctx tsd)
      fromStore $ storeHealth (storeOn net)
      accepted <- fromStore $ broadcast (storeOn net) signedTx
      unless accepted $ throwError notAccepted
      return $ SentOutside net (txHash signedTx)

-- | Delete a pending transaction, named by either of its hashes, freeing
-- the coins it held. One the user sent is abandoned only when the store
-- does not know it: a transaction in the network's mempool or in the chain
-- may still confirm, and the sync retires it once it is listed.
deleteTx :: (MonadUnliftIO m) => Ctx -> (Network -> Store) -> TxHash -> ExceptT String (DB m) (Natural, Natural)
deleteTx ctx storeOn named = do
  held <- requirePendingTx named
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
