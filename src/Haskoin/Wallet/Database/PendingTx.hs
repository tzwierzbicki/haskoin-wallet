{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | The pending transactions of a portfolio on a network, a pool, from
-- preparation to the network: stored with their signing data, holding the
-- coins they spend, signed in place, sent and watched until the sync
-- retires them. The commands of hw are thin layers over the functions
-- here.
--
-- A pending transaction belongs to a pool, not to an account: it may
-- spend the coins of the BIP44 and of the BIP84 account of the portfolio
-- together, and its change goes to either. A file names no portfolio, so
-- the pool of a transaction read from one is found from the addresses it
-- spends from and pays ('transactionPool').
module Haskoin.Wallet.Database.PendingTx where

import Conduit (MonadUnliftIO)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.List (find, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.RawTx
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Database.TxLabel (setTxLabel)
import Haskoin.Wallet.Keys (AccountKind, PoolPath (..))
import Haskoin.Wallet.ShortId (ShortId, startsWith)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (SpentCoin (..), checkOutputReach, heldPoolOf, parseOutsideTxSignData, parseTxSignData, spentCoins, verifySignedTx)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

{- What a transaction touches -}

-- | What a transaction of the wallet touches, read from its summary
data Touched = Touched
  { -- | The coins it spends
    coinsSpent :: ![OutPoint],
    -- | Its internal addresses, where its change goes, and their paths
    changeAddresses :: ![Address],
    changePaths :: ![PoolPath],
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
    (change, paid) = Map.partition (isIntPath . pathInAccount . myOutputsPath) summary.summaryMyOutputs

{- Pending transactions in the database -}

-- | A pending transaction as the database holds it
data PendingTx = PendingTx
  { pendingTxKey :: !DBPendingTxId,
    -- | The portfolio and the network it is of
    pendingTxPool :: !Pool,
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
  net <- maybeToEither "A pending transaction names a network hw does not know" $ netByName (cs (dBPendingTxNetwork row))
  return
    PendingTx
      { pendingTxKey = key,
        pendingTxPool = Pool (dBPendingTxPortfolio row) net,
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

-- | The hashes of the pending transactions that have the short id, of
-- every portfolio: the nosigHash of each, and the txid of a signed one,
-- the two hashes 'findPendingTx' takes. The rows are few, and read whole,
-- as 'findPendingTx' reads them for a txid.
pendingHashesWith :: (MonadUnliftIO m) => ShortId -> ExceptT String (DB m) [TxHash]
pendingHashesWith short = do
  held <- liftEither . mapM fromRow =<< lift (pendingRows Nothing Nothing Nothing)
  return $ filter (startsWith short) (concatMap hashesOf held)
  where
    hashesOf :: PendingTx -> [TxHash]
    hashesOf p =
      let tx = txSignDataTx p.pendingTxData
       in nosigHash tx : [txHash tx | isSigned tx]

-- | The rows of the pools, or of every pool, newest first, and those
-- created at the same time by their nosigHashes, so that the order is
-- settled: those after the transaction given, by when it was created and
-- its nosigHash, or the first ones, up to the count, or all of them
pendingRows :: (MonadUnliftIO m) => Maybe [Pool] -> Maybe (UTCTime, TxHash) -> Maybe Natural -> DB m [Entity DBPendingTx]
pendingRows poolsM afterM countM =
  select . from $ \p -> do
    forM_ poolsM $ \pools ->
      where_ $ foldr (\pool others -> inPool' p pool ||. others) (val False) pools
    forM_ afterM $ \(created, nosigH) ->
      where_ $
        p ^. DBPendingTxCreated <=. val created
          &&. (p ^. DBPendingTxCreated <. val created ||. p ^. DBPendingTxNosigHash <. val (hashBytes nosigH))
    orderBy [desc (p ^. DBPendingTxCreated), desc (p ^. DBPendingTxNosigHash)]
    forM_ countM $ limit . rowLimit
    return p
  where
    inPool' p pool =
      p ^. DBPendingTxPortfolio ==. val pool.poolPortfolio
        &&. p ^. DBPendingTxNetwork ==. val (poolNetworkName pool)

-- | The pending transactions of a pool, newest first
pendingTxs :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) [PendingTx]
pendingTxs pool = liftEither . mapM fromRow =<< lift (pendingRows (Just [pool]) Nothing Nothing)

-- | The pending transactions of a pool as summaries, newest first: those
-- after the transaction given, or the first ones, up to the count, or all
-- of them
pendingTxsAfter :: (MonadUnliftIO m) => Ctx -> Pool -> Maybe (UTCTime, TxHash) -> Maybe Natural -> ExceptT String (DB m) [TxInfo]
pendingTxsAfter ctx pool afterM countM = map snd <$> pendingAfter ctx (Just [pool]) afterM countM

-- | The pending transactions of the pools, or of every pool, newest
-- first, each as a summary with where it is: those after the transaction
-- given, or the first ones, up to the count, or all of them
pendingAfter :: (MonadUnliftIO m) => Ctx -> Maybe [Pool] -> Maybe (UTCTime, TxHash) -> Maybe Natural -> ExceptT String (DB m) [(Place, TxInfo)]
pendingAfter ctx poolsM afterM countM = do
  held <- liftEither . mapM fromRow =<< lift (pendingRows poolsM afterM countM)
  forM held $ \p -> (,) <$> poolPlace p.pendingTxPool <*> pendingSummary ctx p

-- | Where the listing goes on after the pending transaction --after names,
-- by either of its hashes: when it was created, and its nosigHash, which
-- orders the transactions created at the same time. Or why the pools, or
-- any pool, do not hold it.
pendingCursor :: (MonadUnliftIO m) => Maybe [Pool] -> TxHash -> ExceptT String (DB m) (UTCTime, TxHash)
pendingCursor poolsM named = do
  p <- requirePendingTx named
  case poolsM of
    Just pools | p.pendingTxPool `notElem` pools -> throwError "The pending transaction is not among those listed"
    _ -> return (p.pendingTxCreated, nosigHash (txSignDataTx p.pendingTxData))

-- | The kind and the public key of each account of a pool, as the
-- summaries and the signers take the accounts
poolKeys :: (MonadUnliftIO m) => Ctx -> Pool -> DB m [(AccountKind, XPubKey)]
poolKeys ctx pool = map (\(_, acc) -> (accountKind acc, accountXPubKey ctx acc)) <$> poolAccounts pool

-- | The transaction of the signing data as the pool sees it: parsed
-- against the keys of its accounts, with its labels as the wallet has
-- them, and the addresses of other portfolios it pays ('fillTxInfo')
txInfoOf :: (MonadUnliftIO m) => Ctx -> Pool -> TxSignData -> ExceptT String (DB m) TxInfo
txInfoOf ctx pool tsd = do
  keys <- lift $ poolKeys ctx pool
  info <- liftEither $ parseTxSignData pool.poolNetwork ctx keys tsd
  lift $ fillTxInfo pool info

-- | The summary of a pending transaction: the transaction as its portfolio
-- sees it, with its status as held
pendingSummary :: (MonadUnliftIO m) => Ctx -> PendingTx -> ExceptT String (DB m) TxInfo
pendingSummary ctx p = do
  info <- txInfoOf ctx p.pendingTxPool p.pendingTxData
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
-- network: every outpoint one of them spends, whatever its portfolio. A
-- coin is locked exactly as long as a pending transaction spends it;
-- nothing else records it. The transaction of one portfolio can spend the
-- coin of another, a sweep of its addresses for instance, and the coin is
-- then locked in both. The network keeps the chains apart: a transaction
-- from before the Bitcoin Cash fork has the same id on both, and spending
-- its coin on one chain leaves it free on the other.
lockedOutpoints :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Set OutPoint)
lockedOutpoints net = do
  held <- liftEither . mapM fromRow =<< lift (P.selectList [DBPendingTxNetwork P.==. cs net.name] [])
  return $ Set.fromList [input.outpoint | p <- held, input <- (txSignDataTx p.pendingTxData).inputs]

-- | The coins of the wallet on a network that the user locked with
-- lockcoin: no pending transaction may spend them, as the builder may not.
-- The coin table is read here, as in 'coinExists', for the import to
-- refuse them.
userLockedOutpoints :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Set OutPoint)
userLockedOutpoints net = do
  accounts <- lift $ accountsOnNetwork net
  rows <- lift $ P.selectList [DBCoinAccount P.<-. accounts, DBCoinLockedByUser P.==. True] []
  Set.fromList <$> mapM (liftEither . bytesToOutpoint . dBCoinOutpoint . P.entityVal) rows

-- | The change addresses the pending transactions of a pool pay, as
-- paths: held as long as a transaction is pending, like the coins it
-- spends
reservedChangePaths :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) [PoolPath]
reservedChangePaths pool = do
  held <- pendingTxs pool
  return $ nub [path | p <- held, path <- p.pendingTxData.txSignDataOutputPaths, isIntPath (pathInAccount path)]

-- | The change address for a new transaction of the pool, in its account
-- of the kind given: the lowest internal address of that account that no
-- transaction has used and no pending transaction pays, or the next one
-- of the chain. A pool has a change chain for each of its accounts, and
-- the builder says which type the change takes.
changeAddress :: (MonadUnliftIO m) => Ctx -> Config -> Pool -> AccountKind -> ExceptT String (DB m) DBAddress
changeAddress ctx cfg pool kind = do
  (accId, _) <- poolAccountOf pool kind
  reserved <- reservedChangePaths pool
  nextChangeAddress ctx cfg accId [pathInAccount path | path <- reserved, pathKind path == kind]

-- | Whether the wallet holds the coin on the network: a transaction from
-- before the Bitcoin Cash fork has the same id on both chains. The coin
-- table is read here rather than through "Haskoin.Wallet.Database.Coin", which needs
-- this module for the locks.
coinExists :: (MonadUnliftIO m) => Network -> OutPoint -> DB m Bool
coinExists net outpoint = do
  accounts <- accountsOnNetwork net
  (> 0) <$> P.count [DBCoinOutpoint P.==. outpointBytes outpoint, DBCoinAccount P.<-. accounts]

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

-- | Store a transaction of the pool as pending, which locks the coins it
-- spends and holds its change address. A signed transaction must
-- carry valid signatures: they are checked here, so that sending has
-- nothing left to check. Importing a transaction the pool already
-- holds replaces it with the newer version, signed where it was unsigned,
-- as 'heldVersion' merges it; the transaction itself, its inputs and
-- outputs, cannot change, since the hash identifying it leaves the
-- signatures out.
importPendingTx :: (MonadUnliftIO m) => Ctx -> Pool -> TxSignData -> ExceptT String (DB m) TxHash
importPendingTx ctx pool = importPreparedTx ctx pool Nothing

-- | 'importPendingTx' for a transaction preparetx just built, with the
-- address --change-back named for its change, if any. That address may
-- be used already, or be the change of another pending transaction: the
-- user chose it, and the specification takes any address of the
-- portfolio on the network of the payment. A change address the builder
-- took by itself must still be free ('insertPendingTx').
importPreparedTx :: (MonadUnliftIO m) => Ctx -> Pool -> Maybe Address -> TxSignData -> ExceptT String (DB m) TxHash
importPreparedTx ctx pool changeBackM tsd = do
  let net = pool.poolNetwork
  accounts <- lift $ poolAccounts pool
  heldM <- lookupPendingTx pendingHash
  forM_ heldM $ \held ->
    when (held.pendingTxPool /= pool) $ do
      place <- poolPlace held.pendingTxPool
      throwError ("The transaction is pending in " <> placeText place)
  let imported = maybe tsd (`heldVersion` tsd) heldM
      keys = [(accountKind acc, accountXPubKey ctx acc) | (_, acc) <- accounts]
  info <- liftEither $ parseTxSignData net ctx keys imported
  liftEither $ checkOutputReach (heldPoolOf (snd <$> accounts)) imported
  when (isSigned tx) $ liftEither $ verifySignedTx net ctx imported
  case heldM of
    Just held -> replaceVersion held imported
    Nothing -> insertPendingTx pool changeBackM imported info.txInfoSummary =<< replacedBy ctx pool imported
  return pendingHash
  where
    tx = txSignDataTx tsd
    pendingHash = nosigHash tx

-- | A place as a sentence names it: "portfolio0 of wallet0 on btc"
placeText :: Place -> String
placeText place =
  cs (portfolioName (refPortfolio (placePortfolio place)))
    <> " of "
    <> cs (walletName (refWallet (placePortfolio place)))
    <> " on "
    <> (placeNetwork place).name

-- | The version a file brings of a transaction the portfolio holds: the
-- transaction as the file has it, signed where the one held was not, and
-- the seed the file names, when it does. All else is as held: the paths of
-- the portfolio's addresses, the fee rate, the transactions replaced, and
-- the label, unless the one held is empty. A file signed and finalized by
-- other software names no key of its signed inputs, and a file another
-- wallet prepared carries none of hw's fields: the wallet knows them. The
-- label held stays, as the user may have changed it since the file was
-- written.
heldVersion :: PendingTx -> TxSignData -> TxSignData
heldVersion held tsd =
  held.pendingTxData
    { txSignDataTx = tsd.txSignDataTx,
      txSignDataFingerprint = tsd.txSignDataFingerprint <|> held.pendingTxData.txSignDataFingerprint,
      txSignDataLabel = if Text.null heldLabel then tsd.txSignDataLabel else heldLabel
    }
  where
    heldLabel = held.pendingTxData.txSignDataLabel

-- | Replace the version a portfolio holds of a transaction with the one
-- imported, as 'heldVersion' merges it: the signed version of an unsigned
-- transaction, or another valid signature of a signed one
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

-- | Store a transaction the pool does not hold yet. A replacement takes
-- over what the transactions it replaces hold: the coins they spend, gone
-- from the coins of the pool once they are on the network, and locked by
-- them while they are pending; and the addresses of the wallet they pay,
-- its change address among them.
--
-- Its change addresses must be free: neither used, nor the change of
-- another pending transaction. The address given, the one --change-back
-- named, is the exception: the user chose it ('importPreparedTx').
insertPendingTx :: (MonadUnliftIO m) => Pool -> Maybe Address -> TxSignData -> TxSummary -> Replaced -> ExceptT String (DB m) ()
insertPendingTx pool changeBackM tsd summary replaced = do
  let net = pool.poolNetwork
  -- A pending transaction is retired when the sync sees it under one of
  -- the wallet's addresses; one touching none would wait forever
  unless (involvesWallet summary) $
    throwError "The transaction does not involve this wallet"
  let touched = touchedBy summary
      tx = txSignDataTx tsd
  -- The coins of the pool it spends must exist
  forM_ touched.coinsSpent $ \outpoint ->
    unless (outpoint `Set.member` replaced.replacedSpent) $ do
      inWallet <- lift $ coinExists net outpoint
      unless inWallet $ throwError "A coin referenced by the transaction does not exist"
  -- No other pending transaction, of any portfolio, may spend one of its
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
  -- neither used nor paid by another pending transaction, but the one
  -- --change-back named
  let addrs = nub $ touched.changeAddresses <> touched.otherAddresses
      takenChange = [(addr, path) | (addr, path) <- zip touched.changeAddresses touched.changePaths, Just addr /= changeBackM]
  known <- poolPaths pool addrs
  when (Map.size known /= length addrs) $
    throwError "Some referenced addresses do not exist"
  accounts <- lift $ map fst <$> poolAccounts pool
  used <- lift . usedAddresses accounts =<< liftEither (mapM (addrToTextE net) (filter (`notElem` replaced.replacedPaid) (fst <$> takenChange)))
  reserved <- filter (`notElem` pathsOfReplaced) <$> reservedChangePaths pool
  unless (null used && all ((`notElem` reserved) . snd) takenChange) $
    throwError "Some of the internal output addresses are not free"
  time <- liftIO getCurrentTime
  lift $ P.insert_ $ DBPendingTx pool.poolPortfolio (poolNetworkName pool) (hashBytes (nosigHash tx)) (blob tsd) False False time

{- Replacements -}

-- | What the transactions a replacement replaces hold in its pool: the
-- coins they spend, the addresses of the wallet they pay, and the rows of
-- those still pending. Each is a transaction of the pool not yet in a
-- block: pending and sent, or on the network. One the network dropped,
-- whose summary the sync marked dead, holds nothing any more. A
-- transaction that replaces none holds nothing either.
data Replaced = Replaced
  { replacedSpent :: !(Set OutPoint),
    replacedPaid :: ![Address],
    replacedRows :: ![PendingTx]
  }

-- | What the transactions the signing data replaces hold in the pool.
-- One in a block, or not of the pool, is refused: it cannot be replaced.
replacedBy :: (MonadUnliftIO m) => Ctx -> Pool -> TxSignData -> ExceptT String (DB m) Replaced
replacedBy ctx pool tsd = do
  keys <- lift $ poolKeys ctx pool
  held <- pendingTxs pool
  found <- forM tsd.txSignDataReplaces $ \tid ->
    case [p | p <- held, txHash (txSignDataTx p.pendingTxData) == tid] of
      p : _ -> do
        info <- liftEither $ parseTxSignData pool.poolNetwork ctx keys p.pendingTxData
        return $ Just (info.txInfoSummary, Just p)
      [] -> do
        live <- liveSummary pool tid
        case live of
          Just (block, summary)
            | Store.confirmed block -> throwError $ "The transaction " <> cs (txHashToHex tid) <> " it replaces is in a block already: delete this replacement with deletetx"
            | otherwise -> return $ Just (summary, Nothing)
          Nothing -> do
            dead <- lift $ isJust <$> P.get (txInfoKey pool tid)
            unless dead $ throwError $ "The transaction " <> cs (txHashToHex tid) <> " it replaces is not one of the portfolio"
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
-- how many coins of the wallet, of any portfolio, it was holding and how
-- many change addresses it paid.
removePendingTx :: (MonadUnliftIO m) => Ctx -> PendingTx -> ExceptT String (DB m) (Natural, Natural)
removePendingTx ctx held = do
  let pool = held.pendingTxPool
  keys <- lift $ poolKeys ctx pool
  txInfo <- liftEither $ parseTxSignData pool.poolNetwork ctx keys held.pendingTxData
  let touched = touchedBy txInfo.txInfoSummary
      spent = (.outpoint) <$> (txSignDataTx held.pendingTxData).inputs
  heldCoins <- lift $ filterM (coinExists pool.poolNetwork) spent
  lift $ P.delete held.pendingTxKey
  return (fromIntegral (length heldCoins), fromIntegral (length touched.changeAddresses))

-- | Retire a pending transaction the network has under the id given, as
-- the sync does: the row goes, and its label, if any, goes to the
-- transaction of that id in the pool. The id is that of its signed
-- version, unless the network has it signed otherwise. Its coins are gone,
-- or going with the sync, and its change address was used on the chain,
-- so it stays busy.
retirePendingTx :: (MonadUnliftIO m) => PendingTx -> TxHash -> DB m ()
retirePendingTx p networkId = do
  let label = p.pendingTxData.txSignDataLabel
  unless (Text.null label) $ setTxLabel p.pendingTxPool networkId label
  P.delete p.pendingTxKey

{- Labels -}

-- | Give a pending transaction a label, in place of the one it had. An
-- empty label removes it.
setPendingTxLabel :: (MonadUnliftIO m) => PendingTx -> Text -> DB m ()
setPendingTxLabel p label =
  P.update p.pendingTxKey [DBPendingTxBlob P.=. blob p.pendingTxData {txSignDataLabel = label}]

-- | Label a transaction of the wallet, or remove its label with an empty
-- one. A pending transaction is named by either of its hashes, and keeps
-- the label in its signing data, which carries it to the network. A
-- transaction on the network is named by its id. It is labelled in every
-- portfolio whose history holds it, both ends of a payment between two
-- portfolios for instance. Returns the transaction as each portfolio
-- sees it.
labelTransaction :: (MonadUnliftIO m) => Ctx -> TxHash -> Text -> ExceptT String (DB m) [(Place, TxInfo)]
labelTransaction ctx named label = do
  pendingM <- findPendingTx named
  case pendingM of
    Just held -> do
      lift $ setPendingTxLabel held label
      place <- poolPlace held.pendingTxPool
      info <- pendingSummary ctx held {pendingTxData = held.pendingTxData {txSignDataLabel = label}}
      return [(place, info)]
    Nothing -> do
      found <- inHistories named
      forM found $ \(pool, place, info) -> do
        lift $ setTxLabel pool named label
        return (place, info {txInfoLabel = label})

-- | A transaction of the wallet as each portfolio that holds it sees it,
-- found as 'labelTransaction' finds it, with nothing changed: what show
-- answers. A pending transaction is named by either of its hashes, and is
-- of one portfolio. A transaction on the network is named by its id, and
-- is in every portfolio whose history holds it, with the amounts of each.
transactionViews :: (MonadUnliftIO m) => Ctx -> TxHash -> ExceptT String (DB m) [(Pool, Place, TxInfo)]
transactionViews ctx named = do
  pendingM <- findPendingTx named
  case pendingM of
    Just held -> do
      place <- poolPlace held.pendingTxPool
      info <- pendingSummary ctx held
      return [(held.pendingTxPool, place, info)]
    Nothing -> inHistories named

-- | A transaction on the network, by its id, as each pool whose history
-- holds it sees it, or why there is none
inHistories :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) [(Pool, Place, TxInfo)]
inHistories named = do
  pools <- lift allPools
  found <- fmap catMaybes . forM pools $ \pool -> do
    infoM <- storedTxInfo pool named
    forM infoM $ \info -> (\place -> (pool, place, info)) <$> poolPlace pool
  when (null found) $
    throwError $ "The wallet has no transaction " <> cs (txHashToHex named)
  return found

-- | Mark a pending transaction as sent to the network by the user. The
-- number of rows changed: one, or none for a hash that is not pending.
setPendingTxOnline :: (MonadUnliftIO m) => TxHash -> DB m Natural
setPendingTxOnline nosigH =
  fmap fromIntegral . updateCount $ \p -> do
    set p [DBPendingTxOnline =. val True]
    where_ $ p ^. DBPendingTxNosigHash ==. val (hashBytes nosigH)

{- Sending and abandoning -}

-- | What sending does last before a transaction goes on the network, for
-- good: given the transaction, its network, and where it is when the
-- wallet holds it, it may stop the sending with an error. broadcast shows
-- the transaction there and asks the user.
type BeforeSending m = Network -> Maybe Place -> TxInfo -> ExceptT String (DB m) ()

-- | Send a pending transaction of the wallet and wait for the store to see
-- it. Once it does, the transaction is marked online, for the sync to send
-- again should the store lose it, and its raw transaction is stored. If the
-- store does not see it, nothing is recorded and the answer says so. The
-- signatures were checked when the transaction was imported. The
-- transaction is named by either of its hashes.
sendPending :: (MonadUnliftIO m) => Ctx -> (Network -> Store) -> BeforeSending m -> TxHash -> ExceptT String (DB m) (Place, TxInfo)
sendPending ctx storeOn beforeSending named = do
  held <- requirePendingTx named
  let pool = held.pendingTxPool
      net = pool.poolNetwork
      signedTx = txSignDataTx held.pendingTxData
      store = storeOn net
  place <- poolPlace pool
  unless (isSigned signedTx) $ throwError "The transaction is not signed"
  -- A replacement may no longer replace anything: a transaction it
  -- replaces confirmed
  replaced <- replacedBy ctx pool held.pendingTxData
  beforeSending net (Just place) =<< pendingSummary ctx held
  -- The summary as it will read once sent, made before anything leaves
  summary <- pendingSummary ctx held {pendingTxOnline = True}
  fromStore $ checkHealth store
  accepted <- fromStore $ broadcast store signedTx
  unless accepted $ throwError notAccepted
  _ <- lift $ setPendingTxOnline (nosigHash signedTx)
  lift $ insertRawTx signedTx
  -- The pending transactions it replaces are gone from the network once it
  -- spreads, and the sync would send them again. Their labels stay with
  -- their ids, for the history to show them dead. Their raw transactions
  -- go with them, unless a coin of the wallet comes from one after all.
  lift $ forM_ replaced.replacedRows $ \p -> retirePendingTx p (txHash (txSignDataTx p.pendingTxData))
  void $ lift $ dropRawTxsWithoutCoins [txHash (txSignDataTx p.pendingTxData) | p <- replaced.replacedRows]
  return (place, summary)

-- | The pool of a transaction to sign, show or import, and its signing
-- data with the paths of that pool's addresses it leaves out, as
-- 'withWalletPaths' fills them in.
--
-- The pool is the one that holds the transaction pending. A file of
-- another network than the holder's is refused. Otherwise it is one of
-- the pools the transaction involves ('involvement'): those whose keys
-- derive the addresses it spends or pays, at the paths it gives or the
-- wallet knows. The pool that pays comes first: a transaction that moves
-- funds between two portfolios is of the portfolio it spends from, which
-- must lock the coins and sign. With no pool that pays, it is of the one
-- it pays. So the command finds the portfolio by itself, and the user
-- names none.
--
-- When several pay, or none pays and it pays several, the pool of the
-- active portfolio is taken, when it is one of them: the user who
-- switched to a portfolio means that one. Otherwise the refusal names
-- them, for the user to switch to the one to use. The active portfolio
-- settles nothing else: a transfer is of the portfolio that pays it,
-- whichever is active, since that one locks its coins and signs.
--
-- The test networks share their keys, so a transaction of a file that
-- names no network may involve the same portfolio on several of them. The
-- network given, the default one of the configuration, then decides.
--
-- A transaction that involves no pool, a sweep between addresses outside
-- the wallet, is of none, and the user may not name a portfolio for it.
-- One that involves no pool on the network named, and one on another
-- network, is refused: the network named is then the wrong one.
-- Once the pool is known, an output path too far past the addresses it
-- holds is refused ('checkOutputReach'), so that a file cannot strand the
-- change at an index no scan reaches.
transactionPool ::
  (MonadUnliftIO m) =>
  Ctx ->
  -- | The active portfolio, when the database has one
  Maybe DBPortfolioId ->
  -- | The network of the file, or of --network
  Maybe Network ->
  -- | The default network of the configuration
  Network ->
  TxSignData ->
  ExceptT String (DB m) (Maybe Pool, TxSignData)
transactionPool ctx activeM netM fallback tsd = do
  heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
  case heldM of
    Just held -> do
      let pool = held.pendingTxPool
      unless (onTxNetwork pool) $
        throwError "The transaction is pending on another network"
      reachedPool pool =<< withWalletPaths ctx pool tsd
    Nothing -> do
      pools <- candidatePools
      parsed <- forM pools $ \pool -> (pool,) <$> involvement ctx pool tsd
      let involved = [(pool, completed, summary) | (pool, Just (completed, summary)) <- parsed]
          paying = [found | found@(_, _, summary) <- involved, paysFrom summary]
      found <- onOneNetwork (if null paying then involved else paying)
      case found of
        [] -> do
          -- The network named is not the one the wallet has the
          -- transaction on: a wrong --network, or a file of the other
          -- chain. Read as outside the wallet, it would be signed or sent
          -- on the wrong chain.
          elsewhere <- involvedElsewhere
          when elsewhere $ throwError "The transaction involves an account of another network"
          return (Nothing, tsd)
        [(pool, completed, _)] -> reachedPool pool completed
        several
          | [(pool, completed, _)] <- [candidate | candidate@(pool, _, _) <- several, Just pool.poolPortfolio == activeM] -> reachedPool pool completed
          | otherwise -> do
              places <- mapM (\(pool, _, _) -> poolPlace pool) several
              throwError $
                "The transaction "
                  <> (if null paying then "pays" else "spends from")
                  <> " several portfolios, "
                  <> intercalateAnd (placeText <$> places)
                  <> ": switch to the one to use first"
  where
    onTxNetwork :: Pool -> Bool
    onTxNetwork pool = maybe True (== pool.poolNetwork) netM
    -- The pools to try: those of every portfolio, on the network of the
    -- transaction when it is known
    candidatePools = filter onTxNetwork <$> lift allPools
    -- Whether the transaction involves a pool on another network than the
    -- one named, when one is named: the pools the candidates left out
    involvedElsewhere = case netM of
      Nothing -> return False
      Just _ -> do
        pools <- lift allPools
        fmap or . forM (filter (not . onTxNetwork) pools) $ \pool -> isJust <$> involvement ctx pool tsd
    -- The pools found, on one network: when they are on several, those on
    -- the default network, or a refusal when it is none of them
    onOneNetwork :: (Monad n) => [(Pool, a, b)] -> ExceptT String n [(Pool, a, b)]
    onOneNetwork found =
      case nubOrdOn (.name) [poolNetwork pool | (pool, _, _) <- found] of
        networks@(_ : _ : _) ->
          case [f | f@(pool, _, _) <- found, poolNetwork pool == fallback] of
            [] ->
              throwError $
                "The transaction names no network, and the test networks share their keys: it could be on "
                  <> networksText networks
                  <> ". Set the network to read it on with setdefault --network"
            onFallback -> return onFallback
        _ -> return found

-- | The pool of a transaction, with its signing data completed, once the
-- paths of its outputs are checked against the addresses the pool holds
-- ('checkOutputReach')
reachedPool :: (MonadUnliftIO m) => Pool -> TxSignData -> ExceptT String (DB m) (Maybe Pool, TxSignData)
reachedPool pool completed = do
  accounts <- lift $ poolAccounts pool
  liftEither $ checkOutputReach (heldPoolOf (snd <$> accounts)) completed
  return (Just pool, completed)

-- | Whether the portfolio of a summary pays the transaction: it spends a
-- coin of its own
paysFrom :: TxSummary -> Bool
paysFrom = not . Map.null . summaryMyInputs

-- | The transaction as the pool sees it, when it involves the pool: its
-- signing data with the paths of the pool's addresses filled in
-- ('withWalletPaths'), and its summary. Nothing when the pool has no
-- address the transaction spends from or pays.
--
-- The transaction is read with the paths the signing data gives first.
-- Those are the paths of the portfolio that wrote the file. With the keys
-- of another portfolio, they derive other addresses than those of the
-- transaction, and the reading fails. That is the case of a transfer read
-- by the portfolio it pays. The transaction is then read again with only
-- the paths the wallet knows for the pool, as for a file of another
-- wallet, which names none of them.
involvement :: (MonadUnliftIO m) => Ctx -> Pool -> TxSignData -> ExceptT String (DB m) (Maybe (TxSignData, TxSummary))
involvement ctx pool tsd = do
  keys <- lift $ poolKeys ctx pool
  let readWith signing = eitherToMaybe (txInfoSummary <$> parseTxSignData pool.poolNetwork ctx keys signing)
      involving signing = [(signing, summary) | Just summary <- [readWith signing], involvesWallet summary]
  withFilePaths <- withWalletPaths ctx pool tsd
  case readWith withFilePaths of
    Just _ -> return (listToMaybe (involving withFilePaths))
    Nothing -> do
      withOwnPaths <- withWalletPaths ctx pool tsd {txSignDataInputPaths = [], txSignDataOutputPaths = []}
      return (listToMaybe (involving withOwnPaths))

-- | Items as a sentence lists them: "a", "a and b", "a, b and c"
intercalateAnd :: [String] -> String
intercalateAnd items = case reverse items of
  [] -> ""
  [one] -> one
  final : others -> foldr1 (\a b -> a <> ", " <> b) (reverse others) <> " and " <> final

-- | The signing data with the paths of the pool's addresses in the
-- transaction that it leaves out. A file hw prepared or signed names the
-- path of every input of the portfolio, in a field of its own that a
-- finalizer leaves. A file another wallet prepared, and other software
-- signed and finalized, names no key of its signed inputs, as the spec
-- lets a finalizer clear them, and it may name no key of an output of
-- the portfolio. The wallet knows the addresses it handed out or found
-- used, so it fills those paths in. An address it never generated, as
-- on an offline computer, stays without a path. Signing data whose
-- coins cannot be found is left as it is: parsing it says why.
withWalletPaths :: (MonadUnliftIO m) => Ctx -> Pool -> TxSignData -> ExceptT String (DB m) TxSignData
withWalletPaths ctx pool tsd = do
  let net = pool.poolNetwork
      spent = either (const []) (map (.address)) (spentCoins net ctx tsd)
      paid = [addr | out <- tsd.txSignDataTx.outputs, Right script <- [unmarshal ctx out.script], Just addr <- [outputAddress net ctx script]]
  known <- poolPaths pool (spent <> paid)
  let pathsOf addrs listed = nubOrd (listed <> mapMaybe (`Map.lookup` known) addrs)
  return
    tsd
      { txSignDataInputPaths = pathsOf spent tsd.txSignDataInputPaths,
        txSignDataOutputPaths = pathsOf paid tsd.txSignDataOutputPaths
      }

-- | What sending a signed transaction from a file did
data Sent
  = -- | A pending transaction of the wallet was sent, as by its hash
    SentPending Place TxInfo
  | -- | A transaction the wallet does not hold was handed to the store of
    -- the network
    SentOutside Network TxHash
  deriving (Eq, Show)

-- | Send a signed transaction from a file, with the network the file is
-- for when something names it, and the network to send on when the wallet
-- does not hold the transaction and the file names none. One the wallet
-- holds as pending, the file an offline signer returns, takes the place
-- of the version held, as 'heldVersion' merges it, and is sent like one
-- sent by its hash: marked online, its raw transaction stored. One the
-- wallet does not hold, a sweep of outside coins for instance, is checked
-- against the coins it spends and handed to the store of the network, and
-- nothing in the wallet is updated.
sendSigned :: (MonadUnliftIO m) => Ctx -> (Network -> Store) -> BeforeSending m -> Maybe Network -> Network -> TxSignData -> ExceptT String (DB m) Sent
sendSigned ctx storeOn beforeSending netM net tsd = do
  let signedTx = txSignDataTx tsd
      pendingHash = nosigHash signedTx
  unless (isSigned signedTx) $ throwError "The transaction is not signed"
  heldM <- lookupPendingTx pendingHash
  case heldM of
    Just held -> do
      let pool = held.pendingTxPool
      forM_ netM $ \given ->
        when (given /= pool.poolNetwork) $ throwError "The transaction is pending on another network"
      -- A label that differs is no new version: the label held stays
      let version = heldVersion held tsd
      when (version {txSignDataLabel = held.pendingTxData.txSignDataLabel} /= held.pendingTxData) $
        void $ importPendingTx ctx pool tsd
      uncurry SentPending <$> sendPending ctx storeOn beforeSending pendingHash
    Nothing -> do
      liftEither $ verifySignedTx net ctx tsd
      beforeSending net Nothing =<< liftEither (parseOutsideTxSignData net ctx tsd)
      fromStore $ checkHealth (storeOn net)
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
    known <- fromStore $ storeTransactions (storeOn held.pendingTxPool.poolNetwork) [txid]
    when (any (\t -> t.txid == txid && not t.deleted) known) $
      throwError "The transaction is in the network's mempool or in the chain: sync retires it once it is listed"
  freed <- removePendingTx ctx held
  -- The raw transaction stored when it was sent goes with it, unless a
  -- coin of the wallet comes from it after all
  when held.pendingTxOnline $ void $ lift $ dropRawTxsWithoutCoins [txid]
  return freed
