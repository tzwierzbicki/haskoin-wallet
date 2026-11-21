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
--
-- A pending transaction holds the coins it spends, and nothing else may
-- take them. A replacement, which bumpfee prepares, also holds the coins
-- of the transactions it replaces, which go once it is sent
-- ('replacedOutpoints'). The user holds coins too, against every
-- transaction: one coin with lockcoin, and the coins of a tag that locks
-- its coins. Such a coin is refused when a transaction becomes pending,
-- and again when a transaction is signed and when it is sent
-- ('refuseLockedCoins'): a file never becomes pending here, and a lock
-- may come after the transaction.
--
-- A pending transaction can say which group its change belongs to: when
-- the user asks, its change address gets the tags that every coin it
-- spends has ('changeTakesTags'). The change address a deleted
-- transaction gives back loses its tags ('untagFreedChange').
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
import Data.List (find, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config, commandNetwork, configDefaultNetwork, isShown, networksText, onHiddenNetworks, onShownNetworks)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.RawTx
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Database.Tag (clearAddressTags, deleteTxTags, lockingTags, moveTxTags, setAddressTags, sharedAddressTags, tagTx, untagTx)
import Haskoin.Wallet.Database.TxLabel (setTxLabel)
import Haskoin.Wallet.Tags (Tag, readTag, tagText, tagsText)
import Haskoin.Wallet.Keys (AccountKind, PoolPath (..), poolPathText)
import Haskoin.Wallet.ShortId (ShortId, shortCoinText, shortHashText, startsWith)
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

-- | What the transaction of a summary touches. Its change addresses are
-- the internal addresses it pays, and no other: change that goes back to
-- a receiving address is among the other addresses.
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
    pendingTxCreated :: !UTCTime,
    -- | The tags its payment named with --spend-from-tag: it may spend the
    -- coins those tags lock ('refuseLockedCoins')
    pendingTxOpened :: ![Tag]
  }

-- | A row read back. A row whose signing data does not decode is an error,
-- never a transaction that is not there.
fromRow :: Entity DBPendingTx -> Either String PendingTx
fromRow (Entity key row) = do
  tsd <- Json.eitherDecode $ BS.fromStrict $ dBPendingTxBlob row
  net <- maybeToEither "A pending transaction names a network hw does not know" $ netByName (cs (dBPendingTxNetwork row))
  opened <- mapM readTag (Text.words (dBPendingTxOpenedTags row))
  return
    PendingTx
      { pendingTxKey = key,
        pendingTxPool = Pool (dBPendingTxPortfolio row) net,
        pendingTxData = tsd,
        pendingTxOnline = dBPendingTxOnline row,
        pendingTxConflicted = dBPendingTxConflicted row,
        pendingTxCreated = dBPendingTxCreated row,
        pendingTxOpened = opened
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

-- | The hashes of every transaction of the wallet that have the short id:
-- those of the pending transactions ('pendingHashesWith') and the txids
-- of the transactions on the chain, in every portfolio ('walletTxidsWith').
-- These are what label, show and bumpfee can name by a hash. The shell
-- looks a short id up here too, to offer it only when it names one
-- transaction ("Haskoin.Wallet.Cli.Completion").
walletHashesWith :: (MonadUnliftIO m) => ShortId -> ExceptT String (DB m) [TxHash]
walletHashesWith short = (<>) <$> pendingHashesWith short <*> lift (walletTxidsWith short)

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
pendingTxsAfter :: (MonadUnliftIO m) => Pool -> Maybe (UTCTime, TxHash) -> Maybe Natural -> ExceptT String (DB m) [TxInfo]
pendingTxsAfter pool afterM countM = map snd <$> pendingAfter (Just [pool]) afterM countM

-- | The pending transactions of the pools, or of every pool, newest
-- first, each as a summary with where it is: those after the transaction
-- given, or the first ones, up to the count, or all of them
pendingAfter :: (MonadUnliftIO m) => Maybe [Pool] -> Maybe (UTCTime, TxHash) -> Maybe Natural -> ExceptT String (DB m) [(Place, TxInfo)]
pendingAfter poolsM afterM countM = do
  held <- liftEither . mapM fromRow =<< lift (pendingRows poolsM afterM countM)
  forM held $ \p -> (,) <$> poolPlace p.pendingTxPool <*> pendingSummary p

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
poolKeys :: (MonadUnliftIO m) => Pool -> DB m [(AccountKind, XPubKey)]
poolKeys pool = map (\(_, acc) -> (accountKind acc, accountXPubKey acc)) <$> poolAccounts pool

-- | The transaction of the signing data as the pool sees it: parsed
-- against the keys of its accounts, with its labels as the wallet has
-- them, and the addresses of other portfolios it pays ('fillTxInfo')
txInfoOf :: (MonadUnliftIO m) => Pool -> TxSignData -> ExceptT String (DB m) TxInfo
txInfoOf pool tsd = do
  keys <- lift $ poolKeys pool
  info <- liftEither $ parseTxSignData pool.poolNetwork keys tsd
  lift $ fillTxInfo pool info

-- | The summary of a pending transaction: the transaction as its portfolio
-- sees it, with its status as held
pendingSummary :: (MonadUnliftIO m) => PendingTx -> ExceptT String (DB m) TxInfo
pendingSummary p = do
  info <- txInfoOf p.pendingTxPool p.pendingTxData
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
  held <- pendingOn net
  return $ Set.fromList [input.outpoint | p <- held, input <- (txSignDataTx p.pendingTxData).inputs]

-- | The coins of the wallet on a network that a pending replacement
-- removes once it is sent: those of the transactions the pending
-- transactions replace, whatever their portfolio, while no block holds
-- them.
--
-- A replacement spends the coins of the transaction it replaces, so the
-- network keeps one of the two. Once the replacement is sent, the
-- transaction replaced is gone, with every coin it made: its change, and
-- what it paid another portfolio of the wallet. A payment that spent such
-- a coin would go with it, and its recipient would never be paid. So the
-- replacement holds those coins as it holds the coins it spends, from the
-- moment bumpfee prepares it. deletetx gives them back, and the sync
-- takes them away once the replacement is on the network.
--
-- A coin in a block is left alone. The transaction replaced confirmed
-- after all, so nothing replaces it any more, and the replacement is the
-- one that can no longer be sent ('replacedBy'). Its change must not wait
-- for the user to delete a replacement that came too late.
--
-- Nothing is stored for it: the coins are found from the pending
-- transactions each time, as those they spend are. The coins of a
-- transaction are read from the key of the coins, which starts with the
-- id of the transaction.
replacedOutpoints :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Set OutPoint)
replacedOutpoints net = do
  held <- pendingOn net
  case nubOrd (concatMap (.pendingTxData.txSignDataReplaces) held) of
    [] -> return Set.empty
    replaced -> do
      accounts <- lift $ accountsOnNetwork net
      rows <- lift $ concat <$> mapM (coinsOf accounts) replaced
      coins <- forM rows $ \(P.Entity _ row) -> liftEither $ do
        outpoint <- bytesToOutpoint (dBCoinOutpoint row)
        block <- S.decode (dBCoinBlockRef row)
        return (outpoint, block)
      return $ Set.fromList [outpoint | (outpoint, block) <- coins, not (Store.confirmed block)]
  where
    -- The key of a coin is the id of its transaction, then the index of
    -- its output: the coins of a transaction are those from its first
    -- possible output to its last
    coinsOf accounts tid =
      P.selectList
        [ DBCoinAccount P.<-. accounts,
          DBCoinOutpoint P.>=. outpointBytes (OutPoint tid minBound),
          DBCoinOutpoint P.<=. outpointBytes (OutPoint tid maxBound)
        ]
        []

-- | The pending replacement that holds a coin on a network, if there is
-- one: the one that replaces the transaction the coin comes from
-- ('replacedOutpoints')
pendingReplacer :: (MonadUnliftIO m) => Network -> OutPoint -> ExceptT String (DB m) (Maybe PendingTx)
pendingReplacer net outpoint =
  find ((outpoint.hash `elem`) . (.pendingTxData.txSignDataReplaces)) <$> pendingOn net

-- | The pending transactions of the wallet on a network, of every
-- portfolio
pendingOn :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) [PendingTx]
pendingOn net = liftEither . mapM fromRow =<< lift (P.selectList [DBPendingTxNetwork P.==. cs net.name] [])

-- | The pending transaction that spends a coin on a network, if there is
-- one: the one that locks it
pendingSpender :: (MonadUnliftIO m) => Network -> OutPoint -> ExceptT String (DB m) (Maybe PendingTx)
pendingSpender net outpoint =
  find (any ((== outpoint) . (.outpoint)) . (.inputs) . txSignDataTx . (.pendingTxData)) <$> pendingOn net

-- | A pending transaction as a message names it: by the short id of the
-- hash pendingtxs shows for it, which the commands take
pendingShortId :: PendingTx -> String
pendingShortId p = shortHashText (shownHash (txSignDataTx p.pendingTxData))

-- | The coins of the wallet on a network that the user locked with
-- lockcoin: no pending transaction may spend them, as the builder may not.
-- The coin table is read here, as in 'coinExists', for the import to
-- refuse them.
userLockedOutpoints :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) (Set OutPoint)
userLockedOutpoints net = do
  accounts <- lift $ accountsOnNetwork net
  rows <- lift $ P.selectList [DBCoinAccount P.<-. accounts, DBCoinLockedByUser P.==. True] []
  Set.fromList <$> mapM (liftEither . bytesToOutpoint . dBCoinOutpoint . P.entityVal) rows

-- | The coins of the wallet on a network that a tag locks: those at an
-- address that has a tag that locks its coins, other than the tags
-- given, which a payment opens by naming them with --spend-from-tag.
-- An address with two such tags is opened by a payment that names both:
-- each lock has its own key.
--
-- Nothing is stored for it: the coins are found from the tags of the
-- addresses each time, so a coin that arrived since the tag was made to
-- lock is locked too. The rows of each tag are read from their index,
-- and the coins of each address from the index of the coins by account
-- and address. With no locking tag, which is the usual case, nothing is
-- read but the tags.
tagLockedOutpoints :: (MonadUnliftIO m) => [Tag] -> Network -> ExceptT String (DB m) (Set OutPoint)
tagLockedOutpoints opened net = Map.keysSet <$> tagLocks opened net

-- | The coins 'tagLockedOutpoints' gives, each with the tags that lock
-- it, in the order of the alphabet: what a refusal names
tagLocks :: (MonadUnliftIO m) => [Tag] -> Network -> ExceptT String (DB m) (Map.Map OutPoint [Tag])
tagLocks opened net = do
  closed <- lift $ filter (`notElem` opened) <$> lockingTags
  if null closed
    then return Map.empty
    else do
      accounts <- lift $ accountsOnNetwork net
      rows <- lift . select . from $ \(t `InnerJoin` c) -> do
        on $ c ^. DBCoinAccount ==. t ^. DBAddressTagAccount &&. c ^. DBCoinAddress ==. t ^. DBAddressTagAddress
        where_ $ t ^. DBAddressTagTag `in_` valList (tagText <$> closed) &&. t ^. DBAddressTagAccount `in_` valList accounts
        return (c ^. DBCoinOutpoint, t ^. DBAddressTagTag)
      locks <- forM rows $ \(Value key, Value word) ->
        liftEither $ (,) <$> bytesToOutpoint key <*> ((: []) <$> readTag word)
      return (nubOrd . sort <$> Map.fromListWith (<>) locks)

{- The locks of the user -}

-- | A lock the user put on a coin: on that coin, with lockcoin, or on
-- every coin of a tag, with the tags of the address of the coin that lock
-- their coins
data UserLock = LockOfCoin | LockOfTags ![Tag]
  deriving (Eq, Show)

-- | The locks the user put on the coins of the wallet on a network: the
-- coins lockcoin locked, and the coins a tag locks, each with those tags
data UserLocks = UserLocks
  { locksOfCoins :: !(Set OutPoint),
    locksOfTags :: !(Map.Map OutPoint [Tag])
  }

-- | The locks the user put on the coins of the wallet on a network, read
-- once for every transaction to check against them
userLocks :: (MonadUnliftIO m) => Network -> ExceptT String (DB m) UserLocks
userLocks net = UserLocks <$> userLockedOutpoints net <*> tagLocks [] net

-- | The first coin a transaction spends that a lock of the user holds
-- against it, with that lock, given the tags the payment named with
-- --spend-from-tag. lockcoin holds a coin against every transaction. A
-- tag holds it against a transaction that did not name it, and an
-- address with two tags that lock needs both named.
lockedCoinOf :: UserLocks -> [Tag] -> Tx -> Maybe (OutPoint, UserLock)
lockedCoinOf locks opened tx = listToMaybe (mapMaybe (held . (.outpoint)) tx.inputs)
  where
    held outpoint
      | outpoint `Set.member` locks.locksOfCoins = Just (outpoint, LockOfCoin)
      | closed@(_ : _) <- filter (`notElem` opened) (Map.findWithDefault [] outpoint locks.locksOfTags) = Just (outpoint, LockOfTags closed)
      | otherwise = Nothing

-- | Why a transaction that spends a locked coin is refused: the coin, by
-- its short id, as coins lists it, the lock, and what frees the coin.
-- The lock of lockcoin is named as coins names it, "locked by you".
lockedCoinRefusal :: (OutPoint, UserLock) -> String
lockedCoinRefusal (outpoint, lock) =
  "The transaction spends the coin " <> coin <> ", which " <> case lock of
    LockOfCoin -> "is locked by you: unlockcoin " <> coin <> " unlocks it"
    LockOfTags [tag] ->
      let word = cs (tagText tag)
       in ("the tag " <> word <> " locks. A payment made with --spend-from-tag " <> word)
            <> (" spends it, and hw tag " <> word <> " --unlock-coins unlocks it")
    LockOfTags tags ->
      ("the tags " <> intercalateAnd (cs . tagText <$> tags) <> " lock. A payment that names each of them with")
        <> " --spend-from-tag spends it, and hw tag TAG --unlock-coins unlocks the coins of a tag"
  where
    coin = shortCoinText outpoint

-- | Refuse to sign or to send a transaction that spends a coin of the
-- wallet the user locked, on the network given: signtx and broadcast ask
-- here, for a pending transaction and for a file alike.
--
-- 'insertPendingTx' refuses such a coin when a transaction becomes
-- pending, and that is not enough. A file signed with signtx --input and
-- sent with broadcast --input never becomes pending on this computer. A
-- lock may also come after the transaction: a tag is made to lock its
-- coins, an address takes a tag that locks, or importwallet locks a coin.
-- So the question is asked again where the harm would be done, whatever
-- way the lock came.
--
-- A pending transaction may spend the coins of the tags its payment
-- named, which the wallet kept with it. A file the wallet does not hold
-- names no tag, as importtx reads it. A computer that holds no coin, an
-- offline signer, has no lock to ask about, and signs.
refuseLockedCoins :: (MonadUnliftIO m) => Network -> Tx -> ExceptT String (DB m) ()
refuseLockedCoins net tx = do
  heldM <- lookupPendingTx (nosigHash tx)
  locks <- userLocks net
  forM_ (lockedCoinOf locks (maybe [] (.pendingTxOpened) heldM) tx) (throwError . lockedCoinRefusal)

-- | The pending transactions of the wallet that 'refuseLockedCoins'
-- refuses now, each with the first locked coin it spends. A command that
-- locks coins compares them before and after, to tell the user which
-- transactions it stopped.
stoppedPendingTxs :: (MonadUnliftIO m) => ExceptT String (DB m) [(PendingTx, OutPoint)]
stoppedPendingTxs = do
  held <- liftEither . mapM fromRow =<< lift (pendingRows Nothing Nothing Nothing)
  fmap concat . forM (nubOrdOn (.name) [p.pendingTxPool.poolNetwork | p <- held]) $ \net -> do
    locks <- userLocks net
    return
      [ (p, outpoint)
      | p <- held,
        p.pendingTxPool.poolNetwork == net,
        Just (outpoint, _) <- [lockedCoinOf locks p.pendingTxOpened (txSignDataTx p.pendingTxData)]
      ]

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
changeAddress :: (MonadUnliftIO m) => Config -> Pool -> AccountKind -> ExceptT String (DB m) DBAddress
changeAddress cfg pool kind = do
  (accId, _) <- poolAccountOf pool kind
  reserved <- reservedChangePaths pool
  nextChangeAddress cfg accId [pathInAccount path | path <- reserved, pathKind path == kind]

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
importPendingTx :: (MonadUnliftIO m) => Pool -> TxSignData -> ExceptT String (DB m) TxHash
importPendingTx pool = importPreparedTx pool Nothing []

-- | 'importPendingTx' for a transaction preparetx just built, with the
-- address --change-back named for its change, if any. That address may
-- be used already, or be the change of another pending transaction: the
-- user chose it, and the specification takes any address of the
-- portfolio on the network of the payment. A change address the builder
-- took by itself must still be free ('insertPendingTx'). The tags are
-- those the payment named with --spend-from-tag: it may spend the coins
-- they lock.
importPreparedTx :: (MonadUnliftIO m) => Pool -> Maybe Address -> [Tag] -> TxSignData -> ExceptT String (DB m) TxHash
importPreparedTx pool changeBackM opened tsd = do
  let net = pool.poolNetwork
  accounts <- lift $ poolAccounts pool
  heldM <- lookupPendingTx pendingHash
  forM_ heldM $ \held ->
    when (held.pendingTxPool /= pool) $ do
      place <- poolPlace held.pendingTxPool
      throwError ("The transaction is pending in " <> placeText place)
  let imported = maybe tsd (`heldVersion` tsd) heldM
      keys = [(accountKind acc, accountXPubKey acc) | (_, acc) <- accounts]
  info <- liftEither $ parseTxSignData net keys imported
  liftEither $ checkOutputReach (heldPoolOf (snd <$> accounts)) imported
  when (isSigned tx) $ liftEither $ verifySignedTx net imported
  case heldM of
    Just held -> replaceVersion held imported
    Nothing -> insertPendingTx pool changeBackM opened imported info.txInfoSummary =<< replacedBy pool imported
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
-- It may not spend a coin another pending transaction holds: one that
-- transaction spends, or one of a transaction it replaces
-- ('replacedOutpoints').
--
-- The addresses of the pool that it pays are generated when the wallet
-- does not have them ('generatePaidAddresses').
--
-- Its change addresses must be free: neither used, nor the change of
-- another pending transaction. The address given, the one --change-back
-- named, is the exception: the user chose it ('importPreparedTx').
--
-- It may not spend a coin the user locked, nor one that a tag locks,
-- but for the tags given, those the payment named ('lockedCoinOf'). They
-- are kept with the transaction, for signtx and broadcast to ask again
-- ('refuseLockedCoins'). A replacement keeps the coins of the
-- transaction it replaces, which had them by the same rule.
insertPendingTx :: (MonadUnliftIO m) => Pool -> Maybe Address -> [Tag] -> TxSignData -> TxSummary -> Replaced -> ExceptT String (DB m) ()
insertPendingTx pool changeBackM opened tsd summary replaced = do
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
      unless inWallet $ throwError =<< missingCoinRefusal pool tx outpoint
  -- No other pending transaction, of any portfolio, may spend one of its
  -- inputs; storing it is what locks them
  let heldByReplaced = Set.fromList [input.outpoint | p <- replaced.replacedRows, input <- (txSignDataTx p.pendingTxData).inputs]
      pathsOfReplaced = [path | p <- replaced.replacedRows, path <- p.pendingTxData.txSignDataOutputPaths]
  locked <- (`Set.difference` heldByReplaced) <$> lockedOutpoints net
  -- Nor a coin that another pending transaction removes once it is sent,
  -- by replacing the transaction the coin comes from
  goingAway <- replacedOutpoints net
  when (any ((`Set.member` (locked <> goingAway)) . (.outpoint)) tx.inputs) $
    throwError "A coin referenced by the transaction is locked"
  -- Nor a coin the user locked, or one a tag locks, unless the payment
  -- named the tag. A sweep and a transaction made elsewhere name none.
  locks <- userLocks net
  forM_ (lockedCoinOf locks {locksOfTags = Map.withoutKeys locks.locksOfTags heldByReplaced} opened tx) $
    throwError . lockedCoinRefusal
  -- A replacement keeps open what the transactions it replaces opened,
  -- and the tags that lock the coins it takes over from those that were
  -- sent. No lock stops a transaction that was sent, so its fee can still
  -- be raised. One that was not sent is stopped, and so is what replaces
  -- it.
  let sentByReplaced = Set.fromList [input.outpoint | p <- replaced.replacedRows, p.pendingTxOnline, input <- (txSignDataTx p.pendingTxData).inputs]
      takenOver = Map.restrictKeys locks.locksOfTags (sentByReplaced `Set.intersection` Set.fromList ((.outpoint) <$> tx.inputs))
      keptOpen = nubOrd (opened <> concatMap (.pendingTxOpened) replaced.replacedRows <> concat (Map.elems takenOver))
  -- The addresses it pays are generated when the wallet lacks them, as
  -- for a payment prepared on another computer of the wallet. Its change
  -- addresses must be neither used nor paid by another pending
  -- transaction, but the one --change-back named.
  generatePaidAddresses pool summary
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
  lift $ P.insert_ $ DBPendingTx pool.poolPortfolio (poolNetworkName pool) (hashBytes (nosigHash tx)) (blob tsd) False False time (tagsText keptOpen)

-- | Why a transaction cannot become pending in the pool, for a coin of the
-- pool it spends that the pool does not hold, with what to do next. The
-- history of the pool tells four cases apart.
--
-- * The history has the transaction itself. The network has it already,
--   as when the file of a payment is imported after the payment was sent
--   and a sync has seen it. There is nothing to import.
--
-- * The history has the transaction the coin comes from, and a
--   transaction that spent the coin since ('liveSpenders'). The refusal
--   names that one. It may be this very payment, signed and sent since
--   the file was written: an unsigned file of a legacy payment has
--   another id than the payment the network has.
--
-- * The history has the transaction the coin comes from, and the network
--   dropped it, as when its sender replaced it. The coin never came to be.
--
-- * The history knows nothing of the coin. The wallet has not seen it
--   yet, and a sync brings it. On a computer that stays offline no sync
--   does, so the refusal names the command that needs no coin there.
--
-- Before, one sentence answered all four, \"A coin referenced by the
-- transaction does not exist\", which was wrong for a coin that exists and
-- is spent, and gave no next step. A coin the history knows is named by
-- its short id, which show takes. A coin it does not know is named in
-- full, since a short id would name nothing here.
missingCoinRefusal :: (MonadUnliftIO m) => Pool -> Tx -> OutPoint -> ExceptT String (DB m) String
missingCoinRefusal pool tx outpoint = do
  sentM <- lift $ P.get (txInfoKey pool (txHash tx))
  parentM <- lift $ P.get (txInfoKey pool outpoint.hash)
  case (sentM, parentM) of
    (Just sent, _) | not (dBTxInfoDead sent) -> do
      place <- poolPlace pool
      return $
        ("The network has this transaction already: it is in the history of " <> placeText place)
          <> (", and there is nothing to import. hw show " <> shortHashText (txHash tx) <> " shows it")
    (_, Just parent)
      | dBTxInfoDead parent ->
          return $
            (spends (shortCoinText outpoint) <> ", but the network dropped the transaction that made this coin, so the coin does not exist. ")
              <> ("hw show " <> shortHashText outpoint.hash <> " shows that transaction")
      | otherwise -> do
          spenders <- liveSpenders pool (dBTxInfoStanding parent) (Set.singleton outpoint)
          return $ case spenders of
            spender : _ ->
              (spends (shortCoinText outpoint) <> ", which the transaction " <> shortHashText spender <> " spent already. ")
                <> ("hw show " <> shortHashText spender <> " shows it")
            [] -> notHeld
    _ -> return notHeld
  where
    spends coin = "The transaction spends the coin " <> coin
    notHeld =
      (spends (cs (txHashToHex outpoint.hash) <> ":" <> show outpoint.index) <> ", which the wallet does not have. ")
        <> "hw sync finds the coins received since the last sync: run it, then import the file again. "
        <> "A computer that stays offline holds no coins, and importtx cannot work there: sign the file with hw signtx --input instead"

-- | Generate the addresses of the pool that a transaction pays and that
-- the wallet does not have yet, each with its chain up to it.
--
-- A payment takes a new change address. Prepared on another computer of
-- the wallet, it pays one that this computer has not generated: a sync
-- generates the addresses the chain has used, and nothing has paid this
-- one yet. Its file gives the path of the address, and the summary was
-- read with the keys of the pool, so the address is the pool's
-- ('parseTxSignData'). Before, such a file was refused, and so was every
-- file of preparetx on a second computer.
--
-- The address needs its row, and the path the transaction keeps is not
-- enough. The next payment looks for a free change address among the
-- rows, where it leaves out those the pending transactions hold, and
-- then generates the next index without a look at them
-- ('nextChangeAddress'). Without the row, it would take the same
-- address.
--
-- An address further than a thousand past the last used one of its
-- chain is refused, as importwallet leaves it ('withinScanLimit'). The
-- limit of signtx does not do here ('checkOutputReach'): it counts from
-- the last address generated, which this function moves, so each file
-- could push the chain a thousand addresses further.
generatePaidAddresses :: (MonadUnliftIO m) => Pool -> TxSummary -> ExceptT String (DB m) ()
generatePaidAddresses pool summary = do
  accounts <- lift $ poolAccounts pool
  forM_ (myOutputsPath <$> Map.elems summary.summaryMyOutputs) $ \path -> do
    let accountM = find ((== path.pathKind) . accountKind . snd) accounts
    forM_ ((,) <$> accountM <*> chainIndex path.pathInAccount) $ \((accId, _), (addrType, index)) -> do
      generated <- lift $ isJust <$> P.get (DBAddressKey accId (isInternal addrType) (fromIntegral index))
      unless generated $ do
        within <- lift $ withinScanLimit accId addrType index
        unless within $ throwError (tooFarToGenerate path)
        extendChainTo accId addrType (fromIntegral index + 1)

-- | The refusal of a transaction that pays an address of the pool which
-- the wallet has not generated and does not generate on the word of a
-- file ('generatePaidAddresses'). A sync moves the limit, when the chain
-- has used addresses since the last one.
tooFarToGenerate :: PoolPath -> String
tooFarToGenerate path =
  "The transaction pays the address at "
    <> cs (poolPathText path)
    <> ", which the wallet has not generated and which lies over a thousand past the last used address of its chain, further than hw generates on the word of a file. \
       \hw sync finds the addresses used since: run it, then import the file again"

{- The tags of the change -}

-- | Give the change addresses of a pending transaction the tags of the
-- group its coins are of: the tags that every coin it spends has. A
-- payment does this when the user asks for it, with --tag-change.
--
-- A tag names a group of addresses, such as those of the coins bought at
-- an exchange. The change of a payment made with coins of the group alone
-- is of the group too. Without this, it leaves the group: `balances
-- --tag` loses it, and --spend-from-tag does not find it. The other way
-- to keep it in, sending the change back to a tagged address, uses an
-- address twice, which tells an observer which output is the change.
--
-- It is asked for, and never done by itself. A tag may say something of
-- an address and not of its coins, and only the user knows which: the
-- change of a coin received at an address tagged donations is no
-- donation. A payment that tagged addresses the user did not name,
-- unasked, would also surprise.
--
-- The tags are those every coin shares, not those any coin has. A payment
-- that spends a coin from outside a group mixes it with the group, and
-- its change cannot be told to belong to it. So its change takes no tag
-- of that group.
--
-- A change address takes the tags in place of any it has
-- ("Haskoin.Wallet.Database.Tag".'setAddressTags'). They are kept on the
-- address like those the command tag puts, so every listing and filter
-- finds them, and the command untag takes them off. 'removePendingTx' takes
-- them off again. The same rule, for the payments made before an address
-- was tagged, is in "Haskoin.Wallet.Database.ChangeChain".
changeTakesTags :: (MonadUnliftIO m) => Pool -> TxSummary -> DB m ()
changeTakesTags pool summary = do
  shared <- spentTags pool summary
  forM_ (touchedBy summary).changeAddresses $ \addr ->
    mapM_ (`setAddressTags` shared) =<< poolAddressRow pool addr

-- | The tags every coin a transaction spends has: those that all the
-- addresses of the pool it spends from share. A transaction that also
-- spends a coin of others shares none, since that coin has no tag.
spentTags :: (MonadUnliftIO m) => Pool -> TxSummary -> DB m [Tag]
spentTags pool summary
  | Map.null summary.summaryOtherInputs = sharedTagsAt pool (Map.keys summary.summaryMyInputs)
  | otherwise = return []

-- | The tags that the addresses share, which must all be of the pool:
-- none otherwise
sharedTagsAt :: (MonadUnliftIO m) => Pool -> [Address] -> DB m [Tag]
sharedTagsAt pool addrs = maybe (return []) sharedAddressTags . sequence =<< mapM (poolAddressRow pool) addrs

-- | Take the tags off the change addresses a deleted pending transaction
-- gives back: those that no transaction has used and that no other
-- pending transaction pays. Such an address is the next one a payment
-- takes for its change ('changeAddress'), and the tags of the coins of
-- the deleted payment would say something wrong of that one. Every tag
-- goes, those 'changeTakesTags' gave and those the user put: the address
-- is new again.
-- An address that is still held, as the change address a replacement
-- shares with the transaction it replaces, keeps its tags.
untagFreedChange :: (MonadUnliftIO m) => Pool -> Touched -> ExceptT String (DB m) ()
untagFreedChange pool touched = do
  reserved <- reservedChangePaths pool
  forM_ (zip touched.changeAddresses touched.changePaths) $ \(addr, path) ->
    unless (path `elem` reserved) $ do
      rowM <- lift $ poolAddressRow pool addr
      forM_ rowM $ \row -> when (dBAddressBalanceTxs row == 0) $ lift (clearAddressTags row)

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
replacedBy :: (MonadUnliftIO m) => Pool -> TxSignData -> ExceptT String (DB m) Replaced
replacedBy pool tsd = do
  keys <- lift $ poolKeys pool
  held <- pendingTxs pool
  found <- forM tsd.txSignDataReplaces $ \tid ->
    case [p | p <- held, txHash (txSignDataTx p.pendingTxData) == tid] of
      p : _ -> do
        info <- liftEither $ parseTxSignData pool.poolNetwork keys p.pendingTxData
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
-- the change addresses it paid are available again by its absence, and
-- those it gives back lose their tags ('untagFreedChange'). Returns
-- how many coins of the wallet, of any portfolio, it was holding and how
-- many change addresses it paid.
removePendingTx :: (MonadUnliftIO m) => PendingTx -> ExceptT String (DB m) (Natural, Natural)
removePendingTx held = do
  let pool = held.pendingTxPool
  keys <- lift $ poolKeys pool
  txInfo <- liftEither $ parseTxSignData pool.poolNetwork keys held.pendingTxData
  let touched = touchedBy txInfo.txInfoSummary
      spent = (.outpoint) <$> (txSignDataTx held.pendingTxData).inputs
  heldCoins <- lift $ filterM (coinExists pool.poolNetwork) spent
  lift $ P.delete held.pendingTxKey
  -- Its tags go with it: nothing else is tagged under its nosigHash
  lift $ deleteTxTags pool (nosigHash (txSignDataTx held.pendingTxData))
  untagFreedChange pool touched
  return (fromIntegral (length heldCoins), fromIntegral (length touched.changeAddresses))

-- | Retire a pending transaction the network has under the id given, as
-- the sync does: the row goes, and its label, if any, goes to the
-- transaction of that id in the pool. The id is that of its signed
-- version, unless the network has it signed otherwise. Its coins are gone,
-- or going with the sync, and its change address was used on the chain,
-- so it stays busy. Its tags, kept under its nosigHash while it was
-- pending, go to the transaction of that id too
-- ('Haskoin.Wallet.Database.Tag.moveTxTags').
retirePendingTx :: (MonadUnliftIO m) => PendingTx -> TxHash -> DB m ()
retirePendingTx p networkId = do
  let label = p.pendingTxData.txSignDataLabel
  unless (Text.null label) $ setTxLabel p.pendingTxPool networkId label
  moveTxTags p.pendingTxPool (nosigHash (txSignDataTx p.pendingTxData)) networkId
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
labelTransaction :: (MonadUnliftIO m) => TxHash -> Text -> ExceptT String (DB m) [(Place, TxInfo)]
labelTransaction named label = do
  pendingM <- findPendingTx named
  case pendingM of
    Just held -> do
      lift $ setPendingTxLabel held label
      place <- poolPlace held.pendingTxPool
      info <- pendingSummary held {pendingTxData = held.pendingTxData {txSignDataLabel = label}}
      return [(place, info)]
    Nothing -> do
      found <- inHistories named
      forM found $ \(pool, place, info) -> do
        lift $ setTxLabel pool named label
        return (place, info {txInfoLabel = label})

-- | Put tags on a transaction of the wallet, besides those it has, or
-- take tags off it. The transaction is found as 'labelTransaction' finds
-- it. A pending one is named by either of its hashes, and is tagged under
-- its nosigHash, in its portfolio. One on the network is named by its id,
-- and is tagged in every portfolio whose history holds it. A tag says
-- what the payment is, which is the same at both ends of a payment
-- between two portfolios. Returns the transaction as each portfolio sees
-- it now.
--
-- A tag taken off must be one the transaction has, in one of the
-- portfolios at least. Otherwise the user most likely named another
-- transaction or mistyped the tag, and is told so.
retagTransaction :: (MonadUnliftIO m) => Retag -> TxHash -> [Tag] -> ExceptT String (DB m) [(Place, TxInfo)]
retagTransaction retag named tags = do
  pendingM <- findPendingTx named
  case pendingM of
    Just held -> do
      let pool = held.pendingTxPool
          taggedUnder = nosigHash (txSignDataTx held.pendingTxData)
      missing <- lift $ change pool taggedUnder
      refuseTagsNowhere named tags [missing]
      place <- poolPlace pool
      info <- pendingSummary held
      return [(place, info)]
    Nothing -> do
      found <- inHistories named
      missing <- forM found $ \(pool, _, _) -> lift (change pool named)
      refuseTagsNowhere named tags missing
      -- Read again, with the tags as they are now
      forM found $ \(pool, place, info) -> (,) place <$> lift (fillTxInfoTags pool info)
  where
    -- The tags asked for that the transaction of the pool did not have,
    -- when they are taken off
    change pool hash = case retag of
      AddTags -> [] <$ tagTx pool hash tags
      RemoveTags -> untagTx pool hash tags

-- | Refuse the tags to take off a transaction that it has in no
-- portfolio, given the tags each portfolio that holds it lacked
refuseTagsNowhere :: (Monad m) => TxHash -> [Tag] -> [[Tag]] -> ExceptT String m ()
refuseTagsNowhere named tags lackedIn =
  case [tag | tag <- tags, all (tag `elem`) lackedIn] of
    [] -> return ()
    nowhere -> throwError ("The transaction " <> cs (txHashToHex named) <> " does not have " <> tagsNamed nowhere)

-- | Whether a command puts tags on a thing or takes them off
data Retag = AddTags | RemoveTags
  deriving (Eq, Show)

-- | Tags as a refusal names them: "the tag kyc", "the tags kyc cold"
tagsNamed :: [Tag] -> String
tagsNamed tags = (if length tags == 1 then "the tag " else "the tags ") <> cs (tagsText tags)

-- | A transaction of the wallet as each portfolio that holds it sees it,
-- found as 'labelTransaction' finds it, with nothing changed: what show
-- answers. A pending transaction is named by either of its hashes, and is
-- of one portfolio. A transaction on the network is named by its id, and
-- is in every portfolio whose history holds it, with the amounts of each.
transactionViews :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) [(Pool, Place, TxInfo)]
transactionViews named = do
  pendingM <- findPendingTx named
  case pendingM of
    Just held -> do
      place <- poolPlace held.pendingTxPool
      info <- pendingSummary held
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
sendPending :: (MonadUnliftIO m) => (Network -> Store) -> BeforeSending m -> TxHash -> ExceptT String (DB m) (Place, TxInfo)
sendPending storeOn beforeSending named = do
  held <- requirePendingTx named
  let pool = held.pendingTxPool
      net = pool.poolNetwork
      signedTx = txSignDataTx held.pendingTxData
      store = storeOn net
  place <- poolPlace pool
  unless (isSigned signedTx) $ throwError "The transaction is not signed"
  -- A replacement may no longer replace anything: a transaction it
  -- replaces confirmed
  replaced <- replacedBy pool held.pendingTxData
  beforeSending net (Just place) =<< pendingSummary held
  -- The summary as it will read once sent, made before anything leaves
  summary <- pendingSummary held {pendingTxOnline = True}
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
-- default network of the configuration then decides.
--
-- Only the pools on the networks hw shows are tried, so a hidden network
-- is never one of several to tell apart. A transaction that is pending on
-- a hidden network, whose file names one, or that involves pools of
-- hidden networks alone, is refused with the way to show the network
-- ('onHiddenNetworks'). Read as outside the wallet, it would be signed or
-- sent as a transaction of somebody else.
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
  -- | The configuration: its default network, and the networks hw shows
  Config ->
  -- | The active portfolio, when the database has one
  Maybe DBPortfolioId ->
  -- | The network of the file, or of --network
  Maybe Network ->
  TxSignData ->
  ExceptT String (DB m) (Maybe Pool, TxSignData)
transactionPool cfg activeM netM tsd = do
  forM_ netM $ \net -> unless (isShown cfg net) $ throwError (onHiddenNetworks "The transaction" [net])
  heldM <- lookupPendingTx (nosigHash (txSignDataTx tsd))
  case heldM of
    Just held -> do
      let pool = held.pendingTxPool
      unless (isShown cfg pool.poolNetwork) $
        throwError (onHiddenNetworks "The transaction" [pool.poolNetwork])
      unless (onTxNetwork pool) $
        throwError "The transaction is pending on another network"
      reachedPool pool =<< withWalletPaths pool tsd
    Nothing -> do
      pools <- candidatePools
      parsed <- forM pools $ \pool -> (pool,) <$> involvement pool tsd
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
          hidden <- involvedOnHidden
          unless (null hidden) $ throwError (onHiddenNetworks "The transaction" hidden)
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
    fallback = configDefaultNetwork cfg
    onTxNetwork :: Pool -> Bool
    onTxNetwork pool = maybe True (== pool.poolNetwork) netM
    -- The pools of every portfolio on the networks hw shows
    shownPools = onShownNetworks cfg (.poolNetwork) <$> lift allPools
    -- The pools to try: the shown ones, on the network of the transaction
    -- when it is known
    candidatePools = filter onTxNetwork <$> shownPools
    -- Whether the transaction involves a pool on another network than the
    -- one named, when one is named: the pools the candidates left out
    involvedElsewhere = case netM of
      Nothing -> return False
      Just _ -> do
        pools <- shownPools
        fmap or . forM (filter (not . onTxNetwork) pools) $ \pool -> isJust <$> involvement pool tsd
    -- The hidden networks the transaction involves a pool on, when it
    -- names no network: it is then no transaction of somebody else
    involvedOnHidden = case netM of
      Just _ -> return []
      Nothing -> do
        pools <- filter (not . isShown cfg . (.poolNetwork)) <$> lift allPools
        involving <- filterM (\pool -> isJust <$> involvement pool tsd) pools
        return (nubOrdOn (.name) ((.poolNetwork) <$> involving))
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
involvement :: (MonadUnliftIO m) => Pool -> TxSignData -> ExceptT String (DB m) (Maybe (TxSignData, TxSummary))
involvement pool tsd = do
  keys <- lift $ poolKeys pool
  let readWith signing = eitherToMaybe (txInfoSummary <$> parseTxSignData pool.poolNetwork keys signing)
      involving signing = [(signing, summary) | Just summary <- [readWith signing], involvesWallet summary]
  withFilePaths <- withWalletPaths pool tsd
  case readWith withFilePaths of
    Just _ -> return (listToMaybe (involving withFilePaths))
    Nothing -> do
      withOwnPaths <- withWalletPaths pool tsd {txSignDataInputPaths = [], txSignDataOutputPaths = []}
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
withWalletPaths :: (MonadUnliftIO m) => Pool -> TxSignData -> ExceptT String (DB m) TxSignData
withWalletPaths pool tsd = do
  let net = pool.poolNetwork
      spent = either (const []) (map (.address)) (spentCoins net tsd)
      paid = [addr | out <- tsd.txSignDataTx.outputs, Right script <- [decodeSerial out.script], Just addr <- [outputAddress net script]]
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
-- for when something names it. One the wallet holds as pending, the file
-- an offline signer returns, takes the place of the version held, as
-- 'heldVersion' merges it, and is sent like one sent by its hash: marked
-- online, its raw transaction stored. One the wallet does not hold is
-- checked against the coins it spends and handed to the store of its
-- network, and nothing in the wallet is updated. Either one is refused
-- when it spends a coin of the wallet the user locked
-- ('refuseLockedCoins'), before anything is asked.
--
-- A transaction the wallet does not hold may still be of one of its
-- portfolios: a payment prepared on another computer, or one deleted
-- here after its file was written. hw finds that portfolio as show and
-- signtx do ('transactionPool', given the configuration and the active
-- portfolio), shows the transaction as the portfolio sees it, and sends
-- it on the network of the portfolio. Read as a transaction of nobody,
-- the user's own payment was shown as spending nothing. Only one of no
-- portfolio, a sweep of outside coins for instance, is shown outside the
-- wallet, and goes on the network the file names, or on the default
-- network of the configuration when it names none.
sendSigned :: (MonadUnliftIO m) => Config -> (Network -> Store) -> BeforeSending m -> Maybe DBPortfolioId -> Maybe Network -> TxSignData -> ExceptT String (DB m) Sent
sendSigned cfg storeOn beforeSending activeM netM tsd = do
  let signedTx = txSignDataTx tsd
      pendingHash = nosigHash signedTx
  unless (isSigned signedTx) $ throwError "The transaction is not signed"
  heldM <- lookupPendingTx pendingHash
  case heldM of
    Just held -> do
      let pool = held.pendingTxPool
      forM_ netM $ \given ->
        when (given /= pool.poolNetwork) $ throwError "The transaction is pending on another network"
      refuseLockedCoins pool.poolNetwork signedTx
      -- A label that differs is no new version: the label held stays
      let version = heldVersion held tsd
      when (version {txSignDataLabel = held.pendingTxData.txSignDataLabel} /= held.pendingTxData) $
        void $ importPendingTx pool tsd
      uncurry SentPending <$> sendPending storeOn beforeSending pendingHash
    Nothing -> do
      (poolM, completed) <- transactionPool cfg activeM netM tsd
      let net = maybe (commandNetwork cfg netM) (.poolNetwork) poolM
      liftEither $ verifySignedTx net tsd
      refuseLockedCoins net signedTx
      case poolM of
        Just pool -> do
          place <- poolPlace pool
          beforeSending net (Just place) =<< txInfoOf pool completed
        Nothing -> beforeSending net Nothing =<< liftEither (parseOutsideTxSignData net tsd)
      fromStore $ checkHealth (storeOn net)
      accepted <- fromStore $ broadcast (storeOn net) signedTx
      unless accepted $ throwError notAccepted
      return $ SentOutside net (txHash signedTx)

-- | Delete a pending transaction, named by either of its hashes, freeing
-- the coins it held. One the user sent is abandoned only when the store
-- does not know it: a transaction in the network's mempool or in the chain
-- may still confirm, and the sync retires it once it is listed.
deleteTx :: (MonadUnliftIO m) => (Network -> Store) -> TxHash -> ExceptT String (DB m) (Natural, Natural)
deleteTx storeOn = deleteTxAfter storeOn (const (return ()))

-- | Delete a pending transaction as 'deleteTx' does, after an action of
-- the caller, which is given the transaction and stops the deletion by
-- failing. deletetx asks its question there.
--
-- The action runs once the transaction may be deleted, and before
-- anything is deleted. So a transaction the store still knows is refused
-- without a question, and the store is asked only once.
deleteTxAfter :: (MonadUnliftIO m) => (Network -> Store) -> (PendingTx -> ExceptT String (DB m) ()) -> TxHash -> ExceptT String (DB m) (Natural, Natural)
deleteTxAfter storeOn beforeDeleting named = do
  held <- requirePendingTx named
  let txid = txHash (txSignDataTx held.pendingTxData)
  when held.pendingTxOnline $ do
    known <- fromStore $ storeTransactions (storeOn held.pendingTxPool.poolNetwork) [txid]
    when (any (\t -> t.txid == txid && not t.deleted) known) $
      throwError "The transaction is in the network's mempool or in the chain: sync retires it once it is listed"
  beforeDeleting held
  freed <- removePendingTx held
  -- The raw transaction stored when it was sent goes with it, unless a
  -- coin of the wallet comes from it after all
  when held.pendingTxOnline $ void $ lift $ dropRawTxsWithoutCoins [txid]
  return freed
