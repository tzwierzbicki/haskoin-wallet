{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | The chain of change below an address: the change addresses of the
-- payments that spend from it, then those of the payments that spend
-- that change, and so on down to the coins the portfolio holds now.
--
-- = Why tags go down the chain
--
-- A tag on an address names the group its coins are of, such as the
-- coins bought at an exchange. A coin of the group does not stay at the
-- address that received it: the first payment spends it, and what is
-- left goes to a change address, and from there to the next one. A new
-- payment keeps its change in the group with --tag-change
-- ('Haskoin.Wallet.Database.PendingTx.changeTakesTags'). That covers
-- the payments made after the address was tagged, and no other. A user
-- most often tags an address once it has a history: the coins of the
-- group are then at change addresses that have no tag, `balances --tag`
-- counts none of them, and --spend-from-tag finds none. So tag and untag
-- do down the chain what they do to the address when they are asked to,
-- with --with-change ('retagChangeChain'), as if the tag had been there
-- when each payment was made.
--
-- They do it when asked only. A tag may say something of one address
-- and not of its coins, such as what the address receives, and a command
-- that changed addresses the user did not name, unasked, would surprise.
--
-- = Why down and not up
--
-- A tag goes from the coins a payment spends to its change, never from
-- the change to the coins it came from. What is left of a kyc coin is
-- kyc, whatever the user does with it. The other way holds nothing true:
-- a change address tagged cold says where its coin is kept, and nothing
-- of the addresses that were spent to make it. A payment may also spend
-- coins of several addresses, and a tag on its change would then go up
-- to all of them, those of other groups too.
--
-- = The rule
--
-- It is the rule of --tag-change ("Haskoin.Wallet.Database.PendingTx").
-- A change address takes a tag when every coin of the payment that pays
-- it is at an address that has the tag. A payment that mixes the group
-- with a coin from outside ends the tag there: its change cannot be told
-- to belong to the group. A change address that several
-- payments pay holds a coin of each, so the coins of all of them count
-- (@linkSources@).
--
-- The addresses the coins came from may be change addresses of the chain
-- themselves, so the rule is read of the whole chain at once (@mayTake@):
-- the change addresses that take the tag are the largest set of them
-- where the coins of each came from addresses that have the tag or are
-- in the set. Read one address at a time, it would fail where the
-- addresses lead back to one another. --change-back sends change to an
-- address that has coins, which may be the address the payment spends
-- from. That address then holds a coin that came from itself, and it
-- would wait for its own tag for ever.
--
-- Taking a tag off needs no rule. Every change address below the address
-- holds what is left of a coin that has lost the tag, so each of them
-- loses it. That is what takes a tag put by mistake off the whole chain
-- at once.
--
-- = The whole chain, every time
--
-- The walk visits every change address below the one it starts from,
-- whatever tags they have (@changeChain@). So what it leaves is said in
-- one sentence. After tag, every change address below that the rule
-- allows has the tag. After untag, none below has it. A chain tagged in
-- part, as when some of its payments had --tag-change and others not, is
-- tagged whole.
--
-- The walk once stopped at a change address where nothing changed: one
-- that had the tag already, or that had lost it already. That kept a tag
-- the user had taken off by hand further down, but only when a change
-- address above it still had the tag, and it left the rest of a chain
-- tagged in part without the tag. What the command did depended on tags
-- the user could not see from where they ran it, so it was given up. A
-- tag taken off by hand is put back by the next tag --with-change above
-- it: the option asks for that.
--
-- The addresses already visited are remembered, and each is gone below
-- from once. That ends the walk where the addresses lead back to one
-- another.
--
-- = What is not stored
--
-- Nothing says that a tag came down the chain. It is a tag like those
-- the user puts, in the same rows, so every listing and filter finds it,
-- and untag takes it off. The other choice was to work the tags of a
-- change address out of its payments each time they are read. A filter
-- by tag would then follow chains for every row it looks at, and the
-- user could not take one of those tags off.
module Haskoin.Wallet.Database.ChangeChain
  ( ChainRetagged (..),
    ChainStop (..),
    retagChangeChain,
    txidsAtQuery,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (filterM, forM, forM_, unless)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT, gets, modify)
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.List (groupBy, intersect, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Database.Esqueleto.Legacy (SqlExpr, SqlQuery, Value (..), from, select, val, where_, (&&.), (==.), (^.))
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Address (fromDBAddr, poolAddressRow)
import Haskoin.Wallet.Database.PendingTx (PendingTx (..), Retag (..), Touched (..), pendingTxs, poolKeys, touchedBy)
import Haskoin.Wallet.Database.Tag (addressTags, tagAddress, untagAddress)
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..), liveSummary)
import Haskoin.Wallet.Tags (Tag, shownTag)
import Haskoin.Wallet.Tx.SignData (TxSignData (..), shownHash)
import Haskoin.Wallet.Tx.Signing (parseTxSignData)

-- | What tag or untag did down the chain of change of an address
data ChainRetagged = ChainRetagged
  { -- | The change addresses that took tags or lost some, in the order
    -- of their paths
    chainChanged :: ![DBAddress],
    -- | Where the tags stopped on their way down, in the order of the
    -- paths: none for untag, which stops nowhere
    chainStops :: ![ChainStop]
  }

-- | A change address where tags stopped on their way down a chain: one
-- that the coins of the group reached, and that could not take the tags
-- because it also holds a coin from outside the group.
--
-- The rule is right to stop there, and the user must still be told. The
-- coins that came from the tagged address go on below the stop without
-- the tag, and the answer of tag would otherwise look like a chain
-- tagged whole. To take the tags further, the user tags the address the
-- other coin came from, and that address is what they could only find
-- by reading the payment by hand. So a stop names the payment and that
-- address.
data ChainStop = ChainStop
  { -- | The change address that did not take the tags
    stopAt :: !DBAddress,
    -- | The tags it did not take: one at least
    stopTags :: ![Tag],
    -- | The payment that pays it with a coin from outside the group, by
    -- the hash the listings show for it
    stopPayment :: !TxHash,
    -- | The address of the pool that payment spends from and that does
    -- not have the tags. Nothing when the coin is not of the pool: it has
    -- no address to tag.
    stopSource :: !(Maybe DBAddress)
  }
  deriving (Eq, Show)

-- | Do down the chain of change of an address what tag or untag just did
-- to the address: put the tags on every change address below it that may
-- take them (@mayTake@), or take them off every change address below it.
-- Answers the change addresses it changed, and where the tags stopped.
-- The address itself is tagged or untagged by the caller, before.
retagChangeChain :: (MonadUnliftIO m) => Retag -> Pool -> DBAddress -> [Tag] -> ExceptT String (DB m) ChainRetagged
retagChangeChain retag pool top tags = do
  chain <- changeChain pool top
  case retag of
    AddTags -> tagChain pool chain tags
    RemoveTags -> do
      changed <- lift . flip filterM (linkRow <$> Map.elems chain) $ \row -> do
        had <- intersect tags . map shownTag <$> addressTags row
        not (null had) <$ untagAddress row had
      return (ChainRetagged (sortOn rowKey changed) [])

-- | What orders the addresses of an answer: their account, then their path
rowKey :: DBAddress -> (DBAccountId, Bool, Int)
rowKey row = (dBAddressAccount row, dBAddressInternal row, dBAddressIndex row)

-- | Put on each change address of a chain those of the tags that it may
-- take and does not have. Answers the addresses that took one, and where
-- each tag stopped (@stopsOf@).
tagChain :: (MonadUnliftIO m) => Pool -> Map Address Link -> [Tag] -> ExceptT String (DB m) ChainRetagged
tagChain pool chain tags = lift $ do
  let sources = concatMap (concatMap (.payerSources) . (.linkPayers)) (Map.elems chain)
  known <- fmap (Map.fromList . catMaybes) . forM (nubOrd (Map.keys chain <> sources)) $ \addr ->
    fmap (addr,) <$> poolAddressRow pool addr
  held <- mapM (fmap (map shownTag) . addressTags) known
  let holders tag = Map.keysSet (Map.filter (tag `elem`) held)
      -- The addresses of the group of each tag once the command is done:
      -- those that have the tag, and those of the chain that take it
      groups = [(tag, holders tag, mayTake chain (holders tag)) | tag <- tags]
      takers = [(tag, taking `Set.difference` has) | (tag, has, taking) <- groups]
  changed <- fmap catMaybes . forM (Map.toList chain) $ \(addr, link) ->
    case [tag | (tag, set) <- takers, addr `Set.member` set] of
      [] -> return Nothing
      new -> Just link.linkRow <$ tagAddress link.linkRow new
  let stops = concat [stopsOf known chain tag (has `Set.union` taking) | (tag, has, taking) <- groups]
  return (ChainRetagged (sortOn rowKey changed) (mergedStops stops))

-- | Where a tag stopped on its way down a chain, given the addresses of
-- its group: each change address outside the group that a coin of the
-- group reached, with the first payment that pays it from outside the
-- group, and the first address that payment spends from that is not of
-- the group. A change address further down, which no coin of the group
-- reached as far as the tags tell, is no stop: the chain stopped above.
stopsOf :: Map Address DBAddress -> Map Address Link -> Tag -> Set Address -> [ChainStop]
stopsOf known chain tag group =
  [ ChainStop link.linkRow [tag] payer.payerHash outsider
    | (addr, link) <- Map.toList chain,
      addr `Set.notMember` group,
      any (any (`Set.member` group) . (.payerSources)) link.linkPayers,
      (payer, outsider) <- take 1 (mapMaybe fromOutside link.linkPayers)
  ]
  where
    -- A payment with a coin from outside the group, and the address of
    -- that coin when the pool has it
    fromOutside :: Payer -> Maybe (Payer, Maybe DBAddress)
    fromOutside payer =
      case filter (`Set.notMember` group) payer.payerSources of
        source : _ -> Just (payer, Map.lookup source known)
        [] | payer.payerOutside -> Just (payer, Nothing)
        [] -> Nothing

-- | The stops of several tags, those of the same payment at the same
-- change address told once with all their tags, in the order of the
-- paths
mergedStops :: [ChainStop] -> [ChainStop]
mergedStops stops =
  [ first {stopTags = nubOrd (concatMap (.stopTags) same)}
    | same@(first : _) <- groupBy ((==) `on` place) (sortOn place stops)
  ]
  where
    place :: ChainStop -> ((DBAccountId, Bool, Int), TxHash, Maybe (DBAccountId, Bool, Int))
    place stop = (rowKey stop.stopAt, stop.stopPayment, rowKey <$> stop.stopSource)

-- | A change address of the chain below an address
data Link = Link
  { linkRow :: !DBAddress,
    -- | The payments that pay it: its coins came from the addresses they
    -- spend from
    linkPayers :: ![Payer]
  }

-- | A payment that pays a change address of a chain
data Payer = Payer
  { -- | The hash the listings show for it: its id, or the hash of a
    -- pending transaction
    payerHash :: !TxHash,
    -- | The addresses of the pool it spends from
    payerSources :: ![Address],
    -- | Whether it also spends a coin that is not of the pool, which has
    -- no tag
    payerOutside :: !Bool
  }

-- | The addresses the coins of a change address came from: those that
-- the payments that pay it spend from. Nothing when one of those
-- payments also spends a coin that is not of the pool, which has no tag,
-- so the address takes none.
linkSources :: Link -> Maybe [Address]
linkSources link
  | any (.payerOutside) link.linkPayers = Nothing
  | otherwise = Just (nubOrd (concatMap (.payerSources) link.linkPayers))

-- | The change addresses of a chain that may take a tag, given the
-- addresses that have it: the largest set of them where the coins of
-- each came from addresses that have the tag or are in the set. It
-- starts from the whole chain, and drops the addresses with a coin from
-- elsewhere until none is left to drop. Starting from the whole chain is
-- what lets an address whose change came back to it take the tag: its
-- own coin does not count against it.
mayTake :: Map Address Link -> Set Address -> Set Address
mayTake chain holders = go (Map.keysSet chain)
  where
    go taking
      | kept == taking = taking
      | otherwise = go kept
      where
        kept = Set.filter (maybe False (all tagged) . sourcesOf) taking
        tagged source = Set.member source holders || Set.member source taking
    sourcesOf addr = linkSources =<< Map.lookup addr chain

-- | The chain of change below an address: every change address that a
-- payment spending from it pays, then those that the payments spending
-- from these pay, and so on. Each address is visited once, and the one
-- the walk starts from is not of its own chain.
changeChain :: (MonadUnliftIO m) => Pool -> DBAddress -> ExceptT String (DB m) (Map Address Link)
changeChain pool top = do
  pending <- pendingSummaries pool
  (topAddr, _) <- liftEither $ fromDBAddr pool.poolNetwork top
  let visit addr row = do
        summaries <- lift $ summariesAt pool pending row
        let payers = [payerOf paying | paying@(_, summary) <- summaries, Map.member addr (summaryMyOutputs summary)]
            spenders = filter (Map.member addr . summaryMyInputs) (snd <$> summaries)
        modify $ Map.insert addr (Link row payers)
        forM_ (nubOrd (concatMap ((.changeAddresses) . touchedBy) spenders)) $ \change -> do
          seen <- gets (Map.member change)
          unless seen $ mapM_ (visit change) =<< lift (lift (poolAddressRow pool change))
      payerOf (hash, summary) = Payer hash (Map.keys (summaryMyInputs summary)) (not (Map.null (summaryOtherInputs summary)))
  Map.delete topAddr <$> execStateT (visit topAddr top) Map.empty

-- | The transactions of the pool at an address, as their summaries, each
-- with the hash the listings show for it: the
-- live ones of its history, and those of the pending ones given that
-- spend from the address or pay it. A transaction the network dropped or
-- replaced is left out: it moved no coin. One that is both pending and
-- in the history comes twice, which changes nothing for the callers.
summariesAt :: (MonadUnliftIO m) => Pool -> [(TxHash, TxSummary)] -> DBAddress -> ExceptT String (DB m) [(TxHash, TxSummary)]
summariesAt pool pending row = do
  (addr, _) <- liftEither $ fromDBAddr pool.poolNetwork row
  txids <- lift . select $ txidsAtQuery row
  tids <- liftEither $ mapM (bytesToHash . unValue) txids
  settled <- fmap catMaybes . forM tids $ \tid -> fmap ((tid,) . snd) <$> liveSummary pool tid
  let names summary = Map.member addr (summaryMyInputs summary) || Map.member addr (summaryMyOutputs summary)
  return (settled <> filter (names . snd) pending)

-- | The query of @summariesAt@: the ids of the transactions of the
-- history that spend from an address or pay it. The index of the
-- addresses of transactions by address gives them, and no summary is
-- read to find them. The account of the address names its pool, so the
-- query asks for nothing else.
txidsAtQuery :: DBAddress -> SqlQuery (SqlExpr (Value KeyBytes))
txidsAtQuery row =
  from $ \x -> do
    where_ $
      x ^. DBTxAddressAccount ==. val (dBAddressAccount row)
        &&. x ^. DBTxAddressInternal ==. val (dBAddressInternal row)
        &&. x ^. DBTxAddressIndex ==. val (dBAddressIndex row)
    return (x ^. DBTxAddressTxid)

-- | The pending transactions of a pool, as their summaries, each with
-- the hash pendingtxs shows for it. A chain
-- goes through them as through the history: the change of a payment
-- that waits to be signed is of the group of its coins already.
pendingSummaries :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) [(TxHash, TxSummary)]
pendingSummaries pool = do
  keys <- lift $ poolKeys pool
  held <- pendingTxs pool
  forM held $ \p -> (shownHash (txSignDataTx p.pendingTxData),) . txInfoSummary <$> liftEither (parseTxSignData pool.poolNetwork keys p.pendingTxData)
