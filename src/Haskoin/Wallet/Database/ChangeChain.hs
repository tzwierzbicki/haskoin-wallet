{-# LANGUAGE OverloadedRecordDot #-}

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
  ( retagChangeChain,
    txidsAtQuery,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (filterM, forM, forM_, unless)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT, gets, modify)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intersect, sortOn)
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
import Haskoin.Wallet.Tx.Signing (parseTxSignData)

-- | Do down the chain of change of an address what tag or untag just did
-- to the address: put the tags on every change address below it that may
-- take them (@mayTake@), or take them off every change address below it.
-- Answers the change addresses it changed, in the order of their paths.
-- The address itself is tagged or untagged by the caller, before.
retagChangeChain :: (MonadUnliftIO m) => Retag -> Pool -> DBAddress -> [Tag] -> ExceptT String (DB m) [DBAddress]
retagChangeChain retag pool top tags = do
  chain <- changeChain pool top
  changed <- case retag of
    AddTags -> tagChain pool chain tags
    RemoveTags -> lift . flip filterM (linkRow <$> Map.elems chain) $ \row -> do
      had <- intersect tags . map shownTag <$> addressTags row
      not (null had) <$ untagAddress row had
  return (sortOn rowKey changed)
  where
    rowKey row = (dBAddressAccount row, dBAddressInternal row, dBAddressIndex row)

-- | Put on each change address of a chain those of the tags that it may
-- take and does not have. Answers the addresses that took one.
tagChain :: (MonadUnliftIO m) => Pool -> Map Address Link -> [Tag] -> ExceptT String (DB m) [DBAddress]
tagChain pool chain tags = lift $ do
  let sources = concat (mapMaybe linkSources (Map.elems chain))
  held <- fmap Map.fromList . forM (nubOrd (Map.keys chain <> sources)) $ \addr -> do
    tagsM <- mapM addressTags =<< poolAddressRow pool addr
    return (addr, maybe [] (map shownTag) tagsM)
  let holders tag = Map.keysSet (Map.filter (tag `elem`) held)
      taking tag = mayTake chain (holders tag) `Set.difference` holders tag
      takers = [(tag, taking tag) | tag <- tags]
  fmap catMaybes . forM (Map.toList chain) $ \(addr, link) ->
    case [tag | (tag, set) <- takers, addr `Set.member` set] of
      [] -> return Nothing
      new -> Just link.linkRow <$ tagAddress link.linkRow new

-- | A change address of the chain below an address
data Link = Link
  { linkRow :: !DBAddress,
    -- | The addresses its coins came from: those that the payments that
    -- pay it spend from. Nothing when one of those payments also spends
    -- a coin that is not of the pool, which has no tag, so the address
    -- takes none.
    linkSources :: !(Maybe [Address])
  }

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
        let payers = filter (Map.member addr . summaryMyOutputs) summaries
            spenders = filter (Map.member addr . summaryMyInputs) summaries
        modify $ Map.insert addr (Link row (sourcesOf payers))
        forM_ (nubOrd (concatMap ((.changeAddresses) . touchedBy) spenders)) $ \change -> do
          seen <- gets (Map.member change)
          unless seen $ mapM_ (visit change) =<< lift (lift (poolAddressRow pool change))
      sourcesOf payers
        | all (Map.null . summaryOtherInputs) payers = Just (nubOrd (concatMap (Map.keys . summaryMyInputs) payers))
        | otherwise = Nothing
  Map.delete topAddr <$> execStateT (visit topAddr top) Map.empty

-- | The transactions of the pool at an address, as their summaries: the
-- live ones of its history, and those of the pending ones given that
-- spend from the address or pay it. A transaction the network dropped or
-- replaced is left out: it moved no coin. One that is both pending and
-- in the history comes twice, which changes nothing for the callers.
summariesAt :: (MonadUnliftIO m) => Pool -> [TxSummary] -> DBAddress -> ExceptT String (DB m) [TxSummary]
summariesAt pool pending row = do
  (addr, _) <- liftEither $ fromDBAddr pool.poolNetwork row
  txids <- lift . select $ txidsAtQuery row
  tids <- liftEither $ mapM (bytesToHash . unValue) txids
  settled <- catMaybes <$> mapM (liveSummary pool) tids
  let names summary = Map.member addr (summaryMyInputs summary) || Map.member addr (summaryMyOutputs summary)
  return (map snd settled <> filter names pending)

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

-- | The pending transactions of a pool, as their summaries. A chain
-- goes through them as through the history: the change of a payment
-- that waits to be signed is of the group of its coins already.
pendingSummaries :: (MonadUnliftIO m) => Pool -> ExceptT String (DB m) [TxSummary]
pendingSummaries pool = do
  keys <- lift $ poolKeys pool
  held <- pendingTxs pool
  forM held $ \p -> txInfoSummary <$> liftEither (parseTxSignData pool.poolNetwork keys p.pendingTxData)
