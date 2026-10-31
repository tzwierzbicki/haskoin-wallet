{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Replacing a stuck transaction of the account with one that pays a
-- higher fee (BIP 125), as bumpfee prepares it. The replacement spends the
-- coins of the stuck transaction and pays its recipients the same amounts:
-- the higher fee comes out of its change. When the change cannot pay it,
-- confirmed coins of the account are added, as the user agrees: the network
-- takes no replacement that adds a coin it has not confirmed. When no coin
-- of the account would do, or the user wants none added, the recipients
-- pay what is missing out of what they receive, as the user agrees: one of
-- them alone, or all of them in equal shares ('reductions'). A transaction
-- of the account that spends the change of the stuck one dies with it, so
-- the replacement then replaces both, paying the recipients of both, as the
-- user agrees.
--
-- The replacement is a pending transaction like any other, which names the
-- transactions it replaces ('txSignDataReplaces'): signtx signs it, and
-- sendtx sends it and retires those still pending.
module Haskoin.Wallet.TxBumpFee
  ( Sent (..),
    transactionAccounts,
    stuckTransaction,
    checkReplaceable,
    minimumRate,
    spendingChildren,
    refuseSpentElsewhere,
    BumpPlan (..),
    bumpPlan,
    coinsToAdd,
    PaidBy (..),
    Reduction (..),
    reductions,
    Funding (..),
    fundings,
    buildBump,
    incrementalRate,
    checkBumpFee,
    childrenLines,
    childrenQuestion,
    addedLines,
    addedQuestion,
    reducedLines,
    reducedQuestion,
    reductionAnswers,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadTrans (lift))
import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight, isRight)
import Data.List (find, inits, intercalate, nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account (accountNetwork, accountXPubKey, getAccounts)
import Haskoin.Wallet.Address (fromDBAddr, isIntPath, walletPaths)
import Haskoin.Wallet.Coin (coinText)
import Haskoin.Wallet.CoinSelection (CoinSelection (..), FeePayer (..), Selection (..))
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.PendingTx (PendingTx (..), changeAddress, lookupPendingTx, pendingTxs, signedWithId)
import Haskoin.Wallet.RawTx (getRawTx, lookupRawTx)
import Haskoin.Wallet.Store (Store, fromStore, rawTransactions)
import Haskoin.Wallet.TxBuilding (BuiltTx (..), ChosenPayment (..), CoinControl (..), WalletCoins (..), assemblePayment, choosePayment, recipientsPayFee)
import Haskoin.Wallet.TxInfo (MyInputs (..), TxInfo (..), TxSummary (..), liveSummary, ownTransaction, txInfoBlockRefs)
import Haskoin.Wallet.TxSendAll (sendAllFee)
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (parseTxSignData)
import Haskoin.Wallet.Util (addrToString, safeSubtract)
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- The transactions replaced -}

-- | A transaction of the account on the network and not yet in a block,
-- with what replacing it needs
data Sent = Sent
  { sentTx :: !Tx,
    sentSummary :: !TxSummary,
    -- | The transactions its coins come from
    sentParents :: ![Tx]
  }
  deriving (Eq, Show)

-- | The id of a transaction on the network
sentId :: Sent -> TxHash
sentId s = txHash s.sentTx

-- | The accounts that hold a transaction, by its id or, while it is pending,
-- its nosigHash: among their pending transactions, or in their history
transactionAccounts :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) [(DBAccountId, DBAccount)]
transactionAccounts tid = do
  byNosig <- lookupPendingTx tid
  accounts <- lift getAccounts
  filterM (holds byNosig) accounts
  where
    holds byNosig (accId, _) = do
      pending <- sentPending accId tid
      replacement <- pendingReplacementOf accId tid
      inHistory <- lift $ isJust <$> P.get (DBTxInfoKey accId (hashBytes tid))
      return $ isJust pending || isJust replacement || inHistory || ((pendingTxAccount <$> byNosig) == Just accId)

-- | The signed pending transaction of the account with the id, sent or not
sentPending :: (MonadUnliftIO m) => DBAccountId -> TxHash -> ExceptT String (DB m) (Maybe PendingTx)
sentPending accId tid = find (signedWithId tid) <$> pendingTxs accId

-- | The pending transaction of the account that replaces the transaction,
-- if any
pendingReplacementOf :: (MonadUnliftIO m) => DBAccountId -> TxHash -> ExceptT String (DB m) (Maybe PendingTx)
pendingReplacementOf accId tid = find names <$> pendingTxs accId
  where
    names p = tid `elem` txSignDataReplaces (pendingTxData p)

-- | The transaction of the account to replace, by its id: one the user
-- sent, pending until the sync sees it on the network, or in the history
-- and not yet in a block. The raw transactions the wallet no longer keeps,
-- those of coins it has spent, come from the store. One replaced already
-- is refused: its pending replacement is to be sent or deleted first, or
-- bumped in its turn once it is sent.
stuckTransaction :: (MonadUnliftIO m) => Ctx -> Store -> DBAccountId -> DBAccount -> TxHash -> ExceptT String (DB m) Sent
stuckTransaction ctx store accId acc tid = do
  replacementM <- pendingReplacementOf accId tid
  forM_ replacementM $ \r ->
    throwError $
      if r.pendingTxOnline
        then "The transaction is replaced by " <> hex (txHash (txSignDataTx r.pendingTxData)) <> ", which is sent: bump that one instead"
        else "A replacement of the transaction is pending: " <> hex (shownHash (txSignDataTx r.pendingTxData)) <> ". Send it, or delete it with deletetx"
  byNosig <- lookupPendingTx tid
  bySignedId <- sentPending accId tid
  case catMaybes [byNosig, bySignedId] of
    p : _
      | p.pendingTxOnline -> pendingSent ctx acc p
      | otherwise -> throwError "The transaction is not sent yet: delete it with deletetx, and prepare it again at the fee rate you want"
    [] -> do
      live <- liveSummary (accountNetwork acc) accId tid
      case live of
        Nothing -> throwError $ "The account has no transaction " <> hex tid <> " on the network, which may have dropped it"
        Just (block, summary)
          | Store.confirmed block -> throwError "The transaction is in a block already: nothing can replace it"
          -- Before the transactions of its coins are fetched: those of a
          -- payment received are the payer's
          | not (ownTransaction summary) -> throwError notOwnPayment
          | otherwise -> historySent store tid summary

-- | A pending transaction the user sent, as bumpfee replaces it
pendingSent :: (MonadUnliftIO m) => Ctx -> DBAccount -> PendingTx -> ExceptT String (DB m) Sent
pendingSent ctx acc p = do
  info <- liftEither $ parseTxSignData (accountNetwork acc) ctx (accountXPubKey ctx acc) p.pendingTxData
  return $ Sent (txSignDataTx p.pendingTxData) info.txInfoSummary p.pendingTxData.txSignDataInputs

-- | A transaction of the history, as bumpfee replaces it
historySent :: (MonadUnliftIO m) => Store -> TxHash -> TxSummary -> ExceptT String (DB m) Sent
historySent store tid summary = do
  raw <- liftMaybe ("The store has no transaction " <> hex tid) . listToMaybe =<< rawTxs store [tid]
  parents <- rawTxs store $ nubOrd [input.outpoint.hash | input <- raw.inputs]
  return $ Sent raw summary parents

-- | The raw transactions: those the wallet keeps, and the others from the
-- store
rawTxs :: (MonadUnliftIO m) => Store -> [TxHash] -> ExceptT String (DB m) [Tx]
rawTxs store tids = do
  kept <- mapM lookupRawTx tids
  fetched <- fromStore $ rawTransactions store [tid | (tid, Nothing) <- zip tids kept]
  let byId = Map.fromList [(txHash raw, raw) | raw <- catMaybes kept <> fetched]
  forM tids $ \tid -> liftMaybe ("The store has no transaction " <> hex tid) (Map.lookup tid byId)

-- | Refuse to replace a transaction that does not spend the coins of the
-- account alone, or that does not mark itself replaceable. The network
-- takes the replacement of one that does not, but the store hw reads
-- would keep showing it in place of its replacement until a block
-- confirms one of them: the store replaces only the transactions marked
-- so. Once it replaces any, as Bitcoin Core does, the check of the mark
-- can go.
checkReplaceable :: Sent -> Either String ()
checkReplaceable s = do
  unless (ownTransaction s.sentSummary) $ Left notOwnPayment
  unless (any ((< 0xfffffffe) . (.sequence)) s.sentTx.inputs) $
    Left "The transaction is not marked replaceable (BIP 125): the store would keep showing it in place of its replacement until a block confirms one of them"

-- | The refusal of a transaction that spends coins of another wallet: a
-- payment the account receives, or a sweep
notOwnPayment :: String
notOwnPayment = "Only a payment from the coins of the account can be replaced: this transaction spends coins of another wallet"

-- | The lowest fee rate, in satoshis per byte, of a replacement of the
-- transaction: above the rate it pays
minimumRate :: Sent -> Natural
minimumRate s = s.sentSummary.summaryFee `div` s.sentSummary.summarySize + 1

-- | The transactions of an account that spend an output of the given ones:
-- its pending ones, and those of its history not yet in a block, each with
-- its summary
spendersIn :: (MonadUnliftIO m) => Network -> DBAccountId -> [TxHash] -> ExceptT String (DB m) ([PendingTx], [(TxHash, TxSummary)])
spendersIn net accId tids = do
  held <- pendingTxs accId
  let spendsOne :: [OutPoint] -> Bool
      spendsOne = any ((`elem` tids) . (.hash))
      pending = [p | p <- held, spendsOne ((.outpoint) <$> (txSignDataTx p.pendingTxData).inputs)]
  refs <- txInfoBlockRefs accId
  history <- fmap catMaybes . forM [tid | (tid, ref) <- Map.toList refs, not (Store.confirmed ref)] $ \tid -> do
    live <- liveSummary net accId tid
    return $ case live of
      Just (_, summary)
        | spendsOne [outpoint | input <- Map.elems summary.summaryMyInputs, outpoint <- input.myInputsOutpoints] -> Just (tid, summary)
      _ -> Nothing
  return (pending, history)

-- | The transactions of the account that spend outputs of the given ones,
-- and those that spend theirs, which replacing these removes: sent and
-- still pending, or in the history and not yet in a block. One prepared
-- and not sent is refused: it would be left spending a coin that is gone.
spendingChildren :: (MonadUnliftIO m) => Ctx -> Store -> DBAccountId -> DBAccount -> [Sent] -> ExceptT String (DB m) [Sent]
spendingChildren ctx store accId acc parents = go (sentId <$> parents) []
  where
    net = accountNetwork acc
    go [] found = return found
    go tids found = do
      (pending, history) <- spendersIn net accId tids
      fromPending <- forM pending $ \p -> do
        unless p.pendingTxOnline $
          throwError $
            "The pending transaction " <> hex (nosigHash (txSignDataTx p.pendingTxData))
              <> " spends the change of the transaction: send it, or delete it with deletetx, before replacing them"
        pendingSent ctx acc p
      fromHistory <- forM history $ \(tid, summary) -> historySent store tid summary
      let known = Set.fromList (sentId <$> parents <> found)
          new = Map.elems $ Map.fromList [(sentId s, s) | s <- fromPending <> fromHistory, sentId s `Set.notMember` known]
      go (sentId <$> new) (found <> new)

-- | Refuse to replace transactions an output of which a transaction of
-- another account of the network spends: replacing them would remove it,
-- and hw cannot replace it from this account. hw builds no such
-- transaction, since an account spends no unconfirmed coin another account
-- paid it, but another wallet of the seed may have.
refuseSpentElsewhere :: (MonadUnliftIO m) => Network -> DBAccountId -> [Sent] -> ExceptT String (DB m) ()
refuseSpentElsewhere net accId sents = do
  others <- filter (\(otherId, other) -> otherId /= accId && accountNetwork other == net) <$> lift getAccounts
  forM_ others $ \(otherId, other) -> do
    (pending, history) <- spendersIn net otherId (sentId <$> sents)
    let spenders = [nosigHash (txSignDataTx p.pendingTxData) | p <- pending] <> (fst <$> history)
    forM_ (listToMaybe spenders) $ \spender ->
      throwError $
        "The transaction " <> hex spender <> " of the account " <> cs (dBAccountName other)
          <> " spends an output of the transaction, and would be removed with it: hw cannot replace it from this account"

{- The replacement -}

-- | What a replacement keeps of the transactions it replaces, the stuck
-- one first: the coins they spend, but those one of them pays another,
-- with the transactions they come from and the paths of their addresses;
-- what they pay, but their change and what one of them pays another; the
-- address of their change, if any; and the fees they paid.
data BumpPlan = BumpPlan
  { planCoins :: ![Store.Unspent],
    planParents :: ![Tx],
    planCoinPaths :: !(Map Address SoftPath),
    planRecipients :: ![(Address, Natural)],
    planRecipientPaths :: !(Map Address SoftPath),
    planChange :: !(Maybe (Address, SoftPath)),
    planFees :: !Natural,
    planReplaces :: ![TxHash]
  }
  deriving (Eq, Show)

-- | The plan of the replacement of the transactions. The change of a
-- transaction is what it pays the wallet on the change chain, or back to
-- an address it spends from, as --change-back does. What it pays another
-- address of the wallet is a payment, which the replacement keeps.
bumpPlan :: (MonadUnliftIO m) => Network -> Ctx -> DBAccountId -> [Sent] -> ExceptT String (DB m) BumpPlan
bumpPlan net ctx accId sents = do
  let replaced = Set.fromList (sentId <$> sents)
      parents = Map.fromList [(txHash parent, parent) | s <- sents, parent <- s.sentParents]
      spent = [input.outpoint | s <- sents, input <- s.sentTx.inputs]
      -- What one transaction replaced pays another stays out of both
      within = Set.fromList [outpoint | outpoint <- spent, outpoint.hash `Set.member` replaced]
  planned <- forM (nubOrd [outpoint | outpoint <- spent, outpoint `Set.notMember` within]) $ \outpoint -> do
    parent <- liftMaybe ("The transaction of the coin " <> coinText outpoint <> " is missing") (Map.lookup outpoint.hash parents)
    coinOf ctx net accId outpoint parent
  plannedPaths <- walletPaths net accId (mapMaybe (.address) planned)
  paid <-
    liftEither $
      sequence
        [ (,,) s <$> addressOf out <*> pure (fromIntegral out.value)
          | s <- sents,
            (index, out) <- zip [0 ..] s.sentTx.outputs,
            OutPoint (sentId s) index `Set.notMember` within
        ]
  paths <- walletPaths net accId [addr | (_, addr, _) <- paid]
  let changeOf :: Sent -> Address -> Maybe (Address, SoftPath)
      changeOf s addr = case Map.lookup addr paths of
        Just path | isIntPath path || addr `Map.member` s.sentSummary.summaryMyInputs -> Just (addr, path)
        _ -> Nothing
  return
    BumpPlan
      { planCoins = planned,
        planParents = nubOrd [parent | coin <- planned, Just parent <- [Map.lookup coin.outpoint.hash parents]],
        planCoinPaths = plannedPaths,
        planRecipients = [(addr, value) | (s, addr, value) <- paid, isNothing (changeOf s addr)],
        planRecipientPaths = paths,
        planChange = listToMaybe [change | (s, addr, _) <- paid, Just change <- [changeOf s addr]],
        planFees = sum ((.sentSummary.summaryFee) <$> sents),
        planReplaces = sentId <$> sents
      }
  where
    addressOf :: TxOut -> Either String Address
    addressOf out = either (const $ Left "A transaction replaced pays an output without an address") Right (scriptToAddressBS ctx out.script)

-- | The coin of an output of a transaction of the wallet, in the block of
-- that transaction
coinOf :: (MonadUnliftIO m) => Ctx -> Network -> DBAccountId -> OutPoint -> Tx -> ExceptT String (DB m) Store.Unspent
coinOf ctx net accId outpoint parent = do
  out <- liftMaybe ("The coin " <> coinText outpoint <> " is missing") (listToMaybe (drop (fromIntegral outpoint.index) parent.outputs))
  addr <- liftEither $ either (const $ Left ("The coin " <> coinText outpoint <> " pays no address")) Right (scriptToAddressBS ctx out.script)
  live <- liveSummary net accId outpoint.hash
  (block, _) <- liftMaybe ("The transaction of the coin " <> coinText outpoint <> " is not in the history: sync first") live
  return
    Store.Unspent
      { Store.block = block,
        Store.outpoint = outpoint,
        Store.value = out.value,
        Store.script = out.script,
        Store.address = Just addr
      }

-- | The coins of the plan and the coins added, as the builder takes them
walletCoinsOf :: BumpPlan -> [(Store.Unspent, Address, SoftPath)] -> WalletCoins
walletCoinsOf plan added =
  WalletCoins
    { coins = plan.planCoins <> [coin | (coin, _, _) <- added],
      coinPaths = plan.planCoinPaths <> Map.fromList [(addr, path) | (_, addr, path) <- added],
      recipientPaths = plan.planRecipientPaths
    }

-- | The coins of the replacement at the fee rate: all those of the plan,
-- and the coins added
chooseBump :: Ctx -> StdGen -> BumpPlan -> [(Store.Unspent, Address, SoftPath)] -> Natural -> Natural -> Either String ChosenPayment
chooseBump ctx gen plan added feeByte dust =
  choosePayment ctx gen LargestFirst found plan.planRecipients feeByte dust SenderPays (SpendChosen ((.outpoint) <$> found.coins))
  where
    found = walletCoinsOf plan added

-- | The coins to add for the replacement to pay its recipients and its fee:
-- the largest first, as few as do, among the candidates. None when the
-- coins of the plan do. Nothing when even all the candidates do not.
coinsToAdd :: Ctx -> StdGen -> BumpPlan -> [(Store.Unspent, Address, SoftPath)] -> Natural -> Natural -> Maybe [(Store.Unspent, Address, SoftPath)]
coinsToAdd ctx gen plan candidates feeByte dust
  | covers [] = Just []
  | otherwise = find covers (drop 1 (inits (sortOn (\(coin, _, _) -> Down coin.value) candidates)))
  where
    covers added = isRight (chooseBump ctx gen plan added feeByte dust)

{- The recipients paying -}

-- | Which recipients pay what the coins cannot
data PaidBy
  = -- | One of them alone, by its position among them, from 0
    OneRecipient !Int
  | -- | All of them, in equal shares, the first also paying the remainder,
    -- as --recipients-pay of preparetx has them pay
    AllRecipients
  deriving (Eq, Show)

-- | The recipients paying what the coins of the transactions replaced
-- cannot of the fee of the replacement, one way. The replacement then
-- spends the coins in full, with no change, and adds none.
data Reduction = Reduction
  { reductionPaidBy :: !PaidBy,
    -- | What the coins cannot pay, in satoshis
    reductionMissing :: !Natural,
    -- | Each recipient, with what the transactions replaced pay it and what
    -- the replacement pays it
    reductionRecipients :: ![(Address, Natural, Natural)]
  }
  deriving (Eq, Show)

-- | The ways the recipients can pay what the coins of the plan cannot of
-- the fee at the rate, with no coin added: each of them alone, in their
-- order, then all of them in equal shares; those that leave no recipient
-- below the dust limit. A lone recipient has one way. Refused when the
-- coins pay the fee as they are, or when no way is left.
reductions :: Ctx -> BumpPlan -> Natural -> Natural -> Either String [Reduction]
reductions ctx plan feeByte dust = do
  when (total >= amount + fee) $
    Left "The coins of the transaction pay its fee: the recipients have nothing to pay"
  let missing = amount + fee - total
      ways = mapMaybe (reductionBy missing) ([OneRecipient i | several, i <- [0 .. count - 1]] <> [AllRecipients])
  when (null ways) $ Left (cannot missing)
  return ways
  where
    recipients = plan.planRecipients
    count = length recipients
    several = count > 1
    total = sum [fromIntegral coin.value | coin <- plan.planCoins]
    amount = sum (snd <$> recipients)
    -- The replacement spends the coins in full, with no change: its fee is
    -- that of a send-all
    fee = sendAllFee ctx feeByte plan.planCoins (fst <$> recipients)
    -- What each recipient gets paying the missing amount one way, when none
    -- is left below the dust limit
    reductionBy missing paidBy = do
      after <- case paidBy of
        OneRecipient i -> sequence [(addr,) <$> (if j == i then value `safeSubtract` missing else Just value) | (j, (addr, value)) <- zip [0 ..] recipients]
        AllRecipients -> either (const Nothing) Just (recipientsPayFee (fromIntegral missing) recipients)
      guard $ all ((>= dust) . snd) after
      return
        Reduction
          { reductionPaidBy = paidBy,
            reductionMissing = missing,
            reductionRecipients = [(addr, before, afterwards) | ((addr, before), (_, afterwards)) <- zip recipients after]
          }
    cannot missing
      | several =
          "The recipients cannot pay the " <> show missing <> " satoshis missing, together or alone, without one being left with less than the dust limit of " <> show dust <> " satoshis"
      | otherwise =
          "The recipient cannot pay the " <> show missing <> " satoshis missing without being left with less than the dust limit of " <> show dust <> " satoshis"

{- Funding -}

-- | What pays the fee of the replacement, beyond the fees of the
-- transactions replaced
data Funding
  = -- | Their change, which nothing needs to be asked about
    ChangePays
  | -- | Confirmed coins of the account, added as the user agrees
    CoinsAdded ![(Store.Unspent, Address, SoftPath)]
  | -- | The recipients, out of what they receive, one way, as the user
    -- agrees
    RecipientsReduced !Reduction
  deriving (Eq, Show)

-- | The fundings of the replacement at the fee rate, in the order bumpfee
-- offers them: the change alone, when it pays; otherwise the coins of the
-- account to add, when some would do, then each way the recipients can
-- pay. Refused when nothing pays: neither the coins, nor the recipients.
fundings :: Ctx -> StdGen -> BumpPlan -> [(Store.Unspent, Address, SoftPath)] -> Natural -> Natural -> Either String [Funding]
fundings ctx gen plan candidates feeByte dust =
  case (coinsToAdd ctx gen plan candidates feeByte dust, reductions ctx plan feeByte dust) of
    (Just [], _) -> Right [ChangePays]
    (Just added, reduced) -> Right (CoinsAdded added : (RecipientsReduced <$> fromRight [] reduced))
    (Nothing, Right reduced) -> Right (RecipientsReduced <$> reduced)
    (Nothing, Left cannot) -> Left (noCoins <> ". " <> cannot)
  where
    noCoins = "The coins of the transaction cannot pay its fee at " <> show feeByte <> " sat/byte, and no confirmed coins of the account would"

-- | The signing data of the replacement at the fee rate, from the coins of
-- the plan and its funding, with the transactions those come from. Its
-- change goes to the change address of the plan, or to a new change
-- address when the transactions replaced had no change.
buildBump ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  Network ->
  DBAccountId ->
  StdGen ->
  BumpPlan ->
  Funding ->
  -- | Fee per byte
  Natural ->
  -- | Dust
  Natural ->
  ExceptT String (DB m) TxSignData
buildBump ctx cfg net accId gen plan funding feeByte dust = do
  let (funded, added) = case funding of
        ChangePays -> (plan, [])
        CoinsAdded extra -> (plan, extra)
        RecipientsReduced reduction -> (reducedPlan reduction, [])
      found = walletCoinsOf funded added
  addedParents <- mapM getRawTx $ nub [coin.outpoint.hash | (coin, _, _) <- added]
  chosen <- liftEither $ chooseBump ctx gen funded added feeByte dust
  change <-
    if chosen.selection.changeAmount > 0
      then Just <$> maybe newChange return plan.planChange
      else return Nothing
  built <- liftEither $ assemblePayment net ctx found chosen change
  return $ TxSignData built.tx (nubOrd (plan.planParents <> addedParents)) built.inputPaths built.outputPaths (Just feeByte) Nothing plan.planReplaces
  where
    newChange = liftEither . fromDBAddr net =<< changeAddress ctx cfg accId
    -- The plan with the recipients paid what the reduction leaves them
    reducedPlan :: Reduction -> BumpPlan
    reducedPlan reduction = plan {planRecipients = [(addr, after) | (addr, _, after) <- reduction.reductionRecipients]}

-- | The satoshis per byte a replacement pays for its own size, beyond the
-- fees of what it replaces, for the network to relay it: the incremental
-- relay fee of Bitcoin Core, which recent versions lowered to 0.1
incrementalRate :: Natural
incrementalRate = 1

-- | Refuse a replacement the network would not take: it pays the fees of
-- the transactions it replaces, and its own size at 'incrementalRate'
checkBumpFee :: BumpPlan -> TxSummary -> Either String ()
checkBumpFee plan summary =
  when (summary.summaryFee < needed) $
    Left $
      "The replacement pays "
        <> show summary.summaryFee
        <> " satoshis of fee, and must pay "
        <> show needed
        <> ": the fees of the transactions it replaces, and its own "
        <> show summary.summarySize
        <> " bytes. Give a --fee-rate of at least "
        <> show ((needed + summary.summarySize - 1) `div` summary.summarySize)
  where
    needed = plan.planFees + incrementalRate * summary.summarySize

{- What bumpfee asks -}

-- | What bumpfee says of the transactions that spend the change of the one
-- to replace, before it asks to replace them all
childrenLines :: [Sent] -> [String]
childrenLines children =
  ("Replacing the transaction removes those of the account that spend its change:" :)
    ["  " <> hex (sentId child) | child <- children]

-- | The question after 'childrenLines'
childrenQuestion :: [Sent] -> String
childrenQuestion [_] = "Replace both with one transaction that pays the recipients of both?"
childrenQuestion _ = "Replace them all with one transaction that pays all their recipients?"

-- | What bumpfee says of the coins to add, before it asks to add them
addedLines :: Natural -> [(Store.Unspent, Address, SoftPath)] -> [String]
addedLines feeByte added =
  ("The coins of the transaction cannot pay its fee at " <> show feeByte <> " sat/byte, and " <> them <> " of the account would:" :)
    ["  " <> coinText coin.outpoint <> "  " <> show coin.value <> " sat at " <> pathToStr path | (coin, _, path) <- added]
  where
    them = if length added == 1 then "this coin" else "these coins"

-- | The question after 'addedLines'
addedQuestion :: [(Store.Unspent, Address, SoftPath)] -> String
addedQuestion [_] = "Add it?"
addedQuestion _ = "Add them?"

-- | What bumpfee says of the ways the recipients can pay, those of
-- 'reductions', before it asks which: each recipient, numbered when there
-- are several, with what it receives, what it would receive paying alone,
-- and its share when all can pay
reducedLines :: Network -> Natural -> [Reduction] -> [String]
reducedLines _ _ [] = []
reducedLines net feeByte ways@(way : _) = header : rows
  where
    missing = show way.reductionMissing <> " sat missing"
    recipients = [(addr, before) | (addr, before, _) <- way.reductionRecipients]
    several = length recipients > 1
    -- What each recipient gets paying it alone, by its position, when it can
    alone = Map.fromList [(i, after) | w <- ways, OneRecipient i <- [w.reductionPaidBy], (_, _, after) <- [w.reductionRecipients !! i]]
    -- What each gets paying its share, when all can
    shares = listToMaybe [[after | (_, _, after) <- w.reductionRecipients] | w <- ways, w.reductionPaidBy == AllRecipients]
    header =
      "The coins of the transaction cannot pay its fee at " <> show feeByte <> " sat/byte, and no coin of the account is added. " <> who
    who
      | several =
          "The " <> missing <> " can come out of what the recipients receive, one of them paying it alone"
            <> maybe ":" (const ", or all of them in equal shares:") shares
      | otherwise = "The recipient can pay the " <> missing <> " out of what it receives:"
    rows
      | several =
          [ "  " <> show (i + 1) <> "  " <> addrToString net addr <> "  " <> show before <> " sat" <> aloneText i <> shareText i
            | (i, (addr, before)) <- zip [0 :: Int ..] recipients
          ]
      | otherwise = ["  " <> addrToString net addr <> "  " <> show before <> " sat, " <> show after <> " paying it" | (addr, before, after) <- way.reductionRecipients]
    aloneText i = maybe ", too little to pay it alone" (\after -> ", " <> show after <> " paying it alone") (Map.lookup i alone)
    shareText i = maybe "" (\afters -> ", " <> show (afters !! i) <> " paying its share") shares

-- | The question after 'reducedLines': whether the lone recipient pays, or
-- which of several does, by the names of the ways
reducedQuestion :: [Reduction] -> String
reducedQuestion ways = case ways of
  way : _ | length way.reductionRecipients == 1 -> "Reduce what it receives?"
  _ -> "Who pays it? Type " <> offered <> ", or no"
  where
    offered = case wayName <$> ways of
      [one] -> one
      names -> intercalate ", " (init names) <> " or " <> last names

-- | The name of a way, as the question offers it: the number of the
-- recipient paying alone, from 1, or all
wayName :: Reduction -> String
wayName way = case way.reductionPaidBy of
  OneRecipient i -> show (i + 1)
  AllRecipients -> "all"

-- | The answers that pick a way among several: its name, or the address of
-- the recipient paying alone
reductionAnswers :: Network -> Reduction -> [Text]
reductionAnswers net way =
  cs (wayName way) : [cs (addrToString net addr) | OneRecipient i <- [way.reductionPaidBy], (addr, _, _) <- take 1 (drop i way.reductionRecipients)]

-- | A transaction id as hw writes it
hex :: TxHash -> String
hex = cs . txHashToHex
