{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Replacing a stuck transaction of a portfolio with one that pays a
-- higher fee (BIP 125), as bumpfee prepares it. The replacement spends the
-- coins of the stuck transaction and pays its recipients the same amounts:
-- the higher fee comes out of its change. When the change cannot pay it,
-- confirmed coins of the portfolio on the network are added, of either
-- type, as the user agrees: the network takes no replacement that adds a
-- coin it has not confirmed. When no coin of the portfolio would do, or
-- the user wants none added, the recipients
-- pay what is missing out of what they receive, as the user agrees: one of
-- them alone, or all of them in equal shares ('reductions'). A transaction
-- of the portfolio that spends the change of the stuck one dies with it, so
-- the replacement then replaces both, paying the recipients of both, as the
-- user agrees.
--
-- --yes takes what the user would most often take, so that a script
-- needs no other option: the change, else coins of the portfolio, else a
-- lone recipient paying, else all the recipients in equal shares. Only
-- when the shares would leave a recipient below the dust limit does
-- bumpfee still ask which one pays alone: --yes picks no recipient.
-- --recipients-pay has the recipients pay what the higher fee adds
-- instead, in equal shares, as preparetx --recipients-pay has them pay the
-- fee of a payment: the change keeps its amount, and no coin is added
-- ('fundings').
--
-- The replacement is a pending transaction like any other, which names the
-- transactions it replaces ('txSignDataReplaces'): signtx signs it, and
-- broadcast sends it and retires those still pending.
module Haskoin.Wallet.Tx.BumpFee
  ( Sent (..),
    transactionPools,
    stuckTransaction,
    replacedRefusal,
    checkReplaceable,
    marksReplaceable,
    minimumRate,
    spendingChildren,
    refuseSpentElsewhere,
    BumpPlan (..),
    bumpPlan,
    replacementLabel,
    coinsToAdd,
    PaidBy (..),
    Reduction (..),
    reductions,
    sharedReduction,
    Funding (..),
    fundings,
    buildBump,
    incrementalRate,
    checkBumpFee,
    sentId,
    childrenQuestion,
    addedQuestion,
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
import Data.List (find, inits, intercalate, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Persist as P
import Data.Word (Word32)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account (defaultKindOn, poolsOnNetwork)
import Haskoin.Wallet.Database.Address (fromDBAddr, isIntPath, poolPaths)
import Haskoin.Wallet.Database.Coin (coinParents, coinText)
import Haskoin.Wallet.Database.PendingTx (PendingTx (..), changeAddress, lookupPendingTx, pendingTxs, placeText, poolKeys, signedWithId)
import Haskoin.Wallet.Database.Portfolio (poolPlace)
import Haskoin.Wallet.Database.RawTx (lookupRawTx)
import Haskoin.Wallet.Database.TxInfo (ChainStatus (..), MyInputs (..), TxInfo (..), TxStatus (..), TxSummary (..), allPools, liveSummary, ownTransaction, storedTxInfo, txInfoBlockRefs, txInfoKey)
import Haskoin.Wallet.Keys (AccountKind, PoolPath (..))
import Haskoin.Wallet.Store (Store, fromStore, rawTransactions)
import Haskoin.Wallet.Tx.Building (BuiltTx (..), ChosenPayment (..), CoinControl (..), WalletCoins (..), assemblePayment, changeKindRule, choosePayment, recipientsPayFee)
import Haskoin.Wallet.Tx.CoinSelection (CoinSelection (..), FeePayer (..), Selection (..), coinInput)
import Haskoin.Wallet.Database.TxLabel (txLabel)
import Haskoin.Wallet.Tx.SendAll (sendAllFee)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (accountOutputKind, outputDustLimit, recipientOutputKind, txSize)
import Haskoin.Wallet.Tx.Signing (parseTxSignData)
import Haskoin.Wallet.Util (addrToString, safeSubtract)
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- The transactions replaced -}

-- | A transaction of the portfolio on the network and not yet in a block,
-- with what replacing it needs
data Sent = Sent
  { sentTx :: !Tx,
    sentSummary :: !TxSummary,
    -- | The transactions its coins come from
    sentParents :: ![Tx],
    -- | The label the user gave it, or none
    sentLabel :: !Text
  }
  deriving (Eq, Show)

-- | The id of a transaction on the network
sentId :: Sent -> TxHash
sentId s = txHash s.sentTx

-- | The pools that hold a transaction, by its id or, while it is pending,
-- its nosigHash: among their pending transactions, or in their history
transactionPools :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) [Pool]
transactionPools tid = do
  byNosig <- lookupPendingTx tid
  pools <- lift allPools
  filterM (holds byNosig) pools
  where
    holds byNosig pool = do
      pending <- sentPending pool tid
      replacement <- pendingReplacementOf pool tid
      inHistory <- lift $ isJust <$> P.get (txInfoKey pool tid)
      return $ isJust pending || isJust replacement || inHistory || ((pendingTxPool <$> byNosig) == Just pool)

-- | The signed pending transaction of the pool with the id, sent or not
sentPending :: (MonadUnliftIO m) => Pool -> TxHash -> ExceptT String (DB m) (Maybe PendingTx)
sentPending pool tid = find (signedWithId tid) <$> pendingTxs pool

-- | The pending transaction of the pool that replaces the transaction,
-- if any
pendingReplacementOf :: (MonadUnliftIO m) => Pool -> TxHash -> ExceptT String (DB m) (Maybe PendingTx)
pendingReplacementOf pool tid = find names <$> pendingTxs pool
  where
    names p = tid `elem` txSignDataReplaces (pendingTxData p)

-- | The transaction of the pool to replace, by its id: one the user
-- sent, pending until the sync sees it on the network, or in the history
-- and not yet in a block. The raw transactions the wallet no longer keeps,
-- those of coins it has spent, come from the store. One replaced already
-- is refused: its pending replacement is to be sent or deleted first, or
-- bumped in its turn once it is sent. Once the sync has found the
-- replacement on the network, the transaction is dead in the history, and
-- the refusal still names the replacement ('replacedRefusal'). The user
-- often types the txid of the stuck payment again, from the history of
-- the shell.
stuckTransaction :: (MonadUnliftIO m) => Ctx -> Store -> Pool -> TxHash -> ExceptT String (DB m) Sent
stuckTransaction ctx store pool tid = do
  replacementM <- pendingReplacementOf pool tid
  forM_ replacementM $ \r ->
    throwError $
      if r.pendingTxOnline
        then "The transaction is replaced by " <> hex (txHash (txSignDataTx r.pendingTxData)) <> ", which is sent: bump that one instead"
        else "A replacement of the transaction is pending: " <> hex (shownHash (txSignDataTx r.pendingTxData)) <> ". Send it, or delete it with deletetx"
  byNosig <- lookupPendingTx tid
  bySignedId <- sentPending pool tid
  case catMaybes [byNosig, bySignedId] of
    p : _
      | p.pendingTxOnline -> pendingSent ctx p
      | otherwise -> throwError "The transaction is not sent yet: delete it with deletetx, and prepare it again at the fee rate you want"
    [] -> do
      live <- liveSummary pool tid
      case live of
        Nothing -> do
          replacements <- replacementsInHistory
          throwError $
            if null replacements
              then "The portfolio has no transaction " <> hex tid <> " on the network, which may have dropped it"
              else replacedRefusal replacements
        Just (block, summary)
          | Store.confirmed block -> throwError "The transaction is in a block already: nothing can replace it"
          -- Before the transactions of its coins are fetched: those of a
          -- payment received are the payer's
          | not (ownTransaction summary) -> throwError notOwnPayment
          | otherwise -> historySent store pool tid summary
  where
    -- The live transactions that replaced the transaction, when the
    -- history of the pool holds it dead
    replacementsInHistory = do
      stored <- storedTxInfo pool tid
      return $ case txInfoStatus <$> stored of
        Just (OnChain status) -> chainReplacedBy status
        _ -> []

-- | Why bumpfee does not replace a dead transaction that live
-- transactions replaced, by their ids ('chainReplacedBy'). One
-- replacement is the transaction to bump. Several are named, and the
-- user picks the one that is stuck.
replacedRefusal :: [TxHash] -> String
replacedRefusal [replacement] = "The transaction was replaced by " <> hex replacement <> ": bump that one instead"
replacedRefusal replacements = "The transaction was replaced by " <> intercalate " and " (hex <$> replacements)

-- | A pending transaction the user sent, as bumpfee replaces it, with the
-- label of its signing data
pendingSent :: (MonadUnliftIO m) => Ctx -> PendingTx -> ExceptT String (DB m) Sent
pendingSent ctx p = do
  keys <- lift $ poolKeys ctx p.pendingTxPool
  info <- liftEither $ parseTxSignData p.pendingTxPool.poolNetwork ctx keys p.pendingTxData
  return $ Sent (txSignDataTx p.pendingTxData) info.txInfoSummary p.pendingTxData.txSignDataInputs p.pendingTxData.txSignDataLabel

-- | A transaction of the pool's history, as bumpfee replaces it, with
-- the label the pool gives it
historySent :: (MonadUnliftIO m) => Store -> Pool -> TxHash -> TxSummary -> ExceptT String (DB m) Sent
historySent store pool tid summary = do
  raw <- liftMaybe ("The store has no transaction " <> hex tid) . listToMaybe =<< rawTxs store [tid]
  parents <- rawTxs store $ nubOrd [input.outpoint.hash | input <- raw.inputs]
  label <- lift $ txLabel pool tid
  return $ Sent raw summary parents label

-- | The raw transactions: those the wallet keeps, and the others from the
-- store
rawTxs :: (MonadUnliftIO m) => Store -> [TxHash] -> ExceptT String (DB m) [Tx]
rawTxs store tids = do
  kept <- mapM lookupRawTx tids
  fetched <- fromStore $ rawTransactions store [tid | (tid, Nothing) <- zip tids kept]
  let byId = Map.fromList [(txHash raw, raw) | raw <- catMaybes kept <> fetched]
  forM tids $ \tid -> liftMaybe ("The store has no transaction " <> hex tid) (Map.lookup tid byId)

-- | Refuse to replace a transaction that does not spend the coins of the
-- portfolio alone, or that does not mark itself replaceable. The network
-- takes the replacement of one that does not, but the store hw reads
-- would keep showing it in place of its replacement until a block
-- confirms one of them: the store replaces only the transactions marked
-- so. Once it replaces any, as Bitcoin Core does, the check of the mark
-- can go.
checkReplaceable :: Sent -> Either String ()
checkReplaceable s = do
  unless (ownTransaction s.sentSummary) $ Left notOwnPayment
  unless (marksReplaceable s.sentTx) $
    Left "The transaction is not marked replaceable (BIP 125): the store would keep showing it in place of its replacement until a block confirms one of them"

-- | Whether a transaction marks itself replaceable (BIP 125): an input of
-- it has a sequence below 0xfffffffe
marksReplaceable :: Tx -> Bool
marksReplaceable transaction = any ((< 0xfffffffe) . (.sequence)) transaction.inputs

-- | The refusal of a transaction that spends coins of another wallet: a
-- payment the portfolio receives, or a sweep
notOwnPayment :: String
notOwnPayment = "Only a payment from the coins of the portfolio can be replaced: this transaction spends coins of another wallet"

-- | The lowest fee rate, in satoshis per virtual byte, of a replacement of
-- the transaction: above the rate it pays
minimumRate :: Sent -> Natural
minimumRate s = s.sentSummary.summaryFee `div` s.sentSummary.summarySize + 1

-- | The transactions of a pool that spend an output of the given ones:
-- its pending ones, and those of its history not yet in a block, each with
-- its summary
spendersIn :: (MonadUnliftIO m) => Pool -> [TxHash] -> ExceptT String (DB m) ([PendingTx], [(TxHash, TxSummary)])
spendersIn pool tids = do
  held <- pendingTxs pool
  let spendsOne :: [OutPoint] -> Bool
      spendsOne = any ((`elem` tids) . (.hash))
      pending = [p | p <- held, spendsOne ((.outpoint) <$> (txSignDataTx p.pendingTxData).inputs)]
  refs <- txInfoBlockRefs pool
  history <- fmap catMaybes . forM [tid | (tid, ref) <- Map.toList refs, not (Store.confirmed ref)] $ \tid -> do
    live <- liveSummary pool tid
    return $ case live of
      Just (_, summary)
        | spendsOne [outpoint | input <- Map.elems summary.summaryMyInputs, outpoint <- input.myInputsOutpoints] -> Just (tid, summary)
      _ -> Nothing
  return (pending, history)

-- | The transactions of the pool that spend outputs of the given ones,
-- and those that spend theirs, which replacing these removes: sent and
-- still pending, or in the history and not yet in a block. One prepared
-- and not sent is refused: it would be left spending a coin that is gone.
spendingChildren :: (MonadUnliftIO m) => Ctx -> Store -> Pool -> [Sent] -> ExceptT String (DB m) [Sent]
spendingChildren ctx store pool parents = go (sentId <$> parents) []
  where
    go [] found = return found
    go tids found = do
      (pending, history) <- spendersIn pool tids
      fromPending <- forM pending $ \p -> do
        unless p.pendingTxOnline $
          throwError $
            "The pending transaction " <> hex (nosigHash (txSignDataTx p.pendingTxData))
              <> " spends the change of the transaction: send it, or delete it with deletetx, before replacing them"
        pendingSent ctx p
      fromHistory <- forM history $ \(tid, summary) -> historySent store pool tid summary
      let known = Set.fromList (sentId <$> parents <> found)
          new = Map.elems $ Map.fromList [(sentId s, s) | s <- fromPending <> fromHistory, sentId s `Set.notMember` known]
      go (sentId <$> new) (found <> new)

-- | Refuse to replace transactions an output of which a transaction of
-- another portfolio spends on the network: replacing them would remove
-- it, and hw cannot replace it from this portfolio. A portfolio may spend
-- what another portfolio of the wallet paid it before it confirms, so hw
-- builds such a transaction itself: deletetx frees the way while it is
-- pending, and once it is sent, the payment is replaced from neither
-- portfolio.
refuseSpentElsewhere :: (MonadUnliftIO m) => Pool -> [Sent] -> ExceptT String (DB m) ()
refuseSpentElsewhere pool sents = do
  others <- filter (/= pool) <$> lift (poolsOnNetwork pool.poolNetwork)
  forM_ others $ \other -> do
    (pending, history) <- spendersIn other (sentId <$> sents)
    let spenders = [nosigHash (txSignDataTx p.pendingTxData) | p <- pending] <> (fst <$> history)
    forM_ (listToMaybe spenders) $ \spender -> do
      place <- poolPlace other
      throwError $
        "The transaction " <> hex spender <> " of " <> placeText place
          <> " spends an output of the transaction, and would be removed with it: hw cannot replace it from this portfolio"

{- The replacement -}

-- | What a replacement keeps of the transactions it replaces, the stuck
-- one first: the coins they spend, but those one of them pays another,
-- with the transactions they come from and the paths of their addresses;
-- what they pay, but their change and what one of them pays another; the
-- address of their change, if any; the fees they paid; and their labels.
data BumpPlan = BumpPlan
  { -- | The type new change takes when the coins of the replacement are of
    -- both types: the default type of the configuration on the network
    planDefaultKind :: !AccountKind,
    planCoins :: ![Store.Unspent],
    planParents :: ![Tx],
    planCoinPaths :: !(Map Address PoolPath),
    planRecipients :: ![(Address, Natural)],
    planRecipientPaths :: !(Map Address PoolPath),
    planChange :: !(Maybe (Address, PoolPath)),
    planFees :: !Natural,
    planReplaces :: ![TxHash],
    -- | The label of the replacement: see 'replacementLabel'
    planLabel :: !Text
  }
  deriving (Eq, Show)

-- | The plan of the replacement of the transactions. The change of a
-- transaction is what it pays the portfolio on a change chain, or back to
-- an address it spends from. What it pays another address of the
-- portfolio is a payment, which the replacement keeps: change sent to a
-- receiving address with --change-back is such a payment, and is not
-- reduced.
bumpPlan :: (MonadUnliftIO m) => Ctx -> Config -> Pool -> [Sent] -> ExceptT String (DB m) BumpPlan
bumpPlan ctx cfg pool sents = do
  let net = pool.poolNetwork
      replaced = Set.fromList (sentId <$> sents)
      parents = Map.fromList [(txHash parent, parent) | s <- sents, parent <- s.sentParents]
      spent = [input.outpoint | s <- sents, input <- s.sentTx.inputs]
      -- What one transaction replaced pays another stays out of both
      within = Set.fromList [outpoint | outpoint <- spent, outpoint.hash `Set.member` replaced]
  planned <- forM (nubOrd [outpoint | outpoint <- spent, outpoint `Set.notMember` within]) $ \outpoint -> do
    parent <- liftMaybe ("The transaction of the coin " <> coinText outpoint <> " is missing") (Map.lookup outpoint.hash parents)
    coinOf ctx pool outpoint parent
  plannedPaths <- poolPaths pool (mapMaybe (.address) planned)
  paid <-
    liftEither $
      sequence
        [ (,,) s <$> addressOf out <*> pure (fromIntegral out.value)
          | s <- sents,
            (index, out) <- zip [0 ..] s.sentTx.outputs,
            OutPoint (sentId s) index `Set.notMember` within
        ]
  paths <- poolPaths pool [addr | (_, addr, _) <- paid]
  let changeOf :: Sent -> Address -> Maybe (Address, PoolPath)
      changeOf s addr = case Map.lookup addr paths of
        Just path | isIntPath (pathInAccount path) || addr `Map.member` s.sentSummary.summaryMyInputs -> Just (addr, path)
        _ -> Nothing
  return
    BumpPlan
      { planDefaultKind = defaultKindOn cfg net,
        planCoins = planned,
        planParents = nubOrd [parent | coin <- planned, Just parent <- [Map.lookup coin.outpoint.hash parents]],
        planCoinPaths = plannedPaths,
        planRecipients = [(addr, value) | (s, addr, value) <- paid, isNothing (changeOf s addr)],
        planRecipientPaths = paths,
        planChange = listToMaybe [change | (s, addr, _) <- paid, Just change <- [changeOf s addr]],
        planFees = sum ((.sentSummary.summaryFee) <$> sents),
        planReplaces = sentId <$> sents,
        planLabel = replacementLabel sents
      }
  where
    addressOf :: TxOut -> Either String Address
    addressOf out = either (const $ Left "A transaction replaced pays an output without an address") Right (scriptToAddressBS pool.poolNetwork ctx out.script)

-- | The label of a replacement: the labels of the transactions it
-- replaces, the stuck one first, each label once, a semicolon between
-- them. The replacement pays the recipients of all of them.
replacementLabel :: [Sent] -> Text
replacementLabel sents = Text.intercalate "; " (nubOrd [s.sentLabel | s <- sents, not (Text.null s.sentLabel)])

-- | The coin of an output of a transaction of the wallet, in the block of
-- that transaction
coinOf :: (MonadUnliftIO m) => Ctx -> Pool -> OutPoint -> Tx -> ExceptT String (DB m) Store.Unspent
coinOf ctx pool outpoint parent = do
  out <- liftMaybe ("The coin " <> coinText outpoint <> " is missing") (listToMaybe (drop (fromIntegral outpoint.index) parent.outputs))
  addr <- liftEither $ either (const $ Left ("The coin " <> coinText outpoint <> " pays no address")) Right (scriptToAddressBS pool.poolNetwork ctx out.script)
  live <- liveSummary pool outpoint.hash
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
walletCoinsOf :: BumpPlan -> [(Store.Unspent, Address, PoolPath)] -> WalletCoins
walletCoinsOf plan added =
  WalletCoins
    { coins = plan.planCoins <> [coin | (coin, _, _) <- added],
      coinPaths = plan.planCoinPaths <> Map.fromList [(addr, path) | (_, addr, path) <- added],
      recipientPaths = plan.planRecipientPaths
    }

-- | The coins of the replacement at the fee rate: all those of the plan,
-- and the coins added. Its change is of the type of the change address of
-- the plan, when the transactions replaced had change, or else of the
-- type its coins call for ('changeKindRule').
chooseBump :: Ctx -> StdGen -> BumpPlan -> [(Store.Unspent, Address, PoolPath)] -> Natural -> Either String ChosenPayment
chooseBump ctx gen plan added feeByte =
  choosePayment ctx gen LargestFirst found plan.planRecipients feeByte SenderPays (SpendChosen ((.outpoint) <$> found.coins)) (bumpChangeKind plan found.coins)
  where
    found = walletCoinsOf plan added

-- | The type of the change of a replacement that spends the coins given:
-- that of the change address of the plan, or else the type the coins call
-- for ('changeKindRule')
bumpChangeKind :: BumpPlan -> [Store.Unspent] -> AccountKind
bumpChangeKind plan spent = maybe (changeKindRule plan.planDefaultKind spent) (pathKind . snd) plan.planChange

-- | The coins to add for the replacement to pay its recipients and its fee:
-- the largest first, as few as do, among the candidates. None when the
-- coins of the plan do. Nothing when even all the candidates do not.
coinsToAdd :: Ctx -> StdGen -> BumpPlan -> [(Store.Unspent, Address, PoolPath)] -> Natural -> Maybe [(Store.Unspent, Address, PoolPath)]
coinsToAdd ctx gen plan candidates feeByte
  | covers [] = Just []
  | otherwise = find covers (drop 1 (inits (sortOn (\(coin, _, _) -> Down coin.value) candidates)))
  where
    covers added = isRight (chooseBump ctx gen plan added feeByte)

{- The recipients paying -}

-- | Which recipients pay what the coins cannot
data PaidBy
  = -- | One of them alone, by its position among them, from 0
    OneRecipient !Int
  | -- | All of them, in equal shares, the first also paying the remainder,
    -- as --recipients-pay of preparetx has them pay the fee. bumpfee takes
    -- this way without asking with --yes when no coin can be added, and
    -- with --recipients-pay ('sharedReduction').
    AllRecipients
  deriving (Eq, Show)

-- | The recipients paying part of the fee of the replacement, one way,
-- out of what they receive. The replacement adds no coin. Offered by
-- bumpfee, they pay what the coins of the transactions replaced cannot,
-- and the replacement spends those coins in full, with no change
-- ('reductions'). With --recipients-pay, they pay what the higher fee
-- adds, and the change keeps its amount ('sharedReduction').
data Reduction = Reduction
  { reductionPaidBy :: !PaidBy,
    -- | What the recipients pay together, in satoshis
    reductionMissing :: !Natural,
    -- | Each recipient, with what the transactions replaced pay it and what
    -- the replacement pays it
    reductionRecipients :: ![(Address, Natural, Natural)]
  }
  deriving (Eq, Show)

-- | The ways the recipients can pay what the coins of the plan cannot of
-- the fee at the rate, with no coin added: each of them alone, in their
-- order, then all of them in equal shares; those that leave no recipient
-- below the dust limit of its output ('outputDustLimit'). A lone
-- recipient has one way. Refused when the coins pay the fee as they are,
-- or when no way is left.
reductions :: Ctx -> BumpPlan -> Natural -> Either String [Reduction]
reductions ctx plan feeByte = do
  missing <- missingFee ctx plan feeByte
  let ways = mapMaybe (reductionBy ctx plan missing) ([OneRecipient i | several, i <- [0 .. count - 1]] <> [AllRecipients])
  when (null ways) $ Left (cannot missing)
  return ways
  where
    count = length plan.planRecipients
    several = count > 1
    cannot missing
      | several =
          "The recipients cannot pay the " <> show missing <> " satoshis missing, together or alone, without one being left with less than " <> dustLimitText ctx plan
      | otherwise =
          "The recipient cannot pay the " <> show missing <> " satoshis missing without being left with less than " <> dustLimitText ctx plan

-- | The one way all the recipients pay what the higher fee adds, in equal
-- shares, the first also paying the remainder: the way of
-- --recipients-pay. The recipients pay the whole of it, whatever the
-- change could pay, and the change keeps its amount, as preparetx
-- --recipients-pay keeps whole what the sender gets back. A lone
-- recipient pays it all. Refused when the shares would leave a recipient
-- below the dust limit of its output, and, as 'checkBumpFee' refuses the
-- replacement, when the rate pays too little.
sharedReduction :: Ctx -> BumpPlan -> Natural -> Either String Reduction
sharedReduction ctx plan feeByte = do
  increase <- feeIncrease ctx plan feeByte
  maybe (Left (cannot increase)) Right (reductionBy ctx plan increase AllRecipients)
  where
    cannot increase
      | length plan.planRecipients > 1 =
          "The recipients cannot pay the " <> show increase <> " satoshis the higher fee adds in equal shares without one being left with less than " <> dustLimitText ctx plan
      | otherwise =
          "The recipient cannot pay the " <> show increase <> " satoshis the higher fee adds without being left with less than " <> dustLimitText ctx plan

-- | What the replacement at the rate pays beyond the fees of the
-- transactions it replaces, when it keeps their change: it spends the
-- same coins, and pays the same outputs. Refused, as 'checkBumpFee'
-- refuses the replacement, when that is too little.
feeIncrease :: Ctx -> BumpPlan -> Natural -> Either String Natural
feeIncrease ctx plan feeByte = do
  checkReplacementFee plan fee size
  return (fee - plan.planFees)
  where
    kept = keptChange plan
    changeKind = bumpChangeKind plan plan.planCoins
    -- The change is an output as long as the builder makes one of it: at
    -- the dust limit or above, as the change of the transactions replaced
    keepsOutput = kept > 0 && kept >= outputDustLimit (accountOutputKind changeKind)
    outputs = (recipientOutputKind ctx . fst <$> plan.planRecipients) <> [accountOutputKind changeKind | keepsOutput]
    size = fromIntegral (txSize (coinInput <$> plan.planCoins) outputs)
    fee = feeByte * size

-- | The change of the transactions the plan replaces, in all: what their
-- coins hold beyond what they pay their recipients and their fees
keptChange :: BumpPlan -> Natural
keptChange plan = fromMaybe 0 (total `safeSubtract` (amount + plan.planFees))
  where
    total = sum [fromIntegral coin.value | coin <- plan.planCoins]
    amount = sum (snd <$> plan.planRecipients)

-- | What the coins of the plan cannot pay of the amounts and the fee at
-- the rate, when the replacement spends them in full, with no change: its
-- fee is then that of a send-all. Refused when they pay it all.
missingFee :: Ctx -> BumpPlan -> Natural -> Either String Natural
missingFee ctx plan feeByte = do
  when (total >= amount + fee) $
    Left "The coins of the transaction pay its fee: the recipients have nothing to pay"
  return (amount + fee - total)
  where
    total = sum [fromIntegral coin.value | coin <- plan.planCoins]
    amount = sum (snd <$> plan.planRecipients)
    fee = sendAllFee ctx feeByte plan.planCoins (fst <$> plan.planRecipients)

-- | What each recipient of the plan gets when they pay the missing amount
-- the way given, when that leaves none of them below the dust limit of its
-- output
reductionBy :: Ctx -> BumpPlan -> Natural -> PaidBy -> Maybe Reduction
reductionBy ctx plan missing paidBy = do
  after <- case paidBy of
    OneRecipient i -> sequence [(addr,) <$> (if j == i then value `safeSubtract` missing else Just value) | (j, (addr, value)) <- zip [0 ..] recipients]
    AllRecipients -> either (const Nothing) Just (recipientsPayFee (fromIntegral missing) recipients)
  guard $ all (\(addr, value) -> value >= recipientDustLimit ctx addr) after
  return
    Reduction
      { reductionPaidBy = paidBy,
        reductionMissing = missing,
        reductionRecipients = [(addr, before, afterwards) | ((addr, before), (_, afterwards)) <- zip recipients after]
      }
  where
    recipients = plan.planRecipients

-- | The dust limit of the output paying a recipient
recipientDustLimit :: Ctx -> Address -> Natural
recipientDustLimit ctx addr = outputDustLimit (recipientOutputKind ctx addr)

-- | The dust limit of the recipients of the plan, as a refusal names it:
-- one number when their outputs are of one kind
dustLimitText :: Ctx -> BumpPlan -> String
dustLimitText ctx plan = case nubOrd (recipientDustLimit ctx . fst <$> plan.planRecipients) of
  [limit] -> "the dust limit of " <> show limit <> " satoshis"
  _ -> "the dust limit of its output"

{- Funding -}

-- | What pays the fee of the replacement, beyond the fees of the
-- transactions replaced
data Funding
  = -- | Their change, which nothing needs to be asked about
    ChangePays
  | -- | Confirmed coins of the portfolio, added as the user agrees
    CoinsAdded ![(Store.Unspent, Address, PoolPath)]
  | -- | The recipients, out of what they receive, one way, as the user
    -- agrees
    RecipientsReduced !Reduction
  deriving (Eq, Show)

-- | The fundings of the replacement at the fee rate, in the order bumpfee
-- offers them.
--
-- When the sender pays, as by default, the change alone comes first, when
-- it pays: nothing is asked then. The coins of the portfolio to add come
-- next, when some would do, then each way the recipients can pay. Refused
-- when nothing pays: neither the change, nor the coins, nor the
-- recipients. --yes takes the first that pays among them, and all the
-- recipients in equal shares among several.
--
-- When the recipients pay, as --recipients-pay asks, the one funding is
-- all of them paying what the higher fee adds, in equal shares
-- ('sharedReduction'). The change keeps its amount, and no coin is added.
-- When the shares cannot pay, the refusal says what pays without the
-- option, or, when nothing would pay either, gives the refusal of the
-- sender paying. A rate too low for any replacement is refused as it is.
fundings :: Ctx -> StdGen -> BumpPlan -> [(Store.Unspent, Address, PoolPath)] -> Natural -> FeePayer -> Either String [Funding]
fundings ctx gen plan candidates feeByte payer = case payer of
  SenderPays ->
    case (coinsToAdd ctx gen plan candidates feeByte, reductions ctx plan feeByte) of
      (Just [], _) -> Right [ChangePays]
      (Just added, reduced) -> Right (CoinsAdded added : (RecipientsReduced <$> fromRight [] reduced))
      (Nothing, Right reduced) -> Right (RecipientsReduced <$> reduced)
      (Nothing, Left cannot) -> Left (noCoins <> ". " <> cannot)
  RecipientsPay -> do
    _ <- feeIncrease ctx plan feeByte
    case sharedReduction ctx plan feeByte of
      Right shared -> Right [RecipientsReduced shared]
      Left cannot -> do
        offered <- fundings ctx gen plan candidates feeByte SenderPays
        Left (cannot <> ". Without --recipients-pay, " <> withoutTheOption offered)
  where
    noCoins = "The coins of the transaction cannot pay its fee at " <> show feeByte <> " sat/vB, and no confirmed coins of the portfolio would"
    -- What pays in place of the shares. A lone recipient that cannot pay
    -- its share cannot pay at all, so a recipient who pays alone is one of
    -- several.
    withoutTheOption offered
      | ChangePays `elem` offered = "the change pays it"
      | any addsCoins offered = "bumpfee offers to add confirmed coins of the portfolio"
      | otherwise = "bumpfee offers one of the recipients paying it alone"
    addsCoins (CoinsAdded _) = True
    addsCoins _ = False

-- | The signing data of the replacement at the fee rate, from the coins of
-- the plan and its funding, with the transactions those come from. Its
-- change goes to the change address of the plan, or to a new change
-- address when the transactions replaced had no change.
buildBump ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  Pool ->
  StdGen ->
  BumpPlan ->
  Funding ->
  -- | Fee per virtual byte
  Natural ->
  -- | Lock time
  Word32 ->
  ExceptT String (DB m) TxSignData
buildBump ctx cfg pool gen plan funding feeByte lockTime = do
  let (funded, added) = case funding of
        ChangePays -> (plan, [])
        CoinsAdded extra -> (plan, extra)
        RecipientsReduced reduction -> (reducedPlan reduction, [])
      found = walletCoinsOf funded added
  addedParents <- coinParents [coin | (coin, _, _) <- added]
  chosen <- liftEither $ chooseBump ctx gen funded added feeByte
  change <-
    if chosen.selection.changeAmount > 0
      then Just <$> maybe (newChange chosen.changeType) return plan.planChange
      else return Nothing
  built <- liftEither $ assemblePayment pool.poolNetwork ctx found chosen change lockTime
  return $ TxSignData built.tx (nubOrd (plan.planParents <> addedParents)) built.inputPaths built.outputPaths (Just feeByte) Nothing plan.planReplaces plan.planLabel
  where
    -- A new change address of the pool, of the type the fee was counted for
    newChange :: (MonadUnliftIO m) => AccountKind -> ExceptT String (DB m) (Address, PoolPath)
    newChange kind = do
      row <- changeAddress ctx cfg pool kind
      (addr, path) <- liftEither $ fromDBAddr pool.poolNetwork row
      return (addr, PoolPath kind path)
    -- The plan with the recipients paid what the reduction leaves them
    reducedPlan :: Reduction -> BumpPlan
    reducedPlan reduction = plan {planRecipients = [(addr, after) | (addr, _, after) <- reduction.reductionRecipients]}

-- | The satoshis per virtual byte a replacement pays for its own size,
-- beyond the fees of what it replaces, for the network to relay it: the
-- incremental relay fee of Bitcoin Core, which recent versions lowered to
-- 0.1
incrementalRate :: Natural
incrementalRate = 1

-- | Refuse a replacement the network would not take: it pays the fees of
-- the transactions it replaces, and its own size at 'incrementalRate'
checkBumpFee :: BumpPlan -> TxSummary -> Either String ()
checkBumpFee plan summary = checkReplacementFee plan summary.summaryFee summary.summarySize

-- | 'checkBumpFee' for a replacement of the given fee and virtual size,
-- before it is built
checkReplacementFee :: BumpPlan -> Natural -> Natural -> Either String ()
checkReplacementFee plan fee size =
  when (fee < needed) $
    Left $
      "The replacement pays "
        <> show fee
        <> " satoshis of fee, and must pay "
        <> show needed
        <> ": the fees of the transactions it replaces, and its own "
        <> show size
        <> " virtual bytes. Give a --fee-rate of at least "
        <> show ((needed + size - 1) `div` size)
  where
    needed = plan.planFees + incrementalRate * size

{- What bumpfee asks -}

-- | The question after 'childrenDoc' of the printer
childrenQuestion :: [Sent] -> String
childrenQuestion [_] = "Replace both with one transaction that pays the recipients of both?"
childrenQuestion _ = "Replace them all with one transaction that pays all their recipients?"

-- | The question after 'addedCoinsDoc' of the printer
addedQuestion :: [(Store.Unspent, Address, PoolPath)] -> String
addedQuestion [_] = "Add it?"
addedQuestion _ = "Add them?"

-- | The question after 'reducedDoc' of the printer: whether the lone recipient pays, or
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
