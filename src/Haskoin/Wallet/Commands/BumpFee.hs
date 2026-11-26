{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | bumpfee: the replacement of a stuck transaction at a higher fee rate.
-- "Haskoin.Wallet.Tx.BumpFee" builds the replacement. This module asks the
-- user to agree to what pays the higher fee, and keeps the replacement
-- unsigned among the pending transactions, for signtx and broadcast to go
-- on with.
module Haskoin.Wallet.Commands.BumpFee
  ( cmdBumpFee,
  )
where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit, UnitChoice, unitOn)
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.PrettyPrinter.Base (renderOn)
import Haskoin.Wallet.Cli.PrettyPrinter.BumpFee (addedCoinsDoc, childrenDoc, reducedDoc, sharedDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (pathsSeenFrom)
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Answer (seenFromPlace)
import Haskoin.Wallet.Commands.Payment (behindTheChain, chooseFeeRate, confirmBehind, confirmFeeRate, confirmMixing, preparedAnswer)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Commands.Scope (shownAmong)
import Haskoin.Wallet.Commands.ShortId (walletTxHashOf)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Store
import Haskoin.Wallet.Tx.Building (coinWithPath)
import Haskoin.Wallet.Tx.BumpFee
import Haskoin.Wallet.Tx.CoinSelection (FeePayer (..))
import Haskoin.Wallet.Tx.FeeRates
import Haskoin.Wallet.Tx.LockTime (lockTimeFor)
import Haskoin.Wallet.Tx.Mixing (addsGroup)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (paidRateText)
import Numeric.Natural (Natural)
import System.IO (stderr)
import System.Random (initStdGen)

-- | Prepare the replacement of a stuck transaction of a portfolio at a
-- higher fee rate, as "Haskoin.Wallet.Tx.BumpFee" builds it. The
-- transaction tells its portfolio: the one that pays it, when it is in
-- the history of two. hw asks before it replaces the transactions that
-- spend the change of the stuck one, and before it adds coins of the
-- portfolio, unless --yes. When no coin is added, it asks before the
-- recipients pay out of what they receive, and which of several does,
-- unless --yes, which has them all pay in equal shares when they can.
-- --recipients-pay has the recipients pay what the higher fee adds
-- instead, in equal shares: the change keeps its amount, and no coin is
-- added ('fundings'). Every refusal comes before any question: the
-- replacement of every way to pay the fee is built first, and asked about
-- after. The last question is about the coins added: whether they are of
-- another group than the coins the transaction spends already, by their
-- tags ('confirmMixing'). The answer is that of preparetx.
cmdBumpFee :: Config -> UnitChoice -> BumpFee -> IO Response
cmdBumpFee cfg choice opts =
  runDBResponse cfg $ do
    -- Before anything is asked, not after
    for_ opts.output checkFileToWrite
    bumped <- walletTxHashOf opts.bumped
    pool <- bumpedPool bumped
    storeOn <- liftIO $ webStores cfg
    let net = pool.poolNetwork
        store = storeOn net
        unit = unitOn net choice
    unless net.replaceByFee $
      throwError "Bitcoin Cash replaces no transaction: its nodes keep the first one they see"
    stuck <- stuckTransaction store pool bumped
    liftEither $ checkReplaceable stuck
    case opts.feeRate of
      Just (FeeRateOf _) -> return ()
      _ ->
        liftIO . tell terminal $
          "The transaction pays " <> paidRateText (summaryFeeRate stuck.sentSummary)
            <> " satoshis per virtual byte: a replacement must pay at least "
            <> show (minimumRate stuck)
    feeByte <- chooseFeeRate cfg net store (minimumRate stuck) opts.feeRate
    when (feeByte < minimumRate stuck) $
      throwError $
        "The transaction pays " <> paidRateText (summaryFeeRate stuck.sentSummary)
          <> " sat/vB: give a --fee-rate of at least "
          <> show (minimumRate stuck)
    children <- spendingChildren store pool [stuck]
    refuseSpentElsewhere pool (stuck : children)
    plan <- bumpPlan cfg pool (stuck : children)
    -- A wallet behind the chain is asked about with the other questions,
    -- once nothing can refuse the replacement: it is built for a lock time
    -- of 0 meanwhile
    behind <- behindTheChain net
    gen <- liftIO initStdGen
    let (lockTime, buildGen) = lockTimeFor (either (const Nothing) id behind) gen
    -- The network takes no replacement that adds an unconfirmed coin
    confirmed <- spendableCoins pool 1
    candidates <- forM confirmed $ \coin -> (\(addr, path) -> (coin, addr, path)) <$> coinWithPath pool coin
    offered <- liftEither $ fundings buildGen plan candidates feeByte opts.recipientsPay
    built <- forM offered $ \funding -> do
      tsd <- buildBump cfg pool buildGen plan funding feeByte lockTime
      info <- txInfoOf pool tsd
      liftEither $ checkBumpFee plan info.txInfoSummary
      return (funding, tsd)
    -- The questions, once nothing can refuse the replacement any more. A
    -- change address a replacement took is given back if the answer is no,
    -- with everything else the command did.
    confirmFeeRate opts.yes feeByte
    either (confirmBehind opts.yes) (const (return ())) behind
    unless (null children) $ do
      liftIO $ renderOn stderr (childrenDoc children)
      agreed <- if opts.yes then return True else askYes terminal (ScriptOption "--yes") (childrenQuestion children)
      unless agreed $ throwError "The transaction was not replaced"
    tsd <- agreedFunding pool unit feeByte opts.yes opts.recipientsPay built
    confirmMixing opts.yes unit pool (addsGroup (addedCoins plan tsd)) "Replace the transaction anyway?" "The transaction was not replaced" tsd
    inheritTags pool tsd
    preparedAnswer pool Nothing opts.output tsd []
  where
    -- The pool the transaction is replaced in: the one that holds it on a
    -- network hw shows, or among several the one that pays it. Among
    -- several that pay it, or several that hold it when none pays, the
    -- one of the active portfolio, when it is one of them.
    bumpedPool bumped = do
      activeM <- lift activePortfolioId
      holders <- shownAmong cfg ("The transaction " <> hashRefText opts.bumped) (.poolNetwork) =<< transactionPools bumped
      paying <- filterM (fmap (maybe False (not . Map.null . summaryMyInputs . txInfoSummary)) . (`storedTxInfo` bumped)) holders
      let candidates = if null paying then holders else paying
      case candidates of
        [] -> throwError ("The wallet has no transaction " <> cs (txHashToHex bumped))
        [only] -> return only
        _
          | [ofActive] <- filter ((== activeM) . Just . (.poolPortfolio)) candidates -> return ofActive
          | otherwise -> do
              places <- mapM poolPlace candidates
              throwError ("The transaction is in " <> intercalateAnd (placeText <$> places) <> ": switch to the one to use first")

-- | Give the replacement bumpfee prepared the tags of the transactions
-- it replaces, as it gets their labels: it is the same payment, and the
-- user who tagged it rent finds it under rent whichever of the two the
-- network keeps. A replaced transaction on the network is tagged under
-- its id, and one still pending under its nosigHash. The replacement is
-- pending, so it takes them under its own nosigHash.
inheritTags :: Pool -> TxSignData -> ExceptT String (DB IO) ()
inheritTags pool tsd = do
  let replacement = nosigHash (txSignDataTx tsd)
  forM_ tsd.txSignDataReplaces $ \replaced -> do
    pendingM <- findPendingTx replaced
    let taggedUnder = replaced : [nosigHash (txSignDataTx held.pendingTxData) | Just held <- [pendingM]]
    lift $ forM_ taggedUnder $ \hash -> copyTxTags pool hash pool replacement

-- | The replacement of the funding the user agrees to, among those built,
-- each told on stderr with what it is about, to keep the --json answer
-- clean. The change needs no agreement. The coins to add are asked about
-- first, each as coins shows it, then the recipients: whether the lone
-- one pays, or which of several does, or all of them. None agreed, the
-- transaction stays as it is.
--
-- --yes agrees to the first that pays, and among several recipients to
-- all of them paying in equal shares. When the shares would leave one
-- below the dust limit, it is still asked which one pays alone: --yes
-- picks no recipient.
--
-- With --recipients-pay, the one replacement built has all the recipients
-- pay what the higher fee adds, in equal shares ('fundings'). It is told
-- as the others are, and taken without a question.
agreedFunding :: Pool -> AmountUnit -> Natural -> Bool -> FeePayer -> [(Funding, TxSignData)] -> ExceptT String (DB IO) TxSignData
agreedFunding pool unit feeByte yes payer built =
  case built of
    (ChangePays, tsd) : _ -> return tsd
    _ -> do
      let coinOffers = [(added, tsd) | (CoinsAdded added, tsd) <- built]
      fromCoins <- offerCoins coinOffers
      -- Why no coin was offered, when none was
      held <- if null coinOffers then heldBackFrom (snd <$> built) else return Nothing
      fromRecipients <- maybe (offerReductions held [(way, tsd) | (RecipientsReduced way, tsd) <- built]) (return . Just) fromCoins
      liftMaybe "The transaction was not replaced" fromRecipients
  where
    -- What the portfolio holds and could not add, since it cannot spend
    -- it now: a coin that a tag or the user locks, or one without a
    -- block yet, which the network takes in no replacement. The coins of
    -- the transactions replaced are not counted, neither those they
    -- spend nor those they made: they are the coins of the replacement
    -- itself.
    heldBackFrom [] = return Nothing
    heldBackFrom (tsd : _) = do
      let own = (.outpoint) <$> (txSignDataTx tsd).inputs
          replaced = tsd.txSignDataReplaces
      waiting <- unspendableCoins pool 1
      return $ heldBackOf [coin | (coin, _) <- waiting, coin.outpoint `notElem` own, coin.outpoint.hash `notElem` replaced]
    agreedTo asked = if yes then return True else askYes terminal (ScriptOption "--yes") asked
    net = pool.poolNetwork
    offerCoins [] = return Nothing
    offerCoins ((added, tsd) : _) = do
      views <- poolCoinViews pool [coin.outpoint | (coin, _, _) <- added]
      -- The transaction is replaced in its own portfolio, which may be
      -- another one than the active one
      seen <- seenFromPlace =<< poolPlace pool
      naming <- lift walletNaming
      liftIO $ renderOn stderr =<< addedCoinsDoc (pathsSeenFrom naming seen) net unit feeByte views
      agreed <- agreedTo (addedQuestion added)
      return $ if agreed then Just tsd else Nothing
    offerReductions _ [] = return Nothing
    -- The one way --recipients-pay leaves: nothing to choose
    offerReductions held ((way, tsd) : _)
      | payer == RecipientsPay = takeShares held way tsd
    offerReductions held ways@((way, tsd) : _) = case way.reductionRecipients of
      [_] -> do
        liftIO $ renderOn stderr (reducedDoc net unit feeByte held (fst <$> ways))
        agreed <- agreedTo (reducedQuestion (fst <$> ways))
        return $ if agreed then Just tsd else Nothing
      _
        | yes, (shared, sharedTsd) : _ <- [(w, t) | (w, t) <- ways, w.reductionPaidBy == AllRecipients] ->
            takeShares held shared sharedTsd
        | otherwise -> do
            liftIO $ renderOn stderr (reducedDoc net unit feeByte held (fst <$> ways))
            -- No is the default: an empty answer keeps the transaction.
            -- --yes picks no recipient, so nothing answers in a script
            -- that gave it.
            let inScript = if yes then NoScriptOption "to choose the recipient who pays" else ScriptOption "--yes"
            askChoice terminal inScript (reducedQuestion (fst <$> ways)) ((["no", "n"], Nothing) : [(reductionAnswers net w, Just t) | (w, t) <- ways])
    -- All the recipients paying in equal shares, told and taken without a
    -- question, as --yes or --recipients-pay says
    takeShares held way tsd = do
      liftIO $ renderOn stderr (sharedDoc payer net unit feeByte held way)
      return (Just tsd)
