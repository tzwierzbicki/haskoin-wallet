{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Building the wallet's transactions: coin selection, change and fees.
module Haskoin.Wallet.TxBuilding where

import Conduit (MonadUnliftIO)
import Control.Arrow (second)
import Control.Monad (when, (<=<))
import Control.Monad.Except
import Control.Monad.State
import Data.Either (rights)
import Data.List (nub, partition)
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Building Transactions -}

buildTxSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Config ->
  StdGen ->
  DBAccountId ->
  [(Address, Natural)] ->
  Natural ->
  Natural ->
  Bool ->
  Natural ->
  [OutPoint] ->
  ExceptT String (DB m) TxSignData
buildTxSignData net ctx cfg gen accId rcpts feeByte dust rcptPay minConf coinControl
  | null rcpts = throwError "No recipients provided"
  | otherwise = do
      -- Get all spendable coins in the account
      allCoins <- getSpendableCoins net accId minConf
      -- Get a change address
      dbAddr <- nextFreeIntAddr ctx cfg accId
      (change, changeDeriv) <- liftEither $ fromDBAddr net dbAddr
      -- Build a transaction and pick the coins
      (tx, pickedCoins) <-
        liftEither $
          buildWalletTx net ctx gen rcpts change allCoins coinControl feeByte dust rcptPay
      -- Get the derivations of our recipients and picked coins
      rcptsDerivsE <- mapM (lift . getAddrDeriv net accId) $ fst <$> rcpts
      let rcptsDerivs = rights rcptsDerivsE -- It's OK to fail here
      inDerivs <- mapM (liftEither <=< lift . getCoinDeriv net accId) pickedCoins
      -- Check if we have a change output
      let noChange = length tx.outputs == length rcpts
          outDerivs =
            if noChange
              then rcptsDerivs
              else changeDeriv : rcptsDerivs
      -- Get the dependent transactions
      let depTxHash = (.hash) . (.outpoint) <$> tx.inputs
      depTxs <- mapM getRawTx depTxHash
      -- Return the result
      return $ TxSignData tx depTxs (nub inDerivs) (nub outDerivs) False

updateControlCoins ::
     [Store.Unspent]
  -> [OutPoint]
  -> Either String ([Store.Unspent], [Store.Unspent])
updateControlCoins allCoins [] = return ([], allCoins)
updateControlCoins allCoins control
  | length l < length control =
    Left "Selected coin does not exist or is not spendable"
  | otherwise = return (l, r)
  where
    (l, r) =
      partition (\(Store.Unspent _ op _ _ _) -> op `elem` control) allCoins

buildWalletTx ::
  Network ->
  Ctx ->
  StdGen -> -- Randomness for coin selection and change address order
  [(Address, Natural)] -> -- recipients
  Address -> -- change
  [Store.Unspent] -> -- Coins to choose from
  [OutPoint] ->
  Natural -> -- Fee per byte
  Natural -> -- Dust
  Bool -> -- Recipients Pay for Fee
  Either String (Tx, [Store.Unspent])
buildWalletTx net ctx gen rcptsN change coins coinControl feeByteN dustN rcptPay =
  flip evalStateT gen $ do
    -- If coin control was used, move those coins to the front
    (l,r) <- lift $ updateControlCoins coins $ nub coinControl
    rdmCoins <- randomShuffle r
    (pickedCoins, changeAmnt) <-
      lift $ chooseCoins tot feeCoinSel (length rcptsN + 1) False (l ++ rdmCoins)
    let nOuts =
          if changeAmnt <= dust
            then length rcptsN
            else length rcptsN + 1
        totFee =
          guessTxFee (fromIntegral feeByteN) nOuts (length pickedCoins)
    rcptsPayN <-
      if rcptPay
        then lift $ makeRcptsPay (fromIntegral totFee) rcptsN
        else return rcptsN
    let rcpts = second fromIntegral <$> rcptsPayN
        allRcpts
          | changeAmnt <= dust = rcpts
          | otherwise = (change, changeAmnt) : rcpts
        ops = (.outpoint) <$> pickedCoins
    when (any ((<= dust) . snd) allRcpts) $
      lift $
        Left "Recipient output is smaller than the dust value"
    rdmRcpts <- randomShuffle allRcpts
    tx <- lift $ buildAddrTx net ctx ops =<< mapM (addrToText2 net) rdmRcpts
    return (tx, pickedCoins)
  where
    feeCoinSel =
      if rcptPay
        then 0
        else fromIntegral feeByteN :: Word64
    dust = fromIntegral dustN :: Word64
    tot = fromIntegral $ sum $ snd <$> rcptsN :: Word64

makeRcptsPay ::
  Natural -> [(Address, Natural)] -> Either String [(Address, Natural)]
makeRcptsPay fee rcpts =
  mapM f rcpts
  where
    f (a, v) = (a,) <$> maybeToEither err (v `safeSubtract` toPay)
    err = "Recipients can't pay for the fee"
    (q, r) = fee `quotRem` fromIntegral (length rcpts)
    toPay = if r == 0 then q else q + 1
