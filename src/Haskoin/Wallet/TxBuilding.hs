{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Building the wallet's transactions: coin selection, change and fees.
--
-- 'buildTxSignData' is the entry point of preparetx. It fetches what the
-- builder needs from the database ('fetchWalletCoins'), builds the
-- transaction ('buildWalletTx', which is pure), and fetches the transactions
-- the chosen coins come from.
module Haskoin.Wallet.TxBuilding
  ( BuiltTx (..),
    WalletCoins (..),
    buildTxSignData,
    buildWalletTx,
    fetchWalletCoins,
  )
where

import Conduit (MonadUnliftIO)
import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO (TxSignData (..))
import Haskoin.Wallet.Util (addrToText2, randomShuffle, safeSubtract)
import Numeric.Natural (Natural)
import System.Random (StdGen)

-- | What the builder needs from the wallet database
data WalletCoins = WalletCoins
  { -- | The spendable coins of the account
    coins :: ![Store.Unspent],
    -- | The derivation path of every address holding one of the coins
    coinPaths :: !(Map Address SoftPath),
    -- | The next free change address of the account, with its path
    change :: !(Address, SoftPath),
    -- | The recipients that are addresses of the account, with their paths
    recipientPaths :: !(Map Address SoftPath)
  }
  deriving (Eq, Show)

-- | A transaction built by the wallet, before the transactions it spends
-- from are fetched
data BuiltTx = BuiltTx
  { tx :: !Tx,
    -- | The coins the transaction spends, in the order of its inputs
    spentCoins :: ![Store.Unspent],
    inputPaths :: ![SoftPath],
    outputPaths :: ![SoftPath]
  }
  deriving (Eq, Show)

{- Fetching -}

-- | The signing data of a transaction paying the recipients from the
-- account's coins
buildTxSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Config ->
  StdGen ->
  DBAccountId ->
  -- | The recipients and their amounts
  [(Address, Natural)] ->
  -- | Fee per byte
  Natural ->
  -- | Dust
  Natural ->
  -- | The recipients pay the fee
  Bool ->
  -- | Minimum confirmations of the coins
  Natural ->
  -- | Coin control: coins that must be spent
  [OutPoint] ->
  ExceptT String (DB m) TxSignData
buildTxSignData net ctx cfg gen accId recipients feeByte dust recipientsPay minConf coinControl = do
  when (null recipients) $ throwError "No recipients provided"
  found <- fetchWalletCoins net ctx cfg accId (fst <$> recipients) minConf
  built <- liftEither $ buildWalletTx net ctx gen found recipients feeByte dust recipientsPay coinControl
  spentFrom <- mapM getRawTx $ nub $ (.outpoint.hash) <$> built.tx.inputs
  return $ TxSignData built.tx spentFrom built.inputPaths built.outputPaths False

-- | Fetch the spendable coins of the account with their derivation paths,
-- its next free change address, and the paths of the recipients that are
-- its own addresses
fetchWalletCoins ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Config ->
  DBAccountId ->
  -- | The recipients
  [Address] ->
  -- | Minimum confirmations of the coins
  Natural ->
  ExceptT String (DB m) WalletCoins
fetchWalletCoins net ctx cfg accId recipients minConf = do
  coins <- getSpendableCoins net accId minConf
  coinPaths <- Map.fromList <$> mapM (coinWithPath net accId) coins
  change <- liftEither . fromDBAddr net =<< nextFreeIntAddr ctx cfg accId
  recipientPaths <- walletPaths net accId recipients
  return WalletCoins {coins, coinPaths, change, recipientPaths}

-- | The address of a coin of the account, with its derivation path
coinWithPath ::
  (MonadUnliftIO m) =>
  Network -> DBAccountId -> Store.Unspent -> ExceptT String (DB m) (Address, SoftPath)
coinWithPath net accId coin = do
  addr <- liftEither $ maybeToEither "A spendable coin has no address" coin.address
  path <- liftEither =<< lift (getCoinDeriv net accId coin)
  return (addr, path)

{- Building -}

-- | Build a transaction paying the recipients from the wallet's coins. The
-- coins under coin control are spent first; the others are tried in random
-- order until the recipients and the fee are covered. The change, when above
-- the dust limit, goes to the change address, and the outputs are shuffled.
-- When the recipients pay the fee, it is deducted from their amounts in equal
-- parts.
buildWalletTx ::
  Network ->
  Ctx ->
  StdGen ->
  WalletCoins ->
  -- | The recipients and their amounts
  [(Address, Natural)] ->
  -- | Fee per byte
  Natural ->
  -- | Dust
  Natural ->
  -- | The recipients pay the fee
  Bool ->
  -- | Coin control: coins that must be spent
  [OutPoint] ->
  Either String BuiltTx
buildWalletTx net ctx gen found recipients feeByte dust recipientsPay coinControl =
  flip evalStateT gen $ do
    when (null recipients) $ throwError "No recipients provided"
    (controlled, others) <- lift $ controlledCoins found.coins coinControl
    candidates <- (controlled <>) <$> randomShuffle others
    -- When the recipients pay the fee, the coins only need to cover their
    -- amounts: the fee comes out of those amounts
    let feeRateToCover = if recipientsPay then 0 else feeRate
    (spent, changeAmount) <-
      lift $ chooseCoins (sum amounts) feeRateToCover (length recipients + 1) False candidates
    let hasChange = changeAmount > dustLimit
        fee = guessTxFee feeRate (length recipients + fromEnum hasChange) (length spent)
    paid <- lift $ if recipientsPay then recipientsPayFee fee recipients else return recipients
    let outputs = [(fst found.change, changeAmount) | hasChange] <> (second fromIntegral <$> paid)
    when (any ((<= dustLimit) . snd) outputs) $
      throwError "Recipient output is smaller than the dust value"
    shuffledOutputs <- randomShuffle outputs
    tx <- lift $ buildAddrTx net ctx ((.outpoint) <$> spent) =<< mapM (addrToText2 net) shuffledOutputs
    inputPaths <- lift $ nub <$> mapM coinPath spent
    let outputPaths =
          nub $ [snd found.change | hasChange] <> mapMaybe ((`Map.lookup` found.recipientPaths) . fst) recipients
    return BuiltTx {tx, spentCoins = spent, inputPaths, outputPaths}
  where
    amounts = fromIntegral . snd <$> recipients :: [Word64]
    feeRate = fromIntegral feeByte :: Word64
    dustLimit = fromIntegral dust :: Word64
    coinPath :: Store.Unspent -> Either String SoftPath
    coinPath coin =
      maybeToEither "A chosen coin has no derivation path" $
        (`Map.lookup` found.coinPaths) =<< coin.address

-- | Split the coins into those under coin control, which must all be
-- spendable, and the others
controlledCoins :: [Store.Unspent] -> [OutPoint] -> Either String ([Store.Unspent], [Store.Unspent])
controlledCoins coins control
  | length controlled < length (nub control) = Left "Selected coin does not exist or is not spendable"
  | otherwise = Right (controlled, others)
  where
    (controlled, others) = partition ((`elem` control) . (.outpoint)) coins

-- | Deduct the fee from the recipients, in equal parts rounded up
recipientsPayFee :: Word64 -> [(Address, Natural)] -> Either String [(Address, Natural)]
recipientsPayFee fee recipients = mapM pay recipients
  where
    (each, remainder) = fromIntegral fee `quotRem` fromIntegral (length recipients) :: (Natural, Natural)
    share = if remainder == 0 then each else each + 1
    pay (addr, amount) =
      (addr,) <$> maybeToEither "Recipients can't pay for the fee" (amount `safeSubtract` share)
