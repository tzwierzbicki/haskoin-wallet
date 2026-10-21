{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Building the wallet's transactions around the coin selection of
-- "Haskoin.Wallet.CoinSelection".
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
import Control.Monad (when, zipWithM)
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.CoinSelection
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO (TxSignData (..))
import Haskoin.Wallet.Util (addrToText2, randomShuffle, safeSubtract)
import Numeric.Natural (Natural)
import System.Random (StdGen, splitGen)

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
  CoinSelection ->
  DBAccountId ->
  -- | The recipients and their amounts
  [(Address, Natural)] ->
  -- | Fee per byte
  Natural ->
  -- | Dust
  Natural ->
  FeePayer ->
  -- | Minimum confirmations of the coins
  Natural ->
  -- | Coin control: the coins to spend, instead of choosing them
  [OutPoint] ->
  ExceptT String (DB m) TxSignData
buildTxSignData net ctx cfg gen strategy accId recipients feeByte dust payer minConf coinControl = do
  when (null recipients) $ throwError "No recipients provided"
  found <- fetchWalletCoins net ctx cfg accId (fst <$> recipients) minConf
  built <- liftEither $ buildWalletTx net ctx gen strategy found recipients feeByte dust payer coinControl
  spentFrom <- mapM getRawTx $ nub $ (.outpoint.hash) <$> built.tx.inputs
  return $ TxSignData built.tx spentFrom built.inputPaths built.outputPaths (Just feeByte) False

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

-- | The fee rate at which spending a coin is expected to cost in the future,
-- in satoshis per byte: the value Bitcoin Core uses. It only weighs
-- consolidating small coins now against spending them later, so it is not a
-- setting of the user.
longTermRate :: Word64
longTermRate = 10

-- | Build a transaction paying the recipients from the wallet's coins. The
-- coins are chosen by the strategy, or are exactly those under coin control.
-- The change goes to the change address, or to the fee when it is below the
-- dust limit, and the outputs are shuffled. When the recipients pay the fee,
-- it is deducted from their amounts in equal parts, the first recipient also
-- paying the remainder. A fee rate of zero is refused: the network would not
-- relay the transaction.
buildWalletTx ::
  Network ->
  Ctx ->
  StdGen ->
  CoinSelection ->
  WalletCoins ->
  -- | The recipients and their amounts
  [(Address, Natural)] ->
  -- | Fee per byte
  Natural ->
  -- | Dust
  Natural ->
  FeePayer ->
  -- | Coin control: the coins to spend, instead of choosing them
  [OutPoint] ->
  Either String BuiltTx
buildWalletTx net ctx gen strategy found recipients feeByte dust payer coinControl =
  flip evalStateT gen $ do
    when (null recipients) $ throwError "No recipients provided"
    when (feeByte == 0) $ throwError "The fee rate must be at least 1 satoshi per byte"
    -- The coin selection draws from a generator of its own; the outputs are
    -- shuffled with what is left
    drawGen <- state splitGen
    chosen <- lift $ case coinControl of
      [] -> selectCoins strategy drawGen payment found.coins
      _ -> spendCoins payment =<< controlledCoins found.coins coinControl
    let hasChange = chosen.changeAmount > 0
    paid <- lift $ case payer of
      SenderPays -> return recipients
      RecipientsPay -> recipientsPayFee (payment.amount - chosen.paid) recipients
    let outputs = [(fst found.change, chosen.changeAmount) | hasChange] <> (second fromIntegral <$> paid)
    when (any ((< payment.dust) . snd) outputs) $
      throwError "Recipient output is smaller than the dust value"
    shuffledOutputs <- randomShuffle outputs
    tx <- lift $ buildAddrTx net ctx ((.outpoint) <$> chosen.selected) =<< mapM (addrToText2 net) shuffledOutputs
    inputPaths <- lift $ nub <$> mapM coinPath chosen.selected
    let outputPaths =
          nub $ [snd found.change | hasChange] <> mapMaybe ((`Map.lookup` found.recipientPaths) . fst) recipients
    return BuiltTx {tx, spentCoins = chosen.selected, inputPaths, outputPaths}
  where
    payment =
      Payment
        { rates = FeeRates {feeRate = fromIntegral feeByte, longTermFeeRate = longTermRate},
          payer,
          dust = fromIntegral dust,
          outputCount = length recipients,
          amount = sum $ fromIntegral . snd <$> recipients
        }
    coinPath :: Store.Unspent -> Either String SoftPath
    coinPath coin =
      maybeToEither "A chosen coin has no derivation path" $
        (`Map.lookup` found.coinPaths) =<< coin.address

-- | The coins under coin control, which must all be spendable
controlledCoins :: [Store.Unspent] -> [OutPoint] -> Either String [Store.Unspent]
controlledCoins coins control
  | length controlled < length (nub control) = Left "Selected coin does not exist or is not spendable"
  | otherwise = Right controlled
  where
    controlled = filter ((`elem` control) . (.outpoint)) coins

-- | Deduct the fee from the recipients in equal parts; the first recipient
-- also pays the remainder, like Bitcoin Core
recipientsPayFee :: Word64 -> [(Address, Natural)] -> Either String [(Address, Natural)]
recipientsPayFee fee recipients = zipWithM pay (remainder : repeat 0) recipients
  where
    (each, remainder) = fromIntegral fee `quotRem` fromIntegral (length recipients) :: (Natural, Natural)
    pay extra (addr, amount) =
      (addr,) <$> maybeToEither "Recipients can't pay for the fee" (amount `safeSubtract` (each + extra))
