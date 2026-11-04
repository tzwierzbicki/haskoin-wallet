{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Sending every coin of an account: preparesendall. The transaction spends
-- the coins in full, with no change. The recipients with an amount fixed by
-- the user get it as typed, and the others share what remains once those
-- amounts and the fee are paid. The fee is theirs alone: when what remains
-- does not cover it, nothing is built, and a fixed amount is never cut.
--
-- A coin worth no more than the fee to spend it, its input at the fee rate,
-- is a dust coin: spending it lowers what the recipients get. Whether a coin
-- is dust depends on the rate. preparesendall asks the user whether to spend
-- the dust coins too, after telling what each choice costs ('dustReport').
-- Its answer lists every coin left in the account, and why
-- ("Haskoin.Wallet.Database.Coin".'leftCoins').
module Haskoin.Wallet.Tx.SendAll
  ( isDustCoin,
    sendAllFee,
    sendAllAmounts,
    SendAllCoins (..),
    buildSendAll,
    buildSendAllSignData,
    DustReport (..),
    dustReport,
    dustQuestion,
    inputFee,
    valueOf,
    worthSpendingUpTo,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State (evalState)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Word (Word32)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account (accountKind, getAccountById)
import Haskoin.Wallet.Database.Address (walletPaths)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.RawTx (getRawTx)
import Haskoin.Wallet.Keys (AccountKind)
import Haskoin.Wallet.Tx.LockTime (withHeader)
import Haskoin.Wallet.Tx.Recipient (Share (..), shareAmounts)
import Haskoin.Wallet.Tx.Building (BuiltTx (..), coinWithPath, recipientOutputKind)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (accountInput, accountInputSize, accountOutputKind, checkFeeRateAndDust, txSize)
import Haskoin.Wallet.Util (addrToTextE, randomShuffle)
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Dust coins -}

-- | Whether a coin of an account of the kind is a dust coin at the fee
-- rate: worth no more than the fee of its input
isDustCoin :: AccountKind -> Natural -> Store.Unspent -> Bool
isDustCoin kind feeByte coin = fromIntegral coin.value <= inputFee kind feeByte

-- | The fee of the input of a coin of an account of the kind at the fee
-- rate: 148 virtual bytes for a BIP44 account, 68 for a BIP84 one
inputFee :: AccountKind -> Natural -> Natural
inputFee kind feeByte = feeByte * fromIntegral (accountInputSize kind)

-- | The highest fee rate at which a coin of an account of the kind is
-- worth spending, its input costing less than its value: 0 when no rate
-- makes it worth spending
worthSpendingUpTo :: AccountKind -> Store.Unspent -> Natural
worthSpendingUpTo kind coin
  | coin.value == 0 = 0
  | otherwise = (fromIntegral coin.value - 1) `div` fromIntegral (accountInputSize kind)

-- | The value of coins, in satoshis
valueOf :: [Store.Unspent] -> Natural
valueOf coins = sum [fromIntegral coin.value | coin <- coins]

{- Amounts -}

-- | The fee of the transaction spending the coins of an account of the
-- kind to the recipients, at the fee rate
sendAllFee :: Ctx -> AccountKind -> Natural -> [Store.Unspent] -> [Address] -> Natural
sendAllFee ctx kind feeByte coins recipients =
  feeByte * fromIntegral (txSize [accountInput kind | _ <- coins] (recipientOutputKind ctx <$> recipients))

-- | The amount of each recipient when the coins of an account of the kind
-- pay them and their fee at the rate: see 'shareAmounts'
sendAllAmounts :: Ctx -> AccountKind -> Natural -> Natural -> [Store.Unspent] -> [(Address, Share)] -> Either String [(Address, Natural)]
sendAllAmounts ctx kind feeByte dust coins recipients =
  shareAmounts (valueOf coins) (sendAllFee ctx kind feeByte coins (fst <$> recipients)) dust recipients

{- Building -}

-- | What the builder of a send-all needs from the wallet database
data SendAllCoins = SendAllCoins
  { -- | The kind of the account, which sizes its inputs
    ownKind :: !AccountKind,
    -- | The coins to send, every one of them
    coins :: ![Store.Unspent],
    -- | The derivation path of every address holding one of the coins
    coinPaths :: !(Map Address SoftPath),
    -- | The recipients that are addresses of the account, with their paths
    recipientPaths :: !(Map Address SoftPath)
  }
  deriving (Eq, Show)

-- | Build the transaction spending all the coins to the recipients, with no
-- change, its outputs shuffled (see 'sendAllAmounts'), with the lock time
-- given (see "Haskoin.Wallet.Tx.LockTime"). A fee rate or a dust limit no
-- transaction can be built with is refused.
buildSendAll :: Network -> Ctx -> StdGen -> SendAllCoins -> [(Address, Share)] -> Natural -> Natural -> Word32 -> Either String BuiltTx
buildSendAll net ctx gen found recipients feeByte dust lockTime = do
  checkFeeRateAndDust feeByte dust
  when (null found.coins) $ Left "There are no coins to send"
  amounts <- sendAllAmounts ctx found.ownKind feeByte dust found.coins recipients
  tx <- withHeader net lockTime <$> (buildAddrTx net ctx ((.outpoint) <$> found.coins) =<< mapM outputText (evalState (randomShuffle amounts) gen))
  inputPaths <- nub <$> mapM coinPath found.coins
  let outputPaths = nub $ mapMaybe ((`Map.lookup` found.recipientPaths) . fst) recipients
  return BuiltTx {tx, spentCoins = found.coins, inputPaths, outputPaths}
  where
    -- The builder of haskoin takes the recipients as address text
    outputText (addr, amount) = (,fromIntegral amount) <$> addrToTextE net addr
    coinPath :: Store.Unspent -> Either String SoftPath
    coinPath coin = do
      addr <- maybeToEither "A coin to send has no address" coin.address
      maybeToEither "A coin to send has no derivation path" $ Map.lookup addr found.coinPaths

-- | The signing data of the transaction sending coins of the account to
-- the recipients, with the transactions the coins come from
buildSendAllSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  StdGen ->
  DBAccountId ->
  -- | The coins to send
  [Store.Unspent] ->
  [(Address, Share)] ->
  -- | Fee per virtual byte
  Natural ->
  -- | Dust
  Natural ->
  -- | Lock time
  Word32 ->
  ExceptT String (DB m) TxSignData
buildSendAllSignData net ctx gen accId coins recipients feeByte dust lockTime = do
  ownKind <- accountKind <$> getAccountById accId
  coinPaths <- Map.fromList <$> mapM (coinWithPath net accId) coins
  recipientPaths <- walletPaths net accId (fst <$> recipients)
  built <- liftEither $ buildSendAll net ctx gen SendAllCoins {ownKind, coins, coinPaths, recipientPaths} recipients feeByte dust lockTime
  spentFrom <- mapM getRawTx $ nub $ (.outpoint.hash) <$> built.tx.inputs
  return $ TxSignData built.tx spentFrom built.inputPaths built.outputPaths (Just feeByte) Nothing [] noLabel

{- The question about dust coins -}

-- | What spending the dust coins too costs, and what leaving them does, for
-- the question preparesendall asks
data DustReport = DustReport
  { -- | The kind of the account, which sizes the inputs of its coins
    reportKind :: !AccountKind,
    -- | The fee rate, in satoshis per virtual byte
    rate :: !Natural,
    -- | The dust coins, the largest first
    dustCoins :: ![Store.Unspent],
    -- | The fee their inputs add
    addedFee :: !Natural,
    -- | What remains for the recipients without an amount, with the dust
    -- coins spent
    withDust :: !Natural,
    -- | The same, with the dust coins left
    withoutDust :: !Natural,
    -- | Whether other coins stay in the account anyway, locked ones for
    -- instance
    othersStay :: !Bool,
    -- | The highest fee rate at which the dust coins alone can be sent later,
    -- to one address of the kind of the account, leaving it at least the
    -- dust limit; none when no rate can
    laterRate :: !(Maybe Natural),
    -- | The dust limit, in satoshis
    dustLimit :: !Natural
  }
  deriving (Eq, Show)

-- | The report on the dust coins of an account of the kind, from the coins
-- worth spending and the dust coins, once both they and all the coins
-- together can pay the recipients: see 'sendAllAmounts'
dustReport :: Ctx -> AccountKind -> Natural -> Natural -> [(Address, Share)] -> [Store.Unspent] -> [Store.Unspent] -> Bool -> DustReport
dustReport ctx kind feeByte dust recipients worth dustCoins othersStay =
  DustReport
    { reportKind = kind,
      rate = feeByte,
      dustCoins = sortOn (Down . (.value)) dustCoins,
      addedFee = feeOf (worth <> dustCoins) - feeOf worth,
      withDust = remaining (worth <> dustCoins),
      withoutDust = remaining worth,
      othersStay,
      laterRate,
      dustLimit = dust
    }
  where
    feeOf coins = sendAllFee ctx kind feeByte coins (fst <$> recipients)
    fixedTotal = sum [amount | (_, Fixed amount) <- recipients]
    -- Both sets of coins pay the recipients: nothing is below zero
    remaining coins = fromInteger $ max 0 $ toInteger (valueOf coins) - toInteger fixedTotal - toInteger (feeOf coins)
    dustValue = valueOf dustCoins
    -- The dust coins alone, paying one address of the kind of the account
    aloneSize = fromIntegral $ txSize [accountInput kind | _ <- dustCoins] [accountOutputKind kind]
    laterRate
      | dustValue >= dust + aloneSize = Just ((dustValue - dust) `div` aloneSize)
      | otherwise = Nothing

-- | The question after the report
dustQuestion :: DustReport -> String
dustQuestion report
  | length report.dustCoins == 1 = "Spend it too?"
  | otherwise = "Spend them too?"
