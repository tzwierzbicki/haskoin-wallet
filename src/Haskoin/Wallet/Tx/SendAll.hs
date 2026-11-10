{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Sending every coin of a portfolio on a network: preparesendall. The
-- coins may be of both types, P2PKH and P2WPKH, each sized by its own
-- ("Haskoin.Wallet.Tx.CoinSelection".'coinInput'). The transaction spends
-- the coins in full, with no change. The recipients with an amount fixed by
-- the user get it as typed, and the others share what remains once those
-- amounts and the fee are paid. The fee is theirs alone: when what remains
-- does not cover it, nothing is built, and a fixed amount is never cut.
--
-- A coin worth no more than the fee to spend it, its input at the fee rate,
-- is a dust coin: spending it lowers what the recipients get. Whether a coin
-- is dust depends on the rate. preparesendall asks the user whether to spend
-- the dust coins too, after telling what each choice costs ('dustReport').
-- Its answer lists every coin left in the portfolio, and why
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
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Address (poolPaths)
import Haskoin.Wallet.Database.Coin (coinParents, coinsSpentBy)
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath)
import Haskoin.Wallet.Tx.Building (BuiltTx (..), coinWithPath)
import Haskoin.Wallet.Tx.CoinSelection (coinInput, coinKinds)
import Haskoin.Wallet.Tx.LockTime (withHeader)
import Haskoin.Wallet.Tx.Recipient (Share (..), shareAmounts)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (accountOutputKind, checkFeeRate, checkStandardSize, inputVirtualSize, outputDustLimit, recipientOutputKind, txSize)
import Haskoin.Wallet.Util (addrToTextE, randomShuffle)
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Dust coins -}

-- | Whether a coin is a dust coin at the fee rate: worth no more than the
-- fee of its input, which its own kind sizes
isDustCoin :: Natural -> Store.Unspent -> Bool
isDustCoin feeByte coin = fromIntegral coin.value <= inputFee feeByte coin

-- | The fee of the input of a coin at the fee rate: 148 virtual bytes for
-- a P2PKH coin, of a BIP44 account, and 68 for a P2WPKH one, of a BIP84
-- account
inputFee :: Natural -> Store.Unspent -> Natural
inputFee feeByte coin = feeByte * fromIntegral (inputVirtualSize (coinInput coin))

-- | The highest fee rate at which a coin is worth spending, its input
-- costing less than its value: 0 when no rate makes it worth spending
worthSpendingUpTo :: Store.Unspent -> Natural
worthSpendingUpTo coin
  | coin.value == 0 = 0
  | otherwise = (fromIntegral coin.value - 1) `div` fromIntegral (inputVirtualSize (coinInput coin))

-- | The value of coins, in satoshis
valueOf :: [Store.Unspent] -> Natural
valueOf coins = sum [fromIntegral coin.value | coin <- coins]

{- Amounts -}

-- | The fee of the transaction spending the coins to the recipients, at
-- the fee rate, each input of the kind of its coin
sendAllFee :: Ctx -> Natural -> [Store.Unspent] -> [Address] -> Natural
sendAllFee ctx feeByte coins recipients =
  feeByte * fromIntegral (txSize (coinInput <$> coins) (recipientOutputKind ctx <$> recipients))

-- | The amount of each recipient when the coins pay them and their fee at
-- the rate: see 'shareAmounts'
sendAllAmounts :: Ctx -> Natural -> [Store.Unspent] -> [(Address, Share)] -> Either String [(Address, Natural)]
sendAllAmounts ctx feeByte coins recipients =
  shareAmounts (valueOf coins) (sendAllFee ctx feeByte coins (fst <$> recipients)) (outputDustLimit . recipientOutputKind ctx) recipients

{- Building -}

-- | What the builder of a send-all needs from the wallet database
data SendAllCoins = SendAllCoins
  { -- | The coins to send, every one of them
    coins :: ![Store.Unspent],
    -- | The path in the pool of every address holding one of the coins
    coinPaths :: !(Map Address PoolPath),
    -- | The recipients that are addresses of the pool, with their paths
    recipientPaths :: !(Map Address PoolPath)
  }
  deriving (Eq, Show)

-- | Build the transaction spending all the coins to the recipients, with no
-- change, its outputs shuffled (see 'sendAllAmounts'), with the lock time
-- given (see "Haskoin.Wallet.Tx.LockTime"). A fee rate no transaction can
-- be built with is refused, and so are more coins than a transaction nodes
-- relay can hold ('checkStandardSize').
buildSendAll :: Network -> Ctx -> StdGen -> SendAllCoins -> [(Address, Share)] -> Natural -> Word32 -> Either String BuiltTx
buildSendAll net ctx gen found recipients feeByte lockTime = do
  checkFeeRate feeByte
  when (null found.coins) $ Left "There are no coins to send"
  checkStandardSize (coinInput <$> found.coins) (recipientOutputKind ctx . fst <$> recipients)
  amounts <- sendAllAmounts ctx feeByte found.coins recipients
  tx <- withHeader net lockTime <$> (buildAddrTx net ctx ((.outpoint) <$> found.coins) =<< mapM outputText (evalState (randomShuffle amounts) gen))
  inputPaths <- nub <$> mapM coinPath found.coins
  let outputPaths = nub $ mapMaybe ((`Map.lookup` found.recipientPaths) . fst) recipients
  return BuiltTx {tx, spentCoins = found.coins, inputPaths, outputPaths}
  where
    -- The builder of haskoin takes the recipients as address text
    outputText (addr, amount) = (,fromIntegral amount) <$> addrToTextE net addr
    coinPath :: Store.Unspent -> Either String PoolPath
    coinPath coin = do
      addr <- maybeToEither "A coin to send has no address" coin.address
      maybeToEither "A coin to send has no derivation path" $ Map.lookup addr found.coinPaths

-- | The signing data of the transaction sending coins of the pool to
-- the recipients, with the transactions the coins come from
buildSendAllSignData ::
  (MonadUnliftIO m) =>
  Ctx ->
  StdGen ->
  Pool ->
  -- | The coins to send
  [Store.Unspent] ->
  [(Address, Share)] ->
  -- | Fee per virtual byte
  Natural ->
  -- | Lock time
  Word32 ->
  ExceptT String (DB m) TxSignData
buildSendAllSignData ctx gen pool coins recipients feeByte lockTime = do
  coinPaths <- Map.fromList <$> mapM (coinWithPath pool) coins
  recipientPaths <- poolPaths pool (fst <$> recipients)
  built <- liftEither $ buildSendAll pool.poolNetwork ctx gen SendAllCoins {coins, coinPaths, recipientPaths} recipients feeByte lockTime
  spentFrom <- coinParents (coinsSpentBy built.tx coins)
  return $ TxSignData built.tx spentFrom built.inputPaths built.outputPaths (Just feeByte) Nothing [] noLabel

{- The question about dust coins -}

-- | What spending the dust coins too costs, and what leaving them does, for
-- the question preparesendall asks
data DustReport = DustReport
  { -- | The type of the address the dust coins could be sent to later:
    -- their own type when they are all of one, bip44 otherwise
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
    -- | Whether other coins stay in the portfolio anyway, locked ones for
    -- instance
    othersStay :: !Bool,
    -- | The highest fee rate at which the dust coins alone can be sent later,
    -- to one address of the type of 'reportKind', leaving it at least the
    -- dust limit; none when no rate can
    laterRate :: !(Maybe Natural),
    -- | The dust limit of an output of that type, in satoshis: the least
    -- the dust coins must leave when sent later
    dustLimit :: !Natural
  }
  deriving (Eq, Show)

-- | The report on the dust coins of a pool, from the coins worth spending
-- and the dust coins, once both they and all the coins together can pay
-- the recipients: see 'sendAllAmounts'
dustReport :: Ctx -> Natural -> [(Address, Share)] -> [Store.Unspent] -> [Store.Unspent] -> Bool -> DustReport
dustReport ctx feeByte recipients worth dustCoins othersStay =
  DustReport
    { reportKind = kind,
      rate = feeByte,
      dustCoins = sortOn (Down . (.value)) dustCoins,
      addedFee = feeOf (worth <> dustCoins) - feeOf worth,
      withDust = remaining (worth <> dustCoins),
      withoutDust = remaining worth,
      othersStay,
      laterRate,
      dustLimit = aloneDust
    }
  where
    kind = case coinKinds dustCoins of
      [only] -> only
      _ -> Bip44
    feeOf coins = sendAllFee ctx feeByte coins (fst <$> recipients)
    fixedTotal = sum [amount | (_, Fixed amount) <- recipients]
    -- Both sets of coins pay the recipients: nothing is below zero
    remaining coins = fromInteger $ max 0 $ toInteger (valueOf coins) - toInteger fixedTotal - toInteger (feeOf coins)
    dustValue = valueOf dustCoins
    -- The dust coins alone, paying one address of that type
    aloneSize = fromIntegral $ txSize (coinInput <$> dustCoins) [accountOutputKind kind]
    -- The dust limit of that address: the least the dust coins must leave it
    aloneDust = outputDustLimit (accountOutputKind kind)
    laterRate
      | dustValue >= aloneDust + aloneSize = Just ((dustValue - aloneDust) `div` aloneSize)
      | otherwise = Nothing

-- | The question after the report
dustQuestion :: DustReport -> String
dustQuestion report
  | length report.dustCoins == 1 = "Spend it too?"
  | otherwise = "Spend them too?"
