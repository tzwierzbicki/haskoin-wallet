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
-- ("Haskoin.Wallet.Coin".'leftCoins').
module Haskoin.Wallet.TxSendAll
  ( isDustCoin,
    sendAllFee,
    sendAllAmounts,
    SendAllCoins (..),
    buildSendAll,
    buildSendAllSignData,
    DustReport (..),
    dustReport,
    dustReportLines,
    dustQuestion,
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
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Address (walletPaths)
import Haskoin.Wallet.Coin (coinText)
import Haskoin.Wallet.Database
import Haskoin.Wallet.RawTx (getRawTx)
import Haskoin.Wallet.Recipient (Share (..), shareAmounts)
import Haskoin.Wallet.TxBuilding (BuiltTx (..), coinWithPath, recipientOutputSize)
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSize (checkFeeRateAndDust, p2pkhInput, p2pkhInputSize, p2pkhOutputSize, txSize)
import Haskoin.Wallet.Util (addrToTextE, randomShuffle, withInputSequence)
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Dust coins -}

-- | Whether a coin of the wallet is a dust coin at the fee rate: worth no
-- more than the fee of its input. The coins of the wallet are all P2PKH.
isDustCoin :: Natural -> Store.Unspent -> Bool
isDustCoin feeByte coin = fromIntegral coin.value <= inputFee feeByte

-- | The fee of the input of a coin of the wallet, 148 bytes, at the fee rate
inputFee :: Natural -> Natural
inputFee feeByte = feeByte * fromIntegral p2pkhInputSize

-- | The highest fee rate at which a coin is worth spending, its input
-- costing less than its value: 0 when no rate makes it worth spending
worthSpendingUpTo :: Store.Unspent -> Natural
worthSpendingUpTo coin
  | coin.value == 0 = 0
  | otherwise = (fromIntegral coin.value - 1) `div` fromIntegral p2pkhInputSize

-- | The value of coins, in satoshis
valueOf :: [Store.Unspent] -> Natural
valueOf coins = sum [fromIntegral coin.value | coin <- coins]

{- Amounts -}

-- | The fee of the transaction spending the coins to the recipients, at the
-- fee rate
sendAllFee :: Ctx -> Natural -> [Store.Unspent] -> [Address] -> Natural
sendAllFee ctx feeByte coins recipients =
  feeByte * fromIntegral (txSize [p2pkhInput | _ <- coins] (recipientOutputSize ctx <$> recipients))

-- | The amount of each recipient when the coins pay them and their fee at
-- the rate: see 'shareAmounts'
sendAllAmounts :: Ctx -> Natural -> Natural -> [Store.Unspent] -> [(Address, Share)] -> Either String [(Address, Natural)]
sendAllAmounts ctx feeByte dust coins recipients =
  shareAmounts (valueOf coins) (sendAllFee ctx feeByte coins (fst <$> recipients)) dust recipients

{- Building -}

-- | What the builder of a send-all needs from the wallet database
data SendAllCoins = SendAllCoins
  { -- | The coins to send, every one of them
    coins :: ![Store.Unspent],
    -- | The derivation path of every address holding one of the coins
    coinPaths :: !(Map Address SoftPath),
    -- | The recipients that are addresses of the account, with their paths
    recipientPaths :: !(Map Address SoftPath)
  }
  deriving (Eq, Show)

-- | Build the transaction spending all the coins to the recipients, with no
-- change, its outputs shuffled (see 'sendAllAmounts'). A fee rate or a dust
-- limit no transaction can be built with is refused.
buildSendAll :: Network -> Ctx -> StdGen -> SendAllCoins -> [(Address, Share)] -> Natural -> Natural -> Either String BuiltTx
buildSendAll net ctx gen found recipients feeByte dust = do
  checkFeeRateAndDust feeByte dust
  when (null found.coins) $ Left "There are no coins to send"
  amounts <- sendAllAmounts ctx feeByte dust found.coins recipients
  tx <- withInputSequence net <$> (buildAddrTx net ctx ((.outpoint) <$> found.coins) =<< mapM outputText (evalState (randomShuffle amounts) gen))
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
  -- | Fee per byte
  Natural ->
  -- | Dust
  Natural ->
  ExceptT String (DB m) TxSignData
buildSendAllSignData net ctx gen accId coins recipients feeByte dust = do
  coinPaths <- Map.fromList <$> mapM (coinWithPath net accId) coins
  recipientPaths <- walletPaths net accId (fst <$> recipients)
  built <- liftEither $ buildSendAll net ctx gen SendAllCoins {coins, coinPaths, recipientPaths} recipients feeByte dust
  spentFrom <- mapM getRawTx $ nub $ (.outpoint.hash) <$> built.tx.inputs
  return $ TxSignData built.tx spentFrom built.inputPaths built.outputPaths (Just feeByte) Nothing

{- The question about dust coins -}

-- | What spending the dust coins too costs, and what leaving them does, for
-- the question preparesendall asks
data DustReport = DustReport
  { -- | The fee rate, in satoshis per byte
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
    -- to one P2PKH address, leaving it at least the dust limit; none when no
    -- rate can
    laterRate :: !(Maybe Natural),
    -- | The dust limit, in satoshis
    dustLimit :: !Natural
  }
  deriving (Eq, Show)

-- | The report on the dust coins, from the coins worth spending and the dust
-- coins, once both they and all the coins together can pay the recipients:
-- see 'sendAllAmounts'
dustReport :: Ctx -> Natural -> Natural -> [(Address, Share)] -> [Store.Unspent] -> [Store.Unspent] -> Bool -> DustReport
dustReport ctx feeByte dust recipients worth dustCoins othersStay =
  DustReport
    { rate = feeByte,
      dustCoins = sortOn (Down . (.value)) dustCoins,
      addedFee = feeOf (worth <> dustCoins) - feeOf worth,
      withDust = remaining (worth <> dustCoins),
      withoutDust = remaining worth,
      othersStay,
      laterRate,
      dustLimit = dust
    }
  where
    feeOf coins = sendAllFee ctx feeByte coins (fst <$> recipients)
    fixedTotal = sum [amount | (_, Fixed amount) <- recipients]
    -- Both sets of coins pay the recipients: nothing is below zero
    remaining coins = fromInteger $ max 0 $ toInteger (valueOf coins) - toInteger fixedTotal - toInteger (feeOf coins)
    dustValue = valueOf dustCoins
    -- The dust coins alone, paying one P2PKH address
    aloneSize = fromIntegral $ txSize [p2pkhInput | _ <- dustCoins] [p2pkhOutputSize]
    laterRate
      | dustValue >= dust + aloneSize = Just ((dustValue - dust) `div` aloneSize)
      | otherwise = Nothing

-- | The report as preparesendall shows it, before its question
dustReportLines :: DustReport -> [String]
dustReportLines report = [header] <> (coinLine <$> report.dustCoins) <> [spent, left]
  where
    count = length report.dustCoins
    one = count == 1
    them = if one then "it" else "them"
    rateText = show report.rate <> " sat/byte"
    header
      | one = "1 coin costs more to spend than it is worth at " <> rateText <> ", " <> show (inputFee report.rate) <> " sat:"
      | otherwise = show count <> " coins cost more to spend than they are worth at " <> rateText <> ", " <> show (inputFee report.rate) <> " sat each:"
    coinLine coin = "  " <> coinText coin.outpoint <> "  " <> show coin.value <> " sat, " <> worthText coin
    worthText coin = case worthSpendingUpTo coin of
      0 -> "worth less than its fee at any rate"
      upTo -> "worth spending at " <> show upTo <> " sat/byte or less"
    dustValue = valueOf report.dustCoins
    spent =
      "Spending " <> them <> " adds " <> show report.addedFee <> " sat of fee for " <> show dustValue
        <> " sat of coins. What remains for the addresses without an amount is "
        <> show report.withDust
        <> " sat instead of "
        <> show report.withoutDust
        <> ", "
        <> show (report.withoutDust - report.withDust)
        <> " less"
        <> (if report.othersStay then "." else ", and the account is empty.")
    left = "Leaving " <> them <> ", the account keeps " <> show dustValue <> " sat. Once the other coins are gone, " <> later
    later = case report.laterRate of
      Just upTo
        | one -> "it can only be spent at " <> show upTo <> " sat/byte or less."
        | otherwise -> "they can only be spent together, at " <> show upTo <> " sat/byte or less."
      Nothing -> (if one then "it" else "they") <> " can never be spent: any fee would leave less than " <> show report.dustLimit <> " sat."

-- | The question after the report
dustQuestion :: DustReport -> String
dustQuestion report
  | length report.dustCoins == 1 = "Spend it too?"
  | otherwise = "Spend them too?"
