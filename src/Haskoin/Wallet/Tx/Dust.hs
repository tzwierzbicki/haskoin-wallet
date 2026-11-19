{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The dust coins of a transaction that spends coins in full: a payment of
-- everything a portfolio holds (preparesendall, sendall) and a sweep
-- (preparesweep).
--
-- A coin worth no more than the fee to spend it, its input at the fee rate,
-- is a dust coin: spending it lowers what the recipients get. Whether a
-- coin is dust depends on the rate, and on the size of its input, which
-- the kind of the coin sets. A command that spends coins in full would
-- lose money on each dust coin without a word. It asks the user instead,
-- once, about all of them: it tells what the recipients get with the dust
-- coins and without them ('dustReport'), and spends them too or leaves
-- them all. There is no choice coin by coin: the answer is the same for
-- each dust coin, since each lowers what the recipients get.
--
-- The rule reads a coin with the input that spends it (the type
-- t'SizedCoin'), since the two commands size their inputs apart: a coin of
-- the wallet by the type of its account, and a swept coin by its script
-- and by what is known of its key.
module Haskoin.Wallet.Tx.Dust
  ( SizedCoin (..),
    valueOf,
    spendingFee,
    spendingAmounts,
    inputFee,
    isDustCoin,
    worthSpendingUpTo,
    DustPlace (..),
    DustReport (..),
    dustReport,
    dustQuestion,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Haskoin (Address, InputKind, OutputKind)
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Tx.Recipient (Share (..), shareAmounts)
import Haskoin.Wallet.Tx.Size (inputVirtualSize, outputDustLimit, recipientOutputKind, txSize)
import Numeric.Natural (Natural)

{- Coins and their inputs -}

-- | A coin with the input that spends it once signed. The size of the
-- input is what the coin adds to the fee.
data SizedCoin = SizedCoin
  { coin :: !Store.Unspent,
    input :: !InputKind
  }
  deriving (Eq, Show)

-- | The value of coins, in satoshis
valueOf :: [SizedCoin] -> Natural
valueOf coins = sum [fromIntegral sized.coin.value | sized <- coins]

-- | The fee of the transaction spending the coins to the recipients, at
-- the fee rate
spendingFee :: Natural -> [SizedCoin] -> [Address] -> Natural
spendingFee feeByte coins recipients =
  feeByte * fromIntegral (txSize ((.input) <$> coins) (recipientOutputKind <$> recipients))

-- | The amount of each recipient when the coins, spent in full, pay them
-- and their fee at the rate: see 'shareAmounts'
spendingAmounts :: Natural -> [SizedCoin] -> [(Address, Share)] -> Either String [(Address, Natural)]
spendingAmounts feeByte coins recipients =
  shareAmounts (valueOf coins) (spendingFee feeByte coins (fst <$> recipients)) (outputDustLimit . recipientOutputKind) recipients

{- Dust coins -}

-- | The fee of the input of a coin at the fee rate: 148 virtual bytes for
-- a P2PKH coin, 68 for a P2WPKH one
inputFee :: Natural -> SizedCoin -> Natural
inputFee feeByte sized = feeByte * fromIntegral (inputVirtualSize sized.input)

-- | Whether a coin is a dust coin at the fee rate: worth no more than the
-- fee of its input
isDustCoin :: Natural -> SizedCoin -> Bool
isDustCoin feeByte sized = fromIntegral sized.coin.value <= inputFee feeByte sized

-- | The highest fee rate at which a coin is worth spending, its input
-- costing less than its value: 0 when no rate makes it worth spending
worthSpendingUpTo :: SizedCoin -> Natural
worthSpendingUpTo sized
  | sized.coin.value == 0 = 0
  | otherwise = (fromIntegral sized.coin.value - 1) `div` fromIntegral (inputVirtualSize sized.input)

{- The question about dust coins -}

-- | Where the dust coins of a report are, which words what leaving them
-- means
data DustPlace
  = -- | In the portfolio that pays everything it holds. Whether other coins
    -- stay in it anyway, locked ones for instance: without them, spending
    -- the dust coins empties the portfolio on the network.
    InPortfolio !Bool
  | -- | At addresses outside the wallet, which a sweep empties
    AtSweptAddresses
  deriving (Eq, Show)

-- | What spending the dust coins too costs, and what leaving them does, for
-- the question the command asks
data DustReport = DustReport
  { place :: !DustPlace,
    -- | The fee rate, in satoshis per virtual byte
    rate :: !Natural,
    -- | The dust coins, the largest first
    dustCoins :: ![SizedCoin],
    -- | The fee their inputs add
    addedFee :: !Natural,
    -- | What remains for the recipients without an amount, with the dust
    -- coins spent
    withDust :: !Natural,
    -- | The same, with the dust coins left
    withoutDust :: !Natural,
    -- | The kind of the output the dust coins could be sent to later, alone
    laterOutput :: !OutputKind,
    -- | The highest fee rate at which the dust coins alone can be sent later,
    -- to one output of the kind of 'laterOutput', leaving it at least the
    -- dust limit; none when no rate can
    laterRate :: !(Maybe Natural),
    -- | The dust limit of that output, in satoshis: the least the dust
    -- coins must leave when sent later
    dustLimit :: !Natural
  }
  deriving (Eq, Show)

-- | The report on the dust coins of a transaction that spends coins in
-- full, from the coins worth spending and the dust coins, once both they
-- and all the coins together can pay the recipients: see
-- 'spendingAmounts'. The dust coins that are left could be sent later to
-- an output of the kind given.
dustReport :: DustPlace -> Natural -> [(Address, Share)] -> OutputKind -> [SizedCoin] -> [SizedCoin] -> DustReport
dustReport place feeByte recipients laterOutput worth dustCoins =
  DustReport
    { place,
      rate = feeByte,
      dustCoins = sortOn (Down . (.coin.value)) dustCoins,
      addedFee = feeOf (worth <> dustCoins) - feeOf worth,
      withDust = remaining (worth <> dustCoins),
      withoutDust = remaining worth,
      laterOutput,
      laterRate,
      dustLimit = aloneDust
    }
  where
    feeOf coins = spendingFee feeByte coins (fst <$> recipients)
    fixedTotal = sum [amount | (_, Fixed amount) <- recipients]
    -- Both sets of coins pay the recipients: nothing is below zero
    remaining coins = fromInteger $ max 0 $ toInteger (valueOf coins) - toInteger fixedTotal - toInteger (feeOf coins)
    dustValue = valueOf dustCoins
    -- The dust coins alone, paying one output of that kind
    aloneSize = fromIntegral $ txSize ((.input) <$> dustCoins) [laterOutput]
    -- The dust limit of that output: the least the dust coins must leave it
    aloneDust = outputDustLimit laterOutput
    laterRate
      | dustValue >= aloneDust + aloneSize = Just ((dustValue - aloneDust) `div` aloneSize)
      | otherwise = Nothing

-- | The question after the report
dustQuestion :: DustReport -> String
dustQuestion report
  | length report.dustCoins == 1 = "Spend it too?"
  | otherwise = "Spend them too?"
