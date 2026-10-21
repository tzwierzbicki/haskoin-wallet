{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Choosing the coins of a transaction: the strategies, the change and
-- the fee.
--
-- The coins are chosen the way Bitcoin Core does it. Every coin pays the fee
-- of its own input, so a coin is worth its amount minus that fee (its
-- effective value), and the target to cover is the amount to send plus the
-- fee of the rest of the transaction. Several strategies propose a set of
-- coins, and the default keeps the proposal with the least waste: the fee
-- spent on inputs beyond what they would cost at a long-term fee rate, plus
-- the cost of the change output, or the excess paid as fee when there is no
-- change. At a low fee rate this favours spending many small coins, which
-- consolidates them while it is cheap; at a high fee rate it favours few
-- inputs. When the recipients pay the fee out of what they receive, the
-- coins only need to cover the amount, but the waste is still measured at
-- the real fee rate: the recipients are not charged for consolidating. The
-- sizes are those of P2PKH inputs and outputs, the only ones the wallet
-- creates.
module Haskoin.Wallet.CoinSelection
  ( CoinSelection (..),
    FeePayer (..),
    FeeRates (..),
    Payment (..),
    Selection (..),
    selectCoins,
    spendCoins,
    txSize,
    waste,
  )
where

import Control.Monad.State (evalState)
import Data.List (minimumBy, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Word (Word64)
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Util (randomShuffle, safeSubtract)
import System.Random (StdGen)

-- | How the coins to spend are chosen
data CoinSelection
  = -- | The proposal with the least waste among the strategies below and an
    -- exact match without change, like Bitcoin Core
    LeastWaste
  | -- | The candidates in random order: an unpredictable set of coins
    RandomDraw
  | -- | The largest coins first: the fewest inputs, so the smallest fee
    LargestFirst
  | -- | The smallest coins first: consolidates small coins, at the cost of
    -- a bigger transaction
    SmallestFirst
  deriving (Eq, Show)

-- | Who pays the fee
data FeePayer
  = -- | The coins cover the amount and the fee
    SenderPays
  | -- | The coins cover the amount only: the fee is deducted from what the
    -- recipients receive
    RecipientsPay
  deriving (Eq, Show)

-- | The fee rates of a selection, in satoshis per byte
data FeeRates = FeeRates
  { -- | What the transaction pays
    feeRate :: !Word64,
    -- | What spending a coin is expected to cost in the future (Bitcoin Core
    -- uses 10): it prices consolidating small coins now, and spending the
    -- change later
    longTermFeeRate :: !Word64
  }
  deriving (Eq, Show)

-- | What the coins must pay for
data Payment = Payment
  { rates :: !FeeRates,
    payer :: !FeePayer,
    -- | Dust: the smallest change worth an output, like Bitcoin Core's dust
    -- limit (546 for P2PKH), below which an output is dust
    dust :: !Word64,
    -- | The number of outputs paid
    outputCount :: !Int,
    -- | The amount paid, over all the outputs
    amount :: !Word64
  }
  deriving (Eq, Show)

-- | The coins chosen for a transaction, and how their value is split
data Selection = Selection
  { -- | The coins to spend
    selected :: ![Store.Unspent],
    -- | What the recipients receive: the amount, less the fee when they pay it
    paid :: !Word64,
    -- | The change, or 0 when what is left goes to the fee
    changeAmount :: !Word64,
    feeAmount :: !Word64
  }
  deriving (Eq, Show)

-- | The size in bytes of a P2PKH input and of a P2PKH output, as the
-- wallet builds them. An input is 32 bytes of transaction hash, 4 of output
-- index, 1 of script length, a 107-byte script (1 + 72 for the signature
-- with its hash type, 1 + 33 for the public key) and 4 of sequence. An
-- output is 8 bytes of amount, 1 of script length and a 25-byte script.
inputSize, outputSize :: Word64
inputSize = 148
outputSize = 34

-- | The size in bytes of a transaction with the given numbers of P2PKH
-- inputs and outputs: 4 bytes of version, the counts of inputs and outputs
-- as variable-length integers, the inputs, the outputs and 4 bytes of lock
-- time. An upper bound: a signature can be a byte or two shorter than the
-- 72 counted for it.
txSize :: Int -> Int -> Word64
txSize inputs outputs =
  8 + varIntSize inputs + varIntSize outputs + inputSize * fromIntegral inputs + outputSize * fromIntegral outputs

-- | The size in bytes of a count as Bitcoin encodes it: one byte up to 252,
-- then a marker byte and 2, 4 or 8 bytes
varIntSize :: Int -> Word64
varIntSize n
  | n <= 0xfc = 1
  | n <= 0xffff = 3
  | n <= 0xffffffff = 5
  | otherwise = 9

-- | Choose the coins of a payment among the candidates
selectCoins ::
  CoinSelection ->
  -- | The randomness of the random draw
  StdGen ->
  Payment ->
  -- | The candidates
  [Store.Unspent] ->
  Either String Selection
selectCoins strategy gen payment candidates
  | null proposals = Left "The coins do not cover the amount and the fee"
  | otherwise = Right $ minimumBy (comparing (waste payment)) proposals
  where
    -- A candidate whose fee exceeds its amount never helps
    usable = filter ((> 0) . effectiveValue payment.rates) candidates
    proposals = selectionOf payment <$> mapMaybe ($ usable) strategies
    strategies = case strategy of
      LeastWaste -> [exactMatch payment, largestFirst, smallestFirst, randomDraw]
      RandomDraw -> [randomDraw]
      LargestFirst -> [largestFirst]
      SmallestFirst -> [smallestFirst]
    randomDraw = greedy payment . flip evalState gen . randomShuffle
    largestFirst = greedy payment . sortOn (Down . (.value))
    smallestFirst = greedy payment . sortOn (.value)

-- | Spend exactly the given coins, under coin control. They must cover the
-- payment; a coin worth less than the fee of its input is spent all the same.
spendCoins :: Payment -> [Store.Unspent] -> Either String Selection
spendCoins payment coins
  | sum (covers payment <$> coins) < fromIntegral (target payment) =
      Left "The selected coins do not cover the amount and the fee"
  | otherwise = Right $ selectionOf payment coins

-- | What the coins must cover: the amount, plus the fee of the transaction
-- around the inputs when the sender pays it
target :: Payment -> Word64
target payment = case payment.payer of
  SenderPays -> payment.amount + payment.rates.feeRate * txSize 0 payment.outputCount
  RecipientsPay -> payment.amount

-- | What a coin brings towards the target: its effective value when the
-- sender pays for its input, its full value when the recipients do
covers :: Payment -> Store.Unspent -> Integer
covers payment coin = case payment.payer of
  SenderPays -> effectiveValue payment.rates coin
  RecipientsPay -> fromIntegral coin.value

-- | What a coin is worth once its input is paid for. Negative for a coin
-- smaller than the fee of spending it.
effectiveValue :: FeeRates -> Store.Unspent -> Integer
effectiveValue rates coin = fromIntegral coin.value - fromIntegral (rates.feeRate * inputSize)

-- | How the value of coins covering the target is split between the
-- recipients, the change and the fee
selectionOf :: Payment -> [Store.Unspent] -> Selection
selectionOf payment coins = Selection {selected = coins, paid, changeAmount, feeAmount}
  where
    total = sum $ (.value) <$> coins
    inputs = length coins
    -- What the coins hold beyond the amount and, when the sender pays, the
    -- fee of the transaction with a change output: as an Integer, it may be
    -- negative
    change :: Integer
    change = case payment.payer of
      SenderPays ->
        fromIntegral total - fromIntegral payment.amount - fromIntegral (payment.rates.feeRate * txSize inputs (payment.outputCount + 1))
      RecipientsPay -> fromIntegral total - fromIntegral payment.amount
    hasChange = change > 0 && change >= fromIntegral payment.dust
    changeAmount = if hasChange then fromIntegral change else 0
    -- The fee of the whole transaction, deducted from the recipients. A fee
    -- above the amount leaves them nothing, which the builder rejects.
    deducted = case payment.payer of
      SenderPays -> 0
      RecipientsPay -> payment.rates.feeRate * txSize inputs (payment.outputCount + fromEnum hasChange)
    paid = fromMaybe 0 $ payment.amount `safeSubtract` deducted
    feeAmount = total - paid - changeAmount

-- | The coins, in order, until they cover the target
greedy :: Payment -> [Store.Unspent] -> Maybe [Store.Unspent]
greedy payment = go (fromIntegral $ target payment)
  where
    go left _ | left <= 0 = Just []
    go _ [] = Nothing
    go left (c : cs) = (c :) <$> go (left - covers payment c) cs

-- | A set of coins covering the target without exceeding it by more than a
-- change output would cost (creating it now, spending it later), so that no
-- change is needed: a depth-first search over the coins, largest first,
-- bounded to 100000 steps like Bitcoin Core's branch and bound
exactMatch :: Payment -> [Store.Unspent] -> Maybe [Store.Unspent]
exactMatch payment coins =
  snd $ search (100000 :: Int) 0 (sortOn (Down . (.value)) coins)
  where
    lower = fromIntegral (target payment) :: Integer
    upper = lower + fromIntegral (payment.rates.feeRate * outputSize + payment.rates.longTermFeeRate * inputSize)
    -- (steps left, solution): try with the next coin, then without it
    search steps acc cs
      | acc >= lower && acc <= upper = (steps, Just [])
      | steps <= 0 || acc > upper || acc + sum (covers payment <$> cs) < lower = (steps, Nothing)
      | otherwise = case cs of
          [] -> (steps, Nothing)
          c : rest -> case search (steps - 1) (acc + covers payment c) rest of
            (steps', Just found) -> (steps', Just (c : found))
            (steps', Nothing) -> search steps' acc rest

-- | The waste of a selection, as Bitcoin Core measures it: what its inputs
-- cost beyond the long-term fee rate, plus the cost of its change output
-- (creating it now, spending it later) or, without change, the excess paid
-- as fee beyond the fee of the transaction. Lower is better; it can be
-- negative when consolidating is cheap.
waste :: Payment -> Selection -> Integer
waste payment selection = inputs + changeOrExcess
  where
    rates = payment.rates
    count = length selection.selected
    inputs = fromIntegral count * fromIntegral inputSize * (fromIntegral rates.feeRate - fromIntegral rates.longTermFeeRate)
    changeOrExcess
      | selection.changeAmount > 0 = fromIntegral $ rates.feeRate * outputSize + rates.longTermFeeRate * inputSize
      | otherwise = fromIntegral selection.feeAmount - fromIntegral (rates.feeRate * txSize count payment.outputCount)
