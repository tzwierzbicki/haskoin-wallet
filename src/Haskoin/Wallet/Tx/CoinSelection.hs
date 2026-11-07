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
-- sizes of the inputs and of the change output are those of the kind of
-- the account, P2PKH for a BIP44 account and P2WPKH for a BIP84 one
-- ("Haskoin.Wallet.Tx.Size"), in virtual bytes.
--
-- Under coin control, the user chooses the coins. 'spendCoins' spends every
-- chosen coin, whatever the strategy. With --select, 'selectCoins' takes
-- the chosen coins as its candidates instead, and treats them as it treats
-- every coin of the account.
module Haskoin.Wallet.Tx.CoinSelection
  ( CoinSelection (..),
    FeePayer (..),
    FeeRates (..),
    Payment (..),
    Selection (..),
    selectCoins,
    spendCoins,
    waste,
  )
where

import Control.Monad.State (evalState)
import Data.List (minimumBy, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Word (Word64)
import Haskoin (OutputKind)
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Keys (AccountKind)
import Haskoin.Wallet.Tx.Size (accountInput, accountInputSize, accountOutputKind, accountOutputSize, txSize)
import Haskoin.Wallet.Util (randomShuffle, safeSubtract)
import System.Random (StdGen)

{- Types -}

-- | How the coins to spend are selected among the candidates: the
-- spendable coins of the account, or the coins chosen with coin control
-- and --select
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

-- | The fee rates of a selection, in satoshis per virtual byte
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
  { -- | The kind of the account paying, which sizes its inputs and its
    -- change output
    ownKind :: !AccountKind,
    rates :: !FeeRates,
    payer :: !FeePayer,
    -- | Dust: the smallest change worth an output, like Bitcoin Core's dust
    -- limit (546 for P2PKH), below which an output is dust
    dust :: !Word64,
    -- | The kinds of the outputs paid, by the scripts of the recipients,
    -- which size them
    outputKinds :: ![OutputKind],
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

{- Choosing the coins -}

-- | The size of the payment transaction with the given number of the
-- account's own inputs, its outputs, and a change output or not, the
-- inputs and the change of the kind of the account
paymentSize :: Payment -> Int -> Bool -> Word64
paymentSize payment inputs hasChange =
  txSize (replicate inputs (accountInput payment.ownKind)) (payment.outputKinds <> [accountOutputKind payment.ownKind | hasChange])

-- | Select the coins of a payment among the candidates: the spendable coins
-- of the account, or the coins chosen with coin control and --select
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
    usable = filter ((> 0) . effectiveValue payment.ownKind payment.rates) candidates
    proposals = selectionOf payment <$> mapMaybe ($ usable) strategies
    strategies = case strategy of
      LeastWaste -> [exactMatch payment, largestFirst, smallestFirst, randomDraw]
      RandomDraw -> [randomDraw]
      LargestFirst -> [largestFirst]
      SmallestFirst -> [smallestFirst]
    randomDraw = greedy payment . flip evalState gen . randomShuffle
    largestFirst = greedy payment . sortOn (Down . (.value))
    smallestFirst = greedy payment . sortOn (.value)

-- | Spend exactly the given coins, under coin control without --select.
-- The strategy does not apply. The coins must cover the payment; a coin
-- worth less than the fee of its input is spent all the same.
spendCoins :: Payment -> [Store.Unspent] -> Either String Selection
spendCoins payment coins
  | not (covered payment coins) =
      Left "The chosen coins do not cover the amount and the fee"
  | otherwise = Right $ selectionOf payment coins

{- Values and the split -}

-- | What the coins must cover: the amount, plus the fee of the transaction
-- around the inputs when the sender pays it
target :: Payment -> Word64
target payment = case payment.payer of
  SenderPays -> payment.amount + payment.rates.feeRate * paymentSize payment 0 False
  RecipientsPay -> payment.amount

-- | Whether the coins cover the payment: the amount, and the fee of the
-- whole transaction without change when the sender pays it, its size
-- counted for the number of coins. The target and the effective values
-- add up the parts of the transaction, which is a little less than the
-- whole: a SegWit transaction has two bytes of marker and flag, half a
-- virtual byte rounded up, and from 253 inputs on their count takes three
-- bytes instead of one. Counted by the parts, a BIP84 coin of 21090 would
-- pay 20000 at 10 sat/vB with a fee of 1090, where the 110 virtual bytes
-- of the transaction need 1100. The searches use the parts to go fast,
-- and this to decide.
covered :: Payment -> [Store.Unspent] -> Bool
covered payment coins = case payment.payer of
  SenderPays -> total >= fromIntegral payment.amount + fromIntegral (payment.rates.feeRate * paymentSize payment (length coins) False)
  RecipientsPay -> total >= fromIntegral payment.amount
  where
    total = sum (fromIntegral . (.value) <$> coins) :: Integer

-- | What the parts leave out of the fee of a payment without change, at
-- its rate, with the given number of inputs: 0 for a legacy transaction
-- below 253 inputs, more for a SegWit one or a longer count ('covered').
-- It grows with the inputs, never shrinks. None when the recipients pay
-- the fee, since the coins then cover the amount alone.
partsShortfall :: Payment -> Int -> Integer
partsShortfall payment inputs = case payment.payer of
  SenderPays ->
    fromIntegral payment.rates.feeRate
      * ( fromIntegral (paymentSize payment inputs False)
            - fromIntegral (paymentSize payment 0 False)
            - fromIntegral inputs * fromIntegral (accountInputSize payment.ownKind)
        )
  RecipientsPay -> 0

-- | What a coin brings towards the target: its effective value when the
-- sender pays for its input, its full value when the recipients do
covers :: Payment -> Store.Unspent -> Integer
covers payment coin = case payment.payer of
  SenderPays -> effectiveValue payment.ownKind payment.rates coin
  RecipientsPay -> fromIntegral coin.value

-- | What a coin is worth once its input, of the kind of the account, is
-- paid for. Negative for a coin smaller than the fee of spending it.
effectiveValue :: AccountKind -> FeeRates -> Store.Unspent -> Integer
effectiveValue kind rates coin = fromIntegral coin.value - fromIntegral (rates.feeRate * accountInputSize kind)

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
        fromIntegral total - fromIntegral payment.amount - fromIntegral (payment.rates.feeRate * paymentSize payment inputs True)
      RecipientsPay -> fromIntegral total - fromIntegral payment.amount
    hasChange = change > 0 && change >= fromIntegral payment.dust
    changeAmount = if hasChange then fromIntegral change else 0
    -- The fee of the whole transaction, deducted from the recipients. A fee
    -- above the amount leaves them nothing, which the builder rejects.
    deducted = case payment.payer of
      SenderPays -> 0
      RecipientsPay -> payment.rates.feeRate * paymentSize payment inputs hasChange
    paid = fromMaybe 0 $ payment.amount `safeSubtract` deducted
    feeAmount = total - paid - changeAmount

{- Searches -}

-- | The coins, in order, until they cover the payment ('covered'). The
-- target by the parts must be reached first, which the whole needs too.
greedy :: Payment -> [Store.Unspent] -> Maybe [Store.Unspent]
greedy payment = go [] 0 0
  where
    lower = fromIntegral (target payment) :: Integer
    -- The coins taken, the last first, how many, and what they bring
    go taken count acc rest
      | acc >= lower + partsShortfall payment count = Just (reverse taken)
      | otherwise = case rest of
          [] -> Nothing
          c : cs -> go (c : taken) (count + 1) (acc + covers payment c) cs

-- | A set of coins covering the target without exceeding it by more than a
-- change output would cost (creating it now, spending it later), so that no
-- change is needed: a depth-first search over the coins, largest first,
-- bounded to 100000 steps like Bitcoin Core's branch and bound
exactMatch :: Payment -> [Store.Unspent] -> Maybe [Store.Unspent]
exactMatch payment coins =
  snd $ search (100000 :: Int) 0 0 (sortOn (Down . (.value)) coins)
  where
    lower = fromIntegral (target payment) :: Integer
    upper = lower + fromIntegral (payment.rates.feeRate * accountOutputSize payment.ownKind + payment.rates.longTermFeeRate * accountInputSize payment.ownKind)
    -- The bounds for the given number of coins, on the whole transaction:
    -- what the parts leave out of its fee comes on top ('partsShortfall')
    lowerFor count = lower + partsShortfall payment count
    upperFor count = upper + partsShortfall payment count
    -- (steps left, solution): try with the next coin, then without it.
    -- The coins taken so far number count and bring acc.
    search steps count acc cs
      | acc >= lowerFor count && acc <= upperFor count = (steps, Just [])
      | steps <= 0 || acc > upperFor count || acc + sum (covers payment <$> cs) < lower = (steps, Nothing)
      | otherwise = case cs of
          [] -> (steps, Nothing)
          c : rest -> case search (steps - 1) (count + 1) (acc + covers payment c) rest of
            (steps', Just found) -> (steps', Just (c : found))
            (steps', Nothing) -> search steps' count acc rest

{- Waste -}

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
    inputs = fromIntegral count * fromIntegral (accountInputSize payment.ownKind) * (fromIntegral rates.feeRate - fromIntegral rates.longTermFeeRate)
    changeOrExcess
      | selection.changeAmount > 0 = fromIntegral $ rates.feeRate * accountOutputSize payment.ownKind + rates.longTermFeeRate * accountInputSize payment.ownKind
      | otherwise = fromIntegral selection.feeAmount - fromIntegral (rates.feeRate * paymentSize payment count False)
