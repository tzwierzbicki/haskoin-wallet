{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The fee rates recent blocks paid, so that the user chooses a fee rate
-- with some perspective rather than from one number: a low, a median and
-- a high rate, read from the transactions of the latest blocks of the
-- store. They are what got into blocks, not how long transactions waited:
-- after a quiet spell, a sudden backlog makes them too low for a while,
-- and bumpfee is then the remedy.
module Haskoin.Wallet.FeeRates where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, throwError)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.List (sortOn)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Data.List (maximumBy)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Store
import Haskoin.Wallet.Util (readNatural)
import Numeric.Natural (Natural)

{- The rates -}

-- | The fee rates the transactions of recent blocks paid, in satoshis per
-- byte: the rates below which a quarter, half and nine tenths of the bytes
-- of those blocks paid less. The bytes are the virtual bytes miners weigh,
-- which for the transactions hw builds are their bytes. With them, the
-- blocks they come from, and when the latest of them was mined, as a Unix
-- time.
data FeeRates = FeeRates
  { rateLow :: !Natural,
    rateMedian :: !Natural,
    rateHigh :: !Natural,
    ratesFromHeight :: !BlockHeight,
    ratesToHeight :: !BlockHeight,
    ratesLatestTime :: !Word32
  }
  deriving (Eq, Show)

instance ToJSON FeeRates where
  toJSON r =
    object
      [ "low" .= r.rateLow,
        "median" .= r.rateMedian,
        "high" .= r.rateHigh,
        "fromheight" .= r.ratesFromHeight,
        "toheight" .= r.ratesToHeight,
        "latesttime" .= r.ratesLatestTime
      ]

instance FromJSON FeeRates where
  parseJSON =
    withObject "FeeRates" $ \o ->
      FeeRates
        <$> o .: "low"
        <*> o .: "median"
        <*> o .: "high"
        <*> o .: "fromheight"
        <*> o .: "toheight"
        <*> o .: "latesttime"

-- | A level of the rates: the low, the median or the high one
data FeeLevel = Low | Median | High
  deriving (Eq, Show, Enum, Bounded)

-- | The name of a level, as the command line and the answers give it
levelName :: FeeLevel -> Text
levelName Low = "low"
levelName Median = "median"
levelName High = "high"

-- | The share of the bytes that paid less than the rate of a level
levelShare :: FeeLevel -> Rational
levelShare Low = 1 % 4
levelShare Median = 1 % 2
levelShare High = 9 % 10

-- | The rate of a level
rateAt :: FeeLevel -> FeeRates -> Natural
rateAt Low = (.rateLow)
rateAt Median = (.rateMedian)
rateAt High = (.rateHigh)

-- | How the fee rate of a transaction is chosen: a number of satoshis per
-- byte, or a level of the rates recent blocks paid
data FeeRateChoice = FeeRateOf !Natural | FeeLevelOf !FeeLevel
  deriving (Eq, Show)

-- | A fee rate choice as the command line gives it: a level by its name, or
-- a number of satoshis per byte
readFeeRateChoice :: String -> Either String FeeRateChoice
readFeeRateChoice text =
  case [level | level <- [minBound .. maxBound], levelName level == Text.pack text] of
    level : _ -> Right (FeeLevelOf level)
    [] -> maybe (Left "Give a number of satoshis per byte, or low, median or high") (Right . FeeRateOf) (readNatural (Text.pack text))

{- Computing them -}

-- | The fee rates of the transactions of the blocks, each with its
-- transactions: Nothing when none of them pays a fee. A coinbase pays no
-- fee, and no other transaction gets mined without one.
feeRatesOf :: [(Store.BlockData, [Store.Transaction])] -> Maybe FeeRates
feeRatesOf [] = Nothing
feeRatesOf blocks
  | null paying = Nothing
  | otherwise =
      Just
        FeeRates
          { rateLow = at Low,
            rateMedian = at Median,
            rateHigh = at High,
            ratesFromHeight = minimum heights,
            ratesToHeight = maximum heights,
            ratesLatestTime = (maximumBy (comparing (.height)) (fst <$> blocks)).header.timestamp
          }
  where
    paying = sortOn fst [(txRate tx, virtualSize tx) | (_, txs) <- blocks, tx <- txs, tx.fee > 0]
    heights = (.height) . fst <$> blocks
    at level = rateAtShare (levelShare level) paying

-- | The rate below which the share of the bytes paid less, among rates
-- sorted from the lowest, each with the bytes it bought: the rate of the
-- transaction where the bytes, counted from the cheapest, reach the
-- share. Rounded up to a whole satoshi, so that paying it is paying at
-- least as much, and at least 1.
rateAtShare :: Rational -> [(Rational, Natural)] -> Natural
rateAtShare share sorted = max 1 (ceiling (reached 0 sorted))
  where
    needed = share * fromIntegral (sum (snd <$> sorted))
    reached _ [] = 0
    reached seen ((rate, bytes) : rest)
      | fromIntegral (seen + bytes) >= needed || null rest = rate
      | otherwise = reached (seen + bytes) rest

-- | The fee rate of a transaction, in satoshis per virtual byte
txRate :: Store.Transaction -> Rational
txRate tx = fromIntegral tx.fee % fromIntegral (max 1 (virtualSize tx))

-- | The virtual size of a transaction, in bytes: its weight in quarters,
-- rounded up, as Bitcoin Core counts it
virtualSize :: Store.Transaction -> Natural
virtualSize tx = fromIntegral ((tx.weight + 3) `div` 4)

{- Reading them from the store -}

-- | How many blocks the rates are read from: enough to even out the luck
-- of one block, few enough to stay recent
blocksRead :: Int
blocksRead = 6

-- | The fee rates of the latest blocks of the store: the newest
-- 'blocksRead' that hold a transaction paying a fee, since an empty block
-- says nothing of the rates. Their transactions are asked the given number
-- at a time.
recentFeeRates :: Natural -> Store -> ExceptT String IO FeeRates
recentFeeRates atATime store = do
  latest <- storeLatestBlocks store
  let blocks = take blocksRead [b | b <- latest, b.fee > 0]
  when (null blocks) $ throwError "No recent block of the store holds a transaction paying a fee"
  txs <- inParallel atATime [storeBlockTransactions store (headerHash b.header) | b <- blocks]
  maybe (throwError "The recent blocks of the store hold no transaction paying a fee") return (feeRatesOf (zip blocks txs))
