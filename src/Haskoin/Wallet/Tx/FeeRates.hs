{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The fee rates recent blocks paid, so that the user chooses a fee rate
-- with some perspective rather than from one number: a low, a median and
-- a high rate, read from the transactions of the latest blocks of the
-- store. They are what got into blocks, not how long transactions waited:
-- after a quiet spell, a sudden backlog makes them too low for a while,
-- and bumpfee is then the remedy. The question of the fee rate, which
-- offers them beside a number, is here too.
module Haskoin.Wallet.Tx.FeeRates where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, throwError)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.List (intercalate, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import Data.List (maximumBy)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Store
import Haskoin.Wallet.Tx.Size (checkFeeRate, storeTxSize)
import Haskoin.Wallet.Util (readNatural)
import Numeric.Natural (Natural)

{- The rates -}

-- | The fee rates the transactions of recent blocks paid, in satoshis per
-- virtual byte: the rates below which a quarter, half and nine tenths of
-- the virtual bytes of those blocks paid less. With them, the blocks they
-- come from, and when the latest of them was mined, as a Unix time.
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

-- | The share of the virtual bytes that paid less than the rate of a level
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
-- virtual byte, or a level of the rates recent blocks paid
data FeeRateChoice = FeeRateOf !Natural | FeeLevelOf !FeeLevel
  deriving (Eq, Show)

-- | A fee rate choice as the command line gives it: a level by its name, or
-- a number of satoshis per virtual byte
readFeeRateChoice :: String -> Either String FeeRateChoice
readFeeRateChoice text =
  case [level | level <- [minBound .. maxBound], levelName level == Text.pack text] of
    level : _ -> Right (FeeLevelOf level)
    [] -> maybe (Left "Give a number of satoshis per virtual byte, or low, median or high") (Right . FeeRateOf) (readNatural (Text.pack text))

{- The question -}

-- | The levels a transaction that must pay at least the given rate can
-- take: those whose rate is that high, from the lowest
offeredLevels :: Natural -> FeeRates -> [FeeLevel]
offeredLevels lowest rates = [level | level <- [minBound .. maxBound], rateAt level rates >= lowest]

-- | The level an empty answer takes among those offered: the median, or
-- the lowest level offered when the median is not. None when no level is
-- offered.
defaultLevel :: [FeeLevel] -> Maybe FeeLevel
defaultLevel offered
  | Median `elem` offered = Just Median
  | otherwise = listToMaybe offered

-- | The question of the fee rate of a transaction that must pay at least
-- the given rate. It takes the same answers as --fee-rate: a level
-- offered or a number. When no level is offered, it asks for a number
-- alone, and has no default.
feeRateQuestion :: Natural -> FeeRates -> String
feeRateQuestion lowest rates =
  case defaultLevel offered of
    Just level -> "Which fee rate? Type " <> orList (map levelString offered <> [aNumber]) <> " [" <> levelString level <> "] "
    Nothing -> "Which fee rate? Type " <> aNumber <> ", at least " <> show lowest <> ": "
  where
    offered = offeredLevels lowest rates
    orList names = intercalate ", " (init names) <> " or " <> last names

-- | The fee rate an answer to 'feeRateQuestion' picks: a level offered,
-- a number of at least the lowest rate, or the default level for an empty
-- answer. Any other answer is a mistake, told with what to type instead.
feeRateAnswer :: Natural -> FeeRates -> Text -> Either String Natural
feeRateAnswer lowest rates typed
  | Text.null typed = maybe (Left whatToType) (Right . (`rateAt` rates)) (defaultLevel offered)
  | otherwise =
      case readFeeRateChoice (Text.unpack typed) of
        Right (FeeRateOf rate)
          | rate < lowest -> Left $ "The fee rate must be at least " <> perByte lowest
          | otherwise -> rate <$ checkFeeRate rate
        Right (FeeLevelOf level)
          | level `elem` offered -> Right (rateAt level rates)
          | otherwise ->
              Left $
                "The " <> levelString level <> " rate is " <> perByte (rateAt level rates)
                  <> ", and the fee rate must be at least "
                  <> show lowest
        Left _ -> Left whatToType
  where
    offered = offeredLevels lowest rates
    whatToType = case defaultLevel offered of
      Just level -> "Type " <> intercalate ", " (map levelString offered <> [aNumber]) <> ", or nothing for " <> levelString level
      Nothing -> "Type " <> aNumber <> ", at least " <> show lowest

-- | What the fee rate question calls a rate typed as a number
aNumber :: String
aNumber = "a number of satoshis per virtual byte"

-- | The name of a level, as a question shows it
levelString :: FeeLevel -> String
levelString = Text.unpack . levelName

-- | A fee rate in words: 1 satoshi per virtual byte, 2 satoshis per
-- virtual byte
perByte :: Natural -> String
perByte 1 = "1 satoshi per virtual byte"
perByte rate = show rate <> " satoshis per virtual byte"

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
    paying = sortOn fst [(txRate tx, storeTxSize tx) | (_, txs) <- blocks, tx <- txs, tx.fee > 0]
    heights = (.height) . fst <$> blocks
    at level = rateAtShare (levelShare level) paying

-- | The rate below which the share of the virtual bytes paid less, among
-- rates sorted from the lowest, each with the virtual bytes it bought: the
-- rate of the transaction where the bytes, counted from the cheapest,
-- reach the share. Rounded up to a whole satoshi, so that paying it is paying at
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
txRate tx = fromIntegral tx.fee % fromIntegral (max 1 (storeTxSize tx))

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
