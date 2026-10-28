{-# LANGUAGE LambdaCase #-}
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
-- Its answer lists every coin left in the account, and why ('leftCoins').
module Haskoin.Wallet.TxSendAll
  ( Share (..),
    readRecipient,
    checkSharing,
    isDustCoin,
    sendAllFee,
    sendAllAmounts,
    SendAllCoins (..),
    buildSendAll,
    buildSendAllSignData,
    DustReport (..),
    dustReport,
    dustReportLines,
    dustQuestion,
    LeftReason (..),
    LeftCoin (..),
    leftCoins,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.State (evalState)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), object, withObject, withText, (.:), (.=))
import Data.List (mapAccumL, nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Address (walletPaths)
import Haskoin.Wallet.Amounts (AmountUnit, readAmount)
import Haskoin.Wallet.Coin (CoinLock (..), coinText)
import Haskoin.Wallet.Database
import Haskoin.Wallet.RawTx (getRawTx)
import Haskoin.Wallet.TxBuilding (BuiltTx (..), coinWithPath, recipientOutputSize)
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSize (checkFeeRateAndDust, p2pkhInput, p2pkhInputSize, p2pkhOutputSize, txSize)
import Haskoin.Wallet.Util (addrToTextE, randomShuffle, textToAddrE)
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Recipients -}

-- | What a recipient of a send-all gets
data Share
  = -- | The amount the user fixed, in satoshis
    Fixed !Natural
  | -- | A part of what remains once the fixed amounts and the fee are paid
    Remainder
  deriving (Eq, Show)

-- | A recipient as typed: an address, which shares what remains, or an
-- address and its amount after a colon, ADDRESS:AMOUNT. The amount is in
-- the unit given, unless it names one. The whole text is read as an address
-- first, since a Bitcoin Cash address may begin with its prefix and a
-- colon, as bitcoincash:qp…; otherwise the amount is what follows the last
-- colon.
readRecipient :: Network -> AmountUnit -> Text -> Either String (Address, Share)
readRecipient net unit typed =
  case textToAddrE net typed of
    Right addr -> Right (addr, Remainder)
    Left notAnAddress
      | Text.null beforeColon -> Left notAnAddress
      | otherwise -> do
          addr <- textToAddrE net (Text.dropEnd 1 beforeColon)
          satoshis <- readAmount unit amount
          return (addr, Fixed (fromIntegral satoshis))
  where
    -- The text up to and with the last colon, and what follows it
    (beforeColon, amount) = Text.breakOnEnd (Text.pack ":") typed

-- | Refuse recipients that leave no one to receive what remains
checkSharing :: [(Address, Share)] -> Either String ()
checkSharing recipients =
  unless (Remainder `elem` (snd <$> recipients)) $
    Left "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"

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

-- | The amount of each recipient, in their order, when the coins pay them
-- and the fee at the rate: the fixed amounts as typed, and what remains
-- shared equally among the others, the first of them also getting what does
-- not divide. The recipients without an amount pay the fee. Nothing is
-- built when an output would be below the dust limit, or when the coins do
-- not cover the fixed amounts and the fee.
sendAllAmounts :: Ctx -> Natural -> Natural -> [Store.Unspent] -> [(Address, Share)] -> Either String [(Address, Natural)]
sendAllAmounts ctx feeByte dust coins recipients = do
  checkSharing recipients
  when (any (< dust) fixedAmounts) $
    Left $ "A fixed amount is below the dust limit of " <> show dust <> " satoshis"
  when (total < fixedTotal + fee) $
    Left $
      "The coins, " <> show total <> " satoshis, do not cover the fixed amounts, "
        <> show fixedTotal
        <> ", and the fee, "
        <> show fee
  when (each < dust) $
    Left $
      "What remains for each address without an amount, " <> show each
        <> " satoshis, is below the dust limit of "
        <> show dust
  -- What does not divide goes to the first address without an amount
  return $ snd $ mapAccumL give extra recipients
  where
    total = valueOf coins
    fee = sendAllFee ctx feeByte coins (fst <$> recipients)
    fixedAmounts = [amount | (_, Fixed amount) <- recipients]
    fixedTotal = sum fixedAmounts
    sharing = length [() | (_, Remainder) <- recipients]
    (each, extra) = (total - fixedTotal - fee) `quotRem` fromIntegral sharing
    give left (addr, Fixed amount) = (left, (addr, amount))
    give left (addr, Remainder) = (0, (addr, each + left))

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
  tx <- buildAddrTx net ctx ((.outpoint) <$> found.coins) =<< mapM outputText (evalState (randomShuffle amounts) gen)
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

{- What stays in the account -}

-- | Why a send-all leaves a coin of the account
data LeftReason
  = -- | Worth less than the fee to spend it, and left as the user chose
    WorthLessThanFee
  | -- | Locked, by the user or by a pending transaction
    Locked !CoinLock
  | -- | Not confirmed enough yet, or a coinbase not yet mature
    NotSpendableYet
  | -- | Spendable, but not among the coins given with --coin
    NotGiven
  deriving (Eq, Show)

instance ToJSON LeftReason where
  toJSON = String . Text.pack . reasonName

instance FromJSON LeftReason where
  parseJSON = withText "LeftReason" $ \name ->
    case filter ((== Text.unpack name) . reasonName) leftReasons of
      [reason] -> return reason
      _ -> fail $ "Unknown reason a coin stays in the account: " <> Text.unpack name

-- | Every reason, in the order the answer lists the coins in
leftReasons :: [LeftReason]
leftReasons = [WorthLessThanFee, Locked LockedByUser, Locked LockedByTransaction, NotSpendableYet, NotGiven]

-- | The name of a reason in JSON
reasonName :: LeftReason -> String
reasonName = \case
  WorthLessThanFee -> "dust"
  Locked LockedByUser -> "lockedbyuser"
  Locked LockedByTransaction -> "lockedbytransaction"
  NotSpendableYet -> "notspendableyet"
  NotGiven -> "notgiven"

-- | A coin of the account a send-all leaves in it, and why
data LeftCoin = LeftCoin
  { leftCoinOutpoint :: !OutPoint,
    leftCoinValue :: !Word64,
    leftCoinReason :: !LeftReason
  }
  deriving (Eq, Show)

instance ToJSON LeftCoin where
  toJSON coin =
    object
      [ "outpoint" .= leftCoinOutpoint coin,
        "value" .= leftCoinValue coin,
        "reason" .= leftCoinReason coin
      ]

instance FromJSON LeftCoin where
  parseJSON = withObject "LeftCoin" $ \o ->
    LeftCoin <$> o .: "outpoint" <*> o .: "value" <*> o .: "reason"

-- | The coins of the account a send-all leaves, by reason in the order of
-- 'leftReasons', the largest first: the dust coins left, the coins the
-- builder may not spend with their locks, and the spendable coins not
-- given with --coin
leftCoins :: [Store.Unspent] -> [(Store.Unspent, Maybe CoinLock)] -> [Store.Unspent] -> [LeftCoin]
leftCoins dustLeft unspendable notGiven =
  concat [sortOn (Down . leftCoinValue) [left | left <- everyLeft, leftCoinReason left == reason] | reason <- leftReasons]
  where
    everyLeft =
      [leftOf WorthLessThanFee coin | coin <- dustLeft]
        <> [leftOf (maybe NotSpendableYet Locked lock) coin | (coin, lock) <- unspendable]
        <> [leftOf NotGiven coin | coin <- notGiven]
    leftOf :: LeftReason -> Store.Unspent -> LeftCoin
    leftOf reason coin = LeftCoin coin.outpoint coin.value reason
