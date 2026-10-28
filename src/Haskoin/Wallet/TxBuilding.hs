{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | Building the wallet's transactions around the coin selection of
-- "Haskoin.Wallet.CoinSelection".
--
-- 'buildTxSignData' is the entry point of preparetx. It fetches what the
-- builder needs from the database ('fetchWalletCoins'), chooses the coins
-- ('choosePayment'), finds the change address once it knows the coins
-- ('ChangeTo'), assembles the transaction ('assemblePayment'), and fetches
-- the transactions the chosen coins come from. 'buildWalletTx' does the
-- two pure steps at once, with a change address given in advance.
module Haskoin.Wallet.TxBuilding
  ( BuiltTx (..),
    WalletCoins (..),
    ChosenPayment (..),
    ChangeTo (..),
    buildTxSignData,
    buildWalletTx,
    choosePayment,
    assemblePayment,
    spentAddresses,
    fetchWalletCoins,
    coinWithPath,
    recipientOutputSize,
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
import Haskoin.Wallet.Address
import Haskoin.Wallet.Coin
import Haskoin.Wallet.CoinSelection
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.PendingTx (changeAddress)
import Haskoin.Wallet.RawTx
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSize (checkFeeRateAndDust, outputSize, p2pkhOutputSize)
import Haskoin.Wallet.Util (addrToTextE, randomShuffle, safeSubtract)
import Numeric.Natural (Natural)
import System.Random (StdGen, splitGen)

{- Types -}

-- | What the builder needs from the wallet database
data WalletCoins = WalletCoins
  { -- | The spendable coins of the account
    coins :: ![Store.Unspent],
    -- | The derivation path of every address holding one of the coins
    coinPaths :: !(Map Address SoftPath),
    -- | The recipients that are addresses of the account, with their paths
    recipientPaths :: !(Map Address SoftPath)
  }
  deriving (Eq, Show)

-- | A payment whose coins are chosen, before its outputs are assembled
-- around the address of its change
data ChosenPayment = ChosenPayment
  { -- | The coins, the change and the fee
    selection :: !Selection,
    -- | The recipients with what they get, less their share of the fee
    -- when they pay it
    paidRecipients :: ![(Address, Natural)],
    -- | What is left of the generator, which shuffles the outputs
    outputGen :: !StdGen
  }
  deriving (Show)

-- | Where the change of a payment goes
data ChangeTo m
  = -- | The next free change address of the account
    NewChangeAddress
  | -- | Back to one of the addresses the payment spends from, which the
    -- function picks among them once the coins are chosen
    ChangeBack ([Address] -> ExceptT String (DB m) Address)

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

{- Fetching from the database -}

-- | The signing data of a transaction paying the recipients from the
-- account's coins. The change address is found only for a payment with
-- change, once its coins are chosen.
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
  ChangeTo m ->
  ExceptT String (DB m) TxSignData
buildTxSignData net ctx cfg gen strategy accId recipients feeByte dust payer minConf coinControl changeTo = do
  when (null recipients) $ throwError "No recipients provided"
  found <- fetchWalletCoins net ctx accId (fst <$> recipients) minConf
  chosen <- liftEither $ choosePayment ctx gen strategy found recipients feeByte dust payer coinControl
  change <-
    if chosen.selection.changeAmount > 0
      then Just <$> changeFor found.coinPaths chosen
      else return Nothing
  built <- liftEither $ assemblePayment net ctx found chosen change
  spentFrom <- mapM getRawTx $ nub $ (.outpoint.hash) <$> built.tx.inputs
  return $ TxSignData built.tx spentFrom built.inputPaths built.outputPaths (Just feeByte) Nothing
  where
    -- The change address, given the paths of the addresses of the coins
    changeFor paths chosen = case changeTo of
      NewChangeAddress -> liftEither . fromDBAddr net =<< changeAddress ctx cfg accId
      ChangeBack pick -> do
        addr <- pick (spentAddresses chosen)
        path <-
          liftMaybe "The change can only go back to an address the payment spends from" $
            Map.lookup addr paths
        return (addr, path)

-- | Fetch the spendable coins of the account with their derivation paths,
-- and the paths of the recipients that are its own addresses
fetchWalletCoins ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  -- | The recipients
  [Address] ->
  -- | Minimum confirmations of the coins
  Natural ->
  ExceptT String (DB m) WalletCoins
fetchWalletCoins net ctx accId recipients minConf = do
  coins <- spendableCoins net ctx accId minConf
  coinPaths <- Map.fromList <$> mapM (coinWithPath net accId) coins
  recipientPaths <- walletPaths net accId recipients
  return WalletCoins {coins, coinPaths, recipientPaths}

-- | The address of a coin of the account, with its derivation path
coinWithPath ::
  (MonadUnliftIO m) =>
  Network -> DBAccountId -> Store.Unspent -> ExceptT String (DB m) (Address, SoftPath)
coinWithPath net accId coin = do
  addr <- liftEither $ maybeToEither "A spendable coin has no address" coin.address
  path <- addressPath net accId addr >>= liftMaybe "A spendable coin has no derivation path"
  return (addr, path)

{- Building -}

-- | The fee rate at which spending a coin is expected to cost in the future,
-- in satoshis per byte: the value Bitcoin Core uses. It only weighs
-- consolidating small coins now against spending them later, so it is not a
-- setting of the user.
longTermRate :: Word64
longTermRate = 10

-- | Build a transaction paying the recipients from the wallet's coins, its
-- change, if any, at the given change address: 'choosePayment', then
-- 'assemblePayment'
buildWalletTx ::
  Network ->
  Ctx ->
  StdGen ->
  CoinSelection ->
  WalletCoins ->
  -- | The change address, with its path
  (Address, SoftPath) ->
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
buildWalletTx net ctx gen strategy found change recipients feeByte dust payer coinControl = do
  chosen <- choosePayment ctx gen strategy found recipients feeByte dust payer coinControl
  assemblePayment net ctx found chosen (Just change)

-- | Choose the coins of a payment to the recipients among the wallet's
-- coins: by the strategy, or exactly those under coin control. When the
-- recipients pay the fee, it is deducted from their amounts in equal parts,
-- the first recipient also paying the remainder. A fee rate of zero is
-- refused: the network would not relay the transaction. So is a recipient
-- paid less than the dust limit.
choosePayment ::
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
  Either String ChosenPayment
choosePayment ctx gen strategy found recipients feeByte dust payer coinControl = do
  when (null recipients) $ Left "No recipients provided"
  checkFeeRateAndDust feeByte dust
  -- The coin selection draws from a generator of its own; the outputs are
  -- shuffled with what is left
  let (drawGen, outputGen) = splitGen gen
  selection <- case coinControl of
    [] -> selectCoins strategy drawGen payment found.coins
    _ -> spendCoins payment =<< controlledCoins found.coins coinControl
  paidRecipients <- case payer of
    SenderPays -> return recipients
    RecipientsPay -> do
      -- A fee at or above the amount leaves the recipients nothing
      when (selection.paid == 0) $ Left "Recipients can't pay for the fee"
      recipientsPayFee (payment.amount - selection.paid) recipients
  -- The change is never below the dust limit: below it, it goes to the fee
  when (any ((< dust) . snd) paidRecipients) $
    Left "Recipient output is smaller than the dust value"
  return ChosenPayment {selection, paidRecipients, outputGen}
  where
    payment =
      Payment
        { rates = FeeRates {feeRate = fromIntegral feeByte, longTermFeeRate = longTermRate},
          payer,
          dust = fromIntegral dust,
          outputSizes = recipientOutputSize ctx . fst <$> recipients,
          amount = sum $ fromIntegral . snd <$> recipients
        }

-- | The addresses the chosen coins come from, each once, in the order of
-- the coins
spentAddresses :: ChosenPayment -> [Address]
spentAddresses chosen = nub $ mapMaybe (.address) chosen.selection.selected

-- | The transaction of a payment whose coins are chosen: its recipients,
-- and its change at the given address, in an order drawn at random. A
-- payment with change needs the address; one without ignores it. Every
-- output to the wallet, the change and a recipient of the account, has its
-- derivation path, and so has every input.
assemblePayment :: Network -> Ctx -> WalletCoins -> ChosenPayment -> Maybe (Address, SoftPath) -> Either String BuiltTx
assemblePayment net ctx found chosen changeM = do
  change <- case (chosen.selection.changeAmount, changeM) of
    (0, _) -> Right []
    (amount, Just (addr, path)) -> Right [((addr, amount), path)]
    (_, Nothing) -> Left "The payment has change, and no address for it"
  let outputs = (fst <$> change) <> (second fromIntegral <$> chosen.paidRecipients)
      shuffledOutputs = evalState (randomShuffle outputs) chosen.outputGen
  tx <- buildAddrTx net ctx ((.outpoint) <$> chosen.selection.selected) =<< mapM recipientText shuffledOutputs
  inputPaths <- nub <$> mapM coinPath chosen.selection.selected
  let outputPaths =
        nub $ (snd <$> change) <> mapMaybe ((`Map.lookup` found.recipientPaths) . fst) chosen.paidRecipients
  return BuiltTx {tx, spentCoins = chosen.selection.selected, inputPaths, outputPaths}
  where
    -- The builder of haskoin takes the recipients as address text
    recipientText (addr, amount) = (,amount) <$> addrToTextE net addr
    coinPath :: Store.Unspent -> Either String SoftPath
    coinPath coin = do
      addr <- maybeToEither "A chosen coin has no address" coin.address
      maybeToEither "A chosen coin has no derivation path" $ Map.lookup addr found.coinPaths

-- | The size of the output paying a recipient. An address that cannot be
-- paid is counted as P2PKH; the builder refuses it further down.
recipientOutputSize :: Ctx -> Address -> Word64
recipientOutputSize ctx addr =
  maybe p2pkhOutputSize (outputSize . marshal ctx) (addressToOutput addr)

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
