{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

-- | Building the wallet's transactions around the coin selection of
-- "Haskoin.Wallet.Tx.CoinSelection".
--
-- A payment is made from a pool: the coins of a portfolio on one network,
-- those of its BIP44 account and of its BIP84 account together. One
-- transaction can therefore spend coins of both types.
--
-- 'buildTxSignData' is the entry point of preparetx. It fetches what the
-- builder needs from the database ('fetchWalletCoins'), chooses the coins
-- and the type of the change ('chooseWithChange'), finds the change
-- address once it knows them ('ChangeTo'), assembles the transaction
-- ('assemblePayment'), and fetches the transactions the chosen coins come
-- from. 'buildWalletTx' does the two pure steps at once, with a change
-- address given in advance.
--
-- 'CoinControl' says whether the user chose the coins of the payment, and
-- what becomes of the coins chosen: every one is spent, or the strategy
-- selects among them as it does among the coins of the pool.
--
-- The change of a payment goes to a new change address of the portfolio,
-- of the first type that applies ('changeKindRule'):
--
-- 1. the type the command asks for;
-- 2. the type of the coins the payment spends, when they are all of one
--    type;
-- 3. the default type of the configuration, when the coins are of both
--    types, which is bip44 unless the user set another.
--
-- Change of the type of the coins keeps a payment within one type, so
-- that it does not tell an observer that two types of addresses belong
-- to one wallet. A payment that already mixes them has nothing left to
-- hide, and takes the type the user prefers. The fee depends on the type
-- of the change, and the coins on the fee, so the coins are chosen for
-- one type first and again when they call for the other
-- ('chooseWithChange').
module Haskoin.Wallet.Tx.Building
  ( BuiltTx (..),
    WalletCoins (..),
    ChosenPayment (..),
    ChangeTo (..),
    CoinControl (..),
    buildTxSignData,
    buildWalletTx,
    choosePayment,
    chooseWithChange,
    changeKindRule,
    assemblePayment,
    spentAddresses,
    fetchWalletCoins,
    coinWithPath,
    recipientsPayFee,
  )
where

import Conduit (MonadUnliftIO)
import Control.Arrow (second)
import Control.Monad (forM_, when, zipWithM)
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversions (cs)
import Data.Word (Word32, Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account (defaultKindOn)
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database.PendingTx (changeAddress)
import Haskoin.Wallet.Keys (AccountKind, PoolPath (..), checkKindOnNetwork, kindName)
import Haskoin.Wallet.Tx.CoinSelection
import Haskoin.Wallet.Tx.LockTime (withHeader)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (accountOutputKind, checkFeeRate, checkStandardSize, outputDustLimit, recipientOutputKind)
import Haskoin.Wallet.Util (addrToTextE, randomShuffle, safeSubtract)
import Numeric.Natural (Natural)
import System.Random (StdGen, splitGen)

{- Types -}

-- | What the builder needs from the wallet database
data WalletCoins = WalletCoins
  { -- | The spendable coins of the pool, of either kind
    coins :: ![Store.Unspent],
    -- | The path in the pool of every address holding one of the coins
    coinPaths :: !(Map Address PoolPath),
    -- | The recipients that are addresses of the pool, with their paths
    recipientPaths :: !(Map Address PoolPath)
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
    -- | The type of the change output the fee was counted for
    changeType :: !AccountKind,
    -- | What is left of the generator, which shuffles the outputs
    outputGen :: !StdGen
  }
  deriving (Show)

-- | Where the change of a payment goes
data ChangeTo
  = -- | To a new change address of the portfolio: of the type given, when
    -- the command asks for one (--change-type), or else of the type the
    -- coins call for ('changeKindRule')
    NewChangeAddress !(Maybe AccountKind)
  | -- | To the address given (--change-back), which must be an address of
    -- the portfolio on the network of the payment. Any other address is
    -- refused: change sent elsewhere would be a payment, and the builder
    -- would not know it as the wallet's.
    ChangeBackTo !Address
  deriving (Eq, Show)

-- | A transaction built by the wallet, before the transactions it spends
-- from are fetched
data BuiltTx = BuiltTx
  { tx :: !Tx,
    -- | The coins the transaction spends, in the order of its inputs
    spentCoins :: ![Store.Unspent],
    inputPaths :: ![PoolPath],
    outputPaths :: ![PoolPath]
  }
  deriving (Eq, Show)

-- | Coin control: whether the user chose the coins of a payment, and what
-- the builder does with the coins chosen
data CoinControl
  = -- | No coin control: the strategy selects among every spendable coin
    -- of the pool
    NoCoinControl
  | -- | Every chosen coin is spent, whatever the strategy, even one worth
    -- less than the fee of its input
    SpendChosen ![OutPoint]
  | -- | The strategy selects among the chosen coins as it does among every
    -- coin without coin control (--select): only what the payment needs is
    -- spent, and a chosen coin worth less than the fee of its input is left
    SelectAmongChosen ![OutPoint]
  deriving (Eq, Show)

{- Fetching from the database -}

-- | The signing data of a transaction paying the recipients from the
-- coins of the pool. The coins may be narrowed to one type (--coin-type).
-- The change address is found only for a payment with change, once its
-- coins and its type are chosen.
buildTxSignData ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  StdGen ->
  CoinSelection ->
  Pool ->
  -- | The one type of coins the payment may spend, or any
  Maybe AccountKind ->
  -- | The recipients and their amounts
  [(Address, Natural)] ->
  -- | Fee per virtual byte
  Natural ->
  -- | Lock time
  Word32 ->
  FeePayer ->
  -- | Minimum confirmations of the coins
  Natural ->
  CoinControl ->
  ChangeTo ->
  ExceptT String (DB m) TxSignData
buildTxSignData ctx cfg gen strategy pool coinKindM recipients feeByte lockTime payer minConf coinControl changeTo = do
  let net = pool.poolNetwork
  when (null recipients) $ throwError "No recipients provided"
  found <- fetchWalletCoins ctx pool coinKindM (fst <$> recipients) minConf
  -- The address the change goes back to, when the user names one: its
  -- type is the type of the change
  backTo <- case changeTo of
    ChangeBackTo addr -> do
      path <-
        liftMaybe "The change can only go back to an address of the portfolio, on the network of the payment"
          . fmap snd
          =<< poolAddressPath pool addr
      return (Just (addr, path))
    NewChangeAddress asked -> do
      liftEither $ mapM_ (checkKindOnNetwork net) asked
      return Nothing
  let fixedKind = case changeTo of
        ChangeBackTo _ -> pathKind . snd <$> backTo
        NewChangeAddress asked -> asked
  chosen <- liftEither $ chooseWithChange ctx gen strategy found recipients feeByte payer coinControl fixedKind (defaultKindOn cfg net)
  change <-
    if chosen.selection.changeAmount > 0
      then Just <$> maybe (newChange chosen.changeType) return backTo
      else return Nothing
  built <- liftEither $ assemblePayment net ctx found chosen change lockTime
  spentFrom <- coinParents (coinsSpentBy built.tx found.coins)
  return $ TxSignData built.tx spentFrom built.inputPaths built.outputPaths (Just feeByte) Nothing [] noLabel
  where
    -- A new change address of the pool, of the type of the change
    newChange kind = do
      row <- changeAddress ctx cfg pool kind
      (addr, path) <- liftEither $ fromDBAddr pool.poolNetwork row
      return (addr, PoolPath kind path)

-- | Fetch the spendable coins of the pool with their paths, those of one
-- type alone when one is given, and the paths of the recipients that are
-- its own addresses
fetchWalletCoins ::
  (MonadUnliftIO m) =>
  Ctx ->
  Pool ->
  -- | The one type of coins to fetch, or any
  Maybe AccountKind ->
  -- | The recipients
  [Address] ->
  -- | Minimum confirmations of the coins
  Natural ->
  ExceptT String (DB m) WalletCoins
fetchWalletCoins ctx pool coinKindM recipients minConf = do
  spendable <- spendableCoins ctx pool minConf
  let coins = filter (\coin -> maybe True (== coinKind coin) coinKindM) spendable
  coinPaths <- Map.fromList <$> mapM (coinWithPath pool) coins
  recipientPaths <- poolPaths pool recipients
  return WalletCoins {coins, coinPaths, recipientPaths}

-- | The address of a coin of the pool, with its path in the pool
coinWithPath ::
  (MonadUnliftIO m) =>
  Pool -> Store.Unspent -> ExceptT String (DB m) (Address, PoolPath)
coinWithPath pool coin = do
  addr <- liftEither $ maybeToEither "A spendable coin has no address" coin.address
  found <- poolAddressPath pool addr >>= liftMaybe "A spendable coin has no derivation path"
  return (addr, snd found)

{- Building -}

-- | The fee rate at which spending a coin is expected to cost in the future,
-- in satoshis per virtual byte: the value Bitcoin Core uses. It only weighs
-- consolidating small coins now against spending them later, so it is not a
-- setting of the user.
longTermRate :: Word64
longTermRate = 10

-- | Build a transaction paying the recipients from the wallet's coins, its
-- change, if any, at the given change address: 'choosePayment' for the
-- type of that address, then 'assemblePayment'
buildWalletTx ::
  Network ->
  Ctx ->
  StdGen ->
  CoinSelection ->
  WalletCoins ->
  -- | The change address, with its path
  (Address, PoolPath) ->
  -- | The recipients and their amounts
  [(Address, Natural)] ->
  -- | Fee per virtual byte
  Natural ->
  -- | Lock time
  Word32 ->
  FeePayer ->
  CoinControl ->
  Either String BuiltTx
buildWalletTx net ctx gen strategy found change recipients feeByte lockTime payer coinControl = do
  chosen <- choosePayment ctx gen strategy found recipients feeByte payer coinControl (pathKind (snd change))
  assemblePayment net ctx found chosen (Just change) lockTime

-- | The type of the change of a payment that spends the coins, when no
-- option names it: the type of the coins when they are all of one type,
-- or else the default type given, that of the configuration. A payment of
-- one type keeps its change in that type. One that mixes the types
-- already links them, and takes the type the user prefers.
changeKindRule :: AccountKind -> [Store.Unspent] -> AccountKind
changeKindRule defaultKind spent = case coinKinds spent of
  [only] -> only
  _ -> defaultKind

-- | Choose the coins of a payment and the type of its change together.
-- With a type given, by --change-type or by the address of --change-back,
-- the coins are chosen for it ('choosePayment'). Without one, the type
-- follows from the coins ('changeKindRule'), and the coins from the fee,
-- which the type of the change is a part of.
--
-- When the candidates are all of one type, the change is of that type and
-- the coins are chosen once. When they are of both, the coins are chosen
-- for the default type first. If the coins chosen turn out to be all of
-- the other type, the rule gives the change that type, so the coins are
-- chosen again, among the candidates of that type alone and for change of
-- that type. Those candidates held the first choice, so they cover the
-- payment, and the second choice is of one type, as the rule wants.
chooseWithChange ::
  Ctx ->
  StdGen ->
  CoinSelection ->
  WalletCoins ->
  -- | The recipients and their amounts
  [(Address, Natural)] ->
  -- | Fee per virtual byte
  Natural ->
  FeePayer ->
  CoinControl ->
  -- | The type of the change, when the command fixes it
  Maybe AccountKind ->
  -- | The default type, for a payment of coins of both types
  AccountKind ->
  Either String ChosenPayment
chooseWithChange ctx gen strategy found recipients feeByte payer coinControl fixedKind defaultKind =
  case fixedKind of
    Just kind -> choose found coinControl kind
    Nothing -> do
      candidates <- candidateCoins found.coins coinControl
      case coinKinds candidates of
        [only] -> choose found coinControl only
        _ -> do
          first <- choose found coinControl defaultKind
          case changeKindRule defaultKind first.selection.selected of
            kind
              | kind == defaultKind -> return first
              | otherwise -> choose (ofKind kind found) (controlOfKind kind coinControl) kind
  where
    choose from control = choosePayment ctx gen strategy from recipients feeByte payer control
    ofKind kind from = from {coins = filter ((== kind) . coinKind) from.coins}
    -- The coins chosen under coin control that are of the kind
    controlOfKind kind control =
      let kept outpoints = [coin.outpoint | coin <- found.coins, coin.outpoint `elem` outpoints, coinKind coin == kind]
       in case control of
            NoCoinControl -> NoCoinControl
            SpendChosen outpoints -> SpendChosen (kept outpoints)
            SelectAmongChosen outpoints -> SelectAmongChosen (kept outpoints)

-- | The coins a payment may spend: every spendable coin of the pool, or
-- those chosen under coin control
candidateCoins :: [Store.Unspent] -> CoinControl -> Either String [Store.Unspent]
candidateCoins coins control = case control of
  NoCoinControl -> Right coins
  SpendChosen chosen -> controlledCoins coins chosen
  SelectAmongChosen chosen -> controlledCoins coins chosen

-- | Choose the coins of a payment to the recipients among the wallet's
-- coins, for change of the type given: by the strategy, or as coin control
-- says ('CoinControl'). When the recipients pay the fee, it is deducted
-- from their amounts in equal parts, the first recipient also paying the
-- remainder. A fee rate of zero is refused: the network would not relay
-- the transaction. So is a recipient paid less than the dust limit of its
-- output ('outputDustLimit'), and a payment that takes more coins than a
-- transaction nodes relay can hold ('checkStandardSize').
choosePayment ::
  Ctx ->
  StdGen ->
  CoinSelection ->
  WalletCoins ->
  -- | The recipients and their amounts
  [(Address, Natural)] ->
  -- | Fee per virtual byte
  Natural ->
  FeePayer ->
  CoinControl ->
  -- | The type of the change output
  AccountKind ->
  Either String ChosenPayment
choosePayment ctx gen strategy found recipients feeByte payer coinControl changeType = do
  when (null recipients) $ Left "No recipients provided"
  checkFeeRate feeByte
  -- The coin selection draws from a generator of its own; the outputs are
  -- shuffled with what is left
  let (drawGen, outputGen) = splitGen gen
  selection <- case coinControl of
    NoCoinControl -> selectCoins strategy drawGen payment found.coins
    SpendChosen chosen -> spendCoins payment =<< controlledCoins found.coins chosen
    SelectAmongChosen chosen -> selectCoins strategy drawGen payment =<< controlledCoins found.coins chosen
  paidRecipients <- case payer of
    SenderPays -> return recipients
    RecipientsPay -> do
      -- A fee at or above the amount leaves the recipients nothing
      when (selection.paid == 0) $ Left "Recipients can't pay for the fee"
      recipientsPayFee (payment.amount - selection.paid) recipients
  -- The change is never below the dust limit: below it, it goes to the fee
  forM_ paidRecipients $ \(addr, amount) ->
    when (amount < dustOf addr) $ Left (belowDustLimit amount (dustOf addr))
  checkStandardSize
    (coinInput <$> selection.selected)
    (payment.outputKinds <> [changeOutput | selection.changeAmount > 0])
  return ChosenPayment {selection, paidRecipients, changeType, outputGen}
  where
    -- The change is an output of the type given
    changeOutput = accountOutputKind changeType
    -- The dust limit of the output that pays an address
    dustOf addr = outputDustLimit (recipientOutputKind ctx addr)
    payment =
      Payment
        { changeKind = changeType,
          rates = FeeRates {feeRate = fromIntegral feeByte, longTermFeeRate = longTermRate},
          payer,
          dust = fromIntegral (outputDustLimit changeOutput),
          outputKinds = recipientOutputKind ctx . fst <$> recipients,
          amount = sum $ fromIntegral . snd <$> recipients
        }

-- | What a payment is refused with when a recipient would get less than
-- the dust limit of its output: the amount, after its share of the fee
-- when the recipients pay it, and the limit. The limit depends on the kind
-- of the address, 546 satoshis for a legacy one and 294 for a native
-- SegWit one, so the message gives it. It said only that an output was
-- smaller than the dust value before, and the user had to find which
-- value.
belowDustLimit :: Natural -> Natural -> String
belowDustLimit amount limit =
  "A recipient would get "
    <> show amount
    <> " satoshis, less than the dust limit of "
    <> show limit
    <> " satoshis for its kind of address. Nodes do not relay an output that small"

-- | The addresses the chosen coins come from, each once, in the order of
-- the coins
spentAddresses :: ChosenPayment -> [Address]
spentAddresses chosen = nub $ mapMaybe (.address) chosen.selection.selected

-- | The transaction of a payment whose coins are chosen: its recipients,
-- and its change at the given address, in an order drawn at random, with
-- the lock time given (see "Haskoin.Wallet.Tx.LockTime"). A payment with
-- change needs the address, which must be of the type the coins were
-- chosen for; one without ignores it. Every output to the wallet, the
-- change and a recipient of the portfolio, has its path, and so has every
-- input.
assemblePayment :: Network -> Ctx -> WalletCoins -> ChosenPayment -> Maybe (Address, PoolPath) -> Word32 -> Either String BuiltTx
assemblePayment net ctx found chosen changeM lockTime = do
  change <- case (chosen.selection.changeAmount, changeM) of
    (0, _) -> Right []
    (amount, Just (addr, path))
      | pathKind path == chosen.changeType -> Right [((addr, amount), path)]
      | otherwise -> Left ("The change address is not a " <> cs (kindName chosen.changeType) <> " address, the type the fee was counted for")
    (_, Nothing) -> Left "The payment has change, and no address for it"
  let outputs = (fst <$> change) <> (second fromIntegral <$> chosen.paidRecipients)
      shuffledOutputs = evalState (randomShuffle outputs) chosen.outputGen
  tx <- withHeader net lockTime <$> (buildAddrTx net ctx ((.outpoint) <$> chosen.selection.selected) =<< mapM recipientText shuffledOutputs)
  inputPaths <- nub <$> mapM coinPath chosen.selection.selected
  let outputPaths =
        nub $ (snd <$> change) <> mapMaybe ((`Map.lookup` found.recipientPaths) . fst) chosen.paidRecipients
  return BuiltTx {tx, spentCoins = chosen.selection.selected, inputPaths, outputPaths}
  where
    -- The builder of haskoin takes the recipients as address text
    recipientText (addr, amount) = (,amount) <$> addrToTextE net addr
    coinPath :: Store.Unspent -> Either String PoolPath
    coinPath coin = do
      addr <- maybeToEither "A chosen coin has no address" coin.address
      maybeToEither "A chosen coin has no derivation path" $ Map.lookup addr found.coinPaths

-- | The coins chosen under coin control, among the spendable coins of the
-- pool, those of one type when the payment is narrowed to it. Every chosen
-- coin must be one of them: the command checks it first, and tells which
-- are not and why.
controlledCoins :: [Store.Unspent] -> [OutPoint] -> Either String [Store.Unspent]
controlledCoins coins control
  | length controlled < length (nub control) = Left "A chosen coin does not exist or is not spendable"
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
