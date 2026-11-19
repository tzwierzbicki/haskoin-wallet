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
-- A coin worth no more than the fee to spend it is a dust coin
-- ("Haskoin.Wallet.Tx.Dust"). preparesendall asks the user whether to spend
-- the dust coins too, after telling what each choice costs
-- ('sendAllDustReport'). Its answer lists every coin left in the portfolio,
-- and why ('Haskoin.Wallet.Database.Coin.leftCoinsOf').
module Haskoin.Wallet.Tx.SendAll
  ( walletSized,
    sendAllFee,
    sendAllAmounts,
    SendAllCoins (..),
    buildSendAll,
    noCoinsToSend,
    buildSendAllSignData,
    sendAllDustReport,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State (evalState)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Address (poolPaths)
import Haskoin.Wallet.Database.Coin (coinParents, coinsSpentBy)
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath)
import Haskoin.Wallet.Tx.Building (BuiltTx (..), coinWithPath)
import Haskoin.Wallet.Tx.CoinSelection (coinInput, coinKinds)
import Haskoin.Wallet.Tx.Dust (DustPlace (..), DustReport, SizedCoin (..), dustReport, spendingAmounts, spendingFee)
import Haskoin.Wallet.Tx.LockTime (withHeader)
import Haskoin.Wallet.Tx.Recipient (Share (..))
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Size (accountOutputKind, checkFeeRate, checkStandardSize, recipientOutputKind)
import Haskoin.Wallet.Util (addrToTextE, randomShuffle)
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Coins of the wallet -}

-- | A coin of the wallet with the input that spends it, which the type of
-- its account sets: 148 virtual bytes for a P2PKH coin, of a BIP44
-- account, and 68 for a P2WPKH one, of a BIP84 account
walletSized :: Store.Unspent -> SizedCoin
walletSized coin = SizedCoin {coin, input = coinInput coin}

{- Amounts -}

-- | The fee of the transaction spending the coins to the recipients, at
-- the fee rate, each input of the kind of its coin
sendAllFee :: Natural -> [Store.Unspent] -> [Address] -> Natural
sendAllFee feeByte coins = spendingFee feeByte (walletSized <$> coins)

-- | The amount of each recipient when the coins pay them and their fee at
-- the rate: see 'spendingAmounts'
sendAllAmounts :: Natural -> [Store.Unspent] -> [(Address, Share)] -> Either String [(Address, Natural)]
sendAllAmounts feeByte coins = spendingAmounts feeByte (walletSized <$> coins)

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

-- | The refusal of a payment of everything when the portfolio can spend
-- no coin. The command adds what the portfolio holds and cannot spend
-- yet (@tellingHeldBack@ of "Haskoin.Wallet.Commands").
noCoinsToSend :: String
noCoinsToSend = "There are no coins to send"

-- | Build the transaction spending all the coins to the recipients, with no
-- change, its outputs shuffled (see 'sendAllAmounts'), with the lock time
-- given (see "Haskoin.Wallet.Tx.LockTime"). A fee rate no transaction can
-- be built with is refused, and so are more coins than a transaction nodes
-- relay can hold ('checkStandardSize').
buildSendAll :: Network -> StdGen -> SendAllCoins -> [(Address, Share)] -> Natural -> Word32 -> Either String BuiltTx
buildSendAll net gen found recipients feeByte lockTime = do
  checkFeeRate feeByte
  when (null found.coins) $ Left noCoinsToSend
  checkStandardSize (coinInput <$> found.coins) (recipientOutputKind . fst <$> recipients)
  amounts <- sendAllAmounts feeByte found.coins recipients
  tx <- withHeader net lockTime <$> (buildAddrTx net ((.outpoint) <$> found.coins) =<< mapM outputText (evalState (randomShuffle amounts) gen))
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
buildSendAllSignData gen pool coins recipients feeByte lockTime = do
  coinPaths <- Map.fromList <$> mapM (coinWithPath pool) coins
  recipientPaths <- poolPaths pool (fst <$> recipients)
  built <- liftEither $ buildSendAll pool.poolNetwork gen SendAllCoins {coins, coinPaths, recipientPaths} recipients feeByte lockTime
  spentFrom <- coinParents (coinsSpentBy built.tx coins)
  return $ TxSignData built.tx spentFrom built.inputPaths built.outputPaths (Just feeByte) Nothing [] noLabel

{- The question about dust coins -}

-- | The report on the dust coins of a portfolio, for the question
-- preparesendall asks, from the coins worth spending and the dust coins:
-- see 'dustReport'. It tells whether other coins stay in the portfolio
-- anyway, locked ones for instance. The dust coins that are left could be
-- sent later to an address of their own type when they are all of one, and
-- to a bip44 address otherwise.
sendAllDustReport :: Natural -> [(Address, Share)] -> [Store.Unspent] -> [Store.Unspent] -> Bool -> DustReport
sendAllDustReport feeByte recipients worth dustCoins othersStay =
  dustReport (InPortfolio othersStay) feeByte recipients (accountOutputKind laterKind) (walletSized <$> worth) (walletSized <$> dustCoins)
  where
    laterKind = case coinKinds dustCoins of
      [only] -> only
      _ -> Bip44
