{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Sweeping the coins of external private keys into the wallet.
--
-- 'fetchSweepCoins' finds the coins of the keys at every address where a
-- key may hold them, 'buildSweepSignData' builds the transaction spending
-- them all to the destinations, and 'signTxWithKeys' signs it with the keys:
-- P2PKH, P2PK, P2WPKH and P2SH-P2WPKH coins, each with the key that matches
-- its output script. Analysis and verification come from
-- "Haskoin.Wallet.TxSigning".
module Haskoin.Wallet.TxSweep
  ( SweepCoins (..),
    buildSweepSignData,
    fetchSweepCoins,
    keyAddresses,
    signTxWithKeys,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (foldM, when)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.CoinSelection (txSize)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO (TxSignData (..))
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSigning (parseTxSignData, verifySignedTx)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Building -}

-- | What a sweep needs from the network and the database
data SweepCoins = SweepCoins
  { -- | The coins of the keys to sweep
    coins :: ![Store.Unspent],
    -- | The transactions the coins come from
    transactions :: ![Tx],
    -- | The wallet's own addresses among the coins and the destinations
    walletAddrs :: !(Map Address SoftPath)
  }

-- | Fetch the coins of the keys at every address where a key may hold them,
-- the transactions they come from, and which of the addresses involved
-- belong to the wallet
fetchSweepCoins ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Config ->
  DBAccountId ->
  [SecKey] ->
  -- | The destinations
  [Address] ->
  ExceptT String (DB m) SweepCoins
fetchSweepCoins net ctx cfg accId secKeys sweepTo = do
  when (null secKeys) $ throwError "No private keys to sweep from"
  let host = apiHost net cfg
      sweepFrom = nub $ concatMap (keyAddresses net ctx) secKeys
  Store.SerialList coins <-
    liftExcept . apiBatch ctx (configCoinBatch cfg) host $
      GetAddrsUnspent sweepFrom def {limit = Just 0}
  Store.RawResultList transactions <-
    liftExcept . apiBatch ctx (configTxFullBatch cfg) host $
      GetTxsRaw (nub $ (.outpoint.hash) <$> coins)
  walletAddrs <- walletPaths net accId (mapMaybe (.address) coins <> sweepTo)
  return SweepCoins {coins, transactions, walletAddrs}

-- | The addresses where a key may hold coins: the P2PKH addresses of both
-- encodings of its public key and, on SegWit networks, the P2WPKH and
-- P2SH-P2WPKH addresses of the compressed one (SegWit requires compressed
-- public keys).
keyAddresses :: Network -> Ctx -> SecKey -> [Address]
keyAddresses net ctx key =
  [pubKeyAddr ctx compressed, pubKeyAddr ctx uncompressed]
    <> [a | net.segWit, a <- [pubKeyWitnessAddr ctx compressed, pubKeyCompatWitnessAddr ctx compressed]]
  where
    compressed = derivePublicKey ctx (wrapSecKey True key)
    uncompressed = derivePublicKey ctx (wrapSecKey False key)

-- | The sweep transaction and its signing data: all the coins spent to the
-- destinations, both in random order, with what is left after the estimated
-- fee split evenly between the destinations. The signing data carries the
-- transactions the coins come from and the paths of the wallet's own
-- addresses among the coins and the destinations.
buildSweepSignData ::
  Network ->
  Ctx ->
  StdGen ->
  SweepCoins ->
  -- | The destinations
  [Address] ->
  -- | Fee per byte
  Natural ->
  -- | Dust
  Natural ->
  Either String TxSignData
buildSweepSignData net ctx gen found sweepTo feeByte dust = do
  when (null found.coins) $ Left "There are no coins to sweep in those addresses"
  when (null sweepTo) $ Left "No addresses to sweep to"
  when (feeByte == 0) $ Left "The fee rate must be at least 1 satoshi per byte"
  let total = sum $ (.value) <$> found.coins
      fee = fromIntegral feeByte * txSize (length found.coins) (length sweepTo)
  when (total < fee) $ Left "Could not find a sweep solution: fee is too large"
  let (each, remainder) = (total - fee) `quotRem` fromIntegral (length sweepTo)
  when (each < fromIntegral dust) $ Left "Outputs are smaller than the dust value"
  outputs <- mapM payTo $ zip destinations ((each + remainder) : repeat each)
  let tx = Tx 1 [TxIn c.outpoint BS.empty maxBound | c <- shuffledCoins] outputs [] 0
      inputPaths = pathsOf $ mapMaybe (.address) found.coins
  return $ TxSignData tx found.transactions inputPaths (pathsOf sweepTo) (Just feeByte) False
  where
    (shuffledCoins, destinations) =
      evalState ((,) <$> randomShuffle found.coins <*> randomShuffle sweepTo) gen
    pathsOf addrs = nub $ mapMaybe (`Map.lookup` found.walletAddrs) addrs
    payTo (addr, amount) =
      TxOut amount . marshal ctx
        <$> maybeToEither ("Can not pay to " <> addressText net addr) (addressToOutput addr)

{- Signing -}

-- | Sign a transaction with the given private keys: every input is signed
-- with the key that matches the output script of its coin. The account
-- public key only identifies the wallet's own inputs and outputs; the paths
-- of the 'TxSignData' are relative to it.
signTxWithKeys ::
  Network ->
  Ctx ->
  TxSignData ->
  XPubKey ->
  [SecKey] ->
  Either String (TxSignData, TxInfo)
signTxWithKeys net ctx tsd accountPub secKeys = do
  when tsd.txSignDataSigned $ Left "The transaction is already signed"
  when (null secKeys) $ Left "There are no private keys to sign"
  info <- parseTxSignData net ctx accountPub tsd
  -- The sweep builder refuses a fee rate of zero; the signer checks the fee itself
  when (info.txInfoFee == 0) $ Left "The transaction pays no fee"
  coins <- inputCoins tsd.txSignDataTx info
  signedTx <- foldM (signCoin net ctx (indexKeys ctx secKeys)) tsd.txSignDataTx (zip [0 ..] coins)
  let signedData = tsd {txSignDataTx = signedTx, txSignDataSigned = True}
  verifySignedTx net ctx signedData
  signedInfo <- parseTxSignData net ctx accountPub signedData
  return (signedData, signedInfo)

-- | The signing parameters of the coin of every input, in the order of the
-- inputs. The analysis groups them by address; every input has one.
inputCoins :: Tx -> TxInfo -> Either String [SigInput]
inputCoins tx info = mapM coinOf tx.inputs
  where
    coins =
      concatMap (.myInputsSigInput) (Map.elems info.txInfoMyInputs)
        <> concatMap (.otherInputsSigInput) (Map.elems info.txInfoOtherInputs)
    coinOf :: TxIn -> Either String SigInput
    coinOf input =
      maybeToEither "Referenced input transactions are missing" $
        find ((== input.outpoint) . (.outpoint)) coins

-- | The private keys, indexed by what identifies them in an output script.
-- Every key is indexed with both encodings of its public key.
data Keys = Keys
  { -- | By the hash of the public key: P2PKH and P2WPKH coins
    byKeyHash :: !(Map Hash160 PrivateKey),
    -- | By the encoded public key: P2PK coins
    byPubKey :: !(Map BS.ByteString PrivateKey),
    -- | By the hash of the P2WPKH script of the key, with that script:
    -- P2SH-P2WPKH coins
    byWitnessScriptHash :: !(Map Hash160 (PrivateKey, ScriptOutput))
  }

indexKeys :: Ctx -> [SecKey] -> Keys
indexKeys ctx secKeys =
  Keys
    { byKeyHash = Map.fromList [(keyHash pub, key) | (key, pub) <- keys],
      byPubKey = Map.fromList [(marshal ctx pub, key) | (key, pub) <- keys],
      byWitnessScriptHash =
        Map.fromList
          [ (addressHash (marshal ctx witnessScript), (key, witnessScript))
            | (key, pub) <- keys,
              pub.compress,
              let witnessScript = PayWitnessPKHash (keyHash pub)
          ]
    }
  where
    keys =
      [ (key, derivePublicKey ctx key)
        | sec <- secKeys,
          compressed <- [True, False],
          let key = wrapSecKey compressed sec
      ]
    keyHash :: PublicKey -> Hash160
    keyHash pub = addressHash (marshal ctx pub)

-- | Sign input i of the transaction with the key of its coin. A P2SH-P2WPKH
-- coin is signed as a nested witness: the scriptSig pushes the P2WPKH script.
signCoin :: Network -> Ctx -> Keys -> Tx -> (Int, SigInput) -> Either String Tx
signCoin net ctx keys tx (i, coin) =
  case coin.script of
    PayPKHash h -> withKey (Map.lookup h keys.byKeyHash) $ signInput net ctx tx i coin
    PayPK pub -> withKey (Map.lookup (marshal ctx pub) keys.byPubKey) $ signInput net ctx tx i coin
    PayWitnessPKHash h -> withKey (Map.lookup h keys.byKeyHash) $ signInput net ctx tx i coin
    PayScriptHash h -> case Map.lookup h keys.byWitnessScriptHash of
      Just (key, witnessScript) ->
        signNestedInput net ctx tx i (SigInput witnessScript coin.value coin.outpoint coin.sighash Nothing) key
      Nothing -> noKey
    _ -> Left $ "Unsupported output script for " <> describeInput
  where
    withKey maybeKey sign = maybe noKey sign maybeKey
    noKey = Left $ "No private key for " <> describeInput
    describeInput =
      "input " <> show i <> " (" <> maybe "unknown address" (addressText net) (outputAddress ctx coin.script) <> ")"

addressText :: Network -> Address -> String
addressText net addr = Text.unpack $ fromMaybe ("unknown address" :: Text) (addrToText net addr)
