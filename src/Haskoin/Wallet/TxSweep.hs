{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Sweeping the coins of external private keys into the wallet.
--
-- 'fetchSweepCoins' finds the coins to sweep: those at the given addresses
-- and at every address where a given key may hold them, and the coins given
-- by outpoint, so that a sweep can be prepared without the keys at hand;
-- 'buildSweepSignData' builds the transaction spending them all to the
-- destinations, in the wallet or not, with a fee on the size 'sweepTxSize'
-- estimates from the kinds of coins, and 'signTxWithKeys' signs it with the
-- keys: P2PKH, P2PK, P2WPKH and P2SH-P2WPKH coins, each with the key that
-- matches its output script. Analysis and verification come from
-- "Haskoin.Wallet.TxSigning".
module Haskoin.Wallet.TxSweep
  ( SweepCoins (..),
    buildSweepSignData,
    coinsOfTransactions,
    fetchSweepCoins,
    keyAddresses,
    signTxWithKeys,
    sweepTxSize,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (foldM, when)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Function (on)
import Data.List (find, nub, nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Data.Word (Word64)
import Haskoin.Wallet.CoinSelection (varIntSize)
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
    walletAddrs :: !(Map Address SoftPath),
    -- | The P2PKH addresses of the uncompressed encodings of the keys:
    -- their inputs carry a 65-byte public key instead of 33. Known only
    -- when the keys are given; a P2PKH coin of an unknown key is sized for
    -- a compressed one.
    uncompressedAddrs :: ![Address]
  }

-- | Fetch the coins to sweep: every coin at the given addresses and at the
-- addresses of the given keys, and the coins given by outpoint, each once;
-- the transactions they come from; and which of the addresses involved
-- belong to the wallet. The keys may be absent, when they are kept
-- elsewhere: the sweep is then prepared from addresses or coins alone.
fetchSweepCoins ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Config ->
  DBAccountId ->
  -- | The private keys, if at hand
  [SecKey] ->
  -- | The addresses to sweep from
  [Address] ->
  -- | The coins to sweep, by transaction hash and output index
  [(TxHash, Natural)] ->
  -- | The destinations
  [Address] ->
  ExceptT String (DB m) SweepCoins
fetchSweepCoins net ctx cfg accId secKeys sweepFrom given sweepTo = do
  let host = apiHost net cfg
      addresses = nub $ sweepFrom <> concatMap (keyAddresses net ctx) secKeys
  when (null addresses && null given) $
    throwError "No private keys, addresses or coins to sweep from"
  Store.SerialList found <-
    if null addresses
      then return $ Store.SerialList []
      else
        liftExcept . apiBatch ctx (configCoinBatch cfg) host $
          GetAddrsUnspent addresses def {limit = Just 0}
  Store.SerialList parents <-
    if null given
      then return $ Store.SerialList []
      else
        liftExcept . apiBatch ctx (configTxFullBatch cfg) host $
          GetTxs (nub $ fst <$> given)
  givenCoins <- liftEither $ coinsOfTransactions given parents
  let coins = nubBy ((==) `on` (.outpoint)) (found <> givenCoins)
  Store.RawResultList transactions <-
    liftExcept . apiBatch ctx (configTxFullBatch cfg) host $
      GetTxsRaw (nub $ (.outpoint.hash) <$> coins)
  walletAddrs <- walletPaths net accId (mapMaybe (.address) coins <> sweepTo)
  let uncompressedAddrs = nub $ uncompressedKeyAddr ctx <$> secKeys
  return SweepCoins {coins, transactions, walletAddrs, uncompressedAddrs}

-- | The coins given by outpoint, read from their transactions as the store
-- reports them: each must be an output of a transaction the store knows,
-- that is not spent
coinsOfTransactions :: [(TxHash, Natural)] -> [Store.Transaction] -> Either String [Store.Unspent]
coinsOfTransactions given parents = mapM coinOf given
  where
    coinOf (hash, index) = do
      let name = "Coin " <> cs (txHashToHex hash) <> " " <> show index
      tx <-
        maybeToEither (name <> " is unknown to the store") $
          find (\t -> t.txid == hash && not t.deleted) parents
      output <-
        maybeToEither (name <> " does not exist: the transaction has " <> show (length tx.outputs) <> " outputs") $
          nth index tx.outputs
      when (isJust output.spender) $ Left $ name <> " is already spent"
      return
        Store.Unspent
          { Store.block = tx.block,
            Store.outpoint = OutPoint hash (fromIntegral index),
            Store.value = output.value,
            Store.script = output.script,
            Store.address = output.address
          }
    nth index xs = listToMaybe $ drop (fromIntegral index) xs

-- | The addresses where a key may hold coins: the P2PKH addresses of both
-- encodings of its public key and, on SegWit networks, the P2WPKH and
-- P2SH-P2WPKH addresses of the compressed one (SegWit requires compressed
-- public keys).
keyAddresses :: Network -> Ctx -> SecKey -> [Address]
keyAddresses net ctx key =
  [pubKeyAddr ctx compressed, uncompressedKeyAddr ctx key]
    <> [a | net.segWit, a <- [pubKeyWitnessAddr ctx compressed, pubKeyCompatWitnessAddr ctx compressed]]
  where
    compressed = derivePublicKey ctx (wrapSecKey True key)

-- | The P2PKH address of the uncompressed encoding of the key's public key
uncompressedKeyAddr :: Ctx -> SecKey -> Address
uncompressedKeyAddr ctx key = pubKeyAddr ctx $ derivePublicKey ctx (wrapSecKey False key)

-- | The sweep transaction and its signing data: all the coins spent to the
-- destinations, both in random order, with what is left after the estimated
-- fee split evenly between the destinations. The signing data carries the
-- transactions the coins come from and the paths of the wallet's own
-- addresses among the coins and the destinations, which may be none: a
-- sweep need not involve the wallet, but only one that does can wait among
-- its pending transactions.
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
  scripts <- mapM scriptOf sweepTo
  size <- sweepTxSize net ctx found scripts
  let total = sum $ (.value) <$> found.coins
      fee = fromIntegral feeByte * size
  when (total < fee) $ Left "Could not find a sweep solution: fee is too large"
  let (each, remainder) = (total - fee) `quotRem` fromIntegral (length sweepTo)
  when (each < fromIntegral dust) $ Left "Outputs are smaller than the dust value"
  let (shuffledCoins, shuffledScripts) =
        evalState ((,) <$> randomShuffle found.coins <*> randomShuffle scripts) gen
      outputs = [TxOut amount (marshal ctx script) | (script, amount) <- zip shuffledScripts ((each + remainder) : repeat each)]
      tx = Tx 1 [TxIn c.outpoint BS.empty maxBound | c <- shuffledCoins] outputs [] 0
      inputPaths = pathsOf $ mapMaybe (.address) found.coins
  return $ TxSignData tx found.transactions inputPaths (pathsOf sweepTo) (Just feeByte) False
  where
    pathsOf addrs = nub $ mapMaybe (`Map.lookup` found.walletAddrs) addrs
    scriptOf addr = maybeToEither ("Can not pay to " <> addressText net addr) (addressToOutput addr)

{- Size -}

-- | The estimated size of the sweep transaction paying the given output
-- scripts, in bytes of the serialized transaction, which is how hw measures
-- sizes and fee rates everywhere. Every input is sized by the kind of its
-- coin ('inputSize'), every output by its script. Signatures are counted
-- at their largest, so the estimate is an upper bound, by a byte or two
-- per input.
--
-- The network charges fees by weight, where the bytes of the witnesses
-- count a quarter: a sweep of SegWit coins at a given rate per byte pays
-- more per virtual byte than that rate.
sweepTxSize :: Network -> Ctx -> SweepCoins -> [ScriptOutput] -> Either String Word64
sweepTxSize net ctx found outputs = do
  inputs <- mapM (inputSize net ctx found.uncompressedAddrs) found.coins
  let -- 4 bytes of version, the counts of inputs and outputs, 4 of lock time
      overhead = 8 + varIntSize (length found.coins) + varIntSize (length outputs)
      witnesses = (.witnessBytes) <$> inputs
      -- A transaction with a witness carries a marker byte and a flag byte,
      -- then a witness for every input: an empty one, a single count byte,
      -- for an input without
      witnessData
        | all (== 0) witnesses = 0
        | otherwise = 2 + sum [max 1 w | w <- witnesses]
  return $ overhead + sum ((.inputBytes) <$> inputs) + sum (outputSize ctx <$> outputs) + witnessData

-- | The size of a signed input: its bytes among the inputs, and the bytes
-- of its witness with their item count, 0 for an input without a witness
data InputSize = InputSize
  { inputBytes :: !Word64,
    witnessBytes :: !Word64
  }

-- | The size of the signed input of a coin, by the kind of its output
-- script. An input is 32 bytes of transaction hash, 4 of output index, 1 of
-- script length, the script and 4 of sequence; a signature is counted as 72
-- bytes with its hash type, and the push of it as 1 more.
--
--   * P2PKH: the script pushes the signature and the public key, 33 bytes
--     compressed or 65 uncompressed: 148 or 180 bytes.
--   * P2PK: the script pushes the signature alone: 114 bytes.
--   * P2WPKH: an empty script, 41 bytes; the witness holds the signature and
--     the compressed public key, each with its length, after their count:
--     108 bytes.
--   * P2SH-P2WPKH: the script pushes the 22-byte P2WPKH script, 64 bytes,
--     with the same witness. A P2SH coin of a sweep is one of these, the
--     only script hash a sweep looks for.
inputSize :: Network -> Ctx -> [Address] -> Store.Unspent -> Either String InputSize
inputSize net ctx uncompressedAddrs coin = do
  script <- either (const unsupported) Right $ unmarshal ctx coin.script
  case script of
    PayPKHash h
      | PubKeyAddress h `elem` uncompressedAddrs -> Right $ InputSize 180 0
      | otherwise -> Right $ InputSize 148 0
    PayPK _ -> Right $ InputSize 114 0
    PayWitnessPKHash _ -> Right $ InputSize 41 108
    PayScriptHash _ -> Right $ InputSize 64 108
    _ -> unsupported
  where
    unsupported =
      Left $ "Unsupported output script for the coin at " <> maybe "unknown address" (addressText net) coin.address

-- | The size of an output: 8 bytes of amount, the length of the script and
-- the script
outputSize :: Ctx -> ScriptOutput -> Word64
outputSize ctx script = 8 + varIntSize scriptLength + fromIntegral scriptLength
  where
    scriptLength = BS.length $ marshal ctx script

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
