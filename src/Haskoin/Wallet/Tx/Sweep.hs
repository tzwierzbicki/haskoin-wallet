{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Sweeping the coins of external private keys into the wallet.
--
-- 'fetchSweepCoins' asks the store for the coins to sweep: those at the
-- given addresses and at every address where a given key may hold them, and
-- the coins given by outpoint, so that a sweep can be prepared without the
-- keys at hand;
-- 'buildSweepSignData' builds the transaction spending them all to the
-- destinations, in the wallet or not, with a fee on the size 'sweepTxSize'
-- estimates from the kinds of coins, and 'signTxWithKeys' signs it with the
-- keys: P2PKH, P2PK, P2WPKH and P2SH-P2WPKH coins, each with the key that
-- matches its output script. Analysis and verification come from
-- "Haskoin.Wallet.Tx.Signing".
module Haskoin.Wallet.Tx.Sweep
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
import Data.Function (on)
import Data.List (find, nub, nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Word (Word32, Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Coin (coinText)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Keys (AccountKind)
import Haskoin.Wallet.Tx.LockTime (inputSequence, txVersion)
import Haskoin.Wallet.Tx.Recipient (Share, shareAmounts)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (coinSigInput, parseOutsideTxSignData, parseTxSignData, spentCoins, verifySignedTx)
import Haskoin.Wallet.Tx.Size (checkFeeRate, checkStandardSize, inputKind, outputDustLimit, recipientOutputKind, scriptOutputKind, txSize)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Fetching from the store -}

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
  deriving (Eq, Show)

-- | Ask the store for the coins to sweep: every coin at the given
-- addresses and at the addresses of the given keys, and the coins given by
-- outpoint, each once; and the transactions they come from. The database
-- says which of the addresses involved belong to the account, when the
-- sweep pays one; a sweep outside the wallet has none. The keys may be
-- absent, when they are kept elsewhere: the sweep is then prepared from
-- addresses or coins alone.
fetchSweepCoins ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Store ->
  -- | The account the sweep pays, if any
  Maybe DBAccountId ->
  -- | The private keys, if at hand
  [SecKey] ->
  -- | The addresses to sweep from
  [Address] ->
  -- | The coins to sweep, by outpoint
  [OutPoint] ->
  -- | The destinations
  [Address] ->
  ExceptT String (DB m) SweepCoins
fetchSweepCoins net ctx store accIdM secKeys sweepFrom given sweepTo = do
  let addresses = nub $ sweepFrom <> concatMap (keyAddresses net ctx) secKeys
  when (null addresses && null given) $
    throwError "No private keys, addresses or coins to sweep from"
  fromStore $ storeHealth store
  found <-
    if null addresses
      then return []
      else fromStore $ storeUnspents store addresses
  parents <-
    if null given
      then return []
      else fromStore $ storeTransactions store (nub $ (.hash) <$> given)
  givenCoins <- liftEither $ coinsOfTransactions given parents
  let coins = nubBy ((==) `on` (.outpoint)) (found <> givenCoins)
      spentFrom = nub $ (.outpoint.hash) <$> coins
  -- The store lists the coins, so it must have every transaction they
  -- come from
  transactions <- fromStore $ rawTransactions store spentFrom
  walletAddrs <- maybe (return Map.empty) (\accId -> walletPaths net accId (mapMaybe (.address) coins <> sweepTo)) accIdM
  let uncompressedAddrs = nub $ uncompressedKeyAddr ctx <$> secKeys
  return SweepCoins {coins, transactions, walletAddrs, uncompressedAddrs}

-- | The coins given by outpoint, read from their transactions as the store
-- reports them: each must be an output of a transaction the store knows,
-- that is not spent
coinsOfTransactions :: [OutPoint] -> [Store.Transaction] -> Either String [Store.Unspent]
coinsOfTransactions given parents = mapM coinOf given
  where
    coinOf :: OutPoint -> Either String Store.Unspent
    coinOf outpoint = do
      let name = "Coin " <> coinText outpoint
      tx <-
        maybeToEither (name <> " is unknown to the store") $
          find (\t -> t.txid == outpoint.hash && not t.deleted) parents
      output <-
        maybeToEither (name <> " does not exist: the transaction has " <> show (length tx.outputs) <> " outputs") $
          nth outpoint.index tx.outputs
      when (isJust output.spender) $ Left $ name <> " is already spent"
      return
        Store.Unspent
          { Store.block = tx.block,
            Store.outpoint = outpoint,
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
    <> [a | net.segWit, a <- segWitAddresses]
  where
    compressed = derivePublicKey ctx (wrapSecKey True key)
    -- haskoin-core gives no SegWit address to an uncompressed key. The key
    -- here is compressed, so both addresses exist.
    segWitAddresses = catMaybes [pubKeyWitnessAddr ctx compressed, pubKeyCompatWitnessAddr ctx compressed]

-- | The P2PKH address of the uncompressed encoding of the key's public key
uncompressedKeyAddr :: Ctx -> SecKey -> Address
uncompressedKeyAddr ctx key = pubKeyAddr ctx $ derivePublicKey ctx (wrapSecKey False key)

{- Building -}

-- | The sweep transaction and its signing data: all the coins spent to the
-- destinations, both in random order, with the lock time given (see
-- "Haskoin.Wallet.Tx.LockTime"). The destinations with a fixed amount get
-- it, and what is left after the estimated fee is split evenly between
-- the others ('shareAmounts'). The signing data carries the transactions
-- the coins come from and the paths of the wallet's own addresses among the
-- coins and the destinations, which may be none: a sweep need not involve
-- the wallet, but only one that does can wait among its pending
-- transactions.
buildSweepSignData ::
  Network ->
  Ctx ->
  StdGen ->
  SweepCoins ->
  -- | The destinations, with what each gets
  [(Address, Share)] ->
  -- | Fee per virtual byte
  Natural ->
  -- | Lock time
  Word32 ->
  Either String TxSignData
buildSweepSignData net ctx gen found sweepTo feeByte lockTime = do
  when (null found.coins) $ Left "There are no coins to sweep in those addresses"
  when (null sweepTo) $ Left "No addresses to sweep to"
  checkFeeRate feeByte
  scripts <- mapM (scriptOf . fst) sweepTo
  size <- sweepTxSize net ctx found scripts
  let total = sum $ (.value) <$> found.coins
      fee = fromIntegral feeByte * size
  amounts <- shareAmounts (fromIntegral total) (fromIntegral fee) (outputDustLimit . recipientOutputKind ctx) sweepTo
  let (shuffledCoins, shuffledOutputs) =
        evalState ((,) <$> randomShuffle found.coins <*> randomShuffle (zip scripts (snd <$> amounts))) gen
      outputs = [TxOut (fromIntegral amount) (marshal ctx script) | (script, amount) <- shuffledOutputs]
      tx = Tx txVersion [TxIn c.outpoint BS.empty (inputSequence net) | c <- shuffledCoins] outputs [] lockTime
      inputPaths = pathsOf $ mapMaybe (.address) found.coins
  return $ TxSignData tx found.transactions inputPaths (pathsOf (fst <$> sweepTo)) (Just feeByte) Nothing [] noLabel
  where
    pathsOf addrs = nub $ mapMaybe (`Map.lookup` found.walletAddrs) addrs
    scriptOf addr = maybeToEither ("Can not pay to " <> addrToString net addr) (addressToOutput addr)

{- Size -}

-- | The estimated size of the sweep transaction paying the given output
-- scripts, in virtual bytes as "Haskoin.Wallet.Tx.Size" counts it: every
-- input by the kind of its coin, and by the encoding of its key for a
-- P2PKH coin when the keys were given, every output by its script
--
-- A sweep of more coins than a transaction nodes relay can hold is
-- refused ('checkStandardSize').
sweepTxSize :: Network -> Ctx -> SweepCoins -> [ScriptOutput] -> Either String Word64
sweepTxSize net ctx found outputs = do
  inputs <- mapM (sweptInputKind net ctx found.uncompressedAddrs) found.coins
  let outputKinds = scriptOutputKind ctx . marshal ctx <$> outputs
  checkStandardSize inputs outputKinds
  return $ txSize inputs outputKinds

-- | The kind of the signed input of a coin of the sweep: a P2PKH coin at
-- an address of an uncompressed key carries the 65-byte public key; a coin
-- of a kind the sweep cannot sign is refused here, before signing
sweptInputKind :: Network -> Ctx -> [Address] -> Store.Unspent -> Either String InputKind
sweptInputKind net ctx uncompressedAddrs coin = do
  script <- either (const unsupported) Right $ unmarshal ctx coin.script
  case script of
    PayPKHash h | PubKeyAddress h `elem` uncompressedAddrs -> Right InputUncompressedPKHash
    _ -> maybe unsupported Right (inputKind script)
  where
    unsupported =
      Left $ "Unsupported output script for the coin at " <> maybe "unknown address" (addrToString net) coin.address

{- Signing -}

-- | Sign a transaction with the given private keys: every input is signed
-- with the key that matches the output script of its coin. The account,
-- its kind and its public key, only identifies the wallet's own inputs and
-- outputs, the paths of the 'TxSignData' being relative to it; a sweep
-- outside the wallet has no account.
signTxWithKeys ::
  Network ->
  Ctx ->
  TxSignData ->
  Maybe (AccountKind, XPubKey) ->
  [SecKey] ->
  Either String (TxSignData, TxInfo)
signTxWithKeys net ctx tsd accountPubM secKeys = do
  when (isSigned tsd.txSignDataTx) $ Left "The transaction is already signed"
  when (null secKeys) $ Left "There are no private keys to sign"
  info <- parse tsd
  -- The sweep builder refuses a fee rate of zero; the signer checks the fee itself
  when (info.txInfoSummary.summaryFee == 0) $ Left "The transaction pays no fee"
  -- The coin of every input, in the order of the inputs
  coins <- map (coinSigInput net) <$> spentCoins net ctx tsd
  signedTx <- foldM (signCoin net ctx (indexKeys ctx secKeys)) tsd.txSignDataTx (zip [0 ..] coins)
  let signedData = tsd {txSignDataTx = signedTx}
  verifySignedTx net ctx signedData
  signedInfo <- parse signedData
  return (signedData, signedInfo)
  where
    parse = maybe (parseOutsideTxSignData net ctx) (uncurry (parseTxSignData net ctx)) accountPubM

-- | The private keys, indexed by what identifies them in an output script.
-- Every key is indexed with both encodings of its public key.
data Keys = Keys
  { -- | By the hash of the public key: P2PKH and P2WPKH coins
    byKeyHash :: !(Map Hash160 PrivateKey),
    -- | By the hash of the P2WPKH script of the key, with that script:
    -- P2SH-P2WPKH coins
    byWitnessScriptHash :: !(Map Hash160 (PrivateKey, ScriptOutput))
  }

indexKeys :: Ctx -> [SecKey] -> Keys
indexKeys ctx secKeys =
  Keys
    { byKeyHash = Map.fromList [(keyHash pub, key) | (key, pub) <- keys],
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
    PayWitnessPKHash h -> case Map.lookup h keys.byKeyHash of
      Just key
        | key.compress -> signInput net ctx tx i coin key
        -- The coin pays the hash of the uncompressed public key. A block
        -- may hold a spend of it, but nodes do not relay one, so the
        -- transaction would not reach a block. haskoin-core's signTx
        -- refuses it for the same reason.
        | otherwise -> Left $ "The coin of " <> describeInput <> " is a SegWit coin of an uncompressed public key: nodes do not relay a spend of it"
      Nothing -> noKey
    PayScriptHash h -> case Map.lookup h keys.byWitnessScriptHash of
      Just (key, witnessScript) ->
        signNestedInput net ctx tx i (SigInput witnessScript coin.value coin.outpoint coin.sighash Nothing) key
      Nothing -> noKey
    _ -> Left $ "Unsupported output script for " <> describeInput
  where
    withKey maybeKey sign = maybe noKey sign maybeKey
    noKey = Left $ "No private key for " <> describeInput
    describeInput =
      "input " <> show i <> " (" <> maybe "unknown address" (addrToString net) (outputAddress net ctx coin.script) <> ")"
