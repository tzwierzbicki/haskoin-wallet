{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Sweeping the coins of private keys from outside the wallet, into the
-- wallet or anywhere else.
--
-- A sweep spends no coin of the wallet. The mnemonic signs those, in a
-- payment, and the user holds no private key of an address of the wallet
-- to give signsweep: preparesweep refuses such an address before it
-- builds anything. So the wallet is in a sweep it builds by the
-- destinations alone.
--
-- 'fetchSweepCoins' asks the store for the coins to sweep: those at the
-- given addresses and at every address where a given key may hold them, and
-- the coins given by outpoint, so that a sweep can be prepared without the
-- keys at hand;
-- 'buildSweepSignData' builds the transaction spending them all to the
-- destinations, in the wallet or not, with a fee on the size 'sweepTxSize'
-- estimates from the kinds of coins, and 'signTxWithKeys' signs it with the
-- keys: P2PKH, P2WPKH and P2SH-P2WPKH coins, each with the key that
-- matches its output script. A coin paid to a public key itself (P2PK)
-- has no address, so the sweep refuses it, as the signer does. Analysis
-- and verification come from "Haskoin.Wallet.Tx.Signing".
--
-- The fee of a sweep prepared without the keys is set on the largest size
-- the signed sweep can have. The input of a P2PKH coin holds the public
-- key, 33 bytes compressed or 65 not, and nothing but the key tells which:
-- the address is a hash of either. Old paper wallets, which sweeps are
-- for, often have uncompressed keys. Counted for a compressed key, such a
-- coin paid less than the rate asked, and a node that asks for that rate
-- refused the sweep after the keys had signed it. Counted for an
-- uncompressed one, a coin of a compressed key pays 32 bytes too many, a
-- fifth more, and the sweep is always sent. The signer cannot set the fee
-- again once it has the keys: the user approved the amounts, and the
-- wallet holds the sweep by a hash of them.
--
-- A swept coin worth no more than the fee of its input is a dust coin
-- ("Haskoin.Wallet.Tx.Dust"): old wallets hold many, and each lowers what
-- the sweep pays. preparesweep asks whether to sweep them too
-- ('sweepDustReport'), and a sweep that leaves them ('sweepingOnly') names
-- them (the type t'UnsweptCoin').
--
-- A coin of Bitcoin Cash that holds tokens is never swept
-- ("Haskoin.Wallet.CashTokens"): hw could destroy its tokens.
-- 'fetchSweepCoins' sets such coins apart from the coins to sweep, and the
-- sweep names them too, so that the other coins of the keys are still
-- swept.
module Haskoin.Wallet.Tx.Sweep
  ( SweepCoins (..),
    UnsweptCoin (..),
    UnsweptReason (..),
    addressesWithoutCoin,
    buildSweepSignData,
    coinsOfTransactions,
    fetchSweepCoins,
    keyAddresses,
    signTxWithKeys,
    sizedSweepCoins,
    sweptAddresses,
    sweepDustReport,
    sweepTxSize,
    sweepingOnly,
    unsweptCoins,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (foldM, unless, when)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (find, nub, nubBy, partition, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Word (Word32, Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.CashTokens (holdsTokens)
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Coin (checkCoinsPaid, coinText)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Keys (AccountKind, PoolPath)
import Haskoin.Wallet.Tx.Dust (DustPlace (..), DustReport, SizedCoin (..), dustReport)
import Haskoin.Wallet.Tx.LockTime (inputSequence, txVersion)
import Haskoin.Wallet.Tx.Recipient (Share (..), shareAmounts)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (HeldPool, SpentCoin (..), checkOutputReach, coinSigInput, parseTxSignData, spentCoins, verifySignedTx)
import Haskoin.Wallet.Tx.Size (checkFeeRate, checkStandardSize, inputKind, outputDustLimit, recipientOutputKind, scriptOutputKind, txSize)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Random (StdGen)

{- Fetching from the store -}

-- | What a sweep needs from the network and the database
data SweepCoins = SweepCoins
  { -- | The coins of the keys to sweep
    coins :: ![Store.Unspent],
    -- | The coins found that hold tokens of Bitcoin Cash, which no sweep
    -- spends ("Haskoin.Wallet.CashTokens")
    withTokens :: ![Store.Unspent],
    -- | The transactions the coins come from
    transactions :: ![Tx],
    -- | The destinations that are addresses of the portfolio the sweep
    -- pays, with their paths in the portfolio
    destinationPaths :: !(Map Address PoolPath),
    -- | The P2PKH addresses of the compressed encodings of the keys given:
    -- their inputs carry a 33-byte public key. A P2PKH coin at any other
    -- address is counted with the 65 bytes of an uncompressed key, the
    -- address of a key that was not given too ('sweepTxSize').
    compressedAddrs :: ![Address]
  }
  deriving (Eq, Show)

-- | Ask the store for the coins to sweep: every coin at the given
-- addresses and at the addresses of the given keys, and the coins given by
-- outpoint, each once; and the transactions they come from. A coin that
-- holds tokens is set apart, one given by outpoint too, and its
-- transaction is not asked for. The database
-- says which of the destinations belong to the pool, when the sweep pays
-- one; a sweep outside the wallet has none. The keys may be
-- absent, when they are kept elsewhere: the sweep is then prepared from
-- addresses or coins alone.
fetchSweepCoins ::
  (MonadUnliftIO m) =>
  Network ->
  Store ->
  -- | The pool the sweep pays, if any
  Maybe Pool ->
  -- | The private keys, if at hand
  [SecKey] ->
  -- | The addresses to sweep from
  [Address] ->
  -- | The coins to sweep, by outpoint
  [OutPoint] ->
  -- | The destinations
  [Address] ->
  ExceptT String (DB m) SweepCoins
fetchSweepCoins net store poolM secKeys sweepFrom given sweepTo = do
  let addresses = nub $ sweepFrom <> concatMap (keyAddresses net) secKeys
  when (null addresses && null given) $
    throwError "No private keys, addresses or coins to sweep from"
  fromStore $ checkHealth store
  found <-
    if null addresses
      then return []
      else fromStore $ storeUnspents store addresses
  parents <-
    if null given
      then return []
      else fromStore $ storeTransactions store (nub $ (.hash) <$> given)
  givenCoins <- liftEither $ coinsOfTransactions given parents
  let (withTokens, coins) = partition (holdsTokens net . (.script)) $ nubBy ((==) `on` (.outpoint)) (found <> givenCoins)
      spentFrom = nub $ (.outpoint.hash) <$> coins
  -- The store lists the coins, so it must have every transaction they
  -- come from, and each must pay its coin as the store reports it: the
  -- fee is counted from the values of the coins
  transactions <- fromStore $ rawTransactions store spentFrom
  liftEither $ checkCoinsPaid transactions coins
  destinationPaths <- maybe (return Map.empty) (`poolPaths` sweepTo) poolM
  let compressedAddrs = nub $ compressedKeyAddr <$> secKeys
  return SweepCoins {coins, withTokens, transactions, destinationPaths, compressedAddrs}

-- | The addresses among those given that hold none of the coins found,
-- each once, in the order given: the addresses the user asked to sweep
-- from that the sweep leaves out. The addresses of a private key are not
-- among them. A key has an address of each kind, and most hold nothing.
addressesWithoutCoin :: [Address] -> SweepCoins -> [Address]
addressesWithoutCoin given found = filter (`notElem` holding) (nub given)
  where
    -- An address whose only coins hold tokens holds coins: the sweep
    -- names them as coins it leaves
    holding = mapMaybe (.address) (found.coins <> found.withTokens)

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
keyAddresses :: Network -> SecKey -> [Address]
keyAddresses net key =
  [compressedKeyAddr key, uncompressedKeyAddr key]
    <> [a | net.segWit, a <- segWitAddresses]
  where
    compressed = derivePublicKey (wrapSecKey True key)
    -- haskoin-core gives no SegWit address to an uncompressed key. The key
    -- here is compressed, so both addresses exist.
    segWitAddresses = catMaybes [pubKeyWitnessAddr compressed, pubKeyCompatWitnessAddr compressed]

-- | The P2PKH address of the compressed encoding of the key's public key
compressedKeyAddr :: SecKey -> Address
compressedKeyAddr key = pubKeyAddr $ derivePublicKey (wrapSecKey True key)

-- | The P2PKH address of the uncompressed encoding of the key's public key
uncompressedKeyAddr :: SecKey -> Address
uncompressedKeyAddr key = pubKeyAddr $ derivePublicKey (wrapSecKey False key)

{- Building -}

-- | The sweep transaction and its signing data: all the coins spent to the
-- destinations, both in random order, with the lock time given (see
-- "Haskoin.Wallet.Tx.LockTime"). The destinations with a fixed amount get
-- it, and what is left after the estimated fee is split evenly between
-- the others ('shareAmounts'). The signing data carries the transactions
-- the coins come from and the paths of the wallet's own addresses among
-- the destinations, which may be none: a sweep need not pay the wallet,
-- but only one that does can wait among its pending transactions. It
-- carries no path of an input: no coin of a sweep is the wallet's.
buildSweepSignData ::
  Network ->
  StdGen ->
  SweepCoins ->
  -- | The destinations, with what each gets
  [(Address, Share)] ->
  -- | Fee per virtual byte
  Natural ->
  -- | Lock time
  Word32 ->
  Either String TxSignData
buildSweepSignData net gen found sweepTo feeByte lockTime = do
  when (null found.coins && not (null found.withTokens)) $
    Left "The only coins in those addresses hold tokens (CashTokens). hw does not handle tokens and never spends such a coin: move them with a wallet that handles the tokens of Bitcoin Cash"
  when (null found.coins) $ Left "There are no coins to sweep in those addresses"
  when (null sweepTo) $ Left "No addresses to sweep to"
  checkFeeRate feeByte
  scripts <- mapM (scriptOf . fst) sweepTo
  size <- sweepTxSize net found scripts
  let total = sum $ (.value) <$> found.coins
      fee = fromIntegral feeByte * size
  amounts <- shareAmounts (fromIntegral total) (fromIntegral fee) (outputDustLimit . recipientOutputKind) sweepTo
  let (shuffledCoins, shuffledOutputs) =
        evalState ((,) <$> randomShuffle found.coins <*> randomShuffle (zip scripts (snd <$> amounts))) gen
      outputs = [TxOut (fromIntegral amount) (encodeSerial script) | (script, amount) <- shuffledOutputs]
      tx = Tx txVersion [TxIn c.outpoint BS.empty (inputSequence net) | c <- shuffledCoins] outputs [] lockTime
      outputPaths = nub $ mapMaybe ((`Map.lookup` found.destinationPaths) . fst) sweepTo
  return $ TxSignData tx found.transactions [] outputPaths (Just feeByte) Nothing [] noLabel
  where
    scriptOf addr = maybeToEither ("Can not pay to " <> addrToString net addr) (addressToOutput addr)

{- Size -}

-- | The estimated size of the sweep transaction paying the given output
-- scripts, in virtual bytes as "Haskoin.Wallet.Tx.Size" counts it: every
-- input by the kind of its coin, and by the encoding of its key for a
-- P2PKH coin, the longer one unless the key is known to be compressed,
-- every output by its script
--
-- A sweep of more coins than a transaction nodes relay can hold is
-- refused ('checkStandardSize'). Without the keys, each P2PKH coin counts
-- for its larger size there too, so a sweep that is accepted cannot come
-- out too large once signed.
sweepTxSize :: Network -> SweepCoins -> [ScriptOutput] -> Either String Word64
sweepTxSize net found outputs = do
  inputs <- map (.input) <$> sizedSweepCoins net found
  let outputKinds = scriptOutputKind . encodeSerial <$> outputs
  checkStandardSize inputs outputKinds
  return $ txSize inputs outputKinds

-- | The coins of a sweep, each with the input that spends it
-- (@sweptInputKind@), in their order: what each adds to the fee, which
-- tells the dust coins among them. A coin the sweep cannot sign is
-- refused.
sizedSweepCoins :: Network -> SweepCoins -> Either String [SizedCoin]
sizedSweepCoins net found =
  mapM (\coin -> SizedCoin coin <$> sweptInputKind net found.compressedAddrs coin) found.coins

-- | The kind of the signed input of a coin of the sweep. A P2PKH coin
-- carries the 33-byte public key at an address known to be of a
-- compressed key, those given. At any other address it is counted with
-- the 65-byte key: that of an uncompressed key given, or of a key that
-- was not given, which may be either. A coin of a kind the sweep cannot
-- sign is refused here, before signing.
sweptInputKind :: Network -> [Address] -> Store.Unspent -> Either String InputKind
sweptInputKind net compressedAddrs coin = do
  script <- either (const unsupported) Right $ decodeSerial coin.script
  case script of
    PayPKHash h | PubKeyAddress h `notElem` compressedAddrs -> Right InputUncompressedPKHash
    _ -> maybe unsupported Right (inputKind script)
  where
    unsupported =
      Left $ "Unsupported output script for the coin at " <> maybe "unknown address" (addrToString net) coin.address

{- Dust coins -}

-- | The report on the dust coins of a sweep, for the question preparesweep
-- asks, from the coins worth sweeping and the dust coins: see
-- 'dustReport'. The dust coins that are left could be swept later to an
-- address of the type of the first destination without an amount, the one
-- that gets what a sweep leaves.
sweepDustReport :: Natural -> [(Address, Share)] -> [SizedCoin] -> [SizedCoin] -> DustReport
sweepDustReport feeByte sweepTo =
  dustReport AtSweptAddresses feeByte sweepTo laterOutput
  where
    -- A sweep has a destination without an amount: an unreachable default
    laterOutput = maybe OutputPKHash recipientOutputKind (listToMaybe [addr | (addr, Remainder) <- sweepTo])

-- | The sweep of some of its coins only, with the transactions they come
-- from and no other: the signer reads the transaction of every coin it
-- spends, and a file must not carry those of the coins that stay.
sweepingOnly :: [Store.Unspent] -> SweepCoins -> SweepCoins
sweepingOnly kept found =
  found {coins = kept, transactions = filter (\tx -> txHash tx `elem` parents) found.transactions}
  where
    parents = (.outpoint.hash) <$> kept

-- | A coin that a sweep leaves at its address, as preparesweep names it:
-- by its outpoint, since the wallet does not hold the coin, with its
-- address, its value in satoshis, and why the sweep leaves it
data UnsweptCoin = UnsweptCoin
  { outpoint :: !OutPoint,
    address :: !Address,
    value :: !Natural,
    reason :: !UnsweptReason
  }
  deriving (Eq, Show)

-- | Why a sweep leaves a coin of the keys or the addresses it was given
data UnsweptReason
  = -- | A dust coin, which the user chose to leave
    -- ("Haskoin.Wallet.Tx.Dust")
    WorthLessThanItsFee
  | -- | A coin of Bitcoin Cash that holds tokens, which hw never spends
    -- ("Haskoin.Wallet.CashTokens")
    HoldsTokens
  deriving (Eq, Show)

-- | The coins a sweep leaves for a reason, the largest first, as the
-- sweep lists them. A coin without an address is not among the coins
-- found (@sweptInputKind@, and the store gives a coin with tokens its
-- address).
unsweptCoins :: UnsweptReason -> [Store.Unspent] -> [UnsweptCoin]
unsweptCoins reason left =
  sortOn
    (Down . (.value))
    [UnsweptCoin coin.outpoint addr (fromIntegral coin.value) reason | coin <- left, Just addr <- [coin.address]]

{- Signing -}

-- | The addresses of the coins a sweep spends, one for each coin, in the
-- order of its inputs: signsweep asks for private keys until each has its
-- key ('Haskoin.Wallet.Cli.Prompt.keysOfCoins').
sweptAddresses :: Network -> TxSignData -> Either String [Address]
sweptAddresses net tsd = map (.address) <$> spentCoins net tsd

-- | Sign a transaction with the given private keys: every input is signed
-- with the key that matches the output script of its coin. The accounts
-- of the pool the sweep pays, each by its kind and its public key, only
-- identify the portfolio's own inputs and outputs, the paths of the
-- t'TxSignData' being of them, with the addresses the wallet holds of
-- them, which bound its output paths ('HeldPool'). A sweep outside the
-- wallet has no account.
signTxWithKeys ::
  Network ->
  TxSignData ->
  [(AccountKind, XPubKey)] ->
  HeldPool ->
  [SecKey] ->
  Either String (TxSignData, TxInfo)
signTxWithKeys net tsd accounts held secKeys = do
  when (isSigned tsd.txSignDataTx) $ Left "The transaction is already signed"
  when (null secKeys) $ Left "There are no private keys to sign"
  info <- parse tsd
  unless (null accounts) $ checkOutputReach held tsd
  -- The sweep builder refuses a fee rate of zero; the signer checks the fee itself
  when (info.txInfoSummary.summaryFee == 0) $ Left "The transaction pays no fee"
  -- The coin of every input, in the order of the inputs
  coins <- map (coinSigInput net) <$> spentCoins net tsd
  signedTx <- foldM (signCoin net (indexKeys secKeys)) tsd.txSignDataTx (zip [0 ..] coins)
  let signedData = tsd {txSignDataTx = signedTx}
  verifySignedTx net signedData
  signedInfo <- parse signedData
  return (signedData, signedInfo)
  where
    parse = parseTxSignData net accounts

-- | The private keys, indexed by what identifies them in an output script.
-- Every key is indexed with both encodings of its public key.
data Keys = Keys
  { -- | By the hash of the public key: P2PKH and P2WPKH coins
    byKeyHash :: !(Map Hash160 PrivateKey),
    -- | By the hash of the P2WPKH script of the key, with that script:
    -- P2SH-P2WPKH coins
    byWitnessScriptHash :: !(Map Hash160 (PrivateKey, ScriptOutput))
  }

indexKeys :: [SecKey] -> Keys
indexKeys secKeys =
  Keys
    { byKeyHash = Map.fromList [(keyHash pub, key) | (key, pub) <- keys],
      byWitnessScriptHash =
        Map.fromList
          [ (addressHash (encodeSerial witnessScript), (key, witnessScript))
            | (key, pub) <- keys,
              pub.compress,
              let witnessScript = PayWitnessPKHash (keyHash pub)
          ]
    }
  where
    keys =
      [ (key, derivePublicKey key)
        | sec <- secKeys,
          compressed <- [True, False],
          let key = wrapSecKey compressed sec
      ]
    keyHash :: PublicKey -> Hash160
    keyHash pub = addressHash (encodeSerial pub)

-- | Sign input i of the transaction with the key of its coin. A P2SH-P2WPKH
-- coin is signed as a nested witness: the scriptSig pushes the P2WPKH script.
signCoin :: Network -> Keys -> Tx -> (Int, SigInput) -> Either String Tx
signCoin net keys tx (i, coin) =
  case coin.script of
    PayPKHash h -> withKey (Map.lookup h keys.byKeyHash) $ signInput net tx i coin
    PayWitnessPKHash h -> case Map.lookup h keys.byKeyHash of
      Just key
        | key.compress -> signInput net tx i coin key
        -- The coin pays the hash of the uncompressed public key. A block
        -- may hold a spend of it, but nodes do not relay one, so the
        -- transaction would not reach a block. haskoin-core's signTx
        -- refuses it for the same reason.
        | otherwise -> Left $ "The coin of " <> describeInput <> " is a SegWit coin of an uncompressed public key: nodes do not relay a spend of it"
      Nothing -> noKey
    PayScriptHash h -> case Map.lookup h keys.byWitnessScriptHash of
      Just (key, witnessScript) ->
        signNestedInput net tx i (SigInput witnessScript coin.value coin.outpoint coin.sighash Nothing) key
      Nothing -> noKey
    _ -> Left $ "Unsupported output script for " <> describeInput
  where
    withKey maybeKey sign = maybe noKey sign maybeKey
    noKey = Left $ "No private key for " <> describeInput
    describeInput =
      "input " <> show i <> " (" <> maybe "unknown address" (addrToString net) (outputAddress net coin.script) <> ")"
