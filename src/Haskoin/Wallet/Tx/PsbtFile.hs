{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A transaction file as a PSBT (BIP 174), which preparetx, preparesweep,
-- bumpfee, exporttx, signtx and signsweep write, and reviewtx, importtx,
-- signtx, signsweep and sendtx read. The fields of the spec carry the
-- transaction, the whole transaction every coin comes from, the keys of
-- the account in the inputs and outputs with their derivation paths, the
-- sighash to sign with, and the signatures, as the final scripts of the
-- inputs. What is hw's own goes in proprietary fields under the identifier
-- "hw": the network, the fee rate the transaction was built for, the
-- transactions it replaces and its label in the global map, the label of
-- an address on its input or output, and the label a coin shows on its
-- input.
--
-- A derivation record names a key by the fingerprint of the seed and the
-- path from it, m/44'/0'/0'/0/5, when the wallet knows the seed, as the
-- spec wants it and other signers expect it. Otherwise it names the key by
-- the fingerprint of the account's own key and the path from it, /0/5, as
-- Bitcoin Core does for a key whose origin it does not know. The signer
-- knows the seed, so a signed file names it, and the wallet that imports
-- the file learns its fingerprint. A PSBT does not say which account of
-- the wallet it is for: the reader tries the accounts given, by the keys
-- the records name, and gives the paths of the first one with a record,
-- relative to it, as the signing data wants them. A signed input keeps its
-- derivation records, which the spec lets a finalizer clear, so that the
-- signed file still says which inputs are the account's.
module Haskoin.Wallet.Tx.PsbtFile
  ( FileAccount (..),
    txFilePsbt,
    psbtTxFile,

    -- * The fields of hw
    hwIdentifier,
    networkField,
    feeRateField,
    replacesField,
    labelField,
    addressLabelField,
    coinLabelField,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Bip174
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (walletSigHash)
import Numeric.Natural (Natural)

-- | The keys of an account, as the derivation records of its files name
-- them
data FileAccount = FileAccount
  { fileAccountXPub :: !XPubKey,
    -- | The path of the account from the seed, m/44'/coin'/index'
    fileAccountPath :: !HardPath,
    -- | The fingerprint of the seed, when the wallet has learnt it
    fileAccountFingerprint :: !(Maybe Fingerprint)
  }
  deriving (Eq, Show)

{- The fields of hw -}

-- | The identifier of hw's proprietary fields
hwIdentifier :: ByteString
hwIdentifier = "hw"

-- | The subtypes of hw's fields in the global map: the name of the network,
-- the fee rate in satoshis per byte as a 64-bit integer, little-endian, the
-- ids of the transactions replaced, 32 bytes each, and the label of the
-- transaction, in UTF-8
networkField, feeRateField, replacesField, labelField :: Word64
networkField = 0
feeRateField = 1
replacesField = 2
labelField = 3

-- | The subtypes of hw's fields in the map of an input or of an output: the
-- label of the address of the account there, and, on an input, the label
-- the coin shows, both in UTF-8
addressLabelField, coinLabelField :: Word64
addressLabelField = 0
coinLabelField = 1

{- Writing -}

-- | An address of the account at a path, as the file names it: its key,
-- the script that pays it, and the derivation of the key
data AddressKey = AddressKey
  { addressPath :: !SoftPath,
    addressPublic :: !PublicKey,
    addressScript :: !ByteString,
    addressDerivation :: !Derivation
  }

-- | The address of the account at a path, with its key derived from the
-- seed when its fingerprint is known, from the account's key otherwise
addressKey :: Ctx -> FileAccount -> Maybe Fingerprint -> SoftPath -> AddressKey
addressKey ctx acc fingerprintM path =
  AddressKey
    { addressPath = path,
      addressPublic = public,
      addressScript = marshal ctx (PayPKHash (addressHash (marshal ctx public))),
      addressDerivation = case fingerprintM of
        Just fingerprint -> Derivation fingerprint (listToPath (pathToList acc.fileAccountPath <> pathToList path))
        Nothing -> Derivation (xPubFP ctx acc.fileAccountXPub) (toGeneric path)
    }
  where
    public = wrapPubKey True (derivePubPath ctx path acc.fileAccountXPub).key

-- | The PSBT of a transaction file, with the keys of the account given in
-- its derivation records: the account the paths of the file are relative
-- to. A file of a transaction outside the wallet, which has no path, needs
-- none. The file must name its network, as every file hw writes does.
txFilePsbt :: Ctx -> Maybe FileAccount -> TxFile -> Either String Psbt
txFilePsbt ctx accountM file = do
  net <- maybe (Left "The file names no network") Right file.txFileNetwork
  let tsd = file.txFileData
      tx = tsd.txSignDataTx
      parents = Map.fromList [(txHash parent, parent) | parent <- tsd.txSignDataInputs]
  coins <- forM (zip [0 :: Int ..] tx.inputs) $ \(i, input) -> do
    parent <-
      maybe (Left ("The transaction input " <> show i <> " spends from is missing")) Right $
        Map.lookup input.outpoint.hash parents
    coin <-
      maybe (Left ("Input " <> show i <> " spends an output its transaction does not have")) Right $
        listToMaybe (drop (fromIntegral input.outpoint.index) parent.outputs)
    return (parent, coin)
  let fingerprintM = tsd.txSignDataFingerprint <|> (fileAccountFingerprint =<< accountM)
  (inputKeys, outputKeys) <- case accountM of
    Just acc -> do
      let keysOf = map (addressKey ctx acc fingerprintM)
          inputKeys = keysOf tsd.txSignDataInputPaths
          outputKeys = keysOf tsd.txSignDataOutputPaths
      forM_ inputKeys $ \key ->
        unless (any ((== key.addressScript) . (.script) . snd) coins) $
          Left ("The path " <> pathToStr key.addressPath <> " is of no input of the transaction")
      forM_ outputKeys $ \key ->
        unless (any ((== key.addressScript) . (.script)) tx.outputs) $
          Left ("The path " <> pathToStr key.addressPath <> " is of no output of the transaction")
      return (inputKeys, outputKeys)
    Nothing
      | null tsd.txSignDataInputPaths && null tsd.txSignDataOutputPaths -> return ([], [])
      | otherwise -> Left "The file has paths of an account, and no account to derive them from"
  feeRateM <- forM tsd.txSignDataFeeRate $ \rate -> do
    when (rate > fromIntegral (maxBound :: Word64)) $ Left "The fee rate is beyond what the file can hold"
    return (S.runPut (S.putWord64le (fromIntegral rate)))
  let stacks = tx.witness <> repeat []
      inputs = zipWith3 (inputMap ctx net file inputKeys) tx.inputs stacks coins
      outputs = outputMap ctx file outputKeys <$> tx.outputs
      globals =
        foldr ($) Map.empty $
          [insertProprietary hwIdentifier networkField (cs net.name)]
            <> [insertProprietary hwIdentifier feeRateField bytes | Just bytes <- [feeRateM]]
            <> [insertProprietary hwIdentifier replacesField (BS.concat (S.encode <$> tsd.txSignDataReplaces)) | not (null tsd.txSignDataReplaces)]
            <> [insertProprietary hwIdentifier labelField (encodeUtf8 tsd.txSignDataLabel) | not (Text.null tsd.txSignDataLabel)]
            <> [ insertXpubRecord net ctx acc.fileAccountXPub (Derivation fingerprint (toGeneric acc.fileAccountPath))
                 | Just acc <- [accountM],
                   Just fingerprint <- [fingerprintM]
               ]
  return (psbtOf tx) {psbtGlobals = globals, psbtInputs = inputs, psbtOutputs = outputs}

-- | The map of an input: the transaction its coin comes from, the sighash
-- to sign it with while it is unsigned, its final script and witness once
-- signed, the derivation and label of the address of the account it spends
-- from, if any, and the label its coin shows, if any
inputMap :: Ctx -> Network -> TxFile -> [AddressKey] -> TxIn -> WitnessStack -> (Tx, TxOut) -> PsbtMap
inputMap ctx net file keys input stack (parent, coin) =
  foldr ($) (insertNonWitnessUtxo parent Map.empty) $
    [insertSigHashType (walletSigHash net) | unsigned]
      <> [insertFinalScriptSig input.script | not (BS.null input.script)]
      <> [insertFinalScriptWitness stack | not (null stack)]
      <> concat [addressFields (insertInputDerivation ctx) file key | key <- keys, key.addressScript == coin.script]
      <> [insertProprietary hwIdentifier coinLabelField (encodeUtf8 label) | Just label <- [Map.lookup input.outpoint.hash file.txFileInputLabels]]
  where
    unsigned = BS.null input.script && null stack

-- | The map of an output: the derivation and label of the address of the
-- account it pays, if any
outputMap :: Ctx -> TxFile -> [AddressKey] -> TxOut -> PsbtMap
outputMap ctx file keys output =
  foldr ($) Map.empty $
    concat [addressFields (insertOutputDerivation ctx) file key | key <- keys, key.addressScript == output.script]

-- | The fields of an address of the account in an input or an output: the
-- derivation of its key, and its label when it has one
addressFields :: (PublicKey -> Derivation -> PsbtMap -> PsbtMap) -> TxFile -> AddressKey -> [PsbtMap -> PsbtMap]
addressFields insertDerivation file key =
  insertDerivation key.addressPublic key.addressDerivation
    : [insertProprietary hwIdentifier addressLabelField (encodeUtf8 label) | Just label <- [Map.lookup key.addressPath file.txFilePathLabels]]

{- Reading -}

-- | What the derivation records of a PSBT say of an account: the paths of
-- its addresses among the inputs and among the outputs, each once, in
-- order; the fingerprint of its seed when the records carry it; and the
-- labels of those addresses
data AccountMatch = AccountMatch
  { matchInputPaths :: ![SoftPath],
    matchOutputPaths :: ![SoftPath],
    matchFingerprint :: !(Maybe Fingerprint),
    matchLabels :: !(Map SoftPath Text)
  }

-- | The transaction file of a PSBT, as the signing data of hw holds it,
-- with its paths relative to the account among those given that the
-- derivation records name: the first with a record on an input, or else
-- on an output. A PSBT that names none of them is read without paths, as a
-- transaction outside the wallet. Every input must carry the whole
-- transaction its coin comes from, and a signed input must be finalized.
psbtTxFile :: Ctx -> [FileAccount] -> Psbt -> Either String TxFile
psbtTxFile ctx accounts psbt = do
  tx <- psbtTransaction psbt
  let stacks = tx.witness <> repeat []
  parents <- forM (zip3 [0 :: Int ..] tx.inputs (zip stacks psbt.psbtInputs)) $ \(i, input, (stack, m)) -> do
    let place = "Input " <> show i
    when (hasPartialSignatures m && BS.null input.script && null stack) $
      Left (place <> " carries partial signatures and is not finalized: finalize it with the software that signed it")
    parentM <- nonWitnessUtxo m
    parent <- maybe (Left (place <> " has no previous transaction: hw needs the whole transaction each coin comes from")) Right parentM
    unless (txHash parent == input.outpoint.hash) $
      Left ("The previous transaction of input " <> show i <> " is not the one it spends")
    return parent
  netM <- traverse networkOf (lookupProprietary hwIdentifier networkField psbt.psbtGlobals)
  feeRateM <- traverse feeRateOf (lookupProprietary hwIdentifier feeRateField psbt.psbtGlobals)
  replaces <- maybe (Right []) replacesOf (lookupProprietary hwIdentifier replacesField psbt.psbtGlobals)
  label <- maybe (Right noLabel) (textOf "The label of the transaction") (lookupProprietary hwIdentifier labelField psbt.psbtGlobals)
  matches <- mapM (accountMatch ctx netM psbt) accounts
  let chosen =
        listToMaybe [m | m <- matches, not (null m.matchInputPaths)]
          <|> listToMaybe [m | m <- matches, not (null m.matchOutputPaths)]
  coinLabels <- forM (zip tx.inputs psbt.psbtInputs) $ \(input, m) ->
    fmap (\text -> (input.outpoint.hash, text)) <$> traverse (textOf "The label of a coin") (lookupProprietary hwIdentifier coinLabelField m)
  let tsd =
        TxSignData
          { txSignDataTx = tx,
            txSignDataInputs = nubOrdOn txHash parents,
            txSignDataInputPaths = maybe [] (.matchInputPaths) chosen,
            txSignDataOutputPaths = maybe [] (.matchOutputPaths) chosen,
            txSignDataFeeRate = feeRateM,
            txSignDataFingerprint = (.matchFingerprint) =<< chosen,
            txSignDataReplaces = replaces,
            txSignDataLabel = label
          }
  return
    TxFile
      { txFileNetwork = netM,
        txFileData = tsd,
        txFilePathLabels = maybe Map.empty (.matchLabels) chosen,
        txFileInputLabels = firstOfEach (mapMaybe id coinLabels)
      }

-- | What the records of the PSBT say of the account
accountMatch :: Ctx -> Maybe Network -> Psbt -> FileAccount -> Either String AccountMatch
accountMatch ctx netM psbt acc = do
  inputs <- forM (zip [0 :: Int ..] psbt.psbtInputs) $ \(i, m) ->
    addressesOf ("Input " <> show i) m =<< inputDerivations ctx m
  outputs <- forM (zip [0 :: Int ..] psbt.psbtOutputs) $ \(i, m) ->
    addressesOf ("Output " <> show i) m =<< outputDerivations ctx m
  -- The record of the account's own key, if the global map has one, says
  -- the fingerprint of the seed too
  xpubs <- maybe (return []) (\net -> xpubRecords net ctx psbt.psbtGlobals) netM
  let ofAccount = [d.derivationFingerprint | (xpub, d) <- xpubs, xpub == acc.fileAccountXPub, d.derivationPath == toGeneric acc.fileAccountPath]
      found = concat (inputs <> outputs)
  return
    AccountMatch
      { matchInputPaths = nubOrd [path | (path, _, _) <- concat inputs],
        matchOutputPaths = nubOrd [path | (path, _, _) <- concat outputs],
        matchFingerprint = listToMaybe ([fingerprint | (_, Just fingerprint, _) <- found] <> ofAccount),
        matchLabels = firstOfEach [(path, label) | (path, _, Just label) <- found]
      }
  where
    -- The addresses of the account among the keys of a map, with the
    -- fingerprint of the seed when their records carry it, and the label
    -- of the address when the map has one
    addressesOf place m records = do
      labelM <- traverse (textOf "The label of an address") (lookupProprietary hwIdentifier addressLabelField m)
      found <- forM records $ \record -> fmap (\(path, fingerprintM) -> (path, fingerprintM, labelM)) <$> matchRecord ctx acc place record
      return (mapMaybe id found)

-- | What a derivation record says of the account: the path of one of its
-- addresses, with the fingerprint of the seed when the record derives the
-- key from the seed; nothing for a key of another wallet; a refusal for a
-- record that names a key of the account but carries another public key
matchRecord :: Ctx -> FileAccount -> String -> (PublicKey, Derivation) -> Either String (Maybe (SoftPath, Maybe Fingerprint))
matchRecord ctx acc place (public, derivation)
  | Just rest <- stripPrefix (pathToList acc.fileAccountPath) indices, not (null rest) = resolve rest (Just fingerprint)
  | fingerprint == xPubFP ctx acc.fileAccountXPub, not (null indices) = resolve indices Nothing
  | otherwise = Right Nothing
  where
    fingerprint = derivation.derivationFingerprint
    indices = pathToList derivation.derivationPath
    -- Whether the record's fingerprint is the account's: the fingerprint of
    -- its seed, when known, or of its own key
    namesAccount fromSeed = case fromSeed of
      Just f -> Just f == acc.fileAccountFingerprint
      Nothing -> True
    resolve rest fromSeed = case toSoft (listToPath rest) of
      -- A hardened step below the account derives no address of it
      Nothing -> Right Nothing
      Just path
        | (addressKey ctx acc Nothing path).addressPublic == public -> Right (Just (path, fromSeed))
        | namesAccount fromSeed -> Left (place <> " names the key " <> pathToStr path <> " of the account, but carries another public key")
        | otherwise -> Right Nothing

-- | A map of the pairs, the first value of a key winning
firstOfEach :: (Ord k) => [(k, v)] -> Map k v
firstOfEach = Map.fromListWith (\_ earlier -> earlier)

networkOf :: ByteString -> Either String Network
networkOf bytes = do
  name <- textOf "The network" bytes
  maybe (Left ("Unknown network " <> cs name)) Right (netByName (cs name))

feeRateOf :: ByteString -> Either String Natural
feeRateOf bytes
  | BS.length bytes == 8 = first (const problem) (fromIntegral <$> S.runGet S.getWord64le bytes)
  | otherwise = Left problem
  where
    problem = "The fee rate of the file is not a 64-bit integer"

replacesOf :: ByteString -> Either String [TxHash]
replacesOf bytes
  | BS.length bytes `mod` 32 == 0 = first (const problem) (mapM S.decode (chunks bytes))
  | otherwise = Left problem
  where
    problem = "The transactions replaced are not a list of transaction ids"
    chunks b
      | BS.null b = []
      | otherwise = let (chunk, rest) = BS.splitAt 32 b in chunk : chunks rest

textOf :: String -> ByteString -> Either String Text
textOf what = first (const (what <> " is not UTF-8 text")) . decodeUtf8'
