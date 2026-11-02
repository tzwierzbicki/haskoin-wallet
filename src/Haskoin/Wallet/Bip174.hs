{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The format of BIP 174 alone: a partially signed Bitcoin transaction, a
-- PSBT, as a file holds it. The file is five magic bytes, then maps of keys
-- and values: one global map, which holds the unsigned transaction, then
-- one map per input and one per output of that transaction. A key is a
-- type and some data. The spec gives the keys and values of the fields it
-- defines, and leaves the type 0xFC to proprietary fields, under an
-- identifier of the software that writes them.
--
-- The maps are read and written byte for byte, the keys of a map in order,
-- and every key is kept, known or not. The fields hw has a use for are
-- offered here, read and written as the spec shapes them. A PSBT that
-- breaks the spec is refused when it is read: a key out of shape, a key
-- twice in a map, a value that does not decode, bytes after the last map.
-- "Haskoin.Wallet.Tx.PsbtFile" turns a transaction file of hw into a PSBT
-- and back.
module Haskoin.Wallet.Bip174
  ( -- * A PSBT
    Psbt (..),
    PsbtKey (..),
    PsbtMap,
    psbtOf,
    psbtTransaction,

    -- * Bytes and text
    psbtMagic,
    isPsbt,
    encodePsbt,
    decodePsbt,
    encodePsbtBase64,
    decodePsbtBase64,

    -- * The fields of the spec
    Derivation (..),
    xpubRecords,
    insertXpubRecord,
    nonWitnessUtxo,
    insertNonWitnessUtxo,
    witnessUtxo,
    hasPartialSignatures,
    sigHashType,
    insertSigHashType,
    inputDerivations,
    insertInputDerivation,
    finalScriptSig,
    insertFinalScriptSig,
    finalScriptWitness,
    insertFinalScriptWitness,
    outputDerivations,
    insertOutputDerivation,

    -- * Proprietary fields
    Proprietary (..),
    proprietaryKey,
    proprietaryOf,
    lookupProprietary,
    insertProprietary,
  )
where

import Control.Monad (ap, forM, forM_, replicateM, unless, when)
import Data.Bifunctor (first)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (toUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Serialize as S
import Data.Word (Word32, Word64, Word8)
import Haskoin
import Numeric (showHex)

{- A PSBT -}

-- | A key of a map: its type, and the data after the type byte. Most
-- fields have none; a derivation or a signature has the public key it is
-- of.
data PsbtKey = PsbtKey
  { psbtKeyType :: !Word8,
    psbtKeyData :: !ByteString
  }
  deriving (Eq, Ord, Show)

-- | A map of a PSBT. Its keys are written in the order of the map, the
-- type first, as Bitcoin Core writes them.
type PsbtMap = Map PsbtKey ByteString

-- | A PSBT: the unsigned transaction, the other fields of the global map,
-- and the maps of the inputs and of the outputs, one each, in the order of
-- the transaction
data Psbt = Psbt
  { psbtTx :: !Tx,
    psbtGlobals :: !PsbtMap,
    psbtInputs :: ![PsbtMap],
    psbtOutputs :: ![PsbtMap]
  }
  deriving (Eq, Show)

-- | The PSBT of a transaction, with nothing else: the transaction without
-- its input scripts and witness, as the spec wants it, and an empty map per
-- input and per output
psbtOf :: Tx -> Psbt
psbtOf tx =
  Psbt
    { psbtTx = Tx tx.version [TxIn input.outpoint BS.empty input.sequence | input <- tx.inputs] tx.outputs [] tx.locktime,
      psbtGlobals = Map.empty,
      psbtInputs = Map.empty <$ tx.inputs,
      psbtOutputs = Map.empty <$ tx.outputs
    }

-- | The transaction of a PSBT with the final script and witness of every
-- input that has them, as the transaction extractor of the spec builds
-- it. An input without them stays unsigned.
psbtTransaction :: Psbt -> Either String Tx
psbtTransaction psbt = do
  finals <- forM (zip psbt.psbtTx.inputs psbt.psbtInputs) $ \(input, m) -> do
    stack <- fromMaybe [] <$> finalScriptWitness m
    let scriptSig = fromMaybe BS.empty (finalScriptSig m)
    return (TxIn input.outpoint scriptSig input.sequence, stack)
  let stacks = snd <$> finals
      witness = if all null stacks then [] else stacks
  return $ Tx psbt.psbtTx.version (fst <$> finals) psbt.psbtTx.outputs witness psbt.psbtTx.locktime

{- The types of the spec -}

unsignedTxType, xpubType, versionType :: Word8
unsignedTxType = 0x00
xpubType = 0x01
versionType = 0xfb

nonWitnessUtxoType, witnessUtxoType, partialSigType, sigHashTypeType, inRedeemScriptType, inWitnessScriptType, inDerivationType, finalScriptSigType, finalScriptWitnessType :: Word8
nonWitnessUtxoType = 0x00
witnessUtxoType = 0x01
partialSigType = 0x02
sigHashTypeType = 0x03
inRedeemScriptType = 0x04
inWitnessScriptType = 0x05
inDerivationType = 0x06
finalScriptSigType = 0x07
finalScriptWitnessType = 0x08

outRedeemScriptType, outWitnessScriptType, outDerivationType :: Word8
outRedeemScriptType = 0x00
outWitnessScriptType = 0x01
outDerivationType = 0x02

proprietaryType :: Word8
proprietaryType = 0xfc

-- | The key of the unsigned transaction in the global map
unsignedTxKey :: PsbtKey
unsignedTxKey = PsbtKey unsignedTxType BS.empty

{- Bytes -}

-- | The five bytes every PSBT starts with: "psbt" and 0xff
psbtMagic :: ByteString
psbtMagic = BS.pack [0x70, 0x73, 0x62, 0x74, 0xff]

-- | Whether the bytes are those of a PSBT, by their first five
isPsbt :: ByteString -> Bool
isPsbt = BS.isPrefixOf psbtMagic

-- | The bytes of a PSBT: the magic, the global map with the unsigned
-- transaction first, then the maps of the inputs and of the outputs
encodePsbt :: Psbt -> ByteString
encodePsbt psbt =
  BS.concat $ psbtMagic : encodeMap globals : (encodeMap <$> psbt.psbtInputs) <> (encodeMap <$> psbt.psbtOutputs)
  where
    globals = Map.insert unsignedTxKey (S.encode psbt.psbtTx) psbt.psbtGlobals

-- | A map: each key and value after its length, the keys in order, then a
-- zero byte
encodeMap :: PsbtMap -> ByteString
encodeMap m =
  BS.concat [sized (BS.cons key.psbtKeyType key.psbtKeyData) <> sized value | (key, value) <- Map.toAscList m]
    <> BS.singleton 0

-- | Bytes after their length, as a compact size integer
sized :: ByteString -> ByteString
sized bytes = compactSize (fromIntegral (BS.length bytes)) <> bytes

-- | A compact size integer, in its shortest form
compactSize :: Word64 -> ByteString
compactSize = S.encode . VarInt

-- | A 32-bit integer, little-endian
word32le :: Word32 -> ByteString
word32le = S.runPut . S.putWord32le

-- | The PSBT the bytes hold, or why they hold none: not a PSBT, or one that
-- breaks the spec
decodePsbt :: ByteString -> Either String Psbt
decodePsbt = parseAll psbtParser "the last map"

-- | The text of a PSBT, in base64, as the spec gives it for text
encodePsbtBase64 :: Psbt -> ByteString
encodePsbtBase64 = Base64.encode . encodePsbt

-- | The PSBT of a base64 text, around which spaces and line breaks are
-- left aside
decodePsbtBase64 :: ByteString -> Either String Psbt
decodePsbtBase64 text = do
  bytes <- first (const "Not the base64 text of a PSBT") $ Base64.decode (BS.filter (`notElem` spaces) text)
  decodePsbt bytes
  where
    spaces = [0x20, 0x09, 0x0a, 0x0d]

{- Reading bytes -}

-- | A reader of the bytes of a file, which stops at the first byte out of
-- place with a sentence saying so
newtype BytesParser a = BytesParser (ByteString -> Either String (a, ByteString))

instance Functor BytesParser where
  fmap f (BytesParser p) = BytesParser $ \bytes -> do
    (a, rest) <- p bytes
    return (f a, rest)

instance Applicative BytesParser where
  pure a = BytesParser $ \bytes -> Right (a, bytes)
  (<*>) = ap

instance Monad BytesParser where
  BytesParser p >>= f = BytesParser $ \bytes -> do
    (a, rest) <- p bytes
    let BytesParser q = f a
    q rest

refuse :: String -> BytesParser a
refuse reason = BytesParser $ \_ -> Left reason

-- | Read all the bytes with the parser: nothing may follow what it reads
parseAll :: BytesParser a -> String -> ByteString -> Either String a
parseAll (BytesParser p) what bytes = do
  (a, rest) <- p bytes
  unless (BS.null rest) $ Left ("Bytes follow " <> what)
  return a

-- | The next bytes, which must be there. What they are part of names
-- them in the refusal.
bytesOf :: Int -> String -> BytesParser ByteString
bytesOf n what = BytesParser $ \bytes ->
  if BS.length bytes < n
    then Left ("The file ends inside " <> what)
    else Right (BS.splitAt n bytes)

-- | All the bytes left
restOf :: BytesParser ByteString
restOf = BytesParser $ \bytes -> Right (bytes, BS.empty)

-- | A compact size integer: one byte below 0xfd, or 0xfd, 0xfe or 0xff and
-- two, four or eight bytes, little-endian, in the shortest form, as
-- Bitcoin Core reads it
compactSizeOf :: String -> BytesParser Word64
compactSizeOf what = do
  first' <- BS.head <$> bytesOf 1 what
  case first' of
    0xfd -> longer 2 0xfd
    0xfe -> longer 4 0x10000
    0xff -> longer 8 0x100000000
    _ -> return (fromIntegral first')
  where
    longer :: Int -> Word64 -> BytesParser Word64
    longer n least = do
      value <- BS.foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) 0 <$> bytesOf n what
      when (value < least) $ refuse ("A length in " <> what <> " is not in its shortest form")
      return value

-- | A length, which must fit a file
lengthOf :: String -> BytesParser Int
lengthOf what = do
  n <- compactSizeOf what
  when (n > fromIntegral (maxBound :: Int)) $ refuse ("A length in " <> what <> " is beyond any file")
  return (fromIntegral n)

-- | Bytes after their length
sizedBytes :: String -> BytesParser ByteString
sizedBytes what = lengthOf what >>= (`bytesOf` what)

-- | A map: keys and values up to a zero byte, each key once
mapOf :: String -> BytesParser PsbtMap
mapOf what = go Map.empty
  where
    go m = do
      keyLength <- lengthOf what
      if keyLength == 0
        then return m
        else do
          keyBytes <- bytesOf keyLength what
          value <- sizedBytes what
          let key = PsbtKey (BS.head keyBytes) (BS.tail keyBytes)
          when (Map.member key m) $
            refuse (capital what <> " has the key " <> showKey key <> " twice")
          go (Map.insert key value m)

psbtParser :: BytesParser Psbt
psbtParser = do
  magic <- bytesOf 5 "the magic bytes"
  unless (magic == psbtMagic) $ refuse "Not a PSBT: the file does not start with the magic bytes of BIP 174"
  globals <- mapOf "the global map"
  tx <- either refuse return (unsignedTxOf globals)
  let others = Map.delete unsignedTxKey globals
  either refuse return (checkMap "the global map" globalFields others)
  inputs <- forM [0 .. length tx.inputs - 1] $ \i -> do
    let what = "input " <> show i
    m <- mapOf what
    either refuse return (checkMap what inputFields m)
    return m
  outputs <- forM [0 .. length tx.outputs - 1] $ \i -> do
    let what = "output " <> show i
    m <- mapOf what
    either refuse return (checkMap what outputFields m)
    return m
  return Psbt {psbtTx = tx, psbtGlobals = others, psbtInputs = inputs, psbtOutputs = outputs}

-- | The unsigned transaction of the global map, which must be there, in
-- the serialization without witness, with no input script
unsignedTxOf :: PsbtMap -> Either String Tx
unsignedTxOf globals = do
  bytes <- maybe (Left "The global map has no unsigned transaction") Right (Map.lookup unsignedTxKey globals)
  tx <- decodeFully "The unsigned transaction" bytes
  unless (null tx.witness && S.encode tx == bytes) $
    Left "The unsigned transaction is not in the serialization without witness"
  unless (all (BS.null . (.script)) tx.inputs) $
    Left "The unsigned transaction has an input with a script"
  return tx

-- | A value decoded from all the bytes given, or a refusal naming it
decodeFully :: (S.Serialize a) => String -> ByteString -> Either String a
decodeFully what bytes = case S.runGetState S.get bytes 0 of
  Right (a, rest) | BS.null rest -> Right a
  _ -> Left (what <> " does not decode")

-- | A sentence that starts with the words given
capital :: String -> String
capital (c : rest) = toUpper c : rest
capital [] = []

-- | A key as a refusal names it: its type in hexadecimal, and its data
showKey :: PsbtKey -> String
showKey key =
  "0x" <> hexByte key.psbtKeyType <> (if BS.null key.psbtKeyData then "" else " with the data " <> concatMap hexByte (BS.unpack key.psbtKeyData))
  where
    hexByte b = (if b < 16 then "0" else "") <> showHex b ""

{- The shapes of the spec -}

-- | The data of a key of the spec: none, a public key, or an extended
-- public key
data KeyShape = NoData | PublicKeyData | XpubData

-- | The fields the spec defines for a map, by type: the shape of their
-- keys, and a check of their values given the data of the key
type Fields = Map Word8 (KeyShape, ByteString -> ByteString -> Either String ())

-- | Check every key of a map against the spec: the keys of the fields it
-- defines must have their shape and their values must decode, and a
-- proprietary key must have its identifier and subtype. Other keys are
-- not the spec's to check.
checkMap :: String -> Fields -> PsbtMap -> Either String ()
checkMap what fields m = forM_ (Map.toList m) $ \(key, value) ->
  first ((capital what <> ": ") <>) $
    case Map.lookup key.psbtKeyType fields of
      Just (shape, checkValue) -> do
        checkShape shape key
        first (("the value of the key " <> showKey key <> " ") <>) $ checkValue key.psbtKeyData value
      Nothing
        | key.psbtKeyType == proprietaryType -> () <$ proprietaryOf key
        | otherwise -> return ()

checkShape :: KeyShape -> PsbtKey -> Either String ()
checkShape shape key = case shape of
  NoData -> unless (BS.null key.psbtKeyData) $ Left ("the key " <> showKey key <> " carries data")
  PublicKeyData -> unless (isPublicKeyBytes key.psbtKeyData) $ Left ("the key " <> showKey key <> " is not a public key")
  XpubData -> unless (BS.length key.psbtKeyData == 78) $ Left ("the key " <> showKey key <> " is not an extended public key")

-- | Whether the bytes have the length and the first byte of a public key,
-- compressed or not. Whether they are a point of the curve is checked
-- where the key is used.
isPublicKeyBytes :: ByteString -> Bool
isPublicKeyBytes bytes = case BS.unpack (BS.take 1 bytes) of
  [0x02] -> BS.length bytes == 33
  [0x03] -> BS.length bytes == 33
  [0x04] -> BS.length bytes == 65
  _ -> False

globalFields :: Fields
globalFields =
  Map.fromList
    [ (xpubType, (XpubData, xpubValue)),
      (versionType, (NoData, const versionValue))
    ]
  where
    -- The path of an extended key has one index per level of its depth,
    -- the fifth byte of the key
    xpubValue keyData value = do
      derivation <- decodeDerivation value
      unless (length (pathToList derivation.derivationPath) == fromIntegral (BS.index keyData 4)) $
        Left "has a path that is not as deep as the key"
    versionValue value = do
      version <- decodeWord32le value
      unless (version == 0) $ Left ("is a version hw does not read: PSBT version " <> show version)

inputFields :: Fields
inputFields =
  Map.fromList
    [ (nonWitnessUtxoType, (NoData, const txValue)),
      (witnessUtxoType, (NoData, const txOutValue)),
      (partialSigType, (PublicKeyData, const anyValue)),
      (sigHashTypeType, (NoData, const sigHashValue)),
      (inRedeemScriptType, (NoData, const anyValue)),
      (inWitnessScriptType, (NoData, const anyValue)),
      (inDerivationType, (PublicKeyData, const derivationValue)),
      (finalScriptSigType, (NoData, const anyValue)),
      (finalScriptWitnessType, (NoData, const witnessValue))
    ]
  where
    txValue value = () <$ (decodeTx value :: Either String Tx)
    txOutValue value = () <$ (decodeTxOut value :: Either String TxOut)
    sigHashValue value = () <$ decodeWord32le value
    witnessValue value = () <$ decodeWitness value

outputFields :: Fields
outputFields =
  Map.fromList
    [ (outRedeemScriptType, (NoData, const anyValue)),
      (outWitnessScriptType, (NoData, const anyValue)),
      (outDerivationType, (PublicKeyData, const derivationValue))
    ]

anyValue :: ByteString -> Either String ()
anyValue _ = Right ()

derivationValue :: ByteString -> Either String ()
derivationValue value = () <$ decodeDerivation value

decodeTx :: ByteString -> Either String Tx
decodeTx = decodeFully "is not a transaction: it"

decodeTxOut :: ByteString -> Either String TxOut
decodeTxOut = decodeFully "is not an output: it"

decodeWord32le :: ByteString -> Either String Word32
decodeWord32le bytes
  | BS.length bytes == 4 = Right $ BS.foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) 0 bytes
  | otherwise = Left "is not a 32-bit integer"

-- | A witness stack: the number of items, then each item after its length
decodeWitness :: ByteString -> Either String WitnessStack
decodeWitness = parseAll stack "the final witness"
  where
    stack = do
      count <- lengthOf "the final witness"
      replicateM count (sizedBytes "the final witness")

encodeWitness :: WitnessStack -> ByteString
encodeWitness stack = compactSize (fromIntegral (length stack)) <> BS.concat (sized <$> stack)

{- Derivations -}

-- | How a key derives: the fingerprint of the key the path starts from,
-- the master key's as the spec wants it, and the path from it. A key
-- whose origin is unknown is named by its own fingerprint and an empty
-- path, or by the fingerprint of an ancestor and the path from it.
data Derivation = Derivation
  { derivationFingerprint :: !Fingerprint,
    derivationPath :: !DerivPath
  }
  deriving (Eq, Show)

encodeDerivation :: Derivation -> ByteString
encodeDerivation derivation =
  S.encode derivation.derivationFingerprint <> BS.concat (word32le <$> pathToList derivation.derivationPath)

-- | A derivation: four bytes of fingerprint, then four per index of the
-- path, little-endian
decodeDerivation :: ByteString -> Either String Derivation
decodeDerivation bytes = do
  unless (BS.length bytes >= 4 && (BS.length bytes - 4) `mod` 4 == 0) $
    Left "is not a fingerprint and a path"
  let (fingerprintBytes, pathBytes) = BS.splitAt 4 bytes
  fingerprint <- decodeFully "is not a fingerprint: it" fingerprintBytes
  indices <- mapM decodeWord32le (chunksOf 4 pathBytes)
  return $ Derivation fingerprint (listToPath indices)

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bytes
  | BS.null bytes = []
  | otherwise = let (chunk, rest) = BS.splitAt n bytes in chunk : chunksOf n rest

-- | The derivation records of the type, by public key, in the order of
-- the map
derivationsOf :: Word8 -> Ctx -> PsbtMap -> Either String [(PublicKey, Derivation)]
derivationsOf t ctx m =
  forM [(key, value) | (key, value) <- Map.toAscList m, key.psbtKeyType == t] $ \(key, value) -> do
    public <- first (const ("The key " <> showKey key <> " is not a public key")) (unmarshal ctx key.psbtKeyData)
    derivation <- first (("The derivation of the key " <> showKey key <> " ") <>) (decodeDerivation value)
    return (public, derivation)

insertDerivation :: Word8 -> Ctx -> PublicKey -> Derivation -> PsbtMap -> PsbtMap
insertDerivation t ctx public derivation = Map.insert (PsbtKey t (marshal ctx public)) (encodeDerivation derivation)

-- | The derivations of the keys of an input: PSBT_IN_BIP32_DERIVATION
inputDerivations :: Ctx -> PsbtMap -> Either String [(PublicKey, Derivation)]
inputDerivations = derivationsOf inDerivationType

insertInputDerivation :: Ctx -> PublicKey -> Derivation -> PsbtMap -> PsbtMap
insertInputDerivation = insertDerivation inDerivationType

-- | The derivations of the keys of an output: PSBT_OUT_BIP32_DERIVATION
outputDerivations :: Ctx -> PsbtMap -> Either String [(PublicKey, Derivation)]
outputDerivations = derivationsOf outDerivationType

insertOutputDerivation :: Ctx -> PublicKey -> Derivation -> PsbtMap -> PsbtMap
insertOutputDerivation = insertDerivation outDerivationType

-- | The extended public keys of the global map, with their derivations:
-- PSBT_GLOBAL_XPUB. Those of another network, by their version bytes, are
-- left aside.
xpubRecords :: Network -> Ctx -> PsbtMap -> Either String [(XPubKey, Derivation)]
xpubRecords net ctx m =
  fmap catMaybes . forM [(key, value) | (key, value) <- Map.toAscList m, key.psbtKeyType == xpubType] $ \(key, value) -> do
    derivation <- first (("The derivation of the key " <> showKey key <> " ") <>) (decodeDerivation value)
    return $ (\xpub -> (xpub, derivation)) <$> eitherToMaybe (unmarshal (net, ctx) key.psbtKeyData)

insertXpubRecord :: Network -> Ctx -> XPubKey -> Derivation -> PsbtMap -> PsbtMap
insertXpubRecord net ctx xpub derivation = Map.insert (PsbtKey xpubType (marshal (net, ctx) xpub)) (encodeDerivation derivation)

{- The coins and signatures of the inputs -}

-- | The field of a type whose key carries no data, if the map has it
plainField :: Word8 -> PsbtMap -> Maybe ByteString
plainField t = Map.lookup (PsbtKey t BS.empty)

insertPlainField :: Word8 -> ByteString -> PsbtMap -> PsbtMap
insertPlainField t = Map.insert (PsbtKey t BS.empty)

-- | The whole transaction the input spends from: PSBT_IN_NON_WITNESS_UTXO
nonWitnessUtxo :: PsbtMap -> Either String (Maybe Tx)
nonWitnessUtxo = traverse (first ("The previous transaction " <>) . decodeTx) . plainField nonWitnessUtxoType

insertNonWitnessUtxo :: Tx -> PsbtMap -> PsbtMap
insertNonWitnessUtxo = insertPlainField nonWitnessUtxoType . S.encode

-- | The output the input spends, alone: PSBT_IN_WITNESS_UTXO
witnessUtxo :: PsbtMap -> Either String (Maybe TxOut)
witnessUtxo = traverse (first ("The witness output " <>) . decodeTxOut) . plainField witnessUtxoType

-- | Whether the input carries a signature that is not final:
-- PSBT_IN_PARTIAL_SIG
hasPartialSignatures :: PsbtMap -> Bool
hasPartialSignatures = any ((== partialSigType) . psbtKeyType) . Map.keys

-- | The sighash the input is to be signed with: PSBT_IN_SIGHASH_TYPE
sigHashType :: PsbtMap -> Either String (Maybe SigHash)
sigHashType = traverse (fmap fromIntegral . first ("The sighash type " <>) . decodeWord32le) . plainField sigHashTypeType

insertSigHashType :: SigHash -> PsbtMap -> PsbtMap
insertSigHashType = insertPlainField sigHashTypeType . word32le . fromIntegral

-- | The input script of a signed input: PSBT_IN_FINAL_SCRIPTSIG
finalScriptSig :: PsbtMap -> Maybe ByteString
finalScriptSig = plainField finalScriptSigType

insertFinalScriptSig :: ByteString -> PsbtMap -> PsbtMap
insertFinalScriptSig = insertPlainField finalScriptSigType

-- | The witness of a signed input: PSBT_IN_FINAL_SCRIPTWITNESS
finalScriptWitness :: PsbtMap -> Either String (Maybe WitnessStack)
finalScriptWitness = traverse decodeWitness . plainField finalScriptWitnessType

insertFinalScriptWitness :: WitnessStack -> PsbtMap -> PsbtMap
insertFinalScriptWitness = insertPlainField finalScriptWitnessType . encodeWitness

{- Proprietary fields -}

-- | A proprietary field, of type 0xFC: the identifier of the software it
-- belongs to, its subtype under that identifier, and the data of its key,
-- none for most
data Proprietary = Proprietary
  { proprietaryIdentifier :: !ByteString,
    proprietarySubtype :: !Word64,
    proprietaryKeyData :: !ByteString
  }
  deriving (Eq, Show)

-- | The key of a proprietary field: the type 0xFC, the identifier after
-- its length, the subtype as a compact size integer, and the data
proprietaryKey :: Proprietary -> PsbtKey
proprietaryKey p =
  PsbtKey proprietaryType (sized p.proprietaryIdentifier <> compactSize p.proprietarySubtype <> p.proprietaryKeyData)

-- | The proprietary field a key is, if it is one: nothing for a key of
-- another type, a refusal for a proprietary key without its identifier
-- and subtype
proprietaryOf :: PsbtKey -> Either String (Maybe Proprietary)
proprietaryOf key
  | key.psbtKeyType /= proprietaryType = Right Nothing
  | otherwise =
      first (const ("the proprietary key " <> showKey key <> " has no identifier and subtype")) $
        Just <$> parseAll (Proprietary <$> sizedBytes what <*> compactSizeOf what <*> restOf) what key.psbtKeyData
  where
    what = "a proprietary key"

-- | The value of the proprietary field of the identifier and subtype,
-- with no key data, if the map has it
lookupProprietary :: ByteString -> Word64 -> PsbtMap -> Maybe ByteString
lookupProprietary identifier subtype = Map.lookup (proprietaryKey (Proprietary identifier subtype BS.empty))

insertProprietary :: ByteString -> Word64 -> ByteString -> PsbtMap -> PsbtMap
insertProprietary identifier subtype = Map.insert (proprietaryKey (Proprietary identifier subtype BS.empty))
