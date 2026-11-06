{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A transaction file as a PSBT (BIP 174), which preparetx, preparesweep,
-- bumpfee, exporttx, signtx and signsweep write, and reviewtx, importtx,
-- signtx, signsweep and sendtx read. The fields of the spec carry the
-- transaction, the whole transaction every coin comes from, and for a
-- SegWit coin its output alone as well, which the signers of such coins
-- want, the keys of the account in the inputs and outputs with their
-- derivation paths, the sighash to sign with, and the signatures, as the
-- final scripts and witnesses of the inputs. What is hw's own goes in
-- proprietary fields under the identifier "hw": the network, the fee rate
-- the transaction was built for, the transactions it replaces and its
-- label in the global map, the label of an address on its input or output,
-- the label a coin shows on its input, and the path of the address of the
-- account an input spends from.
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
-- relative to it, as the signing data wants them. The file carries no key
-- of the account beyond those of its addresses in the transaction.
--
-- A signed input is final: it carries its final script and witness, and
-- no key, sighash or partial signature any more, as the finalizer of the
-- spec leaves it and as Bitcoin Core writes it. The finalizer is
-- haskoin-core's finalizePsbt, here and for the files of other wallets:
-- it verifies every signature it uses against the coin. A finalizer
-- leaves the fields the spec does not define, so the path of the address
-- an input spends from stays, in the field of hw: the reader derives the
-- key of each of its accounts there, and the input is of the account
-- whose address the coin pays. No table of addresses and no search is
-- needed, so an offline signer that never handed out an address reads
-- the file it signed. A file without that field, which another wallet
-- prepared and finalized, is placed by the addresses the wallet knows.
-- The fingerprint of the seed is in the record of an output of the
-- account, if the transaction has one.
--
-- A file of another wallet is read the same way. An input it signed but
-- did not finalize, as a hardware wallet leaves it, is finalized when it
-- is read. A file that asks for a sighash hw does not sign with is
-- refused, as the spec wants. When hw signs a file of another wallet,
-- 'signedPsbt' hands its signatures to the finalizer in that file, and
-- every other field of it stays, known to hw or not, but those the
-- finalizer clears.
module Haskoin.Wallet.Tx.PsbtFile
  ( FileAccount (..),
    txFilePsbt,
    signedPsbt,
    psbtTxFile,

    -- * The fields of hw
    hwIdentifier,
    networkField,
    feeRateField,
    replacesField,
    labelField,
    addressLabelField,
    coinLabelField,
    inputPathField,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.List (sortOn, stripPrefix, zipWith4)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Keys (AccountKind, isChainPath, keyScript)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (offChainPath, sigHashName, walletSigHash)
import Numeric.Natural (Natural)

-- | The keys of an account, as the derivation records of its files name
-- them
data FileAccount = FileAccount
  { fileAccountXPub :: !XPubKey,
    -- | The kind of the account, which says the scripts of its addresses
    fileAccountKind :: !AccountKind,
    -- | The path of the account from the seed, m/purpose'/coin'/index'
    fileAccountPath :: !HardPath,
    -- | The fingerprint of the seed, when the wallet has learnt it
    fileAccountFingerprint :: !(Maybe Fingerprint),
    -- | The network of the account, which says the sighash it signs with
    fileAccountNetwork :: !Network
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
-- the coin shows, both in UTF-8, and the path of the address of the
-- account the input spends from ('inputPathBytes')
addressLabelField, coinLabelField, inputPathField :: Word64
addressLabelField = 0
coinLabelField = 1
inputPathField = 2

-- | The path of an address in its account, as hw's field of an input
-- holds it: the chain and the index, /0/5, 4 bytes each, little-endian,
-- as a derivation record writes the steps of a path. The field holds no
-- key and no fingerprint. It says where the address is, which the reader
-- checks against the coin.
inputPathBytes :: SoftPath -> ByteString
inputPathBytes path = S.runPut (mapM_ S.putWord32le (pathToList path))

-- | The path hw's field of an input holds: /0/n or /1/n, where an account
-- has its addresses ('isChainPath'). hw writes no other value, so any
-- other is refused.
inputPathOf :: Int -> ByteString -> Either String SoftPath
inputPathOf i bytes =
  case S.runGet ((,) <$> S.getWord32le <*> S.getWord32le) bytes of
    Right (chain, index)
      | BS.length bytes == 8,
        Just path <- toSoft (listToPath [chain, index]),
        isChainPath path ->
          Right path
    _ -> Left ("Input " <> show i <> ": the path of the address it spends from, in the field of hw, is not /0/n or /1/n")

{- Writing -}

-- | An address of the account at a path, as the file names it: its key,
-- the script that pays it, and the derivation of the key
data AddressKey = AddressKey
  { addressPath :: !SoftPath,
    addressPublic :: !PublicKey,
    addressScript :: !ByteString,
    addressDerivation :: !Derivation
  }

-- | The address of the account at a path, its script of the kind of the
-- account, with its key derived from the seed when its fingerprint is
-- known, from the account's key otherwise
addressKey :: Ctx -> FileAccount -> Maybe Fingerprint -> SoftPath -> AddressKey
addressKey ctx acc fingerprintM path =
  AddressKey
    { addressPath = path,
      addressPublic = public,
      addressScript = marshal ctx (keyScript acc.fileAccountKind ctx public),
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
      forM_ (tsd.txSignDataInputPaths <> tsd.txSignDataOutputPaths) $ \path ->
        unless (isChainPath path) $ Left (offChainPath path)
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
  return $ Psbt Version0 (psbtOf tx).unsignedTx globals inputs outputs

-- | The map of an input: the transaction its coin comes from, the coin
-- itself as well for a SegWit one, the label its coin shows, if any, and
-- the path and the label of the address of the account it spends from, if
-- any, in the fields of hw. While it is unsigned: the sighash to sign it
-- with, and the derivation record of that address. Once signed: its final
-- script and witness, and none of what served the signers, as the
-- finalizer of the spec leaves an input. The fields of hw stay, as a
-- finalizer leaves the fields the spec does not define: the path is how
-- the reader of a signed file knows the input as the account's. The
-- whole transaction is what hw and the hardware wallets that check the
-- amounts want; the coin alone is what the spec asks for a SegWit input,
-- and what the signers that read no more look at.
inputMap :: Ctx -> Network -> TxFile -> [AddressKey] -> TxIn -> WitnessStack -> (Tx, TxOut) -> PsbtMap
inputMap ctx net file keys input stack (parent, coin) =
  foldr ($) (insertNonWitnessUtxo parent Map.empty) $
    [insertWitnessUtxo coin | segWitCoin]
      <> [insertSigHashType (walletSigHash net) | unsigned]
      <> [insertFinalScriptSig input.script | not (BS.null input.script)]
      <> [insertFinalScriptWitness stack | not (null stack)]
      <> concat [spentAddressFields key | key <- keys, key.addressScript == coin.script]
      <> [insertProprietary hwIdentifier coinLabelField (encodeUtf8 label) | Just label <- [Map.lookup input.outpoint.hash file.txFileInputLabels]]
  where
    unsigned = BS.null input.script && null stack
    segWitCoin = either (const False) isSegwit (unmarshal ctx coin.script)
    -- The address of the account the input spends from: its path and its
    -- label, and the derivation record of its key while a signer needs it
    spentAddressFields key =
      [insertProprietary hwIdentifier inputPathField (inputPathBytes key.addressPath)]
        <> [insertInputDerivation ctx key.addressPublic key.addressDerivation | unsigned]
        <> addressLabel file key

-- | The map of an output: the derivation and label of the address of the
-- account it pays, if any
outputMap :: Ctx -> TxFile -> [AddressKey] -> TxOut -> PsbtMap
outputMap ctx file keys output =
  foldr ($) Map.empty $
    concat [insertOutputDerivation ctx key.addressPublic key.addressDerivation : addressLabel file key | key <- keys, key.addressScript == output.script]

-- | The label of an address of the account, in the field of hw of its
-- input or output, when the file has one for its path
addressLabel :: TxFile -> AddressKey -> [PsbtMap -> PsbtMap]
addressLabel file key =
  [insertProprietary hwIdentifier addressLabelField (encodeUtf8 label) | Just label <- [Map.lookup key.addressPath file.txFilePathLabels]]

{- Signing into a file -}

-- | The PSBT of a transaction just signed, written into the PSBT its file
-- was read from, as the spec wants a signer and a finalizer to work. The
-- signature of each input hw signed goes among the partial signatures of
-- that input, as a signer hands it over. haskoin-core's finalizePsbt then
-- verifies it against the coin, builds the final script and witness, and
-- clears what served the signers: the partial signatures, the sighash
-- type, the redeem script and the derivation records, as Bitcoin Core
-- does. Every other field of the file stays, known to hw or not. The
-- file, once finalized, must hold the very transaction signed.
--
-- The path of each address of the account the transaction spends from
-- goes in the field of hw of its input first, as hw writes it in a file
-- it prepares ('inputPathField'). A file another wallet prepared has no
-- such field, and its records go with the signature: the path is what
-- lets a wallet that never handed out the address read the signed file.
--
-- The derivation records that are left, those of the outputs and of the
-- inputs still unsigned, and that named the keys of the account by the
-- account's own key, when the wallet did not know the seed, are rewritten
-- from the seed the signer names. The network goes in hw's field when the
-- file has none. The transaction signed must be the file's, and the
-- signed file must name its network, which says how a signature is
-- verified.
signedPsbt :: Ctx -> Maybe FileAccount -> Psbt -> TxFile -> Either String Psbt
signedPsbt ctx accountM source file = do
  net <- maybe (Left "The file names no network") Right file.txFileNetwork
  let tsd = file.txFileData
      tx = tsd.txSignDataTx
  unless (nosigHash tx == nosigHash source.unsignedTx) $
    Left "The transaction signed is not the one of the file"
  coins <- inputCoins source
  let stacks = tx.witness <> repeat []
      -- The paths the account signed with, by the script of their address
      spentPaths = case accountM of
        Just acc -> Map.fromList [(key.addressScript, key.addressPath) | key <- addressKey ctx acc Nothing <$> tsd.txSignDataInputPaths]
        Nothing -> Map.empty
      withPath :: Maybe InputCoin -> PsbtMap -> PsbtMap
      withPath coinM m = case (\found -> Map.lookup found.coin.script spentPaths) =<< coinM of
        Just path -> insertProprietary hwIdentifier inputPathField (inputPathBytes path) m
        Nothing -> m
      withSignatures = zipWith4 (signerFields net) coins tx.inputs stacks (zipWith withPath coins source.inputs)
      globals = case lookupProprietary hwIdentifier networkField source.globals of
        Nothing -> insertProprietary hwIdentifier networkField (cs net.name) source.globals
        Just _ -> source.globals
  finalized <- finalizePsbt net ctx (Psbt source.version source.unsignedTx globals withSignatures source.outputs)
  final <- psbtPartialTransaction finalized
  unless (S.encode final == S.encode tx) $
    Left "The file, once finalized, does not hold the transaction signed"
  let fingerprintM = tsd.txSignDataFingerprint <|> (fileAccountFingerprint =<< accountM)
      namedBySeed records insert m = maybe (return m) (\(acc, fp) -> fromSeed records insert acc fp m) ((,) <$> accountM <*> fingerprintM)
  inputs <- forM finalized.inputs (namedBySeed (inputDerivations ctx) (insertInputDerivation ctx))
  outputs <- forM finalized.outputs (namedBySeed (outputDerivations ctx) (insertOutputDerivation ctx))
  -- The file keeps its version. hw signs with the sighash ALL, with
  -- FORKID on Bitcoin Cash, after which a file of version 2 takes no more
  -- inputs or outputs: its flags say so.
  return $ afterSignature sigHashAll (Psbt finalized.version finalized.unsignedTx finalized.globals inputs outputs)
  where
    -- The map of an input with the signature hw made for it, as a signer
    -- gives it to the finalizer: the key and its signature and, for a
    -- P2WPKH coin nested in P2SH, the redeem script the input pushes.
    -- These are the three kinds of coins hw signs. An input that is
    -- unsigned in the transaction, or final in the file already, is left
    -- as it is.
    signerFields :: Network -> Maybe InputCoin -> TxIn -> WitnessStack -> PsbtMap -> PsbtMap
    signerFields net coinM input stack m
      | BS.null input.script && null stack = m
      | isJust (finalScriptSig m) || either (const False) isJust (finalScriptWitness m) = m
      | otherwise = fromMaybe m $ do
          found <- coinM
          coinScript <- eitherToMaybe (unmarshal ctx found.coin.script)
          let scriptInput = eitherToMaybe (unmarshal (net, ctx) input.script) :: Maybe ScriptInput
          case (coinScript, scriptInput, stack) of
            (PayPKHash _, Just (RegularInput (SpendPKHash signature public)), []) ->
              Just (insertPartialSignature ctx public (encodeTxSig net ctx signature) m)
            (PayWitnessPKHash _, _, [signature, key])
              | BS.null input.script ->
                  (\public -> insertPartialSignature ctx public signature m) <$> eitherToMaybe (unmarshal ctx key)
            (PayScriptHash _, Just (NestedWitnessInput redeem), [signature, key]) -> do
              public <- eitherToMaybe (unmarshal ctx key)
              redeemScript <- eitherToMaybe (encodeOutput ctx redeem)
              Just (insertInputRedeemScript redeemScript (insertPartialSignature ctx public signature m))
            _ -> Nothing
    -- The records of the map that name a key by the account's own key,
    -- named by the seed and the full path instead
    fromSeed ::
      (PsbtMap -> Either String [(PublicKey, Derivation)]) ->
      (PublicKey -> Derivation -> PsbtMap -> PsbtMap) ->
      FileAccount ->
      Fingerprint ->
      PsbtMap ->
      Either String PsbtMap
    fromSeed records insert acc fingerprint m = do
      found <- records m
      return $
        foldr
          (\(public, d) -> insert public (Derivation fingerprint (listToPath (pathToList acc.fileAccountPath <> pathToList d.path))))
          m
          [(public, d) | (public, d) <- found, d.fingerprint == xPubFP ctx acc.fileAccountXPub]

{- Reading -}

-- | What the derivation records of a PSBT say of an account: the paths of
-- its addresses among the inputs and among the outputs, each once, in
-- order; the fingerprint of its seed when the records carry it; and the
-- labels of those addresses
data AccountMatch = AccountMatch
  { matchAccount :: !FileAccount,
    matchInputPaths :: ![SoftPath],
    matchOutputPaths :: ![SoftPath],
    matchFingerprint :: !(Maybe Fingerprint),
    matchLabels :: !(Map SoftPath Text)
  }

-- | The transaction file of a PSBT, as the signing data of hw holds it,
-- with its paths relative to the account among those given that the file
-- names: the first with an address on an input, or else on an output,
-- those on the network the file names coming first. A derivation record
-- names an address of an account by its key. The field of hw of an input
-- names one by its path alone ('inputPathField'), which a signed input
-- still has: it is of the account whose address at that path the coin
-- pays, and of no account when none has it there. A PSBT that names none
-- of the accounts is read without paths, as a transaction outside the
-- wallet. Every input must carry the whole transaction its coin comes
-- from, which haskoin-core checks against the input. An input to be
-- signed with a sighash hw does not sign with is refused: that of the
-- account's network, or ALL, with FORKID on Bitcoin Cash, when no account
-- is involved.
--
-- An input signed but not finalized, as a hardware wallet leaves it, is
-- finalized by haskoin-core's finalizePsbt, which verifies the signatures
-- it uses against the coin and refuses one that does not verify. A
-- signature is verified as its network has it: the network the file
-- names, else that of the account its records name, else the one given,
-- which a command takes from --network or from the configuration. When
-- nothing names a network, nothing is finalized, and such an input reads
-- as unsigned.
psbtTxFile :: Ctx -> Maybe Network -> [FileAccount] -> Psbt -> Either String TxFile
psbtTxFile ctx fallbackM accounts psbt = do
  -- The coin of every input, as haskoin-core reads it from the fields of
  -- the input ('inputCoins'): the output of its previous transaction,
  -- which must be the transaction the input spends and have that output.
  -- A coin given alone beside it, as a SegWit input has it, must be that
  -- output too.
  checkedCoins <- inputCoins psbt
  parents <- forM (zip3 [0 :: Int ..] psbt.inputs checkedCoins) $ \(i, m, checkedM) -> do
    let noParent = Left ("Input " <> show i <> " has no previous transaction: hw needs the whole transaction each coin comes from")
    -- A coin known from its output alone is a claim of whoever wrote the
    -- file: hw wants the transaction it comes from, which proves it
    case checkedM of
      Just checked | checked.fromPreviousTx -> maybe noParent Right =<< nonWitnessUtxo m
      _ -> noParent
  asked <- forM (zip [0 :: Int ..] psbt.inputs) $ \(i, m) -> first (("Input " <> show i <> ": ") <>) (sigHashType m)
  let askedSigHashes = [(i, sh) | (i, Just sh) <- zip [0 :: Int ..] asked]
  netM <- traverse networkOf (lookupProprietary hwIdentifier networkField psbt.globals)
  feeRateM <- traverse feeRateOf (lookupProprietary hwIdentifier feeRateField psbt.globals)
  replaces <- maybe (Right []) replacesOf (lookupProprietary hwIdentifier replacesField psbt.globals)
  label <- maybe (Right noLabel) (textOf "The label of the transaction") (lookupProprietary hwIdentifier labelField psbt.globals)
  -- The path the field of hw names on each input, beside the script of
  -- the coin the input spends
  fieldPaths <- forM (zip [0 :: Int ..] psbt.inputs) $ \(i, m) ->
    traverse (inputPathOf i) (lookupProprietary hwIdentifier inputPathField m)
  let spent = zip fieldPaths (map (fmap (.coin.script)) checkedCoins)
  matches <- mapM (accountMatch ctx netM psbt spent) accounts
  let onFileNetwork :: AccountMatch -> Bool
      onFileNetwork m = maybe True (== m.matchAccount.fileAccountNetwork) netM
      candidates =
        sortOn (not . onFileNetwork) $
          [m | m <- matches, not (null m.matchInputPaths)]
            <> [m | m <- matches, null m.matchInputPaths, not (null m.matchOutputPaths)]
      signsWith :: AccountMatch -> Maybe Network
      signsWith m = Just m.matchAccount.fileAccountNetwork
  chosen <- case candidates of
    [] -> do
      forM_ askedSigHashes $ \(i, sh) -> unless (acceptableSigHash netM sh) $ Left (sigHashRefusal i sh netM)
      return Nothing
    first' : _ -> case filter (\m -> all (acceptableSigHash (signsWith m) . snd) askedSigHashes) candidates of
      m : _ -> return (Just m)
      [] -> case [(i, sh) | (i, sh) <- askedSigHashes, not (acceptableSigHash (signsWith first') sh)] of
        (i, sh) : _ -> Left (sigHashRefusal i sh (signsWith first'))
        [] -> return (Just first')
  -- The finalizer of the spec, for the inputs another signer left signed
  -- and not final
  let signedOn = netM <|> (signsWith =<< chosen) <|> fallbackM
  finalized <- maybe (Right psbt) (\net -> finalizePsbt net ctx psbt) signedOn
  -- The transaction with the final scripts the file has. A file may still
  -- be unsigned, so this is not haskoin-core's psbtTransaction, the
  -- extractor, which takes a PSBT whose inputs are all final.
  tx <- psbtPartialTransaction finalized
  coinLabels <- forM (zip tx.inputs psbt.inputs) $ \(input, m) ->
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

-- | Whether hw signs with the sighash: that of the network when known,
-- else ALL, with or without FORKID
acceptableSigHash :: Maybe Network -> SigHash -> Bool
acceptableSigHash netM sh = case netM of
  Just net -> sh == walletSigHash net
  Nothing -> sh == sigHashAll || sh == setForkIdFlag sigHashAll

-- | The refusal of an input to be signed with a sighash hw does not sign
-- with
sigHashRefusal :: Int -> SigHash -> Maybe Network -> String
sigHashRefusal i sh netM =
  "Input " <> show i <> " is to be signed with the sighash " <> sigHashName sh <> ": hw signs with " <> case netM of
    Just net -> sigHashName (walletSigHash net) <> " on " <> net.name
    Nothing -> "ALL, or ALL|FORKID on Bitcoin Cash"

-- | What the PSBT says of the account: the addresses of the account its
-- derivation records name, and those the field of hw of an input names,
-- given here for each input beside the script of its coin
accountMatch :: Ctx -> Maybe Network -> Psbt -> [(Maybe SoftPath, Maybe ByteString)] -> FileAccount -> Either String AccountMatch
accountMatch ctx netM psbt spent acc = do
  inputs <- forM (zip3 [0 :: Int ..] psbt.inputs spent) $ \(i, m, (fieldPathM, coinScriptM)) -> do
    named <- addressesOf ("Input " <> show i) m =<< inputDerivations ctx m
    labelM <- labelOf m
    -- The path of hw's field is of this account when the address of the
    -- account there is the one the coin pays. Otherwise it is of another
    -- account, or of another wallet, and says nothing of this one.
    let followed = [(path, Nothing, labelM) | Just path <- [fieldPathM], Just coinScript <- [coinScriptM], scriptAt path == Just coinScript]
    return (named <> followed)
  outputs <- forM (zip [0 :: Int ..] psbt.outputs) $ \(i, m) ->
    addressesOf ("Output " <> show i) m =<< outputDerivations ctx m
  -- The record of the account's own key, if the global map has one, says
  -- the fingerprint of the seed too
  xpubs <- maybe (return []) (\net -> xpubRecords net ctx psbt.globals) netM
  let ofAccount = [d.fingerprint | (xpub, d) <- xpubs, xpub == acc.fileAccountXPub, d.path == toGeneric acc.fileAccountPath]
      found = concat (inputs <> outputs)
  return
    AccountMatch
      { matchAccount = acc,
        matchInputPaths = nubOrd [path | (path, _, _) <- concat inputs],
        matchOutputPaths = nubOrd [path | (path, _, _) <- concat outputs],
        matchFingerprint = listToMaybe ([fingerprint | (_, Just fingerprint, _) <- found] <> ofAccount),
        matchLabels = firstOfEach [(path, label) | (path, _, Just label) <- found]
      }
  where
    -- The addresses of the account among the keys of a map, with the
    -- fingerprint of the seed when their records carry it, and the label
    -- of the address when the map has one
    addressesOf place m records = do
      labelM <- labelOf m
      found <- forM records $ \record -> fmap (\(path, fingerprintM) -> (path, fingerprintM, labelM)) <$> matchRecord ctx acc place record
      return (mapMaybe id found)
    labelOf m = traverse (textOf "The label of an address") (lookupProprietary hwIdentifier addressLabelField m)
    -- The script of the address of the account at a path, or none where
    -- the account has no key
    scriptAt path = case tryDerivePubPath ctx path acc.fileAccountXPub of
      Right derived -> Just (marshal ctx (keyScript acc.fileAccountKind ctx (wrapPubKey True derived.key)))
      Left _ -> Nothing

-- | What a derivation record says of the account: the path of one of its
-- addresses, with the fingerprint of the seed when the record derives the
-- key from the seed; nothing for a key of another wallet; a refusal for a
-- record that names a key of the account but carries another public key,
-- or that names a key of the account at a path of no address of it. An
-- address of the account is at /0/n or /1/n ('isChainPath'): a file
-- naming the account's key elsewhere could pay funds there that the
-- account would never see, and later have them signed away.
matchRecord :: Ctx -> FileAccount -> String -> (PublicKey, Derivation) -> Either String (Maybe (SoftPath, Maybe Fingerprint))
matchRecord ctx acc place (public, derivation)
  | Just rest <- stripPrefix (pathToList acc.fileAccountPath) indices, not (null rest) = resolve rest (Just fingerprint)
  | fingerprint == xPubFP ctx acc.fileAccountXPub, not (null indices) = resolve indices Nothing
  | otherwise = Right Nothing
  where
    fingerprint = derivation.fingerprint
    indices = pathToList derivation.path
    -- Whether the record's fingerprint is the account's: the fingerprint of
    -- its seed, when known, or of its own key
    namesAccount fromSeed = case fromSeed of
      Just f -> Just f == acc.fileAccountFingerprint
      Nothing -> True
    -- Whether the record's key is the account's at a soft path below it.
    -- The path comes from the file and may be of any length, so it is
    -- followed with tryDerivePubPath, which does not throw: a path too
    -- deep to have a key is of no key of the account.
    keyAt path = case tryDerivePubPath ctx path acc.fileAccountXPub of
      Right derived -> wrapPubKey True derived.key == public
      Left _ -> False
    resolve rest fromSeed = case toSoft (listToPath rest) of
      Just path
        | isChainPath path -> onChain path fromSeed
        -- A soft path off the chains: the account's key there, or a record
        -- that says it is, is refused; another wallet's is left aside
        | namesAccount fromSeed || keyAt path -> Left (offChains rest)
        | otherwise -> Right Nothing
      -- A hardened step below the account derives no address of it: a
      -- record that names the account there is refused all the same
      Nothing
        | namesAccount fromSeed -> Left (offChains rest)
        | otherwise -> Right Nothing
    -- A record at a path of a chain of the account: matched when its key
    -- is the account's there, refused when it is not but the record says
    -- so, and another wallet's otherwise
    onChain path fromSeed
      | keyAt path = Right (Just (path, fromSeed))
      | namesAccount fromSeed = Left (place <> " names the key " <> pathToStr path <> " of the account, but carries another public key")
      | otherwise = Right Nothing
    offChains rest =
      place <> " names a key of the account at " <> pathToStr (listToPath rest) <> ", where it has no address: its addresses are at /0/n and /1/n"

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
