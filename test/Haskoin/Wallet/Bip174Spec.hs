{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The PSBT container of BIP 174: the vectors of the BIP, the bytes of a
-- PSBT as hw writes them and the refusals of what breaks the spec, and the
-- fields hw reads and writes.
module Haskoin.Wallet.Bip174Spec (spec) where

import Control.Monad (forM_)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Haskoin
import Haskoin.Wallet.Arbitrary (arbitraryPsbt)
import Haskoin.Wallet.Bip174
import Haskoin.Wallet.TestUtils
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)

{- The vectors of the BIP -}

-- The vectors live in test/data/psbt/bip174.json: see the README.md there.

data Vector = Vector
  { vecCase :: String,
    vecHex :: Text,
    vecBase64 :: Maybe Text
  }

instance Json.FromJSON Vector where
  parseJSON = Json.withObject "vector" $ \o -> Vector <$> o .: "case" <*> o .: "hex" <*> o .:? "base64"

-- | The invalid PSBTs, the valid ones, the PSBTs of the walk-through of the
-- roles, in the order of the BIP, and the transaction its extractor gives
data Vectors = Vectors
  { invalid :: [Vector],
    valid :: [Vector],
    roles :: [Vector],
    extracted :: Vector
  }

instance Json.FromJSON Vectors where
  parseJSON = Json.withObject "vectors" $ \o -> Vectors <$> o .: "invalid" <*> o .: "valid" <*> o .: "roles" <*> o .: "extracted"

vectorsFile :: FilePath
vectorsFile = "test/data/psbt/bip174.json"

-- The PSBTs of the walk-through, by their place in it
creator, updater, updaterWithSigHash, firstSigner, secondSigner, combiner, finalizer, unknownA, unknownB, unknownCombined, toCombineA, toCombineB, combined :: Vectors -> Vector
creator v = roles v !! 0
updater v = roles v !! 1
updaterWithSigHash v = roles v !! 2
firstSigner v = roles v !! 3
secondSigner v = roles v !! 4
combiner v = roles v !! 5
finalizer v = roles v !! 6
unknownA v = roles v !! 7
unknownB v = roles v !! 8
unknownCombined v = roles v !! 9
toCombineA v = roles v !! 10
toCombineB v = roles v !! 11
combined v = roles v !! 12

bytesOf :: Vector -> ByteString
bytesOf = hexBytes . vecHex

-- | The PSBT of a vector, which must decode
psbtOfVector :: Vector -> IO Psbt
psbtOfVector v = expectRight (decodePsbt (bytesOf v))

-- | The map of the input, or of the output, at the index
inputAt, outputAt :: Int -> Psbt -> PsbtMap
inputAt i psbt = psbt.psbtInputs !! i
outputAt i psbt = psbt.psbtOutputs !! i

spec :: Spec
spec = do
  vectors <- runIO $ either fail return =<< Json.eitherDecodeFileStrict vectorsFile
  prepareContext $ \ctx -> do
    vectorsSpec vectors
    rolesSpec vectors
    bytesSpec vectors
    fieldsSpec ctx vectors
    propertiesSpec ctx

{- The vectors -}

vectorsSpec :: Vectors -> Spec
vectorsSpec vectors = do
  describe "The invalid PSBTs of BIP 174 are refused" $
    forM_ vectors.invalid $ \v ->
      it v.vecCase $ decodePsbt (bytesOf v) `shouldSatisfy` isLeft

  describe "The valid PSBTs of BIP 174 are read, and written back byte for byte" $
    forM_ vectors.valid $ \v ->
      it v.vecCase $ (encodePsbt <$> decodePsbt (bytesOf v)) `shouldBe` Right (bytesOf v)

  describe "The valid PSBTs of BIP 174 read the same from their base64 text" $
    forM_ [(v, text) | v <- vectors.valid, Just text <- [v.vecBase64]] $ \(v, text) ->
      it v.vecCase $ (encodePsbt <$> decodePsbtBase64 (encodeUtf8 text)) `shouldBe` Right (bytesOf v)

  -- The combiner's PSBT of the two signed ones lists the signatures of
  -- its second input out of order: see 'rolesSpec'
  describe "The PSBTs of the roles of BIP 174 are read, and written back byte for byte" $
    forM_ [v | v <- vectors.roles, v.vecCase /= (combiner vectors).vecCase] $ \v ->
      it v.vecCase $ (encodePsbt <$> decodePsbt (bytesOf v)) `shouldBe` Right (bytesOf v)

-- | The union of two PSBTs of one transaction, as a combiner makes it
unionPsbt :: Psbt -> Psbt -> Psbt
unionPsbt a b =
  Psbt
    { psbtTx = a.psbtTx,
      psbtGlobals = Map.union a.psbtGlobals b.psbtGlobals,
      psbtInputs = zipWith Map.union a.psbtInputs b.psbtInputs,
      psbtOutputs = zipWith Map.union a.psbtOutputs b.psbtOutputs
    }

rolesSpec :: Vectors -> Spec
rolesSpec vectors =
  describe "The roles of BIP 174" $ do
    it "the transaction extractor gives the transaction of the finalizer's PSBT" $ do
      psbt <- psbtOfVector (finalizer vectors)
      (S.encode <$> psbtTransaction psbt) `shouldBe` Right (bytesOf (extracted vectors))

    it "the transaction of a PSBT without final scripts is the unsigned one" $ do
      psbt <- psbtOfVector (creator vectors)
      psbtTransaction psbt `shouldBe` Right psbt.psbtTx

    -- The spec does not say in which order a combiner writes the keys of a
    -- map. The BIP's combiner lists the two signatures of the second input
    -- out of order; hw, like Bitcoin Core, writes the keys of a map in
    -- order. The PSBTs are the same, byte for byte once the BIP's is
    -- written back.
    it "the union of the PSBTs of the two signers is the combiner's PSBT, whatever the order of its inputs" $ do
      a <- psbtOfVector (firstSigner vectors)
      b <- psbtOfVector (secondSigner vectors)
      both <- psbtOfVector (combiner vectors)
      unionPsbt a b `shouldBe` both
      unionPsbt b a `shouldBe` both
      encodePsbt both `shouldNotBe` bytesOf (combiner vectors)
      decodePsbt (encodePsbt both) `shouldBe` Right both

    it "the union of the two PSBTs with unknown keys is the one of a combiner that orders keys lexicographically" $ do
      a <- psbtOfVector (unknownA vectors)
      b <- psbtOfVector (unknownB vectors)
      encodePsbt (unionPsbt a b) `shouldBe` bytesOf (unknownCombined vectors)
      encodePsbt (unionPsbt b a) `shouldBe` bytesOf (unknownCombined vectors)

    it "the union of the two PSBTs to combine is the combiner's PSBT" $ do
      a <- psbtOfVector (toCombineA vectors)
      b <- psbtOfVector (toCombineB vectors)
      encodePsbt (unionPsbt a b) `shouldBe` bytesOf (combined vectors)

    it "the updater's PSBT with SIGHASH_ALL is the updater's PSBT with the sighash type on every input" $ do
      psbt <- psbtOfVector (updater vectors)
      encodePsbt psbt {psbtInputs = insertSigHashType sigHashAll <$> psbt.psbtInputs} `shouldBe` bytesOf (updaterWithSigHash vectors)

{- Bytes -}

-- | A transaction with nothing in it: version 0, no input, no output, lock
-- time 0, which is "0000000000000000000000" over ten bytes
emptyTx :: Tx
emptyTx = Tx 0 [] [] [] 0

-- | The signature of the first signer of the BIP on the first input, as
-- Bitcoin Core decodes its PSBT
firstSignature :: Text
firstSignature = "3044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01"

-- | The global map of the PSBT of 'emptyTx': the key of the unsigned
-- transaction, one byte of type 0x00, and the ten bytes of the transaction
emptyTxEntry :: Text
emptyTxEntry = "01000a00000000000000000000"

bytesSpec :: Vectors -> Spec
bytesSpec vectors =
  describe "The bytes of a PSBT" $ do
    -- The vector of the BIP for a transaction without inputs or outputs
    it "are the magic bytes, the unsigned transaction, and a map for each input and output" $
      encodePsbt (psbtOf emptyTx) `shouldBe` hexBytes ("70736274ff" <> emptyTxEntry <> "00")

    -- The key is 5 bytes: the type 0xfc, the identifier hw after its
    -- length 2, and the subtype 0. The value is the 3 bytes of "btc".
    it "write a key as its length, its type and its data, and its value after its length" $
      encodePsbt (psbtOf emptyTx) {psbtGlobals = insertProprietary "hw" 0 "btc" Map.empty}
        `shouldBe` hexBytes ("70736274ff" <> emptyTxEntry <> "05fc02687700" <> "03627463" <> "00")

    it "write the keys of a map in order, the type first, whatever the order they were added" $ do
      let globals = Map.fromList [(PsbtKey 0x20 "", hexBytes "bb"), (PsbtKey 0x10 (hexBytes "01"), hexBytes "aa")]
      encodePsbt (psbtOf emptyTx) {psbtGlobals = globals}
        `shouldBe` hexBytes ("70736274ff" <> emptyTxEntry <> "02100101aa" <> "012001bb" <> "00")

    it "write the unsigned transaction without the input scripts and the witness of a signed one" $ do
      signed <- expectRight $ S.decode (bytesOf (extracted vectors))
      psbt <- psbtOfVector (finalizer vectors)
      (psbtOf signed).psbtTx `shouldBe` psbt.psbtTx

    it "tell a PSBT from another file by its first five bytes" $ do
      isPsbt (bytesOf (creator vectors)) `shouldBe` True
      isPsbt "{\"tx\": \"0200\"}" `shouldBe` False

    it "accept the unsigned transaction after another key of the global map" $
      decodePsbt (hexBytes ("70736274ff" <> "012001bb" <> emptyTxEntry <> "00"))
        `shouldBe` Right (psbtOf emptyTx) {psbtGlobals = Map.fromList [(PsbtKey 0x20 "", hexBytes "bb")]}

    it "refuse what is not a PSBT" $
      decodePsbt "{\"tx\": \"0200\"}" `shouldBe` Left "Not a PSBT: the file does not start with the magic bytes of BIP 174"

    it "refuse a key twice in a map, naming it" $
      decodePsbt (hexBytes ("70736274ff" <> emptyTxEntry <> "012001aa" <> "012001bb" <> "00"))
        `shouldBe` Left "The global map has the key 0x20 twice"

    it "refuse bytes after the last map" $
      decodePsbt (bytesOf (creator vectors) <> "\0") `shouldBe` Left "Bytes follow the last map"

    it "refuse a PSBT of a version hw does not read" $
      decodePsbt (hexBytes ("70736274ff" <> emptyTxEntry <> "01fb0402000000" <> "00"))
        `shouldBe` Left "The global map: the value of the key 0xfb is a version hw does not read: PSBT version 2"

    -- The length of the first key, 1, written over three bytes
    it "refuse a length that is not in its shortest form" $
      decodePsbt (hexBytes ("70736274ff" <> "fd0100" <> "000a00000000000000000000" <> "00"))
        `shouldBe` Left "A length in the global map is not in its shortest form"

    -- The PSBT with 0 inputs and 2 outputs of the BIP, without the two
    -- bytes that end its output maps
    it "refuse a file that ends inside a map" $ do
      let withoutOutputs = bytesOf (vectors.valid !! 9)
      decodePsbt (BS.take (BS.length withoutOutputs - 2) withoutOutputs) `shouldBe` Left "The file ends inside output 0"

    it "refuse a key of the spec that carries data it should not, naming it" $
      decodePsbt (bytesOf (vectors.invalid !! 15)) `shouldBe` Left "Input 0: the key 0x03 with the data 00 carries data"

    it "refuse a derivation key that is not a public key" $
      decodePsbt (bytesOf (vectors.invalid !! 10)) `shouldSatisfy` either ("is not a public key" `isSuffixOf`) (const False)

    it "read the base64 text of a PSBT, with spaces and line breaks around it" $ do
      let v = vectors.valid !! 0
      (encodePsbt <$> decodePsbtBase64 (" " <> encodeUtf8 (fromMaybe "" v.vecBase64) <> "\n")) `shouldBe` Right (bytesOf v)

    it "write the base64 text the BIP gives" $ do
      let v = vectors.valid !! 0
      psbt <- psbtOfVector v
      Just (encodePsbtBase64 psbt) `shouldBe` (encodeUtf8 <$> v.vecBase64)

    it "refuse text that is not base64" $
      decodePsbtBase64 "not base64!" `shouldBe` Left "Not the base64 text of a PSBT"

{- Fields -}

-- The literal values below are those Bitcoin Core 25.0 shows for the
-- vectors with decodepsbt.
fieldsSpec :: Ctx -> Vectors -> Spec
fieldsSpec ctx vectors =
  describe "The fields of a PSBT" $ do
    let publicKey text = forceRight (unmarshal ctx (hexBytes text)) :: PublicKey
        shown :: [(PublicKey, Derivation)] -> [(Text, Derivation)]
        shown records = [(encodeHex (marshal ctx key), derivation) | (key, derivation) <- records]

    it "read the derivation records of an input: the public key, the fingerprint of the master key and the path" $ do
      psbt <- psbtOfVector (updater vectors)
      records <- expectRight $ inputDerivations ctx (inputAt 0 psbt)
      shown records
        `shouldBe` [ ("029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f", Derivation (fingerprintOf "d90c6a4f") "m/0'/0'/0'"),
                     ("02dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d7", Derivation (fingerprintOf "d90c6a4f") "m/0'/0'/1'")
                   ]

    it "read the derivation records of an output" $ do
      psbt <- psbtOfVector (updater vectors)
      records <- expectRight $ outputDerivations ctx (outputAt 0 psbt)
      shown records `shouldBe` [("03a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca58771", Derivation (fingerprintOf "d90c6a4f") "m/0'/0'/4'")]

    -- The key of m/44'/0'/0'/0/0 of the test wallet. The path is 44'
    -- (0x8000002c), 0', 0', 0 and 0, each over four bytes, little-endian.
    it "write a derivation record as the public key under the type, and the fingerprint then the path, four bytes an index" $ do
      let key = publicKey "032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9"
      Map.toList (insertInputDerivation ctx key (Derivation testFingerprint "m/44'/0'/0'/0/0") Map.empty)
        `shouldBe` [(PsbtKey 0x06 (hexBytes "032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9"), hexBytes ("bc560f95" <> "2c000080" <> "00000080" <> "00000080" <> "00000000" <> "00000000"))]
      Map.keys (insertOutputDerivation ctx key (Derivation testFingerprint "m/44'/0'/0'/1/0") Map.empty)
        `shouldBe` [PsbtKey 0x02 (hexBytes "032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9")]

    it "read the sighash type of an input, when it has one" $ do
      with <- psbtOfVector (updaterWithSigHash vectors)
      without <- psbtOfVector (updater vectors)
      mapM sigHashType with.psbtInputs `shouldBe` Right [Just sigHashAll, Just sigHashAll]
      mapM sigHashType without.psbtInputs `shouldBe` Right [Nothing, Nothing]

    -- SIGHASH_ALL with the FORKID flag of Bitcoin Cash, 0x41
    it "write the sighash type over four bytes, little-endian" $
      Map.toList (insertSigHashType (setForkIdFlag sigHashAll) Map.empty) `shouldBe` [(PsbtKey 0x03 "", hexBytes "41000000")]

    it "read the whole previous transaction of an input" $ do
      psbt <- psbtOfVector (vectors.valid !! 0)
      parent <- expectRight $ nonWitnessUtxo (inputAt 0 psbt)
      (txHashToHex . txHash <$> parent) `shouldBe` Just "f61b1742ca13176464adb3cb66050c00787bb3a4eead37e985f2df1e37718126"

    it "write a previous transaction under the type 0x00" $ do
      psbt <- psbtOfVector (vectors.valid !! 0)
      parent <- expectRight $ nonWitnessUtxo (inputAt 0 psbt)
      Map.toList (insertNonWitnessUtxo emptyTx Map.empty) `shouldBe` [(PsbtKey 0x00 "", hexBytes "00000000000000000000")]
      nonWitnessUtxo (maybe Map.empty (`insertNonWitnessUtxo` Map.empty) parent) `shouldBe` Right parent

    -- The second input of the vector spends 1 BTC at a P2SH address
    it "read the output an input spends when the PSBT has it alone" $ do
      psbt <- psbtOfVector (vectors.valid !! 1)
      mapM witnessUtxo psbt.psbtInputs `shouldBe` Right [Nothing, Just (TxOut 100000000 (hexBytes "a9143545e6e33b832c47050f24d3eeb93c9c03948bc787"))]

    it "tell an input with partial signatures from one without" $ do
      partial <- psbtOfVector (vectors.valid !! 4)
      unsigned <- psbtOfVector (vectors.valid !! 0)
      hasPartialSignatures (inputAt 0 partial) `shouldBe` True
      hasPartialSignatures (inputAt 0 unsigned) `shouldBe` False

    -- The first signer of the BIP signs the first input with the key
    -- 029583bf..., with SIGHASH_ALL, the last byte 01 of the signature
    it "read the partial signatures of an input: the public key, and the signature with its sighash byte" $ do
      psbt <- psbtOfVector (firstSigner vectors)
      records <- expectRight (partialSignatures ctx (inputAt 0 psbt))
      [(encodeHex (marshal ctx key), encodeHex signature) | (key, signature) <- records]
        `shouldBe` [("029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f", firstSignature)]
      partialSignatures ctx (inputAt 0 (updater vectors `seq` psbt)) `shouldSatisfy` (not . null . either (const []) id)

    it "write a partial signature under the type 0x02 with the public key, and drop them all as a finalizer does" $ do
      let key = forceRight (unmarshal ctx (hexBytes "029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f")) :: PublicKey
          m = insertPartialSignature ctx key (hexBytes firstSignature) (insertSigHashType 1 Map.empty)
      Map.toList m
        `shouldBe` [ (PsbtKey 0x02 (hexBytes "029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f"), hexBytes firstSignature),
                     (PsbtKey 0x03 "", hexBytes "01000000")
                   ]
      Map.toList (deletePartialSignatures m) `shouldBe` [(PsbtKey 0x03 "", hexBytes "01000000")]

    it "drop the sighash type of an input as a finalizer does, and nothing else" $ do
      let m = insertSigHashType 1 (insertNonWitnessUtxo emptyTx Map.empty)
      Map.keys (deleteSigHashType m) `shouldBe` [PsbtKey 0x00 ""]
      deleteSigHashType Map.empty `shouldBe` Map.empty

    it "read the final script and witness of a signed input" $ do
      psbt <- psbtOfVector (finalizer vectors)
      let first' = inputAt 0 psbt
          second = inputAt 1 psbt
      finalScriptSig first'
        `shouldBe` Just (hexBytes "00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae")
      finalScriptWitness first' `shouldBe` Right Nothing
      finalScriptSig second `shouldBe` Just (hexBytes "2200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903")
      finalScriptWitness second
        `shouldBe` Right
          ( Just
              [ "",
                hexBytes "3044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f01",
                hexBytes "3044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d201",
                hexBytes "522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae"
              ]
          )

    it "write a final script under the type 0x07, and a final witness as the number of its items then each after its length" $ do
      Map.toList (insertFinalScriptSig (hexBytes "aabb") Map.empty) `shouldBe` [(PsbtKey 0x07 "", hexBytes "aabb")]
      Map.toList (insertFinalScriptWitness [hexBytes "aabb", ""] Map.empty) `shouldBe` [(PsbtKey 0x08 "", hexBytes "0202aabb00")]

    it "read the extended public keys of the global map, with their derivations" $ do
      psbt <- psbtOfVector (vectors.valid !! 7)
      let xpub = fromMaybe (error "xpub") $ xPubImport btc ctx "xpub6CpGH79LXVkeiux2ZPWMpEubBrRfgcGCgy2HiagyN6NW3qdioJaqFYyD1fG6LDfxWEhMXJqcDuU5VneKt5UQYUGPa5Mfxdw2D2NArwX5TBm"
      xpubRecords btc ctx psbt.psbtGlobals `shouldBe` Right [(xpub, Derivation (fingerprintOf "27569c50") "m/49'/0'/0'")]

    -- The two keys of the vector are of the test network
    it "leave aside the extended public keys of another network" $ do
      psbt <- psbtOfVector (vectors.valid !! 5)
      let tpub text = fromMaybe (error "tpub") $ xPubImport btcTest ctx text
      xpubRecords btc ctx psbt.psbtGlobals `shouldBe` Right []
      xpubRecords btcTest ctx psbt.psbtGlobals
        `shouldBe` Right
          [ (tpub "tpubDBkJeJo2X94Yq3RVz65DoUgyLUkaDrkfyrn2VcgyCRSKCRonvKvCF2FpYDGJWDkdRHBajXJGpc63GnumUt63ySvqCu2XaTRGVTKMYGuFk9H", Derivation (fingerprintOf "d90c6a4f") "m/174'/0'"),
            (tpub "tpubDBkJeJo2X94YsvtBEU1eKoibEWiNv51nW5iHhs6VZp59jsE6nen8KZMFyGHuGbCvqjRqirgeMcfpVBkttpUUT6brm4duzSGoZeTbhqCNUu6", Derivation (fingerprintOf "d90c6a4f") "m/174'/1'")
          ]

    it "write an extended public key under the type 0x01, with its derivation" $ do
      let xpub = testAccountPub ctx
      xpubRecords btc ctx (insertXpubRecord btc ctx xpub (Derivation testFingerprint "m/44'/0'/0'") Map.empty)
        `shouldBe` Right [(xpub, Derivation testFingerprint "m/44'/0'/0'")]

    it "write a proprietary key as the type 0xfc, the identifier after its length, the subtype and the data" $ do
      proprietaryKey (Proprietary "hw" 1 (hexBytes "ab")) `shouldBe` PsbtKey 0xfc (hexBytes "02687701ab")
      proprietaryOf (PsbtKey 0xfc (hexBytes "02687701ab")) `shouldBe` Right (Just (Proprietary "hw" 1 (hexBytes "ab")))
      proprietaryOf (PsbtKey 0x20 "") `shouldBe` Right Nothing
      proprietaryOf (PsbtKey 0xfc "") `shouldBe` Left "the proprietary key 0xfc has no identifier and subtype"

    it "look a proprietary field up by its identifier and subtype" $ do
      let m = insertProprietary "hw" 3 "Rent" Map.empty
      lookupProprietary "hw" 3 m `shouldBe` Just "Rent"
      lookupProprietary "hw" 2 m `shouldBe` Nothing
      lookupProprietary "other" 3 m `shouldBe` Nothing

{- Properties -}

propertiesSpec :: Ctx -> Spec
propertiesSpec ctx =
  describe "Any PSBT" $ do
    prop "is read back as it was written" $
      forAll (arbitraryPsbt btc ctx) $ \psbt -> decodePsbt (encodePsbt psbt) == Right psbt

    prop "is read back from its base64 text" $
      forAll (arbitraryPsbt btc ctx) $ \psbt -> decodePsbtBase64 (encodePsbtBase64 psbt) == Right psbt
