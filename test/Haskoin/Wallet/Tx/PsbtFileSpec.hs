{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A transaction file as a PSBT: what hw writes in the fields of the spec
-- and in its own, how it reads a file back, and what it refuses.
module Haskoin.Wallet.Tx.PsbtFileSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.PsbtFile
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (parseOutsideTxSignData, parseTxFile, signWalletTx, verifySignedTx)
import System.FilePath ((</>))
import Test.Hspec

{- Fixtures -}

-- | The transaction funding the wallet's coin of 100000 satoshis at its
-- first receiving address, /0/0
funding :: Ctx -> Tx
funding ctx = fundingTx ctx 1 [(externalAddr 0, 100000)]

-- | The payment of the tests: the coin of 'funding', spent to another
-- wallet (60000) and back to the first change address of the account, /1/0
-- (30000), with a fee of 10000, as hw builds it with the lock time 800000
paymentTx :: Ctx -> Tx
paymentTx ctx = builtTx ctx 800000 [(txHash (funding ctx), 0)] [(otherAddr 0, 60000), (internalAddr 0, 30000)]

-- | The signing data of the payment, built at 10 satoshis per byte
payment :: Ctx -> TxSignData
payment ctx = TxSignData (paymentTx ctx) [funding ctx] ["/0/0"] ["/1/0"] (Just 10) Nothing [] noLabel

-- | The file of the payment on Bitcoin, without labels
paymentFile :: Ctx -> TxFile
paymentFile ctx = TxFile (Just btc) (payment ctx) Map.empty Map.empty

-- | The first account of the test wallet, m/44'/0'/0', with the fingerprint
-- of its seed known, and with it unknown
knownSeed, unknownSeed :: Ctx -> FileAccount
knownSeed ctx = FileAccount (testAccountPub ctx) "m/44'/0'/0'" (Just testFingerprint) btc
unknownSeed ctx = (knownSeed ctx) {fileAccountFingerprint = Nothing}

-- | The first account of the second test wallet
otherWallet :: Ctx -> FileAccount
otherWallet ctx = FileAccount (fromMaybe (error "otherWallet") (xPubImport btc ctx testAccountXPub2)) "m/44'/0'/0'" (Just testFingerprint2) btc

-- | The file as hw writes it for the account
written :: Ctx -> FileAccount -> TxFile -> IO Psbt
written ctx acc file = expectRight (txFilePsbt ctx (Just acc) file)

-- | The file as hw reads it back for the accounts
readBack :: Ctx -> [FileAccount] -> Psbt -> IO TxFile
readBack ctx accounts psbt = expectRight (psbtTxFile ctx accounts psbt)

-- | A file Bitcoin Core wrote, in test/data/psbt/core, in base64 text
coreFile :: FilePath -> IO Psbt
coreFile name = expectRight . decodePsbtBase64 =<< BS.readFile ("test/data/psbt/core" </> name)

-- | The map of the input, or of the output, at the index
inputAt, outputAt :: Int -> Psbt -> PsbtMap
inputAt i psbt = psbt.inputs !! i
outputAt i psbt = psbt.outputs !! i

-- | The PSBT with the maps of its inputs, or its global map, replaced
withInputs :: [PsbtMap] -> Psbt -> Psbt
withInputs inputs psbt = Psbt psbt.unsignedTx psbt.globals inputs psbt.outputs

withGlobals :: PsbtMap -> Psbt -> Psbt
withGlobals globals psbt = Psbt psbt.unsignedTx globals psbt.inputs psbt.outputs

-- | The derivation records of a map, the keys shown in hexadecimal
shownDerivations :: Ctx -> (PsbtMap -> Either String [(PublicKey, Derivation)]) -> PsbtMap -> IO [(Text, Derivation)]
shownDerivations ctx derivations m = do
  records <- expectRight (derivations m)
  return [(encodeHex (marshal ctx key), derivation) | (key, derivation) <- records]

-- | The field of hw with the subtype in a map
hwField :: Integral a => a -> PsbtMap -> Maybe BS.ByteString
hwField subtype = lookupProprietary hwIdentifier (fromIntegral subtype)

spec :: Spec
spec = prepareContext $ \ctx -> do
  writingSpec ctx
  signingIntoSpec ctx
  readingSpec ctx

-- | The payment signed by the test wallet, naming its seed, as signtx
-- hands it to the file
signedWithSeed :: Ctx -> TxSignData
signedWithSeed ctx = (signedByWallet ctx (payment ctx)) {txSignDataFingerprint = Just testFingerprint}

-- | The file hw writes for the unsigned payment, with the seed known:
-- the one Bitcoin Core signed for the vectors in test/data/psbt/core
unsignedPaymentText :: BS.ByteString
unsignedPaymentText = "cHNidP8BAHcCAAAAARtCW1G6d8iktJ6tfcrZ9/NlRYpWG/bQVr97AifbJolaAAAAAAD9////AmDqAAAAAAAAGXapFLy24qoWpyVllUiRDXoCBBTL/mXfiKwwdQAAAAAAABl2qRRFWqUMJmMUF3HrrCCW43gzkRIn1oisADUMAAX8Amh3AANidGMF/AJodwEICgAAAAAAAAAAAQBVAQAAAAEBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/////wGghgEAAAAAABl2qRTIDVUiPi8QYTY63Sp7A2PChb8pjYisAAAAAAEDBAEAAAAiBgMvZDcCIlbIVOAJDn1si2XpZgbvf0BTmeDwGHyTCXrA+Ri8Vg+VLAAAgAAAAIAAAACAAAAAAAAAAAAAACICA3by5wZFjddWc7oZuO/YQu/d2bwK5B4xM53/z5F7bbbrGLxWD5UsAACAAAAAgAAAAIABAAAAAAAAAAA="

{- Writing -}

-- The public keys below are those of the test wallet at m/44'/0'/0'/0/0 and
-- m/44'/0'/0'/1/0, derived apart from hw with the Python tool of the
-- message-signing vectors. 892eb8e4 is the fingerprint of the account's own
-- key, derived the same way.
writingSpec :: Ctx -> Spec
writingSpec ctx =
  describe "The PSBT of a transaction file" $ do
    it "names the key of the account's input by the fingerprint of the seed and the full path, when the wallet knows the seed" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      records <- shownDerivations ctx (inputDerivations ctx) (inputAt 0 psbt)
      records `shouldBe` [("032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9", Derivation testFingerprint "m/44'/0'/0'/0/0")]

    it "names it by the fingerprint of the account's own key and the path from it, when the seed is unknown" $ do
      psbt <- written ctx (unknownSeed ctx) (paymentFile ctx)
      records <- shownDerivations ctx (inputDerivations ctx) (inputAt 0 psbt)
      records `shouldBe` [("032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9", Derivation (fingerprintOf "892eb8e4") "/0/0")]

    -- The signer names the seed in the signing data: the wallet that holds
    -- the account need not know it
    it "names it by the seed the signing data names, which the signer gives" $ do
      let file = (paymentFile ctx) {txFileData = (payment ctx) {txSignDataFingerprint = Just testFingerprint}}
      psbt <- written ctx (unknownSeed ctx) file
      records <- shownDerivations ctx (inputDerivations ctx) (inputAt 0 psbt)
      records `shouldBe` [("032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9", Derivation testFingerprint "m/44'/0'/0'/0/0")]

    it "names the key of the account's change output the same way, and no key of another wallet's output" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      shownDerivations ctx (outputDerivations ctx) (outputAt 0 psbt) `shouldReturn` []
      shownDerivations ctx (outputDerivations ctx) (outputAt 1 psbt) `shouldReturn` [("0376f2e706458dd75673ba19b8efd842efddd9bc0ae41e31339dffcf917b6db6eb", Derivation testFingerprint "m/44'/0'/0'/1/0")]

    it "writes the whole transaction each coin comes from on its input" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      nonWitnessUtxo (inputAt 0 psbt) `shouldBe` Right (Just (funding ctx))

    it "writes SIGHASH_ALL on an unsigned input, with the FORKID flag on Bitcoin Cash" $ do
      bitcoin <- written ctx (knownSeed ctx) (paymentFile ctx)
      cash <- written ctx (knownSeed ctx) (paymentFile ctx) {txFileNetwork = Just bch}
      sigHashType (inputAt 0 bitcoin) `shouldBe` Right (Just 0x01)
      sigHashType (inputAt 0 cash) `shouldBe` Right (Just 0x41)

    it "writes a signed transaction as the unsigned one, with the final script of each signed input" $ do
      let signed = signedByWallet ctx (payment ctx)
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx) {txFileData = signed}
      psbt.unsignedTx `shouldBe` paymentTx ctx
      finalScriptSig (inputAt 0 psbt) `shouldBe` Just (signed.txSignDataTx.inputs !! 0).script
      finalScriptWitness (inputAt 0 psbt) `shouldBe` Right Nothing
      psbtTransaction psbt `shouldBe` Right signed.txSignDataTx

    it "keeps the derivation record of a signed input, and drops its sighash type" $ do
      let signed = signedByWallet ctx (payment ctx)
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx) {txFileData = signed}
      records <- shownDerivations ctx (inputDerivations ctx) (inputAt 0 psbt)
      map fst records `shouldBe` ["032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9"]
      sigHashType (inputAt 0 psbt) `shouldBe` Right Nothing

    -- The fee rate 10 over eight bytes, little-endian; the ids of the
    -- transactions replaced, 32 bytes each
    it "writes the network, the fee rate, the transactions replaced and the label in the fields of hw" $ do
      let replacement = (payment ctx) {txSignDataReplaces = [fakeTxHash 3, fakeTxHash 4], txSignDataLabel = "Rent of October"}
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx) {txFileData = replacement}
      hwField networkField psbt.globals `shouldBe` Just "btc"
      hwField feeRateField psbt.globals `shouldBe` Just (hexBytes "0a00000000000000")
      hwField replacesField psbt.globals
        `shouldBe` Just (hexBytes ("03" <> Text.replicate 62 "0" <> "04" <> Text.replicate 62 "0"))
      hwField labelField psbt.globals `shouldBe` Just "Rent of October"

    it "writes no fee rate, replaced transaction or label when the transaction has none" $ do
      let plain = (payment ctx) {txSignDataFeeRate = Nothing}
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx) {txFileData = plain}
      hwField feeRateField psbt.globals `shouldBe` Nothing
      hwField replacesField psbt.globals `shouldBe` Nothing
      hwField labelField psbt.globals `shouldBe` Nothing

    -- The file names no key of the account beyond those of its addresses
    -- in the transaction: the user chose not to put the account's key in it
    it "writes no key of the account in the global map, whether the seed is known or not" $ do
      known <- written ctx (knownSeed ctx) (paymentFile ctx)
      unknown <- written ctx (unknownSeed ctx) (paymentFile ctx)
      xpubRecords btc ctx known.globals `shouldBe` Right []
      xpubRecords btc ctx unknown.globals `shouldBe` Right []
      Map.keys known.globals `shouldBe` [proprietaryKey (Proprietary "hw" 0 ""), proprietaryKey (Proprietary "hw" 1 "")]

    it "writes the labels of the account's addresses on their inputs and outputs, and the label a coin shows on its input" $ do
      let labelled = TxFile (Just btc) (payment ctx) (Map.fromList [("/0/0", "Salary"), ("/1/0", "Change")]) (Map.fromList [(txHash (funding ctx), "From work")])
      psbt <- written ctx (knownSeed ctx) labelled
      hwField addressLabelField (inputAt 0 psbt) `shouldBe` Just "Salary"
      hwField coinLabelField (inputAt 0 psbt) `shouldBe` Just "From work"
      map (hwField addressLabelField) psbt.outputs `shouldBe` [Nothing, Just "Change"]

    it "writes a sweep outside the wallet without any derivation record" $ do
      let sweep = TxFile (Just btc) (TxSignData (paymentTx ctx) [funding ctx] [] [] (Just 10) Nothing [] noLabel) Map.empty Map.empty
      psbt <- expectRight $ txFilePsbt ctx Nothing sweep
      inputDerivations ctx (inputAt 0 psbt) `shouldBe` Right []
      mapM (outputDerivations ctx) psbt.outputs `shouldBe` Right [[], []]
      xpubRecords btc ctx psbt.globals `shouldBe` Right []

    it "refuses a file that names no network" $
      txFilePsbt ctx (Just (knownSeed ctx)) (paymentFile ctx) {txFileNetwork = Nothing} `shouldBe` Left "The file names no network"

    it "refuses paths without an account to derive them from" $
      txFilePsbt ctx Nothing (paymentFile ctx) `shouldBe` Left "The file has paths of an account, and no account to derive them from"

    -- /0/1 is the second receiving address, which the payment does not spend from
    it "refuses a path of no input of the transaction" $ do
      let file = (paymentFile ctx) {txFileData = (payment ctx) {txSignDataInputPaths = ["/0/1"]}}
      txFilePsbt ctx (Just (knownSeed ctx)) file `shouldBe` Left "The path /0/1 is of no input of the transaction"

    it "refuses a coin whose transaction is missing" $ do
      let file = (paymentFile ctx) {txFileData = (payment ctx) {txSignDataInputs = []}}
      txFilePsbt ctx (Just (knownSeed ctx)) file `shouldBe` Left "The transaction input 0 spends from is missing"

    -- Bitcoin Core decodes this file as the payment: its one input
    -- with the funding transaction, SIGHASH_ALL and the derivation
    -- m/44'/0'/0'/0/0 of the key 032f64... from the master key bc560f95,
    -- its change output with the derivation m/44'/0'/0'/1/0, a fee of
    -- 0.0001, no global xpub, and two proprietary fields of the identifier
    -- 6877 ("hw"): the network "btc" and the fee rate 10. analyzepsbt
    -- names the signer as the next role. See test/data/psbt/README.md.
    it "is this file, for the payment of the test wallet" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      encodePsbtBase64 psbt `shouldBe` unsignedPaymentText

{- Signing into a file -}

signingIntoSpec :: Ctx -> Spec
signingIntoSpec ctx =
  describe "The PSBT of a transaction signed into the PSBT it was read from" $ do
    -- The property the two writers share: for a file hw wrote, signing
    -- into it gives the file hw writes for the signed transaction
    it "is the file hw writes for the signed transaction, when the file was hw's own" $ do
      let signed = signedWithSeed ctx
      forM_ [knownSeed ctx, unknownSeed ctx] $ \acc -> do
        source <- written ctx acc (paymentFile ctx)
        fromScratch <- written ctx acc (paymentFile ctx) {txFileData = signed}
        signedPsbt ctx (Just acc) source (paymentFile ctx) {txFileData = signed} `shouldBe` Right fromScratch

    it "keeps every field of the file it did not write: a key hw does not know in each map, and a field of another wallet" $ do
      source <- written ctx (knownSeed ctx) (paymentFile ctx)
      let foreign' = Map.insert (Key 0x20 "") "global" . insertProprietary "other" 7 "theirs"
          withForeign = Psbt source.unsignedTx (foreign' source.globals) (foreign' <$> source.inputs) (foreign' <$> source.outputs)
      signed <- expectRight $ signedPsbt ctx (Just (knownSeed ctx)) withForeign (paymentFile ctx) {txFileData = signedWithSeed ctx}
      Map.lookup (Key 0x20 "") signed.globals `shouldBe` Just "global"
      lookupProprietary "other" 7 signed.globals `shouldBe` Just "theirs"
      map (Map.lookup (Key 0x20 "")) signed.inputs `shouldBe` [Just "global"]
      map (lookupProprietary "other" 7) signed.outputs `shouldBe` [Just "theirs", Just "theirs"]

    it "writes the final script of a signed input, and drops its sighash type and its partial signatures" $ do
      source <- written ctx (knownSeed ctx) (paymentFile ctx)
      let key = forceRight (unmarshal ctx (hexBytes "032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9")) :: PublicKey
          withPartial = withInputs [insertPartialSignature ctx key "a signature of another signer" (inputAt 0 source)] source
          signedTsd = signedWithSeed ctx
      signed <- expectRight $ signedPsbt ctx (Just (knownSeed ctx)) withPartial (paymentFile ctx) {txFileData = signedTsd}
      finalScriptSig (inputAt 0 signed) `shouldBe` Just (signedTsd.txSignDataTx.inputs !! 0).script
      sigHashType (inputAt 0 signed) `shouldBe` Right Nothing
      hasPartialSignatures (inputAt 0 signed) `shouldBe` False
      nonWitnessUtxo (inputAt 0 signed) `shouldBe` Right (Just (funding ctx))

    it "names the keys of the account by the seed the signer names, where the file named them by the account's own key" $ do
      source <- written ctx (unknownSeed ctx) (paymentFile ctx)
      signed <- expectRight $ signedPsbt ctx (Just (unknownSeed ctx)) source (paymentFile ctx) {txFileData = signedWithSeed ctx}
      records <- shownDerivations ctx (inputDerivations ctx) (inputAt 0 signed)
      records `shouldBe` [("032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9", Derivation testFingerprint "m/44'/0'/0'/0/0")]
      change <- shownDerivations ctx (outputDerivations ctx) (outputAt 1 signed)
      change `shouldBe` [("0376f2e706458dd75673ba19b8efd842efddd9bc0ae41e31339dffcf917b6db6eb", Derivation testFingerprint "m/44'/0'/0'/1/0")]

    it "names the network in the field of hw when the file has none, and leaves the field as it is otherwise" $ do
      source <- written ctx (knownSeed ctx) (paymentFile ctx)
      let withoutNetwork = withGlobals (Map.delete (proprietaryKey (Proprietary hwIdentifier networkField "")) source.globals) source
          signedOn net = signedPsbt ctx (Just (knownSeed ctx)) withoutNetwork (TxFile net (signedWithSeed ctx) Map.empty Map.empty)
      (hwField networkField . (.globals) <$> signedOn (Just btc)) `shouldBe` Right (Just "btc")
      (hwField networkField . (.globals) <$> signedOn Nothing) `shouldBe` Right Nothing
      onCash <- expectRight $ signedPsbt ctx (Just (knownSeed ctx)) source (TxFile (Just bch) (signedWithSeed ctx) Map.empty Map.empty)
      hwField networkField onCash.globals `shouldBe` Just "btc"

    it "refuses a transaction that is not the one of the file" $ do
      source <- written ctx (knownSeed ctx) (paymentFile ctx)
      let otherTx = builtTx ctx 800001 [(txHash (funding ctx), 0)] [(otherAddr 0, 60000), (internalAddr 0, 30000)]
          other = (signedByWallet ctx (payment ctx) {txSignDataTx = otherTx}) {txSignDataFingerprint = Just testFingerprint}
      signedPsbt ctx (Just (knownSeed ctx)) source (paymentFile ctx) {txFileData = other}
        `shouldBe` Left "The transaction signed is not the one of the file"

{- Reading -}

readingSpec :: Ctx -> Spec
readingSpec ctx =
  describe "A transaction file read from a PSBT" $ do
    it "is the file written, with the fingerprint of the seed the records carry" $ do
      let labelled = TxFile (Just btc) (payment ctx) {txSignDataReplaces = [fakeTxHash 3], txSignDataLabel = "Rent"} (Map.fromList [("/0/0", "Salary")]) (Map.fromList [(txHash (funding ctx), "From work")])
      file <- readBack ctx [knownSeed ctx] =<< written ctx (knownSeed ctx) labelled
      file `shouldBe` labelled {txFileData = labelled.txFileData {txSignDataFingerprint = Just testFingerprint}}

    it "is the file written, as is, when the records name the account's own key" $ do
      file <- readBack ctx [unknownSeed ctx] =<< written ctx (unknownSeed ctx) (paymentFile ctx)
      file `shouldBe` paymentFile ctx

    it "is signed when the file was written signed" $ do
      let signed = (paymentFile ctx) {txFileData = signedByWallet ctx (payment ctx)}
      file <- readBack ctx [knownSeed ctx] =<< written ctx (knownSeed ctx) signed
      isSigned file.txFileData.txSignDataTx `shouldBe` True
      file.txFileData.txSignDataTx `shouldBe` signed.txFileData.txSignDataTx
      file.txFileData.txSignDataInputPaths `shouldBe` ["/0/0"]

    it "finds the account among several by the keys its records name" $ do
      file <- readBack ctx [otherWallet ctx, knownSeed ctx] =<< written ctx (knownSeed ctx) (paymentFile ctx)
      file.txFileData.txSignDataInputPaths `shouldBe` ["/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` ["/1/0"]

    it "tells an account that did not know its seed the fingerprint the records carry" $ do
      file <- readBack ctx [unknownSeed ctx] =<< written ctx (knownSeed ctx) (paymentFile ctx)
      file.txFileData.txSignDataInputPaths `shouldBe` ["/0/0"]
      file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint

    it "leaves aside the records of another wallet, and reads the transaction as outside the wallet" $ do
      file <- readBack ctx [otherWallet ctx] =<< written ctx (knownSeed ctx) (paymentFile ctx)
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` []
      file.txFileData.txSignDataFingerprint `shouldBe` Nothing
      file.txFilePathLabels `shouldBe` Map.empty

    -- The record of the second wallet's key, claiming the path of the
    -- first wallet's /0/0: the key of the second wallet at m/44'/0'/0'/0/0
    it "refuses a record that names a key of the account but carries another public key" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      let other = forceRight (unmarshal ctx (hexBytes "02b31089d88f43ba74b787dad983c54486772e2d5c7b432e1bbf16591d698912c8")) :: PublicKey
          forged = insertInputDerivation ctx other (Derivation testFingerprint "m/44'/0'/0'/0/0") (Map.filter (const False) (inputAt 0 psbt) `Map.union` withoutDerivations (inputAt 0 psbt))
      psbtTxFile ctx [knownSeed ctx] (withInputs [forged] psbt)
        `shouldBe` Left "Input 0 names the key /0/0 of the account, but carries another public key"

    it "refuses an input without its previous transaction" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      psbtTxFile ctx [knownSeed ctx] (withInputs [Map.delete (Key 0x00 "") (inputAt 0 psbt)] psbt)
        `shouldBe` Left "Input 0 has no previous transaction: hw needs the whole transaction each coin comes from"

    it "refuses a previous transaction that is not the one the input spends" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      psbtTxFile ctx [knownSeed ctx] (withInputs [insertNonWitnessUtxo (fundingTx ctx 2 [(externalAddr 0, 100000)]) (inputAt 0 psbt)] psbt)
        `shouldBe` Left "The previous transaction of input 0 is not the one it spends"

    -- Bitcoin Core signed the file hw writes for the payment without
    -- finalizing it, as a hardware wallet does: see test/data/psbt/core.
    -- The final script is the one Core builds when it finalizes the same
    -- file: the signature, then the key.
    it "finalizes an input another signer left with a partial signature of the coin's key, as Core finalizes it" $ do
      psbt <- coreFile "hw-payment-partial.psbt"
      file <- readBack ctx [knownSeed ctx] psbt
      let input = file.txFileData.txSignDataTx.inputs !! 0
      encodeHex input.script `shouldBe` "473044022074f111ce88a136dea1259050e9a564a9a021d07e5bc9e35710f4768da5b1b9a502200fbdf1799186f1bb53be566c1f529be81f44d4c461521d9ef5f1495a951f24b70121032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9"
      isSigned file.txFileData.txSignDataTx `shouldBe` True
      file.txFileData.txSignDataInputPaths `shouldBe` ["/0/0"]
      file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint
      verifySignedTx btc ctx file.txFileData `shouldBe` Right ()

    -- The same file signed and finalized by Core, which clears the
    -- derivation records of the input it finalized: the account is found
    -- by its change output. The transaction is the one Core's finalizepsbt
    -- gives.
    it "reads the transaction of an input another signer finalized, which names no key of the account any more" $ do
      psbt <- coreFile "hw-payment-finalized.psbt"
      file <- readBack ctx [knownSeed ctx] psbt
      encodeHex (S.encode file.txFileData.txSignDataTx)
        `shouldBe` "02000000011b425b51ba77c8a4b49ead7dcad9f7f365458a561bf6d056bf7b0227db26895a000000006a473044022074f111ce88a136dea1259050e9a564a9a021d07e5bc9e35710f4768da5b1b9a502200fbdf1799186f1bb53be566c1f529be81f44d4c461521d9ef5f1495a951f24b70121032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9fdffffff0260ea0000000000001976a914bcb6e2aa16a725659548910d7a020414cbfe65df88ac30750000000000001976a914455aa50c2663141771ebac2096e37833911227d688ac00350c00"
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` ["/1/0"]
      file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint
      file.txFileData.txSignDataFeeRate `shouldBe` Just 10
      verifySignedTx btc ctx file.txFileData `shouldBe` Right ()

    -- The key of the second wallet at m/44'/0'/0'/0/0 is not the coin's
    it "leaves an input with a partial signature of another key unsigned" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      let other = forceRight (unmarshal ctx (hexBytes "02b31089d88f43ba74b787dad983c54486772e2d5c7b432e1bbf16591d698912c8")) :: PublicKey
          partial = insertPartialSignature ctx other "a signature" (inputAt 0 psbt)
      file <- readBack ctx [knownSeed ctx] (withInputs [partial] psbt)
      isSigned file.txFileData.txSignDataTx `shouldBe` False
      file.txFileData.txSignDataTx `shouldBe` paymentTx ctx

    it "refuses an input to be signed with a sighash hw does not sign with, naming both" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      let asking sh = withInputs [insertSigHashType sh (inputAt 0 psbt)] psbt
      psbtTxFile ctx [knownSeed ctx] (asking 0x02) `shouldBe` Left "Input 0 is to be signed with the sighash NONE: hw signs with ALL on btc"
      psbtTxFile ctx [knownSeed ctx] (asking 0x83) `shouldBe` Left "Input 0 is to be signed with the sighash SINGLE|ANYONECANPAY: hw signs with ALL on btc"
      psbtTxFile ctx [knownSeed ctx] (asking 0x41) `shouldBe` Left "Input 0 is to be signed with the sighash ALL|FORKID: hw signs with ALL on btc"

    it "accepts ALL with FORKID for an account on Bitcoin Cash, and an input without a sighash" $ do
      let cash = (knownSeed ctx) {fileAccountNetwork = bch}
      psbt <- written ctx cash (paymentFile ctx) {txFileNetwork = Just bch}
      sigHashType (inputAt 0 psbt) `shouldBe` Right (Just 0x41)
      file <- readBack ctx [cash] psbt
      file.txFileData.txSignDataInputPaths `shouldBe` ["/0/0"]
      psbtTxFile ctx [knownSeed ctx] psbt `shouldBe` Left "Input 0 is to be signed with the sighash ALL|FORKID: hw signs with ALL on btc"
      without <- readBack ctx [knownSeed ctx] (withInputs [deleteSigHashType (inputAt 0 psbt)] psbt)
      without.txFileData.txSignDataInputPaths `shouldBe` ["/0/0"]

    it "refuses, for a transaction outside the wallet, a sighash other than ALL, with FORKID only on the network the file names" $ do
      let sweep = TxFile (Just btc) (TxSignData (paymentTx ctx) [funding ctx] [] [] (Just 10) Nothing [] noLabel) Map.empty Map.empty
      psbt <- expectRight $ txFilePsbt ctx Nothing sweep
      let asking sh = withInputs [insertSigHashType sh (inputAt 0 psbt)] psbt
          unnamed p = withGlobals (Map.delete (proprietaryKey (Proprietary hwIdentifier networkField "")) p.globals) p
      psbtTxFile ctx [] (asking 0x41) `shouldBe` Left "Input 0 is to be signed with the sighash ALL|FORKID: hw signs with ALL on btc"
      psbtTxFile ctx [] (unnamed (asking 0x02)) `shouldBe` Left "Input 0 is to be signed with the sighash NONE: hw signs with ALL, or ALL|FORKID on Bitcoin Cash"
      (txFileNetwork <$> psbtTxFile ctx [] (unnamed (asking 0x41))) `shouldBe` Right Nothing

    -- Bitcoin Core prepared this payment for the account, which it
    -- watches by the descriptor of the account's key: 1 BTC from the coin
    -- of 50 BTC mined to /0/0 on regtest, to another wallet, with the
    -- change at /1/0 and a fee of 2250 satoshis. Its records name the keys
    -- by the seed. It carries no field of hw: no network, no fee rate.
    it "reads a file Bitcoin Core prepared for the account, and tells the account the seed" $ do
      psbt <- coreFile "core-prepared.psbt"
      forM_ [knownSeed ctx, unknownSeed ctx] $ \acc -> do
        file <- readBack ctx [otherWallet ctx, acc] psbt
        file.txFileNetwork `shouldBe` Nothing
        file.txFileData.txSignDataInputPaths `shouldBe` ["/0/0"]
        file.txFileData.txSignDataOutputPaths `shouldBe` ["/1/0"]
        file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint
        file.txFileData.txSignDataFeeRate `shouldBe` Nothing
        file.txFileData.txSignDataReplaces `shouldBe` []
        file.txFileData.txSignDataLabel `shouldBe` ""
        map txHash file.txFileData.txSignDataInputs `shouldBe` ["fede72ccadbf9f16f826e3078578b0660526a37b48d4f894ab26f4cf5a95e89c"]
        info <- expectRight $ parseTxFile btc ctx (testAccountPub ctx) file
        info.txInfoSummary.summaryFee `shouldBe` 2250
        info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(testAddress "1GfJmWwG2JUjrvQ6WjVfwaWpPgtbHDhSdE", 100000000)]
        Map.keys info.txInfoSummary.summaryMyOutputs `shouldBe` [internalAddr 0]
        Map.keys info.txInfoSummary.summaryMyInputs `shouldBe` [externalAddr 0]

    -- hw signs it with the key of the test wallet and writes the signatures
    -- into Core's file. Bitcoin Core finds this file complete with
    -- analyzepsbt, extracts the transaction with finalizepsbt, and accepts
    -- it with testmempoolaccept on the regtest chain the coin is on.
    it "signs a file Bitcoin Core prepared into that file, as the file Core accepts" $ do
      psbt <- coreFile "core-prepared.psbt"
      file <- readBack ctx [knownSeed ctx] psbt
      (signed, _) <- expectRight $ signWalletTx btc ctx file.txFileData testAccountKey
      written' <- expectRight $ signedPsbt ctx (Just (knownSeed ctx)) psbt (TxFile (Just btc) signed Map.empty Map.empty)
      encodePsbtBase64 written' `shouldBe` corePreparedSignedText

    -- Bitcoin Core prepared this payment of 2.5 BTC of another wallet
    -- of its own, with the change at a second address of that wallet and a
    -- fee of 2250 satoshis
    it "reads a file Bitcoin Core prepared for another wallet as outside the wallet" $ do
      psbt <- coreFile "core-outside.psbt"
      file <- readBack ctx [knownSeed ctx, otherWallet ctx] psbt
      file.txFileNetwork `shouldBe` Nothing
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` []
      file.txFileData.txSignDataFingerprint `shouldBe` Nothing
      info <- expectRight $ parseOutsideTxSignData btc ctx file.txFileData
      info.txInfoSummary.summaryFee `shouldBe` 2250
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(testAddress "13tJFzYnF13gRMh6Z6eb44mLD9e9R8sHeZ", 250000000), (testAddress "1588dPBmgVnvS2N3BE5YHXUqXcb468zpsz", 4749997750)]
      Map.keys info.txInfoSummary.summaryOtherInputs `shouldBe` [testAddress "157L9V7wdL7pYt2nndN5NN5DMynh1HdYhF"]

    it "names no network when the file has no network field" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      file <- readBack ctx [knownSeed ctx] (withGlobals (Map.delete (proprietaryKey (Proprietary hwIdentifier networkField "")) psbt.globals) psbt)
      file.txFileNetwork `shouldBe` Nothing

    it "refuses a network hw does not know" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      psbtTxFile ctx [knownSeed ctx] (withGlobals (insertProprietary hwIdentifier networkField "doge" psbt.globals) psbt)
        `shouldBe` Left "Unknown network doge"

    it "refuses a fee rate that is not a 64-bit integer, transactions replaced that are not ids, and a label that is not text" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      let withGlobal subtype value = withGlobals (insertProprietary hwIdentifier subtype value psbt.globals) psbt
      psbtTxFile ctx [knownSeed ctx] (withGlobal feeRateField (hexBytes "0a")) `shouldBe` Left "The fee rate of the file is not a 64-bit integer"
      psbtTxFile ctx [knownSeed ctx] (withGlobal replacesField (hexBytes "0300")) `shouldBe` Left "The transactions replaced are not a list of transaction ids"
      psbtTxFile ctx [knownSeed ctx] (withGlobal labelField (hexBytes "ff")) `shouldBe` Left "The label of the transaction is not UTF-8 text"
  where
    -- A map without its derivation records
    withoutDerivations = Map.filterWithKey (\key _ -> key.keyType /= 0x06)
