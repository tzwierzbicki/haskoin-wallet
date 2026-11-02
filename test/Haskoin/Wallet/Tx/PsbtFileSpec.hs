{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A transaction file as a PSBT: what hw writes in the fields of the spec
-- and in its own, how it reads a file back, and what it refuses.
module Haskoin.Wallet.Tx.PsbtFileSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Bip174
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.PsbtFile
import Haskoin.Wallet.Tx.SignData
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
knownSeed ctx = FileAccount (testAccountPub ctx) "m/44'/0'/0'" (Just testFingerprint)
unknownSeed ctx = (knownSeed ctx) {fileAccountFingerprint = Nothing}

-- | The first account of the second test wallet
otherWallet :: Ctx -> FileAccount
otherWallet ctx = FileAccount (fromMaybe (error "otherWallet") (xPubImport btc ctx testAccountXPub2)) "m/44'/0'/0'" (Just testFingerprint2)

-- | The file as hw writes it for the account
written :: Ctx -> FileAccount -> TxFile -> IO Psbt
written ctx acc file = expectRight (txFilePsbt ctx (Just acc) file)

-- | The file as hw reads it back for the accounts
readBack :: Ctx -> [FileAccount] -> Psbt -> IO TxFile
readBack ctx accounts psbt = expectRight (psbtTxFile ctx accounts psbt)

-- | The map of the input, or of the output, at the index
inputAt, outputAt :: Int -> Psbt -> PsbtMap
inputAt i psbt = psbt.psbtInputs !! i
outputAt i psbt = psbt.psbtOutputs !! i

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
  readingSpec ctx

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
      psbt.psbtTx `shouldBe` paymentTx ctx
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
      hwField networkField psbt.psbtGlobals `shouldBe` Just "btc"
      hwField feeRateField psbt.psbtGlobals `shouldBe` Just (hexBytes "0a00000000000000")
      hwField replacesField psbt.psbtGlobals
        `shouldBe` Just (hexBytes ("03" <> Text.replicate 62 "0" <> "04" <> Text.replicate 62 "0"))
      hwField labelField psbt.psbtGlobals `shouldBe` Just "Rent of October"

    it "writes no fee rate, replaced transaction or label when the transaction has none" $ do
      let plain = (payment ctx) {txSignDataFeeRate = Nothing}
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx) {txFileData = plain}
      hwField feeRateField psbt.psbtGlobals `shouldBe` Nothing
      hwField replacesField psbt.psbtGlobals `shouldBe` Nothing
      hwField labelField psbt.psbtGlobals `shouldBe` Nothing

    it "writes the account's key in the global map when the seed is known, and none otherwise" $ do
      known <- written ctx (knownSeed ctx) (paymentFile ctx)
      unknown <- written ctx (unknownSeed ctx) (paymentFile ctx)
      xpubRecords btc ctx known.psbtGlobals `shouldBe` Right [(testAccountPub ctx, Derivation testFingerprint "m/44'/0'/0'")]
      xpubRecords btc ctx unknown.psbtGlobals `shouldBe` Right []

    it "writes the labels of the account's addresses on their inputs and outputs, and the label a coin shows on its input" $ do
      let labelled = TxFile (Just btc) (payment ctx) (Map.fromList [("/0/0", "Salary"), ("/1/0", "Change")]) (Map.fromList [(txHash (funding ctx), "From work")])
      psbt <- written ctx (knownSeed ctx) labelled
      hwField addressLabelField (inputAt 0 psbt) `shouldBe` Just "Salary"
      hwField coinLabelField (inputAt 0 psbt) `shouldBe` Just "From work"
      map (hwField addressLabelField) psbt.psbtOutputs `shouldBe` [Nothing, Just "Change"]

    it "writes a sweep outside the wallet without any derivation record" $ do
      let sweep = TxFile (Just btc) (TxSignData (paymentTx ctx) [funding ctx] [] [] (Just 10) Nothing [] noLabel) Map.empty Map.empty
      psbt <- expectRight $ txFilePsbt ctx Nothing sweep
      inputDerivations ctx (inputAt 0 psbt) `shouldBe` Right []
      mapM (outputDerivations ctx) psbt.psbtOutputs `shouldBe` Right [[], []]
      xpubRecords btc ctx psbt.psbtGlobals `shouldBe` Right []

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

    -- Bitcoin Core 25.0 decodes this file as the payment: its one input
    -- with the funding transaction, SIGHASH_ALL and the derivation
    -- m/44'/0'/0'/0/0 of the key 032f64..., its change output with the
    -- derivation m/44'/0'/0'/1/0, a fee of 0.0001, the account's xpub at
    -- m/44'/0'/0' of the master key bc560f95, and two proprietary fields
    -- of the identifier 6877 ("hw"): the network "btc" and the fee rate 10.
    -- analyzepsbt names the signer as the next role. See
    -- test/data/psbt/README.md.
    it "is this file, for the payment of the test wallet" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      encodePsbtBase64 psbt `shouldBe` "cHNidP8BAHcCAAAAARtCW1G6d8iktJ6tfcrZ9/NlRYpWG/bQVr97AifbJolaAAAAAAD9////AmDqAAAAAAAAGXapFLy24qoWpyVllUiRDXoCBBTL/mXfiKwwdQAAAAAAABl2qRRFWqUMJmMUF3HrrCCW43gzkRIn1oisADUMAE8BBIiyHgNVuD70gAAAABK990NGk+UwynxHP7qUG12/zLjt8wEdccnpS/XXP3xXA3XWzkgJoagF3bdbeT7i2QxRLb+Npe8w6ZCTDqjcNI5pELxWD5UsAACAAAAAgAAAAIAF/AJodwADYnRjBfwCaHcBCAoAAAAAAAAAAAEAVQEAAAABAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP////8BoIYBAAAAAAAZdqkUyA1VIj4vEGE2Ot0qewNjwoW/KY2IrAAAAAABAwQBAAAAIgYDL2Q3AiJWyFTgCQ59bItl6WYG739AU5ng8Bh8kwl6wPkYvFYPlSwAAIAAAACAAAAAgAAAAAAAAAAAAAAiAgN28ucGRY3XVnO6Gbjv2ELv3dm8CuQeMTOd/8+Re2226xi8Vg+VLAAAgAAAAIAAAACAAQAAAAAAAAAA"

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
      psbtTxFile ctx [knownSeed ctx] psbt {psbtInputs = [forged]}
        `shouldBe` Left "Input 0 names the key /0/0 of the account, but carries another public key"

    it "refuses an input without its previous transaction" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      psbtTxFile ctx [knownSeed ctx] psbt {psbtInputs = [Map.delete (PsbtKey 0x00 "") (inputAt 0 psbt)]}
        `shouldBe` Left "Input 0 has no previous transaction: hw needs the whole transaction each coin comes from"

    it "refuses a previous transaction that is not the one the input spends" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      psbtTxFile ctx [knownSeed ctx] psbt {psbtInputs = [insertNonWitnessUtxo (fundingTx ctx 2 [(externalAddr 0, 100000)]) (inputAt 0 psbt)]}
        `shouldBe` Left "The previous transaction of input 0 is not the one it spends"

    it "refuses an input with partial signatures that is not finalized" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      let partial = Map.insert (PsbtKey 0x02 (hexBytes "032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9")) "a signature" (inputAt 0 psbt)
      psbtTxFile ctx [knownSeed ctx] psbt {psbtInputs = [partial]}
        `shouldBe` Left "Input 0 carries partial signatures and is not finalized: finalize it with the software that signed it"

    it "names no network when the file has no network field" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      file <- readBack ctx [knownSeed ctx] psbt {psbtGlobals = Map.delete (proprietaryKey (Proprietary hwIdentifier networkField "")) psbt.psbtGlobals}
      file.txFileNetwork `shouldBe` Nothing

    it "refuses a network hw does not know" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      psbtTxFile ctx [knownSeed ctx] psbt {psbtGlobals = insertProprietary hwIdentifier networkField "doge" psbt.psbtGlobals}
        `shouldBe` Left "Unknown network doge"

    it "refuses a fee rate that is not a 64-bit integer, transactions replaced that are not ids, and a label that is not text" $ do
      psbt <- written ctx (knownSeed ctx) (paymentFile ctx)
      let withGlobal subtype value = psbt {psbtGlobals = insertProprietary hwIdentifier subtype value psbt.psbtGlobals}
      psbtTxFile ctx [knownSeed ctx] (withGlobal feeRateField (hexBytes "0a")) `shouldBe` Left "The fee rate of the file is not a 64-bit integer"
      psbtTxFile ctx [knownSeed ctx] (withGlobal replacesField (hexBytes "0300")) `shouldBe` Left "The transactions replaced are not a list of transaction ids"
      psbtTxFile ctx [knownSeed ctx] (withGlobal labelField (hexBytes "ff")) `shouldBe` Left "The label of the transaction is not UTF-8 text"
  where
    -- A map without its derivation records
    withoutDerivations = Map.filterWithKey (\key _ -> key.psbtKeyType /= 0x06)
