{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A transaction file as a PSBT: what hw writes in the fields of the spec
-- and in its own, how it reads a file back, and what it refuses.
module Haskoin.Wallet.Tx.PsbtFileSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as Map
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Database.TxInfo (MyOutputs (..), TxInfo (..), TxSummary (..))
import Haskoin.Wallet.Keys (AccountKind (..), legacyPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.PsbtFile
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (noneHeld, parseOutsideTxSignData, parseTxFile, signWalletTx, verifySignedTx)
import Haskoin.Wallet.Tx.Sweep (signTxWithKeys)
import Haskoin.Wallet.Tx.SweepSpec (sweptKey)
import System.FilePath ((</>))
import Test.Hspec

{- Fixtures -}

-- | The transaction funding the wallet's coin of 100000 satoshis at its
-- first receiving address, /0/0
funding :: Tx
funding = fundingTx 1 [(externalAddr 0, 100000)]

-- | The payment of the tests: the coin of 'funding', spent to another
-- wallet (60000) and back to the first change address of the account, /1/0
-- (30000), with a fee of 10000, as hw builds it with the lock time 800000
paymentTx :: Tx
paymentTx = builtTx 800000 [(txHash funding, 0)] [(otherAddr 0, 60000), (internalAddr 0, 30000)]

-- | The signing data of the payment, built at 10 satoshis per byte
payment :: TxSignData
payment = TxSignData paymentTx [funding] [legacyPath "/0/0"] [legacyPath "/1/0"] (Just 10) Nothing [] noLabel

-- | The file of the payment on Bitcoin, without labels
paymentFile :: TxFile
paymentFile = TxFile (Just btc) payment Map.empty Map.empty

-- | The same payment by the bip84 account of the test wallet: its coin of
-- 100000 at its first receiving address, /0/0, spent to another wallet
-- (60000) and to its first change address, /1/0 (30000), 10000 of fee
segwitFunding :: Tx
segwitFunding = fundingTx 2 [(segwitExternalAddr 0, 100000)]

segwitPayment :: TxSignData
segwitPayment =
  TxSignData (builtTx 800000 [(txHash segwitFunding, 0)] [(otherAddr 0, 60000), (segwitInternalAddr 0, 30000)]) [segwitFunding] [segwitPath "/0/0"] [segwitPath "/1/0"] (Just 10) Nothing [] noLabel

segwitPaymentFile :: TxFile
segwitPaymentFile = TxFile (Just btc) segwitPayment Map.empty Map.empty

-- | The bip84 account of portfolio 0 of the test wallet on Bitcoin,
-- m/84'/0'/0', with the fingerprint of its wallet
segwitSeed :: FileAccount
segwitSeed = FileAccount testSegwitAccountPub Bip84 "m/84'/0'/0'" testFingerprint btc

-- | The bip44 account of the same portfolio, m/44'/0'/0', with the
-- fingerprint of its wallet
knownSeed :: FileAccount
knownSeed = FileAccount testAccountPub Bip44 "m/44'/0'/0'" testFingerprint btc

-- | The bip44 account of portfolio 0 of the second test wallet
otherWallet :: FileAccount
otherWallet = FileAccount (fromMaybe (error "otherWallet") (xPubImport btc testAccountXPub2)) Bip44 "m/44'/0'/0'" testFingerprint2 btc

-- | What portfolio 0 of the test wallet holds on Bitcoin, its pool: its
-- bip44 and its bip84 account, with the fingerprint of the wallet
walletPool :: [FileAccount]
walletPool = [knownSeed, segwitSeed]

-- | The transactions funding the coins of 'bothPayment': 30000 at the
-- first receiving address of the bip44 account, and 20000 at that of the
-- bip84 account
legacyCoinFunding, segwitCoinFunding :: Tx
legacyCoinFunding = fundingTx 5 [(externalAddr 0, 30000)]
segwitCoinFunding = fundingTx 6 [(segwitExternalAddr 0, 20000)]

-- | A payment that spends coins of both accounts of the pool: the two
-- coins above, spent to another wallet (40000) and to the first change
-- address of the bip84 account (7080), 2920 of fee for 292 virtual bytes
bothPayment :: TxSignData
bothPayment =
  TxSignData
    (builtTx 800000 [(txHash legacyCoinFunding, 0), (txHash segwitCoinFunding, 0)] [(otherAddr 0, 40000), (segwitInternalAddr 0, 7080)])
    [legacyCoinFunding, segwitCoinFunding]
    [legacyPath "/0/0", segwitPath "/0/0"]
    [segwitPath "/1/0"]
    (Just 10)
    Nothing
    []
    noLabel

bothPaymentFile :: TxFile
bothPaymentFile = TxFile (Just btc) bothPayment Map.empty Map.empty

-- | The file as hw writes it with the keys of one account, the one the
-- paths of the file are of
written :: FileAccount -> TxFile -> IO Psbt
written acc file = expectRight (txFilePsbt [acc] file)

-- | The PSBT with the records of the keys of the bip44 account of the test
-- wallet named by the fingerprint of the account's own key, 892eb8e4, and
-- the path from it, /0/0, in place of the seed and the full path: as
-- Bitcoin Core names a key it watches without the origin of its account.
-- hw itself always names the seed.
namedByOwnKey :: Psbt -> IO Psbt
namedByOwnKey psbt = do
  inputs <- mapM (rename inputDerivations insertInputDerivation) psbt.inputs
  outputs <- mapM (rename outputDerivations insertOutputDerivation) psbt.outputs
  return (Psbt psbt.version psbt.unsignedTx psbt.globals inputs outputs)
  where
    accountSteps = pathToList ("m/44'/0'/0'" :: DerivPath)
    rename :: (PsbtMap -> Either String [(PublicKey, Derivation)]) -> (PublicKey -> Derivation -> PsbtMap -> PsbtMap) -> PsbtMap -> IO PsbtMap
    rename records insert m = do
      found <- expectRight (records m)
      return $
        foldr
          ( \(public, d) -> case stripPrefix accountSteps (pathToList d.path) of
              Just rest | d.fingerprint == testFingerprint -> insert public (Derivation (fingerprintOf "892eb8e4") (listToPath rest))
              _ -> id
          )
          m
          found

-- | The file as hw reads it back among the pools given, each the accounts
-- of a portfolio on a network
readBack :: [[FileAccount]] -> Psbt -> IO TxFile
readBack pools psbt = expectRight (psbtTxFile Nothing pools psbt)

-- | A file Bitcoin Core wrote, in test/data/psbt/core, in base64 text
coreFile :: FilePath -> IO Psbt
coreFile name = expectRight . decodePsbtBase64 =<< BS.readFile ("test/data/psbt/core" </> name)

-- | The map of the input, or of the output, at the index
inputAt, outputAt :: Int -> Psbt -> PsbtMap
inputAt i psbt = psbt.inputs !! i
outputAt i psbt = psbt.outputs !! i

-- | The PSBT with the maps of its inputs, or its global map, replaced
withInputs :: [PsbtMap] -> Psbt -> Psbt
withInputs inputs psbt = Psbt psbt.version psbt.unsignedTx psbt.globals inputs psbt.outputs

withOutputs :: [PsbtMap] -> Psbt -> Psbt
withOutputs outputs psbt = Psbt psbt.version psbt.unsignedTx psbt.globals psbt.inputs outputs

withGlobals :: PsbtMap -> Psbt -> Psbt
withGlobals globals psbt = Psbt psbt.version psbt.unsignedTx globals psbt.inputs psbt.outputs

-- | The derivation records of a map, the keys shown in hexadecimal
shownDerivations :: (PsbtMap -> Either String [(PublicKey, Derivation)]) -> PsbtMap -> IO [(Text, Derivation)]
shownDerivations derivations m = do
  records <- expectRight (derivations m)
  return [(encodeHex (encodeSerial key), derivation) | (key, derivation) <- records]

-- | The field of hw with the subtype in a map
hwField :: Integral a => a -> PsbtMap -> Maybe BS.ByteString
hwField subtype = lookupProprietary hwIdentifier (fromIntegral subtype)

spec :: Spec
spec = do
  writingSpec
  signingIntoSpec
  signerChecksSpec
  readingSpec
  poolSpec

-- | The payment signed by the test wallet, naming its seed, as signtx
-- hands it to the file
signedWithSeed :: TxSignData
signedWithSeed = (signedByWallet payment) {txSignDataFingerprint = Just testFingerprint}

-- | The file hw writes for the unsigned payment, with the seed known:
-- the one Bitcoin Core signed for the vectors in test/data/psbt/core
unsignedPaymentText :: BS.ByteString
unsignedPaymentText = "cHNidP8BAHcCAAAAARtCW1G6d8iktJ6tfcrZ9/NlRYpWG/bQVr97AifbJolaAAAAAAD9////AmDqAAAAAAAAGXapFLy24qoWpyVllUiRDXoCBBTL/mXfiKwwdQAAAAAAABl2qRRFWqUMJmMUF3HrrCCW43gzkRIn1oisADUMAAX8Amh3AANidGMF/AJodwEICgAAAAAAAAAAAQBVAQAAAAEBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/////wGghgEAAAAAABl2qRTIDVUiPi8QYTY63Sp7A2PChb8pjYisAAAAAAEDBAEAAAAiBgMvZDcCIlbIVOAJDn1si2XpZgbvf0BTmeDwGHyTCXrA+Ri8Vg+VLAAAgAAAAIAAAACAAAAAAAAAAAAF/AJodwIIAAAAAAAAAAAAACICA3by5wZFjddWc7oZuO/YQu/d2bwK5B4xM53/z5F7bbbrGLxWD5UsAACAAAAAgAAAAIABAAAAAAAAAAA="

-- | The file hw writes for the unsigned payment of the bip84 account:
-- the one Bitcoin Core signed for the vectors in test/data/psbt/core,
-- checked with its decodepsbt: the coin on the input as its whole
-- transaction and as itself, the derivation records of the keys
-- m/84'/0'/0'/0/0 and /1/0 of the seed bc560f95, the sighash ALL and
-- hw's fields
segwitPaymentText :: BS.ByteString
segwitPaymentText = "cHNidP8BAHQCAAAAAf0HLBjt2BsDcA3jyok02bxCoNYoJWOnc0LyAZ3uZ+kZAAAAAAD9////AmDqAAAAAAAAGXapFLy24qoWpyVllUiRDXoCBBTL/mXfiKwwdQAAAAAAABYAFP0l5/X0gP7M5jxDskByzUud0N6hADUMAAX8Amh3AANidGMF/AJodwEICgAAAAAAAAAAAQBSAQAAAAECAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/////wGghgEAAAAAABYAFCTmRxsMgVruAse9ImFMtg5IEaAZAAAAAAEBH6CGAQAAAAAAFgAUJOZHGwyBWu4Cx70iYUy2DkgRoBkBAwQBAAAAIgYCdlLe+8CYMh2khdphdd6Lh49DPw2JM0NVJiXJzVCTP1MYvFYPlVQAAIAAAACAAAAAgAAAAAAAAAAABfwCaHcCCAAAAAAAAAAAAAAiAgMSvuVMTQzd0oxq2bXlbxXHLisLmALO0zcJZKa9BYoOChi8Vg+VVAAAgAAAAIAAAACAAQAAAAAAAAAA"

{- Writing -}

-- The public keys below are those of the test wallet at m/44'/0'/0'/0/0 and
-- m/44'/0'/0'/1/0, derived apart from hw with the Python tool of the
-- message-signing vectors. 892eb8e4 is the fingerprint of the account's own
-- key, derived the same way.
writingSpec :: Spec
writingSpec =
  describe "The PSBT of a transaction file" $ do
    it "names the key of the account's input by the fingerprint of the seed and the full path" $ do
      psbt <- written knownSeed paymentFile
      records <- shownDerivations inputDerivations (inputAt 0 psbt)
      records `shouldBe` [("032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9", Derivation testFingerprint "m/44'/0'/0'/0/0")]

    it "names the key of the account's change output the same way, and no key of another wallet's output" $ do
      psbt <- written knownSeed paymentFile
      shownDerivations outputDerivations (outputAt 0 psbt) `shouldReturn` []
      shownDerivations outputDerivations (outputAt 1 psbt) `shouldReturn` [("0376f2e706458dd75673ba19b8efd842efddd9bc0ae41e31339dffcf917b6db6eb", Derivation testFingerprint "m/44'/0'/0'/1/0")]

    -- The keys at m/84'/0'/0'/0/0 and /1/0 were derived apart from hw with
    -- the Python tool of the message-signing vectors
    it "names the keys of a bip84 account on its input and its change output, whose scripts are P2WPKH" $ do
      psbt <- written segwitSeed segwitPaymentFile
      inputs <- shownDerivations inputDerivations (inputAt 0 psbt)
      inputs `shouldBe` [("027652defbc098321da485da6175de8b878f433f0d893343552625c9cd50933f53", Derivation testFingerprint "m/84'/0'/0'/0/0")]
      outputs <- mapM (shownDerivations outputDerivations) psbt.outputs
      outputs `shouldBe` [[], [("0312bee54c4d0cddd28c6ad9b5e56f15c72e2b0b9802ced3370964a6bd058a0e0a", Derivation testFingerprint "m/84'/0'/0'/1/0")]]

    it "writes the coin itself beside its whole transaction on the input of a SegWit coin, which the signers of such coins ask for" $ do
      psbt <- written segwitSeed segwitPaymentFile
      witnessUtxo (inputAt 0 psbt) `shouldBe` Right (Just (TxOut 100000 (addressScript (segwitExternalAddr 0))))
      nonWitnessUtxo (inputAt 0 psbt) `shouldBe` Right (Just segwitFunding)
      legacy <- written knownSeed paymentFile
      witnessUtxo (inputAt 0 legacy) `shouldBe` Right Nothing

    it "writes the whole transaction each coin comes from on its input" $ do
      psbt <- written knownSeed paymentFile
      nonWitnessUtxo (inputAt 0 psbt) `shouldBe` Right (Just funding)

    it "writes SIGHASH_ALL on an unsigned input, with the FORKID flag on Bitcoin Cash" $ do
      bitcoin <- written knownSeed paymentFile
      cash <- written knownSeed paymentFile {txFileNetwork = Just bch}
      sigHashType (inputAt 0 bitcoin) `shouldBe` Right (Just 0x01)
      sigHashType (inputAt 0 cash) `shouldBe` Right (Just 0x41)

    it "writes a signed transaction as the unsigned one, with the final script of each signed input" $ do
      let signed = signedByWallet payment
      psbt <- written knownSeed paymentFile {txFileData = signed}
      psbt.unsignedTx `shouldBe` paymentTx
      finalScriptSig (inputAt 0 psbt) `shouldBe` Just (signed.txSignDataTx.inputs !! 0).script
      finalScriptWitness (inputAt 0 psbt) `shouldBe` Right Nothing
      psbtTransaction psbt `shouldBe` Right signed.txSignDataTx

    -- A signed input is final: the finalizer of the spec clears what
    -- served the signers, and Bitcoin Core writes it so
    it "writes no derivation record and no sighash type on a signed input, as a finalizer leaves it" $ do
      let signed = signedByWallet payment
      psbt <- written knownSeed paymentFile {txFileData = signed}
      inputDerivations (inputAt 0 psbt) `shouldBe` Right []
      sigHashType (inputAt 0 psbt) `shouldBe` Right Nothing
      nonWitnessUtxo (inputAt 0 psbt) `shouldBe` Right (Just funding)

    -- The coins are at /0/3 and /1/2: the field holds the chain, then the
    -- index, four bytes each, little-endian
    it "writes the path of the address an input spends from in the field of hw: its chain and its index" $ do
      let coins = fundingTx 1 [(externalAddr 3, 60000), (internalAddr 2, 40000)]
          spending = TxSignData (builtTx 800000 [(txHash coins, 0), (txHash coins, 1)] [(otherAddr 0, 90000)]) [coins] [legacyPath "/0/3", legacyPath "/1/2"] [] (Just 10) Nothing [] noLabel
      psbt <- written knownSeed (TxFile (Just btc) spending Map.empty Map.empty)
      map (hwField inputPathField) psbt.inputs `shouldBe` [Just (hexBytes "0000000003000000"), Just (hexBytes "0100000002000000")]

    -- A finalizer leaves the fields the spec does not define. The path
    -- /0/0 is how the reader of a signed file knows the input as the
    -- account's, and where the label of its address goes.
    it "writes that path on a signed input too, with the label of its address" $ do
      let labelled = TxFile (Just btc) (signedByWallet payment) (Map.fromList [(legacyPath "/0/0", "Salary")]) Map.empty
      psbt <- written knownSeed labelled
      hwField inputPathField (inputAt 0 psbt) `shouldBe` Just (hexBytes "0000000000000000")
      hwField addressLabelField (inputAt 0 psbt) `shouldBe` Just "Salary"

    it "keeps the derivation record of the account's change output in a signed file" $ do
      let signed = signedByWallet payment
      psbt <- written knownSeed paymentFile {txFileData = signed}
      change <- shownDerivations outputDerivations (outputAt 1 psbt)
      change `shouldBe` [("0376f2e706458dd75673ba19b8efd842efddd9bc0ae41e31339dffcf917b6db6eb", Derivation testFingerprint "m/44'/0'/0'/1/0")]

    -- The fee rate 10 over eight bytes, little-endian; the ids of the
    -- transactions replaced, 32 bytes each
    it "writes the network, the fee rate, the transactions replaced and the label in the fields of hw" $ do
      let replacement = payment {txSignDataReplaces = [fakeTxHash 3, fakeTxHash 4], txSignDataLabel = "Rent of October"}
      psbt <- written knownSeed paymentFile {txFileData = replacement}
      hwField networkField psbt.globals `shouldBe` Just "btc"
      hwField feeRateField psbt.globals `shouldBe` Just (hexBytes "0a00000000000000")
      hwField replacesField psbt.globals
        `shouldBe` Just (hexBytes ("03" <> Text.replicate 62 "0" <> "04" <> Text.replicate 62 "0"))
      hwField labelField psbt.globals `shouldBe` Just "Rent of October"

    it "writes no fee rate, replaced transaction or label when the transaction has none" $ do
      let plain = payment {txSignDataFeeRate = Nothing}
      psbt <- written knownSeed paymentFile {txFileData = plain}
      hwField feeRateField psbt.globals `shouldBe` Nothing
      hwField replacesField psbt.globals `shouldBe` Nothing
      hwField labelField psbt.globals `shouldBe` Nothing

    -- The file names no key of the account beyond those of its addresses
    -- in the transaction: the user chose not to put the account's key in it
    it "writes no key of the account in the global map" $ do
      known <- written knownSeed paymentFile
      xpubRecords btc known.globals `shouldBe` Right []
      Map.keys known.globals `shouldBe` [proprietaryKey (Proprietary "hw" 0 ""), proprietaryKey (Proprietary "hw" 1 "")]

    it "writes the labels of the account's addresses on their inputs and outputs, and the label a coin shows on its input" $ do
      let labelled = TxFile (Just btc) payment (Map.fromList [(legacyPath "/0/0", "Salary"), (legacyPath "/1/0", "Change")]) (Map.fromList [(txHash funding, "From work")])
      psbt <- written knownSeed labelled
      hwField addressLabelField (inputAt 0 psbt) `shouldBe` Just "Salary"
      hwField coinLabelField (inputAt 0 psbt) `shouldBe` Just "From work"
      map (hwField addressLabelField) psbt.outputs `shouldBe` [Nothing, Just "Change"]

    it "writes a sweep outside the wallet without any derivation record" $ do
      let sweep = TxFile (Just btc) (TxSignData paymentTx [funding] [] [] (Just 10) Nothing [] noLabel) Map.empty Map.empty
      psbt <- expectRight $ txFilePsbt [] sweep
      inputDerivations (inputAt 0 psbt) `shouldBe` Right []
      hwField inputPathField (inputAt 0 psbt) `shouldBe` Nothing
      mapM outputDerivations psbt.outputs `shouldBe` Right [[], []]
      xpubRecords btc psbt.globals `shouldBe` Right []

    it "refuses a file that names no network" $
      txFilePsbt [knownSeed] paymentFile {txFileNetwork = Nothing} `shouldBe` Left "The file names no network"

    it "refuses paths without an account to derive them from" $
      txFilePsbt [] paymentFile `shouldBe` Left "The file has paths of an account, and no account to derive them from"

    -- /0/1 is the second receiving address, which the payment does not spend from
    it "refuses a path of no input of the transaction" $ do
      let file = paymentFile {txFileData = payment {txSignDataInputPaths = [legacyPath "/0/1"]}}
      txFilePsbt [knownSeed] file `shouldBe` Left "The path bip44/0/1 is of no input of the transaction"

    -- The account has no address at /2/7: hw names no key of the account
    -- there, whatever the signing data says
    it "refuses a path of no address of the account" $ do
      let file = paymentFile {txFileData = payment {txSignDataOutputPaths = [legacyPath "/2/7"]}}
      txFilePsbt [knownSeed] file `shouldBe` Left "The path bip44/2/7 is of no address of the account: its addresses are at /0/n and /1/n"

    it "refuses a coin whose transaction is missing" $ do
      let file = paymentFile {txFileData = payment {txSignDataInputs = []}}
      txFilePsbt [knownSeed] file `shouldBe` Left "The transaction input 0 spends from is missing"

    -- Bitcoin Core decodes this file as the payment: its one input
    -- with the funding transaction, SIGHASH_ALL and the derivation
    -- m/44'/0'/0'/0/0 of the key 032f64... from the master key bc560f95,
    -- its change output with the derivation m/44'/0'/0'/1/0, a fee of
    -- 0.0001, no global xpub, and two proprietary fields of the identifier
    -- 6877 ("hw"): the network "btc" and the fee rate 10. analyzepsbt
    -- names the signer as the next role. See test/data/psbt/README.md.
    it "is this file, for the payment of the test wallet" $ do
      psbt <- written knownSeed paymentFile
      encodePsbtBase64 psbt `shouldBe` unsignedPaymentText

{- Signing into a file -}

signingIntoSpec :: Spec
signingIntoSpec =
  describe "The PSBT of a transaction signed into the PSBT it was read from" $ do
    -- The property the two writers share: for a file hw wrote, signing
    -- into it gives the file hw writes for the signed transaction
    it "is this file, for the payment of the bip84 account of the test wallet" $ do
      psbt <- written segwitSeed segwitPaymentFile
      encodePsbtBase64 psbt `shouldBe` segwitPaymentText

    it "is the file hw writes for the signed transaction, when the file was hw's own" $ do
      let signed = signedWithSeed
      source <- written knownSeed paymentFile
      fromScratch <- written knownSeed paymentFile {txFileData = signed}
      signedPsbt [knownSeed] source paymentFile {txFileData = signed} `shouldBe` Right fromScratch

    it "keeps every field of the file it did not write: a key hw does not know in each map, and a field of another wallet" $ do
      source <- written knownSeed paymentFile
      let foreign' = Map.insert (Key 0x20 "") "global" . insertProprietary "other" 7 "theirs"
          withForeign = Psbt source.version source.unsignedTx (foreign' source.globals) (foreign' <$> source.inputs) (foreign' <$> source.outputs)
      signed <- expectRight $ signedPsbt [knownSeed] withForeign paymentFile {txFileData = signedWithSeed}
      Map.lookup (Key 0x20 "") signed.globals `shouldBe` Just "global"
      lookupProprietary "other" 7 signed.globals `shouldBe` Just "theirs"
      map (Map.lookup (Key 0x20 "")) signed.inputs `shouldBe` [Just "global"]
      map (lookupProprietary "other" 7) signed.outputs `shouldBe` [Just "theirs", Just "theirs"]

    -- Bitcoin Core writes version 2 unless asked for version 0: the file
    -- it prepared for the account, in version 2, signed into. The fields
    -- of version 2 stay, and it has no flags: it takes no more inputs or
    -- outputs.
    it "keeps the version of a file of version 2, as the file Core accepts" $ do
      psbt <- coreFile "core-prepared-v2.psbt"
      file <- readBack [[knownSeed]] psbt
      (signed, _) <- expectRight $ signWalletTx btc file.txFileData [(Bip44, testAccountKey)]
      written' <- expectRight $ signedPsbt [knownSeed] psbt (TxFile (Just btc) signed Map.empty Map.empty)
      written'.version `shouldBe` Version2
      encodePsbtBase64 written' `shouldBe` corePreparedSignedV2Text
      txModifiable written'.globals `shouldBe` Right Nothing

    -- A file of version 2 may say its inputs and outputs can be added
    -- to. Once hw signed it with the sighash ALL, they cannot.
    it "clears the flags of a file of version 2 that let inputs and outputs be added, once signed" $ do
      psbt <- coreFile "core-prepared-v2.psbt"
      file <- readBack [[knownSeed]] psbt
      (signed, _) <- expectRight $ signWalletTx btc file.txFileData [(Bip44, testAccountKey)]
      let open = withGlobals (insertTxModifiable (Modifiable True True False 0) psbt.globals) psbt
      written' <- expectRight $ signedPsbt [knownSeed] open (TxFile (Just btc) signed Map.empty Map.empty)
      txModifiable written'.globals `shouldBe` Right (Just (Modifiable False False False 0))

    it "writes the final script of a signed input, and drops its sighash type and its partial signatures" $ do
      source <- written knownSeed paymentFile
      let key = forceRight (decodeSerial (hexBytes "032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9")) :: PublicKey
          withPartial = withInputs [insertPartialSignature key "a signature of another signer" (inputAt 0 source)] source
          signedTsd = signedWithSeed
      signed <- expectRight $ signedPsbt [knownSeed] withPartial paymentFile {txFileData = signedTsd}
      finalScriptSig (inputAt 0 signed) `shouldBe` Just (signedTsd.txSignDataTx.inputs !! 0).script
      sigHashType (inputAt 0 signed) `shouldBe` Right Nothing
      hasPartialSignatures (inputAt 0 signed) `shouldBe` False
      nonWitnessUtxo (inputAt 0 signed) `shouldBe` Right (Just funding)

    -- The signed input names no key any more. The change output still
    -- names its key, by the seed now.
    it "names the key of the account's change output by the seed, where a file of another wallet named it by the account's own key" $ do
      source <- namedByOwnKey =<< written knownSeed paymentFile
      signed <- expectRight $ signedPsbt [knownSeed] source paymentFile {txFileData = signedWithSeed}
      inputDerivations (inputAt 0 signed) `shouldBe` Right []
      change <- shownDerivations outputDerivations (outputAt 1 signed)
      change `shouldBe` [("0376f2e706458dd75673ba19b8efd842efddd9bc0ae41e31339dffcf917b6db6eb", Derivation testFingerprint "m/44'/0'/0'/1/0")]

    -- A sweep of a P2WPKH coin nested in P2SH, at the address 3J6JzvfP...
    -- of the swept key of SweepSpec. Its input pushes the redeem script,
    -- which the signer hands to the finalizer with its signature. The
    -- finalizer clears both, like every field that served the signers,
    -- and the extractor then gives the very transaction hw signed.
    it "clears every field of a signed input that served the signers, the redeem script of a nested SegWit coin among them" $ do
      let nestedAddr = fromMaybe (error "nestedAddr") (textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p")
          nestedFunding = fundingTx 4 [(nestedAddr, 100000)]
          sweep = TxSignData (builtTx 800000 [(txHash nestedFunding, 0)] [(otherAddr 0, 90000)]) [nestedFunding] [] [] (Just 10) Nothing [] noLabel
          sweepFile tsd = TxFile (Just btc) tsd Map.empty Map.empty
      source <- expectRight $ txFilePsbt [] (sweepFile sweep)
      (signedSweep, _) <- expectRight $ signTxWithKeys btc sweep [] noneHeld [sweptKey]
      signed <- expectRight $ signedPsbt [] source (sweepFile signedSweep)
      inputRedeemScript (inputAt 0 signed) `shouldBe` Right Nothing
      sigHashType (inputAt 0 signed) `shouldBe` Right Nothing
      hasPartialSignatures (inputAt 0 signed) `shouldBe` False
      psbtTransaction signed `shouldBe` Right signedSweep.txSignDataTx

    -- Core's file names the key of the input in a derivation record,
    -- which the finalizer clears, and has no field of hw. hw writes the
    -- path /0/0 it signed with in its own field first, so that the signed
    -- file still says which address of the account the input spends from.
    it "writes the path of the address it signed for in the field of hw of a file of another wallet" $ do
      psbt <- coreFile "core-prepared.psbt"
      hwField inputPathField (inputAt 0 psbt) `shouldBe` Nothing
      file <- readBack [[knownSeed]] psbt
      (signed, _) <- expectRight $ signWalletTx btc file.txFileData [(Bip44, testAccountKey)]
      written' <- expectRight $ signedPsbt [knownSeed] psbt (TxFile (Just btc) signed Map.empty Map.empty)
      inputDerivations (inputAt 0 written') `shouldBe` Right []
      hwField inputPathField (inputAt 0 written') `shouldBe` Just (hexBytes "0000000000000000")

    it "names the network in the field of hw when the file has none" $ do
      source <- written knownSeed paymentFile
      let withoutNetwork = withGlobals (Map.delete (proprietaryKey (Proprietary hwIdentifier networkField "")) source.globals) source
      signed <- expectRight $ signedPsbt [knownSeed] withoutNetwork (TxFile (Just btc) signedWithSeed Map.empty Map.empty)
      hwField networkField signed.globals `shouldBe` Just "btc"

    -- The network says how a signature is verified, which the finalizer
    -- does before it builds the final script
    it "refuses a signed transaction that names no network" $ do
      source <- written knownSeed paymentFile
      signedPsbt [knownSeed] source (TxFile Nothing signedWithSeed Map.empty Map.empty)
        `shouldBe` Left "The file names no network"

    -- The signature is that of the same payment with another lock time,
    -- put in the input of the payment. The finalizer verifies a signature
    -- against the coin and the transaction before it uses it, and leaves
    -- the input as it is, so the file does not hold the transaction given.
    it "refuses a signature that does not verify" $ do
      source <- written knownSeed paymentFile
      let otherTx = builtTx 800001 [(txHash funding, 0)] [(otherAddr 0, 60000), (internalAddr 0, 30000)]
          otherSigned = (signedByWallet payment {txSignDataTx = otherTx}).txSignDataTx
          tx = paymentTx
          forged = Tx tx.version [TxIn input.outpoint signed.script input.sequence | (input, signed) <- zip tx.inputs otherSigned.inputs] tx.outputs tx.witness tx.locktime
      signedPsbt [knownSeed] source paymentFile {txFileData = payment {txSignDataTx = forged}}
        `shouldBe` Left "The file, once finalized, does not hold the transaction signed"

    it "refuses a transaction that is not the one of the file" $ do
      source <- written knownSeed paymentFile
      let otherTx = builtTx 800001 [(txHash funding, 0)] [(otherAddr 0, 60000), (internalAddr 0, 30000)]
          other = (signedByWallet payment {txSignDataTx = otherTx}) {txSignDataFingerprint = Just testFingerprint}
      signedPsbt [knownSeed] source paymentFile {txFileData = other}
        `shouldBe` Left "The transaction signed is not the one of the file"

{- Before signing -}

signerChecksSpec :: Spec
signerChecksSpec =
  describe "The checks of the signer of the spec on a file, before hw signs it" $ do
    it "accepts the file hw writes for a payment" $ do
      psbt <- written knownSeed paymentFile
      signerChecks btc psbt `shouldBe` Right ()

    -- The file asks for the sighash ALL with FORKID, 0x41
    it "accepts the file hw writes for an account on Bitcoin Cash" $ do
      psbt <- written knownSeed {fileAccountNetwork = bch} paymentFile {txFileNetwork = Just bch}
      signerChecks bch psbt `shouldBe` Right ()

    -- The input asks for the sighash ALL, 1. The bytes filed as a
    -- signature of the key of the second wallet end with another byte: the
    -- finalizer of the spec would leave the input as it is, whatever hw
    -- adds to it.
    it "refuses a file whose input has a signature that does not end with the sighash of the input" $ do
      psbt <- written knownSeed paymentFile
      let other = forceRight (decodeSerial (hexBytes "02b31089d88f43ba74b787dad983c54486772e2d5c7b432e1bbf16591d698912c8")) :: PublicKey
          partial = insertPartialSignature other "a signature" (inputAt 0 psbt)
      signerChecks btc (withInputs [partial] psbt)
        `shouldBe` Left "Input 0: a signature of the input does not use the sighash type 1"

{- Reading -}

readingSpec :: Spec
readingSpec =
  describe "A transaction file read from a PSBT" $ do
    it "is the file written, with the fingerprint of the seed the records carry" $ do
      let labelled = TxFile (Just btc) payment {txSignDataReplaces = [fakeTxHash 3], txSignDataLabel = "Rent"} (Map.fromList [(legacyPath "/0/0", "Salary")]) (Map.fromList [(txHash funding, "From work")])
      file <- readBack [[knownSeed]] =<< written knownSeed labelled
      file `shouldBe` labelled {txFileData = labelled.txFileData {txSignDataFingerprint = Just testFingerprint}}

    -- Such records carry no fingerprint of the seed
    it "is the file written, as is, when the records name the account's own key, as another wallet may" $ do
      file <- readBack [[knownSeed]] =<< namedByOwnKey =<< written knownSeed paymentFile
      file `shouldBe` paymentFile

    it "is signed when the file was written signed" $ do
      let signed = paymentFile {txFileData = signedByWallet payment}
      file <- readBack [[knownSeed]] =<< written knownSeed signed
      isSigned file.txFileData.txSignDataTx `shouldBe` True
      file.txFileData.txSignDataTx `shouldBe` signed.txFileData.txSignDataTx
      -- The signed input names no key, and still the path of its address,
      -- in the field of hw. The change output still names its key.
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` [legacyPath "/1/0"]

    it "is the file written signed, with the labels of its addresses and the fingerprint of the seed its change output carries" $ do
      let labelled = TxFile (Just btc) (signedByWallet payment) (Map.fromList [(legacyPath "/0/0", "Salary"), (legacyPath "/1/0", "Change")]) (Map.fromList [(txHash funding, "From work")])
      file <- readBack [[knownSeed]] =<< written knownSeed labelled
      file `shouldBe` labelled {txFileData = labelled.txFileData {txSignDataFingerprint = Just testFingerprint}}

    -- The coin of 100000 at /0/0 goes to another wallet, less a fee of
    -- 10000. No output is the account's, so no record names it: the path
    -- of the input is all that does, and it names no seed.
    it "finds the pool of a signed payment without change by the path of its input, among several pools, and its account within the pool" $ do
      let allOfIt = TxSignData (builtTx 800000 [(txHash funding, 0)] [(otherAddr 0, 90000)]) [funding] [legacyPath "/0/0"] [] (Just 10) Nothing [] noLabel
      psbt <- written knownSeed (TxFile (Just btc) (signedByWallet allOfIt) Map.empty Map.empty)
      mapM outputDerivations psbt.outputs `shouldBe` Right [[]]
      file <- readBack [[otherWallet], [knownSeed, segwitSeed]] psbt
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` []
      file.txFileData.txSignDataFingerprint `shouldBe` Nothing

    -- The other wallet has another address at /0/0, which the coin does
    -- not pay: the path says nothing of it
    it "leaves aside the path of an input of another wallet" $ do
      let signed = paymentFile {txFileData = signedByWallet payment}
      file <- readBack [[otherWallet]] =<< written knownSeed signed
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` []

    -- The coin is at /0/0. A file that says /0/1 names the second
    -- receiving address, which the coin does not pay: the input is not
    -- taken for the account's on the word of the file.
    it "follows the path of an input only where the coin pays the address of the account there" $ do
      let signed = paymentFile {txFileData = signedByWallet payment}
      psbt <- written knownSeed signed
      let elsewhere = withInputs [insertProprietary hwIdentifier inputPathField (hexBytes "0000000001000000") (inputAt 0 psbt)] psbt
      file <- readBack [[knownSeed]] elsewhere
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` [legacyPath "/1/0"]

    -- /2/7 is on no chain of an account, 0x80000000 is a hardened index,
    -- and four bytes are no path of two steps: hw writes none of them
    it "refuses a path of an input that is not /0/n or /1/n" $ do
      psbt <- written knownSeed paymentFile
      forM_ ["0200000007000000", "0000000000000080", "00000000"] $ \bytes ->
        psbtTxFile Nothing [[knownSeed]] (withInputs [insertProprietary hwIdentifier inputPathField (hexBytes bytes) (inputAt 0 psbt)] psbt)
          `shouldBe` Left "Input 0: the path of the address it spends from, in the field of hw, is not /0/n or /1/n"

    -- The path /0/0 of eight bytes, and one byte after it
    it "refuses a path field of an input with a byte after its eight" $ do
      psbt <- written knownSeed paymentFile
      psbtTxFile Nothing [[knownSeed]] (withInputs [insertProprietary hwIdentifier inputPathField (hexBytes "000000000000000000") (inputAt 0 psbt)] psbt)
        `shouldBe` Left "Input 0: the path of the address it spends from, in the field of hw, is not /0/n or /1/n"

    it "finds the pool among several by the keys its records name" $ do
      file <- readBack [[otherWallet], [knownSeed]] =<< written knownSeed paymentFile
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` [legacyPath "/1/0"]

    it "leaves aside the records of another wallet, and reads the transaction as outside the wallet" $ do
      file <- readBack [[otherWallet]] =<< written knownSeed paymentFile
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` []
      file.txFileData.txSignDataFingerprint `shouldBe` Nothing
      file.txFilePathLabels `shouldBe` Map.empty

    -- The record of the second wallet's key, claiming the path of the
    -- first wallet's /0/0: the key of the second wallet at m/44'/0'/0'/0/0
    it "refuses a record that names a key of the account but carries another public key" $ do
      psbt <- written knownSeed paymentFile
      let other = forceRight (decodeSerial (hexBytes "02b31089d88f43ba74b787dad983c54486772e2d5c7b432e1bbf16591d698912c8")) :: PublicKey
          forged = insertInputDerivation other (Derivation testFingerprint "m/44'/0'/0'/0/0") (Map.filter (const False) (inputAt 0 psbt) `Map.union` withoutDerivations (inputAt 0 psbt))
      psbtTxFile Nothing [[knownSeed]] (withInputs [forged] psbt)
        `shouldBe` Left "Input 0 names the key /0/0 of the account, but carries another public key"

    -- The account has no address at /2/7, where its key still derives
    -- one. A crafted file pays the wallet's coin there, with the record
    -- that would make hw take the output for the account's own change:
    -- the wallet would never see the funds, since it scans /0/n and /1/n
    -- alone, and a second file could have them signed away.
    it "refuses a record that names a key of the account at a path of no address of it, on an output" $ do
      let hidden = builtTx 800000 [(txHash funding, 0)] [(addrAt27, 90000)]
          file = paymentFile {txFileData = payment {txSignDataTx = hidden, txSignDataOutputPaths = []}}
      psbt <- written knownSeed file
      let naming = insertOutputDerivation keyAt27 (Derivation testFingerprint "m/44'/0'/0'/2/7") (outputAt 0 psbt)
      psbtTxFile Nothing [[knownSeed]] (withOutputs [naming] psbt)
        `shouldBe` Left "Output 0 names a key of the account at /2/7, where it has no address: its addresses are at /0/n and /1/n"

    -- The second file: the coin at /2/7 spent to another wallet, its
    -- input naming the account's key there by the seed, by the account's
    -- own key, or by another seed
    it "refuses a record that names a key of the account at a path of no address of it, on an input, whatever seed it names" $ do
      let coin = fundingTx 2 [(addrAt27, 100000)]
          spend = builtTx 800000 [(txHash coin, 0)] [(otherAddr 0, 90000)]
          outside = TxFile (Just btc) (TxSignData spend [coin] [] [] (Just 10) Nothing [] noLabel) Map.empty Map.empty
      psbt <- expectRight $ txFilePsbt [] outside
      forM_ [Derivation testFingerprint "m/44'/0'/0'/2/7", Derivation (fingerprintOf "892eb8e4") "/2/7", Derivation testFingerprint2 "m/44'/0'/0'/2/7"] $ \derivation -> do
        let naming = insertInputDerivation keyAt27 derivation (inputAt 0 psbt)
        psbtTxFile Nothing [[knownSeed]] (withInputs [naming] psbt)
          `shouldBe` Left "Input 0 names a key of the account at /2/7, where it has no address: its addresses are at /0/n and /1/n"

    -- A hardened step below the account derives no address of it either
    it "refuses a record that names a key of the account at a hardened step below it" $ do
      psbt <- written knownSeed paymentFile
      let naming = insertInputDerivation keyAt27 (Derivation testFingerprint "m/44'/0'/0'/0'/0") (withoutDerivations (inputAt 0 psbt))
      psbtTxFile Nothing [[knownSeed]] (withInputs [naming] psbt)
        `shouldBe` Left "Input 0 names a key of the account at /0'/0, where it has no address: its addresses are at /0/n and /1/n"

    -- The second wallet's key at its /0/0, which its record places at
    -- /2/7: its business, not the account's
    it "leaves aside a record of another wallet at such a path" $ do
      psbt <- written knownSeed paymentFile
      let other = forceRight (decodeSerial (hexBytes "02b31089d88f43ba74b787dad983c54486772e2d5c7b432e1bbf16591d698912c8")) :: PublicKey
          naming = insertInputDerivation other (Derivation testFingerprint2 "m/44'/0'/0'/2/7") (inputAt 0 psbt)
      file <- readBack [[knownSeed]] (withInputs [naming] psbt)
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]

    it "refuses an input without its previous transaction" $ do
      psbt <- written knownSeed paymentFile
      psbtTxFile Nothing [[knownSeed]] (withInputs [Map.delete (Key 0x00 "") (inputAt 0 psbt)] psbt)
        `shouldBe` Left "Input 0 has no previous transaction: hw needs the whole transaction each coin comes from"

    it "refuses a previous transaction that is not the one the input spends" $ do
      psbt <- written knownSeed paymentFile
      psbtTxFile Nothing [[knownSeed]] (withInputs [insertNonWitnessUtxo (fundingTx 2 [(externalAddr 0, 100000)]) (inputAt 0 psbt)] psbt)
        `shouldBe` Left "Input 0: the previous transaction is not the one the input spends"

    -- A SegWit input carries its coin alone beside the transaction it
    -- comes from. The amount of the coin alone is a claim of whoever wrote
    -- the file: here 200000 where the transaction pays 100000. The file
    -- reads as the one hw wrote, whose coin is the 100000 of the
    -- transaction.
    it "reads the coin of an input from its previous transaction, whatever the coin given alone beside it claims" $ do
      psbt <- written segwitSeed segwitPaymentFile
      let claimed = TxOut 200000 (addressScript (segwitExternalAddr 0))
      readBack [[segwitSeed]] (withInputs [insertWitnessUtxo claimed (inputAt 0 psbt)] psbt)
        `shouldReturn` segwitPaymentFile {txFileData = segwitPayment {txSignDataFingerprint = Just testFingerprint}}

    -- Bitcoin Core signed the file hw writes for the payment without
    -- finalizing it, as a hardware wallet does: see test/data/psbt/core.
    -- The final script is the one Core builds when it finalizes the same
    -- file: the signature, then the key.
    it "finalizes an input another signer left with a partial signature of the coin's key, as Core finalizes it" $ do
      psbt <- coreFile "hw-payment-partial.psbt"
      file <- readBack [[knownSeed]] psbt
      let input = file.txFileData.txSignDataTx.inputs !! 0
      encodeHex input.script `shouldBe` "473044022074f111ce88a136dea1259050e9a564a9a021d07e5bc9e35710f4768da5b1b9a502200fbdf1799186f1bb53be566c1f529be81f44d4c461521d9ef5f1495a951f24b70121032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9"
      isSigned file.txFileData.txSignDataTx `shouldBe` True
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]
      file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    -- The same file signed and finalized by Core, which clears the
    -- derivation records of the input it finalized and leaves the fields
    -- of hw: the input still names its path, /0/0. The transaction is the
    -- one Core's finalizepsbt gives.
    it "reads the transaction of an input another signer finalized, which names no key of the account any more, and still its path" $ do
      psbt <- coreFile "hw-payment-finalized.psbt"
      inputDerivations (inputAt 0 psbt) `shouldBe` Right []
      hwField inputPathField (inputAt 0 psbt) `shouldBe` Just (hexBytes "0000000000000000")
      file <- readBack [[knownSeed]] psbt
      encodeHex (S.encode file.txFileData.txSignDataTx)
        `shouldBe` "02000000011b425b51ba77c8a4b49ead7dcad9f7f365458a561bf6d056bf7b0227db26895a000000006a473044022074f111ce88a136dea1259050e9a564a9a021d07e5bc9e35710f4768da5b1b9a502200fbdf1799186f1bb53be566c1f529be81f44d4c461521d9ef5f1495a951f24b70121032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9fdffffff0260ea0000000000001976a914bcb6e2aa16a725659548910d7a020414cbfe65df88ac30750000000000001976a914455aa50c2663141771ebac2096e37833911227d688ac00350c00"
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` [legacyPath "/1/0"]
      file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint
      file.txFileData.txSignDataFeeRate `shouldBe` Just 10
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    it "reads the file of a bip84 account back as the file written, with the fingerprint of the seed, finding the account among those of the pool" $ do
      psbt <- written segwitSeed segwitPaymentFile
      readBack [[knownSeed, segwitSeed]] psbt
        `shouldReturn` segwitPaymentFile {txFileData = segwitPayment {txSignDataFingerprint = Just testFingerprint}}

    -- The signature of the coin is the one hw makes for it, left among the
    -- partial signatures as another signer leaves it
    it "finalizes a partial signature of a P2WPKH coin into the witness of its input, the script left empty" $ do
      (signed, _) <- expectRight $ signWalletTx btc segwitPayment [(Bip84, testSegwitAccountKey)]
      stack <- case signed.txSignDataTx.witness of
        [stack@[_, _]] -> return stack
        other -> fail ("Not one witness of a signature and a key: " <> show other)
      psbt <- written segwitSeed segwitPaymentFile
      let public = forceRight (decodeSerial (stack !! 1)) :: PublicKey
          partial = insertPartialSignature public (stack !! 0) (inputAt 0 psbt)
      file <- readBack [[segwitSeed]] (withInputs [partial] psbt)
      map (.script) file.txFileData.txSignDataTx.inputs `shouldBe` [BS.empty]
      file.txFileData.txSignDataTx.witness `shouldBe` [stack]
      isSigned file.txFileData.txSignDataTx `shouldBe` True
      file.txFileData.txSignDataInputPaths `shouldBe` [segwitPath "/0/0"]
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    -- The key of the second wallet at m/44'/0'/0'/0/0 is not the coin's.
    -- The signature filed under it is the one hw makes for the coin, so
    -- that it reads as a signature: the finalizer looks only at the
    -- signatures of the keys the script of the coin names.
    it "leaves an input with a partial signature of another key unsigned" $ do
      (signed, _) <- expectRight $ signWalletTx btc segwitPayment [(Bip84, testSegwitAccountKey)]
      signature <- case signed.txSignDataTx.witness of
        [[signature, _]] -> return signature
        other -> fail ("Not one witness of a signature and a key: " <> show other)
      psbt <- written segwitSeed segwitPaymentFile
      let other = forceRight (decodeSerial (hexBytes "02b31089d88f43ba74b787dad983c54486772e2d5c7b432e1bbf16591d698912c8")) :: PublicKey
          partial = insertPartialSignature other signature (inputAt 0 psbt)
      file <- readBack [[segwitSeed]] (withInputs [partial] psbt)
      isSigned file.txFileData.txSignDataTx `shouldBe` False
      file.txFileData.txSignDataTx `shouldBe` segwitPayment.txSignDataTx

    -- The spec has a finalizer leave an input when any of its signatures,
    -- used or not, is of another sighash than the input names, ALL here.
    -- The signature of the coin's key is the one hw makes, which alone
    -- makes the input final, as two tests above. The bytes filed under
    -- the key of the second wallet end with another byte.
    it "leaves unsigned an input signed by the coin's key that has another signature, which does not end with the sighash of the input" $ do
      (signed, _) <- expectRight $ signWalletTx btc segwitPayment [(Bip84, testSegwitAccountKey)]
      stack <- case signed.txSignDataTx.witness of
        [stack@[_, _]] -> return stack
        other -> fail ("Not one witness of a signature and a key: " <> show other)
      psbt <- written segwitSeed segwitPaymentFile
      let public = forceRight (decodeSerial (stack !! 1)) :: PublicKey
          other = forceRight (decodeSerial (hexBytes "02b31089d88f43ba74b787dad983c54486772e2d5c7b432e1bbf16591d698912c8")) :: PublicKey
          partial = insertPartialSignature other "a signature" (insertPartialSignature public (stack !! 0) (inputAt 0 psbt))
      file <- readBack [[segwitSeed]] (withInputs [partial] psbt)
      isSigned file.txFileData.txSignDataTx `shouldBe` False
      file.txFileData.txSignDataTx `shouldBe` segwitPayment.txSignDataTx

    -- The signature is that of the same payment with another lock time,
    -- which the coin's key made: it does not verify for this payment
    it "leaves unsigned an input with a signature of the coin's key that does not verify" $ do
      let otherTx = builtTx 800001 [(txHash segwitFunding, 0)] [(otherAddr 0, 60000), (segwitInternalAddr 0, 30000)]
      (signed, _) <- expectRight $ signWalletTx btc segwitPayment {txSignDataTx = otherTx} [(Bip84, testSegwitAccountKey)]
      stack <- case signed.txSignDataTx.witness of
        [stack@[_, _]] -> return stack
        other -> fail ("Not one witness of a signature and a key: " <> show other)
      psbt <- written segwitSeed segwitPaymentFile
      let public = forceRight (decodeSerial (stack !! 1)) :: PublicKey
          partial = insertPartialSignature public (stack !! 0) (inputAt 0 psbt)
      file <- readBack [[segwitSeed]] (withInputs [partial] psbt)
      isSigned file.txFileData.txSignDataTx `shouldBe` False
      file.txFileData.txSignDataTx `shouldBe` segwitPayment.txSignDataTx

    -- Nothing names a network: the file has no field of hw, no account is
    -- given and none comes with the command. A signature is verified as
    -- its network has it, so nothing is finalized.
    it "finalizes nothing when nothing names the network, and reads the input as unsigned" $ do
      psbt <- coreFile "hw-payment-partial.psbt"
      let nameless = withGlobals (Map.delete (proprietaryKey (Proprietary hwIdentifier networkField "")) psbt.globals) psbt
      file <- expectRight $ psbtTxFile Nothing [] nameless
      isSigned file.txFileData.txSignDataTx `shouldBe` False
      -- The network of the command finalizes it
      onBitcoin <- expectRight $ psbtTxFile (Just btc) [] nameless
      isSigned onBitcoin.txFileData.txSignDataTx `shouldBe` True

    it "refuses an input to be signed with a sighash hw does not sign with, naming both" $ do
      psbt <- written knownSeed paymentFile
      let asking sh = withInputs [insertSigHashType sh (inputAt 0 psbt)] psbt
      psbtTxFile Nothing [[knownSeed]] (asking 0x02) `shouldBe` Left "Input 0 is to be signed with the sighash NONE: hw signs with ALL on btc"
      psbtTxFile Nothing [[knownSeed]] (asking 0x83) `shouldBe` Left "Input 0 is to be signed with the sighash SINGLE|ANYONECANPAY: hw signs with ALL on btc"
      psbtTxFile Nothing [[knownSeed]] (asking 0x41) `shouldBe` Left "Input 0 is to be signed with the sighash ALL|FORKID: hw signs with ALL on btc"

    it "accepts ALL with FORKID for an account on Bitcoin Cash, and an input without a sighash" $ do
      let cash = knownSeed {fileAccountNetwork = bch}
      psbt <- written cash paymentFile {txFileNetwork = Just bch}
      sigHashType (inputAt 0 psbt) `shouldBe` Right (Just 0x41)
      file <- readBack [[cash]] psbt
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]
      psbtTxFile Nothing [[knownSeed]] psbt `shouldBe` Left "Input 0 is to be signed with the sighash ALL|FORKID: hw signs with ALL on btc"
      without <- readBack [[knownSeed]] (withInputs [deleteSigHashType (inputAt 0 psbt)] psbt)
      without.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]

    it "refuses, for a transaction outside the wallet, a sighash other than ALL, with FORKID only on the network the file names" $ do
      let sweep = TxFile (Just btc) (TxSignData paymentTx [funding] [] [] (Just 10) Nothing [] noLabel) Map.empty Map.empty
      psbt <- expectRight $ txFilePsbt [] sweep
      let asking sh = withInputs [insertSigHashType sh (inputAt 0 psbt)] psbt
          unnamed p = withGlobals (Map.delete (proprietaryKey (Proprietary hwIdentifier networkField "")) p.globals) p
      psbtTxFile Nothing [] (asking 0x41) `shouldBe` Left "Input 0 is to be signed with the sighash ALL|FORKID: hw signs with ALL on btc"
      psbtTxFile Nothing [] (unnamed (asking 0x02)) `shouldBe` Left "Input 0 is to be signed with the sighash NONE: hw signs with ALL, or ALL|FORKID on Bitcoin Cash"
      (txFileNetwork <$> psbtTxFile Nothing [] (unnamed (asking 0x41))) `shouldBe` Right Nothing

    -- Bitcoin Core prepared this payment for the account, which it
    -- watches by the descriptor of the account's key: 1 BTC from the coin
    -- of 50 BTC mined to /0/0 on regtest, to another wallet, with the
    -- change at /1/0 and a fee of 2250 satoshis. Its records name the keys
    -- by the seed. It carries no field of hw: no network, no fee rate.
    it "reads a file Bitcoin Core prepared for the account, and tells the account the seed" $ do
      psbt <- coreFile "core-prepared.psbt"
      file <- readBack [[otherWallet], [knownSeed]] psbt
      file.txFileNetwork `shouldBe` Nothing
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` [legacyPath "/1/0"]
      file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint
      file.txFileData.txSignDataFeeRate `shouldBe` Nothing
      file.txFileData.txSignDataReplaces `shouldBe` []
      file.txFileData.txSignDataLabel `shouldBe` ""
      map txHash file.txFileData.txSignDataInputs `shouldBe` ["fede72ccadbf9f16f826e3078578b0660526a37b48d4f894ab26f4cf5a95e89c"]
      info <- expectRight $ parseTxFile btc [(Bip44, testAccountPub)] file
      info.txInfoSummary.summaryFee `shouldBe` 2250
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(testAddress "1GfJmWwG2JUjrvQ6WjVfwaWpPgtbHDhSdE", 100000000)]
      Map.keys info.txInfoSummary.summaryMyOutputs `shouldBe` [internalAddr 0]
      Map.keys info.txInfoSummary.summaryMyInputs `shouldBe` [externalAddr 0]

    -- The same payment as Core writes it by default since 32.0: a PSBT of
    -- version 2, whose fields describe the transaction
    it "reads a file Bitcoin Core prepared in version 2 as the same file as in version 0" $ do
      version0 <- readBack [[knownSeed]] =<< coreFile "core-prepared.psbt"
      version2 <- readBack [[knownSeed]] =<< coreFile "core-prepared-v2.psbt"
      version2 `shouldBe` version0

    -- Core signed its version 2 file without finalizing, as a hardware
    -- wallet does. The final script is the one Core builds when it
    -- finalizes the same file.
    it "finalizes a partial signature of the coin's key in a file of version 2, as Core finalizes it" $ do
      file <- readBack [[knownSeed]] =<< coreFile "core-prepared-v2-partial.psbt"
      let input = file.txFileData.txSignDataTx.inputs !! 0
      encodeHex input.script `shouldBe` "47304402202de30e4e2c703ea6f743f04721dbf1d9468c6527911011d7405778f310f7bab402201a19d6a0f44fb17013e2535aa2c9e219e2dc727033a3efa4c07e74028bd4f69e0121032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9"
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0"]
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    -- Core finalized its version 2 file: the input keeps the fields of
    -- version 2 and loses its derivation record, so the file names no
    -- path of it. The id is the one Core's decoderawtransaction gives the
    -- transaction it extracts.
    it "reads a file Bitcoin Core finalized in version 2 as the transaction Core extracted, with no path of its input" $ do
      file <- readBack [[knownSeed]] =<< coreFile "core-prepared-v2-finalized.psbt"
      isSigned file.txFileData.txSignDataTx `shouldBe` True
      -- The transaction Core's finalizepsbt extracted, in the .hex file beside
      extracted <- BS.readFile "test/data/psbt/core/core-prepared-v2-finalized.hex"
      encodeHex (S.encode file.txFileData.txSignDataTx) `shouldBe` decodeUtf8 (C8.strip extracted)
      txHash file.txFileData.txSignDataTx `shouldBe` "4227ad75bbe53198afe887a6c486514c352134e991637bc808dd3d591c1ce6d6"
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` [legacyPath "/1/0"]

    -- Bitcoin Core prepared this payment for the bip84 account, which it
    -- watches by the descriptor of the account's key: 1 BTC from the coin
    -- of 50 BTC mined to /0/0 on regtest, to another wallet, with the
    -- change at /1/0 and a fee of 1440 satoshis, 10 per virtual byte for
    -- 144. Each input carries the coin's transaction and the coin itself.
    it "reads a file Bitcoin Core prepared for a bip84 account, and tells the account the seed" $ do
      psbt <- coreFile "core-prepared-segwit.psbt"
      witnessUtxo (inputAt 0 psbt) `shouldBe` Right (Just (TxOut 5000000000 (addressScript (segwitExternalAddr 0))))
      file <- readBack [[otherWallet], [knownSeed, segwitSeed]] psbt
      file.txFileNetwork `shouldBe` Nothing
      file.txFileData.txSignDataInputPaths `shouldBe` [segwitPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` [segwitPath "/1/0"]
      file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint
      file.txFileData.txSignDataFeeRate `shouldBe` Nothing
      map txHash file.txFileData.txSignDataInputs `shouldBe` ["08d847645a7389f60bc35545b6b54aef449620dbc139433df4fbc874f6c53567"]
      info <- expectRight $ parseTxFile btc [(Bip84, testSegwitAccountPub)] file
      (info.txInfoSummary.summaryFee, info.txInfoSummary.summarySize, feeRateText info.txInfoSummary) `shouldBe` (1440, 144, "10")
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(testAddress "157L9V7wdL7pYt2nndN5NN5DMynh1HdYhF", 100000000)]
      Map.toList info.txInfoSummary.summaryMyOutputs `shouldBe` [(segwitInternalAddr 0, MyOutputs 4899998560 (segwitPath "/1/0") "")]
      Map.keys info.txInfoSummary.summaryMyInputs `shouldBe` [segwitExternalAddr 0]

    -- The signed file, checked with Core: analyzepsbt finds it complete,
    -- finalizepsbt extracts its transaction, which testmempoolaccept
    -- accepts on the regtest chain the coin is on
    it "signs a file Bitcoin Core prepared for a bip84 account into that file, as the file Core accepts" $ do
      psbt <- coreFile "core-prepared-segwit.psbt"
      file <- readBack [[segwitSeed]] psbt
      (signed, info) <- expectRight $ signWalletTx btc file.txFileData [(Bip84, testSegwitAccountKey)]
      txHash signed.txSignDataTx `shouldBe` "7ef8ee09a10d0edd9c06d6cb68a35fc37bbecccd24e35ca957f2526eb22d4848"
      info.txInfoSummary.summarySize `shouldBe` 144
      written' <- expectRight $ signedPsbt [segwitSeed] psbt (TxFile (Just btc) signed Map.empty Map.empty)
      encodePsbtBase64 written' `shouldBe` coreSegwitPreparedSignedText

    -- The same payment as Core writes it by default, a PSBT of version 2,
    -- with the change first this time
    it "reads a file Bitcoin Core prepared in version 2 for a bip84 account" $ do
      file <- readBack [[segwitSeed]] =<< coreFile "core-prepared-segwit-v2.psbt"
      file.txFileData.txSignDataInputPaths `shouldBe` [segwitPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` [segwitPath "/1/0"]
      info <- expectRight $ parseTxFile btc [(Bip84, testSegwitAccountPub)] file
      info.txInfoSummary.summaryFee `shouldBe` 1440

    -- Core signed its version 2 file without finalizing, as a hardware
    -- wallet does: hw puts the signature in the witness, as Core does when
    -- it finalizes the same file, whose transaction is the one in the
    -- .hex file beside it
    it "finalizes a partial signature of a P2WPKH coin in a file of version 2, as Core finalizes it" $ do
      file <- readBack [[segwitSeed]] =<< coreFile "core-prepared-segwit-v2-partial.psbt"
      finalized <- BS.readFile "test/data/psbt/core/core-prepared-segwit-v2-finalized.hex"
      encodeHex (S.encode file.txFileData.txSignDataTx) `shouldBe` decodeUtf8 (C8.strip finalized)
      txHash file.txFileData.txSignDataTx `shouldBe` "d81848d337a1157f00264090d585f370c46f4099618fb9a47de24a2e8b7200cf"
      file.txFileData.txSignDataInputPaths `shouldBe` [segwitPath "/0/0"]
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    -- Core finalized its version 2 file for the bip84 account: the witness
    -- input keeps the fields of version 2 and loses its derivation record.
    -- The transaction is the one Core's finalizepsbt extracted, in the .hex
    -- file beside it.
    it "reads a file Bitcoin Core finalized in version 2 for a bip84 account as the transaction Core extracted, with no path of its input" $ do
      file <- readBack [[segwitSeed]] =<< coreFile "core-prepared-segwit-v2-finalized.psbt"
      extracted <- BS.readFile "test/data/psbt/core/core-prepared-segwit-v2-finalized.hex"
      encodeHex (S.encode file.txFileData.txSignDataTx) `shouldBe` decodeUtf8 (C8.strip extracted)
      txHash file.txFileData.txSignDataTx `shouldBe` "d81848d337a1157f00264090d585f370c46f4099618fb9a47de24a2e8b7200cf"
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` [segwitPath "/1/0"]
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    -- The file hw writes for the payment of the bip84 account, signed by
    -- Core without finalizing it, as a hardware wallet leaves it: the
    -- witness hw builds is the one Core builds when it finalizes the same
    -- file
    it "finalizes a partial signature of a P2WPKH coin another signer left, as Core finalizes it" $ do
      psbt <- coreFile "hw-segwit-payment-partial.psbt"
      file <- readBack [[segwitSeed]] psbt
      map (.script) file.txFileData.txSignDataTx.inputs `shouldBe` [BS.empty]
      map (map encodeHex) file.txFileData.txSignDataTx.witness
        `shouldBe` [["304402202a23cc7e490251459c3954d9800b8231c2cbace769c0e1336b54716ca392404d022041f7d7e7b779f3dea71a3fc9a03f4201f994774e264ef9a50a48abbbfb9e671001", "027652defbc098321da485da6175de8b878f433f0d893343552625c9cd50933f53"]]
      isSigned file.txFileData.txSignDataTx `shouldBe` True
      file.txFileData.txSignDataInputPaths `shouldBe` [segwitPath "/0/0"]
      file.txFileData.txSignDataFingerprint `shouldBe` Just testFingerprint
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    -- The same file finalized by Core, which clears the derivation record
    -- of the input it finalized, and leaves hw's fields: the input still
    -- names its path, /0/0. The transaction is the one Core's
    -- finalizepsbt gives.
    it "reads the transaction of a P2WPKH input another signer finalized, which names no key of the account any more, and still its path" $ do
      file <- readBack [[segwitSeed]] =<< coreFile "hw-segwit-payment-finalized.psbt"
      finalized <- BS.readFile "test/data/psbt/core/hw-segwit-payment-finalized.hex"
      encodeHex (S.encode file.txFileData.txSignDataTx) `shouldBe` decodeUtf8 (C8.strip finalized)
      txHash file.txFileData.txSignDataTx `shouldBe` "1fd25156213613f70bd549422e25c3134741a27315202b015231f645ad7f75fe"
      file.txFileData.txSignDataInputPaths `shouldBe` [segwitPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` [segwitPath "/1/0"]
      file.txFileData.txSignDataFeeRate `shouldBe` Just 10
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    -- The payment of Database/PendingTxSpec.hs, signed and finalized by
    -- Core, in both versions
    it "reads a file finalized in version 2 as the same file as in version 0" $ do
      version0 <- readBack [[knownSeed]] =<< coreFile "pending-payment-finalized.psbt"
      version2 <- readBack [[knownSeed]] =<< coreFile "pending-payment-finalized-v2.psbt"
      version2 `shouldBe` version0

    -- hw signs it with the key of the test wallet and writes the signatures
    -- into Core's file. Bitcoin Core finds this file complete with
    -- analyzepsbt, extracts the transaction with finalizepsbt, and accepts
    -- it with testmempoolaccept on the regtest chain the coin is on.
    it "signs a file Bitcoin Core prepared into that file, as the file Core accepts" $ do
      psbt <- coreFile "core-prepared.psbt"
      file <- readBack [[knownSeed]] psbt
      (signed, _) <- expectRight $ signWalletTx btc file.txFileData [(Bip44, testAccountKey)]
      written' <- expectRight $ signedPsbt [knownSeed] psbt (TxFile (Just btc) signed Map.empty Map.empty)
      encodePsbtBase64 written' `shouldBe` corePreparedSignedText

    -- Bitcoin Core prepared this payment of 2.5 BTC of another wallet
    -- of its own, with the change at a second address of that wallet and a
    -- fee of 2250 satoshis
    it "reads a file Bitcoin Core prepared for another wallet as outside the wallet" $ do
      psbt <- coreFile "core-outside.psbt"
      file <- readBack [[knownSeed, segwitSeed], [otherWallet]] psbt
      file.txFileNetwork `shouldBe` Nothing
      file.txFileData.txSignDataInputPaths `shouldBe` []
      file.txFileData.txSignDataOutputPaths `shouldBe` []
      file.txFileData.txSignDataFingerprint `shouldBe` Nothing
      info <- expectRight $ parseOutsideTxSignData btc file.txFileData
      info.txInfoSummary.summaryFee `shouldBe` 2250
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(testAddress "13tJFzYnF13gRMh6Z6eb44mLD9e9R8sHeZ", 250000000), (testAddress "1588dPBmgVnvS2N3BE5YHXUqXcb468zpsz", 4749997750)]
      Map.keys info.txInfoSummary.summaryOtherInputs `shouldBe` [testAddress "157L9V7wdL7pYt2nndN5NN5DMynh1HdYhF"]

    it "names no network when the file has no network field" $ do
      psbt <- written knownSeed paymentFile
      file <- readBack [[knownSeed]] (withGlobals (Map.delete (proprietaryKey (Proprietary hwIdentifier networkField "")) psbt.globals) psbt)
      file.txFileNetwork `shouldBe` Nothing

    it "refuses a network hw does not know" $ do
      psbt <- written knownSeed paymentFile
      psbtTxFile Nothing [[knownSeed]] (withGlobals (insertProprietary hwIdentifier networkField "doge" psbt.globals) psbt)
        `shouldBe` Left "Unknown network doge"

    it "refuses a fee rate that is not a 64-bit integer, transactions replaced that are not ids, and a label that is not text" $ do
      psbt <- written knownSeed paymentFile
      let withGlobal subtype value = withGlobals (insertProprietary hwIdentifier subtype value psbt.globals) psbt
      psbtTxFile Nothing [[knownSeed]] (withGlobal feeRateField (hexBytes "0a")) `shouldBe` Left "The fee rate of the file is not a 64-bit integer"
      psbtTxFile Nothing [[knownSeed]] (withGlobal replacesField (hexBytes "0300")) `shouldBe` Left "The transactions replaced are not a list of transaction ids"
      psbtTxFile Nothing [[knownSeed]] (withGlobal labelField (hexBytes "ff")) `shouldBe` Left "The label of the transaction is not UTF-8 text"
  where
    -- A map without its derivation records
    withoutDerivations = Map.filterWithKey (\key _ -> key.keyType /= 0x06)

{- A payment of both accounts of a pool -}

-- The public keys are those of the test wallet at m/44'/0'/0'/0/0, at
-- m/84'/0'/0'/0/0 and at m/84'/0'/0'/1/0, and 892eb8e4 and 659a27ed are
-- the fingerprints of the keys of its accounts m/44'/0'/0' and
-- m/84'/0'/0', all derived apart from hw with the Python tool of the
-- message-signing vectors.
poolSpec :: Spec
poolSpec =
  describe "The file of a payment that spends coins of both accounts of a portfolio" $ do
    it "names the key of each input by the path of its own account, from the fingerprint of the wallet" $ do
      psbt <- expectRight $ txFilePsbt walletPool bothPaymentFile
      inputs <- mapM (shownDerivations inputDerivations) psbt.inputs
      inputs
        `shouldBe` [ [("032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9", Derivation testFingerprint "m/44'/0'/0'/0/0")],
                     [("027652defbc098321da485da6175de8b878f433f0d893343552625c9cd50933f53", Derivation testFingerprint "m/84'/0'/0'/0/0")]
                   ]

    it "names the key of its change output by the path of the account of the change" $ do
      psbt <- expectRight $ txFilePsbt walletPool bothPaymentFile
      outputs <- mapM (shownDerivations outputDerivations) psbt.outputs
      outputs `shouldBe` [[], [("0312bee54c4d0cddd28c6ad9b5e56f15c72e2b0b9802ced3370964a6bd058a0e0a", Derivation testFingerprint "m/84'/0'/0'/1/0")]]

    -- The coin of the bip44 account is spent as it is, and that of the
    -- bip84 account has its output beside its transaction
    it "writes the coin itself on the input of the bip84 account alone" $ do
      psbt <- expectRight $ txFilePsbt walletPool bothPaymentFile
      map witnessUtxo psbt.inputs
        `shouldBe` [Right Nothing, Right (Just (TxOut 20000 (addressScript (segwitExternalAddr 0))))]

    it "reads back as the file written, each input with the path of its own account, and the fingerprint of the wallet" $ do
      psbt <- expectRight $ txFilePsbt walletPool bothPaymentFile
      file <- readBack [walletPool] psbt
      file `shouldBe` bothPaymentFile {txFileData = bothPayment {txSignDataFingerprint = Just testFingerprint}}

    it "is found among several pools as the pool both of its inputs are of" $ do
      psbt <- expectRight $ txFilePsbt walletPool bothPaymentFile
      file <- readBack [[otherWallet], walletPool] psbt
      file.txFileData.txSignDataInputPaths `shouldBe` [legacyPath "/0/0", segwitPath "/0/0"]
      file.txFileData.txSignDataOutputPaths `shouldBe` [segwitPath "/1/0"]

    it "keeps the labels of its addresses by the path of each in its own account" $ do
      let labelled = bothPaymentFile {txFilePathLabels = Map.fromList [(legacyPath "/0/0", "Salary"), (segwitPath "/0/0", "Savings")]}
      psbt <- expectRight $ txFilePsbt walletPool labelled
      map (hwField addressLabelField) psbt.inputs `shouldBe` [Just "Salary", Just "Savings"]
      file <- readBack [walletPool] psbt
      file.txFilePathLabels `shouldBe` Map.fromList [(legacyPath "/0/0", "Salary"), (segwitPath "/0/0", "Savings")]

    -- Signed, the inputs name no key any more, and each still names its
    -- path, /0/0 for both: the coin tells which account the path is of
    it "reads back signed with each input in its own account, though both name the same path below their account" $ do
      let signed = (signedByWallet bothPayment) {txSignDataFingerprint = Just testFingerprint}
      psbt <- expectRight $ txFilePsbt walletPool bothPaymentFile {txFileData = signed}
      map (hwField inputPathField) psbt.inputs `shouldBe` [Just (hexBytes "0000000000000000"), Just (hexBytes "0000000000000000")]
      file <- readBack [walletPool] psbt
      file.txFileData `shouldBe` signed
      verifySignedTx btc file.txFileData `shouldBe` Right ()

    it "signed into the file it was read from, is the file hw writes for the signed payment" $ do
      let signed = (signedByWallet bothPayment) {txSignDataFingerprint = Just testFingerprint}
      source <- expectRight $ txFilePsbt walletPool bothPaymentFile
      fromScratch <- expectRight $ txFilePsbt walletPool bothPaymentFile {txFileData = signed}
      signedPsbt walletPool source bothPaymentFile {txFileData = signed} `shouldBe` Right fromScratch

    it "is refused a path of an account the pool given does not have" $
      txFilePsbt [knownSeed] bothPaymentFile
        `shouldBe` Left "The path bip84/0/0 is of a bip84 account, which the file was given no key of"
