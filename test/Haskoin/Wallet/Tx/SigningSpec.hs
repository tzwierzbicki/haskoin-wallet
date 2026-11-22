{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Tx.SigningSpec where

import Control.Monad (void, when)
import qualified Data.ByteString as BS
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (parseMaybe)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Haskoin
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath (..), keyAddress, legacyPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo (MyInputs (..), MyOutputs (..), TxInfo (..), TxSummary (..), TxType (..), txInfoHash)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing
import Haskoin.Wallet.Tx.Size (accountInput, accountOutputKind, txSize)
import Haskoin.Wallet.TxVectors
import Numeric.Natural (Natural)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Data.Default (def)
import Haskoin.Wallet.Arbitrary (arbitraryTxSignData)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- The signing vectors live in JSON files in test/data/tx-signing (see the
-- README.md file there for the format and the origin of the vectors). Every
-- .json file is loaded, so new vectors can be added without changing this
-- module.
spec :: Spec
spec = do
  -- Counted apart from hw: 39 vectors, as verify_vectors.py finds them
  txVectorSpec "test/data/tx-signing" "Transaction signing test vectors" [("edge-cases.json", 16), ("external.json", 1), ("hw-regression.json", 10), ("trezor.json", 12)] walletSigner
  analysisSpec
  signingSpec
  signInputsSpec
  verificationSpec
  propertiesSpec
  fileSpec
  -- The signing data reads back as it was written
  testIdentity def {jsonTests = [JsonBox (arbitraryTxSignData btc)]}

-- | Sign with the account key, like signtx does. Vectors with sweep keys
-- belong in test/data/tx-sweep.
walletSigner :: VectorSigner
walletSigner v = do
  when (isJust v.vecSweepKeys) $
    Left "this is a sweep vector: it belongs in test/data/tx-sweep"
  accountKey <- prefixError "account" $ accountSigningKey v.vecNetwork v.vecAccount
  return $ signWalletTx v.vecNetwork v.vecSignData [(accountSpecKind v.vecAccount, accountKey)]

{- The signing data in JSON, and the files -}

fileSpec :: Spec
fileSpec = do
  describe "The signing data in JSON, as the database keeps it," $ do
    let tsd = simpleTransaction
    it "names no transaction replaced, but for a replacement" $
      (Json.decode (Json.encode tsd) >>= parseMaybe (.: "replaces")) `shouldBe` (Nothing :: Maybe [TxHash])

    it "of a replacement names the transactions it replaces, and reads back" $ do
      let replacement = tsd {txSignDataReplaces = [fakeTxHash 3, fakeTxHash 4]}
          written = Json.encode replacement
      (Json.decode written >>= parseMaybe (.: "replaces"))
        `shouldBe` Just ["0000000000000000000000000000000000000000000000000000000000000003", "0000000000000000000000000000000000000000000000000000000000000004" :: Text]
      Json.eitherDecode written `shouldBe` Right replacement

    it "holds no label, but for a transaction the user labelled" $
      (Json.decode (Json.encode tsd) >>= parseMaybe (.: "label")) `shouldBe` (Nothing :: Maybe Text)

    it "of a labelled transaction holds its label, and reads back" $ do
      let labelled = tsd {txSignDataLabel = "Rent of October"}
          written = Json.encode labelled
      (Json.decode written >>= parseMaybe (.: "label")) `shouldBe` Just ("Rent of October" :: Text)
      Json.eitherDecode written `shouldBe` Right labelled

    -- The simple transaction spends from /0/0 of the bip44 account and
    -- pays the change at /1/0 of it
    it "writes each path with the type of its account" $ do
      let written = Json.toJSON tsd
      parseMaybe (Json.withObject "signing data" (.: "inputpaths")) written
        `shouldBe` Just [Json.object ["type" .= ("bip44" :: Text), "path" .= ("/0/0" :: Text)]]
      parseMaybe (Json.withObject "signing data" (.: "outputpaths")) written
        `shouldBe` Just [Json.object ["type" .= ("bip44" :: Text), "path" .= ("/1/0" :: Text)]]

    it "writes a path of the bip84 account with the type bip84, and reads it back" $ do
      let written = Json.toJSON segwitUnsigned
      parseMaybe (Json.withObject "signing data" (.: "inputpaths")) written
        `shouldBe` Just [Json.object ["type" .= ("bip84" :: Text), "path" .= ("/0/0" :: Text)]]
      parseMaybe Json.parseJSON written `shouldBe` Just segwitUnsigned

  describe "A transaction file" $ do
    let tsd = simpleTransaction
    -- The coin spent is at the first receiving address, /0/0, and the
    -- change goes to the first change address, /1/0, which the file does
    -- not label
    it "is read as its portfolio sees it, with the labels it carries" $ do
      let labelled = TxFile (Just btc) tsd (Map.fromList [(legacyPath "/0/0", "Salary")]) Map.empty
      info <- expectRight $ parseTxFile btc [(Bip44, testAccountPub)] labelled
      fmap (.myInputsLabel) info.txInfoSummary.summaryMyInputs `shouldBe` Map.fromList [(externalAddr 0, "Salary")]
      fmap (.myOutputsLabel) info.txInfoSummary.summaryMyOutputs `shouldBe` Map.fromList [(internalAddr 0, "")]

    -- The payment spends from /0/0 of the bip84 account. A label of the
    -- file at /0/0 of the bip44 account is of another address, which the
    -- transaction does not touch.
    it "is read with the label of a path on the address of the account the path names, not on the same path of the other account" $ do
      let labelled = TxFile (Just btc) segwitUnsigned (Map.fromList [(segwitPath "/0/0", "Savings"), (legacyPath "/0/0", "Salary")]) Map.empty
      info <- expectRight $ parseTxFile btc testPoolPubKeys labelled
      fmap (.myInputsLabel) info.txInfoSummary.summaryMyInputs `shouldBe` Map.fromList [(segwitExternalAddr 0, "Savings")]

    -- /0/1 is the account's second receiving address and /1/3 its fourth
    -- change address. The transaction touches neither.
    it "is read without the labels of paths the transaction does not touch" $ do
      let labelled = TxFile (Just btc) tsd (Map.fromList [(legacyPath "/0/1", "Elsewhere"), (legacyPath "/1/3", "Other change")]) Map.empty
      info <- expectRight $ parseTxFile btc [(Bip44, testAccountPub)] labelled
      fmap (.myInputsLabel) info.txInfoSummary.summaryMyInputs `shouldBe` Map.fromList [(externalAddr 0, "")]
      fmap (.myOutputsLabel) info.txInfoSummary.summaryMyOutputs `shouldBe` Map.fromList [(internalAddr 0, "")]

    -- The change address, /1/0, has no label, so the file leaves it out
    it "is written with the labels of the portfolio's addresses that have one, by their paths" $ do
      info <- expectRight $ parseTxSignData btc [(Bip44, testAccountPub)] tsd
      let salaried = info {txInfoSummary = info.txInfoSummary {summaryMyInputs = Map.map (\i -> i {myInputsLabel = "Salary"}) info.txInfoSummary.summaryMyInputs}}
      txFilePathLabels (labelledTxFile btc salaried tsd) `shouldBe` Map.fromList [(legacyPath "/0/0", "Salary")]

    -- The coin spent is of the funding transaction. Transaction 3 is not
    -- spent, so its label is left aside.
    it "is read with the labels of the coins it spends, and without those of other transactions" $ do
      let labelled = TxFile (Just btc) tsd Map.empty (Map.fromList [(txHash simpleFunding, "Salary"), (fakeTxHash 3, "Elsewhere")])
      info <- expectRight $ parseTxFile btc [(Bip44, testAccountPub)] labelled
      Map.toList info.txInfoInputLabels `shouldBe` [(txHash simpleFunding, "Salary")]

    it "is written with the labels the coins it spends show" $ do
      info <- expectRight $ parseTxSignData btc [(Bip44, testAccountPub)] tsd
      let salaried = info {txInfoInputLabels = Map.fromList [(txHash simpleFunding, "Salary")]}
      Map.toList (txFileInputLabels (labelledTxFile btc salaried tsd)) `shouldBe` [(txHash simpleFunding, "Salary")]

{- Analysis -}

-- | The transaction of the tests below: the wallet's coin of 1 BTC at its
-- first external address, spent to another wallet (0.5 BTC) and back to the
-- wallet's first internal address (0.4 BTC), with a fee of 0.1 BTC. It is
-- the hw/btc/debit vector of test/data/tx-signing.
simpleTransaction :: TxSignData
simpleTransaction =
  TxSignData tx [simpleFunding] [legacyPath (Deriv :/ 0 :/ 0)] [legacyPath (Deriv :/ 1 :/ 0)] Nothing Nothing [] noLabel
  where
    tx = spendingTx [(txHash simpleFunding, 0)] [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]

-- | The transaction funding the wallet's coin of 'simpleTransaction'
simpleFunding :: Tx
simpleFunding = fundingTx 1 [(externalAddr 0, 100000000)]

-- | The payment of the bip84 account of the test wallet: its coin of
-- 100000 at /0/0, spent to another wallet (60000) and to its change
-- address /1/0 (30000), built at 10 satoshis per virtual byte
segwitUnsigned :: TxSignData
segwitUnsigned = TxSignData tx [segwitFunding] [segwitPath (Deriv :/ 0 :/ 0)] [segwitPath (Deriv :/ 1 :/ 0)] (Just 10) Nothing [] noLabel
  where
    segwitFunding = fundingTx 2 [(segwitExternalAddr 0, 100000)]
    tx = spendingTx [(txHash segwitFunding, 0)] [(otherAddr 0, 60000), (segwitInternalAddr 0, 30000)]

-- | A payment of portfolio 0 of the test wallet that spends coins of both
-- of its accounts on Bitcoin: 30000 at /0/0 of the bip44 account and 20000
-- at /0/0 of the bip84 account, spent to another wallet (40000) and to the
-- first change address of the bip44 account (7050), built at 10 satoshis
-- per virtual byte
bothTypesUnsigned :: TxSignData
bothTypesUnsigned =
  TxSignData tx [legacyFunding, segwitFunding] [legacyPath (Deriv :/ 0 :/ 0), segwitPath (Deriv :/ 0 :/ 0)] [legacyPath (Deriv :/ 1 :/ 0)] (Just 10) Nothing [] noLabel
  where
    legacyFunding = fundingTx 1 [(externalAddr 0, 30000)]
    segwitFunding = fundingTx 2 [(segwitExternalAddr 0, 20000)]
    tx = spendingTx [(txHash legacyFunding, 0), (txHash segwitFunding, 0)] [(otherAddr 0, 40000), (internalAddr 0, 7050)]

-- | The simple transaction signed with SIGHASH_ALL and ANYONECANPAY, as
-- another signer could sign it: a valid signature that does not commit to
-- the whole transaction
signedWithAnyoneCanPay :: Either String TxSignData
signedWithAnyoneCanPay = do
  script <- maybeToEither "The address has no script" $ addressToOutput (externalAddr 0)
  let unsigned = simpleTransaction
      coin = SigInput script 100000000 (OutPoint (txHash simpleFunding) 0) (setAnyoneCanPay sigHashAll) Nothing
      key = (derivePath (Deriv :/ 0 :/ 0 :: SoftPath) testAccountKey).key
  tx <- signTx btc unsigned.txSignDataTx [coin] [key]
  return unsigned {txSignDataTx = tx}

analysisSpec :: Spec
analysisSpec =
  describe "Transaction analysis" $ do
    -- The signer sees it, and so can refuse a lock time it did not expect
    it "carries the lock time of the transaction" $ do
      let tsd = simpleTransaction
          tx = tsd.txSignDataTx
          locked = tsd {txSignDataTx = Tx tx.version tx.inputs tx.outputs tx.witness 814037}
      info <- expectRight $ parseTxSignData btc [(Bip44, testAccountPub)] locked
      info.txInfoSummary.summaryLockTime `shouldBe` 814037
      plain <- expectRight $ parseTxSignData btc [(Bip44, testAccountPub)] tsd
      plain.txInfoSummary.summaryLockTime `shouldBe` 0

    -- Its paths are those of an account the wallet does not have: the coin
    -- of 1 BTC and the two outputs are another wallet's
    it "describes a transaction outside the wallet, every input and output another wallet's" $ do
      info <- expectRight $ parseOutsideTxSignData btc unsigned
      info.txInfoSummary.summaryAmount `shouldBe` 0
      info.txInfoSummary.summaryFee `shouldBe` 10000000
      info.txInfoSummary.summaryOtherInputs `shouldBe` Map.fromList [(externalAddr 0, 100000000)]
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]
      (Map.null info.txInfoSummary.summaryMyInputs, Map.null info.txInfoSummary.summaryMyOutputs) `shouldBe` (True, True)

    -- The portfolio neither gains nor loses by it: it is no payment sent
    it "types a transaction that neither spends from nor pays the portfolio as external" $ do
      info <- expectRight $ parseOutsideTxSignData btc unsigned
      info.txInfoSummary.summaryType `shouldBe` TxExternal

    it "describes an unsigned transaction with an estimated size" $ do
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] unsigned
      txInfoHash info `shouldBe` Nothing
      info.txInfoSummary.summaryType `shouldBe` TxDebit
      info.txInfoSummary.summaryAmount `shouldBe` (-60000000)
      info.txInfoSummary.summaryFee `shouldBe` 10000000
      -- The estimate: 10 bytes, 148 for the input and 34 for each output
      info.txInfoSummary.summarySize `shouldBe` 226
      -- 10000000 / 226 = 44247.78
      feeRateText info.txInfoSummary `shouldBe` "44247.7"
      pendingFlags info `shouldBe` Just ((simpleNosigHash, False, False, False))
      -- Without the requested fee rate, no change in the fee can be told
      info.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
      -- The wallet's input, with the coin it spends, and its change
      info.txInfoSummary.summaryMyInputs
        `shouldBe` Map.fromList [(externalAddr 0, MyInputs 100000000 (legacyPath (Deriv :/ 0 :/ 0)) "" [OutPoint (txHash simpleFunding) 0])]
      info.txInfoSummary.summaryMyOutputs `shouldBe` Map.fromList [(internalAddr 0, MyOutputs 40000000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(otherAddr 0, 50000000)]
      info.txInfoSummary.summaryOtherInputs `shouldBe` Map.empty
      info.txInfoSummary.summaryNonStdInputs `shouldBe` []
      info.txInfoSummary.summaryNonStdOutputs `shouldBe` []
    it "describes the signed transaction with its real size" $ do
      (signed, _) <- expectRight $ signWalletTx btc unsigned [(Bip44, testAccountKey)]
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] signed
      txInfoHash info `shouldBe` Just "e66b790c73d4e72fe13a07e247e4439bdea210cac2f947040e901f1e0ce59ac2"
      -- The signature of this transaction is 72 bytes long, which the
      -- estimate assumes: 10 + 148 + 2 * 34
      info.txInfoSummary.summarySize `shouldBe` 226
      feeRateText info.txInfoSummary `shouldBe` "44247.7"
      pendingFlags info `shouldBe` Just ((simpleNosigHash, True, False, False))
    it "reports the change too small for an output that the fee includes" $ do
      -- A coin of 352500 pays 350000 at 10 satoshis per virtual byte: the fee is
      -- 2500, 580 beyond the 10 + 148 + 34 = 192 bytes * 10 = 1920 of the
      -- transaction. Signing does not change it: the estimated size counts.
      let funding = fundingTx 1 [(externalAddr 0, 352500)]
          tx = spendingTx [(txHash funding, 0)] [(otherAddr 0, 350000)]
          tsd = TxSignData tx [funding] [legacyPath (Deriv :/ 0 :/ 0)] [] (Just 10) Nothing [] noLabel
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] tsd
      info.txInfoSummary.summaryFee `shouldBe` 2500
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 580
      (signed, signedInfo) <- expectRight $ signWalletTx btc tsd [(Bip44, testAccountKey)]
      signed.txSignDataFeeRate `shouldBe` Just 10
      signedInfo.txInfoSummary.summaryChangeInFee `shouldBe` Just 580
    it "reports change in the fee up to one satoshi below what a change output could have held" $ do
      -- A coin of 352805 pays 350000: 2805 of fee, 885 beyond 1920. A change
      -- output of 34 bytes would cost 340 of it at 10 sat/vB, which leaves
      -- 545, one below the dust limit of 546: the builder drops such change
      let funding = fundingTx 1 [(externalAddr 0, 352805)]
          tx = spendingTx [(txHash funding, 0)] [(otherAddr 0, 350000)]
          tsd = TxSignData tx [funding] [legacyPath (Deriv :/ 0 :/ 0)] [] (Just 10) Nothing [] noLabel
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] tsd
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 885
    it "reports no change in a fee that a change output could have held" $ do
      -- A coin of 352806: 886 beyond 1920, and 886 - 340 = 546, the dust
      -- limit, which a change output may hold
      let funding = fundingTx 1 [(externalAddr 0, 352806)]
          tx = spendingTx [(txHash funding, 0)] [(otherAddr 0, 350000)]
          tsd = TxSignData tx [funding] [legacyPath (Deriv :/ 0 :/ 0)] [] (Just 10) Nothing [] noLabel
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] tsd
      info.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
    -- A bip84 coin pays 350000 to a P2PKH address: 10 + 41 + 34 = 85
    -- bytes and (2 + 108) / 4 of witness data, rounded up to 28, are 113
    -- virtual bytes, 1130 at 10 sat/vB. A P2WPKH change output of 31 bytes
    -- would cost 310 more and must hold 294: the builder drops change
    -- below 604.
    it "reports change in the fee of a bip84 payment up to one satoshi below what a P2WPKH change output could have held" $ do
      -- A coin of 351733: 1733 of fee, 603 beyond 1130
      let paying value =
            let funding = fundingTx 1 [(segwitExternalAddr 0, value)]
                tx = spendingTx [(txHash funding, 0)] [(otherAddr 0, 350000)]
             in TxSignData tx [funding] [segwitPath (Deriv :/ 0 :/ 0)] [] (Just 10) Nothing [] noLabel
      info <- expectRight $ parseTxSignData btc testPoolPubKeys (paying 351733)
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 603
      -- A coin of 351734: 604 beyond 1130, which a change output may hold
      more <- expectRight $ parseTxSignData btc testPoolPubKeys (paying 351734)
      more.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
    -- The coins of 30000 of the bip44 account and of 20000 of the bip84
    -- account pay one P2PKH address: 261 virtual bytes, 2610 at 10 sat/vB.
    -- The signing data does not say the type the change would have had,
    -- so the limit is the higher of the two: a P2PKH change output, which
    -- costs 340 and must hold 546, so change below 886 is dropped.
    it "reports change in the fee of a payment of coins of both types up to one satoshi below what a P2PKH change output could have held" $ do
      -- 46505 paid: 3495 of fee, 885 beyond 2610
      let paying amount =
            let unsigned' = bothTypesUnsigned
                spent = [(input.outpoint.hash, input.outpoint.index) | input <- unsigned'.txSignDataTx.inputs]
             in unsigned' {txSignDataTx = spendingTx spent [(otherAddr 0, amount)], txSignDataOutputPaths = []}
      info <- expectRight $ parseTxSignData btc testPoolPubKeys (paying 46505)
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 885
      -- 46504 paid: 886 beyond 2610, which a change output may hold
      more <- expectRight $ parseTxSignData btc testPoolPubKeys (paying 46504)
      more.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
    -- A file naming a fee rate of 10 for a coin of 1 btc spent on 350000:
    -- the note would call 99648080 of the fee change, 99650000 - 1920
    it "does not explain as change a fee far above the rate the file names" $ do
      let funding = fundingTx 1 [(externalAddr 0, 100000000)]
          tx = spendingTx [(txHash funding, 0)] [(otherAddr 0, 350000)]
          tsd = TxSignData tx [funding] [legacyPath (Deriv :/ 0 :/ 0)] [] (Just 10) Nothing [] noLabel
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] tsd
      info.txInfoSummary.summaryFee `shouldBe` 99650000
      info.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
    it "reports no change in the fee when the fee matches the requested rate" $ do
      -- A coin of 351920 pays 350000: 1920 of fee, exactly 192 bytes * 10
      let funding = fundingTx 1 [(externalAddr 0, 351920)]
          tx = spendingTx [(txHash funding, 0)] [(otherAddr 0, 350000)]
          tsd = TxSignData tx [funding] [legacyPath (Deriv :/ 0 :/ 0)] [] (Just 10) Nothing [] noLabel
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] tsd
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 0
    it "refuses an output at an address the network cannot write, a SegWit one on Bitcoin Cash" $ do
      -- The same script is a payment to a SegWit address on Bitcoin, and
      -- one to nobody the review could name on Bitcoin Cash
      let tx = spendingTx [(txHash simpleFunding, 0)] [(segwitAddr, 50000000), (internalAddr 0, 40000000)]
          tsd = TxSignData tx [simpleFunding] [legacyPath (Deriv :/ 0 :/ 0)] [legacyPath (Deriv :/ 1 :/ 0)] Nothing Nothing [] noLabel
      parseTxSignData bch [(Bip44, accountPub)] tsd `shouldBe` Left "There are non-standard outputs in the transaction"
      info <- expectRight $ parseTxSignData btc [(Bip44, accountPub)] tsd
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(segwitAddr, 50000000)]

    -- The coin of 1 BTC paid to the key of the account at /2/7, where it
    -- has no address: taken for the account's own, the payment would show
    -- as internal, and the wallet, which scans /0/n and /1/n alone, would
    -- never see the funds
    it "refuses an output at a path of no address of the account, rather than showing it as the wallet's own" $ do
      let tx = spendingTx [(txHash simpleFunding, 0)] [(addrAt27, 90000000)]
          tsd = TxSignData tx [simpleFunding] [legacyPath (Deriv :/ 0 :/ 0)] [legacyPath (Deriv :/ 2 :/ 7)] Nothing Nothing [] noLabel
      parseTxSignData btc [(Bip44, accountPub)] tsd `shouldBe` Left "The path bip44/2/7 is of no address of the account: its addresses are at /0/n and /1/n"

    it "refuses an input at a path of no address of the account" $ do
      let coin = fundingTx 2 [(addrAt27, 100000)]
          tx = spendingTx [(txHash coin, 0)] [(otherAddr 0, 90000)]
          tsd = TxSignData tx [coin] [legacyPath (Deriv :/ 2 :/ 7)] [] Nothing Nothing [] noLabel
      parseTxSignData btc [(Bip44, accountPub)] tsd `shouldBe` Left "The path bip44/2/7 is of no address of the account: its addresses are at /0/n and /1/n"

    -- The coin of 1 btc at /0/0 pays 50000000 out and the rest, less a fee
    -- of 10000000, to the change at the path. No scan of the account looks
    -- a thousand addresses past the last one it knows of a chain.
    it "refuses change a thousand and one addresses into a chain of which the wallet holds no address and spends from none" $
      checkOutputReach noneHeld (changeAt (Deriv :/ 1 :/ 1000) [])
        `shouldBe` Left "The path bip44/1/1000 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
    it "takes change at the thousandth address of a chain of which the wallet holds no address and spends from none" $
      checkOutputReach noneHeld (changeAt (Deriv :/ 1 :/ 999) []) `shouldBe` Right ()
    -- The change address /1/2 the wallet holds is its third: 2 + 1000
    it "takes change a thousand addresses past the last change address the wallet holds, and refuses it one further" $ do
      checkOutputReach (Map.singleton Bip44 (HeldChains 0 3)) (changeAt (Deriv :/ 1 :/ 1002) []) `shouldBe` Right ()
      checkOutputReach (Map.singleton Bip44 (HeldChains 0 3)) (changeAt (Deriv :/ 1 :/ 1003) [])
        `shouldBe` Left "The path bip44/1/1003 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
    -- The coin at /1/5 is in use: 5 + 1000
    it "takes change a thousand addresses past the furthest coin the transaction spends from the chain, and refuses it one further" $ do
      checkOutputReach noneHeld (changeAt (Deriv :/ 1 :/ 1005) [Deriv :/ 1 :/ 5]) `shouldBe` Right ()
      checkOutputReach noneHeld (changeAt (Deriv :/ 1 :/ 1006) [Deriv :/ 1 :/ 5])
        `shouldBe` Left "The path bip44/1/1006 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
    -- The rows of the two accounts of the test portfolio on Bitcoin: 12
    -- receiving and 3 change addresses in the bip44 account, 2 receiving
    -- addresses and no change address in the bip84 account
    it "counts the addresses the wallet holds of each account of the portfolio apart, to measure its output paths against" $
      heldPoolOf [mainAccountRow, segwitAccountRow]
        `shouldBe` Map.fromList [(Bip44, HeldChains 12 3), (Bip84, HeldChains 2 0)]
    -- Each account of a portfolio has chains of its own. The change of the
    -- payment of the bip84 account is at /1/1002 here, which is within
    -- 1000 addresses of the third change address of the bip84 account.
    it "measures a path against the chain of its own account: the change addresses the bip84 account holds, not those of the bip44 one" $ do
      let unsigned' = segwitUnsigned
          farChange = unsigned' {txSignDataOutputPaths = [segwitPath (Deriv :/ 1 :/ 1002)]}
      checkOutputReach (Map.singleton Bip84 (HeldChains 0 3)) farChange `shouldBe` Right ()
      checkOutputReach (Map.singleton Bip44 (HeldChains 0 3)) farChange
        `shouldBe` Left "The path bip84/1/1002 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
    -- The coin at /1/5 of the bip44 account says nothing of the change
    -- chain of the bip84 account
    it "measures a path against the coins the transaction spends from the chain of its own account" $ do
      let spending = changeAt (Deriv :/ 1 :/ 0) [Deriv :/ 1 :/ 5]
      checkOutputReach noneHeld spending {txSignDataOutputPaths = [legacyPath (Deriv :/ 1 :/ 1005)]} `shouldBe` Right ()
      checkOutputReach noneHeld spending {txSignDataOutputPaths = [segwitPath (Deriv :/ 1 :/ 1005)]}
        `shouldBe` Left "The path bip84/1/1005 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
    -- The receiving addresses the wallet holds say nothing of the change chain
    it "measures the change chain by the change addresses alone" $
      checkOutputReach (Map.singleton Bip44 (HeldChains 5000 0)) (changeAt (Deriv :/ 1 :/ 1000) [])
        `shouldBe` Left "The path bip44/1/1000 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
    it "refuses an output to a receiving address far past those the wallet holds" $
      checkOutputReach (Map.singleton Bip44 (HeldChains 3 0)) (changeAt (Deriv :/ 0 :/ 2000000000) [])
        `shouldBe` Left "The path bip44/0/2000000000 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
    it "refuses to sign change far past the addresses of its chain, before anything is signed" $
      signHeldWalletTx btc noneHeld (changeAt (Deriv :/ 1 :/ 2000000000) []) [(Bip44, testAccountKey)]
        `shouldBe` Left "The path bip44/1/2000000000 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
    -- A coin at /0/5000 is there to be spent, however far along its chain
    it "takes an input far past the addresses the wallet holds" $ do
      let coin = fundingTx 2 [(addrAt (Deriv :/ 0 :/ 5000), 100000)]
          tx = spendingTx [(txHash coin, 0)] [(otherAddr 0, 90000)]
          tsd = TxSignData tx [coin] [legacyPath (Deriv :/ 0 :/ 5000)] [] Nothing Nothing [] noLabel
      checkOutputReach noneHeld tsd `shouldBe` Right ()
  where
    unsigned = simpleTransaction
    accountPub = testAccountPub
    -- The address of the account at a path
    addrAt path = keyAddress Bip44 (derivePubPath (path :: SoftPath) accountPub)
    -- The coin of 1 btc at /0/0, and coins at the given paths of the
    -- account worth 100000 each, paying 50000000 out and the rest, less a
    -- fee of 10000000, to the change at the path
    changeAt path spentPaths =
      let coins = [fundingTx (fromIntegral (10 + i)) [(addrAt p, 100000)] | (i, p) <- zip [0 :: Int ..] spentPaths]
          spent = 100000000 + 100000 * fromIntegral (length spentPaths)
          tx = spendingTx ((txHash simpleFunding, 0) : [(txHash c, 0) | c <- coins]) [(otherAddr 0, 50000000), (addrAt path, spent - 60000000)]
       in TxSignData tx (simpleFunding : coins) (legacyPath <$> (Deriv :/ 0 :/ 0 : spentPaths)) [legacyPath path] Nothing Nothing [] noLabel
    simpleNosigHash = "9b41b3cb7b10bfd1dc3f7f1885ce3623789b1b946d206fd59fb432ad4fdb6d70"

{- Signing -}

signingSpec :: Spec
signingSpec =
  describe "Signing" $ do
    it "refuses to sign a transaction without fee" $ do
      -- The coin of 1 BTC is sent in full
      let tx = spendingTx [(txHash simpleFunding, 0)] [(otherAddr 0, 100000000)]
          unsigned = TxSignData tx [simpleFunding] [legacyPath (Deriv :/ 0 :/ 0)] [] Nothing Nothing [] noLabel
      signWalletTx btc unsigned [(Bip44, testAccountKey)] `shouldBe` Left "The transaction pays no fee"

    -- The coin of 100000 at the first receiving address of the bip84
    -- account, /0/0, spent to another wallet (60000) and to the first
    -- change address of the account (30000): 10000 of fee
    it "signs the P2WPKH coin of a bip84 account with the signature in the witness, verified with the digest of BIP 143" $ do
      estimate <- expectRight $ parseTxSignData btc [(Bip84, testSegwitAccountPub)] segwitUnsigned
      -- The estimate: 10 bytes of frame, 41 for the input, 34 and 31 for the
      -- outputs, then (2 + 108) / 4 of witness data, rounded up to 28
      estimate.txInfoSummary.summarySize `shouldBe` 144
      (signed, info) <- expectRight $ signWalletTx btc segwitUnsigned [(Bip84, testSegwitAccountKey)]
      let tx = signed.txSignDataTx
      map (.script) tx.inputs `shouldBe` [BS.empty]
      map length tx.witness `shouldBe` [2]
      verifySignedTx btc signed `shouldBe` Right ()
      -- 116 bytes outside the witness, which count four, and 109 or 110 of
      -- witness data, as the signature is 71 or 72 bytes: 143.25 or 143.5,
      -- rounded up
      info.txInfoSummary.summarySize `shouldBe` 144
      -- 10000 / 144 = 69.44
      (info.txInfoSummary.summaryType, info.txInfoSummary.summaryAmount, info.txInfoSummary.summaryFee, feeRateText info.txInfoSummary)
        `shouldBe` (TxDebit, -70000, 10000, "69.4")

    -- The payment of the bip84 account with its paths named as those of
    -- the bip44 account: /0/0 of the bip44 account is another address than
    -- the one the coin is at
    it "refuses a transaction whose paths name the bip44 account when its coin is at an address of the bip84 account" $ do
      let misnamed = segwitUnsigned {txSignDataInputPaths = [legacyPath (Deriv :/ 0 :/ 0)], txSignDataOutputPaths = [legacyPath (Deriv :/ 1 :/ 0)]}
      signWalletTx btc misnamed testPoolPrvKeys `shouldBe` Left "Input derivations don't match the transaction inputs"

    it "refuses a path of a bip84 account when no key of a bip84 account is given" $
      signWalletTx btc segwitUnsigned [(Bip44, testAccountKey)]
        `shouldBe` Left "The path bip84/0/0 is of a bip84 account, which the portfolio does not have on btc"

    -- The coins of 30000 at /0/0 of the bip44 account and of 20000 at /0/0
    -- of the bip84 account, spent together: 40000 to another wallet and
    -- 7050 to the first change address of the bip44 account. The fee is
    -- 50000 - 47050 = 2950: 10 satoshis for each of the 295 virtual bytes,
    -- 10 + 148 + 41 + 2 * 34 = 267 bytes and (2 + 1 + 108) / 4 of witness
    -- data, rounded up to 28.
    it "signs a payment that spends a bip44 and a bip84 coin together, which verifies" $ do
      (signed, info) <- expectRight $ signWalletTx btc bothTypesUnsigned testPoolPrvKeys
      isSigned signed.txSignDataTx `shouldBe` True
      verifySignedTx btc signed `shouldBe` Right ()
      (info.txInfoSummary.summaryType, info.txInfoSummary.summaryAmount, info.txInfoSummary.summaryFee) `shouldBe` (TxDebit, -42950, 2950)

    -- The public keys at m/44'/0'/0'/0/0 and m/84'/0'/0'/0/0 of the test
    -- wallet, derived apart from hw with the Python tool of the
    -- message-signing vectors. A P2PKH input pushes its signature, then
    -- its key, of 33 bytes, at the end of its script. A P2WPKH input has
    -- them in its witness.
    it "signs each input of such a payment with the key of its own account: the bip44 one in its script, the bip84 one in its witness" $ do
      (signed, _) <- expectRight $ signWalletTx btc bothTypesUnsigned testPoolPrvKeys
      let tx = signed.txSignDataTx
      [encodeHex (BS.takeEnd 33 input.script) | input <- tx.inputs]
        `shouldBe` ["032f6437022256c854e0090e7d6c8b65e96606ef7f405399e0f0187c93097ac0f9", ""]
      [encodeHex <$> drop 1 stack | stack <- tx.witness]
        `shouldBe` [[], ["027652defbc098321da485da6175de8b878f433f0d893343552625c9cd50933f53"]]

    it "tells the inputs of such a payment apart by the type of their paths, and estimates its size from an input of each type" $ do
      info <- expectRight $ parseTxSignData btc testPoolPubKeys bothTypesUnsigned
      fmap (.myInputsPath) info.txInfoSummary.summaryMyInputs
        `shouldBe` Map.fromList [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0)), (segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0))]
      (info.txInfoSummary.summarySize, info.txInfoSummary.summaryFee, feeRateText info.txInfoSummary) `shouldBe` (295, 2950, "10")

    -- The coin at /2/7, where the account has no address, spent to another
    -- wallet: the account's key derives a key there, which hw must not
    -- sign with
    it "refuses to sign a coin at a path of no address of the account" $ do
      let coin = fundingTx 2 [(addrAt27, 100000)]
          tx = spendingTx [(txHash coin, 0)] [(otherAddr 0, 90000)]
          tsd = TxSignData tx [coin] [legacyPath (Deriv :/ 2 :/ 7)] [] Nothing Nothing [] noLabel
      signWalletTx btc tsd [(Bip44, testAccountKey)] `shouldBe` Left "The path bip44/2/7 is of no address of the account: its addresses are at /0/n and /1/n"

{- Signing the inputs -}

-- | The inputs signed one by one, as signWalletTx does once the summary
-- has told the wallet's inputs from the others, each at the path of its
-- address as the summary lists them
signInputsSpec :: Spec
signInputsSpec =
  describe "Signing the inputs of a transaction" $ do
    -- The coin of 1 BTC at /0/0, spent by the simple transaction
    it "signs an input of the wallet with the key at the path of its address, the signature and the key in the script of a P2PKH coin" $ do
      signed <- expectRight $ signInputs btc [(Bip44, testAccountKey)] (mine Bip44 (externalAddr 0) "/0/0") unsigned coins
      map (BS.null . (.script)) signed.inputs `shouldBe` [False]
      signed.witness `shouldBe` []
      verifySignedTx btc simpleTransaction {txSignDataTx = signed} `shouldBe` Right ()

    it "signs a P2WPKH coin of a bip84 account with the signature and the key in the witness, the script left empty" $ do
      let tsd = segwitUnsigned
      segwitCoins <- expectRight $ spentCoins btc tsd
      signed <- expectRight $ signInputs btc [(Bip84, testSegwitAccountKey)] (mine Bip84 (segwitExternalAddr 0) "/0/0") tsd.txSignDataTx segwitCoins
      map (.script) signed.inputs `shouldBe` [BS.empty]
      map length signed.witness `shouldBe` [2]
      verifySignedTx btc tsd {txSignDataTx = signed} `shouldBe` Right ()

    it "signs with the sighash ALL on Bitcoin, and ALL with the FORKID flag on Bitcoin Cash" $ do
      onBitcoin <- expectRight $ signInputs btc [(Bip44, testAccountKey)] (mine Bip44 (externalAddr 0) "/0/0") unsigned coins
      onCash <- expectRight $ signInputs bch [(Bip44, testAccountKey)] (mine Bip44 (externalAddr 0) "/0/0") unsigned coins
      sigHashOf btc onBitcoin `shouldBe` Right 0x01
      sigHashOf bch onCash `shouldBe` Right 0x41
      verifySignedTx bch simpleTransaction {txSignDataTx = onCash} `shouldBe` Right ()

    -- The signatures are deterministic, as RFC 6979 draws their nonces
    it "signs an input of the wallet that is signed already again, to the same signature" $ do
      signedOnce <- expectRight $ signInputs btc [(Bip44, testAccountKey)] (mine Bip44 (externalAddr 0) "/0/0") unsigned coins
      signInputs btc [(Bip44, testAccountKey)] (mine Bip44 (externalAddr 0) "/0/0") signedOnce coins `shouldBe` Right signedOnce

    -- The coins at /0/0 and /1/3 of the wallet, and one of another wallet,
    -- which signed its input already
    it "signs every input of the wallet, each with the key of its own address, and leaves the signed input of another wallet as it is" $ do
      let threeCoinFunding = fundingTx 1 [(externalAddr 0, 60000), (internalAddr 3, 30000), (otherAddr 0, 50000)]
          theirs = withInputScript 2 "a signature of the other wallet" $ spendingTx [(txHash threeCoinFunding, 0), (txHash threeCoinFunding, 1), (txHash threeCoinFunding, 2)] [(otherAddr 1, 130000)]
      threeCoins <- expectRight $ spentCoins btc (TxSignData theirs [threeCoinFunding] [] [] Nothing Nothing [] noLabel)
      signed <- expectRight $ signInputs btc [(Bip44, testAccountKey)] (mine Bip44 (externalAddr 0) "/0/0" <> mine Bip44 (internalAddr 3) "/1/3") theirs threeCoins
      [verifyStdInput btc signed (spent threeCoins) i | i <- [0, 1]] `shouldBe` [True, True]
      signed.inputs !! 2 `shouldBe` theirs.inputs !! 2

    it "refuses an unsigned input that is not the wallet's, and says that the other signers sign first" $
      signInputs btc [(Bip44, testAccountKey)] Map.empty unsigned coins
        `shouldBe` Left "The coin of input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj) is not of this wallet, and the input is not signed yet: hw signs the coins of the wallet once the other signers have signed theirs"

    it "leaves a signed input that is not the wallet's as it is" $ do
      let theirs = withInputScript 0 "a signature of the other wallet" unsigned
      signInputs btc [(Bip44, testAccountKey)] Map.empty theirs coins `shouldBe` Right theirs

    -- A SegWit input has an empty script: its signature is in its witness
    it "leaves an input that is not the wallet's, signed in its witness, as it is" $ do
      let theirs = Tx unsigned.version unsigned.inputs unsigned.outputs [["a signature of the other wallet", "its key"]] unsigned.locktime
      signInputs btc [(Bip44, testAccountKey)] Map.empty theirs coins `shouldBe` Right theirs

    -- The coin of 1 btc at /0/0 of the account and one of 100000 at the
    -- first address of the bip84 account, a wallet of its own here, which
    -- has signed its input already, its signature in the witness
    it "signs the wallet's input beside an input another wallet signed in its witness, and the whole transaction verifies" $ do
      let theirFunding = fundingTx 2 [(segwitExternalAddr 0, 100000)]
          tx = spendingTx [(txHash simpleFunding, 0), (txHash theirFunding, 0)] [(otherAddr 0, 100090000)]
          unsignedBoth = TxSignData tx [simpleFunding, theirFunding] [] [] Nothing Nothing [] noLabel
      bothCoins <- expectRight $ spentCoins btc unsignedBoth
      -- The other wallet signs its input alone, with haskoin-core's signer
      let theirKey = wrapSecKey True (derivePath (Deriv :/ 0 :/ 0 :: SoftPath) testSegwitAccountKey).key
      theirsSigned <- expectRight $ signInput btc tx 1 (coinSigInput btc (bothCoins !! 1)) theirKey
      let tsd = unsignedBoth {txSignDataTx = theirsSigned, txSignDataInputPaths = [legacyPath (Deriv :/ 0 :/ 0)]}
      (signed, _) <- expectRight $ signWalletTx btc tsd [(Bip44, testAccountKey)]
      isSigned signed.txSignDataTx `shouldBe` True
      verifySignedTx btc signed `shouldBe` Right ()

    it "refuses a coin of the wallet that is not of the kind of the account its path names, naming the kind" $ do
      let tsd = segwitUnsigned
      segwitCoins <- expectRight $ spentCoins btc tsd
      signInputs btc [(Bip44, testAccountKey)] (mine Bip44 (segwitExternalAddr 0) "/0/0") tsd.txSignDataTx segwitCoins
        `shouldBe` Left "input 0 (bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc) is not a P2PKH coin"
      signInputs btc [(Bip84, testSegwitAccountKey)] (mine Bip84 (externalAddr 0) "/0/0") unsigned coins
        `shouldBe` Left "input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj) is not a P2WPKH coin"

    it "refuses an input of an account no key of which was given, naming the type of the account" $ do
      let tsd = segwitUnsigned
      segwitCoins <- expectRight $ spentCoins btc tsd
      signInputs btc [(Bip44, testAccountKey)] (mine Bip84 (segwitExternalAddr 0) "/0/0") tsd.txSignDataTx segwitCoins
        `shouldBe` Left "input 0 (bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc) is of a bip84 account, and no key of one was given"

    -- The summary checks that a path derives the address of the input; the
    -- signer takes the path as given. The key of another address is not
    -- the coin's, and haskoin-core's signer refuses to sign with it: its
    -- signature would spend nothing.
    it "refuses to sign with the key of the path given when the path is another address's, whose key is not the coin's" $
      signInputs btc [(Bip44, testAccountKey)] (mine Bip44 (externalAddr 0) "/0/1") unsigned coins
        `shouldBe` Left "signInput: the key is not a key of the coin"

    -- Output 1 of the funding transaction holds tokens of Bitcoin Cash.
    -- hw signs a digest that leaves the tokens out, which a node refuses,
    -- and a transaction of hw has no output to carry them.
    it "refuses a transaction that spends a coin that holds tokens, and says what moves the coin" $ do
      let spend = spendingTx [(txHash tokenFunding, 1)] [(otherAddr 0, 29000)]
      void (spentCoins bch (TxSignData spend [tokenFunding] [] [] Nothing Nothing [] noLabel))
        `shouldBe` Left "The transaction spends a coin that holds tokens (CashTokens). hw does not handle tokens and never spends such a coin: the tokens could be destroyed. Move the coin with a wallet that handles the tokens of Bitcoin Cash"

    -- Output 0 of the same transaction holds 50000 satoshis and no tokens
    it "reads the plain coin of a transaction whose other output holds tokens" $ do
      let spend = spendingTx [(txHash tokenFunding, 0)] [(otherAddr 0, 49000)]
      (map (.value) <$> spentCoins bch (TxSignData spend [tokenFunding] [] [] Nothing Nothing [] noLabel)) `shouldBe` Right [50000]

    -- The coin at /2/7, where the account has no address: refused before
    -- any key is derived, whatever the summary said of the coin
    it "refuses to derive a key at a path of no address of the account, even told the coin is the wallet's" $ do
      let hiddenCoin = fundingTx 2 [(addrAt27, 100000)]
          hiddenSpend = spendingTx [(txHash hiddenCoin, 0)] [(otherAddr 0, 90000)]
      hiddenCoins <- expectRight $ spentCoins btc (TxSignData hiddenSpend [hiddenCoin] [] [] Nothing Nothing [] noLabel)
      signInputs btc [(Bip44, testAccountKey)] (mine Bip44 addrAt27 "/2/7") hiddenSpend hiddenCoins
        `shouldBe` Left "The path bip44/2/7 is of no address of the account: its addresses are at /0/n and /1/n"
  where
    unsigned = simpleTransaction.txSignDataTx
    coins = forceRight (spentCoins btc simpleTransaction)
    -- The wallet's input at the address, at the path of the account of the
    -- kind, as the summary lists it
    mine kind addr path = Map.singleton addr (MyInputs 0 (PoolPath kind path) "" [])
    -- The coins as haskoin-core's verifier takes them
    spent :: [SpentCoin] -> [(ScriptOutput, Word64, OutPoint)]
    spent spentCoins' = [(c.script, c.value, c.outpoint) | c <- spentCoins']
    -- The transaction with the script of input i replaced, as another
    -- signer leaves the input it signed
    withInputScript :: Int -> BS.ByteString -> Tx -> Tx
    withInputScript i script tx =
      Tx tx.version [if j == i then TxIn input.outpoint script input.sequence else input | (j, input) <- zip [0 ..] tx.inputs] tx.outputs tx.witness tx.locktime
    -- The sighash of the signature in the script of the first input
    sigHashOf :: Network -> Tx -> Either String SigHash
    sigHashOf net tx = case unmarshal net (tx.inputs !! 0).script :: Either String ScriptInput of
      Right (RegularInput (SpendPKHash (TxSignature _ sh) _)) -> Right sh
      other -> Left (show other)

{- Verification -}

verificationSpec :: Spec
verificationSpec =
  describe "Verification of a signed transaction" $ do
    it "accepts a correctly signed transaction" $ do
      signed <- signedTransaction
      verifySignedTx btc signed `shouldBe` Right ()
    it "rejects a transaction altered after signing" $ do
      signed <- signedTransaction
      verifySignedTx btc (withLockTime 1 signed) `shouldBe` Left invalidInput0
    it "rejects a transaction without signatures" $ do
      signed <- signedTransaction
      verifySignedTx btc (withoutSignatures signed) `shouldBe` Left invalidInput0
    -- Another signer can sign with ANYONECANPAY, a valid signature that
    -- commits to its own input alone, and with NONE to no output at all
    it "refuses a signature with a sighash other than ALL, naming both" $ do
      signed <- expectRight $ signedWithAnyoneCanPay
      verifySignedTx btc signed `shouldBe` Left "The signature of input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj) has the sighash ALL|ANYONECANPAY, not ALL"
    -- The signature of a P2PKH coin does not commit to its amount, so a
    -- transaction that pays 2 BTC from the coin of 1 BTC can be signed.
    -- No node accepts it: the transaction is verified as a whole as well.
    it "refuses a signed transaction that pays more than its coins hold" $ do
      let overpaying = spendingTx [(txHash simpleFunding, 0)] [(otherAddr 0, 200000000)]
          unsigned = TxSignData overpaying [simpleFunding] [legacyPath (Deriv :/ 0 :/ 0)] [] Nothing Nothing [] noLabel
      coins <- expectRight $ spentCoins btc unsigned
      signed <- expectRight $ signInputs btc [(Bip44, testAccountKey)] (Map.singleton (externalAddr 0) (MyInputs 0 (legacyPath "/0/0") "" [])) overpaying coins
      verifySignedTx btc unsigned {txSignDataTx = signed}
        `shouldBe` Left "The transaction does not verify: it spends a coin twice, or pays more than its coins hold"
  where
    signedTransaction = fst <$> expectRight (signWalletTx btc simpleTransaction [(Bip44, testAccountKey)])
    invalidInput0 = "Invalid signature for input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj)"
    withLockTime lockTime signed =
      let tx = signed.txSignDataTx
       in signed {txSignDataTx = Tx tx.version tx.inputs tx.outputs tx.witness lockTime}
    withoutSignatures signed =
      let tx = signed.txSignDataTx
          unsigned :: TxIn -> TxIn
          unsigned input = TxIn input.outpoint BS.empty input.sequence
       in signed {txSignDataTx = Tx tx.version (unsigned <$> tx.inputs) tx.outputs tx.witness tx.locktime}

{- Properties -}

propertiesSpec :: Spec
propertiesSpec =
  describe "Signing properties" $
    prop "signs and verifies any transaction of the portfolio, whatever the types of its coins" $
      forAll walletTransaction $ \(tsd, expected) ->
        case signWalletTx btc tsd testPoolPrvKeys of
          Left err -> counterexample ("hw refused to sign: " <> err) False
          Right (signed, info) ->
            let inputs = length signed.txSignDataTx.inputs
                estimated = fromIntegral $ txSize expected.expectedInputs expected.expectedOutputs
             in verifySignedTx btc signed === Right ()
                  .&&. info.txInfoSummary.summaryFee === expected.expectedFee
                  .&&. info.txInfoSummary.summaryAmount === expected.expectedAmount
                  .&&. info.txInfoSummary.summaryType === expected.expectedType
                  -- The real size: at most the estimate, and at most a few
                  -- bytes per signature below it
                  .&&. counterexample ("size " <> show info.txInfoSummary.summarySize <> " for an estimate of " <> show estimated)
                    (info.txInfoSummary.summarySize <= estimated && info.txInfoSummary.summarySize + 3 * fromIntegral inputs >= estimated)

-- | What the analysis must report for a generated transaction, computed from
-- the generated amounts, and the kinds of its inputs and outputs, from the
-- types of the addresses drawn
data Expected = Expected
  { expectedFee :: !Natural,
    expectedAmount :: !Integer,
    expectedType :: !TxType,
    expectedInputs :: ![InputKind],
    expectedOutputs :: ![OutputKind]
  }
  deriving (Show)

-- | A transaction of portfolio 0 of the test wallet on Bitcoin: one to six
-- coins at random addresses of its bip44 and of its bip84 account, spent to
-- one to four outputs of random amounts, each paid to another wallet or
-- back to the portfolio. The outputs never reach the coins, so the fee is
-- always positive.
walletTransaction :: Gen (TxSignData, Expected)
walletTransaction = do
  coinCount <- choose (1, 6)
  coins <- vectorOf coinCount $ (,) <$> elements walletAddresses <*> amount 100000 100000000
  let coinsTotal = sum (snd <$> coins)
  outputCount <- choose (1, 4)
  outputs <-
    vectorOf outputCount $
      (,)
        <$> frequency [(2, elements otherAddrs), (1, elements walletAddresses)]
        <*> amount 1 (coinsTotal `div` fromIntegral outputCount - 1)
  let funding = fundingTx 1 coins
      tx = spendingTx [(txHash funding, i) | i <- [0 .. fromIntegral coinCount - 1]] outputs
      pathsOf addrs = nub $ mapMaybe (`lookup` walletPaths) addrs
      paid = sum (snd <$> outputs)
      backToWallet = sum [v | (addr, v) <- outputs, isJust (lookup addr walletPaths)]
      fee = coinsTotal - paid
      net = toInteger backToWallet - toInteger coinsTotal
      kind
        | net > 0 = TxCredit
        | negate net == toInteger fee = TxInternal
        | otherwise = TxDebit
      -- The addresses of other wallets are P2PKH ones
      kindOf addr = maybe Bip44 (.pathKind) (lookup addr walletPaths)
  return
    ( TxSignData tx [funding] (pathsOf (fst <$> coins)) (pathsOf (fst <$> outputs)) Nothing Nothing [] noLabel,
      Expected
        { expectedFee = fee,
          expectedAmount = net,
          expectedType = kind,
          expectedInputs = accountInput . kindOf . fst <$> coins,
          expectedOutputs = accountOutputKind . kindOf . fst <$> outputs
        }
    )
  where
    amount :: Int -> Natural -> Gen Natural
    amount low high = fromIntegral <$> choose (low, fromIntegral high)
    walletAddresses = fst <$> walletPaths
    walletPaths =
      [(externalAddr i, legacyPath (Deriv :/ 0 :/ fromIntegral i)) | i <- [0 .. 9 :: Int]]
        <> [(internalAddr i, legacyPath (Deriv :/ 1 :/ fromIntegral i)) | i <- [0 .. 9 :: Int]]
        <> [(segwitExternalAddr i, segwitPath (Deriv :/ 0 :/ fromIntegral i)) | i <- [0 .. 9 :: Int]]
        <> [(segwitInternalAddr i, segwitPath (Deriv :/ 1 :/ fromIntegral i)) | i <- [0 .. 9 :: Int]]
