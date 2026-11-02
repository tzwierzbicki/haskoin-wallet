{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Tx.SigningSpec where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (parseMaybe)
import Data.List (isInfixOf, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Haskoin
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo (MyInputs (..), MyOutputs (..), TxInfo (..), TxSummary (..), TxType (..), txInfoHash)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing
import Haskoin.Wallet.Tx.Size (p2pkhInput, p2pkhOutputSize, txSize)
import Haskoin.Wallet.TxVectors
import Numeric.Natural (Natural)
import Data.Text (Text)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), MarshalJsonBox (..), testIdentity)
import Data.Default (def)
import Haskoin.Wallet.Arbitrary (arbitraryTxFile, arbitraryTxSignData)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- The signing vectors live in JSON files in test/data/tx-signing (see the
-- README.md file there for the format and the origin of the vectors). Every
-- .json file is loaded, so new vectors can be added without changing this
-- module.
spec :: Spec
spec = do
  txVectorSpec "test/data/tx-signing" "Transaction signing test vectors" walletSigner
  prepareContext $ \ctx -> do
    analysisSpec ctx
    signingSpec ctx
    verificationSpec ctx
    propertiesSpec ctx
    fileSpec ctx
    -- The signing data and its files read back as they were written, the
    -- signing data plain or marshalled
    testIdentity
      def
        { jsonTests = [JsonBox (arbitraryTxSignData btc ctx), JsonBox (arbitraryTxFile ctx)],
          marshalJsonTests = [MarshalJsonBox ((,) ctx <$> arbitraryTxSignData btc ctx)]
        }

-- | Sign with the account key, like signtx does. Vectors with sweep keys
-- belong in test/data/tx-sweep.
walletSigner :: VectorSigner
walletSigner ctx v = do
  when (isJust v.vecSweepKeys) $
    Left "this is a sweep vector: it belongs in test/data/tx-sweep"
  accountKey <- prefixError "account" $ accountSigningKey v.vecNetwork ctx v.vecAccount
  return $ \c -> signWalletTx v.vecNetwork c v.vecSignData accountKey

{- Files -}

fileSpec :: Ctx -> Spec
fileSpec ctx =
  describe "A transaction file" $ do
    let tsd = simpleTransaction ctx
    it "names the network of the transaction, which an earlier hw leaves aside" $ do
      let written = Json.encode (plainTxFile (Just btc) tsd)
      (Json.decode written >>= parseMaybe (.: "network")) `shouldBe` Just ("btc" :: Text)
      Json.eitherDecode written `shouldBe` Right tsd

    it "of an earlier hw names no network" $
      Json.eitherDecode (Json.encode tsd) `shouldBe` Right (plainTxFile Nothing tsd)

    -- As an earlier hw wrote it, for any transaction but a replacement
    it "names no transaction replaced, but for a replacement" $
      (Json.decode (Json.encode tsd) >>= parseMaybe (.: "replaces")) `shouldBe` (Nothing :: Maybe [TxHash])

    it "of a replacement names the transactions it replaces, and reads back" $ do
      let replacement = tsd {txSignDataReplaces = [fakeTxHash 3, fakeTxHash 4]}
          written = Json.encode replacement
      (Json.decode written >>= parseMaybe (.: "replaces"))
        `shouldBe` Just ["0000000000000000000000000000000000000000000000000000000000000003", "0000000000000000000000000000000000000000000000000000000000000004" :: Text]
      Json.eitherDecode written `shouldBe` Right replacement

    -- As an earlier hw wrote it, for any transaction the user did not label
    it "holds no label, but for a transaction the user labelled" $
      (Json.decode (Json.encode tsd) >>= parseMaybe (.: "label")) `shouldBe` (Nothing :: Maybe Text)

    it "of a labelled transaction holds its label, and reads back" $ do
      let labelled = tsd {txSignDataLabel = "Rent of October"}
          written = Json.encode labelled
      (Json.decode written >>= parseMaybe (.: "label")) `shouldBe` Just ("Rent of October" :: Text)
      Json.eitherDecode written `shouldBe` Right labelled

    it "of an earlier hw, without a label, is read as a transaction without one" $
      (txSignDataLabel <$> Json.eitherDecode (Json.encode (Json.object (txSignDataPairs tsd)))) `shouldBe` Right ""

    it "refuses a network hw does not know" $ do
      let written = Json.encode (Json.object (("network" .= ("doge" :: Text)) : txSignDataPairs tsd))
      (Json.eitherDecode written :: Either String TxFile) `shouldSatisfy` either ("Unknown network doge" `isInfixOf`) (const False)

    -- As an earlier hw wrote it, for a transaction whose addresses have no
    -- label
    it "labels no address, but for the account's addresses that have a label" $
      (Json.decode (Json.encode (plainTxFile (Just btc) tsd)) >>= parseMaybe (.: "pathlabels")) `shouldBe` (Nothing :: Maybe (Map.Map Text Text))

    -- The coin spent is at the account's first receiving address, /0/0.
    -- The file names the path, never the address.
    it "carries the labels of the account's addresses by their paths, and reads back" $ do
      let labelled = TxFile (Just btc) tsd (Map.fromList [("/0/0", "Salary")]) Map.empty
          written = Json.encode labelled
      (Json.decode written >>= parseMaybe (.: "pathlabels")) `shouldBe` Just (Map.fromList [("/0/0", "Salary")] :: Map.Map Text Text)
      Json.eitherDecode written `shouldBe` Right labelled

    it "of an earlier hw labels no address" $
      (txFilePathLabels <$> Json.eitherDecode (Json.encode (Json.object (txSignDataPairs tsd)))) `shouldBe` Right Map.empty

    -- A label can name a path only: one at an address is not a label of
    -- the file, however much the address is the account's
    it "refuses a label at something that is not a path" $ do
      let written = Json.encode (Json.object (("pathlabels" .= Map.fromList [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" :: Text, "Salary" :: Text)]) : txSignDataPairs tsd))
      (Json.eitherDecode written :: Either String TxFile) `shouldSatisfy` either ("Invalid path 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `isInfixOf`) (const False)

    -- The coin spent is at the first receiving address, /0/0, and the
    -- change goes to the first change address, /1/0, which the file does
    -- not label
    it "is read as its account sees it, with the labels it carries" $ do
      let labelled = TxFile (Just btc) tsd (Map.fromList [("/0/0", "Salary")]) Map.empty
      info <- expectRight $ parseTxFile btc ctx (testAccountPub ctx) labelled
      fmap (.myInputsLabel) info.txInfoSummary.summaryMyInputs `shouldBe` Map.fromList [(externalAddr 0, "Salary")]
      fmap (.myOutputsLabel) info.txInfoSummary.summaryMyOutputs `shouldBe` Map.fromList [(internalAddr 0, "")]

    -- /0/1 is the account's second receiving address and /1/3 its fourth
    -- change address. The transaction touches neither.
    it "is read without the labels of paths the transaction does not touch" $ do
      let labelled = TxFile (Just btc) tsd (Map.fromList [("/0/1", "Elsewhere"), ("/1/3", "Other change")]) Map.empty
      info <- expectRight $ parseTxFile btc ctx (testAccountPub ctx) labelled
      fmap (.myInputsLabel) info.txInfoSummary.summaryMyInputs `shouldBe` Map.fromList [(externalAddr 0, "")]
      fmap (.myOutputsLabel) info.txInfoSummary.summaryMyOutputs `shouldBe` Map.fromList [(internalAddr 0, "")]

    -- The change address, /1/0, has no label, so the file leaves it out
    it "is written with the labels of the account's addresses that have one, by their paths" $ do
      info <- expectRight $ parseTxSignData btc ctx (testAccountPub ctx) tsd
      let salaried = info {txInfoSummary = info.txInfoSummary {summaryMyInputs = Map.map (\i -> i {myInputsLabel = "Salary"}) info.txInfoSummary.summaryMyInputs}}
      txFilePathLabels (labelledTxFile btc salaried tsd) `shouldBe` Map.fromList [("/0/0", "Salary")]

    -- As an earlier hw wrote it, for a transaction whose coins show no label
    it "labels no coin, but for the coins that show a label" $
      (Json.decode (Json.encode (plainTxFile (Just btc) tsd)) >>= parseMaybe (.: "inputlabels")) `shouldBe` (Nothing :: Maybe (Map.Map Text Text))

    it "carries the labels the coins it spends show, by the id of the transaction that paid each, and reads back" $ do
      let labelled = TxFile (Just btc) tsd Map.empty (Map.fromList [(fakeTxHash 3, "Salary")])
          written = Json.encode labelled
      (Json.decode written >>= parseMaybe (.: "inputlabels")) `shouldBe` Just (Map.fromList [("0000000000000000000000000000000000000000000000000000000000000003", "Salary")] :: Map.Map Text Text)
      Json.eitherDecode written `shouldBe` Right labelled

    it "of an earlier hw labels no coin" $
      (txFileInputLabels <$> Json.eitherDecode (Json.encode (Json.object (txSignDataPairs tsd)))) `shouldBe` Right Map.empty

    -- The coin spent is of the funding transaction. Transaction 3 is not
    -- spent, so its label is left aside.
    it "is read with the labels of the coins it spends, and without those of other transactions" $ do
      let labelled = TxFile (Just btc) tsd Map.empty (Map.fromList [(txHash (simpleFunding ctx), "Salary"), (fakeTxHash 3, "Elsewhere")])
      info <- expectRight $ parseTxFile btc ctx (testAccountPub ctx) labelled
      Map.toList info.txInfoInputLabels `shouldBe` [(txHash (simpleFunding ctx), "Salary")]

    it "is written with the labels the coins it spends show" $ do
      info <- expectRight $ parseTxSignData btc ctx (testAccountPub ctx) tsd
      let salaried = info {txInfoInputLabels = Map.fromList [(txHash (simpleFunding ctx), "Salary")]}
      Map.toList (txFileInputLabels (labelledTxFile btc salaried tsd)) `shouldBe` [(txHash (simpleFunding ctx), "Salary")]

{- Analysis -}

-- | The transaction of the tests below: the wallet's coin of 1 BTC at its
-- first external address, spent to another wallet (0.5 BTC) and back to the
-- wallet's first internal address (0.4 BTC), with a fee of 0.1 BTC. It is
-- the hw/btc/debit vector of test/data/tx-signing.
simpleTransaction :: Ctx -> TxSignData
simpleTransaction ctx =
  TxSignData tx [simpleFunding ctx] [Deriv :/ 0 :/ 0] [Deriv :/ 1 :/ 0] Nothing Nothing [] noLabel
  where
    tx = spendingTx ctx [(txHash (simpleFunding ctx), 0)] [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]

-- | The transaction funding the wallet's coin of 'simpleTransaction'
simpleFunding :: Ctx -> Tx
simpleFunding ctx = fundingTx ctx 1 [(externalAddr 0, 100000000)]

-- | The simple transaction signed with SIGHASH_ALL and ANYONECANPAY, as
-- another signer could sign it: a valid signature that does not commit to
-- the whole transaction
signedWithAnyoneCanPay :: Ctx -> Either String TxSignData
signedWithAnyoneCanPay ctx = do
  script <- maybeToEither "The address has no script" $ addressToOutput (externalAddr 0)
  let unsigned = simpleTransaction ctx
      coin = SigInput script 100000000 (OutPoint (txHash (simpleFunding ctx)) 0) (setAnyoneCanPay sigHashAll) Nothing
      key = (derivePath ctx (Deriv :/ 0 :/ 0 :: SoftPath) testAccountKey).key
  tx <- signTx btc ctx unsigned.txSignDataTx [coin] [key]
  return unsigned {txSignDataTx = tx}

analysisSpec :: Ctx -> Spec
analysisSpec ctx =
  describe "Transaction analysis" $ do
    -- The signer sees it, and so can refuse a lock time it did not expect
    it "carries the lock time of the transaction" $ do
      let tsd = simpleTransaction ctx
          tx = tsd.txSignDataTx
          locked = tsd {txSignDataTx = Tx tx.version tx.inputs tx.outputs tx.witness 814037}
      info <- expectRight $ parseTxSignData btc ctx (testAccountPub ctx) locked
      info.txInfoSummary.summaryLockTime `shouldBe` 814037
      plain <- expectRight $ parseTxSignData btc ctx (testAccountPub ctx) tsd
      plain.txInfoSummary.summaryLockTime `shouldBe` 0

    -- Its paths are those of an account the wallet does not have: the coin
    -- of 1 BTC and the two outputs are another wallet's
    it "describes a transaction outside the wallet, every input and output another wallet's" $ do
      info <- expectRight $ parseOutsideTxSignData btc ctx unsigned
      info.txInfoSummary.summaryAmount `shouldBe` 0
      info.txInfoSummary.summaryFee `shouldBe` 10000000
      info.txInfoSummary.summaryOtherInputs `shouldBe` Map.fromList [(externalAddr 0, 100000000)]
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]
      (Map.null info.txInfoSummary.summaryMyInputs, Map.null info.txInfoSummary.summaryMyOutputs) `shouldBe` (True, True)

    it "describes an unsigned transaction with an estimated size" $ do
      info <- expectRight $ parseTxSignData btc ctx accountPub unsigned
      txInfoHash info `shouldBe` Nothing
      info.txInfoSummary.summaryType `shouldBe` TxDebit
      info.txInfoSummary.summaryAmount `shouldBe` (-60000000)
      info.txInfoSummary.summaryFee `shouldBe` 10000000
      -- The estimate: 10 bytes, 148 for the input and 34 for each output
      info.txInfoSummary.summarySize `shouldBe` 226
      info.txInfoSummary.summaryFeeByte `shouldBe` 44247
      pendingFlags info `shouldBe` Just ((simpleNosigHash, False, False, False))
      -- Without the requested fee rate, no change in the fee can be told
      info.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
      -- The wallet's input, with the coin it spends, and its change
      info.txInfoSummary.summaryMyInputs
        `shouldBe` Map.fromList [(externalAddr 0, MyInputs 100000000 (Deriv :/ 0 :/ 0) "" [OutPoint (txHash (simpleFunding ctx)) 0])]
      info.txInfoSummary.summaryMyOutputs `shouldBe` Map.fromList [(internalAddr 0, MyOutputs 40000000 (Deriv :/ 1 :/ 0) "")]
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(otherAddr 0, 50000000)]
      info.txInfoSummary.summaryOtherInputs `shouldBe` Map.empty
      info.txInfoSummary.summaryNonStdInputs `shouldBe` []
      info.txInfoSummary.summaryNonStdOutputs `shouldBe` []
    it "describes the signed transaction with its real size" $ do
      (signed, _) <- expectRight $ signWalletTx btc ctx unsigned testAccountKey
      info <- expectRight $ parseTxSignData btc ctx accountPub signed
      txInfoHash info `shouldBe` Just "e66b790c73d4e72fe13a07e247e4439bdea210cac2f947040e901f1e0ce59ac2"
      -- The signature of this transaction is 72 bytes long, which the
      -- estimate assumes: 10 + 148 + 2 * 34
      info.txInfoSummary.summarySize `shouldBe` 226
      info.txInfoSummary.summaryFeeByte `shouldBe` 44247
      pendingFlags info `shouldBe` Just ((simpleNosigHash, True, False, False))
    it "reports the change too small for an output that the fee includes" $ do
      -- A coin of 352500 pays 350000 at 10 satoshis per byte: the fee is
      -- 2500, 580 beyond the 10 + 148 + 34 = 192 bytes * 10 = 1920 of the
      -- transaction. Signing does not change it: the estimated size counts.
      let funding = fundingTx ctx 1 [(externalAddr 0, 352500)]
          tx = spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 350000)]
          tsd = TxSignData tx [funding] [Deriv :/ 0 :/ 0] [] (Just 10) Nothing [] noLabel
      info <- expectRight $ parseTxSignData btc ctx accountPub tsd
      info.txInfoSummary.summaryFee `shouldBe` 2500
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 580
      (signed, signedInfo) <- expectRight $ signWalletTx btc ctx tsd testAccountKey
      signed.txSignDataFeeRate `shouldBe` Just 10
      signedInfo.txInfoSummary.summaryChangeInFee `shouldBe` Just 580
    it "reports no change in the fee when the fee matches the requested rate" $ do
      -- A coin of 351920 pays 350000: 1920 of fee, exactly 192 bytes * 10
      let funding = fundingTx ctx 1 [(externalAddr 0, 351920)]
          tx = spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 350000)]
          tsd = TxSignData tx [funding] [Deriv :/ 0 :/ 0] [] (Just 10) Nothing [] noLabel
      info <- expectRight $ parseTxSignData btc ctx accountPub tsd
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 0
    it "refuses an output at an address the network cannot write, a SegWit one on Bitcoin Cash" $ do
      -- The same script is a payment to a SegWit address on Bitcoin, and
      -- one to nobody the review could name on Bitcoin Cash
      let tx = spendingTx ctx [(txHash (simpleFunding ctx), 0)] [(segwitAddr, 50000000), (internalAddr 0, 40000000)]
          tsd = TxSignData tx [simpleFunding ctx] [Deriv :/ 0 :/ 0] [Deriv :/ 1 :/ 0] Nothing Nothing [] noLabel
      parseTxSignData bch ctx accountPub tsd `shouldBe` Left "There are non-standard outputs in the transaction"
      info <- expectRight $ parseTxSignData btc ctx accountPub tsd
      info.txInfoSummary.summaryOtherOutputs `shouldBe` Map.fromList [(segwitAddr, 50000000)]
  where
    unsigned = simpleTransaction ctx
    accountPub = testAccountPub ctx
    simpleNosigHash = "9b41b3cb7b10bfd1dc3f7f1885ce3623789b1b946d206fd59fb432ad4fdb6d70"

{- Signing -}

signingSpec :: Ctx -> Spec
signingSpec ctx =
  describe "Signing" $
    it "refuses to sign a transaction without fee" $ do
      -- The coin of 1 BTC is sent in full
      let tx = spendingTx ctx [(txHash (simpleFunding ctx), 0)] [(otherAddr 0, 100000000)]
          unsigned = TxSignData tx [simpleFunding ctx] [Deriv :/ 0 :/ 0] [] Nothing Nothing [] noLabel
      signWalletTx btc ctx unsigned testAccountKey `shouldBe` Left "The transaction pays no fee"

{- Verification -}

verificationSpec :: Ctx -> Spec
verificationSpec ctx =
  describe "Verification of a signed transaction" $ do
    it "accepts a correctly signed transaction" $ do
      signed <- signedTransaction
      verifySignedTx btc ctx signed `shouldBe` Right ()
    it "rejects a transaction altered after signing" $ do
      signed <- signedTransaction
      verifySignedTx btc ctx (withLockTime 1 signed) `shouldBe` Left invalidInput0
    it "rejects a transaction without signatures" $ do
      signed <- signedTransaction
      verifySignedTx btc ctx (withoutSignatures signed) `shouldBe` Left invalidInput0
    -- Another signer can sign with ANYONECANPAY, a valid signature that
    -- commits to its own input alone, and with NONE to no output at all
    it "refuses a signature with a sighash other than ALL, naming both" $ do
      signed <- expectRight $ signedWithAnyoneCanPay ctx
      verifySignedTx btc ctx signed `shouldBe` Left "The signature of input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj) has the sighash ALL|ANYONECANPAY, not ALL"
  where
    signedTransaction = fst <$> expectRight (signWalletTx btc ctx (simpleTransaction ctx) testAccountKey)
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

propertiesSpec :: Ctx -> Spec
propertiesSpec ctx =
  describe "Signing properties" $
    prop "signs and verifies any transaction of the wallet" $
      forAll (walletTransaction ctx) $ \(tsd, expected) ->
        case signWalletTx btc ctx tsd testAccountKey of
          Left err -> counterexample ("hw refused to sign: " <> err) False
          Right (signed, info) ->
            let inputs = length signed.txSignDataTx.inputs
                outputs = length signed.txSignDataTx.outputs
                estimated = fromIntegral $ txSize (replicate inputs p2pkhInput) (replicate outputs p2pkhOutputSize)
             in verifySignedTx btc ctx signed === Right ()
                  .&&. info.txInfoSummary.summaryFee === expected.expectedFee
                  .&&. info.txInfoSummary.summaryAmount === expected.expectedAmount
                  .&&. info.txInfoSummary.summaryType === expected.expectedType
                  -- The real size: at most the estimate, and at most a few
                  -- bytes per signature below it
                  .&&. counterexample ("size " <> show info.txInfoSummary.summarySize <> " for an estimate of " <> show estimated)
                    (info.txInfoSummary.summarySize <= estimated && info.txInfoSummary.summarySize + 3 * fromIntegral inputs >= estimated)

-- | What the analysis must report for a generated transaction, computed from
-- the generated amounts
data Expected = Expected
  { expectedFee :: !Natural,
    expectedAmount :: !Integer,
    expectedType :: !TxType
  }
  deriving (Show)

-- | A transaction of the test wallet: one to six coins at random wallet
-- addresses, spent to one to four outputs of random amounts, each paid to
-- another wallet or back to the test wallet. The outputs never reach the
-- coins, so the fee is always positive.
walletTransaction :: Ctx -> Gen (TxSignData, Expected)
walletTransaction ctx = do
  coinCount <- choose (1, 6)
  coins <- vectorOf coinCount $ (,) <$> elements walletAddresses <*> amount 100000 100000000
  let coinsTotal = sum (snd <$> coins)
  outputCount <- choose (1, 4)
  outputs <-
    vectorOf outputCount $
      (,)
        <$> frequency [(2, elements otherAddrs), (1, elements walletAddresses)]
        <*> amount 1 (coinsTotal `div` fromIntegral outputCount - 1)
  let funding = fundingTx ctx 1 coins
      tx = spendingTx ctx [(txHash funding, i) | i <- [0 .. fromIntegral coinCount - 1]] outputs
      pathsOf addrs = nub $ mapMaybe (`lookup` walletPaths) addrs
      paid = sum (snd <$> outputs)
      backToWallet = sum [v | (addr, v) <- outputs, isJust (lookup addr walletPaths)]
      fee = coinsTotal - paid
      net = toInteger backToWallet - toInteger coinsTotal
      kind
        | net > 0 = TxCredit
        | negate net == toInteger fee = TxInternal
        | otherwise = TxDebit
  return
    ( TxSignData tx [funding] (pathsOf (fst <$> coins)) (pathsOf (fst <$> outputs)) Nothing Nothing [] noLabel,
      Expected {expectedFee = fee, expectedAmount = net, expectedType = kind}
    )
  where
    amount :: Int -> Natural -> Gen Natural
    amount low high = fromIntegral <$> choose (low, fromIntegral high)
    walletAddresses = fst <$> walletPaths
    walletPaths =
      [(externalAddr i, Deriv :/ 0 :/ fromIntegral i) | i <- [0 .. 9 :: Int]]
        <> [(internalAddr i, Deriv :/ 1 :/ fromIntegral i) | i <- [0 .. 9 :: Int]]
