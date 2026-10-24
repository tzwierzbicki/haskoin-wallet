{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.TxSigningSpec where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Haskoin
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo (MyInputs (..), MyOutputs (..), TxInfo (..), TxSummary (..), TxType (..), txInfoHash)
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning
import Haskoin.Wallet.TxSize (p2pkhInput, p2pkhOutputSize, txSize)
import Haskoin.Wallet.TxVectors
import Numeric.Natural (Natural)
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

-- | Sign with the account key, like signtx does. Vectors with sweep keys
-- belong in test/data/tx-sweep.
walletSigner :: VectorSigner
walletSigner ctx v = do
  when (isJust v.vecSweepKeys) $
    Left "this is a sweep vector: it belongs in test/data/tx-sweep"
  accountKey <- prefixError "account" $ accountSigningKey v.vecNetwork ctx v.vecAccount
  return $ \c -> signWalletTx v.vecNetwork c v.vecSignData accountKey

{- Analysis -}

-- | The transaction of the tests below: the wallet's coin of 1 BTC at its
-- first external address, spent to another wallet (0.5 BTC) and back to the
-- wallet's first internal address (0.4 BTC), with a fee of 0.1 BTC. It is
-- the hw/btc/debit vector of test/data/tx-signing.
simpleTransaction :: Ctx -> TxSignData
simpleTransaction ctx =
  TxSignData tx [simpleFunding ctx] [Deriv :/ 0 :/ 0] [Deriv :/ 1 :/ 0] Nothing
  where
    tx = spendingTx ctx [(txHash (simpleFunding ctx), 0)] [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]

-- | The transaction funding the wallet's coin of 'simpleTransaction'
simpleFunding :: Ctx -> Tx
simpleFunding ctx = fundingTx ctx 1 [(externalAddr 0, 100000000)]

analysisSpec :: Ctx -> Spec
analysisSpec ctx =
  describe "Transaction analysis" $ do
    it "describes an unsigned transaction with an estimated size" $ do
      info <- expectRight $ parseTxSignData ctx accountPub unsigned
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
      info <- expectRight $ parseTxSignData ctx accountPub signed
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
          tsd = TxSignData tx [funding] [Deriv :/ 0 :/ 0] [] (Just 10)
      info <- expectRight $ parseTxSignData ctx accountPub tsd
      info.txInfoSummary.summaryFee `shouldBe` 2500
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 580
      (signed, signedInfo) <- expectRight $ signWalletTx btc ctx tsd testAccountKey
      signed.txSignDataFeeRate `shouldBe` Just 10
      signedInfo.txInfoSummary.summaryChangeInFee `shouldBe` Just 580
    it "reports no change in the fee when the fee matches the requested rate" $ do
      -- A coin of 351920 pays 350000: 1920 of fee, exactly 192 bytes * 10
      let funding = fundingTx ctx 1 [(externalAddr 0, 351920)]
          tx = spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 350000)]
          tsd = TxSignData tx [funding] [Deriv :/ 0 :/ 0] [] (Just 10)
      info <- expectRight $ parseTxSignData ctx accountPub tsd
      info.txInfoSummary.summaryChangeInFee `shouldBe` Just 0
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
          unsigned = TxSignData tx [simpleFunding ctx] [Deriv :/ 0 :/ 0] [] Nothing
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
    ( TxSignData tx [funding] (pathsOf (fst <$> coins)) (pathsOf (fst <$> outputs)) Nothing,
      Expected {expectedFee = fee, expectedAmount = net, expectedType = kind}
    )
  where
    amount :: Int -> Natural -> Gen Natural
    amount low high = fromIntegral <$> choose (low, fromIntegral high)
    walletAddresses = fst <$> walletPaths
    walletPaths =
      [(externalAddr i, Deriv :/ 0 :/ fromIntegral i) | i <- [0 .. 9 :: Int]]
        <> [(internalAddr i, Deriv :/ 1 :/ fromIntegral i) | i <- [0 .. 9 :: Int]]
