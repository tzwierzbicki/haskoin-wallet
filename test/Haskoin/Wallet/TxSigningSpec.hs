{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.TxSigningSpec where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Haskoin
import Haskoin.Wallet.FileIO (TxSignData (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo (TxInfo (..), TxInfoPending (..), TxType (..))
import Haskoin.Wallet.TxSigning
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
  TxSignData tx [funding] [Deriv :/ 0 :/ 0] [Deriv :/ 1 :/ 0] False
  where
    funding = fundingTx ctx 1 [(externalAddr 0, 100000000)]
    tx = spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 50000000), (internalAddr 0, 40000000)]

analysisSpec :: Ctx -> Spec
analysisSpec ctx =
  describe "Transaction analysis" $ do
    it "describes an unsigned transaction with an estimated size" $ do
      info <- expectRight $ parseTxSignData btc ctx accountPub unsigned
      info.txInfoHash `shouldBe` Nothing
      info.txInfoType `shouldBe` TxDebit
      info.txInfoAmount `shouldBe` (-60000000)
      info.txInfoFee `shouldBe` 10000000
      -- The estimate: 10 bytes, 148 for the input and 34 for each output
      info.txInfoSize `shouldBe` 226
      info.txInfoFeeByte `shouldBe` 44247
      info.txInfoPending `shouldBe` Just (TxInfoPending nosigHash False False)
      Map.keys info.txInfoMyInputs `shouldBe` [externalAddr 0]
      Map.keys info.txInfoMyOutputs `shouldBe` [internalAddr 0]
      Map.keys info.txInfoOtherOutputs `shouldBe` [otherAddr 0]
    it "describes the signed transaction with its real size" $ do
      (signed, _) <- expectRight $ signWalletTx btc ctx unsigned testAccountKey
      info <- expectRight $ parseTxSignData btc ctx accountPub signed
      info.txInfoHash `shouldBe` Just "e66b790c73d4e72fe13a07e247e4439bdea210cac2f947040e901f1e0ce59ac2"
      -- The signature of this transaction is 72 bytes long, which the estimate assumes
      info.txInfoSize `shouldBe` 226
      info.txInfoFeeByte `shouldBe` 44247
      info.txInfoPending `shouldBe` Just (TxInfoPending nosigHash True False)
  where
    unsigned = simpleTransaction ctx
    accountPub = testAccountPub ctx
    nosigHash = "9b41b3cb7b10bfd1dc3f7f1885ce3623789b1b946d206fd59fb432ad4fdb6d70"

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
            verifySignedTx btc ctx signed === Right ()
              .&&. info.txInfoFee === expected.expectedFee
              .&&. info.txInfoAmount === expected.expectedAmount
              .&&. info.txInfoType === expected.expectedType

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
-- another wallet or back to the test wallet. The outputs never exceed the
-- coins, so the fee is never negative.
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
        <*> amount 1 (coinsTotal `div` fromIntegral outputCount)
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
    ( TxSignData tx [funding] (pathsOf (fst <$> coins)) (pathsOf (fst <$> outputs)) False,
      Expected {expectedFee = fee, expectedAmount = net, expectedType = kind}
    )
  where
    amount :: Int -> Natural -> Gen Natural
    amount low high = fromIntegral <$> choose (low, fromIntegral high)
    walletAddresses = fst <$> walletPaths
    walletPaths =
      [(externalAddr i, Deriv :/ 0 :/ fromIntegral i) | i <- [0 .. 9 :: Int]]
        <> [(internalAddr i, Deriv :/ 1 :/ fromIntegral i) | i <- [0 .. 9 :: Int]]
