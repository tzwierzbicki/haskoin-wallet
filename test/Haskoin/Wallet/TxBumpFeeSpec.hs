{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The rules of a replacement that do not need the database: whether a
-- transaction can be replaced, the lowest fee rate of its replacement, and
-- the fee the network asks of the replacement. What bumpfee builds is
-- covered with the hw binary, in "Haskoin.Wallet.WorkflowSpec".
module Haskoin.Wallet.TxBumpFeeSpec (spec) where

import qualified Data.Map.Strict as Map
import Haskoin
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxBumpFee
import Haskoin.Wallet.TxInfo (MyInputs (..), TxSummary (..), TxType (..))
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  replaceableSpec ctx
  feeSpec ctx

-- | A payment of the account of 2260 of fee for its 226 bytes, which
-- spends the coin of 50000 at /0/0
paymentSummary :: TxSummary
paymentSummary =
  TxSummary
    { summaryType = TxDebit,
      summaryAmount = -22260,
      summaryFee = 2260,
      summaryFeeByte = 10,
      summaryChangeInFee = Just 0,
      summarySize = 226,
      summaryMyOutputs = Map.empty,
      summaryOtherOutputs = Map.empty,
      summaryNonStdOutputs = [],
      summaryMyInputs = Map.fromList [(externalAddr 0, MyInputs 50000 (Deriv :/ 0 :/ 0) "" [OutPoint (fakeTxHash 1) 0])],
      summaryOtherInputs = Map.empty,
      summaryNonStdInputs = []
    }

-- | The payment, its inputs marked replaceable or final
payment :: Ctx -> Bool -> Sent
payment ctx marked =
  Sent
    { sentTx = (if marked then replaceableTx else spendingTx) ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 20000), (internalAddr 0, 27740)],
      sentSummary = paymentSummary,
      sentParents = []
    }

replaceableSpec :: Ctx -> Spec
replaceableSpec ctx =
  describe "A transaction bumpfee replaces" $ do
    it "is a payment of the account marked replaceable" $
      checkReplaceable (payment ctx True) `shouldBe` Right ()

    it "is not one whose inputs are final, which the store would keep showing" $
      checkReplaceable (payment ctx False)
        `shouldBe` Left "The transaction is not marked replaceable (BIP 125): the store would keep showing it in place of its replacement until a block confirms one of them"

    it "is not one that spends a coin of another wallet" $ do
      let received = (payment ctx True) {sentSummary = paymentSummary {summaryOtherInputs = Map.fromList [(otherAddr 1, 30000)]}}
      checkReplaceable received
        `shouldBe` Left "Only a payment from the coins of the account can be replaced: this transaction spends coins of another wallet"

feeSpec :: Ctx -> Spec
feeSpec ctx =
  describe "The fee of a replacement" $ do
    -- 2260 for 226 bytes is 10 a byte: the replacement pays more
    it "is at a rate above that of the transaction it replaces" $
      minimumRate (payment ctx True) `shouldBe` 11

    -- 2261 for 226 bytes is a little above 10 a byte
    it "is at a rate above that of a transaction that pays a fraction more" $
      minimumRate (payment ctx True) {sentSummary = paymentSummary {summaryFee = 2261}} `shouldBe` 11

    -- 2260 replaced, and 226 bytes at 1 satoshi a byte: 2486
    it "pays the fees of the transactions it replaces, and its own size at 1 satoshi a byte" $ do
      checkBumpFee (plan 2260) (paid 2486 226) `shouldBe` Right ()
      checkBumpFee (plan 2260) (paid 2485 226)
        `shouldBe` Left "The replacement pays 2485 satoshis of fee, and must pay 2486: the fees of the transactions it replaces, and its own 226 bytes. Give a --feerate of at least 11"
  where
    plan :: Natural -> BumpPlan
    plan fees = BumpPlan [] [] Map.empty [] Map.empty Nothing fees []
    paid fee size = paymentSummary {summaryFee = fee, summarySize = size}
