{-# LANGUAGE OverloadedStrings #-}

-- | Which output of a payment is its change, and which outputs to the
-- portfolio are payments the user named
module Haskoin.Wallet.Tx.ChangeSpec (spec) where

import qualified Data.Map.Strict as Map
import Haskoin
import Haskoin.Wallet.Database.TxInfo (MyOutputs (..), TxSummary (..))
import Haskoin.Wallet.Keys (legacyPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.Change
import Test.Hspec

spec :: Spec
spec = do
  changeSpec
  amongSpec

-- The payment of spendSummary spends the coin at /0/0, pays 40000 to
-- another wallet and sends 9000 to /1/0. Each test gives it other outputs
-- to the portfolio.
paying :: [(Address, MyOutputs)] -> TxSummary
paying outputs = spendSummary {summaryMyOutputs = Map.fromList outputs}

changeSpec :: Spec
changeSpec =
  describe "The change of a payment" $ do
    it "is what it pays a change address of the portfolio" $
      changeOf spendSummary `shouldBe` Just (internalAddr 0, legacyPath (Deriv :/ 1 :/ 0))

    it "is what it sends back to a receiving address it spends from" $
      changeOf (paying [(externalAddr 0, MyOutputs 9000 (legacyPath (Deriv :/ 0 :/ 0)) "")])
        `shouldBe` Just (externalAddr 0, legacyPath (Deriv :/ 0 :/ 0))

    -- /0/1 holds no coin the payment spends: the user named it, as a
    -- recipient or with --change-back
    it "is not what it pays a receiving address it does not spend from" $
      changeOf (paying [(externalAddr 1, MyOutputs 9000 (legacyPath (Deriv :/ 0 :/ 1)) "")])
        `shouldBe` Nothing

    it "is at the change address, when the payment also pays a receiving address it spends from" $
      changeOf (paying [(externalAddr 0, MyOutputs 5000 (legacyPath (Deriv :/ 0 :/ 0)) ""), (internalAddr 0, MyOutputs 4000 (legacyPath (Deriv :/ 1 :/ 0)) "")])
        `shouldBe` Just (internalAddr 0, legacyPath (Deriv :/ 1 :/ 0))

    it "is one output: of two change addresses paid, the one of the lower path" $
      changeOf (paying [(internalAddr 1, MyOutputs 5000 (legacyPath (Deriv :/ 1 :/ 1)) ""), (internalAddr 0, MyOutputs 4000 (legacyPath (Deriv :/ 1 :/ 0)) "")])
        `shouldBe` Just (internalAddr 0, legacyPath (Deriv :/ 1 :/ 0))

    -- 9000 at /1/0 in two outputs, 5000 then 4000: the user asked for
    -- both with --repeat-address
    it "is none of the outputs to an address that several outputs pay" $
      changeOf spendSummary {summaryRepeatedOutputs = Map.fromList [(internalAddr 0, [5000, 4000])]}
        `shouldBe` Nothing

    it "is none in a payment the portfolio receives, even at a change address" $
      changeOf paymentSummary {summaryMyOutputs = Map.fromList [(internalAddr 0, MyOutputs 50000 (legacyPath (Deriv :/ 1 :/ 0)) "")]}
        `shouldBe` Nothing

    -- An existing choice: no output of a transaction between the
    -- addresses of the portfolio is told to be the change
    it "is none in a transaction between the addresses of the portfolio" $
      changeOf internalSummary `shouldBe` Nothing

-- bumpfee leaves out the change that another transaction it replaces
-- spends, and looks for the change among the other outputs
amongSpec :: Spec
amongSpec =
  describe "The change among some of the outputs of a payment" $ do
    it "is the output back to an address it spends from, when the output at a change address is not among them" $
      changeAmong
        (paying [(externalAddr 0, MyOutputs 5000 (legacyPath (Deriv :/ 0 :/ 0)) ""), (internalAddr 0, MyOutputs 4000 (legacyPath (Deriv :/ 1 :/ 0)) "")])
        [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0))]
        `shouldBe` Just (externalAddr 0, legacyPath (Deriv :/ 0 :/ 0))

    it "is none when none of them is given" $
      changeAmong spendSummary [] `shouldBe` Nothing
