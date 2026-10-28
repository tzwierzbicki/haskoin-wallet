{-# LANGUAGE OverloadedStrings #-}

-- | The recipients as the user types them: ADDRESS:AMOUNT for preparetx, and
-- also an address alone for preparesendall
module Haskoin.Wallet.RecipientSpec (spec) where

import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Recipient
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = do
  recipientSpec
  paymentSpec

-- The address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is otherAddr 0. The Bitcoin
-- Cash address is the example of the CashAddr specification, whose legacy
-- form is 1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu.

recipientSpec :: Spec
recipientSpec =
  describe "A recipient as typed" $ do
    it "is an address alone, which shares what remains" $
      readRecipient btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" `shouldBe` Right (otherAddr 0, Remainder)

    it "is an address and the amount it gets after a colon, in the unit given" $
      readRecipient btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0003" `shouldBe` Right (otherAddr 0, Fixed 30000)

    it "takes the unit typed after the amount" $
      readRecipient btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:30000sat" `shouldBe` Right (otherAddr 0, Fixed 30000)

    it "is a Bitcoin Cash address with its prefix and its colon" $
      readRecipient bch UnitBitcoin "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"
        `shouldBe` Right (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu", Remainder)

    it "is a Bitcoin Cash address with its prefix, and its amount after the last colon" $
      readRecipient bch UnitBitcoin "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a:0.001"
        `shouldBe` Right (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu", Fixed 100000)

    it "is refused when it is not an address" $
      readRecipient btc UnitBitcoin "nonsense" `shouldBe` Left "The address nonsense is not valid on btc"

    it "is refused when what comes before its colon is not an address" $
      readRecipient btc UnitBitcoin "nonsense:0.001" `shouldBe` Left "The address nonsense is not valid on btc"

    it "is refused an amount that is not a number" $
      readRecipient btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:1,5"
        `shouldBe` Left "The amount 1,5 is not a number: write digits, with a point before the decimals"

paymentSpec :: Spec
paymentSpec =
  describe "A recipient of a payment as typed" $ do
    it "is an address and the amount it gets after a colon" $
      readPayment btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0003" `shouldBe` Right (otherAddr 0, 30000)

    it "takes the unit typed after the amount, or the one given" $ do
      readPayment btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:30000sat" `shouldBe` Right (otherAddr 0, 30000)
      readPayment btc UnitSatoshi "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:30000" `shouldBe` Right (otherAddr 0, 30000)

    it "is a Bitcoin Cash address with its prefix, and its amount" $
      readPayment bch UnitBitcoin "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a:0.001"
        `shouldBe` Right (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu", 100000)

    -- As the recipients were typed before, the amount in a word of its own
    it "is refused without an amount, with how to write one" $
      readPayment btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
        `shouldBe` Left "The recipient 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU has no amount: write it ADDRESS:AMOUNT, as in 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001"

    it "is refused a Bitcoin Cash address with its prefix but no amount" $
      readPayment bch UnitBitcoin "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"
        `shouldBe` Left "The recipient bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a has no amount: write it ADDRESS:AMOUNT, as in bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a:0.001"
