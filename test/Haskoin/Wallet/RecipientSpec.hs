{-# LANGUAGE OverloadedStrings #-}

-- | The recipients as the user types them, ADDRESS:AMOUNT for preparetx,
-- and also an address alone for preparesendall and the --sweepto addresses
-- of preparesweep; and what each recipient gets of the coins
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
  addressSpec
  shareSpec

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

-- preparesweep looks for the account it pays before it knows the network
-- to read the amounts on
addressSpec :: Spec
addressSpec =
  describe "The address of a recipient as typed" $ do
    it "is the address alone" $
      recipientAddress btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" `shouldBe` Just (otherAddr 0)

    it "is the address before the amount" $
      recipientAddress btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0003" `shouldBe` Just (otherAddr 0)

    it "is the address before an amount that does not read" $
      recipientAddress btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:1,5" `shouldBe` Just (otherAddr 0)

    it "is a Bitcoin Cash address with its prefix, an amount after it or not" $ do
      recipientAddress bch "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a" `shouldBe` Just (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu")
      recipientAddress bch "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a:0.001" `shouldBe` Just (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu")

    it "is none when the text has no address" $ do
      recipientAddress btc "nonsense" `shouldBe` Nothing
      recipientAddress btc "nonsense:0.001" `shouldBe` Nothing

-- Coins of 80000 satoshis in all, with a dust limit of 546
shareSpec :: Spec
shareSpec =
  describe "What each recipient gets of the coins" $ do
    it "is everything but the fee for an address without an amount" $
      shareAmounts 80000 3400 546 [(otherAddr 0, Remainder)] `shouldBe` Right [(otherAddr 0, 76600)]

    -- 80000 - 30000 - 3740 = 46260
    it "is the amount typed for a fixed one, the fee paid by the address without one" $
      shareAmounts 80000 3740 546 [(otherAddr 0, Fixed 30000), (otherAddr 1, Remainder)]
        `shouldBe` Right [(otherAddr 0, 30000), (otherAddr 1, 46260)]

    -- 80000 - 3740 = 76260, 38130 each
    it "is an equal part of what remains for the addresses without an amount" $
      shareAmounts 80000 3740 546 [(otherAddr 0, Remainder), (otherAddr 1, Remainder)]
        `shouldBe` Right [(otherAddr 0, 38130), (otherAddr 1, 38130)]

    -- 80001 - 20000 - 4080 = 55921, 27960 each and the satoshi left to the
    -- first
    it "gives what does not divide to the first address without an amount" $
      shareAmounts 80001 4080 546 [(otherAddr 0, Fixed 20000), (otherAddr 1, Remainder), (otherAddr 2, Remainder)]
        `shouldBe` Right [(otherAddr 0, 20000), (otherAddr 1, 27961), (otherAddr 2, 27960)]

    it "is nothing, cutting no fixed amount, when the coins do not cover the fixed amounts and the fee" $
      shareAmounts 80000 3740 546 [(otherAddr 0, Fixed 80000), (otherAddr 1, Remainder)]
        `shouldBe` Left "The coins, 80000 satoshis, do not cover the fixed amounts, 80000, and the fee, 3740"

    it "is nothing when the coins do not cover the fee, without fixed amounts" $
      shareAmounts 80000 80001 546 [(otherAddr 0, Remainder)]
        `shouldBe` Left "The coins, 80000 satoshis, do not cover the fee, 80001"

    -- 80000 - 76000 - 3740 = 260
    it "is nothing when what remains for an address without an amount is below the dust limit" $
      shareAmounts 80000 3740 546 [(otherAddr 0, Fixed 76000), (otherAddr 1, Remainder)]
        `shouldBe` Left "What remains for each address without an amount, 260 satoshis, is below the dust limit of 546"

    it "is nothing for a fixed amount below the dust limit" $
      shareAmounts 80000 3740 546 [(otherAddr 0, Fixed 545), (otherAddr 1, Remainder)]
        `shouldBe` Left "A fixed amount is below the dust limit of 546 satoshis"

    it "is nothing without an address to receive what remains" $ do
      checkSharing [(otherAddr 0, Fixed 30000), (otherAddr 1, Fixed 20000)]
        `shouldBe` Left "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"
      shareAmounts 80000 3740 546 [(otherAddr 0, Fixed 30000)]
        `shouldBe` Left "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"
