{-# LANGUAGE OverloadedStrings #-}

-- | The recipients as the user types them, ADDRESS:AMOUNT for preparetx,
-- and also an address alone for preparesendall and the --sweep-to addresses
-- of preparesweep; and what each recipient gets of the coins
module Haskoin.Wallet.Tx.RecipientSpec (spec) where

import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Tx.Recipient
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = do
  recipientSpec
  paymentSpec
  addressSpec
  repeatSpec
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
      readRecipient bch UnitBitcoinCash "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"
        `shouldBe` Right (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu", Remainder)

    it "is a Bitcoin Cash address with its prefix, and its amount after the last colon" $
      readRecipient bch UnitBitcoinCash "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a:0.001"
        `shouldBe` Right (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu", Fixed 100000)

    it "is refused when it is not an address" $
      readRecipient btc UnitBitcoin "nonsense" `shouldBe` Left "The address nonsense is not valid on btc"

    it "is refused when what comes before its colon is not an address" $
      readRecipient btc UnitBitcoin "nonsense:0.001" `shouldBe` Left "The address nonsense is not valid on btc"

    -- The last letter is wrong. The refusal named the prefix alone, "The
    -- address bitcoincash is not valid on bch".
    it "is refused whole when it is a Bitcoin Cash address with its prefix and a typo" $
      readRecipient bch UnitBitcoinCash "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6b"
        `shouldBe` Left "The address bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6b is not valid on bch"

    -- The key of the Bitcoin wiki's "Wallet import format" page
    it "is refused when it is a private key, which the error does not print" $ do
      readRecipient btc UnitBitcoin "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617" `shouldBe` Left "This is a private key, not an address"
      readRecipient btc UnitBitcoin "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617:0.001" `shouldBe` Left "This is a private key, not an address"

    it "takes bch after its amount on Bitcoin Cash" $
      readRecipient bch UnitSatoshi "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a:0.001bch"
        `shouldBe` Right (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu", Fixed 100000)

    -- A legacy address reads on both chains, so the address does not
    -- tell that the user means the other coin: the unit does
    it "is refused an amount in bch at a legacy address on Bitcoin" $
      readRecipient btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.5bch"
        `shouldBe` Left "The amount 0.5bch is in bch, the unit of Bitcoin Cash, and the network is btc: give it in btc, bit or sat"

    it "is refused an amount in btc at a legacy address on Bitcoin Cash" $
      readRecipient bch UnitBitcoinCash "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.5btc"
        `shouldBe` Left "The amount 0.5btc is in btc, the unit of Bitcoin, and the network is bch: give it in bch, bit or sat"

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
      readPayment bch UnitBitcoinCash "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a:0.001"
        `shouldBe` Right (testAddress "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu", 100000)

    -- As the recipients were typed before, the amount in a word of its own
    it "is refused without an amount, with how to write one" $
      readPayment btc UnitBitcoin "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
        `shouldBe` Left "The recipient 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU has no amount: write it ADDRESS:AMOUNT, as in 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001"

    it "is refused a Bitcoin Cash address with its prefix but no amount" $
      readPayment bch UnitBitcoinCash "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"
        `shouldBe` Left "The recipient bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a has no amount: write it ADDRESS:AMOUNT, as in bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a:0.001"

-- preparesweep looks for the portfolio it pays before it knows the network
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
      shareAmounts 80000 3400 (const 546) [(otherAddr 0, Remainder)] `shouldBe` Right [(otherAddr 0, 76600)]

    -- 80000 - 30000 - 3740 = 46260
    it "is the amount typed for a fixed one, the fee paid by the address without one" $
      shareAmounts 80000 3740 (const 546) [(otherAddr 0, Fixed 30000), (otherAddr 1, Remainder)]
        `shouldBe` Right [(otherAddr 0, 30000), (otherAddr 1, 46260)]

    -- 80000 - 3740 = 76260, 38130 each
    it "is an equal part of what remains for the addresses without an amount" $
      shareAmounts 80000 3740 (const 546) [(otherAddr 0, Remainder), (otherAddr 1, Remainder)]
        `shouldBe` Right [(otherAddr 0, 38130), (otherAddr 1, 38130)]

    -- 80001 - 20000 - 4080 = 55921, 27960 each and the satoshi left to the
    -- first
    it "gives what does not divide to the first address without an amount" $
      shareAmounts 80001 4080 (const 546) [(otherAddr 0, Fixed 20000), (otherAddr 1, Remainder), (otherAddr 2, Remainder)]
        `shouldBe` Right [(otherAddr 0, 20000), (otherAddr 1, 27961), (otherAddr 2, 27960)]

    it "is nothing, cutting no fixed amount, when the coins do not cover the fixed amounts and the fee" $
      shareAmounts 80000 3740 (const 546) [(otherAddr 0, Fixed 80000), (otherAddr 1, Remainder)]
        `shouldBe` Left "The coins, 80000 satoshis, do not cover the fixed amounts, 80000, and the fee, 3740"

    it "is nothing when the coins do not cover the fee, without fixed amounts" $
      shareAmounts 80000 80001 (const 546) [(otherAddr 0, Remainder)]
        `shouldBe` Left "The coins, 80000 satoshis, do not cover the fee, 80001"

    -- 80000 - 76000 - 3740 = 260
    it "is nothing when what remains for an address without an amount is below the dust limit" $
      shareAmounts 80000 3740 (const 546) [(otherAddr 0, Fixed 76000), (otherAddr 1, Remainder)]
        `shouldBe` Left "What remains for each address without an amount, 260 satoshis, is below the dust limit of 546"

    it "is nothing for a fixed amount below the dust limit" $
      shareAmounts 80000 3740 (const 546) [(otherAddr 0, Fixed 545), (otherAddr 1, Remainder)]
        `shouldBe` Left "A fixed amount is below the dust limit of 546 satoshis"

    -- The limit depends on the kind of the output: 546 for the first
    -- address here, 294 for the second
    it "is held to the dust limit of each recipient's own output" $ do
      let limitOf addr = if addr == otherAddr 0 then 546 else 294
      shareAmounts 80000 3740 limitOf [(otherAddr 1, Fixed 294), (otherAddr 2, Remainder)]
        `shouldBe` Right [(otherAddr 1, 294), (otherAddr 2, 75966)] -- 80000 - 294 - 3740
      shareAmounts 80000 3740 limitOf [(otherAddr 0, Fixed 545), (otherAddr 2, Remainder)]
        `shouldBe` Left "A fixed amount is below the dust limit of 546 satoshis"
      -- 80000 - 75800 - 3740 = 460 remains: below the 546 of the first
      -- address, above the 294 of the second
      shareAmounts 80000 3740 limitOf [(otherAddr 1, Fixed 75800), (otherAddr 0, Remainder)]
        `shouldBe` Left "What remains for each address without an amount, 460 satoshis, is below the dust limit of 546"
      shareAmounts 80000 3740 limitOf [(otherAddr 0, Fixed 75800), (otherAddr 1, Remainder)]
        `shouldBe` Right [(otherAddr 0, 75800), (otherAddr 1, 460)]

    it "is nothing without an address to receive what remains" $ do
      checkSharing [(otherAddr 0, Fixed 30000), (otherAddr 1, Fixed 20000)]
        `shouldBe` Left "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"
      shareAmounts 80000 3740 (const 546) [(otherAddr 0, Fixed 30000)]
        `shouldBe` Left "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"

-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is externalAddr 0, the first
-- receiving address of the test portfolio

repeatSpec :: Spec
repeatSpec =
  describe "An address that a transaction would pay in several outputs" $ do
    it "is refused when the recipients name it twice, with the option that takes it" $
      checkPaidOnce btc False [(otherAddr 0, Fixed 30000), (otherAddr 1, Fixed 20000), (otherAddr 0, Remainder)]
        `shouldBe` Left "The address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is given more than once. To pay it in several outputs, each a coin of its own, add --repeat-address. Otherwise, give it once."

    it "is taken twice among the recipients with --repeat-address" $
      checkPaidOnce btc True [(otherAddr 0, Fixed 30000), (otherAddr 1, Fixed 20000), (otherAddr 0, Remainder)]
        `shouldBe` Right ()

    it "is not looked for among recipients that all differ" $
      checkPaidOnce btc False [(otherAddr 0, Fixed 30000), (otherAddr 1, Fixed 30000), (otherAddr 2, Remainder)]
        `shouldBe` Right ()

    -- The example of the CashAddr specification, typed with its prefix,
    -- then without it
    it "is one address of Bitcoin Cash, whether typed with its prefix or without it" $ do
      let typed = mapM (readRecipient bch UnitBitcoinCash) ["bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a", "qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"]
      (checkPaidOnce bch False =<< typed)
        `shouldBe` Left "The address bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a is given more than once. To pay it in several outputs, each a coin of its own, add --repeat-address. Otherwise, give it once."

    it "is refused when the change goes to it and the payment pays it too" $
      checkChangeApart btc False [(externalAddr 0, 20000 :: Natural)] (Just (externalAddr 0))
        `shouldBe` Left "The change goes to the address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, which the payment also pays. To pay it in several outputs, each a coin of its own, add --repeat-address."

    it "takes the change and a payment with --repeat-address" $
      checkChangeApart btc True [(externalAddr 0, 20000 :: Natural)] (Just (externalAddr 0))
        `shouldBe` Right ()

    it "is not refused for change that goes to another address than those the payment pays" $
      checkChangeApart btc False [(otherAddr 0, 20000 :: Natural)] (Just (internalAddr 0))
        `shouldBe` Right ()

    it "is not refused for a payment without change" $
      checkChangeApart btc False [(externalAddr 0, 20000 :: Natural)] Nothing
        `shouldBe` Right ()
