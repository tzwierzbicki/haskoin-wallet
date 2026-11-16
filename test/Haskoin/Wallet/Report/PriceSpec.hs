{-# LANGUAGE OverloadedStrings #-}

-- | The prices a report takes from the command line: how a price, a
-- price of a network and a currency are typed, how they are written, and
-- the value of a balance at a price.
module Haskoin.Wallet.Report.PriceSpec (spec) where

import Haskoin.Wallet.Report.Price
import Test.Hspec

-- | A reader of network names that knows btc alone, and reads it as 0
readTestNetwork :: String -> Either String Int
readTestNetwork name = if name == "btc" then Right 0 else Left ("No network " <> name)

spec :: Spec
spec = do
  describe "A price as typed" $ do
    it "is a whole number" $
      readPrice "87500" `shouldBe` Right (Price 87500 0)

    -- 87500.50 is the number 8750050 with two decimals
    it "keeps the decimals after its point, as many as typed" $
      readPrice "87500.50" `shouldBe` Right (Price 8750050 2)

    it "may be below one" $
      readPrice "0.0042" `shouldBe` Right (Price 42 4)

    it "may be zero, for a coin that is worth nothing" $
      readPrice "0" `shouldBe` Right (Price 0 0)

    it "is refused with a comma, which half of the world reads as a decimal point" $
      readPrice "87,500" `shouldBe` Left "The price 87,500 does not read: give a number with a point before its decimals, such as 87500 or 87500.50"

    it "is refused with a sign, a unit or nothing before its point" $
      [either (const "refused") show (readPrice typed) | typed <- ["-5", "5 CHF", ".5", "5.", "", "1e3"]]
        `shouldBe` ["refused", "refused", "refused", "refused", "refused", "refused"]

    it "is refused with more than eight decimals" $
      either (const "refused") show (readPrice "1.123456789") `shouldBe` "refused"

  describe "The price of a network as typed" $ do
    it "is the name of the network, an equals sign and the price" $
      readNetworkPrice readTestNetwork "btc=87500.5" `shouldBe` Right (0, Price 875005 1)

    it "is refused for a network the reader does not know, with what the reader says" $
      readNetworkPrice readTestNetwork "xyz=87500" `shouldBe` Left "No network xyz"

    it "is refused without a network" $
      readNetworkPrice readTestNetwork "87500" `shouldBe` Left "The price 87500 does not read: give a network and its price, such as btc=87500"

  describe "A currency as typed" $ do
    it "is its code of three letters, written in capitals" $
      (readCurrency "CHF", readCurrency "eur") `shouldBe` (Right "CHF", Right "EUR")

    it "is refused when it is not three letters" $
      readCurrency "Swiss francs" `shouldBe` Left "The currency Swiss francs does not read: give its code of three letters, such as CHF, EUR or USD"

  describe "A price as a report writes it" $ do
    it "has a comma between its thousands and two decimals at least" $
      priceText (Price 87500 0) `shouldBe` "87,500.00"

    it "keeps the decimals typed beyond two" $
      priceText (Price 42 4) `shouldBe` "0.0042"

    it "fills one decimal up to two" $
      priceText (Price 875005 1) `shouldBe` "87,500.50"

  describe "The value of a balance at a price" $ do
    -- One coin is 100000000 satoshis: at 87500.50 it is worth 87500.50,
    -- which is 8750050 hundredths
    it "is the price for one coin" $
      valueAt (Price 8750050 2) 100000000 `shouldBe` 8750050

    -- 50000 satoshis are 0.0005 coins: 0.0005 x 87500 = 43.75
    it "is the balance in coins times the price, in hundredths" $
      valueAt (Price 87500 0) 50000 `shouldBe` 4375

    -- 1 satoshi at 87500 is 0.000875, which rounds to 0.00. 572 satoshis
    -- are 0.5005, which rounds to 0.50, and 6 satoshis are 0.00525,
    -- which rounds to 0.01
    it "is rounded to the nearest hundredth" $
      [valueAt (Price 87500 0) satoshis | satoshis <- [1, 572, 6]] `shouldBe` [0, 50, 1]

    -- 4 satoshis at 125000 are exactly 0.005, half a hundredth
    it "rounds a half upwards" $
      valueAt (Price 125000 0) 4 `shouldBe` 1

    it "is exact for every coin there will ever be at a price of a million" $
      -- 21 million coins at 1000000.00 are 21 million millions
      valueAt (Price 1000000 0) 2100000000000000 `shouldBe` 2100000000000000

  describe "A value as a report writes it" $ do
    it "has a comma between its thousands and two decimals" $
      valueText 123456789 `shouldBe` "1,234,567.89"

    it "writes less than one with a zero before the point" $
      (valueText 5, valueText 0) `shouldBe` ("0.05", "0.00")
