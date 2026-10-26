{-# LANGUAGE OverloadedStrings #-}

-- | Amounts as the user types and reads them, in bitcoins, bits and
-- satoshis: the amounts hw reads, those it refuses and why, how it shows
-- them, and the names of the units.
module Haskoin.Wallet.AmountsSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Amounts
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (choose, chooseBoundedIntegral, elements, forAll)

spec :: Spec
spec = do
  readSpec
  refusedSpec
  showSpec
  unitSpec

{- Amounts as typed -}

readSpec :: Spec
readSpec =
  describe "An amount as typed" $ do
    it "is in bitcoins with up to 8 decimals" $ do
      readAmount UnitBitcoin "1" `shouldBe` Right 100000000
      readAmount UnitBitcoin "100" `shouldBe` Right 10000000000
      readAmount UnitBitcoin "0.1" `shouldBe` Right 10000000
      readAmount UnitBitcoin "0.1000" `shouldBe` Right 10000000
      readAmount UnitBitcoin "0.10000000" `shouldBe` Right 10000000
      readAmount UnitBitcoin "0.00000001" `shouldBe` Right 1
      readAmount UnitBitcoin "1234.56789012" `shouldBe` Right 123456789012

    it "is in bits with up to 2 decimals" $ do
      readAmount UnitBit "1" `shouldBe` Right 100
      readAmount UnitBit "0.1" `shouldBe` Right 10
      readAmount UnitBit "0.10" `shouldBe` Right 10
      readAmount UnitBit "0.01" `shouldBe` Right 1
      readAmount UnitBit "100.01" `shouldBe` Right 10001
      readAmount UnitBit "1234567890.9" `shouldBe` Right 123456789090

    it "is in whole satoshis" $ do
      readAmount UnitSatoshi "8" `shouldBe` Right 8
      readAmount UnitSatoshi "100" `shouldBe` Right 100
      readAmount UnitSatoshi "1234567890" `shouldBe` Right 1234567890

    it "is zero however it is written" $ do
      readAmount UnitSatoshi "0" `shouldBe` Right 0
      readAmount UnitSatoshi "0000" `shouldBe` Right 0
      readAmount UnitBit "0.00" `shouldBe` Right 0
      readAmount UnitBitcoin "0.0" `shouldBe` Right 0
      readAmount UnitBitcoin "0.00000000" `shouldBe` Right 0

    it "may leave out the whole part, or the decimals after its point" $ do
      readAmount UnitBitcoin ".5" `shouldBe` Right 50000000
      readAmount UnitBitcoin "1." `shouldBe` Right 100000000

    it "is in the unit typed after it rather than the default one: btc, bit, bits, sat or sats" $ do
      readAmount UnitSatoshi "0.5btc" `shouldBe` Right 50000000
      readAmount UnitBitcoin "10bit" `shouldBe` Right 1000
      readAmount UnitBitcoin "10bits" `shouldBe` Right 1000
      readAmount UnitBitcoin "1000sat" `shouldBe` Right 1000
      readAmount UnitBitcoin "1000sats" `shouldBe` Right 1000

    it "takes its unit in any case" $ do
      readAmount UnitSatoshi "0.5BTC" `shouldBe` Right 50000000
      readAmount UnitBitcoin "1000Sats" `shouldBe` Right 1000

    it "may have spaces around it, and before its unit" $ do
      readAmount UnitSatoshi " 0.5 btc " `shouldBe` Right 50000000
      readAmount UnitSatoshi "  42 " `shouldBe` Right 42

    it "may be all the 21 million bitcoins there will ever be, in any unit" $ do
      readAmount UnitBitcoin "21000000" `shouldBe` Right 2100000000000000
      readAmount UnitBitcoin "21000000000000bits" `shouldBe` Right 2100000000000000
      readAmount UnitBitcoin "2100000000000000sat" `shouldBe` Right 2100000000000000

{- Refused amounts -}

-- | The refusal of an amount as typed, for the given reason
refused :: Text -> String -> Either String a
refused typed reason = Left ("The amount " <> Text.unpack typed <> " " <> reason)

-- | The reason of an amount that is not a number
notNumber :: String
notNumber = "is not a number: write digits, with a point before the decimals"

refusedSpec :: Spec
refusedSpec =
  describe "An amount as typed is refused" $ do
    it "with more decimals than its unit has" $ do
      readAmount UnitBitcoin "0.000000001" `shouldBe` refused "0.000000001" "has more than 8 decimals, the most an amount in btc has"
      readAmount UnitBitcoin "0.100000000" `shouldBe` refused "0.100000000" "has more than 8 decimals, the most an amount in btc has"
      readAmount UnitBit "0.000" `shouldBe` refused "0.000" "has more than 2 decimals, the most an amount in bits has"

    it "with decimals in satoshis, even zeros" $ do
      readAmount UnitSatoshi "0.0" `shouldBe` refused "0.0" "has decimals, but an amount in sats is whole"
      readAmount UnitBitcoin "1.5sat" `shouldBe` refused "1.5sat" "has decimals, but an amount in sats is whole"

    -- A separator typed for the point would make another amount: 10'00
    -- bits were read as 1000 bits, when 10.00 bits are 10
    it "with separators" $ do
      readAmount UnitBit "10'00" `shouldBe` refused "10'00" notNumber
      readAmount UnitSatoshi "1_000" `shouldBe` refused "1_000" notNumber
      readAmount UnitSatoshi "1 000" `shouldBe` refused "1 000" notNumber
      readAmount UnitBitcoin "1,5" `shouldBe` refused "1,5" notNumber

    it "when it is not a number" $ do
      readAmount UnitBitcoin "" `shouldBe` refused "" notNumber
      readAmount UnitBitcoin "." `shouldBe` refused "." notNumber
      readAmount UnitBitcoin "btc" `shouldBe` refused "btc" notNumber
      readAmount UnitBitcoin "-1" `shouldBe` refused "-1" notNumber
      readAmount UnitBitcoin "+1" `shouldBe` refused "+1" notNumber
      readAmount UnitBitcoin "1e3" `shouldBe` refused "1e3" notNumber
      readAmount UnitBitcoin "1.2.3" `shouldBe` refused "1.2.3" notNumber

    it "with a unit hw does not know" $ do
      readAmount UnitBitcoin "5xyz" `shouldBe` refused "5xyz" "has a unit hw does not know: btc, bit, bits, sat or sats"
      readAmount UnitBitcoin "5 mbtc" `shouldBe` refused "5 mbtc" "has a unit hw does not know: btc, bit, bits, sat or sats"

    -- In the 64 bits of an output, a larger amount wrapped around: 2^64 +
    -- 10000 satoshis, 184467440737.09561616 bitcoins, paid 10000 satoshis
    it "above the 21 million bitcoins there will ever be, in any unit" $ do
      readAmount UnitBitcoin "21000000.00000001" `shouldBe` refused "21000000.00000001" "is more than the 21 million bitcoins there will ever be"
      readAmount UnitBitcoin "2100000000000001sat" `shouldBe` refused "2100000000000001sat" "is more than the 21 million bitcoins there will ever be"
      readAmount UnitBitcoin "184467440737.09561616" `shouldBe` refused "184467440737.09561616" "is more than the 21 million bitcoins there will ever be"

{- Amounts as shown -}

showSpec :: Spec
showSpec =
  describe "An amount as shown" $ do
    it "has the 8 decimals of a bitcoin" $ do
      showAmount UnitBitcoin 100000000 `shouldBe` "1.00000000"
      showAmount UnitBitcoin 112300000 `shouldBe` "1.12300000"
      showAmount UnitBitcoin 1 `shouldBe` "0.00000001"
      showAmount UnitBitcoin 0 `shouldBe` "0.00000000"

    it "has the 2 decimals of a bit" $ do
      showAmount UnitBit 100 `shouldBe` "1.00"
      showAmount UnitBit 12345 `shouldBe` "123.45"
      showAmount UnitBit 1 `shouldBe` "0.01"

    it "is whole in satoshis" $ do
      showAmount UnitSatoshi 0 `shouldBe` "0"
      showAmount UnitSatoshi 1234567890 `shouldBe` "1234567890"

    it "has no separators" $
      showAmount UnitBitcoin 2100000000000000 `shouldBe` "21000000.00000000"

    it "has a minus sign when it is negative" $ do
      showAmount UnitBitcoin (-100000000) `shouldBe` "-1.00000000"
      showAmount UnitBit (-1) `shouldBe` "-0.01"
      showAmount UnitSatoshi (-5) `shouldBe` "-5"

    prop "is read back as the amount it shows, in every unit" $
      forAll ((,) <$> elements [UnitBitcoin, UnitBit, UnitSatoshi] <*> chooseBoundedIntegral (0, 2100000000000000)) $ \(unit, satoshis) ->
        readAmount unit (showAmount unit (toInteger satoshis)) `shouldBe` Right satoshis

{- Units -}

unitSpec :: Spec
unitSpec = do
  describe "A unit as typed" $ do
    it "is btc, bit, bits, sat or sats" $ do
      readUnit "btc" `shouldBe` Just UnitBitcoin
      readUnit "bit" `shouldBe` Just UnitBit
      readUnit "bits" `shouldBe` Just UnitBit
      readUnit "sat" `shouldBe` Just UnitSatoshi
      readUnit "sats" `shouldBe` Just UnitSatoshi

    it "may be in any case" $ do
      readUnit "BTC" `shouldBe` Just UnitBitcoin
      readUnit "Sats" `shouldBe` Just UnitSatoshi

    it "is refused otherwise" $ do
      readUnit "" `shouldBe` Nothing
      readUnit "bitcoin" `shouldBe` Nothing
      readUnit "satoshi" `shouldBe` Nothing
      readUnit "mbtc" `shouldBe` Nothing

  -- showUnit takes the amount in satoshis, as the amounts shown are
  describe "The name of a unit after an amount" $ do
    it "is btc for bitcoins, whatever the amount" $ do
      showUnit UnitBitcoin 100000000 `shouldBe` "btc"
      showUnit UnitBitcoin 250000000 `shouldBe` "btc"
      showUnit UnitBitcoin 1 `shouldBe` "btc"
      showUnit UnitBitcoin 0 `shouldBe` "btc"

    it "is bit or sat for exactly one unit, of either sign" $ do
      showUnit UnitBit 100 `shouldBe` "bit"
      showUnit UnitBit (-100) `shouldBe` "bit"
      showUnit UnitSatoshi 1 `shouldBe` "sat"
      showUnit UnitSatoshi (-1) `shouldBe` "sat"

    -- 1 satoshi in bits is 0.01 bits: hw counted the satoshis rather than
    -- the units, and said bit
    it "is bits or sats for any other amount, a fraction of one unit included" $ do
      showUnit UnitBit 1 `shouldBe` "bits"
      showUnit UnitBit 0 `shouldBe` "bits"
      showUnit UnitBit 250 `shouldBe` "bits"
      showUnit UnitSatoshi 0 `shouldBe` "sats"
      showUnit UnitSatoshi 2 `shouldBe` "sats"

    -- What hw shows after an amount, the user can type after one
    prop "is read back as its unit" $
      forAll ((,) <$> elements [UnitBitcoin, UnitBit, UnitSatoshi] <*> choose (-2100000000000000, 2100000000000000)) $ \(unit, satoshis) ->
        readUnit (showUnit unit satoshis) `shouldBe` Just unit

    prop "is read back with its amount as the amount it follows" $
      forAll ((,) <$> elements [UnitBitcoin, UnitBit, UnitSatoshi] <*> chooseBoundedIntegral (0, 2100000000000000)) $ \(unit, satoshis) ->
        readAmount UnitSatoshi (showAmount unit (toInteger satoshis) <> " " <> showUnit unit (toInteger satoshis)) `shouldBe` Right satoshis
