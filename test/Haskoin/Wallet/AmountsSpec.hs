{-# LANGUAGE OverloadedStrings #-}

-- | Amounts as the user types and reads them, in whole coins, bits and
-- satoshis: the amounts hw reads, those it refuses and why, how it shows
-- them, the names of the units, and the unit of each network: btc on
-- Bitcoin and bch on Bitcoin Cash.
module Haskoin.Wallet.AmountsSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (bch, bchTest4, btc, btcTest4)
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
  networkUnitSpec
  paymentUnitSpec

{- Amounts as typed -}

readSpec :: Spec
readSpec =
  describe "An amount as typed" $ do
    it "is in bitcoins with up to 8 decimals" $ do
      readAmount btc UnitBitcoin "1" `shouldBe` Right 100000000
      readAmount btc UnitBitcoin "100" `shouldBe` Right 10000000000
      readAmount btc UnitBitcoin "0.1" `shouldBe` Right 10000000
      readAmount btc UnitBitcoin "0.1000" `shouldBe` Right 10000000
      readAmount btc UnitBitcoin "0.10000000" `shouldBe` Right 10000000
      readAmount btc UnitBitcoin "0.00000001" `shouldBe` Right 1
      readAmount btc UnitBitcoin "1234.56789012" `shouldBe` Right 123456789012

    it "is in bits with up to 2 decimals" $ do
      readAmount btc UnitBit "1" `shouldBe` Right 100
      readAmount btc UnitBit "0.1" `shouldBe` Right 10
      readAmount btc UnitBit "0.10" `shouldBe` Right 10
      readAmount btc UnitBit "0.01" `shouldBe` Right 1
      readAmount btc UnitBit "100.01" `shouldBe` Right 10001
      readAmount btc UnitBit "1234567890.9" `shouldBe` Right 123456789090

    it "is in whole satoshis" $ do
      readAmount btc UnitSatoshi "8" `shouldBe` Right 8
      readAmount btc UnitSatoshi "100" `shouldBe` Right 100
      readAmount btc UnitSatoshi "1234567890" `shouldBe` Right 1234567890

    it "is zero however it is written" $ do
      readAmount btc UnitSatoshi "0" `shouldBe` Right 0
      readAmount btc UnitSatoshi "0000" `shouldBe` Right 0
      readAmount btc UnitBit "0.00" `shouldBe` Right 0
      readAmount btc UnitBitcoin "0.0" `shouldBe` Right 0
      readAmount btc UnitBitcoin "0.00000000" `shouldBe` Right 0

    it "may leave out the whole part, or the decimals after its point" $ do
      readAmount btc UnitBitcoin ".5" `shouldBe` Right 50000000
      readAmount btc UnitBitcoin "1." `shouldBe` Right 100000000

    it "is in the unit typed after it rather than the default one: btc, bit, bits, sat or sats" $ do
      readAmount btc UnitSatoshi "0.5btc" `shouldBe` Right 50000000
      readAmount btc UnitBitcoin "10bit" `shouldBe` Right 1000
      readAmount btc UnitBitcoin "10bits" `shouldBe` Right 1000
      readAmount btc UnitBitcoin "1000sat" `shouldBe` Right 1000
      readAmount btc UnitBitcoin "1000sats" `shouldBe` Right 1000

    it "takes its unit in any case" $ do
      readAmount btc UnitSatoshi "0.5BTC" `shouldBe` Right 50000000
      readAmount btc UnitBitcoin "1000Sats" `shouldBe` Right 1000

    it "may have spaces around it, and before its unit" $ do
      readAmount btc UnitSatoshi " 0.5 btc " `shouldBe` Right 50000000
      readAmount btc UnitSatoshi "  42 " `shouldBe` Right 42

    it "is in bch on a network of Bitcoin Cash, a test network too" $ do
      readAmount bch UnitSatoshi "0.5bch" `shouldBe` Right 50000000
      readAmount bchTest4 UnitSatoshi "0.5bch" `shouldBe` Right 50000000
      readAmount bch UnitBitcoinCash "0.5" `shouldBe` Right 50000000

    it "is in btc on a test network of Bitcoin" $
      readAmount btcTest4 UnitSatoshi "0.5btc" `shouldBe` Right 50000000

    it "is in bits or satoshis on Bitcoin Cash as on Bitcoin" $ do
      readAmount bch UnitBitcoinCash "10bits" `shouldBe` Right 1000
      readAmount bch UnitBitcoinCash "1000sat" `shouldBe` Right 1000

    it "may be all the 21 million bitcoins there will ever be, in any unit" $ do
      readAmount btc UnitBitcoin "21000000" `shouldBe` Right 2100000000000000
      readAmount btc UnitBitcoin "21000000000000bits" `shouldBe` Right 2100000000000000
      readAmount btc UnitBitcoin "2100000000000000sat" `shouldBe` Right 2100000000000000

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
      readAmount btc UnitBitcoin "0.000000001" `shouldBe` refused "0.000000001" "has more than 8 decimals, the most an amount in btc has"
      readAmount btc UnitBitcoin "0.100000000" `shouldBe` refused "0.100000000" "has more than 8 decimals, the most an amount in btc has"
      readAmount btc UnitBit "0.000" `shouldBe` refused "0.000" "has more than 2 decimals, the most an amount in bits has"

    it "with decimals in satoshis, even zeros" $ do
      readAmount btc UnitSatoshi "0.0" `shouldBe` refused "0.0" "has decimals, but an amount in sats is whole"
      readAmount btc UnitBitcoin "1.5sat" `shouldBe` refused "1.5sat" "has decimals, but an amount in sats is whole"

    -- A separator typed for the point would make another amount: 10'00
    -- bits were read as 1000 bits, when 10.00 bits are 10
    it "with separators" $ do
      readAmount btc UnitBit "10'00" `shouldBe` refused "10'00" notNumber
      readAmount btc UnitSatoshi "1_000" `shouldBe` refused "1_000" notNumber
      readAmount btc UnitSatoshi "1 000" `shouldBe` refused "1 000" notNumber
      readAmount btc UnitBitcoin "1,5" `shouldBe` refused "1,5" notNumber

    it "when it is not a number" $ do
      readAmount btc UnitBitcoin "" `shouldBe` refused "" notNumber
      readAmount btc UnitBitcoin "." `shouldBe` refused "." notNumber
      readAmount btc UnitBitcoin "btc" `shouldBe` refused "btc" notNumber
      readAmount btc UnitBitcoin "-1" `shouldBe` refused "-1" notNumber
      readAmount btc UnitBitcoin "+1" `shouldBe` refused "+1" notNumber
      readAmount btc UnitBitcoin "1e3" `shouldBe` refused "1e3" notNumber
      readAmount btc UnitBitcoin "1.2.3" `shouldBe` refused "1.2.3" notNumber

    -- A legacy address is valid on both chains: the unit is then the one
    -- word that says which coin the user means to pay
    it "in bch on a network of Bitcoin" $ do
      readAmount btc UnitBitcoin "0.5bch" `shouldBe` refused "0.5bch" "is in bch, the unit of Bitcoin Cash, and the network is btc: give it in btc, bit or sat"
      readAmount btcTest4 UnitSatoshi "0.5 BCH" `shouldBe` refused "0.5 BCH" "is in bch, the unit of Bitcoin Cash, and the network is btctest4: give it in btc, bit or sat"

    it "in btc on a network of Bitcoin Cash" $ do
      readAmount bch UnitBitcoinCash "0.5btc" `shouldBe` refused "0.5btc" "is in btc, the unit of Bitcoin, and the network is bch: give it in bch, bit or sat"
      readAmount bchTest4 UnitSatoshi "0.5btc" `shouldBe` refused "0.5btc" "is in btc, the unit of Bitcoin, and the network is bchtest4: give it in bch, bit or sat"

    it "on Bitcoin Cash as on Bitcoin, in the words of bch" $ do
      readAmount bch UnitBitcoinCash "0.000000001" `shouldBe` refused "0.000000001" "has more than 8 decimals, the most an amount in bch has"
      readAmount bch UnitBitcoinCash "5xyz" `shouldBe` refused "5xyz" "has a unit hw does not know: bch, bit, bits, sat or sats"
      readAmount bch UnitBitcoinCash "21000000.00000001" `shouldBe` refused "21000000.00000001" "is more than the 21 million bch there will ever be"

    it "with a unit hw does not know" $ do
      readAmount btc UnitBitcoin "5xyz" `shouldBe` refused "5xyz" "has a unit hw does not know: btc, bit, bits, sat or sats"
      readAmount btc UnitBitcoin "5 mbtc" `shouldBe` refused "5 mbtc" "has a unit hw does not know: btc, bit, bits, sat or sats"

    -- In the 64 bits of an output, a larger amount wrapped around: 2^64 +
    -- 10000 satoshis, 184467440737.09561616 bitcoins, paid 10000 satoshis
    it "above the 21 million bitcoins there will ever be, in any unit" $ do
      readAmount btc UnitBitcoin "21000000.00000001" `shouldBe` refused "21000000.00000001" "is more than the 21 million bitcoins there will ever be"
      readAmount btc UnitBitcoin "2100000000000001sat" `shouldBe` refused "2100000000000001sat" "is more than the 21 million bitcoins there will ever be"
      readAmount btc UnitBitcoin "184467440737.09561616" `shouldBe` refused "184467440737.09561616" "is more than the 21 million bitcoins there will ever be"

{- Amounts as shown -}

showSpec :: Spec
showSpec =
  describe "An amount as shown" $ do
    it "has the 8 decimals of a bitcoin" $ do
      showAmount UnitBitcoin 100000000 `shouldBe` "1.00000000"
      showAmount UnitBitcoin 112300000 `shouldBe` "1.12300000"
      showAmount UnitBitcoin 1 `shouldBe` "0.00000001"
      showAmount UnitBitcoin 0 `shouldBe` "0.00000000"

    it "has the 8 decimals of a whole coin of Bitcoin Cash" $ do
      showAmount UnitBitcoinCash 112300000 `shouldBe` "1.12300000"
      showAmount UnitBitcoinCash 1 `shouldBe` "0.00000001"

    -- As a report and the journal write an amount, beside their own name
    -- of the coin
    it "in whole coins without a unit has 8 decimals" $ do
      showCoins 50000 `shouldBe` "0.00050000"
      showCoins (-120000000) `shouldBe` "-1.20000000"

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
        readAmount btc unit (showAmount unit (toInteger satoshis)) `shouldBe` Right satoshis

{- Units -}

unitSpec :: Spec
unitSpec = do
  describe "A unit as typed" $ do
    it "is btc, bch, bit, bits, sat or sats" $ do
      readUnit "btc" `shouldBe` Just UnitBitcoin
      readUnit "bch" `shouldBe` Just UnitBitcoinCash
      readUnit "bit" `shouldBe` Just UnitBit
      readUnit "bits" `shouldBe` Just UnitBit
      readUnit "sat" `shouldBe` Just UnitSatoshi
      readUnit "sats" `shouldBe` Just UnitSatoshi

    it "may be in any case" $ do
      readUnit "BTC" `shouldBe` Just UnitBitcoin
      readUnit "Bch" `shouldBe` Just UnitBitcoinCash
      readUnit "Sats" `shouldBe` Just UnitSatoshi

    it "is refused otherwise" $ do
      readUnit "" `shouldBe` Nothing
      readUnit "bitcoin" `shouldBe` Nothing
      readUnit "satoshi" `shouldBe` Nothing
      readUnit "mbtc" `shouldBe` Nothing

  -- As the help and the configuration file write it
  describe "The name of a unit on its own" $
    it "is btc, bch, bit or sat" $ do
      unitName UnitBitcoin `shouldBe` "btc"
      unitName UnitBitcoinCash `shouldBe` "bch"
      unitName UnitBit `shouldBe` "bit"
      unitName UnitSatoshi `shouldBe` "sat"

  -- showUnit takes the amount in satoshis, as the amounts shown are
  describe "The name of a unit after an amount" $ do
    it "is btc for bitcoins, whatever the amount" $ do
      showUnit UnitBitcoin 100000000 `shouldBe` "btc"
      showUnit UnitBitcoin 250000000 `shouldBe` "btc"
      showUnit UnitBitcoin 1 `shouldBe` "btc"
      showUnit UnitBitcoin 0 `shouldBe` "btc"

    it "is bch for whole coins of Bitcoin Cash, whatever the amount" $ do
      showUnit UnitBitcoinCash 100000000 `shouldBe` "bch"
      showUnit UnitBitcoinCash 250000000 `shouldBe` "bch"
      showUnit UnitBitcoinCash 0 `shouldBe` "bch"

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
      forAll ((,) <$> elements [UnitBitcoin, UnitBitcoinCash, UnitBit, UnitSatoshi] <*> choose (-2100000000000000, 2100000000000000)) $ \(unit, satoshis) ->
        readUnit (showUnit unit satoshis) `shouldBe` Just unit

    prop "is read back with its amount as the amount it follows" $
      forAll ((,) <$> elements [UnitBitcoin, UnitBit, UnitSatoshi] <*> chooseBoundedIntegral (0, 2100000000000000)) $ \(unit, satoshis) ->
        readAmount btc UnitSatoshi (showAmount unit (toInteger satoshis) <> " " <> showUnit unit (toInteger satoshis)) `shouldBe` Right satoshis

{- The unit of a network -}

networkUnitSpec :: Spec
networkUnitSpec = do
  describe "The whole coin of a network" $ do
    it "is btc on Bitcoin and on its test networks" $ do
      coinUnit btc `shouldBe` UnitBitcoin
      coinUnit btcTest4 `shouldBe` UnitBitcoin

    it "is bch on Bitcoin Cash and on its test networks" $ do
      coinUnit bch `shouldBe` UnitBitcoinCash
      coinUnit bchTest4 `shouldBe` UnitBitcoinCash

  -- One unit is chosen for every network: btc and bch both ask for whole
  -- coins there
  describe "The unit the amounts of a network are shown in" $ do
    it "is bch on Bitcoin Cash when the configuration says btc" $
      unitOn bch (DefaultUnit UnitBitcoin) `shouldBe` UnitBitcoinCash

    it "is btc on Bitcoin when the configuration says bch" $
      unitOn btc (DefaultUnit UnitBitcoinCash) `shouldBe` UnitBitcoin

    it "is the coin of the network whichever of btc and bch --unit names" $ do
      unitOn bch (AskedUnit UnitBitcoin) `shouldBe` UnitBitcoinCash
      unitOn btc (AskedUnit UnitBitcoinCash) `shouldBe` UnitBitcoin
      unitOn btc (AskedUnit UnitBitcoin) `shouldBe` UnitBitcoin

    it "is the bit or the satoshi on both chains" $ do
      unitOn btc (DefaultUnit UnitBit) `shouldBe` UnitBit
      unitOn bch (DefaultUnit UnitBit) `shouldBe` UnitBit
      unitOn btc (AskedUnit UnitSatoshi) `shouldBe` UnitSatoshi
      unitOn bch (AskedUnit UnitSatoshi) `shouldBe` UnitSatoshi

{- The unit of a command that reads amounts -}

paymentUnitSpec :: Spec
paymentUnitSpec =
  describe "The unit of a command that reads amounts" $ do
    it "is refused when --unit names bch and the network is of Bitcoin" $ do
      paymentUnit btc (AskedUnit UnitBitcoinCash)
        `shouldBe` Left "The unit bch, given with --unit, is the unit of Bitcoin Cash, and the network is btc: give --unit btc, bit or sat"
      paymentUnit btcTest4 (AskedUnit UnitBitcoinCash)
        `shouldBe` Left "The unit bch, given with --unit, is the unit of Bitcoin Cash, and the network is btctest4: give --unit btc, bit or sat"

    it "is refused when --unit names btc and the network is of Bitcoin Cash" $
      paymentUnit bch (AskedUnit UnitBitcoin)
        `shouldBe` Left "The unit btc, given with --unit, is the unit of Bitcoin, and the network is bch: give --unit bch, bit or sat"

    it "is the coin of the network when --unit names it" $ do
      paymentUnit btc (AskedUnit UnitBitcoin) `shouldBe` Right UnitBitcoin
      paymentUnit bch (AskedUnit UnitBitcoinCash) `shouldBe` Right UnitBitcoinCash
      paymentUnit bchTest4 (AskedUnit UnitBitcoinCash) `shouldBe` Right UnitBitcoinCash

    it "is the bit or the satoshi of --unit on both chains" $ do
      paymentUnit bch (AskedUnit UnitBit) `shouldBe` Right UnitBit
      paymentUnit btc (AskedUnit UnitSatoshi) `shouldBe` Right UnitSatoshi

    -- The configuration is one for every network: a wallet with both
    -- chains must pay on each with the same file
    it "is the coin of the network when the configuration names the coin of the other chain" $ do
      paymentUnit bch (DefaultUnit UnitBitcoin) `shouldBe` Right UnitBitcoinCash
      paymentUnit btc (DefaultUnit UnitBitcoinCash) `shouldBe` Right UnitBitcoin
