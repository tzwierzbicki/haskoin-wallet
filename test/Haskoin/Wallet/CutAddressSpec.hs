{-# LANGUAGE OverloadedStrings #-}

-- | Cut addresses: what show gives at a path the wallet does not hold. Each
-- form of address keeps as many characters at each end as carry 128 bits,
-- and a dot stands for each character hidden.
module Haskoin.Wallet.CutAddressSpec (spec) where

import Haskoin (bch, bchTest, btc)
import Haskoin.Wallet.CutAddress (cutAddress)
import Haskoin.Wallet.TestUtils (bchExternalAddr, bchTestAddr, externalAddrs, segwitExternalAddr, testAddress, wrappedExternalAddrsText)
import Test.Hspec

spec :: Spec
spec =
  describe "A cut address" $ do
    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc: 4 + 13 shown, 12
    -- hidden, 13 shown = 42
    it "of bech32 shows bc1q and 13 characters, and the last 13, a dot for each of the 12 hidden" $
      cutAddress btc (segwitExternalAddr 0) `shouldBe` Just "bc1qynnywxcvs9dwu............eyprgqeacxwtc"

    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj: 12 shown, 10 hidden, 12
    -- shown = 34
    it "of Base58 shows the first 12 characters and the last 12, a dot for each of the 10 hidden" $
      cutAddress btc (externalAddrs !! 0) `shouldBe` Just "1KEn7jEXa7KC..........qRJBLnPMmrLj"

    -- 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1: 12 shown, 9 hidden, 12 shown =
    -- 33
    it "of Base58 one character shorter shows as many at each end, and hides one less" $
      cutAddress btc (externalAddrs !! 3) `shouldBe` Just "1yQZuJjA6w7h.........Cv22rKCas7F1"

    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u: 13 + 13
    -- shown, 14 hidden, 14 shown = 54
    it "of a cash address shows bitcoincash:q and 13 characters, and the last 14, a dot for each of the 14 hidden" $
      cutAddress bch bchExternalAddr `shouldBe` Just "bitcoincash:qzvzk6v4gdxt70..............pfzercknlz3k4u"

    -- bchtest:qztv0mmm3yscxcvfr93hq3wn63xk50merq7wl5gp3m: 9 + 13 shown,
    -- 14 hidden, 14 shown = 50
    it "of a cash address with a shorter prefix shows the whole prefix, then as many characters" $
      cutAddress bchTest bchTestAddr `shouldBe` Just "bchtest:qztv0mmm3yscxc..............50merq7wl5gp3m"

    -- A P2SH address, which a BIP49 account would derive
    it "is not made of an address of a kind that no account derives" $
      cutAddress btc (testAddress (wrappedExternalAddrsText !! 0)) `shouldBe` Nothing
