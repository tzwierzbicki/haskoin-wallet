{-# LANGUAGE OverloadedStrings #-}

-- | What the QR code of an address holds, and how a terminal draws it. How
-- an answer shows it is in "Haskoin.Wallet.Cli.PrettyPrinterSpec" and
-- "Haskoin.Wallet.Workflow.OutputSpec".
module Haskoin.Wallet.Cli.QrCodeSpec (spec) where

import qualified Data.Text as Text
import Haskoin (bch, bchTest, btc, btcTest)
import Haskoin.Wallet.Cli.QrCode
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = do
  describe "The payment URI of an address" $ do
    it "keeps the case of a Base58 address, which matters" $
      paymentUri btc "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        `shouldBe` "bitcoin:1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"

    it "is all uppercase for a bech32 address, as BIP 173 asks inside a QR code" $
      paymentUri btc "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc"
        `shouldBe` "BITCOIN:BC1QYNNYWXCVS9DWUQK8H53XZN9KPEYPRGQEACXWTC"

    it "keeps the scheme of Bitcoin for a Base58 address of a test network" $
      paymentUri btcTest "mipcBbFg9gMiCh81Kj8tqqdgoZub1ZJRfn"
        `shouldBe` "bitcoin:mipcBbFg9gMiCh81Kj8tqqdgoZub1ZJRfn"

    it "keeps the scheme of Bitcoin for a bech32 address of a test network" $
      paymentUri btcTest "tb1qghfhmd4zh7ncpmxl3qzhmq566jk8ckq4gafnmg"
        `shouldBe` "BITCOIN:TB1QGHFHMD4ZH7NCPMXL3QZHMQ566JK8CKQ4GAFNMG"

    it "is a Bitcoin Cash address itself, in uppercase: its prefix is its scheme" $
      paymentUri bch "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"
        `shouldBe` "BITCOINCASH:QZVZK6V4GDXT705VCMT3NXWWL7J3PFZERCKNLZ3K4U"

    it "is a Bitcoin Cash address of a test network itself, with its own prefix" $
      paymentUri bchTest "bchtest:qzp3djxuuwk0245quuem4e34ks6gt5csuc7cfwjprv"
        `shouldBe` "BCHTEST:QZP3DJXUUWK0245QUUEM4E34KS6GT5CSUC7CFWJPRV"

  describe "A QR code drawn for a terminal" $ do
    it "holds a text in bytes when its case matters" $
      qrRows "bitcoin:1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `shouldBe` Just p2pkhAddressCode

    it "holds an uppercase text in the alphanumeric mode" $
      qrRows "BITCOIN:BC1QYNNYWXCVS9DWUQK8H53XZN9KPEYPRGQEACXWTC" `shouldBe` Just p2wpkhAddressCode

    -- A QR code holds 2953 bytes at most
    it "is none for a text too long for a QR code" $
      qrRows (Text.replicate 3000 "a") `shouldBe` Nothing
