{-# LANGUAGE OverloadedStrings #-}

-- | Sweeps on live regtest chains: the coins of a private key that the
-- wallet does not hold, moved by preparesweep, signsweep and broadcast. A
-- sweep asks the store about addresses outside the wallet, and signs with
-- a key from a file, for each kind of address that key has. A node checks
-- every one of those signatures here. See "Haskoin.Wallet.RegtestUtils"
-- for the chains, the key and its addresses.
--
-- The sizes are those of the network: 10 bytes for a transaction, 148 for
-- the legacy input of a compressed key, 180 for the one of an
-- uncompressed key, whose public key is 32 bytes longer, and 34 for a
-- legacy output. A transaction with a witness takes 10.5 virtual bytes, a
-- native SegWit input 68, and a SegWit input nested in a script address
-- 91. The sum is rounded up.
module Haskoin.Wallet.Regtest.SweepSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as Text
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.Workflow.TestUtils (answer)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $ do
  forM_ [Btc, Bch] $ \chain -> do
    let net = networkOf chain
        -- The key file names the key. hw sweeps the coins at the legacy
        -- address of its compressed public key and at the one of its
        -- uncompressed public key, 100000 satoshis each, to the first
        -- address of the wallet, at 10 satoshis per byte.
        sweptToWallet chains home = do
          keys <- keyFileIn home outsideKey
          address <- receiveOn home chain "bip44"
          sweep chains chain home keys ["--key-file", keys, "--sweep-to", Text.unpack address, "--fee-rate", "10"]

    describe ("A sweep on " <> net <> " of the two legacy coins of a key, to the wallet") $ do
      it "is taken by the node, which checks the signature of each public key" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacy, outsideKeyLegacyUncompressed] $ \home -> do
            txid <- sweptToWallet chains home
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

      -- 10 + 148 + 180 + 34 = 372 bytes × 10 = 3720 of fee, and 200000 -
      -- 3720 = 196280 for the wallet
      it "enters the portfolio, less its fee" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacy, outsideKeyLegacyUncompressed] $ \home -> do
            _ <- sweptToWallet chains home
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxCredit, 196280, Just 0)]

      -- The fee counts the longer input of the uncompressed key, since
      -- the key file says which key each coin has. The node measures the
      -- signed transaction, a byte or two under the 372 hw counted.
      it "pays 10 satoshis for each byte the node measures, or a little more" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacy, outsideKeyLegacyUncompressed] $ \home -> do
            txid <- sweptToWallet chains home
            (fee, size) <- nodeFeeOf chains chain (Text.pack txid)
            fee `shouldBe` 3720
            size `shouldSatisfy` (<= 372)

      it "is one coin of the wallet once it has a block" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacy, outsideKeyLegacyUncompressed] $ \home -> do
            _ <- sweptToWallet chains home
            mine chains chain 1
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(196280, 1, True)]

      it "is no longer pending once a sync has seen it" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacy, outsideKeyLegacyUncompressed] $ \home -> do
            _ <- sweptToWallet chains home
            _ <- syncNow home
            pendingOf home `shouldReturn` []

    describe ("A sweep on " <> net <> " given the uncompressed text of the key") $ do
      -- The text of a key says which public key its owner used. hw looks
      -- at the addresses of both all the same, as for the compressed
      -- text: 200000 - 3720 = 196280.
      it "moves the coins of both legacy addresses of the key too" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacy, outsideKeyLegacyUncompressed] $ \home -> do
            keys <- keyFileIn home outsideKeyUncompressed
            address <- receiveOn home chain "bip44"
            _ <- sweep chains chain home keys ["--key-file", keys, "--sweep-to", Text.unpack address, "--fee-rate", "10"]
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxCredit, 196280, Just 0)]

    -- No portfolio of the wallet is paid, so the sweep goes from file to
    -- file, in a home without a wallet. The coin is named by its address,
    -- and the key is read only to sign.
    describe ("A sweep on " <> net <> " that pays no portfolio, from file to file") $ do
      let fileToFile chains home address = do
            keys <- keyFileIn home outsideKey
            _ <- answer home ["preparesweep", "--sweep-from", address, "--sweep-to", outsideAddress, "--network", net, "--fee-rate", "10", "--output", home </> "sweep.psbt"] []
            _ <- answer home ["signsweep", "--input", home </> "sweep.psbt", "--key-file", keys, "--output", home </> "signed.psbt"] []
            _ <- answer home ["broadcast", "--input", home </> "signed.psbt", "--yes"] []
            waitSynced chains chain

      -- 10 + 148 + 34 = 192 bytes × 10 = 1920 of fee
      it "is taken by the node, at the fee of its 192 bytes" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacy] $ \_ ->
            withHomeOn chains $ \home -> do
              fileToFile chains home outsideKeyLegacy
              held <- mempoolOf chains chain
              length held `shouldBe` 1
              (fst <$>) <$> mapM (nodeFeeOf chains chain) held `shouldReturn` [1920]

      it "leaves nothing pending in the home that made it" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacy] $ \_ ->
            withHomeOn chains $ \home -> do
              fileToFile chains home outsideKeyLegacy
              pendingOf home `shouldReturn` []

      -- Without the key, hw counts the input as the one of a compressed
      -- key: the fee is still 1920, over 32 bytes more. The README says
      -- so. The node takes it, at 8 satoshis per byte and a little more.
      it "of the coin of an uncompressed key is taken by the node, at a fee counted for a compressed key" $
        onChains $ \chains ->
          withCoinsOfKey chains chain [outsideKeyLegacyUncompressed] $ \_ ->
            withHomeOn chains $ \home -> do
              fileToFile chains home outsideKeyLegacyUncompressed
              held <- mempoolOf chains chain
              (fst <$>) <$> mapM (nodeFeeOf chains chain) held `shouldReturn` [1920]

  describe "A sweep on btcreg of the two SegWit coins of a key, to the wallet" $ do
    let sweptToWallet chains home = do
          keys <- keyFileIn home outsideKey
          address <- receiveOn home Btc "bip44"
          sweep chains Btc home keys ["--key-file", keys, "--sweep-to", Text.unpack address, "--fee-rate", "10"]

    it "is taken by the node, which checks the witness of each coin" $
      onChains $ \chains ->
        withCoinsOfKey chains Btc [outsideKeySegwit, outsideKeyNested] $ \home -> do
          txid <- sweptToWallet chains home
          mempoolOf chains Btc `shouldReturn` [Text.pack txid]

    -- 10.5 + 68 + 91 + 34 = 203.5, which rounds up to 204 virtual bytes ×
    -- 10 = 2040 of fee, and 200000 - 2040 = 197960 for the wallet
    it "enters the portfolio, less its fee" $
      onChains $ \chains ->
        withCoinsOfKey chains Btc [outsideKeySegwit, outsideKeyNested] $ \home -> do
          _ <- sweptToWallet chains home
          _ <- syncNow home
          txsOn home Btc `shouldReturn` [(TxCredit, 197960, Just 0)]

  describe "A sweep on btcreg of the coins of every kind of address of a key" $ do
    -- Four coins of 100000: 10.5 + 148 + 180 + 68 + 91 + 34 = 531.5, which
    -- rounds up to 532 virtual bytes × 10 = 5320 of fee, and 400000 - 5320
    -- = 394680
    it "moves the four coins in one transaction, which the node takes" $
      onChains $ \chains ->
        withCoinsOfKey chains Btc [outsideKeyLegacy, outsideKeyLegacyUncompressed, outsideKeySegwit, outsideKeyNested] $ \home -> do
          keys <- keyFileIn home outsideKey
          address <- receiveOn home Btc "bip44"
          txid <- sweep chains Btc home keys ["--key-file", keys, "--sweep-to", Text.unpack address, "--fee-rate", "10"]
          _ <- syncNow home
          mempoolOf chains Btc `shouldReturn` [Text.pack txid]
          txsOn home Btc `shouldReturn` [(TxCredit, 394680, Just 0)]
