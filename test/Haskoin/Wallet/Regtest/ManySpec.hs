{-# LANGUAGE OverloadedStrings #-}

-- | An address with more transactions than the store gives in one page,
-- on live regtest chains. A haskoin-store answers a question that sets no
-- limit with its first hundred items, and says nothing of the rest. A
-- fake store gives everything it has, so only a real one shows whether
-- hw asks for every transaction and every coin of an address. See
-- "Haskoin.Wallet.RegtestUtils" for the chains and the history.
--
-- The first legacy address of the wallet is paid 120 times, 10000
-- satoshis each time: 1200000 in all.
module Haskoin.Wallet.Regtest.ManySpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as Text
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $
  forM_ [Btc, Bch] $ \chain ->
    describe ("An address on " <> networkOf chain <> " paid 120 times, more than a page of the store") $ do
      it "has each of its 120 payments in the history" $
        onChains $ \chains ->
          withManyPayments chains chain 120 $ \home -> do
            listed <- txsOn home chain
            [(kind, amount) | (kind, amount, _) <- listed] `shouldBe` replicate 120 (TxCredit, 10000)

      it "holds a coin for each of its 120 payments, all of them spendable" $
        onChains $ \chains ->
          withManyPayments chains chain 120 $ \home -> do
            held <- coinsOn home chain
            [(value, spendable) | (value, _, spendable) <- held] `shouldBe` replicate 120 (10000, True)

      -- 120 inputs of 148 bytes and one output of 34: 10 + 17760 + 34 =
      -- 17804 bytes × 1 = 17804 of fee
      it "is emptied by one payment of its 120 coins, which the node takes at the fee of its 17804 bytes" $
        onChains $ \chains ->
          withManyPayments chains chain 120 $ \home -> do
            txid <- payAll chains chain home testMnemonic [outsideAddress, "--fee-rate", "1"]
            mempoolOf chains chain `shouldReturn` [Text.pack txid]
            (fst <$> nodeFeeOf chains chain (Text.pack txid)) `shouldReturn` 17804

      it "leaves the wallet without a coin once that payment is sent" $
        onChains $ \chains ->
          withManyPayments chains chain 120 $ \home -> do
            _ <- payAll chains chain home testMnemonic [outsideAddress, "--fee-rate", "1"]
            _ <- syncNow home
            coinsOn home chain `shouldReturn` []
