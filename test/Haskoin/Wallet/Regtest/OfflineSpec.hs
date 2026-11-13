{-# LANGUAGE OverloadedStrings #-}

-- | Payments signed offline, on live regtest chains. The online computer
-- holds the public keys alone and talks to the store. The signer holds
-- the mnemonic and never sees the chain: all it knows of the coins comes
-- in the file preparetx wrote. So the file must carry everything a
-- signature needs, from the answers of a real store, and a node checks
-- here that it did. See "Haskoin.Wallet.RegtestUtils" for the chains and
-- the two computers.
--
-- The amounts are those of "Haskoin.Wallet.Regtest.PaymentSpec": 30000
-- satoshis to an address of nobody at 10 satoshis per byte, from a coin
-- of 100000.
module Haskoin.Wallet.Regtest.OfflineSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as Text
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.RegtestUtils
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $ do
  let payNobody chains chain signer online = payOffline chains chain signer online [outsideAddress <> ":0.0003", "--fee-rate", "10"]

  forM_ [Btc, Bch] $ \chain ->
    describe ("A payment on " <> networkOf chain <> " from a legacy coin, signed on a computer that never saw the chain") $ do
      it "is taken by the node, which checks its signature" $
        onChains $ \chains ->
          withSignerAndOnline chains chain "bip44" $ \signer online -> do
            txid <- payNobody chains chain signer online
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

      -- 226 bytes × 10 = 2260 of fee: 30000 + 2260 = 32260 leave the
      -- portfolio
      it "is in the history of the online computer after a sync" $
        onChains $ \chains ->
          withSignerAndOnline chains chain "bip44" $ \signer online -> do
            _ <- payNobody chains chain signer online
            _ <- syncNow online
            txsOn online chain `shouldReturn` [(TxDebit, -32260, Just 0), (TxCredit, 100000, Just 1)]

      -- The online computer sent it, so it is signed and sent there, and
      -- not in conflict
      it "is pending on the online computer as sent, until a sync sees it" $
        onChains $ \chains ->
          withSignerAndOnline chains chain "bip44" $ \signer online -> do
            txid <- payNobody chains chain signer online
            pendingOf online `shouldReturn` [(txid, True, True, False)]

  describe "A payment on btcreg from a native SegWit coin, signed on a computer that never saw the chain" $ do
    it "is taken by the node, which checks its witness" $
      onChains $ \chains ->
        withSignerAndOnline chains Btc "bip84" $ \signer online -> do
          txid <- payNobody chains Btc signer online
          mempoolOf chains Btc `shouldReturn` [Text.pack txid]

    -- A SegWit input, a legacy output and SegWit change: 10.5 + 68 + 34 +
    -- 31 = 143.5, which rounds up to 144 virtual bytes × 10 = 1440 of
    -- fee. 30000 + 1440 = 31440 leave the portfolio.
    it "is in the history of the online computer after a sync" $
      onChains $ \chains ->
        withSignerAndOnline chains Btc "bip84" $ \signer online -> do
          _ <- payNobody chains Btc signer online
          _ <- syncNow online
          txsOn online Btc `shouldReturn` [(TxDebit, -31440, Just 0), (TxCredit, 100000, Just 1)]
