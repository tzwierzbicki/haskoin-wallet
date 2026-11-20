{-# LANGUAGE OverloadedStrings #-}

-- | Coins that hold tokens (CashTokens) on a live chain of Bitcoin Cash.
-- Bitcoin Cash Node makes the coin, and a real haskoin-store lists it, so
-- these tests see the bytes that the network and the store give for a
-- token, where the other tests of tokens write those bytes themselves.
-- The node also says, after a payment and after a sweep, that the coin
-- still holds its tokens: the proof that hw did not spend it. See
-- "Haskoin.Wallet.RegtestUtils" for the chains, and
-- "Haskoin.Wallet.CashTokens" for why hw never spends such a coin.
--
-- The sizes are those of the network: 10 bytes for a transaction, 148 for
-- the legacy input of a compressed key, and 34 for a legacy output.
module Haskoin.Wallet.Regtest.TokenSpec (spec) where

import qualified Data.Text as Text
import Haskoin.Wallet.Database.Coin (CoinLock (..))
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $ do
  describe "A coin of 30000 satoshis that holds tokens, received on bchreg before a payment of 50000 satoshis" $ do
    it "is told by the sync as a new coin that holds tokens" $
      onChains $ \chains ->
        withTokenWallet chains $ \home _ ->
          (tokenCoinsOn Bch =<< syncNow home) `shouldReturn` [30000]

    it "is a coin of the portfolio that cannot be spent, beside the other one, which can" $
      onChains $ \chains ->
        withTokenWallet chains $ \home _ -> do
          _ <- syncNow home
          coinsOn home Bch `shouldReturn` [(50000, 1, True), (30000, 2, False)]

    it "is locked by its tokens" $
      onChains $ \chains ->
        withTokenWallet chains $ \home withTokens -> do
          _ <- syncNow home
          (lookup withTokens <$> locksOn home Bch) `shouldReturn` Just (Just LockedByTokens)

    it "is in the history with its payment, as the store counts it" $
      onChains $ \chains ->
        withTokenWallet chains $ \home _ -> do
          _ <- syncNow home
          txsOn home Bch `shouldReturn` [(TxCredit, 50000, Just 1), (TxCredit, 30000, Just 2)]

  describe "A sendall on bchreg from a portfolio with a coin that holds tokens" $ do
    let sendAllNobody chains home = payAllWithSendAll chains Bch home testMnemonic [outsideAddress, "--fee-rate", "10"]

    it "is taken by the node" $
      onChains $ \chains ->
        withTokenWallet chains $ \home _ -> do
          _ <- syncNow home
          txid <- sendAllNobody chains home
          mempoolOf chains Bch `shouldReturn` [Text.pack txid]

    -- 10 + 148 + 34 = 192 bytes × 10 = 1920: the fee of one input
    it "spends the other coin alone" $
      onChains $ \chains ->
        withTokenWallet chains $ \home _ -> do
          _ <- syncNow home
          txid <- sendAllNobody chains home
          (fst <$> nodeFeeOf chains Bch (Text.pack txid)) `shouldReturn` 1920

    it "leaves the coin with its 100 tokens, as the node says once the payment has a block" $
      onChains $ \chains ->
        withTokenWallet chains $ \home withTokens -> do
          _ <- syncNow home
          _ <- sendAllNobody chains home
          mine chains Bch 1
          tokensOf chains withTokens `shouldReturn` Just "100"

    it "leaves the coin in the portfolio, still not spendable" $
      onChains $ \chains ->
        withTokenWallet chains $ \home _ -> do
          _ <- syncNow home
          _ <- sendAllNobody chains home
          mine chains Bch 1
          _ <- syncNow home
          coinsOn home Bch `shouldReturn` [(30000, 3, False)]

  -- The address of the key holds a coin of 30000 satoshis with tokens,
  -- and a coin of 100000 satoshis without
  describe "A sweep on bchreg of a key with a coin that holds tokens" $ do
    let withKeyCoins chains test = do
          mine chains Bch 101
          withTokens <- payTokensFromNode chains (Text.pack outsideKeyLegacy) 30000
          _ <- payFromNode chains Bch (Text.pack outsideKeyLegacy) 100000
          mine chains Bch 1
          withWallet chains testMnemonic $ \home -> test home withTokens
        sweptToWallet chains home = do
          keys <- keyFileIn home outsideKey
          address <- receiveOn home Bch "bip44"
          sweep chains Bch home keys ["--key-file", keys, "--sweep-to", Text.unpack address, "--fee-rate", "10"]

    it "is taken by the node" $
      onChains $ \chains ->
        withKeyCoins chains $ \home _ -> do
          txid <- sweptToWallet chains home
          mempoolOf chains Bch `shouldReturn` [Text.pack txid]

    -- 192 bytes × 10 = 1920 of fee, and 100000 - 1920 = 98080
    it "brings the other coin to the wallet, less its fee" $
      onChains $ \chains ->
        withKeyCoins chains $ \home _ -> do
          _ <- sweptToWallet chains home
          _ <- syncNow home
          txsOn home Bch `shouldReturn` [(TxCredit, 98080, Just 0)]

    it "leaves the coin with its 100 tokens, as the node says once the sweep has a block" $
      onChains $ \chains ->
        withKeyCoins chains $ \home withTokens -> do
          _ <- sweptToWallet chains home
          mine chains Bch 1
          tokensOf chains withTokens `shouldReturn` Just "100"
