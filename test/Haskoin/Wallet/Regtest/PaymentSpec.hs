{-# LANGUAGE OverloadedStrings #-}

-- | Payments on live regtest chains: a node checks every signature hw
-- makes, on both chains and for both types of address, whether the
-- payment is made with preparetx, signtx and broadcast, or with send
-- alone. See "Haskoin.Wallet.RegtestUtils" for the chains and the
-- histories.
--
-- The sizes are those of the network: 10 bytes for a transaction, 148 for
-- a legacy input, 34 for a legacy output and 31 for a native SegWit one. A
-- payment from one legacy coin to a legacy address, with legacy change,
-- is 10 + 148 + 34 + 34 = 226 bytes. A transaction with a witness takes
-- 10.5 virtual bytes, and a native SegWit input 68. The sum is rounded
-- up.
module Haskoin.Wallet.Regtest.PaymentSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as Text
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.HwTestUtils (Run (..), hw, hwJson)
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Haskoin.Wallet.Workflow.TestUtils (answer, newTestPortfolio, switchTo)
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $ do
  forM_ [Btc, Bch] $ \chain -> do
    let net = networkOf chain
        -- 30000 satoshis to nobody at 10 satoshis per byte: 226 × 10 =
        -- 2260 of fee, so the wallet gives up 32260 of its 100000 and
        -- keeps 67740
        payNobody chains home = pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]

    describe ("A payment from a legacy coin on " <> net) $ do
      it "is taken by the node, which checks its signature" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- payNobody chains home
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

      it "takes its amount and its fee from the portfolio, unconfirmed, after a sync" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- payNobody chains home
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 0), (TxCredit, 100000, Just 1)]

      -- Nobody else can replace a payment the wallet made
      it "leaves change that can be spent before it has a block" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- payNobody chains home
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(67740, 0, True)]

      it "is no longer pending once a sync has seen it" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- payNobody chains home
            _ <- syncNow home
            pendingOf home `shouldReturn` []

      it "has one confirmation once it has a block" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- payNobody chains home
            mine chains chain 1
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 1), (TxCredit, 100000, Just 2)]

      -- 10 + 148 + 34 = 192 bytes × 10 = 1920 of fee, and the address
      -- gets the rest: the portfolio gives up all of its 100000
      it "of everything leaves no coin, and the node takes it" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- payAll chains chain home testMnemonic [outsideAddress, "--fee-rate", "10"]
            _ <- syncNow home
            mempoolOf chains chain `shouldReturn` [Text.pack txid]
            txsOn home chain `shouldReturn` [(TxDebit, -100000, Just 0), (TxCredit, 100000, Just 1)]
            coinsOn home chain `shouldReturn` []

    -- The same payment as above, made with send alone: 2260 of fee, so
    -- the wallet gives up 32260 of its 100000 and keeps 67740
    describe ("A payment made with send on " <> net) $ do
      let sendNobody chains home = payWithSend chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]

      it "is taken by the node, which checks its signature" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- sendNobody chains home
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

      it "takes its amount and its fee from the portfolio, unconfirmed, after a sync" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- sendNobody chains home
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 0), (TxCredit, 100000, Just 1)]

      it "leaves change that can be spent before it has a block" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- sendNobody chains home
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(67740, 0, True)]

      it "is no longer pending once a sync has seen it" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- sendNobody chains home
            _ <- syncNow home
            pendingOf home `shouldReturn` []

    describe ("A pending transaction on " <> net) $ do
      it "locks the coin it spends" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- prepared home chain [outsideAddress <> ":0.0003", "--fee-rate", "10"]
            coinsOn home chain `shouldReturn` [(100000, 1, False)]

      it "shows the time it was prepared, as pendingtxs shows it from then on" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            run <- hw home ["preparetx", "--network", net, "--yes", outsideAddress <> ":0.0003", "--fee-rate", "10"] []
            runOut run `shouldSatisfy` (not . Text.isInfixOf "time unknown")

      it "frees its coin when it is deleted" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            nosig <- prepared home chain [outsideAddress <> ":0.0003", "--fee-rate", "10"]
            _ <- answer home ["deletetx", nosig] []
            coinsOn home chain `shouldReturn` [(100000, 1, True)]

    describe ("A payment received on " <> net <> " that has no block yet") $ do
      -- The sender can still replace it, so the wallet holds it back. The
      -- refusal says what the portfolio holds and cannot spend.
      it "cannot pay for a payment, and the refusal says what the portfolio holds" $
        onChains $ \chains -> do
          mine chains chain 101
          withWallet chains testMnemonic $ \home -> do
            address <- receiveOn home chain "bip44"
            _ <- payFromNode chains chain address 100000
            _ <- syncNow home
            (errorOf =<< hwJson home ["preparetx", "--network", net, "--yes", outsideAddress <> ":0.0003", "--fee-rate", "10"] [])
              `shouldReturn` Text.pack ("The coins do not cover the amount and the fee. The portfolio holds 0.00100000 btc more on " <> net <> ", in 1 coin it cannot spend now: hw coins says why")

    describe ("The coins of a coinbase on " <> net) $ do
      -- The first block pays the wallet 50 coins. 98 more blocks make it
      -- 99 blocks old.
      it "cannot be spent with 99 confirmations" $
        onChains $ \chains ->
          withWallet chains testMnemonic $ \home -> do
            address <- receiveOn home chain "bip44"
            mineTo chains chain 1 address
            mine chains chain 98
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(5000000000, 99, False)]

      it "can be spent with 100 confirmations, and the node takes the payment" $
        onChains $ \chains ->
          withWallet chains testMnemonic $ \home -> do
            address <- receiveOn home chain "bip44"
            mineTo chains chain 1 address
            mine chains chain 99
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(5000000000, 100, True)]
            txid <- payNobody chains home
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

    describe ("A payment to another portfolio of the wallet on " <> net) $ do
      -- portfolio1 gets 30000, and portfolio0 gives up 30000 + 2260
      it "leaves the portfolio that pays and enters the other one" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            newTestPortfolio home
            switchTo home 0 1
            savings <- receiveOn home chain "bip44"
            switchTo home 0 0
            _ <- pay chains chain home testMnemonic [Text.unpack savings <> ":0.0003", "--fee-rate", "10"]
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 0), (TxCredit, 100000, Just 1)]
            switchTo home 0 1
            txsOn home chain `shouldReturn` [(TxCredit, 30000, Just 0)]

      -- The wallet made the payment, so nobody else can replace it
      it "can be spent in the other portfolio before it has a block" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            newTestPortfolio home
            switchTo home 0 1
            savings <- receiveOn home chain "bip44"
            switchTo home 0 0
            _ <- pay chains chain home testMnemonic [Text.unpack savings <> ":0.0003", "--fee-rate", "10"]
            _ <- syncNow home
            switchTo home 0 1
            coinsOn home chain `shouldReturn` [(30000, 0, True)]

  describe "A payment between the addresses of a portfolio on bchreg" $ do
    -- 226 bytes × 10 = 2260 of fee, which is all the portfolio gives up:
    -- 100000 - 2260 = 97740. The line names the receiving address that
    -- was paid, not the change, which is the larger of the two. The
    -- change is the one other address it pays.
    it "takes its fee alone from the portfolio, and names the receiving address it pays" $
      onChains $ \chains ->
        withPaidWallet chains Bch $ \home -> do
          own <- receiveOn home Bch "bip44"
          _ <- pay chains Bch home testMnemonic [Text.unpack own <> ":0.0003", "--fee-rate", "10"]
          _ <- syncNow home
          txsOn home Bch `shouldReturn` [(TxInternal, -2260, Just 0), (TxCredit, 100000, Just 1)]
          run <- hw home ["txs", "--network", "bchreg"] []
          runOut run `shouldSatisfy` Text.isInfixOf "  balance 0.00097740 btc, to bip44/0/1 and 1 more\n"

  describe "A payment on btcreg from native SegWit coins" $ do
    -- A SegWit input, a legacy output and SegWit change. A transaction
    -- with a witness has 10.5 virtual bytes of its own, and a native
    -- SegWit input 68: 10.5 + 68 + 34 + 31 = 143.5, which rounds up to 144
    -- virtual bytes × 10 = 1440 of fee. 100000 - 30000 - 1440 = 68560 of
    -- change.
    it "is taken by the node, which checks its witness" $
      onChains $ \chains -> do
        mine chains Btc 101
        withWallet chains testMnemonic $ \home -> do
          address <- receiveOn home Btc "bip84"
          _ <- payFromNode chains Btc address 100000
          mine chains Btc 1
          _ <- syncNow home
          txid <- pay chains Btc home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]
          _ <- syncNow home
          mempoolOf chains Btc `shouldReturn` [Text.pack txid]
          coinsOn home Btc `shouldReturn` [(68560, 0, True)]

    -- One coin of each type, 100000 each, pay 150000 together, with
    -- legacy change, the default type: 10.5 + 148 + 68 + 34 + 34 =
    -- 294.5, which rounds up to 295 virtual bytes × 10 = 2950 of fee.
    -- 200000 - 150000 - 2950 = 47050.
    it "and legacy coins together is taken by the node, signed for both" $
      onChains $ \chains -> do
        mine chains Btc 101
        withWallet chains testMnemonic $ \home -> do
          legacy <- receiveOn home Btc "bip44"
          segwit <- receiveOn home Btc "bip84"
          _ <- payFromNode chains Btc legacy 100000
          _ <- payFromNode chains Btc segwit 100000
          mine chains Btc 1
          _ <- syncNow home
          txid <- pay chains Btc home testMnemonic [outsideAddress <> ":0.0015", "--fee-rate", "10"]
          _ <- syncNow home
          mempoolOf chains Btc `shouldReturn` [Text.pack txid]
          coinsOn home Btc `shouldReturn` [(47050, 0, True)]

    -- The same two coins and the same payment, made with send alone:
    -- 2950 of fee, and 47050 of change
    it "and legacy coins together, made with send, is taken by the node, signed for both" $
      onChains $ \chains -> do
        mine chains Btc 101
        withWallet chains testMnemonic $ \home -> do
          legacy <- receiveOn home Btc "bip44"
          segwit <- receiveOn home Btc "bip84"
          _ <- payFromNode chains Btc legacy 100000
          _ <- payFromNode chains Btc segwit 100000
          mine chains Btc 1
          _ <- syncNow home
          txid <- payWithSend chains Btc home testMnemonic [outsideAddress <> ":0.0015", "--fee-rate", "10"]
          _ <- syncNow home
          mempoolOf chains Btc `shouldReturn` [Text.pack txid]
          coinsOn home Btc `shouldReturn` [(47050, 0, True)]

  describe "A stuck payment on btcreg, replaced with bumpfee" $ do
    -- The payment pays 2 satoshis per byte: 226 × 2 = 452 of fee. Its
    -- replacement pays 20: 226 × 20 = 4520.
    it "leaves the mempool of the node to its replacement" $
      onChains $ \chains ->
        withPaidWallet chains Btc $ \home -> do
          stuck <- pay chains Btc home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "2"]
          _ <- syncNow home
          replacement <- replacedWith chains Btc home testMnemonic stuck 20
          mempoolOf chains Btc `shouldReturn` [Text.pack replacement]

    it "is dead in the history, under its replacement, which it names" $
      onChains $ \chains ->
        withPaidWallet chains Btc $ \home -> do
          stuck <- pay chains Btc home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "2"]
          _ <- syncNow home
          replacement <- replacedWith chains Btc home testMnemonic stuck 20
          _ <- syncNow home
          txsOn home Btc `shouldReturn` [(TxDebit, -34520, Just 0), (TxDebit, -30452, Nothing), (TxCredit, 100000, Just 1)]
          deadOn home Btc `shouldReturn` [(stuck, [replacement])]

  describe "A payment on bchreg" $ do
    it "is not replaced by bumpfee, which says that Bitcoin Cash replaces none" $
      onChains $ \chains ->
        withPaidWallet chains Bch $ \home -> do
          stuck <- pay chains Bch home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "2"]
          _ <- syncNow home
          (errorOf =<< hwJson home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] [])
            `shouldReturn` "Bitcoin Cash replaces no transaction: its nodes keep the first one they see"
