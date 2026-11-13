{-# LANGUAGE OverloadedStrings #-}

-- | Reorganisations of live regtest chains under the transactions of a
-- wallet: what sync makes of a payment received, and of a payment made,
-- when the block that held it is replaced. Every case runs on both chains.
-- See "Haskoin.Wallet.RegtestUtils" for the chains and the histories.
--
-- The amounts follow from three payments. The wallet is paid 100000
-- satoshis. It pays 30000 to an address of nobody at 10 satoshis per
-- byte: 226 bytes × 10 = 2260 of fee, so it gives up 32260 and keeps a
-- change of 100000 - 32260 = 67740. Its other computer holds a rival that
-- pays 40000 at 20 satoshis per byte: 226 × 20 = 4520 of fee, so it gives
-- up 44520 and keeps 100000 - 44520 = 55480.
module Haskoin.Wallet.Regtest.ReorgSpec (spec) where

import Control.Monad (forM_, void, when)
import qualified Data.Text as Text
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Haskoin.Wallet.Workflow.TestUtils (answer)
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $
  forM_ [Btc, Bch] $ \chain -> do
    let -- The payment of the wallet, sent, and in a block of its own
        paid chains home = do
          txid <- pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]
          mine chains chain 1
          void $ syncNow home
          return txid
        -- The rival of that payment, signed on the other computer
        rivalOf other = signedBy other testMnemonic =<< prepared other chain [otherOutsideAddress <> ":0.0004", "--fee-rate", "20"]

    describe ("A payment received on " <> networkOf chain <> ", after a reorganisation") $ do
      -- Block 102 held it. Blocks 102 and 103 of the new chain follow, and
      -- it is in the first: 103 - 102 + 1 = 2 confirmations.
      it "is confirmed in its new block when the block that replaced its own holds it" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- takeOff chains chain 1
            mine chains chain 2
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxCredit, 100000, Just 2)]

      it "can be spent again when the block that replaced its own holds it" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- takeOff chains chain 1
            mine chains chain 2
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(100000, 2, True)]

      it "is unconfirmed again when the blocks that replaced its own leave it out" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- takeOff chains chain 1
            mineLeavingMempool chains chain 2
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxCredit, 100000, Just 0)]

      -- A payment of somebody else that has no block can be replaced
      it "cannot be spent while the blocks that replaced its own leave it out" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- takeOff chains chain 1
            mineLeavingMempool chains chain 2
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(100000, 0, False)]

      it "is confirmed once it gets a block after the blocks that left it out" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- takeOff chains chain 1
            mineLeavingMempool chains chain 2
            _ <- syncNow home
            mine chains chain 1
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxCredit, 100000, Just 1)]

      it "is dead when its payer spent the coin elsewhere in the chain that replaced its block" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            _ <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow (receivedHome received)
            txsOn (receivedHome received) chain `shouldReturn` [(TxCredit, 100000, Nothing)]

      it "leaves no coin when its payer spent the coin elsewhere in the chain that replaced its block" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            _ <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow (receivedHome received)
            coinsOn (receivedHome received) chain `shouldReturn` []

      -- The first chain comes back with its block 102, and blocks 103 to
      -- 105 on top: 105 - 102 + 1 = 4 confirmations
      it "lives again when the chain that held it comes back longer" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            lowest <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow (receivedHome received)
            firstChainBack chains chain lowest
            _ <- syncNow (receivedHome received)
            txsOn (receivedHome received) chain `shouldReturn` [(TxCredit, 100000, Just 4)]

      it "can be spent again when the chain that held it comes back longer" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            lowest <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow (receivedHome received)
            firstChainBack chains chain lowest
            _ <- syncNow (receivedHome received)
            coinsOn (receivedHome received) chain `shouldReturn` [(100000, 4, True)]

    describe ("A payment made on " <> networkOf chain <> ", after a reorganisation") $ do
      -- Block 103 held it. It is in block 103 of the new chain, under
      -- block 104: 2 confirmations, and 3 for the payment received in 102.
      it "is confirmed in its new block when the block that replaced its own holds it" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- paid chains home
            _ <- takeOff chains chain 1
            mine chains chain 2
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 2), (TxCredit, 100000, Just 3)]

      it "is unconfirmed again when the blocks that replaced its own leave it out" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- paid chains home
            _ <- takeOff chains chain 1
            mineLeavingMempool chains chain 2
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 0), (TxCredit, 100000, Just 3)]

      -- The wallet made the payment, so nobody else can replace it
      it "leaves its change spendable when the blocks that replaced its own leave it out" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- paid chains home
            _ <- takeOff chains chain 1
            mineLeavingMempool chains chain 2
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(67740, 0, True)]

      it "is dead when the node and the store both forgot it after its block was replaced" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- paid chains home
            _ <- takeOff chains chain 1
            forgetMempool chains chain
            mine chains chain 2
            forgetStoreMempool chains chain
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Nothing), (TxCredit, 100000, Just 3)]

      it "gives its coin back when the node and the store both forgot it after its block was replaced" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- paid chains home
            _ <- takeOff chains chain 1
            forgetMempool chains chain
            mine chains chain 2
            forgetStoreMempool chains chain
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(100000, 3, True)]

      -- Nothing took its place: it was dropped
      it "names no replacement when the node and the store both forgot it" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- paid chains home
            _ <- takeOff chains chain 1
            forgetMempool chains chain
            mine chains chain 2
            forgetStoreMempool chains chain
            _ <- syncNow home
            deadOn home chain `shouldReturn` [(txid, [])]

      -- The rival is in block 105 of the new chain, the best one: 1
      -- confirmation. The payment received is in 102: 105 - 102 + 1 = 4.
      -- The dead payment keeps the place of its block, 103.
      it "is dead, under its rival, when the other computer of the wallet spent the coin in the chain that replaced its block" $
        onChains $ \chains ->
          withTwoComputers chains chain $ \home other -> do
            rival <- rivalOf other
            _ <- paid chains home
            _ <- replaceWithRival chains chain other rival
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -44520, Just 1), (TxDebit, -32260, Nothing), (TxCredit, 100000, Just 4)]

      it "names its rival as its replacement on the computer that made it" $
        onChains $ \chains ->
          withTwoComputers chains chain $ \home other -> do
            rival <- rivalOf other
            txid <- paid chains home
            _ <- replaceWithRival chains chain other rival
            _ <- syncNow home
            deadOn home chain `shouldReturn` [(txid, [rival])]

      it "leaves the change of its rival when the other computer of the wallet spent the coin" $
        onChains $ \chains ->
          withTwoComputers chains chain $ \home other -> do
            rival <- rivalOf other
            _ <- paid chains home
            _ <- replaceWithRival chains chain other rival
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(55480, 1, True)]

      -- The first chain is back: the payment is in its block 103, under
      -- blocks 104 to 106, so 4 confirmations, and 5 for the payment
      -- received. The rival is dead now, where its block 105 was.
      it "lives again, and its rival is dead, when the chain that held it comes back longer" $
        onChains $ \chains ->
          withTwoComputers chains chain $ \home other -> do
            rival <- rivalOf other
            _ <- paid chains home
            lowest <- replaceWithRival chains chain other rival
            _ <- syncNow home
            firstChainBack chains chain lowest
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -44520, Nothing), (TxDebit, -32260, Just 4), (TxCredit, 100000, Just 5)]

    -- sync --full reads the portfolio anew from the store. The store has
    -- forgotten the payment that the rival replaced, and the wallet has
    -- not: the payment stays in the history, dead.
    describe ("A full sync on " <> networkOf chain <> ", after a reorganisation") $ do
      it "gives the history a plain sync gives, when the block that replaced another holds the payment" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- paid chains home
            _ <- takeOff chains chain 1
            mine chains chain 2
            _ <- answer home ["sync", "--full"] []
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 2), (TxCredit, 100000, Just 3)]

      it "keeps a payment dead, under the rival that took its place" $
        onChains $ \chains ->
          withTwoComputers chains chain $ \home other -> do
            rival <- rivalOf other
            _ <- paid chains home
            _ <- replaceWithRival chains chain other rival
            _ <- syncNow home
            _ <- answer home ["sync", "--full"] []
            txsOn home chain `shouldReturn` [(TxDebit, -44520, Just 1), (TxDebit, -32260, Nothing), (TxCredit, 100000, Just 4)]

      -- The wallet learns of the reorganisation from the full sync itself
      it "finds a payment dead, under its rival, when no plain sync came first" $
        onChains $ \chains ->
          withTwoComputers chains chain $ \home other -> do
            rival <- rivalOf other
            _ <- paid chains home
            _ <- replaceWithRival chains chain other rival
            _ <- answer home ["sync", "--full"] []
            txsOn home chain `shouldReturn` [(TxDebit, -44520, Just 1), (TxDebit, -32260, Nothing), (TxCredit, 100000, Just 4)]

    describe ("A reorganisation of six blocks on " <> networkOf chain) $ do
      -- The payment received was in block 102 and the payment made in
      -- 107. Both are in block 102 of the new chain, which ends at 108:
      -- 108 - 102 + 1 = 7 confirmations each.
      it "leaves a payment received and the payment that spends it confirmed in one block" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            mine chains chain 4
            _ <- paid chains home
            _ <- takeOff chains chain 6
            mine chains chain 7
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 7), (TxCredit, 100000, Just 7)]

      it "leaves the change of the payment as the one coin of the wallet" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            mine chains chain 4
            _ <- paid chains home
            _ <- takeOff chains chain 6
            mine chains chain 7
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(67740, 7, True)]

    describe ("A chain of " <> networkOf chain <> " that starts again from its first block") $ do
      it "leaves every transaction of the wallet dead" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- paid chains home
            startAgain chains
            mine chains chain 3
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Nothing), (TxCredit, 100000, Nothing)]

      it "leaves the wallet without a coin" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- paid chains home
            startAgain chains
            mine chains chain 3
            _ <- syncNow home
            coinsOn home chain `shouldReturn` []

    -- The payment pays 2 satoshis per byte, 226 × 2 = 452 of fee, and
    -- its replacement 20, 226 × 20 = 4520. Block 103 held the replacement.
    -- In the chain that took its place, block 103 holds the first payment
    -- instead, under block 104: 2 confirmations, and 3 for the payment
    -- received. Only Bitcoin replaces a payment.
    when (chain == Btc) $ describe "A replacement made with bumpfee on btcreg, undone by a reorganisation that confirms the payment it replaced" $ do
      let undone chains home = do
            stuck <- pay chains Btc home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "2"]
            _ <- syncNow home
            first <- rawTxOf chains Btc (Text.pack stuck)
            replacement <- replacedWith chains Btc home testMnemonic stuck 20
            mine chains Btc 1
            _ <- syncNow home
            _ <- takeOff chains Btc 1
            forgetMempool chains Btc
            sendRawTx chains Btc first
            mine chains Btc 2
            _ <- syncNow home
            return (stuck, replacement)

      it "leaves the first payment confirmed, and its replacement dead" $
        onChains $ \chains ->
          withPaidWallet chains Btc $ \home -> do
            _ <- undone chains home
            listed <- txsOn home Btc
            listed `shouldMatchList` [(TxDebit, -30452, Just 2), (TxDebit, -34520, Nothing), (TxCredit, 100000, Just 3)]

      it "names the first payment as what took the place of the replacement" $
        onChains $ \chains ->
          withPaidWallet chains Btc $ \home -> do
            (stuck, replacement) <- undone chains home
            deadOn home Btc `shouldReturn` [(replacement, [stuck])]

      -- 100000 - 30000 - 452 = 69548
      it "leaves the change of the first payment as the one coin of the wallet" $
        onChains $ \chains ->
          withPaidWallet chains Btc $ \home -> do
            _ <- undone chains home
            coinsOn home Btc `shouldReturn` [(69548, 2, True)]
