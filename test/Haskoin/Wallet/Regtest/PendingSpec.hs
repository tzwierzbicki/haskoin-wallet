{-# LANGUAGE OverloadedStrings #-}

-- | Pending transactions on live regtest chains: what sync makes of one
-- whose coin is gone, and what hw does when the store does not publish a
-- transaction. Every case runs on both chains. See
-- "Haskoin.Wallet.RegtestUtils" for the chains and the histories.
--
-- haskoin-store answers, from version 1.11, what came of a transaction it
-- was handed. The fake stores of the other tests took every one. So a
-- sync that failed for good on a transaction no node takes any more
-- passed them all. These tests ask the store itself.
module Haskoin.Wallet.Regtest.PendingSpec (spec) where

import Control.Monad (forM_, void)
import qualified Data.Text as Text
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.HwTestUtils (hwJson)
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Haskoin.Wallet.Workflow.TestUtils (answer)
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $
  forM_ [Btc, Bch] $ \chain -> do
    let -- A payment of 30000 satoshis to nobody at 10 satoshis per byte,
        -- prepared and no more
        preparedPayment home = prepared home chain [outsideAddress <> ":0.0003", "--fee-rate", "10"]
        signedPayment home = signedBy home testMnemonic =<< preparedPayment home
        sentPayment chains home = pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]
        -- The store, as the messages of hw name it
        store chains = "The store at " <> regtestHost chains

    describe ("A pending transaction on " <> networkOf chain <> " whose coin the other computer of the wallet spent") $ do
      it "is in conflict once a block holds the payment of the other computer" $
        onChains $ \chains ->
          withTwoComputers chains chain $ \home other -> do
            nosig <- preparedPayment other
            _ <- pay chains chain home testMnemonic [otherOutsideAddress <> ":0.0004", "--fee-rate", "20"]
            mine chains chain 1
            _ <- syncNow other
            pendingOf other `shouldReturn` [(nosig, False, False, True)]

      it "is counted by the sync that finds the conflict" $
        onChains $ \chains ->
          withTwoComputers chains chain $ \home other -> do
            _ <- preparedPayment other
            _ <- pay chains chain home testMnemonic [otherOutsideAddress <> ":0.0004", "--fee-rate", "20"]
            mine chains chain 1
            (conflictedOn chain =<< syncNow other) `shouldReturn` 1

    describe ("A pending transaction on " <> networkOf chain <> " whose coin went with its payment in a reorganisation") $ do
      it "is in conflict when it is not signed" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            let home = receivedHome received
            nosig <- preparedPayment home
            _ <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow home
            pendingOf home `shouldReturn` [(nosig, False, False, True)]

      it "is in conflict when it is signed and not sent" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            let home = receivedHome received
            txid <- signedPayment home
            _ <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow home
            pendingOf home `shouldReturn` [(txid, True, False, True)]

      it "is in conflict when it was sent and no sync saw it" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            let home = receivedHome received
            txid <- sentPayment chains home
            _ <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow home
            pendingOf home `shouldReturn` [(txid, True, True, True)]

      -- The store would refuse it: its coin is gone
      it "is not sent again by the sync, which then syncs the network" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            let home = receivedHome received
            _ <- sentPayment chains home
            _ <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            synced <- syncNow home
            notSyncedOn chain synced `shouldReturn` Nothing
            sentAgainOn chain synced `shouldReturn` 0

      it "leaves the payment that funded it dead in the history" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            let home = receivedHome received
            _ <- sentPayment chains home
            _ <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxCredit, 100000, Nothing)]

      it "is deleted with deletetx" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            let home = receivedHome received
            txid <- sentPayment chains home
            _ <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow home
            _ <- answer home ["deletetx", txid] []
            pendingOf home `shouldReturn` []

      it "is out of conflict when the chain that held its coin comes back longer" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            let home = receivedHome received
            nosig <- preparedPayment home
            lowest <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow home
            firstChainBack chains chain lowest
            _ <- syncNow home
            pendingOf home `shouldReturn` [(nosig, False, False, False)]

      it "is taken by the node once the chain that held its coin is back" $
        onChains $ \chains ->
          withReceived chains chain $ \received -> do
            let home = receivedHome received
            nosig <- preparedPayment home
            lowest <- replaceWithRival chains chain (receivedRivalHome received) (receivedRival received)
            _ <- syncNow home
            firstChainBack chains chain lowest
            _ <- syncNow home
            txid <- signedBy home testMnemonic nosig
            sent chains chain home txid
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

    describe ("A payment on " <> networkOf chain <> " that the network forgot before any sync saw it") $ do
      it "is sent again by the sync, and the node takes it" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- sentPayment chains home
            forgetMempool chains chain
            forgetStoreMempool chains chain
            synced <- syncNow home
            sentAgainOn chain synced `shouldReturn` 1
            waitSynced chains chain
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

      it "stays pending and sent after the sync that sent it again" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- sentPayment chains home
            forgetMempool chains chain
            forgetStoreMempool chains chain
            _ <- syncNow home
            pendingOf home `shouldReturn` [(txid, True, True, False)]

      -- 226 bytes × 10 = 2260 of fee: the payment takes 32260
      it "is in the history, unconfirmed, after the next sync" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- sentPayment chains home
            forgetMempool chains chain
            forgetStoreMempool chains chain
            _ <- syncNow home
            waitSynced chains chain
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 0), (TxCredit, 100000, Just 1)]

    describe ("A payment on " <> networkOf chain <> " that the network no longer takes for its fee") $ do
      -- The node now relays nothing under 20 satoshis per byte, and the
      -- payment pays 10
      let feesRise chains = do
            forgetMempool chains chain
            restartNode chains chain "-minrelaytxfee=0.0002"
            forgetStoreMempool chains chain

      it "does not stop the sync of its network" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- sentPayment chains home
            feesRise chains
            (notSyncedOn chain =<< syncNow home) `shouldReturn` Nothing

      it "is told by the sync as not sent again, with what the store said" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- sentPayment chains home
            feesRise chains
            (notPublishedOn chain =<< syncNow home) `shouldReturn` [(txid, store chains <> notTaken chain)]

      it "is not counted as sent again" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- sentPayment chains home
            feesRise chains
            (sentAgainOn chain =<< syncNow home) `shouldReturn` 0

      it "stays pending and sent, for the user to delete" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- sentPayment chains home
            feesRise chains
            _ <- syncNow home
            pendingOf home `shouldReturn` [(txid, True, True, False)]

      it "gives its coin back once deleted" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- sentPayment chains home
            feesRise chains
            _ <- syncNow home
            _ <- answer home ["deletetx", txid] []
            spendableOf home chain `shouldReturn` 100000

      it "is refused by broadcast with what the store said, when it was never sent" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- signedPayment home
            restartNode chains chain "-minrelaytxfee=0.0002"
            (errorOf =<< hwJson home ["broadcast", txid, "--yes"] []) `shouldReturn` Text.pack (store chains <> notTaken chain)

      it "stays signed and not sent after broadcast was refused" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- signedPayment home
            restartNode chains chain "-minrelaytxfee=0.0002"
            _ <- hwJson home ["broadcast", txid, "--yes"] []
            pendingOf home `shouldReturn` [(txid, True, False, False)]

    describe ("A payment on " <> networkOf chain <> " that a sync saw, and that its node then forgot") $ do
      let forgotten chains home = do
            txid <- sentPayment chains home
            void $ syncNow home
            forgetMempool chains chain
            return txid

      -- The store still lists it, and does not send again what it has
      it "stays unconfirmed in the history" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- forgotten chains home
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -32260, Just 0), (TxCredit, 100000, Just 1)]

      it "is refused by broadcast, which says that it was sent already" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- forgotten chains home
            (errorOf =<< hwJson home ["broadcast", txid, "--yes"] [])
              `shouldReturn` Text.pack ("The transaction " <> txid <> " is not pending: it was sent, and a sync has seen it on the network, so there is nothing to send. txs shows where it stands. A payment of Bitcoin that stays unconfirmed is replaced with bumpfee")

    describe ("A transaction sent on " <> networkOf chain <> " while the node of the store is down") $ do
      it "is refused by broadcast, which says that the store has no peer" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- signedPayment home
            stopService chains ("node-" <> (if chain == Btc then "btc" else "bch"))
            waitForPeers chains chain 0
            (errorOf =<< hwJson home ["broadcast", txid, "--yes"] [])
              `shouldReturn` Text.pack (store chains <> " says it is not healthy: it has no peer")
  where
    -- What a store answers of a payment at 10 satoshis per byte once its
    -- node relays nothing under 20. A node of Bitcoin says nothing of a
    -- transaction it refuses, so the store waits, then says that no node
    -- announced it. A node of Bitcoin Cash says why, and the store passes
    -- its words on.
    notTaken Btc = " handed the transaction to a node, and no node announced it in the time the store waits. A node says nothing of a transaction it refuses, so its fee may be too low for the network"
    notTaken Bch = " refused the transaction: A node refused the transaction: min relay fee not met"
