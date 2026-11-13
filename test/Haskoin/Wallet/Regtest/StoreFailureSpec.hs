{-# LANGUAGE OverloadedStrings #-}

-- | What hw says when a part of the server side of a live regtest chain
-- is down, or not well. See "Haskoin.Wallet.RegtestUtils" for the chains.
module Haskoin.Wallet.Regtest.StoreFailureSpec (spec) where

import qualified Data.Text as Text
import Haskoin.Wallet.HwTestUtils (Run (..), hw)
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Haskoin.Wallet.Workflow.TestUtils (leftOutOf, recoveredOf)
import System.Exit (ExitCode (..))
import Test.Hspec

spec :: Spec
spec = aroundAll withRegtest $ do
  let -- The store, as the messages of hw name it
      store chains = Text.pack ("The store at " <> regtestHost chains)
      -- A wallet paid 100000 satoshis on each chain, in a block, and not
      -- synced since
      paidOnBoth chains test = do
        mine chains Btc 101
        mine chains Bch 101
        withWallet chains testMnemonic $ \home -> do
          core <- receiveOn home Btc "bip44"
          cash <- receiveOn home Bch "bip44"
          _ <- payFromNode chains Btc core 100000
          _ <- payFromNode chains Bch cash 100000
          mine chains Btc 1
          mine chains Bch 1
          test home

  describe "sync, while the store of btcreg is down behind its proxy" $ do
    it "names btcreg as not synced, with the status of the proxy" $
      onChains $ \chains ->
        paidOnBoth chains $ \home -> do
          stopService chains "store-btc"
          (notSyncedOn Btc =<< syncNow home) `shouldReturn` Just (store chains <> " answered with an error hw could not read: HTTP 502")

    it "still syncs bchreg" $
      onChains $ \chains ->
        paidOnBoth chains $ \home -> do
          stopService chains "store-btc"
          _ <- syncNow home
          spendableOf home Bch `shouldReturn` 100000

    it "exits with an error" $
      onChains $ \chains ->
        paidOnBoth chains $ \home -> do
          stopService chains "store-btc"
          run <- hw home ["sync"] []
          runExit run `shouldBe` ExitFailure 1

    it "syncs btcreg once its store is back" $
      onChains $ \chains ->
        paidOnBoth chains $ \home -> do
          stopService chains "store-btc"
          _ <- syncNow home
          startService chains "store-btc"
          waitSynced chains Btc
          _ <- syncNow home
          spendableOf home Btc `shouldReturn` 100000

  describe "sync, while the proxy is down" $ do
    -- Podman still holds the port it published. It takes the connection,
    -- finds nothing behind it in the container, and resets it.
    it "says that the connection to the store was lost" $
      onChains $ \chains ->
        paidOnBoth chains $ \home -> do
          stopService chains "proxy"
          (errorOf =<< syncNow home) `shouldReturn` store chains <> " could not be reached: the connection was lost (Connection reset by peer)"

  describe "sync, while the node of btcreg is down" $ do
    it "says that the store of btcreg has no peer" $
      onChains $ \chains ->
        paidOnBoth chains $ \home -> do
          stopService chains "node-btc"
          waitForPeers chains Btc 0
          (notSyncedOn Btc =<< syncNow home) `shouldReturn` Just (store chains <> " says it is not healthy: it has no peer")

    it "still syncs bchreg" $
      onChains $ \chains ->
        paidOnBoth chains $ \home -> do
          stopService chains "node-btc"
          waitForPeers chains Btc 0
          _ <- syncNow home
          spendableOf home Bch `shouldReturn` 100000

  describe "sync, on a quiet chain whose store has the usual limit on the age of its last transaction" $ do
    -- The blocks of the tests are dated in January 2026, so the last
    -- transaction is older than any limit a store would have: here one
    -- second. The age itself grows with the days, and is not checked.
    it "says how old the last transaction of the store is, and what the store allows" $
      onChains $ \chains ->
        paidOnBoth chains $ \home -> do
          startStoreWith chains Btc 4000000000 1
          failure <- notSyncedOn Btc =<< syncNow home
          failure `shouldSatisfy` maybe False (Text.isPrefixOf (store chains <> " says it is not healthy: its last transaction is "))
          failure `shouldSatisfy` maybe False (Text.isSuffixOf " old, and it allows 1 second")

  describe "recover, while the store of btcreg is down behind its proxy" $ do
    it "leaves btcreg out, with the status of the proxy" $
      onChains $ \chains ->
        paidOnBoth chains $ \_ -> do
          stopService chains "store-btc"
          withHomeOn chains $ \home ->
            (leftOutOf <$> recoverWith home testMnemonic [])
              `shouldReturn` Right [("btcreg", store chains <> " answered with an error hw could not read: HTTP 502")]

    it "finds the portfolio paid on bchreg" $
      onChains $ \chains ->
        paidOnBoth chains $ \_ -> do
          stopService chains "store-btc"
          withHomeOn chains $ \home ->
            (recoveredOf <$> recoverWith home testMnemonic []) `shouldReturn` Right (Just 0, [0])
