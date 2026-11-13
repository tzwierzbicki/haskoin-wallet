{-# LANGUAGE OverloadedStrings #-}

-- | What the nodes relay, on live regtest chains. hw builds no
-- transaction that the network would not spread: it keeps every output at
-- or above the dust limit of its kind, pays a fee by the size it
-- estimates, and sets a lock time. Those rules are copies of the policy
-- of the nodes. Here a Bitcoin Core and a Bitcoin Cash Node say whether
-- the copies are right, at the very limits. See
-- "Haskoin.Wallet.RegtestUtils" for the chains and the histories.
--
-- The wallet holds one legacy coin of 100000 satoshis. A payment of it to
-- a legacy address, with legacy change, is 10 + 148 + 34 + 34 = 226
-- bytes. hw counts each signature at its longest, so a signed transaction
-- is a byte or two under what hw counted, and never over.
module Haskoin.Wallet.Regtest.RelaySpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text as Text
import Haskoin.Wallet.Database.TxInfo (TxType (..))
import Haskoin.Wallet.RegtestUtils
import Haskoin.Wallet.TestUtils (testMnemonic)
import Test.Hspec

-- | The kinds of address a payment can pay on each chain, each with the
-- least a node relays to it. The limit is three times what spending the
-- output costs at the least fee rate, so it follows the size of the
-- output and of the input that spends it.
dustLimits :: Chain -> [(String, String, Integer)]
dustLimits Btc =
  [ ("a legacy address", outsideAddress, 546),
    ("a script address", outsideScriptAddress, 540),
    ("a native SegWit address", outsideKeySegwit, 294),
    ("a native SegWit script address", outsideSegwitScriptAddress, 330),
    ("a Taproot address", outsideTaprootAddress, 330)
  ]
dustLimits Bch =
  [ ("a legacy address", outsideAddress, 546),
    ("a script address", outsideCashScriptAddress, 540)
  ]

spec :: Spec
spec = aroundAll withRegtest $ do
  forM_ [Btc, Bch] $ \chain -> do
    let net = networkOf chain

    describe ("A payment on " <> net <> " at 1 satoshi per byte, the least the nodes relay") $ do
      it "is taken by the node" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "1"]
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

      -- 226 bytes × 1 = 226 of fee. A node that measured more bytes than
      -- hw counted would find the rate too low, and refuse.
      it "pays the 226 satoshis hw states, over no more bytes than hw counted" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "1"]
            (fee, size) <- nodeFeeOf chains chain (Text.pack txid)
            fee `shouldBe` 226
            size `shouldSatisfy` (<= 226)

    describe ("A payment on " <> net <> " at 10 satoshis per byte") $
      -- 226 bytes × 10 = 2260
      it "pays the node the 2260 satoshis hw states" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            txid <- pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]
            (fst <$> nodeFeeOf chains chain (Text.pack txid)) `shouldReturn` 2260

    describe ("A payment on " <> net <> " of the least a node relays") $
      forM_ (dustLimits chain) $ \(kind, address, limit) ->
        it ("to " <> kind <> ", " <> show limit <> " satoshis, is taken by the node") $
          onChains $ \chains ->
            withPaidWallet chains chain $ \home -> do
              txid <- pay chains chain home testMnemonic [address <> ":" <> show limit <> "sat", "--fee-rate", "1"]
              mempoolOf chains chain `shouldReturn` [Text.pack txid]

    describe ("A payment on " <> net <> " made from the change of another, before any block") $ do
      let -- 30000 to nobody, with 67740 of change, then 20000 to nobody
          -- from that change: 226 × 10 = 2260 of fee each
          twice chains home = do
            first <- pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]
            _ <- syncNow home
            second <- pay chains chain home testMnemonic [otherOutsideAddress <> ":0.0002", "--fee-rate", "10"]
            return (first, second)

      it "is taken by the node, beside the payment it spends from" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            (first, second) <- twice chains home
            held <- mempoolOf chains chain
            held `shouldMatchList` [Text.pack first, Text.pack second]

      -- The second payment is listed above the first, which it spends
      -- from. 20000 + 2260 = 22260, and 30000 + 2260 = 32260.
      it "is listed above the payment it spends from, both unconfirmed" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- twice chains home
            _ <- syncNow home
            txsOn home chain `shouldReturn` [(TxDebit, -22260, Just 0), (TxDebit, -32260, Just 0), (TxCredit, 100000, Just 1)]

      -- 67740 - 22260 = 45480
      it "leaves the change of the second payment as the one coin of the wallet, once a block holds both" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            _ <- twice chains home
            mine chains chain 1
            _ <- syncNow home
            coinsOn home chain `shouldReturn` [(45480, 1, True)]

    -- Every other chain of the tests is dated long ago, and hw then sets
    -- no lock time. Here the best block, 103, is dated now. hw sets the
    -- height of that block as the lock time, or an earlier height now
    -- and then, so that no miner gains by mining the block again. A node
    -- takes a transaction whose lock time is at most the height of its
    -- best block.
    describe ("A payment on " <> net <> " prepared when the best block is fresh") $ do
      it "carries a lock time, at most the height of that block" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            mineNow chains chain
            _ <- syncNow home
            txid <- pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]
            lockTime <- lockTimeOf chains chain (Text.pack txid)
            lockTime `shouldSatisfy` (\height -> height >= 1 && height <= 103)

      it "is taken by the node, with that lock time" $
        onChains $ \chains ->
          withPaidWallet chains chain $ \home -> do
            mineNow chains chain
            _ <- syncNow home
            txid <- pay chains chain home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "10"]
            mempoolOf chains chain `shouldReturn` [Text.pack txid]

  describe "A payment on btcreg from a native SegWit coin at 1 satoshi per virtual byte" $
    -- A SegWit input, a legacy output and SegWit change: 10.5 + 68 + 34 +
    -- 31 = 143.5, which rounds up to 144 virtual bytes × 1 = 144 of fee.
    -- The node counts the witness as it is signed, a little under.
    it "pays the 144 satoshis hw states, over no more virtual bytes than hw counted" $
      onChains $ \chains -> do
        mine chains Btc 101
        withWallet chains testMnemonic $ \home -> do
          address <- receiveOn home Btc "bip84"
          _ <- payFromNode chains Btc address 100000
          mine chains Btc 1
          _ <- syncNow home
          txid <- pay chains Btc home testMnemonic [outsideAddress <> ":0.0003", "--fee-rate", "1"]
          (fee, size) <- nodeFeeOf chains Btc (Text.pack txid)
          fee `shouldBe` 144
          size `shouldSatisfy` (<= 144)

  describe "A payment on btcreg to an address of each kind at once, each paid the least a node relays" $ do
    let everyKind chains home =
          pay chains Btc home testMnemonic ([address <> ":" <> show limit <> "sat" | (_, address, limit) <- dustLimits Btc] <> ["--fee-rate", "1"])

    it "is taken by the node" $
      onChains $ \chains ->
        withPaidWallet chains Btc $ \home -> do
          txid <- everyKind chains home
          mempoolOf chains Btc `shouldReturn` [Text.pack txid]

    -- The outputs are 34, 32, 31, 43 and 43 bytes, and the change 34:
    -- 10 + 148 + 217 = 375 bytes × 1 = 375 of fee. The five amounts are
    -- 546 + 540 + 294 + 330 + 330 = 2040, so 2040 + 375 = 2415 leave the
    -- portfolio.
    it "takes the five amounts and the fee of its 375 bytes from the portfolio" $
      onChains $ \chains ->
        withPaidWallet chains Btc $ \home -> do
          _ <- everyKind chains home
          _ <- syncNow home
          txsOn home Btc `shouldReturn` [(TxDebit, -2415, Just 0), (TxCredit, 100000, Just 1)]
