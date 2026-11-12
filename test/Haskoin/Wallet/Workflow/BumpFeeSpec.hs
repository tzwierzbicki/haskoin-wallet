{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A stuck payment of a portfolio replaced by bumpfee at a higher fee
-- rate: what the replacement spends and pays, the portfolio it is
-- replaced in, the payments bumpfee does not replace, and how the history
-- and the pending transactions show both.
module Haskoin.Wallet.Workflow.BumpFeeSpec (spec) where

import Data.IORef (modifyIORef, readIORef)
import Data.List (find, findIndex, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import Numeric.Natural (Natural)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  bumpFeeSpec ctx

-- | Sign and send a pending transaction: the id of the transaction sent
signAndSend :: FilePath -> String -> IO String
signAndSend home nosig = do
  _ <- answer home ["signtx", nosig] seedOnce
  txidOf =<< txOf =<< answer home ["broadcast", nosig, "--yes"] []

-- | The ids of the transactions a replacement replaces, as typed
replacesOf :: TxInfo -> [String]
replacesOf info = case txInfoStatus info of
  Pending status -> Text.unpack . txHashToHex <$> pendingReplaces status
  OnChain _ -> []

-- | The transactions of the history: each by its id, dead or not, with its
-- confirmations
historyOf :: Response -> Either String [(String, Bool, Natural)]
historyOf (ResponseTxs groups) =
  Right [(Text.unpack (txHashToHex (chainTxid status)), chainDead status, chainConfirmations status) | g <- groups, ListedTx info _ <- groupRows g, OnChain status <- [txInfoStatus info]]
historyOf other = Left (show other)

-- | The pending transactions of a listing
pendingInfosOf :: Response -> Either String [TxInfo]
pendingInfosOf (ResponsePendingTxs _ held) = Right (snd <$> held)
pendingInfosOf other = Left (show other)

-- | Whether each pending transaction is in conflict
conflictsOf :: [TxInfo] -> [Bool]
conflictsOf infos = [pendingConflicted status | info <- infos, Pending status <- [txInfoStatus info]]

-- | The nosigHashes of the pending transactions, as typed
pendingNosigsOf :: Response -> Either String [String]
pendingNosigsOf (ResponsePendingTxs _ held) =
  Right [Text.unpack (txHashToHex (pendingNosigHash status)) | (_, info) <- held, Pending status <- [txInfoStatus info]]
pendingNosigsOf other = Left (show other)

-- The payment of 20000 at 10 satoshis a byte pays 2260 for its 226 bytes,
-- and leaves 27740 of change. At 20 satoshis a byte, its replacement pays
-- 4520 and leaves 25480.
bumpFeeSpec :: Ctx -> Spec
bumpFeeSpec ctx = do
  describe "A stuck payment replaced by bumpfee" $ do
    it "is replaced, still pending, by one that takes the higher fee from its change" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 25480)], 4520)

    -- The best block of the chain is 800002. One time in ten the lock time
    -- is below it, by up to 99 blocks.
    it "gets the lock time of the best block the wallet knows, or one up to 99 blocks below it, as a payment does" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        summaryLockTime (txInfoSummary bumped) `shouldSatisfy` (\lockTime -> lockTime <= 800002 && lockTime >= 800002 - 99)

    -- The best block was mined nine hours before the test: more than the
    -- eight hours after which the wallet counts as behind the chain. The
    -- payment is prepared with --yes, which answers the same question.
    it "asks before replacing while the wallet is behind the chain, and replaces with a lock time of 0 on yes" $
      withChainMinedAgo ctx [] (9 * 3600) (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--yes"] []
        stuck <- signAndSend home nosig
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf "The wallet is behind the chain: its best block, 800002, was mined 9 hours ago"
        run `shouldSatisfy` printed "The transaction was not prepared: run sync first"
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        summaryLockTime (txInfoSummary bumped) `shouldBe` 0

    -- At 131 satoshis a byte, the 226 bytes pay 29606 and leave 50000 -
    -- 20000 - 29606 = 394 of change, less than the 546 nodes relay: the fee
    -- is all the rest, 50000 - 20000 = 30000
    it "gives the fee a change below the dust limit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "131", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.empty, 30000)

    it "is replaced once the sync has found it on the network" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 25480)], 4520)

    it "is replaced when bumpfee names it by the short id of its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", shortOf stuck, "--fee-rate", "20"] []
        replacesOf bumped `shouldBe` [stuck]

    it "is named by its replacement, by its short id, which pendingtxs shows" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacesOf bumped `shouldBe` [stuck]
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n  portfolio0 btc, replaces " <> Text.pack (shortOf stuck) <> ", ")

    -- Yellow is colour 33 of the terminal. --no-pager prints the listing
    -- itself.
    it "is named by its replacement in yellow on a terminal, in the compact view of pendingtxs" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        run <- hwOnScreen [] home ["pendingtxs", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf ("\ESC[0;33mreplaces " <> Text.pack (shortOf stuck) <> "\ESC[0m")

    it "is named by its replacement in yellow on a terminal, in the description of the table of pendingtxs" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        run <- hwOnScreen [] home ["pendingtxs", "--tabular", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf ("\ESC[0;33mreplaces " <> Text.pack (shortOf stuck) <> "\ESC[0m")

    it "is named by its replacement in full and in yellow on a terminal, in the verbose view of pendingtxs" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        run <- hwOnScreen [] home ["pendingtxs", "--verbose", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf ("\ESC[0;33m" <> Text.pack stuck <> "\ESC[0m")

    it "gives way to its replacement once sent: the history shows it dead, and the replacement confirmed" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home nosig
        modifyIORef chain mine
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 25480)]
        history <- hwJson home ["txs"] []
        (filter (\(tid, _, _) -> tid `elem` [stuck, replacement]) <$> historyOf history)
          `shouldSatisfy` either (const False) (\rows -> sort rows == sort [(stuck, True, 0), (replacement, False, 1)])

    -- The stuck transaction stays first, at its place in the mempool. The
    -- replacement left 25480 of the 50000 paid.
    it "counts only its replacement in the balances of the history: the dead transaction shows none" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- signAndSend home nosig
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (txBalancesOf <$> hwJson home ["txs"] []) `shouldReturn` Right [Nothing, Just 25480, Just 50000]

    it "leaves the pending transactions once its replacement is sent" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- signAndSend home nosig
        (pendingNosigsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [nosig]

    -- The store still has the stuck payment: its replacement spends the
    -- same coin, which is no conflict
    it "is no conflict for its replacement, after a sync that finds it still on the network" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["sync"] []
        listed <- hwJson home ["pendingtxs"] []
        (conflictsOf <$> pendingInfosOf listed) `shouldBe` Right [False]
        sent <- txOf =<< answer home ["broadcast", nosig, "--yes"] []
        pendingState sent `shouldBe` Just (True, True)

    -- The payment sent left 27740 of change at /1/0, mined since. The
    -- second pays 10000 of it with its change sent back there: 27740 -
    -- 10000 - 2260 = 15480 at 10 satoshis a byte, and 27740 - 10000 - 4520
    -- = 13220 at 20.
    it "takes the higher fee from the change --change-back sent to a change address" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--change-back", textOf (internalAddr 0), "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (paidToWallet bumped, feeOf bumped) `shouldBe` (Map.fromList [(internalAddr 0, 13220)], 4520)

    it "takes the higher fee from the change --change-back sent back to the address it spends from" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--change-back", textOf (externalAddr 0), "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        paidToWallet bumped `shouldBe` Map.fromList [(externalAddr 0, 25480)]

    -- The change of 27740 went to /0/2, a receiving address the payment
    -- does not spend from, which bumpfee cannot tell from a payment: it
    -- keeps it, and adds the coin of 30000 at /0/1. Two inputs and three
    -- outputs make 10 + 2 * 148 + 3 * 34 = 408 bytes, a fee of 8160 at 20
    -- satoshis a byte, and 80000 - 20000 - 27740 - 8160 = 24100 of change
    -- at a new change address.
    it "keeps what --change-back sent to another receiving address, as it keeps a payment, and takes the higher fee from a coin it adds" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        third <- receiveWith home []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--change-back", third, "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        (paidToWallet bumped, feeOf bumped) `shouldBe` (Map.fromList [(externalAddr 2, 27740), (internalAddr 0, 24100)], 8160)

    -- BIP 125, rule 2: a replacement spends no unconfirmed coin that its
    -- originals did not. At 160 sat/vB the change cannot pay: 192 bytes
    -- without change need 30720, and the coin of 50000 holds 30000 beyond
    -- the 20000 paid. The only other coin is the unconfirmed change of a
    -- second payment, from the coin at /0/1: the recipient pays the rest.
    it "adds no unconfirmed coin, not even the wallet's own change, and has the recipient pay instead" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        other <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--spend-from", textOf (externalAddr 1), "--fee-rate", "10"] []
        _ <- signAndSend home other
        _ <- answer home ["sync"] []
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "160", "--yes"] []
        concatMap myInputsOutpoints (Map.elems bumped.txInfoSummary.summaryMyInputs) `shouldBe` [OutPoint (txHash (twoAddressesTx ctx)) 0]

    -- The payment to the second receiving address is no change: only the
    -- change at /1/0 pays the higher fee
    it "keeps what it pays another address of the wallet" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (externalAddr 1) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        paidToWallet bumped `shouldBe` Map.fromList [(externalAddr 1, 20000), (internalAddr 0, 25480)]

    it "is refused a fee rate that is not above its own" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "10"] [])
          `shouldReturn` Right "The transaction pays 10 sat/vB: give a --fee-rate of at least 11"

    it "cannot be replaced once in a block" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        stuck <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] [])
          `shouldReturn` Right "The transaction is in a block already: nothing can replace it"

    -- Nothing is asked about the fee rate of 200 for a transaction that is
    -- not there: the input is empty, and a question would end the command
    it "is looked up before anything is asked" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["bumpfee", Text.unpack (txHashToHex (fakeTxHash 9)), "--fee-rate", "200"] []
        run `shouldSatisfy` printed ("The wallet has no transaction " <> txHashToHex (fakeTxHash 9))
        runErr run `shouldNotSatisfy` Text.isInfixOf "Prepare the transaction anyway"

    -- At 120 satoshis a byte the coin still pays: 27120 of fee, 2880 of change
    it "is not replaced at a fee rate above 100 satoshis per virtual byte unless the user agrees" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "120"] ["n"])
          `shouldReturn` Right "The transaction was not prepared: give a lower --fee-rate"

    -- 226 bytes at 120 satoshis a byte are a fee of 27120, which leaves
    -- 50000 - 20000 - 27120 = 2880 of change. Nothing is typed: a question
    -- would cancel the command.
    it "is replaced at a fee rate above 100 satoshis per virtual byte without asking with --yes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "120", "--yes"] []
        (paidToWallet bumped, feeOf bumped) `shouldBe` (Map.fromList [(internalAddr 0, 2880)], 27120)

    it "is not stuck before it is sent" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        (errorOf <$> hwJson home ["bumpfee", nosig, "--fee-rate", "20"] [])
          `shouldReturn` Right "The transaction is not sent yet: delete it with deletetx, and prepare it again at the fee rate you want"

  -- At 30 satoshis a byte, the replacement of the replacement pays 6780 and
  -- leaves 23220
  describe "A stuck payment replaced once" $ do
    it "is not replaced again while its replacement is pending" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack nosig <> ". Send it, or delete it with deletetx")

    it "names its pending replacement by its txid once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        txid <- txidOf =<< txOf =<< answer home ["signtx", nosig] seedOnce
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack txid <> ". Send it, or delete it with deletetx")

    it "is not replaced again once the sync has found it on the network either" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack nosig <> ". Send it, or delete it with deletetx")

    it "is bumped through its replacement once that one is sent" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home nosig
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("The transaction is replaced by " <> Text.pack replacement <> ", which is sent: bump that one instead")
        bumped <- txOf =<< answer home ["bumpfee", replacement, "--fee-rate", "30"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 23220)], 6780)
        replacesOf bumped `shouldBe` [replacement]

    it "cannot have its replacement sent once it is in a block" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- answer home ["signtx", nosig] seedOnce
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["broadcast", nosig, "--yes"] [])
          `shouldReturn` Right ("The transaction " <> Text.pack stuck <> " it replaces is in a block already: delete this replacement with deletetx")

  -- portfolio0 pays 20000 to the first address of portfolio1, and another
  -- wallet of the seed spends that output from portfolio1 before anything
  -- confirms
  describe "A stuck payment an output of which another portfolio spends" $
    it "is not replaced, since that spend would be removed too" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        syncedTwoPortfolios home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo account1Addr "0.0002", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        [sent] <- inMempool <$> readIORef chain
        index <- maybe (fail "The payment does not pay portfolio1") return $ findIndex ((== Right account1Addr) . scriptToAddressBS btc ctx . (.script)) sent.outputs
        let spender = spendingTx ctx [(txHash sent, fromIntegral index)] [(otherAddr 2, 15000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(spender, Nothing)]}
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] [])
          `shouldReturn` Right ("The transaction " <> txHashToHex (txHash spender) <> " of portfolio1 of wallet0 on btc spends an output of the transaction, and would be removed with it: hw cannot replace it from this portfolio")

  -- The child pays 10000 at 50 satoshis a byte: 11300 of fee for its 226
  -- bytes. Replacing both, 260 bytes, must pay 2260 + 11300 + 260 = 13820:
  -- 54 satoshis a byte, which pays 14040 and leaves 50000 - 30000 - 14040 =
  -- 5960 of change.
  describe "A stuck payment whose change a payment at a higher rate spends" $ do
    let withCostlyChild home = do
          stuck <- sentPayment home
          _ <- answer home ["sync"] []
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "50"] []
          _ <- signAndSend home nosig
          return stuck
    it "is refused a rate that cannot pay for both, before any question" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- withCostlyChild home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] []
        runErr run `shouldNotSatisfy` Text.isInfixOf "Replace both"
        run `shouldSatisfy` printed "The replacement pays 5200 satoshis of fee, and must pay 13820: the fees of the transactions it replaces, and its own 260 virtual bytes. Give a --fee-rate of at least 54"

    it "is replaced with the payment at a rate that pays for both" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- withCostlyChild home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "54"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 5960)], 14040)

  describe "A stuck payment replaced from an online computer" $
    it "has its replacement exported, signed offline, imported back and sent, which retires it" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["importtx", signer </> "signed.psbt"] []
        stuck <- txidOf =<< txOf =<< answer online ["broadcast", nosig, "--yes"] []
        bumpNosig <- nosigOf =<< txOf =<< answer online ["bumpfee", stuck, "--fee-rate", "20", "--output", online </> "bump.json"] []
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "bump.json", "--output", signer </> "bump-signed.psbt"] seedOnce
        replacesOf signed `shouldBe` [stuck]
        _ <- answer online ["importtx", signer </> "bump-signed.psbt"] []
        _ <- answer online ["broadcast", bumpNosig, "--yes"] []
        (pendingNosigsOf <$> hwJson online ["pendingtxs"] []) `shouldReturn` Right [bumpNosig]

  -- The payment of 20000 from the coin of 50000 of portfolio1, at 10 and
  -- then at 20 satoshis a byte, as above: its replacement pays 4520.
  -- portfolio1 is active while the payment is prepared, and portfolio0
  -- once it is sent.
  describe "A stuck payment of a portfolio" $ do
    it "is replaced in the portfolio that pays it, which bumpfee finds whichever portfolio is active" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        switchTo home 0 0
        stuck <- signAndSend home nosig
        answered <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        placeOf answered `shouldBe` Right portfolio1Place
        bumped <- txOf answered
        (paidToOthers bumped, feeOf bumped) `shouldBe` (Map.fromList [(otherAddr 0, 20000)], 4520)

    -- The payment of 20000 to portfolio0 at externalAddr 0 is replaced by
    -- one that pays it the same 20000. The address holds 20000
    -- unconfirmed in one coin, from one transaction, before and after.
    it "takes the place of a payment to another portfolio in the history of that portfolio, though it pays the same amount" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (externalAddr 0) "0.0002", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        _ <- answer home ["sync"] []
        bumpNosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home bumpNosig
        _ <- answer home ["sync"] []
        switchTo home 0 0
        history <- hwJson home ["txs"] []
        (sort <$> historyOf history) `shouldBe` Right (sort [(stuck, True, 0), (replacement, False, 0)])

    -- A miner that never saw the replacement puts the stuck payment in
    -- block 800003, and the network drops the replacement. The stuck
    -- payment has 1 confirmation at that best block.
    it "stays in the history of the portfolio it pays, dead, when the network confirms the stuck payment instead" $
      withChain ctx (chainPaying ctx account1Addr) $ \home chain -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (externalAddr 0) "0.0002", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        sent <- readIORef chain
        stuckTx <- maybe (fail "The stuck payment is not on the chain") return $ find ((== stuck) . txidArg) (fst <$> sent.chainTxs)
        _ <- answer home ["sync"] []
        bumpNosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home bumpNosig
        _ <- answer home ["sync"] []
        modifyIORef chain $ \now -> mine now {chainTxs = [entry | entry@(tx, _) <- now.chainTxs, txidArg tx /= replacement] <> [(stuckTx, Nothing)]}
        _ <- answer home ["sync"] []
        switchTo home 0 0
        history <- hwJson home ["txs"] []
        (sort <$> historyOf history) `shouldBe` Right (sort [(stuck, False, 1), (replacement, True, 0)])

    -- The payment of 60000 from the coins of both types of 'bothTypesTx':
    -- 10.5 + 148 + 68 + 34 + 34 = 294.5, so 295 virtual bytes. At 10
    -- satoshis a virtual byte it pays 2950 and leaves 80000 - 60000 - 2950
    -- = 17050 of change; at 20, 5900 and 80000 - 60000 - 5900 = 14100.
    it "takes the higher fee of a payment of bip44 and bip84 coins from its change, at the size of both inputs" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (paidToWallet bumped, feeOf bumped) `shouldBe` (Map.fromList [(internalAddr 0, 14100)], 5900)

    -- The payment of 40000 from the bip44 coin of 50000 leaves 7740 of
    -- change. At 60 satoshis a byte its 226 bytes need 13560, more than
    -- the 10000 beyond what it pays. With the bip84 coin of 30000, the
    -- replacement is 295 virtual bytes, as above: it pays 17700 and leaves
    -- 80000 - 40000 - 17700 = 22300 of change.
    it "adds a confirmed coin of the other type of the portfolio when its change cannot pay, as the user agrees" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--coin-type", "bip44", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60"] ["y"]
        spentFromWallet bumped `shouldBe` Map.fromList [(externalAddr 0, 50000), (segwitExternalAddr 0, 30000)]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 40000)], Map.fromList [(internalAddr 0, 22300)], 17700)

  describe "A payment bumpfee does not replace" $ do
    it "is a payment the portfolio receives: only its payer can replace it" $
      withChain ctx (Chain 800002 [(payment ctx, Nothing)]) $ \home _ -> do
        synced home
        let received = Text.unpack (txHashToHex (txHash (payment ctx)))
        (errorOf <$> hwJson home ["bumpfee", received, "--fee-rate", "20"] [])
          `shouldReturn` Right "Only a payment from the coins of the portfolio can be replaced: this transaction spends coins of another wallet"

    it "is one of Bitcoin Cash, which replaces none" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        let received = Text.unpack (txHashToHex (txHash (cashCoinTx ctx)))
        (errorOf <$> hwJson home ["bumpfee", received, "--fee-rate", "20"] [])
          `shouldReturn` Right "Bitcoin Cash replaces no transaction: its nodes keep the first one they see"

  -- The payment of 40000 from the coin of 50000 at /0/0 leaves 7740 of
  -- change. At 60 satoshis a byte, the coin alone cannot pay even the 11520
  -- of a transaction without change. With the coin of 30000 at /0/1, the
  -- replacement of 374 bytes pays 22440 and leaves 80000 - 40000 - 22440 =
  -- 17560 of change.
  describe "A stuck payment whose change cannot pay the higher fee" $ do
    let chain = confirmedChain (twoAddressesTx ctx)
        stuckPayment home = do
          synced home
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
          signAndSend home nosig
    it "takes a confirmed coin of the portfolio, as the user agrees" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 40000)], Map.fromList [(internalAddr 0, 17560)], 22440)

    it "names the coin it asks to add, by its short id" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "60"] ["y"]
        run `shouldSatisfy` printedSentence "The coins of the transaction cannot pay its fee at 60 sat/vB, and this coin of the portfolio would:"
        runErr run `shouldSatisfy` Text.isInfixOf ("\n    0.00030000 btc  at " <> externalAddrsText !! 1 <> "  bip44/0/1\n  " <> Text.pack (shortCoinArg (twoAddressesTx ctx) 1) <> "\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Add it? [y/N]"

    -- Without the coin, the replacement of 192 bytes pays 11520 and has no
    -- change: the recipient pays the 40000 + 11520 - 50000 = 1520 missing
    it "takes the higher fee from what the recipient receives when the user declines the coin, as the user agrees" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60"] ["n", "y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 38480)], Map.empty, 11520)

    it "offers the recipient once the coin is declined" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "60"] ["n", "n"]
        runErr run `shouldSatisfy` Text.isInfixOf "Add it? [y/N]"
        run `shouldSatisfy` printedSentence "The coins of the transaction cannot pay its fee at 60 sat/vB, and no coin of the portfolio is added. The recipient can pay the 0.00001520 btc missing out of what it receives:"
        runErr run `shouldSatisfy` Text.isInfixOf ("\n  " <> Text.pack (textOf (otherAddr 0)) <> "  0.00040000 btc\n    0.00038480 btc paying it\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Reduce what it receives? [y/N]"

    it "is not replaced when the user wants neither the coin added nor the recipient paying" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "60"] ["n", "n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "takes a confirmed coin of the portfolio without asking with --yes" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 40000)], Map.fromList [(internalAddr 0, 17560)], 22440)

  -- The send-all of the coins of 50000 and 30000 pays 76600 to one address
  -- at 10 satoshis a byte, 3400 for its 340 bytes, with no change, and
  -- leaves no coin in the portfolio. At 20 satoshis a byte, its replacement
  -- pays 6800: the address pays the 76600 + 6800 - 80000 = 3400 missing,
  -- and receives 73200.
  describe "A stuck send-all, whose recipient alone can pay the higher fee" $ do
    let sentAll home = do
          synced home
          nosig <- nosigOf =<< txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
          signAndSend home nosig
    it "is replaced by one that takes the higher fee from what the recipient receives, as the user agrees" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 73200)], Map.empty, 6800)

    it "names the recipient and what it would receive when it asks" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        run `shouldSatisfy` printedSentence "The coins of the transaction cannot pay its fee at 20 sat/vB, and no coin of the portfolio is added. The recipient can pay the 0.00003400 btc missing out of what it receives:"
        runErr run `shouldSatisfy` Text.isInfixOf ("\n  " <> Text.pack (textOf (otherAddr 0)) <> "  0.00076600 btc\n    0.00073200 btc paying it\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Reduce what it receives? [y/N]"

    it "is not replaced when the user does not want the recipient to pay" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] ["n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "takes the higher fee from what the recipient receives without asking with --yes" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 73200)], Map.empty, 6800)

    -- At 250 satoshis a byte, the 340 bytes pay 85000, more than the 80000
    -- of the coins: 76600 + 85000 - 80000 = 81600 are missing. Nothing is
    -- typed: the rate above 100 is not even asked about.
    it "is refused when the recipient cannot pay either, before any question" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "250"] []
        run `shouldSatisfy` printed "The coins of the transaction cannot pay its fee at 250 sat/vB, and no confirmed coins of the portfolio would. The recipient cannot pay the 81600 satoshis missing without being left with less than the dust limit of 546 satoshis"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Reduce what it receives?"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Prepare the transaction anyway"

  -- The send-all to two addresses, 30000 to the second and the 80000 -
  -- 30000 - 3740 = 46260 that remain to the first, pays 3740 for its 374
  -- bytes. At 20 satoshis a byte, its replacement pays 7480: 3740 are
  -- missing, which one of them pays alone, or each pays 1870 of. The
  -- outputs are in an order drawn at random, so the recipient is picked by
  -- its address, and the lines are checked without their numbers.
  describe "A stuck send-all to several recipients" $ do
    let sentAll home = do
          synced home
          nosig <- nosigOf =<< txOf =<< answer home ["preparesendall", textOf (otherAddr 0), fixedTo (otherAddr 1) "0.0003", "--fee-rate", "10"] []
          signAndSend home nosig
    it "is replaced by one that takes the higher fee from the recipient the user picks" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] [Text.pack (textOf (otherAddr 1))]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 46260), (otherAddr 1, 26260)], Map.empty, 7480)

    it "is replaced by one that takes the higher fee from all the recipients in equal shares, when the user says all" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["all"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 44390), (otherAddr 1, 28130)], Map.empty, 7480)

    it "lists the recipients with what each would receive, and asks which pays" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] ["no"]
        run `shouldSatisfy` printedSentence "The 0.00003740 btc missing can come out of what the recipients receive, one of them paying it alone, or all of them in equal shares:"
        -- Numbered in the order of the outputs, which the test does not pin
        runErr run `shouldSatisfy` Text.isInfixOf ("  " <> Text.pack (textOf (otherAddr 0)) <> "  0.00046260 btc\n     0.00042520 btc paying it alone, 0.00044390 btc paying its share\n")
        runErr run `shouldSatisfy` Text.isInfixOf ("  " <> Text.pack (textOf (otherAddr 1)) <> "  0.00030000 btc\n     0.00026260 btc paying it alone, 0.00028130 btc paying its share\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Who pays it? Type 1, 2 or all, or no [no]"

    it "is not replaced when nobody pays, which an empty answer says" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] [""]) `shouldReturn` Right "The transaction was not replaced"

    -- --yes answers yes, and this is no yes-or-no question
    it "still asks which recipient pays with --yes" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] [Text.pack (textOf (otherAddr 1))]
        paidToOthers bumped `shouldBe` Map.fromList [(otherAddr 0, 46260), (otherAddr 1, 26260)]

  -- The second payment, of 10000, spends the change of 27740 of the first
  -- and leaves 15480 of change at /1/1. One transaction of one input and
  -- three outputs, 260 bytes, replaces both at 20 satoshis a byte: it pays
  -- 5200 and leaves 50000 - 30000 - 5200 = 14800 at /1/1.
  describe "A stuck payment whose change another payment spends" $ do
    let withSecond home = do
          stuck <- sentPayment home
          _ <- answer home ["sync"] []
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10"] []
          second <- signAndSend home nosig
          return (stuck, second)
    it "is replaced with that payment, as the user agrees, by one that pays the recipients of both" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (stuck, second) <- withSecond home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 14800)], 5200)
        replacesOf bumped `shouldBe` [stuck, second]

    it "names that payment by its short id when it asks" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (stuck, second) <- withSecond home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf ("Replacing the transaction removes those of the portfolio that spend its change:\n  " <> Text.pack (shortOf second) <> "\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Replace both with one transaction that pays the recipients of both? [y/N]"

    it "is not replaced when the user does not want that payment replaced too" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (stuck, _) <- withSecond home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] ["n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "is replaced with that payment without asking with --yes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (stuck, second) <- withSecond home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 14800)], 5200)
        replacesOf bumped `shouldBe` [stuck, second]
