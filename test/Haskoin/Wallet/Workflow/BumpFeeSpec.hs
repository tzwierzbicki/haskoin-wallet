{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A stuck payment of a portfolio replaced by bumpfee at a higher fee
-- rate: what the replacement spends and pays, the portfolio it is
-- replaced in, the payments bumpfee does not replace, and how the history
-- and the pending transactions show both.
module Haskoin.Wallet.Workflow.BumpFeeSpec (spec) where

import Control.Monad (forM_)
import Data.IORef (modifyIORef, readIORef)
import Data.List (find, findIndex, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), LeftReason (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import Numeric.Natural (Natural)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  bumpFeeSpec

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
bumpFeeSpec :: Spec
bumpFeeSpec = do
  describe "A stuck payment replaced by bumpfee" $ do
    it "is replaced, still pending, by one that takes the higher fee from its change" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 25480)], 4520)

    -- The fee at 20 is 4520, 2260 more than the payment paid: the
    -- recipient gets 20000 - 2260 = 17740, and the change keeps its 27740
    it "takes what the higher fee adds from the recipient with --recipients-pay, and the change keeps its amount" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--recipients-pay"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 17740)], Map.fromList [(internalAddr 0, 27740)], 4520)

    -- The best block of the chain is 800002. One time in ten the lock time
    -- is below it, by up to 99 blocks.
    it "gets the lock time of the best block the wallet knows, or one up to 99 blocks below it, as a payment does" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        summaryLockTime (txInfoSummary bumped) `shouldSatisfy` (\lockTime -> lockTime <= 800002 && lockTime >= 800002 - 99)

    -- The best block was mined nine hours before the test: more than the
    -- eight hours after which the wallet counts as behind the chain. The
    -- payment is prepared with --yes, which answers the same question.
    it "asks before replacing while the wallet is behind the chain, and replaces with a lock time of 0 on yes" $
      withChainMinedAgo [] (9 * 3600) paidChain $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--yes"] []
        stuck <- signAndSend home nosig
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf "The wallet is behind the chain.\nIts best block, 800002, was mined 9 hours ago.\n"
        run `shouldSatisfy` printed "The transaction was not prepared: run sync first"
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        summaryLockTime (txInfoSummary bumped) `shouldBe` 0

    -- At 131 satoshis a byte, the 226 bytes pay 29606 and leave 50000 -
    -- 20000 - 29606 = 394 of change, less than the 546 nodes relay: the fee
    -- is all the rest, 50000 - 20000 = 30000
    it "gives the fee a change below the dust limit" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "131", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.empty, 30000)

    it "is replaced once the sync has found it on the network" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 25480)], 4520)

    it "is replaced when bumpfee names it by the short id of its txid" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", shortOf stuck, "--fee-rate", "20"] []
        replacesOf bumped `shouldBe` [stuck]

    it "is named by its replacement, by its short id, which pendingtxs shows" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacesOf bumped `shouldBe` [stuck]
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n  portfolio0 btc, replaces " <> Text.pack (shortOf stuck) <> ", ")

    -- Yellow is colour 33 of the terminal. --no-pager prints the listing
    -- itself.
    it "is named by its replacement in yellow on a terminal, in the compact view of pendingtxs" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        run <- hwOnScreen [] home ["pendingtxs", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf ("\ESC[0;33mreplaces " <> Text.pack (shortOf stuck) <> "\ESC[0m")

    it "is named by its replacement in yellow on a terminal, in the description of the table of pendingtxs" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        run <- hwOnScreen [] home ["pendingtxs", "--tabular", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf ("\ESC[0;33mreplaces " <> Text.pack (shortOf stuck) <> "\ESC[0m")

    it "is named by its replacement in full and in yellow on a terminal, in the verbose view of pendingtxs" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        run <- hwOnScreen [] home ["pendingtxs", "--verbose", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf ("\ESC[0;33m" <> Text.pack stuck <> "\ESC[0m")

    it "gives way to its replacement once sent: the history shows it dead, and the replacement confirmed" $
      withChain paidChain $ \home chain -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home nosig
        modifyIORef chain mine
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 25480)]
        history <- hwJson home ["txs"] []
        (filter (\(tid, _, _) -> tid `elem` [stuck, replacement]) <$> historyOf history)
          `shouldSatisfy` either (const False) (\rows -> sort rows == sort [(stuck, True, 0), (replacement, False, 1)])

    -- The block that confirmed the replacement came after the wallet last
    -- saw the stuck payment alive, so the replacement is listed above it.
    -- The replacement left 25480 of the 50000 paid.
    it "counts only its replacement in the balances of the history: the dead transaction shows none, under its replacement" $
      withChain paidChain $ \home chain -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- signAndSend home nosig
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (txBalancesOf <$> hwJson home ["txs"] []) `shouldReturn` Right [Just 25480, Nothing, Just 50000]

    -- The stuck payment took 20000 paid + 2260 of fee = 22260
    it "names its replacement in the history once it is dead, by its short id, before what it paid" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home nosig
        _ <- answer home ["sync"] []
        run <- hw home ["txs"] []
        runOut run
          `shouldSatisfy` Text.isInfixOf ("-0.00022260 btc  dead\n  replaced by " <> Text.pack (shortOf replacement) <> ", to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\n")

    -- A miner that never saw the replacement puts the stuck payment in
    -- block 800003, and the network drops the replacement. The
    -- replacement took 20000 paid + 4520 of fee = 24520.
    it "is named in the history by its replacement instead, when the network confirms the stuck payment after all" $
      withChain paidChain $ \home chain -> do
        stuck <- sentPayment home
        sent <- readIORef chain
        stuckTx <- maybe (fail "The stuck payment is not on the chain") return $ find ((== stuck) . txidArg) (fst <$> sent.chainTxs)
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home nosig
        _ <- answer home ["sync"] []
        modifyIORef chain $ \now -> mine now {chainTxs = [entry | entry@(tx, _) <- now.chainTxs, txidArg tx /= replacement] <> [(stuckTx, Nothing)]}
        _ <- answer home ["sync"] []
        run <- hw home ["txs"] []
        runOut run
          `shouldSatisfy` Text.isInfixOf ("-0.00024520 btc  dead\n  replaced by " <> Text.pack (shortOf stuck) <> ", to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\n")
        runOut run `shouldNotSatisfy` Text.isInfixOf ("replaced by " <> Text.pack (shortOf replacement))

    it "leaves the pending transactions once its replacement is sent" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- signAndSend home nosig
        (pendingNosigsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [nosig]

    -- The store still has the stuck payment: its replacement spends the
    -- same coin, which is no conflict
    it "is no conflict for its replacement, after a sync that finds it still on the network" $
      withChain paidChain $ \home _ -> do
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
      withChain paidChain $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--change-back", textOf (internalAddr 0), "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (paidToWallet bumped, feeOf bumped) `shouldBe` (Map.fromList [(internalAddr 0, 13220)], 4520)

    it "takes the higher fee from the change --change-back sent back to the address it spends from" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
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
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
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
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        other <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--spend-from", textOf (externalAddr 1), "--fee-rate", "10"] []
        _ <- signAndSend home other
        _ <- answer home ["sync"] []
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "160", "--yes"] []
        concatMap myInputsOutpoints (Map.elems bumped.txInfoSummary.summaryMyInputs) `shouldBe` [OutPoint (txHash twoAddressesTx) 0]

    -- The payment to the second receiving address is no change: only the
    -- change at /1/0 pays the higher fee
    it "keeps what it pays another address of the wallet" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (externalAddr 1) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        paidToWallet bumped `shouldBe` Map.fromList [(externalAddr 1, 20000), (internalAddr 0, 25480)]

    it "is refused a fee rate that is not above its own" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "10"] [])
          `shouldReturn` Right "The transaction pays 10 sat/vB: give a --fee-rate of at least 11"

    it "cannot be replaced once in a block" $
      withChain paidChain $ \home chain -> do
        stuck <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] [])
          `shouldReturn` Right "The transaction is in a block already: nothing can replace it"

    -- Nothing is asked about the fee rate of 200 for a transaction that is
    -- not there: the input is empty, and a question would end the command
    it "is looked up before anything is asked" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["bumpfee", Text.unpack (txHashToHex (fakeTxHash 9)), "--fee-rate", "200"] []
        run `shouldSatisfy` printed ("The wallet has no transaction " <> txHashToHex (fakeTxHash 9))
        runErr run `shouldNotSatisfy` Text.isInfixOf "Prepare the transaction anyway"

    -- At 120 satoshis a byte the coin still pays: 27120 of fee, 2880 of change
    it "is not replaced at a fee rate above 100 satoshis per virtual byte unless the user agrees" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "120"] ["n"])
          `shouldReturn` Right "The transaction was not prepared: give a lower --fee-rate"

    -- 226 bytes at 120 satoshis a byte are a fee of 27120, which leaves
    -- 50000 - 20000 - 27120 = 2880 of change. Nothing is typed: a question
    -- would cancel the command.
    it "is replaced at a fee rate above 100 satoshis per virtual byte without asking with --yes" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "120", "--yes"] []
        (paidToWallet bumped, feeOf bumped) `shouldBe` (Map.fromList [(internalAddr 0, 2880)], 27120)

    it "is not stuck before it is sent" $
      withChain paidChain $ \home _ -> do
        nosig <- paymentSigned home
        (errorOf <$> hwJson home ["bumpfee", nosig, "--fee-rate", "20"] [])
          `shouldReturn` Right "The transaction is not sent yet: delete it with deletetx, and prepare it again at the fee rate you want"

  -- At 30 satoshis a byte, the replacement of the replacement pays 6780 and
  -- leaves 23220
  describe "A stuck payment replaced once" $ do
    it "is not replaced again while its replacement is pending" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack nosig <> ". Send it, or delete it with deletetx")

    it "names its pending replacement by its txid once signed" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        txid <- txidOf =<< txOf =<< answer home ["signtx", nosig] seedOnce
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack txid <> ". Send it, or delete it with deletetx")

    it "is not replaced again once the sync has found it on the network either" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack nosig <> ". Send it, or delete it with deletetx")

    it "is bumped through its replacement once that one is sent" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home nosig
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("The transaction is replaced by " <> Text.pack replacement <> ", which is sent: bump that one instead")
        bumped <- txOf =<< answer home ["bumpfee", replacement, "--fee-rate", "30"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 23220)], 6780)
        replacesOf bumped `shouldBe` [replacement]

    -- A first sync puts the stuck payment in the history. The second one
    -- retires the replacement from the pending transactions, so the
    -- history alone tells what replaced the stuck payment.
    it "is bumped through its replacement once the sync has found that one on the network, too" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        replacement <- signAndSend home nosig
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "30"] [])
          `shouldReturn` Right ("The transaction was replaced by " <> Text.pack replacement <> ": bump that one instead")

    it "cannot have its replacement sent once it is in a block" $
      withChain paidChain $ \home chain -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- answer home ["signtx", nosig] seedOnce
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["broadcast", nosig, "--yes"] [])
          `shouldReturn` Right ("The transaction " <> Text.pack stuck <> " it replaces is in a block already: delete this replacement with deletetx")

  -- The stuck payment left 27740 of change at /1/0, a coin of the
  -- portfolio that no block holds yet. Its replacement at 20 satoshis a
  -- byte is prepared and not sent. Once it is sent, the stuck payment is
  -- gone, and its change with it: a payment that spent that change would
  -- never reach its recipient.
  describe "The change of a stuck payment whose replacement is pending" $ do
    let replacedPending home = do
          stuck <- sentPayment home
          _ <- answer home ["sync"] []
          nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
          return (stuck, nosig)
        -- The change of the one payment the network has, as the store
        -- reports it: the outputs of a payment are shuffled
        changeOn chain = do
          [sent] <- inMempool <$> readIORef chain
          index <- maybe (fail "The payment has no change") return $ findIndex ((== Right (internalAddr 0)) . scriptToAddressBS btc . (.script)) sent.outputs
          return (OutPoint (txHash sent) (fromIntegral index), coinArg sent index)
    it "is locked by the replacement, and cannot be spent" $
      withChain paidChain $ \home chain -> do
        _ <- replacedPending home
        (change, _) <- changeOn chain
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(change, 27740, 0, Just LockedByReplacement, False)]

    it "counts for nothing in what the portfolio can spend" $
      withChain paidChain $ \home _ -> do
        _ <- replacedPending home
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 0)]

    -- 27740 in all, none of it spendable, all of it locked by a pending
    -- transaction
    it "is counted by balances as locked by a pending transaction" $
      withChain paidChain $ \home _ -> do
        _ <- replacedPending home
        answered <- hwJson home ["balances"] []
        case answered of
          ResponseBalances groups -> map snd groups `shouldBe` [CoinBalance 27740 0 27740 0 0 0]
          other -> expectationFailure (show other)

    it "is not spent by a payment, which says that the portfolio holds a coin it cannot spend" $
      withChain paidChain $ \home _ -> do
        _ <- replacedPending home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee. The portfolio holds 0.00027740 btc more on btc, in 1 coin it cannot spend now: hw coins says why"

    it "is refused to a payment that names it with --coin, which says that a pending transaction replaces its payment" $
      withChain paidChain $ \home chain -> do
        _ <- replacedPending home
        (_, coin) <- changeOn chain
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10", "--coin", coin] [])
          `shouldReturn` Right (Text.pack ("None of the coins chosen can be spent now: " <> coin <> ", a pending transaction replaces the payment it comes from"))

    -- The file of a payment of 10000 from the change is written before
    -- the stuck payment is replaced, and its pending transaction deleted:
    -- the file is as one made on another computer
    --
    -- The answer was "A coin referenced by the transaction is locked",
    -- which named no coin and no transaction.
    it "is refused to a transaction imported from a file, naming the coin, the replacement and deletetx" $
      withChain paidChain $ \home chain -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        spender <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10", "--output", home </> "spender.psbt"] []
        _ <- answer home ["deletetx", spender, "--yes"] []
        replacement <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (_, coin) <- changeOn chain
        let shortCoin = Text.pack (shortOf coin <> drop 64 coin)
            short = Text.pack (shortOf replacement)
        (errorOf <$> hwJson home ["importtx", home </> "spender.psbt"] [])
          `shouldReturn` Right ("The transaction spends the coin " <> shortCoin <> ", and the pending transaction " <> short <> " replaces the payment this coin comes from: the coin goes once the replacement is sent. hw deletetx " <> short <> " deletes that transaction and frees the coin")

    it "is not locked by lockcoin, which names the replacement, and deletetx, which frees the coin" $
      withChain paidChain $ \home chain -> do
        (_, nosig) <- replacedPending home
        (_, coin) <- changeOn chain
        (errorOf <$> hwJson home ["lockcoin", coin] [])
          `shouldReturn` Right (Text.pack ("The pending transaction " <> shortOf nosig <> " replaces the payment the coin comes from: deletetx " <> shortOf nosig <> " frees it"))

    -- The user locked the change before the payment was replaced. The
    -- replacement holds it too, and shows first.
    it "that the user locked before is unlocked by unlockcoin, and stays held by the replacement" $
      withChain paidChain $ \home chain -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        (change, coin) <- changeOn chain
        _ <- answer home ["lockcoin", coin] []
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        (coinsShown <$> hwJson home ["unlockcoin", coin] []) `shouldReturn` Right [(change, 27740, 0, Just LockedByReplacement, False)]

    -- The lock is on the money, and the replacement leaves its change at
    -- the same address: 50000 - 20000 - 4520 = 25480
    it "that the user locked before leaves its lock to the change of the replacement, once that is sent and synced" $
      withChain paidChain $ \home chain -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        (_, coin) <- changeOn chain
        _ <- answer home ["lockcoin", coin] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- signAndSend home nosig
        _ <- answer home ["sync"] []
        (newChange, _) <- changeOn chain
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(newChange, 25480, 0, Just LockedByUser, False)]

    it "can be spent again once the replacement is deleted" $
      withChain paidChain $ \home chain -> do
        (_, nosig) <- replacedPending home
        (change, _) <- changeOn chain
        _ <- answer home ["deletetx", nosig, "--yes"] []
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(change, 27740, 0, Nothing, True)]

    -- A block holds the stuck payment: nothing replaces it any more, and
    -- its change is a coin like any other, though the replacement is
    -- still among the pending transactions
    it "can be spent once the stuck payment is in a block, while the replacement that came too late is still pending" $
      withChain paidChain $ \home chain -> do
        _ <- replacedPending home
        (change, _) <- changeOn chain
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(change, 27740, 1, Nothing, True)]

    -- The wallet learns that the change is gone at the next sync only
    it "stays locked once the replacement is sent, until a sync finds the change gone" $
      withChain paidChain $ \home chain -> do
        (_, nosig) <- replacedPending home
        (change, _) <- changeOn chain
        _ <- signAndSend home nosig
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(change, 27740, 0, Just LockedByReplacement, False)]

  -- The payment spends the coin of 50000 at /0/0 and leaves 27740 of
  -- change. A send-all then spends the other coin, of 30000 at /0/1: one
  -- input and one output, 192 bytes, pay 1920 at 10 satoshis a byte, and
  -- the recipient gets 30000 - 1920 = 28080.
  describe "The change of a stuck payment whose replacement is pending, beside another coin" $
    it "is left by a send-all, whose answer says that a pending transaction replaces its payment" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        _ <- answer home ["sync"] []
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        sendAll <- answer home ["preparesendall", textOf (otherAddr 1), "--fee-rate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 1, 28080)]
        (map (\(_, value, why) -> (value, why)) <$> leftOf sendAll) `shouldBe` Right [(27740, Locked LockedByReplacement)]

  -- portfolio0 pays 20000 to the first address of portfolio1. The
  -- replacement of that payment is pending in portfolio0, and removes
  -- what portfolio1 received once it is sent.
  describe "What a stuck payment paid another portfolio of the wallet" $
    it "is locked in that portfolio while the replacement is pending" $
      withChain paidChain $ \home _ -> do
        syncedTwoPortfolios home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo account1Addr "0.0002", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        _ <- answer home ["sync"] []
        _ <- answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        switchTo home 0 1
        (fmap (map (\(_, value, confirmations, lock, spendable) -> (value, confirmations, lock, spendable))) . coinsShown <$> hwJson home ["coins"] [])
          `shouldReturn` Right [(20000, 0, Just LockedByReplacement, False)]

  -- portfolio0 pays 20000 to the first address of portfolio1, and another
  -- wallet of the seed spends that output from portfolio1 before anything
  -- confirms
  describe "A stuck payment an output of which another portfolio spends" $
    it "is not replaced, since that spend would be removed too" $
      withChain paidChain $ \home chain -> do
        syncedTwoPortfolios home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo account1Addr "0.0002", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        [sent] <- inMempool <$> readIORef chain
        index <- maybe (fail "The payment does not pay portfolio1") return $ findIndex ((== Right account1Addr) . scriptToAddressBS btc . (.script)) sent.outputs
        let spender = spendingTx [(txHash sent, fromIntegral index)] [(otherAddr 2, 15000)]
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
      withChain paidChain $ \home _ -> do
        stuck <- withCostlyChild home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] []
        runErr run `shouldNotSatisfy` Text.isInfixOf "Replace both"
        run `shouldSatisfy` printed "The replacement pays 5200 satoshis of fee, and must pay 13820: the fees of the transactions it replaces, and its own 260 virtual bytes. Give a --fee-rate of at least 54"

    it "is replaced with the payment at a rate that pays for both" $
      withChain paidChain $ \home _ -> do
        stuck <- withCostlyChild home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "54"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 5960)], 14040)

  describe "A stuck payment replaced from an online computer" $
    it "has its replacement exported, signed offline, imported back and sent, which retires it" $
      withHome $ \signer -> withChain paidChain $ \online _ -> do
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
      withChain (chainPaying account1Addr) $ \home _ -> do
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
      withChain (chainPaying account1Addr) $ \home _ -> do
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
      withChain (chainPaying account1Addr) $ \home chain -> do
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
      withChain (confirmedChain bothTypesTx) $ \home _ -> do
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
      withChain (confirmedChain bothTypesTx) $ \home _ -> do
        syncedBothTypes home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--coin-type", "bip44", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60"] ["y"]
        spentFromWallet bumped `shouldBe` Map.fromList [(externalAddr 0, 50000), (segwitExternalAddr 0, 30000)]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 40000)], Map.fromList [(internalAddr 0, 22300)], 17700)

  -- The payment of 40000 from the coin of 50000 at /0/0, which has the tag
  -- kyc, sends its change of 7740 to the new change address /1/0, tagged
  -- kyc with it by --tag-change. At 20 satoshis a byte the change pays the
  -- higher fee. At 60 it cannot, and bumpfee adds the coin of 30000 at
  -- /0/1. bumpfee keeps the change address, and changes no tag.
  describe "The change of a stuck payment made with the coins of a tag" $ do
    let chain = confirmedChain twoAddressesTx
        -- The payment sent, once the addresses given are tagged kyc
        stuckWithTagged home tagged = do
          synced home
          forM_ tagged $ \addr -> answer home ["tag", textOf addr, "kyc"] []
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--spend-from", textOf (externalAddr 0), "--tag-change", "--fee-rate", "10"] []
          signAndSend home nosig
    it "keeps its tag in the replacement that takes the higher fee from it" $
      withChain chain $ \home _ -> do
        stuck <- stuckWithTagged home [externalAddr 0]
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        addressTagsIn bumped (internalAddr 0) `shouldBe` ["kyc"]

    it "keeps its tag when the coin bumpfee adds has the tag too" $
      withChain chain $ \home _ -> do
        stuck <- stuckWithTagged home [externalAddr 0, externalAddr 1]
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60"] ["y"]
        addressTagsIn bumped (internalAddr 0) `shouldBe` ["kyc"]

    -- A yes to adding the coin, and one to spending it with the coin
    -- tagged kyc
    it "keeps its tag when the coin bumpfee adds does not have it: bumpfee takes no tag off" $
      withChain chain $ \home _ -> do
        stuck <- stuckWithTagged home [externalAddr 0]
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60"] ["y", "y"]
        addressTagsIn bumped (internalAddr 0) `shouldBe` ["kyc"]

    -- The coin of 30000 at /0/1 has no tag, and the coin of 50000 the
    -- payment spends has kyc
    it "is asked about after the coin to add, when that coin does not have the tags of the coins of the payment" $
      withChain chain $ \home _ -> do
        stuck <- stuckWithTagged home [externalAddr 0]
        run <- hw home ["bumpfee", stuck, "--fee-rate", "60"] ["y", "n"]
        dropWhile (/= "Add it? [y/N] ") (Text.lines (runErr run))
          `shouldBe` [ "Add it? [y/N] ",
                       "The transaction spends coins that do not all have the same tags:",
                       "  [kyc]   1 coin  0.00050000 btc",
                       "  no tag  1 coin  0.00030000 btc",
                       "A transaction shows that its coins have one owner, so it joins these groups.",
                       "Replace the transaction anyway? [y/N] "
                     ]

    it "is not replaced once the user answers no to the coins of different tags" $
      withChain chain $ \home _ -> do
        stuck <- stuckWithTagged home [externalAddr 0]
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "60"] ["y", "n"])
          `shouldReturn` Right "The transaction was not replaced"

    -- Both coins have kyc: the one question is whether to add the coin
    it "is not asked about when the coin bumpfee adds has the tags of the coins of the payment" $
      withChain chain $ \home _ -> do
        stuck <- stuckWithTagged home [externalAddr 0, externalAddr 1]
        run <- hw home ["bumpfee", stuck, "--fee-rate", "60"] ["y"]
        runErr run `shouldNotSatisfy` Text.isInfixOf "do not all have the same tags"

    it "is replaced without a question on the coins of different tags, with --yes" $
      withChain chain $ \home _ -> do
        stuck <- stuckWithTagged home [externalAddr 0]
        run <- hw home ["bumpfee", stuck, "--fee-rate", "60", "--yes"] []
        (runExit run, Text.isInfixOf "do not all have the same tags" (runErr run)) `shouldBe` (ExitSuccess, False)

  describe "A payment bumpfee does not replace" $ do
    it "is a payment the portfolio receives: only its payer can replace it" $
      withChain (Chain 800002 [(payment, Nothing)]) $ \home _ -> do
        synced home
        let received = Text.unpack (txHashToHex (txHash payment))
        (errorOf <$> hwJson home ["bumpfee", received, "--fee-rate", "20"] [])
          `shouldReturn` Right "Only a payment from the coins of the portfolio can be replaced: this transaction spends coins of another wallet"

    it "is one of Bitcoin Cash, which replaces none" $
      withChain (confirmedChain cashCoinTx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        let received = Text.unpack (txHashToHex (txHash cashCoinTx))
        (errorOf <$> hwJson home ["bumpfee", received, "--fee-rate", "20"] [])
          `shouldReturn` Right "Bitcoin Cash replaces no transaction: its nodes keep the first one they see"

  -- The payment of 40000 from the coin of 50000 at /0/0 leaves 7740 of
  -- change. At 60 satoshis a byte, the coin alone cannot pay even the 11520
  -- of a transaction without change. With the coin of 30000 at /0/1, the
  -- replacement of 374 bytes pays 22440 and leaves 80000 - 40000 - 22440 =
  -- 17560 of change.
  describe "A stuck payment whose change cannot pay the higher fee" $ do
    let chain = confirmedChain twoAddressesTx
        stuckPayment home = do
          synced home
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
          signAndSend home nosig
    it "takes a confirmed coin of the portfolio, as the user agrees" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 40000)], Map.fromList [(internalAddr 0, 17560)], 22440)

    -- The coin is 3 confirmations deep, its transaction seen at
    -- 1700000000, 23:13 in Central Europe
    it "shows the coin it asks to add as coins shows it, its short id first" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        run <- hwWithEnv [("TZ", centralEurope)] home ["bumpfee", stuck, "--fee-rate", "60"] ["y"]
        run `shouldSatisfy` printedSentence "The coins of the transaction cannot pay its fee at 60 sat/vB, and this coin of the portfolio would:"
        runErr run
          `shouldSatisfy` Text.isInfixOf
            ( "\n" <> Text.pack (shortCoinArg twoAddressesTx 1) <> "  14 Nov 2023 23:13      0.00030000 btc  spendable, 3 confirmations\n"
                <> "  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  bip44/0/1\n"
            )
        runErr run `shouldSatisfy` Text.isInfixOf "Add it? [y/N]"

    -- Without the coin, the replacement of 192 bytes pays 11520 and has no
    -- change: the recipient pays the 40000 + 11520 - 50000 = 1520 missing
    it "takes the higher fee from what the recipient receives when the user declines the coin, as the user agrees" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60"] ["n", "y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 38480)], Map.empty, 11520)

    it "offers the recipient once the coin is declined" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "60"] ["n", "n"]
        runErr run `shouldSatisfy` Text.isInfixOf "Add it? [y/N]"
        run `shouldSatisfy` printedSentence "The coins of the transaction cannot pay its fee at 60 sat/vB, and no coin of the portfolio is added. The recipient can pay the 0.00001520 btc missing out of what it receives:"
        runErr run `shouldSatisfy` Text.isInfixOf ("\n  " <> Text.pack (textOf (otherAddr 0)) <> "  0.00040000 btc\n    0.00038480 btc paying it\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Reduce what it receives? [y/N]"

    -- A tag locks the coin of 30000 at /0/1 after the payment is sent:
    -- bumpfee cannot add it, and says that the portfolio holds it. The
    -- change of the payment itself is not counted: the replacement spends
    -- the coin it came from.
    it "says what the portfolio holds and cannot spend now, when a tag locks the coin that would pay the fee" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        _ <- answer home ["tag", textOf (externalAddr 1), "savings"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        run <- hw home ["bumpfee", stuck, "--fee-rate", "60"] ["n"]
        run `shouldSatisfy` printedSentence "The coins of the transaction cannot pay its fee at 60 sat/vB, and no coin of the portfolio is added: it holds 0.00030000 btc more, in 1 coin it cannot spend now, and hw coins says why. The recipient can pay the 0.00001520 btc missing out of what it receives:"

    it "is not replaced when the user wants neither the coin added nor the recipient paying" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "60"] ["n", "n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "takes a confirmed coin of the portfolio without asking with --yes" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 40000)], Map.fromList [(internalAddr 0, 17560)], 22440)

    -- The coin of 30000 would pay, but no coin is added. With its change,
    -- the payment is 226 bytes: 13560 at 60 satoshis a byte, 11300 more
    -- than the 2260 it paid. The recipient gets 40000 - 11300 = 28700, and
    -- the change keeps its 7740. Nothing is typed: a question would cancel
    -- the command.
    it "takes what the higher fee adds from the recipient with --recipients-pay, without adding a coin or asking" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60", "--recipients-pay"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 28700)], Map.fromList [(internalAddr 0, 7740)], 13560)

    -- --yes answers yes to adding coins, and --recipients-pay says that no
    -- coin is added: the recipient pays all the same
    it "adds no coin with --recipients-pay, even with --yes" $
      withChain chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "60", "--recipients-pay", "--yes"] []
        spentFromWallet bumped `shouldBe` Map.fromList [(externalAddr 0, 50000)]

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
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 73200)], Map.empty, 6800)

    it "names the recipient and what it would receive when it asks" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        run `shouldSatisfy` printedSentence "The coins of the transaction cannot pay its fee at 20 sat/vB, and no coin of the portfolio is added. The recipient can pay the 0.00003400 btc missing out of what it receives:"
        runErr run `shouldSatisfy` Text.isInfixOf ("\n  " <> Text.pack (textOf (otherAddr 0)) <> "  0.00076600 btc\n    0.00073200 btc paying it\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Reduce what it receives? [y/N]"

    it "is not replaced when the user does not want the recipient to pay" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] ["n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "takes the higher fee from what the recipient receives without asking with --yes" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 73200)], Map.empty, 6800)

    -- Nothing is typed: a question would cancel the command
    it "takes the higher fee from what the recipient receives without asking with --recipients-pay" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--recipients-pay"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 73200)], Map.empty, 6800)

    -- At 250 satoshis a byte, the 340 bytes pay 85000, more than the 80000
    -- of the coins: 76600 + 85000 - 80000 = 81600 are missing. Nothing is
    -- typed: the rate above 100 is not even asked about.
    it "is refused when the recipient cannot pay either, before any question" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
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
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] [Text.pack (textOf (otherAddr 1))]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 46260), (otherAddr 1, 26260)], Map.empty, 7480)

    it "is replaced by one that takes the higher fee from all the recipients in equal shares, when the user says all" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["all"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 44390), (otherAddr 1, 28130)], Map.empty, 7480)

    it "lists the recipients with what each would receive, and asks which pays" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] ["no"]
        run `shouldSatisfy` printedSentence "The 0.00003740 btc missing can come out of what the recipients receive, one of them paying it alone, or all of them in equal shares:"
        -- Numbered in the order of the outputs, which the test does not pin
        runErr run `shouldSatisfy` Text.isInfixOf ("  " <> Text.pack (textOf (otherAddr 0)) <> "  0.00046260 btc\n     0.00042520 btc paying it alone, 0.00044390 btc paying its share\n")
        runErr run `shouldSatisfy` Text.isInfixOf ("  " <> Text.pack (textOf (otherAddr 1)) <> "  0.00030000 btc\n     0.00026260 btc paying it alone, 0.00028130 btc paying its share\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Who pays it? Type 1, 2 or all, or no [no]"

    it "is not replaced when nobody pays, which an empty answer says" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] [""]) `shouldReturn` Right "The transaction was not replaced"

    -- No coin is left to add. Each pays 3740 / 2 = 1870: 46260 - 1870 =
    -- 44390 and 30000 - 1870 = 28130. Nothing is typed: a question would
    -- cancel the command.
    it "takes the higher fee from all the recipients in equal shares with --yes, without asking" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 44390), (otherAddr 1, 28130)], Map.empty, 7480)

    -- The send-all pays 1000 to the second address and 80000 - 1000 - 3740
    -- = 75260 to the first. At 20 satoshis a byte, 3740 are missing: 1870
    -- each is more than the 1000 of the second, and the first alone can
    -- pay. Nothing is typed, so the question cancels the command.
    it "still asks which recipient pays alone with --yes, when equal shares would leave one below the dust limit" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesendall", textOf (otherAddr 0), fixedTo (otherAddr 1) "0.00001", "--fee-rate", "10"] []
        stuck <- signAndSend home nosig
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] [])
          `shouldReturn` Right "Cancelled: the input ended before the answer"

    -- Each pays 3740 / 2 = 1870: 46260 - 1870 = 44390 and 30000 - 1870 =
    -- 28130. Nothing is typed: a question would cancel the command.
    it "takes the higher fee from all the recipients in equal shares with --recipients-pay, without asking which" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--recipients-pay"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 44390), (otherAddr 1, 28130)], Map.empty, 7480)

    it "tells what each recipient receives paying its share with --recipients-pay" $
      withChain (confirmedChain twoCoinsTx) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20", "--recipients-pay"] []
        run `shouldSatisfy` printedSentence "As --recipients-pay asks, the recipients pay the 0.00003740 btc the higher fee adds at 20 sat/vB, in equal shares, out of what they receive:"
        -- In the order of the outputs, which the test does not pin
        runErr run `shouldSatisfy` Text.isInfixOf ("  " <> Text.pack (textOf (otherAddr 0)) <> "  0.00046260 btc\n    0.00044390 btc paying its share\n")
        runErr run `shouldSatisfy` Text.isInfixOf ("  " <> Text.pack (textOf (otherAddr 1)) <> "  0.00030000 btc\n    0.00028130 btc paying its share\n")

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
      withChain paidChain $ \home _ -> do
        (stuck, second) <- withSecond home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 14800)], 5200)
        replacesOf bumped `shouldBe` [stuck, second]

    -- The stuck payment took 20000 paid + 2260 of fee = 22260, and the
    -- second one 10000 paid + 2260 of fee = 12260. The second spent the
    -- change of the first, and fell with it.
    it "names the one transaction that replaced both in the history, as that payment does" $
      withChain paidChain $ \home _ -> do
        (stuck, _) <- withSecond home
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] []
        replacement <- signAndSend home nosig
        _ <- answer home ["sync"] []
        run <- hw home ["txs"] []
        runOut run
          `shouldSatisfy` Text.isInfixOf ("-0.00022260 btc  dead\n  replaced by " <> Text.pack (shortOf replacement) <> ", to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\n")
        runOut run
          `shouldSatisfy` Text.isInfixOf ("-0.00012260 btc  dead\n  replaced by " <> Text.pack (shortOf replacement) <> ", to 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ\n")

    it "names that payment by its short id when it asks" $
      withChain paidChain $ \home _ -> do
        (stuck, second) <- withSecond home
        run <- hw home ["bumpfee", stuck, "--fee-rate", "20"] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf ("Replacing the transaction removes those of the portfolio that spend its change:\n  " <> Text.pack (shortOf second) <> "\n")
        runErr run `shouldSatisfy` Text.isInfixOf "Replace both with one transaction that pays the recipients of both? [y/N]"

    it "is not replaced when the user does not want that payment replaced too" $
      withChain paidChain $ \home _ -> do
        (stuck, _) <- withSecond home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--fee-rate", "20"] ["n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "is replaced with that payment without asking with --yes" $
      withChain paidChain $ \home _ -> do
        (stuck, second) <- withSecond home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 14800)], 5200)
        replacesOf bumped `shouldBe` [stuck, second]
