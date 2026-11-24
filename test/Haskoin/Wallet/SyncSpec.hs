{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The synchronisation of a pool, what a portfolio holds on one network,
-- played against a scripted store: payments arriving, confirming, being
-- replaced or dropped, a payment the wallet made, the chain reorganising,
-- a wallet with many coins or many addresses, the raw transactions
-- fetched only when missing and dropped when no coin needs them, pending
-- transactions, two portfolios, another network; the two accounts of a
-- pool synced together; the pools of several networks, some of whose
-- stores are down or not served; the gap of addresses watched past the
-- ones the accounts have; the scan of a chain and the recovery of the
-- portfolios of a seed; and the decisions of a sync on their own: what to
-- fetch, what is dead, which balances changed
module Haskoin.Wallet.SyncSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, replicateM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Data.Default (def)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef, newIORef, readIORef)
import Data.List (find, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Word (Word32, Word64, Word8)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.RawTx (hasRawTx, insertRawTx, lookupRawTx)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.SyncStatus
import Haskoin.Wallet.Keys (AccountKind (..), ForeignKind (..), legacyPath, portfolioNetworks)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo (ChainStatus (..), ListedTx (..), MyInputs (..), MyOutputs (..), TxInfo (..), TxSummary (..), TxType (..), listedTxsAfter, markTxInfoDead, repsertSummary, summaryKinds, txInfoHash, txInfoKey, txsAfter)
import Haskoin.Wallet.Database.TxLabel (setTxLabel, txLabel)
import Haskoin.Wallet.Testnet3 (btcTestnet3)
import Haskoin.Wallet.Tx.SignData
import Numeric.Natural (Natural)
import Test.Hspec

-- | The question of a scan answered yes the given number of times, then
-- no, each time noted
answering :: (MonadIO m) => IORef [String] -> Int -> Watcher m
answering asked yeses = answeringWith $ \found -> liftIO $ do
  modifyIORef asked (<> [found])
  (<= yeses) . length <$> readIORef asked

spec :: Spec
spec = do
  let cfg = def :: Config
      -- A store that reports every address used, as one that lies does
      everyAddressUsed = (toStore emptyStore) {storeBalances = \addrs -> return [Store.Balance addr 1000 0 1 1 1000 | addr <- addrs]}
      -- Portfolio 0 of the test wallet on Bitcoin, its pool there, with the
      -- first receiving address of its bip44 account, externalAddr 0
      account = do
        pool <- testPool
        _ <- nextAddress pool
        return pool
      -- The same for portfolio 0 of the second test wallet, whose first
      -- receiving address is externalAddr2
      secondAccount = do
        pool <- (`portfolioPool` btc) <$> portfolioOf testMnemonic2
        _ <- nextAddress pool
        return pool
      -- The next receiving address of the bip44 account of a pool
      nextAddress pool = do
        accId <- bip44Id pool
        genExtAddress cfg accId ""
      syncWith store pool = sync cfg (toStore store) pool Incremental noQuestion

  describe "Syncing a portfolio on a network" $ do
    it "stores a received payment, its coin and the best block" $
      inDatabaseE $ do
        pool <- account
        res <- syncWith paymentStore pool
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        getBest btc `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800002 1).header) 800002 (Just 1700000000))
        balances pool `dbShouldBeE` (50000, 0, 1)
        rawTransactionCount `dbShouldBeE` 1
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)

    it "stores the best block of the network again over one that does not read, on any sync" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith paymentStore pool
        lift $ P.update (DBBestKey "btc") [DBBestBestBlock P.=. "nonsense"]
        _ <- syncWith paymentStore pool
        getBest btc `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800002 1).header) 800002 (Just 1700000000))

    it "does nothing when nothing changed" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith paymentStore pool
        res <- syncWith paymentStore pool
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (0, 0)
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]

    it "stores the time the store gives each transaction" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith paymentStore pool
        (map (chainTime . chainOf) <$> txsAfter pool Nothing Nothing) `dbShouldBeE` [Just seenTime]

    it "gives a coin the time the store gives the transaction that paid it" $
      inDatabaseE $ do
        pool <- account
        -- The store saw the payment 600 seconds after seenTime
        let paidLater = paymentStore {fakeTransactions = [seenAt 1700000600 (creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)]}
        _ <- syncWith paidLater pool
        accIds <- accountIds pool
        -- 22:13:20 UTC + 600 seconds = 22:23:20 UTC
        (map jsonCoinTime <$> coinsAfter pool accIds AllCoins Nothing Nothing)
          `dbShouldBeE` [Just (UTCTime (fromGregorian 2023 11 14) (22 * 3600 + 23 * 60 + 20))]

    it "reports what the portfolio can spend on the network once synced" $
      inDatabaseE $ do
        pool <- account
        res <- syncWith realPaymentStore pool
        liftIO $ syncResSpendable res `shouldBe` 50000

    it "updates a payment when it confirms" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolStore pool
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.MemRef 1700000000, 50000)]
        balances pool `dbShouldBeE` (0, 50000, 1)
        res <- syncWith paymentStore pool
        liftIO $ syncResTxUpdates res `shouldBe` 1
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        balances pool `dbShouldBeE` (50000, 0, 1)

    it "finds an old payment to an address generated after newer transactions were synced" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoPaymentsStore pool
        -- externalAddr 1 is generated now, but was paid long ago by someone
        -- deriving it from the public key
        _ <- nextAddress pool
        _ <- syncWith oldPaymentStore pool
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000),
                        (Just (fakeTxHash 3), Store.BlockRef 800002 1, 20000),
                        (Just (fakeTxHash 4), Store.BlockRef 700000 1, 10000)
                      ]
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0, OutPoint (fakeTxHash 4) 0]

    it "rebuilds a known payment when a new address of ours turns out to be in it" $
      inDatabaseE $ do
        pool <- account
        -- Transaction 5 paid externalAddr 0 and externalAddr 1, but only the
        -- first exists yet: the second output looks like somebody else's
        _ <- syncWith payoutStore pool
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 5), Store.BlockRef 800000 1, 30000)]
        paidAddresses pool `dbShouldBeE` [["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 5) 0]
        _ <- nextAddress pool
        res <- syncWith payoutSeenStore pool
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 5), Store.BlockRef 800000 1, 50000)]
        paidAddresses pool `dbShouldBeE` [["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 5) 0, OutPoint (fakeTxHash 5) 1]
        balances pool `dbShouldBeE` (50000, 0, 2)

    it "marks a payment the store reports as replaced as dead, and keeps it" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolStore pool
        _ <- syncWith replacedStore pool
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.MemRef 1700000000, 50000)]
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 1)]
        coins pool `dbShouldBeE` []
        balances pool `dbShouldBeE` (0, 0, 0)

    it "marks a payment the store no longer knows as dead" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolStore pool
        _ <- syncWith forgottenStore pool
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 1)]
        coins pool `dbShouldBeE` []

    it "counts a payment it marks dead as a transaction updated" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolStore pool
        res <- syncWith forgottenStore pool
        -- The payment is dead, and its coin is gone
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)

    -- The balance of externalAddr 0 is the same before and after the
    -- replacement: 50000 unconfirmed, 1 coin, 1 transaction
    it "takes in a replacement that pays an address the amount the payment it replaces paid, and marks that payment dead" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolStore pool
        _ <- syncWith sameAmountReplacedStore pool
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.MemRef 1700000000, 50000),
                          (Just (fakeTxHash 2), Store.MemRef 1700000100, 50000)
                        ]
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 1)]

    it "puts the coin of a replacement that pays an address the same amount in the place of the coin of the payment it replaces" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolStore pool
        _ <- syncWith sameAmountReplacedStore pool
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 2) 0]

    it "counts that replacement and the payment it marks dead as two transactions updated, and the coins that went and came as two" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolStore pool
        res <- syncWith sameAmountReplacedStore pool
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (2, 2)

    -- The balance of externalAddr 0 is the same before and after the
    -- replacement: nothing left, no coin, 2 transactions, 50000 received
    it "takes in a replacement that spends the coin the spending it replaces spent, with no change, and marks that spending dead" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolSpentStore pool
        _ <- syncWith spendingReplacedStore pool
        -- Each spending: 49000 paid + 1000 of fee = 50000 out
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000),
                          (Just (fakeTxHash 3), Store.MemRef 1700000000, -50000),
                          (Just (fakeTxHash 4), Store.MemRef 1700000100, -50000)
                        ]
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 3)]

    it "brings a dead payment back when the store lists it again" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith mempoolStore pool
        _ <- syncWith replacedStore pool
        res <- syncWith paymentStore pool
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        deadSummaries pool `dbShouldBeE` []
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        balances pool `dbShouldBeE` (50000, 0, 1)

    it "moves a payment to its new block after a reorganisation that kept the balances" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith paymentStore pool
        res <- syncWith movedStore pool
        liftIO $ syncResTxUpdates res `shouldBe` 1
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800001 3, 50000)]
        getBest btc `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800003 7).header) 800003 (Just 1700000000))

    it "marks a payment a reorganisation left out as dead" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoPaymentsStore pool
        _ <- syncWith leftOutStore pool
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000),
                          (Just (fakeTxHash 3), Store.BlockRef 800002 1, 20000)
                        ]
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 3)]
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 2) 0]
        balances pool `dbShouldBeE` (30000, 0, 1)

    -- The best block of the store is 800002. The first sync has no block
    -- of its own, and asks for the block a hundred below, 799902. The
    -- next ones also ask for the block the sync before reached.
    it "asks the store about its blocks in one question, on an incremental sync and on a full one" $
      inDatabaseE $ do
        pool <- account
        asked <- liftIO $ newIORef []
        let store = notingBlockRequests asked $ toStore $ paymentStore
        _ <- sync cfg store pool Incremental noQuestion
        _ <- sync cfg store pool Full noQuestion
        _ <- sync cfg store pool Incremental noQuestion
        liftIO $ readIORef asked `shouldReturn` [[799902], [799902, 800002], [799902, 800002]]

    it "keeps every coin of an address with two hundred of them" $
      inDatabaseE $ do
        pool <- account
        res <- syncWith manyCoinsStore pool
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (200, 200)
        (length <$> coins pool) `dbShouldBeE` 200
        (length <$> summaries pool) `dbShouldBeE` 200
        balances pool `dbShouldBeE` (200000, 0, 200)

    it "lists the transactions of more addresses than it asks about at a time" $
      inDatabaseE $ do
        pool <- account
        -- externalAddr 1 to externalAddr 6 as well
        replicateM_ 6 $ nextAddress pool
        let twoAtATime = cfg {configParallelRequests = 2}
        res <- sync twoAtATime (toStore sevenAddressesStore) pool Incremental noQuestion
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (7, 7)
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 1000),
                          (Just (fakeTxHash 2), Store.BlockRef 800000 2, 1000),
                          (Just (fakeTxHash 3), Store.BlockRef 800000 3, 1000),
                          (Just (fakeTxHash 4), Store.BlockRef 800000 4, 1000),
                          (Just (fakeTxHash 5), Store.BlockRef 800000 5, 1000),
                          (Just (fakeTxHash 6), Store.BlockRef 800000 6, 1000),
                          (Just (fakeTxHash 7), Store.BlockRef 800000 7, 1000)
                        ]
        paidAddresses pool
          `dbShouldBeE` [ ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
                          ["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"],
                          ["1Dg6Kg7kQuyiZz41HRWXKUWKRu6ZyEf1Nr"],
                          ["1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"],
                          ["1cWcYiGK7NwjPBJuKRqZxV4aymUnPu1mx"],
                          ["1MZuimSXigp8oqxkVUvZofqHNtVjdcdAqc"],
                          ["1JReTkpFnsrMqhSEJwUNZXPAyeTo2HQfnE"]
                        ]
        balances pool `dbShouldBeE` (7000, 0, 7)

    it "fetches the raw transaction of a coin once, not again on a full sync" $
      inDatabaseE $ do
        pool <- account
        asked <- liftIO $ newIORef []
        let store = notingRawRequests asked $ toStore $ realPaymentStore
        _ <- sync cfg store pool Incremental noQuestion
        _ <- sync cfg store pool Full noQuestion
        liftIO $ readIORef asked `shouldReturn` [[realPaymentId]]
        rawTransactionCount `dbShouldBeE` 1

    it "fetches the raw transaction of a coin again when it is missing" $
      inDatabaseE $ do
        pool <- account
        asked <- liftIO $ newIORef []
        let store = notingRawRequests asked $ toStore $ realPaymentStore
        _ <- sync cfg store pool Incremental noQuestion
        lift $ P.deleteWhere ([] :: [P.Filter DBRawTx])
        _ <- sync cfg store pool Full noQuestion
        liftIO $ readIORef asked `shouldReturn` [[realPaymentId], [realPaymentId]]
        rawTransactionCount `dbShouldBeE` 1

    it "drops the raw transaction of a coin once the coin is spent" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        rawTransactionCount `dbShouldBeE` 1
        _ <- syncWith spentElsewhereStore pool
        coins pool `dbShouldBeE` []
        rawTransactionCount `dbShouldBeE` 0

    it "keeps the raw transaction of a spent coin while another coin comes from it" $
      inDatabaseE $ do
        pool <- account
        _ <- nextAddress pool
        _ <- syncWith realPayoutStore pool
        coins pool `dbShouldBeE` [OutPoint realPayoutId 0, OutPoint realPayoutId 1]
        rawTransactionCount `dbShouldBeE` 1
        _ <- syncWith realPayoutFirstSpentStore pool
        coins pool `dbShouldBeE` [OutPoint realPayoutId 1]
        rawTransactionCount `dbShouldBeE` 1
        _ <- syncWith realPayoutAllSpentStore pool
        coins pool `dbShouldBeE` []
        rawTransactionCount `dbShouldBeE` 0

    it "drops every raw transaction no coin comes from on a full sync" $
      inDatabaseE $ do
        pool <- account
        -- A raw transaction left behind by an older hw, which no coin comes from
        lift $ insertRawTx (spendingTx [(fakeTxHash 5, 0)] [(otherAddr 0, 1000)])
        _ <- syncWith realPaymentStore pool
        rawTransactionCount `dbShouldBeE` 2
        _ <- sync cfg (toStore realPaymentStore) pool Full noQuestion
        rawTransactionCount `dbShouldBeE` 1
        lift (hasRawTx realPaymentId) `dbShouldBeE` True

    it "refuses a store that does not return a raw transaction asked for" $
      inDatabaseE $ do
        pool <- account
        result <- lift $ runExceptT $ syncWith noRawStore pool
        liftIO $ result `shouldBe` Left "The store was asked for 1 raw transactions but returned 0"

    -- The raw transaction of the payment pays externalAddr 0 50000 at its
    -- output 0. Its txid, a16ab47f...080e, is the double SHA-256 of its
    -- bytes, worked out apart from hw.
    it "refuses a store that reports a coin worth less than its transaction pays, and stores none of its coins" $
      inDatabaseE $ do
        pool <- account
        let store = realPaymentStore {fakeUnspents = [coinOf realPaymentId (Just 800000) (externalAddr 0) 1100]}
        result <- lift $ runExceptT $ syncWith store pool
        liftIO $ result `shouldBe` Left "The store reports the coin a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e:0 as worth 1100 satoshis, but its transaction pays 50000"
        coins pool `dbShouldBeE` []

    it "refuses a store that reports a coin at another script than its transaction pays" $
      inDatabaseE $ do
        pool <- account
        let coin =
              Store.Unspent
                { Store.block = Store.BlockRef 800000 0,
                  Store.outpoint = OutPoint realPaymentId 0,
                  Store.value = 50000,
                  Store.script = addressScript (externalAddr 1),
                  Store.address = Just (externalAddr 0)
                }
            store = realPaymentStore {fakeUnspents = [coin]}
        result <- lift $ runExceptT $ syncWith store pool
        liftIO $ result `shouldBe` Left "The store reports the coin a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e:0 at another script than its transaction pays"

    it "refuses a store that reports a coin at an output its transaction does not have" $
      inDatabaseE $ do
        pool <- account
        let store = realPaymentStore {fakeUnspents = [coinAt realPaymentId 1 (Just 800000) (externalAddr 0) 50000]}
        result <- lift $ runExceptT $ syncWith store pool
        liftIO $ result `shouldBe` Left "The store reports the coin a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e:1, but its transaction has 1 outputs"

    -- The fee rate of a summary is its fee divided by its size
    it "refuses a store that reports a transaction of weight 0, rather than dividing by it" $
      inDatabaseE $ do
        pool <- account
        let weightless (Store.Transaction b v l i o d r t x z _ f) = Store.Transaction b v l i o d r t x z 0 f
            store = realPaymentStore {fakeTransactions = weightless <$> realPaymentStore.fakeTransactions}
        result <- lift $ runExceptT $ syncWith store pool
        liftIO $ result `shouldBe` Left "The store reports a weight of 0 for the transaction a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e"

    it "refuses a store that lists fewer coins than it counts" $
      inDatabaseE $ do
        pool <- account
        result <- lift $ runExceptT $ syncWith fewerCoinsStore pool
        liftIO $ result `shouldBe` Left "The store counts 2 coins for the addresses to update but listed 1"

    it "refuses a store that lists fewer transactions than it counts" $
      inDatabaseE $ do
        pool <- account
        result <- lift $ runExceptT $ syncWith fewerTxsStore pool
        liftIO $
          result
            `shouldBe` Left "The store counts 2 transactions for 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj but listed 1"

    -- The store answers about the address of another wallet besides the
    -- ones asked: it must not reach the rows of another portfolio, nor
    -- plant coins at an address of nobody's
    it "refuses a store that answers about an address it was not asked about, on an incremental sync and on a full one" $
      inDatabaseE $ do
        pool <- account
        let store = toStore paymentStore
            tooMany = store {storeBalances = \addrs -> (<> [Store.Balance (otherAddr 0) 1000 0 1 1 1000]) <$> storeBalances store addrs}
        forM_ [Incremental, Full] $ \mode ->
          lift $ dbShouldFailWith "The store answered about other addresses than the ones asked" $ sync cfg tooMany pool mode noQuestion

    it "refuses a store that returns a transaction it was not asked for" $
      inDatabaseE $ do
        pool <- account
        let store = toStore paymentStore
            tooMany = store {storeTransactions = \tids -> (<> [creditTx (fakeTxHash 9) (Store.BlockRef 800000 9) (otherAddr 0) 1000]) <$> storeTransactions store tids}
        lift $
          dbShouldFailWith "The store returned a transaction it was not asked for: 0000000000000000000000000000000000000000000000000000000000000009" $
            sync cfg tooMany pool Incremental noQuestion

    it "retires a pending transaction seen online" $
      inDatabaseE $ do
        pool <- account
        lift $ P.insert_ $ pendingRow pool pendingTx
        pendingHashes pool `dbShouldBeE` [txHash pendingTx]
        res <- syncWith sentStore pool
        pendingHashes pool `dbShouldBeE` []
        liftIO $ syncResRebroadcast res `shouldBe` 0

    it "gives the label of a pending transaction it retires to that transaction in the history" $
      inDatabaseE $ do
        pool <- account
        lift $ P.insert_ $ pendingRow pool pendingTx
        held <- requirePendingTx (txHash pendingTx)
        lift $ setPendingTxLabel held "Back to myself"
        _ <- syncWith sentStore pool
        (map txInfoLabel <$> txsAfter pool Nothing Nothing) `dbShouldBeE` ["Back to myself"]

    it "sends again a transaction the user sent that the store lost" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        -- The user sent the payment to otherAddr 0 but the store does not
        -- list it: the coin is still unspent
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        published <- liftIO $ newIORef []
        res <- sync cfg (takingPublished published $ toStore $ realPaymentStore) pool Incremental noQuestion
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (1, 0)
        liftIO $ readIORef published `shouldReturn` [txSignDataTx signedSpend]
        pendingHashes pool `dbShouldBeE` [txHash (txSignDataTx signedSpend)]
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]

    -- The store took the payment and does not list it afterwards: its
    -- node refused the payment and gave no reason, as a node of Bitcoin
    -- does to a peer. The payment is not on the network.
    it "does not count as sent again a transaction the store took and does not list" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        res <- sync cfg (toStore realPaymentStore) pool Incremental noQuestion
        liftIO $ syncResRebroadcast res `shouldBe` 0

    it "tells that a transaction the store took and does not list was not sent again, and why it may be so" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        res <- sync cfg (toStore realPaymentStore) pool Incremental noQuestion
        liftIO $
          syncResNotPublished res
            `shouldBe` [ ( txHash (txSignDataTx signedSpend),
                           "The transaction was handed to the network but the store has not seen it after 10 seconds. A node says nothing of a transaction it refuses, so its fee may be too low for the network"
                         )
                       ]

    it "keeps pending and sent a transaction the store took and does not list" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        _ <- sync cfg (toStore realPaymentStore) pool Incremental noQuestion
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]

    -- The store says it did not publish the payment. That is the fate of
    -- one transaction: the sync keeps what it read, and the payment stays
    -- pending and sent, for the user to delete.
    it "goes on when the store says it did not publish a transaction sent again, and tells what the store said" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        let refusal = "The store at https://api.haskoin.com/ refused the transaction: The fee is too low"
        res <- sync cfg (refusingToPublish refusal $ toStore $ realPaymentStore) pool Incremental noQuestion
        liftIO $ syncResNotPublished res `shouldBe` [(txHash (txSignDataTx signedSpend), "The store at https://api.haskoin.com/ refused the transaction: The fee is too low")]
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        -- The payment received is still in the history, with its coin
        coins pool `dbShouldBeE` [OutPoint realPaymentId 0]

    it "does not count as sent again a transaction the store says it did not publish" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        res <- sync cfg (refusingToPublish "The fee is too low" $ toStore $ realPaymentStore) pool Incremental noQuestion
        liftIO $ syncResRebroadcast res `shouldBe` 0

    -- A store that gives no answer has failed, as on any other question
    it "fails when the store gives no answer about a transaction sent again" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $
          dbShouldFailWith "The store at https://api.haskoin.com/ could not be reached: the connection failed" $
            sync cfg (failingToPublish "The store at https://api.haskoin.com/ could not be reached: the connection failed" $ toStore $ realPaymentStore) pool Incremental noQuestion

    it "leaves alone a signed transaction the user has not sent" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        _ <- importPendingTx pool signedSpend
        published <- liftIO $ newIORef []
        res <- sync cfg (notingPublished published $ toStore $ realPaymentStore) pool Incremental noQuestion
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 0)
        liftIO $ readIORef published `shouldReturn` []

    it "does not send again a sent transaction in conflict, which the network would refuse" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        published <- liftIO $ newIORef []
        res <- sync cfg (notingPublished published $ toStore $ spentElsewhereStore) pool Incremental noQuestion
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 1)
        liftIO $ readIORef published `shouldReturn` []

    it "reports a pending transaction whose coin was spent by another transaction" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool unsignedSpend
        coins pool `dbShouldBeE` [OutPoint realPaymentId 0]
        res <- syncWith spentElsewhereStore pool
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 1)
        coins pool `dbShouldBeE` []
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, True))]
        -- Nothing is left to free but the change address, which it had none of
        deleteTx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (0, 0)
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing) `dbShouldBeE` []

    -- A reorganisation replaced the payment received, and the store has
    -- nothing of it any more. The wallet had it, and saw it go.
    it "reports a pending transaction whose coin went with its transaction in a reorganisation" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool unsignedSpend
        res <- syncWith realPaymentGoneStore pool
        liftIO $ syncResConflicted res `shouldBe` 1
        coins pool `dbShouldBeE` []
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, True))]

    it "does not send again a sent transaction whose coin went with its transaction in a reorganisation" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        published <- liftIO $ newIORef []
        res <- sync cfg (notingPublished published $ toStore realPaymentGoneStore) pool Incremental noQuestion
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 1)
        liftIO $ readIORef published `shouldReturn` []

    -- The first chain comes back, with the payment in it: the coin is
    -- there again, and the pending transaction can spend it
    it "takes the conflict away when the transaction its coin comes from is back" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool unsignedSpend
        _ <- syncWith realPaymentGoneStore pool
        res <- syncWith realPaymentStore pool
        liftIO $ syncResConflicted res `shouldBe` 0
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]

    it "retires a sent transaction the network confirmed under another id, its signatures re-encoded by a miner" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        published <- liftIO $ newIORef []
        res <- sync cfg (notingPublished published $ toStore $ malleatedStore) pool Incremental noQuestion
        -- Neither in conflict nor sent again: the payment is on the chain
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 0)
        liftIO $ readIORef published `shouldReturn` []
        pendingTxsAfter pool Nothing Nothing `dbShouldBeE` []
        -- The payment shows under the id the network gave it
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 9), Store.BlockRef 800001 1, -50000),
                          (Just realPaymentId, Store.BlockRef 800000 1, 50000)
                        ]

    it "gives the label of a transaction the network has under another id to that id" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool signedSpend {txSignDataLabel = "Rent"}
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        _ <- syncWith malleatedStore pool
        lift (txLabel pool (fakeTxHash 9)) `dbShouldBeE` "Rent"

    it "retires an unsigned pending transaction the network has, signed and sent elsewhere" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        _ <- importPendingTx pool unsignedSpend
        res <- syncWith malleatedStore pool
        liftIO $ syncResConflicted res `shouldBe` 0
        pendingTxsAfter pool Nothing Nothing `dbShouldBeE` []

    it "asks the store about the parents of the pending transactions, and about a rival only when there is one" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        _ <- importPendingTx pool unsignedSpend
        held <- pendingTxs pool
        free <- liftIO $ newIORef []
        _ <- recordStanding (notingTxRequests free $ toStore $ realPaymentStore) held
        liftIO $ readIORef free `shouldReturn` [[realPaymentId]]
        rivalled <- liftIO $ newIORef []
        _ <- recordStanding (notingTxRequests rivalled $ toStore $ spentElsewhereStore) held
        liftIO $ readIORef rivalled `shouldReturn` [[realPaymentId], [fakeTxHash 8]]

    it "clears the conflict once the transaction that spent the coin leaves the chain" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        pendingHash <- importPendingTx pool unsignedSpend
        _ <- syncWith spentElsewhereStore pool
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, True))]
        -- A reorganisation undoes transaction 8: the store lists the coin
        -- unspent again, and the full sync that follows finds it free
        res <- sync cfg (toStore realPaymentStore) pool Full noQuestion
        liftIO $ syncResConflicted res `shouldBe` 0
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]
        coins pool `dbShouldBeE` [OutPoint realPaymentId 0]

    it "reports a sweep of another portfolio's coin once the coin is spent elsewhere" $
      inDatabaseE $ do
        pool <- account
        otherPool <- secondAccount
        _ <- syncWith realPaymentStore pool
        sweepHash <- importPendingTx otherPool sweepToOther
        -- The sync of the other portfolio is enough: the store is asked
        -- about the coin, whatever the portfolio it belongs to
        res <- syncWith spentElsewhereStore otherPool
        liftIO $ syncResConflicted res `shouldBe` 1
        ((pendingFlags <$>) <$> pendingTxsAfter otherPool Nothing Nothing)
          `dbShouldBeE` [Just ((sweepHash, False, False, True))]

    it "reports a sweep of a coin outside the wallet once that coin is spent elsewhere" $
      inDatabaseE $ do
        pool <- account
        sweepHash <- importPendingTx pool outsideSweep
        unspent <- syncWith outsideStore pool
        liftIO $ syncResConflicted unspent `shouldBe` 0
        spent <- syncWith outsideSpentStore pool
        liftIO $ syncResConflicted spent `shouldBe` 1
        ((pendingFlags <$>) <$> pendingTxsAfter pool Nothing Nothing)
          `dbShouldBeE` [Just ((sweepHash, False, False, True))]

    it "follows a payment the wallet made: the coin goes, the change comes, the summary is a debit" $
      inDatabaseE $ do
        pool <- account
        -- The change address, /1/0, and the coin to spend
        _ <- changeAddress cfg pool Bip44
        _ <- syncWith realPaymentStore pool
        -- The user prepared, signed and sent the payment, as broadcast leaves
        -- it: pending, online, its raw transaction stored
        lift $ P.insert_ $ pendingRow pool changeSpendTx
        lift (setPendingTxOnline (nosigHash changeSpendTx)) `dbShouldBeE` 1
        lift $ insertRawTx changeSpendTx
        -- The store lists it, confirmed
        res <- syncWith changeSpentStore pool
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 2)
        -- A debit of 41000: 40000 paid and 1000 of fee, 9000 of change to /1/0
        txInfos <- txsAfter pool Nothing Nothing
        spend <- liftMaybe "The payment has no summary" $ find ((== Just (txHash changeSpendTx)) . txInfoHash) txInfos
        let summary = txInfoSummary spend
        liftIO $ do
          summaryType summary `shouldBe` TxDebit
          summaryAmount summary `shouldBe` -41000
          summaryFee summary `shouldBe` 1000
          Map.toList (summaryOtherOutputs summary) `shouldBe` [(otherAddr 0, 40000)]
          Map.toList (Map.map myOutputsPath (summaryMyOutputs summary)) `shouldBe` [(internalAddr 0, legacyPath (Deriv :/ 1 :/ 0))]
          Map.keys (summaryMyInputs summary) `shouldBe` [externalAddr 0]
          pendingFlags spend `shouldBe` Nothing
        coins pool `dbShouldBeE` [OutPoint (txHash changeSpendTx) 1]
        balances pool `dbShouldBeE` (9000, 0, 1)
        pendingHashes pool `dbShouldBeE` []
        -- The parent of the spent coin is gone; the payment stays, for its change
        rawTransactionCount `dbShouldBeE` 1
        lift (hasRawTx (txHash changeSpendTx)) `dbShouldBeE` True

    it "syncs each portfolio on its own" $
      inDatabaseE $ do
        pool <- account
        pool2 <- (`portfolioPool` btc) <$> portfolioOf testMnemonic2
        addr2 <- nextAddress pool2
        liftIO $ dBAddressAddress addr2 `shouldBe` externalAddr2Text
        _ <- syncWith twoWalletsStore pool
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        summaries pool2 `dbShouldBeE` []
        balances pool2 `dbShouldBeE` (0, 0, 0)
        _ <- syncWith twoWalletsStore pool2
        summaries pool2 `dbShouldBeE` [(Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000)]
        coins pool2 `dbShouldBeE` [OutPoint (fakeTxHash 2) 0]
        balances pool `dbShouldBeE` (50000, 0, 1)
        balances pool2 `dbShouldBeE` (30000, 0, 1)

    it "syncs a portfolio on another network, with the best block of that network" $
      inDatabaseE $ do
        (accId, acc) <- testNewAccBch
        liftIO $ dBAccountXPubKey acc `shouldBe` testAccountXPubBch
        let pool = accountPool acc
        addr <- nextAddress pool
        liftIO $ dBAddressAddress addr `shouldBe` bchExternalAddrText
        res <- sync cfg (toStore cashPaymentStore) pool Incremental noQuestion
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        (map jsonCoinOutpoint <$> coinsAfter pool [accId] AllCoins Nothing Nothing) `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        balances pool `dbShouldBeE` (50000, 0, 1)
        getBest bch `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800002 1).header) 800002 (Just 1700000000))
        getBest btc `dbShouldBeE` Nothing

  describe "The balances of txs when the history changes in the middle" $ do
    -- The payment of 10000 lands between the 30000 and the 20000:
    -- 30000 + 10000 = 40000, and 40000 + 20000 = 60000
    it "takes in an old payment found late, and changes only the balances above it" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoPaymentsStore pool
        listedBalances pool `dbShouldBeE` [(Just (fakeTxHash 3), Just 50000), (Just (fakeTxHash 2), Just 30000)]
        _ <- nextAddress pool
        _ <- syncWith latePaymentStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 3), Just 60000),
                          (Just (fakeTxHash 4), Just 40000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- 90000 was there with the middle payment of 40000, 50000 without it
    it "drops a payment in the middle a reorganisation left out, and changes only the balances above it" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith threePaymentsStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 3), Just 90000),
                          (Just (fakeTxHash 6), Just 70000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        _ <- syncWith middleLeftOutStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 6), Nothing),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- 50000 confirmed + 15000 unconfirmed = 65000 before, and
    -- 50000 + 5000 = 55000 once the 10000 is replaced
    it "drops an unconfirmed payment replaced in the middle of the mempool, on an incremental sync" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoUnconfirmedStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 7), Just 65000),
                          (Just (fakeTxHash 6), Just 60000),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        _ <- syncWith oneReplacedStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 7), Just 55000),
                          (Just (fakeTxHash 6), Nothing),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- The wallet saw the payment, then its replacement, while the chain
    -- was at block 799999. Block 800000 then confirmed the payment after
    -- all, and the replacement died.
    it "lists a payment a later block confirmed after all above its replacement that died" $
      inDatabaseE $ do
        pool <- account
        let at799999 store = store {fakeBest = blockAt 799999 1, fakeBlocks = [blockAt 799999 1]}
        _ <- syncWith (at799999 mempoolStore) pool
        _ <- syncWith (at799999 sameAmountReplacedStore) pool
        _ <- syncWith (replacementLostStore {fakeBlocks = [blockAt 799999 1, blockAt 800002 1]}) pool
        listedBalances pool `dbShouldBeE` [(Just (fakeTxHash 1), Just 50000), (Just (fakeTxHash 2), Nothing)]

    -- The block puts the spending of the 20000, transaction 8, at
    -- position 1, before the payment it spends, transaction 3, at position
    -- 5, as a block of Bitcoin Cash may. 30000 + 20000 = 50000 after the
    -- payment, and 50000 - 20000 = 30000 after its spending.
    it "lists a spending above the payment it spends in its block, though the block puts it first" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith spentBeforePaidStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 8), Just 30000),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- One chain serves both portfolios, so its reorganisation concerns both
    it "marks a payment a reorganisation left out as dead in every portfolio hw sync goes through" $
      inDatabaseE $ do
        mainPool <- account
        secondPool <- secondAccount
        _ <- syncPools cfg (const (toStore twoWalletsStore)) [mainPool, secondPool] Incremental noQuestion
        _ <- syncPools cfg (const (toStore secondLeftOutStore)) [mainPool, secondPool] Incremental noQuestion
        listedBalances secondPool `dbShouldBeE` [(Just (fakeTxHash 2), Nothing)]

    it "marks a payment a reorganisation left out as dead in a portfolio synced after another one" $
      inDatabaseE $ do
        mainPool <- account
        secondPool <- secondAccount
        _ <- syncWith twoWalletsStore mainPool
        _ <- syncWith twoWalletsStore secondPool
        _ <- syncWith secondLeftOutStore mainPool
        _ <- syncWith secondLeftOutStore secondPool
        listedBalances secondPool `dbShouldBeE` [(Just (fakeTxHash 2), Nothing)]

    it "marks a payment a reorganisation left out as dead in that portfolio on a full sync, whatever came before" $
      inDatabaseE $ do
        mainPool <- account
        secondPool <- secondAccount
        _ <- syncWith twoWalletsStore mainPool
        _ <- syncWith twoWalletsStore secondPool
        _ <- syncWith secondLeftOutStore mainPool
        _ <- syncWith secondLeftOutStore secondPool
        _ <- sync cfg (toStore secondLeftOutStore) secondPool Full noQuestion
        listedBalances secondPool `dbShouldBeE` [(Just (fakeTxHash 2), Nothing)]

  describe "The block the last sync of a portfolio on a network reached" $ do
    let twoAccounts = do
          mainPool <- account
          secondPool <- secondAccount
          return (mainPool, secondPool)
        syncedBlockKey pool = DBSyncedBlockKey pool.poolPortfolio (poolNetworkName pool)

    it "is stored for the portfolio that synced, and for no other" $
      inDatabaseE $ do
        (mainPool, secondPool) <- twoAccounts
        _ <- syncWith twoWalletsStore mainPool
        syncedBlock mainPool `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800002 1).header) 800002)
        syncedBlock secondPool `dbShouldBeE` Nothing

    it "is what the incremental sync of each portfolio asks the store about" $
      inDatabaseE $ do
        (mainPool, secondPool) <- twoAccounts
        let laterChain = twoWalletsStore {fakeBest = blockAt 800003 7, fakeBlocks = [blockAt 800002 1, blockAt 800003 7]}
        _ <- syncWith twoWalletsStore mainPool
        _ <- syncWith laterChain secondPool
        asked <- liftIO $ newIORef []
        let store = notingBlockRequests asked (toStore laterChain)
        _ <- sync cfg store mainPool Incremental noQuestion
        _ <- sync cfg store secondPool Incremental noQuestion
        -- Each asks about the block it reached and the one a hundred
        -- below, and about the block a hundred below the best one, 799903
        liftIO $ readIORef asked `shouldReturn` [[799902, 799903, 800002], [799903, 800003]]

    -- No reorganisation tells of it: only looking at every address does
    it "makes a sync look at every address when the portfolio has no block it last synced to" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoPaymentsStore pool
        lift $ P.delete (syncedBlockKey pool)
        _ <- syncWith (leftOutStore {fakeBest = blockAt 800002 1, fakeBlocks = [blockAt 800002 1]}) pool
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 3)]

    it "refuses an incremental sync when it does not read, and a full sync stores it again" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith paymentStore pool
        lift $ P.update (syncedBlockKey pool) [DBSyncedBlockBlock P.=. "nonsense"]
        lift $
          dbShouldFailWith "The block the last sync of the portfolio reached does not read: sync --full stores it again" $
            syncWith paymentStore pool
        _ <- sync cfg (toStore paymentStore) pool Full noQuestion
        syncedBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800002 1).header) 800002)

  describe "The settled block of a portfolio on a network, a hundred blocks below the one its last sync reached" $ do
    let outOfMain (Store.BlockData h _ w hd z wt t o f s) = Store.BlockData h False w hd z wt t o f s
        syncedBlockKey pool = DBSyncedBlockKey pool.poolPortfolio (poolNetworkName pool)
        -- The store of the last sync was at block 800002: the wallet keeps
        -- block 799902 of it
        syncedTo800002 = do
          pool <- account
          _ <- syncWith paymentStore pool
          return pool
        -- A store at the same height, with another block at height 799902
        otherChain = emptyStore {fakeBlocks = [blockAt 799902 5, blockAt 800002 1]}
        -- The refusal names the block the wallet keeps, by the hash of
        -- 'deepBlock'
        otherChainRefused =
          "The store at https://api.haskoin.com/ is on another chain of btc than the wallet synced with: it does not have the block "
            <> "ef0f4a242609c480d56f34b0547417e5a81f50d7204e9a2629b34fec5793de53 at height 799902, which is too deep for a reorganization to replace. "
            <> "hw did not sync btc with it. If the chain of this store is the right one, as after a test network was reset, follow it with:\n  hw sync --network btc --new-chain"

    -- 800002 - 100 = 799902
    it "is stored by the sync, with the block it reached" $
      inDatabaseE $ do
        pool <- syncedTo800002
        settledBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (deepBlock 799902).header) 799902)

    -- 800003 - 100 = 799903
    it "moves up with the chain at the next sync" $
      inDatabaseE $ do
        pool <- syncedTo800002
        _ <- syncWith (paymentStore {fakeBest = blockAt 800003 7, fakeBlocks = [blockAt 800002 1, blockAt 800003 7]}) pool
        settledBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (deepBlock 799903).header) 799903)

    it "is the first block of a chain that is not a hundred blocks long" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith (emptyStore {fakeBest = blockAt 40 1, fakeBlocks = [blockAt 40 1]}) pool
        settledBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (deepBlock 0).header) 0)

    -- The store is at block 799901, one block short of the settled block
    it "refuses a store whose chain ends below it, and tells how to follow that store" $
      inDatabaseE $ do
        pool <- syncedTo800002
        lift $
          dbShouldFailWith
            ( "The store at https://api.haskoin.com/ has reached block 799901 of btc only, and the wallet synced as far as block 800002. "
                <> "A store that is new or still reads the chain is that far behind, and so is another server than the one of the last sync. "
                <> "hw did not sync btc with it. If the chain of this store is the right one, as after a test network was reset, follow it with:\n  hw sync --network btc --new-chain"
            )
            $ syncWith (emptyStore {fakeBest = blockAt 799901 1, fakeBlocks = [blockAt 799901 1]}) pool

    -- The store lists no payment: a sync that followed it would mark the
    -- payment of the wallet dead
    it "leaves the wallet as it was when it refuses a store" $
      inDatabaseE $ do
        pool <- syncedTo800002
        lift $ dbShouldFail $ syncWith (emptyStore {fakeBest = blockAt 799901 1, fakeBlocks = [blockAt 799901 1]}) pool
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        deadSummaries pool `dbShouldBeE` []
        syncedBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800002 1).header) 800002)

    -- The store is at block 799950, 52 blocks behind the wallet: it has
    -- block 799902, and the sync takes it as it takes a reorganisation
    it "follows a store that is behind the wallet and still has it" $
      inDatabaseE $ do
        pool <- syncedTo800002
        _ <- syncWith (paymentStore {fakeBest = blockAt 799950 1, fakeBlocks = [blockAt 799950 1]}) pool
        syncedBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 799950 1).header) 799950)

    it "refuses a store that has another block at its height" $
      inDatabaseE $ do
        pool <- syncedTo800002
        lift $ dbShouldFailWith otherChainRefused $ syncWith otherChain pool

    it "refuses a store that has it out of its main chain" $
      inDatabaseE $ do
        pool <- syncedTo800002
        lift $
          dbShouldFailWith otherChainRefused $
            syncWith (emptyStore {fakeBlocks = [outOfMain (deepBlock 799902), blockAt 799902 5, blockAt 800002 1]}) pool

    it "refuses such a store on a full sync too" $
      inDatabaseE $ do
        pool <- syncedTo800002
        lift $ dbShouldFailWith otherChainRefused $ sync cfg (toStore otherChain) pool Full noQuestion

    -- The payment of the wallet is not on the new chain
    it "follows such a store on a sync told to take a new chain, which keeps the block of that chain" $
      inDatabaseE $ do
        pool <- syncedTo800002
        _ <- sync cfg (toStore otherChain) pool NewChain noQuestion
        settledBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 799902 5).header) 799902)
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 1)]

    it "takes the chain of any store on the first sync of a portfolio" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith otherChain pool
        settledBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 799902 5).header) 799902)

    -- The store has no block of its main chain at height 799902, below
    -- its best block 800002
    it "refuses a store that has no block of its chain a hundred below its best block" $
      inDatabaseE $ do
        pool <- account
        lift $
          dbShouldFailWith "The store at https://api.haskoin.com/ has no block of btc at height 799902, which is below its best block" $
            syncWith (emptyStore {fakeBlocks = [outOfMain (deepBlock 799902), blockAt 800002 1]}) pool

    it "refuses an incremental sync when it does not read, and a full sync stores it again" $
      inDatabaseE $ do
        pool <- syncedTo800002
        lift $ P.update (syncedBlockKey pool) [DBSyncedBlockSettledBlock P.=. "nonsense"]
        lift $
          dbShouldFailWith "The block the last sync of the portfolio reached does not read: sync --full stores it again" $
            syncWith paymentStore pool
        _ <- sync cfg (toStore paymentStore) pool Full noQuestion
        settledBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (deepBlock 799902).header) 799902)

  describe "A reorganisation of the chain" $ do
    -- After a reorganisation the store still knows the block the last sync
    -- reached, at its height, out of the main chain. The balances stay as
    -- they were, and transaction 3 is listed no more.
    it "looks at every address when the block the last sync reached is at its height but out of the main chain" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoPaymentsStore pool
        let outOfMain (Store.BlockData h _ w hd z wt t o f s) = Store.BlockData h False w hd z wt t o f s
        _ <- syncWith (leftOutStore {fakeBlocks = [outOfMain (blockAt 800002 1), blockAt 800002 9, blockAt 800003 7]}) pool
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 3)]

    it "moves a payment to its new block in a portfolio synced after another one" $
      inDatabaseE $ do
        mainPool <- account
        secondPool <- secondAccount
        _ <- syncWith twoWalletsStore mainPool
        _ <- syncWith twoWalletsStore secondPool
        _ <- syncWith secondMovedStore mainPool
        _ <- syncWith secondMovedStore secondPool
        summaries secondPool `dbShouldBeE` [(Just (fakeTxHash 2), Store.BlockRef 800003 3, 30000)]

    -- The 40000 goes to the top of the list, and the rows it left go down:
    -- 30000 + 20000 = 50000, and 50000 + 40000 = 90000
    it "puts a payment that goes back to the mempool at the top of the list, with the balances that follow" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith threePaymentsStore pool
        _ <- syncWith backInMempoolStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 6), Just 90000),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        balances pool `dbShouldBeE` (50000, 40000, 3)

    it "drops a payment and the spending of its coin when it leaves both out" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith spentPaymentStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 8), Just 30000),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        _ <- syncWith leftOutStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 8), Nothing),
                          (Just (fakeTxHash 3), Nothing),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        balances pool `dbShouldBeE` (30000, 0, 1)

    it "gives back the coin of a spending it leaves out" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith spentPaymentStore pool
        _ <- syncWith middleLeftOutStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 8), Nothing),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]
        balances pool `dbShouldBeE` (50000, 0, 2)

    -- Both sit at the same place in the chain, so they go by their ids.
    -- 30000 + 15000 = 45000.
    it "takes in the rival it puts in the place of a payment" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoPaymentsStore pool
        _ <- syncWith rivalStore pool
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 3), Nothing),
                          (Just (fakeTxHash 9), Just 45000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        balances pool `dbShouldBeE` (45000, 0, 2)

    it "follows a chain that got shorter than the block the portfolio reached" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoPaymentsStore pool
        _ <- syncWith (leftOutStore {fakeBest = blockAt 800001 9, fakeBlocks = [blockAt 800001 9]}) pool
        deadSummaries pool `dbShouldBeE` [Just (fakeTxHash 3)]
        syncedBlock pool `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800001 9).header) 800001)
        getBest btc `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800001 9).header) 800001 (Just 1700000000))

    it "fetches only the transactions that moved: it cannot change what a transaction holds" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith twoPaymentsStore pool
        asked <- liftIO $ newIORef []
        _ <- sync cfg (notingTxRequests asked (toStore oneMovedStore)) pool Incremental noQuestion
        liftIO $ (concat <$> readIORef asked) `shouldReturn` [fakeTxHash 3]

  describe "A full sync, over a database gone wrong" $ do
    let -- The pool with the two payments, and the store that has them
        syncedAccount = do
          pool <- account
          _ <- syncWith twoPaymentsStore pool
          return pool
        fullSync pool = sync cfg (toStore twoPaymentsStore) pool Full noQuestion
        summaryKey pool n = txInfoKey pool (fakeTxHash n)
        -- The row of the coin of a payment in an account
        coinKey accId n = DBCoinKey accId (outpointBytes (OutPoint (fakeTxHash n) 0))
        rightSummaries =
          [ (Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000),
            (Just (fakeTxHash 3), Store.BlockRef 800002 1, 20000)
          ]
        rightBalances = [(Just (fakeTxHash 3), Just 50000), (Just (fakeTxHash 2), Just 30000)]

    it "fetches every transaction the store lists, though no block moved" $
      inDatabaseE $ do
        pool <- syncedAccount
        asked <- liftIO $ newIORef []
        _ <- sync cfg (notingTxRequests asked (toStore twoPaymentsStore)) pool Full noQuestion
        liftIO $ (sort . concat <$> readIORef asked) `shouldReturn` [fakeTxHash 2, fakeTxHash 3]

    it "reports no update over a database that was right" $
      inDatabaseE $ do
        pool <- syncedAccount
        res <- fullSync pool
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (0, 0)

    -- The spending stands after the payment it spends, not at its own
    -- position in the block: writing it again must not count as a change
    it "reports no update over a database that was right, though a block puts a spending before its payment" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith spentBeforePaidStore pool
        res <- sync cfg (toStore spentBeforePaidStore) pool Full noQuestion
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (0, 0)
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 8), Just 30000),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    it "writes the amount of a summary again, which --after adds up" $
      inDatabaseE $ do
        pool <- syncedAccount
        lift $ P.update (summaryKey pool 3) [DBTxInfoAmount P.=. 1]
        _ <- fullSync pool
        (fmap dBTxInfoAmount <$> lift (P.get (summaryKey pool 3))) `dbShouldBeE` Just 20000

    it "writes again a summary that reads but says the wrong thing" $
      inDatabaseE $ do
        pool <- syncedAccount
        _ <- lift $ repsertSummary pool (fakeTxHash 3) (Store.BlockRef 800002 1) fixtureTime paymentSummary
        _ <- fullSync pool
        summaries pool `dbShouldBeE` rightSummaries

    it "writes again a summary that does not read" $
      inDatabaseE $ do
        pool <- syncedAccount
        lift $ P.update (summaryKey pool 3) [DBTxInfoBlob P.=. "nonsense"]
        _ <- fullSync pool
        summaries pool `dbShouldBeE` rightSummaries

    it "writes again a block reference that does not read, which would stop any other sync" $
      inDatabaseE $ do
        pool <- syncedAccount
        lift $ P.update (summaryKey pool 3) [DBTxInfoBlockRef P.=. "nonsense"]
        _ <- fullSync pool
        summaries pool `dbShouldBeE` rightSummaries

    it "brings back a summary that went missing, and the balances below it are right again" $
      inDatabaseE $ do
        pool <- syncedAccount
        lift $ P.delete (summaryKey pool 3)
        -- With the 20000 gone from the list, the older row takes all that
        -- the portfolio holds: 20000 too much
        listedBalances pool `dbShouldBeE` [(Just (fakeTxHash 2), Just 50000)]
        _ <- fullSync pool
        listedBalances pool `dbShouldBeE` rightBalances

    it "brings back to life a transaction the store lists" $
      inDatabaseE $ do
        pool <- syncedAccount
        lift $ markTxInfoDead pool 800002 (fakeTxHash 3)
        _ <- fullSync pool
        listedBalances pool `dbShouldBeE` rightBalances

    it "marks dead a transaction no address lists, and keeps it for the history" $
      inDatabaseE $ do
        pool <- syncedAccount
        _ <- lift $ repsertSummary pool (fakeTxHash 9) (Store.BlockRef 800001 5) fixtureTime paymentSummary
        _ <- fullSync pool
        -- It keeps the place of its block: position 5 of block 800001,
        -- above the payment at position 1
        listedBalances pool
          `dbShouldBeE` [ (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 9), Nothing),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- txs refuses a row that does not read, and nothing can write this one
    it "deletes a dead summary that does not read, which would stop txs for good" $
      inDatabaseE $ do
        pool <- syncedAccount
        _ <- lift $ repsertSummary pool (fakeTxHash 9) (Store.BlockRef 800001 5) fixtureTime paymentSummary
        lift $ markTxInfoDead pool 800002 (fakeTxHash 9)
        lift $ P.update (summaryKey pool 9) [DBTxInfoBlob P.=. "nonsense"]
        _ <- fullSync pool
        listedBalances pool `dbShouldBeE` rightBalances

    it "deletes a live summary that does not read and that no address lists" $
      inDatabaseE $ do
        pool <- syncedAccount
        _ <- lift $ repsertSummary pool (fakeTxHash 9) (Store.BlockRef 800001 5) fixtureTime paymentSummary
        lift $ P.update (summaryKey pool 9) [DBTxInfoBlockRef P.=. "nonsense"]
        _ <- fullSync pool
        listedBalances pool `dbShouldBeE` rightBalances

    it "writes the balances of the addresses and of the account again" $
      inDatabaseE $ do
        pool <- syncedAccount
        accId <- bip44Id pool
        lift $ P.update accId [DBAccountBalanceConfirmed P.=. 7, DBAccountBalanceUnconfirmed P.=. 8, DBAccountBalanceCoins P.=. 9]
        lift $ P.updateWhere [DBAddressAccount P.==. accId] [DBAddressBalanceConfirmed P.=. 7]
        _ <- fullSync pool
        balances pool `dbShouldBeE` (50000, 0, 2)

    it "brings back a coin that went missing" $
      inDatabaseE $ do
        pool <- syncedAccount
        accId <- bip44Id pool
        lift $ P.delete (coinKey accId 3)
        _ <- fullSync pool
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]

    it "deletes a coin the store does not list" $
      inDatabaseE $ do
        pool <- syncedAccount
        accId <- bip44Id pool
        lift $ P.insert_ $ DBCoin accId (outpointBytes (OutPoint (fakeTxHash 9) 0)) "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 1000 (S.encode (Store.BlockRef 800000 1)) seenTime False False fixtureTime
        _ <- fullSync pool
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]

    it "writes the value of a coin again" $
      inDatabaseE $ do
        pool <- syncedAccount
        accId <- bip44Id pool
        lift $ P.update (coinKey accId 3) [DBCoinValue P.=. 1]
        _ <- fullSync pool
        (fmap dBCoinValue <$> lift (P.get (coinKey accId 3))) `dbShouldBeE` Just 20000

    it "writes the block of a coin again" $
      inDatabaseE $ do
        pool <- syncedAccount
        accId <- bip44Id pool
        lift $ P.update (coinKey accId 3) [DBCoinBlockRef P.=. "nonsense"]
        _ <- fullSync pool
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]

    -- Whichever address comes first, its coin is stored under the other
    it "gives each coin its address again when two coins stand under each other's address" $
      inDatabaseE $ do
        pool <- account
        accId <- bip44Id pool
        _ <- nextAddress pool
        _ <- syncWith oldPaymentStore pool
        lift $ P.update (coinKey accId 3) [DBCoinAddress P.=. "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
        lift $ P.update (coinKey accId 4) [DBCoinAddress P.=. "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]
        _ <- sync cfg (toStore oldPaymentStore) pool Full noQuestion
        (fmap dBCoinAddress <$> lift (P.get (coinKey accId 3))) `dbShouldBeE` Just "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        (fmap dBCoinAddress <$> lift (P.get (coinKey accId 4))) `dbShouldBeE` Just "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"

    it "takes back a coin stored under another account" $
      inDatabaseE $ do
        pool <- syncedAccount
        accId <- bip44Id pool
        (otherId, _) <- testNewAcc2
        lift $ P.update (coinKey accId 3) [DBCoinAccount P.=. otherId]
        _ <- fullSync pool
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]

    it "fetches again a raw transaction that does not read" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        lift $ P.update (DBRawTxKey (hashBytes realPaymentId)) [DBRawTxBlob P.=. "nonsense"]
        _ <- sync cfg (toStore realPaymentStore) pool Full noQuestion
        lookupRawTx realPaymentId `dbShouldBeE` Just realPaymentTx

    it "fetches again a raw transaction that reads but holds another transaction" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith realPaymentStore pool
        lift $ P.update (DBRawTxKey (hashBytes realPaymentId)) [DBRawTxBlob P.=. S.encode (spendingTx [(fakeTxHash 5, 0)] [(otherAddr 0, 1000)])]
        _ <- sync cfg (toStore realPaymentStore) pool Full noQuestion
        lookupRawTx realPaymentId `dbShouldBeE` Just realPaymentTx

    it "keeps the labels and the coins the user locked: the store cannot tell them" $
      inDatabaseE $ do
        pool <- syncedAccount
        accId <- bip44Id pool
        lift $ setTxLabel pool (fakeTxHash 3) "Rent"
        _ <- lockCoin =<< walletCoin cfg Nothing (OutPoint (fakeTxHash 3) 0)
        lift $ P.update (coinKey accId 3) [DBCoinValue P.=. 1]
        _ <- fullSync pool
        lift (txLabel pool (fakeTxHash 3)) `dbShouldBeE` "Rent"
        (fmap dBCoinLockedByUser <$> lift (P.get (coinKey accId 3))) `dbShouldBeE` Just True

  describe "A full sync of a pool with a bip44 and a bip84 account" $ do
    -- The first receiving address of the bip44 account was paid 50000,
    -- and that of the bip84 account 30000. A first sync stores it all.
    let syncedBoth = do
          pool <- account
          _ <- syncWith bothAccountsStore pool
          return pool
        fullSync pool = sync cfg (toStore bothAccountsStore) pool Full noQuestion
        -- What the server can tell again, gone from the database
        wipe pool = do
          accIds <- accountIds pool
          lift $ do
            P.deleteWhere [DBTxInfoPortfolio P.==. pool.poolPortfolio, DBTxInfoNetwork P.==. poolNetworkName pool]
            P.deleteWhere [DBCoinAccount P.<-. accIds]
            P.updateWhere
              [DBAddressAccount P.<-. accIds]
              [DBAddressBalanceConfirmed P.=. 0, DBAddressBalanceUnconfirmed P.=. 0, DBAddressBalanceCoins P.=. 0, DBAddressBalanceTxs P.=. 0, DBAddressBalanceReceived P.=. 0]
            P.updateWhere [DBAccountId P.<-. accIds] [DBAccountBalanceConfirmed P.=. 0, DBAccountBalanceUnconfirmed P.=. 0, DBAccountBalanceCoins P.=. 0]
        bothSummaries =
          [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000),
            (Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000)
          ]
        -- The two accounts of the pool: bip44, then bip84
        bothIds pool = (,) <$> bip44Id pool <*> (fst <$> poolAccountOf pool Bip84)

    it "brings back the summaries of the pool after they were wiped, as the first sync stored them" $
      inDatabaseE $ do
        pool <- syncedBoth
        summaries pool `dbShouldBeE` bothSummaries
        wipe pool
        summaries pool `dbShouldBeE` []
        _ <- fullSync pool
        summaries pool `dbShouldBeE` bothSummaries

    it "brings back the coins of both accounts after they were wiped, each in its account" $
      inDatabaseE $ do
        pool <- syncedBoth
        (legacyId, segwitId) <- bothIds pool
        wipe pool
        coins pool `dbShouldBeE` []
        _ <- fullSync pool
        coinsOfAccount legacyId `dbShouldBeE` [(OutPoint (fakeTxHash 1) 0, 50000)]
        coinsOfAccount segwitId `dbShouldBeE` [(OutPoint (fakeTxHash 2) 0, 30000)]

    it "brings back the balances of the addresses of both accounts after they were wiped" $
      inDatabaseE $ do
        pool <- syncedBoth
        (legacyId, segwitId) <- bothIds pool
        wipe pool
        _ <- fullSync pool
        addressBalances legacyId `dbShouldBeE` [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", 50000, 0, 1)]
        addressBalances segwitId `dbShouldBeE` [("bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", 30000, 0, 1)]

    -- 50000 + 30000 = 80000 in 2 coins
    it "brings back what each account holds after it was wiped, and reports what the pool holds" $
      inDatabaseE $ do
        pool <- syncedBoth
        (legacyId, segwitId) <- bothIds pool
        wipe pool
        balances pool `dbShouldBeE` (0, 0, 0)
        res <- fullSync pool
        balancesOfAccount legacyId `dbShouldBeE` (50000, 0, 1)
        balancesOfAccount segwitId `dbShouldBeE` (30000, 0, 1)
        liftIO $ (syncResConfirmed res, syncResUnconfirmed res, syncResCoins res) `shouldBe` (80000, 0, 2)

    -- The summary of the payment to the bip84 account says 1, its coin is
    -- worth 1, and the bip84 account claims 7
    it "writes over summaries, coins and balances that say the wrong thing, in both accounts" $
      inDatabaseE $ do
        pool <- syncedBoth
        (legacyId, segwitId) <- bothIds pool
        lift $ P.update (txInfoKey pool (fakeTxHash 2)) [DBTxInfoAmount P.=. 1]
        lift $ P.update (DBCoinKey segwitId (outpointBytes (OutPoint (fakeTxHash 2) 0))) [DBCoinValue P.=. 1]
        lift $ P.update segwitId [DBAccountBalanceConfirmed P.=. 7]
        _ <- fullSync pool
        listedBalances pool `dbShouldBeE` [(Just (fakeTxHash 2), Just 80000), (Just (fakeTxHash 1), Just 50000)]
        coinsOfAccount legacyId `dbShouldBeE` [(OutPoint (fakeTxHash 1) 0, 50000)]
        coinsOfAccount segwitId `dbShouldBeE` [(OutPoint (fakeTxHash 2) 0, 30000)]
        balancesOfAccount segwitId `dbShouldBeE` (30000, 0, 1)

    -- The server knows nothing of the labels: they are the user's
    it "keeps the labels the user gave to an address of each account and to a transaction, though all the rest was wiped" $
      inDatabaseE $ do
        pool <- syncedBoth
        (legacyId, segwitId) <- bothIds pool
        _ <- setAddrLabel legacyId 0 "Salary"
        _ <- setAddrLabel segwitId 0 "Tips"
        lift $ setTxLabel pool (fakeTxHash 2) "First tip"
        wipe pool
        _ <- fullSync pool
        (map dBAddressLabel <$> lift (labelledAddresses legacyId)) `dbShouldBeE` ["Salary"]
        (map dBAddressLabel <$> lift (labelledAddresses segwitId)) `dbShouldBeE` ["Tips"]
        lift (txLabel pool (fakeTxHash 2)) `dbShouldBeE` "First tip"
        (map txInfoLabel <$> txsAfter pool Nothing Nothing) `dbShouldBeE` ["First tip", ""]

    it "generates no address again, and none anew: the addresses of both accounts are as they were" $
      inDatabaseE $ do
        pool <- syncedBoth
        wipe pool
        res <- fullSync pool
        liftIO $ syncResNewAddrs res `shouldBe` 0
        addressCounts pool Bip44 `dbShouldBeE` (1, 0)
        addressCounts pool Bip84 `dbShouldBeE` (1, 0)

  describe "The two accounts of a pool, synced together" $ do
    -- The portfolio has generated no address. With a gap of 3, the scan
    -- finds the third receiving address of the bip44 account and the
    -- second of the bip84 account, which the chain paid.
    let smallGap = cfg {configGap = 3}
        discoverBoth = do
          pool <- testPool
          res <- sync smallGap (toStore (paidStore [(externalAddr 2, 50000), (segwitExternalAddr 1, 30000)])) pool Incremental noQuestion
          return (pool, res)

    -- 3 addresses of the bip44 account and 2 of the bip84 one: 5
    it "finds the used addresses past those of each account, and generates up to them in both" $
      inDatabaseE $ do
        (pool, res) <- discoverBoth
        liftIO $ syncResNewAddrs res `shouldBe` 5
        addressCounts pool Bip44 `dbShouldBeE` (3, 0)
        addressCounts pool Bip84 `dbShouldBeE` (2, 0)

    -- 50000 + 30000 = 80000, in 2 coins
    it "counts what both accounts hold" $
      inDatabaseE $ do
        (pool, res) <- discoverBoth
        liftIO $ (syncResConfirmed res, syncResUnconfirmed res, syncResCoins res) `shouldBe` (80000, 0, 2)
        balances pool `dbShouldBeE` (80000, 0, 2)

    it "gives each account the balance and the coins of its own addresses" $
      inDatabaseE $ do
        (pool, _) <- discoverBoth
        legacyId <- bip44Id pool
        (segwitId, _) <- poolAccountOf pool Bip84
        balancesOfAccount legacyId `dbShouldBeE` (50000, 0, 1)
        balancesOfAccount segwitId `dbShouldBeE` (30000, 0, 1)
        coinsOfAccount legacyId `dbShouldBeE` [(OutPoint (fakeTxHash 1) 0, 50000)]
        coinsOfAccount segwitId `dbShouldBeE` [(OutPoint (fakeTxHash 2) 0, 30000)]

    it "stores the transactions of both accounts in the one history of the pool" $
      inDatabaseE $ do
        (pool, _) <- discoverBoth
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000),
                          (Just (fakeTxHash 2), Store.BlockRef 800000 2, 30000)
                        ]

    it "gives an address of the portfolio in a transaction the path of its own account" $
      inDatabaseE $ do
        (pool, _) <- discoverBoth
        toLegacy <- summaryOf pool (fakeTxHash 1)
        toSegwit <- summaryOf pool (fakeTxHash 2)
        liftIO $ do
          Map.toList (Map.map myOutputsPath (summaryMyOutputs toLegacy)) `shouldBe` [(externalAddr 2, legacyPath (Deriv :/ 0 :/ 2))]
          Map.toList (Map.map myOutputsPath (summaryMyOutputs toSegwit)) `shouldBe` [(segwitExternalAddr 1, segwitPath (Deriv :/ 0 :/ 1))]

    -- The 50000 of the bip44 address go to the bip84 address, less a fee
    -- of 1000: nothing leaves the portfolio but the fee
    it "stores a payment from a bip44 address to a bip84 address of the portfolio as one internal transaction, which costs its fee" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith movedToSegwitStore pool
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000),
                          (Just (fakeTxHash 2), Store.BlockRef 800001 1, -1000)
                        ]
        moved <- summaryOf pool (fakeTxHash 2)
        liftIO $ (summaryType moved, summaryAmount moved, summaryFee moved) `shouldBe` (TxInternal, -1000, 1000)

    it "names both types of the portfolio's addresses in that internal transaction, the coin spent and the address paid" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith movedToSegwitStore pool
        moved <- summaryOf pool (fakeTxHash 2)
        liftIO $ do
          summaryKinds moved `shouldBe` [Bip44, Bip84]
          Map.toList (Map.map myInputsPath (summaryMyInputs moved)) `shouldBe` [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0))]
          Map.toList (Map.map myOutputsPath (summaryMyOutputs moved)) `shouldBe` [(segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0))]
          Map.toList (summaryOtherOutputs moved) `shouldBe` []

    -- 50000 - 1000 of fee = 49000, now in the bip84 account
    it "leaves the pool with what it held less the fee, after that internal transaction" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith movedToSegwitStore pool
        legacyId <- bip44Id pool
        (segwitId, _) <- poolAccountOf pool Bip84
        balancesOfAccount legacyId `dbShouldBeE` (0, 0, 0)
        balancesOfAccount segwitId `dbShouldBeE` (49000, 0, 1)
        listedBalances pool `dbShouldBeE` [(Just (fakeTxHash 2), Just 49000), (Just (fakeTxHash 1), Just 50000)]

    -- A coin of 50000 of the bip44 account and one of 30000 of the bip84
    -- account pay 70000 to another wallet, with 9000 of change and a fee
    -- of 1000: 70000 + 1000 = 71000 leave the portfolio
    it "stores a payment that spends coins of both accounts as one outgoing transaction" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith spentTogetherStore pool
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000),
                          (Just (fakeTxHash 2), Store.BlockRef 800000 2, 30000),
                          (Just (fakeTxHash 3), Store.BlockRef 800001 1, -71000)
                        ]
        spent <- summaryOf pool (fakeTxHash 3)
        liftIO $ (summaryType spent, summaryAmount spent, summaryFee spent) `shouldBe` (TxDebit, -71000, 1000)

    it "names in that outgoing transaction the coin of each account it spends, and the change" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith spentTogetherStore pool
        spent <- summaryOf pool (fakeTxHash 3)
        liftIO $ do
          summaryKinds spent `shouldBe` [Bip44, Bip84]
          Map.toList (Map.map myInputsPath (summaryMyInputs spent))
            `shouldMatchList` [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0)), (segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0))]
          Map.toList (Map.map myOutputsPath (summaryMyOutputs spent)) `shouldBe` [(internalAddr 0, legacyPath (Deriv :/ 1 :/ 0))]
          Map.toList (summaryOtherOutputs spent) `shouldBe` [(otherAddr 0, 70000)]

    -- 50000 + 30000 - 71000 = 9000, the change
    it "leaves the pool with the change of that payment alone" $
      inDatabaseE $ do
        pool <- account
        _ <- syncWith spentTogetherStore pool
        balances pool `dbShouldBeE` (9000, 0, 1)
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 3) 1]

  describe "Two test networks that share their keys" $ do
    -- Portfolio 0 on btctest and on btctest4: the bip44 account of each has
    -- the key at m/44'/1'/0'. The store of btctest pays its first
    -- receiving address 50000, and that of btctest4 its second one 30000.
    let smallGap = cfg {configGap = 3}
        pools = do
          portfolio <- testPortfolio
          return (portfolioPool portfolio btcTestnet3, portfolioPool portfolio btcTest4)
        storeOn net
          | net == btcTestnet3 = toStore testnetStore
          | otherwise = toStore testnet4Store
        syncBoth = do
          (test, test4) <- pools
          _ <- syncPools smallGap storeOn [test, test4] Incremental noQuestion
          return (test, test4)

    it "have the same key on both networks" $
      inDatabaseE $ do
        (test, test4) <- pools
        (dBAccountXPubKey . snd <$> poolAccountOf test Bip44) `dbShouldBeE` testAccountXPubTest
        (dBAccountXPubKey . snd <$> poolAccountOf test4 Bip44) `dbShouldBeE` testAccountXPubTest

    it "are synced each with the store of its own network" $
      inDatabaseE $ do
        (test, test4) <- pools
        synced <- syncPools smallGap storeOn [test, test4] Incremental noQuestion
        liftIO $ map syncResPlace (syncedPools synced) `shouldBe` [Place (PortfolioRef 0 0) btcTestnet3, Place (PortfolioRef 0 0) btcTest4]
        liftIO $ map syncResConfirmed (syncedPools synced) `shouldBe` [50000, 30000]

    -- The scan finds /0/0 on btctest, and /0/1 on btctest4
    it "generate the addresses each network found used, and no more on the other" $
      inDatabaseE $ do
        (test, test4) <- syncBoth
        addressCounts test Bip44 `dbShouldBeE` (1, 0)
        addressCounts test4 Bip44 `dbShouldBeE` (2, 0)

    it "keep the coins of each network apart" $
      inDatabaseE $ do
        (test, test4) <- syncBoth
        testId <- bip44Id test
        test4Id <- bip44Id test4
        coinsOfAccount testId `dbShouldBeE` [(OutPoint (fakeTxHash 1) 0, 50000)]
        coinsOfAccount test4Id `dbShouldBeE` [(OutPoint (fakeTxHash 7) 0, 30000)]

    it "keep the transactions of each network apart" $
      inDatabaseE $ do
        (test, test4) <- syncBoth
        summaries test `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        summaries test4 `dbShouldBeE` [(Just (fakeTxHash 7), Store.BlockRef 800001 1, 30000)]

    it "keep the balances of each network apart" $
      inDatabaseE $ do
        (test, test4) <- syncBoth
        balances test `dbShouldBeE` (50000, 0, 1)
        balances test4 `dbShouldBeE` (30000, 0, 1)

    -- The chain of btctest4 is one block ahead
    it "keep the best block of each network, and the block the sync of each reached" $
      inDatabaseE $ do
        (test, test4) <- syncBoth
        getBest btcTestnet3 `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800002 1).header) 800002 (Just 1700000000))
        getBest btcTest4 `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800003 7).header) 800003 (Just 1700000000))
        syncedBlock test `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800002 1).header) 800002)
        syncedBlock test4 `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800003 7).header) 800003)

    -- A store that serves both networks the same chain reports the coin
    -- and the transaction on both
    it "keep a row of their own for a coin and a transaction that the stores of both report" $
      inDatabaseE $ do
        (test, test4) <- pools
        _ <- syncPools smallGap (const (toStore testnetStore)) [test, test4] Incremental noQuestion
        testId <- bip44Id test
        test4Id <- bip44Id test4
        coinsOfAccount testId `dbShouldBeE` [(OutPoint (fakeTxHash 1) 0, 50000)]
        coinsOfAccount test4Id `dbShouldBeE` [(OutPoint (fakeTxHash 1) 0, 50000)]
        summaries test `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        summaries test4 `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]

    it "leave the pool of one alone when the other is synced" $
      inDatabaseE $ do
        (test, test4) <- pools
        _ <- sync smallGap (toStore testnetStore) test Incremental noQuestion
        summaries test4 `dbShouldBeE` []
        balances test4 `dbShouldBeE` (0, 0, 0)
        addressCounts test4 Bip44 `dbShouldBeE` (0, 0)
        syncedBlock test4 `dbShouldBeE` Nothing

  describe "Syncing the pools of several networks" $ do
    -- Portfolio 0 of the test wallet on btc and on bch, each pool with the
    -- first receiving address of its bip44 account
    let twoNetworks = do
          mainPool <- account
          cashPool <- (`portfolioPool` bch) <$> testPortfolio
          _ <- nextAddress cashPool
          return (mainPool, cashPool)
        places synced = map syncResPlace (syncedPools synced)
        placeOn = Place (PortfolioRef 0 0)
        -- The store of btc pays the first address 50000, and that of bch
        -- the first address there 50000
        bothUp net
          | net == bch = toStore cashPaymentStore
          | otherwise = toStore paymentStore

    it "syncs every pool given, each with the store of its network, in their order" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        synced <- syncPools cfg bothUp [mainPool, cashPool] Incremental noQuestion
        liftIO $ synced {syncedPools = []} `shouldBe` Synced [] [] []
        liftIO $ places synced `shouldBe` [placeOn btc, placeOn bch]
        summaries mainPool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        summaries cashPool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]

    it "syncs a portfolio on every one of its networks, the test networks included" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        pools <- lift $ portfolioPools portfolio.portfolioId
        synced <- syncPools cfg (const (toStore emptyStore)) pools Incremental noQuestion
        liftIO $
          places synced
            `shouldBe` [placeOn btc, placeOn bch, placeOn btcTestnet3, placeOn btcTest4, placeOn btcRegTest, placeOn bchTest3, placeOn bchTest4, placeOn bchRegTest]

    it "syncs the pools of every portfolio given" $
      inDatabaseE $ do
        first <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        synced <- syncPools cfg (const (toStore emptyStore)) [portfolioPool first btc, portfolioPool second btc, portfolioPool first bch, portfolioPool second bch] Incremental noQuestion
        liftIO $
          places synced
            `shouldBe` [Place (PortfolioRef 0 0) btc, Place (PortfolioRef 0 1) btc, Place (PortfolioRef 0 0) bch, Place (PortfolioRef 0 1) bch]

    -- The store of bch pays 50000 in block 800000, and that of btc is down
    it "syncs the pools of the networks whose store is up when the store of another is down" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        let storeOn net
              | net == bch = toStore cashPaymentStore
              | otherwise = unhealthy (toStore paymentStore)
        synced <- syncPools cfg storeOn [mainPool, cashPool] Incremental noQuestion
        liftIO $ places synced `shouldBe` [placeOn bch]
        summaries cashPool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]

    it "names a network whose store is down with the failure of its store" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        let storeOn net
              | net == bch = toStore cashPaymentStore
              | otherwise = unhealthy (toStore paymentStore)
        synced <- syncPools cfg storeOn [mainPool, cashPool] Incremental noQuestion
        liftIO $ (syncedDown synced, syncedNotServed synced) `shouldBe` ([(btc, "The store is down")], [])

    it "leaves the pools of a network whose store is down as they were" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        let storeOn net
              | net == bch = toStore cashPaymentStore
              | otherwise = unhealthy (toStore paymentStore)
        _ <- syncPools cfg storeOn [mainPool, cashPool] Incremental noQuestion
        summaries mainPool `dbShouldBeE` []
        syncedBlock mainPool `dbShouldBeE` Nothing

    -- The store of btc answers its health check, then fails when the sync
    -- asks for the coins, once the summaries are written. The store of bch
    -- pays 50000 in block 800000.
    let unreachable = "The store at https://api.haskoin.com/ could not be reached: the connection failed"
        failsOnCoins store = store {storeUnspents = \_ -> throwError unreachable}
        bitcoinFailsLater net
          | net == bch = toStore cashPaymentStore
          | otherwise = failsOnCoins (toStore paymentStore)

    it "syncs the pools of the other networks when the store of one fails after its health check" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        synced <- syncPools cfg bitcoinFailsLater [mainPool, cashPool] Incremental noQuestion
        liftIO $ places synced `shouldBe` [placeOn bch]
        summaries cashPool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]

    it "names a network whose store fails after its health check with that failure" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        synced <- syncPools cfg bitcoinFailsLater [mainPool, cashPool] Incremental noQuestion
        liftIO $ syncedDown synced `shouldBe` [(btc, "The store at https://api.haskoin.com/ could not be reached: the connection failed")]

    -- Both pools synced with stores at block 800002. The store of btc is
    -- then at block 799901, below block 799902 that the wallet keeps.
    it "leaves out a network whose store is far behind the wallet, and syncs the others" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        _ <- syncPools cfg bothUp [mainPool, cashPool] Incremental noQuestion
        let bitcoinBehind net
              | net == bch = toStore cashPaymentStore
              | otherwise = toStore (emptyStore {fakeBest = blockAt 799901 1, fakeBlocks = [blockAt 799901 1]})
        synced <- syncPools cfg bitcoinBehind [mainPool, cashPool] Incremental noQuestion
        liftIO $ places synced `shouldBe` [placeOn bch]
        liftIO $ (fst <$> syncedDown synced) `shouldBe` [btc]
        deadSummaries mainPool `dbShouldBeE` []

    -- The summary of the payment and the balance of its address were
    -- written before the store failed
    it "undoes what the sync of a pool wrote before the store of its network failed" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        _ <- syncPools cfg bitcoinFailsLater [mainPool, cashPool] Incremental noQuestion
        summaries mainPool `dbShouldBeE` []
        balances mainPool `dbShouldBeE` (0, 0, 0)
        syncedBlock mainPool `dbShouldBeE` Nothing

    -- Portfolios 0 and 1 of the test wallet on btc, each with its first
    -- receiving address: account1Addr is that of portfolio 1
    it "does not ask the store of a network that failed about the other pools of the network" $
      inDatabaseE $ do
        first <- account
        second <- (`portfolioPool` btc) <$> nextPortfolioOf testMnemonic
        _ <- nextAddress second
        asked <- liftIO $ newIORef []
        let storeOn _ = notingBalanceRequests asked (failsOnCoins (toStore paymentStore))
        synced <- syncPools cfg storeOn [first, second] Incremental noQuestion
        liftIO $ (places synced, syncedDown synced) `shouldBe` ([], [(btc, "The store at https://api.haskoin.com/ could not be reached: the connection failed")])
        liftIO $ (any (account1Addr `elem`) <$> readIORef asked) `shouldReturn` False

    -- The store of btc reports every address used, as one that lies does,
    -- and the user answers no at the limit of the scan. The pool of bch
    -- is synced first. The scan of btc asks about 20 addresses at a time
    -- from address 0, and has found 1020 used when it asks: 1019 past the
    -- one address the account has.
    it "fails when the user answers no to a question, though the store of another network was synced" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        let storeOn net
              | net == bch = toStore cashPaymentStore
              | otherwise = everyAddressUsed
        lift $
          dbShouldFailWith "The store reports used addresses as far as 1019 past the 1 receiving address the bip44 account of portfolio0 on btc has, and more beyond. The scan stopped there, and nothing of what was found was kept. If the report is right for this wallet, answer yes the next time." $
            syncPools cfg storeOn [cashPool, mainPool] Incremental stopLooking

    -- No host serves every network: the public one has no regtest chain
    it "names a network the host does not serve apart, which is no failure" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        let storeOn net
              | net == bch = notServed (toStore emptyStore)
              | otherwise = toStore paymentStore
        synced <- syncPools cfg storeOn [mainPool, cashPool] Incremental noQuestion
        liftIO $ (places synced, syncedNotServed synced, syncedDown synced) `shouldBe` ([placeOn btc], [bch], [])

    it "tells a network that is down from one that is not served, in one sync" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        let storeOn net
              | net == btc = toStore emptyStore
              | net == bch = unhealthy (toStore emptyStore)
              | otherwise = notServed (toStore emptyStore)
        synced <- syncPools cfg storeOn [portfolioPool portfolio net | net <- [btc, bch, btcTestnet3, btcRegTest]] Incremental noQuestion
        liftIO $ (places synced, syncedNotServed synced, syncedDown synced) `shouldBe` ([placeOn btc], [btcTestnet3, btcRegTest], [(bch, "The store is down")])

    it "asks the store of a network once whether it is up, however many pools the network has" $
      inDatabaseE $ do
        first <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        checks <- liftIO $ newIORef (0 :: Int)
        let down = (toStore emptyStore) {storeHealth = liftIO (modifyIORef checks (+ 1)) >> throwError (NotServed "The host does not serve the network")}
            storeOn net
              | net == bch = down
              | otherwise = toStore emptyStore
        _ <- syncPools cfg storeOn [portfolioPool first btc, portfolioPool first bch, portfolioPool second bch] Incremental noQuestion
        liftIO $ readIORef checks `shouldReturn` 1

    -- Both stores are up
    it "tells on the line that it checks the health of the store of each network, before it syncs" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        steps <- liftIO $ newIORef []
        _ <- syncPools cfg (const (toStore emptyStore)) [mainPool, cashPool] Incremental noQuestion {status = notingSteps steps}
        liftIO $ (take 2 <$> readIORef steps) `shouldReturn` [("sync", CheckingHealth btc), ("sync", CheckingHealth bch)]

    it "fails with the failure of the first store when the store of every network is down" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        lift $
          dbShouldFailWith "The store is down" $
            syncPools cfg (const (unhealthy (toStore emptyStore))) [mainPool, cashPool] Incremental noQuestion

    -- What a wrong host looks like
    it "fails when the host serves none of the networks, naming them and the host" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        lift $
          dbShouldFailWith "The store at https://api.haskoin.com/ does not serve btc, bch: check the host in the configuration file" $
            syncPools cfg (const (notServed (toStore emptyStore))) [mainPool, cashPool] Incremental noQuestion

    it "fails with the failure of the store that is down when the host serves no other network" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        let storeOn net
              | net == bch = notServed (toStore emptyStore)
              | otherwise = unhealthy (toStore emptyStore)
        lift $ dbShouldFailWith "The store is down" $ syncPools cfg storeOn [mainPool, cashPool] Incremental noQuestion

    it "syncs nothing, and does not fail, when no pool is given" $
      inDatabaseE $
        syncPools cfg (const (unhealthy (toStore emptyStore))) [] Incremental noQuestion `dbShouldBeE` Synced [] [] []

    it "syncs each pool in full when told to" $
      inDatabaseE $ do
        (mainPool, cashPool) <- twoNetworks
        _ <- syncPools cfg bothUp [mainPool, cashPool] Incremental noQuestion
        asked <- liftIO $ newIORef []
        _ <- syncPools cfg (notingTxRequests asked . bothUp) [mainPool, cashPool] Full noQuestion
        liftIO $ (concat <$> readIORef asked) `shouldReturn` [fakeTxHash 1, fakeTxHash 1]

  describe "Scanning a chain for used addresses" $ do
    -- The scan behind the watch of the gap, on the external chain of an
    -- account that has no address yet, with a gap of 3 instead of the
    -- default 20: the ten written-out addresses of the test wallet are
    -- enough
    let scanOf store = runExceptT $ scanChain store Bip44 testAccountPub extDeriv 3 newChain "on the chain" noQuestion
        scan = scanOf . toStore
        -- A store where the given addresses each have one transaction
        used addrs = emptyStore {fakeBalances = [Store.Balance addr 1000 0 1 1 1000 | addr <- addrs]}
        -- The scan of the store, and the addresses of each of its requests
        requestsOf store = do
          asked <- newIORef []
          result <- scanOf (notingBalanceRequests asked (toStore store))
          (,) result <$> readIORef asked

    it "finds nothing in an empty store, after it asked about as many addresses as the gap" $
      requestsOf emptyStore `shouldReturn` (Right 0, [take 3 externalAddrs])

    -- Addresses 1 and 2 were never used: two in a row, less than the gap
    it "counts the addresses up to the last used one" $
      scan (used [externalAddr 0, externalAddr 3]) `shouldReturn` Right 4

    it "stops at as many unused addresses in a row as the gap, so a gap of three hides address 3" $
      scan (used [externalAddr 3]) `shouldReturn` Right 0

    -- Addresses 1, 2 and 3 were never used: three in a row, the gap
    it "counts the unused addresses from the last used one, so a gap of three after address 0 hides address 4" $
      scan (used [externalAddr 0, externalAddr 4]) `shouldReturn` Right 1

    it "looks as far as the gap past the last used address" $
      requestsOf (used [externalAddr 2]) `shouldReturn` (Right 3, [take 3 externalAddrs, take 3 (drop 3 externalAddrs)])

    -- Address 0 moves the end of the scan from address 2 to address 3
    it "asks about each address once: after address 0, only about the one address the gap gained" $
      requestsOf (used [externalAddr 0]) `shouldReturn` (Right 1, [take 3 externalAddrs, [externalAddr 3]])

    it "counts an address whose coins are all spent as used" $
      scan (emptyStore {fakeBalances = [Store.Balance (externalAddr 1) 0 0 0 2 1000]}) `shouldReturn` Right 2

    it "ignores an address the store lists without transactions" $
      scan (emptyStore {fakeBalances = [Store.Balance (externalAddr 1) 0 0 0 0 0]}) `shouldReturn` Right 0

    it "fails when the store fails" $
      scanOf ((toStore emptyStore) {storeBalances = \_ -> throwError "The store is down"}) `shouldReturn` Left "The store is down"

    -- With a gap of 3, the scan asks about three addresses at a time: it
    -- has found 1002 used ones when it asks, and 1002 more when it asks
    -- again. The first answer goes on, the second stops.
    it "asks whether to go on after a thousand addresses the store reports used in a row, saying where, and asks again as many further" $ do
      asked <- newIORef []
      result <- runExceptT $ scanChain everyAddressUsed Bip44 testAccountPub extDeriv 3 newChain "on the receiving chain of the bip44 account of portfolio0 on btc" (answering asked 1)
      readIORef asked
        `shouldReturn` [ "The store reports used addresses as far as 1002 on the receiving chain of the bip44 account of portfolio0 on btc, and more beyond.",
                         "The store reports used addresses as far as 2004 on the receiving chain of the bip44 account of portfolio0 on btc, and more beyond."
                       ]
      result `shouldBe` Left "The store reports used addresses as far as 2004 on the receiving chain of the bip44 account of portfolio0 on btc, and more beyond. The scan stopped there, and nothing of what was found was kept. If the report is right for this wallet, answer yes the next time."

    it "stops at the limit when the answer is no" $
      runExceptT (scanChain everyAddressUsed Bip44 testAccountPub extDeriv 3 newChain "on the chain" stopLooking)
        `shouldReturn` Left "The store reports used addresses as far as 1002 on the chain, and more beyond. The scan stopped there, and nothing of what was found was kept. If the report is right for this wallet, answer yes the next time."

    it "recovers a portfolio on its first sync, as a restored one is: every used address, its transactions and coins" $
      inDatabaseE $ do
        pool <- testPool
        res <- sync cfg {configGap = 3} (toStore discoveredStore) pool Incremental noQuestion
        -- 4 receiving addresses and 2 change addresses: 6
        liftIO $ syncResNewAddrs res `shouldBe` 6
        addressCounts pool Bip44 `dbShouldBeE` (4, 2)
        found <- summaries pool
        liftIO $ found `shouldMatchList` [(Just (txHash tx), Store.BlockRef height 1, fromIntegral amount) | (_, amount, height, tx) <- discoveredPayments]
        held <- coins pool
        liftIO $ held `shouldMatchList` [OutPoint (txHash tx) 0 | (_, _, _, tx) <- discoveredPayments]
        balances pool `dbShouldBeE` (80000, 0, 3)
        rawTransactionCount `dbShouldBeE` 3

  describe "The steps a sync tells" $ do
    let watching steps = noQuestion {status = notingSteps steps}
        -- What a sync tells of the pool, in the order told
        toldOf steps = map snd <$> readIORef steps
        -- A request of the scan of each chain that finds nothing: the
        -- receiving and the change chain of the bip44 account, then those
        -- of the bip84 account
        fourChains = replicate 4 (LookingForAddresses 0)
        -- The steps of a task on a single item
        oneOf task = [Counted task 0 1, Counted task 1 1]

    -- The pool has one address, paid once, which the wallet does not know
    -- used yet. The scan of its chain finds it used in a first request,
    -- and a second one finds nothing in the gap past it. Each of the other
    -- three chains takes one request, which finds nothing.
    it "tells every step of a sync that finds a payment, each counting its one item" $
      inDatabaseE $ do
        pool <- account
        steps <- liftIO $ newIORef []
        _ <- sync cfg (toStore realPaymentStore) pool Incremental (watching steps)
        liftIO $
          toldOf steps
            `shouldReturn` [CheckingStore, LookingForAddresses 0]
              <> replicate 4 (LookingForAddresses 1)
              <> oneOf ReadingBalances
              <> oneOf ListingTransactions
              <> oneOf FetchingTransactions
              <> oneOf ReadingCoins
              <> oneOf FetchingRawTransactions

    it "tells a sync that finds nothing new, which only reads the balances" $
      inDatabaseE $ do
        pool <- account
        let store = toStore realPaymentStore
        _ <- sync cfg store pool Incremental noQuestion
        steps <- liftIO $ newIORef []
        _ <- sync cfg store pool Incremental (watching steps)
        liftIO $ toldOf steps `shouldReturn` [CheckingStore] <> fourChains <> oneOf ReadingBalances

    it "names the portfolio and its network in every step" $
      inDatabaseE $ do
        pool <- account
        steps <- liftIO $ newIORef []
        _ <- sync cfg (toStore realPaymentStore) pool Incremental (watching steps)
        names <- liftIO $ map fst <$> readIORef steps
        liftIO $ names `shouldSatisfy` all (== "portfolio0 btc")

    -- A sync goes over the portfolios of every wallet: the label names the
    -- wallet as the lines of the answers do, when the database has
    -- several. The database has wallet0 and wallet1 here.
    it "names the wallet of the portfolio in every step when the database has several wallets" $
      inDatabaseE $ do
        _ <- testPool
        pool <- secondAccount
        steps <- liftIO $ newIORef []
        _ <- sync cfg (toStore emptyStore) pool Incremental (watching steps)
        names <- liftIO $ map fst <$> readIORef steps
        liftIO $ names `shouldSatisfy` all (== "portfolio0 of wallet1 btc")

    -- portfolio0 alone could be that of either wallet
    it "names wallet0 too in every step when the database has several wallets" $
      inDatabaseE $ do
        pool <- testPool
        _ <- secondAccount
        steps <- liftIO $ newIORef []
        _ <- sync cfg (toStore emptyStore) pool Incremental (watching steps)
        names <- liftIO $ map fst <$> readIORef steps
        liftIO $ names `shouldSatisfy` all (== "portfolio0 of wallet0 btc")

    it "names a pool by its portfolio and its network in a database of one wallet" $
      poolLabel WithoutWallets (Place (PortfolioRef 0 0) btc) `shouldBe` "portfolio0 btc"

    it "names the wallet of a pool after its portfolio in a database of several wallets, wallet0 too" $ do
      poolLabel WithWallets (Place (PortfolioRef 0 0) btc) `shouldBe` "portfolio0 of wallet0 btc"
      poolLabel WithWallets (Place (PortfolioRef 1 2) bch) `shouldBe` "portfolio2 of wallet1 bch"

    -- With a gap of 3, the receiving chain of the bip44 account takes
    -- three requests: the first finds address 0, the second address 3,
    -- the third nothing. Its change chain takes two: the first finds
    -- address 1, the second nothing. Each chain of the bip84 account
    -- takes one, which finds nothing. Each request tells what the ones
    -- before it found.
    it "tells the used addresses a scan finds, counting on from one chain to the next, and from one account to the other" $
      inDatabaseE $ do
        pool <- testPool
        steps <- liftIO $ newIORef []
        _ <- sync cfg {configGap = 3} (toStore discoveredStore) pool Incremental (watching steps)
        told <- liftIO $ toldOf steps
        liftIO $ [found | LookingForAddresses found <- told] `shouldBe` [0, 1, 2, 2, 3, 3, 3]

    -- The sync makes two requests at a time, and the count still reaches
    -- all seven addresses
    it "counts every address listed, whatever the requests at a time" $
      inDatabaseE $ do
        pool <- account
        -- externalAddr 1 to externalAddr 6 as well
        replicateM_ 6 $ nextAddress pool
        let twoAtATime = cfg {configParallelRequests = 2}
        steps <- liftIO $ newIORef []
        _ <- sync twoAtATime (toStore sevenAddressesStore) pool Incremental (watching steps)
        told <- liftIO $ toldOf steps
        liftIO $ [done | Counted ListingTransactions done _ <- told] `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7]

  describe "Recovering the portfolios of a seed" $ do
    -- Portfolios 0 and 2 of the test seed were paid at the first receiving
    -- address of their bip44 account on Bitcoin; portfolio 1 and the ones
    -- after 2 never were
    let paidAccounts = paidStore [(externalAddr 0, 50000), (account2Addr, 20000)]
        -- A recovery on the networks, each asked of the same store
        recoverOn networks store seed gap = recoverSeed cfg (const (toStore store)) networks seed gap noQuestion
        found networks store seed gap = recoveredPortfolios <$> recoverOn networks store seed gap
        nothingRecovered = Recovered Nothing [] [] []
        placeOf portfolio = Place (PortfolioRef 0 portfolio)

    it "finds the portfolios that were paid, looking past a run of unpaid ones shorter than the gap" $
      inDatabaseE $ found [btc] paidAccounts testSeed 2 `dbShouldBeE` [0, 2]

    it "stops after as many unpaid portfolios in a row as the gap: with a gap of 1, portfolio 2 is not found" $
      inDatabaseE $ found [btc] paidAccounts testSeed 1 `dbShouldBeE` [0]

    -- Portfolios 0 and 10 were paid. The nine between them never were.
    it "finds, with the default gap of 10, a paid portfolio after nine that were never paid" $
      inDatabaseE $
        found [btc] (paidStore [(externalAddr 0, 50000), (account10Addr, 20000)]) testSeed 10 `dbShouldBeE` [0, 10]

    -- Portfolios 0 and 11 were paid. The ten between them never were.
    it "does not find, with the default gap of 10, a paid portfolio after ten that were never paid" $
      inDatabaseE $
        found [btc] (paidStore [(externalAddr 0, 50000), (account11Addr, 20000)]) testSeed 10 `dbShouldBeE` [0]

    -- Portfolios 0 and 1 make the gap of 2 before portfolio 2 is looked at
    it "finds nothing when the first portfolios, as many as the gap, were never paid, though a later one was" $
      inDatabaseE $ do
        recoverOn [btc] (paidStore [(account2Addr, 20000)]) testSeed 2 `dbShouldBeE` nothingRecovered
        walletsHeld `dbShouldBeE` []

    it "looks at portfolio 0, then at portfolio 1, on every network given: one window of the receiving chain of each account" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore emptyStore)
      inDatabaseE $ recoverSeed cfg (const store) [btc, bch] testSeed 2 noQuestion `dbShouldBeE` nothingRecovered
      windows <- readIORef asked
      map length windows `shouldBe` [20, 20, 20, 20, 20, 20]
      map (take 1) windows
        `shouldBe` [[externalAddr 0], [segwitExternalAddr 0], [bchExternalAddr], [account1Addr], [segwitAccount1Addr], [bchAccount1Addr]]

    -- The twelve accounts of portfolio 0. The test networks share their
    -- keys, so each is asked about the same addresses.
    it "looks at every account of a portfolio on the eight networks, the test networks included" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore emptyStore)
      inDatabaseE $ recoverSeed cfg (const store) portfolioNetworks testSeed 1 noQuestion `dbShouldBeE` nothingRecovered
      windows <- readIORef asked
      map (take 1) windows
        `shouldBe` [ [externalAddr 0],
                     [segwitExternalAddr 0],
                     [bchExternalAddr],
                     [testnetExternalAddr],
                     [testnetSegwitAddr],
                     [testnetExternalAddr],
                     [testnetSegwitAddr],
                     [testnetExternalAddr],
                     [testnetSegwitAddr],
                     [testnetExternalAddr],
                     [testnetExternalAddr],
                     [testnetExternalAddr]
                   ]

    it "creates the whole portfolio, with its twelve accounts, when one account of it was paid on Bitcoin Cash alone" $
      inDatabaseE $ do
        found portfolioNetworks (paidStore [(bchExternalAddr, 50000)]) testSeed 1 `dbShouldBeE` [0]
        accountsHeld `dbShouldBeE` twelveOf 0

    it "creates the whole portfolio when one account of it was paid on a test network alone" $
      inDatabaseE $ do
        found portfolioNetworks (paidStore [(testnetExternalAddr, 50000)]) testSeed 1 `dbShouldBeE` [0]
        accountsHeld `dbShouldBeE` twelveOf 0

    it "creates the whole portfolio when its bip84 account alone was paid" $
      inDatabaseE $ do
        found portfolioNetworks (paidStore [(segwitExternalAddr 0, 30000)]) testSeed 1 `dbShouldBeE` [0]
        accountsHeld `dbShouldBeE` twelveOf 0

    it "creates every account of each portfolio found, the test networks included" $
      inDatabaseE $ do
        _ <- recoverOn portfolioNetworks paidAccounts testSeed 2
        accountsHeld `dbShouldBeE` (twelveOf 0 <> twelveOf 2)

    -- Portfolio 2 alone was paid. A wallet always has portfolio 0.
    it "gives the wallet found its portfolio 0, whole, though that one was never paid" $
      inDatabaseE $ do
        found [btc] (paidStore [(account2Addr, 20000)]) testSeed 3 `dbShouldBeE` [2]
        accountsHeld `dbShouldBeE` (twelveOf 0 <> twelveOf 2)

    it "creates the wallet of the seed for the portfolios found, with the fingerprint of the seed" $
      inDatabaseE $ do
        recovered <- recoverOn [btc] paidAccounts testSeed 2
        liftIO $ recoveredWallet recovered `shouldBe` Just 0
        walletsHeld `dbShouldBeE` [(0, "bc560f95")]

    -- Each portfolio is synced on the networks searched. The first
    -- receiving address of each was found used on Bitcoin, and generated.
    it "syncs the portfolios found on the networks searched, and says what each holds" $
      inDatabaseE $ do
        recovered <- recoverOn [btc, bch] paidAccounts testSeed 2
        let syncs = recoveredSyncs recovered
        liftIO $ do
          map syncResPlace syncs `shouldBe` [placeOf 0 btc, placeOf 0 bch, placeOf 2 btc, placeOf 2 bch]
          map syncResConfirmed syncs `shouldBe` [50000, 0, 20000, 0]
          map syncResNewAddrs syncs `shouldBe` [1, 0, 1, 0]

    it "stores the transactions and the coins of the portfolios found" $
      inDatabaseE $ do
        _ <- recoverOn [btc] paidAccounts testSeed 2
        portfolios <- lift getAllPortfolios
        let pools = [portfolioPool portfolio btc | portfolio <- portfolios]
        mapM summaries pools
          `dbShouldBeE` [ [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)],
                          [(Just (fakeTxHash 2), Store.BlockRef 800000 2, 20000)]
                        ]
        mapM coins pools `dbShouldBeE` [[OutPoint (fakeTxHash 1) 0], [OutPoint (fakeTxHash 2) 0]]

    it "creates nothing for a seed that was never paid" $
      inDatabaseE $ do
        recoverOn portfolioNetworks emptyStore testSeed 2 `dbShouldBeE` nothingRecovered
        walletsHeld `dbShouldBeE` []
        accountsHeld `dbShouldBeE` []

    -- Portfolio 2 alone was paid, and the new wallet gets its portfolio 0
    -- first, as every wallet has it
    it "makes portfolio 0 of the new wallet the active portfolio, in a database without a portfolio, though portfolio 2 alone was paid" $
      inDatabaseE $ do
        _ <- recoverOn [btc] (paidStore [(account2Addr, 20000)]) testSeed 3
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 0)

    -- wallet0 is of the second test mnemonic; the test mnemonic recovers
    -- wallet1 with its portfolios 0 and 2
    it "leaves the active portfolio of the database as it is when it creates the portfolios of another wallet" $
      inDatabaseE $ do
        _ <- portfolioOf testMnemonic2
        _ <- recoverOn [btc] paidAccounts testSeed 2
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 0)
        (map portfolioRef <$> lift getAllPortfolios) `dbShouldBeE` [PortfolioRef 0 0, PortfolioRef 1 0, PortfolioRef 1 2]

    it "makes no portfolio active for a seed that was never paid" $
      inDatabaseE $ do
        _ <- recoverOn portfolioNetworks emptyStore testSeed 2
        activePortfolioRef `dbShouldBeE` Nothing

    -- The wallet has portfolio 0, whole. Portfolio 2 alone was paid: were
    -- portfolio 0 counted as never paid, it would make the gap of 2 with
    -- portfolio 1, and portfolio 2 would not be found.
    it "passes over a portfolio the wallet has whole already, without counting it" $
      inDatabaseE $ do
        _ <- testPortfolio
        recovered <- recoverOn [btc] (paidStore [(account2Addr, 20000)]) testSeed 2
        liftIO $ (recoveredWallet recovered, recoveredPortfolios recovered) `shouldBe` (Just 0, [2])

    -- wallet0 is of the second test mnemonic, 4435aa1c, and wallet1 of the
    -- test mnemonic, bc560f95. Portfolio 2 of the test mnemonic was paid.
    it "adds the portfolios found to the wallet with the fingerprint of the seed, among the wallets of other seeds" $
      inDatabaseE $ do
        _ <- portfolioOf testMnemonic2
        _ <- testPortfolio
        recovered <- recoverOn [btc] (paidStore [(account2Addr, 20000)]) testSeed 3
        liftIO $ (recoveredWallet recovered, recoveredPortfolios recovered) `shouldBe` (Just 1, [2])
        portfolios <- lift getAllPortfolios
        liftIO $
          [(walletIndex portfolio.portfolioWallet, portfolioIndex portfolio.portfolioRow) | portfolio <- portfolios]
            `shouldBe` [(0, 0), (1, 0), (1, 2)]

    -- The wallet bears the fingerprint of the test mnemonic, and holds the
    -- portfolio 0 of the second one: a fingerprint is 32 bits of a hash
    it "refuses a wallet with the fingerprint of the seed and the keys of another seed" $
      inDatabase $ do
        dbShouldFailWith
          "wallet0 has the fingerprint of this seed and other keys: it is of another seed with the same fingerprint. Nothing was changed"
          $ do
            held <- lift $ insertWallet testFingerprint
            _ <- createPortfolio testSeed2 held 0
            recoverOn [btc] paidAccounts testSeed 2

    -- Portfolio 0 is paid, 1 is not, 2 is, and 3 and 4 are not, which ends
    -- the search with a gap of 2. Each line tells what came before it.
    it "tells each portfolio it looks at, with the portfolios found paid so far" $
      inDatabaseE $ do
        steps <- liftIO $ newIORef []
        _ <- recoverSeed cfg (const (toStore paidAccounts)) [btc] testSeed 2 noQuestion {status = notingSteps steps}
        told <- liftIO $ readIORef steps
        liftIO $
          [statusText name step | (name, step) <- told, name == "recover"]
            `shouldBe` [ "recover: looking at portfolio0, 0 portfolios paid so far",
                         "recover: looking at portfolio1, 1 portfolio paid so far",
                         "recover: looking at portfolio2, 1 portfolio paid so far",
                         "recover: looking at portfolio3, 2 portfolios paid so far",
                         "recover: looking at portfolio4, 2 portfolios paid so far"
                       ]

    it "passes over an index whose child key is invalid, without counting it" $
      inDatabaseE $ found [btc] paidAccounts seedWithoutIndex1 1 `dbShouldBeE` [0, 2]

    it "asks on the receiving chain of an account of the seed, named by its path and its network" $
      inDatabaseE $
        lift $
          dbShouldFailWith "The store reports used addresses as far as 1000 on the receiving chain of the account /44'/0'/0' of the seed on btc, and more beyond. The scan stopped there, and nothing of what was found was kept. If the report is right for this wallet, answer yes the next time." $
            recoverSeed cfg (const everyAddressUsed) [btc] testSeed 1 stopLooking

    -- The store reports the portfolios 0 to 39 of the seed paid at the
    -- first receiving address of their bip44 account on Bitcoin. The
    -- first answer goes on, the second stops.
    it "asks whether to go on after 20 portfolios the store reports paid, naming the last one, and asks again 20 portfolios further" $
      inDatabaseE $ do
        firstAddrs <- liftIO $ firstAddrsOfAccounts 40
        asked <- liftIO $ newIORef []
        lift $
          dbShouldFailWith "The store reports payments to 40 portfolios of the seed, as far as portfolio39. The search stopped there, and nothing of what was found was kept. If the report is right for this wallet, answer yes the next time." $
            recoverSeed cfg (const (toStore (paidStore [(addr, 1000) | addr <- firstAddrs]))) [btc] testSeed 1 (answering asked 1)
        liftIO $
          readIORef asked
            `shouldReturn` [ "The store reports payments to 20 portfolios of the seed, as far as portfolio19.",
                             "The store reports payments to 40 portfolios of the seed, as far as portfolio39."
                           ]

    -- A store that lies reports a payment at every index, without end
    it "creates nothing when the answer is no" $
      inDatabaseE $ do
        firstAddrs <- liftIO $ firstAddrsOfAccounts 20
        lift $ dbShouldFail $ recoverSeed cfg (const (toStore (paidStore [(addr, 1000) | addr <- firstAddrs]))) [btc] testSeed 1 stopLooking
        walletsHeld `dbShouldBeE` []

    -- The store reports the portfolios 0 to 20 of the seed paid: 21, one
    -- more than the search finds before it asks
    it "finds every paid portfolio when the answer is yes, those before the question and those after" $
      inDatabaseE $ do
        firstAddrs <- liftIO $ firstAddrsOfAccounts 21
        let store = toStore (paidStore [(addr, 1000) | addr <- firstAddrs])
        (recoveredPortfolios <$> recoverSeed cfg (const store) [btc] testSeed 1 keepLooking)
          `dbShouldBeE` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

  describe "Recovering a seed when the store of a network cannot be asked" $ do
    -- As recover does: the store of each network is asked whether it is
    -- up, and the networks whose store is are searched. Portfolio 0 was
    -- paid on Bitcoin Cash alone, and portfolio 1 on Bitcoin alone.
    let paidOnCash = toStore (paidStore [(bchExternalAddr, 50000)])
        paidOnBitcoin = toStore (paidStore [(account1Addr, 20000)])
        cashDown net
          | net == bch = unhealthy paidOnCash
          | otherwise = paidOnBitcoin
        cashNotServed net
          | net == bch = notServed paidOnCash
          | otherwise = paidOnBitcoin
        -- The recovery of the test seed on the networks whose store is up
        recoverWhereUp storeOn gap = do
          served <- liftIO $ servedNetworks storeOn [btc, bch]
          recovered <- recoverSeed cfg storeOn (servedUp served) testSeed gap noQuestion
          return (served, recovered)

    it "leaves a network whose store is down out of those searched, with the failure of its store" $
      inDatabaseE $ do
        (served, _) <- recoverWhereUp cashDown 2
        liftIO $ served `shouldBe` Served {servedUp = [btc], servedNot = [], servedDown = [(bch, "The store is down")]}

    it "searches the other networks, and finds the portfolio paid there" $
      inDatabaseE $ do
        (_, recovered) <- recoverWhereUp cashDown 2
        liftIO $ recoveredPortfolios recovered `shouldBe` [1]

    -- Portfolio 0 is of the wallet found, and was not found paid: what it
    -- holds on Bitcoin Cash is not seen
    it "syncs the portfolios of the wallet on the networks searched alone" $
      inDatabaseE $ do
        (_, recovered) <- recoverWhereUp cashDown 2
        liftIO $ map syncResPlace (recoveredSyncs recovered) `shouldBe` [Place (PortfolioRef 0 0) btc, Place (PortfolioRef 0 1) btc]
        portfolio <- testPortfolio
        balances (portfolioPool portfolio bch) `dbShouldBeE` (0, 0, 0)

    -- With a gap of 1, the search on Bitcoin ends at portfolio 0, which
    -- was paid on Bitcoin Cash alone
    it "does not find a portfolio that was paid only on the network left out" $
      inDatabaseE $ do
        (_, recovered) <- recoverWhereUp cashDown 1
        liftIO $ recovered `shouldBe` Recovered Nothing [] [] []
        walletsHeld `dbShouldBeE` []

    it "finds that portfolio once the store of the network is up" $
      inDatabaseE $ do
        let bothUp net = if net == bch then paidOnCash else paidOnBitcoin
        (served, recovered) <- recoverWhereUp bothUp 1
        liftIO $ servedUp served `shouldBe` [btc, bch]
        liftIO $ recoveredPortfolios recovered `shouldBe` [0, 1]

    it "asks the store of the network left out about no address" $
      inDatabaseE $ do
        asked <- liftIO $ newIORef []
        let storeOn net
              | net == bch = unhealthy (notingBalanceRequests asked paidOnCash)
              | otherwise = paidOnBitcoin
        _ <- recoverWhereUp storeOn 2
        liftIO $ readIORef asked `shouldReturn` []

    it "leaves a network the host does not serve out of those searched, named apart, which is no failure" $
      inDatabaseE $ do
        (served, recovered) <- recoverWhereUp cashNotServed 1
        liftIO $ served `shouldBe` Served {servedUp = [btc], servedNot = [bch], servedDown = []}
        liftIO $ recovered `shouldBe` Recovered Nothing [] [] []

    -- The store of Bitcoin Cash answers its health check, then fails on
    -- the first addresses it is asked about
    let unreachable = "The store at https://api.haskoin.com/ could not be reached: the connection failed"
        cashFailsLater net
          | net == bch = paidOnCash {storeBalances = \_ -> throwError unreachable}
          | otherwise = paidOnBitcoin

    it "names a network whose store fails during the search with that failure" $
      inDatabaseE $ do
        (_, recovered) <- recoverWhereUp cashFailsLater 2
        liftIO $ recoveredDown recovered `shouldBe` [(bch, "The store at https://api.haskoin.com/ could not be reached: the connection failed")]

    it "goes on searching the other networks when the store of one fails during the search, and finds the portfolio paid there" $
      inDatabaseE $ do
        (_, recovered) <- recoverWhereUp cashFailsLater 2
        liftIO $ recoveredPortfolios recovered `shouldBe` [1]

    it "asks the store of a network that failed during the search nothing more" $
      inDatabaseE $ do
        asked <- liftIO $ newIORef []
        let storeOn net
              | net == bch = notingBalanceRequests asked (cashFailsLater bch)
              | otherwise = paidOnBitcoin
        _ <- recoverWhereUp storeOn 2
        liftIO $ (length <$> readIORef asked) `shouldReturn` 1

    -- Both stores answer the search: portfolio 0 was paid on Bitcoin Cash,
    -- and portfolio 1 on Bitcoin. The store of Bitcoin then fails when the
    -- sync of portfolio 1 there asks for the coins of its paid address.
    -- Portfolio 0 has no address on Bitcoin, so its sync there asks for no
    -- coin, and is done before.
    let bitcoinFailsInSync net
          | net == bch = paidOnCash
          | otherwise = paidOnBitcoin {storeUnspents = \_ -> throwError unreachable}

    it "keeps the portfolios found when the store of a network fails during their syncs" $
      inDatabaseE $ do
        (_, recovered) <- recoverWhereUp bitcoinFailsInSync 2
        liftIO $ (recoveredWallet recovered, recoveredPortfolios recovered) `shouldBe` (Just 0, [0, 1])

    it "names a network whose store fails during the syncs with that failure" $
      inDatabaseE $ do
        (_, recovered) <- recoverWhereUp bitcoinFailsInSync 2
        liftIO $ recoveredDown recovered `shouldBe` [(btc, "The store at https://api.haskoin.com/ could not be reached: the connection failed")]

    it "keeps the syncs done before the store of a network fails during the syncs, and goes on with the other networks" $
      inDatabaseE $ do
        (_, recovered) <- recoverWhereUp bitcoinFailsInSync 2
        liftIO $ map syncResPlace (recoveredSyncs recovered) `shouldBe` [Place (PortfolioRef 0 0) btc, Place (PortfolioRef 0 0) bch, Place (PortfolioRef 0 1) bch]

  describe "The funds of a seed at accounts hw does not hold" $ do
    -- The chain pays the bip49 account 0 of the seed at its first
    -- receiving address, confirmed, and at its second change address,
    -- unconfirmed, and the bip86 account 0 at its second receiving address
    let elsewhere =
          emptyStore
            { fakeBalances =
                [ Store.Balance (wrappedExternalAddr 0) 50000 0 1 1 50000,
                  Store.Balance (wrappedInternalAddr 1) 0 20000 1 1 20000,
                  Store.Balance (taprootExternalAddr 1) 7000 0 1 1 7000
                ]
            }
        -- A gap of 2 instead of the default 20: the five written-out
        -- addresses of each chain are enough for two windows
        smallGap = cfg {configGap = 2}
        found store net gap = runExceptT $ foreignFunds smallGap (toStore store) net testSeed gap noQuestion

    it "finds the bip49 and bip86 accounts paid on Bitcoin, with their used addresses and the balance of both chains, unconfirmed included" $
      -- 50000 confirmed + 20000 unconfirmed = 70000
      found elsewhere btc 1 `shouldReturn` Right [ForeignFunds Bip49 0 2 70000, ForeignFunds Bip86 0 1 7000]

    it "finds nothing for a seed never paid there, after one window of the receiving chain of each kind" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore emptyStore)
      runExceptT (foreignFunds smallGap store btc testSeed 1 noQuestion) `shouldReturn` Right []
      readIORef asked `shouldReturn` [[wrappedExternalAddr 0, wrappedExternalAddr 1], [taprootExternalAddr 0, taprootExternalAddr 1]]

    -- The bip49 account 0 was paid at its first receiving address, and
    -- the search moves on to its account 1
    it "scans the change chain of an account found, and only then" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore (emptyStore {fakeBalances = [Store.Balance (wrappedExternalAddr 0) 50000 0 1 1 50000]}))
      runExceptT (foreignFunds smallGap store btc testSeed 1 noQuestion) `shouldReturn` Right [ForeignFunds Bip49 0 1 50000]
      windows <- readIORef asked
      map (take 1) windows `shouldBe` [[wrappedExternalAddr 0], [wrappedExternalAddr 2], [wrappedInternalAddr 0], [wrappedAccount1Addr], [taprootExternalAddr 0]]

    it "looks past unpaid accounts as far as the gap allows, as for the portfolios of the seed" $ do
      let paidAccount1 = emptyStore {fakeBalances = [Store.Balance wrappedAccount1Addr 1000 0 1 1 1000]}
      found paidAccount1 btc 1 `shouldReturn` Right []
      found paidAccount1 btc 2 `shouldReturn` Right [ForeignFunds Bip49 1 1 1000]

    -- The chain pays the first receiving address of account 0 of the seed
    -- on Bitcoin, m/44'/0'/0', whose keys hold Bitcoin Cash as well
    it "finds the Bitcoin Cash at the bip44 accounts of Bitcoin, and looks for no SegWit account there" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore (emptyStore {fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 1 50000]}))
      runExceptT (foreignFunds smallGap store bch testSeed 1 noQuestion) `shouldReturn` Right [ForeignFunds Bip44OfBitcoin 0 1 50000]
      windows <- readIORef asked
      map (take 1) windows `shouldBe` [[externalAddr 0], [externalAddr 2], [internalAddr 0], [account1Addr]]

    it "asks nothing on the test networks of Bitcoin Cash, whose coin type is that of the test networks of Bitcoin" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore (emptyStore {fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 1 50000]}))
      runExceptT (foreignFunds smallGap store bchTest3 testSeed 1 noQuestion) `shouldReturn` Right []
      readIORef asked `shouldReturn` []

    it "fails when the store fails" $
      runExceptT (foreignFunds smallGap ((toStore emptyStore) {storeBalances = \_ -> throwError "The store is down"}) btc testSeed 1 noQuestion)
        `shouldReturn` Left "The store is down"

    it "asks on the chains of an account of a kind hw does not hold, named by its path and its network" $
      runExceptT (foreignFunds cfg everyAddressUsed btc testSeed 1 stopLooking)
        `shouldReturn` Left "The store reports used addresses as far as 1000 on the receiving chain of the account /49'/0'/0' of the seed on btc, and more beyond. The scan stopped there, and nothing of what was found was kept. If the report is right for this wallet, answer yes the next time."

    -- The store reports the bip49 accounts 0 to 19 of the seed paid at
    -- their first receiving address
    it "asks whether to go on after 20 accounts of a kind the store reports paid, as for the portfolios of the seed, and stops when the answer is no" $ do
      firstAddrs <- firstAddrsOfWrappedAccounts 20
      runExceptT (foreignFunds cfg (toStore (paidStore [(addr, 1000) | addr <- firstAddrs])) btc testSeed 1 stopLooking)
        `shouldReturn` Left "The store reports payments to 20 accounts of the seed on btc, as far as the account /49'/0'/19'. The search stopped there, and nothing of what was found was kept. If the report is right for this wallet, answer yes the next time."

  describe "Watching the gap of addresses" $ do
    -- A gap of 3 instead of the default 20: on each chain, the sync looks
    -- at the three addresses past the last used one. The account has its
    -- first receiving address, which was never paid.
    let smallGap = cfg {configGap = 3}
        watchWith store pool = sync smallGap (toStore store) pool Incremental noQuestion

    it "finds a payment to an address past the ones generated, and generates up to it" $
      inDatabaseE $ do
        pool <- account
        res <- watchWith (paidStore [(externalAddr 2, 50000)]) pool
        liftIO $ syncResNewAddrs res `shouldBe` 2
        addressCounts pool Bip44 `dbShouldBeE` (3, 0)
        (fmap snd <$> poolAddressPath pool (externalAddr 2)) `dbShouldBeE` Just (legacyPath (Deriv :/ 0 :/ 2))
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        coins pool `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        balances pool `dbShouldBeE` (50000, 0, 1)

    it "keeps looking while it finds a used address within the gap of the one before" $
      inDatabaseE $ do
        pool <- account
        res <- watchWith (paidStore [(externalAddr 2, 20000), (externalAddr 5, 30000)]) pool
        liftIO $ syncResNewAddrs res `shouldBe` 5
        addressCounts pool Bip44 `dbShouldBeE` (6, 0)
        summaries pool
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 20000),
                          (Just (fakeTxHash 2), Store.BlockRef 800000 2, 30000)
                        ]
        balances pool `dbShouldBeE` (50000, 0, 2)

    -- Addresses 0, 1 and 2 were never used: three in a row, the gap. The
    -- account has address 0, and that does not move the gap further.
    it "counts the gap from the last used address, not from the last address the account has" $
      inDatabaseE $ do
        pool <- account
        res <- watchWith (paidStore [(externalAddr 3, 50000)]) pool
        liftIO $ syncResNewAddrs res `shouldBe` 0
        addressCounts pool Bip44 `dbShouldBeE` (1, 0)
        summaries pool `dbShouldBeE` []
        balances pool `dbShouldBeE` (0, 0, 0)

    -- The chain paid 40000 to address 0 and 60000 to address 4, with
    -- three unused addresses between them: the gap. One wallet gave out
    -- address 0 before the payments, and the other gave out none.
    it "finds the same payments on a wallet that gave out an address and on one that gave out none" $ do
      let foundBy wallet = inDatabaseE $ do
            pool <- wallet
            _ <- watchWith (paidStore [(externalAddr 0, 40000), (externalAddr 4, 60000)]) pool
            addressCounts pool Bip44 `dbShouldBeE` (1, 0)
            balances pool `dbShouldBeE` (40000, 0, 1)
      foundBy account
      foundBy testPool

    -- The account has addresses 0 to 5, none of them paid at the last
    -- sync. The chain then paid address 5, which the account has, and
    -- address 7, one unused address after it.
    it "counts the gap from an address of the account paid since the last sync, in the same sync" $
      inDatabaseE $ do
        pool <- account
        replicateM_ 5 $ nextAddress pool
        res <- watchWith (paidStore [(externalAddr 5, 20000), (externalAddr 7, 30000)]) pool
        liftIO $ syncResNewAddrs res `shouldBe` 2
        addressCounts pool Bip44 `dbShouldBeE` (8, 0)
        balances pool `dbShouldBeE` (50000, 0, 2)

    -- The first sync finds address 2 used. The second one asks about the
    -- three receiving addresses after it.
    it "asks only about the addresses past the last used one it knows" $
      inDatabaseE $ do
        pool <- account
        let store = toStore (paidStore [(externalAddr 2, 50000)])
        _ <- sync smallGap store pool Incremental noQuestion
        asked <- liftIO $ newIORef []
        _ <- sync smallGap (notingBalanceRequests asked store) pool Incremental noQuestion
        liftIO $ (take 1 <$> readIORef asked) `shouldReturn` [take 3 (drop 3 externalAddrs)]

    it "finds change made by another wallet of the same seed" $
      inDatabaseE $ do
        pool <- account
        res <- watchWith (paidStore [(internalAddr 1, 10000)]) pool
        liftIO $ syncResNewAddrs res `shouldBe` 2
        addressCounts pool Bip44 `dbShouldBeE` (1, 2)
        summaries pool `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 10000)]
        balances pool `dbShouldBeE` (10000, 0, 1)
        -- /1/0 was never used: it is the next change address
        (dBAddressIndex <$> changeAddress cfg pool Bip44) `dbShouldBeE` 0

    -- The bip44 account has one receiving address, never paid, and the
    -- bip84 account none: the gap of each of the four chains from its
    -- start, then the balance of the one address the pool has
    it "asks about the gap of each chain of both accounts once when nothing is in it" $
      inDatabaseE $ do
        pool <- account
        asked <- liftIO $ newIORef []
        _ <- sync smallGap (notingBalanceRequests asked (toStore emptyStore)) pool Incremental noQuestion
        liftIO $
          readIORef asked
            `shouldReturn` [ take 3 externalAddrs,
                             take 3 internalAddrs,
                             map segwitExternalAddr [0, 1, 2],
                             map segwitInternalAddr [0, 1, 2],
                             [externalAddr 0]
                           ]

    -- The second receiving address of the bip84 account, of which the
    -- portfolio has generated none
    it "finds a payment to an address of the bip84 account past the ones generated, and generates up to it" $
      inDatabaseE $ do
        pool <- account
        res <- watchWith (paidStore [(segwitExternalAddr 1, 30000)]) pool
        liftIO $ syncResNewAddrs res `shouldBe` 2
        addressCounts pool Bip84 `dbShouldBeE` (2, 0)
        (fmap snd <$> poolAddressPath pool (segwitExternalAddr 1)) `dbShouldBeE` Just (segwitPath (Deriv :/ 0 :/ 1))
        balances pool `dbShouldBeE` (30000, 0, 1)

    -- The store reports every address used, as one that lies does: the
    -- sync asks before it generates a thousand addresses, and a no leaves
    -- the account as it was
    it "asks before generating a thousand addresses the store reports used past the ones an account has, naming the account, and generates none when the answer is no" $
      inDatabaseE $ do
        pool <- account
        asked <- liftIO $ newIORef []
        lift $
          -- The scan has asked about the addresses 0 to 1001: 1001 past
          -- the one the account has
          dbShouldFailWith "The store reports used addresses as far as 1001 past the 1 receiving address the bip44 account of portfolio0 on btc has, and more beyond. The scan stopped there, and nothing of what was found was kept. If the report is right for this wallet, answer yes the next time." $
            sync smallGap everyAddressUsed pool Incremental (answering asked 0)
        liftIO $ readIORef asked `shouldReturn` ["The store reports used addresses as far as 1001 past the 1 receiving address the bip44 account of portfolio0 on btc has, and more beyond."]
        addressCounts pool Bip44 `dbShouldBeE` (1, 0)

  describe "Choosing the transactions to fetch" $ do
    -- The store lists the payment of 50000 to an address as transaction 1
    -- in block 800000; the wallet stores a balance with a transaction count
    let payment = Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)
        listing addr count refs = (Store.Balance addr 50000 0 1 count 50000, refs)
        stored addr count = Map.fromList [(addr, AddressBalance 50000 0 1 count 50000)]
        knownIn block = Map.fromList [(fakeTxHash 1, block)]

    it "fetches a listed transaction it does not know" $
      transactionsToFetch Map.empty (stored (externalAddr 0) 1) [listing (externalAddr 0) 1 [payment]]
        `shouldBe` [fakeTxHash 1]

    it "leaves alone a known transaction listed in its block" $
      transactionsToFetch (knownIn (Store.BlockRef 800000 1)) (stored (externalAddr 0) 1) [listing (externalAddr 0) 1 [payment]]
        `shouldBe` []

    it "fetches a known transaction again when it moved to another block" $
      transactionsToFetch (knownIn (Store.BlockRef 800001 3)) (stored (externalAddr 0) 1) [listing (externalAddr 0) 1 [payment]]
        `shouldBe` [fakeTxHash 1]

    it "fetches the unconfirmed transactions again, listed or not" $
      transactionsToFetch (knownIn (Store.MemRef 1700000000)) Map.empty []
        `shouldBe` [fakeTxHash 1]

    it "fetches every transaction of an address that has transactions for the first time" $
      -- The known payment also paid this address, which its summary does
      -- not show yet: it is rebuilt along with the new transaction
      transactionsToFetch (knownIn (Store.BlockRef 800000 1)) (stored (externalAddr 1) 0) [listing (externalAddr 1) 2 [payment, Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 3)]]
        `shouldMatchList` [fakeTxHash 1, fakeTxHash 3]

    it "names a transaction once when two addresses list it" $
      transactionsToFetch Map.empty (stored (externalAddr 0) 1 <> stored (externalAddr 1) 1) [listing (externalAddr 0) 1 [payment], listing (externalAddr 1) 1 [payment]]
        `shouldBe` [fakeTxHash 1]

  describe "Telling the dead transactions" $ do
    -- Two known transactions; the store still lists the second
    let known = Set.fromList [fakeTxHash 1, fakeTxHash 2]
        alive = creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000
        listedRef = Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)

    it "counts a fetched transaction the store reports deleted as dead" $
      deadTransactions False known [fakeTxHash 1] [replaced alive] [listedRef] `shouldBe` [fakeTxHash 1]

    it "counts a fetched transaction the store does not return as dead" $
      deadTransactions False known [fakeTxHash 1] [] [listedRef] `shouldBe` [fakeTxHash 1]

    it "leaves alone a transaction the store returns alive" $
      deadTransactions False known [fakeTxHash 1] [alive] [listedRef] `shouldBe` []

    it "ignores a fetched transaction it never knew" $
      deadTransactions False known [fakeTxHash 9] [] [listedRef] `shouldBe` []

    it "counts a known transaction no address lists any more as dead, when the sync looked at every address only" $ do
      deadTransactions True known [] [] [listedRef] `shouldBe` [fakeTxHash 1]
      deadTransactions False known [] [] [listedRef] `shouldBe` []

  describe "The balances to update" $ do
    -- externalAddr 0 holds 50000 as stored; externalAddr 1 was empty and
    -- the store now reports 30000 unconfirmed
    let stored = Map.fromList [(externalAddr 0, AddressBalance 50000 0 1 1 50000), (externalAddr 1, AddressBalance 0 0 0 0 0)]
        unchanged = Store.Balance (externalAddr 0) 50000 0 1 1 50000
        paid = Store.Balance (externalAddr 1) 0 30000 1 1 30000

    it "are the addresses whose balance the store reports changed" $
      balancesToLookAt Set.empty [unchanged, paid] stored `shouldBe` [paid]

    it "include an address a transaction in the mempool touches, though its balance did not change" $
      balancesToLookAt (Set.fromList [externalAddr 0]) [unchanged, paid] stored `shouldBe` [unchanged, paid]

    it "are refused when the store answered about other addresses than asked, more or fewer" $ do
      answeredAbout [externalAddr 0, externalAddr 1] [unchanged] `shouldBe` Left "The store answered about other addresses than the ones asked"
      answeredAbout [externalAddr 0] [unchanged, paid] `shouldBe` Left "The store answered about other addresses than the ones asked"
      answeredAbout [externalAddr 0, externalAddr 1] [paid, unchanged] `shouldBe` Right ()

  describe "Asking the store in parallel" $ do
    it "answers in the order asked, even when later questions finish first" $ do
      -- Question 1 takes 25 milliseconds, question 5 takes 5
      let slowQuestion n = liftIO (threadDelay ((6 - n) * 5000)) >> return n
      answers <- runExceptT $ inParallel 3 [slowQuestion n | n <- [1 .. 5 :: Int]]
      answers `shouldBe` Right [1, 2, 3, 4, 5]

    it "asks at most the given number of questions at a time" $ do
      running <- newIORef (0 :: Int)
      most <- newIORef (0 :: Int)
      let countedQuestion = liftIO $ do
            now <- atomicModifyIORef' running $ \n -> (n + 1, n + 1)
            atomicModifyIORef' most $ \m -> (max m now, ())
            threadDelay 20000
            atomicModifyIORef' running $ \n -> (n - 1, ())
      _ <- runExceptT $ inParallel 3 $ replicate 7 countedQuestion
      readIORef most `shouldReturn` 3

    it "fails with the first failed question" $ do
      answers <-
        runExceptT $
          inParallel 2 [return 1, throwError "The store is down", throwError "Not this one", return (4 :: Int)]
      answers `shouldBe` Left "The store is down"

{- The stores -}

-- | The coin of a payment: output 0 of the transaction, at the given block
coinOf :: TxHash -> Maybe Natural -> Address -> Natural -> Store.Unspent
coinOf tid = coinAt tid 0

-- | The coin of an output of a transaction, at the given block
coinAt :: TxHash -> Word32 -> Maybe Natural -> Address -> Natural -> Store.Unspent
coinAt tid index = storeCoin (tid, index)

-- | externalAddr 0 paid 50000 by transaction 1, confirmed at height 800000
paymentStore :: FakeStore
paymentStore =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
      fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 1 50000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)])],
      fakeTransactions = [creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000],
      fakeUnspents = [coinOf (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000],
      fakeRawTransactions = [(fakeTxHash 1, fundingTx 1 [(externalAddr 0, 50000)])]
    }

-- | The same payment, still in the mempool
mempoolStore :: FakeStore
mempoolStore =
  paymentStore
    { fakeBalances = [Store.Balance (externalAddr 0) 0 50000 1 1 50000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.MemRef 1700000000) (fakeTxHash 1)])],
      fakeTransactions = [creditTx (fakeTxHash 1) (Store.MemRef 1700000000) (externalAddr 0) 50000],
      fakeUnspents = [coinOf (fakeTxHash 1) Nothing (externalAddr 0) 50000]
    }

-- | The payment replaced: the store still has it, marked as deleted, and
-- the address is back to nothing
replacedStore :: FakeStore
replacedStore =
  paymentStore
    { fakeBalances = [],
      fakeTxRefs = [],
      fakeTransactions = [replaced $ creditTx (fakeTxHash 1) (Store.MemRef 1700000000) (externalAddr 0) 50000],
      fakeUnspents = []
    }

-- | The payment dropped from the mempool and forgotten by the store
forgottenStore :: FakeStore
forgottenStore =
  replacedStore {fakeTransactions = []}

-- | The payment of 'mempoolStore' replaced by transaction 2, which pays
-- externalAddr 0 the same 50000 and is in the mempool too. The store
-- still has transaction 1, marked as deleted, and the balance of the
-- address is the one 'mempoolStore' reports.
sameAmountReplacedStore :: FakeStore
sameAmountReplacedStore =
  mempoolStore
    { fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.MemRef 1700000100) (fakeTxHash 2)])],
      fakeTransactions =
        [ replaced $ creditTx (fakeTxHash 1) (Store.MemRef 1700000000) (externalAddr 0) 50000,
          creditTx (fakeTxHash 2) (Store.MemRef 1700000100) (externalAddr 0) 50000
        ],
      fakeUnspents = [coinOf (fakeTxHash 2) Nothing (externalAddr 0) 50000],
      fakeRawTransactions = [(fakeTxHash 2, fundingTx 2 [(externalAddr 0, 50000)])]
    }

-- | The payment of 'mempoolStore' confirmed in block 800000 after all, and
-- the replacement of 'sameAmountReplacedStore' gone from the store. The
-- store took the payment in again from the block, dated 1700003700.
replacementLostStore :: FakeStore
replacementLostStore =
  paymentStore
    { fakeTransactions = [seenAt 1700003700 (creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)]
    }

-- | The coin of 'paymentStore' spent in the mempool by transaction 3, which
-- pays otherAddr 0 49000 with a fee of 1000, and no change
mempoolSpentStore :: FakeStore
mempoolSpentStore =
  paymentStore
    { fakeBalances = [Store.Balance (externalAddr 0) 0 0 0 2 50000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.MemRef 1700000000) (fakeTxHash 3), paymentRef])],
      fakeTransactions = paymentStore.fakeTransactions <> [mempoolSpending 3 1700000000],
      fakeUnspents = []
    }
  where
    paymentRef = Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)

-- | That spending replaced by transaction 4, which spends the same coin and
-- pays otherAddr 0 the same 49000. The store still has transaction 3,
-- marked as deleted, and the balance of externalAddr 0 is the one
-- 'mempoolSpentStore' reports.
spendingReplacedStore :: FakeStore
spendingReplacedStore =
  mempoolSpentStore
    { fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.MemRef 1700000100) (fakeTxHash 4), paymentRef])],
      fakeTransactions =
        paymentStore.fakeTransactions
          <> [replaced (mempoolSpending 3 1700000000), mempoolSpending 4 1700000100]
    }
  where
    paymentRef = Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)

-- | Transaction n in the mempool since the given time, spending the coin of
-- 'paymentStore' to pay otherAddr 0 49000 with a fee of 1000
mempoolSpending :: Word8 -> Word64 -> Store.Transaction
mempoolSpending n time =
  spendingStoreTx (fakeTxHash n) (Store.MemRef time) (fakeTxHash 1, 0) (externalAddr 0) [(otherAddr 0, 49000)]

-- | externalAddr 0 paid 30000 by transaction 2 at height 800001 and 20000
-- by transaction 3 at height 800002
twoPaymentsStore :: FakeStore
twoPaymentsStore =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
      fakeBalances = [Store.Balance (externalAddr 0) 50000 0 2 2 50000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        [ creditTx (fakeTxHash 2) (Store.BlockRef 800001 1) (externalAddr 0) 30000,
          creditTx (fakeTxHash 3) (Store.BlockRef 800002 1) (externalAddr 0) 20000
        ],
      fakeUnspents =
        [ coinOf (fakeTxHash 2) (Just 800001) (externalAddr 0) 30000,
          coinOf (fakeTxHash 3) (Just 800002) (externalAddr 0) 20000
        ],
      fakeRawTransactions =
        [ (fakeTxHash 2, fundingTx 2 [(externalAddr 0, 30000)]),
          (fakeTxHash 3, fundingTx 3 [(externalAddr 0, 20000)])
        ]
    }

-- | The two payments, and externalAddr 1 paid 10000 by transaction 4 long
-- ago, at height 700000
oldPaymentStore :: FakeStore
oldPaymentStore =
  twoPaymentsStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 50000 0 2 2 50000,
          Store.Balance (externalAddr 1) 10000 0 1 1 10000
        ],
      fakeTxRefs =
        fakeTxRefs twoPaymentsStore
          <> [(externalAddr 1, [Store.TxRef (Store.BlockRef 700000 1) (fakeTxHash 4)])],
      fakeTransactions =
        fakeTransactions twoPaymentsStore
          <> [creditTx (fakeTxHash 4) (Store.BlockRef 700000 1) (externalAddr 1) 10000],
      fakeUnspents =
        fakeUnspents twoPaymentsStore
          <> [coinOf (fakeTxHash 4) (Just 700000) (externalAddr 1) 10000],
      fakeRawTransactions =
        fakeRawTransactions twoPaymentsStore
          <> [(fakeTxHash 4, fundingTx 4 [(externalAddr 1, 10000)])]
    }

-- | The old payment of 10000 to externalAddr 1 at a place between the two
-- payments: in block 800001, after the one of 30000
latePaymentStore :: FakeStore
latePaymentStore =
  oldPaymentStore
    { fakeTxRefs =
        fakeTxRefs twoPaymentsStore
          <> [(externalAddr 1, [Store.TxRef lateBlock (fakeTxHash 4)])],
      fakeTransactions =
        fakeTransactions twoPaymentsStore
          <> [creditTx (fakeTxHash 4) lateBlock (externalAddr 1) 10000]
    }
  where
    lateBlock = Store.BlockRef 800001 5

-- | The two payments, with transaction 6 between them: 40000 in block
-- 800001, after the one of 30000
threePaymentsStore :: FakeStore
threePaymentsStore =
  twoPaymentsStore
    { fakeBalances = [Store.Balance (externalAddr 0) 90000 0 3 3 90000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800001 5) (fakeTxHash 6),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        fakeTransactions twoPaymentsStore
          <> [creditTx (fakeTxHash 6) (Store.BlockRef 800001 5) (externalAddr 0) 40000],
      fakeUnspents =
        fakeUnspents twoPaymentsStore
          <> [coinOf (fakeTxHash 6) (Just 800001) (externalAddr 0) 40000],
      fakeRawTransactions =
        fakeRawTransactions twoPaymentsStore
          <> [(fakeTxHash 6, fundingTx 6 [(externalAddr 0, 40000)])]
    }

-- | After a reorganisation that left transaction 6 out of the chain, and
-- kept the other two
middleLeftOutStore :: FakeStore
middleLeftOutStore =
  twoPaymentsStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7]
    }

-- | After a reorganisation that sent transaction 6 back to the mempool
backInMempoolStore :: FakeStore
backInMempoolStore =
  threePaymentsStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeBalances = [Store.Balance (externalAddr 0) 50000 40000 3 3 90000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.MemRef 1700000000) (fakeTxHash 6),
              Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        fakeTransactions twoPaymentsStore
          <> [creditTx (fakeTxHash 6) (Store.MemRef 1700000000) (externalAddr 0) 40000],
      fakeUnspents =
        fakeUnspents twoPaymentsStore
          <> [coinOf (fakeTxHash 6) Nothing (externalAddr 0) 40000]
    }

-- | The two payments, with transaction 8 spending the coin of the 20000 on
-- another wallet, 19000 and a fee of 1000, in the block of that payment
spentPaymentStore :: FakeStore
spentPaymentStore =
  twoPaymentsStore
    { fakeBalances = [Store.Balance (externalAddr 0) 30000 0 1 3 50000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800002 5) (fakeTxHash 8),
              Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        fakeTransactions twoPaymentsStore
          <> [spendingStoreTx (fakeTxHash 8) (Store.BlockRef 800002 5) (fakeTxHash 3, 0) (externalAddr 0) [(otherAddr 0, 19000)]],
      fakeUnspents = [coinOf (fakeTxHash 2) (Just 800001) (externalAddr 0) 30000]
    }

-- | 'spentPaymentStore' with the block in another order: the spending,
-- transaction 8, at position 1, and the payment it spends, transaction 3,
-- at position 5
spentBeforePaidStore :: FakeStore
spentBeforePaidStore =
  spentPaymentStore
    { fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800002 5) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 8),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        [ creditTx (fakeTxHash 2) (Store.BlockRef 800001 1) (externalAddr 0) 30000,
          creditTx (fakeTxHash 3) (Store.BlockRef 800002 5) (externalAddr 0) 20000,
          spendingStoreTx (fakeTxHash 8) (Store.BlockRef 800002 1) (fakeTxHash 3, 0) (externalAddr 0) [(otherAddr 0, 19000)]
        ]
    }

-- | After a reorganisation that put transaction 9, a payment of 15000, in
-- the place of the payment of 20000
rivalStore :: FakeStore
rivalStore =
  leftOutStore
    { fakeBalances = [Store.Balance (externalAddr 0) 45000 0 2 2 45000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 9),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        fakeTransactions leftOutStore
          <> [creditTx (fakeTxHash 9) (Store.BlockRef 800002 1) (externalAddr 0) 15000],
      fakeUnspents =
        fakeUnspents leftOutStore
          <> [coinOf (fakeTxHash 9) (Just 800002) (externalAddr 0) 15000],
      fakeRawTransactions =
        fakeRawTransactions twoPaymentsStore
          <> [(fakeTxHash 9, fundingTx 9 [(externalAddr 0, 15000)])]
    }

-- | After a reorganisation that moved the payment of 20000 within its
-- block, with the balances unchanged
oneMovedStore :: FakeStore
oneMovedStore =
  twoPaymentsStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800002 4) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        [ creditTx (fakeTxHash 2) (Store.BlockRef 800001 1) (externalAddr 0) 30000,
          creditTx (fakeTxHash 3) (Store.BlockRef 800002 4) (externalAddr 0) 20000
        ]
    }

-- | The two payments and two unconfirmed ones: transaction 6 pays 10000,
-- then transaction 7 pays 5000
twoUnconfirmedStore :: FakeStore
twoUnconfirmedStore =
  twoPaymentsStore
    { fakeBalances = [Store.Balance (externalAddr 0) 50000 15000 4 4 65000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.MemRef 1700000100) (fakeTxHash 7),
              Store.TxRef (Store.MemRef 1700000000) (fakeTxHash 6),
              Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        fakeTransactions twoPaymentsStore
          <> [ creditTx (fakeTxHash 6) (Store.MemRef 1700000000) (externalAddr 0) 10000,
               creditTx (fakeTxHash 7) (Store.MemRef 1700000100) (externalAddr 0) 5000
             ],
      fakeUnspents =
        fakeUnspents twoPaymentsStore
          <> [ coinOf (fakeTxHash 6) Nothing (externalAddr 0) 10000,
               coinOf (fakeTxHash 7) Nothing (externalAddr 0) 5000
             ],
      fakeRawTransactions =
        fakeRawTransactions twoPaymentsStore
          <> [ (fakeTxHash 6, fundingTx 6 [(externalAddr 0, 10000)]),
               (fakeTxHash 7, fundingTx 7 [(externalAddr 0, 5000)])
             ]
    }

-- | The unconfirmed transaction 6 replaced: the store still has it, marked
-- as deleted, and the address has lost its 10000
oneReplacedStore :: FakeStore
oneReplacedStore =
  twoUnconfirmedStore
    { fakeBalances = [Store.Balance (externalAddr 0) 50000 5000 3 3 55000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.MemRef 1700000100) (fakeTxHash 7),
              Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        fakeTransactions twoPaymentsStore
          <> [ replaced $ creditTx (fakeTxHash 6) (Store.MemRef 1700000000) (externalAddr 0) 10000,
               creditTx (fakeTxHash 7) (Store.MemRef 1700000100) (externalAddr 0) 5000
             ],
      fakeUnspents =
        fakeUnspents twoPaymentsStore
          <> [coinOf (fakeTxHash 7) Nothing (externalAddr 0) 5000]
    }

-- | Transaction 5 paid 30000 to externalAddr 0 and 20000 to externalAddr 1,
-- confirmed at height 800000, while the wallet knows externalAddr 0 only
payoutStore :: FakeStore
payoutStore =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
      fakeBalances = [Store.Balance (externalAddr 0) 30000 0 1 1 30000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 5)])],
      fakeTransactions = [payoutTx (fakeTxHash 5) (Store.BlockRef 800000 1) [(externalAddr 0, 30000), (externalAddr 1, 20000)]],
      fakeUnspents = [coinAt (fakeTxHash 5) 0 (Just 800000) (externalAddr 0) 30000],
      fakeRawTransactions = [(fakeTxHash 5, fundingTx 5 [(externalAddr 0, 30000), (externalAddr 1, 20000)])]
    }

-- | The same store once the wallet asks about externalAddr 1 as well
payoutSeenStore :: FakeStore
payoutSeenStore =
  payoutStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 30000 0 1 1 30000,
          Store.Balance (externalAddr 1) 20000 0 1 1 20000
        ],
      fakeTxRefs =
        [ (externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 5)]),
          (externalAddr 1, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 5)])
        ],
      fakeUnspents =
        [ coinAt (fakeTxHash 5) 0 (Just 800000) (externalAddr 0) 30000,
          coinAt (fakeTxHash 5) 1 (Just 800000) (externalAddr 1) 20000
        ]
    }

-- | After a reorganisation: the block at height 800002 is another one, the
-- chain is one block longer, and transaction 1 sits in block 800001 at
-- position 3 with the balances unchanged
movedStore :: FakeStore
movedStore =
  paymentStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800001 3) (fakeTxHash 1)])],
      fakeTransactions = [creditTx (fakeTxHash 1) (Store.BlockRef 800001 3) (externalAddr 0) 50000],
      fakeUnspents = [coinOf (fakeTxHash 1) (Just 800001) (externalAddr 0) 50000]
    }

-- | After a reorganisation that left transaction 3 out of the chain
leftOutStore :: FakeStore
leftOutStore =
  twoPaymentsStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeBalances = [Store.Balance (externalAddr 0) 30000 0 1 1 30000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)])],
      fakeTransactions = [creditTx (fakeTxHash 2) (Store.BlockRef 800001 1) (externalAddr 0) 30000],
      fakeUnspents = [coinOf (fakeTxHash 2) (Just 800001) (externalAddr 0) 30000]
    }

-- | externalAddr 0 paid 1000 by each of two hundred transactions, all in
-- block 800000
manyCoinsStore :: FakeStore
manyCoinsStore =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
      fakeBalances = [Store.Balance (externalAddr 0) 200000 0 200 200 200000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 (fromIntegral i)) (fakeTxHash i) | i <- indexes])],
      fakeTransactions = [creditTx (fakeTxHash i) (Store.BlockRef 800000 (fromIntegral i)) (externalAddr 0) 1000 | i <- indexes],
      fakeUnspents = [coinOf (fakeTxHash i) (Just 800000) (externalAddr 0) 1000 | i <- indexes],
      fakeRawTransactions = [(fakeTxHash i, fundingTx i [(externalAddr 0, 1000)]) | i <- indexes]
    }
  where
    indexes = [1 .. 200] :: [Word8]

-- | externalAddr 0 to externalAddr 6 each paid 1000, by transactions 1 to
-- 7 in block 800000: more addresses to list than the sync asks about at a
-- time
sevenAddressesStore :: FakeStore
sevenAddressesStore =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
      fakeBalances = [Store.Balance addr 1000 0 1 1 1000 | (_, addr) <- payments],
      fakeTxRefs = [(addr, [Store.TxRef (Store.BlockRef 800000 (fromIntegral i)) (fakeTxHash i)]) | (i, addr) <- payments],
      fakeTransactions = [creditTx (fakeTxHash i) (Store.BlockRef 800000 (fromIntegral i)) addr 1000 | (i, addr) <- payments],
      fakeUnspents = [coinOf (fakeTxHash i) (Just 800000) addr 1000 | (i, addr) <- payments],
      fakeRawTransactions = [(fakeTxHash i, fundingTx i [(addr, 1000)]) | (i, addr) <- payments]
    }
  where
    -- Transaction i paid externalAddr (i - 1)
    payments = [(i, externalAddr (fromIntegral i - 1)) | i <- [1 .. 7 :: Word8]]

-- | The raw transaction of the payment of 50000 to externalAddr 0, and
-- its hash: the id a real store lists the payment under
realPaymentTx :: Tx
realPaymentTx = fundingTx 1 [(externalAddr 0, 50000)]

realPaymentId :: TxHash
realPaymentId = txHash realPaymentTx

-- | The payment listed under the hash of its raw transaction, as a real
-- store lists it: the sync can then tell that it has the raw transaction
realPaymentStore :: FakeStore
realPaymentStore =
  paymentStore
    { fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) tid])],
      fakeTransactions = [creditTx tid (Store.BlockRef 800000 1) (externalAddr 0) 50000],
      fakeUnspents = [coinOf tid (Just 800000) (externalAddr 0) 50000],
      fakeRawTransactions = [(tid, realPaymentTx)]
    }
  where
    tid = realPaymentId

-- | After a reorganisation that left the payment of 'realPaymentStore' out
-- of the chain for good. Block 800002 of the first chain is off the main
-- chain, and the store has nothing of the payment: a store forgets a
-- transaction that a reorganisation replaced.
realPaymentGoneStore :: FakeStore
realPaymentGoneStore =
  emptyStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [outOfMain (blockAt 800002 1), blockAt 800002 9, blockAt 800003 7]
    }
  where
    outOfMain (Store.BlockData h _ w hd z wt t o f s) = Store.BlockData h False w hd z wt t o f s

-- | The raw transaction of a payout of 50000 to externalAddr 0 and 30000
-- to externalAddr 1, and its hash
realPayoutTx :: Tx
realPayoutTx = fundingTx 2 [(externalAddr 0, 50000), (externalAddr 1, 30000)]

realPayoutId :: TxHash
realPayoutId = txHash realPayoutTx

-- | The payout listed under the hash of its raw transaction, confirmed at
-- height 800000, both coins unspent
realPayoutStore :: FakeStore
realPayoutStore =
  emptyStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 50000 0 1 1 50000,
          Store.Balance (externalAddr 1) 30000 0 1 1 30000
        ],
      fakeTxRefs = [(externalAddr 0, [payoutRef]), (externalAddr 1, [payoutRef])],
      fakeTransactions = [realPayout],
      fakeUnspents =
        [ coinAt tid 0 (Just 800000) (externalAddr 0) 50000,
          coinAt tid 1 (Just 800000) (externalAddr 1) 30000
        ],
      fakeRawTransactions = [(tid, realPayoutTx)]
    }
  where
    tid = realPayoutId
    payoutRef = Store.TxRef (Store.BlockRef 800000 1) tid
    realPayout = payoutTx tid (Store.BlockRef 800000 1) [(externalAddr 0, 50000), (externalAddr 1, 30000)]

-- | The payout with its first coin spent by transaction 8, at height 800001
realPayoutFirstSpentStore :: FakeStore
realPayoutFirstSpentStore =
  realPayoutStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 0 0 0 2 50000,
          Store.Balance (externalAddr 1) 30000 0 1 1 30000
        ],
      fakeTxRefs = [(externalAddr 0, [spendRef 8, payoutRef]), (externalAddr 1, [payoutRef])],
      fakeTransactions =
        realPayoutStore.fakeTransactions
          <> [spendingStoreTx (fakeTxHash 8) (Store.BlockRef 800001 1) (tid, 0) (externalAddr 0) [(otherAddr 1, 49000)]],
      fakeUnspents = [coinAt tid 1 (Just 800000) (externalAddr 1) 30000]
    }
  where
    tid = realPayoutId
    payoutRef = Store.TxRef (Store.BlockRef 800000 1) tid
    spendRef n = Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash n)

-- | The payout with both coins spent, by transactions 8 and 9, at height
-- 800001
realPayoutAllSpentStore :: FakeStore
realPayoutAllSpentStore =
  realPayoutFirstSpentStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 0 0 0 2 50000,
          Store.Balance (externalAddr 1) 0 0 0 2 30000
        ],
      fakeTxRefs = [(externalAddr 0, [spendRef 8, payoutRef]), (externalAddr 1, [spendRef 9, payoutRef])],
      fakeTransactions =
        realPayoutFirstSpentStore.fakeTransactions
          <> [spendingStoreTx (fakeTxHash 9) (Store.BlockRef 800001 1) (tid, 1) (externalAddr 1) [(otherAddr 1, 29000)]],
      fakeUnspents = []
    }
  where
    tid = realPayoutId
    payoutRef = Store.TxRef (Store.BlockRef 800000 1) tid
    spendRef n = Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash n)

-- | The wallet's own payment from the coin of 'realPaymentStore', signed:
-- 40000 to otherAddr 0 and 9000 of change to internalAddr 0, 1000 of fee
changeSpendTx :: Tx
changeSpendTx =
  txSignDataTx $
    signedByWallet $
      TxSignData
        (spendingTx [(realPaymentId, 0)] [(otherAddr 0, 40000), (internalAddr 0, 9000)])
        [realPaymentTx]
        [legacyPath (Deriv :/ 0 :/ 0)]
        [legacyPath (Deriv :/ 1 :/ 0)]
        Nothing
        Nothing [] noLabel

-- | The payment of 'realPaymentStore' spent by 'changeSpendTx', confirmed
-- at height 800001: the coin is gone, the change is a coin at internalAddr 0
changeSpentStore :: FakeStore
changeSpentStore =
  realPaymentStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 0 0 0 2 50000,
          Store.Balance (internalAddr 0) 9000 0 1 1 9000
        ],
      fakeTxRefs =
        [ (externalAddr 0, [spendRef, paymentRef]),
          (internalAddr 0, [spendRef])
        ],
      fakeTransactions =
        [ creditTx realPaymentId (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          spendingStoreTx spend (Store.BlockRef 800001 1) (realPaymentId, 0) (externalAddr 0) [(otherAddr 0, 40000), (internalAddr 0, 9000)]
        ],
      fakeUnspents = [coinAt spend 1 (Just 800001) (internalAddr 0) 9000],
      fakeRawTransactions = [(realPaymentId, realPaymentTx), (spend, changeSpendTx)]
    }
  where
    spend = txHash changeSpendTx
    spendRef = Store.TxRef (Store.BlockRef 800001 1) spend
    paymentRef = Store.TxRef (Store.BlockRef 800000 1) realPaymentId

-- | A payment of 50000 to externalAddr 0 of the test wallet, as transaction
-- 1 at height 800000, and one of 30000 to the first address of the second
-- wallet, as transaction 2 at height 800001
twoWalletsStore :: FakeStore
twoWalletsStore =
  emptyStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 50000 0 1 1 50000,
          Store.Balance externalAddr2 30000 0 1 1 30000
        ],
      fakeTxRefs =
        [ (externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)]),
          (externalAddr2, [Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)])
        ],
      fakeTransactions =
        [ creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          creditTx (fakeTxHash 2) (Store.BlockRef 800001 1) externalAddr2 30000
        ],
      fakeUnspents =
        [ coinOf (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000,
          coinOf (fakeTxHash 2) (Just 800001) externalAddr2 30000
        ],
      fakeRawTransactions =
        [ (fakeTxHash 1, fundingTx 1 [(externalAddr 0, 50000)]),
          (fakeTxHash 2, fundingTx 2 [(externalAddr2, 30000)])
        ]
    }

-- | The payments of the two wallets after a reorganisation that moved the
-- payment of the second wallet, transaction 2, to block 800003, with the
-- balances unchanged
secondMovedStore :: FakeStore
secondMovedStore =
  twoWalletsStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeTxRefs =
        [ (externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)]),
          (externalAddr2, [Store.TxRef (Store.BlockRef 800003 3) (fakeTxHash 2)])
        ],
      fakeTransactions =
        [ creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          creditTx (fakeTxHash 2) (Store.BlockRef 800003 3) externalAddr2 30000
        ],
      fakeUnspents =
        [ coinOf (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000,
          coinOf (fakeTxHash 2) (Just 800003) externalAddr2 30000
        ]
    }

-- | The payments of the two wallets after a reorganisation that left the
-- payment of the second wallet, transaction 2, out of the chain
secondLeftOutStore :: FakeStore
secondLeftOutStore =
  twoWalletsStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 1 50000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)])],
      fakeTransactions = [creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000],
      fakeUnspents = [coinOf (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000]
    }

-- | A payment of 50000 to the first address of the Bitcoin Cash account,
-- as transaction 1, confirmed at height 800000
cashPaymentStore :: FakeStore
cashPaymentStore =
  emptyStore
    { fakeBalances = [Store.Balance bchExternalAddr 50000 0 1 1 50000],
      fakeTxRefs = [(bchExternalAddr, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)])],
      fakeTransactions = [creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) bchExternalAddr 50000],
      fakeUnspents = [coinOf (fakeTxHash 1) (Just 800000) bchExternalAddr 50000],
      fakeRawTransactions = [(fakeTxHash 1, fundingTx 1 [(bchExternalAddr, 50000)])]
    }

-- | The payments of 'discoveredStore': the address paid, the amount, the
-- height of the block, and the raw transaction, listed under its own hash
discoveredPayments :: [(Address, Word64, Word32, Tx)]
discoveredPayments =
  [ (externalAddr 0, 50000, 800000, fundingTx 1 [(externalAddr 0, 50000)]),
    (externalAddr 3, 20000, 800001, fundingTx 2 [(externalAddr 3, 20000)]),
    (internalAddr 1, 10000, 800001, fundingTx 3 [(internalAddr 1, 10000)])
  ]

-- | Payments to three addresses of a bip44 account that has not generated
-- any address yet: externalAddr 0 and 3, and internalAddr 1. No two of
-- them are three unused addresses apart, so a gap of 3 finds them all.
discoveredStore :: FakeStore
discoveredStore =
  emptyStore
    { fakeBalances = [Store.Balance addr amount 0 1 1 amount | (addr, amount, _, _) <- paid],
      fakeTxRefs = [(addr, [Store.TxRef (Store.BlockRef height 1) (txHash tx)]) | (addr, _, height, tx) <- paid],
      fakeTransactions = [creditTx (txHash tx) (Store.BlockRef height 1) addr amount | (addr, amount, height, tx) <- paid],
      fakeUnspents = [coinOf (txHash tx) (Just (fromIntegral height)) addr (fromIntegral amount) | (addr, amount, height, tx) <- paid],
      fakeRawTransactions = [(txHash tx, tx) | (_, _, _, tx) <- paid]
    }
  where
    paid = discoveredPayments

-- | Payments of the given amounts to the given addresses, one transaction
-- each, numbered from 1, all confirmed in block 800000
paidStore :: [(Address, Word64)] -> FakeStore
paidStore paid =
  emptyStore
    { fakeBalances = [Store.Balance addr amount 0 1 1 amount | (addr, amount, _) <- numbered],
      fakeTxRefs = [(addr, [Store.TxRef (Store.BlockRef 800000 (fromIntegral n)) (fakeTxHash n)]) | (addr, _, n) <- numbered],
      fakeTransactions = [creditTx (fakeTxHash n) (Store.BlockRef 800000 (fromIntegral n)) addr amount | (addr, amount, n) <- numbered],
      fakeUnspents = [coinOf (fakeTxHash n) (Just 800000) addr (fromIntegral amount) | (addr, amount, n) <- numbered],
      fakeRawTransactions = [(fakeTxHash n, fundingTx n [(addr, fromIntegral amount)]) | (addr, amount, n) <- numbered]
    }
  where
    numbered = [(addr, amount, n) | ((addr, amount), n) <- zip paid [1 :: Word8 ..]]

-- | A store that lists the coin of the payment but has no raw transaction
-- for it
noRawStore :: FakeStore
noRawStore = paymentStore {fakeRawTransactions = []}

-- | A store that counts two coins for the address but lists one
fewerCoinsStore :: FakeStore
fewerCoinsStore =
  paymentStore {fakeBalances = [Store.Balance (externalAddr 0) 50000 0 2 1 50000]}

-- | A store that counts two transactions for the address but lists one
fewerTxsStore :: FakeStore
fewerTxsStore =
  paymentStore {fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 2 50000]}

-- | The coin of 'pendingTx': 51000 paid to externalAddr 0
pendingParent :: Tx
pendingParent = fundingTx 9 [(externalAddr 0, 51000)]

-- | A transaction of ours, signed and waiting to be sent: it moves the coin
-- of 'pendingParent' back to externalAddr 0, for a fee of 1000
pendingTx :: Tx
pendingTx =
  txSignDataTx $
    signedByWallet $
      TxSignData
        (spendingTx [(txHash pendingParent, 0)] [(externalAddr 0, 50000)])
        [pendingParent]
        [legacyPath (Deriv :/ 0 :/ 0)]
        [legacyPath (Deriv :/ 0 :/ 0)]
        Nothing
        Nothing [] noLabel

-- | The coin of 'realPaymentStore' spent to otherAddr 0, with its signing
-- data as the wallet keeps it, signed
signedSpend :: TxSignData
signedSpend = signedByWallet unsignedSpend

unsignedSpend :: TxSignData
unsignedSpend =
  TxSignData
    (spendingTx [(realPaymentId, 0)] [(otherAddr 0, 40000)])
    [realPaymentTx]
    [legacyPath (Deriv :/ 0 :/ 0)]
    []
    (Just 10)
    Nothing [] noLabel

-- | The payment of 'realPaymentStore' spent by somebody else's transaction
-- 8, confirmed at height 800001: the coin is gone, and the store names
-- transaction 8 as the spender of the payment's output
spentElsewhereStore :: FakeStore
spentElsewhereStore =
  realPaymentStore
    { fakeBalances = [Store.Balance (externalAddr 0) 0 0 0 2 50000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 8),
              Store.TxRef (Store.BlockRef 800000 1) realPaymentId
            ]
          )
        ],
      fakeTransactions =
        [ withSpender 0 (fakeTxHash 8, 0) $ creditTx realPaymentId (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          spendingStoreTx (fakeTxHash 8) (Store.BlockRef 800001 1) (realPaymentId, 0) (externalAddr 0) [(otherAddr 1, 49000)]
        ],
      fakeUnspents = []
    }

-- | The pending transaction has reached the store
-- | The payment of 'realPaymentStore' spent by the transaction of
-- 'signedSpend' under another id, 9, confirmed at height 800001: a miner
-- re-encoded its signatures. Its inputs and outputs are those of the
-- transaction the wallet signed, and so is its nosigHash.
malleatedStore :: FakeStore
malleatedStore =
  realPaymentStore
    { fakeBalances = [Store.Balance (externalAddr 0) 0 0 0 2 50000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 9),
              Store.TxRef (Store.BlockRef 800000 1) realPaymentId
            ]
          )
        ],
      fakeTransactions =
        [ withSpender 0 (fakeTxHash 9, 0) $ creditTx realPaymentId (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          storeTx (fakeTxHash 9) (Store.BlockRef 800001 1) [inputFrom (realPaymentId, 0) (externalAddr 0) 50000] [outputTo (otherAddr 0) 40000] 10000
        ],
      fakeUnspents = []
    }

-- | The coin of 'realPaymentStore' swept by the other portfolio, portfolio
-- 0 of the second test wallet, to its first receiving address, less a fee
-- of 1000
sweepToOther :: TxSignData
sweepToOther =
  TxSignData (spendingTx [(realPaymentId, 0)] [(externalAddr2, 49000)]) [realPaymentTx] [] [legacyPath (Deriv :/ 0 :/ 0)] Nothing Nothing [] noLabel

-- | A payment of 10000 to somebody else's address, otherAddr 3: a coin
-- outside the wallet whose key the user holds, on a paper wallet for
-- instance
outsidePayment :: Tx
outsidePayment = fundingTx 9 [(otherAddr 3, 10000)]

-- | The sweep of that coin to the first receiving address of the account,
-- less a fee of 1000
outsideSweep :: TxSignData
outsideSweep =
  TxSignData (spendingTx [(txHash outsidePayment, 0)] [(externalAddr 0, 9000)]) [outsidePayment] [] [legacyPath (Deriv :/ 0 :/ 0)] Nothing Nothing [] noLabel

-- | A store that knows the payment to otherAddr 3, its coin unspent, and
-- nothing of the account
outsideStore :: FakeStore
outsideStore = emptyStore {fakeTransactions = [creditTx (txHash outsidePayment) (Store.BlockRef 800000 2) (otherAddr 3) 10000]}

-- | The same store once somebody else's transaction 8 spent that coin
outsideSpentStore :: FakeStore
outsideSpentStore = emptyStore {fakeTransactions = withSpender 0 (fakeTxHash 8, 0) <$> outsideStore.fakeTransactions}

sentStore :: FakeStore
sentStore =
  mempoolStore
    { fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.MemRef 1700000000) sent])],
      fakeTransactions = [creditTx sent (Store.MemRef 1700000000) (externalAddr 0) 50000],
      fakeUnspents = [coinOf sent Nothing (externalAddr 0) 50000],
      fakeRawTransactions = [(sent, pendingTx)]
    }
  where
    sent = txHash pendingTx

{- The two accounts of a pool -}

-- | externalAddr 0, of the bip44 account, paid 50000 by transaction 1 at
-- height 800000, and segwitExternalAddr 0, of the bip84 account, paid
-- 30000 by transaction 2 at height 800001
bothAccountsStore :: FakeStore
bothAccountsStore =
  emptyStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 50000 0 1 1 50000,
          Store.Balance (segwitExternalAddr 0) 30000 0 1 1 30000
        ],
      fakeTxRefs =
        [ (externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)]),
          (segwitExternalAddr 0, [Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)])
        ],
      fakeTransactions =
        [ creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          creditTx (fakeTxHash 2) (Store.BlockRef 800001 1) (segwitExternalAddr 0) 30000
        ],
      fakeUnspents =
        [ coinOf (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000,
          coinOf (fakeTxHash 2) (Just 800001) (segwitExternalAddr 0) 30000
        ],
      fakeRawTransactions =
        [ (fakeTxHash 1, fundingTx 1 [(externalAddr 0, 50000)]),
          (fakeTxHash 2, fundingTx 2 [(segwitExternalAddr 0, 30000)])
        ]
    }

-- | externalAddr 0 paid 50000 by transaction 1 at height 800000, then
-- that coin moved by transaction 2, at height 800001, to
-- segwitExternalAddr 0 of the same portfolio: 49000, and a fee of 1000
movedToSegwitStore :: FakeStore
movedToSegwitStore =
  emptyStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 0 0 0 2 50000,
          Store.Balance (segwitExternalAddr 0) 49000 0 1 1 49000
        ],
      fakeTxRefs =
        [ (externalAddr 0, [moveRef, paymentRef]),
          (segwitExternalAddr 0, [moveRef])
        ],
      fakeTransactions =
        [ creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          spendingStoreTx (fakeTxHash 2) (Store.BlockRef 800001 1) (fakeTxHash 1, 0) (externalAddr 0) [(segwitExternalAddr 0, 49000)]
        ],
      fakeUnspents = [coinOf (fakeTxHash 2) (Just 800001) (segwitExternalAddr 0) 49000],
      fakeRawTransactions = [(fakeTxHash 2, fundingTx 2 [(segwitExternalAddr 0, 49000)])]
    }
  where
    paymentRef = Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)
    moveRef = Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)

-- | externalAddr 0 paid 50000 by transaction 1 and segwitExternalAddr 0
-- paid 30000 by transaction 2, at height 800000, then both coins spent
-- together by transaction 3, at height 800001: 70000 to otherAddr 0, 9000
-- of change to internalAddr 0, and a fee of 1000
spentTogetherStore :: FakeStore
spentTogetherStore =
  emptyStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 0 0 0 2 50000,
          Store.Balance (segwitExternalAddr 0) 0 0 0 2 30000,
          Store.Balance (internalAddr 0) 9000 0 1 1 9000
        ],
      fakeTxRefs =
        [ (externalAddr 0, [spendRef, Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)]),
          (segwitExternalAddr 0, [spendRef, Store.TxRef (Store.BlockRef 800000 2) (fakeTxHash 2)]),
          (internalAddr 0, [spendRef])
        ],
      fakeTransactions =
        [ creditTx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          creditTx (fakeTxHash 2) (Store.BlockRef 800000 2) (segwitExternalAddr 0) 30000,
          storeTx
            (fakeTxHash 3)
            (Store.BlockRef 800001 1)
            [inputFrom (fakeTxHash 1, 0) (externalAddr 0) 50000, inputFrom (fakeTxHash 2, 0) (segwitExternalAddr 0) 30000]
            [outputTo (otherAddr 0) 70000, outputTo (internalAddr 0) 9000]
            1000
        ],
      fakeUnspents = [coinAt (fakeTxHash 3) 1 (Just 800001) (internalAddr 0) 9000],
      fakeRawTransactions = [(fakeTxHash 3, fundingTx 3 [(otherAddr 0, 70000), (internalAddr 0, 9000)])]
    }
  where
    spendRef = Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 3)

{- The test networks -}

-- | The second receiving address of the bip44 account of portfolio 0 of
-- the test wallet on a test network of Bitcoin, m/44'/1'/0'/0/1, after
-- 'testnetExternalAddr'; and the first receiving address of its bip84
-- account there, m/84'/1'/0'/0/0. Derived apart from hw with the Python
-- tool of the message-signing vectors.
testnetExternalAddr1, testnetSegwitAddr :: Address
testnetExternalAddr1 = testnetAddress "mudmTKxupVTdez63WtFJ8A7zZfE4xL7E3U"
testnetSegwitAddr = testnetAddress "tb1qjngk945r5gv9pk0ur7uqz9c6ntngkmtvknp70r"

testnetAddress :: Text -> Address
testnetAddress = fromMaybe (error "testnetAddress") . textToAddr btcTestnet3

-- | The store of btctest: 'testnetExternalAddr' paid 50000 by transaction
-- 1, confirmed at height 800000, on a chain whose best block is at 800002
testnetStore :: FakeStore
testnetStore = paidStore [(testnetExternalAddr, 50000)]

-- | The store of btctest4: 'testnetExternalAddr1' paid 30000 by
-- transaction 7, confirmed at height 800001, on a chain whose best block
-- is at 800003
testnet4Store :: FakeStore
testnet4Store =
  emptyStore
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800003 7],
      fakeBalances = [Store.Balance testnetExternalAddr1 30000 0 1 1 30000],
      fakeTxRefs = [(testnetExternalAddr1, [Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 7)])],
      fakeTransactions = [creditTx (fakeTxHash 7) (Store.BlockRef 800001 1) testnetExternalAddr1 30000],
      fakeUnspents = [coinOf (fakeTxHash 7) (Just 800001) testnetExternalAddr1 30000],
      fakeRawTransactions = [(fakeTxHash 7, fundingTx 7 [(testnetExternalAddr1, 30000)])]
    }

{- The portfolios of the test seed -}

-- | The first receiving address of the accounts of other portfolios of the
-- test seed, beside 'account1Addr' and 'account2Addr': that of the bip44
-- account of portfolios 10 and 11 on Bitcoin, m/44'/0'/10'/0/0 and
-- m/44'/0'/11'/0/0, of the bip84 account of portfolio 1 on Bitcoin,
-- m/84'/0'/1'/0/0, and of the account of portfolio 1 on Bitcoin Cash,
-- m/44'/145'/1'/0/0. Derived apart from hw with the Python tool of the
-- message-signing vectors, which gave back the addresses the fixtures
-- have first.
account10Addr, account11Addr, segwitAccount1Addr, bchAccount1Addr :: Address
account10Addr = testAddress "18ccscEdR1rVwmHtDuMVhARsP2iFMT9huo"
account11Addr = testAddress "1CiqZ2WTvRdFm4ZAVyNC6L2KLLE6f2UDzP"
segwitAccount1Addr = testAddress "bc1qcrfx4ynj3g2hxhft27pweevdak3600hy0d5dx3"
bchAccount1Addr = fromMaybe (error "bchAccount1Addr") (textToAddr bch "bitcoincash:qzmlqwm7ddxtz9gftwnnpjnxzykzagj0zvp947rlyx")

{- Reading the database back -}

-- | The bip44 account of a pool
bip44Id :: Pool -> ExceptT String (DB IO) DBAccountId
bip44Id pool = fst <$> poolAccountOf pool Bip44

-- | The accounts of a pool
accountIds :: Pool -> ExceptT String (DB IO) [DBAccountId]
accountIds pool = map fst <$> lift (poolAccounts pool)

-- | The summaries of the pool: transaction id, block and amount, in the
-- order of the transaction ids
summaries :: Pool -> ExceptT String (DB IO) [(Maybe TxHash, Store.BlockRef, Integer)]
summaries pool = do
  txInfos <- txsAfter pool Nothing Nothing
  return $ sortOn (\(h, _, _) -> txHashToHex <$> h) [(txInfoHash t, chainBlock (chainOf t), summaryAmount (txInfoSummary t)) | t <- txInfos]

-- | Each transaction txs lists, newest first, with the balance after it
listedBalances :: Pool -> ExceptT String (DB IO) [(Maybe TxHash, Maybe Integer)]
listedBalances pool = do
  rows <- listedTxsAfter UsualOrder pool Nothing Nothing
  return [(txInfoHash (listedTxInfo listed), listedTxBalance listed) | (listed, _) <- rows]

-- | The dead summaries of the pool, by transaction id
deadSummaries :: Pool -> ExceptT String (DB IO) [Maybe TxHash]
deadSummaries pool = do
  txInfos <- txsAfter pool Nothing Nothing
  return $ sortOn (fmap txHashToHex) [txInfoHash t | t <- txInfos, chainDead (chainOf t)]

-- | The addresses of ours each summary pays, as sorted texts, in the order
-- of the transaction ids
paidAddresses :: Pool -> ExceptT String (DB IO) [[Text]]
paidAddresses pool = do
  txInfos <- txsAfter pool Nothing Nothing
  return
    [ sort (mapMaybe (addrToText btc) (Map.keys (summaryMyOutputs (txInfoSummary t))))
      | t <- sortOn (fmap txHashToHex . txInfoHash) txInfos
    ]

-- | The summary of a transaction of the pool
summaryOf :: Pool -> TxHash -> ExceptT String (DB IO) TxSummary
summaryOf pool tid = do
  txInfos <- txsAfter pool Nothing Nothing
  txInfoSummary <$> liftMaybe "The transaction has no summary" (find ((== Just tid) . txInfoHash) txInfos)

-- | The coins of the pool, those of every account of it, in the order of
-- their outpoints
coins :: Pool -> ExceptT String (DB IO) [OutPoint]
coins pool = do
  accIds <- accountIds pool
  sortOn show . map jsonCoinOutpoint <$> coinsAfter pool accIds AllCoins Nothing Nothing

-- | The coins of one account, by its rows: each outpoint with its value,
-- in the order of the outpoints
coinsOfAccount :: DBAccountId -> ExceptT String (DB IO) [(OutPoint, Word64)]
coinsOfAccount accId = do
  rows <- lift $ P.selectList [DBCoinAccount P.==. accId] []
  held <- liftEither $ mapM (\(P.Entity _ row) -> (,) <$> coinOutpoint row <*> pure (dBCoinValue row)) rows
  return (sortOn (show . fst) held)

-- | What the pool holds, the balances of its accounts added up: confirmed,
-- unconfirmed, and the number of coins
balances :: Pool -> ExceptT String (DB IO) (Word64, Word64, Word64)
balances pool = do
  accounts <- lift $ poolAccounts pool
  return
    ( sum (dBAccountBalanceConfirmed . snd <$> accounts),
      sum (dBAccountBalanceUnconfirmed . snd <$> accounts),
      sum (dBAccountBalanceCoins . snd <$> accounts)
    )

-- | What one account holds: confirmed, unconfirmed, and the number of
-- coins
balancesOfAccount :: DBAccountId -> ExceptT String (DB IO) (Word64, Word64, Word64)
balancesOfAccount accId = do
  acc <- getAccountById accId
  return (dBAccountBalanceConfirmed acc, dBAccountBalanceUnconfirmed acc, dBAccountBalanceCoins acc)

-- | How many addresses an account of a pool has generated: receiving,
-- then change
addressCounts :: Pool -> AccountKind -> ExceptT String (DB IO) (Int, Int)
addressCounts pool kind = do
  (_, acc) <- poolAccountOf pool kind
  return (dBAccountExternal acc, dBAccountInternal acc)

-- | The balance of each address of an account that has transactions, by
-- the text of the address: confirmed, unconfirmed, and the number of coins
addressBalances :: DBAccountId -> ExceptT String (DB IO) [(Text, Word64, Word64, Word64)]
addressBalances accId = do
  rows <- lift $ P.selectList [DBAddressAccount P.==. accId] [P.Asc DBAddressInternal, P.Asc DBAddressIndex]
  return [(dBAddressAddress a, dBAddressBalanceConfirmed a, dBAddressBalanceUnconfirmed a, dBAddressBalanceCoins a) | P.Entity _ a <- rows, dBAddressBalanceTxs a > 0]

rawTransactionCount :: ExceptT String (DB IO) Int
rawTransactionCount = lift $ P.count ([] :: [P.Filter DBRawTx])

-- | The hashes of the signed pending transactions of the pool
pendingHashes :: Pool -> ExceptT String (DB IO) [TxHash]
pendingHashes pool = do
  rows <- pendingTxs pool
  return [txHash (txSignDataTx (pendingTxData p)) | p <- rows, isSigned (txSignDataTx (pendingTxData p))]

-- | The accounts of the database: the index of the portfolio of each, its
-- network and its purpose, in the order hw lists them
accountsHeld :: ExceptT String (DB IO) [(Natural, Text, Int)]
accountsHeld =
  map (\(portfolio, _, acc) -> (portfolioIndex portfolio.portfolioRow, dBAccountNetwork acc, dBAccountPurpose acc)) <$> lift getAccounts

-- | The twelve accounts of a whole portfolio, as 'accountsHeld' reads them
twelveOf :: Natural -> [(Natural, Text, Int)]
twelveOf portfolio =
  [ (portfolio, "btc", 44),
    (portfolio, "btc", 84),
    (portfolio, "bch", 44),
    (portfolio, "btctest", 44),
    (portfolio, "btctest", 84),
    (portfolio, "btctest4", 44),
    (portfolio, "btctest4", 84),
    (portfolio, "btcreg", 44),
    (portfolio, "btcreg", 84),
    (portfolio, "bchtest3", 44),
    (portfolio, "bchtest4", 44),
    (portfolio, "bchreg", 44)
  ]
