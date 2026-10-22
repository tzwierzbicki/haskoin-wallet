{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The synchronisation of an account, played against a scripted store:
-- payments arriving, confirming, being replaced or dropped, the chain
-- reorganising, a wallet with many coins or many addresses, and the raw
-- transactions fetched only when missing
module Haskoin.Wallet.SyncSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef, newIORef, readIORef)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.PendingTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo (TxInfo (..), TxInfoPending (..), txsPage)
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.Util (Page (Page))
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  let cfg = def :: Config
      -- An account with its first receiving address, externalAddr 0
      account = do
        (accId, _) <- testNewAcc ctx "main"
        _ <- genExtAddress ctx cfg accId ""
        return accId
      syncWith store accId = sync ctx cfg (toStore store) btc accId False

  describe "Syncing an account" $ do
    it "stores a received payment, its coin and the best block" $
      inDatabaseE $ do
        accId <- account
        res <- syncWith (paymentStore ctx) accId
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        lift (getBest btc) `dbShouldBeE` Just (headerHash (blockAt 800002 1).header, 800002)
        balances accId `dbShouldBeE` (50000, 0, 1)
        rawTransactionCount `dbShouldBeE` 1
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)

    it "does nothing when nothing changed" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        res <- syncWith (paymentStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (0, 0)
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]

    it "updates a payment when it confirms" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.MemRef 1700000000, 50000)]
        balances accId `dbShouldBeE` (0, 50000, 1)
        res <- syncWith (paymentStore ctx) accId
        liftIO $ syncResTxUpdates res `shouldBe` 1
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        balances accId `dbShouldBeE` (50000, 0, 1)

    it "finds an old payment to an address generated after newer transactions were synced" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        -- externalAddr 1 is generated now, but was paid long ago by someone
        -- deriving it from the public key
        _ <- genExtAddress ctx cfg accId ""
        _ <- syncWith (oldPaymentStore ctx) accId
        summaries ctx accId
          `dbShouldBeE` [ (Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000),
                        (Just (fakeTxHash 3), Store.BlockRef 800002 1, 20000),
                        (Just (fakeTxHash 4), Store.BlockRef 700000 1, 10000)
                      ]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0, OutPoint (fakeTxHash 4) 0]

    it "rebuilds a known payment when a new address of ours turns out to be in it" $
      inDatabaseE $ do
        accId <- account
        -- Transaction 5 paid externalAddr 0 and externalAddr 1, but only the
        -- first exists yet: the second output looks like somebody else's
        _ <- syncWith (payoutStore ctx) accId
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 5), Store.BlockRef 800000 1, 30000)]
        paidAddresses ctx accId `dbShouldBeE` [["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 5) 0]
        _ <- genExtAddress ctx cfg accId ""
        res <- syncWith (payoutSeenStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 5), Store.BlockRef 800000 1, 50000)]
        paidAddresses ctx accId `dbShouldBeE` [["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 5) 0, OutPoint (fakeTxHash 5) 1]
        balances accId `dbShouldBeE` (50000, 0, 2)

    it "marks a payment the store reports as replaced as dead, and keeps it" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        _ <- syncWith (replacedStore ctx) accId
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.MemRef 1700000000, 50000)]
        deadSummaries ctx accId `dbShouldBeE` [Just (fakeTxHash 1)]
        coins accId `dbShouldBeE` []
        balances accId `dbShouldBeE` (0, 0, 0)

    it "marks a payment the store no longer knows as dead" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        _ <- syncWith (forgottenStore ctx) accId
        deadSummaries ctx accId `dbShouldBeE` [Just (fakeTxHash 1)]
        coins accId `dbShouldBeE` []

    it "brings a dead payment back when the store lists it again" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        _ <- syncWith (replacedStore ctx) accId
        res <- syncWith (paymentStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        deadSummaries ctx accId `dbShouldBeE` []
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        balances accId `dbShouldBeE` (50000, 0, 1)

    it "moves a payment to its new block after a reorganisation that kept the balances" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        res <- syncWith (movedStore ctx) accId
        liftIO $ syncResTxUpdates res `shouldBe` 1
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800001 3, 50000)]
        lift (getBest btc) `dbShouldBeE` Just (headerHash (blockAt 800003 7).header, 800003)

    it "marks a payment a reorganisation left out as dead" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        _ <- syncWith (leftOutStore ctx) accId
        summaries ctx accId
          `dbShouldBeE` [ (Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000),
                          (Just (fakeTxHash 3), Store.BlockRef 800002 1, 20000)
                        ]
        deadSummaries ctx accId `dbShouldBeE` [Just (fakeTxHash 3)]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0]
        balances accId `dbShouldBeE` (30000, 0, 1)

    it "looks for a reorganisation on an incremental sync, not on a full one" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        let store = notingBlockRequests asked $ toStore $ paymentStore ctx
        -- The first sync has no block to check against, the full ones look
        -- at everything anyway
        _ <- sync ctx cfg store btc accId False
        _ <- sync ctx cfg store btc accId True
        _ <- sync ctx cfg store btc accId False
        _ <- sync ctx cfg store btc accId True
        liftIO $ readIORef asked `shouldReturn` [800002]

    it "keeps every coin of an address with two hundred of them" $
      inDatabaseE $ do
        accId <- account
        res <- syncWith (manyCoinsStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (200, 200)
        (length <$> coins accId) `dbShouldBeE` 200
        (length <$> summaries ctx accId) `dbShouldBeE` 200
        balances accId `dbShouldBeE` (200000, 0, 200)

    it "lists the transactions of more addresses than it asks about at a time" $
      inDatabaseE $ do
        accId <- account
        -- externalAddr 1 to externalAddr 6 as well
        replicateM_ 6 $ genExtAddress ctx cfg accId ""
        let twoAtATime = cfg {configParallelRequests = 2}
        res <- sync ctx twoAtATime (toStore (sevenAddressesStore ctx)) btc accId False
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (7, 7)
        summaries ctx accId
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 1000),
                          (Just (fakeTxHash 2), Store.BlockRef 800000 2, 1000),
                          (Just (fakeTxHash 3), Store.BlockRef 800000 3, 1000),
                          (Just (fakeTxHash 4), Store.BlockRef 800000 4, 1000),
                          (Just (fakeTxHash 5), Store.BlockRef 800000 5, 1000),
                          (Just (fakeTxHash 6), Store.BlockRef 800000 6, 1000),
                          (Just (fakeTxHash 7), Store.BlockRef 800000 7, 1000)
                        ]
        paidAddresses ctx accId
          `dbShouldBeE` [ ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
                          ["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"],
                          ["1Dg6Kg7kQuyiZz41HRWXKUWKRu6ZyEf1Nr"],
                          ["1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"],
                          ["1cWcYiGK7NwjPBJuKRqZxV4aymUnPu1mx"],
                          ["1MZuimSXigp8oqxkVUvZofqHNtVjdcdAqc"],
                          ["1JReTkpFnsrMqhSEJwUNZXPAyeTo2HQfnE"]
                        ]
        balances accId `dbShouldBeE` (7000, 0, 7)

    it "fetches the raw transaction of a coin once, not again on a full sync" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        let store = notingRawRequests asked $ toStore $ realPaymentStore ctx
        _ <- sync ctx cfg store btc accId False
        _ <- sync ctx cfg store btc accId True
        liftIO $ readIORef asked `shouldReturn` [[realPaymentId ctx]]
        rawTransactionCount `dbShouldBeE` 1

    it "fetches the raw transaction of a coin again when it is missing" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        let store = notingRawRequests asked $ toStore $ realPaymentStore ctx
        _ <- sync ctx cfg store btc accId False
        lift $ P.deleteWhere ([] :: [P.Filter DBRawTx])
        _ <- sync ctx cfg store btc accId True
        liftIO $ readIORef asked `shouldReturn` [[realPaymentId ctx], [realPaymentId ctx]]
        rawTransactionCount `dbShouldBeE` 1

    it "refuses a store that does not return a raw transaction asked for" $
      inDatabaseE $ do
        accId <- account
        result <- lift $ runExceptT $ syncWith (noRawStore ctx) accId
        liftIO $ result `shouldBe` Left "The store was asked for 1 raw transactions but returned 0"

    it "refuses a store that lists fewer coins than it counts" $
      inDatabaseE $ do
        accId <- account
        result <- lift $ runExceptT $ syncWith (fewerCoinsStore ctx) accId
        liftIO $ result `shouldBe` Left "The store counts 2 coins for the addresses to update but listed 1"

    it "refuses a store that lists fewer transactions than it counts" $
      inDatabaseE $ do
        accId <- account
        result <- lift $ runExceptT $ syncWith (fewerTxsStore ctx) accId
        liftIO $
          result
            `shouldBe` Left "The store counts 2 transactions for 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj but listed 1"

    it "retires a pending transaction seen online" $
      inDatabaseE $ do
        accId <- account
        lift $ P.insert_ $ pendingRow accId (pendingTx ctx)
        pendingHashes accId `dbShouldBeE` [txHash (pendingTx ctx)]
        res <- syncWith (sentStore ctx) accId
        pendingHashes accId `dbShouldBeE` []
        liftIO $ syncResRebroadcast res `shouldBe` 0

    it "sends again a transaction the user sent that the store lost" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        -- The user sent the payment to otherAddr 0 but the store does not
        -- list it: the coin is still unspent
        pendingHash <- importPendingTx btc ctx accId (signedSpend ctx)
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        published <- liftIO $ newIORef []
        res <- sync ctx cfg (notingPublished published $ toStore $ realPaymentStore ctx) btc accId False
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (1, 0)
        liftIO $ readIORef published `shouldReturn` [txSignDataTx (signedSpend ctx)]
        pendingHashes accId `dbShouldBeE` [txHash (txSignDataTx (signedSpend ctx))]
        ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just (TxInfoPending pendingHash True True False)]

    it "leaves alone a signed transaction the user has not sent" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        _ <- importPendingTx btc ctx accId (signedSpend ctx)
        published <- liftIO $ newIORef []
        res <- sync ctx cfg (notingPublished published $ toStore $ realPaymentStore ctx) btc accId False
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 0)
        liftIO $ readIORef published `shouldReturn` []

    it "reports a pending transaction whose coin was spent by another transaction" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        pendingHash <- importPendingTx btc ctx accId (unsignedSpend ctx)
        coins accId `dbShouldBeE` [OutPoint (realPaymentId ctx) 0]
        res <- syncWith (spentElsewhereStore ctx) accId
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 1)
        coins accId `dbShouldBeE` []
        ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just (TxInfoPending pendingHash False False True)]
        -- Nothing is left to free but the change address, which it had none of
        deleteTx ctx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (0, 0)
        ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0)) `dbShouldBeE` []

  describe "Asking the store in parallel" $ do
    it "answers in the order asked, even when later questions finish first" $ do
      -- Question 1 takes 25 milliseconds, question 5 takes 5
      let question n = liftIO (threadDelay ((6 - n) * 5000)) >> return n
      answers <- runExceptT $ inParallel 3 [question n | n <- [1 .. 5 :: Int]]
      answers `shouldBe` Right [1, 2, 3, 4, 5]

    it "asks at most the given number of questions at a time" $ do
      running <- newIORef (0 :: Int)
      most <- newIORef (0 :: Int)
      let question = liftIO $ do
            now <- atomicModifyIORef' running $ \n -> (n + 1, n + 1)
            atomicModifyIORef' most $ \m -> (max m now, ())
            threadDelay 20000
            atomicModifyIORef' running $ \n -> (n - 1, ())
      _ <- runExceptT $ inParallel 3 $ replicate 7 question
      readIORef most `shouldReturn` 3

    it "fails with the first failed question" $ do
      answers <-
        runExceptT $
          inParallel 2 [return 1, throwError "The store is down", throwError "Not this one", return (4 :: Int)]
      answers `shouldBe` Left "The store is down"

{- The stores -}

-- | The coin of a payment: output 0 of the transaction, at the given block
coinOf :: Ctx -> TxHash -> Maybe Natural -> Address -> Natural -> Store.Unspent
coinOf ctx tid = coinAt ctx tid 0

-- | The coin of an output of a transaction, at the given block
coinAt :: Ctx -> TxHash -> Word32 -> Maybe Natural -> Address -> Natural -> Store.Unspent
coinAt ctx tid index = storeCoin ctx (tid, index)

-- | externalAddr 0 paid 50000 by transaction 1, confirmed at height 800000
paymentStore :: Ctx -> FakeStore
paymentStore ctx =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 1 50000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)])],
      fakeTransactions = [creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000],
      fakeUnspents = [coinOf ctx (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000],
      fakeRawTransactions = [(fakeTxHash 1, fundingTx ctx 1 [(externalAddr 0, 50000)])]
    }

-- | The same payment, still in the mempool
mempoolStore :: Ctx -> FakeStore
mempoolStore ctx =
  (paymentStore ctx)
    { fakeBalances = [Store.Balance (externalAddr 0) 0 50000 1 1 50000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.MemRef 1700000000) (fakeTxHash 1)])],
      fakeTransactions = [creditTx ctx (fakeTxHash 1) (Store.MemRef 1700000000) (externalAddr 0) 50000],
      fakeUnspents = [coinOf ctx (fakeTxHash 1) Nothing (externalAddr 0) 50000]
    }

-- | The payment replaced: the store still has it, marked as deleted, and
-- the address is back to nothing
replacedStore :: Ctx -> FakeStore
replacedStore ctx =
  (paymentStore ctx)
    { fakeBalances = [],
      fakeTxRefs = [],
      fakeTransactions = [replaced $ creditTx ctx (fakeTxHash 1) (Store.MemRef 1700000000) (externalAddr 0) 50000],
      fakeUnspents = []
    }

-- | The payment dropped from the mempool and forgotten by the store
forgottenStore :: Ctx -> FakeStore
forgottenStore ctx =
  (replacedStore ctx) {fakeTransactions = []}

-- | externalAddr 0 paid 30000 by transaction 2 at height 800001 and 20000
-- by transaction 3 at height 800002
twoPaymentsStore :: Ctx -> FakeStore
twoPaymentsStore ctx =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeBalances = [Store.Balance (externalAddr 0) 50000 0 2 2 50000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 3),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        [ creditTx ctx (fakeTxHash 2) (Store.BlockRef 800001 1) (externalAddr 0) 30000,
          creditTx ctx (fakeTxHash 3) (Store.BlockRef 800002 1) (externalAddr 0) 20000
        ],
      fakeUnspents =
        [ coinOf ctx (fakeTxHash 2) (Just 800001) (externalAddr 0) 30000,
          coinOf ctx (fakeTxHash 3) (Just 800002) (externalAddr 0) 20000
        ],
      fakeRawTransactions =
        [ (fakeTxHash 2, fundingTx ctx 2 [(externalAddr 0, 30000)]),
          (fakeTxHash 3, fundingTx ctx 3 [(externalAddr 0, 20000)])
        ]
    }

-- | The two payments, and externalAddr 1 paid 10000 by transaction 4 long
-- ago, at height 700000
oldPaymentStore :: Ctx -> FakeStore
oldPaymentStore ctx =
  (twoPaymentsStore ctx)
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 50000 0 2 2 50000,
          Store.Balance (externalAddr 1) 10000 0 1 1 10000
        ],
      fakeTxRefs =
        fakeTxRefs (twoPaymentsStore ctx)
          <> [(externalAddr 1, [Store.TxRef (Store.BlockRef 700000 1) (fakeTxHash 4)])],
      fakeTransactions =
        fakeTransactions (twoPaymentsStore ctx)
          <> [creditTx ctx (fakeTxHash 4) (Store.BlockRef 700000 1) (externalAddr 1) 10000],
      fakeUnspents =
        fakeUnspents (twoPaymentsStore ctx)
          <> [coinOf ctx (fakeTxHash 4) (Just 700000) (externalAddr 1) 10000],
      fakeRawTransactions =
        fakeRawTransactions (twoPaymentsStore ctx)
          <> [(fakeTxHash 4, fundingTx ctx 4 [(externalAddr 1, 10000)])]
    }

-- | Transaction 5 paid 30000 to externalAddr 0 and 20000 to externalAddr 1,
-- confirmed at height 800000, while the wallet knows externalAddr 0 only
payoutStore :: Ctx -> FakeStore
payoutStore ctx =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeBalances = [Store.Balance (externalAddr 0) 30000 0 1 1 30000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 5)])],
      fakeTransactions = [payoutTx ctx (fakeTxHash 5) (Store.BlockRef 800000 1) [(externalAddr 0, 30000), (externalAddr 1, 20000)]],
      fakeUnspents = [coinAt ctx (fakeTxHash 5) 0 (Just 800000) (externalAddr 0) 30000],
      fakeRawTransactions = [(fakeTxHash 5, fundingTx ctx 5 [(externalAddr 0, 30000), (externalAddr 1, 20000)])]
    }

-- | The same store once the wallet asks about externalAddr 1 as well
payoutSeenStore :: Ctx -> FakeStore
payoutSeenStore ctx =
  (payoutStore ctx)
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 30000 0 1 1 30000,
          Store.Balance (externalAddr 1) 20000 0 1 1 20000
        ],
      fakeTxRefs =
        [ (externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 5)]),
          (externalAddr 1, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 5)])
        ],
      fakeUnspents =
        [ coinAt ctx (fakeTxHash 5) 0 (Just 800000) (externalAddr 0) 30000,
          coinAt ctx (fakeTxHash 5) 1 (Just 800000) (externalAddr 1) 20000
        ]
    }

-- | After a reorganisation: the block at height 800002 is another one, the
-- chain is one block longer, and transaction 1 sits in block 800001 at
-- position 3 with the balances unchanged
movedStore :: Ctx -> FakeStore
movedStore ctx =
  (paymentStore ctx)
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800001 3) (fakeTxHash 1)])],
      fakeTransactions = [creditTx ctx (fakeTxHash 1) (Store.BlockRef 800001 3) (externalAddr 0) 50000],
      fakeUnspents = [coinOf ctx (fakeTxHash 1) (Just 800001) (externalAddr 0) 50000]
    }

-- | After a reorganisation that left transaction 3 out of the chain
leftOutStore :: Ctx -> FakeStore
leftOutStore ctx =
  (twoPaymentsStore ctx)
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeBalances = [Store.Balance (externalAddr 0) 30000 0 1 1 30000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)])],
      fakeTransactions = [creditTx ctx (fakeTxHash 2) (Store.BlockRef 800001 1) (externalAddr 0) 30000],
      fakeUnspents = [coinOf ctx (fakeTxHash 2) (Just 800001) (externalAddr 0) 30000]
    }

-- | externalAddr 0 paid 1000 by each of two hundred transactions, all in
-- block 800000
manyCoinsStore :: Ctx -> FakeStore
manyCoinsStore ctx =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeBalances = [Store.Balance (externalAddr 0) 200000 0 200 200 200000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 (fromIntegral i)) (fakeTxHash i) | i <- indexes])],
      fakeTransactions = [creditTx ctx (fakeTxHash i) (Store.BlockRef 800000 (fromIntegral i)) (externalAddr 0) 1000 | i <- indexes],
      fakeUnspents = [coinOf ctx (fakeTxHash i) (Just 800000) (externalAddr 0) 1000 | i <- indexes],
      fakeRawTransactions = [(fakeTxHash i, fundingTx ctx i [(externalAddr 0, 1000)]) | i <- indexes]
    }
  where
    indexes = [1 .. 200] :: [Word8]

-- | externalAddr 0 to externalAddr 6 each paid 1000, by transactions 1 to
-- 7 in block 800000: more addresses to list than the sync asks about at a
-- time
sevenAddressesStore :: Ctx -> FakeStore
sevenAddressesStore ctx =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeBalances = [Store.Balance addr 1000 0 1 1 1000 | (_, addr) <- payments],
      fakeTxRefs = [(addr, [Store.TxRef (Store.BlockRef 800000 (fromIntegral i)) (fakeTxHash i)]) | (i, addr) <- payments],
      fakeTransactions = [creditTx ctx (fakeTxHash i) (Store.BlockRef 800000 (fromIntegral i)) addr 1000 | (i, addr) <- payments],
      fakeUnspents = [coinOf ctx (fakeTxHash i) (Just 800000) addr 1000 | (i, addr) <- payments],
      fakeRawTransactions = [(fakeTxHash i, fundingTx ctx i [(addr, 1000)]) | (i, addr) <- payments]
    }
  where
    -- Transaction i paid externalAddr (i - 1)
    payments = [(i, externalAddr (fromIntegral i - 1)) | i <- [1 .. 7 :: Word8]]

-- | The raw transaction of the payment of 50000 to externalAddr 0, and
-- its hash: the id a real store lists the payment under
realPaymentTx :: Ctx -> Tx
realPaymentTx ctx = fundingTx ctx 1 [(externalAddr 0, 50000)]

realPaymentId :: Ctx -> TxHash
realPaymentId = txHash . realPaymentTx

-- | The payment listed under the hash of its raw transaction, as a real
-- store lists it: the sync can then tell that it has the raw transaction
realPaymentStore :: Ctx -> FakeStore
realPaymentStore ctx =
  (paymentStore ctx)
    { fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) tid])],
      fakeTransactions = [creditTx ctx tid (Store.BlockRef 800000 1) (externalAddr 0) 50000],
      fakeUnspents = [coinOf ctx tid (Just 800000) (externalAddr 0) 50000],
      fakeRawTransactions = [(tid, realPaymentTx ctx)]
    }
  where
    tid = realPaymentId ctx

-- | A store that lists the coin of the payment but has no raw transaction
-- for it
noRawStore :: Ctx -> FakeStore
noRawStore ctx = (paymentStore ctx) {fakeRawTransactions = []}

-- | A store that counts two coins for the address but lists one
fewerCoinsStore :: Ctx -> FakeStore
fewerCoinsStore ctx =
  (paymentStore ctx) {fakeBalances = [Store.Balance (externalAddr 0) 50000 0 2 1 50000]}

-- | A store that counts two transactions for the address but lists one
fewerTxsStore :: Ctx -> FakeStore
fewerTxsStore ctx =
  (paymentStore ctx) {fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 2 50000]}

-- | A transaction of ours, signed and waiting to be sent: it pays
-- externalAddr 0 from a fake coin
pendingTx :: Ctx -> Tx
pendingTx ctx = spendingTx ctx [(fakeTxHash 9, 0)] [(externalAddr 0, 50000)]

-- | The coin of 'realPaymentStore' spent to otherAddr 0, with its signing
-- data as the wallet keeps it, marked signed
signedSpend :: Ctx -> TxSignData
signedSpend ctx = (unsignedSpend ctx) {txSignDataSigned = True}

unsignedSpend :: Ctx -> TxSignData
unsignedSpend ctx =
  TxSignData
    (spendingTx ctx [(realPaymentId ctx, 0)] [(otherAddr 0, 40000)])
    [realPaymentTx ctx]
    [Deriv :/ 0 :/ 0]
    []
    (Just 10)
    False

-- | The payment of 'realPaymentStore' spent by somebody else's transaction
-- 8, confirmed at height 800001: the coin is gone
spentElsewhereStore :: Ctx -> FakeStore
spentElsewhereStore ctx =
  (realPaymentStore ctx)
    { fakeBalances = [Store.Balance (externalAddr 0) 0 0 0 2 50000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 8),
              Store.TxRef (Store.BlockRef 800000 1) (realPaymentId ctx)
            ]
          )
        ],
      fakeTransactions =
        [ creditTx ctx (realPaymentId ctx) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          spendingStoreTx ctx (fakeTxHash 8) (Store.BlockRef 800001 1) (realPaymentId ctx, 0) (externalAddr 0) (otherAddr 1) 49000
        ],
      fakeUnspents = []
    }

-- | A transaction of the store spending one coin of ours to another address
spendingStoreTx :: Ctx -> TxHash -> Store.BlockRef -> (TxHash, Word32) -> Address -> Address -> Word64 -> Store.Transaction
spendingStoreTx ctx tid block (spentHash, spentIndex) from to amount =
  Store.Transaction
    { Store.block = block,
      Store.version = 1,
      Store.locktime = 0,
      Store.inputs = [Store.StoreInput (OutPoint spentHash spentIndex) 0xffffffff BS.empty (addressScript ctx from) (amount + 1000) [] (Just from)],
      Store.outputs = [Store.StoreOutput amount (addressScript ctx to) Nothing (Just to)],
      Store.deleted = False,
      Store.rbf = False,
      Store.timestamp = 1700000000,
      Store.txid = tid,
      Store.size = 226,
      Store.weight = 904,
      Store.fee = 1000
    }

pendingRow :: DBAccountId -> Tx -> DBPendingTx
pendingRow (DBAccountKey wallet accDeriv) tx =
  DBPendingTx
    { dBPendingTxAccountWallet = wallet,
      dBPendingTxAccountDerivation = accDeriv,
      dBPendingTxNosigHash = txHashToHex (nosigHash tx),
      dBPendingTxBlob = BS.toStrict $ Json.encode $ TxSignData tx [] [] [] Nothing True,
      dBPendingTxOnline = False,
      dBPendingTxCreated = fixtureTime
    }

-- | The pending transaction has reached the store
sentStore :: Ctx -> FakeStore
sentStore ctx =
  (mempoolStore ctx)
    { fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.MemRef 1700000000) sent])],
      fakeTransactions = [creditTx ctx sent (Store.MemRef 1700000000) (externalAddr 0) 50000],
      fakeUnspents = [coinOf ctx sent Nothing (externalAddr 0) 50000],
      fakeRawTransactions = [(sent, pendingTx ctx)]
    }
  where
    sent = txHash (pendingTx ctx)

{- Reading the database back -}

-- | The summaries of the account: transaction id, block and amount, in the
-- order of the transaction ids
summaries :: Ctx -> DBAccountId -> ExceptT String (DB IO) [(Maybe TxHash, Store.BlockRef, Integer)]
summaries ctx accId = do
  txInfos <- txsPage ctx accId (Page 500 0)
  return $ sortOn (\(h, _, _) -> txHashToHex <$> h) [(txInfoHash t, txInfoBlockRef t, txInfoAmount t) | t <- txInfos]

-- | The dead summaries of the account, by transaction id
deadSummaries :: Ctx -> DBAccountId -> ExceptT String (DB IO) [Maybe TxHash]
deadSummaries ctx accId = do
  txInfos <- txsPage ctx accId (Page 500 0)
  return $ sortOn (fmap txHashToHex) [txInfoHash t | t <- txInfos, txInfoDead t]

-- | The addresses of ours each summary pays, as sorted texts, in the order
-- of the transaction ids
paidAddresses :: Ctx -> DBAccountId -> ExceptT String (DB IO) [[Text]]
paidAddresses ctx accId = do
  txInfos <- txsPage ctx accId (Page 500 0)
  return
    [ sort (mapMaybe (addrToText btc) (Map.keys (txInfoMyOutputs t)))
      | t <- sortOn (fmap txHashToHex . txInfoHash) txInfos
    ]

-- | The coins of the account, in the order of their outpoints
coins :: DBAccountId -> ExceptT String (DB IO) [OutPoint]
coins accId = sortOn show . map jsonCoinOutpoint <$> coinPage btc accId (Page 500 0)

-- | The confirmed and unconfirmed balances of the account, and its number
-- of coins
balances :: DBAccountId -> ExceptT String (DB IO) (Word64, Word64, Word64)
balances accId = do
  acc <- getAccountById accId
  return (dBAccountBalanceConfirmed acc, dBAccountBalanceUnconfirmed acc, dBAccountBalanceCoins acc)

rawTransactionCount :: ExceptT String (DB IO) Int
rawTransactionCount = lift $ P.count ([] :: [P.Filter DBRawTx])

-- | The hashes of the signed pending transactions of the account
pendingHashes :: DBAccountId -> ExceptT String (DB IO) [TxHash]
pendingHashes accId = do
  pending <- pendingTxs accId
  return [txHash (txSignDataTx (pendingTxData p)) | p <- pending, txSignDataSigned (pendingTxData p)]
