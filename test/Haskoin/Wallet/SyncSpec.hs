{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The synchronisation of an account, played against a scripted store:
-- payments arriving, confirming, being replaced or dropped, the chain
-- reorganising, and a wallet with many coins
module Haskoin.Wallet.SyncSpec (spec) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64, Word8)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO (TxSignData (..))
import Haskoin.Wallet.Sync
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo (TxInfo (..))
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
      syncWith store accId = sync ctx (toStore store) btc accId False

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

    it "drops a payment the store reports as replaced" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        _ <- syncWith (replacedStore ctx) accId
        summaries ctx accId `dbShouldBeE` []
        coins accId `dbShouldBeE` []
        balances accId `dbShouldBeE` (0, 0, 0)

    it "drops a payment the store no longer knows" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        _ <- syncWith (forgottenStore ctx) accId
        summaries ctx accId `dbShouldBeE` []
        coins accId `dbShouldBeE` []

    it "moves a payment to its new block after a reorganisation that kept the balances" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        res <- syncWith (movedStore ctx) accId
        liftIO $ syncResTxUpdates res `shouldBe` 1
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800001 3, 50000)]
        lift (getBest btc) `dbShouldBeE` Just (headerHash (blockAt 800003 7).header, 800003)

    it "drops a payment a reorganisation left out" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        _ <- syncWith (leftOutStore ctx) accId
        summaries ctx accId `dbShouldBeE` [(Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000)]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0]
        balances accId `dbShouldBeE` (30000, 0, 1)

    it "keeps every coin of an address with two hundred of them" $
      inDatabaseE $ do
        accId <- account
        res <- syncWith (manyCoinsStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (200, 200)
        (length <$> coins accId) `dbShouldBeE` 200
        (length <$> summaries ctx accId) `dbShouldBeE` 200
        balances accId `dbShouldBeE` (200000, 0, 200)

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
        (map fst <$> pendingTxHashes accId) `dbShouldBeE` [txHash (pendingTx ctx)]
        _ <- syncWith (sentStore ctx) accId
        (map fst <$> pendingTxHashes accId) `dbShouldBeE` []

{- The scripted store -}

-- | What the store answers: every list is complete, as the real store's
-- are when asked for everything
data FakeStore = FakeStore
  { fakeBest :: Store.BlockData,
    -- | The blocks at each height, for the reorganisation check
    fakeBlocks :: [Store.BlockData],
    -- | Addresses not listed have an empty balance
    fakeBalances :: [Store.Balance],
    -- | The transactions of each address, newest first
    fakeTxRefs :: [(Address, [Store.TxRef])],
    fakeTransactions :: [Store.Transaction],
    fakeUnspents :: [Store.Unspent],
    -- | Raw transactions by the id the store knows them under
    fakeRawTransactions :: [(TxHash, Tx)]
  }

toStore :: FakeStore -> Store
toStore fake =
  Store
    { storeHealth = return (),
      storeBestBlock = return fake.fakeBest,
      storeBlocksAtHeight = \height ->
        return $ filter ((== height) . (.height)) fake.fakeBlocks,
      storeBalances = \addrs ->
        return [fromMaybe (Store.Balance a 0 0 0 0 0) (find ((== a) . (.address)) fake.fakeBalances) | a <- addrs],
      storeAddressTxs = \addr count ->
        return $ take (fromIntegral count) $ fromMaybe [] $ lookup addr fake.fakeTxRefs,
      storeTransactions = \tids ->
        return $ filter ((`elem` tids) . (.txid)) fake.fakeTransactions,
      storeUnspents = \addrs ->
        return $ filter ((`elem` (Just <$> addrs)) . (.address)) fake.fakeUnspents,
      storeRawTransactions = \tids ->
        return [tx | (tid, tx) <- fake.fakeRawTransactions, tid `elem` tids]
    }

-- | A block of the main chain, whose hash depends on the nonce
blockAt :: BlockHeight -> Word32 -> Store.BlockData
blockAt height nonce =
  Store.BlockData
    height
    True
    0
    (BlockHeader 1 (fakeBlockHash 0) (fakeTxHash 0).get 1700000000 0x1d00ffff nonce)
    1000
    4000
    []
    0
    0
    0

-- | A transaction of the store paying an amount to one of our addresses,
-- funded by somebody else, with a fee of 1000
creditTx :: Ctx -> TxHash -> Store.BlockRef -> Address -> Word64 -> Store.Transaction
creditTx ctx tid block addr amount =
  Store.Transaction
    { Store.block = block,
      Store.version = 1,
      Store.locktime = 0,
      Store.inputs =
        [ Store.StoreInput
            (OutPoint (fakeTxHash 0xee) 0)
            0xffffffff
            BS.empty
            (addressScript ctx (otherAddr 0))
            (amount + 1000)
            []
            (Just (otherAddr 0))
        ],
      Store.outputs = [Store.StoreOutput amount (addressScript ctx addr) Nothing (Just addr)],
      Store.deleted = False,
      Store.rbf = False,
      Store.timestamp = 1700000000,
      Store.txid = tid,
      Store.size = 226,
      Store.weight = 904,
      Store.fee = 1000
    }

-- | The same transaction, replaced or dropped: the store keeps it, marked
-- as deleted
replaced :: Store.Transaction -> Store.Transaction
replaced (Store.Transaction b v l i o _ r t x s w f) =
  Store.Transaction b v l i o True r t x s w f

-- | The coin of a payment: output 0 of the transaction, at the given block
coinOf :: Ctx -> TxHash -> Maybe Natural -> Address -> Natural -> Store.Unspent
coinOf ctx tid heightM = storeCoin ctx (tid, 0) heightM

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
      fakeRawTransactions = []
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
          <> [coinOf ctx (fakeTxHash 4) (Just 700000) (externalAddr 1) 10000]
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
      fakeRawTransactions = []
    }
  where
    indexes = [1 .. 200] :: [Word8]

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

pendingRow :: DBAccountId -> Tx -> DBPendingTx
pendingRow (DBAccountKey wallet accDeriv) tx =
  DBPendingTx
    { dBPendingTxAccountWallet = wallet,
      dBPendingTxAccountDerivation = accDeriv,
      dBPendingTxNosigHash = txHashToHex (nosigTxHash tx),
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
      fakeUnspents = [coinOf ctx sent Nothing (externalAddr 0) 50000]
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
