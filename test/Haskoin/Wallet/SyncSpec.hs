{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The synchronisation of an account, played against a scripted store:
-- payments arriving, confirming, being replaced or dropped, a payment the
-- wallet made, the chain reorganising, a wallet with many coins or many
-- addresses, the raw transactions fetched only when missing and dropped
-- when no coin needs them, pending transactions, two accounts, another
-- network; the gap of addresses watched past the ones the account has; the
-- scan of a chain and the recovery of the accounts of a seed; and the decisions of a
-- sync on their own: what to fetch, what is dead, which balances changed
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
import Data.Maybe (mapMaybe)
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Data.Text (Text)
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
import Haskoin.Wallet.Database.RawTx (hasRawTx, insertRawTx, lookupRawTx)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.SyncStatus
import Haskoin.Wallet.Keys (AccountKind (..), ForeignKind (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo (ChainStatus (..), ListedTx (..), MyOutputs (..), TxInfo (..), TxSummary (..), TxType (..), listedTxsAfter, markTxInfoDead, repsertSummary, txInfoHash, txsAfter)
import Haskoin.Wallet.Database.TxLabel (setTxLabel, txLabel)
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
spec = prepareContext $ \ctx -> do
  let cfg = def :: Config
      -- A store that reports every address used, as one that lies does
      everyAddressUsed = (toStore emptyStore) {storeBalances = \addrs -> return [Store.Balance addr 1000 0 1 1 1000 | addr <- addrs]}
      -- An account with its first receiving address, externalAddr 0
      account = do
        (accId, _) <- testNewAcc ctx "main"
        _ <- genExtAddress ctx cfg accId ""
        return accId
      syncWith store accId = sync ctx cfg (toStore store) btc accId False noQuestion

  describe "Syncing an account" $ do
    it "stores a received payment, its coin and the best block" $
      inDatabaseE $ do
        accId <- account
        res <- syncWith (paymentStore ctx) accId
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        getBest btc `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800002 1).header) 800002 (Just 1700000000))
        balances accId `dbShouldBeE` (50000, 0, 1)
        rawTransactionCount `dbShouldBeE` 1
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)

    it "stores the best block of the network again over one that does not read, on any sync" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        lift $ P.update (DBBestKey "btc") [DBBestBestBlock P.=. "nonsense"]
        _ <- syncWith (paymentStore ctx) accId
        getBest btc `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800002 1).header) 800002 (Just 1700000000))

    it "does nothing when nothing changed" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        res <- syncWith (paymentStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (0, 0)
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]

    it "stores the time the store gives each transaction" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        (map (chainTime . chainOf) <$> txsAfter accId Nothing Nothing) `dbShouldBeE` [Just seenTime]

    -- As the step to version 4 of the database left it: the balances are
    -- unchanged, and no address is listed
    it "fetches again a transaction whose summary has no time, and stores its time" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        lift $ P.update (DBTxInfoKey accId (hashBytes (fakeTxHash 1))) [DBTxInfoTime P.=. Nothing]
        res <- syncWith (paymentStore ctx) accId
        liftIO $ syncResTxUpdates res `shouldBe` 1
        (map (chainTime . chainOf) <$> txsAfter accId Nothing Nothing) `dbShouldBeE` [Just seenTime]

    it "reports what the account can spend once synced" $
      inDatabaseE $ do
        accId <- account
        res <- syncWith (realPaymentStore ctx) accId
        liftIO $ syncResSpendable res `shouldBe` 50000

    it "updates a payment when it confirms" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.MemRef 1700000000, 50000)]
        balances accId `dbShouldBeE` (0, 50000, 1)
        res <- syncWith (paymentStore ctx) accId
        liftIO $ syncResTxUpdates res `shouldBe` 1
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        balances accId `dbShouldBeE` (50000, 0, 1)

    it "finds an old payment to an address generated after newer transactions were synced" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        -- externalAddr 1 is generated now, but was paid long ago by someone
        -- deriving it from the public key
        _ <- genExtAddress ctx cfg accId ""
        _ <- syncWith (oldPaymentStore ctx) accId
        summaries accId
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
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 5), Store.BlockRef 800000 1, 30000)]
        paidAddresses accId `dbShouldBeE` [["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 5) 0]
        _ <- genExtAddress ctx cfg accId ""
        res <- syncWith (payoutSeenStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 5), Store.BlockRef 800000 1, 50000)]
        paidAddresses accId `dbShouldBeE` [["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 5) 0, OutPoint (fakeTxHash 5) 1]
        balances accId `dbShouldBeE` (50000, 0, 2)

    it "marks a payment the store reports as replaced as dead, and keeps it" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        _ <- syncWith (replacedStore ctx) accId
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.MemRef 1700000000, 50000)]
        deadSummaries accId `dbShouldBeE` [Just (fakeTxHash 1)]
        coins accId `dbShouldBeE` []
        balances accId `dbShouldBeE` (0, 0, 0)

    it "marks a payment the store no longer knows as dead" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        _ <- syncWith (forgottenStore ctx) accId
        deadSummaries accId `dbShouldBeE` [Just (fakeTxHash 1)]
        coins accId `dbShouldBeE` []

    it "brings a dead payment back when the store lists it again" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (mempoolStore ctx) accId
        _ <- syncWith (replacedStore ctx) accId
        res <- syncWith (paymentStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        deadSummaries accId `dbShouldBeE` []
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        balances accId `dbShouldBeE` (50000, 0, 1)

    it "moves a payment to its new block after a reorganisation that kept the balances" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        res <- syncWith (movedStore ctx) accId
        liftIO $ syncResTxUpdates res `shouldBe` 1
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800001 3, 50000)]
        getBest btc `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800003 7).header) 800003 (Just 1700000000))

    it "marks a payment a reorganisation left out as dead" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        _ <- syncWith (leftOutStore ctx) accId
        summaries accId
          `dbShouldBeE` [ (Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000),
                          (Just (fakeTxHash 3), Store.BlockRef 800002 1, 20000)
                        ]
        deadSummaries accId `dbShouldBeE` [Just (fakeTxHash 3)]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0]
        balances accId `dbShouldBeE` (30000, 0, 1)

    it "looks for a reorganisation on an incremental sync, not on a full one" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        let store = notingBlockRequests asked $ toStore $ paymentStore ctx
        -- The first sync has no block to check against, the full ones look
        -- at everything anyway
        _ <- sync ctx cfg store btc accId False noQuestion
        _ <- sync ctx cfg store btc accId True noQuestion
        _ <- sync ctx cfg store btc accId False noQuestion
        _ <- sync ctx cfg store btc accId True noQuestion
        liftIO $ readIORef asked `shouldReturn` [800002]

    it "keeps every coin of an address with two hundred of them" $
      inDatabaseE $ do
        accId <- account
        res <- syncWith (manyCoinsStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (200, 200)
        (length <$> coins accId) `dbShouldBeE` 200
        (length <$> summaries accId) `dbShouldBeE` 200
        balances accId `dbShouldBeE` (200000, 0, 200)

    it "lists the transactions of more addresses than it asks about at a time" $
      inDatabaseE $ do
        accId <- account
        -- externalAddr 1 to externalAddr 6 as well
        replicateM_ 6 $ genExtAddress ctx cfg accId ""
        let twoAtATime = cfg {configParallelRequests = 2}
        res <- sync ctx twoAtATime (toStore (sevenAddressesStore ctx)) btc accId False noQuestion
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (7, 7)
        summaries accId
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 1000),
                          (Just (fakeTxHash 2), Store.BlockRef 800000 2, 1000),
                          (Just (fakeTxHash 3), Store.BlockRef 800000 3, 1000),
                          (Just (fakeTxHash 4), Store.BlockRef 800000 4, 1000),
                          (Just (fakeTxHash 5), Store.BlockRef 800000 5, 1000),
                          (Just (fakeTxHash 6), Store.BlockRef 800000 6, 1000),
                          (Just (fakeTxHash 7), Store.BlockRef 800000 7, 1000)
                        ]
        paidAddresses accId
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
        _ <- sync ctx cfg store btc accId False noQuestion
        _ <- sync ctx cfg store btc accId True noQuestion
        liftIO $ readIORef asked `shouldReturn` [[realPaymentId ctx]]
        rawTransactionCount `dbShouldBeE` 1

    it "fetches the raw transaction of a coin again when it is missing" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        let store = notingRawRequests asked $ toStore $ realPaymentStore ctx
        _ <- sync ctx cfg store btc accId False noQuestion
        lift $ P.deleteWhere ([] :: [P.Filter DBRawTx])
        _ <- sync ctx cfg store btc accId True noQuestion
        liftIO $ readIORef asked `shouldReturn` [[realPaymentId ctx], [realPaymentId ctx]]
        rawTransactionCount `dbShouldBeE` 1

    it "drops the raw transaction of a coin once the coin is spent" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        rawTransactionCount `dbShouldBeE` 1
        _ <- syncWith (spentElsewhereStore ctx) accId
        coins accId `dbShouldBeE` []
        rawTransactionCount `dbShouldBeE` 0

    it "keeps the raw transaction of a spent coin while another coin comes from it" $
      inDatabaseE $ do
        accId <- account
        _ <- genExtAddress ctx cfg accId ""
        _ <- syncWith (realPayoutStore ctx) accId
        coins accId `dbShouldBeE` [OutPoint (realPayoutId ctx) 0, OutPoint (realPayoutId ctx) 1]
        rawTransactionCount `dbShouldBeE` 1
        _ <- syncWith (realPayoutFirstSpentStore ctx) accId
        coins accId `dbShouldBeE` [OutPoint (realPayoutId ctx) 1]
        rawTransactionCount `dbShouldBeE` 1
        _ <- syncWith (realPayoutAllSpentStore ctx) accId
        coins accId `dbShouldBeE` []
        rawTransactionCount `dbShouldBeE` 0

    it "drops every raw transaction no coin comes from on a full sync" $
      inDatabaseE $ do
        accId <- account
        -- A raw transaction left behind by an older hw, which no coin comes from
        lift $ insertRawTx (spendingTx ctx [(fakeTxHash 5, 0)] [(otherAddr 0, 1000)])
        _ <- syncWith (realPaymentStore ctx) accId
        rawTransactionCount `dbShouldBeE` 2
        _ <- sync ctx cfg (toStore (realPaymentStore ctx)) btc accId True noQuestion
        rawTransactionCount `dbShouldBeE` 1
        lift (hasRawTx (realPaymentId ctx)) `dbShouldBeE` True

    it "refuses a store that does not return a raw transaction asked for" $
      inDatabaseE $ do
        accId <- account
        result <- lift $ runExceptT $ syncWith (noRawStore ctx) accId
        liftIO $ result `shouldBe` Left "The store was asked for 1 raw transactions but returned 0"

    -- The raw transaction of the payment pays externalAddr 0 50000 at its
    -- output 0. Its txid, a16ab47f...080e, is the double SHA-256 of its
    -- bytes, worked out apart from hw.
    it "refuses a store that reports a coin worth less than its transaction pays, and stores none of its coins" $
      inDatabaseE $ do
        accId <- account
        let store = (realPaymentStore ctx) {fakeUnspents = [coinOf ctx (realPaymentId ctx) (Just 800000) (externalAddr 0) 1100]}
        result <- lift $ runExceptT $ syncWith store accId
        liftIO $ result `shouldBe` Left "The store reports the coin a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e:0 as worth 1100 satoshis, but its transaction pays 50000"
        coins accId `dbShouldBeE` []

    it "refuses a store that reports a coin at another script than its transaction pays" $
      inDatabaseE $ do
        accId <- account
        let coin =
              Store.Unspent
                { Store.block = Store.BlockRef 800000 0,
                  Store.outpoint = OutPoint (realPaymentId ctx) 0,
                  Store.value = 50000,
                  Store.script = addressScript ctx (externalAddr 1),
                  Store.address = Just (externalAddr 0)
                }
            store = (realPaymentStore ctx) {fakeUnspents = [coin]}
        result <- lift $ runExceptT $ syncWith store accId
        liftIO $ result `shouldBe` Left "The store reports the coin a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e:0 at another script than its transaction pays"

    it "refuses a store that reports a coin at an output its transaction does not have" $
      inDatabaseE $ do
        accId <- account
        let store = (realPaymentStore ctx) {fakeUnspents = [coinAt ctx (realPaymentId ctx) 1 (Just 800000) (externalAddr 0) 50000]}
        result <- lift $ runExceptT $ syncWith store accId
        liftIO $ result `shouldBe` Left "The store reports the coin a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e:1, but its transaction has 1 outputs"

    -- The fee rate of a summary is its fee divided by its size
    it "refuses a store that reports a transaction of weight 0, rather than dividing by it" $
      inDatabaseE $ do
        accId <- account
        let weightless (Store.Transaction b v l i o d r t x z _ f) = Store.Transaction b v l i o d r t x z 0 f
            store = (realPaymentStore ctx) {fakeTransactions = weightless <$> (realPaymentStore ctx).fakeTransactions}
        result <- lift $ runExceptT $ syncWith store accId
        liftIO $ result `shouldBe` Left "The store reports a weight of 0 for the transaction a16ab47fec4dadea83515835516e9240df6507475dd0f0c8cd4eba835cfd080e"

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

    -- The store answers about the address of another wallet besides the
    -- ones asked: it must not reach the rows of another account, nor
    -- plant coins at an address of nobody's
    it "refuses a store that answers about an address it was not asked about, on an incremental sync and on a full one" $
      inDatabaseE $ do
        accId <- account
        let store = toStore (paymentStore ctx)
            tooMany = store {storeBalances = \addrs -> (<> [Store.Balance (otherAddr 0) 1000 0 1 1 1000]) <$> storeBalances store addrs}
        forM_ [False, True] $ \full ->
          lift $ dbShouldFailWith "The store answered about other addresses than the ones asked" $ sync ctx cfg tooMany btc accId full noQuestion

    it "refuses a store that returns a transaction it was not asked for" $
      inDatabaseE $ do
        accId <- account
        let store = toStore (paymentStore ctx)
            tooMany = store {storeTransactions = \tids -> (<> [creditTx ctx (fakeTxHash 9) (Store.BlockRef 800000 9) (otherAddr 0) 1000]) <$> storeTransactions store tids}
        lift $
          dbShouldFailWith "The store returned a transaction it was not asked for: 0000000000000000000000000000000000000000000000000000000000000009" $
            sync ctx cfg tooMany btc accId False noQuestion

    it "retires a pending transaction seen online" $
      inDatabaseE $ do
        accId <- account
        lift $ P.insert_ $ pendingRow accId (pendingTx ctx)
        pendingHashes accId `dbShouldBeE` [txHash (pendingTx ctx)]
        res <- syncWith (sentStore ctx) accId
        pendingHashes accId `dbShouldBeE` []
        liftIO $ syncResRebroadcast res `shouldBe` 0

    it "gives the label of a pending transaction it retires to that transaction in the history" $
      inDatabaseE $ do
        accId <- account
        lift $ P.insert_ $ pendingRow accId (pendingTx ctx)
        held <- requirePendingTx (txHash (pendingTx ctx))
        lift $ setPendingTxLabel held "Back to myself"
        _ <- syncWith (sentStore ctx) accId
        (map txInfoLabel <$> txsAfter accId Nothing Nothing) `dbShouldBeE` ["Back to myself"]

    it "sends again a transaction the user sent that the store lost" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        -- The user sent the payment to otherAddr 0 but the store does not
        -- list it: the coin is still unspent
        pendingHash <- importPendingTx btc ctx accId (signedSpend ctx)
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        published <- liftIO $ newIORef []
        res <- sync ctx cfg (notingPublished published $ toStore $ realPaymentStore ctx) btc accId False noQuestion
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (1, 0)
        liftIO $ readIORef published `shouldReturn` [txSignDataTx (signedSpend ctx)]
        pendingHashes accId `dbShouldBeE` [txHash (txSignDataTx (signedSpend ctx))]
        ((pendingFlags <$>) <$> pendingTxsAfter ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]

    it "leaves alone a signed transaction the user has not sent" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        _ <- importPendingTx btc ctx accId (signedSpend ctx)
        published <- liftIO $ newIORef []
        res <- sync ctx cfg (notingPublished published $ toStore $ realPaymentStore ctx) btc accId False noQuestion
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 0)
        liftIO $ readIORef published `shouldReturn` []

    it "does not send again a sent transaction in conflict, which the network would refuse" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        pendingHash <- importPendingTx btc ctx accId (signedSpend ctx)
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        published <- liftIO $ newIORef []
        res <- sync ctx cfg (notingPublished published $ toStore $ spentElsewhereStore ctx) btc accId False noQuestion
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 1)
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
        ((pendingFlags <$>) <$> pendingTxsAfter ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, True))]
        -- Nothing is left to free but the change address, which it had none of
        deleteTx ctx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (0, 0)
        ((pendingFlags <$>) <$> pendingTxsAfter ctx accId Nothing Nothing) `dbShouldBeE` []

    it "retires a sent transaction the network confirmed under another id, its signatures re-encoded by a miner" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        pendingHash <- importPendingTx btc ctx accId (signedSpend ctx)
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        published <- liftIO $ newIORef []
        res <- sync ctx cfg (notingPublished published $ toStore $ malleatedStore ctx) btc accId False noQuestion
        -- Neither in conflict nor sent again: the payment is on the chain
        liftIO $ (syncResRebroadcast res, syncResConflicted res) `shouldBe` (0, 0)
        liftIO $ readIORef published `shouldReturn` []
        pendingTxsAfter ctx accId Nothing Nothing `dbShouldBeE` []
        -- The payment shows under the id the network gave it
        summaries accId
          `dbShouldBeE` [ (Just (fakeTxHash 9), Store.BlockRef 800001 1, -50000),
                          (Just (realPaymentId ctx), Store.BlockRef 800000 1, 50000)
                        ]

    it "gives the label of a transaction the network has under another id to that id" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        pendingHash <- importPendingTx btc ctx accId (signedSpend ctx) {txSignDataLabel = "Rent"}
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        _ <- syncWith (malleatedStore ctx) accId
        lift (txLabel accId (fakeTxHash 9)) `dbShouldBeE` "Rent"

    it "retires an unsigned pending transaction the network has, signed and sent elsewhere" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        _ <- importPendingTx btc ctx accId (unsignedSpend ctx)
        res <- syncWith (malleatedStore ctx) accId
        liftIO $ syncResConflicted res `shouldBe` 0
        pendingTxsAfter ctx accId Nothing Nothing `dbShouldBeE` []

    it "asks the store about the parents of the pending transactions, and about a rival only when there is one" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        _ <- importPendingTx btc ctx accId (unsignedSpend ctx)
        held <- pendingTxs accId
        free <- liftIO $ newIORef []
        _ <- recordStanding (notingTxRequests free $ toStore $ realPaymentStore ctx) held
        liftIO $ readIORef free `shouldReturn` [[realPaymentId ctx]]
        rivalled <- liftIO $ newIORef []
        _ <- recordStanding (notingTxRequests rivalled $ toStore $ spentElsewhereStore ctx) held
        liftIO $ readIORef rivalled `shouldReturn` [[realPaymentId ctx], [fakeTxHash 8]]

    it "clears the conflict once the transaction that spent the coin leaves the chain" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        pendingHash <- importPendingTx btc ctx accId (unsignedSpend ctx)
        _ <- syncWith (spentElsewhereStore ctx) accId
        ((pendingFlags <$>) <$> pendingTxsAfter ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, True))]
        -- A reorganisation undoes transaction 8: the store lists the coin
        -- unspent again, and the full sync that follows finds it free
        res <- sync ctx cfg (toStore (realPaymentStore ctx)) btc accId True noQuestion
        liftIO $ syncResConflicted res `shouldBe` 0
        ((pendingFlags <$>) <$> pendingTxsAfter ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]
        coins accId `dbShouldBeE` [OutPoint (realPaymentId ctx) 0]

    it "reports a sweep of another account's coin once the coin is spent elsewhere" $
      inDatabaseE $ do
        accId <- account
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- genExtAddress ctx cfg otherId ""
        _ <- syncWith (realPaymentStore ctx) accId
        sweepHash <- importPendingTx btc ctx otherId (sweepToOther ctx)
        -- The sync of the other account is enough: the store is asked
        -- about the coin, whatever the account it belongs to
        res <- syncWith (spentElsewhereStore ctx) otherId
        liftIO $ syncResConflicted res `shouldBe` 1
        ((pendingFlags <$>) <$> pendingTxsAfter ctx otherId Nothing Nothing)
          `dbShouldBeE` [Just ((sweepHash, False, False, True))]

    it "reports a sweep of a coin outside the wallet once that coin is spent elsewhere" $
      inDatabaseE $ do
        accId <- account
        sweepHash <- importPendingTx btc ctx accId (outsideSweep ctx)
        unspent <- syncWith (outsideStore ctx) accId
        liftIO $ syncResConflicted unspent `shouldBe` 0
        spent <- syncWith (outsideSpentStore ctx) accId
        liftIO $ syncResConflicted spent `shouldBe` 1
        ((pendingFlags <$>) <$> pendingTxsAfter ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((sweepHash, False, False, True))]

    it "follows a payment the wallet made: the coin goes, the change comes, the summary is a debit" $
      inDatabaseE $ do
        accId <- account
        -- The change address, /1/0, and the coin to spend
        _ <- changeAddress ctx cfg accId
        _ <- syncWith (realPaymentStore ctx) accId
        -- The user prepared, signed and sent the payment, as broadcast leaves
        -- it: pending, online, its raw transaction stored
        lift $ P.insert_ $ pendingRow accId (changeSpendTx ctx)
        lift (setPendingTxOnline (nosigHash (changeSpendTx ctx))) `dbShouldBeE` 1
        lift $ insertRawTx (changeSpendTx ctx)
        -- The store lists it, confirmed
        res <- syncWith (changeSpentStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 2)
        -- A debit of 41000: 40000 paid and 1000 of fee, 9000 of change to /1/0
        txInfos <- txsAfter accId Nothing Nothing
        spend <- liftMaybe "The payment has no summary" $ find ((== Just (txHash (changeSpendTx ctx))) . txInfoHash) txInfos
        let summary = txInfoSummary spend
        liftIO $ do
          summaryType summary `shouldBe` TxDebit
          summaryAmount summary `shouldBe` -41000
          summaryFee summary `shouldBe` 1000
          Map.toList (summaryOtherOutputs summary) `shouldBe` [(otherAddr 0, 40000)]
          Map.toList (Map.map myOutputsPath (summaryMyOutputs summary)) `shouldBe` [(internalAddr 0, Deriv :/ 1 :/ 0)]
          Map.keys (summaryMyInputs summary) `shouldBe` [externalAddr 0]
          pendingFlags spend `shouldBe` Nothing
        coins accId `dbShouldBeE` [OutPoint (txHash (changeSpendTx ctx)) 1]
        balances accId `dbShouldBeE` (9000, 0, 1)
        pendingHashes accId `dbShouldBeE` []
        -- The parent of the spent coin is gone; the payment stays, for its change
        rawTransactionCount `dbShouldBeE` 1
        lift (hasRawTx (txHash (changeSpendTx ctx))) `dbShouldBeE` True

    it "syncs each account on its own" $
      inDatabaseE $ do
        accId <- account
        (accId2, _) <- testNewAcc2 ctx "second"
        addr2 <- genExtAddress ctx cfg accId2 ""
        liftIO $ dBAddressAddress addr2 `shouldBe` externalAddr2Text
        _ <- syncWith (twoWalletsStore ctx) accId
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        summaries accId2 `dbShouldBeE` []
        balances accId2 `dbShouldBeE` (0, 0, 0)
        _ <- syncWith (twoWalletsStore ctx) accId2
        summaries accId2 `dbShouldBeE` [(Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000)]
        coins accId2 `dbShouldBeE` [OutPoint (fakeTxHash 2) 0]
        balances accId `dbShouldBeE` (50000, 0, 1)
        balances accId2 `dbShouldBeE` (30000, 0, 1)

    it "syncs an account on another network, with the best block of that network" $
      inDatabaseE $ do
        (accId, acc) <- testNewAccBch ctx "cash"
        liftIO $ dBAccountXPubKey acc `shouldBe` testAccountXPubBch
        addr <- genExtAddress ctx cfg accId ""
        liftIO $ dBAddressAddress addr `shouldBe` bchExternalAddrText
        res <- sync ctx cfg (toStore (cashPaymentStore ctx)) bch accId False noQuestion
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        (map jsonCoinOutpoint <$> coinsAfter bch accId AllCoins Nothing Nothing) `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        balances accId `dbShouldBeE` (50000, 0, 1)
        getBest bch `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800002 1).header) 800002 (Just 1700000000))
        getBest btc `dbShouldBeE` Nothing

  describe "The balances of txs when the history changes in the middle" $ do
    -- The payment of 10000 lands between the 30000 and the 20000:
    -- 30000 + 10000 = 40000, and 40000 + 20000 = 60000
    it "takes in an old payment found late, and changes only the balances above it" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        listedBalances accId `dbShouldBeE` [(Just (fakeTxHash 3), Just 50000), (Just (fakeTxHash 2), Just 30000)]
        _ <- genExtAddress ctx cfg accId ""
        _ <- syncWith (latePaymentStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 3), Just 60000),
                          (Just (fakeTxHash 4), Just 40000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- 90000 was there with the middle payment of 40000, 50000 without it
    it "drops a payment in the middle a reorganisation left out, and changes only the balances above it" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (threePaymentsStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 3), Just 90000),
                          (Just (fakeTxHash 6), Just 70000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        _ <- syncWith (middleLeftOutStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 6), Nothing),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- 50000 confirmed + 15000 unconfirmed = 65000 before, and
    -- 50000 + 5000 = 55000 once the 10000 is replaced
    it "drops an unconfirmed payment replaced in the middle of the mempool, on an incremental sync" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoUnconfirmedStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 7), Just 65000),
                          (Just (fakeTxHash 6), Just 60000),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        _ <- syncWith (oneReplacedStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 7), Just 55000),
                          (Just (fakeTxHash 6), Nothing),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- One chain serves both accounts, so its reorganisation concerns both
    it "marks a payment a reorganisation left out as dead in every account hw sync goes through" $
      inDatabaseE $ do
        mainId <- account
        (secondId, _) <- testNewAcc2 ctx "second"
        _ <- genExtAddress ctx cfg secondId ""
        accs <- mapM (\accId -> (,) accId <$> getAccountById accId) [mainId, secondId]
        _ <- syncAccounts ctx cfg (const (toStore (twoWalletsStore ctx))) accs False noQuestion
        _ <- syncAccounts ctx cfg (const (toStore (secondLeftOutStore ctx))) accs False noQuestion
        listedBalances secondId `dbShouldBeE` [(Just (fakeTxHash 2), Nothing)]

    it "marks a payment a reorganisation left out as dead in an account synced after another one" $
      inDatabaseE $ do
        mainId <- account
        (secondId, _) <- testNewAcc2 ctx "second"
        _ <- genExtAddress ctx cfg secondId ""
        _ <- syncWith (twoWalletsStore ctx) mainId
        _ <- syncWith (twoWalletsStore ctx) secondId
        _ <- syncWith (secondLeftOutStore ctx) mainId
        _ <- syncWith (secondLeftOutStore ctx) secondId
        listedBalances secondId `dbShouldBeE` [(Just (fakeTxHash 2), Nothing)]

    it "marks a payment a reorganisation left out as dead in that account on a full sync, whatever came before" $
      inDatabaseE $ do
        mainId <- account
        (secondId, _) <- testNewAcc2 ctx "second"
        _ <- genExtAddress ctx cfg secondId ""
        _ <- syncWith (twoWalletsStore ctx) mainId
        _ <- syncWith (twoWalletsStore ctx) secondId
        _ <- syncWith (secondLeftOutStore ctx) mainId
        _ <- syncWith (secondLeftOutStore ctx) secondId
        _ <- sync ctx cfg (toStore (secondLeftOutStore ctx)) btc secondId True noQuestion
        listedBalances secondId `dbShouldBeE` [(Just (fakeTxHash 2), Nothing)]

  describe "The block the last sync of an account reached" $ do
    let twoAccounts = do
          mainId <- account
          (secondId, _) <- testNewAcc2 ctx "second"
          _ <- genExtAddress ctx cfg secondId ""
          return (mainId, secondId)

    it "is stored for the account that synced, and for no other" $
      inDatabaseE $ do
        (mainId, secondId) <- twoAccounts
        _ <- syncWith (twoWalletsStore ctx) mainId
        syncedBlock mainId `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800002 1).header) 800002)
        syncedBlock secondId `dbShouldBeE` Nothing

    it "is what the incremental sync of each account asks the store about" $
      inDatabaseE $ do
        (mainId, secondId) <- twoAccounts
        let laterChain = (twoWalletsStore ctx) {fakeBest = blockAt 800003 7, fakeBlocks = [blockAt 800002 1, blockAt 800003 7]}
        _ <- syncWith (twoWalletsStore ctx) mainId
        _ <- syncWith laterChain secondId
        asked <- liftIO $ newIORef []
        let store = notingBlockRequests asked (toStore laterChain)
        _ <- sync ctx cfg store btc mainId False noQuestion
        _ <- sync ctx cfg store btc secondId False noQuestion
        liftIO $ readIORef asked `shouldReturn` [800002, 800003]

    -- No reorganisation tells of it: only looking at every address does
    it "makes a sync look at every address when the account has none, as after a migration" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        lift $ P.delete (DBSyncedBlockKey accId)
        _ <- syncWith ((leftOutStore ctx) {fakeBest = blockAt 800002 1, fakeBlocks = [blockAt 800002 1]}) accId
        deadSummaries accId `dbShouldBeE` [Just (fakeTxHash 3)]

    it "refuses an incremental sync when it does not read, and a full sync stores it again" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (paymentStore ctx) accId
        lift $ P.update (DBSyncedBlockKey accId) [DBSyncedBlockBlock P.=. "nonsense"]
        lift $
          dbShouldFailWith "The block the last sync of the account reached does not read: sync --full stores it again" $
            syncWith (paymentStore ctx) accId
        _ <- sync ctx cfg (toStore (paymentStore ctx)) btc accId True noQuestion
        syncedBlock accId `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800002 1).header) 800002)

  describe "A reorganisation of the chain" $ do
    -- After a reorganisation the store still knows the block the last sync
    -- reached, at its height, out of the main chain. The balances stay as
    -- they were, and transaction 3 is listed no more.
    it "looks at every address when the block the last sync reached is at its height but out of the main chain" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        let outOfMain (Store.BlockData h _ w hd z wt t o f s) = Store.BlockData h False w hd z wt t o f s
        _ <- syncWith ((leftOutStore ctx) {fakeBlocks = [outOfMain (blockAt 800002 1), blockAt 800002 9, blockAt 800003 7]}) accId
        deadSummaries accId `dbShouldBeE` [Just (fakeTxHash 3)]

    it "moves a payment to its new block in an account synced after another one" $
      inDatabaseE $ do
        mainId <- account
        (secondId, _) <- testNewAcc2 ctx "second"
        _ <- genExtAddress ctx cfg secondId ""
        _ <- syncWith (twoWalletsStore ctx) mainId
        _ <- syncWith (twoWalletsStore ctx) secondId
        _ <- syncWith (secondMovedStore ctx) mainId
        _ <- syncWith (secondMovedStore ctx) secondId
        summaries secondId `dbShouldBeE` [(Just (fakeTxHash 2), Store.BlockRef 800003 3, 30000)]

    -- The 40000 goes to the top of the list, and the rows it left go down:
    -- 30000 + 20000 = 50000, and 50000 + 40000 = 90000
    it "puts a payment that goes back to the mempool at the top of the list, with the balances that follow" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (threePaymentsStore ctx) accId
        _ <- syncWith (backInMempoolStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 6), Just 90000),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        balances accId `dbShouldBeE` (50000, 40000, 3)

    it "drops a payment and the spending of its coin when it leaves both out" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (spentPaymentStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 8), Just 30000),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        _ <- syncWith (leftOutStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 8), Nothing),
                          (Just (fakeTxHash 3), Nothing),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        balances accId `dbShouldBeE` (30000, 0, 1)

    it "gives back the coin of a spending it leaves out" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (spentPaymentStore ctx) accId
        _ <- syncWith (middleLeftOutStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 8), Nothing),
                          (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]
        balances accId `dbShouldBeE` (50000, 0, 2)

    -- Both sit at the same place in the chain, so they go by their ids.
    -- 30000 + 15000 = 45000.
    it "takes in the rival it puts in the place of a payment" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        _ <- syncWith (rivalStore ctx) accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 3), Nothing),
                          (Just (fakeTxHash 9), Just 45000),
                          (Just (fakeTxHash 2), Just 30000)
                        ]
        balances accId `dbShouldBeE` (45000, 0, 2)

    it "follows a chain that got shorter than the block the account reached" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        _ <- syncWith ((leftOutStore ctx) {fakeBest = blockAt 800001 9, fakeBlocks = [blockAt 800001 9]}) accId
        deadSummaries accId `dbShouldBeE` [Just (fakeTxHash 3)]
        syncedBlock accId `dbShouldBeE` Just (SyncedBlock (headerHash (blockAt 800001 9).header) 800001)
        getBest btc `dbShouldBeE` Just (BestBlock (headerHash (blockAt 800001 9).header) 800001 (Just 1700000000))

    it "fetches only the transactions that moved: it cannot change what a transaction holds" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (twoPaymentsStore ctx) accId
        asked <- liftIO $ newIORef []
        _ <- sync ctx cfg (notingTxRequests asked (toStore (oneMovedStore ctx))) btc accId False noQuestion
        liftIO $ (concat <$> readIORef asked) `shouldReturn` [fakeTxHash 3]

  describe "A full sync, over a database gone wrong" $ do
    let -- The account with the two payments, and the store that has them
        syncedAccount = do
          accId <- account
          _ <- syncWith (twoPaymentsStore ctx) accId
          return accId
        fullSync accId = sync ctx cfg (toStore (twoPaymentsStore ctx)) btc accId True noQuestion
        summaryKey accId n = DBTxInfoKey accId (hashBytes (fakeTxHash n))
        coinKey n = DBCoinKey (outpointBytes (OutPoint (fakeTxHash n) 0))
        rightSummaries =
          [ (Just (fakeTxHash 2), Store.BlockRef 800001 1, 30000),
            (Just (fakeTxHash 3), Store.BlockRef 800002 1, 20000)
          ]
        rightBalances = [(Just (fakeTxHash 3), Just 50000), (Just (fakeTxHash 2), Just 30000)]

    it "fetches every transaction the store lists, though no block moved" $
      inDatabaseE $ do
        accId <- syncedAccount
        asked <- liftIO $ newIORef []
        _ <- sync ctx cfg (notingTxRequests asked (toStore (twoPaymentsStore ctx))) btc accId True noQuestion
        liftIO $ (sort . concat <$> readIORef asked) `shouldReturn` [fakeTxHash 2, fakeTxHash 3]

    it "reports no update over a database that was right" $
      inDatabaseE $ do
        accId <- syncedAccount
        res <- fullSync accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (0, 0)

    it "writes the amount of a summary again, which --after adds up" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.update (summaryKey accId 3) [DBTxInfoAmount P.=. 1]
        _ <- fullSync accId
        (fmap dBTxInfoAmount <$> lift (P.get (summaryKey accId 3))) `dbShouldBeE` Just 20000

    it "writes again a summary that reads but says the wrong thing" $
      inDatabaseE $ do
        accId <- syncedAccount
        _ <- lift $ repsertSummary btc accId (fakeTxHash 3) (Store.BlockRef 800002 1) fixtureTime paymentSummary
        _ <- fullSync accId
        summaries accId `dbShouldBeE` rightSummaries

    it "writes again a summary that does not read" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.update (summaryKey accId 3) [DBTxInfoBlob P.=. "nonsense"]
        _ <- fullSync accId
        summaries accId `dbShouldBeE` rightSummaries

    it "writes again a block reference that does not read, which would stop any other sync" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.update (summaryKey accId 3) [DBTxInfoBlockRef P.=. "nonsense"]
        _ <- fullSync accId
        summaries accId `dbShouldBeE` rightSummaries

    it "brings back a summary that went missing, and the balances below it are right again" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.delete (summaryKey accId 3)
        -- With the 20000 gone from the list, the older row takes all that
        -- the account holds: 20000 too much
        listedBalances accId `dbShouldBeE` [(Just (fakeTxHash 2), Just 50000)]
        _ <- fullSync accId
        listedBalances accId `dbShouldBeE` rightBalances

    it "brings back to life a transaction the store lists" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ markTxInfoDead accId (fakeTxHash 3)
        _ <- fullSync accId
        listedBalances accId `dbShouldBeE` rightBalances

    it "marks dead a transaction no address lists, and keeps it for the history" $
      inDatabaseE $ do
        accId <- syncedAccount
        _ <- lift $ repsertSummary btc accId (fakeTxHash 9) (Store.BlockRef 800001 5) fixtureTime paymentSummary
        _ <- fullSync accId
        listedBalances accId
          `dbShouldBeE` [ (Just (fakeTxHash 3), Just 50000),
                          (Just (fakeTxHash 9), Nothing),
                          (Just (fakeTxHash 2), Just 30000)
                        ]

    -- txs refuses a row that does not read, and nothing can write this one
    it "deletes a dead summary that does not read, which would stop txs for good" $
      inDatabaseE $ do
        accId <- syncedAccount
        _ <- lift $ repsertSummary btc accId (fakeTxHash 9) (Store.BlockRef 800001 5) fixtureTime paymentSummary
        lift $ markTxInfoDead accId (fakeTxHash 9)
        lift $ P.update (summaryKey accId 9) [DBTxInfoBlob P.=. "nonsense"]
        _ <- fullSync accId
        listedBalances accId `dbShouldBeE` rightBalances

    it "deletes a live summary that does not read and that no address lists" $
      inDatabaseE $ do
        accId <- syncedAccount
        _ <- lift $ repsertSummary btc accId (fakeTxHash 9) (Store.BlockRef 800001 5) fixtureTime paymentSummary
        lift $ P.update (summaryKey accId 9) [DBTxInfoBlockRef P.=. "nonsense"]
        _ <- fullSync accId
        listedBalances accId `dbShouldBeE` rightBalances

    it "writes the balances of the addresses and of the account again" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.update accId [DBAccountBalanceConfirmed P.=. 7, DBAccountBalanceUnconfirmed P.=. 8, DBAccountBalanceCoins P.=. 9]
        lift $ P.updateWhere [DBAddressAccount P.==. accId] [DBAddressBalanceConfirmed P.=. 7]
        _ <- fullSync accId
        balances accId `dbShouldBeE` (50000, 0, 2)

    it "brings back a coin that went missing" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.delete (coinKey 3)
        _ <- fullSync accId
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]

    it "deletes a coin the store does not list" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.insert_ $ DBCoin accId (outpointBytes (OutPoint (fakeTxHash 9) 0)) "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 1000 (S.encode (Store.BlockRef 800000 1)) False fixtureTime
        _ <- fullSync accId
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]

    it "writes the value of a coin again" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.update (coinKey 3) [DBCoinValue P.=. 1]
        _ <- fullSync accId
        (fmap dBCoinValue <$> lift (P.get (coinKey 3))) `dbShouldBeE` Just 20000

    it "writes the block of a coin again" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ P.update (coinKey 3) [DBCoinBlockRef P.=. "nonsense"]
        _ <- fullSync accId
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]

    -- Whichever address comes first, its coin is stored under the other
    it "gives each coin its address again when two coins stand under each other's address" $
      inDatabaseE $ do
        accId <- account
        _ <- genExtAddress ctx cfg accId ""
        _ <- syncWith (oldPaymentStore ctx) accId
        lift $ P.update (coinKey 3) [DBCoinAddress P.=. "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
        lift $ P.update (coinKey 4) [DBCoinAddress P.=. "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]
        _ <- sync ctx cfg (toStore (oldPaymentStore ctx)) btc accId True noQuestion
        (fmap dBCoinAddress <$> lift (P.get (coinKey 3))) `dbShouldBeE` Just "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        (fmap dBCoinAddress <$> lift (P.get (coinKey 4))) `dbShouldBeE` Just "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"

    it "takes back a coin stored under another account" $
      inDatabaseE $ do
        accId <- syncedAccount
        (otherId, _) <- testNewAcc2 ctx "second"
        lift $ P.update (coinKey 3) [DBCoinAccount P.=. otherId]
        _ <- fullSync accId
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 3) 0]

    it "fetches again a raw transaction that does not read" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        lift $ P.update (DBRawTxKey (hashBytes (realPaymentId ctx))) [DBRawTxBlob P.=. "nonsense"]
        _ <- sync ctx cfg (toStore (realPaymentStore ctx)) btc accId True noQuestion
        lookupRawTx (realPaymentId ctx) `dbShouldBeE` Just (realPaymentTx ctx)

    it "fetches again a raw transaction that reads but holds another transaction" $
      inDatabaseE $ do
        accId <- account
        _ <- syncWith (realPaymentStore ctx) accId
        lift $ P.update (DBRawTxKey (hashBytes (realPaymentId ctx))) [DBRawTxBlob P.=. S.encode (spendingTx ctx [(fakeTxHash 5, 0)] [(otherAddr 0, 1000)])]
        _ <- sync ctx cfg (toStore (realPaymentStore ctx)) btc accId True noQuestion
        lookupRawTx (realPaymentId ctx) `dbShouldBeE` Just (realPaymentTx ctx)

    it "keeps the labels and the coins the user locked: the store cannot tell them" $
      inDatabaseE $ do
        accId <- syncedAccount
        lift $ setTxLabel accId (fakeTxHash 3) "Rent"
        _ <- lockCoin (OutPoint (fakeTxHash 3) 0)
        lift $ P.update (coinKey 3) [DBCoinValue P.=. 1]
        _ <- fullSync accId
        lift (txLabel accId (fakeTxHash 3)) `dbShouldBeE` "Rent"
        (fmap dBCoinLockedByUser <$> lift (P.get (coinKey 3))) `dbShouldBeE` Just True

  describe "Syncing the accounts of several networks" $ do
    -- The account main on btc and the account cash on bch, each with its
    -- first receiving address
    let accounts = do
          mainId <- account
          (cashId, _) <- testNewAccBch ctx "cash"
          _ <- genExtAddress ctx cfg cashId ""
          main <- getAccountById mainId
          cash <- getAccountById cashId
          return [(mainId, main), (cashId, cash)]
        names results = [(dBAccountName acc, failure) | (acc, failure) <- results]

    -- The store of bch pays cash 50000 in block 800000
    it "syncs the accounts whose store is up, and leaves out those whose store is down, with the failure of their store" $
      inDatabaseE $ do
        accs@[(mainId, _), (cashId, _)] <- accounts
        let storeOn net
              | net == bch = toStore (cashPaymentStore ctx)
              | otherwise = unhealthy (toStore (paymentStore ctx))
        (synced, notSynced) <- syncAccounts ctx cfg storeOn accs False noQuestion
        liftIO $ map (dBAccountName . syncResAccount) synced `shouldBe` ["cash"]
        liftIO $ names notSynced `shouldBe` [("main", "The store is down")]
        summaries cashId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        summaries mainId `dbShouldBeE` []

    it "leaves out every account when every store is down, in the order of the accounts, rather than failing" $
      inDatabaseE $ do
        accs <- accounts
        (synced, notSynced) <- syncAccounts ctx cfg (const (unhealthy (toStore emptyStore))) accs False noQuestion
        liftIO $ map syncResAccount synced `shouldBe` []
        liftIO $ names notSynced `shouldBe` [("main", "The store is down"), ("cash", "The store is down")]

  describe "Scanning a chain for used addresses" $ do
    -- The scan behind the watch of the gap, on the external chain with a
    -- window of 3 instead of the default 20: the ten written-out addresses
    -- of the test wallet are enough
    let scan store = runExceptT $ scanChain ctx (toStore store) Bip44 (testAccountPub ctx) extDeriv 3 0 "on the chain" noQuestion
        -- A store where the given addresses each have one transaction
        used addrs = emptyStore {fakeBalances = [Store.Balance addr 1000 0 1 1 1000 | addr <- addrs]}

    it "finds nothing in an empty store, after one window" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore emptyStore)
      runExceptT (scanChain ctx store Bip44 (testAccountPub ctx) extDeriv 3 0 "on the chain" noQuestion) `shouldReturn` Right 0
      readIORef asked `shouldReturn` [take 3 externalAddrs]

    it "counts the addresses up to the last used one" $
      scan (used [externalAddr 0, externalAddr 4]) `shouldReturn` Right 5

    it "stops after a window without a used address, so a gap of three hides address 3" $
      scan (used [externalAddr 3]) `shouldReturn` Right 0

    it "looks at one more window past the last used address" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore (used [externalAddr 2]))
      runExceptT (scanChain ctx store Bip44 (testAccountPub ctx) extDeriv 3 0 "on the chain" noQuestion) `shouldReturn` Right 3
      readIORef asked `shouldReturn` [take 3 externalAddrs, take 3 (drop 3 externalAddrs)]

    it "counts an address whose coins are all spent as used" $
      scan (emptyStore {fakeBalances = [Store.Balance (externalAddr 1) 0 0 0 2 1000]}) `shouldReturn` Right 2

    it "ignores an address the store lists without transactions" $
      scan (emptyStore {fakeBalances = [Store.Balance (externalAddr 1) 0 0 0 0 0]}) `shouldReturn` Right 0

    it "fails when the store fails" $
      runExceptT (scanChain ctx ((toStore emptyStore) {storeBalances = \_ -> throwError "The store is down"}) Bip44 (testAccountPub ctx) extDeriv 3 0 "on the chain" noQuestion)
        `shouldReturn` Left "The store is down"

    -- With a window of 3, the scan looks at 1002 addresses before it asks,
    -- and at 1002 more before it asks again: the first answer goes on,
    -- the second stops
    it "asks whether to go on after a thousand addresses the store reports used in a row, saying where, and asks again as many further" $ do
      asked <- newIORef []
      result <- runExceptT $ scanChain ctx everyAddressUsed Bip44 (testAccountPub ctx) extDeriv 3 0 "on the receiving chain of the account main" (answering asked 1)
      readIORef asked
        `shouldReturn` [ "The store reports used addresses as far as 1002 on the receiving chain of the account main, and more beyond.",
                         "The store reports used addresses as far as 2004 on the receiving chain of the account main, and more beyond."
                       ]
      result `shouldBe` Left "The store reports used addresses as far as 2004 on the receiving chain of the account main, and more beyond. The scan stopped there."

    it "stops at the limit when the answer is no" $
      runExceptT (scanChain ctx everyAddressUsed Bip44 (testAccountPub ctx) extDeriv 3 0 "on the chain" stopLooking)
        `shouldReturn` Left "The store reports used addresses as far as 1002 on the chain, and more beyond. The scan stopped there."

    it "recovers an account on its first sync, as a restored one is: every used address, its transactions and coins" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        res <- sync ctx cfg {configGap = 3} (toStore (discoveredStore ctx)) btc accId False noQuestion
        liftIO $ (syncResNewAddrs res, dBAccountExternal (syncResAccount res), dBAccountInternal (syncResAccount res)) `shouldBe` (7, 5, 2)
        found <- summaries accId
        liftIO $ found `shouldMatchList` [(Just (txHash tx), Store.BlockRef height 1, fromIntegral amount) | (_, amount, height, tx) <- discoveredPayments ctx]
        held <- coins accId
        liftIO $ held `shouldMatchList` [OutPoint (txHash tx) 0 | (_, _, _, tx) <- discoveredPayments ctx]
        balances accId `dbShouldBeE` (80000, 0, 3)
        rawTransactionCount `dbShouldBeE` 3

  describe "The steps a sync tells" $ do
    let watching steps = noQuestion {status = notingSteps steps}
        -- What a sync tells of the account main, in the order told
        toldOf steps = map snd <$> readIORef steps
        -- The steps of a task on a single item
        oneOf task = [Counted task 0 1, Counted task 1 1]

    -- The account has one address, paid once. Each chain takes one window
    -- of the scan, which finds nothing past that address.
    it "tells every step of a sync that finds a payment, each counting its one item" $
      inDatabaseE $ do
        accId <- account
        steps <- liftIO $ newIORef []
        _ <- sync ctx cfg (toStore (realPaymentStore ctx)) btc accId False (watching steps)
        liftIO $
          toldOf steps
            `shouldReturn` [CheckingStore, LookingForAddresses 0, LookingForAddresses 0]
              <> oneOf ReadingBalances
              <> oneOf ListingTransactions
              <> oneOf FetchingTransactions
              <> oneOf ReadingCoins
              <> oneOf FetchingRawTransactions

    it "tells a sync that finds nothing new, which only reads the balances" $
      inDatabaseE $ do
        accId <- account
        let store = toStore (realPaymentStore ctx)
        _ <- sync ctx cfg store btc accId False noQuestion
        steps <- liftIO $ newIORef []
        _ <- sync ctx cfg store btc accId False (watching steps)
        liftIO $ toldOf steps `shouldReturn` [CheckingStore, LookingForAddresses 0, LookingForAddresses 0] <> oneOf ReadingBalances

    it "names the account in every step" $
      inDatabaseE $ do
        accId <- account
        steps <- liftIO $ newIORef []
        _ <- sync ctx cfg (toStore (realPaymentStore ctx)) btc accId False (watching steps)
        names <- liftIO $ map fst <$> readIORef steps
        liftIO $ names `shouldSatisfy` all (== "main")

    -- With windows of 3, the receiving chain takes three of them: the
    -- first finds address 0, the second address 4, the third nothing. The
    -- change chain takes two: the first finds address 1, the second
    -- nothing. Each window tells what the ones before it found.
    it "tells the used addresses a scan finds, counting on from one chain to the next" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        steps <- liftIO $ newIORef []
        _ <- sync ctx cfg {configGap = 3} (toStore (discoveredStore ctx)) btc accId False (watching steps)
        told <- liftIO $ toldOf steps
        liftIO $ [found | LookingForAddresses found <- told] `shouldBe` [0, 1, 2, 2, 3]

    -- The sync makes two requests at a time, and the count still reaches
    -- all seven addresses
    it "counts every address listed, whatever the requests at a time" $
      inDatabaseE $ do
        accId <- account
        -- externalAddr 1 to externalAddr 6 as well
        replicateM_ 6 $ genExtAddress ctx cfg accId ""
        let twoAtATime = cfg {configParallelRequests = 2}
        steps <- liftIO $ newIORef []
        _ <- sync ctx twoAtATime (toStore (sevenAddressesStore ctx)) btc accId False (watching steps)
        told <- liftIO $ toldOf steps
        liftIO $ [done | Counted ListingTransactions done _ <- told] `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7]

  describe "Recovering the accounts of a seed" $ do
    -- Accounts 0 and 2 of the test seed were paid at their first receiving
    -- address; account 1 and the ones after 2 never were
    let paidAccounts = paidStore ctx [(externalAddr 0, 50000), (account2Addr, 20000)]
        found store seed gap = map fst <$> recoverableAccounts ctx cfg (toStore store) Bip44 btc seed gap noQuestion

    it "finds the accounts that were paid, looking past a run of unpaid ones as long as the account gap" $
      inDatabaseE $ do
        found paidAccounts (testSeed ctx) 2 `dbShouldBeE` [0, 2]
        found paidAccounts (testSeed ctx) 1 `dbShouldBeE` [0]

    it "stops after the account gap, having looked at one window of the receiving chain of each account" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore emptyStore)
      inDatabaseE $ (map fst <$> recoverableAccounts ctx cfg store Bip44 btc (testSeed ctx) 3 noQuestion) `dbShouldBeE` []
      windows <- readIORef asked
      map length windows `shouldBe` [20, 20, 20]
      map (take 1) windows `shouldBe` [[externalAddr 0], [account1Addr], [account2Addr]]

    it "passes over an account the wallet has already, without counting it" $
      inDatabaseE $ do
        _ <- testNewAcc ctx "main"
        found paidAccounts (testSeed ctx) 2 `dbShouldBeE` [2]

    -- Account 0 is paid, 1 is not, 2 is, and 3 and 4 are not, which ends
    -- the search with a gap of 2. Each line tells what came before it.
    it "tells each account it looks at, with its network and the accounts found paid so far" $
      inDatabaseE $ do
        steps <- liftIO $ newIORef []
        _ <- recoverableAccounts ctx cfg (toStore paidAccounts) Bip44 btc (testSeed ctx) 2 noQuestion {status = notingSteps steps}
        liftIO $
          (map (uncurry statusText) <$> readIORef steps)
            `shouldReturn` [ "recover on btc: looking at /44'/0'/0', 0 accounts paid so far",
                             "recover on btc: looking at /44'/0'/1', 1 account paid so far",
                             "recover on btc: looking at /44'/0'/2', 1 account paid so far",
                             "recover on btc: looking at /44'/0'/3', 2 accounts paid so far",
                             "recover on btc: looking at /44'/0'/4', 2 accounts paid so far"
                           ]

    it "passes over an index whose child key is invalid, without counting it" $
      inDatabaseE $ found paidAccounts (seedWithoutIndex1 ctx) 1 `dbShouldBeE` [0, 2]

    it "creates the accounts found, named after the network and their index, with the fingerprint of the seed, and syncs them" $
      inDatabaseE $ do
        results <- recoverSeed ctx cfg (toStore paidAccounts) btc (testSeed ctx) 2 noQuestion
        liftIO $ map (dBAccountName . syncResAccount) results `shouldBe` ["btc0", "btc2"]
        liftIO $ map (dBAccountFingerprint . syncResAccount) results `shouldBe` [Just "bc560f95", Just "bc560f95"]
        liftIO $ map syncResNewAddrs results `shouldBe` [1, 1]
        (map (dBAccountIndex . snd) <$> lift getAccounts) `dbShouldBeE` [0, 2]
        accs <- lift getAccounts
        mapM_ (\((accId, _), (amount, tid)) -> do
          balances accId `dbShouldBeE` (amount, 0, 1)
          coins accId `dbShouldBeE` [OutPoint tid 0]) (zip accs [(50000, fakeTxHash 1), (20000, fakeTxHash 2)])

    it "creates nothing for a seed that was never paid" $
      inDatabaseE $ do
        recoverSeed ctx cfg (toStore emptyStore) btc (testSeed ctx) 2 noQuestion `dbShouldBeE` []
        lift getAccounts `dbShouldBeE` []

    -- The chain pays the first receiving address of the bip44 account 0
    -- and of the bip84 account 0 of the seed, and no other account
    it "creates the bip84 accounts found after the bip44 ones, named with -segwit, each kind searched with the account gap" $
      inDatabaseE $ do
        let store = paidStore ctx [(externalAddr 0, 50000), (segwitExternalAddr 0, 30000)]
        results <- recoverSeed ctx cfg (toStore store) btc (testSeed ctx) 1 noQuestion
        liftIO $ map (dBAccountName . syncResAccount) results `shouldBe` ["btc0", "btc0-segwit"]
        liftIO $ map (pathToStr . accountPath . syncResAccount) results `shouldBe` ["/44'/0'/0'", "/84'/0'/0'"]
        liftIO $ map syncResSpendable results `shouldBe` [50000, 30000]

    it "searches no bip84 account on Bitcoin Cash, which has none" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore emptyStore)
      inDatabaseE $ recoverSeed ctx cfg store bch (testSeed ctx) 1 noQuestion `dbShouldBeE` []
      windows <- readIORef asked
      map (take 1) windows `shouldBe` [[bchExternalAddr]]

    it "asks on the receiving chain of an account of the seed, named by its path and its network" $
      inDatabaseE $
        lift $
          dbShouldFailWith "The store reports used addresses as far as 1000 on the receiving chain of the account /44'/0'/0' of the seed on btc, and more beyond. The scan stopped there." $
            recoverableAccounts ctx cfg everyAddressUsed Bip44 btc (testSeed ctx) 1 stopLooking

    -- The store reports the accounts 0 to 39 of the seed paid at their
    -- first receiving address. The first answer goes on, the second stops.
    it "asks whether to go on after 20 accounts the store reports paid, naming the network and the last one by its path, and asks again 20 accounts further" $
      inDatabaseE $ do
        firstAddrs <- liftIO $ firstAddrsOfAccounts ctx 40
        asked <- liftIO $ newIORef []
        lift $
          dbShouldFailWith "The store reports payments to 40 accounts of the seed on btc, as far as the account /44'/0'/39'. The search stopped there." $
            recoverableAccounts ctx cfg (toStore (paidStore ctx [(addr, 1000) | addr <- firstAddrs])) Bip44 btc (testSeed ctx) 1 (answering asked 1)
        liftIO $
          readIORef asked
            `shouldReturn` [ "The store reports payments to 20 accounts of the seed on btc, as far as the account /44'/0'/19'.",
                             "The store reports payments to 40 accounts of the seed on btc, as far as the account /44'/0'/39'."
                           ]

    -- The store reports the accounts 0 to 20 of the seed paid at their
    -- first receiving address: 21 accounts, one more than the search finds
    -- before it asks
    it "finds every paid account when the answer is yes, those before the question and those after" $
      inDatabaseE $ do
        firstAddrs <- liftIO $ firstAddrsOfAccounts ctx 21
        let store = toStore (paidStore ctx [(addr, 1000) | addr <- firstAddrs])
        (map fst <$> recoverableAccounts ctx cfg store Bip44 btc (testSeed ctx) 1 keepLooking)
          `dbShouldBeE` [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

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
        found store net gap = runExceptT $ foreignFunds ctx smallGap (toStore store) net (testSeed ctx) gap noQuestion

    it "finds the bip49 and bip86 accounts paid on Bitcoin, with their used addresses and the balance of both chains, unconfirmed included" $
      -- 50000 confirmed + 20000 unconfirmed = 70000
      found elsewhere btc 1 `shouldReturn` Right [ForeignFunds Bip49 0 2 70000, ForeignFunds Bip86 0 1 7000]

    it "finds nothing for a seed never paid there, after one window of the receiving chain of each kind" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore emptyStore)
      runExceptT (foreignFunds ctx smallGap store btc (testSeed ctx) 1 noQuestion) `shouldReturn` Right []
      readIORef asked `shouldReturn` [[wrappedExternalAddr 0, wrappedExternalAddr 1], [taprootExternalAddr 0, taprootExternalAddr 1]]

    -- The bip49 account 0 was paid at its first receiving address, and
    -- the search moves on to its account 1
    it "scans the change chain of an account found, and only then" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore (emptyStore {fakeBalances = [Store.Balance (wrappedExternalAddr 0) 50000 0 1 1 50000]}))
      runExceptT (foreignFunds ctx smallGap store btc (testSeed ctx) 1 noQuestion) `shouldReturn` Right [ForeignFunds Bip49 0 1 50000]
      windows <- readIORef asked
      map (take 1) windows `shouldBe` [[wrappedExternalAddr 0], [wrappedExternalAddr 2], [wrappedInternalAddr 0], [wrappedAccount1Addr], [taprootExternalAddr 0]]

    it "looks past unpaid accounts as far as the account gap allows, as for the accounts hw holds" $ do
      let paidAccount1 = emptyStore {fakeBalances = [Store.Balance wrappedAccount1Addr 1000 0 1 1 1000]}
      found paidAccount1 btc 1 `shouldReturn` Right []
      found paidAccount1 btc 2 `shouldReturn` Right [ForeignFunds Bip49 1 1 1000]

    -- The chain pays the first receiving address of account 0 of the seed
    -- on Bitcoin, m/44'/0'/0', whose keys hold Bitcoin Cash as well
    it "finds the Bitcoin Cash at the bip44 accounts of Bitcoin, and looks for no SegWit account there" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore (emptyStore {fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 1 50000]}))
      runExceptT (foreignFunds ctx smallGap store bch (testSeed ctx) 1 noQuestion) `shouldReturn` Right [ForeignFunds Bip44OfBitcoin 0 1 50000]
      windows <- readIORef asked
      map (take 1) windows `shouldBe` [[externalAddr 0], [externalAddr 2], [internalAddr 0], [account1Addr]]

    it "asks nothing on the test networks of Bitcoin Cash, whose coin type is that of the test networks of Bitcoin" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore (emptyStore {fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 1 50000]}))
      runExceptT (foreignFunds ctx smallGap store bchTest (testSeed ctx) 1 noQuestion) `shouldReturn` Right []
      readIORef asked `shouldReturn` []

    it "fails when the store fails" $
      runExceptT (foreignFunds ctx smallGap ((toStore emptyStore) {storeBalances = \_ -> throwError "The store is down"}) btc (testSeed ctx) 1 noQuestion)
        `shouldReturn` Left "The store is down"

    it "asks on the chains of an account of a kind hw does not hold, named by its path and its network" $
      runExceptT (foreignFunds ctx cfg everyAddressUsed btc (testSeed ctx) 1 stopLooking)
        `shouldReturn` Left "The store reports used addresses as far as 1000 on the receiving chain of the account /49'/0'/0' of the seed on btc, and more beyond. The scan stopped there."

    -- The store reports the bip49 accounts 0 to 19 of the seed paid at
    -- their first receiving address
    it "asks whether to go on after 20 accounts of a kind the store reports paid, as for the accounts hw holds, and stops when the answer is no" $ do
      firstAddrs <- firstAddrsOfWrappedAccounts ctx 20
      runExceptT (foreignFunds ctx cfg (toStore (paidStore ctx [(addr, 1000) | addr <- firstAddrs])) btc (testSeed ctx) 1 stopLooking)
        `shouldReturn` Left "The store reports payments to 20 accounts of the seed on btc, as far as the account /49'/0'/19'. The search stopped there."

  describe "Watching the gap of addresses" $ do
    -- A gap of 3 instead of the default 20: the sync looks at the three
    -- addresses past the ones the account has, on each chain
    let smallGap = cfg {configGap = 3}
        watchWith store accId = sync ctx smallGap (toStore store) btc accId False noQuestion

    it "finds a payment to an address past the ones generated, and generates up to it" $
      inDatabaseE $ do
        accId <- account
        res <- watchWith (paidStore ctx [(externalAddr 3, 50000)]) accId
        liftIO $ (syncResNewAddrs res, dBAccountExternal (syncResAccount res)) `shouldBe` (3, 4)
        addressPath btc accId (externalAddr 3) `dbShouldBeE` Just (Deriv :/ 0 :/ 3)
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        coins accId `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        balances accId `dbShouldBeE` (50000, 0, 1)

    it "keeps looking while each window finds a used address" $
      inDatabaseE $ do
        accId <- account
        res <- watchWith (paidStore ctx [(externalAddr 2, 20000), (externalAddr 5, 30000)]) accId
        liftIO $ (syncResNewAddrs res, dBAccountExternal (syncResAccount res)) `shouldBe` (5, 6)
        summaries accId
          `dbShouldBeE` [ (Just (fakeTxHash 1), Store.BlockRef 800000 1, 20000),
                          (Just (fakeTxHash 2), Store.BlockRef 800000 2, 30000)
                        ]
        balances accId `dbShouldBeE` (50000, 0, 2)

    it "does not look past the gap" $
      inDatabaseE $ do
        accId <- account
        res <- watchWith (paidStore ctx [(externalAddr 4, 50000)]) accId
        liftIO $ (syncResNewAddrs res, dBAccountExternal (syncResAccount res)) `shouldBe` (0, 1)
        summaries accId `dbShouldBeE` []
        balances accId `dbShouldBeE` (0, 0, 0)

    it "finds change made by another wallet of the same seed" $
      inDatabaseE $ do
        accId <- account
        res <- watchWith (paidStore ctx [(internalAddr 1, 10000)]) accId
        liftIO $ (syncResNewAddrs res, dBAccountInternal (syncResAccount res)) `shouldBe` (2, 2)
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 10000)]
        balances accId `dbShouldBeE` (10000, 0, 1)
        -- /1/0 was never used: it is the next change address
        (dBAddressIndex <$> changeAddress ctx cfg accId) `dbShouldBeE` 0

    it "asks about the gap of each chain once when nothing is in it" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        _ <- sync ctx smallGap (notingBalanceRequests asked (toStore emptyStore)) btc accId False noQuestion
        liftIO $ readIORef asked `shouldReturn` [take 3 (drop 1 externalAddrs), take 3 internalAddrs, [externalAddr 0]]

    -- The store reports every address used, as one that lies does: the
    -- sync asks before it generates a thousand addresses, and a no leaves
    -- the account as it was
    it "asks before generating a thousand addresses the store reports used past the ones the account has, and generates none when the answer is no" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        lift $
          dbShouldFailWith "The store reports used addresses as far as 1002 past the 1 receiving address the account main has, and more beyond. The scan stopped there." $
            sync ctx smallGap everyAddressUsed btc accId False (answering asked 0)
        liftIO $ readIORef asked `shouldReturn` ["The store reports used addresses as far as 1002 past the 1 receiving address the account main has, and more beyond."]
        (dBAccountExternal <$> getAccountById accId) `dbShouldBeE` 1

  describe "Choosing the transactions to fetch" $ do
    -- The store lists the payment of 50000 to an address as transaction 1
    -- in block 800000; the wallet stores a balance with a transaction count
    let payment = Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)
        listing addr count refs = (Store.Balance addr 50000 0 1 count 50000, refs)
        stored addr count = Map.fromList [(addr, AddressBalance 50000 0 1 count 50000)]
        knownIn block = Map.fromList [(fakeTxHash 1, block)]

    it "fetches a listed transaction it does not know" $
      transactionsToFetch Map.empty [] (stored (externalAddr 0) 1) [listing (externalAddr 0) 1 [payment]]
        `shouldBe` [fakeTxHash 1]

    it "leaves alone a known transaction listed in its block" $
      transactionsToFetch (knownIn (Store.BlockRef 800000 1)) [] (stored (externalAddr 0) 1) [listing (externalAddr 0) 1 [payment]]
        `shouldBe` []

    it "fetches a known transaction again when it moved to another block" $
      transactionsToFetch (knownIn (Store.BlockRef 800001 3)) [] (stored (externalAddr 0) 1) [listing (externalAddr 0) 1 [payment]]
        `shouldBe` [fakeTxHash 1]

    it "fetches the unconfirmed transactions again, listed or not" $
      transactionsToFetch (knownIn (Store.MemRef 1700000000)) [] Map.empty []
        `shouldBe` [fakeTxHash 1]

    it "fetches every transaction of an address that has transactions for the first time" $
      -- The known payment also paid this address, which its summary does
      -- not show yet: it is rebuilt along with the new transaction
      transactionsToFetch (knownIn (Store.BlockRef 800000 1)) [] (stored (externalAddr 1) 0) [listing (externalAddr 1) 2 [payment, Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 3)]]
        `shouldMatchList` [fakeTxHash 1, fakeTxHash 3]

    -- As the step to version 4 of the database copied it, before hw kept
    -- the time
    it "fetches a summary without its time again, its address unchanged" $
      transactionsToFetch (knownIn (Store.BlockRef 800000 1)) [fakeTxHash 1] (stored (externalAddr 0) 1) []
        `shouldBe` [fakeTxHash 1]

    it "names a transaction once when two addresses list it" $
      transactionsToFetch Map.empty [] (stored (externalAddr 0) 1 <> stored (externalAddr 1) 1) [listing (externalAddr 0) 1 [payment], listing (externalAddr 1) 1 [payment]]
        `shouldBe` [fakeTxHash 1]

  describe "Telling the dead transactions" $ do
    -- Two known transactions; the store still lists the second
    let known = Set.fromList [fakeTxHash 1, fakeTxHash 2]
        alive = creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000
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
      changedBalances [unchanged, paid] stored `shouldBe` [paid]

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
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
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

-- | The old payment of 10000 to externalAddr 1 at a place between the two
-- payments: in block 800001, after the one of 30000
latePaymentStore :: Ctx -> FakeStore
latePaymentStore ctx =
  (oldPaymentStore ctx)
    { fakeTxRefs =
        fakeTxRefs (twoPaymentsStore ctx)
          <> [(externalAddr 1, [Store.TxRef lateBlock (fakeTxHash 4)])],
      fakeTransactions =
        fakeTransactions (twoPaymentsStore ctx)
          <> [creditTx ctx (fakeTxHash 4) lateBlock (externalAddr 1) 10000]
    }
  where
    lateBlock = Store.BlockRef 800001 5

-- | The two payments, with transaction 6 between them: 40000 in block
-- 800001, after the one of 30000
threePaymentsStore :: Ctx -> FakeStore
threePaymentsStore ctx =
  (twoPaymentsStore ctx)
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
        fakeTransactions (twoPaymentsStore ctx)
          <> [creditTx ctx (fakeTxHash 6) (Store.BlockRef 800001 5) (externalAddr 0) 40000],
      fakeUnspents =
        fakeUnspents (twoPaymentsStore ctx)
          <> [coinOf ctx (fakeTxHash 6) (Just 800001) (externalAddr 0) 40000],
      fakeRawTransactions =
        fakeRawTransactions (twoPaymentsStore ctx)
          <> [(fakeTxHash 6, fundingTx ctx 6 [(externalAddr 0, 40000)])]
    }

-- | After a reorganisation that left transaction 6 out of the chain, and
-- kept the other two
middleLeftOutStore :: Ctx -> FakeStore
middleLeftOutStore ctx =
  (twoPaymentsStore ctx)
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7]
    }

-- | After a reorganisation that sent transaction 6 back to the mempool
backInMempoolStore :: Ctx -> FakeStore
backInMempoolStore ctx =
  (threePaymentsStore ctx)
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
        fakeTransactions (twoPaymentsStore ctx)
          <> [creditTx ctx (fakeTxHash 6) (Store.MemRef 1700000000) (externalAddr 0) 40000],
      fakeUnspents =
        fakeUnspents (twoPaymentsStore ctx)
          <> [coinOf ctx (fakeTxHash 6) Nothing (externalAddr 0) 40000]
    }

-- | The two payments, with transaction 8 spending the coin of the 20000 on
-- another wallet, 19000 and a fee of 1000, in the block of that payment
spentPaymentStore :: Ctx -> FakeStore
spentPaymentStore ctx =
  (twoPaymentsStore ctx)
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
        fakeTransactions (twoPaymentsStore ctx)
          <> [spendingStoreTx ctx (fakeTxHash 8) (Store.BlockRef 800002 5) (fakeTxHash 3, 0) (externalAddr 0) [(otherAddr 0, 19000)]],
      fakeUnspents = [coinOf ctx (fakeTxHash 2) (Just 800001) (externalAddr 0) 30000]
    }

-- | After a reorganisation that put transaction 9, a payment of 15000, in
-- the place of the payment of 20000
rivalStore :: Ctx -> FakeStore
rivalStore ctx =
  (leftOutStore ctx)
    { fakeBalances = [Store.Balance (externalAddr 0) 45000 0 2 2 45000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800002 1) (fakeTxHash 9),
              Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 2)
            ]
          )
        ],
      fakeTransactions =
        fakeTransactions (leftOutStore ctx)
          <> [creditTx ctx (fakeTxHash 9) (Store.BlockRef 800002 1) (externalAddr 0) 15000],
      fakeUnspents =
        fakeUnspents (leftOutStore ctx)
          <> [coinOf ctx (fakeTxHash 9) (Just 800002) (externalAddr 0) 15000],
      fakeRawTransactions =
        fakeRawTransactions (twoPaymentsStore ctx)
          <> [(fakeTxHash 9, fundingTx ctx 9 [(externalAddr 0, 15000)])]
    }

-- | After a reorganisation that moved the payment of 20000 within its
-- block, with the balances unchanged
oneMovedStore :: Ctx -> FakeStore
oneMovedStore ctx =
  (twoPaymentsStore ctx)
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
        [ creditTx ctx (fakeTxHash 2) (Store.BlockRef 800001 1) (externalAddr 0) 30000,
          creditTx ctx (fakeTxHash 3) (Store.BlockRef 800002 4) (externalAddr 0) 20000
        ]
    }

-- | The two payments and two unconfirmed ones: transaction 6 pays 10000,
-- then transaction 7 pays 5000
twoUnconfirmedStore :: Ctx -> FakeStore
twoUnconfirmedStore ctx =
  (twoPaymentsStore ctx)
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
        fakeTransactions (twoPaymentsStore ctx)
          <> [ creditTx ctx (fakeTxHash 6) (Store.MemRef 1700000000) (externalAddr 0) 10000,
               creditTx ctx (fakeTxHash 7) (Store.MemRef 1700000100) (externalAddr 0) 5000
             ],
      fakeUnspents =
        fakeUnspents (twoPaymentsStore ctx)
          <> [ coinOf ctx (fakeTxHash 6) Nothing (externalAddr 0) 10000,
               coinOf ctx (fakeTxHash 7) Nothing (externalAddr 0) 5000
             ],
      fakeRawTransactions =
        fakeRawTransactions (twoPaymentsStore ctx)
          <> [ (fakeTxHash 6, fundingTx ctx 6 [(externalAddr 0, 10000)]),
               (fakeTxHash 7, fundingTx ctx 7 [(externalAddr 0, 5000)])
             ]
    }

-- | The unconfirmed transaction 6 replaced: the store still has it, marked
-- as deleted, and the address has lost its 10000
oneReplacedStore :: Ctx -> FakeStore
oneReplacedStore ctx =
  (twoUnconfirmedStore ctx)
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
        fakeTransactions (twoPaymentsStore ctx)
          <> [ replaced $ creditTx ctx (fakeTxHash 6) (Store.MemRef 1700000000) (externalAddr 0) 10000,
               creditTx ctx (fakeTxHash 7) (Store.MemRef 1700000100) (externalAddr 0) 5000
             ],
      fakeUnspents =
        fakeUnspents (twoPaymentsStore ctx)
          <> [coinOf ctx (fakeTxHash 7) Nothing (externalAddr 0) 5000]
    }

-- | Transaction 5 paid 30000 to externalAddr 0 and 20000 to externalAddr 1,
-- confirmed at height 800000, while the wallet knows externalAddr 0 only
payoutStore :: Ctx -> FakeStore
payoutStore ctx =
  FakeStore
    { fakeBest = blockAt 800002 1,
      fakeBlocks = [blockAt 800002 1],
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
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
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
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
      fakeLatestBlocks = [],
      fakeBlockTxs = [],
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

-- | The raw transaction of a payout of 50000 to externalAddr 0 and 30000
-- to externalAddr 1, and its hash
realPayoutTx :: Ctx -> Tx
realPayoutTx ctx = fundingTx ctx 2 [(externalAddr 0, 50000), (externalAddr 1, 30000)]

realPayoutId :: Ctx -> TxHash
realPayoutId = txHash . realPayoutTx

-- | The payout listed under the hash of its raw transaction, confirmed at
-- height 800000, both coins unspent
realPayoutStore :: Ctx -> FakeStore
realPayoutStore ctx =
  emptyStore
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 50000 0 1 1 50000,
          Store.Balance (externalAddr 1) 30000 0 1 1 30000
        ],
      fakeTxRefs = [(externalAddr 0, [payoutRef]), (externalAddr 1, [payoutRef])],
      fakeTransactions = [realPayout],
      fakeUnspents =
        [ coinAt ctx tid 0 (Just 800000) (externalAddr 0) 50000,
          coinAt ctx tid 1 (Just 800000) (externalAddr 1) 30000
        ],
      fakeRawTransactions = [(tid, realPayoutTx ctx)]
    }
  where
    tid = realPayoutId ctx
    payoutRef = Store.TxRef (Store.BlockRef 800000 1) tid
    realPayout = payoutTx ctx tid (Store.BlockRef 800000 1) [(externalAddr 0, 50000), (externalAddr 1, 30000)]

-- | The payout with its first coin spent by transaction 8, at height 800001
realPayoutFirstSpentStore :: Ctx -> FakeStore
realPayoutFirstSpentStore ctx =
  (realPayoutStore ctx)
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 0 0 0 2 50000,
          Store.Balance (externalAddr 1) 30000 0 1 1 30000
        ],
      fakeTxRefs = [(externalAddr 0, [spendRef 8, payoutRef]), (externalAddr 1, [payoutRef])],
      fakeTransactions =
        (realPayoutStore ctx).fakeTransactions
          <> [spendingStoreTx ctx (fakeTxHash 8) (Store.BlockRef 800001 1) (tid, 0) (externalAddr 0) [(otherAddr 1, 49000)]],
      fakeUnspents = [coinAt ctx tid 1 (Just 800000) (externalAddr 1) 30000]
    }
  where
    tid = realPayoutId ctx
    payoutRef = Store.TxRef (Store.BlockRef 800000 1) tid
    spendRef n = Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash n)

-- | The payout with both coins spent, by transactions 8 and 9, at height
-- 800001
realPayoutAllSpentStore :: Ctx -> FakeStore
realPayoutAllSpentStore ctx =
  (realPayoutFirstSpentStore ctx)
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 0 0 0 2 50000,
          Store.Balance (externalAddr 1) 0 0 0 2 30000
        ],
      fakeTxRefs = [(externalAddr 0, [spendRef 8, payoutRef]), (externalAddr 1, [spendRef 9, payoutRef])],
      fakeTransactions =
        (realPayoutFirstSpentStore ctx).fakeTransactions
          <> [spendingStoreTx ctx (fakeTxHash 9) (Store.BlockRef 800001 1) (tid, 1) (externalAddr 1) [(otherAddr 1, 29000)]],
      fakeUnspents = []
    }
  where
    tid = realPayoutId ctx
    payoutRef = Store.TxRef (Store.BlockRef 800000 1) tid
    spendRef n = Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash n)

-- | The wallet's own payment from the coin of 'realPaymentStore', signed:
-- 40000 to otherAddr 0 and 9000 of change to internalAddr 0, 1000 of fee
changeSpendTx :: Ctx -> Tx
changeSpendTx ctx =
  txSignDataTx $
    signedByWallet ctx $
      TxSignData
        (spendingTx ctx [(realPaymentId ctx, 0)] [(otherAddr 0, 40000), (internalAddr 0, 9000)])
        [realPaymentTx ctx]
        [Deriv :/ 0 :/ 0]
        [Deriv :/ 1 :/ 0]
        Nothing
        Nothing [] noLabel

-- | The payment of 'realPaymentStore' spent by 'changeSpendTx', confirmed
-- at height 800001: the coin is gone, the change is a coin at internalAddr 0
changeSpentStore :: Ctx -> FakeStore
changeSpentStore ctx =
  (realPaymentStore ctx)
    { fakeBalances =
        [ Store.Balance (externalAddr 0) 0 0 0 2 50000,
          Store.Balance (internalAddr 0) 9000 0 1 1 9000
        ],
      fakeTxRefs =
        [ (externalAddr 0, [spendRef, paymentRef]),
          (internalAddr 0, [spendRef])
        ],
      fakeTransactions =
        [ creditTx ctx (realPaymentId ctx) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          spendingStoreTx ctx spend (Store.BlockRef 800001 1) (realPaymentId ctx, 0) (externalAddr 0) [(otherAddr 0, 40000), (internalAddr 0, 9000)]
        ],
      fakeUnspents = [coinAt ctx spend 1 (Just 800001) (internalAddr 0) 9000],
      fakeRawTransactions = [(realPaymentId ctx, realPaymentTx ctx), (spend, changeSpendTx ctx)]
    }
  where
    spend = txHash (changeSpendTx ctx)
    spendRef = Store.TxRef (Store.BlockRef 800001 1) spend
    paymentRef = Store.TxRef (Store.BlockRef 800000 1) (realPaymentId ctx)

-- | A payment of 50000 to externalAddr 0 of the test wallet, as transaction
-- 1 at height 800000, and one of 30000 to the first address of the second
-- wallet, as transaction 2 at height 800001
twoWalletsStore :: Ctx -> FakeStore
twoWalletsStore ctx =
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
        [ creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          creditTx ctx (fakeTxHash 2) (Store.BlockRef 800001 1) externalAddr2 30000
        ],
      fakeUnspents =
        [ coinOf ctx (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000,
          coinOf ctx (fakeTxHash 2) (Just 800001) externalAddr2 30000
        ],
      fakeRawTransactions =
        [ (fakeTxHash 1, fundingTx ctx 1 [(externalAddr 0, 50000)]),
          (fakeTxHash 2, fundingTx ctx 2 [(externalAddr2, 30000)])
        ]
    }

-- | The payments of the two wallets after a reorganisation that moved the
-- payment of the second wallet, transaction 2, to block 800003, with the
-- balances unchanged
secondMovedStore :: Ctx -> FakeStore
secondMovedStore ctx =
  (twoWalletsStore ctx)
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeTxRefs =
        [ (externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)]),
          (externalAddr2, [Store.TxRef (Store.BlockRef 800003 3) (fakeTxHash 2)])
        ],
      fakeTransactions =
        [ creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          creditTx ctx (fakeTxHash 2) (Store.BlockRef 800003 3) externalAddr2 30000
        ],
      fakeUnspents =
        [ coinOf ctx (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000,
          coinOf ctx (fakeTxHash 2) (Just 800003) externalAddr2 30000
        ]
    }

-- | The payments of the two wallets after a reorganisation that left the
-- payment of the second wallet, transaction 2, out of the chain
secondLeftOutStore :: Ctx -> FakeStore
secondLeftOutStore ctx =
  (twoWalletsStore ctx)
    { fakeBest = blockAt 800003 7,
      fakeBlocks = [blockAt 800002 9, blockAt 800003 7],
      fakeBalances = [Store.Balance (externalAddr 0) 50000 0 1 1 50000],
      fakeTxRefs = [(externalAddr 0, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)])],
      fakeTransactions = [creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000],
      fakeUnspents = [coinOf ctx (fakeTxHash 1) (Just 800000) (externalAddr 0) 50000]
    }

-- | A payment of 50000 to the first address of the Bitcoin Cash account,
-- as transaction 1, confirmed at height 800000
cashPaymentStore :: Ctx -> FakeStore
cashPaymentStore ctx =
  emptyStore
    { fakeBalances = [Store.Balance bchExternalAddr 50000 0 1 1 50000],
      fakeTxRefs = [(bchExternalAddr, [Store.TxRef (Store.BlockRef 800000 1) (fakeTxHash 1)])],
      fakeTransactions = [creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) bchExternalAddr 50000],
      fakeUnspents = [coinOf ctx (fakeTxHash 1) (Just 800000) bchExternalAddr 50000],
      fakeRawTransactions = [(fakeTxHash 1, fundingTx ctx 1 [(bchExternalAddr, 50000)])]
    }

-- | The payments of 'discoveredStore': the address paid, the amount, the
-- height of the block, and the raw transaction, listed under its own hash
discoveredPayments :: Ctx -> [(Address, Word64, Word32, Tx)]
discoveredPayments ctx =
  [ (externalAddr 0, 50000, 800000, fundingTx ctx 1 [(externalAddr 0, 50000)]),
    (externalAddr 4, 20000, 800001, fundingTx ctx 2 [(externalAddr 4, 20000)]),
    (internalAddr 1, 10000, 800001, fundingTx ctx 3 [(internalAddr 1, 10000)])
  ]

-- | Payments to three addresses of an account that has not generated any
-- address yet: externalAddr 0 and 4, and internalAddr 1
discoveredStore :: Ctx -> FakeStore
discoveredStore ctx =
  emptyStore
    { fakeBalances = [Store.Balance addr amount 0 1 1 amount | (addr, amount, _, _) <- paid],
      fakeTxRefs = [(addr, [Store.TxRef (Store.BlockRef height 1) (txHash tx)]) | (addr, _, height, tx) <- paid],
      fakeTransactions = [creditTx ctx (txHash tx) (Store.BlockRef height 1) addr amount | (addr, amount, height, tx) <- paid],
      fakeUnspents = [coinOf ctx (txHash tx) (Just (fromIntegral height)) addr (fromIntegral amount) | (addr, amount, height, tx) <- paid],
      fakeRawTransactions = [(txHash tx, tx) | (_, _, _, tx) <- paid]
    }
  where
    paid = discoveredPayments ctx

-- | Payments of the given amounts to the given addresses, one transaction
-- each, numbered from 1, all confirmed in block 800000
paidStore :: Ctx -> [(Address, Word64)] -> FakeStore
paidStore ctx paid =
  emptyStore
    { fakeBalances = [Store.Balance addr amount 0 1 1 amount | (addr, amount, _) <- numbered],
      fakeTxRefs = [(addr, [Store.TxRef (Store.BlockRef 800000 (fromIntegral n)) (fakeTxHash n)]) | (addr, _, n) <- numbered],
      fakeTransactions = [creditTx ctx (fakeTxHash n) (Store.BlockRef 800000 (fromIntegral n)) addr amount | (addr, amount, n) <- numbered],
      fakeUnspents = [coinOf ctx (fakeTxHash n) (Just 800000) addr (fromIntegral amount) | (addr, amount, n) <- numbered],
      fakeRawTransactions = [(fakeTxHash n, fundingTx ctx n [(addr, fromIntegral amount)]) | (addr, amount, n) <- numbered]
    }
  where
    numbered = [(addr, amount, n) | ((addr, amount), n) <- zip paid [1 :: Word8 ..]]

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

-- | The coin of 'pendingTx': 51000 paid to externalAddr 0
pendingParent :: Ctx -> Tx
pendingParent ctx = fundingTx ctx 9 [(externalAddr 0, 51000)]

-- | A transaction of ours, signed and waiting to be sent: it moves the coin
-- of 'pendingParent' back to externalAddr 0, for a fee of 1000
pendingTx :: Ctx -> Tx
pendingTx ctx =
  txSignDataTx $
    signedByWallet ctx $
      TxSignData
        (spendingTx ctx [(txHash (pendingParent ctx), 0)] [(externalAddr 0, 50000)])
        [pendingParent ctx]
        [Deriv :/ 0 :/ 0]
        [Deriv :/ 0 :/ 0]
        Nothing
        Nothing [] noLabel

-- | The coin of 'realPaymentStore' spent to otherAddr 0, with its signing
-- data as the wallet keeps it, signed
signedSpend :: Ctx -> TxSignData
signedSpend ctx = signedByWallet ctx (unsignedSpend ctx)

unsignedSpend :: Ctx -> TxSignData
unsignedSpend ctx =
  TxSignData
    (spendingTx ctx [(realPaymentId ctx, 0)] [(otherAddr 0, 40000)])
    [realPaymentTx ctx]
    [Deriv :/ 0 :/ 0]
    []
    (Just 10)
    Nothing [] noLabel

-- | The payment of 'realPaymentStore' spent by somebody else's transaction
-- 8, confirmed at height 800001: the coin is gone, and the store names
-- transaction 8 as the spender of the payment's output
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
        [ withSpender 0 (fakeTxHash 8, 0) $ creditTx ctx (realPaymentId ctx) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          spendingStoreTx ctx (fakeTxHash 8) (Store.BlockRef 800001 1) (realPaymentId ctx, 0) (externalAddr 0) [(otherAddr 1, 49000)]
        ],
      fakeUnspents = []
    }

-- | The pending transaction has reached the store
-- | The payment of 'realPaymentStore' spent by the transaction of
-- 'signedSpend' under another id, 9, confirmed at height 800001: a miner
-- re-encoded its signatures. Its inputs and outputs are those of the
-- transaction the wallet signed, and so is its nosigHash.
malleatedStore :: Ctx -> FakeStore
malleatedStore ctx =
  (realPaymentStore ctx)
    { fakeBalances = [Store.Balance (externalAddr 0) 0 0 0 2 50000],
      fakeTxRefs =
        [ ( externalAddr 0,
            [ Store.TxRef (Store.BlockRef 800001 1) (fakeTxHash 9),
              Store.TxRef (Store.BlockRef 800000 1) (realPaymentId ctx)
            ]
          )
        ],
      fakeTransactions =
        [ withSpender 0 (fakeTxHash 9, 0) $ creditTx ctx (realPaymentId ctx) (Store.BlockRef 800000 1) (externalAddr 0) 50000,
          storeTx (fakeTxHash 9) (Store.BlockRef 800001 1) [inputFrom ctx (realPaymentId ctx, 0) (externalAddr 0) 50000] [outputTo ctx (otherAddr 0) 40000] 10000
        ],
      fakeUnspents = []
    }

-- | The coin of 'realPaymentStore' swept by the other account, account 0
-- of the second test wallet, to its first receiving address, less a fee of
-- 1000
sweepToOther :: Ctx -> TxSignData
sweepToOther ctx =
  TxSignData (spendingTx ctx [(realPaymentId ctx, 0)] [(externalAddr2, 49000)]) [realPaymentTx ctx] [] [Deriv :/ 0 :/ 0] Nothing Nothing [] noLabel

-- | A payment of 10000 to somebody else's address, otherAddr 3: a coin
-- outside the wallet whose key the user holds, on a paper wallet for
-- instance
outsidePayment :: Ctx -> Tx
outsidePayment ctx = fundingTx ctx 9 [(otherAddr 3, 10000)]

-- | The sweep of that coin to the first receiving address of the account,
-- less a fee of 1000
outsideSweep :: Ctx -> TxSignData
outsideSweep ctx =
  TxSignData (spendingTx ctx [(txHash (outsidePayment ctx), 0)] [(externalAddr 0, 9000)]) [outsidePayment ctx] [] [Deriv :/ 0 :/ 0] Nothing Nothing [] noLabel

-- | A store that knows the payment to otherAddr 3, its coin unspent, and
-- nothing of the account
outsideStore :: Ctx -> FakeStore
outsideStore ctx = emptyStore {fakeTransactions = [creditTx ctx (txHash (outsidePayment ctx)) (Store.BlockRef 800000 2) (otherAddr 3) 10000]}

-- | The same store once somebody else's transaction 8 spent that coin
outsideSpentStore :: Ctx -> FakeStore
outsideSpentStore ctx = emptyStore {fakeTransactions = withSpender 0 (fakeTxHash 8, 0) <$> (outsideStore ctx).fakeTransactions}

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
summaries :: DBAccountId -> ExceptT String (DB IO) [(Maybe TxHash, Store.BlockRef, Integer)]
summaries accId = do
  txInfos <- txsAfter accId Nothing Nothing
  return $ sortOn (\(h, _, _) -> txHashToHex <$> h) [(txInfoHash t, chainBlock (chainOf t), summaryAmount (txInfoSummary t)) | t <- txInfos]

-- | Each transaction txs lists, newest first, with the balance after it
listedBalances :: DBAccountId -> ExceptT String (DB IO) [(Maybe TxHash, Maybe Integer)]
listedBalances accId = do
  rows <- listedTxsAfter accId Nothing Nothing
  return [(txInfoHash (listedTxInfo listed), listedTxBalance listed) | (listed, _) <- rows]

-- | The dead summaries of the account, by transaction id
deadSummaries :: DBAccountId -> ExceptT String (DB IO) [Maybe TxHash]
deadSummaries accId = do
  txInfos <- txsAfter accId Nothing Nothing
  return $ sortOn (fmap txHashToHex) [txInfoHash t | t <- txInfos, chainDead (chainOf t)]

-- | The addresses of ours each summary pays, as sorted texts, in the order
-- of the transaction ids
paidAddresses :: DBAccountId -> ExceptT String (DB IO) [[Text]]
paidAddresses accId = do
  txInfos <- txsAfter accId Nothing Nothing
  return
    [ sort (mapMaybe (addrToText btc) (Map.keys (summaryMyOutputs (txInfoSummary t))))
      | t <- sortOn (fmap txHashToHex . txInfoHash) txInfos
    ]

-- | The coins of the account, in the order of their outpoints
coins :: DBAccountId -> ExceptT String (DB IO) [OutPoint]
coins accId = sortOn show . map jsonCoinOutpoint <$> coinsAfter btc accId AllCoins Nothing Nothing

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
  rows <- pendingTxs accId
  return [txHash (txSignDataTx (pendingTxData p)) | p <- rows, isSigned (txSignDataTx (pendingTxData p))]
