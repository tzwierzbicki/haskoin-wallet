{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The synchronisation of an account, played against a scripted store:
-- payments arriving, confirming, being replaced or dropped, a payment the
-- wallet made, the chain reorganising, a wallet with many coins or many
-- addresses, the raw transactions fetched only when missing and dropped
-- when no coin needs them, pending transactions, two accounts, another
-- network; the gap of addresses watched past the ones the account has; the
-- discovery of the used addresses of an account; and the decisions of a
-- sync on their own: what to fetch, what is dead, which balances changed
module Haskoin.Wallet.SyncSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (find, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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
import Haskoin.Wallet.RawTx (hasRawTx, insertRawTx)
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo (ChainStatus (..), MyOutputs (..), TxInfo (..), TxSummary (..), TxType (..), txInfoHash, txsPage)
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
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
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
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]

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
        lift (getBest btc) `dbShouldBeE` Just (headerHash (blockAt 800003 7).header, 800003)

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
        (length <$> summaries accId) `dbShouldBeE` 200
        balances accId `dbShouldBeE` (200000, 0, 200)

    it "lists the transactions of more addresses than it asks about at a time" $
      inDatabaseE $ do
        accId <- account
        -- externalAddr 1 to externalAddr 6 as well
        replicateM_ 6 $ genExtAddress ctx cfg accId ""
        let twoAtATime = cfg {configParallelRequests = 2}
        res <- sync ctx twoAtATime (toStore (sevenAddressesStore ctx)) btc accId False
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
        _ <- sync ctx cfg (toStore (realPaymentStore ctx)) btc accId True
        rawTransactionCount `dbShouldBeE` 1
        lift (hasRawTx (realPaymentId ctx)) `dbShouldBeE` True

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
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]

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
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, False, False, True))]
        -- Nothing is left to free but the change address, which it had none of
        deleteTx ctx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (0, 0)
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0)) `dbShouldBeE` []

    it "follows a payment the wallet made: the coin goes, the change comes, the summary is a debit" $
      inDatabaseE $ do
        accId <- account
        -- The change address, /1/0, and the coin to spend
        _ <- changeAddress ctx cfg accId
        _ <- syncWith (realPaymentStore ctx) accId
        -- The user prepared, signed and sent the payment, as sendtx leaves
        -- it: pending, online, its raw transaction stored
        lift $ P.insert_ $ pendingRow accId (changeSpendTx ctx)
        lift (setPendingTxOnline (nosigHash (changeSpendTx ctx))) `dbShouldBeE` 1
        lift $ insertRawTx (changeSpendTx ctx)
        -- The store lists it, confirmed
        res <- syncWith (changeSpentStore ctx) accId
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 2)
        -- A debit of 41000: 40000 paid and 1000 of fee, 9000 of change to /1/0
        txInfos <- txsPage accId (Page 5 0)
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
        res <- sync ctx cfg (toStore (cashPaymentStore ctx)) bch accId False
        liftIO $ (syncResTxUpdates res, syncResCoinUpdates res) `shouldBe` (1, 1)
        summaries accId `dbShouldBeE` [(Just (fakeTxHash 1), Store.BlockRef 800000 1, 50000)]
        (map jsonCoinOutpoint <$> coinPage bch accId (Page 5 0)) `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]
        balances accId `dbShouldBeE` (50000, 0, 1)
        lift (getBest bch) `dbShouldBeE` Just (headerHash (blockAt 800002 1).header, 800002)
        lift (getBest btc) `dbShouldBeE` Nothing

  describe "Discovering the addresses of an account" $ do
    -- A recovery gap of 3 instead of the default 40: the scan looks at
    -- windows of three addresses, so the ten written-out addresses of the
    -- test wallet are enough. The gap of new addresses stays at 20 and
    -- plays no part.
    let small = cfg {configRecoveryGap = 3}
        discover store = runExceptT $ discoverAddrs ctx small (toStore store) (testAccountPub ctx)
        -- A store where the given addresses each have one transaction
        used addrs = emptyStore {fakeBalances = [Store.Balance addr 1000 0 1 1 1000 | addr <- addrs]}

    it "finds nothing in an empty store, after one window on each chain" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore emptyStore)
      runExceptT (discoverAddrs ctx small store (testAccountPub ctx)) `shouldReturn` Right (0, 0)
      readIORef asked `shouldReturn` [take 3 externalAddrs, take 3 internalAddrs]

    it "counts the addresses up to the last used one on each chain" $
      discover (used [externalAddr 0, externalAddr 4, internalAddr 1]) `shouldReturn` Right (5, 2)

    it "stops after a window without a used address, so a gap of three hides address 3" $
      discover (used [externalAddr 3]) `shouldReturn` Right (0, 0)

    it "looks at one more window past the last used address" $ do
      asked <- newIORef []
      let store = notingBalanceRequests asked (toStore (used [externalAddr 2]))
      runExceptT (discoverAddrs ctx small store (testAccountPub ctx)) `shouldReturn` Right (3, 0)
      readIORef asked `shouldReturn` [take 3 externalAddrs, take 3 (drop 3 externalAddrs), take 3 internalAddrs]

    it "counts an address whose coins are all spent as used" $
      discover (emptyStore {fakeBalances = [Store.Balance (externalAddr 1) 0 0 0 2 1000]}) `shouldReturn` Right (2, 0)

    it "ignores an address the store lists without transactions" $
      discover (emptyStore {fakeBalances = [Store.Balance (externalAddr 1) 0 0 0 0 0]}) `shouldReturn` Right (0, 0)

    it "fails when the store fails" $
      runExceptT (discoverAddrs ctx small ((toStore emptyStore) {storeBalances = \_ -> throwError "The store is down"}) (testAccountPub ctx))
        `shouldReturn` Left "The store is down"

    it "generates the discovered addresses and syncs them, as discoveracc does" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        let store = toStore (discoveredStore ctx)
        (e, i) <- discoverAddrs ctx small store (testAccountPub ctx)
        liftIO $ (e, i) `shouldBe` (5, 2)
        extendChainTo ctx accId AddrExternal e
        extendChainTo ctx accId AddrInternal i
        _ <- sync ctx small store btc accId True
        found <- summaries accId
        liftIO $ found `shouldMatchList` [(Just (txHash tx), Store.BlockRef height 1, fromIntegral amount) | (_, amount, height, tx) <- discoveredPayments ctx]
        held <- coins accId
        liftIO $ held `shouldMatchList` [OutPoint (txHash tx) 0 | (_, _, _, tx) <- discoveredPayments ctx]
        balances accId `dbShouldBeE` (80000, 0, 3)
        rawTransactionCount `dbShouldBeE` 3

  describe "Watching the gap of addresses" $ do
    -- A gap of 3 instead of the default 20: the sync looks at the three
    -- addresses past the ones the account has, on each chain
    let smallGap = cfg {configGap = 3}
        watchWith store accId = sync ctx smallGap (toStore store) btc accId False

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
        _ <- sync ctx smallGap (notingBalanceRequests asked (toStore emptyStore)) btc accId False
        liftIO $ readIORef asked `shouldReturn` [take 3 (drop 1 externalAddrs), take 3 internalAddrs, [externalAddr 0]]

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
    let known = Map.fromList [(fakeTxHash 1, Store.BlockRef 800000 1), (fakeTxHash 2, Store.BlockRef 800001 1)]
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

    it "counts a known transaction no address lists any more as dead, on a full sync only" $ do
      deadTransactions True known [] [] [listedRef] `shouldBe` [fakeTxHash 1]
      deadTransactions False known [] [] [listedRef] `shouldBe` []

  describe "The balances to update" $ do
    -- externalAddr 0 holds 50000 as stored; externalAddr 1 was empty and
    -- the store now reports 30000 unconfirmed
    let stored = Map.fromList [(externalAddr 0, AddressBalance 50000 0 1 1 50000), (externalAddr 1, AddressBalance 0 0 0 0 0)]
        unchanged = Store.Balance (externalAddr 0) 50000 0 1 1 50000
        paid = Store.Balance (externalAddr 1) 0 30000 1 1 30000

    it "keeps the addresses whose balance the store reports changed" $
      filterAddresses [unchanged, paid] stored `shouldBe` Right [paid]

    it "refuses an answer about other addresses than asked" $
      filterAddresses [unchanged] stored `shouldBe` Left "Sync: addresses do not match"

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
        Nothing

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
        Nothing

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
    Nothing

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
          spendingStoreTx ctx (fakeTxHash 8) (Store.BlockRef 800001 1) (realPaymentId ctx, 0) (externalAddr 0) [(otherAddr 1, 49000)]
        ],
      fakeUnspents = []
    }

pendingRow :: DBAccountId -> Tx -> DBPendingTx
pendingRow accId tx =
  DBPendingTx
    { dBPendingTxAccount = accId,
      dBPendingTxNosigHash = txHashToHex (nosigHash tx),
      dBPendingTxBlob = BS.toStrict $ Json.encode $ TxSignData tx [] [] [] Nothing Nothing,
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
summaries :: DBAccountId -> ExceptT String (DB IO) [(Maybe TxHash, Store.BlockRef, Integer)]
summaries accId = do
  txInfos <- txsPage accId (Page 500 0)
  return $ sortOn (\(h, _, _) -> txHashToHex <$> h) [(txInfoHash t, chainBlock (chainOf t), summaryAmount (txInfoSummary t)) | t <- txInfos]

-- | The dead summaries of the account, by transaction id
deadSummaries :: DBAccountId -> ExceptT String (DB IO) [Maybe TxHash]
deadSummaries accId = do
  txInfos <- txsPage accId (Page 500 0)
  return $ sortOn (fmap txHashToHex) [txInfoHash t | t <- txInfos, chainDead (chainOf t)]

-- | The addresses of ours each summary pays, as sorted texts, in the order
-- of the transaction ids
paidAddresses :: DBAccountId -> ExceptT String (DB IO) [[Text]]
paidAddresses accId = do
  txInfos <- txsPage accId (Page 500 0)
  return
    [ sort (mapMaybe (addrToText btc) (Map.keys (summaryMyOutputs (txInfoSummary t))))
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
  rows <- pendingTxs accId
  return [txHash (txSignDataTx (pendingTxData p)) | p <- rows, isSigned (txSignDataTx (pendingTxData p))]
