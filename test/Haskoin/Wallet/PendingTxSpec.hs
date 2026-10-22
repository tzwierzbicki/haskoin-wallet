{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

-- | The pending transactions of an account: prepared, held, signed in
-- place, sent and deleted
module Haskoin.Wallet.PendingTxSpec (spec) where

import Conduit (liftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Default (def)
import Data.Either
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe
import qualified Data.Set as Set
import Database.Esqueleto.Legacy hiding (isNothing)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Address
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Coin
import Haskoin.Wallet.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.PendingTx
import Haskoin.Wallet.RawTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxBuilding
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (signWalletTx)
import Haskoin.Wallet.Util
import System.Random (StdGen, mkStdGen)
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    let cfg = def :: Config
    describe "Pending transactions" $ pendingTxsSpec ctx cfg
    sendingSpec ctx

-- | A fixed seed for the transactions built in these tests
gen :: StdGen
gen = mkStdGen 0

-- | The signing data with the outputs of its transaction sorted by amount
unshuffled :: TxSignData -> TxSignData
unshuffled tsd = tsd {txSignDataTx = sortedOutputs (txSignDataTx tsd)}

checkFree :: Int -> Bool -> ExceptT String (DB IO) ()
checkFree i free =
  lift $
    ( dBAddressFree . entityVal . fromJust
        <$> P.getBy (UniqueAddress (internalAddrsText !! i))
    )
      `dbShouldBe` free

-- | A store that knows the transaction of the signing data, in its mempool
storeKnowing :: Ctx -> TxSignData -> FakeStore
storeKnowing ctx tsd =
  emptyStore {fakeTransactions = [creditTx ctx (txHash (txSignDataTx tsd)) (Store.MemRef 1700000000) (otherAddr 0) 1]}

-- | The store, counting the times it is asked for transactions
countingAsks :: IORef Int -> Store -> Store
countingAsks asks store =
  store
    { storeTransactions = \tids -> do
        liftIO $ modifyIORef asks (+ 1)
        storeTransactions store tids
    }

-- | A signed payment of 8000 to otherAddr 0 from a coin of 20000 at
-- externalAddr 0, stored as a pending transaction of the account
signedPending :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) (TxHash, TxSignData)
signedPending ctx cfg accId = do
  _ <- genExtAddress ctx cfg accId ""
  _ <- nextFreeIntAddr ctx cfg accId
  let fundTx1 = spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 20000)]
      coin1 = storeCoin ctx (txHash fundTx1, 0) (Just 0) (externalAddr 0) 20000
  lift $ insertRawTx fundTx1
  _ <- refreshCoins btc accId externalAddrs [coin1]
  unsigned <- buildTxSignData btc ctx cfg (mkStdGen 0) LeastWaste accId [(otherAddr 0, 8000)] 1 0 SenderPays 0 []
  (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
  pendingHash <- importPendingTx btc ctx accId signed
  return (pendingHash, signed)

sendingSpec :: Ctx -> Spec
sendingSpec ctx =
  describe "Sending a pending transaction" $ do
    let cfg = def :: Config
    it "marks it online once the store sees it, and stores its raw transaction" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, signed) <- signedPending ctx cfg accId
        (_, sent) <- sendPending ctx (const (toStore (storeKnowing ctx signed))) pendingHash
        liftIO $ txInfoPending sent `shouldBe` Just (TxInfoPending pendingHash True True False)
        ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just (TxInfoPending pendingHash True True False)]
        lift $ P.get (DBRawTxKey (txHashToHex (txHash (txSignDataTx signed)))) `dbShouldSatisfy` isJust
    it "records nothing when the store does not see it, and says so" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, _) <- signedPending ctx cfg accId
        lift $
          dbShouldFailWith "The transaction was handed to the network but the store has not seen it after 10 seconds. It stays signed and not online: run sendtx again later" $
            sendPending ctx (const (toStore emptyStore)) pendingHash
        ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just (TxInfoPending pendingHash True False False)]
    it "refuses a transaction that is not signed" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        _ <- genExtAddress ctx cfg accId ""
        let fundTx1 = spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 20000)]
            coin1 = storeCoin ctx (txHash fundTx1, 0) (Just 0) (externalAddr 0) 20000
        lift $ insertRawTx fundTx1
        _ <- refreshCoins btc accId externalAddrs [coin1]
        unsigned <- buildTxSignData btc ctx cfg (mkStdGen 0) LeastWaste accId [(otherAddr 0, 8000)] 1 0 SenderPays 0 []
        pendingHash <- importPendingTx btc ctx accId unsigned
        lift $
          dbShouldFailWith "The transaction is not signed" $
            sendPending ctx (const (toStore emptyStore)) pendingHash
    it "gives up after the asks it is allowed" $ do
      asks <- newIORef 0
      let tx = spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 8000)]
      runExceptT (broadcastWith 1000 3 (countingAsks asks (toStore emptyStore)) tx) `shouldReturn` Right False
      readIORef asks `shouldReturn` 3
    it "sees the transaction on the first ask" $ do
      asks <- newIORef 0
      let signed = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 8000)]) [] [] [] Nothing True
      runExceptT (broadcastWith 1000 3 (countingAsks asks (toStore (storeKnowing ctx signed))) (txSignDataTx signed))
        `shouldReturn` Right True
      readIORef asks `shouldReturn` 1
    it "abandons a sent transaction the store does not know, freeing its coin" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, _) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [True]
        deleteTx ctx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (1, 1)
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [False]
        ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0)) `dbShouldBeE` []

pendingTxsSpec :: Ctx -> Config -> Spec
pendingTxsSpec ctx cfg =
  it "can manage pending transactions" $ do
    inDatabaseE $ do
      (accId, _) <- testNewAcc ctx "test"
      lift $ updateBest btc (fakeBlockHash 0) 0
      -- Build some test data coins/txs
      (dBAddressAddress <$> genExtAddress ctx cfg accId "")
        `dbShouldBeE` head externalAddrsText
      (dBAddressAddress <$> nextFreeIntAddr ctx cfg accId)
        `dbShouldBeE` head internalAddrsText
      checkFree 0 True
      lift $ setAddrsFree AddrBusy [head internalAddrsText] `dbShouldBe` 1
      checkFree 0 False
      -- Two coins of 20000 and 10000, spent at 1 satoshi per byte with no
      -- dust limit: a transaction with one input pays 226 with two outputs,
      -- 192 with one
      let fundTx1 = spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 20000)]
          fundTx2 = spendingTx ctx [(fakeTxHash 1, 1)] [(internalAddr 0, 10000)]
          coin1 = storeCoin ctx (txHash fundTx1, 0) (Just 0) (externalAddr 0) 20000
          coin2 = storeCoin ctx (txHash fundTx2, 0) (Just 0) (internalAddr 0) 10000
      -- Insert the dependent transactions
      lift $ insertRawTx fundTx1
      lift $ insertRawTx fundTx2
      -- Insert two coins in the database
      (c, dbCoins) <- refreshCoins btc accId (externalAddrs <> internalAddrs) [coin1, coin2]
      liftIO $ c `shouldBe` 2
      let jsonCoin1 = forceRight $ toJsonCoin btc Nothing Set.empty (head dbCoins)
          jsonCoin2 = forceRight $ toJsonCoin btc Nothing Set.empty (dbCoins !! 1)
      tsd <- buildTxSignData btc ctx cfg gen LeastWaste accId [(otherAddr 0, 8000)] 1 0 SenderPays 1 []
      -- The outputs are compared by amount: the builder shuffles them
      liftIO $
        unshuffled tsd
          `shouldBe` TxSignData
            { txSignDataTx =
                spendingTx
                  ctx
                  [(txHash fundTx1, 0)]
                  [(otherAddr 0, 8000), (internalAddr 1, 11774)],
              txSignDataInputs = [fundTx1],
              txSignDataInputPaths = [Deriv :/ 0 :/ 0],
              txSignDataOutputPaths = [Deriv :/ 1 :/ 1],
              txSignDataFeeRate = Just 1,
              txSignDataSigned = False
            }
      -- Import the pending transaction
      checkFree 0 False
      checkFree 1 True
      -- The hash depends on the order of the shuffled outputs
      let h1 = nosigHash (txSignDataTx tsd)
      importPendingTx btc ctx accId tsd `dbShouldBeE` h1
      -- Check locked coins
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = False},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [Just $ TxInfoPending h1 False False False]
      -- Check address free status
      checkFree 0 False
      checkFree 1 False
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 2
      lift $
        dbShouldFailWith "The coins do not cover the amount and the fee" $
          buildTxSignData btc ctx cfg gen LeastWaste accId [(otherAddr 1, 12000)] 1 0 SenderPays 1 []
      lift $
        dbShouldFailWith "The transaction already exists" $
          importPendingTx btc ctx accId tsd
      -- Create second pending transaction
      tsd2 <- buildTxSignData btc ctx cfg gen LeastWaste accId [(otherAddr 1, 7000)] 1 0 SenderPays 1 []
      liftIO $
        unshuffled tsd2
          `shouldBe` TxSignData
            { txSignDataTx =
                spendingTx
                  ctx
                  [(txHash fundTx2, 0)]
                  [(internalAddr 2, 2774), (otherAddr 1, 7000)],
              txSignDataInputs = [fundTx2],
              txSignDataInputPaths = [Deriv :/ 1 :/ 0],
              txSignDataOutputPaths = [Deriv :/ 1 :/ 2],
              txSignDataFeeRate = Just 1,
              txSignDataSigned = False
            }
      -- Import the second transaction
      checkFree 2 True
      let h2 = nosigHash (txSignDataTx tsd2)
      importPendingTx btc ctx accId tsd2 `dbShouldBeE` h2
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [ Just $ TxInfoPending h2 False False False,
                        Just $ TxInfoPending h1 False False False
                      ]
      checkFree 0 False
      checkFree 1 False
      checkFree 2 False
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 3
      lift $
        dbShouldFailWith "The coins do not cover the amount and the fee" $
          buildTxSignData btc ctx cfg gen LeastWaste accId [(otherAddr 2, 1000)] 1 0 SenderPays 1 []
      -- Delete first transaction
      deletePendingTx ctx h1 `dbShouldBeE` (1, 1)
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = False}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [Just $ TxInfoPending h2 False False False]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 1
      -- Create transaction with no change
      tsd3 <- buildTxSignData btc ctx cfg gen LeastWaste accId [(otherAddr 2, 19808)] 1 0 SenderPays 1 []
      liftIO $
        unshuffled tsd3
          `shouldBe` TxSignData
            { txSignDataTx =
                spendingTx
                  ctx
                  [(txHash fundTx1, 0)]
                  [(otherAddr 2, 19808)],
              txSignDataInputs = [fundTx1],
              txSignDataInputPaths = [Deriv :/ 0 :/ 0],
              txSignDataOutputPaths = [],
              txSignDataFeeRate = Just 1,
              txSignDataSigned = False
            }
      -- One output: nothing to shuffle, so the hash is fixed
      let h3 = "720a9cfea170cc699458a97fe34da547dde30d9d54e5210cd88f1b76b139696c"
      importPendingTx btc ctx accId tsd3 `dbShouldBeE` h3
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [ Just $ TxInfoPending h3 False False False,
                        Just $ TxInfoPending h2 False False False
                      ]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 1
      -- Import a signed transaction
      let resE = signWalletTx btc ctx tsd2 (testAccountKey)
      liftIO $ resE `shouldSatisfy` isRight
      let tsd2' = fst $ forceRight resE
      liftIO $ txSignDataSigned tsd2' `shouldBe` True
      importPendingTx btc ctx accId tsd2' `dbShouldBeE` h2
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [ Just $ TxInfoPending h3 False False False,
                        Just $ TxInfoPending h2 True False False
                      ]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      lift $ do
        dbShouldFailWith "The transaction already exists" $
          importPendingTx btc ctx accId tsd2'
        dbShouldFailWith "Can not replace a signed transaction with an unsigned one" $
          importPendingTx btc ctx accId tsd2
      -- Set tsd2 to online
      lift $ do
        setPendingTxOnline h2 `dbShouldBe` 1
        dbShouldFailWith "The transaction is already online" $
          importPendingTx btc ctx accId tsd2'
        -- An online transaction is deleted only when the store does not know it
        dbShouldFailWith "The transaction is in the network's mempool or in the chain: syncacc retires it once it is listed" $
          deleteTx ctx (const (toStore (storeKnowing ctx tsd2'))) h2
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [ Just $ TxInfoPending h3 False False False,
                        Just $ TxInfoPending h2 True True False
                      ]
      -- Retired by the sync: nothing locks its coin any more (the sync
      -- deletes the coin itself once the store shows it spent)
      lift $ deletePendingTxOnline $ DBPendingTxKey $ txHashToHex h2
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = False},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [Just $ TxInfoPending h3 False False False]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      -- Check for other errors
      let fundTx3 = spendingTx ctx [(fakeTxHash 2, 0)] [(externalAddr 8, 30)]
          coin3 = storeCoin ctx (txHash fundTx3, 0) (Just 0) (externalAddr 8) 30
      lift $ do
        dbShouldFailWith "A coin referenced by the transaction is locked" $
          importPendingTx btc ctx accId $
            TxSignData
              { txSignDataTx =
                  spendingTx
                    ctx
                    [(txHash fundTx1, 0)]
                    [(otherAddr 3, 20)],
                txSignDataInputs = [fundTx3, fundTx1],
                txSignDataInputPaths = [Deriv :/ 0 :/ 0],
                txSignDataOutputPaths = [],
                txSignDataFeeRate = Nothing,
                txSignDataSigned = False
              }
        dbShouldFailWith "A coin referenced by the transaction does not exist" $
          importPendingTx btc ctx accId $
            TxSignData
              { txSignDataTx =
                  spendingTx
                    ctx
                    [(txHash fundTx3, 0)]
                    [(otherAddr 3, 20)],
                txSignDataInputs = [fundTx3, fundTx1],
                txSignDataInputPaths = [Deriv :/ 0 :/ 8],
                txSignDataOutputPaths = [],
                txSignDataFeeRate = Nothing,
                txSignDataSigned = False
              }
      (_, dbCoins3) <- refreshCoins btc accId (externalAddrs <> internalAddrs) [coin1, coin3]
      let jsonCoin3 = forceRight $ toJsonCoin btc Nothing Set.empty (head dbCoins3)
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin3 {jsonCoinLocked = False},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      lift $
        dbShouldFailWith "Some referenced addresses do not exist" $
          importPendingTx btc ctx accId $
            TxSignData
              { txSignDataTx =
                  spendingTx
                    ctx
                    [(txHash fundTx3, 0)]
                    [(internalAddr 1, 5), (otherAddr 3, 25)],
                txSignDataInputs = [fundTx3],
                txSignDataInputPaths = [Deriv :/ 0 :/ 8],
                txSignDataOutputPaths = [Deriv :/ 1 :/ 1],
                txSignDataFeeRate = Nothing,
                txSignDataSigned = False
              }
      -- The transaction is not being rolled back: the addresses stay
      replicateM_ 10 $ genExtAddress ctx cfg accId ""
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin3 {jsonCoinLocked = False},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      lift $
        dbShouldFailWith "Some of the internal output addresses are not free" $
          importPendingTx btc ctx accId $
            TxSignData
              { txSignDataTx =
                  spendingTx
                    ctx
                    [(txHash fundTx3, 0)]
                    [(internalAddr 2, 1), (otherAddr 3, 25)],
                txSignDataInputs = [fundTx3],
                txSignDataInputPaths = [Deriv :/ 0 :/ 8],
                txSignDataOutputPaths = [Deriv :/ 1 :/ 2],
                txSignDataFeeRate = Nothing,
                txSignDataSigned = False
              }
