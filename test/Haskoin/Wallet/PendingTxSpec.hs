{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

-- | The pending transactions of an account: imported, held, listed, signed
-- in place, sent and deleted. The account of these tests holds a coin of
-- 20000 at its first external address, and its payment is 8000 to an
-- outside address, with the change at its first internal address. Some
-- tests give it a second coin, of 10000 at its second external address.
module Haskoin.Wallet.PendingTxSpec (spec) where

import Conduit (liftIO)
import Control.Monad.Except
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Either (isLeft)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Address
import Haskoin.Wallet.Coin
import Haskoin.Wallet.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.PendingTx
import Haskoin.Wallet.RawTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxBuilding (buildTxSignData)
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (signWalletTx, verifySignedTx)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    importingSpec ctx
    listingSpec ctx
    sendingSpec ctx
    sendingFileSpec ctx
    deletingSpec ctx
    conflictsSpec ctx

-- | A store that knows the transaction of the signing data, in its mempool
storeKnowing :: Ctx -> TxSignData -> FakeStore
storeKnowing ctx tsd =
  emptyStore {fakeTransactions = [creditTx ctx (txHash (txSignDataTx tsd)) (Store.MemRef 1700000000) (otherAddr 0) 1]}

-- | A store that saw the transaction of the signing data and then dropped
-- it, replaced in its mempool by another transaction: it still lists it,
-- as deleted
storeThatDropped :: Ctx -> TxSignData -> FakeStore
storeThatDropped ctx tsd = known {fakeTransactions = replaced <$> fakeTransactions known}
  where
    known = storeKnowing ctx tsd

-- | The transaction with the signature of its first input damaged: signed
-- to the eye, but no longer valid
withBrokenSignature :: Tx -> Tx
withBrokenSignature tx =
  case tx.inputs of
    first : rest ->
      Tx tx.version (TxIn first.outpoint (BS.reverse first.script) first.sequence : rest) tx.outputs tx.witness tx.locktime
    [] -> tx

-- | The store, failing its health check
unhealthy :: Store -> Store
unhealthy store = store {storeHealth = throwError "The store is down"}

-- | The store, failing every question about transactions
notAnswering :: Store -> Store
notAnswering store = store {storeTransactions = \_ -> throwError "The store is down"}

-- | The store, counting the times it is asked for transactions
countingAsks :: IORef Int -> Store -> Store
countingAsks asks store =
  store
    { storeTransactions = \tids -> do
        liftIO $ modifyIORef asks (+ 1)
        storeTransactions store tids
    }

-- | The transaction paying 20000 to externalAddr 0: the one coin of the
-- account in these tests
funding :: Ctx -> Tx
funding ctx = spendingTx ctx [(fakeTxHash 1, 0)] [(externalAddr 0, 20000)]

-- | The transaction paying 10000 to externalAddr 1: the second coin of the
-- account, in the tests that need two
secondFunding :: Ctx -> Tx
secondFunding ctx = spendingTx ctx [(fakeTxHash 2, 0)] [(externalAddr 1, 10000)]

-- | The account with its first external address, its first change address
-- and the coin of 'funding'
fundAccount :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) ()
fundAccount ctx cfg accId = do
  _ <- genExtAddress ctx cfg accId ""
  _ <- changeAddress ctx cfg accId
  lift $ insertRawTx (funding ctx)
  _ <- refreshCoins btc accId externalAddrs [storeCoin ctx (txHash (funding ctx), 0) (Just 0) (externalAddr 0) 20000]
  return ()

-- | The account funded as by 'fundAccount', with its second external
-- address and the coin of 'secondFunding' as well
fundAccountTwice :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) ()
fundAccountTwice ctx cfg accId = do
  fundAccount ctx cfg accId
  _ <- genExtAddress ctx cfg accId ""
  lift $ insertRawTx (secondFunding ctx)
  _ <-
    refreshCoins
      btc
      accId
      externalAddrs
      [ storeCoin ctx (txHash (funding ctx), 0) (Just 0) (externalAddr 0) 20000,
        storeCoin ctx (txHash (secondFunding ctx), 0) (Just 0) (externalAddr 1) 10000
      ]
  return ()

-- | An unsigned payment of the amount to the address, at 1 satoshi per
-- byte, from the given coins of the account, or from any of its coins
payment :: Ctx -> Config -> DBAccountId -> [OutPoint] -> Address -> Natural -> ExceptT String (DB IO) TxSignData
payment ctx cfg accId chosen to amount =
  buildTxSignData btc ctx cfg (mkStdGen 0) LeastWaste accId [(to, amount)] 1 0 SenderPays 0 chosen

-- | An unsigned payment of 8000 to otherAddr 0 from the coin of the funded
-- account, with 11774 of change at internalAddr 0
unsignedPayment :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) TxSignData
unsignedPayment ctx cfg accId = do
  fundAccount ctx cfg accId
  payment ctx cfg accId [] (otherAddr 0) 8000

-- | The payment, signed and stored as a pending transaction of the account
signedPending :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) (TxHash, TxSignData)
signedPending ctx cfg accId = do
  unsigned <- unsignedPayment ctx cfg accId
  (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
  pendingHash <- importPendingTx btc ctx accId signed
  return (pendingHash, signed)

-- | The account with two coins and two pending payments: 8000 to otherAddr 0
-- from the coin of 20000, with 11774 of change at internalAddr 0, then 7000
-- to otherAddr 1 from the coin of 10000, with 2774 of change at
-- internalAddr 1. Their hashes, in that order.
twoPending :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) (TxHash, TxHash)
twoPending ctx cfg accId = do
  fundAccountTwice ctx cfg accId
  first <- payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
  firstHash <- importPendingTx btc ctx accId first
  second <- payment ctx cfg accId [] (otherAddr 1) 7000
  secondHash <- importPendingTx btc ctx accId second
  return (firstHash, secondHash)

-- | The other account of these tests, account 0 of the second test wallet,
-- with its first external address
otherAccount :: Ctx -> Config -> ExceptT String (DB IO) DBAccountId
otherAccount ctx cfg = do
  (otherId, _) <- testNewAcc2 ctx "other"
  _ <- genExtAddress ctx cfg otherId ""
  return otherId

-- | A sweep by the other account of the coin of 'funding': the 20000 at the
-- account's first external address go to the first external address of the
-- other account, less a fee of 1000
sweepOfFunding :: Ctx -> TxSignData
sweepOfFunding ctx =
  TxSignData (spendingTx ctx [(txHash (funding ctx), 0)] [(externalAddr2, 19000)]) [funding ctx] [] [Deriv :/ 0 :/ 0] Nothing Nothing

-- | The coins of the account, by address, and whether a pending transaction
-- holds each
lockedCoins :: DBAccountId -> ExceptT String (DB IO) [(Address, Bool)]
lockedCoins accId = map (\c -> (jsonCoinAddress c, jsonCoinLocked c)) <$> coinPage btc accId (Page 5 0)

-- | The payment signed with SIGHASH_ALL and ANYONECANPAY: a valid signature
-- that differs from the one hw makes, as another signer could produce. The
-- signatures of hw itself are deterministic: signing twice gives the same
-- bytes.
signedByAnotherSigner :: Ctx -> TxSignData -> Either String TxSignData
signedByAnotherSigner ctx unsigned = do
  script <- maybeToEither "The address has no script" $ addressToOutput (externalAddr 0)
  let coin = SigInput script 20000 (OutPoint (txHash (funding ctx)) 0) (setAnyoneCanPay sigHashAll) Nothing
      key = (derivePath ctx (Deriv :/ 0 :/ 0 :: SoftPath) testAccountKey).key
  tx <- signTx btc ctx (txSignDataTx unsigned) [coin] [key]
  return unsigned {txSignDataTx = tx}

importingSpec :: Ctx -> Spec
importingSpec ctx =
  describe "Importing a pending transaction" $ do
    let cfg = def :: Config
    it "locks the coins it spends and leaves the others free" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        fundAccountTwice ctx cfg accId
        first <- payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
        _ <- importPendingTx btc ctx accId first
        held <- lockedCoins accId
        liftIO $ held `shouldMatchList` [(externalAddr 0, True), (externalAddr 1, False)]
        -- Only the free coin of 10000 is left to build from
        lift $
          dbShouldFailWith "The coins do not cover the amount and the fee" $
            payment ctx cfg accId [] (otherAddr 1) 12000
        second <- payment ctx cfg accId [] (otherAddr 1) 7000
        liftIO $
          sortedOutputs (txSignDataTx second)
            `shouldBe` spendingTx ctx [(txHash (secondFunding ctx), 0)] [(internalAddr 1, 2774), (otherAddr 1, 7000)]
        _ <- importPendingTx btc ctx accId second
        bothHeld <- lockedCoins accId
        liftIO $ bothHeld `shouldMatchList` [(externalAddr 0, True), (externalAddr 1, True)]
        lift $
          dbShouldFailWith "The coins do not cover the amount and the fee" $
            payment ctx cfg accId [] (otherAddr 2) 1000
    it "takes no change address for a transaction without change" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        fundAccount ctx cfg accId
        -- 19808 to otherAddr 2 leaves exactly the fee of 192 for one input
        -- and one output at 1 satoshi per byte
        tsd <- payment ctx cfg accId [] (otherAddr 2) 19808
        liftIO $ do
          txSignDataTx tsd `shouldBe` spendingTx ctx [(txHash (funding ctx), 0)] [(otherAddr 2, 19808)]
          txSignDataOutputPaths tsd `shouldBe` []
        -- One output, so nothing is shuffled and the hash is fixed
        importPendingTx btc ctx accId tsd `dbShouldBeE` "720a9cfea170cc699458a97fe34da547dde30d9d54e5210cd88f1b76b139696c"
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [True]
        (dBAddressIndex <$> changeAddress ctx cfg accId) `dbShouldBeE` 0
    it "holds a payment to an external address of the wallet as an internal transaction" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        fundAccount ctx cfg accId
        _ <- genExtAddress ctx cfg accId ""
        tsd <- payment ctx cfg accId [] (externalAddr 1) 8000
        -- 8000 to the wallet's own /0/1 and the change to /1/0: 226 bytes at
        -- 1 satoshi per byte leave 11774
        liftIO $ do
          sortedOutputs (txSignDataTx tsd)
            `shouldBe` spendingTx ctx [(txHash (funding ctx), 0)] [(externalAddr 1, 8000), (internalAddr 0, 11774)]
          txSignDataInputPaths tsd `shouldBe` [Deriv :/ 0 :/ 0]
          txSignDataOutputPaths tsd `shouldMatchList` [Deriv :/ 0 :/ 1, Deriv :/ 1 :/ 0]
        pendingHash <- importPendingTx btc ctx accId tsd
        -- Only the fee leaves the wallet
        summaries <- pendingTxPage ctx accId (Page 5 0)
        liftIO $ do
          map pendingFlags summaries `shouldBe` [Just ((pendingHash, False, False, False))]
          map (summaryType . txInfoSummary) summaries `shouldBe` [TxInternal]
          map (summaryAmount . txInfoSummary) summaries `shouldBe` [-226]
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [True]
        -- The change address is taken
        (dBAddressIndex <$> changeAddress ctx cfg accId) `dbShouldBeE` 1
    it "replaces the unsigned version with the signed one, and refuses the rest" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importPendingTx btc ctx accId unsigned
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]
        lift $
          dbShouldFailWith "The transaction already exists" $
            importPendingTx btc ctx accId unsigned
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        importPendingTx btc ctx accId signed `dbShouldBeE` pendingHash
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
        lift $ do
          dbShouldFailWith "The transaction already exists" $
            importPendingTx btc ctx accId signed
          dbShouldFailWith "Can not replace a signed transaction with an unsigned one" $
            importPendingTx btc ctx accId unsigned
    it "replaces a signed version with one signed by another signer" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importPendingTx btc ctx accId unsigned
        (byWallet, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        byOther <- liftEither $ signedByAnotherSigner ctx unsigned
        -- Both versions are valid, with different signatures, so different ids
        liftIO $ do
          verifySignedTx btc ctx byOther `shouldBe` Right ()
          txHash (txSignDataTx byOther) `shouldNotBe` txHash (txSignDataTx byWallet)
        importPendingTx btc ctx accId byWallet `dbShouldBeE` pendingHash
        importPendingTx btc ctx accId byOther `dbShouldBeE` pendingHash
        -- The wallet holds the version imported last: sendtx sends that one
        stored <- lookupPendingTx pendingHash
        liftIO $ (pendingTxData <$> stored) `shouldBe` Just byOther
    it "refuses a transaction pending in another account" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (otherId, _) <- testNewAcc2 ctx "other"
        unsigned <- unsignedPayment ctx cfg accId
        _ <- importPendingTx btc ctx accId unsigned
        lift $
          dbShouldFailWith "The transaction is pending in another account" $
            importPendingTx btc ctx otherId unsigned
    it "refuses a signed transaction whose signatures do not check out" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        let broken = signed {txSignDataTx = withBrokenSignature (txSignDataTx signed)}
        lift $
          dbShouldFailWith "Invalid signature for input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj)" $
            importPendingTx btc ctx accId broken
        pendingTxPage ctx accId (Page 5 0) `dbShouldBeE` []
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [False]
    it "keeps the unsigned version when the signed one does not check out" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importPendingTx btc ctx accId unsigned
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        let broken = signed {txSignDataTx = withBrokenSignature (txSignDataTx signed)}
        lift $
          dbShouldFailWith "Invalid signature for input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj)" $
            importPendingTx btc ctx accId broken
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]
    it "refuses a new version of a transaction that is online" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, signed) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $
          dbShouldFailWith "The transaction is already online" $
            importPendingTx btc ctx accId signed
    it "refuses a transaction spending a coin another pending transaction holds" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        _ <- signedPending ctx cfg accId
        let spend = spendingTx ctx [(txHash (funding ctx), 0)] [(otherAddr 3, 19000)]
        lift $
          dbShouldFailWith "A coin referenced by the transaction is locked" $
            importPendingTx btc ctx accId (TxSignData spend [funding ctx] [Deriv :/ 0 :/ 0] [] Nothing Nothing)
    it "locks the coin of another account it spends, in that account too" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        fundAccount ctx cfg accId
        otherId <- otherAccount ctx cfg
        _ <- importPendingTx btc ctx otherId (sweepOfFunding ctx)
        lockedCoins accId `dbShouldBeE` [(externalAddr 0, True)]
        lift $
          dbShouldFailWith "The coins do not cover the amount and the fee" $
            payment ctx cfg accId [] (otherAddr 0) 8000
    it "refuses a transaction spending a coin a pending transaction of another account holds" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        _ <- signedPending ctx cfg accId
        otherId <- otherAccount ctx cfg
        lift $
          dbShouldFailWith "A coin referenced by the transaction is locked" $
            importPendingTx btc ctx otherId (sweepOfFunding ctx)
    it "refuses a transaction spending a coin the wallet does not have" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        fundAccount ctx cfg accId
        -- A payment to the wallet's first address that the wallet never saw
        let unseen = spendingTx ctx [(fakeTxHash 3, 0)] [(externalAddr 0, 5000)]
            spend = spendingTx ctx [(txHash unseen, 0)] [(otherAddr 3, 4000)]
        lift $
          dbShouldFailWith "A coin referenced by the transaction does not exist" $
            importPendingTx btc ctx accId (TxSignData spend [unseen] [Deriv :/ 0 :/ 0] [] Nothing Nothing)
    it "refuses a transaction paying change to an address the wallet has not generated" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        fundAccount ctx cfg accId
        -- Change to /1/1, when only /1/0 exists
        let spend = spendingTx ctx [(txHash (funding ctx), 0)] [(internalAddr 1, 11774), (otherAddr 0, 8000)]
        lift $
          dbShouldFailWith "Some referenced addresses do not exist" $
            importPendingTx btc ctx accId (TxSignData spend [funding ctx] [Deriv :/ 0 :/ 0] [Deriv :/ 1 :/ 1] Nothing Nothing)
    it "refuses a transaction whose change address another pending transaction holds" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        fundAccountTwice ctx cfg accId
        first <- payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
        _ <- importPendingTx btc ctx accId first
        -- The change of the first payment went to /1/0
        let spend = spendingTx ctx [(txHash (secondFunding ctx), 0)] [(internalAddr 0, 2774), (otherAddr 1, 7000)]
        lift $
          dbShouldFailWith "Some of the internal output addresses are not free" $
            importPendingTx btc ctx accId (TxSignData spend [secondFunding ctx] [Deriv :/ 0 :/ 1] [Deriv :/ 1 :/ 0] Nothing Nothing)

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "Listing the pending transactions" $ do
    let cfg = def :: Config
    it "lists them newest first, a page at a time" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (first, second) <- twoPending ctx cfg accId
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((second, False, False, False)), Just ((first, False, False, False))]
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 1 1))
          `dbShouldBeE` [Just ((first, False, False, False))]
    it "lists nothing for an account without any" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        pendingTxPage ctx accId (Page 5 0) `dbShouldBeE` []
    it "reports a row whose signing data does not decode, instead of hiding it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        lift $ P.insert_ $ DBPendingTx accId (hashBytes (fakeTxHash 7)) "not signing data" False False fixtureTime
        listed <- lift $ runExceptT $ pendingTxPage ctx accId (Page 5 0)
        deleted <- lift $ runExceptT $ deleteTx ctx (const (toStore emptyStore)) (fakeTxHash 7)
        liftIO $ do
          listed `shouldSatisfy` isLeft
          deleted `shouldSatisfy` isLeft
          deleted `shouldNotBe` Left noPendingTx

sendingSpec :: Ctx -> Spec
sendingSpec ctx =
  describe "Sending a pending transaction" $ do
    let cfg = def :: Config
    it "marks it online once the store sees it, and stores its raw transaction" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, signed) <- signedPending ctx cfg accId
        (_, sent) <- sendPending ctx (const (toStore (storeKnowing ctx signed))) pendingHash
        liftIO $ pendingFlags sent `shouldBe` Just ((pendingHash, True, True, False))
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` True
    it "sends again a transaction that is already online" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, signed) <- signedPending ctx cfg accId
        -- As if sent once, then lost by the store
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $ insertRawTx (txSignDataTx signed)
        published <- liftIO $ newIORef []
        (_, sent) <- sendPending ctx (const (notingPublished published (toStore (storeKnowing ctx signed)))) pendingHash
        liftIO $ readIORef published `shouldReturn` [txSignDataTx signed]
        liftIO $ pendingFlags sent `shouldBe` Just ((pendingHash, True, True, False))
        -- The raw transaction is stored once, next to the parent of its coin
        lift (P.count ([] :: [P.Filter DBRawTx])) `dbShouldBeE` 2
    it "records nothing when the store does not see it, and says so" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, _) <- signedPending ctx cfg accId
        lift $
          dbShouldFailWith "The transaction was handed to the network but the store has not seen it after 10 seconds. It stays signed and not online: run sendtx again later" $
            sendPending ctx (const (toStore emptyStore)) pendingHash
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
    it "sends nothing when the store is not healthy" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, _) <- signedPending ctx cfg accId
        published <- liftIO $ newIORef []
        lift $
          dbShouldFailWith "The store is down" $
            sendPending ctx (const (notingPublished published (unhealthy (toStore emptyStore)))) pendingHash
        liftIO $ readIORef published `shouldReturn` []
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
    it "refuses a transaction that is not signed" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importPendingTx btc ctx accId unsigned
        lift $
          dbShouldFailWith "The transaction is not signed" $
            sendPending ctx (const (toStore emptyStore)) pendingHash
    it "refuses a transaction the wallet does not hold" $
      inDatabaseE $
        lift $
          dbShouldFailWith "The pending transaction does not exist" $
            sendPending ctx (const (toStore emptyStore)) (fakeTxHash 7)
    it "gives up after the asks it is allowed" $ do
      asks <- newIORef 0
      let tx = spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 8000)]
      runExceptT (broadcastWith 1000 3 (countingAsks asks (toStore emptyStore)) tx) `shouldReturn` Right False
      readIORef asks `shouldReturn` 3
    it "sees the transaction on the first ask" $ do
      asks <- newIORef 0
      let tsd = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 8000)]) [] [] [] Nothing Nothing
      runExceptT (broadcastWith 1000 3 (countingAsks asks (toStore (storeKnowing ctx tsd))) (txSignDataTx tsd))
        `shouldReturn` Right True
      readIORef asks `shouldReturn` 1

sendingFileSpec :: Ctx -> Spec
sendingFileSpec ctx =
  describe "Sending a signed transaction from a file" $ do
    let cfg = def :: Config
    it "sends the signed version of a transaction the wallet holds, and marks it online" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importPendingTx btc ctx accId unsigned
        -- The file the offline signer returns
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        sent <- sendSigned ctx (const (toStore (storeKnowing ctx signed))) Nothing signed
        liftIO $ case sent of
          SentPending _ info -> pendingFlags info `shouldBe` Just ((pendingHash, True, True, False))
          SentOutside _ _ -> expectationFailure "A transaction of the wallet was sent as an outside one"
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        held <- requirePendingTx pendingHash
        liftIO $ held.pendingTxData `shouldBe` signed
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` True
    it "refuses a network other than the account's" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        _ <- importPendingTx btc ctx accId unsigned
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        lift $
          dbShouldFailWith "The transaction is pending in an account of another network" $
            sendSigned ctx (const (toStore emptyStore)) (Just bch) signed
    it "hands a transaction the wallet does not hold to the store of the given network, and records nothing" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        -- The payment is built but never held, as a file prepared elsewhere
        -- would be: the next sync catches up with it
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        published <- liftIO $ newIORef []
        sent <- sendSigned ctx (const (notingPublished published (toStore (storeKnowing ctx signed)))) (Just btc) signed
        liftIO $ case sent of
          SentOutside net txid -> (net.name, txid) `shouldBe` ("btc", txHash (txSignDataTx signed))
          SentPending _ _ -> expectationFailure "A transaction the wallet does not hold was sent as pending"
        liftIO $ readIORef published `shouldReturn` [txSignDataTx signed]
        pendingTxPage ctx accId (Page 5 0) `dbShouldBeE` []
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` False
    it "needs the network of a transaction the wallet does not hold" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        lift $
          dbShouldFailWith "Give the --network of a transaction the wallet does not hold" $
            sendSigned ctx (const (toStore emptyStore)) Nothing signed
    it "refuses a transaction the wallet does not hold whose signatures do not check out" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned testAccountKey
        let broken = signed {txSignDataTx = withBrokenSignature (txSignDataTx signed)}
        lift $
          dbShouldFailWith "Invalid signature for input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj)" $
            sendSigned ctx (const (toStore emptyStore)) (Just btc) broken
    it "refuses an unsigned file" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        unsigned <- unsignedPayment ctx cfg accId
        lift $
          dbShouldFailWith "The transaction is not signed" $
            sendSigned ctx (const (toStore emptyStore)) (Just btc) unsigned

deletingSpec :: Ctx -> Spec
deletingSpec ctx =
  describe "Deleting a pending transaction" $ do
    let cfg = def :: Config
    it "abandons a sent transaction the store does not know, freeing its coin" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, signed) <- signedPending ctx cfg accId
        -- As if sent: online, with its raw transaction stored next to the
        -- parent of its coin
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $ insertRawTx (txSignDataTx signed)
        lift (P.count ([] :: [P.Filter DBRawTx])) `dbShouldBeE` 2
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [True]
        deleteTx ctx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (1, 1)
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [False]
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0)) `dbShouldBeE` []
        -- Its raw transaction went with it; the parent of the coin stays
        lift (P.count ([] :: [P.Filter DBRawTx])) `dbShouldBeE` 1
    it "abandons a sent transaction the network dropped" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, signed) <- signedPending ctx cfg accId
        -- As if sent, then replaced in the mempool by another transaction
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $ insertRawTx (txSignDataTx signed)
        deleteTx ctx (const (toStore (storeThatDropped ctx signed))) pendingHash `dbShouldBeE` (1, 1)
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [False]
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0)) `dbShouldBeE` []
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` False
    it "keeps a sent transaction when the store cannot be asked" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, signed) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $ insertRawTx (txSignDataTx signed)
        lift $
          dbShouldFailWith "The store is down" $
            deleteTx ctx (const (notAnswering (toStore emptyStore))) pendingHash
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [True]
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` True
    it "deletes a transaction that was never sent without asking the store" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, _) <- signedPending ctx cfg accId
        deleteTx ctx (const (notAnswering (unhealthy (toStore emptyStore)))) pendingHash `dbShouldBeE` (1, 1)
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0)) `dbShouldBeE` []
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [False]
    it "keeps a sent transaction the store still lists" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, signed) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $
          dbShouldFailWith "The transaction is in the network's mempool or in the chain: sync retires it once it is listed" $
            deleteTx ctx (const (toStore (storeKnowing ctx signed))) pendingHash
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [True]
    it "frees the change address for the next transaction" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, _) <- signedPending ctx cfg accId
        -- The change went to /1/0
        (dBAddressIndex <$> changeAddress ctx cfg accId) `dbShouldBeE` 1
        deleteTx ctx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (1, 1)
        (dBAddressIndex <$> changeAddress ctx cfg accId) `dbShouldBeE` 0
    it "retiring a sent transaction, as the sync does, frees its coin but keeps its change address" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        (pendingHash, _) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        held <- requirePendingTx pendingHash
        lift $ retirePendingTx held
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0)) `dbShouldBeE` []
        (map jsonCoinLocked <$> coinPage btc accId (Page 5 0)) `dbShouldBeE` [False]
        -- The sync that retires it also records the change paid to /1/0,
        -- and an address paid on the chain is never used again
        updateAddressBalances btc [Store.Balance (internalAddr 0) 11774 0 1 1 11774]
        (dBAddressIndex <$> changeAddress ctx cfg accId) `dbShouldBeE` 1
    it "counts and frees the coin of another account it held" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "test"
        fundAccount ctx cfg accId
        otherId <- otherAccount ctx cfg
        sweepHash <- importPendingTx btc ctx otherId (sweepOfFunding ctx)
        deleteTx ctx (const (toStore emptyStore)) sweepHash `dbShouldBeE` (1, 0)
        lockedCoins accId `dbShouldBeE` [(externalAddr 0, False)]
    it "refuses a transaction the wallet does not hold" $
      inDatabaseE $
        lift $
          dbShouldFailWith "The pending transaction does not exist" $
            deleteTx ctx (const (toStore emptyStore)) (fakeTxHash 7)

conflictsSpec :: Ctx -> Spec
conflictsSpec ctx =
  describe "The conflicts of a transaction, from what the store reports of the coins it spends" $ do
    -- The coin of 'funding', as the store reports its transaction, and a
    -- payment from it
    let parent = creditTx ctx (txHash (funding ctx)) (Store.BlockRef 800000 1) (externalAddr 0) 20000
        coin = OutPoint (txHash (funding ctx)) 0
        spend = spendingTx ctx [(txHash (funding ctx), 0)] [(otherAddr 0, 19000)]
    it "are none while the coins it spends are unspent" $
      conflictedInputs [parent] spend `shouldBe` []
    it "are the coins another transaction spends" $
      conflictedInputs [withSpender 0 (fakeTxHash 8, 0) parent] spend `shouldBe` [coin]
    it "leave out a coin the transaction itself spends, once the network has it" $
      conflictedInputs [withSpender 0 (txHash spend, 0) parent] spend `shouldBe` []
    it "are the coins of a transaction the store dropped" $
      conflictedInputs [replaced parent] spend `shouldBe` [coin]
    it "leave out the coins of a transaction the store has not seen, which proves nothing yet" $
      conflictedInputs [] spend `shouldBe` []
    it "are the coins past the outputs of their transaction" $
      conflictedInputs [parent] (spendingTx ctx [(txHash (funding ctx), 1)] [(otherAddr 0, 19000)])
        `shouldBe` [OutPoint (txHash (funding ctx)) 1]
