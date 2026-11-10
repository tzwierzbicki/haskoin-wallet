{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

-- | The pending transactions of a portfolio on a network, a pool:
-- imported, held, labelled, listed, signed in place, sent and deleted,
-- and the pool a transaction of a file is of. The pool of these tests is
-- what portfolio 0 of the test wallet holds on Bitcoin. Its bip44 account
-- holds a coin of 20000 at its first external address, and its payment is
-- 8000 to an outside address, with the change at its first internal
-- address. Some tests give it a second coin, of 10000 at its second
-- external address.
module Haskoin.Wallet.Database.PendingTxSpec (spec) where

import Conduit (MonadIO, liftIO)
import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Either (isLeft)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account (accountPool, getAccountById, insertAccount)
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Tx.CoinSelection (CoinSelection (..), FeePayer (..))
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.RawTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.Keys (AccountKind (..), legacyPath)
import Haskoin.Wallet.ShortId (shortIdOf)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.Building (ChangeTo (..), CoinControl (..), buildTxSignData)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Database.TxLabel (txLabel)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.PsbtFile (FileAccount (..), psbtTxFile)
import Haskoin.Wallet.Tx.Signing (signWalletTx, verifySignedTx)
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    importingSpec ctx
    namingSpec ctx
    labellingSpec ctx
    listingSpec ctx
    sendingSpec ctx
    sendingFileSpec ctx
    deletingSpec ctx
    conflictsSpec ctx
    fingerprintSpec ctx
    poolOfTxSpec ctx
    payerSpec ctx

-- | Nothing before sending: the transaction goes on, as with broadcast --yes
withoutAsking :: (Monad m) => Network -> Maybe Place -> TxInfo -> ExceptT String m ()
withoutAsking _ _ _ = return ()

-- | A step before sending that refuses, as the user who answers no
refusing :: (Monad m) => Network -> Maybe Place -> TxInfo -> ExceptT String m ()
refusing _ _ _ = throwError "The transaction was not sent"

-- | A step before sending that notes the network and the flags of the
-- transaction it is given, then lets it go
noting :: (MonadIO m) => IORef [(String, Maybe (TxHash, Bool, Bool, Bool))] -> Network -> Maybe Place -> TxInfo -> ExceptT String m ()
noting seen net _ info = liftIO $ modifyIORef seen (<> [(net.name, pendingFlags info)])

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

-- | The pool an account is a part of: what its portfolio holds on its
-- network
poolOf :: DBAccountId -> ExceptT String (DB IO) Pool
poolOf accId = accountPool <$> getAccountById accId

-- | Store a transaction as pending in the pool of an account
importTo :: Ctx -> DBAccountId -> TxSignData -> ExceptT String (DB IO) TxHash
importTo ctx accId tsd = do
  pool <- poolOf accId
  importPendingTx ctx pool tsd

-- | The pending transactions of the pool of an account as summaries,
-- newest first: those after the one given, up to the count
pendingOf :: Ctx -> DBAccountId -> Maybe (UTCTime, TxHash) -> Maybe Natural -> ExceptT String (DB IO) [TxInfo]
pendingOf ctx accId afterM countM = do
  pool <- poolOf accId
  pendingTxsAfter ctx pool afterM countM

-- | The coins of an account, as coins lists them
coinsOfAccount :: DBAccountId -> ExceptT String (DB IO) [JsonCoin]
coinsOfAccount accId = do
  pool <- poolOf accId
  coinsAfter pool [accId] AllCoins Nothing Nothing

-- | The change address a new transaction of the pool of an account takes
-- in its bip44 account
changeOf :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) DBAddress
changeOf ctx cfg accId = do
  pool <- poolOf accId
  changeAddress ctx cfg pool Bip44

-- | Where the listing of the pools of the accounts goes on after a
-- pending transaction
cursorAmong :: [DBAccountId] -> TxHash -> ExceptT String (DB IO) (UTCTime, TxHash)
cursorAmong accIds hash = do
  pools <- mapM poolOf accIds
  pendingCursor (Just pools) hash

-- | The label of a transaction of the history of the pool of an account
labelOf :: DBAccountId -> TxHash -> ExceptT String (DB IO) Text
labelOf accId tid = do
  pool <- poolOf accId
  lift $ txLabel pool tid

-- | The row of a pending transaction of the pool of an account, holding
-- the signing data given, created at 'fixtureTime'
heldRow :: DBAccountId -> TxSignData -> ExceptT String (DB IO) DBPendingTx
heldRow accId tsd = do
  pool <- poolOf accId
  return (pendingRow pool tsd.txSignDataTx) {dBPendingTxBlob = blob tsd}

-- | The account with its first external address, its first change address
-- and the coin of 'funding'
fundAccount :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) ()
fundAccount ctx cfg accId = do
  _ <- genExtAddress ctx cfg accId ""
  _ <- changeOf ctx cfg accId
  lift $ insertRawTx (funding ctx)
  _ <- refreshCoins btc ctx accId externalAddrs [storeCoin ctx (txHash (funding ctx), 0) (Just 0) (externalAddr 0) 20000]
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
      ctx
      accId
      externalAddrs
      [ storeCoin ctx (txHash (funding ctx), 0) (Just 0) (externalAddr 0) 20000,
        storeCoin ctx (txHash (secondFunding ctx), 0) (Just 0) (externalAddr 1) 10000
      ]
  return ()

-- | An unsigned payment of the amount to the address, at 1 satoshi per
-- byte, from the given coins of the pool of the account, or from any of
-- its coins
payment :: Ctx -> Config -> DBAccountId -> [OutPoint] -> Address -> Natural -> ExceptT String (DB IO) TxSignData
payment ctx cfg accId chosen to amount = do
  pool <- poolOf accId
  buildTxSignData ctx cfg (mkStdGen 0) LeastWaste pool Nothing [(to, amount)] 1 800002 SenderPays 0 control (NewChangeAddress Nothing)
  where
    control = if null chosen then NoCoinControl else SpendChosen chosen

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
  (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
  pendingHash <- importTo ctx accId signed
  return (pendingHash, signed)

-- | The account with two coins and two pending payments: 8000 to otherAddr 0
-- from the coin of 20000, with 11774 of change at internalAddr 0, then 7000
-- to otherAddr 1 from the coin of 10000, with 2774 of change at
-- internalAddr 1. Their hashes, in that order.
twoPending :: Ctx -> Config -> DBAccountId -> ExceptT String (DB IO) (TxHash, TxHash)
twoPending ctx cfg accId = do
  fundAccountTwice ctx cfg accId
  first <- payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
  firstHash <- importTo ctx accId first
  second <- payment ctx cfg accId [] (otherAddr 1) 7000
  secondHash <- importTo ctx accId second
  return (firstHash, secondHash)

-- | The other account of these tests, in another portfolio: the bip44
-- account of portfolio 0 of the second test wallet on Bitcoin, with its
-- first external address
otherAccount :: Ctx -> Config -> ExceptT String (DB IO) DBAccountId
otherAccount ctx cfg = do
  (otherId, _) <- testNewAcc2 ctx
  _ <- genExtAddress ctx cfg otherId ""
  return otherId

-- | A sweep by the other account of the coin of 'funding': the 20000 at the
-- account's first external address go to the first external address of the
-- other account, less a fee of 1000
sweepOfFunding :: Ctx -> TxSignData
sweepOfFunding ctx =
  TxSignData (spendingTx ctx [(txHash (funding ctx), 0)] [(externalAddr2, 19000)]) [funding ctx] [] [legacyPath (Deriv :/ 0 :/ 0)] Nothing Nothing [] noLabel

-- | A sweep by the other account of the coin of 'secondFunding': the 10000
-- at the account's second external address go to the first external
-- address of the other account, less a fee of 1000
sweepOfSecondFunding :: Ctx -> TxSignData
sweepOfSecondFunding ctx =
  TxSignData (spendingTx ctx [(txHash (secondFunding ctx), 0)] [(externalAddr2, 9000)]) [secondFunding ctx] [] [legacyPath (Deriv :/ 0 :/ 0)] Nothing Nothing [] noLabel

-- | When a transaction in preparation was created, if the wallet holds it
createdOf :: TxInfo -> Maybe UTCTime
createdOf info = case txInfoStatus info of
  Pending status -> pendingCreated status
  OnChain _ -> Nothing

-- | A sweep on Bitcoin Cash of the outpoint of 'funding', to the first
-- receiving address of a Bitcoin Cash account of the test wallet: as if
-- 'funding' came from before the fork, when a transaction had the same id
-- on both chains
cashSweepOfFunding :: Ctx -> TxSignData
cashSweepOfFunding ctx =
  TxSignData (spendingTx ctx [(txHash (funding ctx), 0)] [(bchExternalAddr, 19000)]) [funding ctx] [] [legacyPath (Deriv :/ 0 :/ 0)] Nothing Nothing [] noLabel

-- | Whether a pending transaction holds the coin
transactionLocked :: JsonCoin -> Bool
transactionLocked c = jsonCoinLock c == Just LockedByTransaction

-- | The coins of the account, by address, and whether a pending transaction
-- holds each
lockedCoins :: DBAccountId -> ExceptT String (DB IO) [(Address, Bool)]
lockedCoins accId = map (\c -> (jsonCoinAddress c, transactionLocked c)) <$> coinsOfAccount accId

-- | The payment of 'unsignedPayment' as Bitcoin Core signed and
-- finalized the file hw writes for it, in
-- test/data/psbt/core/pending-payment-finalized.psbt: another signer's
-- signature, which differs from the one hw makes, and no derivation record
-- on the input, which Core clears when it finalizes an input. The file is
-- read as importtx reads it, for the test account.
corePayment :: Ctx -> IO TxSignData
corePayment ctx = do
  text <- BS.readFile "test/data/psbt/core/pending-payment-finalized.psbt"
  file <- expectRight $ psbtTxFile ctx Nothing [[FileAccount (testAccountPub ctx) Bip44 "m/44'/0'/0'" (Just testFingerprint) btc]] =<< decodePsbtBase64 text
  return (txFileData file)

importingSpec :: Ctx -> Spec
importingSpec ctx =
  describe "Importing a pending transaction" $ do
    let cfg = def :: Config
    it "locks the coins it spends and leaves the others free" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccountTwice ctx cfg accId
        first <- payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
        _ <- importTo ctx accId first
        held <- lockedCoins accId
        liftIO $ held `shouldMatchList` [(externalAddr 0, True), (externalAddr 1, False)]
        -- Only the free coin of 10000 is left to build from
        lift $
          dbShouldFailWith "The coins do not cover the amount and the fee" $
            payment ctx cfg accId [] (otherAddr 1) 12000
        second <- payment ctx cfg accId [] (otherAddr 1) 7000
        liftIO $
          sortedOutputs (txSignDataTx second)
            `shouldBe` builtTx ctx 800002 [(txHash (secondFunding ctx), 0)] [(internalAddr 1, 2774), (otherAddr 1, 7000)]
        _ <- importTo ctx accId second
        bothHeld <- lockedCoins accId
        liftIO $ bothHeld `shouldMatchList` [(externalAddr 0, True), (externalAddr 1, True)]
        lift $
          dbShouldFailWith "The coins do not cover the amount and the fee" $
            payment ctx cfg accId [] (otherAddr 2) 1000
    it "takes no change address for a transaction without change" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        -- 19808 to otherAddr 2 leaves exactly the fee of 192 for one input
        -- and one output at 1 satoshi per byte
        tsd <- payment ctx cfg accId [] (otherAddr 2) 19808
        liftIO $ do
          txSignDataTx tsd `shouldBe` builtTx ctx 800002 [(txHash (funding ctx), 0)] [(otherAddr 2, 19808)]
          txSignDataOutputPaths tsd `shouldBe` []
        -- One output, so nothing is shuffled and the hash is fixed. Computed
        -- apart from hw from the parts of the two transactions: version 2,
        -- lock time 800002. As a version 1 transaction with no lock time,
        -- the same computation gives afc69158…596c, and with final sequences
        -- as well 720a9cfe…696c.
        importTo ctx accId tsd `dbShouldBeE` "43a4f39c6be9b7e98f1b2f1fca9b79f5af1adc07ffa14b42b82cc08b3c7b4a84"
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [True]
        (dBAddressIndex <$> changeOf ctx cfg accId) `dbShouldBeE` 0
    it "holds a payment to an external address of the wallet as an internal transaction" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        _ <- genExtAddress ctx cfg accId ""
        tsd <- payment ctx cfg accId [] (externalAddr 1) 8000
        -- 8000 to the wallet's own /0/1 and the change to /1/0: 226 bytes at
        -- 1 satoshi per byte leave 11774
        liftIO $ do
          sortedOutputs (txSignDataTx tsd)
            `shouldBe` builtTx ctx 800002 [(txHash (funding ctx), 0)] [(externalAddr 1, 8000), (internalAddr 0, 11774)]
          txSignDataInputPaths tsd `shouldBe` [legacyPath (Deriv :/ 0 :/ 0)]
          txSignDataOutputPaths tsd `shouldMatchList` [legacyPath (Deriv :/ 0 :/ 1), legacyPath (Deriv :/ 1 :/ 0)]
        pendingHash <- importTo ctx accId tsd
        -- Only the fee leaves the wallet
        summaries <- pendingOf ctx accId Nothing Nothing
        liftIO $ do
          map pendingFlags summaries `shouldBe` [Just ((pendingHash, False, False, False))]
          map (summaryType . txInfoSummary) summaries `shouldBe` [TxInternal]
          map (summaryAmount . txInfoSummary) summaries `shouldBe` [-226]
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [True]
        -- The change address is taken
        (dBAddressIndex <$> changeOf ctx cfg accId) `dbShouldBeE` 1
    it "replaces the unsigned version with the signed one, and refuses the rest" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]
        lift $
          dbShouldFailWith "The transaction already exists" $
            importTo ctx accId unsigned
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        importTo ctx accId signed `dbShouldBeE` pendingHash
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
        lift $ do
          dbShouldFailWith "The transaction already exists" $
            importTo ctx accId signed
          dbShouldFailWith "Can not replace a signed transaction with an unsigned one" $
            importTo ctx accId unsigned
    it "replaces a signed version with one signed by another signer" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned
        (byWallet, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        byCore <- liftIO $ corePayment ctx
        -- Both versions are valid, with different signatures, so different ids
        liftIO $ do
          verifySignedTx btc ctx byCore `shouldBe` Right ()
          -- The id Bitcoin Core gives the signed transaction with decoderawtransaction
          txHash (txSignDataTx byCore) `shouldBe` "5e56f64882174577d459b3350a592658854a45813389796017e48a6abe94d584"
          txHash (txSignDataTx byCore) `shouldNotBe` txHash (txSignDataTx byWallet)
        importTo ctx accId byWallet `dbShouldBeE` pendingHash
        importTo ctx accId byCore `dbShouldBeE` pendingHash
        -- The wallet holds the version imported last: broadcast sends that one
        stored <- lookupPendingTx pendingHash
        liftIO $ (txSignDataTx . pendingTxData <$> stored) `shouldBe` Just (txSignDataTx byCore)
    -- Core clears the derivation records of the input it finalized: the
    -- file names no path of the input, and the wallet knows it
    it "takes the signatures of a version finalized by other software, and keeps the paths and the label held" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned {txSignDataLabel = "Rent"}
        byCore <- liftIO $ corePayment ctx
        liftIO $ txSignDataInputPaths byCore `shouldBe` []
        importTo ctx accId byCore `dbShouldBeE` pendingHash
        held <- requirePendingTx pendingHash
        liftIO $ do
          txSignDataTx held.pendingTxData `shouldBe` txSignDataTx byCore
          txSignDataInputPaths held.pendingTxData `shouldBe` [legacyPath (Deriv :/ 0 :/ 0)]
          txSignDataOutputPaths held.pendingTxData `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]
          txSignDataLabel held.pendingTxData `shouldBe` "Rent"
          txSignDataFingerprint held.pendingTxData `shouldBe` Just testFingerprint
        -- The payment is still the account's: 8000 sent and the fee of 226
        -- paid, 8226 out of the wallet
        summaries <- pendingOf ctx accId Nothing Nothing
        liftIO $ map (summaryAmount . txInfoSummary) summaries `shouldBe` [-8226]
    -- The user may change the label while the file is away to be signed
    it "keeps the label held when a signed version comes in with another label" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned {txSignDataLabel = "Rent of October"}
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned {txSignDataLabel = "Rent"} [(Bip44, testAccountKey)]
        _ <- importTo ctx accId signed
        ((.pendingTxData.txSignDataLabel) <$> requirePendingTx pendingHash) `dbShouldBeE` "Rent of October"

    it "takes the label of a signed version when the version held has none" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned {txSignDataLabel = "Rent"} [(Bip44, testAccountKey)]
        _ <- importTo ctx accId signed
        ((.pendingTxData.txSignDataLabel) <$> requirePendingTx pendingHash) `dbShouldBeE` "Rent"

    it "refuses a transaction pending in another portfolio, naming where it is" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (otherId, _) <- testNewAcc2 ctx
        unsigned <- unsignedPayment ctx cfg accId
        _ <- importTo ctx accId unsigned
        lift $
          dbShouldFailWith "The transaction is pending in portfolio0 of wallet0 on btc" $
            importTo ctx otherId unsigned
    it "refuses a signed transaction whose signatures do not check out" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        let broken = signed {txSignDataTx = withBrokenSignature (txSignDataTx signed)}
        lift $
          dbShouldFailWith "Invalid signature for input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj)" $
            importTo ctx accId broken
        pendingOf ctx accId Nothing Nothing `dbShouldBeE` []
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [False]
    it "keeps the unsigned version when the signed one does not check out" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        let broken = signed {txSignDataTx = withBrokenSignature (txSignDataTx signed)}
        lift $
          dbShouldFailWith "Invalid signature for input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj)" $
            importTo ctx accId broken
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]
    it "refuses a new version of a transaction that is online" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $
          dbShouldFailWith "The transaction is already online" $
            importTo ctx accId signed
    it "refuses a transaction spending a coin another pending transaction holds" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- signedPending ctx cfg accId
        let spend = spendingTx ctx [(txHash (funding ctx), 0)] [(otherAddr 3, 19000)]
        lift $
          dbShouldFailWith "A coin referenced by the transaction is locked" $
            importTo ctx accId (TxSignData spend [funding ctx] [legacyPath (Deriv :/ 0 :/ 0)] [] Nothing Nothing [] noLabel)
    it "locks the coin of another portfolio it spends, in that portfolio too" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        otherId <- otherAccount ctx cfg
        _ <- importTo ctx otherId (sweepOfFunding ctx)
        lockedCoins accId `dbShouldBeE` [(externalAddr 0, True)]
        lift $
          dbShouldFailWith "The coins do not cover the amount and the fee" $
            payment ctx cfg accId [] (otherAddr 0) 8000
    it "refuses a transaction spending a coin a pending transaction of another portfolio holds" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- signedPending ctx cfg accId
        otherId <- otherAccount ctx cfg
        lift $
          dbShouldFailWith "A coin referenced by the transaction is locked" $
            importTo ctx otherId (sweepOfFunding ctx)
    it "takes a Bitcoin Cash sweep of an outpoint a Bitcoin payment holds: the chains keep their coins apart" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- signedPending ctx cfg accId
        (cashId, _) <- testNewAccBch ctx
        _ <- genExtAddress ctx cfg cashId ""
        sweepHash <- importTo ctx cashId (cashSweepOfFunding ctx)
        -- It held no coin of the wallet on Bitcoin Cash, and the Bitcoin
        -- payment still holds its coin
        deleteTx ctx (const (toStore emptyStore)) sweepHash `dbShouldBeE` (0, 0)
        lockedCoins accId `dbShouldBeE` [(externalAddr 0, True)]
    it "refuses a transaction spending a coin the user locked" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        _ <- lockCoin =<< walletCoin def Nothing (OutPoint (txHash (funding ctx)) 0)
        let spend = spendingTx ctx [(txHash (funding ctx), 0)] [(otherAddr 3, 19000)]
        lift $
          dbShouldFailWith "A coin referenced by the transaction is locked by the user: unlockcoin frees it" $
            importTo ctx accId (TxSignData spend [funding ctx] [legacyPath (Deriv :/ 0 :/ 0)] [] Nothing Nothing [] noLabel)
    it "refuses under coin control a coin the user locked" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        _ <- lockCoin =<< walletCoin def Nothing (OutPoint (txHash (funding ctx)) 0)
        lift $
          dbShouldFailWith "A chosen coin does not exist or is not spendable" $
            payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
    it "refuses a transaction spending a coin the wallet does not have" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        -- A payment to the wallet's first address that the wallet never saw
        let unseen = spendingTx ctx [(fakeTxHash 3, 0)] [(externalAddr 0, 5000)]
            spend = spendingTx ctx [(txHash unseen, 0)] [(otherAddr 3, 4000)]
        lift $
          dbShouldFailWith "A coin referenced by the transaction does not exist" $
            importTo ctx accId (TxSignData spend [unseen] [legacyPath (Deriv :/ 0 :/ 0)] [] Nothing Nothing [] noLabel)
    it "refuses a transaction paying change to an address the wallet has not generated" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        -- Change to /1/1, when only /1/0 exists
        let spend = spendingTx ctx [(txHash (funding ctx), 0)] [(internalAddr 1, 11774), (otherAddr 0, 8000)]
        lift $
          dbShouldFailWith "Some referenced addresses do not exist" $
            importTo ctx accId (TxSignData spend [funding ctx] [legacyPath (Deriv :/ 0 :/ 0)] [legacyPath (Deriv :/ 1 :/ 1)] Nothing Nothing [] noLabel)
    -- The change of the payment is at the first change address of the
    -- bip44 account, /1/0
    it "holds the change address of the type its change went to, and leaves the first one of the other type free" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- signedPending ctx cfg accId
        pool <- poolOf accId
        (dBAddressAddress <$> changeAddress ctx cfg pool Bip44) `dbShouldBeE` "1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6"
        (dBAddressAddress <$> changeAddress ctx cfg pool Bip84) `dbShouldBeE` "bc1ql5j70a05srlvee3ugweyqukdfwwaph4pwaup74"

    -- The pool holds 20000 at the first receiving address of its bip44
    -- account and 10000 at that of its bip84 account: a payment of 25000
    -- needs both
    it "holds a payment that spends a bip44 and a bip84 coin of the pool together, and locks both" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (segwitId, _) <- testNewSegwitAcc ctx
        fundAccount ctx cfg accId
        _ <- genExtAddress ctx cfg segwitId ""
        let segwitFunding = spendingTx ctx [(fakeTxHash 2, 0)] [(segwitExternalAddr 0, 10000)]
        lift $ insertRawTx segwitFunding
        _ <- refreshCoins btc ctx segwitId [segwitExternalAddr 0] [storeCoin ctx (txHash segwitFunding, 0) (Just 0) (segwitExternalAddr 0) 10000]
        unsigned <- payment ctx cfg accId [] (otherAddr 0) 25000
        pendingHash <- importTo ctx accId (signedByWallet ctx unsigned)
        summaries <- pendingOf ctx accId Nothing Nothing
        liftIO $ do
          map pendingFlags summaries `shouldBe` [Just (pendingHash, True, False, False)]
          map (Map.keys . summaryMyInputs . txInfoSummary) summaries `shouldBe` [[externalAddr 0, segwitExternalAddr 0]]
          map (summaryKinds . txInfoSummary) summaries `shouldBe` [[Bip44, Bip84]]
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [True]
        (map transactionLocked <$> coinsOfAccount segwitId) `dbShouldBeE` [True]

    it "refuses a transaction whose change address another pending transaction holds" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccountTwice ctx cfg accId
        first <- payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
        _ <- importTo ctx accId first
        -- The change of the first payment went to /1/0
        let spend = spendingTx ctx [(txHash (secondFunding ctx), 0)] [(internalAddr 0, 2774), (otherAddr 1, 7000)]
        lift $
          dbShouldFailWith "Some of the internal output addresses are not free" $
            importTo ctx accId (TxSignData spend [secondFunding ctx] [legacyPath (Deriv :/ 0 :/ 1)] [legacyPath (Deriv :/ 1 :/ 0)] Nothing Nothing [] noLabel)

    -- The first payment holds /1/0 for its change, and --change-back
    -- named /1/0 for the change of the second: the user chose it
    it "takes a transaction whose change goes back to the address --change-back named, though another pending transaction holds it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccountTwice ctx cfg accId
        first <- payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
        _ <- importTo ctx accId first
        pool <- poolOf accId
        let spend = spendingTx ctx [(txHash (secondFunding ctx), 0)] [(internalAddr 0, 2774), (otherAddr 1, 7000)]
        _ <- importPreparedTx ctx pool (Just (internalAddr 0)) (TxSignData spend [secondFunding ctx] [legacyPath (Deriv :/ 0 :/ 1)] [legacyPath (Deriv :/ 1 :/ 0)] Nothing Nothing [] noLabel)
        (length <$> pendingTxs pool) `dbShouldBeE` 2

    -- --change-back named the receiving address /0/1, which the payment
    -- does not pay: its change at /1/0 is the builder's, and the first
    -- payment holds it
    it "refuses a change address another pending transaction holds, when --change-back named another address" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccountTwice ctx cfg accId
        first <- payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
        _ <- importTo ctx accId first
        pool <- poolOf accId
        let spend = spendingTx ctx [(txHash (secondFunding ctx), 0)] [(internalAddr 0, 2774), (otherAddr 1, 7000)]
        lift $
          dbShouldFailWith "Some of the internal output addresses are not free" $
            importPreparedTx ctx pool (Just (externalAddr 1)) (TxSignData spend [secondFunding ctx] [legacyPath (Deriv :/ 0 :/ 1)] [legacyPath (Deriv :/ 1 :/ 0)] Nothing Nothing [] noLabel)

    -- The change address /1/0 has had a transaction
    it "refuses a transaction whose change goes to a used change address" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        updateAddressBalances btc accId [Store.Balance (internalAddr 0) 0 0 0 1 5000]
        let spend = spendingTx ctx [(txHash (funding ctx), 0)] [(internalAddr 0, 11774), (otherAddr 0, 8000)]
        lift $
          dbShouldFailWith "Some of the internal output addresses are not free" $
            importTo ctx accId (TxSignData spend [funding ctx] [legacyPath (Deriv :/ 0 :/ 0)] [legacyPath (Deriv :/ 1 :/ 0)] Nothing Nothing [] noLabel)

    it "takes a transaction whose change goes back to a used change address --change-back named" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        updateAddressBalances btc accId [Store.Balance (internalAddr 0) 0 0 0 1 5000]
        pool <- poolOf accId
        let spend = spendingTx ctx [(txHash (funding ctx), 0)] [(internalAddr 0, 11774), (otherAddr 0, 8000)]
        _ <- importPreparedTx ctx pool (Just (internalAddr 0)) (TxSignData spend [funding ctx] [legacyPath (Deriv :/ 0 :/ 0)] [legacyPath (Deriv :/ 1 :/ 0)] Nothing Nothing [] noLabel)
        (length <$> pendingTxs pool) `dbShouldBeE` 1

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "Listing the pending transactions" $ do
    let cfg = def :: Config
    it "lists them newest first, and after a given one when asked" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (first, second) <- twoPending ctx cfg accId
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((second, False, False, False)), Just ((first, False, False, False))]
        cursor <- cursorAmong [accId] second
        ((pendingFlags <$>) <$> pendingOf ctx accId (Just cursor) (Just 1))
          `dbShouldBeE` [Just ((first, False, False, False))]

    it "tells where --after goes on after a transaction, and refuses one that is not among those listed" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        otherId <- otherAccount ctx cfg
        (first, _) <- twoPending ctx cfg accId
        cursor <- cursorAmong [accId] first
        pendingCursor Nothing first `dbShouldBeE` cursor
        lift $ dbShouldFailWith "The pending transaction is not among those listed" $ cursorAmong [otherId] first
        lift $ dbShouldFailWith "The pending transaction does not exist" $ pendingCursor Nothing (fakeTxHash 9)

    -- The listing is ordered by the nosigHash, whichever hash names the
    -- transaction
    it "goes on after a signed transaction named by its txid, at its creation time and its nosigHash" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        let key = nosigHash unsigned.txSignDataTx
        lift . P.insert_ =<< heldRow accId signed
        cursorAmong [accId] (txHash signed.txSignDataTx) `dbShouldBeE` (fixtureTime, key)
    it "lists nothing for an account without any" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        pendingOf ctx accId Nothing Nothing `dbShouldBeE` []

    -- Portfolio 0 of wallet0 pays from its first coin, then portfolio 0
    -- of wallet1 holds a sweep of the second coin of the first to its
    -- first address
    it "lists those of every portfolio, newest first, each with where it is" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        otherId <- otherAccount ctx cfg
        fundAccountTwice ctx cfg accId
        paid <- importTo ctx accId =<< payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
        swept <- importTo ctx otherId (sweepOfSecondFunding ctx)
        (map (\(place, info) -> (place, pendingFlags info)) <$> pendingAfter ctx Nothing Nothing Nothing)
          `dbShouldBeE` [(Place (PortfolioRef 1 0) btc, Just (swept, False, False, False)), (mainPlace, Just (paid, False, False, False))]
        otherPool <- poolOf otherId
        (map (\(place, info) -> (place, pendingFlags info)) <$> pendingAfter ctx (Just [otherPool]) Nothing Nothing)
          `dbShouldBeE` [(Place (PortfolioRef 1 0) btc, Just (swept, False, False, False))]

    it "gives each the time it was created" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        lift . P.insert_ =<< heldRow accId unsigned
        (map createdOf <$> pendingOf ctx accId Nothing Nothing) `dbShouldBeE` [Just fixtureTime]

    it "keeps the time it was created when a signed version takes its place" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        lift . P.insert_ =<< heldRow accId unsigned
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        _ <- importTo ctx accId signed
        (map createdOf <$> pendingOf ctx accId Nothing Nothing) `dbShouldBeE` [Just fixtureTime]

    -- As a file of an offline signer would be
    it "knows no creation time for signing data the wallet does not hold" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pool <- poolOf accId
        (createdOf <$> txInfoOf ctx pool unsigned) `dbShouldBeE` Nothing
    it "reports a row whose signing data does not decode, instead of hiding it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        pool <- poolOf accId
        lift $ P.insert_ $ DBPendingTx pool.poolPortfolio "btc" (hashBytes (fakeTxHash 7)) "not signing data" False False fixtureTime
        listed <- lift $ runExceptT $ pendingOf ctx accId Nothing Nothing
        deleted <- lift $ runExceptT $ deleteTx ctx (const (toStore emptyStore)) (fakeTxHash 7)
        liftIO $ do
          listed `shouldSatisfy` isLeft
          deleted `shouldSatisfy` isLeft
          deleted `shouldNotBe` Left noPendingTx

-- pendingtxs shows an unsigned transaction by its nosigHash, and a signed
-- one by its txid. The commands take either.
namingSpec :: Ctx -> Spec
namingSpec ctx =
  describe "A pending transaction named by a command" $ do
    let cfg = def :: Config
    it "is found by its txid once signed" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (_, signed) <- signedPending ctx cfg accId
        ((.pendingTxData) <$> requirePendingTx (txHash signed.txSignDataTx)) `dbShouldBeE` signed
    it "is still found by its nosigHash once signed" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        ((.pendingTxData) <$> requirePendingTx pendingHash) `dbShouldBeE` signed

    -- The short id of a hash is its first 8 digits
    it "is found by the short id of either of its hashes once signed" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        let txid = txHash signed.txSignDataTx
        pendingHashesWith (shortIdOf pendingHash) `dbShouldBeE` [pendingHash]
        pendingHashesWith (shortIdOf txid) `dbShouldBeE` [txid]

    -- The hashes of the pending transaction are real ones, which do not
    -- start with 00000000
    it "is not found by a short id neither of its hashes has" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- signedPending ctx cfg accId
        pendingHashesWith (typedShortId "00000000") `dbShouldBeE` []

labellingSpec :: Ctx -> Spec
labellingSpec ctx =
  describe "Labelling a transaction" $ do
    let cfg = def :: Config
        -- Where the transactions of a labelling answer are, each with the
        -- label of its transaction
        labelsOf answered = [(place, txInfoLabel info) | (place, info) <- answered]
        otherPlace = Place (PortfolioRef 1 0) btc
    it "keeps the label of a pending transaction in its signing data, which its file carries" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned
        (labelsOf <$> labelTransaction ctx pendingHash "Rent") `dbShouldBeE` [(mainPlace, "Rent")]
        ((.pendingTxData.txSignDataLabel) <$> requirePendingTx pendingHash) `dbShouldBeE` "Rent"

    it "finds a signed pending transaction by its txid" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        _ <- labelTransaction ctx (txHash signed.txSignDataTx) "Rent"
        ((.pendingTxData.txSignDataLabel) <$> requirePendingTx pendingHash) `dbShouldBeE` "Rent"

    it "removes the label of a pending transaction with an empty one" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned {txSignDataLabel = "Rent"}
        (labelsOf <$> labelTransaction ctx pendingHash "") `dbShouldBeE` [(mainPlace, "")]
        ((.pendingTxData.txSignDataLabel) <$> requirePendingTx pendingHash) `dbShouldBeE` ""

    -- A payment from one portfolio to another is in the history of both
    it "labels a transaction of the history in every portfolio that has it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (otherId, _) <- testNewAcc2 ctx
        forM_ [accId, otherId] $ \holder -> do
          pool <- poolOf holder
          lift $ repsertSummary pool (fakeTxHash 5) (Store.BlockRef 800000 1) fixtureTime paymentSummary
        labelled <- labelTransaction ctx (fakeTxHash 5) "To savings"
        liftIO $ labelsOf labelled `shouldMatchList` [(mainPlace, "To savings"), (otherPlace, "To savings")]
        labelOf accId (fakeTxHash 5) `dbShouldBeE` "To savings"
        labelOf otherId (fakeTxHash 5) `dbShouldBeE` "To savings"

    it "labels a transaction of the history in no portfolio that does not have it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (otherId, _) <- testNewAcc2 ctx
        pool <- poolOf accId
        _ <- lift $ repsertSummary pool (fakeTxHash 5) (Store.BlockRef 800000 1) fixtureTime paymentSummary
        (labelsOf <$> labelTransaction ctx (fakeTxHash 5) "Salary") `dbShouldBeE` [(mainPlace, "Salary")]
        labelOf otherId (fakeTxHash 5) `dbShouldBeE` ""

    it "refuses a transaction the wallet does not have" $
      inDatabase $ do
        _ <- runExceptT $ testNewAcc ctx
        dbShouldFailWith "The wallet has no transaction 0000000000000000000000000000000000000000000000000000000000000005" $
          labelTransaction ctx (fakeTxHash 5) "Rent"

sendingSpec :: Ctx -> Spec
sendingSpec ctx =
  describe "Sending a pending transaction" $ do
    let cfg = def :: Config
    it "marks it online once the store sees it, and stores its raw transaction" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        (_, sent) <- sendPending ctx (const (toStore (storeKnowing ctx signed))) withoutAsking pendingHash
        liftIO $ pendingFlags sent `shouldBe` Just ((pendingHash, True, True, False))
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` True
    -- The row is keyed by the nosigHash, which marking it online must use
    it "marks a transaction named by its txid online" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        _ <- sendPending ctx (const (toStore (storeKnowing ctx signed))) withoutAsking (txHash signed.txSignDataTx)
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
    it "sends again a transaction that is already online" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        -- As if sent once, then lost by the store
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $ insertRawTx (txSignDataTx signed)
        published <- liftIO $ newIORef []
        (_, sent) <- sendPending ctx (const (notingPublished published (toStore (storeKnowing ctx signed)))) withoutAsking pendingHash
        liftIO $ readIORef published `shouldReturn` [txSignDataTx signed]
        liftIO $ pendingFlags sent `shouldBe` Just ((pendingHash, True, True, False))
        -- The raw transaction is stored once, next to the parent of its coin
        lift (P.count ([] :: [P.Filter DBRawTx])) `dbShouldBeE` 2
    it "records nothing when the store does not see it, and says so" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, _) <- signedPending ctx cfg accId
        lift $
          dbShouldFailWith "The transaction was handed to the network but the store has not seen it after 10 seconds. It stays signed and not online: run broadcast again later" $
            sendPending ctx (const (toStore emptyStore)) withoutAsking pendingHash
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
    it "sends nothing when the store is not healthy" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, _) <- signedPending ctx cfg accId
        published <- liftIO $ newIORef []
        lift $
          dbShouldFailWith "The store is down" $
            sendPending ctx (const (notingPublished published (unhealthy (toStore emptyStore)))) withoutAsking pendingHash
        liftIO $ readIORef published `shouldReturn` []
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
    it "refuses a transaction that is not signed" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned
        lift $
          dbShouldFailWith "The transaction is not signed" $
            sendPending ctx (const (toStore emptyStore)) withoutAsking pendingHash
    it "refuses a transaction the wallet does not hold" $
      inDatabaseE $
        lift $
          dbShouldFailWith "The pending transaction does not exist" $
            sendPending ctx (const (toStore emptyStore)) withoutAsking (fakeTxHash 7)
    -- What broadcast shows the user, before it asks: signed, and not online yet
    it "gives the step before sending the transaction as it is held, on the network of its pool" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        seen <- liftIO $ newIORef []
        _ <- sendPending ctx (const (toStore (storeKnowing ctx signed))) (noting seen) pendingHash
        liftIO $ readIORef seen `shouldReturn` [("btc", Just (pendingHash, True, False, False))]
    it "sends nothing and records nothing when the step before sending refuses" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        published <- liftIO $ newIORef []
        lift $
          dbShouldFailWith "The transaction was not sent" $
            sendPending ctx (const (notingPublished published (toStore (storeKnowing ctx signed)))) refusing pendingHash
        liftIO $ readIORef published `shouldReturn` []
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
    it "gives up after the asks it is allowed" $ do
      asks <- newIORef 0
      let tx = spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 8000)]
      runExceptT (broadcastWith 1000 3 (countingAsks asks (toStore emptyStore)) tx) `shouldReturn` Right False
      readIORef asks `shouldReturn` 3
    it "sees the transaction on the first ask" $ do
      asks <- newIORef 0
      let tsd = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 8000)]) [] [] [] Nothing Nothing [] noLabel
      runExceptT (broadcastWith 1000 3 (countingAsks asks (toStore (storeKnowing ctx tsd))) (txSignDataTx tsd))
        `shouldReturn` Right True
      readIORef asks `shouldReturn` 1

sendingFileSpec :: Ctx -> Spec
sendingFileSpec ctx =
  describe "Sending a signed transaction from a file" $ do
    let cfg = def :: Config
    it "sends the signed version of a transaction the wallet holds, and marks it online" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        pendingHash <- importTo ctx accId unsigned
        -- The file the offline signer returns
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        sent <- sendSigned ctx (const (toStore (storeKnowing ctx signed))) withoutAsking Nothing btc signed
        liftIO $ case sent of
          SentPending _ info -> pendingFlags info `shouldBe` Just ((pendingHash, True, True, False))
          SentOutside _ _ -> expectationFailure "A transaction of the wallet was sent as an outside one"
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        held <- requirePendingTx pendingHash
        liftIO $ held.pendingTxData `shouldBe` signed
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` True
    -- The user may change the label while the file is away to be signed
    it "sends a file whose label differs from the one held, keeping the label held" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        pendingHash <- importTo ctx accId signed {txSignDataLabel = "Rent of October"}
        sent <- sendSigned ctx (const (toStore (storeKnowing ctx signed))) withoutAsking Nothing btc signed {txSignDataLabel = "Rent"}
        liftIO $ case sent of
          SentPending _ info -> (pendingFlags info, txInfoLabel info) `shouldBe` (Just (pendingHash, True, True, False), "Rent of October")
          SentOutside _ _ -> expectationFailure "A transaction of the wallet was sent as an outside one"

    -- As a file of another wallet, which names no network: --network or
    -- the configuration file says it
    it "hands a transaction the wallet does not hold to the store of the network given for a file that names none" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        sent <- sendSigned ctx (const (toStore (storeKnowing ctx signed))) withoutAsking Nothing btc signed
        liftIO $ case sent of
          SentOutside net txid -> (net.name, txid) `shouldBe` ("btc", txHash (txSignDataTx signed))
          SentPending _ _ -> expectationFailure "A transaction the wallet does not hold was sent as pending"

    it "refuses a network other than the one it is pending on" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        _ <- importTo ctx accId unsigned
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        lift $
          dbShouldFailWith "The transaction is pending on another network" $
            sendSigned ctx (const (toStore emptyStore)) withoutAsking (Just bch) bch signed
    it "hands a transaction the wallet does not hold to the store of the given network, and records nothing" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        -- The payment is built but never held, as a file prepared elsewhere
        -- would be: the next sync catches up with it
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        published <- liftIO $ newIORef []
        sent <- sendSigned ctx (const (notingPublished published (toStore (storeKnowing ctx signed)))) withoutAsking (Just btc) btc signed
        liftIO $ case sent of
          SentOutside net txid -> (net.name, txid) `shouldBe` ("btc", txHash (txSignDataTx signed))
          SentPending _ _ -> expectationFailure "A transaction the wallet does not hold was sent as pending"
        liftIO $ readIORef published `shouldReturn` [txSignDataTx signed]
        pendingOf ctx accId Nothing Nothing `dbShouldBeE` []
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` False
    -- The payment is never held, as a file prepared elsewhere: its nosigHash
    -- is that of its transaction
    it "gives the step before sending a transaction the wallet does not hold, on the network of its file" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        seen <- liftIO $ newIORef []
        _ <- sendSigned ctx (const (toStore (storeKnowing ctx signed))) (noting seen) (Just btc) btc signed
        liftIO $ readIORef seen `shouldReturn` [("btc", Just (nosigHash (txSignDataTx signed), True, False, False))]
    it "sends nothing when the step before sending refuses a transaction the wallet does not hold" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        published <- liftIO $ newIORef []
        lift $
          dbShouldFailWith "The transaction was not sent" $
            sendSigned ctx (const (notingPublished published (toStore (storeKnowing ctx signed)))) refusing (Just btc) btc signed
        liftIO $ readIORef published `shouldReturn` []
    it "refuses a transaction the wallet does not hold whose signatures do not check out" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        (signed, _) <- liftEither $ signWalletTx btc ctx unsigned [(Bip44, testAccountKey)]
        let broken = signed {txSignDataTx = withBrokenSignature (txSignDataTx signed)}
        lift $
          dbShouldFailWith "Invalid signature for input 0 (1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj)" $
            sendSigned ctx (const (toStore emptyStore)) withoutAsking (Just btc) btc broken
    it "refuses an unsigned file" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        lift $
          dbShouldFailWith "The transaction is not signed" $
            sendSigned ctx (const (toStore emptyStore)) withoutAsking (Just btc) btc unsigned

deletingSpec :: Ctx -> Spec
deletingSpec ctx =
  describe "Deleting a pending transaction" $ do
    let cfg = def :: Config
    it "abandons a sent transaction the store does not know, freeing its coin" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        -- As if sent: online, with its raw transaction stored next to the
        -- parent of its coin
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $ insertRawTx (txSignDataTx signed)
        lift (P.count ([] :: [P.Filter DBRawTx])) `dbShouldBeE` 2
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [True]
        deleteTx ctx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (1, 1)
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [False]
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing) `dbShouldBeE` []
        -- Its raw transaction went with it; the parent of the coin stays
        lift (P.count ([] :: [P.Filter DBRawTx])) `dbShouldBeE` 1
    it "abandons a sent transaction the network dropped" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        -- As if sent, then replaced in the mempool by another transaction
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $ insertRawTx (txSignDataTx signed)
        deleteTx ctx (const (toStore (storeThatDropped ctx signed))) pendingHash `dbShouldBeE` (1, 1)
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [False]
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing) `dbShouldBeE` []
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` False
    it "keeps a sent transaction when the store cannot be asked" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $ insertRawTx (txSignDataTx signed)
        lift $
          dbShouldFailWith "The store is down" $
            deleteTx ctx (const (notAnswering (toStore emptyStore))) pendingHash
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [True]
        lift (hasRawTx (txHash (txSignDataTx signed))) `dbShouldBeE` True
    it "deletes a transaction that was never sent without asking the store" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, _) <- signedPending ctx cfg accId
        deleteTx ctx (const (notAnswering (unhealthy (toStore emptyStore)))) pendingHash `dbShouldBeE` (1, 1)
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing) `dbShouldBeE` []
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [False]
    it "keeps a sent transaction the store still lists" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        lift $
          dbShouldFailWith "The transaction is in the network's mempool or in the chain: sync retires it once it is listed" $
            deleteTx ctx (const (toStore (storeKnowing ctx signed))) pendingHash
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [True]
    -- The coin of 20000 it spends, and its change address
    it "deletes a signed transaction named by its txid" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (_, signed) <- signedPending ctx cfg accId
        deleteTx ctx (const (toStore emptyStore)) (txHash signed.txSignDataTx) `dbShouldBeE` (1, 1)
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing) `dbShouldBeE` []
    it "frees the change address for the next transaction" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, _) <- signedPending ctx cfg accId
        -- The change went to /1/0
        (dBAddressIndex <$> changeOf ctx cfg accId) `dbShouldBeE` 1
        deleteTx ctx (const (toStore emptyStore)) pendingHash `dbShouldBeE` (1, 1)
        (dBAddressIndex <$> changeOf ctx cfg accId) `dbShouldBeE` 0
    it "retiring a sent transaction, as the sync does, frees its coin but keeps its change address" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, _) <- signedPending ctx cfg accId
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        held <- requirePendingTx pendingHash
        lift $ retirePendingTx held (txHash (txSignDataTx held.pendingTxData))
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing) `dbShouldBeE` []
        (map transactionLocked <$> coinsOfAccount accId) `dbShouldBeE` [False]
        -- The sync that retires it also records the change paid to /1/0,
        -- and an address paid on the chain is never used again
        updateAddressBalances btc accId [Store.Balance (internalAddr 0) 11774 0 1 1 11774]
        (dBAddressIndex <$> changeOf ctx cfg accId) `dbShouldBeE` 1
    -- A miner may re-encode the signatures: the network has it as 9
    it "retiring a labelled transaction gives its label to the id the network has it under" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, _) <- signedPending ctx cfg accId
        _ <- labelTransaction ctx pendingHash "Rent"
        held <- requirePendingTx pendingHash
        lift $ retirePendingTx held (fakeTxHash 9)
        labelOf accId (fakeTxHash 9) `dbShouldBeE` "Rent"

    it "retiring a transaction without a label labels nothing" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        (pendingHash, signed) <- signedPending ctx cfg accId
        held <- requirePendingTx pendingHash
        lift $ retirePendingTx held (txHash signed.txSignDataTx)
        lift (tableRows "d_b_tx_label") `dbShouldBeE` []

    it "counts and frees the coin of another portfolio it held" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        otherId <- otherAccount ctx cfg
        sweepHash <- importTo ctx otherId (sweepOfFunding ctx)
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
      conflictedInputs [parent] Set.empty spend `shouldBe` []
    it "are the coins another transaction spends" $
      conflictedInputs [withSpender 0 (fakeTxHash 8, 0) parent] Set.empty spend `shouldBe` [coin]
    it "leave out a coin the transaction itself spends, once the network has it" $
      conflictedInputs [withSpender 0 (txHash spend, 0) parent] Set.empty spend `shouldBe` []
    it "are the coins of a transaction the store dropped" $
      conflictedInputs [replaced parent] Set.empty spend `shouldBe` [coin]
    -- A replacement spends the coins of the transaction it replaces
    it "leave out a coin spent by a transaction it replaces" $
      conflictedInputs [withSpender 0 (fakeTxHash 8, 0) parent] (Set.singleton (fakeTxHash 8)) spend `shouldBe` []
    it "leave out the coins of a transaction the store has not seen, which proves nothing yet" $
      conflictedInputs [] Set.empty spend `shouldBe` []
    it "name the other transactions that spend its coins, each once" $ do
      let twoCoins = spendingTx ctx [(txHash (funding ctx), 0), (txHash (funding ctx), 1)] [(otherAddr 0, 19000)]
          bothSpent = withSpender 1 (fakeTxHash 8, 1) $ withSpender 0 (fakeTxHash 8, 0) $
            payoutTx ctx (txHash (funding ctx)) (Store.BlockRef 800000 1) [(externalAddr 0, 20000), (externalAddr 1, 10000)]
      rivalSpenders [bothSpent] twoCoins `shouldBe` [fakeTxHash 8]
    it "name no rival while the coins are unspent, or spent by the transaction itself" $ do
      rivalSpenders [parent] spend `shouldBe` []
      rivalSpenders [withSpender 0 (txHash spend, 0) parent] spend `shouldBe` []
    it "are the coins past the outputs of their transaction" $
      conflictedInputs [parent] Set.empty (spendingTx ctx [(txHash (funding ctx), 1)] [(otherAddr 0, 19000)])
        `shouldBe` [OutPoint (txHash (funding ctx)) 1]

{- The fingerprint a signed transaction names -}

-- A wallet learns the fingerprint of its seed from the seed, or from a
-- file of the wallet. A signed transaction names the seed that signed it,
-- which the wallet cannot check against its keys.
fingerprintSpec :: Ctx -> Spec
fingerprintSpec ctx =
  describe "The seed a signed transaction names" $ do
    let cfg = def :: Config
        -- The payment of these tests, signed, its file naming the seed
        signedNaming fingerprint accId = do
          unsigned <- unsignedPayment ctx cfg accId
          fst <$> liftEither (signWalletTx btc ctx unsigned {txSignDataFingerprint = Just fingerprint} [(Bip44, testAccountKey)])
    it "does not change the fingerprint of the wallet, which its seed gave" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- importTo ctx accId =<< signedNaming testFingerprint2 accId
        (fmap (dBWalletFingerprint . snd) <$> lift (walletAt 0)) `dbShouldBeE` Just (Just "bc560f95")

    -- The wallet is as the step from hw 0.9 leaves it: one account, and no
    -- fingerprint
    it "is not taken for the fingerprint of a wallet that has none yet" $
      inDatabaseE $ do
        (walletId, _) <- lift $ insertWallet Nothing
        (portfolioKey, _) <- insertPortfolio walletId 0 ""
        (accId, _) <- insertAccount portfolioKey Bip44 btc ctx (testAccountPub ctx)
        _ <- importTo ctx accId =<< signedNaming testFingerprint accId
        (fmap (dBWalletFingerprint . snd) <$> lift (walletAt 0)) `dbShouldBeE` Just Nothing

    -- A counterparty names the seed 4435aa1c in a file the user signed,
    -- and hands it over first: the next genuine file, of another payment,
    -- is not refused for it
    it "does not stand in the way of the next signed transaction, which names another seed" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccountTwice ctx cfg accId
        let signedBy :: Fingerprint -> TxSignData -> ExceptT String (DB IO) TxSignData
            signedBy fingerprint unsigned = fst <$> liftEither (signWalletTx btc ctx unsigned {txSignDataFingerprint = Just fingerprint} [(Bip44, testAccountKey)])
        first <- importTo ctx accId =<< signedBy testFingerprint2 =<< payment ctx cfg accId [OutPoint (txHash (funding ctx)) 0] (otherAddr 0) 8000
        second <- importTo ctx accId =<< signedBy testFingerprint =<< payment ctx cfg accId [] (otherAddr 1) 7000
        ((pendingFlags <$>) <$> pendingOf ctx accId Nothing Nothing)
          `dbShouldBeE` [Just (second, True, False, False), Just (first, True, False, False)]

    it "stays in the signing data held, as the signer wrote it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        pendingHash <- importTo ctx accId =<< signedNaming testFingerprint accId
        ((.pendingTxData.txSignDataFingerprint) <$> requirePendingTx pendingHash) `dbShouldBeE` Just testFingerprint

{- The pool of a transaction -}

-- | A transaction between two addresses outside the wallet, as a sweep of
-- keys kept elsewhere to another wallet is: it involves no portfolio
outsideTx :: Ctx -> TxSignData
outsideTx ctx = TxSignData (spendingTx ctx [(txHash parent, 0)] [(otherAddr 0, 9000)]) [parent] [] [] Nothing Nothing [] noLabel
  where
    parent = spendingTx ctx [(fakeTxHash 9, 0)] [(otherAddr 1, 10000)]

-- | The signing data without the paths of the addresses of any portfolio,
-- as a file of other software names them: the wallet finds them itself
withoutPaths :: TxSignData -> TxSignData
withoutPaths tsd = tsd {txSignDataInputPaths = [], txSignDataOutputPaths = []}

-- | A transaction from outside the wallet that pays 9000 to the first
-- receiving address of the test wallet. It names no path.
incoming :: Ctx -> TxSignData
incoming ctx = TxSignData (spendingTx ctx [(txHash parent, 0)] [(externalAddr 0, 9000)]) [parent] [] [] Nothing Nothing [] noLabel
  where
    parent = spendingTx ctx [(fakeTxHash 9, 0)] [(otherAddr 1, 10000)]

-- | A transaction from outside the wallet that pays 9000 to the first
-- receiving address of portfolio 1 of the test wallet. It names no path.
incomingToPortfolio1 :: Ctx -> TxSignData
incomingToPortfolio1 ctx = TxSignData (spendingTx ctx [(txHash parent, 0)] [(account1Addr, 9000)]) [parent] [] [] Nothing Nothing [] noLabel
  where
    parent = spendingTx ctx [(fakeTxHash 9, 0)] [(otherAddr 1, 10000)]

-- | A transfer between two portfolios of the test wallet: portfolio 0,
-- funded as by 'fundAccount', pays 8000 to the first receiving address of
-- portfolio 1, which the wallet has generated. The transfer is unsigned,
-- with the paths of portfolio 0 that a file of portfolio 0 names. Gives
-- the pool of portfolio 0 on Bitcoin, which pays, portfolio 1, and the
-- transfer.
transferToPortfolio1 :: Ctx -> Config -> ExceptT String (DB IO) (Pool, Portfolio, TxSignData)
transferToPortfolio1 ctx cfg = do
  (accId, _) <- testNewAcc ctx
  second <- nextPortfolioOf ctx testMnemonic
  (secondId, _) <- accountOf second btc Bip44
  _ <- genExtAddress ctx cfg secondId ""
  fundAccount ctx cfg accId
  transfer <- payment ctx cfg accId [] account1Addr 8000
  payer <- poolOf accId
  return (payer, second, transfer)

-- | The transaction paying 15000 to the first receiving address of the
-- second test wallet: the coin of the other portfolio of these tests
otherFunding :: Ctx -> Tx
otherFunding ctx = spendingTx ctx [(fakeTxHash 3, 0)] [(externalAddr2, 15000)]

-- | A transaction two portfolios pay together: it spends the coin of
-- 'funding', of the test wallet, and that of 'otherFunding', of the second
-- test wallet, 35000 in all, and pays 34000 to an outside address. It
-- names no path.
jointSpend :: Ctx -> TxSignData
jointSpend ctx =
  TxSignData
    (spendingTx ctx [(txHash (funding ctx), 0), (txHash (otherFunding ctx), 0)] [(otherAddr 0, 34000)])
    [funding ctx, otherFunding ctx]
    []
    []
    Nothing
    Nothing
    []
    noLabel

-- | A transaction from outside the wallet that pays two portfolios: 5000
-- to the first receiving address of the test wallet and 4000 to that of
-- the second test wallet. It names no path.
jointPayout :: Ctx -> TxSignData
jointPayout ctx =
  TxSignData (spendingTx ctx [(txHash parent, 0)] [(externalAddr 0, 5000), (externalAddr2, 4000)]) [parent] [] [] Nothing Nothing [] noLabel
  where
    parent = spendingTx ctx [(fakeTxHash 9, 0)] [(otherAddr 1, 10000)]

-- | A payment of the bip44 account of portfolio 0 of the test wallet on a
-- test network: 30000 at its first receiving address, of which 29000 go
-- to an outside address. It names the path of its input, and no network.
testnetSpend :: Ctx -> TxSignData
testnetSpend ctx =
  TxSignData (spendingTx ctx [(txHash parent, 0)] [(otherAddr 0, 29000)]) [parent] [legacyPath (Deriv :/ 0 :/ 0)] [] Nothing Nothing [] noLabel
  where
    parent = spendingTx ctx [(fakeTxHash 4, 0)] [(testnetExternalAddr, 30000)]

-- The payment is from portfolio 0 of the test wallet, wallet0, on
-- Bitcoin. The other portfolio is portfolio 0 of the second test wallet,
-- wallet1. The third argument of transactionPool is the network of the
-- file or of --network, and the fourth the default network.
poolOfTxSpec :: Ctx -> Spec
poolOfTxSpec ctx =
  describe "The pool of a transaction to sign, show or import" $ do
    let cfg = def :: Config
        found = fmap fst
    it "is the pool that holds it pending" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- testNewAcc2 ctx
        unsigned <- unsignedPayment ctx cfg accId
        _ <- importTo ctx accId unsigned
        pool <- poolOf accId
        found (transactionPool ctx Nothing Nothing btc unsigned) `dbShouldBeE` Just pool

    it "refuses another portfolio than the one that holds it pending, naming both" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        other <- portfolioOf ctx testMnemonic2
        unsigned <- unsignedPayment ctx cfg accId
        _ <- importTo ctx accId unsigned
        lift $
          dbShouldFailWith "The transaction is pending in portfolio0 of wallet0 on btc, not in portfolio0 of wallet1" $
            transactionPool ctx (Just (onePortfolio other)) Nothing btc unsigned

    it "refuses a file of another network than the one it is pending on" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        _ <- importTo ctx accId unsigned
        lift $
          dbShouldFailWith "The transaction is pending on another network" $
            transactionPool ctx Nothing (Just bch) btc unsigned

    it "is the pool whose keys derive the addresses it spends, when it is not pending" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- testNewAcc2 ctx
        unsigned <- unsignedPayment ctx cfg accId
        pool <- poolOf accId
        found (transactionPool ctx Nothing Nothing btc unsigned) `dbShouldBeE` Just pool

    -- As a file signed and finalized by other software, which names no key
    -- of its signed inputs, or a file another wallet prepared, which may
    -- name no key of an output
    it "is the pool whose addresses it spends when it names no path of its inputs, and fills the paths in from the wallet" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- testNewAcc2 ctx
        unsigned <- unsignedPayment ctx cfg accId
        pool <- poolOf accId
        (chosen, completed) <- transactionPool ctx Nothing Nothing btc (withoutPaths unsigned)
        liftIO $ do
          chosen `shouldBe` Just pool
          txSignDataInputPaths completed `shouldBe` [legacyPath (Deriv :/ 0 :/ 0)]
          txSignDataOutputPaths completed `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]

    -- Portfolio 0 of the test wallet pays 8000 to the first receiving
    -- address of its portfolio 1, which the wallet has generated
    it "is the pool of the portfolio that pays, when the transaction moves funds between two portfolios" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        second <- nextPortfolioOf ctx testMnemonic
        (secondId, _) <- accountOf second btc Bip44
        _ <- genExtAddress ctx cfg secondId ""
        fundAccount ctx cfg accId
        transfer <- payment ctx cfg accId [] account1Addr 8000
        pool <- poolOf accId
        found (transactionPool ctx Nothing Nothing btc (withoutPaths transfer)) `dbShouldBeE` Just pool

    -- The same transfer: portfolio 1 is involved too, since it is paid
    it "is the pool of the portfolio that is paid when --portfolio names it, for a transaction between two portfolios" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        second <- nextPortfolioOf ctx testMnemonic
        (secondId, _) <- accountOf second btc Bip44
        _ <- genExtAddress ctx cfg secondId ""
        fundAccount ctx cfg accId
        transfer <- payment ctx cfg accId [] account1Addr 8000
        found (transactionPool ctx (Just (onePortfolio second)) Nothing btc (withoutPaths transfer)) `dbShouldBeE` Just (portfolioPool second btc)

    -- The file of the transfer names the paths of portfolio 0, which the
    -- keys of portfolio 1 do not derive: portfolio 1 reads it with the
    -- paths it knows of its own addresses
    it "is the pool of the portfolio a transfer pays when --portfolio names it, though the file names the paths of the one that pays" $
      inDatabaseE $ do
        (_, second, transfer) <- transferToPortfolio1 ctx cfg
        found (transactionPool ctx (Just (onePortfolio second)) Nothing btc transfer) `dbShouldBeE` Just (portfolioPool second btc)

    -- Portfolio 1 gets 8000, and spends none of the coins
    it "reads a transfer as the portfolio it pays sees it, when --portfolio names that one: incoming" $
      inDatabaseE $ do
        (_, second, transfer) <- transferToPortfolio1 ctx cfg
        (_, completed) <- transactionPool ctx (Just (onePortfolio second)) Nothing btc transfer
        summary <- txInfoSummary <$> txInfoOf ctx (portfolioPool second btc) completed
        liftIO $ (summaryType summary, summaryAmount summary, Map.keys (summaryMyInputs summary)) `shouldBe` (TxCredit, 8000, [])

    it "is the pool of the portfolio that pays a transfer, without an option, though the file names its paths" $
      inDatabaseE $ do
        (payer, _, transfer) <- transferToPortfolio1 ctx cfg
        found (transactionPool ctx Nothing Nothing btc transfer) `dbShouldBeE` Just payer

    -- Portfolio 1 of wallet0 is paid; portfolio 0 is the default one
    it "is the pool of any portfolio of the wallet --wallet names alone, not only of the default portfolio" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        second <- nextPortfolioOf ctx testMnemonic
        (secondId, _) <- accountOf second btc Bip44
        _ <- genExtAddress ctx cfg secondId ""
        narrowedM <- narrowedBy cfg (PortfolioChoice (Just 0) Nothing)
        found (transactionPool ctx narrowedM Nothing btc (incomingToPortfolio1 ctx)) `dbShouldBeE` Just (portfolioPool second btc)

    it "refuses the wallet --wallet names alone when it involves none of its portfolios, naming the wallet" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- portfolioOf ctx testMnemonic2
        unsigned <- unsignedPayment ctx cfg accId
        narrowedM <- narrowedBy cfg (PortfolioChoice (Just 1) Nothing)
        lift $
          dbShouldFailWith "The transaction does not involve wallet1" $
            transactionPool ctx narrowedM Nothing btc unsigned

    it "refuses the wallet --wallet names alone when the transaction is pending in another, naming both" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- portfolioOf ctx testMnemonic2
        unsigned <- unsignedPayment ctx cfg accId
        _ <- importTo ctx accId unsigned
        narrowedM <- narrowedBy cfg (PortfolioChoice (Just 1) Nothing)
        lift $
          dbShouldFailWith "The transaction is pending in portfolio0 of wallet0 on btc, not in wallet1" $
            transactionPool ctx narrowedM Nothing btc unsigned

    it "is the pool of the portfolio it pays, when no portfolio of the wallet pays" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- testNewAcc2 ctx
        fundAccount ctx cfg accId
        pool <- poolOf accId
        found (transactionPool ctx Nothing Nothing btc (incoming ctx)) `dbShouldBeE` Just pool

    it "asks for --portfolio when several portfolios pay, naming them" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        _ <- otherAccount ctx cfg
        lift $
          dbShouldFailWith "The transaction spends from several portfolios, portfolio0 of wallet0 on btc and portfolio0 of wallet1 on btc: name the one to use with --portfolio and --wallet" $
            transactionPool ctx Nothing Nothing btc (jointSpend ctx)

    it "is the pool of the portfolio --portfolio names, among several that pay" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        otherId <- otherAccount ctx cfg
        other <- portfolioOf ctx testMnemonic2
        otherPool <- poolOf otherId
        found (transactionPool ctx (Just (onePortfolio other)) Nothing btc (jointSpend ctx)) `dbShouldBeE` Just otherPool

    it "asks for --portfolio when no portfolio pays and it pays several, naming them" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        fundAccount ctx cfg accId
        _ <- otherAccount ctx cfg
        lift $
          dbShouldFailWith "The transaction pays several portfolios, portfolio0 of wallet0 on btc and portfolio0 of wallet1 on btc: name the one to use with --portfolio and --wallet" $
            transactionPool ctx Nothing Nothing btc (jointPayout ctx)

    it "refuses a portfolio it does not involve" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        other <- portfolioOf ctx testMnemonic2
        unsigned <- unsignedPayment ctx cfg accId
        lift $
          dbShouldFailWith "The transaction does not involve portfolio0 of wallet1" $
            transactionPool ctx (Just (onePortfolio other)) Nothing btc unsigned

    it "refuses a network other than that of the account it involves" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        unsigned <- unsignedPayment ctx cfg accId
        lift $
          dbShouldFailWith "The transaction involves an account of another network" $
            transactionPool ctx Nothing (Just bch) btc unsigned

    -- A whole portfolio has accounts on Bitcoin and on Bitcoin Cash: on
    -- either network, and without one
    it "is none when it involves no portfolio" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        found (transactionPool ctx Nothing (Just bch) btc (outsideTx ctx)) `dbShouldBeE` Nothing
        found (transactionPool ctx Nothing Nothing btc (outsideTx ctx)) `dbShouldBeE` Nothing

    it "leaves the signing data of a transaction that involves no portfolio as it is" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        (snd <$> transactionPool ctx Nothing Nothing btc (outsideTx ctx)) `dbShouldBeE` outsideTx ctx

    it "refuses a portfolio when it involves no portfolio" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift $
          dbShouldFailWith "The transaction does not involve portfolio0 of wallet0" $
            transactionPool ctx (Just (onePortfolio portfolio)) (Just btc) btc (outsideTx ctx)

    it "is none in a database without a wallet" $
      inDatabaseE $
        found (transactionPool ctx Nothing (Just btcTest) btc (outsideTx ctx)) `dbShouldBeE` Nothing

    -- Every test network has the coin type 1: the key that derives the
    -- address the transaction spends is that of the portfolio on each
    it "is on the default network when the file names none and the test networks share its keys" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        found (transactionPool ctx Nothing Nothing btcTest4 (testnetSpend ctx)) `dbShouldBeE` Just (portfolioPool portfolio btcTest4)

    it "is on the network of the file, or of --network, whatever the default network" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        found (transactionPool ctx Nothing (Just btcRegTest) btcTest4 (testnetSpend ctx)) `dbShouldBeE` Just (portfolioPool portfolio btcRegTest)

    it "is refused when the file names no network, the test networks share its keys, and the default network is none of them" $
      inDatabaseE $ do
        _ <- testPortfolio ctx
        lift $
          dbShouldFailWith "The transaction names no network, and the test networks share their keys: it could be on btctest, btctest4, btcreg, bchtest, bchtest4 and bchreg. Set the network to read it on with setdefault --network" $
            transactionPool ctx Nothing Nothing btc (testnetSpend ctx)

-- A transfer between two portfolios is kept and signed in the one that
-- pays it, whatever --portfolio names
payerSpec :: Ctx -> Spec
payerSpec ctx =
  describe "The portfolio that keeps and signs a transaction" $ do
    let cfg = def :: Config
    it "is not the portfolio a transfer pays: that one is refused, and the one that pays is named" $
      inDatabaseE $ do
        (_, second, transfer) <- transferToPortfolio1 ctx cfg
        lift $
          dbShouldFailWith "The transaction spends from portfolio0 of wallet0, the portfolio that pays it: it is kept pending there, and signed there" $
            refusePaidElsewhere ctx (portfolioPool second btc) transfer

    it "is the portfolio that pays a transfer" $
      inDatabaseE $ do
        (payer, _, transfer) <- transferToPortfolio1 ctx cfg
        refusePaidElsewhere ctx payer transfer `dbShouldBeE` ()

    it "is the portfolio a transaction pays, when no portfolio of the wallet pays it" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        _ <- testNewAcc2 ctx
        fundAccount ctx cfg accId
        pool <- poolOf accId
        refusePaidElsewhere ctx pool (incoming ctx) `dbShouldBeE` ()
