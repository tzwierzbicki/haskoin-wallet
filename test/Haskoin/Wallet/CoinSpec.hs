{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The coins of an account: the unspent outputs the store lists at its
-- addresses, kept in step by the sync, listed by coins with their
-- confirmations, locks and whether they can be spent, and offered to the
-- transaction builder when they can: confirmed enough, held by no pending
-- transaction of their network, a coinbase matured, an unconfirmed coin
-- the account's own change. The balances the account and its addresses
-- can spend add up those coins, and so does the balance of the coins at
-- each address that balances lists. The account of these tests is account
-- 0 of the test wallet on Bitcoin.
module Haskoin.Wallet.CoinSpec (spec) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson (eitherDecode, (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Address (AddrType (..), CoinBalance (..), addrPath, extendChainTo, pathText)
import Haskoin.Wallet.BestBlock (updateBest)
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Database
import Haskoin.Wallet.RawTx (insertRawTx)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo (repsertSummary, storeSummary, storeTime)
import Data.Word (Word32, Word8)
import Numeric.Natural (Natural)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), MarshalJsonBox (..), testIdentity)
import Data.Default (def)
import Haskoin.Wallet.Arbitrary (arbitraryCoinLock, arbitraryJsonCoin)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx -> do
  refreshingSpec ctx
  listingSpec ctx
  spendableSpec ctx
  balancesSpec ctx
  coinBalanceSpec ctx
  userLocksSpec ctx
  lifeSpec ctx
  rowSpec ctx
  holdersSpec ctx
  jsonSpec ctx
  leftSpec ctx
  chosenSpec ctx
  -- A coin and its lock read back as they were written
  testIdentity
    def
      { jsonTests = [JsonBox arbitraryCoinLock],
        marshalJsonTests = [MarshalJsonBox ((,) btc <$> arbitraryJsonCoin btc)]
      }

{- Fixtures -}

-- | The account of these tests
account :: Ctx -> ExceptT String (DB IO) DBAccountId
account ctx = fst <$> testNewAcc ctx "main"

-- | The addresses of the account these tests use, with their derivation
-- paths: the first two receiving addresses and the first two change
-- addresses
accountPaths :: Map Address SoftPath
accountPaths =
  Map.fromList
    [ (externalAddr 0, Deriv :/ 0 :/ 0),
      (externalAddr 1, Deriv :/ 0 :/ 1),
      (internalAddr 0, Deriv :/ 1 :/ 0),
      (internalAddr 1, Deriv :/ 1 :/ 1)
    ]

-- | 50000 paid to the first receiving address by transaction 1, waiting in
-- the mempool
payment :: Ctx -> Store.Unspent
payment ctx = storeCoin ctx (fakeTxHash 1, 0) Nothing (externalAddr 0) 50000

-- | The same coin, confirmed at height 800000
confirmedPayment :: Ctx -> Store.Unspent
confirmedPayment ctx = storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000

-- | 20000 paid to the second receiving address by transaction 2, at height
-- 800001
secondPayment :: Ctx -> Store.Unspent
secondPayment ctx = storeCoin ctx (fakeTxHash 2, 0) (Just 800001) (externalAddr 1) 20000

-- | 30000 paid to the second receiving address by transaction 3, waiting in
-- the mempool
thirdPayment :: Ctx -> Store.Unspent
thirdPayment ctx = storeCoin ctx (fakeTxHash 3, 0) Nothing (externalAddr 1) 30000

-- | 1000 at an output whose script pays no address, a bare script that
-- anyone can spend: the store lists it without an address
unaddressed :: Store.Unspent
unaddressed =
  Store.Unspent
    { Store.block = Store.MemRef 0,
      Store.outpoint = OutPoint (fakeTxHash 7) 0,
      Store.value = 1000,
      Store.script = BS.pack [0x51],
      Store.address = Nothing
    }

-- | The outpoints of the three payments
firstCoin, secondCoin, thirdCoin :: OutPoint
firstCoin = OutPoint (fakeTxHash 1) 0
secondCoin = OutPoint (fakeTxHash 2) 0
thirdCoin = OutPoint (fakeTxHash 3) 0

-- | The account with the confirmed payment at 800000, the second payment at
-- 800001 and the third one in the mempool
threeCoins :: Ctx -> DBAccountId -> ExceptT String (DB IO) ()
threeCoins ctx accId = void $ refresh ctx accId [confirmedPayment ctx, secondPayment ctx, thirdPayment ctx]

-- | Refresh the coins of the addresses of these tests with what the store
-- lists, as the sync does when their balances changed
refresh :: Ctx -> DBAccountId -> [Store.Unspent] -> ExceptT String (DB IO) Refreshed
refresh ctx accId = refreshCoins btc ctx accId (Map.keys accountPaths)

-- | What a refresh changed: how many coins, and the outpoints of those it
-- inserted and of those it deleted
changes :: Refreshed -> (Int, [OutPoint], [OutPoint])
changes refreshed = (refreshedCount refreshed, sort (rowOutpoint <$> insertedCoins refreshed), sort (rowOutpoint <$> deletedCoins refreshed))

rowOutpoint :: DBCoin -> OutPoint
rowOutpoint = forceRight . coinOutpoint

-- | The coin rows stored at an address
storedAt :: Text -> ExceptT String (DB IO) [DBCoin]
storedAt addr = lift $ map P.entityVal <$> P.selectList [DBCoinAddress P.==. addr] []

-- | The coins of the account as coins lists them: outpoint, confirmations
-- and lock
listed :: DBAccountId -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> ExceptT String (DB IO) [(OutPoint, Natural, Maybe CoinLock)]
listed accId afterM countM = map (\c -> (jsonCoinOutpoint c, jsonCoinConfirmations c, jsonCoinLock c)) <$> coinsAfter btc accId AllCoins afterM countM

-- | The coins of the account as coins lists them, and whether each can be
-- spent now
spendableFlags :: DBAccountId -> ExceptT String (DB IO) [(OutPoint, Bool)]
spendableFlags accId = map (\c -> (jsonCoinOutpoint c, jsonCoinSpendable c)) <$> coinsAfter btc accId AllCoins Nothing Nothing

-- | The outpoints of the coins offered to the builder at a number of
-- confirmations
spendable :: Ctx -> DBAccountId -> Natural -> ExceptT String (DB IO) [OutPoint]
spendable ctx accId minConf = sort . map (.outpoint) <$> spendableCoins btc ctx accId minConf

-- | The outpoints of the coins the builder may not spend at a number of
-- confirmations, with their locks
unspendable :: Ctx -> DBAccountId -> Natural -> ExceptT String (DB IO) [(OutPoint, Maybe CoinLock)]
unspendable ctx accId minConf = sortOn fst . map (\(coin, lock) -> (coin.outpoint, lock)) <$> unspendableCoins btc ctx accId minConf

-- | The best block of Bitcoin at a height
bestAt :: BlockHeight -> ExceptT String (DB IO) ()
bestAt height = lift $ updateBest btc (fakeBlockHash 1) height

-- | A pending transaction of an account spending the first payment
spendingFirst :: Ctx -> DBAccountId -> ExceptT String (DB IO) ()
spendingFirst ctx accId = lift $ P.insert_ $ pendingRow accId (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 49000)])

-- | A pending transaction of a Bitcoin Cash account spending the outpoint of
-- the first payment: from before the fork, the same transaction would have
-- the same id on both chains, but its coin on one is not its coin on the
-- other
spendingFirstOnCash :: Ctx -> ExceptT String (DB IO) ()
spendingFirstOnCash ctx = do
  (cashId, _) <- testNewAccBch ctx "cash"
  lift $ P.insert_ $ pendingRow cashId (spendingTx ctx [(fakeTxHash 1, 0)] [(bchExternalAddr, 49000)])

-- | The row of the confirmed payment as a refresh writes it, for the tests
-- that spoil one of its columns
paymentRow :: DBAccountId -> DBCoin
paymentRow accId =
  DBCoin accId (outpointBytes firstCoin) "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 50000 (S.encode (Store.BlockRef 800000 0)) False fixtureTime

-- | The confirmed payment, its script written in other bytes: the
-- 20-byte hash of the first receiving address pushed with OP_PUSHDATA1
-- (4c 14) instead of the one-byte push (14). The script means the same and
-- reads as the same address, but it is not standard: only a miner can put
-- it in a block, and a signature over the standard script does not spend it.
oddlyWrittenPayment :: Store.Unspent
oddlyWrittenPayment =
  Store.Unspent
    { Store.block = Store.BlockRef 800000 0,
      Store.outpoint = firstCoin,
      Store.value = 50000,
      Store.script = fromJust (decodeHex "76a94c14c80d55223e2f1061363add2a7b0363c285bf298d88ac"),
      Store.address = Just (externalAddr 0)
    }

{- Transactions behind the coins -}

-- | Store the summary of a transaction of the store, as the sync does
summarise :: DBAccountId -> Store.Transaction -> ExceptT String (DB IO) ()
summarise accId tx = void $ lift $ repsertSummary btc accId tx.txid tx.block (storeTime tx.timestamp) (storeSummary btc accountPaths tx)

-- | Transaction 1 confirmed at 800000: someone else pays the first payment
firstPaymentTx :: Ctx -> Store.Transaction
firstPaymentTx ctx = spendingStoreTx ctx (fakeTxHash 1) (Store.BlockRef 800000 0) (fakeTxHash 21, 0) (otherAddr 1) [(externalAddr 0, 50000)]

-- | Transaction 3 in the mempool: someone else pays the third payment
thirdPaymentTx :: Ctx -> Store.Transaction
thirdPaymentTx ctx = spendingStoreTx ctx (fakeTxHash 3) (Store.MemRef 0) (fakeTxHash 23, 0) (otherAddr 1) [(externalAddr 1, 30000)]

-- | Transaction 8 in the mempool: the account pays 30000 from the confirmed
-- first payment, with 19000 of change at the first change address
ownPaymentTx :: Ctx -> Store.Transaction
ownPaymentTx ctx = spendingStoreTx ctx (fakeTxHash 8) (Store.MemRef 0) (fakeTxHash 1, 0) (externalAddr 0) [(otherAddr 0, 30000), (internalAddr 0, 19000)]

-- | The change of transaction 8
ownChange :: Ctx -> Store.Unspent
ownChange ctx = storeCoin ctx (fakeTxHash 8, 1) Nothing (internalAddr 0) 19000

-- | Transaction 9 in the mempool: the account pays 20000 from the
-- unconfirmed third payment, with 9000 of change at the second change
-- address
paymentOnThirdTx :: Ctx -> Store.Transaction
paymentOnThirdTx ctx = spendingStoreTx ctx (fakeTxHash 9) (Store.MemRef 0) (fakeTxHash 3, 0) (externalAddr 1) [(otherAddr 0, 20000), (internalAddr 1, 9000)]

-- | The change of transaction 9
changeOnThird :: Ctx -> Store.Unspent
changeOnThird ctx = storeCoin ctx (fakeTxHash 9, 1) Nothing (internalAddr 1) 9000

-- | The coinbase of block 800000, paying the reward of 312500000 to the
-- first receiving address. Its single input spends nothing and carries the
-- height of the block, as BIP34 asks.
coinbase :: Ctx -> Tx
coinbase ctx =
  Tx 1 [TxIn nullOutPoint (BS.pack [0x03, 0x00, 0x35, 0x0c]) maxBound] [TxOut 312500000 (addressScript ctx (externalAddr 0))] [] 0

{- Refreshing -}

refreshingSpec :: Ctx -> Spec
refreshingSpec ctx =
  describe "Refreshing the coins of addresses, as the sync does" $ do
    it "stores a new coin with its account, address, value and block" $
      inDatabaseE $ do
        accId <- account ctx
        (changes <$> refresh ctx accId [payment ctx]) `dbShouldBeE` (1, [firstCoin], [])
        rows <- storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        liftIO $ do
          map (\c -> (dBCoinAccount c, dBCoinValue c, S.decode (dBCoinBlockRef c))) rows `shouldBe` [(accId, 50000, Right (Store.MemRef 0))]

    it "changes nothing when the store lists the same coins again" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [payment ctx]
        (changes <$> refresh ctx accId [payment ctx]) `dbShouldBeE` (0, [], [])

    it "stores once a coin the store lists twice" $
      inDatabaseE $ do
        accId <- account ctx
        (changes <$> refresh ctx accId [payment ctx, payment ctx]) `dbShouldBeE` (1, [firstCoin], [])
        (map rowOutpoint <$> storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") `dbShouldBeE` [firstCoin]

    it "moves a coin to the block that confirms it, as the store reports it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [payment ctx]
        (changes <$> refresh ctx accId [confirmedPayment ctx]) `dbShouldBeE` (1, [], [])
        rows <- storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        liftIO $ map (S.decode . dBCoinBlockRef) rows `shouldBe` [Right (Store.BlockRef 800000 0)]
        spendableCoins btc ctx accId 0 `dbShouldBeE` [confirmedPayment ctx]

    it "deletes a coin the store no longer lists, as once it is spent" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [confirmedPayment ctx]
        (changes <$> refresh ctx accId []) `dbShouldBeE` (1, [], [firstCoin])
        storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `dbShouldBeE` []

    it "keeps the coins of one address apart, deleting only the one that went" $
      inDatabaseE $ do
        accId <- account ctx
        (changes <$> refresh ctx accId [secondPayment ctx, thirdPayment ctx]) `dbShouldBeE` (2, [secondCoin, thirdCoin], [])
        (changes <$> refresh ctx accId [thirdPayment ctx]) `dbShouldBeE` (1, [], [secondCoin])
        (map rowOutpoint <$> storedAt "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg") `dbShouldBeE` [thirdCoin]

    it "leaves alone the addresses it is not given, whatever the store lists for them" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [secondPayment ctx]
        -- The second address is not refreshed: its coin stays, and the new
        -- one the store lists there is not stored
        (changes <$> refreshCoins btc ctx accId [externalAddr 0] [payment ctx, thirdPayment ctx]) `dbShouldBeE` (1, [firstCoin], [])
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [firstCoin, secondCoin]

    it "passes over a coin the store lists without an address" $
      inDatabaseE $ do
        accId <- account ctx
        (changes <$> refresh ctx accId [unaddressed]) `dbShouldBeE` (0, [], [])

    it "passes over a coin whose script is not the standard script of its address" $
      inDatabaseE $ do
        accId <- account ctx
        (changes <$> refresh ctx accId [oddlyWrittenPayment]) `dbShouldBeE` (0, [], [])

    it "refuses an address its network cannot write" $
      inDatabaseE $ do
        (accId, _) <- testNewAccBch ctx "cash"
        lift $ dbShouldFailWith "The coins of an address of another network cannot be refreshed" $ refreshCoins bch ctx accId [segwitAddr] []

{- Listing -}

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "Listing the coins of an account, as coins does" $ do
    it "lists the unconfirmed coins first, then the confirmed ones from the newest block" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [thirdCoin, secondCoin, firstCoin]

    it "lists a page at a time, each page after the last coin of the one before" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        (map fst3 <$> listed accId Nothing (Just 1)) `dbShouldBeE` [thirdCoin]
        (map fst3 <$> listed accId (Just (Store.MemRef 0, thirdCoin)) (Just 1)) `dbShouldBeE` [secondCoin]
        (map fst3 <$> listed accId (Just (Store.BlockRef 800001 0, secondCoin)) (Just 2)) `dbShouldBeE` [firstCoin]
        listed accId (Just (Store.BlockRef 800000 0, firstCoin)) Nothing `dbShouldBeE` []

    it "lists the outputs of one transaction by their index, so that no page repeats one" $
      inDatabaseE $ do
        accId <- account ctx
        let output i = storeCoin ctx (fakeTxHash 4, i) (Just 800000) (externalAddr 0) 10000
        _ <- refresh ctx accId [output 2, output 0, output 1]
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [OutPoint (fakeTxHash 4) 0, OutPoint (fakeTxHash 4) 1, OutPoint (fakeTxHash 4) 2]
        (map fst3 <$> listed accId (Just (Store.BlockRef 800000 0, OutPoint (fakeTxHash 4) 0)) (Just 1)) `dbShouldBeE` [OutPoint (fakeTxHash 4) 1]

    it "names the block of a coin of the account, for --after, and refuses one the account does not have" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        coinBlock accId thirdCoin `dbShouldBeE` Store.MemRef 0
        coinBlock accId secondCoin `dbShouldBeE` Store.BlockRef 800001 0
        lift $
          dbShouldFailWith "The coin 0000000000000000000000000000000000000000000000000000000000000009:0 is not in the account" $
            coinBlock accId (OutPoint (fakeTxHash 9) 0)

    it "counts the confirmations from the best block, and none in the mempool" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        bestAt 800002
        listed accId Nothing Nothing `dbShouldBeE` [(thirdCoin, 0, Nothing), (secondCoin, 2, Nothing), (firstCoin, 3, Nothing)]

    it "counts one confirmation for a confirmed coin while the best block is unknown or behind it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [confirmedPayment ctx]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]
        bestAt 799990
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]

    it "counts from the best block of the account's network, not another's" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [confirmedPayment ctx]
        lift $ updateBest bch (fakeBlockHash 2) 900000
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]

    it "shows a coin a pending transaction spends as locked, and the others as free" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        (map (\(o, _, l) -> (o, l)) <$> listed accId Nothing Nothing) `dbShouldBeE` [(thirdCoin, Nothing), (secondCoin, Nothing), (firstCoin, Just LockedByTransaction)]

    it "shows as locked a coin a pending transaction of another account spends" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        threeCoins ctx accId
        spendingFirst ctx otherId
        (map (\(o, _, l) -> (o, l)) <$> listed accId Nothing Nothing) `dbShouldBeE` [(thirdCoin, Nothing), (secondCoin, Nothing), (firstCoin, Just LockedByTransaction)]

    it "leaves free a coin whose outpoint a pending transaction of the other chain spends" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirstOnCash ctx
        (map (\(o, _, l) -> (o, l)) <$> listed accId Nothing Nothing) `dbShouldBeE` [(thirdCoin, Nothing), (secondCoin, Nothing), (firstCoin, Nothing)]

    it "marks a confirmed coin as spendable, and one a pending transaction holds as not" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [confirmedPayment ctx, secondPayment ctx]
        spendingFirst ctx accId
        spendableFlags accId `dbShouldBeE` [(secondCoin, True), (firstCoin, False)]

    it "marks an unconfirmed coin as spendable only when it is the account's own change" $
      inDatabaseE $ do
        accId <- account ctx
        summarise accId (thirdPaymentTx ctx)
        summarise accId (firstPaymentTx ctx)
        summarise accId (ownPaymentTx ctx)
        _ <- refresh ctx accId [thirdPayment ctx, ownChange ctx]
        (sort <$> spendableFlags accId) `dbShouldBeE` [(thirdCoin, False), (OutPoint (fakeTxHash 8) 1, True)]

    it "marks a coinbase as spendable once it has matured" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ insertRawTx (coinbase ctx)
        _ <- refresh ctx accId [storeCoin ctx (txHash (coinbase ctx), 0) (Just 800000) (externalAddr 0) 312500000]
        bestAt 800098
        spendableFlags accId `dbShouldBeE` [(OutPoint (txHash (coinbase ctx)) 0, False)]
        bestAt 800099
        spendableFlags accId `dbShouldBeE` [(OutPoint (txHash (coinbase ctx)) 0, True)]

    it "shows a coin the user locked as locked by the user" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        _ <- lockCoin secondCoin
        (map (\(o, _, l) -> (o, l)) <$> listed accId Nothing Nothing)
          `dbShouldBeE` [(thirdCoin, Nothing), (secondCoin, Just LockedByUser), (firstCoin, Nothing)]

    it "lists the locked coins only, of both kinds, when asked" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        _ <- lockCoin secondCoin
        (map (\c -> (jsonCoinOutpoint c, jsonCoinLock c)) <$> coinsAfter btc accId LockedCoins Nothing Nothing)
          `dbShouldBeE` [(secondCoin, Just LockedByUser), (firstCoin, Just LockedByTransaction)]

    it "lists the coins of the account only" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- refresh ctx accId [confirmedPayment ctx]
        _ <- refreshCoins btc ctx otherId [externalAddr2] [storeCoin ctx (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [firstCoin]
        (map fst3 <$> listed otherId Nothing Nothing) `dbShouldBeE` [OutPoint (fakeTxHash 4) 0]

    it "reads the address of a Bitcoin Cash coin on its network" $
      inDatabaseE $ do
        (accId, _) <- testNewAccBch ctx "cash"
        _ <- refreshCoins bch ctx accId [bchExternalAddr] [storeCoin ctx (fakeTxHash 5, 0) (Just 800000) bchExternalAddr 10000]
        (map jsonCoinAddress <$> coinsAfter bch accId AllCoins Nothing Nothing) `dbShouldBeE` [bchExternalAddr]

    it "refuses a stored coin whose address does not read, rather than leaving it out" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ P.insert_ (paymentRow accId) {dBCoinAddress = "nonsense"}
        lift $
          dbShouldFailWith "The stored coin 0000000000000000000000000000000000000000000000000000000000000001:0 has an address that does not read: nonsense" $
            coinsAfter btc accId AllCoins Nothing Nothing

    it "refuses a stored coin whose block does not read" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ P.insert_ (paymentRow accId) {dBCoinBlockRef = "nonsense"}
        lift $
          dbShouldFailWith "The stored coin 0000000000000000000000000000000000000000000000000000000000000001:0 has a block that does not read" $
            coinsAfter btc accId AllCoins Nothing Nothing

{- Spendable -}

spendableSpec :: Ctx -> Spec
spendableSpec ctx =
  describe "The coins offered to the transaction builder" $ do
    it "offers the coins with at least the confirmations asked" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        -- 800002 is the third block for the first payment, the second for
        -- the second payment
        bestAt 800002
        spendable ctx accId 1 `dbShouldBeE` [firstCoin, secondCoin]
        spendable ctx accId 2 `dbShouldBeE` [firstCoin, secondCoin]
        spendable ctx accId 3 `dbShouldBeE` [firstCoin]
        spendable ctx accId 4 `dbShouldBeE` []

    it "counts one confirmation for a confirmed coin while the best block is unknown" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendable ctx accId 1 `dbShouldBeE` [firstCoin, secondCoin]
        spendable ctx accId 2 `dbShouldBeE` []

    it "holds back a coin a pending transaction spends" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        spendable ctx accId 1 `dbShouldBeE` [secondCoin]

    it "holds back a coin a pending transaction of another account spends" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        threeCoins ctx accId
        spendingFirst ctx otherId
        spendable ctx accId 1 `dbShouldBeE` [secondCoin]

    it "offers a coin whose outpoint a pending transaction of the other chain spends" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirstOnCash ctx
        spendable ctx accId 1 `dbShouldBeE` [firstCoin, secondCoin]

    it "holds back a coin the user locked" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        _ <- lockCoin firstCoin
        spendable ctx accId 1 `dbShouldBeE` [secondCoin]

    it "holds back a coinbase until its 100th confirmation" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ insertRawTx (coinbase ctx)
        _ <- refresh ctx accId [storeCoin ctx (txHash (coinbase ctx), 0) (Just 800000) (externalAddr 0) 312500000]
        bestAt 800098
        spendable ctx accId 1 `dbShouldBeE` []
        bestAt 800099
        spendable ctx accId 1 `dbShouldBeE` [OutPoint (txHash (coinbase ctx)) 0]

    it "holds back an unconfirmed coin someone else pays, even at zero confirmations: they can still replace it" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        summarise accId (thirdPaymentTx ctx)
        spendable ctx accId 0 `dbShouldBeE` [firstCoin, secondCoin]

    it "holds back an unconfirmed coin whose transaction the sync has not stored" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [thirdPayment ctx]
        spendable ctx accId 0 `dbShouldBeE` []

    it "offers at zero confirmations the unconfirmed change of the account's own payment" $
      inDatabaseE $ do
        accId <- account ctx
        summarise accId (firstPaymentTx ctx)
        summarise accId (ownPaymentTx ctx)
        _ <- refresh ctx accId [ownChange ctx]
        spendable ctx accId 0 `dbShouldBeE` [OutPoint (fakeTxHash 8) 1]
        spendable ctx accId 1 `dbShouldBeE` []

    it "holds back the change of a payment that spends an unconfirmed coin someone else paid" $
      inDatabaseE $ do
        accId <- account ctx
        summarise accId (thirdPaymentTx ctx)
        summarise accId (paymentOnThirdTx ctx)
        _ <- refresh ctx accId [changeOnThird ctx]
        spendable ctx accId 0 `dbShouldBeE` []

    it "offers each coin as the store last reported it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [secondPayment ctx]
        spendableCoins btc ctx accId 1 `dbShouldBeE` [secondPayment ctx]

    it "offers the coins of the account only" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- refresh ctx accId [confirmedPayment ctx]
        _ <- refreshCoins btc ctx otherId [externalAddr2] [storeCoin ctx (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        spendable ctx accId 0 `dbShouldBeE` [firstCoin]
        spendable ctx otherId 0 `dbShouldBeE` [OutPoint (fakeTxHash 4) 0]

    it "refuses a stored coin whose address does not read, rather than leaving it out" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ P.insert_ (paymentRow accId) {dBCoinAddress = "nonsense"}
        lift $
          dbShouldFailWith "The stored coin 0000000000000000000000000000000000000000000000000000000000000001:0 has an address that does not read: nonsense" $
            spendableCoins btc ctx accId 1

    -- A pending transaction spends the first coin, the user locked the
    -- second, and the third is in the mempool, paid by someone else
    it "holds back the others, each with its lock, which the third coin has none of" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        _ <- lockCoin secondCoin
        unspendable ctx accId 1 `dbShouldBeE` [(firstCoin, Just LockedByTransaction), (secondCoin, Just LockedByUser), (thirdCoin, Nothing)]

    -- 800002 is the third block for the first payment, the second for the
    -- second payment
    it "holds back the coins below the confirmations asked, without a lock" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        bestAt 800002
        unspendable ctx accId 3 `dbShouldBeE` [(secondCoin, Nothing), (thirdCoin, Nothing)]

{- Balances -}

balancesSpec :: Ctx -> Spec
balancesSpec ctx =
  describe "The balances the account and its addresses can spend now" $ do
    it "add up the coins that can be spent, by address" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        summarise accId (thirdPaymentTx ctx)
        -- The third payment, unconfirmed and someone else's, is left out
        spendableAtAddresses btc accId ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
          `dbShouldBeE` Map.fromList [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", 50000), ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 20000)]
        spendableBalance btc accId `dbShouldBeE` 70000

    it "are read for the addresses asked for only" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendableAtAddresses btc accId ["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"] `dbShouldBeE` Map.fromList [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 20000)]
        spendableAtAddresses btc accId [] `dbShouldBeE` Map.empty

    it "leave out a coin a pending transaction holds" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        spendableAtAddresses btc accId ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
          `dbShouldBeE` Map.fromList [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 20000)]
        spendableBalance btc accId `dbShouldBeE` 20000

    it "leave out a coin the user locked" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        _ <- lockCoin secondCoin
        spendableBalance btc accId `dbShouldBeE` 50000

    it "count the account's own unconfirmed change" $
      inDatabaseE $ do
        accId <- account ctx
        summarise accId (firstPaymentTx ctx)
        summarise accId (ownPaymentTx ctx)
        _ <- refresh ctx accId [ownChange ctx]
        spendableAtAddresses btc accId ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"] `dbShouldBeE` Map.fromList [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", 19000)]
        spendableBalance btc accId `dbShouldBeE` 19000

    it "count a coinbase once it has matured" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ insertRawTx (coinbase ctx)
        _ <- refresh ctx accId [storeCoin ctx (txHash (coinbase ctx), 0) (Just 800000) (externalAddr 0) 312500000]
        bestAt 800098
        spendableBalance btc accId `dbShouldBeE` 0
        bestAt 800099
        spendableBalance btc accId `dbShouldBeE` 312500000

    it "are those of the account only" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- refresh ctx accId [confirmedPayment ctx]
        _ <- refreshCoins btc ctx otherId [externalAddr2] [storeCoin ctx (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        spendableBalance btc accId `dbShouldBeE` 50000
        spendableBalance btc otherId `dbShouldBeE` 10000

{- The balance of the coins at each address -}

-- | The account of these tests with the addresses of 'accountPaths' stored,
-- as the sync stores them before their coins
accountWithAddresses :: Ctx -> ExceptT String (DB IO) DBAccountId
accountWithAddresses ctx = do
  accId <- account ctx
  extendChainTo ctx accId AddrExternal 2
  extendChainTo ctx accId AddrInternal 2
  return accId

-- | A page of the addresses of the account that hold coins, as balances
-- lists them: each by its derivation path, with the balance of its coins
balancesListed :: DBAccountId -> Maybe (DBAddress, CoinBalance) -> Maybe Natural -> ExceptT String (DB IO) [(Text, CoinBalance)]
balancesListed accId afterM countM = map (\(addr, balance) -> (pathText (addrPath addr), balance)) <$> coinBalancesAfter btc accId afterM countM

-- | A confirmed coin of a value at an address, paid by a transaction
paidAt :: Ctx -> Word8 -> Address -> Natural -> Store.Unspent
paidAt ctx tx addr = storeCoin ctx (fakeTxHash tx, 0) (Just 800000) addr

coinBalanceSpec :: Ctx -> Spec
coinBalanceSpec ctx =
  describe "The balance of the coins at each address, as balances lists it" $ do
    it "shows an address with two coins once, with the value of both" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        _ <- refresh ctx accId [secondPayment ctx, paidAt ctx 4 (externalAddr 1) 15000]
        -- 20000 + 15000, both confirmed
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/1", CoinBalance 35000 35000)]

    -- The third payment is unconfirmed and someone else's
    it "counts a coin that cannot be spent yet in the total, not as spendable" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        _ <- refresh ctx accId [secondPayment ctx, thirdPayment ctx]
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/1", CoinBalance 50000 20000)]

    it "counts the coins a pending transaction or the user locks in the total, not as spendable" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        _ <- refresh ctx accId [confirmedPayment ctx, secondPayment ctx]
        spendingFirst ctx accId
        _ <- lockCoin secondCoin
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 50000 0), ("/0/1", CoinBalance 20000 0)]

    it "counts a coinbase in the total, and as spendable once it has matured" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        lift $ insertRawTx (coinbase ctx)
        _ <- refresh ctx accId [storeCoin ctx (txHash (coinbase ctx), 0) (Just 800000) (externalAddr 0) 312500000]
        bestAt 800098
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 312500000 0)]
        bestAt 800099
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 312500000 312500000)]

    it "leaves out an address whose coins are all spent" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        _ <- refresh ctx accId [confirmedPayment ctx, secondPayment ctx]
        -- The store no longer lists the first payment
        _ <- refresh ctx accId [secondPayment ctx]
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/1", CoinBalance 20000 20000)]

    it "lists the largest total first, change addresses among the receiving ones" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        _ <- refresh ctx accId [confirmedPayment ctx, secondPayment ctx, paidAt ctx 4 (internalAddr 0) 90000]
        balancesListed accId Nothing Nothing
          `dbShouldBeE` [("/1/0", CoinBalance 90000 90000), ("/0/0", CoinBalance 50000 50000), ("/0/1", CoinBalance 20000 20000)]

    it "lists the addresses with the same total in the order of their paths, receiving addresses first" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        _ <- refresh ctx accId [paidAt ctx 4 (internalAddr 0) 20000, paidAt ctx 5 (externalAddr 1) 20000, paidAt ctx 6 (externalAddr 0) 20000]
        balancesListed accId Nothing Nothing
          `dbShouldBeE` [("/0/0", CoinBalance 20000 20000), ("/0/1", CoinBalance 20000 20000), ("/1/0", CoinBalance 20000 20000)]

    it "lists the addresses after a given one, with its balance, when asked" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        _ <- refresh ctx accId [confirmedPayment ctx, secondPayment ctx, paidAt ctx 4 (internalAddr 0) 90000]
        -- After the largest, 90000 at /1/0: 50000 and 20000
        largest <- coinBalancesAt btc accId ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"]
        (map fst <$> balancesListed accId (listToMaybe largest) Nothing) `dbShouldBeE` ["/0/0", "/0/1"]
        balancesListed accId (listToMaybe largest) (Just 1) `dbShouldBeE` [("/0/0", CoinBalance 50000 50000)]

    it "lists the addresses with the same total after a given one by their paths" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        _ <- refresh ctx accId [paidAt ctx 4 (internalAddr 0) 20000, paidAt ctx 5 (externalAddr 1) 20000, paidAt ctx 6 (externalAddr 0) 20000]
        -- After /0/1: the change address /1/0, with the same total
        second <- coinBalancesAt btc accId ["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
        (map fst <$> balancesListed accId (listToMaybe second) Nothing) `dbShouldBeE` ["/1/0"]

    it "are those of the account only" $
      inDatabaseE $ do
        accId <- accountWithAddresses ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        extendChainTo ctx otherId AddrExternal 1
        _ <- refresh ctx accId [confirmedPayment ctx]
        _ <- refreshCoins btc ctx otherId [externalAddr2] [storeCoin ctx (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 50000 50000)]
        balancesListed otherId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 10000 10000)]

{- Locks of the user -}

userLocksSpec :: Ctx -> Spec
userLocksSpec ctx =
  describe "The coins the user locks, as lockcoin and unlockcoin do" $ do
    it "locks a free coin, which shows locked by the user and not spendable" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        (lockState <$> lockCoin secondCoin) `dbShouldBeE` ("main", Just LockedByUser, False)

    it "refuses a coin the wallet does not have" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        lift $
          dbShouldFailWith "The wallet has no coin 0000000000000000000000000000000000000000000000000000000000000009:0" $
            lockCoin (OutPoint (fakeTxHash 9) 0)

    it "refuses a coin a pending transaction holds" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        lift $ dbShouldFailWith "The coin is already locked by a pending transaction" $ lockCoin firstCoin

    it "refuses a coin the user locked already" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        _ <- lockCoin secondCoin
        lift $ dbShouldFailWith "The coin is already locked" $ lockCoin secondCoin

    it "unlocks a coin the user locked, which can be spent again" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        _ <- lockCoin secondCoin
        (lockState <$> unlockCoin secondCoin) `dbShouldBeE` ("main", Nothing, True)
        spendable ctx accId 1 `dbShouldBeE` [firstCoin, secondCoin]

    it "refuses to unlock a coin that is not locked" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        lift $ dbShouldFailWith "The coin is not locked" $ unlockCoin secondCoin

    it "refuses to unlock a coin a pending transaction holds, which deletetx frees" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        lift $
          dbShouldFailWith "The coin is locked by a pending transaction, not by the user: deletetx frees it" $
            unlockCoin firstCoin

    it "keeps the lock while the coin confirms" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [payment ctx]
        _ <- lockCoin firstCoin
        _ <- refresh ctx accId [confirmedPayment ctx]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Just LockedByUser)]

    it "loses the lock with the coin once the store no longer lists it: a coin that comes back comes back free" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh ctx accId [confirmedPayment ctx]
        _ <- lockCoin firstCoin
        _ <- refresh ctx accId []
        _ <- refresh ctx accId [confirmedPayment ctx]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]

    it "locks a coin whose outpoint a pending transaction of the other chain spends" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirstOnCash ctx
        (lockState <$> lockCoin firstCoin) `dbShouldBeE` ("main", Just LockedByUser, False)
  where
    lockState (acc, coin) = (dBAccountName acc, jsonCoinLock coin, jsonCoinSpendable coin)

{- The life of a coin -}

lifeSpec :: Ctx -> Spec
lifeSpec ctx =
  describe "The life of a coin" $
    it "arrives unconfirmed, confirms, is held by a payment, and goes once spent" $
      inDatabaseE $ do
        accId <- account ctx
        -- A payment waits in the mempool: listed, but not offered, since
        -- whoever pays can still replace it
        bestAt 799999
        _ <- refresh ctx accId [payment ctx]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 0, Nothing)]
        spendable ctx accId 0 `dbShouldBeE` []
        -- It confirms in the next block
        bestAt 800000
        _ <- refresh ctx accId [confirmedPayment ctx]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]
        spendable ctx accId 1 `dbShouldBeE` [firstCoin]
        -- A payment of the wallet spends it: held, offered no more
        spendingFirst ctx accId
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Just LockedByTransaction)]
        spendable ctx accId 0 `dbShouldBeE` []
        -- The payment is mined: the store no longer lists the coin
        bestAt 800001
        (changes <$> refresh ctx accId []) `dbShouldBeE` (1, [], [firstCoin])
        listed accId Nothing Nothing `dbShouldBeE` []

{- A coin row -}

-- The first coin is the account's, at its first receiving address; 20000
-- at the first receiving address of the second test wallet is another
-- account's; the third coin is none of the wallet
holdersSpec :: Ctx -> Spec
holdersSpec ctx =
  describe "The accounts that hold coins" $
    it "are those of the coins the wallet holds, each once" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- refresh ctx accId [confirmedPayment ctx]
        _ <- refreshCoins btc ctx otherId [externalAddr2] [storeCoin ctx (fakeTxHash 2, 0) (Just 800001) externalAddr2 20000]
        (map fst <$> coinAccounts [firstCoin, secondCoin, firstCoin, thirdCoin]) `dbShouldBeE` [accId, otherId]

rowSpec :: Ctx -> Spec
rowSpec ctx =
  describe "A coin row" $ do
    it "shows as coins lists it: outpoint, address, value, block, confirmations from the best block, lock, and whether it can be spent" $
      inDatabaseE $ do
        accId <- account ctx
        refreshed <- refresh ctx accId [confirmedPayment ctx]
        row <- liftMaybe "The coin is not stored" $ listToMaybe (insertedCoins refreshed)
        let rules = Spending btc accId (Just 800002) Map.empty
            shown = JsonCoin firstCoin (externalAddr 0) 50000 (Store.BlockRef 800000 0) 3 Nothing True
        toJsonCoin rules row `dbShouldBeE` shown
        toJsonCoin rules {spendingLocks = Map.fromList [(firstCoin, LockedByTransaction)]} row
          `dbShouldBeE` shown {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}
        toJsonCoin rules {spendingLocks = Map.fromList [(firstCoin, LockedByUser)]} row
          `dbShouldBeE` shown {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False}
        toJsonCoin rules {spendingBest = Nothing} row `dbShouldBeE` shown {jsonCoinConfirmations = 1}

    it "keeps its outpoint, read back from its key" $
      inDatabaseE $ do
        accId <- account ctx
        refreshed <- refresh ctx accId [secondPayment ctx]
        liftIO $ map coinOutpoint (insertedCoins refreshed) `shouldBe` [Right secondCoin]

    it "refuses a key that does not read as an outpoint" $
      inDatabaseE $ do
        accId <- account ctx
        let row = (paymentRow accId) {dBCoinOutpoint = KeyBytes "nonsense"}
        liftIO $ coinOutpoint row `shouldBe` Left "A stored coin has an outpoint that does not read"

    it "is grouped with the others of its address, as the store lists them" $ do
      let grouped = Map.map (map (.outpoint) . Map.elems) (groupCoins btc ctx [payment ctx, secondPayment ctx, thirdPayment ctx, unaddressed])
      grouped
        `shouldBe` Map.fromList
          [ ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", [firstCoin]),
            ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", [secondCoin, thirdCoin])
          ]

    it "is left out of the groups when its script is not the standard script of its address" $
      Map.map (map (.outpoint) . Map.elems) (groupCoins btc ctx [oddlyWrittenPayment, secondPayment ctx])
        `shouldBe` Map.fromList [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", [secondCoin])]

    it "is left out of the groups when the network cannot write its address" $ do
      let segwit = storeCoin ctx (fakeTxHash 6, 0) Nothing segwitAddr 1000
          cash = storeCoin ctx (fakeTxHash 5, 0) Nothing bchExternalAddr 1000
      Map.map (map (.outpoint) . Map.elems) (groupCoins bch ctx [segwit, cash])
        `shouldBe` Map.fromList [(bchExternalAddrText, [OutPoint (fakeTxHash 5) 0])]

{- JSON -}

jsonSpec :: Ctx -> Spec
jsonSpec _ =
  describe "Coins in JSON" $ do
    it "writes a confirmed and spendable coin as coins prints it, and reads it back" $ do
      let coin = JsonCoin firstCoin (externalAddr 0) 50000 (Store.BlockRef 800000 0) 3 Nothing True
          json :: String
          json =
            [r|{
              "outpoint": {"txid": "0000000000000000000000000000000000000000000000000000000000000001", "index": 0},
              "address": "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
              "value": 50000,
              "block": {"height": 800000, "position": 0},
              "confirmations": 3,
              "lock": null,
              "spendable": true
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc coin)
      unmarshalJSON btc (cs json) `shouldBe` Just coin

    it "writes an unconfirmed and locked coin of Bitcoin Cash with its cash address" $ do
      let coin = JsonCoin (OutPoint (fakeTxHash 5) 1) bchExternalAddr 10000 (Store.MemRef 0) 0 (Just LockedByTransaction) False
          json :: String
          json =
            [r|{
              "outpoint": {"txid": "0000000000000000000000000000000000000000000000000000000000000005", "index": 1},
              "address": "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u",
              "value": 10000,
              "block": {"mempool": 0},
              "confirmations": 0,
              "lock": "transaction",
              "spendable": false
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue bch coin)
      unmarshalJSON bch (cs json) `shouldBe` Just coin

{- The coins a transaction leaves -}

-- | A confirmed coin of the test wallet: an output of a fake transaction, at
-- the external address of the same index
unspentAt :: Ctx -> Word8 -> Word32 -> Natural -> Store.Unspent
unspentAt ctx w i = storeCoin ctx (fakeTxHash w, i) (Just 800000) (externalAddr (fromIntegral i))

leftSpec :: Ctx -> Spec
leftSpec ctx =
  describe "The coins a transaction leaves in the account" $ do
    it "are none when it leaves none" $
      leftCoins [] [] [] `shouldBe` []

    -- The dust coins left, the coins the builder may not spend, and a coin
    -- not chosen with coin control
    it "are listed by reason, the largest coins first" $
      leftCoins
        [unspentAt ctx 2 0 900, unspentAt ctx 2 1 1200]
        [(unspentAt ctx 3 0 5000, Nothing), (unspentAt ctx 3 1 20000, Just LockedByTransaction), (unspentAt ctx 3 2 30000, Just LockedByUser)]
        [unspentAt ctx 4 0 40000]
        `shouldBe` [ LeftCoin (OutPoint (fakeTxHash 2) 1) 1200 WorthLessThanFee,
                     LeftCoin (OutPoint (fakeTxHash 2) 0) 900 WorthLessThanFee,
                     LeftCoin (OutPoint (fakeTxHash 3) 2) 30000 (Locked LockedByUser),
                     LeftCoin (OutPoint (fakeTxHash 3) 1) 20000 (Locked LockedByTransaction),
                     LeftCoin (OutPoint (fakeTxHash 3) 0) 5000 NotSpendableYet,
                     LeftCoin (OutPoint (fakeTxHash 4) 0) 40000 NotGiven
                   ]

    it "tell the user why each stays" $
      (leftReasonText <$> [WorthLessThanFee, Locked LockedByUser, Locked LockedByTransaction, NotSpendableYet, NotGiven])
        `shouldBe` ["worth less than its fee", "you locked it", "a pending transaction spends it", "not spendable yet", "not chosen with --coin or --spendfrom"]

    it "are written in JSON with their outpoint, their value and why they stay" $
      Json.toJSON (LeftCoin (OutPoint (fakeTxHash 2) 0) 900 WorthLessThanFee)
        `shouldBe` Json.object
          [ "outpoint" .= Json.object ["txid" .= ("0000000000000000000000000000000000000000000000000000000000000002" :: Text), "index" .= (0 :: Int)],
            "value" .= (900 :: Int),
            "reason" .= ("dust" :: Text)
          ]

    it "name each reason in JSON" $
      (Json.toJSON <$> [WorthLessThanFee, Locked LockedByUser, Locked LockedByTransaction, NotSpendableYet, NotGiven])
        `shouldBe` (Json.String <$> ["dust", "lockedbyuser", "lockedbytransaction", "notspendableyet", "notgiven"])

    it "read each reason back from its JSON name" $
      (Json.fromJSON . Json.String <$> ["dust", "lockedbyuser", "lockedbytransaction", "notspendableyet", "notgiven"])
        `shouldBe` (Json.Success <$> [WorthLessThanFee, Locked LockedByUser, Locked LockedByTransaction, NotSpendableYet, NotGiven])

    it "refuse a reason they do not know" $
      (Json.fromJSON (Json.String "lost") :: Json.Result LeftReason)
        `shouldBe` Json.Error "Unknown reason a coin stays in the account: lost"

{- Coin control -}

-- The builder may spend 50000 at /0/0 and 30000 at /0/1. It may not spend
-- 20000 at /0/0, which the user locked, nor 5000 at /0/1, not confirmed
-- enough.
chosenSpec :: Ctx -> Spec
chosenSpec ctx =
  describe "The coins that coin control chooses" $ do
    it "are the coins given by outpoint" $
      choose [OutPoint (fakeTxHash 1) 0] [] `shouldBe` Right ([fifty], [])

    it "are every coin at an address given, those the builder may not spend with their locks" $
      choose [] [externalAddr 0] `shouldBe` Right ([fifty], [(twentyLocked, Just LockedByUser)])

    it "are the coins of the outpoints and of the addresses together" $
      choose [OutPoint (fakeTxHash 1) 1] [externalAddr 0] `shouldBe` Right ([fifty, thirty], [(twentyLocked, Just LockedByUser)])

    it "keep a coin given by outpoint that the builder may not spend, with why" $
      choose [OutPoint (fakeTxHash 1) 0, OutPoint (fakeTxHash 2) 1] [] `shouldBe` Right ([fifty], [(fiveUnconfirmed, Nothing)])

    it "are refused an outpoint the account does not hold" $
      choose [OutPoint (fakeTxHash 9) 0] []
        `shouldBe` Left "The account has no coin 0000000000000000000000000000000000000000000000000000000000000009:0"

    it "are refused when none can be spent, with why" $
      choose [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 2) 1] []
        `shouldBe` Left "None of the coins chosen can be spent now: 0000000000000000000000000000000000000000000000000000000000000002:0, you locked it; 0000000000000000000000000000000000000000000000000000000000000002:1, not spendable yet"

    it "are refused when the addresses given hold no coins" $
      choose [] [externalAddr 5] `shouldBe` Left "The addresses chosen with --spendfrom hold no coins"
  where
    fifty = unspentAt ctx 1 0 50000
    thirty = unspentAt ctx 1 1 30000
    twentyLocked = unspentAt ctx 2 0 20000
    fiveUnconfirmed = unspentAt ctx 2 1 5000
    choose outpoints addrs = chosenCoins outpoints addrs [fifty, thirty] [(twentyLocked, Just LockedByUser), (fiveUnconfirmed, Nothing)]
