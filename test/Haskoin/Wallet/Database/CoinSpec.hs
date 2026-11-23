{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The coins of the accounts: the unspent outputs the store lists at
-- their addresses, kept in step by the sync, listed by coins with their
-- confirmations, locks and whether they can be spent, and offered to the
-- transaction builder when they can: confirmed enough, held by no pending
-- transaction of their network, a coinbase matured, an unconfirmed coin
-- from a transaction of the wallet, the portfolio's own change or what
-- another portfolio of the wallet paid it. A coin belongs to its account,
-- and a payment spends from a pool, the accounts of a portfolio on one
-- network together. The balances a pool and its addresses can spend add
-- up those coins, and so does the balance of the coins at each address
-- that balances lists. The account of these tests is the bip44 account of
-- portfolio 0 of the test wallet on Bitcoin, and their pool what that
-- portfolio holds there.
module Haskoin.Wallet.Database.CoinSpec (spec) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database.Account (CoinTotals (..), accountPool, getAccountById)
import Haskoin.Wallet.Database.Address (AddrType (..), CoinBalance (..), addrPath, extendChainTo, pathText)
import Haskoin.Wallet.Database.BestBlock (updateBest)
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio (Place (..), portfolioPool)
import Haskoin.Wallet.Database.RawTx (insertRawTx)
import Haskoin.Wallet.Database.Tag (noTagFilter)
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath (..), legacyPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo (repsertSummary, storeSummary, storeTime)
import Haskoin.Wallet.Database.TxLabel (setTxLabel)
import Data.Word (Word32, Word64, Word8)
import Haskoin.Wallet.Testnet3 (btcTestnet3)
import Numeric.Natural (Natural)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), MarshalJsonBox (..), testIdentity)
import Data.Default (def)
import Haskoin.Wallet.Arbitrary (arbitraryCoinLock, arbitraryJsonCoin)
import System.Timeout (timeout)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  refreshingSpec
  listingSpec
  spendableSpec
  balancesSpec
  coinBalanceSpec
  userLocksSpec
  tokensSpec
  bothTypesSpec
  sharedKeySpec
  lifeSpec
  rowSpec
  holdersSpec
  shortIdSpec
  jsonSpec
  leftSpec
  chosenSpec
  -- A coin and its lock read back as they were written
  testIdentity
    def
      { jsonTests = [JsonBox arbitraryCoinLock],
        marshalJsonTests = [MarshalJsonBox ((,) btc <$> arbitraryJsonCoin btc)]
      }

{- Fixtures -}

-- | The account of these tests
account :: ExceptT String (DB IO) DBAccountId
account = fst <$> testNewAcc

-- | The pool an account is a part of: what its portfolio holds on its
-- network
poolOf :: DBAccountId -> ExceptT String (DB IO) Pool
poolOf accId = accountPool <$> getAccountById accId

-- | The bip44 account on Bitcoin of another portfolio of the wallet:
-- portfolio 1 of the test seed, whose first receiving address is
-- 'account1Addr'
otherAccount :: ExceptT String (DB IO) DBAccountId
otherAccount = do
  _ <- testPortfolio
  second <- nextPortfolioOf testMnemonic
  fst <$> accountOf second btc Bip44

-- | The address of the other portfolio these tests use, with its path
otherAccountPaths :: Map Address SoftPath
otherAccountPaths = Map.fromList [(account1Addr, Deriv :/ 0 :/ 0)]

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
payment :: Store.Unspent
payment = storeCoin (fakeTxHash 1, 0) Nothing (externalAddr 0) 50000

-- | The same coin, confirmed at height 800000
confirmedPayment :: Store.Unspent
confirmedPayment = storeCoin (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000

-- | 20000 paid to the second receiving address by transaction 2, at height
-- 800001
secondPayment :: Store.Unspent
secondPayment = storeCoin (fakeTxHash 2, 0) (Just 800001) (externalAddr 1) 20000

-- | 30000 paid to the second receiving address by transaction 3, waiting in
-- the mempool
thirdPayment :: Store.Unspent
thirdPayment = storeCoin (fakeTxHash 3, 0) Nothing (externalAddr 1) 30000

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
threeCoins :: DBAccountId -> ExceptT String (DB IO) ()
threeCoins accId = void $ refresh accId [confirmedPayment, secondPayment, thirdPayment]

-- | Refresh the coins of the addresses of these tests with what the store
-- lists, as the sync does when their balances changed
refresh :: DBAccountId -> [Store.Unspent] -> ExceptT String (DB IO) Refreshed
refresh accId = refreshCoins btc accId (const seenTime) (Map.keys accountPaths)

-- | What a refresh changed: how many coins, and the outpoints of those it
-- inserted and of those it deleted
changes :: Refreshed -> (Int, [OutPoint], [OutPoint])
changes refreshed = (refreshedCount refreshed, sort (rowOutpoint <$> insertedCoins refreshed), sort (rowOutpoint <$> deletedCoins refreshed))

rowOutpoint :: DBCoin -> OutPoint
rowOutpoint = forceRight . coinOutpoint

-- | The coin rows stored at an address
storedAt :: Text -> ExceptT String (DB IO) [DBCoin]
storedAt addr = lift $ map P.entityVal <$> P.selectList [DBCoinAddress P.==. addr] []

-- | The coins of an account as coins lists them when narrowed to its
-- type: those after the coin given, or the first ones, up to the count
accountCoins :: DBAccountId -> CoinFilter -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> ExceptT String (DB IO) [JsonCoin]
accountCoins accId shown afterM countM = do
  pool <- poolOf accId
  coinsAfter pool [accId] shown afterM countM

-- | The coins of the account as coins lists them: outpoint, confirmations
-- and lock
listed :: DBAccountId -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> ExceptT String (DB IO) [(OutPoint, Natural, Maybe CoinLock)]
listed accId afterM countM = map (\c -> (jsonCoinOutpoint c, jsonCoinConfirmations c, jsonCoinLock c)) <$> accountCoins accId AllCoins afterM countM

-- | The outpoints of the coins of the account as `coins --reverse` lists
-- them: those after the coin given, or the first ones, up to the count
listedReversed :: DBAccountId -> Maybe (Store.BlockRef, OutPoint) -> Maybe Natural -> ExceptT String (DB IO) [OutPoint]
listedReversed accId afterM countM = do
  pool <- poolOf accId
  map (jsonCoinOutpoint . fst) <$> listedCoinsAfter ReverseOrder pool [accId] AllCoins noTagFilter afterM countM

-- | The coins of the account as coins lists them, and whether each can be
-- spent now
spendableFlags :: DBAccountId -> ExceptT String (DB IO) [(OutPoint, Bool)]
spendableFlags accId = map (\c -> (jsonCoinOutpoint c, jsonCoinSpendable c)) <$> accountCoins accId AllCoins Nothing Nothing

-- | The coins of the pool of an account that are offered to the builder
-- at a number of confirmations, as the store last reported them
offered :: DBAccountId -> Natural -> ExceptT String (DB IO) [Store.Unspent]
offered accId minConf = do
  pool <- poolOf accId
  spendableCoins pool minConf

-- | The outpoints of the coins of the pool of an account offered to the
-- builder at a number of confirmations
spendable :: DBAccountId -> Natural -> ExceptT String (DB IO) [OutPoint]
spendable accId minConf = sort . map (.outpoint) <$> offered accId minConf

-- | The outpoints of the coins of the pool of an account the builder may
-- not spend at a number of confirmations, with their locks
unspendable :: DBAccountId -> Natural -> ExceptT String (DB IO) [(OutPoint, Maybe CoinLock)]
unspendable accId minConf = do
  pool <- poolOf accId
  sortOn fst . map (\(coin, lock) -> (coin.outpoint, lock)) <$> unspendableCoins pool minConf

-- | What the pool of an account can spend now
spendableOf :: DBAccountId -> ExceptT String (DB IO) Word64
spendableOf accId = coinsSpendable <$> (coinTotals =<< poolOf accId)

-- | Lock the coin the user names, as lockcoin does without --network
lockNamed :: OutPoint -> ExceptT String (DB IO) (Place, JsonCoin)
lockNamed outpoint = lockCoin =<< walletCoin def Nothing outpoint

-- | Unlock the coin the user names, as unlockcoin does without --network
unlockNamed :: OutPoint -> ExceptT String (DB IO) (Place, JsonCoin)
unlockNamed outpoint = unlockCoin =<< walletCoin def Nothing outpoint

-- | Label a transaction of the pool of an account
labelTx :: DBAccountId -> TxHash -> Text -> ExceptT String (DB IO) ()
labelTx accId tid label = do
  pool <- poolOf accId
  lift $ setTxLabel pool tid label

-- | The best block of Bitcoin at a height
bestAt :: BlockHeight -> ExceptT String (DB IO) ()
bestAt height = lift $ updateBest btc (fakeBlockHash 1) height 1700000000

-- | A pending transaction of the pool of an account spending the first
-- payment
spendingFirst :: DBAccountId -> ExceptT String (DB IO) ()
spendingFirst accId = do
  pool <- poolOf accId
  lift $ P.insert_ $ pendingRow pool (spendingTx [(fakeTxHash 1, 0)] [(otherAddr 0, 49000)])

-- | A pending transaction of the portfolio on Bitcoin Cash spending the
-- outpoint of the first payment: from before the fork, the same
-- transaction would have the same id on both chains, but its coin on one
-- is not its coin on the other
spendingFirstOnCash :: ExceptT String (DB IO) ()
spendingFirstOnCash = do
  (cashId, _) <- testNewAccBch
  spendingFirst cashId

-- | The row of the confirmed payment as a refresh writes it, for the tests
-- that spoil one of its columns
paymentRow :: DBAccountId -> DBCoin
paymentRow accId =
  DBCoin accId (outpointBytes firstCoin) "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 50000 (S.encode (Store.BlockRef 800000 0)) seenTime False False fixtureTime

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

-- | Store the summary of a transaction of the store for the pool of the
-- account of these tests, as the sync does
summarise :: DBAccountId -> Store.Transaction -> ExceptT String (DB IO) ()
summarise = summariseFor accountPaths

-- | Store the summary of a transaction of the store for the pool of an
-- account, as the sync does, the addresses of the pool given with their
-- paths: those of its bip44 account
summariseFor :: Map Address SoftPath -> DBAccountId -> Store.Transaction -> ExceptT String (DB IO) ()
summariseFor paths accId tx = do
  pool <- poolOf accId
  void $ lift $ repsertSummary pool tx.txid tx.block (storeTime tx.timestamp) (storeSummary btc (Map.map legacyPath paths) tx)

-- | Transaction 1 confirmed at 800000: someone else pays the first payment
firstPaymentTx :: Store.Transaction
firstPaymentTx = spendingStoreTx (fakeTxHash 1) (Store.BlockRef 800000 0) (fakeTxHash 21, 0) (otherAddr 1) [(externalAddr 0, 50000)]

-- | Transaction 3 in the mempool: someone else pays the third payment
thirdPaymentTx :: Store.Transaction
thirdPaymentTx = spendingStoreTx (fakeTxHash 3) (Store.MemRef 0) (fakeTxHash 23, 0) (otherAddr 1) [(externalAddr 1, 30000)]

-- | Transaction 8 in the mempool: the account pays 30000 from the confirmed
-- first payment, with 19000 of change at the first change address
ownPaymentTx :: Store.Transaction
ownPaymentTx = spendingStoreTx (fakeTxHash 8) (Store.MemRef 0) (fakeTxHash 1, 0) (externalAddr 0) [(otherAddr 0, 30000), (internalAddr 0, 19000)]

-- | The change of transaction 8
ownChange :: Store.Unspent
ownChange = storeCoin (fakeTxHash 8, 1) Nothing (internalAddr 0) 19000

-- | Transaction 9 in the mempool: the account pays 20000 from the
-- unconfirmed third payment, with 9000 of change at the second change
-- address
paymentOnThirdTx :: Store.Transaction
paymentOnThirdTx = spendingStoreTx (fakeTxHash 9) (Store.MemRef 0) (fakeTxHash 3, 0) (externalAddr 1) [(otherAddr 0, 20000), (internalAddr 1, 9000)]

-- | The change of transaction 9
changeOnThird :: Store.Unspent
changeOnThird = storeCoin (fakeTxHash 9, 1) Nothing (internalAddr 1) 9000

-- | Transaction 10 in the mempool: the account pays 30000 to the other
-- account from the confirmed first payment, with 19000 of change at the
-- first change address
paymentToOtherTx :: Store.Transaction
paymentToOtherTx = spendingStoreTx (fakeTxHash 10) (Store.MemRef 0) (fakeTxHash 1, 0) (externalAddr 0) [(account1Addr, 30000), (internalAddr 0, 19000)]

-- | The coin of the other account from transaction 10
paidToOther :: Store.Unspent
paidToOther = storeCoin (fakeTxHash 10, 0) Nothing account1Addr 30000

-- | Transaction 11 in the mempool: the account pays 20000 to the other
-- account from the unconfirmed third payment, with 9000 of change at the
-- second change address
paymentToOtherOnThirdTx :: Store.Transaction
paymentToOtherOnThirdTx = spendingStoreTx (fakeTxHash 11) (Store.MemRef 0) (fakeTxHash 3, 0) (externalAddr 1) [(account1Addr, 20000), (internalAddr 1, 9000)]

-- | The coin of the other account from transaction 11
paidToOtherOnThird :: Store.Unspent
paidToOtherOnThird = storeCoin (fakeTxHash 11, 0) Nothing account1Addr 20000

-- | Transaction 12 in the mempool: the account and someone else pay the
-- other account 69000 together, 50000 from the confirmed first payment and
-- 20000 from a coin of theirs, with a fee of 1000
jointPaymentToOtherTx :: Store.Transaction
jointPaymentToOtherTx =
  storeTx
    (fakeTxHash 12)
    (Store.MemRef 0)
    [inputFrom (fakeTxHash 1, 0) (externalAddr 0) 50000, inputFrom (fakeTxHash 24, 0) (otherAddr 1) 20000]
    [outputTo account1Addr 69000]
    1000

-- | The coin of the other account from transaction 12
jointlyPaidToOther :: Store.Unspent
jointlyPaidToOther = storeCoin (fakeTxHash 12, 0) Nothing account1Addr 69000

-- | The coinbase of block 800000, paying the reward of 312500000 to the
-- first receiving address. Its single input spends nothing and carries the
-- height of the block, as BIP34 asks.
coinbase :: Tx
coinbase =
  Tx 1 [TxIn nullOutPoint (BS.pack [0x03, 0x00, 0x35, 0x0c]) maxBound] [TxOut 312500000 (addressScript (externalAddr 0))] [] 0

{- Refreshing -}

refreshingSpec :: Spec
refreshingSpec =
  describe "Refreshing the coins of addresses, as the sync does" $ do
    it "stores a new coin with its account, address, value and block" $
      inDatabaseE $ do
        accId <- account
        (changes <$> refresh accId [payment]) `dbShouldBeE` (1, [firstCoin], [])
        rows <- storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        liftIO $ do
          map (\c -> (dBCoinAccount c, dBCoinValue c, S.decode (dBCoinBlockRef c))) rows `shouldBe` [(accId, 50000, Right (Store.MemRef 0))]

    it "stores a coin with the time of the transaction that paid it" $
      inDatabaseE $ do
        accId <- account
        -- The transaction of the coin was seen 600 seconds after seenTime
        _ <- refreshCoins btc accId (const (addUTCTime 600 seenTime)) [externalAddr 0] [payment]
        -- 22:13:20 UTC + 600 seconds = 22:23:20 UTC
        (map dBCoinTime <$> storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") `dbShouldBeE` [UTCTime (fromGregorian 2023 11 14) (22 * 3600 + 23 * 60 + 20)]

    it "writes the time of a coin again when the time of its transaction changed, and counts the coin" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [confirmedPayment]
        -- The store took the transaction in again from a block dated an
        -- hour later
        (changes <$> refreshCoins btc accId (const (addUTCTime 3600 seenTime)) [externalAddr 0] [confirmedPayment]) `dbShouldBeE` (1, [], [])
        -- 22:13:20 UTC + 3600 seconds = 23:13:20 UTC
        (map dBCoinTime <$> storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") `dbShouldBeE` [UTCTime (fromGregorian 2023 11 14) (23 * 3600 + 13 * 60 + 20)]

    it "changes nothing when the store lists the same coins again" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [payment]
        (changes <$> refresh accId [payment]) `dbShouldBeE` (0, [], [])

    it "stores once a coin the store lists twice" $
      inDatabaseE $ do
        accId <- account
        (changes <$> refresh accId [payment, payment]) `dbShouldBeE` (1, [firstCoin], [])
        (map rowOutpoint <$> storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") `dbShouldBeE` [firstCoin]

    it "moves a coin to the block that confirms it, as the store reports it" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [payment]
        (changes <$> refresh accId [confirmedPayment]) `dbShouldBeE` (1, [], [])
        rows <- storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        liftIO $ map (S.decode . dBCoinBlockRef) rows `shouldBe` [Right (Store.BlockRef 800000 0)]
        offered accId 0 `dbShouldBeE` [confirmedPayment]

    it "deletes a coin the store no longer lists, as once it is spent" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [confirmedPayment]
        (changes <$> refresh accId []) `dbShouldBeE` (1, [], [firstCoin])
        storedAt "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" `dbShouldBeE` []

    it "keeps the coins of one address apart, deleting only the one that went" $
      inDatabaseE $ do
        accId <- account
        (changes <$> refresh accId [secondPayment, thirdPayment]) `dbShouldBeE` (2, [secondCoin, thirdCoin], [])
        (changes <$> refresh accId [thirdPayment]) `dbShouldBeE` (1, [], [secondCoin])
        (map rowOutpoint <$> storedAt "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg") `dbShouldBeE` [thirdCoin]

    it "leaves alone the addresses it is not given, whatever the store lists for them" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [secondPayment]
        -- The second address is not refreshed: its coin stays, and the new
        -- one the store lists there is not stored
        (changes <$> refreshCoins btc accId (const seenTime) [externalAddr 0] [payment, thirdPayment]) `dbShouldBeE` (1, [firstCoin], [])
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [firstCoin, secondCoin]

    it "passes over a coin the store lists without an address" $
      inDatabaseE $ do
        accId <- account
        (changes <$> refresh accId [unaddressed]) `dbShouldBeE` (0, [], [])

    it "passes over a coin whose script is not the standard script of its address" $
      inDatabaseE $ do
        accId <- account
        (changes <$> refresh accId [oddlyWrittenPayment]) `dbShouldBeE` (0, [], [])

    it "refuses an address its network cannot write" $
      inDatabaseE $ do
        (accId, _) <- testNewAccBch
        lift $ dbShouldFailWith "The coins of an address of another network cannot be refreshed" $ refreshCoins bch accId (const seenTime) [segwitAddr] []

{- Listing -}

listingSpec :: Spec
listingSpec =
  describe "Listing the coins of an account, as coins does" $ do
    it "lists the unconfirmed coins first, then the confirmed ones from the newest block" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [thirdCoin, secondCoin, firstCoin]

    -- The coin of transaction 1 is in block 800001, dated 1700000000. The
    -- coin of transaction 2 is in block 800000 under it, dated 600 seconds
    -- later.
    it "lists the confirmed coins by their blocks, the newest first, whatever the times of the blocks" $
      inDatabaseE $ do
        accId <- account
        let seenWhen tid = if tid == fakeTxHash 2 then addUTCTime 600 seenTime else seenTime
        _ <-
          refreshCoins btc accId seenWhen [externalAddr 0]
            [storeCoin (fakeTxHash 1, 0) (Just 800001) (externalAddr 0) 10000, storeCoin (fakeTxHash 2, 0) (Just 800000) (externalAddr 0) 20000]
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [OutPoint (fakeTxHash 1) 0, OutPoint (fakeTxHash 2) 0]

    -- The coin of transaction 1 is in a block dated an hour after
    -- 1700000000. The coin of transaction 2 is in the mempool, paid at
    -- 1700000000.
    it "lists the unconfirmed coins first, though a confirmed one was paid later" $
      inDatabaseE $ do
        accId <- account
        let seenWhen tid = if tid == fakeTxHash 1 then addUTCTime 3600 seenTime else seenTime
        _ <-
          refreshCoins btc accId seenWhen [externalAddr 0]
            [storeCoin (fakeTxHash 1, 0) (Just 800001) (externalAddr 0) 10000, storeCoin (fakeTxHash 2, 0) Nothing (externalAddr 0) 20000]
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 1) 0]

    it "lists a page at a time, each page after the last coin of the one before" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        (map fst3 <$> listed accId Nothing (Just 1)) `dbShouldBeE` [thirdCoin]
        (map fst3 <$> listed accId (Just (Store.MemRef 0, thirdCoin)) (Just 1)) `dbShouldBeE` [secondCoin]
        (map fst3 <$> listed accId (Just (Store.BlockRef 800001 0, secondCoin)) (Just 2)) `dbShouldBeE` [firstCoin]
        listed accId (Just (Store.BlockRef 800000 0, firstCoin)) Nothing `dbShouldBeE` []

    it "lists the outputs of one transaction by their index, so that no page repeats one" $
      inDatabaseE $ do
        accId <- account
        let output i = storeCoin (fakeTxHash 4, i) (Just 800000) (externalAddr 0) 10000
        _ <- refresh accId [output 2, output 0, output 1]
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [OutPoint (fakeTxHash 4) 0, OutPoint (fakeTxHash 4) 1, OutPoint (fakeTxHash 4) 2]
        (map fst3 <$> listed accId (Just (Store.BlockRef 800000 0, OutPoint (fakeTxHash 4) 0)) (Just 1)) `dbShouldBeE` [OutPoint (fakeTxHash 4) 1]

    -- The first coin is in block 800000, the second in block 800001, and
    -- the third in the mempool
    it "lists the oldest block first and the unconfirmed coins last with --reverse" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        listedReversed accId Nothing Nothing `dbShouldBeE` [firstCoin, secondCoin, thirdCoin]

    it "lists a page at a time with --reverse, each page after the last coin of the one before" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        listedReversed accId Nothing (Just 1) `dbShouldBeE` [firstCoin]
        listedReversed accId (Just (Store.BlockRef 800000 0, firstCoin)) (Just 1) `dbShouldBeE` [secondCoin]
        listedReversed accId (Just (Store.BlockRef 800001 0, secondCoin)) (Just 2) `dbShouldBeE` [thirdCoin]
        listedReversed accId (Just (Store.MemRef 0, thirdCoin)) Nothing `dbShouldBeE` []

    it "lists the outputs of one transaction from the last index to the first with --reverse, so that no page repeats one" $
      inDatabaseE $ do
        accId <- account
        let output i = storeCoin (fakeTxHash 4, i) (Just 800000) (externalAddr 0) 10000
        _ <- refresh accId [output 2, output 0, output 1]
        listedReversed accId Nothing Nothing `dbShouldBeE` [OutPoint (fakeTxHash 4) 2, OutPoint (fakeTxHash 4) 1, OutPoint (fakeTxHash 4) 0]
        listedReversed accId (Just (Store.BlockRef 800000 0, OutPoint (fakeTxHash 4) 2)) (Just 1) `dbShouldBeE` [OutPoint (fakeTxHash 4) 1]

    it "names the block of a coin of the accounts listed, for --after, and refuses one they do not have" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        coinBlock [accId] thirdCoin `dbShouldBeE` Store.MemRef 0
        coinBlock [accId] secondCoin `dbShouldBeE` Store.BlockRef 800001 0
        lift $
          dbShouldFailWith "The coin 0000000000000000000000000000000000000000000000000000000000000009:0 is not among the coins listed" $
            coinBlock [accId] (OutPoint (fakeTxHash 9) 0)

    it "counts the confirmations from the best block, and none in the mempool" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        bestAt 800002
        listed accId Nothing Nothing `dbShouldBeE` [(thirdCoin, 0, Nothing), (secondCoin, 2, Nothing), (firstCoin, 3, Nothing)]

    it "counts one confirmation for a confirmed coin while the best block is unknown or behind it" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [confirmedPayment]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]
        bestAt 799990
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]

    it "counts from the best block of the account's network, not another's" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [confirmedPayment]
        lift $ updateBest bch (fakeBlockHash 2) 900000 1700000000
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]

    it "shows a coin a pending transaction spends as locked, and the others as free" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirst accId
        (map (\(o, _, l) -> (o, l)) <$> listed accId Nothing Nothing) `dbShouldBeE` [(thirdCoin, Nothing), (secondCoin, Nothing), (firstCoin, Just LockedByTransaction)]

    it "shows as locked a coin a pending transaction of another portfolio spends" $
      inDatabaseE $ do
        accId <- account
        (otherId, _) <- testNewAcc2
        threeCoins accId
        spendingFirst otherId
        (map (\(o, _, l) -> (o, l)) <$> listed accId Nothing Nothing) `dbShouldBeE` [(thirdCoin, Nothing), (secondCoin, Nothing), (firstCoin, Just LockedByTransaction)]

    it "leaves free a coin whose outpoint a pending transaction of the other chain spends" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirstOnCash
        (map (\(o, _, l) -> (o, l)) <$> listed accId Nothing Nothing) `dbShouldBeE` [(thirdCoin, Nothing), (secondCoin, Nothing), (firstCoin, Nothing)]

    it "marks a confirmed coin as spendable, and one a pending transaction holds as not" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [confirmedPayment, secondPayment]
        spendingFirst accId
        spendableFlags accId `dbShouldBeE` [(secondCoin, True), (firstCoin, False)]

    it "marks an unconfirmed coin as spendable only when the wallet made its transaction" $
      inDatabaseE $ do
        accId <- account
        summarise accId thirdPaymentTx
        summarise accId firstPaymentTx
        summarise accId ownPaymentTx
        _ <- refresh accId [thirdPayment, ownChange]
        (sort <$> spendableFlags accId) `dbShouldBeE` [(thirdCoin, False), (OutPoint (fakeTxHash 8) 1, True)]

    it "marks as spendable an unconfirmed coin another portfolio of the wallet paid" $
      inDatabaseE $ do
        accId <- account
        otherId <- otherAccount
        summarise accId firstPaymentTx
        summarise accId paymentToOtherTx
        summariseFor otherAccountPaths otherId paymentToOtherTx
        _ <- refreshCoins btc otherId (const seenTime) [account1Addr] [paidToOther]
        spendableFlags otherId `dbShouldBeE` [(OutPoint (fakeTxHash 10) 0, True)]

    it "marks a coinbase as spendable once it has matured" $
      inDatabaseE $ do
        accId <- account
        lift $ insertRawTx coinbase
        _ <- refresh accId [storeCoin (txHash coinbase, 0) (Just 800000) (externalAddr 0) 312500000]
        bestAt 800098
        spendableFlags accId `dbShouldBeE` [(OutPoint (txHash coinbase) 0, False)]
        bestAt 800099
        spendableFlags accId `dbShouldBeE` [(OutPoint (txHash coinbase) 0, True)]

    it "shows on a coin the label of the payment that paid it" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [confirmedPayment]
        summarise accId firstPaymentTx
        labelTx accId (fakeTxHash 1) "Salary"
        (map (\c -> (jsonCoinOutpoint c, jsonCoinTxLabel c)) <$> accountCoins accId AllCoins Nothing Nothing) `dbShouldBeE` [(firstCoin, "Salary")]

    -- The label says what the payment was for, not what its change is
    it "shows no label on the change of a payment the account made, whatever its label" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [ownChange]
        summarise accId ownPaymentTx
        labelTx accId (fakeTxHash 8) "Rent"
        (map (\c -> (jsonCoinOutpoint c, jsonCoinTxLabel c)) <$> accountCoins accId AllCoins Nothing Nothing) `dbShouldBeE` [(OutPoint (fakeTxHash 8) 1, "")]

    -- The first payment is at the first receiving address, and the change
    -- of transaction 8 at the first change address. The unconfirmed change
    -- is listed first.
    it "tells a coin at a change address from one at a receiving address" $
      inDatabaseE $ do
        accId <- account
        extendChainTo accId AddrExternal 1
        extendChainTo accId AddrInternal 1
        _ <- refresh accId [confirmedPayment, ownChange]
        (map (\c -> (jsonCoinOutpoint c, jsonCoinChange c)) <$> accountCoins accId AllCoins Nothing Nothing) `dbShouldBeE` [(OutPoint (fakeTxHash 8) 1, True), (firstCoin, False)]

    -- The same coins: the change at /1/0, the payment at /0/0
    it "gives the path of the address that holds each coin" $
      inDatabaseE $ do
        accId <- account
        extendChainTo accId AddrExternal 1
        extendChainTo accId AddrInternal 1
        _ <- refresh accId [confirmedPayment, ownChange]
        (map (\c -> (jsonCoinOutpoint c, jsonCoinPath c)) <$> accountCoins accId AllCoins Nothing Nothing)
          `dbShouldBeE` [(OutPoint (fakeTxHash 8) 1, Just (legacyPath (Deriv :/ 1 :/ 0))), (firstCoin, Just (legacyPath (Deriv :/ 0 :/ 0)))]

    it "shows a coin the user locked as locked by the user" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        _ <- lockNamed secondCoin
        (map (\(o, _, l) -> (o, l)) <$> listed accId Nothing Nothing)
          `dbShouldBeE` [(thirdCoin, Nothing), (secondCoin, Just LockedByUser), (firstCoin, Nothing)]

    it "lists the locked coins only, of both kinds, when asked" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirst accId
        _ <- lockNamed secondCoin
        (map (\c -> (jsonCoinOutpoint c, jsonCoinLock c)) <$> accountCoins accId LockedCoins Nothing Nothing)
          `dbShouldBeE` [(secondCoin, Just LockedByUser), (firstCoin, Just LockedByTransaction)]

    it "lists the coins of the account only" $
      inDatabaseE $ do
        accId <- account
        (otherId, _) <- testNewAcc2
        _ <- refresh accId [confirmedPayment]
        _ <- refreshCoins btc otherId (const seenTime) [externalAddr2] [storeCoin (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        (map fst3 <$> listed accId Nothing Nothing) `dbShouldBeE` [firstCoin]
        (map fst3 <$> listed otherId Nothing Nothing) `dbShouldBeE` [OutPoint (fakeTxHash 4) 0]

    it "reads the address of a Bitcoin Cash coin on its network" $
      inDatabaseE $ do
        (accId, _) <- testNewAccBch
        _ <- refreshCoins bch accId (const seenTime) [bchExternalAddr] [storeCoin (fakeTxHash 5, 0) (Just 800000) bchExternalAddr 10000]
        (map jsonCoinAddress <$> accountCoins accId AllCoins Nothing Nothing) `dbShouldBeE` [bchExternalAddr]

    it "refuses a stored coin whose address does not read, rather than leaving it out" $
      inDatabaseE $ do
        accId <- account
        lift $ P.insert_ (paymentRow accId) {dBCoinAddress = "nonsense"}
        lift $
          dbShouldFailWith "The stored coin 0000000000000000000000000000000000000000000000000000000000000001:0 has an address that does not read: nonsense" $
            accountCoins accId AllCoins Nothing Nothing

    it "refuses a stored coin whose block does not read" $
      inDatabaseE $ do
        accId <- account
        lift $ P.insert_ (paymentRow accId) {dBCoinBlockRef = "nonsense"}
        lift $
          dbShouldFailWith "The stored coin 0000000000000000000000000000000000000000000000000000000000000001:0 has a block that does not read" $
            accountCoins accId AllCoins Nothing Nothing

{- Spendable -}

spendableSpec :: Spec
spendableSpec =
  describe "The coins offered to the transaction builder" $ do
    it "offers the coins with at least the confirmations asked" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        -- 800002 is the third block for the first payment, the second for
        -- the second payment
        bestAt 800002
        spendable accId 1 `dbShouldBeE` [firstCoin, secondCoin]
        spendable accId 2 `dbShouldBeE` [firstCoin, secondCoin]
        spendable accId 3 `dbShouldBeE` [firstCoin]
        spendable accId 4 `dbShouldBeE` []

    it "counts one confirmation for a confirmed coin while the best block is unknown" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendable accId 1 `dbShouldBeE` [firstCoin, secondCoin]
        spendable accId 2 `dbShouldBeE` []

    it "holds back a coin a pending transaction spends" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirst accId
        spendable accId 1 `dbShouldBeE` [secondCoin]

    it "holds back a coin a pending transaction of another portfolio spends" $
      inDatabaseE $ do
        accId <- account
        (otherId, _) <- testNewAcc2
        threeCoins accId
        spendingFirst otherId
        spendable accId 1 `dbShouldBeE` [secondCoin]

    it "offers a coin whose outpoint a pending transaction of the other chain spends" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirstOnCash
        spendable accId 1 `dbShouldBeE` [firstCoin, secondCoin]

    it "holds back a coin the user locked" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        _ <- lockNamed firstCoin
        spendable accId 1 `dbShouldBeE` [secondCoin]

    it "holds back a coinbase until its 100th confirmation" $
      inDatabaseE $ do
        accId <- account
        lift $ insertRawTx coinbase
        _ <- refresh accId [storeCoin (txHash coinbase, 0) (Just 800000) (externalAddr 0) 312500000]
        bestAt 800098
        spendable accId 1 `dbShouldBeE` []
        bestAt 800099
        spendable accId 1 `dbShouldBeE` [OutPoint (txHash coinbase) 0]

    it "holds back an unconfirmed coin someone else pays, even at zero confirmations: they can still replace it" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        summarise accId thirdPaymentTx
        spendable accId 0 `dbShouldBeE` [firstCoin, secondCoin]

    it "holds back an unconfirmed coin whose transaction the sync has not stored" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [thirdPayment]
        spendable accId 0 `dbShouldBeE` []

    it "offers at zero confirmations the unconfirmed change of the portfolio's own payment" $
      inDatabaseE $ do
        accId <- account
        summarise accId firstPaymentTx
        summarise accId ownPaymentTx
        _ <- refresh accId [ownChange]
        spendable accId 0 `dbShouldBeE` [OutPoint (fakeTxHash 8) 1]
        spendable accId 1 `dbShouldBeE` []

    -- Transactions 31 and 32, in the mempool, each spend the output 0 of the
    -- other from the first receiving address: no chain can hold them, and
    -- a store that reports them lies. The check gives up after 5 seconds,
    -- where it would otherwise follow them for ever.
    it "holds back a coin of two unconfirmed transactions that spend each other, and gives an answer" $ do
      answered <- timeout 5000000 $ inDatabaseE $ do
        accId <- account
        summarise accId (spendingStoreTx (fakeTxHash 31) (Store.MemRef 0) (fakeTxHash 32, 0) (externalAddr 0) [(externalAddr 0, 40000)])
        summarise accId (spendingStoreTx (fakeTxHash 32) (Store.MemRef 0) (fakeTxHash 31, 0) (externalAddr 0) [(externalAddr 0, 40000)])
        _ <- refresh accId [storeCoin (fakeTxHash 31, 0) Nothing (externalAddr 0) 40000]
        spendable accId 0 `dbShouldBeE` []
      answered `shouldBe` Just ()

    -- Transaction 33 spends its own output 0
    it "holds back a coin of an unconfirmed transaction that spends itself" $ do
      answered <- timeout 5000000 $ inDatabaseE $ do
        accId <- account
        summarise accId (spendingStoreTx (fakeTxHash 33) (Store.MemRef 0) (fakeTxHash 33, 0) (externalAddr 0) [(externalAddr 0, 40000)])
        _ <- refresh accId [storeCoin (fakeTxHash 33, 0) Nothing (externalAddr 0) 40000]
        spendable accId 0 `dbShouldBeE` []
      answered `shouldBe` Just ()

    it "holds back the change of a payment that spends an unconfirmed coin someone else paid" $
      inDatabaseE $ do
        accId <- account
        summarise accId thirdPaymentTx
        summarise accId paymentOnThirdTx
        _ <- refresh accId [changeOnThird]
        spendable accId 0 `dbShouldBeE` []

    -- Only the wallet can replace a payment between its portfolios, as for
    -- its change
    it "offers at zero confirmations what another portfolio of the wallet paid, as it offers the portfolio's own change" $
      inDatabaseE $ do
        accId <- account
        otherId <- otherAccount
        summarise accId firstPaymentTx
        summarise accId paymentToOtherTx
        summariseFor otherAccountPaths otherId paymentToOtherTx
        _ <- refreshCoins btc otherId (const seenTime) [account1Addr] [paidToOther]
        spendable otherId 0 `dbShouldBeE` [OutPoint (fakeTxHash 10) 0]
        spendable otherId 1 `dbShouldBeE` []

    -- The payment is stored for the receiving portfolio alone, as when the
    -- paying portfolio has not synced yet: nothing says its inputs are the
    -- wallet's
    it "holds back what another portfolio paid until the sync has stored the payment for that portfolio too" $
      inDatabaseE $ do
        accId <- account
        otherId <- otherAccount
        summarise accId firstPaymentTx
        summariseFor otherAccountPaths otherId paymentToOtherTx
        _ <- refreshCoins btc otherId (const seenTime) [account1Addr] [paidToOther]
        spendable otherId 0 `dbShouldBeE` []

    it "holds back what another portfolio paid out of an unconfirmed coin someone else paid it" $
      inDatabaseE $ do
        accId <- account
        otherId <- otherAccount
        summarise accId thirdPaymentTx
        summarise accId paymentToOtherOnThirdTx
        summariseFor otherAccountPaths otherId paymentToOtherOnThirdTx
        _ <- refreshCoins btc otherId (const seenTime) [account1Addr] [paidToOtherOnThird]
        spendable otherId 0 `dbShouldBeE` []

    it "holds back what another portfolio paid along with someone else, who can still undo the payment" $
      inDatabaseE $ do
        accId <- account
        otherId <- otherAccount
        summarise accId firstPaymentTx
        summarise accId jointPaymentToOtherTx
        summariseFor otherAccountPaths otherId jointPaymentToOtherTx
        _ <- refreshCoins btc otherId (const seenTime) [account1Addr] [jointlyPaidToOther]
        spendable otherId 0 `dbShouldBeE` []

    it "offers each coin as the store last reported it" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [secondPayment]
        offered accId 1 `dbShouldBeE` [secondPayment]

    it "offers the coins of the pool only" $
      inDatabaseE $ do
        accId <- account
        (otherId, _) <- testNewAcc2
        _ <- refresh accId [confirmedPayment]
        _ <- refreshCoins btc otherId (const seenTime) [externalAddr2] [storeCoin (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        spendable accId 0 `dbShouldBeE` [firstCoin]
        spendable otherId 0 `dbShouldBeE` [OutPoint (fakeTxHash 4) 0]

    it "refuses a stored coin whose address does not read, rather than leaving it out" $
      inDatabaseE $ do
        accId <- account
        lift $ P.insert_ (paymentRow accId) {dBCoinAddress = "nonsense"}
        lift $
          dbShouldFailWith "The stored coin 0000000000000000000000000000000000000000000000000000000000000001:0 has an address that does not read: nonsense" $
            offered accId 1

    -- A pending transaction spends the first coin, the user locked the
    -- second, and the third is in the mempool, paid by someone else
    it "holds back the others, each with its lock, which the third coin has none of" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirst accId
        _ <- lockNamed secondCoin
        unspendable accId 1 `dbShouldBeE` [(firstCoin, Just LockedByTransaction), (secondCoin, Just LockedByUser), (thirdCoin, Nothing)]

    -- 800002 is the third block for the first payment, the second for the
    -- second payment
    it "holds back the coins below the confirmations asked, without a lock" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        bestAt 800002
        unspendable accId 3 `dbShouldBeE` [(secondCoin, Nothing), (thirdCoin, Nothing)]

{- Balances -}

balancesSpec :: Spec
balancesSpec =
  describe "The balances the pool and its addresses can spend now" $ do
    it "add up the coins that can be spent, by address" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        summarise accId thirdPaymentTx
        -- The third payment, unconfirmed and someone else's, is left out
        spendableAtAddresses btc accId ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
          `dbShouldBeE` Map.fromList [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", 50000), ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 20000)]
        spendableOf accId `dbShouldBeE` 70000

    it "are read for the addresses asked for only" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendableAtAddresses btc accId ["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"] `dbShouldBeE` Map.fromList [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 20000)]
        spendableAtAddresses btc accId [] `dbShouldBeE` Map.empty

    it "leave out a coin a pending transaction holds" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirst accId
        spendableAtAddresses btc accId ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
          `dbShouldBeE` Map.fromList [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 20000)]
        spendableOf accId `dbShouldBeE` 20000

    it "leave out a coin the user locked" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        _ <- lockNamed secondCoin
        spendableOf accId `dbShouldBeE` 50000

    it "count the portfolio's own unconfirmed change" $
      inDatabaseE $ do
        accId <- account
        summarise accId firstPaymentTx
        summarise accId ownPaymentTx
        _ <- refresh accId [ownChange]
        spendableAtAddresses btc accId ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"] `dbShouldBeE` Map.fromList [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", 19000)]
        spendableOf accId `dbShouldBeE` 19000

    it "count a coinbase once it has matured" $
      inDatabaseE $ do
        accId <- account
        lift $ insertRawTx coinbase
        _ <- refresh accId [storeCoin (txHash coinbase, 0) (Just 800000) (externalAddr 0) 312500000]
        bestAt 800098
        spendableOf accId `dbShouldBeE` 0
        bestAt 800099
        spendableOf accId `dbShouldBeE` 312500000

    it "are those of the pool only" $
      inDatabaseE $ do
        accId <- account
        (otherId, _) <- testNewAcc2
        _ <- refresh accId [confirmedPayment]
        _ <- refreshCoins btc otherId (const seenTime) [externalAddr2] [storeCoin (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        spendableOf accId `dbShouldBeE` 50000
        spendableOf otherId `dbShouldBeE` 10000

{- The balance of the coins at each address -}

-- | The account of these tests with the addresses of 'accountPaths' stored,
-- as the sync stores them before their coins
accountWithAddresses :: ExceptT String (DB IO) DBAccountId
accountWithAddresses = do
  accId <- account
  extendChainTo accId AddrExternal 2
  extendChainTo accId AddrInternal 2
  return accId

-- | A page of the addresses of the account that hold coins, as balances
-- lists them: each by its derivation path, with the balance of its coins
balancesListed :: DBAccountId -> Maybe (DBAddress, CoinBalance) -> Maybe Natural -> ExceptT String (DB IO) [(Text, CoinBalance)]
balancesListed accId afterM countM = map (\(addr, balance) -> (pathText (addrPath addr), balance)) <$> coinBalancesAfter UsualOrder btc [accId] noTagFilter afterM countM

-- | The derivation paths of a page of the addresses of the account that
-- hold coins, as `balances --reverse` lists them
balancePathsReversed :: DBAccountId -> Maybe (DBAddress, CoinBalance) -> Maybe Natural -> ExceptT String (DB IO) [Text]
balancePathsReversed accId afterM countM = map (pathText . addrPath . fst) <$> coinBalancesAfter ReverseOrder btc [accId] noTagFilter afterM countM

-- | A confirmed coin of a value at an address, paid by a transaction
paidAt :: Word8 -> Address -> Natural -> Store.Unspent
paidAt tx addr = storeCoin (fakeTxHash tx, 0) (Just 800000) addr

coinBalanceSpec :: Spec
coinBalanceSpec =
  describe "The balance of the coins at each address, as balances lists it" $ do
    it "shows an address with two coins once, with the value of both" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [secondPayment, paidAt 4 (externalAddr 1) 15000]
        -- 20000 + 15000, both confirmed
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/1", CoinBalance 35000 35000 0 0 0 0)]

    -- The third payment is unconfirmed and someone else's
    it "counts a coin that cannot be spent yet in the total, not as spendable" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [secondPayment, thirdPayment]
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/1", CoinBalance 50000 20000 0 0 0 0)]

    -- The pending transaction spends the first payment, at /0/0, and the
    -- user locks the second, at /0/1
    it "counts the coins a pending transaction or the user locks in the total, as locked by each, not as spendable" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [confirmedPayment, secondPayment]
        spendingFirst accId
        _ <- lockNamed secondCoin
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 50000 0 50000 0 0 0), ("/0/1", CoinBalance 20000 0 0 20000 0 0)]

    it "counts a coinbase in the total, and as spendable once it has matured" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        lift $ insertRawTx coinbase
        _ <- refresh accId [storeCoin (txHash coinbase, 0) (Just 800000) (externalAddr 0) 312500000]
        bestAt 800098
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 312500000 0 0 0 0 0)]
        bestAt 800099
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 312500000 312500000 0 0 0 0)]

    it "leaves out an address whose coins are all spent" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [confirmedPayment, secondPayment]
        -- The store no longer lists the first payment
        _ <- refresh accId [secondPayment]
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/1", CoinBalance 20000 20000 0 0 0 0)]

    it "lists the largest total first, change addresses among the receiving ones" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [confirmedPayment, secondPayment, paidAt 4 (internalAddr 0) 90000]
        balancesListed accId Nothing Nothing
          `dbShouldBeE` [("/1/0", CoinBalance 90000 90000 0 0 0 0), ("/0/0", CoinBalance 50000 50000 0 0 0 0), ("/0/1", CoinBalance 20000 20000 0 0 0 0)]

    it "lists the addresses with the same total in the order of their paths, receiving addresses first" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [paidAt 4 (internalAddr 0) 20000, paidAt 5 (externalAddr 1) 20000, paidAt 6 (externalAddr 0) 20000]
        balancesListed accId Nothing Nothing
          `dbShouldBeE` [("/0/0", CoinBalance 20000 20000 0 0 0 0), ("/0/1", CoinBalance 20000 20000 0 0 0 0), ("/1/0", CoinBalance 20000 20000 0 0 0 0)]

    it "lists the addresses after a given one, with its balance, when asked" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [confirmedPayment, secondPayment, paidAt 4 (internalAddr 0) 90000]
        -- After the largest, 90000 at /1/0: 50000 and 20000
        largest <- coinBalancesAt btc [accId] ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"]
        (map fst <$> balancesListed accId (listToMaybe largest) Nothing) `dbShouldBeE` ["/0/0", "/0/1"]
        balancesListed accId (listToMaybe largest) (Just 1) `dbShouldBeE` [("/0/0", CoinBalance 50000 50000 0 0 0 0)]

    it "lists the addresses with the same total after a given one by their paths" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [paidAt 4 (internalAddr 0) 20000, paidAt 5 (externalAddr 1) 20000, paidAt 6 (externalAddr 0) 20000]
        -- After /0/1: the change address /1/0, with the same total
        second <- coinBalancesAt btc [accId] ["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
        (map fst <$> balancesListed accId (listToMaybe second) Nothing) `dbShouldBeE` ["/1/0"]

    -- 90000 at /1/0, 50000 at /0/0 and 20000 at /0/1
    it "lists the smallest total first with --reverse" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [confirmedPayment, secondPayment, paidAt 4 (internalAddr 0) 90000]
        balancePathsReversed accId Nothing Nothing `dbShouldBeE` ["/0/1", "/0/0", "/1/0"]

    it "lists the addresses with the same total from the last path to the first with --reverse" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [paidAt 4 (internalAddr 0) 20000, paidAt 5 (externalAddr 1) 20000, paidAt 6 (externalAddr 0) 20000]
        balancePathsReversed accId Nothing Nothing `dbShouldBeE` ["/1/0", "/0/1", "/0/0"]

    it "lists the addresses after a given one with --reverse: those with a larger total" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [confirmedPayment, secondPayment, paidAt 4 (internalAddr 0) 90000]
        -- After the 50000 at /0/0: the 90000 at /1/0
        middle <- coinBalancesAt btc [accId] ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]
        balancePathsReversed accId (listToMaybe middle) Nothing `dbShouldBeE` ["/1/0"]

    it "lists the addresses with the same total after a given one with --reverse: those of the paths before it" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [paidAt 4 (internalAddr 0) 20000, paidAt 5 (externalAddr 1) 20000, paidAt 6 (externalAddr 0) 20000]
        -- After /0/1: the receiving address /0/0, with the same total
        second <- coinBalancesAt btc [accId] ["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]
        balancePathsReversed accId (listToMaybe second) Nothing `dbShouldBeE` ["/0/0"]

    -- 50000 at /0/0, 20000 at /0/1 and 90000 at /1/0, asked about two
    -- addresses at a time. SQLite takes at most 32766 values in a query,
    -- and a listing read whole asks about every address of an account.
    it "reads the coins at more addresses than one query holds, a few at a time" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        _ <- refresh accId [confirmedPayment, secondPayment, paidAt 4 (internalAddr 0) 90000]
        coins <- balanceColumnsBy 2 [accId] (Just ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"])
        liftIO $
          sort [(addr, value) | (_, addr, value, _) <- coins]
            `shouldBe` [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", 90000), ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 20000), ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", 50000)]

    it "are those of the account only" $
      inDatabaseE $ do
        accId <- accountWithAddresses
        (otherId, _) <- testNewAcc2
        extendChainTo otherId AddrExternal 1
        _ <- refresh accId [confirmedPayment]
        _ <- refreshCoins btc otherId (const seenTime) [externalAddr2] [storeCoin (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        balancesListed accId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 50000 50000 0 0 0 0)]
        balancesListed otherId Nothing Nothing `dbShouldBeE` [("/0/0", CoinBalance 10000 10000 0 0 0 0)]

{- Locks of the user -}

userLocksSpec :: Spec
userLocksSpec =
  describe "The coins the user locks, as lockcoin and unlockcoin do" $ do
    it "locks a free coin, which shows locked by the user and not spendable" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        (lockState <$> lockNamed secondCoin) `dbShouldBeE` (mainPlace, Just LockedByUser, False)

    it "refuses a coin the wallet does not have" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        lift $
          dbShouldFailWith "The wallet has no coin 0000000000000000000000000000000000000000000000000000000000000009:0" $
            lockNamed (OutPoint (fakeTxHash 9) 0)

    -- a8c9c149 is the short id of the nosigHash of the pending transaction
    it "refuses a coin a pending transaction holds, and names the transaction and deletetx" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirst accId
        lift $ dbShouldFailWith "The pending transaction a8c9c149 spends the coin already: deletetx a8c9c149 frees it" $ lockNamed firstCoin

    it "refuses a coin the user locked already" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        _ <- lockNamed secondCoin
        lift $ dbShouldFailWith "The coin is already locked" $ lockNamed secondCoin

    it "unlocks a coin the user locked, which can be spent again" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        _ <- lockNamed secondCoin
        (lockState <$> unlockNamed secondCoin) `dbShouldBeE` (mainPlace, Nothing, True)
        spendable accId 1 `dbShouldBeE` [firstCoin, secondCoin]

    it "refuses to unlock a coin that is not locked" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        lift $ dbShouldFailWith "The coin is not locked" $ unlockNamed secondCoin

    it "refuses to unlock a coin a pending transaction holds, which deletetx frees" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirst accId
        lift $
          dbShouldFailWith "The coin is locked by a pending transaction, not by the user: deletetx frees it" $
            unlockNamed firstCoin

    it "keeps the lock while the coin confirms" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [payment]
        _ <- lockNamed firstCoin
        _ <- refresh accId [confirmedPayment]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Just LockedByUser)]

    it "loses the lock with the coin once the store no longer lists it: a coin that comes back comes back free" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [confirmedPayment]
        _ <- lockNamed firstCoin
        _ <- refresh accId []
        _ <- refresh accId [confirmedPayment]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]

    -- The sender of a payment that has no block replaces it to raise the
    -- fee: the same 30000 arrive at the same address under another txid
    it "moves the lock to the coin of the payment that replaces the payment of the locked coin" $
      inDatabaseE $ do
        accId <- account
        _ <- replacedAfterLock accId [replacingCoin]
        listed accId Nothing Nothing `dbShouldBeE` [(replacingCoin.outpoint, 0, Just LockedByUser)]

    it "tells which coin lost its lock, and which coin took it" $
      inDatabaseE $ do
        accId <- account
        (movedLocks <$> replacedAfterLock accId [replacingCoin])
          `dbShouldBeE` [(OutPoint (txHash unsettledPayment) 0, OutPoint (txHash raisedFeePayment) 0)]

    -- The sender takes the higher fee from the payment: 30000 - 1000 = 29000
    it "moves the lock though the replacement pays the address another amount" $
      inDatabaseE $ do
        accId <- account
        let paysLess = fundingTx 9 [(externalAddr 0, 29000), (otherAddr 0, 69000)]
            lesserCoin = storeCoin (txHash paysLess, 0) Nothing (externalAddr 0) 29000
        lift $ insertRawTx paysLess
        _ <- replacedAfterLock accId [lesserCoin]
        listed accId Nothing Nothing `dbShouldBeE` [(lesserCoin.outpoint, 0, Just LockedByUser)]

    -- The payment of the locked coin is dropped, and another payment, which
    -- spends other coins of its sender, arrives at the address
    it "leaves free the coin of a payment that replaces nothing, though it comes as the locked coin goes" $
      inDatabaseE $ do
        accId <- account
        let unrelated = fundingTx 8 [(externalAddr 0, 30000)]
            unrelatedCoin = storeCoin (txHash unrelated, 0) Nothing (externalAddr 0) 30000
        lift $ insertRawTx unrelated
        (movedLocks <$> replacedAfterLock accId [unrelatedCoin]) `dbShouldBeE` []
        listed accId Nothing Nothing `dbShouldBeE` [(unrelatedCoin.outpoint, 0, Nothing)]

    it "leaves free the coin of a replacement when the user had not locked the coin it replaces" $
      inDatabaseE $ do
        accId <- account
        lift $ mapM_ insertRawTx [unsettledPayment, raisedFeePayment]
        _ <- refresh accId [replaceableCoin]
        (movedLocks <$> refresh accId [replacingCoin]) `dbShouldBeE` []
        listed accId Nothing Nothing `dbShouldBeE` [(replacingCoin.outpoint, 0, Nothing)]

    it "locks a coin whose outpoint a pending transaction of the other chain spends" $
      inDatabaseE $ do
        accId <- account
        threeCoins accId
        spendingFirstOnCash
        (lockState <$> lockNamed firstCoin) `dbShouldBeE` (mainPlace, Just LockedByUser, False)
  where
    -- Where the coin is, its lock, and whether it can be spent now
    lockState (place, coin) = (place, jsonCoinLock coin, jsonCoinSpendable coin)
    -- The coins of 'unsettledPayment' and of 'raisedFeePayment' at the
    -- first receiving address, both waiting in the mempool
    replaceableCoin = storeCoin (txHash unsettledPayment, 0) Nothing (externalAddr 0) 30000
    replacingCoin = storeCoin (txHash raisedFeePayment, 0) Nothing (externalAddr 0) 30000
    -- The account once the user locked the coin of 'unsettledPayment' and
    -- the store lists the given coins in its place. The sync stores the
    -- raw transactions of the coins before the coins.
    replacedAfterLock accId coins = do
      lift $ mapM_ insertRawTx [unsettledPayment, raisedFeePayment]
      _ <- refresh accId [replaceableCoin]
      _ <- lockNamed replaceableCoin.outpoint
      refresh accId coins

{- The coins of both types of a pool -}

-- The pool has a confirmed coin of 50000 at the first receiving address
-- of its bip44 account, from transaction 1, and one of 30000 at the first
-- receiving address of its bip84 account, from transaction 2 a block later
{- Coins that hold tokens -}

-- | The account on Bitcoin Cash with its first receiving address, which
-- holds 'plainCashCoin' and 'tokenCoin'
cashAccountWithTokens :: ExceptT String (DB IO) DBAccountId
cashAccountWithTokens = do
  (cashId, _) <- testNewAccBch
  extendChainTo cashId AddrExternal 1
  _ <- refreshCoins bch cashId (const seenTime) [bchExternalAddr] [plainCashCoin, tokenCoin]
  return cashId

tokensSpec :: Spec
tokensSpec =
  describe "A coin of Bitcoin Cash that holds tokens" $ do
    let plain = OutPoint (txHash tokenFunding) 0
        withTokens = OutPoint (txHash tokenFunding) 1

    it "is a coin of its address, although a token prefix stands before its script" $
      Map.map (map (.outpoint) . Map.elems) (groupCoins bch [plainCashCoin, tokenCoin])
        `shouldBe` Map.fromList [(bchExternalAddrText, [plain, withTokens])]

    it "is no coin on Bitcoin, which has no tokens: the same bytes there are a script hw cannot sign for" $
      groupCoins btc [storeTokenCoin (fakeTxHash 5, 1) (Just 800000) (externalAddr 0) 30000] `shouldBe` Map.empty

    it "is listed by coins as locked by its tokens, beside a plain coin that is free" $
      inDatabaseE $ do
        cashId <- cashAccountWithTokens
        (sortOn fst3 <$> listed cashId Nothing Nothing) `dbShouldBeE` [(plain, 1, Nothing), (withTokens, 1, Just LockedByTokens)]

    it "cannot be spent, and the transaction builder is never offered it" $
      inDatabaseE $ do
        cashId <- cashAccountWithTokens
        (sortOn fst <$> spendableFlags cashId) `dbShouldBeE` [(plain, True), (withTokens, False)]
        spendable cashId 1 `dbShouldBeE` [plain]
        unspendable cashId 1 `dbShouldBeE` [(withTokens, Just LockedByTokens)]

    it "is left out of what the portfolio can spend" $
      inDatabaseE $ do
        cashId <- cashAccountWithTokens
        -- Of 50000 + 30000, the 30000 that hold tokens stay
        spendableOf cashId `dbShouldBeE` 50000

    it "counts in the balance of its address, in the total and as locked by tokens" $
      inDatabaseE $ do
        cashId <- cashAccountWithTokens
        -- Total 50000 + 30000 = 80000, spendable 50000, locked by tokens 30000
        (map snd <$> coinBalancesAfter UsualOrder bch [cashId] noTagFilter Nothing Nothing) `dbShouldBeE` [CoinBalance 80000 50000 0 0 0 30000]

    it "is refused by lockcoin, which says it needs no lock" $
      inDatabaseE $ do
        _ <- cashAccountWithTokens
        lift $
          dbShouldFailWith "The coin holds tokens (CashTokens), and hw never spends such a coin: it needs no lock" $
            lockCoin =<< walletCoin def (Just bch) withTokens

    it "is refused by unlockcoin, which says nothing unlocks it" $
      inDatabaseE $ do
        _ <- cashAccountWithTokens
        lift $
          dbShouldFailWith "The coin holds tokens (CashTokens), and nothing unlocks it: hw does not handle tokens and never spends such a coin. Move it with a wallet that handles the tokens of Bitcoin Cash" $
            unlockCoin =<< walletCoin def (Just bch) withTokens

    -- A row from a build that did not know tokens, or one gone wrong, says
    -- the coin holds none
    it "is locked again by a full sync, when its row no longer says that it holds tokens" $
      inDatabaseE $ do
        cashId <- cashAccountWithTokens
        lift $ P.updateWhere [DBCoinAccount P.==. cashId] [DBCoinTokens P.=. False]
        spendable cashId 1 `dbShouldBeE` [plain, withTokens]
        (changes <$> refreshCoins bch cashId (const seenTime) [bchExternalAddr] [plainCashCoin, tokenCoin]) `dbShouldBeE` (1, [], [])
        spendable cashId 1 `dbShouldBeE` [plain]

bothTypesSpec :: Spec
bothTypesSpec =
  describe "The coins of a pool with a bip44 and a bip84 account" $ do
    let segwitCoin = OutPoint (fakeTxHash 2) 0
        bothFunded = do
          (legacyId, _) <- testNewAcc
          (segwitId, _) <- testNewSegwitAcc
          extendChainTo legacyId AddrExternal 1
          extendChainTo segwitId AddrExternal 1
          _ <- refreshCoins btc legacyId (const seenTime) [externalAddr 0] [confirmedPayment]
          _ <- refreshCoins btc segwitId (const seenTime) [segwitExternalAddr 0] [storeCoin (fakeTxHash 2, 0) (Just 800001) (segwitExternalAddr 0) 30000]
          pool <- testPool
          return (pool, legacyId, segwitId)
    it "are offered to the builder together, whatever their type" $
      inDatabaseE $ do
        (pool, _, _) <- bothFunded
        (sort . map (.outpoint) <$> spendableCoins pool 1) `dbShouldBeE` [firstCoin, segwitCoin]

    it "are each offered at its own address: a P2PKH and a P2WPKH one" $
      inDatabaseE $ do
        (pool, _, _) <- bothFunded
        (sort . map (\coin -> (coin.outpoint, coin.address)) <$> spendableCoins pool 1)
          `dbShouldBeE` [(firstCoin, Just (externalAddr 0)), (segwitCoin, Just (segwitExternalAddr 0))]

    it "add up to what the pool can spend now" $
      inDatabaseE $ do
        (pool, _, _) <- bothFunded
        -- 50000 + 30000 = 80000
        (coinsSpendable <$> coinTotals pool) `dbShouldBeE` 80000

    it "are held back together when they cannot be spent, each with its lock" $
      inDatabaseE $ do
        (pool, _, _) <- bothFunded
        _ <- lockNamed segwitCoin
        lift $ P.insert_ $ pendingRow pool (spendingTx [(fakeTxHash 1, 0)] [(otherAddr 0, 49000)])
        (sortOn fst . map (\(coin, lock) -> (coin.outpoint, lock)) <$> unspendableCoins pool 1)
          `dbShouldBeE` [(firstCoin, Just LockedByTransaction), (segwitCoin, Just LockedByUser)]

    -- The newest block first: the bip84 coin is one block above
    it "are listed together in one order, each with the type and the path of its address" $
      inDatabaseE $ do
        (pool, legacyId, segwitId) <- bothFunded
        (map (\c -> (jsonCoinOutpoint c, jsonCoinPath c)) <$> coinsAfter pool [legacyId, segwitId] AllCoins Nothing Nothing)
          `dbShouldBeE` [ (segwitCoin, Just (PoolPath Bip84 (Deriv :/ 0 :/ 0))),
                          (firstCoin, Just (PoolPath Bip44 (Deriv :/ 0 :/ 0)))
                        ]

    it "are listed a page at a time across both accounts" $
      inDatabaseE $ do
        (pool, legacyId, segwitId) <- bothFunded
        (map jsonCoinOutpoint <$> coinsAfter pool [legacyId, segwitId] AllCoins Nothing (Just 1)) `dbShouldBeE` [segwitCoin]
        (map jsonCoinOutpoint <$> coinsAfter pool [legacyId, segwitId] AllCoins (Just (Store.BlockRef 800001 0, segwitCoin)) (Just 1))
          `dbShouldBeE` [firstCoin]

    -- The coin of the bip44 account is in block 800000, the coin of the
    -- bip84 account in block 800001
    it "are listed from the oldest across both accounts with --reverse, a page at a time" $
      inDatabaseE $ do
        (pool, legacyId, segwitId) <- bothFunded
        (map (jsonCoinOutpoint . fst) <$> listedCoinsAfter ReverseOrder pool [legacyId, segwitId] AllCoins noTagFilter Nothing (Just 1)) `dbShouldBeE` [firstCoin]
        (map (jsonCoinOutpoint . fst) <$> listedCoinsAfter ReverseOrder pool [legacyId, segwitId] AllCoins noTagFilter (Just (Store.BlockRef 800000 0, firstCoin)) (Just 1))
          `dbShouldBeE` [segwitCoin]

    it "are listed for one type alone when the listing is narrowed to it" $
      inDatabaseE $ do
        (pool, legacyId, segwitId) <- bothFunded
        (map jsonCoinOutpoint <$> coinsAfter pool [segwitId] AllCoins Nothing Nothing) `dbShouldBeE` [segwitCoin]
        (map jsonCoinOutpoint <$> coinsAfter pool [legacyId] AllCoins Nothing Nothing) `dbShouldBeE` [firstCoin]

    it "name the block of a coin of either account, for --after" $
      inDatabaseE $ do
        (_, legacyId, segwitId) <- bothFunded
        coinBlock [legacyId, segwitId] segwitCoin `dbShouldBeE` Store.BlockRef 800001 0
        coinBlock [legacyId, segwitId] firstCoin `dbShouldBeE` Store.BlockRef 800000 0

    it "give the balance of the coins at the addresses of both accounts, the largest first" $
      inDatabaseE $ do
        (_, legacyId, segwitId) <- bothFunded
        (map (\(addr, balance) -> (dBAddressAddress addr, balance)) <$> coinBalancesAfter UsualOrder btc [legacyId, segwitId] noTagFilter Nothing Nothing)
          `dbShouldBeE` [ ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", CoinBalance 50000 50000 0 0 0 0),
                          ("bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", CoinBalance 30000 30000 0 0 0 0)
                        ]

    -- 20000 at the first receiving address of each account
    it "list the addresses of both accounts with the same total in the order of the accounts, and go on after one of them" $
      inDatabaseE $ do
        (legacyId, _) <- testNewAcc
        (segwitId, _) <- testNewSegwitAcc
        extendChainTo legacyId AddrExternal 1
        extendChainTo segwitId AddrExternal 1
        _ <- refreshCoins btc legacyId (const seenTime) [externalAddr 0] [paidAt 4 (externalAddr 0) 20000]
        _ <- refreshCoins btc segwitId (const seenTime) [segwitExternalAddr 0] [paidAt 5 (segwitExternalAddr 0) 20000]
        both <- coinBalancesAfter UsualOrder btc [legacyId, segwitId] noTagFilter Nothing Nothing
        liftIO $ map (dBAddressAddress . fst) both `shouldBe` ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc"]
        (map (dBAddressAddress . fst) <$> coinBalancesAfter UsualOrder btc [legacyId, segwitId] noTagFilter (listToMaybe both) Nothing)
          `dbShouldBeE` ["bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc"]

{- A coin two test networks report -}

-- btctest and btctest4 share the coin type 1, so the bip44 accounts of a
-- portfolio on them have the same key and the same addresses. A store
-- that serves both the same chain reports the same coin on each: 50000
-- at their first receiving address, from transaction 1.
sharedKeySpec :: Spec
sharedKeySpec =
  describe "A coin that two test networks sharing a key both report" $ do
    let reported = storeCoin (fakeTxHash 1, 0) (Just 800000) testnetExternalAddr 50000
        -- hw shows every network here, but for the tests of the hidden ones
        shown = showingEveryNetwork def
        onBothTestnets = do
          portfolio <- testPortfolio
          (testnet3Id, _) <- accountOf portfolio btcTestnet3 Bip44
          (testnet4Id, _) <- accountOf portfolio btcTest4 Bip44
          _ <- refreshCoins btcTestnet3 testnet3Id (const seenTime) [testnetExternalAddr] [reported]
          _ <- refreshCoins btcTest4 testnet4Id (const seenTime) [testnetExternalAddr] [reported]
          return (testnet3Id, testnet4Id)
        -- The network and the account of the row a command takes
        taken :: (Network, DBCoin) -> (String, DBAccountId)
        taken (net, row) = (net.name, dBCoinAccount row)
    it "is kept in a row for each account, under the same outpoint" $
      inDatabaseE $ do
        (testnet3Id, testnet4Id) <- onBothTestnets
        (sort . map (\row -> (dBCoinAccount row, coinOutpoint row, dBCoinValue row)) <$> lift (coinRows firstCoin))
          `dbShouldBeE` [(testnet3Id, Right firstCoin, 50000), (testnet4Id, Right firstCoin, 50000)]

    it "is refreshed on one network without touching its row on the other" $
      inDatabaseE $ do
        (testnet3Id, testnet4Id) <- onBothTestnets
        -- The store of btctest no longer lists the coin
        (changes <$> refreshCoins btcTestnet3 testnet3Id (const seenTime) [testnetExternalAddr] []) `dbShouldBeE` (1, [], [firstCoin])
        (map dBCoinAccount <$> lift (coinRows firstCoin)) `dbShouldBeE` [testnet4Id]

    it "is held by both accounts" $
      inDatabaseE $ do
        (testnet3Id, testnet4Id) <- onBothTestnets
        (sort . map fst <$> coinHolders [firstCoin]) `dbShouldBeE` [testnet3Id, testnet4Id]

    it "names the row on the --network when a command names the coin" $
      inDatabaseE $ do
        (_, testnet4Id) <- onBothTestnets
        (taken <$> walletCoin shown (Just btcTest4) firstCoin) `dbShouldBeE` ("btctest4", testnet4Id)

    it "names the row on the default network without --network" $
      inDatabaseE $ do
        (testnet3Id, _) <- onBothTestnets
        (taken <$> walletCoin shown {configDefaultNetwork = btcTestnet3} Nothing firstCoin) `dbShouldBeE` ("btctest", testnet3Id)

    it "takes --network over the default network" $
      inDatabaseE $ do
        (_, testnet4Id) <- onBothTestnets
        (taken <$> walletCoin shown {configDefaultNetwork = btcTestnet3} (Just btcTest4) firstCoin) `dbShouldBeE` ("btctest4", testnet4Id)

    -- The default network is btc, where the coin is not
    it "is refused without --network when the default network has no row of it, naming the networks that have" $
      inDatabaseE $ do
        _ <- onBothTestnets
        lift $
          dbShouldFailWith "The coin 0000000000000000000000000000000000000000000000000000000000000001:0 is on btctest and btctest4: give the --network" $
            walletCoin shown Nothing firstCoin

    it "is refused on a --network that has no row of it, naming the networks that have" $
      inDatabaseE $ do
        _ <- onBothTestnets
        lift $
          dbShouldFailWith "The coin 0000000000000000000000000000000000000000000000000000000000000001:0 is on btctest and btctest4, not on the --network btc" $
            walletCoin shown (Just btc) firstCoin

    -- The coin is on btctest alone, and the default network is btc
    it "names its one row without --network when one network alone has it, whatever the default network" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (testnet3Id, _) <- accountOf portfolio btcTestnet3 Bip44
        _ <- refreshCoins btcTestnet3 testnet3Id (const seenTime) [testnetExternalAddr] [reported]
        (taken <$> walletCoin shown Nothing firstCoin) `dbShouldBeE` ("btctest", testnet3Id)

    -- hw shows btc, bch and btctest4: btctest, which has the coin too,
    -- is hidden, and is no candidate. The default network is btc.
    it "names its row on the one network hw shows that has it, without --network, though a hidden network has it too" $
      inDatabaseE $ do
        (_, testnet4Id) <- onBothTestnets
        (taken <$> walletCoin def {configShownNetworks = [btc, bch, btcTest4]} Nothing firstCoin) `dbShouldBeE` ("btctest4", testnet4Id)

    -- The default configuration shows btc and bch
    it "is refused when the networks that have it are all hidden, naming them, with the command that shows one" $
      inDatabaseE $ do
        _ <- onBothTestnets
        lift $
          dbShouldFailWith "The coin 0000000000000000000000000000000000000000000000000000000000000001:0 is on btctest and btctest4, which are hidden. To show one of them, run for instance: hw setdefault --show-network btctest" $
            walletCoin def Nothing firstCoin

    it "is locked by the user on the network named alone" $
      inDatabaseE $ do
        (testnet3Id, testnet4Id) <- onBothTestnets
        _ <- lockCoin =<< walletCoin shown (Just btcTest4) firstCoin
        (map coinOutpoint <$> lift (userLockedCoins testnet4Id)) `dbShouldBeE` [Right firstCoin]
        lift (userLockedCoins testnet3Id) `dbShouldBeE` []

    it "stays spendable on the other network when the user locks it on one" $
      inDatabaseE $ do
        _ <- onBothTestnets
        portfolio <- testPortfolio
        _ <- lockCoin =<< walletCoin shown (Just btcTest4) firstCoin
        (map (.outpoint) <$> spendableCoins (portfolioPool portfolio btcTest4) 1) `dbShouldBeE` []
        (map (.outpoint) <$> spendableCoins (portfolioPool portfolio btcTestnet3) 1) `dbShouldBeE` [firstCoin]

    it "is shown where it is: in the portfolio, on the network of its row" $
      inDatabaseE $ do
        _ <- onBothTestnets
        (place, coin) <- shownCoin =<< walletCoin shown (Just btcTest4) firstCoin
        liftIO $ (placeNetwork place).name `shouldBe` "btctest4"
        liftIO $ (jsonCoinOutpoint coin, jsonCoinValue coin) `shouldBe` (firstCoin, 50000)

{- The life of a coin -}

lifeSpec :: Spec
lifeSpec =
  describe "The life of a coin" $
    it "arrives unconfirmed, confirms, is held by a payment, and goes once spent" $
      inDatabaseE $ do
        accId <- account
        -- A payment waits in the mempool: listed, but not offered, since
        -- whoever pays can still replace it
        bestAt 799999
        _ <- refresh accId [payment]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 0, Nothing)]
        spendable accId 0 `dbShouldBeE` []
        -- It confirms in the next block
        bestAt 800000
        _ <- refresh accId [confirmedPayment]
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Nothing)]
        spendable accId 1 `dbShouldBeE` [firstCoin]
        -- A payment of the wallet spends it: held, offered no more
        spendingFirst accId
        listed accId Nothing Nothing `dbShouldBeE` [(firstCoin, 1, Just LockedByTransaction)]
        spendable accId 0 `dbShouldBeE` []
        -- The payment is mined: the store no longer lists the coin
        bestAt 800001
        (changes <$> refresh accId []) `dbShouldBeE` (1, [], [firstCoin])
        listed accId Nothing Nothing `dbShouldBeE` []

{- A coin row -}

-- The first coin is the account's, at its first receiving address; 20000
-- at the first receiving address of the second test wallet is another
-- account's; the third coin is none of the wallet
holdersSpec :: Spec
holdersSpec =
  describe "The accounts that hold coins" $
    it "are those of the coins the wallet holds, each once" $
      inDatabaseE $ do
        accId <- account
        (otherId, _) <- testNewAcc2
        _ <- refresh accId [confirmedPayment]
        _ <- refreshCoins btc otherId (const seenTime) [externalAddr2] [storeCoin (fakeTxHash 2, 0) (Just 800001) externalAddr2 20000]
        (map fst <$> coinHolders [firstCoin, secondCoin, firstCoin, thirdCoin]) `dbShouldBeE` [accId, otherId]

-- lockcoin and --coin take a coin by the short id of its txid and its
-- index: a1075db5:1
shortIdSpec :: Spec
shortIdSpec =
  describe "The coins named by a short id and an index" $ do
    it "are the coins at that index of the transactions with the short id, in every account" $
      inDatabaseE $ do
        accId <- account
        (otherId, _) <- testNewAcc2
        _ <-
          refresh
            accId
            [ storeCoin (a1075db5First, 0) (Just 800000) (externalAddr 0) 10000,
              storeCoin (a1075db5First, 1) (Just 800000) (externalAddr 1) 20000,
              storeCoin (b2d4f6a8Txid, 1) (Just 800000) (externalAddr 0) 30000
            ]
        _ <- refreshCoins btc otherId (const seenTime) [externalAddr2] [storeCoin (a1075db5Second, 1) (Just 800000) externalAddr2 40000]
        (sort . map coinText <$> lift (coinsWith (typedShortId "a1075db5") 1))
          `dbShouldBeE` [ "a1075db5c0ffee00000000000000000000000000000000000000000000000001:1",
                          "a1075db5c0ffee00000000000000000000000000000000000000000000000002:1"
                        ]

    it "are none when no transaction with the short id has a coin at that index" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [storeCoin (a1075db5First, 0) (Just 800000) (externalAddr 0) 10000]
        lift (coinsWith (typedShortId "a1075db5") 1) `dbShouldBeE` []

rowSpec :: Spec
rowSpec =
  describe "A coin row" $ do
    -- The store saw transaction 1 at seenTime
    it "shows as coins lists it: outpoint, address, value, block, confirmations from the best block, lock, whether it can be spent, the path of its address and the time of its transaction" $
      inDatabaseE $ do
        accId <- account
        extendChainTo accId AddrExternal 1
        summarise accId firstPaymentTx
        refreshed <- refresh accId [confirmedPayment]
        row <- liftMaybe "The coin is not stored" $ listToMaybe (insertedCoins refreshed)
        pool <- poolOf accId
        let rules = Spending btc (Just 800002) Map.empty
            shown = JsonCoin firstCoin (externalAddr 0) 50000 (Store.BlockRef 800000 0) 3 Nothing True "" "" [] False (Just (legacyPath (Deriv :/ 0 :/ 0))) (Just seenTime)
        toJsonCoin rules pool row `dbShouldBeE` shown
        toJsonCoin rules {spendingLocks = Map.fromList [(firstCoin, LockedByTransaction)]} pool row
          `dbShouldBeE` shown {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}
        toJsonCoin rules {spendingLocks = Map.fromList [(firstCoin, LockedByUser)]} pool row
          `dbShouldBeE` shown {jsonCoinLock = Just LockedByUser, jsonCoinSpendable = False}
        toJsonCoin rules {spendingBest = Nothing} pool row `dbShouldBeE` shown {jsonCoinConfirmations = 1}

    it "keeps its outpoint, read back from its key" $
      inDatabaseE $ do
        accId <- account
        refreshed <- refresh accId [secondPayment]
        liftIO $ map coinOutpoint (insertedCoins refreshed) `shouldBe` [Right secondCoin]

    it "refuses a key that does not read as an outpoint" $
      inDatabaseE $ do
        accId <- account
        let row = (paymentRow accId) {dBCoinOutpoint = KeyBytes "nonsense"}
        liftIO $ coinOutpoint row `shouldBe` Left "A stored coin has an outpoint that does not read"

    it "is grouped with the others of its address, as the store lists them" $ do
      let grouped = Map.map (map (.outpoint) . Map.elems) (groupCoins btc [payment, secondPayment, thirdPayment, unaddressed])
      grouped
        `shouldBe` Map.fromList
          [ ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", [firstCoin]),
            ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", [secondCoin, thirdCoin])
          ]

    it "is left out of the groups when its script is not the standard script of its address" $
      Map.map (map (.outpoint) . Map.elems) (groupCoins btc [oddlyWrittenPayment, secondPayment])
        `shouldBe` Map.fromList [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", [secondCoin])]

    it "is left out of the groups when the network cannot write its address" $ do
      let segwit = storeCoin (fakeTxHash 6, 0) Nothing segwitAddr 1000
          cash = storeCoin (fakeTxHash 5, 0) Nothing bchExternalAddr 1000
      Map.map (map (.outpoint) . Map.elems) (groupCoins bch [segwit, cash])
        `shouldBe` Map.fromList [(bchExternalAddrText, [OutPoint (fakeTxHash 5) 0])]

{- JSON -}

jsonSpec :: Spec
jsonSpec =
  describe "Coins in JSON" $ do
    it "writes a confirmed and spendable coin as coins prints it, and reads it back" $ do
      let coin = JsonCoin firstCoin (externalAddr 0) 50000 (Store.BlockRef 800000 0) 3 Nothing True "" "" [] False (Just (legacyPath (Deriv :/ 0 :/ 0))) (Just seenTime)
          json :: String
          json =
            [r|{
              "outpoint": {"txid": "0000000000000000000000000000000000000000000000000000000000000001", "index": 0},
              "address": "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
              "value": 50000,
              "block": {"height": 800000, "position": 0},
              "confirmations": 3,
              "lock": null,
              "spendable": true,
              "txlabel": "",
              "addresslabel": "",
              "addresstags": [],
              "change": false,
              "path": "/0/0",
              "type": "bip44",
              "time": "2023-11-14T22:13:20Z"
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc coin)
      unmarshalJSON btc (cs json) `shouldBe` Just coin

    it "writes an unconfirmed and locked coin of Bitcoin Cash with its cash address" $ do
      let coin = JsonCoin (OutPoint (fakeTxHash 5) 1) bchExternalAddr 10000 (Store.MemRef 0) 0 (Just LockedByTransaction) False "" "" [] False (Just (legacyPath (Deriv :/ 0 :/ 0))) Nothing
          json :: String
          json =
            [r|{
              "outpoint": {"txid": "0000000000000000000000000000000000000000000000000000000000000005", "index": 1},
              "address": "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u",
              "value": 10000,
              "block": {"mempool": 0},
              "confirmations": 0,
              "lock": "transaction",
              "spendable": false,
              "txlabel": "",
              "addresslabel": "",
              "addresstags": [],
              "change": false,
              "path": "/0/0",
              "type": "bip44",
              "time": null
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue bch coin)
      unmarshalJSON bch (cs json) `shouldBe` Just coin

    it "names the lock of a pending replacement, and reads it back" $ do
      Json.toJSON LockedByReplacement `shouldBe` Json.String "replacement"
      Json.fromJSON (Json.String "replacement") `shouldBe` Json.Success LockedByReplacement

{- The coins a transaction leaves -}

-- | A confirmed coin of the test wallet: an output of a fake transaction, at
-- the external address of the same index
unspentAt :: Word8 -> Word32 -> Natural -> Store.Unspent
unspentAt w i = storeCoin (fakeTxHash w, i) (Just 800000) (externalAddr (fromIntegral i))

leftSpec :: Spec
leftSpec =
  describe "The coins a transaction leaves in the portfolio" $ do
    it "are none when it leaves none" $
      leftOutpoints [] [] [] `shouldBe` []

    -- The dust coins left, the coins the builder may not spend, and a coin
    -- not chosen with coin control
    it "are listed by reason, the largest coins first" $
      leftOutpoints
        [unspentAt 2 0 900, unspentAt 2 1 1200]
        [(unspentAt 3 0 5000, Nothing), (unspentAt 3 1 20000, Just LockedByTransaction), (unspentAt 3 2 30000, Just LockedByUser)]
        [unspentAt 4 0 40000]
        `shouldBe` [ (OutPoint (fakeTxHash 2) 1, WorthLessThanFee),
                     (OutPoint (fakeTxHash 2) 0, WorthLessThanFee),
                     (OutPoint (fakeTxHash 3) 2, Locked LockedByUser),
                     (OutPoint (fakeTxHash 3) 1, Locked LockedByTransaction),
                     (OutPoint (fakeTxHash 3) 0, NotSpendableYet),
                     (OutPoint (fakeTxHash 4) 0, NotGiven)
                   ]

    it "tell the user why each stays" $
      (leftReasonText [] <$> [WorthLessThanFee, Locked LockedByUser, Locked LockedByTag, Locked LockedByTransaction, NotSpendableYet, NotGiven])
        `shouldBe` ["worth less than its fee", "you locked it", "a tag of its address locks it", "a pending transaction spends it", "not spendable yet", "not chosen with --coin, --spend-from or --spend-from-tag"]

    it "name the tag that locks a coin" $
      leftReasonText [kycTag] (Locked LockedByTag) `shouldBe` "the tag kyc locks it"

    it "name every tag that locks a coin, when two do" $
      leftReasonText [coldTag, kycTag] (Locked LockedByTag) `shouldBe` "the tags cold and kyc lock it"

    -- The pending transaction does not spend the coin: it replaces the
    -- payment that made it, and the coin goes with that payment
    it "tell the user that a pending transaction replaces the payment a coin comes from" $
      leftReasonText [] (Locked LockedByReplacement) `shouldBe` "a pending transaction replaces the payment it comes from"

    -- A send-all with --coin-type bip84 leaves the bip44 coins, and the
    -- other way round
    it "are the spendable coins of the type --coin-type leaves out, the largest first, with the reason othertype" $
      leftOfOtherType [unspentAt 5 0 5000, unspentAt 5 1 30000]
        `shouldBe` [ (OutPoint (fakeTxHash 5) 1, OtherType),
                     (OutPoint (fakeTxHash 5) 0, OtherType)
                   ]

    it "are each shown as coins shows it, with the label of the payment that paid it, and why it stays" $
      inDatabaseE $ do
        accId <- account
        _ <- refresh accId [confirmedPayment]
        summarise accId firstPaymentTx
        labelTx accId (fakeTxHash 1) "Salary"
        pool <- poolOf accId
        left <- leftCoinsOf pool [] [(firstCoin, OtherType)]
        liftIO $
          [(jsonCoinOutpoint coin.leftCoin, jsonCoinValue coin.leftCoin, jsonCoinTxLabel coin.leftCoin, coin.leftCoinReason) | coin <- left]
            `shouldBe` [(firstCoin, 50000, "Salary", OtherType)]

    it "tell the user that a coin of the other type stays for its type, and name that reason othertype in JSON" $ do
      leftReasonText [] OtherType `shouldBe` "not of the type --coin-type sends"
      Json.toJSON OtherType `shouldBe` Json.String "othertype"
      Json.fromJSON (Json.String "othertype") `shouldBe` Json.Success OtherType

    it "list the coins of the other type after every other reason" $
      leftReasons `shouldBe` [WorthLessThanFee, Locked LockedByTokens, Locked LockedByUser, Locked LockedByTag, Locked LockedByTransaction, Locked LockedByReplacement, NotSpendableYet, NotGiven, OtherType]

    it "are written in JSON as coins writes the coin, with why they stay, and read back" $ do
      let left = LeftCoin (JsonCoin firstCoin (externalAddr 0) 50000 (Store.BlockRef 800000 0) 3 Nothing True "" "" [] False (Just (legacyPath (Deriv :/ 0 :/ 0))) (Just seenTime)) WorthLessThanFee []
          json :: String
          json =
            [r|{
              "outpoint": {"txid": "0000000000000000000000000000000000000000000000000000000000000001", "index": 0},
              "address": "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
              "value": 50000,
              "block": {"height": 800000, "position": 0},
              "confirmations": 3,
              "lock": null,
              "spendable": true,
              "txlabel": "",
              "addresslabel": "",
              "addresstags": [],
              "change": false,
              "path": "/0/0",
              "type": "bip44",
              "time": "2023-11-14T22:13:20Z",
              "reason": "dust",
              "lockingtags": []
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc left)
      unmarshalJSON btc (cs json) `shouldBe` Just left

    it "name each reason in JSON" $
      (Json.toJSON <$> [WorthLessThanFee, Locked LockedByUser, Locked LockedByTag, Locked LockedByTransaction, Locked LockedByReplacement, NotSpendableYet, NotGiven])
        `shouldBe` (Json.String <$> ["dust", "lockedbyuser", "lockedbytag", "lockedbytransaction", "lockedbyreplacement", "notspendableyet", "notgiven"])

    it "read each reason back from its JSON name" $
      (Json.fromJSON . Json.String <$> ["dust", "lockedbyuser", "lockedbytag", "lockedbytransaction", "lockedbyreplacement", "notspendableyet", "notgiven"])
        `shouldBe` (Json.Success <$> [WorthLessThanFee, Locked LockedByUser, Locked LockedByTag, Locked LockedByTransaction, Locked LockedByReplacement, NotSpendableYet, NotGiven])

    it "refuse a reason they do not know" $
      (Json.fromJSON (Json.String "lost") :: Json.Result LeftReason)
        `shouldBe` Json.Error "Unknown reason a coin stays in the portfolio: lost"

{- Coin control -}

-- The builder may spend 50000 at /0/0 and 30000 at /0/1. It may not spend
-- 20000 at /0/0, which the user locked, nor 5000 at /0/1, not confirmed
-- enough.
chosenSpec :: Spec
chosenSpec =
  describe "The coins that coin control chooses" $ do
    it "are the coins given by outpoint" $
      choose [OutPoint (fakeTxHash 1) 0] [] `shouldBe` Right ([fifty], [])

    it "are every coin at an address given, those the builder may not spend with their locks" $
      choose [] [externalAddr 0] `shouldBe` Right ([fifty], [(twentyLocked, Just LockedByUser)])

    it "are the coins of the outpoints and of the addresses together" $
      choose [OutPoint (fakeTxHash 1) 1] [externalAddr 0] `shouldBe` Right ([fifty, thirty], [(twentyLocked, Just LockedByUser)])

    it "keep a coin given by outpoint that the builder may not spend, with why" $
      choose [OutPoint (fakeTxHash 1) 0, OutPoint (fakeTxHash 2) 1] [] `shouldBe` Right ([fifty], [(fiveUnconfirmed, Nothing)])

    it "are refused an outpoint the portfolio does not hold on the network" $
      choose [OutPoint (fakeTxHash 9) 0] []
        `shouldBe` Left "The portfolio has no coin 0000000000000000000000000000000000000000000000000000000000000009:0 on the network of the payment"

    it "are refused when none can be spent, with why" $
      choose [OutPoint (fakeTxHash 2) 0, OutPoint (fakeTxHash 2) 1] []
        `shouldBe` Left "None of the coins chosen can be spent now: 0000000000000000000000000000000000000000000000000000000000000002:0, you locked it; 0000000000000000000000000000000000000000000000000000000000000002:1, not spendable yet"

    it "are refused when the addresses given hold no coins" $
      choose [] [externalAddr 5] `shouldBe` Left "The addresses chosen with --spend-from or --spend-from-tag hold no coins"

    -- The coin of 20000 is at an address of the tag kyc, which locks
    it "are refused a coin that a tag locks, with the tag and the option that opens its lock" $
      chosenCoins [OutPoint (fakeTxHash 2) 0] [] (Map.fromList [(OutPoint (fakeTxHash 2) 0, [kycTag])]) [fifty] [(twentyLocked, Just LockedByTag)]
        `shouldBe` Left "None of the coins chosen can be spent now: 0000000000000000000000000000000000000000000000000000000000000002:0, the tag kyc locks it. Only a payment that names the tag spends its coins: --spend-from-tag kyc"

    -- The address of the coin has two tags that lock, and the payment
    -- named neither
    it "are refused a coin that two tags lock, with both and the options that open their locks" $
      chosenCoins [OutPoint (fakeTxHash 2) 0] [] (Map.fromList [(OutPoint (fakeTxHash 2) 0, [coldTag, kycTag])]) [fifty] [(twentyLocked, Just LockedByTag)]
        `shouldBe` Left "None of the coins chosen can be spent now: 0000000000000000000000000000000000000000000000000000000000000002:0, the tags cold and kyc lock it. Only a payment that names these tags spends their coins: --spend-from-tag cold --spend-from-tag kyc"
  where
    fifty = unspentAt 1 0 50000
    thirty = unspentAt 1 1 30000
    twentyLocked = unspentAt 2 0 20000
    fiveUnconfirmed = unspentAt 2 1 5000
    choose outpoints addrs = chosenCoins outpoints addrs Map.empty [fifty, thirty] [(twentyLocked, Just LockedByUser), (fiveUnconfirmed, Nothing)]
