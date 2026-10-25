{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The coins of an account: the unspent outputs the store lists at its
-- addresses, kept in step by the sync, listed by coins with their
-- confirmations and locks, and offered to the transaction builder when
-- they are confirmed enough and no pending transaction holds them. The
-- account of these tests is account 0 of the test wallet on Bitcoin.
module Haskoin.Wallet.CoinSpec (spec) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.BestBlock (updateBest)
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Util (Page (Page))
import Numeric.Natural (Natural)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx -> do
  refreshingSpec ctx
  listingSpec ctx
  spendableSpec ctx
  lifeSpec ctx
  rowSpec ctx
  jsonSpec ctx

{- Fixtures -}

-- | The account of these tests
account :: Ctx -> ExceptT String (DB IO) DBAccountId
account ctx = fst <$> testNewAcc ctx "main"

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
threeCoins ctx accId = do
  _ <- refresh accId [confirmedPayment ctx, secondPayment ctx, thirdPayment ctx]
  return ()

-- | Refresh the coins of the first two receiving addresses with what the
-- store lists, as the sync does when their balances changed
refresh :: DBAccountId -> [Store.Unspent] -> ExceptT String (DB IO) Refreshed
refresh accId = refreshCoins btc accId [externalAddr 0, externalAddr 1]

-- | What a refresh changed: how many coins, and the outpoints of those it
-- inserted and of those it deleted
changes :: Refreshed -> (Int, [OutPoint], [OutPoint])
changes refreshed = (refreshedCount refreshed, sort (rowOutpoint <$> insertedCoins refreshed), sort (rowOutpoint <$> deletedCoins refreshed))

rowOutpoint :: DBCoin -> OutPoint
rowOutpoint = forceRight . coinOutpoint

-- | The coins of the account as coins lists them: outpoint, confirmations
-- and lock
listed :: DBAccountId -> Page -> ExceptT String (DB IO) [(OutPoint, Natural, Bool)]
listed accId page = map (\c -> (jsonCoinOutpoint c, jsonCoinConfirmations c, jsonCoinLocked c)) <$> coinPage btc accId page

-- | The outpoints of the coins offered to the builder at a number of
-- confirmations
spendable :: DBAccountId -> Natural -> ExceptT String (DB IO) [OutPoint]
spendable accId minConf = sort . map (.outpoint) <$> getSpendableCoins btc accId minConf

-- | The best block of Bitcoin at a height
bestAt :: BlockHeight -> ExceptT String (DB IO) ()
bestAt height = lift $ updateBest btc (fakeBlockHash 1) height

-- | A pending transaction of the account spending the first payment
spendingFirst :: Ctx -> DBAccountId -> ExceptT String (DB IO) ()
spendingFirst ctx accId = lift $ P.insert_ $ pendingRow accId (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 49000)])

{- Refreshing -}

refreshingSpec :: Ctx -> Spec
refreshingSpec ctx =
  describe "Refreshing the coins of addresses, as the sync does" $ do
    it "stores a new coin with its account, address, value and block" $
      inDatabaseE $ do
        accId <- account ctx
        (changes <$> refresh accId [payment ctx]) `dbShouldBeE` (1, [firstCoin], [])
        rows <- lift $ getCoinsByAddr "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        liftIO $ do
          map (\c -> (dBCoinAccount c, dBCoinValue c, S.decode (dBCoinBlockRef c))) rows `shouldBe` [(accId, 50000, Right (Store.MemRef 0))]
          map (S.decode . dBCoinBlob) rows `shouldBe` [Right (payment ctx)]

    it "changes nothing when the store lists the same coins again" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh accId [payment ctx]
        (changes <$> refresh accId [payment ctx]) `dbShouldBeE` (0, [], [])

    it "moves a coin to the block that confirms it, as the store reports it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh accId [payment ctx]
        (changes <$> refresh accId [confirmedPayment ctx]) `dbShouldBeE` (1, [], [])
        rows <- lift $ getCoinsByAddr "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
        liftIO $ map (S.decode . dBCoinBlockRef) rows `shouldBe` [Right (Store.BlockRef 800000 0)]
        getSpendableCoins btc accId 0 `dbShouldBeE` [confirmedPayment ctx]

    it "deletes a coin the store no longer lists, as once it is spent" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh accId [confirmedPayment ctx]
        (changes <$> refresh accId []) `dbShouldBeE` (1, [], [firstCoin])
        lift (getCoinsByAddr "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") `dbShouldBeE` []

    it "keeps the coins of one address apart, deleting only the one that went" $
      inDatabaseE $ do
        accId <- account ctx
        (changes <$> refresh accId [secondPayment ctx, thirdPayment ctx]) `dbShouldBeE` (2, [secondCoin, thirdCoin], [])
        (changes <$> refresh accId [thirdPayment ctx]) `dbShouldBeE` (1, [], [secondCoin])
        (map rowOutpoint <$> lift (getCoinsByAddr "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg")) `dbShouldBeE` [thirdCoin]

    it "leaves alone the addresses it is not given, whatever the store lists for them" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh accId [secondPayment ctx]
        -- The second address is not refreshed: its coin stays, and the new
        -- one the store lists there is not stored
        (changes <$> refreshCoins btc accId [externalAddr 0] [payment ctx, thirdPayment ctx]) `dbShouldBeE` (1, [firstCoin], [])
        spendable accId 0 `dbShouldBeE` [firstCoin, secondCoin]

    it "passes over a coin the store lists without an address" $
      inDatabaseE $ do
        accId <- account ctx
        (changes <$> refresh accId [unaddressed]) `dbShouldBeE` (0, [], [])

{- Listing -}

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "Listing the coins of an account, as coins does" $ do
    it "lists the unconfirmed coins first, then the confirmed ones from the newest block" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        (map fst3 <$> listed accId (Page 10 0)) `dbShouldBeE` [thirdCoin, secondCoin, firstCoin]

    it "lists a page at a time" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        (map fst3 <$> listed accId (Page 1 1)) `dbShouldBeE` [secondCoin]
        (map fst3 <$> listed accId (Page 2 2)) `dbShouldBeE` [firstCoin]
        listed accId (Page 2 3) `dbShouldBeE` []

    it "counts the confirmations from the best block, and none in the mempool" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        bestAt 800002
        listed accId (Page 10 0) `dbShouldBeE` [(thirdCoin, 0, False), (secondCoin, 2, False), (firstCoin, 3, False)]

    it "counts one confirmation for a confirmed coin while the best block is unknown or behind it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh accId [confirmedPayment ctx]
        listed accId (Page 10 0) `dbShouldBeE` [(firstCoin, 1, False)]
        bestAt 799990
        listed accId (Page 10 0) `dbShouldBeE` [(firstCoin, 1, False)]

    it "counts from the best block of the account's network, not another's" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh accId [confirmedPayment ctx]
        lift $ updateBest bch (fakeBlockHash 2) 900000
        listed accId (Page 10 0) `dbShouldBeE` [(firstCoin, 1, False)]

    it "shows a coin a pending transaction spends as locked, and the others as free" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        (map (\(o, _, l) -> (o, l)) <$> listed accId (Page 10 0)) `dbShouldBeE` [(thirdCoin, False), (secondCoin, False), (firstCoin, True)]

    it "lists the coins of the account only" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- refresh accId [confirmedPayment ctx]
        _ <- refreshCoins btc otherId [externalAddr2] [storeCoin ctx (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        (map fst3 <$> listed accId (Page 10 0)) `dbShouldBeE` [firstCoin]
        (map fst3 <$> listed otherId (Page 10 0)) `dbShouldBeE` [OutPoint (fakeTxHash 4) 0]

    it "reads the address of a Bitcoin Cash coin on its network" $
      inDatabaseE $ do
        (accId, _) <- testNewAccBch ctx "cash"
        _ <- refreshCoins bch accId [bchExternalAddr] [storeCoin ctx (fakeTxHash 5, 0) (Just 800000) bchExternalAddr 10000]
        (map jsonCoinAddress <$> coinPage bch accId (Page 10 0)) `dbShouldBeE` [bchExternalAddr]

    it "refuses a stored coin whose address does not read, rather than leaving it out" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ P.insert_ $ DBCoin accId (outpointBytes firstCoin) "nonsense" 50000 (S.encode (Store.BlockRef 800000 0)) (S.encode (confirmedPayment ctx)) fixtureTime
        lift $ dbShouldFailWith "toJsonCoin: Invalid address" $ coinPage btc accId (Page 10 0)

{- Spendable -}

spendableSpec :: Ctx -> Spec
spendableSpec ctx =
  describe "The coins offered to the transaction builder" $ do
    it "offers every coin at zero confirmations, the unconfirmed ones included" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        bestAt 800002
        spendable accId 0 `dbShouldBeE` [firstCoin, secondCoin, thirdCoin]

    it "offers the coins with at least the confirmations asked" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        -- 800002 is the third block for the first payment, the second for
        -- the second payment
        bestAt 800002
        spendable accId 1 `dbShouldBeE` [firstCoin, secondCoin]
        spendable accId 2 `dbShouldBeE` [firstCoin, secondCoin]
        spendable accId 3 `dbShouldBeE` [firstCoin]
        spendable accId 4 `dbShouldBeE` []

    it "counts one confirmation for a confirmed coin while the best block is unknown" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendable accId 1 `dbShouldBeE` [firstCoin, secondCoin]
        spendable accId 2 `dbShouldBeE` []

    it "holds back a coin a pending transaction spends" $
      inDatabaseE $ do
        accId <- account ctx
        threeCoins ctx accId
        spendingFirst ctx accId
        spendable accId 0 `dbShouldBeE` [secondCoin, thirdCoin]

    it "offers each coin as the store last reported it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- refresh accId [secondPayment ctx]
        getSpendableCoins btc accId 1 `dbShouldBeE` [secondPayment ctx]

    it "offers the coins of the account only" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- refresh accId [confirmedPayment ctx]
        _ <- refreshCoins btc otherId [externalAddr2] [storeCoin ctx (fakeTxHash 4, 0) (Just 800000) externalAddr2 10000]
        spendable accId 0 `dbShouldBeE` [firstCoin]
        spendable otherId 0 `dbShouldBeE` [OutPoint (fakeTxHash 4) 0]

{- The life of a coin -}

lifeSpec :: Ctx -> Spec
lifeSpec ctx =
  describe "The life of a coin" $
    it "arrives unconfirmed, confirms, is held by a payment, and goes once spent" $
      inDatabaseE $ do
        accId <- account ctx
        -- A payment waits in the mempool: listed, offered only to whoever
        -- accepts unconfirmed coins
        bestAt 799999
        _ <- refresh accId [payment ctx]
        listed accId (Page 10 0) `dbShouldBeE` [(firstCoin, 0, False)]
        spendable accId 0 `dbShouldBeE` [firstCoin]
        spendable accId 1 `dbShouldBeE` []
        -- It confirms in the next block
        bestAt 800000
        _ <- refresh accId [confirmedPayment ctx]
        listed accId (Page 10 0) `dbShouldBeE` [(firstCoin, 1, False)]
        spendable accId 1 `dbShouldBeE` [firstCoin]
        -- A payment of the wallet spends it: held, offered no more
        spendingFirst ctx accId
        listed accId (Page 10 0) `dbShouldBeE` [(firstCoin, 1, True)]
        spendable accId 0 `dbShouldBeE` []
        -- The payment is mined: the store no longer lists the coin
        bestAt 800001
        (changes <$> refresh accId []) `dbShouldBeE` (1, [], [firstCoin])
        listed accId (Page 10 0) `dbShouldBeE` []

{- A coin row -}

rowSpec :: Ctx -> Spec
rowSpec ctx =
  describe "A coin row" $ do
    it "shows as coins lists it: outpoint, address, value, block, confirmations from the best block, and lock" $
      inDatabaseE $ do
        accId <- account ctx
        refreshed <- refresh accId [confirmedPayment ctx]
        row <- liftMaybe "The coin is not stored" $ listToMaybe (insertedCoins refreshed)
        let shown = JsonCoin firstCoin (externalAddr 0) 50000 (Store.BlockRef 800000 0) 3 False
        liftIO $ do
          toJsonCoin btc (Just 800002) Set.empty row `shouldBe` Right shown
          toJsonCoin btc (Just 800002) (Set.fromList [firstCoin]) row `shouldBe` Right shown {jsonCoinLocked = True}
          toJsonCoin btc Nothing Set.empty row `shouldBe` Right shown {jsonCoinConfirmations = 1}

    it "keeps its outpoint, read back from its key" $
      inDatabaseE $ do
        accId <- account ctx
        refreshed <- refresh accId [secondPayment ctx]
        liftIO $ map coinOutpoint (insertedCoins refreshed) `shouldBe` [Right secondCoin]

    it "is grouped with the others of its address, as the store lists them" $ do
      let grouped = Map.map (sort . map (.outpoint)) (groupCoins btc [payment ctx, secondPayment ctx, thirdPayment ctx, unaddressed])
      grouped
        `shouldBe` Map.fromList
          [ ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", [firstCoin]),
            ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", [secondCoin, thirdCoin])
          ]

    it "is left out of the groups when the network cannot write its address" $ do
      let segwit = storeCoin ctx (fakeTxHash 6, 0) Nothing segwitAddr 1000
          cash = storeCoin ctx (fakeTxHash 5, 0) Nothing bchExternalAddr 1000
      Map.map (map (.outpoint)) (groupCoins bch [segwit, cash])
        `shouldBe` Map.fromList [(bchExternalAddrText, [OutPoint (fakeTxHash 5) 0])]

{- JSON -}

jsonSpec :: Ctx -> Spec
jsonSpec _ =
  describe "Coins in JSON" $ do
    it "writes a confirmed coin as coins prints it, and reads it back" $ do
      let coin = JsonCoin firstCoin (externalAddr 0) 50000 (Store.BlockRef 800000 0) 3 False
          json :: String
          json =
            [r|{
              "outpoint": {"txid": "0000000000000000000000000000000000000000000000000000000000000001", "index": 0},
              "address": "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
              "value": 50000,
              "block": {"height": 800000, "position": 0},
              "confirmations": 3,
              "locked": false
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc coin)
      unmarshalJSON btc (cs json) `shouldBe` Just coin

    it "writes an unconfirmed and locked coin of Bitcoin Cash with its cash address" $ do
      let coin = JsonCoin (OutPoint (fakeTxHash 5) 1) bchExternalAddr 10000 (Store.MemRef 0) 0 True
          json :: String
          json =
            [r|{
              "outpoint": {"txid": "0000000000000000000000000000000000000000000000000000000000000005", "index": 1},
              "address": "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u",
              "value": 10000,
              "block": {"mempool": 0},
              "confirmations": 0,
              "locked": true
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue bch coin)
      unmarshalJSON bch (cs json) `shouldBe` Just coin
