{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Cli.ListingSpec (spec) where

import Control.Monad (forM_, replicateM_)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadTrans (lift))
import Data.Default (def)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import Data.Word (Word8)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Address (genExtAddress)
import Haskoin.Wallet.Commands (addressesListing, txsListing)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.TxInfo (ListedTx (..), markTxInfoDead, repsertSummary)
import Haskoin.Wallet.Cli.Listing (Listing (..), Paging (..), streamListing)
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.TestUtils
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "Reading a listing in chunks, as the screen does" $ do
    it "reads 100 rows at a time, each chunk after the last row of the one before, until the rows run out" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 250
        chunks <- streamed wallet (Paging Nothing Nothing) (const True)
        map snd chunks `shouldBe` [(True, [249, 248 .. 150]), (False, [149, 148 .. 50]), (False, [49, 48 .. 0])]

    it "stops at --max rows, and starts after the row --after names" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 250
        chunks <- streamed wallet (Paging Nothing (Just 150)) (const True)
        map snd chunks `shouldBe` [(True, [249, 248 .. 150]), (False, [149, 148 .. 100])]
        -- After the address at /0/3, the fourth one generated
        continued <- streamed wallet (Paging (Just (externalAddrsText !! 3)) (Just 2)) (const True)
        map snd continued `shouldBe` [(True, [2, 1])]

    it "passes a listing of exactly a chunk of rows on as one chunk" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 100
        chunks <- streamed wallet (Paging Nothing Nothing) (const True)
        map snd chunks `shouldBe` [(True, [99, 98 .. 0])]

    it "passes an empty listing on as one empty first chunk" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 0
        chunks <- streamed wallet (Paging Nothing Nothing) (const True)
        map snd chunks `shouldBe` [(True, [])]

    -- 101 payments, newest first. The one in the second block is the row
    -- that ends the first chunk, and it is dead. The chunk after it starts
    -- with the oldest payment, whose 50000 is all that was there.
    it "carries the balance of txs over to the next chunk, through a dead transaction that ends a chunk" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithPayments ctx dir 101 2
        chunks <- streamedBalances wallet
        length <$> chunks `shouldBe` [100, 1]
        -- 100 payments are alive: 100 × 50000 = 5000000
        take 1 (concat chunks) `shouldBe` [Just 5000000]
        drop 98 (concat chunks) `shouldBe` [Just 100000, Nothing, Just 50000]

    it "stops once the reader has no use for more, as a pager quit early" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 250
        chunks <- streamed wallet (Paging Nothing Nothing) (const False)
        map snd chunks `shouldBe` [(True, [249, 248 .. 150])]

    it "passes a listing read whole on as one first chunk, however many rows it has" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 250
        chunks <- streamedFrom wallet ((\listing -> listing {listingWhole = True}) <$> addressesListing Nothing False (Paging Nothing Nothing)) (const True)
        map snd chunks `shouldBe` [(True, [249, 248 .. 0])]

    it "fails as the listing does, before any chunk" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 3
        chunks <- streamed wallet (Paging (Just "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") Nothing) (const True)
        chunks `shouldBe` [(Left "The address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is not a receiving address of the account", (False, []))]

-- | The account of a wallet with the given number of payments of 50000,
-- each in a block of its own, holding them all but the one marked dead
walletWithPayments :: Ctx -> FilePath -> Word8 -> Word8 -> IO Config
walletWithPayments ctx dir count dead =
  walletInFile (dir </> "wallet.sqlite") $ do
    (accId, _) <- testNewAcc ctx "main"
    lift $ do
      forM_ [1 .. count] $ \n ->
        repsertSummary btc accId (fakeTxHash n) (Store.BlockRef (800000 + fromIntegral n) 1) fixtureTime paymentSummary
      markTxInfoDead accId (fakeTxHash dead)
      P.update accId [DBAccountBalanceConfirmed P.=. fromIntegral (count - 1) * 50000]

-- | The balances of txs, chunk by chunk, as the screen reads them
streamedBalances :: Config -> IO [[Maybe Integer]]
streamedBalances wallet = do
  seen <- newIORef []
  result <- streamListing wallet (txsListing Nothing (Paging Nothing Nothing)) $ \_ answer -> do
    case answer of
      ResponseTxs _ listed -> modifyIORef seen ((listedTxBalance <$> listed) :)
      _ -> return ()
    return True
  result `shouldBe` Right ()
  reverse <$> readIORef seen

-- | A wallet of one account with the given number of receiving addresses,
-- generated past the usual gap
walletWithAddresses :: Ctx -> FilePath -> Int -> IO Config
walletWithAddresses ctx dir count =
  walletInFile (dir </> "wallet.sqlite") $ do
    (accId, _) <- testNewAcc ctx "main"
    replicateM_ count $ genExtAddress ctx def {configGap = 1000} accId ""

-- | Stream the receiving addresses of the wallet, each chunk recorded as
-- whether it is the first and the indices of its rows, the reader answering
-- as told. A failure is recorded as a chunk of its own, with no rows.
streamed :: Config -> Paging Text -> (Int -> Bool) -> IO [(Either String (), (Bool, [Int]))]
streamed wallet paging = streamedFrom wallet (addressesListing Nothing False paging)

-- | Stream a listing of receiving addresses as 'streamed' does, the
-- listing given
streamedFrom :: Config -> ExceptT String (DB IO) Listing -> (Int -> Bool) -> IO [(Either String (), (Bool, [Int]))]
streamedFrom wallet listing keepReading = do
  seen <- newIORef []
  result <- streamListing wallet listing $ \first answer -> do
    let rows = case answer of
          ResponseAddresses _ addrs -> dBAddressIndex . fst <$> addrs
          _ -> []
    modifyIORef seen ((Right (), (first, rows)) :)
    return (keepReading (length rows))
  chunks <- reverse <$> readIORef seen
  return $ case result of
    Right () -> chunks
    Left err -> chunks <> [(Left err, (False, []))]
