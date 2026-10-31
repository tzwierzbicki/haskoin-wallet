{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.ListingSpec (spec) where

import Control.Monad (replicateM_)
import Data.Default (def)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Address (genExtAddress)
import Haskoin.Wallet.Commands (addressesListing)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Listing (Paging (..), streamListing)
import Haskoin.Wallet.Response (Response (..))
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

    it "stops once the reader has no use for more, as a pager quit early" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 250
        chunks <- streamed wallet (Paging Nothing Nothing) (const False)
        map snd chunks `shouldBe` [(True, [249, 248 .. 150])]

    it "fails as the listing does, before any chunk" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 3
        chunks <- streamed wallet (Paging (Just "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") Nothing) (const True)
        chunks `shouldBe` [(Left "The address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is not a receiving address of the account", (False, []))]

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
streamed wallet paging keepReading = do
  seen <- newIORef []
  result <- streamListing wallet (addressesListing Nothing False paging) $ \first answer -> do
    let rows = case answer of
          ResponseAddresses _ addrs -> dBAddressIndex . fst <$> addrs
          _ -> []
    modifyIORef seen ((Right (), (first, rows)) :)
    return (keepReading (length rows))
  chunks <- reverse <$> readIORef seen
  return $ case result of
    Right () -> chunks
    Left err -> chunks <> [(Left err, (False, []))]
