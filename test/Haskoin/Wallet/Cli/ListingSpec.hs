{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How the screen reads a listing: section by section, and chunk by chunk
-- in each section, as "Haskoin.Wallet.Cli.Listing" says. The rules of the
-- sections are first checked on sections of numbers, then the listings of
-- the commands on a portfolio of the test wallet.
module Haskoin.Wallet.Cli.ListingSpec (spec) where

import Control.Monad (forM_, replicateM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (MonadTrans (lift))
import Data.Default (def)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (genericTake)
import Data.Text (Text)
import Data.Word (Word8)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Listing (ChunkAt (..), Listing (..), Paging (..), Sections (..), streamListing, wholeListing)
import Haskoin.Wallet.Cli.Parser (Target (..))
import Haskoin.Wallet.Cli.Response (AccountCounts (..), Group (..), Response (..))
import Haskoin.Wallet.Commands (addressesListing, txsListing)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Address (genExtAddress)
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioChoice (..))
import Haskoin.Wallet.Database.TxInfo (ListedTx (..), markTxInfoDead, repsertSummary)
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  sectionsSpec
  addressesSpec ctx
  txsSpec ctx

{- Sections of numbers -}

-- A section here lists numbers, newest first as the listings do, each its
-- own cursor. Its answer gives each number as the index of an address of
-- a group on the network of the section, so that a chunk shows which
-- section it is of: btc, bch, then btctest.

-- | A section on the network of the numbers given, which starts after the
-- number given, if any
numbers :: Network -> Maybe Int -> [Int] -> Listing
numbers net start rows =
  Listing
    { listingStart = start,
      listingRows = \afterM countM -> return (maybe id genericTake countM (maybe rows (\cursor -> drop 1 (dropWhile (/= cursor) rows)) afterM)),
      listingCursor = Right,
      listingAnswer = \found -> ResponseAddresses [Group (Place mainPortfolioRef net) [] [(unusedAddressRow {dBAddressIndex = n}, 0) | n <- found] | not (null found)],
      listingWhole = False
    }

-- | A section whose rows cannot be read
failingSection :: Listing
failingSection =
  Listing
    { listingStart = Nothing,
      listingRows = \_ _ -> throwError "The database is gone" :: ExceptT String (DB IO) [Int],
      listingCursor = Right,
      listingAnswer = const (ResponseAddresses []),
      listingWhole = False
    }

-- | A chunk as the screen got it: where it is, and the rows of each group
-- by the name of its network
type Chunk = (ChunkAt, [(String, [Int])])

-- | The rows of an answer of sections of numbers, by the name of the
-- network of each group
groupsOf :: Response -> [(String, [Int])]
groupsOf (ResponseAddresses groups) = [((placeNetwork (groupPlace g)).name, dBAddressIndex . fst <$> groupRows g) | g <- groups]
groupsOf _ = []

-- | The chunks of the sections, read by a screen that reads on while the
-- function given says so of the rows it has seen, with the failure, if any
streamedSections :: Sections -> (Int -> Bool) -> IO ([Chunk], Either String ())
streamedSections sections keepReading =
  inScratchDirectory $ \dir -> do
    cfg <- walletInFile (dir </> "wallet.sqlite") (return ())
    seen <- newIORef []
    result <- streamListing cfg (return sections) $ \at answer -> do
      modifyIORef seen ((at, groupsOf answer) :)
      chunks <- readIORef seen
      return (keepReading (sum (length . concatMap snd . snd <$> chunks)))
    chunks <- reverse <$> readIORef seen
    return (chunks, result)

-- | The chunks of sections read to the end
readToTheEnd :: Sections -> IO [Chunk]
readToTheEnd sections = fst <$> streamedSections sections (const True)

-- | The sections read whole, as --json reads them
readWhole :: Sections -> IO (Either String [(String, [Int])])
readWhole sections =
  inScratchDirectory $ \dir -> do
    cfg <- walletInFile (dir </> "wallet.sqlite") (return ())
    fmap groupsOf <$> readOnceIn cfg (wholeListing (return sections))

-- | A reading of the database in one transaction, as a command runs it
readOnceIn :: Config -> ExceptT String (DB IO) a -> IO (Either String a)
readOnceIn cfg action = runDB cfg (runExceptT action)

sectionsSpec :: Spec
sectionsSpec =
  describe "A listing in sections" $ do
    it "reads each section after the one before, in its order" $
      readToTheEnd (Sections True Nothing [numbers btc Nothing [2, 1], numbers bch Nothing [3]])
        `shouldReturn` [(ChunkAt True True True, [("btc", [2, 1])]), (ChunkAt False True True, [("bch", [3])])]

    -- 3 rows of the first section, then 1 of the second: 3 + 1 = 4
    it "stops at --max rows counted over every section together" $
      readToTheEnd (Sections True (Just 4) [numbers btc Nothing [3, 2, 1], numbers bch Nothing [3, 2, 1]])
        `shouldReturn` [(ChunkAt True True True, [("btc", [3, 2, 1])]), (ChunkAt False True True, [("bch", [3])])]

    it "gives --json every row --max allows, over every section together" $
      readWhole (Sections True (Just 4) [numbers btc Nothing [3, 2, 1], numbers bch Nothing [3, 2, 1]])
        `shouldReturn` Right [("btc", [3, 2, 1]), ("bch", [3])]

    it "starts after the row --after names in the section that holds it, then goes on into the next sections" $
      readToTheEnd (Sections True Nothing [numbers bch (Just 2) [3, 2, 1], numbers btcTest Nothing [5, 4]])
        `shouldReturn` [(ChunkAt True True True, [("bch", [1])]), (ChunkAt False True True, [("btctest", [5, 4])])]

    it "shows nothing of a section without rows, the next one starting the listing" $
      readToTheEnd (Sections True Nothing [numbers btc Nothing [], numbers bch Nothing [2, 1]])
        `shouldReturn` [(ChunkAt True True True, [("bch", [2, 1])])]

    it "says once that a listing without any row has none, in one first chunk without rows" $
      readToTheEnd (Sections True Nothing [numbers btc Nothing [], numbers bch Nothing [], numbers btcTest Nothing []])
        `shouldReturn` [(ChunkAt True True True, [])]

    -- 150 rows: a chunk of 100, then one of 50, then the next section
    it "reads a long section in chunks of 100 rows, only the first of which starts the section" $ do
      chunks <- readToTheEnd (Sections True Nothing [numbers btc Nothing [150, 149 .. 1], numbers bch Nothing [1]])
      [(at, map (fmap length) groups) | (at, groups) <- chunks]
        `shouldBe` [ (ChunkAt True True True, [("btc", 100)]),
                     (ChunkAt False False True, [("btc", 50)]),
                     (ChunkAt False True True, [("bch", 1)])
                   ]

    it "tells each chunk that its sections say where they are only when the listing says so" $
      readToTheEnd (Sections False Nothing [numbers btc Nothing [2, 1]])
        `shouldReturn` [(ChunkAt True True False, [("btc", [2, 1])])]

    it "reads a section read whole in one chunk, however many rows it has" $ do
      chunks <- readToTheEnd (Sections True Nothing [(numbers btc Nothing [150, 149 .. 1]) {listingWhole = True}])
      [(at, map (fmap length) groups) | (at, groups) <- chunks] `shouldBe` [(ChunkAt True True True, [("btc", 150)])]

    it "stops once the reader has no use for more, as a pager quit early, though a section is left" $ do
      (chunks, result) <- streamedSections (Sections True Nothing [numbers btc Nothing [150, 149 .. 1], numbers bch Nothing [1]]) (const False)
      [(at, map (fmap length) groups) | (at, groups) <- chunks] `shouldBe` [(ChunkAt True True True, [("btc", 100)])]
      result `shouldBe` Right ()

    it "fails as the reading of its rows does" $
      streamedSections (Sections True Nothing [numbers btc Nothing [2, 1], failingSection]) (const True)
        `shouldReturn` ([(ChunkAt True True True, [("btc", [2, 1])])], Left "The database is gone")

{- The listings of the commands -}

-- | The receiving addresses of the bip44 account of portfolio 0 on btc,
-- as addresses --network btc --type bip44 lists them
onMainAccount :: Target
onMainAccount = Target (PortfolioChoice Nothing Nothing) (Just btc) (Just Bip44)

-- | The whole portfolio 0, as addresses lists it without an option
wholePortfolio :: Target
wholePortfolio = Target (PortfolioChoice Nothing Nothing) Nothing Nothing

addressesSpec :: Ctx -> Spec
addressesSpec ctx =
  describe "The listing of the receiving addresses" $ do
    it "reads 100 rows at a time, each chunk after the last row of the one before, until the rows run out" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 250 0
        chunks <- streamed wallet onMainAccount (Paging Nothing Nothing) (const True)
        [(chunkFirst at, rows) | (at, rows) <- chunks]
          `shouldBe` [(True, [(Bip44, [249, 248 .. 150])]), (False, [(Bip44, [149, 148 .. 50])]), (False, [(Bip44, [49, 48 .. 0])])]

    it "stops at --max rows, and starts after the row --after names" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 250 0
        chunks <- streamed wallet onMainAccount (Paging Nothing (Just 150)) (const True)
        snd <$> chunks `shouldBe` [[(Bip44, [249, 248 .. 150])], [(Bip44, [149, 148 .. 100])]]
        -- After the address at /0/3, the fourth one generated
        continued <- streamed wallet onMainAccount (Paging (Just (externalAddrsText !! 3)) (Just 2)) (const True)
        snd <$> continued `shouldBe` [[(Bip44, [2, 1])]]

    it "passes a listing of exactly a chunk of rows on as one chunk" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 100 0
        chunks <- streamed wallet onMainAccount (Paging Nothing Nothing) (const True)
        snd <$> chunks `shouldBe` [[(Bip44, [99, 98 .. 0])]]

    it "passes an empty listing on as one empty first chunk" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 0 0
        streamed wallet onMainAccount (Paging Nothing Nothing) (const True) `shouldReturn` [(ChunkAt True True False, [])]

    -- A portfolio has twelve accounts; only the two of btc hold addresses
    it "lists the accounts of the whole portfolio in sections that say where they are, those without an address left out" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 3 2
        streamed wallet wholePortfolio (Paging Nothing Nothing) (const True)
          `shouldReturn` [(ChunkAt True True True, [(Bip44, [2, 1, 0])]), (ChunkAt False True True, [(Bip84, [1, 0])])]

    it "keeps to one section, which says nothing of where it is, when --network and --type pick one account" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 3 2
        streamed wallet onMainAccount (Paging Nothing Nothing) (const True)
          `shouldReturn` [(ChunkAt True True False, [(Bip44, [2, 1, 0])])]

    -- The address at /0/1 of the bip44 account is followed by /0/0 there,
    -- then by the addresses of the bip84 account
    it "starts after --after in the section of the account of the address, and goes on into the next sections" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 3 2
        streamed wallet wholePortfolio (Paging (Just (externalAddrsText !! 1)) Nothing) (const True)
          `shouldReturn` [(ChunkAt True True True, [(Bip44, [0])]), (ChunkAt False True True, [(Bip84, [1, 0])])]

    -- 3 addresses of the bip44 account, then 1 of the bip84 one: 3 + 1 = 4
    it "counts --max over the sections of every account together" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 3 2
        streamed wallet wholePortfolio (Paging Nothing (Just 4)) (const True)
          `shouldReturn` [(ChunkAt True True True, [(Bip44, [2, 1, 0])]), (ChunkAt False True True, [(Bip84, [1])])]

    it "stops once the reader has no use for more, as a pager quit early" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 250 0
        chunks <- streamed wallet onMainAccount (Paging Nothing Nothing) (const False)
        snd <$> chunks `shouldBe` [[(Bip44, [249, 248 .. 150])]]

    it "fails as the listing does, before any chunk, when --after names an address of no account listed" $
      inScratchDirectory $ \dir -> do
        wallet <- walletWithAddresses ctx dir 3 0
        result <- streamedWithResult wallet onMainAccount (Paging (Just "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") Nothing)
        result `shouldBe` ([], Left "The address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is not a receiving address of the account")

txsSpec :: Ctx -> Spec
txsSpec ctx =
  describe "The listing of the transactions" $
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

-- | Portfolio 0 of the test wallet on btc, with the given number of
-- payments of 50000 to its bip44 account, each in a block of its own,
-- holding them all but the one marked dead
walletWithPayments :: Ctx -> FilePath -> Word8 -> Word8 -> IO Config
walletWithPayments ctx dir count dead =
  walletInFile (dir </> "wallet.sqlite") $ do
    (accId, _) <- testNewAcc ctx
    pool <- testPool ctx
    lift $ do
      forM_ [1 .. count] $ \n ->
        repsertSummary pool (fakeTxHash n) (Store.BlockRef (800000 + fromIntegral n) 1) fixtureTime paymentSummary
      markTxInfoDead pool (fakeTxHash dead)
      P.update accId [DBAccountBalanceConfirmed P.=. fromIntegral (count - 1) * 50000]

-- | The balances of txs on btc, chunk by chunk, as the screen reads them
streamedBalances :: Config -> IO [[Maybe Integer]]
streamedBalances wallet = do
  seen <- newIORef []
  result <- streamListing wallet (txsListing wallet (PortfolioChoice Nothing Nothing) (Just btc) (Paging Nothing Nothing)) $ \_ answer -> do
    case answer of
      ResponseTxs groups -> modifyIORef seen ((listedTxBalance <$> concatMap groupRows groups) :)
      _ -> return ()
    return True
  result `shouldBe` Right ()
  reverse <$> readIORef seen

-- | Portfolio 0 of the test wallet, with the given numbers of receiving
-- addresses on btc of its bip44 account and of its bip84 account,
-- generated past the usual gap
walletWithAddresses :: Ctx -> FilePath -> Int -> Int -> IO Config
walletWithAddresses ctx dir legacy segwit =
  walletInFile (dir </> "wallet.sqlite") $ do
    (accId, _) <- testNewAcc ctx
    (segwitId, _) <- testNewSegwitAcc ctx
    replicateM_ legacy $ genExtAddress ctx def {configGap = 1000} accId ""
    replicateM_ segwit $ genExtAddress ctx def {configGap = 1000} segwitId ""

-- | Stream the receiving addresses the target picks, each chunk recorded
-- as where it is and, for each of its groups, the type of its account and
-- the indices of its rows, the reader answering as told
streamed :: Config -> Target -> Paging Text -> (Int -> Bool) -> IO [(ChunkAt, [(AccountKind, [Int])])]
streamed wallet target paging keepReading = do
  (chunks, result) <- streamedFrom wallet target paging keepReading
  result `shouldBe` Right ()
  return chunks

-- | The chunks of a stream read to the end, with its failure, if any
streamedWithResult :: Config -> Target -> Paging Text -> IO ([(ChunkAt, [(AccountKind, [Int])])], Either String ())
streamedWithResult wallet target paging = streamedFrom wallet target paging (const True)

-- | Stream the receiving addresses as 'streamed' does, with the failure
streamedFrom :: Config -> Target -> Paging Text -> (Int -> Bool) -> IO ([(ChunkAt, [(AccountKind, [Int])])], Either String ())
streamedFrom wallet target paging keepReading = do
  seen <- newIORef []
  result <- streamListing wallet (addressesListing wallet target False paging) $ \at answer -> do
    let groups = case answer of
          ResponseAddresses gs -> [(kindOf g, dBAddressIndex . fst <$> groupRows g) | g <- gs]
          _ -> []
    modifyIORef seen ((at, groups) :)
    return (keepReading (length (concatMap snd groups)))
  chunks <- reverse <$> readIORef seen
  return (chunks, result)
  where
    kindOf g = case groupAccounts g of
      [account] -> countsKind account
      _ -> error "A group of addresses covers one account"
