{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What hw shows: the listings, their paging and the view the
-- configuration file names for them, a transaction as hw
-- lays it out, the pending transactions, the balances of the addresses,
-- the coins, the next step shown after a payment or a sweep, the pager of
-- a listing and of a help, the CSV files of --csv, and the spreadsheet of
-- exportsheet, the journal of exportjournal, the journal hw keeps next
-- to the database, and the balance statement of report.
--
-- A listing covers the active portfolio, and portfolios and exportsheet
-- every portfolio, the active one marked. A listing over a portfolio comes
-- in sections, one for each network, or for each account for addresses. A
-- section says where it is in a title line when the listing covers
-- several places, and balances always titles it, with the total of the
-- network. pendingtxs lists the active portfolio, in one list sorted by
-- network, each transaction naming its place.
module Haskoin.Wallet.Workflow.OutputSpec (spec) where

import Codec.Archive.Zip (findEntryByPath, fromEntry, toArchive)
import Control.Monad (forM_, replicateM_)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isHexDigit)
import Data.IORef (modifyIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.List (isInfixOf)
import Data.Time (UTCTime (..), addDays, addUTCTime, getCurrentTime, showGregorian)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (ActiveShown (..), Group (..), JournalSwitch (..), Response (..))
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..), coinBalanceUnspendable)
import Haskoin.Wallet.Database.Coin (JsonCoin (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import System.Directory (createDirectory, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files (fileID, fileMode, getFileStatus, modificationTimeHiRes, setFileMode)
import Test.Hspec
import qualified Toml

spec :: Spec
spec = do
  describe "The status line" $ do
    let shown text run = text `Text.isInfixOf` runErr run

    it "tells on a terminal what sync is doing" $
      withChain paidChain $ \home _ -> do
        newTestPortfolio home
        run <- hwOnTerminal [] home ["sync"] []
        run `shouldSatisfy` shown "\rportfolio0 btc: checking the store\ESC[0K"

    it "shows on a sync with nothing new as well" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hwOnTerminal [] home ["sync"] []
        run `shouldSatisfy` shown "\rportfolio0 btc: reading balances  [--------------------]  0/1 addresses\ESC[0K"

    it "is gone from the terminal when sync ends" $
      withChain paidChain $ \home _ -> do
        newTestPortfolio home
        run <- hwOnTerminal [] home ["sync"] []
        runErr run `shouldSatisfy` Text.isSuffixOf "\r\ESC[0K"

    it "leaves the answer of sync as it is off a terminal" $
      withChain paidChain $ \home _ -> do
        synced home
        plain <- hw home ["sync"] []
        run <- hwOnTerminal [] home ["sync"] []
        runOut run `shouldBe` runOut plain

    it "writes nothing off a terminal" $
      withChain paidChain $ \home _ -> do
        newTestPortfolio home
        run <- hw home ["sync"] []
        runErr run `shouldBe` ""

    -- The question comes right after the code that clears the line, so
    -- it starts at the left edge and nothing is left behind it
    it "makes room for the question of a scan" $
      withLyingChain paidChain $ \home _ -> do
        newTestPortfolio home
        run <- hwOnTerminal [] home ["sync"] ["n"]
        run `shouldSatisfy` shown "\r\ESC[0KThe store reports used addresses as far as 1000"

    -- The chain pays portfolio0, so the search has found one portfolio
    -- paid when it looks at portfolio1
    it "tells on a terminal which portfolio recover looks at, after the mnemonic is typed" $
      withChain paidChain $ \home _ -> do
        run <- hwOnTerminal [] home ["recover"] seedOnce
        run `shouldSatisfy` shown "\rrecover: looking at portfolio0, 0 portfolios paid so far\ESC[0K"
        run `shouldSatisfy` shown "\rrecover: looking at portfolio1, 1 portfolio paid so far\ESC[0K"

    -- The funds at paths hw cannot spend from: bip49 at m/49' on Bitcoin,
    -- and the accounts of Bitcoin, at m/44'/0', on Bitcoin Cash. A path
    -- alone would not tell the two networks apart.
    it "tells on a terminal which network and account recover looks at for the funds hw cannot spend" $
      withChain paidChain $ \home _ -> do
        run <- hwOnTerminal [] home ["recover"] seedOnce
        run `shouldSatisfy` shown "\rrecover on btc: looking at /49'/0'/0', 0 accounts paid so far\ESC[0K"
        run `shouldSatisfy` shown "\rrecover on bch: looking at /44'/0'/0', 0 accounts paid so far\ESC[0K"

  listingSpec
  listingViewSpec
  listingReverseSpec
  layoutSpec
  pendingListSpec
  csvSpec
  exportSheetSpec
  exportJournalSpec
  keptJournalSpec
  reportSpec
  balancesListSpec
  coinsListSpec
  sectionsSpec
  syncAnswerSpec
  pagerSpec
  helpPagerSpec
  addressCodeSpec
  nextStepSpec

{- Listings -}

-- | How many rows a listing shows, over all its sections
rowsOf :: Response -> Either String Int
rowsOf (ResponseAddresses groups) = Right (sum (length . groupRows <$> groups))
rowsOf (ResponseBalances groups) = Right (sum (length . groupRows . fst <$> groups))
rowsOf (ResponseTxs groups) = Right (sum (length . groupRows <$> groups))
rowsOf (ResponsePendingTxs _ held) = Right (length held)
rowsOf (ResponseCoins groups) = Right (sum (length . groupRows <$> groups))
rowsOf other = Left (show other)

-- | What names each row of a listing, as --after takes it: the address,
-- the coin, the txid, or the hash of a pending transaction as shown. The
-- sections of a listing come one after the other.
keysOf :: Response -> Either String [String]
keysOf (ResponseAddresses groups) = Right [Text.unpack (dBAddressAddress addr) | g <- groups, (addr, _, _) <- groupRows g]
keysOf (ResponseBalances groups) = Right [Text.unpack (dBAddressAddress addr) | (g, _) <- groups, (addr, _, _) <- groupRows g]
keysOf (ResponseTxs groups) = Right [Text.unpack (txHashToHex (chainTxid status)) | g <- groups, ListedTx info _ <- groupRows g, OnChain status <- [txInfoStatus info]]
keysOf (ResponsePendingTxs _ held) = Right [Text.unpack (txHashToHex (pendingShownHash status)) | (_, info) <- held, Pending status <- [txInfoStatus info]]
keysOf (ResponseCoins groups) = Right [Text.unpack (txHashToHex outpoint.hash) <> ":" <> show outpoint.index | g <- groups, coin <- groupRows g, let outpoint = jsonCoinOutpoint coin]
keysOf other = Left (show other)

-- | The listings, each with one row in a synced home with a payment
-- prepared: the address paid, the payment received, its coin, the payment
-- prepared, and the address that holds the coin
listings :: [String]
listings = ["addresses", "txs", "coins", "pendingtxs", "balances"]

-- | The time of each transaction an answer lists
timesOf :: Response -> Either String [Maybe UTCTime]
timesOf (ResponseTxs groups) = Right [chainTime (chainOf (listedTxInfo listed)) | g <- groups, listed <- groupRows g]
timesOf other = Left (show other)

listingSpec :: Spec
listingSpec = do
  -- The fake store gives every transaction the time 1700000000
  describe "The history txs lists" $
    it "gives each transaction the time the store gives it" $
      withChain paidChain $ \home _ -> do
        synced home
        (timesOf <$> hwJson home ["txs"] []) `shouldReturn` Right [Just seenTime]

  describe "A listing" $ do
    it "shows every row without --max" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 6 (answer home ["receive"] [])
        (rowsOf <$> hwJson home ["addresses"] []) `shouldReturn` Right 6

    -- The header takes two lines: the headings, and the line under them.
    -- Each of the 6 addresses takes one row. --network and --type narrow
    -- the listing to one account, which needs no title.
    it "shows each address on one row under a header with addresses --tabular, for one account" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 6 (answer home ["receive"] [])
        listed <- runOut <$> hw home ["addresses", "--network", "btc", "--type", "bip44", "--tabular"] []
        length (Text.lines listed) `shouldBe` 8

    -- The listing covers every account of the portfolio, so its section
    -- says which account it is, above the two lines of the header and the
    -- row of the address
    it "puts the title of the account above the header with addresses --tabular, over the whole portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        listed <- Text.lines . runOut <$> hw home ["addresses", "--tabular"] []
        (take 1 listed, length listed) `shouldBe` (["portfolio0  btc  bip44"], 4)

    -- The header takes two lines. A portfolio that has no address yet has
    -- one row, with no network.
    it "shows a portfolio on one row under a header with portfolios --tabular" $
      withHome $ \home -> do
        newTestPortfolio home
        listed <- runOut <$> hw home ["portfolios", "--tabular"] []
        length (Text.lines listed) `shouldBe` 3

    -- The payment waits in the mempool. Yellow is colour 33 of the
    -- terminal. --no-pager prints the listing itself.
    it "shows the unconfirmed balance of a portfolio in yellow on a terminal, as what cannot be spent yet" $
      withChain (Chain 800002 [(payment, Nothing)]) $ \home _ -> do
        synced home
        run <- hwOnScreen [] home ["portfolios", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "unconfirmed \ESC[0;33m0.00050000\ESC[0m btc"

    -- The header takes two lines, and the coin of the payment one row.
    -- --network narrows the listing to one network, which needs no
    -- title.
    it "shows each coin on one row under a header with coins --tabular, on one network" $
      withChain paidChain $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["coins", "--network", "btc", "--tabular"] []
        length (Text.lines listed) `shouldBe` 3

    -- The title of the network with its total, the two lines of the
    -- header, and the address paid on one row
    it "shows each address on one row under the title of its network and a header with balances --tabular" $
      withChain paidChain $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["balances", "--tabular"] []
        length (Text.lines listed) `shouldBe` 4

    -- The header takes two lines, and the payment prepared one row
    it "shows each pending transaction on one row under a header with pendingtxs --tabular" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        listed <- runOut <$> hw home ["pendingtxs", "--tabular"] []
        length (Text.lines listed) `shouldBe` 3

    it "shows the row of each listing" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        forM_ listings $ \listing ->
          (rowsOf <$> hwJson home [listing] []) `shouldReturn` Right 1

    -- 2^64 + 1 wrapped around to 1, and showed one address of the three
    it "shows every row with a --max past 64 bits, rather than wrapping around" $
      withHome $ \home -> do
        newTestPortfolio home
        replicateM_ 3 $ answer home ["receive"] []
        (rowsOf <$> hwJson home ["addresses", "--max", "18446744073709551617"] []) `shouldReturn` Right 3

    it "refuses a --max of 0" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["addresses", "--max", "0"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --max: cannot parse value `0'"

    it "refuses to start after a row that is not in it" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["addresses", "--after", Text.unpack (internalAddrsText !! 0)] [])
          `shouldReturn` Right ("The address " <> internalAddrsText !! 0 <> " is not among the addresses listed")

    -- portfolio1 is a portfolio of its own, without any row. pendingtxs
    -- lists every portfolio, as "The pending transactions pendingtxs
    -- lists" checks.
    it "shows the rows of the active portfolio only, but for pendingtxs" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        newTestPortfolio home
        let ofActive = filter (/= "pendingtxs") listings
        forM_ ofActive $ \listing ->
          (rowsOf <$> hwJson home [listing] []) `shouldReturn` Right 1
        switchTo home 0 1
        forM_ ofActive $ \listing ->
          (rowsOf <$> hwJson home [listing] []) `shouldReturn` Right 0

    -- Two of each: the addresses /0/0 and /0/1, the two payments to them,
    -- their coins, and a payment prepared from each coin
    it "shows --max rows of each listing, then the rows after the one --after names" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg payment 0, "--fee-rate", "10"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg newerPayment 0, "--fee-rate", "10"] []
        forM_ listings $ \listing -> do
          keys <- either fail return . keysOf =<< hwJson home [listing] []
          length keys `shouldBe` 2
          (keysOf <$> hwJson home [listing, "--max", "1"] []) `shouldReturn` Right [keys !! 0]
          (keysOf <$> hwJson home [listing, "--after", keys !! 0] []) `shouldReturn` Right [keys !! 1]
          (rowsOf <$> hwJson home [listing, "--after", keys !! 1] []) `shouldReturn` Right 0

    -- The two rows of each listing of the test before
    it "shows the rows of each listing from the last to the first with --reverse, and pages them in that order" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg payment 0, "--fee-rate", "10"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg newerPayment 0, "--fee-rate", "10"] []
        forM_ listings $ \listing -> do
          keys <- either fail return . keysOf =<< hwJson home [listing] []
          length keys `shouldBe` 2
          (keysOf <$> hwJson home [listing, "--reverse"] []) `shouldReturn` Right [keys !! 1, keys !! 0]
          (keysOf <$> hwJson home [listing, "--reverse", "--max", "1"] []) `shouldReturn` Right [keys !! 1]
          (keysOf <$> hwJson home [listing, "--reverse", "--after", keys !! 1] []) `shouldReturn` Right [keys !! 0]
          (rowsOf <$> hwJson home [listing, "--reverse", "--after", keys !! 0] []) `shouldReturn` Right 0

    -- The address at /0/0 was given out first, then the one at /0/1
    it "lists the addresses from the first one given out with --reverse" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive"] []
        (addressesOf <$> hwJson home ["addresses", "--reverse"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ""), ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "")]

    -- The listings of the test before that name a row by a hash. A coin is
    -- the short id of its txid, then its index.
    it "shows the rows after the one --after names by its short id" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg payment 0, "--fee-rate", "10"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg newerPayment 0, "--fee-rate", "10"] []
        let shortKey key = shortOf key <> dropWhile (/= ':') key
        forM_ ["txs", "coins", "pendingtxs"] $ \listing -> do
          keys <- either fail return . keysOf =<< hwJson home [listing] []
          (keysOf <$> hwJson home [listing, "--after", shortKey (keys !! 0)] []) `shouldReturn` Right [keys !! 1]

    -- btctest and btctest4 share their keys, so each has the address in a
    -- section of its own: starting in the first of them would loop
    it "refuses to start addresses after an address two test networks share, and asks for the --network" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        (errorOf <$> hwJson home ["addresses", "--after", "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"] [])
          `shouldReturn` Right "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4: give the --network"

    -- Two addresses handed out on each network, the same two. The newer,
    -- /0/1, is listed first, then mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih at
    -- /0/0.
    it "starts addresses after an address two test networks share on the --network named" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        newer <- receiveWith home ["--network", "btctest4"]
        (addressesOf <$> hwJson home ["addresses", "--network", "btctest4", "--after", newer] [])
          `shouldReturn` Right [("mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih", "")]

    -- The host serves btctest and btctest4 alone, and the coin is at the
    -- address on both
    it "refuses to start balances after an address two test networks share, and asks for the --network" $
      withChainServing [btcTest, btcTest4] [] (confirmedChain testnetCoinTx) $ \home _ -> do
        testnetCoinSynced home
        (errorOf <$> hwJson home ["balances", "--after", "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"] [])
          `shouldReturn` Right "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4: give the --network"

    -- The two payments of the test before, signed. The newer is listed
    -- first.
    it "shows the pending transactions after the signed one --after names by its txid" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        older <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg payment 0, "--fee-rate", "10"] []
        newer <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg newerPayment 0, "--fee-rate", "10"] []
        olderId <- txidOf =<< txOf =<< answer home ["signtx", older] seedOnce
        newerId <- txidOf =<< txOf =<< answer home ["signtx", newer] seedOnce
        (keysOf <$> hwJson home ["pendingtxs", "--after", newerId] []) `shouldReturn` Right [olderId]

-- | The key listing-view of the configuration file, for a user who always
-- wants the same view, and --compact, which brings back the compact view
listingViewSpec :: Spec
listingViewSpec =
  describe "The view of the listings that the configuration file names" $ do
    -- Each column is as wide as its heading, which is longer than the
    -- cells under it. The portfolio has one address on btc.
    it "shows a listing in that view when no option asks for one" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        setListingView home "tabular"
        listed <- Text.lines . runOut <$> hw home ["portfolios"] []
        take 1 listed `shouldBe` ["  │ Wallet  │ Portfolio  │ Network │ Spendable (btc) │ Unconfirmed (btc) │ Coins │ Addresses │ Description"]
        listed `shouldContain` ["* │ wallet0 │ portfolio0 │ btc     │      0.00000000 │        0.00000000 │     0 │         1 │"]

    -- Only the verbose view gives the block of a coin
    it "can be the verbose view" $
      withChain paidChain $ \home _ -> do
        synced home
        setListingView home "verbose"
        run <- hw home ["coins"] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

    -- A table has a line of dashes under its headings, which no other
    -- view has: the second line, or the third under the title of a
    -- section. The payment prepared gives each listing a row.
    it "is the view of each of the six listings" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        setListingView home "tabular"
        let ruleOfTable line = not (Text.null line) && Text.all (`elem` ("─┼" :: String)) line
        forM_ ("portfolios" : listings) $ \listing -> do
          listed <- Text.lines . runOut <$> hw home [listing] []
          -- The name of the listing goes with the check, so that a
          -- failure tells which listing it was
          (listing, any ruleOfTable (take 3 listed)) `shouldBe` (listing, True)

    -- The compact view of a portfolio nothing has paid, in three lines:
    -- its name, marked as the active portfolio, the day it was created,
    -- which is today, and that it has no address yet
    it "gives way to --compact, which shows the compact view" $
      withHome $ \home -> do
        newTestPortfolio home
        setListingView home "tabular"
        listed <- Text.lines . runOut <$> hw home ["portfolios", "--compact"] []
        (take 1 listed, drop 2 listed) `shouldBe` (["* portfolio0  wallet0"], ["    no addresses yet"])

    -- Only the verbose view gives the fingerprint of the seed
    it "gives way to the view another option asks for" $
      withHome $ \home -> do
        newTestPortfolio home
        setListingView home "tabular"
        run <- hw home ["portfolios", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n    Fingerprint: " <> testFingerprintText <> "\n")

    -- A script that reads the JSON answers is not affected by the view
    it "leaves the JSON answer of a listing as it is" $
      withHome $ \home -> do
        newTestPortfolio home
        setListingView home "tabular"
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [mainPortfolioRef]

    -- lockcoin answers with one coin, as a listing of coins does, but it
    -- is not a listing. The block is in its verbose view only.
    it "leaves a command that answers with one element in its own view" $
      withChain paidChain $ \home _ -> do
        synced home
        setListingView home "tabular"
        run <- hw home ["lockcoin", coinArg payment 0] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

-- | The key listing-reverse of the configuration file, for a user who
-- always reads the oldest row first, and --no-reverse, which brings back
-- the usual order. The address at /0/0 was given out first, then the one
-- at /0/1. The second line of each address says it is unused.
listingReverseSpec :: Spec
listingReverseSpec =
  describe "The reverse order of the listings that the configuration file asks for" $ do
    let twoAddresses home = do
          newTestPortfolio home
          _ <- answer home ["receive"] []
          _ <- answer home ["receive"] []
          return ()
        addressLines = filter ("bip44/" `Text.isPrefixOf`) . Text.lines . runOut
    it "lists the rows of a listing in the reverse order on the screen when no option asks for one" $
      withHome $ \home -> do
        twoAddresses home
        setListingReverse home True
        (addressLines <$> hw home ["addresses"] [])
          `shouldReturn` ["bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"]

    it "lists the rows in the usual order when the key is off, as without it" $
      withHome $ \home -> do
        twoAddresses home
        setListingReverse home False
        (addressLines <$> hw home ["addresses"] [])
          `shouldReturn` ["bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]

    it "gives way to --no-reverse, which lists the rows in the usual order" $
      withHome $ \home -> do
        twoAddresses home
        setListingReverse home True
        (addressLines <$> hw home ["addresses", "--no-reverse"] [])
          `shouldReturn` ["bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]

    -- A script that pages through the JSON answers reads the same rows
    -- whatever the file says
    it "leaves the JSON answer of a listing in the usual order" $
      withHome $ \home -> do
        twoAddresses home
        setListingReverse home True
        (addressesOf <$> hwJson home ["addresses"] [])
          `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", ""), ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "")]

    it "leaves --reverse to reverse the JSON answer of a listing" $
      withHome $ \home -> do
        twoAddresses home
        setListingReverse home True
        (addressesOf <$> hwJson home ["addresses", "--reverse"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ""), ("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "")]

    -- With the reverse order, the payment of 50000 comes first, then the
    -- one of 30000: 50000 + 30000 = 80000
    it "is the order of txs too, each transaction with the balance after it" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        setListingReverse home True
        listed <- runOut <$> hw home ["txs", "--verbose"] []
        filter ("Balance" `Text.isInfixOf`) (Text.lines listed)
          `shouldBe` ["  Balance  : 0.00050000 btc", "  Balance  : 0.00080000 btc"]

{- Transactions as hw shows them -}

-- | A payment of 50000 to the first receiving address of the test wallet,
-- from another wallet that keeps 70000 of change at an address of its own.
-- The store gives its input 121000, so that its fee is 1000. The payer
-- spends from otherAddr 0.
paymentWithPayerChange :: Tx
paymentWithPayerChange = fundingTx 9 [(externalAddr 0, 50000), (otherAddr 1, 70000)]

-- | What hw shows of the payment of 20000 prepared in a synced home
shownPayment :: FilePath -> [String] -> IO Text
shownPayment home options = runOut <$> hw home (["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] <> options) []

-- The payment of 20000 spends the coin of 50000 at /0/0, and leaves 27740
-- of change
layoutSpec :: Spec
layoutSpec =
  describe "A transaction as hw shows it" $ do
    it "shows the addresses of the wallet a payment spends from, with their paths" $
      withChain paidChain $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0\n"

    it "shows the labels of the addresses of the wallet" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "salary"] []
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0\n             \"salary\"\n"

    it "shows the recipients of a payment" $
      withChain paidChain $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc\n"

    it "shows the change of a payment, at its change address" $
      withChain paidChain $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0\n"

    it "shows the change a payment sends back to the address it spends from" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        shown <- shownPayment home ["--spend-from", textOf (externalAddr 0), "--change-back", textOf (externalAddr 0)]
        shown `shouldSatisfy` Text.isInfixOf "  Change   : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00027740 btc  bip44/0/0\n"

    -- The payer's coins are worth 121000, nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts, in txs --verbose" $
      withChain (confirmedChain paymentWithPayerChange) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--verbose"] []
        listed `shouldSatisfy` Text.isInfixOf "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\n"

    it "shows the address of the wallet a payment received is sent to, with its path, in txs --verbose" $
      withChain (confirmedChain paymentWithPayerChange) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--verbose"] []
        listed `shouldSatisfy` Text.isInfixOf "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0\n"

    -- A portfolio has an address at /0/0 in each of its accounts on btc,
    -- so the path names the type of the account
    it "shows the type and the path of the address of the portfolio a payment received is paid at, in txs" $
      withChain (confirmedChain paymentWithPayerChange) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "\n  balance 0.00050000 btc, at bip44/0/0\n"

    -- Blue is colour 34 of the terminal, and 3 sets italics, as labels
    -- have them. --no-pager prints the listing itself, which a pager would
    -- read.
    it "shows the path of an address of the wallet in blue on a terminal" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hwOnScreen [] home ["txs", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "at \ESC[0;34mbip44/0/0\ESC[0m"

    it "shows the label of an address of the wallet in its blue on a terminal, in italics" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "rent"] []
        run <- hwOnScreen [] home ["txs", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "\ESC[0;34;3m\"rent\""

    -- Yellow is colour 33 of the terminal. The address is of portfolio1,
    -- which receive gave out while it was active, and portfolio0 is active
    -- when show shows it.
    it "says in yellow on a terminal that an answer is about another portfolio than the active one" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        addr <- receiveWith home []
        switchTo home 0 0
        run <- hwOnScreen [] home ["show", addr]
        runOut run `shouldSatisfy` Text.isInfixOf "portfolio1\ESC[0m, \ESC[0;33mnot the active portfolio0\ESC[0m"

    it "shows an address of the wallet in full in blue on a terminal, in txs --verbose" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hwOnScreen [] home ["txs", "--verbose", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "\ESC[0;34m1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\ESC[0m"

    -- portfolio0 pays 20000 of its coin of 50000 to the first receiving
    -- address of portfolio1, 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo. Blue is
    -- colour 34 of the terminal, and 2 is faint. The 20000 leave
    -- portfolio0, so they are not green, as what it receives is.
    it "shows an address of another portfolio that a payment pays in blue on a terminal, its value in no colour, its path and its portfolio faint" $
      withChain paidChain $ \home _ -> do
        syncedTwoPortfolios home
        _ <- answer home ["preparetx", fixedTo account1Addr "0.0002", "--fee-rate", "10"] []
        run <- hwOnScreen [] home ["pendingtxs", "--verbose", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "\ESC[0;34m17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo\ESC[0m  0.00020000 btc  \ESC[0;2mbip44/0/0 of portfolio1\ESC[0m"

    it "shows the path and the portfolio of an address of another portfolio that a payment pays in blue on a terminal, in the compact view" $
      withChain paidChain $ \home _ -> do
        syncedTwoPortfolios home
        _ <- answer home ["preparetx", fixedTo account1Addr "0.0002", "--fee-rate", "10"] []
        run <- hwOnScreen [] home ["pendingtxs", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "to \ESC[0;34mbip44/0/0 of portfolio1\ESC[0m"

    it "does not show the change the payer of a payment received keeps, in either view" $
      withChain (confirmedChain paymentWithPayerChange) $ \home _ -> do
        synced home
        compact <- runOut <$> hw home ["txs"] []
        verbose <- runOut <$> hw home ["txs", "--verbose"] []
        compact `shouldNotSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 1)))
        verbose `shouldNotSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 1)))

    -- --network narrows the listing to one network, which needs no title
    it "lists each transaction in two lines without --verbose" $
      withChain (confirmedChain paymentWithPayerChange) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--network", "btc"] []
        length (Text.lines listed) `shouldBe` 2

    -- The two payments, two lines each, and a blank line between them
    it "sets the transactions apart with a blank line without --verbose" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--network", "btc"] []
        map Text.null (Text.lines listed) `shouldBe` [False, False, True, False, False]

    -- The header takes two lines: the headings, and the line under them.
    -- Each of the two payments takes one row.
    it "lists each transaction on one row under a header with --tabular" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--network", "btc", "--tabular"] []
        length (Text.lines listed) `shouldBe` 4

    -- The newest payment brought 30000 to the 50000: 50000 + 30000 = 80000
    it "puts the balance after each transaction next to its amount with --tabular" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--network", "btc", "--tabular"] []
        map (Text.isInfixOf "│    +0.00030000 │     0.00080000 │") (Text.lines listed) `shouldBe` [False, False, True, False]
        map (Text.isInfixOf "│    +0.00050000 │     0.00050000 │") (Text.lines listed) `shouldBe` [False, False, False, True]

    -- The newest payment brought 30000 to the 50000 at /0/1: 50000 + 30000
    -- = 80000
    it "shows the balance after each transaction, the newest with what the portfolio holds on the network" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        filter ("balance" `Text.isInfixOf`) (Text.lines listed)
          `shouldBe` [ "  balance 0.00080000 btc, at bip44/0/1",
                       "  balance 0.00050000 btc, at bip44/0/0"
                     ]

    it "shows the balance after each transaction as a field in txs --verbose" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--verbose"] []
        filter ("Balance" `Text.isInfixOf`) (Text.lines listed)
          `shouldBe` ["  Balance  : 0.00080000 btc", "  Balance  : 0.00050000 btc"]

    it "writes the balance after each transaction in satoshis with --json" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        run <- hw home ["txs", "--json"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\"balance\": 80000"
        runOut run `shouldSatisfy` Text.isInfixOf "\"balance\": 50000"

    -- The transaction after the newest one keeps the balance it has in the
    -- whole listing: 80000 - 30000 = 50000
    it "gives the transaction after the one --after names the balance it has in the whole listing" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        Right (newest : _) <- keysOf <$> hwJson home ["txs"] []
        (txBalancesOf <$> hwJson home ["txs", "--after", newest] []) `shouldReturn` Right [Just 50000]

    -- The payment of 50000 comes first, then the one of 30000:
    -- 50000 + 30000 = 80000
    it "lists the oldest transaction first with --reverse, each with the balance after it" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        (txBalancesOf <$> hwJson home ["txs", "--reverse"] []) `shouldReturn` Right [Just 50000, Just 80000]

    it "gives the transaction after the one --after names with --reverse the balance it has in the whole listing" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        Right (oldest : _) <- keysOf <$> hwJson home ["txs", "--reverse"] []
        (txBalancesOf <$> hwJson home ["txs", "--reverse", "--after", oldest] []) `shouldReturn` Right [Just 80000]

    -- The payment of 20000 with its fee of 2260: 50000 - 22260 = 27740
    it "counts an unconfirmed payment in the balance of the newest transaction" $
      withChain paidChain $ \home _ -> do
        _ <- sentPayment home
        _ <- answer home ["sync"] []
        (txBalancesOf <$> hwJson home ["txs"] []) `shouldReturn` Right [Just 27740, Just 50000]

    it "shows no balance in a transaction a command answers with" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0001"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "Balance"

    -- The store saw the payment at 22:13:20 UTC, 23:13:20 in Central
    -- Europe in November
    it "shows the time of a transaction in the time zone of the computer, to the minute" $
      withChain (confirmedChain paymentWithPayerChange) $ \home _ -> do
        synced home
        listed <- runOut <$> hwWithEnv [("TZ", centralEurope)] home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "  14 Nov 2023 23:13     +0.00050000 btc  "

    it "shows the time of a transaction to the second, with its time zone, in txs --verbose" $
      withChain (confirmedChain paymentWithPayerChange) $ \home _ -> do
        synced home
        listed <- runOut <$> hwWithEnv [("TZ", centralEurope)] home ["txs", "--verbose"] []
        listed `shouldSatisfy` Text.isInfixOf "  Time     : 14 Nov 2023 23:13:20 CET\n"

    -- A zone an hour ahead of UTC, and two hours ahead in its summer time,
    -- from 10 to 20 November. 22:13:20 UTC on 14 November is then 00:13:20
    -- the next day. Today's offset would give 23:13, unless the tests run
    -- in that summer time.
    it "shows the time of a transaction with the offset of its day, summer time or not" $
      withChain (confirmedChain paymentWithPayerChange) $ \home _ -> do
        synced home
        listed <- runOut <$> hwWithEnv [("TZ", "XST-1XDT,J314/0,J324/0")] home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "  15 Nov 2023 00:13     +0.00050000 btc  "

    -- Paying the second receiving address from the first leaves 27740 of
    -- change at /1/0: both outputs are the wallet's, and neither can be told
    -- to be the change of the payment. The one at a change address says so.
    it "shows every output of a transaction between addresses of the wallet as sent to, none as change" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        shown <- runOut <$> hw home ["preparetx", fixedTo (externalAddr 1) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        let firstLine = Text.takeWhile (/= '\n') shown
            (_, sent) = Text.breakOn "  To       : " shown
        firstLine `shouldSatisfy` Text.isInfixOf "  Internal  "
        sent `shouldSatisfy` Text.isInfixOf "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  0.00020000 btc  bip44/0/1\n"
        sent `shouldSatisfy` Text.isInfixOf "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0\n             change address\n"
        shown `shouldNotSatisfy` Text.isInfixOf "  Change   : "

{- Pending transactions -}

-- | A home with portfolio0 and portfolio1 of the test wallet, synced with
-- 'twoPortfoliosTx', and a payment prepared in each: 20000 from the coin
-- of 50000 of portfolio0, then 10000 from the coin of 30000 of portfolio1,
-- while it is active. portfolio0 is active again afterwards.
twoPortfoliosPending :: FilePath -> IO ()
twoPortfoliosPending home = do
  syncedTwoPortfolios home
  _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
  switchTo home 0 1
  _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--fee-rate", "10"] []
  switchTo home 0 0

-- | A chain that pays portfolio0 of the test wallet on two networks: on
-- btc 50000 at the bip44 /0/0 and 30000 at the bip44 /0/1, and on bch
-- 50000 at the bip44 /0/0
twoNetworksChain :: Chain
twoNetworksChain = Chain 800002 [(payment, Just 800000), (newerPayment, Just 800000), (cashCoinTx, Just 800000)]

-- | A home with portfolio0 of the test wallet, synced with
-- 'twoNetworksChain', and three payments prepared in this order: one
-- from the coin of 50000 on btc, one on bch, and one from the coin of
-- 30000 on btc. The nosigHash of each, in that order.
threePendingOnTwoNetworks :: FilePath -> IO (String, String, String)
threePendingOnTwoNetworks home = do
  newTestPortfolio home
  _ <- answer home ["receive"] []
  _ <- answer home ["receive", "--network", "bch"] []
  _ <- answer home ["sync"] []
  olderOnBtc <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg payment 0, "--fee-rate", "10"] []
  onBch <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--network", "bch", "--fee-rate", "10"] []
  newerOnBtc <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg newerPayment 0, "--fee-rate", "10"] []
  return (olderOnBtc, onBch, newerOnBtc)

-- | Where each pending transaction pendingtxs lists is
pendingPlacesOf :: Response -> Either String [Place]
pendingPlacesOf (ResponsePendingTxs _ held) = Right (fst <$> held)
pendingPlacesOf other = Left (show other)

-- | The network of each pending transaction pendingtxs lists, with its
-- nosigHash
pendingNetworksOf :: Response -> Either String [(String, String)]
pendingNetworksOf (ResponsePendingTxs _ held) =
  Right [((placeNetwork place).name, Text.unpack (txHashToHex (pendingNosigHash status))) | (place, info) <- held, Pending status <- [txInfoStatus info]]
pendingNetworksOf other = Left (show other)

-- | When each pending transaction pendingtxs lists was created
createdTimesOf :: Response -> Either String [Maybe UTCTime]
createdTimesOf (ResponsePendingTxs _ held) = Right (created . snd <$> held)
  where
    created info = case txInfoStatus info of
      Pending status -> pendingCreated status
      OnChain _ -> Nothing
createdTimesOf other = Left (show other)

pendingListSpec :: Spec
pendingListSpec =
  describe "The pending transactions pendingtxs lists" $ do
    -- portfolio0 is active, and each portfolio has one
    it "are those of every portfolio, each with its place" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        twoPortfoliosPending home
        (pendingPlacesOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [mainPlace, portfolio1Place]

    it "are the same whichever portfolio is active" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        twoPortfoliosPending home
        switchTo home 0 1
        (pendingPlacesOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [mainPlace, portfolio1Place]

    -- The payment is of portfolio0 of wallet1, and portfolio0 of wallet0
    -- is active
    it "are those of every wallet" $
      withChain (chainPaying externalAddr2) $ \home _ -> do
        paymentOfSecondWallet home
        switchTo home 0 0
        (pendingPlacesOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [Place (PortfolioRef 1 0) btc]

    -- The payment on bch was prepared between the two on btc. btc comes
    -- before bch among the networks of a portfolio.
    it "are sorted by network, in the order of the networks of a portfolio, and the newest first on each" $
      withChain twoNetworksChain $ \home _ -> do
        (olderOnBtc, onBch, newerOnBtc) <- threePendingOnTwoNetworks home
        (pendingNetworksOf <$> hwJson home ["pendingtxs"] [])
          `shouldReturn` Right [("btc", newerOnBtc), ("btc", olderOnBtc), ("bch", onBch)]

    it "keep the order of the networks with --reverse, the oldest first on each" $
      withChain twoNetworksChain $ \home _ -> do
        (olderOnBtc, onBch, newerOnBtc) <- threePendingOnTwoNetworks home
        (pendingNetworksOf <$> hwJson home ["pendingtxs", "--reverse"] [])
          `shouldReturn` Right [("btc", olderOnBtc), ("btc", newerOnBtc), ("bch", onBch)]

    it "are those of the --network named alone" $
      withChain twoNetworksChain $ \home _ -> do
        (_, onBch, _) <- threePendingOnTwoNetworks home
        (pendingNetworksOf <$> hwJson home ["pendingtxs", "--network", "bch"] []) `shouldReturn` Right [("bch", onBch)]

    -- The database has one wallet, so no line names it
    it "name the portfolio and the network of each on its second line" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        twoPortfoliosPending home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  portfolio1 btc, "

    it "name the wallet of each portfolio when the database has several wallets" $
      withChain (chainPaying externalAddr2) $ \home _ -> do
        paymentOfSecondWallet home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  portfolio0 of wallet1 btc, "

    -- "wallet1" takes 7, more than "Wallet"
    it "give the wallet a column of the table when the database has several wallets" $
      withChain (chainPaying externalAddr2) $ \home _ -> do
        paymentOfSecondWallet home
        listed <- Text.lines . runOut <$> hw home ["pendingtxs", "--tabular"] []
        take 1 listed `shouldBe` ["Hash     │ Wallet  │ Portfolio  │ Network │ Time              │ Type     │   Amount (btc) │ Status     │ sat/vB │ Tags         │ Description"]

    it "show their portfolio and their network as fields with --verbose" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        twoPortfoliosPending home
        run <- hw home ["pendingtxs", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0\n  Network  : btc\n"

    -- portfolio0 pays 20000 of its coin of 50000 to the first receiving
    -- address of portfolio1, with 2260 of fee: 50000 - 20000 - 2260 =
    -- 27740 come back to its first change address. portfolio1 is active
    -- when pendingtxs lists the payment.
    it "name the portfolio after every path with --verbose, when their portfolio is not the active one" $
      withChain paidChain $ \home _ -> do
        syncedTwoPortfolios home
        _ <- answer home ["preparetx", fixedTo account1Addr "0.0002", "--fee-rate", "10"] []
        switchTo home 0 1
        run <- hw home ["pendingtxs", "--verbose"] []
        filter (\l -> any (`Text.isPrefixOf` l) ["  From", "  To", "  Change"]) (Text.lines (runOut run))
          `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0",
                       "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00020000 btc  bip44/0/0 of portfolio1",
                       "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0 of portfolio0"
                     ]

    -- The same payment while portfolio0, which pays it, is active
    it "leave the paths of the active portfolio alone with --verbose, and name the other portfolio the payment pays" $
      withChain paidChain $ \home _ -> do
        syncedTwoPortfolios home
        _ <- answer home ["preparetx", fixedTo account1Addr "0.0002", "--fee-rate", "10"] []
        run <- hw home ["pendingtxs", "--verbose"] []
        filter (\l -> any (`Text.isPrefixOf` l) ["  From", "  To", "  Change"]) (Text.lines (runOut run))
          `shouldBe` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                       "  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00020000 btc  bip44/0/0 of portfolio1",
                       "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0"
                     ]

    it "take two lines each without --verbose" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["pendingtxs"] []
        length (Text.lines (runOut run)) `shouldBe` 2

    -- Two payments of the default portfolio, two lines each, and a blank
    -- line between them
    it "are set apart by a blank line without --verbose" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg payment 0, "--fee-rate", "10"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg newerPayment 0, "--fee-rate", "10"] []
        run <- hw home ["pendingtxs"] []
        map Text.null (Text.lines (runOut run)) `shouldBe` [False, False, True, False, False]

    -- The time is the wallet's clock when preparetx ran, within a second
    it "have the time they were created" $
      withChain paidChain $ \home _ -> do
        synced home
        started <- getCurrentTime
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        ended <- getCurrentTime
        times <- createdTimesOf <$> hwJson home ["pendingtxs"] []
        let duringRun time = addUTCTime (-1) started <= time && time <= addUTCTime 1 ended
        (fmap (fmap duringRun) <$> times) `shouldBe` Right [Just True]

    it "start with the short id of the nosigHash of an unsigned transaction" $
      withChain paidChain $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isPrefixOf (Text.pack (shortOf nosig) <> "  ")

    it "start with the short id of the txid of a signed transaction" $
      withChain paidChain $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isPrefixOf (Text.pack (shortOf txid) <> "  ")

    it "hide the nosigHash of a signed transaction, even its short id" $
      withChain paidChain $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf (Text.pack (shortOf nosig))

    it "show the time they were created to the second with --verbose" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["pendingtxs", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Created  : "

    it "are none, which pendingtxs says" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "There are no pending transactions"

{- Balances -}

-- | The addresses balances lists, each by its text, with the total, the
-- spendable and the unspendable balances of its coins, the sections of
-- the listing one after the other
balancesOf :: Response -> Either String [(Text, Word64, Word64, Word64)]
balancesOf (ResponseBalances groups) =
  Right [(dBAddressAddress addr, coinBalanceTotal b, coinBalanceSpendable b, coinBalanceUnspendable b) | (g, _) <- groups, (addr, b, _) <- groupRows g]
balancesOf other = Left (show other)

-- | The total balances gives each network of the listing, with what can
-- be spent of it: the value of every coin of the portfolio there
balanceTotalsOf :: Response -> Either String [(String, Word64, Word64)]
balanceTotalsOf (ResponseBalances groups) =
  Right [((placeNetwork (groupPlace g)).name, coinBalanceTotal total, coinBalanceSpendable total) | (g, total) <- groups]
balanceTotalsOf other = Left (show other)

-- The coin of the paid chain is in block 800000, three blocks deep at best
-- block 800002. Only the verbose view gives the block.
coinsListSpec :: Spec
coinsListSpec =
  describe "The coins coins lists" $ do
    it "show the block that holds each coin with --verbose" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["coins", "--verbose"] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

    it "do not show the block without --verbose" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["coins"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "Block"

    it "are shown with every detail by lockcoin, which answers with one" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["lockcoin", coinArg payment 0] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

    it "are shown with every detail by unlockcoin, which answers with one" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg payment 0] []
        run <- hw home ["unlockcoin", coinArg payment 0] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

balancesListSpec :: Spec
balancesListSpec =
  describe "The addresses balances lists" $ do
    -- The second receiving address holds nothing
    it "are those that hold coins, each with the value of its coins and what it can spend" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["receive"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(externalAddrsText !! 0, 50000, 50000, 0)]

    it "count the coin a pending payment spends as unspendable" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(externalAddrsText !! 0, 50000, 0, 50000)]

    -- The payment of 20000 spends the coin of 50000 and leaves 27740 of
    -- change at the first change address
    it "include the change addresses" $
      withChain paidChain $ \home chain -> do
        nosig <- paymentSigned home
        _ <- answer home ["broadcast", nosig, "--yes"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(internalAddrsText !! 0, 27740, 27740, 0)]

    -- The payment of 20000 spends the coin of 50000 at /0/0
    it "tell with --verbose that a pending payment locks the coin it spends" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["balances", "--verbose"] []
        Text.lines (runOut run) `shouldContain` ["  Locked by tx     : 0.00050000 btc"]

    it "show the path of each address, then its total, spendable and unspendable balances" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["balances"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/0  " <> externalAddrsText !! 0 <> "\n  total 0.00050000 btc, spendable 0.00000000 btc, unspendable 0.00050000 btc\n")

    -- 50000 at the bip44 /0/0 and 30000 at the bip84 /0/0 on btc: 50000
    -- + 30000 = 80000. 50000 on bch.
    it "give each network its total, which adds up both accounts on btc" $
      withChain (Chain 800002 [(bothTypesTx, Just 800000), (cashCoinTx, Just 800000)]) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--type", "bip84"] []
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        (balanceTotalsOf <$> hwJson home ["balances"] []) `shouldReturn` Right [("btc", 80000, 80000), ("bch", 50000, 50000)]

    -- The title line of the network: its place, then the total of its
    -- coins, what can be spent now, and the rest
    it "give the total of the network in the title of its section" $
      withChain (confirmedChain bothTypesTx) $ \home _ -> do
        syncedBothTypes home
        run <- hw home ["balances"] []
        take 1 (Text.lines (runOut run))
          `shouldBe` ["portfolio0  btc  total 0.00080000 btc, spendable 0.00080000 btc, unspendable 0.00000000 btc"]

    -- --network narrows the listing to one network, and the title stays
    it "give the total of the network in the title of its section, though --network names one" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["balances", "--network", "btc"] []
        take 1 (Text.lines (runOut run))
          `shouldBe` ["portfolio0  btc  total 0.00050000 btc, spendable 0.00050000 btc, unspendable 0.00000000 btc"]

    it "are none before a payment, which balances says" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["balances"] []
        runOut run `shouldSatisfy` Text.isInfixOf "There are no addresses with coins to display"

{- Sections -}

-- | The lines of a listing that are the titles of its sections: those
-- that name portfolio0
titlesOf :: Text -> [Text]
titlesOf = filter (Text.isPrefixOf "portfolio0") . Text.lines

-- A listing over the whole portfolio covers several places, so each
-- section says where it is on a line of its own
sectionsSpec :: Spec
sectionsSpec =
  describe "A listing over a whole portfolio" $ do
    -- The addresses of the bip44 account come before those of the bip84
    -- account
    it "lists the addresses of each account under a title that names the account" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--type", "bip84"] []
        (titlesOf . runOut <$> hw home ["addresses"] []) `shouldReturn` ["portfolio0  btc  bip44", "portfolio0  btc  bip84"]

    -- A blank line ends each address, the last of a section included,
    -- before the title of the next section
    it "sets the sections apart with a blank line" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--type", "bip84"] []
        (Text.lines . runOut <$> hw home ["addresses"] [])
          `shouldReturn` [ "portfolio0  btc  bip44",
                           "",
                           "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
                           "  unused",
                           "",
                           "portfolio0  btc  bip84",
                           "",
                           "bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc",
                           "  unused"
                         ]

    -- The bip44 account has the addresses /0/0 and /0/1, and the bip84
    -- account comes after it, as without --reverse
    it "keeps the sections in their order with --reverse, each from its first address" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--type", "bip84"] []
        (Text.lines . runOut <$> hw home ["addresses", "--reverse"] [])
          `shouldReturn` [ "portfolio0  btc  bip44",
                           "",
                           "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
                           "  unused",
                           "",
                           "bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
                           "  unused",
                           "",
                           "portfolio0  btc  bip84",
                           "",
                           "bip84/0/0  bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc",
                           "  unused"
                         ]

    -- btc comes before bch among the networks of a portfolio
    it "lists the coins of each network under a title that names the network" $
      withChain (Chain 800002 [(payment, Just 800000), (cashCoinTx, Just 800000)]) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        (titlesOf . runOut <$> hw home ["coins"] []) `shouldReturn` ["portfolio0  btc", "portfolio0  bch"]

    it "lists the transactions of each network under a title that names the network" $
      withChain (Chain 800002 [(payment, Just 800000), (cashCoinTx, Just 800000)]) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        (titlesOf . runOut <$> hw home ["txs"] []) `shouldReturn` ["portfolio0  btc", "portfolio0  bch"]

    -- The payment of 50000 on btc: the title, a blank line, as above the
    -- addresses, then the two lines of the payment
    it "titles the one network that has rows, and leaves the others out" $
      withChain paidChain $ \home _ -> do
        synced home
        listed <- Text.lines . runOut <$> hw home ["txs"] []
        (take 1 listed, Text.null <$> listed) `shouldBe` (["portfolio0  btc"], [False, True, False, False])

    -- The title of a network in --verbose and --tabular too
    it "titles each section in every view" $
      withChain paidChain $ \home _ -> do
        synced home
        forM_ ["--compact", "--verbose", "--tabular"] $ \view -> do
          listed <- Text.lines . runOut <$> hw home ["coins", view] []
          (view, take 1 listed) `shouldBe` (view, ["portfolio0  btc"])

    -- The database has wallet0 and wallet1, and portfolio0 of wallet1 is
    -- active
    it "names the wallet of the portfolio in the titles when the database has several wallets" $
      withChain (chainPaying externalAddr2) $ \home _ -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        switchTo home 1 0
        _ <- answer home ["receive"] []
        _ <- answer home ["sync"] []
        (take 1 . Text.lines . runOut <$> hw home ["coins"] []) `shouldReturn` ["portfolio0 of wallet1  btc"]

    -- The same two wallets, with portfolio0 of wallet0 active: portfolio0
    -- alone could be that of either wallet
    it "names wallet0 too in the titles when the database has several wallets" $
      withChain paidChain $ \home _ -> do
        synced home
        newPortfolioOf home testMnemonic2
        (take 1 . Text.lines . runOut <$> hw home ["coins"] []) `shouldReturn` ["portfolio0 of wallet0  btc"]

    -- portfolio0 of wallet0 pays 20000 of its coin of 50000 to the first
    -- receiving address of portfolio0 of wallet1, with 2260 of fee:
    -- 50000 - 20000 - 2260 = 27740 come back to its first change address.
    -- portfolio0 of wallet1 is active when show answers. A portfolio0
    -- named alone would read there as the active one.
    it "names the wallet of every portfolio of a payment between two wallets, wallet0 too" $
      withChain paidChain $ \home _ -> do
        synced home
        newPortfolioOf home testMnemonic2
        switchTo home 1 0
        _ <- answer home ["receive"] []
        switchTo home 0 0
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo externalAddr2 "0.0002", "--fee-rate", "10"] []
        switchTo home 1 0
        run <- hw home ["show", nosig] []
        filter (\l -> any (`Text.isPrefixOf` l) ["  Portfolio", "  From", "  To", "  Change"]) (Text.lines (runOut run))
          `shouldBe` [ "  Portfolio: portfolio0 of wallet0, not the active portfolio0 of wallet1",
                       "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0 of wallet0",
                       "  To       : 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2  0.00020000 btc  bip44/0/0 of portfolio0 of wallet1",
                       "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0 of portfolio0 of wallet0"
                     ]

{- The answer of sync -}

-- sync goes over every network of every portfolio, and most of them
-- hold nothing
syncAnswerSpec :: Spec
syncAnswerSpec =
  describe "The answer of sync" $
    -- portfolio0 holds the coin of 50000 on btc, and nothing else. The
    -- sync before found it, so this one finds nothing new.
    it "shows each network a portfolio holds something on, then the quiet networks of each portfolio in one line" $
      withChain paidChain $ \home _ -> do
        syncedTwoPortfolios home
        run <- hw home ["sync"] []
        take 6 (Text.lines (runOut run))
          `shouldBe` [ "portfolio0  btc",
                       "  spendable 0.00050000 btc, 1 coin",
                       "  synced at block 800002, nothing new",
                       "",
                       "portfolio0  nothing on bch, btctest, btctest4, btcreg, bchtest, bchtest4 and bchreg",
                       "portfolio1  nothing on btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4 and bchreg"
                     ]

{- The pager -}

-- | Run hw on a terminal through a pager that keeps what it was given:
-- the options hw gave less in LESS, and the lines of the listing or of
-- the help.
paged :: FilePath -> [String] -> IO (Text, [Text])
paged home args = do
  environment <- pagerOfTests home
  _ <- hwOnScreen environment home args
  kept home

-- | Put the pager of the tests in a home, and give the environment that
-- has hw use it. The pager is a script named less, which answers
-- less --version as less 668 does, so that hw gives it the options of
-- that version. It keeps the options hw gave it in LESS and the lines it
-- was given, in two files of the home, and prints the lines as a pager
-- does. hw looks for the pager on the PATH, and NO_COLOR keeps what it
-- prints plain.
pagerOfTests :: FilePath -> IO [(String, String)]
pagerOfTests home = do
  let pager = home </> "less"
  writeFile pager $
    unlines
      [ "#!/bin/sh",
        "if [ \"$1\" = --version ]; then echo 'less 668 (a pager of the tests)'; exit 0; fi",
        "printf '%s' \"$LESS\" > \"$HOME/less-options\"",
        "exec /usr/bin/tee \"$HOME/paged\""
      ]
  setFileMode pager 0o700
  return [("HW_PAGER", pager), ("PATH", "/usr/bin:/bin"), ("NO_COLOR", "1")]

-- | What the pager of the tests kept in a home: the options hw gave it,
-- and the lines it was given
kept :: FilePath -> IO (Text, [Text])
kept home = do
  options <- Text.pack <$> readFile (home </> "less-options")
  lines' <- Text.lines . Text.pack <$> readFile (home </> "paged")
  return (options, lines')

-- The terminal of the tests has no size, so every table counts as taller
-- than the screen, which is when less can keep its header at the top
pagerSpec :: Spec
pagerSpec =
  describe "A listing on a terminal" $ do
    it "goes through the pager whole, as hw prints it without one" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (_, listed) <- paged home ["addresses"]
        listed `shouldBe` ["portfolio0  btc  bip44", "", "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "  unused"]

    -- --header=2 keeps the line of headings and the line under it
    it "keeps the header of a table on the screen as its rows scroll, when the listing is of one place" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (fst <$> paged home ["addresses", "--network", "btc", "--type", "bip44", "--tabular"]) `shouldReturn` "FRXS --header=2"

    it "keeps the header of the table of portfolios on the screen as its rows scroll" $
      withHome $ \home -> do
        newTestPortfolio home
        (fst <$> paged home ["portfolios", "--tabular"]) `shouldReturn` "FRXS --header=2"

    -- The first lines of the listing are the title of its section, not
    -- the header of the table
    it "lets the header of a table scroll with its rows under the title of a section" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (fst <$> paged home ["addresses", "--tabular"]) `shouldReturn` "FRXS"

    it "lets the header of a table of balances scroll with its rows, under the title of its network" $
      withChain paidChain $ \home _ -> do
        synced home
        (fst <$> paged home ["balances", "--network", "btc", "--tabular"]) `shouldReturn` "FRXS"

    -- Each account ends with its descriptor, which the user copies whole
    it "wraps the lines of portfolios --verbose rather than cut them" $
      withHome $ \home -> do
        newTestPortfolio home
        (fst <$> paged home ["portfolios", "--verbose"]) `shouldReturn` "FRX"

    -- Each address on one line of the screen, cut at its edge
    it "cuts the lines of a compact listing at the edge of the screen" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (fst <$> paged home ["addresses"]) `shouldReturn` "FRXS"

    -- The terminal of the tests gives hw no PATH, so less, the pager
    -- without HW_PAGER, cannot be found. A new line reaches the terminal
    -- as a carriage return and a line feed.
    it "is printed without a pager, which hw says, when there is no PATH to find one" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hwOnScreen [("NO_COLOR", "1")] home ["addresses"]
        (runExit run, runErr run, runOut run)
          `shouldBe` ( ExitSuccess,
                       "The pager less was not found. The listing is printed without one.\n",
                       "portfolio0  btc  bip44\r\n\r\nbip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\r\n  unused\r\n"
                     )

-- unlockcoin has a short help, and needs a coin as its argument
helpPagerSpec :: Spec
helpPagerSpec =
  describe "A help on a terminal" $ do
    it "of a command goes through the pager, from its summary to its last line" $
      withHome $ \home -> do
        (_, shown) <- paged home ["unlockcoin", "--help"]
        (take 1 shown, drop (length shown - 1) shown)
          `shouldBe` (["Unlock a coin locked with lockcoin"], ["See also: lockcoin, deletetx, coins"])

    it "of the program goes through the pager" $
      withHome $ \home -> do
        (_, shown) <- paged home ["--help"]
        take 1 shown `shouldBe` ["hw is a BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash. It stores public"]

    -- A help is sentences: one cut at the edge of a narrow screen would
    -- be read by scrolling sideways
    it "has its lines wrapped by the pager rather than cut" $
      withHome $ \home ->
        (fst <$> paged home ["unlockcoin", "--help"]) `shouldReturn` "FRX"

    -- The pager of the tests would keep the help in a file of the home
    it "is printed as it is with --no-pager, through no pager" $
      withHome $ \home -> do
        environment <- pagerOfTests home
        run <- hwOnScreen environment home ["unlockcoin", "--help", "--no-pager"]
        runOut run `shouldSatisfy` Text.isPrefixOf "Unlock a coin locked with lockcoin\r\n"
        doesFileExist (home </> "paged") `shouldReturn` False

    it "of the program goes through the pager when nothing is typed, and hw exits as a failure" $
      withHome $ \home -> do
        environment <- pagerOfTests home
        run <- hwOnTerminal environment home [] []
        (_, shown) <- kept home
        (runExit run, take 1 shown)
          `shouldBe` (ExitFailure 1, ["hw is a BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash. It stores public"])

    -- The help of a command typed without its arguments is a failure, on
    -- stderr. The terminal of this test is stderr, and stdout is a pipe.
    it "of a command typed alone is printed by the pager on stderr, and nothing on stdout" $
      withHome $ \home -> do
        environment <- pagerOfTests home
        run <- hwOnTerminal environment home ["unlockcoin"] []
        runExit run `shouldBe` ExitFailure 1
        runOut run `shouldBe` ""
        runErr run `shouldSatisfy` Text.isPrefixOf "Unlock a coin locked with lockcoin\r\n"

    -- The terminal of the tests gives hw no PATH, so less, the pager
    -- without HW_PAGER, cannot be found
    it "is printed without a pager, which hw says, when there is no PATH to find one" $
      withHome $ \home -> do
        run <- hwOnScreen [("NO_COLOR", "1")] home ["unlockcoin", "--help"]
        runErr run `shouldBe` "The pager less was not found. The help is printed without one.\n"
        runOut run `shouldSatisfy` Text.isPrefixOf "Unlock a coin locked with lockcoin\r\n"

{- Next steps -}

-- | The short id of the nosigHash of the one pending transaction of a
-- home, its first 8 digits, by which the next steps name it
onePendingShortId :: FilePath -> IO Text
onePendingShortId home = Text.pack . shortOf <$> (nosigOf =<< onePendingTx home)

-- | Whether the output of a run shows the command
showsCommand :: Text -> Run -> Bool
showsCommand command run = command `Text.isInfixOf` runOut run

{- The QR code of an address -}

-- | What a person reads on a terminal, without colours
screen :: FilePath -> [String] -> IO [Text]
screen home args = Text.lines . Text.filter (/= '\r') . runOut <$> hwOnScreen [("NO_COLOR", "1")] home args

-- | The lines of an answer under the text of an address, which takes 9:
-- its path and the address, seven figures, and its account
codeUnderAddress :: [Text] -> [Text]
codeUnderAddress = drop 9

-- An address given out, as a terminal receives it
addressCodeSpec :: Spec
addressCodeSpec = do
  describe "An address on a terminal" $ do
    it "comes with its QR code from receive, under its text" $
      withHome $ \home -> do
        newTestPortfolio home
        (codeUnderAddress <$> screen home ["receive"]) `shouldReturn` p2pkhAddressCode

    -- The eighth line tells when it was created, which changes. The ninth
    -- names the account of the address.
    it "keeps its text whole above the code, with nothing beside it" $
      withHome $ \home -> do
        newTestPortfolio home
        shown <- screen home ["receive", "--label", "rent"]
        (take 7 shown, take 1 (drop 8 shown))
          `shouldBe` ( [ "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"rent\"",
                         "  Transactions: 0",
                         "  Received    : 0.00000000 btc",
                         "  Confirmed   : 0.00000000 btc",
                         "  Unconfirmed : 0.00000000 btc",
                         "  Spendable   : 0.00000000 btc",
                         "  Coins       : 0"
                       ],
                       ["  Account     : portfolio0  btc  bip44"]
                     )

    it "shows the same text as off a terminal" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        piped <- hw home ["show", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"] []
        (take 9 <$> screen home ["show", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]) `shouldReturn` Text.lines (runOut piped)

    it "draws the code of a BIP84 account from its bech32 address" $
      withHome $ \home -> do
        newTestPortfolio home
        (codeUnderAddress <$> screen home ["receive", "--type", "bip84"]) `shouldReturn` p2wpkhAddressCode

    -- Black is colour 16 of the palette, and white 231
    it "draws the code in black on white, whatever the colours of the terminal" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hwOnScreen [] home ["receive"]
        runOut run `shouldSatisfy` Text.isInfixOf "\ESC[0;38;5;16;48;5;231m"

    it "sets no colour with NO_COLOR, and still draws the code" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hwOnScreen [("NO_COLOR", "1")] home ["receive"]
        runOut run `shouldSatisfy` (not . Text.isInfixOf "\ESC[")
        runOut run `shouldSatisfy` Text.isInfixOf "█"

    it "has no code when the answer goes to a pipe" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["receive"] []
        runOut run `shouldSatisfy` (not . Text.isInfixOf "█")

    it "has no code with --json, even on a terminal" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hwOnScreen [] home ["receive", "--json"]
        runOut run `shouldSatisfy` (not . Text.isInfixOf "█")

    it "has no code in the answer of label, which gives out no address" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hwOnScreen [] home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "rent"]
        runOut run `shouldSatisfy` (not . Text.isInfixOf "█")

  describe "show for an address" $ do
    it "shows the QR code of an address the wallet holds" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (codeUnderAddress <$> screen home ["show", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]) `shouldReturn` p2pkhAddressCode

    it "hands out no new address" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["show", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"] []
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "")]

    it "answers in JSON as receive does" $
      withHome $ \home -> do
        newTestPortfolio home
        given <- hwJson home ["receive"] []
        (addressesOf <$> hwJson home ["show", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"] []) `shouldReturn` addressesOf given

    -- 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1 is /0/3 of portfolio0, which receive
    -- has not given out
    it "refuses an address the wallet does not hold, and shows no code" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hwOnScreen [] home ["show", "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"]
        runExit run `shouldBe` ExitFailure 1
        runOut run `shouldSatisfy` Text.isInfixOf "The wallet has no address 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"
        runOut run `shouldSatisfy` (not . Text.isInfixOf "█")

    -- The default type is bip44, and the address is of the bip84 account
    it "finds the account of the address, whatever the default type" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--type", "bip84"] []
        (accountRefOf <$> hwJson home ["show", "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc"] []) `shouldReturn` Right (AccountRef mainPlace Bip84)

    it "takes a Bitcoin Cash address without its prefix, and shows the whole address" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        (take 1 <$> screen home ["show", "qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"])
          `shouldReturn` ["bip44/0/0  bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"]

    it "shows a change address at its path on the change chain" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (take 1 <$> screen home ["show", "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"])
          `shouldReturn` ["bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"]

  describe "show for a path or for the newest address" $ do
    it "shows the QR code at a path the wallet holds" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (codeUnderAddress <$> screen home ["show", "/0/0"]) `shouldReturn` p2pkhAddressCode

    -- receive gave out none: /0/0 may lie past the gap of a restored wallet
    it "shows no code at a path the wallet does not hold" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hwOnScreen [] home ["show", "/0/0"]
        runOut run `shouldSatisfy` (not . Text.isInfixOf "█")

    it "shows the QR code of the newest receiving address without a word" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (codeUnderAddress <$> screen home ["show"]) `shouldReturn` p2pkhAddressCode

-- The pretty output, after the answer
nextStepSpec :: Spec
nextStepSpec =
  describe "The next step of a payment or a sweep, shown after the answer" $ do
    it "is to sign the transaction preparetx made, or to export it to sign it offline, by the short id of its nosigHash" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signtx " <> short)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> short <> " FILE")

    it "is to sign the file preparetx wrote on an offline computer, or the transaction here" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "unsigned.psbt", "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signtx --input " <> Text.pack (home </> "unsigned.psbt") <> "\n")
        run `shouldSatisfy` showsCommand ("hw signtx " <> short)

    it "is to sign the transaction preparesendall made, or to export it to sign it offline" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signtx " <> short)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> short <> " FILE")

    it "is to send the transaction signtx signed in the wallet, by the short id of its txid" $
      withChain paidChain $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["signtx", nosig] seedOnce
        txid <- txidOf =<< onePendingTx home
        run `shouldSatisfy` showsCommand ("hw broadcast " <> Text.pack (shortOf txid) <> "\n")

    it "is to send the file signtx signed offline, from the online computer" $
      withHome $ \signer -> withChain paidChain $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        run `shouldSatisfy` showsCommand ("hw broadcast --input " <> Text.pack (signer </> "signed.psbt"))

    it "is to send the --input file signtx replaced with the signed transaction" $
      withHome $ \signer -> withChain paidChain $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.psbt"] (["y"] <> seedOnce)
        run `shouldSatisfy` showsCommand ("hw broadcast --input " <> Text.pack (online </> "unsigned.psbt"))

    -- sync covers every portfolio on every network
    it "is to sync the wallet once broadcast sent the transaction" $
      withChain paidChain $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["broadcast", nosig, "--yes"] []
        run `shouldSatisfy` showsCommand "hw sync\n"

    -- The payment is in the active portfolio, which txs lists without
    -- an option. The sync checked is the first one, which finds the
    -- payment.
    it "is to see the transactions sync found, with no option for the active portfolio" $
      withChain paidChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw txs\n"

    -- txs would show what it showed before that sync
    it "is none after a sync that found no new transaction" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["sync"] []
        run `shouldSatisfy` (not . showsCommand "hw txs\n")

    -- The chain pays 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, the first
    -- receiving address of portfolio1, and nothing of portfolio0, which is
    -- active. txs lists the active portfolio alone, so the steps start with
    -- the switch to portfolio1. The sync checked is the first one, which
    -- finds the payment.
    it "starts with the switch to see the transactions sync found in another portfolio than the active one" $
      withChain (chainPaying account1Addr) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        _ <- answer home ["receive"] []
        switchTo home 0 0
        run <- hw home ["sync"] []
        run
          `shouldSatisfy` showsCommand
            "Next, switch to portfolio1 of wallet0, where the sync found new transactions:\n\
            \  hw switch 1 --wallet 0\n\
            \Then see the transactions:\n\
            \  hw txs\n"

    -- Another transaction of the mempool spends the coin of the payment,
    -- which is of the active portfolio
    it "is to find and delete the pending transaction a sync found in conflict" $
      withChain paidChain $ \home chain -> do
        _ <- paymentSigned home
        let other = spendingTx [(txHash payment, 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw pendingtxs\n"
        run `shouldSatisfy` showsCommand "hw deletetx HASH"
        run `shouldNotSatisfy` showsCommand "hw switch"

    -- The payment in conflict is of portfolio0, and portfolio1 is active.
    -- pendingtxs lists both.
    it "is to find and delete the pending transaction a sync found in conflict, without a switch, when another portfolio is active" $
      withChain paidChain $ \home chain -> do
        _ <- paymentSigned home
        newTestPortfolio home
        switchTo home 0 1
        let other = spendingTx [(txHash payment, 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        run <- hw home ["sync"] []
        run
          `shouldSatisfy` showsCommand
            "Next, find the pending transaction in conflict, which can no longer be sent:\n\
            \  hw pendingtxs\n\
            \Then delete it, to free its coins:\n\
            \  hw deletetx HASH\n"
        run `shouldNotSatisfy` showsCommand "hw switch"

    -- A payment is signed in each portfolio, from its coin of
    -- 'twoPortfoliosTx', and the mempool spends both coins otherwise
    it "is to find and delete the pending transactions a sync found in conflict in several portfolios, without a switch" $
      withChain (confirmedChain twoPortfoliosTx) $ \home chain -> do
        syncedTwoPortfolios home
        forM_ [0, 1] $ \portfolio -> do
          switchTo home 0 portfolio
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--fee-rate", "10"] []
          answer home ["signtx", nosig] seedOnce
        let other = spendingTx [(txHash twoPortfoliosTx, 0), (txHash twoPortfoliosTx, 1)] [(otherAddr 1, 70000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        run <- hw home ["sync"] []
        run
          `shouldSatisfy` showsCommand
            "Next, find the pending transactions in conflict, which can no longer be sent:\n\
            \  hw pendingtxs\n\
            \Then delete them, to free their coins:\n\
            \  hw deletetx HASH\n"
        run `shouldNotSatisfy` showsCommand "hw switch"

    -- The key of the old wallet is in a file, and the sweep pays
    -- portfolio0
    it "is to sign the sweep preparesweep made with the keys of its file, or to export it to sign it where the keys are" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        run <- hw home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signsweep " <> short <> " --key-file " <> Text.pack keys)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> short <> " FILE")

    -- Prepared from the address of the key, without it: signsweep asks
    -- for the key on the terminal
    it "is to sign the sweep preparesweep made from addresses, the keys typed when asked" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signsweep " <> short <> "\n")

    it "is to sign the file of the sweep where the keys are, when preparesweep wrote one" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-to", textOf (externalAddr 0), "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signsweep --input " <> Text.pack (home </> "sweep.json") <> "\n")

    -- In a database without wallets, the sweep is in its file only
    it "is to sign the file of a sweep outside the wallet with the keys of its file" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        run <- hw home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signsweep --input " <> Text.pack (home </> "sweep.json") <> " --key-file " <> Text.pack keys)

    it "is to send the sweep signsweep signed in the wallet, by the short id of its txid" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        run <- hw home ["signsweep", nosig, "--key-file", keys] []
        txid <- txidOf =<< onePendingTx home
        run `shouldSatisfy` showsCommand ("hw broadcast " <> Text.pack (shortOf txid) <> "\n")

    it "is to send the file of a sweep outside the wallet that signsweep signed" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run <- hw home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.psbt", "--key-file", keys] []
        run `shouldSatisfy` showsCommand ("hw broadcast --input " <> Text.pack (home </> "signed.psbt"))

    -- The name of the file with a space would be two words for the shell
    it "quotes a file name the shell would split" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "my payment.psbt", "--fee-rate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signtx --input '" <> Text.pack (home </> "my payment.psbt") <> "'\n")

    it "is not shown with --json, whose output is the answer alone" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--json", "--fee-rate", "10"] []
        run `shouldNotSatisfy` showsCommand "hw signtx"

{- CSV files -}

-- | The listings --csv writes to a file
csvSpec :: Spec
csvSpec =
  describe "A listing written with --csv" $ do
    let -- The lines of a CSV file, without the mark it starts with
        linesOf file = filter (not . Text.null) . Text.splitOn "\r\n" . Text.drop 1 . decodeUtf8 <$> BS.readFile file
        -- A line without its last three fields: when the wallet created
        -- the element, which is the time of the test
        withoutCreated = Text.intercalate "," . reverse . drop 3 . reverse . Text.splitOn ","
        balanceHeadings = "Wallet,Portfolio,Network,Type,Address,Path,Chain,Address label,Address tags,Total (sats),Spendable (sats),Locked by tx (sats),Locked by you (sats),Not spendable yet (sats),Confirmed (sats),Unconfirmed (sats),Received (sats),Coins,Transactions,Created (UTC),Created (local),Created (offset)"

    -- The coin of the payment, 3 confirmations deep, can be spent: 50000
    -- in all and spendable, nothing locked or waiting. The store counts
    -- 50000 confirmed and received at /0/0, in one coin and one
    -- transaction.
    it "is written to the file --csv names, a row for each address that holds coins, and hw says where" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.csv"
        run <- hw home ["balances", "--csv", file] []
        runOut run `shouldBe` "Written to " <> Text.pack file <> "\n"
        rows <- linesOf file
        (take 1 rows, withoutCreated <$> drop 1 rows)
          `shouldBe` ([balanceHeadings], ["wallet0,portfolio0,btc,bip44,1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,/0/0,receiving,,,50000,50000,0,0,0,50000,0,50000,1,1"])

    -- EF BB BF is the byte order mark of UTF-8: 3 bytes, then the 24
    -- characters of the first three headings and their commas
    it "starts with the byte order mark of UTF-8, then its headings" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.csv"
        _ <- answer home ["balances", "--csv", file] []
        BS.take 27 <$> BS.readFile file `shouldReturn` "\xEF\xBB\xBF\&Wallet,Portfolio,Network"

    -- The store saw the payment at 22:13:20 UTC, 23:13:20 in Central
    -- Europe in November, an hour ahead of UTC
    it "gives the time of a transaction in UTC, then on the clock of the computer, then how far that clock is from UTC" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "txs.csv"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["txs", "--csv", file] []
        content <- BS.readFile file
        content `shouldSatisfy` BS.isInfixOf ",2023-11-14T22:13:20,2023-11-14T23:13:20,UTC+01:00,Incoming,"

    it "names the wallet, the portfolio, the network and the type on every row, though the listing covers one portfolio" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "coins.csv"
        _ <- answer home ["coins", "--csv", file] []
        rows <- linesOf file
        Text.intercalate "," . take 4 . Text.splitOn "," <$> rows
          `shouldBe` ["Wallet,Portfolio,Network,Type", "wallet0,portfolio0,btc,bip44"]

    it "writes the headings alone for a listing with no rows" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "pending.csv"
        _ <- answer home ["pendingtxs", "--csv", file] []
        linesOf file
          `shouldReturn` ["Wallet,Portfolio,Network,Type,Nosighash,Short nosighash,Txid,Short txid,Direction,Amount (sats),Fee (sats),Fee rate (sat/vB),Size (vB),Change in fee (sats),Status,Replaces,Transaction label,Transaction tags,From address,From path,From label,From tags,From value (sats),To address,To path,To label,To tags,To value (sats),Change address,Change path,Change tags,Change value (sats),Other addresses,Lock time,Created (UTC),Created (local),Created (offset)"]

    -- 50000 at /0/0 is the largest total, before 30000 at /0/1
    it "writes the rows --max allows, the first ones of the listing" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        let file = home </> "balances.csv"
        _ <- answer home ["balances", "--csv", file, "--max", "1"] []
        rows <- linesOf file
        (take 1 rows, withoutCreated <$> drop 1 rows)
          `shouldBe` ([balanceHeadings], ["wallet0,portfolio0,btc,bip44,1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,/0/0,receiving,,,50000,50000,0,0,0,50000,0,50000,1,1"])

    -- 30000 at /0/1 is the smallest total
    it "writes the rows in the reverse order with --reverse" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        let file = home </> "balances.csv"
        _ <- answer home ["balances", "--csv", file, "--reverse", "--max", "1"] []
        rows <- linesOf file
        (take 1 rows, withoutCreated <$> drop 1 rows)
          `shouldBe` ([balanceHeadings], ["wallet0,portfolio0,btc,bip44,1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg,/0/1,receiving,,,30000,30000,0,0,0,30000,0,30000,1,1"])

    -- The key listing-reverse changes the screen alone. 50000 at /0/0 is
    -- the largest total, the first row of balances in the usual order.
    it "writes the rows in the usual order when the configuration file reverses the listings" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        setListingReverse home True
        let file = home </> "balances.csv"
        _ <- answer home ["balances", "--csv", file, "--max", "1"] []
        rows <- linesOf file
        (take 1 rows, withoutCreated <$> drop 1 rows)
          `shouldBe` ([balanceHeadings], ["wallet0,portfolio0,btc,bip44,1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,/0/0,receiving,,,50000,50000,0,0,0,50000,0,50000,1,1"])

    -- Each network of the portfolio on a row of its own, eight in all,
    -- after the row of the headings
    it "gives the portfolios a row for each network" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "portfolios.csv"
        _ <- answer home ["portfolios", "--csv", file] []
        rows <- linesOf file
        (take 1 rows, length rows) `shouldBe` (["Wallet,Portfolio,Network,Type,Active,Description,Wallet description,Account paths,Spendable (sats),Confirmed (sats),Unconfirmed (sats),Coins,Addresses,Created (UTC),Created (local),Created (offset)"], 9)

    -- A portfolio has a bip44 and a bip84 account on Bitcoin and on its
    -- test networks, and a bip44 account alone on Bitcoin Cash and on its
    -- test networks. The two types of a row are one field, in quotes
    -- since it holds a comma.
    it "starts each row of the portfolios with its wallet, its portfolio, its network and the types of its accounts there, then yes for the active portfolio and no for the others" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        let file = home </> "portfolios.csv"
        _ <- answer home ["portfolios", "--csv", file] []
        rows <- drop 1 <$> linesOf file
        let expected =
              [ "wallet0,portfolio0,btc,\"bip44, bip84\",no,",
                "wallet0,portfolio0,bch,bip44,no,",
                "wallet0,portfolio0,btctest,\"bip44, bip84\",no,",
                "wallet0,portfolio0,btctest4,\"bip44, bip84\",no,",
                "wallet0,portfolio0,btcreg,\"bip44, bip84\",no,",
                "wallet0,portfolio0,bchtest,bip44,no,",
                "wallet0,portfolio0,bchtest4,bip44,no,",
                "wallet0,portfolio0,bchreg,bip44,no,",
                "wallet0,portfolio1,btc,\"bip44, bip84\",yes,",
                "wallet0,portfolio1,bch,bip44,yes,",
                "wallet0,portfolio1,btctest,\"bip44, bip84\",yes,",
                "wallet0,portfolio1,btctest4,\"bip44, bip84\",yes,",
                "wallet0,portfolio1,btcreg,\"bip44, bip84\",yes,",
                "wallet0,portfolio1,bchtest,bip44,yes,",
                "wallet0,portfolio1,bchtest4,bip44,yes,",
                "wallet0,portfolio1,bchreg,bip44,yes,"
              ]
        [Text.take (Text.length start) row | (start, row) <- zip expected rows] `shouldBe` expected
        length rows `shouldBe` 16

    it "is refused a --csv file that exists, and the file is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "portfolios.csv"
        writeFile file "kept\n"
        run <- hw home ["portfolios", "--csv", file] []
        run `shouldSatisfy` printed (alreadyExists file)
        readFile file `shouldReturn` "kept\n"

    it "can be read and written by its owner alone" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "portfolios.csv"
        _ <- answer home ["portfolios", "--csv", file] []
        (.&. 0o777) . fileMode <$> getFileStatus file `shouldReturn` 0o600

    it "is answered in --json by the file written" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "portfolios.csv"
        hwJson home ["portfolios", "--csv", file] [] `shouldReturn` ResponseFile file

{- The spreadsheet of exportsheet -}

-- | The spreadsheet exportsheet writes
exportSheetSpec :: Spec
exportSheetSpec =
  describe "The spreadsheet of exportsheet" $ do
    let contentOf file = do
          archive <- toArchive <$> BL.readFile file
          maybe (fail "The spreadsheet has no content.xml") (return . decodeUtf8 . BL.toStrict . fromEntry) (findEntryByPath "content.xml" archive)
        -- The names of the tables, in the order of the file
        tabsOf content = [Text.takeWhile (/= '"') rest | rest <- drop 1 (Text.splitOn "<table:table table:name=\"" content)]

    -- The type of the file shows at byte 30, after the header of the
    -- first entry of the archive and its name, mimetype
    it "is written to the file exportsheet names, as an OpenDocument spreadsheet, and hw says where" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "wallet.ods"
        run <- hw home ["exportsheet", file] []
        runOut run `shouldBe` "Written to " <> Text.pack file <> "\n"
        BS.take 54 . BS.drop 30 <$> BS.readFile file `shouldReturn` "mimetypeapplication/vnd.oasis.opendocument.spreadsheet"

    it "has a tab for each listing, in the order of the help, one for the addresses of the transactions after each tab of transactions, and one for the address book last" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        tabsOf <$> contentOf file `shouldReturn` ["Portfolios", "Addresses", "Balances", "Coins", "Transactions", "Transaction addresses", "Pending transactions", "Pending transaction addresses", "Address book"]

    -- The payment pays 50000 at /0/0 of the bip44 account,
    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, a receiving address without a
    -- label, which portfolio0 of wallet0 holds
    it "gives the address a payment was received at a row in the tab of the addresses of transactions: its role, its value, the address, its path and chain, and the portfolio that holds it" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        content <- contentOf file
        let (_, fromTab) = Text.breakOn "<table:table table:name=\"Transaction addresses\"" content
            tab = fst (Text.breakOn "</table:table>" fromTab)
        tab
          `shouldSatisfy` Text.isInfixOf
            ( Text.concat
                [ "<table:table-cell office:value-type=\"string\"><text:p>to</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"50000\"><text:p>50000</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>/0/0</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>receiving</text:p></table:table-cell>",
                  -- No label
                  "<table:table-cell/>",
                  -- No tags
                  "<table:table-cell/>",
                  "<table:table-cell office:value-type=\"string\"><text:p>wallet0</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>portfolio0</text:p></table:table-cell>"
                ]
            )

    -- The prepared payment spends from one address of the portfolio, pays
    -- one address of another wallet and sends its change back: 3 rows,
    -- after the row of the headings
    it "gives each address of a pending transaction a row in the tab of the addresses of pending transactions" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        content <- contentOf file
        let (_, fromTab) = Text.breakOn "<table:table table:name=\"Pending transaction addresses\"" content
            tab = fst (Text.breakOn "</table:table>" fromTab)
            roleCells = [role | role <- ["from", "to", "change"], Text.isInfixOf ("<text:p>" <> role <> "</text:p>") tab]
        (Text.count "<table:table-row>" tab, roleCells) `shouldBe` (4, ["from", "to", "change"])

    -- The coin of the payment, 3 confirmations deep, can be spent: 50000
    -- in all and spendable, nothing locked or waiting. The store counts
    -- 50000 confirmed and received at /0/0, in one coin and one
    -- transaction. The row ends with when the address was created, the
    -- time of the test, which is left out.
    it "gives the balance of each address, as numbers of satoshis, after the columns that say where it is" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        content <- contentOf file
        content
          `shouldSatisfy` Text.isInfixOf
            ( Text.concat
                [ "<table:table-row>",
                  -- Wallet, portfolio, network and type
                  "<table:table-cell office:value-type=\"string\"><text:p>wallet0</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>portfolio0</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>btc</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>bip44</text:p></table:table-cell>",
                  -- Address, path and chain
                  "<table:table-cell office:value-type=\"string\"><text:p>1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>/0/0</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>receiving</text:p></table:table-cell>",
                  -- No label
                  "<table:table-cell/>",
                  -- No tags
                  "<table:table-cell/>",
                  -- Total, spendable, locked by tx, locked by you, not spendable yet
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"50000\"><text:p>50000</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"50000\"><text:p>50000</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"0\"><text:p>0</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"0\"><text:p>0</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"0\"><text:p>0</text:p></table:table-cell>",
                  -- Confirmed, unconfirmed and received
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"50000\"><text:p>50000</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"0\"><text:p>0</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"50000\"><text:p>50000</text:p></table:table-cell>",
                  -- Coins and transactions
                  "<table:table-cell office:value-type=\"float\" office:value=\"1\"><text:p>1</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"float\" office:value=\"1\"><text:p>1</text:p></table:table-cell>"
                ]
            )

    -- The headings of a tab are the texts of its header row. No heading
    -- holds a comma, so those of a CSV file are apart by its commas.
    it "has in each tab the headings of the CSV file of its listing, in the same order, so that the rows of one fit the other" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        content <- contentOf file
        let headingsOfTab name =
              let (_, tab) = Text.breakOn ("<table:table table:name=\"" <> name <> "\"") content
                  header = fst (Text.breakOn "</table:table-header-rows>" tab)
               in [Text.takeWhile (/= '<') rest | rest <- drop 1 (Text.splitOn "<text:p>" header)]
            headingsOfCsv listing = do
              let csv = home </> (listing <> ".csv")
              _ <- answer home [listing, "--csv", csv] []
              Text.splitOn "," . Text.takeWhile (/= '\r') . Text.drop 1 . decodeUtf8 <$> BS.readFile csv
            tabsOfListings = [("Portfolios", "portfolios"), ("Addresses", "addresses"), ("Balances", "balances"), ("Coins", "coins"), ("Transactions", "txs"), ("Pending transactions", "pendingtxs")]
        inCsv <- mapM (headingsOfCsv . snd) tabsOfListings
        map (headingsOfTab . fst) tabsOfListings `shouldBe` inCsv
        map length inCsv `shouldSatisfy` all (> 4)

    -- The store saw the payment at 22:13:20 UTC, 23:13:20 in Central
    -- Europe in November, an hour ahead of UTC
    it "gives the time of a transaction in UTC, then on the clock of the computer, then how far that clock is from UTC" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "wallet.ods"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["exportsheet", file] []
        content <- contentOf file
        content
          `shouldSatisfy` Text.isInfixOf
            ( Text.concat
                [ "<table:table-cell table:style-name=\"time\" office:value-type=\"date\" office:date-value=\"2023-11-14T22:13:20\"><text:p>2023-11-14 22:13:20</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"time\" office:value-type=\"date\" office:date-value=\"2023-11-14T23:13:20\"><text:p>2023-11-14 23:13:20</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>UTC+01:00</text:p></table:table-cell>"
                ]
            )

    it "holds every portfolio, whichever is active" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        content <- contentOf file
        (Text.isInfixOf "<text:p>portfolio0</text:p>" content, Text.isInfixOf "<text:p>portfolio1</text:p>" content) `shouldBe` (True, True)

    -- pendingtxs lists one portfolio, and the spreadsheet every portfolio
    -- it holds: the payment of portfolio0 and that of portfolio1
    it "holds the pending transactions of every portfolio in one tab" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        twoPortfoliosPending home
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        content <- contentOf file
        let (_, pendingTab) = Text.breakOn "<table:table table:name=\"Pending transactions\"" content
        (Text.isInfixOf "<text:p>portfolio0</text:p>" pendingTab, Text.isInfixOf "<text:p>portfolio1</text:p>" pendingTab) `shouldBe` (True, True)

    it "is refused a file that exists, and the file is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "wallet.ods"
        writeFile file "kept\n"
        run <- hw home ["exportsheet", file] []
        run `shouldSatisfy` printed (alreadyExists file)
        readFile file `shouldReturn` "kept\n"

    it "is refused in a database without wallets, and writes no file" $
      withHome $ \home -> do
        let file = home </> "wallet.ods"
        run <- hw home ["exportsheet", file] []
        run `shouldSatisfy` printed "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"
        doesFileExist file `shouldReturn` False

    it "can be read and written by its owner alone" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        (.&. 0o777) . fileMode <$> getFileStatus file `shouldReturn` 0o600

    it "is answered in --json by the file written" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "wallet.ods"
        hwJson home ["exportsheet", file] [] `shouldReturn` ResponseFile file

{- The journal of exportjournal -}

-- | The journal exportjournal writes for the ledger command line tool
exportJournalSpec :: Spec
exportJournalSpec =
  describe "The journal of exportjournal" $ do
    let linesOf file = Text.lines . decodeUtf8 <$> BS.readFile file
        -- The lines of the postings: the indented lines that start with
        -- the name of an account. They name no hash and no time, which
        -- differ from one run of a test to the next.
        postingsOf file = filter (\line -> any (`Text.isPrefixOf` line) ["    Assets:", "    Income:", "    Expenses:"]) <$> linesOf file

    it "is written to the file exportjournal names, and hw says where" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "hw.ledger"
        run <- hw home ["exportjournal", file] []
        runOut run `shouldBe` "Written to " <> Text.pack file <> "\n"
        take 1 <$> linesOf file `shouldReturn` ["; The transactions of every portfolio of hw on the chain, as a journal for ledger."]

    -- The payment of 50000 is in block 800000, with a best block at
    -- 800002, so it is confirmed. The last posting is the balance.
    it "posts a payment received to the account of its portfolio, from Income:Unknown, then states the balance" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "hw.ledger"
        _ <- answer home ["exportjournal", file] []
        postingsOf file
          `shouldReturn` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                           "    Income:Unknown                -0.00050000 BTC",
                           "    Assets:hw:wallet0:portfolio0   0.00000000 BTC = 0.00050000 BTC"
                         ]

    -- The store saw the payment at 22:13:20 UTC on 14 Nov 2023, which is
    -- 00:13:20 on 15 Nov on a clock two hours ahead of UTC
    it "dates an entry on the clock of the computer, as cleared for a confirmed transaction, and gives its time in UTC in a note" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "hw.ledger"
        _ <- hwWithEnv [("TZ", "EET-2")] home ["exportjournal", file] []
        journal <- linesOf file
        ( [Text.take 14 line | line <- journal, Text.isSuffixOf "Payment received" line],
          [line | line <- journal, Text.isInfixOf "; time:" line]
          )
          `shouldBe` (["2023-11-15 * ("], ["    ; time: 2023-11-14 22:13:20 UTC"])

    -- portfolio0 pays 20000 of its coin of 50000 to the first receiving
    -- address of portfolio1, at 10 satoshis a byte for 226 bytes: 2260 of
    -- fee. 20000 + 2260 = 22260 leave portfolio0, which keeps 50000 -
    -- 22260 = 27740. The journal has the payment received first, then the
    -- payment between the portfolios, then the balances.
    it "writes a payment from a portfolio to another as one entry, which moves the money between their two accounts" $
      withChain paidChain $ \home _ -> do
        syncedTwoPortfolios home
        _ <- answer home ["send", fixedTo account1Addr "0.0002", "--fee-rate", "10"] seedOnce
        _ <- answer home ["sync"] []
        let file = home </> "hw.ledger"
        _ <- answer home ["exportjournal", file] []
        postingsOf file
          `shouldReturn` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                           "    Income:Unknown                -0.00050000 BTC",
                           "    Assets:hw:wallet0:portfolio1   0.00020000 BTC",
                           "    Expenses:Fees:Mining           0.00002260 BTC",
                           "    Assets:hw:wallet0:portfolio0  -0.00022260 BTC",
                           "    Assets:hw:wallet0:portfolio0   0.00000000 BTC = 0.00027740 BTC",
                           "    Assets:hw:wallet0:portfolio1   0.00000000 BTC = 0.00020000 BTC"
                         ]

    it "holds every portfolio, whichever is active" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        let file = home </> "hw.ledger"
        _ <- answer home ["exportjournal", file] []
        filter (Text.isPrefixOf "account Assets") <$> linesOf file
          `shouldReturn` ["account Assets:hw:wallet0:portfolio0", "account Assets:hw:wallet0:portfolio1"]

    it "names an entry after the label of its transaction" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["label", txidArg payment, "Salary of November"] []
        let file = home </> "hw.ledger"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["exportjournal", file] []
        journal <- linesOf file
        [Text.drop 24 line | line <- journal, Text.isPrefixOf "2023-11-14 * (" line] `shouldBe` ["Salary of November"]

    it "is refused a file that exists, and the file is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "hw.ledger"
        writeFile file "kept\n"
        run <- hw home ["exportjournal", file] []
        run `shouldSatisfy` printed (alreadyExists file)
        readFile file `shouldReturn` "kept\n"

    it "is refused in a database without wallets, and writes no file" $
      withHome $ \home -> do
        let file = home </> "hw.ledger"
        run <- hw home ["exportjournal", file] []
        run `shouldSatisfy` printed "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"
        doesFileExist file `shouldReturn` False

    it "can be read and written by its owner alone" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "hw.ledger"
        _ <- answer home ["exportjournal", file] []
        (.&. 0o777) . fileMode <$> getFileStatus file `shouldReturn` 0o600

    it "is answered in --json by the file written" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "hw.ledger"
        hwJson home ["exportjournal", file] [] `shouldReturn` ResponseFile file

{- The journal hw keeps -}

-- | The journal hw keeps for ledger next to the database of a home
keptJournalOf :: FilePath -> FilePath
keptJournalOf home = home </> ".hw" </> "portfolios.ledger"

-- | Turn on the journal hw keeps, in the configuration file of a home, as
-- a user who edits the file does
keepJournalIn :: FilePath -> IO ()
keepJournalIn home = setConfigKey home "journal" (Toml.Bool True)

-- | The journal hw keeps next to the database when the key journal of
-- the configuration is on, and writes again after each command that
-- changes it
keptJournalSpec :: Spec
keptJournalSpec =
  describe "The journal hw keeps next to the database" $ do
    let linesOf file = Text.lines . decodeUtf8 <$> BS.readFile file
        -- The lines of the postings: the indented lines that start with
        -- the name of an account
        postingsOf file = filter (\line -> any (`Text.isPrefixOf` line) ["    Assets:", "    Income:", "    Expenses:"]) <$> linesOf file
        -- The first line of each entry of a transaction, without its
        -- date, its state and its code, which take 24 characters, as in
        -- "2023-11-14 * (a16ab47f) "
        namesOf file = do
          journal <- linesOf file
          return [Text.drop 24 line | line <- journal, Text.isPrefixOf "20" line, not (Text.isSuffixOf "Balances of hw" line)]

    it "is not written unless the configuration asks for it" $
      withChain paidChain $ \home _ -> do
        synced home
        doesFileExist (keptJournalOf home) `shouldReturn` False

    it "is turned on by setdefault --journal, which writes the key and says where the journal is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["setdefault", "--journal"] []
        runOut run
          `shouldBe` Text.unlines
            [ "journal: true",
              "Written to " <> Text.pack (configOf home),
              "hw now keeps a journal for ledger at " <> Text.pack (keptJournalOf home)
            ]
        configKeyOf home "journal" `shouldReturn` Just (Toml.Bool True)

    -- The payment of 50000 was synced before the journal was turned on
    it "is written at once by setdefault --journal, with the transactions the wallet has" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["setdefault", "--journal"] []
        postingsOf (keptJournalOf home)
          `shouldReturn` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                           "    Income:Unknown                -0.00050000 BTC",
                           "    Assets:hw:wallet0:portfolio0   0.00000000 BTC = 0.00050000 BTC"
                         ]

    it "holds what exportjournal writes, byte for byte" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["setdefault", "--journal"] []
        let file = home </> "hw.ledger"
        _ <- answer home ["exportjournal", file] []
        keptBytes <- BS.readFile (keptJournalOf home)
        exported <- BS.readFile file
        keptBytes `shouldBe` exported

    it "is answered in --json by where the journal is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        hwJson home ["setdefault", "--journal"] []
          `shouldReturn` ResponseDefaults [("journal", "true")] (configOf home) (Just (JournalSwitch True (keptJournalOf home)))

    -- The journal is on before the wallet syncs the payment of 50000
    it "is written after a sync, with the transactions the sync found" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        postingsOf (keptJournalOf home)
          `shouldReturn` [ "    Assets:hw:wallet0:portfolio0   0.00050000 BTC",
                           "    Income:Unknown                -0.00050000 BTC",
                           "    Assets:hw:wallet0:portfolio0   0.00000000 BTC = 0.00050000 BTC"
                         ]

    it "is written again after label, with the new label of a transaction as the name of its entry" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        _ <- answer home ["label", txidArg payment, "Salary of November"] []
        namesOf (keptJournalOf home) `shouldReturn` ["Salary of November"]

    -- The payment was received at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,
    -- and has no label of its own
    it "is written again after label, with the new label of an address as the name of the payment received there" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        _ <- answer home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Invoice 42"] []
        namesOf (keptJournalOf home) `shouldReturn` ["Invoice 42"]

    it "is written again after tag, with the new tag of a transaction on its entry" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        filter (Text.isInfixOf ":salary:") <$> linesOf (keptJournalOf home) `shouldReturn` ["    ; :salary:"]

    -- The wallet pays 20000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, and
    -- the sync finds the payment in the mempool. The address then
    -- becomes a contact.
    it "is written again after newcontact, with the label of the contact as the name of the payment to it" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        _ <- answer home ["send", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] seedOnce
        _ <- answer home ["sync"] []
        _ <- answer home ["newcontact", textOf (otherAddr 0), "Landlord"] []
        namesOf (keptJournalOf home) `shouldReturn` ["Payment received", "Landlord"]

    -- The payment the wallet sent is in preparation until a sync finds
    -- it on the chain: the journal still holds the payment received alone
    it "does not hold a payment the wallet sent before the sync that finds it on the chain" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        _ <- answer home ["send", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] seedOnce
        namesOf (keptJournalOf home) `shouldReturn` ["Payment received"]

    -- The wallet has a coin of 50000 on bch and nothing on btc
    it "loses the entries of a network once setdefault hides that network" $
      withChain (confirmedChain cashCoinTx) $ \home _ -> do
        setShownNetworks home ["btc", "bch"]
        keepJournalIn home
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        earlier <- namesOf (keptJournalOf home)
        _ <- answer home ["setdefault", "--hide-network", "bch"] []
        later <- namesOf (keptJournalOf home)
        (earlier, later) `shouldBe` (["Payment received"], [])

    -- The second sync finds nothing new. A file written again would be a
    -- new file, with another number in the file system.
    it "is left as it is by a command that did not change it" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        earlier <- getFileStatus (keptJournalOf home)
        _ <- answer home ["sync"] []
        later <- getFileStatus (keptJournalOf home)
        (fileID later, modificationTimeHiRes later) `shouldBe` (fileID earlier, modificationTimeHiRes earlier)

    -- txs changes nothing, so it does not bring back a journal that was
    -- deleted by hand
    it "is not written by a command that changes nothing of the wallet" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        removeFile (keptJournalOf home)
        _ <- answer home ["txs"] []
        doesFileExist (keptJournalOf home) `shouldReturn` False

    it "holds its first comment alone in a database without a wallet, so that a journal that includes it still reads" $
      withHome $ \home -> do
        _ <- answer home ["setdefault", "--journal"] []
        linesOf (keptJournalOf home)
          `shouldReturn` [ "; The transactions of every portfolio of hw on the chain, as a journal for ledger.",
                           "; hw writes this file whole each time. Do not edit it: your changes would be lost."
                         ]

    it "can be read and written by its owner alone" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["setdefault", "--journal"] []
        (.&. 0o777) . fileMode <$> getFileStatus (keptJournalOf home) `shouldReturn` 0o600

    it "is removed by setdefault --no-journal, which says so" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        run <- hw home ["setdefault", "--no-journal"] []
        runOut run
          `shouldBe` Text.unlines
            [ "journal: false",
              "Written to " <> Text.pack (configOf home),
              "hw no longer keeps a journal, and removes " <> Text.pack (keptJournalOf home)
            ]
        doesFileExist (keptJournalOf home) `shouldReturn` False

    it "is no longer written once setdefault --no-journal turned it off" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        synced home
        _ <- answer home ["setdefault", "--no-journal"] []
        _ <- answer home ["label", txidArg payment, "Salary of November"] []
        doesFileExist (keptJournalOf home) `shouldReturn` False

    -- A directory stands where the journal goes, so the file cannot take
    -- its place. The sync itself worked.
    it "leaves a command that worked as it is when the journal cannot be written, and says on stderr that the journal is behind" $
      withChain paidChain $ \home _ -> do
        keepJournalIn home
        newTestPortfolio home
        _ <- answer home ["receive"] []
        createDirectory (keptJournalOf home)
        run <- hw home ["sync"] []
        (runExit run, Text.isPrefixOf ("The journal " <> Text.pack (keptJournalOf home) <> " was not brought up to date. ") (runErr run))
          `shouldBe` (ExitSuccess, True)
        runErr run `shouldSatisfy` Text.isSuffixOf ". The command itself is done. To write the journal now, run: hw setdefault --journal\n"

    it "is shown by showdefault --journal, as on or off" $
      withHome $ \home -> do
        newTestPortfolio home
        earlier <- hwJson home ["showdefault", "--journal"] []
        _ <- answer home ["setdefault", "--journal"] []
        later <- hwJson home ["showdefault", "--journal"] []
        (earlier, later)
          `shouldBe` ( ResponseShownDefaults [("journal", "false")] (configOf home) ActiveNotAsked,
                       ResponseShownDefaults [("journal", "true")] (configOf home) ActiveNotAsked
                     )

{- The balance statement of report -}

-- | The balance statement report writes as a PDF file
reportSpec :: Spec
reportSpec =
  describe "The balance statement of report" $ do
    let textsOf file = pdfTexts <$> BS.readFile file
        -- The first row of the summary, that of Bitcoin: the four texts
        -- after the titles of the columns, of which Balance is the last
        bitcoinRow file = take 4 . drop 1 . dropWhile (/= "BALANCE") <$> textsOf file

    it "is written to the file report names, as a PDF file, and hw says where" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        run <- hw home ["report", file, "--balances"] []
        runOut run `shouldBe` "Written to " <> Text.pack file <> "\n"
        BS.take 8 <$> BS.readFile file `shouldReturn` "%PDF-1.4"

    -- The payment of 50000 is in block 800000, with a best block at
    -- 800002, so it is confirmed: 0.00050000 btc at one address
    it "sums up what the wallet holds of Bitcoin: how many addresses hold coins, and the balance" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- answer home ["report", file, "--balances"] []
        bitcoinRow file `shouldReturn` ["Bitcoin", "BTC", "1", "0.00050000"]

    it "gives the address that holds the coins, with its path and its balance" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- answer home ["report", file, "--balances"] []
        texts <- textsOf file
        texts `shouldSatisfy` isInfixOf ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "bip44/0/0", "0.00050000"]

    -- The store dates the payment at 22:13:20 UTC on 14 Nov 2023, which
    -- is 23:13:20 on the same day in Central Europe
    it "counts a payment of the day --date names, and is dated that day" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["report", file, "--balances", "--date", "2023-11-14"] []
        row <- bitcoinRow file
        texts <- textsOf file
        (row, "As of 14 November 2023" `elem` texts) `shouldBe` (["Bitcoin", "BTC", "1", "0.00050000"], True)

    it "leaves out a payment that came after the day --date names" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["report", file, "--balances", "--date", "2023-11-13"] []
        bitcoinRow file `shouldReturn` ["Bitcoin", "BTC", "0", "0.00000000"]

    -- 22:13:20 UTC on the 14th is 00:13:20 on the 15th on a clock two
    -- hours ahead of UTC
    it "reads the day of a payment on the clock of the computer" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- hwWithEnv [("TZ", "EET-2")] home ["report", file, "--balances", "--date", "2023-11-14"] []
        bitcoinRow file `shouldReturn` ["Bitcoin", "BTC", "0", "0.00000000"]

    it "leaves out an unconfirmed payment, and says in a note what it would add" $
      withChain (Chain 800002 [(payment, Nothing)]) $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- answer home ["report", file, "--balances"] []
        row <- bitcoinRow file
        texts <- textsOf file
        (row, "On Bitcoin, 1 unconfirmed transaction was left out, which would change the balance by +0.00050000 BTC." `Text.isInfixOf` Text.unwords texts)
          `shouldBe` (["Bitcoin", "BTC", "0", "0.00000000"], True)

    -- 50000 at portfolio0 and 30000 at portfolio1 are 80000 in all
    it "holds every portfolio, whichever is active, and their total" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        let file = home </> "balances.pdf"
        _ <- answer home ["report", file, "--balances"] []
        texts <- textsOf file
        texts `shouldSatisfy` isInfixOf ["portfolio0", "1", "0.00050000", "portfolio1", "1", "0.00030000", "Total Bitcoin", "2", "0.00080000"]

    it "names the holder --holder gives" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        _ <- answer home ["report", file, "--balances", "--holder", "Jane Doe"] []
        texts <- textsOf file
        texts `shouldSatisfy` isInfixOf ["Holder", "Jane Doe"]

    it "says that the holder prepared it, and that the wallets are in self-custody" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        _ <- answer home ["report", file, "--balances"] []
        texts <- textsOf file
        texts `shouldSatisfy` isInfixOf ["Prepared by", "The holder, from public blockchain data. Not audited."]
        texts `shouldSatisfy` isInfixOf ["Custody", "Self-custody: the holder keeps the keys. No exchange or online wallet."]

    -- Central Europe is an hour ahead of UTC in November and two hours
    -- ahead in June
    it "states the midnight that ends its day with the zone of the clock that night" $
      withChain paidChain $ \home _ -> do
        synced home
        let winter = home </> "winter.pdf"
            summer = home </> "summer.pdf"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["report", winter, "--balances", "--date", "2023-11-14"] []
        _ <- hwWithEnv [("TZ", centralEurope)] home ["report", summer, "--balances", "--date", "2023-06-30"] []
        winterTexts <- textsOf winter
        summerTexts <- textsOf summer
        winterTexts `shouldSatisfy` isInfixOf ["Statement date", "14 November 2023, 24:00 (UTC+01:00)"]
        summerTexts `shouldSatisfy` isInfixOf ["Statement date", "30 June 2023, 24:00 (UTC+02:00)"]

    -- The reference is the first eight digits of the SHA-256 hash of the
    -- lines "end of 2023-11-14", "holder " and "btc 0 0
    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj 50000", worked out with
    -- sha256sum
    it "has a reference that follows from its date and its balances, at the foot of each page" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["report", file, "--balances", "--date", "2023-11-14"] []
        texts <- textsOf file
        texts `shouldSatisfy` isInfixOf ["Reference", "99EF-0FD3"]
        texts `shouldSatisfy` elem "Balance Statement \183 As of 14 November 2023 \183 Ref. 99EF-0FD3"

    -- 50000 satoshis are 0.0005 bitcoins: 0.0005 x 87500 = 43.75
    it "states the value of what the wallet holds at the price --price gives, and the total" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- answer home ["report", file, "--balances", "--price", "btc=87500", "--currency", "CHF", "--price-source", "Rate list of the tax office"] []
        texts <- textsOf file
        texts `shouldSatisfy` isInfixOf ["Bitcoin", "BTC", "1", "0.00050000", "87,500.00", "43.75"]
        texts `shouldSatisfy` isInfixOf ["Total value", "43.75"]
        texts `shouldSatisfy` elem "Source of the prices: Rate list of the tax office"

    it "is refused a price without a currency, and writes no file" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        run <- hw home ["report", file, "--balances", "--price", "btc=87500"] []
        run `shouldSatisfy` printedSentence "Give the currency of the prices with --currency, such as --currency CHF"
        doesFileExist file `shouldReturn` False

    it "is refused a currency without a price, and writes no file" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        run <- hw home ["report", file, "--balances", "--currency", "CHF"] []
        run `shouldSatisfy` printedSentence "--currency goes with --price: give the price of a coin of each network, such as --price btc=87500"
        doesFileExist file `shouldReturn` False

    it "is refused when the wallet holds coins on a network that has no price, and writes no file" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        run <- hw home ["report", file, "--balances", "--price", "bch=400", "--currency", "CHF"] []
        run `shouldSatisfy` printedSentence "The wallets hold coins on btc, which has no price: give one with --price btc=PRICE, or the total value would leave them out"
        doesFileExist file `shouldReturn` False

    -- The payment is in the block 800000, and the sync reached 800002:
    -- hw asks the server for the hash of the block 800000. A hash is 64
    -- hexadecimal digits, after the word Hash and a space: 69 characters.
    it "names the last block it counts by the hash the server gives" $
      withChain paidChain $ \home _ -> do
        synced home
        let file = home </> "balances.pdf"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["report", file, "--balances"] []
        texts <- textsOf file
        [(Text.take 5 after, Text.length after, Text.all isHexDigit (Text.drop 5 after)) | (block, after) <- zip texts (drop 1 texts), block == "Block 800000, dated 14 November 2023, 23:13"]
          `shouldBe` [("Hash ", 69, True)]

    -- Nothing listens on the port 1
    it "is written without that hash when the server does not answer, and says so" $
      withChain paidChain $ \home _ -> do
        synced home
        setConfigKey home "host" (Toml.Text "http://127.0.0.1:1")
        let file = home </> "balances.pdf"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["report", file, "--balances"] []
        texts <- textsOf file
        texts `shouldSatisfy` isInfixOf ["Block 800000, dated 14 November 2023, 23:13", "Hash not available: the server did not give it"]

    it "is refused for a day that is not over, and writes no file" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        run <- hw home ["report", file, "--balances", "--date", "2999-12-31"] []
        run `shouldSatisfy` printedSentence "The day of --date must be over, and today is"
        doesFileExist file `shouldReturn` False

    -- The best block of the chain was mined forty days ago, and the day
    -- asked for ended ten days ago: the wallet knows nothing of the
    -- thirty days between
    it "is refused for a day the last sync does not reach, and writes no file" $
      withChainMinedAgo [] (40 * 86400) paidChain $ \home _ -> do
        synced home
        tenDaysAgo <- showGregorian . addDays (-10) . utctDay <$> getCurrentTime
        let file = home </> "balances.pdf"
        run <- hwWithEnv [("TZ", "UTC")] home ["report", file, "--balances", "--date", tenDaysAgo] []
        run `shouldSatisfy` printedSentence "it could miss transactions. Run hw sync first"
        doesFileExist file `shouldReturn` False

    it "is refused a file that exists, and the file is kept" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        writeFile file "kept\n"
        run <- hw home ["report", file, "--balances"] []
        run `shouldSatisfy` printed (alreadyExists file)
        readFile file `shouldReturn` "kept\n"

    it "is refused in a database without wallets, and writes no file" $
      withHome $ \home -> do
        let file = home </> "balances.pdf"
        run <- hw home ["report", file, "--balances"] []
        run `shouldSatisfy` printed "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"
        doesFileExist file `shouldReturn` False

    it "can be read and written by its owner alone" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        _ <- answer home ["report", file, "--balances"] []
        (.&. 0o777) . fileMode <$> getFileStatus file `shouldReturn` 0o600

    it "is answered in --json by the file written" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "balances.pdf"
        hwJson home ["report", file, "--balances"] [] `shouldReturn` ResponseFile file
