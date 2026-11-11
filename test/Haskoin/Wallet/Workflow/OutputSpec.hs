{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What hw shows: the listings, their paging and the view the
-- configuration file names for them, a transaction as hw
-- lays it out, the pending transactions, the balances of the addresses,
-- the coins, the next step shown after a payment or a sweep, the pager,
-- the CSV files of --csv, and the spreadsheet of exportsheet.
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
import Data.IORef (modifyIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..), coinBalanceUnspendable)
import Haskoin.Wallet.Database.Coin (JsonCoin (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), Place (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  describe "The status line" $ do
    let shown text run = text `Text.isInfixOf` runErr run

    it "tells on a terminal what sync is doing" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hwOnTerminal home ["sync"] []
        run `shouldSatisfy` shown "\rportfolio0 btc: checking the store\ESC[0K"

    it "shows on a sync with nothing new as well" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hwOnTerminal home ["sync"] []
        run `shouldSatisfy` shown "\rportfolio0 btc: reading balances  [--------------------]  0/1 addresses\ESC[0K"

    it "is gone from the terminal when sync ends" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hwOnTerminal home ["sync"] []
        runErr run `shouldSatisfy` Text.isSuffixOf "\r\ESC[0K"

    it "leaves the answer of sync as it is off a terminal" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        plain <- hw home ["sync"] []
        run <- hwOnTerminal home ["sync"] []
        runOut run `shouldBe` runOut plain

    it "writes nothing off a terminal" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hw home ["sync"] []
        runErr run `shouldBe` ""

    -- The question comes right after the code that clears the line, so
    -- it starts at the left edge and nothing is left behind it
    it "makes room for the question of a scan" $
      withLyingChain ctx (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hwOnTerminal home ["sync"] ["n"]
        run `shouldSatisfy` shown "\r\ESC[0KThe store reports used addresses as far as 1000"

    -- The chain pays portfolio0, so the search has found one portfolio
    -- paid when it looks at portfolio1
    it "tells on a terminal which portfolio recover looks at, after the mnemonic is typed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- hwOnTerminal home ["recover"] seedOnce
        run `shouldSatisfy` shown "\rrecover: looking at portfolio0, 0 portfolios paid so far\ESC[0K"
        run `shouldSatisfy` shown "\rrecover: looking at portfolio1, 1 portfolio paid so far\ESC[0K"

    -- The funds at paths hw cannot spend from: bip49 at m/49' on Bitcoin,
    -- and the accounts of Bitcoin, at m/44'/0', on Bitcoin Cash. A path
    -- alone would not tell the two networks apart.
    it "tells on a terminal which network and account recover looks at for the funds hw cannot spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- hwOnTerminal home ["recover"] seedOnce
        run `shouldSatisfy` shown "\rrecover on btc: looking at /49'/0'/0', 0 accounts paid so far\ESC[0K"
        run `shouldSatisfy` shown "\rrecover on bch: looking at /44'/0'/0', 0 accounts paid so far\ESC[0K"

  listingSpec ctx
  listingViewSpec ctx
  layoutSpec ctx
  pendingListSpec ctx
  csvSpec ctx
  exportSheetSpec ctx
  balancesListSpec ctx
  coinsListSpec ctx
  sectionsSpec ctx
  syncAnswerSpec ctx
  pagerSpec ctx
  addressCodeSpec ctx
  nextStepSpec ctx

{- Listings -}

-- | How many rows a listing shows, over all its sections
rowsOf :: Response -> Either String Int
rowsOf (ResponseAddresses groups) = Right (sum (length . groupRows <$> groups))
rowsOf (ResponseBalances groups) = Right (sum (length . groupRows . fst <$> groups))
rowsOf (ResponseTxs groups) = Right (sum (length . groupRows <$> groups))
rowsOf (ResponsePendingTxs held) = Right (length held)
rowsOf (ResponseCoins groups) = Right (sum (length . groupRows <$> groups))
rowsOf other = Left (show other)

-- | What names each row of a listing, as --after takes it: the address,
-- the coin, the txid, or the hash of a pending transaction as shown. The
-- sections of a listing come one after the other.
keysOf :: Response -> Either String [String]
keysOf (ResponseAddresses groups) = Right [Text.unpack (dBAddressAddress addr) | g <- groups, (addr, _) <- groupRows g]
keysOf (ResponseBalances groups) = Right [Text.unpack (dBAddressAddress addr) | (g, _) <- groups, (addr, _) <- groupRows g]
keysOf (ResponseTxs groups) = Right [Text.unpack (txHashToHex (chainTxid status)) | g <- groups, ListedTx info _ <- groupRows g, OnChain status <- [txInfoStatus info]]
keysOf (ResponsePendingTxs held) = Right [Text.unpack (txHashToHex (pendingShownHash status)) | (_, info) <- held, Pending status <- [txInfoStatus info]]
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

listingSpec :: Ctx -> Spec
listingSpec ctx = do
  -- The fake store gives every transaction the time 1700000000
  describe "The history txs lists" $
    it "gives each transaction the time the store gives it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
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
      withChain ctx (Chain 800002 [(payment ctx, Nothing)]) $ \home _ -> do
        synced home
        run <- hwOnScreen [] home ["portfolios", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "unconfirmed \ESC[0;33m0.00050000\ESC[0m btc"

    -- The header takes two lines, and the coin of the payment one row.
    -- --network narrows the listing to one network, which needs no
    -- title.
    it "shows each coin on one row under a header with coins --tabular, on one network" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["coins", "--network", "btc", "--tabular"] []
        length (Text.lines listed) `shouldBe` 3

    -- The title of the network with its total, the two lines of the
    -- header, and the address paid on one row
    it "shows each address on one row under the title of its network and a header with balances --tabular" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["balances", "--tabular"] []
        length (Text.lines listed) `shouldBe` 4

    -- The header takes two lines, and the payment prepared one row
    it "shows each pending transaction on one row under a header with pendingtxs --tabular" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        listed <- runOut <$> hw home ["pendingtxs", "--tabular"] []
        length (Text.lines listed) `shouldBe` 3

    it "shows the row of each listing" $
      withChain ctx (paidChain ctx) $ \home _ -> do
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

    -- portfolio1 is a portfolio of its own, without any row
    it "shows the rows of the active portfolio only" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        newTestPortfolio home
        forM_ listings $ \listing ->
          (rowsOf <$> hwJson home [listing] []) `shouldReturn` Right 1
        switchTo home 0 1
        forM_ listings $ \listing ->
          (rowsOf <$> hwJson home [listing] []) `shouldReturn` Right 0

    -- Two of each: the addresses /0/0 and /0/1, the two payments to them,
    -- their coins, and a payment prepared from each coin
    it "shows --max rows of each listing, then the rows after the one --after names" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--fee-rate", "10"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg (newerPayment ctx) 0, "--fee-rate", "10"] []
        forM_ listings $ \listing -> do
          keys <- either fail return . keysOf =<< hwJson home [listing] []
          length keys `shouldBe` 2
          (keysOf <$> hwJson home [listing, "--max", "1"] []) `shouldReturn` Right [keys !! 0]
          (keysOf <$> hwJson home [listing, "--after", keys !! 0] []) `shouldReturn` Right [keys !! 1]
          (rowsOf <$> hwJson home [listing, "--after", keys !! 1] []) `shouldReturn` Right 0

    -- The listings of the test before that name a row by a hash. A coin is
    -- the short id of its txid, then its index.
    it "shows the rows after the one --after names by its short id" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--fee-rate", "10"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg (newerPayment ctx) 0, "--fee-rate", "10"] []
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
      withChainServing ctx [btcTest, btcTest4] [] (confirmedChain (testnetCoinTx ctx)) $ \home _ -> do
        testnetCoinSynced home
        (errorOf <$> hwJson home ["balances", "--after", "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"] [])
          `shouldReturn` Right "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4: give the --network"

    -- The two payments of the test before, signed. The newer is listed
    -- first.
    it "shows the pending transactions after the signed one --after names by its txid" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        older <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--fee-rate", "10"] []
        newer <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg (newerPayment ctx) 0, "--fee-rate", "10"] []
        olderId <- txidOf =<< txOf =<< answer home ["signtx", older] seedOnce
        newerId <- txidOf =<< txOf =<< answer home ["signtx", newer] seedOnce
        (keysOf <$> hwJson home ["pendingtxs", "--after", newerId] []) `shouldReturn` Right [olderId]

-- | The key listing-view of the configuration file, for a user who always
-- wants the same view, and --compact, which brings back the compact view
listingViewSpec :: Ctx -> Spec
listingViewSpec ctx =
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
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setListingView home "verbose"
        run <- hw home ["coins"] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

    -- A table has a line of dashes under its headings, which no other
    -- view has: the second line, or the third under the title of a
    -- section. The payment prepared gives each listing a row.
    it "is the view of each of the six listings" $
      withChain ctx (paidChain ctx) $ \home _ -> do
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
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setListingView home "tabular"
        run <- hw home ["lockcoin", coinArg (payment ctx) 0] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

{- Transactions as hw shows them -}

-- | A payment of 50000 to the first receiving address of the test wallet,
-- from another wallet that keeps 70000 of change at an address of its own.
-- The store gives its input 121000, so that its fee is 1000. The payer
-- spends from otherAddr 0.
paymentWithPayerChange :: Ctx -> Tx
paymentWithPayerChange ctx = fundingTx ctx 9 [(externalAddr 0, 50000), (otherAddr 1, 70000)]

-- | What hw shows of the payment of 20000 prepared in a synced home
shownPayment :: FilePath -> [String] -> IO Text
shownPayment home options = runOut <$> hw home (["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] <> options) []

-- The payment of 20000 spends the coin of 50000 at /0/0, and leaves 27740
-- of change
layoutSpec :: Ctx -> Spec
layoutSpec ctx =
  describe "A transaction as hw shows it" $ do
    it "shows the addresses of the wallet a payment spends from, with their paths" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0\n"

    it "shows the labels of the addresses of the wallet" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "salary"] []
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0\n             \"salary\"\n"

    it "shows the recipients of a payment" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc\n"

    it "shows the change of a payment, at its change address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0\n"

    it "shows the change a payment sends back to the address it spends from" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        shown <- shownPayment home ["--spend-from", textOf (externalAddr 0), "--change-back", textOf (externalAddr 0)]
        shown `shouldSatisfy` Text.isInfixOf "  Change   : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00027740 btc  bip44/0/0\n"

    -- The payer's coins are worth 121000, nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts, in txs --verbose" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--verbose"] []
        listed `shouldSatisfy` Text.isInfixOf "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\n"

    it "shows the address of the wallet a payment received is sent to, with its path, in txs --verbose" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--verbose"] []
        listed `shouldSatisfy` Text.isInfixOf "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0\n"

    -- A portfolio has an address at /0/0 in each of its accounts on btc,
    -- so the path names the type of the account
    it "shows the type and the path of the address of the portfolio a payment received is paid at, in txs" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "\n  balance 0.00050000 btc, at bip44/0/0\n"

    -- Blue is colour 34 of the terminal, and 3 sets italics, as labels
    -- have them. --no-pager prints the listing itself, which a pager would
    -- read.
    it "shows the path of an address of the wallet in blue on a terminal" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hwOnScreen [] home ["txs", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "at \ESC[0;34mbip44/0/0\ESC[0m"

    it "shows the label of an address of the wallet in its blue on a terminal, in italics" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "rent"] []
        run <- hwOnScreen [] home ["txs", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "\ESC[0;34;3m\"rent\""

    it "shows an address of the wallet in full in blue on a terminal, in txs --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hwOnScreen [] home ["txs", "--verbose", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "\ESC[0;34m1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\ESC[0m"

    it "does not show the change the payer of a payment received keeps, in either view" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        compact <- runOut <$> hw home ["txs"] []
        verbose <- runOut <$> hw home ["txs", "--verbose"] []
        compact `shouldNotSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 1)))
        verbose `shouldNotSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 1)))

    -- --network narrows the listing to one network, which needs no title
    it "lists each transaction in two lines without --verbose" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--network", "btc"] []
        length (Text.lines listed) `shouldBe` 2

    -- The two payments, two lines each, and a blank line between them
    it "sets the transactions apart with a blank line without --verbose" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--network", "btc"] []
        map Text.null (Text.lines listed) `shouldBe` [False, False, True, False, False]

    -- The header takes two lines: the headings, and the line under them.
    -- Each of the two payments takes one row.
    it "lists each transaction on one row under a header with --tabular" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--network", "btc", "--tabular"] []
        length (Text.lines listed) `shouldBe` 4

    -- The newest payment brought 30000 to the 50000: 50000 + 30000 = 80000
    it "puts the balance after each transaction next to its amount with --tabular" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--network", "btc", "--tabular"] []
        map (Text.isInfixOf "│    +0.00030000 │     0.00080000 │") (Text.lines listed) `shouldBe` [False, False, True, False]
        map (Text.isInfixOf "│    +0.00050000 │     0.00050000 │") (Text.lines listed) `shouldBe` [False, False, False, True]

    -- The newest payment brought 30000 to the 50000 at /0/1: 50000 + 30000
    -- = 80000
    it "shows the balance after each transaction, the newest with what the portfolio holds on the network" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        filter ("balance" `Text.isInfixOf`) (Text.lines listed)
          `shouldBe` [ "  balance 0.00080000 btc, at bip44/0/1",
                       "  balance 0.00050000 btc, at bip44/0/0"
                     ]

    it "shows the balance after each transaction as a field in txs --verbose" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs", "--verbose"] []
        filter ("Balance" `Text.isInfixOf`) (Text.lines listed)
          `shouldBe` ["  Balance  : 0.00080000 btc", "  Balance  : 0.00050000 btc"]

    it "writes the balance after each transaction in satoshis with --json" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["txs", "--json"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\"balance\": 80000"
        runOut run `shouldSatisfy` Text.isInfixOf "\"balance\": 50000"

    -- The transaction after the newest one keeps the balance it has in the
    -- whole listing: 80000 - 30000 = 50000
    it "gives the transaction after the one --after names the balance it has in the whole listing" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        Right (newest : _) <- keysOf <$> hwJson home ["txs"] []
        (txBalancesOf <$> hwJson home ["txs", "--after", newest] []) `shouldReturn` Right [Just 50000]

    -- The payment of 20000 with its fee of 2260: 50000 - 22260 = 27740
    it "counts an unconfirmed payment in the balance of the newest transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- sentPayment home
        _ <- answer home ["sync"] []
        (txBalancesOf <$> hwJson home ["txs"] []) `shouldReturn` Right [Just 27740, Just 50000]

    it "shows no balance in a transaction a command answers with" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0001"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "Balance"

    -- The store saw the payment at 22:13:20 UTC, 23:13:20 in Central
    -- Europe in November
    it "shows the time of a transaction in the time zone of the computer, to the minute" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hwWithEnv [("TZ", centralEurope)] home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "  14 Nov 2023 23:13     +0.00050000 btc  "

    it "shows the time of a transaction to the second, with its time zone, in txs --verbose" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hwWithEnv [("TZ", centralEurope)] home ["txs", "--verbose"] []
        listed `shouldSatisfy` Text.isInfixOf "  Time     : 14 Nov 2023 23:13:20 CET\n"

    -- A zone an hour ahead of UTC, and two hours ahead in its summer time,
    -- from 10 to 20 November. 22:13:20 UTC on 14 November is then 00:13:20
    -- the next day. Today's offset would give 23:13, unless the tests run
    -- in that summer time.
    it "shows the time of a transaction with the offset of its day, summer time or not" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hwWithEnv [("TZ", "XST-1XDT,J314/0,J324/0")] home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "  15 Nov 2023 00:13     +0.00050000 btc  "

    -- Paying the second receiving address from the first leaves 27740 of
    -- change at /1/0: both outputs are the wallet's, and neither can be told
    -- to be the change of the payment. The one at a change address says so.
    it "shows every output of a transaction between addresses of the wallet as sent to, none as change" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
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
twoNetworksChain :: Ctx -> Chain
twoNetworksChain ctx = Chain 800002 [(payment ctx, Just 800000), (newerPayment ctx, Just 800000), (cashCoinTx ctx, Just 800000)]

-- | A home with portfolio0 of the test wallet, synced with
-- 'twoNetworksChain', and three payments prepared in this order: one
-- from the coin of 50000 on btc, one on bch, and one from the coin of
-- 30000 on btc. The nosigHash of each, in that order.
threePendingOnTwoNetworks :: Ctx -> FilePath -> IO (String, String, String)
threePendingOnTwoNetworks ctx home = do
  newTestPortfolio home
  _ <- answer home ["receive"] []
  _ <- answer home ["receive", "--network", "bch"] []
  _ <- answer home ["sync"] []
  olderOnBtc <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--fee-rate", "10"] []
  onBch <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--network", "bch", "--fee-rate", "10"] []
  newerOnBtc <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg (newerPayment ctx) 0, "--fee-rate", "10"] []
  return (olderOnBtc, onBch, newerOnBtc)

-- | Where each pending transaction pendingtxs lists is
pendingPlacesOf :: Response -> Either String [Place]
pendingPlacesOf (ResponsePendingTxs held) = Right (fst <$> held)
pendingPlacesOf other = Left (show other)

-- | The network of each pending transaction pendingtxs lists, with its
-- nosigHash
pendingNetworksOf :: Response -> Either String [(String, String)]
pendingNetworksOf (ResponsePendingTxs held) =
  Right [((placeNetwork place).name, Text.unpack (txHashToHex (pendingNosigHash status))) | (place, info) <- held, Pending status <- [txInfoStatus info]]
pendingNetworksOf other = Left (show other)

-- | When each pending transaction pendingtxs lists was created
createdTimesOf :: Response -> Either String [Maybe UTCTime]
createdTimesOf (ResponsePendingTxs held) = Right (created . snd <$> held)
  where
    created info = case txInfoStatus info of
      Pending status -> pendingCreated status
      OnChain _ -> Nothing
createdTimesOf other = Left (show other)

pendingListSpec :: Ctx -> Spec
pendingListSpec ctx =
  describe "The pending transactions pendingtxs lists" $ do
    it "are those of the active portfolio, each with its place" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        twoPortfoliosPending home
        (pendingPlacesOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [mainPlace]

    it "are those of the portfolio switch made active" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        twoPortfoliosPending home
        switchTo home 0 1
        (pendingPlacesOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [portfolio1Place]

    -- The payment on bch was prepared between the two on btc. btc comes
    -- before bch among the networks of a portfolio.
    it "are sorted by network, in the order of the networks of a portfolio, and the newest first on each" $
      withChain ctx (twoNetworksChain ctx) $ \home _ -> do
        (olderOnBtc, onBch, newerOnBtc) <- threePendingOnTwoNetworks ctx home
        (pendingNetworksOf <$> hwJson home ["pendingtxs"] [])
          `shouldReturn` Right [("btc", newerOnBtc), ("btc", olderOnBtc), ("bch", onBch)]

    it "are those of the --network named alone" $
      withChain ctx (twoNetworksChain ctx) $ \home _ -> do
        (_, onBch, _) <- threePendingOnTwoNetworks ctx home
        (pendingNetworksOf <$> hwJson home ["pendingtxs", "--network", "bch"] []) `shouldReturn` Right [("bch", onBch)]

    it "name the portfolio and the network of each on its second line" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        twoPortfoliosPending home
        switchTo home 0 1
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  portfolio1 btc, "

    -- The second test mnemonic is wallet1, whose first receiving address
    -- the chain pays
    it "name the wallet of a portfolio that is not of wallet0" $
      withChain ctx (chainPaying ctx externalAddr2) $ \home _ -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        switchTo home 1 0
        _ <- answer home ["receive"] []
        _ <- answer home ["sync"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  portfolio0 of wallet1 btc, "

    it "show their portfolio and their network as fields with --verbose" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        twoPortfoliosPending home
        run <- hw home ["pendingtxs", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0\n  Network  : btc\n"

    it "take two lines each without --verbose" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        twoPortfoliosPending home
        run <- hw home ["pendingtxs"] []
        length (Text.lines (runOut run)) `shouldBe` 2

    -- Two payments of the default portfolio, two lines each, and a blank
    -- line between them
    it "are set apart by a blank line without --verbose" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--fee-rate", "10"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg (newerPayment ctx) 0, "--fee-rate", "10"] []
        run <- hw home ["pendingtxs"] []
        map Text.null (Text.lines (runOut run)) `shouldBe` [False, False, True, False, False]

    -- The time is the wallet's clock when preparetx ran, within a second
    it "have the time they were created" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        started <- getCurrentTime
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        ended <- getCurrentTime
        times <- createdTimesOf <$> hwJson home ["pendingtxs"] []
        let duringRun time = addUTCTime (-1) started <= time && time <= addUTCTime 1 ended
        (fmap (fmap duringRun) <$> times) `shouldBe` Right [Just True]

    it "start with the short id of the nosigHash of an unsigned transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isPrefixOf (Text.pack (shortOf nosig) <> "  ")

    it "start with the short id of the txid of a signed transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isPrefixOf (Text.pack (shortOf txid) <> "  ")

    it "hide the nosigHash of a signed transaction, even its short id" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf (Text.pack (shortOf nosig))

    it "show the time they were created to the second with --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["pendingtxs", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Created  : "

    it "are none, which pendingtxs says" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "There are no pending transactions"

{- Balances -}

-- | The addresses balances lists, each by its text, with the total, the
-- spendable and the unspendable balances of its coins, the sections of
-- the listing one after the other
balancesOf :: Response -> Either String [(Text, Word64, Word64, Word64)]
balancesOf (ResponseBalances groups) =
  Right [(dBAddressAddress addr, coinBalanceTotal b, coinBalanceSpendable b, coinBalanceUnspendable b) | (g, _) <- groups, (addr, b) <- groupRows g]
balancesOf other = Left (show other)

-- | The total balances gives each network of the listing, with what can
-- be spent of it: the value of every coin of the portfolio there
balanceTotalsOf :: Response -> Either String [(String, Word64, Word64)]
balanceTotalsOf (ResponseBalances groups) =
  Right [((placeNetwork (groupPlace g)).name, coinBalanceTotal total, coinBalanceSpendable total) | (g, total) <- groups]
balanceTotalsOf other = Left (show other)

-- The coin of the paid chain is in block 800000, three blocks deep at best
-- block 800002. Only the verbose view gives the block.
coinsListSpec :: Ctx -> Spec
coinsListSpec ctx =
  describe "The coins coins lists" $ do
    it "show the block that holds each coin with --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["coins", "--verbose"] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

    it "do not show the block without --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["coins"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "Block"

    it "are shown with every detail by lockcoin, which answers with one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["lockcoin", coinArg (payment ctx) 0] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

    it "are shown with every detail by unlockcoin, which answers with one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (payment ctx) 0] []
        run <- hw home ["unlockcoin", coinArg (payment ctx) 0] []
        Text.lines (runOut run) `shouldContain` ["  Block   : 800000"]

balancesListSpec :: Ctx -> Spec
balancesListSpec ctx =
  describe "The addresses balances lists" $ do
    -- The second receiving address holds nothing
    it "are those that hold coins, each with the value of its coins and what it can spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["receive"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(externalAddrsText !! 0, 50000, 50000, 0)]

    it "count the coin a pending payment spends as unspendable" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(externalAddrsText !! 0, 50000, 0, 50000)]

    -- The payment of 20000 spends the coin of 50000 and leaves 27740 of
    -- change at the first change address
    it "include the change addresses" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        _ <- answer home ["broadcast", nosig, "--yes"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(internalAddrsText !! 0, 27740, 27740, 0)]

    -- The payment of 20000 spends the coin of 50000 at /0/0
    it "tell with --verbose that a pending payment locks the coin it spends" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["balances", "--verbose"] []
        Text.lines (runOut run) `shouldContain` ["  Locked by tx     : 0.00050000 btc"]

    it "show the path of each address, then its total, spendable and unspendable balances" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["balances"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/0  " <> externalAddrsText !! 0 <> "\n  total 0.00050000 btc, spendable 0.00000000 btc, unspendable 0.00050000 btc\n")

    -- 50000 at the bip44 /0/0 and 30000 at the bip84 /0/0 on btc: 50000
    -- + 30000 = 80000. 50000 on bch.
    it "give each network its total, which adds up both accounts on btc" $
      withChain ctx (Chain 800002 [(bothTypesTx ctx, Just 800000), (cashCoinTx ctx, Just 800000)]) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--type", "bip84"] []
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        (balanceTotalsOf <$> hwJson home ["balances"] []) `shouldReturn` Right [("btc", 80000, 80000), ("bch", 50000, 50000)]

    -- The title line of the network: its place, then the total of its
    -- coins, what can be spent now, and the rest
    it "give the total of the network in the title of its section" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        run <- hw home ["balances"] []
        take 1 (Text.lines (runOut run))
          `shouldBe` ["portfolio0  btc  total 0.00080000 btc, spendable 0.00080000 btc, unspendable 0.00000000 btc"]

    -- --network narrows the listing to one network, and the title stays
    it "give the total of the network in the title of its section, though --network names one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
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
sectionsSpec :: Ctx -> Spec
sectionsSpec ctx =
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

    -- btc comes before bch among the networks of a portfolio
    it "lists the coins of each network under a title that names the network" $
      withChain ctx (Chain 800002 [(payment ctx, Just 800000), (cashCoinTx ctx, Just 800000)]) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        (titlesOf . runOut <$> hw home ["coins"] []) `shouldReturn` ["portfolio0  btc", "portfolio0  bch"]

    it "lists the transactions of each network under a title that names the network" $
      withChain ctx (Chain 800002 [(payment ctx, Just 800000), (cashCoinTx ctx, Just 800000)]) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        (titlesOf . runOut <$> hw home ["txs"] []) `shouldReturn` ["portfolio0  btc", "portfolio0  bch"]

    -- The payment of 50000 on btc: the title, a blank line, as above the
    -- addresses, then the two lines of the payment
    it "titles the one network that has rows, and leaves the others out" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        listed <- Text.lines . runOut <$> hw home ["txs"] []
        (take 1 listed, Text.null <$> listed) `shouldBe` (["portfolio0  btc"], [False, True, False, False])

    -- The title of a network in --verbose and --tabular too
    it "titles each section in every view" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        forM_ ["--compact", "--verbose", "--tabular"] $ \view -> do
          listed <- Text.lines . runOut <$> hw home ["coins", view] []
          (view, take 1 listed) `shouldBe` (view, ["portfolio0  btc"])

    -- A portfolio of another wallet is named with its wallet
    it "names the wallet of a portfolio that is not of wallet0 in the titles" $
      withChain ctx (chainPaying ctx externalAddr2) $ \home _ -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        switchTo home 1 0
        _ <- answer home ["receive"] []
        _ <- answer home ["sync"] []
        (take 1 . Text.lines . runOut <$> hw home ["coins"] []) `shouldReturn` ["portfolio0 of wallet1  btc"]

{- The answer of sync -}

-- sync goes over every network of every portfolio, and most of them
-- hold nothing
syncAnswerSpec :: Ctx -> Spec
syncAnswerSpec ctx =
  describe "The answer of sync" $
    -- portfolio0 holds the coin of 50000 on btc, and nothing else. The
    -- sync before found it, so this one finds nothing new.
    it "shows each network a portfolio holds something on, then the quiet networks of each portfolio in one line" $
      withChain ctx (paidChain ctx) $ \home _ -> do
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
-- the options hw gave less in LESS, and the lines of the listing. The
-- pager is a script named less, which answers less --version as less 668
-- does, so that hw gives it the options of that version. hw looks for the
-- pager on the PATH, and NO_COLOR keeps the listing plain.
paged :: FilePath -> [String] -> IO (Text, [Text])
paged home args = do
  let pager = home </> "less"
  writeFile pager $
    unlines
      [ "#!/bin/sh",
        "if [ \"$1\" = --version ]; then echo 'less 668 (a pager of the tests)'; exit 0; fi",
        "printf '%s' \"$LESS\" > \"$HOME/less-options\"",
        "exec /bin/cat > \"$HOME/paged\""
      ]
  setFileMode pager 0o700
  _ <- hwOnScreen [("HW_PAGER", pager), ("PATH", "/usr/bin:/bin"), ("NO_COLOR", "1")] home args
  options <- Text.pack <$> readFile (home </> "less-options")
  listed <- Text.lines . Text.pack <$> readFile (home </> "paged")
  return (options, listed)

-- The terminal of the tests has no size, so every table counts as taller
-- than the screen, which is when less can keep its header at the top
pagerSpec :: Ctx -> Spec
pagerSpec ctx =
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
      withChain ctx (paidChain ctx) $ \home _ -> do
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
addressCodeSpec :: Ctx -> Spec
addressCodeSpec ctx = do
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
      withChain ctx (paidChain ctx) $ \home _ -> do
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
nextStepSpec :: Ctx -> Spec
nextStepSpec ctx =
  describe "The next step of a payment or a sweep, shown after the answer" $ do
    it "is to sign the transaction preparetx made, or to export it to sign it offline, by the short id of its nosigHash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signtx " <> short)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> short <> " FILE")

    it "is to sign the file preparetx wrote on an offline computer, or the transaction here" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "unsigned.psbt", "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signtx --input " <> Text.pack (home </> "unsigned.psbt") <> "\n")
        run `shouldSatisfy` showsCommand ("hw signtx " <> short)

    it "is to sign the transaction preparesendall made, or to export it to sign it offline" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signtx " <> short)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> short <> " FILE")

    it "is to send the transaction signtx signed in the wallet, by the short id of its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["signtx", nosig] seedOnce
        txid <- txidOf =<< onePendingTx home
        run `shouldSatisfy` showsCommand ("hw broadcast " <> Text.pack (shortOf txid) <> "\n")

    it "is to send the file signtx signed offline, from the online computer" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        run `shouldSatisfy` showsCommand ("hw broadcast --input " <> Text.pack (signer </> "signed.psbt"))

    it "is to send the --input file signtx replaced with the signed transaction" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.psbt"] (["y"] <> seedOnce)
        run `shouldSatisfy` showsCommand ("hw broadcast --input " <> Text.pack (online </> "unsigned.psbt"))

    -- sync covers every portfolio on every network
    it "is to sync the wallet once broadcast sent the transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["broadcast", nosig, "--yes"] []
        run `shouldSatisfy` showsCommand "hw sync\n"

    -- The payment is in the active portfolio, which txs lists without
    -- an option
    it "is to see the transactions sync found, with no option for the active portfolio" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw txs\n"

    -- The chain pays 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, the first
    -- receiving address of portfolio1, and nothing of portfolio0, which is
    -- active. txs lists the active portfolio alone, so the steps start with
    -- the switch to portfolio1. The sync checked is the first one, which
    -- finds the payment.
    it "starts with the switch to see the transactions sync found in another portfolio than the active one" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
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
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- paymentSigned home
        let other = spendingTx ctx [(txHash (payment ctx), 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw pendingtxs\n"
        run `shouldSatisfy` showsCommand "hw deletetx HASH"
        run `shouldNotSatisfy` showsCommand "hw switch"

    -- The payment in conflict is of portfolio0, and portfolio1 is active:
    -- pendingtxs alone would list portfolio1, without the payment
    it "is to switch to the portfolio of the pending transaction a sync found in conflict first, when another portfolio is active" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- paymentSigned home
        newTestPortfolio home
        switchTo home 0 1
        let other = spendingTx ctx [(txHash (payment ctx), 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        run <- hw home ["sync"] []
        run
          `shouldSatisfy` showsCommand
            "Next, switch to portfolio0 of wallet0, where a pending transaction is in conflict:\n\
            \  hw switch 0 --wallet 0\n\
            \Then find the pending transaction in conflict, which can no longer be sent:\n\
            \  hw pendingtxs\n\
            \Then delete it, to free its coins:\n\
            \  hw deletetx HASH\n"

    -- A payment is signed in each portfolio, from its coin of
    -- 'twoPortfoliosTx', and the mempool spends both coins otherwise
    it "is to switch in turn to each portfolio that holds a pending transaction a sync found in conflict, when there are several" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home chain -> do
        syncedTwoPortfolios home
        forM_ [0, 1] $ \portfolio -> do
          switchTo home 0 portfolio
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--fee-rate", "10"] []
          answer home ["signtx", nosig] seedOnce
        let other = spendingTx ctx [(txHash (twoPortfoliosTx ctx), 0), (txHash (twoPortfoliosTx ctx), 1)] [(otherAddr 1, 70000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        run <- hw home ["sync"] []
        run
          `shouldSatisfy` showsCommand
            "Next, switch in turn to each portfolio where a pending transaction is in conflict:\n\
            \  hw switch 0 --wallet 0\n\
            \  hw switch 1 --wallet 0\n\
            \In each, find the pending transactions in conflict, which can no longer be sent:\n\
            \  hw pendingtxs\n\
            \Then delete them, to free their coins:\n\
            \  hw deletetx HASH\n"

    -- The key of the old wallet is in a file, and the sweep pays
    -- portfolio0
    it "is to sign the sweep preparesweep made with the keys of its file, or to export it to sign it where the keys are" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
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
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        short <- onePendingShortId home
        run `shouldSatisfy` showsCommand ("hw signsweep " <> short <> "\n")

    it "is to sign the file of the sweep where the keys are, when preparesweep wrote one" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-to", textOf (externalAddr 0), "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signsweep --input " <> Text.pack (home </> "sweep.json") <> "\n")

    -- In a database without wallets, the sweep is in its file only
    it "is to sign the file of a sweep outside the wallet with the keys of its file" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        run <- hw home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signsweep --input " <> Text.pack (home </> "sweep.json") <> " --key-file " <> Text.pack keys)

    it "is to send the sweep signsweep signed in the wallet, by the short id of its txid" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        run <- hw home ["signsweep", nosig, "--key-file", keys] []
        txid <- txidOf =<< onePendingTx home
        run `shouldSatisfy` showsCommand ("hw broadcast " <> Text.pack (shortOf txid) <> "\n")

    it "is to send the file of a sweep outside the wallet that signsweep signed" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run <- hw home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.psbt", "--key-file", keys] []
        run `shouldSatisfy` showsCommand ("hw broadcast --input " <> Text.pack (home </> "signed.psbt"))

    -- The name of the file with a space would be two words for the shell
    it "quotes a file name the shell would split" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "my payment.psbt", "--fee-rate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signtx --input '" <> Text.pack (home </> "my payment.psbt") <> "'\n")

    it "is not shown with --json, whose output is the answer alone" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--json", "--fee-rate", "10"] []
        run `shouldNotSatisfy` showsCommand "hw signtx"

{- CSV files -}

-- | The listings --csv writes to a file
csvSpec :: Ctx -> Spec
csvSpec ctx =
  describe "A listing written with --csv" $ do
    -- The coin of the payment, 3 confirmations deep, can be spent. The
    -- file starts with EF BB BF, the byte order mark of UTF-8.
    it "is written to the file --csv names, a row for each address that holds coins, and hw says where" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let file = home </> "balances.csv"
        run <- hw home ["balances", "--csv", file] []
        runOut run `shouldBe` "Written to " <> Text.pack file <> "\n"
        BS.readFile file
          `shouldReturn` "\xEF\xBB\xBF\&Network,Type,Path,Address,Label,Total (sats),Spendable (sats),Locked by tx (sats),Locked by you (sats),Not spendable yet (sats),Coins\r\nbtc,bip44,/0/0,1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,,50000,50000,0,0,0,1\r\n"

    -- The store saw the payment at 22:13:20 UTC, 23:13:20 in Central
    -- Europe in November
    it "gives the time of a transaction in UTC, whatever the time zone of the computer" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let file = home </> "txs.csv"
        _ <- hwWithEnv [("TZ", centralEurope)] home ["txs", "--csv", file] []
        content <- BS.readFile file
        content `shouldSatisfy` BS.isInfixOf "\r\nbtc,2023-11-14T22:13:20,Incoming,"

    it "writes the headings alone for a listing with no rows" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "pending.csv"
        _ <- answer home ["pendingtxs", "--csv", file] []
        BS.readFile file
          `shouldReturn` "\xEF\xBB\xBF\&Wallet,Portfolio,Network,Created (UTC),Type,Address types,Amount (sats),Fee (sats),Fee rate (sat/vB),Status,Nosighash,Short nosighash,Txid,Short txid,Label,Address,Path,Address label,Other addresses\r\n"

    -- 50000 at /0/0 is the largest total, before 30000 at /0/1
    it "writes the rows --max allows, the first ones of the listing" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        let file = home </> "balances.csv"
        _ <- answer home ["balances", "--csv", file, "--max", "1"] []
        BS.readFile file
          `shouldReturn` "\xEF\xBB\xBF\&Network,Type,Path,Address,Label,Total (sats),Spendable (sats),Locked by tx (sats),Locked by you (sats),Not spendable yet (sats),Coins\r\nbtc,bip44,/0/0,1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,,50000,50000,0,0,0,1\r\n"

    -- Each network of the portfolio on a row of its own, eight in all,
    -- after the row of the headings
    it "gives the portfolios a row for each network, after a Network column" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "portfolios.csv"
        _ <- answer home ["portfolios", "--csv", file] []
        rows <- Text.lines . decodeUtf8 <$> BS.readFile file
        (take 1 rows, length rows) `shouldBe` (["\xFEFFWallet,Portfolio,Active,Description,Network,Spendable (sats),Confirmed (sats),Unconfirmed (sats),Coins,Addresses,Created (UTC)\r"], 9)

    -- Eight rows of each portfolio, one for each network
    it "says yes in the column Active of the portfolios for each row of the active one, and no for the others" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        let file = home </> "portfolios.csv"
        _ <- answer home ["portfolios", "--csv", file] []
        rows <- drop 1 . Text.lines . decodeUtf8 <$> BS.readFile file
        map (Text.intercalate "," . take 3 . Text.splitOn ",") rows
          `shouldBe` replicate 8 "wallet0,portfolio0,no" <> replicate 8 "wallet0,portfolio1,yes"

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
exportSheetSpec :: Ctx -> Spec
exportSheetSpec ctx =
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

    it "has a tab for each listing, in the order of the help" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "wallet.ods"
        _ <- answer home ["exportsheet", file] []
        tabsOf <$> contentOf file `shouldReturn` ["Portfolios", "Addresses", "Balances", "Coins", "Transactions", "Pending transactions"]

    -- The coin of the payment, 3 confirmations deep, can be spent: 50000
    -- in all and spendable, nothing locked or waiting, in one coin
    it "gives the balance of each address, as numbers of satoshis, after the columns that say where it is" $
      withChain ctx (paidChain ctx) $ \home _ -> do
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
                  "<table:table-cell office:value-type=\"string\"><text:p>/0/0</text:p></table:table-cell>",
                  "<table:table-cell office:value-type=\"string\"><text:p>1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj</text:p></table:table-cell>",
                  -- No label
                  "<table:table-cell/>",
                  -- Total, spendable, locked by tx, locked by you, not spendable yet
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"50000\"><text:p>50000</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"50000\"><text:p>50000</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"0\"><text:p>0</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"0\"><text:p>0</text:p></table:table-cell>",
                  "<table:table-cell table:style-name=\"satoshis\" office:value-type=\"float\" office:value=\"0\"><text:p>0</text:p></table:table-cell>",
                  -- Coins
                  "<table:table-cell office:value-type=\"float\" office:value=\"1\"><text:p>1</text:p></table:table-cell>",
                  "</table:table-row>"
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
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
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
