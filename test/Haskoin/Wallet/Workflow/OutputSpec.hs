{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What hw shows: the listings and their paging, a transaction as hw
-- lays it out, the pending transactions, the balances of the addresses,
-- and the next step shown after a payment or a sweep.
module Haskoin.Wallet.Workflow.OutputSpec (spec) where

import Control.Monad (forM_, replicateM_, void)
import Data.IORef (modifyIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..), DBAddress (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..), coinBalanceUnspendable)
import Haskoin.Wallet.Database.Coin (JsonCoin (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  listingSpec ctx
  layoutSpec ctx
  pendingListSpec ctx
  balancesListSpec ctx
  nextStepSpec ctx

{- Listings -}

-- | How many rows a listing shows
rowsOf :: Response -> Either String Int
rowsOf (ResponseAddresses _ addrs) = Right (length addrs)
rowsOf (ResponseBalances _ addrs) = Right (length addrs)
rowsOf (ResponseTxs _ txs) = Right (length txs)
rowsOf (ResponsePendingTxs _ held) = Right (length held)
rowsOf (ResponseCoins _ coins) = Right (length coins)
rowsOf other = Left (show other)

-- | What names each row of a listing, as --after takes it: the address,
-- the coin, the txid, or the hash of a pending transaction as shown
keysOf :: Response -> Either String [String]
keysOf (ResponseAddresses _ addrs) = Right (Text.unpack . dBAddressAddress . fst <$> addrs)
keysOf (ResponseBalances _ addrs) = Right (Text.unpack . dBAddressAddress . fst <$> addrs)
keysOf (ResponseTxs _ infos) = Right [Text.unpack (txHashToHex (chainTxid status)) | info <- infos, OnChain status <- [txInfoStatus info]]
keysOf (ResponsePendingTxs _ held) = Right [Text.unpack (txHashToHex (pendingShownHash status)) | (_, info) <- held, Pending status <- [txInfoStatus info]]
keysOf (ResponseCoins _ coins) = Right [Text.unpack (txHashToHex outpoint.hash) <> ":" <> show outpoint.index | coin <- coins, let outpoint = jsonCoinOutpoint coin]
keysOf other = Left (show other)

-- | The listings, each with one row in a synced home with a payment
-- prepared: the address paid, the payment received, its coin, the payment
-- prepared, and the address that holds the coin
listings :: [String]
listings = ["addresses", "txs", "coins", "pendingtxs", "balances"]

-- | The time of each transaction an answer lists
timesOf :: Response -> Either String [Maybe UTCTime]
timesOf (ResponseTxs _ infos) = Right (chainTime . chainOf <$> infos)
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
        createMain home
        replicateM_ 6 (answer home ["receive"] [])
        (rowsOf <$> hwJson home ["addresses"] []) `shouldReturn` Right 6

    it "shows the row of each listing" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        forM_ listings $ \listing ->
          (rowsOf <$> hwJson home [listing] []) `shouldReturn` Right 1

    -- 2^64 + 1 wrapped around to 1, and showed one address of the three
    it "shows every row with a --max past 64 bits, rather than wrapping around" $
      withHome $ \home -> do
        createMain home
        replicateM_ 3 $ answer home ["receive"] []
        (rowsOf <$> hwJson home ["addresses", "--max", "18446744073709551617"] []) `shouldReturn` Right 3

    it "refuses a --max of 0" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["addresses", "--max", "0"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --max: cannot parse value `0'"

    it "refuses to start after a row that is not in it" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["addresses", "--after", Text.unpack (internalAddrsText !! 0)] [])
          `shouldReturn` Right ("The address " <> internalAddrsText !! 0 <> " is not a receiving address of the account")

    -- second is an account of its own, without any row
    it "shows the rows of the account of --account only" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["createacc", "second"] seedTwice
        forM_ listings $ \listing -> do
          (rowsOf <$> hwJson home [listing, "--account", "main"] []) `shouldReturn` Right 1
          (rowsOf <$> hwJson home [listing, "--account", "second"] []) `shouldReturn` Right 0

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
        shown `shouldSatisfy` Text.isInfixOf "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0\n"

    it "shows the labels of the addresses of the wallet" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "salary"] []
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0  \"salary\"\n"

    it "shows the recipients of a payment" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc\n"

    it "shows the change of a payment, at its change address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  /1/0\n"

    it "shows the change a payment sends back to the address it spends from" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        shown <- shownPayment home ["--spend-from", textOf (externalAddr 0), "--change-back"]
        shown `shouldSatisfy` Text.isInfixOf "  Change   : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00027740 btc  /0/0\n"

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
        listed `shouldSatisfy` Text.isInfixOf "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0\n"

    it "shows the address of the wallet a payment received is paid at, in txs" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "\n  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    it "does not show the change the payer of a payment received keeps, in either view" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        compact <- runOut <$> hw home ["txs"] []
        verbose <- runOut <$> hw home ["txs", "--verbose"] []
        compact `shouldNotSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 1)))
        verbose `shouldNotSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 1)))

    it "lists each transaction in three lines without --verbose" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        length (Text.lines listed) `shouldBe` 3

    -- The two payments, three lines each, and a blank line between them
    it "sets the transactions apart with a blank line without --verbose" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        map Text.null (Text.lines listed) `shouldBe` [False, False, False, True, False, False, False]

    -- The store saw the payment at 22:13:20 UTC, 23:13:20 in Central
    -- Europe in November
    it "shows the time of a transaction in the time zone of the computer, to the minute" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hwWithEnv [("TZ", centralEurope)] home ["txs"] []
        listed `shouldSatisfy` Text.isPrefixOf "14 Nov 2023 23:13  Incoming"

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
        listed `shouldSatisfy` Text.isPrefixOf "15 Nov 2023 00:13  Incoming"

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
        sent `shouldSatisfy` Text.isInfixOf "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  0.00020000 btc  /0/1\n"
        sent `shouldSatisfy` Text.isInfixOf "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  /1/0  change address\n"
        shown `shouldNotSatisfy` Text.isInfixOf "  Change   : "

-- old key
paymentAndKeyChain :: Ctx -> Chain
paymentAndKeyChain ctx = Chain 800002 [(payment ctx, Just 800000), (oldCoin ctx, Just 800000)]

-- | A home with the accounts main and second, main holding a payment, and
-- second a sweep of the old key, prepared after it
twoAccountsPending :: FilePath -> IO ()
twoAccountsPending home = do
  createMain home
  _ <- answer home ["createacc", "second"] seedTwice
  _ <- answer home ["receive", "--account", "main"] []
  _ <- answer home ["sync"] []
  _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "main", "--fee-rate", "10"] []
  secondAddr <- receiveFor home "second"
  keys <- writeKeyFile home
  void $ answer home ["preparesweep", "--key-file", keys, "--sweep-to", secondAddr, "--fee-rate", "10"] []

-- | The account pendingtxs was asked for, and the account of each pending
-- transaction it lists
pendingAccountsOf :: Response -> Either String (Maybe Text, [Text])
pendingAccountsOf (ResponsePendingTxs asked held) = Right (dBAccountName <$> asked, dBAccountName . fst <$> held)
pendingAccountsOf other = Left (show other)

-- | When each pending transaction pendingtxs lists was created
createdTimesOf :: Response -> Either String [Maybe UTCTime]
createdTimesOf (ResponsePendingTxs _ held) = Right (created . snd <$> held)
  where
    created info = case txInfoStatus info of
      Pending status -> pendingCreated status
      OnChain _ -> Nothing
createdTimesOf other = Left (show other)

pendingListSpec :: Ctx -> Spec
pendingListSpec ctx =
  describe "The pending transactions pendingtxs lists" $ do
    it "are those of every account without --account, newest first, each with its account" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        (pendingAccountsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right (Nothing, ["second", "main"])

    it "are those of the account of --account only" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        (pendingAccountsOf <$> hwJson home ["pendingtxs", "--account", "main"] []) `shouldReturn` Right (Just "main", ["main"])

    it "show their account when listed for every account" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  account second, "
        runOut run `shouldSatisfy` Text.isInfixOf "\n  account main, "

    it "show their account as a field with --verbose when listed for every account" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        run <- hw home ["pendingtxs", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Account  : second\n"
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Account  : main\n"

    it "do not show their account when listed for one account" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        run <- hw home ["pendingtxs", "--account", "main"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "account main"

    it "take three lines each without --verbose" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        run <- hw home ["pendingtxs", "--account", "main"] []
        length (Text.lines (runOut run)) `shouldBe` 3

    -- The payment of main and the sweep of second, three lines each, and a
    -- blank line between them
    it "are set apart by a blank line without --verbose" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        run <- hw home ["pendingtxs"] []
        map Text.null (Text.lines (runOut run)) `shouldBe` [False, False, False, True, False, False, False]

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

    it "show an unsigned transaction by its nosigHash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n  " <> Text.pack nosig <> "\n")

    it "show a signed transaction by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("\n  " <> Text.pack txid <> "\n")

    it "hide the nosigHash of a signed transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf (Text.pack nosig)

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
-- spendable and the unspendable balances of its coins
balancesOf :: Response -> Either String [(Text, Word64, Word64, Word64)]
balancesOf (ResponseBalances _ addrs) =
  Right [(dBAddressAddress addr, coinBalanceTotal b, coinBalanceSpendable b, coinBalanceUnspendable b) | (addr, b) <- addrs]
balancesOf other = Left (show other)

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
        _ <- answer home ["sendtx", nosig, "--yes"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(internalAddrsText !! 0, 27740, 27740, 0)]

    it "show the path of each address, then its total, spendable and unspendable balances" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["balances"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/0  " <> externalAddrsText !! 0 <> "\n  total 0.00050000 btc, spendable 0.00000000 btc, unspendable 0.00050000 btc\n")

    it "are none before a payment, which balances says" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["balances"] []
        runOut run `shouldSatisfy` Text.isInfixOf "There are no addresses with coins to display"

{- Next steps -}

-- | The nosigHash of the one pending transaction of a home
onePendingNosig :: FilePath -> IO Text
onePendingNosig home = Text.pack <$> (nosigOf =<< onePendingTx home)

-- | Whether the output of a run shows the command
showsCommand :: Text -> Run -> Bool
showsCommand command run = command `Text.isInfixOf` runOut run

-- The pretty output, after the answer
nextStepSpec :: Ctx -> Spec
nextStepSpec ctx =
  describe "The next step of a payment or a sweep, shown after the answer" $ do
    it "is to sign the transaction preparetx made, or to export it to sign it offline" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signtx " <> nosig)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> nosig <> " FILE")

    it "is to sign the file preparetx wrote on an offline computer, or the transaction here" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "unsigned.psbt", "--fee-rate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signtx --input " <> Text.pack (home </> "unsigned.psbt") <> "\n")
        run `shouldSatisfy` showsCommand ("hw signtx " <> nosig)

    it "is to sign the transaction preparesendall made, or to export it to sign it offline" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signtx " <> nosig)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> nosig <> " FILE")

    it "is to send the transaction signtx signed in the wallet, by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["signtx", nosig] seedOnce
        txid <- txidOf =<< onePendingTx home
        run `shouldSatisfy` showsCommand ("hw sendtx " <> Text.pack txid)

    it "is to send the file signtx signed offline, from the online computer" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        run `shouldSatisfy` showsCommand ("hw sendtx --input " <> Text.pack (signer </> "signed.psbt"))

    it "is to send the --input file signtx replaced with the signed transaction" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.psbt"] (["y"] <> seedOnce)
        run `shouldSatisfy` showsCommand ("hw sendtx --input " <> Text.pack (online </> "unsigned.psbt"))

    it "is to sync the account of the transaction sendtx sent" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["sendtx", nosig, "--yes"] []
        run `shouldSatisfy` showsCommand "hw sync --account main"

    it "is to see the transactions of the account synced" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw txs --account main"

    -- Another transaction of the mempool spends the coin of the payment
    it "is to find and delete the pending transaction a sync found in conflict" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- paymentSigned home
        let other = spendingTx ctx [(txHash (payment ctx), 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw pendingtxs --account main"
        run `shouldSatisfy` showsCommand "hw deletetx HASH"

    -- The key of the old wallet is in a file, and the sweep pays main
    it "is to sign the sweep preparesweep made with the keys of its file, or to export it to sign it where the keys are" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        run <- hw home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signsweep " <> nosig <> " --key-file " <> Text.pack keys)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> nosig <> " FILE")

    -- Prepared from the address of the key, without it: signsweep asks
    -- for the key on the terminal
    it "is to sign the sweep preparesweep made from addresses, the keys typed when asked" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signsweep " <> nosig <> "\n")

    it "is to sign the file of the sweep where the keys are, when preparesweep wrote one" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-to", textOf (externalAddr 0), "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signsweep --input " <> Text.pack (home </> "sweep.json") <> "\n")

    -- In a wallet without accounts, the sweep is in its file only
    it "is to sign the file of a sweep outside the wallet with the keys of its file" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        run <- hw home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signsweep --input " <> Text.pack (home </> "sweep.json") <> " --key-file " <> Text.pack keys)

    it "is to send the sweep signsweep signed in the wallet, by its txid" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        run <- hw home ["signsweep", nosig, "--key-file", keys] []
        txid <- txidOf =<< onePendingTx home
        run `shouldSatisfy` showsCommand ("hw sendtx " <> Text.pack txid)

    it "is to send the file of a sweep outside the wallet that signsweep signed" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run <- hw home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.psbt", "--key-file", keys] []
        run `shouldSatisfy` showsCommand ("hw sendtx --input " <> Text.pack (home </> "signed.psbt"))

    -- The name with a space would be two words for the shell
    it "quotes an account name the shell would split" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["renameacc", "main", "my main"] []
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw txs --account 'my main'"

    it "is not shown with --json, whose output is the answer alone" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--json", "--fee-rate", "10"] []
        run `shouldNotSatisfy` showsCommand "hw signtx"
