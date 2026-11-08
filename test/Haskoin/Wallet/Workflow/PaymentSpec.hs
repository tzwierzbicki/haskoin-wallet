{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A payment as the user prepares it with preparetx, after a sync: the
-- fee rates recent blocks paid, the coins received and locked, the amount
-- and the fee, the confirmations asked of the coins, a fee rate too high,
-- coin control, and the change sent back.
module Haskoin.Wallet.Workflow.PaymentSpec (spec) where

import Control.Monad (forM_, void)
import qualified Data.Aeson as Json
import Data.IORef (modifyIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..), DBRawTx, runDBWith)
import Haskoin.Wallet.Database.Coin (CoinLock (..), LeftReason (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.FeeRates (FeeRates (..))
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData (..))
import Haskoin.Wallet.Workflow.TestUtils
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  feesSpec ctx
  receivingSpec ctx
  paymentSpec ctx
  minConfSpec ctx
  highFeeSpec ctx
  coinControlSpec ctx
  changeBackSpec ctx

{- Fee rates -}

-- | A chain whose latest two blocks hold three transactions: one paying
-- the wallet, of 85 bytes and a fee of 1000 satoshis (11.76 per byte);
-- one of two outputs, of 119 bytes and 1000 satoshis (8.40 per byte); and
-- one spending the latter, of 85 bytes and 10000 satoshis (117.65 per
-- byte). The sizes follow from the serialization: 4 bytes of version, 41
-- per input, 34 per P2PKH output, 4 of lock time, and a count byte each
-- for the inputs and the outputs. Sorted by rate, the 289 bytes reach a
-- quarter within the 119 at 8.40, half within the next 85 at 11.76, and
-- nine tenths within the last 85 at 117.65: rounded up, 9, 12 and 118.
feesChain :: Ctx -> Chain
feesChain ctx =
  Chain
    800001
    [ (fundingTx ctx 1 [(externalAddr 0, 50000)], Just 800000),
      (twoOutputs, Just 800001),
      (spendingTx ctx [(txHash twoOutputs, 0)] [(otherAddr 2, 50000)], Just 800001)
    ]
  where
    twoOutputs = fundingTx ctx 2 [(otherAddr 1, 60000), (otherAddr 1, 70000)]

feesSpec :: Ctx -> Spec
feesSpec ctx =
  describe "The fee rates recent blocks paid" $ do
    it "are listed by fees, with the blocks they come from and the age of the latest" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        run <- hw home ["fees"] []
        forM_
          [ "Fee rates of btc, from blocks 800000 to 800001\n",
            "  low 9, median 12, high 118 sat/vB\n",
            "  the latest block was mined 3 minutes ago\n"
          ]
          $ \line -> run `shouldSatisfy` printed line
        answered <- answer home ["fees"] []
        case answered of
          ResponseFees net rates ->
            (net.name, rates.rateLow, rates.rateMedian, rates.rateHigh, rates.ratesFromHeight, rates.ratesToHeight)
              `shouldBe` ("btc", 9, 12, 118, 800000, 800001)
          other -> expectationFailure $ "Not a fees answer: " <> show other

    -- The configuration file of the test home names btc, and the fake store
    -- serves the same chain on every network
    it "are those of the --network given" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        answered <- answer home ["fees", "--network", "bch"] []
        case answered of
          ResponseFees net _ -> net.name `shouldBe` "bch"
          other -> expectationFailure $ "Not a fees answer: " <> show other

    it "are shown by preparetx without --fee-rate, which asks which to take, the median by default" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.psbt"
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file] [""]
        run `shouldSatisfy` printed "low 9, median 12, high 118 sat/vB"
        run `shouldSatisfy` printed "Which fee rate? Type low, median, high or a number of satoshis per virtual byte [median]"
        (txSignDataFeeRate . txFileData <$> readTxFile ctx file) `shouldReturn` Just 12

    it "are taken at the level typed" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.psbt"
        _ <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file] ["high", "y"]
        (txSignDataFeeRate . txFileData <$> readTxFile ctx file) `shouldReturn` Just 118

    it "are not the only answer: a number typed is the fee rate taken" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.psbt"
        _ <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file] ["15"]
        (txSignDataFeeRate . txFileData <$> readTxFile ctx file) `shouldReturn` Just 15

    it "are taken at the level of --fee-rate without asking" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.psbt"
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file, "--fee-rate", "low"] []
        run `shouldSatisfy` printed "The low rate recent blocks paid: 9 satoshis per virtual byte"
        run `shouldNotSatisfy` printed "Which fee rate?"
        (txSignDataFeeRate . txFileData <$> readTxFile ctx file) `shouldReturn` Just 9

    it "stop preparetx without --fee-rate when the store cannot be reached: hw never guesses a rate" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002"] [])
          `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed. Give a --fee-rate to go on without the rates recent blocks paid"

    it "are offered to bumpfee only above what a replacement must pay" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        let file = home </> "replacement.json"
        run <- hw home ["bumpfee", stuck, "--output", file] [""]
        run `shouldSatisfy` printed "The transaction pays 10 satoshis per virtual byte: a replacement must pay at least 11"
        run `shouldSatisfy` printed "Which fee rate? Type median, high or a number of satoshis per virtual byte [median]"
        (txSignDataFeeRate . txFileData <$> readTxFile ctx file) `shouldReturn` Just 12

    it "are offered to bumpfee beside a number, asked again when below what a replacement must pay" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        let file = home </> "replacement.json"
        run <- hw home ["bumpfee", stuck, "--output", file] ["10", "11"]
        run `shouldSatisfy` printed "The fee rate must be at least 11 satoshis per virtual byte"
        (txSignDataFeeRate . txFileData <$> readTxFile ctx file) `shouldReturn` Just 11

    -- The one transaction of the paid chain pays 1000 satoshis for 85
    -- bytes, 11.76 per byte: every level is 12. The payment pays 20 per
    -- byte, so a replacement must pay 21.
    it "leave bumpfee asking for a number when all are below what a replacement must pay" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "20"] []
        _ <- answer home ["signtx", nosig] seedOnce
        stuck <- txidOf =<< txOf =<< answer home ["broadcast", nosig, "--yes"] []
        let file = home </> "replacement.json"
        run <- hw home ["bumpfee", stuck, "--output", file] ["25"]
        run `shouldSatisfy` printed "Which fee rate? Type a number of satoshis per virtual byte, at least 21:"
        (txSignDataFeeRate . txFileData <$> readTxFile ctx file) `shouldReturn` Just 25

{- Receiving -}

receivingSpec :: Ctx -> Spec
receivingSpec ctx = do
  describe "A payment to the wallet" $ do
    it "is found by sync, which tells what the account can spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 50000)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 50000)]

    -- 50000 satoshis are 0.0005 bitcoins and 500 bits
    it "is shown by accounts in bitcoins, or in the unit of --unit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (printed "0.00050000 btc" <$> hw home ["accounts"] []) `shouldReturn` True
        (printed "50000 sats" <$> hw home ["accounts", "--unit", "sat"] []) `shouldReturn` True
        (printed "500.00 bits" <$> hw home ["accounts", "--unit", "bit"] []) `shouldReturn` True

    -- The help says the two options go before or after the command
    it "is shown in the unit of --unit given before the command" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (printed "50000 sats" <$> hw home ["--unit", "sat", "accounts"] []) `shouldReturn` True

    it "is shown in the unit of the configuration file when --unit is not given" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setUnit home "sat"
        (printed "50000 sats" <$> hw home ["accounts"] []) `shouldReturn` True

    -- 50000 satoshis are 500 bits
    it "is shown in the unit of --unit, even when the configuration file names another" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setUnit home "sat"
        (printed "500.00 bits" <$> hw home ["accounts", "--unit", "bit"] []) `shouldReturn` True

    -- A script that reads the JSON answers is not affected by the unit
    it "is answered in JSON in satoshis, whatever the unit of the configuration file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setUnit home "bit"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 50000)]

    it "is answered in JSON with --json given before the command" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["--json", "accounts"] []
        (accountsOf <$> Json.eitherDecodeStrict (encodeUtf8 (runOut run))) `shouldBe` Right (Right [("main", 0, 50000)])

    it "is listed by txs, with its confirmations" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (txsOf <$> hwJson home ["txs"] []) `shouldReturn` Right [(TxCredit, 50000, Just 3)]

    it "gives a coin, which coins lists" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Nothing, True)]

    -- Another wallet can replace a payment it has not yet seen confirmed
    it "can be spent once confirmed, not while in the mempool" $
      withChain ctx (Chain 800002 [(payment ctx, Nothing)]) $ \home chain -> do
        synced home
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 0, Nothing, False)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]
        modifyIORef chain mine
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 50000)]
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 1, Nothing, True)]

  -- Only this wallet can replace the payment, which spends the coin of
  -- main. At 10 satoshis a byte, its 226 bytes cost 2260, so main keeps
  -- 50000 - 30000 - 2260 = 17740 of change.
  describe "A payment from another account of the wallet" $
    it "can be spent while in the mempool, like the change of the account that paid it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "main"] []
        secondAddr <- receiveFor home "second"
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", secondAddr <> ":0.0003", "--account", "main", "--fee-rate", "10"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["broadcast", nosig, "--yes"] []
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 17740), ("second", 1, 30000)]
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "second", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(account1Addr, 30000)]

  describe "A coin locked by the user" $ do
    let twoCoins = fundingTx ctx 1 [(externalAddr 0, 50000), (externalAddr 0, 20000)]
        chain = Chain 800002 [(twoCoins, Just 800000)]
        firstCoin = OutPoint (txHash twoCoins) 0
        coinArgs = [Text.unpack (txHashToHex (txHash twoCoins)) <> ":0"]
        locks = fmap (fmap (\(outpoint, _, _, lock, _) -> (outpoint, lock)))
    it "is locked by lockcoin and freed by unlockcoin, which refuses it then" $
      withChain ctx chain $ \home _ -> do
        synced home
        (locks . coinsShown <$> hwJson home ("lockcoin" : coinArgs) []) `shouldReturn` Right [(firstCoin, Just LockedByUser)]
        (locks . coinsShown <$> hwJson home ("unlockcoin" : coinArgs) []) `shouldReturn` Right [(firstCoin, Nothing)]
        (errorOf <$> hwJson home ("unlockcoin" : coinArgs) []) `shouldReturn` Right "The coin is not locked"

    it "is unlocked by its short id" $
      withChain ctx chain $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg twoCoins 0] []
        (locks . coinsShown <$> hwJson home ["unlockcoin", shortCoinArg twoCoins 0] []) `shouldReturn` Right [(firstCoin, Nothing)]

    -- No transaction of the wallet has a txid that starts with 00000000
    it "is not found by a short id that matches no coin of the wallet" $
      withChain ctx chain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["lockcoin", "00000000:0"] []) `shouldReturn` Right "The wallet has no coin 00000000:0"

    -- Copied from the listing, as the user would
    it "is written by coins by its short id, the first 8 digits of its txid and its index, which lockcoin takes" $
      withChain ctx chain $ \home _ -> do
        synced home
        listed <- hw home ["coins"] []
        let written = shortCoinArg twoCoins 0
        Text.lines (runOut listed) `shouldSatisfy` any (Text.isPrefixOf (Text.pack written <> "  "))
        (locks . coinsShown <$> hwJson home ["lockcoin", written] []) `shouldReturn` Right [(firstCoin, Just LockedByUser)]

    it "is listed alone by coins --locked" $
      withChain ctx chain $ \home _ -> do
        synced home
        _ <- answer home ("lockcoin" : coinArgs) []
        (locks . coinsShown <$> hwJson home ["coins", "--locked"] []) `shouldReturn` Right [(firstCoin, Just LockedByUser)]
        (fmap length . coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right 2

    it "is not spent by preparetx" $
      withChain ctx chain $ \home _ -> do
        synced home
        _ <- answer home ("lockcoin" : coinArgs) []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

  describe "A sync" $ do
    -- The store reports every address used, as one that lies does: after
    -- a thousand on the receiving chain, sync asks, and a no leaves the
    -- account as it was, without an address
    it "asks whether to keep looking when the store reports used addresses a thousand deep, and generates none of them when the answer is no" $
      withLyingChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        run <- hw home ["sync"] ["n"]
        run `shouldSatisfy` printed "The store reports used addresses as far as 1000 on the receiving chain of the account main, and more beyond. Keep looking? [y/N] "
        run `shouldSatisfy` printed "The scan stopped there."
        runExit run `shouldBe` ExitFailure 1
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

    it "stops at the question when the input has ended, as a sync nobody watches does" $
      withLyingChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        run <- hw home ["sync"] []
        run `shouldSatisfy` printed "Keep looking? [y/N] "
        run `shouldSatisfy` printed "Cancelled: the input ended before the answer"
        runExit run `shouldBe` ExitFailure 1

    -- The store answers every question about Bitcoin Cash with the error
    -- page of a proxy, as when the store behind the proxy is down. hw
    -- failed the whole sync before, with the error StringError "could not
    -- decode error". The chain pays main 50000.
    it "syncs the accounts whose store is up, and leaves out those whose store is down, with the failure of their store" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        answered <- hwJson home ["sync"] []
        accountsOf answered `shouldBe` Right [("main", 0, 50000)]
        (map fst <$> notSyncedOf answered) `shouldBe` Right ["cash"]
        (map snd <$> notSyncedOf answered)
          `shouldSatisfy` either (const False) (all (\failure -> "The store at http://127.0.0.1:" `Text.isPrefixOf` failure && "/ answered with an error hw could not read" `Text.isSuffixOf` failure))

    it "exits with 1 when it left out an account, and prints why" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        run <- hw home ["sync"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "cash  bch  /44'/145'/0'\n  not synced: The store at http://127.0.0.1:"

    -- The chain pays main: a sync of second alone leaves main as it was
    it "updates only the account of --account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        _ <- answer home ["createacc", "second"] seedTwice
        (accountsOf <$> hwJson home ["sync", "--account", "second"] []) `shouldReturn` Right [("second", 1, 0)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0), ("second", 1, 0)]

    -- The database lost the transaction the coin of the payment comes
    -- from, which signing needs. No balance changed, so only a full sync
    -- looks at the coin again.
    it "fetches with --full the raw transaction of a coin the database lost, as preparetx asks" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        dropRawTxs home
        let missing = "The raw transaction " <> txHashToHex (txHash (payment ctx)) <> " of a coin is missing: sync --full fetches it"
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []) `shouldReturn` Right missing
        _ <- answer home ["sync", "--full"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

-- | Drop every raw transaction from the database of a home, as a database
-- that lost them
dropRawTxs :: FilePath -> IO ()
dropRawTxs home = runDBWith 100 (home </> ".hw" </> "accounts.sqlite") $ P.deleteWhere ([] :: [P.Filter DBRawTx])

{- Amounts and fees -}

paymentSpec :: Ctx -> Spec
paymentSpec ctx =
  describe "A payment prepared by preparetx" $ do
    -- The default unit is the bitcoin; --unit changes it, and a unit after
    -- the amount wins over both
    it "takes its amount in the unit of --unit, or in the one typed after it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        forM_ [("0.0002", []), ("20000", ["--unit", "sat"]), ("0.0002btc", []), ("200bits", []), ("0.0002btc", ["--unit", "sat"])] $ \(amount, unit) -> do
          prepared <- txOf =<< answer home (["preparetx", fixedTo (otherAddr 0) amount, "--fee-rate", "10"] <> unit) []
          paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000)]
          nosig <- nosigOf prepared
          void $ answer home ["deletetx", nosig] []

    it "takes an amount typed without a unit in the unit of the configuration file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setUnit home "sat"
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "20000", "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000)]

    it "pays each recipient the amount after its address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)]

    -- The best block of the chain is 800002, mined three minutes before the
    -- test. One time in ten the lock time is below it, by up to 99 blocks.
    it "has the lock time of the best block the wallet knows, or one up to 99 blocks below it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        summaryLockTime (txInfoSummary prepared) `shouldSatisfy` (\lockTime -> lockTime <= 800002 && lockTime >= 800002 - 99)

    -- The best block was mined nine hours before the test: more than the
    -- eight hours after which the wallet counts as behind the chain
    it "is not prepared while the wallet is behind the chain, unless the user agrees to a lock time of 0" $
      withChainMinedAgo ctx [] (9 * 3600) (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] ["n"]
        runErr run
          `shouldSatisfy` Text.isInfixOf
            "The wallet is behind the chain: its best block, 800002, was mined 9 hours ago, and a sync may find coins spent or received since. Prepare the transaction anyway, with a lock time of 0?"
        run `shouldSatisfy` printed "The transaction was not prepared: run sync first"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] ["y"]
        summaryLockTime (txInfoSummary prepared) `shouldBe` 0

    it "is prepared with a lock time of 0 while the wallet is behind the chain by --yes, without asking" $
      withChainMinedAgo ctx [] (9 * 3600) (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--yes"] []
        runErr run `shouldNotSatisfy` Text.isInfixOf "behind the chain"
        prepared <- onePendingTx home
        summaryLockTime (txInfoSummary prepared) `shouldBe` 0

    -- As the recipients were typed before, the amount in a word of its own
    it "is refused a recipient without an amount, with how to write one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "0.0002", "--fee-rate", "10"] [])
          `shouldReturn` Right "The recipient 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU has no amount: write it ADDRESS:AMOUNT, as in 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- 226 bytes at 20 satoshis a byte
    it "pays the fee rate of --fee for each byte" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "20"] []
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary, summarySize summary) `shouldBe` (4520, 20, 226)

    -- The fee of 2260 comes out of the 20000 the recipient gets
    it "takes the fee from the recipient with --recipients-pay" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--recipients-pay", "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 17740)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 30000)]

    -- 545 satoshis are less than the 546 nodes relay for a P2PKH output
    it "is refused a payment below the dust limit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "545sat", "--fee-rate", "10"] [])
          `shouldReturn` Right "Recipient output is smaller than the dust value"

    -- A payment of 47400 from the coin of 50000 leaves 50000 - 47400 - 2260
    -- = 340 of change, less than the 546 nodes relay: the fee is all the
    -- rest, 50000 - 47400 = 2600
    it "gives the fee a change below the dust limit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "47400sat", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.empty
        feeOf prepared `shouldBe` 2600

    -- The coins of 50000 and 30000 are both at /0/0, and either pays 20000
    -- alone. Each payment is deleted, which frees its coin for the next.
    it "spends the coins in the order of --strategy" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        forM_ [("largest-first", 50000), ("smallest-first", 30000)] $ \(strategy, spent) -> do
          prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--strategy", strategy, "--fee-rate", "10"] []
          spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, spent)]
          nosig <- nosigOf prepared
          void $ answer home ["deletetx", nosig] []

    it "is refused a --strategy it does not know, with the ones it knows" $
      withHome $ \home -> do
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--strategy", "biggest", "--fee-rate", "10"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --strategy: Invalid strategy. Select one of the following: least-waste, random-draw, largest-first, smallest-first"

    it "is refused for more than the coins of the account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.001", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

    -- The coin of the payment is of main, and the wallet has a second
    -- account. Without --account, hw asked which account to use.
    it "is from the account of the coins of --coin, without --account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        let coin = Text.unpack (txHashToHex (txHash (payment ctx))) <> ":0"
        (fmap dBAccountName . accountOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coin, "--fee-rate", "10"] [])
          `shouldReturn` Right "main"

    it "is refused an --account that does not hold the coins of --coin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        let coin = Text.unpack (txHashToHex (txHash (payment ctx))) <> ":0"
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coin, "--account", "second", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins or addresses given are not in the account second"

    it "is written to its --output file with the network of its account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "unsigned.psbt", "--fee-rate", "10"] []
        (txFileNetwork <$> readTxFile ctx (home </> "unsigned.psbt")) `shouldReturn` Just btc

    -- The path is checked first: the coins would not cover these 100000
    -- satoshis either
    it "is refused an --output file that exists, before it is built" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        writeFile (home </> "taken.json") ""
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.001", "--output", home </> "taken.json", "--fee-rate", "10"] [])
          `shouldReturn` Right (alreadyExists (home </> "taken.json"))
        readFile (home </> "taken.json") `shouldReturn` ""

{- The confirmations of the coins spent -}

-- A payment of 20000 from the coin of 50000 makes one input and two
-- outputs, 226 bytes: a fee of 2260 at 10 satoshis a byte, and 27740 of
-- change. The coin of the paid chain is in block 800000, two below the best
-- block: 3 confirmations.
minConfSpec :: Ctx -> Spec
minConfSpec ctx =
  describe "The confirmations preparetx asks of its coins with --min-conf" $ do
    it "spends a coin with as many confirmations as --min-conf asks" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--min-conf", "3", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    it "is refused when no coin has as many confirmations as --min-conf asks, and prepares nothing" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--min-conf", "4", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- Smallest first would spend the coin of 30000 alone, but it has 1
    -- confirmation
    it "chooses only among the coins with as many confirmations as --min-conf asks" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--min-conf", "2", "--strategy", "smallest-first", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    -- The payment sent spends the coin of 50000 and leaves 27740 of change
    -- in the mempool, the only coin of the account
    it "spends the unconfirmed change of the wallet's own payment without --min-conf" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- sentPayment home
        _ <- answer home ["sync"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]

    it "does not spend the unconfirmed change of the wallet's own payment with --min-conf 1" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- sentPayment home
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--min-conf", "1", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

    it "lists a coin of --coin with fewer confirmations than --min-conf asks as not spendable yet" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--coin", coinArg (newerPayment ctx) 0, "--min-conf", "2", "--fee-rate", "10"] []
        spentFromWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 50000)]
        leftOf prepared `shouldBe` Right [(OutPoint (txHash (newerPayment ctx)) 0, 30000, NotSpendableYet)]

    it "is refused a --min-conf that is not a whole number" $
      withHome $ \home -> do
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--min-conf", "1.5", "--fee-rate", "10"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --min-conf: cannot parse value `1.5'"

{- A high fee rate -}

-- | The question of preparetx and preparesweep at 101 satoshis per virtual byte
highFeeQuestion :: Text
highFeeQuestion = "The fee rate of 101 satoshis per virtual byte is above 100, the most hw takes without asking. Prepare the transaction anyway? [y/N] "

-- The payment of 20000 satoshis is 226 bytes: its fee is 22600 at 100
-- satoshis a byte, and 22826 at 101. The sweep of the coin of 30000 of the
-- old key is 192 bytes: its fee is 19392 at 101, which leaves 10608 for the
-- destination. Where nothing is typed, a question would cancel the command.
highFeeSpec :: Ctx -> Spec
highFeeSpec ctx =
  describe "A fee rate above 100 satoshis per virtual byte" $ do
    it "is not asked about at 100, the highest rate taken without asking" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "100"] []
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary) `shouldBe` (22600, 100)

    it "is asked about by preparetx on stderr" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "101"] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf highFeeQuestion

    it "is taken by preparetx when the user answers yes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "101"] ["y"]
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary) `shouldBe` (22826, 101)

    it "makes preparetx prepare nothing when the user answers anything else" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "101"] ["n"])
          `shouldReturn` Right highFeeRefused
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "is taken by preparetx without asking with --yes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "101", "--yes"] []
        summaryFeeByte (txInfoSummary prepared) `shouldBe` 101

    -- The keys would be typed for nothing
    it "is asked about by preparesweep before the private keys are typed" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "101"] ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf highFeeQuestion
        run `shouldSatisfy` printed highFeeRefused
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"
        doesFileExist (home </> "sweep.json") `shouldReturn` False

    it "is taken by preparesweep when the user answers yes" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "101"] ["y"]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 10608)]

    it "is taken by preparesweep without asking with --yes" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "101", "--yes"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 10608)]

{- Coin control -}

-- A payment of 20000 from the coin of 50000 makes one input and two
-- outputs, 226 bytes: a fee of 2260, and 27740 of change. Sending all of the
-- coin of 50000 makes 192 bytes: a fee of 1920, and 48080 for the address.
coinControlSpec :: Ctx -> Spec
coinControlSpec ctx =
  describe "Coin control" $ do
    -- The coin of 50000 at /0/0 pays 20000: a fee of 2260, and 27740 of
    -- change
    it "spends the coin --coin names by its short id" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", shortCoinArg (twoAddressesTx ctx) 0, "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    it "spends the coins at the address of --spend-from, and only them" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(internalAddr 0, 27740)]
        leftOf prepared `shouldBe` Right []

    -- Both coins at the address, 80000: two inputs and two outputs make 374
    -- bytes, a fee of 3740, and 56260 of change. Smallest first would spend
    -- the coin of 30000 alone.
    it "spends every coin at the --spend-from address, whatever --strategy says" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--strategy", "smallest-first", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 80000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 56260)]

    -- The coin of 30000 pays 20000 alone: 226 bytes, a fee of 2260, and
    -- 7740 of change. The coin of 50000 stays, and is not listed.
    it "spends only what the payment needs among them with --select, as --strategy says" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        answered <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--select", "--strategy", "smallest-first", "--fee-rate", "10"] []
        prepared <- txOf answered
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 7740)]
        leftOf answered `shouldBe` Right []

    -- The coin of 30000 is locked, so smallest first spends the one of
    -- 50000: 27740 of change. The list says why the one of 30000 was not
    -- taken.
    it "lists the chosen coins that cannot be spent with --select too, with why" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoCoinsTx ctx) 1] []
        answered <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--select", "--strategy", "smallest-first", "--fee-rate", "10"] []
        paidToWallet <$> txOf answered `shouldReturn` Map.fromList [(internalAddr 0, 27740)]
        leftOf answered `shouldBe` Right [(OutPoint (txHash (twoCoinsTx ctx)) 1, 30000, Locked LockedByUser)]

    it "refuses --select without --coin or --spend-from" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--select", "--fee-rate", "10"] [])
          `shouldReturn` Right "--select needs coins to select among: give --coin or --spend-from"

    -- 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the first receiving address of
    -- cash in the legacy form; hw stores it as
    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u. The wallet has
    -- main too: the address names the account.
    it "finds the account of a --spend-from address however it is written" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        _ <- receiveFor home "cash"
        _ <- answer home ["sync"] []
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--fee-rate", "10"] []
        (dBAccountName <$> accountOf prepared) `shouldBe` Right "cash"
        paidToOthers <$> txOf prepared `shouldReturn` Map.fromList [(otherAddr 0, 20000)]

    it "spends the coins chosen that can be spent, and lists the others with why" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 0, "--coin", coinArg (twoAddressesTx ctx) 1, "--fee-rate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(internalAddr 0, 27740)]
        leftOf prepared `shouldBe` Right [(OutPoint (txHash (twoAddressesTx ctx)) 1, 30000, Locked LockedByUser)]

    -- 30000 satoshis are 0.0003 bitcoins
    it "shows the coins chosen that cannot be spent after the transaction" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 0, "--coin", coinArg (twoAddressesTx ctx) 1, "--fee-rate", "10"] []
        let (transaction, staying) = Text.breakOn "in the account:" (runOut run)
        transaction `shouldSatisfy` Text.isInfixOf "NoSigHash"
        staying `shouldSatisfy` Text.isInfixOf ("\n    0.00030000 btc  you locked it\n  " <> Text.pack (shortCoinArg (twoAddressesTx ctx) 1) <> "\n")

    it "prepares nothing when none of the coins chosen can be spent, and tells why" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 1, "--fee-rate", "10"] [])
          `shouldReturn` Right ("None of the coins chosen can be spent now: " <> Text.pack (coinArg (twoAddressesTx ctx) 1) <> ", you locked it")
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "is refused a --spend-from address that is not in the account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (otherAddr 1), "--fee-rate", "10"] [])
          `shouldReturn` Right "The address 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ is not in the account main"

    it "is refused a --coin the account does not have" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let unknown = Text.unpack (txHashToHex (fakeTxHash 9)) <> ":0"
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", unknown, "--fee-rate", "10"] [])
          `shouldReturn` Right ("The account has no coin " <> Text.pack unknown)

    it "lists nothing left in the account without coin control" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (leftOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []) `shouldReturn` Right []

    it "makes preparesendall send the coins at --spend-from, listing the others as not chosen" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 48080)]
        leftOf sendAll `shouldBe` Right [(OutPoint (txHash (twoAddressesTx ctx)) 1, 30000, NotGiven)]

{- The change sent back -}

-- A payment of 20000 from the coin of 50000 leaves 27740 of change. One of
-- 60000 needs the coins at both addresses, 80000: two inputs and two
-- outputs make 374 bytes, a fee of 3740, and 16260 of change.
changeBackSpec :: Ctx -> Spec
changeBackSpec ctx =
  describe "The change of preparetx sent back" $ do
    let chain = confirmedChain (twoAddressesTx ctx)
        fromBoth = ["preparetx", fixedTo (otherAddr 0) "0.0006", "--change-back", "--fee-rate", "10"]
    it "goes to the one address the payment spends from, without a question" $
      withChain ctx chain $ \home _ -> do
        synced home
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--change-back", "--fee-rate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 27740)]

    it "goes to the address whose path the user types, among those the payment spends from" $
      withChain ctx chain $ \home _ -> do
        synced home
        prepared <- answer home fromBoth ["/0/1"]
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 1, 16260)]

    it "goes to the address with the largest total when the user types nothing" $
      withChain ctx chain $ \home _ -> do
        synced home
        prepared <- answer home fromBoth [""]
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 16260)]

    it "is asked for with the addresses listed as balances lists them, the largest first" $
      withChain ctx chain $ \home _ -> do
        synced home
        run <- hw home fromBoth ["/0/1"]
        runErr run `shouldSatisfy` Text.isInfixOf "The payment spends coins at 2 addresses. Its change can go back to any of them:\n/0/0  "
        runErr run `shouldSatisfy` Text.isInfixOf ("/0/0  " <> externalAddrsText !! 0 <> "\n  total 0.00050000 btc")
        runErr run `shouldSatisfy` Text.isInfixOf ("/0/1  " <> externalAddrsText !! 1 <> "\n  total 0.00030000 btc")
        runErr run `shouldSatisfy` Text.isInfixOf "Which address gets the change? Type its path [/0/0]"

    it "goes back without --change-back when the configuration file says so" $
      withChain ctx chain $ \home _ -> do
        synced home
        setChangeBack home True
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 27740)]

    it "goes to a new change address with --new-change, whatever the configuration file says" $
      withChain ctx chain $ \home _ -> do
        synced home
        setChangeBack home True
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--new-change", "--fee-rate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(internalAddr 0, 27740)]

    it "cannot go back and to a new change address at once" $
      withHome $ \home -> do
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", "--new-change", "--fee-rate", "10"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid option `--new-change'"
