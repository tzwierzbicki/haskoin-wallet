{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A payment as the user prepares it with preparetx, after a sync: the
-- fee rates recent blocks paid, the coins received and locked, the amount
-- and the fee, the confirmations asked of the coins, a fee rate too high,
-- coin control, the bip44 and the bip84 coins of a portfolio spent
-- together, the type of the change, and the change sent back to an
-- address of the portfolio.
module Haskoin.Wallet.Workflow.PaymentSpec (spec) where

import Control.Monad (forM_, void)
import qualified Data.Aeson as Json
import Data.IORef (modifyIORef, readIORef)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBRawTx, runDBWith)
import Haskoin.Wallet.Database.Coin (CoinLock (..), LeftReason (..))
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Store (PublishFailure (..))
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
  bothTypesSpec ctx
  changeTypeSpec ctx
  changeBackSpec ctx
  bitcoinCashSpec ctx

{- Fee rates -}

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
        newTestPortfolio home
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

-- | A payment of 50000 satoshis from another wallet to the first
-- receiving address of the bip44 account of portfolio1 of the test wallet
-- on btc, 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo
portfolio1Payment :: Ctx -> Tx
portfolio1Payment ctx = fundingTx ctx 1 [(account1Addr, 50000)]

receivingSpec :: Ctx -> Spec
receivingSpec ctx = do
  describe "A payment to the wallet" $ do
    it "is found by sync, which tells what the portfolio can spend on the network" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 50000)]
        (spendableOn btc <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, 50000)]

    -- 50000 satoshis are 0.0005 bitcoins and 500 bits
    it "is shown by portfolios in bitcoins, or in the unit of --unit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (printed "0.00050000 btc" <$> hw home ["portfolios"] []) `shouldReturn` True
        (printed "50000 sats" <$> hw home ["portfolios", "--unit", "sat"] []) `shouldReturn` True
        (printed "500.00 bits" <$> hw home ["portfolios", "--unit", "bit"] []) `shouldReturn` True

    -- The help says the two options go before or after the command
    it "is shown in the unit of --unit given before the command" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (printed "50000 sats" <$> hw home ["--unit", "sat", "portfolios"] []) `shouldReturn` True

    it "is shown in the unit of the configuration file when --unit is not given" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setUnit home "sat"
        (printed "50000 sats" <$> hw home ["portfolios"] []) `shouldReturn` True

    -- 50000 satoshis are 500 bits
    it "is shown in the unit of --unit, even when the configuration file names another" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setUnit home "sat"
        (printed "500.00 bits" <$> hw home ["portfolios", "--unit", "bit"] []) `shouldReturn` True

    -- A script that reads the JSON answers is not affected by the unit
    it "is answered in JSON in satoshis, whatever the unit of the configuration file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setUnit home "bit"
        (spendableOn btc <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, 50000)]

    it "is answered in JSON with --json given before the command" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["--json", "portfolios"] []
        (spendableOn btc <$> Json.eitherDecodeStrict (encodeUtf8 (runOut run))) `shouldBe` Right (Right [(mainPortfolioRef, 50000)])

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
        (spendableOn btc <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, 0)]
        modifyIORef chain mine
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 50000)]
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 1, Nothing, True)]

  -- Only this wallet can replace the payment, which spends the coin of
  -- portfolio0. At 10 satoshis a byte, its 226 bytes cost 2260, so
  -- portfolio0 keeps 50000 - 30000 - 2260 = 17740 of change.
  describe "A payment from another portfolio of the wallet" $
    it "can be spent while in the mempool, like the change of the portfolio that paid it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        syncedTwoPortfolios home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo account1Addr "0.0003", "--fee-rate", "10"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["broadcast", nosig, "--yes"] []
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(PortfolioRef 0 0, 17740), (PortfolioRef 0 1, 30000)]
        switchTo home 0 1
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(account1Addr, 30000)]

  describe "A payment from the active portfolio" $ do
    -- portfolio0 holds the coin of 50000 the chain pays, and portfolio1
    -- holds nothing. portfolio0, the first portfolio created, is active.
    it "is from portfolio0, the first portfolio created, in a wallet of several portfolios" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        newTestPortfolio home
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right mainPlace
        spentFromWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 50000)]

    -- The chain pays portfolio1 alone
    it "is from the portfolio switch made active" $
      withChain ctx (confirmedChain (portfolio1Payment ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right portfolio1Place
        spentFromWallet <$> txOf prepared `shouldReturn` Map.fromList [(account1Addr, 50000)]

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

    -- The coin is of portfolio0, at its first receiving address, and
    -- portfolio1 is active
    it "is shown by lockcoin with its account, which says it is not of the active portfolio" $
      withChain ctx chain $ \home _ -> do
        synced home
        newTestPortfolio home
        switchTo home 0 1
        run <- hw home ("lockcoin" : coinArgs) []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Account : portfolio0, not the active portfolio1  btc  bip44\n"

    it "is not spent by preparetx" $
      withChain ctx chain $ \home _ -> do
        synced home
        _ <- answer home ("lockcoin" : coinArgs) []
        -- The refusal says what the portfolio holds and cannot spend: the
        -- coin of 50000 satoshis, which is locked
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee. The portfolio holds 0.00050000 btc more on btc, in 1 coin it cannot spend now: hw coins says why"

  describe "A sync" $ do
    -- The store reports every address used, as one that lies does: after
    -- a thousand on the receiving chain of the first account the sync
    -- looks at, sync asks, and a no leaves the portfolio as it was,
    -- without an address
    it "asks whether to keep looking when the store reports used addresses a thousand deep, and generates none of them when the answer is no" $
      withLyingChain ctx (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hw home ["sync"] ["n"]
        run `shouldSatisfy` printed "The store reports used addresses as far as 1000 on the receiving chain of the bip44 account of portfolio0 on btc, and more beyond. Keep looking? [y/N] "
        run `shouldSatisfy` printed "The scan stopped there."
        runExit run `shouldBe` ExitFailure 1
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

    it "stops at the question when the input has ended, as a sync nobody watches does" $
      withLyingChain ctx (paidChain ctx) $ \home _ -> do
        newTestPortfolio home
        run <- hw home ["sync"] []
        run `shouldSatisfy` printed "Keep looking? [y/N] "
        run `shouldSatisfy` printed "Cancelled: the input ended before the answer"
        runExit run `shouldBe` ExitFailure 1

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

    -- The payment was sent, and the network forgot it: its node dropped
    -- it, and the store no longer takes it. A sync hands the store every
    -- payment the network lacks.
    describe "that hands the store a sent payment the network forgot, which the store refuses" $ do
      let refusal = NotPublished "bad-txns-inputs-missingorspent"
          forgotten home chain = do
            txid <- sentPayment home
            modifyIORef chain $ \now -> now {chainTxs = [entry | entry@(_, Just _) <- now.chainTxs]}
            return txid

      it "syncs the network all the same" $
        withChainNotPublishingAgain ctx refusal (paidChain ctx) $ \home chain -> do
          _ <- forgotten home chain
          (notSyncedOf <$> hwJson home ["sync"] []) `shouldReturn` Right []

      it "names the payment that was not sent again" $
        withChainNotPublishingAgain ctx refusal (paidChain ctx) $ \home chain -> do
          txid <- forgotten home chain
          (fmap (map fst) . notPublishedOf <$> hwJson home ["sync"] []) `shouldReturn` Right [txid]

      it "gives what the store said of the payment" $
        withChainNotPublishingAgain ctx refusal (paidChain ctx) $ \home chain -> do
          _ <- forgotten home chain
          reasons <- fmap (map snd) . notPublishedOf <$> hwJson home ["sync"] []
          reasons `shouldSatisfy` either (const False) (all (" refused the transaction: bad-txns-inputs-missingorspent" `isSuffixOf`))

      -- The coin of 50000 is still held by the payment, which the wallet
      -- keeps: only the user knows whether to give it up
      it "keeps the payment pending, and its coin held, until the user deletes it" $
        withChainNotPublishingAgain ctx refusal (paidChain ctx) $ \home chain -> do
          _ <- forgotten home chain
          (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 0)]

      it "says how to delete the payment, to free its coin" $
        withChainNotPublishingAgain ctx refusal (paidChain ctx) $ \home chain -> do
          txid <- forgotten home chain
          run <- hw home ["sync"] []
          run `shouldSatisfy` printedSentence "Next, delete the pending transaction that was not sent again, to free its coins"
          run `shouldSatisfy` printed (Text.pack ("hw deletetx " <> shortOf txid))

      -- 50000 received, and nothing spent
      it "gives the coin back once the user deletes the payment" $
        withChainNotPublishingAgain ctx refusal (paidChain ctx) $ \home chain -> do
          txid <- forgotten home chain
          _ <- answer home ["sync"] []
          _ <- answer home ["deletetx", txid] []
          (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 50000)]

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

    -- The answer of preparetx is the transaction as the wallet holds it
    -- from then on
    it "has the time it was prepared, the one pendingtxs gives from then on" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        held <- onePendingTx home
        createdOf prepared `shouldSatisfy` isJust
        createdOf prepared `shouldBe` createdOf held

    it "shows the time it was prepared, not that its time is unknown" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run `shouldNotSatisfy` printed "time unknown"

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
          `shouldReturn` Right "A recipient would get 545 satoshis, less than the dust limit of 546 satoshis for its kind of address. Nodes do not relay an output that small"

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

    it "is refused for more than the coins of the portfolio" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.001", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

    -- The coin is of portfolio1, which switch made active
    it "is from the active portfolio with a --coin of it" $
      withChain ctx (confirmedChain (portfolio1Payment ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        (placeOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (portfolio1Payment ctx) 0, "--fee-rate", "10"] [])
          `shouldReturn` Right portfolio1Place

    -- The coin is of portfolio1, and portfolio0 is active: a payment from
    -- another portfolio than the one the user works in would surprise.
    -- The coin is named by its short id.
    it "is refused a --coin of another portfolio than the active one, naming the portfolio that holds it and the switch to it" $
      withChain ctx (confirmedChain (portfolio1Payment ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (portfolio1Payment ctx) 0, "--fee-rate", "10"] [])
          `shouldReturn` Right ("The coin " <> Text.pack (shortCoinArg (portfolio1Payment ctx) 0) <> " is in portfolio1 of wallet0, not in the active portfolio: run switch 1 --wallet 0 first")
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "is refused a --spend-from address of another portfolio than the active one, naming the portfolio that holds it and the switch to it" $
      withChain ctx (confirmedChain (portfolio1Payment ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf account1Addr, "--fee-rate", "10"] [])
          `shouldReturn` Right "The address 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is in portfolio1 of wallet0, not in the active portfolio: run switch 1 --wallet 0 first"

    -- The coin of the payment is on btc
    it "is refused a --network that holds none of the coins of --coin, naming the network they are on" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--network", "bch", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins or addresses given are on btc, not on the --network bch"

    it "is written to its --output file with the network of its portfolio" $
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
        -- The refusal says what the portfolio holds and cannot spend: the
        -- coin of 50000 satoshis, which has fewer confirmations
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--min-conf", "4", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee. The portfolio holds 0.00050000 btc more on btc, in 1 coin it cannot spend now: hw coins says why"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- Smallest first would spend the coin of 30000 alone, but it has 1
    -- confirmation
    it "chooses only among the coins with as many confirmations as --min-conf asks" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--min-conf", "2", "--strategy", "smallest-first", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    -- The payment sent spends the coin of 50000 and leaves 27740 of change
    -- in the mempool, the only coin of the portfolio
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
        -- The refusal says what the portfolio holds and cannot spend: the
        -- unconfirmed change, 27740 satoshis
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--min-conf", "1", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee. The portfolio holds 0.00027740 btc more on btc, in 1 coin it cannot spend now: hw coins says why"

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
        newTestPortfolio home
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "101"] ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf highFeeQuestion
        run `shouldSatisfy` printed highFeeRefused
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"
        doesFileExist (home </> "sweep.json") `shouldReturn` False

    it "is taken by preparesweep when the user answers yes" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "101"] ["y"]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 10608)]

    it "is taken by preparesweep without asking with --yes" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
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

    -- The choosers add up. --coin names the coin of 30000 at /0/1, and
    -- --spend-from the address /0/0, which holds the coin of 50000. Either
    -- coin would cover 20000, yet both are spent.
    it "spends the coins of --coin and those at the --spend-from address together" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", shortCoinArg (twoAddressesTx ctx) 1, "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000), (externalAddr 1, 30000)]

    -- A filter narrows the coins chosen, and adds none. Both coins are
    -- bip44, and --spend-from names /0/1, which holds the coin of 30000.
    -- The coin of 50000 at /0/0 is of the type --coin-type keeps, but it
    -- was not chosen.
    it "spends only the coins at the --spend-from address under --coin-type of their type, not every coin of that type" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin-type", "bip44", "--spend-from", textOf (externalAddr 1), "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 1, 30000)]

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
    -- the bip44 account of portfolio0 on bch in the legacy form; hw stores
    -- it as bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u. The
    -- default network is btc: the address names the network.
    it "finds the portfolio and the network of a --spend-from address however it is written" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right (Place mainPortfolioRef bch)
        paidToOthers <$> txOf prepared `shouldReturn` Map.fromList [(otherAddr 0, 20000)]

    it "spends the coins chosen that can be spent, and lists the others with why" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 0, "--coin", coinArg (twoAddressesTx ctx) 1, "--fee-rate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(internalAddr 0, 27740)]
        leftOf prepared `shouldBe` Right [(OutPoint (txHash (twoAddressesTx ctx)) 1, 30000, Locked LockedByUser)]

    -- 30000 satoshis are 0.0003 bitcoins. The coin is 3 confirmations
    -- deep, its transaction seen at 1700000000, 23:13 in Central Europe.
    it "shows the coins chosen that cannot be spent after the transaction, each as coins shows it, then why it stays" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        run <- hwWithEnv [("TZ", centralEurope)] home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 0, "--coin", coinArg (twoAddressesTx ctx) 1, "--fee-rate", "10"] []
        let (transaction, staying) = Text.breakOn "in the portfolio:" (runOut run)
        transaction `shouldSatisfy` Text.isInfixOf "NoSigHash"
        staying
          `shouldSatisfy` Text.isInfixOf
            ( "\n" <> Text.pack (shortCoinArg (twoAddressesTx ctx) 1) <> "  14 Nov 2023 23:13      0.00030000 btc  locked by you, 3 confirmations\n"
                <> "  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  bip44/0/1\n"
                <> "  you locked it\n"
            )

    it "prepares nothing when none of the coins chosen can be spent, and tells why" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 1, "--fee-rate", "10"] [])
          `shouldReturn` Right ("None of the coins chosen can be spent now: " <> Text.pack (coinArg (twoAddressesTx ctx) 1) <> ", you locked it")
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "is refused a --spend-from address that is not in the portfolio" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (otherAddr 1), "--fee-rate", "10"] [])
          `shouldReturn` Right "The address 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ is not in portfolio0 of wallet0 on btc"

    it "is refused a --coin the portfolio does not have" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let unknown = Text.unpack (txHashToHex (fakeTxHash 9)) <> ":0"
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", unknown, "--fee-rate", "10"] [])
          `shouldReturn` Right ("The portfolio has no coin " <> Text.pack unknown <> " on the network of the payment")

    it "lists nothing left in the portfolio without coin control" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (leftOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []) `shouldReturn` Right []

    it "makes preparesendall send the coins at --spend-from, listing the others as not chosen" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 48080)]
        leftOf sendAll `shouldBe` Right [(OutPoint (txHash (twoAddressesTx ctx)) 1, 30000, NotGiven)]

    -- bip44/0/0 is 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, which holds the coin
    -- of 50000; the coin of 30000 is at bip44/0/1
    it "spends the coins at the address at a --spend-from path, and only them" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", "bip44/0/0", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    -- The payment sent left 50000 - 20000 - 2260 = 27740 at the change
    -- address bip44/1/0, mined since
    it "spends the coins at the change address at a --spend-from path" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--spend-from", "bip44/1/0", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]

    -- The portfolio has an address at bip44/0/0 on bch alone, and the
    -- default network is btc
    it "pays on the only network where the portfolio has an address at the --spend-from path" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        _ <- answer home ["sync"] []
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", "bip44/0/0", "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right (Place mainPortfolioRef bch)

    -- The portfolio has an address at bip44/0/0 on btc and on btctest
    it "pays on the default network when the portfolio has an address at the --spend-from path on several, the default one among them" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["receive", "--network", "btctest"] []
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", "bip44/0/0", "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right (Place mainPortfolioRef btc)

    it "is refused a --spend-from path the portfolio has no address at on the network of the payment" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", "bip44/0/5", "--fee-rate", "10"] [])
          `shouldReturn` Right "portfolio0 of wallet0 on btc has no address at bip44/0/5"

    -- The coin of 50000 at bip44/0/0, one input and one output: 192 bytes,
    -- a fee of 1920, and 50000 - 1920 = 48080 sent
    it "makes preparesendall send the coins at a --spend-from path" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--spend-from", "bip44/0/0", "--fee-rate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 48080)]

{- The coins of both types of a portfolio -}

-- The coins of 'bothTypesTx': 50000 at the first receiving address of the
-- bip44 account of portfolio0 on btc, a P2PKH input of 148 bytes, and
-- 30000 at the first receiving address of its bip84 account, a P2WPKH
-- input of 68 virtual bytes. A transaction with a SegWit input has 10.5
-- virtual bytes of its own, where one without has 10. A P2PKH output, to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU or to bip44 change, is 34 bytes, and
-- a P2WPKH output, to bip84 change, 31.
--
-- A payment of 60000 needs both coins, 80000. With bip44 change: 10.5 +
-- 148 + 68 + 34 + 34 = 294.5, so 295 virtual bytes, a fee of 2950 at 10
-- satoshis a virtual byte, and 80000 - 60000 - 2950 = 17050 of change.
bothTypesSpec :: Ctx -> Spec
bothTypesSpec ctx =
  describe "A payment from the bip44 and the bip84 coins of a portfolio" $ do
    it "spends coins of both accounts in one transaction" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000), (segwitExternalAddr 0, 30000)]

    it "pays the fee of each input at its own size, the bip84 one less than the bip44 one" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        let summary = txInfoSummary prepared
        (summaryFee summary, summarySize summary) `shouldBe` (2950, 295)
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 17050)]

    -- The inputs are compared sorted: their order is drawn at random
    it "is signed with the mnemonic typed once, for both accounts, and sent" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home chain -> do
        syncedBothTypes home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        _ <- answer home ["broadcast", nosig, "--yes"] []
        published <- inMempool <$> readIORef chain
        ((\(spent, paid) -> (sort spent, paid)) . spendsAndPays ctx <$> published)
          `shouldBe` [(sort [OutPoint (txHash (bothTypesTx ctx)) 0, OutPoint (txHash (bothTypesTx ctx)) 1], [(17050, Right (internalAddr 0)), (60000, Right (otherAddr 0))])]

    it "leaves the change once mined, which the portfolio can spend" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home chain -> do
        syncedBothTypes home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["broadcast", nosig, "--yes"] []
        modifyIORef chain mine
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 17050)]

    -- 10.5 + 68 + 34 + 31 = 143.5, so 144 virtual bytes, a fee of 1440, and
    -- 30000 - 20000 - 1440 = 8560 of change, bip84 as the coin
    it "spends the bip84 coins alone with --coin-type bip84" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin-type", "bip84", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(segwitExternalAddr 0, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(segwitInternalAddr 0, 8560)]

    -- 10 + 148 + 34 + 34 = 226 bytes, a fee of 2260, and 50000 - 20000 -
    -- 2260 = 27740 of change, bip44 as the coin
    it "spends the bip44 coins alone with --coin-type bip44" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin-type", "bip44", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]

    -- The bip84 coin holds 30000: both coins would cover 40000
    it "is refused with --coin-type when the coins of that type do not cover it" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--coin-type", "bip84", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

    it "refuses a coin of the other type chosen with --coin under --coin-type, naming it" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (bothTypesTx ctx) 0, "--coin-type", "bip84", "--fee-rate", "10"] [])
          `shouldReturn` Right ("The coin " <> Text.pack (shortCoinArg (bothTypesTx ctx) 0) <> " is a bip44 coin, and --coin-type bip84 spends bip84 coins only")

{- The type of the change -}

-- | A coin of 50000 satoshis at the first receiving address of the bip84
-- account of portfolio0 on btc, bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc
segwitPayment :: Ctx -> Tx
segwitPayment ctx = fundingTx ctx 11 [(segwitExternalAddr 0, 50000)]

-- The rule of the specification: the type --change-type names; else the
-- type of the coins spent, when they are all of one type; else, for coins
-- of both types, the default-type of the configuration file; else bip44.
-- The sizes are those above.
changeTypeSpec :: Ctx -> Spec
changeTypeSpec ctx =
  describe "The type of the change of preparetx" $ do
    -- 10 + 148 + 34 + 31 = 223 bytes, a fee of 2230, and 50000 - 20000 -
    -- 2230 = 27770 of change
    it "is the type --change-type names, bip84 for a payment of bip44 coins" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-type", "bip84", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(segwitInternalAddr 0, 27770)]

    -- 10.5 + 68 + 34 + 34 = 146.5, so 147 virtual bytes, a fee of 1470,
    -- and 50000 - 20000 - 1470 = 28530 of change
    it "is the type --change-type names, bip44 for a payment of bip84 coins" $
      withChain ctx (confirmedChain (segwitPayment ctx)) $ \home _ -> do
        syncedBothTypes home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-type", "bip44", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 28530)]

    -- 10.5 + 68 + 34 + 31 = 143.5, so 144 virtual bytes, a fee of 1440, and
    -- 50000 - 20000 - 1440 = 28560 of change
    it "is bip84 for a payment of bip84 coins alone" $
      withChain ctx (confirmedChain (segwitPayment ctx)) $ \home _ -> do
        syncedBothTypes home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(segwitInternalAddr 0, 28560)]

    it "is the type of the coins spent, bip44, though the default-type of the configuration file is bip84" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        setConfigKey home "default-type" (Json.String "bip84")
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]

    -- Smallest first spends the coin of 30000 alone, a bip84 coin: 144
    -- virtual bytes, a fee of 1440, and 30000 - 20000 - 1440 = 8560 of
    -- change. Largest first spends the coin of 50000 alone, a bip44 coin:
    -- 226 bytes, a fee of 2260, and 27740 of change. Each payment is
    -- deleted, which frees its coin for the next.
    it "is the type of the coins the selection spends, among coins of both types" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        forM_ [("smallest-first", segwitInternalAddr 0, 8560), ("largest-first", internalAddr 0, 27740)] $ \(strategy, change, amount) -> do
          prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--strategy", strategy, "--fee-rate", "10"] []
          paidToWallet prepared `shouldBe` Map.fromList [(change, amount)]
          nosig <- nosigOf prepared
          void $ answer home ["deletetx", nosig] []

    -- 10.5 + 148 + 68 + 34 + 31 = 291.5, so 292 virtual bytes, a fee of
    -- 2920, and 80000 - 60000 - 2920 = 17080 of change
    it "is the default-type of the configuration file for coins of both types, bip84" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        setConfigKey home "default-type" (Json.String "bip84")
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(segwitInternalAddr 0, 17080)]

    -- 295 virtual bytes, a fee of 2950, and 17050 of change, as above
    it "is bip44 for coins of both types when the configuration file names no default-type" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 17050)]

{- The change sent back -}

-- A payment of 20000 from the coin of 50000 makes one input and two P2PKH
-- outputs, 226 bytes: a fee of 2260 at 10 satoshis a byte, and 50000 -
-- 20000 - 2260 = 27740 of change. With a P2WPKH output for the change, 10
-- + 148 + 34 + 31 = 223 bytes: a fee of 2230, and 27770 of change.
changeBackSpec :: Ctx -> Spec
changeBackSpec ctx = do
  describe "The change of preparetx sent back with --change-back" $ do
    it "goes to the address it names, one the payment spends from" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--change-back", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 27740)]

    -- receive hands out /0/1 after the /0/0 the payment spends from
    it "goes to the receiving address of the portfolio it names, which the payment does not spend from" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        second <- receiveWith home []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", second, "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 1, 27740)]

    it "goes to a bip84 address of the portfolio it names, with the size of that type" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        segwit <- receiveWith home ["--type", "bip84"]
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", segwit, "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(segwitExternalAddr 0, 27770)]
        summarySize (txInfoSummary prepared) `shouldBe` 223

    -- The payment sent left 27740 at the change address /1/0, mined since.
    -- Paying 10000 of it: 226 bytes, a fee of 2260, and 27740 - 10000 -
    -- 2260 = 15480 of change.
    it "goes to a used change address it names" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--change-back", textOf (internalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 15480)]

    -- The first payment, from the coin at /0/0, takes the change address
    -- /1/0. The second spends the coin of 30000 at /0/1: 30000 - 10000 -
    -- 2260 = 17740 of change.
    it "goes to the change address of another pending transaction it names" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--spend-from", textOf (externalAddr 1), "--change-back", textOf (internalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 17740)]

    it "is refused an address outside the wallet, and nothing is prepared" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", textOf (otherAddr 1), "--fee-rate", "10"] [])
          `shouldReturn` Right "The change can only go back to an address of portfolio0 of wallet0 on btc, and 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ is not one"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is the first receiving address of
    -- portfolio1, which receive handed out
    it "is refused an address of another portfolio of the wallet" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        syncedTwoPortfolios home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", textOf account1Addr, "--fee-rate", "10"] [])
          `shouldReturn` Right "The change can only go back to an address of portfolio0 of wallet0 on btc, and 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is not one"

    -- 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the first receiving address of
    -- portfolio0 on bch, in the legacy form, which btc reads too
    it "is refused an address of the portfolio on another network than the payment's" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["receive", "--network", "bch"] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--fee-rate", "10"] [])
          `shouldReturn` Right "The change can only go back to an address of portfolio0 of wallet0 on btc, and 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is not one"

    it "is refused with --change-type, since the address has a type" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", textOf (externalAddr 0), "--change-type", "bip84", "--fee-rate", "10"] [])
          `shouldReturn` Right "--change-back ADDRESS names the address of the change, which has a type: it takes no --change-type. --change-back largest does."

    -- The coins of 50000 at /0/0 and 30000 at /0/1 pay 60000 together: 374
    -- bytes, a fee of 3740, and 80000 - 60000 - 3740 = 16260 of change
    it "goes back to the address of the largest coin the payment spends, with largest" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--change-back", "largest", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 16260)]

    -- The payment sent left 27740 at the change address /1/0, mined since,
    -- and it is the only coin. Paying 10000 of it: 27740 - 10000 - 2260 =
    -- 15480 of change.
    it "goes back with largest to the used change address of the coin spent" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--change-back", "largest", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 15480)]

    -- The bip44 coin of 50000 and the bip84 coin of 30000 pay 60000
    -- together. With a P2WPKH change output: 292 virtual bytes, a fee of
    -- 2920, and 80000 - 60000 - 2920 = 17080 of change.
    it "goes back with largest to the largest coin of the type --change-type asks for, though a coin of the other type is larger" $
      withChain ctx (confirmedChain (bothTypesTx ctx)) $ \home _ -> do
        syncedBothTypes home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--change-back", "largest", "--change-type", "bip84", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(segwitExternalAddr 0, 17080)]

    -- The paid chain holds one bip44 coin
    it "is refused with largest when the payment spends no coin of the type --change-type asks for, and nothing is prepared" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", "largest", "--change-type", "bip84", "--fee-rate", "10"] [])
          `shouldReturn` Right "The payment spends no bip84 coin, so --change-back largest has no address of the type --change-type asks for. Add --coin-type bip84 to spend only bip84 coins, or leave out --change-type."
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- receive hands out bip44/0/1, 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg
    it "goes to the receiving address at the path it names" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", "bip44/0/1", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 1, 27740)]

    -- The payment sent left 27740 at the change address bip44/1/0, mined
    -- since. Paying 10000 of it: 27740 - 10000 - 2260 = 15480 of change.
    it "goes to the used change address at the path it names" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--change-back", "bip44/1/0", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 15480)]

    -- The bip84 account took no change address yet
    it "is refused a path the portfolio has no address at on the network of the payment" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--change-back", "bip84/1/0", "--fee-rate", "10"] [])
          `shouldReturn` Right "The change can only go back to an address of portfolio0 of wallet0 on btc, and it has none at bip84/1/0"

  -- Without --change-back, the change goes to a new change address: one
  -- no pending transaction pays, and that was never used
  describe "The change address preparetx takes" $ do
    it "is not the change address of another pending transaction" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--spend-from", textOf (externalAddr 1), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 1, 17740)]

    it "is not a change address used before" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 1, 15480)]

{- Bitcoin Cash -}

-- The payment of 20000 from the coin of 50000 of 'cashCoinTx': 226 bytes,
-- a fee of 2260 at 10 satoshis a byte, and 27740 of change
bitcoinCashSpec :: Ctx -> Spec
bitcoinCashSpec ctx =
  describe "A payment on Bitcoin Cash" $ do
    let cashSynced home = do
          newTestPortfolio home
          _ <- answer home ["receive", "--network", "bch"] []
          void $ answer home ["sync"] []
    it "sends its change to the bip44 account of the portfolio there" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashSynced home
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--network", "bch", "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right (Place mainPortfolioRef bch)
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(cashChangeAddr, 27740)]

    it "sends its change to a bip44 address, though the default-type of the configuration file is bip84" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashSynced home
        setConfigKey home "default-type" (Json.String "bip84")
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--network", "bch", "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(cashChangeAddr, 27740)]

    it "refuses bip84 for its change or its coins, which Bitcoin Cash does not have" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashSynced home
        forM_ ["--change-type", "--coin-type"] $ \option ->
          (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--network", "bch", option, "bip84", "--fee-rate", "10"] [])
            `shouldReturn` Right "A bip84 account needs SegWit, which bch does not have"
