{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A payment made with send, the one command that prepares a payment,
-- signs it and sends it.
--
-- send must do what preparetx, signtx and broadcast do when they are run
-- one after the other with the same options. So the payments of this spec
-- are made both ways, and each expectation, written out, is held against
-- both ('payments'): the two ways agree on every rule tested here, and
-- neither is the measure of the other.
--
-- Then what send alone does: the transaction shown once, before the
-- mnemonic, which is the confirmation; what a payment that was not signed
-- leaves in the wallet, which is nothing; and what one that was signed and
-- not sent leaves, which is what signtx leaves.
module Haskoin.Wallet.Workflow.SendSpec (spec) where

import Control.Monad (forM_, void)
import Data.Char (isHexDigit)
import Data.IORef (modifyIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Database.Coin (CoinLock (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Store (PublishFailure (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import System.Exit (ExitCode (..))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  bothWaysSpec ctx
  screenSpec ctx
  notSignedSpec ctx
  notSentSpec ctx

{- The same payment, both ways -}

-- | A payment the tests make both ways
data Payment = Payment
  { -- | What the payment shows, in the words of its test
    payRule :: String,
    -- | The chain the store serves
    payChain :: Chain,
    -- | How the home gets its portfolio and its coins
    paySetup :: FilePath -> IO (),
    -- | The options of the payment, as preparetx and send both take them
    payOptions :: [String],
    -- | What the user types for the questions of those options, before the
    -- mnemonic
    payTyped :: [Text],
    -- | What the network must be handed: the coins the transaction spends,
    -- sorted, and what it pays, by amount
    payHanded :: ([OutPoint], [(Word64, Either String Address)])
  }

-- | The payments made both ways, one for each kind of option of
-- preparetx. Every payment is of 20000 satoshis to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU at 10 satoshis a byte, unless its
-- comment says otherwise.
--
-- The sizes are those of the network: 10 bytes for a transaction, 148 for
-- a legacy input and 34 for a legacy output. One coin that pays one
-- recipient and legacy change makes 10 + 148 + 34 + 34 = 226 bytes, so a
-- fee of 2260. A transaction with a native SegWit input has 10.5 virtual
-- bytes of its own, such an input 68, and a native SegWit output 31. The
-- sum is rounded up.
payments :: Ctx -> [Payment]
payments ctx =
  [ -- The coin of 50000 pays 20000 and 2260 of fee: 27740 of change
    Payment
      { payRule = "pays the recipient its amount, and the change to a new change address",
        payChain = paidChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--fee-rate", "10"],
        payTyped = [],
        payHanded = paymentSent ctx
      },
    -- One input and three outputs: 10 + 148 + 3 × 34 = 260 bytes, a fee
    -- of 2600, and 50000 - 20000 - 10000 - 2600 = 17400 of change
    Payment
      { payRule = "pays each of several recipients its amount",
        payChain = paidChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(10000, Right (otherAddr 1)), (17400, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- 226 bytes × 20 = 4520 of fee, and 50000 - 20000 - 4520 = 25480 of
    -- change
    Payment
      { payRule = "pays the fee rate of --fee-rate for each byte",
        payChain = paidChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--fee-rate", "20"],
        payTyped = [],
        payHanded = ([paidCoin], [(20000, Right (otherAddr 0)), (25480, Right (internalAddr 0))])
      },
    -- The median rate of the chain of fees is 12: 226 × 12 = 2712 of fee,
    -- and 50000 - 20000 - 2712 = 27288 of change. An empty line takes the
    -- median.
    Payment
      { payRule = "pays the median rate of recent blocks when nothing is typed at the question of the fee rate",
        payChain = feesChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002"],
        payTyped = [""],
        payHanded = ([paidCoin], [(20000, Right (otherAddr 0)), (27288, Right (internalAddr 0))])
      },
    -- 226 × 101 = 22826 of fee, and 50000 - 20000 - 22826 = 7174 of change
    Payment
      { payRule = "pays a fee rate above 100 once the user answers yes",
        payChain = paidChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--fee-rate", "101"],
        payTyped = ["y"],
        payHanded = ([paidCoin], [(7174, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    Payment
      { payRule = "pays a fee rate above 100 without a question, with --yes",
        payChain = paidChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--fee-rate", "101", "--yes"],
        payTyped = [],
        payHanded = ([paidCoin], [(7174, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- The fee of 2260 comes out of the 20000: the recipient gets 17740,
    -- and the change is 50000 - 20000 = 30000
    Payment
      { payRule = "takes the fee from the recipient with --recipients-pay",
        payChain = paidChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--recipients-pay", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(17740, Right (otherAddr 0)), (30000, Right (internalAddr 0))])
      },
    -- Coins of 50000 and 30000, and either pays 20000 alone. Smallest
    -- first spends the one of 30000, output 1: 30000 - 20000 - 2260 = 7740
    -- of change.
    Payment
      { payRule = "spends the coins --strategy selects",
        payChain = confirmedChain (twoCoinsTx ctx),
        paySetup = synced,
        payOptions = [recipient "0.0002", "--strategy", "smallest-first", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf (twoCoinsTx ctx) 1], [(7740, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- The coin of 30000 has 1 confirmation and the one of 50000 has 3.
    -- Smallest first would spend the one of 30000, which --min-conf 2
    -- leaves out.
    Payment
      { payRule = "spends only the coins with the confirmations --min-conf asks",
        payChain = olderAndNewerChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--min-conf", "2", "--strategy", "smallest-first", "--fee-rate", "10"],
        payTyped = [],
        payHanded = paymentSent ctx
      },
    -- The coin of 30000 at /0/1, output 1, named by its short id
    Payment
      { payRule = "spends the coin --coin names",
        payChain = confirmedChain (twoAddressesTx ctx),
        paySetup = synced,
        payOptions = [recipient "0.0002", "--coin", shortCoinArg (twoAddressesTx ctx) 1, "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf (twoAddressesTx ctx) 1], [(7740, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- Both coins at the address, 80000: two inputs and two outputs make
    -- 10 + 2 × 148 + 2 × 34 = 374 bytes, a fee of 3740, and 80000 - 20000
    -- - 3740 = 56260 of change
    Payment
      { payRule = "spends every coin at the --spend-from address",
        payChain = confirmedChain (twoCoinsTx ctx),
        paySetup = synced,
        payOptions = [recipient "0.0002", "--spend-from", textOf (externalAddr 0), "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf (twoCoinsTx ctx) 0, coinOf (twoCoinsTx ctx) 1], [(20000, Right (otherAddr 0)), (56260, Right (internalAddr 0))])
      },
    -- Of the two coins at the address, smallest first needs the one of
    -- 30000 alone
    Payment
      { payRule = "spends only what the payment needs among the coins chosen, with --select",
        payChain = confirmedChain (twoCoinsTx ctx),
        paySetup = synced,
        payOptions = [recipient "0.0002", "--spend-from", textOf (externalAddr 0), "--select", "--strategy", "smallest-first", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf (twoCoinsTx ctx) 1], [(7740, Right (internalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- The bip84 coin of 30000, output 1, with bip84 change: 10.5 + 68 +
    -- 34 + 31 = 143.5, so 144 virtual bytes, a fee of 1440, and 30000 -
    -- 20000 - 1440 = 8560 of change
    Payment
      { payRule = "spends only the coins of the type --coin-type names",
        payChain = confirmedChain (bothTypesTx ctx),
        paySetup = syncedBothTypes,
        payOptions = [recipient "0.0002", "--coin-type", "bip84", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf (bothTypesTx ctx) 1], [(8560, Right (segwitInternalAddr 0)), (20000, Right (otherAddr 0))])
      },
    -- 60000 needs both coins, 80000, with bip44 change: 10.5 + 148 + 68 +
    -- 34 + 34 = 294.5, so 295 virtual bytes, a fee of 2950, and 80000 -
    -- 60000 - 2950 = 17050 of change
    Payment
      { payRule = "spends bip44 and bip84 coins together, signed for both from the mnemonic typed once",
        payChain = confirmedChain (bothTypesTx ctx),
        paySetup = syncedBothTypes,
        payOptions = [recipient "0.0006", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf (bothTypesTx ctx) 0, coinOf (bothTypesTx ctx) 1], [(17050, Right (internalAddr 0)), (60000, Right (otherAddr 0))])
      },
    -- A legacy coin with bip84 change: 10 + 148 + 34 + 31 = 223 bytes, a
    -- fee of 2230, and 50000 - 20000 - 2230 = 27770 of change
    Payment
      { payRule = "sends the change to an address of the type --change-type names",
        payChain = paidChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--change-type", "bip84", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(20000, Right (otherAddr 0)), (27770, Right (segwitInternalAddr 0))])
      },
    -- The change of 27740 goes back to /0/0, which the coin spent is at
    Payment
      { payRule = "sends the change back to the address --change-back names",
        payChain = paidChain ctx,
        paySetup = synced,
        payOptions = [recipient "0.0002", "--change-back", textOf (externalAddr 0), "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([paidCoin], [(20000, Right (otherAddr 0)), (27740, Right (externalAddr 0))])
      },
    -- The coins of 50000 at /0/0 and 30000 at /0/1 pay 60000 together:
    -- 374 bytes, a fee of 3740, and 80000 - 60000 - 3740 = 16260 of
    -- change, back to /0/0, where the larger coin is
    Payment
      { payRule = "sends the change back to the address of the largest coin spent, with --change-back largest",
        payChain = confirmedChain (twoAddressesTx ctx),
        paySetup = synced,
        payOptions = [recipient "0.0006", "--change-back", "largest", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf (twoAddressesTx ctx) 0, coinOf (twoAddressesTx ctx) 1], [(16260, Right (externalAddr 0)), (60000, Right (otherAddr 0))])
      },
    -- The coin of 50000 of the portfolio on Bitcoin Cash, and its first
    -- change address there
    Payment
      { payRule = "pays on the --network given, from the coins of the portfolio there",
        payChain = confirmedChain (cashCoinTx ctx),
        paySetup = cashSynced,
        payOptions = [recipient "0.0002", "--network", "bch", "--fee-rate", "10"],
        payTyped = [],
        payHanded = ([coinOf (cashCoinTx ctx) 0], [(20000, Right (otherAddr 0)), (27740, Right cashChangeAddr)])
      }
  ]
  where
    recipient = fixedTo (otherAddr 0)
    coinOf tx = OutPoint (txHash tx)
    -- The coin of 50000 of the paid chain, and of the chain of fees
    paidCoin = coinOf (payment ctx) 0
    -- A home with portfolio0, its first address on Bitcoin Cash handed
    -- out, and synced
    cashSynced home = do
      newTestPortfolio home
      _ <- answer home ["receive", "--network", "bch"] []
      void $ answer home ["sync"] []

-- | The two ways to pay, each with how the tests name it
ways :: [(PayWith, String)]
ways = [(WithSend, "send"), (WithThreeCommands, "preparetx, signtx and broadcast")]

-- Every test below runs once for each way to pay, with the same
-- expectation. The payment of the tests that name none is the one of
-- 20000 satoshis from the coin of 50000: a fee of 2260, and 27740 of
-- change.
bothWaysSpec :: Ctx -> Spec
bothWaysSpec ctx =
  forM_ ways $ \(way, name) ->
    describe ("A payment made with " <> name) $ do
      let plain = [fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"]

      -- The inputs are compared sorted: their order is drawn at random
      forM_ (payments ctx) $ \pay ->
        it pay.payRule $
          withChain ctx pay.payChain $ \home chain -> do
            pay.paySetup home
            _ <- paidWith way home pay.payOptions pay.payTyped
            published <- inMempool <$> readIORef chain
            ((\(spent, paid) -> (sort spent, paid)) . spendsAndPays ctx <$> published) `shouldBe` [pay.payHanded]

      it "is answered with the transaction signed and sent" $
        withChain ctx (paidChain ctx) $ \home _ -> do
          synced home
          sent <- paidWith way home plain []
          pendingState sent `shouldBe` Just (True, True)

      it "is held by the wallet as signed and sent" $
        withChain ctx (paidChain ctx) $ \home _ -> do
          synced home
          _ <- paidWith way home plain []
          onePendingState home `shouldReturn` Just (True, True)

      it "keeps the label of --label" $
        withChain ctx (paidChain ctx) $ \home _ -> do
          synced home
          _ <- paidWith way home (plain <> ["--label", "rent of October"]) []
          (txInfoLabel <$> onePendingTx home) `shouldReturn` "rent of October"

      -- The best block of the chain is 800002. One time in ten the lock
      -- time is below it, by up to 99 blocks.
      it "has the lock time of the best block the wallet knows, or one up to 99 blocks below it" $
        withChain ctx (paidChain ctx) $ \home _ -> do
          synced home
          sent <- paidWith way home plain []
          summaryLockTime (txInfoSummary sent) `shouldSatisfy` (\lockTime -> lockTime <= 800002 && lockTime >= 800002 - 99)

      -- The best block was mined nine hours before the test, so the wallet
      -- is behind the chain
      it "has a lock time of 0 from a wallet behind the chain, with --yes" $
        withChainMinedAgo ctx [] (9 * 3600) (paidChain ctx) $ \home _ -> do
          synced home
          sent <- paidWith way home (plain <> ["--yes"]) []
          summaryLockTime (txInfoSummary sent) `shouldBe` 0

      -- The sequence number of BIP 125, which bumpfee needs
      it "is marked replaceable on Bitcoin" $
        withChain ctx (paidChain ctx) $ \home chain -> do
          synced home
          _ <- paidWith way home plain []
          published <- inMempool <$> readIORef chain
          concatMap (map (.sequence) . (.inputs)) published `shouldBe` [0xfffffffd]

      it "leaves its change to spend once it has a block and a sync has seen it" $
        withChain ctx (paidChain ctx) $ \home chain -> do
          synced home
          _ <- paidWith way home plain []
          modifyIORef chain mine
          (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 27740)]

      it "is no longer pending once a sync has seen it on the network" $
        withChain ctx (paidChain ctx) $ \home _ -> do
          synced home
          _ <- paidWith way home plain []
          _ <- answer home ["sync"] []
          (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

{- What send shows -}

-- Each test runs send of the payment of 20000 satoshis to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU at 10 satoshis a byte, in a home
-- synced with the paid chain, with the mnemonic typed.
screenSpec :: Ctx -> Spec
screenSpec ctx =
  describe "send, on the screen" $ do
    let sendRun home = do
          synced home
          hw home ["send", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] seedOnce

    it "shows the transaction before it asks for the mnemonic" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        shownBeforeMnemonic run `shouldSatisfy` Text.isInfixOf "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc\n"

    it "asks for the mnemonic after the transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        run `shouldSatisfy` askedMnemonic

    -- The recipient is on one line of the transaction, and nowhere else
    it "shows the transaction once" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        Text.count "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" (runOut run <> runErr run) `shouldBe` 1

    -- After a blank line, apart from the transaction above it
    it "says before the mnemonic that typing it signs and sends the transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        shownBeforeMnemonic run
          `shouldSatisfy` Text.isSuffixOf "\n\nTyping the mnemonic signs this transaction and sends it to the network. This cannot be undone.\n"

    -- The mnemonic is the answer
    it "does not ask whether to send, as broadcast does" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        run `shouldNotSatisfy` printed sendQuestion

    it "answers with the txid of the transaction it sent, not with the transaction again" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        txid <- txidOf =<< onePendingTx home
        take 1 (Text.lines (runOut run)) `shouldBe` ["Sent, txid " <> Text.pack txid]

    it "answers with a txid of 64 hexadecimal digits" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        (Text.stripPrefix "Sent, txid " =<< firstLine (runOut run))
          `shouldSatisfy` maybe False (\txid -> Text.length txid == 64 && Text.all isHexDigit txid)

    it "says to sync next, as broadcast does" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        runOut run `shouldSatisfy` Text.isSuffixOf "\n\nNext, sync the wallet to follow the transaction:\n  hw sync\n"

    it "exits as a success" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        run <- sendRun home
        runExit run `shouldBe` ExitSuccess

    -- The coin of 30000 at /0/1 is locked, and --coin names it with the
    -- one of 50000. 30000 satoshis are 0.0003 bitcoins. The coin is 3
    -- confirmations deep, its transaction seen at 1700000000, 23:13 in
    -- Central Europe.
    it "shows the coins chosen that cannot be spent below the transaction, before the mnemonic, as preparetx lists them" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        run <- hwWithEnv [("TZ", centralEurope)] home ["send", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 0, "--coin", coinArg (twoAddressesTx ctx) 1, "--fee-rate", "10"] seedOnce
        let (transaction, staying) = Text.breakOn "1 coin stays in the portfolio:" (shownBeforeMnemonic run)
        transaction `shouldSatisfy` Text.isInfixOf "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00020000 btc\n"
        staying
          `shouldSatisfy` Text.isInfixOf
            ( "\n" <> Text.pack (shortCoinArg (twoAddressesTx ctx) 1) <> "  14 Nov 2023 23:13      0.00030000 btc  locked by you, 3 confirmations\n"
                <> "  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  bip44/0/1\n"
                <> "  you locked it\n"
            )
  where
    firstLine text = case Text.lines text of
      line : _ -> Just line
      [] -> Nothing

{- A payment that was not signed -}

-- The payments are of 20000 satoshis from the coin of 50000 of the paid
-- chain, at 10 satoshis a byte, unless the test says otherwise. Each
-- failure is checked for what it says, and for what it leaves in the
-- wallet, in tests of their own. A wallet left as it was has no pending
-- transaction, and its one coin is as coins showed it before: 50000
-- satoshis, 3 confirmations, no lock, and free to spend.
notSignedSpec :: Ctx -> Spec
notSignedSpec ctx = do
  let plain = ["send", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"]
      noPendingTx home = (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
      coinIsFree home = (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Nothing, True)]

  describe "send of a payment preparetx refuses" $ do
    -- 100000 satoshis from a portfolio that holds 50000
    let tooMuch = ["send", fixedTo (otherAddr 0) "0.001", "--fee-rate", "10"]
    it "refuses it as preparetx does" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home tooMuch seedOnce) `shouldReturn` Right "The coins do not cover the amount and the fee"

    it "does not ask for the mnemonic" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home tooMuch seedOnce
        run `shouldNotSatisfy` askedMnemonic

    it "exits as a failure" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home tooMuch seedOnce
        runExit run `shouldBe` ExitFailure 1

    it "keeps no pending transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- hwJson home tooMuch seedOnce
        noPendingTx home

    it "leaves the coin of the portfolio free to spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- hwJson home tooMuch seedOnce
        coinIsFree home

  describe "send at a fee rate above 100 that the user turns down" $ do
    let highFee = ["send", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "101"]
    it "asks about the rate, as preparetx does" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home highFee ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf highFeeQuestion

    it "says that nothing was prepared" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home highFee ["n"]) `shouldReturn` Right highFeeRefused

    it "does not ask for the mnemonic" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home highFee ["n"]
        run `shouldNotSatisfy` askedMnemonic

    it "keeps no pending transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- hwJson home highFee ["n"]
        noPendingTx home

    it "leaves the coin of the portfolio free to spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- hwJson home highFee ["n"]
        coinIsFree home

  -- The input ends where the mnemonic is asked, as when the user stops
  -- there
  describe "send stopped at the mnemonic" $ do
    it "says it was cancelled" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home plain []) `shouldReturn` Right "Cancelled: the input ended before the answer"

    it "sends nothing" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        synced home
        _ <- hwJson home plain []
        (inMempool <$> readIORef chain) `shouldReturn` []

    it "keeps no pending transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- hwJson home plain []
        noPendingTx home

    it "leaves the coin of the portfolio free to spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- hwJson home plain []
        coinIsFree home

  -- The second test mnemonic is of another wallet
  describe "send given a mnemonic that is not the portfolio's" $ do
    let wrong = seedOnceOf testMnemonic2
    it "asks again, and sends once the right one is typed" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        synced home
        _ <- answer home plain (wrong <> seedOnce)
        ((spendsAndPays ctx <$>) . inMempool <$> readIORef chain) `shouldReturn` [paymentSent ctx]

    it "gives up after three, and says the mnemonic does not match" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home plain (wrong <> wrong <> wrong))
          `shouldReturn` Right "The mnemonic or the passphrase does not match the portfolio"

    it "sends nothing after three" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        synced home
        _ <- hwJson home plain (wrong <> wrong <> wrong)
        (inMempool <$> readIORef chain) `shouldReturn` []

    it "keeps no pending transaction after three" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- hwJson home plain (wrong <> wrong <> wrong)
        noPendingTx home

    it "leaves the coin of the portfolio free to spend after three" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- hwJson home plain (wrong <> wrong <> wrong)
        coinIsFree home

  -- The home is synced, then its configuration names a store that
  -- refuses every connection
  describe "send when the store cannot be reached" $ do
    let unreachable home = setConfigKey home "host" "http://127.0.0.1:1/"
    it "says so" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        unreachable home
        (errorOf <$> hwJson home plain seedOnce) `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed"

    it "does not ask for the mnemonic, which would be typed for nothing" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        unreachable home
        run <- hw home plain seedOnce
        run `shouldNotSatisfy` askedMnemonic

    it "keeps no pending transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        unreachable home
        _ <- hwJson home plain seedOnce
        noPendingTx home

    it "leaves the coin of the portfolio free to spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        unreachable home
        _ <- hwJson home plain seedOnce
        coinIsFree home

{- A payment that was signed and not sent -}

-- The store takes no transaction. send has signed the payment by then, and
-- the wallet holds it as signtx leaves one: signed, and not online.
notSentSpec :: Ctx -> Spec
notSentSpec ctx = do
  let plain = ["send", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"]
      refusing = withChainNotPublishing ctx (NotPublished "bad-txns-inputs-missingorspent") (paidChain ctx)
      -- What send adds to the failure of a sending: that the transaction
      -- is signed and kept, and the two commands that take it from there,
      -- each with the short id of its txid, its first 8 digits. The txid
      -- differs from run to run, as the order of the outputs does.
      keptNote txid =
        ". The transaction is signed and stays pending: broadcast "
          <> Text.pack (shortOf txid)
          <> " sends it again, and deletetx "
          <> Text.pack (shortOf txid)
          <> " deletes it and frees its coins"

  describe "send of a transaction the store says no node would take" $ do
    it "fails with what the store said, then with what to do with the signed transaction" $
      refusing $ \home _ -> do
        synced home
        failure <- errorOf <$> hwJson home plain seedOnce
        txid <- txidOf =<< onePendingTx home
        failure `shouldSatisfy` either (const False) (Text.isSuffixOf (" refused the transaction: bad-txns-inputs-missingorspent" <> keptNote txid))

    it "exits as a failure" $
      refusing $ \home _ -> do
        synced home
        run <- hw home plain seedOnce
        runExit run `shouldBe` ExitFailure 1

    it "keeps the transaction signed and not sent, as signtx leaves one" $
      refusing $ \home _ -> do
        synced home
        _ <- hwJson home plain seedOnce
        onePendingState home `shouldReturn` Just (True, False)

    -- The coin of 50000, 3 confirmations deep, is the one the signed
    -- transaction spends
    it "keeps the coin of the transaction locked" $
      refusing $ \home _ -> do
        synced home
        _ <- hwJson home plain seedOnce
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Just LockedByTransaction, False)]

    it "leaves a transaction that deletetx deletes, which frees its coin" $
      refusing $ \home _ -> do
        synced home
        _ <- hwJson home plain seedOnce
        txid <- txidOf =<< onePendingTx home
        _ <- answer home ["deletetx", shortOf txid] []
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Nothing, True)]

  -- A node says nothing of a transaction it refuses for its fee
  describe "send of a transaction no node announced back to the store" $ do
    let silent = withChainNotPublishing ctx (PublishFailed "no node announced it") (paidChain ctx)
    it "fails as broadcast does, then with what to do with the signed transaction" $
      silent $ \home _ -> do
        synced home
        failure <- errorOf <$> hwJson home plain seedOnce
        txid <- txidOf =<< onePendingTx home
        failure
          `shouldSatisfy` either
            (const False)
            ( Text.isSuffixOf
                ( " handed the transaction to a node, and no node announced it in the time the store waits. A node says nothing of a transaction it refuses, so its fee may be too low for the network"
                    <> keptNote txid
                )
            )

    it "keeps the transaction signed and not sent" $
      silent $ \home _ -> do
        synced home
        _ <- hwJson home plain seedOnce
        onePendingState home `shouldReturn` Just (True, False)

  -- The store takes no transaction the first time it is handed one, and
  -- takes it the second time. broadcast names the transaction by the
  -- short id of its txid, as the failure of send names it.
  describe "A transaction send signed and could not send" $ do
    let awayAtFirst = withChainNotPublishingAtFirst ctx (PublishFailed "no node announced it") (paidChain ctx)
    it "is sent by broadcast once the store takes it" $
      awayAtFirst $ \home chain -> do
        synced home
        _ <- hwJson home plain seedOnce
        txid <- txidOf =<< onePendingTx home
        _ <- answer home ["broadcast", shortOf txid, "--yes"] []
        ((spendsAndPays ctx <$>) . inMempool <$> readIORef chain) `shouldReturn` [paymentSent ctx]

    it "is held as signed and sent once broadcast has sent it" $
      awayAtFirst $ \home _ -> do
        synced home
        _ <- hwJson home plain seedOnce
        txid <- txidOf =<< onePendingTx home
        _ <- answer home ["broadcast", shortOf txid, "--yes"] []
        onePendingState home `shouldReturn` Just (True, True)
