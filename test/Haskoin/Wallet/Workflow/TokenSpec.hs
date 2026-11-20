{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A coin of Bitcoin Cash that holds tokens (CashTokens), through the
-- commands: hw shows it as a coin of the wallet that is locked for good,
-- counts it in every balance, and never spends it, in a payment or in a
-- sweep.
--
-- The store of these tests lists such a coin as haskoin-store 2.0.0 does:
-- at the address of its locking script, with the token prefix before
-- that script.
module Haskoin.Wallet.Workflow.TokenSpec (spec) where

import Data.IORef (readIORef)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.Sweep (UnsweptCoin (..), UnsweptReason (..))
import Haskoin.Wallet.Workflow.TestUtils
import Test.Hspec

-- | A home whose portfolio has handed out its first address on Bitcoin
-- Cash, which 'tokenFunding' pays, and has not synced yet
withCashAddress :: FilePath -> IO ()
withCashAddress home = do
  newTestPortfolio home
  _ <- answer home ["receive", "--network", "bch"] []
  return ()

-- | That home once synced: it holds the two coins of 'tokenFunding', 50000
-- satoshis that are plain and 30000 that hold tokens
syncedWithTokens :: FilePath -> IO ()
syncedWithTokens home = do
  withCashAddress home
  _ <- answer home ["sync"] []
  return ()

-- | The plain coin and the coin with tokens of a transaction of these
-- tests: its outputs 0 and 1
plainOf, tokensOf :: Tx -> OutPoint
plainOf tx = OutPoint (txHash tx) 0
tokensOf tx = OutPoint (txHash tx) 1

spec :: Spec
spec = do
  walletSpec
  paymentSpec
  sweepSpec

walletSpec :: Spec
walletSpec =
  describe "A coin of Bitcoin Cash that holds tokens, in the wallet" $ do
    it "is announced by the sync that finds it, with what it holds and what moves it" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        withCashAddress home
        run <- hw home ["sync"] []
        -- The lines of the other networks follow
        runOut run
          `shouldSatisfy` Text.isPrefixOf
            ( Text.unlines
                [ "portfolio0  bch",
                  "  spendable 0.00050000 btc, 2 coins",
                  "  synced at block 800002",
                  "  1 transaction and 2 coins updated",
                  "  1 new coin holds tokens (CashTokens): 0.00030000 btc, which hw never spends",
                  "    a wallet that handles tokens moves it, with the same mnemonic"
                ]
            )

    it "is not announced again by the next sync" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        syncedWithTokens home
        run <- hw home ["sync"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "tokens"

    -- The best block is 800002 and the coin is in block 800000
    it "is listed by coins as locked by tokens" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        syncedWithTokens home
        run <- hw home ["coins", "--network", "bch"] []
        runOut run `shouldSatisfy` Text.isInfixOf "0.00030000 btc  locked by tokens, 3 confirmations\n"

    -- 50000 + 30000 = 80000, of which the 30000 that hold tokens cannot be
    -- spent
    it "counts in the total of balances, as unspendable" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        syncedWithTokens home
        run <- hw home ["balances", "--network", "bch"] []
        runOut run `shouldSatisfy` Text.isInfixOf "total 0.00080000 btc, spendable 0.00050000 btc, unspendable 0.00030000 btc"

    it "is the part of its address that balances --verbose gives as locked by tokens" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        syncedWithTokens home
        run <- hw home ["balances", "--network", "bch", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Locked by tokens : 0.00030000 btc\n"

    -- The history and the balance must agree: the payment brought 80000
    it "counts in the amount txs gives for the payment that brought it" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        syncedWithTokens home
        run <- hw home ["txs", "--network", "bch"] []
        runOut run `shouldSatisfy` Text.isInfixOf "+0.00080000 btc"

    it "is refused by lockcoin, which says it needs no lock" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        syncedWithTokens home
        (errorOf <$> hwJson home ["lockcoin", coinArg tokenFunding 1] [])
          `shouldReturn` Right "The coin holds tokens (CashTokens), and hw never spends such a coin: it needs no lock"

paymentSpec :: Spec
paymentSpec =
  describe "A payment from a portfolio with a coin that holds tokens" $ do
    -- One P2PKH input and one P2PKH output are 10 + 148 + 34 = 192 bytes:
    -- at 10 satoshis a byte the fee is 1920, and 50000 - 1920 = 48080
    it "sends everything but that coin, with sendall" $
      withChain (confirmedChain tokenFunding) $ \home chain -> do
        syncedWithTokens home
        _ <- hw home ["sendall", textOf (otherAddr 0), "--network", "bch", "--fee-rate", "10"] seedOnce
        published <- inMempool <$> readIORef chain
        (spendsAndPays <$> published) `shouldBe` [([plainOf tokenFunding], [(48080, Right (otherAddr 0))])]

    it "names that coin among the coins a sendall leaves, with why" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        syncedWithTokens home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--network", "bch", "--fee-rate", "10"] []
        runOut run `shouldSatisfy` Text.isInfixOf "locked by tokens, 3 confirmations\n"
        runOut run `shouldSatisfy` Text.isInfixOf "\n  it holds tokens, and hw never spends such a coin\n"

    -- 60000 is more than the plain coin of 50000, and less than both coins
    it "is not made when it needs that coin: nothing is sent" $
      withChain (confirmedChain tokenFunding) $ \home chain -> do
        syncedWithTokens home
        _ <- hw home ["send", fixedTo (otherAddr 0) "0.0006", "--network", "bch", "--fee-rate", "10"] seedOnce
        (inMempool <$> readIORef chain) `shouldReturn` []

    it "is refused when --coin names that coin, with why" $
      withChain (confirmedChain tokenFunding) $ \home _ -> do
        syncedWithTokens home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--network", "bch", "--fee-rate", "10", "--coin", coinArg tokenFunding 1] [])
          `shouldReturn` Right ("None of the coins chosen can be spent now: " <> Text.pack (coinArg tokenFunding 1) <> ", it holds tokens, and hw never spends such a coin")

sweepSpec :: Spec
sweepSpec =
  describe "A sweep of a key with a coin that holds tokens" $ do
    -- The key holds 50000 that are plain and 30000 that hold tokens. One
    -- P2PKH input and one P2PKH output are 192 bytes: a fee of 1920 at 10
    -- satoshis a byte, and 50000 - 1920 = 48080
    it "sweeps the other coins of the key, and leaves that one" $
      withChain (confirmedChain keyTokenFunding) $ \home chain -> do
        withCashAddress home
        keys <- writeKeyFile home
        _ <- hw home (["sweep", "--key-file", keys] <> toWallet) ["y"]
        published <- inMempool <$> readIORef chain
        (spendsAndPays <$> published) `shouldBe` [([plainOf keyTokenFunding], [(48080, Right bchExternalAddr)])]

    it "names the coin it leaves below the transaction, and says what moves it" $
      withChain (confirmedChain keyTokenFunding) $ \home _ -> do
        withCashAddress home
        keys <- writeKeyFile home
        run <- hw home (["preparesweep", "--key-file", keys] <> toWallet) []
        runOut run
          `shouldSatisfy` Text.isInfixOf
            (Text.pack ("\n\n1 coin holds tokens (CashTokens) and is not in the sweep:\n  " <> coinArg keyTokenFunding 1 <> "\n    at "))
        run `shouldSatisfy` printedSentence "0.00030000 btc hw does not handle tokens and never spends such a coin. Move it with a wallet that handles the tokens of Bitcoin Cash."

    it "gives the coin it leaves in its --json answer, as a coin that holds tokens" $
      withChain (confirmedChain keyTokenFunding) $ \home _ -> do
        withCashAddress home
        keys <- writeKeyFile home
        prepared <- answer home (["preparesweep", "--key-file", keys] <> toWallet) []
        (\case ResponseSweep _ _ _ _ unswept _ -> unswept; _ -> []) prepared
          `shouldBe` [UnsweptCoin (tokensOf keyTokenFunding) wikiAddr 30000 HoldsTokens]

    -- The key holds one coin, of 30000, which holds tokens
    it "is refused when the key holds no other coin, with what moves the coin" $
      withChain (confirmedChain (holdingTokens 0 (fundingTx 9 [(wikiAddr, 30000)]))) $ \home chain -> do
        withCashAddress home
        keys <- writeKeyFile home
        (errorOf <$> hwJson home (["preparesweep", "--key-file", keys] <> toWallet) [])
          `shouldReturn` Right "The only coins in those addresses hold tokens (CashTokens). hw does not handle tokens and never spends such a coin: move them with a wallet that handles the tokens of Bitcoin Cash"
        (inMempool <$> readIORef chain) `shouldReturn` []
  where
    -- A sweep to the first address of the portfolio on Bitcoin Cash, at 10
    -- satoshis a byte
    toWallet = ["--sweep-to", textOf bchExternalAddr, "--network", "bch", "--fee-rate", "10"]
