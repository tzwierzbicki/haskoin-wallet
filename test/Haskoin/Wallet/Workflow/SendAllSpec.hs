{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The coins of an account sent by preparesendall, and the coins worth
-- less than their fee, sent or left.
module Haskoin.Wallet.Workflow.SendAllSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Database (DBAccount (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), LeftReason (..))
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.SignData (TxFile (..))
import Haskoin.Wallet.Workflow.TestUtils
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  sendAllSpec ctx

-- | The same coins, and dust coins of 1200, 900 and 546 satoshis
dustCoinsTx :: Ctx -> Tx
dustCoinsTx ctx = fundingTx ctx 6 [(externalAddr 0, 50000), (externalAddr 0, 30000), (externalAddr 0, 1200), (externalAddr 0, 900), (externalAddr 0, 546)]

-- | The question of preparesendall about the three dust coins
dustQuestion :: Text
dustQuestion = "Spend them too? [y/N] "

-- At 10 satoshis a byte, the coins of 50000 and 30000 to one address make
-- 10 + 2 * 148 + 34 = 340 bytes, a fee of 3400, and 76600 for the address;
-- to two addresses, 374 bytes, a fee of 3740, and 76260 to share. The coin
-- of 50000 alone to one address makes 192 bytes, a fee of 1920. The dust
-- coins add 3 inputs, 4440 of fee for 2646 of coins: all 5 coins to one
-- address make 784 bytes, a fee of 7840, and 82646 - 7840 = 74806 for the
-- address. Where nothing is typed, a question would cancel the command.
sendAllSpec :: Ctx -> Spec
sendAllSpec ctx = do
  describe "The coins of an account sent by preparesendall" $ do
    it "all go to the address, which pays the fee, with no change" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
        prepared <- txOf sendAll
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 76600)]
        paidToWallet prepared `shouldBe` Map.empty
        leftOf sendAll `shouldBe` Right []

    it "give the fixed amounts as typed, and what remains to the address without one" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", fixedTo (otherAddr 0) "0.0003", textOf (otherAddr 1), "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 30000), (otherAddr 1, 46260)]

    it "share what remains equally among the addresses without an amount" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), textOf (otherAddr 1), "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 38130), (otherAddr 1, 38130)]

    it "prepare nothing when they do not cover the fixed amounts and the fee" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", fixedTo (otherAddr 0) "0.0008", textOf (otherAddr 1), "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins, 80000 satoshis, do not cover the fixed amounts, 80000, and the fee, 3740"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "need an address without an amount to receive what remains" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", fixedTo (otherAddr 0) "0.0003", "--fee-rate", "10"] [])
          `shouldReturn` Right "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"

    it "are those of --coin only, sent in full" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--coin", coinArg (twoCoinsTx ctx) 0, "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 48080)]
        paidToWallet prepared `shouldBe` Map.empty

    it "not given with --coin stay in the account, which the answer lists" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (leftOf <$> hwJson home ["preparesendall", textOf (otherAddr 0), "--coin", coinArg (twoCoinsTx ctx) 0, "--fee-rate", "10"] [])
          `shouldReturn` Right [(OutPoint (txHash (twoCoinsTx ctx)) 1, 30000, NotGiven)]

    it "stay in the account when the user locked them, which the answer lists" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoCoinsTx ctx) 1] []
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 48080)]
        leftOf sendAll `shouldBe` Right [(OutPoint (txHash (twoCoinsTx ctx)) 1, 30000, Locked LockedByUser)]

    -- 30000 satoshis are 0.0003 bitcoins
    it "that stay in the account are shown after the transaction, each with why" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoCoinsTx ctx) 1] []
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
        -- The transaction, then the coins staying, then the next steps
        let (beforeSteps, steps) = Text.breakOn "Next, sign it" (runOut run)
            (transaction, staying) = Text.breakOn "in the account:" beforeSteps
        transaction `shouldSatisfy` Text.isInfixOf "NoSigHash"
        staying `shouldSatisfy` Text.isInfixOf ("\n    0.00030000 btc  you locked it\n  " <> Text.pack (shortCoinArg (twoCoinsTx ctx) 1) <> "\n")
        steps `shouldSatisfy` Text.isInfixOf "hw signtx"

    it "are sent at a fee rate above 100 satoshis per virtual byte only if the user takes it" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "101"] ["n"]) `shouldReturn` Right highFeeRefused

    -- 340 bytes at 101 satoshis a byte are a fee of 34340, which leaves
    -- 80000 - 34340 = 45660 for the address
    it "are sent at a fee rate above 100 satoshis per virtual byte without asking with --yes" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "101", "--yes"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 45660)]

    -- The wallet has two accounts, and the coins are of main
    it "are those of the account of --account" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        prepared <- answer home ["preparesendall", textOf (otherAddr 0), "--account", "main", "--fee-rate", "10"] []
        (dBAccountName <$> accountOf prepared) `shouldBe` Right "main"
        paidToOthers <$> txOf prepared `shouldReturn` Map.fromList [(otherAddr 0, 76600)]

    -- Two outputs make 374 bytes, a fee of 3740, which leaves 80000 - 3740 -
    -- 76000 = 260 for the second address
    it "are not sent when what remains for an address is below the dust limit" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", fixedTo (otherAddr 0) "76000sat", textOf (otherAddr 1), "--fee-rate", "10"] [])
          `shouldReturn` Right "What remains for each address without an amount, 260 satoshis, is below the dust limit of 546"

    -- The coin of 50000 has 3 confirmations, and the coin of 30000 has 1.
    -- The coin of 50000 alone makes 192 bytes, a fee of 1920.
    it "stay in the account with fewer confirmations than --min-conf asks, which the answer lists" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--min-conf", "2", "--fee-rate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 48080)]
        leftOf sendAll `shouldBe` Right [(OutPoint (txHash (newerPayment ctx)) 0, 30000, NotSpendableYet)]

    it "are written to the --output file with the network of their account" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["preparesendall", textOf (otherAddr 0), "--output", home </> "unsigned.psbt", "--fee-rate", "10"] []
        (txFileNetwork <$> readTxFile ctx (home </> "unsigned.psbt")) `shouldReturn` Just btc

  describe "The coins worth less than their fee, sent by preparesendall or not" $ do
    it "are listed on stderr with what spending them or leaving them means, and asked about" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] ["n"]
        run `shouldSatisfy` printedSentence "3 coins cost more to spend than they are worth at 10 sat/vB, 0.00001480 btc each:"
        run `shouldSatisfy` printedSentence "What remains for the addresses without an amount is 0.00074806 btc instead of 0.00076600 btc, 0.00001794 btc less, and the account is empty."
        runErr run `shouldSatisfy` Text.isInfixOf dustQuestion

    it "stay in the account when the user answers anything but yes" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] ["n"]
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 76600)]

    it "are sent too when the user answers yes" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] ["y"]
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 74806)]

    it "stay in the account without a question with --leave-dust, which the answer lists" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--leave-dust", "--fee-rate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 76600)]
        let dustCoin i = OutPoint (txHash (dustCoinsTx ctx)) i
        leftOf sendAll `shouldBe` Right [(dustCoin 2, 1200, WorthLessThanFee), (dustCoin 3, 900, WorthLessThanFee), (dustCoin 4, 546, WorthLessThanFee)]

    it "are sent too without a question with --spend-dust" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--spend-dust", "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 74806)]

    -- The coins of 30000 and 900: 340 bytes, a fee of 3400, and 30900 - 3400
    it "are sent without a question when given with --coin" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--coin", coinArg (dustCoinsTx ctx) 1, "--coin", coinArg (dustCoinsTx ctx) 3, "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 27500)]

    it "are not left or spent by choice with --coin, whose coins are all sent" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", textOf (otherAddr 0), "--coin", coinArg (dustCoinsTx ctx) 1, "--leave-dust", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins chosen with --coin or --spend-from are all sent: give --spend-dust or --leave-dust without them"
