{-# LANGUAGE OverloadedStrings #-}

-- | A transaction labelled by the user: by preparetx, preparesendall and
-- preparesweep, carried in its file to the signer and back, kept through
-- sending and the sync, taken by the replacement bumpfee prepares, changed
-- by label, and shown by txs and pendingtxs. And the labels of the
-- account's addresses, carried in the file for the signer to show.
module Haskoin.Wallet.Workflow.LabelSpec (spec) where

import Data.IORef (modifyIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData (..))
import Haskoin.Wallet.Workflow.TestUtils
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  preparingSpec ctx
  fileSpec ctx
  addressLabelsSpec ctx
  coinLabelsSpec ctx
  changeAddressSpec ctx
  historySpec ctx
  labelCommandSpec ctx
  shownSpec ctx

-- | The transactions of the history, each by its id, with its label
historyLabels :: Response -> Either String [(String, Text)]
historyLabels (ResponseTxs _ infos) =
  Right [(Text.unpack (txHashToHex (chainTxid status)), txInfoLabel info) | info <- infos, OnChain status <- [txInfoStatus info]]
historyLabels other = Left (show other)

-- | The answer of label for a transaction: each account by name, with the
-- label of the transaction
labelledOf :: Response -> Either String [(Text, Text)]
labelledOf (ResponseLabelledTxs held) = Right [(dBAccountName acc, txInfoLabel info) | (acc, info) <- held]
labelledOf other = Left (show other)

-- | The id of the payment of the paid chain, as typed
paidTxid :: Ctx -> String
paidTxid ctx = Text.unpack (txHashToHex (txHash (payment ctx)))

-- | The payment of 20000 to the other wallet, prepared with the label,
-- signed and sent at 10 satoshis a byte, in a home synced with the paid
-- chain: the id of the payment
sentWithLabel :: FilePath -> String -> IO String
sentWithLabel home label = do
  synced home
  nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", label] []
  _ <- answer home ["signtx", nosig] seedOnce
  txidOf =<< txOf =<< answer home ["sendtx", nosig, "--yes"] []

preparingSpec :: Ctx -> Spec
preparingSpec ctx =
  describe "A label given to a transaction in preparation" $ do
    it "is shown by preparetx, and kept among the pending transactions" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "Rent"] []
        txInfoLabel prepared `shouldBe` "Rent"
        (txInfoLabel <$> onePendingTx home) `shouldReturn` "Rent"

    it "is none without --label" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        txInfoLabel prepared `shouldBe` ""

    it "is given by preparesendall" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10", "--label", "Everything"] []
        txInfoLabel prepared `shouldBe` "Everything"

    it "is given by preparesweep to a sweep that pays the wallet" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home ["preparesweep", "--coin", coinArg (oldCoin ctx) 0, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10", "--label", "Old wallet"] []
        txInfoLabel prepared `shouldBe` "Old wallet"

    -- -l is short for --label, as it is for receive
    it "goes with a sweep outside the wallet to its file, given with -l" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        let file = home </> "sweep.json"
        _ <- answer home ["preparesweep", "--coin", coinArg (oldCoin ctx) 0, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", file, "--fee-rate", "10", "-l", "Old wallet"] []
        (txSignDataLabel . txFileData <$> readTxFile file) `shouldReturn` "Old wallet"

fileSpec :: Ctx -> Spec
fileSpec ctx =
  describe "A label in the file of a transaction" $ do
    it "goes with the transaction to its --output file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.json"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file, "--label", "Rent"] []
        (txSignDataLabel . txFileData <$> readTxFile file) `shouldReturn` "Rent"

    it "goes with the transaction to the file of exporttx" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "Rent"] []
        let file = home </> "exported.json"
        _ <- answer home ["exporttx", nosig, file] []
        (txSignDataLabel . txFileData <$> readTxFile file) `shouldReturn` "Rent"

    it "comes back from an offline signer with the signed transaction, which sendtx sends with it" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        (txSignDataLabel . txFileData <$> readTxFile (signer </> "signed.json")) `shouldReturn` "Rent"
        sent <- txOf =<< answer online ["sendtx", "--input", signer </> "signed.json", "--yes"] []
        txInfoLabel sent `shouldBe` "Rent"

    -- The user changes the label while the file is away to be signed
    it "gives way to the label the online computer holds when the signed file comes back" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["label", nosig, "Rent of October"] []
        sent <- txOf =<< answer online ["sendtx", "--input", signer </> "signed.json", "--yes"] []
        txInfoLabel sent `shouldBe` "Rent of October"

    it "gives way to the label the online computer holds in the answer of importtx too" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["label", nosig, "Rent of October"] []
        imported <- txOf =<< answer online ["importtx", signer </> "signed.json"] []
        txInfoLabel imported `shouldBe` "Rent of October"

-- The payment of the paid chain is at the first receiving address,
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, which the transaction of these tests
-- spends. Its change goes to the first change address, which takes no
-- label.
addressLabelsSpec :: Ctx -> Spec
addressLabelsSpec ctx =
  describe "The labels of the account's addresses in a transaction file" $ do
    it "go with the transaction to its --output file, by address, those that have one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "unsigned.json"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        (txFileAddressLabels <$> readTxFile file) `shouldReturn` Map.fromList [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Salary")]

    -- The address is labelled after the transaction is prepared
    it "are those of the wallet when exporttx writes the file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "exported.json"
        _ <- answer home ["exporttx", nosig, file] []
        (txFileAddressLabels <$> readTxFile file) `shouldReturn` Map.fromList [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Salary")]

    it "are those of the wallet when signtx signs a pending transaction, shown and written to its --output file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "signed.json"
        signed <- txOf =<< answer home ["signtx", nosig, "--output", file] seedOnce
        addressLabelsOf signed `shouldBe` Map.fromList [(externalAddr 0, "Salary"), (internalAddr 0, "")]
        (txFileAddressLabels <$> readTxFile file) `shouldReturn` Map.fromList [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Salary")]

    -- The address is relabelled after the file is written
    it "are shown by reviewtx in place of the wallet's own" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "unsigned.json"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        _ <- answer home ["label", textOf (externalAddr 0), "Wages"] []
        reviewed <- txOf =<< answer home ["reviewtx", file] []
        addressLabelsOf reviewed `shouldBe` Map.fromList [(externalAddr 0, "Salary"), (internalAddr 0, "")]

    it "are shown by reviewtx on the offline signer, which has no labels of its own" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        run <- hw signer ["reviewtx", online </> "unsigned.json"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0  \"Salary\"\n"

    it "are shown by signtx --input on the offline signer, and go on to the signed file" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        addressLabelsOf signed `shouldBe` Map.fromList [(externalAddr 0, "Salary"), (internalAddr 0, "")]
        (txFileAddressLabels <$> readTxFile (signer </> "signed.json")) `shouldReturn` Map.fromList [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Salary")]

    -- The address is relabelled while the file is away to be signed
    it "give way to the wallet's own when importtx takes the signed file back" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["label", textOf (externalAddr 0), "Wages"] []
        imported <- txOf =<< answer online ["importtx", signer </> "signed.json"] []
        addressLabelsOf imported `shouldBe` Map.fromList [(externalAddr 0, "Wages"), (internalAddr 0, "")]

    it "give way to the wallet's own when sendtx --input sends the signed file" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["label", textOf (externalAddr 0), "Wages"] []
        sent <- txOf =<< answer online ["sendtx", "--input", signer </> "signed.json", "--yes"] []
        addressLabelsOf sent `shouldBe` Map.fromList [(externalAddr 0, "Wages"), (internalAddr 0, "")]

-- The coin of the paid chain is the output 0 of the payment, 50000 at
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj with 3 confirmations
coinLabelsSpec :: Ctx -> Spec
coinLabelsSpec ctx =
  describe "The label a coin shows" $ do
    it "is the label of the payment that paid it, in coins" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        (coinLabelsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, "Salary")]

    it "is on a line of its own below the first, and the label of the address follows the address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Wages"] []
        run <- hw home ["coins"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  \"Salary\"\n  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj \"Wages\"\n"

    -- The change of the payment of 20000 is the one coin left
    it "is none on the change of a payment the user made and labelled" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentWithLabel home "Rent"
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (fmap (map snd) . coinLabelsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [""]

    it "goes with the transaction that spends the coin to its file, by the id of the payment" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        let file = home </> "unsigned.json"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        (txFileInputLabels <$> readTxFile file) `shouldReturn` Map.fromList [(txHash (payment ctx), "Salary")]

    it "ends the address spent from in reviewtx on the offline signer, after the label of the address" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", paidTxid ctx, "Salary"] []
        _ <- answer online ["label", textOf (externalAddr 0), "Wages"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        run <- hw signer ["reviewtx", online </> "unsigned.json"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0  \"Wages\"  from \"Salary\"\n"

    -- The label is given while the file is away
    it "is the wallet's own in the answer of importtx, not the file's" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["label", paidTxid ctx, "Salary"] []
        imported <- txOf =<< answer online ["importtx", signer </> "signed.json"] []
        txInfoInputLabels imported `shouldBe` Map.fromList [(txHash (payment ctx), "Salary")]

-- The change of the payment of 20000, 27740, goes to the first change
-- address, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, which takes no label
changeAddressSpec :: Ctx -> Spec
changeAddressSpec ctx =
  describe "A change address" $ do
    it "is said to be one in coins, where the label of the address would be" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentWithLabel home "Rent"
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        run <- hw home ["coins"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  at 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn change address\n"

    it "is said to be one by a transaction that spends from it" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentWithLabel home "Rent"
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--fee-rate", "10"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  /1/0  change address\n"

historySpec :: Ctx -> Spec
historySpec ctx =
  describe "A label once the network has the transaction" $ do
    it "stays with the transaction in the history after the sync" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        sent <- sentWithLabel home "Rent"
        _ <- answer home ["sync"] []
        (fmap (lookup sent) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Rent")

    it "goes to the replacement bumpfee prepares" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentWithLabel home "Rent"
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        txInfoLabel bumped `shouldBe` "Rent"

    it "shows on both the replaced transaction and its replacement in the history" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        stuck <- sentWithLabel home "Rent"
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        _ <- answer home ["signtx", nosig] seedOnce
        replacement <- txidOf =<< txOf =<< answer home ["sendtx", nosig, "--yes"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        labels <- historyLabels <$> hwJson home ["txs"] []
        fmap (\rows -> (lookup stuck rows, lookup replacement rows)) labels `shouldBe` Right (Just "Rent", Just "Rent")

labelCommandSpec :: Ctx -> Spec
labelCommandSpec ctx =
  describe "label for a transaction" $ do
    it "labels a pending transaction by its nosigHash, and answers with it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (labelledOf <$> hwJson home ["label", nosig, "Rent"] []) `shouldReturn` Right [("main", "Rent")]
        (txInfoLabel <$> onePendingTx home) `shouldReturn` "Rent"

    it "labels a pending transaction by its txid once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        (labelledOf <$> hwJson home ["label", txid, "Rent"] []) `shouldReturn` Right [("main", "Rent")]

    it "labels a transaction of the history by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (labelledOf <$> hwJson home ["label", paidTxid ctx, "Salary"] []) `shouldReturn` Right [("main", "Salary")]
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")

    it "removes the label of a transaction with an empty one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        _ <- answer home ["label", paidTxid ctx, ""] []
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "")

    it "refuses a transaction the wallet does not have" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["label", "ab00000000000000000000000000000000000000000000000000000000000000", "Rent"] [])
          `shouldReturn` Right "The wallet has no transaction ab00000000000000000000000000000000000000000000000000000000000000"

shownSpec :: Ctx -> Spec
shownSpec ctx =
  describe "A label as hw shows it" $ do
    -- The payment received at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj
    it "is on a line of its own in txs, below the first line of its transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        run <- hw home ["txs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  \"Salary\"\n  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n"

    it "is a field of txs --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        run <- hw home ["txs", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Label    : \"Salary\"\n"

    -- pendingtxs lists every account without --account, so it names the
    -- account before the address paid
    it "is on a line of its own in pendingtxs" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "Rent"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  \"Rent\"\n  account main, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\n"

    it "is shown by label with the transaction, after its account and network" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["label", paidTxid ctx, "Salary"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Account  : main\n  Network  : btc\n  Label    : \"Salary\"\n"
