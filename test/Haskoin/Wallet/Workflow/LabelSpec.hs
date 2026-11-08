{-# LANGUAGE OverloadedStrings #-}

-- | A transaction labelled by the user: by preparetx, preparesendall and
-- preparesweep, carried in its file to the signer and back, kept through
-- sending and the sync, taken by the replacement bumpfee prepares, changed
-- by label, and shown by txs and pendingtxs. And the labels of the
-- account's addresses, carried in the file for the signer to show. Last,
-- the accounts with their labels and locks in the BIP 329 file exportacc
-- writes and importacc reads, from hw or from another wallet.
module Haskoin.Wallet.Workflow.LabelSpec (spec) where

import Data.IORef (modifyIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Haskoin
import Haskoin.Wallet.Bip329 (parseLine, Line (..), Record (..), RecordType (..), readLines)
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Labels (AccountImport (..), ImportReport (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData (..))
import Haskoin.Wallet.Workflow.TestUtils
import Numeric.Natural (Natural)
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
  exportImportSpec ctx

-- | The transactions of the history, each by its id, with its label
historyLabels :: Response -> Either String [(String, Text)]
historyLabels (ResponseTxs _ listed) =
  Right [(Text.unpack (txHashToHex (chainTxid status)), txInfoLabel info) | ListedTx info _ <- listed, OnChain status <- [txInfoStatus info]]
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
        (txSignDataLabel . txFileData <$> readTxFile ctx file) `shouldReturn` "Old wallet"

fileSpec :: Ctx -> Spec
fileSpec ctx =
  describe "A label in the file of a transaction" $ do
    it "goes with the transaction to its --output file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.psbt"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file, "--label", "Rent"] []
        (txSignDataLabel . txFileData <$> readTxFile ctx file) `shouldReturn` "Rent"

    it "goes with the transaction to the file of exporttx" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "Rent"] []
        let file = home </> "exported.psbt"
        _ <- answer home ["exporttx", nosig, file] []
        (txSignDataLabel . txFileData <$> readTxFile ctx file) `shouldReturn` "Rent"

    it "comes back from an offline signer with the signed transaction, which sendtx sends with it" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        (txSignDataLabel . txFileData <$> readTxFile ctx (signer </> "signed.psbt")) `shouldReturn` "Rent"
        sent <- txOf =<< answer online ["sendtx", "--input", signer </> "signed.psbt", "--yes"] []
        txInfoLabel sent `shouldBe` "Rent"

    -- The user changes the label while the file is away to be signed
    it "gives way to the label the online computer holds when the signed file comes back" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["label", nosig, "Rent of October"] []
        sent <- txOf =<< answer online ["sendtx", "--input", signer </> "signed.psbt", "--yes"] []
        txInfoLabel sent `shouldBe` "Rent of October"

    it "gives way to the label the online computer holds in the answer of importtx too" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["label", nosig, "Rent of October"] []
        imported <- txOf =<< answer online ["importtx", signer </> "signed.psbt"] []
        txInfoLabel imported `shouldBe` "Rent of October"

-- The payment of the paid chain is at the first receiving address,
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj at /0/0, which the transaction of
-- these tests spends. Its change goes to the first change address, which
-- takes no label. The file names the path of a labelled address, never
-- the address.
addressLabelsSpec :: Ctx -> Spec
addressLabelsSpec ctx =
  describe "The labels of the account's addresses in a transaction file" $ do
    it "go with the transaction to its --output file, by their paths, those that have one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "unsigned.psbt"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        (txFilePathLabels <$> readTxFile ctx file) `shouldReturn` Map.fromList [("/0/0", "Salary")]

    -- The address is labelled after the transaction is prepared
    it "are those of the wallet when exporttx writes the file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "exported.psbt"
        _ <- answer home ["exporttx", nosig, file] []
        (txFilePathLabels <$> readTxFile ctx file) `shouldReturn` Map.fromList [("/0/0", "Salary")]

    -- The address labelled is the one the payment spends from. A signed
    -- input names no key any more, and still the path of its address, in
    -- a field of hw: the label goes with that path.
    it "are those of the wallet when signtx signs a pending transaction, shown and written to its --output file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "signed.psbt"
        signed <- txOf =<< answer home ["signtx", nosig, "--output", file] seedOnce
        addressLabelsOf signed `shouldBe` Map.fromList [(externalAddr 0, "Salary"), (internalAddr 0, "")]
        (txFilePathLabels <$> readTxFile ctx file) `shouldReturn` Map.fromList [("/0/0", "Salary")]

    -- The address is relabelled after the file is written
    it "are shown by reviewtx in place of the wallet's own" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "unsigned.psbt"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        _ <- answer home ["label", textOf (externalAddr 0), "Wages"] []
        reviewed <- txOf =<< answer home ["reviewtx", file] []
        addressLabelsOf reviewed `shouldBe` Map.fromList [(externalAddr 0, "Salary"), (internalAddr 0, "")]

    it "are shown by reviewtx on the offline signer, which has no labels of its own" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["reviewtx", online </> "unsigned.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0\n             \"Salary\"\n"

    -- ESC [ 1 A moves the cursor up a line: a file could rewrite the lines
    -- above the label, the recipient among them
    it "reach the terminal of reviewtx with their control characters written out, so that a file cannot rewrite the screen" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", textOf (externalAddr 0), "x\ESC[1Ay"] []
        let file = home </> "unsigned.psbt"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        run <- hwOnScreen [] home ["reviewtx", file]
        runOut run `shouldSatisfy` Text.isInfixOf "\"x\\x1b[1Ay\""
        runOut run `shouldSatisfy` (not . Text.isInfixOf "\ESC[1A")

    it "are shown by signtx --input on the offline signer, and go on to the signed file" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        addressLabelsOf signed `shouldBe` Map.fromList [(externalAddr 0, "Salary"), (internalAddr 0, "")]
        (txFilePathLabels <$> readTxFile ctx (signer </> "signed.psbt")) `shouldReturn` Map.fromList [("/0/0", "Salary")]

    -- The address is relabelled while the file is away to be signed
    it "give way to the wallet's own when importtx takes the signed file back" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["label", textOf (externalAddr 0), "Wages"] []
        imported <- txOf =<< answer online ["importtx", signer </> "signed.psbt"] []
        addressLabelsOf imported `shouldBe` Map.fromList [(externalAddr 0, "Wages"), (internalAddr 0, "")]

    it "give way to the wallet's own when sendtx --input sends the signed file" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["label", textOf (externalAddr 0), "Wages"] []
        sent <- txOf =<< answer online ["sendtx", "--input", signer </> "signed.psbt", "--yes"] []
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

    it "is a field of coins --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        run <- hw home ["coins", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Label   : \"Salary\"\n"

    it "is left out of coins without --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        run <- hw home ["coins"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "Salary"

    it "gives way to the label of the address in coins, on the line below the address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Wages"] []
        run <- hw home ["coins"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  /0/0\n     \"Wages\"\n"

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
        let file = home </> "unsigned.psbt"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        (txFileInputLabels <$> readTxFile ctx file) `shouldReturn` Map.fromList [(txHash (payment ctx), "Salary")]

    it "ends the address spent from in reviewtx on the offline signer, after the label of the address" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", paidTxid ctx, "Salary"] []
        _ <- answer online ["label", textOf (externalAddr 0), "Wages"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["reviewtx", online </> "unsigned.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0\n             \"Wages\"  from \"Salary\"\n"

    -- The line below the address starts at column 13, under the address,
    -- and wraps before column 80, each new line under the address again
    it "wraps with the label of the address spent from at the edge of the screen, under the address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "This is a very long label for an internal transaction that was done on oct 31. Bloooooooooooooob! Bloooooooooooooooob!"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Label for an address blablabla"] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        Text.lines (runOut run)
          `shouldContain` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  /0/0",
                            "             \"Label for an address blablabla\"  from \"This is a very long label",
                            "             for an internal transaction that was done on oct 31.",
                            "             Bloooooooooooooob! Bloooooooooooooooob!\""
                          ]

    -- The label is given while the file is away
    it "is the wallet's own in the answer of importtx, not the file's" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["label", paidTxid ctx, "Salary"] []
        imported <- txOf =<< answer online ["importtx", signer </> "signed.psbt"] []
        txInfoInputLabels imported `shouldBe` Map.fromList [(txHash (payment ctx), "Salary")]

-- The change of the payment of 20000, 27740, goes to the first change
-- address, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, which takes no label
changeAddressSpec :: Ctx -> Spec
changeAddressSpec ctx =
  describe "A change address" $ do
    it "is said to be one in coins, on the line below the address" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentWithLabel home "Rent"
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        run <- hw home ["coins"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  at 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  /1/0\n     change address\n"

    it "is said to be one by a transaction that spends from it" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentWithLabel home "Rent"
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--fee-rate", "10"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  /1/0\n             change address\n"

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

    it "labels a transaction of the history by the short id of its txid, its first 8 digits" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (labelledOf <$> hwJson home ["label", shortOf (paidTxid ctx), "Salary"] []) `shouldReturn` Right [("main", "Salary")]
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")

    it "labels a pending transaction by the short id of its nosigHash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (labelledOf <$> hwJson home ["label", shortOf nosig, "Rent"] []) `shouldReturn` Right [("main", "Rent")]

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

    it "refuses a short id that matches no transaction of the wallet, as it refuses such a txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["label", "ab000000", "Rent"] [])
          `shouldReturn` Right "The wallet has no transaction ab000000"

shownSpec :: Ctx -> Spec
shownSpec ctx =
  describe "A label as hw shows it" $ do
    -- The payment of 50000 received at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,
    -- the first receiving address
    it "follows the path of the address a payment received is paid at, in txs" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        run <- hw home ["txs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  balance 0.00050000 btc, at /0/0 \"Salary\"\n"

    it "is a field of txs --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        run <- hw home ["txs", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Label    : \"Salary\"\n"

    -- pendingtxs lists every account without --account, so it names the
    -- account first
    it "takes the place of the address a payment pays in pendingtxs" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "Rent"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  account main, \"Rent\"\n"

    it "is shown by label with the transaction, after its account and network" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["label", paidTxid ctx, "Salary"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Account  : main\n  Network  : btc\n  Label    : \"Salary\"\n"

{- The accounts and labels in a file -}

-- | A home synced with the paid chain, its payment labelled "Salary", the
-- address it paid labelled "Wages", a second receiving address labelled
-- "From Bob" and a third one handed out without a label, and the coin of
-- the payment locked
labelledHome :: Ctx -> FilePath -> IO ()
labelledHome ctx home = do
  synced home
  _ <- answer home ["label", paidTxid ctx, "Salary"] []
  _ <- answer home ["label", textOf (externalAddr 0), "Wages"] []
  _ <- answer home ["receive", "--label", "From Bob"] []
  _ <- answer home ["receive"] []
  _ <- answer home ["lockcoin", coinArg (payment ctx) 0] []
  return ()

-- | The lines exportacc writes for that home: the key of main, its two
-- labelled addresses, the payment, 1000 of fee for 50000 received, three
-- blocks deep and so without a height, and its coin, locked
labelledLines :: Ctx -> [Text]
labelledLines ctx =
  [ "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"main\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"purpose\":44}",
    "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"keypath\":\"/0/0\"}",
    "{\"type\":\"addr\",\"ref\":\"1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg\",\"label\":\"From Bob\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"keypath\":\"/0/1\"}",
    "{\"type\":\"tx\",\"ref\":\"" <> txid <> "\",\"label\":\"Salary\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"fee\":1000,\"value\":50000}",
    "{\"type\":\"output\",\"ref\":\"" <> txid <> ":0\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"spendable\":false,\"keypath\":\"/0/0\",\"value\":50000}"
  ]
  where
    txid = Text.pack (paidTxid ctx)

-- | The records of a file hw wrote, in order
recordsOf :: FilePath -> IO [Record]
recordsOf file = do
  content <- TextIO.readFile file
  return [r | (_, line) <- readLines content, Right (LineRecord r) <- [parseLine line]]

-- | What became of each account importacc answered for, by name
accountsImported :: Response -> Either String [(Text, String)]
accountsImported (ResponseImport report) = Right (describe' <$> reportAccounts report)
  where
    describe' (AccountCreated acc _) = (dBAccountName acc, "created")
    describe' (AccountKept acc _) = (dBAccountName acc, "kept")
    describe' (AccountRenamed was acc) = (dBAccountName acc, "renamed from " <> Text.unpack was)
    describe' (AccountRefused name _) = (name, "refused")
accountsImported other = Left (show other)

-- | The addresses an answer of importacc counts as not in the wallet yet,
-- and of them those too far past the last used address to derive
missingAddressesOf :: Response -> Either String (Natural, Natural)
missingAddressesOf (ResponseImport report) = Right (reportAddressesMissing report, reportAddressesTooFar report)
missingAddressesOf other = Left (show other)

exportImportSpec :: Ctx -> Spec
exportImportSpec ctx =
  describe "The accounts and labels in a file of BIP 329" $ do
    it "are written by exportacc, the key of the account first, then its labelled addresses, its labelled transactions and its locked coins" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        labelledHome ctx home
        _ <- answer home ["exportacc", home </> "labels.jsonl"] []
        (Text.lines <$> TextIO.readFile (home </> "labels.jsonl")) `shouldReturn` labelledLines ctx

    it "are written for every account without --account, each key before the labels of its account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        labelledHome ctx home
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "second", "--label", "Savings"] []
        _ <- answer home ["exportacc", home </> "labels.jsonl"] []
        records <- recordsOf (home </> "labels.jsonl")
        [(recordType r, recordLabel r) | r <- records, recordType r `elem` [XpubRecord, AddrRecord]]
          `shouldBe` [(XpubRecord, Just "main"), (AddrRecord, Just "Wages"), (AddrRecord, Just "From Bob"), (XpubRecord, Just "second"), (AddrRecord, Just "Savings")]

    it "include the label of a signed pending transaction, by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "Rent"] []
        txid <- txidOf =<< txOf =<< answer home ["signtx", nosig] seedOnce
        _ <- answer home ["exportacc", home </> "labels.jsonl"] []
        records <- recordsOf (home </> "labels.jsonl")
        [(recordRef r, recordLabel r) | r <- records, recordType r == TxRecord] `shouldBe` [(Text.pack txid, Just "Rent")]

    -- The wallet is lost with its computer. recover finds the account from
    -- the mnemonic, as btc0, with the one address the chain paid. The file
    -- gives the account its name and its labels back, the labelled address
    -- nobody paid among them. The unlabelled one nobody paid is gone, until
    -- receive hands it out again.
    it "are read back by importacc into the account recovered from the mnemonic, give or take the unused addresses without a label" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        labelledHome ctx home
        _ <- answer home ["exportacc", home </> "labels.jsonl"] []
        _ <- answer home ["deleteacc", "main", "btc", "/44'/0'/0'"] []
        _ <- answer home ["recover"] seedOnce
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]
        (accountsImported <$> hwJson home ["importacc", home </> "labels.jsonl"] []) `shouldReturn` Right [("main", "renamed from btc0")]
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", Just "bc560f95")]
        -- Nothing to spend: the coin is locked again
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 1, "From Bob"), (externalAddrsText !! 0, "Wages")]
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Just LockedByUser, False)]

    -- A wallet that never synced: the account is created, its addresses
    -- derived, and the transaction labelled by the origin of its account,
    -- but the coin is not there to lock
    it "are shown by importacc account by account, then the labels and locks set, then what to do for the rest" $
      withChain ctx (paidChain ctx) $ \home _ -> withHome $ \fresh -> do
        labelledHome ctx home
        _ <- answer home ["exportacc", home </> "labels.jsonl"] []
        run <- hw fresh ["importacc", home </> "labels.jsonl"] []
        runOut run
          `shouldBe` Text.unlines
            [ "main  btc  /44'/0'/0'",
              "  created",
              "",
              "Labels set: 2 addresses, 1 transaction",
              "Not in the wallet yet: 1 coin",
              "",
              "Next, sync the wallet, for the addresses, transactions and coins the file labels:",
              "  hw sync",
              "Then import the file again, for the labels it could not place:",
              "  hw importacc " <> Text.pack (home </> "labels.jsonl")
            ]

    it "place what the wallet did not have once it is synced and the file imported again" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        labelledHome ctx home
        _ <- answer home ["exportacc", home </> "labels.jsonl"] []
        _ <- answer home ["deleteacc", "main", "btc", "/44'/0'/0'"] []
        _ <- answer home ["importacc", home </> "labels.jsonl"] []
        _ <- answer home ["sync"] []
        run <- hw home ["importacc", home </> "labels.jsonl"] []
        runOut run `shouldBe` "main  btc  /44'/0'/0'\n  already in the wallet\n\nLabels set: 2 addresses, 1 transaction\nCoins locked: 1\n"
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Just LockedByUser, False)]

    -- As Sparrow writes them: the account named by its origin alone, with
    -- its hardened steps marked h, and the labels of inputs and coins too
    it "are read from a file of another wallet, which names the account by its origin and keeps labels hw does not" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let txid = Text.pack (paidTxid ctx)
        TextIO.writeFile (home </> "sparrow.jsonl") $
          Text.unlines
            [ "{\"type\":\"tx\",\"ref\":\"" <> txid <> "\",\"label\":\"Salary\",\"origin\":\"pkh([bc560f95/44h/0h/0h])\"}",
              "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\"}",
              "{\"type\":\"output\",\"ref\":\"" <> txid <> ":0\",\"label\":\"Salary (received)\",\"spendable\":false}",
              "{\"type\":\"input\",\"ref\":\"" <> txid <> ":0\",\"label\":\"Payer (input)\"}"
            ]
        run <- hw home ["importacc", home </> "sparrow.jsonl"] []
        runOut run `shouldBe` "Labels set: 1 address, 1 transaction\nCoins locked: 1\nLeft aside: 1 record hw keeps no label for\n"
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 0, "Wages")]
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")

    -- The address /0/1001 of main, with /0/0 used: one past the thousand
    -- that hw derives on the word of a file
    it "say which addresses hw would not derive, over a thousand past the last used one of their chain" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        TextIO.writeFile (home </> "far.jsonl") "{\"type\":\"addr\",\"ref\":\"1BUtUkmTvZHpz7wPeEJNTX4VLTgLNxKNt2\",\"label\":\"Far\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"keypath\":\"/0/1001\"}\n"
        run <- hw home ["importacc", home </> "far.jsonl"] []
        runOut run
          `shouldBe` Text.unlines
            [ "Labels set: none",
              "Not in the wallet yet: 1 address",
              "  Of them, 1 address lies over a thousand past the last used address of its",
              "  chain, further than hw generates on the word of a file.",
              "",
              "Next, sync the wallet, for the addresses, transactions and coins the file labels:",
              "  hw sync",
              "Then import the file again, for the labels it could not place:",
              "  hw importacc " <> Text.pack (home </> "far.jsonl")
            ]
        (missingAddressesOf <$> hwJson home ["importacc", home </> "far.jsonl"] []) `shouldReturn` Right (1, 1)
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

    -- The file of another wallet names no network for its key
    it "create the account of a key of another wallet on the --network given, and refuse it without a network" $
      withHome $ \home -> do
        setDefaultNetwork home Nothing
        TextIO.writeFile (home </> "other.jsonl") ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"BIP39\"}\n")
        run <- hw home ["importacc", home </> "other.jsonl"] []
        runOut run `shouldBe` "BIP39\n  not imported: Give the --network, or a default-network in the configuration file\n\nLabels set: none\n"
        (accountsImported <$> hwJson home ["importacc", home </> "other.jsonl", "--network", "btc"] []) `shouldReturn` Right [("BIP39", "created")]
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("BIP39", Nothing)]
