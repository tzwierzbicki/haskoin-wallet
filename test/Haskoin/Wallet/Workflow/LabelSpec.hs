{-# LANGUAGE OverloadedStrings #-}

-- | A transaction labelled by the user: by preparetx, preparesendall and
-- preparesweep, carried in its file to the signer and back, kept through
-- sending and the sync, taken by the replacement bumpfee prepares, changed
-- by label, and shown by txs and pendingtxs. A label is a portfolio's:
-- a transaction between two portfolios gets it in both. And the labels of
-- the portfolio's addresses, of either type, carried in the file for the
-- signer to show, and of an address two test networks share, on the
-- network named. Last, the portfolios with their labels and locks in the
-- BIP 329 file exportwallet writes and importwallet reads, from hw or from
-- another wallet.
module Haskoin.Wallet.Workflow.LabelSpec (spec) where

import Data.IORef (modifyIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Haskoin
import Haskoin.Wallet.Bip329 (Line (..), Record (..), RecordType (..), parseLine, readLines)
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..))
import Haskoin.Wallet.Database.Portfolio (Place)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (PoolPath, legacyPath)
import Haskoin.Wallet.Labels (ImportReport (..))
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
  addressLabelSpec
  exportImportSpec ctx

-- | The transactions of the history, each by its id, with its label, the
-- sections of the listing one after the other
historyLabels :: Response -> Either String [(String, Text)]
historyLabels (ResponseTxs groups) =
  Right [(Text.unpack (txHashToHex (chainTxid status)), txInfoLabel info) | g <- groups, ListedTx info _ <- groupRows g, OnChain status <- [txInfoStatus info]]
historyLabels other = Left (show other)

-- | The answer of label for a transaction: each portfolio that holds it,
-- by where it is, with the label of the transaction
labelledOf :: Response -> Either String [(Place, Text)]
labelledOf (ResponseLabelledTxs held) = Right [(place, txInfoLabel info) | (place, info) <- held]
labelledOf other = Left (show other)

-- | The path of the first receiving address of the bip44 account, /0/0,
-- as the labels of a transaction file name it
firstReceiving :: PoolPath
firstReceiving = legacyPath (Deriv :/ 0 :/ 0)

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
  txidOf =<< txOf =<< answer home ["broadcast", nosig, "--yes"] []

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
        newTestPortfolio home
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

    it "comes back from an offline signer with the signed transaction, which broadcast sends with it" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        (txSignDataLabel . txFileData <$> readTxFile ctx (signer </> "signed.psbt")) `shouldReturn` "Rent"
        sent <- txOf =<< answer online ["broadcast", "--input", signer </> "signed.psbt", "--yes"] []
        txInfoLabel sent `shouldBe` "Rent"

    -- The user changes the label while the file is away to be signed
    it "gives way to the label the online computer holds when the signed file comes back" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["label", nosig, "Rent of October"] []
        sent <- txOf =<< answer online ["broadcast", "--input", signer </> "signed.psbt", "--yes"] []
        txInfoLabel sent `shouldBe` "Rent of October"

    it "gives way to the label the online computer holds in the answer of importtx too" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10", "--label", "Rent"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["label", nosig, "Rent of October"] []
        imported <- txOf =<< answer online ["importtx", signer </> "signed.psbt"] []
        txInfoLabel imported `shouldBe` "Rent of October"

-- The payment of the paid chain is at the first receiving address of the
-- bip44 account of portfolio0 on btc, 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj
-- at /0/0, which the transaction of these tests spends. Its change goes to
-- the first change address, which takes no label. The file names the path
-- of a labelled address, with the type of its account, never the address.
addressLabelsSpec :: Ctx -> Spec
addressLabelsSpec ctx =
  describe "The labels of the portfolio's addresses in a transaction file" $ do
    it "go with the transaction to its --output file, by their paths, those that have one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "unsigned.psbt"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        (txFilePathLabels <$> readTxFile ctx file) `shouldReturn` Map.fromList [(firstReceiving, "Salary")]

    -- The address is labelled after the transaction is prepared
    it "are those of the wallet when exporttx writes the file" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "exported.psbt"
        _ <- answer home ["exporttx", nosig, file] []
        (txFilePathLabels <$> readTxFile ctx file) `shouldReturn` Map.fromList [(firstReceiving, "Salary")]

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
        (txFilePathLabels <$> readTxFile ctx file) `shouldReturn` Map.fromList [(firstReceiving, "Salary")]

    -- The address is relabelled after the file is written
    it "are shown by show in place of the wallet's own" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", textOf (externalAddr 0), "Salary"] []
        let file = home </> "unsigned.psbt"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        _ <- answer home ["label", textOf (externalAddr 0), "Wages"] []
        reviewed <- txOf =<< answer home ["show", file] []
        addressLabelsOf reviewed `shouldBe` Map.fromList [(externalAddr 0, "Salary"), (internalAddr 0, "")]

    it "are shown by show on the offline signer, which has no labels of its own" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["show", online </> "unsigned.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0\n             \"Salary\"\n"

    -- ESC [ 1 A moves the cursor up a line: a file could rewrite the lines
    -- above the label, the recipient among them
    it "reach the terminal of show with their control characters written out, so that a file cannot rewrite the screen" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", textOf (externalAddr 0), "x\ESC[1Ay"] []
        let file = home </> "unsigned.psbt"
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", file] []
        run <- hwOnScreen [] home ["show", file]
        runOut run `shouldSatisfy` Text.isInfixOf "\"x\\x1b[1Ay\""
        runOut run `shouldSatisfy` (not . Text.isInfixOf "\ESC[1A")

    it "are shown by signtx --input on the offline signer, and go on to the signed file" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        addressLabelsOf signed `shouldBe` Map.fromList [(externalAddr 0, "Salary"), (internalAddr 0, "")]
        (txFilePathLabels <$> readTxFile ctx (signer </> "signed.psbt")) `shouldReturn` Map.fromList [(firstReceiving, "Salary")]

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

    it "give way to the wallet's own when broadcast --input sends the signed file" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", textOf (externalAddr 0), "Salary"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        _ <- answer online ["label", textOf (externalAddr 0), "Wages"] []
        sent <- txOf =<< answer online ["broadcast", "--input", signer </> "signed.psbt", "--yes"] []
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

    it "gives way to the label of the address in coins, on the line of the address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Wages"] []
        run <- hw home ["coins"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  at 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  bip44/0/0  \"Wages\"\n"

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

    it "ends the address spent from in show on the offline signer, after the label of the address" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["label", paidTxid ctx, "Salary"] []
        _ <- answer online ["label", textOf (externalAddr 0), "Wages"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["show", online </> "unsigned.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0\n             \"Wages\"  from \"Salary\"\n"

    -- The line below the address starts at column 13, under the address,
    -- and wraps before column 80, each new line under the address again
    it "wraps with the label of the address spent from at the edge of the screen, under the address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "This is a very long label for an internal transaction that was done on oct 31. Bloooooooooooooob! Bloooooooooooooooob!"] []
        _ <- answer home ["label", textOf (externalAddr 0), "Label for an address blablabla"] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        Text.lines (runOut run)
          `shouldContain` [ "  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
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
    it "is said to be one in coins, on the line of the address" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentWithLabel home "Rent"
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        run <- hw home ["coins"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  at 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  bip44/1/0  change address\n"

    it "is said to be one by a transaction that spends from it" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- sentWithLabel home "Rent"
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--fee-rate", "10"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0\n             change address\n"

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
        replacement <- txidOf =<< txOf =<< answer home ["broadcast", nosig, "--yes"] []
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
        (labelledOf <$> hwJson home ["label", nosig, "Rent"] []) `shouldReturn` Right [(mainPlace, "Rent")]
        (txInfoLabel <$> onePendingTx home) `shouldReturn` "Rent"

    it "labels a pending transaction by its txid once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        (labelledOf <$> hwJson home ["label", txid, "Rent"] []) `shouldReturn` Right [(mainPlace, "Rent")]

    it "labels a transaction of the history by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (labelledOf <$> hwJson home ["label", paidTxid ctx, "Salary"] []) `shouldReturn` Right [(mainPlace, "Salary")]
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")

    it "labels a transaction of the history by the short id of its txid, its first 8 digits" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (labelledOf <$> hwJson home ["label", shortOf (paidTxid ctx), "Salary"] []) `shouldReturn` Right [(mainPlace, "Salary")]
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")

    it "labels a pending transaction by the short id of its nosigHash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (labelledOf <$> hwJson home ["label", shortOf nosig, "Rent"] []) `shouldReturn` Right [(mainPlace, "Rent")]

    -- The payment pays 50000 to portfolio0 and 30000 to portfolio1
    it "labels a transaction of the history in every portfolio that has it" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        (labelledOf <$> hwJson home ["label", txidArg (twoPortfoliosTx ctx), "Gift"] []) `shouldReturn` Right [(mainPlace, "Gift"), (portfolio1Place, "Gift")]

    it "labels a transaction of the history of portfolio1 there, as txs of portfolio1 shows" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        _ <- answer home ["label", txidArg (twoPortfoliosTx ctx), "Gift"] []
        switchTo home 0 1
        (fmap (lookup (txidArg (twoPortfoliosTx ctx))) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Gift")

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
    -- the first receiving address of the bip44 account. The bip84 account
    -- of the portfolio has an address at /0/0 too, so the path names its
    -- type.
    it "follows the path of the address a payment received is paid at, with the type of its account, in txs" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        run <- hw home ["txs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  balance 0.00050000 btc, at bip44/0/0 \"Salary\"\n"

    it "is a field of txs --verbose" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", paidTxid ctx, "Salary"] []
        run <- hw home ["txs", "--verbose"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Label    : \"Salary\"\n"

    -- pendingtxs names where each transaction is first: portfolio0 of
    -- wallet0, named without its wallet, on btc
    it "takes the place of the address a payment pays in pendingtxs" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "Rent"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  portfolio0 btc, \"Rent\"\n"

    -- The payment pays the bip44 account alone
    it "is shown by label with the transaction, after its portfolio, its network and the types it involves" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["label", paidTxid ctx, "Salary"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0\n  Network  : btc\n  Types    : bip44\n  Label    : \"Salary\"\n"

{- The label of an address -}

-- The first receiving address of the bip44 account of portfolio0 on the
-- test networks of Bitcoin, mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih, is the same
-- on btctest and on btctest4: they share the key of m/44'/1'/0'.
addressLabelSpec :: Spec
addressLabelSpec =
  describe "label for an address" $ do
    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc is the first receiving
    -- address of the bip84 account of portfolio0 on btc
    it "labels an address of the bip84 account of the portfolio, as addresses of that account shows it" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--type", "bip84"] []
        _ <- answer home ["label", "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", "Savings"] []
        (addressesOf <$> hwJson home ["addresses", "--type", "bip84"] []) `shouldReturn` Right [("bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", "Savings")]

    it "is refused for an address two test networks share without --network, naming the networks" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        (errorOf <$> hwJson home ["label", Text.unpack testnetExternalAddrText, "Faucet"] [])
          `shouldReturn` Right "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4: give the --network"

    it "labels an address two test networks share on the --network named alone" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        _ <- answer home ["label", Text.unpack testnetExternalAddrText, "Faucet", "--network", "btctest4"] []
        (addressesOf <$> hwJson home ["addresses", "--network", "btctest4"] []) `shouldReturn` Right [(testnetExternalAddrText, "Faucet")]
        (addressesOf <$> hwJson home ["addresses", "--network", "btctest"] []) `shouldReturn` Right [(testnetExternalAddrText, "")]

{- The portfolios and labels in a file -}

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

-- | The lines exportwallet writes for that home: the key of the bip44
-- account of portfolio0 on btc, its two labelled addresses, the payment,
-- 1000 of fee for 50000 received, three blocks deep and so without a
-- height, and its coin, locked. Then the keys of the eleven other accounts
-- of the portfolio, which have no label: the bip84 account on btc, the
-- bip44 one on bch, and those of the test networks, which share the keys
-- of m/44'/1'/0' and m/84'/1'/0'.
labelledLines :: Ctx -> [Text]
labelledLines ctx =
  [ "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"purpose\":44}",
    "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"keypath\":\"/0/0\"}",
    "{\"type\":\"addr\",\"ref\":\"1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg\",\"label\":\"From Bob\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"keypath\":\"/0/1\"}",
    "{\"type\":\"tx\",\"ref\":\"" <> txid <> "\",\"label\":\"Salary\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"fee\":1000,\"value\":50000}",
    "{\"type\":\"output\",\"ref\":\"" <> txid <> ":0\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"spendable\":false,\"keypath\":\"/0/0\",\"value\":50000}",
    keyLine testSegwitAccountXPub "wpkh" "84'/0'" "btc" 84,
    keyLine testAccountXPubBch "pkh" "44'/145'" "bch" 44,
    keyLine testAccountXPubTest "pkh" "44'/1'" "btctest" 44,
    keyLine testSegwitAccountXPubTest "wpkh" "84'/1'" "btctest" 84,
    keyLine testAccountXPubTest "pkh" "44'/1'" "btctest4" 44,
    keyLine testSegwitAccountXPubTest "wpkh" "84'/1'" "btctest4" 84,
    keyLine testAccountXPubTest "pkh" "44'/1'" "btcreg" 44,
    keyLine testSegwitAccountXPubTest "wpkh" "84'/1'" "btcreg" 84,
    keyLine testAccountXPubTest "pkh" "44'/1'" "bchtest" 44,
    keyLine testAccountXPubTest "pkh" "44'/1'" "bchtest4" 44,
    keyLine testAccountXPubTest "pkh" "44'/1'" "bchreg" 44
  ]
  where
    txid = Text.pack (paidTxid ctx)
    -- The record of the key of an account at index 0 of the test wallet,
    -- of the script, purpose and coin type given, on the network
    keyLine :: Text -> Text -> Text -> Text -> Int -> Text
    keyLine key script purposeAndCoin net purpose =
      "{\"type\":\"xpub\",\"ref\":\"" <> key <> "\",\"origin\":\"" <> script <> "([bc560f95/" <> purposeAndCoin <> "/0'])\",\"network\":\"" <> net <> "\",\"purpose\":" <> Text.pack (show purpose) <> "}"

-- | The records of a file hw wrote, in order
recordsOf :: FilePath -> IO [Record]
recordsOf file = do
  content <- TextIO.readFile file
  return [r | (_, line) <- readLines content, Right (LineRecord r) <- [parseLine line]]

-- | The addresses an answer of importwallet counts as not in the wallet
-- yet, and of them those too far past the last used address to derive
missingAddressesOf :: Response -> Either String (Natural, Natural)
missingAddressesOf (ResponseImport report _) = Right (reportAddressesMissing report, reportAddressesTooFar report)
missingAddressesOf other = Left (show other)

exportImportSpec :: Ctx -> Spec
exportImportSpec ctx =
  describe "The portfolios and labels in a file of BIP 329" $ do
    it "are written by exportwallet, the key of each account first, then its labelled addresses, its labelled transactions and its locked coins" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        labelledHome ctx home
        _ <- answer home ["exportwallet", home </> "labels.jsonl"] []
        (Text.lines <$> TextIO.readFile (home </> "labels.jsonl")) `shouldReturn` labelledLines ctx

    -- The keys and the addresses on btc: those of the bip44 and the bip84
    -- account of portfolio0, then those of portfolio1, m/44'/0'/1' and
    -- m/84'/0'/1'
    it "are written for every portfolio of the wallet without --portfolio, each key before the labels of its account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        labelledHome ctx home
        newTestPortfolio home
        switchTo home 0 1
        _ <- answer home ["receive", "--label", "Savings"] []
        switchTo home 0 0
        _ <- answer home ["exportwallet", home </> "labels.jsonl"] []
        records <- recordsOf (home </> "labels.jsonl")
        [(recordType r, recordRef r) | r <- records, recordNetwork r == Just "btc", recordType r `elem` [XpubRecord, AddrRecord]]
          `shouldBe` [ (XpubRecord, testAccountXPub),
                       (AddrRecord, "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"),
                       (AddrRecord, "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"),
                       (XpubRecord, testSegwitAccountXPub),
                       (XpubRecord, testAccountXPub1),
                       (AddrRecord, "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo"),
                       (XpubRecord, testSegwitAccountXPub1)
                     ]

    it "include the label of a signed pending transaction, by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "Rent"] []
        txid <- txidOf =<< txOf =<< answer home ["signtx", nosig] seedOnce
        _ <- answer home ["exportwallet", home </> "labels.jsonl"] []
        records <- recordsOf (home </> "labels.jsonl")
        [(recordRef r, recordLabel r) | r <- records, recordType r == TxRecord] `shouldBe` [(Text.pack txid, Just "Rent")]

    -- The wallet is lost with its computer: deletewallet stands for that.
    -- recover finds portfolio0 from the mnemonic, with the one address the
    -- chain paid. The file gives the portfolio its labels back, the
    -- labelled address nobody paid among them, and adds no account: the
    -- portfolio recover made is whole. The unlabelled address nobody paid
    -- is gone, until receive hands it out again.
    it "are read back by importwallet into the portfolio recovered from the mnemonic, give or take the unused addresses without a label" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        labelledHome ctx home
        _ <- answer home ["exportwallet", home </> "labels.jsonl"] []
        _ <- answer home ["deletewallet", "bc560f95", "--wallet", "0"] []
        _ <- answer home ["recover"] seedOnce
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]
        (addedAccountsOf <$> hwJson home ["importwallet", home </> "labels.jsonl"] []) `shouldReturn` Right []
        (fingerprintsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, "bc560f95")]
        -- Nothing to spend: the coin is locked again
        (spendableOn btc <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, 0)]
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 1, "From Bob"), (externalAddrsText !! 0, "Wages")]
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Just LockedByUser, False)]

    -- A wallet that never synced: the wallet and its portfolio are
    -- created, the addresses derived, and the transaction labelled by the
    -- origin of its record, but the coin is not there to lock. The
    -- portfolio is the first of the database, which makes it active.
    it "are shown by importwallet wallet by wallet, then the labels and locks set, then what to do for the rest" $
      withChain ctx (paidChain ctx) $ \home _ -> withHome $ \fresh -> do
        labelledHome ctx home
        _ <- answer home ["exportwallet", home </> "labels.jsonl"] []
        run <- hw fresh ["importwallet", home </> "labels.jsonl"] []
        runOut run
          `shouldBe` Text.unlines
            [ "wallet0  bc560f95  created",
              "  portfolio0  created, 12 accounts added",
              "",
              "portfolio0 of wallet0 is the active portfolio: the commands work in it.",
              "",
              "Labels set: 2 addresses, 1 transaction",
              "Not in the wallet yet: 1 coin",
              "",
              "Next, sync the wallet, for the addresses, transactions and coins the file labels:",
              "  hw sync",
              "Then import the file again, for the labels it could not place:",
              "  hw importwallet " <> Text.pack (home </> "labels.jsonl")
            ]

    it "place what the wallet did not have once it is synced and the file imported again" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        labelledHome ctx home
        _ <- answer home ["exportwallet", home </> "labels.jsonl"] []
        _ <- answer home ["deletewallet", "bc560f95", "--wallet", "0"] []
        _ <- answer home ["importwallet", home </> "labels.jsonl"] []
        _ <- answer home ["sync"] []
        run <- hw home ["importwallet", home </> "labels.jsonl"] []
        runOut run `shouldBe` "wallet0  bc560f95  already in the database\n  portfolio0  nothing to add\n\nLabels set: 2 addresses, 1 transaction\nCoins locked: 1\n"
        (coinsShown <$> hwJson home ["coins"] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Just LockedByUser, False)]

    -- As another wallet writes them: the account named by its origin alone,
    -- with its hardened steps marked h, no network, and the labels of
    -- inputs and coins too
    it "are read from a file of another wallet, which names the account by its origin and keeps labels hw does not" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let txid = Text.pack (paidTxid ctx)
        TextIO.writeFile (home </> "other.jsonl") $
          Text.unlines
            [ "{\"type\":\"tx\",\"ref\":\"" <> txid <> "\",\"label\":\"Salary\",\"origin\":\"pkh([bc560f95/44h/0h/0h])\"}",
              "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\"}",
              "{\"type\":\"output\",\"ref\":\"" <> txid <> ":0\",\"label\":\"Salary (received)\",\"spendable\":false}",
              "{\"type\":\"input\",\"ref\":\"" <> txid <> ":0\",\"label\":\"Payer (input)\"}"
            ]
        run <- hw home ["importwallet", home </> "other.jsonl"] []
        runOut run `shouldBe` "Labels set: 1 address, 1 transaction\nCoins locked: 1\nLeft aside: 1 record hw keeps no label for\n"
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 0, "Wages")]
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")

    -- The address /0/1001 of the bip44 account on btc, with /0/0 used: one
    -- past the thousand that hw derives on the word of a file
    it "say which addresses hw would not derive, over a thousand past the last used one of their chain" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        TextIO.writeFile (home </> "far.jsonl") "{\"type\":\"addr\",\"ref\":\"1BUtUkmTvZHpz7wPeEJNTX4VLTgLNxKNt2\",\"label\":\"Far\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"keypath\":\"/0/1001\"}\n"
        run <- hw home ["importwallet", home </> "far.jsonl"] []
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
              "  hw importwallet " <> Text.pack (home </> "far.jsonl")
            ]
        (missingAddressesOf <$> hwJson home ["importwallet", home </> "far.jsonl"] []) `shouldReturn` Right (1, 1)
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

    -- The file of another wallet names no network and no purpose for its
    -- key: it is one account, of no portfolio
    it "leave aside the key of another wallet, which names no network, and say why" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "other.jsonl") ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"BIP39\"}\n")
        run <- hw home ["importwallet", home </> "other.jsonl"] []
        runOut run
          `shouldBe` Text.unlines
            [ "xpub6CHK45s4...",
              "  not imported: The record names no network. hw imports the wallets exportwallet writes, not a key alone: a key is one account, and does not say which portfolio it belongs to",
              "",
              "Labels set: none"
            ]
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    -- The payment of the paid chain pays portfolio0 alone, and the record
    -- names its network, btc, and no origin
    it "label a transaction whose record names a network and no origin in the portfolio that has it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        syncedTwoPortfolios home
        TextIO.writeFile (home </> "tx.jsonl") (noOriginRecord ctx)
        (labelsSetOf <$> hwJson home ["importwallet", home </> "tx.jsonl"] []) `shouldReturn` Right (0, 1)
        (fmap (lookup (paidTxid ctx)) . historyLabels <$> hwJson home ["txs"] []) `shouldReturn` Right (Just "Salary")

    it "label such a transaction in no portfolio of the network that does not have it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        syncedTwoPortfolios home
        TextIO.writeFile (home </> "tx.jsonl") (noOriginRecord ctx)
        _ <- answer home ["importwallet", home </> "tx.jsonl"] []
        _ <- answer home ["exportwallet", "--portfolio", "1", home </> "portfolio1.jsonl"] []
        records <- recordsOf (home </> "portfolio1.jsonl")
        [recordRef r | r <- records, recordType r == TxRecord] `shouldBe` []

-- | A record that labels the payment of the paid chain "Salary" on btc,
-- without an origin
noOriginRecord :: Ctx -> Text
noOriginRecord ctx = "{\"type\":\"tx\",\"ref\":\"" <> Text.pack (paidTxid ctx) <> "\",\"label\":\"Salary\",\"network\":\"btc\"}\n"
