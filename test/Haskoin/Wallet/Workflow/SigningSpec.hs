{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A payment signed by signtx and sent by sendtx, on the same computer
-- or offline: the --input and --output files, what the signing commands
-- refuse before they ask for the mnemonic, and the question sendtx asks
-- before it sends.
module Haskoin.Wallet.Workflow.SigningSpec (spec) where

import Control.Monad (forM_, void)
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData (..), isSigned)
import Haskoin.Wallet.Workflow.TestUtils
import Numeric.Natural (Natural)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  signingSpec ctx
  inputFileSpec ctx
  confirmationSpec ctx

{- Signing -}

-- | Import the account main of the test wallet in a home, from a document
-- without the fingerprint of its seed, as hw wrote them before it recorded
-- fingerprints
importWithoutFingerprint :: FilePath -> IO ()
importWithoutFingerprint home = do
  Json.encodeFile (home </> "main.json") $
    Json.object ["xpubkey" Json..= testAccountXPub, "network" Json..= ("btc" :: Text), "name" Json..= ("main" :: Text)]
  void $ answer home ["importacc", home </> "main.json"] []

-- | What the payment of 20000 satoshis spends and pays, as the store sees
-- it once sent: the coin of the payment to the wallet, and 27740 of change
paymentSent :: Ctx -> ([OutPoint], [(Word64, Either String Address)])
paymentSent ctx = ([OutPoint (txHash (payment ctx)) 0], [(20000, Right (otherAddr 0)), (27740, Right (internalAddr 0))])

-- | What deletetx answers: the nosigHash and the txid of the transaction
-- deleted, as typed, and how many coins and change addresses it freed
deletedOf :: Response -> Either String (String, Maybe String, Natural, Natural)
deletedOf (ResponseDeleteTx nosigH txidM coins addrs) = Right (hex nosigH, hex <$> txidM, coins, addrs)
  where
    hex = Text.unpack . txHashToHex
deletedOf other = Left (show other)

-- 20000 satoshis are paid at 10 satoshis a byte: the transaction of one
-- input and two outputs is 226 bytes, so the fee is 2260, and the change
-- 50000 - 20000 - 2260 = 27740.
signingSpec :: Ctx -> Spec
signingSpec ctx = do
  describe "A payment signed on the same computer" $ do
    -- signtx keeps the signed version in the wallet, which sendtx sends: it
    -- sends no transaction that is not signed
    it "is prepared, signed and sent by its nosigHash, and confirmed by a sync" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        sent <- txOf =<< answer home ["sendtx", nosig, "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        modifyIORef chain mine
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 27740)]

    -- The answer of sendtx reads as once sent: what the wallet holds tells
    -- whether it marked the transaction online
    it "is sent by the txid signtx shows, and marked online" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        txid <- txidOf =<< txOf =<< answer home ["signtx", nosig] seedOnce
        _ <- answer home ["sendtx", txid, "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        onePendingState home `shouldReturn` Just (True, True)

    -- It held the coin of 50000 and the change address
    it "is deleted by its txid once signed, and deletetx answers with both its hashes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        (deletedOf <$> hwJson home ["deletetx", txid] []) `shouldReturn` Right (nosig, Just txid, 1, 1)
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- As pendingtxs showed it, even when named by its nosigHash
    it "is shown by deletetx by its txid once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home ["deletetx", nosig] []
        runOut run `shouldSatisfy` Text.isInfixOf ("Deleted  : " <> Text.pack txid)

    -- The payment spends one coin and pays its change to one change address
    it "says what deletetx freed: the coins it spent, and the change addresses it paid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["deletetx", nosig] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Freed    : 1 coin, 1 change address\n"

    it "is signed from the parts of the mnemonic with signtx --parts" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        signed <- txOf =<< answer home ["signtx", nosig, "--parts", "2"] partsOnce
        pendingState signed `shouldBe` Just (True, False)

    -- The wallet has two accounts of the test seed, and the payment is
    -- pending in main. Without --account, hw asked which account to use.
    it "is signed by the account that holds it, without --account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "main", "--fee-rate", "10"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)

    -- The mnemonic of second would be typed for nothing: its key cannot
    -- sign what main holds
    it "is refused an --account that does not hold it, before the mnemonic is asked" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "main", "--fee-rate", "10"] []
        run <- hw home ["signtx", nosig, "--account", "second"] seedOnce
        run `shouldSatisfy` printed "The transaction is pending in the account main, not in second"
        run `shouldNotSatisfy` askedMnemonic

    -- The account came from a document without the fingerprint of its
    -- seed, which signtx records. The answer read the account before, and
    -- showed none.
    it "shows the account with the seed signtx recorded" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        importWithoutFingerprint home
        _ <- answer home ["receive"] []
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (fmap dBAccountFingerprint . accountOf <$> hwJson home ["signtx", nosig] seedOnce)
          `shouldReturn` Right (Just "bc560f95")

  -- The signer holds the account and asks for its mnemonic; the online
  -- computer holds the same account, imported, and talks to the store
  describe "A payment signed offline" $ do
    it "is prepared online, signed offline, sent online, and confirmed by a sync" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        prepared <- txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        pendingState prepared `shouldBe` Just (False, False)
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]
        summaryFee (txInfoSummary prepared) `shouldBe` 2260
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        sent <- txOf =<< answer online ["sendtx", "--input", signer </> "signed.json", "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        modifyIORef chain mine
        (accountsOf <$> hwJson online ["sync"] []) `shouldReturn` Right [("main", 0, 27740)]
        (txsOf <$> hwJson online ["pendingtxs"] []) `shouldReturn` Right []
        (txsOf <$> hwJson online ["txs"] []) `shouldReturn` Right [(TxDebit, -22260, Just 1), (TxCredit, 50000, Just 4)]
        (addressesOf <$> hwJson online ["addresses", "--change"] []) `shouldReturn` Right [(internalAddrsText !! 0, "")]

    -- Prepared without a file, the transaction is written out by exporttx
    it "is exported by exporttx, signed offline, imported back by importtx, and sent by its nosigHash" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        answer online ["exporttx", nosig, online </> "unsigned.json"] [] `shouldReturn` ResponseFile (online </> "unsigned.json")
        (fmap txFileNetwork <$> Json.decodeFileStrict (online </> "unsigned.json")) `shouldReturn` Just (Just btc)
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        imported <- txOf =<< answer online ["importtx", signer </> "signed.json"] []
        pendingState imported `shouldBe` Just (True, False)
        sent <- txOf =<< answer online ["sendtx", nosig, "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]

    -- Both computers have a second account of the test wallet. The file
    -- names neither the account nor main, the account of the payment: the
    -- keys of main derive the addresses it spends, and the online wallet
    -- holds it pending.
    it "is reviewed, signed and imported back without --account, by wallets of several accounts" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer signer ["createacc", "second"] seedTwice
        _ <- answer signer ["exportacc", "--account", "second", signer </> "second.json"] []
        _ <- answer online ["importacc", signer </> "second.json"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "main", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        (fmap dBAccountName . accountOf <$> hwJson signer ["reviewtx", online </> "unsigned.json"] []) `shouldReturn` Right "main"
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        (fmap dBAccountName . accountOf <$> hwJson online ["importtx", signer </> "signed.json"] []) `shouldReturn` Right "main"

    -- The signer has no coins: the file carries the transactions the
    -- payment spends, and the paths of the change
    it "is shown by reviewtx from its file, as the signer reads it before signing, and not kept" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        reviewed <- txOf =<< answer signer ["reviewtx", online </> "unsigned.json"] []
        paidToOthers reviewed `shouldBe` Map.fromList [(otherAddr 0, 20000)]
        paidToWallet reviewed `shouldBe` Map.fromList [(internalAddr 0, 27740)]
        summaryFee (txInfoSummary reviewed) `shouldBe` 2260
        (txsOf <$> hwJson signer ["pendingtxs"] []) `shouldReturn` Right []

    -- The payment is from main
    it "is refused by reviewtx and importtx for an --account the transaction does not involve" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        Json.encodeFile (home </> "payment.json") (unsignedPayment ctx)
        forM_ ["reviewtx", "importtx"] $ \command ->
          (errorOf <$> hwJson home [command, home </> "payment.json", "--account", "second"] [])
            `shouldReturn` Right "The transaction does not involve the account second"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- The online wallet has the account from a document without the
    -- fingerprint of its seed. bc560f95 is the fingerprint of the test seed.
    it "names the seed in the signed file, which the online wallet learns as it sends it" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        createMain signer
        importWithoutFingerprint online
        _ <- answer online ["receive"] []
        _ <- answer online ["sync"] []
        (fingerprintsOf <$> hwJson online ["accounts"] []) `shouldReturn` Right [("main", Nothing)]
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["sendtx", "--input", signer </> "signed.json", "--yes"] []
        (fingerprintsOf <$> hwJson online ["accounts"] []) `shouldReturn` Right [("main", Just "bc560f95")]

{- The files of signtx, signsweep and sendtx -}

-- | A transaction of the test wallet, unsigned, as a sweep of a coin it
-- does not hold leaves it
unsignedSweep :: Ctx -> TxSignData
unsignedSweep ctx = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 1000)]) [] [] [] (Just 1) Nothing []

-- | A payment of the test wallet to another wallet, unsigned
unsignedPayment :: Ctx -> TxSignData
unsignedPayment ctx =
  TxSignData (spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 40000)]) [funding] [Deriv :/ 0 :/ 0] [] Nothing Nothing []
  where
    funding = fundingTx ctx 1 [(externalAddr 0, 50000)]

-- | The same payment signed by the test wallet, which another wallet holds
signedPayment :: Ctx -> TxSignData
signedPayment ctx = signedByWallet ctx (unsignedPayment ctx)

-- | The files of signtx, signsweep and sendtx: what they refuse, and the
-- --input file replaced by the signed transaction
inputFileSpec :: Ctx -> Spec
inputFileSpec ctx = do
  describe "sendtx from a file" $ do
    it "needs a txid or a file" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["sendtx"] []) `shouldReturn` Right "Give the TXID of a pending transaction or an --input file"

    it "takes a txid or a file, not both" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["sendtx", Text.unpack (txHashToHex (fakeTxHash 1)), "--input", home </> "sweep.json"] [])
          `shouldReturn` Right "Can not specify both a TXID and an --input file"

    it "refuses a missing file" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["sendtx", "--input", home </> "missing.json"] [])
          `shouldReturn` Right (noSuchFile (home </> "missing.json"))

    it "refuses an unsigned transaction" $
      withHome $ \home -> do
        Json.encodeFile (home </> "sweep.json") (unsignedSweep ctx)
        (errorOf <$> hwJson home ["sendtx", "--input", home </> "sweep.json"] [])
          `shouldReturn` Right "The transaction is not signed"

    -- The file is written as by an earlier hw, without the network
    it "refuses the file of an earlier hw, which names no network, for a transaction the wallet does not hold" $
      withHome $ \home -> do
        Json.encodeFile (home </> "payment.json") (signedPayment ctx)
        (errorOf <$> hwJson home ["sendtx", "--input", home </> "payment.json"] [])
          `shouldReturn` Right "The file names no network, as those of an earlier hw: prepare the transaction again"

  describe "signtx and signsweep" $ do
    let nosig = Text.unpack (txHashToHex (fakeTxHash 1))
    it "need a nosigHash or an --input file, not both" $
      withHome $ \home ->
        forM_ ["signtx", "signsweep"] $ \command -> do
          (errorOf <$> hwJson home [command] [])
            `shouldReturn` Right "Give the NOSIGHASH of a pending transaction, or an --input file"
          (errorOf <$> hwJson home [command, nosig, "--input", home </> "unsigned.json", "--output", home </> "signed.json"] [])
            `shouldReturn` Right "Can not specify both a NOSIGHASH and an --input file"

    it "refuse a missing --input file" $
      withHome $ \home ->
        forM_ ["signtx", "signsweep"] $ \command ->
          (errorOf <$> hwJson home [command, "--input", home </> "missing.json", "--output", home </> "signed.json"] [])
            `shouldReturn` Right (noSuchFile (home </> "missing.json"))

  -- The unsigned file is of no use once the transaction is signed
  describe "signtx and signsweep with an --input file and no --output file" $ do
    let question file = "Replace " <> Text.pack file <> " with the signed transaction? Give an --output file to write it elsewhere. [y/N] "
    it "replace the --input file with the signed transaction when the user agrees" $
      withHome $ \home -> do
        createMain home
        Json.encodeFile (home </> "payment.json") (unsignedPayment ctx)
        signed <- txOf =<< answer home ["signtx", "--input", home </> "payment.json"] (["y"] <> seedOnce)
        pendingState signed `shouldBe` Just (True, False)
        written <- readTxFile (home </> "payment.json")
        isSigned (txSignDataTx (txFileData written)) `shouldBe` True

    it "ask whether to replace the --input file, naming --output" $
      withHome $ \home -> do
        createMain home
        Json.encodeFile (home </> "payment.json") (unsignedPayment ctx)
        run <- hw home ["signtx", "--input", home </> "payment.json"] (["y"] <> seedOnce)
        runErr run `shouldSatisfy` Text.isInfixOf (question (home </> "payment.json"))

    -- The mnemonic would be typed for nothing
    it "ask before the mnemonic, and leave the file as it was when the user does not agree" $
      withHome $ \home -> do
        createMain home
        Json.encodeFile (home </> "payment.json") (unsignedPayment ctx)
        original <- BS.readFile (home </> "payment.json")
        run <- hw home ["signtx", "--input", home </> "payment.json"] (["n"] <> seedOnce)
        run `shouldSatisfy` printed "The transaction was not signed: give an --output file to write it to"
        run `shouldNotSatisfy` askedMnemonic
        BS.readFile (home </> "payment.json") `shouldReturn` original

    it "do not ask with --output, and leave the --input file as it was" $
      withHome $ \home -> do
        createMain home
        Json.encodeFile (home </> "payment.json") (unsignedPayment ctx)
        original <- BS.readFile (home </> "payment.json")
        run <- hw home ["signtx", "--input", home </> "payment.json", "--output", home </> "signed.json"] seedOnce
        runErr run `shouldNotSatisfy` Text.isInfixOf "Replace "
        BS.readFile (home </> "payment.json") `shouldReturn` original

    -- The keys would be typed for nothing
    it "ask before the keys of a sweep" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run <- hw home ["signsweep", "--input", home </> "sweep.json", "--key-file", "-"] ["n", wikiKeyWif]
        run `shouldSatisfy` printed "The transaction was not signed: give an --output file to write it to"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- In a wallet without accounts: the file is all there is of the sweep
    it "sign a sweep outside the wallet in place of its file, which is sent from there" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        keys <- writeKeyFile home
        _ <- answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        _ <- answer home ["signsweep", "--input", home </> "sweep.json", "--key-file", keys] ["y"]
        _ <- answer home ["sendtx", "--input", home </> "sweep.json", "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (otherAddr 0))])]

  -- The mnemonic would be typed for nothing
  describe "signtx, before it asks for the mnemonic," $ do
    it "refuses a transaction signed already" $
      withHome $ \home -> do
        createMain home
        Json.encodeFile (home </> "payment.json") (signedPayment ctx)
        run <- hw home ["signtx", "--input", home </> "payment.json", "--output", home </> "signed.json"] seedOnce
        run `shouldSatisfy` printed "The transaction is already signed"
        run `shouldNotSatisfy` askedMnemonic

    it "refuses a transaction sent already" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["sendtx", nosig, "--yes"] []
        run <- hw home ["signtx", nosig] seedOnce
        run `shouldSatisfy` printed "The transaction is already online"
        run `shouldNotSatisfy` askedMnemonic

    -- The payment is from main
    it "refuses an --account the transaction does not involve" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        Json.encodeFile (home </> "payment.json") (unsignedPayment ctx)
        run <- hw home ["signtx", "--input", home </> "payment.json", "--output", home </> "signed.json", "--account", "second"] seedOnce
        run `shouldSatisfy` printed "The transaction does not involve the account second"
        run `shouldNotSatisfy` askedMnemonic

    it "refuses an --output file that exists" $
      withHome $ \home -> do
        createMain home
        Json.encodeFile (home </> "payment.json") (unsignedPayment ctx)
        writeFile (home </> "signed.json") ""
        run <- hw home ["signtx", "--input", home </> "payment.json", "--output", home </> "signed.json"] seedOnce
        run `shouldSatisfy` printed (alreadyExists (home </> "signed.json"))
        run `shouldNotSatisfy` askedMnemonic

{- Sending -}

-- | The state of the one pending transaction of a home: signed, and online
onePendingState :: FilePath -> IO (Maybe (Bool, Bool))
onePendingState home = pendingState <$> onePendingTx home

-- | The question of sendtx
sendQuestion :: Text
sendQuestion = "Send this transaction to the network? This cannot be undone. [y/N] "

confirmationSpec :: Ctx -> Spec
confirmationSpec ctx =
  describe "sendtx, before it sends" $ do
    -- On stderr, which keeps the answer clean: the payment of 20000 to
    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    it "shows the transaction on stderr and asks whether to send it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["sendtx", nosig] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
        runErr run `shouldSatisfy` Text.isInfixOf sendQuestion

    it "sends it when the user answers yes, the --json answer kept clean" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        sent <- txOf =<< answer home ["sendtx", nosig] ["y"]
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]

    it "sends nothing when the user answers anything else, and keeps the transaction as it was" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        (errorOf <$> hwJson home ["sendtx", nosig] ["n"]) `shouldReturn` Right "The transaction was not sent"
        (inMempool <$> readIORef chain) `shouldReturn` []
        onePendingState home `shouldReturn` Just (True, False)

    -- The file of the signer takes the place of the unsigned version before
    -- the question: the answer no undoes it
    it "keeps the unsigned version of a file signed offline that the user does not send" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        (errorOf <$> hwJson online ["sendtx", "--input", signer </> "signed.json"] ["no"]) `shouldReturn` Right "The transaction was not sent"
        (inMempool <$> readIORef chain) `shouldReturn` []
        onePendingState online `shouldReturn` Just (False, False)

    it "sends without showing the transaction or asking with --yes" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        run <- hw home ["sendtx", nosig, "--yes"] []
        runExit run `shouldBe` ExitSuccess
        runErr run `shouldNotSatisfy` Text.isInfixOf sendQuestion
        (spendsAndPays ctx <$>) . inMempool <$> readIORef chain `shouldReturn` [paymentSent ctx]

    -- A script without --yes has nothing to answer with
    it "sends nothing when the input ends before the answer" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        (errorOf <$> hwJson home ["sendtx", nosig] []) `shouldReturn` Right "Cancelled: the input ended before the answer"
        (inMempool <$> readIORef chain) `shouldReturn` []
