{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A payment signed by signtx and sent by broadcast, on the same computer
-- or offline: the --input and --output files, the portfolio a file is of,
-- a transfer between two portfolios, what the signing commands refuse
-- before they ask for the mnemonic, and the question broadcast asks before
-- it sends.
module Haskoin.Wallet.Workflow.SigningSpec (spec) where

import Control.Monad (forM_)
import Data.Char (isHexDigit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Serialize as S
import Data.IORef (modifyIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (AccountKind (..), keyAddress, legacyPath)
import Haskoin.Wallet.Store (PublishFailure (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData (..), isSigned, noLabel)
import Haskoin.Wallet.Workflow.TestUtils
import Numeric.Natural (Natural)
import System.Directory (copyFile, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  signingSpec ctx
  inputFileSpec ctx
  foreignFileSpec ctx
  confirmationSpec ctx
  notPublishedSpec ctx

{- Signing -}

-- | What the payment of 20000 satoshis spends and pays, as the store sees
-- it once sent: the coin of the payment to the wallet, and 27740 of change
paymentSent :: Ctx -> ([OutPoint], [(Word64, Either String Address)])
paymentSent ctx = ([OutPoint (txHash (payment ctx)) 0], [(20000, Right (otherAddr 0)), (27740, Right (internalAddr 0))])

-- | What deletetx answers: the nosigHash and the txid of the transaction
-- deleted, as typed, and how many coins and change addresses it freed
deletedTxOf :: Response -> Either String (String, Maybe String, Natural, Natural)
deletedTxOf (ResponseDeleteTx nosigH txidM coins addrs) = Right (hex nosigH, hex <$> txidM, coins, addrs)
  where
    hex = Text.unpack . txHashToHex
deletedTxOf other = Left (show other)

-- 20000 satoshis are paid at 10 satoshis a byte: the transaction of one
-- input and two outputs is 226 bytes, so the fee is 2260, and the change
-- 50000 - 20000 - 2260 = 27740.
signingSpec :: Ctx -> Spec
signingSpec ctx = do
  describe "A payment signed on the same computer" $ do
    -- signtx keeps the signed version in the wallet, which broadcast sends: it
    -- sends no transaction that is not signed
    it "is prepared, signed and sent by its nosigHash, and confirmed by a sync" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        sent <- txOf =<< answer home ["broadcast", nosig, "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        modifyIORef chain mine
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 27740)]

    -- The answer of broadcast reads as once sent: what the wallet holds tells
    -- whether it marked the transaction online
    it "is sent by the txid signtx shows, and marked online" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        txid <- txidOf =<< txOf =<< answer home ["signtx", nosig] seedOnce
        _ <- answer home ["broadcast", txid, "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        onePendingState home `shouldReturn` Just (True, True)

    it "is signed by the short id of its nosigHash, as pendingtxs shows it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        signed <- txOf =<< answer home ["signtx", shortOf nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)

    it "is sent by the short id of its txid once signed, and marked online" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        _ <- answer home ["broadcast", shortOf txid, "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        onePendingState home `shouldReturn` Just (True, True)

    it "is deleted by the short id of its nosigHash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        _ <- answer home ["deletetx", shortOf nosig] []
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- No pending transaction has a hash that starts with 00000000
    it "is not found by a short id that matches no pending transaction, as a hash that matches none" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        (errorOf <$> hwJson home ["deletetx", "00000000"] []) `shouldReturn` Right "The pending transaction does not exist"

    -- It held the coin of 50000 and the change address
    it "is deleted by its txid once signed, and deletetx answers with both its hashes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        (deletedTxOf <$> hwJson home ["deletetx", txid] []) `shouldReturn` Right (nosig, Just txid, 1, 1)
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- As pendingtxs showed it, even when named by its nosigHash
    it "is shown by deletetx by its txid once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home ["deletetx", nosig] []
        runOut run `shouldSatisfy` Text.isInfixOf ("Deleted  : " <> Text.pack txid)

    it "is shown by deletetx by its txid in full, then its short id" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home ["deletetx", nosig] []
        runOut run `shouldSatisfy` Text.isInfixOf ("Deleted  : " <> Text.pack txid <> "\n  Short id : " <> Text.pack (shortOf txid) <> "\n")

    -- The payment spends one coin and pays its change to one change address
    it "says what deletetx freed: the coins it spent, and the change addresses it paid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["deletetx", nosig] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Freed    : 1 coin, 1 change address\n"

    -- The chain pays the first address of the bip44 account of portfolio0
    -- of the backup on testnet, and the portfolio pays 20000 to another
    -- wallet there
    it "is signed from the shares of a backup, typed in place of the mnemonic" $
      withChain ctx (chainPaying ctx sharesTestnetAddr) $ \home _ -> do
        _ <- answer home ["newportfolio", "--existing"] sharesForNew
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["sync"] []
        let recipient = maybe (error "recipient") Text.unpack (addrToText btcTest (otherAddr 0))
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", recipient <> ":0.0002", "--network", "btctest", "--fee-rate", "10"] []
        signed <- txOf =<< answer home ["signtx", nosig] sharesOnce
        pendingState signed `shouldBe` Just (True, False)

    -- The wallet has portfolio0 and portfolio1 of the test seed, and the
    -- payment is pending in portfolio1, which the chain pays alone. It is
    -- prepared while portfolio1 is active, and signed once portfolio0 is.
    it "is signed in the portfolio that holds it, whichever is active" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        switchTo home 0 0
        signed <- answer home ["signtx", nosig] seedOnce
        placeOf signed `shouldBe` Right portfolio1Place
        pendingState <$> txOf signed `shouldReturn` Just (True, False)

  -- The signer holds the portfolio and asks for its mnemonic; the online
  -- computer holds the same portfolio, imported, and talks to the store
  describe "A payment signed offline" $ do
    it "is prepared online, signed offline, sent online, and confirmed by a sync" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        prepared <- txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        pendingState prepared `shouldBe` Just (False, False)
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]
        summaryFee (txInfoSummary prepared) `shouldBe` 2260
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        sent <- txOf =<< answer online ["broadcast", "--input", signer </> "signed.psbt", "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        modifyIORef chain mine
        (spendableOn btc <$> hwJson online ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 27740)]
        (txsOf <$> hwJson online ["pendingtxs"] []) `shouldReturn` Right []
        (txsOf <$> hwJson online ["txs"] []) `shouldReturn` Right [(TxDebit, -22260, Just 1), (TxCredit, 50000, Just 4)]
        (addressesOf <$> hwJson online ["addresses", "--change"] []) `shouldReturn` Right [(internalAddrsText !! 0, "")]

    -- Prepared without a file, the transaction is written out by exporttx
    it "is exported by exporttx, signed offline, imported back by importtx, and sent by its nosigHash" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        answer online ["exporttx", nosig, online </> "unsigned.psbt"] [] `shouldReturn` ResponseFile (online </> "unsigned.psbt")
        -- The file is a PSBT (BIP 174), by its first five bytes
        BS.take 5 <$> BS.readFile (online </> "unsigned.psbt") `shouldReturn` "psbt\xff"
        (txFileNetwork <$> readTxFile ctx (online </> "unsigned.psbt")) `shouldReturn` Just btc
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        imported <- txOf =<< answer online ["importtx", signer </> "signed.psbt"] []
        pendingState imported `shouldBe` Just (True, False)
        sent <- txOf =<< answer online ["broadcast", nosig, "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]

    -- Both computers have portfolio0 and portfolio1 of the test wallet, and
    -- the payment is of portfolio1, which the chain pays alone. The file
    -- names no portfolio: the keys of portfolio1 derive the addresses it
    -- spends, and the online wallet holds it pending. portfolio0 is
    -- active on both computers once the payment is prepared.
    it "is shown, signed and imported back by wallets of several portfolios, whichever is active" $
      withHome $ \signer -> withChain ctx (chainPaying ctx account1Addr) $ \online _ -> do
        newTestPortfolio signer
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        _ <- answer online ["importwallet", signer </> "wallet.jsonl"] []
        switchTo online 0 1
        _ <- answer online ["receive"] []
        _ <- answer online ["sync"] []
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        switchTo online 0 0
        (placeOf <$> hwJson signer ["show", online </> "unsigned.psbt"] []) `shouldReturn` Right portfolio1Place
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        (placeOf <$> hwJson online ["importtx", signer </> "signed.psbt"] []) `shouldReturn` Right portfolio1Place

    -- The signer has no coins: the file carries the transactions the
    -- payment spends, and the paths of the change
    it "is shown from its file by show, as the signer reads it before signing, and not kept" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        reviewed <- txOf =<< answer signer ["show", online </> "unsigned.psbt"] []
        paidToOthers reviewed `shouldBe` Map.fromList [(otherAddr 0, 20000)]
        paidToWallet reviewed `shouldBe` Map.fromList [(internalAddr 0, 27740)]
        summaryFee (txInfoSummary reviewed) `shouldBe` 2260
        (txsOf <$> hwJson signer ["pendingtxs"] []) `shouldReturn` Right []

    -- The signer never handed out the address the payment spends from, and
    -- a signed input names no key any more. The file names the path of
    -- that address in a field of hw, which the signer follows.
    it "is shown again from its file on the signer once signed, as the payment it signed" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        reviewed <- hwJson signer ["show", signer </> "signed.psbt"] []
        placeOf reviewed `shouldBe` Right mainPlace
        shown <- txOf reviewed
        pendingState shown `shouldBe` Just (True, False)
        -- 20000 paid and 2260 of fee leave the portfolio
        txRow shown `shouldBe` (TxDebit, -22260, Nothing)
        spentFromWallet shown `shouldBe` Map.fromList [(externalAddr 0, 50000)]
        paidToOthers shown `shouldBe` Map.fromList [(otherAddr 0, 20000)]
        paidToWallet shown `shouldBe` Map.fromList [(internalAddr 0, 27740)]

    -- A payment of every coin has no change, so no output of the file
    -- names the portfolio either: the path of the address spent from is all
    -- that does. One input and one output are 192 bytes, so the fee is
    -- 1920, and 50000 - 1920 = 48080 are paid.
    it "of every coin of the portfolio is shown again from its file on the signer once signed, as spent from its portfolio" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparesendall", textOf (otherAddr 0), "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        reviewed <- hwJson signer ["show", signer </> "signed.psbt"] []
        placeOf reviewed `shouldBe` Right mainPlace
        shown <- txOf reviewed
        txRow shown `shouldBe` (TxDebit, -50000, Nothing)
        spentFromWallet shown `shouldBe` Map.fromList [(externalAddr 0, 50000)]
        paidToOthers shown `shouldBe` Map.fromList [(otherAddr 0, 48080)]

    -- The payment is from the bip44 account of portfolio0 on btc
    it "is shown from its file with its portfolio, its network and the types it involves" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        run <- hw home ["show", home </> "payment.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0\n  Network  : btc\n  Types    : bip44\n"

  -- portfolio0 pays 20000 of its coin of 50000 to the first receiving
  -- address of portfolio1, 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, with 2260 of
  -- fee. The online computer prepares it. The signer holds both
  -- portfolios, and not the payment, and handed out the address of
  -- portfolio1 the payment pays: a database knows an address of its
  -- portfolios once it has handed it out.
  describe "A transfer between two portfolios, in its file" $ do
    let transferFile online = do
          syncedTwoPortfolios online
          _ <- answer online ["preparetx", fixedTo account1Addr "0.0002", "--output", online </> "transfer.psbt", "--fee-rate", "10"] []
          return (online </> "transfer.psbt")
        -- receive gives out the address of portfolio1 while it is active
        twoPortfoliosSigner signer = do
          newTestPortfolio signer
          newTestPortfolio signer
          switchTo signer 0 1
          _ <- answer signer ["receive"] []
          switchTo signer 0 0
        -- The online computer deletes the transfer it holds pending, and
        -- holds its coin again: importtx then finds the portfolio of the
        -- file by itself
        deletedTransfer online = do
          file <- transferFile online
          nosig <- nosigOf =<< onePendingTx online
          _ <- answer online ["deletetx", nosig] []
          return file
    it "is imported by importtx into the portfolio that pays it" $
      withChain ctx (paidChain ctx) $ \online _ -> do
        file <- deletedTransfer online
        (placeOf <$> hwJson online ["importtx", file] []) `shouldReturn` Right mainPlace

    -- The portfolio that pays locks the coins it spends, and signs
    it "is imported by importtx into the portfolio that pays it, though the portfolio it pays is active" $
      withChain ctx (paidChain ctx) $ \online _ -> do
        file <- deletedTransfer online
        switchTo online 0 1
        (placeOf <$> hwJson online ["importtx", file] []) `shouldReturn` Right mainPlace

    it "is signed by the portfolio that pays it, though the portfolio it pays is active" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        file <- transferFile online
        twoPortfoliosSigner signer
        switchTo signer 0 1
        signed <- answer signer ["signtx", "--input", file, "--output", signer </> "signed.psbt"] seedOnce
        placeOf signed `shouldBe` Right mainPlace
        pendingState <$> txOf signed `shouldReturn` Just (True, False)

    -- The mnemonic signs for portfolio0, so the view before it is that of
    -- portfolio0, and says that portfolio1 is active
    it "is shown before the mnemonic as the portfolio that pays sees it, which says it is not the active one" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        file <- transferFile online
        twoPortfoliosSigner signer
        switchTo signer 0 1
        run <- hw signer ["signtx", "--input", file, "--output", signer </> "signed.psbt"] seedOnce
        let (beforeMnemonic, _) = Text.breakOn "Mnemonic" (runErr run)
        beforeMnemonic `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0, not the active portfolio1\n"

    -- The payment pays 20000 to the first receiving address of the bip44
    -- account of portfolio1, which the signer gave out
    it "is shown before the mnemonic with the portfolio of the address it pays named after the path of that address" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        file <- transferFile online
        twoPortfoliosSigner signer
        run <- hw signer ["signtx", "--input", file, "--output", signer </> "signed.psbt"] seedOnce
        let (beforeMnemonic, _) = Text.breakOn "Mnemonic" (runErr run)
        beforeMnemonic `shouldSatisfy` Text.isInfixOf "\n  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00020000 btc  bip44/0/0 of portfolio1\n"

  -- The online computer holds portfolio0 and portfolio1, and the payment
  -- is of portfolio1, which the chain pays alone. The signer holds
  -- portfolio0 alone.
  describe "A payment signed offline by a signer that lacks its portfolio" $
    it "is refused before the mnemonic is asked, with how to add the portfolio" $
      withHome $ \signer -> withChain ctx (chainPaying ctx account1Addr) $ \online _ -> do
        syncedTwoPortfolios online
        newTestPortfolio signer
        switchTo online 0 1
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        run
          `shouldSatisfy` printedSentence "The transaction does not involve this wallet. It may be of a portfolio this database does not have yet: newportfolio --existing adds the next portfolio of a mnemonic, and importwallet the portfolios of a file"
        run `shouldNotSatisfy` askedMnemonic

{- The files of other wallets -}

-- | A file Bitcoin Core wrote, copied from test/data/psbt/core into a
-- home: see test/data/psbt/README.md
coreFile :: FilePath -> FilePath -> IO FilePath
coreFile home name = do
  copyFile ("test/data/psbt/core" </> name) (home </> name)
  return (home </> name)

-- | The network show shows a transaction outside the wallet on
outsideNetworkOf :: Response -> Either String String
outsideNetworkOf (ResponseOutsideTx net _) = Right net.name
outsideNetworkOf other = Left (show other)

-- | The files of other wallets, and the forms a file takes: base64 text,
-- and the transaction alone in hexadecimal
foreignFileSpec :: Ctx -> Spec
foreignFileSpec ctx =
  describe "A file of another wallet" $ do
    -- Core prepared 1 BTC from the coin of 50 BTC of /0/0 to another
    -- wallet, with the change at /1/0 and a fee of 2250. The signed file
    -- is the one Core accepts, pinned in TestUtils.
    it "prepared by Bitcoin Core for the portfolio is shown, and signed into that file, which is base64 text as Core gives it" $
      withHome $ \home -> do
        newTestPortfolio home
        prepared <- coreFile home "core-prepared.psbt"
        shown <- txOf =<< answer home ["show", prepared] []
        pendingState shown `shouldBe` Just (False, False)
        paidToOthers shown `shouldBe` Map.fromList [(testAddress "1GfJmWwG2JUjrvQ6WjVfwaWpPgtbHDhSdE", 100000000)]
        paidToWallet shown `shouldBe` Map.fromList [(internalAddr 0, 4899997750)]
        feeOf shown `shouldBe` 2250
        signed <- txOf =<< answer home ["signtx", "--input", prepared, "--output", home </> "signed.psbt"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        (Base64.encode <$> BS.readFile (home </> "signed.psbt")) `shouldReturn` corePreparedSignedText

    -- Core's file has no field of hw. The wallet never handed out the
    -- address the payment spends from: signtx writes its path into the
    -- file it signs, which the wallet then follows.
    it "prepared by Bitcoin Core and signed by hw is shown again as the payment of the portfolio, by a wallet that never handed out its address" $
      withHome $ \home -> do
        newTestPortfolio home
        prepared <- coreFile home "core-prepared.psbt"
        _ <- answer home ["signtx", "--input", prepared, "--output", home </> "signed.psbt"] seedOnce
        shown <- txOf =<< answer home ["show", home </> "signed.psbt"] []
        pendingState shown `shouldBe` Just (True, False)
        spentFromWallet shown `shouldBe` Map.fromList [(externalAddr 0, 5000000000)]
        paidToOthers shown `shouldBe` Map.fromList [(testAddress "1GfJmWwG2JUjrvQ6WjVfwaWpPgtbHDhSdE", 100000000)]
        paidToWallet shown `shouldBe` Map.fromList [(internalAddr 0, 4899997750)]

    -- Bitcoin Core 32 and later write a PSBT of version 2 (BIP 370) unless
    -- asked for version 0: the same payment, as Core 32.1 writes it. The
    -- signed file keeps the version: it is the one Core accepts, pinned
    -- in TestUtils.
    it "prepared by Bitcoin Core as a PSBT of version 2 is shown and signed the same, into a file of version 2" $
      withHome $ \home -> do
        newTestPortfolio home
        prepared <- coreFile home "core-prepared-v2.psbt"
        shown <- txOf =<< answer home ["show", prepared] []
        paidToOthers shown `shouldBe` Map.fromList [(testAddress "1GfJmWwG2JUjrvQ6WjVfwaWpPgtbHDhSdE", 100000000)]
        paidToWallet shown `shouldBe` Map.fromList [(internalAddr 0, 4899997750)]
        feeOf shown `shouldBe` 2250
        signed <- txOf =<< answer home ["signtx", "--input", prepared, "--output", home </> "signed.psbt"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        (Base64.encode <$> BS.readFile (home </> "signed.psbt")) `shouldReturn` corePreparedSignedV2Text

    -- Core prepared 1 BTC from the coin of 50 BTC of /0/0 of the bip84
    -- account to another wallet, with the change at /1/0 and a fee of
    -- 1440: 10 satoshis a virtual byte for 144. The signed file is the one
    -- Core accepts, pinned in TestUtils.
    it "prepared by Bitcoin Core for a bip84 account is shown and signed into that file, the signature in the witness" $
      withHome $ \home -> do
        newTestPortfolio home
        prepared <- coreFile home "core-prepared-segwit.psbt"
        shown <- txOf =<< answer home ["show", prepared] []
        paidToOthers shown `shouldBe` Map.fromList [(testAddress "157L9V7wdL7pYt2nndN5NN5DMynh1HdYhF", 100000000)]
        paidToWallet shown `shouldBe` Map.fromList [(segwitInternalAddr 0, 4899998560)]
        (feeOf shown, summarySize (txInfoSummary shown)) `shouldBe` (1440, 144)
        signed <- txOf =<< answer home ["signtx", "--input", prepared, "--output", home </> "signed.psbt"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        (Base64.encode <$> BS.readFile (home </> "signed.psbt")) `shouldReturn` coreSegwitPreparedSignedText

    -- Core prepared 2.5 BTC of a wallet of its own to another address of
    -- that wallet, with a fee of 2250
    it "prepared by Bitcoin Core for another wallet is shown outside the wallet on the --network given" $
      withHome $ \home -> do
        newTestPortfolio home
        outside <- coreFile home "core-outside.psbt"
        shown <- hwJson home ["show", outside, "--network", "btc"] []
        case shown of
          ResponseOutsideTx net info -> do
            net.name `shouldBe` "btc"
            feeOf info `shouldBe` 2250
            paidToOthers info `shouldBe` Map.fromList [(testAddress "13tJFzYnF13gRMh6Z6eb44mLD9e9R8sHeZ", 250000000), (testAddress "1588dPBmgVnvS2N3BE5YHXUqXcb468zpsz", 4749997750)]
          other -> expectationFailure ("Not a transaction outside the wallet: " <> show other)

    -- The file names no network, as a file of another wallet does
    it "prepared by Bitcoin Core for another wallet is shown on the default network of the configuration file without --network" $
      withHome $ \home -> do
        newTestPortfolio home
        setDefaultNetwork home (Just "bch")
        outside <- coreFile home "core-outside.psbt"
        (outsideNetworkOf <$> hwJson home ["show", outside] []) `shouldReturn` Right "bch"

    -- btc is the default network when the configuration file names none
    it "prepared by Bitcoin Core for another wallet is shown on btc when the configuration file names no default network" $
      withHome $ \home -> do
        newTestPortfolio home
        setDefaultNetwork home Nothing
        outside <- coreFile home "core-outside.psbt"
        (outsideNetworkOf <$> hwJson home ["show", outside] []) `shouldReturn` Right "btc"

    it "written by hw names its network, which --network may only repeat" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "unsigned.psbt", "--fee-rate", "10"] []
        (errorOf <$> hwJson home ["show", home </> "unsigned.psbt", "--network", "bch"] [])
          `shouldReturn` Right "The file is on btc, not on the --network bch"
        (placeOf <$> hwJson home ["show", home </> "unsigned.psbt", "--network", "btc"] []) `shouldReturn` Right mainPlace

    -- Bitcoin Core's finalizepsbt gives the transaction alone, in hexadecimal
    it "holding the signed transaction alone is sent when the wallet holds the transaction, and refused otherwise" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        _ <- answer home ["exporttx", nosig, home </> "signed.psbt"] []
        signed <- readTxFile ctx (home </> "signed.psbt")
        writeFile (home </> "signed.hex") (Text.unpack (encodeHex (S.encode (txSignDataTx (txFileData signed)))) <> "\n")
        sent <- txOf =<< answer home ["broadcast", "--input", home </> "signed.hex", "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        writeFile (home </> "other.hex") (Text.unpack (encodeHex (S.encode (txSignDataTx (signedPayment ctx)))))
        (errorOf <$> hwJson home ["broadcast", "--input", home </> "other.hex"] [])
          `shouldReturn` Right "The file holds a transaction alone, which the wallet does not hold as pending: hw needs the whole transaction each of its coins comes from, which a PSBT carries"

    -- A transaction given alone is the whole file. Bytes after it would
    -- be dropped without a word, so the file is refused, as Bitcoin Core
    -- refuses such hexadecimal. A transaction cut short is refused the
    -- same way.
    it "holding a transaction in hexadecimal with bytes after it, or cut short, is refused" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        _ <- answer home ["exporttx", nosig, home </> "signed.psbt"] []
        signed <- readTxFile ctx (home </> "signed.psbt")
        let hex = Text.unpack (encodeHex (S.encode (txSignDataTx (txFileData signed))))
            longer = home </> "longer.hex"
            shorter = home </> "shorter.hex"
        writeFile longer (hex <> "00\n")
        writeFile shorter (take (length hex - 2) hex <> "\n")
        forM_ [longer, shorter] $ \file ->
          (errorOf <$> hwJson home ["broadcast", "--input", file] [])
            `shouldReturn` Right ("The file " <> Text.pack file <> " could not be read: The hexadecimal text is not one whole transaction")

    it "in base64 text, as Bitcoin Core and HWI give a PSBT, is read like its bytes" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        _ <- answer home ["exporttx", nosig, home </> "signed.psbt"] []
        BS.writeFile (home </> "signed.txt") . (<> "\n") . Base64.encode =<< BS.readFile (home </> "signed.psbt")
        sent <- txOf =<< answer home ["broadcast", "--input", home </> "signed.txt", "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]

{- The files of signtx, signsweep and broadcast -}

-- | A payment of the test wallet to another wallet, unsigned
unsignedPayment :: Ctx -> TxSignData
unsignedPayment ctx =
  TxSignData (spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 40000)]) [funding] [legacyPath (Deriv :/ 0 :/ 0)] [] Nothing Nothing [] noLabel
  where
    funding = fundingTx ctx 1 [(externalAddr 0, 50000)]

-- | The same payment signed by the test wallet, which another wallet holds
signedPayment :: Ctx -> TxSignData
signedPayment ctx = signedByWallet ctx (unsignedPayment ctx)

-- | The files of signtx, signsweep and broadcast: what they refuse, and the
-- --input file replaced by the signed transaction
inputFileSpec :: Ctx -> Spec
inputFileSpec ctx = do
  describe "broadcast from a file" $ do
    it "needs a txid or a file" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["broadcast"] []) `shouldReturn` Right "Give the TXID of a pending transaction or an --input file"

    it "takes a txid or a file, not both" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["broadcast", Text.unpack (txHashToHex (fakeTxHash 1)), "--input", home </> "sweep.psbt"] [])
          `shouldReturn` Right "Can not specify both a TXID and an --input file"

    it "refuses a missing file" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["broadcast", "--input", home </> "missing.json"] [])
          `shouldReturn` Right (noSuchFile (home </> "missing.json"))

    it "refuses an unsigned transaction" $
      withHome $ \home -> do
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        (errorOf <$> hwJson home ["broadcast", "--input", home </> "payment.psbt"] [])
          `shouldReturn` Right "The transaction is not signed"

    -- The file is written as another wallet writes it, without the
    -- network, and the configuration names no default network: btc is
    -- the default network then
    it "sends a file of another wallet, which names no network, for a transaction the wallet does not hold, on btc without a default network" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        setDefaultNetwork home Nothing
        writeOtherWalletTxFile ctx (home </> "payment.psbt") (signedPayment ctx)
        sent <- hwJson home ["broadcast", "--input", home </> "payment.psbt", "--yes"] []
        case sent of
          ResponseSentTx net txid -> (net.name, txid) `shouldBe` ("btc", txHash (txSignDataTx (signedPayment ctx)))
          other -> expectationFailure ("Not a transaction sent from outside the wallet: " <> show other)
        (map txHash . inMempool <$> readIORef chain) `shouldReturn` [txHash (txSignDataTx (signedPayment ctx))]

  describe "signtx and signsweep" $ do
    let nosig = Text.unpack (txHashToHex (fakeTxHash 1))
    it "need a nosigHash or an --input file, not both" $
      withHome $ \home ->
        forM_ ["signtx", "signsweep"] $ \command -> do
          (errorOf <$> hwJson home [command] [])
            `shouldReturn` Right "Give the NOSIGHASH of a pending transaction, or an --input file"
          (errorOf <$> hwJson home [command, nosig, "--input", home </> "unsigned.psbt", "--output", home </> "signed.psbt"] [])
            `shouldReturn` Right "Can not specify both a NOSIGHASH and an --input file"

    it "refuse a missing --input file" $
      withHome $ \home ->
        forM_ ["signtx", "signsweep"] $ \command ->
          (errorOf <$> hwJson home [command, "--input", home </> "missing.json", "--output", home </> "signed.psbt"] [])
            `shouldReturn` Right (noSuchFile (home </> "missing.json"))

  -- The payment of 40000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, from the
  -- coin at /0/0
  describe "signtx, showing what the mnemonic signs" $ do
    it "shows the transaction as show does before it asks for the mnemonic" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        run <- hw home ["signtx", "--input", home </> "payment.psbt", "--output", home </> "signed.psbt"] seedOnce
        let (beforeMnemonic, mnemonicOnward) = Text.breakOn "Mnemonic" (runErr run)
        beforeMnemonic `shouldSatisfy` Text.isInfixOf "  To       : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00040000 btc\n"
        mnemonicOnward `shouldSatisfy` (not . Text.null)

    it "answers on the screen with the id of the signed transaction and the file written, not with the transaction again" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        run <- hw home ["signtx", "--input", home </> "payment.psbt", "--output", home </> "signed.psbt"] seedOnce
        case Text.lines (runOut run) of
          signedLine : writtenLine : _ -> do
            Text.stripPrefix "Signed, txid " signedLine `shouldSatisfy` maybe False (\txid -> Text.length txid == 64 && Text.all isHexDigit txid)
            writtenLine `shouldBe` "Written to " <> Text.pack (home </> "signed.psbt")
          other -> expectationFailure ("Not the answer of a signature: " <> show other)
        runOut run `shouldNotSatisfy` Text.isInfixOf "To       :"

  -- The unsigned file is of no use once the transaction is signed
  describe "signtx and signsweep with an --input file and no --output file" $ do
    let question file = "Replace " <> Text.pack file <> " with the signed transaction? Give an --output file to write it elsewhere. [y/N] "
    it "replace the --input file with the signed transaction when the user agrees" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        signed <- txOf =<< answer home ["signtx", "--input", home </> "payment.psbt"] (["y"] <> seedOnce)
        pendingState signed `shouldBe` Just (True, False)
        written <- readTxFile ctx (home </> "payment.psbt")
        isSigned (txSignDataTx (txFileData written)) `shouldBe` True

    it "ask whether to replace the --input file, naming --output" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        run <- hw home ["signtx", "--input", home </> "payment.psbt"] (["y"] <> seedOnce)
        runErr run `shouldSatisfy` Text.isInfixOf (question (home </> "payment.psbt"))

    -- The mnemonic would be typed for nothing
    it "ask before the mnemonic, and leave the file as it was when the user does not agree" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        original <- BS.readFile (home </> "payment.psbt")
        run <- hw home ["signtx", "--input", home </> "payment.psbt"] (["n"] <> seedOnce)
        run `shouldSatisfy` printed "The transaction was not signed: give an --output file to write it to"
        run `shouldNotSatisfy` askedMnemonic
        BS.readFile (home </> "payment.psbt") `shouldReturn` original

    it "do not ask with --output, and leave the --input file as it was" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        original <- BS.readFile (home </> "payment.psbt")
        run <- hw home ["signtx", "--input", home </> "payment.psbt", "--output", home </> "signed.psbt"] seedOnce
        runErr run `shouldNotSatisfy` Text.isInfixOf "Replace "
        BS.readFile (home </> "payment.psbt") `shouldReturn` original

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
        _ <- answer home ["broadcast", "--input", home </> "sweep.json", "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (otherAddr 0))])]

  -- The mnemonic would be typed for nothing
  describe "signtx, before it asks for the mnemonic," $ do
    it "refuses a transaction signed already" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (signedPayment ctx)
        run <- hw home ["signtx", "--input", home </> "payment.psbt", "--output", home </> "signed.psbt"] seedOnce
        run `shouldSatisfy` printed "The transaction is already signed"
        run `shouldNotSatisfy` askedMnemonic

    it "refuses a transaction sent already" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["broadcast", nosig, "--yes"] []
        run <- hw home ["signtx", nosig] seedOnce
        run `shouldSatisfy` printed "The transaction is already online"
        run `shouldNotSatisfy` askedMnemonic

    it "refuses an --output file that exists" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile ctx (home </> "payment.psbt") (unsignedPayment ctx)
        writeFile (home </> "signed.psbt") ""
        run <- hw home ["signtx", "--input", home </> "payment.psbt", "--output", home </> "signed.psbt"] seedOnce
        run `shouldSatisfy` printed (alreadyExists (home </> "signed.psbt"))
        run `shouldNotSatisfy` askedMnemonic

    -- A file crafted to pay the wallet's coin to the key of the account at
    -- /2/7, where it has no address, with the record that would make hw
    -- take the output for the account's own change. The wallet would never
    -- see the funds, since it scans /0/n and /1/n alone.
    it "refuses a file that names a key of the account at a path of no address of it, as show does" $
      withHome $ \home -> do
        newTestPortfolio home
        let hidden = spendingTx ctx [(txHash (payment ctx), 0)] [(addrAt27, 40000)]
            refusal = "Output 0 names a key of the account at /2/7, where it has no address: its addresses are at /0/n and /1/n"
        psbt <- psbtOnBitcoin ctx (TxSignData hidden [payment ctx] [legacyPath (Deriv :/ 0 :/ 0)] [] (Just 10) Nothing [] noLabel)
        let naming = insertOutputDerivation ctx (keyAt27 ctx) (Derivation testFingerprint "m/44'/0'/0'/2/7")
        BS.writeFile (home </> "hidden.psbt") (encodePsbt (Psbt psbt.version psbt.unsignedTx psbt.globals psbt.inputs (naming <$> psbt.outputs)))
        review <- hw home ["show", home </> "hidden.psbt"] []
        review `shouldSatisfy` printed refusal
        run <- hw home ["signtx", "--input", home </> "hidden.psbt", "--output", home </> "signed.psbt"] seedOnce
        run `shouldSatisfy` printed refusal
        run `shouldNotSatisfy` askedMnemonic

    -- The change goes to the key of the account at /1/2000000000, as a
    -- genuine record says: no scan of the account would find it there
    it "refuses a file whose change lies far past every address of its chain, as show does, and writes no signed file" $
      withHome $ \home -> do
        newTestPortfolio home
        let farPath = Deriv :/ 1 :/ 2000000000 :: SoftPath
            farChange = keyAddress Bip44 ctx (derivePubPath ctx farPath (testAccountPub ctx))
            tx = spendingTx ctx [(txHash (payment ctx), 0)] [(otherAddr 0, 20000), (farChange, 27740)]
            refusal = "The path bip44/1/2000000000 of an output is over 1000 addresses past the last one of its chain that the wallet holds or the transaction spends from: no scan of the account would find what it pays there"
        writeTxFile ctx (home </> "far.psbt") (TxSignData tx [payment ctx] [legacyPath (Deriv :/ 0 :/ 0)] [legacyPath farPath] (Just 10) Nothing [] noLabel)
        review <- hw home ["show", home </> "far.psbt"] []
        review `shouldSatisfy` printed refusal
        run <- hw home ["signtx", "--input", home </> "far.psbt", "--output", home </> "signed.psbt"] seedOnce
        run `shouldSatisfy` printed refusal
        run `shouldNotSatisfy` askedMnemonic
        doesFileExist (home </> "signed.psbt") `shouldReturn` False

{- Sending -}

-- | The state of the one pending transaction of a home: signed, and online
onePendingState :: FilePath -> IO (Maybe (Bool, Bool))
onePendingState home = pendingState <$> onePendingTx home

-- | The question of broadcast
sendQuestion :: Text
sendQuestion = "Send this transaction to the network? This cannot be undone. [y/N] "

confirmationSpec :: Ctx -> Spec
confirmationSpec ctx =
  describe "broadcast, before it sends" $ do
    -- On stderr, which keeps the answer clean: the payment of 20000 to
    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    it "shows the transaction on stderr and asks whether to send it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["broadcast", nosig] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
        runErr run `shouldSatisfy` Text.isInfixOf sendQuestion

    -- The payment is from the bip44 account of portfolio0 on btc, as show
    -- shows it
    it "names the portfolio, the network and the types of the transaction it shows" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["broadcast", nosig] ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0\n  Network  : btc\n  Types    : bip44\n"

    -- The payment is of portfolio0, and portfolio1 is active when it is
    -- sent
    it "says the portfolio of the transaction it shows is not the active one, when another one is active" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        newTestPortfolio home
        switchTo home 0 1
        run <- hw home ["broadcast", nosig] ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0, not the active portfolio1\n"

    it "sends it when the user answers yes, the --json answer kept clean" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        sent <- txOf =<< answer home ["broadcast", nosig] ["y"]
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]

    it "sends nothing when the user answers anything else, and keeps the transaction as it was" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        (errorOf <$> hwJson home ["broadcast", nosig] ["n"]) `shouldReturn` Right "The transaction was not sent"
        (inMempool <$> readIORef chain) `shouldReturn` []
        onePendingState home `shouldReturn` Just (True, False)

    -- The file of the signer takes the place of the unsigned version before
    -- the question: the answer no undoes it
    it "keeps the unsigned version of a file signed offline that the user does not send" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.psbt", "--fee-rate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.psbt", "--output", signer </> "signed.psbt"] seedOnce
        (errorOf <$> hwJson online ["broadcast", "--input", signer </> "signed.psbt"] ["no"]) `shouldReturn` Right "The transaction was not sent"
        (inMempool <$> readIORef chain) `shouldReturn` []
        onePendingState online `shouldReturn` Just (False, False)

    it "sends without showing the transaction or asking with --yes" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        run <- hw home ["broadcast", nosig, "--yes"] []
        runExit run `shouldBe` ExitSuccess
        runErr run `shouldNotSatisfy` Text.isInfixOf sendQuestion
        (spendsAndPays ctx <$>) . inMempool <$> readIORef chain `shouldReturn` [paymentSent ctx]

    -- A script without --yes has nothing to answer with
    it "sends nothing when the input ends before the answer" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        (errorOf <$> hwJson home ["broadcast", nosig] []) `shouldReturn` Right "Cancelled: the input ended before the answer"
        (inMempool <$> readIORef chain) `shouldReturn` []

-- | What broadcast does with a transaction the store does not publish, and
-- with one that needs no sending. From version 1.11 a haskoin-store
-- answers that it published a transaction only once a node announced it
-- back. It says why when it can tell no node will take the transaction,
-- and that it heard nothing when a node kept silent.
notPublishedSpec :: Ctx -> Spec
notPublishedSpec ctx = do
  describe "broadcast of a transaction the store says no node would take" $ do
    it "fails with what the store said" $
      withChainNotPublishing ctx (NotPublished "bad-txns-inputs-missingorspent") (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        failure <- errorOf <$> hwJson home ["broadcast", nosig, "--yes"] []
        failure `shouldSatisfy` either (const False) (Text.isSuffixOf " refused the transaction: bad-txns-inputs-missingorspent")

    -- Signed, and not online: the user can send it again, or delete it
    it "leaves the transaction signed, and not marked as sent" $
      withChainNotPublishing ctx (NotPublished "bad-txns-inputs-missingorspent") (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        _ <- hwJson home ["broadcast", nosig, "--yes"] []
        onePendingState home `shouldReturn` Just (True, False)

  describe "broadcast of a transaction no node announced back to the store" $ do
    -- A node says nothing of a transaction it refuses for its fee
    it "fails, and says the fee may be too low" $
      withChainNotPublishing ctx (PublishFailed "no node announced it") (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        failure <- errorOf <$> hwJson home ["broadcast", nosig, "--yes"] []
        failure
          `shouldSatisfy` either
            (const False)
            ( Text.isSuffixOf
                " handed the transaction to a node, and no node announced it in the time the store waits. A node says nothing of a transaction it refuses, so its fee may be too low for the network"
            )

    it "leaves the transaction signed, and not marked as sent" $
      withChainNotPublishing ctx (PublishFailed "no node announced it") (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        _ <- hwJson home ["broadcast", nosig, "--yes"] []
        onePendingState home `shouldReturn` Just (True, False)

  describe "broadcast of a payment that a sync has seen on the network" $ do
    -- The payment is in the history now, and no longer pending: the user
    -- who names it is told so, rather than that no transaction has its id
    it "says the payment was sent already, and where to look" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        txid <- sentPayment home
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["broadcast", txid, "--yes"] [])
          `shouldReturn` Right
            ( "The transaction "
                <> Text.pack txid
                <> " is not pending: it was sent, and a sync has seen it on the network, so there is nothing to send."
                <> " txs shows where it stands. A payment of Bitcoin that stays unconfirmed is replaced with bumpfee"
            )

    it "sends nothing more to the store" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        txid <- sentPayment home
        _ <- answer home ["sync"] []
        _ <- hwJson home ["broadcast", txid, "--yes"] []
        (length . inMempool <$> readIORef chain) `shouldReturn` 1
