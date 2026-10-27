{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | hw as a user runs it: the commands of whole workflows, each run by the
-- hw binary of this build in a home of its own, with the mnemonic typed on
-- its input, against a fake store when it needs the network. The modules
-- under the commands are tested on their own; these tests put them
-- together, as the user meets them, from what hw prints and the code it
-- exits with.
module Haskoin.Wallet.WorkflowSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import Data.Default (def)
import Data.IORef (modifyIORef, readIORef)
import Data.List (isInfixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryResponse)
import Haskoin.Wallet.Coin (CoinLock (..), JsonCoin (..))
import Haskoin.Wallet.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..), DBAddress (..), databaseInUse, runDBWith)
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (MnemonicPass (..))
import qualified Haskoin.Wallet.Schema.V3 as V3
import Haskoin.Wallet.Sync (SyncRes (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData (TxSignData (..))
import Numeric.Natural (Natural)
import System.Directory (copyFile, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Async (async, wait)

spec :: Spec
spec = prepareContext $ \ctx -> do
  accountSpec
  addressSpec
  splitSpec
  messageSpec
  failureSpec ctx
  backupSpec
  receivingSpec ctx
  listingSpec ctx
  signingSpec ctx
  paymentSpec ctx
  recoverSpec ctx
  sweepSpec ctx
  -- An answer of hw reads back as it was written, as --json writes it
  testIdentity def {jsonTests = [JsonBox (arbitraryResponse btc ctx)]}

{- Typing and answers -}

-- | The words and the passphrase of the test wallet, typed once each, as
-- the commands that check a seed ask for them
seedOnce :: [Text]
seedOnce = [mnemonicWords testMnemonic, mnemonicPass testMnemonic]

-- | The same, typed twice each, as createacc asks for them
seedTwice :: [Text]
seedTwice = [mnemonicWords testMnemonic, mnemonicWords testMnemonic, mnemonicPass testMnemonic, mnemonicPass testMnemonic]

-- | The answer of hw with --json, which must not be an error
answer :: FilePath -> [String] -> [Text] -> IO Response
answer home args typed = do
  response <- hwJson home args typed
  case response of
    ResponseError err -> fail $ "hw " <> unwords args <> " failed: " <> Text.unpack err
    _ -> return response

-- | Create the account main of the test wallet in a home
createMain :: FilePath -> IO ()
createMain home = void $ answer home ["createacc", "main"] seedTwice

-- | The error of an answer
errorOf :: Response -> Either String Text
errorOf (ResponseError err) = Right err
errorOf other = Left (show other)

-- | The refusal of a file that exists already
alreadyExists :: FilePath -> Text
alreadyExists file = "Path '" <> Text.pack file <> "' already exists"

-- | The refusal of a file to read that does not exist
noSuchFile :: FilePath -> Text
noSuchFile file = "The file " <> Text.pack file <> " could not be read: no such file or directory"

-- | Whether hw asked for a mnemonic
askedMnemonic :: Run -> Bool
askedMnemonic run = "Mnemonic" `Text.isInfixOf` runErr run

-- | The accounts of an answer: name, index and what each can spend now
accountsOf :: Response -> Either String [(Text, Int, Word64)]
accountsOf (ResponseAccount acc spendable) = Right [(dBAccountName acc, dBAccountIndex acc, spendable)]
accountsOf (ResponseAccounts accs) = Right [(dBAccountName acc, dBAccountIndex acc, spendable) | (acc, spendable) <- accs]
accountsOf (ResponseSync results) = Right [(dBAccountName (syncResAccount s), dBAccountIndex (syncResAccount s), syncResSpendable s) | s <- results]
accountsOf (ResponseRecover _ _ results) = accountsOf (ResponseSync results)
accountsOf other = Left (show other)

-- | The account of an answer, with its index and its public key
keyOf :: Response -> Either String (Text, Int, Text)
keyOf (ResponseAccount acc _) = Right (dBAccountName acc, dBAccountIndex acc, dBAccountXPubKey acc)
keyOf other = Left (show other)

-- | The accounts of an answer, with the fingerprints of their seeds
fingerprintsOf :: Response -> Either String [(Text, Maybe Text)]
fingerprintsOf (ResponseAccounts accs) = Right [(dBAccountName acc, dBAccountFingerprint acc) | (acc, _) <- accs]
fingerprintsOf other = Left (show other)

-- | The addresses of an answer, with their labels
addressesOf :: Response -> Either String [(Text, Text)]
addressesOf (ResponseAddress _ addr _) = Right [(dBAddressAddress addr, dBAddressLabel addr)]
addressesOf (ResponseAddresses _ addrs) = Right [(dBAddressAddress addr, dBAddressLabel addr) | (addr, _) <- addrs]
addressesOf other = Left (show other)

-- | The coins of an answer: outpoint, value, confirmations, lock, and
-- whether the builder may spend it now
coinsShown :: Response -> Either String [(OutPoint, Word64, Natural, Maybe CoinLock, Bool)]
coinsShown (ResponseCoins _ coins) = Right [(c.jsonCoinOutpoint, c.jsonCoinValue, c.jsonCoinConfirmations, c.jsonCoinLock, c.jsonCoinSpendable) | c <- coins]
coinsShown other = Left (show other)

-- | The transaction of an answer
txOf :: Response -> IO TxInfo
txOf (ResponseTx _ info) = return info
txOf other = fail $ "Not a transaction: " <> show other

-- | The transactions of an answer: type, amount, and confirmations once on
-- the chain
txsOf :: Response -> Either String [(TxType, Integer, Maybe Natural)]
txsOf (ResponseTxs _ infos) = Right [(summaryType (txInfoSummary i), summaryAmount (txInfoSummary i), confirmations (txInfoStatus i)) | i <- infos]
  where
    confirmations (OnChain status) = Just (chainConfirmations status)
    confirmations (Pending _) = Nothing
txsOf other = Left (show other)

-- | The state of a transaction in preparation: signed, and online
pendingState :: TxInfo -> Maybe (Bool, Bool)
pendingState info = case txInfoStatus info of
  Pending status -> Just (pendingSigned status, pendingOnline status)
  OnChain _ -> Nothing

-- | The nosigHash of a transaction in preparation, as typed
nosigOf :: TxInfo -> IO String
nosigOf info = case txInfoStatus info of
  Pending status -> return $ Text.unpack (txHashToHex (pendingNosigHash status))
  OnChain _ -> fail "The transaction is not in preparation"

-- | What a transaction pays to the outputs of other wallets
paidToOthers :: TxInfo -> Map.Map Address Natural
paidToOthers = summaryOtherOutputs . txInfoSummary

-- | What a transaction pays to the outputs of the wallet
paidToWallet :: TxInfo -> Map.Map Address Natural
paidToWallet = fmap myOutputsValue . summaryMyOutputs . txInfoSummary

-- | The text of an address of the tests
textOf :: Address -> String
textOf = maybe (error "textOf") Text.unpack . addrToText btc

-- | What a transaction spends and pays: its outpoints, and its outputs by
-- amount, with their addresses
spendsAndPays :: Ctx -> Tx -> ([OutPoint], [(Word64, Either String Address)])
spendsAndPays ctx tx = ((.outpoint) <$> tx.inputs, sortOn fst [(out.value, scriptToAddressBS ctx out.script) | out <- tx.outputs])

-- | Whether hw printed the text, on its output or its error output
printed :: Text -> Run -> Bool
printed text run = text `Text.isInfixOf` (runOut run <> runErr run)

{- An account -}

accountSpec :: Spec
accountSpec =
  describe "An account, from createacc to deleteacc" $ do
    it "is created from the mnemonic and the passphrase, each typed twice, at the first index of the seed" $
      withHome $ \home ->
        (keyOf <$> hwJson home ["createacc", "main"] seedTwice) `shouldReturn` Right ("main", 0, testAccountXPub)

    it "is listed by accounts, with what it can spend" $
      withHome $ \home -> do
        createMain home
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    it "takes the next index of a seed that has an account already" $
      withHome $ \home -> do
        createMain home
        (keyOf <$> hwJson home ["createacc", "second"] seedTwice) `shouldReturn` Right ("second", 1, testAccountXPub1)

    it "is refused a name the wallet has, before the mnemonic is asked" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["createacc", "main"] seedTwice
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The account main already exists"
        run `shouldNotSatisfy` askedMnemonic

    it "is checked by testacc against a mnemonic and a passphrase" $
      withHome $ \home -> do
        createMain home
        matched <- hwJson home ["testacc"] seedOnce
        matched `shouldSatisfy` \case ResponseAccResult _ True _ -> True; _ -> False
        missed <- hwJson home ["testacc"] [mnemonicWords testMnemonic, "wrong passphrase"]
        missed `shouldSatisfy` \case ResponseAccResult _ False "The mnemonic and passphrase did not match the account" -> True; _ -> False

    it "is renamed by renameacc" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["renameacc", "main", "savings"] []
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("savings", 0, 0)]

    it "is deleted by deleteacc, given its network and its path" $
      withHome $ \home -> do
        createMain home
        deleted <- hwJson home ["deleteacc", "main", "btc", "/44'/0'/0'"] []
        deleted `shouldSatisfy` \case ResponseAccResult _ True "The account main has been deleted" -> True; _ -> False
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

    it "is not deleted for a network or a path that is not its own" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["deleteacc", "main", "bch", "/44'/0'/0'"] [])
          `shouldReturn` Right "The network of the account to delete did not match"
        (errorOf <$> hwJson home ["deleteacc", "main", "btc", "/44'/0'/1'"] [])
          `shouldReturn` Right "The full derivation path of the account to delete did not match"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    -- As an offline signer hands its accounts to an online computer
    it "is exported to a file by exportacc, and imported from it in another wallet by importacc" $
      withHome $ \signer -> withHome $ \online -> do
        createMain signer
        _ <- answer signer ["exportacc", signer </> "main.json"] []
        (keyOf <$> hwJson online ["importacc", signer </> "main.json"] []) `shouldReturn` Right ("main", 0, testAccountXPub)

{- Addresses -}

addressSpec :: Spec
addressSpec =
  describe "The addresses of an account" $ do
    it "are handed out by receive in the order of the receiving chain" $
      withHome $ \home -> do
        createMain home
        forM_ (take 3 externalAddrsText) $ \expected ->
          (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(expected, "")]

    it "take the label given to receive" $
      withHome $ \home -> do
        createMain home
        (addressesOf <$> hwJson home ["receive", "--label", "rent"] []) `shouldReturn` Right [(externalAddrsText !! 0, "rent")]

    it "are listed by addrs newest first, a page at a time" $
      withHome $ \home -> do
        createMain home
        replicateM_ 3 $ answer home ["receive"] []
        (addressesOf <$> hwJson home ["addrs"] [])
          `shouldReturn` Right [(externalAddrsText !! 2, ""), (externalAddrsText !! 1, ""), (externalAddrsText !! 0, "")]
        (addressesOf <$> hwJson home ["addrs", "--limit", "1", "--offset", "1"] []) `shouldReturn` Right [(externalAddrsText !! 1, "")]

    it "are named by label, by their index" $
      withHome $ \home -> do
        createMain home
        replicateM_ 2 $ answer home ["receive"] []
        (addressesOf <$> hwJson home ["label", "1", "salary"] []) `shouldReturn` Right [(externalAddrsText !! 1, "salary")]
        (addressesOf <$> hwJson home ["addrs"] []) `shouldReturn` Right [(externalAddrsText !! 1, "salary"), (externalAddrsText !! 0, "")]

    it "are not labelled beyond the ones the account reached" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["label", "5", "salary"] []) `shouldReturn` Right "Address 5 does not exist"

    -- An address has an index below 2^31. The index 2^64 wrapped around to
    -- 0, and hw labelled the first address.
    it "are not labelled at an index no address has, rather than at another one" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        run <- hw home ["label", "18446744073709551616", "salary"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "cannot parse value `18446744073709551616'"
        (addressesOf <$> hwJson home ["addrs"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

    -- The gap of BIP44: a wallet looking for payments stops after twenty
    -- unused addresses, so hw hands out no more until one is paid
    it "are refused past twenty that await a payment" $
      withHome $ \home -> do
        createMain home
        replicateM_ 20 $ answer home ["receive"] []
        (errorOf <$> hwJson home ["receive"] [])
          `shouldReturn` Right "The last 20 receiving addresses await a payment: reuse one of them, listed by addrs"

    it "have no change address before the account spends" $
      withHome $ \home -> do
        createMain home
        (addressesOf <$> hwJson home ["addrs", "--internal"] []) `shouldReturn` Right []

{- Mnemonics in parts -}

splitSpec :: Spec
splitSpec =
  describe "A new mnemonic" $ do
    it "has 12 words of the entropy of /dev/random by default, and 24 words for 32 bytes" $
      withHome $ \home -> do
        whole <- answer home ["mnemonic"] []
        whole `shouldSatisfy` \case ResponseMnemonic "/dev/random" ws [] -> length ws == 12; _ -> False
        long <- answer home ["mnemonic", "--entropy", "32"] []
        long `shouldSatisfy` \case ResponseMnemonic _ ws [] -> length ws == 24; _ -> False

    -- Any of the parts alone says nothing of the mnemonic: all of them make
    -- the account, as the mnemonic does
    it "comes in parts with --split, which createacc --split takes for the account of the mnemonic" $
      withHome $ \home -> withHome $ \wholeHome -> withHome $ \partsHome -> do
        generated <- answer home ["mnemonic", "--split", "3"] []
        (ws, parts) <- case generated of
          ResponseMnemonic _ ws parts@[_, _, _] -> return (Text.unwords ws, Text.unwords <$> parts)
          other -> fail $ "Not a mnemonic in three parts: " <> show other
        -- Without a passphrase, which is asked once
        whole <- keyOf <$> hwJson wholeHome ["createacc", "main"] [ws, ws, ""]
        fromParts <- keyOf <$> hwJson partsHome ["createacc", "main", "--split", "3"] (parts <> parts <> [""])
        fromParts `shouldBe` whole
        (\(_, index, _) -> index) <$> fromParts `shouldBe` Right 0

    -- One part is the whole mnemonic, as without --split
    it "comes in 1 to 12 parts" $
      withHome $ \home -> do
        whole <- answer home ["mnemonic", "--split", "1"] []
        whole `shouldSatisfy` \case ResponseMnemonic _ ws [] -> length ws == 12; _ -> False
        forM_ ["0", "13"] $ \parts -> do
          run <- hw home ["mnemonic", "--split", parts] []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf "The --split value has to be between 1 and 12"

    -- The rolls are asked once the entropy of the system is read. None are
    -- typed here, and no mnemonic is made without them.
    it "asks for dice rolls with --dice, and is not made without them" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--dice"] []
        runErr run `shouldSatisfy` Text.isInfixOf "Rolls: "
        run `shouldSatisfy` printed "Cancelled: the input ended before the answer"

{- Signed messages -}

messageSpec :: Spec
messageSpec =
  describe "A message signed by signmsg" $ do
    -- The signature of the address /0/0 of the test wallet, checked apart
    -- from hw with the pure-Python verifier of the message-signing vectors
    -- (test/data/message-signing/tools/btc.py): it recovers the key of the
    -- address from it
    it "is signed by the key of an address of the wallet, from its mnemonic" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hwJson home ["signmsg", textOf (externalAddr 0), "hello"] seedOnce
        signed
          `shouldBe` ResponseSignMsg
            "hello"
            "IJ6CSrFWDRjgP/CfS+0UTM6ziJ2pil12tGj0xLBzPTEGeN2FipE7Y/tWxGnLliGzRObBxkgxKXQEZEgCmoeWeMo="
            (externalAddrsText !! 0)

    it "is refused after three seeds that are not the account's" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        let otherSeed = [mnemonicWords testMnemonic2, mnemonicPass testMnemonic2]
        (errorOf <$> hwJson home ["signmsg", textOf (externalAddr 0), "hello"] (concat (replicate 3 otherSeed)))
          `shouldReturn` Right "The mnemonic or the passphrase does not match the account"

    it "is refused for an address the wallet does not have, before the mnemonic is asked" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["signmsg", textOf (otherAddr 0), "hello"] seedOnce
        run `shouldSatisfy` printed "Address does not exist"
        run `shouldNotSatisfy` askedMnemonic

{- Failures -}

-- | A transaction of the test wallet, unsigned, as a sweep of a coin it
-- does not hold leaves it
unsignedSweep :: Ctx -> TxSignData
unsignedSweep ctx = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 1000)]) [] [] [] (Just 1) Nothing

-- | A payment of the test wallet to another wallet, unsigned
unsignedPayment :: Ctx -> TxSignData
unsignedPayment ctx =
  TxSignData (spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 40000)]) [funding] [Deriv :/ 0 :/ 0] [] Nothing Nothing
  where
    funding = fundingTx ctx 1 [(externalAddr 0, 50000)]

-- | The same payment signed by the test wallet, which another wallet holds
signedPayment :: Ctx -> TxSignData
signedPayment ctx = signedByWallet ctx (unsignedPayment ctx)

failureSpec :: Ctx -> Spec
failureSpec ctx = do
  describe "A command that fails" $ do
    it "exits with 1, and tells why" $
      withHome $ \home -> do
        run <- hw home ["receive", "--account", "nope"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The account nope does not exist"

    -- A script may read the exit code or the JSON: both tell the failure.
    -- hw exited with 0 on an error in JSON before.
    it "exits with 1 with --json too, the error in its JSON" $
      withHome $ \home -> do
        run <- hw home ["receive", "--account", "nope", "--json"] []
        runExit run `shouldBe` ExitFailure 1
        (errorOf <$> hwJson home ["receive", "--account", "nope"] []) `shouldReturn` Right "The account nope does not exist"

    it "refuses an amount, with its reason" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "1.5sat"] [])
          `shouldReturn` Right "The amount 1.5sat has decimals, but an amount in sats is whole"

    it "is refused an option it does not know, before the database is touched" $
      withHome $ \home -> do
        run <- hw home ["accounts", "--unit", "eur"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --unit: cannot parse value `eur'"
        doesFileExist (home </> ".hw" </> "accounts.sqlite") `shouldReturn` False

    -- An output has an index of 32 bits. The index 2^32 wrapped around to
    -- 0, and hw spent the first output of the transaction instead.
    it "is refused a --coin index no output has, rather than spending another coin" $
      withHome $ \home -> do
        let coin = Text.unpack (txHashToHex (fakeTxHash 1)) <> " 4294967296"
        forM_ [["preparetx", textOf (otherAddr 0), "0.0002"], ["preparesweep", "--sweepto", textOf (otherAddr 0)]] $ \command -> do
          run <- hw home (command <> ["--coin", coin]) []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf ("option --coin: cannot parse value `" <> Text.pack coin <> "'")

    it "is cancelled when the input ends before the answer, and leaves nothing" $
      withHome $ \home -> do
        (errorOf <$> hwJson home ["createacc", "main"] [mnemonicWords testMnemonic]) `shouldReturn` Right "Cancelled: the input ended before the answer"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

    -- A rate of 2^64 wrapped around to zero: preparetx made a transaction
    -- without fee, which signtx refused
    it "refuses a fee rate past 32 bits, rather than wrapping around" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "0.0002", "--fee", "18446744073709551616"] [])
          `shouldReturn` Right "The fee rate must be at most 4294967295 satoshis per byte"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- preparetx takes a change address before it chooses the coins, which
    -- do not cover the 100000 satoshis here. The failure undoes it, with
    -- everything else the command wrote.
    it "leaves nothing of what it wrote before it failed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "0.001"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"
        (addressesOf <$> hwJson home ["addrs", "--internal"] []) `shouldReturn` Right []
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- The store of the test home refuses every connection. hw crashed with
    -- the exception of the web client before, and wrote no JSON.
    it "tells when the store is out of reach, in its JSON too" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["sync"] [])
          `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed"

    -- The web client decodes an answer only once hw uses it: hw crashed
    -- there before, with the error of the decoder
    it "tells when the store gives an answer it cannot read, as a server that is not a haskoin-store does" $
      withServerAnswering "not a store" $ \home url -> do
        createMain home
        failure <- errorOf <$> hwJson home ["sync"] []
        failure `shouldSatisfy` either (const False) (Text.isPrefixOf ("The store at " <> Text.pack url <> " gave an answer hw could not read: "))

    -- hw crashed with the exception of the system before, and wrote no JSON
    it "tells when a file it reads does not exist, naming it" $
      withHome $ \home -> do
        createMain home
        forM_ ["reviewtx", "importtx", "importacc"] $ \command ->
          (errorOf <$> hwJson home [command, home </> "missing.json"] [])
            `shouldReturn` Right (noSuchFile (home </> "missing.json"))

    it "tells when a file it writes has no directory to go in, naming it" $
      withHome $ \home -> do
        createMain home
        let file = home </> "missing" </> "main.json"
        (errorOf <$> hwJson home ["exportacc", file] [])
          `shouldReturn` Right ("The file " <> Text.pack file <> " could not be written: no such file or directory")

    -- preparetx writes its file last, once the transaction is pending and
    -- its change address taken: the failure undoes both
    it "keeps nothing of a transaction whose file could not be written" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let file = home </> "missing" </> "unsigned.json"
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "0.0002", "--output", file] [])
          `shouldReturn` Right ("The file " <> Text.pack file <> " could not be written: no such file or directory")
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
        (addressesOf <$> hwJson home ["addrs", "--internal"] []) `shouldReturn` Right []

    -- The other command holds the database the whole time: hw waits its
    -- three seconds, then gives up
    it "waits for another command that holds the database, and says so when it does not finish" $
      withHome $ \home -> do
        held <- newEmptyMVar
        release <- newEmptyMVar
        holder <- async $ runDBWith 100 (home </> ".hw" </> "accounts.sqlite") $ do
          liftIO $ putMVar held ()
          liftIO $ takeMVar release
        takeMVar held
        run <- hw home ["accounts"] []
        putMVar release ()
        wait holder
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed (Text.pack databaseInUse)

  describe "sendtx from a file" $ do
    it "needs a nosigHash or a file" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["sendtx"] []) `shouldReturn` Right "Give the NOSIGHASH of a pending transaction or an --input file"

    it "takes a nosigHash or a file, not both" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["sendtx", Text.unpack (txHashToHex (fakeTxHash 1)), "--input", home </> "sweep.json"] [])
          `shouldReturn` Right "Can not specify both a NOSIGHASH and an --input file"

    it "refuses a missing file" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["sendtx", "--input", home </> "missing.json", "--network", "btc"] [])
          `shouldReturn` Right (noSuchFile (home </> "missing.json"))

    it "refuses an unsigned transaction" $
      withHome $ \home -> do
        Json.encodeFile (home </> "sweep.json") (unsignedSweep ctx)
        (errorOf <$> hwJson home ["sendtx", "--input", home </> "sweep.json", "--network", "btc"] [])
          `shouldReturn` Right "The transaction is not signed"

    it "needs the network of a signed transaction the wallet does not hold" $
      withHome $ \home -> do
        Json.encodeFile (home </> "payment.json") (signedPayment ctx)
        (errorOf <$> hwJson home ["sendtx", "--input", home </> "payment.json"] [])
          `shouldReturn` Right "Give the --network of a transaction the wallet does not hold"

  describe "signtx and signsweep" $ do
    let nosig = Text.unpack (txHashToHex (fakeTxHash 1))
    it "need a nosigHash, or an --input file with an --output file" $
      withHome $ \home ->
        forM_ ["signtx", "signsweep"] $ \command -> do
          (errorOf <$> hwJson home [command] [])
            `shouldReturn` Right "Give the NOSIGHASH of a pending transaction, or an --input file and an --output file"
          (errorOf <$> hwJson home [command, nosig, "--input", home </> "unsigned.json", "--output", home </> "signed.json"] [])
            `shouldReturn` Right "Can not specify both a NOSIGHASH and an --input file"
          (errorOf <$> hwJson home [command, "--input", home </> "unsigned.json"] [])
            `shouldReturn` Right "Give an --output file with the --input file"

    it "refuse a missing --input file" $
      withHome $ \home ->
        forM_ ["signtx", "signsweep"] $ \command ->
          (errorOf <$> hwJson home [command, "--input", home </> "missing.json", "--output", home </> "signed.json"] [])
            `shouldReturn` Right (noSuchFile (home </> "missing.json"))

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
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", textOf (otherAddr 0), "0.0002"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["sendtx", nosig] []
        run <- hw home ["signtx", nosig] seedOnce
        run `shouldSatisfy` printed "The transaction is already online"
        run `shouldNotSatisfy` askedMnemonic

    it "refuses an --output file that exists" $
      withHome $ \home -> do
        createMain home
        Json.encodeFile (home </> "payment.json") (unsignedPayment ctx)
        writeFile (home </> "signed.json") ""
        run <- hw home ["signtx", "--input", home </> "payment.json", "--output", home </> "signed.json"] seedOnce
        run `shouldSatisfy` printed (alreadyExists (home </> "signed.json"))
        run `shouldNotSatisfy` askedMnemonic

{- Backups and old databases -}

-- | A database of version 3 holding the account main of the test wallet,
-- in its wallet 892eb8e4, with its first receiving address
version3Main :: FilePath -> IO ()
version3Main file =
  inDatabaseFile file $ do
    createTables V3.migrateAll
    P.insert_ $ V3.DBVersion 3
    P.insert_ $ V3.DBWallet "892eb8e4"
    P.insert_ $ V3.DBAccount "main" wallet 0 "btc" "/44'/0'/0'" 1 0 testAccountXPub 0 0 0 fixtureTime
    P.insert_ $ V3.DBAddress 0 wallet "/44'/0'/0'" "/0/0" (externalAddrsText !! 0) "" 0 0 0 0 0 False False fixtureTime
  where
    wallet = V3.DBWalletKey "892eb8e4"

backupSpec :: Spec
backupSpec =
  describe "The database" $ do
    it "is copied by backup to a file, a database with the accounts of the wallet" $
      withHome $ \home -> withHome $ \restored -> do
        createMain home
        (shownFile <$> hwJson home ["backup", home </> "copy.sqlite"] []) `shouldReturn` Right (home </> "copy.sqlite")
        copyFile (home </> "copy.sqlite") (restored </> ".hw" </> "accounts.sqlite")
        (accountsOf <$> hwJson restored ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    it "is not copied over a file that exists" $
      withHome $ \home -> do
        createMain home
        writeFile (home </> "copy.sqlite") ""
        (errorOf <$> hwJson home ["backup", home </> "copy.sqlite"] [])
          `shouldReturn` Right (alreadyExists (home </> "copy.sqlite"))

    -- hw crashed with the exception of SQLite before
    it "is not copied into a directory that does not exist, and hw says so" $
      withHome $ \home -> do
        createMain home
        let copy = home </> "missing" </> "copy.sqlite"
        (errorOf <$> hwJson home ["backup", copy] [])
          `shouldReturn` Right ("The database could not be copied to " <> Text.pack copy <> ": the file could not be created")

    it "is copied by backup into the backups of hw without a file" $
      withHome $ \home -> do
        createMain home
        copy <- shownFile <$> hwJson home ["backup"] []
        copy `shouldSatisfy` either (const False) ((home </> ".hw" </> "backups") `isInfixOf`)

    it "is migrated from version 3 by the first command, backed up first" $
      withHome $ \home -> do
        version3Main (home </> ".hw" </> "accounts.sqlite")
        run <- hw home ["accounts"] []
        runExit run `shouldBe` ExitSuccess
        runErr run `shouldSatisfy` Text.isInfixOf "Migrating the database to version 4: the accounts identified by their keys, without wallets"
        runErr run `shouldSatisfy` Text.isInfixOf "The database at version 3 was backed up to"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]
        (addressesOf <$> hwJson home ["addrs"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]
        hwJson home ["version"] [] `shouldReturn` ResponseVersion "0.10.0" "4"
  where
    shownFile (ResponseFile file) = Right file
    shownFile other = Left (show other)

{- Receiving -}

-- | A payment of 50000 satoshis from another wallet to the first receiving
-- address of the test wallet
payment :: Ctx -> Tx
payment ctx = fundingTx ctx 1 [(externalAddr 0, 50000)]

-- | A chain whose block 800000 holds the payment, two blocks below its best
paidChain :: Ctx -> Chain
paidChain ctx = Chain 800002 [(payment ctx, Just 800000)]

-- | A home with the account main, its first receiving address handed out,
-- and synced with the chain of the store
synced :: FilePath -> IO ()
synced home = do
  createMain home
  _ <- answer home ["receive"] []
  void $ answer home ["sync"] []

receivingSpec :: Ctx -> Spec
receivingSpec ctx = do
  describe "A payment to the wallet" $ do
    it "is found by sync, which tells what the account can spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 50000)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 50000)]

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

  describe "A coin locked by the user" $ do
    let twoCoins = fundingTx ctx 1 [(externalAddr 0, 50000), (externalAddr 0, 20000)]
        chain = Chain 800002 [(twoCoins, Just 800000)]
        firstCoin = OutPoint (txHash twoCoins) 0
        coinArgs = [Text.unpack (txHashToHex (txHash twoCoins)), "0"]
        locks = fmap (fmap (\(outpoint, _, _, lock, _) -> (outpoint, lock)))
    it "is locked by lockcoin and freed by unlockcoin, which refuses it then" $
      withChain ctx chain $ \home _ -> do
        synced home
        (locks . coinsShown <$> hwJson home ("lockcoin" : coinArgs) []) `shouldReturn` Right [(firstCoin, Just LockedByUser)]
        (locks . coinsShown <$> hwJson home ("unlockcoin" : coinArgs) []) `shouldReturn` Right [(firstCoin, Nothing)]
        (errorOf <$> hwJson home ("unlockcoin" : coinArgs) []) `shouldReturn` Right "The coin is not locked"

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
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "0.0004"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

{- Signing -}

-- | A signer holding the account main, and an online computer holding the
-- same account, imported from the signer, and synced with the chain
signerAndOnline :: FilePath -> FilePath -> IO ()
signerAndOnline signer online = do
  createMain signer
  _ <- answer signer ["exportacc", signer </> "main.json"] []
  _ <- answer online ["importacc", signer </> "main.json"] []
  _ <- answer online ["receive"] []
  void $ answer online ["sync"] []

-- | What the payment of 20000 satoshis spends and pays, as the store sees
-- it once sent: the coin of the payment to the wallet, and 27740 of change
paymentSent :: Ctx -> ([OutPoint], [(Word64, Either String Address)])
paymentSent ctx = ([OutPoint (txHash (payment ctx)) 0], [(20000, Right (otherAddr 0)), (27740, Right (internalAddr 0))])

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
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", textOf (otherAddr 0), "0.0002"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        sent <- txOf =<< answer home ["sendtx", nosig] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        modifyIORef chain mine
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 27740)]

    -- The wallet has two accounts of the test seed, and the payment is
    -- pending in main. Without --account, hw asked which account to use.
    it "is signed by the account that holds it, without --account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", textOf (otherAddr 0), "0.0002", "--account", "main"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)

    -- The mnemonic of second would be typed for nothing: its key cannot
    -- sign what main holds
    it "is refused an --account that does not hold it, before the mnemonic is asked" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", textOf (otherAddr 0), "0.0002", "--account", "main"] []
        run <- hw home ["signtx", nosig, "--account", "second"] seedOnce
        run `shouldSatisfy` printed "The transaction is pending in the account main, not in second"
        run `shouldNotSatisfy` askedMnemonic

  -- The signer holds the account and asks for its mnemonic; the online
  -- computer holds the same account, imported, and talks to the store
  describe "A payment signed offline" $ do
    it "is prepared online, signed offline, sent online, and confirmed by a sync" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        prepared <- txOf =<< answer online ["preparetx", textOf (otherAddr 0), "0.0002", "--output", online </> "unsigned.json"] []
        pendingState prepared `shouldBe` Just (False, False)
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]
        summaryFee (txInfoSummary prepared) `shouldBe` 2260
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        sent <- txOf =<< answer online ["sendtx", "--input", signer </> "signed.json"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]
        modifyIORef chain mine
        (accountsOf <$> hwJson online ["sync"] []) `shouldReturn` Right [("main", 0, 27740)]
        (txsOf <$> hwJson online ["pendingtxs"] []) `shouldReturn` Right []
        (txsOf <$> hwJson online ["txs"] []) `shouldReturn` Right [(TxDebit, -22260, Just 1), (TxCredit, 50000, Just 4)]
        (addressesOf <$> hwJson online ["addrs", "--internal"] []) `shouldReturn` Right [(internalAddrsText !! 0, "")]

    -- Prepared without a file, the transaction is written out by exporttx
    it "is exported by exporttx, signed offline, imported back by importtx, and sent by its nosigHash" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", textOf (otherAddr 0), "0.0002"] []
        answer online ["exporttx", nosig, online </> "unsigned.json"] [] `shouldReturn` ResponseFile (online </> "unsigned.json")
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        imported <- txOf =<< answer online ["importtx", signer </> "signed.json"] []
        pendingState imported `shouldBe` Just (True, False)
        sent <- txOf =<< answer online ["sendtx", nosig] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [paymentSent ctx]

    -- The signer has no coins: the file carries the transactions the
    -- payment spends, and the paths of the change
    it "is shown by reviewtx from its file, as the signer reads it before signing, and not kept" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", textOf (otherAddr 0), "0.0002", "--output", online </> "unsigned.json"] []
        reviewed <- txOf =<< answer signer ["reviewtx", online </> "unsigned.json"] []
        paidToOthers reviewed `shouldBe` Map.fromList [(otherAddr 0, 20000)]
        paidToWallet reviewed `shouldBe` Map.fromList [(internalAddr 0, 27740)]
        summaryFee (txInfoSummary reviewed) `shouldBe` 2260
        (txsOf <$> hwJson signer ["pendingtxs"] []) `shouldReturn` Right []

    -- The online wallet has the account from a document without the
    -- fingerprint of its seed, as hw wrote them before it recorded
    -- fingerprints. bc560f95 is the fingerprint of the test seed.
    it "names the seed in the signed file, which the online wallet learns as it sends it" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        createMain signer
        Json.encodeFile (online </> "main.json") $
          Json.object ["xpubkey" Json..= testAccountXPub, "network" Json..= ("btc" :: Text), "name" Json..= ("main" :: Text)]
        _ <- answer online ["importacc", online </> "main.json"] []
        _ <- answer online ["receive"] []
        _ <- answer online ["sync"] []
        (fingerprintsOf <$> hwJson online ["accounts"] []) `shouldReturn` Right [("main", Nothing)]
        _ <- answer online ["preparetx", textOf (otherAddr 0), "0.0002", "--output", online </> "unsigned.json"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["sendtx", "--input", signer </> "signed.json"] []
        (fingerprintsOf <$> hwJson online ["accounts"] []) `shouldReturn` Right [("main", Just "bc560f95")]

{- Amounts and fees -}

paymentSpec :: Ctx -> Spec
paymentSpec ctx =
  describe "A payment prepared by preparetx" $ do
    -- The default unit is the bitcoin; --unit changes it, and a unit after
    -- the amount wins over both
    it "takes its amount in the unit of --unit, or in the one typed after it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        forM_ [["0.0002"], ["20000", "--unit", "sat"], ["0.0002btc"], ["200bits"], ["0.0002btc", "--unit", "sat"]] $ \amount -> do
          prepared <- txOf =<< answer home (["preparetx", textOf (otherAddr 0)] <> amount) []
          paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000)]
          nosig <- nosigOf prepared
          void $ answer home ["deletetx", nosig] []

    -- 226 bytes at 20 satoshis a byte
    it "pays the fee rate of --fee for each byte" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", textOf (otherAddr 0), "0.0002", "--fee", "20"] []
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary, summarySize summary) `shouldBe` (4520, 20, 226)

    -- The fee of 2260 comes out of the 20000 the recipient gets
    it "takes the fee from the recipient with --recipientpay" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", textOf (otherAddr 0), "0.0002", "--recipientpay"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 17740)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 30000)]

    it "is refused for more than the coins of the account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "0.001"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

    -- The path is checked first: the coins would not cover these 100000
    -- satoshis either
    it "is refused an --output file that exists, before it is built" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        writeFile (home </> "taken.json") ""
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "0.001", "--output", home </> "taken.json"] [])
          `shouldReturn` Right (alreadyExists (home </> "taken.json"))
        readFile (home </> "taken.json") `shouldReturn` ""

{- Recovering a seed -}

recoverSpec :: Ctx -> Spec
recoverSpec ctx =
  describe "The accounts of a seed recovered by recover" $ do
    it "are those paid, named after their network and index, and synced" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (accountsOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right [("btc0", 0, 50000)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", 0, 50000)]

    it "leave out an account the wallet has" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        (accountsOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right []
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    -- Every account searched is a scan at the store
    it "are looked for with an account gap from 1 to 100" $
      withHome $ \home -> do
        forM_ ["0", "101"] $ \gap -> do
          run <- hw home ["recover", "--account-gap", gap] seedOnce
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf "The account gap is a number of accounts, from 1 to 100"
        -- 100 is taken: recover goes on to the store, out of reach here
        run <- hw home ["recover", "--account-gap", "100"] seedOnce
        run `shouldSatisfy` printed "The store at http://127.0.0.1:1/ could not be reached: the connection failed"

    -- The store of the test home refuses every connection, as on an offline
    -- computer, where the mnemonic would be typed for nothing
    it "are not looked for when the store is out of reach, which recover asks before the mnemonic" $
      withHome $ \home -> do
        run <- hw home ["recover"] seedOnce
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The store at http://127.0.0.1:1/ could not be reached: the connection failed"
        run `shouldNotSatisfy` askedMnemonic

{- Sweeping -}

-- | The key of the Bitcoin wiki's "Wallet import format" page, in WIF for
-- its compressed public key, and the address of that key, computed with
-- test/data/message-signing/tools/btc.py
wikiKeyWif :: Text
wikiKeyWif = "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617"

wikiAddr :: Address
wikiAddr = testAddress "1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK"

-- | A coin of 30000 satoshis at the address of the key
oldCoin :: Ctx -> Tx
oldCoin ctx = fundingTx ctx 2 [(wikiAddr, 30000)]

-- | A chain whose block 800000 holds the coin of the key
oldKeyChain :: Ctx -> Chain
oldKeyChain ctx = Chain 800002 [(oldCoin ctx, Just 800000)]

-- | Write the key to a file in a home, as the user keeps it, with a note
-- after it, and give its path
writeKeyFile :: FilePath -> IO FilePath
writeKeyFile home = do
  writeFile (home </> "keys.txt") (Text.unpack wikiKeyWif <> " old wallet\n")
  return (home </> "keys.txt")

-- The coin of 30000 at the address of the key is swept at 10 satoshis a
-- byte: one input and one output make 192 bytes, a fee of 1920, and 28080
-- for the destination.
sweepSpec :: Ctx -> Spec
sweepSpec ctx =
  describe "A sweep of an old key" $ do
    it "is prepared by preparesweep, signed by signsweep with the key, sent, and paid to the wallet" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0)] []
        pendingState prepared `shouldBe` Just (False, False)
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]
        nosig <- nosigOf prepared
        signed <- txOf =<< answer home ["signsweep", nosig, "--prvkeys", keys] []
        pendingState signed `shouldBe` Just (True, False)
        _ <- answer home ["sendtx", nosig] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (externalAddr 0))])]
        modifyIORef chain mine
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 28080)]

    it "that pays outside the wallet is refused without an --output file" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0)] [])
          `shouldReturn` Right "The sweep does not involve this wallet: give an --output file to write it to"

    -- The wallet keeps no trace of it: the sweep is signed from its file,
    -- and sent with the network sendtx cannot read from the wallet
    it "that pays outside the wallet is written to its file only, signed from it, and sent with its network" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        createMain home
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--output", home </> "sweep.json"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 28080)]
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
        signed <- txOf =<< answer home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.json", "--prvkeys", keys] []
        pendingState signed `shouldBe` Just (True, False)
        sent <- answer home ["sendtx", "--input", home </> "signed.json", "--network", "btc"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (otherAddr 0))])]
        (ResponseSentTx btc . txHash <$> published) `shouldBe` [sent]

    -- The keys would be typed for nothing
    it "is refused an --output file that exists, before the keys are typed" $
      withHome $ \home -> do
        createMain home
        writeFile (home </> "sweep.json") ""
        run <- hw home ["preparesweep", "--prvkeys", "-", "--sweepto", textOf (otherAddr 0), "--output", home </> "sweep.json"] []
        run `shouldSatisfy` printed (alreadyExists (home </> "sweep.json"))
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- The keys would be typed for nothing
    it "is refused an address that is not one, before the keys are typed" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["preparesweep", "--prvkeys", "-", "--sweepto", "nonsense"] []
        run `shouldSatisfy` printed "The address nonsense is not valid on btc"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- A rate of 2^64 wrapped around to zero, and others to any fee
    it "is refused a fee rate past 32 bits, rather than wrapping around" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--fee", "18446744073709551616"] [])
          `shouldReturn` Right "The fee rate must be at most 4294967295 satoshis per byte"

    -- The wallet has two accounts of the test seed, and the sweep is
    -- pending in main
    it "is signed by the account that holds it, without --account" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "main"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--account", "main", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0)] []
        signed <- txOf =<< answer home ["signsweep", nosig, "--prvkeys", keys] []
        pendingState signed `shouldBe` Just (True, False)

{- Listings -}

-- | How many rows a listing shows
rowsOf :: Response -> Either String Int
rowsOf (ResponseAddresses _ addrs) = Right (length addrs)
rowsOf (ResponseTxs _ txs) = Right (length txs)
rowsOf (ResponseCoins _ coins) = Right (length coins)
rowsOf other = Left (show other)

-- | The listings, each with one row in a synced home with a payment
-- prepared: the address paid, the payment received, its coin, and the
-- payment prepared
listings :: [String]
listings = ["addrs", "txs", "coins", "pendingtxs"]

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "A listing" $ do
    -- 2^64 wrapped around to 0
    it "shows nothing at an offset past 64 bits, rather than the first page" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", textOf (otherAddr 0), "0.0002"] []
        forM_ listings $ \listing ->
          (rowsOf <$> hwJson home [listing, "--offset", "18446744073709551616"] []) `shouldReturn` Right 0

    -- 2^64 + 1 wrapped around to 1, and showed one address of the three
    it "shows every row with a limit past 64 bits, rather than wrapping around" $
      withHome $ \home -> do
        createMain home
        replicateM_ 3 $ answer home ["receive"] []
        (rowsOf <$> hwJson home ["addrs", "--limit", "18446744073709551617"] []) `shouldReturn` Right 3
