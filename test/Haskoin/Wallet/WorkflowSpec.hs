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
import Haskoin.Wallet.Commands (Response (..))
import Haskoin.Wallet.Database (DBAccount (..), DBAddress (..), databaseInUse, runDBWith)
import Haskoin.Wallet.FileIO (writeJsonFile)
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
  offlineSigningSpec ctx
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
        runErr run `shouldNotSatisfy` Text.isInfixOf "Mnemonic"

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
        runErr run `shouldNotSatisfy` Text.isInfixOf "Mnemonic"

{- Failures -}

-- | A transaction of the test wallet, unsigned, as a sweep of a coin it
-- does not hold leaves it
unsignedSweep :: Ctx -> TxSignData
unsignedSweep ctx = TxSignData (spendingTx ctx [(fakeTxHash 1, 0)] [(otherAddr 0, 1000)]) [] [] [] (Just 1) Nothing

-- | A payment signed by the test wallet, which another wallet holds
signedPayment :: Ctx -> TxSignData
signedPayment ctx =
  signedByWallet ctx $ TxSignData (spendingTx ctx [(txHash funding, 0)] [(otherAddr 0, 40000)]) [funding] [Deriv :/ 0 :/ 0] [] Nothing Nothing
  where
    funding = fundingTx ctx 1 [(externalAddr 0, 50000)]

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

    it "is cancelled when the input ends before the answer, and leaves nothing" $
      withHome $ \home -> do
        (errorOf <$> hwJson home ["createacc", "main"] [mnemonicWords testMnemonic]) `shouldReturn` Right "Cancelled: the input ended before the answer"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

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
          `shouldReturn` Right "Input file does not exist"

    it "refuses an unsigned transaction" $
      withHome $ \home -> do
        writeJsonFile (home </> "sweep.json") (Json.toJSON (unsignedSweep ctx))
        (errorOf <$> hwJson home ["sendtx", "--input", home </> "sweep.json", "--network", "btc"] [])
          `shouldReturn` Right "The transaction is not signed"

    it "needs the network of a signed transaction the wallet does not hold" $
      withHome $ \home -> do
        writeJsonFile (home </> "payment.json") (Json.toJSON (signedPayment ctx))
        (errorOf <$> hwJson home ["sendtx", "--input", home </> "payment.json"] [])
          `shouldReturn` Right "Give the --network of a transaction the wallet does not hold"

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
          `shouldReturn` Right ("Path '" <> Text.pack (home </> "copy.sqlite") <> "' already exists")

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

{- Signing offline -}

offlineSigningSpec :: Ctx -> Spec
offlineSigningSpec ctx =
  describe "A payment signed offline" $
    -- The signer holds the account and asks for its mnemonic; the online
    -- computer holds the same account, imported, and talks to the store.
    -- 20000 satoshis are paid at 10 satoshis a byte: the transaction of one
    -- input and two outputs is 226 bytes, so the fee is 2260, and the
    -- change 50000 - 20000 - 2260 = 27740.
    it "is prepared online, signed offline, sent online, and confirmed by a sync" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        createMain signer
        _ <- answer signer ["exportacc", signer </> "main.json"] []
        _ <- answer online ["importacc", signer </> "main.json"] []
        _ <- answer online ["receive"] []
        _ <- answer online ["sync"] []
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
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (payment ctx)) 0], [(20000, Right (otherAddr 0)), (27740, Right (internalAddr 0))])]
        modifyIORef chain mine
        (accountsOf <$> hwJson online ["sync"] []) `shouldReturn` Right [("main", 0, 27740)]
        (txsOf <$> hwJson online ["pendingtxs"] []) `shouldReturn` Right []
        (txsOf <$> hwJson online ["txs"] []) `shouldReturn` Right [(TxDebit, -22260, Just 1), (TxCredit, 50000, Just 4)]
        (addressesOf <$> hwJson online ["addrs", "--internal"] []) `shouldReturn` Right [(internalAddrsText !! 0, "")]

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

{- Sweeping -}

-- | The key of the Bitcoin wiki's "Wallet import format" page, in WIF for
-- its compressed public key, and the address of that key, computed with
-- test/data/message-signing/tools/btc.py
wikiKeyWif :: Text
wikiKeyWif = "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617"

wikiAddr :: Address
wikiAddr = testAddress "1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK"

sweepSpec :: Ctx -> Spec
sweepSpec ctx =
  describe "A sweep of an old key" $
    -- The coin of 30000 at the address of the key goes to the wallet, at 10
    -- satoshis a byte: one input and one output make 192 bytes, a fee of
    -- 1920, and 28080 for the wallet
    it "is prepared by preparesweep, signed by signsweep with the key, sent, and paid to the wallet" $ do
      let oldCoin = fundingTx ctx 2 [(wikiAddr, 30000)]
      withChain ctx (Chain 800002 [(oldCoin, Just 800000)]) $ \home chain -> do
        createMain home
        _ <- answer home ["receive"] []
        writeFile (home </> "keys.txt") (Text.unpack wikiKeyWif <> " old wallet\n")
        prepared <- txOf =<< answer home ["preparesweep", "--prvkeys", home </> "keys.txt", "--sweepto", textOf (externalAddr 0)] []
        pendingState prepared `shouldBe` Just (False, False)
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]
        nosig <- nosigOf prepared
        signed <- txOf =<< answer home ["signsweep", nosig, "--prvkeys", home </> "keys.txt"] []
        pendingState signed `shouldBe` Just (True, False)
        _ <- answer home ["sendtx", nosig] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash oldCoin) 0], [(28080, Right (externalAddr 0))])]
        modifyIORef chain mine
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 28080)]

