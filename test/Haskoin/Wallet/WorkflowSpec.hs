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
import Data.IORef (modifyIORef, readIORef)
import Data.List (findIndex, isInfixOf, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Address (CoinBalance (..), coinBalanceUnspendable)
import Haskoin.Wallet.Coin (CoinLock (..), JsonCoin (..), LeftCoin (..), LeftReason (..))
import Haskoin.Wallet.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..), DBAddress (..), DBRawTx, databaseInUse, runDBWith)
import Haskoin.Wallet.FeeRates (FeeRates (..))
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (MnemonicPass (..))
import qualified Haskoin.Wallet.Schema.V3 as V3
import Haskoin.Wallet.Sync (SyncRes (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData (TxFile (..), TxSignData (..), isSigned)
import Numeric.Natural (Natural)
import System.Directory (copyFile, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Async (async, wait)

spec :: Spec
spec = prepareContext $ \ctx -> do
  accountSpec
  addressSpec ctx
  splitSpec
  messageSpec
  verifySpec
  failureSpec ctx
  backupSpec
  receivingSpec ctx
  listingSpec ctx
  signingSpec ctx
  paymentSpec ctx
  minConfSpec ctx
  recoverSpec ctx
  passphraseSpec ctx
  sweepSpec ctx
  confirmationSpec ctx
  highFeeSpec ctx
  sendAllSpec ctx
  coinControlSpec ctx
  changeBackSpec ctx
  bumpFeeSpec ctx
  feesSpec ctx
  layoutSpec ctx
  pendingListSpec ctx
  balancesListSpec ctx
  nextStepSpec ctx

{- Typing and answers -}

-- | The words and the passphrase of the test wallet, typed once each, as
-- the commands that check a seed ask for them
seedOnce :: [Text]
seedOnce = [mnemonicWords testMnemonic, mnemonicPass testMnemonic]

-- | The same, typed twice each, as createacc asks for them
seedTwice :: [Text]
seedTwice = [mnemonicWords testMnemonic, mnemonicWords testMnemonic, mnemonicPass testMnemonic, mnemonicPass testMnemonic]

-- | The words of the test wallet in their two parts, then its passphrase,
-- typed once each, as a command with --split 2 asks for them
partsOnce :: [Text]
partsOnce = testMnemonicParts <> [mnemonicPass testMnemonic]

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

-- | Import the account main of the test wallet in a home, from a document
-- without the fingerprint of its seed, as hw wrote them before it recorded
-- fingerprints
importWithoutFingerprint :: FilePath -> IO ()
importWithoutFingerprint home = do
  Json.encodeFile (home </> "main.json") $
    Json.object ["xpubkey" Json..= testAccountXPub, "network" Json..= ("btc" :: Text), "name" Json..= ("main" :: Text)]
  void $ answer home ["importacc", home </> "main.json"] []

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
accountsOf (ResponseSync results _) = Right [(dBAccountName (syncResAccount s), dBAccountIndex (syncResAccount s), syncResSpendable s) | s <- results]
accountsOf (ResponseRecover _ _ results) = accountsOf (ResponseSync results [])
accountsOf other = Left (show other)

-- | The accounts a sync left out, each by name with the failure of its
-- store
notSyncedOf :: Response -> Either String [(Text, Text)]
notSyncedOf (ResponseSync _ notSynced) = Right [(dBAccountName acc, failure) | (acc, failure) <- notSynced]
notSyncedOf other = Left (show other)

-- | The account of an answer, with its index and its public key
keyOf :: Response -> Either String (Text, Int, Text)
keyOf (ResponseAccount acc _) = Right (dBAccountName acc, dBAccountIndex acc, dBAccountXPubKey acc)
keyOf other = Left (show other)

-- | The address receive hands out to the account, as text
receiveFor :: FilePath -> String -> IO String
receiveFor home account = do
  shown <- addressesOf <$> answer home ["receive", "--account", account] []
  case shown of
    Right [(addr, _)] -> return (Text.unpack addr)
    other -> fail $ "Not an address: " <> show other

-- | The transaction file at the path, as hw wrote it
readTxFile :: FilePath -> IO TxFile
readTxFile file = maybe (fail ("Not a transaction file: " <> file)) return =<< Json.decodeFileStrict file

-- | The account an answer shows
accountOf :: Response -> Either String DBAccount
accountOf (ResponseAccount acc _) = Right acc
accountOf (ResponseAddress acc _ _) = Right acc
accountOf (ResponseTx acc _) = Right acc
accountOf (ResponsePrepared acc _ _) = Right acc
accountOf other = Left (show other)

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

-- | The transaction of an answer, of an account or outside the wallet
txOf :: Response -> IO TxInfo
txOf (ResponseTx _ info) = return info
txOf (ResponsePrepared _ info _) = return info
txOf (ResponseOutsideTx _ info) = return info
txOf other = fail $ "Not a transaction: " <> show other

-- | The transactions of an answer: type, amount, and confirmations once on
-- the chain
txsOf :: Response -> Either String [(TxType, Integer, Maybe Natural)]
txsOf (ResponseTxs _ infos) = Right (txRow <$> infos)
txsOf (ResponsePendingTxs _ held) = Right (txRow . snd <$> held)
txsOf other = Left (show other)

-- | A transaction as 'txsOf' gives it
txRow :: TxInfo -> (TxType, Integer, Maybe Natural)
txRow i = (summaryType (txInfoSummary i), summaryAmount (txInfoSummary i), confirmations (txInfoStatus i))
  where
    confirmations (OnChain status) = Just (chainConfirmations status)
    confirmations (Pending _) = Nothing

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

-- | What a transaction spends from the wallet, by address
spentFromWallet :: TxInfo -> Map.Map Address Natural
spentFromWallet = fmap myInputsValue . summaryMyInputs . txInfoSummary

-- | The text of an address of the tests
textOf :: Address -> String
textOf = maybe (error "textOf") Text.unpack . addrToText btc

-- | An address and its amount, as preparetx and preparesendall take them:
-- ADDRESS:AMOUNT
fixedTo :: Address -> String -> String
fixedTo addr amount = textOf addr <> ":" <> amount

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

    it "is listed alone by accounts --account" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        (accountsOf <$> hwJson home ["accounts", "--account", "second"] []) `shouldReturn` Right [("second", 1, 0)]

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

    -- The configuration file of the test home names btc
    it "is created on the default network of the configuration file, or on the --network given" $
      withHome $ \home -> do
        setDefaultNetwork home (Just "bch")
        (fmap dBAccountNetwork . accountOf <$> hwJson home ["createacc", "main"] seedTwice) `shouldReturn` Right "bch"
        (fmap dBAccountNetwork . accountOf <$> hwJson home ["createacc", "second", "--network", "btc"] seedTwice) `shouldReturn` Right "btc"

    it "needs the --network without a default network in the configuration file, before the mnemonic is asked" $
      withHome $ \home -> do
        setDefaultNetwork home Nothing
        run <- hw home ["createacc", "main"] seedTwice
        run `shouldSatisfy` printed "Give the --network, or a default-network in the configuration file"
        run `shouldNotSatisfy` askedMnemonic

    it "is checked by testacc against a mnemonic and a passphrase" $
      withHome $ \home -> do
        createMain home
        matched <- hwJson home ["testacc"] seedOnce
        matched `shouldSatisfy` \case ResponseAccResult _ True _ -> True; _ -> False
        missed <- hwJson home ["testacc"] [mnemonicWords testMnemonic, "wrong passphrase"]
        missed `shouldSatisfy` \case ResponseAccResult _ False "The mnemonic and passphrase did not match the account" -> True; _ -> False

    -- other is of the second seed, and main of the first
    it "is checked by testacc --account, whose seed is the one to type" $
      withHome $ \home -> do
        createMain home
        let second = [mnemonicWords testMnemonic2, mnemonicPass testMnemonic2]
        _ <- answer home ["createacc", "other"] (concatMap (replicate 2) second)
        matched <- hwJson home ["testacc", "--account", "other"] second
        matched `shouldSatisfy` \case ResponseAccResult acc True _ -> dBAccountName acc == "other"; _ -> False
        missed <- hwJson home ["testacc", "--account", "other"] seedOnce
        missed `shouldSatisfy` \case ResponseAccResult _ False _ -> True; _ -> False

    it "is checked by testacc --split against the parts of its mnemonic" $
      withHome $ \home -> do
        createMain home
        matched <- hwJson home ["testacc", "--split", "2"] partsOnce
        matched `shouldSatisfy` \case ResponseAccResult _ True _ -> True; _ -> False

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

addressSpec :: Ctx -> Spec
addressSpec ctx =
  describe "The addresses of an account" $ do
    it "are handed out by receive in the order of the receiving chain" $
      withHome $ \home -> do
        createMain home
        forM_ (take 3 externalAddrsText) $ \expected ->
          (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(expected, "")]

    -- The answer read the account before the address was taken, and showed
    -- none counted
    it "are counted in the account receive shows" $
      withHome $ \home -> do
        createMain home
        (fmap dBAccountExternal . accountOf <$> hwJson home ["receive"] []) `shouldReturn` Right 1

    it "take the label given to receive" $
      withHome $ \home -> do
        createMain home
        (addressesOf <$> hwJson home ["receive", "--label", "rent"] []) `shouldReturn` Right [(externalAddrsText !! 0, "rent")]

    it "are listed by addrs newest first, and after a given one when asked" $
      withHome $ \home -> do
        createMain home
        replicateM_ 3 $ answer home ["receive"] []
        (addressesOf <$> hwJson home ["addrs"] [])
          `shouldReturn` Right [(externalAddrsText !! 2, ""), (externalAddrsText !! 1, ""), (externalAddrsText !! 0, "")]
        (addressesOf <$> hwJson home ["addrs", "--max", "1"] []) `shouldReturn` Right [(externalAddrsText !! 2, "")]
        (addressesOf <$> hwJson home ["addrs", "--max", "1", "--after", Text.unpack (externalAddrsText !! 2)] [])
          `shouldReturn` Right [(externalAddrsText !! 1, "")]

    it "are shown by addrs under their derivation paths" $
      withHome $ \home -> do
        createMain home
        replicateM_ 2 $ answer home ["receive"] []
        run <- hw home ["addrs"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/1: " <> externalAddrsText !! 1)
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/0: " <> externalAddrsText !! 0)

    -- The path /0/9 is padded to the width of /0/10
    it "are aligned by addrs when their paths differ in length" $
      withHome $ \home -> do
        createMain home
        replicateM_ 11 $ answer home ["receive"] []
        run <- hw home ["addrs", "--max", "2"] []
        runOut run `shouldSatisfy` Text.isInfixOf "/0/10: "
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/9 : " <> externalAddrsText !! 9)

    -- The payment of 20000 pays its change to the first change address
    it "are shown by addrs --internal under their paths on the change chain" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        run <- hw home ["addrs", "--internal"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/1/0: " <> internalAddrsText !! 0)

    it "are shown by receive under their derivation paths" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["receive"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/0: " <> externalAddrsText !! 0)

    it "are named by label, by their index" $
      withHome $ \home -> do
        createMain home
        replicateM_ 2 $ answer home ["receive"] []
        (addressesOf <$> hwJson home ["label", "1", "salary"] []) `shouldReturn` Right [(externalAddrsText !! 1, "salary")]
        (addressesOf <$> hwJson home ["addrs"] []) `shouldReturn` Right [(externalAddrsText !! 1, "salary"), (externalAddrsText !! 0, "")]

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is the first receiving address of
    -- second, account 1 of the seed
    it "are named by label in the account of --account" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "second"] []
        (addressesOf <$> hwJson home ["label", "0", "rent", "--account", "second"] []) `shouldReturn` Right [("17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "rent")]
        (addressesOf <$> hwJson home ["addrs", "--account", "main"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

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

    -- BIP39 adds a word of 11 bits for every 4 bytes of entropy
    it "has 15, 18 and 21 words for 20, 24 and 28 bytes of --entropy" $
      withHome $ \home ->
        forM_ [("20", 15), ("24", 18), ("28", 21)] $ \(bytes, count) -> do
          generated <- answer home ["mnemonic", "--entropy", bytes] []
          generated `shouldSatisfy` \case ResponseMnemonic _ ws [] -> length ws == count; _ -> False

    it "is refused an --entropy other than 16, 20, 24, 28 or 32 bytes" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--entropy", "17"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --entropy: cannot parse value `17'"

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

{- Fee rates -}

-- | A chain whose latest two blocks hold three transactions: one paying
-- the wallet, of 85 bytes and a fee of 1000 satoshis (11.76 per byte);
-- one of two outputs, of 119 bytes and 1000 satoshis (8.40 per byte); and
-- one spending the latter, of 85 bytes and 10000 satoshis (117.65 per
-- byte). The sizes follow from the serialization: 4 bytes of version, 41
-- per input, 34 per P2PKH output, 4 of lock time, and a count byte each
-- for the inputs and the outputs. Sorted by rate, the 289 bytes reach a
-- quarter within the 119 at 8.40, half within the next 85 at 11.76, and
-- nine tenths within the last 85 at 117.65: rounded up, 9, 12 and 118.
feesChain :: Ctx -> Chain
feesChain ctx =
  Chain
    800001
    [ (fundingTx ctx 1 [(externalAddr 0, 50000)], Just 800000),
      (twoOutputs, Just 800001),
      (spendingTx ctx [(txHash twoOutputs, 0)] [(otherAddr 2, 50000)], Just 800001)
    ]
  where
    twoOutputs = fundingTx ctx 2 [(otherAddr 1, 60000), (otherAddr 1, 70000)]

feesSpec :: Ctx -> Spec
feesSpec ctx =
  describe "The fee rates recent blocks paid" $ do
    it "are listed by fees, with the blocks they come from and the age of the latest" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        run <- hw home ["fees"] []
        forM_
          [ "Fee rates paid in blocks 800000 to 800001 (satoshis per byte):",
            "Low:     9",
            "Median:  12",
            "High:    118",
            "The latest of these blocks was mined 3 minutes ago."
          ]
          $ \line -> run `shouldSatisfy` printed line
        answered <- answer home ["fees"] []
        case answered of
          ResponseFees net rates ->
            (net.name, rates.rateLow, rates.rateMedian, rates.rateHigh, rates.ratesFromHeight, rates.ratesToHeight)
              `shouldBe` ("btc", 9, 12, 118, 800000, 800001)
          other -> expectationFailure $ "Not a fees answer: " <> show other

    -- The configuration file of the test home names btc, and the fake store
    -- serves the same chain on every network
    it "are those of the --network given" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        answered <- answer home ["fees", "--network", "bch"] []
        case answered of
          ResponseFees net _ -> net.name `shouldBe` "bch"
          other -> expectationFailure $ "Not a fees answer: " <> show other

    it "are shown by preparetx without --feerate, which asks which to take, the median by default" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.json"
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file] [""]
        run `shouldSatisfy` printed "Median:  12"
        run `shouldSatisfy` printed "Which fee rate? Type low, median or high [median]"
        (txSignDataFeeRate . txFileData <$> readTxFile file) `shouldReturn` Just 12

    it "are taken at the level typed" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.json"
        _ <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file] ["high", "y"]
        (txSignDataFeeRate . txFileData <$> readTxFile file) `shouldReturn` Just 118

    it "are taken at the level of --feerate without asking" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        synced home
        let file = home </> "unsigned.json"
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file, "--feerate", "low"] []
        run `shouldSatisfy` printed "The low rate recent blocks paid: 9 satoshis per byte"
        run `shouldNotSatisfy` printed "Which fee rate?"
        (txSignDataFeeRate . txFileData <$> readTxFile file) `shouldReturn` Just 9

    it "stop preparetx without --feerate when the store cannot be reached: hw never guesses a rate" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002"] [])
          `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed. Give a --feerate to go on without the rates recent blocks paid"

    it "are offered to bumpfee only above what a replacement must pay" $
      withChain ctx (feesChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        let file = home </> "replacement.json"
        run <- hw home ["bumpfee", stuck, "--output", file] [""]
        run `shouldSatisfy` printed "The transaction pays 10 satoshis per byte: a replacement must pay at least 11"
        run `shouldSatisfy` printed "Which fee rate? Type median or high [median]"
        (txSignDataFeeRate . txFileData <$> readTxFile file) `shouldReturn` Just 12

{- Signed messages -}

messageSpec :: Spec
messageSpec =
  describe "A message signed by signmsg" $ do
    it "is signed by the key of an address of the wallet, from its mnemonic" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hwJson home ["signmsg", textOf (externalAddr 0), "hello"] seedOnce
        signed `shouldBe` ResponseSignMsg "hello" helloSignature (externalAddrsText !! 0)

    -- A signature of the key is always the same
    it "is signed from the parts of the mnemonic with --split" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hwJson home ["signmsg", textOf (externalAddr 0), "hello", "--split", "2"] partsOnce
        signed `shouldBe` ResponseSignMsg "hello" helloSignature (externalAddrsText !! 0)

    -- cash is the Bitcoin Cash account of the test wallet. hw stores the
    -- address /0/0 of cash as
    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u; it is typed
    -- here without its prefix, and in the legacy form. The signature is
    -- checked apart from hw as above: the key it recovers is the one of the
    -- address in both forms, m/44'/145'/0'/0/0 of the test seed.
    it "is signed by the key of a Bitcoin Cash address however it is written" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        _ <- answer home ["receive"] []
        forM_ ["qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ"] $ \written ->
          hwJson home ["signmsg", written, "hello"] seedOnce
            `shouldReturn` ResponseSignMsg
              "hello"
              "H98t/71A7/ED0lRtdsnAiVwcgUv+5sZr7WJwpHS3YFcCcTKtSG/rx2ooIXOTMbdhApTDjcfGjTu3Sjgh6Pl5qQc="
              (Text.pack written)

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

{- Verified messages -}

verifySpec :: Spec
verifySpec =
  describe "A signature checked by verifymsg" $ do
    it "is valid for the message as signed, without the wallet" $
      withHome $ \home ->
        hwJson home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello"] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "is invalid for a changed message, and hw exits with a failure" $
      withHome $ \home -> do
        run <- hw home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello!"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "Invalid"
        hwJson home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello!"] []
          `shouldReturn` ResponseVerifyMsg "hello!" helloSignature (externalAddrsText !! 0) False

    it "warns about the whitespace of a message found invalid" $
      withHome $ \home -> do
        run <- hw home ["verifymsg", textOf (externalAddr 0), Text.unpack helloSignature, "hello "] []
        run `shouldSatisfy` printed "Warning: The message has leading or trailing whitespace"

    -- The signature of Bitcoin Core's rpc_signmessagewithprivkey test, by a
    -- regtest key: its P2PKH address reads on testnet
    it "reads the address on the network given, the default one otherwise" $
      withHome $ \home -> do
        let signature = "INbVnW4e6PeRmsv2Qgu8NuopvrVjkcxob+sX8OcZG0SALhWybUjzMLPdAsXI46YZGb0KQTRii+wWIQzRpG/U+S0="
            address = "mpLQjfK79b7CCV4VMJWEWAj5Mpx8Up5zxB"
            message = "This is just a test message"
        hwJson home ["verifymsg", address, signature, message, "--network", "btctest"] []
          `shouldReturn` ResponseVerifyMsg (Text.pack message) (Text.pack signature) (Text.pack address) True
        (errorOf <$> hwJson home ["verifymsg", address, signature, message] [])
          `shouldReturn` Right "The address mpLQjfK79b7CCV4VMJWEWAj5Mpx8Up5zxB is not valid on btc"

    it "reads the block signmsg printed, saved in a file" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hw home ["signmsg", textOf (externalAddr 0), "hello"] seedOnce
        Text.lines (runOut signed)
          `shouldBe` [ "-----BEGIN BITCOIN SIGNED MESSAGE-----",
                       "hello",
                       "-----BEGIN BITCOIN SIGNATURE-----",
                       externalAddrsText !! 0,
                       helloSignature,
                       "-----END BITCOIN SIGNATURE-----"
                     ]
        let file = home </> "signed.txt"
        writeFile file (Text.unpack (runOut signed))
        hwJson home ["verifymsg", "--input", file] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "refuses a file that holds no block" $
      withHome $ \home -> do
        let file = home </> "plain.txt"
        writeFile file "hello\n"
        (errorOf <$> hwJson home ["verifymsg", "--input", file] [])
          `shouldReturn` Right (Text.pack $ "The file " <> file <> " could not be read: no \"-----BEGIN BITCOIN SIGNED MESSAGE-----\" line")

    it "reads a message of several lines, with its whitespace, from the block" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        let message = "two lines, \nand a trailing space "
        signed <- hw home ["signmsg", textOf (externalAddr 0), message] seedOnce
        let file = home </> "signed.txt"
        writeFile file (Text.unpack (runOut signed))
        checked <- hwJson home ["verifymsg", "--input", file] []
        case checked of
          ResponseVerifyMsg m _ a valid -> (m, a, valid) `shouldBe` (Text.pack message, externalAddrsText !! 0, True)
          other -> expectationFailure $ "Not a verifymsg answer: " <> show other

    it "reads the --json answer of signmsg, saved in a file" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        signed <- hw home ["signmsg", textOf (externalAddr 0), "hello", "--json"] seedOnce
        let file = home </> "signed.json"
        writeFile file (Text.unpack (runOut signed))
        hwJson home ["verifymsg", "--input", file] []
          `shouldReturn` ResponseVerifyMsg "hello" helloSignature (externalAddrsText !! 0) True

    it "refuses a file holding another answer of hw" $
      withHome $ \home -> do
        run <- hw home ["version", "--json"] []
        let file = home </> "version.json"
        writeFile file (Text.unpack (runOut run))
        (errorOf <$> hwJson home ["verifymsg", "--input", file] [])
          `shouldReturn` Right (Text.pack $ "The file " <> file <> " holds an answer of hw, but not one of signmsg")

    -- The refusals of Bitcoin Core's message_verify test
    it "refuses an address that is not P2PKH" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["verifymsg", "3B5fQsEXEaV8v6U3ejYc8XaKXAkyQj2MjV", "signature should be irrelevant", "message too"] [])
          `shouldReturn` Right "The address is not a P2PKH address: a message can only be signed from one"

    it "refuses a signature that is not in base64" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["verifymsg", "1KqbBpLy5FARmTPD4VZnDDpYjkUvkr82Pm", "invalid signature, not in base64 encoding", "message should be irrelevant"] [])
          `shouldReturn` Right "The signature is not in base64"

{- Failures -}

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

failureSpec :: Ctx -> Spec
failureSpec ctx = do
  describe "The help" $ do
    it "of a command is shown with --help, its summary first, and hw exits with success" $
      withHome $ \home -> do
        run <- hw home ["signtx", "--help"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldSatisfy` Text.isPrefixOf "Sign a pending transaction or a transaction file\n"
        runOut run `shouldSatisfy` Text.isInfixOf "Usage: hw signtx"
        runOut run `shouldSatisfy` Text.isInfixOf "-i, --input FILENAME"

    it "of a command that asks for a mnemonic says to run it on an offline computer" $
      withHome $ \home -> do
        run <- hw home ["signtx", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "This command asks for a mnemonic or private keys.\nRun it on an offline computer if you can.\n"

    it "of a command lists its options under their subheadings" $
      withHome $ \home -> do
        run <- hw home ["preparetx", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n\n  Change\n      --changeback "

    -- --help is the last of them, and Fee the first subheading of preparetx
    it "of a command lists the options without a subheading first" $
      withHome $ \home -> do
        run <- hw home ["preparetx", "--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "Show this help text\n\n  Fee\n  -f, --feerate RATE "

    -- The text of preparetx names them in this order
    it "of a command ends with the commands its text names" $
      withHome $ \home -> do
        run <- hw home ["preparetx", "--help"] []
        runOut run `shouldSatisfy` Text.isSuffixOf "See also: signtx, sendtx, pendingtxs, deletetx, bumpfee\n"

    it "of the program lists the commands by group with --help" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldSatisfy` Text.isInfixOf "Usage: hw COMMAND"
        runOut run `shouldSatisfy` Text.isInfixOf "Payments\n  fees "

    -- In a pipe, the help has no colours to show them
    it "marks the commands that ask for a mnemonic or private keys with ! when plain" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runOut run `shouldSatisfy` Text.isInfixOf "The commands marked ! ask for a mnemonic or private keys."
        runOut run `shouldSatisfy` Text.isInfixOf "! Generate a new mnemonic"
        runOut run `shouldSatisfy` (not . Text.isInfixOf "! Send a signed transaction")

    it "has no colour codes when written to a pipe" $
      withHome $ \home -> do
        run <- hw home ["--help"] []
        runOut run `shouldSatisfy` (not . Text.isInfixOf "\ESC")

    -- --unit and --json go before or after the command, and --help too
    it "of a command is shown with --help after its options" $
      withHome $ \home -> do
        run <- hw home ["--unit", "sat", "sync", "--json", "--help"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldSatisfy` Text.isInfixOf "Usage: hw sync"

    it "of a command is shown when it is typed alone and needs arguments, as a failure" $
      withHome $ \home -> do
        run <- hw home ["preparetx"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isPrefixOf "Prepare a payment, without signing it\n"
        runErr run `shouldSatisfy` Text.isInfixOf "Options:"

    it "of the program is shown when nothing is typed, as a failure" $
      withHome $ \home -> do
        run <- hw home [] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Mnemonics and accounts"

    it "is not shown for a --help after --, which is an argument" $
      withHome $ \home -> do
        run <- hw home ["label", "0", "--", "--help"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "There are no accounts in the wallet"

  describe "A command line hw cannot parse" $ do
    it "names the unknown command, and suggests the closest one" $
      withHome $ \home -> do
        run <- hw home ["preparetxx"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid argument `preparetxx'"
        runErr run `shouldSatisfy` Text.isInfixOf "Did you mean this?\n    preparetx"

    it "says what is wrong with the value of an option, then the usage of the command" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--split", "20"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Error: option --split: The --split value has to be between 1 and 12"
        runErr run `shouldSatisfy` Text.isInfixOf "Usage: hw mnemonic"

    it "ends with the help to read" $
      withHome $ \home -> do
        run <- hw home ["mnemonic", "--split", "20"] []
        runErr run `shouldSatisfy` Text.isSuffixOf "Type hw mnemonic --help for the help of mnemonic.\n"

    it "says what a command misses" $
      withHome $ \home -> do
        run <- hw home ["exporttx", Text.unpack (txHashToHex (fakeTxHash 1))] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Missing: FILENAME"
        runErr run `shouldSatisfy` Text.isInfixOf "Usage: hw exporttx HASH FILENAME"

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
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "1.5sat", "--feerate", "10"] [])
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
        let coin = Text.unpack (txHashToHex (fakeTxHash 1)) <> ":4294967296"
        forM_ [["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"], ["preparesweep", "--sweepto", textOf (otherAddr 0), "--feerate", "10"]] $ \command -> do
          run <- hw home (command <> ["--coin", coin]) []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf ("option --coin: The coin " <> Text.pack coin <> " is not valid: write it TXHASH:INDEX, as coins lists it")

    -- The hash and the index were one argument with a space, which needed
    -- quotes; a block explorer writes TXHASH:INDEX
    it "is refused a --coin written as two words" $
      withHome $ \home -> do
        let hash = Text.unpack (txHashToHex (fakeTxHash 1))
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", hash, "0", "--feerate", "10"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf ("option --coin: The coin " <> Text.pack hash <> " is not valid: write it TXHASH:INDEX, as coins lists it")

    it "is cancelled when the input ends before the answer, and leaves nothing" $
      withHome $ \home -> do
        (errorOf <$> hwJson home ["createacc", "main"] [mnemonicWords testMnemonic]) `shouldReturn` Right "Cancelled: the input ended before the answer"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

    -- A rate of 2^64 wrapped around to zero: preparetx made a transaction
    -- without fee, which signtx refused
    it "refuses a fee rate past 32 bits, rather than wrapping around" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "18446744073709551616"] [])
          `shouldReturn` Right "The fee rate must be at most 4294967295 satoshis per byte"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- preparetx takes a change address before it chooses the coins, which
    -- do not cover the 100000 satoshis here. The failure undoes it, with
    -- everything else the command wrote.
    it "leaves nothing of what it wrote before it failed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.001", "--feerate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"
        (addressesOf <$> hwJson home ["addrs", "--internal"] []) `shouldReturn` Right []
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- The store of the test home refuses every connection. hw crashed with
    -- the exception of the web client before, and wrote no JSON. The sync
    -- leaves the account out, with the failure of its store.
    it "tells when the store is out of reach, in its JSON too" $
      withHome $ \home -> do
        createMain home
        (notSyncedOf <$> hwJson home ["sync"] [])
          `shouldReturn` Right [("main", "The store at http://127.0.0.1:1/ could not be reached: the connection failed")]

    -- The web client decodes an answer only once hw uses it: hw crashed
    -- there before, with the error of the decoder
    it "tells when the store gives an answer it cannot read, as a server that is not a haskoin-store does" $
      withServerAnswering "not a store" $ \home url -> do
        createMain home
        notSynced <- notSyncedOf <$> hwJson home ["sync"] []
        (map fst <$> notSynced) `shouldBe` Right ["main"]
        (map snd <$> notSynced) `shouldSatisfy` either (const False) (all (Text.isPrefixOf ("The store at " <> Text.pack url <> " gave an answer hw could not read: ")))

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
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", file, "--feerate", "10"] [])
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
        _ <- answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] []
        run <- hw home ["signsweep", "--input", home </> "sweep.json", "--prvkeys", "-"] ["n", wikiKeyWif]
        run `shouldSatisfy` printed "The transaction was not signed: give an --output file to write it to"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- In a wallet without accounts: the file is all there is of the sweep
    it "sign a sweep outside the wallet in place of its file, which is sent from there" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        keys <- writeKeyFile home
        _ <- answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] []
        _ <- answer home ["signsweep", "--input", home </> "sweep.json", "--prvkeys", keys] ["y"]
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
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
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

    -- 50000 satoshis are 0.0005 bitcoins and 500 bits
    it "is shown by accounts in bitcoins, or in the unit of --unit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (printed "0.00050000 btc" <$> hw home ["accounts"] []) `shouldReturn` True
        (printed "50000 sats" <$> hw home ["accounts", "--unit", "sat"] []) `shouldReturn` True
        (printed "500.00 bits" <$> hw home ["accounts", "--unit", "bit"] []) `shouldReturn` True

    -- The help says the two options go before or after the command
    it "is shown in the unit of --unit given before the command" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (printed "50000 sats" <$> hw home ["--unit", "sat", "accounts"] []) `shouldReturn` True

    it "is answered in JSON with --json given before the command" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["--json", "accounts"] []
        (accountsOf <$> Json.eitherDecodeStrict (encodeUtf8 (runOut run))) `shouldBe` Right (Right [("main", 0, 50000)])

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
        coinArgs = [Text.unpack (txHashToHex (txHash twoCoins)) <> ":0"]
        locks = fmap (fmap (\(outpoint, _, _, lock, _) -> (outpoint, lock)))
    it "is locked by lockcoin and freed by unlockcoin, which refuses it then" $
      withChain ctx chain $ \home _ -> do
        synced home
        (locks . coinsShown <$> hwJson home ("lockcoin" : coinArgs) []) `shouldReturn` Right [(firstCoin, Just LockedByUser)]
        (locks . coinsShown <$> hwJson home ("unlockcoin" : coinArgs) []) `shouldReturn` Right [(firstCoin, Nothing)]
        (errorOf <$> hwJson home ("unlockcoin" : coinArgs) []) `shouldReturn` Right "The coin is not locked"

    -- Copied from the listing, as the user would
    it "is written by coins as lockcoin takes it, TXHASH:INDEX" $
      withChain ctx chain $ \home _ -> do
        synced home
        listed <- hw home ["coins"] []
        let written = Text.unpack (txHashToHex (txHash twoCoins)) <> ":0"
        runOut listed `shouldSatisfy` Text.isInfixOf (Text.pack written)
        (locks . coinsShown <$> hwJson home ["lockcoin", written] []) `shouldReturn` Right [(firstCoin, Just LockedByUser)]

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
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--feerate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

  describe "A sync" $ do
    -- The store answers every question about Bitcoin Cash with the error
    -- page of a proxy, as when the store behind the proxy is down. hw
    -- failed the whole sync before, with the error StringError "could not
    -- decode error". The chain pays main 50000.
    it "syncs the accounts whose store is up, and leaves out those whose store is down, with the failure of their store" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        answered <- hwJson home ["sync"] []
        accountsOf answered `shouldBe` Right [("main", 0, 50000)]
        (map fst <$> notSyncedOf answered) `shouldBe` Right ["cash"]
        (map snd <$> notSyncedOf answered)
          `shouldSatisfy` either (const False) (all (\failure -> "The store at http://127.0.0.1:" `Text.isPrefixOf` failure && "/ answered with an error hw could not read" `Text.isSuffixOf` failure))

    it "exits with 1 when it left out an account, and prints why" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        run <- hw home ["sync"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "Account   : cash"
        run `shouldSatisfy` printed "Not synced: The store at http://127.0.0.1:"

    -- The chain pays main: a sync of second alone leaves main as it was
    it "updates only the account of --account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        _ <- answer home ["createacc", "second"] seedTwice
        (accountsOf <$> hwJson home ["sync", "--account", "second"] []) `shouldReturn` Right [("second", 1, 0)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0), ("second", 1, 0)]

    -- The database lost the transaction the coin of the payment comes
    -- from, which signing needs. No balance changed, so only a full sync
    -- looks at the coin again.
    it "fetches with --full the raw transaction of a coin the database lost, as preparetx asks" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        dropRawTxs home
        let missing = "The raw transaction " <> txHashToHex (txHash (payment ctx)) <> " of a coin is missing: sync --full fetches it"
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []) `shouldReturn` Right missing
        _ <- answer home ["sync", "--full"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

-- | Drop every raw transaction from the database of a home, as a database
-- that lost them
dropRawTxs :: FilePath -> IO ()
dropRawTxs home = runDBWith 100 (home </> ".hw" </> "accounts.sqlite") $ P.deleteWhere ([] :: [P.Filter DBRawTx])

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
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
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
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
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
        runOut run `shouldSatisfy` Text.isInfixOf ("Deleted: " <> Text.pack txid)

    it "is signed from the parts of the mnemonic with signtx --split" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        signed <- txOf =<< answer home ["signtx", nosig, "--split", "2"] partsOnce
        pendingState signed `shouldBe` Just (True, False)

    -- The wallet has two accounts of the test seed, and the payment is
    -- pending in main. Without --account, hw asked which account to use.
    it "is signed by the account that holds it, without --account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "main", "--feerate", "10"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)

    -- The mnemonic of second would be typed for nothing: its key cannot
    -- sign what main holds
    it "is refused an --account that does not hold it, before the mnemonic is asked" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "main", "--feerate", "10"] []
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
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        (fmap dBAccountFingerprint . accountOf <$> hwJson home ["signtx", nosig] seedOnce)
          `shouldReturn` Right (Just "bc560f95")

  -- The signer holds the account and asks for its mnemonic; the online
  -- computer holds the same account, imported, and talks to the store
  describe "A payment signed offline" $ do
    it "is prepared online, signed offline, sent online, and confirmed by a sync" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        prepared <- txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--feerate", "10"] []
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
        (addressesOf <$> hwJson online ["addrs", "--internal"] []) `shouldReturn` Right [(internalAddrsText !! 0, "")]

    -- Prepared without a file, the transaction is written out by exporttx
    it "is exported by exporttx, signed offline, imported back by importtx, and sent by its nosigHash" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online chain -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
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
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "main", "--output", online </> "unsigned.json", "--feerate", "10"] []
        (fmap dBAccountName . accountOf <$> hwJson signer ["reviewtx", online </> "unsigned.json"] []) `shouldReturn` Right "main"
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        pendingState signed `shouldBe` Just (True, False)
        (fmap dBAccountName . accountOf <$> hwJson online ["importtx", signer </> "signed.json"] []) `shouldReturn` Right "main"

    -- The signer has no coins: the file carries the transactions the
    -- payment spends, and the paths of the change
    it "is shown by reviewtx from its file, as the signer reads it before signing, and not kept" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--feerate", "10"] []
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
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--feerate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["sendtx", "--input", signer </> "signed.json", "--yes"] []
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
        forM_ [("0.0002", []), ("20000", ["--unit", "sat"]), ("0.0002btc", []), ("200bits", []), ("0.0002btc", ["--unit", "sat"])] $ \(amount, unit) -> do
          prepared <- txOf =<< answer home (["preparetx", fixedTo (otherAddr 0) amount, "--feerate", "10"] <> unit) []
          paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000)]
          nosig <- nosigOf prepared
          void $ answer home ["deletetx", nosig] []

    it "pays each recipient the amount after its address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", fixedTo (otherAddr 1) "0.0001", "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)]

    -- As the recipients were typed before, the amount in a word of its own
    it "is refused a recipient without an amount, with how to write one" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", textOf (otherAddr 0), "0.0002", "--feerate", "10"] [])
          `shouldReturn` Right "The recipient 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU has no amount: write it ADDRESS:AMOUNT, as in 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.001"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- 226 bytes at 20 satoshis a byte
    it "pays the fee rate of --fee for each byte" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "20"] []
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary, summarySize summary) `shouldBe` (4520, 20, 226)

    -- The fee of 2260 comes out of the 20000 the recipient gets
    it "takes the fee from the recipient with --recipientpay" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--recipientpay", "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 17740)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 30000)]

    -- 999 satoshis are less than the 1000 of --dust, and more than the 546
    -- of the default
    it "is refused a payment below the --dust limit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "999sat", "--dust", "1000", "--feerate", "10"] [])
          `shouldReturn` Right "Recipient output is smaller than the dust value"

    -- A payment of 30000 from the coin of 50000 leaves 50000 - 30000 - 2260
    -- = 17740 of change, less than the 20000 of --dust: the fee is all the
    -- rest, 50000 - 30000 = 20000
    it "gives the fee a change below the --dust limit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0003", "--dust", "20000", "--feerate", "10"] []
        paidToWallet prepared `shouldBe` Map.empty
        feeOf prepared `shouldBe` 20000

    -- The coins of 50000 and 30000 are both at /0/0, and either pays 20000
    -- alone. Each payment is deleted, which frees its coin for the next.
    it "spends the coins in the order of --strategy" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        forM_ [("largest-first", 50000), ("smallest-first", 30000)] $ \(strategy, spent) -> do
          prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--strategy", strategy, "--feerate", "10"] []
          spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, spent)]
          nosig <- nosigOf prepared
          void $ answer home ["deletetx", nosig] []

    it "is refused a --strategy it does not know, with the ones it knows" $
      withHome $ \home -> do
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--strategy", "biggest", "--feerate", "10"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --strategy: Invalid strategy. Select one of the following: least-waste, random-draw, largest-first, smallest-first"

    it "is refused for more than the coins of the account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.001", "--feerate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

    -- The coin of the payment is of main, and the wallet has a second
    -- account. Without --account, hw asked which account to use.
    it "is from the account of the coins of --coin, without --account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        let coin = Text.unpack (txHashToHex (txHash (payment ctx))) <> ":0"
        (fmap dBAccountName . accountOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coin, "--feerate", "10"] [])
          `shouldReturn` Right "main"

    it "is refused an --account that does not hold the coins of --coin" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        let coin = Text.unpack (txHashToHex (txHash (payment ctx))) <> ":0"
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coin, "--account", "second", "--feerate", "10"] [])
          `shouldReturn` Right "The coins or addresses given are not in the account second"

    it "is written to its --output file with the network of its account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "unsigned.json", "--feerate", "10"] []
        (fmap txFileNetwork <$> Json.decodeFileStrict (home </> "unsigned.json")) `shouldReturn` Just (Just btc)

    -- The path is checked first: the coins would not cover these 100000
    -- satoshis either
    it "is refused an --output file that exists, before it is built" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        writeFile (home </> "taken.json") ""
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.001", "--output", home </> "taken.json", "--feerate", "10"] [])
          `shouldReturn` Right (alreadyExists (home </> "taken.json"))
        readFile (home </> "taken.json") `shouldReturn` ""

{- The confirmations of the coins spent -}

-- | A payment of 30000 satoshis from another wallet to the second
-- receiving address of the test wallet
newerPayment :: Ctx -> Tx
newerPayment ctx = fundingTx ctx 10 [(externalAddr 1, 30000)]

-- | The paid chain with the newer payment in its best block, 800002: the
-- coin of 50000 has 3 confirmations, and the coin of 30000 has 1
olderAndNewerChain :: Ctx -> Chain
olderAndNewerChain ctx = Chain 800002 [(payment ctx, Just 800000), (newerPayment ctx, Just 800002)]

-- A payment of 20000 from the coin of 50000 makes one input and two
-- outputs, 226 bytes: a fee of 2260 at 10 satoshis a byte, and 27740 of
-- change. The coin of the paid chain is in block 800000, two below the best
-- block: 3 confirmations.
minConfSpec :: Ctx -> Spec
minConfSpec ctx =
  describe "The confirmations preparetx asks of its coins with --minconf" $ do
    it "spends a coin with as many confirmations as --minconf asks" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--minconf", "3", "--feerate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    it "is refused when no coin has as many confirmations as --minconf asks, and prepares nothing" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--minconf", "4", "--feerate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    -- Smallest first would spend the coin of 30000 alone, but it has 1
    -- confirmation
    it "chooses only among the coins with as many confirmations as --minconf asks" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--minconf", "2", "--strategy", "smallest-first", "--feerate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    -- The payment sent spends the coin of 50000 and leaves 27740 of change
    -- in the mempool, the only coin of the account
    it "spends the unconfirmed change of the wallet's own payment without --minconf" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- sentPayment home
        _ <- answer home ["sync"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--feerate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]

    it "does not spend the unconfirmed change of the wallet's own payment with --minconf 1" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- sentPayment home
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--minconf", "1", "--feerate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee"

    it "lists a coin of --coin with fewer confirmations than --minconf asks as not spendable yet" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--coin", coinArg (newerPayment ctx) 0, "--minconf", "2", "--feerate", "10"] []
        spentFromWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 50000)]
        leftOf prepared `shouldBe` Right [(OutPoint (txHash (newerPayment ctx)) 0, 30000, NotSpendableYet)]

    it "is refused a --minconf that is not a whole number" $
      withHome $ \home -> do
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--minconf", "1.5", "--feerate", "10"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --minconf: cannot parse value `1.5'"

{- Recovering a seed -}

recoverSpec :: Ctx -> Spec
recoverSpec ctx =
  describe "The accounts of a seed recovered by recover" $ do
    it "are those paid, named after their network and index, and synced" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (accountsOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right [("btc0", 0, 50000)]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", 0, 50000)]

    it "are found from the parts of the mnemonic with --split" $
      withChain ctx (paidChain ctx) $ \home _ ->
        (accountsOf <$> hwJson home ["recover", "--split", "2"] partsOnce) `shouldReturn` Right [("btc0", 0, 50000)]

    -- The chain pays account 1 only. Account 0 is one without a payment.
    it "are looked for until --account-gap accounts in a row without a payment" $
      withChain ctx (chainPaying ctx account1Addr) $ \home _ -> do
        (accountsOf <$> hwJson home ["recover", "--account-gap", "1"] seedOnce) `shouldReturn` Right []
        (accountsOf <$> hwJson home ["recover", "--account-gap", "2"] seedOnce) `shouldReturn` Right [("btc1", 1, 50000)]

    -- The configuration file of the test home names btc. The chain pays the
    -- first receiving address of account 0 of the seed on Bitcoin Cash.
    it "are looked for on the --network given" $
      withChain ctx (chainPaying ctx bchExternalAddr) $ \home _ ->
        (accountsOf <$> hwJson home ["recover", "--network", "bch"] seedOnce) `shouldReturn` Right [("bch0", 0, 50000)]

    it "leave out an account the wallet has" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        createMain home
        (accountsOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right []
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", 0, 0)]

    it "are not looked for without a --network or a default network in the configuration file, before the mnemonic" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        setDefaultNetwork home Nothing
        run <- hw home ["recover"] seedOnce
        run `shouldSatisfy` printed "Give the --network, or a default-network in the configuration file"
        run `shouldNotSatisfy` askedMnemonic

    -- The chain pays the account hw derived before it normalized the
    -- passphrase, where BIP39 gives another seed
    it "are looked for from the passphrase as typed when the normalized one has none, which recover says" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \home _ -> do
        run <- hw home ["recover"] cafeOnce
        runErr run `shouldSatisfy` Text.isInfixOf "The accounts found come from the passphrase as typed"
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", 0, 50000)]
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", Just cafeTypedFingerprint)]

    it "are those of the normalized passphrase, as BIP39 derives them, without a word about the other" $
      withChain ctx (chainPaying ctx cafeNormalizedAddr) $ \home _ -> do
        run <- hw home ["recover"] cafeOnce
        runErr run `shouldNotSatisfy` Text.isInfixOf "as typed"
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", Just cafeNormalizedFingerprint)]

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

{- The passphrase as typed -}

-- | The words of the test wallet with the passphrase "café", typed once, or
-- twice each as createacc asks
cafeOnce, cafeTwice :: [Text]
cafeOnce = [mnemonicWords testMnemonic, cafePassphrase]
cafeTwice = [mnemonicWords testMnemonic, mnemonicWords testMnemonic, cafePassphrase, cafePassphrase]

-- | A chain whose block 800000 pays 50000 to the address
chainPaying :: Ctx -> Address -> Chain
chainPaying ctx addr = Chain 800002 [(fundingTx ctx 1 [(addr, 50000)], Just 800000)]

passphraseSpec :: Ctx -> Spec
passphraseSpec ctx =
  describe "An account created from a passphrase with accents" $ do
    it "is of the seed BIP39 derives, when the wallet has no account of the passphrase as typed" $
      withHome $ \home -> do
        run <- hw home ["createacc", "main"] cafeTwice
        runErr run `shouldNotSatisfy` Text.isInfixOf "as typed"
        (fingerprintsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("main", Just cafeNormalizedFingerprint)]

    -- btc0 was recovered from the passphrase as typed
    it "is of the seed of the passphrase as typed when the wallet has an account of it and none of the other, which createacc says" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \home _ -> do
        _ <- answer home ["recover"] cafeOnce
        run <- hw home ["createacc", "second"] cafeTwice
        runErr run `shouldSatisfy` Text.isInfixOf "to keep them together"
        (fmap sort . accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right [("btc0", 0, 50000), ("second", 1, 0)]
        (fmap sort . fingerprintsOf <$> hwJson home ["accounts"] [])
          `shouldReturn` Right [("btc0", Just cafeTypedFingerprint), ("second", Just cafeTypedFingerprint)]

    -- main is of the seed of BIP39, btc0 of the passphrase as typed
    it "is of the seed BIP39 derives when the wallet has accounts of both seeds" $
      withChain ctx (chainPaying ctx cafeTypedAddr) $ \home _ -> do
        _ <- answer home ["createacc", "main"] cafeTwice
        _ <- answer home ["recover"] cafeOnce
        run <- hw home ["createacc", "third"] cafeTwice
        runErr run `shouldNotSatisfy` Text.isInfixOf "as typed"
        (fmap sort . fingerprintsOf <$> hwJson home ["accounts"] [])
          `shouldReturn` Right [("btc0", Just cafeTypedFingerprint), ("main", Just cafeNormalizedFingerprint), ("third", Just cafeNormalizedFingerprint)]

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

-- | A coin of 20000 satoshis at the first address of the second wallet,
-- another old wallet
otherOldCoin :: Ctx -> Tx
otherOldCoin ctx = fundingTx ctx 11 [(externalAddr2, 20000)]

-- | A chain whose block 800000 holds the coins of both old wallets
twoOldWalletsChain :: Ctx -> Chain
twoOldWalletsChain ctx = Chain 800002 [(oldCoin ctx, Just 800000), (otherOldCoin ctx, Just 800000)]

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
        prepared <- txOf =<< answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        pendingState prepared `shouldBe` Just (False, False)
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]
        nosig <- nosigOf prepared
        signed <- txOf =<< answer home ["signsweep", nosig, "--prvkeys", keys] []
        pendingState signed `shouldBe` Just (True, False)
        _ <- answer home ["sendtx", nosig, "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (externalAddr 0))])]
        modifyIORef chain mine
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 28080)]

    -- Without the key, hw counts it as compressed, which it is: the sweep
    -- is the same 192 bytes
    it "takes the coin of --coin, without its key or its address" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home ["preparesweep", "--coin", coinArg (oldCoin ctx) 0, "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]

    it "takes the coins of the addresses of the --addressfile file" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        writeFile (home </> "addresses.txt") (textOf wikiAddr <> " old wallet\n")
        prepared <- txOf =<< answer home ["preparesweep", "--addressfile", home </> "addresses.txt", "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]

    -- Two inputs and one output make 340 bytes, a fee of 3400: 30000 +
    -- 20000 - 3400 = 46600
    it "takes the coins of the --addressfile addresses along with those of --sweepfrom" $
      withChain ctx (twoOldWalletsChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        writeFile (home </> "addresses.txt") (Text.unpack externalAddr2Text <> "\n")
        prepared <- txOf =<< answer home ["preparesweep", "--sweepfrom", textOf wikiAddr, "--addressfile", home </> "addresses.txt", "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 46600)]

    -- The keys would be typed for nothing
    it "is refused an --addressfile file that does not exist, before the keys are typed" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["preparesweep", "--prvkeys", "-", "--addressfile", home </> "addresses.txt", "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] []
        run `shouldSatisfy` printed (noSuchFile (home </> "addresses.txt"))
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- The last letter of the address on the third line is wrong
    it "is refused a line of the --addressfile file with words but no address, which is named" $
      withHome $ \home -> do
        createMain home
        writeFile (home </> "addresses.txt") ("# The old wallet\n" <> textOf wikiAddr <> "\n1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmX old wallet\n")
        (errorOf <$> hwJson home ["preparesweep", "--addressfile", home </> "addresses.txt", "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] [])
          `shouldReturn` Right "No address on line 3: 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmX old wallet"

    it "is refused when what remains for the destination is below the --dust limit" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--dust", "30000", "--feerate", "10"] [])
          `shouldReturn` Right "What remains for each address without an amount, 28080 satoshis, is below the dust limit of 30000"

    it "that pays outside the wallet is refused without an --output file" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--network", "btc", "--feerate", "10"] [])
          `shouldReturn` Right "The sweep does not involve this wallet: give an --output file to write it to"

    -- The wallet keeps no trace of it: the sweep is signed from its file,
    -- and sent with the network sendtx cannot read from the wallet
    it "that pays outside the wallet is written to its file only, signed from it, and sent on the network it names" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        createMain home
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 28080)]
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
        signed <- txOf =<< answer home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.json", "--prvkeys", keys] []
        pendingState signed `shouldBe` Just (True, False)
        sent <- answer home ["sendtx", "--input", home </> "signed.json", "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (otherAddr 0))])]
        (ResponseSentTx btc . txHash <$> published) `shouldBe` [sent]

    -- The keys would be typed for nothing
    it "is refused an --output file that exists, before the keys are typed" $
      withHome $ \home -> do
        createMain home
        writeFile (home </> "sweep.json") ""
        run <- hw home ["preparesweep", "--prvkeys", "-", "--sweepto", textOf (otherAddr 0), "--output", home </> "sweep.json", "--feerate", "10"] []
        run `shouldSatisfy` printed (alreadyExists (home </> "sweep.json"))
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- The keys would be typed for nothing
    it "is refused an address that is not one, before the keys are typed" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["preparesweep", "--prvkeys", "-", "--sweepto", "nonsense", "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] []
        run `shouldSatisfy` printed "The address nonsense is not valid on btc"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- One input and two outputs make 226 bytes, a fee of 2260: 30000 - 10000
    -- - 2260 = 17740 for the address without an amount
    it "pays a --sweepto address its fixed amount, and what remains to the address without one" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--prvkeys", keys, "--sweepto", fixedTo (otherAddr 0) "0.0001", "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 10000)]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 17740)]

    it "is of the account of a --sweepto address that has an amount" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- answer home ["preparesweep", "--prvkeys", keys, "--sweepto", fixedTo (externalAddr 0) "0.0001", "--sweepto", textOf (otherAddr 0), "--feerate", "10"] []
        (dBAccountName <$> accountOf prepared) `shouldBe` Right "main"
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 10000)]

    -- The keys would be typed for nothing
    it "is refused --sweepto addresses that all have an amount, before the keys are typed" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["preparesweep", "--prvkeys", "-", "--sweepto", fixedTo (otherAddr 0) "0.0001", "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] []
        run `shouldSatisfy` printed "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- A rate of 2^64 wrapped around to zero, and others to any fee
    it "is refused a fee rate past 32 bits, rather than wrapping around" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "18446744073709551616"] [])
          `shouldReturn` Right "The fee rate must be at most 4294967295 satoshis per byte"

    -- The wallet has two accounts of the test seed, and the sweep is
    -- pending in main
    it "is signed by the account that holds it, without --account" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "main"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--account", "main", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        signed <- txOf =<< answer home ["signsweep", nosig, "--prvkeys", keys] []
        pendingState signed `shouldBe` Just (True, False)

    -- The wallet has a second account, and the destination is of main.
    -- Without --account, hw asked which account to use.
    it "is of the account it pays, without --account" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "main"] []
        keys <- writeKeyFile home
        (fmap dBAccountName . accountOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "10"] [])
          `shouldReturn` Right "main"

    it "is refused an --account it pays no address of" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "main"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--account", "second", "--feerate", "10"] [])
          `shouldReturn` Right "The sweep pays no address of the account second"

    -- With no account at all, hw sweeps as a utility, through files: on the
    -- --network given, then on the network the file names
    it "that pays outside the wallet is prepared, signed and sent from files in a wallet without accounts" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        keys <- writeKeyFile home
        prepared <- answer home (outsideSweep home keys) []
        prepared `shouldSatisfy` \case ResponseOutsideTx net _ -> net == btc; _ -> False
        signed <- txOf =<< answer home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.json", "--prvkeys", keys] []
        pendingState signed `shouldBe` Just (True, False)
        _ <- answer home ["sendtx", "--input", home </> "signed.json", "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (otherAddr 0))])]
        (accountsOf <$> hwJson home ["accounts"] []) `shouldReturn` Right []

    -- The configuration file of the test home names btc, and the key has a
    -- coin on Bitcoin. Keys, coins and the legacy addresses of Bitcoin Cash
    -- read the same on Bitcoin: the default network could sweep the coins
    -- of another chain than the one meant.
    it "that pays outside the wallet needs the --network it is for, whatever the default network" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--output", home </> "sweep.json", "--feerate", "10"] [])
          `shouldReturn` Right "Give the --network of a sweep that pays no account of the wallet: the default-network of the configuration file does not apply to it"

    -- The default network is not a --network given: main is on btc
    it "that pays the wallet is on the network of its account, whatever the default network" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        setDefaultNetwork home (Just "bch")
        keys <- writeKeyFile home
        (fmap dBAccountName . accountOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "10"] [])
          `shouldReturn` Right "main"

    -- cash is the Bitcoin Cash account of the test wallet. hw stores its
    -- first receiving address as
    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u. The store has
    -- the coin of the key on Bitcoin Cash too.
    it "that pays the wallet finds its account by a Bitcoin Cash address written without its prefix" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        _ <- receiveFor home "cash"
        keys <- writeKeyFile home
        (fmap dBAccountName . accountOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", "qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", "--feerate", "10"] [])
          `shouldReturn` Right "cash"

    -- 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the same address of cash in the
    -- legacy form, which Bitcoin reads too. The sweep is of cash all the
    -- same, rather than outside the wallet on the default network btc.
    it "that pays the wallet finds its account by a Bitcoin Cash address in the legacy form" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        _ <- receiveFor home "cash"
        keys <- writeKeyFile home
        (fmap dBAccountName . accountOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--feerate", "10"] [])
          `shouldReturn` Right "cash"

    it "that pays outside the wallet is refused an --account" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        keys <- writeKeyFile home
        (errorOf <$> hwJson home (outsideSweep home keys <> ["--account", "main"]) [])
          `shouldReturn` Right "The sweep pays no address of the account main"

    it "that pays outside the wallet is reviewed from its file, outside the wallet, on its network" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        reviewed <- answer home ["reviewtx", home </> "sweep.json"] []
        reviewed `shouldSatisfy` \case ResponseOutsideTx net _ -> net == btc; _ -> False
        (paidToOthers <$> txOf reviewed) `shouldReturn` Map.fromList [(otherAddr 0, 28080)]

    it "that pays outside the wallet is not imported by importtx" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        (errorOf <$> hwJson home ["importtx", home </> "sweep.json"] [])
          `shouldReturn` Right "The transaction does not involve this wallet"

    -- No key of the wallet signs it: the mnemonic would be typed for nothing
    it "that pays outside the wallet is refused by signtx, before the mnemonic is asked" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        run <- hw home ["signtx", "--input", home </> "sweep.json", "--output", home </> "signed.json"] seedOnce
        run `shouldSatisfy` printed "The transaction does not involve this wallet"
        run `shouldNotSatisfy` askedMnemonic

    it "that pays outside the wallet is refused an --account to sign it" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        (errorOf <$> hwJson home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.json", "--prvkeys", keys, "--account", "main"] [])
          `shouldReturn` Right "The transaction does not involve the account main"

    -- The file of an earlier hw names no network
    it "that pays outside the wallet is not signed from the file of an earlier hw, which names no network" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        written <- readTxFile (home </> "sweep.json")
        Json.encodeFile (home </> "old.json") (txFileData written)
        (errorOf <$> hwJson home ["signsweep", "--input", home </> "old.json", "--output", home </> "signed.json", "--prvkeys", keys] [])
          `shouldReturn` Right "The file names no network, as those of an earlier hw: prepare the transaction again"

    -- main and second, two accounts of the test wallet, each with its first
    -- receiving address
    it "that pays two accounts asks which one, and is of the one --account names" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        mainAddr <- receiveFor home "main"
        secondAddr <- receiveFor home "second"
        keys <- writeKeyFile home
        let sweep = ["preparesweep", "--prvkeys", keys, "--sweepto", mainAddr, "--sweepto", secondAddr, "--feerate", "10"]
        (errorOf <$> hwJson home sweep []) `shouldReturn` Right "Specify which account to use"
        (fmap dBAccountName . accountOf <$> hwJson home (sweep <> ["--account", "second"]) []) `shouldReturn` Right "second"

    -- main is on Bitcoin, and cash on Bitcoin Cash
    it "that pays accounts of several networks is refused" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        mainAddr <- receiveFor home "main"
        cashAddr <- receiveFor home "cash"
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", mainAddr, "--sweepto", cashAddr, "--feerate", "10"] [])
          `shouldReturn` Right "The sweep pays accounts of several networks"

    it "that pays the wallet is refused a --network other than the one of its account" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--network", "bch", "--feerate", "10"] [])
          `shouldReturn` Right "The sweep pays an account on btc, not on the --network bch"
  where
    -- The sweep of the old key to another wallet, written to sweep.json
    outsideSweep home keys =
      ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"]

{- Listings -}

-- | How many rows a listing shows
rowsOf :: Response -> Either String Int
rowsOf (ResponseAddresses _ addrs) = Right (length addrs)
rowsOf (ResponseBalances _ addrs) = Right (length addrs)
rowsOf (ResponseTxs _ txs) = Right (length txs)
rowsOf (ResponsePendingTxs _ held) = Right (length held)
rowsOf (ResponseCoins _ coins) = Right (length coins)
rowsOf other = Left (show other)

-- | What names each row of a listing, as --after takes it: the address,
-- the coin, the txid, or the hash of a pending transaction as shown
keysOf :: Response -> Either String [String]
keysOf (ResponseAddresses _ addrs) = Right (Text.unpack . dBAddressAddress . fst <$> addrs)
keysOf (ResponseBalances _ addrs) = Right (Text.unpack . dBAddressAddress . fst <$> addrs)
keysOf (ResponseTxs _ infos) = Right [Text.unpack (txHashToHex (chainTxid status)) | info <- infos, OnChain status <- [txInfoStatus info]]
keysOf (ResponsePendingTxs _ held) = Right [Text.unpack (txHashToHex (pendingShownHash status)) | (_, info) <- held, Pending status <- [txInfoStatus info]]
keysOf (ResponseCoins _ coins) = Right [Text.unpack (txHashToHex outpoint.hash) <> ":" <> show outpoint.index | coin <- coins, let outpoint = jsonCoinOutpoint coin]
keysOf other = Left (show other)

-- | The listings, each with one row in a synced home with a payment
-- prepared: the address paid, the payment received, its coin, the payment
-- prepared, and the address that holds the coin
listings :: [String]
listings = ["addrs", "txs", "coins", "pendingtxs", "balances"]

-- | The time of each transaction an answer lists
timesOf :: Response -> Either String [Maybe UTCTime]
timesOf (ResponseTxs _ infos) = Right (chainTime . chainOf <$> infos)
timesOf other = Left (show other)

listingSpec :: Ctx -> Spec
listingSpec ctx = do
  -- The fake store gives every transaction the time 1700000000
  describe "The history txs lists" $
    it "gives each transaction the time the store gives it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (timesOf <$> hwJson home ["txs"] []) `shouldReturn` Right [Just seenTime]

  describe "A listing" $ do
    it "shows every row without --max" $
      withHome $ \home -> do
        createMain home
        replicateM_ 6 (answer home ["receive"] [])
        (rowsOf <$> hwJson home ["addrs"] []) `shouldReturn` Right 6

    it "shows the row of each listing" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        forM_ listings $ \listing ->
          (rowsOf <$> hwJson home [listing] []) `shouldReturn` Right 1

    -- 2^64 + 1 wrapped around to 1, and showed one address of the three
    it "shows every row with a --max past 64 bits, rather than wrapping around" $
      withHome $ \home -> do
        createMain home
        replicateM_ 3 $ answer home ["receive"] []
        (rowsOf <$> hwJson home ["addrs", "--max", "18446744073709551617"] []) `shouldReturn` Right 3

    it "refuses a --max of 0" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["addrs", "--max", "0"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "option --max: cannot parse value `0'"

    it "refuses to start after a row that is not in it" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["addrs", "--after", Text.unpack (internalAddrsText !! 0)] [])
          `shouldReturn` Right ("The address " <> internalAddrsText !! 0 <> " is not a receiving address of the account")

    -- second is an account of its own, without any row
    it "shows the rows of the account of --account only" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        _ <- answer home ["createacc", "second"] seedTwice
        forM_ listings $ \listing -> do
          (rowsOf <$> hwJson home [listing, "--account", "main"] []) `shouldReturn` Right 1
          (rowsOf <$> hwJson home [listing, "--account", "second"] []) `shouldReturn` Right 0

    -- Two of each: the addresses /0/0 and /0/1, the two payments to them,
    -- their coins, and a payment prepared from each coin
    it "shows --max rows of each listing, then the rows after the one --after names" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--feerate", "10"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg (newerPayment ctx) 0, "--feerate", "10"] []
        forM_ listings $ \listing -> do
          keys <- either fail return . keysOf =<< hwJson home [listing] []
          length keys `shouldBe` 2
          (keysOf <$> hwJson home [listing, "--max", "1"] []) `shouldReturn` Right [keys !! 0]
          (keysOf <$> hwJson home [listing, "--after", keys !! 0] []) `shouldReturn` Right [keys !! 1]
          (rowsOf <$> hwJson home [listing, "--after", keys !! 1] []) `shouldReturn` Right 0

    -- The two payments of the test before, signed. The newer is listed
    -- first.
    it "shows the pending transactions after the signed one --after names by its txid" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        older <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (payment ctx) 0, "--feerate", "10"] []
        newer <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", "--coin", coinArg (newerPayment ctx) 0, "--feerate", "10"] []
        olderId <- txidOf =<< txOf =<< answer home ["signtx", older] seedOnce
        newerId <- txidOf =<< txOf =<< answer home ["signtx", newer] seedOnce
        (keysOf <$> hwJson home ["pendingtxs", "--after", newerId] []) `shouldReturn` Right [olderId]

{- Sending -}

-- | A home synced with the paid chain, with the payment of 20000 to the
-- other wallet prepared and signed: its nosigHash
paymentSigned :: FilePath -> IO String
paymentSigned home = do
  synced home
  nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
  _ <- answer home ["signtx", nosig] seedOnce
  return nosig

-- | The one pending transaction of a home
onePendingTx :: FilePath -> IO TxInfo
onePendingTx home = do
  listed <- hwJson home ["pendingtxs"] []
  case listed of
    ResponsePendingTxs _ [(_, info)] -> return info
    other -> fail $ "Not one pending transaction: " <> show other

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
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--feerate", "10"] []
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

{- A high fee rate -}

-- | The question of preparetx and preparesweep at 101 satoshis per byte
highFeeQuestion :: Text
highFeeQuestion = "The fee rate of 101 satoshis per byte is above 100, the most hw takes without asking. Prepare the transaction anyway? [y/N] "

-- | The refusal of a high fee rate the user does not take
highFeeRefused :: Text
highFeeRefused = "The transaction was not prepared: give a lower --feerate"

-- The payment of 20000 satoshis is 226 bytes: its fee is 22600 at 100
-- satoshis a byte, and 22826 at 101. The sweep of the coin of 30000 of the
-- old key is 192 bytes: its fee is 19392 at 101, which leaves 10608 for the
-- destination. Where nothing is typed, a question would cancel the command.
highFeeSpec :: Ctx -> Spec
highFeeSpec ctx =
  describe "A fee rate above 100 satoshis per byte" $ do
    it "is not asked about at 100, the highest rate taken without asking" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "100"] []
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary) `shouldBe` (22600, 100)

    it "is asked about by preparetx on stderr" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "101"] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf highFeeQuestion

    it "is taken by preparetx when the user answers yes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "101"] ["y"]
        let summary = txInfoSummary prepared
        (summaryFee summary, summaryFeeByte summary) `shouldBe` (22826, 101)

    it "makes preparetx prepare nothing when the user answers anything else" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "101"] ["n"])
          `shouldReturn` Right highFeeRefused
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "is taken by preparetx without asking with --yes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "101", "--yes"] []
        summaryFeeByte (txInfoSummary prepared) `shouldBe` 101

    -- The keys would be typed for nothing
    it "is asked about by preparesweep before the private keys are typed" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["preparesweep", "--prvkeys", "-", "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "101"] ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf highFeeQuestion
        run `shouldSatisfy` printed highFeeRefused
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"
        doesFileExist (home </> "sweep.json") `shouldReturn` False

    it "is taken by preparesweep when the user answers yes" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "101"] ["y"]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 10608)]

    it "is taken by preparesweep without asking with --yes" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "101", "--yes"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 10608)]

{- Sending all the coins -}

-- | Coins of 50000 and 30000 satoshis at the first receiving address of the
-- test wallet
twoCoinsTx :: Ctx -> Tx
twoCoinsTx ctx = fundingTx ctx 5 [(externalAddr 0, 50000), (externalAddr 0, 30000)]

-- | The same coins, and dust coins of 1200, 900 and 546 satoshis
dustCoinsTx :: Ctx -> Tx
dustCoinsTx ctx = fundingTx ctx 6 [(externalAddr 0, 50000), (externalAddr 0, 30000), (externalAddr 0, 1200), (externalAddr 0, 900), (externalAddr 0, 546)]

-- | A chain whose block 800000 holds the transaction, two blocks below its
-- best
confirmedChain :: Tx -> Chain
confirmedChain tx = Chain 800002 [(tx, Just 800000)]

-- | A coin of a transaction, as --coin takes it
coinArg :: Tx -> Int -> String
coinArg tx i = Text.unpack (txHashToHex (txHash tx)) <> ":" <> show i

-- | The coins preparetx or preparesendall leaves in the account, as its
-- answer lists them: outpoint, value and why
leftOf :: Response -> Either String [(OutPoint, Word64, LeftReason)]
leftOf (ResponsePrepared _ _ left) = Right [(c.leftCoinOutpoint, c.leftCoinValue, c.leftCoinReason) | c <- left]
leftOf other = Left (show other)

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
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--feerate", "10"] []
        prepared <- txOf sendAll
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 76600)]
        paidToWallet prepared `shouldBe` Map.empty
        leftOf sendAll `shouldBe` Right []

    it "give the fixed amounts as typed, and what remains to the address without one" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", fixedTo (otherAddr 0) "0.0003", textOf (otherAddr 1), "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 30000), (otherAddr 1, 46260)]

    it "share what remains equally among the addresses without an amount" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), textOf (otherAddr 1), "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 38130), (otherAddr 1, 38130)]

    it "prepare nothing when they do not cover the fixed amounts and the fee" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", fixedTo (otherAddr 0) "0.0008", textOf (otherAddr 1), "--feerate", "10"] [])
          `shouldReturn` Right "The coins, 80000 satoshis, do not cover the fixed amounts, 80000, and the fee, 3740"
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "need an address without an amount to receive what remains" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", fixedTo (otherAddr 0) "0.0003", "--feerate", "10"] [])
          `shouldReturn` Right "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"

    it "are those of --coin only, sent in full" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--coin", coinArg (twoCoinsTx ctx) 0, "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 48080)]
        paidToWallet prepared `shouldBe` Map.empty

    it "not given with --coin stay in the account, which the answer lists" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (leftOf <$> hwJson home ["preparesendall", textOf (otherAddr 0), "--coin", coinArg (twoCoinsTx ctx) 0, "--feerate", "10"] [])
          `shouldReturn` Right [(OutPoint (txHash (twoCoinsTx ctx)) 1, 30000, NotGiven)]

    it "stay in the account when the user locked them, which the answer lists" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoCoinsTx ctx) 1] []
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--feerate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 48080)]
        leftOf sendAll `shouldBe` Right [(OutPoint (txHash (twoCoinsTx ctx)) 1, 30000, Locked LockedByUser)]

    -- 30000 satoshis are 0.0003 bitcoins
    it "that stay in the account are shown after the transaction, each with why" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoCoinsTx ctx) 1] []
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--feerate", "10"] []
        -- The transaction, then the coins staying, then the next steps
        let (beforeSteps, steps) = Text.breakOn "Next, sign it" (runOut run)
            (transaction, staying) = Text.breakOn "Staying in the account:" beforeSteps
        transaction `shouldSatisfy` Text.isInfixOf "NoSigHash"
        staying `shouldSatisfy` Text.isInfixOf (Text.pack (coinArg (twoCoinsTx ctx) 1) <> " 0.00030000 btc, you locked it")
        steps `shouldSatisfy` Text.isInfixOf "hw signtx"

    it "are sent at a fee rate above 100 satoshis per byte only if the user takes it" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", textOf (otherAddr 0), "--feerate", "101"] ["n"]) `shouldReturn` Right highFeeRefused

    -- 340 bytes at 101 satoshis a byte are a fee of 34340, which leaves
    -- 80000 - 34340 = 45660 for the address
    it "are sent at a fee rate above 100 satoshis per byte without asking with --yes" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--feerate", "101", "--yes"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 45660)]

    -- The wallet has two accounts, and the coins are of main
    it "are those of the account of --account" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        prepared <- answer home ["preparesendall", textOf (otherAddr 0), "--account", "main", "--feerate", "10"] []
        (dBAccountName <$> accountOf prepared) `shouldBe` Right "main"
        paidToOthers <$> txOf prepared `shouldReturn` Map.fromList [(otherAddr 0, 76600)]

    it "are not sent when what remains for the address is below the --dust limit" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", textOf (otherAddr 0), "--dust", "80000", "--feerate", "10"] [])
          `shouldReturn` Right "What remains for each address without an amount, 76600 satoshis, is below the dust limit of 80000"

    -- The coin of 50000 has 3 confirmations, and the coin of 30000 has 1.
    -- The coin of 50000 alone makes 192 bytes, a fee of 1920.
    it "stay in the account with fewer confirmations than --minconf asks, which the answer lists" $
      withChain ctx (olderAndNewerChain ctx) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--minconf", "2", "--feerate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 48080)]
        leftOf sendAll `shouldBe` Right [(OutPoint (txHash (newerPayment ctx)) 0, 30000, NotSpendableYet)]

    it "are written to the --output file with the network of their account" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["preparesendall", textOf (otherAddr 0), "--output", home </> "unsigned.json", "--feerate", "10"] []
        (fmap txFileNetwork <$> Json.decodeFileStrict (home </> "unsigned.json")) `shouldReturn` Just (Just btc)

  describe "The coins worth less than their fee, sent by preparesendall or not" $ do
    it "are listed on stderr with what spending them or leaving them means, and asked about" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--feerate", "10"] ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf "3 coins cost more to spend than they are worth at 10 sat/byte, 1480 sat each:"
        runErr run `shouldSatisfy` Text.isInfixOf "What remains for the addresses without an amount is 74806 sat instead of 76600, 1794 less, and the account is empty."
        runErr run `shouldSatisfy` Text.isInfixOf dustQuestion

    it "stay in the account when the user answers anything but yes" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--feerate", "10"] ["n"]
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 76600)]

    it "are sent too when the user answers yes" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--feerate", "10"] ["y"]
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 74806)]

    it "stay in the account without a question with --leavedust, which the answer lists" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--leavedust", "--feerate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 76600)]
        let dustCoin i = OutPoint (txHash (dustCoinsTx ctx)) i
        leftOf sendAll `shouldBe` Right [(dustCoin 2, 1200, WorthLessThanFee), (dustCoin 3, 900, WorthLessThanFee), (dustCoin 4, 546, WorthLessThanFee)]

    it "are sent too without a question with --spenddust" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--spenddust", "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 74806)]

    -- The coins of 30000 and 900: 340 bytes, a fee of 3400, and 30900 - 3400
    it "are sent without a question when given with --coin" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--coin", coinArg (dustCoinsTx ctx) 1, "--coin", coinArg (dustCoinsTx ctx) 3, "--feerate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 27500)]

    it "are not left or spent by choice with --coin, whose coins are all sent" $
      withChain ctx (confirmedChain (dustCoinsTx ctx)) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparesendall", textOf (otherAddr 0), "--coin", coinArg (dustCoinsTx ctx) 1, "--leavedust", "--feerate", "10"] [])
          `shouldReturn` Right "The coins chosen with --coin or --spendfrom are all sent: give --spenddust or --leavedust without them"

{- Coin control -}

-- | Coins of 50000 satoshis at the first receiving address of the test
-- wallet, and 30000 at the second
twoAddressesTx :: Ctx -> Tx
twoAddressesTx ctx = fundingTx ctx 7 [(externalAddr 0, 50000), (externalAddr 1, 30000)]

-- | A coin of 50000 satoshis at the first receiving address of cash, the
-- Bitcoin Cash account of the test wallet
cashCoinTx :: Ctx -> Tx
cashCoinTx ctx = fundingTx ctx 8 [(bchExternalAddr, 50000)]

-- A payment of 20000 from the coin of 50000 makes one input and two
-- outputs, 226 bytes: a fee of 2260, and 27740 of change. Sending all of the
-- coin of 50000 makes 192 bytes: a fee of 1920, and 48080 for the address.
coinControlSpec :: Ctx -> Spec
coinControlSpec ctx =
  describe "Coin control" $ do
    it "spends the coins at the address of --spendfrom, and only them" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (externalAddr 0), "--feerate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(internalAddr 0, 27740)]
        leftOf prepared `shouldBe` Right []

    -- Both coins at the address, 80000: two inputs and two outputs make 374
    -- bytes, a fee of 3740, and 56260 of change. Smallest first would spend
    -- the coin of 30000 alone.
    it "spends every coin at the --spendfrom address, whatever --strategy says" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (externalAddr 0), "--strategy", "smallest-first", "--feerate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 80000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 56260)]

    -- The coin of 30000 pays 20000 alone: 226 bytes, a fee of 2260, and
    -- 7740 of change. The coin of 50000 stays, and is not listed.
    it "spends only what the payment needs among them with --select, as --strategy says" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        answered <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (externalAddr 0), "--select", "--strategy", "smallest-first", "--feerate", "10"] []
        prepared <- txOf answered
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 7740)]
        leftOf answered `shouldBe` Right []

    -- The coin of 30000 is locked, so smallest first spends the one of
    -- 50000: 27740 of change. The list says why the one of 30000 was not
    -- taken.
    it "lists the chosen coins that cannot be spent with --select too, with why" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoCoinsTx ctx) 1] []
        answered <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (externalAddr 0), "--select", "--strategy", "smallest-first", "--feerate", "10"] []
        paidToWallet <$> txOf answered `shouldReturn` Map.fromList [(internalAddr 0, 27740)]
        leftOf answered `shouldBe` Right [(OutPoint (txHash (twoCoinsTx ctx)) 1, 30000, Locked LockedByUser)]

    it "refuses --select without --coin or --spendfrom" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--select", "--feerate", "10"] [])
          `shouldReturn` Right "--select needs coins to select among: give --coin or --spendfrom"

    -- 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the first receiving address of
    -- cash in the legacy form; hw stores it as
    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u. The wallet has
    -- main too: the address names the account.
    it "finds the account of a --spendfrom address however it is written" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        _ <- receiveFor home "cash"
        _ <- answer home ["sync"] []
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--feerate", "10"] []
        (dBAccountName <$> accountOf prepared) `shouldBe` Right "cash"
        paidToOthers <$> txOf prepared `shouldReturn` Map.fromList [(otherAddr 0, 20000)]

    it "spends the coins chosen that can be spent, and lists the others with why" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 0, "--coin", coinArg (twoAddressesTx ctx) 1, "--feerate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(internalAddr 0, 27740)]
        leftOf prepared `shouldBe` Right [(OutPoint (txHash (twoAddressesTx ctx)) 1, 30000, Locked LockedByUser)]

    -- 30000 satoshis are 0.0003 bitcoins
    it "shows the coins chosen that cannot be spent after the transaction" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 0, "--coin", coinArg (twoAddressesTx ctx) 1, "--feerate", "10"] []
        let (transaction, staying) = Text.breakOn "Staying in the account:" (runOut run)
        transaction `shouldSatisfy` Text.isInfixOf "NoSigHash"
        staying `shouldSatisfy` Text.isInfixOf (Text.pack (coinArg (twoAddressesTx ctx) 1) <> " 0.00030000 btc, you locked it")

    it "prepares nothing when none of the coins chosen can be spent, and tells why" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        _ <- answer home ["lockcoin", coinArg (twoAddressesTx ctx) 1] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", coinArg (twoAddressesTx ctx) 1, "--feerate", "10"] [])
          `shouldReturn` Right ("None of the coins chosen can be spent now: " <> Text.pack (coinArg (twoAddressesTx ctx) 1) <> ", you locked it")
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "is refused a --spendfrom address that is not in the account" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (otherAddr 1), "--feerate", "10"] [])
          `shouldReturn` Right "The address 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ is not in the account main"

    it "is refused a --coin the account does not have" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        let unknown = Text.unpack (txHashToHex (fakeTxHash 9)) <> ":0"
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", unknown, "--feerate", "10"] [])
          `shouldReturn` Right ("The account has no coin " <> Text.pack unknown)

    it "lists nothing left in the account without coin control" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (leftOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []) `shouldReturn` Right []

    it "makes preparesendall send the coins at --spendfrom, listing the others as not chosen" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        sendAll <- answer home ["preparesendall", textOf (otherAddr 0), "--spendfrom", textOf (externalAddr 0), "--feerate", "10"] []
        paidToOthers <$> txOf sendAll `shouldReturn` Map.fromList [(otherAddr 0, 48080)]
        leftOf sendAll `shouldBe` Right [(OutPoint (txHash (twoAddressesTx ctx)) 1, 30000, NotGiven)]

{- The change sent back -}

-- A payment of 20000 from the coin of 50000 leaves 27740 of change. One of
-- 60000 needs the coins at both addresses, 80000: two inputs and two
-- outputs make 374 bytes, a fee of 3740, and 16260 of change.
changeBackSpec :: Ctx -> Spec
changeBackSpec ctx =
  describe "The change of preparetx sent back" $ do
    let chain = confirmedChain (twoAddressesTx ctx)
        fromBoth = ["preparetx", fixedTo (otherAddr 0) "0.0006", "--changeback", "--feerate", "10"]
    it "goes to the one address the payment spends from, without a question" $
      withChain ctx chain $ \home _ -> do
        synced home
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (externalAddr 0), "--changeback", "--feerate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 27740)]

    it "goes to the address whose path the user types, among those the payment spends from" $
      withChain ctx chain $ \home _ -> do
        synced home
        prepared <- answer home fromBoth ["/0/1"]
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 1, 16260)]

    it "goes to the address with the largest total when the user types nothing" $
      withChain ctx chain $ \home _ -> do
        synced home
        prepared <- answer home fromBoth [""]
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 16260)]

    it "is asked for with the addresses listed as balances lists them, the largest first" $
      withChain ctx chain $ \home _ -> do
        synced home
        run <- hw home fromBoth ["/0/1"]
        runErr run `shouldSatisfy` Text.isInfixOf "The payment spends coins at 2 addresses. Its change can go back to any of them:\n/0/0: "
        runErr run `shouldSatisfy` Text.isInfixOf ("/0/0: " <> externalAddrsText !! 0 <> "\n  Total      : 0.00050000 btc")
        runErr run `shouldSatisfy` Text.isInfixOf ("/0/1: " <> externalAddrsText !! 1 <> "\n  Total      : 0.00030000 btc")
        runErr run `shouldSatisfy` Text.isInfixOf "Which address gets the change? Type its path [/0/0]"

    it "goes back without --changeback when the configuration file says so" $
      withChain ctx chain $ \home _ -> do
        synced home
        setChangeBack home True
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (externalAddr 0), "--feerate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 27740)]

    it "goes to a new change address with --newchange, whatever the configuration file says" $
      withChain ctx chain $ \home _ -> do
        synced home
        setChangeBack home True
        prepared <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (externalAddr 0), "--newchange", "--feerate", "10"] []
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(internalAddr 0, 27740)]

    it "cannot go back and to a new change address at once" $
      withHome $ \home -> do
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--changeback", "--newchange", "--feerate", "10"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "Invalid option `--newchange'"

{- Replacing a stuck payment -}

-- | A home synced with the paid chain, with the payment of 20000 to the
-- other wallet sent at 10 satoshis a byte: the id of the payment
sentPayment :: FilePath -> IO String
sentPayment home = do
  nosig <- paymentSigned home
  txidOf =<< txOf =<< answer home ["sendtx", nosig, "--yes"] []

-- | Sign and send a pending transaction: the id of the transaction sent
signAndSend :: FilePath -> String -> IO String
signAndSend home nosig = do
  _ <- answer home ["signtx", nosig] seedOnce
  txidOf =<< txOf =<< answer home ["sendtx", nosig, "--yes"] []

-- | The id of a signed transaction, as typed
txidOf :: TxInfo -> IO String
txidOf info = case txInfoStatus info of
  Pending status | Just tid <- pendingTxid status -> return $ Text.unpack (txHashToHex tid)
  OnChain status -> return $ Text.unpack (txHashToHex (chainTxid status))
  _ -> fail "The transaction is not signed"

-- | The ids of the transactions a replacement replaces, as typed
replacesOf :: TxInfo -> [String]
replacesOf info = case txInfoStatus info of
  Pending status -> Text.unpack . txHashToHex <$> pendingReplaces status
  OnChain _ -> []

-- | The fee of a transaction
feeOf :: TxInfo -> Natural
feeOf = summaryFee . txInfoSummary

-- | The transactions of the history: each by its id, dead or not, with its
-- confirmations
historyOf :: Response -> Either String [(String, Bool, Natural)]
historyOf (ResponseTxs _ infos) =
  Right [(Text.unpack (txHashToHex (chainTxid status)), chainDead status, chainConfirmations status) | info <- infos, OnChain status <- [txInfoStatus info]]
historyOf other = Left (show other)

-- | The pending transactions of a listing
pendingInfosOf :: Response -> Either String [TxInfo]
pendingInfosOf (ResponsePendingTxs _ held) = Right (snd <$> held)
pendingInfosOf other = Left (show other)

-- | Whether each pending transaction is in conflict
conflictsOf :: [TxInfo] -> [Bool]
conflictsOf infos = [pendingConflicted status | info <- infos, Pending status <- [txInfoStatus info]]

-- | The nosigHashes of the pending transactions, as typed
pendingNosigsOf :: Response -> Either String [String]
pendingNosigsOf (ResponsePendingTxs _ held) =
  Right [Text.unpack (txHashToHex (pendingNosigHash status)) | (_, info) <- held, Pending status <- [txInfoStatus info]]
pendingNosigsOf other = Left (show other)

-- The payment of 20000 at 10 satoshis a byte pays 2260 for its 226 bytes,
-- and leaves 27740 of change. At 20 satoshis a byte, its replacement pays
-- 4520 and leaves 25480.
bumpFeeSpec :: Ctx -> Spec
bumpFeeSpec ctx = do
  describe "A stuck payment replaced by bumpfee" $ do
    it "is replaced, still pending, by one that takes the higher fee from its change" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 25480)], 4520)

    -- At 100 satoshis a byte, the 226 bytes pay 22600 and leave 50000 -
    -- 20000 - 22600 = 7400 of change, less than the 10000 of --dust: the fee
    -- is all the rest, 50000 - 20000 = 30000
    it "gives the fee a change below the --dust limit" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "100", "--dust", "10000"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.empty, 30000)

    it "is replaced once the sync has found it on the network" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 25480)], 4520)

    it "is named by its replacement, which pendingtxs shows" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        replacesOf bumped `shouldBe` [stuck]
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("Replaces : " <> Text.pack stuck)

    it "gives way to its replacement once sent: the history shows it dead, and the replacement confirmed" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        replacement <- signAndSend home nosig
        modifyIORef chain mine
        (accountsOf <$> hwJson home ["sync"] []) `shouldReturn` Right [("main", 0, 25480)]
        history <- hwJson home ["txs"] []
        (filter (\(tid, _, _) -> tid `elem` [stuck, replacement]) <$> historyOf history)
          `shouldSatisfy` either (const False) (\rows -> sort rows == sort [(stuck, True, 0), (replacement, False, 1)])

    it "leaves the pending transactions once its replacement is sent" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        _ <- signAndSend home nosig
        (pendingNosigsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [nosig]

    -- The store still has the stuck payment: its replacement spends the
    -- same coin, which is no conflict
    it "is no conflict for its replacement, after a sync that finds it still on the network" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["sync"] []
        listed <- hwJson home ["pendingtxs"] []
        (conflictsOf <$> pendingInfosOf listed) `shouldBe` Right [False]
        sent <- txOf =<< answer home ["sendtx", nosig, "--yes"] []
        pendingState sent `shouldBe` Just (True, True)

    it "takes the higher fee from the change it sent back to the address it spends from" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spendfrom", textOf (externalAddr 0), "--changeback", "--feerate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        paidToWallet bumped `shouldBe` Map.fromList [(externalAddr 0, 25480)]

    -- The payment to the second receiving address is no change: only the
    -- change at /1/0 pays the higher fee
    it "keeps what it pays another address of the wallet" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (externalAddr 1) "0.0002", "--spendfrom", textOf (externalAddr 0), "--feerate", "10"] []
        stuck <- signAndSend home nosig
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        paidToWallet bumped `shouldBe` Map.fromList [(externalAddr 1, 20000), (internalAddr 0, 25480)]

    it "is refused a fee rate that is not above its own" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "10"] [])
          `shouldReturn` Right "The transaction pays 10 sat/byte: give a --feerate of at least 11"

    it "cannot be replaced once in a block" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        stuck <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "20"] [])
          `shouldReturn` Right "The transaction is in a block already: nothing can replace it"

    -- Nothing is asked about the fee rate of 200 for a transaction that is
    -- not there: the input is empty, and a question would end the command
    it "is looked up before anything is asked" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["bumpfee", Text.unpack (txHashToHex (fakeTxHash 9)), "--feerate", "200"] []
        run `shouldSatisfy` printed ("The wallet has no transaction " <> txHashToHex (fakeTxHash 9))
        runErr run `shouldNotSatisfy` Text.isInfixOf "Prepare the transaction anyway"

    -- At 120 satoshis a byte the coin still pays: 27120 of fee, 2880 of change
    it "is not replaced at a fee rate above 100 satoshis per byte unless the user agrees" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "120"] ["n"])
          `shouldReturn` Right "The transaction was not prepared: give a lower --feerate"

    -- 226 bytes at 120 satoshis a byte are a fee of 27120, which leaves
    -- 50000 - 20000 - 27120 = 2880 of change. Nothing is typed: a question
    -- would cancel the command.
    it "is replaced at a fee rate above 100 satoshis per byte without asking with --yes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "120", "--yes"] []
        (paidToWallet bumped, feeOf bumped) `shouldBe` (Map.fromList [(internalAddr 0, 2880)], 27120)

    it "is not stuck before it is sent" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        (errorOf <$> hwJson home ["bumpfee", nosig, "--feerate", "20"] [])
          `shouldReturn` Right "The transaction is not sent yet: delete it with deletetx, and prepare it again at the fee rate you want"

  -- At 30 satoshis a byte, the replacement of the replacement pays 6780 and
  -- leaves 23220
  describe "A stuck payment replaced once" $ do
    it "is not replaced again while its replacement is pending" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack nosig <> ". Send it, or delete it with deletetx")

    it "names its pending replacement by its txid once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        txid <- txidOf =<< txOf =<< answer home ["signtx", nosig] seedOnce
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack txid <> ". Send it, or delete it with deletetx")

    it "is not replaced again once the sync has found it on the network either" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "30"] [])
          `shouldReturn` Right ("A replacement of the transaction is pending: " <> Text.pack nosig <> ". Send it, or delete it with deletetx")

    it "is bumped through its replacement once that one is sent" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        replacement <- signAndSend home nosig
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "30"] [])
          `shouldReturn` Right ("The transaction is replaced by " <> Text.pack replacement <> ", which is sent: bump that one instead")
        bumped <- txOf =<< answer home ["bumpfee", replacement, "--feerate", "30"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000)], Map.fromList [(internalAddr 0, 23220)], 6780)
        replacesOf bumped `shouldBe` [replacement]

    it "cannot have its replacement sent once it is in a block" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        stuck <- sentPayment home
        nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] []
        _ <- answer home ["signtx", nosig] seedOnce
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["sendtx", nosig, "--yes"] [])
          `shouldReturn` Right ("The transaction " <> Text.pack stuck <> " it replaces is in a block already: delete this replacement with deletetx")

  -- main pays 20000 to the first address of second, and another wallet of
  -- the seed spends that output from second before anything confirms
  describe "A stuck payment an output of which another account spends" $
    it "is not replaced, since that spend would be removed too" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "main"] []
        secondAddr <- receiveFor home "second"
        _ <- answer home ["sync"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", secondAddr <> ":0.0002", "--account", "main", "--feerate", "10"] []
        stuck <- signAndSend home nosig
        [sent] <- inMempool <$> readIORef chain
        index <- maybe (fail "The payment does not pay second") return $ findIndex ((== Right account1Addr) . scriptToAddressBS ctx . (.script)) sent.outputs
        let spender = spendingTx ctx [(txHash sent, fromIntegral index)] [(otherAddr 2, 15000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(spender, Nothing)]}
        _ <- answer home ["sync"] []
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "20", "--account", "main"] [])
          `shouldReturn` Right ("The transaction " <> txHashToHex (txHash spender) <> " of the account second spends an output of the transaction, and would be removed with it: hw cannot replace it from this account")

  -- The child pays 10000 at 50 satoshis a byte: 11300 of fee for its 226
  -- bytes. Replacing both, 260 bytes, must pay 2260 + 11300 + 260 = 13820:
  -- 54 satoshis a byte, which pays 14040 and leaves 50000 - 30000 - 14040 =
  -- 5960 of change.
  describe "A stuck payment whose change a payment at a higher rate spends" $ do
    let withCostlyChild home = do
          stuck <- sentPayment home
          _ <- answer home ["sync"] []
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--feerate", "50"] []
          _ <- signAndSend home nosig
          return stuck
    it "is refused a rate that cannot pay for both, before any question" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- withCostlyChild home
        run <- hw home ["bumpfee", stuck, "--feerate", "20"] []
        runErr run `shouldNotSatisfy` Text.isInfixOf "Replace both"
        run `shouldSatisfy` printed "The replacement pays 5200 satoshis of fee, and must pay 13820: the fees of the transactions it replaces, and its own 260 bytes. Give a --feerate of at least 54"

    it "is replaced with the payment at a rate that pays for both" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        stuck <- withCostlyChild home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "54"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 5960)], 14040)

  describe "A stuck payment replaced from an online computer" $
    it "has its replacement exported, signed offline, imported back and sent, which retires it" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        nosig <- nosigOf =<< txOf =<< answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--feerate", "10"] []
        _ <- answer signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        _ <- answer online ["importtx", signer </> "signed.json"] []
        stuck <- txidOf =<< txOf =<< answer online ["sendtx", nosig, "--yes"] []
        bumpNosig <- nosigOf =<< txOf =<< answer online ["bumpfee", stuck, "--feerate", "20", "--output", online </> "bump.json"] []
        signed <- txOf =<< answer signer ["signtx", "--input", online </> "bump.json", "--output", signer </> "bump-signed.json"] seedOnce
        replacesOf signed `shouldBe` [stuck]
        _ <- answer online ["importtx", signer </> "bump-signed.json"] []
        _ <- answer online ["sendtx", bumpNosig, "--yes"] []
        (pendingNosigsOf <$> hwJson online ["pendingtxs"] []) `shouldReturn` Right [bumpNosig]

  describe "A payment bumpfee does not replace" $ do
    it "is a payment the account receives: only its payer can replace it" $
      withChain ctx (Chain 800002 [(payment ctx, Nothing)]) $ \home _ -> do
        synced home
        let received = Text.unpack (txHashToHex (txHash (payment ctx)))
        (errorOf <$> hwJson home ["bumpfee", received, "--feerate", "20"] [])
          `shouldReturn` Right "Only a payment from the coins of the account can be replaced: this transaction spends coins of another wallet"

    it "is one of Bitcoin Cash, which replaces none" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        createMain home
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        _ <- receiveFor home "cash"
        _ <- answer home ["sync"] []
        let received = Text.unpack (txHashToHex (txHash (cashCoinTx ctx)))
        (errorOf <$> hwJson home ["bumpfee", received, "--feerate", "20"] [])
          `shouldReturn` Right "Bitcoin Cash replaces no transaction: its nodes keep the first one they see"

  -- The payment of 40000 from the coin of 50000 at /0/0 leaves 7740 of
  -- change. At 60 satoshis a byte, the coin alone cannot pay even the 11520
  -- of a transaction without change. With the coin of 30000 at /0/1, the
  -- replacement of 374 bytes pays 22440 and leaves 80000 - 40000 - 22440 =
  -- 17560 of change.
  describe "A stuck payment whose change cannot pay the higher fee" $ do
    let chain = confirmedChain (twoAddressesTx ctx)
        stuckPayment home = do
          synced home
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0004", "--spendfrom", textOf (externalAddr 0), "--feerate", "10"] []
          signAndSend home nosig
    it "takes a confirmed coin of the account, as the user agrees" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "60"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 40000)], Map.fromList [(internalAddr 0, 17560)], 22440)

    it "names the coin it asks to add" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        run <- hw home ["bumpfee", stuck, "--feerate", "60"] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf "The coins of the transaction cannot pay its fee at 60 sat/byte, and this coin of the account would:"
        runErr run `shouldSatisfy` Text.isInfixOf (Text.pack (coinArg (twoAddressesTx ctx) 1) <> "  30000 sat at /0/1")
        runErr run `shouldSatisfy` Text.isInfixOf "Add it? [y/N]"

    -- Without the coin, the replacement of 192 bytes pays 11520 and has no
    -- change: the recipient pays the 40000 + 11520 - 50000 = 1520 missing
    it "takes the higher fee from what the recipient receives when the user declines the coin, as the user agrees" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "60"] ["n", "y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 38480)], Map.empty, 11520)

    it "offers the recipient once the coin is declined" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        run <- hw home ["bumpfee", stuck, "--feerate", "60"] ["n", "n"]
        runErr run `shouldSatisfy` Text.isInfixOf "Add it? [y/N]"
        runErr run `shouldSatisfy` Text.isInfixOf "The coins of the transaction cannot pay its fee at 60 sat/byte, and no coin of the account is added. The recipient can pay the 1520 sat missing out of what it receives:"
        runErr run `shouldSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 0)) <> "  40000 sat, 38480 paying it")
        runErr run `shouldSatisfy` Text.isInfixOf "Reduce what it receives? [y/N]"

    it "is not replaced when the user wants neither the coin added nor the recipient paying" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "60"] ["n", "n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "takes a confirmed coin of the account without asking with --yes" $
      withChain ctx chain $ \home _ -> do
        stuck <- stuckPayment home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "60", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 40000)], Map.fromList [(internalAddr 0, 17560)], 22440)

  -- The send-all of the coins of 50000 and 30000 pays 76600 to one address
  -- at 10 satoshis a byte, 3400 for its 340 bytes, with no change, and
  -- leaves no coin in the account. At 20 satoshis a byte, its replacement
  -- pays 6800: the address pays the 76600 + 6800 - 80000 = 3400 missing,
  -- and receives 73200.
  describe "A stuck send-all, whose recipient alone can pay the higher fee" $ do
    let sentAll home = do
          synced home
          nosig <- nosigOf =<< txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--feerate", "10"] []
          signAndSend home nosig
    it "is replaced by one that takes the higher fee from what the recipient receives, as the user agrees" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 73200)], Map.empty, 6800)

    it "names the recipient and what it would receive when it asks" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--feerate", "20"] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf "The coins of the transaction cannot pay its fee at 20 sat/byte, and no coin of the account is added. The recipient can pay the 3400 sat missing out of what it receives:"
        runErr run `shouldSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 0)) <> "  76600 sat, 73200 paying it")
        runErr run `shouldSatisfy` Text.isInfixOf "Reduce what it receives? [y/N]"

    it "is not replaced when the user does not want the recipient to pay" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "20"] ["n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "takes the higher fee from what the recipient receives without asking with --yes" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 73200)], Map.empty, 6800)

    -- At 250 satoshis a byte, the 340 bytes pay 85000, more than the 80000
    -- of the coins: 76600 + 85000 - 80000 = 81600 are missing. Nothing is
    -- typed: the rate above 100 is not even asked about.
    it "is refused when the recipient cannot pay either, before any question" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--feerate", "250"] []
        run `shouldSatisfy` printed "The coins of the transaction cannot pay its fee at 250 sat/byte, and no confirmed coins of the account would. The recipient cannot pay the 81600 satoshis missing without being left with less than the dust limit of 546 satoshis"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Reduce what it receives?"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Prepare the transaction anyway"

  -- The send-all to two addresses, 30000 to the second and the 80000 -
  -- 30000 - 3740 = 46260 that remain to the first, pays 3740 for its 374
  -- bytes. At 20 satoshis a byte, its replacement pays 7480: 3740 are
  -- missing, which one of them pays alone, or each pays 1870 of. The
  -- outputs are in an order drawn at random, so the recipient is picked by
  -- its address, and the lines are checked without their numbers.
  describe "A stuck send-all to several recipients" $ do
    let sentAll home = do
          synced home
          nosig <- nosigOf =<< txOf =<< answer home ["preparesendall", textOf (otherAddr 0), fixedTo (otherAddr 1) "0.0003", "--feerate", "10"] []
          signAndSend home nosig
    it "is replaced by one that takes the higher fee from the recipient the user picks" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] [Text.pack (textOf (otherAddr 1))]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 46260), (otherAddr 1, 26260)], Map.empty, 7480)

    it "is replaced by one that takes the higher fee from all the recipients in equal shares, when the user says all" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] ["all"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 44390), (otherAddr 1, 28130)], Map.empty, 7480)

    it "lists the recipients with what each would receive, and asks which pays" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        run <- hw home ["bumpfee", stuck, "--feerate", "20"] ["no"]
        runErr run `shouldSatisfy` Text.isInfixOf "The 3740 sat missing can come out of what the recipients receive, one of them paying it alone, or all of them in equal shares:"
        runErr run `shouldSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 0)) <> "  46260 sat, 42520 paying it alone, 44390 paying its share")
        runErr run `shouldSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 1)) <> "  30000 sat, 26260 paying it alone, 28130 paying its share")
        runErr run `shouldSatisfy` Text.isInfixOf "Who pays it? Type 1, 2 or all, or no [no]"

    it "is not replaced when nobody pays, which an empty answer says" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "20"] [""]) `shouldReturn` Right "The transaction was not replaced"

    -- --yes answers yes, and this is no yes-or-no question
    it "still asks which recipient pays with --yes" $
      withChain ctx (confirmedChain (twoCoinsTx ctx)) $ \home _ -> do
        stuck <- sentAll home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20", "--yes"] [Text.pack (textOf (otherAddr 1))]
        paidToOthers bumped `shouldBe` Map.fromList [(otherAddr 0, 46260), (otherAddr 1, 26260)]

  -- The second payment, of 10000, spends the change of 27740 of the first
  -- and leaves 15480 of change at /1/1. One transaction of one input and
  -- three outputs, 260 bytes, replaces both at 20 satoshis a byte: it pays
  -- 5200 and leaves 50000 - 30000 - 5200 = 14800 at /1/1.
  describe "A stuck payment whose change another payment spends" $ do
    let withSecond home = do
          stuck <- sentPayment home
          _ <- answer home ["sync"] []
          nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--feerate", "10"] []
          second <- signAndSend home nosig
          return (stuck, second)
    it "is replaced with that payment, as the user agrees, by one that pays the recipients of both" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (stuck, second) <- withSecond home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20"] ["y"]
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 14800)], 5200)
        replacesOf bumped `shouldBe` [stuck, second]

    it "names that payment when it asks" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (stuck, second) <- withSecond home
        run <- hw home ["bumpfee", stuck, "--feerate", "20"] ["y"]
        runErr run `shouldSatisfy` Text.isInfixOf ("Replacing the transaction removes those of the account that spend its change:\n  " <> Text.pack second)
        runErr run `shouldSatisfy` Text.isInfixOf "Replace both with one transaction that pays the recipients of both? [y/N]"

    it "is not replaced when the user does not want that payment replaced too" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (stuck, _) <- withSecond home
        (errorOf <$> hwJson home ["bumpfee", stuck, "--feerate", "20"] ["n"]) `shouldReturn` Right "The transaction was not replaced"

    -- Nothing is typed: a question would cancel the command
    it "is replaced with that payment without asking with --yes" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        (stuck, second) <- withSecond home
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--feerate", "20", "--yes"] []
        (paidToOthers bumped, paidToWallet bumped, feeOf bumped)
          `shouldBe` (Map.fromList [(otherAddr 0, 20000), (otherAddr 1, 10000)], Map.fromList [(internalAddr 1, 14800)], 5200)
        replacesOf bumped `shouldBe` [stuck, second]

{- Transactions as hw shows them -}

-- | A payment of 50000 to the first receiving address of the test wallet,
-- from another wallet that keeps 70000 of change at an address of its own.
-- The store gives its input 121000, so that its fee is 1000. The payer
-- spends from otherAddr 0.
paymentWithPayerChange :: Ctx -> Tx
paymentWithPayerChange ctx = fundingTx ctx 9 [(externalAddr 0, 50000), (otherAddr 1, 70000)]

-- | What hw shows of the payment of 20000 prepared in a synced home
shownPayment :: FilePath -> [String] -> IO Text
shownPayment home options = runOut <$> hw home (["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] <> options) []

-- The payment of 20000 spends the coin of 50000 at /0/0, and leaves 27740
-- of change
layoutSpec :: Ctx -> Spec
layoutSpec ctx =
  describe "A transaction as hw shows it" $ do
    it "shows the addresses of the wallet a payment spends from, with their paths" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "Spent from:\n  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj: 0.00050000 btc\n    Path: /0/0\n"

    it "shows the labels of the addresses of the wallet" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["label", "0", "salary"] []
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj: 0.00050000 btc\n    Path: /0/0\n    Label: salary\n"

    it "shows the recipients of a payment" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "Sending to:\n  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU: 0.00020000 btc\n"

    it "shows the change of a payment, at its change address" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        shown <- shownPayment home []
        shown `shouldSatisfy` Text.isInfixOf "Change to:\n  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn: 0.00027740 btc\n    Path: /1/0\n"

    it "shows the change a payment sends back to the address it spends from" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        shown <- shownPayment home ["--spendfrom", textOf (externalAddr 0), "--changeback"]
        shown `shouldSatisfy` Text.isInfixOf "Change to:\n  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj: 0.00027740 btc\n    Path: /0/0\n"

    -- The payer's coins are worth 121000, nothing to the wallet
    it "shows the addresses the payer of a payment received spends from, without their amounts, in txs" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "Spent from:\n  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\n"

    it "shows the address of the wallet a payment received is sent to, in txs" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        listed `shouldSatisfy` Text.isInfixOf "Sending to:\n  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj: 0.00050000 btc\n    Path: /0/0\n"

    it "does not show the change the payer of a payment received keeps" $
      withChain ctx (confirmedChain (paymentWithPayerChange ctx)) $ \home _ -> do
        synced home
        listed <- runOut <$> hw home ["txs"] []
        listed `shouldNotSatisfy` Text.isInfixOf (Text.pack (textOf (otherAddr 1)))

    -- Paying the second receiving address from the first leaves 27740 of
    -- change at /1/0: both outputs are the wallet's, and neither can be told
    -- to be the change
    it "shows every output of a transaction between addresses of the wallet as sent to, none as change" $
      withChain ctx (confirmedChain (twoAddressesTx ctx)) $ \home _ -> do
        synced home
        shown <- runOut <$> hw home ["preparetx", fixedTo (externalAddr 1) "0.0002", "--spendfrom", textOf (externalAddr 0), "--feerate", "10"] []
        let (_, sent) = Text.breakOn "Sending to:" shown
        shown `shouldSatisfy` Text.isPrefixOf "Internal"
        sent `shouldSatisfy` Text.isInfixOf "  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg: 0.00020000 btc\n    Path: /0/1\n"
        sent `shouldSatisfy` Text.isInfixOf "  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn: 0.00027740 btc\n    Path: /1/0\n"
        shown `shouldNotSatisfy` Text.isInfixOf "Change to:"

-- old key
paymentAndKeyChain :: Ctx -> Chain
paymentAndKeyChain ctx = Chain 800002 [(payment ctx, Just 800000), (oldCoin ctx, Just 800000)]

-- | A home with the accounts main and second, main holding a payment, and
-- second a sweep of the old key, prepared after it
twoAccountsPending :: FilePath -> IO ()
twoAccountsPending home = do
  createMain home
  _ <- answer home ["createacc", "second"] seedTwice
  _ <- answer home ["receive", "--account", "main"] []
  _ <- answer home ["sync"] []
  _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--account", "main", "--feerate", "10"] []
  secondAddr <- receiveFor home "second"
  keys <- writeKeyFile home
  void $ answer home ["preparesweep", "--prvkeys", keys, "--sweepto", secondAddr, "--feerate", "10"] []

-- | The account pendingtxs was asked for, and the account of each pending
-- transaction it lists
pendingAccountsOf :: Response -> Either String (Maybe Text, [Text])
pendingAccountsOf (ResponsePendingTxs asked held) = Right (dBAccountName <$> asked, dBAccountName . fst <$> held)
pendingAccountsOf other = Left (show other)

-- | When each pending transaction pendingtxs lists was created
createdTimesOf :: Response -> Either String [Maybe UTCTime]
createdTimesOf (ResponsePendingTxs _ held) = Right (created . snd <$> held)
  where
    created info = case txInfoStatus info of
      Pending status -> pendingCreated status
      OnChain _ -> Nothing
createdTimesOf other = Left (show other)

pendingListSpec :: Ctx -> Spec
pendingListSpec ctx =
  describe "The pending transactions pendingtxs lists" $ do
    it "are those of every account without --account, newest first, each with its account" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        (pendingAccountsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right (Nothing, ["second", "main"])

    it "are those of the account of --account only" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        (pendingAccountsOf <$> hwJson home ["pendingtxs", "--account", "main"] []) `shouldReturn` Right (Just "main", ["main"])

    it "show their account when listed for every account" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "Account  : second"
        runOut run `shouldSatisfy` Text.isInfixOf "Account  : main"

    it "do not show their account when listed for one account" $
      withChain ctx (paymentAndKeyChain ctx) $ \home _ -> do
        twoAccountsPending home
        run <- hw home ["pendingtxs", "--account", "main"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "Account  :"

    -- The time is the wallet's clock when preparetx ran, within a second
    it "have the time they were created" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        started <- getCurrentTime
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        ended <- getCurrentTime
        times <- createdTimesOf <$> hwJson home ["pendingtxs"] []
        let duringRun time = addUTCTime (-1) started <= time && time <= addUTCTime 1 ended
        (fmap (fmap duringRun) <$> times) `shouldBe` Right [Just True]

    it "show an unsigned transaction by its nosigHash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("NoSigHash: " <> Text.pack nosig)

    it "show a signed transaction by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("TxHash   : " <> Text.pack txid)

    it "hide the nosigHash of a signed transaction" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf (Text.pack nosig)

    it "show the time they were created" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "Created  : "

    it "are none, which pendingtxs says" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["pendingtxs"] []
        runOut run `shouldSatisfy` Text.isInfixOf "There are no pending transactions"

{- Balances -}

-- | The addresses balances lists, each by its text, with the total, the
-- spendable and the unspendable balances of its coins
balancesOf :: Response -> Either String [(Text, Word64, Word64, Word64)]
balancesOf (ResponseBalances _ addrs) =
  Right [(dBAddressAddress addr, coinBalanceTotal b, coinBalanceSpendable b, coinBalanceUnspendable b) | (addr, b) <- addrs]
balancesOf other = Left (show other)

balancesListSpec :: Ctx -> Spec
balancesListSpec ctx =
  describe "The addresses balances lists" $ do
    -- The second receiving address holds nothing
    it "are those that hold coins, each with the value of its coins and what it can spend" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["receive"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(externalAddrsText !! 0, 50000, 50000, 0)]

    it "count the coin a pending payment spends as unspendable" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(externalAddrsText !! 0, 50000, 0, 50000)]

    -- The payment of 20000 spends the coin of 50000 and leaves 27740 of
    -- change at the first change address
    it "include the change addresses" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        nosig <- paymentSigned home
        _ <- answer home ["sendtx", nosig, "--yes"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (balancesOf <$> hwJson home ["balances"] []) `shouldReturn` Right [(internalAddrsText !! 0, 27740, 27740, 0)]

    it "show the path of each address, then its total, spendable and unspendable balances" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        run <- hw home ["balances"] []
        runOut run `shouldSatisfy` Text.isInfixOf ("/0/0: " <> externalAddrsText !! 0)
        runOut run `shouldSatisfy` Text.isInfixOf "Total      : 0.00050000 btc"
        runOut run `shouldSatisfy` Text.isInfixOf "Spendable  : 0.00000000 btc"
        runOut run `shouldSatisfy` Text.isInfixOf "Unspendable: 0.00050000 btc"

    it "are none before a payment, which balances says" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["balances"] []
        runOut run `shouldSatisfy` Text.isInfixOf "There are no addresses with coins to display"

{- Next steps -}

-- | The nosigHash of the one pending transaction of a home
onePendingNosig :: FilePath -> IO Text
onePendingNosig home = Text.pack <$> (nosigOf =<< onePendingTx home)

-- | Whether the output of a run shows the command
showsCommand :: Text -> Run -> Bool
showsCommand command run = command `Text.isInfixOf` runOut run

-- The pretty output, after the answer
nextStepSpec :: Ctx -> Spec
nextStepSpec ctx =
  describe "The next step of a payment or a sweep, shown after the answer" $ do
    it "is to sign the transaction preparetx made, or to export it to sign it offline" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signtx " <> nosig)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> nosig <> " FILE")

    it "is to sign the file preparetx wrote on an offline computer, or the transaction here" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", home </> "unsigned.json", "--feerate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signtx --input " <> Text.pack (home </> "unsigned.json") <> "\n")
        run `shouldSatisfy` showsCommand ("hw signtx " <> nosig)

    it "is to sign the transaction preparesendall made, or to export it to sign it offline" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--feerate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signtx " <> nosig)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> nosig <> " FILE")

    it "is to send the transaction signtx signed in the wallet, by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--feerate", "10"] []
        run <- hw home ["signtx", nosig] seedOnce
        txid <- txidOf =<< onePendingTx home
        run `shouldSatisfy` showsCommand ("hw sendtx " <> Text.pack txid)

    it "is to send the file signtx signed offline, from the online computer" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--feerate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.json", "--output", signer </> "signed.json"] seedOnce
        run `shouldSatisfy` showsCommand ("hw sendtx --input " <> Text.pack (signer </> "signed.json"))

    it "is to send the --input file signtx replaced with the signed transaction" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        signerAndOnline signer online
        _ <- answer online ["preparetx", fixedTo (otherAddr 0) "0.0002", "--output", online </> "unsigned.json", "--feerate", "10"] []
        run <- hw signer ["signtx", "--input", online </> "unsigned.json"] (["y"] <> seedOnce)
        run `shouldSatisfy` showsCommand ("hw sendtx --input " <> Text.pack (online </> "unsigned.json"))

    it "is to sync the account of the transaction sendtx sent" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        nosig <- paymentSigned home
        run <- hw home ["sendtx", nosig, "--yes"] []
        run `shouldSatisfy` showsCommand "hw sync --account main"

    it "is to see the transactions of the account synced" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw txs --account main"

    -- Another transaction of the mempool spends the coin of the payment
    it "is to find and delete the pending transaction a sync found in conflict" $
      withChain ctx (paidChain ctx) $ \home chain -> do
        _ <- paymentSigned home
        let other = spendingTx ctx [(txHash (payment ctx), 0)] [(otherAddr 1, 40000)]
        modifyIORef chain $ \c -> c {chainTxs = c.chainTxs <> [(other, Nothing)]}
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw pendingtxs --account main"
        run `shouldSatisfy` showsCommand "hw deletetx HASH"

    -- The key of the old wallet is in a file, and the sweep pays main
    it "is to sign the sweep preparesweep made with the keys of its file, or to export it to sign it where the keys are" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        run <- hw home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signsweep " <> nosig <> " --prvkeys " <> Text.pack keys)
        run `shouldSatisfy` showsCommand ("hw exporttx " <> nosig <> " FILE")

    -- Prepared from the address of the key, without it: signsweep asks
    -- for the key on the terminal
    it "is to sign the sweep preparesweep made from addresses, the keys typed when asked" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweepfrom", textOf wikiAddr, "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        nosig <- onePendingNosig home
        run `shouldSatisfy` showsCommand ("hw signsweep " <> nosig <> "\n")

    it "is to sign the file of the sweep where the keys are, when preparesweep wrote one" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweepfrom", textOf wikiAddr, "--sweepto", textOf (externalAddr 0), "--output", home </> "sweep.json", "--feerate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signsweep --input " <> Text.pack (home </> "sweep.json") <> "\n")

    -- In a wallet without accounts, the sweep is in its file only
    it "is to sign the file of a sweep outside the wallet with the keys of its file" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        run <- hw home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] []
        run `shouldSatisfy` showsCommand ("hw signsweep --input " <> Text.pack (home </> "sweep.json") <> " --prvkeys " <> Text.pack keys)

    it "is to send the sweep signsweep signed in the wallet, by its txid" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        createMain home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (externalAddr 0), "--feerate", "10"] []
        run <- hw home ["signsweep", nosig, "--prvkeys", keys] []
        txid <- txidOf =<< onePendingTx home
        run `shouldSatisfy` showsCommand ("hw sendtx " <> Text.pack txid)

    it "is to send the file of a sweep outside the wallet that signsweep signed" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home ["preparesweep", "--prvkeys", keys, "--sweepto", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--feerate", "10"] []
        run <- hw home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.json", "--prvkeys", keys] []
        run `shouldSatisfy` showsCommand ("hw sendtx --input " <> Text.pack (home </> "signed.json"))

    -- The name with a space would be two words for the shell
    it "quotes an account name the shell would split" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["renameacc", "main", "my main"] []
        run <- hw home ["sync"] []
        run `shouldSatisfy` showsCommand "hw txs --account 'my main'"

    it "is not shown with --json, whose output is the answer alone" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--json", "--feerate", "10"] []
        run `shouldNotSatisfy` showsCommand "hw signtx"
