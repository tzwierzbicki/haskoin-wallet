{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the tests of whole workflows share. Each spec of this directory
-- runs the hw binary of this build in a home of its own, with the mnemonic
-- typed on its input, against a fake store when it needs the network
-- ("Haskoin.Wallet.HwTestUtils"). The modules under the commands are
-- tested on their own; these tests put them together, as the user meets
-- them, from what hw prints and the code it exits with.
--
-- Here: the words the user types, the commands most workflows run, how an
-- answer of hw is read, and the payments and chains of the fixtures.
module Haskoin.Wallet.Workflow.TestUtils
  ( -- * What the user types
    seedOnce,
    seedTwice,
    partsOnce,
    -- * Running a command
    answer,
    createMain,
    synced,
    signerAndOnline,
    receiveFor,
    writeKeyFile,
    paymentSigned,
    sentPayment,
    onePendingTx,
    textOf,
    fixedTo,
    coinArg,
    -- * Reading an answer
    errorOf,
    alreadyExists,
    noSuchFile,
    highFeeRefused,
    askedMnemonic,
    printed,
    printedSentence,
    accountsOf,
    notSyncedOf,
    unspendableOf,
    accResultOf,
    keyOf,
    refusedOf,
    createdNotesOf,
    descriptorsOf,
    slip132KeysOf,
    accountOf,
    fingerprintsOf,
    addressesOf,
    coinsShown,
    coinLabelsShown,
    leftOf,
    txOf,
    txsOf,
    txRow,
    txidOf,
    feeOf,
    pendingState,
    nosigOf,
    paidToOthers,
    paidToWallet,
    spentFromWallet,
    addressLabelsOf,
    spendsAndPays,
    readTxFile,
    writeTxFile,
    psbtOnBitcoin,
    writeOtherWalletTxFile,
    -- * Payments and chains
    payment,
    paidChain,
    newerPayment,
    olderAndNewerChain,
    twoCoinsTx,
    twoAddressesTx,
    cashCoinTx,
    confirmedChain,
    wikiKeyWif,
    wikiAddr,
    oldCoin,
    oldKeyChain,
    version3Main,
  )
where

import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import qualified Database.Persist as P
import Haskoin
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..), LeftCoin (..), LeftReason (..))
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database (DBAccount (..), DBAddress (..))
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (AccountKind (..), MnemonicPass (..), foreignKindName)
import Haskoin.Wallet.Labels (AccountImport (..), ImportReport (..), importedAccount)
import qualified Haskoin.Wallet.Schema.V3 as V3
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncRes (..), foreignPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.PsbtFile (FileAccount (..), hwIdentifier, networkField, psbtTxFile, txFilePsbt)
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData)
import Numeric.Natural (Natural)
import System.FilePath ((</>))

{- What the user types -}

-- | The words and the passphrase of the test wallet, typed once each, as
-- the commands that check a seed ask for them
seedOnce :: [Text]
seedOnce = [mnemonicWords testMnemonic, mnemonicPass testMnemonic]

-- | The same, typed twice each, as createacc asks for them
seedTwice :: [Text]
seedTwice = [mnemonicWords testMnemonic, mnemonicWords testMnemonic, mnemonicPass testMnemonic, mnemonicPass testMnemonic]

-- | The words of the test wallet in their two parts, then its passphrase,
-- typed once each, as a command with --parts 2 asks for them
partsOnce :: [Text]
partsOnce = testMnemonicParts <> [mnemonicPass testMnemonic]

{- Running a command -}

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

-- | A home with the account main, its first receiving address handed out,
-- and synced with the chain of the store
synced :: FilePath -> IO ()
synced home = do
  createMain home
  _ <- answer home ["receive"] []
  void $ answer home ["sync"] []

-- | A signer holding the account main, and an online computer holding the
-- same account, imported from the signer, and synced with the chain
signerAndOnline :: FilePath -> FilePath -> IO ()
signerAndOnline signer online = do
  createMain signer
  _ <- answer signer ["exportacc", signer </> "main.json"] []
  _ <- answer online ["importacc", signer </> "main.json"] []
  _ <- answer online ["receive"] []
  void $ answer online ["sync"] []

-- | The address receive hands out to the account, as text
receiveFor :: FilePath -> String -> IO String
receiveFor home account = do
  shown <- addressesOf <$> answer home ["receive", "--account", account] []
  case shown of
    Right [(addr, _)] -> return (Text.unpack addr)
    other -> fail $ "Not an address: " <> show other

-- | Write the key to a file in a home, as the user keeps it, with a note
-- after it, and give its path
writeKeyFile :: FilePath -> IO FilePath
writeKeyFile home = do
  writeFile (home </> "keys.txt") (Text.unpack wikiKeyWif <> " old wallet\n")
  return (home </> "keys.txt")

-- | A home synced with the paid chain, with the payment of 20000 to the
-- other wallet prepared and signed: its nosigHash
paymentSigned :: FilePath -> IO String
paymentSigned home = do
  synced home
  nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
  _ <- answer home ["signtx", nosig] seedOnce
  return nosig

-- | A home synced with the paid chain, with the payment of 20000 to the
-- other wallet sent at 10 satoshis a byte: the id of the payment
sentPayment :: FilePath -> IO String
sentPayment home = do
  nosig <- paymentSigned home
  txidOf =<< txOf =<< answer home ["sendtx", nosig, "--yes"] []

-- | The one pending transaction of a home
onePendingTx :: FilePath -> IO TxInfo
onePendingTx home = do
  listed <- hwJson home ["pendingtxs"] []
  case listed of
    ResponsePendingTxs _ [(_, info)] -> return info
    other -> fail $ "Not one pending transaction: " <> show other

-- | The text of an address of the tests
textOf :: Address -> String
textOf = maybe (error "textOf") Text.unpack . addrToText btc

-- | An address and its amount, as preparetx and preparesendall take them:
-- ADDRESS:AMOUNT
fixedTo :: Address -> String -> String
fixedTo addr amount = textOf addr <> ":" <> amount

-- | A coin of a transaction, as --coin takes it
coinArg :: Tx -> Int -> String
coinArg tx i = Text.unpack (txHashToHex (txHash tx)) <> ":" <> show i

{- Reading an answer -}

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

-- | The refusal of a high fee rate the user does not take
highFeeRefused :: Text
highFeeRefused = "The transaction was not prepared: give a lower --fee-rate"

-- | Whether hw asked for a mnemonic
askedMnemonic :: Run -> Bool
askedMnemonic run = "Mnemonic" `Text.isInfixOf` runErr run

-- | Whether hw printed the text, on its output or its error output
printed :: Text -> Run -> Bool
printed text run = text `Text.isInfixOf` (runOut run <> runErr run)

-- | Whether hw printed the sentence, wrapped over lines or not
printedSentence :: Text -> Run -> Bool
printedSentence sentence run = sentence `Text.isInfixOf` Text.unwords (Text.lines (runOut run <> runErr run))

-- | The accounts of an answer: name, index and what each can spend now
accountsOf :: Response -> Either String [(Text, Int, Word64)]
accountsOf (ResponseAccount acc spendable) = Right [(dBAccountName acc, dBAccountIndex acc, spendable)]
accountsOf (ResponseAccounts accs) = Right [(dBAccountName acc, dBAccountIndex acc, spendable) | (acc, spendable) <- accs]
accountsOf (ResponseSync results _) = Right [(dBAccountName (syncResAccount s), dBAccountIndex (syncResAccount s), syncResSpendable s) | s <- results]
accountsOf (ResponseRecover _ _ results _) = accountsOf (ResponseSync results [])
accountsOf other = Left (show other)

-- | The funds recover found at paths hw cannot spend from: the kind and
-- the path of each account, how many of its addresses were used, and
-- what they hold in satoshis
unspendableOf :: Response -> Either String [(Text, String, Natural, Word64)]
unspendableOf (ResponseRecover net _ _ elsewhere) =
  Right [(foreignKindName (foreignKind funds), pathToStr (foreignPath net funds), foreignUsed funds, foreignBalance funds) | funds <- elsewhere]
unspendableOf other = Left (show other)

-- | Whether a command on an account did what it was asked, and what it
-- says of it
accResultOf :: Response -> Either String (Bool, Text)
accResultOf (ResponseAccResult _ result text) = Right (result, text)
accResultOf other = Left (show other)

-- | The accounts a sync left out, each by name with the failure of its
-- store
notSyncedOf :: Response -> Either String [(Text, Text)]
notSyncedOf (ResponseSync _ notSynced) = Right [(dBAccountName acc, failure) | (acc, failure) <- notSynced]
notSyncedOf other = Left (show other)

-- | The account of an answer, with its index and its public key: the one
-- account importacc created or found, for an import
keyOf :: Response -> Either String (Text, Int, Text)
keyOf (ResponseAccount acc _) = Right (dBAccountName acc, dBAccountIndex acc, dBAccountXPubKey acc)
keyOf (ResponseImport report) =
  case importedAccount <$> reportAccounts report of
    [Just acc] -> Right (dBAccountName acc, dBAccountIndex acc, dBAccountXPubKey acc)
    other -> Left (show other)
keyOf other = Left (show other)

-- | The accounts importacc refused, by the name the file gave them or the
-- start of their key, each with the reason
refusedOf :: Response -> Either String [(Text, Text)]
refusedOf (ResponseImport report) = Right [(name, why) | AccountRefused name why <- reportAccounts report]
refusedOf other = Left (show other)

-- | The accounts importacc created, each with the note on what the import
-- took on trust, if anything
createdNotesOf :: Response -> Either String [(Text, Maybe Text)]
createdNotesOf (ResponseImport report) = Right [(dBAccountName acc, note) | AccountCreated acc note <- reportAccounts report]
createdNotesOf other = Left (show other)

-- | The accounts of an answer with their descriptors, as descriptor
-- shows them
descriptorsOf :: Response -> Either String [(Text, Text)]
descriptorsOf (ResponseDescriptors held) = Right [(dBAccountName acc, descriptor) | (acc, descriptor, _) <- held]
descriptorsOf other = Left (show other)

-- | The accounts of an answer with their keys in the form of SLIP-132, as
-- descriptor shows them: a zpub for a bip84 account, none for a bip44 one
slip132KeysOf :: Response -> Either String [(Text, Maybe Text)]
slip132KeysOf (ResponseDescriptors held) = Right [(dBAccountName acc, key) | (acc, _, key) <- held]
slip132KeysOf other = Left (show other)

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
addressesOf (ResponseCheckAddr _ _ addr label) = Right [(addr, label)]
addressesOf other = Left (show other)

-- | The coins of an answer: outpoint, value, confirmations, lock, and
-- whether the builder may spend it now
coinsShown :: Response -> Either String [(OutPoint, Word64, Natural, Maybe CoinLock, Bool)]
coinsShown (ResponseCoins _ coins) = Right [(c.jsonCoinOutpoint, c.jsonCoinValue, c.jsonCoinConfirmations, c.jsonCoinLock, c.jsonCoinSpendable) | c <- coins]
coinsShown other = Left (show other)

-- | The coins of an answer with the label each shows, that of the
-- transaction that paid it
coinLabelsShown :: Response -> Either String [(OutPoint, Text)]
coinLabelsShown (ResponseCoins _ coins) = Right [(c.jsonCoinOutpoint, c.jsonCoinTxLabel) | c <- coins]
coinLabelsShown other = Left (show other)

-- | The coins preparetx or preparesendall leaves in the account, as its
-- answer lists them: outpoint, value and why
leftOf :: Response -> Either String [(OutPoint, Word64, LeftReason)]
leftOf (ResponsePrepared _ _ left) = Right [(c.leftCoinOutpoint, c.leftCoinValue, c.leftCoinReason) | c <- left]
leftOf other = Left (show other)

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

-- | The id of a signed transaction, as typed
txidOf :: TxInfo -> IO String
txidOf info = case txInfoStatus info of
  Pending status | Just tid <- pendingTxid status -> return $ Text.unpack (txHashToHex tid)
  OnChain status -> return $ Text.unpack (txHashToHex (chainTxid status))
  _ -> fail "The transaction is not signed"

-- | The fee of a transaction
feeOf :: TxInfo -> Natural
feeOf = summaryFee . txInfoSummary

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

-- | The labels of the wallet's addresses in a transaction, those it spends
-- from and those it pays, by address
addressLabelsOf :: TxInfo -> Map.Map Address Text
addressLabelsOf info = Map.union (myInputsLabel <$> summaryMyInputs summary) (myOutputsLabel <$> summaryMyOutputs summary)
  where
    summary = txInfoSummary info

-- | What a transaction spends and pays: its outpoints, and its outputs by
-- amount, with their addresses
spendsAndPays :: Ctx -> Tx -> ([OutPoint], [(Word64, Either String Address)])
spendsAndPays ctx tx = ((.outpoint) <$> tx.inputs, sortOn fst [(out.value, scriptToAddressBS ctx out.script) | out <- tx.outputs])

-- | The first account of the test wallet, and that of the second wallet,
-- as the derivation records of their files name their keys
mainFileAccount, otherFileAccount :: Ctx -> FileAccount
mainFileAccount ctx = FileAccount (testAccountPub ctx) Bip44 "m/44'/0'/0'" (Just testFingerprint) btc
otherFileAccount ctx = FileAccount (fromMaybe (error "otherFileAccount") (xPubImport btc ctx testAccountXPub2)) Bip44 "m/44'/0'/0'" (Just testFingerprint2) btc

-- | The transaction file at the path, as hw wrote it: a PSBT, read with
-- the paths of the first account of the test wallet, or of the second
-- wallet's
readTxFile :: Ctx -> FilePath -> IO TxFile
readTxFile ctx file = do
  bytes <- BS.readFile file
  either (\reason -> fail ("Not a transaction file: " <> file <> ": " <> reason)) return $
    psbtTxFile ctx [mainFileAccount ctx, otherFileAccount ctx] =<< decodePsbt bytes

-- | Write the signing data of a transaction at the path, as hw writes a
-- file: a PSBT on Bitcoin, with the keys of the first account of the test
-- wallet in its derivation records, and without labels
writeTxFile :: Ctx -> FilePath -> TxSignData -> IO ()
writeTxFile ctx file tsd = BS.writeFile file . encodePsbt =<< psbtOnBitcoin ctx tsd

-- | Write the signing data of a transaction at the path as a file of
-- another wallet names it: the PSBT of 'writeTxFile' without the field of
-- hw that names the network
writeOtherWalletTxFile :: Ctx -> FilePath -> TxSignData -> IO ()
writeOtherWalletTxFile ctx file tsd = do
  psbt <- psbtOnBitcoin ctx tsd
  let globals = Map.delete (proprietaryKey (Proprietary hwIdentifier networkField BS.empty)) psbt.globals
  BS.writeFile file (encodePsbt (Psbt psbt.version psbt.unsignedTx globals psbt.inputs psbt.outputs))

-- | The PSBT of the signing data on Bitcoin, with the keys of the first
-- account of the test wallet, as 'writeTxFile' writes it
psbtOnBitcoin :: Ctx -> TxSignData -> IO Psbt
psbtOnBitcoin ctx tsd = expectRight (txFilePsbt ctx (Just (mainFileAccount ctx)) (TxFile (Just btc) tsd Map.empty Map.empty))

{- Payments and chains -}

-- | A payment of 50000 satoshis from another wallet to the first receiving
-- address of the test wallet
payment :: Ctx -> Tx
payment ctx = fundingTx ctx 1 [(externalAddr 0, 50000)]

-- | A chain whose block 800000 holds the payment, two blocks below its best
paidChain :: Ctx -> Chain
paidChain ctx = Chain 800002 [(payment ctx, Just 800000)]

-- | A payment of 30000 satoshis from another wallet to the second
-- receiving address of the test wallet
newerPayment :: Ctx -> Tx
newerPayment ctx = fundingTx ctx 10 [(externalAddr 1, 30000)]

-- | The paid chain with the newer payment in its best block, 800002: the
-- coin of 50000 has 3 confirmations, and the coin of 30000 has 1
olderAndNewerChain :: Ctx -> Chain
olderAndNewerChain ctx = Chain 800002 [(payment ctx, Just 800000), (newerPayment ctx, Just 800002)]

-- | Coins of 50000 and 30000 satoshis at the first receiving address of the
-- test wallet
twoCoinsTx :: Ctx -> Tx
twoCoinsTx ctx = fundingTx ctx 5 [(externalAddr 0, 50000), (externalAddr 0, 30000)]

-- | Coins of 50000 satoshis at the first receiving address of the test
-- wallet, and 30000 at the second
twoAddressesTx :: Ctx -> Tx
twoAddressesTx ctx = fundingTx ctx 7 [(externalAddr 0, 50000), (externalAddr 1, 30000)]

-- | A coin of 50000 satoshis at the first receiving address of cash, the
-- Bitcoin Cash account of the test wallet
cashCoinTx :: Ctx -> Tx
cashCoinTx ctx = fundingTx ctx 8 [(bchExternalAddr, 50000)]

-- | A chain whose block 800000 holds the transaction, two blocks below its
-- best
confirmedChain :: Tx -> Chain
confirmedChain tx = Chain 800002 [(tx, Just 800000)]

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
