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
--
-- A home holds portfolios, made from the test mnemonic with newportfolio
-- --existing: portfolio0 of wallet0 first, then portfolio1. portfolio0,
-- the first created, is the active portfolio, the one the commands work
-- in, until a switch ('switchTo'). The fake store serves the same chain
-- on every network, and each sync covers the eight networks of every
-- portfolio. A payment to an address of the bip44 account of
-- portfolio0 on btc, as the chains below make, is seen on btc alone: the
-- other networks derive other keys. The answers are read from the JSON hw
-- writes with --json, each place named by its 'Place' or its
-- 'PortfolioRef', such as 'mainPlace', portfolio0 of wallet0 on btc.
module Haskoin.Wallet.Workflow.TestUtils
  ( -- * What the user types
    seedOnce,
    seedTwice,
    seedOnceOf,
    seedTwiceOf,
    sharesOnce,
    sharesForNew,
    sharesTestnetAddr,
    -- * Running a command
    answer,
    newTestPortfolio,
    newPortfolioOf,
    switchTo,
    activePortfolioIn,
    receiveWith,
    synced,
    syncedBothTypes,
    syncedTwoPortfolios,
    paymentOfSecondWallet,
    testnetCoinSynced,
    signerAndOnline,
    writeKeyFile,
    paymentSigned,
    sentPayment,
    PayWith (..),
    paidWith,
    onePendingTx,
    onePendingState,
    textOf,
    fixedTo,
    txidArg,
    coinArg,
    shortOf,
    shortCoinArg,
    -- * Reading an answer
    errorOf,
    alreadyExists,
    noSuchFile,
    highFeeRefused,
    highFeeQuestion,
    sendQuestion,
    askedMnemonic,
    shownBeforeMnemonic,
    printed,
    printedSentence,
    portfolio1Place,
    portfolioRefsOf,
    activePortfoliosOf,
    networksShownOf,
    spendableOn,
    fingerprintsOf,
    walletDescriptionsOf,
    descriptorsOf,
    slip132KeysOf,
    accountXPubsOf,
    checkedOf,
    deletedOf,
    syncedOf,
    notServedOf,
    notSyncedOf,
    notPublishedOf,
    recoveredOf,
    searchedOf,
    leftOutOf,
    unspendableOf,
    walletsImportedOf,
    portfoliosImportedOf,
    addedAccountsOf,
    leftAsideOf,
    labelsSetOf,
    placeOf,
    accountRefOf,
    addressesOf,
    addressGroupsOf,
    cutAddressOf,
    heldTxsOf,
    coinsShown,
    coinLabelsShown,
    coinNetworksOf,
    coinPathsOf,
    leftOf,
    txOf,
    txsOf,
    txRow,
    txBalancesOf,
    deadTxsOf,
    txidOf,
    feeOf,
    pendingState,
    createdOf,
    nosigOf,
    paidToOthers,
    paidToWallet,
    spentFromWallet,
    walletPathsOf,
    addressLabelsOf,
    spendsAndPays,
    readTxFile,
    writeTxFile,
    psbtOnBitcoin,
    writeOtherWalletTxFile,
    -- * Payments and chains
    payment,
    paymentSent,
    paidChain,
    feesChain,
    chainPaying,
    shortIdClashChain,
    newerPayment,
    olderAndNewerChain,
    twoCoinsTx,
    twoAddressesTx,
    bothTypesTx,
    twoPortfoliosTx,
    cashCoinTx,
    cashChangeAddr,
    testnetCoinTx,
    confirmedChain,
    wikiKeyWif,
    wikiAddr,
    oldCoin,
    oldKeyChain,
  )
where

import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (Group (..), Response (..), groupAccountRef)
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Account (AccountKeys (..), NetworkView (..), PortfolioView (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..), LeftCoin (..), LeftReason (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), Place (..), PortfolioRef (..), accountRefNetwork)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (AccountKind (..), MnemonicPass (..), PoolPath, foreignKindName)
import Haskoin.Wallet.Labels (AccountImport (..), ImportReport (..), PortfolioImport (..), WalletImport (..))
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncRes (..), foreignPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.PsbtFile (FileAccount (..), hwIdentifier, networkField, psbtTxFile, txFilePsbt)
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData)
import Numeric.Natural (Natural)
import System.FilePath ((</>))

{- What the user types -}

-- | The words and the passphrase of a mnemonic, typed once each, as the
-- commands that check a seed ask for them: signtx, checkmnemonic,
-- recover
seedOnceOf :: MnemonicPass -> [Text]
seedOnceOf mnem = [mnemonicWords mnem, mnemonicPass mnem]

-- | The same, typed twice each, as newportfolio --existing asks for them
seedTwiceOf :: MnemonicPass -> [Text]
seedTwiceOf mnem = [mnemonicWords mnem, mnemonicWords mnem, mnemonicPass mnem, mnemonicPass mnem]

-- | The words and the passphrase of the test wallet, typed once each
seedOnce :: [Text]
seedOnce = seedOnceOf testMnemonic

-- | The same, typed twice each, as newportfolio --existing asks for them
seedTwice :: [Text]
seedTwice = seedTwiceOf testMnemonic

-- | The shares of the backup of the device tests, then its passphrase,
-- typed once each, as the commands that check a seed ask for them
sharesOnce :: [Text]
sharesOnce = testShares <> [testSharesPassphrase]

-- | The shares typed once and the passphrase twice, as newportfolio
-- --existing asks for them: the checksum of a share catches a typo, so a
-- share is not typed again
sharesForNew :: [Text]
sharesForNew = testShares <> [testSharesPassphrase, testSharesPassphrase]

-- | The first receiving address of the bip44 account of portfolio0 of
-- that backup on testnet
sharesTestnetAddr :: Address
sharesTestnetAddr = fromMaybe (error "sharesTestnetAddr") (textToAddr btcTest =<< listToMaybe testSharesTestnetAddrs)

{- Running a command -}

-- | The answer of hw with --json, which must not be an error
answer :: FilePath -> [String] -> [Text] -> IO Response
answer home args typed = do
  response <- hwJson home args typed
  case response of
    ResponseError err -> fail $ "hw " <> unwords args <> " failed: " <> Text.unpack err
    _ -> return response

-- | Add the next portfolio of a mnemonic to a home, as newportfolio
-- --existing does: portfolio0 of a new wallet when the home has no wallet
-- of the mnemonic, then portfolio1, and so on. The second test mnemonic
-- makes wallet1 in a home that has the wallet of the first.
newPortfolioOf :: FilePath -> MnemonicPass -> IO ()
newPortfolioOf home mnem = void $ answer home ["newportfolio", "--existing"] (seedTwiceOf mnem)

-- | Add the next portfolio of the test mnemonic to a home: portfolio0 of
-- wallet0 in a new home, whole, with its twelve accounts
newTestPortfolio :: FilePath -> IO ()
newTestPortfolio home = newPortfolioOf home testMnemonic

-- | The portfolio portfolios marks as the active one in a home, the one
-- the commands work in: none in a database without a portfolio
activePortfolioIn :: FilePath -> IO (Either String [PortfolioRef])
activePortfolioIn home = activePortfoliosOf <$> hwJson home ["portfolios"] []

-- | Make the portfolio of the indexes given, of a wallet and of one of its
-- portfolios, the active one of a home, as switch PORTFOLIO --wallet
-- does: the commands that run next work in it
switchTo :: FilePath -> Natural -> Natural -> IO ()
switchTo home wallet portfolio = void $ answer home ["switch", show portfolio, "--wallet", show wallet] []

-- | The address receive hands out with the options given, as text: the
-- first receiving address of the bip44 account of portfolio0 on btc
-- without options in a home of the test wallet, and with --network or
-- --type one of another account of the active portfolio
receiveWith :: FilePath -> [String] -> IO String
receiveWith home options = do
  shown <- addressesOf <$> answer home ("receive" : options) []
  case shown of
    Right [(addr, _)] -> return (Text.unpack addr)
    other -> fail $ "Not an address: " <> show other

-- | A home with portfolio0 of the test wallet, the first receiving address
-- of its bip44 account on btc handed out, 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,
-- and synced with the chain of the store
synced :: FilePath -> IO ()
synced home = do
  newTestPortfolio home
  _ <- answer home ["receive"] []
  void $ answer home ["sync"] []

-- | A home with portfolio0 of the test wallet, the first receiving address
-- of each of its accounts on btc handed out, the bip44 one and the bip84
-- one, bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc, and synced with the
-- chain of the store
syncedBothTypes :: FilePath -> IO ()
syncedBothTypes home = do
  newTestPortfolio home
  _ <- answer home ["receive"] []
  _ <- answer home ["receive", "--type", "bip84"] []
  void $ answer home ["sync"] []

-- | A home with portfolio0 and portfolio1 of the test wallet, the first
-- receiving address of the bip44 account of each on btc handed out,
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj and 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo,
-- and synced with the chain of the store. portfolio0, the first one
-- created, is the active portfolio afterwards, as it was before: receive
-- gives the address of portfolio1 after a switch to it, and a switch back.
syncedTwoPortfolios :: FilePath -> IO ()
syncedTwoPortfolios home = do
  newTestPortfolio home
  newTestPortfolio home
  _ <- answer home ["receive"] []
  switchTo home 0 1
  _ <- answer home ["receive"] []
  switchTo home 0 0
  void $ answer home ["sync"] []

-- | A home with the test wallet, wallet0, and the wallet of the second
-- test mnemonic, wallet1, whose first receiving address 'chainPaying'
-- pays. portfolio0 of wallet1 is active, synced, and has a payment
-- prepared.
paymentOfSecondWallet :: FilePath -> IO ()
paymentOfSecondWallet home = do
  newTestPortfolio home
  newPortfolioOf home testMnemonic2
  switchTo home 1 0
  _ <- answer home ["receive"] []
  _ <- answer home ["sync"] []
  void $ answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []

-- | A home with portfolio0 of the test wallet, its first receiving address
-- on the test networks handed out on btctest and on btctest4, which share
-- it, mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih, and synced. With a host that
-- serves those two networks alone, as 'withChainServing' gives one, the
-- coin of 'testnetCoinTx' is on two networks, rather than on the six that
-- share the key.
testnetCoinSynced :: FilePath -> IO ()
testnetCoinSynced home = do
  newTestPortfolio home
  _ <- answer home ["receive", "--network", "btctest"] []
  _ <- answer home ["receive", "--network", "btctest4"] []
  void $ answer home ["sync"] []

-- | A signer holding portfolio0 of the test wallet, and an online computer
-- holding the same portfolio, imported from the file exportwallet wrote on
-- the signer, with its first receiving address handed out, and synced with
-- the chain
signerAndOnline :: FilePath -> FilePath -> IO ()
signerAndOnline signer online = do
  newTestPortfolio signer
  _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
  _ <- answer online ["importwallet", signer </> "wallet.jsonl"] []
  _ <- answer online ["receive"] []
  void $ answer online ["sync"] []

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
  txidOf =<< txOf =<< answer home ["broadcast", nosig, "--yes"] []

-- | The two ways to make a payment from the same options: send, which
-- takes the three steps in one command, or preparetx, signtx and
-- broadcast, one after the other. Both must make the same payment.
data PayWith = WithSend | WithThreeCommands
  deriving (Eq, Show)

-- | Make a payment in a home, one way or the other, with the options of
-- preparetx given, signed with the test mnemonic: the transaction as the
-- command that sent it answers with it. The lines are what the user types
-- for the questions of the options, such as the fee rate, before the
-- mnemonic. broadcast takes --yes: send asks no question of its own
-- either.
paidWith :: PayWith -> FilePath -> [String] -> [Text] -> IO TxInfo
paidWith WithSend home options typed = txOf =<< answer home ("send" : options) (typed <> seedOnce)
paidWith WithThreeCommands home options typed = do
  nosig <- nosigOf =<< txOf =<< answer home ("preparetx" : options) typed
  txid <- txidOf =<< txOf =<< answer home ["signtx", nosig] seedOnce
  txOf =<< answer home ["broadcast", txid, "--yes"] []

-- | The one pending transaction of a home, in its active portfolio
onePendingTx :: FilePath -> IO TxInfo
onePendingTx home = do
  listed <- hwJson home ["pendingtxs"] []
  case listed of
    ResponsePendingTxs _ [(_, info)] -> return info
    other -> fail $ "Not one pending transaction: " <> show other

-- | Whether the one pending transaction of a home is signed, and online
onePendingState :: FilePath -> IO (Maybe (Bool, Bool))
onePendingState home = pendingState <$> onePendingTx home

-- | The text of an address of the tests on Bitcoin
textOf :: Address -> String
textOf = maybe (error "textOf") Text.unpack . addrToText btc

-- | An address and its amount, as preparetx and preparesendall take them:
-- ADDRESS:AMOUNT
fixedTo :: Address -> String -> String
fixedTo addr amount = textOf addr <> ":" <> amount

-- | A transaction by its txid in full, as typed
txidArg :: Tx -> String
txidArg tx = Text.unpack (txHashToHex (txHash tx))

-- | A coin of a transaction, as --coin takes it
coinArg :: Tx -> Int -> String
coinArg tx i = txidArg tx <> ":" <> show i

-- | The short id of a hash as typed: its first 8 digits, as the listings
-- show it
shortOf :: String -> String
shortOf = take 8

-- | A coin of a transaction by the short id of its txid, as coins shows it
-- and --coin takes it
shortCoinArg :: Tx -> Int -> String
shortCoinArg tx i = shortOf (coinArg tx i) <> ":" <> show i

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

-- | The question of a fee rate of 101 satoshis per virtual byte, one above
-- the highest rate taken without asking
highFeeQuestion :: Text
highFeeQuestion = "The fee rate of 101 satoshis per virtual byte is above 100, the most hw takes without asking. Prepare the transaction anyway? [y/N] "

-- | The question of broadcast
sendQuestion :: Text
sendQuestion = "Send this transaction to the network? This cannot be undone. [y/N] "

-- | Whether hw asked for a mnemonic
askedMnemonic :: Run -> Bool
askedMnemonic run = "Mnemonic" `Text.isInfixOf` runErr run

-- | What hw wrote on its error output before it asked for a mnemonic:
-- what the user reads before typing it
shownBeforeMnemonic :: Run -> Text
shownBeforeMnemonic run = fst (Text.breakOn "Mnemonic" (runErr run))

-- | Whether hw printed the text, on its output or its error output
printed :: Text -> Run -> Bool
printed text run = text `Text.isInfixOf` (runOut run <> runErr run)

-- | Whether hw printed the sentence, wrapped over lines or not
printedSentence :: Text -> Run -> Bool
printedSentence sentence run = sentence `Text.isInfixOf` Text.unwords (Text.lines (runOut run <> runErr run))

-- | Where portfolio1 of wallet0 is on btc, as the answers name it:
-- 'mainPlace' is portfolio0 there
portfolio1Place :: Place
portfolio1Place = Place (PortfolioRef 0 1) btc

-- | The portfolios an answer shows, as portfolios lists them, or as
-- newportfolio leaves them, or switch makes active: each by its wallet
-- and its index
portfolioRefsOf :: Response -> Either String [PortfolioRef]
portfolioRefsOf response = fmap viewRef <$> portfolioViewsOf response

-- | The views of the portfolios of an answer
portfolioViewsOf :: Response -> Either String [PortfolioView]
portfolioViewsOf = \case
  ResponsePortfolios views -> Right views
  ResponsePortfolio view -> Right [view]
  ResponseSwitched view -> Right [view]
  ResponseNewPortfolio _ view _ -> Right [view]
  other -> Left (show other)

-- | The portfolios an answer marks as the active one, as portfolios marks
-- it in its JSON: one at most
activePortfoliosOf :: Response -> Either String [PortfolioRef]
activePortfoliosOf response = fmap viewRef . filter viewActive <$> portfolioViewsOf response

-- | The networks an answer shows of each portfolio, by their names, in
-- the order it shows them: the lines of portfolios
networksShownOf :: Response -> Either String [(PortfolioRef, [String])]
networksShownOf response = do
  views <- portfolioViewsOf response
  return [(viewRef v, [(networkViewNetwork n).name | n <- viewNetworks v]) | v <- views]

-- | What each portfolio of an answer can spend now on the network: from
-- the figures portfolios shows, or from what sync and recover synced. A
-- portfolio without an account on the network is left out.
spendableOn :: Network -> Response -> Either String [(PortfolioRef, Word64)]
spendableOn net = \case
  ResponseSync results _ _ _ -> Right (fromSyncs results)
  ResponseRecover _ _ _ _ results _ _ _ _ -> Right (fromSyncs results)
  other -> do
    views <- portfolioViewsOf other
    return [(viewRef v, networkViewSpendable n) | v <- views, n <- viewNetworks v, networkViewNetwork n == net]
  where
    fromSyncs results = [(placePortfolio (syncResPlace s), syncResSpendable s) | s <- results, placeNetwork (syncResPlace s) == net]

-- | The portfolios of an answer, with the fingerprint of the seed of their
-- wallet, which every wallet has
fingerprintsOf :: Response -> Either String [(PortfolioRef, Text)]
fingerprintsOf response = do
  views <- portfolioViewsOf response
  return [(viewRef v, fingerprintToText (viewFingerprint v)) | v <- views]

-- | The portfolios of an answer, with the description of their wallet,
-- empty for a wallet the user did not describe
walletDescriptionsOf :: Response -> Either String [(PortfolioRef, Text)]
walletDescriptionsOf response = do
  views <- portfolioViewsOf response
  return [(viewRef v, viewWalletDescription v) | v <- views]

-- | The accounts of the portfolios of an answer, each by its portfolio,
-- its network and its type, with its output descriptor, as portfolios
-- --json gives it
descriptorsOf :: Response -> Either String [(PortfolioRef, String, AccountKind, Text)]
descriptorsOf response = fmap (\(ref, net, keys) -> (ref, net, keysKind keys, keysDescriptor keys)) <$> accountKeysOf response

-- | The same accounts, each with its key in the form of SLIP-132: its xpub
-- or tpub for a bip44 account, a zpub or a vpub for a bip84 one
slip132KeysOf :: Response -> Either String [(PortfolioRef, String, AccountKind, Maybe Text)]
slip132KeysOf response = fmap (\(ref, net, keys) -> (ref, net, keysKind keys, keysSlip132 keys)) <$> accountKeysOf response

-- | The same accounts, each with its extended public key as the database
-- holds it: an xpub, or a tpub on a test network, whatever its type
accountXPubsOf :: Response -> Either String [(PortfolioRef, String, AccountKind, Text)]
accountXPubsOf response = fmap (\(ref, net, keys) -> (ref, net, keysKind keys, keysXPubKey keys)) <$> accountKeysOf response

-- | The keys of each account of the portfolios of an answer, with the
-- portfolio and the network of the account
accountKeysOf :: Response -> Either String [(PortfolioRef, String, AccountKeys)]
accountKeysOf response = do
  views <- portfolioViewsOf response
  return [(viewRef v, (networkViewNetwork n).name, keys) | v <- views, n <- viewNetworks v, keys <- networkViewAccounts n]

-- | Whether checkmnemonic found the mnemonic to match the portfolio, and
-- what it says of it
checkedOf :: Response -> Either String (Bool, Text)
checkedOf (ResponseChecked _ result text) = Right (result, text)
checkedOf other = Left (show other)

-- | What deleteportfolio or deletewallet says it deleted
deletedOf :: Response -> Either String Text
deletedOf (ResponseDeleted text) = Right text
deletedOf other = Left (show other)

-- | The pools sync or recover synced, each by its portfolio and its
-- network, with what it can spend now
syncedOf :: Response -> Either String [(Place, Word64)]
syncedOf = \case
  ResponseSync results _ _ _ -> Right (fromSyncs results)
  ResponseRecover _ _ _ _ results _ _ _ _ -> Right (fromSyncs results)
  other -> Left (show other)
  where
    fromSyncs results = [(syncResPlace s, syncResSpendable s) | s <- results]

-- | The networks the host does not serve, as sync and recover name them
notServedOf :: Response -> Either String [String]
notServedOf = \case
  ResponseSync _ unserved _ _ -> Right ((.name) <$> unserved)
  ResponseRecover _ _ _ _ _ _ unserved _ _ -> Right ((.name) <$> unserved)
  other -> Left (show other)

-- | The networks a sync left out, each with the failure of its store
notSyncedOf :: Response -> Either String [(String, Text)]
notSyncedOf (ResponseSync _ _ notSynced _) = Right [(net.name, failure) | (net, failure) <- notSynced]
notSyncedOf other = Left (show other)

-- | The transactions a sync handed to the store again, and that the store
-- did not publish: each by its txid, as typed, with what the store said
notPublishedOf :: Response -> Either String [(String, String)]
notPublishedOf (ResponseSync results _ _ _) =
  Right [(Text.unpack (txHashToHex txid), why) | s <- results, (txid, why) <- syncResNotPublished s]
notPublishedOf other = Left (show other)

-- | What recover found of a seed: the index of its wallet, if it has one
-- now, and the indexes of the portfolios it created
recoveredOf :: Response -> Either String (Maybe Natural, [Natural])
recoveredOf (ResponseRecover _ _ walletM created _ _ _ _ _) = Right (walletM, created)
recoveredOf other = Left (show other)

-- | The networks recover names as searched, by their names
searchedOf :: Response -> Either String [String]
searchedOf (ResponseRecover searched _ _ _ _ _ _ _ _) = Right ((.name) <$> searched)
searchedOf other = Left (show other)

-- | The networks recover left out, each with the failure of its store, as
-- it prints them after "left out"
leftOutOf :: Response -> Either String [(String, Text)]
leftOutOf (ResponseRecover _ _ _ _ _ _ _ leftOut _) = Right [(net.name, failure) | (net, failure) <- leftOut]
leftOutOf other = Left (show other)

-- | The funds recover found at paths hw cannot spend from: the network,
-- the kind and the path of each account, how many of its addresses were
-- used, and what they hold in satoshis
unspendableOf :: Response -> Either String [(String, Text, String, Natural, Word64)]
unspendableOf (ResponseRecover _ _ _ _ _ elsewhere _ _ _) =
  Right [(net.name, foreignKindName (foreignKind funds), pathToStr (foreignPath net funds), foreignUsed funds, foreignBalance funds) | (net, funds) <- elsewhere]
unspendableOf other = Left (show other)

-- | The wallets importwallet answers for, each by its index, with whether
-- it created the wallet
walletsImportedOf :: Response -> Either String [(Natural, Bool)]
walletsImportedOf (ResponseImport report _) =
  Right [(walletImportIndex w, walletImportCreated w) | w <- reportWallets report]
walletsImportedOf other = Left (show other)

-- | The portfolios importwallet answers for, each by the index of its
-- wallet and its own, with whether it created the portfolio, whole, or
-- found it in the database
portfoliosImportedOf :: Response -> Either String [(Natural, Natural, Bool)]
portfoliosImportedOf (ResponseImport report _) =
  Right
    [ (walletImportIndex w, portfolioImportIndex p, portfolioImportCreated p)
      | w <- reportWallets report,
        p <- walletImportPortfolios w
    ]
portfoliosImportedOf other = Left (show other)

-- | The accounts importwallet added to the database, with the portfolios
-- it created
addedAccountsOf :: Response -> Either String [AccountRef]
addedAccountsOf (ResponseImport report _) = Right [ref | AccountAdded ref <- reportAccounts report]
addedAccountsOf other = Left (show other)

-- | The key records importwallet left aside, by the start of their key,
-- each with the reason
leftAsideOf :: Response -> Either String [(Text, Text)]
leftAsideOf (ResponseImport report _) = Right [(key, why) | AccountLeftAside key why <- reportAccounts report]
leftAsideOf other = Left (show other)

-- | How many labels importwallet set: on addresses, and on transactions,
-- each portfolio counting once
labelsSetOf :: Response -> Either String (Natural, Natural)
labelsSetOf (ResponseImport report _) = Right (reportAddressLabels report, reportTxLabels report)
labelsSetOf other = Left (show other)

-- | Where the address, the transaction or the payment of an answer is: its
-- portfolio and its network
placeOf :: Response -> Either String Place
placeOf = \case
  ResponseAddress ref _ _ _ -> Right (accountRefPlace ref)
  ResponseDerivedAddr ref _ _ -> Right (accountRefPlace ref)
  ResponseTx place _ _ -> Right place
  ResponsePrepared place _ _ _ -> Right place
  other -> Left (show other)

-- | The account of the address an answer shows: its portfolio, its
-- network and its type
accountRefOf :: Response -> Either String AccountRef
accountRefOf = \case
  ResponseAddress ref _ _ _ -> Right ref
  ResponseDerivedAddr ref _ _ -> Right ref
  other -> Left (show other)

-- | The addresses of an answer, with their labels, the sections of a
-- listing one after the other
addressesOf :: Response -> Either String [(Text, Text)]
addressesOf (ResponseAddress _ addr _ _) = Right [(dBAddressAddress addr, dBAddressLabel addr)]
addressesOf (ResponseAddresses groups) = Right [(dBAddressAddress addr, dBAddressLabel addr) | g <- groups, (addr, _) <- groupRows g]
addressesOf other = Left (show other)

-- | The addresses of a listing section by section: each section by its
-- account, with its addresses and their labels
addressGroupsOf :: Response -> Either String [(AccountRef, [(Text, Text)])]
addressGroupsOf (ResponseAddresses groups) =
  traverse (\g -> maybe (Left "A group of addresses of no one account") (\ref -> Right (ref, [(dBAddressAddress addr, dBAddressLabel addr) | (addr, _) <- groupRows g])) (groupAccountRef g)) groups
addressGroupsOf other = Left (show other)

-- | The address that show derives at a path the wallet does not hold, cut,
-- with its middle hidden: no address, so not one of 'addressesOf'
cutAddressOf :: Response -> Either String Text
cutAddressOf (ResponseDerivedAddr _ _ cut) = Right cut
cutAddressOf other = Left (show other)

-- | A transaction as show answers with it, as each portfolio that holds it
-- sees it: where, and what the transaction adds to the portfolio there,
-- or takes from it when negative
heldTxsOf :: Response -> Either String [(Place, Integer)]
heldTxsOf (ResponseHeldTxs held _) = Right [(place, summaryAmount (txInfoSummary info)) | (place, info) <- held]
heldTxsOf other = Left (show other)

-- | The coins of an answer, the sections of a listing one after the
-- other: outpoint, value, confirmations, lock, and whether the builder may
-- spend it now
coinsShown :: Response -> Either String [(OutPoint, Word64, Natural, Maybe CoinLock, Bool)]
coinsShown response = fmap (\c -> (c.jsonCoinOutpoint, c.jsonCoinValue, c.jsonCoinConfirmations, c.jsonCoinLock, c.jsonCoinSpendable)) <$> coinRowsOf response

-- | The coins of an answer with the label each shows, that of the
-- transaction that paid it
coinLabelsShown :: Response -> Either String [(OutPoint, Text)]
coinLabelsShown response = fmap (\c -> (c.jsonCoinOutpoint, c.jsonCoinTxLabel)) <$> coinRowsOf response

-- | The coins of an answer, each with the network of its section
coinNetworksOf :: Response -> Either String [(String, OutPoint)]
coinNetworksOf (ResponseCoins groups) = Right [((placeNetwork (groupPlace g)).name, c.jsonCoinOutpoint) | g <- groups, c <- groupRows g]
coinNetworksOf (ResponseCoin ref c _) = Right [((accountRefNetwork ref).name, c.jsonCoinOutpoint)]
coinNetworksOf other = Left (show other)

-- | The coins of an answer, each with the path of its address in the
-- portfolio, which names the type of its account
coinPathsOf :: Response -> Either String [(OutPoint, Maybe PoolPath)]
coinPathsOf response = fmap (\c -> (c.jsonCoinOutpoint, c.jsonCoinPath)) <$> coinRowsOf response

-- | The rows of an answer of coins, every section together
coinRowsOf :: Response -> Either String [JsonCoin]
coinRowsOf (ResponseCoins groups) = Right (concatMap groupRows groups)
coinRowsOf (ResponseCoin _ coin _) = Right [coin]
coinRowsOf other = Left (show other)

-- | The coins preparetx or preparesendall leaves in the portfolio, as its
-- answer lists them: outpoint, value and why
leftOf :: Response -> Either String [(OutPoint, Word64, LeftReason)]
leftOf (ResponsePrepared _ _ left _) = Right [(jsonCoinOutpoint c.leftCoin, jsonCoinValue c.leftCoin, c.leftCoinReason) | c <- left]
leftOf other = Left (show other)

-- | The transaction of an answer, of a portfolio or outside the wallet
txOf :: Response -> IO TxInfo
txOf (ResponseTx _ info _) = return info
txOf (ResponsePrepared _ info _ _) = return info
txOf (ResponseOutsideTx _ info) = return info
txOf other = fail $ "Not a transaction: " <> show other

-- | The transactions of an answer, the sections of a listing one after the
-- other: type, amount, and confirmations once on the chain
txsOf :: Response -> Either String [(TxType, Integer, Maybe Natural)]
txsOf (ResponseTxs groups) = Right [txRow (listedTxInfo listed) | g <- groups, listed <- groupRows g]
txsOf (ResponsePendingTxs _ held) = Right (txRow . snd <$> held)
txsOf other = Left (show other)

-- | A transaction as 'txsOf' gives it
txRow :: TxInfo -> (TxType, Integer, Maybe Natural)
txRow i = (summaryType (txInfoSummary i), summaryAmount (txInfoSummary i), confirmations (txInfoStatus i))
  where
    confirmations (OnChain status) = Just (chainConfirmations status)
    confirmations (Pending _) = Nothing

-- | The balance of its portfolio on its network after each transaction
-- txs lists
txBalancesOf :: Response -> Either String [Maybe Integer]
txBalancesOf (ResponseTxs groups) = Right [listedTxBalance listed | g <- groups, listed <- groupRows g]
txBalancesOf other = Left (show other)

-- | The transactions of a listing that are dead, replaced or dropped from
-- the network, each by its txid, the sections of the listing one after
-- the other
deadTxsOf :: Response -> Either String [TxHash]
deadTxsOf (ResponseTxs groups) =
  Right [chainTxid status | g <- groups, listed <- groupRows g, OnChain status <- [txInfoStatus (listedTxInfo listed)], chainDead status]
deadTxsOf other = Left (show other)

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

-- | When the wallet started holding a transaction in preparation, or none
-- for one it does not hold
createdOf :: TxInfo -> Maybe UTCTime
createdOf info = case txInfoStatus info of
  Pending status -> pendingCreated status
  OnChain _ -> Nothing

-- | The nosigHash of a transaction in preparation, as typed
nosigOf :: TxInfo -> IO String
nosigOf info = case txInfoStatus info of
  Pending status -> return $ Text.unpack (txHashToHex (pendingNosigHash status))
  OnChain _ -> fail "The transaction is not in preparation"

-- | What a transaction pays to the outputs of other wallets
paidToOthers :: TxInfo -> Map.Map Address Natural
paidToOthers = summaryOtherOutputs . txInfoSummary

-- | What a transaction pays to the outputs of the portfolio
paidToWallet :: TxInfo -> Map.Map Address Natural
paidToWallet = fmap myOutputsValue . summaryMyOutputs . txInfoSummary

-- | What a transaction spends from the portfolio, by address
spentFromWallet :: TxInfo -> Map.Map Address Natural
spentFromWallet = fmap myInputsValue . summaryMyInputs . txInfoSummary

-- | The paths of the portfolio's addresses in a transaction, those it
-- spends from and those it pays, each with the type of its account
walletPathsOf :: TxInfo -> Map.Map Address PoolPath
walletPathsOf info = Map.union (myInputsPath <$> summaryMyInputs summary) (myOutputsPath <$> summaryMyOutputs summary)
  where
    summary = txInfoSummary info

-- | The labels of the portfolio's addresses in a transaction, those it
-- spends from and those it pays, by address
addressLabelsOf :: TxInfo -> Map.Map Address Text
addressLabelsOf info = Map.union (myInputsLabel <$> summaryMyInputs summary) (myOutputsLabel <$> summaryMyOutputs summary)
  where
    summary = txInfoSummary info

-- | What a transaction spends and pays: its outpoints, and its outputs by
-- amount, with their addresses
spendsAndPays :: Ctx -> Tx -> ([OutPoint], [(Word64, Either String Address)])
spendsAndPays ctx tx = ((.outpoint) <$> tx.inputs, sortOn fst [(out.value, scriptToAddressBS btc ctx out.script) | out <- tx.outputs])

-- | The two accounts of portfolio0 of the test wallet on Bitcoin, and the
-- first account of the second wallet, as the derivation records of their
-- files name their keys
mainFileAccount, segwitFileAccount, otherFileAccount :: Ctx -> FileAccount
mainFileAccount ctx = FileAccount (testAccountPub ctx) Bip44 "m/44'/0'/0'" testFingerprint btc
segwitFileAccount ctx = FileAccount (testSegwitAccountPub ctx) Bip84 "m/84'/0'/0'" testFingerprint btc
otherFileAccount ctx = FileAccount (fromMaybe (error "otherFileAccount") (xPubImport btc ctx testAccountXPub2)) Bip44 "m/44'/0'/0'" testFingerprint2 btc

-- | The transaction file at the path, as hw wrote it: a PSBT, read with
-- the paths of portfolio0 of the test wallet on Bitcoin, both of its
-- accounts, or of the second wallet's
readTxFile :: Ctx -> FilePath -> IO TxFile
readTxFile ctx file = do
  bytes <- BS.readFile file
  either (\reason -> fail ("Not a transaction file: " <> file <> ": " <> reason)) return $
    psbtTxFile ctx Nothing [[mainFileAccount ctx, segwitFileAccount ctx], [otherFileAccount ctx]] =<< decodePsbt bytes

-- | Write the signing data of a transaction at the path, as hw writes a
-- file: a PSBT on Bitcoin, with the keys of the two accounts of
-- portfolio0 of the test wallet in its derivation records, and without
-- labels
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

-- | The PSBT of the signing data on Bitcoin, with the keys of the two
-- accounts of portfolio0 of the test wallet, as 'writeTxFile' writes it
psbtOnBitcoin :: Ctx -> TxSignData -> IO Psbt
psbtOnBitcoin ctx tsd = expectRight (txFilePsbt ctx [mainFileAccount ctx, segwitFileAccount ctx] (TxFile (Just btc) tsd Map.empty Map.empty))

{- Payments and chains -}

-- | A payment of 50000 satoshis from another wallet to the first receiving
-- address of the bip44 account of portfolio0 of the test wallet on btc
payment :: Ctx -> Tx
payment ctx = fundingTx ctx 1 [(externalAddr 0, 50000)]

-- | What the payment of 20000 satoshis to the other wallet spends and
-- pays, as the store sees it once sent at 10 satoshis a byte: the coin of
-- the payment to the wallet, and 27740 of change. The transaction of one
-- input and two outputs is 226 bytes, so the fee is 2260, and the change
-- 50000 - 20000 - 2260 = 27740.
paymentSent :: Ctx -> ([OutPoint], [(Word64, Either String Address)])
paymentSent ctx = ([OutPoint (txHash (payment ctx)) 0], [(20000, Right (otherAddr 0)), (27740, Right (internalAddr 0))])

-- | A chain whose block 800000 holds the payment, two blocks below its best
paidChain :: Ctx -> Chain
paidChain ctx = Chain 800002 [(payment ctx, Just 800000)]

-- | A chain whose latest two blocks hold three transactions: one paying
-- the wallet, of 85 bytes and a fee of 1000 satoshis (11.76 per byte);
-- one of two outputs, of 119 bytes and 1000 satoshis (8.40 per byte); and
-- one spending the latter, of 85 bytes and 10000 satoshis (117.65 per
-- byte). The sizes follow from the serialization: 4 bytes of version, 41
-- per input, 34 per P2PKH output, 4 of lock time, and a count byte each
-- for the inputs and the outputs. Sorted by rate, the 289 bytes reach a
-- quarter within the 119 at 8.40, half within the next 85 at 11.76, and
-- nine tenths within the last 85 at 117.65: rounded up, 9, 12 and 118.
-- The transaction that pays the wallet is 'payment'.
feesChain :: Ctx -> Chain
feesChain ctx =
  Chain
    800001
    [ (payment ctx, Just 800000),
      (twoOutputs, Just 800001),
      (spendingTx ctx [(txHash twoOutputs, 0)] [(otherAddr 2, 50000)], Just 800001)
    ]
  where
    twoOutputs = fundingTx ctx 2 [(otherAddr 1, 60000), (otherAddr 1, 70000)]

-- | A chain whose block 800000 pays 50000 to the address
chainPaying :: Ctx -> Address -> Chain
chainPaying ctx addr = Chain 800002 [(fundingTx ctx 1 [(addr, 50000)], Just 800000)]

-- | A chain whose block 800000 holds two payments whose txids share their
-- short id, 61ecd188. One pays 42720 satoshis to the first receiving
-- address of portfolio0:
-- 61ecd1881ba374b36e5476bdadccd2c625bf9ff486cb8196ee4909f304e6fa51. The
-- other pays 17008 to the first receiving address of portfolio1,
-- account1Addr:
-- 61ecd188f00503cb0e11cfd351e5934e45ec7894bb048c903bb86b2cca75d36e. The
-- amounts were searched for until the first 8 digits of the two txids
-- matched. Each payment has one output, so the two coins share their
-- short id and their index too: 61ecd188:0.
shortIdClashChain :: Ctx -> Chain
shortIdClashChain ctx =
  Chain
    800002
    [ (fundingTx ctx 1 [(externalAddr 0, 42720)], Just 800000),
      (fundingTx ctx 2 [(account1Addr, 17008)], Just 800000)
    ]

-- | A payment of 30000 satoshis from another wallet to the second
-- receiving address of the same account
newerPayment :: Ctx -> Tx
newerPayment ctx = fundingTx ctx 10 [(externalAddr 1, 30000)]

-- | The paid chain with the newer payment in its best block, 800002: the
-- coin of 50000 has 3 confirmations, and the coin of 30000 has 1
olderAndNewerChain :: Ctx -> Chain
olderAndNewerChain ctx = Chain 800002 [(payment ctx, Just 800000), (newerPayment ctx, Just 800002)]

-- | Coins of 50000 and 30000 satoshis at the first receiving address of
-- the same account
twoCoinsTx :: Ctx -> Tx
twoCoinsTx ctx = fundingTx ctx 5 [(externalAddr 0, 50000), (externalAddr 0, 30000)]

-- | Coins of 50000 satoshis at the first receiving address of the same
-- account, and 30000 at the second
twoAddressesTx :: Ctx -> Tx
twoAddressesTx ctx = fundingTx ctx 7 [(externalAddr 0, 50000), (externalAddr 1, 30000)]

-- | Coins of both types of portfolio0 of the test wallet on btc: 50000
-- satoshis at the first receiving address of its bip44 account, output 0,
-- and 30000 at the first receiving address of its bip84 account,
-- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc, output 1
bothTypesTx :: Ctx -> Tx
bothTypesTx ctx = fundingTx ctx 12 [(externalAddr 0, 50000), (segwitExternalAddr 0, 30000)]

-- | A payment from another wallet to two portfolios of the test wallet: 50000
-- satoshis to the first receiving address of the bip44 account of
-- portfolio0 on btc, and 30000 to that of portfolio1,
-- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo
twoPortfoliosTx :: Ctx -> Tx
twoPortfoliosTx ctx = fundingTx ctx 3 [(externalAddr 0, 50000), (account1Addr, 30000)]

-- | A coin of 50000 satoshis at the first receiving address of the bip44
-- account of portfolio0 of the test wallet on Bitcoin Cash
cashCoinTx :: Ctx -> Tx
cashCoinTx ctx = fundingTx ctx 8 [(bchExternalAddr, 50000)]

-- | The first change address of the bip44 account of portfolio0 of the
-- test wallet on Bitcoin Cash, m/44'/145'/0'/1/0, in the legacy form,
-- derived apart from hw (see TEST-CHANGES, the transaction modules)
cashChangeAddr :: Address
cashChangeAddr = testAddress "1Eb2U8LPm71fMhgL5EF2fg4UktNkuGY6ia"

-- | A coin of 50000 satoshis at the first receiving address of the bip44
-- account of portfolio0 on the test networks of Bitcoin,
-- mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih, which btctest and btctest4 share
testnetCoinTx :: Ctx -> Tx
testnetCoinTx ctx = fundingTx ctx 13 [(testnetExternalAddr, 50000)]

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
